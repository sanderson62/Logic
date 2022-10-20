00001  ID DIVISION.
00002
00003  PROGRAM-ID.                 EL114.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 03/20/95 15:43:46.
00007 *                            VMOD=2.009.
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
00023 *REMARKS.
00024
00025 *        THIS PROGRAM PROVIDES THE MAINTENANCE FUNCTIONS NEEDED
00026 *    FOR THE BENEFICIARY CONTROL RECORDS.
00027
00028 *    SCREENS     - EL114A - BENEFICIARY MAINTENANCE
00029
00030 *    ENTERED BY  - EL101 - MAINTENANCE MENU
00031
00032 *    EXIT TO     - EL101 - MAINTENANCE MENU
00033
00034 *    INPUT FILE  - ELBENE -              - BENEFICIARY RECORDS
00035
00036 *    OUTPUT FILE - ELBENE -              - BENEFICIARY RECORDS
00037
00038 *    COMMAREA    - PASSED
00039
00040 *    NARRATIVE   - FIRST ENTRY IS VIA AN XCTL FROM EL101.  ON
00041 *                  FIRST ENTRY, A SKELETON SCREEN IS SENT AND THE
00042 *                  PROGRAM EXITS TO WAIT FOR INPUT.  ON SUCCESSIVE
00043 *                  ENTRIES (XCTL FROM CICS VIA EX39) THE SCREEN
00044 *                  WILL BE READ AND ACTION WILL BE BASED ON THE
00045 *                  MAINTENANCE TYPE INDICATED.
081312******************************************************************
081312*                   C H A N G E   L O G
081312*
081312* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
081312*-----------------------------------------------------------------
081312*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
081312* EFFECTIVE    NUMBER
081312*-----------------------------------------------------------------
081312* 081312  IR2012081000001  AJRA  EDIT FOR BLANK BENEFICARY NUMBER
082317* 082317  CR2017082100003  PEMA  ADD SUB TYPE to elbene
032019* 032019  CR2019011400002  PEMA  ADD ACH processing
081312******************************************************************
00046
00047      EJECT
00048  ENVIRONMENT DIVISION.
00049  DATA DIVISION.
00050  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00051  77  FILLER  PIC X(32)  VALUE '********************************'.
00052  77  FILLER  PIC X(32)  VALUE '*    EL114 WORKING STORAGE     *'.
00053  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.009 *********'.
00054
00055  77  BENE-IDX                PIC S9(04) VALUE +0  COMP.
00056  77  MAP-MAXIMUM             PIC S9(04) VALUE +8  COMP.
00057  77  FIRST-READ-PREV-SW      PIC X(01)  VALUE SPACES.
00058      88  FIRST-READ-PREV                VALUE 'Y'.
00059
00060  01  ACCESS-KEYS.
00061      12  ELBENE-KEY.
00062          16  ELBENE-COMPANY-CD   PIC X.
00063          16  ELBENE-RECORD-TYPE  PIC X.
00064          16  ELBENE-BENEFICIARY  PIC X(10).
00065      12  ELBENE2-KEY.
00066          16  ELBENE2-COMPANY-CD  PIC X.
00067          16  ELBENE2-RECORD-TYPE PIC X.
00068          16  ELBENE2-BENE-NAME   PIC X(30).
00069          16  ELBENE2-PRIME-ALT   PIC X(10).
00070
00071  01  WS-DATE-AREA.
00072      05  SAVE-DATE           PIC X(8)    VALUE SPACES.
00073      05  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.
00074
00075  01  MISC-WORK-AREAS.
032019     12  ws-email-sw             pic 9 value 9.
032019         88  valid-email           value 0.
032019         88  warn-email            value 1.
032019         88  invalid-email         value 2.
032019     12  e1                      pic s999 comp-3 value +0.
032019     12  e2                      pic s999 comp-3 value +0.
032019     12  ws-tally-counter        pic s999 comp-3 value +0.
032019     12  ws-work-email-address   pic x(35).
00076
00077      12  WS-ABENEI-10.
00078          16  FILLER              PIC X(4) VALUE ZEROS.
00079          16  WS-ABENEI           PIC X(6).
00080
00081      12  WS-ZIP-CODE.
00082          16  WS-ZIP-1            PIC X.
00083              88  WS-CANADIAN-ZIP    VALUE 'A' THRU 'Z'.
00084          16  WS-ZIP-2-3          PIC XX.
00085          16  WS-ZIP-4            PIC X.
00086          16  WS-ZIP-5            PIC X.
00087          16  WS-ZIP-6            PIC X.
00088          16  FILLER              PIC X(4).
00089      12  WS-ZIP-AM-1  REDEFINES  WS-ZIP-CODE.
00090          16  WS-ZIP-AM-1-CODE    PIC X(5).
00091          16  WS-ZIP-AM-1-PLUS4   PIC X(4).
00092          16  FILLER              PIC X.
00093      12  WS-ZIP-AM-2  REDEFINES  WS-ZIP-CODE.
00094          16  WS-ZIP-AM-2-CODE    PIC X(5).
00095          16  WS-ZIP-AM-2-DASH    PIC X.
00096          16  WS-ZIP-AM-2-PLUS4   PIC X(4).
00097      12  WS-ZIP-CAN-1  REDEFINES  WS-ZIP-CODE.
00098          16  WS-ZIP-CAN-1-POST1  PIC XXX.
00099          16  WS-ZIP-CAN-1-POST2  PIC XXX.
00100          16  FILLER              PIC X(4).
00101      12  WS-ZIP-CAN-2  REDEFINES  WS-ZIP-CODE.
00102          16  WS-ZIP-CAN-2-POST1  PIC XXX.
00103          16  FILLER              PIC X.
00104          16  WS-ZIP-CAN-2-POST2  PIC XXX.
00105          16  FILLER              PIC XXX.
00106
00107  01  STANDARD-AREAS.
           12  WS-RESPONSE             PIC S9(8)   COMP.
               88  RESP-NORMAL                  VALUE +00.
               88  RESP-NOTFND                  VALUE +13.
               88  RESP-DUPKEY                  VALUE +15.
               88  RESP-NOTOPEN                 VALUE +19.
               88  RESP-ENDFILE                 VALUE +20.
           12  FILE-ID-ELCNTL          PIC X(8)    VALUE 'ELCNTL'.
           12  ELCNTL-KEY.
               16  ELCNTL-COMPANY-ID   PIC X(3)  VALUE SPACES.
               16  ELCNTL-REC-TYPE     PIC X     VALUE SPACES.
               16  ELCNTL-ACCESS.
                   20  FILLER          PIC XX.
                   20  FILLER          PIC XX.
               16  ELCNTL-SEQ          PIC S9(4) VALUE +0 COMP.
00108      12  SC-ITEM             PIC S9(4) COMP VALUE +1.
00109      12  TRANS-ID            PIC X(4)    VALUE 'EX39'.
00110      12  PGM-NAME            PIC X(8).
00111      12  TIME-IN             PIC S9(7).
00112      12  TIME-OUT-R  REDEFINES TIME-IN.
00113          16  FILLER          PIC X.
00114          16  TIME-OUT        PIC 99V99.
00115          16  FILLER          PIC XX.
00116      12  XCTL-005            PIC X(8)    VALUE 'EL005'.
00117      12  XCTL-010            PIC X(8)    VALUE 'EL010'.
00118      12  XCTL-126            PIC X(8)    VALUE 'EL126'.
00119      12  XCTL-400            PIC X(8)    VALUE 'EL400'.
00120      12  XCTL-800            PIC X(8)    VALUE 'EL800'.
00121      12  LINK-001            PIC X(8)    VALUE 'EL001'.
00122      12  LINK-004            PIC X(8)    VALUE 'EL004'.
00123      12  LINK-ELDATCV        PIC X(8)    VALUE 'ELDATCV'.
00124      12  THIS-PGM            PIC X(8)    VALUE 'EL114'.
00125      12  ELBENE-FILEID       PIC X(8)    VALUE 'ELBENE'.
00126      12  ELBENE2-FILEID      PIC X(8)    VALUE 'ELBENE2'.
00127      12  SUB                 PIC 99.
00128      12  GETMAIN-SPACE       PIC X       VALUE SPACE.
00129      12  APHONE-LENGTH       PIC S9(4)   VALUE +12 COMP.
00130      12  MAPSET-NAME         PIC X(8)    VALUE 'EL114S'.
00131      12  MAP-NAME-A          PIC X(8)    VALUE 'EL114A'.
00132      12  MAP-NAME-B          PIC X(8)    VALUE 'EL114B'.
00133      12  WS-MAP-NAME         PIC X(8)    VALUE 'EL114A'.
00134      12  WS-PF-KEY           PIC 99      VALUE ZEROS.
00135
00136  01  WS-ANALYZE-NAME.
00137      12  ONE                  PIC S9(04) VALUE +1 COMP.
00138      12  TWO                  PIC S9(04) VALUE +2 COMP.
00139      12  EIGHT                PIC S9(04) VALUE +8 COMP.
00140      12  NINE                 PIC S9(04) VALUE +9 COMP.
00141      12  NAME-CNT                                 PIC S9(4) COMP.
00142      12  SCREEN-CNT                               PIC S9(4) COMP.
00143      12  WS-INPUT-NAME                            PIC X(30).
00144      12  WS-R-INPUT-NAME  REDEFINES  WS-INPUT-NAME.
00145          16  WS-INPUT-CHAR  OCCURS 30 TIMES       PIC X.
00146
00147      12  WS-READ-NAME                             PIC X(30).
00148      12  WS-R-READ-NAME  REDEFINES  WS-READ-NAME.
00149          16  WS-READ-CHAR   OCCURS 30 TIMES       PIC X.
00150      12  NAME-IDX                                 PIC S9(4) COMP.
00151
00152  01  MAP-ENTRY-NUMBERS.
00153      12  FILLER                  PIC X(3)  VALUE ' 1)'.
00154      12  FILLER                  PIC X(3)  VALUE ' 2)'.
00155      12  FILLER                  PIC X(3)  VALUE ' 3)'.
00156      12  FILLER                  PIC X(3)  VALUE ' 4)'.
00157      12  FILLER                  PIC X(3)  VALUE ' 5)'.
00158      12  FILLER                  PIC X(3)  VALUE ' 6)'.
00159      12  FILLER                  PIC X(3)  VALUE ' 7)'.
00160      12  FILLER                  PIC X(3)  VALUE ' 8)'.
00161
00162  01  MAP-LINE-TABLE  REDEFINES  MAP-ENTRY-NUMBERS.
00163      12  TBL-LINE-NUMBER  OCCURS 8 TIMES  PIC X(3).
00164
00165      EJECT
00166
00167 *                            COPY ELCSCTM.
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
00168 *                            COPY ELCSCRTY.
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
00169
00170      EJECT
00171  01  ERROR-MESSAGES.
00172      12  ER-0000                 PIC X(4)  VALUE '0000'.
00173      12  ER-0004                 PIC X(4)  VALUE '0004'.
00174      12  ER-0006                 PIC X(4)  VALUE '0006'.
00175      12  ER-0008                 PIC X(4)  VALUE '0008'.
00176      12  ER-0023                 PIC X(4)  VALUE '0023'.
00177      12  ER-0029                 PIC X(4)  VALUE '0029'.
00178      12  ER-0042                 PIC X(4)  VALUE '0042'.
00179      12  ER-0050                 PIC X(4)  VALUE '0050'.
00180      12  ER-0052                 PIC X(4)  VALUE '0052'.
00181      12  ER-0053                 PIC X(4)  VALUE '0053'.
00182      12  ER-0068                 PIC X(4)  VALUE '0068'.
00183      12  ER-0070                 PIC X(4)  VALUE '0070'.
00184      12  ER-0130                 PIC X(4)  VALUE '0130'.
00185      12  ER-0131                 PIC X(4)  VALUE '0131'.
00186      12  ER-0132                 PIC X(4)  VALUE '0132'.
00187      12  ER-0138                 PIC X(4)  VALUE '0138'.
00188      12  ER-0145                 PIC X(4)  VALUE '0145'.
00189      12  ER-0147                 PIC X(4)  VALUE '0147'.
00190      12  ER-0148                 PIC X(4)  VALUE '0148'.
00191      12  ER-0149                 PIC X(4)  VALUE '0149'.
00192      12  ER-0200                 PIC X(4)  VALUE '0200'.
00193      12  ER-0565                 PIC X(4)  VALUE '0565'.
00194      12  ER-0566                 PIC X(4)  VALUE '0566'.
00195      12  ER-0658                 PIC X(4)  VALUE '0658'.
           12  ER-2209                 PIC X(4)  VALUE '2209'.
           12  er-2565                 pic x(4)  value '2565'.
00196      12  ER-7008                 PIC X(4)  VALUE '7008'.
00197      12  ER-7220                 PIC X(4)  VALUE '7220'.
032019     12  ER-7663                 PIC X(4)  VALUE '7663'.
032019     12  ER-7664                 PIC X(4)  VALUE '7664'.
032019     12  ER-7665                 PIC X(4)  VALUE '7665'.
032019     12  ER-7666                 PIC X(4)  VALUE '7666'.
032019     12  ER-7667                 PIC X(4)  VALUE '7667'.
081312     12  ER-7668                 PIC X(4)  VALUE '7668'.
00198      12  ER-7672                 PIC X(4)  VALUE '7672'.
00199      12  ER-7673                 PIC X(4)  VALUE '7673'.
00200      12  ER-7674                 PIC X(4)  VALUE '7674'.
00201      12  ER-7675                 PIC X(4)  VALUE '7675'.
00202      12  ER-7676                 PIC X(4)  VALUE '7676'.
00203      12  ER-7677                 PIC X(4)  VALUE '7677'.
00204      12  ER-7678                 PIC X(4)  VALUE '7678'.
00205      12  ER-7679                 PIC X(4)  VALUE '7679'.
00206      12  ER-8124                 PIC X(4)  VALUE '8124'.
00207      12  ER-8125                 PIC X(4)  VALUE '8125'.
00208      EJECT
00209 *                            COPY ELCDATE.
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
00210      EJECT
00211 *                            COPY ELCLOGOF.
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
00212      EJECT
00213 *                            COPY ELCATTR.
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
00214      EJECT
00215 *                            COPY ELCEMIB.
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
00216      EJECT
00217 *                            COPY ELCINTF.
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
00218      12  FILLER    REDEFINES PI-PROGRAM-WORK-AREA.
00219          16  PI-MAX-SELECTION             PIC S9(04) COMP.
00220          16  PI-KEY-LENGTH                PIC S9(04) COMP.
00221          16  PI-NAME-LENGTH               PIC S9(04) COMP.
00222          16  PI-PREV-DIRECTION            PIC X(1).
00223              88  PI-PREV-FORWARD                   VALUE 'F'.
00224              88  PI-PREV-BACKWARD                  VALUE 'B'.
00225          16  PI-PREV-MAP-NAME             PIC X(8).
00226          16  PI-PREV-BENEFICIARY          PIC X(10).
00227          16  PI-LOW-BENE-NAME             PIC X(30).
00228          16  PI-LOW-BENE-PRIME            PIC X(10).
00229          16  PI-HIGH-BENE-NAME            PIC X(30).
00230          16  PI-HIGH-BENE-PRIME           PIC X(10).
013017         16  pi-approval-level            pic x.
00231          16  FILLER                       PIC X(534).
00232
00233      EJECT
00234 *                            COPY ELCAID.
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
00235  01  FILLER    REDEFINES DFHAID.
00236      12  FILLER              PIC X(8).
00237      12  PF-VALUES           PIC X       OCCURS 2.
00238
00239      EJECT
00240 *                                COPY EL114S.
       01  EL114BI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  BDATEL PIC S9(0004) COMP.
           05  BDATEF PIC  X(0001).
           05  FILLER REDEFINES BDATEF.
               10  BDATEA PIC  X(0001).
           05  BDATEI PIC  X(0008).
      *    -------------------------------
           05  BTIMEL PIC S9(0004) COMP.
           05  BTIMEF PIC  X(0001).
           05  FILLER REDEFINES BTIMEF.
               10  BTIMEA PIC  X(0001).
           05  BTIMEI PIC  X(0005).
      *    -------------------------------
           05  BHEAD1L PIC S9(0004) COMP.
           05  BHEAD1F PIC  X(0001).
           05  FILLER REDEFINES BHEAD1F.
               10  BHEAD1A PIC  X(0001).
           05  BHEAD1I PIC  X(0033).
      *    -------------------------------
           05  BCOMPL PIC S9(0004) COMP.
           05  BCOMPF PIC  X(0001).
           05  FILLER REDEFINES BCOMPF.
               10  BCOMPA PIC  X(0001).
           05  BCOMPI PIC  X(0003).
      *    -------------------------------
           05  BNUM01L PIC S9(0004) COMP.
           05  BNUM01F PIC  X(0001).
           05  FILLER REDEFINES BNUM01F.
               10  BNUM01A PIC  X(0001).
           05  BNUM01I PIC  X(0003).
      *    -------------------------------
           05  BNAME01L PIC S9(0004) COMP.
           05  BNAME01F PIC  X(0001).
           05  FILLER REDEFINES BNAME01F.
               10  BNAME01A PIC  X(0001).
           05  BNAME01I PIC  X(0030).
      *    -------------------------------
           05  BSTE01L PIC S9(0004) COMP.
           05  BSTE01F PIC  X(0001).
           05  FILLER REDEFINES BSTE01F.
               10  BSTE01A PIC  X(0001).
           05  BSTE01I PIC  X(0030).
      *    -------------------------------
           05  BCNTL01L PIC S9(0004) COMP.
           05  BCNTL01F PIC  X(0001).
           05  FILLER REDEFINES BCNTL01F.
               10  BCNTL01A PIC  X(0001).
           05  BCNTL01I PIC  X(0010).
      *    -------------------------------
           05  BNUM02L PIC S9(0004) COMP.
           05  BNUM02F PIC  X(0001).
           05  FILLER REDEFINES BNUM02F.
               10  BNUM02A PIC  X(0001).
           05  BNUM02I PIC  X(0003).
      *    -------------------------------
           05  BNAME02L PIC S9(0004) COMP.
           05  BNAME02F PIC  X(0001).
           05  FILLER REDEFINES BNAME02F.
               10  BNAME02A PIC  X(0001).
           05  BNAME02I PIC  X(0030).
      *    -------------------------------
           05  BSTE02L PIC S9(0004) COMP.
           05  BSTE02F PIC  X(0001).
           05  FILLER REDEFINES BSTE02F.
               10  BSTE02A PIC  X(0001).
           05  BSTE02I PIC  X(0030).
      *    -------------------------------
           05  BCNTL02L PIC S9(0004) COMP.
           05  BCNTL02F PIC  X(0001).
           05  FILLER REDEFINES BCNTL02F.
               10  BCNTL02A PIC  X(0001).
           05  BCNTL02I PIC  X(0010).
      *    -------------------------------
           05  BNUM03L PIC S9(0004) COMP.
           05  BNUM03F PIC  X(0001).
           05  FILLER REDEFINES BNUM03F.
               10  BNUM03A PIC  X(0001).
           05  BNUM03I PIC  X(0003).
      *    -------------------------------
           05  BNAME03L PIC S9(0004) COMP.
           05  BNAME03F PIC  X(0001).
           05  FILLER REDEFINES BNAME03F.
               10  BNAME03A PIC  X(0001).
           05  BNAME03I PIC  X(0030).
      *    -------------------------------
           05  BSTE03L PIC S9(0004) COMP.
           05  BSTE03F PIC  X(0001).
           05  FILLER REDEFINES BSTE03F.
               10  BSTE03A PIC  X(0001).
           05  BSTE03I PIC  X(0030).
      *    -------------------------------
           05  BCNTL03L PIC S9(0004) COMP.
           05  BCNTL03F PIC  X(0001).
           05  FILLER REDEFINES BCNTL03F.
               10  BCNTL03A PIC  X(0001).
           05  BCNTL03I PIC  X(0010).
      *    -------------------------------
           05  BNUM04L PIC S9(0004) COMP.
           05  BNUM04F PIC  X(0001).
           05  FILLER REDEFINES BNUM04F.
               10  BNUM04A PIC  X(0001).
           05  BNUM04I PIC  X(0003).
      *    -------------------------------
           05  BNAME04L PIC S9(0004) COMP.
           05  BNAME04F PIC  X(0001).
           05  FILLER REDEFINES BNAME04F.
               10  BNAME04A PIC  X(0001).
           05  BNAME04I PIC  X(0030).
      *    -------------------------------
           05  BSTE04L PIC S9(0004) COMP.
           05  BSTE04F PIC  X(0001).
           05  FILLER REDEFINES BSTE04F.
               10  BSTE04A PIC  X(0001).
           05  BSTE04I PIC  X(0030).
      *    -------------------------------
           05  BCNTL04L PIC S9(0004) COMP.
           05  BCNTL04F PIC  X(0001).
           05  FILLER REDEFINES BCNTL04F.
               10  BCNTL04A PIC  X(0001).
           05  BCNTL04I PIC  X(0010).
      *    -------------------------------
           05  BNUM05L PIC S9(0004) COMP.
           05  BNUM05F PIC  X(0001).
           05  FILLER REDEFINES BNUM05F.
               10  BNUM05A PIC  X(0001).
           05  BNUM05I PIC  X(0003).
      *    -------------------------------
           05  BNAME05L PIC S9(0004) COMP.
           05  BNAME05F PIC  X(0001).
           05  FILLER REDEFINES BNAME05F.
               10  BNAME05A PIC  X(0001).
           05  BNAME05I PIC  X(0030).
      *    -------------------------------
           05  BSTE05L PIC S9(0004) COMP.
           05  BSTE05F PIC  X(0001).
           05  FILLER REDEFINES BSTE05F.
               10  BSTE05A PIC  X(0001).
           05  BSTE05I PIC  X(0030).
      *    -------------------------------
           05  BCNTL05L PIC S9(0004) COMP.
           05  BCNTL05F PIC  X(0001).
           05  FILLER REDEFINES BCNTL05F.
               10  BCNTL05A PIC  X(0001).
           05  BCNTL05I PIC  X(0010).
      *    -------------------------------
           05  BNUM06L PIC S9(0004) COMP.
           05  BNUM06F PIC  X(0001).
           05  FILLER REDEFINES BNUM06F.
               10  BNUM06A PIC  X(0001).
           05  BNUM06I PIC  X(0003).
      *    -------------------------------
           05  BNAME06L PIC S9(0004) COMP.
           05  BNAME06F PIC  X(0001).
           05  FILLER REDEFINES BNAME06F.
               10  BNAME06A PIC  X(0001).
           05  BNAME06I PIC  X(0030).
      *    -------------------------------
           05  BSTE06L PIC S9(0004) COMP.
           05  BSTE06F PIC  X(0001).
           05  FILLER REDEFINES BSTE06F.
               10  BSTE06A PIC  X(0001).
           05  BSTE06I PIC  X(0030).
      *    -------------------------------
           05  BCNTL06L PIC S9(0004) COMP.
           05  BCNTL06F PIC  X(0001).
           05  FILLER REDEFINES BCNTL06F.
               10  BCNTL06A PIC  X(0001).
           05  BCNTL06I PIC  X(0010).
      *    -------------------------------
           05  BNUM07L PIC S9(0004) COMP.
           05  BNUM07F PIC  X(0001).
           05  FILLER REDEFINES BNUM07F.
               10  BNUM07A PIC  X(0001).
           05  BNUM07I PIC  X(0003).
      *    -------------------------------
           05  BNAME07L PIC S9(0004) COMP.
           05  BNAME07F PIC  X(0001).
           05  FILLER REDEFINES BNAME07F.
               10  BNAME07A PIC  X(0001).
           05  BNAME07I PIC  X(0030).
      *    -------------------------------
           05  BSTE07L PIC S9(0004) COMP.
           05  BSTE07F PIC  X(0001).
           05  FILLER REDEFINES BSTE07F.
               10  BSTE07A PIC  X(0001).
           05  BSTE07I PIC  X(0030).
      *    -------------------------------
           05  BCNTL07L PIC S9(0004) COMP.
           05  BCNTL07F PIC  X(0001).
           05  FILLER REDEFINES BCNTL07F.
               10  BCNTL07A PIC  X(0001).
           05  BCNTL07I PIC  X(0010).
      *    -------------------------------
           05  BNUM08L PIC S9(0004) COMP.
           05  BNUM08F PIC  X(0001).
           05  FILLER REDEFINES BNUM08F.
               10  BNUM08A PIC  X(0001).
           05  BNUM08I PIC  X(0003).
      *    -------------------------------
           05  BNAME08L PIC S9(0004) COMP.
           05  BNAME08F PIC  X(0001).
           05  FILLER REDEFINES BNAME08F.
               10  BNAME08A PIC  X(0001).
           05  BNAME08I PIC  X(0030).
      *    -------------------------------
           05  BSTE08L PIC S9(0004) COMP.
           05  BSTE08F PIC  X(0001).
           05  FILLER REDEFINES BSTE08F.
               10  BSTE08A PIC  X(0001).
           05  BSTE08I PIC  X(0030).
      *    -------------------------------
           05  BCNTL08L PIC S9(0004) COMP.
           05  BCNTL08F PIC  X(0001).
           05  FILLER REDEFINES BCNTL08F.
               10  BCNTL08A PIC  X(0001).
           05  BCNTL08I PIC  X(0010).
      *    -------------------------------
           05  BSELL PIC S9(0004) COMP.
           05  BSELF PIC  X(0001).
           05  FILLER REDEFINES BSELF.
               10  BSELA PIC  X(0001).
           05  BSELI PIC  99.
      *    -------------------------------
           05  BEMSG1L PIC S9(0004) COMP.
           05  BEMSG1F PIC  X(0001).
           05  FILLER REDEFINES BEMSG1F.
               10  BEMSG1A PIC  X(0001).
           05  BEMSG1I PIC  X(0079).
      *    -------------------------------
           05  BPFKL PIC S9(0004) COMP.
           05  BPFKF PIC  X(0001).
           05  FILLER REDEFINES BPFKF.
               10  BPFKA PIC  X(0001).
           05  BPFKI PIC  99.
       01  EL114BO REDEFINES EL114BI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BHEAD1O PIC  X(0033).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCOMPO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNUM01O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNAME01O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTE01O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCNTL01O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNUM02O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNAME02O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTE02O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCNTL02O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNUM03O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNAME03O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTE03O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCNTL03O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNUM04O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNAME04O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTE04O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCNTL04O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNUM05O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNAME05O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTE05O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCNTL05O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNUM06O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNAME06O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTE06O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCNTL06O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNUM07O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNAME07O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTE07O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCNTL07O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNUM08O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNAME08O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTE08O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCNTL08O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSELO PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BEMSG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPFKO PIC  X(0002).
      *    -------------------------------
       01  EL114AI REDEFINES EL114BI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  ADATEL PIC S9(0004) COMP.
           05  ADATEF PIC  X(0001).
           05  FILLER REDEFINES ADATEF.
               10  ADATEA PIC  X(0001).
           05  ADATEI PIC  X(0008).
      *    -------------------------------
           05  ATIMEL PIC S9(0004) COMP.
           05  ATIMEF PIC  X(0001).
           05  FILLER REDEFINES ATIMEF.
               10  ATIMEA PIC  X(0001).
           05  ATIMEI PIC  X(0005).
      *    -------------------------------
           05  ACOMPL PIC S9(0004) COMP.
           05  ACOMPF PIC  X(0001).
           05  FILLER REDEFINES ACOMPF.
               10  ACOMPA PIC  X(0001).
           05  ACOMPI PIC  X(0003).
      *    -------------------------------
           05  AMAINTL PIC S9(0004) COMP.
           05  AMAINTF PIC  X(0001).
           05  FILLER REDEFINES AMAINTF.
               10  AMAINTA PIC  X(0001).
           05  AMAINTI PIC  X(0001).
      *    -------------------------------
           05  ALUDATEL PIC S9(0004) COMP.
           05  ALUDATEF PIC  X(0001).
           05  FILLER REDEFINES ALUDATEF.
               10  ALUDATEA PIC  X(0001).
           05  ALUDATEI PIC  X(0008).
      *    -------------------------------
           05  ALUTIMEL PIC S9(0004) COMP.
           05  ALUTIMEF PIC  X(0001).
           05  FILLER REDEFINES ALUTIMEF.
               10  ALUTIMEA PIC  X(0001).
           05  ALUTIMEI PIC  X(0008).
      *    -------------------------------
           05  ALUBYL PIC S9(0004) COMP.
           05  ALUBYF PIC  X(0001).
           05  FILLER REDEFINES ALUBYF.
               10  ALUBYA PIC  X(0001).
           05  ALUBYI PIC  X(0004).
      *    -------------------------------
           05  ABENEL PIC S9(0004) COMP.
           05  ABENEF PIC  X(0001).
           05  FILLER REDEFINES ABENEF.
               10  ABENEA PIC  X(0001).
           05  ABENEI PIC  X(0010).
      *    -------------------------------
           05  GRPCHKL PIC S9(0004) COMP.
           05  GRPCHKF PIC  X(0001).
           05  FILLER REDEFINES GRPCHKF.
               10  GRPCHKA PIC  X(0001).
           05  GRPCHKI PIC  X(0001).
      *    -------------------------------
           05  ABENAMEL PIC S9(0004) COMP.
           05  ABENAMEF PIC  X(0001).
           05  FILLER REDEFINES ABENAMEF.
               10  ABENAMEA PIC  X(0001).
           05  ABENAMEI PIC  X(0030).
      *    -------------------------------
           05  ACARHDGL PIC S9(0004) COMP.
           05  ACARHDGF PIC  X(0001).
           05  FILLER REDEFINES ACARHDGF.
               10  ACARHDGA PIC  X(0001).
           05  ACARHDGI PIC  X(0014).
      *    -------------------------------
           05  ACARRL PIC S9(0004) COMP.
           05  ACARRF PIC  X(0001).
           05  FILLER REDEFINES ACARRF.
               10  ACARRA PIC  X(0001).
           05  ACARRI PIC  X(0001).
      *    -------------------------------
           05  AADDR1L PIC S9(0004) COMP.
           05  AADDR1F PIC  X(0001).
           05  FILLER REDEFINES AADDR1F.
               10  AADDR1A PIC  X(0001).
           05  AADDR1I PIC  X(0030).
      *    -------------------------------
           05  AADDR2L PIC S9(0004) COMP.
           05  AADDR2F PIC  X(0001).
           05  FILLER REDEFINES AADDR2F.
               10  AADDR2A PIC  X(0001).
           05  AADDR2I PIC  X(0030).
      *    -------------------------------
           05  APHONEL PIC S9(0004) COMP.
           05  APHONEF PIC  X(0001).
           05  FILLER REDEFINES APHONEF.
               10  APHONEA PIC  X(0001).
           05  APHONEI PIC  S9(12).
      *    -------------------------------
           05  AADDR3L PIC S9(0004) COMP.
           05  AADDR3F PIC  X(0001).
           05  FILLER REDEFINES AADDR3F.
               10  AADDR3A PIC  X(0001).
           05  AADDR3I PIC  X(0030).
      *    -------------------------------
           05  ACITYL PIC S9(0004) COMP.
           05  ACITYF PIC  X(0001).
           05  FILLER REDEFINES ACITYF.
               10  ACITYA PIC  X(0001).
           05  ACITYI PIC  X(0028).
      *    -------------------------------
           05  ASTATEL PIC S9(0004) COMP.
           05  ASTATEF PIC  X(0001).
           05  FILLER REDEFINES ASTATEF.
               10  ASTATEA PIC  X(0001).
           05  ASTATEI PIC  X(0002).
      *    -------------------------------
           05  AZIPCDEL PIC S9(0004) COMP.
           05  AZIPCDEF PIC  X(0001).
           05  FILLER REDEFINES AZIPCDEF.
               10  AZIPCDEA PIC  X(0001).
           05  AZIPCDEI PIC  X(0010).
      *    -------------------------------
           05  ACORRESL PIC S9(0004) COMP.
           05  ACORRESF PIC  X(0001).
           05  FILLER REDEFINES ACORRESF.
               10  ACORRESA PIC  X(0001).
           05  ACORRESI PIC  X(0030).
      *    -------------------------------
           05  ACADDR1L PIC S9(0004) COMP.
           05  ACADDR1F PIC  X(0001).
           05  FILLER REDEFINES ACADDR1F.
               10  ACADDR1A PIC  X(0001).
           05  ACADDR1I PIC  X(0030).
      *    -------------------------------
           05  ACADDR2L PIC S9(0004) COMP.
           05  ACADDR2F PIC  X(0001).
           05  FILLER REDEFINES ACADDR2F.
               10  ACADDR2A PIC  X(0001).
           05  ACADDR2I PIC  X(0030).
      *    -------------------------------
           05  ACPHONEL PIC S9(0004) COMP.
           05  ACPHONEF PIC  X(0001).
           05  FILLER REDEFINES ACPHONEF.
               10  ACPHONEA PIC  X(0001).
           05  ACPHONEI PIC  S9(12).
      *    -------------------------------
           05  ACADDR3L PIC S9(0004) COMP.
           05  ACADDR3F PIC  X(0001).
           05  FILLER REDEFINES ACADDR3F.
               10  ACADDR3A PIC  X(0001).
           05  ACADDR3I PIC  X(0030).
      *    -------------------------------
           05  ACCITYL PIC S9(0004) COMP.
           05  ACCITYF PIC  X(0001).
           05  FILLER REDEFINES ACCITYF.
               10  ACCITYA PIC  X(0001).
           05  ACCITYI PIC  X(0028).
      *    -------------------------------
           05  ACSTATEL PIC S9(0004) COMP.
           05  ACSTATEF PIC  X(0001).
           05  FILLER REDEFINES ACSTATEF.
               10  ACSTATEA PIC  X(0001).
           05  ACSTATEI PIC  X(0002).
      *    -------------------------------
           05  ACZPCDEL PIC S9(0004) COMP.
           05  ACZPCDEF PIC  X(0001).
           05  FILLER REDEFINES ACZPCDEF.
               10  ACZPCDEA PIC  X(0001).
           05  ACZPCDEI PIC  X(0010).
      *    -------------------------------
           05  AFAXNOL PIC S9(0004) COMP.
           05  AFAXNOF PIC  X(0001).
           05  FILLER REDEFINES AFAXNOF.
               10  AFAXNOA PIC  X(0001).
           05  AFAXNOI PIC  S9(12).
      *    -------------------------------
           05  ACBSOTL PIC S9(0004) COMP.
           05  ACBSOTF PIC  X(0001).
           05  FILLER REDEFINES ACBSOTF.
               10  ACBSOTA PIC  X(0001).
           05  ACBSOTI PIC  X(0001).
      *    -------------------------------
           05  AACHYNL PIC S9(0004) COMP.
           05  AACHYNF PIC  X(0001).
           05  FILLER REDEFINES AACHYNF.
               10  AACHYNA PIC  X(0001).
           05  AACHYNI PIC  X(0001).
      *    -------------------------------
           05  AABANOL PIC S9(0004) COMP.
           05  AABANOF PIC  X(0001).
           05  FILLER REDEFINES AABANOF.
               10  AABANOA PIC  X(0001).
           05  AABANOI PIC  X(0015).
      *    -------------------------------
           05  ASUBTYPL PIC S9(0004) COMP.
           05  ASUBTYPF PIC  X(0001).
           05  FILLER REDEFINES ASUBTYPF.
               10  ASUBTYPA PIC  X(0001).
           05  ASUBTYPI PIC  X(0002).
      *    -------------------------------
           05  AACCTNOL PIC S9(0004) COMP.
           05  AACCTNOF PIC  X(0001).
           05  FILLER REDEFINES AACCTNOF.
               10  AACCTNOA PIC  X(0001).
           05  AACCTNOI PIC  X(0020).
      *    -------------------------------
           05  AACHEYNL PIC S9(0004) COMP.
           05  AACHEYNF PIC  X(0001).
           05  FILLER REDEFINES AACHEYNF.
               10  AACHEYNA PIC  X(0001).
           05  AACHEYNI PIC  X(0001).
      *    -------------------------------
           05  AEMAILL PIC S9(0004) COMP.
           05  AEMAILF PIC  X(0001).
           05  FILLER REDEFINES AEMAILF.
               10  AEMAILA PIC  X(0001).
           05  AEMAILI PIC  X(0035).
      *    -------------------------------
           05  AEMSG1L PIC S9(0004) COMP.
           05  AEMSG1F PIC  X(0001).
           05  FILLER REDEFINES AEMSG1F.
               10  AEMSG1A PIC  X(0001).
           05  AEMSG1I PIC  X(0079).
      *    -------------------------------
           05  APFKL PIC S9(0004) COMP.
           05  APFKF PIC  X(0001).
           05  FILLER REDEFINES APFKF.
               10  APFKA PIC  X(0001).
           05  APFKI PIC  99.
       01  EL114AO REDEFINES EL114BI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACOMPO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMAINTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALUDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALUTIMEO PIC  99B99B99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALUBYO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABENEO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GRPCHKO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABENAMEO PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARHDGO PIC  X(0014).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARRO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AADDR1O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AADDR2O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APHONEO PIC  999B999B9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AADDR3O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACITYO PIC  X(0028).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ASTATEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AZIPCDEO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACORRESO PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACADDR1O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACADDR2O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACPHONEO PIC  999B999B9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACADDR3O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCITYO PIC  X(0028).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACSTATEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACZPCDEO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AFAXNOO PIC  999B999B9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACBSOTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AACHYNO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AABANOO PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ASUBTYPO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AACCTNOO PIC  X(0020).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AACHEYNO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEMAILO PIC  X(0035).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEMSG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APFKO PIC  X(0002).
      *    -------------------------------
00241
00242      EJECT
00243  01  MAP-REDEFINITION  REDEFINES  EL114BI.
00244      16  FILLER                        PIC X(73).
00245      16  MAP-SELECTION-DISPLAY OCCURS 8 TIMES.
00246          20  FILLER                    PIC X(03).
00247          20  MAP-LINE-NUMBER           PIC X(03).
00248          20  FILLER                    PIC X(03).
00249          20  MAP-BENE-NAME             PIC X(30).
00250          20  FILLER                    PIC X(03).
00251          20  MAP-BENE-CITY             PIC X(30).
00252          20  FILLER                    PIC X(03).
00253          20  MAP-BENE-CNTL             PIC X(10).
00254
00255      EJECT
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
00257
00258  01  DFHCOMMAREA             PIC X(1024).
00259 *01 PARMLIST .
00260 *    02  FILLER              PIC S9(8)   COMP.
00261 *    02  ELBENE-POINTER      PIC S9(8)   COMP.
00262
00263      EJECT
00264 *                               COPY ELCBENE.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCBENE.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.006                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = BENEFICIARY FILE                          *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 500   RECFORM = FIX                            *
00011 *                                                                *
00012 *   BASE CLUSTER NAME = ELBENE                   RKP=2,LEN=12    *
00013 *     ALTERNATE PATH1 = ELBENE2 (ALT BY NAME)    RKP=14,LEN=42   *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00017 *                                                                *
CIDMOD*  NO  CID  MODS  TO  COPYBOOK  ELCBENE                          *
00018 ******************************************************************
013017*                   C H A N G E   L O G
013017*
013017* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
013017*-----------------------------------------------------------------
013017*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
013017* EFFECTIVE    NUMBER
013017*-----------------------------------------------------------------
013017* 013017  CR2016053100001  PEMA  ACH PROCESSING
082317* 082317  CR2017082100003  PEMA  Add sub type
032019* 032019  CR2019011400002  PEMA  Add email address for ach report
013017******************************************************************
00019
00020  01  BENEFICIARY-MASTER.
00021      12  BE-RECORD-ID                PIC XX.
00022          88  VALID-BE-ID                VALUE 'BE'.
00023
00024      12  BE-CONTROL-PRIMARY.
00025          16  BE-COMPANY-CD           PIC X.
00026          16  BE-RECORD-TYPE          PIC X.
00027              88  BENEFICIARY-RECORD  VALUE 'B'.
00028              88  ADJUSTOR-RECORD     VALUE 'A'.
00029          16  BE-BENEFICIARY          PIC X(10).
00030      12  BE-CONTROL-BY-NAME.
00031          16  BE-COMPANY-CD-A1        PIC X.
00032          16  BE-RECORD-TYPE-A1       PIC X.
00033          16  BE-MAIL-TO-NAME-A1      PIC X(30).
00034          16  BE-ALTERNATE-PRIME-A1   PIC X(10).
00035
00036      12  BE-LAST-MAINT-DT            PIC XX.
00037      12  BE-LAST-MAINT-BY            PIC X(4).
00038      12  BE-LAST-MAINT-HHMMSS        PIC S9(6)     COMP-3.
00039
00040      12  BE-ADDRESS-DATA.
00041          16  BE-MAIL-TO-NAME         PIC X(30).
00042          16  BE-ADDRESS-LINE-1       PIC X(30).
00043          16  BE-ADDRESS-LINE-2       PIC X(30).
00044          16  BE-ADDRESS-LINE-3       PIC X(30).
00045          16  BE-CITY-STATE.
051810             20  BE-CITY             PIC X(28).
051810             20  BE-STATE            PIC XX.
00046          16  BE-ZIP-CODE.
00047              20  BE-ZIP-PRIME.
00048                  24  BE-ZIP-1ST      PIC X.
00049                      88  BE-CANADIAN-POST-CODE
00050                                          VALUE 'A' THRU 'Z'.
00051                  24  FILLER          PIC X(4).
00052              20  BE-ZIP-PLUS4        PIC X(4).
00053          16  BE-CANADIAN-POSTAL-CODE  REDEFINES  BE-ZIP-CODE.
00054              20  BE-CAN-POSTAL-1     PIC XXX.
00055              20  BE-CAN-POSTAL-2     PIC XXX.
00056              20  FILLER              PIC XXX.
00057          16  BE-PHONE-NO             PIC 9(11)     COMP-3.
00058          16  BE-GROUP-CHECKS-Y-N     PIC X.
00059
00060 ******************************************************************
00061 *    THE BE-CARRIER FIELD IS USED BY 'AIG' TO DETERMINE HOW TO   *
00062 *    SET THE CARRIER CODE IN THE PENDING CLAIM FILE.             *
00063 ******************************************************************
00064      12  BE-CARRIER                  PIC X.
00065
00066      12  BE-ADDRESS-DATA2.
00067          16  BE-MAIL-TO-NAME2        PIC X(30).
00068          16  BE-ADDRESS-LINE-12      PIC X(30).
00069          16  BE-ADDRESS-LINE-22      PIC X(30).
00070          16  BE-ADDRESS-LINE-32      PIC X(30).
00071          16  BE-CITY-STATE2.
051810             20  BE-CITY2            PIC X(28).
051810             20  BE-STATE2           PIC XX.
00072          16  BE-ZIP-CODE2.
00073              20  BE-ZIP-PRIME2.
00074                  24  BE-ZIP-1ST2     PIC X.
00075                      88  BE-CANADIAN-POST-CODE2
00076                                          VALUE 'A' THRU 'Z'.
00077                  24  FILLER          PIC X(4).
00078              20  BE-ZIP-PLUS42       PIC X(4).
00079          16  BE-CANADIAN-POSTAL-CODE2 REDEFINES  BE-ZIP-CODE2.
00080              20  BE-CAN-POSTAL-12    PIC XXX.
00081              20  BE-CAN-POSTAL-22    PIC XXX.
00082              20  FILLER              PIC XXX.
00083          16  BE-PHONE-NO2            PIC 9(11)     COMP-3.
               16  BE-ACH-DATA.
                   20  BE-ACH-YES-OR-NO    PIC X.
                       88  BE-ON-ACH       VALUE 'Y'.
                       88  BE-NOT-ON-ACH   VALUE 'N' ' '.
                   20  BE-ACH-ABA-ROUTING-NUMBER
                                           PIC X(15).
                   20  BE-ACH-BANK-ACCOUNT-NUMBER
                                           PIC X(20).
                   20  BE-ACH-SUB-TYPE     PIC XX.
032019             20  BE-ACH-EMAIL-YN     PIC X.
032019                 88  BE-EMAIL-ACH-RPT  VALUE 'Y'.
032019             20  be-ach-email-addr   PIC X(40).
00084          16  BE-BILLING-STMT-DATA.
032019*            20  BE-BSR-PHONE-NUM    PIC 9(11)     COMP-3.
00091              20  BE-BSR-FAX-NUM      PIC 9(11)     COMP-3.
00092              20  BE-OUTPUT-TYPE      PIC X.
00093                  88  BE-FAX-OUTPUT         VALUE 'F'.
00094                  88  BE-PRINT-OUTPUT       VALUE 'P' ' '.
00095
032019     12  filler                      PIC X(16).
00097 ******************************************************************
      *                               COPY ELCCNTL.
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
102717* 102717  CR2017062000003  PEMA  COMM CAP CHANGES
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
102717                 88  ST-LIMIT-TO-GA             VALUE 'G'.
102717                 88  ST-LIMIT-TO-BOTH           VALUE 'B'.
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
102717         16  cf-commission-cap-required     pic x.
102717         16  CF-ST-GA-COMMISSION-CAPS.
102717             20  CF-ST-GA-COMM-CAP-SL       PIC S9V9(4) COMP-3.
102717             20  CF-ST-GA-COMM-CAP-JL       PIC S9V9(4) COMP-3.
102717             20  CF-ST-GA-COMM-CAP-SA       PIC S9V9(4) COMP-3.
102717             20  CF-ST-GA-COMM-CAP-JA       PIC S9V9(4) COMP-3.
102717         16  CF-ST-TOT-COMMISSION-CAPS.
102717             20  CF-ST-TOT-COMM-CAP-SL      PIC S9V9(4) COMP-3.
102717             20  CF-ST-TOT-COMM-CAP-JL      PIC S9V9(4) COMP-3.
102717             20  CF-ST-TOT-COMM-CAP-SA      PIC S9V9(4) COMP-3.
102717             20  CF-ST-TOT-COMM-CAP-JA      PIC S9V9(4) COMP-3.
102717         16  FILLER                         PIC X(156).
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
00265
00266      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA
                                BENEFICIARY-MASTER CONTROL-FILE.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL114' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00268      MOVE EIBDATE               TO DC-JULIAN-YYDDD.
00269      MOVE '5'                   TO DC-OPTION-CODE.
00270      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
00271      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.
00272      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.
00273
00274      MOVE DFHCOMMAREA TO PROGRAM-INTERFACE-BLOCK.
00275      IF EIBCALEN EQUAL 0
00276          GO TO 8800-UNAUTHORIZED-ACCESS.
00277
00278      
      * EXEC CICS HANDLE CONDITION
00279 *        DUPREC  (8850-DUPREC)
00280 *        NOTOPEN (8870-NOTOPEN)
00281 *        NOTFND  (8880-NOT-FOUND)
00282 *        PGMIDERR(9600-PGMID-ERROR)
00283 *        ERROR   (9990-ABEND)
00284 *    END-EXEC.
      *    MOVE '"$%JIL.               ! " #00003478' TO DFHEIV0
           MOVE X'2224254A494C2E2020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303033343738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00285
00286      IF PI-CALLING-PROGRAM NOT EQUAL THIS-PGM
00287          IF PI-RETURN-TO-PROGRAM NOT EQUAL THIS-PGM
00288              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
00289              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
00290              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
00291              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
00292              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
00293              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
00294              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
00295              MOVE THIS-PGM             TO PI-CALLING-PROGRAM
00296          ELSE
00297              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM
00298              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM
00299              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1
00300              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2
00301              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3
00302              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4
00303              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5
00304              MOVE SPACES               TO PI-SAVED-PROGRAM-6
00305              MOVE PI-PREV-BENEFICIARY  TO ABENEI
00306      ELSE
00307          GO TO 0200-RECEIVE.
00308
00309      MOVE SPACES                 TO  PI-PROGRAM-WORK-AREA.
013017     MOVE PI-COMPANY-ID          TO ELCNTL-COMPANY-ID
013017     MOVE '2'                    TO ELCNTL-REC-TYPE
013017     MOVE +0                     TO ELCNTL-SEQ
013017     MOVE PI-PROCESSOR-ID        TO ELCNTL-ACCESS
013017     
      * EXEC CICS READ
013017*         DATASET  (FILE-ID-ELCNTL)
013017*         SET      (ADDRESS OF CONTROL-FILE)
013017*         RIDFLD   (ELCNTL-KEY)
013017*    END-EXEC
      *    MOVE '&"S        E          (   #00003514' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033353134' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ELCNTL, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
013017     MOVE CF-APPROVAL-LEVEL      TO PI-APPROVAL-LEVEL
00311      MOVE LOW-VALUES             TO  EL114AO.
00312      MOVE MAP-NAME-A             TO  PI-PREV-MAP-NAME.
00313      MOVE -1                     TO  AMAINTL.
00314      GO TO 8100-SEND-INITIAL-MAP.
00315
00316      EJECT
00317  0200-RECEIVE.
00318
00319      IF EIBAID NOT EQUAL DFHCLEAR
00320          GO TO 0205-SCREEN-NOT-CLEARED.
00321
00322      IF PI-PREV-MAP-NAME EQUAL MAP-NAME-A
00323          GO TO 9400-CLEAR.
00324
00325      MOVE LOW-VALUES             TO EL114BO.
00326      MOVE MAP-NAME-A             TO WS-MAP-NAME
00327                                     PI-PREV-MAP-NAME.
00328      MOVE SPACES                 TO PI-PREV-DIRECTION.
00329
00330      IF PI-PREV-BENEFICIARY NOT EQUAL SPACES AND LOW-VALUES
00331          MOVE PI-PREV-BENEFICIARY TO ABENEI
00332          GO TO 1000-SHOW-BENEFICIARY
00333      ELSE
00334          MOVE -1                 TO AMAINTL
00335          GO TO 8100-SEND-INITIAL-MAP.
00336
00337  0205-SCREEN-NOT-CLEARED.
00338
00339      IF PI-PROCESSOR-ID EQUAL 'LGXX'
00340          GO TO 0210-RECEIVE.
00341
00342      
      * EXEC CICS READQ TS
00343 *        QUEUE  (PI-SECURITY-TEMP-STORE-ID)
00344 *        INTO   (SECURITY-CONTROL)
00345 *        LENGTH (SC-COMM-LENGTH)
00346 *        ITEM   (SC-ITEM)
00347 *    END-EXEC.
      *    MOVE '*$II   L              ''   #00003551' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033353531' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00348
00349      MOVE SC-CLAIMS-DISPLAY (4)  TO PI-DISPLAY-CAP.
00350      MOVE SC-CLAIMS-UPDATE  (4)  TO PI-MODIFY-CAP.
00351
00352      IF NOT DISPLAY-CAP
00353          MOVE 'READ'             TO SM-READ
00354          PERFORM 9995-SECURITY-VIOLATION
00355          MOVE ER-0070            TO EMI-ERROR
00356          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00357          MOVE MAP-NAME-A         TO WS-MAP-NAME
00358          MOVE -1                 TO AMAINTL
00359          GO TO 8100-SEND-INITIAL-MAP.
00360
00361  0210-RECEIVE.
00362
00363      MOVE PI-PREV-MAP-NAME       TO WS-MAP-NAME.
00364
00365      IF EIBAID EQUAL DFHPA1 OR DFHPA2 OR DFHPA3
00366          IF WS-MAP-NAME EQUAL MAP-NAME-A
00367              MOVE LOW-VALUES     TO EL114AI
00368              MOVE ER-7008        TO EMI-ERROR
00369              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00370              MOVE -1             TO AMAINTL
00371              GO TO 8200-SEND-DATAONLY
00372          ELSE
00373              MOVE LOW-VALUES     TO EL114BI
00374              MOVE ER-7008        TO EMI-ERROR
00375              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00376              MOVE -1             TO BSELL
00377              GO TO 8200-SEND-DATAONLY.
00378
00379      
      * EXEC CICS RECEIVE
00380 *        MAP   (WS-MAP-NAME)
00381 *        MAPSET(MAPSET-NAME)
00382 *        INTO  (EL114AI)
00383 *    END-EXEC.
           MOVE LENGTH OF
            EL114AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00003588' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033353838' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL114AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00384
00385      IF WS-MAP-NAME EQUAL MAP-NAME-A
00386          IF APFKL EQUAL 0
00387              GO TO 0300-CHECK-PFKEYS
00388          ELSE
00389              MOVE APFKI          TO WS-PF-KEY.
00390
00391      IF WS-MAP-NAME EQUAL MAP-NAME-B
00392          IF BPFKL EQUAL 0
00393              GO TO 0310-CHECK-PFKEYS
00394          ELSE
00395              MOVE BPFKI          TO WS-PF-KEY.
00396
00397      IF EIBAID NOT EQUAL DFHENTER
00398          MOVE ER-0004            TO EMI-ERROR
00399          GO TO 0320-INPUT-ERROR.
00400
00401      IF WS-PF-KEY GREATER 0 AND LESS 25
00402          MOVE PF-VALUES (WS-PF-KEY)
00403                                  TO EIBAID
00404      ELSE
00405          MOVE ER-0029            TO EMI-ERROR
00406          GO TO 0320-INPUT-ERROR.
00407
00408      IF WS-MAP-NAME EQUAL MAP-NAME-B
00409          GO TO 0310-CHECK-PFKEYS.
00410
00411  0300-CHECK-PFKEYS.
00412
00413      IF EIBAID EQUAL DFHPF23
00414          GO TO 8810-PF23.
00415
00416      IF EIBAID EQUAL DFHPF24
00417          GO TO 9200-RETURN-MAIN-MENU.
00418
00419      IF EIBAID EQUAL DFHPF12
00420          GO TO 9500-PF12.
00421
00422      IF (AMAINTL NOT EQUAL 0) AND (EIBAID NOT EQUAL DFHENTER)
00423          MOVE ER-0050            TO EMI-ERROR
00424          GO TO 0320-INPUT-ERROR.
00425
00426      IF EIBAID EQUAL DFHPF1
00427          GO TO 5000-FIND-NEXT-BENEFICIARY.
00428
00429      IF EIBAID EQUAL DFHPF2
00430          GO TO 5100-FIND-PREV-BENEFICIARY.
00431
00432      IF EIBAID EQUAL DFHPF3
00433          GO TO 0500-LOOKUP-BENEFICIARY.
00434
PEMMOD     IF EIBAID = DFHPF4
PEMMOD        MOVE ABENEI              TO PI-PROGRAM-WORK-AREA (1:10)
PEMMOD        GO TO 9400-CLEAR
PEMMOD     END-IF
PEMMOD
00435      IF EIBAID EQUAL DFHENTER
00436          GO TO 0330-EDIT-DATA-MAP-A.
00437
00438      MOVE ER-0029                TO EMI-ERROR.
00439      GO TO 0320-INPUT-ERROR.
00440
00441  0310-CHECK-PFKEYS.
00442
00443      IF EIBAID EQUAL DFHPF23
00444          GO TO 8810-PF23.
00445
00446      IF EIBAID EQUAL DFHPF24
00447          GO TO 9200-RETURN-MAIN-MENU.
00448
00449      IF EIBAID EQUAL DFHPF12
00450          GO TO 9500-PF12.
00451
00452      IF EIBAID EQUAL DFHPF1
00453          GO TO 5200-ROLL-NEXT-BENEFICIARY.
00454
00455      IF EIBAID EQUAL DFHPF2
00456          GO TO 5300-ROLL-PREV-BENEFICIARY.
00457
PEMMOD*    IF EIBAID EQUAL DFHENTER
PEMMOD     IF EIBAID EQUAL DFHENTER OR DFHPF3
00459          GO TO 0340-EDIT-DATA-MAP-B.
00460
00461      MOVE ER-0029                TO EMI-ERROR.
00462
00463  0320-INPUT-ERROR.
00464
00465      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00466
00467      IF WS-MAP-NAME EQUAL MAP-NAME-A
00468          IF APFKL EQUAL 0
00469              MOVE -1             TO AMAINTL
00470          ELSE
00471              MOVE -1             TO APFKL.
00472
00473      IF WS-MAP-NAME EQUAL MAP-NAME-B
00474          IF BPFKL EQUAL 0
00475              MOVE -1             TO BSELL
00476          ELSE
00477              MOVE -1             TO BPFKL.
00478
00479      GO TO 8200-SEND-DATAONLY.
00480
00481      EJECT
00482  0330-EDIT-DATA-MAP-A.
00483
00484      IF ABENEL EQUAL ZERO
00485          MOVE ER-0565            TO EMI-ERROR
00486          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00487          MOVE -1                 TO ABENEL
00488          MOVE AL-UABON           TO ABENEA
00489          MOVE AL-UANON           TO AMAINTA
00490          GO TO 8200-SEND-DATAONLY.
00491
00492      IF PI-COMPANY-ID = 'DMD'
00493        IF ABENEL = +6
00494          MOVE ABENEI (1:6)       TO WS-ABENEI
00495          MOVE WS-ABENEI-10       TO ABENEI.
00496
00497      IF AMAINTI EQUAL 'S'
00498          GO TO 1000-SHOW-BENEFICIARY.
00499
00500      IF AMAINTI EQUAL 'A' OR 'C' OR 'D'
00501         IF NOT MODIFY-CAP
00502             PERFORM 9995-SECURITY-VIOLATION
00503             MOVE ER-0070         TO EMI-ERROR
00504             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00505             MOVE LOW-VALUES      TO EL114AO
00506             MOVE -1              TO AMAINTL
00507             GO TO 8100-SEND-INITIAL-MAP.
00508
00509      IF AMAINTI EQUAL 'C'
00510          GO TO 2000-CHANGE-BENEFICIARY.
00511
00512      IF AMAINTI EQUAL 'A'
00513          GO TO 3000-ADD-BENEFICIARY.
00514
00515      IF AMAINTI EQUAL 'D'
00516          GO TO 4000-DELETE-BENEFICIARY.
00517
00518      MOVE ER-0023                TO EMI-ERROR.
00519      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00520      MOVE -1                     TO AMAINTL.
00521      MOVE AL-UABON               TO AMAINTA.
00522      GO TO 8200-SEND-DATAONLY.
00523
00524      EJECT
00525  0340-EDIT-DATA-MAP-B.
00526
00527      IF BSELL EQUAL ZERO
00528          MOVE ER-0200            TO EMI-ERROR
00529          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00530          MOVE -1                 TO BSELL
00531          MOVE AL-UNBON           TO BSELA
00532          GO TO 8200-SEND-DATAONLY.
00533
00534      IF BSELI GREATER THAN PI-MAX-SELECTION
00535          MOVE ER-0200            TO EMI-ERROR
00536          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00537          MOVE -1                 TO BSELL
00538          MOVE AL-UNBON           TO BSELA
00539          GO TO 8200-SEND-DATAONLY.
00540
00541      MOVE BSELI                  TO BENE-IDX.
00542      MOVE MAP-BENE-CNTL (BENE-IDX)
00543                                  TO PI-PREV-BENEFICIARY.
00544      MOVE LOW-VALUES             TO EL114AI.
00545      MOVE MAP-NAME-A             TO WS-MAP-NAME
00546                                     PI-PREV-MAP-NAME.
00547      MOVE PI-PREV-BENEFICIARY    TO ABENEI.
PEMMOD
PEMMOD     IF EIBAID = DFHPF3
PEMMOD        MOVE PI-PREV-BENEFICIARY TO PI-PROGRAM-WORK-AREA (1:10)
PEMMOD        GO TO 9400-CLEAR
PEMMOD     END-IF
PEMMOD
00548      GO TO 1000-SHOW-BENEFICIARY.
00549
00550      EJECT
00551  0500-LOOKUP-BENEFICIARY.
00552
00553      IF ABENAMEL EQUAL ZERO
00554          MOVE ER-7677            TO EMI-ERROR
00555          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00556          MOVE -1                 TO ABENAMEL
00557          MOVE AL-UABON           TO ABENEA
00558          GO TO 8200-SEND-DATAONLY.
00559
00560      IF ABENAMEI EQUAL SPACES OR LOW-VALUES
00561          MOVE ER-7677            TO EMI-ERROR
00562          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00563          MOVE -1                 TO ABENAMEL
00564          MOVE AL-UABON           TO ABENEA
00565          GO TO 8200-SEND-DATAONLY.
00566
00567      MOVE ABENEI                 TO PI-PREV-BENEFICIARY.
00568      MOVE TWO                    TO PI-KEY-LENGTH.
00569
00570      MOVE PI-COMPANY-CD          TO ELBENE2-COMPANY-CD.
00571      MOVE 'B'                    TO ELBENE2-RECORD-TYPE.
00572      MOVE ABENAMEI               TO ELBENE2-BENE-NAME
00573                                     WS-INPUT-NAME.
00574
00575      PERFORM 9999-DUMMY-ROUTINE THRU 9999-EXIT
00576          VARYING NAME-IDX FROM ABENAMEL BY -1
00577           UNTIL WS-INPUT-CHAR (NAME-IDX) GREATER THAN SPACES.
00578
00579      MOVE NAME-IDX               TO PI-NAME-LENGTH
00580      ADD NAME-IDX                TO PI-KEY-LENGTH.
00581
00582      
      * EXEC CICS HANDLE CONDITION
00583 *        NOTFND (0510-BENEFICIARY-NOTFND)
00584 *    END-EXEC.
      *    MOVE '"$I                   ! # #00003803' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303033383033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00585
00586      
      * EXEC CICS READ
00587 *        DATASET(ELBENE2-FILEID)
00588 *        SET    (ADDRESS OF BENEFICIARY-MASTER)
00589 *        RIDFLD (ELBENE2-KEY)
00590 *        GENERIC
00591 *        EQUAL
00592 *        KEYLENGTH(PI-KEY-LENGTH)
00593 *    END-EXEC.
      *    MOVE '&"S  KG    E          (   #00003807' TO DFHEIV0
           MOVE X'26225320204B472020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383037' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELBENE2-FILEID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELBENE2-KEY, 
                 PI-KEY-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF BENEFICIARY-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00594
00595      MOVE BE-CONTROL-BY-NAME     TO ELBENE2-KEY.
00596
00597      GO TO 5210-BENE-KEY-BUILT.
00598
00599  0510-BENEFICIARY-NOTFND.
00600
00601      MOVE ER-7678                TO EMI-ERROR.
00602      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00603      MOVE -1                     TO ABENAMEL.
00604      MOVE AL-UABON               TO ABENAMEA.
00605      GO TO 8200-SEND-DATAONLY.
00606
00607      EJECT
00608  1000-SHOW-BENEFICIARY.
00609      MOVE PI-COMPANY-CD          TO ELBENE-COMPANY-CD.
00610      MOVE 'B'                    TO ELBENE-RECORD-TYPE.
00611      MOVE ABENEI                 TO ELBENE-BENEFICIARY.
00612
00613      
      * EXEC CICS READ
00614 *        DATASET(ELBENE-FILEID)
00615 *        SET    (ADDRESS OF BENEFICIARY-MASTER)
00616 *        RIDFLD (ELBENE-KEY)
00617 *    END-EXEC.
      *    MOVE '&"S        E          (   #00003834' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383334' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELBENE-FILEID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELBENE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF BENEFICIARY-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00618
00619      GO TO 7000-BUILD-OUTPUT-MAP.
00620
00621      EJECT
00622  2000-CHANGE-BENEFICIARY.
00623      IF ABENEI NOT EQUAL PI-PREV-BENEFICIARY
00624          MOVE ER-0138            TO EMI-ERROR
00625          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00626          MOVE -1                 TO ABENEL
00627          MOVE AL-UABON           TO ABENEA
00628          GO TO 8200-SEND-DATAONLY.
00629
00630      PERFORM 6000-EDIT-INPUT-DATA THRU 6000-EXIT.
00631
00632      IF NOT EMI-NO-ERRORS
00633          GO TO 8200-SEND-DATAONLY.
00634
00635      MOVE PI-COMPANY-CD          TO ELBENE-COMPANY-CD
00636      MOVE 'B'                    TO ELBENE-RECORD-TYPE.
00637      MOVE ABENEI                 TO ELBENE-BENEFICIARY
00638
00639      
      * EXEC CICS READ
00640 *        UPDATE
00641 *        DATASET(ELBENE-FILEID)
00642 *        SET    (ADDRESS OF BENEFICIARY-MASTER)
00643 *        RIDFLD(ELBENE-KEY)
00644 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00003860' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELBENE-FILEID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELBENE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF BENEFICIARY-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00645
00646      IF BE-LAST-MAINT-BY     NOT EQUAL PI-UPDATE-BY OR
00647         BE-LAST-MAINT-HHMMSS NOT EQUAL PI-UPDATE-HHMMSS
00648          
      * EXEC CICS UNLOCK
00649 *            DATASET(ELBENE-FILEID)
00650 *        END-EXEC
      *    MOVE '&*                    #   #00003869' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELBENE-FILEID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00651          MOVE ER-0068 TO EMI-ERROR
00652          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00653          GO TO 1000-SHOW-BENEFICIARY.
00654
00655      MOVE PI-PROCESSOR-ID        TO BE-LAST-MAINT-BY.
00656      MOVE EIBTIME                TO BE-LAST-MAINT-HHMMSS.
00657      MOVE EIBDATE                TO DC-JULIAN-YYDDD.
00658      MOVE '5'                    TO DC-OPTION-CODE.
00659      MOVE LINK-ELDATCV           TO PGM-NAME.
00660
00661      
      * EXEC CICS LINK
00662 *        PROGRAM (PGM-NAME)
00663 *        COMMAREA(DATE-CONVERSION-DATA)
00664 *        LENGTH  (DC-COMM-LENGTH)
00665 *    END-EXEC.
      *    MOVE '."C                   (   #00003882' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00666
00667      IF DATE-CONVERSION-ERROR
00668          MOVE LOW-VALUES         TO BE-LAST-MAINT-DT
00669      ELSE
00670          MOVE DC-BIN-DATE-1      TO BE-LAST-MAINT-DT.
00671
00672      IF ABENAMEL NOT EQUAL ZEROS
00673         MOVE ABENAMEI            TO BE-MAIL-TO-NAME
00674                                     BE-MAIL-TO-NAME-A1.
00675
00676      IF AADDR1L  NOT EQUAL ZEROS
00677         MOVE AADDR1I             TO BE-ADDRESS-LINE-1.
00678
00679      IF AADDR2L  NOT EQUAL ZEROS
00680         MOVE AADDR2I             TO BE-ADDRESS-LINE-2.
00681
00682      IF AADDR3L  NOT EQUAL ZEROS
00683         MOVE AADDR3I             TO BE-ADDRESS-LINE-3.
00684
           IF ACITYL NOT = 0
              MOVE ACITYI              TO BE-CITY
           END-IF
           IF ASTATEL NOT = 0
              MOVE ASTATEI             TO BE-STATE
           END-IF
00685 *    IF ACITYSTL NOT EQUAL ZEROS
00686 *       MOVE ACITYSTI            TO BE-CITY-STATE.
00687
00688      IF AZIPCDEL = ZEROS
00689          GO TO 2000-CHANGE-CONTINUE.
00690
00691      MOVE SPACES                      TO BE-ZIP-CODE.
00692      MOVE AZIPCDEI                    TO WS-ZIP-CODE.
00693
00694      IF WS-CANADIAN-ZIP
00695          IF WS-ZIP-4 = SPACE  OR  '-'
00696              MOVE WS-ZIP-CAN-2-POST1  TO BE-CAN-POSTAL-1
00697              MOVE WS-ZIP-CAN-2-POST2  TO BE-CAN-POSTAL-2
00698          ELSE
00699              MOVE WS-ZIP-CAN-1-POST1  TO BE-CAN-POSTAL-1
00700              MOVE WS-ZIP-CAN-1-POST2  TO BE-CAN-POSTAL-2
00701      ELSE
00702          IF WS-ZIP-6 = SPACE  OR  '-'
00703              MOVE WS-ZIP-AM-2-CODE    TO BE-ZIP-PRIME
00704              MOVE WS-ZIP-AM-2-PLUS4   TO BE-ZIP-PLUS4
00705          ELSE
00706              MOVE WS-ZIP-AM-1-CODE    TO BE-ZIP-PRIME
00707              MOVE WS-ZIP-AM-1-PLUS4   TO BE-ZIP-PLUS4.
00708
00709  2000-CHANGE-CONTINUE.
00710
00711      IF APHONEL GREATER THAN ZERO
00712          
      * EXEC CICS BIF DEEDIT
00713 *            FIELD   (APHONEI)
00714 *            LENGTH  (APHONE-LENGTH)
00715 *        END-EXEC
      *    MOVE '@"L                   #   #00003939' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393339' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 APHONEI, 
                 APHONE-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00716          MOVE APHONEI            TO BE-PHONE-NO.
00717
00718      IF GRPCHKL GREATER THAN +0
00719         MOVE GRPCHKI             TO BE-GROUP-CHECKS-Y-N.
00720
00721      IF ACORRESL GREATER THAN +0
00722         MOVE ACORRESI            TO  BE-MAIL-TO-NAME2.
00723
00724      IF ACADDR1L GREATER THAN +0
00725         MOVE  ACADDR1I           TO  BE-ADDRESS-LINE-12.
00726
00727      IF ACADDR2L GREATER THAN +0
00728         MOVE  ACADDR2I           TO  BE-ADDRESS-LINE-22.
00729
00730      IF ACADDR3L GREATER THAN +0
00731         MOVE ACADDR3I            TO  BE-ADDRESS-LINE-32.
00732
00733      IF ACCITYL GREATER THAN +0
00734         MOVE ACCITYI            TO  BE-CITY2.
00733      IF ACSTATEL GREATER THAN +0
00734         MOVE ACSTATEI            TO  BE-STATE2.
00736      IF ACZPCDEL = ZEROS
00737          GO TO 2001-CHANGE-CONTINUE.
00738
00739      MOVE SPACES                      TO BE-ZIP-CODE2.
00740      MOVE ACZPCDEI                    TO WS-ZIP-CODE.
00741
00742      IF WS-CANADIAN-ZIP
00743          IF WS-ZIP-4 = SPACE  OR  '-'
00744              MOVE WS-ZIP-CAN-2-POST1  TO BE-CAN-POSTAL-12
00745              MOVE WS-ZIP-CAN-2-POST2  TO BE-CAN-POSTAL-22
00746          ELSE
00747              MOVE WS-ZIP-CAN-1-POST1  TO BE-CAN-POSTAL-12
00748              MOVE WS-ZIP-CAN-1-POST2  TO BE-CAN-POSTAL-22
00749      ELSE
00750          IF WS-ZIP-6 = SPACE  OR  '-'
00751              MOVE WS-ZIP-AM-2-CODE    TO BE-ZIP-PRIME2
00752              MOVE WS-ZIP-AM-2-PLUS4   TO BE-ZIP-PLUS42
00753          ELSE
00754              MOVE WS-ZIP-AM-1-CODE    TO BE-ZIP-PRIME2
00755              MOVE WS-ZIP-AM-1-PLUS4   TO BE-ZIP-PLUS42.
00756
00757  2001-CHANGE-CONTINUE.
00758      IF ACPHONEL GREATER THAN ZERO
00759          
      * EXEC CICS BIF DEEDIT
00760 *            FIELD   (ACPHONEI)
00761 *            LENGTH  (APHONE-LENGTH)
00762 *        END-EXEC
      *    MOVE '@"L                   #   #00003987' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393837' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACPHONEI, 
                 APHONE-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00763         MOVE ACPHONEI            TO  BE-PHONE-NO2.
00764
00781      IF AFAXNOL   GREATER THAN +0
00782          
      * EXEC CICS BIF DEEDIT
00783 *            FIELD   (AFAXNOI)
00784 *            LENGTH  (APHONE-LENGTH)
00785 *        END-EXEC
      *    MOVE '@"L                   #   #00003994' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AFAXNOI, 
                 APHONE-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00786         MOVE AFAXNOI             TO  BE-BSR-FAX-NUM.
00787
032019*    IF ACBSOTL   GREATER THAN +0
032019*       MOVE ACBSOTI             TO  BE-OUTPUT-TYPE.
           IF AACHYNL > +0
              MOVE AACHYNI             TO BE-ACH-YES-OR-NO
           END-IF
           IF (AABANOL > +0)
              and (pi-approval-level = '4' or '5')
              MOVE AABANOI             TO BE-ACH-ABA-ROUTING-NUMBER
           END-IF
           IF AACCTNOL > +0
              and (pi-approval-level = '4' or '5')
              MOVE AACCTNOI            TO BE-ACH-BANK-ACCOUNT-NUMBER
           END-IF
032019     if aacheynl > +0
032019        move aacheyni            to be-ach-email-yn
032019     end-if
032019
032019     if aemaill > +0
032019        move aemaili             to be-ach-email-addr
032019     end-if
           IF (ASUBTYPL > +0)
              and (pi-approval-level = '4' or '5')
              MOVE ASUBTYPI            TO BE-ACH-SUB-TYPE
           END-IF
00791      IF (PI-COMPANY-ID IS EQUAL TO 'AIG' OR 'AUK')
00792          IF ACARRL GREATER THAN +0
00793              MOVE ACARRI         TO  BE-CARRIER.
00794
00795      
      * EXEC CICS REWRITE
00796 *        DATASET(ELBENE-FILEID)
00797 *        FROM(BENEFICIARY-MASTER)
00798 *    END-EXEC.
           MOVE LENGTH OF
            BENEFICIARY-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00004028' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303034303238' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELBENE-FILEID, 
                 BENEFICIARY-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00799
00800      MOVE ER-0000                TO EMI-ERROR.
00801      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00802      MOVE LOW-VALUES             TO EL114AO.
00803      MOVE -1                     TO AMAINTL.
00804      MOVE SPACES                 TO PI-PREV-BENEFICIARY.
00805      MOVE ELBENE-BENEFICIARY     TO ABENEI.
00806      GO TO 1000-SHOW-BENEFICIARY.
00807
00808      EJECT
00809  3000-ADD-BENEFICIARY.
00810
00811      PERFORM 6000-EDIT-INPUT-DATA THRU 6000-EXIT.
00812
00813      IF NOT EMI-NO-ERRORS
00814          GO TO 8200-SEND-DATAONLY.
00815
00816      
      * EXEC CICS GETMAIN
00817 *        SET    (ADDRESS OF BENEFICIARY-MASTER)
00818 *        LENGTH (500)
00819 *        INITIMG(GETMAIN-SPACE)
00820 *    END-EXEC.
           MOVE 500
             TO DFHEIV11
      *    MOVE ',"IL                  $   #00004049' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034303439' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 GETMAIN-SPACE
           SET ADDRESS OF BENEFICIARY-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00821
00822      MOVE 'BE'                   TO BE-RECORD-ID.
00823      MOVE 'B'                    TO BE-RECORD-TYPE
00824                                     BE-RECORD-TYPE-A1
00825                                     ELBENE-RECORD-TYPE.
00826      MOVE PI-COMPANY-CD          TO BE-COMPANY-CD
00827                                     BE-COMPANY-CD-A1
00828                                     ELBENE-COMPANY-CD.
00829      MOVE ABENEI                 TO BE-BENEFICIARY
00830                                     BE-ALTERNATE-PRIME-A1
00831                                     ELBENE-BENEFICIARY.
00832      MOVE ABENAMEI               TO BE-MAIL-TO-NAME
00833                                     BE-MAIL-TO-NAME-A1.
00834      MOVE AADDR1I                TO BE-ADDRESS-LINE-1.
00835
00836      IF AADDR2L GREATER THAN ZERO
00837          MOVE AADDR2I            TO BE-ADDRESS-LINE-2.
00838
00839      IF AADDR3L  NOT EQUAL ZEROS
00840         MOVE AADDR3I             TO BE-ADDRESS-LINE-3.
00841
00842      MOVE ACITYI                 TO BE-CITY.
           MOVE ASTATEI                TO BE-STATE
00843
00844      IF AZIPCDEL = ZEROS
00845          GO TO 3000-ADD-CONTINUE.
00846
00847      MOVE SPACES                      TO BE-ZIP-CODE.
00848      MOVE AZIPCDEI                    TO WS-ZIP-CODE.
00849
00850      IF WS-CANADIAN-ZIP
00851          IF WS-ZIP-4 = SPACE  OR  '-'
00852              MOVE WS-ZIP-CAN-2-POST1  TO BE-CAN-POSTAL-1
00853              MOVE WS-ZIP-CAN-2-POST2  TO BE-CAN-POSTAL-2
00854          ELSE
00855              MOVE WS-ZIP-CAN-1-POST1  TO BE-CAN-POSTAL-1
00856              MOVE WS-ZIP-CAN-1-POST2  TO BE-CAN-POSTAL-2
00857      ELSE
00858          IF WS-ZIP-6 = SPACE  OR  '-'
00859              MOVE WS-ZIP-AM-2-CODE    TO BE-ZIP-PRIME
00860              MOVE WS-ZIP-AM-2-PLUS4   TO BE-ZIP-PLUS4
00861          ELSE
00862              MOVE WS-ZIP-AM-1-CODE    TO BE-ZIP-PRIME
00863              MOVE WS-ZIP-AM-1-PLUS4   TO BE-ZIP-PLUS4.
00864
00865  3000-ADD-CONTINUE.
00866
00867      IF APHONEL GREATER THAN ZERO
00868          
      * EXEC CICS BIF DEEDIT
00869 *            FIELD   (APHONEI)
00870 *            LENGTH  (APHONE-LENGTH)
00871 *        END-EXEC
      *    MOVE '@"L                   #   #00004102' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313032' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 APHONEI, 
                 APHONE-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00872          MOVE APHONEI            TO BE-PHONE-NO
00873      ELSE
00874          MOVE ZEROS              TO BE-PHONE-NO.
00875
00876      IF GRPCHKL GREATER THAN +0
00877         MOVE GRPCHKI             TO BE-GROUP-CHECKS-Y-N.
00878
00879      IF ACORRESL GREATER THAN +0
00880         MOVE ACORRESI            TO  BE-MAIL-TO-NAME2.
00881
00882      IF ACADDR1L GREATER THAN +0
00883         MOVE  ACADDR1I           TO  BE-ADDRESS-LINE-12.
00884
00885      IF ACADDR2L GREATER THAN +0
00886         MOVE  ACADDR2I           TO  BE-ADDRESS-LINE-22.
00887
00888      IF ACADDR3L GREATER THAN +0
00889         MOVE ACADDR3I            TO  BE-ADDRESS-LINE-32.
00890
00891      IF ACCITYL GREATER THAN +0
00892         MOVE ACCITYI            TO  BE-CITY2.
00891      IF ACSTATEL GREATER THAN +0
00892         MOVE ACSTATEI            TO  BE-STATE2.
00894      IF ACZPCDEL = ZEROS
00895          GO TO 3001-ADD-CONTINUE.
00896
00897      MOVE SPACES                      TO BE-ZIP-CODE2.
00898      MOVE ACZPCDEI                    TO WS-ZIP-CODE.
00899
00900      IF WS-CANADIAN-ZIP
00901          IF WS-ZIP-4 = SPACE  OR  '-'
00902              MOVE WS-ZIP-CAN-2-POST1  TO BE-CAN-POSTAL-12
00903              MOVE WS-ZIP-CAN-2-POST2  TO BE-CAN-POSTAL-22
00904          ELSE
00905              MOVE WS-ZIP-CAN-1-POST1  TO BE-CAN-POSTAL-12
00906              MOVE WS-ZIP-CAN-1-POST2  TO BE-CAN-POSTAL-22
00907      ELSE
00908          IF WS-ZIP-6 = SPACE  OR  '-'
00909              MOVE WS-ZIP-AM-2-CODE    TO BE-ZIP-PRIME2
00910              MOVE WS-ZIP-AM-2-PLUS4   TO BE-ZIP-PLUS42
00911          ELSE
00912              MOVE WS-ZIP-AM-1-CODE    TO BE-ZIP-PRIME2
00913              MOVE WS-ZIP-AM-1-PLUS4   TO BE-ZIP-PLUS42.
00914
00915  3001-ADD-CONTINUE.
00916      IF ACPHONEL GREATER THAN ZERO
00917          
      * EXEC CICS BIF DEEDIT
00918 *            FIELD   (ACPHONEI)
00919 *            LENGTH  (APHONE-LENGTH)
00920 *        END-EXEC
      *    MOVE '@"L                   #   #00004152' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACPHONEI, 
                 APHONE-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00921          MOVE ACPHONEI           TO  BE-PHONE-NO2
00922      ELSE
00923          MOVE ZEROS              TO  BE-PHONE-NO2.
00924
00943      IF AFAXNOL   GREATER THAN +0
00944          
      * EXEC CICS BIF DEEDIT
00945 *            FIELD   (AFAXNOI)
00946 *            LENGTH  (APHONE-LENGTH)
00947 *        END-EXEC
      *    MOVE '@"L                   #   #00004161' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AFAXNOI, 
                 APHONE-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00948         MOVE AFAXNOI             TO  BE-BSR-FAX-NUM
00949      ELSE
00950         MOVE ZEROS               TO  BE-BSR-FAX-NUM.
00951
032019*    IF ACBSOTL   GREATER THAN +0
032019*       MOVE ACBSOTI             TO  BE-OUTPUT-TYPE.
           IF AACHYNL > +0
              MOVE AACHYNI             TO BE-ACH-YES-OR-NO
           END-IF
           IF AABANOL > +0
              and (pi-approval-level = '4' or '5')
              MOVE AABANOI             TO BE-ACH-ABA-ROUTING-NUMBER
           END-IF
           IF AACCTNOL > +0
              and (pi-approval-level = '4' or '5')
              MOVE AACCTNOI            to BE-ACH-BANK-ACCOUNT-NUMBER
           END-IF
032019     if aacheynl > +0
032019        move aacheyni            to be-ach-email-yn
032019     end-if
032019
032019     if aemaill > +0
032019        move aemaili             to be-ach-email-addr
032019     end-if
00955      IF (PI-COMPANY-ID IS EQUAL TO 'AIG' OR 'AUK')
00956          IF ACARRL GREATER THAN +0
00957              MOVE ACARRI         TO BE-CARRIER.
00958
00959      MOVE EIBDATE                TO DC-JULIAN-YYDDD.
00960      MOVE '5'                    TO DC-OPTION-CODE.
00961      MOVE LINK-ELDATCV           TO PGM-NAME.
00962
00963      
      * EXEC CICS LINK
00964 *        PROGRAM (PGM-NAME)
00965 *        COMMAREA(DATE-CONVERSION-DATA)
00966 *        LENGTH  (DC-COMM-LENGTH)
00967 *    END-EXEC.
      *    MOVE '."C                   (   #00004197' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313937' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00968
00969      IF DATE-CONVERSION-ERROR
00970          MOVE LOW-VALUES         TO BE-LAST-MAINT-DT
00971      ELSE
00972          MOVE DC-BIN-DATE-1      TO BE-LAST-MAINT-DT.
00973
00974      MOVE PI-PROCESSOR-ID        TO BE-LAST-MAINT-BY.
00975      MOVE EIBTIME                TO BE-LAST-MAINT-HHMMSS.
00976
00977  3005-WRITE-ELBENE-FILE.
00978
00979      
      * EXEC CICS WRITE
00980 *        FROM   (BENEFICIARY-MASTER)
00981 *        DATASET(ELBENE-FILEID)
00982 *        RIDFLD (ELBENE-KEY)
00983 *    END-EXEC.
           MOVE LENGTH OF
            BENEFICIARY-MASTER
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00004213' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELBENE-FILEID, 
                 BENEFICIARY-MASTER, 
                 DFHEIV11, 
                 ELBENE-KEY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00984
00985      MOVE ER-0000                TO EMI-ERROR.
00986      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00987      MOVE LOW-VALUES             TO EL114AO.
00988      MOVE -1                     TO AMAINTL.
00989      MOVE SPACES                 TO PI-PREV-BENEFICIARY.
00990      MOVE ELBENE-BENEFICIARY     TO ABENEI.
00991      GO TO 1000-SHOW-BENEFICIARY.
00992
00993      EJECT
00994  4000-DELETE-BENEFICIARY.
00995
00996      IF ABENEI NOT EQUAL PI-PREV-BENEFICIARY
00997          MOVE ER-0138            TO EMI-ERROR
00998          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00999          MOVE -1                 TO ABENEL
01000          MOVE AL-UABON           TO ABENEA
01001          GO TO 8200-SEND-DATAONLY.
01002
01003      MOVE PI-COMPANY-CD          TO ELBENE-COMPANY-CD.
01004      MOVE 'B'                    TO ELBENE-RECORD-TYPE.
01005      MOVE ABENEI                 TO ELBENE-BENEFICIARY.
01006
01007      
      * EXEC CICS READ
01008 *        UPDATE
01009 *        DATASET(ELBENE-FILEID)
01010 *        SET    (ADDRESS OF BENEFICIARY-MASTER)
01011 *        RIDFLD (ELBENE-KEY)
01012 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00004241' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323431' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELBENE-FILEID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELBENE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF BENEFICIARY-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01013
01014      IF BE-LAST-MAINT-BY     NOT EQUAL PI-UPDATE-BY OR
01015         BE-LAST-MAINT-HHMMSS NOT EQUAL PI-UPDATE-HHMMSS
01016          
      * EXEC CICS UNLOCK
01017 *            DATASET(ELBENE-FILEID)
01018 *            END-EXEC
      *    MOVE '&*                    #   #00004250' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323530' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELBENE-FILEID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01019          MOVE ER-0068            TO EMI-ERROR
01020          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01021          GO TO 1000-SHOW-BENEFICIARY.
01022
01023      
      * EXEC CICS DELETE
01024 *        DATASET(ELBENE-FILEID)
01025 *        END-EXEC.
      *    MOVE '&(                    &   #00004257' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323537' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELBENE-FILEID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01026
01027      MOVE ER-0000                TO EMI-ERROR.
01028      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01029      MOVE LOW-VALUES             TO EL114AO.
01030      MOVE -1                     TO AMAINTL.
01031      MOVE SPACES                 TO PI-PREV-BENEFICIARY
01032      GO TO 8100-SEND-INITIAL-MAP.
01033
01034      EJECT
01035  5000-FIND-NEXT-BENEFICIARY.
01036
01037      MOVE PI-COMPANY-CD          TO ELBENE-COMPANY-CD.
01038      MOVE 'B'                    TO ELBENE-RECORD-TYPE.
01039
01040      IF ABENEL EQUAL 0
01041          MOVE LOW-VALUES         TO ELBENE-BENEFICIARY
01042      ELSE
01043          MOVE ABENEI             TO ELBENE-BENEFICIARY.
01044
01045      
      * EXEC CICS HANDLE CONDITION
01046 *        ENDFILE (5000-UNSUCCESSFUL-SEARCH)
01047 *    END-EXEC.
      *    MOVE '"$''                   ! $ #00004279' TO DFHEIV0
           MOVE X'222427202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303034323739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01048
01049      
      * EXEC CICS STARTBR
01050 *        DATASET (ELBENE-FILEID)
01051 *        RIDFLD  (ELBENE-KEY)
01052 *        GTEQ
01053 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00004283' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323833' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELBENE-FILEID, 
                 ELBENE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01054
01055  5000-READNEXT-LOOP.
01056      
      * EXEC CICS READNEXT
01057 *        DATASET(ELBENE-FILEID)
01058 *        SET    (ADDRESS OF BENEFICIARY-MASTER)
01059 *        RIDFLD (ELBENE-KEY)
01060 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00004290' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323930' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELBENE-FILEID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELBENE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF BENEFICIARY-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01061
01062      IF BE-COMPANY-CD  NOT EQUAL PI-COMPANY-CD
01063          PERFORM 5000-END-BROWSE
01064          GO TO 8860-ENDFILE.
01065
01066      IF BE-RECORD-TYPE NOT EQUAL 'B'
01067          PERFORM 5000-END-BROWSE
01068          GO TO 8860-ENDFILE.
01069
01070      IF ELBENE-BENEFICIARY EQUAL PI-PREV-BENEFICIARY
01071          GO TO 5000-READNEXT-LOOP.
01072
01073      MOVE ELBENE-BENEFICIARY     TO ABENEI
01074                                     PI-PREV-BENEFICIARY.
01075
01076      IF ABENEL EQUAL 0
01077          MOVE ER-7220            TO EMI-ERROR
01078          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01079
01080      GO TO 7000-BUILD-OUTPUT-MAP.
01081
01082  5000-END-BROWSE.
01083
01084      
      * EXEC CICS ENDBR
01085 *        DATASET (ELBENE-FILEID)
01086 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00004318' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELBENE-FILEID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01087
01088  5000-UNSUCCESSFUL-SEARCH.
01089
01090      PERFORM 5000-END-BROWSE.
01091      GO TO 8860-ENDFILE.
01092
01093      EJECT
01094  5100-FIND-PREV-BENEFICIARY.
01095
01096      MOVE 'Y'                    TO FIRST-READ-PREV-SW.
01097      MOVE PI-COMPANY-CD          TO ELBENE-COMPANY-CD.
01098      MOVE 'B'                    TO ELBENE-RECORD-TYPE.
01099      MOVE PI-PREV-BENEFICIARY    TO ELBENE-BENEFICIARY.
01100
01101      IF ABENEL GREATER 0
01102          MOVE ABENEI             TO ELBENE-BENEFICIARY.
01103
01104      MOVE LOW-VALUES             TO PI-PREV-BENEFICIARY.
01105
01106      
      * EXEC CICS HANDLE CONDITION
01107 *        ENDFILE (5100-UNSUCCESSFUL-SEARCH)
01108 *    END-EXEC.
      *    MOVE '"$''                   ! % #00004340' TO DFHEIV0
           MOVE X'222427202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303034333430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01109
01110      
      * EXEC CICS STARTBR
01111 *        DATASET (ELBENE-FILEID)
01112 *        RIDFLD  (ELBENE-KEY)
01113 *        GTEQ
01114 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00004344' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELBENE-FILEID, 
                 ELBENE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01115
01116  5100-READPREV-LOOP.
01117      
      * EXEC CICS READPREV
01118 *        DATASET(ELBENE-FILEID)
01119 *        SET    (ADDRESS OF BENEFICIARY-MASTER)
01120 *        RIDFLD (ELBENE-KEY)
01121 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00004351' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333531' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELBENE-FILEID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELBENE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF BENEFICIARY-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01122
01123      IF BE-COMPANY-CD  NOT EQUAL PI-COMPANY-CD
01124          PERFORM 5100-END-BROWSE
01125          GO TO 8860-ENDFILE.
01126
01127      IF BE-RECORD-TYPE NOT EQUAL 'B'
01128          PERFORM 5100-END-BROWSE
01129          GO TO 8860-ENDFILE.
01130
01131      IF FIRST-READ-PREV
01132          MOVE 'N'                TO FIRST-READ-PREV-SW
01133          GO TO 5100-READPREV-LOOP.
01134
01135      MOVE ELBENE-BENEFICIARY     TO ABENEI.
01136      MOVE BE-BENEFICIARY         TO PI-PREV-BENEFICIARY.
01137      GO TO 7000-BUILD-OUTPUT-MAP.
01138
01139  5100-END-BROWSE.
01140      
      * EXEC CICS ENDBR
01141 *        DATASET (ELBENE-FILEID)
01142 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00004374' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333734' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELBENE-FILEID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01143
01144
01145  5100-UNSUCCESSFUL-SEARCH.
01146
01147      PERFORM 5100-END-BROWSE.
01148      GO TO 8860-ENDFILE.
01149
01150      EJECT
01151  5200-ROLL-NEXT-BENEFICIARY.
01152
01153      IF PI-MAX-SELECTION LESS THAN MAP-MAXIMUM AND
01154          PI-PREV-FORWARD
01155              MOVE ER-0130        TO EMI-ERROR
01156              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01157              MOVE -1             TO BPFKL
01158              MOVE AL-UABON       TO BPFKA
01159              GO TO 8200-SEND-DATAONLY.
01160
01161      MOVE PI-COMPANY-CD          TO ELBENE2-COMPANY-CD.
01162      MOVE 'B'                    TO ELBENE2-RECORD-TYPE.
01163      MOVE PI-HIGH-BENE-NAME      TO ELBENE2-BENE-NAME
01164                                     WS-INPUT-NAME.
01165      MOVE PI-HIGH-BENE-PRIME     TO ELBENE2-PRIME-ALT.
01166
01167  5210-BENE-KEY-BUILT.
01168
01169      MOVE LOW-VALUES             TO EL114BO.
01170      MOVE ONE                    TO NAME-CNT.
01171
01172      
      * EXEC CICS HANDLE CONDITION
01173 *        ENDFILE (5230-END-BROWSE)
01174 *    END-EXEC.
      *    MOVE '"$''                   ! & #00004406' TO DFHEIV0
           MOVE X'222427202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303034343036' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01175
01176      
      * EXEC CICS STARTBR
01177 *        DATASET (ELBENE2-FILEID)
01178 *        RIDFLD  (ELBENE2-KEY)
01179 *        GTEQ
01180 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00004410' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343130' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELBENE2-FILEID, 
                 ELBENE2-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01181
01182  5220-READNEXT-LOOP.
01183
01184      
      * EXEC CICS READNEXT
01185 *        DATASET(ELBENE2-FILEID)
01186 *        SET    (ADDRESS OF BENEFICIARY-MASTER)
01187 *        RIDFLD (ELBENE2-KEY)
01188 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00004418' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELBENE2-FILEID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELBENE2-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF BENEFICIARY-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01189
01190      IF BE-COMPANY-CD  NOT EQUAL PI-COMPANY-CD
01191          GO TO 5230-END-BROWSE.
01192
01193      IF BE-RECORD-TYPE NOT EQUAL 'B'
01194          GO TO 5230-END-BROWSE.
01195
01196      MOVE BE-MAIL-TO-NAME-A1     TO WS-READ-NAME.
01197
01198      PERFORM 9999-DUMMY-ROUTINE THRU 9999-EXIT
01199          VARYING NAME-IDX FROM ONE BY ONE
01200              UNTIL NAME-IDX GREATER THAN PI-NAME-LENGTH OR
01201                  WS-INPUT-CHAR (NAME-IDX) NOT EQUAL
01202                      WS-READ-CHAR (NAME-IDX).
01203
01204      IF NAME-IDX NOT GREATER THAN PI-NAME-LENGTH
01205          GO TO 5230-END-BROWSE.
01206
01207      IF NAME-CNT EQUAL ONE
01208          MOVE BE-MAIL-TO-NAME-A1 TO PI-LOW-BENE-NAME
01209          MOVE BE-BENEFICIARY     TO PI-LOW-BENE-PRIME.
01210
01211      MOVE TBL-LINE-NUMBER (NAME-CNT)
01212                                  TO MAP-LINE-NUMBER (NAME-CNT).
01213      MOVE BE-MAIL-TO-NAME-A1     TO MAP-BENE-NAME (NAME-CNT)
01214                                     PI-HIGH-BENE-NAME.
01215      MOVE BE-BENEFICIARY         TO PI-HIGH-BENE-PRIME
01216                                     MAP-BENE-CNTL (NAME-CNT).
01217      MOVE BE-CITY-STATE          TO MAP-BENE-CITY (NAME-CNT).
01218      MOVE NAME-CNT               TO PI-MAX-SELECTION.
01219      ADD ONE                     TO NAME-CNT.
01220
01221      IF NAME-CNT LESS THAN NINE
01222          GO TO 5220-READNEXT-LOOP.
01223
01224  5230-END-BROWSE.
01225
01226      
      * EXEC CICS ENDBR
01227 *        DATASET (ELBENE2-FILEID)
01228 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00004460' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELBENE2-FILEID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01229
01230      IF NAME-CNT LESS THAN EIGHT
01231          MOVE ER-0130            TO EMI-ERROR
01232          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01233
01234      MOVE 'F'                    TO PI-PREV-DIRECTION.
01235      MOVE MAP-NAME-B             TO WS-MAP-NAME
01236                                     PI-PREV-MAP-NAME.
01237      MOVE -1                     TO BSELL.
01238      GO TO 8100-SEND-INITIAL-MAP.
01239
01240      EJECT
01241  5300-ROLL-PREV-BENEFICIARY.
01242
01243      IF PI-MAX-SELECTION LESS THAN MAP-MAXIMUM AND
01244          PI-PREV-BACKWARD
01245              MOVE ER-0130        TO EMI-ERROR
01246              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01247              MOVE -1             TO BPFKL
01248              MOVE AL-UABON       TO BPFKA
01249              GO TO 8200-SEND-DATAONLY.
01250
01251      MOVE PI-COMPANY-CD          TO ELBENE2-COMPANY-CD.
01252      MOVE 'B'                    TO ELBENE2-RECORD-TYPE.
01253
01254      MOVE PI-LOW-BENE-NAME       TO ELBENE2-BENE-NAME
01255                                     WS-INPUT-NAME.
01256      MOVE PI-LOW-BENE-PRIME      TO ELBENE2-PRIME-ALT.
01257
01258  5310-BENE-KEY-BUILT.
01259
01260      MOVE LOW-VALUES             TO EL114BO.
01261      MOVE EIGHT                  TO NAME-CNT.
01262
01263      
      * EXEC CICS HANDLE CONDITION
01264 *        ENDFILE (5330-END-BROWSE)
01265 *    END-EXEC.
      *    MOVE '"$''                   ! '' #00004497' TO DFHEIV0
           MOVE X'222427202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303034343937' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01266
01267      
      * EXEC CICS STARTBR
01268 *        DATASET (ELBENE2-FILEID)
01269 *        RIDFLD  (ELBENE2-KEY)
01270 *        GTEQ
01271 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00004501' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034353031' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELBENE2-FILEID, 
                 ELBENE2-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01272
01273  5320-READPREV-LOOP.
01274
01275      
      * EXEC CICS READPREV
01276 *        DATASET(ELBENE2-FILEID)
01277 *        SET    (ADDRESS OF BENEFICIARY-MASTER)
01278 *        RIDFLD (ELBENE2-KEY)
01279 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00004509' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303034353039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELBENE2-FILEID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELBENE2-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF BENEFICIARY-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01280
01281      IF BE-COMPANY-CD  NOT EQUAL PI-COMPANY-CD
01282          GO TO 5330-END-BROWSE.
01283
01284      IF BE-RECORD-TYPE NOT EQUAL 'B'
01285          GO TO 5330-END-BROWSE.
01286
01287      MOVE BE-MAIL-TO-NAME-A1     TO WS-READ-NAME.
01288
01289      PERFORM 9999-DUMMY-ROUTINE THRU 9999-EXIT
01290          VARYING NAME-IDX FROM ONE BY ONE
01291              UNTIL NAME-IDX GREATER THAN PI-NAME-LENGTH OR
01292                  WS-INPUT-CHAR (NAME-IDX) NOT EQUAL
01293                      WS-READ-CHAR (NAME-IDX).
01294
01295      IF NAME-IDX NOT GREATER THAN PI-NAME-LENGTH
01296          GO TO 5330-END-BROWSE.
01297
01298      IF NAME-CNT EQUAL EIGHT
01299          MOVE BE-MAIL-TO-NAME-A1 TO PI-HIGH-BENE-NAME
01300          MOVE BE-BENEFICIARY     TO PI-HIGH-BENE-PRIME.
01301
01302      MOVE TBL-LINE-NUMBER (NAME-CNT)
01303                                  TO MAP-LINE-NUMBER (NAME-CNT).
01304      MOVE BE-MAIL-TO-NAME-A1     TO MAP-BENE-NAME (NAME-CNT)
01305                                     PI-LOW-BENE-NAME.
01306      MOVE BE-BENEFICIARY         TO PI-LOW-BENE-PRIME
01307                                     MAP-BENE-CNTL (NAME-CNT).
01308      MOVE BE-CITY-STATE          TO MAP-BENE-CITY (NAME-CNT).
01309      SUBTRACT ONE              FROM NAME-CNT.
01310
01311      IF NAME-CNT GREATER THAN +0
01312          GO TO 5320-READPREV-LOOP.
01313
01314  5330-END-BROWSE.
01315
01316      
      * EXEC CICS ENDBR
01317 *        DATASET (ELBENE2-FILEID)
01318 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00004550' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034353530' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELBENE2-FILEID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01319
01320      IF NAME-CNT NOT EQUAL +0
01321          ADD +1                  TO NAME-CNT
01322              PERFORM 5400-REARRANGE-SCREEN THRU 5400-EXIT
01323                  VARYING SCREEN-CNT FROM ONE BY ONE
01324                      UNTIL NAME-CNT GREATER THAN EIGHT
01325      ELSE
01326          MOVE EIGHT              TO PI-MAX-SELECTION.
01327
01328      IF PI-MAX-SELECTION LESS THAN EIGHT
01329          MOVE PI-MAX-SELECTION   TO SCREEN-CNT
01330          PERFORM 5500-FILL-REMAINING-SLOTS THRU 5550-EXIT
01331          MOVE ER-0130            TO EMI-ERROR
01332          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01333
01334
01335      MOVE 'B'                    TO PI-PREV-DIRECTION.
01336      MOVE MAP-NAME-B             TO WS-MAP-NAME
01337                                     PI-PREV-MAP-NAME.
01338      MOVE -1                     TO BSELL.
01339      GO TO 8100-SEND-INITIAL-MAP.
01340
01341      EJECT
01342  5400-REARRANGE-SCREEN.
01343
01344      MOVE TBL-LINE-NUMBER (SCREEN-CNT)
01345                                  TO MAP-LINE-NUMBER (SCREEN-CNT).
01346      MOVE MAP-BENE-NAME (NAME-CNT)
01347                                  TO MAP-BENE-NAME (SCREEN-CNT).
01348      MOVE MAP-BENE-CITY (NAME-CNT)
01349                                  TO MAP-BENE-CITY (SCREEN-CNT).
01350      MOVE MAP-BENE-CNTL (NAME-CNT)
01351                                  TO MAP-BENE-CNTL (SCREEN-CNT).
01352
01353      MOVE SPACES                 TO MAP-LINE-NUMBER (NAME-CNT)
01354                                     MAP-BENE-NAME   (NAME-CNT)
01355                                     MAP-BENE-CITY   (NAME-CNT)
01356                                     MAP-BENE-CNTL   (NAME-CNT).
01357
01358      ADD +1                      TO NAME-CNT.
01359      MOVE SCREEN-CNT             TO PI-MAX-SELECTION.
01360
01361  5400-EXIT.
01362      EXIT.
01363
01364      EJECT
01365  5500-FILL-REMAINING-SLOTS.
01366
01367      MOVE PI-COMPANY-CD          TO ELBENE2-COMPANY-CD.
01368      MOVE 'B'                    TO ELBENE2-RECORD-TYPE.
01369      MOVE MAP-BENE-NAME (SCREEN-CNT)
01370                                  TO ELBENE2-BENE-NAME
01371                                     WS-INPUT-NAME.
01372      MOVE MAP-BENE-CNTL (SCREEN-CNT)
01373                                  TO ELBENE2-PRIME-ALT.
01374
01375      MOVE SCREEN-CNT             TO NAME-CNT.
01376
01377  5510-BENE-KEY-BUILT.
01378
01379      
      * EXEC CICS HANDLE CONDITION
01380 *        ENDFILE (5530-END-BROWSE)
01381 *    END-EXEC.
      *    MOVE '"$''                   ! ( #00004613' TO DFHEIV0
           MOVE X'222427202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303034363133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01382
01383      
      * EXEC CICS STARTBR
01384 *        DATASET (ELBENE2-FILEID)
01385 *        RIDFLD  (ELBENE2-KEY)
01386 *        GTEQ
01387 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00004617' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034363137' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELBENE2-FILEID, 
                 ELBENE2-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01388
01389  5520-READNEXT-LOOP.
01390
01391      
      * EXEC CICS READNEXT
01392 *        DATASET(ELBENE2-FILEID)
01393 *        SET    (ADDRESS OF BENEFICIARY-MASTER)
01394 *        RIDFLD (ELBENE2-KEY)
01395 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00004625' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303034363235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELBENE2-FILEID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELBENE2-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF BENEFICIARY-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01396
01397      IF BE-COMPANY-CD  NOT EQUAL PI-COMPANY-CD
01398          GO TO 5530-END-BROWSE.
01399
01400      IF BE-RECORD-TYPE NOT EQUAL 'B'
01401          GO TO 5530-END-BROWSE.
01402
01403      MOVE BE-MAIL-TO-NAME-A1     TO WS-READ-NAME.
01404
01405      PERFORM 9999-DUMMY-ROUTINE THRU 9999-EXIT
01406          VARYING NAME-IDX FROM ONE BY ONE
01407              UNTIL NAME-IDX GREATER THAN PI-NAME-LENGTH OR
01408                  WS-INPUT-CHAR (NAME-IDX) NOT EQUAL
01409                      WS-READ-CHAR (NAME-IDX).
01410
01411      IF NAME-IDX NOT GREATER THAN PI-NAME-LENGTH
01412          GO TO 5530-END-BROWSE.
01413
01414      MOVE TBL-LINE-NUMBER (NAME-CNT)
01415                                  TO MAP-LINE-NUMBER (NAME-CNT).
01416      MOVE BE-MAIL-TO-NAME-A1     TO MAP-BENE-NAME (NAME-CNT)
01417                                     PI-HIGH-BENE-NAME.
01418      MOVE BE-BENEFICIARY         TO PI-HIGH-BENE-PRIME
01419                                     MAP-BENE-CNTL (NAME-CNT).
01420      MOVE BE-CITY-STATE          TO MAP-BENE-CITY (NAME-CNT).
01421      ADD ONE                     TO NAME-CNT.
01422
01423      IF NAME-CNT LESS THAN NINE
01424          GO TO 5520-READNEXT-LOOP.
01425
01426  5530-END-BROWSE.
01427
01428      
      * EXEC CICS ENDBR
01429 *        DATASET (ELBENE2-FILEID)
01430 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00004662' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034363632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELBENE2-FILEID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01431
01432  5550-EXIT.
01433      EXIT.
01434
01435      EJECT
01436  6000-EDIT-INPUT-DATA.
081312
081312     IF ABENEI EQUAL SPACES
081312        MOVE -1                  TO ABENEL
081312        MOVE ER-7668             TO EMI-ERROR
081312        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
081312     END-IF.
081312
01437      IF ABENAMEL EQUAL ZEROS
01438         MOVE -1                  TO ABENAMEL
01439         MOVE ER-7672             TO EMI-ERROR
01440         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01441
01442      IF AADDR1L EQUAL ZEROS
01443         MOVE -1                  TO AADDR1L
01444         MOVE ER-7673             TO EMI-ERROR
01445         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01446
01447      IF APHONEL GREATER THAN ZERO
01448          
      * EXEC CICS BIF DEEDIT
01449 *            FIELD  (APHONEI)
01450 *            LENGTH (APHONE-LENGTH)
01451 *        END-EXEC
      *    MOVE '@"L                   #   #00004689' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034363839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 APHONEI, 
                 APHONE-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01452          IF APHONEI NUMERIC
01453              MOVE AL-UNNON       TO APHONEA
01454              MOVE APHONEI        TO APHONEO
01455              INSPECT APHONEO CONVERTING SPACES TO '-'
01456          ELSE
01457              MOVE ER-0053        TO EMI-ERROR
01458              PERFORM 9900-ERROR-FORMAT
01459              MOVE AL-UNBON       TO APHONEA
01460              MOVE -1             TO APHONEL.
01461
01462      IF ACITYL EQUAL ZEROS
01463         MOVE -1                  TO ACITYL
01464         MOVE AL-UABON            TO ACITYA
01465         MOVE ER-7674             TO EMI-ERROR
01466         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01467      ELSE
01468         MOVE AL-UANON            TO ACITYA.
01469
01462      IF (ASTATEL = +0)
              AND (AMAINTI = 'A')
01463         MOVE -1                  TO ASTATEL
01464         MOVE AL-UABON            TO ASTATEA
01465         MOVE ER-7674             TO EMI-ERROR
01466         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01467      ELSE
01468         MOVE AL-UANON            TO ASTATEA.
01469
           IF ASTATEL > +0
              MOVE SPACES              TO ELCNTL-KEY
              MOVE PI-COMPANY-ID       TO ELCNTL-COMPANY-ID
              MOVE '3'                 TO ELCNTL-REC-TYPE
              MOVE ASTATEI             TO ELCNTL-ACCESS
              MOVE +0                  TO ELCNTL-SEQ
              
      * EXEC CICS READ
      *          DATASET   (FILE-ID-ELCNTL)
      *          SET       (ADDRESS OF CONTROL-FILE)
      *          RIDFLD    (ELCNTL-KEY)
      *          RESP      (WS-RESPONSE)
      *       END-EXEC
      *    MOVE '&"S        E          (  N#00004726' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303034373236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ELCNTL, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              IF RESP-NORMAL
                 MOVE AL-UANON         TO ASTATEA
              ELSE
                 MOVE ER-2209          TO EMI-ERROR
                 MOVE -1               TO ASTATEL
                 MOVE AL-UABON         TO ASTATEA
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              END-IF
           END-IF
01470      IF AZIPCDEL EQUAL ZEROS
01471         MOVE -1                  TO AZIPCDEL
01472         MOVE AL-UABON            TO AZIPCDEA
01473         MOVE ER-7676             TO EMI-ERROR
01474         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01475      ELSE
01476         MOVE AL-UANON            TO AZIPCDEA.
01477
01478      IF GRPCHKL GREATER THAN +0
01479         IF GRPCHKI EQUAL ' ' OR 'Y' OR 'N'
01480            MOVE AL-UANON               TO GRPCHKA
01481         ELSE
01482            MOVE ER-0658                TO EMI-ERROR
01483            MOVE -1                     TO GRPCHKL
01484            MOVE AL-UABON               TO GRPCHKA
01485            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01486
01487      IF ACORRESL GREATER THAN +0
01488         MOVE AL-UANON                  TO ACORRESA.
01489
01490      IF ACADDR1L GREATER THAN +0
01491         MOVE AL-UANON                  TO  ACADDR1A.
01492
01493      IF ACADDR2L GREATER THAN +0
01494         MOVE AL-UANON                  TO  ACADDR2A.
01495
01496      IF ACADDR3L GREATER THAN +0
01497         MOVE AL-UANON                  TO ACADDR3A.
01498
01499      IF ACCITYL GREATER THAN +0
01500         MOVE AL-UANON                  TO ACCITYA.
           IF ACSTATEL > +0
              MOVE SPACES              TO ELCNTL-KEY
              MOVE PI-COMPANY-ID       TO ELCNTL-COMPANY-ID
              MOVE '3'                 TO ELCNTL-REC-TYPE
              MOVE ACSTATEI            TO ELCNTL-ACCESS
              MOVE +0                  TO ELCNTL-SEQ
              
      * EXEC CICS READ
      *          DATASET   (FILE-ID-ELCNTL)
      *          SET       (ADDRESS OF CONTROL-FILE)
      *          RIDFLD    (ELCNTL-KEY)
      *          RESP      (WS-RESPONSE)
      *       END-EXEC
      *    MOVE '&"S        E          (  N#00004779' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303034373739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ELCNTL, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              IF RESP-NORMAL
                 MOVE AL-UANON         TO ACSTATEA
              ELSE
                 MOVE ER-2209          TO EMI-ERROR
                 MOVE -1               TO ACSTATEL
                 MOVE AL-UABON         TO ACSTATEA
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              END-IF
           END-IF
01502      IF ACZPCDEL GREATER THAN +0
01503         MOVE AL-UANON                  TO ACZPCDEA.
01504
01505      IF ACPHONEL GREATER THAN ZERO
01506          
      * EXEC CICS BIF DEEDIT
01507 *            FIELD  (ACPHONEI)
01508 *            LENGTH (APHONE-LENGTH)
01509 *        END-EXEC
      *    MOVE '@"L                   #   #00004799' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACPHONEI, 
                 APHONE-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01510          IF ACPHONEI NUMERIC
01511              MOVE AL-UNNON       TO ACPHONEA
01512              MOVE ACPHONEI       TO ACPHONEO
01513              INSPECT ACPHONEO CONVERTING SPACES TO '-'
01514          ELSE
01515              MOVE ER-0053        TO EMI-ERROR
01516              PERFORM 9900-ERROR-FORMAT
01517              MOVE AL-UNBON       TO ACPHONEA
01518              MOVE -1             TO ACPHONEL.
01519
01549
01550      IF AFAXNOL   GREATER THAN +0
01551          
      * EXEC CICS BIF DEEDIT
01552 *            FIELD  (AFAXNOI)
01553 *            LENGTH (APHONE-LENGTH)
01554 *        END-EXEC
      *    MOVE '@"L                   #   #00004815' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AFAXNOI, 
                 APHONE-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01555          IF AFAXNOI NUMERIC
01556              MOVE AL-UNNON       TO AFAXNOA
01557              MOVE AFAXNOI        TO AFAXNOO
01558              INSPECT AFAXNOO CONVERTING SPACES TO '-'
01559          ELSE
01560              MOVE ER-0053        TO EMI-ERROR
01561              PERFORM 9900-ERROR-FORMAT
01562              MOVE AL-UNBON       TO AFAXNOA
01563              MOVE -1             TO AFAXNOL.
01564
01565      IF ACBSOTL   GREATER THAN +0
01566         IF ACBSOTI EQUAL ' ' OR 'F' OR 'P'
01567            MOVE AL-UANON               TO ACBSOTA
01568         ELSE
01569            MOVE ER-8125                TO EMI-ERROR
01570            MOVE -1                     TO ACBSOTL
01571            MOVE AL-UABON               TO ACBSOTA
01572            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
           IF AACHYNL > +0
              IF AACHYNI = ' ' OR 'Y' OR 'N'
                 MOVE AL-UANON         TO AACHYNA
              ELSE
032019           MOVE ER-7665          TO EMI-ERROR
                 MOVE AL-UABON         TO AACHYNA
                 MOVE -1               TO AACHYNL
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              END-IF
           END-IF
032019     IF AACHEYNL > +0
032019        IF AACHEYNI = ' ' OR 'Y' OR 'N'
032019           MOVE AL-UANON         TO AACHEYNA
032019        ELSE
032019           MOVE ER-7664          TO EMI-ERROR
032019           MOVE AL-UABON         TO AACHEYNA
032019           MOVE -1               TO AACHEYNL
032019           PERFORM 9900-ERROR-FORMAT
032019                                 THRU 9900-EXIT
032019        END-IF
032019     END-IF
           if asubtypl > +0
              move al-uanon            to asubtypa
           end-if
032019     if aemaill > +0
032019        move al-uanon            to aemaila
032019        move aemaili             to ws-work-email-address
032019        if ws-work-email-address not = spaces
032019           perform 6100-check-email-reasonability
032019                                 thru 6100-exit
032019           if invalid-email
032019              move er-7667       to emi-error
032019              MOVE AL-UABON      TO Aemaila
032019              MOVE -1            TO Aemaill
032019              PERFORM 9900-ERROR-FORMAT
032019                                 THRU 9900-EXIT
032019           end-if
032019        end-if
032019     end-if
032019     if aacheynl > +0
032019        and aacheyni = 'Y'
032019        and (aemaili = spaces or aemaill = +0)
032019        move er-7663             to emi-error
032019        MOVE AL-UABON            TO Aemaila
032019        MOVE -1                  TO Aemaill
032019        PERFORM 9900-ERROR-FORMAT
032019                                 THRU 9900-EXIT
032019     end-if
           IF AABANOL > +0
              MOVE AL-UANON            TO AABANOA
           END-IF
           IF AACCTNOL > +0
032019        if pi-approval-level = '4' or '5'
032019           MOVE AL-UANON         TO aacctnoa
032019        else
032019           move er-2565          to emi-error
032019           move -1               to aacctnol
032019           MOVE AL-UABON         TO aacctnoa
032019           PERFORM 9900-ERROR-FORMAT
032019                                 THRU 9900-EXIT
032019        end-if
           END-IF
032019     if amainti = 'A'
032019        go to 6000-exit
032019     end-if
032019
032019     display ' made it to edit change ' abenei
032019     MOVE PI-COMPANY-CD          TO ELBENE-COMPANY-CD
032019     MOVE 'B'                    TO ELBENE-RECORD-TYPE
032019     MOVE ABENEI                 TO ELBENE-BENEFICIARY
032019
032019     
      * EXEC CICS READ
032019*        DATASET(ELBENE-FILEID)
032019*        SET    (ADDRESS OF BENEFICIARY-MASTER)
032019*        RIDFLD (ELBENE-KEY)
032019*        resp   (ws-response)
032019*    END-EXEC
      *    MOVE '&"S        E          (  N#00004909' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303034393039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELBENE-FILEID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELBENE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF BENEFICIARY-MASTER TO
               DFHEIV20
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
032019
032019     if not resp-normal
032019        go to 6000-exit
032019     end-if
032019
032019     if (aacheyni = 'Y')
032019        and ((aemaili = spaces or low-values)
032019                        and
032019        (be-ach-email-addr = spaces or low-values))
032019        move er-7663             to emi-error
032019        MOVE AL-UABON            TO Aemaila
032019        MOVE -1                  TO Aemaill
032019        PERFORM 9900-ERROR-FORMAT
032019                                 THRU 9900-EXIT
032019     end-if
032019
032019     if (aacheynl = zeros)
032019        and (BE-ACH-EMAIL-YN = 'Y')
032019        and (aemaili = spaces or low-values)
032019        move er-7663             to emi-error
032019        MOVE AL-UABON            TO Aemaila
032019        MOVE -1                  TO Aemaill
032019        PERFORM 9900-ERROR-FORMAT
032019                                 THRU 9900-EXIT
032019     end-if
032019
           .
01574  6000-EXIT.
01575      EXIT.
032019 6100-check-email-reasonability.
032019
032019     move zeros                  to ws-tally-counter
032019     move 9                      to ws-email-sw
032019
032019     inspect ws-work-email-address
032019        tallying ws-tally-counter for all '@'
032019     if ws-tally-counter not = +1
032019        set invalid-email to true
032019        go to 6100-exit
032019     end-if
032019     perform varying e1 from +35 by -1 until
032019        ws-work-email-address(e1:1) not = spaces
032019     end-perform
032019     if e1 < +5
032019        set invalid-email to true
032019        go to 6100-exit
032019     end-if
032019     if ws-work-email-address (e1 - 3:4) not = '.COM' and
032019        '.ORG' and '.NET'
032019        set invalid-email to true
032019        go to 6100-exit
032019     end-if
032019
032019     set valid-email to true
032019
032019     .
032019 6100-exit.
032019     exit.
01578  7000-BUILD-OUTPUT-MAP.
01579
01580      MOVE LOW-VALUES             TO EL114AO.
01581      MOVE BE-BENEFICIARY         TO ABENEO.
01582      MOVE BE-MAIL-TO-NAME        TO ABENAMEO.
01583      MOVE BE-ADDRESS-LINE-1      TO AADDR1O.
01584      MOVE BE-ADDRESS-LINE-2      TO AADDR2O.
01585      MOVE BE-ADDRESS-LINE-3      TO AADDR3O.
01586
01587      IF BE-PHONE-NO NOT NUMERIC
01588          MOVE ZEROS              TO BE-PHONE-NO.
01589
01590      MOVE BE-PHONE-NO            TO APHONEO.
01591
01592      INSPECT APHONEO CONVERTING SPACES TO '-'.
01593
01594      MOVE BE-CITY                TO ACITYO
           MOVE BE-STATE               TO ASTATEO
01595
01596      MOVE SPACES                 TO WS-ZIP-CODE.
01597      IF BE-CANADIAN-POST-CODE
01598          MOVE BE-CAN-POSTAL-1    TO WS-ZIP-CAN-2-POST1
01599          MOVE BE-CAN-POSTAL-2    TO WS-ZIP-CAN-2-POST2
01600      ELSE
01601          MOVE BE-ZIP-PRIME       TO WS-ZIP-AM-2-CODE
01602          IF BE-ZIP-PLUS4 NOT = SPACES  AND  ZEROS
01603              MOVE '-'            TO WS-ZIP-AM-2-DASH
01604              MOVE BE-ZIP-PLUS4   TO WS-ZIP-AM-2-PLUS4.
01605
01606      MOVE WS-ZIP-CODE            TO AZIPCDEO.
01607
01608      MOVE BE-LAST-MAINT-BY       TO ALUBYO.
01609      MOVE ' '                    TO DC-OPTION-CODE.
01610      MOVE BE-LAST-MAINT-DT       TO DC-BIN-DATE-1.
01611      MOVE LINK-ELDATCV           TO PGM-NAME.
01612      
      * EXEC CICS LINK
01613 *        PROGRAM (PGM-NAME)
01614 *        COMMAREA(DATE-CONVERSION-DATA)
01615 *        LENGTH  (DC-COMM-LENGTH)
01616 *    END-EXEC.
      *    MOVE '."C                   (   #00005008' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01617
01618      IF DATE-CONVERSION-ERROR
01619          MOVE ZEROS              TO ALUDATEO
01620      ELSE
01621          MOVE DC-GREG-DATE-1-EDIT TO ALUDATEO.
01622
01623      MOVE BE-LAST-MAINT-HHMMSS   TO ALUTIMEO.
01624      INSPECT ALUTIMEI CONVERTING SPACES TO '.'.
01625      MOVE -1                     TO AMAINTL.
01626      MOVE BE-LAST-MAINT-BY       TO PI-UPDATE-BY.
01627      MOVE BE-LAST-MAINT-HHMMSS   TO PI-UPDATE-HHMMSS.
01628      MOVE BE-BENEFICIARY         TO PI-PREV-BENEFICIARY.
01629      MOVE AL-UANOF               TO AMAINTA.
01630      MOVE AL-UANON               TO ABENEA.
01631
01632      IF BE-GROUP-CHECKS-Y-N EQUAL 'Y'
01633         MOVE 'Y'                 TO GRPCHKO
01634      ELSE
01635         MOVE 'N'                 TO GRPCHKO.
01636
01637      MOVE BE-MAIL-TO-NAME2       TO ACORRESO.
01638      MOVE BE-ADDRESS-LINE-12     TO ACADDR1O.
01639      MOVE BE-ADDRESS-LINE-22     TO ACADDR2O.
01640      MOVE BE-ADDRESS-LINE-32     TO ACADDR3O.
01641      MOVE BE-CITY2               TO ACCITYO
01641      MOVE BE-STATE2              TO ACSTATEO
01642
01643      MOVE SPACES                 TO WS-ZIP-CODE.
01644      IF BE-CANADIAN-POST-CODE2
01645          MOVE BE-CAN-POSTAL-12   TO WS-ZIP-CAN-2-POST1
01646          MOVE BE-CAN-POSTAL-22   TO WS-ZIP-CAN-2-POST2
01647      ELSE
01648          MOVE BE-ZIP-PRIME2      TO WS-ZIP-AM-2-CODE
01649          IF BE-ZIP-PLUS42 NOT = SPACES AND ZEROS
01650              MOVE '-'            TO WS-ZIP-AM-2-DASH
01651              MOVE BE-ZIP-PLUS42  TO WS-ZIP-AM-2-PLUS4.
01652
01653      MOVE WS-ZIP-CODE            TO ACZPCDEO.
01654
01655      IF BE-PHONE-NO2 NOT NUMERIC
01656          MOVE ZEROS              TO BE-PHONE-NO2.
01657      MOVE BE-PHONE-NO2           TO ACPHONEO.
01658      INSPECT ACPHONEO CONVERTING SPACES TO '-'.
01659
032019*    IF BE-BSR-PHONE-NUM NOT NUMERIC
032019*       MOVE ZEROS               TO BE-BSR-PHONE-NUM.
01668
01669      IF BE-BSR-FAX-NUM NOT NUMERIC
01670         MOVE ZEROS               TO BE-BSR-FAX-NUM.
01671      MOVE BE-BSR-FAX-NUM         TO AFAXNOO.
01672      INSPECT AFAXNOO CONVERTING SPACES TO '-'.
01673
032019*    MOVE BE-OUTPUT-TYPE         TO ACBSOTO.
           IF BE-ACH-YES-OR-NO = 'Y'
              MOVE 'Y'                 TO AACHYNO
           ELSE
              MOVE 'N'                 TO AACHYNO
           END-IF
032019     if be-ach-email-yn = 'Y'
032019        move 'Y'                 to aacheyno
032019     else
032019        move 'N'                 to aacheyno
032019     end-if
032019
032019     if be-ach-email-addr <> spaces
032019        move be-ach-email-addr   to aemailo
032019     end-if
           move be-ach-sub-type        to asubtypo
           MOVE BE-ACH-ABA-ROUTING-NUMBER
                                       TO AABANOO
032019     move be-ach-email-addr      to aemailo
           if pi-approval-level = '4' or '5'
              MOVE BE-ACH-BANK-ACCOUNT-NUMBER
                                       TO AACCTNOO
           else
              if be-ach-bank-account-number not = spaces
                 move '********************'
                                       to aacctnoo
              end-if
           end-if
01676      IF (PI-COMPANY-ID IS EQUAL TO 'AIG' OR 'AUK')
01677          MOVE BE-CARRIER         TO ACARRO.
01678
01679      EJECT
01680  8100-SEND-INITIAL-MAP.
01681
01682      IF WS-MAP-NAME IS EQUAL TO MAP-NAME-A
01683          IF (PI-COMPANY-ID IS EQUAL TO 'AIG' OR 'AUK')
01684              NEXT SENTENCE
01685          ELSE
01686              MOVE AL-PADOF         TO ACARRA
01687              MOVE SPACES           TO ACARHDGO
01688                                       ACARRO.
01689
01690      IF WS-MAP-NAME EQUAL MAP-NAME-A
01691          MOVE EMI-MESSAGE-AREA (1) TO AEMSG1O
01692          MOVE EIBTIME              TO TIME-IN
01693          MOVE SAVE-DATE            TO ADATEO
01694          MOVE TIME-OUT             TO ATIMEO
01695          MOVE PI-COMPANY-ID        TO ACOMPO
01696      ELSE
01697          MOVE EMI-MESSAGE-AREA (1) TO BEMSG1O
01698          MOVE EIBTIME              TO TIME-IN
01699          MOVE SAVE-DATE            TO BDATEO
01700          MOVE TIME-OUT             TO BTIMEO
01701          MOVE PI-COMPANY-ID        TO BCOMPO.
01702
013017     if ws-map-name = map-name-a
              if pi-approval-level = '4' or '5'
                 continue
              else
                 move al-panof         to aachyna
                                          aabanoa
                                          asubtypa
                 move al-padof         to aacctnoa
              end-if
           end-if
01703      
      * EXEC CICS SEND
01704 *        MAP   (WS-MAP-NAME)
01705 *        MAPSET(MAPSET-NAME)
01706 *        FROM  (EL114BO)
01707 *        ERASE
01708 *        CURSOR
01709 *    END-EXEC.
           MOVE LENGTH OF
            EL114BO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00005130' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL114BO, 
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
           
01710
01711      GO TO 9100-RETURN-TRAN.
01712
01713      EJECT
01714  8200-SEND-DATAONLY.
01715
01716      IF WS-MAP-NAME IS EQUAL TO MAP-NAME-A
01717          IF (PI-COMPANY-ID IS EQUAL TO 'AIG' OR 'AUK')
01718              NEXT SENTENCE
01719          ELSE
01720              MOVE AL-PADOF         TO ACARRA
01721              MOVE SPACES           TO ACARHDGO
01722                                       ACARRO.
01723
01724      IF WS-MAP-NAME EQUAL MAP-NAME-A
01725          MOVE EMI-MESSAGE-AREA (1) TO AEMSG1O
01726          MOVE EIBTIME              TO TIME-IN
01727          MOVE SAVE-DATE            TO ADATEO
01728          MOVE TIME-OUT             TO ATIMEO
01729          MOVE PI-COMPANY-ID        TO ACOMPO
01730      ELSE
01731          MOVE EMI-MESSAGE-AREA (1) TO BEMSG1O
01732          MOVE EIBTIME              TO TIME-IN
01733          MOVE SAVE-DATE            TO BDATEO
01734          MOVE TIME-OUT             TO BTIMEO
01735          MOVE PI-COMPANY-ID        TO BCOMPO.
01736
01737      
      * EXEC CICS SEND
01738 *        MAP   (WS-MAP-NAME)
01739 *        MAPSET(MAPSET-NAME)
01740 *        FROM  (EL114BO)
01741 *        DATAONLY
01742 *        ERASEAUP
01743 *        CURSOR
01744 *    END-EXEC.
           MOVE LENGTH OF
            EL114BO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT  A    H L F ,   #00005164' TO DFHEIV0
           MOVE X'382444202020204354202041' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL114BO, 
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
           
01745
01746      GO TO 9100-RETURN-TRAN.
01747      EJECT
01748  8300-SEND-TEXT.
01749      
      * EXEC CICS SEND TEXT
01750 *        FROM  (LOGOFF-TEXT)
01751 *        LENGTH(LOGOFF-LENGTH)
01752 *        ERASE
01753 *        FREEKB
01754 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00005176' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313736' TO DFHEIV0(25:11)
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
           
01755
01756      
      * EXEC CICS RETURN
01757 *        END-EXEC.
      *    MOVE '.(                    ''   #00005183' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313833' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01758
01759      EJECT
01760  8800-UNAUTHORIZED-ACCESS.
01761      MOVE UNACCESS-MSG           TO LOGOFF-MSG.
01762      GO TO 8300-SEND-TEXT.
01763
01764  8810-PF23.
01765      MOVE EIBAID                 TO PI-ENTRY-CD-1.
01766      MOVE XCTL-005               TO PGM-NAME.
01767      GO TO 9300-XCTL.
01768
01769  8850-DUPREC.
01770      MOVE ER-0132                TO EMI-ERROR.
01771      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01772      MOVE -1                     TO ABENEL.
01773      MOVE AL-UABON               TO ABENEA.
01774      GO TO 8100-SEND-INITIAL-MAP.
01775
01776  8860-ENDFILE.
01777
01778      IF WS-MAP-NAME EQUAL MAP-NAME-A
01779          MOVE -1                 TO AMAINTL
01780      ELSE
01781          MOVE -1                 TO BPFKL.
01782
01783      MOVE ER-0130                TO EMI-ERROR.
01784      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01785      GO TO 8200-SEND-DATAONLY.
01786
01787  8870-NOTOPEN.
01788
01789      IF WS-MAP-NAME EQUAL MAP-NAME-A
01790          MOVE LOW-VALUES         TO EL114AO
01791          MOVE -1                 TO AMAINTL
01792      ELSE
01793          MOVE LOW-VALUES         TO EL114BO
01794          MOVE -1                 TO BPFKL.
01795
01796      MOVE ER-7675                TO EMI-ERROR.
01797      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01798      GO TO 8100-SEND-INITIAL-MAP.
01799
01800  8880-NOT-FOUND.
01801      MOVE ER-0565                TO EMI-ERROR.
01802      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01803      MOVE -1                     TO ABENEL.
01804      MOVE AL-UABON               TO ABENEA.
01805      MOVE AL-UANON               TO AMAINTA.
01806      GO TO 8100-SEND-INITIAL-MAP.
01807
01808  9000-RETURN-CICS.
01809      
      * EXEC CICS RETURN
01810 *    END-EXEC.
      *    MOVE '.(                    ''   #00005236' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01811
01812      EJECT
01813  9100-RETURN-TRAN.
01814      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.
01815      MOVE '114A' TO PI-CURRENT-SCREEN-NO.
01816
01817      
      * EXEC CICS RETURN
01818 *        TRANSID (TRANS-ID)
01819 *        COMMAREA(PROGRAM-INTERFACE-BLOCK)
01820 *        LENGTH  (PI-COMM-LENGTH)
01821 *    END-EXEC.
      *    MOVE '.(CT                  ''   #00005244' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01822
01823  9200-RETURN-MAIN-MENU.
01824      MOVE XCTL-126               TO PGM-NAME.
01825
01826      IF PI-SESSION-IN-PROGRESS EQUAL '4'
01827          MOVE XCTL-400           TO PGM-NAME.
01828
01829      IF PI-SESSION-IN-PROGRESS EQUAL '5'
01830          MOVE XCTL-800           TO PGM-NAME.
01831
01832      GO TO 9300-XCTL.
01833
01834  9300-XCTL.
01835      
      * EXEC CICS XCTL
01836 *        PROGRAM (PGM-NAME)
01837 *        COMMAREA(PROGRAM-INTERFACE-BLOCK)
01838 *        LENGTH  (PI-COMM-LENGTH)
01839 *    END-EXEC.
      *    MOVE '.$C                   %   #00005262' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01840
01841  9400-CLEAR.
01842      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME.
01843      GO TO 9300-XCTL.
01844
01845  9500-PF12.
01846      MOVE XCTL-010               TO PGM-NAME.
01847      GO TO 9300-XCTL.
01848
01849  9600-PGMID-ERROR.
01850      
      * EXEC CICS HANDLE CONDITION
01851 *        PGMIDERR(8300-SEND-TEXT)
01852 *    END-EXEC.
      *    MOVE '"$L                   ! ) #00005277' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303035323737' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01853
01854      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.
01855      MOVE ' '                    TO PI-ENTRY-CD-1.
01856      MOVE XCTL-005               TO PGM-NAME.
01857      MOVE PGM-NAME               TO LOGOFF-PGM.
01858      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.
01859      GO TO 9300-XCTL.
01860
01861      EJECT
01862  9700-LINK-DATE-CONVERT.
01863      
      * EXEC CICS LINK
01864 *        PROGRAM    ('ELDATCV')
01865 *        COMMAREA   (DATE-CONVERSION-DATA)
01866 *        LENGTH     (DC-COMM-LENGTH)
01867 *    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   (   #00005290' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323930' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01868
01869  9700-EXIT.
01870      EXIT.
01871
01872
01873
01874  9900-ERROR-FORMAT.
01875      IF NOT EMI-ERRORS-COMPLETE
01876          MOVE LINK-001           TO PGM-NAME
01877          
      * EXEC CICS LINK
01878 *            PROGRAM(PGM-NAME)
01879 *            COMMAREA(ERROR-MESSAGE-INTERFACE-BLOCK)
01880 *            LENGTH(EMI-COMM-LENGTH)
01881 *        END-EXEC.
      *    MOVE '."C                   (   #00005304' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01882
01883  9900-EXIT.
01884      EXIT.
01885
01886
01887
01888
01889  9990-ABEND.
01890      MOVE LINK-004               TO PGM-NAME.
01891      MOVE DFHEIBLK               TO EMI-LINE1.
01892      
      * EXEC CICS LINK
01893 *        PROGRAM   (PGM-NAME)
01894 *        COMMAREA  (EMI-LINE1)
01895 *        LENGTH    (72)
01896 *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00005319' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333139' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01897
01898      GO TO 8100-SEND-INITIAL-MAP.
01899
01900      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL114' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
01901
01902      EJECT
01903  9995-SECURITY-VIOLATION.
01904 *                            COPY ELCSCTP.
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
      *    MOVE '."C                   (   #00005348' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333438' TO DFHEIV0(25:11)
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
01905
01906  9995-EXIT.
01907      EXIT.
01908
01909  9999-DUMMY-ROUTINE.
01910
01911  9999-EXIT.
01912      EXIT.
01913

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL114' TO DFHEIV1
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
               GO TO 0510-BENEFICIARY-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 5000-UNSUCCESSFUL-SEARCH
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 5100-UNSUCCESSFUL-SEARCH
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 5230-END-BROWSE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 5330-END-BROWSE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 5530-END-BROWSE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL114' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
