00001  ID DIVISION.
00002
00003  PROGRAM-ID.                 EL158.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 02/13/96 09:56:35.
00007 *                            VMOD=2.004.
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
00018 *                                                                *
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
00021 *            *                                                   *
00022 *            *****************************************************
00023
00024 *REMARKS.
00025
00026 *        THIS PROGRAM PROVIDES THE MAINTENANCE FUNCTIONS NEEDED
00027 *    FOR THE POLICY FORM FILE RECORDS.
00028
00029 *    SCREENS     - EL158A - FORM MAINTENANCE
00030
00031 *    ENTERED BY  - EL601 - MAINTENANCE MENU
00032
00033 *    EXIT TO     - EL601 - MAINTENANCE MENU
00034
00035 *    INPUT FILE  - ERFORM -              - POLICY FORM FILE
00036
00037 *    OUTPUT FILE - ERFORM -              - POLICY FORM FILE
00038
00039 *    COMMAREA    - PASSED
00040
00041 *    NARRATIVE   - FIRST ENTRY IS VIA AN XCTL FROM EL101.  ON
00042 *                  FIRST ENTRY, A SKELETON SCREEN IS SENT AND THE
00043 *                  PROGRAM EXITS TO WAIT FOR INPUT.  ON SUCCESSIVE
00044 *                  ENTRIES (XCTL FROM CICS VIA EX39) THE SCREEN
00045 *                  WILL BE READ AND ACTION WILL BE BASED ON THE
00046 *                  MAINTENANCE TYPE INDICATED.
00047
00048      EJECT
00049  ENVIRONMENT DIVISION.
00050  DATA DIVISION.
00051  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00052  77  FILLER  PIC X(32)  VALUE '********************************'.
00053  77  FILLER  PIC X(32)  VALUE '*    EL158 WORKING STORAGE     *'.
00054  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.004 *********'.
00055
00056  77  FIRST-READ-PREV-SW              PIC X(01)   VALUE SPACES.
00057      88  FIRST-READ-PREV                         VALUE 'Y'.
00058
00059  01  ACCESS-KEYS.
00060      12  ERFORM-KEY.
00061          16  ERFORM-COMPANY-CD       PIC X(01).
00062          16  ERFORM-STATE            PIC X(02).
00063          16  ERFORM-FORM-ID          PIC X(12).
00064          16  ERFORM-EXP-DT           PIC X(02).
00065
00066      12  ELCNTL-KEY.
00067          16  ELCNTL-COMPANY-ID       PIC X(03).
00068          16  ELCNTL-RECORD-TYPE      PIC X(01).
00069          16  ELCNTL-ACCESS           PIC X(04).
00070          16  ELCNTL-STATE-ACCESS REDEFINES ELCNTL-ACCESS.
00071              20  ELCNTL-STATE-CD     PIC  X(02).
00072              20  FILLER              PIC  X(02).
00073          16  ELCNTL-BENEFIT-ACCESS REDEFINES ELCNTL-ACCESS.
00074              20  FILLER              PIC  X(02).
00075              20  ELCNTL-BENE-CD      PIC  X(02).
00076          16  ELCNTL-SEQUENCE-NO      PIC S9(04)      COMP.
00077
00078  01  WS-DATE-AREA.
00079      05  SAVE-DATE                   PIC X(08)   VALUE SPACES.
00080      05  SAVE-BIN-DATE               PIC X(02)   VALUE SPACES.
00081
00082  01  MISC-WORK-AREAS.
00083
00084      12  DEEDIT-FIELD                PIC X(15).
00085      12  DEEDIT-FIELD-V0 REDEFINES DEEDIT-FIELD
00086                                      PIC S9(15).
00087      12  DEEDIT-FIELD-V2 REDEFINES DEEDIT-FIELD
00088                                      PIC S9(13)V99.
00089
00090      12  WS-BENEFIT-FIELD.
00091          16  WS-BENE-TYPE            PIC X(01).
00092          16  WS-BENE-CODE            PIC X(02).
00093
00094      12  WS-EXP-DT                   PIC X(02)   VALUE LOW-VALUES.
00095
00096  01  STANDARD-AREAS.
00097      12  SC-ITEM                     PIC S9(4)   VALUE +1  COMP.
00098      12  TRANS-ID                    PIC X(04)   VALUE 'E030'.
00099      12  EL150-TRANS-ID              PIC X(04)   VALUE 'EX23'.
00100      12  PGM-NAME                    PIC X(08).
00101      12  TIME-IN                     PIC S9(07).
00102      12  TIME-OUT-R  REDEFINES TIME-IN.
00103          16  FILLER                  PIC X(01).
00104          16  TIME-OUT                PIC 99V99.
00105          16  FILLER                  PIC X(02).
00106      12  XCTL-005                    PIC X(08)   VALUE 'EL005'.
00107      12  XCTL-010                    PIC X(08)   VALUE 'EL010'.
00108      12  XCTL-155                    PIC X(08)   VALUE 'EL155'.
00109      12  XCTL-626                    PIC X(08)   VALUE 'EL626'.
00110      12  LINK-001                    PIC X(08)   VALUE 'EL001'.
00111      12  LINK-004                    PIC X(08)   VALUE 'EL004'.
00112      12  LINK-ELDATCV                PIC X(08)   VALUE 'ELDATCV'.
00113      12  THIS-PGM                    PIC X(08)   VALUE 'EL158'.
00114      12  ERFORM-FILE-ID              PIC X(08)   VALUE 'ERFORM'.
00115      12  ELCNTL-FILE-ID              PIC X(08)   VALUE 'ELCNTL'.
00116      12  ERFORM-LENGTH               PIC S9(04)  VALUE +500  COMP.
00117      12  SUB                         PIC 9(02).
00118      12  SUB-1                       PIC 9(02).
00119      12  SUB2                        PIC 9(02).
00120      12  GETMAIN-SPACE               PIC X(01)   VALUE SPACE.
00121      12  MAPSET-NAME                 PIC X(08)   VALUE 'EL158S'.
00122      12  WS-MAP-NAME                 PIC X(08)   VALUE 'EL158A'.
00123      12  WS-PF-KEY                   PIC 9(02)   VALUE ZEROS.
00124
00125      12  WS-CNTL-REC-FOUND-SW        PIC X(01)   VALUE 'N'.
00126          88  CNTL-RECORD-FOUND                   VALUE 'Y'.
00127
00128      12  WS-PRE-EXIST-CODES          PIC X(02)   VALUE ZEROS.
00129          88  VALID-PRE-EXIST-CODE                VALUES ARE '00'
00130                                                        THRU '99'.
00131
00132      12  WS-SUICIDE-CODES            PIC X(02)   VALUE ZEROS.
00133          88  VALID-SUICIDE-CODE                  VALUES ARE '00'
00134                                                        THRU '99'.
00135
00136      12  WS-DISABILITY-CODES         PIC X(02)   VALUE ZEROS.
00137          88  VALID-DISABILITY-CODE               VALUES ARE '00'
00138                                                        THRU '99'.
00139
00140      12  WS-REFUND-METHOD            PIC X       VALUE ZEROS.
00141          88  VALID-REFUND-METHOD                 VALUES ARE ' ',
00142                                                    '1' THRU '9'.
00143
00144      12  WS-BENEFIT-SW               PIC X       VALUE SPACE.
00145          88  BENEFIT-FOUND                       VALUE 'Y'.
00146          88  BENEFIT-NOT-FOUND                   VALUE 'N'.
00147
00148      12  WS-LFAMT                    PIC S9(9)V99 VALUE +0 COMP-3.
00149      12  WS-AHAMT                    PIC S9(9)V99 VALUE +0 COMP-3.
00150      12  WS-LFMIN                    PIC 9(3)     VALUE  0 COMP-3.
00151      12  WS-AHMIN                    PIC 9(3)     VALUE  0 COMP-3.
00152      12  WS-LFMAX                    PIC 9(3)     VALUE  0 COMP-3.
00153      12  WS-AHMAX                    PIC 9(3)     VALUE  0 COMP-3.
00154      12  WS-LFTRM                    PIC 9(3)     VALUE  0 COMP-3.
00155      12  WS-AHTRM                    PIC 9(3)     VALUE  0 COMP-3.
00156      12  WS-DEFTYP                   PIC 99       VALUE  0.
00157      12  WS-LFPRE                    PIC 99       VALUE  0.
00158      12  WS-AHPRE                    PIC 99       VALUE  0.
00159      12  WS-SUICIDE                  PIC 99       VALUE  0.
00160      12  WS-INIT-TABLE.
00161          16  FILLER                  PIC X(03)  VALUE '01.'.
00162          16  FILLER                  PIC X(03)  VALUE '02.'.
00163          16  FILLER                  PIC X(03)  VALUE '03.'.
00164          16  FILLER                  PIC X(03)  VALUE '04.'.
00165          16  FILLER                  PIC X(03)  VALUE '05.'.
00166          16  FILLER                  PIC X(03)  VALUE '06.'.
00167          16  FILLER                  PIC X(03)  VALUE '07.'.
00168          16  FILLER                  PIC X(03)  VALUE '08.'.
00169          16  FILLER                  PIC X(03)  VALUE '09.'.
00170          16  FILLER                  PIC X(03)  VALUE '10.'.
00171          16  FILLER                  PIC X(03)  VALUE '11.'.
00172          16  FILLER                  PIC X(03)  VALUE '12.'.
00173          16  FILLER                  PIC X(03)  VALUE '13.'.
00174          16  FILLER                  PIC X(03)  VALUE '14.'.
00175          16  FILLER                  PIC X(03)  VALUE '15.'.
00176          16  FILLER                  PIC X(03)  VALUE '16.'.
00177          16  FILLER                  PIC X(03)  VALUE '17.'.
00178          16  FILLER                  PIC X(03)  VALUE '18.'.
00179          16  FILLER                  PIC X(03)  VALUE '19.'.
00180          16  FILLER                  PIC X(03)  VALUE '20.'.
00181          16  FILLER                  PIC X(03)  VALUE '21.'.
00182          16  FILLER                  PIC X(03)  VALUE '22.'.
00183          16  FILLER                  PIC X(03)  VALUE '23.'.
00184          16  FILLER                  PIC X(03)  VALUE '24.'.
00185          16  FILLER                  PIC X(03)  VALUE '25.'.
00186          16  FILLER                  PIC X(03)  VALUE '26.'.
00187          16  FILLER                  PIC X(03)  VALUE '27.'.
00188          16  FILLER                  PIC X(03)  VALUE '28.'.
00189          16  FILLER                  PIC X(03)  VALUE '29.'.
00190          16  FILLER                  PIC X(03)  VALUE '30.'.
00191          16  FILLER                  PIC X(03)  VALUE '31.'.
00192          16  FILLER                  PIC X(03)  VALUE '32.'.
00193          16  FILLER                  PIC X(03)  VALUE '33.'.
00194          16  FILLER                  PIC X(03)  VALUE '34.'.
00195          16  FILLER                  PIC X(03)  VALUE '35.'.
00196          16  FILLER                  PIC X(03)  VALUE '36.'.
00197          16  FILLER                  PIC X(03)  VALUE '37.'.
00198          16  FILLER                  PIC X(03)  VALUE '38.'.
00199          16  FILLER                  PIC X(03)  VALUE '39.'.
00200          16  FILLER                  PIC X(03)  VALUE '40.'.
00201      12  WS-INIT-VALUE REDEFINES WS-INIT-TABLE OCCURS 40 TIMES
00202                                      PIC X(03).
00203      12  WS158-BENE-INIT             PIC X(03) VALUE '01.'.
00204      EJECT
00205 *                                    COPY ELCSCTM.
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
00206 *                                    COPY ELCSCRTY.
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
00207
00208      EJECT
00209  01  ERROR-MESSAGES.
00210      12  ER-0000                     PIC X(04)   VALUE '0000'.
00211      12  ER-0023                     PIC X(04)   VALUE '0023'.
00212      12  ER-0029                     PIC X(04)   VALUE '0029'.
00213      12  ER-0050                     PIC X(04)   VALUE '0050'.
00214      12  ER-0068                     PIC X(04)   VALUE '0068'.
00215      12  ER-0070                     PIC X(04)   VALUE '0070'.
00216      12  ER-0130                     PIC X(04)   VALUE '0130'.
00217      12  ER-0131                     PIC X(04)   VALUE '0131'.
00218      12  ER-0132                     PIC X(04)   VALUE '0132'.
00219      12  ER-0138                     PIC X(04)   VALUE '0138'.
00220      12  ER-0144                     PIC X(04)   VALUE '0144'.
00221      12  ER-0418                     PIC X(04)   VALUE '0418'.
00222      12  ER-0582                     PIC X(04)   VALUE '0582'.
00223      12  ER-0701                     PIC X(04)   VALUE '0701'.
00224      12  ER-0702                     PIC X(04)   VALUE '0702'.
00225      12  ER-0703                     PIC X(04)   VALUE '0703'.
00226      12  ER-0704                     PIC X(04)   VALUE '0704'.
00227      12  ER-0705                     PIC X(04)   VALUE '0705'.
00228      12  ER-0706                     PIC X(04)   VALUE '0706'.
00229      12  ER-0707                     PIC X(04)   VALUE '0707'.
00230      12  ER-0708                     PIC X(04)   VALUE '0708'.
00231      12  ER-0709                     PIC X(04)   VALUE '0709'.
00232      12  ER-0710                     PIC X(04)   VALUE '0710'.
00233      12  ER-0711                     PIC X(04)   VALUE '0711'.
00234      12  ER-0712                     PIC X(04)   VALUE '0712'.
00235      12  ER-0713                     PIC X(04)   VALUE '0713'.
00236      12  ER-0717                     PIC X(04)   VALUE '0717'.
00237      12  ER-0718                     PIC X(04)   VALUE '0718'.
00238      12  ER-0719                     PIC X(04)   VALUE '0719'.
00239      12  ER-0720                     PIC X(04)   VALUE '0720'.
00240      12  ER-0721                     PIC X(04)   VALUE '0721'.
00241      12  ER-0722                     PIC X(04)   VALUE '0722'.
00242      12  ER-0723                     PIC X(04)   VALUE '0723'.
00243      12  ER-0724                     PIC X(04)   VALUE '0724'.
00244      12  ER-0725                     PIC X(04)   VALUE '0725'.
00245      12  ER-0726                     PIC X(04)   VALUE '0726'.
00246      12  ER-0727                     PIC X(04)   VALUE '0727'.
00247      12  ER-0729                     PIC X(04)   VALUE '0729'.
00248      12  ER-0754                     PIC X(04)   VALUE '0754'.
00249      12  ER-2241                     PIC X(04)   VALUE '2241'.
00250      12  ER-2276                     PIC X(04)   VALUE '2276'.
00251      12  ER-7008                     PIC X(04)   VALUE '7008'.
00252      12  ER-7031                     PIC X(04)   VALUE '7031'.
00253      12  ER-7123                     PIC X(04)   VALUE '7123'.
00254      12  ER-8150                     PIC X(04)   VALUE '8150'.
00255      EJECT
00256 *                                    COPY ELCDATE.
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
00257      EJECT
00258 *                                    COPY ELCLOGOF.
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
00259      EJECT
00260 *                                    COPY ELCATTR.
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
00261      EJECT
00262 *                                    COPY ELCEMIB.
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
00263      EJECT
00264 *                                    COPY ELCJPFX.
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
00265                                      PIC X(530).
00266
00267      EJECT
00268 *                                    COPY ELCINTF.
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
00269      12  FILLER    REDEFINES PI-PROGRAM-WORK-AREA.
00270
00271          16  PI-FORM-NUMBER          PIC X(12).
00272
00273          16  PI-PREV-FORM-KEY.
00274              20  PI-PREV-COMPANY-CD  PIC X(01).
00275              20  PI-PREV-STATE       PIC X(02).
00276              20  PI-PREV-FORM-ID     PIC X(12).
00277              20  PI-PREV-EXP-DT      PIC X(02).
00278
00279          16  PI-FORM-KEY.
00280              20  PI-STATE-CODE       PIC X(02).
00281              20  PI-FORM-ID          PIC X(12).
00282              20  PI-EXP-DT           PIC X(08).
00283          16  FILLER                  PIC X(589).
00284
00285      EJECT
00286 *                                    COPY ELCAID.
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
00287  01  FILLER    REDEFINES DFHAID.
00288      12  FILLER                      PIC X(08).
00289      12  PF-VALUES                   PIC X         OCCURS 24.
00290
00291      EJECT
00292 *                                    COPY EL158S.
       01  EL158AI.
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
           05  FORML PIC S9(0004) COMP.
           05  FORMF PIC  X(0001).
           05  FILLER REDEFINES FORMF.
               10  FORMA PIC  X(0001).
           05  FORMI PIC  X(0012).
      *    -------------------------------
           05  EXPDTL PIC S9(0004) COMP.
           05  EXPDTF PIC  X(0001).
           05  FILLER REDEFINES EXPDTF.
               10  EXPDTA PIC  X(0001).
           05  EXPDTI PIC  X(0008).
      *    -------------------------------
           05  INDGRPL PIC S9(0004) COMP.
           05  INDGRPF PIC  X(0001).
           05  FILLER REDEFINES INDGRPF.
               10  INDGRPA PIC  X(0001).
           05  INDGRPI PIC  X(0001).
      *    -------------------------------
           05  MAXATTL PIC S9(0004) COMP.
           05  MAXATTF PIC  X(0001).
           05  FILLER REDEFINES MAXATTF.
               10  MAXATTA PIC  X(0001).
           05  MAXATTI PIC  99.
      *    -------------------------------
           05  DEFTYPL PIC S9(0004) COMP.
           05  DEFTYPF PIC  X(0001).
           05  FILLER REDEFINES DEFTYPF.
               10  DEFTYPA PIC  X(0001).
           05  DEFTYPI PIC  X(0002).
      *    -------------------------------
           05  DISMCDL PIC S9(0004) COMP.
           05  DISMCDF PIC  X(0001).
           05  FILLER REDEFINES DISMCDF.
               10  DISMCDA PIC  X(0001).
           05  DISMCDI PIC  X(0001).
      *    -------------------------------
           05  CRTAPPL PIC S9(0004) COMP.
           05  CRTAPPF PIC  X(0001).
           05  FILLER REDEFINES CRTAPPF.
               10  CRTAPPA PIC  X(0001).
           05  CRTAPPI PIC  X(0001).
      *    -------------------------------
           05  LFHDGL PIC S9(0004) COMP.
           05  LFHDGF PIC  X(0001).
           05  FILLER REDEFINES LFHDGF.
               10  LFHDGA PIC  X(0001).
           05  LFHDGI PIC  X(0002).
      *    -------------------------------
           05  LFMINL PIC S9(0004) COMP.
           05  LFMINF PIC  X(0001).
           05  FILLER REDEFINES LFMINF.
               10  LFMINA PIC  X(0001).
           05  LFMINI PIC  99.
      *    -------------------------------
           05  LFMAXL PIC S9(0004) COMP.
           05  LFMAXF PIC  X(0001).
           05  FILLER REDEFINES LFMAXF.
               10  LFMAXA PIC  X(0001).
           05  LFMAXI PIC  99.
      *    -------------------------------
           05  LFTRML PIC S9(0004) COMP.
           05  LFTRMF PIC  X(0001).
           05  FILLER REDEFINES LFTRMF.
               10  LFTRMA PIC  X(0001).
           05  LFTRMI PIC  999.
      *    -------------------------------
           05  LFAMTL PIC S9(0004) COMP.
           05  LFAMTF PIC  X(0001).
           05  FILLER REDEFINES LFAMTF.
               10  LFAMTA PIC  X(0001).
           05  LFAMTI PIC  9(8)V99.
      *    -------------------------------
           05  LFPREL PIC S9(0004) COMP.
           05  LFPREF PIC  X(0001).
           05  FILLER REDEFINES LFPREF.
               10  LFPREA PIC  X(0001).
           05  LFPREI PIC  99.
      *    -------------------------------
           05  SUICIDEL PIC S9(0004) COMP.
           05  SUICIDEF PIC  X(0001).
           05  FILLER REDEFINES SUICIDEF.
               10  SUICIDEA PIC  X(0001).
           05  SUICIDEI PIC  99.
      *    -------------------------------
           05  AHHDGL PIC S9(0004) COMP.
           05  AHHDGF PIC  X(0001).
           05  FILLER REDEFINES AHHDGF.
               10  AHHDGA PIC  X(0001).
           05  AHHDGI PIC  X(0002).
      *    -------------------------------
           05  AHMINL PIC S9(0004) COMP.
           05  AHMINF PIC  X(0001).
           05  FILLER REDEFINES AHMINF.
               10  AHMINA PIC  X(0001).
           05  AHMINI PIC  99.
      *    -------------------------------
           05  AHMAXL PIC S9(0004) COMP.
           05  AHMAXF PIC  X(0001).
           05  FILLER REDEFINES AHMAXF.
               10  AHMAXA PIC  X(0001).
           05  AHMAXI PIC  99.
      *    -------------------------------
           05  AHTRML PIC S9(0004) COMP.
           05  AHTRMF PIC  X(0001).
           05  FILLER REDEFINES AHTRMF.
               10  AHTRMA PIC  X(0001).
           05  AHTRMI PIC  999.
      *    -------------------------------
           05  AHAMTL PIC S9(0004) COMP.
           05  AHAMTF PIC  X(0001).
           05  FILLER REDEFINES AHAMTF.
               10  AHAMTA PIC  X(0001).
           05  AHAMTI PIC  9(6)V99.
      *    -------------------------------
           05  AHPREL PIC S9(0004) COMP.
           05  AHPREF PIC  X(0001).
           05  FILLER REDEFINES AHPREF.
               10  AHPREA PIC  X(0001).
           05  AHPREI PIC  99.
      *    -------------------------------
           05  INIT1L PIC S9(0004) COMP.
           05  INIT1F PIC  X(0001).
           05  FILLER REDEFINES INIT1F.
               10  INIT1A PIC  X(0001).
           05  INIT1I PIC  X(0003).
      *    -------------------------------
           05  BENE1L PIC S9(0004) COMP.
           05  BENE1F PIC  X(0001).
           05  FILLER REDEFINES BENE1F.
               10  BENE1A PIC  X(0001).
           05  BENE1I PIC  X(0003).
      *    -------------------------------
           05  TERM1L PIC S9(0004) COMP.
           05  TERM1F PIC  X(0001).
           05  FILLER REDEFINES TERM1F.
               10  TERM1A PIC  X(0001).
           05  TERM1I PIC  999.
      *    -------------------------------
           05  REFM1L PIC S9(0004) COMP.
           05  REFM1F PIC  X(0001).
           05  FILLER REDEFINES REFM1F.
               10  REFM1A PIC  X(0001).
           05  REFM1I PIC  X(0001).
      *    -------------------------------
           05  INIT2L PIC S9(0004) COMP.
           05  INIT2F PIC  X(0001).
           05  FILLER REDEFINES INIT2F.
               10  INIT2A PIC  X(0001).
           05  INIT2I PIC  X(0003).
      *    -------------------------------
           05  BENE2L PIC S9(0004) COMP.
           05  BENE2F PIC  X(0001).
           05  FILLER REDEFINES BENE2F.
               10  BENE2A PIC  X(0001).
           05  BENE2I PIC  X(0003).
      *    -------------------------------
           05  TERM2L PIC S9(0004) COMP.
           05  TERM2F PIC  X(0001).
           05  FILLER REDEFINES TERM2F.
               10  TERM2A PIC  X(0001).
           05  TERM2I PIC  999.
      *    -------------------------------
           05  REFM2L PIC S9(0004) COMP.
           05  REFM2F PIC  X(0001).
           05  FILLER REDEFINES REFM2F.
               10  REFM2A PIC  X(0001).
           05  REFM2I PIC  X(0001).
      *    -------------------------------
           05  INIT3L PIC S9(0004) COMP.
           05  INIT3F PIC  X(0001).
           05  FILLER REDEFINES INIT3F.
               10  INIT3A PIC  X(0001).
           05  INIT3I PIC  X(0003).
      *    -------------------------------
           05  BENE3L PIC S9(0004) COMP.
           05  BENE3F PIC  X(0001).
           05  FILLER REDEFINES BENE3F.
               10  BENE3A PIC  X(0001).
           05  BENE3I PIC  X(0003).
      *    -------------------------------
           05  TERM3L PIC S9(0004) COMP.
           05  TERM3F PIC  X(0001).
           05  FILLER REDEFINES TERM3F.
               10  TERM3A PIC  X(0001).
           05  TERM3I PIC  999.
      *    -------------------------------
           05  REFM3L PIC S9(0004) COMP.
           05  REFM3F PIC  X(0001).
           05  FILLER REDEFINES REFM3F.
               10  REFM3A PIC  X(0001).
           05  REFM3I PIC  X(0001).
      *    -------------------------------
           05  INIT4L PIC S9(0004) COMP.
           05  INIT4F PIC  X(0001).
           05  FILLER REDEFINES INIT4F.
               10  INIT4A PIC  X(0001).
           05  INIT4I PIC  X(0003).
      *    -------------------------------
           05  BENE4L PIC S9(0004) COMP.
           05  BENE4F PIC  X(0001).
           05  FILLER REDEFINES BENE4F.
               10  BENE4A PIC  X(0001).
           05  BENE4I PIC  X(0003).
      *    -------------------------------
           05  TERM4L PIC S9(0004) COMP.
           05  TERM4F PIC  X(0001).
           05  FILLER REDEFINES TERM4F.
               10  TERM4A PIC  X(0001).
           05  TERM4I PIC  999.
      *    -------------------------------
           05  REFM4L PIC S9(0004) COMP.
           05  REFM4F PIC  X(0001).
           05  FILLER REDEFINES REFM4F.
               10  REFM4A PIC  X(0001).
           05  REFM4I PIC  X(0001).
      *    -------------------------------
           05  INIT5L PIC S9(0004) COMP.
           05  INIT5F PIC  X(0001).
           05  FILLER REDEFINES INIT5F.
               10  INIT5A PIC  X(0001).
           05  INIT5I PIC  X(0003).
      *    -------------------------------
           05  BENE5L PIC S9(0004) COMP.
           05  BENE5F PIC  X(0001).
           05  FILLER REDEFINES BENE5F.
               10  BENE5A PIC  X(0001).
           05  BENE5I PIC  X(0003).
      *    -------------------------------
           05  TERM5L PIC S9(0004) COMP.
           05  TERM5F PIC  X(0001).
           05  FILLER REDEFINES TERM5F.
               10  TERM5A PIC  X(0001).
           05  TERM5I PIC  999.
      *    -------------------------------
           05  REFM5L PIC S9(0004) COMP.
           05  REFM5F PIC  X(0001).
           05  FILLER REDEFINES REFM5F.
               10  REFM5A PIC  X(0001).
           05  REFM5I PIC  X(0001).
      *    -------------------------------
           05  INIT6L PIC S9(0004) COMP.
           05  INIT6F PIC  X(0001).
           05  FILLER REDEFINES INIT6F.
               10  INIT6A PIC  X(0001).
           05  INIT6I PIC  X(0003).
      *    -------------------------------
           05  BENE6L PIC S9(0004) COMP.
           05  BENE6F PIC  X(0001).
           05  FILLER REDEFINES BENE6F.
               10  BENE6A PIC  X(0001).
           05  BENE6I PIC  X(0003).
      *    -------------------------------
           05  TERM6L PIC S9(0004) COMP.
           05  TERM6F PIC  X(0001).
           05  FILLER REDEFINES TERM6F.
               10  TERM6A PIC  X(0001).
           05  TERM6I PIC  999.
      *    -------------------------------
           05  REFM6L PIC S9(0004) COMP.
           05  REFM6F PIC  X(0001).
           05  FILLER REDEFINES REFM6F.
               10  REFM6A PIC  X(0001).
           05  REFM6I PIC  X(0001).
      *    -------------------------------
           05  INIT7L PIC S9(0004) COMP.
           05  INIT7F PIC  X(0001).
           05  FILLER REDEFINES INIT7F.
               10  INIT7A PIC  X(0001).
           05  INIT7I PIC  X(0003).
      *    -------------------------------
           05  BENE7L PIC S9(0004) COMP.
           05  BENE7F PIC  X(0001).
           05  FILLER REDEFINES BENE7F.
               10  BENE7A PIC  X(0001).
           05  BENE7I PIC  X(0003).
      *    -------------------------------
           05  TERM7L PIC S9(0004) COMP.
           05  TERM7F PIC  X(0001).
           05  FILLER REDEFINES TERM7F.
               10  TERM7A PIC  X(0001).
           05  TERM7I PIC  999.
      *    -------------------------------
           05  REFM7L PIC S9(0004) COMP.
           05  REFM7F PIC  X(0001).
           05  FILLER REDEFINES REFM7F.
               10  REFM7A PIC  X(0001).
           05  REFM7I PIC  X(0001).
      *    -------------------------------
           05  INIT8L PIC S9(0004) COMP.
           05  INIT8F PIC  X(0001).
           05  FILLER REDEFINES INIT8F.
               10  INIT8A PIC  X(0001).
           05  INIT8I PIC  X(0003).
      *    -------------------------------
           05  BENE8L PIC S9(0004) COMP.
           05  BENE8F PIC  X(0001).
           05  FILLER REDEFINES BENE8F.
               10  BENE8A PIC  X(0001).
           05  BENE8I PIC  X(0003).
      *    -------------------------------
           05  TERM8L PIC S9(0004) COMP.
           05  TERM8F PIC  X(0001).
           05  FILLER REDEFINES TERM8F.
               10  TERM8A PIC  X(0001).
           05  TERM8I PIC  999.
      *    -------------------------------
           05  REFM8L PIC S9(0004) COMP.
           05  REFM8F PIC  X(0001).
           05  FILLER REDEFINES REFM8F.
               10  REFM8A PIC  X(0001).
           05  REFM8I PIC  X(0001).
      *    -------------------------------
           05  INIT9L PIC S9(0004) COMP.
           05  INIT9F PIC  X(0001).
           05  FILLER REDEFINES INIT9F.
               10  INIT9A PIC  X(0001).
           05  INIT9I PIC  X(0003).
      *    -------------------------------
           05  BENE9L PIC S9(0004) COMP.
           05  BENE9F PIC  X(0001).
           05  FILLER REDEFINES BENE9F.
               10  BENE9A PIC  X(0001).
           05  BENE9I PIC  X(0003).
      *    -------------------------------
           05  TERM9L PIC S9(0004) COMP.
           05  TERM9F PIC  X(0001).
           05  FILLER REDEFINES TERM9F.
               10  TERM9A PIC  X(0001).
           05  TERM9I PIC  999.
      *    -------------------------------
           05  REFM9L PIC S9(0004) COMP.
           05  REFM9F PIC  X(0001).
           05  FILLER REDEFINES REFM9F.
               10  REFM9A PIC  X(0001).
           05  REFM9I PIC  X(0001).
      *    -------------------------------
           05  INIT10L PIC S9(0004) COMP.
           05  INIT10F PIC  X(0001).
           05  FILLER REDEFINES INIT10F.
               10  INIT10A PIC  X(0001).
           05  INIT10I PIC  X(0003).
      *    -------------------------------
           05  BENE10L PIC S9(0004) COMP.
           05  BENE10F PIC  X(0001).
           05  FILLER REDEFINES BENE10F.
               10  BENE10A PIC  X(0001).
           05  BENE10I PIC  X(0003).
      *    -------------------------------
           05  TERM10L PIC S9(0004) COMP.
           05  TERM10F PIC  X(0001).
           05  FILLER REDEFINES TERM10F.
               10  TERM10A PIC  X(0001).
           05  TERM10I PIC  999.
      *    -------------------------------
           05  REFM10L PIC S9(0004) COMP.
           05  REFM10F PIC  X(0001).
           05  FILLER REDEFINES REFM10F.
               10  REFM10A PIC  X(0001).
           05  REFM10I PIC  X(0001).
      *    -------------------------------
           05  INIT11L PIC S9(0004) COMP.
           05  INIT11F PIC  X(0001).
           05  FILLER REDEFINES INIT11F.
               10  INIT11A PIC  X(0001).
           05  INIT11I PIC  X(0003).
      *    -------------------------------
           05  BENE11L PIC S9(0004) COMP.
           05  BENE11F PIC  X(0001).
           05  FILLER REDEFINES BENE11F.
               10  BENE11A PIC  X(0001).
           05  BENE11I PIC  X(0003).
      *    -------------------------------
           05  TERM11L PIC S9(0004) COMP.
           05  TERM11F PIC  X(0001).
           05  FILLER REDEFINES TERM11F.
               10  TERM11A PIC  X(0001).
           05  TERM11I PIC  999.
      *    -------------------------------
           05  REFM11L PIC S9(0004) COMP.
           05  REFM11F PIC  X(0001).
           05  FILLER REDEFINES REFM11F.
               10  REFM11A PIC  X(0001).
           05  REFM11I PIC  X(0001).
      *    -------------------------------
           05  INIT12L PIC S9(0004) COMP.
           05  INIT12F PIC  X(0001).
           05  FILLER REDEFINES INIT12F.
               10  INIT12A PIC  X(0001).
           05  INIT12I PIC  X(0003).
      *    -------------------------------
           05  BENE12L PIC S9(0004) COMP.
           05  BENE12F PIC  X(0001).
           05  FILLER REDEFINES BENE12F.
               10  BENE12A PIC  X(0001).
           05  BENE12I PIC  X(0003).
      *    -------------------------------
           05  TERM12L PIC S9(0004) COMP.
           05  TERM12F PIC  X(0001).
           05  FILLER REDEFINES TERM12F.
               10  TERM12A PIC  X(0001).
           05  TERM12I PIC  999.
      *    -------------------------------
           05  REFM12L PIC S9(0004) COMP.
           05  REFM12F PIC  X(0001).
           05  FILLER REDEFINES REFM12F.
               10  REFM12A PIC  X(0001).
           05  REFM12I PIC  X(0001).
      *    -------------------------------
           05  INIT13L PIC S9(0004) COMP.
           05  INIT13F PIC  X(0001).
           05  FILLER REDEFINES INIT13F.
               10  INIT13A PIC  X(0001).
           05  INIT13I PIC  X(0003).
      *    -------------------------------
           05  BENE13L PIC S9(0004) COMP.
           05  BENE13F PIC  X(0001).
           05  FILLER REDEFINES BENE13F.
               10  BENE13A PIC  X(0001).
           05  BENE13I PIC  X(0003).
      *    -------------------------------
           05  TERM13L PIC S9(0004) COMP.
           05  TERM13F PIC  X(0001).
           05  FILLER REDEFINES TERM13F.
               10  TERM13A PIC  X(0001).
           05  TERM13I PIC  999.
      *    -------------------------------
           05  REFM13L PIC S9(0004) COMP.
           05  REFM13F PIC  X(0001).
           05  FILLER REDEFINES REFM13F.
               10  REFM13A PIC  X(0001).
           05  REFM13I PIC  X(0001).
      *    -------------------------------
           05  INIT14L PIC S9(0004) COMP.
           05  INIT14F PIC  X(0001).
           05  FILLER REDEFINES INIT14F.
               10  INIT14A PIC  X(0001).
           05  INIT14I PIC  X(0003).
      *    -------------------------------
           05  BENE14L PIC S9(0004) COMP.
           05  BENE14F PIC  X(0001).
           05  FILLER REDEFINES BENE14F.
               10  BENE14A PIC  X(0001).
           05  BENE14I PIC  X(0003).
      *    -------------------------------
           05  TERM14L PIC S9(0004) COMP.
           05  TERM14F PIC  X(0001).
           05  FILLER REDEFINES TERM14F.
               10  TERM14A PIC  X(0001).
           05  TERM14I PIC  999.
      *    -------------------------------
           05  REFM14L PIC S9(0004) COMP.
           05  REFM14F PIC  X(0001).
           05  FILLER REDEFINES REFM14F.
               10  REFM14A PIC  X(0001).
           05  REFM14I PIC  X(0001).
      *    -------------------------------
           05  INIT15L PIC S9(0004) COMP.
           05  INIT15F PIC  X(0001).
           05  FILLER REDEFINES INIT15F.
               10  INIT15A PIC  X(0001).
           05  INIT15I PIC  X(0003).
      *    -------------------------------
           05  BENE15L PIC S9(0004) COMP.
           05  BENE15F PIC  X(0001).
           05  FILLER REDEFINES BENE15F.
               10  BENE15A PIC  X(0001).
           05  BENE15I PIC  X(0003).
      *    -------------------------------
           05  TERM15L PIC S9(0004) COMP.
           05  TERM15F PIC  X(0001).
           05  FILLER REDEFINES TERM15F.
               10  TERM15A PIC  X(0001).
           05  TERM15I PIC  999.
      *    -------------------------------
           05  REFM15L PIC S9(0004) COMP.
           05  REFM15F PIC  X(0001).
           05  FILLER REDEFINES REFM15F.
               10  REFM15A PIC  X(0001).
           05  REFM15I PIC  X(0001).
      *    -------------------------------
           05  INIT16L PIC S9(0004) COMP.
           05  INIT16F PIC  X(0001).
           05  FILLER REDEFINES INIT16F.
               10  INIT16A PIC  X(0001).
           05  INIT16I PIC  X(0003).
      *    -------------------------------
           05  BENE16L PIC S9(0004) COMP.
           05  BENE16F PIC  X(0001).
           05  FILLER REDEFINES BENE16F.
               10  BENE16A PIC  X(0001).
           05  BENE16I PIC  X(0003).
      *    -------------------------------
           05  TERM16L PIC S9(0004) COMP.
           05  TERM16F PIC  X(0001).
           05  FILLER REDEFINES TERM16F.
               10  TERM16A PIC  X(0001).
           05  TERM16I PIC  999.
      *    -------------------------------
           05  REFM16L PIC S9(0004) COMP.
           05  REFM16F PIC  X(0001).
           05  FILLER REDEFINES REFM16F.
               10  REFM16A PIC  X(0001).
           05  REFM16I PIC  X(0001).
      *    -------------------------------
           05  INIT17L PIC S9(0004) COMP.
           05  INIT17F PIC  X(0001).
           05  FILLER REDEFINES INIT17F.
               10  INIT17A PIC  X(0001).
           05  INIT17I PIC  X(0003).
      *    -------------------------------
           05  BENE17L PIC S9(0004) COMP.
           05  BENE17F PIC  X(0001).
           05  FILLER REDEFINES BENE17F.
               10  BENE17A PIC  X(0001).
           05  BENE17I PIC  X(0003).
      *    -------------------------------
           05  TERM17L PIC S9(0004) COMP.
           05  TERM17F PIC  X(0001).
           05  FILLER REDEFINES TERM17F.
               10  TERM17A PIC  X(0001).
           05  TERM17I PIC  999.
      *    -------------------------------
           05  REFM17L PIC S9(0004) COMP.
           05  REFM17F PIC  X(0001).
           05  FILLER REDEFINES REFM17F.
               10  REFM17A PIC  X(0001).
           05  REFM17I PIC  X(0001).
      *    -------------------------------
           05  INIT18L PIC S9(0004) COMP.
           05  INIT18F PIC  X(0001).
           05  FILLER REDEFINES INIT18F.
               10  INIT18A PIC  X(0001).
           05  INIT18I PIC  X(0003).
      *    -------------------------------
           05  BENE18L PIC S9(0004) COMP.
           05  BENE18F PIC  X(0001).
           05  FILLER REDEFINES BENE18F.
               10  BENE18A PIC  X(0001).
           05  BENE18I PIC  X(0003).
      *    -------------------------------
           05  TERM18L PIC S9(0004) COMP.
           05  TERM18F PIC  X(0001).
           05  FILLER REDEFINES TERM18F.
               10  TERM18A PIC  X(0001).
           05  TERM18I PIC  999.
      *    -------------------------------
           05  REFM18L PIC S9(0004) COMP.
           05  REFM18F PIC  X(0001).
           05  FILLER REDEFINES REFM18F.
               10  REFM18A PIC  X(0001).
           05  REFM18I PIC  X(0001).
      *    -------------------------------
           05  INIT19L PIC S9(0004) COMP.
           05  INIT19F PIC  X(0001).
           05  FILLER REDEFINES INIT19F.
               10  INIT19A PIC  X(0001).
           05  INIT19I PIC  X(0003).
      *    -------------------------------
           05  BENE19L PIC S9(0004) COMP.
           05  BENE19F PIC  X(0001).
           05  FILLER REDEFINES BENE19F.
               10  BENE19A PIC  X(0001).
           05  BENE19I PIC  X(0003).
      *    -------------------------------
           05  TERM19L PIC S9(0004) COMP.
           05  TERM19F PIC  X(0001).
           05  FILLER REDEFINES TERM19F.
               10  TERM19A PIC  X(0001).
           05  TERM19I PIC  999.
      *    -------------------------------
           05  REFM19L PIC S9(0004) COMP.
           05  REFM19F PIC  X(0001).
           05  FILLER REDEFINES REFM19F.
               10  REFM19A PIC  X(0001).
           05  REFM19I PIC  X(0001).
      *    -------------------------------
           05  INIT20L PIC S9(0004) COMP.
           05  INIT20F PIC  X(0001).
           05  FILLER REDEFINES INIT20F.
               10  INIT20A PIC  X(0001).
           05  INIT20I PIC  X(0003).
      *    -------------------------------
           05  BENE20L PIC S9(0004) COMP.
           05  BENE20F PIC  X(0001).
           05  FILLER REDEFINES BENE20F.
               10  BENE20A PIC  X(0001).
           05  BENE20I PIC  X(0003).
      *    -------------------------------
           05  TERM20L PIC S9(0004) COMP.
           05  TERM20F PIC  X(0001).
           05  FILLER REDEFINES TERM20F.
               10  TERM20A PIC  X(0001).
           05  TERM20I PIC  999.
      *    -------------------------------
           05  REFM20L PIC S9(0004) COMP.
           05  REFM20F PIC  X(0001).
           05  FILLER REDEFINES REFM20F.
               10  REFM20A PIC  X(0001).
           05  REFM20I PIC  X(0001).
      *    -------------------------------
           05  DESCL PIC S9(0004) COMP.
           05  DESCF PIC  X(0001).
           05  FILLER REDEFINES DESCF.
               10  DESCA PIC  X(0001).
           05  DESCI PIC  X(0076).
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
       01  EL158AO REDEFINES EL158AI.
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
           05  FORMO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXPDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INDGRPO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAXATTO PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DEFTYPO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DISMCDO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CRTAPPO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFHDGO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFMINO PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFMAXO PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFTRMO PIC  999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFAMTO PIC  Z(7).99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFPREO PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SUICIDEO PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHHDGO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHMINO PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHMAXO PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHTRMO PIC  999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHAMTO PIC  Z(5).99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHPREO PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INIT1O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENE1O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TERM1O PIC  999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFM1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INIT2O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENE2O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TERM2O PIC  999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFM2O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INIT3O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENE3O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TERM3O PIC  999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFM3O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INIT4O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENE4O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TERM4O PIC  999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFM4O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INIT5O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENE5O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TERM5O PIC  999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFM5O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INIT6O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENE6O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TERM6O PIC  999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFM6O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INIT7O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENE7O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TERM7O PIC  999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFM7O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INIT8O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENE8O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TERM8O PIC  999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFM8O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INIT9O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENE9O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TERM9O PIC  999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFM9O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INIT10O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENE10O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TERM10O PIC  999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFM10O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INIT11O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENE11O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TERM11O PIC  999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFM11O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INIT12O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENE12O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TERM12O PIC  999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFM12O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INIT13O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENE13O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TERM13O PIC  999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFM13O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INIT14O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENE14O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TERM14O PIC  999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFM14O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INIT15O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENE15O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TERM15O PIC  999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFM15O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INIT16O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENE16O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TERM16O PIC  999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFM16O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INIT17O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENE17O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TERM17O PIC  999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFM17O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INIT18O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENE18O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TERM18O PIC  999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFM18O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INIT19O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENE19O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TERM19O PIC  999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFM19O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INIT20O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENE20O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TERM20O PIC  999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFM20O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DESCO PIC  X(0076).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFKEYO PIC  99.
      *    -------------------------------
00293  01  EL158AI-R REDEFINES EL158AI.
00294      12  FILLER                      PIC X(195).
00295      12  EL158-BENE-TABLE OCCURS 20 TIMES.
00296          16  EL158-BENE-INIT-LENGTH  PIC S9(04)  COMP.
00297          16  EL158-BENE-INIT-ATTRB   PIC X(01).
00298          16  EL158-BENE-INIT         PIC X(03).
00299          16  EL158-BENE-LENGTH       PIC S9(04)  COMP.
00300          16  EL158-BENE-ATTRB        PIC X(01).
00301          16  EL158-BENE.
00302              20  EL158-BENE-TYPE     PIC X(01).
00303              20  EL158-BENE-ID       PIC X(02).
00304          16  EL158-BENE-TERM-LENGTH  PIC S9(04)  COMP.
00305          16  EL158-BENE-TERM-ATTRB   PIC X(01).
00306          16  EL158-BENE-TERM         PIC 9(03).
00307          16  EL158-BENE-TERM-R REDEFINES EL158-BENE-TERM
00308                                      PIC X(03).
00309          16  EL158-BENE-REFM-LENGTH  PIC S9(04)  COMP.
00310          16  EL158-BENE-REFM-ATTRB   PIC X(01).
00311          16  EL158-BENE-REFM         PIC X.
00312
00313      EJECT
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
00315
00316  01  DFHCOMMAREA                     PIC X(1024).
00317
00318      EJECT
00319 *                                    COPY ERCFORM.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCFORM.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003                          *
00006 *                                                                *
00007 *    FILE DESCRIPTION = POLICY FORM MASTER FILE                  *
00008 *                                                                *
00009 *    FILE TYPE = VSAM,KSDS                                       *
00010 *    RECORD SIZE = 500  RECFORM = FIXED                          *
00011 *                                                                *
00012 *    BASE CLUSTER = ERFORM                      RKP=02,LEN=20    *
00013 *                                                                *
00014 *    LOG = YES                                                   *
00015 *    SEVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00016 ******************************************************************
00017  01  FORM-MASTER.
00018     12  FO-RECORD-ID                 PIC X(02).
00019         88  VALID-FO-ID                  VALUE 'FO'.
00020
00021     12  FO-CONTROL-PRIMARY.
00022         16  FO-COMPANY-CD            PIC X(01).
00023         16  FO-STATE                 PIC X(02).
00024         16  FO-FORM-ID               PIC X(12).
00025         16  FO-FORM-EXP-DT           PIC X(02).
00026
00027     12  FO-POLICY-FORM-DATA.
00028         16  FO-IND-GRP-CD            PIC X(01).
00029         16  FO-MAX-ATT-AGE           PIC S9(03)      COMP-3.
00030         16  FO-LF-MIN-ISSUE-AGE      PIC S9(03)      COMP-3.
00031         16  FO-AH-MIN-ISSUE-AGE      PIC S9(03)      COMP-3.
00032         16  FO-LF-MAX-ISSUE-AGE      PIC S9(03)      COMP-3.
00033         16  FO-AH-MAX-ISSUE-AGE      PIC S9(03)      COMP-3.
00034         16  FO-LF-MAX-TERM           PIC S9(03)      COMP-3.
00035         16  FO-AH-MAX-TERM           PIC S9(03)      COMP-3.
00036         16  FO-MAX-LF-AMT            PIC S9(07)V99   COMP-3.
00037         16  FO-MAX-AH-AMT            PIC S9(05)V99   COMP-3.
00038         16  FO-SUICIDE-EXCL-TYPE     PIC 9(02).
00039         16  FO-LF-PRE-EXIST-EXCL-TYPE    PIC 9(02).
00040         16  FO-AH-PRE-EXIST-EXCL-TYPE    PIC 9(02).
00041         16  FO-DIS-DEF-TYPE          PIC 9(02).
00042         16  FO-DISMEMBERMENT-CD      PIC X(01).
00043         16  FO-APP-CERT-USE-CD       PIC X(01).
00044
00045     12  FILLER                       PIC X(29).
00046
00047     12  FO-FORM-PLAN-TABLE.
00048         16  FO-FORM-PLANS     OCCURS 40 TIMES.
00049             20  FO-PLAN-TYPE         PIC X(01).
00050             20  FO-PLAN-ID           PIC X(02).
00051             20  FO-PLAN-TERM         PIC 9(03).
00052             20  FO-PLAN-REFUND-METHOD
00053                                      PIC X.
00054
00055     12  FILLER                       PIC X(30).
00056
00057     12  FO-COMMENTS-AREA.
00058         16  FO-COMMENT-LINE-1        PIC X(78).
00059
00060     12  FILLER                       PIC X(20).
00061
00062     12  FO-MAINT-INFORMATION.
00063         16  FO-LAST-MAINT-DT         PIC X(02).
00064         16  FO-LAST-MAINT-HHMMSS     PIC S9(07)      COMP-3.
00065         16  FO-LAST-MAINT-BY         PIC X(04).
00320      EJECT
00321 *                                    COPY ELCCNTL.
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
00322      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA FORM-MASTER
                                CONTROL-FILE.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL158' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00324
00325      MOVE EIBDATE                    TO  DC-JULIAN-YYDDD.
00326      MOVE '5'                        TO  DC-OPTION-CODE.
00327      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
00328      MOVE DC-GREG-DATE-1-EDIT        TO  SAVE-DATE.
00329      MOVE DC-BIN-DATE-1              TO  SAVE-BIN-DATE.
00330
00331      MOVE DFHCOMMAREA                TO  PROGRAM-INTERFACE-BLOCK.
00332      IF EIBCALEN EQUAL 0
00333          GO TO 8800-UNAUTHORIZED-ACCESS.
00334
00335      MOVE PI-LIFE-OVERRIDE-L6        TO  EMI-LIFE-OVERRIDE-L6.
00336      MOVE PI-AH-OVERRIDE-L6          TO  EMI-AH-OVERRIDE-L6.
00337
00338      
      * EXEC CICS HANDLE CONDITION
00339 *        DUPREC     (8850-DUPREC)
00340 *        NOTOPEN    (8870-NOTOPEN)
00341 *        NOTFND     (8880-NOT-FOUND)
00342 *        PGMIDERR   (9600-PGMID-ERROR)
00343 *        ERROR      (9990-ABEND)
00344 *    END-EXEC.
      *    MOVE '"$%JIL.               ! " #00003707' TO DFHEIV0
           MOVE X'2224254A494C2E2020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303033373037' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00345
00346      IF EIBTRNID NOT = EL150-TRANS-ID
00347         GO TO 0150-SET-PROGRAM-SAVES.
00348
00349      MOVE PI-COMPANY-CD              TO ERFORM-COMPANY-CD.
00350      MOVE PI-FORM-NUMBER             TO ERFORM-FORM-ID.
00351      MOVE PI-STATE                   TO ERFORM-STATE.
00352      MOVE PI-CERT-EFF-DT             TO ERFORM-EXP-DT.
00353
00354      
      * EXEC CICS HANDLE CONDITION
00355 *        NOTFND     (0100-FORM-NOTFND)
00356 *    END-EXEC.
      *    MOVE '"$I                   ! # #00003723' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303033373233' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00357
00358      
      * EXEC CICS READ
00359 *        DATASET    (ERFORM-FILE-ID)
00360 *        SET        (ADDRESS OF FORM-MASTER)
00361 *        RIDFLD     (ERFORM-KEY)
00362 *        GTEQ
00363 *    END-EXEC.
      *    MOVE '&"S        G          (   #00003727' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033373237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERFORM-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERFORM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF FORM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00364
00365      IF PI-COMPANY-CD       =  FO-COMPANY-CD
00366         AND FO-STATE        =  FO-STATE
00367         AND FO-FORM-ID      =  ERFORM-FORM-ID
00368         GO TO 0150-SET-PROGRAM-SAVES.
00369
00370  0100-FORM-NOTFND.
00371
00372      MOVE XCTL-155                   TO  PGM-NAME.
00373      GO TO 9300-XCTL.
00374
00375  0150-SET-PROGRAM-SAVES.
00376
00377      IF PI-CALLING-PROGRAM NOT EQUAL THIS-PGM
00378          IF PI-RETURN-TO-PROGRAM NOT EQUAL THIS-PGM
00379              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
00380              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
00381              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
00382              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
00383              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
00384              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
00385              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
00386              MOVE THIS-PGM             TO PI-CALLING-PROGRAM
00387          ELSE
00388              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM
00389              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM
00390              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1
00391              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2
00392              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3
00393              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4
00394              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5
00395              MOVE SPACES               TO PI-SAVED-PROGRAM-6
00396      ELSE
00397          GO TO 0200-RECEIVE.
00398
00399      IF EIBTRNID = EL150-TRANS-ID
00400         MOVE 'I'                     TO MAINTI
00401         MOVE +1                      TO MAINTL
00402         MOVE DFHENTER                TO EIBAID
00403         MOVE PI-STATE                TO STATEI
00404         MOVE +2                      TO STATEL
00405         MOVE PI-FORM-NUMBER          TO FORMI
00406         MOVE +12                     TO FORML
00407         MOVE FO-FORM-EXP-DT          TO DC-BIN-DATE-1
00408         MOVE ' '                     TO DC-OPTION-CODE
00409         MOVE +0                      TO DC-ELAPSED-DAYS
00410                                         DC-ELAPSED-MONTHS
00411         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
00412         IF NO-CONVERSION-ERROR
00413            MOVE DC-GREG-DATE-1-EDIT  TO EXPDTO
00414            GO TO 1000-SHOW-FORM-RECORD
00415         ELSE
00416            MOVE LOW-VALUES           TO EXPDTO
00417            GO TO 1000-SHOW-FORM-RECORD.
00418
00419      MOVE SPACES                     TO  PI-PROGRAM-WORK-AREA.
00420      MOVE LOW-VALUES                 TO  EL158AO.
00421      MOVE -1                         TO  MAINTL.
00422      GO TO 8100-SEND-INITIAL-MAP.
00423
00424      EJECT
00425  0200-RECEIVE.
00426
00427      IF EIBAID IS EQUAL TO DFHCLEAR
00428          GO TO 9400-CLEAR.
00429
00430      IF EIBAID EQUAL DFHPA1 OR DFHPA2 OR DFHPA3
00431          MOVE LOW-VALUES             TO  EL158AI
00432          MOVE ER-7008                TO  EMI-ERROR
00433          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00434          MOVE -1                     TO  MAINTL
00435          GO TO 8200-SEND-DATAONLY.
00436
00437      IF PI-PROCESSOR-ID EQUAL 'LGXX'
00438          NEXT SENTENCE
00439      ELSE
00440          
      * EXEC CICS READQ TS
00441 *            QUEUE  (PI-SECURITY-TEMP-STORE-ID)
00442 *            INTO   (SECURITY-CONTROL)
00443 *            LENGTH (SC-COMM-LENGTH)
00444 *            ITEM   (SC-ITEM)
00445 *        END-EXEC
      *    MOVE '*$II   L              ''   #00003809' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00446          MOVE SC-CLAIMS-DISPLAY (4)  TO  PI-DISPLAY-CAP
00447          MOVE SC-CLAIMS-UPDATE  (4)  TO  PI-MODIFY-CAP.
00448
00449      
      * EXEC CICS RECEIVE
00450 *        MAP      (WS-MAP-NAME)
00451 *        MAPSET   (MAPSET-NAME)
00452 *        INTO     (EL158AI)
00453 *    END-EXEC.
           MOVE LENGTH OF
            EL158AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00003818' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL158AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00454
00455      IF PFKEYL IS EQUAL TO +0
00456          GO TO 0300-CHECK-PFKEYS.
00457
00458      IF (PFKEYI NUMERIC) AND (PFKEYI GREATER 0 AND LESS 25)
00459          MOVE PF-VALUES (PFKEYI)     TO  EIBAID
00460      ELSE
00461          MOVE ER-0029                TO  EMI-ERROR
00462          GO TO 0320-INPUT-ERROR.
00463
00464
00465  0300-CHECK-PFKEYS.
00466
00467      IF EIBAID EQUAL DFHPF23
00468          GO TO 8810-PF23.
00469
00470      IF EIBAID EQUAL DFHPF24
00471          GO TO 9200-RETURN-MAIN-MENU.
00472
00473      IF EIBAID EQUAL DFHPF12
00474          GO TO 9500-PF12.
00475
00476      IF (MAINTL NOT EQUAL 0) AND (EIBAID NOT EQUAL DFHENTER)
00477          MOVE ER-0050            TO EMI-ERROR
00478          GO TO 0320-INPUT-ERROR.
00479
00480      IF EIBAID EQUAL DFHPF1
00481          GO TO 5000-FIND-NEXT-FORM-RECORD.
00482
00483      IF EIBAID EQUAL DFHPF2
00484          GO TO 5100-FIND-PREV-FORM-RECORD.
00485
00486      IF EIBAID EQUAL DFHPF3
00487          GO TO 5200-FIND-NEXT-BENEFIT.
00488
00489      IF EIBAID EQUAL DFHPF4
00490          GO TO 5200-FIND-PREV-BENEFIT.
00491
00492      IF EIBAID EQUAL DFHENTER
00493          GO TO 0330-EDIT-DATA.
00494
00495      MOVE ER-0029                    TO  EMI-ERROR.
00496
00497  0320-INPUT-ERROR.
00498
00499      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00500      MOVE AL-UNBON                   TO  PFKEYA.
00501      MOVE -1                         TO  PFKEYL.
00502      GO TO 8200-SEND-DATAONLY.
00503
00504      EJECT
00505  0330-EDIT-DATA.
00506
00507      IF NOT DISPLAY-CAP
00508          MOVE 'READ'                 TO  SM-READ
00509          PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
00510          MOVE ER-0070                TO  EMI-ERROR
00511          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00512          MOVE -1                     TO  MAINTL
00513          GO TO 8100-SEND-INITIAL-MAP.
00514
00515      IF (STATEL GREATER THAN +0 AND
00516          FORML  GREATER THAN +0 AND
00517          EXPDTL GREATER THAN +0)
00518          NEXT SENTENCE
00519      ELSE
00520          IF (MAINTI    EQUAL   'S'   AND
00521              EXPDTL    EQUAL   +0)
00522              GO TO 1000-SHOW-FORM-RECORD
00523          ELSE
00524              MOVE ER-0754                TO  EMI-ERROR
00525              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00526              MOVE -1                     TO  STATEL
00527              MOVE AL-UABON               TO  STATEA
00528                                          FORMA
00529                                          EXPDTA
00530              GO TO 8200-SEND-DATAONLY.
00531
00532
00533      IF MAINTI EQUAL 'S'
00534          GO TO 1000-SHOW-FORM-RECORD.
00535
00536      IF MAINTI EQUAL 'A' OR 'C' OR 'D'
00537         IF NOT MODIFY-CAP
00538             PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
00539             MOVE ER-0070             TO  EMI-ERROR
00540             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00541             MOVE LOW-VALUES          TO  EL158AO
00542             MOVE -1                  TO  MAINTL
00543             GO TO 8100-SEND-INITIAL-MAP.
00544
00545      IF MAINTI EQUAL 'C'
00546          GO TO 2000-CHANGE-FORM-RECORD.
00547
00548      IF MAINTI EQUAL 'A'
00549          GO TO 3000-ADD-FORM-RECORD.
00550
00551      IF MAINTI EQUAL 'D'
00552          GO TO 4000-DELETE-FORM-RECORD.
00553
00554      MOVE ER-0023                    TO  EMI-ERROR.
00555      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00556      MOVE -1                         TO  MAINTL.
00557      MOVE AL-UABON                   TO  MAINTA.
00558      GO TO 8200-SEND-DATAONLY.
00559
00560      EJECT
00561  1000-SHOW-FORM-RECORD.
00562
00563      MOVE PI-COMPANY-CD              TO  ERFORM-COMPANY-CD.
00564      MOVE STATEI                     TO  ERFORM-STATE.
00565      MOVE FORMI                      TO  ERFORM-FORM-ID.
00566
00567      IF (MAINTI    EQUAL   'S'  AND
00568          EXPDTL    EQUAL   +0)
00569          MOVE  HIGH-VALUES           TO  ERFORM-EXP-DT
00570          GO TO 1000-CONTINUE.
00571
00572      MOVE EXPDTI                     TO  DEEDIT-FIELD.
00573      PERFORM 8600-DEEDIT THRU 8600-EXIT.
00574      IF DEEDIT-FIELD-V0 IS NOT LESS THAN 999999
00575          MOVE HIGH-VALUES            TO  ERFORM-EXP-DT
00576      ELSE
00577          MOVE DEEDIT-FIELD-V0        TO  DC-GREG-DATE-1-MDY
00578          MOVE '4'                    TO  DC-OPTION-CODE
00579          MOVE +0                     TO  DC-ELAPSED-DAYS
00580                                          DC-ELAPSED-MONTHS
00581          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
00582          IF NO-CONVERSION-ERROR
00583              MOVE DC-BIN-DATE-1      TO  ERFORM-EXP-DT
00584          ELSE
00585              MOVE LOW-VALUES         TO  ERFORM-EXP-DT.
00586
00587  1000-CONTINUE.
00588
00589      
      * EXEC CICS HANDLE CONDITION
00590 *        NOTFND     (1000-FORM-NOTFND)
00591 *    END-EXEC.
      *    MOVE '"$I                   ! $ #00003958' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303033393538' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00592
00593      
      * EXEC CICS READ
00594 *        DATASET    (ERFORM-FILE-ID)
00595 *        SET        (ADDRESS OF FORM-MASTER)
00596 *        RIDFLD     (ERFORM-KEY)
00597 *    END-EXEC.
      *    MOVE '&"S        E          (   #00003962' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERFORM-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERFORM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF FORM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00598
00599      GO TO 7000-BUILD-OUTPUT-MAP.
00600
00601  1000-FORM-NOTFND.
00602
00603       MOVE ER-0418                   TO  EMI-ERROR.
00604       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00605       MOVE -1                        TO  MAINTL.
00606       MOVE AL-UABON                  TO  STATEA
00607                                          FORMA
00608                                          EXPDTA.
00609
00610      IF EIBTRNID = EL150-TRANS-ID
00611         GO TO 8100-SEND-INITIAL-MAP
00612      ELSE
00613         GO TO 8200-SEND-DATAONLY.
00614
00615      EJECT
00616  2000-CHANGE-FORM-RECORD.
00617
00618      MOVE EXPDTI                     TO  DEEDIT-FIELD.
00619      PERFORM 8600-DEEDIT THRU 8600-EXIT.
00620      IF DEEDIT-FIELD-V0 IS NOT LESS THAN 999999
00621          MOVE HIGH-VALUES            TO  WS-EXP-DT
00622      ELSE
00623          MOVE DEEDIT-FIELD-V0        TO  DC-GREG-DATE-1-MDY
00624          MOVE '4'                    TO  DC-OPTION-CODE
00625          MOVE +0                     TO  DC-ELAPSED-DAYS
00626                                          DC-ELAPSED-MONTHS
00627          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
00628          MOVE DC-BIN-DATE-1          TO  WS-EXP-DT.
00629
00630      IF MAINTI = 'C'
00631         IF (STATEI IS EQUAL TO PI-PREV-STATE AND
00632            FORMI  IS EQUAL TO PI-PREV-FORM-ID) AND
00633            (WS-EXP-DT NOT EQUAL TO PI-PREV-EXP-DT)
00634             MOVE ER-0717                TO  EMI-ERROR
00635             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00636             MOVE -1                     TO  MAINTL
00637             MOVE AL-UABON               TO  STATEA
00638                                          FORMA
00639                                          EXPDTA
00640             GO TO 8200-SEND-DATAONLY.
00641
00642      IF STATEI IS EQUAL TO PI-PREV-STATE AND
00643         FORMI  IS EQUAL TO PI-PREV-FORM-ID AND
00644         WS-EXP-DT IS EQUAL TO PI-PREV-EXP-DT
00645          NEXT SENTENCE
00646      ELSE
00647          MOVE ER-0138                TO  EMI-ERROR
00648          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00649          MOVE -1                     TO  MAINTL
00650          MOVE AL-UABON               TO  STATEA
00651                                          FORMA
00652                                          EXPDTA
00653          GO TO 8200-SEND-DATAONLY.
00654
00655      PERFORM 6000-EDIT-INPUT-DATA THRU 6000-EXIT.
00656
00657      IF NOT EMI-NO-ERRORS
00658          GO TO 8200-SEND-DATAONLY.
00659
00660      MOVE PI-COMPANY-CD              TO  ERFORM-COMPANY-CD.
00661      MOVE STATEI                     TO  ERFORM-STATE
00662                                          PI-STATE-CODE.
00663      MOVE FORMI                      TO  ERFORM-FORM-ID
00664                                          PI-FORM-ID.
00665      MOVE EXPDTI                     TO  DEEDIT-FIELD.
00666      PERFORM 8600-DEEDIT THRU 8600-EXIT.
00667      IF DEEDIT-FIELD-V0 IS NOT LESS THAN 999999
00668          MOVE HIGH-VALUES            TO  ERFORM-EXP-DT
00669          MOVE '99/99/99'             TO  PI-EXP-DT
00670      ELSE
00671          MOVE DEEDIT-FIELD-V0        TO  DC-GREG-DATE-1-MDY
00672          MOVE '4'                    TO  DC-OPTION-CODE
00673          MOVE +0                     TO  DC-ELAPSED-DAYS
00674                                          DC-ELAPSED-MONTHS
00675          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
00676          IF NO-CONVERSION-ERROR
00677              MOVE DC-BIN-DATE-1      TO  ERFORM-EXP-DT
00678              MOVE DC-GREG-DATE-1-EDIT TO PI-EXP-DT
00679          ELSE
00680              MOVE LOW-VALUES         TO  ERFORM-EXP-DT
00681                                          PI-EXP-DT.
00682
00683      
      * EXEC CICS READ
00684 *        DATASET    (ERFORM-FILE-ID)
00685 *        SET        (ADDRESS OF FORM-MASTER)
00686 *        RIDFLD     (ERFORM-KEY)
00687 *        UPDATE
00688 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00004052' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034303532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERFORM-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERFORM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF FORM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00689
00690      IF FO-LAST-MAINT-BY     NOT EQUAL PI-UPDATE-BY OR
00691         FO-LAST-MAINT-HHMMSS NOT EQUAL PI-UPDATE-HHMMSS
00692          
      * EXEC CICS UNLOCK
00693 *            DATASET   (ERFORM-FILE-ID)
00694 *        END-EXEC
      *    MOVE '&*                    #   #00004061' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034303631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERFORM-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00695          MOVE ER-0068                TO  EMI-ERROR
00696          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00697          GO TO 1000-SHOW-FORM-RECORD.
00698
00699 *    MOVE 'B'                        TO  JP-RECORD-TYPE.
00700 *    MOVE FORM-MASTER                TO  JP-RECORD-AREA.
00701 *    PERFORM 8400-LOG-JOURNAL-RECORD.
00702
00703      MOVE PI-PROCESSOR-ID            TO  FO-LAST-MAINT-BY.
00704      MOVE EIBTIME                    TO  FO-LAST-MAINT-HHMMSS.
00705      MOVE SAVE-BIN-DATE              TO  FO-LAST-MAINT-DT.
00706
00707      IF INDGRPL IS GREATER THAN +0
00708          MOVE INDGRPI                TO  FO-IND-GRP-CD.
00709
00710      IF MAXATTL IS GREATER THAN +0
00711          MOVE MAXATTI                TO  FO-MAX-ATT-AGE.
00712
00713      IF LFMINL IS GREATER THAN +0
00714          MOVE LFMINI                 TO  FO-LF-MIN-ISSUE-AGE.
00715
00716      IF LFMAXL IS GREATER THAN +0
00717          MOVE LFMAXI                 TO  FO-LF-MAX-ISSUE-AGE.
00718
00719      IF LFTRML IS GREATER THAN +0
00720          MOVE LFTRMI                 TO  FO-LF-MAX-TERM.
00721
00722      IF LFAMTL IS GREATER THAN +0
00723          MOVE LFAMTI                 TO  DEEDIT-FIELD-V2
00724          PERFORM 8600-DEEDIT THRU 8600-EXIT
00725          MOVE DEEDIT-FIELD-V2        TO  FO-MAX-LF-AMT.
00726
00727      IF FO-MAX-LF-AMT = +0
00728         MOVE +0                      TO  FO-LF-MIN-ISSUE-AGE
00729                                          FO-LF-MAX-ISSUE-AGE
00730                                          FO-LF-MAX-TERM.
00731
00732      IF LFPREL IS GREATER THAN +0
00733          MOVE LFPREI                 TO FO-LF-PRE-EXIST-EXCL-TYPE.
00734
00735      IF SUICIDEL IS GREATER THAN +0
00736          MOVE SUICIDEI               TO  FO-SUICIDE-EXCL-TYPE.
00737
00738      IF DEFTYPL IS GREATER THAN +0
00739          MOVE DEFTYPI                TO  FO-DIS-DEF-TYPE.
00740
00741      IF DISMCDL IS GREATER THAN +0
00742          MOVE DISMCDI                TO  FO-DISMEMBERMENT-CD.
00743
00744      IF CRTAPPL IS GREATER THAN +0
00745          MOVE CRTAPPI                TO  FO-APP-CERT-USE-CD.
00746
00747      IF AHMINL IS GREATER THAN +0
00748          MOVE AHMINI                 TO  FO-AH-MIN-ISSUE-AGE.
00749
00750      IF AHMAXL IS GREATER THAN +0
00751          MOVE AHMAXI                 TO  FO-AH-MAX-ISSUE-AGE.
00752
00753      IF AHTRML IS GREATER THAN +0
00754          MOVE AHTRMI                 TO  FO-AH-MAX-TERM.
00755
00756      IF AHAMTL IS GREATER THAN +0
00757          MOVE AHAMTI                 TO  DEEDIT-FIELD-V2
00758          PERFORM 8600-DEEDIT THRU 8600-EXIT
00759          MOVE DEEDIT-FIELD-V2        TO  FO-MAX-AH-AMT.
00760
00761      IF FO-MAX-AH-AMT = +0
00762         MOVE +0                      TO  FO-AH-MIN-ISSUE-AGE
00763                                          FO-AH-MAX-ISSUE-AGE
00764                                          FO-AH-MAX-TERM.
00765
00766      IF AHPREL IS GREATER THAN +0
00767          MOVE AHPREI                 TO FO-AH-PRE-EXIST-EXCL-TYPE.
00768
00769      IF DESCL IS GREATER THAN +0
00770          MOVE DESCI                  TO  FO-COMMENT-LINE-1.
00771
00772          MOVE +0                     TO  SUB.
00773      IF EL158-BENE-INIT (1) EQUAL TO WS-INIT-VALUE (21)
00774          MOVE EL158-BENE-INIT (1) TO WS158-BENE-INIT
00775          MOVE +20                    TO SUB2
00776      ELSE
00777          MOVE +0                     TO SUB2.
00778
00779  2000-CHANGE-BENEFIT-LOOP.
00780
00781      ADD +1                          TO  SUB.
00782      ADD +1                          TO  SUB2.
00783      IF SUB IS GREATER THAN +20
00784          GO TO 2000-CONTINUE-CHANGE.
00785
00786      IF EL158-BENE-LENGTH (SUB) IS GREATER THAN +0
00787          MOVE EL158-BENE-TYPE (SUB)  TO  FO-PLAN-TYPE (SUB2)
00788          MOVE EL158-BENE-ID   (SUB)  TO  FO-PLAN-ID   (SUB2).
00789
00790      IF EL158-BENE-TERM-LENGTH (SUB) IS GREATER THAN +0
00791          MOVE EL158-BENE-TERM (SUB)  TO  FO-PLAN-TERM (SUB2).
00792
00793      IF EL158-BENE-REFM-LENGTH (SUB) IS GREATER THAN +0
00794          MOVE EL158-BENE-REFM (SUB)
00795                               TO FO-PLAN-REFUND-METHOD (SUB2).
00796
00797      GO TO 2000-CHANGE-BENEFIT-LOOP.
00798
00799  2000-CONTINUE-CHANGE.
00800 *    MOVE 'C'                        TO  JP-RECORD-TYPE.
00801 *    MOVE FORM-MASTER                TO  JP-RECORD-AREA.
00802
00803      
      * EXEC CICS REWRITE
00804 *        DATASET   (ERFORM-FILE-ID)
00805 *        FROM      (FORM-MASTER)
00806 *    END-EXEC.
           MOVE LENGTH OF
            FORM-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00004172' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313732' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERFORM-FILE-ID, 
                 FORM-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00807
00808 *    PERFORM 8400-LOG-JOURNAL-RECORD.
00809      MOVE ER-0000                    TO  EMI-ERROR.
00810      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00811      MOVE LOW-VALUES                 TO  EL158AO.
00812      MOVE -1                         TO  MAINTL.
00813      MOVE PI-STATE-CODE              TO  STATEI.
00814      MOVE PI-FORM-ID                 TO  FORMI.
00815      MOVE PI-EXP-DT                  TO  EXPDTI.
00816      GO TO 1000-SHOW-FORM-RECORD.
00817
00818      EJECT
00819  3000-ADD-FORM-RECORD.
00820
00821      PERFORM 6000-EDIT-INPUT-DATA THRU 6000-EXIT.
00822
00823      IF NOT EMI-NO-ERRORS
00824          GO TO 8200-SEND-DATAONLY.
00825
00826      
      * EXEC CICS GETMAIN
00827 *        SET       (ADDRESS OF FORM-MASTER)
00828 *        LENGTH    (ERFORM-LENGTH)
00829 *        INITIMG   (GETMAIN-SPACE)
00830 *    END-EXEC.
      *    MOVE ',"IL                  $   #00004195' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313935' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ERFORM-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF FORM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00831
00832      MOVE SPACES                     TO  FORM-MASTER.
00833
00834      MOVE 'FO'                       TO  FO-RECORD-ID.
00835      MOVE PI-COMPANY-CD              TO  FO-COMPANY-CD.
00836      MOVE STATEI                     TO  FO-STATE
00837                                          PI-STATE-CODE.
00838      MOVE FORMI                      TO  FO-FORM-ID
00839                                          PI-FORM-ID.
00840
00841      MOVE EXPDTI                     TO  DEEDIT-FIELD.
00842      PERFORM 8600-DEEDIT THRU 8600-EXIT.
00843      IF DEEDIT-FIELD-V0 IS NOT LESS THAN 999999
00844          MOVE HIGH-VALUES            TO  FO-FORM-EXP-DT
00845          MOVE '99/99/99'             TO  PI-EXP-DT
00846      ELSE
00847          MOVE DEEDIT-FIELD-V0        TO  DC-GREG-DATE-1-MDY
00848          MOVE '4'                    TO  DC-OPTION-CODE
00849          MOVE +0                     TO  DC-ELAPSED-DAYS
00850                                          DC-ELAPSED-MONTHS
00851          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
00852          IF NO-CONVERSION-ERROR
00853              MOVE DC-BIN-DATE-1      TO  FO-FORM-EXP-DT
00854              MOVE DC-GREG-DATE-1-EDIT TO PI-EXP-DT
00855           ELSE
00856              MOVE LOW-VALUES         TO  FO-FORM-EXP-DT
00857                                          PI-EXP-DT.
00858
00859      IF INDGRPL IS GREATER THAN +0
00860          MOVE INDGRPI                TO  FO-IND-GRP-CD.
00861
00862      IF MAXATTL IS GREATER THAN +0
00863          MOVE MAXATTI                TO  FO-MAX-ATT-AGE
00864      ELSE
00865          MOVE +0                     TO  FO-MAX-ATT-AGE.
00866
00867      IF LFMINL IS GREATER THAN +0
00868          MOVE LFMINI                 TO  FO-LF-MIN-ISSUE-AGE
00869      ELSE
00870          MOVE ZEROS                  TO  FO-LF-MIN-ISSUE-AGE.
00871
00872      IF LFMAXL IS GREATER THAN +0
00873          MOVE LFMAXI                 TO  FO-LF-MAX-ISSUE-AGE
00874      ELSE
00875          MOVE +0                     TO  FO-LF-MAX-ISSUE-AGE.
00876
00877      IF LFTRML IS GREATER THAN +0
00878          MOVE LFTRMI                 TO  FO-LF-MAX-TERM
00879      ELSE
00880          MOVE +0                     TO  FO-LF-MAX-TERM.
00881
00882      IF LFAMTL IS GREATER THAN +0
00883          MOVE LFAMTI                 TO  DEEDIT-FIELD-V2
00884          PERFORM 8600-DEEDIT THRU 8600-EXIT
00885          MOVE DEEDIT-FIELD-V2        TO  FO-MAX-LF-AMT
00886      ELSE
00887          MOVE +0                     TO  FO-MAX-LF-AMT.
00888
00889      IF LFPREL IS GREATER THAN +0
00890          MOVE LFPREI                 TO FO-LF-PRE-EXIST-EXCL-TYPE
00891      ELSE
00892          MOVE ZEROS                  TO FO-LF-PRE-EXIST-EXCL-TYPE.
00893
00894      IF SUICIDEL IS GREATER THAN +0
00895          MOVE SUICIDEI               TO  FO-SUICIDE-EXCL-TYPE
00896      ELSE
00897          MOVE +0                     TO  FO-SUICIDE-EXCL-TYPE.
00898
00899      IF DEFTYPL IS GREATER THAN +0
00900          MOVE DEFTYPI                TO  FO-DIS-DEF-TYPE
00901      ELSE
00902          MOVE ZEROS                  TO  FO-DIS-DEF-TYPE.
00903
00904      IF DISMCDL IS GREATER THAN +0
00905          MOVE DISMCDI                TO  FO-DISMEMBERMENT-CD
00906      ELSE
00907          MOVE 'N'                    TO  FO-DISMEMBERMENT-CD.
00908
00909      IF CRTAPPL IS GREATER THAN +0
00910          MOVE CRTAPPI                TO  FO-APP-CERT-USE-CD
00911      ELSE
00912          MOVE 'N'                    TO  FO-APP-CERT-USE-CD.
00913
00914      IF AHMINL IS GREATER THAN +0
00915          MOVE AHMINI                 TO  FO-AH-MIN-ISSUE-AGE
00916      ELSE
00917          MOVE ZEROS                  TO  FO-AH-MIN-ISSUE-AGE.
00918
00919      IF AHMAXL IS GREATER THAN +0
00920          MOVE AHMAXI                 TO  FO-AH-MAX-ISSUE-AGE
00921      ELSE
00922          MOVE +0                     TO  FO-AH-MAX-ISSUE-AGE.
00923
00924      IF AHTRML IS GREATER THAN +0
00925          MOVE AHTRMI                 TO  FO-AH-MAX-TERM
00926      ELSE
00927          MOVE +0                     TO  FO-AH-MAX-TERM.
00928
00929      IF AHPREL IS GREATER THAN +0
00930          MOVE AHPREI                 TO FO-AH-PRE-EXIST-EXCL-TYPE
00931      ELSE
00932          MOVE ZEROS                  TO FO-AH-PRE-EXIST-EXCL-TYPE.
00933
00934      IF AHAMTL IS GREATER THAN +0
00935          MOVE AHAMTI                 TO  DEEDIT-FIELD-V2
00936          PERFORM 8600-DEEDIT THRU 8600-EXIT
00937          MOVE DEEDIT-FIELD-V2        TO  FO-MAX-AH-AMT
00938      ELSE
00939          MOVE +0                     TO  FO-MAX-AH-AMT.
00940
00941      IF DESCL IS GREATER THAN +0
00942          MOVE DESCI                  TO  FO-COMMENT-LINE-1.
00943
00944      MOVE +0                         TO  SUB.
00945
00946  3000-ADD-BENEFIT-LOOP.
00947
00948      ADD +1                          TO  SUB.
00949      IF SUB IS GREATER THAN +20
00950          PERFORM 3010-ADD-BENEFIT-INIT THRU 3010-ABI-EXIT 20 TIMES
00951          GO TO 3000-ADD-CONTINUE.
00952
00953      IF EL158-BENE-LENGTH (SUB) IS GREATER THAN +0
00954          MOVE EL158-BENE-TYPE (SUB)  TO  FO-PLAN-TYPE  (SUB)
00955          MOVE EL158-BENE-ID   (SUB)  TO  FO-PLAN-ID    (SUB)
00956      ELSE
00957          MOVE SPACES                 TO  FO-PLAN-TYPE  (SUB)
00958          MOVE SPACES                 TO  FO-PLAN-ID    (SUB).
00959
00960      IF EL158-BENE-TERM-LENGTH (SUB) IS GREATER THAN +0
00961          MOVE EL158-BENE-TERM (SUB)  TO  DEEDIT-FIELD-V0
00962          PERFORM 8600-DEEDIT THRU 8600-EXIT
00963          MOVE DEEDIT-FIELD-V0        TO  FO-PLAN-TERM (SUB)
00964      ELSE
00965          MOVE ZEROES                 TO  FO-PLAN-TERM  (SUB).
00966
00967      IF EL158-BENE-REFM-LENGTH (SUB) IS GREATER THAN +0
00968          MOVE EL158-BENE-REFM (SUB)
00969                               TO  FO-PLAN-REFUND-METHOD (SUB)
00970      ELSE
00971          MOVE SPACES          TO  FO-PLAN-REFUND-METHOD (SUB).
00972
00973
00974      GO TO 3000-ADD-BENEFIT-LOOP.
00975
00976  3000-ADD-CONTINUE.
00977
00978 *    IF DATE-CONVERSION-ERROR
00979 *        MOVE LOW-VALUES             TO  FO-LAST-MAINT-DT
00980 *    ELSE
00981 *        MOVE DC-BIN-DATE-1          TO  FO-LAST-MAINT-DT.
00982
00983      MOVE SAVE-BIN-DATE              TO  FO-LAST-MAINT-DT.
00984      MOVE PI-PROCESSOR-ID            TO  FO-LAST-MAINT-BY.
00985      MOVE EIBTIME                    TO  FO-LAST-MAINT-HHMMSS.
00986
00987 *    MOVE 'A'                        TO  JP-RECORD-TYPE.
00988 *    MOVE FORM-MASTER                TO  JP-RECORD-AREA.
00989
00990  3005-WRITE-ERFORM-FILE.
00991
00992      
      * EXEC CICS WRITE
00993 *        DATASET    (ERFORM-FILE-ID)
00994 *        FROM       (FORM-MASTER)
00995 *        RIDFLD     (FO-CONTROL-PRIMARY)
00996 *    END-EXEC.
           MOVE LENGTH OF
            FORM-MASTER
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00004361' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERFORM-FILE-ID, 
                 FORM-MASTER, 
                 DFHEIV11, 
                 FO-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00997
00998 *    PERFORM 8400-LOG-JOURNAL-RECORD.
00999      MOVE ER-0000                    TO  EMI-ERROR.
01000      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01001      MOVE LOW-VALUES                 TO  EL158AO.
01002      MOVE -1                         TO  MAINTL.
01003      MOVE PI-STATE-CODE              TO  STATEI.
01004      MOVE PI-FORM-ID                 TO  FORMI.
01005      MOVE PI-EXP-DT                  TO  EXPDTI.
01006      GO TO 1000-SHOW-FORM-RECORD.
01007
01008  3010-ADD-BENEFIT-INIT.
01009      MOVE ZEROES                 TO  FO-PLAN-TERM  (SUB).
01010      ADD +1                      TO  SUB.
01011  3010-ABI-EXIT.
01012      EXIT.
01013      EJECT
01014  4000-DELETE-FORM-RECORD.
01015
01016      MOVE PI-COMPANY-CD              TO  ERFORM-COMPANY-CD.
01017      MOVE STATEI                     TO  ERFORM-STATE.
01018      MOVE FORMI                      TO  ERFORM-FORM-ID.
01019
01020      MOVE EXPDTI                     TO  DEEDIT-FIELD.
01021      PERFORM 8600-DEEDIT THRU 8600-EXIT.
01022      IF DEEDIT-FIELD-V0 IS NOT LESS THAN 999999
01023          MOVE HIGH-VALUES            TO  ERFORM-EXP-DT
01024      ELSE
01025          MOVE DEEDIT-FIELD-V0        TO  DC-GREG-DATE-1-MDY
01026          MOVE '4'                    TO  DC-OPTION-CODE
01027          MOVE +0                     TO  DC-ELAPSED-DAYS
01028                                          DC-ELAPSED-MONTHS
01029          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
01030          IF NO-CONVERSION-ERROR
01031              MOVE DC-BIN-DATE-1      TO  ERFORM-EXP-DT
01032          ELSE
01033              MOVE LOW-VALUES         TO  ERFORM-EXP-DT.
01034
01035      
      * EXEC CICS READ
01036 *        DATASET   (ERFORM-FILE-ID)
01037 *        SET       (ADDRESS OF FORM-MASTER)
01038 *        RIDFLD    (ERFORM-KEY)
01039 *        UPDATE
01040 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00004404' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERFORM-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERFORM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF FORM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01041
01042      IF FO-LAST-MAINT-BY     NOT EQUAL PI-UPDATE-BY OR
01043         FO-LAST-MAINT-HHMMSS NOT EQUAL PI-UPDATE-HHMMSS
01044          
      * EXEC CICS UNLOCK
01045 *            DATASET   (ERFORM-FILE-ID)
01046 *            END-EXEC
      *    MOVE '&*                    #   #00004413' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERFORM-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01047          MOVE ER-0068                TO  EMI-ERROR
01048          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01049          GO TO 1000-SHOW-FORM-RECORD.
01050
01051 *    MOVE 'D'                        TO  JP-RECORD-TYPE.
01052 *    MOVE FORM-MASTER                TO  JP-RECORD-AREA.
01053      
      * EXEC CICS DELETE
01054 *        DATASET   (ERFORM-FILE-ID)
01055 *        END-EXEC.
      *    MOVE '&(                    &   #00004422' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERFORM-FILE-ID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01056
01057 *    PERFORM 8400-LOG-JOURNAL-RECORD
01058      MOVE ER-0000                    TO  EMI-ERROR.
01059      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01060      MOVE LOW-VALUES                 TO  EL158AO.
01061      MOVE -1                         TO  MAINTL.
01062      GO TO 8100-SEND-INITIAL-MAP.
01063
01064      EJECT
01065  5000-FIND-NEXT-FORM-RECORD.
01066
01067      MOVE LOW-VALUES                 TO  ERFORM-KEY.
01068      MOVE PI-COMPANY-CD              TO  ERFORM-COMPANY-CD.
01069
01070      IF STATEL IS GREATER THAN +0
01071          MOVE STATEI                 TO  ERFORM-STATE.
01072      IF FORML IS GREATER THAN +0
01073          MOVE FORMI                  TO  ERFORM-FORM-ID.
01074      IF EXPDTL IS GREATER THAN +0
01075          MOVE EXPDTI                 TO  DEEDIT-FIELD
01076          PERFORM 8600-DEEDIT THRU 8600-EXIT
01077          IF DEEDIT-FIELD-V0 IS NOT LESS THAN 999999
01078              MOVE HIGH-VALUES        TO  ERFORM-EXP-DT
01079          ELSE
01080              MOVE DEEDIT-FIELD-V0    TO  DC-GREG-DATE-1-MDY
01081              MOVE '4'                TO  DC-OPTION-CODE
01082              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
01083              IF NO-CONVERSION-ERROR
01084                  MOVE DC-BIN-DATE-1  TO  ERFORM-EXP-DT
01085              ELSE
01086                  MOVE LOW-VALUES     TO  ERFORM-EXP-DT.
01087
01088      
      * EXEC CICS HANDLE CONDITION
01089 *        ENDFILE (5000-UNSUCCESSFUL-SEARCH)
01090 *    END-EXEC.
      *    MOVE '"$''                   ! % #00004457' TO DFHEIV0
           MOVE X'222427202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303034343537' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01091
01092      
      * EXEC CICS STARTBR
01093 *        DATASET   (ERFORM-FILE-ID)
01094 *        RIDFLD    (ERFORM-KEY)
01095 *        GTEQ
01096 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00004461' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERFORM-FILE-ID, 
                 ERFORM-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01097
01098  5000-READNEXT-LOOP.
01099      
      * EXEC CICS READNEXT
01100 *        DATASET   (ERFORM-FILE-ID)
01101 *        SET       (ADDRESS OF FORM-MASTER)
01102 *        RIDFLD    (ERFORM-KEY)
01103 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00004468' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERFORM-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERFORM-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF FORM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01104
01105      IF FO-COMPANY-CD  NOT EQUAL PI-COMPANY-CD
01106          GO TO 5000-UNSUCCESSFUL-SEARCH.
01107
01108      IF ERFORM-KEY IS EQUAL TO PI-PREV-FORM-KEY
01109          GO TO 5000-READNEXT-LOOP.
01110
01111      GO TO 7000-BUILD-OUTPUT-MAP.
01112
01113  5000-END-BROWSE.
01114
01115      
      * EXEC CICS ENDBR
01116 *        DATASET   (ERFORM-FILE-ID)
01117 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00004484' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343834' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERFORM-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01118
01119  5000-UNSUCCESSFUL-SEARCH.
01120
01121      PERFORM 5000-END-BROWSE.
01122      MOVE -1                         TO  PFKEYL.
01123      MOVE ER-0130                    TO  EMI-ERROR.
01124      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01125      GO TO 1000-SHOW-FORM-RECORD.
01126
01127      EJECT
01128  5100-FIND-PREV-FORM-RECORD.
01129
01130      MOVE LOW-VALUES                 TO  ERFORM-KEY.
01131      MOVE PI-COMPANY-CD              TO  ERFORM-COMPANY-CD.
01132      MOVE PI-PREV-FORM-KEY           TO  ERFORM-KEY.
01133
01134      IF STATEL IS GREATER THAN +0
01135          MOVE STATEI                 TO  ERFORM-STATE.
01136      IF FORML IS GREATER THAN +0
01137          MOVE FORMI                  TO  ERFORM-FORM-ID.
01138      IF EXPDTL IS GREATER THAN +0
01139          MOVE EXPDTI                 TO  DEEDIT-FIELD
01140          PERFORM 8600-DEEDIT THRU 8600-EXIT
01141          IF DEEDIT-FIELD-V0 IS NOT LESS THAN 999999
01142              MOVE HIGH-VALUES        TO  ERFORM-EXP-DT
01143          ELSE
01144              MOVE DEEDIT-FIELD-V0    TO  DC-GREG-DATE-1-MDY
01145              MOVE '4'                TO  DC-OPTION-CODE
01146              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
01147              IF NO-CONVERSION-ERROR
01148                  MOVE DC-BIN-DATE-1  TO  ERFORM-EXP-DT
01149              ELSE
01150                  MOVE LOW-VALUES     TO  ERFORM-EXP-DT.
01151
01152      
      * EXEC CICS HANDLE CONDITION
01153 *        ENDFILE (5100-UNSUCCESSFUL-SEARCH)
01154 *    END-EXEC.
      *    MOVE '"$''                   ! & #00004521' TO DFHEIV0
           MOVE X'222427202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303034353231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01155
01156      
      * EXEC CICS STARTBR
01157 *        DATASET   (ERFORM-FILE-ID)
01158 *        RIDFLD    (ERFORM-KEY)
01159 *        GTEQ
01160 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00004525' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034353235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERFORM-FILE-ID, 
                 ERFORM-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01161
01162  5100-READPREV-LOOP.
01163      
      * EXEC CICS READPREV
01164 *        DATASET   (ERFORM-FILE-ID)
01165 *        SET       (ADDRESS OF FORM-MASTER)
01166 *        RIDFLD    (ERFORM-KEY)
01167 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00004532' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303034353332' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERFORM-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERFORM-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF FORM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01168
01169      IF FO-COMPANY-CD  NOT EQUAL PI-COMPANY-CD
01170          GO TO 5100-UNSUCCESSFUL-SEARCH.
01171
01172      IF ERFORM-KEY IS EQUAL TO PI-PREV-FORM-KEY
01173          GO TO 5100-READPREV-LOOP.
01174
01175      GO TO 7000-BUILD-OUTPUT-MAP.
01176
01177  5100-END-BROWSE.
01178      
      * EXEC CICS ENDBR
01179 *        DATASET   (ERFORM-FILE-ID)
01180 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00004547' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034353437' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERFORM-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01181
01182  5100-UNSUCCESSFUL-SEARCH.
01183
01184      PERFORM 5100-END-BROWSE.
01185      MOVE -1                         TO  PFKEYL.
01186      MOVE ER-0131                    TO  EMI-ERROR.
01187      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01188      GO TO 1000-SHOW-FORM-RECORD.
01189
01190      EJECT
01191  5200-FIND-NEXT-BENEFIT.
01192
01193      IF EL158-BENE-INIT (1)  EQUAL WS-INIT-VALUE (1)
01194          MOVE WS-INIT-VALUE  (21) TO WS158-BENE-INIT
01195      ELSE
01196          MOVE WS-INIT-VALUE  (21) TO WS158-BENE-INIT
01197          MOVE ER-8150             TO EMI-ERROR
01198          MOVE -1                  TO  PFKEYL
01199          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01200
01201      GO TO 1000-SHOW-FORM-RECORD.
01202
01203
01204  5200-FIND-PREV-BENEFIT.
01205
01206      IF EL158-BENE-INIT (1)  EQUAL WS-INIT-VALUE (21)
01207          MOVE WS-INIT-VALUE  (1)  TO WS158-BENE-INIT
01208      ELSE
01209          MOVE WS-INIT-VALUE  (1)  TO WS158-BENE-INIT
01210          MOVE ER-8150             TO EMI-ERROR
01211          MOVE -1                  TO  PFKEYL
01212          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01213
01214      GO TO 1000-SHOW-FORM-RECORD.
01215
01216      EJECT
01217  6000-EDIT-INPUT-DATA.
01218
01219      IF MAINTI IS EQUAL TO 'A'
01220          IF STATEL IS EQUAL TO +0
01221              MOVE ER-0144            TO  EMI-ERROR
01222              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01223              MOVE -1                 TO  STATEL
01224              MOVE AL-UABON           TO  STATEA
01225          ELSE
01226              MOVE SPACES             TO  ELCNTL-KEY
01227              MOVE PI-COMPANY-ID      TO  ELCNTL-COMPANY-ID
01228              MOVE '3'                TO  ELCNTL-RECORD-TYPE
01229              MOVE STATEI             TO  ELCNTL-STATE-CD
01230              MOVE +0                 TO  ELCNTL-SEQUENCE-NO
01231              PERFORM 8000-READ-CNTL THRU 8010-EXIT
01232              IF CNTL-RECORD-FOUND
01233                  MOVE AL-UANON       TO  STATEA
01234              ELSE
01235                  MOVE ER-0144        TO  EMI-ERROR
01236                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01237                  MOVE -1             TO  STATEL
01238                  MOVE AL-UABON       TO  STATEA.
01239
01240      IF MAINTI IS EQUAL TO 'A'
01241          IF FORML IS EQUAL TO +0
01242              MOVE ER-0703            TO  EMI-ERROR
01243              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01244              MOVE -1                 TO  FORML
01245              MOVE AL-UABON           TO  FORMA
01246          ELSE
01247              MOVE AL-UANON           TO  FORMA.
01248
01249      IF MAINTI IS EQUAL TO 'A'
01250          IF EXPDTL IS EQUAL TO +0
01251              MOVE ER-0704            TO  EMI-ERROR
01252              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01253              MOVE -1                 TO  EXPDTL
01254              MOVE AL-UABON           TO  EXPDTA
01255          ELSE
01256              MOVE EXPDTI             TO  DEEDIT-FIELD
01257              PERFORM 8600-DEEDIT THRU 8600-EXIT
01258              IF DEEDIT-FIELD-V0 IS NOT LESS THAN 999999
01259                  MOVE '99/99/99'     TO  EXPDTO
01260                  MOVE AL-UANON       TO  EXPDTA
01261              ELSE
01262                  MOVE DEEDIT-FIELD-V0 TO DC-GREG-DATE-1-MDY
01263                  MOVE '4'            TO  DC-OPTION-CODE
01264                  MOVE +0             TO  DC-ELAPSED-DAYS
01265                                          DC-ELAPSED-MONTHS
01266                  PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
01267                  IF NO-CONVERSION-ERROR
01268                      MOVE DC-GREG-DATE-1-EDIT TO  EXPDTO
01269                      MOVE AL-UANON   TO  EXPDTA
01270                  ELSE
01271                      MOVE ER-0705    TO  EMI-ERROR
01272                      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
01273                      MOVE -1         TO  EXPDTL
01274                      MOVE AL-UABON   TO  EXPDTA.
01275
01276      IF MAINTI = 'C'
01277         IF INDGRPL IS GREATER THAN +0
01278             IF INDGRPI IS EQUAL TO 'I' OR 'G'
01279                 MOVE AL-UANON        TO  INDGRPA
01280             ELSE
01281                 MOVE ER-7031         TO  EMI-ERROR
01282                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01283                 MOVE -1              TO  INDGRPL
01284                 MOVE AL-UABON        TO  INDGRPA.
01285
01286      IF MAINTI = 'A'
01287         IF INDGRPL IS GREATER THAN +0
01288             IF INDGRPI IS EQUAL TO 'I' OR 'G'
01289                 MOVE AL-UANON        TO  INDGRPA
01290             ELSE
01291                 MOVE ER-7031         TO  EMI-ERROR
01292                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01293                 MOVE -1              TO  INDGRPL
01294                 MOVE AL-UABON        TO  INDGRPA
01295         ELSE
01296             MOVE ER-7031         TO  EMI-ERROR
01297             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01298             MOVE -1              TO  INDGRPL
01299             MOVE AL-UABON        TO  INDGRPA.
01300
01301      IF MAXATTL IS GREATER THAN +0
01302          MOVE MAXATTI                TO  DEEDIT-FIELD
01303          PERFORM 8600-DEEDIT THRU 8600-EXIT
01304          MOVE DEEDIT-FIELD-V0        TO  MAXATTI
01305          MOVE AL-UNNON               TO  MAXATTA.
01306
01307      IF DEFTYPL IS GREATER THAN +0
01308 *       MOVE DEFTYPI                TO  DEEDIT-FIELD
01309 *       PERFORM 8600-DEEDIT THRU 8600-EXIT
01310 *       MOVE DEEDIT-FIELD-V0        TO  WS-DISABILITY-CODES
01311         MOVE DEFTYPI                TO  WS-DISABILITY-CODES
01312         IF VALID-DISABILITY-CODE
01313 *          MOVE DEEDIT-FIELD-V0     TO  WS-DEFTYP
01314            MOVE AL-UNNON            TO  DEFTYPA
01315         ELSE
01316            MOVE ER-0706             TO  EMI-ERROR
01317            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01318            MOVE -1                  TO  DEFTYPL
01319            MOVE AL-UNBON            TO  DEFTYPA.
01320
01321      IF DISMCDL IS GREATER THAN +0
01322         IF DISMCDI IS EQUAL TO 'Y' OR 'N'
01323            MOVE AL-UANON            TO  DISMCDA
01324          ELSE
01325            MOVE ER-0707             TO  EMI-ERROR
01326            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01327            MOVE -1                  TO  DISMCDL
01328            MOVE AL-UABON            TO  DISMCDA.
01329
01330      IF CRTAPPL IS GREATER THAN +0
01331        IF CRTAPPI IS EQUAL TO 'Y' OR 'N'
01332            MOVE AL-UANON           TO  CRTAPPA
01333        ELSE
01334            MOVE ER-0708            TO  EMI-ERROR
01335            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01336            MOVE -1                 TO  CRTAPPL
01337            MOVE AL-UABON           TO  CRTAPPA.
01338
01339      IF LFMINL IS GREATER THAN +0
01340          MOVE LFMINI                 TO  DEEDIT-FIELD
01341          PERFORM 8600-DEEDIT THRU 8600-EXIT
01342          MOVE DEEDIT-FIELD-V0        TO  LFMINI
01343                                          WS-LFMIN
01344          MOVE AL-UNNON               TO  LFMINA.
01345
01346      IF MAINTI = 'A'
01347         IF MAXATTL NOT GREATER THAN +0
01348            MOVE ER-0718              TO  EMI-ERROR
01349            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01350            MOVE -1                   TO  MAXATTL
01351            MOVE AL-UABON             TO  MAXATTA.
01352
01353      IF LFMAXL IS GREATER THAN +0
01354          MOVE LFMAXI                 TO  DEEDIT-FIELD
01355          PERFORM 8600-DEEDIT THRU 8600-EXIT
01356          MOVE DEEDIT-FIELD-V0        TO  LFMAXI
01357                                          WS-LFMAX
01358          MOVE AL-UNNON               TO  LFMAXA.
01359
01360      IF LFTRML IS GREATER THAN +0
01361          MOVE LFTRMI                 TO  DEEDIT-FIELD
01362          PERFORM 8600-DEEDIT THRU 8600-EXIT
01363          MOVE DEEDIT-FIELD-V0        TO  LFTRMI
01364                                          WS-LFTRM
01365          MOVE AL-UNNON               TO  LFTRMA.
01366
01367      IF LFAMTL IS GREATER THAN +0
01368          MOVE LFAMTI                 TO  DEEDIT-FIELD-V2
01369          PERFORM 8600-DEEDIT THRU 8600-EXIT
01370          IF DEEDIT-FIELD-V0 IS GREATER THAN +999999999
01371              MOVE ER-0709            TO  EMI-ERROR
01372              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01373              MOVE -1                 TO  LFAMTL
01374              MOVE AL-UNBON           TO  LFAMTA
01375          ELSE
01376              MOVE DEEDIT-FIELD-V2    TO  LFAMTI
01377                                          WS-LFAMT
01378              MOVE AL-UNNON           TO  LFAMTA.
01379
01380      IF LFMINL = +0 AND
01381         LFMAXL = +0 AND
01382         LFTRML = +0 AND
01383         LFAMTL = +0 AND
01384         AHMINL = +0 AND
01385         AHMAXL = +0 AND
01386         AHTRML = +0 AND
01387         AHAMTL = +0
01388         MOVE ER-0729                 TO  EMI-ERROR
01389         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01390         MOVE -1                      TO  LFMINL.
01391
01392      IF LFMINL = +0 AND
01393         LFMAXL = +0 AND
01394         LFTRML = +0 AND
01395         LFAMTL = +0
01396         GO TO 6000-EDIT-LF-PREEXS.
01397
01398      IF MAINTI = 'A'
01399         IF LFMINL NOT GREATER THAN +0
01400            MOVE ER-0719              TO  EMI-ERROR
01401            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01402            MOVE -1                   TO  LFMINL
01403            MOVE AL-UABON             TO  LFMINA.
01404
01405      IF MAINTI = 'A'
01406         IF LFMAXL NOT GREATER THAN +0
01407            MOVE ER-0720              TO  EMI-ERROR
01408            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01409            MOVE -1                   TO  LFMAXL
01410            MOVE AL-UABON             TO  LFMAXA.
01411
01412      IF MAINTI = 'A'
01413         IF LFTRML NOT GREATER THAN +0
01414            MOVE ER-0721              TO  EMI-ERROR
01415            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01416            MOVE -1                   TO  LFTRML
01417            MOVE AL-UABON             TO  LFTRMA.
01418
01419      IF MAINTI = 'A'
01420         IF LFAMTL NOT GREATER THAN +0
01421            MOVE ER-0722              TO  EMI-ERROR
01422            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01423            MOVE -1                   TO  LFAMTL
01424            MOVE AL-UABON             TO  LFAMTA.
01425
01426  6000-EDIT-LF-PREEXS.
01427
01428      IF LFPREL IS GREATER THAN +0
01429 *       MOVE LFPREI                 TO  DEEDIT-FIELD
01430 *       PERFORM 8600-DEEDIT THRU 8600-EXIT
01431 *       MOVE DEEDIT-FIELD-V0       TO  WS-PRE-EXIST-CODES
01432         MOVE LFPREI                TO  WS-PRE-EXIST-CODES
01433         IF VALID-PRE-EXIST-CODE
01434 *          MOVE DEEDIT-FIELD-V0    TO  WS-LFPRE
01435            MOVE AL-UNNON           TO  LFPREA
01436         ELSE
01437            MOVE ER-0710            TO  EMI-ERROR
01438            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01439            MOVE -1                 TO  LFPREL
01440            MOVE AL-UNBON           TO  LFPREA.
01441
01442      IF SUICIDEL IS GREATER THAN +0
01443 *        MOVE SUICIDEI               TO  DEEDIT-FIELD
01444 *        PERFORM 8600-DEEDIT THRU 8600-EXIT
01445 *        MOVE DEEDIT-FIELD-V0        TO  WS-SUICIDE-CODES
01446          MOVE SUICIDEI               TO  WS-SUICIDE-CODES
01447          IF VALID-SUICIDE-CODE
01448              MOVE AL-UANON           TO  SUICIDEA
01449 *            MOVE DEEDIT-FIELD-V0    TO  WS-SUICIDE
01450          ELSE
01451              MOVE ER-0711            TO  EMI-ERROR
01452              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01453              MOVE -1                 TO  SUICIDEL
01454              MOVE AL-UABON           TO  SUICIDEA.
01455
01456
01457      IF AHMINL IS GREATER THAN +0
01458          MOVE AHMINI                 TO  DEEDIT-FIELD
01459          PERFORM 8600-DEEDIT THRU 8600-EXIT
01460          MOVE DEEDIT-FIELD-V0        TO  AHMINI
01461                                          WS-AHMIN
01462          MOVE AL-UNNON               TO  AHMINA.
01463
01464
01465      IF AHMAXL IS GREATER THAN +0
01466          MOVE AHMAXI                 TO  DEEDIT-FIELD
01467          PERFORM 8600-DEEDIT THRU 8600-EXIT
01468          MOVE DEEDIT-FIELD-V0        TO  AHMAXI
01469                                          WS-AHMAX
01470          MOVE AL-UNNON               TO  AHMAXA.
01471
01472      IF AHTRML IS GREATER THAN +0
01473          MOVE AHTRMI                 TO  DEEDIT-FIELD
01474          PERFORM 8600-DEEDIT THRU 8600-EXIT
01475          MOVE DEEDIT-FIELD-V0        TO  AHTRMI
01476                                          WS-AHTRM
01477          MOVE AL-UNNON               TO  AHTRMA.
01478
01479      IF AHAMTL IS GREATER THAN +0
01480          MOVE AHAMTI                 TO  DEEDIT-FIELD-V2
01481          PERFORM 8600-DEEDIT THRU 8600-EXIT
01482          IF DEEDIT-FIELD-V0 IS GREATER THAN +9999999
01483              MOVE ER-0712            TO  EMI-ERROR
01484              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01485              MOVE -1                 TO  AHAMTL
01486              MOVE AL-UNBON           TO  AHAMTA
01487          ELSE
01488              MOVE DEEDIT-FIELD-V2    TO  AHAMTO
01489                                          WS-AHAMT
01490              MOVE AL-UNNON           TO  AHAMTA.
01491
01492      IF AHMINL = +0 AND
01493         AHMAXL = +0 AND
01494         AHTRML = +0 AND
01495         AHAMTL = +0
01496         GO TO 6000-EDIT-AH-PREEXS.
01497
01498      IF MAINTI = 'A'
01499         IF AHMINL NOT GREATER THAN +0
01500            MOVE ER-0723              TO  EMI-ERROR
01501            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01502            MOVE -1                   TO  AHMINL
01503            MOVE AL-UABON             TO  AHMINA.
01504
01505      IF MAINTI = 'A'
01506         IF AHMAXL NOT GREATER THAN +0
01507            MOVE ER-0724              TO  EMI-ERROR
01508            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01509            MOVE -1                   TO  AHMAXL
01510            MOVE AL-UABON             TO  AHMAXA.
01511
01512      IF MAINTI = 'A'
01513         IF AHTRML NOT GREATER THAN +0
01514            MOVE ER-0725              TO  EMI-ERROR
01515            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01516            MOVE -1                   TO  AHTRML
01517            MOVE AL-UABON             TO  AHTRMA.
01518
01519      IF MAINTI = 'A'
01520         IF AHAMTL NOT GREATER THAN +0
01521            MOVE ER-0726              TO  EMI-ERROR
01522            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01523            MOVE -1                   TO  AHAMTL
01524            MOVE AL-UABON             TO  AHAMTA.
01525
01526  6000-EDIT-AH-PREEXS.
01527
01528      IF AHPREL IS GREATER THAN +0
01529 *        MOVE AHPREI                 TO  DEEDIT-FIELD
01530 *        PERFORM 8600-DEEDIT THRU 8600-EXIT
01531 *        MOVE DEEDIT-FIELD-V0        TO  WS-PRE-EXIST-CODES
01532          MOVE AHPREI                 TO  WS-PRE-EXIST-CODES
01533          IF VALID-PRE-EXIST-CODE
01534              MOVE AL-UNNON           TO  AHPREA
01535 *            MOVE DEEDIT-FIELD-V0    TO  WS-AHPRE
01536          ELSE
01537              MOVE ER-0710            TO  EMI-ERROR
01538              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01539              MOVE -1                 TO  AHPREL
01540              MOVE AL-UNBON           TO  AHPREA.
01541      MOVE +0                         TO  SUB.
01542
01543      MOVE 'N'                        TO WS-BENEFIT-SW.
01544
01545      IF EL158-BENE-INIT (1) EQUAL TO '21.'
01546          MOVE 'Y'                    TO  WS-BENEFIT-SW.
01547
01548  6000-EDIT-BENEFIT-LOOP.
01549
01550
01551      ADD +1                          TO  SUB.
01552      IF SUB IS GREATER THAN +20
01553          GO TO 6000-EDIT-FINISHED.
01554
01555      IF (EL158-BENE-LENGTH (SUB) IS GREATER THAN +0
01556          OR EL158-BENE-TERM-LENGTH (SUB) IS GREATER THAN +0
01557          OR EL158-BENE-REFM-LENGTH (SUB) IS GREATER THAN +0)
01558          MOVE EL158-BENE (SUB)       TO  WS-BENEFIT-FIELD
01559          MOVE 'Y'                    TO  WS-BENEFIT-SW
01560      ELSE
01561          GO TO 6000-EDIT-BENEFIT-LOOP.
01562
01563      IF EL158-BENE (SUB) IS EQUAL TO SPACES
01564                      AND
01565        (EL158-BENE-TERM (SUB) IS GREATER THAN +0
01566         OR EL158-BENE-REFM (SUB) IS GREATER THAN +0)
01567          MOVE ER-0713                TO  EMI-ERROR
01568          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01569          MOVE -1               TO  EL158-BENE-LENGTH (SUB)
01570          MOVE AL-UABON         TO  EL158-BENE-ATTRB  (SUB)
01571          GO TO 6000-EDIT-BENEFIT-LOOP.
01572
01573      IF MAINTI IS EQUAL TO 'C'
01574          IF EL158-BENE (SUB) IS EQUAL TO SPACES
01575              GO TO 6000-EDIT-BENEFIT-LOOP.
01576
01577      IF WS-BENE-TYPE IS EQUAL TO PI-LIFE-OVERRIDE-L1 OR
01578         WS-BENE-TYPE IS EQUAL TO PI-AH-OVERRIDE-L1
01579          NEXT SENTENCE
01580      ELSE
01581          MOVE ER-0713                TO  EMI-ERROR
01582          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01583          MOVE -1                     TO  EL158-BENE-LENGTH (SUB)
01584          MOVE AL-UABON               TO  EL158-BENE-ATTRB  (SUB)
01585          GO TO 6000-EDIT-BENEFIT-LOOP.
01586
01587      IF WS-BENE-TYPE IS EQUAL TO PI-LIFE-OVERRIDE-L1
01588          MOVE SPACES                 TO  ELCNTL-KEY
01589          MOVE '4'                    TO  ELCNTL-RECORD-TYPE
01590          MOVE WS-BENE-CODE           TO  ELCNTL-BENE-CD
01591          MOVE PI-COMPANY-ID          TO  ELCNTL-COMPANY-ID
01592          MOVE ZEROS                  TO  ELCNTL-SEQUENCE-NO
01593          PERFORM 7100-READ-BENEFIT THRU 7199-EXIT
01594          IF CNTL-RECORD-FOUND
01595              MOVE AL-UANON           TO  EL158-BENE-ATTRB (SUB)
01596          ELSE
01597              MOVE ER-7123            TO  EMI-ERROR
01598              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01599              MOVE -1                 TO  EL158-BENE-LENGTH (SUB)
01600              MOVE AL-UABON           TO  EL158-BENE-ATTRB  (SUB).
01601
01602      IF WS-BENE-TYPE IS EQUAL TO PI-AH-OVERRIDE-L1
01603          MOVE SPACES                 TO  ELCNTL-KEY
01604          MOVE '5'                    TO  ELCNTL-RECORD-TYPE
01605          MOVE WS-BENE-CODE           TO  ELCNTL-BENE-CD
01606          MOVE PI-COMPANY-ID          TO  ELCNTL-COMPANY-ID
01607          MOVE ZEROS                  TO  ELCNTL-SEQUENCE-NO
01608          PERFORM 7100-READ-BENEFIT THRU 7199-EXIT
01609          IF CNTL-RECORD-FOUND
01610              MOVE AL-UANON           TO  EL158-BENE-ATTRB (SUB)
01611          ELSE
01612              MOVE ER-7123            TO  EMI-ERROR
01613              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01614              MOVE -1                 TO  EL158-BENE-LENGTH (SUB)
01615              MOVE AL-UABON           TO  EL158-BENE-ATTRB  (SUB).
01616
01617      IF EL158-BENE-TERM-LENGTH (SUB) IS GREATER THAN +0
01618          MOVE EL158-BENE-TERM (SUB)  TO  DEEDIT-FIELD
01619          PERFORM 8600-DEEDIT THRU 8600-EXIT
01620          MOVE DEEDIT-FIELD-V0        TO  EL158-BENE-TERM (SUB)
01621          IF (EL158-BENE-TERM (SUB) > 360)  OR
01622             (EL158-BENE-TERM (SUB) < 001)
01623              MOVE ER-2241            TO  EMI-ERROR
01624              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01625              MOVE -1            TO  EL158-BENE-TERM-LENGTH (SUB)
01626              MOVE AL-UNBON      TO  EL158-BENE-TERM-ATTRB  (SUB)
01627          ELSE
01628              MOVE AL-UNNON      TO  EL158-BENE-TERM-ATTRB (SUB).
01629
01630      IF EL158-BENE (SUB) NOT EQUAL TO (SPACES AND LOW-VALUES)
01631                      AND
01632         EL158-BENE-TERM (SUB) IS NOT NUMERIC
01633          MOVE ER-2276          TO  EMI-ERROR
01634          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01635          MOVE -1            TO  EL158-BENE-TERM-LENGTH (SUB)
01636          MOVE AL-UNBON      TO  EL158-BENE-TERM-ATTRB  (SUB).
01637
01638      IF EL158-BENE-REFM-LENGTH (SUB) IS GREATER THAN +0
01639          MOVE EL158-BENE-REFM (SUB) TO WS-REFUND-METHOD
01640          IF VALID-REFUND-METHOD
01641              MOVE AL-UANON      TO  EL158-BENE-REFM-ATTRB (SUB)
01642          ELSE
01643              MOVE ER-0582            TO  EMI-ERROR
01644              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01645              MOVE -1            TO  EL158-BENE-REFM-LENGTH (SUB)
01646              MOVE AL-UABON      TO  EL158-BENE-REFM-ATTRB  (SUB).
01647
01648      GO TO 6000-EDIT-BENEFIT-LOOP.
01649
01650  6000-EDIT-FINISHED.
01651
01652      IF LFAMTL GREATER THAN +0 AND
01653         WS-LFAMT =  +0         AND
01654         AHAMTL GREATER THAN +0 AND
01655         WS-AHAMT = +0
01656         MOVE ER-0729                TO  EMI-ERROR
01657         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01658         MOVE -1                     TO  LFAMTL
01659         MOVE AL-UNBON               TO  LFAMTA.
01660
01661      IF (WS-LFMIN GREATER THAN +0 OR
01662          WS-LFMAX GREATER THAN +0 OR
01663          WS-LFTRM GREATER THAN +0 OR
01664          WS-LFAMT GREATER THAN +0)
01665          AND
01666          WS-LFMIN = +0
01667          MOVE ER-0729               TO  EMI-ERROR
01668          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01669          MOVE -1                    TO  LFMINL
01670          MOVE AL-UNBON              TO  LFMINA.
01671
01672      IF (WS-LFMIN GREATER THAN +0 OR
01673          WS-LFMAX GREATER THAN +0 OR
01674          WS-LFTRM GREATER THAN +0 OR
01675          WS-LFAMT GREATER THAN +0)
01676          AND
01677          WS-LFMAX = +0
01678          MOVE ER-0729               TO  EMI-ERROR
01679          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01680          MOVE -1                    TO  LFMAXL
01681          MOVE AL-UNBON              TO  LFMAXA.
01682
01683      IF (WS-LFMIN GREATER THAN +0 OR
01684          WS-LFMAX GREATER THAN +0 OR
01685          WS-LFTRM GREATER THAN +0 OR
01686          WS-LFAMT GREATER THAN +0)
01687          AND
01688          WS-LFTRM = +0
01689          MOVE ER-0729               TO  EMI-ERROR
01690          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01691          MOVE -1                    TO  LFTRML
01692          MOVE AL-UNBON              TO  LFTRMA.
01693
01694      IF (WS-AHMIN GREATER THAN +0 OR
01695          WS-AHMAX GREATER THAN +0 OR
01696          WS-AHTRM GREATER THAN +0 OR
01697          WS-AHAMT GREATER THAN +0)
01698          AND
01699          WS-AHMIN = +0
01700          MOVE ER-0729               TO  EMI-ERROR
01701          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01702          MOVE -1                    TO  AHMINL
01703          MOVE AL-UNBON              TO  AHMINA.
01704
01705      IF (WS-AHMIN GREATER THAN +0 OR
01706          WS-AHMAX GREATER THAN +0 OR
01707          WS-AHTRM GREATER THAN +0 OR
01708          WS-AHAMT GREATER THAN +0)
01709          AND
01710          WS-AHMAX = +0
01711          MOVE ER-0729               TO  EMI-ERROR
01712          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01713          MOVE -1                    TO  AHMAXL
01714          MOVE AL-UNBON              TO  AHMAXA.
01715
01716      IF (WS-AHMIN GREATER THAN +0 OR
01717          WS-AHMAX GREATER THAN +0 OR
01718          WS-AHTRM GREATER THAN +0 OR
01719          WS-AHAMT GREATER THAN +0)
01720          AND
01721          WS-AHTRM = +0
01722          MOVE ER-0729               TO  EMI-ERROR
01723          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01724          MOVE -1                    TO  AHTRML
01725          MOVE AL-UNBON              TO  AHTRMA.
01726
01727      IF BENEFIT-NOT-FOUND
01728         MOVE ER-0727                 TO  EMI-ERROR
01729         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01730         MOVE -1                      TO  EL158-BENE-LENGTH (1)
01731         MOVE AL-UABON                TO  EL158-BENE-ATTRB  (1).
01732
01733  6000-EXIT.
01734      EXIT.
01735
01736      EJECT
01737  7000-BUILD-OUTPUT-MAP.
01738
01739      MOVE LOW-VALUES                 TO  EL158AO.
01740      MOVE PI-COMPANY-CD              TO  PI-PREV-COMPANY-CD.
01741      MOVE FO-STATE                   TO  STATEO
01742                                          PI-PREV-STATE.
01743      MOVE FO-FORM-ID                 TO  FORMO
01744                                          PI-PREV-FORM-ID.
01745      IF FO-FORM-EXP-DT IS EQUAL TO HIGH-VALUES
01746          MOVE '99/99/99'             TO  EXPDTO
01747          MOVE HIGH-VALUES            TO  PI-PREV-EXP-DT
01748      ELSE
01749          MOVE FO-FORM-EXP-DT         TO  DC-BIN-DATE-1
01750                                          PI-PREV-EXP-DT
01751          MOVE ' '                    TO  DC-OPTION-CODE
01752          MOVE +0                     TO  DC-ELAPSED-DAYS
01753                                          DC-ELAPSED-MONTHS
01754          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
01755          IF NO-CONVERSION-ERROR
01756              MOVE DC-GREG-DATE-1-EDIT TO EXPDTO
01757          ELSE
01758              MOVE LOW-VALUES         TO  EXPDTO.
01759
01760      MOVE FO-IND-GRP-CD              TO  INDGRPO.
01761      MOVE AL-UANON                   TO  INDGRPA.
01762      MOVE FO-MAX-ATT-AGE             TO  MAXATTO.
01763      MOVE AL-UNNON                   TO  MAXATTA.
01764
01765      MOVE FO-LF-MIN-ISSUE-AGE        TO  LFMINO.
01766      MOVE AL-UNNON                   TO  LFMINA.
01767      MOVE FO-LF-MAX-ISSUE-AGE        TO  LFMAXO.
01768      MOVE AL-UNNON                   TO  LFMAXA.
01769      MOVE FO-LF-MAX-TERM             TO  LFTRMO.
01770      MOVE AL-UNNON                   TO  LFTRMA.
01771      MOVE FO-MAX-LF-AMT              TO  LFAMTO.
01772      MOVE AL-UNNON                   TO  LFAMTA.
01773      MOVE FO-LF-PRE-EXIST-EXCL-TYPE  TO  LFPREO.
01774      MOVE AL-UNNON                   TO  LFPREA.
01775      MOVE FO-SUICIDE-EXCL-TYPE       TO  SUICIDEO.
01776      MOVE AL-UNNON                   TO  SUICIDEA.
01777      MOVE FO-DIS-DEF-TYPE            TO  DEFTYPO.
01778      MOVE AL-UANON                   TO  DEFTYPA.
01779      MOVE FO-DISMEMBERMENT-CD        TO  DISMCDO.
01780      MOVE AL-UANON                   TO  DISMCDA.
01781      MOVE FO-APP-CERT-USE-CD         TO  CRTAPPO.
01782      MOVE AL-UANON                   TO  CRTAPPA.
01783
01784      MOVE FO-AH-MIN-ISSUE-AGE        TO  AHMINO.
01785      MOVE AL-UNNON                   TO  AHMINA.
01786      MOVE FO-AH-MAX-ISSUE-AGE        TO  AHMAXO.
01787      MOVE AL-UNNON                   TO  AHMAXA.
01788      MOVE FO-AH-MAX-TERM             TO  AHTRMO.
01789      MOVE AL-UNNON                   TO  AHTRMA.
01790      MOVE FO-MAX-AH-AMT              TO  AHAMTO.
01791      MOVE AL-UNNON                   TO  AHAMTA.
01792      MOVE FO-AH-PRE-EXIST-EXCL-TYPE  TO  AHPREO.
01793      MOVE AL-UNNON                   TO  AHPREA.
01794
01795      MOVE FO-LAST-MAINT-BY           TO  MAINTBYO
01796                                          PI-UPDATE-BY.
01797      MOVE FO-LAST-MAINT-HHMMSS       TO  TIME-IN
01798                                          PI-UPDATE-HHMMSS.
01799      MOVE TIME-OUT                   TO  MAINTATO.
01800      MOVE FO-LAST-MAINT-DT           TO  DC-BIN-DATE-1.
01801      MOVE ' '                        TO  DC-OPTION-CODE.
01802      MOVE +0                         TO  DC-ELAPSED-DAYS
01803                                          DC-ELAPSED-MONTHS.
01804      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
01805      IF NO-CONVERSION-ERROR
01806          MOVE DC-GREG-DATE-1-EDIT    TO  MAINTONO
01807      ELSE
01808          MOVE LOW-VALUES             TO  MAINTONO.
01809
01810      MOVE +0                         TO  SUB.
01811
01812      IF WS158-BENE-INIT     EQUAL WS-INIT-VALUE (21)
01813          MOVE +20 TO SUB2
01814      ELSE
01815          MOVE +0  TO SUB2.
01816
01817  7000-BUILD-BENEFIT-LOOP.
01818
01819      ADD +1                          TO  SUB.
01820      ADD +1                          TO  SUB2.
01821      IF SUB IS GREATER THAN +20
01822          GO TO 7000-FINISH-BUILD.
01823
01824      MOVE WS-INIT-VALUE (SUB2) TO EL158-BENE-INIT( SUB).
01825      MOVE AL-SANON             TO EL158-BENE-INIT-ATTRB (SUB).
01826      MOVE FO-PLAN-TYPE  (SUB2)        TO  EL158-BENE-TYPE  (SUB).
01827      MOVE FO-PLAN-ID    (SUB2)        TO  EL158-BENE-ID    (SUB).
01828      IF FO-PLAN-TYPE (SUB2) GREATER THAN SPACES
01829          MOVE FO-PLAN-TERM  (SUB2)    TO  EL158-BENE-TERM  (SUB)
01830      ELSE
01831          MOVE SPACES                  TO  EL158-BENE-TERM-R(SUB).
01832      MOVE FO-PLAN-REFUND-METHOD(SUB2) TO  EL158-BENE-REFM  (SUB).
01833
01834      IF FO-PLAN-TYPE   (SUB2) GREATER THAN SPACES
01835         MOVE AL-UANON         TO  EL158-BENE-ATTRB      (SUB)
01836         MOVE AL-UNNON         TO  EL158-BENE-TERM-ATTRB (SUB)
01837         MOVE AL-UANON         TO  EL158-BENE-REFM-ATTRB (SUB).
01838
01839      GO TO 7000-BUILD-BENEFIT-LOOP.
01840
01841  7000-FINISH-BUILD.
01842
01843      MOVE FO-COMMENT-LINE-1          TO  DESCO.
01844
01845      MOVE -1                         TO  MAINTL.
01846      MOVE AL-UANON                   TO  STATEA
01847                                          FORMA
01848                                          EXPDTA.
01849      GO TO 8100-SEND-INITIAL-MAP.
01850
01851      EJECT
01852  7100-READ-BENEFIT.
01853
01854      
      * EXEC CICS HANDLE CONDITION
01855 *        NOTFND   (7120-NOT-FOUND)
01856 *    END-EXEC.
      *    MOVE '"$I                   ! '' #00005223' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303035323233' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01857
01858      
      * EXEC CICS READ
01859 *        DATASET   (ELCNTL-FILE-ID)
01860 *        RIDFLD    (ELCNTL-KEY)
01861 *        SET       (ADDRESS OF CONTROL-FILE)
01862 *        GTEQ
01863 *    END-EXEC.
      *    MOVE '&"S        G          (   #00005227' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01864
01865      IF PI-COMPANY-ID IS NOT EQUAL TO CF-COMPANY-ID OR
01866         ELCNTL-RECORD-TYPE IS NOT EQUAL TO CF-RECORD-TYPE
01867          GO TO 7120-NOT-FOUND.
01868
01869      MOVE +1                         TO  SUB-1.
01870
01871  7110-LOOP.
01872
01873      IF SUB-1 IS EQUAL TO +9
01874          GO TO 7120-NOT-FOUND.
01875
01876      IF WS-BENE-CODE IS NOT EQUAL TO CF-BENEFIT-CODE (SUB-1)
01877          ADD +1                      TO  SUB-1
01878          GO TO 7110-LOOP.
01879
01880      MOVE 'Y'                        TO  WS-CNTL-REC-FOUND-SW.
01881      GO TO 7199-EXIT.
01882
01883  7120-NOT-FOUND.
01884      MOVE 'N'                        TO  WS-CNTL-REC-FOUND-SW.
01885
01886  7199-EXIT.
01887      EXIT.
01888      EJECT
01889  8000-READ-CNTL.
01890
01891      
      * EXEC CICS HANDLE CONDITION
01892 *        NOTFND   (8009-NOTFND)
01893 *    END-EXEC.
      *    MOVE '"$I                   ! ( #00005260' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303035323630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01894
01895      
      * EXEC CICS READ
01896 *        DATASET   (ELCNTL-FILE-ID)
01897 *        RIDFLD    (ELCNTL-KEY)
01898 *        SET       (ADDRESS OF CONTROL-FILE)
01899 *    END-EXEC.
      *    MOVE '&"S        E          (   #00005264' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01900
01901      MOVE 'Y'                         TO  WS-CNTL-REC-FOUND-SW.
01902      GO TO 8010-EXIT.
01903
01904  8009-NOTFND.
01905      MOVE 'N'                         TO  WS-CNTL-REC-FOUND-SW.
01906
01907  8010-EXIT.
01908      EXIT.
01909      EJECT
01910  8100-SEND-INITIAL-MAP.
01911
01912      MOVE PI-LIFE-OVERRIDE-L2        TO  LFHDGO.
01913      MOVE PI-AH-OVERRIDE-L2          TO  AHHDGO.
01914      MOVE EMI-MESSAGE-AREA (1)       TO  ERRMSG1O.
01915      MOVE EIBTIME                    TO  TIME-IN.
01916      MOVE SAVE-DATE                  TO  DATEO.
01917      MOVE TIME-OUT                   TO  TIMEO.
01918
01919      
      * EXEC CICS SEND
01920 *        MAP      (WS-MAP-NAME)
01921 *        MAPSET   (MAPSET-NAME)
01922 *        FROM     (EL158AO)
01923 *        ERASE
01924 *        CURSOR
01925 *    END-EXEC.
           MOVE LENGTH OF
            EL158AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00005288' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323838' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL158AO, 
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
           
01926
01927      GO TO 9100-RETURN-TRAN.
01928
01929      EJECT
01930  8200-SEND-DATAONLY.
01931
01932      MOVE PI-LIFE-OVERRIDE-L2        TO  LFHDGO.
01933      MOVE PI-AH-OVERRIDE-L2          TO  AHHDGO.
01934      MOVE EMI-MESSAGE-AREA (1)       TO  ERRMSG1O.
01935      MOVE EIBTIME                    TO  TIME-IN.
01936      MOVE SAVE-DATE                  TO  DATEO.
01937      MOVE TIME-OUT                   TO  TIMEO.
01938
01939      
      * EXEC CICS SEND
01940 *        MAP      (WS-MAP-NAME)
01941 *        MAPSET   (MAPSET-NAME)
01942 *        FROM     (EL158AO)
01943 *        DATAONLY
01944 *        CURSOR
01945 *    END-EXEC.
           MOVE LENGTH OF
            EL158AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00005308' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL158AO, 
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
           
01946
01947      GO TO 9100-RETURN-TRAN.
01948      EJECT
01949  8300-SEND-TEXT.
01950
01951      
      * EXEC CICS SEND TEXT
01952 *        FROM  (LOGOFF-TEXT)
01953 *        LENGTH(LOGOFF-LENGTH)
01954 *        ERASE
01955 *        FREEKB
01956 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00005320' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333230' TO DFHEIV0(25:11)
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
           
01957
01958      
      * EXEC CICS RETURN
01959 *        END-EXEC.
      *    MOVE '.(                    &   #00005327' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01960
01961      EJECT
01962 *8400-LOG-JOURNAL-RECORD.
01963 *    MOVE PI-PROCESSOR-ID            TO  JP-USER-ID.
01964 *    MOVE ELBENE-FILE-ID             TO  JP-FILE-ID.
01965 *    MOVE THIS-PGM                   TO  JP-PROGRAM-ID.
01966 *    EXEC CICS JOURNAL
01967 *         JFILEID   (PI-JOURNAL-FILE-ID)
01968 *         JTYPEID   ('EL')
01969 *         FROM      (JOURNAL-RECORD)
01970 *         LENGTH    (527)
01971 *         END-EXEC.
01972
01973  8600-DEEDIT.
01974
01975      
      * EXEC CICS BIF DEEDIT
01976 *        FIELD    (DEEDIT-FIELD)
01977 *        LENGTH   (15)
01978 *    END-EXEC.
           MOVE 15
             TO DFHEIV11
      *    MOVE '@"L                   #   #00005344' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01979
01980  8600-EXIT.
01981      EXIT.
01982
01983  8800-UNAUTHORIZED-ACCESS.
01984      MOVE UNACCESS-MSG               TO  LOGOFF-MSG.
01985      GO TO 8300-SEND-TEXT.
01986
01987  8810-PF23.
01988      MOVE EIBAID                     TO  PI-ENTRY-CD-1.
01989      MOVE XCTL-005                   TO  PGM-NAME.
01990      GO TO 9300-XCTL.
01991
01992  8850-DUPREC.
01993      MOVE ER-0132                    TO  EMI-ERROR.
01994      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01995      MOVE -1                         TO  STATEL.
01996      MOVE AL-UABON                   TO  STATEA  FORMA  EXPDTA.
01997      GO TO 8100-SEND-INITIAL-MAP.
01998
01999  8870-NOTOPEN.
02000
02001      MOVE LOW-VALUES                 TO  EL158AO.
02002      MOVE -1                         TO  MAINTL.
02003      MOVE ER-0701                    TO  EMI-ERROR.
02004      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02005      GO TO 8100-SEND-INITIAL-MAP.
02006
02007  8880-NOT-FOUND.
02008      MOVE ER-0702                    TO  EMI-ERROR.
02009      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02010      MOVE -1                         TO  STATEL.
02011      MOVE AL-UABON                   TO  STATEA  FORMA EXPDTA.
02012      GO TO 8100-SEND-INITIAL-MAP.
02013
02014      EJECT
02015  9100-RETURN-TRAN.
02016      MOVE EMI-ERROR-NUMBER (1)       TO  PI-LAST-ERROR-NO.
02017      MOVE '158A'                     TO  PI-CURRENT-SCREEN-NO.
02018
02019      
      * EXEC CICS RETURN
02020 *        TRANSID    (TRANS-ID)
02021 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
02022 *        LENGTH     (PI-COMM-LENGTH)
02023 *    END-EXEC.
      *    MOVE '.(CT                  &   #00005388' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333838' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02024
02025  9200-RETURN-MAIN-MENU.
02026
02027      MOVE XCTL-626                   TO  PGM-NAME.
02028      GO TO 9300-XCTL.
02029
02030  9300-XCTL.
02031      
      * EXEC CICS XCTL
02032 *        PROGRAM    (PGM-NAME)
02033 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
02034 *        LENGTH     (PI-COMM-LENGTH)
02035 *    END-EXEC.
      *    MOVE '.$C                   $   #00005400' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02036
02037  9400-CLEAR.
02038      MOVE PI-RETURN-TO-PROGRAM       TO  PGM-NAME.
02039      GO TO 9300-XCTL.
02040
02041  9500-PF12.
02042      MOVE XCTL-010                   TO  PGM-NAME.
02043      GO TO 9300-XCTL.
02044
02045  9600-PGMID-ERROR.
02046      
      * EXEC CICS HANDLE CONDITION
02047 *        PGMIDERR   (8300-SEND-TEXT)
02048 *    END-EXEC.
      *    MOVE '"$L                   ! ) #00005415' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303035343135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02049
02050      MOVE PGM-NAME                   TO  PI-CALLING-PROGRAM.
02051      MOVE ' '                        TO  PI-ENTRY-CD-1.
02052      MOVE XCTL-005                   TO  PGM-NAME.
02053      MOVE PGM-NAME                   TO  LOGOFF-PGM.
02054      MOVE PGMIDERR-MSG               TO  LOGOFF-FILL.
02055      GO TO 9300-XCTL.
02056
02057      EJECT
02058  9700-LINK-DATE-CONVERT.
02059      
      * EXEC CICS LINK
02060 *        PROGRAM    ('ELDATCV')
02061 *        COMMAREA   (DATE-CONVERSION-DATA)
02062 *        LENGTH     (DC-COMM-LENGTH)
02063 *    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   ''   #00005428' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343238' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02064
02065  9700-EXIT.
02066      EXIT.
02067
02068  9900-ERROR-FORMAT.
02069      IF NOT EMI-ERRORS-COMPLETE
02070          MOVE LINK-001               TO  PGM-NAME
02071          
      * EXEC CICS LINK
02072 *            PROGRAM    (PGM-NAME)
02073 *            COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
02074 *            LENGTH     (EMI-COMM-LENGTH)
02075 *        END-EXEC.
      *    MOVE '."C                   ''   #00005440' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02076
02077  9900-EXIT.
02078      EXIT.
02079
02080  9990-ABEND.
02081      MOVE LINK-004                   TO  PGM-NAME.
02082      MOVE DFHEIBLK                   TO  EMI-LINE1.
02083      
      * EXEC CICS LINK
02084 *        PROGRAM   (PGM-NAME)
02085 *        COMMAREA  (EMI-LINE1)
02086 *        LENGTH    (72)
02087 *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   ''   #00005452' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02088
02089      GO TO 8100-SEND-INITIAL-MAP.
02090
02091      EJECT
02092  9995-SECURITY-VIOLATION.
02093 *                            COPY ELCSCTP.
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
      *    MOVE '."C                   ''   #00005479' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343739' TO DFHEIV0(25:11)
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
02094
02095  9995-EXIT.
02096      EXIT.
02097

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL158' TO DFHEIV1
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
               GO TO 0100-FORM-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 1000-FORM-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 5000-UNSUCCESSFUL-SEARCH
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 5100-UNSUCCESSFUL-SEARCH
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 7120-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 8009-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL158' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
