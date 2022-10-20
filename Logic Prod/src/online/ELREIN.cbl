00001  ID DIVISION.
00002
00003  PROGRAM-ID.   ELREIN.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 03/05/96 16:21:39.
00007 *                            VMOD=2.008
00008 *
00009 *AUTHOR.     LOGIC, INC.
00010 *            DALLAS, TEXAS.
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
00024 *REMARKS.
00025
00026 *        THIS SUBROUTINE IS USED TO TEST REINSURANCE TABLES.
00027
00028      EJECT
00029  ENVIRONMENT DIVISION.
00030  DATA DIVISION.
00031
00032  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00033  77  FILLER   PIC X(32) VALUE '********************************'.
00034  77  FILLER   PIC X(32) VALUE '**  ELREIN  WORKING STORAGE   **'.
00035  77  FILLER   PIC X(32) VALUE '*********** VMOD 2.008 *********'.
00036
00037  01  ACCESS-KEYS.
00038      12  ELCNTL-KEY.
00039          16  ENTL-COMP-ID            PIC X(3).
00040          16  ENTL-REC-TYPE           PIC X.
00041          16  ENTL-ACCESS             PIC X(4).
00042          16  ENTL-SEQ-NO             PIC S9(4)    COMP.
00043
00044      12  ERREIN-KEY.
00045          16  REIN-COMP-CD            PIC X.
00046          16  REIN-TYPE               PIC X.
00047          16  REIN-TABLE-CO.
00048              20  REIN-CODE-1         PIC X.
00049              20  REIN-CODE-2         PIC X.
00050              20  REIN-CODE-3         PIC X.
00051          16  REIN-TABLE-SUB          PIC XXX.
00052
00053      12  WS-ACCESS.
00054          16  FILLER                  PIC XX      VALUE SPACES.
00055          16  WS-BEN-CD               PIC XX.
00056
00057      12  WS-SWITCHES.
00058          16  BEN-SEARCH-SW           PIC X       VALUE SPACES.
00059              88  NO-BENEFIT-FOUND                VALUE 'N'.
00060
00061          16  AH-BENEFIT-ERROR        PIC X       VALUE SPACE.
00062              88  AH-BENEFIT-FOUND                VALUE ' '.
00063              88  NO-AH-BENEFIT-MATCH             VALUE '1'.
00064
00065          16  LF-BENEFIT-ERROR        PIC X       VALUE SPACE.
00066              88  LF-BENEFIT-FOUND                VALUE ' '.
00067              88  NO-LF-BENEFIT-MATCH             VALUE '1'.
00068
00069  01  MISC.
00070      12  SUB                         PIC 9999    VALUE ZEROS COMP.
00071      12  SUB3                        PIC 9999    VALUE ZEROS COMP.
00072
00073      12  ABEND-CODE.
00074          16  ABEND-FILE-ID           PIC XX      VALUE ZEROS.
00075          16  ABEND-REASON            PIC XX      VALUE ZEROS.
00076      12  ABEND-OPTION                PIC X       VALUE 'Y'.
00077      12  LINK-ELDATCV                PIC X(8)    VALUE 'ELDATCV'.
00078      12  PGM-SUB                     PIC S999    VALUE +090  COMP.
00079      12  REIN-RT-SW                  PIC X       VALUE SPACE.
00080
00081      12  DTE-CLIENT                  PIC  XXX.
00082
00083      12  CLAS-INDEXL                 PIC S9(4) COMP.
00084      12  CLAS-INDEXA                 PIC S9(4) COMP.
00085
00086
00087  01  CLAS-INS-TYPES.
00088      12 CLAS-ALL-TYPES       OCCURS 2 TIMES.
00089          16  CLAS-I-BEN              PIC  XX.
00090          16  CLAS-I-AB3.
00091              20  FILLER              PIC  X.
00092              20  CLAS-I-AB2          PIC  XX.
00093          16  CLAS-I-AB10.
00094              20  FILLER              PIC  X(9).
00095              20  CLAS-I-REIN-YN      PIC  X.
00096          16  CLAS-I-JOINT            PIC  X.
00097          16  CLAS-I-RL-AH            PIC  X.
00098          16  CLAS-I-CALC-TYPE.
00099              20  CLAS-I-BAL          PIC  X.
00100          16  CLAS-I-EP               PIC  X.
00101
00102  EJECT
00103 *                                    COPY ELCDATE.
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
00104  EJECT
00105 *                                    COPY ECSCRT01.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ECSCRT01                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.016                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = CERTIFICATE RECORD    (OFF-LINE)          *
00008 *                                                                *
00009 *   FILE TYPE = SEQUENTIAL
00010 *   RECORD SIZE = 1056 RECFORM = FIXED                           *
00011 *                                                                *
00012 *   KEY DATA =                         START=4, LEN=36           *
00013 *                                                                *
CIDMOD*  NO  CID  MODS  IN  COPYBOOK  ECSCRT01                         *
CIDMOD*                                                                *
00014 ******************************************************************
122002******************************************************************
122002*                   C H A N G E   L O G
122002*
122002* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
122002*-----------------------------------------------------------------
122002*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
122002* EFFECTIVE    NUMBER
122002*-----------------------------------------------------------------
122002* 122002                   PEMA  ADD MONTHLY PRODUCT PROCESSING
100703* 100703    2003080800002  PEMA  ADD SUPER GAP PROCESSING
040504* 040504    2003080800002  PEMA  ADD DEALER INCENTIVE PROCESSING
020305* 020305    2005020000000  PEMA  ADD CLP STATE TO CERT RECORD
110105* 110105    2005071200004  PEMA  INCREASE SIZE OF LOAN OFFICER
042408* 042408    2007110500003  PEMA  ADD REFUND INTEREST PROCESSING
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
122002******************************************************************
00015
00016  01  CERTIFICATE-RECORD.
00017      12  CR-RECORD-ID                      PIC XX.
00018          88  VALID-CR-ID                      VALUE 'CR'.
00019
00020      12  CR-COMPANY-CD                     PIC X.
00021
00022      12  CR-FULL-CONTROL.
00023          16  CR-CONTROL-1.
00024              20  CR-ACCT-CONTROL.
00025                  24  CR-CARRIER            PIC X.
00026                  24  CR-GROUPING.
00027                      28  CR-GROUP-PREFIX   PIC XXX.
00028                      28  CR-GROUP-PRIME    PIC XXX.
00029                  24  CR-STATE              PIC XX.
00030                  24  CR-ACCOUNT.
00031                      28  CR-ACCT-PREFIX    PIC X(4).
00032                      28  CR-ACCT-PRIME     PIC X(6).
00033              20  CR-DT                     PIC 9(11)  COMP-3.
00034          16  CR-CERT-NO.
00035              20  CR-CERT.
00036                  24  CR-CERT-PREFIX        PIC X(3).
00037                  24  CR-CERT-PRIME         PIC X(7).
00038              20  CR-CERT-SFX               PIC X.
00039
00040      12  CR-INSUREDS-PROFILE.
00041          16  CR-NAME.
00042              20  CR-LNAME                  PIC X(15).
00043              20  CR-FNAME.
00044                  24  CR-1ST-INITIAL        PIC X.
00045                  24  FILLER                PIC X(9).
00046              20  CR-INIT                   PIC X.
00047          16  CR-AGE                        PIC 99.
00048          16  CR-SEX                        PIC X.
00049              88  CR-SEX-MALE                  VALUE 'M'.
00050              88  CR-SEX-FEMALE                VALUE 'F'.
00051          16  CR-SOC-SEC                    PIC X(11).
00052          16  CR-JOINT-NAME.
00053              20  CR-JT-LNAME               PIC X(15).
00054              20  CR-JT-FNAME.
00055                  24  CR-JT-1ST-INIT        PIC X.
00056                  24  FILLER                PIC X(9).
00057              20  CR-JT-INIT                PIC X.
00058          16  CR-JOINT-AGE                  PIC 99.
00059
00060          16  FILLER                        PIC X(20).
00061
00062      12  CR-LF-COVERAGE-DATA.
00063          16  CR-LFTYP                      PIC XX.
00064          16  CR-LF-TERM                    PIC S999       COMP-3.
00065          16  CR-LF-CRIT-PERIOD             PIC S999       COMP-3.
00066          16  CR-LF-TERM-IN-DAYS            PIC S9(5)      COMP-3.
00067          16  CR-LF-DEV-CODE                PIC XXX.
00068          16  CR-LF-DEV-PCT                 PIC S9V9(6)    COMP-3.
00069
00070          16  CR-LFAMT                      PIC S9(9)V99   COMP-3.
00071          16  CR-LFPRM                      PIC S9(7)V99   COMP-3.
00072          16  CR-LFPRM-CALC                 PIC S9(7)V99   COMP-3.
00073          16  CR-LFPRM-RATE                 PIC S99V9(5)   COMP-3.
00074
00075          16  CR-LFAMT-ALT                  PIC S9(9)V99   COMP-3.
00076          16  CR-LFPRM-ALT                  PIC S9(7)V99   COMP-3.
00077          16  CR-LFPRM-CALC-ALT             PIC S9(7)V99   COMP-3.
00078          16  CR-LFPRM-RATE-ALT             PIC S99V9(5)   COMP-3.
00079
00080          16  CR-LFRFND                     PIC S9(7)V99   COMP-3.
00081          16  CR-LFRFND-CALC                PIC S9(7)V99   COMP-3.
00082
00083          16  CR-LF-NSP-PRM                 PIC S9(7)V99   COMP-3.
00084          16  CR-LF-NSP-PRM-RATE            PIC S99V9(5)   COMP-3.
00085
00086          16  CR-LF-REFUND-TYPE             PIC X.
00087          16  CR-LF-POLICY-FEE              PIC S9(3)V99   COMP-3.
00088
00089          16  CR-LF-COMM-CHARGEBACK         PIC X.
00090              88  CR-NO-LF-CHARGEBACK          VALUE 'N'.
00091
00092          16  CR-LF-REI-RISK-PRM            PIC S9(7)V99   COMP-3.
00093
00094          16  CR-LF-EXPIRE-DATE             PIC 9(11)      COMP-3.
PEMMOD         16  CR-LF-ISS-PREM-TAX            PIC S9V9(4)    COMP-3.
PEMMOD         16  CR-LF-CNC-PREM-TAX            PIC S9V9(4)    COMP-3.
00095
00096          16  FILLER                        PIC X(14).
PEMMOD*        16  FILLER                        PIC X(20).
00097
00098      12  CR-AH-COVERAGE-DATA.
00099          16  CR-AHTYP                      PIC XX.
00100          16  CR-AH-TERM                    PIC S999       COMP-3.
00101          16  CR-AH-CRIT-PERIOD             PIC S999       COMP-3.
00102          16  CR-AH-DEV-CODE                PIC XXX.
00103          16  CR-AH-DEV-PCT                 PIC S9V9(6)    COMP-3.
00104
00105          16  CR-AHAMT                      PIC S9(7)V99   COMP-3.
00106          16  CR-AHPRM                      PIC S9(7)V99   COMP-3.
00107          16  CR-AHPRM-CALC                 PIC S9(7)V99   COMP-3.
00108          16  CR-AHPRM-RATE                 PIC S99V9(5)   COMP-3.
00109
00110          16  CR-AHRFND                     PIC S9(7)V99   COMP-3.
00111          16  CR-AHRFND-CALC                PIC S9(7)V99   COMP-3.
00112
00113          16  CR-AH-NSP-PRM                 PIC S9(7)V99   COMP-3.
00114          16  CR-AH-NSP-PRM-RATE            PIC S99V9(5)   COMP-3.
00115
00116          16  CR-AH-REFUND-TYPE             PIC X.
00117          16  CR-AH-POLICY-FEE              PIC S9(3)V99   COMP-3.
00118
00119          16  CR-AH-COMM-CHARGEBACK         PIC X.
00120              88  CR-NO-AH-CHARGEBACK          VALUE 'N'.
00121
00122          16  CR-AH-REI-RISK-PRM            PIC S9(7)V99   COMP-3.
00123
00124          16  CR-AH-EXPIRE-DATE             PIC 9(11)      COMP-3.
PEMMOD         16  CR-AH-ISS-PREM-TAX            PIC S9V9(4)    COMP-3.
PEMMOD         16  CR-AH-CNC-PREM-TAX            PIC S9V9(4)    COMP-3.
00125
00126          16  FILLER                        PIC XX.
PEMMOD*        16  FILLER                        PIC X(8).
00127
00128      12  CR-LOAN-DATA.
00129          16  CR-LIVES                      PIC S9(7)      COMP-3.
00130          16  CR-BILLED                     PIC S9(7)      COMP-3.
00131          16  CR-APR                        PIC S999V9(4)  COMP-3.
00132          16  CR-PMT-FREQ                   PIC 99.
00133          16  CR-LOAN-TERM                  PIC S999       COMP-3.
00134          16  CR-RATING-CLASS               PIC XX.
00135          16  CR-POLICY-FORM-NO             PIC X(12).
00136          16  CR-GRPTYP                     PIC XX.
00137          16  CR-IND-GRP                    PIC X.
00138              88  CR-INDIVIDUAL                VALUE '1'.
00139              88  CR-GROUP                     VALUE '2'.
00140          16  CR-SKIP                       PIC 99.
00141              88  NO-MONTHS-SKIPPED            VALUE 00.
00142              88  SKIP-JULY                    VALUE 01.
00143              88  SKIP-AUGUST                  VALUE 02.
00144              88  SKIP-SEPTEMBER               VALUE 03.
00145              88  SKIP-JULY-AUG                VALUE 04.
00146              88  SKIP-AUG-SEPT                VALUE 05.
00147              88  SKIP-JULY-AUG-SEPT           VALUE 06.
00148              88  SKIP-JUNE-JULY-AUG           VALUE 07.
00149              88  SKIP-JUNE                    VALUE 08.
00150              88  SKIP-JUNE-JULY               VALUE 09.
00151              88  SKIP-AUG-SEPT-OCT            VALUE 10.
00152              88  SKIP-BI-WEEKLY-3RD-PMT       VALUE 11.
00153          16  CR-RESIDENT-STATE             PIC XX.
00154          16  CR-RATE-CODE                  PIC X(4).
00155          16  CR-MORT.
00156              20  CR-TAB                    PIC X.
00157              20  CR-INT                    PIC 99.
00158              20  CR-GRP                    PIC X.
00159          16  CR-MEMBER-NO                  PIC X(12).
110105         16  CR-OLD-LOF                    PIC XXX.
110105*        16  CR-LOAN-OFFICER               PIC XXX.
00161          16  CR-REIN-TABLE                 PIC XXX.
00162          16  CR-REIN-SPEC                  PIC X.
00163          16  CR-LOAN-1ST-PMT-DT.
00164              20  CR-1ST-PMT-YR             PIC 99.
00165              20  CR-1ST-PMT-MO             PIC 99.
00166              20  CR-1ST-PMT-DA             PIC 99.
00167          16  CR-SUM-CAN-CNT-ITD            PIC S999       COMP-3.
00168          16  CR-SUM-CAN-CNT-YTD            PIC S999       COMP-3.
00169          16  CR-PMT-EXTENSION-DAYS         PIC S999       COMP-3.
00170          16  CR-LAST-ADD-ON-DT             PIC XX.
00171
00172          16  CR-UNDERWRITING-CODE          PIC X.
00173              88  CR-POLICY-UNDERWRITTEN       VALUE 'Y'.
00174
00175          16  CR-STATE-TAX                  PIC S9(7)V99   COMP-3.
00176          16  CR-MUNI-TAX                   PIC S9(7)V99   COMP-3.
00177          16  CR-CANCEL-STATE-TAX           PIC S9(7)V99   COMP-3.
00178          16  CR-CANCEL-MUNI-TAX            PIC S9(7)V99   COMP-3.
00179
00180      12  CR-STATUS-INFORMATION.
00181          16  CR-ENTRY-STATUS               PIC X.
00182              88  CR-NORMAL-ENTRY              VALUE '1'.
00183              88  CR-POLICY-IS-RESTORE         VALUE '3'.
00184              88  CR-CONVERSION-ENTRY          VALUE '4'.
00185              88  CR-POLICY-IS-REISSUE         VALUE '5'.
122002             88  CR-POLICY-IS-MONTHLY         VALUE 'M'.
00186              88  CR-POLICY-IS-REIN-ONLY       VALUE '9'.
00187              88  CR-POLICY-IS-DECLINED        VALUE 'D'.
00188              88  CR-POLICY-IS-VOID            VALUE 'V'.
00189              88  CR-POLICY-IS-PREM-ONLY       VALUE 'P'.
00190              88  CR-POLICY-IS-ACTIVE          VALUE '1' '3' '4'
00191                                                     '5' '9' 'P'.
00192          16  CR-ENTRY-DATE                 PIC 9(11)  COMP-3.
00193
00194          16  CR-LF-STATUS-AT-CANCEL        PIC X.
00195          16  CR-LF-CANC-DT                 PIC 9(11)  COMP-3.
00196          16  CR-LF-CANCEL-EXIT-DATE        PIC 9(11)  COMP-3.
00197
00198          16  CR-LF-STATUS-AT-DEATH         PIC X.
00199          16  CR-LF-CLAIM-EXIT-DATE         PIC 9(11)  COMP-3.
00200
00201          16  CR-LF-CURRENT-STATUS          PIC X.
00202              88  CR-LF-NORMAL-ENTRY           VALUE '1'.
00203              88  CR-LF-POLICY-PENDING         VALUE '2'.
00204              88  CR-LF-POLICY-IS-RESTORE      VALUE '3'.
00205              88  CR-LF-CONVERSION-ENTRY       VALUE '4'.
00206              88  CR-LF-POLICY-IS-REISSUE      VALUE '5'.
122002             88  CR-LF-POLICY-IS-MONTHLY      VALUE 'M'.
00207              88  CR-LF-LUMP-SUM-DISAB         VALUE '6'.
00208              88  CR-LF-DEATH-CLAIM-APPLIED    VALUE '7'.
00209              88  CR-LF-CANCEL-APPLIED         VALUE '8'.
00210              88  CR-LF-IS-REIN-ONLY           VALUE '9'.
00211              88  CR-LF-IS-DECLINED            VALUE 'D'.
00212              88  CR-LF-IS-VOID                VALUE 'V'.
00213              88  CR-LF-POLICY-IS-ACTIVE       VALUE '1' '2' '3'
00214                                                     '4' '5' '9'.
00215
00216          16  CR-AH-STATUS-AT-CANCEL        PIC X.
00217
00218          16  CR-AH-CANC-DT                 PIC 9(11)  COMP-3.
00219          16  CR-AH-CANCEL-EXIT-DATE        PIC 9(11)  COMP-3.
00220
00221          16  CR-AH-STATUS-AT-SETTLEMENT    PIC X.
00222          16  CR-AH-SETTLEMENT-EXIT-DATE    PIC 9(11)  COMP-3.
00223
00224          16  CR-AH-CURRENT-STATUS          PIC X.
00225              88  CR-AH-NORMAL-ENTRY           VALUE '1'.
00226              88  CR-AH-POLICY-PENDING         VALUE '2'.
00227              88  CR-AH-POLICY-IS-RESTORE      VALUE '3'.
00228              88  CR-AH-CONVERSION-ENTRY       VALUE '4'.
00229              88  CR-AH-POLICY-IS-REISSUE      VALUE '5'.
122002             88  CR-AH-POLICY-IS-MONTHLY      VALUE 'M'.
00230              88  CR-AH-LUMP-SUM-DISAB         VALUE '6'.
00231              88  CR-AH-DEATH-CLAIM-APPLIED    VALUE '7'.
00232              88  CR-AH-CANCEL-APPLIED         VALUE '8'.
00233              88  CR-AH-IS-REIN-ONLY           VALUE '9'.
00234              88  CR-AH-IS-DECLINED            VALUE 'D'.
00235              88  CR-AH-IS-VOID                VALUE 'V'.
00236              88  CR-AH-POLICY-IS-ACTIVE       VALUE '1' '2' '3'
00237                                                     '4' '5' '9'.
110105         16  CR-LOAN-OFFICER               PIC X(5).
110105         16  FILLER                        PIC X(15).
110105*        16  FILLER                        PIC X(20).
00239
00240      12  CR-DEATH-CLAIM-DATA.
00241          16  CR-NUM-DTH-CLM                PIC S999       COMP-3.
00242
00243          16  CR-DTH-DT                     PIC 9(11)      COMP-3.
00244          16  CR-DTH-RPT-DT.
00245              20  CR-DTH-RPT-YR             PIC 99.
00246              20  CR-DTH-RPT-MO             PIC 99.
00247              20  CR-DTH-RPT-DA             PIC 99.
00248          16  CR-DTH-PAY-DT                 PIC 9(11)      COMP-3.
00249
00250          16  CR-DTHAMT                     PIC S9(9)V99   COMP-3.
00251          16  CR-DTHAMT-YTD                 PIC S9(9)V99   COMP-3.
00252          16  CR-DTHAMT-LAST                PIC S9(9)V99   COMP-3.
00253          16  CR-DTHEXP                     PIC S9(7)V99   COMP-3.
00254          16  CR-DTHEXP-YTD                 PIC S9(7)V99   COMP-3.
00255
00256          16  CR-DTH-AGE                    PIC 99.
00257          16  CR-DTH-PAY-CD                 PIC X.
00258          16  CR-DEATH-CAUSE                PIC X(6).
00259
00260          16  FILLER                        PIC X(16).
00261
00262      12  CR-DISAB-CLAIM-DATA.
00263          16  CR-NUM-DIS-CLM                PIC S999       COMP-3.
00264
00265          16  CR-DIS-DT                     PIC 9(11)      COMP-3.
00266          16  CR-DIS-RPT-DT.
00267              20  CR-DIS-RPT-YR             PIC 99.
00268              20  CR-DIS-RPT-MO             PIC 99.
00269              20  CR-DIS-RPT-DA             PIC 99.
00270          16  CR-DIS-PAY-DT                 PIC 9(11)      COMP-3.
00271          16  CR-DIS-PTO-DT                 PIC 9(11)      COMP-3.
00272
00273          16  CR-DISAMT                     PIC S9(9)V99   COMP-3.
00274          16  CR-DISAMT-YTD                 PIC S9(9)V99   COMP-3.
00275          16  CR-DISAMT-LAST                PIC S9(9)V99   COMP-3.
00276          16  CR-DISEXP                     PIC S9(7)V99   COMP-3.
00277          16  CR-DISEXP-YTD                 PIC S9(7)V99   COMP-3.
00278
00279          16  CR-DAYS-DISAB                 PIC 999        COMP-3.
00280          16  CR-DIS-PAY-CD                 PIC X.
00281          16  FILLER                        PIC XX.
00282
00283          16  CR-DISAB-INCURRED-DETAIL.
00284              20  CR-DISAB-DETAIL-DATA  OCCURS 5 TIMES.
00285 **ELCCRTVR MUST BE CHANGED IF THE "OCCURS" IS CHANGED.
00286                  24  CR-DIS-INCUR-DT       PIC 9(11)     COMP-3.
00287                  24  CR-INCUR-DISAMT       PIC S9(9)V99  COMP-3.
00288                  24  CR-INCUR-DISEXP       PIC S9(9)V99  COMP-3.
00289
00290          16  CR-DISAB-CAUSE                PIC X(6).
00291          16  FILLER                        PIC X(14).
00292
00293      12  CR-REMIT-TO                       PIC 99.
00294
00295      12  CR-COMPENSATION-LEVELS.
00296          16  CR-AGT-LEVELS      OCCURS 10 TIMES.
00297              20  CR-COM-AGT.
00298                  24  CR-COM-AGT-PREFIX     PIC X(4).
00299                  24  CR-COM-AGT-PRIME      PIC X(6).
00300              20  CR-AGT-TYPE               PIC X.
00301              20  CR-LCOM-L                 PIC SV9(5)     COMP-3.
00302              20  CR-LCOM-AH                PIC SV9(5)     COMP-3.
00303
100703     12  CR-BANK-NOCHRGB-MONTHS            PIC 99.
011904     12  CR-MOB-NET-TOT-FEES               PIC S9(7)V99   COMP-3.
040504     12  CR-ADDL-CLP                       PIC S9(5)V99   COMP-3.
020305     12  CR-CLP-STATE                      PIC XX.
041508     12  CR-POST-CARD-IND                  PIC X.
011410     12  CR-LF-CLP                         PIC S9(5)V99   COMP-3.
011410     12  CR-AH-CLP                         PIC S9(5)V99   COMP-3.
011410     12  FILLER                            PIC X(18).
00305
00306      12  CR-CSR-CODE                       PIC XXX.
00307
00308      12  CR-DEDUCTIBLE-AMOUNTS.
00309          16  CR-CLAIM-DEDUCT-WITHHELD      PIC S9(5)V99   COMP-3.
00310          16  CR-CANCEL-DEDUCT-WITHHELD     PIC S9(5)V99   COMP-3.
00311
00312      12  CR-MICROFILM-NUMBERS.
011410*        16  CR-ISS-MICROFILM-NO           PIC S9(9)      COMP-3.
011410         16  FILLER                        PIC X(5).
00314      12  CR-NH-INT-ON-REF                  PIC S9(7)V99   COMP-3.
00315
00316      12  CR-USER-CODE                      PIC X.
00317      12  CR-USER-FUTURE                    PIC X(9).
00318
100703     12  CR-BENEFICIARY.
100703         16  CR-BANK-NO                    PIC X(10).
100703         16  FILLER                        PIC X(15).
00320
00321      12  CR-ENTRY-BATCH                    PIC X(6).
00322      12  CR-LF-EXIT-BATCH                  PIC X(6).
00323      12  CR-AH-EXIT-BATCH                  PIC X(6).
00324
00325      12  CR-NOTE-SW                        PIC X.
00326
00327      12  CR-ORIGIN-INDICATOR               PIC X.
00328          88  CR-ENTERED-MANUALLY              VALUE '1'.
00329          88  CR-CREATED-FROM-TAPE-LOAD        VALUE '2'.
00330
042408     12  CR-LF-CNC-ENT-DT                  PIC 9(11)  COMP-3.
042408     12  CR-AH-CNC-ENT-DT                  PIC 9(11)  COMP-3.
00331      12  FILLER                            PIC X(33).
00332
00333 ******************************************************************
00106
00107  EJECT
00108 *                                    COPY ELCREIN.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCREIN.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.006                          *
00006 *                                                                *
00007 *   DESCRIPTION:  CALCULATE REINSURANCE TEST CASE                *
00008 *                                                                *
00009 *  PASSED TO ELREIN                                              *
00010 *  -----------------                                             *
00011 *  TABLE CODE                                                    *
00012 *  EFFECTIVE DATE                                                *
00013 *  ISSUE AGE                                                     *
00014 *  ORIGINAL TERM                                                 *
00015 *  LIFE TYPE                                                     *
00016 *  LIFE PREMIUM                                                  *
00017 *  LIFE BENEFIT                                                  *
00018 *  LIFE REFUND                                                   *
00019 *  A/H  TYPE                                                     *
00020 *  A/H  PREMIUM                                                  *
00021 *  A/H  BENEFIT                                                  *
00022 *  A/H  REFUND                                                   *
00023 *  I/G  CODE                                                     *
00024 *  CLAIM AMOUNT                                                  *
00025 *                                                                *
00026 *  RETURNED FROM ELREIN                                          *
00027 *  --------------------                                          *
00028 *  CEDED TO                                                      *
00029 *  LIFE PREMIUM                                                  *
00030 *  LIFE BENEFIT                                                  *
00031 *  LIFE REFUND                                                   *
00032 *  LIFE CLAIM                                                    *
00033 *  A/H  PREMIUM                                                  *
00034 *  A/H  BENEFIT                                                  *
00035 *  A/H  REFUND                                                   *
00036 *  A/H  CLAIM                                                    *
00037 *                                                                *
00038 ******************************************************************
00039
00040  01  REINSURANCE-PASS-AREA.
00041      12  CP-COMM-LENGTH            PIC S9(4)         VALUE +2868
00042                                      COMP.
00043
00044      12  CP-RETURN-CODE            PIC X             VALUE ZERO.
00045        88  NO-CP-ERROR                             VALUE ZERO.
00046        88  CP-ERROR-OCCURED                        VALUE '1'  '2'
00047                                                          '3'  '4'
00048                                                          '5'.
00049        88  CP-ERROR-IN-AMOUNTS                     VALUE '1'.
00050        88  CP-ERROR-IN-DATES                       VALUE '2'.
00051        88  CP-ERROR-IN-OPTIONS                     VALUE '3'.
00052        88  CP-ERROR-IN-TERMS                       VALUE '4'.
00053        88  CP-ERROR-IN-FREQUENCY                   VALUE '5'.
00054
00055 ***********************  INPUT AREAS ****************************
00056
00057      12  CP-CALCULATION-AREA.
00058          16  CP-TBCOD              PIC X(3).
00059          16  CP-COMPANY-CD         PIC X.
00060          16  CP-COMPANY-ID         PIC X(3).
00061          16  CP-EFF-DT             PIC XX.
00062          16  CP-ISSUE-AGE          PIC S9(3)         VALUE ZERO
00063                                      COMP-3.
00064          16  CP-ORIGINAL-TERM      PIC S9(3)         VALUE ZERO
00065                                      COMP-3.
00066          16  CP-LF-TYPE            PIC XX.
00067          16  CP-LF-PREM            PIC S9(7)V99      VALUE ZERO
00068                                      COMP-3.
00069          16  CP-LF-BEN             PIC S9(9)V99      VALUE ZERO
00070                                      COMP-3.
00071          16  CP-LF-REF             PIC S9(7)V99      VALUE ZERO
00072                                      COMP-3.
00073          16  CP-AH-TYPE            PIC XX.
00074          16  CP-AH-PREM            PIC S9(7)V99      VALUE ZERO
00075                                      COMP-3.
00076          16  CP-AH-BEN             PIC S9(7)V99      VALUE ZERO
00077                                      COMP-3.
00078          16  CP-AH-REF             PIC S9(7)V99      VALUE ZERO
00079                                      COMP-3.
00080          16  CP-CLAIM-AMT          PIC S9(9)V99      VALUE ZERO
00081                                      COMP-3.
00082          16  CP-IG-CODE            PIC X.
00083
00084          16  CP-LFPRM-ALT          PIC S9(07)V99     VALUE +0
00085                                      COMP-3.
00086          16  CP-LFAMT-ALT          PIC S9(09)V99     VALUE +0
00087                                      COMP-3.
00088          16  FILLER                PIC X(59).
00089
00090 ****************** OUTPUT AREA **********************************
00091      12  REIN-WORK-AREAS.
00092          16  RW-LFAMT        PIC S9(9)V99   VALUE +0    COMP-3.
00093          16  RW-AHWRK        PIC S9(7)V99   VALUE +0    COMP-3.
00094          16  RW-LFPRM        PIC S9(7)V99   VALUE +0    COMP-3.
00095          16  RW-LFPRMC       PIC S9(7)V99   VALUE +0    COMP-3.
00096          16  RW-AHPRM        PIC S9(7)V99   VALUE +0    COMP-3.
00097          16  RW-AHPRMC       PIC S9(7)V99   VALUE +0    COMP-3.
00098          16  RW-LFCLMWK      PIC S9(9)V99   VALUE +0    COMP-3.
00099          16  RW-AHCLMWK      PIC S9(9)V99   VALUE +0    COMP-3.
00100          16  RW-LFCLM        PIC S9(9)V99   VALUE +0    COMP-3.
00101          16  RW-AHCLM        PIC S9(9)V99   VALUE +0    COMP-3.
00102          16  RW-AH-LIMIT     PIC S9(7)V99   VALUE +0    COMP-3.
00103          16  RW-LFIBNR       PIC S9(7)V99   VALUE +0    COMP-3.
00104          16  RW-LFPAYCUR     PIC S9(7)V99   VALUE +0    COMP-3.
00105          16  RW-LFFUTRSV     PIC S9(7)V99   VALUE +0    COMP-3.
00106          16  RW-AHIBNR       PIC S9(7)V99   VALUE +0    COMP-3.
00107          16  RW-AHPAYCUR     PIC S9(7)V99   VALUE +0    COMP-3.
00108          16  RW-AHFUTRSV     PIC S9(7)V99   VALUE +0    COMP-3.
00109          16  RW-LFIBNRWK     PIC S9(7)V99   VALUE +0    COMP-3.
00110          16  RW-LFPAYCURWK   PIC S9(7)V99   VALUE +0    COMP-3.
00111          16  RW-LFFUTRSVWK   PIC S9(7)V99   VALUE +0    COMP-3.
00112          16  RW-AHIBNRWK     PIC S9(7)V99   VALUE +0    COMP-3.
00113          16  RW-AHPAYCURWK   PIC S9(7)V99   VALUE +0    COMP-3.
00114          16  RW-AHFUTRSVWK   PIC S9(7)V99   VALUE +0    COMP-3.
PEMMOD         16  RW-AHLIM-LO     PIC S9(9)V99   VALUE +0    COMP-3.
pemmod         16  RW-AHLIM-HI     PIC S9(9)V99   VALUE +0    COMP-3.
00117          16  RW-ACCUM-LF     PIC S9(5)V99   VALUE +0    COMP-3.
00118          16  RW-ACCUM-AH     PIC S9(5)V99   VALUE +0    COMP-3.
00119          16  RW-ACCUM-CLM    PIC S9(5)V99   VALUE +0    COMP-3.
00120          16  RW-ACCUM-IBNR   PIC S9(5)V99   VALUE +0    COMP-3.
00121          16  RW-ACCUM-PAYCUR PIC S9(5)V99   VALUE +0    COMP-3.
00122          16  RW-ACCUM-FUTRSV PIC S9(5)V99   VALUE +0    COMP-3.
00123
00124      12  REIN-HOLD-AREAS.
00125          16  REIN-LEVELS                 OCCURS 30 TIMES.
00126              20  REIN-COMP.
00127                  24  REIN-CO-PRIME       PIC XXX.
00128                  24  REIN-CO-SUB         PIC XXX.
00129              20  REIN-LF-AH-FLGS.
00130                  24  REIN-LF-FLG         PIC X.
00131                  24  REIN-AH-FLG         PIC X.
00132
00133              20  REIN-WORK-FLDS.
00134                  24  REIN-LFAMT          PIC S9(9)V99   COMP-3.
00135                  24  REIN-LFPRM          PIC S9(7)V99   COMP-3.
00136                  24  REIN-AHAMT          PIC S9(7)V99   COMP-3.
00137                  24  REIN-AHPRM          PIC S9(7)V99   COMP-3.
00138                  24  REIN-LFRFND         PIC S9(7)V99   COMP-3.
00139                  24  REIN-AHRFND         PIC S9(7)V99   COMP-3.
00140                  24  REIN-LFCLM          PIC S9(9)V99   COMP-3.
00141                  24  REIN-AHCLM          PIC S9(9)V99   COMP-3.
00142                  24  REIN-LFCLML         PIC S9(9)V99   COMP-3.
00143                  24  REIN-AHCLML         PIC S9(9)V99   COMP-3.
00144                  24  REIN-DIS-IBNR       PIC S9(7)V99   COMP-3.
00145                  24  REIN-DIS-PAYCUR     PIC S9(7)V99   COMP-3.
00146                  24  REIN-DIS-FUTRSV     PIC S9(7)V99   COMP-3.
00147                  24  REIN-DIS-IBNRL      PIC S9(7)V99   COMP-3.
00148                  24  REIN-DIS-PAYCURL    PIC S9(7)V99   COMP-3.
00149                  24  REIN-DIS-FUTRSVL    PIC S9(7)V99   COMP-3.
00150                  24  REIN-DTH-IBNR       PIC S9(7)V99   COMP-3.
00151                  24  REIN-DTH-PAYCUR     PIC S9(7)V99   COMP-3.
00152                  24  REIN-DTH-FUTRSV     PIC S9(7)V99   COMP-3.
00153                  24  REIN-DTH-IBNRL      PIC S9(7)V99   COMP-3.
00154                  24  REIN-DTH-PAYCURL    PIC S9(7)V99   COMP-3.
00155                  24  REIN-DTH-FUTRSVL    PIC S9(7)V99   COMP-3.
00156                  24  REIN-AH-LIMIT       PIC S9(7)V99   COMP-3.
00157                  24  REIN-REM-SW         PIC X.
00158                  24  REIN-REM-AH-100     PIC X.
00159      12  REIN-LEVELS-END                 PIC X(6).
00160
00161      12  RWF-FIELDS.
00162          16  RWF-LFAMT                   PIC S9(9)V99   COMP-3.
00163          16  RWF-LFPRM                   PIC S9(7)V99   COMP-3.
00164          16  RWF-AHAMT                   PIC S9(7)V99   COMP-3.
00165          16  RWF-AHPRM                   PIC S9(7)V99   COMP-3.
00166          16  RWF-LFRFND                  PIC S9(7)V99   COMP-3.
00167          16  RWF-AHRFND                  PIC S9(7)V99   COMP-3.
00168          16  RWF-LFCLM                   PIC S9(9)V99   COMP-3.
00169          16  RWF-AHCLM                   PIC S9(9)V99   COMP-3.
00170          16  RWF-LFCLML                  PIC S9(9)V99   COMP-3.
00171          16  RWF-AHCLML                  PIC S9(9)V99   COMP-3.
00172          16  RWF-DIS-IBNR                PIC S9(7)V99   COMP-3.
00173          16  RWF-DIS-PAYCUR              PIC S9(7)V99   COMP-3.
00174          16  RWF-DIS-FUTRSV              PIC S9(7)V99   COMP-3.
00175          16  RWF-DIS-IBNRL               PIC S9(7)V99   COMP-3.
00176          16  RWF-DIS-PAYCURL             PIC S9(7)V99   COMP-3.
00177          16  RWF-DIS-FUTRSVL             PIC S9(7)V99   COMP-3.
00178          16  RWF-DTH-IBNR                PIC S9(7)V99   COMP-3.
00179          16  RWF-DTH-PAYCUR              PIC S9(7)V99   COMP-3.
00180          16  RWF-DTH-FUTRSV              PIC S9(7)V99   COMP-3.
00181          16  RWF-DTH-IBNRL               PIC S9(7)V99   COMP-3.
00182          16  RWF-DTH-PAYCURL             PIC S9(7)V99   COMP-3.
00183          16  RWF-DTH-FUTRSVL             PIC S9(7)V99   COMP-3.
00184          16  RWF-AH-LIMIT                PIC S9(7)V99   COMP-3.
00185          16  RWF-REM-SW                  PIC X.
00186          16  RWF-REM-AH-100              PIC X.
00187
00188      12  WT-COMP-INFO.
00189          16  WT-REI-COMP.
00190              20  WT-REIN-PRIME           PIC XXX.
00191              20  WT-REIN-SUB             PIC XXX.
00192          16  WT-LF-QC                    PIC X.
00193          16  WT-AH-QC                    PIC X.
00194          16  WT-LO-DATE                  PIC 9(11)  COMP-3.
00195          16  WT-HI-DATE                  PIC 9(11)  COMP-3.
00196          16  WT-LFAGE-LO                 PIC 99.
00197          16  WT-LFAGE-HI                 PIC 99.
00198          16  WT-AHAGE-LO                 PIC 99.
00199          16  WT-AHAGE-HI                 PIC 99.
00200          16  WT-LFTRM-LO                 PIC S9(3)      COMP-3.
00201          16  WT-LFTRM-HI                 PIC S9(3)      COMP-3.
00202          16  WT-AHTRM-LO                 PIC S9(3)      COMP-3.
00203          16  WT-AHTRM-HI                 PIC S9(3)      COMP-3.
00204          16  WT-LF-PCT                   PIC S9V9999    COMP-3.
00205          16  WT-AH-PCT                   PIC S9V9999    COMP-3.
00206          16  WT-LF-LIM-LO                PIC S9(9)V99   COMP-3.
00207          16  WT-LF-LIM-HI                PIC S9(9)V99   COMP-3.
00208          16  WT-LF-LO                    PIC S9(9)V99   COMP-3.
00209          16  WT-LF-HI                    PIC S9(9)V99   COMP-3.
00210          16  WT-AHBEN-LIM-LO             PIC S9(7)V99   COMP-3.
00211          16  WT-AHBEN-LIM-HI             PIC S9(7)V99   COMP-3.
00212          16  WT-AHBEN-LO                 PIC S9(7)V99   COMP-3.
00213          16  WT-AHBEN-HI                 PIC S9(7)V99   COMP-3.
00214          16  WT-AHMOA-LIM-LO             PIC S9(7)V99   COMP-3.
00215          16  WT-AHMOA-LIM-HI             PIC S9(7)V99   COMP-3.
00216          16  WT-AHMOA-LO                 PIC S9(7)V99   COMP-3.
00217          16  WT-AHMOA-HI                 PIC S9(7)V99   COMP-3.
00218          16  WT-LF-BEN-CODE              PIC X.
00219          16  WT-AH-BEN-CODE              PIC X.
00220          16  WT-INTERACTIVE              PIC X.
00221          16  WT-REMAINING                PIC X.
00222
00223      12  REIN-MISC-WORK-FIELDS.
00224          16  SUB1                    PIC S9(3)      COMP.
00225          16  SUB2                    PIC S9(3)      COMP.
00226          16  CO-SUB                  PIC S9(3)      COMP.
00227          16  REIN-FACTOR             PIC S99V9(7) COMP-3 VALUE +0.
00228          16  REIN-CALCED-LIFE        PIC S9(7)V99 COMP-3 VALUE +0.
00229          16  REIN-CALCED-AH          PIC S9(7)V99 COMP-3 VALUE +0.
00230
00231          16  REIN-SW-DTE             PIC  9(11)  COMP-3.
00232          16  REIN-OPEN-SW            PIC  X          VALUE SPACE.
00233          16  REIN-SEARCH.
00234              20  REIN-SRCH-CODE      PIC  X          VALUE SPACE.
00235              20  REIN-SRCH           PIC  XXX        VALUE SPACE.
00236          16  SAVE-REIN-SRCH          PIC  X(4)       VALUE SPACE.
00237          16  REIN-BUS-FLAG           PIC  X          VALUE SPACE.
00238          16  RATE-WORK               PIC S9(8)V9(5) COMP-3.
00239          16  RATE-WORK-L             PIC S9(8)V9(5) COMP-3.
00240          16  RATE-WORK-A             PIC S9(8)V9(5) COMP-3.
00241          16  REIN-WRK                PIC S9(7)V99   COMP-3.
00242          16  REIN-WRK-2              PIC S9(7)V99   COMP-3.
00243          16  RS-LIFE-BEN       PIC S9(9)V99   COMP-3    VALUE +0.
00244          16  RS-LIFE-PREM      PIC S9(7)V99   COMP-3    VALUE +0.
00245          16  RS-AH-BEN         PIC S9(7)V99   COMP-3    VALUE +0.
00246          16  RS-AH-PREM        PIC S9(7)V99   COMP-3    VALUE +0.
00247          16  RS-LP-CALC        PIC S9(7)V99   COMP-3    VALUE +0.
00248          16  RS-AP-CALC        PIC S9(7)V99   COMP-3    VALUE +0.
00249          16  RS-R-LB           PIC S9(9)V99   COMP-3    VALUE +0.
00250          16  RS-R-LP           PIC S9(7)V99   COMP-3    VALUE +0.
00251          16  RS-R-AB           PIC S9(9)V99   COMP-3    VALUE +0.
00252          16  RS-R-AP           PIC S9(7)V99   COMP-3    VALUE +0.
00253          16  RS-R-LPC          PIC S9(7)V99   COMP-3    VALUE +0.
00254          16  RS-R-APC          PIC S9(7)V99   COMP-3    VALUE +0.
00255
00256      12  REIN-CO-HOLD-TABLE.
00257          16  REIN-CO-HOLD-ENTRIES  OCCURS 200 TIMES.
00258              20  RCT-REIN-CO.
00259                  24  RCT-REIN-PRIME   PIC XXX.
00260                  24  RCT-REIN-SUB     PIC XXX.
00261              20  RCT-CLM-CUTOFF-DT    PIC 9(11)  COMP-3.
00262              20  RCT-LF-CLM-PCT       PIC S9V9(4)    COMP-3.
00263              20  RCT-LF-CLM-MAX       PIC S9(7)V99   COMP-3.
00264              20  RCT-AH-CLM-PCT       PIC S9V9(4)    COMP-3.
00265              20  RCT-AH-CLM-MAX       PIC S9(7)V99   COMP-3.
00266
00267      12  REIN-CO-TABLE-ENT-CNT     PIC S9(3) COMP-3    VALUE +200.
00268 ******************************************************************
00109
00110  EJECT
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
00112  01  DFHCOMMAREA                     PIC X(4600).
00113
00114 *01 PARMLIST .
00115 *    02  FILLER                      PIC S9(8)   COMP.
00116 *    02  ELCNTL-POINTER              PIC S9(8)   COMP.
00117 *    02  ERREIN-POINTER              PIC S9(8)   COMP.
00118
00119  EJECT
00120 *                                    COPY ELCCNTL.
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
00121  EJECT
00122 *                                    COPY ERCREIN.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ERCREIN                             *
00003 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00004 *                            VMOD=2.010                          *
00005 *                                                                *
00006 *   ONLINE CREDIT SYSTEM                                         *
00007 *                                                                *
00008 *   FILE DESCRIPTION = REINSURANCE MASTER FILE                   *
00009 *                                                                *
00010 *   FILE TYPE = VSAM,KSDS                                        *
00011 *   RECORD SIZE = 4000  RECFORM = FIXED                          *
00012 *                                                                *
00013 *   BASE CLUSTER NAME = ERREIN                   RKP=2,LEN=8     *
00014 *       ALTERNATE PATH = NONE                                    *
00015 *                                                                *
00016 *   LOG = NO                                                     *
00017 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00018 *                                                                *
00019 ******************************************************************
103101*                   C H A N G E   L O G
103101*
103101* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
103101*-----------------------------------------------------------------
103101*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
103101* EFFECTIVE    NUMBER
103101*-----------------------------------------------------------------
103101* 103101    2001100100006  SMVA  ADD STATE EXHIBIT REPORT OPTION F
032707* 032707    2007032100006  PEMA  ADD EXCISE TAX CAPABILITY
103101******************************************************************
00021  01  REINSURANCE-RECORD.
00022      12  RE-RECORD-ID                      PIC XX.
00023          88  VALID-RE-ID                      VALUE 'RE'.
00024
00025      12  RE-CONTROL-PRIMARY.
00026          16  RE-COMPANY-CD                 PIC X.
00027          16  RE-KEY.
00028              20  RE-CODE                   PIC X.
00029                  88  RE-TABLE-RECORD          VALUE 'A'.
00030                  88  RE-COMPANY-RECORD        VALUE 'B'.
00031              20  RE-TABLE                  PIC XXX.
00032              20  FILLER                    PIC XXX.
00033          16  RE-COMPANY-KEY REDEFINES RE-KEY.
00034              20  FILLER                    PIC X.
00035              20  RE-COMPANY.
00036                  24  RE-COMP-PRIME         PIC XXX.
00037                  24  RE-COMP-SUB           PIC XXX.
00038
00039      12  RE-MAINT-INFORMATION.
00040          16  RE-LAST-MAINT-DT              PIC XX.
00041          16  RE-LAST-MAINT-HHMMSS          PIC S9(7)      COMP-3.
00042          16  RE-LAST-MAINT-USER            PIC X(4).
00043          16  FILLER                        PIC X(10).
00044
00045      12  RE-TABLE-DATA.
00046          16  RE-100-COMP                   PIC 99.
00047
00048          16  RE-COMP-INFO    OCCURS 30 TIMES.
00049              20  RE-REI-COMP-NO.
00050                  24  RE-REI-COMP           PIC XXX.
00051                  24  RE-REI-COMP-SUB       PIC XXX.
00052              20  RE-LF-QC                  PIC X.
00053              20  RE-AH-QC                  PIC X.
00054              20  RE-LO-DATE                PIC 9(11)     COMP-3.
00055              20  RE-HI-DATE                PIC 9(11)     COMP-3.
00056              20  RE-LFAGE-LO               PIC 99.
00057              20  RE-LFAGE-HI               PIC 99.
00058              20  RE-AHAGE-LO               PIC 99.
00059              20  RE-AHAGE-HI               PIC 99.
00060              20  RE-LFTRM-LO               PIC S999       COMP-3.
00061              20  RE-LFTRM-HI               PIC S999       COMP-3.
00062              20  RE-AHTRM-LO               PIC S999       COMP-3.
00063              20  RE-AHTRM-HI               PIC S999       COMP-3.
00064              20  RE-LF-PCT                 PIC S9V9999    COMP-3.
00065              20  RE-AH-PCT                 PIC S9V9999    COMP-3.
00066              20  RE-LF-LIM-LO              PIC S9(9)V99   COMP-3.
00067              20  RE-LF-LIM-HI              PIC S9(9)V99   COMP-3.
00068              20  RE-LF-LO                  PIC S9(9)V99   COMP-3.
00069              20  RE-LF-HI                  PIC S9(9)V99   COMP-3.
00070              20  RE-AHBEN-LIM-LO           PIC S9(7)V99   COMP-3.
00071              20  RE-AHBEN-LIM-HI           PIC S9(7)V99   COMP-3.
00072              20  RE-AHBEN-LO               PIC S9(7)V99   COMP-3.
00073              20  RE-AHBEN-HI               PIC S9(7)V99   COMP-3.
00074              20  RE-AHMOA-LIM-LO           PIC S9(7)V99   COMP-3.
00075              20  RE-AHMOA-LIM-HI           PIC S9(7)V99   COMP-3.
00076              20  RE-AHMOA-LO               PIC S9(7)V99   COMP-3.
00077              20  RE-AHMOA-HI               PIC S9(7)V99   COMP-3.
00078              20  RE-LF-BEN-CODE            PIC X.
00079              20  RE-AH-BEN-CODE            PIC X.
00080              20  RE-INTERACTIVE            PIC X.
00081              20  RE-REMAINING              PIC X.
CIDMOD             20  RE-LF-RUNOFF-SW           PIC X.
CIDMOD             20  RE-AH-RUNOFF-SW           PIC X.
CIDMOD             20  FILLER                    PIC X(19).
00083
00084          16  RE-COMP-INFO-END              PIC X(6).
00085          16  RE-NSP-ST-CD-LF               PIC XX.
00086          16  RE-NSP-ST-CD-AH               PIC XX.
00087          16  RE-TABLE-CARRIER-SECURITY     PIC X.
00088              88  NO-TABLE-CARRIER-SECURITY    VALUE SPACE.
00089
00090          16  FILLER                        PIC X(27).
00091
00092      12  RE-COMPANY-DATA   REDEFINES   RE-TABLE-DATA.
00093          16  RE-NAME                       PIC X(30).
00094          16  RE-LF-PE                      PIC X.
00095          16  RE-AH-PE                      PIC X.
00096          16  RE-LF-FEE                     PIC S9V9999    COMP-3.
00097          16  RE-AH-FEE                     PIC S9V9999    COMP-3.
00098          16  RE-AH-PR-PCT                  PIC S9V9999    COMP-3.
00099          16  RE-AH-78-PCT                  PIC S9V9999    COMP-3.
00100          16  RE-PRT-ST                     PIC X.
00101          16  RE-PRT-OW                     PIC X.
00102          16  RE-MORT-CODE                  PIC X(4).
00103          16  RE-CLAIM-CODE                 PIC X.
00104          16  RE-ZERO-LF-FEE                PIC X.
00105          16  RE-ZERO-AH-FEE                PIC X.
00106          16  RE-CEDE-NAME                  PIC X(30).
00107          16  RE-LF-COMM                    PIC X.
00108          16  RE-AH-COMM                    PIC X.
00109          16  RE-LF-TAX                     PIC X.
00110          16  RE-AH-TAX                     PIC X.
00111          16  RE-CLM-INCURRED-LIM           PIC 9(11)  COMP-3.
00116          16  RE-LF-IBNR-PCT                PIC SV999      COMP-3.
00117          16  RE-AH-IBNR-PCT                PIC SV999      COMP-3.
00118
00119          16  RE-COMP-CARRIER-SECURITY      PIC X.
00120              88  NO-COMP-CARRIER-SECURITY     VALUE SPACE.
00121
00122          16  RE-LF-CEDING-FEE-BRACKETS.
00123              20  RE-LF-FEE-METHOD          PIC X.
00124                  88  RE-LF-FEE-BRACKETED         VALUE '1' '2'.
00125                  88  RE-LF-FEE-METHOD-1          VALUE '1'.
00126                  88  RE-LF-FEE-METHOD-2          VALUE '2'.
00127                  88  RE-LF-FEE-PERCENT           VALUE ' ' 'P'.
00128              20  RE-LF-FEE-BASIS           PIC X.
00129                  88  RE-LF-GROSS-CEDED             VALUE '1'.
00130                  88  RE-LF-NET-CEDED               VALUE '2'.
00131                  88  RE-LF-GROSS-WRITTEN           VALUE '3'.
00132                  88  RE-LF-NET-WRITTEN             VALUE '4'.
00133                  88  RE-LF-COMBINE-GROSS-CEDED     VALUE '5'.
00134                  88  RE-LF-COMBINE-NET-CEDED       VALUE '6'.
00135                  88  RE-LF-COMBINE-GROSS-WRITTEN   VALUE '7'.
00136                  88  RE-LF-COMBINE-NET-WRITTEN     VALUE '8'.
00137              20  FILLER                    PIC XXX.
00138              20  RE-LF-FEE-RANGES  OCCURS 6 TIMES.
00139                  24  RE-LF-FEE-RANGE-PCT   PIC S9V9999    COMP-3.
00140                  24  RE-LF-FEE-THRU-AMT    PIC S9(7)V99   COMP-3.
00141
00142          16  RE-AH-CEDING-FEE-BRACKETS.
00143              20  RE-AH-FEE-METHOD          PIC X.
00144                  88  RE-AH-FEE-BRACKETED         VALUE '1' '2'.
00145                  88  RE-AH-FEE-METHOD-1          VALUE '1'.
00146                  88  RE-AH-FEE-METHOD-2          VALUE '2'.
00147                  88  RE-AH-FEE-PERCENT           VALUE ' ' 'P'.
00148              20  RE-AH-FEE-BASIS           PIC X.
00149                  88  RE-AH-GROSS-CEDED             VALUE '1'.
00150                  88  RE-AH-NET-CEDED               VALUE '2'.
00151                  88  RE-AH-GROSS-WRITTEN           VALUE '3'.
00152                  88  RE-AH-NET-WRITTEN             VALUE '4'.
00153                  88  RE-AH-COMBINE-GROSS-CEDED     VALUE '5'.
00154                  88  RE-AH-COMBINE-NET-CEDED       VALUE '6'.
00155                  88  RE-AH-COMBINE-GROSS-WRITTEN   VALUE '7'.
00156                  88  RE-AH-COMBINE-NET-WRITTEN     VALUE '8'.
00157              20  FILLER                    PIC XXX.
00158              20  RE-AH-FEE-RANGES  OCCURS 6 TIMES.
00159                  24  RE-AH-FEE-RANGE-PCT   PIC S9V9999    COMP-3.
00160                  24  RE-AH-FEE-THRU-AMT    PIC S9(7)V99   COMP-3.
00161
00162          16  RE-EARNING-START-DT           PIC 9(11)  COMP-3.
00166
00167          16  RE-OLD-CEDING-STMT            PIC X.
00168
00169          16  RE-LF-CLM-PCT                 PIC S9V9999    COMP-3.
00170          16  RE-AH-CLM-PCT                 PIC S9V9999    COMP-3.
00171          16  RE-LF-CLM-MAX                 PIC S9(7)V99   COMP-3.
00172          16  RE-AH-CLM-MAX                 PIC S9(7)V99   COMP-3.
00173          16  RE-LF-PR-PCT                  PIC S9V9999    COMP-3.
00174          16  RE-LF-78-PCT                  PIC S9V9999    COMP-3.
00175          16  RE-REINS-GROUPING-CODE        PIC X(6).
00176          16  RE-MORT-SW                    PIC X.
00177          16  RE-CEDING-TYPE-FLAG           PIC X.
00178              88  RE-NO-CESSION-TYPE                VALUE ' '.
00179              88  RE-CEDED                          VALUE 'C'.
00180              88  RE-ASSUMED                        VALUE 'A'.
00181              88  RE-PHANTOM                        VALUE 'P'.
00182
00183          16  RE-CEDING-STMT-OPT-A          PIC X.
00184              88  REPORT-A-WANTED    VALUE ' ' 'Y'.
00185          16  RE-CEDING-STMT-OPT-B          PIC X.
00186              88  REPORT-B-WANTED    VALUE ' ' 'Y'.
00187          16  RE-CEDING-STMT-OPT-C          PIC X.
00188              88  REPORT-C-WANTED    VALUE ' ' 'Y'.
00189          16  RE-CEDING-STMT-OPT-D          PIC X.
00190              88  REPORT-D-WANTED    VALUE ' ' 'Y'.
00191          16  RE-CEDING-STMT-OPT-E          PIC X.
00192              88  REPORT-E-WANTED    VALUE ' ' 'Y'.
00193
00194          16  RE-PRT-CRSV                   PIC X.
00195
00196          16  RE-GL-CENTER                  PIC X(4).
00197
00198          16  RE-CUSTODIAL-BAL              PIC S9(7)V99   COMP-3.
00199
00200          16  RE-EARNING-STOP-DT            PIC 9(11)  COMP-3.
00204
00205          16  RE-EARN-STOP-CODE             PIC X.
00206              88  STOP-LIFE-EARNING  VALUE 'L' 'B'.
00207              88  STOP-AH-EARNING    VALUE 'A' 'B'.
00208
103101         16  RE-STATE-EXHIBIT-OPT-F        PIC X.
103101             88  RPTF-ECS152-WANTED VALUE ' ' 'Y'.
103101
032707         16  RE-EXCISE-TAX                 PIC S9V9999 COMP-3.
032707         16  FILLER                        PIC X(2281).
00210
00211          16  RE-DESC OCCURS 18 TIMES       PIC X(79).
00212
00213 ******************************************************************
00123  EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA CONTROL-FILE
                                REINSURANCE-RECORD.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'ELREIN' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00125
00126      MOVE DFHCOMMAREA            TO  REINSURANCE-PASS-AREA.
00127
00128  0000-CALCULATE-REIN-TEST.
00129      MOVE SPACES                 TO  CERTIFICATE-RECORD.
00130      MOVE CP-COMPANY-ID          TO  DTE-CLIENT.
00131      MOVE CP-ISSUE-AGE           TO  CR-AGE.
00132      MOVE CP-ORIGINAL-TERM       TO  CR-LF-TERM
00133                                      CR-AH-TERM.
00134      MOVE CP-TBCOD               TO  CR-REIN-TABLE
00135                                      REIN-SRCH.
00136
00137      MOVE CP-EFF-DT              TO  DC-BIN-DATE-1.
00138      MOVE SPACES                 TO  DC-OPTION-CODE.
00139      PERFORM 8510-DATE-RTN  THRU 8510-DATE-RTN-EXIT.
00140      MOVE DC-GREG-DATE-CYMD      TO  CR-DT.
00141
00142      MOVE CP-LF-TYPE             TO  CR-LFTYP.
00143      MOVE CP-AH-TYPE             TO  CR-AHTYP.
00144      MOVE CP-LF-BEN              TO  CR-LFAMT  RS-R-LB.
00145      MOVE CP-LF-PREM             TO  CR-LFPRM  CR-LFPRM-CALC
00146                                      RS-R-LP   RS-R-LPC.
00147      MOVE CP-LFPRM-ALT           TO  CR-LFPRM-ALT.
00148      MOVE CP-LFAMT-ALT           TO  CR-LFAMT-ALT.
00149      MOVE CP-AH-BEN              TO  CR-AHAMT  RS-R-AB.
00150      MOVE CP-AH-PREM             TO  CR-AHPRM  CR-AHPRM-CALC
00151                                      RS-R-AP   RS-R-APC.
00152      MOVE CP-LF-REF              TO  CR-LFRFND CR-LFRFND-CALC.
00153      MOVE CP-AH-REF              TO  CR-AHRFND CR-AHRFND-CALC.
00154
00155      IF CP-IG-CODE = 'G'
00156          MOVE '2'                TO  CR-IND-GRP
00157        ELSE
00158          MOVE '1'                TO  CR-IND-GRP.
00159
00160      MOVE HIGH-VALUES            TO  REIN-HOLD-AREAS.
00161      MOVE SPACES                 TO  REIN-LEVELS-END.
00162
00163      PERFORM CLEAR-REIN-HOLD THRU CLEAR-REIN-HOLD-X.
00164
00165      PERFORM 6000-REINSURE-ROUTINE THRU 6000-EXIT.
00166      GO TO 8200-RETURN.
00167
00168  6000-REINSURE-ROUTINE.
00169      MOVE 1                      TO  CLAS-INDEXL.
00170      MOVE 2                      TO  CLAS-INDEXA.
00171
00172      IF CR-LFTYP = ZEROS
00173         GO TO 0310-FIND-AH-BENEFIT-RECORD.
00174
00175      MOVE CP-COMPANY-ID          TO  ENTL-COMP-ID.
00176      MOVE '4'                    TO  ENTL-REC-TYPE.
00177      MOVE +0                     TO  ENTL-SEQ-NO.
00178      MOVE CR-LFTYP               TO  WS-BEN-CD.
00179      MOVE WS-ACCESS              TO  ENTL-ACCESS.
00180
00181      PERFORM 7200-FIND-BENEFIT THRU 7200-EXIT.
00182
00183      IF NO-BENEFIT-FOUND
00184          GO TO 0305-LF-BENEFIT-NOT-FOUND.
00185
00186      MOVE CF-LF-COVERAGE-TYPE  (SUB3) TO  CLAS-I-RL-AH (1).
00187      MOVE CF-SPECIAL-CALC-CD   (SUB3) TO  CLAS-I-BAL (1).
00188      MOVE CF-JOINT-INDICATOR   (SUB3) TO  CLAS-I-JOINT (1).
00189
00190      IF CLAS-I-BAL (1) = 'O'
00191          MOVE 'B' TO CLAS-I-BAL (1).
00192
00193      MOVE CF-BENEFIT-COMMENT (SUB3)   TO  CLAS-I-AB10 (1).
00194
00195      GO TO 0310-FIND-AH-BENEFIT-RECORD.
00196
00197  0305-LF-BENEFIT-NOT-FOUND.
00198
00199      MOVE '1'                    TO  CP-RETURN-CODE.
00200      GO TO 6000-EXIT.
00201
00202  0306-AH-BENEFIT-NOT-FOUND.
00203
00204      MOVE '2'                    TO  CP-RETURN-CODE.
00205      GO TO 6000-EXIT.
00206
00207  EJECT
00208  0310-FIND-AH-BENEFIT-RECORD.
00209
00210      IF CR-AHTYP = ZEROS
00211         GO TO 0400-READ-REIN.
00212
00213      MOVE CP-COMPANY-ID          TO  ENTL-COMP-ID.
00214      MOVE '5'                    TO  ENTL-REC-TYPE.
00215      MOVE +0                     TO  ENTL-SEQ-NO.
00216      MOVE CR-AHTYP               TO  WS-BEN-CD.
00217      MOVE WS-ACCESS              TO  ENTL-ACCESS.
00218
00219      PERFORM 7200-FIND-BENEFIT THRU 7200-EXIT.
00220
00221      IF NO-BENEFIT-FOUND
00222          GO TO 0306-AH-BENEFIT-NOT-FOUND.
00223
00224      MOVE CF-LF-COVERAGE-TYPE  (SUB3)    TO  CLAS-I-RL-AH (2).
00225      MOVE CF-SPECIAL-CALC-CD   (SUB3)    TO  CLAS-I-BAL (2).
00226      MOVE CF-JOINT-INDICATOR   (SUB3)    TO  CLAS-I-JOINT (2).
00227
00228      IF CLAS-I-BAL (2) = 'O'
00229          MOVE 'B'                TO  CLAS-I-BAL (2).
00230
00231      MOVE CF-BENEFIT-COMMENT (SUB3)      TO  CLAS-I-AB10 (2).
00232
00233  0400-READ-REIN.
00234
00235      MOVE CP-COMPANY-CD          TO  REIN-COMP-CD.
00236      MOVE 'A'                    TO  REIN-TYPE.
00237      MOVE CR-REIN-TABLE          TO  REIN-TABLE-CO.
00238      MOVE LOW-VALUES             TO  REIN-TABLE-SUB.
00239
00240      PERFORM 9200-READ-REIN-TABLE THRU 9200-EXIT.
00241
00242      PERFORM REINSURE-CALC THRU REINSURE-CALC-X
00243              VARYING SUB1 FROM +1 BY +1 UNTIL
00244              RE-REI-COMP (SUB1) = SPACES.
00245
00246  6000-EXIT.
00247      EXIT.
00248
00249  EJECT
00250  7200-FIND-BENEFIT.
00251
00252      MOVE 'N'                    TO  BEN-SEARCH-SW.
00253
00254      
      * EXEC CICS HANDLE CONDITION
00255 *        ENDFILE   (7200-EXIT)
00256 *        NOTFND    (7200-EXIT)
00257 *        END-EXEC.
      *    MOVE '"$''I                  ! " #00002878' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303032383738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00258
00259      
      * EXEC CICS READ
00260 *        DATASET     ('ELCNTL')
00261 *        SET         (ADDRESS OF CONTROL-FILE)
00262 *        RIDFLD      (ELCNTL-KEY)
00263 *        GTEQ
00264 *        END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S        G          (   #00002883' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303032383833' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00265
00266      IF ENTL-COMP-ID  NOT = CF-COMPANY-ID   OR
00267         ENTL-REC-TYPE NOT = CF-RECORD-TYPE
00268            GO TO 7200-EXIT.
00269
00270      MOVE +1                     TO SUB3.
00271
00272  7200-BENEFIT-LOOP.
00273
00274      IF SUB3 GREATER THAN +8
00275          GO TO 7200-EXIT.
00276
00277      IF CF-BENEFIT-CODE (SUB3) EQUAL WS-BEN-CD
00278          MOVE 'Y'                TO BEN-SEARCH-SW
00279              GO TO 7200-EXIT.
00280
00281      ADD +1                      TO SUB3.
00282      GO TO 7200-BENEFIT-LOOP.
00283
00284  7200-EXIT.
00285      EXIT.
00286
00287  EJECT
00288  8000-CALC-REIN.
00289 *                                COPY ECSRIRT2.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ECSRIRT2                            *
00004 *                            VMOD=2.016                          *
00005 *                                                                *
00006 ******************************************************************
00007  REINSURE-CALC.
00008      MOVE RE-REI-COMP-NO (SUB1)  TO  REIN-COMP (SUB1).
00009      MOVE REIN-WORK-FLDS (SUB1)  TO  RWF-FIELDS.
00010      MOVE RE-COMP-INFO (SUB1)    TO  WT-COMP-INFO.
00011      MOVE WT-REMAINING           TO  RWF-REM-SW
00012                                      REIN-REM-SW (SUB1).
00013
00014      IF WT-REMAINING = 'Y' OR 'Z' OR 'L' OR 'R'
00015          MOVE RS-R-LB            TO  RS-LIFE-BEN
00016          MOVE RS-R-LP            TO  RS-LIFE-PREM
00017          MOVE RS-R-AB            TO  RS-AH-BEN
00018          MOVE RS-R-AP            TO  RS-AH-PREM
00019          MOVE RS-R-LPC           TO  RS-LP-CALC
00020          MOVE RS-R-APC           TO  RS-AP-CALC
00021      ELSE
00022          MOVE CR-LFAMT           TO  RS-LIFE-BEN
00023          MOVE CR-LFPRM           TO  RS-LIFE-PREM
00024          MOVE CR-AHAMT           TO  RS-AH-BEN
00025          MOVE CR-AHPRM           TO  RS-AH-PREM
00026          MOVE CR-LFPRM-CALC      TO  RS-LP-CALC
00027                                      REIN-CALCED-LIFE
00028          MOVE CR-AHPRM-CALC      TO  RS-AP-CALC
00029                                      REIN-CALCED-AH.
00030
00031      IF SUB1 = RE-100-COMP
00032          GO TO REINSURE-CALC-REMAINDER.
00033
00034      IF CR-DT LESS WT-LO-DATE
00035          GO TO REINSURE-CALC-X.
00036
00037      IF CR-DT NOT LESS WT-HI-DATE
00038          GO TO REINSURE-CALC-X.
00039
00040      PERFORM REINSURE-CK-LIFE      THRU REINSURE-CK-LIFE-X.
00041      PERFORM REINSURE-CK-AH        THRU REINSURE-CK-AH-X.
00042      IF WT-INTERACTIVE = 'E'
00043          PERFORM REIN-CHECK-INTERACT-E THRU REIN-INTERACT-CHECK-X.
00044
00045      IF WT-INTERACTIVE = 'X'
00046        IF  (REIN-LF-FLG (SUB1) = 'X' AND REIN-AH-FLG (SUB1) = 'X')
00047         OR (REIN-LF-FLG (SUB1) = 'X' AND REIN-AH-FLG (SUB1) = ' ')
00048         OR (REIN-LF-FLG (SUB1) = ' ' AND REIN-AH-FLG (SUB1) = 'X')
00049            NEXT SENTENCE
00050        ELSE
00051            MOVE 'Y'              TO  REIN-LF-FLG (SUB1)
00052                                      REIN-AH-FLG (SUB1).
00053
00054      IF WT-INTERACTIVE = 'Y'
00055          IF REIN-LF-FLG (SUB1) = 'X' AND REIN-AH-FLG (SUB1) = 'Y'
00056              MOVE 'X'            TO  REIN-AH-FLG (SUB1).
00057
00058      IF WT-INTERACTIVE = 'Y'
00059          IF REIN-AH-FLG (SUB1) = 'X' AND REIN-LF-FLG (SUB1) = 'Y'
00060              MOVE 'X'            TO  REIN-LF-FLG (SUB1).
00061
00062      IF WT-INTERACTIVE = 'Z'
00063          IF REIN-LF-FLG (SUB1) = 'X'
00064              IF REIN-AH-FLG (SUB1) = 'Y'
00065                  MOVE 'X'        TO  REIN-AH-FLG (SUB1)
00066              ELSE
00067                  NEXT SENTENCE
00068          ELSE
00069              MOVE 'Y'            TO  REIN-LF-FLG (SUB1)
00070                                      REIN-AH-FLG (SUB1).
00071
00072      IF WT-INTERACTIVE = 'W'
00073          IF REIN-AH-FLG (SUB1) = 'X'
00074              IF REIN-LF-FLG (SUB1) = 'Y'
00075                  MOVE 'X'        TO  REIN-LF-FLG (SUB1)
00076              ELSE
00077                  NEXT SENTENCE
00078          ELSE
00079              MOVE 'Y'            TO  REIN-LF-FLG (SUB1)
00080                                      REIN-AH-FLG (SUB1).
00081
00082      IF REIN-LF-FLG (SUB1) = 'X'
00083          PERFORM REIN-CALC-LIFE THRU REIN-CALC-LIFE-X.
00084
00085      IF REIN-LF-FLG (SUB1) = 'E'
00086          PERFORM REIN-CALC-A THRU REIN-CALC-LIFE-X
00087          MOVE 'X'                TO  REIN-LF-FLG (SUB1).
00088
00089      IF REIN-AH-FLG (SUB1) = 'X'
00090          PERFORM REIN-CALC-AH THRU REIN-CALC-AH-X.
00091
00092      IF REIN-AH-FLG (SUB1) = 'E'
00093          PERFORM REIN-AH-SKIP-LIMIT THRU REIN-CALC-AH-X
00094          MOVE 'X'                TO  REIN-AH-FLG (SUB1).
00095
00096      IF REIN-LF-FLG (SUB1) NOT = 'X'
00097          MOVE SPACE              TO  REIN-LF-FLG (SUB1).
00098
00099      IF REIN-AH-FLG (SUB1) NOT = 'X'
00100          MOVE SPACE              TO  REIN-AH-FLG (SUB1).
00101
00102      GO TO REINSURE-CALC-X.
00103
00104  REINSURE-CK-LIFE.
00105      IF CR-LFTYP = ZERO
00106          GO TO REINSURE-CK-LIFE-X.
00107
00108      IF WT-LF-PCT = ZERO
00109          GO TO REINSURE-CK-LIFE-X.
00110
00111      IF DTE-CLIENT = 'CSO' OR 'CSI'
00112          IF CLAS-I-REIN-YN (CLAS-INDEXL) = 'N'
00113              GO TO REINSURE-CK-LIFE-X.
00114
00115      IF WT-LF-BEN-CODE = '2'
00116          IF CLAS-I-BAL (CLAS-INDEXL) NOT = '2'
00117             GO TO REINSURE-CK-LIFE-X
00118          ELSE
00119              NEXT SENTENCE
00120      ELSE
00121          IF CLAS-I-BAL (CLAS-INDEXL) = '2'
00122             GO TO REINSURE-CK-LIFE-X.
00123
00124      IF WT-LF-BEN-CODE = '4'
00125          IF CLAS-I-BAL (CLAS-INDEXL) NOT = '4'
00126             GO TO REINSURE-CK-LIFE-X
00127          ELSE
00128              NEXT SENTENCE
00129      ELSE
00130          IF CLAS-I-BAL (CLAS-INDEXL) = '4'
00131             GO TO REINSURE-CK-LIFE-X.
00132
00133      IF WT-LF-BEN-CODE = 'A'
00134      AND
00135         CLAS-I-BAL (CLAS-INDEXL) = 'B'
00136             GO TO REINSURE-CK-LIFE-X.
00137
00138      IF WT-LF-BEN-CODE = 'B'
00139      AND
00140         CLAS-I-BAL (CLAS-INDEXL) NOT = 'B'
00141             GO TO REINSURE-CK-LIFE-X.
00142
00143      IF WT-LF-BEN-CODE = 'C'
00144      AND
00145         CLAS-I-RL-AH (CLAS-INDEXL) = 'L' OR 'P'
00146             GO TO REINSURE-CK-LIFE-X.
00147
00148      IF WT-LF-BEN-CODE = 'D'
00149      AND
00150         CLAS-I-RL-AH (CLAS-INDEXL) NOT = 'L' AND 'P'
00151             GO TO REINSURE-CK-LIFE-X.
00152
00153      IF WT-LF-BEN-CODE = 'E'
00154      AND
00155         CLAS-I-RL-AH (CLAS-INDEXL) = 'R'
00156             GO TO REINSURE-CK-LIFE-X.
00157
00158      IF WT-LF-BEN-CODE = 'F'
00159      AND
00160         CLAS-I-RL-AH (CLAS-INDEXL) NOT = 'R'
00161             GO TO REINSURE-CK-LIFE-X.
00162
00163      IF WT-LF-BEN-CODE = 'G'
00164      AND
00165         CR-IND-GRP NOT = '2'
00166             GO TO REINSURE-CK-LIFE-X.
00167
00168      IF WT-LF-BEN-CODE = 'H'
00169          IF (CR-IND-GRP = '1')  OR
00170             (CLAS-I-RL-AH (CLAS-INDEXL) = 'L' OR 'P') OR
00171             (CR-AHTYP NOT = ZERO)
00172                 GO TO REINSURE-CK-LIFE-X.
00173
00174      IF WT-LF-BEN-CODE = 'I'
00175      AND
00176         CR-IND-GRP NOT = '1'
00177             GO TO REINSURE-CK-LIFE-X.
00178
00179      IF WT-LF-BEN-CODE = 'J'
00180      AND
00181         CLAS-I-JOINT (CLAS-INDEXL) NOT = 'J'
00182             GO TO REINSURE-CK-LIFE-X.
00183
00184      IF WT-LF-BEN-CODE = 'K'
00185         IF (CLAS-I-RL-AH (CLAS-INDEXL) NOT = 'R')
00186           OR
00187             ((CLAS-I-RL-AH (CLAS-INDEXL) = 'R')
00188              AND
00189              (CLAS-I-BAL (CLAS-INDEXL) = 'B'))
00190                 GO TO REINSURE-CK-LIFE-X.
00191
00192      IF WT-LF-BEN-CODE = 'L'
00193          IF CR-IND-GRP = '1'  OR
00194             CLAS-I-BAL (CLAS-INDEXL) = 'B'
00195                 GO TO REINSURE-CK-LIFE-X.
00196
00197      IF WT-LF-BEN-CODE = 'M'
00198          IF CR-IND-GRP = '1'  OR
00199             CLAS-I-RL-AH (CLAS-INDEXL) = 'R'
00200                 GO TO REINSURE-CK-LIFE-X.
00201
00202      IF WT-LF-BEN-CODE = 'N'
00203          IF CR-IND-GRP = '1'  OR
00204             (CLAS-I-RL-AH (CLAS-INDEXL) = 'L' OR 'P')  OR
00205             CLAS-I-BAL (CLAS-INDEXL) = 'B'
00206                 GO TO REINSURE-CK-LIFE-X.
00207
00208      IF WT-LF-BEN-CODE = 'P'
00209          IF CR-IND-GRP = '1'  OR
00210             CLAS-I-RL-AH (CLAS-INDEXL) = 'R'  OR
00211             CLAS-I-BAL (CLAS-INDEXL) = 'B'
00212                 GO TO REINSURE-CK-LIFE-X.
00213
00214      IF WT-LF-BEN-CODE = 'R'
00215          IF (CR-IND-GRP = '1')  OR
00216             (CLAS-I-RL-AH (CLAS-INDEXL) = 'L' OR 'P')
00217                 GO TO REINSURE-CK-LIFE-X.
00218
00219      IF WT-LF-BEN-CODE = 'S'
00220      AND
00221         CLAS-I-JOINT (CLAS-INDEXL) = 'J'
00222             GO TO REINSURE-CK-LIFE-X.
00223
00224      IF DTE-CLIENT EQUAL 'NCL'
00225         MOVE 'Y'                 TO  REIN-LF-FLG (SUB1).
00226
00227      IF CR-AGE LESS WT-LFAGE-LO
00228          GO TO REINSURE-CK-LIFE-X.
00229
00230      IF CR-AGE GREATER WT-LFAGE-HI
00231          GO TO REINSURE-CK-LIFE-X.
00232
00233      IF CR-LF-TERM LESS WT-LFTRM-LO
00234          GO TO REINSURE-CK-LIFE-X.
00235
00236      IF CR-LF-TERM GREATER WT-LFTRM-HI
00237          GO TO REINSURE-CK-LIFE-X.
00238
00239      MOVE 'Y'                    TO  REIN-LF-FLG (SUB1).
00240
00241      IF RS-LIFE-BEN NOT GREATER WT-LF-LIM-LO
00242          GO TO REINSURE-CK-LIFE-X.
00243
00244      IF RS-LIFE-BEN GREATER WT-LF-LIM-HI
00245          GO TO REINSURE-CK-LIFE-X.
00246
00247      MOVE 'X'                    TO  REIN-LF-FLG (SUB1).
00248
00249  REINSURE-CK-LIFE-X.
00250      EXIT.
00251
00252  REINSURE-CK-AH.
00253      IF CR-AHTYP = ZERO
00254          GO TO REINSURE-CK-AH-X.
00255
00256      IF WT-AH-PCT = ZERO
00257          GO TO REINSURE-CK-AH-X.
00258
00259      IF DTE-CLIENT = 'CSO' OR 'CSI'
00260          IF CLAS-I-REIN-YN (CLAS-INDEXA) = 'N'
00261              GO TO REINSURE-CK-AH-X.
00262
00263      IF WT-AH-BEN-CODE = '3'
00264          IF CLAS-I-BAL (CLAS-INDEXA) NOT = '3'
00265             GO TO REINSURE-CK-AH-X
00266          ELSE
00267              NEXT SENTENCE
00268      ELSE
00269          IF CLAS-I-BAL (CLAS-INDEXA) = '3'
00270             GO TO REINSURE-CK-AH-X.
00271
00272      IF WT-AH-BEN-CODE = 'S'
00273      AND
00274         CLAS-I-JOINT (CLAS-INDEXA) = 'J'
00275             GO TO REINSURE-CK-AH-X.
00276
00277      IF WT-AH-BEN-CODE = 'J'
00278      AND
00279         CLAS-I-JOINT (CLAS-INDEXA) NOT = 'J'
00280             GO TO REINSURE-CK-AH-X.
00281
00282      IF WT-AH-BEN-CODE = 'A'
00283      AND
00284         CLAS-I-BAL (CLAS-INDEXA) = 'B'
00285             GO TO REINSURE-CK-AH-X.
00286
00287      IF WT-AH-BEN-CODE = 'B'
00288      AND
00289         CLAS-I-BAL (CLAS-INDEXA) NOT = 'B'
00290             GO TO REINSURE-CK-AH-X.
00291
00292      IF WT-AH-BEN-CODE = 'G'
00293      AND
00294         CR-IND-GRP NOT = '2'
00295             GO TO REINSURE-CK-AH-X.
00296
00297      IF WT-AH-BEN-CODE = 'I'
00298      AND
00299         CR-IND-GRP NOT = '1'
00300             GO TO REINSURE-CK-AH-X.
00301
00302      IF WT-AH-BEN-CODE = 'L'
00303          IF CR-IND-GRP = '1'  OR
00304             CLAS-I-BAL (CLAS-INDEXA) = 'B'
00305                 GO TO REINSURE-CK-AH-X.
00306
00307      IF WT-AH-BEN-CODE = '7'
00308      AND
00309         CLAS-I-AB3 (CLAS-INDEXA) NOT = (' 7R'  AND  '07R')
00310             GO TO REINSURE-CK-AH-X.
00311
00312      IF WT-AH-BEN-CODE = '8'
00313      AND
00314         CLAS-I-AB3 (CLAS-INDEXA) NOT = '14R'
00315             GO TO REINSURE-CK-AH-X.
00316
00317      IF DTE-CLIENT EQUAL 'NCL'
00318         MOVE 'Y'                 TO  REIN-AH-FLG (SUB1).
00319
00320      IF CR-AGE LESS WT-AHAGE-LO
00321          GO TO REINSURE-CK-AH-X.
00322
00323      IF CR-AGE GREATER WT-AHAGE-HI
00324          GO TO REINSURE-CK-AH-X.
00325
00326      IF CR-AH-TERM LESS WT-AHTRM-LO
00327          GO TO REINSURE-CK-AH-X.
00328
00329      IF CR-AH-TERM GREATER WT-AHTRM-HI
00330          GO TO REINSURE-CK-AH-X.
00331
00332      MOVE 'Y'                    TO  REIN-AH-FLG (SUB1).
00333
00334      IF WT-AHBEN-LIM-LO = ZERO
00335          AND WT-AHBEN-LIM-HI = ZERO
00336              MOVE +9999999.99    TO  WT-AHBEN-LIM-LO
00337              MOVE +9999999.99    TO  WT-AHBEN-LIM-HI.
00338
00339      IF WT-AHMOA-LIM-LO = ZERO
00340          AND WT-AHMOA-LIM-HI = ZERO
00341              MOVE +9999999.99    TO  WT-AHMOA-LIM-LO
00342              MOVE +9999999.99    TO  WT-AHMOA-LIM-HI.
00343
00344      COMPUTE RW-AHLIM-LO = CR-AH-TERM * WT-AHMOA-LIM-LO.
00345      COMPUTE RW-AHLIM-HI = CR-AH-TERM * WT-AHMOA-LIM-HI.
00346
00347      IF RW-AHLIM-LO GREATER WT-AHBEN-LIM-LO
00348          MOVE WT-AHBEN-LIM-LO    TO  RW-AHLIM-LO.
00349
00350      IF RW-AHLIM-HI GREATER WT-AHBEN-LIM-HI
00351          MOVE WT-AHBEN-LIM-HI    TO  RW-AHLIM-HI.
00352
00353      COMPUTE RW-AHWRK = RS-AH-BEN * CR-AH-TERM.
00354      MOVE RW-AHLIM-LO            TO  RWF-AH-LIMIT.
00355
00356      IF RW-AHWRK NOT GREATER RW-AHLIM-LO
00357          GO TO REINSURE-CK-AH-X.
00358
00359      IF RW-AHWRK GREATER RW-AHLIM-HI
00360          GO TO REINSURE-CK-AH-X.
00361
00362      MOVE 'X'                    TO  REIN-AH-FLG (SUB1).
00363
00364  REINSURE-CK-AH-X.
00365      EXIT.
00366
00367  REIN-CHECK-INTERACT-E.
00368
00369      IF RS-LIFE-BEN = ZERO  OR
00370         RS-AH-BEN = ZERO
00371          GO TO REIN-INTERACT-CHECK-X.
00372
00373      IF REIN-LF-FLG (SUB1) NOT = 'X'  AND
00374         REIN-AH-FLG (SUB1) NOT = 'X'
00375          GO TO REIN-INTERACT-CHECK-X.
00376
00377      MOVE 'X'                    TO REIN-LF-FLG (SUB1)
00378                                     REIN-AH-FLG (SUB1).
00379
00380      IF WT-AHBEN-LO = ZERO
00381          AND WT-AHBEN-HI = ZERO
00382              MOVE +9999999.99    TO  WT-AHBEN-LO
00383                                      WT-AHBEN-HI.
00384      IF WT-AHMOA-LO = ZERO
00385          AND WT-AHMOA-HI = ZERO
00386              MOVE +9999999.99    TO  WT-AHMOA-LO
00387                                      WT-AHMOA-HI.
00388
00389      COMPUTE RW-AHLIM-LO = CR-AH-TERM * WT-AHMOA-LO.
00390      COMPUTE RW-AHLIM-HI = CR-AH-TERM * WT-AHMOA-HI.
00391
00392      IF RW-AHLIM-LO GREATER WT-AHBEN-LO
00393          MOVE WT-AHBEN-LO        TO  RW-AHLIM-LO.
00394
00395      IF RW-AHLIM-HI GREATER WT-AHBEN-HI
00396          MOVE WT-AHBEN-HI        TO  RW-AHLIM-HI.
00397
00398      COMPUTE RW-AHWRK = RS-AH-BEN * CR-AH-TERM.
00399
00400      IF RW-AHWRK GREATER RW-AHLIM-HI
00401          MOVE RW-AHLIM-HI        TO  RW-AHWRK.
00402
00403      IF RW-AHWRK GREATER RW-AHLIM-LO
00404          COMPUTE RATE-WORK-A ROUNDED = (RW-AHWRK - RW-AHLIM-LO)
00405                                        / RW-AHWRK
00406        ELSE
00407          MOVE ZERO               TO RATE-WORK-A.
00408
00409      IF RS-LIFE-BEN GREATER WT-LF-LO
00410          COMPUTE RATE-WORK-L ROUNDED = (RS-LIFE-BEN - WT-LF-LO)
00411                                        / RS-LIFE-BEN
00412        ELSE
00413          MOVE ZERO               TO RATE-WORK-L.
00414
00415      IF (RS-LIFE-BEN GREATER WT-LF-LO) AND
00416         (RW-AHWRK NOT GREATER RW-AHLIM-LO)
00417           MOVE 'E'               TO REIN-AH-FLG (SUB1)
00418           COMPUTE RWF-AHAMT ROUNDED = (RW-AHWRK * RATE-WORK-L)
00419                                       / CR-AH-TERM.
00420
00421      IF (RW-AHWRK GREATER RW-AHLIM-LO) AND
00422         (RS-LIFE-BEN NOT GREATER WT-LF-LO)
00423           MOVE 'E'               TO REIN-LF-FLG (SUB1)
00424           IF (RW-AHWRK - RW-AHLIM-LO) NOT GREATER RS-LIFE-BEN
00425                COMPUTE RWF-LFAMT = RW-AHWRK - RW-AHLIM-LO
00426             ELSE
00427                 MOVE RS-LIFE-BEN TO RWF-LFAMT.
00428
00429      IF (RW-AHWRK GREATER RW-AHLIM-LO) AND
00430         (RS-LIFE-BEN GREATER WT-LF-LO)
00431         IF RATE-WORK-L GREATER RATE-WORK-A
00432           MOVE 'E'               TO REIN-AH-FLG (SUB1)
00433           COMPUTE RWF-AHAMT ROUNDED = (RW-AHWRK * RATE-WORK-L)
00434                                       / CR-AH-TERM
00435         ELSE
00436           MOVE 'E'               TO REIN-LF-FLG (SUB1)
00437           IF (RW-AHWRK - RW-AHLIM-LO) NOT GREATER RS-LIFE-BEN
00438                COMPUTE RWF-LFAMT = RW-AHWRK - RW-AHLIM-LO
00439             ELSE
00440                 MOVE RS-LIFE-BEN TO RWF-LFAMT.
00441
00442  REIN-INTERACT-CHECK-X.
00443      EXIT.
00444
00445
00446  REIN-CALC-LIFE.
00447
00448      IF RS-LIFE-BEN NOT GREATER WT-LF-HI
00449          MOVE RS-LIFE-BEN        TO  RWF-LFAMT
00450      ELSE
00451          MOVE WT-LF-HI           TO  RWF-LFAMT.
00452
00453      IF RS-LIFE-BEN LESS WT-LF-LO
00454          MOVE +0                 TO  REIN-WRK-2
00455          MOVE +0                 TO  RWF-LFAMT
00456      ELSE
00457          COMPUTE REIN-WRK-2 = RWF-LFAMT - WT-LF-LO
00458          IF WT-REMAINING NOT = 'L'
00459             IF RS-LIFE-BEN LESS THAN ZERO
00460                COMPUTE RWF-LFAMT ROUNDED = WT-LF-PCT * RS-LIFE-BEN
00461             ELSE
00462                COMPUTE RWF-LFAMT ROUNDED = WT-LF-PCT *
00463                                           (RWF-LFAMT - WT-LF-LO).
00464
00465  REIN-CALC-A.
00466      MOVE +0                     TO  RW-LFPRM  RW-LFPRMC.
00467
00468      IF WT-REMAINING = 'L'
00469          COMPUTE RWF-LFPRM ROUNDED = RS-LIFE-PREM * WT-LF-PCT
00470          GO TO REIN-CALC-LIFE-X.
00471
00472      IF RS-LIFE-BEN NOT = ZERO
00473          COMPUTE RW-LFPRM ROUNDED = (RS-LIFE-PREM *
00474                    RWF-LFAMT) / RS-LIFE-BEN
00475          COMPUTE RW-LFPRMC ROUNDED = (RS-LP-CALC *
00476                    RWF-LFAMT) / RS-LIFE-BEN
00477      ELSE
00478          IF WT-LF-LIM-LO = ZERO
00479              COMPUTE RW-LFPRM ROUNDED = RS-LIFE-PREM * WT-LF-PCT
00480              COMPUTE RW-LFPRMC ROUNDED = RS-LP-CALC * WT-LF-PCT.
00481
00482      IF ((WT-LF-QC = '1' OR '3')  OR
00483          (WT-LF-QC = '2' AND
00484           PGM-SUB = +50  AND
00485           DTE-CLIENT = 'LAP'))
00486          MOVE RW-LFPRM           TO  RWF-LFPRM
00487      ELSE
00488          MOVE RW-LFPRMC          TO  RWF-LFPRM.
00489
00490      IF WT-REMAINING = 'Y' OR 'Z' OR 'X' OR 'N'
00491          COMPUTE RS-R-LB = RS-R-LB - RWF-LFAMT
00492          COMPUTE RS-R-LP = RS-R-LP - RW-LFPRM
00493          COMPUTE RS-R-LPC = RS-R-LPC - RW-LFPRMC.
00494
00495  REIN-CALC-LIFE-X.
00496      EXIT.
00497
00498  REIN-CALC-AH.
00499
00500      IF WT-AHBEN-LO = ZERO
00501          AND WT-AHBEN-HI = ZERO
00502              MOVE +9999999.99    TO  WT-AHBEN-LO
00503                                      WT-AHBEN-HI.
00504      IF WT-AHMOA-LO = ZERO
00505          AND WT-AHMOA-HI = ZERO
00506              MOVE +9999999.99    TO  WT-AHMOA-LO
00507                                      WT-AHMOA-HI.
00508
00509      COMPUTE RW-AHLIM-LO = CR-AH-TERM * WT-AHMOA-LO.
00510      COMPUTE RW-AHLIM-HI = CR-AH-TERM * WT-AHMOA-HI.
00511
00512      IF RW-AHLIM-LO GREATER WT-AHBEN-LO
00513          MOVE WT-AHBEN-LO        TO  RW-AHLIM-LO.
00514
00515      IF RW-AHLIM-HI GREATER WT-AHBEN-HI
00516          MOVE WT-AHBEN-HI        TO  RW-AHLIM-HI.
00517
00518      IF RW-AHWRK GREATER RW-AHLIM-HI
00519          MOVE RW-AHLIM-HI        TO  RW-AHWRK.
00520
00521      IF RW-AHWRK LESS RW-AHLIM-LO
00522          MOVE +0                 TO  RWF-AHAMT
00523      ELSE
00524          IF WT-REMAINING = 'L'
00525              COMPUTE RWF-AHAMT ROUNDED = RW-AHWRK / CR-AH-TERM
00526          ELSE
00527              COMPUTE RWF-AHAMT ROUNDED =
00528              ((RW-AHWRK - RW-AHLIM-LO) / CR-AH-TERM) * WT-AH-PCT.
00529
00530  REIN-AH-SKIP-LIMIT.
00531      IF WT-REMAINING = 'L'
00532          COMPUTE RWF-AHPRM ROUNDED = RS-AH-PREM * WT-AH-PCT
00533          GO TO REIN-CALC-AH-X.
00534
00535      IF RS-AH-BEN NOT = ZERO
00536          COMPUTE RW-AHPRM ROUNDED = (RS-AH-PREM *
00537                    RWF-AHAMT) / RS-AH-BEN
00538          COMPUTE RW-AHPRMC ROUNDED = (RS-AP-CALC *
00539                    RWF-AHAMT) / RS-AH-BEN
00540      ELSE
00541          IF RW-AHLIM-LO = ZERO
00542              COMPUTE RW-AHPRM ROUNDED = RS-AH-PREM * WT-AH-PCT
00543              COMPUTE RW-AHPRMC ROUNDED = RS-AP-CALC * WT-AH-PCT.
00544
00545      IF ((WT-AH-QC = '1' OR '3')  OR
00546          (WT-AH-QC = '2' AND
00547           PGM-SUB = +50  AND
00548           DTE-CLIENT = 'LAP'))
00549          MOVE RW-AHPRM           TO  RWF-AHPRM
00550      ELSE
00551          MOVE RW-AHPRMC          TO  RWF-AHPRM.
00552
00553      IF RE-NSP-ST-CD-AH = SPACES OR ZERO
00554          IF (WT-REMAINING = 'Y' OR 'Z') AND (WT-AH-QC = '1')
00555             AND (RWF-AHAMT = RS-R-AB) AND (RWF-AHAMT NOT = +0)
00556                 MOVE 'Y'         TO  RWF-REM-AH-100.
00557
00558      IF WT-REMAINING = 'Y' OR 'Z' OR 'X' OR 'N'
00559          COMPUTE RS-R-AB = RS-R-AB - RWF-AHAMT
00560          COMPUTE RS-R-AP = RS-R-AP - RW-AHPRM
00561          COMPUTE RS-R-APC = RS-R-APC - RW-AHPRMC.
00562
00563  REIN-CALC-AH-X.
00564      EXIT.
00565
00566  REINSURE-CALC-REMAINDER.
00567
00568      COMPUTE RWF-LFAMT = CR-LFAMT + CR-LFAMT-ALT.
00569      COMPUTE RWF-LFPRM = CR-LFPRM + CR-LFPRM-ALT.
00570      MOVE CR-AHAMT               TO  RWF-AHAMT.
00571      MOVE CR-AHPRM               TO  RWF-AHPRM.
00572
00573      PERFORM REINSURE-SUBTRACT THRU REINSURE-SUBTRACT-X
00574          VARYING SUB2 FROM +1 BY +1 UNTIL SUB2 = SUB1.
00575
00576      IF RWF-LFAMT NOT = ZEROS
00577      OR
00578        RWF-LFPRM NOT = ZEROS
00579            MOVE 'X'              TO  REIN-LF-FLG (SUB1).
00580
00581      IF RWF-AHAMT NOT = ZEROS
00582      OR
00583        RWF-AHPRM NOT = ZEROS
00584            MOVE 'X'              TO  REIN-AH-FLG (SUB1).
00585
00586  REINSURE-CALC-X.
00587      IF REIN-LF-AH-FLGS (SUB1) NOT = SPACES
00588          MOVE 'X'                TO  REIN-BUS-FLAG.
00589
00590      MOVE RWF-FIELDS             TO  REIN-WORK-FLDS (SUB1).
00591
00592  REINSURE-SUBTRACT.
00593      SUBTRACT REIN-LFAMT (SUB2) FROM RWF-LFAMT.
00594      SUBTRACT REIN-LFPRM (SUB2) FROM RWF-LFPRM.
00595      SUBTRACT REIN-AHAMT (SUB2) FROM RWF-AHAMT.
00596      SUBTRACT REIN-AHPRM (SUB2) FROM RWF-AHPRM.
00597
00598  REINSURE-SUBTRACT-X.
00599      EXIT.
00600
00601
00602  CLEAR-REIN-HOLD.
00603      MOVE SPACES                 TO  REIN-COMP (1).
00604      MOVE SPACES                 TO  REIN-LF-AH-FLGS (1).
00605      MOVE SPACES                 TO  REIN-REM-SW (1).
00606      MOVE SPACES                 TO  REIN-REM-AH-100 (1).
00607
00608      MOVE +0 TO REIN-LFAMT (1)  REIN-LFPRM (1)  REIN-AHAMT (1)
00609          REIN-AHPRM (1)     REIN-LFRFND (1)     REIN-AHRFND (1)
00610          REIN-LFCLM (1)     REIN-AHCLM (1)      REIN-LFCLML (1)
00611          REIN-AHCLML (1)    REIN-DIS-IBNR (1)  REIN-DIS-PAYCUR (1)
00612          REIN-DIS-FUTRSV (1)   REIN-DTH-IBNR (1)
00613          REIN-DTH-PAYCUR (1)   REIN-DTH-FUTRSV (1)
00614          REIN-DIS-IBNRL (1)    REIN-DIS-PAYCURL (1)
00615          REIN-DIS-FUTRSVL (1)  REIN-DTH-IBNRL (1)
00616          REIN-DTH-PAYCURL (1)  REIN-DTH-FUTRSVL (1)
00617          REIN-AH-LIMIT (1).
00618
00619      PERFORM CLEAR-REIN-HOLD-ALL VARYING SUB1 FROM 2 BY 1
00620          UNTIL REIN-COMP (SUB1) = SPACES.
00621
00622      GO TO CLEAR-REIN-HOLD-X.
00623
00624  CLEAR-REIN-HOLD-ALL.
00625      MOVE REIN-LEVELS (1)        TO  REIN-LEVELS (SUB1).
00626
00627  CLEAR-REIN-HOLD-X.
00628      EXIT.
00629
00630 ******************************************************************
00290
00291  EJECT
00292  8200-RETURN.
00293      MOVE REINSURANCE-PASS-AREA  TO  DFHCOMMAREA.
00294
00295      
      * EXEC CICS RETURN
00296 *        END-EXEC.
      *    MOVE '.(                    &   #00003549' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303033353439' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00297       
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'ELREIN' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
00298      EJECT
00299  8500-DATE-CONVERSION SECTION.
00300
00301 *    NOTE *******************************************************
00302 *         *                                                     *
00303 *         *  THIS SECTION CALLS THE DATE CONVERSION SUBROUTINE. *
00304 *         *                                                     *
00305 *         *******************************************************.
00306
00307  8510-DATE-RTN.
00308      
      * EXEC CICS LINK
00309 *        PROGRAM  (LINK-ELDATCV)
00310 *        COMMAREA (DATE-CONVERSION-DATA)
00311 *        LENGTH   (DC-COMM-LENGTH)
00312 *    END-EXEC.
      *    MOVE '."C                   ''   #00003562' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033353632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LINK-ELDATCV, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00313
00314      IF DC-ERROR-CODE NOT = SPACES
00315          MOVE '2'                TO  CP-RETURN-CODE.
00316
00317  8510-DATE-RTN-EXIT.
00318      EXIT.
00319
00320  EJECT
00321  9200-READ-REIN-TABLE.
00322      
      * EXEC CICS HANDLE CONDITION
00323 *        ENDFILE   (9250-INVALID-TABLE)
00324 *        NOTFND    (9250-INVALID-TABLE)
00325 *        END-EXEC.
      *    MOVE '"$''I                  ! # #00003576' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303033353736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00326
00327      
      * EXEC CICS READ
00328 *        DATASET     ('ERREIN')
00329 *        SET         (ADDRESS OF REINSURANCE-RECORD)
00330 *        RIDFLD      (ERREIN-KEY)
00331 *        END-EXEC.
           MOVE 'ERREIN' TO DFHEIV1
      *    MOVE '&"S        E          (   #00003581' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033353831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERREIN-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF REINSURANCE-RECORD TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00332
00333  9200-EXIT.
00334       EXIT.
00335
00336  9250-INVALID-TABLE.
00337      MOVE '3'                    TO  CP-RETURN-CODE.
00338
00339      GO TO 6000-EXIT.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'ELREIN' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 7200-EXIT,
                     7200-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 9250-INVALID-TABLE,
                     9250-INVALID-TABLE
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'ELREIN' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
