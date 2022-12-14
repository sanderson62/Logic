00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL1602.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 05/16/95 15:31:54.
00007 *                            VMOD=2.007.
00008 *
00008 *
00009 *AUTHOR.        LOGIC, INC.
00010 *               DALLAS, TEXAS.
00011
00024 *REMARKS. TRANSACTION EX34 - CLAIM DISPLAY.
121802******************************************************************
121802*                   C H A N G E   L O G
121802*
121802* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
121802*-----------------------------------------------------------------
121802*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
121802* EFFECTIVE    NUMBER
121802*-----------------------------------------------------------------
121802* 121802    2001061800003  SMVA  ADD PROCESSING FOR NEW CLM TYPE I
030612* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
052614* 052614    2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
080322* 080322  CR2021100800003  TANA  Add B and H claim types
121802******************************************************************
00025      EJECT
00026  ENVIRONMENT DIVISION.
00027  DATA DIVISION.
00028  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00029  77  FILLER  PIC X(32)  VALUE '********************************'.
00030  77  FILLER  PIC X(32)  VALUE '*   EL1602 WORKING STORAGE     *'.
00031  77  FILLER  PIC X(32)  VALUE '********** VMOD=2.007 **********'.
00032
00033  01  LCP-TIME-OF-DAY-XX.
00034      05  LCP-TIME-OF-DAY-68        PIC 9(6).
00035      05  FILLER                    PIC 99.
00036  01  LCP-CICS-TIME                 PIC 9(15).
00037
00038  01  WS-TS-AREA.
00039      12  FILLER                  PIC X(145).
00040      12  WS-TS-CLAIM             PIC X(7).
00041      12  FILLER                  PIC X(3).
00042      12  WS-TS-TYPE              PIC X.
00043      12  FILLER                  PIC X(3).
00044      12  WS-TS-CERT              PIC X(10).
00045      12  FILLER                  PIC X(3).
00046      12  WS-TS-CERT-SFX          PIC X.
00047      12  FILLER                  PIC X(3).
00048      12  WS-TS-CARR              PIC X.
00049      12  FILLER                  PIC X(3).
00050      12  WS-TS-STATUS            PIC X.
00051      12  FILLER                  PIC X(10).
00052      12  WS-TS-FILE              PIC X(4).
00053      12  FILLER                  PIC X(3).
00054      12  WS-TS-CCN               PIC X(16).
00055      12  FILLER                  PIC X(3).
00056      12  WS-TS-LNAME             PIC X(15).
00057      12  FILLER                  PIC X(3).
00058      12  WS-TS-FNAME             PIC X(15).
00059      12  FILLER                  PIC X(3).
00060      12  WS-TS-MINIT             PIC X.
00061      12  FILLER                  PIC X(608).
00062
00063      EJECT
00064  01  WS-HEADING1.
00065      12  FILLER                  PIC X(24)   VALUE SPACES.
00066      12  WS-H1-TITLE             PIC X(18)   VALUE
00067          'CLAIM AUDIT REPORT'.
00068      12  FILLER                  PIC X(12)   VALUE SPACES.
00069      12  WS-H1-REPORT-NUMBER     PIC X(7)    VALUE 'EL -160'.
00070      12  FILLER                  PIC X(71)   VALUE SPACES.
00071
00072  01  WS-HEADING2.
00073      12  FILLER                  PIC X(5)    VALUE SPACES.
00074      12  FILLER                  PIC X(50)   VALUE
00075          'CAR CLAIM     CERT      TYPE   STATUS   FILE  NAME'.
00076      12  FILLER                  PIC X(77)   VALUE SPACES.
00077
00078  01  WS-DETAIL1.
00079      12  FILLER                  PIC X(6)    VALUE SPACES.
00080      12  WS-D1-CARR              PIC X       VALUE SPACES.
00081      12  FILLER                  PIC X       VALUE SPACES.
00082      12  WS-D1-CLAIM             PIC X(7)    VALUE SPACES.
00083      12  FILLER                  PIC X       VALUE SPACES.
00084      12  WS-D1-CERT.
00085          16  WS-D1-CERT-PRIME    PIC X(10)   VALUE SPACES.
00086          16  WS-D1-CERT-SFX      PIC X       VALUE SPACES.
00087      12  FILLER                  PIC X       VALUE SPACES.
00088      12  WS-D1-TYPE              PIC X(6)    VALUE SPACES.
00089      12  FILLER                  PIC XX      VALUE SPACES.
00090      12  WS-D1-STATUS            PIC X(6)    VALUE SPACES.
00091      12  FILLER                  PIC X(3)    VALUE SPACES.
00092      12  WS-D1-FILE              PIC X(4)   VALUE SPACES.
00093      12  FILLER                  PIC XX      VALUE SPACES.
00094      12  WS-D1-NAME              PIC X(30)   VALUE SPACES.
00095      12  FILLER                  PIC X(54)   VALUE SPACES.
00096
00097      EJECT
00098 *                                COPY ELCREPT.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCREPT.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.002                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = REPORT STORAGE                            *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 146   RECFORM = FIX                            *
00011 *                                                                *
00012 *   BASE CLUSTER NAME = ELREPT                 RKP=2,LEN=11      *
00013 *       ALTERNATE PATH  = NOT USED                               *
00014 *                                                                *
00015 *   LOG = NO                                                     *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00017 ******************************************************************
00018  01  REPORT-SAVE-FILE.
00019      12  RF-RECORD-ID                PIC XX.
00020          88  VALID-RF-ID                VALUE 'RF'.
00021
00022      12  RF-CONTROL-PRIMARY.
00023          16  RF-COMPANY-CD           PIC X.
00024          16  RF-RECORD-TYPE          PIC X.
00025              88  REPORT-DETAIL-RECORD   VALUE '1'.
00026              88  REPORT-TRAILER-RECORD  VALUE '2'.
00027          16  RF-REPORT-ID.
00028              20  RF-SYSTEM-CODE      PIC XX.
00029                  88  CLAS-IC-ONLINE     VALUE 'EL'.
00030                  88  CLAS-SCS-BATCH     VALUE 'EC'.
00031              20  RF-PROGRAM-SEQUENCE PIC 999.
00032          16  RF-LINE-NUMBER          PIC S9(8)       COMP.
00033      12  RF-REPORT-LINE-133.
00034          16  RF-CTL-CHAR-133         PIC X.
00035          16  RF-DATA-133             PIC X(132).
00036          16  RF-DATA-FIRST  REDEFINES RF-DATA-133.
00037              20  RF-DATA-2-81        PIC X(80).
00038              20  FILLER              PIC X(52).
00039          16  RF-DATA-LAST   REDEFINES RF-DATA-133.
00040              20  FILLER              PIC X(53).
00041              20  RF-DATA-55-133      PIC X(79).
00042      12  RF-TRAILER-RECORD  REDEFINES RF-REPORT-LINE-133.
00043          16  FILLER                  PIC X.
00044          16  RF-CURRENT-DATE         PIC X(8).
00045          16  RF-PRINT-HH-MM-SS       PIC X(6).
00046          16  FILLER                  PIC X(115).
00047          16  RF-COMPANY-ID           PIC XXX.
00048 ******************************************************************
00099      EJECT
00100  01  WS-DATE-AREA.
00101      12  SAVE-DATE               PIC X(8)   VALUE SPACES.
00102      12  SAVE-BIN-DATE           PIC XX     VALUE SPACES.
00103
00104  01  LITERALS-NUMBERS.
00105      12  LIT-SP                  PIC XX      VALUE 'SP'.
00106      12  LIT-OB                  PIC XX      VALUE 'OB'.
00107      12  LIT-OE                  PIC XX      VALUE 'OE'.
00108      12  LIT-TYPE-L              PIC X(4)    VALUE 'LIFE'.
00109      12  LIT-TYPE-A              PIC X(4)    VALUE 'A/H'.
00110      12  LIT-CLOSED              PIC X(6)    VALUE 'CLOSED'.
00111      12  LIT-OPEN                PIC X(6)    VALUE ' OPEN'.
00112      12  LIT-SET-UP              PIC X(6)    VALUE 'SET UP'.
00113      12  LIT-PMT                 PIC X(6)    VALUE 'PAYMNT'.
00114      12  LIT-LETTER              PIC X(6)    VALUE 'LETTER'.
00115      12  LIT-UPDATE              PIC X(6)    VALUE 'UPDATE'.
00116      12  LIT-RESTORE             PIC X(6)    VALUE 'RESTOR'.
00117      12  LIT-INC-CHG             PIC X(6)    VALUE 'INC DT'.
00118      12  LIT-CONV                PIC X(6)    VALUE ' CONV'.
00119      12  LIT-SIGN-OFF            PIC X(8)    VALUE 'EL005'.
00120      12  LIT-HELP                PIC X(8)    VALUE 'EL010'.
00121      12  LIT-MASTER              PIC X(8)    VALUE 'EL126'.
00122      12  LIT-ACT                 PIC X(8)    VALUE 'EL142'.
00123      12  LIT-PROG                PIC X(8)    VALUE 'EL1602'.
00124      12  DATE-CONV               PIC X(8)    VALUE 'ELDATCV'.
00125      12  LIT-TRAN                PIC X(4)    VALUE 'EX34'.
00126      12  LIT-MAP                 PIC X(4)    VALUE '160B'.
00127      12  LIT-SCREEN              PIC X(4)    VALUE '160C'.
00128      12  NUM-ONE                 PIC 99      VALUE 1.
00129      12  NUM-TWENTY-FOUR         PIC 99      VALUE 24.
00130      12  START-TRANS-ID          PIC X(4)    VALUE 'EX58'.
00131      12  FILE-SWITCH             PIC X(4)    VALUE SPACES.
00132      12  WS-PRINT-SW             PIC S9      VALUE +0.
00133          88  PRINT-IN-PROCESS                VALUE +1.
00134      12  WS-FIRST-TIME-SW        PIC XX      VALUE LOW-VALUES.
00135          88  FIRST-TIME-THRU                 VALUE LOW-VALUES.
00136      12  REPT-FILE-ID            PIC X(8)   VALUE 'ELREPT  '.
00137      12  GETMAIN-SPACE           PIC X      VALUE SPACE.
00138      12  MAX-TS-PAGES            PIC 9999    VALUE 251.
00139      12  W-FILE-ID               PIC X(8)    VALUE 'ELMSTR'.
00140
00141  01  FILLER          COMP-3.
00142      12  WS-LINE-COUNT           PIC S9(3)       VALUE +99.
00143      12  WS-RECORD-COUNT         PIC S9(9)       VALUE ZERO.
00144      12  WS-LINE-NUMBER          PIC S9(4)       VALUE +0.
00145
00146      EJECT
00147  01  EDIT-WORK-AREA.
00148      12  CALL-PGM                PIC X(8).
00149      12  TRANS-ID                PIC X(4).
00150      12  CHECK-PFKEYS            PIC 99.
00151      12  CONV-COUNT              PIC 9(4).
00152      12  EDIT-COUNT              PIC ZZZ9.
00153      12  PI-KEY.
00154          16  CLAS-TERM           PIC X(4).
00155          16  CLAS-QUAL           PIC X(4).
00156      12  HOLD-PROC               PIC X(4).
00157      12  HOLD-PRI                PIC X.
00158      12  HOLD-SUPV               PIC X.
00159      12  HOLD-FILE               PIC X(4).
00160      12  DAYS-PAID               PIC ZZZZ9.
00161      12  PMTS-MADE               PIC ZZZZZ9.
00162      12  EDIT-DOLLARS-9          PIC ZZZZZZ.99.
00163
00164  01  WS-OLD-CLAIM-RECORD         PIC X(350).
00165
00166  01  CNTL-KEY.
00167      12  COMPANY-ID              PIC X(3).
00168      12  RECORD-TYPE             PIC X.
00169      12  CNTL-PROC               PIC X(4).
00170      12  SEQ-NO                  PIC 9(4)    COMP.
00171
00172  01  MSTR-KEY.
00173      12  MSTR-COMPANY-CODE       PIC X.
00174      12  MSTR-CARRIER            PIC X.
00175      12  MSTR-CLAIM-NO           PIC X(7).
00176      12  MSTR-CERT-NO            PIC X(11).
00177
00178  01  ELACTQ-KEY.
00179      12  ACTQ-COMP-CD            PIC X.
00180      12  ACTQ-CARRIER            PIC X.
00181      12  ACTQ-CLAIM-NO           PIC X(7).
00182      12  ACTQ-CERT-NO            PIC X(11).
00183
00184  01  TIME-UNFORMATTED.
00185      12  UN-HOURS                PIC XX.
00186      12  UN-MINUTES              PIC XX.
00187      12  FILLER                  PIC X(4).
00188
00189  01  TIME-FORMATTED.
00190      12  FOR-HOURS               PIC XX.
00191      12  FILLER                  PIC X       VALUE '.'.
00192      12  FOR-MINUTES             PIC XX.
00193      EJECT
00194  01  ERROR-NUMBERS.
00195      12  ER-0000                 PIC X(4)    VALUE '0000'.
00196      12  ER-0008                 PIC X(4)    VALUE '0008'.
00197      12  ER-0029                 PIC X(4)    VALUE '0029'.
00198      12  ER-0042                 PIC X(4)    VALUE '0042'.
00199      12  ER-0068                 PIC X(4)    VALUE '0068'.
00200      12  ER-0070                 PIC X(4)    VALUE '0070'.
00201      12  ER-0190                 PIC X(4)    VALUE '0190'.
00202      12  ER-0609                 PIC X(4)    VALUE '0609'.
00203      12  ER-0130                 PIC X(4)    VALUE '0130'.
00204      12  ER-0131                 PIC X(4)    VALUE '0131'.
00205      12  ER-0192                 PIC X(4)    VALUE '0192'.
00206      12  ER-0230                 PIC X(4)    VALUE '0230'.
00207      12  ER-0273                 PIC X(4)    VALUE '0273'.
00208      12  ER-0274                 PIC X(4)    VALUE '0274'.
00209      12  ER-0276                 PIC X(4)    VALUE '0276'.
00210      12  ER-0337                 PIC X(4)    VALUE '0337'.
00211      12  ER-0412                 PIC X(4)    VALUE '0412'.
00212      12  ER-0413                 PIC X(4)    VALUE '0413'.
00213      12  ER-0515                 PIC X(4)    VALUE '0515'.
00214      12  ER-0971                 PIC X(4)    VALUE '0971'.
00215      12  ER-0972                 PIC X(4)    VALUE '0972'.
00216      12  ER-2379                 PIC X(4)    VALUE '2379'.
00217
00218  01  ERROR-SWITCHES.
00219      12  ERROR-SWITCH            PIC X.
00220          88  SCREEN-ERROR                    VALUE 'X'.
00221      12  UPDATE-SWITCH           PIC X.
00222          88  NO-UPDATES                      VALUE SPACE.
00223      12  KEY-SWITCH              PIC X.
00224          88  KEY-CHANGE                      VALUE 'X'.
00225      12  WS-SAVE-PRINT-OPTION    PIC X       VALUE SPACE.
00226
00227  01  COMP-LENGTHS.
00228      12  LIT-IC                  PIC S9(4)   COMP VALUE -1.
00229      12  EL1602-LENGTH           PIC S9(4)   COMP VALUE +881.
00230      12  MSTR-LENGTH             PIC S9(4)   COMP VALUE +350.
00231      12  WS-SAVE-TS-COUNT        PIC S9(4)   COMP VALUE +0.
00232      12  WS-TS-ITEM-NO           PIC S9(4)   COMP VALUE +0.
00233      EJECT
00234 *                                COPY ELCATTR.
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
00235      EJECT
00236 *                                COPY ELCDATE.
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
00237      EJECT
00238 *                                COPY ELCLOGOF.
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
00239      EJECT
00240 *                                COPY ELCMSTR.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCMSTR.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.012                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = CLAIM MASTER FILE                         *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 350  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELMSTR                         RKP=2,LEN=20   *
00013 *       ALTERNATE PATH1 = ELMSTR2 (BY NAME)       RKP=22,LEN=29  *
00014 *       ALTERNATE PATH2 = ELMSTR3 (BY SOC SEC NO) RKP=51,LEN=12  *
00015 *       ALTERNATE PATH3 = ELMSTR5 (BY CERT NO)    RKP=63,LEN=12  *
00016 *       ALTERNATE PATH4 = ELMSTR6 (BY CREDIT CARD NO)            *
00017 *                                                 RKP=75,LEN=21  *
00018 *                                                                *
00019 *   **** NOTE ****                                               *
00020 *             ANY CHANGES TO THIS COPYBOOK MUST ALSO BE          *
00021 *             IMPLEMENTED IN COPYBOOK ELCRETR (RETRIEVE MASTER)  *
00022 *                                                                *
00023 *   LOG = YES                                                    *
00024 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
120503******************************************************************
120503*                   C H A N G E   L O G
120503*
120503* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
120503*-----------------------------------------------------------------
120503*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
120503* EFFECTIVE    NUMBER
120503*-----------------------------------------------------------------
120503* 120503    2003080800002  SMVA  INITIAL SECURE PAY CHANGES
080307* 080307    2007032100001  PEMA  ADD TOTAL INTEREST PAID FIELD
031213* 031213    2012113000002  PEMA  ADD ACCIDENT INDICATOR
051414* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
052614* 052614    2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
081817* 081817    2016100700001  TANA  ADD NBR OF EXTENSIONS
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
022122* 022122  CR2021100800003  PEMA  Add B and H claim types
00025 ******************************************************************
00026  01  CLAIM-MASTER.
00027      12  CL-RECORD-ID                PIC XX.
00028          88  VALID-CL-ID         VALUE 'CL'.
00029
00030      12  CL-CONTROL-PRIMARY.
00031          16  CL-COMPANY-CD           PIC X.
00032          16  CL-CARRIER              PIC X.
00033          16  CL-CLAIM-NO             PIC X(7).
00034          16  CL-CERT-NO.
00035              20  CL-CERT-PRIME       PIC X(10).
00036              20  CL-CERT-SFX         PIC X.
00037
00038      12  CL-CONTROL-BY-NAME.
00039          16  CL-COMPANY-CD-A1        PIC X.
00040          16  CL-INSURED-LAST-NAME    PIC X(15).
00041          16  CL-INSURED-NAME.
00042              20  CL-INSURED-1ST-NAME PIC X(12).
00043              20  CL-INSURED-MID-INIT PIC X.
00044
00045      12  CL-CONTROL-BY-SSN.
00046          16  CL-COMPANY-CD-A2        PIC X.
00047          16  CL-SOC-SEC-NO.
00048              20  CL-SSN-STATE        PIC XX.
00049              20  CL-SSN-ACCOUNT      PIC X(6).
00050              20  CL-SSN-LN3          PIC X(3).
00051
00052      12  CL-CONTROL-BY-CERT-NO.
00053          16  CL-COMPANY-CD-A4        PIC X.
00054          16  CL-CERT-NO-A4.
00055              20  CL-CERT-A4-PRIME    PIC X(10).
00056              20  CL-CERT-A4-SFX      PIC X.
00057
00058      12  CL-CONTROL-BY-CCN.
00059          16  CL-COMPANY-CD-A5        PIC X.
00060          16  CL-CCN-A5.
00061              20  CL-CCN.
00062                  24  CL-CCN-PREFIX-A5 PIC X(4).
00063                  24  CL-CCN-PRIME-A5 PIC X(12).
00064              20  CL-CCN-FILLER-A5    PIC X(4).
00065
00066      12  CL-INSURED-PROFILE-DATA.
00067          16  CL-INSURED-BIRTH-DT     PIC XX.
00068          16  CL-INSURED-SEX-CD       PIC X.
00069              88  INSURED-IS-MALE        VALUE 'M'.
00070              88  INSURED-IS-FEMALE      VALUE 'F'.
00071              88  INSURED-SEX-UNKNOWN    VALUE ' '.
00072          16  CL-INSURED-OCC-CD       PIC X(6).
00073          16  FILLER                  PIC X(5).
00074
00075      12  CL-PROCESSING-INFO.
00076          16  CL-PROCESSOR-ID         PIC X(4).
00077          16  CL-CLAIM-STATUS         PIC X.
00078              88  CLAIM-IS-OPEN          VALUE 'O'.
00079              88  CLAIM-IS-CLOSED        VALUE 'C'.
00080          16  CL-CLAIM-TYPE           PIC X.
00081 *            88  AH-CLAIM               VALUE 'A'.
00082 *            88  LIFE-CLAIM             VALUE 'L'.
00083 *            88  PROPERTY-CLAIM         VALUE 'P'.
00084 *            88  IUI-CLAIM              VALUE 'I'.
120503*            88  GAP-CLAIM              VALUE 'G'.
052614*            88  FAMILY-LEAVE-CLAIM     VALUE 'F'.
100518*            88  OTHER-CLAIM            VALUE 'O'.
022122*            88  hospital-claim         value 'H'.
022122*            88  bereavement-claim      value 'B'.
00085          16  CL-CLAIM-PREM-TYPE      PIC X.
00086              88  SINGLE-PREMIUM         VALUE '1'.
00087              88  O-B-COVERAGE           VALUE '2'.
00088              88  OPEN-END-COVERAGE      VALUE '3'.
00089          16  CL-INCURRED-DT          PIC XX.
00090          16  CL-REPORTED-DT          PIC XX.
00091          16  CL-FILE-ESTABLISH-DT    PIC XX.
00092          16  CL-EST-END-OF-DISAB-DT  PIC XX.
00093          16  CL-LAST-PMT-DT          PIC XX.
00094          16  CL-LAST-PMT-AMT         PIC S9(7)V99  COMP-3.
00095          16  CL-PAID-THRU-DT         PIC XX.
00096          16  CL-TOTAL-PAID-AMT       PIC S9(7)V99  COMP-3.
00097          16  CL-NO-OF-PMTS-MADE      PIC S9(3)     COMP-3.
00098          16  CL-NO-OF-DAYS-PAID      PIC S9(4)     COMP.
00099          16  CL-PMT-CALC-METHOD      PIC X.
00100              88  CL-360-DAY-YR          VALUE '1'.
00101              88  CL-365-DAY-YR          VALUE '2'.
00102              88  CL-FULL-MONTHS         VALUE '3'.
00103          16  CL-CAUSE-CD             PIC X(6).
00104
00105          16  CL-PRIME-CERT-NO.
00106              20  CL-PRIME-CERT-PRIME PIC X(10).
00107              20  CL-PRIME-CERT-SFX   PIC X.
00108
00109          16  CL-SYSTEM-IDENTIFIER    PIC XX.
00110              88  CL-CREDIT-CLAIM        VALUE 'CR'.
00111              88  CL-CONVENIENCE-CLAIM   VALUE 'CV'.
00112
00113          16  CL-MICROFILM-NO         PIC X(10).
051414         16  FILLER REDEFINES CL-MICROFILM-NO.
051414             20  CL-BENEFIT-PERIOD   PIC 99.
051414             20  FILLER              PIC X(8).
00114          16  CL-PROG-FORM-TYPE       PIC X.
00115          16  CL-LAST-ADD-ON-DT       PIC XX.
00116
00117          16  CL-LAST-REOPEN-DT       PIC XX.
00118          16  CL-LAST-CLOSE-DT        PIC XX.
00119          16  CL-LAST-CLOSE-REASON    PIC X(01).
00120              88  FINAL-PAID             VALUE '1'.
00121              88  CLAIM-DENIED           VALUE '2'.
00122              88  AUTO-CLOSE             VALUE '3'.
00123              88  MANUAL-CLOSE           VALUE '4'.
00124              88  BENEFITS-CHANGED       VALUE 'C'.
00125              88  SETUP-ERRORS           VALUE 'E'.
00126          16  CL-ASSOC-CERT-SEQU      PIC S99.
00127          16  CL-ASSOC-CERT-TOTAL     PIC S99.
00128          16  CL-CLAIM-PAYMENT-STATUS PIC 9.
00129              88  PAYMENT-IN-PREP        VALUE 1 THRU 9.
080307         16  CL-TOTAL-INT-PAID       PIC S9(5)V99 COMP-3.
080307         16  FILLER                  PIC X.
00131
00132      12  CL-CERTIFICATE-DATA.
00133          16  CL-CERT-ORIGIN          PIC X.
00134              88  CERT-WAS-ONLINE        VALUE '1'.
00135              88  CERT-WAS-CREATED       VALUE '2'.
00136              88  COVERAGE-WAS-ADDED     VALUE '3'.
00137          16  CL-CERT-KEY-DATA.
00138              20  CL-CERT-CARRIER     PIC X.
00139              20  CL-CERT-GROUPING    PIC X(6).
00140              20  CL-CERT-STATE       PIC XX.
00141              20  CL-CERT-ACCOUNT.
00142                  24  CL-CERT-ACCOUNT-PREFIX PIC X(4).
00143                  24  CL-CERT-ACCOUNT-PRIME  PIC X(6).
00144              20  CL-CERT-EFF-DT      PIC XX.
00145
00146      12  CL-STATUS-CONTROLS.
00147          16  CL-PRIORITY-CD          PIC X.
00148              88  CONFIDENTIAL-DATA      VALUE '8'.
00149              88  HIGHEST-PRIORITY       VALUE '9'.
00150          16  CL-SUPV-ATTN-CD         PIC X.
00151              88  SUPV-NOT-REQUIRED      VALUE ' ' 'N'.
00152              88  SUPV-IS-REQUIRED       VALUE 'Y'.
00153          16  CL-PURGED-DT            PIC XX.
00154          16  CL-RESTORED-DT          PIC XX.
00155          16  CL-NEXT-AUTO-PAY-DT     PIC XX.
00156          16  CL-NEXT-RESEND-DT       PIC XX.
00157          16  CL-NEXT-FOLLOWUP-DT     PIC XX.
031213         16  CL-CRITICAL-PERIOD      PIC 99.
031213*        16  FILLER                  PIC XX.
00159          16  CL-LAST-MAINT-DT        PIC XX.
00160          16  CL-LAST-MAINT-USER      PIC X(4).
00161          16  CL-LAST-MAINT-HHMMSS    PIC S9(6)     COMP-3.
00162          16  CL-LAST-MAINT-TYPE      PIC X.
00163              88  CLAIM-SET-UP           VALUE ' '.
00164              88  PAYMENT-MADE           VALUE '1'.
00165              88  LETTER-SENT            VALUE '2'.
00166              88  MASTER-WAS-ALTERED     VALUE '3'.
00167              88  MASTER-WAS-RESTORED    VALUE '4'.
00168              88  INCURRED-DATE-CHANGED  VALUE '5'.
00169              88  FILE-CONVERTED         VALUE '6'.
00170              88  CHANGE-OF-BENEFITS     VALUE 'C'.
00171              88  ERROR-CORRECTION       VALUE 'E'.
00172          16  CL-RELATED-CLAIM-NO     PIC X(7).
00173          16  CL-HISTORY-ARCHIVE-DT   PIC XX.
00174          16  CL-BENEFICIARY          PIC X(10).
00175          16  CL-FILE-ESTABLISHED-BY  PIC X(4).
120808         16  CL-DENIAL-TYPE          PIC X.
                   88  CL-TYPE-DENIAL          VALUE '1'.
                   88  CL-TYPE-RESCISSION      VALUE '2'.
                   88  CL-TYPE-REFORMATION     VALUE '3'.
                   88  CL-TYPE-REF-TO-RES      VALUE '4'.
                   88  CL-TYPE-RECONSIDERED    VALUE '5'.
081817         16  CL-NO-OF-EXTENSIONS     PIC 99.
081817         16  filler                  pic x(3).
      *        16  CL-CRIT-PER-RECURRENT   PIC X.
      *        16  CL-CRIT-PER-RTW-MOS     PIC 99.
      *        16  CL-RTW-DT               PIC XX.
00177
00178      12  CL-TRAILER-CONTROLS.
00179          16  CL-TRAILER-SEQ-CNT      PIC S9(4)     COMP.
00180              88  CL-1ST-TRL-AVAIL       VALUE +4095.
00181              88  CL-LAST-TRL-AVAIL      VALUE +100.
00182              88  CL-RESV-EXP-HIST-TRLR  VALUE +0.
00183          16  CL-LAST-INC-DT-CHANGE   PIC S9(4)     COMP.
00184          16  FILLER                  PIC XX.
00185          16  CL-AUTO-PAY-SEQ         PIC S9(4)     COMP.
00186          16  CL-ADDRESS-TRAILER-CNT.
00187              20  CL-INSURED-ADDR-CNT  PIC S9(1).
00188                  88  NO-INSURED-AVAILABLE    VALUE ZERO.
00189              20  CL-ACCOUNT-ADDR-CNT  PIC S9(1).
00190                  88  ACCOUNT-IS-ONLINE       VALUE ZERO.
00191              20  CL-BENIF-ADDR-CNT    PIC S9(1).
00192                  88  BENEFICIARY-IS-ONLINE   VALUE ZERO.
00193              20  CL-EMPLOYER-ADDR-CNT PIC S9(1).
00194                  88  NO-EMPLOY-AVAILABLE     VALUE ZERO.
00195              20  CL-DOCTOR-ADDR-CNT   PIC S9(1).
00196                  88  NO-DOCTOR-AVAILABLE     VALUE ZERO.
00197              20  CL-OTHER-1-ADDR-CNT  PIC S9(1).
00198                  88  NO-OTHER-1-ADDRESSES    VALUE ZERO.
00199              20  CL-OTHER-2-ADDR-CNT  PIC S9(1).
00200                  88  NO-OTHER-2-ADDRESSES    VALUE ZERO.
00201
00202      12  CL-CV-REFERENCE-NO.
00203          16  CL-CV-REFNO-PRIME       PIC X(18).
00204          16  CL-CV-REFNO-SFX         PIC XX.
00205
00206      12  CL-FILE-LOCATION            PIC X(4).
00207
00208      12  CL-PROCESS-ERRORS.
00209          16  CL-FATAL-ERROR-CNT      PIC S9(4)     COMP.
00210              88  NO-FATAL-ERRORS        VALUE ZERO.
00211          16  CL-FORCEABLE-ERROR-CNT  PIC S9(4)     COMP.
00212              88  NO-FORCABLE-ERRORS     VALUE ZERO.
00213
00214      12  CL-PRODUCT-CD               PIC X.
00215
00216      12  CL-CURRENT-KEY-DATA.
00217          16  CL-CURRENT-CARRIER      PIC X.
00218          16  CL-CURRENT-GROUPING     PIC X(6).
00219          16  CL-CURRENT-STATE        PIC XX.
00220          16  CL-CURRENT-ACCOUNT      PIC X(10).
00221
00222      12  CL-ASSOCIATES               PIC X.
00223          88  CL-ASSOC-NO-INTERFACE      VALUE 'A'.
00224          88  CL-ASSOC-INTERFACE         VALUE 'I'.
00225          88  CL-NON-ASSOC-NO-INTERFACE  VALUE 'N'.
00226          88  CL-NON-ASSOC-INTERFACE     VALUE 'M'.
00227
00228      12  CL-ACTIVITY-CODE            PIC 99.
00229      12  CL-ACTIVITY-MAINT-DT        PIC XX.
00230      12  CL-ACTIVITY-MAINT-TYPE      PIC X(4).
00231
00232      12  CL-LAPSE-REPORT-CODE        PIC 9.
00233      12  CL-LAG-REPORT-CODE          PIC 9.
00234      12  CL-LOAN-TYPE                PIC XX.
00235      12  CL-LEGAL-STATE              PIC XX.
00236
CIDMOD     12  CL-YESNOSW                  PIC X.
031213     12  CL-ACCIDENT-CLAIM-SW        PIC X.
031213         88  CL-ACCIDENT-NOT-SET           VALUE ' '.
031213         88  CL-CLAIM-DUE-TO-ACCIDENT      VALUE 'Y'.
031213         88  CL-CLAIM-NOT-DUE-TO-ACCIDENT  VALUE 'N'.
051414     12  cl-insured-type             pic x.
051414         88  cl-claim-on-primary         value 'P'.
051414         88  cl-claim-on-co-borrower     value 'C'.
031213     12  cl-benefit-expiration-dt    PIC XX.
00241      EJECT
00242 *                                COPY ELCAID.
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
051007*00039    02  DFHPF22   PIC  X  VALUE  '?'.
051007   02  DFHPF22   PIC  X  VALUE  '['.
00040    02  DFHPF23   PIC  X  VALUE  '.'.
00041    02  DFHPF24   PIC  X  VALUE  '<'.
00042    02  DFHMSRE   PIC  X  VALUE  'X'.
00043    02  DFHSTRF   PIC  X  VALUE  'h'.
00044    02  DFHTRIG   PIC  X  VALUE  '"'.
00243  01  FILLER REDEFINES DFHAID.
00244      12  FILLER                  PIC X(8).
00245      12  AID-KEYS OCCURS 24 TIMES.
00246          16  FILLER              PIC X.
00247      EJECT
00248 *                                COPY EL160S.
       01  EL160AI.
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
           05  CFILEIDL PIC S9(0004) COMP.
           05  CFILEIDF PIC  X(0001).
           05  FILLER REDEFINES CFILEIDF.
               10  CFILEIDA PIC  X(0001).
           05  CFILEIDI PIC  X(0001).
      *    -------------------------------
           05  CARRSL PIC S9(0004) COMP.
           05  CARRSF PIC  X(0001).
           05  FILLER REDEFINES CARRSF.
               10  CARRSA PIC  X(0001).
           05  CARRSI PIC  X(0001).
      *    -------------------------------
           05  INCLSL PIC S9(0004) COMP.
           05  INCLSF PIC  X(0001).
           05  FILLER REDEFINES INCLSF.
               10  INCLSA PIC  X(0001).
           05  INCLSI PIC  X(0008).
      *    -------------------------------
           05  INCHSL PIC S9(0004) COMP.
           05  INCHSF PIC  X(0001).
           05  FILLER REDEFINES INCHSF.
               10  INCHSA PIC  X(0001).
           05  INCHSI PIC  X(0008).
      *    -------------------------------
           05  GRPSL PIC S9(0004) COMP.
           05  GRPSF PIC  X(0001).
           05  FILLER REDEFINES GRPSF.
               10  GRPSA PIC  X(0001).
           05  GRPSI PIC  X(0006).
      *    -------------------------------
           05  PMTDLSL PIC S9(0004) COMP.
           05  PMTDLSF PIC  X(0001).
           05  FILLER REDEFINES PMTDLSF.
               10  PMTDLSA PIC  X(0001).
           05  PMTDLSI PIC  X(0008).
      *    -------------------------------
           05  PMTDHSL PIC S9(0004) COMP.
           05  PMTDHSF PIC  X(0001).
           05  FILLER REDEFINES PMTDHSF.
               10  PMTDHSA PIC  X(0001).
           05  PMTDHSI PIC  X(0008).
      *    -------------------------------
           05  STATESL PIC S9(0004) COMP.
           05  STATESF PIC  X(0001).
           05  FILLER REDEFINES STATESF.
               10  STATESA PIC  X(0001).
           05  STATESI PIC  X(0002).
      *    -------------------------------
           05  OPENLSL PIC S9(0004) COMP.
           05  OPENLSF PIC  X(0001).
           05  FILLER REDEFINES OPENLSF.
               10  OPENLSA PIC  X(0001).
           05  OPENLSI PIC  X(0003).
      *    -------------------------------
           05  OPENHSL PIC S9(0004) COMP.
           05  OPENHSF PIC  X(0001).
           05  FILLER REDEFINES OPENHSF.
               10  OPENHSA PIC  X(0001).
           05  OPENHSI PIC  X(0003).
      *    -------------------------------
           05  ACCTSL PIC S9(0004) COMP.
           05  ACCTSF PIC  X(0001).
           05  FILLER REDEFINES ACCTSF.
               10  ACCTSA PIC  X(0001).
           05  ACCTSI PIC  X(0010).
      *    -------------------------------
           05  AMTLSL PIC S9(0004) COMP.
           05  AMTLSF PIC  X(0001).
           05  FILLER REDEFINES AMTLSF.
               10  AMTLSA PIC  X(0001).
           05  AMTLSI PIC  X(0010).
      *    -------------------------------
           05  AMTHSL PIC S9(0004) COMP.
           05  AMTHSF PIC  X(0001).
           05  FILLER REDEFINES AMTHSF.
               10  AMTHSA PIC  X(0001).
           05  AMTHSI PIC  X(0010).
      *    -------------------------------
           05  TYPESL PIC S9(0004) COMP.
           05  TYPESF PIC  X(0001).
           05  FILLER REDEFINES TYPESF.
               10  TYPESA PIC  X(0001).
           05  TYPESI PIC  X(0001).
      *    -------------------------------
           05  CAUSELSL PIC S9(0004) COMP.
           05  CAUSELSF PIC  X(0001).
           05  FILLER REDEFINES CAUSELSF.
               10  CAUSELSA PIC  X(0001).
           05  CAUSELSI PIC  X(0006).
      *    -------------------------------
           05  CAUSEHSL PIC S9(0004) COMP.
           05  CAUSEHSF PIC  X(0001).
           05  FILLER REDEFINES CAUSEHSF.
               10  CAUSEHSA PIC  X(0001).
           05  CAUSEHSI PIC  X(0006).
      *    -------------------------------
           05  DENSL PIC S9(0004) COMP.
           05  DENSF PIC  X(0001).
           05  FILLER REDEFINES DENSF.
               10  DENSA PIC  X(0001).
           05  DENSI PIC  X(0001).
      *    -------------------------------
           05  REPLSL PIC S9(0004) COMP.
           05  REPLSF PIC  X(0001).
           05  FILLER REDEFINES REPLSF.
               10  REPLSA PIC  X(0001).
           05  REPLSI PIC  X(0008).
      *    -------------------------------
           05  REPHSL PIC S9(0004) COMP.
           05  REPHSF PIC  X(0001).
           05  FILLER REDEFINES REPHSF.
               10  REPHSA PIC  X(0001).
           05  REPHSI PIC  X(0008).
      *    -------------------------------
           05  PROCSL PIC S9(0004) COMP.
           05  PROCSF PIC  X(0001).
           05  FILLER REDEFINES PROCSF.
               10  PROCSA PIC  X(0001).
           05  PROCSI PIC  X(0004).
      *    -------------------------------
           05  PMTLSL PIC S9(0004) COMP.
           05  PMTLSF PIC  X(0001).
           05  FILLER REDEFINES PMTLSF.
               10  PMTLSA PIC  X(0001).
           05  PMTLSI PIC  X(0010).
      *    -------------------------------
           05  PMTHSL PIC S9(0004) COMP.
           05  PMTHSF PIC  X(0001).
           05  FILLER REDEFINES PMTHSF.
               10  PMTHSA PIC  X(0001).
           05  PMTHSI PIC  X(0010).
      *    -------------------------------
           05  PREMSL PIC S9(0004) COMP.
           05  PREMSF PIC  X(0001).
           05  FILLER REDEFINES PREMSF.
               10  PREMSA PIC  X(0001).
           05  PREMSI PIC  X(0001).
      *    -------------------------------
           05  MNTLSL PIC S9(0004) COMP.
           05  MNTLSF PIC  X(0001).
           05  FILLER REDEFINES MNTLSF.
               10  MNTLSA PIC  X(0001).
           05  MNTLSI PIC  X(0008).
      *    -------------------------------
           05  MNTHSL PIC S9(0004) COMP.
           05  MNTHSF PIC  X(0001).
           05  FILLER REDEFINES MNTHSF.
               10  MNTHSA PIC  X(0001).
           05  MNTHSI PIC  X(0008).
      *    -------------------------------
           05  REQSL PIC S9(0004) COMP.
           05  REQSF PIC  X(0001).
           05  FILLER REDEFINES REQSF.
               10  REQSA PIC  X(0001).
           05  REQSI PIC  X(0001).
      *    -------------------------------
           05  ESTLSL PIC S9(0004) COMP.
           05  ESTLSF PIC  X(0001).
           05  FILLER REDEFINES ESTLSF.
               10  ESTLSA PIC  X(0001).
           05  ESTLSI PIC  X(0008).
      *    -------------------------------
           05  ESTHSL PIC S9(0004) COMP.
           05  ESTHSF PIC  X(0001).
           05  FILLER REDEFINES ESTHSF.
               10  ESTHSA PIC  X(0001).
           05  ESTHSI PIC  X(0008).
      *    -------------------------------
           05  SUPRSL PIC S9(0004) COMP.
           05  SUPRSF PIC  X(0001).
           05  FILLER REDEFINES SUPRSF.
               10  SUPRSA PIC  X(0001).
           05  SUPRSI PIC  X(0001).
      *    -------------------------------
           05  FOLLSL PIC S9(0004) COMP.
           05  FOLLSF PIC  X(0001).
           05  FILLER REDEFINES FOLLSF.
               10  FOLLSA PIC  X(0001).
           05  FOLLSI PIC  X(0008).
      *    -------------------------------
           05  FOLHSL PIC S9(0004) COMP.
           05  FOLHSF PIC  X(0001).
           05  FILLER REDEFINES FOLHSF.
               10  FOLHSA PIC  X(0001).
           05  FOLHSI PIC  X(0008).
      *    -------------------------------
           05  CERTSL PIC S9(0004) COMP.
           05  CERTSF PIC  X(0001).
           05  FILLER REDEFINES CERTSF.
               10  CERTSA PIC  X(0001).
           05  CERTSI PIC  X(0001).
      *    -------------------------------
           05  DAYSLSL PIC S9(0004) COMP.
           05  DAYSLSF PIC  X(0001).
           05  FILLER REDEFINES DAYSLSF.
               10  DAYSLSA PIC  X(0001).
           05  DAYSLSI PIC  X(0003).
      *    -------------------------------
           05  DAYSHSL PIC S9(0004) COMP.
           05  DAYSHSF PIC  X(0001).
           05  FILLER REDEFINES DAYSHSF.
               10  DAYSHSA PIC  X(0001).
           05  DAYSHSI PIC  X(0003).
      *    -------------------------------
           05  PRISL PIC S9(0004) COMP.
           05  PRISF PIC  X(0001).
           05  FILLER REDEFINES PRISF.
               10  PRISA PIC  X(0001).
           05  PRISI PIC  X(0001).
      *    -------------------------------
           05  AUTOSL PIC S9(0004) COMP.
           05  AUTOSF PIC  X(0001).
           05  FILLER REDEFINES AUTOSF.
               10  AUTOSA PIC  X(0001).
           05  AUTOSI PIC  X(0001).
      *    -------------------------------
           05  PRTOPTL PIC S9(0004) COMP.
           05  PRTOPTF PIC  X(0001).
           05  FILLER REDEFINES PRTOPTF.
               10  PRTOPTA PIC  X(0001).
           05  PRTOPTI PIC  X(0001).
      *    -------------------------------
           05  OPCLSL PIC S9(0004) COMP.
           05  OPCLSF PIC  X(0001).
           05  FILLER REDEFINES OPCLSF.
               10  OPCLSA PIC  X(0001).
           05  OPCLSI PIC  X(0001).
      *    -------------------------------
           05  FMTOPTL PIC S9(0004) COMP.
           05  FMTOPTF PIC  X(0001).
           05  FILLER REDEFINES FMTOPTF.
               10  FMTOPTA PIC  X(0001).
           05  FMTOPTI PIC  X(0001).
      *    -------------------------------
           05  ASEXL PIC S9(0004) COMP.
           05  ASEXF PIC  X(0001).
           05  FILLER REDEFINES ASEXF.
               10  ASEXA PIC  X(0001).
           05  ASEXI PIC  X(0001).
      *    -------------------------------
           05  ALTPRTL PIC S9(0004) COMP.
           05  ALTPRTF PIC  X(0001).
           05  FILLER REDEFINES ALTPRTF.
               10  ALTPRTA PIC  X(0001).
           05  ALTPRTI PIC  X(0004).
      *    -------------------------------
           05  MSG1L PIC S9(0004) COMP.
           05  MSG1F PIC  X(0001).
           05  FILLER REDEFINES MSG1F.
               10  MSG1A PIC  X(0001).
           05  MSG1I PIC  X(0075).
      *    -------------------------------
           05  MSG2L PIC S9(0004) COMP.
           05  MSG2F PIC  X(0001).
           05  FILLER REDEFINES MSG2F.
               10  MSG2A PIC  X(0001).
           05  MSG2I PIC  X(0075).
      *    -------------------------------
           05  PFKEYL PIC S9(0004) COMP.
           05  PFKEYF PIC  X(0001).
           05  FILLER REDEFINES PFKEYF.
               10  PFKEYA PIC  X(0001).
           05  PFKEYI PIC  X(0002).
       01  EL160AO REDEFINES EL160AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TIMEO PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CFILEIDO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CARRSO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INCLSO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INCHSO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GRPSO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PMTDLSO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PMTDHSO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATESO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  OPENLSO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  OPENHSO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCTSO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMTLSO PIC  Z(7).99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMTHSO PIC  Z(7).99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPESO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CAUSELSO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CAUSEHSO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DENSO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REPLSO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REPHSO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PROCSO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PMTLSO PIC  Z(7).99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PMTHSO PIC  Z(7).99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PREMSO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MNTLSO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MNTHSO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REQSO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ESTLSO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ESTHSO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SUPRSO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FOLLSO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FOLHSO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CERTSO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DAYSLSO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DAYSHSO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRISO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AUTOSO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRTOPTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  OPCLSO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FMTOPTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ASEXO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALTPRTO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSG1O PIC  X(0075).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSG2O PIC  X(0075).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFKEYO PIC  X(0002).
      *    -------------------------------
       01  EL160BI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  DATEBL PIC S9(0004) COMP.
           05  DATEBF PIC  X(0001).
           05  FILLER REDEFINES DATEBF.
               10  DATEBA PIC  X(0001).
           05  DATEBI PIC  X(0008).
      *    -------------------------------
           05  TIMEBL PIC S9(0004) COMP.
           05  TIMEBF PIC  X(0001).
           05  FILLER REDEFINES TIMEBF.
               10  TIMEBA PIC  X(0001).
           05  TIMEBI PIC  X(0005).
      *    -------------------------------
           05  TITLEL PIC S9(0004) COMP.
           05  TITLEF PIC  X(0001).
           05  FILLER REDEFINES TITLEF.
               10  TITLEA PIC  X(0001).
           05  TITLEI PIC  X(0028).
      *    -------------------------------
           05  PIKEYL PIC S9(0004) COMP.
           05  PIKEYF PIC  X(0001).
           05  FILLER REDEFINES PIKEYF.
               10  PIKEYA PIC  X(0001).
           05  PIKEYI PIC  X(0039).
      *    -------------------------------
           05  SCNERRL PIC S9(0004) COMP.
           05  SCNERRF PIC  X(0001).
           05  FILLER REDEFINES SCNERRF.
               10  SCNERRA PIC  X(0001).
           05  SCNERRI PIC  X(0004).
      *    -------------------------------
           05  USERSAVL PIC S9(0004) COMP.
           05  USERSAVF PIC  X(0001).
           05  FILLER REDEFINES USERSAVF.
               10  USERSAVA PIC  X(0001).
           05  USERSAVI PIC  X(0004).
      *    -------------------------------
           05  TIMESAVL PIC S9(0004) COMP.
           05  TIMESAVF PIC  X(0001).
           05  FILLER REDEFINES TIMESAVF.
               10  TIMESAVA PIC  X(0001).
           05  TIMESAVI PIC  9(07).
      *    -------------------------------
           05  NOSCRNL PIC S9(0004) COMP.
           05  NOSCRNF PIC  X(0001).
           05  FILLER REDEFINES NOSCRNF.
               10  NOSCRNA PIC  X(0001).
           05  NOSCRNI PIC  9999.
      *    -------------------------------
           05  TOTSCRNL PIC S9(0004) COMP.
           05  TOTSCRNF PIC  X(0001).
           05  FILLER REDEFINES TOTSCRNF.
               10  TOTSCRNA PIC  X(0001).
           05  TOTSCRNI PIC  X(0004).
      *    -------------------------------
           05  CLAIML PIC S9(0004) COMP.
           05  CLAIMF PIC  X(0001).
           05  FILLER REDEFINES CLAIMF.
               10  CLAIMA PIC  X(0001).
           05  CLAIMI PIC  X(0007).
      *    -------------------------------
           05  TYPEL PIC S9(0004) COMP.
           05  TYPEF PIC  X(0001).
           05  FILLER REDEFINES TYPEF.
               10  TYPEA PIC  X(0001).
           05  TYPEI PIC  X(0001).
      *    -------------------------------
           05  CERTL PIC S9(0004) COMP.
           05  CERTF PIC  X(0001).
           05  FILLER REDEFINES CERTF.
               10  CERTA PIC  X(0001).
           05  CERTI PIC  X(0010).
      *    -------------------------------
           05  CERTSXL PIC S9(0004) COMP.
           05  CERTSXF PIC  X(0001).
           05  FILLER REDEFINES CERTSXF.
               10  CERTSXA PIC  X(0001).
           05  CERTSXI PIC  X(0001).
      *    -------------------------------
           05  CARRL PIC S9(0004) COMP.
           05  CARRF PIC  X(0001).
           05  FILLER REDEFINES CARRF.
               10  CARRA PIC  X(0001).
           05  CARRI PIC  X(0001).
      *    -------------------------------
           05  STATUSL PIC S9(0004) COMP.
           05  STATUSF PIC  X(0001).
           05  FILLER REDEFINES STATUSF.
               10  STATUSA PIC  X(0001).
           05  STATUSI PIC  X(0001).
      *    -------------------------------
           05  PROCL PIC S9(0004) COMP.
           05  PROCF PIC  X(0001).
           05  FILLER REDEFINES PROCF.
               10  PROCA PIC  X(0001).
           05  PROCI PIC  X(0004).
      *    -------------------------------
           05  FILEL PIC S9(0004) COMP.
           05  FILEF PIC  X(0001).
           05  FILLER REDEFINES FILEF.
               10  FILEA PIC  X(0001).
           05  FILEI PIC  X(0004).
      *    -------------------------------
           05  CREDCDL PIC S9(0004) COMP.
           05  CREDCDF PIC  X(0001).
           05  FILLER REDEFINES CREDCDF.
               10  CREDCDA PIC  X(0001).
           05  CREDCDI PIC  X(0016).
      *    -------------------------------
           05  MLNAMEL PIC S9(0004) COMP.
           05  MLNAMEF PIC  X(0001).
           05  FILLER REDEFINES MLNAMEF.
               10  MLNAMEA PIC  X(0001).
           05  MLNAMEI PIC  X(0015).
      *    -------------------------------
           05  MFNAMEL PIC S9(0004) COMP.
           05  MFNAMEF PIC  X(0001).
           05  FILLER REDEFINES MFNAMEF.
               10  MFNAMEA PIC  X(0001).
           05  MFNAMEI PIC  X(0015).
      *    -------------------------------
           05  MMINITL PIC S9(0004) COMP.
           05  MMINITF PIC  X(0001).
           05  FILLER REDEFINES MMINITF.
               10  MMINITA PIC  X(0001).
           05  MMINITI PIC  X(0001).
      *    -------------------------------
           05  SEXL PIC S9(0004) COMP.
           05  SEXF PIC  X(0001).
           05  FILLER REDEFINES SEXF.
               10  SEXA PIC  X(0001).
           05  SEXI PIC  X(0001).
      *    -------------------------------
           05  BIRTHL PIC S9(0004) COMP.
           05  BIRTHF PIC  X(0001).
           05  FILLER REDEFINES BIRTHF.
               10  BIRTHA PIC  X(0001).
           05  BIRTHI PIC  X(0008).
      *    -------------------------------
           05  SOCIALL PIC S9(0004) COMP.
           05  SOCIALF PIC  X(0001).
           05  FILLER REDEFINES SOCIALF.
               10  SOCIALA PIC  X(0001).
           05  SOCIALI PIC  X(0011).
      *    -------------------------------
           05  OCCL PIC S9(0004) COMP.
           05  OCCF PIC  X(0001).
           05  FILLER REDEFINES OCCF.
               10  OCCA PIC  X(0001).
           05  OCCI PIC  X(0006).
      *    -------------------------------
           05  CBENEL PIC S9(0004) COMP.
           05  CBENEF PIC  X(0001).
           05  FILLER REDEFINES CBENEF.
               10  CBENEA PIC  X(0001).
           05  CBENEI PIC  X(0010).
      *    -------------------------------
           05  BHEADL PIC S9(0004) COMP.
           05  BHEADF PIC  X(0001).
           05  FILLER REDEFINES BHEADF.
               10  BHEADA PIC  X(0001).
           05  BHEADI PIC  X(0029).
      *    -------------------------------
           05  CAUSEL PIC S9(0004) COMP.
           05  CAUSEF PIC  X(0001).
           05  FILLER REDEFINES CAUSEF.
               10  CAUSEA PIC  X(0001).
           05  CAUSEI PIC  X(0026).
      *    -------------------------------
           05  CCAUSCDL PIC S9(0004) COMP.
           05  CCAUSCDF PIC  X(0001).
           05  FILLER REDEFINES CCAUSCDF.
               10  CCAUSCDA PIC  X(0001).
           05  CCAUSCDI PIC  X(0006).
      *    -------------------------------
           05  ENDL PIC S9(0004) COMP.
           05  ENDF PIC  X(0001).
           05  FILLER REDEFINES ENDF.
               10  ENDA PIC  X(0001).
           05  ENDI PIC  X(0008).
      *    -------------------------------
           05  PDTHRUL PIC S9(0004) COMP.
           05  PDTHRUF PIC  X(0001).
           05  FILLER REDEFINES PDTHRUF.
               10  PDTHRUA PIC  X(0001).
           05  PDTHRUI PIC  X(0008).
      *    -------------------------------
           05  PDAMTL PIC S9(0004) COMP.
           05  PDAMTF PIC  X(0001).
           05  FILLER REDEFINES PDAMTF.
               10  PDAMTA PIC  X(0001).
           05  PDAMTI PIC  9(7)V99.
      *    -------------------------------
           05  NODAYSL PIC S9(0004) COMP.
           05  NODAYSF PIC  X(0001).
           05  FILLER REDEFINES NODAYSF.
               10  NODAYSA PIC  X(0001).
           05  NODAYSI PIC  9(5).
      *    -------------------------------
           05  NOPMTSL PIC S9(0004) COMP.
           05  NOPMTSF PIC  X(0001).
           05  FILLER REDEFINES NOPMTSF.
               10  NOPMTSA PIC  X(0001).
           05  NOPMTSI PIC  9(4).
      *    -------------------------------
           05  INCL PIC S9(0004) COMP.
           05  INCF PIC  X(0001).
           05  FILLER REDEFINES INCF.
               10  INCA PIC  X(0001).
           05  INCI PIC  X(0008).
      *    -------------------------------
           05  REPL PIC S9(0004) COMP.
           05  REPF PIC  X(0001).
           05  FILLER REDEFINES REPF.
               10  REPA PIC  X(0001).
           05  REPI PIC  X(0008).
      *    -------------------------------
           05  ESTL PIC S9(0004) COMP.
           05  ESTF PIC  X(0001).
           05  FILLER REDEFINES ESTF.
               10  ESTA PIC  X(0001).
           05  ESTI PIC  X(0008).
      *    -------------------------------
           05  MNTDTL PIC S9(0004) COMP.
           05  MNTDTF PIC  X(0001).
           05  FILLER REDEFINES MNTDTF.
               10  MNTDTA PIC  X(0001).
           05  MNTDTI PIC  X(0008).
      *    -------------------------------
           05  MNTTYPEL PIC S9(0004) COMP.
           05  MNTTYPEF PIC  X(0001).
           05  FILLER REDEFINES MNTTYPEF.
               10  MNTTYPEA PIC  X(0001).
           05  MNTTYPEI PIC  X(0006).
      *    -------------------------------
           05  PRICDL PIC S9(0004) COMP.
           05  PRICDF PIC  X(0001).
           05  FILLER REDEFINES PRICDF.
               10  PRICDA PIC  X(0001).
           05  PRICDI PIC  X(0001).
      *    -------------------------------
           05  SUPVL PIC S9(0004) COMP.
           05  SUPVF PIC  X(0001).
           05  FILLER REDEFINES SUPVF.
               10  SUPVA PIC  X(0001).
           05  SUPVI PIC  X(0001).
      *    -------------------------------
           05  LOANNOL PIC S9(0004) COMP.
           05  LOANNOF PIC  X(0001).
           05  FILLER REDEFINES LOANNOF.
               10  LOANNOA PIC  X(0001).
           05  LOANNOI PIC  X(0008).
      *    -------------------------------
           05  LOANBALL PIC S9(0004) COMP.
           05  LOANBALF PIC  X(0001).
           05  FILLER REDEFINES LOANBALF.
               10  LOANBALA PIC  X(0001).
           05  LOANBALI PIC  9(10)V99.
      *    -------------------------------
           05  CERTEFFL PIC S9(0004) COMP.
           05  CERTEFFF PIC  X(0001).
           05  FILLER REDEFINES CERTEFFF.
               10  CERTEFFA PIC  X(0001).
           05  CERTEFFI PIC  X(0008).
      *    -------------------------------
           05  CERTACTL PIC S9(0004) COMP.
           05  CERTACTF PIC  X(0001).
           05  FILLER REDEFINES CERTACTF.
               10  CERTACTA PIC  X(0001).
           05  CERTACTI PIC  X(0010).
      *    -------------------------------
           05  CERTSTL PIC S9(0004) COMP.
           05  CERTSTF PIC  X(0001).
           05  FILLER REDEFINES CERTSTF.
               10  CERTSTA PIC  X(0001).
           05  CERTSTI PIC  X(0002).
      *    -------------------------------
           05  CERTCARL PIC S9(0004) COMP.
           05  CERTCARF PIC  X(0001).
           05  FILLER REDEFINES CERTCARF.
               10  CERTCARA PIC  X(0001).
           05  CERTCARI PIC  X(0001).
      *    -------------------------------
           05  CERTGRPL PIC S9(0004) COMP.
           05  CERTGRPF PIC  X(0001).
           05  FILLER REDEFINES CERTGRPF.
               10  CERTGRPA PIC  X(0001).
           05  CERTGRPI PIC  X(0006).
      *    -------------------------------
           05  SOCSECL PIC S9(0004) COMP.
           05  SOCSECF PIC  X(0001).
           05  FILLER REDEFINES SOCSECF.
               10  SOCSECA PIC  X(0001).
           05  SOCSECI PIC  X(0011).
      *    -------------------------------
           05  CLNAMEL PIC S9(0004) COMP.
           05  CLNAMEF PIC  X(0001).
           05  FILLER REDEFINES CLNAMEF.
               10  CLNAMEA PIC  X(0001).
           05  CLNAMEI PIC  X(0015).
      *    -------------------------------
           05  CFNAMEL PIC S9(0004) COMP.
           05  CFNAMEF PIC  X(0001).
           05  FILLER REDEFINES CFNAMEF.
               10  CFNAMEA PIC  X(0001).
           05  CFNAMEI PIC  X(0010).
      *    -------------------------------
           05  CINITL PIC S9(0004) COMP.
           05  CINITF PIC  X(0001).
           05  FILLER REDEFINES CINITF.
               10  CINITA PIC  X(0001).
           05  CINITI PIC  X(0001).
      *    -------------------------------
           05  INSAGEL PIC S9(0004) COMP.
           05  INSAGEF PIC  X(0001).
           05  FILLER REDEFINES INSAGEF.
               10  INSAGEA PIC  X(0001).
           05  INSAGEI PIC  X(0002).
      *    -------------------------------
           05  CJLNAMEL PIC S9(0004) COMP.
           05  CJLNAMEF PIC  X(0001).
           05  FILLER REDEFINES CJLNAMEF.
               10  CJLNAMEA PIC  X(0001).
           05  CJLNAMEI PIC  X(0015).
      *    -------------------------------
           05  CJFAMEL PIC S9(0004) COMP.
           05  CJFAMEF PIC  X(0001).
           05  FILLER REDEFINES CJFAMEF.
               10  CJFAMEA PIC  X(0001).
           05  CJFAMEI PIC  X(0010).
      *    -------------------------------
           05  CJINITL PIC S9(0004) COMP.
           05  CJINITF PIC  X(0001).
           05  FILLER REDEFINES CJINITF.
               10  CJINITA PIC  X(0001).
           05  CJINITI PIC  X(0001).
      *    -------------------------------
           05  JAGEL PIC S9(0004) COMP.
           05  JAGEF PIC  X(0001).
           05  FILLER REDEFINES JAGEF.
               10  JAGEA PIC  X(0001).
           05  JAGEI PIC  X(0002).
      *    -------------------------------
           05  CVDESCRL PIC S9(0004) COMP.
           05  CVDESCRF PIC  X(0001).
           05  FILLER REDEFINES CVDESCRF.
               10  CVDESCRA PIC  X(0001).
           05  CVDESCRI PIC  X(0006).
      *    -------------------------------
           05  CVKINDL PIC S9(0004) COMP.
           05  CVKINDF PIC  X(0001).
           05  FILLER REDEFINES CVKINDF.
               10  CVKINDA PIC  X(0001).
           05  CVKINDI PIC  X(0003).
      *    -------------------------------
           05  CVCDL PIC S9(0004) COMP.
           05  CVCDF PIC  X(0001).
           05  FILLER REDEFINES CVCDF.
               10  CVCDA PIC  X(0001).
           05  CVCDI PIC  X(0002).
      *    -------------------------------
           05  CVOTRML PIC S9(0004) COMP.
           05  CVOTRMF PIC  X(0001).
           05  FILLER REDEFINES CVOTRMF.
               10  CVOTRMA PIC  X(0001).
           05  CVOTRMI PIC  X(0003).
      *    -------------------------------
           05  CVRTRML PIC S9(0004) COMP.
           05  CVRTRMF PIC  X(0001).
           05  FILLER REDEFINES CVRTRMF.
               10  CVRTRMA PIC  X(0001).
           05  CVRTRMI PIC  X(0003).
      *    -------------------------------
           05  CVOBENEL PIC S9(0004) COMP.
           05  CVOBENEF PIC  X(0001).
           05  FILLER REDEFINES CVOBENEF.
               10  CVOBENEA PIC  X(0001).
           05  CVOBENEI PIC  9(9)V99.
      *    -------------------------------
           05  CVFORML PIC S9(0004) COMP.
           05  CVFORMF PIC  X(0001).
           05  FILLER REDEFINES CVFORMF.
               10  CVFORMA PIC  X(0001).
           05  CVFORMI PIC  X(0012).
      *    -------------------------------
           05  CVCNCDTL PIC S9(0004) COMP.
           05  CVCNCDTF PIC  X(0001).
           05  FILLER REDEFINES CVCNCDTF.
               10  CVCNCDTA PIC  X(0001).
           05  CVCNCDTI PIC  X(0008).
      *    -------------------------------
           05  CVEXITL PIC S9(0004) COMP.
           05  CVEXITF PIC  X(0001).
           05  FILLER REDEFINES CVEXITF.
               10  CVEXITA PIC  X(0001).
           05  CVEXITI PIC  X(0008).
      *    -------------------------------
           05  CVSTATL PIC S9(0004) COMP.
           05  CVSTATF PIC  X(0001).
           05  FILLER REDEFINES CVSTATF.
               10  CVSTATA PIC  X(0001).
           05  CVSTATI PIC  X(0006).
      *    -------------------------------
           05  CMEMCAPL PIC S9(0004) COMP.
           05  CMEMCAPF PIC  X(0001).
           05  FILLER REDEFINES CMEMCAPF.
               10  CMEMCAPA PIC  X(0001).
           05  CMEMCAPI PIC  X(0010).
      *    -------------------------------
           05  CAPRL PIC S9(0004) COMP.
           05  CAPRF PIC  X(0001).
           05  FILLER REDEFINES CAPRF.
               10  CAPRA PIC  X(0001).
           05  CAPRI PIC  9(4)V9(4).
      *    -------------------------------
           05  CPFREQL PIC S9(0004) COMP.
           05  CPFREQF PIC  X(0001).
           05  FILLER REDEFINES CPFREQF.
               10  CPFREQA PIC  X(0001).
           05  CPFREQI PIC  99.
      *    -------------------------------
           05  CINDGRPL PIC S9(0004) COMP.
           05  CINDGRPF PIC  X(0001).
           05  FILLER REDEFINES CINDGRPF.
               10  CINDGRPA PIC  X(0001).
           05  CINDGRPI PIC  X(0001).
      *    -------------------------------
           05  CPREMTPL PIC S9(0004) COMP.
           05  CPREMTPF PIC  X(0001).
           05  FILLER REDEFINES CPREMTPF.
               10  CPREMTPA PIC  X(0001).
           05  CPREMTPI PIC  X(0002).
      *    -------------------------------
           05  CREINCDL PIC S9(0004) COMP.
           05  CREINCDF PIC  X(0001).
           05  FILLER REDEFINES CREINCDF.
               10  CREINCDA PIC  X(0001).
           05  CREINCDI PIC  X(0003).
      *    -------------------------------
           05  CMEMBERL PIC S9(0004) COMP.
           05  CMEMBERF PIC  X(0001).
           05  FILLER REDEFINES CMEMBERF.
               10  CMEMBERA PIC  X(0001).
           05  CMEMBERI PIC  X(0012).
      *    -------------------------------
           05  MSGBL PIC S9(0004) COMP.
           05  MSGBF PIC  X(0001).
           05  FILLER REDEFINES MSGBF.
               10  MSGBA PIC  X(0001).
           05  MSGBI PIC  X(0075).
      *    -------------------------------
           05  PFKEYBL PIC S9(0004) COMP.
           05  PFKEYBF PIC  X(0001).
           05  FILLER REDEFINES PFKEYBF.
               10  PFKEYBA PIC  X(0001).
           05  PFKEYBI PIC  X(0002).
       01  EL160BO REDEFINES EL160BI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DATEBO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TIMEBO PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TITLEO PIC  X(0028).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PIKEYO PIC  X(0039).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SCNERRO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USERSAVO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TIMESAVO PIC  9(07).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NOSCRNO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TOTSCRNO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLAIMO PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPEO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CERTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CERTSXO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CARRO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATUSO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PROCO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FILEO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CREDCDO PIC  X(0016).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MLNAMEO PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MFNAMEO PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MMINITO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SEXO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BIRTHO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SOCIALO PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  OCCO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CBENEO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BHEADO PIC  X(0029).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CAUSEO PIC  X(0026).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCAUSCDO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ENDO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PDTHRUO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PDAMTO PIC  Z(06).99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NODAYSO PIC  ZZZ99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NOPMTSO PIC  ZZ99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INCO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REPO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ESTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MNTDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MNTTYPEO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRICDO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SUPVO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LOANNOO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LOANBALO PIC  Z,ZZZ,Z99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CERTEFFO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CERTACTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CERTSTO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CERTCARO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CERTGRPO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SOCSECO PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLNAMEO PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CFNAMEO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CINITO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INSAGEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CJLNAMEO PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CJFAMEO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CJINITO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JAGEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CVDESCRO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CVKINDO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CVCDO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CVOTRMO PIC  999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CVRTRMO PIC  999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CVOBENEO PIC  ZZZZZZZZ.ZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CVFORMO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CVCNCDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CVEXITO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CVSTATO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CMEMCAPO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CAPRO PIC  9(3).9(4).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CPFREQO PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CINDGRPO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CPREMTPO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CREINCDO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CMEMBERO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSGBO PIC  X(0075).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFKEYBO PIC  X(0002).
      *    -------------------------------
00249      EJECT
00250 *                                COPY ELCINTF.
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
00251      12  EL160-WORK-AREA REDEFINES PI-PROGRAM-WORK-AREA.
00252          16  PI-TS-COUNT         PIC S9(4)   COMP.
00253          16  PI-TS-COUNT-1       PIC S9(4)   COMP.
00254          16  PI-EL160-KEY        PIC X(8).
00255          16  PI-EL1602-KEY       PIC X(8).
00256          16  PI-PRINT-OPTION     PIC X.
00257          16  PI-FORMAT-OPTION    PIC X.
00258          16  PI-PRINT-ID         PIC X(4).
00259          16  PI-ALT-PRINT-ID     PIC X(4).
00260          16  PI-FILE-ID-IND      PIC X(1).
00261              88  PI-RETRIEVAL-FILE           VALUE 'R'.
00262              88  PI-MASTER-FILE              VALUE 'M'.
00263          16  FILLER              PIC X(609).
00264      EJECT
00265 *                                COPY ELCEMIB.
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
00266      EJECT
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
00268  01  DFHCOMMAREA                 PIC X(1024).
00269
00270  01  CLAIM-MASTER-L              PIC X(0350).
00271      EJECT
00272 *                                COPY ELCCNTL.
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
00273      EJECT
00274 *                                COPY ELCACTQ.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCACTQ.                            *
00004 *           PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.004                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = ACTIVITY QUE FILE                         *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 60     RECFORM = FIXED                         *
00011 *                                                                *
00012 *   BASE CLUSTER NAME = ELACTQ             RKP=2,LEN=20          *
00013 *       ALTERNATE INDEX = NONE                                   *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
CIDMOD*                                                                *
CIDMOD*  NO  CID  MODS  IN  COPYBOOK  ELCACTQ                          *
00017 ******************************************************************
00018
00019  01  ACTIVITY-QUE.
00020      12  AQ-RECORD-ID                PIC XX.
00021          88  VALID-AQ-ID                VALUE 'AQ'.
00022
00023      12  AQ-CONTROL-PRIMARY.
00024          16  AQ-COMPANY-CD           PIC X.
00025          16  AQ-CARRIER              PIC X.
00026          16  AQ-CLAIM-NO             PIC X(7).
00027          16  AQ-CERT-NO.
00028              20  AQ-CERT-PRIME       PIC X(10).
00029              20  AQ-CERT-SFX         PIC X.
00030
00031      12  AQ-PENDING-ACTIVITY-FLAGS.
00032          88  NO-PENDING-ACTIVITY        VALUE SPACES.
00033          16  AQ-PENDING-PAYMENT-FLAG PIC X.
00034              88  PENDING-PAYMENTS       VALUE '1'.
00035          16  AQ-PENDING-STATUS-FLAG  PIC X.
00036              88  PENDING-FULL-PRINT     VALUE '1'.
00037              88  PENDING-PART-PRINT     VALUE '2'.
00038          16  AQ-PENDING-LETTER-FLAG  PIC X.
00039              88  PENDING-LETTERS        VALUE '1'.
00040          16  AQ-PENDING-CLAIM-RESTORE PIC X.
00041              88  PENDING-RESTORE        VALUE 'C'.
00042              88  PENDING-RESTORE-LETTER VALUE 'L'.
00043
00044      12  FILLER                      PIC X(20).
00045
00046      12  AQ-RESEND-DATE              PIC XX.
00047      12  AQ-FOLLOWUP-DATE            PIC XX.
00048      12  AQ-PAYMENT-COUNTER          PIC S9        COMP-3.
00049      12  AQ-PMT-UNAPPROVED-COUNT     PIC S9        COMP-3.
00050      12  AQ-AUTO-LETTER              PIC X(4).
00051      12  FILLER                      PIC XX.
00052      12  AQ-LAST-UPDATED-BY          PIC S9(4)     COMP.
00053 *****************************************************************
00275      EJECT
00276 *                                COPY ELCRETR.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCRETR.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.002                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = CLAIM MASTER RETRIEVE FILE                *
00008 *                                                                *
00009 *   **** NOTE -- THIS FILE IS IDENTICAL TO CLAIM MASTER (ELMSTR) *
00010 *   ****      ANY CHANGES TO THIS COPYBOOK OR ELCMSTR MUST BE    *
00011 *   ****      DUPLICATED IN THE OTHER.                           *
00012 *                                                                *
00013 *   FILE TYPE = VSAM,KSDS                                        *
00014 *   RECORD SIZE = 350  RECFORM = FIXED                           *
00015 *                                                                *
00016 *   BASE CLUSTER = ELRETR                         RKP=2,LEN=20   *
00017 *       ALTERNATE PATH1 = ELRETR2 (BY NAME)       RKP=22,LEN=29  *
00018 *       ALTERNATE PATH2 = ELRETR3 (BY SOC SEC NO) RKP=51,LEN=12  *
00019 *       ALTERNATE PATH3 = ELRETR5 (BY CERT NO)    RKP=63,LEN=12  *
00020 *       ALTERNATE PATH4 = ELRETR6 (BY CREDIT CARD NO)
00021 *                                                 RKP=75,LEN=21  *
00022 *                                                                *
00023 *   LOG = YES                                                    *
00024 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00025 ******************************************************************
00026  01  RETRIEVE-MASTER.
00027      12  RL-RECORD-ID                PIC XX.
00028          88  VALID-RL-ID         VALUE 'RL'.
00029
00030      12  RL-CONTROL-PRIMARY.
00031          16  RL-COMPANY-CD           PIC X.
00032          16  RL-CARRIER              PIC X.
00033          16  RL-CLAIM-NO             PIC X(7).
00034          16  RL-CERT-NO.
00035              20  RL-CERT-PRIME       PIC X(10).
00036              20  RL-CERT-SFX         PIC X.
00037
00038      12  RL-CONTROL-BY-NAME.
00039          16  RL-COMPANY-CD-A1        PIC X.
00040          16  RL-INSURED-LAST-NAME    PIC X(15).
00041          16  RL-INSURED-NAME.
00042              20  RL-INSURED-1ST-NAME PIC X(12).
00043              20  RL-INSURED-MID-INIT PIC X.
00044
00045      12  RL-CONTROL-BY-SSN.
00046          16  RL-COMPANY-CD-A2        PIC X.
00047          16  RL-SOC-SEC-NO.
00048              20  RL-SSN-STATE        PIC XX.
00049              20  RL-SSN-ACCOUNT      PIC X(6).
00050              20  RL-SSN-LN3          PIC X(3).
00051
00052      12  RL-CONTROL-BY-CERT-NO.
00053          16  RL-COMPANY-CD-A4        PIC X.
00054          16  RL-CERT-NO-A4.
00055              20  RL-CERT-A4-PRIME    PIC X(10).
00056              20  RL-CERT-A4-SFX      PIC X.
00057
00058      12  RL-CONTROL-BY-CCN.
00059          16  RL-COMPANY-CD-A5        PIC X.
00060          16  RL-CCN-A5.
00061              20  RL-CCN-NO.
00062                  24  RL-CCN-PREFIX-A5 PIC X(4).
00063                  24  RL-CCN-PRIME-A5 PIC X(12).
00064              20  RL-CCN-FILLER-A5    PIC X(4).
00065
00066      12  RL-INSURED-PROFILE-DATA.
00067          16  RL-INSURED-BIRTH-DT     PIC XX.
00068          16  RL-INSURED-SEX-CD       PIC X.
00069              88  RL-INSURED-IS-MALE     VALUE 'M'.
00070              88  RL-INSURED-IS-FEMALE   VALUE 'F'.
00071              88  RL-INSURED-SEX-UNKNOWN VALUE ' '.
00072          16  RL-INSURED-OCC-CD       PIC X(6).
00073          16  FILLER                  PIC X(5).
00074
00075      12  RL-PROCESSING-INFO.
00076          16  RL-PROCESSOR-ID         PIC X(4).
00077          16  RL-CLAIM-STATUS         PIC X.
00078              88  RL-CLAIM-IS-OPEN       VALUE 'O'.
00079              88  RL-CLAIM-IS-CLOSED     VALUE 'C'.
00080          16  RL-CLAIM-TYPE           PIC X.
00081 *            88  RL-AH-CLAIM            VALUE 'A'.
00082 *            88  RL-LIFE-CLAIM          VALUE 'L'.
00083 *            88  RL-PROPERTY-CLAIM      VALUE 'P'.
00084 *            88  RL-UNEMPLOYMENT-CLAIM  VALUE 'U'.
00085          16  RL-CLAIM-PREM-TYPE      PIC X.
00086              88  RL-SINGLE-PREMIUM         VALUE '1'.
00087              88  RL-O-B-COVERAGE           VALUE '2'.
00088              88  RL-OPEN-END-COVERAGE      VALUE '3'.
00089          16  RL-INCURRED-DT          PIC XX.
00090          16  RL-REPORTED-DT          PIC XX.
00091          16  RL-FILE-ESTABLISH-DT    PIC XX.
00092          16  RL-EST-END-OF-DISAB-DT  PIC XX.
00093          16  RL-LAST-PMT-DT          PIC XX.
00094          16  RL-LAST-PMT-AMT         PIC S9(7)V99  COMP-3.
00095          16  RL-PAID-THRU-DT         PIC XX.
00096          16  RL-TOTAL-PAID-AMT       PIC S9(7)V99  COMP-3.
00097          16  RL-NO-OF-PMTS-MADE      PIC S9(3)     COMP-3.
00098          16  RL-NO-OF-DAYS-PAID      PIC S9(4)     COMP.
00099          16  RL-PMT-CALC-METHOD      PIC X.
00100              88  RL-360-DAY-YR          VALUE '1'.
00101              88  RL-365-DAY-YR          VALUE '2'.
00102              88  RL-FULL-MONTHS         VALUE '3'.
00103          16  RL-CAUSE-CD             PIC X(6).
00104
00105          16  RL-PRIME-CERT-NO.
00106              20  RL-PRIME-CERT-PRIME PIC X(10).
00107              20  RL-PRIME-CERT-SFX   PIC X.
00108
00109          16  RL-SYSTEM-IDENTIFIER    PIC XX.
00110              88  RL-CREDIT-CLAIM        VALUE 'CR'.
00111              88  RL-CONVENIENCE-CLAIM   VALUE 'CV'.
00112
00113          16  RL-MICROFILM-NO         PIC X(10).
00114          16  RL-PROG-FORM-TYPE       PIC X.
00115          16  RL-LAST-ADD-ON-DT       PIC XX.
00116
00117          16  RL-LAST-REOPEN-DT       PIC XX.
00118          16  RL-LAST-CLOSE-DT        PIC XX.
00119          16  RL-LAST-CLOSE-REASON    PIC X.
00120              88  RL-FINAL-PAID          VALUE '1'.
00121              88  RL-CLAIM-DENIED        VALUE '2'.
00122              88  RL-AUTO-CLOSE          VALUE '3'.
00123              88  RL-MANUAL-CLOSE        VALUE '4'.
00124          16  RL-ASSOC-CERT-SEQU      PIC S99.
00125          16  RL-ASSOC-CERT-TOTAL     PIC S99.
00126          16  RL-CLAIM-PAYMENT-STATUS PIC 9.
00127              88  RL-PAYMENT-IN-PREP     VALUE 1 THRU 9.
00128          16  FILLER                  PIC X(5).
00129
00130      12  RL-CERTIFICATE-DATA.
00131          16  RL-CERT-ORIGIN          PIC X.
00132              88  RL-CERT-WAS-ONLINE     VALUE '1'.
00133              88  RL-CERT-WAS-CREATED    VALUE '2'.
00134              88  RL-COVERAGE-WAS-ADDED  VALUE '3'.
00135          16  RL-CERT-KEY-DATA.
00136              20  RL-CERT-CARRIER     PIC X.
00137              20  RL-CERT-GROUPING    PIC X(6).
00138              20  RL-CERT-STATE       PIC XX.
00139              20  RL-CERT-ACCOUNT.
00140                  24  RL-CERT-ACCOUNT-PREFIX PIC X(4).
00141                  24  RL-CERT-ACCOUNT-PRIME  PIC X(6).
00142              20  RL-CERT-EFF-DT      PIC XX.
00143
00144      12  RL-STATUS-CONTROLS.
00145          16  RL-PRIORITY-CD          PIC X.
00146              88  RL-HIGHEST-PRIORITY    VALUE '9'.
00147          16  RL-SUPV-ATTN-CD         PIC X.
00148              88  RL-SUPV-NOT-REQUIRED   VALUE ' ' 'N'.
00149              88  RL-SUPV-IS-REQUIRED    VALUE 'Y'.
00150          16  RL-PURGED-DT            PIC XX.
00151          16  RL-RESTORED-DT          PIC XX.
00152          16  RL-NEXT-AUTO-PAY-DT     PIC XX.
00153          16  RL-NEXT-RESEND-DT       PIC XX.
00154          16  RL-NEXT-FOLLOWUP-DT     PIC XX.
00155          16  FILLER                  PIC XX.
00156          16  RL-LAST-MAINT-DT        PIC XX.
00157          16  RL-LAST-MAINT-USER      PIC X(4).
00158          16  RL-LAST-MAINT-HHMMSS    PIC S9(6)     COMP-3.
00159          16  RL-LAST-MAINT-TYPE      PIC X.
00160              88  RL-CLAIM-SET-UP           VALUE ' '.
00161              88  RL-PAYMENT-MADE           VALUE '1'.
00162              88  RL-LETTER-SENT            VALUE '2'.
00163              88  RL-MASTER-WAS-ALTERED     VALUE '3'.
00164              88  RL-MASTER-WAS-RESTORED    VALUE '4'.
00165              88  RL-INCURRED-DATE-CHANGED  VALUE '5'.
00166              88  RL-FILE-CONVERTED         VALUE '6'.
00167          16  RL-RELATED-CLAIM-NO     PIC X(7).
00168          16  RL-HISTORY-ARCHIVE-DT   PIC XX.
00169          16  RL-BENEFICIARY          PIC X(10).
00170          16  RL-FILE-ESTABLISHED-BY  PIC X(4).
00171          16  FILLER                  PIC X(6).
00172
00173      12  RL-TRAILER-CONTROLS.
00174          16  RL-TRAILER-SEQ-CNT      PIC S9(4)     COMP.
00175              88  RL-1ST-TRL-AVAIL       VALUE +4095.
00176              88  RL-LAST-TRL-AVAIL      VALUE +100.
00177              88  RL-RESV-EXP-HIST-TRLR  VALUE +0.
00178          16  RL-LAST-INC-DT-CHANGE   PIC S9(4)     COMP.
00179          16  FILLER                  PIC XX.
00180          16  RL-AUTO-PAY-SEQ         PIC S9(4)     COMP.
00181          16  RL-ADDRESS-TRAILER-CNT.
00182              20  RL-INSURED-ADDR-CNT  PIC S9.
00183                  88  RL-NO-INSURED-AVAILABLE VALUE ZERO.
00184              20  RL-ACCOUNT-ADDR-CNT  PIC S9.
00185                  88  RL-ACCOUNT-IS-ONLINE    VALUE ZERO.
00186              20  RL-BENIF-ADDR-CNT    PIC S9.
00187                  88  RL-BENEFICIARY-IS-ONLINE VALUE ZERO.
00188              20  RL-EMPLOYER-ADDR-CNT PIC S9.
00189                  88  RL-NO-EMPLOY-AVAILABLE   VALUE ZERO.
00190              20  RL-DOCTOR-ADDR-CNT   PIC S9.
00191                  88  RL-NO-DOCTOR-AVAILABLE   VALUE ZERO.
00192              20  RL-OTHER-1-ADDR-CNT  PIC S9.
00193                  88  RL-NO-OTHER-1-ADDRESSES  VALUE ZERO.
00194              20  RL-OTHER-2-ADDR-CNT  PIC S9.
00195                  88  RL-NO-OTHER-2-ADDRESSES  VALUE ZERO.
00196
00197      12  RL-CV-REFERENCE-NO.
00198          16  RL-CV-REFNO-PRIME       PIC X(18).
00199          16  RL-CV-REFNO-SFX         PIC XX.
00200
00201      12  RL-FILE-LOCATION            PIC X(4).
00202
00203      12  RL-PROCESS-ERRORS.
00204          16  RL-FATAL-ERROR-CNT      PIC S9(4)     COMP.
00205              88  RL-NO-FATAL-ERRORS     VALUE ZERO.
00206          16  RL-FORCEABLE-ERROR-CNT  PIC S9(4)     COMP.
00207              88  RL-NO-FORCABLE-ERRORS  VALUE ZERO.
00208
00209      12  RL-PRODUCT-CD               PIC X.
00210
00211      12  RL-CURRENT-KEY-DATA.
00212          16  RL-CURRENT-CARRIER      PIC X.
00213          16  RL-CURRENT-GROUPING     PIC X(6).
00214          16  RL-CURRENT-STATE        PIC XX.
00215          16  RL-CURRENT-ACCOUNT      PIC X(10).
00216
00217      12  RL-ASSOCIATES               PIC X.
00218          88  RL-ASSOC-NO-INTERFACE      VALUE 'A'.
00219          88  RL-ASSOC-INTERFACE         VALUE 'I'.
00220          88  RL-NON-ASSOC-NO-INTERFACE  VALUE 'N'.
00221          88  RL-NON-ASSOC-INTERFACE     VALUE 'M'.
00222
00223      12  RL-ACTIVITY-CODE            PIC 99.
00224      12  RL-ACTIVITY-MAINT-DT        PIC XX.
00225      12  RL-ACTIVITY-MAINT-TYPE      PIC X(4).
00226
00227      12  RL-LAPSE-REPORT-CODE        PIC 9.
00228      12  RL-LAG-REPORT-CODE          PIC 9.
00229      12  RL-LOAN-TYPE                PIC XX.
00230      12  RL-LEGAL-STATE              PIC XX.
00231
00232      12  FILLER                      PIC X(5).
00277      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA CLAIM-MASTER-L
                                CONTROL-FILE ACTIVITY-QUE
                                RETRIEVE-MASTER.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL1602' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00279
00280      MOVE EIBDATE               TO DC-JULIAN-YYDDD.
00281      MOVE '5'                   TO DC-OPTION-CODE.
00282      PERFORM 9800-CONVERT-DATE THRU 9800-CONVERT-DATE-EXIT.
00283      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.
00284      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.
00285
00286      IF EIBCALEN = ZERO
00287          GO TO 8800-UNAUTHORIZED-ACCESS.
00288
00289      
      * EXEC CICS HANDLE CONDITION
00290 *        PGMIDERR (8820-XCTL-ERROR)
00291 *        ERROR (9990-ABEND)
00292 *    END-EXEC.
      *    MOVE '"$L.                  ! " #00004311' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303034333131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00293
00294      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.
00295      MOVE LIT-TRAN               TO TRANS-ID.
00296
00297      IF PI-RETRIEVAL-FILE
00298          MOVE 'ELRETR'           TO W-FILE-ID.
00299
00300      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00301          MOVE LOW-VALUES         TO EL160BO
00302          MOVE ER-0008            TO EMI-ERROR
00303          PERFORM 9900-ERROR-FORMAT THRU 9900-ERROR-FORMAT-EXIT
00304          MOVE LIT-IC             TO NOSCRNL
00305          GO TO 8110-SEND-DATA.
00306
00307      MOVE EIBTRMID   TO CLAS-TERM.
00308      MOVE LIT-SCREEN TO CLAS-QUAL.
00309
00310      IF LIT-PROG NOT = PI-CALLING-PROGRAM
00311          GO TO 0100-UPDATE-PI.
00312
00313      IF EIBAID = DFHCLEAR
00314          GO TO 8200-RETURN-PRIOR.
00315
00316      
      * EXEC CICS RECEIVE
00317 *        MAP ('EL160B')
00318 *        MAPSET ('EL160S')
00319 *    END-EXEC.
           MOVE 'EL160B' TO DFHEIV1
           MOVE 'EL160S' TO DFHEIV2
      *    MOVE '8"T                   ''   #00004338' TO DFHEIV0
           MOVE X'382254202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333338' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EL160BI, 
                 DFHEIV99, 
                 DFHEIV2, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00320
00321      MOVE SPACES TO ERROR-SWITCHES.
00322
00323      IF PFKEYBL GREATER THAN ZERO
00324          PERFORM 0200-TRANS-PF THRU 0210-TRANS-PF-EXIT.
00325
00326      IF SCREEN-ERROR
00327          MOVE ER-0008            TO EMI-ERROR
00328          PERFORM 9900-ERROR-FORMAT THRU 9900-ERROR-FORMAT-EXIT
00329          MOVE AL-UNBON           TO PFKEYBA
00330          MOVE LIT-IC             TO PFKEYBL
00331          GO TO 8110-SEND-DATA.
00332
00333      IF EIBAID = DFHPF1 OR DFHPF2
00334          GO TO 1100-CHECK-PFKEYS.
00335      IF EIBAID = DFHPF5 OR DFHPF6
00336          GO TO 1100-CHECK-PFKEYS.
00337      IF EIBAID = DFHPF3
00338          GO TO 8200-RETURN-PRIOR.
00339      IF EIBAID = DFHPF4
00340          GO TO 8500-GET-ACT.
00341      IF EIBAID = DFHPF7
00342          GO TO 0500-CHECK-IN-PROGRESS.
00343      IF EIBAID = DFHPF12
00344          GO TO 8300-GET-HELP.
00345      IF EIBAID = DFHPF23
00346          GO TO 8810-PF23-ENTERED.
00347      IF EIBAID = DFHPF24
00348          GO TO 8400-RETURN-MASTER.
00349
00350      IF EIBAID = DFHENTER
00351          NEXT SENTENCE
00352      ELSE
00353          MOVE ER-0029            TO EMI-ERROR
00354          PERFORM 9900-ERROR-FORMAT THRU 9900-ERROR-FORMAT-EXIT
00355          MOVE AL-UNBON           TO PFKEYBA
00356          MOVE LIT-IC             TO PFKEYBL
00357          GO TO 8110-SEND-DATA.
00358
00359      PERFORM 4000-CHECK-UPDATE THRU 4000-CHECK-UPDATE-EXIT.
00360      IF SCREEN-ERROR
00361          GO TO 8110-SEND-DATA.
00362
00363      IF NOSCRNL NOT = ZEROS
00364         GO TO 1000-BROWSE.
00365
00366      IF EIBAID = DFHPF1 OR DFHPF2
00367          GO TO 1100-CHECK-PFKEYS.
00368
00369      MOVE LIT-IC TO NOSCRNL.
00370      MOVE ER-0000 TO EMI-ERROR.
00371      PERFORM 9900-ERROR-FORMAT
00372              THRU 9900-ERROR-FORMAT-EXIT.
00373      GO TO 8110-SEND-DATA.
00374      EJECT
00375  0100-UPDATE-PI.
00376      IF PI-RETURN-TO-PROGRAM = LIT-PROG
00377          GO TO 0110-UPDATE-UP.
00378
00379      MOVE PI-SAVED-PROGRAM-5     TO PI-SAVED-PROGRAM-6.
00380      MOVE PI-SAVED-PROGRAM-4     TO PI-SAVED-PROGRAM-5.
00381      MOVE PI-SAVED-PROGRAM-3     TO PI-SAVED-PROGRAM-4.
00382      MOVE PI-SAVED-PROGRAM-2     TO PI-SAVED-PROGRAM-3.
00383      MOVE PI-SAVED-PROGRAM-1     TO PI-SAVED-PROGRAM-2.
00384      MOVE PI-RETURN-TO-PROGRAM   TO PI-SAVED-PROGRAM-1.
00385      MOVE PI-CALLING-PROGRAM     TO PI-RETURN-TO-PROGRAM.
00386      MOVE LIT-PROG               TO PI-CALLING-PROGRAM.
00387      MOVE ZEROS                  TO PI-TS-COUNT.
00388      GO TO 1100-CHECK-PFKEYS.
00389
00390  0110-UPDATE-UP.
00391      MOVE PI-RETURN-TO-PROGRAM   TO PI-CALLING-PROGRAM.
00392      MOVE PI-SAVED-PROGRAM-1     TO PI-RETURN-TO-PROGRAM.
00393      MOVE PI-SAVED-PROGRAM-2     TO PI-SAVED-PROGRAM-1.
00394      MOVE PI-SAVED-PROGRAM-3     TO PI-SAVED-PROGRAM-2.
00395      MOVE PI-SAVED-PROGRAM-4     TO PI-SAVED-PROGRAM-3.
00396      MOVE PI-SAVED-PROGRAM-5     TO PI-SAVED-PROGRAM-4.
00397      MOVE PI-SAVED-PROGRAM-6     TO PI-SAVED-PROGRAM-5.
00398      MOVE SPACES                 TO PI-SAVED-PROGRAM-6.
00399
00400      
      * EXEC CICS HANDLE CONDITION
00401 *        QIDERR       (0130-TS-ERROR)
00402 *        ITEMERR      (0130-TS-ERROR)
00403 *    END-EXEC.
      *    MOVE '"$N<                  ! # #00004422' TO DFHEIV0
           MOVE X'22244E3C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303034343232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00404
00405      
      * EXEC CICS READQ TS
00406 *        QUEUE      (PI-KEY)
00407 *        INTO       (PROGRAM-INTERFACE-BLOCK)
00408 *        LENGTH     (PI-COMM-LENGTH)
00409 *    END-EXEC.
      *    MOVE '*$I    L              ''   #00004427' TO DFHEIV0
           MOVE X'2A2449202020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-KEY, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00410
00411      
      * EXEC CICS DELETEQ TS
00412 *        QUEUE      (PI-KEY)
00413 *    END-EXEC.
      *    MOVE '*&                    #   #00004433' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343333' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-KEY, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00414
00415      PERFORM 3000-GET-RECORD THRU 3020-GET-RECORD-EXIT.
00416      MOVE LIT-IC                 TO NOSCRNL.
00417      GO TO 8100-SEND-MAP.
00418
00419  0130-TS-ERROR.
00420      MOVE LOW-VALUES             TO EL160BO.
00421      MOVE ER-0192                TO EMI-ERROR.
00422      PERFORM 9900-ERROR-FORMAT THRU 9900-ERROR-FORMAT-EXIT.
00423      MOVE EMI-ERROR-NUMBER (1) TO PI-LAST-ERROR-NO.
00424      MOVE EMI-MESSAGE-AREA (1)   TO MSGBO.
00425      GO TO 8100-SEND-MAP.
00426
00427  0200-TRANS-PF.
00428      IF EIBAID NOT = DFHENTER
00429          MOVE 'X'                TO ERROR-SWITCH
00430          GO TO 0210-TRANS-PF-EXIT.
00431
00432      IF PFKEYBI NOT NUMERIC
00433          MOVE 'X'                TO ERROR-SWITCH
00434          GO TO 0210-TRANS-PF-EXIT.
00435
00436      MOVE PFKEYBI TO CHECK-PFKEYS.
00437
00438      IF CHECK-PFKEYS LESS THAN NUM-ONE
00439        OR
00440         CHECK-PFKEYS GREATER THAN NUM-TWENTY-FOUR
00441          MOVE 'X'                TO ERROR-SWITCH
00442          GO TO 0210-TRANS-PF-EXIT.
00443
00444      MOVE AID-KEYS (CHECK-PFKEYS) TO EIBAID.
00445
00446  0210-TRANS-PF-EXIT.
00447      EXIT.
00448      EJECT
00449  0500-CHECK-IN-PROGRESS.
00450      
      * EXEC CICS  HANDLE CONDITION
00451 *           NOTFND   (0510-WRITE-INITIAL-TRAILER)
00452 *    END-EXEC.
      *    MOVE '"$I                   ! $ #00004472' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303034343732' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00453
00454      MOVE PI-COMPANY-CD          TO  RF-COMPANY-CD.
00455      MOVE 'RF'                   TO  RF-RECORD-ID.
00456      MOVE '2'                    TO  RF-RECORD-TYPE.
00457      MOVE 'EL160'                TO  RF-REPORT-ID.
00458      MOVE ZEROS                  TO  RF-LINE-NUMBER.
00459
00460      
      * EXEC CICS READ
00461 *        DATASET    (REPT-FILE-ID)
00462 *        INTO       (REPORT-SAVE-FILE)
00463 *        RIDFLD     (RF-CONTROL-PRIMARY)
00464 *    END-EXEC.
           MOVE LENGTH OF
            REPORT-SAVE-FILE
             TO DFHEIV11
      *    MOVE '&"IL       E          (   #00004482' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 REPT-FILE-ID, 
                 REPORT-SAVE-FILE, 
                 DFHEIV11, 
                 RF-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00465
00466      MOVE ER-0000                TO EMI-ERROR
00467      PERFORM 9900-ERROR-FORMAT THRU 9900-ERROR-FORMAT-EXIT
00468      MOVE -1                     TO PFKEYBL
00469      GO TO 8110-SEND-DATA.
00470
00471  0510-WRITE-INITIAL-TRAILER.
00472      MOVE PI-COMPANY-CD          TO  RF-COMPANY-CD.
00473      MOVE 'RF'                   TO  RF-RECORD-ID.
00474      MOVE '2'                    TO  RF-RECORD-TYPE.
00475      MOVE 'EL160'                TO  RF-REPORT-ID.
00476      MOVE ZEROS                  TO  RF-LINE-NUMBER.
00477
00478      MOVE SPACES                 TO  RF-TRAILER-RECORD.
00479      
      * EXEC CICS ASKTIME ABSTIME(LCP-CICS-TIME)
00480 *    END-EXEC
      *    MOVE '0"A                   "   #00004501' TO DFHEIV0
           MOVE X'302241202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202220' TO DFHEIV0(13:12)
           MOVE X'2020233030303034353031' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCP-CICS-TIME
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00481      
      * EXEC CICS FORMATTIME
00482 *              ABSTIME(LCP-CICS-TIME)
00483 *              TIME(LCP-TIME-OF-DAY-XX)
00484 *    END-EXEC
      *    MOVE 'j$(     (             #   #00004503' TO DFHEIV0
           MOVE X'6A2428202020202028202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034353033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCP-CICS-TIME, 
                 LCP-TIME-OF-DAY-XX
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00485      MOVE  LCP-TIME-OF-DAY-68 TO RF-PRINT-HH-MM-SS.
00486      MOVE 'STARTED'              TO  RF-CURRENT-DATE.
00487
00488      
      * EXEC CICS WRITE
00489 *        DATASET (REPT-FILE-ID)
00490 *        FROM    (REPORT-SAVE-FILE)
00491 *        RIDFLD  (RF-CONTROL-PRIMARY)
00492 *    END-EXEC.
           MOVE LENGTH OF
            REPORT-SAVE-FILE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00004510' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034353130' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 REPT-FILE-ID, 
                 REPORT-SAVE-FILE, 
                 DFHEIV11, 
                 RF-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00493
00494  0520-DELETE-REC.
00495      MOVE 1                      TO RF-LINE-NUMBER.
00496      
      * EXEC CICS  HANDLE CONDITION
00497 *           NOTFND   (0540-DELETE-REC)
00498 *    END-EXEC.
      *    MOVE '"$I                   ! % #00004518' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303034353138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00499
00500  0530-DELETE-1.
00501      MOVE PI-COMPANY-CD          TO RF-COMPANY-CD.
00502      MOVE 'RF'                   TO RF-RECORD-ID.
00503      MOVE '1'                    TO RF-RECORD-TYPE.
00504      MOVE 'EL160'                TO RF-REPORT-ID.
00505
00506      
      * EXEC CICS DELETE
00507 *        DATASET (REPT-FILE-ID)
00508 *        RIDFLD  (RF-CONTROL-PRIMARY)
00509 *        KEYLENGTH (11)
00510 *    END-EXEC.
           MOVE 11
             TO DFHEIV11
      *    MOVE '&(  RK                &   #00004528' TO DFHEIV0
           MOVE X'26282020524B202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034353238' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 REPT-FILE-ID, 
                 RF-CONTROL-PRIMARY, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00511
00512      ADD 1 TO RF-LINE-NUMBER.
00513      GO TO 0530-DELETE-1.
00514
00515  0540-DELETE-REC.
00516      
      * EXEC CICS  HANDLE CONDITION
00517 *           NOTFND   (0560-READ-TEMP-STORAGE)
00518 *    END-EXEC.
      *    MOVE '"$I                   ! & #00004538' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303034353338' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00519
00520  0550-DELETE-2.
00521      MOVE PI-COMPANY-CD          TO RF-COMPANY-CD.
00522      MOVE 'RF'                   TO RF-RECORD-ID.
00523      MOVE '2'                    TO RF-RECORD-TYPE.
00524      MOVE 'EL160'                TO RF-REPORT-ID.
00525
00526      
      * EXEC CICS DELETE
00527 *        DATASET (REPT-FILE-ID)
00528 *        RIDFLD  (RF-CONTROL-PRIMARY)
00529 *        KEYLENGTH (11)
00530 *    END-EXEC.
           MOVE 11
             TO DFHEIV11
      *    MOVE '&(  RK                &   #00004548' TO DFHEIV0
           MOVE X'26282020524B202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034353438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 REPT-FILE-ID, 
                 RF-CONTROL-PRIMARY, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00531
00532      ADD 1 TO RF-LINE-NUMBER.
00533      GO TO 0550-DELETE-2.
00534
00535  EJECT
00536  0560-READ-TEMP-STORAGE.
00537      
      * EXEC CICS HANDLE CONDITION
00538 *         ITEMERR (0570-DELETE-INITIAL-2)
00539 *    END-EXEC.
      *    MOVE '"$<                   ! '' #00004559' TO DFHEIV0
           MOVE X'22243C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303034353539' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00540
00541      MOVE +0 TO WS-TS-ITEM-NO.
00542
00543  0565-READ-NEXT-TEMP.
00544      ADD +1 TO WS-TS-ITEM-NO.
00545
00546      
      * EXEC CICS READQ TS
00547 *         QUEUE    (PI-EL1602-KEY)
00548 *         INTO     (WS-TS-AREA)
00549 *         LENGTH   (EL1602-LENGTH)
00550 *         ITEM     (WS-TS-ITEM-NO)
00551 *    END-EXEC.
      *    MOVE '*$II   L              ''   #00004568' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034353638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-EL1602-KEY, 
                 WS-TS-AREA, 
                 EL1602-LENGTH, 
                 WS-TS-ITEM-NO, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00552
00553      IF WS-LINE-COUNT GREATER THAN +55
00554         PERFORM 0800-PRINT-HEADINGS THRU 0800-EXIT.
00555
00556      MOVE WS-TS-CARR             TO WS-D1-CARR.
00557      MOVE WS-TS-CLAIM            TO WS-D1-CLAIM.
00558      MOVE WS-TS-CERT             TO WS-D1-CERT-PRIME.
00559      MOVE WS-TS-CERT-SFX         TO WS-D1-CERT-SFX.
00560
121802     EVALUATE TRUE
00561         WHEN WS-TS-TYPE = PI-LIFE-OVERRIDE-L1
00562            MOVE PI-LIFE-OVERRIDE-L6
                                       TO WS-D1-TYPE
121802        WHEN WS-TS-TYPE = PI-AH-OVERRIDE-L1
00564            MOVE PI-AH-OVERRIDE-L6
                                       TO WS-D1-TYPE
121802        WHEN WS-TS-TYPE = 'I'
121802           MOVE '  IU  '         TO WS-D1-TYPE
121802        WHEN WS-TS-TYPE = 'G'
121802           MOVE ' GAP  '         TO WS-D1-TYPE
052614
052614        WHEN WS-TS-TYPE = 'F'
052614           MOVE ' FAM  '         TO WS-D1-TYPE
080322        WHEN WS-TS-TYPE = 'B'
080322           MOVE ' BRV  '         TO WS-D1-TYPE
080322        WHEN WS-TS-TYPE = 'H'
080322           MOVE ' HOSP '         TO WS-D1-TYPE
100518
100518        WHEN WS-TS-TYPE = 'O'
100518           MOVE ' OTH  '         TO WS-D1-TYPE
121802     END-EVALUATE
00565
00566      IF WS-TS-STATUS = 'O'
00567         MOVE ' OPEN '            TO WS-D1-STATUS
00568      ELSE
00569         MOVE 'CLOSED'            TO WS-D1-STATUS.
00570
00571      MOVE WS-TS-FILE             TO WS-D1-FILE.
00572
00573      MOVE WS-TS-LNAME            TO WS-D1-NAME.
00574
00575      MOVE WS-DETAIL1             TO RF-DATA-133.
00576      MOVE ' '                    TO RF-CTL-CHAR-133.
00577      PERFORM 0600-PRT-LINE THRU 0600-EXIT.
00578
00579      GO TO 0565-READ-NEXT-TEMP.
00580
00581  0570-DELETE-INITIAL-2.
00582      MOVE PI-COMPANY-CD          TO RF-COMPANY-CD.
00583      MOVE 'RF'                   TO RF-RECORD-ID.
00584      MOVE '2'                    TO RF-RECORD-TYPE.
00585      MOVE 'EL160'                TO RF-REPORT-ID.
00586      MOVE ZEROS                  TO RF-LINE-NUMBER.
00587
00588      
      * EXEC CICS DELETE
00589 *         DATASET     (REPT-FILE-ID)
00590 *         RIDFLD      (RF-CONTROL-PRIMARY)
00591 *         KEYLENGTH   (11)
00592 *    END-EXEC.
           MOVE 11
             TO DFHEIV11
      *    MOVE '&(  RK                &   #00004628' TO DFHEIV0
           MOVE X'26282020524B202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034363238' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 REPT-FILE-ID, 
                 RF-CONTROL-PRIMARY, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00593
00594  0580-WRITE-TRAILER.
00595      MOVE PI-COMPANY-CD          TO RF-COMPANY-CD.
00596      MOVE 'RF'                   TO RF-RECORD-ID.
00597      MOVE 'EL160'                TO RF-REPORT-ID.
00598      MOVE '2'                    TO RF-RECORD-TYPE.
00599      ADD +1                      TO WS-LINE-NUMBER.
00600      MOVE WS-LINE-NUMBER         TO RF-LINE-NUMBER.
00601      MOVE SPACES                 TO RF-TRAILER-RECORD.
00602
00603      
      * EXEC CICS ASKTIME
00604 *        ABSTIME(LCP-CICS-TIME)
00605 *    END-EXEC.
      *    MOVE '0"A                   "   #00004643' TO DFHEIV0
           MOVE X'302241202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202220' TO DFHEIV0(13:12)
           MOVE X'2020233030303034363433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCP-CICS-TIME
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00606
00607      
      * EXEC CICS FORMATTIME
00608 *        ABSTIME(LCP-CICS-TIME)
00609 *        TIME(LCP-TIME-OF-DAY-XX)
00610 *    END-EXEC.
      *    MOVE 'j$(     (             #   #00004647' TO DFHEIV0
           MOVE X'6A2428202020202028202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034363437' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCP-CICS-TIME, 
                 LCP-TIME-OF-DAY-XX
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00611
00612      MOVE LCP-TIME-OF-DAY-68 TO RF-PRINT-HH-MM-SS.
00613      MOVE SAVE-DATE              TO RF-CURRENT-DATE.
00614
00615      
      * EXEC CICS WRITE
00616 *        DATASET (REPT-FILE-ID)
00617 *        FROM    (REPORT-SAVE-FILE)
00618 *        RIDFLD  (RF-CONTROL-PRIMARY)
00619 *    END-EXEC.
           MOVE LENGTH OF
            REPORT-SAVE-FILE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00004655' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034363535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 REPT-FILE-ID, 
                 REPORT-SAVE-FILE, 
                 DFHEIV11, 
                 RF-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00620
00621      MOVE ER-0000                TO EMI-ERROR.
00622      PERFORM 9900-ERROR-FORMAT THRU 9900-ERROR-FORMAT-EXIT.
00623      MOVE -1                     TO PFKEYBL.
00624      GO TO 8110-SEND-DATA.
00625
00626  0600-PRT-LINE.
00627      MOVE PI-COMPANY-CD          TO RF-COMPANY-CD.
00628      MOVE 'RF'                   TO RF-RECORD-ID.
00629      MOVE '1'                    TO RF-RECORD-TYPE.
00630      MOVE 'EL160'                TO RF-REPORT-ID.
00631      ADD +1                      TO WS-LINE-NUMBER.
00632      MOVE WS-LINE-NUMBER         TO RF-LINE-NUMBER.
00633
00634      
      * EXEC CICS WRITE
00635 *        DATASET     (REPT-FILE-ID)
00636 *        FROM        (REPORT-SAVE-FILE)
00637 *        RIDFLD      (RF-CONTROL-PRIMARY)
00638 *    END-EXEC.
           MOVE LENGTH OF
            REPORT-SAVE-FILE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00004674' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034363734' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 REPT-FILE-ID, 
                 REPORT-SAVE-FILE, 
                 DFHEIV11, 
                 RF-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00639
00640      ADD +1 TO WS-LINE-COUNT.
00641  0600-EXIT.
00642      EXIT.
00643
00644  0800-PRINT-HEADINGS.
00645      MOVE WS-HEADING1            TO RF-DATA-133.
00646      MOVE '1'                    TO RF-CTL-CHAR-133.
00647      PERFORM 0600-PRT-LINE THRU 0600-EXIT.
00648
00649      MOVE WS-HEADING2            TO RF-DATA-133.
00650      MOVE '-'                    TO RF-CTL-CHAR-133.
00651      PERFORM 0600-PRT-LINE THRU 0600-EXIT.
00652
00653      MOVE SPACES                 TO RF-DATA-133.
00654      MOVE '0'                    TO RF-CTL-CHAR-133.
00655      PERFORM 0600-PRT-LINE THRU 0600-EXIT.
00656
00657      MOVE +6                     TO WS-LINE-COUNT.
00658
00659  0800-EXIT.
00660      EXIT.
00661      EJECT
00662
00663  1000-BROWSE.
00664
00665      IF NOSCRNL = ZEROS
00666         GO TO 1100-CHECK-PFKEYS.
00667
00668      
      * EXEC CICS BIF DEEDIT
00669 *        FIELD   (NOSCRNI)
00670 *        LENGTH  (4)
00671 *    END-EXEC.
           MOVE 4
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004708' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 NOSCRNI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00672
00673      IF (PI-PROCESSOR-ID = 'LGXX') OR
00674         (PI-COMPANY-ID   = 'AIG' OR 'AUK')
00675          MOVE 2501               TO  MAX-TS-PAGES.
00676
00677      IF (NOSCRNI GREATER '00' AND LESS MAX-TS-PAGES)
00678         NEXT SENTENCE
00679      ELSE
00680         MOVE ER-0515             TO EMI-ERROR
00681         MOVE AL-UNBON            TO NOSCRNA
00682         MOVE -1                  TO NOSCRNL
00683         PERFORM 9900-ERROR-FORMAT THRU 9900-ERROR-FORMAT-EXIT
00684         GO TO 8110-SEND-DATA.
00685
00686      IF NOSCRNI GREATER THAN PI-TS-COUNT-1
00687         MOVE ER-0515             TO EMI-ERROR
00688         MOVE AL-UNBON            TO NOSCRNA
00689         MOVE -1                  TO NOSCRNL
00690         PERFORM 9900-ERROR-FORMAT THRU 9900-ERROR-FORMAT-EXIT
00691         GO TO 8110-SEND-DATA.
00692
00693      MOVE NOSCRNI                TO PI-TS-COUNT
00694      GO TO 1150-GET-TEMP-STOR.
00695
00696  1100-CHECK-PFKEYS.
00697 ********MODIFICATION TO ENABLE PRINTING OF AUDIT*******
00698 *
00699 ******************************************************************
00700 *       PI-PRINT-OPTION MUST BE 'N' FOR CARRIER AND ACCOUNT      *
00701 *       AND MUST HAVE AN ALTERNATE PRINTER-ID.                   *
00702 ******************************************************************
00703
00704      IF PI-RETRIEVAL-FILE
00705          IF EIBAID = DFHPF5
00706              MOVE ER-0971        TO EMI-ERROR
00707              PERFORM 9900-ERROR-FORMAT THRU 9900-ERROR-FORMAT-EXIT
00708              MOVE -1             TO PFKEYBL
00709              GO TO 8110-SEND-DATA
00710          ELSE
00711              IF EIBAID = DFHPF6
00712                  MOVE ER-0972    TO EMI-ERROR
00713                  PERFORM 9900-ERROR-FORMAT
00714                      THRU 9900-ERROR-FORMAT-EXIT
00715                  MOVE -1         TO PFKEYBL
00716                  GO TO 8110-SEND-DATA.
00717
00718      IF PI-CARRIER-SECURITY GREATER THAN SPACES OR
00719         PI-ACCOUNT-SECURITY GREATER THAN SPACES
00720            IF EIBAID = DFHPF5
00721               IF PI-ALT-PRINT-ID GREATER THAN SPACES
00722                  NEXT SENTENCE
00723               ELSE
00724                  MOVE ER-2379    TO EMI-ERROR
00725                  PERFORM 9900-ERROR-FORMAT THRU
00726                  9900-ERROR-FORMAT-EXIT
00727                  MOVE -1         TO PFKEYBL
00728                  GO TO 8110-SEND-DATA.
00729
00730      IF EIBAID = DFHPF5
00731          PERFORM 1200-PROCESS-OPTIONS
00732             THRU 1299-EXIT
00733          GO TO 1150-GET-TEMP-STOR.
00734
00735      IF EIBAID = DFHPF6 AND
00736         PI-PRINT-OPTION = 'N'
00737          MOVE ER-0609            TO EMI-ERROR
00738          PERFORM 9900-ERROR-FORMAT
00739                 THRU 9900-ERROR-FORMAT-EXIT
00740          MOVE -1                 TO PFKEYBL
00741          GO TO 8110-SEND-DATA.
00742
00743      IF EIBAID = DFHPF6
00744          MOVE PI-TS-COUNT        TO WS-SAVE-TS-COUNT
00745          MOVE +1                 TO PI-TS-COUNT  WS-PRINT-SW
00746          PERFORM 1200-PROCESS-OPTIONS THRU 1299-EXIT
00747             UNTIL PI-TS-COUNT GREATER PI-TS-COUNT-1
00748          MOVE WS-SAVE-TS-COUNT   TO PI-TS-COUNT
00749          GO TO 1150-GET-TEMP-STOR.
00750 *******************************************************
00751
00752      IF EIBAID = DFHPF2
00753         SUBTRACT +1 FROM PI-TS-COUNT
00754      ELSE
00755         ADD +1        TO PI-TS-COUNT.
00756
00757  1150-GET-TEMP-STOR.
00758      MOVE SPACE TO ERROR-SWITCH.
00759
00760      IF PI-TS-COUNT LESS THAN NUM-ONE
00761          MOVE 'X'                TO ERROR-SWITCH
00762          MOVE ER-0131         TO EMI-ERROR
00763          PERFORM 9900-ERROR-FORMAT
00764              THRU 9900-ERROR-FORMAT-EXIT
00765          MOVE NUM-ONE            TO PI-TS-COUNT.
00766
00767      PERFORM 3000-GET-RECORD THRU 3020-GET-RECORD-EXIT.
00768
00769      IF SCREEN-ERROR
00770          MOVE EMI-ERROR-NUMBER (1) TO PI-LAST-ERROR-NO
00771          MOVE EMI-MESSAGE-AREA (1) TO MSGBO
00772      ELSE
00773          IF SCNERRI = SPACES OR LOW-VALUES
00774              MOVE SCNERRI TO PI-LAST-ERROR-NO
00775              MOVE SPACES TO MSGBO
00776          ELSE
00777              MOVE SCNERRI TO EMI-ERROR PI-LAST-ERROR-NO
00778              PERFORM 9900-ERROR-FORMAT
00779                  THRU 9900-ERROR-FORMAT-EXIT
00780              MOVE EMI-MESSAGE-AREA (1) TO MSGBO.
00781
00782      MOVE LIT-IC                 TO NOSCRNL.
00783      GO TO 8100-SEND-MAP.
00784      EJECT
00785
00786  1200-PROCESS-OPTIONS.
00787      MOVE SPACE TO ERROR-SWITCH.
00788      PERFORM 3000-GET-RECORD THRU 3020-GET-RECORD-EXIT.
00789
00790      IF SCREEN-ERROR
00791          MOVE EMI-ERROR-NUMBER (1) TO PI-LAST-ERROR-NO
00792          MOVE EMI-MESSAGE-AREA (1) TO MSGBO
00793      ELSE
00794          IF SCNERRI = SPACES OR LOW-VALUES
00795              MOVE SCNERRI TO PI-LAST-ERROR-NO
00796              MOVE SPACES TO MSGBO
00797          ELSE
00798              MOVE SCNERRI TO EMI-ERROR PI-LAST-ERROR-NO
00799              PERFORM 9900-ERROR-FORMAT
00800                  THRU 9900-ERROR-FORMAT-EXIT
00801              MOVE EMI-MESSAGE-AREA (1) TO MSGBO.
00802
00803      MOVE PIKEYI                 TO PI-CONTROL-IN-PROGRESS.
00804
00805      IF PI-PRINT-OPTION = SPACE
00806          MOVE 'N' TO PI-PRINT-OPTION.
00807
00808      IF PI-FORMAT-OPTION = SPACE
00809          MOVE 'P' TO PI-FORMAT-OPTION.
00810
00811      IF PI-PRINT-OPTION = 'N'
00812        AND EIBAID NOT = DFHPF6
00813          GO TO 1260-START-PRINTER.
00814
00815      
      * EXEC CICS HANDLE CONDITION
00816 *        NOTFND(1230-CREATE-NEW-ACTQ)
00817 *        END-EXEC.
      *    MOVE '"$I                   ! ( #00004855' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303034383535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00818
00819      MOVE PI-COMPANY-CD          TO ACTQ-COMP-CD.
00820      MOVE PI-CARRIER             TO ACTQ-CARRIER.
00821      MOVE PI-CLAIM-NO            TO ACTQ-CLAIM-NO.
00822      MOVE PI-CERT-NO             TO ACTQ-CERT-NO.
00823
00824      MOVE 'ACTQ'                 TO FILE-SWITCH.
00825
00826      
      * EXEC CICS READ
00827 *        UPDATE
00828 *        DATASET  ('ELACTQ')
00829 *        SET      (ADDRESS OF ACTIVITY-QUE)
00830 *        RIDFLD   (ELACTQ-KEY)
00831 *    END-EXEC.
           MOVE 'ELACTQ' TO DFHEIV1
      *    MOVE '&"S        EU         (   #00004866' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383636' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELACTQ-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00832
00833      IF PI-FORMAT-OPTION = 'F'
00834          MOVE '1'                TO AQ-PENDING-STATUS-FLAG
00835      ELSE
00836          MOVE '2'                TO AQ-PENDING-STATUS-FLAG.
00837
00838      
      * EXEC CICS REWRITE
00839 *        DATASET ('ELACTQ')
00840 *        FROM    (ACTIVITY-QUE)
00841 *    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-QUE
             TO DFHEIV11
           MOVE 'ELACTQ' TO DFHEIV1
      *    MOVE '&& L                  %   #00004878' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ACTIVITY-QUE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00842
00843      IF PRINT-IN-PROCESS
00844          ADD +1 TO PI-TS-COUNT.
00845
00846      GO TO 1299-EXIT.
00847
00848  1230-CREATE-NEW-ACTQ.
00849      
      * EXEC CICS GETMAIN
00850 *        SET     (ADDRESS OF ACTIVITY-QUE)
00851 *        LENGTH  (60)
00852 *        INITIMG (GETMAIN-SPACE)
00853 *    END-EXEC.
           MOVE 60
             TO DFHEIV11
      *    MOVE ',"IL                  $   #00004889' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 GETMAIN-SPACE
           SET ADDRESS OF ACTIVITY-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00854
00855      MOVE 'AQ'                   TO AQ-RECORD-ID.
00856      MOVE PI-COMPANY-CD          TO AQ-COMPANY-CD.
00857      MOVE PI-CARRIER             TO AQ-CARRIER.
00858      MOVE PI-CLAIM-NO            TO AQ-CLAIM-NO.
00859      MOVE PI-CERT-NO             TO AQ-CERT-NO.
00860
00861      IF PI-FORMAT-OPTION = 'F'
00862          MOVE '1'                TO AQ-PENDING-STATUS-FLAG
00863      ELSE
00864          MOVE '2'                TO AQ-PENDING-STATUS-FLAG.
00865
00866      MOVE +0                     TO AQ-PAYMENT-COUNTER.
00867      MOVE 'ACTQ'                 TO FILE-SWITCH.
00868
00869      
      * EXEC CICS WRITE
00870 *        DATASET ('ELACTQ')
00871 *        FROM    (ACTIVITY-QUE)
00872 *        RIDFLD  (AQ-CONTROL-PRIMARY)
00873 *    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-QUE
             TO DFHEIV11
           MOVE 'ELACTQ' TO DFHEIV1
      *    MOVE '&$ L                  ''   #00004909' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034393039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ACTIVITY-QUE, 
                 DFHEIV11, 
                 AQ-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00874
00875      IF PRINT-IN-PROCESS
00876          ADD +1 TO PI-TS-COUNT.
00877
00878      GO TO 1299-EXIT.
00879
00880  1260-START-PRINTER.
00881      IF PI-FORMAT-OPTION = 'F'
00882          MOVE '2' TO PI-ENTRY-CD-1
00883      ELSE
00884          MOVE '1' TO PI-ENTRY-CD-1.
00885
00886  1265-START-PRINTER.
00887      IF FIRST-TIME-THRU
00888          IF PI-ALT-PRINT-ID = SPACES
00889              GO TO 1270-GET-PRINT-ID
00890          ELSE
00891              MOVE PI-ALT-PRINT-ID TO PI-PRINT-ID.
00892
00893      
      * EXEC CICS HANDLE CONDITION
00894 *        NOTOPEN (1275-CNTL-NOT-OPEN)
00895 *        NOTFND  (1280-NOT-FOUND)
00896 *        TERMIDERR  (1270-GET-PRINT-ID)
00897 *        TRANSIDERR (1285-TRANS-ERROR)
00898 *    END-EXEC.
      *    MOVE '"$JI[\                ! ) #00004933' TO DFHEIV0
           MOVE X'22244A495B5C202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303034393333' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00899
030612     IF PI-COMPANY-ID = 'DMD' OR 'CID' OR 'AHL'
00901 *        MOVE EIBTRMID       TO PI-PRINT-ID
00902          
      * EXEC CICS START
00903 *            TRANSID (START-TRANS-ID)
00904 *            TERMID  (PI-PRINT-ID)
00905 *            FROM    (PROGRAM-INTERFACE-BLOCK)
00906 *            LENGTH  (PI-COMM-LENGTH)
00907 *        END-EXEC
      *    MOVE '0( LF                 1   #00004942' TO DFHEIV0
           MOVE X'3028204C4620202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020203120' TO DFHEIV0(13:12)
           MOVE X'2020233030303034393432' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 START-TRANS-ID, 
                 DFHEIV99, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
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
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00908      ELSE
00909          
      * EXEC CICS START
00910 *            TRANSID (START-TRANS-ID)
00911 *            TERMID  (PI-PRINT-ID)
00912 *            FROM    (PROGRAM-INTERFACE-BLOCK)
00913 *            LENGTH  (PI-COMM-LENGTH)
00914 *        END-EXEC.
      *    MOVE '0( LFT                1   #00004949' TO DFHEIV0
           MOVE X'3028204C4654202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020203120' TO DFHEIV0(13:12)
           MOVE X'2020233030303034393439' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 START-TRANS-ID, 
                 DFHEIV99, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 PI-PRINT-ID, 
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
           
00915
00916      GO TO 1299-EXIT.
00917
00918  1270-GET-PRINT-ID.
00919      IF FIRST-TIME-THRU
00920          MOVE HIGH-VALUES TO WS-FIRST-TIME-SW
00921      ELSE
00922          MOVE ER-0412 TO EMI-ERROR
00923          PERFORM 9900-ERROR-FORMAT
00924             THRU 9900-ERROR-FORMAT-EXIT
00925          GO TO 8110-SEND-DATA.
00926
00927      MOVE PI-COMPANY-ID          TO COMPANY-ID.
00928      MOVE '1'                    TO RECORD-TYPE.
00929      MOVE SPACES                 TO CNTL-PROC.
00930      MOVE +0                     TO SEQ-NO.
00931      MOVE 'CNTL'                 TO FILE-SWITCH.
00932
00933      
      * EXEC CICS READ
00934 *        DATASET ('ELCNTL')
00935 *        SET     (ADDRESS OF CONTROL-FILE)
00936 *        RIDFLD  (CNTL-KEY)
00937 *    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S        E          (   #00004973' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034393733' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00938
00939      IF CF-FORMS-PRINTER-ID = SPACES
00940          MOVE ER-0337 TO EMI-ERROR
00941          PERFORM 9900-ERROR-FORMAT THRU 9900-ERROR-FORMAT-EXIT
00942          GO TO 8110-SEND-DATA
00943      ELSE
00944          MOVE CF-FORMS-PRINTER-ID TO PI-PRINT-ID
00945          GO TO 1265-START-PRINTER.
00946
00947  1275-CNTL-NOT-OPEN.
00948      MOVE ER-0042 TO EMI-ERROR.
00949      PERFORM 9900-ERROR-FORMAT
00950         THRU 9900-ERROR-FORMAT-EXIT.
00951      GO TO 8110-SEND-DATA.
00952
00953  1280-NOT-FOUND.
00954      MOVE ER-0190 TO EMI-ERROR.
00955      PERFORM 9900-ERROR-FORMAT
00956         THRU 9900-ERROR-FORMAT-EXIT.
00957
00958      GO TO 8110-SEND-DATA.
00959
00960  1285-TRANS-ERROR.
00961      MOVE ER-0413 TO EMI-ERROR.
00962      PERFORM 9900-ERROR-FORMAT
00963         THRU 9900-ERROR-FORMAT-EXIT.
00964
00965      GO TO 8110-SEND-DATA.
00966
00967  1299-EXIT.
00968      EXIT.
00969      EJECT
00970
00971  3000-GET-RECORD.
00972      IF PI-TS-COUNT GREATER THAN PI-TS-COUNT-1
00973         GO TO 3010-RECORD-NOT-FOUND.
00974
00975      
      * EXEC CICS HANDLE CONDITION
00976 *        ITEMERR (3010-RECORD-NOT-FOUND)
00977 *    END-EXEC.
      *    MOVE '"$<                   ! * #00005015' TO DFHEIV0
           MOVE X'22243C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2A20233030303035303135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00978
00979      
      * EXEC CICS READQ TS
00980 *        QUEUE (PI-EL1602-KEY)
00981 *        INTO (EL160BO)
00982 *        LENGTH (EL1602-LENGTH)
00983 *        ITEM (PI-TS-COUNT)
00984 *    END-EXEC.
      *    MOVE '*$II   L              ''   #00005019' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303139' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-EL1602-KEY, 
                 EL160BO, 
                 EL1602-LENGTH, 
                 PI-TS-COUNT, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00985
00986      GO TO 3020-GET-RECORD-EXIT.
00987
00988  3010-RECORD-NOT-FOUND.
00989      MOVE 'X'                    TO ERROR-SWITCH.
00990      MOVE ER-0130             TO EMI-ERROR.
00991      PERFORM 9900-ERROR-FORMAT
00992              THRU 9900-ERROR-FORMAT-EXIT.
00993      SUBTRACT NUM-ONE            FROM PI-TS-COUNT.
00994      GO TO 3000-GET-RECORD.
00995
00996  3020-GET-RECORD-EXIT.
00997      EXIT.
00998      EJECT
00999  4000-CHECK-UPDATE.
01000      MOVE SPACE                  TO UPDATE-SWITCH.
01001
01002      IF PRICDL GREATER THAN ZEROES
01003          PERFORM 4100-CHECK-PRI
01004              THRU 4100-CHECK-PRI-EXIT
01005      ELSE
01006          MOVE AL-UANOF           TO PRICDA.
01007
01008      IF SUPVL GREATER THAN ZEROES
01009          PERFORM 4110-CHECK-SUPR
01010              THRU 4110-CHECK-SUPR-EXIT
01011      ELSE
01012          MOVE AL-UANOF           TO SUPVA.
01013
01014      IF FILEL GREATER THAN ZEROES
01015          PERFORM 4120-CHECK-FILE
01016              THRU 4120-CHECK-FILE-EXIT
01017      ELSE
01018          MOVE AL-UANOF TO FILEA.
01019
01020      IF PROCL GREATER THAN ZEROES
01021          PERFORM 4130-CHECK-PROC
01022              THRU 4150-CHECK-PROC-EXIT
01023      ELSE
01024          MOVE AL-UANOF TO PROCA.
01025
01026      IF NOT NO-UPDATES
01027         GO TO 4000-CHECK-CAP.
01028
01029      IF NOSCRNL NOT = ZEROS
01030         GO TO 1000-BROWSE.
01031
01032      IF EIBAID = DFHPF1 OR DFHPF2 OR
01033                  DFHPF5 OR DFHPF6
01034         MOVE NUM-ONE TO PI-TS-COUNT
01035         GO TO 1100-CHECK-PFKEYS.
01036
01037      IF  EIBAID = DFHENTER
01038          MOVE 'X'                TO ERROR-SWITCH
01039          MOVE ER-0276         TO EMI-ERROR
01040          PERFORM 9900-ERROR-FORMAT
01041              THRU 9900-ERROR-FORMAT-EXIT
01042          MOVE LIT-IC             TO NOSCRNL
01043          GO TO 4000-CHECK-UPDATE-EXIT.
01044
01045  4000-CHECK-CAP.
01046      IF NOT MODIFY-CAP
01047          MOVE 'X'                TO ERROR-SWITCH
01048          MOVE ER-0070         TO EMI-ERROR
01049          PERFORM 9900-ERROR-FORMAT
01050              THRU 9900-ERROR-FORMAT-EXIT
01051          MOVE LIT-IC             TO NOSCRNL
01052          GO TO 4000-CHECK-UPDATE-EXIT.
01053
01054      PERFORM 4200-UPDATE-MSTR THRU 4210-UPDATE-MSTR-EXIT.
01055
01056      IF SCREEN-ERROR
01057          PERFORM 5000-UPDATE-TS
01058              THRU 5000-UPDATE-TS-EXIT
01059          MOVE LIT-IC             TO NOSCRNL
01060          MOVE ER-0068         TO EMI-ERROR
01061          PERFORM 9900-ERROR-FORMAT
01062              THRU 9900-ERROR-FORMAT-EXIT.
01063
01064  4000-CHECK-UPDATE-EXIT.
01065      EXIT.
01066      EJECT
01067  4100-CHECK-PRI.
01068      IF PRICDI GREATER THAN ZERO
01069        AND
01070         PRICDI NOT GREATER THAN '9'
01071          MOVE AL-UANON TO PRICDA
01072          MOVE 'X'                TO UPDATE-SWITCH
01073          GO TO 4100-CHECK-PRI-EXIT.
01074
01075      MOVE LIT-IC                 TO PRICDL.
01076      MOVE AL-UABON               TO PRICDA.
01077      MOVE ER-0274             TO EMI-ERROR.
01078      PERFORM 9900-ERROR-FORMAT
01079          THRU 9900-ERROR-FORMAT-EXIT.
01080
01081  4100-CHECK-PRI-EXIT.
01082      EXIT.
01083
01084  4110-CHECK-SUPR.
01085      IF SUPVI = 'Y' OR 'N' OR SPACES
01086          MOVE 'X'                TO UPDATE-SWITCH
01087          MOVE AL-UANON           TO SUPVA
01088          GO TO 4110-CHECK-SUPR-EXIT.
01089
01090      MOVE LIT-IC                 TO SUPVL.
01091      MOVE AL-UABON               TO SUPVA.
01092      MOVE ER-0230             TO EMI-ERROR.
01093      PERFORM 9900-ERROR-FORMAT
01094          THRU 9900-ERROR-FORMAT-EXIT.
01095
01096  4110-CHECK-SUPR-EXIT.
01097      EXIT.
01098
01099  4120-CHECK-FILE.
01100      IF FILEI NOT = SPACES
01101          MOVE 'X'                TO UPDATE-SWITCH
01102          GO TO 4120-CHECK-FILE-EXIT.
01103
01104  4120-CHECK-FILE-EXIT.
01105      EXIT.
01106
01107  4130-CHECK-PROC.
01108      
      * EXEC CICS HANDLE CONDITION
01109 *        NOTFND (4140-PROC-ERROR)
01110 *    END-EXEC.
      *    MOVE '"$I                   ! + #00005148' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2B20233030303035313438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01111
01112      MOVE PI-COMPANY-ID          TO COMPANY-ID.
01113      MOVE '2'                    TO RECORD-TYPE.
01114      MOVE PROCI                  TO CNTL-PROC.
01115      MOVE ZEROES                 TO SEQ-NO.
01116
01117      
      * EXEC CICS READ
01118 *        SET (ADDRESS OF CONTROL-FILE)
01119 *        DATASET ('ELCNTL')
01120 *        RIDFLD (CNTL-KEY)
01121 *    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S        E          (   #00005157' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313537' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01122
01123      MOVE 'X'                    TO KEY-SWITCH UPDATE-SWITCH.
01124      GO TO 4150-CHECK-PROC-EXIT.
01125
01126  4140-PROC-ERROR.
01127
01128      MOVE ER-0273 TO EMI-ERROR.
01129      MOVE LIT-IC                 TO PROCL.
01130      MOVE AL-UABON               TO PROCA.
01131      PERFORM 9900-ERROR-FORMAT
01132          THRU 9900-ERROR-FORMAT-EXIT.
01133
01134  4150-CHECK-PROC-EXIT.
01135      EXIT.
01136      EJECT
01137  4200-UPDATE-MSTR.
01138      MOVE PIKEYI         TO PI-CONTROL-IN-PROGRESS.
01139      MOVE SPACES         TO MSTR-KEY.
01140      MOVE PI-COMPANY-CD  TO MSTR-COMPANY-CODE.
01141
01142      MOVE PI-CARRIER     TO MSTR-CARRIER.
01143
01144      MOVE PI-CLAIM-NO    TO MSTR-CLAIM-NO.
01145      MOVE PI-CERT-NO     TO MSTR-CERT-NO.
01146
01147      
      * EXEC CICS READ
01148 *        INTO    (CLAIM-MASTER)
01149 *        DATASET (W-FILE-ID)
01150 *        RIDFLD  (MSTR-KEY)
01151 *        UPDATE
01152 *    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
      *    MOVE '&"IL       EU         (   #00005187' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313837' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-FILE-ID, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 MSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01153
01154      MOVE USERSAVI TO PI-UPDATE-BY.
01155      MOVE TIMESAVI TO PI-UPDATE-HHMMSS.
01156
01157      IF PI-UPDATE-BY NOT = CL-LAST-MAINT-USER
01158          PERFORM 4220-UNLOCK-MSTR
01159              THRU 4230-UNLOCK-MSTR-EXIT
01160          MOVE 'X'                TO ERROR-SWITCH
01161          GO TO 4210-UPDATE-MSTR-EXIT.
01162
01163      IF PI-UPDATE-HHMMSS NOT = CL-LAST-MAINT-HHMMSS
01164          PERFORM 4220-UNLOCK-MSTR
01165              THRU 4230-UNLOCK-MSTR-EXIT
01166          MOVE 'X'                TO ERROR-SWITCH
01167          GO TO 4210-UPDATE-MSTR-EXIT.
01168
01169      IF KEY-CHANGE
01170          PERFORM 4300-CHANGE-KEY
01171              THRU 4310-CHANGE-KEY-EXIT
01172      ELSE
01173          PERFORM 4320-UPDATE-RECORD
01174              THRU 4330-UPDATE-RECORD-EXIT.
01175
01176      MOVE PROCI                  TO HOLD-PROC.
01177      MOVE PRICDI                 TO HOLD-PRI.
01178      MOVE SUPVI                  TO HOLD-SUPV.
01179      MOVE FILEI                  TO HOLD-FILE.
01180
01181      PERFORM 3000-GET-RECORD THRU 3020-GET-RECORD-EXIT.
01182
01183      IF HOLD-PROC NOT = LOW-VALUES
01184          MOVE HOLD-PROC     TO PROCO.
01185      IF HOLD-PRI NOT = LOW-VALUES
01186          MOVE HOLD-PRI      TO PRICDO.
01187      IF HOLD-SUPV NOT = LOW-VALUES
01188          MOVE HOLD-SUPV     TO SUPVO.
01189      IF HOLD-FILE NOT = LOW-VALUES
01190          MOVE HOLD-FILE     TO FILEO.
01191
01192      MOVE SAVE-DATE         TO MNTDTO.
01193      MOVE LIT-UPDATE        TO MNTTYPEO.
01194      MOVE EIBTIME           TO TIMESAVO.
01195      MOVE PI-PROCESSOR-ID   TO USERSAVO.
01196
01197      PERFORM 4400-REWRITE-TS THRU 4410-REWRITE-TS-EXIT.
01198
01199      MOVE AL-UANOF TO PROCA
01200                       PRICDA
01201                       SUPVA
01202                       FILEA.
01203
01204  4210-UPDATE-MSTR-EXIT.
01205      EXIT.
01206
01207  4220-UNLOCK-MSTR.
01208      
      * EXEC CICS UNLOCK
01209 *        DATASET (W-FILE-ID)
01210 *    END-EXEC.
      *    MOVE '&*                    #   #00005248' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01211
01212  4230-UNLOCK-MSTR-EXIT.
01213      EXIT.
01214      EJECT
01215  4300-CHANGE-KEY.
01216      MOVE CLAIM-MASTER           TO  WS-OLD-CLAIM-RECORD.
01217
01218      
      * EXEC CICS DELETE
01219 *        DATASET (W-FILE-ID)
01220 *    END-EXEC.
      *    MOVE '&(                    &   #00005258' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323538' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-FILE-ID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01221
01222 *    EXEC CICS GETMAIN
01223 *        SET (ADDRESS OF CLAIM-MASTER)
01224 *        LENGTH (MSTR-LENGTH)
01225 *        INITIMG (GETMAIN-SPACE)
01226 *    END-EXEC.
01227
01228      MOVE WS-OLD-CLAIM-RECORD    TO  CLAIM-MASTER.
01229
01230      IF PROCI NOT = LOW-VALUES
01231          MOVE PROCI          TO CL-PROCESSOR-ID.
01232      IF PRICDI NOT = LOW-VALUES
01233          MOVE PRICDI         TO CL-PRIORITY-CD.
01234      IF SUPVI NOT = LOW-VALUES
01235          MOVE SUPVI          TO CL-SUPV-ATTN-CD.
01236      IF FILEI NOT = LOW-VALUES
01237          MOVE FILEI          TO CL-FILE-LOCATION.
01238
01239      MOVE SAVE-BIN-DATE      TO CL-LAST-MAINT-DT.
01240      MOVE PI-PROCESSOR-ID    TO CL-LAST-MAINT-USER.
01241      MOVE EIBTIME            TO CL-LAST-MAINT-HHMMSS.
01242      MOVE '3'                TO CL-LAST-MAINT-TYPE.
01243
01244      
      * EXEC CICS HANDLE CONDITION
01245 *        DUPKEY (4310-CHANGE-KEY-EXIT)
01246 *    END-EXEC.
      *    MOVE '"$$                   ! , #00005284' TO DFHEIV0
           MOVE X'222424202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2C20233030303035323834' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01247
01248      
      * EXEC CICS WRITE
01249 *        FROM    (CLAIM-MASTER)
01250 *        DATASET (W-FILE-ID)
01251 *        RIDFLD  (MSTR-KEY)
01252 *    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00005288' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323838' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-FILE-ID, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 MSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01253
01254  4310-CHANGE-KEY-EXIT.
01255      EXIT.
01256
01257  4320-UPDATE-RECORD.
01258      
      * EXEC CICS HANDLE CONDITION
01259 *        DUPKEY (4330-UPDATE-RECORD-EXIT)
01260 *    END-EXEC.
      *    MOVE '"$$                   ! - #00005298' TO DFHEIV0
           MOVE X'222424202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2D20233030303035323938' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01261
01262      IF PRICDI NOT = LOW-VALUES
01263          MOVE PRICDI TO CL-PRIORITY-CD.
01264      IF SUPVI NOT = LOW-VALUES
01265          MOVE SUPVI TO CL-SUPV-ATTN-CD.
01266      IF FILEI NOT = LOW-VALUES
01267          MOVE FILEI TO CL-FILE-LOCATION.
01268
01269      MOVE SAVE-BIN-DATE          TO CL-LAST-MAINT-DT.
01270      MOVE PI-PROCESSOR-ID        TO CL-LAST-MAINT-USER.
01271      MOVE EIBTIME                TO CL-LAST-MAINT-HHMMSS.
01272      MOVE '3'                    TO CL-LAST-MAINT-TYPE.
01273
01274      
      * EXEC CICS REWRITE
01275 *        FROM    (CLAIM-MASTER)
01276 *        DATASET (W-FILE-ID)
01277 *    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00005314' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333134' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-FILE-ID, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01278
01279  4330-UPDATE-RECORD-EXIT.
01280      EXIT.
01281      EJECT
01282  4400-REWRITE-TS.
01283      
      * EXEC CICS WRITEQ TS
01284 *        QUEUE (PI-EL1602-KEY)
01285 *        FROM (EL160BO)
01286 *        LENGTH (EL1602-LENGTH)
01287 *        ITEM (PI-TS-COUNT)
01288 *        REWRITE
01289 *    END-EXEC.
      *    MOVE '*" IR  L              ''   #00005323' TO DFHEIV0
           MOVE X'2A2220495220204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333233' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-EL1602-KEY, 
                 EL160BO, 
                 EL1602-LENGTH, 
                 PI-TS-COUNT, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01290
01291  4410-REWRITE-TS-EXIT.
01292      EXIT.
01293
01294      EJECT
01295  5000-UPDATE-TS.
01296      PERFORM 3000-GET-RECORD THRU 3020-GET-RECORD-EXIT.
01297      PERFORM 5010-MOVE-MSTR THRU 5010-MOVE-MSTR-EXIT.
01298      PERFORM 4400-REWRITE-TS THRU 4410-REWRITE-TS-EXIT.
01299
01300  5000-UPDATE-TS-EXIT.
01301      EXIT.
01302
01303  5010-MOVE-MSTR.
01304      MOVE CL-CLAIM-NO            TO CLAIMO PI-CLAIM-NO.
01305      MOVE CL-CLAIM-TYPE          TO TYPEO.
01306      MOVE CL-CERT-PRIME          TO CERTO.
01307      MOVE CL-CERT-SFX            TO CERTSXO.
01308
01309      MOVE CL-CCN                 TO CREDCDO.
01310
01311      MOVE CL-CERT-CARRIER        TO CARRO.
01312      MOVE CL-CLAIM-STATUS        TO STATUSO.
01313      MOVE CL-PROCESSOR-ID        TO PROCO.
01314      MOVE CL-INSURED-LAST-NAME   TO MLNAMEO.
01315      MOVE CL-INSURED-1ST-NAME    TO MFNAMEO.
01316      MOVE CL-INSURED-MID-INIT    TO MMINITO.
01317      MOVE CL-INSURED-SEX-CD      TO SEXO.
01318
01319      IF CL-INSURED-BIRTH-DT GREATER THAN LOW-VALUES
01320          MOVE CL-INSURED-BIRTH-DT   TO DC-BIN-DATE-1
01321          MOVE SPACES TO DC-OPTION-CODE DC-ERROR-CODE
01322          PERFORM 9800-CONVERT-DATE
01323              THRU 9800-CONVERT-DATE-EXIT
01324          MOVE DC-GREG-DATE-1-EDIT TO BIRTHO
01325      ELSE
01326          MOVE SPACES             TO BIRTHO.
01327
01328      MOVE CL-SOC-SEC-NO          TO SOCIALO.
01329      MOVE CL-INSURED-OCC-CD      TO OCCO.
01330
01331      IF SINGLE-PREMIUM
01332          MOVE LIT-SP             TO PREMSO
01333      ELSE
01334          IF O-B-COVERAGE
01335              MOVE LIT-OB         TO PREMSO
01336          ELSE
01337              IF OPEN-END-COVERAGE
01338                  MOVE LIT-OE     TO PREMSO
01339              ELSE
01340                  MOVE SPACES     TO PREMSO.
01341
01342      MOVE CL-CAUSE-CD            TO CAUSEO.
01343
01344      IF CL-EST-END-OF-DISAB-DT GREATER THAN LOW-VALUES
01345          MOVE CL-EST-END-OF-DISAB-DT TO DC-BIN-DATE-1
01346          MOVE SPACES TO DC-OPTION-CODE DC-ERROR-CODE
01347          PERFORM 9800-CONVERT-DATE
01348              THRU 9800-CONVERT-DATE-EXIT
01349          MOVE DC-GREG-DATE-1-EDIT TO ENDO
01350      ELSE
01351          MOVE SPACES             TO ENDO.
01352
01353      IF CL-PAID-THRU-DT GREATER THAN LOW-VALUES
01354          MOVE CL-PAID-THRU-DT    TO DC-BIN-DATE-1
01355          MOVE SPACES TO DC-OPTION-CODE DC-ERROR-CODE
01356          PERFORM 9800-CONVERT-DATE
01357              THRU 9800-CONVERT-DATE-EXIT
01358          MOVE DC-GREG-DATE-1-EDIT TO PDTHRUO
01359      ELSE
01360          MOVE SPACES TO PDTHRUO.
01361
01362      MOVE CL-TOTAL-PAID-AMT      TO PDAMTO.
01363      MOVE CL-NO-OF-DAYS-PAID     TO NODAYSO.
01364      MOVE CL-NO-OF-PMTS-MADE     TO NOPMTSO.
01365
01366      IF CL-INCURRED-DT GREATER THAN LOW-VALUES
01367          MOVE CL-INCURRED-DT     TO DC-BIN-DATE-1
01368          MOVE SPACES TO DC-OPTION-CODE DC-ERROR-CODE
01369          PERFORM 9800-CONVERT-DATE
01370              THRU 9800-CONVERT-DATE-EXIT
01371          MOVE DC-GREG-DATE-1-EDIT TO INCO
01372      ELSE
01373          MOVE SPACES             TO INCO.
01374
01375      IF CL-REPORTED-DT GREATER THAN LOW-VALUES
01376          MOVE CL-REPORTED-DT     TO DC-BIN-DATE-1
01377          MOVE SPACES TO DC-OPTION-CODE DC-ERROR-CODE
01378          PERFORM 9800-CONVERT-DATE
01379              THRU 9800-CONVERT-DATE-EXIT
01380          MOVE DC-GREG-DATE-1-EDIT TO REPO
01381      ELSE
01382          MOVE SPACES             TO REPO.
01383
01384      IF CL-FILE-ESTABLISH-DT GREATER THAN LOW-VALUES
01385          MOVE CL-FILE-ESTABLISH-DT TO DC-BIN-DATE-1
01386          MOVE SPACES TO DC-OPTION-CODE DC-ERROR-CODE
01387          PERFORM 9800-CONVERT-DATE
01388              THRU 9800-CONVERT-DATE-EXIT
01389          MOVE DC-GREG-DATE-1-EDIT TO ESTO
01390      ELSE
01391          MOVE SPACES             TO ESTO.
01392
01393 *    IF CL-LAST-PMT-DT GREATER THAN LOW-VALUES
01394 *        MOVE CL-LAST-PMT-DT TO DC-BIN-DATE-1
01395 *        MOVE SPACES TO DC-OPTION-CODE DC-ERROR-CODE
01396 *        PERFORM 9800-CONVERT-DATE
01397 *            THRU 9800-CONVERT-DATE-EXIT
01398 *        MOVE DC-GREG-DATE-1-EDIT TO LSTPMTO
01399 *    ELSE
01400 *        MOVE SPACES             TO LSTPMTO.
01401 *    MOVE CL-LAST-PMT-AMT        TO LSTAMTO.
01402
01403      IF CL-LAST-MAINT-DT GREATER THAN LOW-VALUES
01404          MOVE CL-LAST-MAINT-DT TO DC-BIN-DATE-1
01405          MOVE SPACES TO DC-OPTION-CODE DC-ERROR-CODE
01406          PERFORM 9800-CONVERT-DATE
01407              THRU 9800-CONVERT-DATE-EXIT
01408          MOVE DC-GREG-DATE-1-EDIT TO MNTDTO
01409      ELSE
01410          MOVE SPACES             TO MNTDTO.
01411
01412      IF CL-LAST-MAINT-TYPE = SPACE
01413         MOVE LIT-SET-UP         TO MNTTYPEO
01414      ELSE
01415      IF CL-LAST-MAINT-TYPE = '1'
01416         MOVE LIT-PMT             TO MNTTYPEO
01417      ELSE
01418      IF CL-LAST-MAINT-TYPE = '2'
01419         MOVE LIT-LETTER          TO MNTTYPEO
01420      ELSE
01421      IF CL-LAST-MAINT-TYPE = '3'
01422         MOVE LIT-UPDATE          TO MNTTYPEO
01423      ELSE
01424      IF CL-LAST-MAINT-TYPE = '4'
01425         MOVE LIT-RESTORE         TO MNTTYPEO
01426      ELSE
01427      IF CL-LAST-MAINT-TYPE = '5'
01428         MOVE LIT-INC-CHG         TO MNTTYPEO
01429      ELSE
01430      IF CL-LAST-MAINT-TYPE = '6'
01431         MOVE LIT-CONV            TO MNTTYPEO
01432      ELSE
01433         MOVE SPACES              TO MNTTYPEO.
01434
01435      MOVE CL-PRIORITY-CD         TO PRICDO.
01436      MOVE CL-SUPV-ATTN-CD        TO SUPVO.
01437      MOVE CL-FILE-LOCATION       TO FILEO.
01438      MOVE CL-LAST-MAINT-USER     TO PI-UPDATE-BY.
01439      MOVE CL-LAST-MAINT-HHMMSS   TO PI-UPDATE-HHMMSS.
01440      MOVE PI-CONTROL-IN-PROGRESS TO PIKEYO.
01441      MOVE PI-UPDATE-BY           TO USERSAVO.
01442      MOVE PI-UPDATE-HHMMSS       TO TIMESAVO.
01443
01444  5010-MOVE-MSTR-EXIT.
01445      EXIT.
01446      EJECT
01447  8100-SEND-MAP.
01448      PERFORM 8120-FORMAT-TIME-DATE
01449              THRU 8130-FORMAT-TIME-DATE-EXIT.
01450
01451      IF PI-RETRIEVAL-FILE
01452          MOVE '- CLAIM MASTER (RETRIEVAL) -'
01453                                  TO TITLEO
01454          MOVE AL-SANOF           TO PROCA
01455                                     FILEA
01456                                     PRICDA
01457                                     SUPVA.
01458
01459      MOVE EMI-ERROR-NUMBER (1) TO PI-LAST-ERROR-NO.
01460      MOVE EMI-MESSAGE-AREA (1) TO MSGBO.
01461
01462      
      * EXEC CICS SEND
01463 *        MAP ('EL160B')
01464 *        MAPSET ('EL160S')
01465 *        ERASE
01466 *        FREEKB
01467 *        CURSOR
01468 *    END-EXEC.
           MOVE 'EL160B' TO DFHEIV1
           MOVE -1
             TO DFHEIV11
           MOVE 'EL160S' TO DFHEIV2
      *    MOVE '8$     CT  E F  H     ,   #00005502' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202020202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353032' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EL160BO, 
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
           
01469
01470      GO TO 9000-RETURN-TRANS.
01471
01472  8110-SEND-DATA.
01473      PERFORM 8120-FORMAT-TIME-DATE
01474              THRU 8130-FORMAT-TIME-DATE-EXIT.
01475
01476      IF PI-RETRIEVAL-FILE
01477          MOVE '- CLAIM MASTER (RETRIEVAL) -'
01478                                  TO TITLEO
01479          MOVE AL-SANOF           TO PROCA
01480                                     FILEA
01481                                     PRICDA
01482                                     SUPVA.
01483
01484      MOVE EMI-ERROR-NUMBER (1) TO PI-LAST-ERROR-NO.
01485      MOVE EMI-MESSAGE-AREA (1) TO MSGBO.
01486
01487      
      * EXEC CICS SEND
01488 *        MAP ('EL160B')
01489 *        MAPSET ('EL160S')
01490 *        DATAONLY
01491 *        FREEKB
01492 *        CURSOR
01493 *    END-EXEC.
           MOVE 'EL160B' TO DFHEIV1
           MOVE -1
             TO DFHEIV11
           MOVE 'EL160S' TO DFHEIV2
      *    MOVE '8$D    CT    F  H     ,   #00005527' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'204620204820202020202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EL160BO, 
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
           
01494
01495      GO TO 9000-RETURN-TRANS.
01496
01497  8120-FORMAT-TIME-DATE.
01498      MOVE SAVE-DATE      TO DATEBO.
01499
01500      
      * EXEC CICS ASKTIME
01501 *        ABSTIME(LCP-CICS-TIME)
01502 *    END-EXEC.
      *    MOVE '0"A                   "   #00005540' TO DFHEIV0
           MOVE X'302241202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202220' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCP-CICS-TIME
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01503
01504      
      * EXEC CICS FORMATTIME
01505 *        ABSTIME(LCP-CICS-TIME)
01506 *        TIME(LCP-TIME-OF-DAY-XX)
01507 *    END-EXEC.
      *    MOVE 'j$(     (             #   #00005544' TO DFHEIV0
           MOVE X'6A2428202020202028202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCP-CICS-TIME, 
                 LCP-TIME-OF-DAY-XX
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01508
01509      MOVE LCP-TIME-OF-DAY-68 TO TIME-UNFORMATTED.
01510      MOVE UN-HOURS       TO FOR-HOURS.
01511      MOVE UN-MINUTES     TO FOR-MINUTES.
01512      MOVE TIME-FORMATTED TO TIMEBO.
01513      MOVE LIT-MAP        TO PI-CURRENT-SCREEN-NO.
01514
01515      IF EMI-ERROR NOT = 0515
01516         MOVE PI-TS-COUNT         TO CONV-COUNT
01517         MOVE CONV-COUNT          TO EDIT-COUNT
01518         MOVE EDIT-COUNT          TO NOSCRNO.
01519
01520      MOVE PI-TS-COUNT-1 TO CONV-COUNT.
01521      MOVE CONV-COUNT    TO EDIT-COUNT.
01522      MOVE EDIT-COUNT    TO TOTSCRNO.
01523
01524  8130-FORMAT-TIME-DATE-EXIT.
01525      EXIT.
01526
01527  8200-RETURN-PRIOR.
01528      MOVE PI-RETURN-TO-PROGRAM TO CALL-PGM.
01529      GO TO 9200-XCTL.
01530
01531  8300-GET-HELP.
01532      MOVE LIT-HELP TO CALL-PGM.
01533      GO TO 9200-XCTL.
01534
01535  8400-RETURN-MASTER.
01536      PERFORM 8700-DELETEQ THRU 8700-DELETEQ-EXIT.
01537      MOVE LIT-MASTER TO CALL-PGM.
01538      GO TO 9200-XCTL.
01539
01540  8500-GET-ACT.
01541      MOVE PIKEYI TO PI-CONTROL-IN-PROGRESS.
01542
01543      
      * EXEC CICS WRITEQ TS
01544 *        QUEUE (PI-KEY)
01545 *        FROM (PROGRAM-INTERFACE-BLOCK)
01546 *        LENGTH (PI-COMM-LENGTH)
01547 *    END-EXEC.
      *    MOVE '*"     L              ''   #00005583' TO DFHEIV0
           MOVE X'2A2220202020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353833' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-KEY, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01548
01549      MOVE LIT-ACT TO CALL-PGM.
01550      GO TO 9200-XCTL.
01551
01552  8700-DELETEQ.
01553       
      * EXEC CICS DELETEQ
01554 *        QUEUE (PI-EL160-KEY)
01555 *    END-EXEC.
      *    MOVE '*&                    #   #00005593' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-EL160-KEY, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01556       
      * EXEC CICS DELETEQ
01557 *        QUEUE (PI-EL1602-KEY)
01558 *    END-EXEC.
      *    MOVE '*&                    #   #00005596' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353936' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-EL1602-KEY, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01559
01560  8700-DELETEQ-EXIT.
01561      EXIT.
01562
01563  8800-UNAUTHORIZED-ACCESS.
01564      MOVE UNACCESS-MSG TO LOGOFF-MSG.
01565      GO TO 8990-SEND-TEXT.
01566
01567  8810-PF23-ENTERED.
01568      PERFORM 8700-DELETEQ THRU 8700-DELETEQ-EXIT.
01569      MOVE EIBAID TO PI-ENTRY-CD-1.
01570      MOVE LIT-SIGN-OFF TO CALL-PGM.
01571      GO TO 9200-XCTL.
01572
01573  8820-XCTL-ERROR.
01574      
      * EXEC CICS HANDLE CONDITION
01575 *        PGMIDERR (8990-SEND-TEXT)
01576 *    END-EXEC.
      *    MOVE '"$L                   ! . #00005614' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2E20233030303035363134' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01577
01578      MOVE SPACE        TO PI-ENTRY-CD-1.
01579      MOVE CALL-PGM     TO PI-CALLING-PROGRAM   LOGOFF-PGM
01580      MOVE PGMIDERR-MSG TO LOGOFF-FILL.
01581      MOVE LIT-SIGN-OFF TO CALL-PGM.
01582      GO TO 9200-XCTL.
01583
01584  8990-SEND-TEXT.
01585      
      * EXEC CICS SEND TEXT
01586 *        FROM (LOGOFF-TEXT)
01587 *        LENGTH (LOGOFF-LENGTH)
01588 *        ERASE
01589 *        FREEKB
01590 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00005625' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363235' TO DFHEIV0(25:11)
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
           
01591
01592      GO TO 9100-RETURN-CICS.
01593      EJECT
01594  9000-RETURN-TRANS.
01595      
      * EXEC CICS RETURN
01596 *        TRANSID (TRANS-ID)
01597 *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
01598 *        LENGTH (PI-COMM-LENGTH)
01599 *    END-EXEC.
      *    MOVE '.(CT                  ''   #00005635' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363335' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01600
01601      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1602' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
01602
01603  9100-RETURN-CICS.
01604      
      * EXEC CICS RETURN
01605 *    END-EXEC.
      *    MOVE '.(                    ''   #00005644' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01606
01607      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1602' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
01608
01609  9200-XCTL.
01610      
      * EXEC CICS XCTL
01611 *        PROGRAM (CALL-PGM)
01612 *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
01613 *        LENGTH (PI-COMM-LENGTH)
01614 *    END-EXEC.
      *    MOVE '.$C                   %   #00005650' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363530' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CALL-PGM, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01615
01616  9800-CONVERT-DATE.
01617      MOVE SPACE TO DC-ERROR-CODE.
01618
01619      
      * EXEC CICS LINK
01620 *        PROGRAM   (DATE-CONV)
01621 *        COMMAREA  (DATE-CONVERSION-DATA)
01622 *        LENGTH    (DC-COMM-LENGTH)
01623 *    END-EXEC.
      *    MOVE '."C                   (   #00005659' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363539' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DATE-CONV, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01624
01625  9800-CONVERT-DATE-EXIT.
01626      EXIT.
01627
01628  9900-ERROR-FORMAT.
01629      IF EMI-ERRORS-COMPLETE
01630          GO TO 9900-ERROR-FORMAT-EXIT.
01631
01632      
      * EXEC CICS LINK
01633 *        PROGRAM ('EL001')
01634 *        COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)
01635 *        LENGTH (EMI-COMM-LENGTH)
01636 *    END-EXEC.
           MOVE 'EL001' TO DFHEIV1
      *    MOVE '."C                   (   #00005672' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363732' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01637
01638  9900-ERROR-FORMAT-EXIT.
01639      EXIT.
01640
01641  9990-ABEND.
01642      
      * EXEC CICS LINK
01643 *        PROGRAM('EL004')
01644 *        COMMAREA(DFHEIBLK)
01645 *        LENGTH(64)
01646 *    END-EXEC.
           MOVE 'EL004' TO DFHEIV1
           MOVE 64
             TO DFHEIV11
      *    MOVE '."C                   (   #00005682' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIBLK, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01647
01648      GO TO 9100-RETURN-CICS.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1602' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 8820-XCTL-ERROR,
                     9990-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 0130-TS-ERROR,
                     0130-TS-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 0510-WRITE-INITIAL-TRAILER
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 0540-DELETE-REC
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 0560-READ-TEMP-STORAGE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 0570-DELETE-INITIAL-2
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 1230-CREATE-NEW-ACTQ
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 1275-CNTL-NOT-OPEN,
                     1280-NOT-FOUND,
                     1270-GET-PRINT-ID,
                     1285-TRANS-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 3010-RECORD-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 4140-PROC-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 12
               GO TO 4310-CHANGE-KEY-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 13
               GO TO 4330-UPDATE-RECORD-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 14
               GO TO 8990-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1602' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
