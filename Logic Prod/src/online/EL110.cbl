00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL110 .
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 03/02/95 10:36:31.
00007 *                            VMOD=2.006.
00008 *AUTHOR.        LOGIC, INC.
00009 *               DALLAS, TEXAS.
00010
00011 *DATE-COMPILED.
00012
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
00024 *REMARKS. TRANSACTION EX35 - ERROR FILE MAINTENANCE.
00025      EJECT
00026  ENVIRONMENT DIVISION.
00027  DATA DIVISION.
00028
00029  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00030  77  LCP-WS-ADDR-COMP              PIC S9(8) COMP.
00031  77  LCP-WS-ADDR-PNTR              REDEFINES LCP-WS-ADDR-COMP
00032                                    USAGE POINTER.
00033  01  LCP-TIME-OF-DAY-XX.
00034      05  LCP-TIME-OF-DAY-68        PIC 9(6).
00035      05  FILLER                    PIC 99.
00036  01  LCP-CICS-TIME                 PIC 9(15).
00037  77  FILLER  PIC X(32)  VALUE '********************************'.
00038  77  FILLER  PIC X(32)  VALUE '*    EL110 WORKING STORAGE     *'.
00039  77  FILLER  PIC X(32)  VALUE '*********** VMOD 2.006 *********'.
00040
00041  01  WS-DATE-AREA.
00042      05  SAVE-DATE           PIC X(8)    VALUE SPACES.
00043      05  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.
00044
00045  01  LITERALS-NUMBERS.
00046      12  LIT-SPACE               PIC X       VALUE SPACE.
00047      12  LIT-A                   PIC X       VALUE 'A'.
00048      12  LIT-B                   PIC X       VALUE 'B'.
00049      12  LIT-C                   PIC X       VALUE 'C'.
00050      12  LIT-D                   PIC X       VALUE 'D'.
00051      12  LIT-E                   PIC X       VALUE 'E'.
00052      12  LIT-F                   PIC X       VALUE 'F'.
00053      12  LIT-S                   PIC X       VALUE 'S'.
00054      12  LIT-X                   PIC X       VALUE 'X'.
00055      12  LIT-EM                  PIC XX      VALUE 'EM'.
00056      12  LIT-PGM                 PIC X(8)    VALUE 'EL110'.
00057      12  MAP-EL110A              PIC X(8)    VALUE 'EL110A'.
00058      12  LIT-SYS                 PIC X(4)    VALUE 'SYS'.
00059      12  LIT-TRAN                PIC X(4)    VALUE 'EX35'.
00060      12  LIT-MAP                 PIC X(4)    VALUE '110A'.
00061      12  LIT-HELP                PIC X(8)    VALUE 'EL010'.
00062      12  CALL-PGM                PIC X(8).
00063      12  XCTL-005                PIC X(8)    VALUE 'EL005'.
00064      12  XCTL-EL126              PIC X(8)    VALUE 'EL126'.
00065      12  XCTL-EL626              PIC X(8)    VALUE 'EL626'.
00066      12  XCTL-EM626              PIC X(8)    VALUE 'EM626'.
00067      12  XCTL-GL800              PIC X(8)    VALUE 'GL800'.
00068
00069  01  COUNT-FIELDS.
00070      12  COUNT-1                 PIC 99.
00071      12  COUNT-2                 PIC 99.
00072      12  COUNT-3                 PIC 99.
00073
00074  01  EDIT-WORK-AREA.
00075      12  CHECK-MAINT             PIC X.
00076          88  SHOW-OPTION                     VALUE 'S'.
00077          88  ADD-OPTION                      VALUE 'A'.
00078          88  CHANGE-OPTION                   VALUE 'C'.
00079          88  DELETE-OPTION                   VALUE 'D'.
00080          88  VALID-OPTION                  VALUE 'A' 'C' 'D' 'S'.
00081      12  CHECK-SEV               PIC X.
00082          88  VALID-SEV                     VALUE 'N' 'W' 'F' 'X'.
00083      12  CHECK-PFKEYS            PIC 99.
00084      12  SCREEN-SWITCH           PIC X.
00085          88  END-OF-FILE                     VALUE 'E'.
00086          88  SCREEN-FULL                     VALUE 'F'.
00087          88  SCREEN-ERROR                    VALUE 'X'.
00088      12  BROWSE-KEY              PIC X(4).
00089
00090  01  DISPLAY-LINE.
00091      12  ERR-CD                  PIC X(4).
00092      12  FILLER                  PIC X.
00093      12  ERR-SEV                 PIC X.
00094      12  FILLER                  PIC XX.
00095      12  ERR-TEXT                PIC X(65).
00096
00097  01  TIME-UNFORMATTED.
00098      12  UN-HOURS                PIC XX.
00099      12  UN-MINUTES              PIC XX.
00100      12  FILLER                  PIC X(4).
00101
00102  01  TIME-FORMATTED.
00103      12  FOR-HOURS               PIC XX.
00104      12  FILLER                  PIC X       VALUE '.'.
00105      12  FOR-MINUTES             PIC XX.
00106
00107  01  ERROR-MESSAGES.
00108      12  MAINT-MIS.
00109          16  FILLER              PIC X(39)
00110              VALUE 'MAINTENANCE FUNCTION INVALID OR UNAUTHO'.
00111          16  FILLER              PIC X(18)
00112              VALUE 'RIZED             '.
00113      12  LANG-MIS.
00114          16  FILLER              PIC X(39)
00115              VALUE 'LANGUAGE CODE INVALID OR NOT YET SUPPOR'.
00116          16  FILLER              PIC X(18)
00117              VALUE 'TED               '.
00118      12  ERROR-MIS               PIC X(46)
00119          VALUE 'ERROR NUMBER MISSING OR INVALID - PLEASE ENTER'.
00120      12  ERROR-INV               PIC X(38)
00121          VALUE 'ERROR NUMBER INVALID - PLEASE RE-ENTER'.
00122      12  ERROR-DUP               PIC X(40)
00123          VALUE 'ERROR NUMBER DUPLICATE - PLEASE RE-ENTER'.
00124      12  TEXT-MIS                PIC X(40)
00125          VALUE 'TEXT IS A MANDATORY FIELD - PLEASE ENTER'.
00126      12  SEV-MIS                 PIC X(42)
00127          VALUE 'SEVERITY MISSING OR INVALID - PLEASE ENTER'.
00128      12  INVALID-PFKEY           PIC X(45)
00129          VALUE 'INVALID PF-OPTION REQUESTED - PLEASE RE-ENTER'.
00130      12  END-MSG                 PIC X(11)
00131          VALUE 'END OF FILE'.
00132      12  END-UPDATE              PIC X(16)
00133          VALUE 'UPDATE COMPLETED'.
00134      12  SCREEN-TERM             PIC X(40)
00135            VALUE '     CLAS-IC ERROR MAINTENANCE COMPLETED'.
00136      12  FILE-MSG                PIC X(25)
00137            VALUE '     FILE ELERRS NOT OPEN'.
00138      12  FILE-MSG-FR             PIC X(25)
00139            VALUE '     FILE MPFERR NOT OPEN'.
00140
00141  01  COMP-LENGTHS.
00142      12  LIT-IC                  PIC S9(4)  COMP VALUE -1.
00143      12  FILE-LENGTH             PIC S9(4)  COMP VALUE +25.
00144      12  TERM-LENGTH             PIC S9(4)  COMP VALUE +40.
00145      12  COMM-LENGTH             PIC S9(4)  COMP VALUE +10.
00146      12  JOURNAL-LENGTH          PIC S9(4)  COMP VALUE +95.
00147      12  ERRS-LENGTH             PIC S9(4)  COMP VALUE +72.
00148
00149      EJECT
00150 *                                COPY ELCDATE.
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
00151      EJECT
00152 *                                COPY ELCERRS.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCERRS.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD 2.002                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = SYSTEM ERROR MESSAGES                     *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 72    RECFORM = FIX                            *
00011 *                                                                *
00012 *   BASE CLUSTER NAME = ELERRS                   RKP=2,LEN=4     *
00013 *       ALTERNATE PATH  = NOT USED                               *
00014 *                                                                *
00015 *   LOG = NO                                                     *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00017 ******************************************************************
00018  01  ERROR-MESSAGE-FILE.
00019      12  EM-RECORD-ID                PIC XX.
00020          88  VALID-EM-ID                VALUE 'EM'.
00021
00022      12  EM-CONTROL-PRIMARY.
00023          16  EM-MESSAGE-NUMBER       PIC 9(4).
00024
00025      12  EM-ERROR-TEXT.
00026          16  EM-ERROR-TEXT-PREFIX    PIC X(6).
00027          16  FILLER                  PIC X(59).
00028
00029      12  EM-ERROR-SEVERITY           PIC X.
00030          88  ERROR-IS-NOTE              VALUE 'N'.
00031          88  ERROR-IS-WARNING           VALUE 'W'.
00032          88  ERROR-IS-FORCABLE          VALUE 'F'.
00033          88  ERROR-IS-FATAL             VALUE 'X'.
00034
00035 ******************************************************************
00153      EJECT
00154 *                                COPY ELCATTR.
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
00155      EJECT
00156 *                                COPY ELCLOGOF.
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
00157      EJECT
00158
00159 *                                COPY ELCAID.
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
00160  01  FILLER REDEFINES DFHAID.
00161      12  FILLER                  PIC X(8).
00162      12  AID-KEYS OCCURS 24 TIMES.
00163          16  FILLER              PIC X.
00164      EJECT
00165 *                                COPY ELCJPFX.
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
00166                                  PIC X(72).
00167      EJECT
00168 *                                COPY ELCINTF.
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
00169      12  FILLER REDEFINES PI-PROGRAM-WORK-AREA.
00170          16  MENU-SWITCH         PIC X.
00171          16  SAVE-BEGIN          PIC X(4).
00172          16  SAVE-ENDING         PIC X(4).
00173          16  SHOW-SWITCH         PIC X.
00174              88   NOT-SHOWN                 VALUE 'X'.
00175          16  PI-FILE-ID          PIC X(8).
00176              88 PI-FRENCH-FILE              VALUE 'MPFERR'.
00177              88 PI-ENGLISH-FILE             VALUE 'ELERRS'.
00178          16  PI-POINTER          PIC S9(8)  COMP.
00179          16  FILLER              PIC X(618).
00180      EJECT
00181 *                                COPY EL110S.
       01  EL110AI.
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
           05  ERRORL PIC S9(0004) COMP.
           05  ERRORF PIC  X(0001).
           05  FILLER REDEFINES ERRORF.
               10  ERRORA PIC  X(0001).
           05  ERRORI PIC  X(0004).
      *    -------------------------------
           05  LANGL PIC S9(0004) COMP.
           05  LANGF PIC  X(0001).
           05  FILLER REDEFINES LANGF.
               10  LANGA PIC  X(0001).
           05  LANGI PIC  X(0001).
      *    -------------------------------
           05  TEXTL PIC S9(0004) COMP.
           05  TEXTF PIC  X(0001).
           05  FILLER REDEFINES TEXTF.
               10  TEXTA PIC  X(0001).
           05  TEXTI PIC  X(0065).
      *    -------------------------------
           05  SEVL PIC S9(0004) COMP.
           05  SEVF PIC  X(0001).
           05  FILLER REDEFINES SEVF.
               10  SEVA PIC  X(0001).
           05  SEVI PIC  X(0001).
           05  ERRMSGD OCCURS 11  TIMES.
      *    -------------------------------
               10  ERRMSGL PIC S9(0004) COMP.
               10  ERRMSGF PIC  X(0001).
               10  FILLER REDEFINES ERRMSGF.
                   15  ERRMSGA PIC  X(0001).
               10  ERRMSGI PIC  X(0079).
      *    -------------------------------
           05  MSGL PIC S9(0004) COMP.
           05  MSGF PIC  X(0001).
           05  FILLER REDEFINES MSGF.
               10  MSGA PIC  X(0001).
           05  MSGI PIC  X(0070).
      *    -------------------------------
           05  PFKEYL PIC S9(0004) COMP.
           05  PFKEYF PIC  X(0001).
           05  FILLER REDEFINES PFKEYF.
               10  PFKEYA PIC  X(0001).
           05  PFKEYI PIC  X(0002).
       01  EL110AO REDEFINES EL110AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TIMEO PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAINTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRORO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LANGO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TEXTO PIC  X(0065).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SEVO PIC  X(0001).
      *    -------------------------------
           05  ERRMSGD OCCURS 11  TIMES.
               10  FILLER        PIC  X(0003).
               10  ERRMSGO PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSGO PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFKEYO PIC  X(0002).
      *    -------------------------------
00182      EJECT
00183  01  ERROR-MESSAGE-FILE-REST     PIC X(72).
00184  01  ERROR-MESSAGE-FILE-FR       PIC X(72).
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
00186  01  DFHCOMMAREA                 PIC X(1024).
00187
00188 *01 PARM-LIST .
00189 *    12  FILLER                  PIC S9(8)  COMP.
00190 *    12  ERRS-PNT                PIC S9(8)  COMP.
00191 *    12  FERR-PNT                PIC S9(8)  COMP.
00192
00193 ******************************************************************
00194      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL110' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00196      CONTINUE.
00197
00198      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.
00199      MOVE EIBDATE                TO DC-JULIAN-YYDDD.
00200      MOVE '5'                    TO DC-OPTION-CODE.
00201      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
00202      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.
00203      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.
00204
00205      IF EIBCALEN = ZERO
00206          GO TO 8800-UNAUTHORIZED-ACCESS.
00207
00208      IF PI-CALLING-PROGRAM NOT = LIT-PGM
00209          IF PI-RETURN-TO-PROGRAM NOT = LIT-PGM
00210              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-6
00211              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-5
00212              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-4
00213              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-3
00214              MOVE PI-SAVED-PROGRAM-1    TO  PI-SAVED-PROGRAM-2
00215              MOVE PI-RETURN-TO-PROGRAM  TO  PI-SAVED-PROGRAM-1
00216              MOVE PI-CALLING-PROGRAM    TO  PI-RETURN-TO-PROGRAM
00217              MOVE LIT-PGM               TO  PI-CALLING-PROGRAM
00218              MOVE LOW-VALUES            TO  EL110AO
00219              GO TO 8100-SEND-MAP
00220          ELSE
00221              MOVE PI-RETURN-TO-PROGRAM  TO  PI-CALLING-PROGRAM
00222              MOVE PI-SAVED-PROGRAM-1    TO  PI-RETURN-TO-PROGRAM
00223              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-1
00224              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-2
00225              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-3
00226              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-4
00227              MOVE PI-SAVED-PROGRAM-6    TO  PI-SAVED-PROGRAM-5
00228              MOVE SPACES                TO  PI-SAVED-PROGRAM-6
00229              MOVE LOW-VALUES            TO  EL110AO
00230              GO TO 8100-SEND-MAP.
00231
00232      IF EIBAID = DFHCLEAR
00233          GO TO 9000-XCTL.
00234
00235      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00236          MOVE LIT-IC             TO PFKEYL
00237          MOVE INVALID-PFKEY      TO MSGO
00238          GO TO 8110-SEND-DATA.
00239
00240      
      * EXEC CICS RECEIVE
00241 *        MAP ('EL110A')
00242 *        MAPSET ('EL110S')
00243 *    END-EXEC.
           MOVE 'EL110A' TO DFHEIV1
           MOVE 'EL110S' TO DFHEIV2
      *    MOVE '8"T                   ''   #00001084' TO DFHEIV0
           MOVE X'382254202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031303834' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EL110AI, 
                 DFHEIV99, 
                 DFHEIV2, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00244
00245      MOVE SPACE                  TO SCREEN-SWITCH.
00246
00247      IF PFKEYL GREATER ZERO
00248          PERFORM 0300-TRANS-PF THRU 0310-EXIT.
00249
00250      IF SCREEN-ERROR
00251          MOVE LIT-IC             TO PFKEYL
00252          GO TO 8110-SEND-DATA.
00253
00254  0010-MAPFAIL-CONTINUE.
00255      IF EIBAID = DFHPF23
00256          GO TO 9100-XCTL.
00257
00258      IF EIBAID = DFHPF24
00259          GO TO 9200-XCTL.
00260
00261      IF  LANGL NOT EQUAL ZEROS
00262
00263          IF  LANGI EQUAL 'F'
00264              MOVE 'MPFERR'       TO PI-FILE-ID
00265              MOVE LCP-WS-ADDR-COMP TO PI-POINTER
00266
00267          ELSE
00268              MOVE 'ELERRS'       TO PI-FILE-ID
00269              MOVE LCP-WS-ADDR-COMP TO PI-POINTER
00270
00271              IF  LANGI EQUAL 'E' OR SPACES OR LOW-VALUES
00272                  MOVE 'E'        TO LANGI
00273
00274              ELSE
00275                  MOVE LIT-IC     TO LANGL
00276                  MOVE AL-UABON   TO LANGA
00277                  MOVE LANG-MIS   TO MSGO
00278                  MOVE LIT-X      TO SCREEN-SWITCH
00279                  GO TO 8110-SEND-DATA
00280
00281      ELSE
00282          MOVE 'ELERRS'           TO PI-FILE-ID
00283          MOVE LCP-WS-ADDR-COMP TO PI-POINTER.
00284
00285      IF EIBAID = DFHPF1
00286          GO TO 0100-PAGE-FORWARD.
00287
00288      IF EIBAID = DFHPF2
00289          GO TO 0200-PAGE-BACKWARD.
00290
00291      IF EIBAID = DFHPF12
00292          GO TO 8300-GET-HELP.
00293
00294      IF EIBAID NOT = DFHENTER
00295          MOVE INVALID-PFKEY      TO MSGO
00296          MOVE LIT-IC             TO MAINTL
00297          GO TO 8110-SEND-DATA.
00298
00299      PERFORM 0400-SET-ATTRB THRU 0410-EXIT.
00300      MOVE SPACE                  TO SCREEN-SWITCH.
00301      PERFORM 1000-EDIT-SCREEN THRU 1010-EXIT.
00302
00303      IF SCREEN-ERROR
00304          GO TO 8110-SEND-DATA.
00305
00306      PERFORM 1100-UPDATE-FILE THRU 1110-EXIT.
00307      MOVE LIT-IC                 TO MAINTL.
00308
00309      IF SHOW-OPTION
00310          MOVE SPACES             TO MSGO
00311          GO TO 8110-SEND-DATA
00312      ELSE
00313          MOVE LIT-X              TO SHOW-SWITCH
00314          MOVE LOW-VALUES         TO EL110AO
00315          MOVE LIT-IC             TO MAINTL
00316          MOVE END-UPDATE         TO MSGO
00317          MOVE HIGH-VALUES        TO SAVE-BEGIN
00318          MOVE LOW-VALUES         TO SAVE-ENDING
00319          GO TO 8100-SEND-MAP.
00320      EJECT
00321
00322  0100-PAGE-FORWARD.
00323      MOVE ZEROS                  TO COUNT-1.
00324      MOVE SPACES                 TO SCREEN-SWITCH MSGO.
00325
00326      IF ERRORL = ZEROS
00327          MOVE SAVE-ENDING        TO BROWSE-KEY
00328      ELSE
00329          MOVE AL-UNNOF           TO ERRORA
00330          MOVE ERRORI             TO BROWSE-KEY.
00331
00332      MOVE SPACES                 TO MAINTO
00333                                     TEXTO
00334                                     SEVO.
00335      
      * EXEC CICS HANDLE CONDITION
00336 *        NOTOPEN (8230-NOT-OPEN)
00337 *        NOTFND (0110-REC-NOT-FND)
00338 *    END-EXEC.
      *    MOVE '"$JI                  ! " #00001179' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303031313739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00339
00340      
      * EXEC CICS STARTBR
00341 *        DATASET (PI-FILE-ID)
00342 *        RIDFLD (BROWSE-KEY)
00343 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00001184' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031313834' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-FILE-ID, 
                 BROWSE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00344
00345      PERFORM 3000-BUILD-FWD-PAGE THRU 3020-EXIT
00346          UNTIL SCREEN-FULL OR END-OF-FILE.
00347
00348      
      * EXEC CICS ENDBR
00349 *        DATASET (PI-FILE-ID)
00350 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00001192' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303031313932' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00351
00352      MOVE LIT-IC                 TO MAINTL.
00353      GO TO 8110-SEND-DATA.
00354
00355  0110-REC-NOT-FND.
00356      MOVE HIGH-VALUES            TO SAVE-BEGIN SAVE-ENDING.
00357
00358      PERFORM 3100-FILL-SCREEN THRU 3110-EXIT
00359          UNTIL SCREEN-FULL.
00360      MOVE LIT-IC                 TO MAINTL.
00361      MOVE END-MSG                TO MSGO.
00362      GO TO 8110-SEND-DATA.
00363      EJECT
00364  0200-PAGE-BACKWARD.
00365      MOVE 11                     TO COUNT-1.
00366      MOVE SPACES                 TO SCREEN-SWITCH MSGO.
00367
00368      IF ERRORL = ZEROS
00369          MOVE SAVE-BEGIN         TO BROWSE-KEY
00370      ELSE
00371          MOVE AL-UNNOF           TO ERRORA
00372          MOVE ERRORI             TO BROWSE-KEY SAVE-BEGIN.
00373          MOVE SPACES             TO MAINTO
00374                                     TEXTO
00375                                     SEVO.
00376
00377      
      * EXEC CICS HANDLE CONDITION
00378 *        NOTOPEN (8230-NOT-OPEN)
00379 *        NOTFND (0210-REC-NOT-FND)
00380 *    END-EXEC.
      *    MOVE '"$JI                  ! # #00001221' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303031323231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00381
00382      
      * EXEC CICS STARTBR
00383 *        DATASET (PI-FILE-ID)
00384 *        RIDFLD (BROWSE-KEY)
00385 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00001226' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031323236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-FILE-ID, 
                 BROWSE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00386
00387      IF BROWSE-KEY = HIGH-VALUES
00388          GO TO 0205-SKIP-RESET.
00389
00390      
      * EXEC CICS READNEXT
00391 *        DATASET (PI-FILE-ID)
00392 *        RIDFLD (BROWSE-KEY)
00393 *        INTO (ERROR-MESSAGE-FILE)
00394 *    END-EXEC.
           MOVE LENGTH OF
            ERROR-MESSAGE-FILE
             TO DFHEIV12
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )   #00001234' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303031323334' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-FILE-ID, 
                 ERROR-MESSAGE-FILE, 
                 DFHEIV12, 
                 BROWSE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00395
00396      
      * EXEC CICS RESETBR
00397 *        DATASET (PI-FILE-ID)
00398 *        RIDFLD (BROWSE-KEY)
00399 *        EQUAL
00400 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&4         E          &   #00001240' TO DFHEIV0
           MOVE X'263420202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031323430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-FILE-ID, 
                 BROWSE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00401
00402  0205-SKIP-RESET.
00403      PERFORM 3030-BUILD-BCK-PAGE THRU 3050-EXIT
00404          UNTIL SCREEN-FULL OR END-OF-FILE.
00405
00406      
      * EXEC CICS ENDBR
00407 *        DATASET (PI-FILE-ID)
00408 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00001250' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303031323530' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00409
00410      MOVE COUNT-1                TO COUNT-3.
00411      PERFORM 3060-RAISE-PAGE THRU 3070-EXIT
00412          COUNT-3 TIMES.
00413      MOVE LIT-IC                 TO MAINTL.
00414      GO TO 8110-SEND-DATA.
00415
00416  0210-REC-NOT-FND.
00417      MOVE HIGH-VALUES            TO SAVE-BEGIN.
00418      MOVE ZEROS                  TO ERRORL.
00419      GO TO 0200-PAGE-BACKWARD.
00420      EJECT
00421  0300-TRANS-PF.
00422      IF EIBAID NOT = DFHENTER
00423          MOVE INVALID-PFKEY      TO MSGO
00424          MOVE LIT-X              TO SCREEN-SWITCH
00425          GO TO 0310-EXIT.
00426
00427      IF PFKEYI NOT NUMERIC
00428          MOVE INVALID-PFKEY      TO MSGO
00429          MOVE LIT-X              TO SCREEN-SWITCH
00430          GO TO 0310-EXIT.
00431
00432      IF PFKEYI LESS 1 OR GREATER 24
00433          MOVE INVALID-PFKEY      TO MSGO
00434          MOVE LIT-X              TO SCREEN-SWITCH
00435          GO TO 0310-EXIT.
00436
00437      MOVE PFKEYI                 TO CHECK-PFKEYS.
00438      MOVE AID-KEYS (CHECK-PFKEYS)                    TO EIBAID.
00439
00440  0310-EXIT.
00441      EXIT.
00442
00443  0400-SET-ATTRB.
00444      MOVE AL-UANON               TO MAINTA
00445                                     TEXTA
00446                                     LANGA
00447                                     SEVA.
00448      MOVE AL-UNNON               TO ERRORA.
00449
00450  0410-EXIT.
00451      EXIT.
00452
00453  1000-EDIT-SCREEN.
00454 *
00455 *    IF  LANGL NOT EQUAL ZEROS
00456 *
00457 *        IF  LANGI EQUAL 'F'
00458 *            MOVE 'MPFERR'       TO PI-FILE-ID
00459 *            MOVE FERR-PNT       TO PI-POINTER
00460 *
00461 *        ELSE
00462 *            MOVE 'ELERRS'       TO PI-FILE-ID
00463 *            MOVE ERRS-PNT       TO PI-POINTER
00464 *
00465 *            IF  LANGI EQUAL 'E' OR SPACES OR LOW-VALUES
00466 *                MOVE 'E'        TO LANGI
00467 *
00468 *            ELSE
00469 *                MOVE LIT-IC     TO LANGL
00470 *                MOVE AL-UABON   TO LANGA
00471 *                MOVE LANG-MIS   TO MSGO
00472 *                MOVE LIT-X      TO SCREEN-SWITCH
00473 *                GO TO 1010-EXIT
00474 *    ELSE
00475 *        MOVE 'ELERRS'           TO PI-FILE-ID
00476 *        MOVE ERRS-PNT           TO PI-POINTER.
00477
00478      MOVE MAINTI                 TO CHECK-MAINT.
00479
00480      IF PI-COMPANY-ID = 'MON'
00481       IF NOT VALID-OPTION
00482          MOVE LIT-IC             TO MAINTL
00483          MOVE AL-UABON           TO MAINTA
00484          MOVE MAINT-MIS          TO MSGO
00485          MOVE LIT-X              TO SCREEN-SWITCH
00486          GO TO 1010-EXIT.
00487
00488      IF PI-COMPANY-ID = 'LII'
00489             AND
00490         PI-PROCESSOR-ID EQUAL 'TPT'
00491         NEXT SENTENCE
00492
00493      ELSE
00494      IF PI-COMPANY-ID NOT = 'MON'
00495       IF (NOT VALID-OPTION)
CIDMOD               OR
CIDMOD        ((PI-PROCESSOR-ID NOT = 'EMER' AND 'PEMA')
CIDMOD        AND (NOT SHOW-OPTION))
00496 **      (PI-PROCESSOR-ID NOT = 'LGXX' AND NOT SHOW-OPTION)
00497          MOVE LIT-IC             TO MAINTL
00498          MOVE AL-UABON           TO MAINTA
00499          MOVE MAINT-MIS          TO MSGO
00500          MOVE LIT-X              TO SCREEN-SWITCH
00501          GO TO 1010-EXIT.
00502
00503      IF ERRORI = LOW-VALUES
00504          MOVE LIT-IC             TO ERRORL
00505          MOVE AL-UNBON           TO ERRORA
00506          MOVE ERROR-MIS          TO MSGO
00507          MOVE LIT-X              TO SCREEN-SWITCH
00508          GO TO 1010-EXIT.
00509
00510      IF DELETE-OPTION AND NOT-SHOWN
00511          MOVE SPACES             TO SHOW-SWITCH SCREEN-SWITCH
00512          MOVE ZEROES             TO COUNT-1
00513          PERFORM 3100-FILL-SCREEN THRU 3110-EXIT
00514              UNTIL SCREEN-FULL
00515          MOVE LIT-S              TO CHECK-MAINT.
00516
00517      IF CHANGE-OPTION AND NOT-SHOWN
00518          MOVE SPACES             TO SHOW-SWITCH SCREEN-SWITCH
00519          MOVE ZEROES             TO COUNT-1
00520          PERFORM 3100-FILL-SCREEN THRU 3110-EXIT
00521              UNTIL SCREEN-FULL
00522          MOVE LIT-S              TO CHECK-MAINT.
00523
00524      PERFORM 1020-VERIFY-ERROR THRU 1050-EXIT.
00525
00526      IF SCREEN-ERROR OR DELETE-OPTION
00527          GO TO 1010-EXIT.
00528
00529      IF SHOW-OPTION
00530          MOVE SPACES             TO SHOW-SWITCH SCREEN-SWITCH
00531          MOVE ZEROES             TO COUNT-1
00532          PERFORM 3100-FILL-SCREEN THRU 3110-EXIT
00533              UNTIL SCREEN-FULL
00534          GO TO 1010-EXIT.
00535
00536      IF TEXTI = SPACES OR LOW-VALUES
00537          MOVE LIT-IC             TO TEXTL
00538          MOVE AL-UABON           TO TEXTA
00539          MOVE TEXT-MIS           TO MSGO
00540          MOVE LIT-X              TO SCREEN-SWITCH
00541          GO TO 1010-EXIT.
00542
00543      MOVE SEVI                   TO CHECK-SEV.
00544
00545      IF NOT VALID-SEV
00546          MOVE LIT-IC             TO SEVL
00547          MOVE AL-UABON           TO SEVA
00548          MOVE SEV-MIS            TO MSGO
00549          MOVE LIT-X              TO SCREEN-SWITCH.
00550
00551  1010-EXIT.
00552      EXIT.
00553      EJECT
00554  1020-VERIFY-ERROR.
00555      
      * EXEC CICS HANDLE CONDITION
00556 *        NOTOPEN (1040-RECORD-NOT-FOUND)
00557 *        NOTFND (1040-RECORD-NOT-FOUND)
00558 *    END-EXEC.
      *    MOVE '"$JI                  ! $ #00001402' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303031343032' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00559
00560      MOVE ERRORI TO BROWSE-KEY.
00561      
      * EXEC CICS READ
00562 *        DATASET (PI-FILE-ID)
00563 *        RIDFLD (BROWSE-KEY)
00564 *        INTO (ERROR-MESSAGE-FILE)
00565 *    END-EXEC.
           MOVE LENGTH OF
            ERROR-MESSAGE-FILE
             TO DFHEIV11
      *    MOVE '&"IL       E          (   #00001408' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303031343038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-FILE-ID, 
                 ERROR-MESSAGE-FILE, 
                 DFHEIV11, 
                 BROWSE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00566 *
00567 *    IF  PI-FRENCH-FILE
00568 *        SERVICE RELOAD ERROR-MESSAGE-FILE-FR
00569 *
00570 *    ELSE
00571 *        SERVICE RELOAD ERROR-MESSAGE-FILE-REST.
00572
00573      IF ADD-OPTION
00574          MOVE ERROR-DUP          TO MSGO
00575          MOVE AL-UNBON           TO ERRORA
00576          MOVE LIT-IC             TO ERRORL
00577          MOVE LIT-X              TO SCREEN-SWITCH
00578          GO TO 1050-EXIT.
00579
00580      IF NOT SHOW-OPTION
00581          GO TO 1050-EXIT.
00582
00583      MOVE EM-ERROR-TEXT          TO TEXTO.
00584      MOVE EM-ERROR-SEVERITY      TO SEVO.
00585      GO TO 1050-EXIT.
00586
00587  1040-RECORD-NOT-FOUND.
00588      IF NOT ADD-OPTION
00589          MOVE ERROR-INV          TO MSGO
00590          MOVE AL-UNBON           TO ERRORA
00591          MOVE LIT-IC             TO ERRORL
00592          MOVE LIT-X              TO SCREEN-SWITCH.
00593
00594  1050-EXIT.
00595      EXIT.
00596      EJECT
00597  1100-UPDATE-FILE.
00598      IF SHOW-OPTION
00599          GO TO 1110-EXIT.
00600
00601      IF ADD-OPTION
00602          PERFORM 1120-ADD-OPTION THRU 1140-EXIT.
00603
00604      IF CHANGE-OPTION
00605          PERFORM 1150-CHANGE-OPTION THRU 1160-EXIT.
00606
00607      IF DELETE-OPTION
00608          PERFORM 1170-DELETE-OPTION THRU 1180-EXIT.
00609
00610  1110-EXIT.
00611      EXIT.
00612
00613  1120-ADD-OPTION.
00614      
      * EXEC CICS HANDLE CONDITION
00615 *        NOTOPEN (8230-NOT-OPEN)
00616 *        NOSPACE (1130-FILE-FULL)
00617 *    END-EXEC.
      *    MOVE '"$JE                  ! % #00001461' TO DFHEIV0
           MOVE X'22244A452020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303031343631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00618
00619      
      * EXEC CICS GETMAIN
00620 *        SET (PI-POINTER)
00621 *        LENGTH (ERRS-LENGTH)
00622 *        INITIMG (LIT-SPACE)
00623 *    END-EXEC.
      *    MOVE ',"IL                  $   #00001466' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303031343636' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-POINTER, 
                 ERRS-LENGTH, 
                 LIT-SPACE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00624
00625      IF  PI-FRENCH-FILE
00626          CONTINUE
00627
00628      ELSE
00629          CONTINUE.
00630
00631      MOVE SPACES                 TO ERROR-MESSAGE-FILE.
00632      MOVE LIT-EM                 TO EM-RECORD-ID.
00633      MOVE ERRORI                 TO EM-MESSAGE-NUMBER BROWSE-KEY.
00634      MOVE TEXTI                  TO EM-ERROR-TEXT.
00635      MOVE SEVI                   TO EM-ERROR-SEVERITY.
00636      MOVE LIT-A                  TO JP-RECORD-TYPE.
00637      MOVE ERROR-MESSAGE-FILE     TO JP-RECORD-AREA.
00638      
      * EXEC CICS WRITE
00639 *        DATASET (PI-FILE-ID)
00640 *        FROM (ERROR-MESSAGE-FILE)
00641 *        RIDFLD (BROWSE-KEY)
00642 *    END-EXEC.
           MOVE LENGTH OF
            ERROR-MESSAGE-FILE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00001485' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031343835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-FILE-ID, 
                 ERROR-MESSAGE-FILE, 
                 DFHEIV11, 
                 BROWSE-KEY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00643
00644      PERFORM 2000-JOURNAL-WRITE THRU 2010-EXIT.
00645
00646  1130-FILE-FULL.
00647      MOVE LIT-X                  TO SCREEN-SWITCH.
00648
00649  1140-EXIT.
00650      EXIT.
00651      EJECT
00652  1150-CHANGE-OPTION.
00653      
      * EXEC CICS HANDLE CONDITION
00654 *        NOTOPEN (8230-NOT-OPEN)
00655 *    END-EXEC.
      *    MOVE '"$J                   ! & #00001500' TO DFHEIV0
           MOVE X'22244A202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303031353030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00656
00657      MOVE ERRORI                 TO BROWSE-KEY.
00658      PERFORM 1190-READ-FILE THRU 1200-EXIT.
00659      MOVE LIT-B                  TO JP-RECORD-TYPE.
00660      MOVE ERROR-MESSAGE-FILE     TO JP-RECORD-AREA.
00661      PERFORM 2000-JOURNAL-WRITE THRU 2010-EXIT.
00662      MOVE TEXTI                  TO EM-ERROR-TEXT.
00663      MOVE SEVI                   TO EM-ERROR-SEVERITY.
00664      MOVE LIT-C                  TO JP-RECORD-TYPE.
00665      MOVE ERROR-MESSAGE-FILE     TO JP-RECORD-AREA.
00666      
      * EXEC CICS REWRITE
00667 *        DATASET (PI-FILE-ID)
00668 *        FROM (ERROR-MESSAGE-FILE)
00669 *    END-EXEC.
           MOVE LENGTH OF
            ERROR-MESSAGE-FILE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00001513' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303031353133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-FILE-ID, 
                 ERROR-MESSAGE-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00670
00671      PERFORM 2000-JOURNAL-WRITE THRU 2010-EXIT.
00672
00673  1160-EXIT.
00674      EXIT.
00675
00676  1170-DELETE-OPTION.
00677      
      * EXEC CICS HANDLE CONDITION
00678 *        NOTOPEN (8230-NOT-OPEN)
00679 *    END-EXEC.
      *    MOVE '"$J                   ! '' #00001524' TO DFHEIV0
           MOVE X'22244A202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303031353234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00680      MOVE ERRORI                 TO BROWSE-KEY.
00681      PERFORM 1190-READ-FILE THRU 1200-EXIT.
00682      MOVE LIT-D                  TO JP-RECORD-TYPE.
00683      MOVE ERROR-MESSAGE-FILE     TO JP-RECORD-AREA.
00684      
      * EXEC CICS DELETE
00685 *        DATASET (PI-FILE-ID)
00686 *    END-EXEC.
      *    MOVE '&(                    &   #00001531' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031353331' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-FILE-ID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00687      PERFORM 2000-JOURNAL-WRITE THRU 2010-EXIT.
00688  1180-EXIT.
00689      EXIT.
00690
00691  1190-READ-FILE.
00692      
      * EXEC CICS READ
00693 *        DATASET (PI-FILE-ID)
00694 *        RIDFLD  (BROWSE-KEY)
00695 *        INTO    (ERROR-MESSAGE-FILE)
00696 *        UPDATE
00697 *    END-EXEC.
           MOVE LENGTH OF
            ERROR-MESSAGE-FILE
             TO DFHEIV11
      *    MOVE '&"IL       EU         (   #00001539' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303031353339' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-FILE-ID, 
                 ERROR-MESSAGE-FILE, 
                 DFHEIV11, 
                 BROWSE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00698 *
00699 *    IF  PI-FRENCH-FILE
00700 *        SERVICE RELOAD ERROR-MESSAGE-FILE-FR
00701 *
00702 *    ELSE
00703 *        SERVICE RELOAD ERROR-MESSAGE-FILE-REST.
00704
00705  1200-EXIT.
00706      EXIT.
00707
00708  2000-JOURNAL-WRITE.
00709      MOVE LIT-SYS                TO JP-USER-ID.
00710      MOVE PI-FILE-ID             TO JP-FILE-ID.
00711      MOVE LIT-PGM                TO JP-PROGRAM-ID.
pemuni*    IF PI-JOURNAL-FILE-ID NOT = ZERO
pemuni*        EXEC CICS JOURNAL
pemuni*            JFILEID (1)
pemuni*            JTYPEID ('EL')
pemuni*            FROM    (JOURNAL-RECORD)
pemuni*            LENGTH  (JOURNAL-LENGTH)
pemuni*        END-EXEC.
00719
00720  2010-EXIT.
00721      EXIT.
00722      EJECT
00723  3000-BUILD-FWD-PAGE.
00724      
      * EXEC CICS HANDLE CONDITION
00725 *        ENDFILE (3010-END-FILE)
00726 *        NOTFND  (3010-END-FILE)
00727 *    END-EXEC.
      *    MOVE '"$''I                  ! ( #00001571' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303031353731' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00728
00729      
      * EXEC CICS READNEXT
00730 *        DATASET (PI-FILE-ID)
00731 *        RIDFLD (BROWSE-KEY)
00732 *        INTO (ERROR-MESSAGE-FILE)
00733 *    END-EXEC.
           MOVE LENGTH OF
            ERROR-MESSAGE-FILE
             TO DFHEIV12
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )   #00001576' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303031353736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-FILE-ID, 
                 ERROR-MESSAGE-FILE, 
                 DFHEIV12, 
                 BROWSE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00734 *
00735 *    IF  PI-FRENCH-FILE
00736 *        SERVICE RELOAD ERROR-MESSAGE-FILE-FR
00737 *
00738 *    ELSE
00739 *        SERVICE RELOAD ERROR-MESSAGE-FILE-REST.
00740
00741      MOVE SPACES                 TO DISPLAY-LINE.
00742      MOVE EM-CONTROL-PRIMARY     TO ERR-CD.
00743      MOVE EM-ERROR-SEVERITY      TO ERR-SEV.
00744      MOVE EM-ERROR-TEXT          TO ERR-TEXT.
00745      ADD 1 TO COUNT-1.
00746      MOVE DISPLAY-LINE           TO ERRMSGO (COUNT-1).
00747
00748      IF COUNT-1 = 1
00749          MOVE ERR-CD             TO SAVE-BEGIN.
00750
00751      IF COUNT-1 = 11
00752          MOVE ERR-CD             TO SAVE-ENDING
00753          MOVE LIT-F              TO SCREEN-SWITCH.
00754
00755      GO TO 3020-EXIT.
00756
00757  3010-END-FILE.
00758      PERFORM 3100-FILL-SCREEN THRU 3110-EXIT
00759          UNTIL SCREEN-FULL.
00760      MOVE ERR-CD                 TO SAVE-ENDING.
00761      MOVE END-MSG                TO MSGO.
00762      MOVE LIT-E                  TO SCREEN-SWITCH.
00763
00764  3020-EXIT.
00765      EXIT.
00766      EJECT
00767  3030-BUILD-BCK-PAGE.
00768      
      * EXEC CICS HANDLE CONDITION
00769 *        ENDFILE (3040-END-FILE)
00770 *        NOTFND  (3040-END-FILE)
00771 *    END-EXEC.
      *    MOVE '"$''I                  ! ) #00001615' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303031363135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00772
00773      
      * EXEC CICS READPREV
00774 *        DATASET (PI-FILE-ID)
00775 *        RIDFLD  (BROWSE-KEY)
00776 *        INTO (ERROR-MESSAGE-FILE)
00777 *    END-EXEC.
           MOVE LENGTH OF
            ERROR-MESSAGE-FILE
             TO DFHEIV12
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0IL                  )   #00001620' TO DFHEIV0
           MOVE X'2630494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303031363230' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-FILE-ID, 
                 ERROR-MESSAGE-FILE, 
                 DFHEIV12, 
                 BROWSE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00778 *
00779 *    IF  PI-FRENCH-FILE
00780 *        SERVICE RELOAD ERROR-MESSAGE-FILE-FR
00781 *
00782 *    ELSE
00783 *        SERVICE RELOAD ERROR-MESSAGE-FILE-REST.
00784
00785      IF EM-MESSAGE-NUMBER GREATER SAVE-BEGIN
00786          GO TO 3050-EXIT.
00787
00788      MOVE SPACES                 TO DISPLAY-LINE.
00789      MOVE EM-CONTROL-PRIMARY     TO ERR-CD.
00790      MOVE EM-ERROR-SEVERITY      TO ERR-SEV.
00791      MOVE EM-ERROR-TEXT          TO ERR-TEXT.
00792      MOVE DISPLAY-LINE           TO ERRMSGO (COUNT-1).
00793
00794      IF COUNT-1 = 11
00795          MOVE ERR-CD             TO SAVE-ENDING.
00796
00797      IF COUNT-1 = 1
00798          MOVE ERR-CD             TO SAVE-BEGIN
00799          MOVE LIT-F              TO SCREEN-SWITCH.
00800
00801      SUBTRACT 1 FROM COUNT-1.
00802      GO TO 3050-EXIT.
00803
00804  3040-END-FILE.
00805      MOVE ERR-CD                 TO SAVE-BEGIN.
00806      MOVE END-MSG                TO MSGO.
00807      MOVE LIT-E                  TO SCREEN-SWITCH.
00808
00809  3050-EXIT.
00810      EXIT.
00811      EJECT
00812  3060-RAISE-PAGE.
00813      MOVE 1                      TO COUNT-1.
00814      MOVE 2                      TO COUNT-2.
00815      PERFORM 3080-SHIFT-SCREEN THRU 3090-EXIT
00816           UNTIL COUNT-1 = 11.
00817      MOVE SPACES                 TO ERRMSGO (COUNT-1).
00818
00819  3070-EXIT.
00820      EXIT.
00821
00822  3080-SHIFT-SCREEN.
00823      MOVE ERRMSGO (COUNT-2)      TO ERRMSGO (COUNT-1).
00824      ADD 1 TO COUNT-1 COUNT-2.
00825
00826  3090-EXIT.
00827      EXIT.
00828
00829  3100-FILL-SCREEN.
00830      ADD 1 TO COUNT-1.
00831      MOVE SPACES                 TO ERRMSGO (COUNT-1).
00832
00833      IF COUNT-1 GREATER 10
00834          MOVE LIT-F TO SCREEN-SWITCH.
00835
00836  3110-EXIT.
00837      EXIT.
00838      EJECT
00839  8100-SEND-MAP.
00840      PERFORM 8130-FORMAT-DATE-TIME THRU 8140-EXIT.
00841      MOVE LIT-IC                 TO MAINTL.
00842      
      * EXEC CICS SEND
00843 *        MAP    ('EL110A')
00844 *        MAPSET ('EL110S')
00845 *        ERASE
00846 *        FREEKB
00847 *        CURSOR
00848 *    END-EXEC.
           MOVE 'EL110A' TO DFHEIV1
           MOVE -1
             TO DFHEIV11
           MOVE 'EL110S' TO DFHEIV2
      *    MOVE '8$     CT  E F  H     ,   #00001689' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202020202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303031363839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EL110AO, 
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
           
00849
00850      GO TO 8120-RETURN-TRANS.
00851
00852  8110-SEND-DATA.
00853      PERFORM 8130-FORMAT-DATE-TIME THRU 8140-EXIT.
00854      
      * EXEC CICS SEND
00855 *        MAP    ('EL110A')
00856 *        MAPSET ('EL110S')
00857 *        DATAONLY
00858 *        ERASEAUP
00859 *        FREEKB
00860 *        CURSOR
00861 *    END-EXEC.
           MOVE 'EL110A' TO DFHEIV1
           MOVE -1
             TO DFHEIV11
           MOVE 'EL110S' TO DFHEIV2
      *    MOVE '8$D    CT  A F  H     ,   #00001701' TO DFHEIV0
           MOVE X'382444202020204354202041' TO DFHEIV0(1:12)
           MOVE X'204620204820202020202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373031' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EL110AO, 
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
           
00862
00863  8120-RETURN-TRANS.
00864      
      * EXEC CICS RETURN
00865 *        TRANSID  (LIT-TRAN)
00866 *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
00867 *        LENGTH   (PI-COMM-LENGTH)
00868 *    END-EXEC.
      *    MOVE '.(CT                  &   #00001711' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LIT-TRAN, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00869      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL110' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
00870
00871  8130-FORMAT-DATE-TIME.
00872      MOVE SAVE-DATE              TO DATEO.
00873      
      * EXEC CICS ASKTIME ABSTIME(LCP-CICS-TIME)
00874 *    END-EXEC
      *    MOVE '0"A                   "   #00001720' TO DFHEIV0
           MOVE X'302241202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202220' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373230' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCP-CICS-TIME
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00875      
      * EXEC CICS FORMATTIME
00876 *              ABSTIME(LCP-CICS-TIME)
00877 *              TIME(LCP-TIME-OF-DAY-XX)
00878 *    END-EXEC
      *    MOVE 'j$(     (             #   #00001722' TO DFHEIV0
           MOVE X'6A2428202020202028202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCP-CICS-TIME, 
                 LCP-TIME-OF-DAY-XX
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00879      MOVE  LCP-TIME-OF-DAY-68 TO TIME-UNFORMATTED.
00880      MOVE UN-HOURS               TO FOR-HOURS.
00881      MOVE UN-MINUTES             TO FOR-MINUTES.
00882      MOVE TIME-FORMATTED         TO TIMEO.
00883      MOVE LIT-MAP                TO PI-CURRENT-SCREEN-NO.
00884
00885  8140-EXIT.
00886      EXIT.
00887
00888
00889  8220-RETURN-CICS.
00890      
      * EXEC CICS RETURN
00891 *    END-EXEC.
      *    MOVE '.(                    &   #00001737' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373337' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00892      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL110' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
00893
00894  8230-NOT-OPEN.
00895      IF  PI-FRENCH-FILE
00896          
      * EXEC CICS SEND TEXT
00897 *            FROM (FILE-MSG-FR)
00898 *            LENGTH (FILE-LENGTH)
00899 *            ERASE
00900 *            FREEKB
00901 *        END-EXEC
      *    MOVE '8&      T  E F  H   F -   #00001743' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-MSG-FR, 
                 FILE-LENGTH, 
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
00902
00903      ELSE
00904          
      * EXEC CICS SEND TEXT
00905 *            FROM (FILE-MSG)
00906 *            LENGTH (FILE-LENGTH)
00907 *            ERASE
00908 *            FREEKB
00909 *        END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00001751' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373531' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-MSG, 
                 FILE-LENGTH, 
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
           
00910
00911      GO TO 8220-RETURN-CICS.
00912
00913  8300-GET-HELP.
00914      MOVE LIT-HELP TO CALL-PGM.
00915      GO TO 9400-XCTL.
00916
00917      EJECT
00918  8800-UNAUTHORIZED-ACCESS.
00919      MOVE UNACCESS-MSG           TO  LOGOFF-MSG.
00920      GO TO 8800-SEND-TEXT.
00921
00922  8800-SEND-TEXT.
00923      
      * EXEC CICS SEND TEXT
00924 *        FROM    (LOGOFF-TEXT)
00925 *        LENGTH  (LOGOFF-LENGTH)
00926 *        ERASE
00927 *        FREEKB
00928 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00001770' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373730' TO DFHEIV0(25:11)
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
           
00929
00930      GO TO 8220-RETURN-CICS.
00931
00932  EJECT
00933  9000-XCTL.
00934      MOVE PI-RETURN-TO-PROGRAM   TO CALL-PGM.
00935      GO TO 9400-XCTL.
00936
00937  9100-XCTL.
00938      MOVE XCTL-005               TO CALL-PGM.
00939      MOVE EIBAID                 TO PI-ENTRY-CD-1.
00940      GO TO 9400-XCTL.
00941
00942  9200-XCTL.
00943
00944      IF  CREDIT-SESSION
00945          MOVE XCTL-EL626         TO CALL-PGM
00946
00947      ELSE
00948          IF  CLAIM-SESSION
00949              MOVE XCTL-EL126     TO CALL-PGM
00950
00951          ELSE
00952              IF  MORTGAGE-SESSION
00953                  MOVE XCTL-EM626 TO CALL-PGM
00954
00955              ELSE
00956                  IF  GENERAL-LEDGER-SESSION
00957                      MOVE XCTL-GL800
00958                                  TO CALL-PGM.
00959
00960      GO TO 9400-XCTL.
00961
00962  9400-XCTL.
00963      
      * EXEC CICS XCTL
00964 *        PROGRAM  (CALL-PGM)
00965 *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
00966 *        LENGTH   (PI-COMM-LENGTH)
00967 *    END-EXEC.
      *    MOVE '.$C                   $   #00001810' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303031383130' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CALL-PGM, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00968
00969      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL110' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
00970
00971  9700-LINK-DATE-CONVERT.
00972      
      * EXEC CICS LINK
00973 *        PROGRAM    ('ELDATCV')
00974 *        COMMAREA   (DATE-CONVERSION-DATA)
00975 *        LENGTH     (DC-COMM-LENGTH)
00976 *        END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   ''   #00001819' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031383139' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00977  9700-EXIT.
00978      EXIT.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL110' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 8230-NOT-OPEN,
                     0110-REC-NOT-FND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 8230-NOT-OPEN,
                     0210-REC-NOT-FND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 1040-RECORD-NOT-FOUND,
                     1040-RECORD-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 8230-NOT-OPEN,
                     1130-FILE-FULL
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 8230-NOT-OPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 8230-NOT-OPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 3010-END-FILE,
                     3010-END-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 3040-END-FILE,
                     3040-END-FILE
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL110' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
