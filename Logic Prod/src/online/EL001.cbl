00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL001 .
00004 *                            VMOD=2.017.
00005
00006 *AUTHOR.           LOGIC,INC.
00007 *                  DALLAS,TEXAS.
00008 *
00008 *
00009  DATE-COMPILED.
00010  SECURITY.   *****************************************************
00011              *                                                   *
00012              *    THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.    *
00013              *                                                   *
00014              *  USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES  *
00015              *  OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT   *
00016              *  THE PRIOR WRITTEN PERMISSION OF LOGIC, INC.      *
00017              *                                                   *
00018              *****************************************************
00019
00020 *REMARKS. ERROR TEXT PROCESSOR.
00021 *         THIS IS A LINKED TO SUBROUTINE FOR HANDLING THE ERROR
00022 *         LOOKUP AND TEXT FORMATTING OF THE ERROR LINES.
00023 *
00024 *
00025 *         ANY PROGRAM CHANGES TO THIS MODULE SHOULD ALSO BE
00026 *         APPLIED TO LGX001 WHEN CHANGES EFFECT LOGIC PROCESSING
00027 *         CLIENTS.
00028
00029      EJECT
00030  ENVIRONMENT DIVISION.
00031  DATA DIVISION.
00032  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00033  77  FILLER  PIC X(32)  VALUE '********************************'.
00034  77  FILLER  PIC X(32)  VALUE '*    EL001 WORKING STORAGE     *'.
00035  77  FILLER  PIC X(32)  VALUE '******** VMOD=2.017 ************'.
00036
00037  01  WS-DATE-AREA.
00038      05  TRANS-EXB1              PIC X(4)    VALUE 'EXB1'.
00039      05  TRANS-MXB1              PIC X(4)    VALUE 'MXB1'.
00040      05  TRANS-MXA5              PIC X(4)    VALUE 'MXA5'.
00041      05  TRANS-MXA6              PIC X(4)    VALUE 'MXA6'.
00042      05  TRANS-MXB2              PIC X(4)    VALUE 'MXB2'.
00043      05  TRANS-MXB3              PIC X(4)    VALUE 'MXB3'.
061013     05  trans-ex19              pic x(4)    value 'EX19'.
00044      05  SAVE-DATE               PIC X(8)    VALUE SPACES.
00045
00046  01  WORK-AREA.
00047      12  MAP-LENGTH              PIC S9(4)  COMP.
00048      12  WCC-CNTL                PIC X      VALUE ' '.
00049      12  EM-KEY                  PIC 9(4).
00050      12  EM-FILE-ID              PIC X(8)    VALUE 'ELERRS'.
00051      12  EM-FILE-ID-FR           PIC X(8)    VALUE 'MPFERR'.
00052      12  FOUND-SWITCH            PIC 9       VALUE 0.
00053          88  REC-NOT-FOUND           VALUE 1.
00054      12  TIME-IN                 PIC S9(7).
00055      12  TIME-OUT REDEFINES TIME-IN.
00056          16  FILLER              PIC X.
00057          16  TIME-O              PIC 99V99.
00058          16  FILLER              PIC XX.
00059      12  ER-NUM                  PIC 99      VALUE 1.
00060      12  ER-SUB                  PIC 99      VALUE 0.
00061      12  WCC-CTL                 PIC X       VALUE ' '.
00062      12  WS-ERROR-LINE.
00063          16  WS-ERROR-NUMBER     PIC 9999.
00064          16  WS-FILL             PIC X.
00065          16  WS-ERROR-SEVERITY   PIC X.
00066          16  FILLER              PIC X.
00067          16  WS-ERROR-TEXT.
00068              20  WS-PREFIX       PIC X(06).
00069              20  FILLER          PIC X(59).
00070      12  TEMP-ID.
00071          16  TEMP-TERM           PIC X(4).
00072          16  FILLER              PIC X(4)    VALUE '001A'.
00073
00074 *            COPY EL001S.
       01  EL001AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  RUNDTEL PIC S9(0004) COMP.
           05  RUNDTEF PIC  X(0001).
           05  FILLER REDEFINES RUNDTEF.
               10  RUNDTEA PIC  X(0001).
           05  RUNDTEI PIC  X(0008).
      *    -------------------------------
           05  RUNTIML PIC S9(0004) COMP.
           05  RUNTIMF PIC  X(0001).
           05  FILLER REDEFINES RUNTIMF.
               10  RUNTIMA PIC  X(0001).
           05  RUNTIMI PIC  X(0005).
      *    -------------------------------
           05  L1L PIC S9(0004) COMP.
           05  L1F PIC  X(0001).
           05  FILLER REDEFINES L1F.
               10  L1A PIC  X(0001).
           05  L1I PIC  X(0072).
      *    -------------------------------
           05  L2L PIC S9(0004) COMP.
           05  L2F PIC  X(0001).
           05  FILLER REDEFINES L2F.
               10  L2A PIC  X(0001).
           05  L2I PIC  X(0072).
      *    -------------------------------
           05  L3L PIC S9(0004) COMP.
           05  L3F PIC  X(0001).
           05  FILLER REDEFINES L3F.
               10  L3A PIC  X(0001).
           05  L3I PIC  X(0072).
      *    -------------------------------
           05  L4L PIC S9(0004) COMP.
           05  L4F PIC  X(0001).
           05  FILLER REDEFINES L4F.
               10  L4A PIC  X(0001).
           05  L4I PIC  X(0072).
      *    -------------------------------
           05  L5L PIC S9(0004) COMP.
           05  L5F PIC  X(0001).
           05  FILLER REDEFINES L5F.
               10  L5A PIC  X(0001).
           05  L5I PIC  X(0072).
      *    -------------------------------
           05  L6L PIC S9(0004) COMP.
           05  L6F PIC  X(0001).
           05  FILLER REDEFINES L6F.
               10  L6A PIC  X(0001).
           05  L6I PIC  X(0072).
      *    -------------------------------
           05  L7L PIC S9(0004) COMP.
           05  L7F PIC  X(0001).
           05  FILLER REDEFINES L7F.
               10  L7A PIC  X(0001).
           05  L7I PIC  X(0072).
      *    -------------------------------
           05  L8L PIC S9(0004) COMP.
           05  L8F PIC  X(0001).
           05  FILLER REDEFINES L8F.
               10  L8A PIC  X(0001).
           05  L8I PIC  X(0072).
      *    -------------------------------
           05  L9L PIC S9(0004) COMP.
           05  L9F PIC  X(0001).
           05  FILLER REDEFINES L9F.
               10  L9A PIC  X(0001).
           05  L9I PIC  X(0072).
      *    -------------------------------
           05  L10L PIC S9(0004) COMP.
           05  L10F PIC  X(0001).
           05  FILLER REDEFINES L10F.
               10  L10A PIC  X(0001).
           05  L10I PIC  X(0072).
      *    -------------------------------
           05  L11L PIC S9(0004) COMP.
           05  L11F PIC  X(0001).
           05  FILLER REDEFINES L11F.
               10  L11A PIC  X(0001).
           05  L11I PIC  X(0072).
      *    -------------------------------
           05  L12L PIC S9(0004) COMP.
           05  L12F PIC  X(0001).
           05  FILLER REDEFINES L12F.
               10  L12A PIC  X(0001).
           05  L12I PIC  X(0072).
       01  EL001AO REDEFINES EL001AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RUNDTEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RUNTIMO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L1O PIC  X(0072).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L2O PIC  X(0072).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L3O PIC  X(0072).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L4O PIC  X(0072).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L5O PIC  X(0072).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L6O PIC  X(0072).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L7O PIC  X(0072).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L8O PIC  X(0072).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L9O PIC  X(0072).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L10O PIC  X(0072).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L11O PIC  X(0072).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L12O PIC  X(0072).
      *    -------------------------------
00075  01  MAP-AREA REDEFINES EL001AI.
00076      12  FILLER                  PIC X(31).
00077      12  ERROR-LINES-FOR-MAP OCCURS 12 TIMES.
00078          16  FILLER              PIC X(3).
00079          16  ER-LINE             PIC X(72).
00080
00081      EJECT
00082 *            COPY ELCEMIB.
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
00083      EJECT
00084 *            COPY ELCDATE.
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
00085      EJECT
00086 *            COPY ELCERRWS.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCERRWS                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.005                          *
00006 *                                                                *
00007 *                                                                *
00008 *    WORKING STORAGE FOR ERROR MESSAGES ROUTINE ELCERRPD         *
00009 *                                                                *
00010 ******************************************************************
00011
00012  01  W-ERROR-WORK.
00013      12  E-CLIENT-ID             PIC X(03)   VALUE SPACES.
00014      12  CLIENT-ABL              PIC X(03)   VALUE 'ABL'.
00015      12  CLIENT-AIG              PIC X(03)   VALUE 'AIG'.
00016      12  CLIENT-AUK              PIC X(03)   VALUE 'AUK'.
00017      12  CLIENT-CIG              PIC X(03)   VALUE 'CIG'.
00018      12  CLIENT-CUK              PIC X(03)   VALUE 'CUK'.
00019      12  CLIENT-LAP              PIC X(03)   VALUE 'LAP'.
00020      12  CLIENT-NCB              PIC X(03)   VALUE 'NCB'.
00021      12  CLIENT-TIC              PIC X(03)   VALUE 'TIC'.
00022      12  CLIENT-TMS              PIC X(03)   VALUE 'TMS'.
00023      12  CLIENT-LBL              PIC X(03)   VALUE 'LBL'.
00024      12  CLIENT-HAN              PIC X(03)   VALUE 'HAN'.
00025      12  CLIENT-JHL              PIC X(03)   VALUE 'JHL'.
00026      12  CLIENT-HER              PIC X(03)   VALUE 'HER'.
00027      12  CLIENT-NCX              PIC X(03)   VALUE 'NCX'.
           12  CLIENT-DCC              PIC X(03)   VALUE 'DCC'.
           12  CLIENT-CID              PIC X(03)   VALUE 'CID'.
00028
00087      EJECT
00088 *            COPY ELCERRS.
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
00089      EJECT
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
00091
00092  01  DFHCOMMAREA                 PIC X(400).
00093
00094  01  PARMLIST.
00095      12  FILLER                  PIC S9(8) COMP.
00096      12  ERROR-POINTER           PIC S9(8) COMP.
00097      12  ERROR-POINTER-FR        PIC S9(8) COMP.
00098      12  MAP-POINTER             PIC S9(8) COMP.
00099
00100  01  ERROR-MESSAGE-FILE-REST     PIC X(72).
00101  01  ERROR-MESSAGE-FILE-FR       PIC X(72).
00102
00103  01  MAP-BUFFER                  PIC X.
00104      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA PARMLIST
                                ERROR-MESSAGE-FILE-REST
                                ERROR-MESSAGE-FILE-FR MAP-BUFFER.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL001' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00106      SERVICE RELOAD PARMLIST.
00107
00108      MOVE DFHCOMMAREA  TO  ERROR-MESSAGE-INTERFACE-BLOCK.
00109
00110      IF EMI-ROLL-SWITCH = 'Y'
00111         GO TO 3000-ROLL-CODES.
00112
00113      MOVE EMI-ERROR              TO EM-KEY.
00114
00115  0100-READ-ERRORS.
00116
00117      IF  EMI-LANGUAGE-IS-FR
00118          
      * EXEC CICS HANDLE CONDITION
00119 *             NOTFND   (2150-NOT-FOUND-FR)
00120 *             NOTOPEN  (2250-NOT-OPEN-FR)
00121 *        END-EXEC
      *    MOVE '"$IJ                  ! " #00000686' TO DFHEIV0
           MOVE X'2224494A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303030363836' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00122
00123          
      * EXEC CICS READ
00124 *             DATASET  (EM-FILE-ID-FR)
00125 *             RIDFLD   (EM-KEY)
00126 *             INTO     (ERROR-MESSAGE-FILE)
00127 *        END-EXEC
           MOVE LENGTH OF
            ERROR-MESSAGE-FILE
             TO DFHEIV11
      *    MOVE '&"IL       E          (   #00000691' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303030363931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EM-FILE-ID-FR, 
                 ERROR-MESSAGE-FILE, 
                 DFHEIV11, 
                 EM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00128
00129 *        SERVICE RELOAD ERROR-MESSAGE-FILE-FR
00130
00131      ELSE
00132          
      * EXEC CICS HANDLE CONDITION
00133 *             NOTFND   (2100-NOT-FOUND)
00134 *             NOTOPEN  (2200-NOT-OPEN)
00135 *        END-EXEC
      *    MOVE '"$IJ                  ! # #00000700' TO DFHEIV0
           MOVE X'2224494A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303030373030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00136
00137          
      * EXEC CICS READ
00138 *             DATASET  (EM-FILE-ID)
00139 *             RIDFLD   (EM-KEY)
00140 *             INTO     (ERROR-MESSAGE-FILE)
00141 *        END-EXEC.
           MOVE LENGTH OF
            ERROR-MESSAGE-FILE
             TO DFHEIV11
      *    MOVE '&"IL       E          (   #00000705' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303030373035' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EM-FILE-ID, 
                 ERROR-MESSAGE-FILE, 
                 DFHEIV11, 
                 EM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00142
00143 *        SERVICE RELOAD ERROR-MESSAGE-FILE-REST.
00144
00145      MOVE EMI-CLIENT-ID TO E-CLIENT-ID.
00146
00147 *                                COPY ELCERRPD.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCERRPD                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.005                          *
00006 *                                                                *
00007 *    CLIENT CODED ERROR MESSAGES                                 *
00008 *                                                                *
00009 ******************************************************************
00010
00011      IF EM-MESSAGE-NUMBER NOT NUMERIC
00012          MOVE ZEROS TO EM-MESSAGE-NUMBER.
00013
00014      IF E-CLIENT-ID EQUAL CLIENT-AIG OR CLIENT-AUK
00015                          OR CLIENT-CIG OR CLIENT-CUK
00016         IF EM-MESSAGE-NUMBER EQUAL 0541
00017             MOVE 'X' TO EM-ERROR-SEVERITY.
00018
00019      IF E-CLIENT-ID IS EQUAL TO CLIENT-HER
00020         IF EM-MESSAGE-NUMBER IS EQUAL TO 1909
00021             MOVE 'F'             TO  EM-ERROR-SEVERITY.
00022
00023      IF E-CLIENT-ID EQUAL CLIENT-LAP
00024         IF EM-MESSAGE-NUMBER EQUAL 2656
00025             MOVE 'F' TO EM-ERROR-SEVERITY.
00026
00027      IF E-CLIENT-ID IS EQUAL TO CLIENT-NCB
00028         IF EM-MESSAGE-NUMBER IS EQUAL TO 2671
00029             MOVE 'F'             TO  EM-ERROR-SEVERITY.
00030
00031      IF E-CLIENT-ID IS EQUAL TO CLIENT-ABL OR CLIENT-TIC
00032         IF EM-MESSAGE-NUMBER IS EQUAL TO 2672
00033             MOVE 'W'             TO  EM-ERROR-SEVERITY.
00034
00035      IF E-CLIENT-ID IS EQUAL TO CLIENT-HER
00036         IF EM-MESSAGE-NUMBER IS EQUAL TO 2693
00037             MOVE 'W'             TO  EM-ERROR-SEVERITY.
00038
00039      IF E-CLIENT-ID IS EQUAL TO CLIENT-TMS
00040         IF EM-MESSAGE-NUMBER IS EQUAL TO 2696
00041             MOVE 'F'             TO  EM-ERROR-SEVERITY.
00042
00043      IF E-CLIENT-ID EQUAL CLIENT-LBL
00044         IF EM-MESSAGE-NUMBER EQUAL 2738
00045             MOVE 'W' TO EM-ERROR-SEVERITY.
00046
00047      IF E-CLIENT-ID EQUAL CLIENT-LBL
00048         IF EM-MESSAGE-NUMBER EQUAL 2739
00049             MOVE 'W' TO EM-ERROR-SEVERITY.
00050
00051      IF E-CLIENT-ID EQUAL CLIENT-LBL
00052         IF EM-MESSAGE-NUMBER EQUAL 2741
00053             MOVE 'W' TO EM-ERROR-SEVERITY.
00054
00055      IF  E-CLIENT-ID EQUAL CLIENT-HAN
00056              OR
00057          E-CLIENT-ID EQUAL CLIENT-JHL
00058
00059          IF  EM-MESSAGE-NUMBER EQUAL 2756
00060              MOVE 'F'            TO EM-ERROR-SEVERITY.
00061
00062      IF E-CLIENT-ID = CLIENT-NCX
00063          IF EM-MESSAGE-NUMBER = 9895
00064              MOVE 'F'            TO EM-ERROR-SEVERITY.
           IF E-CLIENT-ID = CLIENT-DCC
              IF EM-MESSAGE-NUMBER = 2719
                 MOVE 'W'              TO EM-ERROR-SEVERITY
              END-IF
           END-IF
00065      .
00148
00149      IF EM-ERROR-TEXT-PREFIX = 'LLLLLL'
00150         MOVE EMI-LIFE-OVERRIDE-L6  TO EM-ERROR-TEXT-PREFIX.
00151
00152      IF EM-ERROR-TEXT-PREFIX = 'AAAAAA'
00153         MOVE EMI-AH-OVERRIDE-L6    TO EM-ERROR-TEXT-PREFIX.
00154
00155      IF EM-ERROR-TEXT-PREFIX = 'DDDDDD'
00156         MOVE EMI-DATE-FIELD        TO EM-ERROR-TEXT-PREFIX.
061013     if em-error-text-prefix = 'BBBBBB'
061013        move emi-claim-type        to em-error-text-prefix
061013     end-if
061013     if ws-error-text (1:10) = 'XXXXXXXXXX'
061013        and emi-claim-no not = spaces
061013        move emi-claim-no        to ws-error-text (1:10)
061013     end-if
00157
           .
00159  2000-PROCESS-ERRORS.
00160 **********************************************************
00161 *       THIS ROUTINE WILL CHECK THE SEVERITY OF THE      *
00162 *       ERROR AND DETERMINE IF IT IS TO BE PROCESSED     *
00163 *       OR NOT                                           *
00164 **********************************************************
00165      IF REC-NOT-FOUND
00166 *        MOVE 'N'                TO  EMI-MESSAGE-FLAG
00167          GO TO 2000-CONT-PROCESS-ERRORS.
00168
00169      MOVE EM-ERROR-SEVERITY      TO EMI-SEVERITY-SAVE.
00170      MOVE 'N'                    TO EMI-MESSAGE-FLAG.
00171
00172      IF EM-ERROR-SEVERITY = 'N'
00173         ADD 1 TO EMI-NOTE-CTR
00174         IF NOT EMI-PROCESS-ALL-ERRORS
00175            GO TO 2300-RETURN.
00176
00177      IF EM-ERROR-SEVERITY = 'W'
00178         ADD 1 TO EMI-WARNING-CTR
00179         IF EMI-BYPASS-WARNINGS  OR
00180            EMI-BYPASS-FORCABLES OR
00181            EMI-BYPASS-FATALS
00182              GO TO 2300-RETURN.
00183
00184      IF EM-ERROR-SEVERITY = 'F'
00185         ADD 1 TO EMI-FORCABLE-CTR
00186         IF EMI-BYPASS-FORCABLES OR
00187            EMI-BYPASS-FATALS
00188              GO TO 2300-RETURN.
00189
00190      IF EM-ERROR-SEVERITY = 'X'
00191         ADD 1 TO EMI-FATAL-CTR
00192         IF EMI-BYPASS-FATALS
00193            GO TO 2300-RETURN.
00194
00195  2000-CONT-PROCESS-ERRORS.
00196
00197      MOVE 'Y'                    TO EMI-MESSAGE-FLAG.
00198
00199      IF EMI-ERRORS-COMPLETE
00200         GO TO 2300-RETURN.
00201
00202      EJECT
00203 **********************************************************
00204 *       THIS ROUTINE WILL CHECK TO SEE IF THE ERROR      *
00205 *       HAS ALREADY BEEN PROCESSED. IF IT HAS, THEN      *
00206 *       PROCESSING WILL BE TERMINATED FOR THIS ERROR.    *
00207 **********************************************************
00208      IF EMI-FORMAT-CODES-ONLY
00209         IF EMI-ERROR  =  EMI-ERR-NUM (1) OR
00210                          EMI-ERR-NUM (2) OR
00211                          EMI-ERR-NUM (3) OR
00212                          EMI-ERR-NUM (4) OR
00213                          EMI-ERR-NUM (5) OR
00214                          EMI-ERR-NUM (6) OR
00215                          EMI-ERR-NUM (7) OR
00216                          EMI-ERR-NUM (8) OR
00217                          EMI-ERR-NUM (9) OR
00218                          EMI-ERR-NUM (10)
00219            GO TO 2300-RETURN.
00220
00221
00222      IF EMI-NUMBER-OF-LINES = 2
00223         IF EMI-ERROR  =  EMI-ERROR-NUMBER (1)  OR
00224                          EMI-ERROR-NUMBER (2)
00225            GO TO 2300-RETURN
00226         ELSE
00227            GO TO 2050-CHECK-LINE.
00228
00229      IF EMI-NUMBER-OF-LINES = 3
00230         IF EMI-ERROR  =  EMI-ERROR-NUMBER (1)  OR
00231                          EMI-ERROR-NUMBER (2)  OR
00232                          EMI-ERROR-NUMBER (3)
00233            GO TO 2300-RETURN.
00234
00235      EJECT
00236  2050-CHECK-LINE.
00237 **********************************************************
00238 *       THIS ROUTINE WILL DETERMINE WHICH ERROR LINE     *
00239 *       THE MESSAGE WILL FORMATTED INTO AND SET THE      *
00240 *       NECESSARY SWITCHES AND SUBSCRIPTS.               *
00241 **********************************************************
00242      MOVE '2' TO EMI-SWITCH1.
00243
00244      IF EMI-NUMBER-OF-LINES = 1
00245         SET EMI-INDX TO 1
00246         PERFORM 2490-FORMAT-LINE
00247         MOVE '2'                 TO EMI-SWITCH-AREA-1
00248         MOVE '3'                 TO EMI-SWITCH1
00249         GO TO 2300-RETURN.
00250
00251      IF EMI-NUMBER-OF-LINES = 2
00252         IF EMI-AREA1-EMPTY
00253            SET EMI-INDX TO 1
00254            MOVE '2'              TO EMI-SWITCH-AREA-1
00255            PERFORM 2490-FORMAT-LINE
00256            GO TO 2300-RETURN
00257         ELSE
00258            SET EMI-INDX TO 2
00259            IF EMI-FORMAT-CODES-ONLY
00260               PERFORM 2500-FORMAT-CODES-ONLY THRU 2500-EXIT
00261               GO TO 2300-RETURN
00262            ELSE
00263               MOVE '3'           TO EMI-SWITCH1
00264               MOVE '2'           TO EMI-SWITCH-AREA-2
00265               PERFORM 2490-FORMAT-LINE
00266               GO TO 2300-RETURN.
00267
00268         IF EMI-AREA1-EMPTY
00269            SET EMI-INDX TO 1
00270            MOVE '2'              TO EMI-SWITCH-AREA-1
00271            PERFORM 2490-FORMAT-LINE
00272            GO TO 2300-RETURN.
00273
00274         IF EMI-AREA2-EMPTY
00275            SET EMI-INDX TO 2
00276            MOVE '2'              TO EMI-SWITCH-AREA-2
00277            PERFORM 2490-FORMAT-LINE
00278            GO TO 2300-RETURN.
00279
00280         SET EMI-INDX TO 3.
00281
00282         IF EMI-FORMAT-CODES-ONLY
00283            PERFORM 2500-FORMAT-CODES-ONLY THRU 2500-EXIT
00284            GO TO 2300-RETURN
00285         ELSE
00286            MOVE '3'              TO EMI-SWITCH1
00287            PERFORM 2490-FORMAT-LINE
00288            GO TO 2300-RETURN.
00289
00290  2100-NOT-FOUND.
00291      MOVE 1                      TO FOUND-SWITCH.
00292      GO TO 2000-PROCESS-ERRORS.
00293
00294  2150-NOT-FOUND-FR.
00295      MOVE 'E'                    TO EMI-LANGUAGE-IND.
00296      GO TO 0100-READ-ERRORS.
00297
00298  2200-NOT-OPEN.
00299      SET EMI-INDX TO 1.
00300      MOVE 'ELERRS FILE NOT OPEN' TO EMI-ERROR-TEXT (EMI-INDX).
00301      MOVE '3'                    TO EMI-SWITCH1.
00302
00303  2250-NOT-OPEN-FR.
00304      SET EMI-INDX TO 1.
00305      MOVE 'MPERRS FILE NOT OPEN' TO EMI-ERROR-TEXT (EMI-INDX).
00306      MOVE '3'                    TO EMI-SWITCH1.
00307
00308  2300-RETURN.
00309      MOVE ERROR-MESSAGE-INTERFACE-BLOCK  TO  DFHCOMMAREA.
00310
00311      
      * EXEC CICS RETURN
00312 *    END-EXEC.
      *    MOVE '.(                    ''   #00000957' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303030393537' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00313
00314      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL001' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
00315      EJECT
00316  2490-FORMAT-LINE.
00317      IF REC-NOT-FOUND
00318         MOVE 'NO TEXT RECORD FOUND FOR THIS ERROR' TO
00319               EMI-ERROR-TEXT (EMI-INDX)
00320         MOVE 'X'                 TO EMI-SEVERITY (EMI-INDX)
00321      ELSE
00322         MOVE EM-ERROR-SEVERITY   TO EMI-SEVERITY (EMI-INDX)
00323         MOVE '-'                 TO EMI-FILL (EMI-INDX)
00324         MOVE EM-ERROR-TEXT       TO EMI-ERROR-TEXT (EMI-INDX).
00325
00326      MOVE EMI-ERROR              TO EMI-ERROR-NUMBER (EMI-INDX).
00327
00328  2500-FORMAT-CODES-ONLY.
00329      IF EMI-SUB = 2
00330         MOVE SPACES              TO EMI-ERROR-TEXT (3).
00331
00332      MOVE EMI-ERROR              TO EMI-ERR-NUM (EMI-SUB).
00333      MOVE '-'                    TO EMI-FILLER (EMI-SUB).
00334
00335      IF REC-NOT-FOUND
00336         MOVE 'X'                 TO EMI-SEV (EMI-SUB)
00337      ELSE
00338         MOVE EM-ERROR-SEVERITY   TO EMI-SEV (EMI-SUB).
00339
00340      IF EMI-SUB = 1
00341         ADD 1 TO EMI-SUB
00342         PERFORM 2490-FORMAT-LINE
00343         GO TO 2500-EXIT.
00344
00345      IF EMI-INDX = 2
00346         MOVE EMI-MESSAGE-AREA (3)
00347                                  TO EMI-MESSAGE-AREA (2).
00348
00349      IF EMI-SUB = 10
00350         MOVE '3'                 TO EMI-SWITCH1
00351      ELSE
00352         ADD 1 TO EMI-SUB.
00353
00354  2500-EXIT.
00355       EXIT.
00356
00357      EJECT
00358  3000-ROLL-CODES.
00359      MOVE LOW-VALUES             TO EL001AI.
00360 *
00361 *    EXEC CICS HANDLE CONDITION
00362 *         NOTFND    (4080-NOT-FOUND)
00363 *         NOTOPEN   (2200-NOT-OPEN)
00364 *    END-EXEC.
00365
00366      IF EMI-NUMBER-OF-LINES = 1
00367         IF EMI-FORMAT-CODES-ONLY
00368            MOVE EMI-LINE1        TO EMI-LINE3
00369            MOVE 1                TO ER-SUB
00370            PERFORM 4000-READ-ERRORS THRU 4099-EXIT
00371                    UNTIL ER-NUM GREATER THAN EMI-SUB
00372            GO TO 3500-SEND-ERROR-SCREEN.
00373
00374      IF EMI-NUMBER-OF-LINES = 2
00375         IF EMI-FORMAT-CODES-ONLY
00376            MOVE EMI-LINE1        TO ER-LINE (1)
00377            MOVE EMI-LINE2        TO EMI-LINE3
00378            MOVE 2                TO ER-SUB
00379            PERFORM 4000-READ-ERRORS THRU 4099-EXIT
00380                    UNTIL ER-NUM GREATER THAN EMI-SUB
00381            GO TO 3500-SEND-ERROR-SCREEN.
00382
00383      IF EMI-NUMBER-OF-LINES = 3
00384         IF EMI-FORMAT-CODES-ONLY
00385            MOVE EMI-LINE1        TO ER-LINE (1)
00386            MOVE EMI-LINE2        TO ER-LINE (2)
00387            MOVE 3                TO ER-SUB
00388            PERFORM 4000-READ-ERRORS THRU 4099-EXIT
00389                    UNTIL ER-NUM GREATER THAN EMI-SUB
00390            GO TO 3500-SEND-ERROR-SCREEN.
00391
00392      GO TO 2300-RETURN.
00393      EJECT
00394  3500-SEND-ERROR-SCREEN.
00395
00396      IF EIBTRNID = TRANS-EXB1 OR
00397                    TRANS-MXB1 OR
00398                    TRANS-MXA6 OR
00399                    TRANS-MXB2 OR
00400                    TRANS-MXB3 OR
00401                    TRANS-MXA5
061013                or trans-ex19
00402         GO TO 3515-SEND-ERRORS.
00403
00404      
      * EXEC CICS RECEIVE
00405 *         SET    (MAP-POINTER)
00406 *         LENGTH (MAP-LENGTH)
00407 *         BUFFER
00408 *    END-EXEC.
      *    MOVE '$"S B  L              ''   #00001051' TO DFHEIV0
           MOVE X'242253204220204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031303531' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-POINTER, 
                 MAP-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00409
00410      SERVICE RELOAD MAP-BUFFER.
00411
00412      MOVE EIBTRMID               TO TEMP-TERM.
00413
00414      
      * EXEC CICS HANDLE CONDITION
00415 *         QIDERR  (3510-WRITE-TS)
00416 *    END-EXEC.
      *    MOVE '"$N                   ! $ #00001061' TO DFHEIV0
           MOVE X'22244E202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303031303631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00417
00418      
      * EXEC CICS DELETEQ TS
00419 *         QUEUE  (TEMP-ID)
00420 *    END-EXEC.
      *    MOVE '*&                    #   #00001065' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303031303635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TEMP-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00421
00422  3510-WRITE-TS.
00423
00424      
      * EXEC CICS WRITEQ TS
00425 *         QUEUE  (TEMP-ID)
00426 *         LENGTH (MAP-LENGTH)
00427 *         FROM   (MAP-BUFFER)
00428 *    END-EXEC.
      *    MOVE '*"                    ''   #00001071' TO DFHEIV0
           MOVE X'2A2220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031303731' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TEMP-ID, 
                 MAP-BUFFER, 
                 MAP-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00429
00430  3515-SEND-ERRORS.
00431
00432      MOVE EIBDATE                TO DC-JULIAN-YYDDD.
00433      MOVE '5'                    TO DC-OPTION-CODE.
00434      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
00435      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.
00436
00437      MOVE SAVE-DATE              TO RUNDTEI.
00438      MOVE EIBTIME                TO TIME-IN.
00439      MOVE TIME-O                 TO RUNTIMO.
00440
00441      
      * EXEC CICS SEND
00442 *         MAPSET  ('EL001S')
00443 *         MAP     ('EL001A')
00444 *         FROM    (EL001AI)
00445 *         ERASE   WAIT
00446 *    END-EXEC.
           MOVE LENGTH OF
            EL001AI
             TO DFHEIV11
           MOVE 'EL001A' TO DFHEIV1
           MOVE 'EL001S' TO DFHEIV2
      *    MOVE '8$      TW E    H L F ,   #00001088' TO DFHEIV0
           MOVE X'382420202020202054572045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303031303838' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EL001AI, 
                 DFHEIV11, 
                 DFHEIV2, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00447
00448      
      * EXEC CICS HANDLE AID
00449 *         ANYKEY(3520-SEND-MAP)
00450 *    END-EXEC.
      *    MOVE '"&@                  V! % #00001095' TO DFHEIV0
           MOVE X'222640202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020562120' TO DFHEIV0(13:12)
           MOVE X'2520233030303031303935' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00451
00452      
      * EXEC CICS RECEIVE
00453 *         MAPSET  ('EL001S')
00454 *         MAP     ('EL001A')
00455 *         INTO    (EL001AI)
00456 *    END-EXEC.
           MOVE LENGTH OF
            EL001AI
             TO DFHEIV11
           MOVE 'EL001A' TO DFHEIV1
           MOVE 'EL001S' TO DFHEIV2
      *    MOVE '8"T I  L              ''   #00001099' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031303939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EL001AI, 
                 DFHEIV11, 
                 DFHEIV2, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00457
00458  3520-SEND-MAP.
00459
00460      IF EIBTRNID = TRANS-EXB1 OR
00461                    TRANS-MXB1 OR
00462                    TRANS-MXA6 OR
00463                    TRANS-MXB2 OR
00464                    TRANS-MXB3 OR
00465                    TRANS-MXA5
061013                or trans-ex19
00466         GO TO 2300-RETURN.
00467
00468      
      * EXEC CICS READQ TS
00469 *         QUEUE  (TEMP-ID)
00470 *         LENGTH (MAP-LENGTH)
00471 *         SET    (MAP-POINTER)
00472 *    END-EXEC
      *    MOVE '*$S    L              ''   #00001116' TO DFHEIV0
           MOVE X'2A2453202020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031313136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TEMP-ID, 
                 MAP-POINTER, 
                 MAP-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00473
00474      SERVICE RELOAD MAP-BUFFER.
00475
00476      
      * EXEC CICS SEND
00477 *         FROM    (MAP-BUFFER)
00478 *         LENGTH  (MAP-LENGTH)
00479 *         CTLCHAR (WCC-CTL)
00480 *    END-EXEC.
      *    MOVE '$$    C           L F ,   #00001124' TO DFHEIV0
           MOVE X'242420202020432020202020' TO DFHEIV0(1:12)
           MOVE X'2020202020204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303031313234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-BUFFER, 
                 MAP-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 WCC-CTL, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00481
00482      GO TO 2300-RETURN.
00483
00484  4000-READ-ERRORS.
00485      IF EMI-ERR-NUM (ER-NUM) NOT NUMERIC
00486         MOVE 99                  TO ER-NUM
00487         GO TO 4099-EXIT.
00488
00489      MOVE EMI-ERR-NUM (ER-NUM)   TO EM-KEY.
00490      MOVE SPACES                 TO WS-ERROR-LINE.
00491
00492      IF  EMI-LANGUAGE-IS-FR
00493          
      * EXEC CICS HANDLE CONDITION
00494 *             NOTFND   (4090-NOT-FOUND-FR)
00495 *             NOTOPEN  (2250-NOT-OPEN-FR)
00496 *        END-EXEC
      *    MOVE '"$IJ                  ! & #00001141' TO DFHEIV0
           MOVE X'2224494A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303031313431' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00497
00498          
      * EXEC CICS READ
00499 *             DATASET (EM-FILE-ID)
00500 *             RIDFLD  (EM-KEY)
00501 *             INTO    (ERROR-MESSAGE-FILE)
00502 *        END-EXEC
           MOVE LENGTH OF
            ERROR-MESSAGE-FILE
             TO DFHEIV11
      *    MOVE '&"IL       E          (   #00001146' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303031313436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EM-FILE-ID, 
                 ERROR-MESSAGE-FILE, 
                 DFHEIV11, 
                 EM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00503
00504 *        SERVICE RELOAD ERROR-MESSAGE-FILE-FR
00505
00506      ELSE
00507          
      * EXEC CICS HANDLE CONDITION
00508 *             NOTFND   (4080-NOT-FOUND)
00509 *             NOTOPEN  (2200-NOT-OPEN)
00510 *        END-EXEC
      *    MOVE '"$IJ                  ! '' #00001155' TO DFHEIV0
           MOVE X'2224494A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303031313535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00511
00512          
      * EXEC CICS READ
00513 *             DATASET (EM-FILE-ID)
00514 *             RIDFLD  (EM-KEY)
00515 *             INTO    (ERROR-MESSAGE-FILE)
00516 *        END-EXEC.
           MOVE LENGTH OF
            ERROR-MESSAGE-FILE
             TO DFHEIV11
      *    MOVE '&"IL       E          (   #00001160' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303031313630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EM-FILE-ID, 
                 ERROR-MESSAGE-FILE, 
                 DFHEIV11, 
                 EM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00517
00518 *        SERVICE RELOAD ERROR-MESSAGE-FILE-REST.
00519
00520      MOVE EMI-CLIENT-ID TO E-CLIENT-ID.
00521
00522 *                                COPY ELCERRPD.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCERRPD                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.005                          *
00006 *                                                                *
00007 *    CLIENT CODED ERROR MESSAGES                                 *
00008 *                                                                *
00009 ******************************************************************
00010
00011      IF EM-MESSAGE-NUMBER NOT NUMERIC
00012          MOVE ZEROS TO EM-MESSAGE-NUMBER.
00013
00014      IF E-CLIENT-ID EQUAL CLIENT-AIG OR CLIENT-AUK
00015                          OR CLIENT-CIG OR CLIENT-CUK
00016         IF EM-MESSAGE-NUMBER EQUAL 0541
00017             MOVE 'X' TO EM-ERROR-SEVERITY.
00018
00019      IF E-CLIENT-ID IS EQUAL TO CLIENT-HER
00020         IF EM-MESSAGE-NUMBER IS EQUAL TO 1909
00021             MOVE 'F'             TO  EM-ERROR-SEVERITY.
00022
00023      IF E-CLIENT-ID EQUAL CLIENT-LAP
00024         IF EM-MESSAGE-NUMBER EQUAL 2656
00025             MOVE 'F' TO EM-ERROR-SEVERITY.
00026
00027      IF E-CLIENT-ID IS EQUAL TO CLIENT-NCB
00028         IF EM-MESSAGE-NUMBER IS EQUAL TO 2671
00029             MOVE 'F'             TO  EM-ERROR-SEVERITY.
00030
00031      IF E-CLIENT-ID IS EQUAL TO CLIENT-ABL OR CLIENT-TIC
00032         IF EM-MESSAGE-NUMBER IS EQUAL TO 2672
00033             MOVE 'W'             TO  EM-ERROR-SEVERITY.
00034
00035      IF E-CLIENT-ID IS EQUAL TO CLIENT-HER
00036         IF EM-MESSAGE-NUMBER IS EQUAL TO 2693
00037             MOVE 'W'             TO  EM-ERROR-SEVERITY.
00038
00039      IF E-CLIENT-ID IS EQUAL TO CLIENT-TMS
00040         IF EM-MESSAGE-NUMBER IS EQUAL TO 2696
00041             MOVE 'F'             TO  EM-ERROR-SEVERITY.
00042
00043      IF E-CLIENT-ID EQUAL CLIENT-LBL
00044         IF EM-MESSAGE-NUMBER EQUAL 2738
00045             MOVE 'W' TO EM-ERROR-SEVERITY.
00046
00047      IF E-CLIENT-ID EQUAL CLIENT-LBL
00048         IF EM-MESSAGE-NUMBER EQUAL 2739
00049             MOVE 'W' TO EM-ERROR-SEVERITY.
00050
00051      IF E-CLIENT-ID EQUAL CLIENT-LBL
00052         IF EM-MESSAGE-NUMBER EQUAL 2741
00053             MOVE 'W' TO EM-ERROR-SEVERITY.
00054
00055      IF  E-CLIENT-ID EQUAL CLIENT-HAN
00056              OR
00057          E-CLIENT-ID EQUAL CLIENT-JHL
00058
00059          IF  EM-MESSAGE-NUMBER EQUAL 2756
00060              MOVE 'F'            TO EM-ERROR-SEVERITY.
00061
00062      IF E-CLIENT-ID = CLIENT-NCX
00063          IF EM-MESSAGE-NUMBER = 9895
00064              MOVE 'F'            TO EM-ERROR-SEVERITY.
           IF E-CLIENT-ID = CLIENT-DCC
              IF EM-MESSAGE-NUMBER = 2719
                 MOVE 'W'              TO EM-ERROR-SEVERITY
              END-IF
           END-IF
00065      .
00523
00524      MOVE '-'                    TO WS-FILL.
00525      MOVE EM-ERROR-SEVERITY      TO WS-ERROR-SEVERITY.
00526      MOVE EM-ERROR-TEXT          TO WS-ERROR-TEXT.
00527
00528  4050-MOVE-NUMBER.
00529      MOVE EM-MESSAGE-NUMBER      TO WS-ERROR-NUMBER.
00530
00531      IF WS-PREFIX = 'LLLLLL'
00532         MOVE EMI-LIFE-OVERRIDE-L6  TO WS-PREFIX.
00533
00534      IF WS-PREFIX = 'AAAAAA'
00535         MOVE EMI-AH-OVERRIDE-L6    TO WS-PREFIX.
00536
061013     if ws-error-text (1:10) = 'XXXXXXXXXX'
061013        and emi-claim-no not = spaces
061013        move emi-claim-no        to ws-error-text (1:10)
061013     end-if
00537      MOVE WS-ERROR-LINE          TO ER-LINE (ER-SUB).
00538
00539      ADD 1   TO ER-SUB
00540                 ER-NUM.
00541
00542      GO TO 4099-EXIT.
00543
00544  4080-NOT-FOUND.
00545      MOVE SPACES                 TO ER-LINE (ER-SUB).
00546      MOVE 'NO TEXT RECORD FOUND FOR THIS ERROR'  TO WS-ERROR-TEXT.
00547      MOVE WS-ERROR-LINE          TO ER-LINE (ER-SUB).
00548      ADD 1                       TO ER-SUB
00549                                     ER-NUM.
00550      GO TO 4099-EXIT.
00551
00552  4090-NOT-FOUND-FR.
00553
00554      MOVE 'E'                    TO EMI-LANGUAGE-IND.
00555      GO TO 4000-READ-ERRORS.
00556
00557  4099-EXIT.
00558       EXIT.
00559
00560  9700-LINK-DATE-CONVERT.
00561      
      * EXEC CICS LINK
00562 *        PROGRAM    ('ELDATCV')
00563 *        COMMAREA   (DATE-CONVERSION-DATA)
00564 *        LENGTH     (DC-COMM-LENGTH)
00565 *    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   (   #00001284' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303031323834' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00566
00567  9700-EXIT.
00568      EXIT.
00569

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL001' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 2150-NOT-FOUND-FR,
                     2250-NOT-OPEN-FR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 2100-NOT-FOUND,
                     2200-NOT-OPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 3510-WRITE-TS
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 3520-SEND-MAP
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 4090-NOT-FOUND-FR,
                     2250-NOT-OPEN-FR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 4080-NOT-FOUND,
                     2200-NOT-OPEN
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL001' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
