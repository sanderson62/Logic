00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL179 .
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 02/13/96 09:38:02.
00007 *                            VMOD=2.011.
00008 *
00009 *AUTHOR.        LOGIC, INC.
00010 *               DALLAS, TEXAS.
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
00023 *REMARKS. TRANSACTION EX49 - REPORT REVIEW.
00024      EJECT
00025  ENVIRONMENT DIVISION.
00026  DATA DIVISION.
00027  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00028  01  LCP-TIME-OF-DAY-XX.
00029      05  LCP-TIME-OF-DAY-68        PIC 9(6).
00030      05  FILLER                    PIC 99.
00031  01  LCP-CICS-TIME                 PIC 9(15).
00032  77  FILLER  PIC X(32)  VALUE '********************************'.
00033  77  FILLER  PIC X(32)  VALUE '*   EL179  WORKING STORAGE     *'.
00034  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.011 *********'.
00035
00036      EJECT
00037  01  WS-DATE-AREA.
00038      05  SAVE-DATE               PIC X(8)    VALUE SPACES.
00039      05  SAVE-BIN-DATE           PIC XX      VALUE SPACES.
00040
00041  01  LITERALS-NUMBERS.
00042      12  W-APPL-SCRTY-NDX        PIC S9(04) COMP VALUE +36.
00043      12  XCTL-EL005              PIC X(8)    VALUE 'EL005'.
00044      12  XCTL-EL010              PIC X(8)    VALUE 'EL010'.
00045      12  XCTL-EL126              PIC X(8)    VALUE 'EL126'.
00046      12  XCTL-EM626              PIC X(8)    VALUE 'EM626'.
00047      12  XCTL-GL800              PIC X(8)    VALUE 'GL800'.
00048      12  THIS-PGM                PIC X(8)    VALUE 'EL179'.
00049      12  DATE-CONV               PIC X(8)    VALUE 'ELDATCV'.
00050      12  LIT-TRAN                PIC X(4)    VALUE 'EX49'.
00051      12  LIT-TRAN-PR             PIC X(4)    VALUE 'EX59'.
00052      12  LIT-MAP                 PIC X(4)    VALUE '179A'.
00053      12  LIT-MAP-2               PIC X(4)    VALUE '179B'.
00054
00055  01  EDIT-WORK-AREA.
00056
00057      12  W-TIME-IN               PIC S9(07).
00058      12  FILLER   REDEFINES W-TIME-IN.
00059          16  FILLER              PIC  X(01).
00060          16  W-TIME-OUT          PIC  9(02)V9(02).
00061          16  FILLER              PIC  X(02).
00062      12  CALL-PGM                PIC X(8).
00063      12  TRANS-ID                PIC X(4).
00064      12  USER-TERM               PIC X(4).
00065      12  CHECK-PFKEYS            PIC 99.
00066      12  TEST-RESP               PIC X.
00067      12  COUNT-1                 PIC 99.
00068      12  COUNT-2                 PIC 99.
00069      12  COUNT-3                 PIC 99.
00070      12  COUNT-4                 PIC 99   VALUE  0.
00071      12  DISPLAY-LINE.
00072          16  REP-ID              PIC X(5).
00073          16  FILLER              PIC X(4).
00074          16  REP-NAME            PIC X(44).
00075          16  FILLER              PIC XX.
00076          16  REP-CREATED         PIC X(8).
00077          16  FILLER              PIC XX.
00078          16  REP-TIME            PIC X(5).
00079          16  FILLER              PIC XX.
00080          16  REP-LINES           PIC X(5).
00081          16  FILLER              PIC XX.
00082      12  PGMN-KEY                PIC X(5).
00083      12  CONVERT-LINES           PIC 9(8).
00084      12  HOLD-LINES.
00085          16  FILLER              PIC X(3).
00086          16  DISPLAY-LINES       PIC 9(5).
00087      12  EDIT-LINES              PIC ZZZZ9.
00088      12  OPTION-HOLD             PIC X.
00089          88  PRINT-REPORT                    VALUE '1'.
00090          88  REVIEW-ONLINE                   VALUE '2'.
00091          88  PURGE-REPORT                    VALUE '3'.
00092          88  REVIEW-QUE                      VALUE '4'.
00093          88  VALID-OPTION                VALUE '1' '2' '3' '4'.
00094
00095  01  TIME-UNFORMATTED.
00096      12  UN-HOURS                PIC XX.
00097      12  UN-MINUTES              PIC XX.
00098      12  FILLER                  PIC XX.
00099
00100  01  TIME-FORMATTED.
00101      12  FOR-HOURS               PIC XX.
00102      12  FILLER                  PIC X       VALUE '.'.
00103      12  FOR-MINUTES             PIC XX.
00104
00105  01  ERROR-NUMBERS.
00106      12  ER-0000                 PIC X(4)    VALUE '0000'.
00107      12  ER-0029                 PIC X(4)    VALUE '0029'.
00108      12  ER-0042                 PIC X(4)    VALUE '0042'.
00109      12  ER-0065                 PIC X(4)    VALUE '0065'.
00110      12  ER-0070                 PIC X(4)    VALUE '0070'.
00111      12  ER-0087                 PIC X(4)    VALUE '0087'.
00112      12  ER-0088                 PIC X(4)    VALUE '0088'.
00113      12  ER-0130                 PIC X(4)    VALUE '0130'.
00114      12  ER-0131                 PIC X(4)    VALUE '0131'.
00115      12  ER-0162                 PIC X(4)    VALUE '0162'.
00116      12  ER-0275                 PIC X(4)    VALUE '0275'.
00117      12  ER-0412                 PIC X(4)    VALUE '0412'.
00118      12  ER-0413                 PIC X(4)    VALUE '0413'.
00119      12  ER-0487                 PIC X(4)    VALUE '0487'.
00120      12  ER-2358                 PIC X(4)    VALUE '2358'.
00121      12  ER-2359                 PIC X(4)    VALUE '2359'.
00122      12  ER-2360                 PIC X(4)    VALUE '2360'.
00123      12  ER-2545                 PIC X(4)    VALUE '2545'.
00124      12  ER-3340                 PIC X(4)    VALUE '3340'.
00125      12  ER-7008                 PIC X(4)    VALUE '7008'.
00126      12  ER-9096                 PIC X(4)    VALUE '9096'.
00127      12  ER-9097                 PIC X(4)    VALUE '9097'.
00128
00129  01  ERROR-SWITCHES.
00130      12  ERROR-SWITCH            PIC X.
00131          88  SCREEN-ERROR                    VALUE 'X'.
00132      12  PGMN-SWITCH             PIC X.
00133          88  PGMN-FOUND                      VALUE '1'.
00134      12  SCREEN-SWITCH           PIC X.
00135          88  END-OF-FILE                     VALUE 'E'.
00136          88  SCREEN-FULL                     VALUE 'F'.
00137
00138  01  REPT-KEY.
00139      12  REPT-COMPANY-CODE       PIC X.
00140      12  REPT-RECORD-TYPE        PIC X.
00141      12  REPT-REPORT-ID          PIC X(5).
00142      12  REPT-LINE-NUMBER        PIC S9(8)   COMP.
00143
00144  01  CNTL-KEY.
00145      12  CNTL-COMPANY-ID         PIC X(3).
00146      12  CNTL-RECORD-TYPE        PIC X.
00147      12  FILLER                  PIC X(4).
00148      12  CNTL-SEQ-NUMBER         PIC S9(4)   COMP.
00149
00150  01  COMP-LENGTHS.
CIDMOD     12  LIT-IC                  PIC S9(4)   COMP VALUE -1.
00151      12  DELETE-LENGTH           PIC S9(4)   COMP VALUE +7.
00152      EJECT
00153 *                                COPY ELCLOGOF.
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
00154      EJECT
00155 *                                COPY ELCATTR.
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
00156      EJECT
00157 *                                COPY ELCAID.
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
00158
00159  01  FILLER REDEFINES DFHAID.
00160      12  FILLER                  PIC X(8).
00161      12  AID-KEYS OCCURS 24 TIMES.
00162          16  FILLER              PIC X.
00163      EJECT
00164 *                                COPY EL179S.
       01  EL179AI.
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
           05  OPTIONL PIC S9(0004) COMP.
           05  OPTIONF PIC  X(0001).
           05  FILLER REDEFINES OPTIONF.
               10  OPTIONA PIC  X(0001).
           05  OPTIONI PIC  X(0001).
      *    -------------------------------
           05  REPL PIC S9(0004) COMP.
           05  REPF PIC  X(0001).
           05  FILLER REDEFINES REPF.
               10  REPA PIC  X(0001).
           05  REPI PIC  X(0005).
      *    -------------------------------
           05  STPAGEL PIC S9(0004) COMP.
           05  STPAGEF PIC  X(0001).
           05  FILLER REDEFINES STPAGEF.
               10  STPAGEA PIC  X(0001).
           05  STPAGEI PIC  X(0004).
      *    -------------------------------
           05  ENDPAGEL PIC S9(0004) COMP.
           05  ENDPAGEF PIC  X(0001).
           05  FILLER REDEFINES ENDPAGEF.
               10  ENDPAGEA PIC  X(0001).
           05  ENDPAGEI PIC  X(0004).
      *    -------------------------------
           05  ALTPRNTL PIC S9(0004) COMP.
           05  ALTPRNTF PIC  X(0001).
           05  FILLER REDEFINES ALTPRNTF.
               10  ALTPRNTA PIC  X(0001).
           05  ALTPRNTI PIC  X(0004).
           05  REPLINED OCCURS 11  TIMES.
      *    -------------------------------
               10  REPLINEL PIC S9(0004) COMP.
               10  REPLINEF PIC  X(0001).
               10  FILLER REDEFINES REPLINEF.
                   15  REPLINEA PIC  X(0001).
               10  REPLINEI PIC  X(0079).
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
       01  EL179AO REDEFINES EL179AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TIMEO PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  OPTIONO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REPO PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STPAGEO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ENDPAGEO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALTPRNTO PIC  X(0004).
      *    -------------------------------
           05  REPLINED OCCURS 11  TIMES.
               10  FILLER        PIC  X(0003).
               10  REPLINEO PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSGO PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFKEYO PIC  X(0002).
      *    -------------------------------
       01  EL179BI.
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
           05  REPIDL PIC S9(0004) COMP.
           05  REPIDF PIC  X(0001).
           05  FILLER REDEFINES REPIDF.
               10  REPIDA PIC  X(0001).
           05  REPIDI PIC  X(0005).
      *    -------------------------------
           05  REPNML PIC S9(0004) COMP.
           05  REPNMF PIC  X(0001).
           05  FILLER REDEFINES REPNMF.
               10  REPNMA PIC  X(0001).
           05  REPNMI PIC  X(0045).
           05  LINEREPD OCCURS 16  TIMES.
      *    -------------------------------
               10  LINEREPL PIC S9(0004) COMP.
               10  LINEREPF PIC  X(0001).
               10  FILLER REDEFINES LINEREPF.
                   15  LINEREPA PIC  X(0001).
               10  LINEREPI PIC  X(0079).
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
       01  EL179BO REDEFINES EL179BI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DATEBO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TIMEBO PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REPIDO PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REPNMO PIC  X(0045).
      *    -------------------------------
           05  LINEREPD OCCURS 16  TIMES.
               10  FILLER        PIC  X(0003).
               10  LINEREPO PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSGBO PIC  X(0075).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFKEYBO PIC  X(0002).
      *    -------------------------------
00165      EJECT
00166 *                                COPY ELCINTF.
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
00167      12  EL179-WORK-AREA REDEFINES PI-PROGRAM-WORK-AREA.
00168          16  PI-PRINT-REPORT     PIC X(5).
00169          16  PI-START-COUNT      PIC S9(8)   COMP.
00170          16  PI-TOTAL-COUNT      PIC S9(8)   COMP.
00171          16  PI-START-PAGE-NO    PIC S9(4).
00172          16  PI-ACCUM-PAGE-NO    PIC S9(4).
00173          16  PI-END-PAGE-NO      PIC S9(4).
00174          16  PI-FORMS-PRINTER-ID PIC X(4).
00175          16  PI-SAVE-BEGIN       PIC X(5).
00176          16  PI-SAVE-ENDING      PIC X(5).
00177          16  PI-VIEW-SWITCH      PIC X.
00178          16  PI-DELETE-SW        PIC X.
00179              88  FIRST-REQUEST VALUE ' '.
00180              88  DELETE-OK     VALUE 'Y'.
00181          16  PI-ELREPT-BROWSE-SW PIC X.
00182              88  STARTED-BROWSE VALUE 'Y'.
00183          16  FILLER              PIC X(598).
00184
00185      EJECT
00186 *                                COPY ELCEMIB.
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
00187      EJECT
00188 *                                COPY ELCDATE.
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
00189      EJECT
00190 *                                COPY ELCSCTM.
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
00191      EJECT
00192 *                                COPY ELCSCRTY.
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
00193      EJECT
00194 *                                COPY MPCSCRT.
00001 ******************************************************************
00002 *                                                                *
00003 *                            MPCSCRT                             *
00004 *                            VMOD=1.001                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = C.I.C.S. COMMON SECURITY DATA AREA        *
00007 *        ACQUIRED BY SIGN-ON PROGRAM EL125.                      *
00008 *                                      (MP MORTGAGE PROTECTION)  *
00009 *                                                                *
00010 ******************************************************************
00011 *
00012  01  SECURITY-CONTROL-E.
00013      12  SC-COMM-LENGTH-E             PIC S9(04) VALUE +144 COMP.
00014      12  FILLER                       PIC  X(02) VALUE 'SC'.
00015      12  SC-QUID-KEY.
00016          16  SC-QUID-TERMINAL         PIC  X(04).
00017          16  SC-QUID-SYSTEM           PIC  X(04).
00018      12  SC-ITEM                      PIC S9(04) VALUE +1   COMP.
00019      12  SC-SECURITY-ACCESS-CODE      PIC  X(01).
00020      12  SC-PRODUCER-AUTHORIZED-SW    PIC  X(01).
00021          88 SC-PRODUCER-AUTHORIZED               VALUE ' '.
00022          88 SC-PRODUCER-NOT-AUTHORIZED           VALUE 'N'.
00023      12  SC-MP-CODES.
00024          16  SC-MP-AUTHORIZATION OCCURS 44 TIMES.
00025              20  SC-MP-DISPLAY        PIC  X(01).
00026              20  SC-MP-UPDATE         PIC  X(01).
00027      12  FILLER                       PIC  X(40).
00195      EJECT
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
00197  01  DFHCOMMAREA                 PIC X(1024).
00198
00199 *01 PARM-LIST .
00200 *    12  FILLER                  PIC S9(8)   COMP.
00201 *    12  REPT-PNT                PIC S9(8)   COMP.
00202 *    12  PGMN-PNT                PIC S9(8)   COMP.
00203 *    12  CNTL-PNT                PIC S9(8)   COMP.
00204      EJECT
00205 *                                COPY ELCREPT.
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
00206      EJECT
00207 *                                COPY ELCPGMN.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCPGMN.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.002                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = PROGRAM DESCRIPTIONS                      *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 52    RECFORM = FIX                            *
00011 *                                                                *
00012 *   BASE CLUSTER NAME = ELPGMN                 RKP=2,LEN=5       *
00013 *       ALTERNATE PATH  = NOT USED                               *
00014 *                                                                *
00015 *   LOG = NO                                                     *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00017 ******************************************************************
00018  01  PROGRAM-DESCRIPTIONS.
00019      12  PN-RECORD-ID                PIC XX.
00020          88  VALID-PN-ID                VALUE 'PN'.
00021
00022      12  PN-CONTROL-PRIMARY.
00023          16  PN-PROGRAM-NUMBER.
00024              20  PN-SYSTEM-CODE      PIC XX.
00025                  88  CLAS-IC-ONLINE     VALUE 'EL'.
00026                  88  CLAS-IC-REPORT     VALUE 'EC'.
00027                  88  CLAS-GL-BATCH      VALUE 'GL'.
00028              20  PN-PROGRAM-SEQUENCE PIC 999.
00029      12  PN-PROGRAM-DESCRIPTION      PIC X(40).
00030      12  PN-TRANSACTION-CODE         PIC X(4).
00031      12  PN-ENTRY-METHOD             PIC X.
00032          88  ENTERED-FROM-CICS          VALUE 'C'.
00033          88  ENTERED-FROM-CLASIC-MENU   VALUE 'M'.
00034          88  ENTERED-FROM-BATCH         VALUE 'B'.
00035
00036 ******************************************************************
00208      EJECT
00209 *                                COPY ELCCNTL.
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
00210      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA REPORT-SAVE-FILE
                                PROGRAM-DESCRIPTIONS CONTROL-FILE.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL179' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00212
00213      CONTINUE.
00214
00215      MOVE EIBDATE                TO DC-JULIAN-YYDDD.
00216      MOVE '5'                    TO DC-OPTION-CODE.
00217      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
00218      MOVE DC-GREG-DATE-1-EDIT    TO SAVE-DATE.
00219      MOVE DC-BIN-DATE-1          TO SAVE-BIN-DATE.
00220      MOVE SPACES                 TO PI-ELREPT-BROWSE-SW.
00221
00222      IF EIBCALEN = ZERO
00223          GO TO 8800-UNAUTHORIZED-ACCESS.
00224
00225      
      * EXEC CICS HANDLE CONDITION
00226 *         PGMIDERR      (8820-XCTL-ERROR)
00227 *         TERMIDERR     (1110-TERMID-ERROR)
00228 *         TRANSIDERR    (1120-TRANS-ERROR)
00229 *         ERROR         (9990-ABEND)
00230 *    END-EXEC.
      *    MOVE '"$L[\.                ! " #00002781' TO DFHEIV0
           MOVE X'22244C5B5C2E202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303032373831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00231
00232      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.
00233      MOVE SPACES                 TO ERROR-SWITCHES.
00234      MOVE LIT-TRAN               TO TRANS-ID.
00235      MOVE LOW-VALUES             TO EL179AI EL179BI.
00236
00237      IF THIS-PGM NOT = PI-CALLING-PROGRAM
00238          MOVE LOW-VALUES         TO EL179AO
00239          MOVE ZEROS              TO PI-START-COUNT
00240          PERFORM 0100-UPDATE-PI THRU 0120-EXIT
00241          GO TO 2000-PAGE-FORWARD.
00242
00243      IF  EIBAID = DFHCLEAR
00244              OR
00245          NOT DISPLAY-CAP
00246          GO TO 8200-RETURN-PRIOR.
00247
00248      IF PI-CURRENT-SCREEN-NO = LIT-MAP-2
00249          MOVE ZEROS              TO COUNT-2
00250          GO TO 5000-PROCESS-REVIEW.
00251
00252      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00253          MOVE LOW-VALUES         TO EL179AO
00254          MOVE ER-7008            TO EMI-ERROR
00255          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00256          MOVE -1                 TO OPTIONL
00257          GO TO 8110-SEND-DATA.
00258
00259      
      * EXEC CICS RECEIVE
00260 *        MAP     ('EL179A')
00261 *        MAPSET  ('EL179S')
00262 *    END-EXEC.
           MOVE 'EL179A' TO DFHEIV1
           MOVE 'EL179S' TO DFHEIV2
      *    MOVE '8"T                   ''   #00002815' TO DFHEIV0
           MOVE X'382254202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032383135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EL179AI, 
                 DFHEIV99, 
                 DFHEIV2, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00263
00264      IF PFKEYL GREATER THAN ZERO
00265          PERFORM 0200-TRANS-PF THRU 0210-EXIT.
00266
00267      IF SCREEN-ERROR
00268          MOVE ER-7008            TO EMI-ERROR
00269          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00270          MOVE -1                 TO OPTIONL
00271          GO TO 8110-SEND-DATA.
00272
00273      IF EIBAID = DFHPF12
00274          GO TO 8300-GET-HELP.
00275
00276      IF EIBAID = DFHPF23
00277          GO TO 8810-PF23-ENTERED.
00278
00279      IF EIBAID = DFHPF24
00280          GO TO 8400-RETURN-MASTER.
00281
00282      IF EIBAID = DFHPF1
00283          GO TO 2000-PAGE-FORWARD.
00284
00285      IF EIBAID = DFHPF2
00286          GO TO 3000-PAGE-BACKWARD.
00287
00288      PERFORM 1000-EDIT-SCREEN THRU 1000-EXIT.
00289
00290      IF SCREEN-ERROR
00291          GO TO 8110-SEND-DATA.
00292
00293      IF EIBAID NOT = DFHENTER
00294          MOVE ER-0029            TO EMI-ERROR
00295          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00296          MOVE -1                 TO OPTIONL
00297          GO TO 8110-SEND-DATA.
00298
00299      IF REVIEW-ONLINE
00300          IF PI-START-PAGE-NO GREATER ZERO
00301              PERFORM 6500-PAGE-SEARCH THRU 6500-EXIT
00302              GO TO 1200-REVIEW-REPORT
00303          ELSE
00304              GO TO 1200-REVIEW-REPORT.
00305
00306      IF PRINT-REPORT
00307          IF PI-START-PAGE-NO GREATER ZERO
00308              PERFORM 6500-PAGE-SEARCH THRU 6500-EXIT
00309              GO TO 1100-START-PRINTER
00310          ELSE
00311              GO TO 1100-START-PRINTER.
00312
00313      IF PURGE-REPORT
00314         IF DELETE-OK
00315             MOVE SPACES          TO PI-DELETE-SW
00316             GO TO 1300-DELETE-REPORT
00317          ELSE
00318             MOVE 'Y'                TO PI-DELETE-SW
00319             MOVE ER-0065            TO EMI-ERROR
00320             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00321             MOVE AL-UNNON           TO OPTIONA
00322             MOVE +1                 TO OPTIONL
00323             MOVE '3'                TO OPTIONI
00324             MOVE AL-UANON           TO REPA
00325             MOVE +5                 TO REPL
00326             MOVE PI-PRINT-REPORT    TO REPI
00327             GO TO 2000-PAGE-FORWARD.
00328
00329      GO TO 2000-PAGE-FORWARD.
00330      EJECT
00331  0100-UPDATE-PI.
00332      IF PI-RETURN-TO-PROGRAM = THIS-PGM
00333          GO TO 0110-UPDATE-UP.
00334
00335      MOVE PI-SAVED-PROGRAM-5     TO PI-SAVED-PROGRAM-6.
00336      MOVE PI-SAVED-PROGRAM-4     TO PI-SAVED-PROGRAM-5.
00337      MOVE PI-SAVED-PROGRAM-3     TO PI-SAVED-PROGRAM-4.
00338      MOVE PI-SAVED-PROGRAM-2     TO PI-SAVED-PROGRAM-3.
00339      MOVE PI-SAVED-PROGRAM-1     TO PI-SAVED-PROGRAM-2.
00340      MOVE PI-RETURN-TO-PROGRAM   TO PI-SAVED-PROGRAM-1.
00341      MOVE PI-CALLING-PROGRAM     TO PI-RETURN-TO-PROGRAM.
00342      MOVE THIS-PGM               TO PI-CALLING-PROGRAM.
00343      MOVE HIGH-VALUES            TO PI-SAVE-BEGIN.
00344      MOVE LOW-VALUES             TO PI-SAVE-ENDING.
00345      MOVE SPACES                 TO PI-FORMS-PRINTER-ID.
00346      MOVE SPACES                 TO PI-DELETE-SW.
00347      PERFORM 9910-INITIALIZE-SECURITY THRU 9910-EXIT.
00348      GO TO 0120-EXIT.
00349
00350  0110-UPDATE-UP.
00351      MOVE PI-RETURN-TO-PROGRAM   TO PI-CALLING-PROGRAM.
00352      MOVE PI-SAVED-PROGRAM-1     TO PI-RETURN-TO-PROGRAM.
00353      MOVE PI-SAVED-PROGRAM-2     TO PI-SAVED-PROGRAM-1.
00354      MOVE PI-SAVED-PROGRAM-3     TO PI-SAVED-PROGRAM-2.
00355      MOVE PI-SAVED-PROGRAM-4     TO PI-SAVED-PROGRAM-3.
00356      MOVE PI-SAVED-PROGRAM-5     TO PI-SAVED-PROGRAM-4.
00357      MOVE PI-SAVED-PROGRAM-6     TO PI-SAVED-PROGRAM-5.
00358      MOVE SPACES                 TO PI-SAVED-PROGRAM-6.
00359
00360  0120-EXIT.
00361      EXIT.
00362
00363  0200-TRANS-PF.
00364      IF EIBAID NOT = DFHENTER
00365          MOVE 'X'                TO ERROR-SWITCH
00366          GO TO 0210-EXIT.
00367
00368      IF PFKEYI NOT NUMERIC
00369          MOVE 'X'                TO ERROR-SWITCH
00370          GO TO 0210-EXIT.
00371
00372      MOVE PFKEYI                 TO CHECK-PFKEYS.
00373
00374      IF CHECK-PFKEYS LESS 1 OR GREATER 24
00375          MOVE 'X'                TO ERROR-SWITCH
00376          GO TO 0210-EXIT.
00377
00378      MOVE AID-KEYS (CHECK-PFKEYS) TO EIBAID.
00379
00380  0210-EXIT.
00381      EXIT.
00382      EJECT
00383  1000-EDIT-SCREEN.
00384      MOVE ZEROS                  TO PI-START-COUNT.
00385
00386      MOVE OPTIONI                TO OPTION-HOLD.
00387
00388      IF NOT VALID-OPTION
00389          MOVE ER-0087            TO EMI-ERROR
00390          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00391          MOVE AL-UNBON           TO OPTIONA
00392          MOVE -1                 TO OPTIONL
00393          GO TO 1000-EXIT.
00394
00395      IF PI-PROCESSOR-PRINTER IS NOT EQUAL SPACES
Y2KMOD                                          AND  LOW-VALUES
00396          MOVE PI-PROCESSOR-PRINTER   TO  PI-FORMS-PRINTER-ID.
00397
00398      MOVE SPACES                 TO PI-ALT-DMD-PRT-ID.
00399      IF ALTPRNTL GREATER ZERO
00400          MOVE ALTPRNTI           TO PI-FORMS-PRINTER-ID
00401                                     PI-ALT-DMD-PRT-ID.
00402
00403      IF REVIEW-QUE
00404        AND
00405         REPI NOT GREATER THAN SPACES
00406          MOVE AL-UNNON           TO OPTIONA
00407          MOVE AL-UANOF           TO REPA
00408          MOVE LOW-VALUES         TO STPAGEO
00409          MOVE AL-UNNOF           TO STPAGEA
00410          GO TO 1000-EXIT.
00411
00412      IF NOT PURGE-REPORT
00413          MOVE SPACES             TO PI-DELETE-SW.
00414
00415      IF REVIEW-ONLINE OR PRINT-REPORT
00416          IF STPAGEL GREATER ZERO
00417              IF STPAGEI NUMERIC
00418                  MOVE ZEROS      TO PI-ACCUM-PAGE-NO
00419                  MOVE STPAGEI    TO PI-START-PAGE-NO
00420                  MOVE PI-START-PAGE-NO
00421                                  TO STPAGEO
00422                  MOVE AL-UNNON   TO STPAGEA
00423              ELSE
00424                  MOVE AL-UNBON   TO STPAGEA
00425                  MOVE -1         TO STPAGEL
00426                  MOVE 'X'        TO ERROR-SWITCH
00427                  MOVE ER-2358    TO EMI-ERROR
00428                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00429          ELSE
00430              MOVE ZEROS          TO PI-START-PAGE-NO
00431                                     PI-ACCUM-PAGE-NO
00432      ELSE
00433          MOVE ZEROS              TO PI-START-PAGE-NO
00434                                     PI-ACCUM-PAGE-NO
00435          MOVE LOW-VALUES         TO STPAGEO
00436          MOVE AL-UNNOF           TO STPAGEA.
00437
00438      IF PRINT-REPORT
00439          IF ENDPAGEL GREATER ZERO
00440              IF ENDPAGEI NUMERIC
00441                  MOVE ENDPAGEI   TO PI-END-PAGE-NO
00442                  MOVE PI-END-PAGE-NO
00443                                  TO ENDPAGEO
00444                  MOVE AL-UNNON   TO ENDPAGEA
00445              ELSE
00446                  MOVE AL-UNBON   TO ENDPAGEA
00447                  MOVE -1         TO ENDPAGEL
00448                  MOVE 'X'        TO ERROR-SWITCH
00449                  MOVE ER-2358    TO EMI-ERROR
00450                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00451          ELSE
00452              MOVE ZEROS          TO PI-END-PAGE-NO
00453      ELSE
00454          MOVE ZEROS              TO PI-END-PAGE-NO
00455          MOVE LOW-VALUES         TO ENDPAGEO
00456          MOVE AL-UNNOF           TO ENDPAGEA.
00457
00458      IF NOT REVIEW-QUE
00459        AND
00460         EIBAID NOT = DFHENTER
00461          GO TO 1000-PFKEY-ERROR.
00462
00463      MOVE REPI                   TO PGMN-KEY  PI-PRINT-REPORT.
00464
00465      PERFORM 1600-READ-PGMN THRU 1600-EXIT.
00466
00467      IF PGMN-FOUND
00468         GO TO 1000-EXIT.
00469
00470      MOVE ER-0162                TO EMI-ERROR.
00471      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00472      MOVE 'X'                    TO ERROR-SWITCH.
00473      MOVE -1                     TO REPL.
00474      MOVE AL-UABON               TO REPA.
00475      GO TO 1000-EXIT.
00476
00477  1000-PFKEY-ERROR.
00478      MOVE ER-7008                TO EMI-ERROR.
00479      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00480      MOVE 'X'                    TO ERROR-SWITCH.
00481      MOVE -1                     TO OPTIONL.
00482
00483  1000-EXIT.
00484      EXIT.
00485      EJECT
00486  1100-START-PRINTER.
00487
00488      IF  NOT MODIFY-CAP
00489          MOVE 'UPDATE'           TO SM-READ
00490          PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
00491          MOVE ER-9096            TO EMI-ERROR
00492          PERFORM 9900-ERROR-FORMAT THRU 9910-EXIT
00493          MOVE -1                 TO OPTIONL
00494          GO TO 8110-SEND-DATA.
00495
00496      MOVE SPACES                 TO REPT-KEY.
00497      MOVE PI-COMPANY-CD          TO REPT-COMPANY-CODE.
00498      MOVE '2'                    TO REPT-RECORD-TYPE.
00499      MOVE REPI                   TO REPT-REPORT-ID PI-PRINT-REPORT
00500      MOVE ZEROS                  TO REPT-LINE-NUMBER.
00501      PERFORM 1400-FIND-REPORT THRU 1400-EXIT.
00502
00503      IF SCREEN-ERROR
00504          GO TO 8110-SEND-DATA.
00505
00506      IF PI-FORMS-PRINTER-ID = SPACES
Y2KMOD                            OR  LOW-VALUES
00507          MOVE SPACES             TO CNTL-KEY
00508          MOVE PI-COMPANY-ID      TO CNTL-COMPANY-ID
00509          MOVE '1'                TO CNTL-RECORD-TYPE
00510          MOVE ZEROS              TO CNTL-SEQ-NUMBER
00511          PERFORM 1500-FIND-PRINTER THRU 1500-EXIT
00512          IF SCREEN-ERROR
00513              MOVE -1             TO OPTIONL
00514              GO TO 8110-SEND-DATA
00515          ELSE
00516              MOVE CF-FORMS-PRINTER-ID TO USER-TERM
CIDMOD                                         PI-FORMS-PRINTER-ID
CIDMOD         END-IF
00517      ELSE
00518          MOVE PI-FORMS-PRINTER-ID    TO USER-TERM.
CIDMOD
CIDMOD     MOVE PI-FORMS-PRINTER-ID TO PI-PROCESSOR-PRINTER.
CIDMOD
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'
CIDMOD         
      * EXEC CICS START
CIDMOD*            TRANSID   (LIT-TRAN-PR)
CIDMOD*            FROM      (PROGRAM-INTERFACE-BLOCK)
CIDMOD*            LENGTH    (PI-COMM-LENGTH)
PEMMOD*            TERMID    ('A046')
CIDMOD*        END-EXEC
      *    MOVE '0( LF                 0   #00003083' TO DFHEIV0
           MOVE X'3028204C4620202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020203020' TO DFHEIV0(13:12)
           MOVE X'2020233030303033303833' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 LIT-TRAN-PR, 
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
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00528      ELSE
00529          
      * EXEC CICS START
00530 *            TRANSID   (LIT-TRAN-PR)
00531 *            FROM      (PROGRAM-INTERFACE-BLOCK)
00532 *            LENGTH    (PI-COMM-LENGTH)
00533 *            TERMID    (USER-TERM)
00534 *        END-EXEC.
      *    MOVE '0( LFT                0   #00003090' TO DFHEIV0
           MOVE X'3028204C4654202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020203020' TO DFHEIV0(13:12)
           MOVE X'2020233030303033303930' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 LIT-TRAN-PR, 
                 DFHEIV99, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 USER-TERM, 
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
           
00535
00536      MOVE ER-2360                TO EMI-ERROR.
00537      GO TO 1130-SEND-MESSAGE.
00538
00539  1110-TERMID-ERROR.
00540      MOVE ER-0412                TO EMI-ERROR.
00541      GO TO 1130-SEND-MESSAGE.
00542
00543  1120-TRANS-ERROR.
00544      MOVE ER-0413                TO EMI-ERROR.
00545
00546  1130-SEND-MESSAGE.
00547      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00548      MOVE -1                     TO OPTIONL.
00549      GO TO 8110-SEND-DATA.
00550      EJECT
00551  1200-REVIEW-REPORT.
00552      MOVE SPACES                 TO REPT-KEY.
00553      MOVE PI-COMPANY-CD          TO REPT-COMPANY-CODE.
00554      MOVE '2'                    TO REPT-RECORD-TYPE.
00555      MOVE REPI                   TO REPT-REPORT-ID.
00556      MOVE ZEROS                  TO REPT-LINE-NUMBER.
00557      PERFORM 1400-FIND-REPORT THRU 1400-EXIT.
00558
00559      IF RF-LINE-NUMBER = ZERO
00560         MOVE ER-2545             TO EMI-ERROR
00561         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00562         MOVE -1                  TO OPTIONL
00563         MOVE AL-UNBON            TO OPTIONA
00564         GO TO 8110-SEND-DATA.
00565
00566      IF SCREEN-ERROR
00567          MOVE -1                 TO REPL
00568          MOVE AL-UABON           TO REPA
00569          GO TO 8110-SEND-DATA.
00570
00571      MOVE RF-LINE-NUMBER         TO PI-TOTAL-COUNT.
00572      MOVE LOW-VALUES             TO EL179BO.
00573      MOVE '1'                    TO PI-VIEW-SWITCH.
00574
00575      IF PI-START-COUNT GREATER ZERO
00576          NEXT SENTENCE
00577      ELSE
00578          MOVE 1                  TO PI-START-COUNT.
00579
00580      MOVE REPI                   TO PI-PRINT-REPORT.
00581      PERFORM 6000-BUILD-FIRST THRU 6000-EXIT.
00582      MOVE -1                     TO PFKEYBL.
00583      GO TO 8140-SEND-MAP-B.
00584      EJECT
00585  1300-DELETE-REPORT.
00586
00587      IF  NOT MODIFY-CAP
00588          MOVE 'UPDATE'           TO SM-READ
00589          PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
00590          MOVE ER-9096            TO EMI-ERROR
00591          PERFORM 9900-ERROR-FORMAT THRU 9910-EXIT
00592          MOVE -1                 TO OPTIONL
00593          GO TO 8110-SEND-DATA.
00594
00595      MOVE SPACES                 TO REPT-KEY.
00596      MOVE PI-COMPANY-CD          TO REPT-COMPANY-CODE.
00597      MOVE '2'                    TO REPT-RECORD-TYPE.
00598      MOVE REPI                   TO REPT-REPORT-ID.
00599      MOVE ZEROS                  TO REPT-LINE-NUMBER.
00600      PERFORM 1400-FIND-REPORT THRU 1400-EXIT.
00601
00602      IF SCREEN-ERROR
00603          MOVE -1                 TO REPL
00604          MOVE AL-UABON           TO REPA
00605          GO TO 8110-SEND-DATA.
00606
00607 ********************************  DELTETE TRAILER RECORD ********
00608      
      * EXEC CICS DELETE
00609 *        DATASET ('ELREPT')
00610 *        RIDFLD  (RF-CONTROL-PRIMARY)
00611 *        END-EXEC.
           MOVE 'ELREPT' TO DFHEIV1
      *    MOVE '&(  R                 &   #00003169' TO DFHEIV0
           MOVE X'262820205220202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303033313639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 RF-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00612
00613      MOVE '1'                    TO REPT-RECORD-TYPE.
00614      MOVE ZEROS                  TO REPT-LINE-NUMBER.
00615
00616  1305-READ-LOOP.
00617      
      * EXEC CICS READ
00618 *        SET     (ADDRESS OF REPORT-SAVE-FILE)
00619 *        DATASET ('ELREPT')
00620 *        RIDFLD  (REPT-KEY)
00621 *        GTEQ
00622 *        END-EXEC.
           MOVE 'ELREPT' TO DFHEIV1
      *    MOVE '&"S        G          (   #00003178' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033313738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 REPT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF REPORT-SAVE-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00623
00624      CONTINUE.
00625
00626      IF PI-COMPANY-CD = RF-COMPANY-CD  AND
00627         REPI          = RF-REPORT-ID
00628          MOVE RF-CONTROL-PRIMARY TO REPT-KEY
00629      ELSE
00630          GO TO 1310-DELETE-DETAIL-COMPLETE.
00631
00632      
      * EXEC CICS DELETE
00633 *        DATASET ('ELREPT')
00634 *        RIDFLD  (REPT-KEY)
00635 *    END-EXEC.
           MOVE 'ELREPT' TO DFHEIV1
      *    MOVE '&(  R                 &   #00003193' TO DFHEIV0
           MOVE X'262820205220202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303033313933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 REPT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00636
00637      ADD +1 TO REPT-LINE-NUMBER.
00638      MOVE REPT-LINE-NUMBER   TO  COUNT-4.
00639      IF COUNT-4  =  0
00640          
      * EXEC CICS
00641 *            SYNCPOINT
00642 *        END-EXEC.
      *    MOVE '6"                    !   #00003201' TO DFHEIV0
           MOVE X'362220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303033323031' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00643
00644      GO TO 1305-READ-LOOP.
00645
00646
00647  1310-DELETE-DETAIL-COMPLETE.
00648      MOVE SPACES                 TO OPTIONO REPO.
00649      MOVE ZEROS                  TO COUNT-1.
00650      PERFORM 2200-FILL-SCREEN THRU 2210-EXIT
00651          UNTIL SCREEN-FULL.
00652      MOVE ER-0000                TO EMI-ERROR.
00653      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00654      MOVE -1                     TO OPTIONL.
00655      GO TO 8110-SEND-DATA.
00656      EJECT
00657  1400-FIND-REPORT.
00658      
      * EXEC CICS HANDLE CONDITION
00659 *        NOTFND   (1400-REPORT-NOTFND)
00660 *        NOTOPEN  (1400-REPT-NOT-OPEN)
00661 *    END-EXEC.
      *    MOVE '"$IJ                  ! # #00003219' TO DFHEIV0
           MOVE X'2224494A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303033323139' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00662
00663      
      * EXEC CICS READ
00664 *        SET      (ADDRESS OF REPORT-SAVE-FILE)
00665 *        DATASET  ('ELREPT')
00666 *        RIDFLD   (REPT-KEY)
00667 *        GTEQ
00668 *    END-EXEC.
           MOVE 'ELREPT' TO DFHEIV1
      *    MOVE '&"S        G          (   #00003224' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033323234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 REPT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF REPORT-SAVE-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00669
00670      CONTINUE.
00671
00672      IF RF-REPORT-ID = REPI
00673          GO TO 1400-EXIT.
00674
00675  1400-REPORT-NOTFND.
00676      MOVE ER-0088                TO EMI-ERROR.
00677      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00678      MOVE 'X'                    TO ERROR-SWITCH.
00679      GO TO 1400-EXIT.
00680
00681  1400-REPT-NOT-OPEN.
00682      MOVE ER-0275                TO EMI-ERROR.
00683      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00684      MOVE 'X'                    TO ERROR-SWITCH.
00685
00686  1400-EXIT.
00687      EXIT.
00688      EJECT
00689  1500-FIND-PRINTER.
00690      
      * EXEC CICS HANDLE CONDITION
00691 *        NOTOPEN (1500-CNTL-NOT-OPEN)
00692 *    END-EXEC.
      *    MOVE '"$J                   ! $ #00003251' TO DFHEIV0
           MOVE X'22244A202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303033323531' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00693
00694      
      * EXEC CICS READ
00695 *        SET      (ADDRESS OF CONTROL-FILE)
00696 *        DATASET  ('ELCNTL')
00697 *        RIDFLD   (CNTL-KEY)
00698 *    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S        E          (   #00003255' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033323535' TO DFHEIV0(25:11)
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
           
00699
00700      CONTINUE.
00701      GO TO 1500-EXIT.
00702
00703  1500-CNTL-NOT-OPEN.
00704      MOVE ER-0042                TO EMI-ERROR.
00705      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00706      MOVE 'X'                    TO ERROR-SWITCH.
00707
00708  1500-EXIT.
00709      EXIT.
00710      EJECT
00711  1600-READ-PGMN.
00712      MOVE SPACES                 TO PGMN-SWITCH
00713
00714      
      * EXEC CICS HANDLE CONDITION
00715 *        NOTFND (1600-EXIT)
00716 *    END-EXEC.
      *    MOVE '"$I                   ! % #00003275' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303033323735' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00717
00718      
      * EXEC CICS READ
00719 *        SET      (ADDRESS OF PROGRAM-DESCRIPTIONS)
00720 *        DATASET  ('ELPGMN')
00721 *        RIDFLD   (PGMN-KEY)
00722 *    END-EXEC.
           MOVE 'ELPGMN' TO DFHEIV1
      *    MOVE '&"S        E          (   #00003279' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033323739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PGMN-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PROGRAM-DESCRIPTIONS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00723
00724      CONTINUE.
00725      MOVE '1'                    TO PGMN-SWITCH.
00726
00727  1600-EXIT.
00728      EXIT.
00729      EJECT
00730  2000-PAGE-FORWARD.
00731      MOVE ZEROS                  TO COUNT-1.
00732      MOVE SPACES                 TO SCREEN-SWITCH MSGO REPT-KEY.
00733      MOVE PI-COMPANY-CD          TO REPT-COMPANY-CODE.
00734      MOVE '2'                    TO REPT-RECORD-TYPE.
00735      MOVE ZEROS                  TO REPT-LINE-NUMBER.
00736
00737      IF REPL = ZEROS
00738          MOVE PI-SAVE-ENDING     TO REPT-REPORT-ID
00739      ELSE
00740          MOVE REPI               TO REPT-REPORT-ID.
00741
00742      
      * EXEC CICS HANDLE CONDITION
00743 *        NOTOPEN  (3500-NOT-OPEN)
00744 *        NOTFND   (2010-REC-NOT-FND)
00745 *    END-EXEC.
      *    MOVE '"$JI                  ! & #00003303' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303033333033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00746
00747      PERFORM 7000-STARTBR THRU 7000-EXIT
00748      PERFORM 2100-BUILD-FWD-PAGE THRU 2120-EXIT
00749          UNTIL SCREEN-FULL OR END-OF-FILE.
00750      PERFORM 7020-ENDBR THRU 7020-EXIT
00751
00752      MOVE -1                     TO OPTIONL.
00753
00754      IF PI-DELETE-SW NOT EQUAL 'Y'
00755          MOVE SPACES                 TO REPO.
00756
00757      GO TO 8110-SEND-DATA.
00758
00759  2010-REC-NOT-FND.
00760      MOVE HIGH-VALUES            TO PI-SAVE-BEGIN PI-SAVE-ENDING.
00761      PERFORM 2200-FILL-SCREEN THRU 2210-EXIT
00762          UNTIL SCREEN-FULL.
00763
00764      MOVE -1                     TO OPTIONL.
00765      MOVE ER-0130                TO EMI-ERROR.
00766
00767      PERFORM 9900-ERROR-FORMAT
00768          THRU 9900-EXIT.
00769
00770      GO TO 8110-SEND-DATA.
00771      EJECT
00772  2100-BUILD-FWD-PAGE.
00773      
      * EXEC CICS HANDLE CONDITION
00774 *        ENDFILE  (2110-END-FILE)
00775 *        NOTFND   (2110-END-FILE)
00776 *    END-EXEC.
      *    MOVE '"$''I                  ! '' #00003334' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303033333334' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00777
00778      PERFORM 7010-READNEXT THRU 7010-EXIT.
00779
00780      IF RF-RECORD-TYPE NOT = '2' OR
00781         RF-COMPANY-CD  NOT = PI-COMPANY-CD
00782          GO TO 2110-END-FILE.
00783
00784      MOVE SPACES                 TO DISPLAY-LINE.
00785      MOVE RF-REPORT-ID           TO REP-ID PGMN-KEY.
00786      MOVE RF-CURRENT-DATE        TO REP-CREATED.
00787      MOVE RF-PRINT-HH-MM-SS      TO TIME-UNFORMATTED.
00788      MOVE UN-HOURS               TO FOR-HOURS.
00789      MOVE UN-MINUTES             TO FOR-MINUTES.
00790      MOVE TIME-FORMATTED         TO REP-TIME.
00791      MOVE RF-LINE-NUMBER         TO CONVERT-LINES.
00792      MOVE CONVERT-LINES          TO HOLD-LINES.
00793      MOVE DISPLAY-LINES          TO EDIT-LINES.
00794      MOVE EDIT-LINES             TO REP-LINES.
00795      PERFORM 1600-READ-PGMN THRU 1600-EXIT.
00796
00797      IF PGMN-FOUND
00798         MOVE PN-PROGRAM-DESCRIPTION TO REP-NAME
00799      ELSE
00800         MOVE 'DESCRIPTION NOT FOUND' TO REP-NAME.
00801
00802      ADD 1 TO COUNT-1.
00803      MOVE DISPLAY-LINE           TO REPLINEO (COUNT-1).
00804
00805      IF COUNT-1 = 1
00806          MOVE RF-REPORT-ID       TO PI-SAVE-BEGIN.
00807
00808      IF COUNT-1 = 11
00809          MOVE RF-REPORT-ID       TO PI-SAVE-ENDING
00810          MOVE 'F'                TO SCREEN-SWITCH.
00811
00812      GO TO 2120-EXIT.
00813
00814  2110-END-FILE.
00815      PERFORM 2200-FILL-SCREEN THRU 2210-EXIT
00816          UNTIL SCREEN-FULL.
00817      MOVE ER-0130                TO EMI-ERROR.
00818      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00819      MOVE 'E'                    TO SCREEN-SWITCH.
00820
00821  2120-EXIT.
00822      EXIT.
00823
00824  2200-FILL-SCREEN.
00825      ADD 1 TO COUNT-1.
00826      MOVE SPACES                 TO REPLINEO (COUNT-1).
00827
00828      IF COUNT-1 GREATER THAN 10
00829          MOVE 'F'                TO SCREEN-SWITCH.
00830
00831  2210-EXIT.
00832      EXIT.
00833      EJECT
00834  3000-PAGE-BACKWARD.
00835      MOVE 11                     TO COUNT-1.
00836      MOVE SPACES                 TO SCREEN-SWITCH MSGO REPT-KEY.
00837      MOVE PI-COMPANY-CD          TO REPT-COMPANY-CODE.
00838      MOVE '2'                    TO REPT-RECORD-TYPE.
00839      MOVE ZEROS                  TO REPT-LINE-NUMBER.
00840
00841      IF REPL = ZEROS
00842          MOVE PI-SAVE-BEGIN      TO REPT-REPORT-ID
00843      ELSE
00844          MOVE REPI               TO REPT-REPORT-ID.
00845
00846      
      * EXEC CICS HANDLE CONDITION
00847 *        NOTOPEN  (3500-NOT-OPEN)
00848 *        NOTFND   (3020-REC-NOT-FND)
00849 *    END-EXEC.
      *    MOVE '"$JI                  ! ( #00003407' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303033343037' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00850
00851      PERFORM 7000-STARTBR THRU 7000-EXIT.
00852
00853      IF REPT-KEY = HIGH-VALUES
00854          GO TO 3010-SKIP-RESET.
00855
00856      PERFORM 7010-READNEXT THRU 7010-EXIT.
00857
00858      
      * EXEC CICS RESETBR
00859 *        DATASET  ('ELREPT')
00860 *        RIDFLD   (REPT-KEY)
00861 *        EQUAL
00862 *    END-EXEC.
           MOVE 'ELREPT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&4         E          &   #00003419' TO DFHEIV0
           MOVE X'263420202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303033343139' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 REPT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00863
00864  3010-SKIP-RESET.
00865      PERFORM 3100-BUILD-BCK-PAGE THRU 3120-EXIT
00866          UNTIL SCREEN-FULL OR END-OF-FILE.
00867      PERFORM 7020-ENDBR THRU 7020-EXIT
00868
00869      MOVE COUNT-1                TO COUNT-3.
00870      PERFORM 3130-RAISE-PAGE THRU 3140-EXIT
00871          COUNT-3 TIMES.
00872
00873      MOVE -1                     TO OPTIONL.
00874      MOVE SPACES                 TO REPO.
00875      GO TO 8110-SEND-DATA.
00876
00877
00878  3020-REC-NOT-FND.
00879      MOVE ER-0487                TO EMI-ERROR
00880      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00881      MOVE -1                     TO OPTIONL
00882      GO TO 8110-SEND-DATA.
00883      EJECT
00884  3100-BUILD-BCK-PAGE.
00885      
      * EXEC CICS HANDLE CONDITION
00886 *        ENDFILE  (3110-END-FILE)
00887 *        NOTFND   (3110-END-FILE)
00888 *    END-EXEC.
      *    MOVE '"$''I                  ! ) #00003446' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303033343436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00889
00890      PERFORM 7040-READPREV THRU 7040-EXIT.
00891
00892      IF RF-RECORD-TYPE NOT = '2' OR
00893         RF-COMPANY-CD  NOT = PI-COMPANY-CD
00894          GO TO 3110-END-FILE.
00895
00896      MOVE SPACES                 TO DISPLAY-LINE.
00897      MOVE RF-REPORT-ID           TO REP-ID PGMN-KEY.
00898      MOVE RF-CURRENT-DATE        TO REP-CREATED.
00899      MOVE RF-PRINT-HH-MM-SS      TO TIME-UNFORMATTED.
00900      MOVE UN-HOURS               TO FOR-HOURS.
00901      MOVE UN-MINUTES             TO FOR-MINUTES.
00902      MOVE TIME-FORMATTED         TO REP-TIME.
00903      MOVE RF-LINE-NUMBER         TO CONVERT-LINES.
00904      MOVE CONVERT-LINES          TO HOLD-LINES.
00905      MOVE DISPLAY-LINES          TO EDIT-LINES.
00906      MOVE EDIT-LINES             TO REP-LINES.
00907      PERFORM 1600-READ-PGMN THRU 1600-EXIT.
00908
00909      IF PGMN-FOUND
00910         MOVE PN-PROGRAM-DESCRIPTION TO REP-NAME
00911      ELSE
00912         MOVE 'DESCRIPTION NOT FOUND' TO REP-NAME.
00913
00914      MOVE DISPLAY-LINE           TO REPLINEO (COUNT-1).
00915
00916      IF COUNT-1 = 11
00917          MOVE RF-REPORT-ID       TO PI-SAVE-ENDING.
00918
00919      IF COUNT-1 = 1
00920          MOVE RF-REPORT-ID       TO PI-SAVE-BEGIN
00921          MOVE 'F'                TO SCREEN-SWITCH.
00922
00923      SUBTRACT 1 FROM COUNT-1.
00924      GO TO 3120-EXIT.
00925
00926  3110-END-FILE.
00927      MOVE RF-REPORT-ID           TO PI-SAVE-BEGIN.
00928      MOVE ER-0131                TO EMI-ERROR.
00929      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00930      MOVE 'E'                    TO SCREEN-SWITCH.
00931
00932  3120-EXIT.
00933      EXIT.
00934
00935
00936  3130-RAISE-PAGE.
00937      MOVE 1                      TO COUNT-1.
00938      MOVE 2                      TO COUNT-2.
00939      PERFORM 3150-SHIFT-SCREEN THRU 3160-EXIT
00940           UNTIL COUNT-1 = 11.
00941      MOVE SPACES                 TO REPLINEO (COUNT-1).
00942
00943  3140-EXIT.
00944      EXIT.
00945
00946  3150-SHIFT-SCREEN.
00947      MOVE REPLINEO (COUNT-2)     TO REPLINEO (COUNT-1).
00948      ADD 1 TO COUNT-1 COUNT-2.
00949
00950  3160-EXIT.
00951      EXIT.
00952      EJECT
00953  3500-NOT-OPEN.
00954      MOVE ER-0275                TO EMI-ERROR.
00955      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00956      GO TO 8110-SEND-DATA.
00957      EJECT
00958  5000-PROCESS-REVIEW.
00959      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00960          MOVE LOW-VALUES         TO EL179BO
00961          MOVE ER-7008            TO EMI-ERROR
00962          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00963          GO TO 8110-SEND-DATA.
00964
00965      
      * EXEC CICS RECEIVE
00966 *        MAP     ('EL179B')
00967 *        MAPSET  ('EL179S')
00968 *    END-EXEC.
           MOVE 'EL179B' TO DFHEIV1
           MOVE 'EL179S' TO DFHEIV2
      *    MOVE '8"T                   ''   #00003526' TO DFHEIV0
           MOVE X'382254202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033353236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EL179BI, 
                 DFHEIV99, 
                 DFHEIV2, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00969
00970      IF PFKEYBL GREATER THAN ZERO
00971          PERFORM 5010-TRANS-PF THRU 5010-EXIT.
00972
00973      IF SCREEN-ERROR
00974          MOVE ER-7008            TO EMI-ERROR
00975          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00976          GO TO 8150-SEND-DATA-B.
00977
00978      IF EIBAID = DFHPF1 OR DFHPF3
00979          GO TO 5100-PAGE-FORWARD.
00980
00981      IF EIBAID = DFHPF2 OR DFHPF4
00982          GO TO 5200-PAGE-BACKWARD.
00983
00984      IF EIBAID = DFHPF5
00985          GO TO 5500-RESET-TOP.
00986
00987      IF EIBAID = DFHPF6
00988          GO TO 5600-RESET-BOTTOM.
00989
00990      IF EIBAID = DFHPF7
00991          GO TO 5700-SHIFT-VIEW.
00992
00993      IF EIBAID = DFHPF12
00994          GO TO 8300-GET-HELP.
00995
00996      IF EIBAID = DFHPF23
00997          GO TO 8810-PF23-ENTERED.
00998
00999      IF EIBAID = DFHPF24
01000          GO TO 8400-RETURN-MASTER.
01001
01002      MOVE ER-0029                TO EMI-ERROR.
01003      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01004      MOVE -1                     TO PFKEYBL.
01005      GO TO 8150-SEND-DATA-B.
01006
01007  5010-TRANS-PF.
01008      IF EIBAID NOT = DFHENTER
01009          MOVE 'X'                TO ERROR-SWITCH
01010          GO TO 5010-EXIT.
01011
01012      IF PFKEYBI NOT NUMERIC
01013          MOVE 'X'                TO ERROR-SWITCH
01014          GO TO 5010-EXIT.
01015
01016      MOVE PFKEYBI                TO CHECK-PFKEYS.
01017
01018      IF CHECK-PFKEYS LESS 1 OR GREATER 24
01019          MOVE 'X'                TO ERROR-SWITCH
01020          GO TO 5010-EXIT.
01021
01022      MOVE AID-KEYS (CHECK-PFKEYS) TO EIBAID.
01023
01024  5010-EXIT.
01025      EXIT.
01026      EJECT
01027  5100-PAGE-FORWARD.
01028      IF EIBAID = DFHPF1
01029         ADD 16                   TO PI-START-COUNT
01030      ELSE
01031         ADD 5                    TO PI-START-COUNT.
01032
01033      IF PI-START-COUNT NOT LESS THAN PI-TOTAL-COUNT
01034          GO TO 5600-RESET-BOTTOM.
01035
01036      PERFORM 6000-BUILD-FIRST THRU 6000-EXIT.
01037      MOVE -1                     TO PFKEYBL.
01038      GO TO 8150-SEND-DATA-B.
01039
01040  5200-PAGE-BACKWARD.
01041      IF EIBAID = DFHPF2
01042         SUBTRACT 1               FROM PI-START-COUNT
01043      ELSE
01044         ADD  +10                 TO PI-START-COUNT.
01045
01046      IF PI-START-COUNT LESS THAN 1
01047          GO TO 5500-RESET-TOP.
01048
01049      IF PI-START-COUNT LESS THAN PI-TOTAL-COUNT
01050          NEXT SENTENCE
01051      ELSE
01052          GO TO 5600-RESET-BOTTOM.
01053
01054      PERFORM 6010-BUILD-FIRST THRU 6010-EXIT.
01055
01056      MOVE -1                     TO PFKEYBL.
01057      GO TO 8150-SEND-DATA-B.
01058
01059  5500-RESET-TOP.
01060      MOVE 1                      TO PI-START-COUNT.
01061      PERFORM 6000-BUILD-FIRST THRU 6000-EXIT.
01062      MOVE -1                     TO PFKEYBL.
01063      GO TO 8150-SEND-DATA-B.
01064      EJECT
01065  5600-RESET-BOTTOM.
01066      MOVE PI-TOTAL-COUNT TO PI-START-COUNT.
01067      SUBTRACT 1 FROM PI-START-COUNT.
01068      PERFORM 6010-BUILD-FIRST THRU 6010-EXIT
01069      MOVE -1 TO PFKEYBL.
01070      GO TO 8150-SEND-DATA-B.
01071
01072  5700-SHIFT-VIEW.
01073      IF PI-VIEW-SWITCH = '1'
01074          MOVE '2'                TO PI-VIEW-SWITCH
01075      ELSE
01076          MOVE '1'                TO PI-VIEW-SWITCH.
01077
01078      PERFORM 6000-BUILD-FIRST THRU 6000-EXIT.
01079      MOVE -1                     TO PFKEYBL.
01080      GO TO 8150-SEND-DATA-B.
01081      EJECT
01082  6000-BUILD-FIRST.
01083      MOVE PI-COMPANY-CD          TO REPT-COMPANY-CODE.
01084      MOVE '1'                    TO REPT-RECORD-TYPE.
01085      MOVE PI-PRINT-REPORT        TO REPT-REPORT-ID REPIDO PGMN-KEY
01086      MOVE PI-START-COUNT         TO REPT-LINE-NUMBER.
01087      PERFORM 1600-READ-PGMN THRU 1600-EXIT.
01088
01089      IF PGMN-FOUND
01090         MOVE PN-PROGRAM-DESCRIPTION TO REPNMO
01091      ELSE
01092         MOVE 'DESCRIPTION NOT FOUND' TO REPNMO.
01093
01094      MOVE ZEROS                  TO COUNT-1.
01095
01096      
      * EXEC CICS HANDLE CONDITION
01097 *        NOTFND   (6000-BUILD-NOT-FND)
01098 *        ENDFILE  (6000-BUILD-FIRST-END)
01099 *    END-EXEC.
      *    MOVE '"$I''                  ! * #00003657' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2A20233030303033363537' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01100
01101      PERFORM 7000-STARTBR THRU 7000-EXIT.
01102
01103  6000-BUILD-LOOP.
01104      
      * EXEC CICS HANDLE CONDITION
01105 *        NOTFND   (6000-BUILD-FIRST-END)
01106 *        ENDFILE  (6000-BUILD-FIRST-END)
01107 *    END-EXEC.
      *    MOVE '"$I''                  ! + #00003665' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2B20233030303033363635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01108
01109      PERFORM 7010-READNEXT THRU 7010-EXIT.
01110
01111      IF RF-COMPANY-CD  NOT = PI-COMPANY-CD  OR
01112         RF-RECORD-TYPE NOT = '1'            OR
01113         RF-REPORT-ID   NOT = PI-PRINT-REPORT
01114          GO TO 6000-BUILD-FIRST-END.
01115
01116      ADD 1 TO COUNT-1.
01117
01118      IF PI-VIEW-SWITCH = '1'
01119         MOVE RF-DATA-2-81        TO LINEREPO (COUNT-1)
01120      ELSE
01121         MOVE RF-DATA-55-133      TO LINEREPO (COUNT-1).
01122
01123      IF COUNT-1 = 1
01124          MOVE RF-LINE-NUMBER     TO PI-START-COUNT.
01125
01126      IF COUNT-1 LESS THAN 16
01127          GO TO 6000-BUILD-LOOP.
01128
01129      GO TO 6000-EXIT.
01130
01131  6000-BUILD-FIRST-END.
01132
01133      PERFORM 6200-BLANK-SCREEN THRU 6200-EXIT
01134          UNTIL COUNT-1 GREATER THAN 15.
01135      PERFORM 7020-ENDBR THRU 7020-EXIT.
01136      MOVE ER-0130                TO EMI-ERROR.
01137      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01138
01139      GO TO 6000-EXIT.
01140
01141  6000-BUILD-NOT-FND.
01142
01143      PERFORM 6200-BLANK-SCREEN THRU 6200-EXIT
01144          UNTIL COUNT-1 GREATER THAN 15.
01145      MOVE ER-3340                TO EMI-ERROR.
01146      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01147
01148      GO TO 6000-EXIT.
01149  6000-EXIT.
01150      EXIT.
01151      EJECT
01152  6010-BUILD-FIRST.
01153      MOVE PI-COMPANY-CD         TO REPT-COMPANY-CODE.
01154      MOVE '1'                   TO REPT-RECORD-TYPE.
01155      MOVE PI-PRINT-REPORT       TO REPT-REPORT-ID REPIDO PGMN-KEY.
01156      MOVE PI-START-COUNT        TO REPT-LINE-NUMBER.
01157      PERFORM 1600-READ-PGMN THRU 1600-EXIT.
01158
01159      IF PGMN-FOUND
01160         MOVE PN-PROGRAM-DESCRIPTION TO REPNMO
01161      ELSE
01162         MOVE 'DESCRIPTION NOT FOUND' TO REPNMO.
01163
01164      MOVE 16                     TO COUNT-1.
01165
01166      
      * EXEC CICS HANDLE CONDITION
01167 *        NOTFND   (6010-BUILD-FIRST-END)
01168 *        ENDFILE  (6010-BUILD-FIRST-END)
01169 *    END-EXEC.
      *    MOVE '"$I''                  ! , #00003727' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2C20233030303033373237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01170
01171      PERFORM 7030-STARTBR THRU 7030-EXIT.
01172
01173  6010-BUILD-LOOP.
01174      PERFORM 7040-READPREV THRU 7040-EXIT.
01175
01176      IF RF-COMPANY-CD  NOT = PI-COMPANY-CD OR
01177         RF-RECORD-TYPE NOT = '1'           OR
01178         RF-REPORT-ID   NOT = PI-PRINT-REPORT
01179          GO TO 6010-BUILD-FIRST-END.
01180
01181      IF PI-VIEW-SWITCH = '1'
01182         MOVE RF-DATA-2-81        TO LINEREPO (COUNT-1)
01183      ELSE
01184         MOVE RF-DATA-55-133      TO LINEREPO (COUNT-1).
01185
01186      SUBTRACT 1 FROM COUNT-1.
01187
01188      IF COUNT-1 LESS THAN 1
01189         MOVE RF-LINE-NUMBER         TO PI-START-COUNT
01190         PERFORM 7020-ENDBR THRU 7020-EXIT
01191         GO TO 6010-EXIT.
01192
01193      GO TO 6010-BUILD-LOOP.
01194
01195  6010-BUILD-FIRST-END.
01196      MOVE   1                    TO PI-START-COUNT.
01197      PERFORM 7020-ENDBR THRU 7020-EXIT.
01198      MOVE ER-0131                TO EMI-ERROR.
01199      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01200      PERFORM 6000-BUILD-FIRST
01201         THRU 6000-EXIT.
01202
01203  6010-EXIT.
01204      EXIT.
01205      EJECT
01206  6200-BLANK-SCREEN.
01207      ADD 1 TO COUNT-1.
01208      MOVE SPACES                 TO LINEREPO (COUNT-1).
01209
01210  6200-EXIT.
01211      EXIT.
01212
01213      EJECT
01214  6500-PAGE-SEARCH.
01215      MOVE PI-COMPANY-CD         TO REPT-COMPANY-CODE.
01216      MOVE '1'                   TO REPT-RECORD-TYPE.
01217      MOVE PI-PRINT-REPORT       TO REPT-REPORT-ID REPIDO PGMN-KEY.
01218      MOVE PI-START-COUNT        TO REPT-LINE-NUMBER.
01219      PERFORM 1600-READ-PGMN THRU 1600-EXIT.
01220
01221      IF PGMN-FOUND
01222         MOVE PN-PROGRAM-DESCRIPTION TO REPNMO
01223      ELSE
01224         MOVE 'DESCRIPTION NOT FOUND' TO REPNMO.
01225
01226      MOVE ZEROS                  TO COUNT-1.
01227
01228      
      * EXEC CICS HANDLE CONDITION
01229 *
01230 *        ENDFILE  (6500-NOTFND)
01231 *    END-EXEC.
      *    MOVE '"$''                   ! - #00003789' TO DFHEIV0
           MOVE X'222427202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2D20233030303033373839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01232
01233      PERFORM 7000-STARTBR THRU 7000-EXIT.
01234
01235  6500-PAGE-LOOP.
01236      PERFORM 7010-READNEXT THRU 7010-EXIT.
01237
01238      IF RF-COMPANY-CD  NOT = PI-COMPANY-CD  OR
01239         RF-RECORD-TYPE NOT = '1'            OR
01240         RF-REPORT-ID   NOT = PI-PRINT-REPORT
01241          GO TO 6500-NOTFND.
01242
01243      IF RF-CTL-CHAR-133 = '1'
01244          ADD +1  TO  PI-ACCUM-PAGE-NO.
01245
01246      IF PI-ACCUM-PAGE-NO LESS THAN PI-START-PAGE-NO
01247          GO TO 6500-PAGE-LOOP.
01248
01249      MOVE REPT-LINE-NUMBER       TO PI-START-COUNT.
01250
01251  6500-NOTFND.
01252      IF REPT-LINE-NUMBER = PI-START-COUNT
01253          PERFORM 7020-ENDBR THRU 7020-EXIT
01254          GO TO 6500-EXIT
01255      ELSE
01256          MOVE ER-2359            TO EMI-ERROR
01257          MOVE -1                 TO STPAGEL
01258          MOVE 'X'                TO ERROR-SWITCH
01259          MOVE AL-UNBON           TO STPAGEA
01260          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01261
01262      PERFORM 7020-ENDBR THRU 7020-EXIT.
01263
01264      IF SCREEN-ERROR
01265          GO TO 8110-SEND-DATA.
01266
01267  6500-EXIT.
01268      EXIT.
01269
01270      EJECT
01271  7000-STARTBR.
01272      
      * EXEC CICS STARTBR
01273 *        DATASET  ('ELREPT')
01274 *        RIDFLD   (REPT-KEY)
01275 *    END-EXEC.
           MOVE 'ELREPT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00003833' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383333' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 REPT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01276
01277      MOVE 'Y'                 TO PI-ELREPT-BROWSE-SW.
01278
01279  7000-EXIT.
01280      EXIT.
01281      EJECT
01282  7010-READNEXT.
01283      
      * EXEC CICS READNEXT
01284 *        SET      (ADDRESS OF REPORT-SAVE-FILE)
01285 *        DATASET  ('ELREPT')
01286 *        RIDFLD   (REPT-KEY)
01287 *    END-EXEC.
           MOVE 'ELREPT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00003844' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 REPT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF REPORT-SAVE-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01288
01289      CONTINUE.
01290
01291  7010-EXIT.
01292      EXIT.
01293      EJECT
01294  7020-ENDBR.
01295      IF STARTED-BROWSE
01296         
      * EXEC CICS ENDBR
01297 *           DATASET ('ELREPT')
01298 *       END-EXEC.
           MOVE 'ELREPT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00003857' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383537' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01299
01300      MOVE SPACES               TO PI-ELREPT-BROWSE-SW.
01301
01302  7020-EXIT.
01303      EXIT.
01304      EJECT
01305  7030-STARTBR.
01306      
      * EXEC CICS STARTBR
01307 *        DATASET  ('ELREPT')
01308 *        RIDFLD   (REPT-KEY)
01309 *        EQUAL
01310 *    END-EXEC.
           MOVE 'ELREPT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         E          &   #00003867' TO DFHEIV0
           MOVE X'262C20202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 REPT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01311
01312      MOVE 'Y'                  TO PI-ELREPT-BROWSE-SW.
01313
01314  7030-EXIT.
01315      EXIT.
01316      EJECT
01317  7040-READPREV.
01318      
      * EXEC CICS READPREV
01319 *        SET      (ADDRESS OF REPORT-SAVE-FILE)
01320 *        DATASET  ('ELREPT')
01321 *        RIDFLD   (REPT-KEY)
01322 *    END-EXEC.
           MOVE 'ELREPT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00003879' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 REPT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF REPORT-SAVE-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01323
01324      CONTINUE.
01325
01326  7040-EXIT.
01327      EXIT.
01328      EJECT
01329  8100-SEND-MAP.
01330      PERFORM 8120-FORMAT-TIME-DATE THRU 8130-EXIT.
01331
01332      
      * EXEC CICS SEND
01333 *        MAP     ('EL179A')
01334 *        MAPSET  ('EL179S')
01335 *        ERASE
01336 *        FREEKB
01337 *        CURSOR
01338 *    END-EXEC.
           MOVE 'EL179A' TO DFHEIV1
           MOVE -1
             TO DFHEIV11
           MOVE 'EL179S' TO DFHEIV2
      *    MOVE '8$     CT  E F  H     ,   #00003893' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202020202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EL179AO, 
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
           
01339
01340      GO TO 9000-RETURN-TRANS.
01341
01342  8110-SEND-DATA.
01343
01344      PERFORM 8120-FORMAT-TIME-DATE THRU 8130-EXIT.
01345
01346      
      * EXEC CICS SEND
01347 *        MAP     ('EL179A')
01348 *        MAPSET  ('EL179S')
01349 *        ERASE
01350 *        FREEKB
01351 *        CURSOR
01352 *    END-EXEC.
           MOVE 'EL179A' TO DFHEIV1
           MOVE -1
             TO DFHEIV11
           MOVE 'EL179S' TO DFHEIV2
      *    MOVE '8$     CT  E F  H     ,   #00003907' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202020202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393037' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EL179AO, 
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
           
01353
01354      GO TO 9000-RETURN-TRANS.
01355
01356  8120-FORMAT-TIME-DATE.
01357      MOVE SAVE-DATE              TO DATEO.
01358      
      * EXEC CICS ASKTIME ABSTIME(LCP-CICS-TIME)
01359 *    END-EXEC
      *    MOVE '0"A                   "   #00003919' TO DFHEIV0
           MOVE X'302241202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202220' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393139' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCP-CICS-TIME
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01360      
      * EXEC CICS FORMATTIME
01361 *              ABSTIME(LCP-CICS-TIME)
01362 *              TIME(LCP-TIME-OF-DAY-XX)
01363 *    END-EXEC
      *    MOVE 'j$(     (             #   #00003921' TO DFHEIV0
           MOVE X'6A2428202020202028202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCP-CICS-TIME, 
                 LCP-TIME-OF-DAY-XX
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01364      MOVE  LCP-TIME-OF-DAY-68 TO TIME-UNFORMATTED.
01365      MOVE UN-HOURS               TO FOR-HOURS.
01366      MOVE UN-MINUTES             TO FOR-MINUTES.
01367      MOVE TIME-FORMATTED         TO TIMEO.
01368      MOVE LIT-MAP                TO PI-CURRENT-SCREEN-NO.
01369      MOVE EMI-MESSAGE-AREA (1)   TO MSGO.
01370      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.
01371
01372  8130-EXIT.
01373      EXIT.
01374
01375  8140-SEND-MAP-B.
01376      PERFORM 8160-FORMAT-TIME-DATE THRU 8170-EXIT.
01377
01378      
      * EXEC CICS SEND
01379 *        MAP     ('EL179B')
01380 *        MAPSET  ('EL179S')
01381 *        ERASE
01382 *        FREEKB
01383 *        CURSOR
01384 *    END-EXEC.
           MOVE 'EL179B' TO DFHEIV1
           MOVE -1
             TO DFHEIV11
           MOVE 'EL179S' TO DFHEIV2
      *    MOVE '8$     CT  E F  H     ,   #00003939' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202020202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393339' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EL179BO, 
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
           
01385
01386      GO TO 9000-RETURN-TRANS.
01387
01388  8150-SEND-DATA-B.
01389      PERFORM 8160-FORMAT-TIME-DATE THRU 8170-EXIT.
01390
01391      
      * EXEC CICS SEND
01392 *        MAP     ('EL179B')
01393 *        MAPSET  ('EL179S')
01394 *        DATAONLY
01395 *        FREEKB
01396 *        CURSOR
01397 *    END-EXEC.
           MOVE 'EL179B' TO DFHEIV1
           MOVE -1
             TO DFHEIV11
           MOVE 'EL179S' TO DFHEIV2
      *    MOVE '8$D    CT    F  H     ,   #00003952' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'204620204820202020202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EL179BO, 
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
           
01398
01399      GO TO 9000-RETURN-TRANS.
01400
01401  8160-FORMAT-TIME-DATE.
01402      MOVE SAVE-DATE              TO DATEBO.
01403      
      * EXEC CICS ASKTIME ABSTIME(LCP-CICS-TIME)
01404 *    END-EXEC
      *    MOVE '0"A                   "   #00003964' TO DFHEIV0
           MOVE X'302241202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202220' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCP-CICS-TIME
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01405      
      * EXEC CICS FORMATTIME
01406 *              ABSTIME(LCP-CICS-TIME)
01407 *              TIME(LCP-TIME-OF-DAY-XX)
01408 *    END-EXEC
      *    MOVE 'j$(     (             #   #00003966' TO DFHEIV0
           MOVE X'6A2428202020202028202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393636' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCP-CICS-TIME, 
                 LCP-TIME-OF-DAY-XX
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01409      MOVE  LCP-TIME-OF-DAY-68 TO TIME-UNFORMATTED.
01410      MOVE UN-HOURS               TO FOR-HOURS.
01411      MOVE UN-MINUTES             TO FOR-MINUTES.
01412      MOVE TIME-FORMATTED         TO TIMEBO.
01413      MOVE LIT-MAP-2              TO PI-CURRENT-SCREEN-NO.
01414      MOVE EMI-MESSAGE-AREA (1)   TO MSGBO.
01415      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.
01416
01417  8170-EXIT.
01418      EXIT.
01419
01420  8200-RETURN-PRIOR.
01421      IF PI-CURRENT-SCREEN-NO = LIT-MAP-2
01422          MOVE HIGH-VALUES        TO PI-SAVE-BEGIN
01423          MOVE LOW-VALUES         TO PI-SAVE-ENDING
01424          MOVE LOW-VALUES         TO EL179AO
01425          MOVE -1                 TO OPTIONL
01426          GO TO 2000-PAGE-FORWARD.
01427
01428      MOVE SPACE                  TO PI-RETURN-CD-1.
01429      MOVE PI-RETURN-TO-PROGRAM   TO CALL-PGM.
01430      GO TO 9200-XCTL.
01431
01432  8300-GET-HELP.
01433      MOVE XCTL-EL010             TO CALL-PGM.
01434      GO TO 9200-XCTL.
01435
01436  8400-RETURN-MASTER.
01437      MOVE SPACE                  TO PI-RETURN-CD-1.
01438
01439      IF  CREDIT-SESSION
01440              OR
01441          CLAIM-SESSION
01442          MOVE XCTL-EL126         TO CALL-PGM
01443
01444      ELSE
01445          IF  MORTGAGE-SESSION
01446              MOVE XCTL-EM626     TO CALL-PGM
01447
01448          ELSE
01449              IF  WARRANTY-SESSION
01450                  MOVE XCTL-GL800 TO CALL-PGM.
01451
01452      GO TO 9200-XCTL.
01453
01454  8800-UNAUTHORIZED-ACCESS.
01455      MOVE UNACCESS-MSG           TO LOGOFF-MSG.
01456      GO TO 8990-SEND-TEXT.
01457
01458  8810-PF23-ENTERED.
01459      MOVE EIBAID                 TO PI-ENTRY-CD-1.
01460      MOVE XCTL-EL005             TO CALL-PGM.
01461      GO TO 9200-XCTL.
01462
01463  8820-XCTL-ERROR.
01464      
      * EXEC CICS HANDLE CONDITION
01465 *        PGMIDERR (8990-SEND-TEXT)
01466 *    END-EXEC.
      *    MOVE '"$L                   ! . #00004025' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2E20233030303034303235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01467
01468      MOVE SPACE                  TO PI-ENTRY-CD-1.
01469      MOVE CALL-PGM               TO PI-CALLING-PROGRAM LOGOFF-PGM.
01470      MOVE XCTL-EL005             TO CALL-PGM.
01471      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.
01472      GO TO 9200-XCTL.
01473
01474  8990-SEND-TEXT.
01475      
      * EXEC CICS SEND TEXT
01476 *        FROM    (LOGOFF-TEXT)
01477 *        LENGTH  (LOGOFF-LENGTH)
01478 *        ERASE
01479 *        FREEKB
01480 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00004036' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303034303336' TO DFHEIV0(25:11)
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
           
01481
01482      GO TO 9100-RETURN-CICS.
01483      EJECT
01484  9000-RETURN-TRANS.
01485      
      * EXEC CICS RETURN
01486 *        TRANSID   (TRANS-ID)
01487 *        COMMAREA  (PROGRAM-INTERFACE-BLOCK)
01488 *        LENGTH    (PI-COMM-LENGTH)
01489 *    END-EXEC.
      *    MOVE '.(CT                  &   #00004046' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034303436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01490
01491  9100-RETURN-CICS.
01492      
      * EXEC CICS RETURN
01493 *    END-EXEC.
      *    MOVE '.(                    &   #00004053' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034303533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01494
01495      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL179' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
01496
01497  9200-XCTL.
01498      
      * EXEC CICS XCTL
01499 *        PROGRAM   (CALL-PGM)
01500 *        COMMAREA  (PROGRAM-INTERFACE-BLOCK)
01501 *        LENGTH    (PI-COMM-LENGTH)
01502 *    END-EXEC.
      *    MOVE '.$C                   $   #00004059' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034303539' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CALL-PGM, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01503
01504  9700-LINK-DATE-CONVERT.
01505      
      * EXEC CICS LINK
01506 *        PROGRAM    ('ELDATCV')
01507 *        COMMAREA   (DATE-CONVERSION-DATA)
01508 *        LENGTH     (DC-COMM-LENGTH)
01509 *        END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   ''   #00004066' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034303636' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01510
01511  9700-EXIT.
01512      EXIT.
01513
01514  9900-ERROR-FORMAT.
01515      IF EMI-ERRORS-COMPLETE
01516          GO TO 9900-EXIT.
01517
01518      
      * EXEC CICS LINK
01519 *        PROGRAM   ('EL001')
01520 *        COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)
01521 *        LENGTH    (EMI-COMM-LENGTH)
01522 *    END-EXEC.
           MOVE 'EL001' TO DFHEIV1
      *    MOVE '."C                   ''   #00004079' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034303739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01523
01524  9900-EXIT.
01525      EXIT.
01526      EJECT
01527  9910-INITIALIZE-SECURITY.
01528 ******************************************************************
01529 *                                                                *
01530 *       THIS LOGIC SETS UP AND READS THE TEMPORARY STORAGE       *
01531 *       USER SECURITY RECORD SET UP BY EL125.  THIS PROGRAM      *
01532 *       MOVES THE APPROPRIATE APPLICATION AUTHORIZATION INTO     *
01533 *       PI-DISPLAY-CAP AND PI-MODIFY-CAP.  IT THEN CHECKS FOR    *
01534 *       BROWSE AUTHORITY AND IF NOT AUTHORIZED WILL SET AN       *
01535 *       ERROR CONDITION AND EXITS THE PROGRAM.                   *
01536 *                                                                *
01537 ******************************************************************
01538
01539      IF  PI-PROCESSOR-ID NOT EQUAL 'LGXX'
01540
01541          IF  MORTGAGE-SESSION
01542              MOVE '125E'                 TO SC-QUID-SYSTEM
01543              MOVE EIBTRMID               TO SC-QUID-TERMINAL
01544
01545              
      * EXEC CICS READQ TS
01546 *                QUEUE  (SC-QUID-KEY)
01547 *                INTO   (SECURITY-CONTROL-E)
01548 *                LENGTH (SC-COMM-LENGTH-E)
01549 *                ITEM   (SC-ITEM)
01550 *            END-EXEC
      *    MOVE '*$II   L              ''   #00004106' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313036' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 SC-QUID-KEY, 
                 SECURITY-CONTROL-E, 
                 SC-COMM-LENGTH-E, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01551
01552              MOVE SC-MP-DISPLAY (W-APPL-SCRTY-NDX)
01553                                      TO PI-DISPLAY-CAP
01554              MOVE SC-MP-UPDATE (W-APPL-SCRTY-NDX)
01555                                      TO PI-MODIFY-CAP
01556
01557              IF  NOT DISPLAY-CAP
01558                  MOVE 'READ'         TO SM-READ
01559                  PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
01560                  MOVE ER-9097        TO EMI-ERROR
01561                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01562                  GO TO 8100-SEND-MAP
01563
01564              ELSE
01565                  NEXT SENTENCE
01566          ELSE
01567              IF CLAIM-SESSION
01568                  
      * EXEC CICS READQ TS
01569 *                    QUEUE    (PI-SECURITY-TEMP-STORE-ID)
01570 *                    INTO     (SECURITY-CONTROL)
01571 *                    LENGTH   (SC-COMM-LENGTH)
01572 *                    ITEM     (SC-ITEM)
01573 *                END-EXEC
      *    MOVE '*$II   L              ''   #00004129' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313239' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01574              MOVE SC-CLAIMS-DISPLAY (27)  TO  PI-DISPLAY-CAP
01575              MOVE SC-CLAIMS-UPDATE  (27)  TO  PI-MODIFY-CAP
01576              IF NOT DISPLAY-CAP
01577                  MOVE 'READ'              TO  SM-READ
01578                  PERFORM 9995-SECURITY-VIOLATION
01579                  MOVE ER-0070             TO  EMI-ERROR
01580                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01581                  GO TO 8100-SEND-MAP
01582              ELSE
01583                  NEXT SENTENCE
01584          ELSE
01585              MOVE 'Y'                TO PI-DISPLAY-CAP
01586                                         PI-MODIFY-CAP.
01587
01588  9910-EXIT.
01589      EXIT.
01590      EJECT
01591  9990-ABEND.
01592      MOVE DFHEIBLK               TO EMI-LINE1.
01593
01594      
      * EXEC CICS LINK
01595 *        PROGRAM   ('EL004')
01596 *        COMMAREA  (EMI-LINE1)
01597 *        LENGTH    (72)
01598 *    END-EXEC.
           MOVE 'EL004' TO DFHEIV1
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   ''   #00004155' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01599
01600      GO TO 8110-SEND-DATA.
01601      EJECT
01602  9995-SECURITY-VIOLATION.
01603 ******************************************************************
01604 *                                                                *
01605 *    THIS LOGIC CONTAINS THE COMMON SECURITY-MESSAGE LINK        *
01606 *    THE COPYBOOK ELCSCTM MUST RESIDE IN WORKING-STORAGE.        *
01607 *                                                                *
01608 *    THE FOLLOWING ADDITION FIELDS MUST RESIDE IN WORKING-       *
01609 *                                                                *
01610 *    THIS-PGM            PIC  X(08)                              *
01611 *                                                                *
01612 *    W-TIME-IN           PIC S9(07).                             *
01613 *    FILLER   REDEFINES W-TIME-IN.                               *
01614 *    FILLER              PIC  X(01).                             *
01615 *    W-TIME-OUT          PIC  9(02)V9(02).                       *
01616 *    FILLER              PIC  X(02).                             *
01617 *                                                                *
01618 ******************************************************************
01619
01620      MOVE EIBDATE          TO SM-JUL-DATE.
01621      MOVE EIBTRMID         TO SM-TERMID.
01622      MOVE THIS-PGM         TO SM-PGM.
01623      MOVE EIBTIME          TO W-TIME-IN.
01624      MOVE W-TIME-OUT       TO SM-TIME.
01625      MOVE PI-PROCESSOR-ID  TO SM-PROCESSOR-ID.
01626
01627      
      * EXEC CICS LINK
01628 *         PROGRAM  ('EL003')
01629 *         COMMAREA (SECURITY-MESSAGE)
01630 *         LENGTH   (80)
01631 *    END-EXEC.
           MOVE 'EL003' TO DFHEIV1
           MOVE 80
             TO DFHEIV11
      *    MOVE '."C                   ''   #00004188' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313838' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 SECURITY-MESSAGE, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01632
01633  9995-EXIT.
01634      EXIT.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL179' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 8820-XCTL-ERROR,
                     1110-TERMID-ERROR,
                     1120-TRANS-ERROR,
                     9990-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 1400-REPORT-NOTFND,
                     1400-REPT-NOT-OPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 1500-CNTL-NOT-OPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 1600-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 3500-NOT-OPEN,
                     2010-REC-NOT-FND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 2110-END-FILE,
                     2110-END-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 3500-NOT-OPEN,
                     3020-REC-NOT-FND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 3110-END-FILE,
                     3110-END-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 6000-BUILD-NOT-FND,
                     6000-BUILD-FIRST-END
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 6000-BUILD-FIRST-END,
                     6000-BUILD-FIRST-END
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 12
               GO TO 6010-BUILD-FIRST-END,
                     6010-BUILD-FIRST-END
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 13
               GO TO 6500-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 14
               GO TO 8990-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL179' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
