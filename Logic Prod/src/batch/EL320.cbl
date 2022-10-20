00001  IDENTIFICATION DIVISION.                                         03/10/98
00002                                                                   EL320
00003  PROGRAM-ID.                 EL320 .                                 LV003
00004 *              PROGRAM CONVERTED BY                               EL320
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL320
00006 *              CONVERSION DATE 02/14/96 13:37:20.                 EL320
00007 *           PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE             EL320
00008 *                            VMOD=2.019                           EL320
00009                                                                   EL320
00009                                                                   EL320
00010 *AUTHOR.        LOGIC INC.                                        EL320
00011 *               DALLAS, TEXAS.                                    EL320
00012                                                                   EL320
00013 *DATE-COMPILED.                                                   EL320
00014                                                                   EL320
00015 *SECURITY.   *****************************************************EL320
00016 *            *                                                   *EL320
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *EL320
00018 *            *                                                   *EL320
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL320
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL320
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *EL320
00022 *            *                                                   *EL320
00023 *            *****************************************************EL320
00024                                                                   EL320
00025 *REMARKS.                                                         EL320
00026 *        THIS PROGRAM WILL PRODUCE THE FOLLOWING REPORTS          EL320
00027 *                                                                 EL320
00028 *        FROM THE ELCNTL  FILE:                                   EL320
00029 *            REPORT OF  COMPANY   RECORDS         ELCNTL          EL320
00030 *            REPORT OF  CARRIER   RECORDS         ELCNTL          EL320
00031 *            REPORT OF  PROCESSOR RECORDS         ELCNTL          EL320
00032 *            REPORT OF  STATE     RECORDS         ELCNTL          EL320
00033 *            REPORT OF  LIFE BENEFIT RECORDS      ELCNTL          EL320
00034 *            REPORT OF  A-H  BENEFIT RECORDS      ELCNTL          EL320
00035 *                                                                 EL320
00036 *        FROM THE ELPGMS  FILE:                                   EL320
00037 *            REPORT OF  PROGRAM OPTIONS RECORDS   ELPGMS          EL320
00038 *                                                                 EL320
00039 *        FROM THE ELFORM  FILE:                                   EL320
00040 *            REPORT OF  FORM DESC RECORDS         ELFORM          EL320
00041 *                                                                 EL320
00042 *        FROM THE ELLETR  FILE:                                   EL320
00043 *            REPORT OF  LETTER DESC RECORDS       ELLETR          EL320
00044 *                                                                 EL320
00045 *        ELCNTL IS SEARCHED, AND FOR EVERY COMPANY RECORD         EL320
00046 *        THE ABOVE REPORTS ARE PRODUCED (IF ANY DATA EXISTS)      EL320
00047 *        FOR ALL COMPANIES                                        EL320
00048 *                                                                 EL320
00049 *        LATER MODIFICATION,                                      EL320
00050 *            SELECTIVE CONTROL THROUGH A DATE-CARD                EL320
00051      EJECT                                                        EL320
00052  ENVIRONMENT DIVISION.                                            EL320
00053  INPUT-OUTPUT SECTION.                                            EL320
00054  FILE-CONTROL.                                                    EL320
00055                                                                   EL320
00056      SELECT PRINTF       ASSIGN TO SYS008-UR-1403-S-SYS008.       EL320
00057                                                                   EL320
00058      SELECT DISK-DATE    ASSIGN TO SYS019-FBA1-S-SYS019.          EL320
00059                                                                   EL320
00060      SELECT FICH         ASSIGN TO SYS020-UT-2400-S-SYS020.       EL320
00061                                                                   EL320
00062      SELECT ELCNTLF      ASSIGN TO SYS020-FBA1-ELCNTL             EL320
00063                          ORGANIZATION IS INDEXED                  EL320
00064                          ACCESS IS DYNAMIC                        EL320
00065                          RECORD KEY IS CF-CONTROL-PRIMARY         EL320
00066                          FILE STATUS IS ELCNTL-STATUS.            EL320
00067                                                                   EL320
00068      SELECT ELPGMSF      ASSIGN TO SYS021-FBA1-ELPGMS             EL320
00069                          ORGANIZATION IS INDEXED                  EL320
00070                          ACCESS IS DYNAMIC                        EL320
00071                          RECORD KEY IS PS-CONTROL-PRIMARY         EL320
00072                          FILE STATUS IS ELPGMS-STATUS.            EL320
00073                                                                   EL320
00074      SELECT ELFORMF      ASSIGN TO SYS022-FBA1-ELFORM             EL320
00075                          ORGANIZATION IS INDEXED                  EL320
00076                          ACCESS IS DYNAMIC                        EL320
00077                          RECORD KEY IS TX-CONTROL-PRIMARY         EL320
00078                          FILE STATUS IS ELFORM-STATUS.            EL320
00079                                                                   EL320
00080      SELECT ELLETRF      ASSIGN TO SYS023-FBA1-ELLETR             EL320
00081                          ORGANIZATION IS INDEXED                  EL320
00082                          ACCESS IS DYNAMIC                        EL320
00083                          RECORD KEY IS LT-CONTROL-PRIMARY         EL320
00084                          FILE STATUS IS ELLETR-STATUS.            EL320
00085                                                                   EL320
00086      SELECT ELREPT       ASSIGN TO SYS024-FBA1-ELREPT             EL320
00087                          ORGANIZATION IS INDEXED                  EL320
00088                          ACCESS IS DYNAMIC                        EL320
00089                          RECORD KEY IS RF-CONTROL-PRIMARY         EL320
00090                          FILE STATUS IS DTE-VSAM-FLAGS.           EL320
00091      EJECT                                                        EL320
00092  DATA DIVISION.                                                   EL320
00093  FILE SECTION.                                                    EL320
00094                                                                   EL320
00095  FD  PRINTF                                                       EL320
00096                              COPY ELCPRTFD.                       EL320
00097      EJECT                                                        EL320
00098  FD  DISK-DATE                                                    EL320
00099                              COPY ELCDTEFD.                       EL320
00100      EJECT                                                        EL320
00101  FD  FICH                                                         EL320
00102                              COPY ELCFCHFD.                       EL320
00103      EJECT                                                        EL320
00104  FD  ELCNTLF.                                                     EL320
00105                              COPY ELCCNTL.                        EL320
00106      EJECT                                                        EL320
00107  FD  ELPGMSF.                                                     EL320
00108                                                                   EL320
00109                              COPY ELCPGMS.                        EL320
00110      EJECT                                                        EL320
00111  FD  ELFORMF.                                                     EL320
00112                              COPY ELCTEXT.                        EL320
00113      EJECT                                                        EL320
00114  FD  ELLETRF.                                                     EL320
00115                              COPY ELCTEXT                         EL320
00116          REPLACING TEXT-FILE-ID         BY  LETR-FILE-ID          EL320
00117                    FORMS-FILE-TEXT      BY  LT-FORM-FILE-TEXT     EL320
00118                    LETTER-FILE-TEXT     BY  LT-LETTER-FILE-TEXT   EL320
00119                    HELP-FILE-TEXT       BY  LT-HELP-FILE-TEXT     EL320
00120                    TX-CONTROL-PRIMARY   BY  LT-CONTROL-PRIMARY    EL320
00121                    TX-COMPANY-CD        BY  LT-COMPANY-CD         EL320
00122                    TX-ACCESS-CD-GENL    BY  LT-ACCESS-CD-GENL     EL320
00123                    TX-LETTER-ACCESS     BY  LT-LETTER-ACCESS      EL320
00124                    TX-LETTER-NO         BY  LT-LETTER-NO          EL320
00125                    TX-FORM-ACCESS       BY  LT-FORM-ACCESS        EL320
00126                    TX-FORM-NO           BY  LT-FORM-NO            EL320
00127                    TX-HELP-ACCESS       BY  LT-HELP-ACCESS        EL320
00128                    TX-HELP-TYPE         BY  LT-HELP-TYPE          EL320
00129                    TX-SCREEN-OR-ERROR   BY  LT-SCREEN-OR-ERROR    EL320
00130                    TX-HELP-FOR-COMPANY  BY  LT-HELP-FOR-COMPANY   EL320
00131                    TX-LINE-SEQUENCE     BY  LT-LINE-SEQUENCE      EL320
00132                    TX-PROCESS-CONTROL   BY  LT-PROCESS-CONTROL    EL320
00133                    TX-TEXT-LINE         BY  LT-TEXT-LINE          EL320
00134                    TX-LINE-SQUEEZE-CONTROL                        EL320
00135                                      BY  LT-LINE-SQUEEZE-CONTROL. EL320
00136      EJECT                                                        EL320
00137  FD  ELREPT                  COPY ELCRPTFD.                       EL320
00138                              COPY ELCREPT.                        EL320
00139      EJECT                                                        EL320
00140  WORKING-STORAGE SECTION.                                         EL320
00141  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      EL320
00142  77  FILLER  PIC X(32) VALUE '********************************'.  EL320
00143  77  FILLER  PIC X(32) VALUE '     EL320  WORKING-STORAGE     '.  EL320
00144  77  FILLER  PIC X(32) VALUE '******** VMOD=2.019 ************'.  EL320
00145                                                                   EL320
00146  77  MAX-WTI-INDX            PIC S9(4)   COMP    VALUE +500.      EL320
00147  77  ABEND-CODE              PIC S9(4)   COMP    VALUE ZERO.      EL320
00148  77  PGM-SUB                 PIC S9(4)   COMP    VALUE +320.      EL320
00149  77  XSEQ                    PIC S9(4)   COMP.                    EL320
00150  77  XROW                    PIC S9(4)   COMP.                    EL320
00151  77  XCOL                    PIC S9(4)   COMP.                    EL320
00152  77  WS-RETURN-CODE          PIC S9(4)   COMP    VALUE ZERO.      EL320
00153  77  WS-ZERO                 PIC S9      COMP-3  VALUE ZERO.      EL320
00154  77  WS-TABLE-EXCEEDED-SW    PIC X               VALUE 'N'.       EL320
00155      88  TABLE-SIZE-EXCEEDED                     VALUE 'Y'.       EL320
00156  77  WS-ELCNTL-EOF-SW        PIC X               VALUE SPACE.     EL320
00157      88  END-OF-CNTL-FILE                        VALUE 'E'.       EL320
00158  77  WS-ELPGMS-EOF-SW        PIC X               VALUE SPACE.     EL320
00159      88  END-OF-PGMS-FILE                        VALUE 'E'.       EL320
00160  77  WS-FIRST-TIME-SW        PIC X               VALUE '1'.       EL320
00161      88  FIRST-TIME                              VALUE '1'.       EL320
00162  77  WS-ELFORM-EOF-SW        PIC X               VALUE SPACE.     EL320
00163      88  END-OF-TEXT-FILE                        VALUE 'E'.       EL320
00164  77  WS-ELLETR-EOF-SW        PIC X               VALUE SPACE.     EL320
00165      88  END-OF-LETR-FILE                        VALUE 'E'.       EL320
00166  77  WS-CO-CHG-SW            PIC X               VALUE SPACE.     EL320
00167      88  COMPANY-CHANGED                         VALUE 'X'.       EL320
00168  77  ERROR-SW                PIC X               VALUE SPACE.     EL320
00169      88  ERROR-OCCURRED                          VALUE 'E'.       EL320
00170      88  NO-ERRORS                               VALUE ' '.       EL320
00171  77  WS-STAT-FIRST-SW        PIC X               VALUE SPACE.     EL320
00172      88  FIRST-TIME-STATE                        VALUE ' '.       EL320
00173  77  SS                      PIC X               VALUE ' '.       EL320
00174  77  DS                      PIC X               VALUE '0'.       EL320
00175  77  TS                      PIC X               VALUE '-'.       EL320
00176  77  TP                      PIC X               VALUE '1'.       EL320
00177  77  X                       PIC X               VALUE SPACE.     EL320
00178  77  WS-COMP-CD              PIC X               VALUE SPACES.    EL320
00179  77  WS-RECORD-TYPE          PIC X               VALUE SPACE.     EL320
00180  77  WS-STATE-SAVED          PIC XX              VALUE SPACES.    EL320
00181  77  WS-ABEND-FILE-STATUS    PIC XX              VALUE ZERO.      EL320
00182  77  WS-YES                  PIC XXX             VALUE 'YES'.     EL320
00183  77  WS-NO                   PIC XXX             VALUE 'NO '.     EL320
00184  77  WS-SAVED-CO             PIC XXX             VALUE SPACES.    EL320
00185  77  LB-SUB                  PIC 99              VALUE ZEROS.     EL320
00186  77  AB-SUB                  PIC 99              VALUE ZEROS.     EL320
00187  77  ST-SUB                  PIC 99              VALUE ZEROS.     EL320
00188  77  BZ-SUB                  PIC 99              VALUE ZEROS.     EL320
00189  77  BZ-SUB-PRT              PIC 99              VALUE ZEROS.     EL320
00190  77  BUSS-PRINTED-CODE       PIC 99              VALUE ZEROS.     EL320
00191  77  TABLE-PRT-CONTROL       PIC 99              VALUE ZEROS.     EL320
00192  77  BUSS-RECORD-INDX        PIC 99              VALUE ZEROS.     EL320
00193  77  TERM-SUB                PIC 999             VALUE ZEROS.     EL320
00194  77  V-SUB                   PIC 99              VALUE ZEROS.     EL320
00195  77  H-SUB                   PIC 99              VALUE ZEROS.     EL320
00196  77  LFED-SUB                PIC 999             VALUE ZEROS.     EL320
00197  77  V1-SUB                  PIC 99              VALUE ZEROS.     EL320
00198  77  H1-SUB                  PIC 99              VALUE ZEROS.     EL320
00199  77  AHED-SUB                PIC 99              VALUE ZEROS.     EL320
00200  77  V2-SUB                  PIC 99              VALUE ZEROS.     EL320
00201  77  H2-SUB                  PIC 99              VALUE ZEROS.     EL320
00202  77  MORT-SUB                PIC 99              VALUE ZEROS.     EL320
00203  77  PGMS-SUB                PIC 99              VALUE ZEROS.     EL320
00204  77  TIMES-TO-PRINT          PIC 9(02)           VALUE ZEROS.     EL320
00205  77  WS-SAVED-LETR-NO        PIC X(4)            VALUE SPACE.     EL320
00206  77  OLC-REPORT-NAME         PIC X(5)            VALUE 'EL320'.   EL320
00207  77  WS-SAVED-FORM-NO        PIC X(12)           VALUE SPACE.     EL320
00208  77  WS-ABEND-MESSAGE        PIC X(80)           VALUE SPACES.    EL320
00209      EJECT                                                        EL320
00210  01  COUNTERS.                                                    EL320
00211      12  WS-HOLD-LF-L6           PIC X(06).                       EL320
00212      12  WS-HOLD-AH-L6           PIC X(06).                       EL320
00213      12  WS-BUSS-RECORD-CNT      PIC 9.                           EL320
00214      12  WS-PRT-ST-CNT           PIC 9.                           EL320
00215                                                                   EL320
00216  01  WS-PROGRAM-OPTIONS.                                          EL320
00217      12  WS-FREQUENCY-CODE       PIC X(4).                        EL320
00218      12  WS-PRINT-OPTION         PIC X.                           EL320
00219      12  WS-FORMAT-OPTION        PIC X.                           EL320
00220      12  WS-PROCESS-OPTION       PIC X.                           EL320
00221      12  WS-TOTAL-OPTION         PIC X.                           EL320
00222                                                                   EL320
00223  01  SAVE-ELCNTL-RECORDS.                                         EL320
00224      12  SAVE-AH-EDIT-MASTER     PIC X(750).                      EL320
00225      12  SAVE-LIFE-EDIT-MASTER   PIC X(750).                      EL320
00226                                                                   EL320
00227  01  BWS-LETTER-HEAD.                                             EL320
00228      12  FILLER              PIC X(77).                           EL320
00229      12  BWS-LETTER          PIC X(04).                           EL320
00230                                                                   EL320
00231  01  WS-TEXT-INDEX.                                               EL320
00232      12  WS-TEXT-RECORDS OCCURS 500 TIMES                         EL320
00233              INDEXED BY WTI-INDX.                                 EL320
00234          16  WS-TEXT-NO      PIC X(12).                           EL320
00235          16  WS-TEXT-PAGE    PIC 9(4).                            EL320
00236      EJECT                                                        EL320
00237  01  WS-SCRATRCH-AREA.                                            EL320
00238      12  WS-ZIP-CODE             PIC 9(9).                        EL320
00239      12  WS-ZIP-CODE-X  REDEFINES  WS-ZIP-CODE                    EL320
00240                                  PIC X(9).                        EL320
00241      12  WS-PHONE                PIC 9(11).                       EL320
00242      12  WS-DUM-PHONE REDEFINES WS-PHONE.                         EL320
00243          16  FILLER              PIC 9.                           EL320
00244          16  WS-PHONE-AC         PIC 999.                         EL320
00245          16  WS-PHONE-PF         PIC 999.                         EL320
00246          16  WS-PHONE-NO         PIC 9999.                        EL320
00247      12  WS-PHON-EDIT.                                            EL320
00248          16  WS-ED-AC            PIC 999.                         EL320
00249          16  FILLER              PIC X           VALUE '-'.       EL320
00250          16  WS-ED-PF            PIC 999.                         EL320
00251          16  FILLER              PIC X           VALUE '-'.       EL320
00252          16  WS-ED-NO            PIC 9999.                        EL320
00253      12  WS-CONTROL-PRIMARY.                                      EL320
00254          16  WS-COMPANY-CD       PIC X.                           EL320
00255          16  FILLER              PIC X(14).                       EL320
00256      12  WS-INVALID-SETTING      PIC X(21)                        EL320
00257              VALUE '...INVALID SETTING...'.                       EL320
00258      12  ELCNTL-STATUS.                                           EL320
00259          16  ELCNTL-STAT-1       PIC X.                           EL320
00260          16  ELCNTL-STAT-2       PIC X.                           EL320
00261      12  ELPGMS-STATUS.                                           EL320
00262          16  ELPGMS-STAT-1       PIC X.                           EL320
00263          16  ELPGMS-STAT-2       PIC X.                           EL320
00264      12  ELFORM-STATUS.                                           EL320
00265          16  ELFORM-STAT-1       PIC X.                           EL320
00266          16  ELFORM-STAT-2       PIC X.                           EL320
00267      12  ELLETR-STATUS.                                           EL320
00268          16  ELLETR-STAT-1       PIC X.                           EL320
00269          16  ELLETR-STAT-2       PIC X.                           EL320
00270      12  PRINT-CONTROL-WORK-AREA.                                 EL320
00271          16  WS-LINE-CNT         PIC 9(4)        VALUE 1.         EL320
00272          16  WS-PAGE-CNT         PIC 9(4)        VALUE 1.         EL320
00273          16  BLANKLINE           PIC X(132)      VALUE SPACES.    EL320
00274          16  WS-MAINT-TIME       PIC 9(6).                        EL320
00275          16  WS-MAINT-TM  REDEFINES WS-MAINT-TIME.                EL320
00276              20  WS-MAINT-HH     PIC 99.                          EL320
00277              20  WS-MAINT-MM     PIC 99.                          EL320
00278              20  WS-MAINT-SS     PIC 99.                          EL320
00279          16  WS-CURRENT-DATE-MDY.                                 EL320
00280              20  WS-CURR-MO      PIC 99.                          EL320
00281              20  WS-CURR-DY      PIC 99.                          EL320
00282              20  WS-CURR-YR      PIC 99.                          EL320
00283      EJECT                                                        EL320
00284  01  WS-PRINT-BUSS-TABLE.                                         EL320
00285      12  WS-BUSS-DETAIL OCCURS 20  TIMES                          EL320
00286              INDEXED BY TABLE-INDX.                               EL320
00287          16  WS-CODE-NAME OCCURS 2 TIMES                          EL320
00288                  INDEXED BY INDX-SUB.                             EL320
00289              20  FILLER              PIC X.                       EL320
00290              20  WS-TABLE-CODE       PIC XX.                      EL320
00291              20  FILLER              PIC X(4).                    EL320
00292              20  WS-TABLE-NAME       PIC X(24).                   EL320
00293              20  FILLER              PIC X(22).                   EL320
00294      EJECT                                                        EL320
00295  01  PRINT-LINES.                                                 EL320
00296      12  HEAD-LINE-1.                                             EL320
00297          16  FILLER              PIC X(22)       VALUE SPACE.     EL320
00298          16  FILLER              PIC X(35)       VALUE            EL320
00299                  '   CLAS-IC  CONTROL FILE LISTING   '.           EL320
00300          16  FILLER              PIC X(15)       VALUE SPACES.    EL320
00301          16  FILLER              PIC X(8)        VALUE 'EL320'.   EL320
00302          16  FILLER              PIC X(5)        VALUE SPACES.    EL320
00303      12  HEAD-LINE-2.                                             EL320
00304          16  FILLER              PIC X(25)       VALUE SPACE.     EL320
00305          16  HEAD-CLIENT         PIC X(30)       VALUE SPACES.    EL320
00306          16  FILLER              PIC X(17)       VALUE SPACES.    EL320
00307          16  HEAD-RUN-DATE       PIC X(8).                        EL320
00308          16  FILLER              PIC X(5)        VALUE SPACES.    EL320
00309      12  HEAD-LINE-3.                                             EL320
00310          16  FILLER              PIC X(30)       VALUE SPACE.     EL320
00311          16  HEAD-RUN-DATE-FULL  PIC X(19).                       EL320
00312          16  FILLER              PIC X(15)       VALUE SPACES.    EL320
00313          16  FILLER              PIC X(5)        VALUE 'PAGE'.    EL320
00314          16  HEAD-PAGE-NO        PIC ZZZ9.                        EL320
00315          16  FILLER              PIC X(12)       VALUE SPACES.    EL320
00316      12  COMP-LINE-A1.                                            EL320
00317          16  FILLER              PIC X(40)       VALUE            EL320
00318                  'RECORD TYPE - COMPANY - GENERAL CONTROLS'.      EL320
00319          16  FILLER              PIC X(13)       VALUE SPACE.     EL320
00320          16  FILLER              PIC X(22)       VALUE            EL320
00321                  'LAST MAINTENANCE BY - '.                        EL320
00322          16  COMP-MAINT-BY-A     PIC X(4)        VALUE SPACE.     EL320
00323      12  COMP-LINE-A2.                                            EL320
00324          16  FILLER              PIC X(53)       VALUE SPACES.    EL320
00325          16  FILLER              PIC X(5)        VALUE 'ON - '.   EL320
00326          16  COMP-MAINT-DT-A     PIC X(8).                        EL320
00327          16  FILLER              PIC X(6)        VALUE ' AT - '.  EL320
00328          16  COMP-MAINT-TM-A.                                     EL320
00329              20  COMP-MAINT-HHA  PIC XX.                          EL320
00330              20  FILLER          PIC X           VALUE '.'.       EL320
00331              20  COMP-MAINT-MMA  PIC XX.                          EL320
00332              20  FILLER          PIC X           VALUE '.'.       EL320
00333              20  COMP-MAINT-SSA  PIC XX.                          EL320
00334      12  COMP-LINE-A3.                                            EL320
00335          16  FILLER              PIC X(15)       VALUE            EL320
00336                  'COMPANY NAME - '.                               EL320
00337          16  COMP-NAME           PIC X(30).                       EL320
00338          16  FILLER              PIC X(23)       VALUE            EL320
00339                  '     COMPANY I.D. IS - '.                       EL320
00340          16  COMP-ID             PIC X(3).                        EL320
00341      12  COMP-LINE-A4.                                            EL320
00342          16  FILLER              PIC X(15)       VALUE            EL320
00343                  '  IN CARE OF - '.                               EL320
00344          16  COMP-IN-CARE        PIC X(30).                       EL320
00345      EJECT                                                        EL320
00346      12  COMP-LINE-A5.                                            EL320
00347          16  FILLER              PIC X(15)       VALUE            EL320
00348                  '  ADDRESS 1  - '.                               EL320
00349          16  COMP-ADDRESS-1      PIC X(30)       VALUE SPACES.    EL320
00350          16  FILLER              PIC X(23)       VALUE            EL320
00351                  '     COMPANY CODE IS - '.                       EL320
00352          16  COMP-CODE           PIC 999.                         EL320
00353      12  COMP-LINE-A6.                                            EL320
00354          16  FILLER              PIC X(15)       VALUE            EL320
00355                  '  ADDRESS 2  - '.                               EL320
00356          16  COMP-ADDRESS-2      PIC X(30)       VALUE SPACES.    EL320
00357      12  COMP-LINE-A7.                                            EL320
00358          16  FILLER              PIC X(15)       VALUE            EL320
00359                  '  CITY, STATE- '.                               EL320
00360          16  COMP-CITY-STAT      PIC X(30)       VALUE SPACES.    EL320
00361          16  FILLER              PIC X(5)        VALUE SPACES.    EL320
00362          16  COMP-TAX-ID         PIC X(11)       VALUE SPACES.    EL320
00363      12  COMP-LINE-A8.                                            EL320
00364          16  FILLER              PIC X(15)       VALUE            EL320
00365                  '  ZIP  PHONE - '.                               EL320
00366          16  COMP-ZIP-CODE       PIC X(9)        VALUE ZEROS.     EL320
00367          16  FILLER              PIC X(9)        VALUE SPACES.    EL320
00368          16  COMP-PHONE          PIC X(12)       VALUE SPACES.    EL320
00369      12  COMP-LINE-A9.                                            EL320
00370          16  FILLER              PIC X(10)       VALUE            EL320
00371                  'FIELD NAME'.                                    EL320
00372          16  FILLER              PIC X(18)       VALUE SPACES.    EL320
00373          16  FILLER              PIC X(5)        VALUE            EL320
00374                  'VALUE'.                                         EL320
00375          16  FILLER              PIC X(7)        VALUE SPACES.    EL320
00376          16  FILLER              PIC X(30)       VALUE            EL320
00377                  'DESCRIPTION OF SELECTED OPTION'.                EL320
00378      12  COMP-LINE-A10.                                           EL320
00379          16  FILLER              PIC X(22)       VALUE            EL320
00380                  ' SECURITY OPTION     '.                         EL320
00381          16  FILLER              PIC X(10)       VALUE  SPACES.   EL320
00382          16  COMP-OPT-A10        PIC X.                           EL320
00383          16  FILLER              PIC X(7)        VALUE SPACES.    EL320
00384          16  COMP-DESC-A10       PIC X(42)       VALUE SPACES.    EL320
00385      12  COMP-LINE-A11.                                           EL320
00386          16  FILLER              PIC X(22)       VALUE            EL320
00387                  ' LOGIC TIME SHARE     '.                        EL320
00388          16  FILLER              PIC X(10)       VALUE SPACES.    EL320
00389          16  COMP-OPT-A11        PIC X.                           EL320
00390          16  FILLER              PIC X(7)        VALUE SPACES.    EL320
00391          16  COMP-DESC-A11       PIC X(42)       VALUE SPACES.    EL320
00392      12  COMP-LINE-A12.                                           EL320
00393          16  FILLER              PIC X(22)       VALUE            EL320
00394                  ' LOGIC CLAS SYSTEM    '.                        EL320
00395          16  FILLER              PIC X(10)       VALUE SPACES.    EL320
00396          16  COMP-OPT-A12        PIC X.                           EL320
00397          16  FILLER              PIC X(7)        VALUE SPACES.    EL320
00398          16  COMP-DESC-A12       PIC X(42)       VALUE SPACES.    EL320
00399      EJECT                                                        EL320
00400      12  COMP-LINE-A13.                                           EL320
00401          16  FILLER              PIC X(22)       VALUE            EL320
00402                  ' LOGIC CLAIM SYSTEM   '.                        EL320
00403          16  FILLER              PIC X(10)       VALUE SPACES.    EL320
00404          16  COMP-OPT-A13        PIC X.                           EL320
00405          16  FILLER              PIC X(7)        VALUE SPACES.    EL320
00406          16  COMP-DESC-A13       PIC X(42)       VALUE SPACES.    EL320
00407      12  COMP-LINE-A14.                                           EL320
00408          16  FILLER              PIC X(22)       VALUE            EL320
00409                  ' LOGIC LIFE SYSTEM    '.                        EL320
00410          16  FILLER              PIC X(10)       VALUE SPACES.    EL320
00411          16  COMP-OPT-A14        PIC X.                           EL320
00412          16  FILLER              PIC X(7)        VALUE SPACES.    EL320
00413          16  COMP-DESC-A14       PIC X(42)       VALUE SPACES.    EL320
00414      12  COMP-LINE-A15.                                           EL320
00415          16  FILLER              PIC X(22)       VALUE            EL320
00416                  ' GEN LEDGER SYSTEM    '.                        EL320
00417          16  FILLER              PIC X(10)       VALUE SPACES.    EL320
00418          16  COMP-OPT-A15        PIC X.                           EL320
00419          16  FILLER              PIC X(7)        VALUE SPACES.    EL320
00420          16  COMP-DESC-A15       PIC X(42)       VALUE SPACES.    EL320
00421      12  COMP-LINE-A16.                                           EL320
00422          16  FILLER              PIC X(22)       VALUE            EL320
00423                  ' LIFE ACCESS CONTROL  '.                        EL320
00424          16  FILLER              PIC X(10)       VALUE SPACES.    EL320
00425          16  COMP-OPT-A16        PIC X.                           EL320
00426          16  FILLER              PIC X(7)        VALUE SPACES.    EL320
00427          16  COMP-DESC-A16       PIC X(42)       VALUE SPACES.    EL320
00428      12  COMP-LINE-A17.                                           EL320
00429          16  FILLER              PIC X(22)       VALUE            EL320
00430                  ' CERT ACCESS CONTROL  '.                        EL320
00431          16  FILLER              PIC X(10)       VALUE SPACES.    EL320
00432          16  COMP-OPT-A17        PIC X.                           EL320
00433          16  FILLER              PIC X(7)        VALUE SPACES.    EL320
00434          16  COMP-DESC-A17       PIC X(42)       VALUE SPACES.    EL320
00435      12  COMP-LINE-A18.                                           EL320
00436          16  FILLER              PIC X(22)       VALUE            EL320
00437                  ' REMAINING TERM       '.                        EL320
00438          16  FILLER              PIC X(10)       VALUE SPACES.    EL320
00439          16  COMP-OPT-A18        PIC X.                           EL320
00440          16  FILLER              PIC X(7)        VALUE SPACES.    EL320
00441          16  COMP-DESC-A18       PIC X(42)       VALUE SPACES.    EL320
00442      12  COMP-LINE-A19.                                           EL320
00443          16  FILLER              PIC X(22)       VALUE            EL320
00444                  ' RULE - 78 METHOD     '.                        EL320
00445          16  FILLER              PIC X(10)       VALUE SPACES.    EL320
00446          16  COMP-OPT-A19        PIC X.                           EL320
00447          16  FILLER              PIC X(7)        VALUE SPACES.    EL320
00448          16  COMP-DESC-A19       PIC X(42)       VALUE SPACES.    EL320
00449      EJECT                                                        EL320
00450      12  COMP-LINE-A20.                                           EL320
00451          16  FILLER              PIC X(22)       VALUE            EL320
00452                  ' G.A. BILLING SYSTEM  '.                        EL320
00453          16  FILLER              PIC X(10)       VALUE SPACES.    EL320
00454          16  COMP-OPT-A20        PIC X.                           EL320
00455          16  FILLER              PIC X(7)        VALUE SPACES.    EL320
00456          16  COMP-DESC-A20       PIC X(42)       VALUE SPACES.    EL320
00457      12  COMP-LINE-A21.                                           EL320
00458          16  FILLER              PIC X(22)       VALUE            EL320
00459                  ' CONFIRMATION SYSTEM  '.                        EL320
00460          16  FILLER              PIC X(10)       VALUE SPACES.    EL320
00461          16  COMP-OPT-A21        PIC X.                           EL320
00462          16  FILLER              PIC X(7)        VALUE SPACES.    EL320
00463          16  COMP-DESC-A21       PIC X(42)       VALUE SPACES.    EL320
00464      12  COMP-LINE-A22.                                           EL320
00465          16  FILLER              PIC X(22)       VALUE            EL320
00466                  ' USER INDEX CAPTION   '.                        EL320
00467          16  FILLER              PIC X(4)        VALUE SPACES.    EL320
00468          16  COMP-OPT-A22        PIC X(10).                       EL320
00469          16  FILLER              PIC X(7)        VALUE SPACES.    EL320
00470          16  COMP-DESC-A22       PIC X(42)       VALUE SPACES.    EL320
00471      12  COMP-LINE-A23.                                           EL320
00472          16  FILLER              PIC X(22)       VALUE            EL320
00473                  ' MAIL PROCESSING      '.                        EL320
00474          16  FILLER              PIC X(10)       VALUE SPACES.    EL320
00475          16  COMP-OPT-A23        PIC X.                           EL320
00476          16  FILLER              PIC X(7)        VALUE SPACES.    EL320
00477          16  COMP-DESC-A23       PIC X(42)       VALUE SPACES.    EL320
00478      12  COMP-LINE-A24.                                           EL320
00479          16  FILLER              PIC X(22)       VALUE            EL320
00480                  ' FORMS PRINTER I.D.   '.                        EL320
00481          16  FILLER              PIC X(7)        VALUE SPACES.    EL320
00482          16  COMP-OPT-A24        PIC XXXX.                        EL320
00483          16  FILLER              PIC X(7)        VALUE SPACES.    EL320
00484          16  FILLER              PIC X(38)       VALUE            EL320
00485                  'PRINTER NORMALLY HAVING 8.5 X 11 PAPER'.        EL320
00486      12  COMP-LINE-A25.                                           EL320
00487          16  FILLER              PIC X(22)       VALUE            EL320
00488                  ' CHECK PRINTER I.D.   '.                        EL320
00489          16  FILLER              PIC X(7)        VALUE SPACES.    EL320
00490          16  COMP-OPT-A25        PIC XXXX.                        EL320
00491          16  FILLER              PIC X(7)        VALUE SPACES.    EL320
00492          16  FILLER              PIC X(38)       VALUE            EL320
00493                  'PRINTER NORMALLY HAVING SPECIAL FORMS '.        EL320
00494      12  COMP-LINE-A26.                                           EL320
00495          16  FILLER              PIC X(22)       VALUE            EL320
00496                  ' NEXT COMPANY  I.D.   '.                        EL320
00497          16  FILLER              PIC X(7)        VALUE SPACES.    EL320
00498          16  COMP-OPT-A26        PIC XXX.                         EL320
00499          16  FILLER              PIC X(7)        VALUE SPACES.    EL320
00500          16  FILLER              PIC X(38)       VALUE SPACES.    EL320
00501      EJECT                                                        EL320
00502      12  COMP-LINE-A27.                                           EL320
00503          16  FILLER              PIC X(22)       VALUE            EL320
00504                  ' SOCIAL SECURITY NO.  '.                        EL320
00505          16  FILLER              PIC X(10)       VALUE SPACES.    EL320
00506          16  COMP-OPT-A27        PIC X.                           EL320
00507          16  FILLER              PIC X(7)        VALUE SPACES.    EL320
00508          16  COMP-DESC-A27       PIC X(42)       VALUE SPACES.    EL320
00509      12  COMP-LINE-A28.                                           EL320
00510          16  FILLER              PIC X(22)       VALUE            EL320
00511                  ' MEMBER NUMBER        '.                        EL320
00512          16  FILLER              PIC X(10)       VALUE SPACES.    EL320
00513          16  COMP-OPT-A28        PIC X.                           EL320
00514          16  FILLER              PIC X(7)        VALUE SPACES.    EL320
00515          16  COMP-DESC-A28       PIC X(42)       VALUE SPACES.    EL320
00516      12  COMP-LINE-A29.                                           EL320
00517          16  FILLER              PIC X(22)       VALUE            EL320
00518                  ' BENEFIT DESCRIPTION  '.                        EL320
00519          16  FILLER              PIC X(3)        VALUE ' - '.     EL320
00520          16  COMP-OPT-A29-1      PIC X.                           EL320
00521          16  FILLER              PIC X(3)        VALUE ' - '.     EL320
00522          16  COMP-OPT-A29-2      PIC XX.                          EL320
00523          16  FILLER              PIC X(3)        VALUE ' - '.     EL320
00524          16  COMP-OPT-A29-3      PIC X(6).                        EL320
00525          16  FILLER              PIC X(3)        VALUE ' - '.     EL320
00526          16  COMP-OPT-A29-4      PIC X(12).                       EL320
00527      12  COMP-LINE-A30.                                           EL320
00528          16  FILLER              PIC X(22)       VALUE            EL320
00529                  ' BENEFIT DESCRIPTION  '.                        EL320
00530          16  FILLER              PIC X(3)        VALUE ' - '.     EL320
00531          16  COMP-OPT-A30-1      PIC X.                           EL320
00532          16  FILLER              PIC X(3)        VALUE ' - '.     EL320
00533          16  COMP-OPT-A30-2      PIC XX.                          EL320
00534          16  FILLER              PIC X(3)        VALUE ' - '.     EL320
00535          16  COMP-OPT-A30-3      PIC X(6).                        EL320
00536          16  FILLER              PIC X(3)        VALUE ' - '.     EL320
00537          16  COMP-OPT-A30-4      PIC X(12).                       EL320
00538      12  COMP-LINE-A31.                                           EL320
00539          16  FILLER              PIC X(27)       VALUE            EL320
00540                  ' REPORT CODE ONE CAPTION - '.                   EL320
00541          16  COMP-OPT-A31        PIC X(10).                       EL320
00542      12  COMP-LINE-A32.                                           EL320
00543          16  FILLER              PIC X(27)       VALUE            EL320
00544                  ' REPORT CODE TWO CAPTION - '.                   EL320
00545          16  COMP-OPT-A32        PIC X(10).                       EL320
00546      12  COMP-LINE-A33.                                           EL320
00547          16  FILLER              PIC X(20)       VALUE            EL320
00548                  ' CLAIMS MONTH END - '.                          EL320
00549          16  COMP-OPT-A33        PIC X(8).                        EL320
00550      12  COMP-LINE-A34.                                           EL320
00551          16  FILLER              PIC X(20)       VALUE            EL320
00552                  ' CREDIT MONTH END - '.                          EL320
00553          16  COMP-OPT-A34        PIC X(8).                        EL320
00554      EJECT                                                        EL320
00555      12  COMP-LINE-B1.                                            EL320
00556          16  FILLER              PIC X(40)       VALUE            EL320
00557                  'RECORD TYPE - COMPANY - CLAIM CONTROLS  '.      EL320
00558          16  FILLER              PIC X(13)       VALUE SPACE.     EL320
00559          16  FILLER              PIC X(22)       VALUE            EL320
00560                  'LAST MAINTENANCE BY - '.                        EL320
00561          16  COMP-MAINT-BY-B     PIC X(4)        VALUE SPACE.     EL320
00562      12  COMP-LINE-B2.                                            EL320
00563          16  FILLER              PIC X(53)       VALUE SPACES.    EL320
00564          16  FILLER              PIC X(5)        VALUE 'ON - '.   EL320
00565          16  COMP-MAINT-DT-B     PIC X(8).                        EL320
00566          16  FILLER              PIC X(6)        VALUE ' AT - '.  EL320
00567          16  COMP-MAINT-TM-B.                                     EL320
00568              20  COMP-MAINT-HHB  PIC XX.                          EL320
00569              20  FILLER          PIC X           VALUE '.'.       EL320
00570              20  COMP-MAINT-MMB  PIC XX.                          EL320
00571              20  FILLER          PIC X           VALUE '.'.       EL320
00572              20  COMP-MAINT-SSB  PIC XX.                          EL320
00573      12  COMP-LINE-B3.                                            EL320
00574          16  FILLER              PIC X(22)       VALUE            EL320
00575                  ' CLAIM ACCESS CONTROL '.                        EL320
00576          16  FILLER              PIC X(10)       VALUE SPACES.    EL320
00577          16  COMP-OPT-B3         PIC X.                           EL320
00578          16  FILLER              PIC X(7)        VALUE SPACES.    EL320
00579          16  COMP-DESC-B3        PIC X(42)       VALUE SPACES.    EL320
00580      12  COMP-LINE-B4.                                            EL320
00581          16  FILLER              PIC X(22)       VALUE            EL320
00582                  ' CONTROL LEVEL CARRIER'.                        EL320
00583          16  FILLER              PIC X(10)       VALUE SPACES.    EL320
00584          16  COMP-OPT-B4         PIC X.                           EL320
00585          16  FILLER              PIC X(7)        VALUE SPACES.    EL320
00586          16  COMP-DESC-B4        PIC X(42)       VALUE SPACES.    EL320
00587      12  COMP-LINE-B5.                                            EL320
00588          16  FILLER              PIC X(22)       VALUE            EL320
00589                  ' PAYMENT APPROVAL     '.                        EL320
00590          16  FILLER              PIC X(10)       VALUE SPACES.    EL320
00591          16  COMP-OPT-B5         PIC X.                           EL320
00592          16  FILLER              PIC X(7)        VALUE SPACES.    EL320
00593          16  COMP-DESC-B5        PIC X(42)       VALUE SPACES.    EL320
00594      12  COMP-LINE-B6.                                            EL320
00595          16  FILLER              PIC X(22)       VALUE            EL320
00596                  ' CLAIM TOLERANCE     '.                         EL320
00597          16  FILLER              PIC X(5)        VALUE SPACES.    EL320
00598          16  COMP-OPT-B6         PIC ZZZ.99.                      EL320
00599          16  FILLER              PIC X(7)        VALUE SPACES.    EL320
00600          16  COMP-DESC-B6        PIC X(42)       VALUE SPACES.    EL320
00601      12  COMP-LINE-B7.                                            EL320
00602          16  FILLER              PIC X(22)       VALUE            EL320
00603                  ' CLAIM REJECT CODE    '.                        EL320
00604          16  FILLER              PIC X(10)       VALUE SPACES.    EL320
00605          16  COMP-OPT-B7         PIC X.                           EL320
00606          16  FILLER              PIC X(7)        VALUE SPACES.    EL320
00607          16  COMP-DESC-B7        PIC X(42)       VALUE SPACES.    EL320
00608      EJECT                                                        EL320
00609      12  COMP-LINE-B8.                                            EL320
00610          16  FILLER              PIC X(22)       VALUE            EL320
00611                  ' CLAIM CUT OFF DATE   '.                        EL320
00612          16  FILLER              PIC X(3)        VALUE SPACES.    EL320
00613          16  COMP-OPT-B8         PIC X(8).                        EL320
00614          16  FILLER              PIC X(7)        VALUE SPACES.    EL320
00615          16  COMP-DESC-B8        PIC X(42)       VALUE SPACES.    EL320
00616      12  COMP-LINE-B9.                                            EL320
00617          16  FILLER              PIC X(22)       VALUE            EL320
00618                  ' LAST CLAIM NUMBER    '.                        EL320
00619          16  FILLER              PIC X(6)        VALUE SPACES.    EL320
00620          16  COMP-OPT-B9         PIC 9(5).                        EL320
00621          16  FILLER              PIC X(7)        VALUE SPACES.    EL320
00622          16  FILLER              PIC X(38)       VALUE            EL320
00623                  'USED FOR UNIQUE CLAIM NUMBERS         '.        EL320
00624      12  COMP-LINE-B10.                                           EL320
00625          16  FILLER              PIC X(22)       VALUE            EL320
00626                  ' LAST ARCHIVE NUMBER  '.                        EL320
00627          16  FILLER              PIC X(6)        VALUE SPACES.    EL320
00628          16  COMP-OPT-B10        PIC 9(5).                        EL320
00629      12  COMP-LINE-B11.                                           EL320
00630          16  FILLER              PIC X(22)       VALUE            EL320
00631                  ' LAST CHECK NUMBER    '.                        EL320
00632          16  FILLER              PIC X(6)        VALUE SPACES.    EL320
00633          16  COMP-OPT-B11        PIC 9(5).                        EL320
00634          16  FILLER              PIC X(7)        VALUE SPACES.    EL320
00635          16  FILLER              PIC X(38)       VALUE            EL320
00636                  'USED FOR UNIQUE CHECK NUMBERS'.                 EL320
00637      12  COMP-LINE-B12.                                           EL320
00638          16  FILLER              PIC X(22)       VALUE            EL320
00639                  ' LAST CHECK QUE NUMBER'.                        EL320
00640          16  FILLER              PIC X(6)        VALUE SPACES.    EL320
00641          16  COMP-OPT-B12        PIC 9(5).                        EL320
00642          16  FILLER              PIC X(7)        VALUE SPACES.    EL320
00643          16  FILLER              PIC X(38)       VALUE            EL320
00644                  'CHECK BATCH ASSIGNMENTS'.                       EL320
00645      12  COMP-LINE-B13.                                           EL320
00646          16  FILLER              PIC X(22)       VALUE            EL320
00647                  ' START ARCHIVE NUMBER '.                        EL320
00648          16  FILLER              PIC X(6)        VALUE SPACES.    EL320
00649          16  COMP-OPT-B13        PIC 9(5).                        EL320
00650      EJECT                                                        EL320
00651      12  COMP-LINE-C1.                                            EL320
00652          16  FILLER              PIC X(40)       VALUE            EL320
00653                  'RECORD TYPE - COMPANY - CREDIT CONTROLS '.      EL320
00654          16  FILLER              PIC X(13)       VALUE SPACE.     EL320
00655          16  FILLER              PIC X(22)       VALUE            EL320
00656                  'LAST MAINTENANCE BY - '.                        EL320
00657          16  COMP-MAINT-BY-C     PIC X(4)        VALUE SPACE.     EL320
00658      12  COMP-LINE-C2.                                            EL320
00659          16  FILLER              PIC X(53)       VALUE SPACES.    EL320
00660          16  FILLER              PIC X(5)        VALUE 'ON - '.   EL320
00661          16  COMP-MAINT-DT-C     PIC X(8).                        EL320
00662          16  FILLER              PIC X(6)        VALUE ' AT - '.  EL320
00663          16  COMP-MAINT-TM-C.                                     EL320
00664              20  COMP-MAINT-HHC  PIC XX.                          EL320
00665              20  FILLER          PIC X           VALUE '.'.       EL320
00666              20  COMP-MAINT-MMC  PIC XX.                          EL320
00667              20  FILLER          PIC X           VALUE '.'.       EL320
00668              20  COMP-MAINT-SSC  PIC XX.                          EL320
00669      12  COMP-LINE-C3.                                            EL320
00670          16  FILLER              PIC X(22)       VALUE            EL320
00671                  ' PREMIUM TOLERANCE   '.                         EL320
00672          16  FILLER              PIC X(5)        VALUE SPACES.    EL320
00673          16  COMP-OPT-C3         PIC ZZZ.99.                      EL320
00674          16  FILLER              PIC X(7)        VALUE SPACES.    EL320
00675          16  COMP-DESC-C3        PIC X(42)       VALUE SPACES.    EL320
00676      12  COMP-LINE-C4.                                            EL320
00677          16  FILLER              PIC X(22)       VALUE            EL320
00678                  ' PREMIUM REJECT CODE  '.                        EL320
00679          16  FILLER              PIC X(10)       VALUE SPACES.    EL320
00680          16  COMP-OPT-C4         PIC X.                           EL320
00681          16  FILLER              PIC X(7)        VALUE SPACES.    EL320
00682          16  COMP-DESC-C4        PIC X(42)       VALUE SPACES.    EL320
00683      12  COMP-LINE-C5.                                            EL320
00684          16  FILLER              PIC X(22)       VALUE            EL320
00685                  ' PREMIUM TOLERANCE   '.                         EL320
00686          16  FILLER              PIC X(5)        VALUE SPACES.    EL320
00687          16  COMP-OPT-C5         PIC ZZZ.99.                      EL320
00688          16  FILLER              PIC X(7)        VALUE SPACES.    EL320
00689          16  COMP-DESC-C5        PIC X(42)       VALUE SPACES.    EL320
00690      12  COMP-LINE-C6.                                            EL320
00691          16  FILLER              PIC X(22)       VALUE            EL320
00692                  ' PREMIUM REJECT CODE  '.                        EL320
00693          16  FILLER              PIC X(10)       VALUE SPACES.    EL320
00694          16  COMP-OPT-C6         PIC X.                           EL320
00695          16  FILLER              PIC X(7)        VALUE SPACES.    EL320
00696          16  COMP-DESC-C6        PIC X(42)       VALUE SPACES.    EL320
00697      EJECT                                                        EL320
00698      12  COMP-LINE-C7.                                            EL320
00699          16  FILLER              PIC X(22)       VALUE            EL320
00700                  ' PROCESSING FREQUENCY '.                        EL320
00701          16  FILLER              PIC X(10)       VALUE SPACES.    EL320
00702          16  COMP-OPT-C7         PIC X.                           EL320
00703          16  FILLER              PIC X(7)        VALUE SPACES.    EL320
00704          16  COMP-DESC-C7        PIC X(42)       VALUE SPACES.    EL320
00705      12  COMP-LINE-C8.                                            EL320
00706          16  FILLER              PIC X(22)       VALUE            EL320
00707                  ' REINSURANCE TABLES   '.                        EL320
00708          16  FILLER              PIC X(10)       VALUE SPACES.    EL320
00709          16  COMP-OPT-C8         PIC X.                           EL320
00710          16  FILLER              PIC X(7)        VALUE SPACES.    EL320
00711          16  COMP-DESC-C8        PIC X(42)       VALUE SPACES.    EL320
00712      12  COMP-LINE-C9.                                            EL320
00713          16  FILLER              PIC X(22)       VALUE            EL320
00714                  ' COMPENSATION TABLES  '.                        EL320
00715          16  FILLER              PIC X(10)       VALUE SPACES.    EL320
00716          16  COMP-OPT-C9         PIC X.                           EL320
00717          16  FILLER              PIC X(7)        VALUE SPACES.    EL320
00718          16  COMP-DESC-C9        PIC X(42)       VALUE SPACES.    EL320
00719      12  COMP-LINE-C10.                                           EL320
00720          16  FILLER              PIC X(23)       VALUE            EL320
00721                  ' COMPENSATION WRITE OFF'.                       EL320
00722          16  FILLER              PIC X(4)        VALUE SPACES.    EL320
00723          16  COMP-OPT-C10        PIC ZZZ.99.                      EL320
00724          16  FILLER              PIC X(7)        VALUE SPACES.    EL320
00725          16  COMP-DESC-C10       PIC X(42)       VALUE SPACES.    EL320
00726      12  COMP-LINE-C11.                                           EL320
00727          16  FILLER              PIC X(24)       VALUE            EL320
00728                  ' CHECK PROCESSING METHOD'.                      EL320
00729          16  FILLER              PIC X(8)        VALUE SPACES.    EL320
00730          16  COMP-OPT-C11        PIC X.                           EL320
00731          16  FILLER              PIC X(7)        VALUE SPACES.    EL320
00732          16  COMP-DESC-C11       PIC X(42)       VALUE SPACES.    EL320
00733      12  COMP-LINE-C12.                                           EL320
00734          16  FILLER              PIC X(22)       VALUE            EL320
00735                  ' COMPENSATION CONTROL'.                         EL320
00736          16  FILLER              PIC X(10)       VALUE SPACES.    EL320
00737          16  COMP-OPT-C12        PIC X.                           EL320
00738          16  FILLER              PIC X(7)        VALUE SPACES.    EL320
00739          16  COMP-DESC-C12       PIC X(42)       VALUE SPACES.    EL320
00740      12  COMP-LINE-C13.                                           EL320
00741          16  FILLER              PIC X(22)       VALUE            EL320
00742                  ' ALTERNATE MORT. CODE'.                         EL320
00743          16  FILLER              PIC X(8)        VALUE SPACES.    EL320
00744          16  COMP-OPT-C13        PIC XXX.                         EL320
00745          16  FILLER              PIC X(7)        VALUE SPACES.    EL320
00746          16  COMP-DESC-C13       PIC X(42)       VALUE SPACES.    EL320
00747      EJECT                                                        EL320
00748      12  COMP-LINE-C14.                                           EL320
00749          16  FILLER              PIC X(22)       VALUE            EL320
00750                  ' MINIMUM AGE          '.                        EL320
00751          16  FILLER              PIC X(9)        VALUE SPACES.    EL320
00752          16  COMP-OPT-C14        PIC XX.                          EL320
00753          16  FILLER              PIC X(7)        VALUE SPACES.    EL320
00754          16  COMP-DESC-C14       PIC X(42)       VALUE SPACES.    EL320
00755      12  COMP-LINE-C15.                                           EL320
00756          16  FILLER              PIC X(22)       VALUE            EL320
00757                  ' MAXIMUM TERM         '.                        EL320
00758          16  FILLER              PIC X(8)        VALUE SPACES.    EL320
00759          16  COMP-OPT-C15        PIC ZZZ.                         EL320
00760          16  FILLER              PIC X(7)        VALUE SPACES.    EL320
00761          16  COMP-DESC-C15       PIC X(42)       VALUE SPACES.    EL320
00762      12  COMP-LINE-C16.                                           EL320
00763          16  FILLER              PIC X(22)       VALUE            EL320
00764                  ' DEFAULT AGE          '.                        EL320
00765          16  FILLER              PIC X(9)        VALUE SPACES.    EL320
00766          16  COMP-OPT-C16        PIC XX.                          EL320
00767          16  FILLER              PIC X(7)        VALUE SPACES.    EL320
00768          16  COMP-DESC-C16       PIC X(42)       VALUE SPACES.    EL320
00769      12  COMP-LINE-C17.                                           EL320
00770          16  FILLER              PIC X(22)       VALUE            EL320
00771                  ' DEFAULT SEX          '.                        EL320
00772          16  FILLER              PIC X(10)       VALUE SPACES.    EL320
00773          16  COMP-OPT-C17        PIC X.                           EL320
00774          16  FILLER              PIC X(7)        VALUE SPACES.    EL320
00775          16  COMP-DESC-C17       PIC X(42)       VALUE SPACES.    EL320
00776      12  COMP-LINE-C18.                                           EL320
00777          16  FILLER              PIC X(22)       VALUE            EL320
00778                  ' JOINT AGE INPUT      '.                        EL320
00779          16  FILLER              PIC X(10)       VALUE SPACES.    EL320
00780          16  COMP-OPT-C18        PIC X.                           EL320
00781          16  FILLER              PIC X(7)        VALUE SPACES.    EL320
00782          16  COMP-DESC-C18       PIC X(42)       VALUE SPACES.    EL320
00783      12  COMP-LINE-C19.                                           EL320
00784          16  FILLER              PIC X(22)       VALUE            EL320
00785                  ' BIRTH DATE INPUT     '.                        EL320
00786          16  FILLER              PIC X(10)       VALUE SPACES.    EL320
00787          16  COMP-OPT-C19        PIC X.                           EL320
00788          16  FILLER              PIC X(7)        VALUE SPACES.    EL320
00789          16  COMP-DESC-C19       PIC X(42)       VALUE SPACES.    EL320
00790      12  COMP-LINE-C20.                                           EL320
00791          16  FILLER              PIC X(22)       VALUE            EL320
00792                  ' MINIMUM PREMIUM      '.                        EL320
00793          16  FILLER              PIC X(5)        VALUE SPACES.    EL320
00794          16  COMP-OPT-C20        PIC ZZZ.99.                      EL320
00795          16  FILLER              PIC X(7)        VALUE SPACES.    EL320
00796          16  COMP-DESC-C20       PIC X(42)       VALUE SPACES.    EL320
00797      EJECT                                                        EL320
00798      12  COMP-LINE-C21.                                           EL320
00799          16  FILLER              PIC X(22)       VALUE            EL320
00800                  ' CONVERSION DATE      '.                        EL320
00801          16  FILLER              PIC X(3)        VALUE SPACES.    EL320
00802          16  COMP-OPT-C21        PIC X(8).                        EL320
00803          16  FILLER              PIC X(7)        VALUE SPACES.    EL320
00804          16  COMP-DESC-C21       PIC X(42)       VALUE SPACES.    EL320
00805      12  COMP-LINE-C22.                                           EL320
00806          16  FILLER              PIC X(22)       VALUE            EL320
00807                  ' LAST BATCH NUMBER    '.                        EL320
00808          16  FILLER              PIC X(3)        VALUE SPACES.    EL320
00809          16  COMP-OPT-C22        PIC ZZZZZZZ9.                    EL320
00810          16  FILLER              PIC X(7)        VALUE SPACES.    EL320
00811          16  COMP-DESC-C22       PIC X(42)       VALUE SPACES.    EL320
00812      12  COMP-LINE-C23.                                           EL320
00813          16  FILLER              PIC X(22)       VALUE            EL320
00814                  ' LAST CHECK NUMBER    '.                        EL320
00815          16  FILLER              PIC X(6)        VALUE SPACES.    EL320
00816          16  COMP-OPT-C23        PIC 9(5).                        EL320
00817          16  FILLER              PIC X(7)        VALUE SPACES.    EL320
00818          16  FILLER              PIC X(38)       VALUE            EL320
00819                  'USED FOR UNIQUE CHECK NUMBERS'.                 EL320
00820      12  COMP-LINE-C24.                                           EL320
00821          16  FILLER              PIC X(22)       VALUE            EL320
00822                  ' LAST CHECK QUE NUMBER'.                        EL320
00823          16  FILLER              PIC X(6)        VALUE SPACES.    EL320
00824          16  COMP-OPT-C24        PIC 9(5).                        EL320
00825          16  FILLER              PIC X(7)        VALUE SPACES.    EL320
00826          16  FILLER              PIC X(38)       VALUE            EL320
00827                  'CHECK BATCH ASSIGNMENTS'.                       EL320
00828      12  COMP-LINE-C25.                                           EL320
00829          16  FILLER              PIC X(26)       VALUE SPACES.    EL320
00830          16  FILLER              PIC X(17)       VALUE            EL320
00831                  ' MAINT     CREATE'.                             EL320
00832      12  COMP-LINE-C26.                                           EL320
00833          16  FILLER              PIC X(25)       VALUE            EL320
00834                  'RATES FILE             : '.                     EL320
00835          16  COMP-OPT-C26A       PIC X(8).                        EL320
00836          16  FILLER              PIC X(3)        VALUE SPACES.    EL320
00837          16  COMP-OPT-C26B       PIC X(8).                        EL320
00838      12  COMP-LINE-C27.                                           EL320
00839          16  FILLER              PIC X(25)       VALUE            EL320
00840                  'COMMISSION TABLE       : '.                     EL320
00841          16  COMP-OPT-C27A       PIC X(8).                        EL320
00842          16  FILLER              PIC X(3)        VALUE SPACES.    EL320
00843          16  COMP-OPT-C27B       PIC X(8).                        EL320
00844      EJECT                                                        EL320
00845      12  COMP-LINE-C28.                                           EL320
00846          16  FILLER              PIC X(25)       VALUE            EL320
00847                  'ACCOUNT MASTER         : '.                     EL320
00848          16  COMP-OPT-C28A       PIC X(8).                        EL320
00849          16  FILLER              PIC X(3)        VALUE SPACES.    EL320
00850          16  COMP-OPT-C28B       PIC X(8).                        EL320
00851      12  COMP-LINE-C29.                                           EL320
00852          16  FILLER              PIC X(25)       VALUE            EL320
00853                  'REINSURANCE TABLE      : '.                     EL320
00854          16  COMP-OPT-C29A       PIC X(8).                        EL320
00855          16  FILLER              PIC X(3)        VALUE SPACES.    EL320
00856          16  COMP-OPT-C29B       PIC X(8).                        EL320
00857      12  COMP-LINE-C30.                                           EL320
00858          16  FILLER              PIC X(25)       VALUE            EL320
00859                  'COMPENSATION MASTER    : '.                     EL320
00860          16  COMP-OPT-C30A       PIC X(8).                        EL320
00861          16  FILLER              PIC X(3)        VALUE SPACES.    EL320
00862          16  COMP-OPT-C30B       PIC X(8).                        EL320
00863      EJECT                                                        EL320
00864      12  PROC-LINE-A1.                                            EL320
00865          16  FILLER              PIC X(23)       VALUE            EL320
00866                  'RECORD TYPE - PROCESSOR'.                       EL320
00867          16  FILLER              PIC X(30)       VALUE SPACE.     EL320
00868          16  FILLER              PIC X(22)       VALUE            EL320
00869                  'LAST MAINTENANCE BY - '.                        EL320
00870          16  PROC-MAINT-BY-A     PIC X(4)        VALUE SPACE.     EL320
00871      12  PROC-LINE-A2.                                            EL320
00872          16  FILLER              PIC X(53)       VALUE SPACES.    EL320
00873          16  FILLER              PIC X(5)        VALUE 'ON - '.   EL320
00874          16  PROC-MAINT-DT-A     PIC X(8).                        EL320
00875          16  FILLER              PIC X(6)        VALUE ' AT - '.  EL320
00876          16  PROC-MAINT-TM-A.                                     EL320
00877              20  PROC-MAINT-HHA  PIC XX.                          EL320
00878              20  FILLER          PIC X           VALUE '.'.       EL320
00879              20  PROC-MAINT-MMA  PIC XX.                          EL320
00880              20  FILLER          PIC X           VALUE '.'.       EL320
00881              20  PROC-MAINT-SSA  PIC XX.                          EL320
00882      12  PROC-LINE-A3.                                            EL320
00883          16  FILLER              PIC X(8)        VALUE            EL320
00884                  'NAME  - '.                                      EL320
00885          16  PROC-NAME-A         PIC X(30).                       EL320
00886      12  PROC-LINE-A4.                                            EL320
00887          16  FILLER              PIC X(8)        VALUE            EL320
00888                  'TITLE - '.                                      EL320
00889          16  PROC-TITLE-A        PIC X(26).                       EL320
00890      12  PROC-LINE-A5.                                            EL320
00891          16  FILLER              PIC X(19)       VALUE            EL320
00892                  'PROCESSOR ID     - '.                           EL320
00893          16  PROC-ID-A           PIC X(4).                        EL320
00894      12  PROC-LINE-A6.                                            EL320
00895          16  FILLER              PIC X(20)       VALUE            EL320
00896                  ' CARRIER SECURITY - '.                          EL320
00897          16  PROC-CAP-A6         PIC X(4).                        EL320
00898      12  PROC-LINE-A7.                                            EL320
00899          16  FILLER              PIC X(20)       VALUE            EL320
00900                  ' ACCOUNT SECURITY - '.                          EL320
00901          16  PROC-CAP-A7         PIC X(4).                        EL320
00902      12  PROC-LINE-A8.                                            EL320
00903          16  FILLER              PIC X(23)       VALUE            EL320
00904                  'PROCESSING CAPABILITIES'.                       EL320
00905          16  FILLER              PIC X(7)        VALUE SPACE.     EL320
00906          16  FILLER              PIC X(9)        VALUE            EL320
00907                  'AVAILABLE'.                                     EL320
00908      12  PROC-LINE-A9.                                            EL320
00909          16  FILLER              PIC X(33)       VALUE            EL320
00910                  ' ACCESS CREDIT'.                                EL320
00911          16  PROC-CAP-A9         PIC XXX.                         EL320
00912      12  PROC-LINE-A10.                                           EL320
00913          16  FILLER              PIC X(33)       VALUE            EL320
00914                  ' ACCESS CLAIMS'.                                EL320
00915          16  PROC-CAP-A10        PIC XXX.                         EL320
00916      EJECT                                                        EL320
00917      12  PROC-LINE-A11.                                           EL320
00918          16  FILLER              PIC X(33)       VALUE            EL320
00919                  ' ACCESS GEN LEDGER'.                            EL320
00920          16  PROC-CAP-A11        PIC XXX.                         EL320
00921      12  PROC-LINE-A12.                                           EL320
00922          16  FILLER              PIC X(33)       VALUE            EL320
00923                  ' RECEIVE LOGON MESSAGES'.                       EL320
00924          16  PROC-CAP-A12        PIC XXX.                         EL320
00925      12  PROC-LINE-A13.                                           EL320
00926          16  FILLER              PIC X(33)       VALUE            EL320
00927                  ' SECURITY OFFICER'.                             EL320
00928          16  PROC-CAP-A13        PIC XXX.                         EL320
00929      12  PROC-LINE-A14.                                           EL320
00930          16  FILLER              PIC X(33)       VALUE            EL320
00931                  ' CREDIT AUTHORIZATION'.                         EL320
00932      12  PROC-LINE-A15.                                           EL320
00933          16  FILLER              PIC X(33)       VALUE            EL320
00934                  ' SYSTEM CONTROLS'.                              EL320
00935          16  PROC-CAP-A15        PIC X(6).                        EL320
00936      12  PROC-LINE-A16.                                           EL320
00937          16  FILLER              PIC X(33)       VALUE            EL320
00938                  ' CAN USER FORCE (CREDIT)'.                      EL320
00939          16  PROC-CAP-A16        PIC XXX.                         EL320
00940      12  PROC-LINE-A17.                                           EL320
00941          16  FILLER              PIC X(33)       VALUE            EL320
00942                  ' CLAIMS AUTHORIZATION'.                         EL320
00943      12  PROC-LINE-A18.                                           EL320
00944          16  FILLER              PIC X(33)       VALUE            EL320
00945                  ' SYSTEM CONTROLS'.                              EL320
00946          16  PROC-CAP-A18        PIC X(6).                        EL320
00947      12  PROC-LINE-A19.                                           EL320
00948          16  FILLER              PIC X(33)       VALUE            EL320
00949                  ' CAN USER FORCE (CLAIMS)'.                      EL320
00950          16  PROC-CAP-A19        PIC XXX.                         EL320
00951      12  PROC-LINE-A20.                                           EL320
00952          16  FILLER              PIC X(21)       VALUE            EL320
00953                  ' LIMITS ON PROCESSING'.                         EL320
00954      12  PROC-LINE-A21.                                           EL320
00955          16  FILLER              PIC X(31)       VALUE            EL320
00956                  '  QUOTED/CALCULATED DAYS       '.               EL320
00957          16  FILLER              PIC X(5)        VALUE SPACES.    EL320
00958          16  PROC-DAYS-A21       PIC ZZ9.                         EL320
00959          16  FILLER              PIC X(38)       VALUE            EL320
00960                  '    ALLOWABLE DIFFERENCE WITHOUT ERROR'.        EL320
00961      12  PROC-LINE-A22.                                           EL320
00962          16  FILLER              PIC X(30)       VALUE            EL320
00963                  '  QUOTED/CALCULATED AMOUNTS   '.                EL320
00964          16  PROC-AMT-A22        PIC  ZZ,ZZZ.99.                  EL320
00965          16  FILLER              PIC X(38)       VALUE            EL320
00966                  '    ALLOWABLE DIFFERENCE WITHOUT ERROR'.        EL320
00967      EJECT                                                        EL320
00968      12  PROC-LINE-A23.                                           EL320
00969          16  FILLER              PIC X(36)       VALUE            EL320
00970                  '  MAXIMUM DAYS PER PAYMENT          '.          EL320
00971          16  PROC-DAYS-A23       PIC ZZ9.                         EL320
00972          16  FILLER              PIC X(38)       VALUE            EL320
00973                  '    GREATER AMOUNT RESULTS IN ERROR   '.        EL320
00974      12  PROC-LINE-A24.                                           EL320
00975          16  FILLER              PIC X(28)       VALUE            EL320
00976                  '  MAXIMUM REGULAR PYMT A&H  '.                  EL320
00977          16  PROC-AMT-A24        PIC  ZZZZ,ZZZ.99.                EL320
00978          16  FILLER              PIC X(38)       VALUE            EL320
00979                  '    GREATER AMOUNT RESULTS IN ERROR   '.        EL320
00980      12  PROC-LINE-A25.                                           EL320
00981          16  FILLER              PIC X(28)       VALUE            EL320
00982                  '  MAXIMUM REGULAR PYMT LIFE '.                  EL320
00983          16  PROC-AMT-A25        PIC  ZZZZ,ZZZ.99.                EL320
00984          16  FILLER              PIC X(38)       VALUE            EL320
00985                  '    GREATER AMOUNT RESULTS IN ERROR   '.        EL320
00986      12  PROC-LINE-A26.                                           EL320
00987          16  FILLER              PIC X(36)       VALUE            EL320
00988                  '  MAXIMUM AUTO PAY MONTHS           '.          EL320
00989          16  PROC-MOS-A26        PIC ZZ9.                         EL320
00990          16  FILLER              PIC X(38)       VALUE            EL320
00991                  '    LONGER TERMS WILL RESULT IN ERROR '.        EL320
00992      12  PROC-LINE-A27.                                           EL320
00993          16  FILLER              PIC X(28)       VALUE            EL320
00994                  '  MAXIMUM AUTOMATIC PAYMENT '.                  EL320
00995          16  PROC-AMT-A27        PIC  ZZZZ,ZZZ.99.                EL320
00996          16  FILLER              PIC X(38)       VALUE            EL320
00997                  '    GREATER AMOUNT RESULTS IN ERROR   '.        EL320
00998      EJECT                                                        EL320
00999      12  PROC-HEAD-B1.                                            EL320
01000          16  FILLER              PIC X(32)       VALUE            EL320
01001                  'RECORD TYPE - PROCESSOR - CREDIT'.              EL320
01002          16  FILLER              PIC X(21)       VALUE SPACE.     EL320
01003          16  FILLER              PIC X(22)       VALUE            EL320
01004                  'LAST MAINTENANCE BY - '.                        EL320
01005          16  PROC-HB1-MAINT-BY   PIC X(4)        VALUE SPACE.     EL320
01006      12  PROC-HEAD-B2.                                            EL320
01007          16  FILLER              PIC X(53)       VALUE SPACES.    EL320
01008          16  FILLER              PIC X(5)        VALUE 'ON - '.   EL320
01009          16  PROC-HB2-MAINT-DATE PIC X(8).                        EL320
01010          16  FILLER              PIC X(6)        VALUE ' AT - '.  EL320
01011          16  PROC-HB2-MAINT-TIME.                                 EL320
01012            20  PROC-HB2-MAINT-HH PIC XX.                          EL320
01013            20  FILLER            PIC X           VALUE '.'.       EL320
01014            20  PROC-HB2-MAINT-MM PIC XX.                          EL320
01015            20  FILLER            PIC X           VALUE '.'.       EL320
01016            20  PROC-HB2-MAINT-SS PIC XX.                          EL320
01017      12  PROC-HEAD-B3.                                            EL320
01018          16  FILLER              PIC X(8)        VALUE            EL320
01019                  'NAME  - '.                                      EL320
01020          16  PROC-HB3-NAME       PIC X(30).                       EL320
01021      12  PROC-HEAD-B4.                                            EL320
01022          16  FILLER              PIC X(8)        VALUE            EL320
01023                  'TITLE - '.                                      EL320
01024          16  PROC-HB4-TITLE      PIC X(26).                       EL320
01025      12  PROC-HEAD-B5.                                            EL320
01026          16  FILLER              PIC X(19)       VALUE            EL320
01027                  'PROCESSOR ID     - '.                           EL320
01028          16  PROC-HB5-PROCESSOR-ID PIC X(4).                      EL320
01029      12  PROC-HEAD-B6.                                            EL320
01030          16  FILLER              PIC X(01)       VALUE SPACE.     EL320
01031          16  PROC-HB6-APPL-DESC  PIC X(33)       VALUE            EL320
01032                  'CREDIT APPLICATIONS'.                           EL320
01033      12  PROC-DETAIL-B.                                           EL320
01034          16  FILLER              PIC X(02)       VALUE SPACE.     EL320
01035          16  PROC-DB-APPL-NBR    PIC X(02).                       EL320
01036          16  FILLER              PIC X(02)       VALUE SPACE.     EL320
01037          16  PROC-DB-APPL-ACCESS PIC X(15).                       EL320
01038          16  PROC-DB-APPL-DESC   PIC X(40).                       EL320
01039          16  FILLER              PIC X(19)       VALUE SPACE.     EL320
01040                                                                   EL320
01041                                                                   EL320
01042                                                                   EL320
01043                                                                   EL320
01044                                                                   EL320
01045      EJECT                                                        EL320
01046      12  STAT-LINE-1.                                             EL320
01047          16  FILLER              PIC X(19)       VALUE            EL320
01048                  'RECORD TYPE - STATE'.                           EL320
01049      12  STAT-LINE-2.                                             EL320
01050          16  FILLER              PIC X(11)       VALUE            EL320
01051                  'CODE   NAME'.                                   EL320
01052          16  FILLER              PIC X(26)       VALUE SPACES.    EL320
01053          16  FILLER              PIC X(28)       VALUE 'ABREV'.   EL320
01054 *                'ABREV    BENEFIT  TYPE  FORM'.                  EL320
01055      12  STAT-LINE-3.                                             EL320
01056          16  FILLER              PIC X        VALUE SPACES.       EL320
01057          16  FILLER              PIC X(13)    VALUE               EL320
01058                  'STATE CODE : '.                                 EL320
01059          16  ST-3-CODE           PIC XX       VALUE SPACES.       EL320
01060          16  FILLER              PIC XXX      VALUE SPACES.       EL320
01061          16  FILLER              PIC X(13)    VALUE               EL320
01062                  'STATE NAME : '.                                 EL320
01063          16  ST-3-NAME           PIC X(28)    VALUE SPACES.       EL320
01064                                                                   EL320
01065      12  STAT-LINE-4.                                             EL320
01066          16  FILLER              PIC X        VALUE SPACES.       EL320
01067          16  FILLER              PIC X(21)    VALUE               EL320
01068                   'STATE ABBREVIATION : '.                        EL320
01069          16  ST-4-ABBREV         PIC XX       VALUE SPACES.       EL320
01070                                                                   EL320
01071      12  STAT-LINE-5.                                             EL320
01072          16  FILLER              PIC X        VALUE SPACES.       EL320
01073          16  FILLER              PIC X(28)    VALUE               EL320
01074                  'EXPENSE ALLOC  -  LIFE    : '.                  EL320
01075          16  ST-5-EXP-ALLOC-L    PIC ZZZ.ZZZZ-.                   EL320
01076          16  FILLER              PIC X(3)     VALUE SPACES.       EL320
01077          16  FILLER              PIC X(11)    VALUE               EL320
01078                 ' A&H     : '.                                    EL320
01079          16  ST-5-EXP-ALLOC-A    PIC ZZZ.ZZZZ-.                   EL320
01080      12  STAT-LINE-6.                                             EL320
01081          16  FILLER              PIC X        VALUE SPACES.       EL320
01082          16  FILLER              PIC X(28)    VALUE               EL320
01083                  'QUOTE/CALC TOL - CLAIMS   : '.                  EL320
01084          16  ST-6-QTE-CLMS       PIC ZZ9.99-.                     EL320
01085          16  FILLER              PIC X(5)     VALUE SPACES.       EL320
01086          16  FILLER              PIC X(11)    VALUE               EL320
01087                  'PREMIUMS : '.                                   EL320
01088          16  ST-6-QTE-PREM       PIC ZZ9.99-.                     EL320
01089          16  FILLER              PIC X(12)    VALUE               EL320
01090                 ' REFUNDS  : '.                                   EL320
01091          16  ST-6-QTE-REFD       PIC ZZ9.99-.                     EL320
01092      12  STAT-LINE-7.                                             EL320
01093          16  FILLER              PIC X        VALUE SPACES.       EL320
01094          16  FILLER              PIC X(28)    VALUE               EL320
01095                  'REJECT SWITCH  - CLAIMS   : '.                  EL320
01096          16  ST-7-REJ-CLMS-SW    PIC X        VALUE SPACES.       EL320
01097          16  FILLER              PIC X(11)    VALUE SPACES.       EL320
01098          16  FILLER              PIC X(11)    VALUE               EL320
01099                 'PREMIUMS : '.                                    EL320
01100          16  ST-7-REJ-PREM-SW    PIC X        VALUE SPACES.       EL320
01101          16  FILLER              PIC X(7)     VALUE SPACES.       EL320
01102          16  FILLER              PIC X(11)    VALUE               EL320
01103                 'REFUNDS  : '.                                    EL320
01104          16  ST-7-REJ-REFD-SW    PIC X        VALUE SPACES.       EL320
01105      12  STAT-LINE-8.                                             EL320
01106          16  FILLER              PIC X        VALUE SPACES.       EL320
01107          16  FILLER              PIC X(28)    VALUE               EL320
01108                  'REFUND-RULES   - MINIMUM  : '.                  EL320
01109          16  ST-8-REF-MIN        PIC ZZZ.ZZ-.                     EL320
01110          16  FILLER              PIC X(5)     VALUE SPACES.       EL320
01111          16  FILLER              PIC X(11)    VALUE               EL320
01112                 'DAYS     : '.                                    EL320
01113          16  ST-8-REF-DAYS       PIC 99.                          EL320
01114          16  FILLER              PIC X        VALUE SPACES.       EL320
01115          16  ST-8-REF-DAYS-SUB   PIC 99.                          EL320
01116      12  STAT-LINE-9.                                             EL320
01117          16  FILLER              PIC X        VALUE SPACES.       EL320
01118          16  FILLER              PIC X(28)    VALUE               EL320
01119                  '1ST PMT EXT    - MAX DAYS : '.                  EL320
01120          16  ST-9-PMT-EXT-DAYS   PIC 999.                         EL320
01121          16  FILLER              PIC X(9)     VALUE SPACES.       EL320
01122          16  FILLER              PIC X(11)    VALUE               EL320
01123                 'CHARGE   : '.                                    EL320
01124          16  ST-9-PMT-EXT-CHRG   PIC X        VALUE SPACES.       EL320
01125      12  STAT-LINE-10.                                            EL320
01126          16  FILLER             PIC X        VALUE SPACES.        EL320
01127          16  FILLER             PIC X(28)    VALUE                EL320
01128                  'STATE CALL     - UNEARNED : '.                  EL320
01129          16  ST-10-CALL-UNERND  PIC X        VALUE SPACES.        EL320
01130          16  FILLER             PIC X(11)    VALUE SPACES.        EL320
01131          16  FILLER             PIC X(11)    VALUE                EL320
01132                 'RPT CNTL : '.                                    EL320
01133          16  ST-10-CALL-CNTL    PIC X        VALUE SPACES.        EL320
01134          16  FILLER             PIC X(7)     VALUE SPACES.        EL320
01135          16  FILLER             PIC X(11)    VALUE                EL320
01136                 'RATE DEV : '.                                    EL320
01137          16  ST-10-CALL-RTE-DEV PIC XXX      VALUE SPACES.        EL320
01138      12  STAT-LINE-11.                                            EL320
01139          16  FILLER             PIC XX       VALUE SPACES.        EL320
01140          16  FILLER             PIC X(21)    VALUE                EL320
01141                  'CODE KIND REF REM/TRM'.                         EL320
01142          16  FILLER             PIC X(6)     VALUE SPACES.        EL320
01143          16  FILLER             PIC X(21)    VALUE                EL320
01144                  'CODE KIND REF REM/TRM'.                         EL320
01145          16  FILLER             PIC X(6)     VALUE SPACES.        EL320
01146          16  FILLER             PIC X(21)    VALUE                EL320
01147                   'CODE KIND REF REM/TRM'.                        EL320
01148      12  STAT-LINE-12.                                            EL320
01149          16  ST-LINE-12-DETAIL  OCCURS 3 TIMES.                   EL320
01150              20  FILLER             PIC XXX.                      EL320
01151              20  ST-12-BEN-CD       PIC ZZ.                       EL320
01152              20  FILLER             PIC XXX.                      EL320
01153              20  ST-12-BEN-KND      PIC X.                        EL320
01154              20  FILLER             PIC X(4).                     EL320
01155              20  ST-12-BEN-REF      PIC X.                        EL320
01156              20  FILLER             PIC X(5).                     EL320
01157              20  ST-12-BEN-REM      PIC X.                        EL320
01158              20  FILLER             PIC X(7).                     EL320
01159                                                                   EL320
01160      12  STAT-DETAIL.                                             EL320
01161          16  FILLER              PIC X           VALUE SPACE.     EL320
01162          16  STAT-CODE           PIC XX.                          EL320
01163          16  FILLER              PIC X(4)        VALUE SPACES.    EL320
01164          16  STAT-NAME           PIC X(28)       VALUE SPACES.    EL320
01165          16  FILLER              PIC XXX         VALUE SPACE.     EL320
01166          16  STAT-ABREV          PIC XX.                          EL320
01167          16  FILLER              PIC X(9)        VALUE SPACES.    EL320
01168          16  STAT-BENEF          PIC XX          VALUE SPACE.     EL320
01169          16  FILLER              PIC X(4)        VALUE SPACE.     EL320
01170          16  STAT-TYPE           PIC X(4)        VALUE SPACES.    EL320
01171          16  FILLER              PIC X(2)        VALUE SPACES.    EL320
01172          16  STAT-FORM           PIC X(12)       VALUE SPACES.    EL320
01173      EJECT                                                        EL320
01174      12  BNFT-LINE-1.                                             EL320
01175          16  FILLER              PIC X(14)       VALUE            EL320
01176                  'RECORD TYPE - '.                                EL320
01177          16  BNFT-REC-DESC       PIC X(06)       VALUE SPACES.    EL320
01178          16  BNFT-REC-TYPE       PIC X(09)       VALUE SPACES.    EL320
01179      12  BNFT-LINE-2.                                             EL320
01180          16  FILLER             PIC X(45)    VALUE SPACES.        EL320
01181          16  FILLER             PIC X(27)    VALUE                EL320
01182                  '   LOAN         J S L R R I'.                   EL320
01183      12  BNFT-LINE-3.                                             EL320
01184          16  FILLER             PIC X        VALUE SPACES.        EL320
01185          16  FILLER             PIC X(30)    VALUE                EL320
01186                  'CODE ABRV   DESC       COMMENT'.                EL320
01187          16  FILLER             PIC X(30)    VALUE                EL320
01188                  '     EARNING     TYPE  O.B.   '.                EL320
01189          16  FILLER             PIC X(12)    VALUE                EL320
01190                  'T P R T M G '.                                  EL320
01191      12  BNFT-DETAIL.                                             EL320
01192          16  FILLER             PIC X        VALUE SPACES.        EL320
01193          16  BNFT-CODE          PIC XX       VALUE SPACES.        EL320
01194          16  FILLER             PIC XXX      VALUE SPACES.        EL320
01195          16  BNFT-ABREV         PIC XXX      VALUE SPACES.        EL320
01196          16  FILLER             PIC XX       VALUE SPACES.        EL320
01197          16  BNFT-DESC          PIC X(11)    VALUE SPACES.        EL320
01198          16  FILLER             PIC X        VALUE SPACES.        EL320
01199          16  BNFT-COMMENT       PIC X(10)    VALUE SPACES.        EL320
01200          16  FILLER             PIC XX       VALUE SPACES.        EL320
01201          16  BNFT-EARN          PIC X(8)     VALUE SPACES.        EL320
01202          16  FILLER             PIC X(2)     VALUE SPACES.        EL320
01203          16  BNFT-TYPE          PIC X(8)     VALUE SPACES.        EL320
01204          16  FILLER             PIC X(2)     VALUE SPACES.        EL320
01205          16  BNFT-OB            PIC XXX      VALUE SPACES.        EL320
01206          16  FILLER             PIC X        VALUE SPACES.        EL320
01207          16  BNFT-EM            PIC X        VALUE SPACES.        EL320
01208          16  FILLER             PIC X        VALUE SPACES.        EL320
01209          16  BNFT-JT            PIC X        VALUE SPACES.        EL320
01210          16  FILLER             PIC X        VALUE SPACES.        EL320
01211          16  BNFT-SP            PIC X        VALUE SPACES.        EL320
01212          16  FILLER             PIC X        VALUE SPACES.        EL320
01213          16  BNFT-LR            PIC X        VALUE SPACES.        EL320
01214          16  FILLER             PIC X        VALUE SPACES.        EL320
01215          16  BNFT-RT            PIC X        VALUE SPACES.        EL320
01216          16  FILLER             PIC X        VALUE SPACES.        EL320
01217          16  BNFT-RM            PIC X        VALUE SPACES.        EL320
01218          16  FILLER             PIC X        VALUE SPACES.        EL320
01219          16  BNFT-IG            PIC X        VALUE SPACES.        EL320
01220      12  BNFT-LINE-2-A.                                           EL320
01221          16  FILLER              PIC X(63)       VALUE SPACES.    EL320
01222          16  FILLER              PIC X(20)       VALUE            EL320
01223                           'LOAN   S R I        '.                 EL320
01224      12  BNFT-LINE-3-A.                                           EL320
01225          16  FILLER              PIC X           VALUE SPACE.     EL320
01226          16  FILLER              PIC X(26)       VALUE            EL320
01227                  'CODE  ABRV  DESCRIPTION CO'.                    EL320
01228          16  FILLER              PIC X(26)       VALUE            EL320
01229                  'MMENT     EARNING   REM-TR'.                    EL320
01230          16  FILLER              PIC X(29)       VALUE            EL320
01231                  'M   O.B.  TYPE   P M G       '.                 EL320
01232      12  BNFT-DETAIL-A.                                           EL320
01233          16  FILLER              PIC XX          VALUE SPACE.     EL320
01234          16  BNFT-CODE-A         PIC XX.                          EL320
01235          16  FILLER              PIC X(3)        VALUE SPACES.    EL320
01236          16  BNFT-ABREV-A        PIC XXX         VALUE SPACES.    EL320
01237          16  FILLER              PIC X(3)        VALUE SPACE.     EL320
01238          16  BNFT-DESC-A         PIC X(11)       VALUE SPACES.    EL320
01239          16  FILLER              PIC X           VALUE SPACES.    EL320
01240          16  BNFT-COMMENT-A      PIC X(10)       VALUE SPACES.    EL320
01241          16  FILLER              PIC X(2)        VALUE SPACES.    EL320
01242          16  BNFT-EARN-A         PIC X(8)        VALUE SPACES.    EL320
01243          16  FILLER              PIC XX          VALUE SPACE.     EL320
01244          16  BNFT-REM-A          PIC X(8)        VALUE SPACES.    EL320
01245          16  FILLER              PIC XX          VALUE SPACES.    EL320
01246          16  BNFT-OB-A           PIC X(3)        VALUE SPACES.    EL320
01247          16  FILLER              PIC X(1)        VALUE SPACES.    EL320
01248          16  BNFT-TYPE-A         PIC X(8)        VALUE SPACES.    EL320
01249          16  FILLER              PIC X(1)        VALUE SPACES.    EL320
01250          16  BNFT-SP-A           PIC X           VALUE SPACE.     EL320
01251          16  FILLER              PIC X           VALUE SPACE.     EL320
01252          16  BNFT-RM-A           PIC X           VALUE SPACE.     EL320
01253          16  FILLER              PIC X           VALUE SPACE.     EL320
01254          16  BNFT-IG-A           PIC X           VALUE SPACE.     EL320
01255          16  FILLER              PIC X           VALUE SPACE.     EL320
01256      EJECT                                                        EL320
01257      12  CARR-LINE-1.                                             EL320
01258          16  FILLER              PIC X(21)       VALUE            EL320
01259                  'RECORD TYPE - CARRIER'.                         EL320
01260          16  FILLER              PIC X(32)       VALUE SPACE.     EL320
01261          16  FILLER              PIC X(22)       VALUE            EL320
01262                  'LAST MAINTENANCE BY - '.                        EL320
01263          16  CARR-MAINT-BY       PIC X(4)        VALUE SPACE.     EL320
01264      12  CARR-LINE-2.                                             EL320
01265          16  FILLER              PIC X(53)       VALUE SPACES.    EL320
01266          16  FILLER              PIC X(5)        VALUE 'ON - '.   EL320
01267          16  CARR-MAINT-DT       PIC X(8).                        EL320
01268          16  FILLER              PIC X(6)        VALUE ' AT - '.  EL320
01269          16  CARR-MAINT-TM.                                       EL320
01270              20  CARR-MAINT-HH   PIC XX.                          EL320
01271              20  FILLER          PIC X           VALUE '.'.       EL320
01272              20  CARR-MAINT-MM   PIC XX.                          EL320
01273              20  FILLER          PIC X           VALUE '.'.       EL320
01274              20  CARR-MAINT-SS   PIC XX.                          EL320
01275      12  CARR-LINE-3.                                             EL320
01276          16  FILLER              PIC X(17)       VALUE            EL320
01277                  'CARRIER NUMBER - '.                             EL320
01278          16  CARR-NO             PIC X.                           EL320
01279      12  CARR-LINE-4.                                             EL320
01280          16  FILLER              PIC X(15)       VALUE            EL320
01281                  'CARRIER NAME - '.                               EL320
01282          16  CARR-NAME           PIC X(30).                       EL320
01283          16  FILLER              PIC X(19)       VALUE            EL320
01284                  '     CURRENT COUNTS'.                           EL320
01285      12  CARR-LINE-5.                                             EL320
01286          16  FILLER              PIC X(15)       VALUE            EL320
01287                  '  IN CARE OF - '.                               EL320
01288          16  CARR-IN-CARE        PIC X(30).                       EL320
01289      12  CARR-LINE-6.                                             EL320
01290          16  FILLER              PIC X(15)       VALUE            EL320
01291                             '  ADDRESS 1  - '.                    EL320
01292          16  CARR-ADDRESS-1      PIC X(30)       VALUE SPACES.    EL320
01293          16  FILLER              PIC X(23)       VALUE            EL320
01294                  '      CLAIM NUMBER   - '.                       EL320
01295          16  CARR-CLAIM-NO       PIC 9(8).                        EL320
01296      12  CARR-LINE-7.                                             EL320
01297          16  FILLER              PIC X(15)       VALUE            EL320
01298                  '  ADDRESS 2  - '.                               EL320
01299          16  CARR-ADDRESS-2      PIC X(30)       VALUE SPACES.    EL320
01300          16  FILLER              PIC X(23)       VALUE            EL320
01301                  '      CHECK NUMBER   - '.                       EL320
01302          16  CARR-CHECK-NO       PIC 9(8).                        EL320
01303      12  CARR-LINE-8.                                             EL320
01304          16  FILLER              PIC X(15)       VALUE            EL320
01305                  '  CITY STATE - '.                               EL320
01306          16  CARR-CITY-STATE     PIC X(30)       VALUE SPACES.    EL320
01307          16  FILLER              PIC X(31)       VALUE SPACES.    EL320
01308      EJECT                                                        EL320
01309      12  CARR-LINE-9.                                             EL320
01310          16  FILLER              PIC X(15)       VALUE            EL320
01311                  '  ZIP  PHONE - '.                               EL320
01312          16  CARR-ZIP            PIC X(9)        VALUE ZEROS.     EL320
01313          16  FILLER              PIC X(9)        VALUE SPACES.    EL320
01314          16  CARR-PHONE.                                          EL320
01315              20  CARR-PHONE-AC   PIC 999         VALUE ZEROS.     EL320
01316              20  FILLER          PIC X           VALUE '-'.       EL320
01317              20  CARR-PHONE-PF   PIC 999         VALUE ZEROS.     EL320
01318              20  FILLER          PIC X           VALUE '-'.       EL320
01319              20  CARR-PHONE-NO   PIC 9(4)        VALUE ZEROS.     EL320
01320          16  FILLER              PIC X(31)       VALUE SPACES.    EL320
01321      12  CARR-LINE-10.                                            EL320
01322          16  FILLER              PIC X(15)       VALUE            EL320
01323                  '  DOMICILE ST- '.                               EL320
01324          16  CARR-DOM-ST         PIC XX          VALUE SPACES.    EL320
01325          16  FILLER              PIC X(59)       VALUE SPACES.    EL320
01326      12  CARR-LINE-11.                                            EL320
01327          16  FILLER              PIC X(10)       VALUE            EL320
01328                  'FIELD NAME'.                                    EL320
01329          16  FILLER              PIC X(24)       VALUE SPACES.    EL320
01330          16  FILLER              PIC X(20)       VALUE            EL320
01331                  'VALUE    DESCRIPTION'.                          EL320
01332      12  CARR-LINE-12.                                            EL320
01333          16  FILLER              PIC X(24)       VALUE            EL320
01334                  ' CLAIM NUMBER ASSIGNMENT'.                      EL320
01335          16  FILLER              PIC X(14)       VALUE SPACES.    EL320
01336          16  CARR-CLAIM-NO-ASS   PIC X.                           EL320
01337          16  FILLER              PIC X(4)        VALUE SPACES.    EL320
01338          16  CARR-DESC-12        PIC X(20)       VALUE SPACES.    EL320
01339      12  CARR-LINE-13.                                            EL320
01340          16  FILLER              PIC X(24)       VALUE            EL320
01341                  ' CHECK NUMBER ASSIGNMENT'.                      EL320
01342          16  FILLER              PIC X(14)       VALUE SPACES.    EL320
01343          16  CARR-CHECK-NO-ASS   PIC X.                           EL320
01344          16  FILLER              PIC X(4)        VALUE SPACES.    EL320
01345          16  CARR-DESC-13        PIC X(21)       VALUE SPACES.    EL320
01346      12  CARR-LINE-14.                                            EL320
01347          16  FILLER              PIC X(38)       VALUE            EL320
01348                  ' EXPENSE CALCULATION - TYPE           '.        EL320
01349          16  CARR-CALC-TYPE      PIC X           VALUE SPACES.    EL320
01350          16  FILLER              PIC X(4)        VALUE SPACES.    EL320
01351          16  CARR-DESC-14        PIC X(20)       VALUE SPACES.    EL320
01352      12  CARR-LINE-15.                                            EL320
01353          16  FILLER              PIC X(23)       VALUE SPACES.    EL320
01354          16  FILLER              PIC X(10)       VALUE            EL320
01355                  'AMOUNT    '.                                    EL320
01356          16  CARR-CALC-AMT       PIC ZZZ.99.                      EL320
01357      12  CARR-LINE-16.                                            EL320
01358          16  FILLER              PIC X(23)       VALUE SPACES.    EL320
01359          16  FILLER              PIC X(10)       VALUE            EL320
01360                  'PERCENT   '.                                    EL320
01361          16  CARR-CALC-PERCT     PIC ZZZ.99.                      EL320
01362      EJECT                                                        EL320
01363      12  CARR-LINE-17.                                            EL320
01364          16  FILLER              PIC X(38)       VALUE            EL320
01365                  ' CLAIM CALCULATION METHOD             '.        EL320
01366          16  CARR-CALC-METHOD    PIC X.                           EL320
01367          16  FILLER              PIC X(4)        VALUE SPACES.    EL320
01368          16  CARR-DESC-17        PIC X(20)       VALUE SPACES.    EL320
01369      12  CARR-LINE-18.                                            EL320
01370          16  FILLER              PIC X(36)       VALUE            EL320
01371                  ' CLAIM RESERVES USE - MANUAL        '.          EL320
01372          16  CARR-CLAIM-MAN      PIC XXX.                         EL320
01373      12  CARR-LINE-19.                                            EL320
01374          16  FILLER              PIC X(22)       VALUE SPACES.    EL320
01375          16  FILLER              PIC X(14)       VALUE            EL320
01376                  'FUTURE        '.                                EL320
01377          16  CARR-CLAIM-FUT      PIC XXX.                         EL320
01378      12  CARR-LINE-20.                                            EL320
01379          16  FILLER              PIC X(22)       VALUE SPACES.    EL320
01380          16  FILLER              PIC X(14)       VALUE            EL320
01381                  'I.B.N.R.      '.                                EL320
01382          16  CARR-CLAIM-IBNR     PIC XXX.                         EL320
01383      12  CARR-LINE-21.                                            EL320
01384          16  FILLER              PIC X(22)       VALUE SPACES.    EL320
01385          16  FILLER              PIC X(14)       VALUE            EL320
01386                  'P.T.C.        '.                                EL320
01387          16  CARR-CLAIM-PTC      PIC XXX.                         EL320
01388      12  CARR-LINE-22.                                            EL320
01389          16  FILLER              PIC X(22)       VALUE SPACES.    EL320
01390          16  FILLER              PIC X(14)       VALUE            EL320
01391                  'CDT PCNT   '.                                   EL320
01392          16  CARR-CDT-PCNT       PIC ZZZ.99.                      EL320
01393      12  CARR-LINE-23.                                            EL320
01394          16  FILLER              PIC X(22)       VALUE SPACES.    EL320
01395          16  FILLER              PIC X(14)       VALUE            EL320
01396                  'IBNR PCNT  '.                                   EL320
01397          16  CARR-IBNR-PCNT      PIC ZZZ.99.                      EL320
01398      12  CARR-LINE-24.                                            EL320
01399          16  FILLER              PIC X(20)       VALUE            EL320
01400                  'LIMITS ON PROCESSING'.                          EL320
01401      12  CARR-LINE-25.                                            EL320
01402          16  FILLER              PIC X(37)       VALUE            EL320
01403                  ' QUOTED/CALCULATED DAYS              '.         EL320
01404          16  CARR-LIMIT-25       PIC ZZ9.                         EL320
01405          16  FILLER              PIC X(38)       VALUE            EL320
01406                  '    ALLOWABLE DIFFERENCE WITHOUT ERROR'.        EL320
01407      12  CARR-LINE-26.                                            EL320
01408          16  FILLER              PIC X(34)       VALUE            EL320
01409                  ' QUOTED/CALCULATED AMOUNTS        '.            EL320
01410          16  CARR-LIMIT-26       PIC ZZZ.99.                      EL320
01411          16  FILLER              PIC X(38)       VALUE            EL320
01412                  '    ALLOWABLE DIFFERENCE WITHOUT ERROR'.        EL320
01413      EJECT                                                        EL320
01414      12  CARR-LINE-27.                                            EL320
01415          16  FILLER              PIC X(36)       VALUE            EL320
01416                  ' MAXIMUM DAYS PER PAYMENT           '.          EL320
01417          16  CARR-LIMIT-27       PIC ZZZ9.                        EL320
01418          16  FILLER              PIC X(38)       VALUE            EL320
01419                  '    GREATER AMOUNT RESULTS IN ERROR   '.        EL320
01420      12  CARR-LINE-28.                                            EL320
01421          16  FILLER              PIC X(30)       VALUE            EL320
01422                  ' MAXIMUM REGULAR PAYMENT      '.                EL320
01423          16  CARR-LIMIT-28       PIC ZZZZZZZ.99.                  EL320
01424          16  FILLER              PIC X(38)       VALUE            EL320
01425                  '    GREATER AMOUNT RESULTS IN ERROR   '.        EL320
01426      12  CARR-LINE-29.                                            EL320
01427          16  FILLER              PIC X(36)       VALUE            EL320
01428                  ' MAXIMUM AUTO PAY MONTHS            '.          EL320
01429          16  CARR-LIMIT-29       PIC ZZZ9.                        EL320
01430          16  FILLER              PIC X(38)       VALUE            EL320
01431                  '    LONGER TERMS WILL RESULT IN ERROR '.        EL320
01432      12  CARR-LINE-30.                                            EL320
01433          16  FILLER              PIC X(30)       VALUE            EL320
01434                  ' MAXIMUM AUTOMATIC PAYMENT    '.                EL320
01435          16  CARR-LIMIT-30       PIC ZZZZZZZ.99.                  EL320
01436          16  FILLER              PIC X(38)       VALUE            EL320
01437                  '    GREATER AMOUNT RESULTS IN ERROR   '.        EL320
01438      12  CARR-LINE-31.                                            EL320
01439          16  FILLER              PIC X(36)       VALUE            EL320
01440                  ' DAYS BEFORE CLOSED                 '.          EL320
01441          16  CARR-LIMIT-31       PIC ZZZ9.                        EL320
01442          16  FILLER              PIC X(38)       VALUE            EL320
01443                  '    AUTOMATICALLY CLOSED IF INACTIVE '.         EL320
01444      12  CARR-LINE-32.                                            EL320
01445          16  FILLER              PIC X(37)       VALUE            EL320
01446                  ' MONTHS BEFORE PURGED                '.         EL320
01447          16  CARR-LIMIT-32       PIC ZZ9.                         EL320
01448          16  FILLER              PIC X(38)       VALUE            EL320
01449                  '    AUTOMATICALLY PURGED IF CLOSED    '.        EL320
01450      EJECT                                                        EL320
01451      12  MORT-LINE-1.                                             EL320
01452          16  FILLER              PIC X(29)       VALUE            EL320
01453                  'RECORD TYPE - MORTALITY TABLE'.                 EL320
01454          16  FILLER              PIC X(24)       VALUE SPACE.     EL320
01455          16  FILLER              PIC X(22)       VALUE            EL320
01456                  'LAST MAINTENANCE BY - '.                        EL320
01457          16  MORT-MAINT-BY       PIC X(4)        VALUE SPACE.     EL320
01458      12  MORT-LINE-2.                                             EL320
01459          16  FILLER              PIC X(53)       VALUE SPACES.    EL320
01460          16  FILLER              PIC X(5)        VALUE 'ON - '.   EL320
01461          16  MORT-MAINT-DT       PIC X(8).                        EL320
01462          16  FILLER              PIC X(6)        VALUE ' AT - '.  EL320
01463          16  MORT-MAINT-TM.                                       EL320
01464              20  MORT-MAINT-HH   PIC XX.                          EL320
01465              20  FILLER          PIC X           VALUE '.'.       EL320
01466              20  MORT-MAINT-MM   PIC XX.                          EL320
01467              20  FILLER          PIC X           VALUE '.'.       EL320
01468              20  MORT-MAINT-SS   PIC XX.                          EL320
01469      12  MORT-LINE-3.                                             EL320
01470          16  FILLER              PIC X(3)        VALUE SPACES.    EL320
01471          16  FILLER              PIC X(44)       VALUE            EL320
01472                  'TABLE TYPE INTEREST AGE MTHD RESERVE ADJ DIR'.  EL320
01473          16  FILLER              PIC X(33)       VALUE            EL320
01474                  ' JNT FACT JNT CDE PC-Q  MORT CODE'.             EL320
01475      12  MORT-DETAIL.                                             EL320
01476          16  FILLER              PIC X(03)       VALUE SPACES.    EL320
01477          16  MORT-TABLE          PIC X(05).                       EL320
01478          16  FILLER              PIC X(02)       VALUE SPACES.    EL320
01479          16  MORT-TYPE           PIC X(01).                       EL320
01480          16  FILLER              PIC X(04)       VALUE SPACES.    EL320
01481          16  MORT-INTEREST       PIC .9999.                       EL320
01482          16  FILLER              PIC X(06)       VALUE SPACES.    EL320
01483          16  MORT-AGE-METHOD     PIC X(02).                       EL320
01484          16  FILLER              PIC X(04)       VALUE SPACES.    EL320
01485          16  MORT-RESERVE-ADJ    PIC 9.9999.                      EL320
01486          16  FILLER              PIC X(05)       VALUE SPACES.    EL320
01487          16  MORT-ADJ-DIRECTION  PIC X(01).                       EL320
01488          16  FILLER              PIC X(04)       VALUE SPACES.    EL320
01489          16  MORT-JOINT-FACTOR   PIC 9.9999.                      EL320
01490          16  FILLER              PIC X(05)       VALUE SPACES.    EL320
01491          16  MORT-JOINT-CODE     PIC X(01).                       EL320
01492          16  FILLER              PIC X(07)       VALUE SPACES.    EL320
01493          16  MORT-PC-Q           PIC X(01).                       EL320
01494          16  FILLER              PIC X(05)       VALUE SPACES.    EL320
01495          16  MORT-MORT-CODE      PIC X(04).                       EL320
01496          16  FILLER              PIC X(03)       VALUE SPACES.    EL320
01497      EJECT                                                        EL320
01498      12  BUSS-LINE-1.                                             EL320
01499          16  FILLER              PIC X(27)       VALUE            EL320
01500                  'RECORD TYPE - BUSINESS TYPE'.                   EL320
01501          16  FILLER              PIC X(26)       VALUE SPACE.     EL320
01502          16  FILLER              PIC X(22)       VALUE            EL320
01503                  'LAST MAINTENANCE BY - '.                        EL320
01504          16  BUSS-MAINT-BY       PIC X(4)        VALUE SPACE.     EL320
01505      12  BUSS-LINE-2.                                             EL320
01506          16  FILLER              PIC X(53)       VALUE SPACES.    EL320
01507          16  FILLER              PIC X(5)        VALUE 'ON - '.   EL320
01508          16  BUSS-MAINT-DT       PIC X(8).                        EL320
01509          16  FILLER              PIC X(6)        VALUE ' AT - '.  EL320
01510          16  BUSS-MAINT-TM.                                       EL320
01511              20  BUSS-MAINT-HH   PIC XX.                          EL320
01512              20  FILLER          PIC X           VALUE '.'.       EL320
01513              20  BUSS-MAINT-MM   PIC XX.                          EL320
01514              20  FILLER          PIC X           VALUE '.'.       EL320
01515              20  BUSS-MAINT-SS   PIC XX.                          EL320
01516      12  BUSS-LINE-3.                                             EL320
01517          16  FILLER              PIC X(11)       VALUE            EL320
01518                  'CODE   NAME'.                                   EL320
01519          16  FILLER              PIC X(42)       VALUE SPACES.    EL320
01520          16  FILLER              PIC X(11)       VALUE            EL320
01521                  'CODE   NAME'.                                   EL320
01522      12  BUSS-DETAIL.                                             EL320
01523          16  FILLER              PIC X           VALUE SPACE.     EL320
01524          16  BUSS-CODE           PIC XX.                          EL320
01525          16  FILLER              PIC X(4)        VALUE SPACES.    EL320
01526          16  BUSS-NAME           PIC X(24)       VALUE SPACES.    EL320
01527          16  FILLER              PIC X(22)       VALUE SPACE.     EL320
01528          16  BUSS-CODE-2         PIC XX.                          EL320
01529          16  FILLER              PIC X(4)        VALUE SPACES.    EL320
01530          16  BUSS-NAME-2         PIC X(24)       VALUE SPACES.    EL320
01531      EJECT                                                        EL320
01532      12  TERM-LINE-1.                                             EL320
01533          16  FILLER              PIC X(29)       VALUE            EL320
01534                  'RECORD TYPE - TERMINAL MASTER'.                 EL320
01535          16  FILLER              PIC X(24)       VALUE SPACE.     EL320
01536          16  FILLER              PIC X(22)       VALUE            EL320
01537                  'LAST MAINTENANCE BY - '.                        EL320
01538          16  TERM-MAINT-BY       PIC X(4)        VALUE SPACE.     EL320
01539      12  TERM-LINE-2.                                             EL320
01540          16  FILLER              PIC X(53)       VALUE SPACES.    EL320
01541          16  FILLER              PIC X(5)        VALUE 'ON - '.   EL320
01542          16  TERM-MAINT-DT       PIC X(8).                        EL320
01543          16  FILLER              PIC X(6)        VALUE ' AT - '.  EL320
01544          16  TERM-MAINT-TM.                                       EL320
01545              20  TERM-MAINT-HH   PIC XX.                          EL320
01546              20  FILLER          PIC X           VALUE '.'.       EL320
01547              20  TERM-MAINT-MM   PIC XX.                          EL320
01548              20  FILLER          PIC X           VALUE '.'.       EL320
01549              20  TERM-MAINT-SS   PIC XX.                          EL320
01550      12  TERM-LINE-3.                                             EL320
01551          16  FILLER              PIC X(16)       VALUE            EL320
01552                  'ONLINE TERMINALS'.                              EL320
01553      12  TERM-DETAIL-LINE.                                        EL320
01554          16  TERM-ARRAY  OCCURS  10  TIMES.                       EL320
01555              20  TERM-LINE   OCCURS  12  TIMES.                   EL320
01556                  24  FILLER      PIC XX.                          EL320
01557                  24  TERMNL      PIC X(4).                        EL320
01558      EJECT                                                        EL320
01559      12  LFED-LINE-1.                                             EL320
01560          16  FILLER              PIC X(30)       VALUE            EL320
01561                  'RECORD TYPE - LIFE EDIT MASTER'.                EL320
01562          16  FILLER              PIC X(23)       VALUE SPACE.     EL320
01563          16  FILLER              PIC X(22)       VALUE            EL320
01564                  'LAST MAINTENANCE BY - '.                        EL320
01565          16  LFED-MAINT-BY       PIC X(4)        VALUE SPACE.     EL320
01566      12  LFED-LINE-2.                                             EL320
01567          16  FILLER              PIC X(53)       VALUE SPACES.    EL320
01568          16  FILLER              PIC X(5)        VALUE 'ON - '.   EL320
01569          16  LFED-MAINT-DT       PIC X(8).                        EL320
01570          16  FILLER              PIC X(6)        VALUE ' AT - '.  EL320
01571          16  LFED-MAINT-TM.                                       EL320
01572              20  LFED-MAINT-HH   PIC XX.                          EL320
01573              20  FILLER          PIC X           VALUE '.'.       EL320
01574              20  LFED-MAINT-MM   PIC XX.                          EL320
01575              20  FILLER          PIC X           VALUE '.'.       EL320
01576              20  LFED-MAINT-SS   PIC XX.                          EL320
01577      12  LFED-LINE-3.                                             EL320
01578          16  FILLER              PIC X(16)       VALUE            EL320
01579                  'LIFE EDIT TABLES'.                              EL320
01580      12  LFED-LINE-4.                                             EL320
01581          16  FILLER              PIC X(3)        VALUE SPACE.     EL320
01582          16  FILLER              PIC X(6)        VALUE            EL320
01583                  'IN OUT'.                                        EL320
01584          16  FILLER              PIC X(2)        VALUE SPACE.     EL320
01585          16  FILLER              PIC X(6)        VALUE            EL320
01586                  'IN OUT'.                                        EL320
01587          16  FILLER              PIC X(2)        VALUE SPACE.     EL320
01588          16  FILLER              PIC X(6)        VALUE            EL320
01589                  'IN OUT'.                                        EL320
01590          16  FILLER              PIC X(2)        VALUE SPACE.     EL320
01591          16  FILLER              PIC X(6)        VALUE            EL320
01592                  'IN OUT'.                                        EL320
01593          16  FILLER              PIC X(2)        VALUE SPACE.     EL320
01594          16  FILLER              PIC X(6)        VALUE            EL320
01595                  'IN OUT'.                                        EL320
01596          16  FILLER              PIC X(2)        VALUE SPACE.     EL320
01597          16  FILLER              PIC X(6)        VALUE            EL320
01598                  'IN OUT'.                                        EL320
01599          16  FILLER              PIC X(2)        VALUE SPACE.     EL320
01600          16  FILLER              PIC X(6)        VALUE            EL320
01601                  'IN OUT'.                                        EL320
01602          16  FILLER              PIC X(2)        VALUE SPACE.     EL320
01603          16  FILLER              PIC X(6)        VALUE            EL320
01604                  'IN OUT'.                                        EL320
01605      12  LFED-DETAIL-LINE.                                        EL320
01606          16  LFED-ARRAY  OCCURS  15  TIMES.                       EL320
01607              20  LFED-LINE   OCCURS  8  TIMES.                    EL320
01608                  24  FILLER      PIC XXX.                         EL320
01609                  24  LFED-CD-IN  PIC XX.                          EL320
01610                  24  FILLER      PIC X.                           EL320
01611                  24  LFED-CD-OT  PIC XX.                          EL320
01612      EJECT                                                        EL320
01613      12  AHED-LINE-1.                                             EL320
01614          16  FILLER              PIC X(29)       VALUE            EL320
01615                  'RECORD TYPE - A&H EDIT MASTER'.                 EL320
01616          16  FILLER              PIC X(24)       VALUE SPACE.     EL320
01617          16  FILLER              PIC X(22)       VALUE            EL320
01618                  'LAST MAINTENANCE BY - '.                        EL320
01619          16  AHED-MAINT-BY       PIC X(4)        VALUE SPACE.     EL320
01620      12  AHED-LINE-2.                                             EL320
01621          16  FILLER              PIC X(53)       VALUE SPACES.    EL320
01622          16  FILLER              PIC X(5)        VALUE 'ON - '.   EL320
01623          16  AHED-MAINT-DT       PIC X(8).                        EL320
01624          16  FILLER              PIC X(6)        VALUE ' AT - '.  EL320
01625          16  AHED-MAINT-TM.                                       EL320
01626              20  AHED-MAINT-HH   PIC XX.                          EL320
01627              20  FILLER          PIC X           VALUE '.'.       EL320
01628              20  AHED-MAINT-MM   PIC XX.                          EL320
01629              20  FILLER          PIC X           VALUE '.'.       EL320
01630              20  AHED-MAINT-SS   PIC XX.                          EL320
01631      12  AHED-LINE-3.                                             EL320
01632          16  FILLER              PIC X(16)       VALUE            EL320
01633                  'A&H EDIT TABLES'.                               EL320
01634      12  AHED-LINE-4.                                             EL320
01635          16  FILLER              PIC X(3)        VALUE SPACE.     EL320
01636          16  FILLER              PIC X(6)        VALUE            EL320
01637                  'IN OUT'.                                        EL320
01638          16  FILLER              PIC X(2)        VALUE SPACE.     EL320
01639          16  FILLER              PIC X(6)        VALUE            EL320
01640                  'IN OUT'.                                        EL320
01641          16  FILLER              PIC X(2)        VALUE SPACE.     EL320
01642          16  FILLER              PIC X(6)        VALUE            EL320
01643                  'IN OUT'.                                        EL320
01644          16  FILLER              PIC X(2)        VALUE SPACE.     EL320
01645          16  FILLER              PIC X(6)        VALUE            EL320
01646                  'IN OUT'.                                        EL320
01647          16  FILLER              PIC X(2)        VALUE SPACE.     EL320
01648          16  FILLER              PIC X(6)        VALUE            EL320
01649                  'IN OUT'.                                        EL320
01650          16  FILLER              PIC X(2)        VALUE SPACE.     EL320
01651          16  FILLER              PIC X(6)        VALUE            EL320
01652                  'IN OUT'.                                        EL320
01653          16  FILLER              PIC X(2)        VALUE SPACE.     EL320
01654          16  FILLER              PIC X(6)        VALUE            EL320
01655                  'IN OUT'.                                        EL320
01656          16  FILLER              PIC X(2)        VALUE SPACE.     EL320
01657          16  FILLER              PIC X(6)        VALUE            EL320
01658                  'IN OUT'.                                        EL320
01659      12  AHED-DETAIL-LINE.                                        EL320
01660          16  AHED-ARRAY  OCCURS  12  TIMES.                       EL320
01661              20  AHED-LINE   OCCURS  8  TIMES.                    EL320
01662                  24  FILLER      PIC XX.                          EL320
01663                  24  AHED-CD-IN  PIC XXX.                         EL320
01664                  24  FILLER      PIC X.                           EL320
01665                  24  AHED-CD-OT  PIC XX.                          EL320
01666      EJECT                                                        EL320
01667      12  PGMS-LINE-1.                                             EL320
01668          16  FILLER              PIC X(14)       VALUE            EL320
01669                  'RECORD TYPE - '.                                EL320
01670          16  PGMS-TYPE           PIC X(24)       VALUE            EL320
01671                  'SELECTED PROGRAM OPTIONS'.                      EL320
01672      12  PGMS-LINE-2.                                             EL320
01673          16  FILLER              PIC X(24)       VALUE            EL320
01674                  'PROGRAM   FREQUENCY   PR'.                      EL320
01675          16  FILLER              PIC X(23)       VALUE            EL320
01676                  'INT    FORMAT   PROCESS'.                       EL320
01677          16  FILLER              PIC X(12)       VALUE            EL320
01678                  '   TOTALLING'.                                  EL320
01679      12  PGMS-LINE-3.                                             EL320
01680          16  FILLER              PIC X(13)       VALUE SPACES.    EL320
01681          16  FILLER              PIC X(43)       VALUE            EL320
01682                  'CODE     OPTION   OPTION   OPTION    OPTION'.   EL320
01683      12  PGMS-DETAIL.                                             EL320
01684          16  PGMS-PROG           PIC X(5)        VALUE SPACES.    EL320
01685          16  FILLER              PIC X(8)        VALUE SPACES.    EL320
01686          16  PGMS-FREQ           PIC XXXX        VALUE SPACES.    EL320
01687          16  FILLER              PIC X(7)        VALUE SPACE.     EL320
01688          16  PGMS-PRNT           PIC X           VALUE SPACES.    EL320
01689          16  FILLER              PIC X(8)        VALUE SPACES.    EL320
01690          16  PGMS-FORMAT         PIC X           VALUE SPACES.    EL320
01691          16  FILLER              PIC X(8)        VALUE SPACES.    EL320
01692          16  PGMS-PROC           PIC X           VALUE SPACES.    EL320
01693          16  FILLER              PIC X(9)        VALUE SPACE.     EL320
01694          16  PGMS-TOT            PIC X           VALUE SPACES.    EL320
01695      EJECT                                                        EL320
01696      12  FORM-LINE-9.                                             EL320
01697          16  FILLER              PIC X(14)       VALUE            EL320
01698          'RECORD TYPE - '.                                        EL320
01699          16  FILLER              PIC X(17)       VALUE            EL320
01700          'FORM DESCRIPTIONS'.                                     EL320
01701      12  FORM-LINE-11.                                            EL320
01702          16  FILLER              PIC X(14)       VALUE            EL320
01703          'FORM NUMBER - '.                                        EL320
01704          16  FORM-NUMBER         PIC X(12)       VALUE SPACES.    EL320
01705      12  FORM-LINE-13.                                            EL320
01706          16  FILLER              PIC X(14)       VALUE            EL320
01707          '  LINE    TEXT'.                                        EL320
01708      12  FORM-DETAIL.                                             EL320
01709          16  FILLER              PIC XX          VALUE SPACES.    EL320
01710          16  FORM-LINE           PIC XXXX        VALUE SPACES.    EL320
01711          16  FILLER              PIC X(4)        VALUE SPACES.    EL320
01712          16  FORM-TEXT           PIC X(70)       VALUE SPACES.    EL320
01713      EJECT                                                        EL320
01714      12  LETR-LINE-9.                                             EL320
01715          16  FILLER              PIC X(14)       VALUE            EL320
01716                  'RECORD TYPE - '.                                EL320
01717          16  FILLER              PIC X(19)       VALUE            EL320
01718                  'LETTER DESCRIPTIONS'.                           EL320
01719      12  LETR-LINE-11.                                            EL320
01720          16  FILLER              PIC X(16)       VALUE            EL320
01721                  'LETTER NUMBER - '.                              EL320
01722          16  LETR-NUMBER         PIC X(4)        VALUE  SPACES.   EL320
01723          16  FILLER              PIC X(2)        VALUE  SPACES.   EL320
01724          16  LETR-CONTINUED      PIC X(14)       VALUE  SPACES.   EL320
01725      12  LETR-LINE-13.                                            EL320
01726          16  FILLER              PIC X(12)       VALUE            EL320
01727                  '  LINE  TEXT'.                                  EL320
01728          16  FILLER              PIC X(64)       VALUE SPACES.    EL320
01729          16  FILLER              PIC X(4)        VALUE 'SKIP'.    EL320
01730      12  LETR-DETAIL.                                             EL320
01731          16  FILLER              PIC XX          VALUE SPACES.    EL320
01732          16  LETR-LINE           PIC XXXX        VALUE SPACES.    EL320
01733          16  FILLER              PIC X(2)        VALUE SPACES.    EL320
01734          16  LETR-TEXT           PIC X(70)       VALUE SPACES.    EL320
01735          16  LETR-SKIP           PIC XX          VALUE SPACES.    EL320
01736      EJECT                                                        EL320
01737      12  TEXT-LINE-9.                                             EL320
01738          16  FILLER              PIC X(14)       VALUE            EL320
01739                  'RECORD TYPE - '.                                EL320
01740          16  WS-TEXT-DESC        PIC X(19)       VALUE SPACES.    EL320
01741      12  TEXT-LINE-11.                                            EL320
01742          16  WS-TEXT-TYPE-DESC   PIC X(16)       VALUE  SPACES.   EL320
01743          16  FILLER              PIC X(11)       VALUE            EL320
01744                  'PAGE NUMBER'.                                   EL320
01745      12  TEXT-DETAIL.                                             EL320
01746          16  FILLER              PIC XX          VALUE SPACES.    EL320
01747          16  WS-TEXT-NUMBER      PIC X(12)       VALUE SPACES.    EL320
01748          16  FILLER              PIC X(6)        VALUE SPACES.    EL320
01749          16  WS-TEXT-PAGE-NO     PIC ZZZ9        VALUE ZEROS.     EL320
01750          16  FILLER              PIC X(5)        VALUE SPACES.    EL320
01751                                                                   EL320
01752      12  TEXT-DETAIL1.                                            EL320
01753          16  FILLER              PIC X(2)        VALUE SPACES.    EL320
01754          16  WS-TEXT-DETAIL-MSG  PIC X(27)       VALUE            EL320
01755                  'TABLE EXCEEDED 500 ENTRIES'.                    EL320
01756      EJECT                                                        EL320
01757 *   ************************************************************* EL320
01758 *   *  THE PROCESSOR SECURITY ACCESS DESCRIPTION TABLES         * EL320
01759 *   *  FOLLOW.  THEY ARE ORGANIZED IN THE FOLLOWING MANNER:     * EL320
01760 *   *  1)  THE FIRST FOUR "SUB-TABLES" PROVIDE DESCRIPTIONS     * EL320
01761 *   *      FOR THE FIRST PROCESSOR CONTROL RECORD               * EL320
01762 *   *      (CF-SEQUENCE-NO = 0), WHERE THE FIRST SUB-TABLE IS   * EL320
01763 *   *      FOR CREDIT APPLICATIONS, THE SECOND SUB-TABLE IS     * EL320
01764 *   *      FOR CLAIMS APPLICATIONS, AND THE THIRD AND FOURTH    * EL320
01765 *   *      SUB-TABLES ARE FOR FUTURE DEFINITION.                * EL320
01766 *   *  2)  THE SECOND FOUR "SUB-TABLES" PROVIDE DESCRIPTIONS    * EL320
01767 *   *      FOR THE SECOND PROCESSOR CONTROL RECORD              * EL320
01768 *   *      (CF-SEQUENCE-NO = 1), WHERE THE FIRST SUB-TABLE IS   * EL320
01769 *   *      FOR MORTGAGE APPLICATIONS, AND THE SECOND, THIRD     * EL320
01770 *   *      AND FOURTH SUB-TABLES ARE FOR FUTURE DEFINITION.     * EL320
01771 *   *  3)  ACCESS TO THE TABLES IS THREE-DIMENSIONAL; I.E.,     * EL320
01772 *   *      THE FIRST SUBSCRIPT IS BY CF-SEQUENCE-NO (+1),       * EL320
01773 *   *      THE SECOND SUBSCRIPT CORRESPONDS TO THE FIRST        * EL320
01774 *   *      OCCURRENCE OF THE CF RECORD'S SECURITY TABLE, AND    * EL320
01775 *   *      THE THIRD SUBSCRIPT CORRESPONDS TO THE CF RECORD'S   * EL320
01776 *   *      SECURITY TABLE'S TABLE ELEMENTS.                     * EL320
01777 *   *  4)  IN ADDITION, WHEN THERE IS AN UNDEFINED ELEMENT IN   * EL320
01778 *   *      THE CF RECORD'S SECURITY TABLE, EITHER OF TWO        * EL320
01779 *   *      OPTIONS CAN BE SET IN THE FOLLOWING TABLES:          * EL320
01780 *   *      A)  BY PLACING A '**' IN THE "TBL-APPL-NBR" FIELD,   * EL320
01781 *   *          THE SECURITY DATA AND DESCRIPTION PRINTING IS    * EL320
01782 *   *          SUPPRESSED FOR THAT CODE, OR                     * EL320
01783 *   *      B)  BY PLACING A NUMBER IN THE "TBL-APPL-NBR" FIELD  * EL320
01784 *   *          AND PLACING SOMETHING TO THE EFFECT OF           * EL320
01785 *   *          'FUTURE USE' (OR WHATEVER ELSE) IN THE           * EL320
01786 *   *          "TBL-APPL-DESC" FIELD WILL CAUSE THAT LINE TO BE * EL320
01787 *   *          PRINTED.                                         * EL320
01788 *   *  5)  IF FUTURE CF PROCESSOR CONTROL RECORDS ARE ADDED,    * EL320
01789 *   *      ADDITIONAL TABLES SHOULD BE ADDED, FOUR SUB-TABLES   * EL320
01790 *   *      PER RECORD, AND SEQUENCE NUMBERS SHOULD BE           * EL320
01791 *   *      CONTIGUOUS, SINCE IS DRIVES THE FIRST SUBSCRIPT.     * EL320
01792 *   ************************************************************* EL320
01793      EJECT                                                        EL320
01794  01  PROC-DESCRIPTION-TABLE.                                      EL320
01795      12  PROC-CREDIT-DESCRIPTION-TABLE.                           EL320
01796        16  FILLER PIC X(43) VALUE                                 EL320
01797                    '01 REPORT CUSTOMIZATION MAINTENANCE        '. EL320
01798        16  FILLER PIC X(43) VALUE                                 EL320
01799                    '02 PROGRAM OPTION MAINTENANCE              '. EL320
01800        16  FILLER PIC X(43) VALUE                                 EL320
01801                    '03 TEXT FILE MAINTENANCE                   '. EL320
01802        16  FILLER PIC X(43) VALUE                                 EL320
01803                    '04 ACCOUNT MASTER MAINTENANCE              '. EL320
01804        16  FILLER PIC X(43) VALUE                                 EL320
01805                    '05 COMPENSATION MASTER MAINTENANCE         '. EL320
01806        16  FILLER PIC X(43) VALUE                                 EL320
01807                    '06 RATE MASTER MAINTENANCE                 '. EL320
01808        16  FILLER PIC X(43) VALUE                                 EL320
01809                    '07 REINSURANCE MASTER MAINTENANCE          '. EL320
01810        16  FILLER PIC X(43) VALUE                                 EL320
01811                    '08 COMMISSION TABLE MAINTENANCE            '. EL320
01812        16  FILLER PIC X(43) VALUE                                 EL320
01813                    '09 FUTURE USE                              '. EL320
01814        16  FILLER PIC X(43) VALUE                                 EL320
01815                    '10 LOAN OFFICER MAINTENANCE                '. EL320
01816        16  FILLER PIC X(43) VALUE                                 EL320
01817                    '11 NEW BUSINESS - DATA ENTRY               '. EL320
01818        16  FILLER PIC X(43) VALUE                                 EL320
01819                    '12 NEW BUSINESS - REVIEW AND CORRECTION    '. EL320
01820        16  FILLER PIC X(43) VALUE                                 EL320
01821                    '13 NEW BUSINESS FULL FILE EDIT             '. EL320
01822        16  FILLER PIC X(43) VALUE                                 EL320
01823                    '14 CLAIMS AND RESERVES                     '. EL320
01824        16  FILLER PIC X(43) VALUE                                 EL320
01825                    '15 COMPENSATIONS PAYMENT/ADJUSTMENTS       '. EL320
01826        16  FILLER PIC X(43) VALUE                                 EL320
01827                    '16 RETRO/REIN PAYMENT/ADJUSTMENTS          '. EL320
01828        16  FILLER PIC X(43) VALUE                                 EL320
01829                    '17 CHECK MAINTENANCE - REFUND & COMMISSION '. EL320
01830        16  FILLER PIC X(43) VALUE                                 EL320
01831                    '18 ACCOUNT STATEMENTS                      '. EL320
01832        16  FILLER PIC X(43) VALUE                                 EL320
01833                    '19 GENERAL AGENT STATEMENTS                '. EL320
01834        16  FILLER PIC X(43) VALUE                                 EL320
01835                    '20 FUTURE USE                              '. EL320
01836        16  FILLER PIC X(43) VALUE                                 EL320
01837                    '21 BILLING STATEMENT PRINTING              '. EL320
01838        16  FILLER PIC X(43) VALUE                                 EL320
01839                    '22 CHECKS TO BE PRINTED LOOK-UP AND REPORT '. EL320
01840        16  FILLER PIC X(43) VALUE                                 EL320
01841                    '23 CHECK RELEASE                           '. EL320
01842        16  FILLER PIC X(43) VALUE                                 EL320
01843                    '24 PRINT COMMISSION AND REFUND CHECKS      '. EL320
01844        16  FILLER PIC X(43) VALUE                                 EL320
01845                    '25 FUTURE USE                              '. EL320
01846        16  FILLER PIC X(43) VALUE                                 EL320
01847                    '26 FUTURE USE                              '. EL320
01848        16  FILLER PIC X(43) VALUE                                 EL320
01849                    '27 FUTURE USE                              '. EL320
01850        16  FILLER PIC X(43) VALUE                                 EL320
01851                    '28 FUTURE USE                              '. EL320
01852        16  FILLER PIC X(43) VALUE                                 EL320
01853                    '29 FUTURE USE                              '. EL320
01854        16  FILLER PIC X(43) VALUE                                 EL320
01855                    '30 FUTURE USE                              '. EL320
01856        16  FILLER PIC X(43) VALUE                                 EL320
01857                    '31 CERTIFICATE LOOK UP                     '. EL320
01858        16  FILLER PIC X(43) VALUE                                 EL320
01859                    '32 CERTIFICATE NOTES                       '. EL320
01860        16  FILLER PIC X(43) VALUE                                 EL320
01861                    '33 CERTIFICATE CHANGES                     '. EL320
01862        16  FILLER PIC X(43) VALUE                                 EL320
01863                    '** FUTURE USE                              '. EL320
01864        16  FILLER PIC X(43) VALUE                                 EL320
01865                    '** FUTURE USE                              '. EL320
01866        16  FILLER PIC X(43) VALUE                                 EL320
01867                    '** FUTURE USE                              '. EL320
01868        16  FILLER PIC X(43) VALUE                                 EL320
01869                    '** FUTURE USE                              '. EL320
01870        16  FILLER PIC X(43) VALUE                                 EL320
01871                    '** FUTURE USE                              '. EL320
01872        16  FILLER PIC X(43) VALUE                                 EL320
01873                    '** FUTURE USE                              '. EL320
01874        16  FILLER PIC X(43) VALUE                                 EL320
01875                    '** FUTURE USE                              '. EL320
01876        16  FILLER PIC X(43) VALUE                                 EL320
01877                    '**                                         '. EL320
01878        16  FILLER PIC X(43) VALUE                                 EL320
01879                    '**                                         '. EL320
01880        16  FILLER PIC X(43) VALUE                                 EL320
01881                    '**                                         '. EL320
01882        16  FILLER PIC X(43) VALUE                                 EL320
01883                    '**                                         '. EL320
01884  EJECT                                                            EL320
01885      12  PROC-CLAIMS-DESCRIPTION-TABLE.                           EL320
01886        16  FILLER PIC X(43) VALUE                                 EL320
01887                    '41 NEW CLAIM SETUP                         '. EL320
01888        16  FILLER PIC X(43) VALUE                                 EL320
01889                    '42 RECORD MAIL RECEIVED                    '. EL320
01890        16  FILLER PIC X(43) VALUE                                 EL320
01891                    '43 CLAIM AUDIT                             '. EL320
01892        16  FILLER PIC X(43) VALUE                                 EL320
01893                    '44 BENEFICIARY MASTER MAINTENANCE          '. EL320
01894        16  FILLER PIC X(43) VALUE                                 EL320
01895                    '45 CLAIM MAINTENANCE                       '. EL320
01896        16  FILLER PIC X(43) VALUE                                 EL320
01897                    '46 DENIAL PROCESSING                       '. EL320
01898        16  FILLER PIC X(43) VALUE                                 EL320
01899                    '47 CLAIMS LETTER WRITER                    '. EL320
01900        16  FILLER PIC X(43) VALUE                                 EL320
01901                    '48 NOTE AND REMINDER RECORDING             '. EL320
01902        16  FILLER PIC X(43) VALUE                                 EL320
01903                    '49 SET-UP AUTOMATIC PAYMENT                '. EL320
01904        16  FILLER PIC X(43) VALUE                                 EL320
01905                    '50 PAYMENT PROCESSING                      '. EL320
01906        16  FILLER PIC X(43) VALUE                                 EL320
01907                    '51 CHECKS TO BE PRINTED LOOKUP AND REPORT  '. EL320
01908        16  FILLER PIC X(43) VALUE                                 EL320
01909                    '52 RELEASE CHECKS TO BE PRINTED            '. EL320
01910        16  FILLER PIC X(43) VALUE                                 EL320
01911                    '53 PRINT CLAIMS CHECKS                     '. EL320
01912        16  FILLER PIC X(43) VALUE                                 EL320
01913                    '54 CLAIM ADDRESS MAINTENANCE               '. EL320
01914        16  FILLER PIC X(43) VALUE                                 EL320
01915                    '55 TRAILER DISPLAY AND MAINTENANCE         '. EL320
01916        16  FILLER PIC X(43) VALUE                                 EL320
01917                    '56 CLAIM STATUS DISPLAY AND DISPOSITION    '. EL320
01918        16  FILLER PIC X(43) VALUE                                 EL320
01919                    '57 SUPERVISOR REQUEST REPORT               '. EL320
01920        16  FILLER PIC X(43) VALUE                                 EL320
01921                    '58 FILE FOLDER LABEL PRINT                 '. EL320
01922        16  FILLER PIC X(43) VALUE                                 EL320
01923                    '59 CLAIMS STATUS PRINT                     '. EL320
01924        16  FILLER PIC X(43) VALUE                                 EL320
01925                    '60 LETTER AND ADDRESS LABEL PRINT          '. EL320
01926        16  FILLER PIC X(43) VALUE                                 EL320
01927                    '61 CLAIM LOOK-UP                           '. EL320
01928        16  FILLER PIC X(43) VALUE                                 EL320
01929                    '62 FOR FUTURE USE                          '. EL320
01930        16  FILLER PIC X(43) VALUE                                 EL320
01931                    '**                                         '. EL320
01932        16  FILLER PIC X(43) VALUE                                 EL320
01933                    '**                                         '. EL320
01934        16  FILLER PIC X(43) VALUE                                 EL320
01935                    '**                                         '. EL320
01936        16  FILLER PIC X(43) VALUE                                 EL320
01937                    '**                                         '. EL320
01938        16  FILLER PIC X(43) VALUE                                 EL320
01939                    '**                                         '. EL320
01940        16  FILLER PIC X(43) VALUE                                 EL320
01941                    '**                                         '. EL320
01942        16  FILLER PIC X(43) VALUE                                 EL320
01943                    '**                                         '. EL320
01944        16  FILLER PIC X(43) VALUE                                 EL320
01945                    '**                                         '. EL320
01946        16  FILLER PIC X(43) VALUE                                 EL320
01947                    '**                                         '. EL320
01948        16  FILLER PIC X(43) VALUE                                 EL320
01949                    '**                                         '. EL320
01950        16  FILLER PIC X(43) VALUE                                 EL320
01951                    '**                                         '. EL320
01952        16  FILLER PIC X(43) VALUE                                 EL320
01953                    '**                                         '. EL320
01954        16  FILLER PIC X(43) VALUE                                 EL320
01955                    '**                                         '. EL320
01956        16  FILLER PIC X(43) VALUE                                 EL320
01957                    '**                                         '. EL320
01958        16  FILLER PIC X(43) VALUE                                 EL320
01959                    '**                                         '. EL320
01960        16  FILLER PIC X(43) VALUE                                 EL320
01961                    '**                                         '. EL320
01962        16  FILLER PIC X(43) VALUE                                 EL320
01963                    '**                                         '. EL320
01964        16  FILLER PIC X(43) VALUE                                 EL320
01965                    '**                                         '. EL320
01966        16  FILLER PIC X(43) VALUE                                 EL320
01967                    '**                                         '. EL320
01968        16  FILLER PIC X(43) VALUE                                 EL320
01969                    '**                                         '. EL320
01970        16  FILLER PIC X(43) VALUE                                 EL320
01971                    '**                                         '. EL320
01972        16  FILLER PIC X(43) VALUE                                 EL320
01973                    '**                                         '. EL320
01974  EJECT                                                            EL320
01975      12  PROC-CREDIT-CARD-DESC-TABLE.                             EL320
01976        16  FILLER  PIC X(43) VALUE '**'.                          EL320
01977        16  FILLER  PIC X(43) VALUE '**'.                          EL320
01978        16  FILLER  PIC X(43) VALUE '**'.                          EL320
01979        16  FILLER  PIC X(43) VALUE '**'.                          EL320
01980        16  FILLER  PIC X(43) VALUE '**'.                          EL320
01981        16  FILLER  PIC X(43) VALUE '**'.                          EL320
01982        16  FILLER  PIC X(43) VALUE '**'.                          EL320
01983        16  FILLER  PIC X(43) VALUE '**'.                          EL320
01984        16  FILLER  PIC X(43) VALUE '**'.                          EL320
01985        16  FILLER  PIC X(43) VALUE '**'.                          EL320
01986        16  FILLER  PIC X(43) VALUE '**'.                          EL320
01987        16  FILLER  PIC X(43) VALUE '**'.                          EL320
01988        16  FILLER  PIC X(43) VALUE '**'.                          EL320
01989        16  FILLER  PIC X(43) VALUE '**'.                          EL320
01990        16  FILLER  PIC X(43) VALUE '**'.                          EL320
01991        16  FILLER  PIC X(43) VALUE '**'.                          EL320
01992        16  FILLER  PIC X(43) VALUE '**'.                          EL320
01993        16  FILLER  PIC X(43) VALUE '**'.                          EL320
01994        16  FILLER  PIC X(43) VALUE '**'.                          EL320
01995        16  FILLER  PIC X(43) VALUE '**'.                          EL320
01996        16  FILLER  PIC X(43) VALUE '**'.                          EL320
01997        16  FILLER  PIC X(43) VALUE '**'.                          EL320
01998        16  FILLER  PIC X(43) VALUE '**'.                          EL320
01999        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02000        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02001        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02002        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02003        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02004        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02005        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02006        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02007        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02008        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02009        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02010        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02011        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02012        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02013        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02014        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02015        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02016        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02017        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02018        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02019        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02020  EJECT                                                            EL320
02021      12  PROC-ACCTS-RECV-DESC-TABLE.                              EL320
02022        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02023        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02024        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02025        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02026        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02027        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02028        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02029        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02030        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02031        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02032        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02033        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02034        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02035        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02036        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02037        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02038        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02039        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02040        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02041        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02042        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02043        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02044        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02045        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02046        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02047        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02048        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02049        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02050        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02051        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02052        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02053        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02054        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02055        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02056        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02057        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02058        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02059        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02060        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02061        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02062        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02063        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02064        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02065        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02066  EJECT                                                            EL320
02067      12  PROC-MORTGAGE-DESC-TABLE.                                EL320
02068        16  FILLER PIC X(43) VALUE                                 EL320
02069                    '01 MORTGAGE CODES MAINTENANCE              '. EL320
02070        16  FILLER PIC X(43) VALUE                                 EL320
02071                    '02 PRODUCER MASTER MAINTENANCE             '. EL320
02072        16  FILLER PIC X(43) VALUE                                 EL320
02073                    '03 LETTER ARCHIVE                          '. EL320
02074        16  FILLER PIC X(43) VALUE                                 EL320
02075                    '04 SOLICITATION EVALUATION                 '. EL320
02076        16  FILLER PIC X(43) VALUE                                 EL320
02077                    '05 POLICY MASTER LOOKUP                    '. EL320
02078        16  FILLER PIC X(43) VALUE                                 EL320
02079                    '06 LETTER PRINTER                          '. EL320
02080        16  FILLER PIC X(43) VALUE                                 EL320
02081                    '07 TEXT FILE UPDATE                        '. EL320
02082        16  FILLER PIC X(43) VALUE                                 EL320
02083                    '08 PRODUCER PLANS                          '. EL320
02084        16  FILLER PIC X(43) VALUE                                 EL320
02085                    '09 DATA ENTRY                              '. EL320
02086        16  FILLER PIC X(43) VALUE                                 EL320
02087                    '10 DATA REVIEW AND CORRECTION              '. EL320
02088        16  FILLER PIC X(43) VALUE                                 EL320
02089                    '11 GROUP INV MATCHLIST                     '. EL320
02090        16  FILLER PIC X(43) VALUE                                 EL320
02091                    '12 RATE UPDATE                             '. EL320
02092        16  FILLER PIC X(43) VALUE                                 EL320
02093                    '13 SOLICITATION ENTRY                      '. EL320
02094        16  FILLER PIC X(43) VALUE                                 EL320
02095                    '14 UNDERWRITERS                            '. EL320
02096        16  FILLER PIC X(43) VALUE                                 EL320
02097                    '15 INDIVIDUAL PAYMENT                      '. EL320
02098        16  FILLER PIC X(43) VALUE                                 EL320
02099                    '16 POLICY INV MATCHLIST                    '. EL320
02100        16  FILLER PIC X(43) VALUE                                 EL320
02101                    '17 PAYMENT REVERSAL                        '. EL320
02102        16  FILLER PIC X(43) VALUE                                 EL320
02103                    '18 PREMIUM QUOTE                           '. EL320
02104        16  FILLER PIC X(43) VALUE                                 EL320
02105                    '19 CLAIMS AND RES                          '. EL320
02106        16  FILLER PIC X(43) VALUE                                 EL320
02107                    '20 M I B  SEARCH                           '. EL320
02108        16  FILLER PIC X(43) VALUE                                 EL320
02109                    '21 PLAN SUMMARY REPORT                     '. EL320
02110        16  FILLER PIC X(43) VALUE                                 EL320
02111                    '22 PROD SUMMARY REPORT                     '. EL320
02112        16  FILLER PIC X(43) VALUE                                 EL320
02113                    '23 COMP MASTER SUMMARY                     '. EL320
02114        16  FILLER PIC X(43) VALUE                                 EL320
02115                    '24 PAYMENT AND ADJUSTMENT REPORT           '. EL320
02116        16  FILLER PIC X(43) VALUE                                 EL320
02117                    '25 UNDERWRITER STATUS                      '. EL320
02118        16  FILLER PIC X(43) VALUE                                 EL320
02119                    '26 UNDERWRITER WORKSHEET                   '. EL320
02120        16  FILLER PIC X(43) VALUE                                 EL320
02121                    '27 APP FILE SUMMARY                        '. EL320
02122        16  FILLER PIC X(43) VALUE                                 EL320
02123                    '28 POLICY HOLDER - GENERAL                 '. EL320
02124        16  FILLER PIC X(43) VALUE                                 EL320
02125                    '29 POLICY HOLDER - CLAIMS                  '. EL320
02126        16  FILLER PIC X(43) VALUE                                 EL320
02127                    '30 POLICY HOLDER - BILLING                 '. EL320
02128        16  FILLER PIC X(43) VALUE                                 EL320
02129                    '31 ISSUE POLICY                            '. EL320
02130        16  FILLER PIC X(43) VALUE                                 EL320
02131                    '32 POLICY STATUS REPORT                    '. EL320
02132        16  FILLER PIC X(43) VALUE                                 EL320
02133                    '33 RETRO PAYMENTS / ADJUSTMENTS            '. EL320
02134        16  FILLER PIC X(43) VALUE                                 EL320
02135                    '34 GROUP PREMIUM PAYMENT POST              '. EL320
02136        16  FILLER PIC X(43) VALUE                                 EL320
02137                    '35 COMP MASTER UPDATE                      '. EL320
02138        16  FILLER PIC X(43) VALUE                                 EL320
02139                    '36 ONLINE REPORTS                          '. EL320
02140        16  FILLER PIC X(43) VALUE                                 EL320
02141                    '37 ACCOUNT RECEIVABLE                      '. EL320
02142        16  FILLER PIC X(43) VALUE                                 EL320
02143                    '38 COMP PAYMENTS / ADJUSTMENTS             '. EL320
02144        16  FILLER PIC X(43) VALUE                                 EL320
02145                    '39 BANK MASTER UPDATE                      '. EL320
02146        16  FILLER PIC X(43) VALUE                                 EL320
02147                    '40 LOAN OFFICER UPDATE                     '. EL320
02148        16  FILLER PIC X(43) VALUE                                 EL320
02149                    '41 REINSTATE CANCELS                       '. EL320
02150        16  FILLER PIC X(43) VALUE                                 EL320
02151                    '42 CHECKS                                  '. EL320
02152        16  FILLER PIC X(43) VALUE                                 EL320
02153                    '43 FUTURE USE                              '. EL320
02154        16  FILLER PIC X(43) VALUE                                 EL320
02155                    '44 FUTURE USE                              '. EL320
02156  EJECT                                                            EL320
02157      12  FUTURE-2-TABLE.                                          EL320
02158        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02159        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02160        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02161        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02162        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02163        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02164        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02165        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02166        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02167        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02168        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02169        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02170        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02171        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02172        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02173        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02174        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02175        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02176        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02177        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02178        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02179        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02180        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02181        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02182        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02183        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02184        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02185        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02186        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02187        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02188        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02189        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02190        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02191        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02192        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02193        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02194        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02195        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02196        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02197        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02198        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02199        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02200        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02201        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02202  EJECT                                                            EL320
02203      12  FUTURE-3-TABLE.                                          EL320
02204        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02205        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02206        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02207        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02208        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02209        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02210        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02211        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02212        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02213        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02214        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02215        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02216        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02217        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02218        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02219        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02220        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02221        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02222        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02223        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02224        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02225        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02226        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02227        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02228        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02229        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02230        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02231        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02232        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02233        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02234        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02235        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02236        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02237        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02238        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02239        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02240        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02241        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02242        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02243        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02244        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02245        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02246        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02247        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02248  EJECT                                                            EL320
02249      12  FUTURE-4-TABLE.                                          EL320
02250        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02251        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02252        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02253        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02254        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02255        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02256        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02257        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02258        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02259        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02260        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02261        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02262        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02263        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02264        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02265        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02266        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02267        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02268        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02269        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02270        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02271        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02272        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02273        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02274        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02275        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02276        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02277        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02278        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02279        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02280        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02281        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02282        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02283        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02284        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02285        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02286        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02287        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02288        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02289        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02290        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02291        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02292        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02293        16  FILLER  PIC X(43) VALUE '**'.                          EL320
02294  EJECT                                                            EL320
02295   01  FILLER REDEFINES PROC-DESCRIPTION-TABLE.                    EL320
02296       12  FILLER       OCCURS 2 TIMES.                            EL320
02297         16  FILLER       OCCURS 4 TIMES.                          EL320
02298           20  FILLER       OCCURS 44 TIMES.                       EL320
02299             24  TBL-APPL-NBR        PIC X(02).                    EL320
02300             24  FILLER              PIC X(01).                    EL320
02301             24  TBL-APPL-DESC       PIC X(40).                    EL320
02302      EJECT                                                        EL320
02303                              COPY ELCDATE.                        EL320
02304      EJECT                                                        EL320
02305                              COPY ELCDTECX.                       EL320
02306      EJECT                                                           CL**2
02307                              COPY ELCDTEVR.                          CL**2
02308      EJECT                                                        EL320
02309  PROCEDURE DIVISION.                                              EL320
02310                                                                   EL320
02311  0000-LOAD-DATE-CARD.                                             EL320
02312                              COPY ELCDTERX.                       EL320
02313                                                                   EL320
02314      IF DTE-PRC-OPT  NOT EQUAL  2                                 EL320
02315          MOVE 1                  TO DTE-PRC-OPT.                  EL320
02316                                                                   EL320
02317  0100-INITIALIZE.                                                 EL320
02318      OPEN INPUT   ELCNTLF                                         EL320
02319                   ELFORMF                                         EL320
02320                   ELLETRF                                         EL320
02321                   ELPGMSF                                         EL320
02322           OUTPUT  PRINTF.                                         EL320
02323                                                                   EL320
02324      IF ELCNTL-STATUS  EQUAL  '00'  OR  '97'                      EL320
02325          NEXT SENTENCE                                            EL320
02326      ELSE                                                         EL320
02327          MOVE 'ERROR OCCURED OPEN - ELCNTL'                       EL320
02328                                  TO  WS-ABEND-MESSAGE             EL320
02329          MOVE ELCNTL-STATUS      TO  WS-ABEND-FILE-STATUS         EL320
02330          PERFORM  ABEND-PGM.                                      EL320
02331                                                                   EL320
02332      IF ELFORM-STATUS  EQUAL  '00'  OR  '97'                      EL320
02333          NEXT SENTENCE                                            EL320
02334      ELSE                                                         EL320
02335          MOVE 'ERROR OCCURED OPEN - ELFORM'                       EL320
02336                                  TO  WS-ABEND-MESSAGE             EL320
02337          MOVE ELFORM-STATUS      TO  WS-ABEND-FILE-STATUS         EL320
02338          PERFORM  ABEND-PGM.                                      EL320
02339                                                                   EL320
02340      IF ELLETR-STATUS  EQUAL  '00'  OR  '97'                      EL320
02341          NEXT SENTENCE                                            EL320
02342      ELSE                                                         EL320
02343          MOVE 'ERROR OCCURED OPEN - ELLETR'                       EL320
02344                                  TO  WS-ABEND-MESSAGE             EL320
02345          MOVE ELLETR-STATUS      TO  WS-ABEND-FILE-STATUS         EL320
02346          PERFORM  ABEND-PGM.                                      EL320
02347                                                                   EL320
02348      IF ELPGMS-STATUS  EQUAL  '00'  OR  '97'                      EL320
02349          NEXT SENTENCE                                            EL320
02350      ELSE                                                         EL320
02351          MOVE 'ERROR OCCURED OPEN - ELPGMS'                       EL320
02352                                  TO  WS-ABEND-MESSAGE             EL320
02353          MOVE ELPGMS-STATUS      TO  WS-ABEND-FILE-STATUS         EL320
02354          PERFORM  ABEND-PGM.                                      EL320
02355                                                                   EL320
02356      ACCEPT WS-TIME-OF-DAY       FROM  TIME.                      EL320
02357                                                                   EL320
02358      ACCEPT WS-ACCEPT-DATE       FROM  DATE.                      EL320
02359                                                                   EL320
02360      MOVE WS-AD-YY               TO  WS-CD-YY.                    EL320
02361      MOVE WS-AD-MM               TO  WS-CD-MM.                    EL320
02362      MOVE WS-AD-DD               TO  WS-CD-DD.                    EL320
02363      MOVE WS-CURRENT-DATE        TO  HEAD-RUN-DATE.               EL320
02364      MOVE WS-CD-MM               TO  WS-CURR-MO.                  EL320
02365      MOVE WS-CD-DD               TO  WS-CURR-DY.                  EL320
02366      MOVE WS-CD-YY               TO  WS-CURR-YR.                  EL320
02367      MOVE ALPH-DATE              TO  HEAD-RUN-DATE-FULL.          EL320
02368      MOVE COMPANY-NAME           TO  HEAD-CLIENT.                 EL320
02369      MOVE LOW-VALUES             TO  CF-CONTROL-PRIMARY.          EL320
02370      MOVE DTE-CLIENT             TO  CF-COMPANY-ID.               EL320
02371                                                                   EL320
02372      START  ELCNTLF                                               EL320
02373          KEY  NOT LESS THAN  CF-CONTROL-PRIMARY.                  EL320
02374                                                                   EL320
02375      IF ELCNTL-STAT-1  NOT EQUAL  '0'                             EL320
02376          DISPLAY 'NO COMPANY RECORD----'                          EL320
02377          DISPLAY CF-CONTROL-PRIMARY                               EL320
02378          GO TO 9999-FINALIZE.                                     EL320
02379                                                                   EL320
02380  1000-CONTROL-FILE-DOC.                                           EL320
02381                                                                   EL320
02382      PERFORM  2200-PROCESS-CNTL-RECORDS  THRU  2200-EXIT          EL320
02383          UNTIL  END-OF-CNTL-FILE.                                 EL320
02384                                                                   EL320
02385      MOVE SAVE-AH-EDIT-MASTER    TO  CONTROL-FILE.                EL320
02386                                                                   EL320
02387      PERFORM  2221-PROCESS-AHED-RECORD  THRU  2221-EXIT.          EL320
02388                                                                   EL320
02389      MOVE SAVE-LIFE-EDIT-MASTER  TO  CONTROL-FILE.                EL320
02390                                                                   EL320
02391      PERFORM  2220-PROCESS-LFED-RECORD  THRU  2220-EXIT.          EL320
02392                                                                   EL320
02393      MOVE 99                     TO  WS-LINE-CNT.                 EL320
02394                                                                   EL320
02395      PERFORM  3000-PROCESS-PGMS-FILE  THRU  3000-EXIT             EL320
02396          UNTIL  END-OF-PGMS-FILE.                                 EL320
02397                                                                   EL320
02398      MOVE 99                     TO  WS-LINE-CNT.                 EL320
02399                                                                   EL320
02400      PERFORM  4000-PROCESS-TEXT-FILE  THRU  4000-EXIT             EL320
02401          UNTIL  END-OF-TEXT-FILE.                                 EL320
02402                                                                   EL320
02403      MOVE 99                     TO  WS-LINE-CNT.                 EL320
02404                                                                   EL320
02405      PERFORM  4500-PROCESS-LETR-FILE  THRU  4500-EXIT             EL320
02406          UNTIL  END-OF-LETR-FILE.                                 EL320
02407                                                                   EL320
02408      MOVE ALL '*'                TO  P-DATA.                      EL320
02409      MOVE TP                     TO  X.                           EL320
02410                                                                   EL320
02411      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
02412                                                                   EL320
02413      GO TO 9999-FINALIZE.                                         EL320
02414      EJECT                                                        EL320
02415  2200-PROCESS-CNTL-RECORDS.                                       EL320
02416                                                                   EL320
02417      READ  ELCNTLF  NEXT RECORD.                                  EL320
02418                                                                   EL320
02419      IF ELCNTL-STAT-1  EQUAL  '1'                                 EL320
02420          MOVE 'E'                TO  WS-ELCNTL-EOF-SW             EL320
02421          GO TO 2200-EXIT.                                         EL320
02422                                                                   EL320
02423      IF ELCNTL-STAT-1  NOT EQUAL  '0'                             EL320
02424          MOVE 'ERROR OCCURED READ - ELCNTL'                       EL320
02425                                  TO  WS-ABEND-MESSAGE             EL320
02426          MOVE ELCNTL-STATUS      TO  WS-ABEND-FILE-STATUS         EL320
02427          PERFORM  ABEND-PGM.                                      EL320
02428                                                                   EL320
02429      IF CF-COMPANY-ID  NOT EQUAL  DTE-CLIENT                      EL320
02430          MOVE 'E'                TO  WS-ELCNTL-EOF-SW             EL320
02431          GO TO 2200-EXIT.                                         EL320
02432                                                                   EL320
02433      IF CF-COMPANY-MASTER                                         EL320
02434          PERFORM  2211-PROCESS-COMP-RECORD  THRU  2211-EXIT       EL320
02435      ELSE                                                         EL320
02436      IF CF-PROCESSOR-MASTER                                       EL320
02437          PERFORM  2212-PROCESS-PROC-RECORD  THRU  2212-EXIT       EL320
02438      ELSE                                                         EL320
02439      IF CF-STATE-MASTER                                           EL320
02440          PERFORM  2216-PROCESS-STAT-RECORD  THRU  2216-EXIT       EL320
02441      ELSE                                                         EL320
02442      IF CF-LF-BENEFIT-MASTER                                      EL320
02443          PERFORM  2214-PROCESS-LF-BNFT-RECORD  THRU  2214-EXIT    EL320
02444      ELSE                                                         EL320
02445      IF CF-AH-BENEFIT-MASTER                                      EL320
02446          PERFORM  2215-PROCESS-AH-BNFT-RECORD  THRU  2215-EXIT    EL320
02447      ELSE                                                         EL320
02448      IF CF-CARRIER-MASTER                                         EL320
02449          PERFORM  2213-PROCESS-CARR-RECORD  THRU  2213-EXIT       EL320
02450      ELSE                                                         EL320
02451      IF CF-MORTALITY-MASTER                                       EL320
02452          PERFORM  2217-PROCESS-MORT-RECORD  THRU  2217-EXIT       EL320
02453      ELSE                                                         EL320
02454      IF CF-BUSINESS-TYPE-MASTER                                   EL320
02455          PERFORM  2218-PROCESS-BUSS-RECORD  THRU  2218-EXIT       EL320
02456      ELSE                                                         EL320
02457      IF CF-TERMINAL-MASTER                                        EL320
02458          PERFORM  2219-PROCESS-TERM-RECORD  THRU  2219-EXIT       EL320
02459      ELSE                                                         EL320
02460      IF CF-LIFE-EDIT-MASTER                                       EL320
02461          MOVE CONTROL-FILE       TO  SAVE-LIFE-EDIT-MASTER        EL320
02462      ELSE                                                         EL320
02463      IF CF-AH-EDIT-MASTER                                         EL320
02464          MOVE CONTROL-FILE       TO  SAVE-AH-EDIT-MASTER.         EL320
02465                                                                   EL320
02466  2200-EXIT.                                                       EL320
02467      EXIT.                                                        EL320
02468                                                                   EL320
02469  2211-PROCESS-COMP-RECORD.                                        EL320
02470      PERFORM  1200-HEADING  THRU  1200-EXIT.                      EL320
02471                                                                   EL320
02472      MOVE CF-RECORD-TYPE         TO  WS-RECORD-TYPE.              EL320
02473      MOVE CF-LAST-MAINT-BY       TO  COMP-MAINT-BY-A              EL320
02474                                      COMP-MAINT-BY-B              EL320
02475                                      COMP-MAINT-BY-C.             EL320
02476      MOVE CF-LAST-MAINT-DT       TO  DC-BIN-DATE-1.               EL320
02477      MOVE ' '                    TO  DC-OPTION-CODE.              EL320
02478                                                                   EL320
02479      PERFORM  1100-DATE-RTN  THRU  1100-EXIT.                     EL320
02480                                                                   EL320
02481      MOVE DC-GREG-DATE-1-EDIT    TO  COMP-MAINT-DT-A              EL320
02482                                      COMP-MAINT-DT-B              EL320
02483                                      COMP-MAINT-DT-C.             EL320
02484      MOVE CF-LAST-MAINT-HHMMSS   TO  WS-MAINT-TIME.               EL320
02485      MOVE WS-MAINT-HH            TO  COMP-MAINT-HHA               EL320
02486                                      COMP-MAINT-HHB               EL320
02487                                      COMP-MAINT-HHC.              EL320
02488      MOVE WS-MAINT-MM            TO  COMP-MAINT-MMA               EL320
02489                                      COMP-MAINT-MMB               EL320
02490                                      COMP-MAINT-MMC.              EL320
02491      MOVE WS-MAINT-SS            TO  COMP-MAINT-SSA               EL320
02492                                      COMP-MAINT-SSB               EL320
02493                                      COMP-MAINT-SSC.              EL320
02494      MOVE CF-CL-MAIL-TO-NAME     TO  COMP-NAME.                   EL320
02495      MOVE CF-CL-IN-CARE-OF       TO  COMP-IN-CARE.                EL320
02496      MOVE CF-CL-ADDR-LINE-1      TO  COMP-ADDRESS-1.              EL320
02497      MOVE CF-CL-ADDR-LINE-2      TO  COMP-ADDRESS-2.              EL320
02498      MOVE CF-CL-CITY-STATE       TO  COMP-CITY-STAT.              EL320
02499      MOVE CF-TAX-ID-NUMBER       TO  COMP-TAX-ID.                 EL320
02500                                                                   EL320
02501      IF CF-CL-ZIP-CODE-NUM NOT NUMERIC                            EL320
02502          MOVE ZEROS              TO CF-CL-ZIP-CODE-NUM.           EL320
02503      IF CF-CL-ZIP-CODE-NUM NOT = ZEROS                            EL320
02504          MOVE CF-CL-ZIP-CODE-NUM TO WS-ZIP-CODE                   EL320
02505          MOVE WS-ZIP-CODE-X      TO CF-CL-ZIP-CODE.               EL320
02506                                                                   EL320
02507      MOVE CF-CL-ZIP-CODE         TO  COMP-ZIP-CODE.               EL320
02508      MOVE CF-CL-PHONE-NO         TO  WS-PHONE.                    EL320
02509      MOVE WS-PHONE-AC            TO  WS-ED-AC.                    EL320
02510      MOVE WS-PHONE-PF            TO  WS-ED-PF.                    EL320
02511      MOVE WS-PHONE-NO            TO  WS-ED-NO.                    EL320
02512      MOVE WS-PHON-EDIT           TO  COMP-PHONE.                  EL320
02513      MOVE CF-COMPANY-ID          TO  COMP-ID.                     EL320
02514      MOVE DTE-CLASIC-COMPANY-NUMBER                               EL320
02515                                  TO  COMP-CODE.                   EL320
02516      MOVE CF-SECURITY-OPTION     TO  COMP-OPT-A10.                EL320
02517      MOVE SPACES                 TO  COMP-DESC-A10                EL320
02518                                                                   EL320
02519      IF ALL-SECURITY                                              EL320
02520          MOVE 'ALL SECURITY        '  TO  COMP-DESC-A10.          EL320
02521                                                                   EL320
02522      IF COMPANY-VERIFY                                            EL320
02523          MOVE 'COMPANY VERIFY      '  TO  COMP-DESC-A10.          EL320
02524                                                                   EL320
02525      IF PROCESSOR-VERIFY                                          EL320
02526          MOVE 'PROCESSOR VERIFY    '  TO  COMP-DESC-A10.          EL320
02527                                                                   EL320
02528      IF NO-SECURITY                                               EL320
02529          MOVE 'NO SECURITY         '  TO  COMP-DESC-A10.          EL320
02530                                                                   EL320
02531      IF ALL-BUT-TERM                                              EL320
02532          MOVE 'ALL BUT TERMINAL    '  TO  COMP-DESC-A10.          EL320
02533                                                                   EL320
02534      MOVE CF-LGX-INTERFACE-CNTL  TO  COMP-OPT-A11.                EL320
02535      MOVE WS-INVALID-SETTING     TO  COMP-DESC-A11.               EL320
02536                                                                   EL320
02537      IF LGX-TIME-SHR-COMPANY                                      EL320
02538          MOVE 'LOGIC TIME SHARE      '  TO  COMP-DESC-A11         EL320
02539      ELSE                                                         EL320
02540          MOVE 'NOT LOGIC TIME SHARE  '  TO  COMP-DESC-A11.        EL320
02541                                                                   EL320
02542      MOVE CF-LGX-CREDIT-USER     TO  COMP-OPT-A12.                EL320
02543      MOVE WS-INVALID-SETTING     TO  COMP-DESC-A12.               EL320
02544                                                                   EL320
02545      IF CO-IS-NOT-USER                                            EL320
02546          MOVE 'COMPANY NOT USER      '  TO  COMP-DESC-A12.        EL320
02547                                                                   EL320
02548      IF CO-HAS-CLAS-IC-CREDIT                                     EL320
02549          MOVE 'CO. HAS CLAS-IC CREDIT'  TO  COMP-DESC-A12.        EL320
02550                                                                   EL320
02551      MOVE CF-LGX-CLAIM-USER      TO  COMP-OPT-A13.                EL320
02552      MOVE WS-INVALID-SETTING     TO  COMP-DESC-A13.               EL320
02553                                                                   EL320
02554      IF CO-IS-NOT-CLAIM-USER                                      EL320
02555          MOVE 'COMPANY NOT CLAIM USER'  TO  COMP-DESC-A13.        EL320
02556                                                                   EL320
02557      IF CO-HAS-CLAS-IC-CLAIM                                      EL320
02558          MOVE 'CO. HAS CLAS-IC CLAIMS'  TO  COMP-DESC-A13.        EL320
02559                                                                   EL320
02560      MOVE CF-LGX-LIFE-USER       TO  COMP-OPT-A14.                EL320
02561      MOVE WS-INVALID-SETTING     TO  COMP-DESC-A14.               EL320
02562                                                                   EL320
02563      IF CO-IS-NOT-LIFE-USER                                       EL320
02564          MOVE 'COMPANY NOT LIFE USER '  TO  COMP-DESC-A14.        EL320
02565                                                                   EL320
02566      IF CO-HAS-CLAS-IC-LIFE                                       EL320
02567          MOVE 'CO. HAS CLAS-IC LIFE  '  TO  COMP-DESC-A14.        EL320
02568                                                                   EL320
02569      MOVE CF-LIFE-ACCESS-CONTROL  TO  COMP-OPT-A16.               EL320
02570      MOVE WS-INVALID-SETTING      TO  COMP-DESC-A16.              EL320
02571                                                                   EL320
02572      IF CF-LIFE-ST-ACCNT-CNTL                                     EL320
02573          MOVE 'STATE, ACCT,          '  TO  COMP-DESC-A16.        EL320
02574                                                                   EL320
02575      IF CF-LIFE-CARR-GRP-ST-ACCNT-CNTL                            EL320
02576          MOVE 'CARR, GRUP, STATE,ACCT'  TO  COMP-DESC-A16.        EL320
02577                                                                   EL320
02578      IF CF-LIFE-CARR-ST-ACCNT-CNTL                                EL320
02579          MOVE 'CARR, STATE, ACCT     '  TO  COMP-DESC-A16.        EL320
02580                                                                   EL320
02581      IF CF-LIFE-ACCNT-CNTL                                        EL320
02582          MOVE 'ACCT,                 '  TO  COMP-DESC-A16.        EL320
02583                                                                   EL320
02584      IF CF-LIFE-CARR-ACCNT-CNTL                                   EL320
02585          MOVE 'CARR, ACCT,           '  TO  COMP-DESC-A16.        EL320
02586                                                                   EL320
02587      MOVE CF-CERT-ACCESS-CONTROL                                  EL320
02588                                  TO  COMP-OPT-A17.                EL320
02589      MOVE WS-INVALID-SETTING     TO  COMP-DESC-A17.               EL320
02590                                                                   EL320
02591      IF CF-ST-ACCNT-CNTL                                          EL320
02592          MOVE 'STATE, ACCT,          '  TO  COMP-DESC-A17.        EL320
02593                                                                   EL320
02594      IF CF-CARR-GROUP-ST-ACCNT-CNTL                               EL320
02595          MOVE 'CARR, GRUP, STATE,ACCT'  TO  COMP-DESC-A17.        EL320
02596                                                                   EL320
02597      IF CF-CARR-ST-ACCNT-CNTL                                     EL320
02598          MOVE 'CARR, STATE, ACCT,    '  TO  COMP-DESC-A17.        EL320
02599                                                                   EL320
02600      IF CF-ACCNT-CNTL                                             EL320
02601          MOVE 'ACCT,                 '  TO  COMP-DESC-A17.        EL320
02602                                                                   EL320
02603      IF CF-CARR-ACCNT-CNTL                                        EL320
02604          MOVE 'CARR, ACCT,           '  TO  COMP-DESC-A17.        EL320
02605                                                                   EL320
02606      MOVE CF-CREDIT-CALC-CODES   TO  COMP-OPT-A18.                EL320
02607      MOVE WS-INVALID-SETTING     TO  COMP-DESC-A18.               EL320
02608                                                                   EL320
02609      IF CR-EARN-AFTER-15TH                                        EL320
02610          MOVE 'EARN AFTER 15TH       '  TO  COMP-DESC-A18.        EL320
02611                                                                   EL320
02612      IF CR-EARN-ON-HALF-MO                                        EL320
02613          MOVE 'EARN ON HALF MONTH    '  TO  COMP-DESC-A18.        EL320
02614                                                                   EL320
02615      IF CR-EARN-ON-1ST-DAY                                        EL320
02616          MOVE 'EARN ON FIRST DAY     '  TO  COMP-DESC-A18.        EL320
02617                                                                   EL320
02618      IF CR-EARN-ON-FULL-MO                                        EL320
02619          MOVE 'EARN ON FULL MONTH    '  TO  COMP-DESC-A18.        EL320
02620                                                                   EL320
02621      IF CR-EARN-WITH-NO-DAYS                                      EL320
02622          MOVE 'EARN WITH NO DAYS     '  TO  COMP-DESC-A18.        EL320
02623                                                                   EL320
02624      MOVE CF-CR-R78-METHOD       TO  COMP-OPT-A19.                EL320
02625      MOVE WS-INVALID-SETTING     TO  COMP-DESC-A19.               EL320
02626                                                                   EL320
02627      IF USE-TERM-PLUS-ONE                                         EL320
02628          MOVE 'USE TERM X (TERM + 1) '  TO  COMP-DESC-A19.        EL320
02629                                                                   EL320
02630      IF DONT-USE-PLUS-ONE                                         EL320
02631          MOVE 'USE TERM X TERM       '  TO  COMP-DESC-A19.        EL320
02632                                                                   EL320
02633      MOVE CF-SYSTEM-D            TO  COMP-OPT-A20.                EL320
02634      MOVE WS-INVALID-SETTING     TO  COMP-DESC-A20.               EL320
02635                                                                   EL320
02636      IF DAILY-BILL-SYS-USED                                       EL320
02637          MOVE 'G.A. BILLING IS USED  '  TO  COMP-DESC-A20         EL320
02638      ELSE                                                         EL320
02639          MOVE 'G.A. BILLING NOT USED '  TO  COMP-DESC-A20.        EL320
02640                                                                   EL320
02641      MOVE CF-SYSTEM-C            TO  COMP-OPT-A21.                EL320
02642      MOVE WS-INVALID-SETTING     TO  COMP-DESC-A21.               EL320
02643                                                                   EL320
02644      IF DAILY-BILL-SYS-USED                                       EL320
02645          MOVE 'CONFIRMATION IS USED  '  TO  COMP-DESC-A21         EL320
02646      ELSE                                                         EL320
02647          MOVE 'CONFIRMATION NOT USED '  TO  COMP-DESC-A21.        EL320
02648                                                                   EL320
02649      MOVE CF-MEMBER-CAPTION      TO  COMP-OPT-A22.                EL320
02650      MOVE SPACES                 TO  COMP-DESC-A22.               EL320
02651      MOVE CF-MAIL-PROCESSING     TO  COMP-OPT-A23.                EL320
02652      MOVE WS-INVALID-SETTING     TO  COMP-DESC-A23.               EL320
02653                                                                   EL320
02654      IF MAIL-PROCESSING                                           EL320
02655          MOVE 'MAIL PROCESSING USED    '  TO  COMP-DESC-A23       EL320
02656      ELSE                                                         EL320
02657          MOVE 'MAIL PROCESSING NOT USED'  TO  COMP-DESC-A23.      EL320
02658                                                                   EL320
02659      MOVE CF-FORMS-PRINTER-ID    TO  COMP-OPT-A24.                EL320
02660      MOVE CF-CHECK-PRINTER-ID    TO  COMP-OPT-A25.                EL320
02661      MOVE CF-NEXT-COMPANY-ID     TO  COMP-OPT-A26.                EL320
02662      MOVE CF-SOC-SEC-NO-SW       TO  COMP-OPT-A27.                EL320
02663      MOVE WS-INVALID-SETTING     TO  COMP-DESC-A27.               EL320
02664                                                                   EL320
02665      IF SOC-SEC-NO-USED                                           EL320
02666          MOVE 'SOCIAL SECURITY NUMBER USED'  TO  COMP-DESC-A27    EL320
02667      ELSE                                                         EL320
02668          MOVE 'SOCIAL SECURITY NUMBER NOT USED'                   EL320
02669                                  TO  COMP-DESC-A27.               EL320
02670                                                                   EL320
02671      MOVE CF-MEMBER-NO-SW        TO  COMP-OPT-A28.                EL320
02672      MOVE WS-INVALID-SETTING     TO  COMP-DESC-A28.               EL320
02673                                                                   EL320
02674      IF MEMBER-NO-USED                                            EL320
02675          MOVE 'MEMBER NUMBER IS USED'  TO  COMP-DESC-A28          EL320
02676      ELSE                                                         EL320
02677          MOVE 'MEMBER NUMBER NOT USED'  TO  COMP-DESC-A28.        EL320
02678                                                                   EL320
02679      MOVE CF-LIFE-OVERRIDE-L1    TO  COMP-OPT-A29-1.              EL320
02680      MOVE CF-LIFE-OVERRIDE-L2    TO  COMP-OPT-A29-2.              EL320
02681      MOVE CF-LIFE-OVERRIDE-L6    TO  COMP-OPT-A29-3               EL320
02682                                      WS-HOLD-LF-L6                EL320
02683      MOVE CF-LIFE-OVERRIDE-L12   TO  COMP-OPT-A29-4.              EL320
02684      MOVE CF-AH-OVERRIDE-L1      TO  COMP-OPT-A30-1.              EL320
02685      MOVE CF-AH-OVERRIDE-L2      TO  COMP-OPT-A30-2.              EL320
02686      MOVE CF-AH-OVERRIDE-L6      TO  COMP-OPT-A30-3               EL320
02687                                      WS-HOLD-AH-L6                EL320
02688      MOVE CF-AH-OVERRIDE-L12     TO  COMP-OPT-A30-4.              EL320
02689      MOVE CF-REPORT-CD1-CAPTION  TO  COMP-OPT-A31.                EL320
02690      MOVE CF-REPORT-CD2-CAPTION  TO  COMP-OPT-A32.                EL320
02691      MOVE CF-CURRENT-MONTH-END   TO  DC-BIN-DATE-1.               EL320
02692      MOVE ' '                    TO  DC-OPTION-CODE.              EL320
02693                                                                   EL320
02694      PERFORM 1100-DATE-RTN  THRU  1100-EXIT.                      EL320
02695                                                                   EL320
02696      MOVE DC-GREG-DATE-1-EDIT    TO  COMP-OPT-A33.                EL320
02697      MOVE CF-CR-MONTH-END-DT     TO  DC-BIN-DATE-1.               EL320
02698      MOVE ' '                    TO  DC-OPTION-CODE.              EL320
02699                                                                   EL320
02700      PERFORM 1100-DATE-RTN  THRU  1100-EXIT.                      EL320
02701                                                                   EL320
02702      MOVE DC-GREG-DATE-1-EDIT    TO  COMP-OPT-A34.                EL320
02703                                                                   EL320
02704      MOVE +1                     TO  COMP-OPT-B3.                 EL320
02705      MOVE WS-INVALID-SETTING     TO  COMP-DESC-B3.                EL320
02706                                                                   EL320
02707      MOVE 'CARRIER CLAIM CONTROL'                                 EL320
02708                                  TO  COMP-DESC-B3.                EL320
02709                                                                   EL320
02710      MOVE CF-CARRIER-CONTROL-LEVEL                                EL320
02711                                  TO  COMP-OPT-B4.                 EL320
02712      MOVE 'CONTROLLING CARRIER'  TO  COMP-DESC-B4.                EL320
02713                                                                   EL320
02714      IF CF-CARRIER-CONTROL-LEVEL  EQUAL  SPACE                    EL320
02715          MOVE 'USE ACTUAL CARRIER    '                            EL320
02716                                  TO  COMP-DESC-B4.                EL320
02717                                                                   EL320
02718      MOVE CF-PAYMENT-APPROVAL-SW                                  EL320
02719                                  TO  COMP-OPT-B5.                 EL320
02720      MOVE WS-INVALID-SETTING     TO  COMP-DESC-B5.                EL320
02721                                                                   EL320
02722      IF CF-PMT-APPROVAL-USED                                      EL320
02723          MOVE 'PAYMENT APPROVAL USED '                            EL320
02724                                  TO  COMP-DESC-B5                 EL320
02725      ELSE                                                         EL320
02726          MOVE 'PAYMENT APPROVAL NOT USED'                         EL320
02727                                  TO  COMP-DESC-B5.                EL320
02728                                                                   EL320
02729      MOVE CF-CO-TOL-CLAIM        TO  COMP-OPT-B6.                 EL320
02730      MOVE WS-INVALID-SETTING     TO  COMP-DESC-B6.                EL320
02731                                                                   EL320
02732      IF CF-CO-TOL-CLAIM  EQUAL  ZEROS                             EL320
02733          MOVE 'CLAIM TOLERANCE NOT USED'                          EL320
02734                                  TO  COMP-DESC-B6                 EL320
02735      ELSE                                                         EL320
02736          MOVE 'CLAIM TOLERANCE USED'                              EL320
02737                                  TO  COMP-DESC-B6.                EL320
02738                                                                   EL320
02739      MOVE CF-CO-CLAIM-REJECT-SW  TO  COMP-OPT-B7.                 EL320
02740      MOVE WS-INVALID-SETTING     TO  COMP-DESC-B7.                EL320
02741                                                                   EL320
02742      IF CO-WARN-IF-CLAIM-OUT                                      EL320
02743          MOVE 'WARNING IF OUT OF TOLERANCE'                       EL320
02744                                  TO  COMP-DESC-B7.                EL320
02745                                                                   EL320
02746      IF CO-FORCE-IF-CLAIM-OUT                                     EL320
02747          MOVE 'FORCE IF OUT OF TOLERANCE'                         EL320
02748                                  TO  COMP-DESC-B7.                EL320
02749                                                                   EL320
02750      MOVE CF-CLAIM-CUTOFF-DATE   TO  DC-BIN-DATE-1.               EL320
02751      MOVE ' '                    TO  DC-OPTION-CODE.              EL320
02752                                                                   EL320
02753      PERFORM 1100-DATE-RTN  THRU  1100-EXIT.                      EL320
02754                                                                   EL320
02755      MOVE DC-GREG-DATE-1-EDIT    TO  COMP-OPT-B8.                 EL320
02756      MOVE CF-CO-CLAIM-COUNTER    TO  COMP-OPT-B9.                 EL320
02757      MOVE CF-CO-ARCHIVE-COUNTER  TO  COMP-OPT-B10.                EL320
02758      MOVE CF-CO-CHECK-COUNTER    TO  COMP-OPT-B11.                EL320
02759      MOVE CF-CO-CHECK-QUE-COUNTER                                 EL320
02760                                  TO  COMP-OPT-B12.                EL320
02761      MOVE CF-STARTING-ARCH-NO    TO  COMP-OPT-B13.                EL320
02762                                                                   EL320
02763      MOVE CF-CO-TOL-PREM         TO  COMP-OPT-C3.                 EL320
02764      MOVE WS-INVALID-SETTING     TO  COMP-DESC-C3.                EL320
02765                                                                   EL320
02766      IF CF-CO-TOL-PREM  EQUAL  ZEROS                              EL320
02767          MOVE 'PREMIUM TOLERANCE NOT USED'                        EL320
02768                                  TO  COMP-DESC-C3                 EL320
02769      ELSE                                                         EL320
02770          MOVE 'PREMIUM TOLERANCE USED'                            EL320
02771                                  TO  COMP-DESC-C3.                EL320
02772                                                                   EL320
02773      MOVE CF-CO-PREM-REJECT-SW   TO  COMP-OPT-C4.                 EL320
02774      MOVE WS-INVALID-SETTING     TO  COMP-DESC-C4.                EL320
02775                                                                   EL320
02776      IF CO-WARN-IF-PREM-OUT                                       EL320
02777          MOVE 'WARNING IF OUT OF TOLERANCE'                       EL320
02778                                  TO  COMP-DESC-C4.                EL320
02779                                                                   EL320
02780      IF CO-FORCE-IF-PREM-OUT                                      EL320
02781          MOVE 'FORCE IF OUT OF TOLERANCE'                         EL320
02782                                  TO  COMP-DESC-C4.                EL320
02783                                                                   EL320
02784      MOVE CF-CO-TOL-REFUND       TO  COMP-OPT-C5.                 EL320
02785      MOVE WS-INVALID-SETTING     TO  COMP-DESC-C5.                EL320
02786                                                                   EL320
02787      IF CF-CO-TOL-REFUND  EQUAL  ZEROS                            EL320
02788          MOVE 'REFUND TOLERANCE NOT USED'                         EL320
02789                                  TO  COMP-DESC-C5                 EL320
02790      ELSE                                                         EL320
02791          MOVE 'REFUND TOLERANCE USED'                             EL320
02792                                  TO  COMP-DESC-C5.                EL320
02793                                                                   EL320
02794      MOVE CF-CO-REF-REJECT-SW    TO  COMP-OPT-C6.                 EL320
02795      MOVE WS-INVALID-SETTING     TO  COMP-DESC-C6.                EL320
02796                                                                   EL320
02797      IF CO-WARN-IF-REF-OUT                                        EL320
02798          MOVE 'WARNING IF OUT OF TOLERANCE'                       EL320
02799                                  TO  COMP-DESC-C6.                EL320
02800                                                                   EL320
02801      IF CO-FORCE-IF-REF-OUT                                       EL320
02802          MOVE 'FORCE IF OUT OF TOLERANCE'                         EL320
02803                                  TO  COMP-DESC-C6.                EL320
02804                                                                   EL320
02805      MOVE CF-RUN-FREQUENCY-SW    TO  COMP-OPT-C7.                 EL320
02806      MOVE WS-INVALID-SETTING     TO  COMP-DESC-C7.                EL320
02807                                                                   EL320
02808      IF CO-IS-PROCESSED-MONTHLY                                   EL320
02809          MOVE 'COMPANY PROCESSES MONTHLY'                         EL320
02810                                  TO  COMP-DESC-C7.                EL320
02811                                                                   EL320
02812      IF CO-IS-PROCESSED-ON-QTR                                    EL320
02813          MOVE 'COMPANY PROCESSES QUARTERLY'                       EL320
02814                                  TO  COMP-DESC-C7.                EL320
02815                                                                   EL320
02816      MOVE CF-REIN-TABLE-SW       TO  COMP-OPT-C8.                 EL320
02817      MOVE WS-INVALID-SETTING     TO  COMP-DESC-C8.                EL320
02818                                                                   EL320
02819      IF REIN-TABLES-ARE-USED                                      EL320
02820          MOVE 'REINSURANCE TABLES ARE USED'                       EL320
02821                                  TO  COMP-DESC-C8.                EL320
02822                                                                   EL320
02823      MOVE CF-COMP-TABLE-SW       TO  COMP-OPT-C9.                 EL320
02824      MOVE WS-INVALID-SETTING     TO  COMP-DESC-C9.                EL320
02825                                                                   EL320
02826      IF COMP-TABLES-ARE-USED                                      EL320
02827          MOVE 'COMPENSATION TABLES ARE USED'                      EL320
02828                                  TO  COMP-DESC-C9.                EL320
02829                                                                   EL320
02830      MOVE CF-COMP-WRITE-OFF-AMT  TO  COMP-OPT-C10.                EL320
02831      MOVE WS-INVALID-SETTING     TO  COMP-DESC-C10.               EL320
02832                                                                   EL320
02833      IF CF-COMP-WRITE-OFF-AMT  EQUAL  ZEROS                       EL320
02834          MOVE 'COMPENSATION WRITE OFF NOT USED'                   EL320
02835                                  TO  COMP-DESC-C10                EL320
02836      ELSE                                                         EL320
02837          MOVE 'COMPENSATION WRITE OFF NOT'                        EL320
02838                                  TO  COMP-DESC-C10.               EL320
02839                                                                   EL320
02840      MOVE CF-CR-CHECK-NO-METHOD  TO  COMP-OPT-C11.                EL320
02841      MOVE WS-INVALID-SETTING     TO  COMP-DESC-C11.               EL320
02842                                                                   EL320
02843      IF CR-CHECK-NO-MANUAL                                        EL320
02844          MOVE 'MANUAL CHECK NUMBERING USED'                       EL320
02845                                  TO  COMP-DESC-C11.               EL320
02846                                                                   EL320
02847      IF CR-CHECK-NO-AUTO-SEQ                                      EL320
02848          MOVE 'AUTOMATIC CHECK NUMBERING USED'                    EL320
02849                                  TO  COMP-DESC-C11.               EL320
02850                                                                   EL320
02851      IF CR-CHECK-NO-AT-PRINT                                      EL320
02852          MOVE 'CHECKS NUMBERED AT PRINT TIME'                     EL320
02853                                  TO  COMP-DESC-C11.               EL320
02854                                                                   EL320
02855      MOVE CF-CAR-GROUP-ACCESS-CNTL                                EL320
02856                                  TO  COMP-OPT-C12.                EL320
02857      MOVE WS-INVALID-SETTING     TO  COMP-DESC-C12.               EL320
02858                                                                   EL320
02859      IF CF-USE-ACTUAL-CARRIER                                     EL320
02860          MOVE 'USE ACTUAL CARRIER'                                EL320
02861                                  TO  COMP-DESC-C12.               EL320
02862                                                                   EL320
02863      IF CF-ZERO-CARRIER                                           EL320
02864          MOVE 'ZERO CARRIER'     TO  COMP-DESC-C12.               EL320
02865                                                                   EL320
02866      IF CF-ZERO-GROUPING                                          EL320
02867          MOVE 'ZERO GROUPING'    TO  COMP-DESC-C12.               EL320
02868                                                                   EL320
02869      IF CF-ZERO-CAR-GROUP                                         EL320
02870          MOVE 'ZERO CARRIER AND GROUPING'                         EL320
02871                                  TO  COMP-DESC-C12.               EL320
02872                                                                   EL320
02873      MOVE CF-ALT-MORT-CODE       TO  COMP-OPT-C13.                EL320
02874      MOVE WS-INVALID-SETTING     TO  COMP-DESC-C13.               EL320
02875                                                                   EL320
02876      IF CF-ALT-MORT-CODE  EQUAL  SPACES                           EL320
02877          MOVE 'ALTERNATE MORTALITY CODE NOT USED'                 EL320
02878                                  TO  COMP-DESC-C13                EL320
02879      ELSE                                                         EL320
02880          MOVE 'USED AS ALTERNATE MORTALITY CODE'                  EL320
02881                                  TO  COMP-DESC-C13.               EL320
02882                                                                   EL320
02883      MOVE CF-MIN-AGE             TO  COMP-OPT-C14.                EL320
02884      MOVE CF-MAX-TERM            TO  COMP-OPT-C15.                EL320
02885      MOVE CF-DEFAULT-AGE         TO  COMP-OPT-C16.                EL320
02886      MOVE CF-DEFAULT-SEX         TO  COMP-OPT-C17.                EL320
02887      MOVE CF-JOINT-AGE-INPUT     TO  COMP-OPT-C18.                EL320
02888      MOVE WS-INVALID-SETTING     TO  COMP-DESC-C18.               EL320
02889                                                                   EL320
02890      IF CF-JOINT-AGE-INPUT  EQUAL  SPACES                         EL320
02891          MOVE 'JOINT AGE IS NOT INPUT'                            EL320
02892                                  TO  COMP-DESC-C18.               EL320
02893                                                                   EL320
02894      IF CF-JOINT-AGE-IS-INPUT                                     EL320
02895          MOVE 'JOINT AGE IS INPUT'                                EL320
02896                                  TO  COMP-DESC-C18.               EL320
02897                                                                   EL320
02898      MOVE CF-BIRTH-DATE-INPUT    TO  COMP-OPT-C19.                EL320
02899      MOVE WS-INVALID-SETTING     TO  COMP-DESC-C19.               EL320
02900                                                                   EL320
02901      IF CF-BIRTH-DATE-INPUT  EQUAL  SPACES                        EL320
02902          MOVE 'BIRTH DATE IS NOT INPUT'                           EL320
02903                                  TO  COMP-DESC-C19.               EL320
02904                                                                   EL320
02905      IF CF-BIRTH-DATE-IS-INPUT                                    EL320
02906          MOVE 'BIRTH DATE IS INPUT'                               EL320
02907                                  TO  COMP-DESC-C19.               EL320
02908                                                                   EL320
02909      MOVE CF-MIN-PREMIUM         TO  COMP-OPT-C20.                EL320
02910                                                                   EL320
02911      IF CF-MIN-PREMIUM  EQUAL  ZEROS                              EL320
02912          MOVE 'MINIMUM PREMIUM NOT USED'                          EL320
02913                                  TO  COMP-DESC-C20.               EL320
02914                                                                   EL320
02915      MOVE CF-CONVERSION-DT       TO  DC-BIN-DATE-1.               EL320
02916      MOVE ' '                    TO  DC-OPTION-CODE.              EL320
02917                                                                   EL320
02918      PERFORM 1100-DATE-RTN  THRU  1100-EXIT.                      EL320
02919                                                                   EL320
02920      MOVE DC-GREG-DATE-1-EDIT    TO  COMP-OPT-C21.                EL320
02921      MOVE CF-LAST-BATCH-NO       TO  COMP-OPT-C22.                EL320
02922      MOVE CF-CR-CHECK-COUNTER    TO  COMP-OPT-C23.                EL320
02923      MOVE CF-CR-CHECK-QUE-COUNTER                                 EL320
02924                                  TO  COMP-OPT-C24.                EL320
02925      MOVE CF-RATES-FILE-MAINT-DT                                  EL320
02926                                  TO  DC-BIN-DATE-1.               EL320
02927      MOVE ' '                    TO  DC-OPTION-CODE.              EL320
02928                                                                   EL320
02929      PERFORM 1100-DATE-RTN  THRU  1100-EXIT.                      EL320
02930                                                                   EL320
02931      MOVE DC-GREG-DATE-1-EDIT    TO  COMP-OPT-C26A.               EL320
02932      MOVE CF-RATES-FILE-CREATE-DT                                 EL320
02933                                  TO  DC-BIN-DATE-1.               EL320
02934      MOVE ' '                    TO  DC-OPTION-CODE.              EL320
02935                                                                   EL320
02936      PERFORM 1100-DATE-RTN  THRU  1100-EXIT.                      EL320
02937                                                                   EL320
02938      MOVE DC-GREG-DATE-1-EDIT    TO  COMP-OPT-C26B.               EL320
02939      MOVE CF-COMMISSION-TAB-MAINT-DT                              EL320
02940                                  TO  DC-BIN-DATE-1.               EL320
02941      MOVE ' '                    TO  DC-OPTION-CODE.              EL320
02942                                                                   EL320
02943      PERFORM 1100-DATE-RTN  THRU  1100-EXIT.                      EL320
02944                                                                   EL320
02945      MOVE DC-GREG-DATE-1-EDIT    TO  COMP-OPT-C27A.               EL320
02946      MOVE CF-COMMISSION-TAB-CREATE-DT                             EL320
02947                                  TO  DC-BIN-DATE-1.               EL320
02948      MOVE ' '                    TO  DC-OPTION-CODE.              EL320
02949                                                                   EL320
02950      PERFORM 1100-DATE-RTN  THRU  1100-EXIT.                      EL320
02951                                                                   EL320
02952      MOVE DC-GREG-DATE-1-EDIT    TO  COMP-OPT-C27B.               EL320
02953      MOVE CF-ACCOUNT-MSTR-MAINT-DT                                EL320
02954                                  TO  DC-BIN-DATE-1.               EL320
02955      MOVE ' '                    TO  DC-OPTION-CODE.              EL320
02956                                                                   EL320
02957      PERFORM 1100-DATE-RTN  THRU  1100-EXIT.                      EL320
02958                                                                   EL320
02959      MOVE DC-GREG-DATE-1-EDIT    TO  COMP-OPT-C28A.               EL320
02960      MOVE CF-ACCOUNT-MSTR-CREATE-DT                               EL320
02961                                  TO  DC-BIN-DATE-1.               EL320
02962      MOVE ' '                    TO  DC-OPTION-CODE.              EL320
02963                                                                   EL320
02964      PERFORM 1100-DATE-RTN  THRU  1100-EXIT.                      EL320
02965                                                                   EL320
02966      MOVE DC-GREG-DATE-1-EDIT    TO  COMP-OPT-C28B.               EL320
02967      MOVE CF-REINSURANCE-TAB-MAINT-DT                             EL320
02968                                  TO  DC-BIN-DATE-1.               EL320
02969      MOVE ' '                    TO  DC-OPTION-CODE.              EL320
02970                                                                   EL320
02971      PERFORM 1100-DATE-RTN  THRU  1100-EXIT.                      EL320
02972                                                                   EL320
02973      MOVE DC-GREG-DATE-1-EDIT    TO  COMP-OPT-C29A.               EL320
02974      MOVE CF-REINSURANCE-TAB-CREATE-DT                            EL320
02975                                  TO  DC-BIN-DATE-1.               EL320
02976      MOVE ' '                    TO  DC-OPTION-CODE.              EL320
02977                                                                   EL320
02978      PERFORM 1100-DATE-RTN  THRU  1100-EXIT.                      EL320
02979                                                                   EL320
02980      MOVE DC-GREG-DATE-1-EDIT    TO  COMP-OPT-C29B.               EL320
02981      MOVE CF-COMPENSATION-MSTR-MAINT-DT                           EL320
02982                                  TO  DC-BIN-DATE-1.               EL320
02983      MOVE ' '                    TO  DC-OPTION-CODE.              EL320
02984                                                                   EL320
02985      PERFORM 1100-DATE-RTN  THRU  1100-EXIT.                      EL320
02986                                                                   EL320
02987      MOVE DC-GREG-DATE-1-EDIT    TO  COMP-OPT-C30A.               EL320
02988      MOVE CF-COMPENSATION-MSTR-CREATE-DT                          EL320
02989                                  TO  DC-BIN-DATE-1.               EL320
02990      MOVE ' '                    TO  DC-OPTION-CODE.              EL320
02991                                                                   EL320
02992      PERFORM 1100-DATE-RTN  THRU  1100-EXIT.                      EL320
02993                                                                   EL320
02994      MOVE DC-GREG-DATE-1-EDIT    TO  COMP-OPT-C30B.               EL320
02995                                                                   EL320
02996      MOVE TS                     TO  X.                           EL320
02997      MOVE COMP-LINE-A1           TO  P-DATA.                      EL320
02998                                                                   EL320
02999      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03000                                                                   EL320
03001      MOVE SS                     TO  X.                           EL320
03002      MOVE COMP-LINE-A2           TO  P-DATA.                      EL320
03003                                                                   EL320
03004      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03005                                                                   EL320
03006      MOVE DS                     TO  X.                           EL320
03007      MOVE COMP-LINE-A3           TO  P-DATA.                      EL320
03008                                                                   EL320
03009      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03010                                                                   EL320
03011      MOVE SS                     TO  X.                           EL320
03012      MOVE COMP-LINE-A4           TO  P-DATA.                      EL320
03013                                                                   EL320
03014      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03015                                                                   EL320
03016      MOVE SS                     TO  X.                           EL320
03017      MOVE COMP-LINE-A5           TO  P-DATA.                      EL320
03018                                                                   EL320
03019      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03020                                                                   EL320
03021      MOVE SS                     TO  X.                           EL320
03022      MOVE COMP-LINE-A6           TO  P-DATA.                      EL320
03023                                                                   EL320
03024      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03025                                                                   EL320
03026      MOVE SS                     TO  X.                           EL320
03027      MOVE COMP-LINE-A7           TO  P-DATA.                      EL320
03028                                                                   EL320
03029      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03030                                                                   EL320
03031      MOVE SS                     TO  X.                           EL320
03032      MOVE COMP-LINE-A8           TO  P-DATA.                      EL320
03033                                                                   EL320
03034      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03035                                                                   EL320
03036      MOVE TS                     TO  X.                           EL320
03037      MOVE COMP-LINE-A9           TO  P-DATA.                      EL320
03038                                                                   EL320
03039      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03040                                                                   EL320
03041      MOVE DS                     TO  X.                           EL320
03042      MOVE COMP-LINE-A10          TO  P-DATA.                      EL320
03043                                                                   EL320
03044      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03045                                                                   EL320
03046      MOVE SS                     TO  X.                           EL320
03047      MOVE COMP-LINE-A11          TO  P-DATA.                      EL320
03048                                                                   EL320
03049      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03050                                                                   EL320
03051      MOVE SS                     TO  X.                           EL320
03052      MOVE COMP-LINE-A12          TO  P-DATA.                      EL320
03053                                                                   EL320
03054      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03055                                                                   EL320
03056      MOVE SS                     TO  X.                           EL320
03057      MOVE COMP-LINE-A13          TO  P-DATA.                      EL320
03058                                                                   EL320
03059      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03060                                                                   EL320
03061      MOVE SS                     TO  X.                           EL320
03062      MOVE COMP-LINE-A14          TO  P-DATA.                      EL320
03063                                                                   EL320
03064      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03065                                                                   EL320
03066      MOVE SS                     TO  X.                           EL320
03067      MOVE COMP-LINE-A15          TO  P-DATA.                      EL320
03068                                                                   EL320
03069      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03070                                                                   EL320
03071      MOVE SS                     TO  X.                           EL320
03072      MOVE COMP-LINE-A16          TO  P-DATA.                      EL320
03073                                                                   EL320
03074      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03075                                                                   EL320
03076      MOVE DS                     TO  X.                           EL320
03077      MOVE COMP-LINE-A17          TO  P-DATA.                      EL320
03078                                                                   EL320
03079      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03080                                                                   EL320
03081      MOVE SS                     TO  X.                           EL320
03082      MOVE COMP-LINE-A18          TO  P-DATA.                      EL320
03083                                                                   EL320
03084      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03085                                                                   EL320
03086      MOVE SS                     TO  X.                           EL320
03087      MOVE COMP-LINE-A19          TO  P-DATA.                      EL320
03088                                                                   EL320
03089      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03090                                                                   EL320
03091      MOVE SS                     TO  X.                           EL320
03092      MOVE COMP-LINE-A20          TO  P-DATA.                      EL320
03093                                                                   EL320
03094      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03095                                                                   EL320
03096      MOVE SS                     TO  X.                           EL320
03097      MOVE COMP-LINE-A21          TO  P-DATA.                      EL320
03098                                                                   EL320
03099      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03100                                                                   EL320
03101      MOVE SS                     TO  X.                           EL320
03102      MOVE COMP-LINE-A22          TO  P-DATA.                      EL320
03103                                                                   EL320
03104      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03105                                                                   EL320
03106      MOVE SS                     TO  X.                           EL320
03107      MOVE COMP-LINE-A23          TO  P-DATA.                      EL320
03108                                                                   EL320
03109      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03110                                                                   EL320
03111      MOVE DS                     TO  X.                           EL320
03112      MOVE COMP-LINE-A24          TO  P-DATA.                      EL320
03113                                                                   EL320
03114      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03115                                                                   EL320
03116      MOVE SS                     TO  X.                           EL320
03117      MOVE COMP-LINE-A25          TO  P-DATA.                      EL320
03118                                                                   EL320
03119      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03120                                                                   EL320
03121      MOVE SS                     TO  X.                           EL320
03122      MOVE COMP-LINE-A26          TO  P-DATA.                      EL320
03123                                                                   EL320
03124      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03125                                                                   EL320
03126      MOVE SS                     TO  X.                           EL320
03127      MOVE COMP-LINE-A27          TO  P-DATA.                      EL320
03128                                                                   EL320
03129      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03130                                                                   EL320
03131      MOVE SS                     TO  X.                           EL320
03132      MOVE COMP-LINE-A28          TO  P-DATA.                      EL320
03133                                                                   EL320
03134      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03135                                                                   EL320
03136      MOVE DS                     TO  X.                           EL320
03137      MOVE COMP-LINE-A29          TO  P-DATA.                      EL320
03138                                                                   EL320
03139      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03140                                                                   EL320
03141      MOVE SS                     TO  X.                           EL320
03142      MOVE COMP-LINE-A30          TO  P-DATA.                      EL320
03143                                                                   EL320
03144      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03145                                                                   EL320
03146      MOVE DS                     TO  X.                           EL320
03147      MOVE COMP-LINE-A31          TO  P-DATA.                      EL320
03148                                                                   EL320
03149      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03150                                                                   EL320
03151      MOVE SS                     TO  X.                           EL320
03152      MOVE COMP-LINE-A32          TO  P-DATA.                      EL320
03153                                                                   EL320
03154      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03155                                                                   EL320
03156      MOVE DS                     TO  X.                           EL320
03157      MOVE COMP-LINE-A33          TO  P-DATA.                      EL320
03158                                                                   EL320
03159      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03160                                                                   EL320
03161      MOVE SS                     TO  X.                           EL320
03162      MOVE COMP-LINE-A34          TO  P-DATA.                      EL320
03163                                                                   EL320
03164      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03165                                                                   EL320
03166      PERFORM  1200-HEADING  THRU  1200-EXIT.                      EL320
03167                                                                   EL320
03168      MOVE TS                     TO  X.                           EL320
03169      MOVE COMP-LINE-B1           TO  P-DATA.                      EL320
03170                                                                   EL320
03171      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03172                                                                   EL320
03173      MOVE SS                     TO  X.                           EL320
03174      MOVE COMP-LINE-B2           TO  P-DATA.                      EL320
03175                                                                   EL320
03176      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03177                                                                   EL320
03178      MOVE DS                     TO  X.                           EL320
03179      MOVE COMP-LINE-B3           TO  P-DATA.                      EL320
03180                                                                   EL320
03181      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03182                                                                   EL320
03183      MOVE SS                     TO  X.                           EL320
03184      MOVE COMP-LINE-B4           TO  P-DATA.                      EL320
03185                                                                   EL320
03186      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03187                                                                   EL320
03188      MOVE SS                     TO  X.                           EL320
03189      MOVE COMP-LINE-B5           TO  P-DATA.                      EL320
03190                                                                   EL320
03191      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03192                                                                   EL320
03193      MOVE SS                     TO  X.                           EL320
03194      MOVE COMP-LINE-B6           TO  P-DATA.                      EL320
03195                                                                   EL320
03196      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03197                                                                   EL320
03198      MOVE SS                     TO  X.                           EL320
03199      MOVE COMP-LINE-B7           TO  P-DATA.                      EL320
03200                                                                   EL320
03201      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03202                                                                   EL320
03203      MOVE SS                     TO  X.                           EL320
03204      MOVE COMP-LINE-B8           TO  P-DATA.                      EL320
03205                                                                   EL320
03206      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03207                                                                   EL320
03208      MOVE DS                     TO  X.                           EL320
03209      MOVE COMP-LINE-B9           TO  P-DATA.                      EL320
03210                                                                   EL320
03211      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03212                                                                   EL320
03213      MOVE SS                     TO  X.                           EL320
03214      MOVE COMP-LINE-B10          TO  P-DATA.                      EL320
03215                                                                   EL320
03216      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03217                                                                   EL320
03218      MOVE SS                     TO  X.                           EL320
03219      MOVE COMP-LINE-B11          TO  P-DATA.                      EL320
03220                                                                   EL320
03221      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03222                                                                   EL320
03223      MOVE SS                     TO  X.                           EL320
03224      MOVE COMP-LINE-B12          TO  P-DATA.                      EL320
03225                                                                   EL320
03226      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03227                                                                   EL320
03228      MOVE SS                     TO  X.                           EL320
03229      MOVE COMP-LINE-B13          TO  P-DATA.                      EL320
03230                                                                   EL320
03231      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03232                                                                   EL320
03233      PERFORM  1200-HEADING  THRU  1200-EXIT.                      EL320
03234                                                                   EL320
03235      MOVE TS                     TO  X.                           EL320
03236      MOVE COMP-LINE-C1           TO  P-DATA.                      EL320
03237                                                                   EL320
03238      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03239                                                                   EL320
03240      MOVE SS                     TO  X.                           EL320
03241      MOVE COMP-LINE-C2           TO  P-DATA.                      EL320
03242                                                                   EL320
03243      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03244                                                                   EL320
03245      MOVE DS                     TO  X.                           EL320
03246      MOVE COMP-LINE-C3           TO  P-DATA.                      EL320
03247                                                                   EL320
03248      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03249                                                                   EL320
03250      MOVE SS                     TO  X.                           EL320
03251      MOVE COMP-LINE-C4           TO  P-DATA.                      EL320
03252                                                                   EL320
03253      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03254                                                                   EL320
03255      MOVE SS                     TO  X.                           EL320
03256      MOVE COMP-LINE-C5           TO  P-DATA.                      EL320
03257                                                                   EL320
03258      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03259                                                                   EL320
03260      MOVE SS                     TO  X.                           EL320
03261      MOVE COMP-LINE-C6           TO  P-DATA.                      EL320
03262                                                                   EL320
03263      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03264                                                                   EL320
03265      MOVE SS                     TO  X.                           EL320
03266      MOVE COMP-LINE-C7           TO  P-DATA.                      EL320
03267                                                                   EL320
03268      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03269                                                                   EL320
03270      MOVE SS                     TO  X.                           EL320
03271      MOVE COMP-LINE-C8           TO  P-DATA.                      EL320
03272                                                                   EL320
03273      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03274                                                                   EL320
03275      MOVE SS                     TO  X.                           EL320
03276      MOVE COMP-LINE-C9           TO  P-DATA.                      EL320
03277                                                                   EL320
03278      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03279                                                                   EL320
03280      MOVE SS                     TO  X.                           EL320
03281      MOVE COMP-LINE-C10          TO  P-DATA.                      EL320
03282                                                                   EL320
03283      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03284                                                                   EL320
03285      MOVE SS                     TO  X.                           EL320
03286      MOVE COMP-LINE-C11          TO  P-DATA.                      EL320
03287                                                                   EL320
03288      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03289                                                                   EL320
03290      MOVE SS                     TO  X.                           EL320
03291      MOVE COMP-LINE-C12          TO  P-DATA.                      EL320
03292                                                                   EL320
03293      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03294                                                                   EL320
03295      MOVE DS                     TO  X.                           EL320
03296      MOVE COMP-LINE-C13          TO  P-DATA.                      EL320
03297                                                                   EL320
03298      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03299                                                                   EL320
03300      MOVE SS                     TO  X.                           EL320
03301      MOVE COMP-LINE-C14          TO  P-DATA.                      EL320
03302                                                                   EL320
03303      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03304                                                                   EL320
03305      MOVE SS                     TO  X.                           EL320
03306      MOVE COMP-LINE-C15          TO  P-DATA.                      EL320
03307                                                                   EL320
03308      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03309                                                                   EL320
03310      MOVE SS                     TO  X.                           EL320
03311      MOVE COMP-LINE-C16          TO  P-DATA.                      EL320
03312                                                                   EL320
03313      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03314                                                                   EL320
03315      MOVE SS                     TO  X.                           EL320
03316      MOVE COMP-LINE-C17          TO  P-DATA.                      EL320
03317                                                                   EL320
03318      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03319                                                                   EL320
03320      MOVE SS                     TO  X.                           EL320
03321      MOVE COMP-LINE-C18          TO  P-DATA.                      EL320
03322                                                                   EL320
03323      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03324                                                                   EL320
03325      MOVE SS                     TO  X.                           EL320
03326      MOVE COMP-LINE-C19          TO  P-DATA.                      EL320
03327                                                                   EL320
03328      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03329                                                                   EL320
03330      MOVE SS                     TO  X.                           EL320
03331      MOVE COMP-LINE-C20          TO  P-DATA.                      EL320
03332                                                                   EL320
03333      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03334                                                                   EL320
03335      MOVE SS                     TO  X.                           EL320
03336      MOVE COMP-LINE-C21          TO  P-DATA.                      EL320
03337                                                                   EL320
03338      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03339                                                                   EL320
03340      MOVE SS                     TO  X.                           EL320
03341      MOVE COMP-LINE-C22          TO  P-DATA.                      EL320
03342                                                                   EL320
03343      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03344                                                                   EL320
03345      MOVE DS                     TO  X.                           EL320
03346      MOVE COMP-LINE-C23          TO  P-DATA.                      EL320
03347                                                                   EL320
03348      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03349                                                                   EL320
03350      MOVE SS                     TO  X.                           EL320
03351      MOVE COMP-LINE-C24          TO  P-DATA.                      EL320
03352                                                                   EL320
03353      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03354                                                                   EL320
03355      MOVE DS                     TO  X.                           EL320
03356      MOVE COMP-LINE-C25          TO  P-DATA.                      EL320
03357                                                                   EL320
03358      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03359                                                                   EL320
03360      MOVE SS                     TO  X.                           EL320
03361      MOVE COMP-LINE-C26          TO  P-DATA.                      EL320
03362                                                                   EL320
03363      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03364                                                                   EL320
03365      MOVE SS                     TO  X.                           EL320
03366      MOVE COMP-LINE-C27          TO  P-DATA.                      EL320
03367                                                                   EL320
03368      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03369                                                                   EL320
03370      MOVE SS                     TO  X.                           EL320
03371      MOVE COMP-LINE-C28          TO  P-DATA.                      EL320
03372                                                                   EL320
03373      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03374                                                                   EL320
03375      MOVE SS                     TO  X.                           EL320
03376      MOVE COMP-LINE-C29          TO  P-DATA.                      EL320
03377                                                                   EL320
03378      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03379                                                                   EL320
03380      MOVE SS                     TO  X.                           EL320
03381      MOVE COMP-LINE-C30          TO  P-DATA.                      EL320
03382                                                                   EL320
03383      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03384                                                                   EL320
03385  2211-EXIT.                                                       EL320
03386      EXIT.                                                        EL320
03387      EJECT                                                        EL320
03388  2212-PROCESS-PROC-RECORD.                                        EL320
03389                                                                   EL320
03390      IF CF-SEQUENCE-NO EQUAL ZERO                                 EL320
03391          NEXT SENTENCE                                            EL320
03392        ELSE                                                       EL320
03393          GO TO 2212-PRINT-SECURITY.                               EL320
03394                                                                   EL320
03395      MOVE CF-RECORD-TYPE         TO  WS-RECORD-TYPE.              EL320
03396      MOVE CF-LAST-MAINT-BY       TO  PROC-MAINT-BY-A,             EL320
03397                                      PROC-HB1-MAINT-BY.           EL320
03398      MOVE CF-LAST-MAINT-DT       TO  DC-BIN-DATE-1.               EL320
03399      MOVE ' '                    TO  DC-OPTION-CODE.              EL320
03400                                                                   EL320
03401      PERFORM  1100-DATE-RTN  THRU  1100-EXIT.                     EL320
03402                                                                   EL320
03403      MOVE DC-GREG-DATE-1-EDIT    TO  PROC-MAINT-DT-A,             EL320
03404                                      PROC-HB2-MAINT-DATE.         EL320
03405      MOVE CF-LAST-MAINT-HHMMSS   TO  WS-MAINT-TIME.               EL320
03406      MOVE WS-MAINT-HH            TO  PROC-MAINT-HHA,              EL320
03407                                      PROC-HB2-MAINT-HH.           EL320
03408      MOVE WS-MAINT-MM            TO  PROC-MAINT-MMA,              EL320
03409                                      PROC-HB2-MAINT-MM.           EL320
03410      MOVE WS-MAINT-SS            TO  PROC-MAINT-SSA,              EL320
03411                                      PROC-HB2-MAINT-SS.           EL320
03412      MOVE CF-PROCESSOR-NAME      TO  PROC-NAME-A,                 EL320
03413                                      PROC-HB3-NAME.               EL320
03414      MOVE CF-PROCESSOR-TITLE     TO  PROC-TITLE-A,                EL320
03415                                      PROC-HB4-TITLE.              EL320
03416      MOVE CF-PROCESSOR           TO  PROC-ID-A,                   EL320
03417                                      PROC-HB5-PROCESSOR-ID.       EL320
03418                                                                   EL320
03419      IF NO-CARRIER-SECURITY                                       EL320
03420          MOVE WS-NO              TO  PROC-CAP-A6                  EL320
03421      ELSE                                                         EL320
03422          MOVE WS-YES             TO  PROC-CAP-A6.                 EL320
03423                                                                   EL320
03424      IF NO-ACCOUNT-SECURITY                                       EL320
03425          MOVE WS-NO              TO  PROC-CAP-A7                  EL320
03426      ELSE                                                         EL320
03427          MOVE WS-YES             TO  PROC-CAP-A7.                 EL320
03428                                                                   EL320
03429      IF ACCESS-TO-CREDIT       OR                                 EL320
03430         ACCESS-TO-CLAIM-CREDIT OR                                 EL320
03431         ACCESS-TO-ALL-SYSTEMS                                     EL320
03432          MOVE WS-YES             TO  PROC-CAP-A9                  EL320
03433      ELSE                                                         EL320
03434          MOVE WS-NO              TO  PROC-CAP-A9.                 EL320
03435                                                                   EL320
03436      IF ACCESS-TO-CLAIMS       OR                                 EL320
03437         ACCESS-TO-CLAIM-CREDIT OR                                 EL320
03438         ACCESS-TO-ALL-SYSTEMS                                     EL320
03439          MOVE WS-YES             TO  PROC-CAP-A10                 EL320
03440      ELSE                                                         EL320
03441          MOVE WS-NO              TO  PROC-CAP-A10.                EL320
03442                                                                   EL320
03443      IF ACCESS-TO-GNRLDGR      OR                                 EL320
03444         ACCESS-TO-LIFE-GNRLDGR OR                                 EL320
03445         ACCESS-TO-ALL-SYSTEMS                                     EL320
03446          MOVE WS-YES             TO  PROC-CAP-A11                 EL320
03447      ELSE                                                         EL320
03448          MOVE WS-NO              TO  PROC-CAP-A11.                EL320
03449                                                                   EL320
03450      IF MESSAGE-YES                                               EL320
03451          MOVE WS-YES             TO  PROC-CAP-A12                 EL320
03452      ELSE                                                         EL320
03453          MOVE WS-NO              TO  PROC-CAP-A12.                EL320
03454                                                                   EL320
03455      IF PROCESSOR-USER-IS-ALMIGHTY                                EL320
03456          MOVE WS-YES             TO  PROC-CAP-A13                 EL320
03457      ELSE                                                         EL320
03458          MOVE WS-NO              TO  PROC-CAP-A13                 EL320
           END-IF
      *        GO TO 2212-EXIT.
03459                                                                   EL320
03460      IF CF-ADMINISTRATION-CONTROLS (1) = 'YY'                     EL320
03461          MOVE 'UPDATE'           TO  PROC-CAP-A15                 EL320
03462      ELSE                                                         EL320
03463          MOVE 'ACCESS'           TO  PROC-CAP-A15.                EL320
03464                                                                   EL320
03465      IF CF-APPLICATION-FORCE (1) = 'Y'                            EL320
03466          MOVE WS-YES             TO  PROC-CAP-A16                 EL320
03467      ELSE                                                         EL320
03468          MOVE WS-NO              TO  PROC-CAP-A16.                EL320
03469                                                                   EL320
03470      IF CF-ADMINISTRATION-CONTROLS (2) = 'YY'                     EL320
03471          MOVE 'UPDATE'           TO  PROC-CAP-A18                 EL320
03472      ELSE                                                         EL320
03473          MOVE 'ACCESS'           TO  PROC-CAP-A18.                EL320
03474                                                                   EL320
03475      IF CF-APPLICATION-FORCE (2) = 'Y'                            EL320
03476          MOVE WS-YES             TO  PROC-CAP-A19                 EL320
03477      ELSE                                                         EL320
03478          MOVE WS-NO              TO  PROC-CAP-A19.                EL320
03479                                                                   EL320
03480      MOVE CF-PROC-CALC-DAYS-TOL  TO  PROC-DAYS-A21.               EL320
03481      MOVE CF-PROC-CALC-AMT-TOL   TO  PROC-AMT-A22.                EL320
03482      MOVE CF-PROC-MAX-REG-DAYS   TO  PROC-DAYS-A23.               EL320
03483      MOVE CF-PROC-MAX-REG-PMT    TO  PROC-AMT-A24.                EL320
03484      MOVE CF-PROC-MAX-LF-PMT     TO  PROC-AMT-A25.                EL320
03485      MOVE CF-PROC-MAX-AUTO-MOS   TO  PROC-MOS-A26.                EL320
03486      MOVE CF-PROC-MAX-AUTO-PMT   TO  PROC-AMT-A27.                EL320
03487                                                                   EL320
03488      EJECT                                                        EL320
03489  2212-PRINT-PROC-PAGE-1.                                          EL320
03490                                                                   EL320
03491      PERFORM  1200-HEADING  THRU  1200-EXIT.                      EL320
03492                                                                   EL320
03493      MOVE DS                     TO  X.                           EL320
03494      MOVE PROC-LINE-A1           TO  P-DATA.                      EL320
03495      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03496                                                                   EL320
03497      MOVE SS                     TO  X.                           EL320
03498      MOVE PROC-LINE-A2           TO  P-DATA.                      EL320
03499      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03500                                                                   EL320
03501      MOVE TS                     TO  X.                           EL320
03502      MOVE PROC-LINE-A3           TO  P-DATA.                      EL320
03503      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03504                                                                   EL320
03505      MOVE DS                     TO  X.                           EL320
03506      MOVE PROC-LINE-A4           TO  P-DATA.                      EL320
03507      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03508                                                                   EL320
03509      MOVE SS                     TO  X.                           EL320
03510      MOVE PROC-LINE-A5           TO  P-DATA.                      EL320
03511      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03512                                                                   EL320
03513      MOVE DS                     TO  X.                           EL320
03514      MOVE PROC-LINE-A6           TO  P-DATA.                      EL320
03515      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03516                                                                   EL320
03517      MOVE SS                     TO  X.                           EL320
03518      MOVE PROC-LINE-A7           TO  P-DATA.                      EL320
03519      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03520                                                                   EL320
03521      MOVE DS                     TO  X.                           EL320
03522      MOVE PROC-LINE-A8           TO  P-DATA.                      EL320
03523      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03524                                                                   EL320
03525      MOVE DS                     TO  X.                           EL320
03526      MOVE PROC-LINE-A9           TO  P-DATA.                      EL320
03527      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03528                                                                   EL320
03529      MOVE SS                     TO  X.                           EL320
03530      MOVE PROC-LINE-A10          TO  P-DATA.                      EL320
03531      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03532                                                                   EL320
03533      MOVE SS                     TO  X.                           EL320
03534      MOVE PROC-LINE-A11          TO  P-DATA.                      EL320
03535      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03536                                                                   EL320
03537      MOVE SS                     TO  X.                           EL320
03538      MOVE PROC-LINE-A12          TO  P-DATA.                      EL320
03539      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03540                                                                   EL320
03541      MOVE DS                     TO  X.                           EL320
03542      MOVE PROC-LINE-A13          TO  P-DATA.                      EL320
03543      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03544                                                                   EL320
03545      MOVE DS                     TO  X.                           EL320
03546      MOVE PROC-LINE-A14          TO  P-DATA.                      EL320
03547      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03548                                                                   EL320
03549      MOVE DS                     TO  X.                           EL320
03550      MOVE PROC-LINE-A15          TO  P-DATA.                      EL320
03551      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03552                                                                   EL320
03553      MOVE SS                     TO  X.                           EL320
03554      MOVE PROC-LINE-A16          TO  P-DATA.                      EL320
03555      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03556                                                                   EL320
03557      MOVE DS                     TO  X.                           EL320
03558      MOVE PROC-LINE-A17          TO  P-DATA.                      EL320
03559      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03560                                                                   EL320
03561      MOVE DS                     TO  X.                           EL320
03562      MOVE PROC-LINE-A18          TO  P-DATA.                      EL320
03563      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03564                                                                   EL320
03565      MOVE SS                     TO  X.                           EL320
03566      MOVE PROC-LINE-A19          TO  P-DATA.                      EL320
03567      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03568                                                                   EL320
03569      MOVE TS                     TO  X.                           EL320
03570      MOVE PROC-LINE-A20          TO  P-DATA.                      EL320
03571      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03572                                                                   EL320
03573      MOVE DS                     TO  X.                           EL320
03574      MOVE PROC-LINE-A21          TO  P-DATA.                      EL320
03575      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03576                                                                   EL320
03577      MOVE SS                     TO  X.                           EL320
03578      MOVE PROC-LINE-A22          TO  P-DATA.                      EL320
03579      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03580                                                                   EL320
03581      MOVE SS                     TO  X.                           EL320
03582      MOVE PROC-LINE-A23          TO  P-DATA.                      EL320
03583      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03584                                                                   EL320
03585      MOVE SS                     TO  X.                           EL320
03586      MOVE PROC-LINE-A24          TO  P-DATA.                      EL320
03587      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03588                                                                   EL320
03589      MOVE SS                     TO  X.                           EL320
03590      MOVE PROC-LINE-A25          TO  P-DATA.                      EL320
03591      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03592                                                                   EL320
03593      MOVE SS                     TO  X.                           EL320
03594      MOVE PROC-LINE-A26          TO  P-DATA.                      EL320
03595      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03596                                                                   EL320
03597      MOVE SS                     TO  X.                           EL320
03598      MOVE PROC-LINE-A27          TO  P-DATA.                      EL320
03599      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03600                                                                   EL320
03601      EJECT                                                        EL320
03602  2212-PRINT-SECURITY.                                             EL320
03603                                                                   EL320
03604      IF DTE-PRC-OPT EQUAL 1                                       EL320
03605          GO TO 2212-EXIT.                                         EL320
03606                                                                   EL320
03607      PERFORM 2212-SECURITY-TABLE-ROWS THRU 2212-ROWS-EXIT         EL320
03608        VARYING XROW FROM +1 BY +1                                 EL320
03609          UNTIL XROW > +4.                                         EL320
03610                                                                   EL320
03611      GO TO 2212-EXIT.                                             EL320
03612                                                                   EL320
03613                                                                   EL320
03614  2212-SECURITY-TABLE-ROWS.                                        EL320
03615                                                                   EL320
03616 *    ***********************************************************  EL320
03617 *    *   BY ISSUING A "GO TO 2212-EXIT" IN THE FOLLOWING "IF"  *  EL320
03618 *    * STATEMENTS, SECURITY PROCESSING IS BYPASSED FOR THAT    *  EL320
03619 *    * OCCURRENCE IN THE CF PROCESSOR CONTROL RECORD, SO AS    *  EL320
03620 *    * NOT TO PRINT PAGES THAT HAVE NOT BEEN DEFINED YET.      *  EL320
03621 *    ***********************************************************  EL320
03622                                                                   EL320
03623      IF CF-SEQUENCE-NO = ZERO                                     EL320
03624          IF XROW = +1                                             EL320
03625              MOVE 'CREDIT APPLICATIONS'                           EL320
03626                                  TO PROC-HB6-APPL-DESC            EL320
03627            ELSE                                                   EL320
03628          IF XROW = +2                                             EL320
03629              MOVE 'CLAIMS APPLICATIONS'                           EL320
03630                                  TO PROC-HB6-APPL-DESC            EL320
03631            ELSE                                                   EL320
03632          IF XROW = +3                                             EL320
03633              GO TO 2212-ROWS-EXIT                                 EL320
03634            ELSE                                                   EL320
03635          IF XROW = +4                                             EL320
03636              GO TO 2212-ROWS-EXIT.                                EL320
03637                                                                   EL320
03638      IF CF-SEQUENCE-NO = 1                                        EL320
03639          IF XROW = +1                                             EL320
03640              MOVE 'MORTGAGE APPLICATIONS'                         EL320
03641                                  TO PROC-HB6-APPL-DESC            EL320
03642            ELSE                                                   EL320
03643          IF XROW = +2                                             EL320
03644              GO TO 2212-ROWS-EXIT                                 EL320
03645            ELSE                                                   EL320
03646          IF XROW = +3                                             EL320
03647              GO TO 2212-ROWS-EXIT                                 EL320
03648            ELSE                                                   EL320
03649          IF XROW = +4                                             EL320
03650              GO TO 2212-ROWS-EXIT.                                EL320
03651                                                                   EL320
03652      PERFORM  1200-HEADING  THRU  1200-EXIT.                      EL320
03653                                                                   EL320
03654      MOVE DS                     TO  X.                           EL320
03655      MOVE PROC-HEAD-B1           TO  P-DATA.                      EL320
03656      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03657                                                                   EL320
03658      MOVE SS                     TO  X.                           EL320
03659      MOVE PROC-HEAD-B2           TO  P-DATA.                      EL320
03660      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03661                                                                   EL320
03662      MOVE TS                     TO  X.                           EL320
03663      MOVE PROC-HEAD-B3           TO  P-DATA.                      EL320
03664      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03665                                                                   EL320
03666      MOVE DS                     TO  X.                           EL320
03667      MOVE PROC-HEAD-B4           TO  P-DATA.                      EL320
03668      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03669                                                                   EL320
03670      MOVE SS                     TO  X.                           EL320
03671      MOVE PROC-HEAD-B5           TO  P-DATA.                      EL320
03672      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03673                                                                   EL320
03674      MOVE TS                     TO  X.                           EL320
03675      MOVE PROC-HEAD-B6           TO  P-DATA.                      EL320
03676      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03677                                                                   EL320
03678      MOVE DS                     TO  X.                           EL320
03679      COMPUTE XSEQ = CF-SEQUENCE-NO + 1.                           EL320
03680                                                                   EL320
03681      PERFORM 2212-SECURITY-TABLE-COLUMNS THRU 2212-COLUMNS-EXIT   EL320
03682        VARYING XCOL FROM +1 BY +1                                 EL320
03683          UNTIL XCOL > +44.                                        EL320
03684                                                                   EL320
03685  2212-ROWS-EXIT.                                                  EL320
03686      EXIT.                                                        EL320
03687                                                                   EL320
03688      EJECT                                                        EL320
03689  2212-SECURITY-TABLE-COLUMNS.                                     EL320
03690                                                                   EL320
03691      IF TBL-APPL-NBR (XSEQ, XROW, XCOL) = '**'                    EL320
03692          GO TO 2212-COLUMNS-EXIT.                                 EL320
03693                                                                   EL320
03694      MOVE TBL-APPL-NBR  (XSEQ, XROW, XCOL)                        EL320
03695                                      TO PROC-DB-APPL-NBR.         EL320
03696                                                                   EL320
03697      IF CF-APP-SWITCHES (XROW, XCOL) EQUAL 'YY'                   EL320
03698          MOVE 'UPDATE'               TO PROC-DB-APPL-ACCESS       EL320
03699      ELSE                                                         EL320
03700      IF CF-APP-SWITCHES (XROW, XCOL) EQUAL 'YN'                   EL320
03701          MOVE 'DISPLAY'              TO PROC-DB-APPL-ACCESS       EL320
03702      ELSE                                                         EL320
03703      IF CF-APP-SWITCHES (XROW, XCOL) EQUAL 'NN'                   EL320
03704          MOVE 'NOT AUTHORIZED'       TO PROC-DB-APPL-ACCESS       EL320
03705      ELSE                                                         EL320
03706          MOVE 'INVALID (NY)'         TO PROC-DB-APPL-ACCESS.      EL320
03707                                                                   EL320
03708      MOVE TBL-APPL-DESC (XSEQ, XROW, XCOL)                        EL320
03709                                      TO PROC-DB-APPL-DESC.        EL320
03710                                                                   EL320
03711      MOVE PROC-DETAIL-B          TO  P-DATA.                      EL320
03712      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03713      MOVE SS                     TO  X.                           EL320
03714                                                                   EL320
03715  2212-COLUMNS-EXIT.                                               EL320
03716      EXIT.                                                        EL320
03717                                                                   EL320
03718  2212-EXIT.                                                       EL320
03719      EXIT.                                                        EL320
03720      EJECT                                                        EL320
03721  2213-PROCESS-CARR-RECORD.                                        EL320
03722                                                                   EL320
03723      PERFORM  1200-HEADING  THRU  1200-EXIT.                      EL320
03724                                                                   EL320
03725      MOVE CF-RECORD-TYPE         TO  WS-RECORD-TYPE.              EL320
03726      MOVE CF-LAST-MAINT-BY       TO  CARR-MAINT-BY.               EL320
03727      MOVE CF-LAST-MAINT-DT       TO  DC-BIN-DATE-1.               EL320
03728      MOVE ' '                    TO  DC-OPTION-CODE.              EL320
03729                                                                   EL320
03730      PERFORM  1100-DATE-RTN  THRU  1100-EXIT.                     EL320
03731                                                                   EL320
03732      MOVE DC-GREG-DATE-1-EDIT    TO  CARR-MAINT-DT.               EL320
03733      MOVE CF-LAST-MAINT-HHMMSS   TO  WS-MAINT-TIME.               EL320
03734      MOVE WS-MAINT-HH            TO  CARR-MAINT-HH.               EL320
03735      MOVE WS-MAINT-MM            TO  CARR-MAINT-MM.               EL320
03736      MOVE WS-MAINT-SS            TO  CARR-MAINT-SS.               EL320
03737      MOVE CF-CARRIER-CNTL        TO  CARR-NO.                     EL320
03738      MOVE CF-MAIL-TO-NAME        TO  CARR-NAME.                   EL320
03739      MOVE CF-IN-CARE-OF          TO  CARR-IN-CARE.                EL320
03740      MOVE CF-ADDRESS-LINE-1      TO  CARR-ADDRESS-1.              EL320
03741      MOVE CF-ADDRESS-LINE-2      TO  CARR-ADDRESS-2.              EL320
03742      MOVE CF-CITY-STATE          TO  CARR-CITY-STATE.             EL320
03743      MOVE DS                     TO  X.                           EL320
03744      MOVE BLANKLINE              TO  P-DATA                       EL320
03745                                                                   EL320
03746      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03747                                                                   EL320
03748      MOVE DS                     TO  X.                           EL320
03749      MOVE CARR-LINE-1            TO  P-DATA.                      EL320
03750                                                                   EL320
03751      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03752                                                                   EL320
03753      MOVE SS                     TO  X.                           EL320
03754      MOVE CARR-LINE-2            TO  P-DATA.                      EL320
03755                                                                   EL320
03756      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03757                                                                   EL320
03758      MOVE TS                     TO  X.                           EL320
03759      MOVE CARR-LINE-3            TO  P-DATA.                      EL320
03760                                                                   EL320
03761      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03762                                                                   EL320
03763      IF CF-ZIP-CODE-NUM NOT NUMERIC                               EL320
03764          MOVE ZEROS              TO CF-ZIP-CODE-NUM.              EL320
03765      IF CF-ZIP-CODE-NUM NOT = ZEROS                               EL320
03766          MOVE CF-ZIP-CODE-NUM    TO WS-ZIP-CODE                   EL320
03767          MOVE WS-ZIP-CODE-X      TO CF-ZIP-CODE.                  EL320
03768                                                                   EL320
03769      MOVE CF-ZIP-CODE            TO  CARR-ZIP.                    EL320
03770      MOVE CF-PHONE-NO            TO  WS-PHONE.                    EL320
03771      MOVE WS-PHONE-AC            TO  CARR-PHONE-AC.               EL320
03772      MOVE WS-PHONE-PF            TO  CARR-PHONE-PF.               EL320
03773      MOVE WS-PHONE-NO            TO  CARR-PHONE-NO.               EL320
03774      MOVE CF-DOMICILE-STATE      TO  CARR-DOM-ST.                 EL320
03775      MOVE CF-CLAIM-COUNTER       TO  CARR-CLAIM-NO.               EL320
03776      MOVE CF-CHECK-COUNTER       TO  CARR-CHECK-NO.               EL320
03777                                                                   EL320
03778      MOVE SPACES                 TO  CARR-DESC-12.                EL320
03779      MOVE CF-CLAIM-NO-METHOD     TO  CARR-CLAIM-NO-ASS.           EL320
03780                                                                   EL320
03781      IF CLAIM-NO-MANUAL                                           EL320
03782          MOVE 'CLAIM NUMBER MANUAL  '  TO  CARR-DESC-12.          EL320
03783                                                                   EL320
03784      IF CLAIM-NO-Y-M-SEQ                                          EL320
03785          MOVE 'CLAIM NUMBER Y-M SEQ.'  TO  CARR-DESC-12.          EL320
03786                                                                   EL320
03787      IF CLAIM-NO-SEQ                                              EL320
03788          MOVE 'CLAIM NUMBER SEQUENCE'  TO  CARR-DESC-12.          EL320
03789                                                                   EL320
03790      MOVE SPACES                 TO  CARR-DESC-13.                EL320
03791      MOVE CF-CHECK-NO-METHOD     TO  CARR-CHECK-NO-ASS.           EL320
03792                                                                   EL320
03793      IF CHECK-NO-MANUAL                                           EL320
03794          MOVE 'CHECK NUMBER MANUAL  '  TO  CARR-DESC-13.          EL320
03795                                                                   EL320
03796      IF CHECK-NO-AUTO-SEQ                                         EL320
03797          MOVE 'CHECK NUMBER AUTO SEQ'  TO  CARR-DESC-13.          EL320
03798                                                                   EL320
03799      IF CHECK-NO-CARR-SEQ                                         EL320
03800          MOVE 'CHECK NUMBER CARR SEQ'  TO  CARR-DESC-13.          EL320
03801                                                                   EL320
03802      IF CHECK-NO-AT-PRINT                                         EL320
03803          MOVE 'CHECK NUMBER AT PRINT'  TO  CARR-DESC-13.          EL320
03804                                                                   EL320
03805      MOVE SPACES                 TO  CARR-DESC-14.                EL320
03806      MOVE CF-EXPENSE-METHOD      TO  CARR-CALC-TYPE.              EL320
03807                                                                   EL320
03808      IF EXPENSE-CALC-MANUAL                                       EL320
03809          MOVE 'EXPENSE CALC MANUAL  '  TO  CARR-DESC-14.          EL320
03810                                                                   EL320
03811      IF DOLLARS-PER-PMT                                           EL320
03812          MOVE 'DOLLAR PER PAYMENT   '  TO  CARR-DESC-14.          EL320
03813                                                                   EL320
03814      IF PERCENT-OF-PAYMENT                                        EL320
03815          MOVE 'PERCENT OF PAYMENT   '  TO  CARR-DESC-14.          EL320
03816                                                                   EL320
03817      IF DOLLARS-PER-MONTH                                         EL320
03818          MOVE 'DOLLARS PER MONTH    '  TO  CARR-DESC-14.          EL320
03819                                                                   EL320
03820      MOVE CF-EXPENSE-PERCENT     TO  CARR-CALC-PERCT.             EL320
03821      MOVE CF-EXPENSE-DOLLAR      TO  CARR-CALC-AMT.               EL320
03822      MOVE SPACES                 TO  CARR-DESC-17.                EL320
03823      MOVE CF-CLAIM-CALC-METHOD   TO  CARR-CALC-METHOD.            EL320
03824                                                                   EL320
03825      IF CF-CLAIM-CALC-METHOD  EQUAL TO  '1'                       EL320
03826          MOVE '365 PLUS MONTHS      '  TO  CARR-DESC-17.          EL320
03827                                                                   EL320
03828      IF CF-CLAIM-CALC-METHOD  EQUAL TO  '2'                       EL320
03829          MOVE '360 PLUS MONTHS      '  TO  CARR-DESC-17.          EL320
03830                                                                   EL320
03831      IF CF-CLAIM-CALC-METHOD  EQUAL TO  '3'                       EL320
03832          MOVE 'FULL MONTHS          '  TO  CARR-DESC-17.          EL320
03833                                                                   EL320
03834      IF CF-CLAIM-CALC-METHOD  EQUAL TO  '4'                       EL320
03835          MOVE '360 DAY YEAR         '  TO  CARR-DESC-17.          EL320
03836                                                                   EL320
03837      IF CF-CLAIM-CALC-METHOD  EQUAL TO  '5'                       EL320
03838          MOVE '365 DAY YEAR         '  TO  CARR-DESC-17.          EL320
03839                                                                   EL320
03840      IF CF-MANUAL-RESERVES-USED                                   EL320
03841          MOVE WS-YES             TO  CARR-CLAIM-MAN               EL320
03842      ELSE                                                         EL320
03843          MOVE WS-NO              TO  CARR-CLAIM-MAN.              EL320
03844                                                                   EL320
03845      IF CF-IBNR-RESERVES-USED                                     EL320
03846          MOVE WS-YES             TO  CARR-CLAIM-IBNR              EL320
03847      ELSE                                                         EL320
03848          MOVE WS-NO              TO  CARR-CLAIM-IBNR.             EL320
03849                                                                   EL320
03850      IF CF-PAY-TO-CURRENT-USED                                    EL320
03851          MOVE WS-YES             TO  CARR-CLAIM-PTC               EL320
03852      ELSE                                                         EL320
03853          MOVE WS-NO              TO  CARR-CLAIM-PTC.              EL320
03854                                                                   EL320
03855      IF CF-FUTURE-RESERVES-USED                                   EL320
03856          MOVE WS-YES             TO  CARR-CLAIM-FUT               EL320
03857      ELSE                                                         EL320
03858          MOVE WS-NO              TO  CARR-CLAIM-FUT.              EL320
03859                                                                   EL320
03860      MOVE CF-PERCENT-OF-CDT      TO  CARR-CDT-PCNT.               EL320
03861      MOVE CF-IBNR-PERCENT        TO  CARR-IBNR-PCNT.              EL320
03862      MOVE CF-CALC-DAYS-TOL       TO  CARR-LIMIT-25.               EL320
03863      MOVE CF-CALC-AMT-TOL        TO  CARR-LIMIT-26.               EL320
03864      MOVE CF-MAX-REG-DAYS        TO  CARR-LIMIT-27.               EL320
03865      MOVE CF-MAX-REG-PMT         TO  CARR-LIMIT-28.               EL320
03866      MOVE CF-MAX-AUTO-MOS        TO  CARR-LIMIT-29.               EL320
03867      MOVE CF-MAX-AUTO-PMT        TO  CARR-LIMIT-30.               EL320
03868      MOVE CF-DAYS-BEFORE-CLOSED  TO  CARR-LIMIT-31.               EL320
03869      MOVE CF-MONTHS-BEFORE-PURGED                                 EL320
03870                                  TO  CARR-LIMIT-32.               EL320
03871      MOVE SS                     TO  X.                           EL320
03872      MOVE CARR-LINE-4            TO  P-DATA.                      EL320
03873                                                                   EL320
03874      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03875                                                                   EL320
03876      MOVE SS                     TO  X.                           EL320
03877      MOVE CARR-LINE-5            TO  P-DATA.                      EL320
03878                                                                   EL320
03879      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03880                                                                   EL320
03881      MOVE SS                     TO  X.                           EL320
03882      MOVE CARR-LINE-6            TO  P-DATA.                      EL320
03883                                                                   EL320
03884      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03885                                                                   EL320
03886      MOVE SS                     TO  X.                           EL320
03887      MOVE CARR-LINE-7            TO  P-DATA.                      EL320
03888                                                                   EL320
03889      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03890                                                                   EL320
03891      MOVE SS                     TO  X.                           EL320
03892      MOVE CARR-LINE-8            TO  P-DATA.                      EL320
03893                                                                   EL320
03894      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03895                                                                   EL320
03896      MOVE SS                     TO  X.                           EL320
03897      MOVE CARR-LINE-9            TO  P-DATA.                      EL320
03898                                                                   EL320
03899      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03900                                                                   EL320
03901      MOVE SS                     TO  X.                           EL320
03902      MOVE CARR-LINE-10           TO  P-DATA.                      EL320
03903                                                                   EL320
03904      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03905                                                                   EL320
03906      MOVE TS                     TO  X.                           EL320
03907      MOVE CARR-LINE-11           TO  P-DATA.                      EL320
03908                                                                   EL320
03909      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03910                                                                   EL320
03911      MOVE DS                     TO  X.                           EL320
03912      MOVE CARR-LINE-12           TO  P-DATA.                      EL320
03913                                                                   EL320
03914      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03915                                                                   EL320
03916      MOVE SS                     TO  X.                           EL320
03917      MOVE CARR-LINE-13           TO  P-DATA.                      EL320
03918                                                                   EL320
03919      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03920                                                                   EL320
03921      MOVE DS                     TO  X.                           EL320
03922      MOVE CARR-LINE-14           TO  P-DATA.                      EL320
03923                                                                   EL320
03924      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03925                                                                   EL320
03926      MOVE SS                     TO  X.                           EL320
03927      MOVE CARR-LINE-15           TO  P-DATA.                      EL320
03928                                                                   EL320
03929      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03930                                                                   EL320
03931      MOVE SS                     TO  X.                           EL320
03932      MOVE CARR-LINE-16           TO  P-DATA.                      EL320
03933                                                                   EL320
03934      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03935                                                                   EL320
03936      MOVE DS                     TO  X.                           EL320
03937      MOVE CARR-LINE-17           TO  P-DATA.                      EL320
03938                                                                   EL320
03939      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03940                                                                   EL320
03941      MOVE DS                     TO  X.                           EL320
03942      MOVE CARR-LINE-18           TO  P-DATA.                      EL320
03943                                                                   EL320
03944      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03945                                                                   EL320
03946      MOVE SS                     TO  X.                           EL320
03947      MOVE CARR-LINE-19           TO  P-DATA.                      EL320
03948                                                                   EL320
03949      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03950                                                                   EL320
03951      MOVE SS                     TO  X.                           EL320
03952      MOVE CARR-LINE-20           TO  P-DATA.                      EL320
03953                                                                   EL320
03954      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03955                                                                   EL320
03956      MOVE SS                     TO  X.                           EL320
03957      MOVE CARR-LINE-21           TO  P-DATA.                      EL320
03958                                                                   EL320
03959      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03960                                                                   EL320
03961      MOVE SS                     TO  X.                           EL320
03962      MOVE CARR-LINE-22           TO  P-DATA.                      EL320
03963                                                                   EL320
03964      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03965                                                                   EL320
03966      MOVE SS                     TO  X.                           EL320
03967      MOVE CARR-LINE-23           TO  P-DATA.                      EL320
03968                                                                   EL320
03969      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03970                                                                   EL320
03971      MOVE TS                     TO  X.                           EL320
03972      MOVE CARR-LINE-24           TO  P-DATA.                      EL320
03973                                                                   EL320
03974      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03975                                                                   EL320
03976      MOVE DS                     TO  X.                           EL320
03977      MOVE CARR-LINE-25           TO  P-DATA.                      EL320
03978                                                                   EL320
03979      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03980                                                                   EL320
03981      MOVE SS                     TO  X.                           EL320
03982      MOVE CARR-LINE-26           TO  P-DATA.                      EL320
03983                                                                   EL320
03984      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03985                                                                   EL320
03986      MOVE SS                     TO  X.                           EL320
03987      MOVE CARR-LINE-27           TO  P-DATA.                      EL320
03988                                                                   EL320
03989      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03990                                                                   EL320
03991      MOVE SS                     TO  X.                           EL320
03992      MOVE CARR-LINE-28           TO  P-DATA.                      EL320
03993                                                                   EL320
03994      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
03995                                                                   EL320
03996      MOVE SS                     TO  X.                           EL320
03997      MOVE CARR-LINE-29           TO  P-DATA.                      EL320
03998                                                                   EL320
03999      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
04000                                                                   EL320
04001      MOVE SS                     TO  X.                           EL320
04002      MOVE CARR-LINE-30           TO  P-DATA.                      EL320
04003                                                                   EL320
04004      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
04005                                                                   EL320
04006      MOVE SS                     TO  X.                           EL320
04007      MOVE CARR-LINE-31           TO  P-DATA.                      EL320
04008                                                                   EL320
04009      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
04010                                                                   EL320
04011      MOVE SS                     TO  X.                           EL320
04012      MOVE CARR-LINE-32           TO  P-DATA.                      EL320
04013                                                                   EL320
04014      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
04015                                                                   EL320
04016  2213-EXIT.                                                       EL320
04017      EXIT.                                                        EL320
04018      EJECT                                                        EL320
04019  2214-PROCESS-LF-BNFT-RECORD.                                     EL320
04020                                                                   EL320
04021      IF CF-RECORD-TYPE  NOT EQUAL  WS-RECORD-TYPE                 EL320
04022        OR  WS-LINE-CNT  GREATER THAN  55                          EL320
04023          PERFORM  1200-HEADING  THRU  1200-EXIT                   EL320
04024          MOVE CF-RECORD-TYPE     TO  WS-RECORD-TYPE               EL320
04025          PERFORM  2214-HEADING  THRU  2214-HEAD-EXIT.             EL320
04026                                                                   EL320
04027      MOVE  1                     TO  LB-SUB.                      EL320
04028                                                                   EL320
04029      PERFORM  2214-MOVE-ARRAY  THRU  2214-MOVE-EXIT  8  TIMES.    EL320
04030                                                                   EL320
04031  2214-EXIT.                                                       EL320
04032      EXIT.                                                        EL320
04033                                                                   EL320
04034  2214-MOVE-ARRAY.                                                 EL320
04035                                                                   EL320
04036      IF CF-BENEFIT-ALPHA (LB-SUB)  EQUAL  SPACES                  EL320
04037          GO TO 2214-MOVE-EXIT.                                    EL320
04038                                                                   EL320
04039      MOVE CF-BENEFIT-CODE    (LB-SUB)  TO  BNFT-CODE.             EL320
04040      MOVE CF-BENEFIT-ALPHA   (LB-SUB)  TO  BNFT-ABREV.            EL320
04041      MOVE CF-BENEFIT-DESCRIP (LB-SUB)  TO  BNFT-DESC.             EL320
04042      MOVE CF-BENEFIT-COMMENT (LB-SUB)  TO  BNFT-COMMENT.          EL320
04043      MOVE CF-LOAN-TYPE       (LB-SUB)  TO  BNFT-TYPE.             EL320
04044      MOVE CF-JOINT-INDICATOR (LB-SUB)  TO  BNFT-JT.               EL320
04045      MOVE CF-SPECIAL-CALC-CD (LB-SUB)  TO  BNFT-SP.               EL320
04046      MOVE CF-LF-COVERAGE-TYPE (LB-SUB) TO  BNFT-LR.               EL320
04047      MOVE CF-CO-REM-TERM-CALC (LB-SUB) TO  BNFT-RT.               EL320
04048      MOVE CF-CO-OVRD-EARNINGS-CALC (LB-SUB)  TO  BNFT-RM.         EL320
04049      MOVE CF-CO-BEN-I-G-CD   (LB-SUB)  TO  BNFT-IG.               EL320
04050                                                                   EL320
04051      MOVE SPACES                       TO  BNFT-EARN.             EL320
04052                                                                   EL320
04053      IF CO-EARN-BY-R78 (LB-SUB)                                   EL320
04054          MOVE 'RULE 78'          TO  BNFT-EARN.                   EL320
04055                                                                   EL320
04056      IF CO-EARN-AS-CALIF (LB-SUB)                                 EL320
04057          MOVE 'CALIF  '          TO  BNFT-EARN.                   EL320
04058                                                                   EL320
04059      IF CO-EARN-BY-PRO-RATA (LB-SUB)                              EL320
04060          MOVE 'PRO RATA'         TO  BNFT-EARN.                   EL320
04061                                                                   EL320
04062      IF CO-EARN-AS-TEXAS (LB-SUB)                                 EL320
04063          MOVE 'IRREG   '         TO  BNFT-EARN.                   EL320
04064                                                                   EL320
04065      IF CO-EARN-IS-NET-PAY (LB-SUB)                               EL320
04066          MOVE 'NET-PAY '         TO  BNFT-EARN.                   EL320
04067                                                                   EL320
04068      IF CO-EARN-ANTICIPATION (LB-SUB)                             EL320
04069          MOVE 'ANTICIP '         TO  BNFT-EARN.                   EL320
04070                                                                   EL320
04071      IF CO-EARN-AS-MEAN (LB-SUB)                                  EL320
04072          MOVE 'MEAN    '         TO  BNFT-EARN.                   EL320
04073                                                                   EL320
04074      IF CO-EARN-AS-REG-BALLOON (LB-SUB)                           EL320
04075          MOVE 'BALLOON '         TO  BNFT-EARN.                   EL320
04076                                                                   EL320
04077      MOVE SPACES                 TO  BNFT-OB.                     EL320
04078                                                                   EL320
04079      IF CF-SPECIAL-CALC-CD (LB-SUB)  EQUAL  'O'                   EL320
04080          MOVE WS-YES             TO  BNFT-OB                      EL320
04081      ELSE                                                         EL320
04082          MOVE WS-NO              TO  BNFT-OB.                     EL320
04083                                                                   EL320
04084      MOVE SS                     TO  X.                           EL320
04085      MOVE BNFT-DETAIL            TO  P-DATA.                      EL320
04086                                                                   EL320
04087      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
04088                                                                   EL320
04089      ADD 1                       TO  WS-LINE-CNT.                 EL320
04090      ADD 1                       TO  LB-SUB.                      EL320
04091                                                                   EL320
04092      IF WS-LINE-CNT  GREATER THAN  55                             EL320
04093          PERFORM  1200-HEADING  THRU  1200-EXIT                   EL320
04094          PERFORM  2214-HEADING  THRU  2214-HEAD-EXIT.             EL320
04095                                                                   EL320
04096  2214-MOVE-EXIT.                                                  EL320
04097      EXIT.                                                        EL320
04098                                                                      CL**3
04099  2214-HEADING.                                                    EL320
04100                                                                   EL320
04101      MOVE WS-HOLD-LF-L6          TO  BNFT-REC-DESC.               EL320
04102      MOVE ' BENEFITS'            TO  BNFT-REC-TYPE.               EL320
04103      MOVE TS                     TO  X.                           EL320
04104      MOVE BNFT-LINE-1            TO  P-DATA.                      EL320
04105                                                                   EL320
04106      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
04107                                                                   EL320
04108      MOVE DS                     TO  X.                           EL320
04109      MOVE BNFT-LINE-2            TO  P-DATA.                      EL320
04110                                                                   EL320
04111      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
04112                                                                   EL320
04113      MOVE SS                     TO  X.                           EL320
04114      MOVE BNFT-LINE-3            TO  P-DATA.                      EL320
04115                                                                   EL320
04116      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
04117                                                                   EL320
04118      MOVE SS                     TO  X.                           EL320
04119      MOVE SPACES                 TO  P-DATA.                      EL320
04120                                                                   EL320
04121      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
04122                                                                   EL320
04123      MOVE 10                     TO  WS-LINE-CNT.                 EL320
04124                                                                   EL320
04125  2214-HEAD-EXIT.                                                  EL320
04126      EXIT.                                                        EL320
04127      EJECT                                                        EL320
04128                                                                   EL320
04129  2215-PROCESS-AH-BNFT-RECORD.                                     EL320
04130                                                                   EL320
04131      IF CF-RECORD-TYPE  NOT EQUAL  WS-RECORD-TYPE                 EL320
04132        OR  WS-LINE-CNT  GREATER THAN  55                          EL320
04133          PERFORM  1200-HEADING  THRU  1200-EXIT                   EL320
04134          MOVE CF-RECORD-TYPE     TO  WS-RECORD-TYPE               EL320
04135          PERFORM  2215-HEADING  THRU  2215-HEAD-EXIT.             EL320
04136                                                                   EL320
04137      MOVE  1                     TO  AB-SUB.                      EL320
04138                                                                   EL320
04139      PERFORM 2215-MOVE-ARRAY THRU 2215-MOVE-EXIT 8 TIMES.         EL320
04140                                                                   EL320
04141  2215-EXIT.                                                       EL320
04142      EXIT.                                                        EL320
04143                                                                   EL320
04144  2215-MOVE-ARRAY.                                                 EL320
04145                                                                   EL320
04146      IF CF-BENEFIT-ALPHA (AB-SUB)  EQUAL  SPACES                  EL320
04147          ADD 1                   TO  AB-SUB                       EL320
04148          GO TO 2215-MOVE-EXIT.                                    EL320
04149                                                                   EL320
04150      MOVE CF-BENEFIT-CODE    (AB-SUB)  TO  BNFT-CODE-A.           EL320
04151      MOVE CF-BENEFIT-ALPHA   (AB-SUB)  TO  BNFT-ABREV-A.          EL320
04152      MOVE CF-BENEFIT-DESCRIP (AB-SUB)  TO  BNFT-DESC-A.           EL320
04153      MOVE CF-BENEFIT-COMMENT (AB-SUB)  TO  BNFT-COMMENT-A.        EL320
04154      MOVE CF-LOAN-TYPE       (AB-SUB)  TO  BNFT-TYPE-A.           EL320
04155      MOVE CF-SPECIAL-CALC-CD (AB-SUB)  TO  BNFT-SP-A.             EL320
04156      MOVE CF-CO-OVRD-EARNINGS-CALC (AB-SUB)  TO  BNFT-RM-A.       EL320
04157      MOVE CF-CO-BEN-I-G-CD   (AB-SUB)  TO  BNFT-IG-A.             EL320
04158      MOVE SPACES                       TO  BNFT-EARN-A.           EL320
04159                                                                   EL320
04160      IF CF-CO-EARNINGS-CALC (AB-SUB)  EQUAL  '1'                  EL320
04161          MOVE 'RULE 78'          TO  BNFT-EARN-A.                 EL320
04162                                                                   EL320
04163      IF CF-CO-EARNINGS-CALC (AB-SUB)  EQUAL  '2'                  EL320
04164          MOVE 'PRO RATA'         TO  BNFT-EARN-A.                 EL320
04165                                                                   EL320
04166      IF CF-CO-EARNINGS-CALC (AB-SUB)  EQUAL  '3'                  EL320
04167          MOVE 'CALIF   '         TO  BNFT-EARN-A.                 EL320
04168                                                                   EL320
04169      IF CF-CO-EARNINGS-CALC (AB-SUB)  EQUAL  '4'                  EL320
04170          MOVE 'IRREG   '         TO  BNFT-EARN-A.                 EL320
04171                                                                   EL320
04172      IF CF-CO-EARNINGS-CALC (AB-SUB)  EQUAL  '5'                  EL320
04173          MOVE 'NET-PAY '         TO  BNFT-EARN-A.                 EL320
04174                                                                   EL320
04175      IF CF-CO-EARNINGS-CALC (AB-SUB)  EQUAL  '6'                  EL320
04176          MOVE 'ANTICIP '         TO  BNFT-EARN-A.                 EL320
04177                                                                   EL320
04178      IF CF-CO-EARNINGS-CALC (AB-SUB)  EQUAL  '7'                  EL320
04179          MOVE SPACES             TO  BNFT-OB-A.                   EL320
04180                                                                   EL320
04181      IF CF-CO-EARNINGS-CALC (AB-SUB)  EQUAL  '8'                  EL320
04182          MOVE 'MEAN    '         TO  BNFT-EARN-A.                 EL320
04183                                                                   EL320
04184      IF CF-CO-EARNINGS-CALC (AB-SUB)  EQUAL  'B'                  EL320
04185          MOVE 'BALLOON '         TO  BNFT-EARN-A.                 EL320
04186                                                                   EL320
04187      IF CF-SPECIAL-CALC-CD (AB-SUB)  EQUAL  'O'                   EL320
04188          MOVE WS-YES             TO  BNFT-OB-A                    EL320
04189      ELSE                                                         EL320
04190          MOVE WS-NO              TO  BNFT-OB-A.                   EL320
04191                                                                   EL320
04192      MOVE SPACES                 TO  BNFT-REM-A.                  EL320
04193                                                                   EL320
04194      IF CF-CO-REM-TERM-CALC (AB-SUB)  EQUAL  '1'                  EL320
04195          MOVE 'AFTER 15'         TO  BNFT-REM-A.                  EL320
04196                                                                   EL320
04197      IF CF-CO-REM-TERM-CALC (AB-SUB)  EQUAL  '2'                  EL320
04198          MOVE 'HALF MO '         TO  BNFT-REM-A.                  EL320
04199                                                                   EL320
04200      IF CF-CO-REM-TERM-CALC (AB-SUB)  EQUAL  '3'                  EL320
04201          MOVE '1ST DAY '         TO  BNFT-REM-A.                  EL320
04202                                                                   EL320
04203      IF CF-CO-REM-TERM-CALC (AB-SUB)  EQUAL  '4'                  EL320
04204          MOVE 'FULL MO '         TO  BNFT-REM-A.                  EL320
04205                                                                   EL320
04206      MOVE SS                     TO  X.                           EL320
04207      MOVE BNFT-DETAIL-A          TO  P-DATA.                      EL320
04208                                                                   EL320
04209      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
04210                                                                   EL320
04211      ADD 1                       TO  WS-LINE-CNT.                 EL320
04212      ADD 1                       TO  AB-SUB.                      EL320
04213                                                                   EL320
04214      IF WS-LINE-CNT  GREATER THAN  55                             EL320
04215          PERFORM  1200-HEADING                                    EL320
04216          PERFORM  2215-HEADING.                                   EL320
04217                                                                   EL320
04218  2215-MOVE-EXIT.                                                  EL320
04219      EXIT.                                                        EL320
04220                                                                   EL320
04221  2215-HEADING.                                                    EL320
04222                                                                   EL320
04223      MOVE WS-HOLD-AH-L6          TO  BNFT-REC-DESC.               EL320
04224      MOVE ' BENEFITS'            TO  BNFT-REC-TYPE.               EL320
04225      MOVE TS                     TO  X.                           EL320
04226      MOVE BNFT-LINE-1            TO  P-DATA.                      EL320
04227                                                                   EL320
04228      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
04229                                                                   EL320
04230      MOVE DS                     TO  X.                           EL320
04231      MOVE BNFT-LINE-2-A          TO  P-DATA.                      EL320
04232                                                                   EL320
04233      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
04234                                                                   EL320
04235      MOVE SS                     TO  X.                           EL320
04236      MOVE BNFT-LINE-3-A          TO  P-DATA.                      EL320
04237                                                                   EL320
04238      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
04239                                                                   EL320
04240      MOVE SS                     TO  X.                           EL320
04241      MOVE SPACES                 TO  P-DATA.                      EL320
04242                                                                   EL320
04243      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
04244      MOVE 10                     TO  WS-LINE-CNT.                 EL320
04245                                                                   EL320
04246  2215-HEAD-EXIT.                                                  EL320
04247      EXIT.                                                        EL320
04248                                                                      CL**3
04249  2216-PROCESS-STAT-RECORD.                                        EL320
04250      IF FIRST-TIME-STATE                                          EL320
04251          PERFORM 2216A-GET-OPTIONS THRU 2216A-GET-EXIT.           EL320
04252                                                                   EL320
04253      IF WS-FORMAT-OPTION  EQUAL '1'                               EL320
04254          PERFORM 2216A-PROCESS-STAT-RECORD                        EL320
04255                                  THRU 2216A-EXIT                  EL320
04256          GO TO 2216-EXIT.                                         EL320
04257                                                                   EL320
04258      IF CF-RECORD-TYPE  NOT EQUAL  WS-RECORD-TYPE                 EL320
04259          PERFORM  1200-HEADING                                    EL320
04260          MOVE TS                 TO  X                            EL320
04261          PERFORM  2216-HEADING                                    EL320
04262          MOVE CF-RECORD-TYPE     TO  WS-RECORD-TYPE.              EL320
04263                                                                   EL320
04264      MOVE 1                      TO  ST-SUB.                      EL320
04265      MOVE ZERO                   TO  X                            EL320
04266      MOVE CF-STATE-CODE          TO  STAT-CODE                    EL320
04267      MOVE CF-STATE-ABBREVIATION  TO  STAT-ABREV                   EL320
04268                                      WS-STATE-SAVED               EL320
04269      MOVE CF-STATE-NAME          TO  STAT-NAME                    EL320
04270                                                                   EL320
04271      PERFORM  2216-MOVE-ARRAY  THRU  2216-MOVE-EXIT.              EL320
04272                                                                   EL320
04273  2216-EXIT.                                                       EL320
04274      EXIT.                                                        EL320
04275                                                                   EL320
04276  2216-MOVE-ARRAY.                                                 EL320
04277                                                                   EL320
04278      MOVE SPACES                     TO  STAT-BENEF.              EL320
04279      MOVE SPACES                     TO  STAT-FORM.               EL320
04280      MOVE SPACES                     TO  STAT-TYPE.               EL320
04281                                                                   EL320
04282  2216-MOVE-END.                                                   EL320
04283                                                                   EL320
04284      IF X  EQUAL TO  SPACES                                       EL320
04285          ADD +1                  TO  WS-LINE-CNT                  EL320
04286      ELSE                                                         EL320
04287          ADD +2                  TO  WS-LINE-CNT.                 EL320
04288                                                                   EL320
04289      IF WS-LINE-CNT  GREATER THAN  55                             EL320
04290          PERFORM  1200-HEADING  THRU  1200-EXIT                   EL320
04291          MOVE DS                 TO  X                            EL320
04292          MOVE '(CONTINUED...)'   TO  P-DATA                       EL320
04293          PERFORM  1300-PRINT-LINE  THRU  1300-EXIT                EL320
04294          MOVE SS                 TO  X                            EL320
04295          PERFORM  2216-HEADING  THRU  2216-HEAD-EXIT.             EL320
04296                                                                   EL320
04297      MOVE STAT-DETAIL            TO  P-DATA.                      EL320
04298                                                                   EL320
04299      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
04300                                                                   EL320
04301  2216-MOVE-EXIT.                                                  EL320
04302      EXIT.                                                        EL320
04303                                                                      CL**3
04304  2216-HEADING.                                                    EL320
04305                                                                   EL320
04306      MOVE STAT-LINE-1            TO  P-DATA.                      EL320
04307                                                                   EL320
04308      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
04309                                                                   EL320
04310      MOVE DS                     TO  X.                           EL320
04311      MOVE STAT-LINE-2            TO  P-DATA.                      EL320
04312                                                                   EL320
04313      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
04314                                                                   EL320
04315      MOVE 12                     TO  WS-LINE-CNT.                 EL320
04316                                                                   EL320
04317  2216-HEAD-EXIT.                                                  EL320
04318      EXIT.                                                        EL320
04319  2216A-PROCESS-STAT-RECORD.                                       EL320
04320      IF CF-RECORD-TYPE NOT EQUAL WS-RECORD-TYPE                   EL320
04321         PERFORM 1200-HEADING     THRU 1200-EXIT                   EL320
04322         MOVE TS                  TO X                             EL320
04323         PERFORM 2216A-HEADING    THRU 2216A-HEAD-EXIT             EL320
04324         MOVE CF-RECORD-TYPE      TO WS-RECORD-TYPE.               EL320
04325      MOVE 1                      TO ST-SUB.                       EL320
04326      MOVE ZERO                   TO X.                            EL320
04327      PERFORM 2216A-MOVE-ARRAY    THRU 2216A-MOVE-EXIT.            EL320
04328      PERFORM 2216A-PRT-RPT       THRU 2216A-PRT-EXIT.             EL320
04329                                                                   EL320
04330  2216A-EXIT.                                                      EL320
04331      EXIT.                                                        EL320
04332                                                                   EL320
04333  2216A-MOVE-ARRAY.                                                EL320
04334                                                                   EL320
04335      MOVE CF-STATE-CODE          TO ST-3-CODE.                    EL320
04336      MOVE CF-STATE-NAME          TO ST-3-NAME.                    EL320
04337                                                                   EL320
04338      MOVE CF-STATE-ABBREVIATION  TO ST-4-ABBREV.                  EL320
04339                                                                   EL320
04340      MOVE CF-ST-LF-EXP-PCT       TO ST-5-EXP-ALLOC-L.             EL320
04341      MOVE CF-ST-AH-EXP-PCT       TO ST-5-EXP-ALLOC-A.             EL320
04342                                                                   EL320
04343      MOVE CF-ST-TOL-CLAIM        TO ST-6-QTE-CLMS.                EL320
04344      MOVE CF-ST-TOL-PREM         TO ST-6-QTE-PREM.                EL320
04345      MOVE CF-ST-TOL-REFUND       TO ST-6-QTE-REFD.                EL320
04346                                                                   EL320
04347      MOVE CF-ST-CLAIM-REJECT-SW  TO ST-7-REJ-CLMS-SW.             EL320
04348      MOVE CF-ST-PREM-REJECT-SW   TO ST-7-REJ-PREM-SW.             EL320
04349      MOVE CF-ST-REF-REJECT-SW    TO ST-7-REJ-REFD-SW.             EL320
04350                                                                   EL320
04351      MOVE CF-ST-REFUND-MIN       TO ST-8-REF-MIN.                 EL320
04352      MOVE CF-ST-REFUND-DAYS-FIRST TO ST-8-REF-DAYS.               EL320
04353      MOVE CF-ST-REFUND-DAYS-SUBSEQ TO ST-8-REF-DAYS-SUB.          EL320
04354                                                                   EL320
04355      MOVE CF-ST-FST-PMT-DAYS-MAX TO ST-9-PMT-EXT-DAYS.            EL320
04356      MOVE CF-ST-FST-PMT-DAYS-CHG TO ST-9-PMT-EXT-CHRG.            EL320
04357                                                                   EL320
04358      MOVE CF-ST-CALL-UNEARNED    TO ST-10-CALL-UNERND.            EL320
04359      MOVE CF-ST-CALL-RPT-CNTL    TO ST-10-CALL-CNTL.              EL320
04360      MOVE CF-ST-CALL-RATE-DEV    TO ST-10-CALL-RTE-DEV.           EL320
04361                                                                   EL320
04362      PERFORM 2216A-MOVE-CNTL     THRU 2216A-CNTL-EXIT.            EL320
04363  2216A-MOVE-EXIT.                                                 EL320
04364      EXIT.                                                        EL320
04365                                                                   EL320
04366  2216A-MOVE-CNTL.                                                 EL320
04367                                                                   EL320
04368      MOVE CF-ST-BENEFIT-CD (ST-SUB)                               EL320
04369                                TO ST-12-BEN-CD (ST-SUB).          EL320
04370      MOVE CF-ST-BENEFIT-KIND (ST-SUB)                             EL320
04371                                TO ST-12-BEN-KND (ST-SUB).         EL320
04372      MOVE CF-ST-REFUND-CALC (ST-SUB)                              EL320
04373                                TO ST-12-BEN-REF (ST-SUB).         EL320
04374      MOVE CF-ST-REM-TERM-CALC (ST-SUB)                            EL320
04375                                TO ST-12-BEN-REM (ST-SUB).         EL320
04376      ADD 1                     TO ST-SUB.                         EL320
04377      IF ST-SUB LESS THAN 4                                        EL320
04378          GO TO 2216A-MOVE-CNTL.                                   EL320
04379                                                                   EL320
04380  2216A-CNTL-EXIT.                                                 EL320
04381      EXIT.                                                        EL320
04382                                                                   EL320
04383  2216A-HEADING.                                                   EL320
04384      MOVE STAT-LINE-1          TO P-DATA.                         EL320
04385      PERFORM 1300-PRINT-LINE   THRU 1300-EXIT.                    EL320
04386      MOVE 12                   TO WS-LINE-CNT.                    EL320
04387                                                                   EL320
04388  2216A-HEAD-EXIT.                                                 EL320
04389      EXIT.                                                        EL320
04390                                                                   EL320
04391  2216A-PRT-RPT.                                                   EL320
04392      IF WS-LINE-CNT  GREATER THAN  55                             EL320
04393          PERFORM 1200-HEADING  THRU 1200-EXIT                     EL320
04394          MOVE DS               TO X                               EL320
04395          MOVE '(CONTINUED...)' TO P-DATA                          EL320
04396          PERFORM 1300-PRINT-LINE THRU 1300-EXIT                   EL320
04397          MOVE SS               TO X                               EL320
04398          PERFORM 2216A-HEADING THRU 2216A-HEAD-EXIT.              EL320
04399                                                                   EL320
04400      MOVE TS                   TO X.                              EL320
04401      MOVE STAT-LINE-3          TO P-DATA.                         EL320
04402      PERFORM 1300-PRINT-LINE   THRU 1300-EXIT.                    EL320
04403      ADD 2                     TO WS-LINE-CNT.                    EL320
04404                                                                   EL320
04405      MOVE DS                   TO X.                              EL320
04406      MOVE STAT-LINE-4          TO P-DATA.                         EL320
04407      PERFORM 1300-PRINT-LINE   THRU 1300-EXIT.                    EL320
04408      ADD 2                     TO WS-LINE-CNT.                    EL320
04409                                                                   EL320
04410      MOVE DS                   TO X.                              EL320
04411      MOVE STAT-LINE-5          TO P-DATA.                         EL320
04412      PERFORM 1300-PRINT-LINE   THRU 1300-EXIT.                    EL320
04413      ADD 1                     TO WS-LINE-CNT.                    EL320
04414                                                                   EL320
04415      MOVE SS                   TO X.                              EL320
04416      MOVE STAT-LINE-6          TO P-DATA.                         EL320
04417      PERFORM 1300-PRINT-LINE   THRU 1300-EXIT.                    EL320
04418      ADD 1                     TO WS-LINE-CNT.                    EL320
04419                                                                   EL320
04420      MOVE SS                   TO X.                              EL320
04421      MOVE STAT-LINE-7          TO P-DATA.                         EL320
04422      PERFORM 1300-PRINT-LINE   THRU 1300-EXIT.                    EL320
04423      ADD 1                     TO WS-LINE-CNT.                    EL320
04424                                                                   EL320
04425      MOVE SS                   TO X.                              EL320
04426      MOVE STAT-LINE-8          TO P-DATA.                         EL320
04427      PERFORM 1300-PRINT-LINE   THRU 1300-EXIT.                    EL320
04428      ADD 1                     TO WS-LINE-CNT.                    EL320
04429                                                                   EL320
04430      MOVE SS                   TO X.                              EL320
04431      MOVE STAT-LINE-9          TO P-DATA.                         EL320
04432      PERFORM 1300-PRINT-LINE   THRU 1300-EXIT.                    EL320
04433      ADD 1                     TO WS-LINE-CNT.                    EL320
04434                                                                   EL320
04435      MOVE SS                   TO X.                              EL320
04436      MOVE STAT-LINE-10         TO P-DATA.                         EL320
04437      PERFORM 1300-PRINT-LINE   THRU 1300-EXIT.                    EL320
04438      ADD 2                     TO WS-LINE-CNT.                    EL320
04439                                                                   EL320
04440      MOVE TS                   TO X.                              EL320
04441      MOVE STAT-LINE-11         TO P-DATA.                         EL320
04442      PERFORM 1300-PRINT-LINE   THRU 1300-EXIT.                    EL320
04443      ADD 1                     TO WS-LINE-CNT.                    EL320
04444                                                                   EL320
04445      MOVE SS                   TO X.                              EL320
04446      MOVE STAT-LINE-12         TO P-DATA.                         EL320
04447      PERFORM 1300-PRINT-LINE   THRU 1300-EXIT.                    EL320
04448      ADD 6                     TO WS-LINE-CNT.                    EL320
04449                                                                   EL320
04450      MOVE TS                   TO X.                              EL320
04451      MOVE BLANKLINE            TO P-DATA.                         EL320
04452      PERFORM 1300-PRINT-LINE   THRU 1300-EXIT.                    EL320
04453                                                                   EL320
04454      ADD 21                    TO WS-LINE-CNT.                    EL320
04455                                                                   EL320
04456  2216A-PRT-EXIT.                                                  EL320
04457      EXIT.                                                        EL320
04458  2216A-GET-OPTIONS.                                               EL320
04459      MOVE DTE-CLASIC-COMPANY-CD  TO PS-COMPANY-CD.                EL320
04460      MOVE OLC-REPORT-NAME        TO PS-PROGRAM-NUMBER.            EL320
04461                                                                   EL320
04462      READ ELPGMSF.                                                EL320
04463                                                                   EL320
04464      IF ELPGMS-STATUS  = '23'                                     EL320
04465          MOVE SPACES             TO WS-PROGRAM-OPTIONS            EL320
04466      ELSE                                                         EL320
04467      IF ELPGMS-STATUS  NOT EQUAL  ZERO                            EL320
04468          MOVE 'ERROR OCCURED READ  - ELPGMS'                      EL320
04469                                  TO  WS-ABEND-MESSAGE             EL320
04470          MOVE ELPGMS-STATUS      TO  WS-ABEND-FILE-STATUS         EL320
04471          PERFORM  ABEND-PGM.                                      EL320
04472                                                                   EL320
04473      MOVE PS-PROGRAM-OPTIONS (1) TO WS-PROGRAM-OPTIONS.           EL320
04474      MOVE '1'                    TO WS-STAT-FIRST-SW.             EL320
04475                                                                      CL**3
04476  2216A-GET-EXIT.                                                  EL320
04477      EXIT.                                                        EL320
04478      EJECT                                                        EL320
04479  2217-PROCESS-MORT-RECORD.                                        EL320
04480                                                                   EL320
04481      IF  CF-SEQUENCE-NO EQUAL ZERO                                EL320
04482          MOVE CF-RECORD-TYPE         TO  WS-RECORD-TYPE           EL320
04483          MOVE CF-LAST-MAINT-BY       TO  MORT-MAINT-BY            EL320
04484          MOVE CF-LAST-MAINT-DT       TO  DC-BIN-DATE-1            EL320
04485          MOVE ' '                    TO  DC-OPTION-CODE           EL320
04486                                                                   EL320
04487          PERFORM  1100-DATE-RTN  THRU  1100-EXIT                  EL320
04488                                                                   EL320
04489          MOVE DC-GREG-DATE-1-EDIT    TO  MORT-MAINT-DT            EL320
04490          MOVE CF-LAST-MAINT-HHMMSS   TO  WS-MAINT-TIME            EL320
04491          MOVE WS-MAINT-HH            TO  MORT-MAINT-HH            EL320
04492          MOVE WS-MAINT-MM            TO  MORT-MAINT-MM            EL320
04493          MOVE WS-MAINT-SS            TO  MORT-MAINT-SS            EL320
04494      ELSE                                                         EL320
04495          IF  WS-LINE-CNT LESS THAN +55                            EL320
04496              GO TO 2217-BYPASS-HEADING.                           EL320
04497                                                                   EL320
04498  2217-BYPASS-INITIAL-SETUP.                                       EL320
04499                                                                   EL320
04500      PERFORM  1200-HEADING  THRU  1200-EXIT.                      EL320
04501                                                                   EL320
04502      MOVE DS                     TO  X.                           EL320
04503      MOVE BLANKLINE              TO  P-DATA                       EL320
04504                                                                   EL320
04505      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
04506                                                                   EL320
04507      MOVE DS                     TO  X.                           EL320
04508      MOVE MORT-LINE-1            TO  P-DATA.                      EL320
04509                                                                   EL320
04510      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
04511                                                                   EL320
04512      MOVE SS                     TO  X.                           EL320
04513      MOVE MORT-LINE-2            TO  P-DATA.                      EL320
04514                                                                   EL320
04515      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
04516                                                                   EL320
04517      MOVE TS                     TO  X.                           EL320
04518      MOVE MORT-LINE-3            TO  P-DATA.                      EL320
04519                                                                   EL320
04520      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
04521      MOVE DS                     TO  X.                           EL320
04522      ADD +7                      TO  WS-LINE-CNT.                 EL320
04523                                                                   EL320
04524  2217-BYPASS-HEADING.                                             EL320
04525                                                                   EL320
04526      IF MORT-SUB GREATER THAN +9                                  EL320
04527          MOVE +0                 TO  MORT-SUB.                    EL320
04528                                                                   EL320
04529  2217-MORT-LOOP.                                                  EL320
04530                                                                   EL320
04531      ADD +1                      TO  MORT-SUB.                    EL320
04532                                                                   EL320
04533      IF MORT-SUB GREATER THAN +9                                  EL320
04534          GO TO 2217-EXIT.                                         EL320
04535                                                                   EL320
04536      IF CF-MORT-TABLE (MORT-SUB) EQUAL LOW-VALUES                 EL320
04537          GO TO 2217-EXIT.                                         EL320
04538                                                                   EL320
04539      ADD 1                       TO  WS-LINE-CNT.                 EL320
04540                                                                   EL320
04541      IF  WS-LINE-CNT GREATER THAN +55                             EL320
04542          GO TO 2217-PROCESS-MORT-RECORD.                          EL320
04543                                                                   EL320
04544      MOVE CF-MORT-TABLE (MORT-SUB)                                EL320
04545                                  TO MORT-TABLE.                   EL320
04546      MOVE CF-MORT-TABLE-TYPE (MORT-SUB)                           EL320
04547                                  TO MORT-TYPE.                    EL320
04548      MOVE CF-MORT-INTEREST (MORT-SUB)                             EL320
04549                                  TO MORT-INTEREST.                EL320
04550      MOVE CF-MORT-AGE-METHOD (MORT-SUB)                           EL320
04551                                  TO MORT-AGE-METHOD.              EL320
04552      MOVE CF-MORT-RESERVE-ADJUSTMENT (MORT-SUB)                   EL320
04553                                  TO MORT-RESERVE-ADJ.             EL320
04554      MOVE CF-MORT-ADJUSTMENT-DIRECTION (MORT-SUB)                 EL320
04555                                  TO MORT-ADJ-DIRECTION.           EL320
04556      MOVE CF-MORT-JOINT-FACTOR (MORT-SUB)                         EL320
04557                                  TO MORT-JOINT-FACTOR.            EL320
04558      MOVE CF-MORT-JOINT-CODE (MORT-SUB)                           EL320
04559                                  TO MORT-JOINT-CODE.              EL320
04560      MOVE CF-MORT-PC-Q (MORT-SUB)                                 EL320
04561                                  TO MORT-PC-Q.                    EL320
04562      MOVE CF-MORT-TABLE-CODE (MORT-SUB)                           EL320
04563                                  TO MORT-MORT-CODE.               EL320
04564      MOVE SS                     TO  X.                           EL320
04565      MOVE MORT-DETAIL            TO  P-DATA.                      EL320
04566                                                                   EL320
04567      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
04568                                                                   EL320
04569      GO TO 2217-MORT-LOOP.                                        EL320
04570                                                                   EL320
04571  2217-EXIT.                                                       EL320
04572      EXIT.                                                        EL320
04573      EJECT                                                        EL320
04574  2218-PROCESS-BUSS-RECORD.                                        EL320
04575                                                                   EL320
04576      IF CF-RECORD-TYPE  NOT EQUAL  WS-RECORD-TYPE                 EL320
04577          SET TABLE-INDX          TO +1                            EL320
04578          PERFORM 2218-INITIALIZE-TABLE THRU 2218-INITIALIZE-EXIT  EL320
04579            VARYING TABLE-INDX FROM +1 BY +1                       EL320
04580              UNTIL TABLE-INDX > +20                               EL320
04581          MOVE CF-LAST-MAINT-BY   TO  BUSS-MAINT-BY                EL320
04582          MOVE CF-LAST-MAINT-DT   TO  DC-BIN-DATE-1                EL320
04583          MOVE ' '                TO  DC-OPTION-CODE               EL320
04584          PERFORM  1100-DATE-RTN  THRU  1100-EXIT                  EL320
04585          MOVE DC-GREG-DATE-1-EDIT                                 EL320
04586                                  TO  BUSS-MAINT-DT                EL320
04587          MOVE CF-LAST-MAINT-HHMMSS                                EL320
04588                                  TO  WS-MAINT-TIME                EL320
04589          MOVE WS-MAINT-HH        TO  BUSS-MAINT-HH                EL320
04590          MOVE WS-MAINT-MM        TO  BUSS-MAINT-MM                EL320
04591          MOVE WS-MAINT-SS        TO  BUSS-MAINT-SS                EL320
04592          MOVE 1                  TO  BUSS-PRINTED-CODE            EL320
04593          SET TABLE-INDX          TO +1                            EL320
04594          SET TABLE-INDX DOWN BY 1                                 EL320
04595          SET INDX-SUB            TO +1                            EL320
04596          MOVE ZERO               TO  TABLE-PRT-CONTROL            EL320
04597                                      WS-BUSS-RECORD-CNT           EL320
04598          MOVE CF-RECORD-TYPE     TO  WS-RECORD-TYPE.              EL320
04599                                                                   EL320
04600      MOVE 1                      TO  BUSS-RECORD-INDX.            EL320
04601      ADD  1                      TO  WS-BUSS-RECORD-CNT.          EL320
04602      MOVE DS                     TO  X.                           EL320
04603                                                                   EL320
04604  2218-MOVE-ARRAY.                                                 EL320
04605                                                                   EL320
04606      IF CF-BUSINESS-TITLE (BUSS-RECORD-INDX)  EQUAL TO  SPACES    EL320
04607          GO TO 2218-BUMP-SUB                                      EL320
04608      ELSE                                                         EL320
04609          SET TABLE-INDX UP BY +1                                  EL320
04610          ADD 1                   TO TABLE-PRT-CONTROL.            EL320
04611                                                                   EL320
04612      IF TABLE-PRT-CONTROL NOT LESS THAN 21                        EL320
04613          SET INDX-SUB            TO +2                            EL320
04614      ELSE                                                         EL320
04615          SET INDX-SUB            TO +1.                           EL320
04616                                                                   EL320
04617      IF TABLE-INDX NOT LESS THAN 21                               EL320
04618          SET TABLE-INDX          TO +1.                           EL320
04619                                                                   EL320
04620      MOVE BUSS-PRINTED-CODE                                       EL320
04621                 TO WS-TABLE-CODE (TABLE-INDX INDX-SUB).           EL320
04622      MOVE CF-BUSINESS-TITLE (BUSS-RECORD-INDX)                    EL320
04623                 TO WS-TABLE-NAME (TABLE-INDX INDX-SUB).           EL320
04624                                                                   EL320
04625  2218-BUMP-SUB.                                                   EL320
04626                                                                   EL320
04627      ADD  1                      TO  BUSS-PRINTED-CODE.           EL320
04628                                                                   EL320
04629      IF BUSS-RECORD-INDX LESS THAN 20                             EL320
04630          ADD +1                  TO  BUSS-RECORD-INDX             EL320
04631          GO TO 2218-MOVE-ARRAY.                                   EL320
04632                                                                   EL320
04633      PERFORM 2218-PRT-CHECK      THRU 2218-PRT-CHECK-EXIT.        EL320
04634                                                                   EL320
04635  2218-EXIT.                                                       EL320
04636      EXIT.                                                        EL320
04637                                                                      CL**3
04638  2218-PRT-CHECK.                                                  EL320
04639      IF (TABLE-PRT-CONTROL NOT LESS THAN 40 OR                    EL320
04640                 WS-BUSS-RECORD-CNT = 5)  AND                      EL320
04641                 WS-TABLE-NAME (1 1) NOT EQUAL SPACES              EL320
04642          PERFORM 1200-HEADING    THRU 1200-EXIT                   EL320
04643          MOVE TS                 TO  X                            EL320
04644          PERFORM 2218-HEADING    THRU 2218-HEAD-EXIT              EL320
04645          SET TABLE-INDX          TO +1                            EL320
04646          PERFORM 2218-PRT-PAGE   THRU 2218-PRT-EXIT               EL320
04647          PERFORM 2218-INITIALIZE-TABLE THRU 2218-INITIALIZE-EXIT  EL320
04648            VARYING TABLE-INDX FROM +1 BY +1                       EL320
04649              UNTIL TABLE-INDX > +20                               EL320
04650          SET TABLE-INDX          TO +1                            EL320
04651          SET INDX-SUB            TO +1                            EL320
04652          MOVE 1                  TO TABLE-PRT-CONTROL.            EL320
04653                                                                   EL320
04654      IF (TABLE-PRT-CONTROL NOT LESS THAN 40 OR                    EL320
04655                 WS-BUSS-RECORD-CNT = 5)  AND                      EL320
04656                 WS-TABLE-NAME (1 1) EQUAL SPACES                  EL320
04657          SET TABLE-INDX          TO +1                            EL320
04658          SET INDX-SUB            TO +1                            EL320
04659          MOVE 1                  TO TABLE-PRT-CONTROL.            EL320
04660                                                                      CL**3
04661  2218-PRT-CHECK-EXIT.                                             EL320
04662       EXIT.                                                          CL**3
04663                                                                   EL320
04664  2218-HEADING.                                                    EL320
04665                                                                   EL320
04666      MOVE BUSS-LINE-1            TO  P-DATA.                      EL320
04667                                                                   EL320
04668      PERFORM  1300-PRINT-LINE    THRU  1300-EXIT.                 EL320
04669                                                                   EL320
04670      MOVE SS                     TO  X.                           EL320
04671      MOVE BUSS-LINE-2            TO  P-DATA.                      EL320
04672                                                                   EL320
04673      PERFORM  1300-PRINT-LINE    THRU  1300-EXIT.                 EL320
04674                                                                   EL320
04675      MOVE DS                     TO  X.                           EL320
04676      MOVE BUSS-LINE-3            TO  P-DATA.                      EL320
04677                                                                   EL320
04678      PERFORM  1300-PRINT-LINE    THRU  1300-EXIT.                 EL320
04679                                                                   EL320
04680      MOVE 12                     TO  WS-LINE-CNT.                 EL320
04681                                                                   EL320
04682  2218-HEAD-EXIT.                                                  EL320
04683      EXIT.                                                        EL320
04684                                                                   EL320
04685  2218-INITIALIZE-TABLE.                                           EL320
04686                                                                   EL320
04687      MOVE SPACES TO WS-BUSS-DETAIL (TABLE-INDX).                  EL320
04688                                                                   EL320
04689  2218-INITIALIZE-EXIT.                                            EL320
04690      EXIT.                                                        EL320
04691                                                                   EL320
04692  2218-PRT-PAGE.                                                   EL320
04693                                                                   EL320
04694      IF WS-LINE-CNT  GREATER THAN  55                             EL320
04695          PERFORM  1200-HEADING  THRU  1200-EXIT                   EL320
04696          MOVE DS                 TO  X                            EL320
04697          MOVE '(CONTINUED...)'   TO  P-DATA                       EL320
04698          PERFORM  1300-PRINT-LINE  THRU  1300-EXIT                EL320
04699          MOVE SS                 TO  X                            EL320
04700          PERFORM  2218-HEADING  THRU  2218-HEAD-EXIT.             EL320
04701                                                                   EL320
04702      MOVE DS                     TO X.                            EL320
04703      MOVE WS-BUSS-DETAIL (TABLE-INDX) TO P-DATA.                  EL320
04704                                                                   EL320
04705      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
04706                                                                   EL320
04707      ADD +2                      TO  WS-LINE-CNT.                 EL320
04708                                                                   EL320
04709      IF TABLE-INDX LESS THAN 20                                   EL320
04710          SET TABLE-INDX UP BY +1                                  EL320
04711          GO TO 2218-PRT-PAGE.                                     EL320
04712                                                                      CL**3
04713  2218-PRT-EXIT.                                                   EL320
04714      EXIT.                                                           CL**3
04715                                                                      CL**3
04716  2219-PROCESS-TERM-RECORD.                                        EL320
04717                                                                   EL320
04718      PERFORM  1200-HEADING  THRU  1200-EXIT.                      EL320
04719                                                                   EL320
04720      MOVE CF-LAST-MAINT-BY       TO  TERM-MAINT-BY.               EL320
04721      MOVE CF-LAST-MAINT-DT       TO  DC-BIN-DATE-1.               EL320
04722      MOVE ' '                    TO  DC-OPTION-CODE.              EL320
04723                                                                   EL320
04724      PERFORM  1100-DATE-RTN  THRU  1100-EXIT.                     EL320
04725                                                                   EL320
04726      MOVE DC-GREG-DATE-1-EDIT    TO  TERM-MAINT-DT.               EL320
04727      MOVE CF-LAST-MAINT-HHMMSS   TO  WS-MAINT-TIME.               EL320
04728      MOVE WS-MAINT-HH            TO  TERM-MAINT-HH.               EL320
04729      MOVE WS-MAINT-MM            TO  TERM-MAINT-MM.               EL320
04730      MOVE WS-MAINT-SS            TO  TERM-MAINT-SS.               EL320
04731      MOVE CF-RECORD-TYPE         TO  WS-RECORD-TYPE.              EL320
04732      MOVE TS                     TO  X.                           EL320
04733      MOVE TERM-LINE-1            TO  P-DATA.                      EL320
04734                                                                   EL320
04735      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
04736                                                                   EL320
04737      MOVE SS                     TO  X.                           EL320
04738      MOVE TERM-LINE-2            TO  P-DATA.                      EL320
04739                                                                   EL320
04740      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
04741                                                                   EL320
04742      MOVE TS                     TO  X.                           EL320
04743      MOVE TERM-LINE-3            TO  P-DATA.                      EL320
04744                                                                   EL320
04745      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
04746                                                                   EL320
04747      MOVE SPACES                 TO  TERM-DETAIL-LINE.            EL320
04748      MOVE 1                      TO  TERM-SUB  V-SUB.             EL320
04749                                                                   EL320
04750      PERFORM  2219A-MOVE  THRU  2219A-EXIT.                       EL320
04751                                                                   EL320
04752  2219-EXIT.                                                       EL320
04753      EXIT.                                                        EL320
04754                                                                      CL**3
04755  2219A-MOVE.                                                      EL320
04756                                                                   EL320
04757      MOVE 1                      TO  H-SUB.                       EL320
04758                                                                   EL320
04759      PERFORM  2219B-MOVE  THRU  2219B-EXIT.                       EL320
04760                                                                   EL320
04761      MOVE SS                     TO  X.                           EL320
04762      MOVE TERM-ARRAY (V-SUB)     TO  P-DATA.                      EL320
04763                                                                   EL320
04764      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
04765                                                                   EL320
04766      ADD 1                       TO  V-SUB.                       EL320
04767                                                                   EL320
04768      IF V-SUB  LESS THAN  11                                      EL320
04769          GO TO 2219A-MOVE.                                        EL320
04770                                                                   EL320
04771  2219A-EXIT.                                                      EL320
04772      EXIT.                                                        EL320
04773                                                                   EL320
04774  2219B-MOVE.                                                      EL320
04775                                                                   EL320
04776      IF CF-TERMINAL-ID (TERM-SUB)  EQUAL TO  SPACES               EL320
04777          MOVE SPACES             TO  TERMNL (V-SUB  H-SUB)        EL320
04778      ELSE                                                         EL320
04779          MOVE CF-TERMINAL-ID (TERM-SUB)                           EL320
04780                                  TO  TERMNL (V-SUB  H-SUB).       EL320
04781                                                                   EL320
04782      ADD 1                       TO  TERM-SUB.                    EL320
04783      ADD 1                       TO  H-SUB.                       EL320
04784                                                                   EL320
04785      IF H-SUB  LESS THAN  13                                      EL320
04786          GO TO 2219B-MOVE.                                        EL320
04787                                                                   EL320
04788  2219B-EXIT.                                                      EL320
04789      EXIT.                                                        EL320
04790                                                                      CL**3
04791  2220-PROCESS-LFED-RECORD.                                        EL320
04792                                                                   EL320
04793      PERFORM  1200-HEADING  THRU  1200-EXIT.                      EL320
04794                                                                   EL320
04795      MOVE CF-LAST-MAINT-BY       TO  LFED-MAINT-BY.               EL320
04796      MOVE CF-LAST-MAINT-DT       TO  DC-BIN-DATE-1.               EL320
04797      MOVE ' '                    TO  DC-OPTION-CODE.              EL320
04798                                                                   EL320
04799      PERFORM  1100-DATE-RTN  THRU  1100-EXIT.                     EL320
04800                                                                   EL320
04801      MOVE DC-GREG-DATE-1-EDIT    TO  LFED-MAINT-DT.               EL320
04802      MOVE CF-LAST-MAINT-HHMMSS   TO  WS-MAINT-TIME.               EL320
04803      MOVE WS-MAINT-HH            TO  LFED-MAINT-HH.               EL320
04804      MOVE WS-MAINT-MM            TO  LFED-MAINT-MM.               EL320
04805      MOVE WS-MAINT-SS            TO  LFED-MAINT-SS.               EL320
04806      MOVE CF-RECORD-TYPE         TO  WS-RECORD-TYPE.              EL320
04807      MOVE TS                     TO  X.                           EL320
04808      MOVE LFED-LINE-1            TO  P-DATA.                      EL320
04809                                                                   EL320
04810      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
04811                                                                   EL320
04812      MOVE SS                     TO  X.                           EL320
04813      MOVE LFED-LINE-2            TO  P-DATA.                      EL320
04814                                                                   EL320
04815      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
04816                                                                   EL320
04817      MOVE TS                     TO  X.                           EL320
04818      MOVE LFED-LINE-3            TO  P-DATA.                      EL320
04819                                                                   EL320
04820      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
04821                                                                   EL320
04822      MOVE TS                     TO  X.                           EL320
04823      MOVE LFED-LINE-4            TO  P-DATA.                      EL320
04824                                                                   EL320
04825      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
04826                                                                   EL320
04827      MOVE SPACES                 TO  LFED-DETAIL-LINE.            EL320
04828      MOVE 1                      TO  LFED-SUB  V1-SUB.            EL320
04829                                                                   EL320
04830      PERFORM  2220A-MOVE  THRU  2220A-EXIT.                       EL320
04831                                                                   EL320
04832  2220-EXIT.                                                       EL320
04833      EXIT.                                                        EL320
04834                                                                      CL**3
04835  2220A-MOVE.                                                      EL320
04836                                                                   EL320
04837      MOVE 1                      TO  H1-SUB.                      EL320
04838                                                                   EL320
04839      PERFORM  2220B-MOVE  THRU  2220B-EXIT.                       EL320
04840                                                                   EL320
04841      MOVE SS                     TO  X.                           EL320
04842      MOVE LFED-ARRAY (V1-SUB)    TO  P-DATA.                      EL320
04843                                                                   EL320
04844      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
04845                                                                   EL320
04846      ADD 1                       TO  V1-SUB.                      EL320
04847                                                                   EL320
04848      IF V1-SUB  LESS THAN  16                                     EL320
04849          GO TO 2220A-MOVE.                                        EL320
04850                                                                   EL320
04851  2220A-EXIT.                                                      EL320
04852      EXIT.                                                        EL320
04853                                                                   EL320
04854  2220B-MOVE.                                                      EL320
04855                                                                   EL320
04856      IF CF-LIFE-CODE-IN (LFED-SUB)  EQUAL TO  SPACES              EL320
04857          MOVE SPACES             TO  LFED-CD-IN (V1-SUB  H1-SUB)  EL320
04858          MOVE SPACES             TO  LFED-CD-OT (V1-SUB  H1-SUB)  EL320
04859      ELSE                                                         EL320
04860          MOVE CF-LIFE-CODE-IN (LFED-SUB)                          EL320
04861                                  TO  LFED-CD-IN (V1-SUB  H1-SUB)  EL320
04862          MOVE CF-LIFE-CODE-OUT (LFED-SUB)                         EL320
04863                                  TO  LFED-CD-OT (V1-SUB  H1-SUB). EL320
04864                                                                   EL320
04865      ADD 1                       TO  LFED-SUB.                    EL320
04866      ADD 1                       TO  H1-SUB.                      EL320
04867                                                                   EL320
04868      IF H1-SUB  LESS THAN  9                                      EL320
04869          GO TO 2220B-MOVE.                                        EL320
04870                                                                   EL320
04871  2220B-EXIT.                                                      EL320
04872      EXIT.                                                        EL320
04873                                                                      CL**3
04874  2221-PROCESS-AHED-RECORD.                                        EL320
04875                                                                   EL320
04876      PERFORM  1200-HEADING  THRU  1200-EXIT.                      EL320
04877                                                                   EL320
04878      MOVE CF-LAST-MAINT-BY       TO  AHED-MAINT-BY.               EL320
04879      MOVE CF-LAST-MAINT-DT       TO  DC-BIN-DATE-1.               EL320
04880      MOVE ' '                    TO  DC-OPTION-CODE.              EL320
04881                                                                   EL320
04882      PERFORM  1100-DATE-RTN  THRU  1100-EXIT.                     EL320
04883                                                                   EL320
04884      MOVE DC-GREG-DATE-1-EDIT    TO  AHED-MAINT-DT.               EL320
04885      MOVE CF-LAST-MAINT-HHMMSS   TO  WS-MAINT-TIME.               EL320
04886      MOVE WS-MAINT-HH            TO  AHED-MAINT-HH.               EL320
04887      MOVE WS-MAINT-MM            TO  AHED-MAINT-MM.               EL320
04888      MOVE WS-MAINT-SS            TO  AHED-MAINT-SS.               EL320
04889      MOVE CF-RECORD-TYPE         TO  WS-RECORD-TYPE.              EL320
04890      MOVE TS                     TO  X.                           EL320
04891      MOVE AHED-LINE-1            TO  P-DATA.                      EL320
04892                                                                   EL320
04893      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
04894                                                                   EL320
04895      MOVE SS                     TO  X.                           EL320
04896      MOVE AHED-LINE-2            TO  P-DATA.                      EL320
04897                                                                   EL320
04898      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
04899                                                                   EL320
04900      MOVE TS                     TO  X.                           EL320
04901      MOVE AHED-LINE-3            TO  P-DATA.                      EL320
04902                                                                   EL320
04903      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
04904                                                                   EL320
04905      MOVE TS                     TO  X.                           EL320
04906      MOVE AHED-LINE-4            TO  P-DATA.                      EL320
04907                                                                   EL320
04908      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
04909                                                                   EL320
04910      MOVE SPACES                 TO  AHED-DETAIL-LINE.            EL320
04911      MOVE 1                      TO  AHED-SUB  V2-SUB.            EL320
04912                                                                   EL320
04913      PERFORM  2221A-MOVE  THRU  2221A-EXIT.                       EL320
04914                                                                   EL320
04915  2221-EXIT.                                                       EL320
04916      EXIT.                                                        EL320
04917                                                                      CL**3
04918  2221A-MOVE.                                                      EL320
04919                                                                   EL320
04920      MOVE 1                      TO  H2-SUB.                      EL320
04921                                                                   EL320
04922      PERFORM  2221B-MOVE  THRU  2221B-EXIT.                       EL320
04923                                                                   EL320
04924      MOVE SS                     TO  X.                           EL320
04925      MOVE AHED-ARRAY (V2-SUB)    TO  P-DATA.                      EL320
04926                                                                   EL320
04927      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
04928                                                                   EL320
04929      ADD 1                       TO  V2-SUB.                      EL320
04930                                                                   EL320
04931      IF V2-SUB  LESS THAN  13                                     EL320
04932          GO TO 2221A-MOVE.                                        EL320
04933                                                                   EL320
04934  2221A-EXIT.                                                      EL320
04935      EXIT.                                                        EL320
04936                                                                   EL320
04937  2221B-MOVE.                                                      EL320
04938                                                                   EL320
04939      IF CF-AH-CODE-IN (AHED-SUB)  EQUAL TO  SPACES                EL320
04940          MOVE SPACES             TO  AHED-CD-IN (V2-SUB  H2-SUB)  EL320
04941          MOVE SPACES             TO  AHED-CD-OT (V2-SUB  H2-SUB)  EL320
04942      ELSE                                                         EL320
04943          MOVE CF-AH-CODE-IN (AHED-SUB)                            EL320
04944                                  TO  AHED-CD-IN (V2-SUB  H2-SUB)  EL320
04945          MOVE CF-AH-CODE-OUT (AHED-SUB)                           EL320
04946                                  TO  AHED-CD-OT (V2-SUB  H2-SUB). EL320
04947                                                                   EL320
04948      ADD 1                       TO  AHED-SUB.                    EL320
04949      ADD 1                       TO  H2-SUB.                      EL320
04950                                                                   EL320
04951      IF H2-SUB  LESS THAN  9                                      EL320
04952          GO TO 2221B-MOVE.                                        EL320
04953                                                                   EL320
04954  2221B-EXIT.                                                      EL320
04955      EXIT.                                                        EL320
04956                                                                      CL**3
04957  3000-PROCESS-PGMS-FILE.                                          EL320
04958                                                                   EL320
04959      MOVE LOW-VALUES             TO  PS-CONTROL-PRIMARY.          EL320
04960      MOVE DTE-CLASIC-COMPANY-CD  TO  PS-COMPANY-CD.               EL320
04961 *    MOVE 'EL'                   TO  PS-SYSTEM-CODE.              EL320
04962      MOVE LOW-VALUES             TO  PS-SYSTEM-CODE.              EL320
04963                                                                   EL320
04964      START  ELPGMSF                                               EL320
04965          KEY  NOT LESS THAN  PS-CONTROL-PRIMARY.                  EL320
04966                                                                   EL320
04967      IF ELPGMS-STAT-1  NOT EQUAL  ZEROS                           EL320
04968          MOVE 'E'                TO  WS-ELPGMS-EOF-SW             EL320
04969          DISPLAY 'NO PGMS RECS FOUND-------'                      EL320
04970          GO TO 3000-EXIT.                                         EL320
04971                                                                   EL320
04972      PERFORM  3300-PROCESS-PGMS-RECORDS  THRU  3300-EXIT          EL320
04973          UNTIL END-OF-PGMS-FILE.                                  EL320
04974                                                                   EL320
04975  3000-EXIT.                                                       EL320
04976      EXIT.                                                        EL320
04977                                                                      CL**3
04978  3300-PROCESS-PGMS-RECORDS.                                       EL320
04979                                                                   EL320
04980      READ  ELPGMSF  NEXT RECORD.                                  EL320
04981                                                                   EL320
04982      IF ELPGMS-STAT-1  EQUAL  '1'                                 EL320
04983          MOVE 'E'                TO  WS-ELPGMS-EOF-SW             EL320
04984          GO TO 3300-EXIT.                                         EL320
04985                                                                   EL320
04986      IF ELPGMS-STAT-1  NOT EQUAL  '0'                             EL320
04987          MOVE 'ERROR OCCURED READNEXT - ELPGMS'                   EL320
04988                                  TO  WS-ABEND-MESSAGE             EL320
04989          MOVE ELPGMS-STATUS      TO  WS-ABEND-FILE-STATUS         EL320
04990          PERFORM  ABEND-PGM.                                      EL320
04991                                                                   EL320
04992      IF PS-COMPANY-CD  EQUAL  DTE-CLASIC-COMPANY-CD               EL320
04993          NEXT SENTENCE                                            EL320
04994      ELSE                                                         EL320
04995          MOVE 'E'                TO  WS-ELPGMS-EOF-SW             EL320
04996          GO TO 3300-EXIT.                                         EL320
04997                                                                   EL320
04998      IF PS-SYSTEM-CODE  EQUAL  'EC' OR 'EL' OR 'GL'               EL320
04999          NEXT SENTENCE                                            EL320
05000      ELSE                                                         EL320
05001          GO TO 3300-EXIT.                                         EL320
05002                                                                   EL320
05003      IF WS-LINE-CNT  GREATER THAN  55                             EL320
05004          PERFORM  1200-HEADING  THRU  1200-EXIT                   EL320
05005          PERFORM  3100-HEADING  THRU  3100-EXIT.                  EL320
05006                                                                   EL320
05007      MOVE PS-PROGRAM-NUMBER      TO  PGMS-PROG.                   EL320
05008      MOVE 1                      TO  PGMS-SUB.                    EL320
05009                                                                   EL320
05010      PERFORM  3200-MOVE-ARRAY  THRU  3200-EXIT  4  TIMES.         EL320
05011                                                                   EL320
05012  3300-EXIT.                                                       EL320
05013      EXIT.                                                        EL320
05014                                                                   EL320
05015  3100-HEADING.                                                    EL320
05016                                                                   EL320
05017      MOVE TS                     TO  X.                           EL320
05018      MOVE PGMS-LINE-1            TO  P-DATA.                      EL320
05019                                                                   EL320
05020      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
05021                                                                   EL320
05022      MOVE TS                     TO  X.                           EL320
05023      MOVE PGMS-LINE-2            TO  P-DATA.                      EL320
05024                                                                   EL320
05025      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
05026                                                                   EL320
05027      MOVE SS                     TO  X.                           EL320
05028      MOVE PGMS-LINE-3            TO  P-DATA.                      EL320
05029                                                                   EL320
05030      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
05031                                                                   EL320
05032      MOVE SS                     TO  X.                           EL320
05033      MOVE SPACES                 TO  P-DATA.                      EL320
05034                                                                   EL320
05035      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
05036                                                                   EL320
05037      MOVE 13                     TO  WS-LINE-CNT.                 EL320
05038                                                                   EL320
05039  3100-EXIT.                                                       EL320
05040      EXIT.                                                        EL320
05041                                                                   EL320
05042  3200-MOVE-ARRAY.                                                 EL320
05043                                                                   EL320
05044      IF PS-FREQUENCY-CODE (PGMS-SUB)  = SPACE  AND                EL320
05045         PS-PRINT-OPTION   (PGMS-SUB)  = SPACE  AND                EL320
05046         PS-FORMAT-OPTION  (PGMS-SUB)  = SPACE  AND                EL320
05047         PS-PROCESS-OPTION (PGMS-SUB)  = SPACE  AND                EL320
05048         PS-TOTAL-OPTION   (PGMS-SUB)  = SPACE                     EL320
05049          GO TO 3200-CONTINUE.                                     EL320
05050                                                                   EL320
05051      MOVE PS-FREQUENCY-CODE (PGMS-SUB)  TO  PGMS-FREQ.            EL320
05052      MOVE PS-PRINT-OPTION   (PGMS-SUB)  TO  PGMS-PRNT.            EL320
05053      MOVE PS-FORMAT-OPTION  (PGMS-SUB)  TO  PGMS-FORMAT.          EL320
05054      MOVE PS-PROCESS-OPTION (PGMS-SUB)  TO  PGMS-PROC.            EL320
05055      MOVE PS-TOTAL-OPTION   (PGMS-SUB)  TO  PGMS-TOT.             EL320
05056                                                                   EL320
05057  3200-CONTINUE.                                                   EL320
05058      ADD 1                       TO  PGMS-SUB.                    EL320
05059                                                                   EL320
05060      MOVE SS                     TO  X.                           EL320
05061      MOVE PGMS-DETAIL            TO  P-DATA.                      EL320
05062                                                                   EL320
05063      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
05064                                                                   EL320
05065      MOVE SPACES                 TO  PGMS-PROG.                   EL320
05066                                                                   EL320
05067      ADD 1                       TO  WS-LINE-CNT.                 EL320
05068                                                                   EL320
05069  3200-EXIT.                                                       EL320
05070      EXIT.                                                        EL320
05071      EJECT                                                        EL320
05072  4000-PROCESS-TEXT-FILE.                                          EL320
05073                                                                   EL320
05074      MOVE LOW-VALUES             TO  TX-CONTROL-PRIMARY.          EL320
05075      MOVE DTE-CLASIC-COMPANY-CD  TO  TX-COMPANY-CD.               EL320
05076                                                                   EL320
05077      PERFORM  5000-INITIALIZE-TEXT-INDEX  THRU  5000-EXIT.        EL320
05078                                                                   EL320
05079      START  ELFORMF                                               EL320
05080          KEY  NOT LESS THAN  TX-CONTROL-PRIMARY.                  EL320
05081                                                                   EL320
05082      IF ELFORM-STAT-1  NOT EQUAL  ZEROS                           EL320
05083          MOVE 'E'                TO  WS-ELFORM-EOF-SW             EL320
05084          DISPLAY 'NO FORM RECS FOUND-------'                      EL320
05085          GO TO 4000-EXIT.                                         EL320
05086                                                                   EL320
05087      IF ELFORM-STATUS  NOT EQUAL  ZERO                            EL320
05088          MOVE 'ERROR OCCURED START - ELFORM'                      EL320
05089                                  TO  WS-ABEND-MESSAGE             EL320
05090          MOVE ELFORM-STATUS      TO  WS-ABEND-FILE-STATUS         EL320
05091          PERFORM  ABEND-PGM.                                      EL320
05092                                                                   EL320
05093      MOVE SPACE                  TO  WS-SAVED-FORM-NO.            EL320
05094                                                                   EL320
05095      PERFORM  4200-PROCESS-TEXT-RECORDS  THRU  4200-EXIT          EL320
05096          UNTIL END-OF-TEXT-FILE.                                  EL320
05097                                                                   EL320
05098      MOVE 99                     TO  WS-LINE-CNT.                 EL320
05099      MOVE '  FORM NUMBER'        TO  WS-TEXT-TYPE-DESC.           EL320
05100      MOVE 'FORM INDEX  '         TO  WS-TEXT-DESC.                EL320
05101                                                                   EL320
05102      SET WTI-INDX                TO  +1.                          EL320
05103                                                                   EL320
05104      PERFORM  4800-PROCESS-TEXT-INDEX  THRU  4800-EXIT.           EL320
05105                                                                   EL320
05106  4000-EXIT.                                                       EL320
05107      EXIT.                                                        EL320
05108                                                                      CL**3
05109  4200-PROCESS-TEXT-RECORDS.                                       EL320
05110                                                                   EL320
05111      READ  ELFORMF  NEXT RECORD.                                  EL320
05112                                                                   EL320
05113      IF ELFORM-STAT-1  EQUAL  '1'                                 EL320
05114          DISPLAY '***END OF FORM ***'                             EL320
05115          MOVE 'E'                TO  WS-ELFORM-EOF-SW             EL320
05116          GO TO 4200-EXIT.                                         EL320
05117                                                                   EL320
05118      IF ELFORM-STATUS  NOT EQUAL  ZERO                            EL320
05119          MOVE 'ERROR OCCURED READNEXT - ELFORM'                   EL320
05120                                  TO  WS-ABEND-MESSAGE             EL320
05121          MOVE ELFORM-STATUS      TO  WS-ABEND-FILE-STATUS         EL320
05122          PERFORM  ABEND-PGM.                                      EL320
05123                                                                   EL320
05124      IF TX-COMPANY-CD  EQUAL  DTE-CLASIC-COMPANY-CD               EL320
05125          NEXT SENTENCE                                            EL320
05126      ELSE                                                         EL320
05127          MOVE 'E'                TO  WS-ELFORM-EOF-SW             EL320
05128          GO TO 4200-EXIT.                                         EL320
05129                                                                   EL320
05130      IF FORMS-FILE-TEXT                                           EL320
05131          NEXT SENTENCE                                            EL320
05132      ELSE                                                         EL320
05133          GO TO 4200-EXIT.                                         EL320
05134                                                                   EL320
05135      IF TABLE-SIZE-EXCEEDED                                       EL320
05136          GO TO 4200-CONTINUE.                                     EL320
05137                                                                   EL320
05138      IF WTI-INDX  IS GREATER THAN  MAX-WTI-INDX                   EL320
05139          MOVE 'Y'                TO  WS-TABLE-EXCEEDED-SW         EL320
05140          GO TO 4200-CONTINUE.                                     EL320
05141                                                                   EL320
05142      IF TX-FORM-NO  NOT EQUAL  WS-SAVED-FORM-NO                   EL320
05143          MOVE TX-FORM-NO         TO  FORM-NUMBER                  EL320
05144                                      WS-SAVED-FORM-NO             EL320
05145                                      WS-TEXT-NO (WTI-INDX)        EL320
05146          MOVE 99                 TO  WS-LINE-CNT                  EL320
05147          IF WS-LINE-CNT  GREATER THAN  45                         EL320
05148              MOVE WS-PAGE-CNT    TO  WS-TEXT-PAGE (WTI-INDX)      EL320
05149              SET WTI-INDX  UP  BY  +1                             EL320
05150              PERFORM  1200-HEADING                                EL320
05151              PERFORM  4100-HEADING.                               EL320
05152                                                                   EL320
05153  4200-CONTINUE.                                                   EL320
05154      MOVE TX-LINE-SEQUENCE       TO  FORM-LINE.                   EL320
05155      MOVE TX-TEXT-LINE           TO  FORM-TEXT.                   EL320
05156      MOVE SS                     TO  X.                           EL320
05157      MOVE FORM-DETAIL            TO  P-DATA.                      EL320
05158                                                                   EL320
05159      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
05160                                                                   EL320
05161      ADD 1                       TO  WS-LINE-CNT.                 EL320
05162                                                                   EL320
05163      IF WS-LINE-CNT  GREATER THAN  50                             EL320
05164          PERFORM  1200-HEADING                                    EL320
05165          PERFORM  4100-HEADING.                                   EL320
05166                                                                   EL320
05167  4200-EXIT.                                                       EL320
05168      EXIT.                                                        EL320
05169                                                                   EL320
05170  4100-HEADING.                                                    EL320
05171                                                                   EL320
05172      MOVE TS                     TO  X.                           EL320
05173      MOVE FORM-LINE-9            TO  P-DATA.                      EL320
05174                                                                   EL320
05175      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
05176                                                                   EL320
05177      MOVE TS                     TO  X.                           EL320
05178      MOVE FORM-LINE-11           TO  P-DATA.                      EL320
05179                                                                   EL320
05180      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
05181                                                                   EL320
05182      MOVE SS                     TO  X.                           EL320
05183      MOVE FORM-LINE-13           TO  P-DATA.                      EL320
05184                                                                   EL320
05185      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
05186                                                                   EL320
05187      MOVE SS                     TO  X.                           EL320
05188      MOVE SPACES                 TO  P-DATA.                      EL320
05189                                                                   EL320
05190      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
05191                                                                   EL320
05192      MOVE 13                     TO  WS-LINE-CNT.                 EL320
05193                                                                   EL320
05194  4100-EXIT. EXIT.                                                 EL320
05195      EJECT                                                        EL320
05196  4500-PROCESS-LETR-FILE.                                          EL320
05197                                                                   EL320
05198      MOVE LOW-VALUES             TO  LT-CONTROL-PRIMARY.          EL320
05199      MOVE DTE-CLASIC-COMPANY-CD  TO  LT-COMPANY-CD.               EL320
05200                                                                   EL320
05201      PERFORM  5000-INITIALIZE-TEXT-INDEX  THRU  5000-EXIT.        EL320
05202                                                                   EL320
05203      START  ELLETRF                                               EL320
05204          KEY  NOT LESS THAN  LT-CONTROL-PRIMARY.                  EL320
05205                                                                   EL320
05206      IF ELLETR-STAT-1  NOT EQUAL  ZEROS                           EL320
05207          MOVE 'E'                TO  WS-ELLETR-EOF-SW             EL320
05208          DISPLAY 'NO LETR RECS FOUND-------'                      EL320
05209          GO TO 4500-EXIT.                                         EL320
05210                                                                   EL320
05211      IF ELLETR-STATUS  NOT EQUAL  ZERO                            EL320
05212          MOVE 'ERROR OCCURED START - ELLETR'                      EL320
05213                                  TO  WS-ABEND-MESSAGE             EL320
05214          MOVE ELLETR-STATUS      TO  WS-ABEND-FILE-STATUS         EL320
05215          PERFORM  ABEND-PGM.                                      EL320
05216                                                                   EL320
05217      MOVE SPACE                  TO  WS-SAVED-LETR-NO.            EL320
05218                                                                   EL320
05219      PERFORM  4700-PROCESS-LETR-RECORDS  THRU  4700-EXIT          EL320
05220          UNTIL END-OF-LETR-FILE.                                  EL320
05221                                                                   EL320
05222      MOVE 99                     TO  WS-LINE-CNT.                 EL320
05223      MOVE 'LETTER NUMBER'        TO  WS-TEXT-TYPE-DESC.           EL320
05224      MOVE 'LETTER INDEX'         TO  WS-TEXT-DESC.                EL320
05225                                                                   EL320
05226      SET WTI-INDX                TO  +1.                          EL320
05227                                                                   EL320
05228      PERFORM  4800-PROCESS-TEXT-INDEX  THRU  4800-EXIT.           EL320
05229                                                                   EL320
05230  4500-EXIT.                                                       EL320
05231      EXIT.                                                        EL320
05232      EJECT                                                        EL320
05233  4700-PROCESS-LETR-RECORDS.                                       EL320
05234                                                                   EL320
05235      READ  ELLETRF  NEXT RECORD.                                  EL320
05236                                                                   EL320
05237      IF ELLETR-STAT-1  EQUAL  '1'                                 EL320
05238          DISPLAY '***END OF LETR ***'                             EL320
05239          MOVE 'E'                TO  WS-ELLETR-EOF-SW             EL320
05240          GO TO 4700-EXIT.                                         EL320
05241                                                                   EL320
05242      IF ELLETR-STATUS  NOT EQUAL  ZERO                            EL320
05243          MOVE 'ERROR OCCURED READNEXT - ELLETR'                   EL320
05244                                  TO  WS-ABEND-MESSAGE             EL320
05245          MOVE ELLETR-STATUS      TO  WS-ABEND-FILE-STATUS         EL320
05246          PERFORM  ABEND-PGM.                                      EL320
05247                                                                   EL320
05248      IF LT-COMPANY-CD  EQUAL  DTE-CLASIC-COMPANY-CD               EL320
05249          NEXT SENTENCE                                            EL320
05250      ELSE                                                         EL320
05251          MOVE 'E'                TO  WS-ELLETR-EOF-SW             EL320
05252          GO TO 4700-EXIT.                                         EL320
05253                                                                   EL320
05254      IF TABLE-SIZE-EXCEEDED                                       EL320
05255          GO TO 4700-CONTINUE.                                     EL320
05256                                                                   EL320
05257      IF WTI-INDX  IS GREATER THAN  MAX-WTI-INDX                   EL320
05258          MOVE 'Y'                TO  WS-TABLE-EXCEEDED-SW         EL320
05259          GO TO 4700-CONTINUE.                                     EL320
05260                                                                   EL320
05261      IF LT-LETTER-NO EQUAL WS-SAVED-LETR-NO                       EL320
05262          IF WS-LINE-CNT GREATER THAN  50                          EL320
05263              MOVE '(CONTINUED...)' TO LETR-CONTINUED              EL320
05264              PERFORM  4750-HEADING THRU 4750-EXIT                 EL320
05265            ELSE                                                   EL320
05266              NEXT SENTENCE                                        EL320
05267        ELSE                                                       EL320
05268          MOVE LT-LETTER-NO       TO WS-SAVED-LETR-NO,             EL320
05269                                     LETR-NUMBER,                  EL320
05270                                     WS-TEXT-NO (WTI-INDX)         EL320
05271          MOVE WS-PAGE-CNT        TO WS-TEXT-PAGE (WTI-INDX)       EL320
05272          SET WTI-INDX UP BY +1                                    EL320
05273          MOVE SPACES             TO LETR-CONTINUED                EL320
05274              PERFORM  4750-HEADING THRU 4750-EXIT.                EL320
05275                                                                   EL320
05276  4700-CONTINUE.                                                   EL320
05277      MOVE LT-LINE-SEQUENCE       TO  LETR-LINE.                   EL320
05278      MOVE LT-TEXT-LINE           TO  LETR-TEXT.                   EL320
05279      MOVE LT-PROCESS-CONTROL     TO  LETR-SKIP.                   EL320
05280                                                                   EL320
05281      IF LT-PROCESS-CONTROL  NOT NUMERIC                           EL320
05282          MOVE ZEROS              TO  LT-PROCESS-CONTROL.          EL320
05283                                                                   EL320
05284      IF LT-LINE-SQUEEZE-CONTROL = 'K' OR 'Z'                      EL320
05285          MOVE LT-LINE-SQUEEZE-CONTROL  TO  LETR-SKIP.             EL320
05286                                                                   EL320
05287      MOVE SS                     TO  X.                           EL320
05288      MOVE LETR-DETAIL            TO  P-DATA.                      EL320
05289      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
05290      ADD 1                       TO  WS-LINE-CNT.                 EL320
05291                                                                   EL320
05292      MOVE LT-PROCESS-CONTROL     TO  TIMES-TO-PRINT.              EL320
05293                                                                   EL320
05294      IF TIMES-TO-PRINT  EQUAL  99                                 EL320
05295          MOVE 99                 TO  WS-LINE-CNT                  EL320
05296        ELSE                                                       EL320
05297      IF TIMES-TO-PRINT > ZERO                                     EL320
05298          MOVE SPACES             TO  P-DATA                       EL320
05299          PERFORM  1300-PRINT-LINE  THRU  1300-EXIT                EL320
05300            TIMES-TO-PRINT TIMES.                                  EL320
05301                                                                   EL320
05302  4700-EXIT.                                                       EL320
05303      EXIT.                                                        EL320
05304                                                                   EL320
05305  4750-HEADING.                                                    EL320
05306                                                                   EL320
05307      PERFORM  1200-HEADING.                                       EL320
05308                                                                   EL320
05309      MOVE TS                     TO  X.                           EL320
05310      MOVE LETR-LINE-9            TO  P-DATA.                      EL320
05311      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
05312                                                                   EL320
05313      MOVE TS                     TO  X.                           EL320
05314      MOVE LETR-LINE-11           TO  P-DATA.                      EL320
05315      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
05316                                                                   EL320
05317      MOVE SS                     TO  X.                           EL320
05318      MOVE LETR-LINE-13           TO  P-DATA.                      EL320
05319      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
05320                                                                   EL320
05321      MOVE SS                     TO  X.                           EL320
05322      MOVE SPACES                 TO  P-DATA.                      EL320
05323      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
05324                                                                   EL320
05325      MOVE 13                     TO  WS-LINE-CNT.                 EL320
05326                                                                   EL320
05327  4750-EXIT.                                                       EL320
05328      EXIT.                                                        EL320
05329      EJECT                                                        EL320
05330  4800-PROCESS-TEXT-INDEX.                                         EL320
05331                                                                   EL320
05332      IF WS-LINE-CNT  GREATER THAN  45                             EL320
05333          PERFORM  1200-HEADING                                    EL320
05334          PERFORM  4900-HEADING.                                   EL320
05335                                                                   EL320
05336      IF WS-TEXT-NO (WTI-INDX)  EQUAL  SPACES                      EL320
05337        AND  WS-TEXT-PAGE (WTI-INDX)  EQUAL  ZEROS                 EL320
05338          GO TO 4800-EXIT.                                         EL320
05339                                                                   EL320
05340      IF WTI-INDX  IS EQUAL TO  MAX-WTI-INDX                       EL320
05341        AND TABLE-SIZE-EXCEEDED                                    EL320
05342          MOVE 'N'                TO  WS-TABLE-EXCEEDED-SW         EL320
05343          MOVE SS                 TO  X                            EL320
05344          MOVE TEXT-DETAIL1       TO  P-DATA                       EL320
05345      ELSE                                                         EL320
05346          MOVE WS-TEXT-NO (WTI-INDX)                               EL320
05347                                  TO  WS-TEXT-NUMBER               EL320
05348          MOVE WS-TEXT-PAGE (WTI-INDX)                             EL320
05349                                  TO  WS-TEXT-PAGE-NO              EL320
05350          MOVE SS                     TO  X                        EL320
05351          MOVE TEXT-DETAIL            TO  P-DATA.                  EL320
05352                                                                   EL320
05353      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
05354                                                                   EL320
05355      ADD 1                       TO  WS-LINE-CNT.                 EL320
05356                                                                   EL320
05357      IF WS-LINE-CNT  GREATER THAN  50                             EL320
05358          PERFORM  1200-HEADING                                    EL320
05359          PERFORM  4900-HEADING.                                   EL320
05360                                                                   EL320
05361      SET WTI-INDX  UP  BY  +1.                                    EL320
05362                                                                   EL320
05363      IF WTI-INDX  GREATER  MAX-WTI-INDX                           EL320
05364          NEXT SENTENCE                                            EL320
05365      ELSE                                                         EL320
05366          GO TO 4800-PROCESS-TEXT-INDEX.                           EL320
05367                                                                   EL320
05368  4800-EXIT.                                                       EL320
05369      EXIT.                                                        EL320
05370                                                                   EL320
05371  4900-HEADING.                                                    EL320
05372                                                                   EL320
05373      MOVE TS                     TO  X.                           EL320
05374      MOVE TEXT-LINE-9            TO  P-DATA.                      EL320
05375                                                                   EL320
05376      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
05377                                                                   EL320
05378      MOVE TS                     TO  X.                           EL320
05379      MOVE TEXT-LINE-11           TO  P-DATA.                      EL320
05380                                                                   EL320
05381      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
05382                                                                   EL320
05383      MOVE SS                     TO  X.                           EL320
05384      MOVE SPACES                 TO  P-DATA.                      EL320
05385                                                                   EL320
05386      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
05387                                                                   EL320
05388      MOVE 11                     TO  WS-LINE-CNT.                 EL320
05389                                                                   EL320
05390  4900-EXIT.                                                       EL320
05391      EXIT.                                                        EL320
05392      EJECT                                                        EL320
05393  5000-INITIALIZE-TEXT-INDEX.                                      EL320
05394                                                                   EL320
05395      SET WTI-INDX                TO  +1.                          EL320
05396                                                                   EL320
05397  5000-CONT.                                                       EL320
05398                                                                   EL320
05399      MOVE SPACES                 TO  WS-TEXT-NO (WTI-INDX).       EL320
05400      MOVE ZEROS                  TO  WS-TEXT-PAGE (WTI-INDX).     EL320
05401                                                                   EL320
05402      SET WTI-INDX  UP  BY  +1.                                    EL320
05403                                                                   EL320
05404      IF WTI-INDX  GREATER  MAX-WTI-INDX                           EL320
05405          SET WTI-INDX            TO  +1                           EL320
05406          GO TO 5000-EXIT                                          EL320
05407      ELSE                                                         EL320
05408          GO TO 5000-CONT.                                         EL320
05409                                                                   EL320
05410  5000-EXIT.                                                       EL320
05411      EXIT.                                                        EL320
05412      EJECT                                                        EL320
05413  1100-DATE-RTN.                                                   EL320
05414                                                                   EL320
05415      CALL 'ELDATCX'  USING  DATE-CONVERSION-DATA.                 EL320
05416                                                                   EL320
05417      IF DC-ERROR-CODE  NOT EQUAL  SPACE                           EL320
05418          MOVE ZEROS              TO  DC-CONVERSION-DATES.         EL320
05419                                                                   EL320
05420  1100-EXIT.                                                       EL320
05421      EXIT.                                                        EL320
05422                                                                   EL320
05423  1200-HEADING.                                                    EL320
05424                                                                   EL320
05425      MOVE WS-PAGE-CNT            TO  HEAD-PAGE-NO.                EL320
05426      MOVE TP                     TO  X.                           EL320
05427      MOVE HEAD-LINE-1            TO  P-DATA.                      EL320
05428                                                                   EL320
05429      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
05430                                                                   EL320
05431      MOVE SS                     TO  X.                           EL320
05432      MOVE HEAD-LINE-2            TO  P-DATA.                      EL320
05433                                                                   EL320
05434      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
05435                                                                   EL320
05436      MOVE SS                     TO  X.                           EL320
05437      MOVE HEAD-LINE-3            TO  P-DATA.                      EL320
05438                                                                   EL320
05439      PERFORM  1300-PRINT-LINE  THRU  1300-EXIT.                   EL320
05440                                                                   EL320
05441      MOVE 5                      TO  WS-LINE-CNT.                 EL320
05442                                                                   EL320
05443      ADD 01                      TO  WS-PAGE-CNT.                 EL320
05444                                                                   EL320
05445  1200-EXIT.                                                       EL320
05446      EXIT.                                                        EL320
05447                                                                   EL320
05448  1300-PRINT-LINE.                                                 EL320
05449                                                                   EL320
05450      IF DTE-FICH NOT = SPACE AND                                  EL320
05451          FICH-OPEN   = SPACE                                      EL320
05452          MOVE 'X' TO FICH-OPEN                                    EL320
05453          OPEN OUTPUT FICH.                                        EL320
05454                                                                   EL320
05455      IF DTE-PRT-OPT = 'S' OR 'T'                                  EL320
05456          IF (REPT-OPEN = SPACE) AND (DTE-ABEND-CD-1 = SPACE)      EL320
05457              OPEN I-O ELREPT                                      EL320
05458              IF DTE-F-1 NOT = ZERO AND                            EL320
05459                 DTE-VSAM-FLAGS NOT = '97'                         EL320
05460                  MOVE DTE-VSAM-FLAGS  TO  WS-ABEND-FILE-STATUS    EL320
05461                  MOVE 'ERROR OCCURED OPEN - ELREPT'               EL320
05462                                  TO  WS-ABEND-MESSAGE             EL320
05463                  GO TO ABEND-PGM                                  EL320
05464              ELSE                                                 EL320
05465                  MOVE '1'                   TO REPT-OPEN          EL320
05466                  MOVE DTE-CLASIC-COMPANY-CD TO RF-COMPANY-CD      EL320
05467                  MOVE '1'                   TO RF-RECORD-TYPE     EL320
05468                  MOVE OLC-REPORT-NAME       TO RF-REPORT-ID       EL320
05469                  MOVE ZERO                  TO RF-LINE-NUMBER     EL320
05470                  START ELREPT  KEY NOT LESS RF-CONTROL-PRIMARY    EL320
05471                  PERFORM DTE-REPORT-DELETE THRU DTE-DELETE-EXIT   EL320
05472                  MOVE DTE-CLASIC-COMPANY-CD TO RF-COMPANY-CD      EL320
05473                  MOVE '2'                   TO RF-RECORD-TYPE     EL320
05474                  MOVE OLC-REPORT-NAME       TO RF-REPORT-ID       EL320
05475                  MOVE ZERO                  TO RF-LINE-NUMBER     EL320
05476                  START ELREPT  KEY NOT LESS RF-CONTROL-PRIMARY    EL320
05477                  PERFORM DTE-REPORT-DELETE THRU DTE-DELETE-EXIT   EL320
05478                  MOVE DTE-CLASIC-COMPANY-CD TO RF-COMPANY-CD      EL320
05479                  MOVE '1'                   TO RF-RECORD-TYPE     EL320
05480                  MOVE OLC-REPORT-NAME       TO RF-REPORT-ID       EL320
05481                  MOVE SPACES                TO RF-REPORT-LINE-133.EL320
05482                                                                   EL320
05483      IF DTE-ABEND-CD-1 = '81' AND                                 EL320
05484         DTE-PRT-OPT    = 'S'                                      EL320
05485          MOVE +0302  TO WS-RETURN-CODE                            EL320
05486          GO TO ABEND-PGM.                                         EL320
05487                                                                   EL320
05488      IF DTE-PRT-OPT = 'S' OR 'T'                                  EL320
05489          MOVE X      TO RF-CTL-CHAR-133                           EL320
05490          MOVE P-DATA TO RF-DATA-133                               EL320
05491              IF DTE-ABEND-CD-1 = SPACES                           EL320
05492                  ADD +1 TO DTE-TOT-LINES                          EL320
05493                  MOVE DTE-TOT-LINES TO RF-LINE-NUMBER             EL320
05494                  WRITE REPORT-SAVE-FILE                           EL320
05495                      INVALID KEY                                  EL320
05496                          MOVE '88' TO DTE-ABEND-CD-1              EL320
05497                          CLOSE ELREPT                             EL320
05498                          MOVE SPACE TO REPT-OPEN.                 EL320
05499                                                                   EL320
05500      IF DTE-FICH NOT = SPACE                                      EL320
05501          MOVE X TO P-CTL                                          EL320
05502          WRITE FICH-REC FROM PRT.                                 EL320
05503                                                                   EL320
05504      IF DTE-PRT-OPT = 'P' OR 'B' OR 'T'                           EL320
05505          MOVE X TO P-CTL                                          EL320
05506          WRITE PRT.                                               EL320
05507                                                                   EL320
05508      GO TO DTE-PRINT-EXIT.                                        EL320
05509                                                                   EL320
05510  DTE-REPORT-DELETE.                                               EL320
05511                                                                   EL320
05512      IF DTE-F-1 NOT = ZERO                                        EL320
05513          MOVE ZERO TO DTE-VSAM-FLAGS                              EL320
05514          GO TO DTE-DELETE-EXIT.                                   EL320
05515                                                                   EL320
05516      READ ELREPT   NEXT RECORD                                    EL320
05517            AT END   GO TO DTE-DELETE-EXIT.                        EL320
05518                                                                   EL320
05519      IF DTE-CLASIC-COMPANY-CD = RF-COMPANY-CD  AND                EL320
05520         OLC-REPORT-NAME       = RF-REPORT-ID                      EL320
05521          DELETE ELREPT RECORD                                     EL320
05522          GO TO DTE-REPORT-DELETE.                                 EL320
05523                                                                   EL320
05524  DTE-DELETE-EXIT.                                                 EL320
05525      EXIT.                                                        EL320
05526                                                                   EL320
05527  DTE-PRINT-EXIT.                                                  EL320
05528      EXIT.                                                        EL320
05529 ******************************************************************EL320
05530                                                                   EL320
05531  1300-EXIT.                                                       EL320
05532      EXIT.                                                        EL320
05533                                                                   EL320
05534  9999-FINALIZE.                                                   EL320
05535      CLOSE  ELCNTLF                                               EL320
05536             ELPGMSF                                               EL320
05537             ELFORMF                                               EL320
05538             PRINTF.                                               EL320
05539                                                                   EL320
05540      IF ELCNTL-STATUS  NOT EQUAL  ZERO                            EL320
05541          MOVE 'ERROR OCCURED CLOSE - ELCNTL'                      EL320
05542                                  TO  WS-ABEND-MESSAGE             EL320
05543          MOVE ELCNTL-STATUS      TO  WS-ABEND-FILE-STATUS         EL320
05544          PERFORM  ABEND-PGM.                                      EL320
05545                                                                   EL320
05546      IF ELFORM-STATUS  NOT EQUAL  ZERO                            EL320
05547          MOVE 'ERROR OCCURED CLOSE - ELFORM'                      EL320
05548                                  TO  WS-ABEND-MESSAGE             EL320
05549          MOVE ELFORM-STATUS      TO  WS-ABEND-FILE-STATUS         EL320
05550          PERFORM  ABEND-PGM.                                      EL320
05551                                                                   EL320
05552      IF ELPGMS-STATUS  NOT EQUAL  ZERO                            EL320
05553          MOVE 'ERROR OCCURED CLOSE - ELPGMS'                      EL320
05554                                  TO  WS-ABEND-MESSAGE             EL320
05555          MOVE ELPGMS-STATUS      TO  WS-ABEND-FILE-STATUS         EL320
05556          PERFORM  ABEND-PGM.                                      EL320
05557                                                                   EL320
05558  9999-CLOSE-OTHER.                                                EL320
05559                             COPY ELCPRTCX.                        EL320
05560                                                                   EL320
05561  9999-EXIT.                                                       EL320
05562      GOBACK.                                                      EL320
05563                                                                   EL320
05564  ABEND-PGM SECTION.          COPY ELCABEND.                       EL320
