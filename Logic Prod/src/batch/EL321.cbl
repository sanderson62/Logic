00001  IDENTIFICATION DIVISION.                                         04/18/98
00002                                                                   EL321
00003  PROGRAM-ID.                 EL321 .                                 LV004
00004 *              PROGRAM CONVERTED BY                               EL321
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL321
00006 *              CONVERSION DATE 02/14/96 13:40:47.                 EL321
00007 *           PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE             EL321
00008 *                            VMOD=2.005                           EL321
00009                                                                   EL321
00010 *AUTHOR.     LOGIC INC.                                           EL321
00011 *            DALLAS, TEXAS.                                       EL321
00012                                                                   EL321
00012                                                                   EL321
00013 *DATE-COMPILED.                                                   EL321
00014                                                                   EL321
00015 *SECURITY.   *****************************************************EL321
00016 *            *                                                   *EL321
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *EL321
00018 *            *                                                   *EL321
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL321
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL321
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *EL321
00022 *            *                                                   *EL321
00023 *            *****************************************************EL321
00024                                                                   EL321
00025 *REMARKS.                                                         EL321
00026                                                                   EL321
00027 *         THIS PROGRAM WILL PRODUCE THE FOLLOWING REPORTS         EL321
00028                                                                   EL321
00029 *       FROM THE ELERRS  FILE:                                    EL321
00030 *           REPORT OF  ERROR MESSAGES            ELERRS           EL321
00031                                                                   EL321
00032 *       FROM THE ELPGMN  FILE:                                    EL321
00033 *           REPORT OF  PROGRAM NAMES             ELPGMN           EL321
00034                                                                   EL321
00035 *       FROM THE ELPGMO  FILE:                                    EL321
00036 *           REPORT OF  AVAILABLE PROG OPTIONS    ELPGMO           EL321
00037                                                                   EL321
00038 *       FROM THE ELHELP  FILE:                                    EL321
00039 *           REPORT OF  HELP SERVICE TEXT         ELHELP           EL321
00040                                                                   EL321
00041 ****************   PROGRAM SWITCHES   ***************             EL321
00042 *    PROCESS 1 = ALL FILES                                        EL321
00043 *    PROCESS 2 = PROGRAM NAMES AND OPTIONS                        EL321
00044 *    PROCESS 3 = ERRORS ONLY                                      EL321
00045 *    PROCESS 4 = HELP ONLY                                        EL321
00046                                                                   EL321
00047                                                                   EL321
00048  ENVIRONMENT DIVISION.                                            EL321
00049                                                                   EL321
00050  INPUT-OUTPUT SECTION.                                            EL321
00051                                                                   EL321
00052  FILE-CONTROL.                                                    EL321
00053                                                                   EL321
00054      SELECT PRINTF      ASSIGN TO SYS008-UR-1403-SYS008.          EL321
00055                                                                   EL321
00056      SELECT FICH        ASSIGN TO SYS020-UT-2400-SYS020.          EL321
00057                                                                   EL321
00058      SELECT DISK-DATE   ASSIGN TO SYS019-FBA1-SYS019.             EL321
00059                                                                   EL321
00060                                                                   EL321
00061      SELECT ELERRSF        ASSIGN TO SYS011-FBA1-ELERRS           EL321
00062              ORGANIZATION IS INDEXED                              EL321
00063              ACCESS IS DYNAMIC                                    EL321
00064              RECORD KEY  IS EM-CONTROL-PRIMARY                    EL321
00065              FILE STATUS IS EM-STATUS.                            EL321
00066                                                                   EL321
00067      SELECT ELPGMNF        ASSIGN TO SYS012-FBA1-ELPGMN           EL321
00068              ORGANIZATION IS INDEXED                              EL321
00069              ACCESS IS DYNAMIC                                    EL321
00070              RECORD KEY  IS PN-CONTROL-PRIMARY                    EL321
00071              FILE STATUS IS PN-STATUS.                            EL321
00072                                                                   EL321
00073      SELECT ELPGMOF        ASSIGN TO SYS013-FBA1-ELPGMO           EL321
00074              ORGANIZATION IS INDEXED                              EL321
00075              ACCESS IS DYNAMIC                                    EL321
00076              RECORD KEY  IS PO-CONTROL-PRIMARY                    EL321
00077              FILE STATUS IS PO-STATUS.                            EL321
00078                                                                   EL321
00079      SELECT ELHELPF        ASSIGN TO SYS014-FBA1-ELHELP           EL321
00080              ORGANIZATION IS INDEXED                              EL321
00081              ACCESS IS DYNAMIC                                    EL321
00082              RECORD KEY  IS TX-CONTROL-PRIMARY                    EL321
00083              FILE STATUS IS TX-STATUS.                            EL321
00084                                                                   EL321
00085      SELECT ELREPT         ASSIGN TO SYS018-FBA1-ELREPT           EL321
00086              ORGANIZATION IS INDEXED                              EL321
00087              ACCESS IS DYNAMIC                                    EL321
00088              RECORD KEY  IS RF-CONTROL-PRIMARY                    EL321
00089              FILE STATUS IS DTE-VSAM-FLAGS.                       EL321
00090                                                                   EL321
00091      EJECT                                                        EL321
00092                                                                   EL321
00093  DATA DIVISION.                                                   EL321
00094                                                                   EL321
00095  FILE SECTION.                                                    EL321
00096                                                                   EL321
00097  FD  PRINTF                  COPY ELCPRTFD.                       EL321
00098                                                                   EL321
00099  FD  FICH                    COPY ELCFCHFD.                       EL321
00100                                                                   EL321
00101      EJECT                                                        EL321
00102  FD  DISK-DATE               COPY ELCDTEFD.                       EL321
00103                                                                   EL321
00104      EJECT                                                        EL321
00105  FD  ELREPT                  COPY ELCRPTFD.                       EL321
00106                              COPY ELCREPT.                        EL321
00107                                                                   EL321
00108      EJECT                                                        EL321
00109  FD  ELERRSF.                                                     EL321
00110                              COPY ELCERRS.                        EL321
00111                                                                   EL321
00112      EJECT                                                        EL321
00113  FD  ELPGMNF.                                                     EL321
00114                              COPY ELCPGMN.                        EL321
00115                                                                   EL321
00116      EJECT                                                        EL321
00117  FD  ELPGMOF.                                                     EL321
00118                              COPY ELCPGMO.                        EL321
00119                                                                   EL321
00120      EJECT                                                        EL321
00121  FD  ELHELPF.                                                     EL321
00122                              COPY ELCTEXT.                        EL321
00123                                                                   EL321
00124      EJECT                                                        EL321
00125  WORKING-STORAGE SECTION.                                         EL321
00126  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      EL321
00127  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      EL321
00128                                                                   EL321
00129  77  FILLER  PIC X(32) VALUE '********************************'.  EL321
00130  77  FILLER  PIC X(32) VALUE '     ELC321 WORKING-STORAGE     '.  EL321
00131  77  FILLER  PIC X(32) VALUE '********** V/M 2.005 ***********'.  EL321
00132                                                                   EL321
00133  77  WS-ELERRS-EOF-SW       PIC X    VALUE SPACE.                 EL321
00134      88  END-OF-ERRS-FILE            VALUE 'E'.                   EL321
00135                                                                   EL321
00136  77  WS-ELPGMN-EOF-SW       PIC X    VALUE SPACE.                 EL321
00137      88  END-OF-PGMN-FILE            VALUE 'E'.                   EL321
00138                                                                   EL321
00139  77  WS-ELPGMO-EOF-SW       PIC X    VALUE SPACE.                 EL321
00140      88  END-OF-PGMO-FILE            VALUE 'E'.                   EL321
00141                                                                   EL321
00142  77  WS-ELHELP-EOF-SW       PIC X    VALUE SPACE.                 EL321
00143      88  END-OF-TEXT-FILE            VALUE 'E'.                   EL321
00144                                                                   EL321
00145  77  WS-FIRST-TIME-SW       PIC X    VALUE '1'.                   EL321
00146      88  FIRST-TIME                  VALUE '1'.                   EL321
00147                                                                   EL321
00148  77  WS-ELCNTL-EOF-SW       PIC X    VALUE SPACE.                 EL321
00149      88  END-OF-CNTL-FILE            VALUE 'E'.                   EL321
00150                                                                   EL321
00151  77  WS-CO-CHG-SW           PIC X    VALUE SPACE.                 EL321
00152      88  COMPANY-CHANGED             VALUE 'X'.                   EL321
00153                                                                   EL321
00154  77  ERROR-SW               PIC X    VALUE SPACE.                 EL321
00155      88  ERROR-OCCURRED              VALUE 'E'.                   EL321
00156      88  NO-ERRORS                   VALUE ' '.                   EL321
00157                                                                   EL321
00158  77  WS-COMP-NOTFND-SW      PIC X    VALUE ZEROS.                 EL321
00159      88  COMP-NOTFND                 VALUE 'N'.                   EL321
00160                                                                   EL321
00161  77  SS                    PIC X     VALUE ' '.                   EL321
00162  77  DS                    PIC X     VALUE '0'.                   EL321
00163  77  TS                    PIC X     VALUE '-'.                   EL321
00164  77  TP                    PIC X     VALUE '1'.                   EL321
00165                                                                   EL321
00166  77  WS-YES                 PIC XXX  VALUE 'YES'.                 EL321
00167                                                                   EL321
00168  77  WS-NO                  PIC XXX  VALUE 'NO '.                 EL321
00169                                                                   EL321
00170  77  WS-STATE-SAVED         PIC XX   VALUE SPACES.                EL321
00171                                                                   EL321
00172  77  LB-SUB                  PIC 99   VALUE ZEROS.                EL321
00173  77  AB-SUB                  PIC 99   VALUE ZEROS.                EL321
00174  77  ST-SUB                  PIC 99   VALUE ZEROS.                EL321
00175  77  TERM-SUB                PIC 99   VALUE ZEROS.                EL321
00176  77  PGMS-SUB                PIC 99   VALUE ZEROS.                EL321
00177                                                                   EL321
00178  77  X                      PIC X     VALUE SPACE.                EL321
00179  77  PGM-SUB                PIC S9(4) COMP VALUE +321.            EL321
00180  77  ABEND-CODE             PIC X     VALUE SPACE.                EL321
00181  77  ABEND-OPTION           PIC X(4)  VALUE SPACE.                EL321
00182  77  OLC-REPORT-NAME        PIC X(5)  VALUE 'EL321'.              EL321
00183                                                                   EL321
00184  77  WS-ZERO                PIC S9    VALUE ZERO  COMP-3.         EL321
00185  77  WS-RETURN-CODE         PIC S9(4) COMP VALUE ZERO.            EL321
00186                                                                   EL321
00187  77  WS-ABEND-MESSAGE       PIC X(80)                VALUE SPACES.EL321
00188  77  WS-ABEND-FILE-STATUS   PIC XX                   VALUE ZERO.  EL321
00189                                                                   EL321
00190  77  WS-RECORD-TYPE         PIC X     VALUE SPACE.                EL321
00191                                                                   EL321
00192  77  WS-SAVED-FORM-NO       PIC X(12) VALUE SPACE.                EL321
00193                                                                   EL321
00194  77  WS-SAVED-CO            PIC XXX   VALUE SPACES.               EL321
00195                                                                   EL321
00196  77  WS-HELP-ACCESS         PIC X(12) VALUE SPACES.               EL321
00197                                                                   EL321
00198  77  WS-PGMO-NAME           PIC X(5) VALUE SPACES.                EL321
00199                                                                   EL321
00200                                                                   EL321
00201  01  WS-PHONE               PIC 9(11).                            EL321
00202  01  WS-DUM-PHONE    REDEFINES WS-PHONE.                          EL321
00203          10  WS-PHONE-AC    PIC 999.                              EL321
00204          10  WS-PHONE-PF    PIC 999.                              EL321
00205          10  WS-PHONE-NO    PIC 9999.                             EL321
00206          10  FILLER         PIC X(01).                            EL321
00207                                                                   EL321
00208  01  WS-PHON-EDIT.                                                EL321
00209          10  WS-ED-AC       PIC 999.                              EL321
00210          10  FILLER         PIC X   VALUE '-'.                    EL321
00211          10  WS-ED-PF       PIC 999.                              EL321
00212          10  FILLER         PIC X   VALUE '-'.                    EL321
00213          10  WS-ED-NO       PIC 9999.                             EL321
00214                                                                   EL321
00215  01  WS-CONTROL-PRIMARY.                                          EL321
00216      05  WS-COMPANY-CD      PIC X.                                EL321
00217      05  FILLER             PIC X(14).                            EL321
00218                                                                   EL321
00219  01  CF-STATUS.                                                   EL321
00220      05  CF-STAT-1      PIC X.                                    EL321
00221      05  CF-STAT-2      PIC X.                                    EL321
00222                                                                   EL321
00223  01  EM-STATUS.                                                   EL321
00224      05  EM-STAT-1      PIC X.                                    EL321
00225      05  EM-STAT-2      PIC X.                                    EL321
00226                                                                   EL321
00227  01  PN-STATUS.                                                   EL321
00228      05  PN-STAT-1      PIC X.                                    EL321
00229      05  PN-STAT-2      PIC X.                                    EL321
00230                                                                   EL321
00231  01  PO-STATUS.                                                   EL321
00232      05  PO-STAT-1      PIC X.                                    EL321
00233      05  PO-STAT-2      PIC X.                                    EL321
00234                                                                   EL321
00235  01  TX-STATUS.                                                   EL321
00236      05  TX-STAT-1      PIC X.                                    EL321
00237      05  TX-STAT-2      PIC X.                                    EL321
00238                                                                   EL321
00239                                                                   EL321
00240  01  PRINT-CONTROL-WORK-AREA.                                     EL321
00241                                                                   EL321
00242      05  WS-LINE-CNT      PIC S9(3)   VALUE +1       COMP-3.      EL321
00243                                                                   EL321
00244      05  WS-PAGE-CNT      PIC S9(5)   VALUE +1       COMP-3.      EL321
00245                                                                   EL321
00246      05  BLANKLINE        PIC X(132) VALUE SPACES.                EL321
00247                                                                   EL321
00248      05  WS-MAINT-TIME    PIC 9(6).                               EL321
00249      05  WS-MAINT-TM  REDEFINES WS-MAINT-TIME.                    EL321
00250          10  WS-MAINT-HH  PIC 99.                                 EL321
00251          10  WS-MAINT-MM  PIC 99.                                 EL321
00252          10  WS-MAINT-SS  PIC 99.                                 EL321
00253                                                                   EL321
00254      05  WS-CURRENT-DATE-MDY.                                     EL321
00255          10  WS-CURR-MO   PIC 99.                                 EL321
00256          10  WS-CURR-DY   PIC 99.                                 EL321
00257          10  WS-CURR-YR   PIC 99.                                 EL321
00258                                                                   EL321
00259      EJECT                                                        EL321
00260      COPY ELCDATE.                                                   CL**4
00261                                                                   EL321
00262      EJECT                                                        EL321
00263 **               PRINT-AREAS                                   ***EL321
00264 ******************************************************************EL321
00265  01  COMMON-HEADING-LINES.                                        EL321
00266      05  HEAD-LINE-3.                                             EL321
00267          15  FILLER             PIC X(22) VALUE SPACE.            EL321
00268          15  FILLER             PIC X(35) VALUE                   EL321
00269                             '    CLAS-IC SYSTEM FILE LISTING    '.EL321
00270          15  FILLER             PIC X(15) VALUE SPACES.           EL321
00271          15  FILLER             PIC X(8) VALUE 'EL321  '.         EL321
00272                                                                   EL321
00273                                                                   EL321
00274      05  HEAD-LINE-4.                                             EL321
00275          15  FILLER             PIC X(25) VALUE SPACE.            EL321
00276          15  HEAD-CLIENT        PIC X(30) VALUE SPACES.           EL321
00277          15  FILLER             PIC X(17) VALUE SPACES.           EL321
00278          15  HEAD-RUN-DATE      PIC X(8).                         EL321
00279                                                                   EL321
00280      05  HEAD-LINE-5.                                             EL321
00281          15  FILLER             PIC X(30) VALUE SPACE.            EL321
00282          15  HEAD-RUN-DATE-FULL PIC X(19).                        EL321
00283          15  FILLER             PIC X(19) VALUE SPACES.           EL321
00284          15  FILLER             PIC X(5)  VALUE 'PAGE '.          EL321
00285          15  HEAD-PAGE-NO       PIC ZZZZ9-.                       EL321
00286                                                                   EL321
00287 ***      OTHER PRINT LINES     ********************************   EL321
00288                                                                   EL321
00289      05  ERRS-LINE-8.                                             EL321
00290          15  FILLER             PIC X(28) VALUE                   EL321
00291                             'RECORD TYPE - ERROR MESSAGES'.       EL321
00292                                                                   EL321
00293      05  ERRS-LINE-11.                                            EL321
00294          15  FILLER             PIC X(26) VALUE                   EL321
00295              'NUMBER SEV   TEXT OF ERROR'.                        EL321
00296                                                                   EL321
00297      05  ERRS-DETAIL.                                             EL321
00298          15  FILLER             PIC X     VALUE SPACE.            EL321
00299          15  ERRS-NO            PIC XXXX.                         EL321
00300          15  FILLER             PIC XXX   VALUE  SPACE.           EL321
00301          15  ERRS-SEV           PIC X.                            EL321
00302          15  FILLER             PIC XXXX  VALUE SPACE.            EL321
00303          15  ERRS-TEXT          PIC X(65).                        EL321
00304                                                                   EL321
00305                                                                   EL321
00306      05  PGMN-LINE-8.                                             EL321
00307          15  FILLER             PIC X(28) VALUE                   EL321
00308                             'RECORD TYPE - PROGRAM NAMES '.       EL321
00309                                                                   EL321
00310      05  PGMN-LINE-11.                                            EL321
00311          15  FILLER             PIC X(38) VALUE                   EL321
00312              'NAME   TRANS CD  ENTERED   DESCRIPTION'.            EL321
00313                                                                   EL321
00314      05  PGMN-DETAIL.                                             EL321
00315          15  PGMN-NAME          PIC X(6).                         EL321
00316          15  FILLER             PIC XXX   VALUE  SPACE.           EL321
00317          15  PGMN-CD            PIC X(4).                         EL321
00318          15  FILLER             PIC X(5) VALUE SPACE.             EL321
00319          15  PGMN-ENTERED       PIC X(7).                         EL321
00320          15  FILLER             PIC X(3) VALUE SPACE.             EL321
00321          15  PGMN-TEXT          PIC X(70).                        EL321
00322                                                                   EL321
00323      05  PGMO-LINE-8.                                             EL321
00324          15  FILLER             PIC X(39) VALUE                   EL321
00325                       'RECORD TYPE - AVAILABLE PROGRAM OPTIONS'.  EL321
00326                                                                   EL321
00327      05  PGMO-LINE-11.                                            EL321
00328          15  FILLER             PIC X(38) VALUE                   EL321
00329              'PROGRAM  OPTION  OPTION   DESCRIPTION '.            EL321
00330                                                                   EL321
00331      05  PGMO-LINE-12.                                            EL321
00332          15  FILLER             PIC X(23) VALUE                   EL321
00333              '          TYPE   NUMBER'.                           EL321
00334                                                                   EL321
00335      05  PGMO-DETAIL.                                             EL321
00336          15  PGMO-NAME          PIC X(5).                         EL321
00337          15  FILLER             PIC X(5) VALUE   SPACE.           EL321
00338          15  PGMO-TYPE          PIC X(5).                         EL321
00339          15  FILLER             PIC X(4) VALUE SPACE.             EL321
00340          15  PGMO-OPTION        PIC X.                            EL321
00341          15  FILLER             PIC X(6) VALUE SPACE.             EL321
00342          15  PGMO-TEXT          PIC X(70).                        EL321
00343                                                                   EL321
00344      05  HELP-LINE-8.                                             EL321
00345          15  FILLER             PIC X(41) VALUE                   EL321
00346                             'RECORD TYPE - HELP SERVICE TEXT'.    EL321
00347                                                                   EL321
00348      05  HELP-LINE-11.                                            EL321
00349          15  FILLER             PIC X(11) VALUE                   EL321
00350              'HELP FOR - '.                                       EL321
00351          15  HELP-FOR           PIC X(7)    VALUE SPACES.         EL321
00352          15  FILLER             PIC X(11)   VALUE                 EL321
00353              '  NUMBER - '.                                       EL321
00354          15  HELP-NUMBER.                                         EL321
00355              25  HELP-NO-1          PIC XX    VALUE SPACES.       EL321
00356              25  HELP-NO-2          PIC XXXX  VALUE SPACES.       EL321
00357                                                                   EL321
00358      05  HELP-LINE-12.                                            EL321
00359          15  FILLER             PIC X(10) VALUE                   EL321
00360              'LINE  TEXT'.                                        EL321
00361                                                                   EL321
00362      05  HELP-DETAIL.                                             EL321
00363          15  HELP-LINE          PIC 9(04).                        EL321
00364          15  FILLER             PIC XX    VALUE  SPACE.           EL321
00365          15  HELP-TEXT          PIC X(70).                        EL321
00366          15  FILLER             PIC XX    VALUE SPACE.            EL321
00367          15  HELP-CF            PIC ZZ.                           EL321
00368                                                                   EL321
00369      EJECT                                                        EL321
00370      COPY ELCDTECX.                                               EL321
00371                                                                   EL321
00372      COPY ELCDTEVR.                                                  CL**2
00373                                                                      CL**2
00374 ****************************************************************  EL321
00375      EJECT                                                        EL321
00376  PROCEDURE DIVISION.                                              EL321
00377                                                                   EL321
00378  0000-LOAD-DATE-CARD.                                             EL321
00379                                 COPY ELCDTERX.                       CL**2
00380  0000-MAINLINE.                                                   EL321
00381                                                                   EL321
00382      PERFORM 1000-INITIALIZE     THRU 1000-EXIT.                  EL321
00383                                                                   EL321
00384  0001-ERRORS-LIST.                                                EL321
00385      IF DTE-PRC-OPT NOT = '1' AND '3'                             EL321
00386         GO TO 0002-HELP-LIST.                                     EL321
00387                                                                   EL321
00388      OPEN INPUT  ELERRSF.                                         EL321
00389                                                                   EL321
00390      IF EM-STATUS  = '00' OR '97'                                 EL321
00391          NEXT SENTENCE                                            EL321
00392        ELSE                                                       EL321
00393          DISPLAY '***OPEN ERROR ELERRS***'   EM-STATUS            EL321
00394          GO TO 9999-PROGRAM-END.                                  EL321
00395                                                                   EL321
00396      MOVE 1                      TO WS-PAGE-CNT.                  EL321
00397      PERFORM  1200-HEADING       THRU 1200-EXIT.                  EL321
00398      PERFORM  3100-HEADING       THRU 3100-EXIT.                  EL321
00399                                                                   EL321
00400      PERFORM 3000-PROCESS-ERRS-FILE THRU 3000-EXIT                EL321
00401         UNTIL END-OF-ERRS-FILE.                                   EL321
00402                                                                   EL321
00403      CLOSE ELERRSF.                                               EL321
00404                                                                   EL321
00405  0002-HELP-LIST.                                                  EL321
00406      IF DTE-PRC-OPT NOT = '1' AND '4'                             EL321
00407         GO TO 0003-NAMES-LIST.                                    EL321
00408                                                                   EL321
00409      OPEN INPUT  ELHELPF.                                         EL321
00410                                                                   EL321
00411      IF TX-STATUS  = '00' OR '97'                                 EL321
00412          NEXT SENTENCE                                            EL321
00413        ELSE                                                       EL321
00414          DISPLAY '***OPEN ERROR ELHELP***'                        EL321
00415          DISPLAY TX-STATUS                                        EL321
00416          GO TO 9999-FINALIZE.                                     EL321
00417                                                                   EL321
00418      MOVE 1                      TO WS-PAGE-CNT.                  EL321
00419      PERFORM  1200-HEADING       THRU 1200-EXIT.                  EL321
00420                                                                   EL321
00421      PERFORM 4000-PROCESS-TEXT-FILE THRU 4000-EXIT                EL321
00422         UNTIL END-OF-TEXT-FILE.                                   EL321
00423                                                                   EL321
00424      CLOSE ELHELPF.                                               EL321
00425                                                                   EL321
00426  0003-NAMES-LIST.                                                 EL321
00427      IF DTE-PRC-OPT NOT = '1' AND '2'                             EL321
00428          GO TO 9999-FINALIZE.                                     EL321
00429                                                                   EL321
00430      OPEN INPUT  ELPGMNF.                                         EL321
00431                                                                   EL321
00432      IF PN-STATUS  = '00' OR '97'                                 EL321
00433          NEXT SENTENCE                                            EL321
00434        ELSE                                                       EL321
00435          DISPLAY '***OPEN ERROR ELPGMN***'   PN-STATUS            EL321
00436          GO TO 9999-FINALIZE.                                     EL321
00437                                                                   EL321
00438      MOVE 1                      TO WS-PAGE-CNT.                  EL321
00439      PERFORM  1200-HEADING       THRU 1200-EXIT.                  EL321
00440                                                                   EL321
00441      PERFORM  5100-HEADING       THRU 5100-EXIT.                  EL321
00442                                                                   EL321
00443      PERFORM 5000-PROCESS-PGMN-FILE THRU 5000-EXIT                EL321
00444         UNTIL END-OF-PGMN-FILE.                                   EL321
00445                                                                   EL321
00446      OPEN INPUT  ELPGMOF.                                         EL321
00447                                                                   EL321
00448      IF PO-STATUS  = '00' OR '97'                                 EL321
00449          NEXT SENTENCE                                            EL321
00450        ELSE                                                       EL321
00451          DISPLAY '***OPEN ERROR ELPGMO***'    PO-STATUS           EL321
00452          GO TO 9999-FINALIZE.                                     EL321
00453                                                                   EL321
00454      MOVE 1                      TO WS-PAGE-CNT.                  EL321
00455                                                                   EL321
00456      PERFORM  1200-HEADING       THRU 1200-EXIT.                  EL321
00457                                                                   EL321
00458      PERFORM  6100-HEADING       THRU 6100-EXIT.                  EL321
00459                                                                   EL321
00460      PERFORM  6000-PROCESS-PGMO-FILE THRU 6000-EXIT               EL321
00461         UNTIL END-OF-PGMO-FILE.                                   EL321
00462                                                                   EL321
00463      CLOSE ELPGMOF ELPGMNF.                                       EL321
00464      GO TO 9999-FINALIZE.                                         EL321
00465                                                                   EL321
00466  0000-EXIT.                                                       EL321
00467       EXIT.                                                       EL321
00468                                                                   EL321
00469      EJECT                                                        EL321
00470  3000-PROCESS-ERRS-FILE.                                          EL321
00471      READ ELERRSF  NEXT RECORD.                                   EL321
00472                                                                   EL321
00473      IF EM-STAT-1  = '1'                                          EL321
00474           MOVE 'E'               TO WS-ELERRS-EOF-SW              EL321
00475           GO TO 3000-EXIT.                                        EL321
00476                                                                   EL321
00477      IF EM-STAT-1  NOT = '0'                                      EL321
00478           DISPLAY '**ERROR READING ELERRS FILE**'                 EL321
00479           DISPLAY EM-STATUS                                       EL321
00480           MOVE 'E'               TO WS-ELERRS-EOF-SW              EL321
00481           GO TO 3000-EXIT.                                        EL321
00482                                                                   EL321
00483      MOVE EM-MESSAGE-NUMBER      TO ERRS-NO.                      EL321
00484      MOVE EM-ERROR-SEVERITY      TO ERRS-SEV.                     EL321
00485      MOVE EM-ERROR-TEXT          TO ERRS-TEXT.                    EL321
00486                                                                   EL321
00487      MOVE SS                     TO X.                            EL321
00488      MOVE ERRS-DETAIL            TO P-DATA.                       EL321
00489      PERFORM 1300-PRINT-RTN      THRU 1300-EXIT.                  EL321
00490                                                                   EL321
00491      ADD 1                       TO WS-LINE-CNT.                  EL321
00492                                                                   EL321
00493      IF WS-LINE-CNT GREATER THAN 60                               EL321
00494          PERFORM  1200-HEADING   THRU 1200-EXIT                   EL321
00495          PERFORM  3100-HEADING   THRU 3100-EXIT.                  EL321
00496                                                                   EL321
00497  3000-EXIT.                                                       EL321
00498       EXIT.                                                       EL321
00499                                                                   EL321
00500  3100-HEADING.                                                    EL321
00501      MOVE TS                     TO X.                            EL321
00502      MOVE ERRS-LINE-8            TO   P-DATA.                     EL321
00503      PERFORM 1300-PRINT-RTN      THRU 1300-EXIT.                  EL321
00504      MOVE TS                     TO X.                            EL321
00505      MOVE ERRS-LINE-11           TO   P-DATA.                     EL321
00506      PERFORM 1300-PRINT-RTN      THRU 1300-EXIT.                  EL321
00507                                                                   EL321
00508      MOVE SS                     TO X.                            EL321
00509      MOVE SPACES                 TO   P-DATA.                     EL321
00510                                                                   EL321
00511      PERFORM 1300-PRINT-RTN      THRU 1300-EXIT.                  EL321
00512       MOVE 13                    TO WS-LINE-CNT.                  EL321
00513                                                                   EL321
00514  3100-EXIT.                                                       EL321
00515       EXIT.                                                       EL321
00516      EJECT                                                        EL321
00517                                                                   EL321
00518  4000-PROCESS-TEXT-FILE.                                          EL321
00519      READ ELHELPF  NEXT RECORD                                    EL321
00520                                                                   EL321
00521      IF TX-STAT-1  = '1'                                          EL321
00522           MOVE 'E'               TO WS-ELHELP-EOF-SW              EL321
00523           GO TO 4000-EXIT.                                        EL321
00524                                                                   EL321
00525      IF TX-STAT-1  NOT = '0'                                      EL321
00526           DISPLAY '**ERROR READING ELTEXT FILE**'                 EL321
00527           DISPLAY TX-STATUS                                       EL321
00528           MOVE 'E'               TO WS-ELHELP-EOF-SW              EL321
00529           GO TO 4000-EXIT.                                        EL321
00530 *                                                                 EL321
00531       IF HELP-FILE-TEXT                                           EL321
00532           NEXT SENTENCE                                           EL321
00533       ELSE                                                        EL321
00534           GO TO 4000-EXIT.                                        EL321
00535                                                                   EL321
00536       IF TX-HELP-ACCESS   = WS-HELP-ACCESS                        EL321
00537           GO TO 4000-DETAIL-LINE.                                 EL321
00538                                                                   EL321
00539       MOVE TX-HELP-ACCESS        TO WS-HELP-ACCESS.               EL321
00540                                                                   EL321
00541       IF WS-LINE-CNT GREATER THAN 50                              EL321
00542            PERFORM 1200-HEADING  THRU 1200-EXIT.                  EL321
00543                                                                   EL321
00544       IF HELP-FOR-GENERAL                                         EL321
00545            MOVE 'GENERAL'        TO HELP-FOR                      EL321
00546            MOVE '  '             TO HELP-NO-1                     EL321
00547            MOVE TX-SCREEN-OR-ERROR TO  HELP-NO-2.                 EL321
00548                                                                   EL321
00549       IF HELP-BY-SCREEN                                           EL321
00550            MOVE 'SCREEN'         TO HELP-FOR                      EL321
00551            MOVE 'EL'             TO HELP-NO-1                     EL321
00552            MOVE TX-SCREEN-OR-ERROR TO  HELP-NO-2.                 EL321
00553                                                                   EL321
00554       IF HELP-BY-ERROR                                            EL321
00555            MOVE 'ERROR '         TO HELP-FOR                      EL321
00556            MOVE '  '             TO HELP-NO-1                     EL321
00557            MOVE TX-SCREEN-OR-ERROR TO  HELP-NO-2.                 EL321
00558                                                                   EL321
00559       MOVE HELP-LINE-11          TO P-DATA.                       EL321
00560       MOVE TS                    TO X.                            EL321
00561       PERFORM 1300-PRINT-RTN     THRU 1300-EXIT.                  EL321
00562                                                                   EL321
00563       MOVE HELP-LINE-12          TO P-DATA.                       EL321
00564       MOVE SS                    TO X.                            EL321
00565       PERFORM 1300-PRINT-RTN     THRU 1300-EXIT.                  EL321
00566                                                                   EL321
00567       MOVE SPACES                TO P-DATA.                       EL321
00568       MOVE SS                    TO X.                            EL321
00569       PERFORM 1300-PRINT-RTN     THRU 1300-EXIT.                  EL321
00570                                                                   EL321
00571       ADD 5                      TO WS-LINE-CNT.                  EL321
00572                                                                   EL321
00573      EJECT                                                        EL321
00574  4000-DETAIL-LINE.                                                EL321
00575      MOVE TX-TEXT-LINE           TO HELP-TEXT.                    EL321
00576      MOVE TX-LINE-SEQUENCE       TO HELP-LINE.                    EL321
00577      MOVE TX-PROCESS-CONTROL     TO HELP-CF.                      EL321
00578                                                                   EL321
00579      MOVE SS                     TO X.                            EL321
00580      MOVE HELP-DETAIL            TO P-DATA.                       EL321
00581      PERFORM 1300-PRINT-RTN      THRU 1300-EXIT.                  EL321
00582                                                                   EL321
00583      ADD 1 TO WS-LINE-CNT.                                        EL321
00584                                                                   EL321
00585      IF WS-LINE-CNT GREATER THAN 60                               EL321
00586          PERFORM  1200-HEADING   THRU 1200-EXIT                   EL321
00587          PERFORM  4100-HEADING   THRU 4100-EXIT.                  EL321
00588                                                                   EL321
00589  4000-EXIT.                                                       EL321
00590       EXIT.                                                       EL321
00591                                                                   EL321
00592  4100-HEADING.                                                    EL321
00593      MOVE TS                     TO X.                            EL321
00594      MOVE HELP-LINE-8            TO   P-DATA.                     EL321
00595      PERFORM 1300-PRINT-RTN      THRU 1300-EXIT.                  EL321
00596                                                                   EL321
00597      MOVE TS                     TO X.                            EL321
00598      MOVE HELP-LINE-11           TO   P-DATA.                     EL321
00599      PERFORM 1300-PRINT-RTN      THRU 1300-EXIT.                  EL321
00600                                                                   EL321
00601      MOVE SS                     TO X.                            EL321
00602      MOVE HELP-LINE-12           TO   P-DATA.                     EL321
00603      PERFORM 1300-PRINT-RTN      THRU 1300-EXIT.                  EL321
00604                                                                   EL321
00605      MOVE SS                     TO X.                            EL321
00606      MOVE SPACES                 TO   P-DATA.                     EL321
00607      PERFORM 1300-PRINT-RTN      THRU 1300-EXIT.                  EL321
00608                                                                   EL321
00609      MOVE 13                     TO WS-LINE-CNT.                  EL321
00610                                                                   EL321
00611  4100-EXIT.                                                       EL321
00612       EXIT.                                                       EL321
00613                                                                   EL321
00614      EJECT                                                        EL321
00615  5000-PROCESS-PGMN-FILE.                                          EL321
00616      READ ELPGMNF  NEXT RECORD.                                   EL321
00617                                                                   EL321
00618      IF PN-STAT-1  = '1'                                          EL321
00619           MOVE 'E'               TO WS-ELPGMN-EOF-SW              EL321
00620           GO TO 5000-EXIT.                                        EL321
00621                                                                   EL321
00622      IF PN-STAT-1  NOT = '0'                                      EL321
00623           DISPLAY '**ERROR READING ELPGMN FILE**'                 EL321
00624           DISPLAY PN-STATUS                                       EL321
00625           MOVE 'E'               TO WS-ELPGMN-EOF-SW              EL321
00626           GO TO 5000-EXIT.                                        EL321
00627                                                                   EL321
00628       IF PN-SYSTEM-CODE    NOT  = 'EL'                            EL321
00629            GO TO 5000-EXIT.                                       EL321
00630                                                                   EL321
00631       MOVE PN-PROGRAM-NUMBER     TO PGMN-NAME.                    EL321
00632       MOVE SPACES                TO PGMN-CD.                      EL321
00633       MOVE SPACES                TO PGMN-ENTERED.                 EL321
00634       MOVE PN-PROGRAM-DESCRIPTION TO PGMN-TEXT.                   EL321
00635                                                                   EL321
00636       IF ENTERED-FROM-CICS                                        EL321
00637           MOVE 'C.I.C.S'         TO PGMN-ENTERED                  EL321
00638           MOVE PN-TRANSACTION-CODE TO  PGMN-CD.                   EL321
00639                                                                   EL321
00640       IF ENTERED-FROM-CLASIC-MENU                                 EL321
00641           MOVE 'MENU   '         TO PGMN-ENTERED.                 EL321
00642                                                                   EL321
00643                                                                   EL321
00644      MOVE SS                     TO X.                            EL321
00645      MOVE PGMN-DETAIL            TO P-DATA.                       EL321
00646      PERFORM 1300-PRINT-RTN      THRU 1300-EXIT.                  EL321
00647                                                                   EL321
00648      ADD 1                       TO WS-LINE-CNT.                  EL321
00649                                                                   EL321
00650      IF WS-LINE-CNT GREATER THAN 60                               EL321
00651          PERFORM  1200-HEADING  THRU 1200-EXIT                    EL321
00652          PERFORM  5100-HEADING  THRU 5100-EXIT.                   EL321
00653                                                                   EL321
00654  5000-EXIT.                                                       EL321
00655       EXIT.                                                       EL321
00656                                                                   EL321
00657  5100-HEADING.                                                    EL321
00658      MOVE TS                    TO X.                             EL321
00659      MOVE PGMN-LINE-8           TO   P-DATA.                      EL321
00660      PERFORM 1300-PRINT-RTN     THRU 1300-EXIT.                   EL321
00661                                                                   EL321
00662      MOVE TS                    TO X.                             EL321
00663      MOVE PGMN-LINE-11          TO   P-DATA.                      EL321
00664      PERFORM 1300-PRINT-RTN     THRU 1300-EXIT.                   EL321
00665                                                                   EL321
00666      MOVE SS                    TO X.                             EL321
00667      MOVE SPACES                TO   P-DATA.                      EL321
00668      PERFORM 1300-PRINT-RTN     THRU 1300-EXIT.                   EL321
00669                                                                   EL321
00670      MOVE 13                    TO WS-LINE-CNT.                   EL321
00671                                                                   EL321
00672  5100-EXIT.                                                       EL321
00673       EXIT.                                                       EL321
00674                                                                   EL321
00675      EJECT                                                        EL321
00676  6000-PROCESS-PGMO-FILE.                                          EL321
00677      READ ELPGMOF  NEXT RECORD.                                   EL321
00678                                                                   EL321
00679      IF PO-STAT-1  = '1'                                          EL321
00680           MOVE 'E'               TO WS-ELPGMO-EOF-SW              EL321
00681           GO TO 6000-EXIT.                                        EL321
00682                                                                   EL321
00683      IF PO-STAT-1  NOT = '0'                                      EL321
00684           DISPLAY '**ERROR READING ELPGMO FILE**'                 EL321
00685           DISPLAY PO-STATUS                                       EL321
00686           MOVE 'E'               TO WS-ELPGMO-EOF-SW              EL321
00687           GO TO 6000-EXIT.                                        EL321
00688                                                                   EL321
00689       IF PO-SYSTEM-CODE    NOT = 'EL'                             EL321
00690           GO TO 6000-EXIT.                                        EL321
00691                                                                   EL321
00692       IF PO-PROGRAM-NUMBER  = WS-PGMO-NAME                        EL321
00693           MOVE SPACES            TO PGMO-NAME                     EL321
00694       ELSE                                                        EL321
00695           MOVE SS                TO X                             EL321
00696           MOVE SPACES            TO   P-DATA                      EL321
00697           PERFORM 1300-PRINT-RTN THRU 1300-EXIT.                  EL321
00698           MOVE PO-PROGRAM-NUMBER TO PGMO-NAME                     EL321
00699                                     WS-PGMO-NAME.                 EL321
00700                                                                   EL321
00701       MOVE SPACES                TO PGMO-TYPE.                    EL321
00702                                                                   EL321
00703       IF PO-FORMAT-OPTION                                         EL321
00704            MOVE 'FMT  '          TO PGMO-TYPE.                    EL321
00705                                                                   EL321
00706       IF PO-PROCESS-OPTION                                        EL321
00707            MOVE 'PROC '          TO PGMO-TYPE.                    EL321
00708                                                                   EL321
00709       IF PO-TOTAL-OPTION                                          EL321
00710            MOVE 'TOTAL'          TO PGMO-TYPE.                    EL321
00711                                                                   EL321
00712        MOVE PO-PGM-OPTION-CD     TO PGMO-OPTION.                  EL321
00713        MOVE PO-OPTION-DESCRIPTION TO PGMO-TEXT.                   EL321
00714                                                                   EL321
00715      MOVE SS                     TO X.                            EL321
00716      MOVE PGMO-DETAIL            TO P-DATA.                       EL321
00717      PERFORM 1300-PRINT-RTN      THRU 1300-EXIT.                  EL321
00718                                                                   EL321
00719      ADD 1                       TO WS-LINE-CNT.                  EL321
00720                                                                   EL321
00721      IF WS-LINE-CNT GREATER THAN 60                               EL321
00722          PERFORM  1200-HEADING  THRU 1200-EXIT                    EL321
00723          PERFORM  6100-HEADING  THRU 6100-EXIT.                   EL321
00724                                                                   EL321
00725  6000-EXIT.                                                       EL321
00726       EXIT.                                                       EL321
00727                                                                   EL321
00728      EJECT                                                        EL321
00729                                                                   EL321
00730  6100-HEADING.                                                    EL321
00731      MOVE TS                     TO X.                            EL321
00732      MOVE PGMO-LINE-8            TO   P-DATA.                     EL321
00733      PERFORM 1300-PRINT-RTN      THRU 1300-EXIT.                  EL321
00734                                                                   EL321
00735      MOVE TS                     TO X.                            EL321
00736      MOVE PGMO-LINE-11           TO   P-DATA.                     EL321
00737      PERFORM 1300-PRINT-RTN      THRU 1300-EXIT.                  EL321
00738                                                                   EL321
00739      MOVE SS                     TO X.                            EL321
00740      MOVE PGMO-LINE-12           TO   P-DATA.                     EL321
00741      PERFORM 1300-PRINT-RTN      THRU 1300-EXIT.                  EL321
00742                                                                   EL321
00743      MOVE SS                     TO X.                            EL321
00744      MOVE SPACES                 TO   P-DATA.                     EL321
00745      PERFORM 1300-PRINT-RTN      THRU 1300-EXIT.                  EL321
00746                                                                   EL321
00747      MOVE 13                     TO WS-LINE-CNT.                  EL321
00748                                                                   EL321
00749  6100-EXIT.                                                       EL321
00750       EXIT.                                                       EL321
00751                                                                   EL321
00752      EJECT                                                        EL321
00753  1000-INITIALIZE.                                                 EL321
00754      OPEN OUTPUT PRINTF.                                          EL321
00755                                                                   EL321
00756      ACCEPT WS-ACCEPT-DATE       FROM DATE.                       EL321
00757      MOVE WS-AD-YY               TO WS-CD-YY.                     EL321
00758      MOVE WS-AD-MM               TO WS-CD-MM.                     EL321
00759      MOVE WS-AD-DD               TO WS-CD-DD.                     EL321
00760      MOVE WS-CURRENT-DATE        TO HEAD-RUN-DATE.                EL321
00761      MOVE WS-CD-MM               TO WS-CURR-MO.                   EL321
00762      MOVE WS-CD-DD               TO WS-CURR-DY.                   EL321
00763      MOVE WS-CD-YY               TO WS-CURR-YR.                   EL321
00764      MOVE ALPH-DATE              TO HEAD-RUN-DATE-FULL.           EL321
00765      MOVE COMPANY-NAME           TO HEAD-CLIENT.                  EL321
00766      MOVE LOW-VALUES             TO WS-CONTROL-PRIMARY.           EL321
00767                                                                   EL321
00768  1000-EXIT.                                                       EL321
00769       EXIT.                                                       EL321
00770                                                                   EL321
00771      EJECT                                                        EL321
00772  1200-HEADING.                                                    EL321
00773       MOVE WS-PAGE-CNT           TO HEAD-PAGE-NO.                 EL321
00774                                                                   EL321
00775      MOVE TP                     TO X.                            EL321
00776      MOVE HEAD-LINE-3            TO   P-DATA.                     EL321
00777      PERFORM 1300-PRINT-RTN      THRU 1300-EXIT.                  EL321
00778                                                                   EL321
00779      MOVE SS                     TO X.                            EL321
00780      MOVE HEAD-LINE-4            TO   P-DATA.                     EL321
00781      PERFORM 1300-PRINT-RTN      THRU 1300-EXIT.                  EL321
00782                                                                   EL321
00783      MOVE SS                     TO X.                            EL321
00784      MOVE HEAD-LINE-5            TO   P-DATA.                     EL321
00785      PERFORM 1300-PRINT-RTN      THRU 1300-EXIT.                  EL321
00786                                                                   EL321
00787       MOVE 5                     TO WS-LINE-CNT.                  EL321
00788                                                                   EL321
00789       ADD 01                     TO WS-PAGE-CNT.                  EL321
00790                                                                   EL321
00791  1200-EXIT.                                                       EL321
00792       EXIT.                                                       EL321
00793                                                                   EL321
00794  1300-PRINT-RTN.                                                  EL321
00795                                                                   EL321
00796      IF DTE-FICH NOT = SPACE AND                                  EL321
00797          FICH-OPEN   = SPACE                                      EL321
00798          MOVE 'X' TO FICH-OPEN                                    EL321
00799          OPEN OUTPUT FICH.                                        EL321
00800                                                                   EL321
00801      IF DTE-PRT-OPT = 'S' OR 'T'                                  EL321
00802          IF (REPT-OPEN = SPACE) AND (DTE-ABEND-CD-1 = SPACE)      EL321
00803              OPEN I-O ELREPT                                      EL321
00804              IF DTE-F-1 NOT = ZERO AND                            EL321
00805                 DTE-VSAM-FLAGS NOT = '97'                         EL321
00806                  MOVE DTE-VSAM-FLAGS  TO  WS-ABEND-FILE-STATUS    EL321
00807                  MOVE 'ERROR OCCURED OPEN - ELREPT'               EL321
00808                                  TO  WS-ABEND-MESSAGE             EL321
00809                  GO TO ABEND-PGM                                  EL321
00810              ELSE                                                 EL321
00811                  MOVE '1'                   TO REPT-OPEN          EL321
00812                  MOVE DTE-CLASIC-COMPANY-CD TO RF-COMPANY-CD      EL321
00813                  MOVE '1'                   TO RF-RECORD-TYPE     EL321
00814                  MOVE OLC-REPORT-NAME       TO RF-REPORT-ID       EL321
00815                  MOVE ZERO                  TO RF-LINE-NUMBER     EL321
00816                  START ELREPT  KEY NOT LESS RF-CONTROL-PRIMARY    EL321
00817                  PERFORM DTE-REPORT-DELETE THRU DTE-DELETE-EXIT   EL321
00818                  MOVE DTE-CLASIC-COMPANY-CD TO RF-COMPANY-CD      EL321
00819                  MOVE '2'                   TO RF-RECORD-TYPE     EL321
00820                  MOVE OLC-REPORT-NAME       TO RF-REPORT-ID       EL321
00821                  MOVE ZERO                  TO RF-LINE-NUMBER     EL321
00822                  START ELREPT  KEY NOT LESS RF-CONTROL-PRIMARY    EL321
00823                  PERFORM DTE-REPORT-DELETE THRU DTE-DELETE-EXIT   EL321
00824                  MOVE DTE-CLASIC-COMPANY-CD TO RF-COMPANY-CD      EL321
00825                  MOVE '1'                   TO RF-RECORD-TYPE     EL321
00826                  MOVE OLC-REPORT-NAME       TO RF-REPORT-ID       EL321
00827                  MOVE SPACES                TO RF-REPORT-LINE-133.EL321
00828                                                                   EL321
00829      IF DTE-ABEND-CD-1 = '81' AND                                 EL321
00830         DTE-PRT-OPT    = 'S'                                      EL321
00831          MOVE +0302  TO WS-RETURN-CODE                            EL321
00832          GO TO ABEND-PGM.                                         EL321
00833                                                                   EL321
00834      IF DTE-PRT-OPT = 'S' OR 'T'                                  EL321
00835          MOVE X      TO RF-CTL-CHAR-133                           EL321
00836          MOVE P-DATA TO RF-DATA-133                               EL321
00837              IF DTE-ABEND-CD-1 = SPACES                           EL321
00838                  ADD +1 TO DTE-TOT-LINES                          EL321
00839                  MOVE DTE-TOT-LINES TO RF-LINE-NUMBER             EL321
00840                  WRITE REPORT-SAVE-FILE                           EL321
00841                      INVALID KEY                                  EL321
00842                          MOVE '88' TO DTE-ABEND-CD-1              EL321
00843                          CLOSE ELREPT                             EL321
00844                          MOVE SPACE TO REPT-OPEN.                 EL321
00845                                                                   EL321
00846      IF DTE-FICH NOT = SPACE                                      EL321
00847          MOVE X TO P-CTL                                          EL321
00848          WRITE FICH-REC FROM PRT.                                 EL321
00849                                                                   EL321
00850      IF DTE-PRT-OPT = 'P' OR 'B' OR 'T'                           EL321
00851          MOVE X TO P-CTL                                          EL321
00852          WRITE PRT.                                               EL321
00853                                                                   EL321
00854      GO TO DTE-PRINT-EXIT.                                        EL321
00855                                                                   EL321
00856  DTE-REPORT-DELETE.                                               EL321
00857      IF DTE-F-1 NOT = ZERO                                        EL321
00858          MOVE ZERO TO DTE-VSAM-FLAGS                              EL321
00859          GO TO DTE-DELETE-EXIT.                                   EL321
00860                                                                   EL321
00861      READ ELREPT   NEXT RECORD                                    EL321
00862            AT END   GO TO DTE-DELETE-EXIT.                        EL321
00863                                                                   EL321
00864      IF DTE-CLASIC-COMPANY-CD = RF-COMPANY-CD  AND                EL321
00865         OLC-REPORT-NAME       = RF-REPORT-ID                      EL321
00866          DELETE ELREPT RECORD                                     EL321
00867          GO TO DTE-REPORT-DELETE.                                 EL321
00868                                                                   EL321
00869  DTE-DELETE-EXIT.                                                 EL321
00870      EXIT.                                                        EL321
00871                                                                   EL321
00872  DTE-PRINT-EXIT.                                                  EL321
00873      EXIT.                                                        EL321
00874 ******************************************************************EL321
00875                                                                   EL321
00876  1300-EXIT.                                                       EL321
00877       EXIT.                                                       EL321
00878                                                                   EL321
00879      EJECT                                                        EL321
00880  9999-FINALIZE.                                                   EL321
00881                                                                   EL321
00882      CLOSE PRINTF.                                                EL321
00883                                                                   EL321
00884  9999-CLOSE-OTHER.                                                EL321
00885                              COPY ELCPRTCX.                       EL321
00886                                                                   EL321
00887  9999-EXIT.                                                       EL321
00888                                                                   EL321
00889  9999-PROGRAM-END.                                                EL321
00890                                                                   EL321
00891      GOBACK.                                                      EL321
00892                                                                   EL321
00893  ABEND-PGM SECTION. COPY ELCABEND.                                   CL**2
00894                                                                   EL321
