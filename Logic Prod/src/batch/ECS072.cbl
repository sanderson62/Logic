00001  IDENTIFICATION DIVISION.                                         10/01/97
00002                                                                   ECS072
00003  PROGRAM-ID.                ECS072.                                  LV001
00004 *              PROGRAM CONVERTED BY                               ECS072
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ECS072
00006 *              CONVERSION DATE 02/08/96 18:27:45.                 ECS072
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          ECS072
00008 *                           VMOD=2.006.                           ECS072
00009 *AUTHOR.     LOGIC, INC.                                          ECS072
00010 *            DALLAS, TEXAS.                                       ECS072
00011                                                                   ECS072
00012 *DATE-COMPILED.                                                   ECS072
00013                                                                   ECS072
00014 *SECURITY.   *****************************************************ECS072
00015 *            *                                                   *ECS072
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ECS072
00017 *            *                                                   *ECS072
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS072
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ECS072
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *ECS072
00021 *            *                                                   *ECS072
00022 *            *****************************************************ECS072
00023                                                                   ECS072
00024 *REMARKS. THIS PROGRAM READS AND PRINTS THE RATE MASTER           ECS072
00025                                                                   ECS072
00026                                                                   ECS072
00027  ENVIRONMENT DIVISION.                                            ECS072
00028                                                                   ECS072
00029  INPUT-OUTPUT SECTION.                                            ECS072
00030                                                                   ECS072
00031  FILE-CONTROL.                                                    ECS072
00032                                                                   ECS072
00033      SELECT RATE-MASTER      ASSIGN TO SYS010-FBA1-ERRATE         ECS072
00034                                   ORGANIZATION INDEXED            ECS072
00035                                   ACCESS       SEQUENTIAL         ECS072
00036                                   RECORD KEY   RT-CONTROL-PRIMARY ECS072
00037                                   FILE STATUS  RATE-FILE-STATUS.  ECS072
00038                                                                   ECS072
00039      SELECT PRINTX           ASSIGN TO SYS008-UR-1403-S-SYS008.   ECS072
00040      SELECT DISK-DATE        ASSIGN TO SYS019-UT-FBA1-S-SYS019.   ECS072
00041      SELECT FICH             ASSIGN TO SYS020-UT-2400-S-SYS020.   ECS072
00042                                                                   ECS072
00043      EJECT                                                        ECS072
00044  DATA DIVISION.                                                   ECS072
00045                                                                   ECS072
00046  FILE SECTION.                                                    ECS072
00047                                                                   ECS072
00048  FD  RATE-MASTER.                                                 ECS072
00049                                                                   ECS072
00050     COPY ERCRATE.                                                 ECS072
00051                                                                   ECS072
00052  EJECT                                                            ECS072
00053  FD  PRINTX                                                       ECS072
00054                                  COPY ELCPRTFD.                   ECS072
00055  EJECT                                                            ECS072
00056  FD  FICH                                                         ECS072
00057                                  COPY ELCFCHFD.                   ECS072
00058  EJECT                                                            ECS072
00059  FD  DISK-DATE                                                    ECS072
00060                                  COPY ELCDTEFD.                   ECS072
00061  EJECT                                                            ECS072
00062  WORKING-STORAGE SECTION.                                         ECS072
00063  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      ECS072
00064  77  FILLER  PIC X(32) VALUE '********************************'.  ECS072
00065  77  FILLER  PIC X(32) VALUE '     ECS072 WORKING-STORAGE     '.  ECS072
00066  77  FILLER  PIC X(32) VALUE '*****VMOD=2.006*****************'.  ECS072
00067                                                                   ECS072
00068  77  ACTR                        PIC S99     COMP.                ECS072
00069  77  BCTR                        PIC S99     COMP.                ECS072
00070  77  CTR                         PIC S999    COMP.                ECS072
00071  77  CTS                         PIC S999    COMP.                ECS072
00072  77  CTT                         PIC S999    COMP.                ECS072
00073  77  WK-1                        PIC S999    COMP-3.              ECS072
00074  77  X                           PIC X.                           ECS072
00075  77  T-FLD                       PIC S9(6)V9(5)  COMP-3.          ECS072
00076  77  PAGER                       PIC S9(4)   VALUE +1.            ECS072
00077  77  LINE-CNT                    PIC S9(4)   VALUE ZEROS.         ECS072
00078  77  CTA                         PIC 9.                           ECS072
00079  77  MO-RTS                      PIC S999    COMP-3    VALUE +360.ECS072
00080                                                                   ECS072
00081  01  WS-ABEND.                                                    ECS072
00082      12  WS-RETURN-CODE          PIC S9(4)       VALUE ZERO.      ECS072
00083      12  WS-ZERO                 PIC S9          VALUE ZERO.      ECS072
00084      12  WS-ABEND-MESSAGE        PIC X(80)       VALUE SPACES.    ECS072
00085      12  WS-ABEND-FILE-STATUS    PIC XX          VALUE ZERO.      ECS072
00086                                                                   ECS072
00087  01  WS.                                                          ECS072
00088      12  PGM-SUB                 PIC S999    COMP    VALUE +072.  ECS072
00089      12  RATE-FILE-STATUS        PIC XX          VALUE ZERO.      ECS072
00090  EJECT                                                            ECS072
00091      COPY ELCDTECX.                                               ECS072
00092  EJECT                                                            ECS072
00093      COPY ELCDTEVR.                                               ECS072
00094  EJECT                                                            ECS072
00095  01  DATA-LINE-1.                                                 ECS072
00096      12  FILLER                  PIC X       VALUE SPACE.         ECS072
00097      12  FILLER                  PIC X(6)    VALUE 'STATE '.      ECS072
00098      12  DL-1AA                  PIC XX.                          ECS072
00099      12  FILLER                  PIC XXX     VALUE ' - '.         ECS072
00100      12  DL-1A                   PIC X(30).                       ECS072
00101      12  FILLER                  PIC X(5)    VALUE SPACE.         ECS072
00102      12  DL-1DTE                 PIC X(28).                       ECS072
00103      12  FILLER                  PIC X(5)    VALUE SPACES.        ECS072
00104      12  DL-COMNT                PIC X(50).                       ECS072
00105                                                                   ECS072
00106  01  DATA-LINE-1A.                                                ECS072
00107      12  FILLER                  PIC X(11)   VALUE SPACES.        ECS072
00108      12  FILLER                  PIC X(8)    VALUE 'CLASS = '.    ECS072
00109      12  DL-CLASS                PIC XX.                          ECS072
00110      12  FILLER                  PIC X(16)                        ECS072
00111          VALUE '    DEVIATION = '.                                ECS072
00112      12  DL-DEV                  PIC XXX.                         ECS072
00113      12  FILLER                  PIC X(15)                        ECS072
00114          VALUE '    HIGH AGE = '.                                 ECS072
00115      12  DL-HI-AGE               PIC XX.                          ECS072
00116                                                                   ECS072
00117  01  DATA-LINE-2.                                                 ECS072
00118      12  FILLER                  PIC X(11)   VALUE '        *'.   ECS072
00119      12  DL-2-OVRD               PIC X(12).                       ECS072
00120      12  DL-2A                   PIC XX.                          ECS072
00121      12  FILLER                  PIC XXX     VALUE ' - '.         ECS072
00122      12  DL-2B                   PIC X(12).                       ECS072
00123      12  FILLER                  PIC X(5)    VALUE SPACES.        ECS072
00124      12  DL-2A-COMNT             PIC X(50).                       ECS072
00125                                                                   ECS072
00126  01  DATA-LINE-4.                                                 ECS072
00127      12  FILLER                  PIC X(11)   VALUE SPACES.        ECS072
00128      12  DL-4-OVRD               PIC X(06).                       ECS072
00129      12  FILLER      PIC X(11)                                    ECS072
00130          VALUE ' MORTALITY '.                                     ECS072
00131      12  FILLER                  PIC X(7)    VALUE 'CODE = '.     ECS072
00132      12  DL-4C                   PIC X(4)    VALUE SPACES.        ECS072
00133      12  FILLER                  PIC XXX     VALUE ' = '.         ECS072
00134      12  DL-4B                   PIC X(26).                       ECS072
00135                                                                   ECS072
00136  01  DATA-LINE-4A.                                                ECS072
00137      12  FILLER                  PIC X(11)   VALUE SPACES.        ECS072
00138      12  FILLER      PIC X(22)                                    ECS072
00139          VALUE 'MAXIMUM ATTAINED AGE '.                           ECS072
00140      12  DL-4AGE                 PIC Z9.                          ECS072
00141                                                                   ECS072
00142  01  DATA-LINE-5.                                                 ECS072
00143      12  FILLER                  PIC X(11)   VALUE SPACES.        ECS072
00144      12  FILLER      PIC X(29)                                    ECS072
00145          VALUE 'EXCEPTIONS     AGE'.                              ECS072
00146      12  DL-5A               OCCURS 8.                            ECS072
00147          16  DL-5B               PIC ZZ.                          ECS072
00148          16  DL-5C               PIC X(8).                        ECS072
00149                                                                   ECS072
00150  01  DATA-LINE-6.                                                 ECS072
00151      12  FILLER                  PIC X(26)   VALUE SPACES.        ECS072
00152      12  FILLER                  PIC X(13)   VALUE 'TERM'.        ECS072
00153      12  DL-6A               OCCURS 8.                            ECS072
00154          16  DL-6B               PIC ZZZ.                         ECS072
00155          16  DL-6C               PIC X(7).                        ECS072
00156                                                                   ECS072
00157  01  DATA-LINE-7.                                                 ECS072
00158      12  FILLER                  PIC X(26)   VALUE SPACES.        ECS072
00159      12  FILLER                  PIC X(9)    VALUE 'FACE'.        ECS072
00160      12  DL-7A               OCCURS 8.                            ECS072
00161          16  DL-7B               PIC Z(7).                        ECS072
00162          16  DL-7C               PIC X(3).                        ECS072
00163                                                                   ECS072
00164  01  DATA-LINE-8.                                                 ECS072
00165      12  FILLER                  PIC X(11)   VALUE SPACES.        ECS072
00166      12  DL-8-OVRD               PIC X(06).                       ECS072
00167      12  FILLER                  PIC X(17)                        ECS072
00168          VALUE ' RATES BY MONTHS'.                                ECS072
00169                                                                   ECS072
00170  01  DATA-LINE-9.                                                 ECS072
00171      12  FILLER                  PIC X(16)   VALUE SPACES.        ECS072
00172      12  FILLER                  PIC X(10)   VALUE '   1'.        ECS072
00173      12  FILLER                  PIC X(10)   VALUE '   2'.        ECS072
00174      12  FILLER                  PIC X(10)   VALUE '   3'.        ECS072
00175      12  FILLER                  PIC X(10)   VALUE '   4'.        ECS072
00176      12  FILLER                  PIC X(10)   VALUE '   5'.        ECS072
00177      12  FILLER                  PIC X(10)   VALUE '   6'.        ECS072
00178      12  FILLER                  PIC X(10)   VALUE '   7'.        ECS072
00179      12  FILLER                  PIC X(10)   VALUE '   8'.        ECS072
00180      12  FILLER                  PIC X(10)   VALUE '   9'.        ECS072
00181      12  FILLER                  PIC X(10)   VALUE '  10'.        ECS072
00182      12  FILLER                  PIC X(10)   VALUE '  11'.        ECS072
00183      12  FILLER                  PIC X(10)   VALUE '  12'.        ECS072
00184                                                                   ECS072
00185  01  DATA-LINE-10.                                                ECS072
00186      12  FILLER                  PIC X(5)    VALUE SPACES.        ECS072
00187      12  FILLER                  PIC X(4)    VALUE 'YEAR'.        ECS072
00188      12  DL-10A                  PIC ZZ9.                         ECS072
00189      12  DL-10B              OCCURS 12.                           ECS072
00190          16  DL-10C              PIC XX.                          ECS072
00191          16  DL-10D              PIC ZZ.Z(5).                     ECS072
00192      12  FILLER                  PIC XXXX    VALUE SPACES.        ECS072
00193                                                                   ECS072
00194  01  DATA-LINE-11.                                                ECS072
00195      12  FILLER                  PIC X(11)   VALUE '        *'.   ECS072
00196      12  DL-11-OVRD              PIC X(12).                       ECS072
00197      12  FILLER                  PIC X(5)    VALUE 'TYPE'.        ECS072
00198      12  DL-11A                  PIC XX.                          ECS072
00199      12  FILLER                  PIC XXX     VALUE ' - '.         ECS072
00200      12  DL-11B                  PIC X(12).                       ECS072
00201      12  FILLER                  PIC X(5)    VALUE SPACES.        ECS072
00202      12  DL-11A-COMM             PIC X(50).                       ECS072
00203                                                                   ECS072
00204  01  DATA-LINE-12.                                                ECS072
00205      12  FILLER                  PIC X(11)   VALUE SPACES.        ECS072
00206      12  FILLER                  PIC X(29)                        ECS072
00207          VALUE 'EXCEPTIONS     AGE'.                              ECS072
00208      12  DL-12A              OCCURS 8.                            ECS072
00209          16  DL-12B              PIC ZZ.                          ECS072
00210          16  DL-12C              PIC X(8).                        ECS072
00211                                                                   ECS072
00212  01  DATA-LINE-12A.                                               ECS072
00213      12  FILLER                  PIC X(26)   VALUE SPACES.        ECS072
00214      12  FILLER                  PIC X(13)   VALUE 'TERM'.        ECS072
00215      12  DL-12A-TERM     OCCURS 8.                                ECS072
00216          16  DL-12A-T            PIC ZZZ.                         ECS072
00217          16  DL-12A-FIL          PIC X(7).                        ECS072
00218                                                                   ECS072
00219                                                                   ECS072
00220  01  DATA-LINE-13.                                                ECS072
00221      12  FILLER                  PIC X(26)   VALUE SPACES.        ECS072
00222      12  FILLER                  PIC X(11)   VALUE 'BENEFIT'.     ECS072
00223      12  DL-13A              OCCURS 8.                            ECS072
00224          16  DL-13B              PIC Z(5).                        ECS072
00225          16  DL-13C              PIC X(5).                        ECS072
00226                                                                   ECS072
00227  01  DATA-LINE-13A.                                               ECS072
00228      12  FILLER                  PIC X(26)   VALUE SPACES.        ECS072
00229      12  FILLER                  PIC X(9)    VALUE 'FACE'.        ECS072
00230      12  DL-13A-BENF     OCCURS 8 TIMES.                          ECS072
00231          16  DL-13A-BF           PIC Z(7).                        ECS072
00232          16  DL-13A-FIL          PIC X(3).                        ECS072
00233                                                                   ECS072
00234  01  DATA-LINE-14.                                                ECS072
00235      12  FILLER                  PIC X(11)   VALUE SPACES.        ECS072
00236      12  DL-14-OVRD              PIC X(06).                       ECS072
00237      12  FILLER                  PIC X(18)                        ECS072
00238          VALUE ' RATES BY MONTHS'.                                ECS072
00239                                                                   ECS072
00240  01  TRAILER-LINE.                                                ECS072
00241      12  FILLER                  PIC X(110) VALUE SPACES.         ECS072
00242      12  TRAILER-DESC            PIC X(12)  VALUE SPACES.         ECS072
00243      12  TRAILER-DETL            PIC X(8)   VALUE SPACES.         ECS072
00244                                                                   ECS072
00245  EJECT                                                            ECS072
00246  01  HEAD-1.                                                      ECS072
00247      12  FILLER                  PIC X(45)   VALUE SPACES.        ECS072
00248      12  H1-OVRD-1               PIC X(12).                       ECS072
00249      12  FILLER                  PIC X(05)   VALUE ' AND '.       ECS072
00250      12  H1-OVRD-2               PIC X(12).                       ECS072
00251      12  FILLER                  PIC X(06)   VALUE ' RATES'.      ECS072
00252      12  FILLER                  PIC X(39)   VALUE SPACES.        ECS072
00253      12  FILLER                  PIC X(8)    VALUE 'ECS072 '.     ECS072
00254                                                                   ECS072
00255  01  HEAD-2.                                                      ECS072
00256      12  FILLER                  PIC X(47)   VALUE SPACES.        ECS072
00257      12  H1-A                    PIC X(30).                       ECS072
00258      12  FILLER                  PIC X(42)   VALUE SPACES.        ECS072
00259      12  H2-DATE                 PIC X(8).                        ECS072
00260                                                                   ECS072
00261  01  HEAD-2A.                                                     ECS072
00262      12  FILLER                  PIC X(53)   VALUE SPACES.        ECS072
00263      12  H2A-DATE                PIC X(18).                       ECS072
00264      12  FILLER                  PIC X(48)   VALUE SPACES.        ECS072
00265      12  FILLER                  PIC X(5)    VALUE 'PAGE '.       ECS072
00266      12  DL-1D                   PIC ZZ,ZZ9.                      ECS072
00267                                                                   ECS072
00268  EJECT                                                            ECS072
00269  01  PRETTY-DATE.                                                 ECS072
00270      12  PRETTY-MO               PIC Z9.                          ECS072
00271      12  FILLER                  PIC X       VALUE '-'.           ECS072
00272      12  PRETTY-DAY              PIC 99.                          ECS072
00273      12  FILLER                  PIC X       VALUE '-'.           ECS072
00274      12  PRETTY-YR               PIC 99.                          ECS072
00275                                                                   ECS072
00276  01  L1-CUR-DTE.                                                  ECS072
00277      12  FILLER      PIC X(27)                                    ECS072
00278          VALUE 'THESE ARE THE CURRENT RATES'.                     ECS072
00279                                                                   ECS072
00280  01  L1-DATER.                                                    ECS072
00281      12  FILLER                  PIC X(20)                        ECS072
00282          VALUE 'THESE RATES EXPIRED'.                             ECS072
00283      12  L1D-FILL                PIC X(8).                        ECS072
00284                                                                   ECS072
00285      EJECT                                                        ECS072
00286  PROCEDURE DIVISION.                                              ECS072
00287                                                                   ECS072
00288  0100-GET-CLAS-CARD.                                              ECS072
00289                                                                   ECS072
00290      COPY ELCDTERX.                                               ECS072
00291                                                                   ECS072
00292      MOVE COMPANY-NAME           TO H1-A.                         ECS072
00293      MOVE ALPH-DATE              TO H2A-DATE.                     ECS072
00294      MOVE CLAS-STARTM            TO CLAS-INDEXM.                  ECS072
00295                                                                   ECS072
00296  0110-SPACE-PRNT-ROUTINE.                                         ECS072
00297                                                                   ECS072
00298      MOVE 1                      TO CTR.                          ECS072
00299      PERFORM 0120-SPACER-RTN 8 TIMES.                             ECS072
00300      MOVE SPACE TO DL-10C (9) DL-10C (10) DL-10C (11) DL-10C (12).ECS072
00301      GO TO 0140-OPEN-FILES.                                       ECS072
00302                                                                   ECS072
00303  0120-SPACER-RTN.                                                 ECS072
00304                                                                   ECS072
00305      MOVE SPACES TO DL-5C (CTR)  DL-6C (CTR)  DL-7C (CTR).        ECS072
00306      MOVE SPACES TO DL-10C (CTR)  DL-12C (CTR)  DL-13C (CTR).     ECS072
00307      ADD 1 TO CTR.                                                ECS072
00308                                                                   ECS072
00309  EJECT                                                            ECS072
00310  0140-OPEN-FILES.                                                 ECS072
00311                                                                   ECS072
00312      OPEN INPUT RATE-MASTER                                       ECS072
00313          OUTPUT PRINTX.                                           ECS072
00314                                                                   ECS072
00315      IF RATE-FILE-STATUS  = '00' OR '97'                          ECS072
00316          NEXT SENTENCE                                            ECS072
00317        ELSE                                                       ECS072
00318          MOVE RATE-FILE-STATUS TO WS-ABEND-FILE-STATUS            ECS072
00319          MOVE 'OPEN ERROR - ERRATE ' TO WS-ABEND-MESSAGE          ECS072
00320          PERFORM ABEND-PGM.                                       ECS072
00321                                                                   ECS072
00322      MOVE WS-CURRENT-DATE        TO H2-DATE.                      ECS072
00323      MOVE LOW-VALUES             TO RT-CONTROL-PRIMARY.           ECS072
00324      MOVE DTE-CLASIC-COMPANY-CD  TO RT-COMPANY-CD.                ECS072
00325                                                                   ECS072
00326      START RATE-MASTER KEY IS NOT LESS THAN RT-CONTROL-PRIMARY.   ECS072
00327                                                                   ECS072
00328      IF RATE-FILE-STATUS NOT = ZERO                               ECS072
00329          MOVE RATE-FILE-STATUS TO WS-ABEND-FILE-STATUS            ECS072
00330          MOVE 'START ERROR - ERRATE ' TO WS-ABEND-MESSAGE         ECS072
00331          PERFORM ABEND-PGM.                                       ECS072
00332                                                                   ECS072
00333  0150-READ-MASTER-RATE.                                           ECS072
00334                                                                   ECS072
00335      READ RATE-MASTER.                                            ECS072
00336                                                                   ECS072
00337      IF RATE-FILE-STATUS = '10'                                   ECS072
00338          GO TO 0510-EOJ-CLOSER.                                   ECS072
00339                                                                   ECS072
00340      IF RT-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD                 ECS072
00341          GO TO 0510-EOJ-CLOSER.                                   ECS072
00342                                                                   ECS072
00343      IF RATE-FILE-STATUS NOT = ZERO                               ECS072
00344          MOVE RATE-FILE-STATUS TO WS-ABEND-FILE-STATUS            ECS072
00345          MOVE 'READ ERROR - ERRATE ' TO WS-ABEND-MESSAGE          ECS072
00346          PERFORM ABEND-PGM.                                       ECS072
00347                                                                   ECS072
00348      IF DTE-CLIENT = 'FMK'                                        ECS072
00349          IF RT-ST-CODE NOT = '34'                                 ECS072
00350              GO TO 0150-READ-MASTER-RATE.                         ECS072
00351                                                                   ECS072
00352      COPY ELCRTEM1.                                               ECS072
00353                                                                   ECS072
00354      MOVE RT-ST-CODE             TO DL-1AA.                       ECS072
00355      MOVE RT-ST-DEV              TO DL-DEV.                       ECS072
00356      MOVE RT-STATE-CODE          TO STATE-L.                      ECS072
00357                                                                   ECS072
00358  0160-LOOK-UP-STATE.                                              ECS072
00359                              COPY ECSSTLOK.                       ECS072
00360                                                                   ECS072
00361      MOVE STATE-PIC (CLAS-INDEXS) TO DL-1A.                       ECS072
00362                                                                   ECS072
00363      IF RT-EXP-DA = 99                                            ECS072
00364          MOVE L1-CUR-DTE         TO DL-1DTE                       ECS072
00365      ELSE                                                         ECS072
00366          MOVE RT-EXP-YR          TO PRETTY-YR                     ECS072
00367          MOVE RT-EXP-MO          TO PRETTY-MO                     ECS072
00368          MOVE RT-EXP-DA          TO PRETTY-DAY                    ECS072
00369          MOVE PRETTY-DATE        TO L1D-FILL                      ECS072
00370          MOVE L1-DATER           TO DL-1DTE.                      ECS072
00371                                                                   ECS072
00372      MOVE PAGER                  TO DL-1D.                        ECS072
00373      ADD 1 TO PAGER.                                              ECS072
00374      MOVE ZEROS                  TO LINE-CNT.                     ECS072
00375      MOVE '1'                    TO X.                            ECS072
00376      MOVE LIFE-OVERRIDE-L12      TO H1-OVRD-1.                    ECS072
00377      MOVE   AH-OVERRIDE-L12      TO H1-OVRD-2.                    ECS072
00378      MOVE HEAD-1                 TO P-DATA.                       ECS072
00379      PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 ECS072
00380      MOVE HEAD-2                 TO P-DATA.                       ECS072
00381      PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 ECS072
00382      MOVE HEAD-2A                TO P-DATA.                       ECS072
00383      PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 ECS072
00384      MOVE '0'                    TO X.                            ECS072
00385                                                                   ECS072
00386      MOVE RT-ST-CLASS            TO DL-CLASS.                     ECS072
00387      MOVE RT-HIGH-AGE            TO DL-HI-AGE.                    ECS072
00388      MOVE RT-STRUCTURE-COMMENT   TO DL-COMNT.                     ECS072
00389                                                                   ECS072
00390      MOVE DATA-LINE-1            TO P-DATA.                       ECS072
00391      PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 ECS072
00392      MOVE DATA-LINE-1A           TO P-DATA.                       ECS072
00393      PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 ECS072
00394      MOVE '0'                    TO X.                            ECS072
00395      IF RT-L-AH NOT = 'L'                                         ECS072
00396          GO TO 0220-DOING-A-AND-H.                                ECS072
00397                                                                   ECS072
00398  0170-PRINT-THIS-LIFE.                                            ECS072
00399                                                                   ECS072
00400      MOVE RT-LAH-NUM             TO DL-2A.                        ECS072
00401      MOVE LIFE-OVERRIDE-L12      TO DL-2-OVRD.                    ECS072
00402      PERFORM 0440-FIND-L-NAME THRU 0460-E-F-L-NAME.               ECS072
00403      MOVE RT-RATE-COMMENT        TO DL-2A-COMNT.                  ECS072
00404      MOVE DATA-LINE-2            TO P-DATA.                       ECS072
00405      PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 ECS072
00406      MOVE SPACES                 TO DL-4B.                        ECS072
00407                                                                   ECS072
00408      IF RT-LIFE-MORT-CODE NOT = CLAS-MORT-CODE (CLAS-INDEXM)      ECS072
00409          MOVE CLAS-STARTM        TO CLAS-INDEXM.                  ECS072
00410                                                                   ECS072
00411  0180-MORT-DESC-LOOP.                                             ECS072
00412                                                                   ECS072
00413      IF RT-LIFE-MORT-CODE = CLAS-MORT-CODE (CLAS-INDEXM)          ECS072
00414          MOVE CLAS-MORT-DESC (CLAS-INDEXM) TO DL-4B               ECS072
00415      ELSE                                                         ECS072
00416          ADD +1 TO CLAS-INDEXM                                    ECS072
00417          IF CLAS-INDEXM NOT GREATER THAN CLAS-MAXM                ECS072
00418              GO TO 0180-MORT-DESC-LOOP.                           ECS072
00419                                                                   ECS072
00420      MOVE LIFE-OVERRIDE-L6       TO DL-4-OVRD.                    ECS072
00421      MOVE RT-LIFE-MORT-CODE      TO DL-4C.                        ECS072
00422      MOVE DATA-LINE-4            TO P-DATA.                       ECS072
00423      PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 ECS072
00424      MOVE RT-MAX-AGE             TO DL-4AGE.                      ECS072
00425      MOVE DATA-LINE-4A           TO P-DATA.                       ECS072
00426      PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 ECS072
00427      MOVE 1                      TO CTS.                          ECS072
00428      PERFORM 0320-L-X-AGE 8 TIMES.                                ECS072
00429      MOVE DATA-LINE-5            TO P-DATA.                       ECS072
00430      PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 ECS072
00431      MOVE 1                      TO CTS.                          ECS072
00432      PERFORM 0340-L-X-TERM 8 TIMES.                               ECS072
00433      MOVE DATA-LINE-6            TO P-DATA.                       ECS072
00434      PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 ECS072
00435      MOVE 1                      TO CTS.                          ECS072
00436      PERFORM 0350-L-X-FACE 8 TIMES.                               ECS072
00437      MOVE DATA-LINE-7            TO P-DATA.                       ECS072
00438      PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 ECS072
00439      MOVE '-'                    TO X.                            ECS072
00440      MOVE LIFE-OVERRIDE-L6       TO DL-8-OVRD.                    ECS072
00441      MOVE DATA-LINE-8            TO P-DATA.                       ECS072
00442      PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 ECS072
00443      MOVE '0'                    TO X.                            ECS072
00444      MOVE DATA-LINE-9            TO P-DATA.                       ECS072
00445      PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 ECS072
00446      MOVE 1                      TO CTT.                          ECS072
00447                                                                   ECS072
00448  0190-CALC-YEAR-NUMBER-L.                                         ECS072
00449                                                                   ECS072
00450      COMPUTE WK-1 = (CTT + 11) / 12.                              ECS072
00451      MOVE WK-1                   TO DL-10A.                       ECS072
00452      MOVE CTT                    TO CTS.                          ECS072
00453      MOVE ZERO                   TO T-FLD.                        ECS072
00454      PERFORM 0330-LIFE-RATE-ADD 12 TIMES.                         ECS072
00455                                                                   ECS072
00456      IF T-FLD = ZERO                                              ECS072
00457          ADD 12 TO CTT                                            ECS072
00458          GO TO 0200-CK-LIFE-END.                                  ECS072
00459                                                                   ECS072
00460      MOVE 1                      TO CTS.                          ECS072
00461      PERFORM 0360-FILL-LIFE-RATES 12 TIMES.                       ECS072
00462      MOVE DATA-LINE-10           TO P-DATA.                       ECS072
00463      PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 ECS072
00464                                                                   ECS072
00465  0200-CK-LIFE-END.                                                ECS072
00466                                                                   ECS072
00467      IF CTT NOT GREATER MO-RTS                                    ECS072
00468          GO TO 0190-CALC-YEAR-NUMBER-L.                           ECS072
00469                                                                   ECS072
00470      PERFORM 0290-TRAILER-ROUTINE THRU 0300-TRAILER-ROUTINE-X.    ECS072
00471      GO TO 0150-READ-MASTER-RATE.                                 ECS072
00472                                                                   ECS072
00473  EJECT                                                            ECS072
00474  0220-DOING-A-AND-H.                                              ECS072
00475                                                                   ECS072
00476      MOVE AH-OVERRIDE-L12        TO DL-11-OVRD.                   ECS072
00477      MOVE RT-LAH-NUM             TO DL-11A.                       ECS072
00478      PERFORM 0470-FIND-D-NAME THRU 0490-E-F-D-NAME.               ECS072
00479      MOVE RT-RATE-COMMENT        TO DL-11A-COMM.                  ECS072
00480      MOVE DATA-LINE-11           TO P-DATA.                       ECS072
00481      PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 ECS072
00482      MOVE RT-MAX-AGE             TO DL-4AGE.                      ECS072
00483      MOVE DATA-LINE-4A           TO P-DATA.                       ECS072
00484      PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 ECS072
00485      MOVE 1                      TO CTS.                          ECS072
00486      PERFORM 0380-AH-X-AGE 8 TIMES.                               ECS072
00487      MOVE DATA-LINE-12           TO P-DATA.                       ECS072
00488      PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 ECS072
00489      MOVE 1 TO CTS.                                               ECS072
00490      PERFORM 0390-AH-X-TERM 8 TIMES.                              ECS072
00491      MOVE DATA-LINE-12A          TO P-DATA.                       ECS072
00492      PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 ECS072
00493      MOVE 1                      TO CTS.                          ECS072
00494      PERFORM 0400-AH-X-BEN 8 TIMES.                               ECS072
00495      MOVE DATA-LINE-13           TO P-DATA.                       ECS072
00496      PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 ECS072
00497      MOVE 1 TO CTS.                                               ECS072
00498      PERFORM 0410-AH-X-FACE 8 TIMES.                              ECS072
00499      MOVE DATA-LINE-13A          TO P-DATA.                       ECS072
00500      PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 ECS072
00501      MOVE '-'                    TO X.                            ECS072
00502      MOVE AH-OVERRIDE-L6         TO DL-14-OVRD.                   ECS072
00503      MOVE DATA-LINE-14           TO P-DATA.                       ECS072
00504      PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 ECS072
00505      MOVE '0'                    TO X.                            ECS072
00506      MOVE DATA-LINE-9            TO P-DATA.                       ECS072
00507      PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 ECS072
00508      MOVE 1                      TO CTT.                          ECS072
00509                                                                   ECS072
00510  0230-CALC-YEAR-NUMBER-AH.                                        ECS072
00511                                                                   ECS072
00512      COMPUTE WK-1 = (CTT + 11) / 12.                              ECS072
00513      MOVE WK-1                   TO DL-10A.                       ECS072
00514      MOVE CTT                    TO CTS.                          ECS072
00515      MOVE ZERO                   TO T-FLD.                        ECS072
00516      PERFORM 0370-AH-RATE-ADD 12 TIMES.                           ECS072
00517                                                                   ECS072
00518      IF T-FLD = ZERO                                              ECS072
00519          ADD 12 TO CTT                                            ECS072
00520          GO TO 0240-CK-AH-END.                                    ECS072
00521                                                                   ECS072
00522      MOVE 1                      TO CTS.                          ECS072
00523      PERFORM 0420-FILL-AH-RATES 12 TIMES.                         ECS072
00524      MOVE DATA-LINE-10           TO P-DATA.                       ECS072
00525      PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 ECS072
00526                                                                   ECS072
00527  0240-CK-AH-END.                                                  ECS072
00528                                                                   ECS072
00529      IF CTT NOT GREATER MO-RTS                                    ECS072
00530          GO TO 0230-CALC-YEAR-NUMBER-AH.                          ECS072
00531                                                                   ECS072
00532      PERFORM 0290-TRAILER-ROUTINE THRU 0300-TRAILER-ROUTINE-X.    ECS072
00533      GO TO 0150-READ-MASTER-RATE.                                 ECS072
00534                                                                   ECS072
00535  EJECT                                                            ECS072
00536  0260-PRT-RTN.                                                    ECS072
00537                                  COPY ELCPRT2.                    ECS072
00538      ADD 1 TO LINE-CNT.                                           ECS072
00539                                                                   ECS072
00540      IF X = '0'                                                   ECS072
00541          ADD 1 TO LINE-CNT                                        ECS072
00542      ELSE                                                         ECS072
00543          IF X = '-'                                               ECS072
00544              ADD 2 TO LINE-CNT.                                   ECS072
00545                                                                   ECS072
00546      MOVE ' '                    TO X.                            ECS072
00547                                                                   ECS072
00548  0270-PRT-RTN-EXIT.    EXIT.                                      ECS072
00549                                                                   ECS072
00550  EJECT                                                            ECS072
00551  0290-TRAILER-ROUTINE.                                            ECS072
00552                                                                   ECS072
00553      IF LINE-CNT LESS 48                                          ECS072
00554          MOVE SPACES             TO P-DATA                        ECS072
00555          PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT              ECS072
00556          GO TO 0290-TRAILER-ROUTINE.                              ECS072
00557                                                                   ECS072
00558      MOVE 'STATE'                TO TRAILER-DESC.                 ECS072
00559      MOVE RT-ST-CODE             TO TRAILER-DETL.                 ECS072
00560      MOVE TRAILER-LINE           TO P-DATA.                       ECS072
00561      PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 ECS072
00562      MOVE 'CLASS'                TO TRAILER-DESC.                 ECS072
00563      MOVE RT-ST-CLASS            TO TRAILER-DETL.                 ECS072
00564      MOVE TRAILER-LINE           TO P-DATA.                       ECS072
00565      PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 ECS072
00566      MOVE 'DEVIATION'            TO TRAILER-DESC.                 ECS072
00567      MOVE RT-ST-DEV              TO TRAILER-DETL.                 ECS072
00568      MOVE TRAILER-LINE           TO P-DATA.                       ECS072
00569      PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 ECS072
00570      MOVE 'TYPE'                 TO TRAILER-DESC.                 ECS072
00571      MOVE RT-L-AH-CODE           TO TRAILER-DETL.                 ECS072
00572      MOVE TRAILER-LINE           TO P-DATA.                       ECS072
00573      PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 ECS072
00574      MOVE 'HIGH AGE'             TO TRAILER-DESC.                 ECS072
00575      MOVE RT-HIGH-AGE            TO TRAILER-DETL.                 ECS072
00576      MOVE TRAILER-LINE           TO P-DATA.                       ECS072
00577      PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 ECS072
00578      MOVE RT-EXP-YR              TO PRETTY-YR.                    ECS072
00579      MOVE RT-EXP-MO              TO PRETTY-MO.                    ECS072
00580      MOVE RT-EXP-DA              TO PRETTY-DAY.                   ECS072
00581      MOVE PRETTY-DATE            TO TRAILER-DETL.                 ECS072
00582      MOVE 'EXP DATE'             TO TRAILER-DESC.                 ECS072
00583      MOVE TRAILER-LINE           TO P-DATA.                       ECS072
00584      PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 ECS072
00585                                                                   ECS072
00586  0300-TRAILER-ROUTINE-X.     EXIT.                                ECS072
00587                                                                   ECS072
00588  EJECT                                                            ECS072
00589  0320-L-X-AGE.                                                    ECS072
00590                                                                   ECS072
00591      MOVE RT-L-EX-AGE (CTS)      TO DL-5B (CTS).                  ECS072
00592      ADD 1 TO CTS.                                                ECS072
00593                                                                   ECS072
00594  0330-LIFE-RATE-ADD.                                              ECS072
00595                                                                   ECS072
00596      ADD RT-L-RATE (CTS)         TO T-FLD.                        ECS072
00597      ADD 1 TO CTS.                                                ECS072
00598                                                                   ECS072
00599  0340-L-X-TERM.                                                   ECS072
00600                                                                   ECS072
00601      MOVE RT-L-EX-TERM (CTS)     TO DL-6B (CTS).                  ECS072
00602      ADD 1 TO CTS.                                                ECS072
00603                                                                   ECS072
00604  0350-L-X-FACE.                                                   ECS072
00605                                                                   ECS072
00606      MOVE RT-L-EX-FACE (CTS)     TO DL-7B (CTS).                  ECS072
00607      ADD 1 TO CTS.                                                ECS072
00608                                                                   ECS072
00609  0360-FILL-LIFE-RATES.                                            ECS072
00610                                                                   ECS072
00611      MOVE RT-L-RATE (CTT)        TO DL-10D (CTS).                 ECS072
00612      ADD 1 TO CTT.                                                ECS072
00613      ADD 1 TO CTS.                                                ECS072
00614                                                                   ECS072
00615  0370-AH-RATE-ADD.                                                ECS072
00616                                                                   ECS072
00617      ADD RT-AH-RATE (CTS)        TO T-FLD.                        ECS072
00618      ADD 1 TO CTS.                                                ECS072
00619                                                                   ECS072
00620  0380-AH-X-AGE.                                                   ECS072
00621                                                                   ECS072
00622      MOVE RT-AH-AGE (CTS)        TO DL-12B (CTS).                 ECS072
00623      ADD 1 TO CTS.                                                ECS072
00624                                                                   ECS072
00625  0390-AH-X-TERM.                                                  ECS072
00626                                                                   ECS072
00627      MOVE RT-AH-TERM (CTS)       TO DL-12A-T (CTS).               ECS072
00628      MOVE SPACES                 TO DL-12A-FIL (CTS).             ECS072
00629      ADD 1 TO CTS.                                                ECS072
00630                                                                   ECS072
00631  0400-AH-X-BEN.                                                   ECS072
00632                                                                   ECS072
00633      MOVE RT-AH-BEN-M (CTS)      TO DL-13B (CTS).                 ECS072
00634      ADD 1 TO CTS.                                                ECS072
00635                                                                   ECS072
00636  0410-AH-X-FACE.                                                  ECS072
00637                                                                   ECS072
00638      MOVE RT-AH-BEN-F (CTS)      TO DL-13A-BF (CTS).              ECS072
00639      MOVE SPACES                 TO DL-13A-FIL (CTS).             ECS072
00640      ADD 1 TO CTS.                                                ECS072
00641                                                                   ECS072
00642  0420-FILL-AH-RATES.                                              ECS072
00643                                                                   ECS072
00644      MOVE RT-AH-RATE (CTT)       TO DL-10D (CTS).                 ECS072
00645      ADD 1 TO CTT.                                                ECS072
00646      ADD 1 TO CTS.                                                ECS072
00647                                                                   ECS072
00648  EJECT                                                            ECS072
00649  0440-FIND-L-NAME.                                                ECS072
00650                                                                   ECS072
00651      MOVE SPACES                 TO DL-2B.                        ECS072
00652                                                                   ECS072
00653      IF CLAS-STARTL = ZERO                                        ECS072
00654          GO TO 0460-E-F-L-NAME.                                   ECS072
00655                                                                   ECS072
00656      MOVE CLAS-STARTL            TO CLAS-INDEXL.                  ECS072
00657                                                                   ECS072
00658  0450-FLN-LOOP.                                                   ECS072
00659                                                                   ECS072
00660      IF CLAS-I-BEN (CLAS-INDEXL) = RT-LAH-NUM                     ECS072
00661          MOVE CLAS-I-AB10 (CLAS-INDEXL) TO DL-2B                  ECS072
00662          GO TO 0460-E-F-L-NAME.                                   ECS072
00663                                                                   ECS072
00664      ADD +1 TO CLAS-INDEXL.                                       ECS072
00665                                                                   ECS072
00666      IF CLAS-INDEXL NOT GREATER THAN CLAS-MAXL                    ECS072
00667          GO TO 0450-FLN-LOOP.                                     ECS072
00668                                                                   ECS072
00669  0460-E-F-L-NAME.     EXIT.                                       ECS072
00670                                                                   ECS072
00671                                                                   ECS072
00672  0470-FIND-D-NAME.                                                ECS072
00673                                                                   ECS072
00674      MOVE SPACES                 TO DL-11B.                       ECS072
00675                                                                   ECS072
00676      IF CLAS-STARTA = ZERO                                        ECS072
00677          GO TO 0490-E-F-D-NAME.                                   ECS072
00678                                                                   ECS072
00679      MOVE CLAS-STARTA            TO CLAS-INDEXA.                  ECS072
00680                                                                   ECS072
00681  0480-FDN-LOOP.                                                   ECS072
00682                                                                   ECS072
00683      IF CLAS-I-BEN (CLAS-INDEXA) = RT-LAH-NUM                     ECS072
00684          MOVE CLAS-I-AB10 (CLAS-INDEXA) TO DL-11B                 ECS072
00685          GO TO 0490-E-F-D-NAME.                                   ECS072
00686                                                                   ECS072
00687      ADD +1 TO CLAS-INDEXA.                                       ECS072
00688                                                                   ECS072
00689      IF CLAS-INDEXA NOT GREATER THAN CLAS-MAXA                    ECS072
00690          GO TO 0480-FDN-LOOP.                                     ECS072
00691                                                                   ECS072
00692  0490-E-F-D-NAME.     EXIT.                                       ECS072
00693                                                                   ECS072
00694  EJECT                                                            ECS072
00695  0510-EOJ-CLOSER.                                                 ECS072
00696                              COPY ELCPRTC.                        ECS072
00697                                                                   ECS072
00698      CLOSE RATE-MASTER                                            ECS072
00699            PRINTX.                                                ECS072
00700                                                                   ECS072
00701      GOBACK.                                                      ECS072
00702                                                                   ECS072
00703  ABEND-PGM SECTION.              COPY ELCABEND.                   ECS072
