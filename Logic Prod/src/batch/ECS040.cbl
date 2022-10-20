00001  IDENTIFICATION DIVISION.                                         03/26/98
00002                                                                   ECS040
00003  PROGRAM-ID.                ECS040.                                  LV008
00004 *              PROGRAM CONVERTED BY                               ECS040
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ECS040
00006 *              CONVERSION DATE 11/28/95 11:13:00.                 ECS040
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          ECS040
00008 *                           VMOD=2.006.                           ECS040
00009                                                                   ECS040
00010 *AUTHOR.     LOGIC, INC.                                          ECS040
00011 *            DALLAS, TEXAS.                                       ECS040
00012                                                                   ECS040
00013 *DATE-COMPILED.                                                   ECS040
00014                                                                   ECS040
00015 *SECURITY.   *****************************************************ECS040
00016 *            *                                                   *ECS040
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ECS040
00018 *            *                                                   *ECS040
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS040
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ECS040
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *ECS040
00022 *            *                                                   *ECS040
00023 *            *****************************************************ECS040
00024                                                                   ECS040
00025 *REMARKS.                                                         ECS040
00026 *        THIS PROGRAM WILL EXTRACT MORTALITY INFO FOR POSTING     ECS040
00027 *        TO THE EPEC FILE.                                        ECS040
00028                                                                   ECS040
00029  ENVIRONMENT DIVISION.                                            ECS040
00030  CONFIGURATION SECTION.                                           ECS040
00031                                                                      CL**8
00032  INPUT-OUTPUT SECTION.                                            ECS040
00033  FILE-CONTROL.                                                    ECS040
00034                                                                   ECS040
00035      SELECT WORK-FILE        ASSIGN TO SYS004-UT-3380-S-SYS004.   ECS040
00036      SELECT PRNTR            ASSIGN TO SYS008-UR-1403-S-SYS008.   ECS040
00037      SELECT GAP-FILE         ASSIGN TO SYS010-UT-2400-S-SYS010.   ECS040
00038      SELECT DISK-DATE        ASSIGN TO SYS019-UT-3380-S-SYS019.   ECS040
00039      SELECT FICH             ASSIGN TO SYS020-UT-2400-S-SYS020.   ECS040
00040  EJECT                                                            ECS040
00041  DATA DIVISION.                                                   ECS040
00042  FILE SECTION.                                                    ECS040
00043                                                                   ECS040
00044  FD  WORK-FILE                                                    ECS040
00045      BLOCK CONTAINS 0 RECORDS
00046      RECORDING MODE F.                                            ECS040
00047                                                                   ECS040
00048  01  WORK-REC.                                                    ECS040
00049      12  WR-COMPANY-CD       PIC X.                               ECS040
00050      12  WR-CARR             PIC X.                               ECS040
00051      12  WR-COMP             PIC X(6).                            ECS040
00052      12  WR-STATE            PIC XX.                              ECS040
00053      12  WR-ACCT             PIC X(10).                           ECS040
00054      12  WR-DATE             PIC 9(11)        COMP-3.             ECS040
00055      12  WR-LF-AH            PIC X.                               ECS040
00056      12  WR-BEN-TYPE         PIC XX.                              ECS040
00057      12  WR-REI-CO           PIC X(6).                            ECS040
00058      12  WR-CARD             PIC X.                               ECS040
00059      12  WR-RESV             PIC S9(9)V99     COMP-3.             ECS040
00060      12  WR-REM-AMT          PIC S9(9)V99     COMP-3.             ECS040
00061      12  WR-FUT-RESERVE      PIC S9(7)V99     COMP-3.             ECS040
00062      12  WR-PTC-RESERVE      PIC S9(7)V99     COMP-3.             ECS040
00063      12  WR-IBNR-RESERVE     PIC S9(7)V99     COMP-3.             ECS040
00064      12  WR-CLM-ADJ-AMT      PIC S9(7)V99     COMP-3.             ECS040
00065      12  WR-EXPENSES         PIC S9(7)V99     COMP-3.             ECS040
00066      12  WR-PAYMENTS         PIC S9(7)V99     COMP-3.             ECS040
00067      12  WR-OTH-COMMISSIONS  PIC S9(7)V99     COMP-3.             ECS040
00068      12  WR-REIN-PREM-ADJS   PIC S9(7)V99     COMP-3.             ECS040
00069      12  WR-PDATE            PIC 9(07)        COMP-3.                CL**4
00070  EJECT                                                            ECS040
00071  FD  PRNTR                                                        ECS040
00072                              COPY ELCPRTFD.                       ECS040
00073  EJECT                                                            ECS040
00074  FD  GAP-FILE                                                     ECS040
00075                              COPY ECSGAPFD.                       ECS040
00076                                                                   ECS040
00077                              COPY ECSGAP01.                       ECS040
00078                                                                   ECS040
00079  EJECT                                                            ECS040
00080  FD  DISK-DATE                                                    ECS040
00081                              COPY ELCDTEFD.                       ECS040
00082  EJECT                                                            ECS040
00083  FD  FICH                                                         ECS040
00084                              COPY ECSFICH.                        ECS040
00085  EJECT                                                            ECS040
00086  WORKING-STORAGE SECTION.                                         ECS040
00087  77  FILLER  PIC X(32) VALUE '********************************'.  ECS040
00088  77  FILLER  PIC X(32) VALUE '     ECS040 WORKING STORAGE     '.  ECS040
00089  77  FILLER  PIC X(32) VALUE '*****VMOD=2.006*****************'.  ECS040
00090                                                                   ECS040
00091  01  MISC.                                                        ECS040
00092      12  WS-RETURN-CODE      PIC S9(4)      COMP.                 ECS040
00093      12  WS-ABEND-FILE-STATUS PIC XX        VALUE '00'.           ECS040
00094      12  WS-ABEND-MESSAGE    PIC X(80).                           ECS040
00095      12  WS-ZERO             PIC S9          COMP-3  VALUE +0.    ECS040
00096      12  PGM-SUB             PIC S999        COMP-3  VALUE +40.   ECS040
00097      12  REC-COUNT           PIC S9(9)       COMP-3  VALUE ZERO.  ECS040
00098      12  MORT-RESV           PIC S9(11)V9(6) COMP-3  VALUE ZERO.  ECS040
00099      12  IN-FORCE            PIC S9(11)V99   COMP-3  VALUE ZERO.  ECS040
00100      12  AH-IN-FORCE         PIC S9(11)V99   COMP-3  VALUE ZERO.  ECS040
00101      12  SV-WORK-REC         PIC X(92).                           ECS040
00102      12  X                   PIC X                   VALUE ' '.   ECS040
00103                                                                      CL**8
00104      12  WS-WR-PDATE         PIC 9(07).                              CL**8
00105      12  WS-WR-PDATE-R REDEFINES WS-WR-PDATE.                        CL**5
00106          16  FILLER          PIC 9.                                  CL**7
00107          16  WR-PCC          PIC 99.                                 CL**7
00108          16  WR-PYR          PIC 99.                                 CL**7
00109          16  WR-PMO          PIC 99.                                 CL**8
00110                                                                   ECS040
00111  01  PRT-LINES.                                                   ECS040
00112      12  HDR-1.                                                   ECS040
00113          16  FILLER          PIC X(46)           VALUE SPACES.    ECS040
00114          16  FILLER          PIC X(74)           VALUE            ECS040
00115                  'MORTALITY RESERVE EXTRACT SUMMARY'.             ECS040
00116          16  FILLER          PIC X(8)            VALUE 'ECS040'.  ECS040
00117      12  HDR-2.                                                   ECS040
00118          16  FILLER          PIC X(47)           VALUE SPACES.    ECS040
00119          16  H2-COMP         PIC X(30).                           ECS040
00120          16  FILLER          PIC X(43)           VALUE SPACES.    ECS040
00121          16  H2-DATE         PIC X(8).                            ECS040
00122      12  HDR-3.                                                   ECS040
00123          16  FILLER          PIC X(53)           VALUE SPACES.    ECS040
00124          16  H3-DATE         PIC X(18).                           ECS040
00125          16  FILLER          PIC X(49)           VALUE SPACES.    ECS040
00126          16  FILLER          PIC X(5)            VALUE 'PAGE'.    ECS040
00127          16  HD-PAGE         PIC ZZ,ZZZ.                          ECS040
00128  EJECT                                                            ECS040
00129  01  TOTAL-LINES.                                                 ECS040
00130      12  TOT-1.                                                   ECS040
00131          16  FILLER          PIC X.                               ECS040
00132          16  FILLER          PIC X(39)           VALUE            ECS040
00133                  'TOTAL EXTRACTED RECORDS............    '.       ECS040
00134          16  T1-COUNT        PIC ZZZ,ZZZ,ZZ9.-.                   ECS040
00135      12  TOT-2.                                                   ECS040
00136          16  FILLER          PIC X.                               ECS040
00137          16  FILLER          PIC X(36)           VALUE            ECS040
00138                  'MORTALITY RESERVE.................. '.          ECS040
00139          16  T2-MORT         PIC ZZ,ZZZ,ZZZ,ZZZ.999999.           ECS040
00140      12  TOT-3.                                                   ECS040
00141          16  FILLER          PIC X.                               ECS040
00142          16  FILLER          PIC X(36)           VALUE            ECS040
00143                  'INSURANCE IN FORCE................. '.          ECS040
00144          16  T3-IN-FORCE     PIC ZZ,ZZZ,ZZZ,ZZZ.99.               ECS040
00145      12  TOT-4.                                                   ECS040
00146          16  FILLER          PIC X.                               ECS040
00147          16  TOT4-RES-TYPE   PIC X(12)           VALUE            ECS040
00148                  '            '.                                  ECS040
00149          16  FILLER          PIC X(24)           VALUE            ECS040
00150                  ' IN FORCE.............. '.                      ECS040
00151          16  T4-IN-FORCE     PIC ZZ,ZZZ,ZZZ,ZZZ.99.               ECS040
00152                                                                   ECS040
00153      COPY ELCDTECX.                                               ECS040
00154      COPY ELCDTEVR.                                               ECS040
00155                                                                   ECS040
00156  EJECT                                                            ECS040
00157  PROCEDURE DIVISION.                                              ECS040
00158                                                                   ECS040
00159  0000-READ-DATE.                                                  ECS040
00160                              COPY ELCDTERX SUPPRESS.              ECS040
00161                                                                   ECS040
00162  0100-OPEN-GAP.                                                   ECS040
00163      MOVE WS-CURRENT-DATE TO H2-DATE.                             ECS040
00164      MOVE ALPH-DATE       TO H3-DATE.                             ECS040
00165      MOVE COMPANY-NAME    TO H2-COMP.                             ECS040
00166                                                                   ECS040
00167      MOVE AH-OVERRIDE-L12 TO TOT4-RES-TYPE.                       ECS040
00168                                                                   ECS040
00169      OPEN INPUT GAP-FILE                                          ECS040
00170          OUTPUT PRNTR                                             ECS040
00171                 WORK-FILE.                                        ECS040
00172                                                                   ECS040
00173  0110-READ-GAP.                                                   ECS040
00174      READ GAP-FILE AT END                                         ECS040
00175          GO TO 0170-E-O-J.                                        ECS040
00176                                                                   ECS040
00177  0130-BUILD-WORK-REC.                                             ECS040
00178                                                                      CL**8
00179      MOVE SPACES        TO WORK-REC.                              ECS040
00180      MOVE GR-COMPANY-CD TO WR-COMPANY-CD.                         ECS040
00181      MOVE GR-CARRIER    TO WR-CARR.                               ECS040
00182      MOVE GR-GROUPING   TO WR-COMP.                               ECS040
00183      MOVE GR-STATE      TO WR-STATE.                              ECS040
00184      MOVE GR-ACCOUNT    TO WR-ACCT.                               ECS040
00185      MOVE GR-REIN-COMP  TO WR-REI-CO.                             ECS040
00186      MOVE GR-EFF        TO WR-DATE.                                  CL**8
00187      MOVE '1'           TO WR-CARD.                               ECS040
00188                                                                      CL**8
00189      MOVE ZEROS         TO WS-WR-PDATE.                              CL**8
00190      MOVE RUN-CC        TO WR-PCC.                                   CL**4
00191      MOVE RUN-YR        TO WR-PYR.                                   CL**8
00192      MOVE RUN-MO        TO WR-PMO.                                   CL**8
00193      MOVE WS-WR-PDATE   TO WR-PDATE.                                 CL**4
00194                                                                      CL**8
00195      MOVE WORK-REC      TO SV-WORK-REC.                           ECS040
00196                                                                   ECS040
00197      IF GR-LFTYP NOT = ZEROS                                      ECS040
00198          MOVE LIFE-OVERRIDE-L1  TO WR-LF-AH                       ECS040
00199          MOVE GR-LFTYP   TO WR-BEN-TYPE                           ECS040
00200          MOVE GR-REM-AMT TO WR-REM-AMT                            ECS040
00201          MOVE GR-RESV    TO WR-RESV                               ECS040
00202          ADD GR-RESV     TO MORT-RESV                             ECS040
00203          ADD GR-REM-AMT  TO IN-FORCE                              ECS040
00204          MOVE +0    TO     WR-FUT-RESERVE                         ECS040
00205                            WR-PTC-RESERVE                         ECS040
00206                            WR-IBNR-RESERVE                        ECS040
00207                            WR-CLM-ADJ-AMT                         ECS040
00208                            WR-EXPENSES                            ECS040
00209                            WR-PAYMENTS                            ECS040
00210                            WR-OTH-COMMISSIONS                     ECS040
00211                            WR-REIN-PREM-ADJS                      ECS040
00212          PERFORM 0140-WRITE-WORK-REC THRU 0140-EXIT.              ECS040
00213                                                                   ECS040
00214      IF DTE-CLIENT = 'PEK'                                        ECS040
00215          IF GR-AHTYP NOT = ZEROS                                  ECS040
00216              MOVE SV-WORK-REC TO WORK-REC                         ECS040
00217              MOVE AH-OVERRIDE-L1  TO WR-LF-AH                     ECS040
00218              MOVE GR-AHTYP    TO WR-BEN-TYPE                      ECS040
00219              MOVE GR-AHPRM    TO WR-REM-AMT                       ECS040
00220              MOVE ZEROS       TO WR-RESV                          ECS040
00221              ADD GR-AHPRM     TO AH-IN-FORCE                      ECS040
00222              PERFORM 0140-WRITE-WORK-REC THRU 0140-EXIT           ECS040
00223              GO TO 0110-READ-GAP.                                 ECS040
00224                                                                   ECS040
00225      IF GR-AHTYP NOT = ZEROS                                      ECS040
00226          MOVE SV-WORK-REC     TO WORK-REC                         ECS040
00227          MOVE AH-OVERRIDE-L1  TO WR-LF-AH                         ECS040
00228          MOVE GR-AHTYP        TO WR-BEN-TYPE                      ECS040
00229          MOVE GR-AH-REM-BEN   TO WR-REM-AMT                       ECS040
00230          MOVE ZEROS           TO WR-RESV                          ECS040
00231          ADD GR-AH-REM-BEN    TO AH-IN-FORCE                      ECS040
00232          PERFORM 0140-WRITE-WORK-REC THRU 0140-EXIT.              ECS040
00233                                                                   ECS040
00234      GO TO 0110-READ-GAP.                                         ECS040
00235                                                                   ECS040
00236  0140-WRITE-WORK-REC.                                             ECS040
00237      WRITE WORK-REC.                                              ECS040
00238                                                                   ECS040
00239      ADD +1 TO REC-COUNT.                                         ECS040
00240                                                                   ECS040
00241  0140-EXIT.                                                       ECS040
00242      EXIT.                                                        ECS040
00243  EJECT                                                            ECS040
00244  0150-PRT-RTN.                                                    ECS040
00245                              COPY ELCPRT2.                        ECS040
00246                                                                   ECS040
00247  0160-EXIT.                                                       ECS040
00248      EXIT.                                                        ECS040
00249  EJECT                                                            ECS040
00250  0170-E-O-J.                                                      ECS040
00251      MOVE HDR-1 TO PRT.                                           ECS040
00252      MOVE '1' TO X.                                               ECS040
00253      PERFORM 0150-PRT-RTN THRU 0160-EXIT.                         ECS040
00254                                                                   ECS040
00255      MOVE HDR-2 TO PRT.                                           ECS040
00256      MOVE ' ' TO X.                                               ECS040
00257      PERFORM 0150-PRT-RTN THRU 0160-EXIT.                         ECS040
00258                                                                   ECS040
00259      MOVE 1 TO HD-PAGE.                                           ECS040
00260      MOVE HDR-3 TO PRT.                                           ECS040
00261      MOVE ' ' TO X.                                               ECS040
00262      PERFORM 0150-PRT-RTN THRU 0160-EXIT.                         ECS040
00263                                                                   ECS040
00264      MOVE SPACES TO PRT.                                          ECS040
00265      MOVE '0' TO X.                                               ECS040
00266      PERFORM 0150-PRT-RTN THRU 0160-EXIT.                         ECS040
00267                                                                   ECS040
00268      MOVE REC-COUNT TO T1-COUNT.                                  ECS040
00269      MOVE TOT-1 TO PRT.                                           ECS040
00270      MOVE ' ' TO X.                                               ECS040
00271      PERFORM 0150-PRT-RTN THRU 0160-EXIT.                         ECS040
00272                                                                   ECS040
00273      MOVE MORT-RESV TO T2-MORT.                                   ECS040
00274      MOVE TOT-2 TO PRT.                                           ECS040
00275      MOVE ' ' TO X.                                               ECS040
00276      PERFORM 0150-PRT-RTN THRU 0160-EXIT.                         ECS040
00277                                                                   ECS040
00278      MOVE IN-FORCE TO T3-IN-FORCE.                                ECS040
00279      MOVE TOT-3 TO PRT.                                           ECS040
00280      MOVE ' ' TO X.                                               ECS040
00281      PERFORM 0150-PRT-RTN THRU 0160-EXIT.                         ECS040
00282                                                                   ECS040
00283      MOVE AH-IN-FORCE TO T4-IN-FORCE.                             ECS040
00284      MOVE TOT-4 TO PRT.                                           ECS040
00285      PERFORM 0150-PRT-RTN THRU 0160-EXIT.                         ECS040
00286                                                                   ECS040
00287      CLOSE                                                        ECS040
00288          GAP-FILE                                                 ECS040
00289          PRNTR                                                    ECS040
00290          WORK-FILE.                                               ECS040
00291                                                                   ECS040
00292  0180-CLOSE-FICH.                                                 ECS040
00293                              COPY ELCPRTC.                        ECS040
00294                                                                   ECS040
00295      GOBACK.                                                      ECS040
00296                                                                   ECS040
00297  ABEND-PGM.                                                       ECS040
00298                              COPY ELCABEND SUPPRESS.              ECS040
00299                                                                      CL**8
