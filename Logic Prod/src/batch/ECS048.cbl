00001  IDENTIFICATION DIVISION.                                         04/18/98
00002                                                                   ECS048
00003  PROGRAM-ID.                ECS048.                                  LV005
00004 *             PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE           ECS048
00005 *                           VMOD=2.006.                           ECS048
00006 *                                                                 ECS048
00007 *AUTHOR.        LOGIC, INC.                                       ECS048
00008 *               DALLAS, TEXAS.                                    ECS048
00009                                                                   ECS048
00010 *DATE-COMPILED.                                                   ECS048
00011                                                                   ECS048
00012 *SECURITY.   *****************************************************ECS048
00013 *            *                                                   *ECS048
00014 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ECS048
00015 *            *                                                   *ECS048
00016 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS048
00017 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ECS048
00018 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC, INC.     *ECS048
00019 *            *                                                   *ECS048
00020 *            *****************************************************ECS048
00021                                                                   ECS048
00022 *REMARKS.                                                         ECS048
00023 *        PROGRAM MERGES CURRENT MONTH RESERVE EXTRACTS FROM       ECS048
00024 *        THE UPDATE PROGRAM (10) WITH THE RESERVE HISTORY FILE.   ECS048
00025 *        RESERVE SEQUENCE IS REIN-COMPANY, REIN-SUB-COMPANY,      ECS048
00026 *        CARRIER, GROUP, STATE, ACCOUNT, CLAIM-NUMBER,            ECS048
00027 *        AND PROCESS-DATE.                                        ECS048
071806******************************************************************
071806*                   C H A N G E   L O G
071806*
071806* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
071806*-----------------------------------------------------------------
071806*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
071806* EFFECTIVE    NUMBER
071806*-----------------------------------------------------------------
071806* 071806   2006070200002   PEMA  CORRECT READING PAST EOF
071806******************************************************************
00028                                                                   ECS048
00029  ENVIRONMENT DIVISION.                                            ECS048
00030  INPUT-OUTPUT SECTION.                                            ECS048
00031  FILE-CONTROL.                                                    ECS048
00032                                                                   ECS048
00033      SELECT SORT-FILE       ASSIGN TO SYS001-UT-3380-S-SORTWK1.   ECS048
00034      SELECT PRNTR           ASSIGN TO SYS008-UR-1403-S-SYS008.    ECS048
00035      SELECT RESERVE-HISTORY ASSIGN TO SYS010-UT-2400-S-SYS010.    ECS048
00036      SELECT MERGE-RESERVE   ASSIGN TO SYS011-UT-2400-S-SYS011.    ECS048
00037      SELECT DE-EXTRACT      ASSIGN TO SYS018-UT-2400-S-SYS018.    ECS048
00038      SELECT DISK-DATE       ASSIGN TO SYS019-UT-3380-S-SYS019.    ECS048
00039      SELECT FICH            ASSIGN TO SYS020-UT-2400-S-SYS020.    ECS048
00040                                                                   ECS048
00041      SELECT ERMEBL                                                ECS048
00042              ASSIGN SYS024-FBA1-ERMEBL                            ECS048
00043              ORGANIZATION INDEXED                                 ECS048
00044              ACCESS DYNAMIC                                       ECS048
00045              RECORD KEY ME-CONTROL-PRIMARY                        ECS048
00046              FILE STATUS ERMEBL-FILE-STATUS.                      ECS048
00047                                                                   ECS048
00048  EJECT                                                            ECS048
00049  DATA DIVISION.                                                   ECS048
00050  FILE SECTION.                                                    ECS048
00051                                                                   ECS048
00052  SD  SORT-FILE.                                                   ECS048
00053                                                                   ECS048
00054  01  SORT-REC.                                                    ECS048
00055      12  S-RSV-REC.                                               ECS048
00056          16  FILLER              PIC X(334).                      ECS048
00057          16  SR-DE-RESERVE-TYPE  PIC X.                           ECS048
00058              88  SR-DE-LIFE-RSV                  VALUE '1' '3'.   ECS048
00059              88  SR-DE-AH-RSV                    VALUE '2' '4'.   ECS048
00060          16  FILLER              PIC X(175).                      ECS048
00061      12  SR-CNTRL.                                                ECS048
00062          16  S-REIN-COMP.                                         ECS048
00063              20 S-REIN           PIC XXX.                         ECS048
00064              20 S-REIN-SUB       PIC XXX.                         ECS048
00065          16  S-CARR              PIC X.                           ECS048
00066          16  S-GROUP             PIC X(6).                        ECS048
00067          16  S-ST                PIC XX.                          ECS048
00068          16  S-ACCT              PIC X(10).                       ECS048
00069          16  S-CLAIM             PIC X(7).                        ECS048
00070          16  S-CLM REDEFINES S-CLAIM.                             ECS048
00071              20  S1-CLM          PIC XX.                          ECS048
00072              20  S2-CLM          PIC X(5).                        ECS048
00073          16  S-PROC              PIC 9(11)    COMP-3.                CL**3
00074                                                                   ECS048
00075  FD  PRNTR                                                        ECS048
00076                                  COPY ELCPRTFD.                   ECS048
00077  EJECT                                                            ECS048
00078  FD  RESERVE-HISTORY                                              ECS048
00079      BLOCK CONTAINS 0 RECORDS
00080      RECORDING MODE IS F.                                         ECS048
00081                                                                   ECS048
00082  01  OLD-RSV                     PIC X(510).                      ECS048
00083                                                                   ECS048
00084  FD  MERGE-RESERVE                                                ECS048
00085      BLOCK CONTAINS 0 RECORDS
00086      RECORDING MODE IS F.                                         ECS048
00087                                                                   ECS048
00088  01  NEW-RSVS                    PIC X(510).                      ECS048
00089                                                                   ECS048
00090  FD  DE-EXTRACT                                                   ECS048
00091      BLOCK CONTAINS 0 RECORDS
00092      RECORDING MODE IS F.                                         ECS048
00093                                                                   ECS048
00094  01  IN-RSVS                     PIC X(510).                      ECS048
00095                                                                   ECS048
00096  FD  DISK-DATE                                                    ECS048
00097                                  COPY ELCDTEFD SUPPRESS.          ECS048
00098                                                                   ECS048
00099  FD  FICH                                                         ECS048
00100                                  COPY ELCFCHFD.                   ECS048
00101                                                                   ECS048
00102  FD  ERMEBL.                                                      ECS048
00103                                  COPY ERCMEBL.                    ECS048
00104  EJECT                                                            ECS048
00105  WORKING-STORAGE SECTION.                                         ECS048
00106  77  FILLER  PIC X(32) VALUE '********************************'.  ECS048
00107  77  FILLER  PIC X(32) VALUE '     ECS048 WORKING STORAGE     '.  ECS048
00108  77  FILLER  PIC X(32) VALUE '*********** VMOD=2.006 *********'.  ECS048
00109                                                                   ECS048
00110  01  MONTH-END-DATA.                                              ECS048
00111      12  ME-START-DATE.                                           ECS048
00112          16  ME-START-MO         PIC 99.                          ECS048
00113          16  FILLER              PIC X.                           ECS048
00114          16  ME-START-DA         PIC 99.                          ECS048
00115          16  FILLER              PIC X.                           ECS048
00116          16  ME-START-YR         PIC 99.                          ECS048
00117      12  ME-CNDS-DATE            PIC 9(6).                        ECS048
00118      12  ME-CNDS-DATE-R REDEFINES ME-CNDS-DATE.                   ECS048
00119          16  ME-CNDS-MO          PIC 99.                          ECS048
00120          16  ME-CNDS-DA          PIC 99.                          ECS048
00121          16  ME-CNDS-YR          PIC 99.                          ECS048
00122      12  ME-START-TIME           PIC 9(6).                        ECS048
00123      12  ME-UPDATE-FLAG          PIC X VALUE 'Y'.                 ECS048
00124          88  ME-DO-UPDATE        VALUE 'Y'.                       ECS048
00125          88  ME-NO-UPDATE        VALUE 'N'.                       ECS048
00126      12  ERMEBL-FILE-STATUS      PIC XX.                          ECS048
00127      12  MONTH-END-MOYR          PIC S9(5) COMP-3.                ECS048
00128                                                                   ECS048
00129  01  WS.                                                          ECS048
00130      12  PGM-SUB                 PIC S999 COMP       VALUE +048.  ECS048
00131      12  X                       PIC X               VALUE SPACE. ECS048
00132      12  BIN-DE-RSV-PROC-DT      PIC XX   VALUE SPACES.           ECS048
00133                                                                   ECS048
00134  01  WS-ABEND-FIELDS.                                             ECS048
00135      12  WS-RETURN-CODE          PIC S9(4)       VALUE ZERO.      ECS048
00136      12  WS-ZERO                 PIC S9          VALUE ZERO.      ECS048
00137      12  WS-ABEND-MESSAGE        PIC X(80)       VALUE SPACES.    ECS048
00138      12  WS-ABEND-FILE-STATUS    PIC XX          VALUE ZERO.      ECS048
00139                                                                   ECS048
00140  01  WS-DROP-DATE                PIC XX.                          ECS048
00141                                                                   ECS048
00142  01  TOTALS-COUNTERS.                                             ECS048
00143      12  RESERVES-HIST-CNTR              PIC 9(8) VALUE ZERO.     ECS048
00144      12  RESERVES-IN-CNTR                PIC 9(8) VALUE ZERO.     ECS048
00145      12  RESERVES-MERGED-OUT-CNTR        PIC 9(8) VALUE ZERO.     ECS048
00146      12  RESERVES-DROPPED-CNTR           PIC 9(8) VALUE ZERO.     ECS048
00147      12  LIFE-HIST-CNTR                  PIC 9(8) VALUE ZERO.     ECS048
00148      12  LIFE-IN-CNTR                    PIC 9(8) VALUE ZERO.     ECS048
00149      12  LIFE-MERGED-OUT-CNTR            PIC 9(8) VALUE ZERO.     ECS048
00150      12  LIFE-DROPPED-CNTR               PIC 9(8) VALUE ZERO.     ECS048
00151      12  AH-HIST-CNTR                    PIC 9(8) VALUE ZERO.     ECS048
00152      12  AH-IN-CNTR                      PIC 9(8) VALUE ZERO.     ECS048
00153      12  AH-MERGED-OUT-CNTR              PIC 9(8) VALUE ZERO.     ECS048
00154      12  AH-DROPPED-CNTR                 PIC 9(8) VALUE ZERO.     ECS048
00155                                                                   ECS048
00156  01  OLD-CNTRL.                                                   ECS048
00157      12  O-REIN-COMP.                                             ECS048
00158          16 O-REIN               PIC XXX.                         ECS048
00159          16 O-REIN-SUB           PIC XXX.                         ECS048
00160      12  O-CARR                  PIC X.                           ECS048
00161      12  O-GROUP                 PIC X(6).                        ECS048
00162      12  O-ST                    PIC XX.                          ECS048
00163      12  O-ACCT                  PIC X(10).                       ECS048
00164      12  O-CLAIM                 PIC X(7).                        ECS048
00165      12  O-PROC                  PIC 9(11)    COMP-3.                CL**3
00166                                                                   ECS048
00167  01  HEAD-1.                                                      ECS048
00168      12  FILLER      PIC X(52) VALUE SPACES.                      ECS048
00169      12  FILLER      PIC X(22) VALUE 'RESERVES HISTORY MERGE'.    ECS048
00170      12  FILLER      PIC X(45) VALUE SPACES.                      ECS048
00171      12  FILLER      PIC X(07) VALUE 'ECS048 '.                   ECS048
00172      12  FILLER      PIC X(06) VALUE SPACES.                      ECS048
00173                                                                   ECS048
00174  01  HEAD-2.                                                      ECS048
00175      12  FILLER      PIC X(47) VALUE SPACES.                      ECS048
00176      12  HD-CLIENT   PIC X(30) VALUE SPACES.                      ECS048
00177      12  FILLER      PIC X(42) VALUE SPACES.                      ECS048
00178      12  HD-DATE     PIC X(08) VALUE SPACES.                      ECS048
00179      12  FILLER      PIC X(05) VALUE SPACES.                      ECS048
00180                                                                   ECS048
00181  01  HEAD-3.                                                      ECS048
00182      12  FILLER      PIC X(53) VALUE SPACES.                      ECS048
00183      12  HD-ALF-DTE  PIC X(18) VALUE SPACES.                      ECS048
00184      12  FILLER      PIC X(48) VALUE SPACES.                      ECS048
00185      12  FILLER      PIC X(05) VALUE 'PAGE'.                      ECS048
00186      12  HD-PAGE     PIC ZZ,ZZZ.                                  ECS048
00187      12  FILLER      PIC X(02) VALUE SPACES.                      ECS048
00188                                                                   ECS048
00189  01  TOT-LINE.                                                    ECS048
00190      12  FILLER      PIC X     VALUE SPACES.                      ECS048
00191      12  TOT-DESC.                                                ECS048
00192          16  FILLER  PIC X(22) VALUE SPACES.                      ECS048
00193          16  TOT-L6  PIC X(6)  VALUE SPACES.                      ECS048
00194          16  FILLER  PIC X(2)  VALUE SPACES.                      ECS048
00195      12  FILLER      PIC X     VALUE SPACES.                      ECS048
00196      12  TOT-CNTR    PIC ZZ,ZZZ,ZZ9.                              ECS048
00197      12  FILLER      PIC X(90) VALUE SPACES.                      ECS048
00198  EJECT                                                            ECS048
00199                                  COPY ECSEXT01.                   ECS048
00200                                                                   ECS048
00201                                  COPY ELCDATE.                       CL**5
00202                                                                   ECS048
00203                                                                   ECS048
00204                                  COPY ELCDTECX.                      CL**5
00205                                  COPY ELCDTEVR.                      CL**5
00206  EJECT                                                            ECS048
00207  PROCEDURE DIVISION.                                              ECS048
00208                                                                   ECS048
00209  CAPTURE-START            SECTION.                                ECS048
00210                                                                   ECS048
00211      OPEN I-O ERMEBL.                                             ECS048
00212                                                                   ECS048
00213      IF ERMEBL-FILE-STATUS  = '00' OR '97'                        ECS048
00214          NEXT SENTENCE                                            ECS048
00215        ELSE                                                       ECS048
00216          MOVE 'N'                TO ME-UPDATE-FLAG.               ECS048
00217                                                                   ECS048
00218      MOVE ZEROS                  TO WS-RETURN-CODE                ECS048
00219                                     WS-ABEND-FILE-STATUS.         ECS048
00220                                                                   ECS048
00221  0100-SET-START.                                                  ECS048
00222                                  COPY ELCDTERX SUPPRESS.          ECS048
00223      MOVE WS-TIME                TO ME-START-TIME.                ECS048
00224      MOVE WS-CURRENT-DATE        TO ME-START-DATE.                ECS048
00225      MOVE ME-START-MO            TO ME-CNDS-MO.                   ECS048
00226      MOVE ME-START-DA            TO ME-CNDS-DA.                   ECS048
00227      MOVE ME-START-YR            TO ME-CNDS-YR.                   ECS048
00228                                                                   ECS048
00229      MOVE BIN-RUN-DATE           TO DC-BIN-DATE-1                 ECS048
00230      MOVE -60                    TO DC-ELAPSED-MONTHS.            ECS048
pemuni     move +0                     to dc-elapsed-days
00231      MOVE '6'                    TO DC-OPTION-CODE.               ECS048
00232      PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT.                 ECS048
00233      IF NO-CONVERSION-ERROR                                       ECS048
00234         MOVE DC-BIN-DATE-2       TO WS-DROP-DATE                  ECS048
00235      ELSE                                                         ECS048
00236         MOVE 'ERROR FINDING DROP DATE' TO WS-ABEND-MESSAGE        ECS048
00237         MOVE DC-OPTION-CODE  TO  WS-ABEND-FILE-STATUS             ECS048
00238         GO TO ABEND-PGM.                                             CL**2
00239                                                                   ECS048
00240      MOVE DTE-CLIENT             TO ME-COMPANY.                   ECS048
00241      COMPUTE MONTH-END-MOYR = (RUN-CCYY * 12) + RUN-MO.              CL**3
00242      MOVE MONTH-END-MOYR         TO ME-MOYR.                      ECS048
00243                                                                   ECS048
00244      IF ME-DO-UPDATE                                              ECS048
00245          READ ERMEBL INVALID KEY                                  ECS048
00246          MOVE 'N'                TO ME-UPDATE-FLAG                ECS048
00247          CLOSE ERMEBL.                                            ECS048
00248                                                                   ECS048
00249  0110-DO-SORT             SECTION.                                ECS048
00250                                                                   ECS048
00251  0120-NOW-SORT.                                                   ECS048
00252                                                                   ECS048
00253      SORT SORT-FILE ASCENDING KEY SR-CNTRL                        ECS048
00254          INPUT PROCEDURE 0140-GET-ONLY-RESERVES                   ECS048
00255          OUTPUT PROCEDURE 0180-MERGE-WITH-OLD.                    ECS048
00256                                                                   ECS048
00257      IF SORT-RETURN NOT = (ZEROS AND 4)                           ECS048
00258          MOVE  0101              TO WS-RETURN-CODE                ECS048
00259          GO TO ABEND-PGM.                                         ECS048
00260                                                                   ECS048
00261  0130-E-NOW-SORT.                                                 ECS048
00262      GO TO 0250-E-O-J-CLOSER.                                     ECS048
00263                                                                   ECS048
00264  EJECT                                                            ECS048
00265  0140-GET-ONLY-RESERVES   SECTION.                                ECS048
00266                                                                   ECS048
00267  0150-OPEN-EM.                                                    ECS048
00268      OPEN  INPUT DE-EXTRACT
pemuni           output merge-reserve
pemuni
00269      .                                                            ECS048
00270  0160-R-INPUT.                                                    ECS048
00271      READ DE-EXTRACT INTO DETAIL-EXTRACT AT END                   ECS048
00272          GO TO 0170-E-GET-RESERVES.                               ECS048
00273                                                                   ECS048
00274      IF DE-RECORD-ID NOT = 'DE'                                   ECS048
00275          GO TO 0160-R-INPUT.                                      ECS048
00276                                                                   ECS048
00277      IF NOT DE-RESERVE                                            ECS048
00278          GO TO 0160-R-INPUT.                                      ECS048
00279                                                                   ECS048
00280      ADD 1 TO RESERVES-IN-CNTR.                                   ECS048
00281                                                                   ECS048
00282      IF DE-LIFE-RSV                                               ECS048
00283          ADD 1 TO LIFE-IN-CNTR.                                   ECS048
00284                                                                   ECS048
00285      IF DE-AH-RSV                                                 ECS048
00286          ADD 1 TO AH-IN-CNTR.                                     ECS048
00287                                                                   ECS048
00288      MOVE DE-RESERVE-EXTRACT     TO SORT-REC.                     ECS048
00289      MOVE DE-CARRIER             TO S-CARR.                       ECS048
00290      MOVE DE-GROUPING            TO S-GROUP.                      ECS048
00291      MOVE SPACES                 TO S-REIN-COMP.                  ECS048
00292                                                                   ECS048
00293      IF DE-REIN NOT = SPACE                                       ECS048
00294          MOVE DE-REI-COMP        TO S-REIN-COMP.                  ECS048
00295                                                                   ECS048
00296      MOVE DE-STATE               TO S-ST.                         ECS048
00297      MOVE DE-ACCOUNT             TO S-ACCT.                       ECS048
00298      MOVE DE-CLMNO               TO S-CLAIM.                      ECS048
00299      MOVE DE-RSV-PROC-DT         TO S-PROC.                          CL**3
00300                                                                   ECS048
00301      RELEASE SORT-REC.                                            ECS048
00302      GO TO 0160-R-INPUT.                                          ECS048
00303                                                                   ECS048
00304  0170-E-GET-RESERVES.                                             ECS048
00305      EXIT.                                                        ECS048
00306  EJECT                                                            ECS048
00307  0180-MERGE-WITH-OLD      SECTION.                                ECS048
00308                                                                   ECS048
00309  0190-OPEN-OLDS.                                                  ECS048
00310      CLOSE DE-EXTRACT.                                            ECS048
00311                                                                   ECS048
00312      OPEN INPUT  RESERVE-HISTORY.                                 ECS048
pemuni*         OUTPUT MERGE-RESERVE.                                   ECS048
00314                                                                   ECS048
00315  0200-R-OLDS.                                                     ECS048

00316      READ RESERVE-HISTORY INTO DETAIL-EXTRACT AT END              ECS048
071806         MOVE HIGH-VALUES        TO OLD-CNTRL
00318          GO TO 0210-E-R-OLDS.                                     ECS048
00319                                                                   ECS048
00320      ADD 1 TO RESERVES-HIST-CNTR.                                 ECS048
00321                                                                   ECS048
00322      IF DE-LIFE-RSV                                               ECS048
00323          ADD 1 TO LIFE-HIST-CNTR.                                 ECS048
00324                                                                   ECS048
00325      IF DE-AH-RSV                                                 ECS048
00326          ADD 1 TO AH-HIST-CNTR.                                   ECS048
00327                                                                   ECS048
00328      MOVE DE-RSV-PROC-DT  TO  DC-GREG-DATE-CYMD.                     CL**3
00329      MOVE 'L'             TO  DC-OPTION-CODE.                        CL**3
00330      PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT.                 ECS048
00331      IF DATE-CONVERSION-ERROR                                        CL**3
00332         MOVE 'DE-RSV-PROC-DT CONVERSION ERROR'  TO                ECS048
00333                                     WS-ABEND-MESSAGE              ECS048
00334         MOVE DC-ERROR-CODE  TO  WS-ABEND-FILE-STATUS              ECS048
00335         GO TO ABEND-PGM.                                             CL**2
00336                                                                   ECS048
00337      IF DC-BIN-DATE-1 LESS THAN WS-DROP-DATE                         CL**3
00338          ADD 1 TO RESERVES-DROPPED-CNTR                           ECS048
00339          IF DE-LIFE-RSV                                           ECS048
00340              ADD 1 TO LIFE-DROPPED-CNTR                           ECS048
00341          END-IF                                                      CL**2
00342          IF DE-AH-RSV                                                CL**2
00343              ADD 1 TO AH-DROPPED-CNTR                                CL**2
00344          END-IF                                                      CL**2
00345          GO TO 0200-R-OLDS.                                          CL**2
00346                                                                   ECS048
00347      IF DE-REIN NOT = SPACES                                         CL**3
00348          MOVE DE-REI-COMP        TO O-REIN-COMP                      CL**3
00349      ELSE                                                            CL**3
00350          MOVE SPACES             TO O-REIN-COMP.                     CL**3
00351                                                                      CL**3
00352      MOVE DE-CARRIER             TO O-CARR.                       ECS048
00353      MOVE DE-GROUPING            TO O-GROUP.                      ECS048
00354      MOVE DE-STATE               TO O-ST.                         ECS048
00355      MOVE DE-ACCOUNT             TO O-ACCT.                       ECS048
00356      MOVE DE-CLMNO               TO O-CLAIM.                      ECS048
00357      MOVE DE-RSV-PROC-DT         TO O-PROC.                          CL**3
00358                                                                   ECS048
00359  0210-E-R-OLDS.                                                   ECS048
00360      EXIT.                                                        ECS048
00361                                                                   ECS048
00362  0220-R-SORTED.                                                   ECS048
00363      RETURN SORT-FILE AT END                                      ECS048
071806         MOVE HIGH-VALUES        TO SR-CNTRL.                     ECS048
00365                                                                   ECS048
00366  0230-MATCH-EM.                                                   ECS048
00367      IF (SR-CNTRL = OLD-CNTRL)
071806        AND (SR-CNTRL = HIGH-VALUES)
00369          GO TO 0240-E-MERGE-EM.                                   ECS048
00370                                                                   ECS048
00371      ADD 1 TO RESERVES-MERGED-OUT-CNTR.                           ECS048
00372                                                                   ECS048
00373      IF SR-CNTRL < OLD-CNTRL
00374          PERFORM 0245-COUNT-EM THRU 0245-COUNT-EM-EXIT            ECS048
00375          MOVE SORT-REC           TO NEW-RSVS                      ECS048
00376          WRITE NEW-RSVS                                           ECS048
00377          GO TO 0220-R-SORTED.                                     ECS048
00378                                                                   ECS048
00379      MOVE OLD-RSV                TO NEW-RSVS.                     ECS048
00380                                                                   ECS048
00381      IF DE-LIFE-RSV                                               ECS048
00382          ADD 1 TO LIFE-MERGED-OUT-CNTR.                           ECS048
00383                                                                   ECS048
00384      IF DE-AH-RSV                                                 ECS048
00385          ADD 1 TO AH-MERGED-OUT-CNTR.                             ECS048
00386                                                                   ECS048
00387      WRITE NEW-RSVS.                                              ECS048
00388      PERFORM 0200-R-OLDS THRU 0210-E-R-OLDS.                      ECS048
00389      GO TO 0230-MATCH-EM.                                         ECS048
00390                                                                   ECS048
00391  0240-E-MERGE-EM.                                                 ECS048
00392      EXIT.                                                        ECS048
00393                                                                   ECS048
00394  0245-COUNT-EM                SECTION.                            ECS048
00395                                                                   ECS048
00396      IF SR-DE-LIFE-RSV                                            ECS048
00397          ADD 1 TO LIFE-MERGED-OUT-CNTR.                           ECS048
00398                                                                   ECS048
00399      IF SR-DE-AH-RSV                                              ECS048
00400          ADD 1 TO AH-MERGED-OUT-CNTR.                             ECS048
00401                                                                   ECS048
00402  0245-COUNT-EM-EXIT.                                              ECS048
00403      EXIT.                                                        ECS048
00404  EJECT                                                            ECS048
00405  0250-E-O-J-CLOSER            SECTION.                            ECS048
00406      OPEN OUTPUT PRNTR.                                           ECS048
00407                                                                   ECS048
00408  0260-PRINT-HEADINGS.                                             ECS048
00409      MOVE '1'                    TO X.                            ECS048
00410      MOVE HEAD-1                 TO P-DATA.                       ECS048
00411      PERFORM 0270-PRINT-RTN THRU 0280-PRINT-RTN-EXIT.             ECS048
00412      MOVE ' '                    TO X.                            ECS048
00413      MOVE WS-CURRENT-DATE        TO HD-DATE.                      ECS048
00414      MOVE COMPANY-NAME           TO HD-CLIENT.                    ECS048
00415      MOVE HEAD-2                 TO P-DATA.                       ECS048
00416      PERFORM 0270-PRINT-RTN THRU 0280-PRINT-RTN-EXIT.             ECS048
00417      MOVE 1                      TO HD-PAGE.                      ECS048
00418      MOVE ALPH-DATE              TO HD-ALF-DTE.                   ECS048
00419      MOVE HEAD-3                 TO P-DATA.                       ECS048
00420      PERFORM 0270-PRINT-RTN THRU 0280-PRINT-RTN-EXIT.             ECS048
00421                                                                   ECS048
00422      IF ME-DO-UPDATE                                              ECS048
00423          MOVE RESERVES-HIST-CNTR           TO ME-048-RECS-IN      ECS048
00424          MOVE RESERVES-MERGED-OUT-CNTR     TO ME-048-RECS-OUT.    ECS048
00425                                                                   ECS048
00426      MOVE 'NEW RESERVE RECORDS - XXXXXX =' TO TOT-DESC.           ECS048
00427      MOVE LIFE-OVERRIDE-L6                 TO TOT-L6.             ECS048
00428      MOVE LIFE-IN-CNTR                   TO TOT-CNTR.             ECS048
00429      MOVE '-'                            TO X.                    ECS048
00430      MOVE TOT-LINE                       TO P-DATA.               ECS048
00431      PERFORM 0270-PRINT-RTN THRU 0280-PRINT-RTN-EXIT.             ECS048
00432                                                                   ECS048
00433      MOVE 'NEW RESERVE RECORDS - XXXXXX =' TO TOT-DESC.           ECS048
00434      MOVE AH-OVERRIDE-L6                   TO TOT-L6.             ECS048
00435      MOVE AH-IN-CNTR                     TO TOT-CNTR.             ECS048
00436      MOVE ' '                            TO X.                    ECS048
00437      MOVE TOT-LINE                       TO P-DATA.               ECS048
00438      PERFORM 0270-PRINT-RTN THRU 0280-PRINT-RTN-EXIT.             ECS048
00439                                                                   ECS048
00440      MOVE 'NEW RESERVE RECORDS - TOTAL  =' TO TOT-DESC.           ECS048
00441      MOVE RESERVES-IN-CNTR               TO TOT-CNTR.             ECS048
00442      MOVE ' '                            TO X.                    ECS048
00443      MOVE TOT-LINE                       TO P-DATA.               ECS048
00444      PERFORM 0270-PRINT-RTN THRU 0280-PRINT-RTN-EXIT.             ECS048
00445                                                                   ECS048
00446      MOVE 'HISTORY RESERVES IN - XXXXXX =' TO TOT-DESC.           ECS048
00447      MOVE LIFE-OVERRIDE-L6                 TO TOT-L6.             ECS048
00448      MOVE LIFE-HIST-CNTR                 TO TOT-CNTR.             ECS048
00449      MOVE '0'                            TO X.                    ECS048
00450      MOVE TOT-LINE                       TO P-DATA.               ECS048
00451      PERFORM 0270-PRINT-RTN THRU 0280-PRINT-RTN-EXIT.             ECS048
00452                                                                   ECS048
00453      MOVE 'HISTORY RESERVES IN - XXXXXX =' TO TOT-DESC.           ECS048
00454      MOVE AH-OVERRIDE-L6                   TO TOT-L6.             ECS048
00455      MOVE AH-HIST-CNTR                   TO TOT-CNTR.             ECS048
00456      MOVE ' '                            TO X.                    ECS048
00457      MOVE TOT-LINE                       TO P-DATA.               ECS048
00458      PERFORM 0270-PRINT-RTN THRU 0280-PRINT-RTN-EXIT.             ECS048
00459                                                                   ECS048
00460      MOVE 'HISTORY RESERVES IN - TOTAL  =' TO TOT-DESC.           ECS048
00461      MOVE RESERVES-HIST-CNTR             TO TOT-CNTR.             ECS048
00462      MOVE ' '                            TO X.                    ECS048
00463      MOVE TOT-LINE                       TO P-DATA.               ECS048
00464      PERFORM 0270-PRINT-RTN THRU 0280-PRINT-RTN-EXIT.             ECS048
00465                                                                   ECS048
00466      IF RESERVES-DROPPED-CNTR NOT = ZERO                          ECS048
00467          MOVE 'HISTORY RSV DROPPED - XXXXXX =' TO TOT-DESC        ECS048
00468          MOVE LIFE-OVERRIDE-L6                 TO TOT-L6          ECS048
00469          MOVE LIFE-DROPPED-CNTR              TO TOT-CNTR          ECS048
00470          MOVE '0'                            TO X                 ECS048
00471          MOVE TOT-LINE                       TO P-DATA            ECS048
00472          PERFORM 0270-PRINT-RTN THRU 0280-PRINT-RTN-EXIT          ECS048
00473          MOVE 'HISTORY RSV DROPPED - XXXXXX =' TO TOT-DESC        ECS048
00474          MOVE AH-OVERRIDE-L6                   TO TOT-L6          ECS048
00475          MOVE AH-DROPPED-CNTR                TO TOT-CNTR          ECS048
00476          MOVE ' '                            TO X                 ECS048
00477          MOVE TOT-LINE                       TO P-DATA            ECS048
00478          PERFORM 0270-PRINT-RTN THRU 0280-PRINT-RTN-EXIT          ECS048
00479          MOVE 'HISTORY RSV DROPPED - TOTAL  =' TO TOT-DESC        ECS048
00480          MOVE RESERVES-DROPPED-CNTR          TO TOT-CNTR          ECS048
00481          MOVE ' '                            TO X                 ECS048
00482          MOVE TOT-LINE                       TO P-DATA            ECS048
00483          PERFORM 0270-PRINT-RTN THRU 0280-PRINT-RTN-EXIT.         ECS048
00484                                                                   ECS048
00485      MOVE 'HISTORY RESERVE OUT - XXXXXX =' TO TOT-DESC.           ECS048
00486      MOVE LIFE-OVERRIDE-L6                 TO TOT-L6.             ECS048
00487      MOVE LIFE-MERGED-OUT-CNTR           TO TOT-CNTR.             ECS048
00488      MOVE '0'                            TO X.                    ECS048
00489      MOVE TOT-LINE                       TO P-DATA.               ECS048
00490      PERFORM 0270-PRINT-RTN THRU 0280-PRINT-RTN-EXIT.             ECS048
00491                                                                   ECS048
00492      MOVE 'HISTORY RESERVE OUT - XXXXXX =' TO TOT-DESC.           ECS048
00493      MOVE AH-OVERRIDE-L6                   TO TOT-L6.             ECS048
00494      MOVE AH-MERGED-OUT-CNTR             TO TOT-CNTR.             ECS048
00495      MOVE ' '                            TO X.                    ECS048
00496      MOVE TOT-LINE                       TO P-DATA.               ECS048
00497      PERFORM 0270-PRINT-RTN THRU 0280-PRINT-RTN-EXIT.             ECS048
00498                                                                   ECS048
00499      MOVE 'HISTORY RESERVE OUT - TOTAL  =' TO TOT-DESC.           ECS048
00500      MOVE RESERVES-MERGED-OUT-CNTR       TO TOT-CNTR.             ECS048
00501      MOVE ' '                            TO X.                    ECS048
00502      MOVE TOT-LINE                       TO P-DATA.               ECS048
00503      PERFORM 0270-PRINT-RTN THRU 0280-PRINT-RTN-EXIT.             ECS048
00504                                                                   ECS048
00505      CLOSE RESERVE-HISTORY MERGE-RESERVE.                         ECS048
00506                                                                   ECS048
00507      IF ME-DO-UPDATE                                              ECS048
00508          MOVE ME-START-TIME      TO ME-048-START                  ECS048
00509          MOVE ME-CNDS-DATE       TO ME-048-RUN-DT                 ECS048
00510          ACCEPT WS-TIME-OF-DAY   FROM TIME                        ECS048
00511          MOVE WS-TIME            TO ME-048-END                    ECS048
00512          ADD 1                   TO ME-048-RUN-CT                 ECS048
00513          REWRITE MONTH-END-BALANCES                               ECS048
00514          CLOSE ERMEBL.                                            ECS048
00515                                                                   ECS048
00516      IF ME-DO-UPDATE                                              ECS048
00517          MOVE 'MONTH-END BALANCES POSTED'     TO P-DATA           ECS048
00518      ELSE                                                         ECS048
00519          MOVE 'MONTH-END BALANCES NOT POSTED' TO P-DATA.          ECS048
00520                                                                   ECS048
00521      MOVE '-'                            TO X.                    ECS048
00522      PERFORM 0270-PRINT-RTN THRU 0280-PRINT-RTN-EXIT.             ECS048
00523                                                                   ECS048
00524      MOVE '-'                            TO X.                    ECS048
00525      MOVE 'RESERVES HISTORY UPDATE COMPLETED' TO P-DATA.          ECS048
00526      PERFORM 0270-PRINT-RTN THRU 0280-PRINT-RTN-EXIT.             ECS048
00527      GO TO 0290-CLOSE-FICH.                                       ECS048
00528                                                                   ECS048
00529  0270-PRINT-RTN.                                                  ECS048
00530                                  COPY ELCPRT2.                    ECS048
00531                                                                   ECS048
00532  0280-PRINT-RTN-EXIT.                                             ECS048
00533      EXIT.                                                        ECS048
00534                                                                   ECS048
00535  0290-CLOSE-FICH.                                                 ECS048
00536                                  COPY ELCPRTC.                    ECS048
00537                                                                   ECS048
00538  0300-CLOSE-PRINTER.                                              ECS048
00539      CLOSE PRNTR.                                                 ECS048
00540                                                                   ECS048
00541      GOBACK.                                                      ECS048
00542                                                                   ECS048
00543      COPY ELCDCS.                                                    CL**4
00544                                                                   ECS048
00545  ABEND-PGM SECTION.                                               ECS048
00546                                COPY ELCABEND SUPPRESS.            ECS048
