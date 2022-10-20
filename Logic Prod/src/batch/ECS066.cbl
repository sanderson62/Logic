00001  IDENTIFICATION DIVISION.                                         03/06/98
00002                                                                   ECS066
00003  PROGRAM-ID.                ECS066.                                  LV002
00004 *              PROGRAM CONVERTED BY                               ECS066
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ECS066
00006 *              CONVERSION DATE 02/08/96 18:17:49.                 ECS066
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          ECS066
00008 *                           VMOD=2.006.                           ECS066
00009                                                                   ECS066
00010 *AUTHOR.        LOGIC, INC.                                       ECS066
00011 *               DALLAS, TEXAS.                                    ECS066
00012                                                                   ECS066
00013 *DATE-COMPILED.                                                   ECS066
00014                                                                   ECS066
00015 *SECURITY.   *****************************************************ECS066
00016 *            *                                                   *ECS066
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ECS066
00018 *            *                                                   *ECS066
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS066
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ECS066
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *ECS066
00022 *            *                                                   *ECS066
00023 *            *****************************************************ECS066
00024                                                                   ECS066
00025 *REMARKS.                                                         ECS066
00026 *        PRINTS ROLODEX CARDS FROM THE COMPENSATION MASTER.       ECS066
00027                                                                   ECS066
00028 *        PROGRAM OPTION      -     DESCRIPTION.                   ECS066
00029                                                                   ECS066
00030 *        1   -   ROLODEX FOR MASTERS WITH MAINTENANCE THIS MONTH  ECS066
00031 *        2   -   ROLODEX FOR ALL MASTERS                          ECS066
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
051810* 051810 CR2010042900001   PEMA  SPEARATE CITY AND STATE
      ******************************************************************
00033  ENVIRONMENT DIVISION.                                            ECS066
00034  INPUT-OUTPUT SECTION.                                            ECS066
00035  FILE-CONTROL.                                                    ECS066
00036                                                                   ECS066
00037      SELECT PRNTR            ASSIGN TO SYS008-UR-1403-S-SYS008.   ECS066
00038      SELECT COMM-MSTR-IN     ASSIGN TO SYS010-UT-2400-S-SYS010.   ECS066
00039      SELECT DISK-DATE        ASSIGN TO SYS019-UT-FBA1-S-SYS019.   ECS066
00040      SELECT FICH             ASSIGN TO SYS020-UT-2400-S-SYS020.   ECS066
00041  EJECT                                                            ECS066
00042  DATA DIVISION.                                                   ECS066
00043  FILE SECTION.                                                    ECS066
00044                                                                   ECS066
00045  FD  PRNTR                                                        ECS066
00046                              COPY ELCPRTFD.                       ECS066
00047  EJECT                                                            ECS066
00048  FD  COMM-MSTR-IN                                                 ECS066
00049                              COPY ECSCOIFD.                       ECS066
00050  EJECT                                                            ECS066
00051  FD  DISK-DATE                                                    ECS066
00052                              COPY ELCDTEFD.                       ECS066
00053  EJECT                                                            ECS066
00054  FD  FICH                                                         ECS066
00055                              COPY ELCFCHFD.                       ECS066
00056  EJECT                                                            ECS066
00057  WORKING-STORAGE SECTION.                                         ECS066
00058  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      ECS066
00059  77  FILLER PIC X(32) VALUE '********************************'.   ECS066
00060  77  FILLER PIC X(32) VALUE '*            ECS066            *'.   ECS066
00061  77  FILLER PIC X(32) VALUE '*********** VMOD=2.006 *********'.   ECS066
00062                                                                   ECS066
00063  77  K0                      PIC S9          VALUE +0   COMP-3.   ECS066
00064  77  K1                      PIC S9          VALUE +1   COMP-3.   ECS066
00065  77  K2                      PIC S9          VALUE +2   COMP-3.   ECS066
00066  77  K3                      PIC S9          VALUE +3   COMP-3.   ECS066
00067  77  X1                      PIC S9(3)       VALUE +0   COMP-3.   ECS066
00068  77  PGM-SUB                 PIC S9(3)       VALUE +66  COMP-3.   ECS066
00069  77  MAX-NDX                 PIC S9(3)       VALUE +2   COMP-3.   ECS066
00070  77  A                       PIC S9(3)       VALUE +0   COMP-3.   ECS066
00071  77  B                       PIC S9(3)       VALUE +0   COMP-3.   ECS066
00072  77  NBR-OF-CARDS            PIC S9(7)       VALUE +0   COMP-3.   ECS066
00073  77  MAX-ALIGN-CTR           PIC S9(9)       VALUE +4   COMP-3.   ECS066
00074  77  ALIGN-CTR               PIC S9(9)       VALUE +0   COMP-3.   ECS066
00075  77  X                       PIC  X          VALUE ' '.           ECS066
00076  77  SPACE-NP                PIC  X          VALUE '1'.           ECS066
00077  77  SPACE-1                 PIC  X          VALUE ' '.           ECS066
00078  77  SPACE-2                 PIC  X          VALUE '0'.           ECS066
00079  77  SPACE-3                 PIC  X          VALUE '-'.           ECS066
00080 *                                                                 ECS066
00081 *                                                                 ECS066
00082 * LEAVE THE ABOVE 2 LINE IN TO PREVENT LOSING THE LINE ABOVE      ECS066
00094                                                                   ECS066
00095  01  WS.                                                          ECS066
00096      12  WS-RETURN-CODE          PIC S9(4)              COMP.     ECS066
00097      12  WS-ABEND-MESSAGE        PIC  X(80).                      ECS066
00098      12  WS-ABEND-FILE-STATUS    PIC  X(2)   VALUE ZEROS.         ECS066
00099      12  WS-ZERO                 PIC S9      VALUE ZERO COMP-3.   ECS066
00100      12  CMR-DATE.                                                ECS066
00101          16  CMR-MONTH           PIC  9(2).                       ECS066
00102          16  CMR-DAY             PIC  9(2).                       ECS066
00103          16  CMR-YEAR            PIC  9(2).                       ECS066
00104  EJECT                                                            ECS066
00105                              COPY ERCCOMP.                        ECS066
00106  EJECT                                                            ECS066
00107  01  MISC-WS.                                                     ECS066
00108      12  A-NAME.                                                  ECS066
00109          16  A-CHAR          PIC  X      OCCURS 30 TIMES.         ECS066
00110      12  B-NAME.                                                  ECS066
00111          16  B-CHAR          PIC  X      OCCURS 30 TIMES.         ECS066
00112                                                                   ECS066
00113  01  ROLODEX-AREA.                                                ECS066
00114      12  ROLX-1.                                                  ECS066
00115          16  RX-1        OCCURS 2 TIMES.                          ECS066
00116              20  FILLER              PIC  X(2).                   ECS066
00117              20  R1-INFO.                                         ECS066
00118                  24  FILLER          PIC  X(19).                  ECS066
00119                  24  R1-CARR         PIC  X.                      ECS066
00120                  24  R1-GROUP        PIC  X(6).                   ECS066
00121                  24  R1-DSH1         PIC  X.                      ECS066
00122                  24  R1-RESP         PIC  X(10).                  ECS066
00123                  24  R1-DSH2         PIC  X.                      ECS066
00124                  24  R1-ACCT         PIC  X(10).                  ECS066
00125      12  ROLX-2.                                                  ECS066
00126          16  RX-2        OCCURS 2 TIMES.                          ECS066
00127              20  FILLER              PIC  X(2).                   ECS066
00128              20  R2-INFO.                                         ECS066
00129                  24  R2-ACCT-NAME    PIC  X(30).                  ECS066
00130                  24  FILLER          PIC  X(18).                  ECS066
00131      12  ROLX-3.                                                  ECS066
00132          16  RX-3        OCCURS 2 TIMES.                          ECS066
00133              20  FILLER              PIC  X(2).                   ECS066
00134              20  R3-INFO.                                         ECS066
00135                  24  FILLER          PIC  X(30).                  ECS066
00136                  24  R3-CON1         PIC  X(6).                   ECS066
00137                  24  R3-AREA         PIC  X(3).                   ECS066
00138                  24  FILLER          PIC  X.                      ECS066
00139                  24  R3-PREF         PIC  X(3).                   ECS066
00140                  24  R3-DSH1         PIC  X.                      ECS066
00141                  24  R3-FONE         PIC  X(4).                   ECS066
00142      12  ROLX-4.                                                  ECS066
00143          16  RX-4        OCCURS 2 TIMES.                          ECS066
00144              20  FILLER              PIC  X(2).                   ECS066
00145              20  R4-INFO.                                         ECS066
00146                  24  R4-MAIL-NAME    PIC  X(30).                  ECS066
00147                  24  FILLER          PIC  X(18).                  ECS066
00148      12  ROLX-5.                                                  ECS066
00149          16  RX-5        OCCURS 2 TIMES.                          ECS066
00150              20  FILLER              PIC  X(2).                   ECS066
00151              20  R5-INFO.                                         ECS066
00152                  24  FILLER          PIC  X(30).                  ECS066
00153                  24  R5-CON1         PIC  X(5).                   ECS066
00154                  24  R5-SS-ID        PIC  X(13).                  ECS066
00155      12  ROLX-6.                                                  ECS066
00156          16  RX-6        OCCURS 2 TIMES.                          ECS066
00157              20  FILLER              PIC  X(2).                   ECS066
00158              20  R6-INFO.                                         ECS066
00159                  24  R6-ADDR-1       PIC  X(30).                  ECS066
00160                  24  FILLER          PIC  X(18).                  ECS066
00161      12  ROLX-7.                                                  ECS066
00162          16  RX-7        OCCURS 2 TIMES.                          ECS066
00163              20  FILLER              PIC  X(2).                   ECS066
00164              20  R7-INFO             PIC  X(48).                  ECS066
00165      12  ROLX-8.                                                  ECS066
00166          16  RX-8        OCCURS 2 TIMES.                          ECS066
00167              20  FILLER              PIC  X(2).                   ECS066
00168              20  R8-INFO.                                         ECS066
00169                  24  FILLER          PIC  X(31).                  ECS066
00170                  24  R8-ZIP          PIC  X(9).                   ECS066
00171                  24  FILLER          PIC  X(8).                   ECS066
00172      12  ROLX-9.                                                  ECS066
00173          16  RX-9        OCCURS 2 TIMES.                          ECS066
00174              20  FILLER              PIC  X(2).                   ECS066
00175              20  R9-INFO.                                         ECS066
00176                  24  FILLER          PIC  X(48).                  ECS066
00177  EJECT                                                            ECS066
00178  01  P-REC.                                                       ECS066
00179      12  P-CCSW              PIC  X.                              ECS066
00180      12  P-LN.                                                    ECS066
00181          16  FILLER          PIC  X(132).                         ECS066
00182  EJECT                                                            ECS066
00183                              COPY ELCDATE.                        ECS066
00184  EJECT                                                            ECS066
00185                              COPY ELCDTECX.                       ECS066
00186  EJECT                                                            ECS066
00187                              COPY ELCDTEVR.                       ECS066
00188  EJECT                                                            ECS066
00189  PROCEDURE DIVISION.                                              ECS066
00190                                                                   ECS066
00191  0000-STANDARD-COPY.                                              ECS066
00192                              COPY ELCDTERX.                       ECS066
00193  EJECT                                                            ECS066
00194  1000-INTL-INPUT.                                                 ECS066
00195      OPEN INPUT  COMM-MSTR-IN                                     ECS066
00196           OUTPUT PRNTR.                                           ECS066
00197                                                                   ECS066
00198      IF DTE-PGM-OPT  IS LESS THAN  1                              ECS066
00199        OR  DTE-PGM-OPT  IS GREATER THAN  2                        ECS066
00200          MOVE 1                  TO  DTE-PGM-OPT.                 ECS066
00201                                                                   ECS066
00202      IF DTE-PGM-OPT = 2                                           ECS066
00203          MOVE +001               TO  MAX-ALIGN-CTR.               ECS066
00204                                                                   ECS066
00205      MOVE SPACES                 TO  ROLODEX-AREA.                ECS066
00206      MOVE ALL 'X'                TO  R1-INFO (1)  R1-INFO (2)     ECS066
00207                                      R2-INFO (1)  R2-INFO (2)     ECS066
00208                                      R3-INFO (1)  R3-INFO (2)     ECS066
00209                                      R4-INFO (1)  R4-INFO (2)     ECS066
00210                                      R5-INFO (1)  R5-INFO (2)     ECS066
00211                                      R6-INFO (1)  R6-INFO (2)     ECS066
00212                                      R7-INFO (1)  R7-INFO (2)     ECS066
00213                                      R8-INFO (1)  R8-INFO (2)     ECS066
00214                                      R9-INFO (1)  R9-INFO (2).    ECS066
00215      MOVE COMPANY-NAME           TO  R2-INFO (1)  R2-INFO (2).    ECS066
00216      MOVE MAX-NDX                TO  X1.                          ECS066
00217                                                                   ECS066
00218      GO TO 2000-RD-COMM.                                          ECS066
00219  EJECT                                                            ECS066
00220  2000-RD-COMM.                                                    ECS066
00221      READ COMM-MSTR-IN                                            ECS066
00222          AT END                                                   ECS066
00223             GO TO 9990-E-O-J.                                     ECS066
00224                                                                   ECS066
00225      MOVE COMP-IN-RECORD         TO  COMPENSATION-MASTER.         ECS066
00226                                                                   ECS066
00227      IF CO-AUTO-GENERATED-THIS-RUN                                ECS066
00228        OR  CO-AUTO-GENERATED                                      ECS066
00229          GO TO 2000-RD-COMM.                                      ECS066
00230                                                                   ECS066
00231  3400-SELECT-ROLODEX.                                             ECS066
00232      IF DTE-PGM-OPT = 2                                           ECS066
00233          GO TO 5000-FORMAT-ROLODEX-RTN.                           ECS066
00234                                                                   ECS066
00235      IF CO-ROLADEX-PRINT-DT IS EQUAL TO LOW-VALUES                ECS066
00236          GO TO 2000-RD-COMM.                                      ECS066
00237                                                                   ECS066
00238      MOVE CO-ROLADEX-PRINT-DT    TO  DC-BIN-DATE-1.               ECS066
00239      MOVE ' '                    TO  DC-OPTION-CODE.              ECS066
00240                                                                   ECS066
00241      PERFORM 8500-DATE-CONVERSION  THRU  8599-EXIT.               ECS066
00242                                                                   ECS066
00243      IF DATE-CONVERSION-ERROR                                     ECS066
00244          MOVE '0997'             TO  WS-RETURN-CODE               ECS066
00245          MOVE 'MAINT DATE CONVERSION ERROR'                       ECS066
00246                                  TO  WS-ABEND-MESSAGE             ECS066
00247          GO TO ABEND-PGM.                                         ECS066
00248                                                                   ECS066
00249      MOVE DC-GREG-DATE-1-MDY     TO  CMR-DATE.                    ECS066
00250                                                                   ECS066
00251      IF CMR-MONTH = RUN-MO                                        ECS066
00252        AND  CMR-YEAR = RUN-YR                                     ECS066
00253          GO TO 5000-FORMAT-ROLODEX-RTN.                           ECS066
00254                                                                   ECS066
00255      GO TO 2000-RD-COMM.                                          ECS066
00256  EJECT                                                            ECS066
00257  5000-FORMAT-ROLODEX-RTN.                                         ECS066
00258      IF CO-COMPANY-TYPE                                           ECS066
00259          PERFORM 6000-LEFT-RTN  THRU  6199-EXIT.                  ECS066
00260                                                                   ECS066
00261      ADD K1                      TO  X1.                          ECS066
00262                                                                   ECS066
00263      IF X1  IS GREATER THAN  MAX-NDX                              ECS066
00264          PERFORM 5800-GEN-ROLX-RTN  THRU  5999-EXIT.              ECS066
00265                                                                   ECS066
00266      ADD +1                      TO  NBR-OF-CARDS.                ECS066
00267                                                                   ECS066
00268      MOVE CO-CARRIER             TO  R1-CARR      (X1).           ECS066
00269      MOVE CO-GROUPING            TO  R1-GROUP     (X1).           ECS066
00270      MOVE CO-RESP-NO             TO  R1-RESP      (X1).           ECS066
00271                                                                   ECS066
00272      IF CO-ACCOUNT NOT = LOW-VALUE                                ECS066
00273          MOVE CO-ACCOUNT         TO  R1-ACCT (X1)                 ECS066
00274          MOVE '-'                TO  R1-DSH2 (X1).                ECS066
00275                                                                   ECS066
00276      MOVE '-'                    TO  R1-DSH1 (X1).                ECS066
00277                                                                   ECS066
00278      MOVE CO-ACCT-NAME           TO  R2-ACCT-NAME (X1).           ECS066
00279      MOVE CO-AREA-CODE           TO  R3-AREA      (X1).           ECS066
00280      MOVE CO-PREFIX              TO  R3-PREF      (X1).           ECS066
00281      MOVE CO-PHONE               TO  R3-FONE      (X1).           ECS066
00282      MOVE '-'                    TO  R3-DSH1 (X1).                ECS066
00283      MOVE 'PHONE '               TO  R3-CON1 (X1).                ECS066
00284      MOVE CO-MAIL-NAME           TO  R4-MAIL-NAME (X1).           ECS066
00285      MOVE 'TAX  '                TO  R5-CON1 (X1).                ECS066
00286      MOVE CO-SOC-SEC             TO  R5-SS-ID (X1).               ECS066
00287      MOVE CO-ADDR-1              TO  R6-ADDR-1 (X1).              ECS066
00288      MOVE CO-ADDR-2              TO  R7-INFO (X1).                ECS066
051810     MOVE SPACES                 TO  R8-INFO (X1)
051810     STRING CO-ADDR-CITY ' ' CO-ADDR-STATE
051810        DELIMITED BY '  ' INTO R8-INFO (X1)
051810     END-STRING
00290      MOVE CO-ZIP                 TO  R8-ZIP  (X1).                ECS066
00291                                                                   ECS066
00292      IF CO-GEN-AGENT-TYPE                                         ECS066
00293          MOVE 'GENERAL AGENT RECORD '                             ECS066
00294                                  TO  R9-INFO (X1).                ECS066
00295                                                                   ECS066
00296      IF CO-ACCOUNT-TYPE                                           ECS066
00297        AND  CO-CARRY-BALANCE                                      ECS066
00298          MOVE 'ACCOUNT RECORD - BALANCE CARRIED'                  ECS066
00299                                  TO  R9-INFO (X1).                ECS066
00300                                                                   ECS066
00301      IF CO-ACCOUNT-TYPE                                           ECS066
00302        AND  CO-NO-BALANCE                                         ECS066
00303          MOVE 'ACCOUNT RECORD - GENERAL AGENT CARRIES BALANCE'    ECS066
00304                                  TO  R9-INFO (X1).                ECS066
00305                                                                   ECS066
00306      IF CO-COMPANY-TYPE                                           ECS066
00307          MOVE 'COMPANY RECORD'   TO  R9-INFO (X1).                ECS066
00308                                                                   ECS066
00309      IF R7-INFO (X1) = SPACES                                     ECS066
00310          MOVE R8-INFO (X1)       TO  R7-INFO (X1)                 ECS066
00311          MOVE SPACES             TO  R8-INFO (X1).                ECS066
00312                                                                   ECS066
00313      IF R8-INFO (X1) = SPACES                                     ECS066
00314          MOVE R9-INFO (X1)       TO  R8-INFO (X1)                 ECS066
00315          MOVE SPACES             TO  R9-INFO (X1).                ECS066
00316                                                                   ECS066
00317      GO TO 2000-RD-COMM.                                          ECS066
00318  EJECT                                                            ECS066
00319  5800-GEN-ROLX-RTN.                                               ECS066
00320      MOVE SPACE-NP               TO  P-CCSW.                      ECS066
00321      MOVE ROLX-1                 TO  P-LN.                        ECS066
00322                                                                   ECS066
00323      PERFORM 8800-PRT-RTN  THRU  8999-EXIT.                       ECS066
00324                                                                   ECS066
00325      MOVE SPACE-1                TO  P-CCSW.                      ECS066
00326      MOVE ROLX-2                 TO  P-LN.                        ECS066
00327                                                                   ECS066
00328      PERFORM 8800-PRT-RTN  THRU  8999-EXIT.                       ECS066
00329                                                                   ECS066
00330      MOVE SPACE-1                TO  P-CCSW.                      ECS066
00331      MOVE ROLX-3                 TO  P-LN.                        ECS066
00332                                                                   ECS066
00333      PERFORM 8800-PRT-RTN  THRU  8999-EXIT.                       ECS066
00334                                                                   ECS066
00335      MOVE SPACE-1                TO  P-CCSW.                      ECS066
00336      MOVE ROLX-4                 TO  P-LN.                        ECS066
00337                                                                   ECS066
00338      PERFORM 8800-PRT-RTN  THRU  8999-EXIT.                       ECS066
00339                                                                   ECS066
00340      MOVE SPACE-1                TO  P-CCSW.                      ECS066
00341      MOVE ROLX-5                 TO  P-LN.                        ECS066
00342                                                                   ECS066
00343      PERFORM 8800-PRT-RTN  THRU  8999-EXIT.                       ECS066
00344                                                                   ECS066
00345      MOVE SPACE-1                TO  P-CCSW.                      ECS066
00346      MOVE ROLX-6                 TO  P-LN.                        ECS066
00347                                                                   ECS066
00348      PERFORM 8800-PRT-RTN  THRU  8999-EXIT.                       ECS066
00349                                                                   ECS066
00350      MOVE SPACE-2                TO  P-CCSW.                      ECS066
00351      MOVE ROLX-7                 TO  P-LN.                        ECS066
00352                                                                   ECS066
00353      PERFORM 8800-PRT-RTN  THRU  8999-EXIT.                       ECS066
00354                                                                   ECS066
00355      MOVE SPACE-2                TO  P-CCSW.                      ECS066
00356      MOVE ROLX-8                 TO  P-LN.                        ECS066
00357                                                                   ECS066
00358      PERFORM 8800-PRT-RTN  THRU  8999-EXIT.                       ECS066
00359                                                                   ECS066
00360      MOVE SPACE-2                TO  P-CCSW.                      ECS066
00361      MOVE ROLX-9                 TO  P-LN.                        ECS066
00362                                                                   ECS066
00363      PERFORM 8800-PRT-RTN  THRU  8999-EXIT.                       ECS066
00364                                                                   ECS066
00365      ADD +1                      TO  ALIGN-CTR.                   ECS066
00366                                                                   ECS066
00367      IF ALIGN-CTR  IS LESS THAN  MAX-ALIGN-CTR                    ECS066
00368          GO TO 5800-GEN-ROLX-RTN.                                 ECS066
00369                                                                   ECS066
00370      MOVE SPACES                 TO  ROLODEX-AREA.                ECS066
00371      MOVE K1                     TO  X1.                          ECS066
00372                                                                   ECS066
00373  5999-EXIT.                                                       ECS066
00374      EXIT.                                                        ECS066
00375  EJECT                                                            ECS066
00376  6000-LEFT-RTN.                                                   ECS066
00377      IF CO-ACCT-NAME = SPACES                                     ECS066
00378          GO TO 6199-EXIT.                                         ECS066
00379                                                                   ECS066
00380      MOVE CO-ACCT-NAME           TO  A-NAME.                      ECS066
00381      MOVE SPACES                 TO  B-NAME.                      ECS066
00382      MOVE +1                     TO  A  B.                        ECS066
00383                                                                   ECS066
00384  6050-LOOP-A.                                                     ECS066
00385      IF A-CHAR (A) = SPACES                                       ECS066
00386          ADD +1                  TO  A                            ECS066
00387          GO TO 6050-LOOP-A.                                       ECS066
00388                                                                   ECS066
00389  6100-LOOP-B.                                                     ECS066
00390      MOVE A-CHAR (A)             TO  B-CHAR (B).                  ECS066
00391                                                                   ECS066
00392      ADD +1                      TO  A.                           ECS066
00393                                                                   ECS066
00394      IF A  IS GREATER THAN  +30                                   ECS066
00395          GO TO 6150-MOVE-NAME.                                    ECS066
00396                                                                   ECS066
00397      ADD +1                      TO  B.                           ECS066
00398                                                                   ECS066
00399      GO TO 6100-LOOP-B.                                           ECS066
00400                                                                   ECS066
00401  6150-MOVE-NAME.                                                  ECS066
00402      MOVE B-NAME                 TO  CO-ACCT-NAME.                ECS066
00403                                                                   ECS066
00404  6199-EXIT.                                                       ECS066
00405      EXIT.                                                        ECS066
00406  EJECT                                                            ECS066
00407  8500-DATE-CONVERSION.                                            ECS066
00408                              COPY ELCDCS.                         ECS066
00409                                                                   ECS066
00410  8599-EXIT.                                                       ECS066
00411      EXIT.                                                        ECS066
00412  EJECT                                                            ECS066
00413  8800-PRT-RTN.                                                    ECS066
00414      MOVE P-CCSW                 TO  X  P-CTL.                    ECS066
00415      MOVE P-LN                   TO  P-DATA.                      ECS066
00416      MOVE SPACE-1                TO  P-REC.                       ECS066
00417                                                                   ECS066
00418  8900-PRT-COPY.                                                   ECS066
00419                              COPY ELCPRT2.                        ECS066
00420                                                                   ECS066
00421  8999-EXIT.                                                       ECS066
00422      EXIT.                                                        ECS066
00423  EJECT                                                            ECS066
00424  9990-E-O-J.                                                      ECS066
00425      IF NBR-OF-CARDS = +0                                         ECS066
00426          GO TO 9995-CLOSE-FILES.                                  ECS066
00427                                                                   ECS066
00428      PERFORM 5800-GEN-ROLX-RTN  THRU  5999-EXIT.                  ECS066
00429 *                                                                 ECS066
00430 *                                                                 ECS066
00431 * LEAVE THE ABOVE 2 LINE IN TO PREVENT LOSING THE LINE ABOVE      ECS066
00439                                                                   ECS066
00440  9995-CLOSE-FILES.                                                ECS066
00441      CLOSE COMM-MSTR-IN                                           ECS066
00442            PRNTR.                                                 ECS066
00443                                                                   ECS066
00444  9996-CLOSE-FICH.                                                 ECS066
00445                              COPY ELCPRTC.                        ECS066
00446                                                                   ECS066
00447      GOBACK.                                                      ECS066
00448                                                                   ECS066
00449  ABEND-PGM SECTION.                                               ECS066
00450                              COPY ELCABEND.                       ECS066
