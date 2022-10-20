00001  IDENTIFICATION DIVISION.                                         10/08/97
00002                                                                   ECS261
00003  PROGRAM-ID.                 ECS261.                                 LV002
00004 *           PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE             ECS261
00005 *                            VMOD=2.003.                             CL**2
00006 *                                                                 ECS261
00007 *AUTHOR.        LOGIC, INC.                                       ECS261
00008 *               DALLAS, TEXAS.                                    ECS261
00009                                                                   ECS261
00010 *DATE-COMPILED.                                                   ECS261
00011 *                                                                 ECS261
00012 *SECURITY.   *****************************************************ECS261
00013 *            *                                                   *ECS261
00014 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ECS261
00015 *            *                                                   *ECS261
00016 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS261
00017 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ECS261
00018 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC, INC.     *ECS261
00019 *            *                                                   *ECS261
00020 *            *****************************************************ECS261
00021 *                                                                 ECS261
00022 *REMARKS.                                                         ECS261
00023 *        THIS PROGRAM SPLITS THE ECS YEAR END BACK-UP FILE BACK   ECS261
00024 *        INTO THE INDIVIDUAL FILES. COMBINED FILE IS CREATED BY   ECS261
00025 *        PROGRAM ECS260.                                          ECS261
00026                                                                   ECS261
00027  ENVIRONMENT DIVISION.                                            ECS261
00028  INPUT-OUTPUT SECTION.                                            ECS261
00029  FILE-CONTROL.                                                    ECS261
00030                                                                   ECS261
00031      SELECT ECS-YE-BACKUP    ASSIGN TO SYS010-UT-2400-S.          ECS261
00032      SELECT ACCOUNT-MSTR-TP  ASSIGN TO SYS011-UT-2400-S.          ECS261
00033      SELECT CERT-FILE-TP     ASSIGN TO SYS012-UT-2400-S.          ECS261
00034      SELECT REINS-TABLE-TP   ASSIGN TO SYS013-UT-2400-S.          ECS261
00035      SELECT EPEC-TP          ASSIGN TO SYS014-UT-2400-S.          ECS261
00036      SELECT COMP-TP          ASSIGN TO SYS015-UT-2400-S.          ECS261
00037  EJECT                                                            ECS261
00038  DATA DIVISION.                                                   ECS261
00039  FILE SECTION.                                                    ECS261
00040                                                                   ECS261
00041  FD  ECS-YE-BACKUP                                                ECS261
00042      RECORDING MODE F                                             ECS261
00043      LABEL RECORDS STANDARD                                       ECS261
00044      BLOCK CONTAINS 0 RECORDS
00045      RECORD CONTAINS 8206 CHARACTERS.                             ECS261
00046  01  DUMMY-REC.                                                   ECS261
00047      12  REC-ID              PIC  X(4).                           ECS261
00048      12  REC-CNT             PIC S999       COMP-3.               ECS261
00049      12  FILLER              PIC  X(8200).                        ECS261
00050                                                                   ECS261
00051  01  B-ACCT-REC.                                                  ECS261
00052      12  FILLER              PIC  X(6).                           ECS261
00053      12  ACCOUNT-ENTRY       PIC  X(2000)    OCCURS  4  TIMES.    ECS261
00054      12  FILLER              PIC  X(200).                         ECS261
00055                                                                   ECS261
00056  01  B-CERT-REC.                                                  ECS261
00057      12  FILLER              PIC  X(6).                           ECS261
00058      12  CERT-ENTRY          PIC  X(700)     OCCURS  11  TIMES.   ECS261
00059      12  FILLER              PIC  X(500).                         ECS261
00060                                                                   ECS261
00061  01  B-COMP.                                                      ECS261
00062      12  FILLER              PIC  X(6).                           ECS261
00063      12  COMP-ENTRY          PIC  X(450)     OCCURS  18  TIMES.   ECS261
00064      12  FILLER              PIC  X(100).                         ECS261
00065                                                                   ECS261
00066  01  B-EPECS.                                                     ECS261
00067      12  FILLER              PIC  X(6).                           ECS261
00068      12  EPEC-ENTRY          PIC  X(325)     OCCURS  25  TIMES.   ECS261
00069      12  FILLER              PIC  X(75).                          ECS261
00070                                                                   ECS261
00071  01  B-REINS.                                                     ECS261
00072      12  FILLER              PIC  X(6).                           ECS261
00073      12  REINS-ENTRY         PIC  X(4000)    OCCURS  2  TIMES.    ECS261
00074      12  FILLER              PIC  X(200).                         ECS261
00075  EJECT                                                            ECS261
00076  FD  ACCOUNT-MSTR-TP                                              ECS261
00077                              COPY ECSFDAMD.                       ECS261
00078  EJECT                                                            ECS261
00079  FD  CERT-FILE-TP                                                 ECS261
00080                              COPY ECSCRIFD.                       ECS261
00081  EJECT                                                            ECS261
00082  FD  REINS-TABLE-TP                                               ECS261
00083                              COPY ECSRTFDD.                       ECS261
00084  EJECT                                                            ECS261
00085  FD  EPEC-TP                                                      ECS261
00086                              COPY ECSEPCFD.                       ECS261
00087                                                                   ECS261
00088  01  EP-RECORD               PIC  X(325).                         ECS261
00089  EJECT                                                            ECS261
00090  FD  COMP-TP                                                      ECS261
00091                              COPY ECSCOIFD.                       ECS261
00092  EJECT                                                            ECS261
00093  WORKING-STORAGE SECTION.                                         ECS261
00094  77  FILLER  PIC X(32) VALUE '********************************'.  ECS261
00095  77  FILLER  PIC X(32) VALUE '     ECS261 WORKING-STORAGE     '.  ECS261
00096  77  FILLER  PIC X(32) VALUE '*********VMOD=2.003*************'.     CL**2
00097                                                                   ECS261
00098  77  SUB1                    PIC S9(4)   COMP    VALUE +0.        ECS261
00099  77  EOF-SW                  PIC  9              VALUE 0.         ECS261
00100      88  FILE-END                                VALUE 9.         ECS261
00101  77  C-ACCT                  PIC  9(7)           VALUE 0.         ECS261
00102  77  C-CERT                  PIC  9(7)           VALUE 0.         ECS261
00103  77  C-REIN                  PIC  9(7)           VALUE 0.         ECS261
00104  77  C-COMP                  PIC  9(7)           VALUE 0.         ECS261
00105  77  C-EPEC                  PIC  9(7)           VALUE 0.         ECS261
00106                                                                   ECS261
00107  01  WS.                                                          ECS261
00108     12  WS-RETURN-CODE       PIC S9(4)  COMP     VALUE +0.        ECS261
00109     12  WS-ABEND-MESSAGE     PIC X(80)           VALUE SPACES.    ECS261
00110     12  WS-ABEND-FILE-STATUS PIC XX              VALUE ZEROS.     ECS261
00111     12  WS-ZERO              PIC S9              VALUE +0.        ECS261
00112  EJECT                                                            ECS261
00113  PROCEDURE DIVISION.                                              ECS261
00114                                                                   ECS261
00115  0000-SELECT-LOAD-OPTIONS.                                        ECS261
00116      OPEN INPUT ECS-YE-BACKUP.                                    ECS261
00117                                                                   ECS261
00118      PERFORM 1000-ACCOUNT-RESTORE  THRU  1999-EXIT.               ECS261
00119                                                                   ECS261
00120      PERFORM 2000-CERT-RESTORE  THRU  2999-EXIT.                  ECS261
00121                                                                   ECS261
00122      PERFORM 3000-COMP-RESTORE  THRU  3999-EXIT.                  ECS261
00123                                                                   ECS261
00124      PERFORM 4000-EPEC-RESTORE  THRU  4999-EXIT.                  ECS261
00125                                                                   ECS261
00126      PERFORM 5000-REIN-RESTORE  THRU  5999-EXIT.                  ECS261
00127                                                                   ECS261
00128      DISPLAY 'FILE RESTORE COMPLETE'.                             ECS261
00129                                                                   ECS261
00130      CLOSE ECS-YE-BACKUP.                                         ECS261
00131                                                                   ECS261
00132  0999-JOB-END  SECTION.                                           ECS261
00133      GOBACK.                                                      ECS261
00134                                                                   ECS261
00135  1000-ACCOUNT-RESTORE  SECTION.                                   ECS261
00136      OPEN OUTPUT ACCOUNT-MSTR-TP.                                 ECS261
00137                                                                   ECS261
00138      READ ECS-YE-BACKUP                                           ECS261
00139          AT END                                                   ECS261
00140              MOVE +701                TO WS-RETURN-CODE           ECS261
00141              MOVE 'EOF ECS-YE-BACKUP' TO WS-ABEND-MESSAGE         ECS261
00142              GO TO ABEND-PGM.                                     ECS261
00143                                                                   ECS261
00144  1100-READ-ACCT-LOOP.                                             ECS261
00145      PERFORM 1500-WRITE-ACCT  THRU  1599-EXIT                     ECS261
00146          VARYING  SUB1  FROM  1  BY  1                            ECS261
00147              UNTIL  SUB1  IS GREATER THAN  REC-CNT.               ECS261
00148                                                                   ECS261
00149      READ ECS-YE-BACKUP                                           ECS261
00150          AT END                                                   ECS261
00151              MOVE +701                TO WS-RETURN-CODE           ECS261
00152              MOVE 'EOF ECS-YE-BACKUP' TO WS-ABEND-MESSAGE         ECS261
00153              GO TO ABEND-PGM.                                     ECS261
00154                                                                   ECS261
00155      IF REC-ID NOT = 'ACCT'                                       ECS261
00156          SUBTRACT 1               FROM  C-ACCT                    ECS261
00157          DISPLAY C-ACCT  ' ACCOUNT RECORDS PROCESSED'             ECS261
00158          CLOSE ACCOUNT-MSTR-TP                                    ECS261
00159          MOVE 0                   TO  EOF-SW                      ECS261
00160          GO TO 1999-EXIT                                          ECS261
00161      ELSE                                                         ECS261
00162          GO TO 1100-READ-ACCT-LOOP.                               ECS261
00163                                                                   ECS261
00164  1500-WRITE-ACCT.                                                 ECS261
00165      MOVE ACCOUNT-ENTRY (SUB1)    TO  CLS-AM-DK.                  ECS261
00166                                                                   ECS261
00167      IF CLS-AM-DK = SPACE                                         ECS261
00168          GO TO 1599-EXIT.                                         ECS261
00169                                                                   ECS261
00170      WRITE CLS-AM-DK.                                             ECS261
00171                                                                   ECS261
00172      ADD 1                       TO  C-ACCT.                      ECS261
00173                                                                   ECS261
00174  1599-EXIT.                                                       ECS261
00175      EXIT.                                                        ECS261
00176                                                                   ECS261
00177  1999-EXIT.                                                       ECS261
00178      EXIT.                                                        ECS261
00179  EJECT                                                            ECS261
00180  2000-CERT-RESTORE  SECTION.                                      ECS261
00181      OPEN OUTPUT CERT-FILE-TP.                                    ECS261
00182                                                                   ECS261
00183  2100-READ-CERT-LOOP.                                             ECS261
00184      PERFORM 2500-WRITE-CERT  THRU  2599-EXIT                     ECS261
00185          VARYING  SUB1  FROM  1  BY  1                            ECS261
00186              UNTIL  SUB1  IS GREATER THAN  REC-CNT.               ECS261
00187                                                                   ECS261
00188      READ ECS-YE-BACKUP                                           ECS261
00189          AT END                                                   ECS261
00190              MOVE +701                TO WS-RETURN-CODE           ECS261
00191              MOVE 'EOF ECS-YE-BACKUP' TO WS-ABEND-MESSAGE         ECS261
00192              GO TO ABEND-PGM.                                     ECS261
00193                                                                   ECS261
00194      IF REC-ID NOT = 'CERT'                                       ECS261
00195          SUBTRACT 1              FROM  C-CERT                     ECS261
00196          DISPLAY C-CERT  ' CERT RECORDS PROCESSED'                ECS261
00197          CLOSE CERT-FILE-TP                                       ECS261
00198          MOVE 0                  TO  EOF-SW                       ECS261
00199          GO TO 2999-EXIT                                          ECS261
00200      ELSE                                                         ECS261
00201          GO TO 2100-READ-CERT-LOOP.                               ECS261
00202                                                                   ECS261
00203  2500-WRITE-CERT.                                                 ECS261
00204      MOVE CERT-ENTRY (SUB1)      TO  CERT-IN-RECORD.              ECS261
00205                                                                   ECS261
00206      IF CERT-IN-RECORD = SPACE                                    ECS261
00207          GO TO 2599-EXIT.                                         ECS261
00208                                                                   ECS261
00209      WRITE CERT-IN-RECORD.                                        ECS261
00210                                                                   ECS261
00211      ADD 1                       TO  C-CERT.                      ECS261
00212                                                                   ECS261
00213  2599-EXIT.                                                       ECS261
00214      EXIT.                                                        ECS261
00215                                                                   ECS261
00216  2999-EXIT.                                                       ECS261
00217      EXIT.                                                        ECS261
00218  EJECT                                                            ECS261
00219  3000-COMP-RESTORE  SECTION.                                      ECS261
00220      OPEN OUTPUT COMP-TP.                                         ECS261
00221                                                                   ECS261
00222  3100-READ-COMP-LOOP.                                             ECS261
00223      PERFORM 3500-WRITE-COMP  THRU  3599-EXIT                     ECS261
00224          VARYING  SUB1  FROM  1  BY  1                            ECS261
00225              UNTIL  SUB1  IS GREATER THAN  REC-CNT.               ECS261
00226                                                                   ECS261
00227      READ ECS-YE-BACKUP                                           ECS261
00228          AT END                                                   ECS261
00229              MOVE +701                TO WS-RETURN-CODE           ECS261
00230              MOVE 'EOF ECS-YE-BACKUP' TO WS-ABEND-MESSAGE         ECS261
00231              GO TO ABEND-PGM.                                     ECS261
00232                                                                   ECS261
00233      IF REC-ID NOT = 'COMP'                                       ECS261
00234          SUBTRACT 1              FROM  C-COMP                     ECS261
00235          DISPLAY C-COMP  ' COMP RECORDS PROCESSED'                ECS261
00236          CLOSE COMP-TP                                            ECS261
00237          MOVE 0                  TO  EOF-SW                       ECS261
00238          GO TO 3999-EXIT                                          ECS261
00239      ELSE                                                         ECS261
00240          GO TO 3100-READ-COMP-LOOP.                               ECS261
00241                                                                   ECS261
00242  3500-WRITE-COMP.                                                 ECS261
00243      MOVE COMP-ENTRY (SUB1)      TO  COMP-IN-RECORD.              ECS261
00244                                                                   ECS261
00245      IF COMP-IN-RECORD = SPACE                                    ECS261
00246          GO TO 3599-EXIT.                                         ECS261
00247                                                                   ECS261
00248      WRITE COMP-IN-RECORD.                                        ECS261
00249                                                                   ECS261
00250      ADD 1                       TO  C-COMP.                      ECS261
00251                                                                   ECS261
00252  3599-EXIT.                                                       ECS261
00253      EXIT.                                                        ECS261
00254                                                                   ECS261
00255  3999-EXIT.                                                       ECS261
00256      EXIT.                                                        ECS261
00257  EJECT                                                            ECS261
00258  4000-EPEC-RESTORE  SECTION.                                      ECS261
00259      OPEN OUTPUT EPEC-TP.                                         ECS261
00260                                                                   ECS261
00261  4100-READ-EPEC-LOOP.                                             ECS261
00262      PERFORM 4500-WRITE-EPEC  THRU  4599-EXIT                     ECS261
00263          VARYING  SUB1  FROM  1  BY  1                            ECS261
00264              UNTIL  SUB1  IS GREATER THAN  REC-CNT.               ECS261
00265                                                                   ECS261
00266      READ ECS-YE-BACKUP                                           ECS261
00267          AT END                                                   ECS261
00268              MOVE +701                TO WS-RETURN-CODE           ECS261
00269              MOVE 'EOF ECS-YE-BACKUP' TO WS-ABEND-MESSAGE         ECS261
00270              GO TO ABEND-PGM.                                     ECS261
00271                                                                   ECS261
00272      IF REC-ID NOT = 'EPEC'                                       ECS261
00273          SUBTRACT 1              FROM  C-EPEC                     ECS261
00274          DISPLAY C-EPEC  ' EPEC RECORDS PROCESSED'                ECS261
00275          CLOSE EPEC-TP                                            ECS261
00276          MOVE 0                  TO  EOF-SW                       ECS261
00277          GO TO 4999-EXIT                                          ECS261
00278      ELSE                                                         ECS261
00279          GO TO 4100-READ-EPEC-LOOP.                               ECS261
00280                                                                   ECS261
00281  4500-WRITE-EPEC.                                                 ECS261
00282      MOVE EPEC-ENTRY (SUB1)      TO  EP-RECORD.                   ECS261
00283                                                                   ECS261
00284      IF EP-RECORD = SPACE                                         ECS261
00285          GO TO 4599-EXIT.                                         ECS261
00286                                                                   ECS261
00287      WRITE EP-RECORD.                                             ECS261
00288                                                                   ECS261
00289      ADD 1                       TO  C-EPEC.                      ECS261
00290                                                                   ECS261
00291  4599-EXIT.                                                       ECS261
00292      EXIT.                                                        ECS261
00293                                                                   ECS261
00294  4999-EXIT.                                                       ECS261
00295      EXIT.                                                        ECS261
00296  EJECT                                                            ECS261
00297  5000-REIN-RESTORE  SECTION.                                      ECS261
00298      OPEN OUTPUT REINS-TABLE-TP.                                  ECS261
00299                                                                   ECS261
00300  5100-READ-REIN-LOOP.                                             ECS261
00301      PERFORM 5500-WRITE-REIN  THRU  5599-EXIT                     ECS261
00302          VARYING  SUB1  FROM  1  BY  1                            ECS261
00303              UNTIL  SUB1  IS GREATER THAN  REC-CNT.               ECS261
00304                                                                   ECS261
00305      READ ECS-YE-BACKUP                                           ECS261
00306          AT END                                                   ECS261
00307              SUBTRACT 1          FROM  C-REIN                     ECS261
00308              DISPLAY C-REIN  ' REINS RECORDS PROCESSED'           ECS261
00309              CLOSE REINS-TABLE-TP                                 ECS261
00310              MOVE 0              TO  EOF-SW                       ECS261
00311              GO TO 5999-EXIT.                                     ECS261
00312                                                                   ECS261
00313      GO TO 5100-READ-REIN-LOOP.                                   ECS261
00314                                                                   ECS261
00315  5500-WRITE-REIN.                                                 ECS261
00316      MOVE REINS-ENTRY (SUB1)     TO  RT-REC.                      ECS261
00317                                                                   ECS261
00318      IF RT-REC = SPACE                                            ECS261
00319          GO TO 5599-EXIT.                                         ECS261
00320                                                                   ECS261
00321      WRITE RT-REC.                                                ECS261
00322                                                                   ECS261
00323      ADD 1                       TO  C-REIN.                      ECS261
00324                                                                   ECS261
00325  5599-EXIT.                                                       ECS261
00326      EXIT.                                                        ECS261
00327                                                                   ECS261
00328  5999-EXIT.                                                       ECS261
00329      EXIT.                                                        ECS261
00330                                                                   ECS261
00331  ABEND-PGM   SECTION.                                             ECS261
00332                           COPY ELCABEND.                          ECS261
