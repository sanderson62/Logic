00001  IDENTIFICATION DIVISION.                                         04/20/98
00002                                                                   ECS260
00003  PROGRAM-ID.                 ECS260.                                 LV003
00004 *               PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE         ECS260
00005 *                           VMOD=2.003                               CL**2
00006 *                                                                 ECS260
00007 *AUTHOR.        LOGIC, INC.                                       ECS260
00008 *               DALLAS, TEXAS.                                    ECS260
00009 *                                                                 ECS260
00010 *DATE-COMPILED.                                                   ECS260
00011                                                                      CL**3
00012 *SECURITY.   *****************************************************ECS260
00013 *            *                                                   *ECS260
00014 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ECS260
00015 *            *                                                   *ECS260
00016 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS260
00017 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ECS260
00018 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC, INC.     *ECS260
00019 *            *                                                   *ECS260
00020 *            *****************************************************ECS260
00021 *                                                                 ECS260
00022 *REMARKS.                                                         ECS260
00023 *        THIS PROGRAM IS USED TO BACK-UP YEAR-END FILES TO A      ECS260
00024 *        SINBLE TAPE FILE.  A PREVIOUS VERSION OF THIS PROGRAM    ECS260
00025 *        (SCSBKUP) IS USED FOR CLIENTS ON PRE 6.0 RELEASE.        ECS260
00026 *        PROGRAM ECS261 IS USED TO SPLIT THIS FILE INTO           ECS260
00027 *        INDIVIDUAL FILES.                                        ECS260
00028                                                                   ECS260
00029  ENVIRONMENT DIVISION.                                            ECS260
00030  INPUT-OUTPUT SECTION.                                            ECS260
00031  FILE-CONTROL.                                                    ECS260
00032                                                                   ECS260
00033      SELECT ECS-YE-BACKUP    ASSIGN TO SYS010-UT-2400-S-SYS010.   ECS260
00034      SELECT ACCOUNT-MSTR-TP  ASSIGN TO SYS011-UT-2400-S-SYS011.   ECS260
00035      SELECT CERT-FILE-TP     ASSIGN TO SYS012-UT-2400-S-SYS012.   ECS260
00036      SELECT REINS-TABLE-TP   ASSIGN TO SYS013-UT-2400-S-SYS013.   ECS260
00037      SELECT EPEC-TP          ASSIGN TO SYS014-UT-2400-S-SYS014.   ECS260
00038      SELECT COMP-TP          ASSIGN TO SYS015-UT-2400-S-SYS015.   ECS260
00039  EJECT                                                            ECS260
00040  DATA DIVISION.                                                   ECS260
00041  FILE SECTION.                                                    ECS260
00042                                                                   ECS260
00043  FD  ECS-YE-BACKUP                                                ECS260
00044      RECORDING MODE IS F                                          ECS260
00045      LABEL RECORDS ARE STANDARD                                   ECS260
00046      BLOCK CONTAINS 0 RECORDS
00047      RECORD CONTAINS 8206 CHARACTERS.                             ECS260
00048                                                                   ECS260
00049  01  DUMMY-REC.                                                   ECS260
00050      12  REC-ID              PIC  X(04).                          ECS260
00051      12  REC-CNT             PIC S9(03)      COMP-3.              ECS260
00052      12  FILLER              PIC  X(8200).                        ECS260
00053                                                                   ECS260
00054  01  B-ACCT-REC.                                                  ECS260
00055      12  FILLER              PIC  X(06).                          ECS260
00056      12  ACCOUNT-ENTRY       PIC  X(2000)    OCCURS  4  TIMES.    ECS260
00057      12  FILLER              PIC  X(200).                         ECS260
00058                                                                   ECS260
00059  01  B-CERT-REC.                                                  ECS260
00060      12  FILLER              PIC  X(06).                          ECS260
00061      12  CERT-ENTRY          PIC  X(700)     OCCURS  11  TIMES.   ECS260
00062      12  FILLER              PIC  X(500).                         ECS260
00063                                                                   ECS260
00064  01  B-COMP.                                                      ECS260
00065      12  FILLER              PIC  X(06).                          ECS260
00066      12  COMP-ENTRY          PIC  X(450)     OCCURS  18  TIMES.   ECS260
00067      12  FILLER              PIC  X(100).                         ECS260
00068                                                                   ECS260
00069  01  B-EPECS.                                                     ECS260
00070      12  FILLER              PIC  X(06).                          ECS260
00071      12  EPEC-ENTRY          PIC  X(325)     OCCURS  25  TIMES.   ECS260
00072      12  FILLER              PIC  X(75).                          ECS260
00073                                                                   ECS260
00074  01  B-REINS.                                                     ECS260
00075      12  FILLER              PIC  X(06).                          ECS260
00076      12  REINS-ENTRY         PIC  X(4000)    OCCURS  2  TIMES.    ECS260
00077      12  FILLER              PIC  X(200).                         ECS260
00078  EJECT                                                            ECS260
00079  FD  ACCOUNT-MSTR-TP                                              ECS260
00080                              COPY ECSFDAMD.                       ECS260
00081  EJECT                                                            ECS260
00082  FD  CERT-FILE-TP                                                 ECS260
00083                              COPY ECSCRIFD.                       ECS260
00084  EJECT                                                            ECS260
00085  FD  REINS-TABLE-TP                                               ECS260
00086                              COPY ECSRTFDD.                       ECS260
00087  EJECT                                                            ECS260
00088  FD  EPEC-TP                                                      ECS260
00089                              COPY ECSEPCFD.                       ECS260
00090                                                                   ECS260
00091  01  EP-RECORD               PIC  X(325).                         ECS260
00092  EJECT                                                            ECS260
00093  FD  COMP-TP                                                      ECS260
00094                              COPY ECSCOIFD.                       ECS260
00095  EJECT                                                            ECS260
00096  WORKING-STORAGE SECTION.                                         ECS260
00097  77  FILLER  PIC X(32) VALUE '********************************'.  ECS260
00098  77  FILLER  PIC X(32) VALUE '     ECS260 WORKING-STORAGE     '.  ECS260
00099  77  FILLER  PIC X(32) VALUE '*********VMOD=2.003*************'.     CL**2
00100  77  SUB1                    PIC S9(04)  COMP    VALUE +0.        ECS260
00101  77  EOF-SW                  PIC  9(01)          VALUE 0.         ECS260
00102      88  FILE-END                                VALUE 9.         ECS260
00103  77  C-ACCT                  PIC  9(07)          VALUE 0.         ECS260
00104  77  C-CERT                  PIC  9(07)          VALUE 0.         ECS260
00105  77  C-REIN                  PIC  9(07)          VALUE 0.         ECS260
00106  77  C-COMP                  PIC  9(07)          VALUE 0.         ECS260
00107  77  C-EPEC                  PIC  9(07)          VALUE 0.         ECS260
00108  EJECT                                                            ECS260
00109  PROCEDURE DIVISION.                                              ECS260
00110                                                                   ECS260
00111  0000-SELECT-LOAD-OPTIONS.                                        ECS260
00112      OPEN OUTPUT ECS-YE-BACKUP.                                   ECS260
00113                                                                   ECS260
00114      MOVE SPACES                 TO  DUMMY-REC.                   ECS260
00115                                                                   ECS260
00116      PERFORM 1000-ACCT-BACKUP  THRU  1999-EXIT.                   ECS260
00117                                                                   ECS260
00118      PERFORM 2000-CERT-BACKUP  THRU  2999-EXIT.                   ECS260
00119                                                                   ECS260
00120      PERFORM 3000-COMP-BACKUP  THRU  3999-EXIT.                   ECS260
00121                                                                   ECS260
00122      PERFORM 4000-EPEC-BACKUP  THRU  4999-EXIT.                   ECS260
00123                                                                   ECS260
00124      PERFORM 5000-REIN-BACKUP  THRU  5999-EXIT.                   ECS260
00125                                                                   ECS260
00126      DISPLAY 'FILE BACKUP COMPLETE'.                              ECS260
00127                                                                   ECS260
00128      CLOSE ECS-YE-BACKUP.                                         ECS260
00129                                                                   ECS260
00130  0999-JOB-END  SECTION.                                           ECS260
00131      GOBACK.                                                      ECS260
00132  EJECT                                                            ECS260
00133  1000-ACCT-BACKUP  SECTION.                                       ECS260
00134      OPEN INPUT ACCOUNT-MSTR-TP.                                  ECS260
00135                                                                   ECS260
00136  1100-READ-ACCT-LOOP.                                             ECS260
00137      MOVE SPACES                 TO  DUMMY-REC.                   ECS260
00138                                                                   ECS260
00139      PERFORM 1500-READ-ACCT                                       ECS260
00140          VARYING  SUB1  FROM  1  BY  1                            ECS260
00141              UNTIL  SUB1  IS GREATER THAN  6  OR  FILE-END.       ECS260
00142                                                                   ECS260
00143      MOVE 'ACCT'                 TO  REC-ID.                      ECS260
00144                                                                   ECS260
00145      SUBTRACT 1                  FROM  SUB1.                      ECS260
00146                                                                   ECS260
00147      MOVE SUB1                   TO  REC-CNT.                     ECS260
00148                                                                   ECS260
00149      WRITE DUMMY-REC.                                             ECS260
00150                                                                   ECS260
00151      IF FILE-END                                                  ECS260
00152          DISPLAY C-ACCT  ' ACCOUNT RECORDS PROCESSED'             ECS260
00153          CLOSE ACCOUNT-MSTR-TP                                    ECS260
00154          MOVE 0                  TO  EOF-SW                       ECS260
00155          GO TO 1999-EXIT                                          ECS260
00156      ELSE                                                         ECS260
00157          GO TO 1100-READ-ACCT-LOOP.                               ECS260
00158                                                                   ECS260
00159  1500-READ-ACCT.                                                  ECS260
00160      READ ACCOUNT-MSTR-TP                                         ECS260
00161          AT END                                                   ECS260
00162              MOVE 9              TO  EOF-SW.                      ECS260
00163                                                                   ECS260
00164      IF NOT FILE-END                                              ECS260
00165          MOVE CLS-AM-DK          TO  ACCOUNT-ENTRY (SUB1)         ECS260
00166          ADD 1                   TO  C-ACCT.                      ECS260
00167                                                                   ECS260
00168  1999-EXIT.                                                       ECS260
00169      EXIT.                                                        ECS260
00170  EJECT                                                            ECS260
00171  2000-CERT-BACKUP  SECTION.                                       ECS260
00172      OPEN INPUT CERT-FILE-TP.                                     ECS260
00173                                                                   ECS260
00174  2100-READ-CERT-LOOP.                                             ECS260
00175      MOVE SPACES                 TO  DUMMY-REC.                   ECS260
00176                                                                   ECS260
00177      PERFORM 2500-READ-CERT                                       ECS260
00178          VARYING  SUB1  FROM  1  BY  1                            ECS260
00179              UNTIL  SUB1  IS GREATER THAN  21  OR  FILE-END.      ECS260
00180                                                                   ECS260
00181      MOVE 'CERT'                 TO  REC-ID.                      ECS260
00182                                                                   ECS260
00183      SUBTRACT 1                  FROM  SUB1.                      ECS260
00184                                                                   ECS260
00185      MOVE SUB1                   TO  REC-CNT.                     ECS260
00186                                                                   ECS260
00187      WRITE DUMMY-REC.                                             ECS260
00188                                                                   ECS260
00189      IF FILE-END                                                  ECS260
00190          DISPLAY C-CERT, ' CERT RECORDS PROCESSED'                ECS260
00191          CLOSE CERT-FILE-TP                                       ECS260
00192          MOVE 0                  TO  EOF-SW                       ECS260
00193          GO TO 2999-EXIT                                          ECS260
00194      ELSE                                                         ECS260
00195          GO TO 2100-READ-CERT-LOOP.                               ECS260
00196                                                                   ECS260
00197  2500-READ-CERT.                                                  ECS260
00198      READ CERT-FILE-TP                                            ECS260
00199          AT END                                                   ECS260
00200              MOVE 9              TO  EOF-SW.                      ECS260
00201                                                                   ECS260
00202      IF NOT FILE-END                                              ECS260
00203          MOVE CERT-IN-RECORD     TO  CERT-ENTRY (SUB1)            ECS260
00204          ADD 1                   TO  C-CERT.                      ECS260
00205  2999-EXIT.                                                       ECS260
00206      EXIT.                                                        ECS260
00207  EJECT                                                            ECS260
00208  3000-COMP-BACKUP  SECTION.                                       ECS260
00209      OPEN INPUT COMP-TP.                                          ECS260
00210                                                                   ECS260
00211  3100-READ-COMP-LOOP.                                             ECS260
00212      MOVE SPACES                 TO  DUMMY-REC.                   ECS260
00213                                                                   ECS260
00214      PERFORM 3500-READ-COMP                                       ECS260
00215          VARYING  SUB1  FROM  1  BY  1                            ECS260
00216              UNTIL  SUB1  IS GREATER THAN  28  OR  FILE-END.      ECS260
00217                                                                   ECS260
00218      MOVE 'COMP'                 TO  REC-ID.                      ECS260
00219                                                                   ECS260
00220      SUBTRACT 1                  FROM  SUB1.                      ECS260
00221                                                                   ECS260
00222      MOVE SUB1                   TO  REC-CNT.                     ECS260
00223                                                                   ECS260
00224      WRITE DUMMY-REC.                                             ECS260
00225                                                                   ECS260
00226      IF FILE-END                                                  ECS260
00227          DISPLAY C-COMP, ' COMP RECORDS PROCESSED'                ECS260
00228          CLOSE COMP-TP                                            ECS260
00229          MOVE 0                  TO  EOF-SW                       ECS260
00230          GO TO 3999-EXIT                                          ECS260
00231      ELSE                                                         ECS260
00232          GO TO 3100-READ-COMP-LOOP.                               ECS260
00233                                                                   ECS260
00234  3500-READ-COMP.                                                  ECS260
00235      READ COMP-TP                                                 ECS260
00236          AT END                                                   ECS260
00237              MOVE 9              TO  EOF-SW.                      ECS260
00238                                                                   ECS260
00239      IF NOT FILE-END                                              ECS260
00240          MOVE COMP-IN-RECORD     TO  COMP-ENTRY (SUB1)            ECS260
00241          ADD 1                   TO  C-COMP.                      ECS260
00242                                                                   ECS260
00243  3999-EXIT.                                                       ECS260
00244      EXIT.                                                        ECS260
00245  EJECT                                                            ECS260
00246  4000-EPEC-BACKUP  SECTION.                                       ECS260
00247      OPEN INPUT EPEC-TP.                                          ECS260
00248                                                                   ECS260
00249  4100-READ-EPEC-LOOP.                                             ECS260
00250      MOVE SPACES                 TO  DUMMY-REC.                   ECS260
00251                                                                   ECS260
00252      PERFORM 4500-READ-EPEC                                       ECS260
00253          VARYING  SUB1  FROM  1  BY  1                            ECS260
00254              UNTIL  SUB1  IS GREATER THAN  27  OR  FILE-END.      ECS260
00255                                                                   ECS260
00256      MOVE 'EPEC'                 TO  REC-ID.                      ECS260
00257                                                                   ECS260
00258      SUBTRACT 1                  FROM  SUB1.                      ECS260
00259                                                                   ECS260
00260      MOVE SUB1                   TO  REC-CNT.                     ECS260
00261                                                                   ECS260
00262      WRITE DUMMY-REC.                                             ECS260
00263                                                                   ECS260
00264      IF FILE-END                                                  ECS260
00265          DISPLAY C-EPEC, ' EPEC RECORDS PROCESSED'                ECS260
00266          CLOSE EPEC-TP                                            ECS260
00267          MOVE 0                  TO  EOF-SW                       ECS260
00268          GO TO 4999-EXIT                                          ECS260
00269      ELSE                                                         ECS260
00270          GO TO 4100-READ-EPEC-LOOP.                               ECS260
00271                                                                   ECS260
00272  4500-READ-EPEC.                                                  ECS260
00273      READ EPEC-TP                                                 ECS260
00274          AT END                                                   ECS260
00275              MOVE 9              TO  EOF-SW.                      ECS260
00276                                                                   ECS260
00277      IF NOT FILE-END                                              ECS260
00278          MOVE EP-RECORD          TO  EPEC-ENTRY (SUB1)            ECS260
00279          ADD 1                   TO  C-EPEC.                      ECS260
00280                                                                   ECS260
00281  4999-EXIT.                                                       ECS260
00282      EXIT.                                                        ECS260
00283  EJECT                                                            ECS260
00284  5000-REIN-BACKUP  SECTION.                                       ECS260
00285      OPEN INPUT REINS-TABLE-TP.                                   ECS260
00286                                                                   ECS260
00287  5100-READ-REIN-LOOP.                                             ECS260
00288      MOVE SPACES                 TO  DUMMY-REC.                   ECS260
00289                                                                   ECS260
00290      PERFORM 5500-READ-REIN                                       ECS260
00291          VARYING  SUB1  FROM  1  BY  1                            ECS260
00292              UNTIL  SUB1  IS GREATER THAN  4  OR  FILE-END.       ECS260
00293                                                                   ECS260
00294      MOVE 'REIN'                 TO  REC-ID.                      ECS260
00295                                                                   ECS260
00296      SUBTRACT 1                  FROM  SUB1.                      ECS260
00297                                                                   ECS260
00298      MOVE SUB1                   TO  REC-CNT.                     ECS260
00299                                                                   ECS260
00300      WRITE DUMMY-REC.                                             ECS260
00301                                                                   ECS260
00302      IF FILE-END                                                  ECS260
00303          DISPLAY C-REIN, ' REINS RECORDS PROCESSED'               ECS260
00304          CLOSE REINS-TABLE-TP                                     ECS260
00305          MOVE 0                  TO  EOF-SW                       ECS260
00306          GO TO 5999-EXIT                                          ECS260
00307      ELSE                                                         ECS260
00308          GO TO 5100-READ-REIN-LOOP.                               ECS260
00309                                                                   ECS260
00310  5500-READ-REIN.                                                  ECS260
00311      READ REINS-TABLE-TP                                          ECS260
00312          AT END                                                   ECS260
00313              MOVE 9              TO  EOF-SW.                      ECS260
00314                                                                   ECS260
00315      IF NOT FILE-END                                              ECS260
00316          MOVE RT-REC             TO  REINS-ENTRY (SUB1)           ECS260
00317          ADD 1                   TO  C-REIN.                      ECS260
00318                                                                   ECS260
00319  5999-EXIT.                                                       ECS260
00320      EXIT.                                                        ECS260
