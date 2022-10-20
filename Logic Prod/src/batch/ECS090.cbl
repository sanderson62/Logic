00001  IDENTIFICATION DIVISION.                                         03/19/98
00002                                                                   ECS090
00003  PROGRAM-ID.                 ECS090.                                 LV003
00004 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          ECS090
00005 *                            VMOD=2.010.                             CL**2
00006 *                                                                 ECS090
00007 *AUTHOR.        LOGIC, INC.                                       ECS090
00008 *               DALLAS, TEXAS.                                    ECS090
00009                                                                   ECS090
00010 *DATE-COMPILED.                                                   ECS090
00011                                                                   ECS090
00012 *SECURITY.   *****************************************************ECS090
00013 *            *                                                   *ECS090
00014 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ECS090
00015 *            *                                                   *ECS090
00016 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS090
00017 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ECS090
00018 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC, INC.     *ECS090
00019 *            *                                                   *ECS090
00020 *            *****************************************************ECS090
00021                                                                   ECS090
00022 *REMARKS.                                                         ECS090
00023 *        THIS PROGRAM CREATES A BACKUP TAPE FILE WITH MULTI-      ECS090
00024 *        FILES AFTER ECS010 HAS RUN. THE FILES ARE  DATE FILE,    ECS090
00025 *        ACCOUNT MASTER, EXTRACT FILE, EPEC FILE, REINSURANCE     ECS090
00026 *        TABLES, COMPENSATION TABLES, EPEC EXTRACTS, AND          ECS090
00027 *        COMPENSATION MASTER.                                     ECS090
00028 *                                                                 ECS090
00029 *        IN THE CASE OF CLASIC-CREDIT CLIENTS, THE EXTRACT FILE   ECS090
00030 *        FROM EL521 IS BACKED UP INSTEAD OF THE PENDING FILE.     ECS090
00031                                                                   ECS090
00032  EJECT                                                            ECS090
00033  ENVIRONMENT DIVISION.                                            ECS090
00034  INPUT-OUTPUT SECTION.                                            ECS090
00035  FILE-CONTROL.                                                    ECS090
00036                                                                   ECS090
00037      SELECT DISK-DATE        ASSIGN TO SYS019-UT-FBA1-S-SYS019.   ECS090
00038      SELECT ERACCTT          ASSIGN TO SYS010-FBA1-ERACCTT        ECS090
00039                              ACCESS IS SEQUENTIAL                 ECS090
00040                              ORGANIZATION IS INDEXED              ECS090
00041                              FILE STATUS IS ERACCTT-FILE-STATUS   ECS090
00042                              RECORD KEY IS AM-CONTROL-PRIMARY.    ECS090
00043      SELECT EPECEXTR         ASSIGN TO SYS018-UT-2400-S-SYS018.   ECS090
00044      SELECT DETEXTR          ASSIGN TO SYS012-UT-2400-S-SYS012.   ECS090
00045      SELECT REIN-FILE        ASSIGN TO SYS014-FBA1-REFILE         ECS090
00046                              ACCESS IS SEQUENTIAL                 ECS090
00047                              ORGANIZATION IS INDEXED              ECS090
00048                              FILE STATUS IS REIN-FILE-STATUS      ECS090
00049                              RECORD KEY IS REIN-KEY.              ECS090
00050      SELECT CTBL-FILE        ASSIGN TO SYS016-FBA1-CTFILE         ECS090
00051                              ACCESS IS SEQUENTIAL                 ECS090
00052                              ORGANIZATION IS INDEXED              ECS090
00053                              FILE STATUS IS COMM-FILE-STATUS      ECS090
00054                              RECORD KEY IS COMM-KEY.              ECS090
00055      SELECT CRED-FILE        ASSIGN TO SYS011-UT-2400-S-SYS011.   ECS090
00056      SELECT COMP-FILE        ASSIGN TO SYS015-UT-2400-S-SYS015.   ECS090
00057                                                                   ECS090
00058      SELECT PRNT-FILE        ASSIGN TO SYS008-UR-1403-S-SYS008.   ECS090
00059      SELECT BKUP-DATE        ASSIGN TO SYS013-UT-2400-S-SYS013.   ECS090
00060      SELECT BKUP-ACCT        ASSIGN TO SYS013-UT-2400-S-SYS013.   ECS090
00061      SELECT BKUP-EXTR        ASSIGN TO SYS013-UT-2400-S-SYS013.   ECS090
00062      SELECT BKUP-EPEC        ASSIGN TO SYS013-UT-2400-S-SYS013.   ECS090
00063      SELECT BKUP-REIN        ASSIGN TO SYS013-UT-2400-S-SYS013.   ECS090
00064      SELECT BKUP-CTBL        ASSIGN TO SYS013-UT-2400-S-SYS013.   ECS090
00065      SELECT BKUP-CRED        ASSIGN TO SYS013-UT-2400-S-SYS013.   ECS090
00066      SELECT BKUP-COMP        ASSIGN TO SYS013-UT-2400-S-SYS013.   ECS090
00067  EJECT                                                            ECS090
00068  DATA DIVISION.                                                   ECS090
00069  FILE SECTION.                                                    ECS090
00070                                                                   ECS090
00071  FD  PRNT-FILE                                                    ECS090
00072      RECORDING MODE F                                             ECS090
00073      LABEL RECORDS OMITTED                                        ECS090
00074      RECORD CONTAINS 133 CHARACTERS.                              ECS090
00075  01  PRT-REC.                                                     ECS090
00076      12  PRT-LINE.                                                ECS090
00077          16  PRT-C           PIC X.                               ECS090
00078          16  PRT-RST         PIC X(132).                          ECS090
00079                                                                   ECS090
00080  FD  DISK-DATE                                                    ECS090
00081                              COPY ELCDTEFD.                       ECS090
00082  EJECT                                                            ECS090
00083  FD  BKUP-DATE                                                    ECS090
00084      RECORDING MODE F                                             ECS090
00085      LABEL RECORDS STANDARD                                       ECS090
00086      BLOCK CONTAINS 0 RECORDS
00087      RECORD CONTAINS 100 CHARACTERS.                              ECS090
00088  01  BK-DATE.                                                     ECS090
00089      12  FILLER              PIC X(100).                          ECS090
00090                                                                   ECS090
00091  FD  ERACCTT                                                      ECS090
00092      LABEL RECORDS ARE STANDARD                                   ECS090
00093      BLOCK CONTAINS 0 RECORDS
00094      RECORD CONTAINS 2000 CHARACTERS.                             ECS090
00095  01  ACCT-REC.                                                    ECS090
00096      12  FILLER              PIC XX.                              ECS090
00097      12  AM-CONTROL-PRIMARY  PIC X(26).                           ECS090
00098      12  FILLER              PIC X(1972).                         ECS090
00099                                                                   ECS090
00100  FD  BKUP-ACCT                                                    ECS090
00101      RECORDING MODE  F                                            ECS090
00102      LABEL RECORDS STANDARD                                       ECS090
00103      BLOCK CONTAINS 0 RECORDS
00104      RECORD CONTAINS 2000 CHARACTERS.                             ECS090
00105  01  BK-ACCT.                                                     ECS090
00106      12  FILLER              PIC X(2000).                         ECS090
00107                                                                   ECS090
00108  FD  EPECEXTR                 COPY ECSEPCFD.                      ECS090
00109                                                                   ECS090
00110  01  EXTR-REC.                                                    ECS090
00111      12  FILLER              PIC X(325).                          ECS090
00112                                                                   ECS090
00113  FD  BKUP-EXTR                                                    ECS090
00114      RECORDING MODE F                                             ECS090
00115      LABEL RECORDS STANDARD                                       ECS090
00116      BLOCK CONTAINS 0 RECORDS
00117      RECORD CONTAINS 325 CHARACTERS.                              ECS090
00118  01  BK-EXTR.                                                     ECS090
00119      12  FILLER              PIC X(325).                          ECS090
00120  EJECT                                                            ECS090
00121  FD  DETEXTR                                                      ECS090
00122                                  COPY ECSEXTFD.                   ECS090
00123  01  DETAIL-EPEC.                                                 ECS090
00124      12  FILLER              PIC X(510).                          ECS090
00125                                                                   ECS090
00126  FD  BKUP-EPEC                                                    ECS090
00127                                  COPY ECSEXTFD.                   ECS090
00128                                                                   ECS090
00129  01  BK-EPEC.                                                     ECS090
00130      12  FILLER              PIC X(510).                          ECS090
00131                                                                   ECS090
00132  FD  REIN-FILE                                                    ECS090
00133      LABEL RECORDS STANDARD                                       ECS090
00134      RECORD CONTAINS 4000 CHARACTERS.                             ECS090
00135  01  REIN-REC.                                                    ECS090
00136      12  FILLER              PIC X(2).                            ECS090
00137      12  REIN-KEY            PIC X(8).                            ECS090
00138      12  FILLER              PIC X(3990).                         ECS090
00139                                                                   ECS090
00140  FD  BKUP-REIN                                                    ECS090
00141      RECORDING MODE F                                             ECS090
00142      LABEL RECORDS STANDARD                                       ECS090
00143      BLOCK CONTAINS 0 RECORDS
00144      RECORD CONTAINS 4000 CHARACTERS.                             ECS090
00145  01  BK-REIN.                                                     ECS090
00146      12  FILLER              PIC X(4000).                         ECS090
00147  EJECT                                                            ECS090
00148  FD  CTBL-FILE                                                    ECS090
00149      LABEL RECORDS STANDARD                                       ECS090
00150      RECORD CONTAINS 200 CHARACTERS.                              ECS090
00151  01  CTBL-REC.                                                    ECS090
00152      12  FILLER              PIC XX.                              ECS090
00153      12  COMM-KEY            PIC X(7).                            ECS090
00154      12  FILLER              PIC X(191).                          ECS090
00155                                                                   ECS090
00156  FD  BKUP-CTBL                                                    ECS090
00157      RECORDING MODE F                                             ECS090
00158      LABEL RECORDS STANDARD                                       ECS090
00159      BLOCK CONTAINS 0 RECORDS
00160      RECORD CONTAINS 200 CHARACTERS.                              ECS090
00161  01  BK-CTBL.                                                     ECS090
00162      12  FILLER              PIC X(200).                          ECS090
00163                                                                   ECS090
00164  FD  CRED-FILE                                                    ECS090
00165      RECORDING MODE F                                             ECS090
00166      LABEL RECORDS STANDARD                                       ECS090
00167      BLOCK CONTAINS 0 RECORDS
00168      RECORD CONTAINS 629 CHARACTERS.                              ECS090
00169  01  CRED-REC                PIC X(629).                          ECS090
00170                                                                   ECS090
00171  FD  BKUP-CRED                                                    ECS090
00172      RECORDING MODE F                                             ECS090
00173      LABEL RECORDS STANDARD                                       ECS090
00174      BLOCK CONTAINS 0 RECORDS
00175      RECORD CONTAINS 629 CHARACTERS.                              ECS090
00176  01  BK-CRED                 PIC X(629).                          ECS090
00177                                                                   ECS090
00178  FD  COMP-FILE                                                    ECS090
00179                                  COPY ECSCOIFD.                   ECS090
00180  01  COMP-MSTR-REC.                                               ECS090
00181      12  FILLER              PIC X(700).                          ECS090
00182                                                                   ECS090
00183  FD  BKUP-COMP                                                    ECS090
00184      RECORDING MODE F                                             ECS090
00185      LABEL RECORDS STANDARD                                       ECS090
00186      BLOCK CONTAINS 0 RECORDS
00187      RECORD CONTAINS 700 CHARACTERS.                              ECS090
00188  01  BK-COMP                 PIC X(700).                          ECS090
00189  EJECT                                                            ECS090
00190  WORKING-STORAGE SECTION.                                         ECS090
00191  77  FILLER  PIC X(32) VALUE '********************************'.  ECS090
00192  77  FILLER  PIC X(32) VALUE '     ECS090 WORKING-STORAGE     '.  ECS090
00193  77  FILLER  PIC X(32) VALUE '*****V/M=2.010 *****************'.     CL**2
00194                                                                   ECS090
00195  77  LN-CTR                  PIC S999  COMP-3    VALUE +066.      ECS090
00196  77  DATE-CTR                PIC S9(9) COMP-3    VALUE +0.        ECS090
00197  77  ACCT-CTR                PIC S9(9) COMP-3    VALUE +0.        ECS090
00198  77  EXTR-CTR                PIC S9(9) COMP-3    VALUE +0.        ECS090
00199  77  EPEC-CTR                PIC S9(9) COMP-3    VALUE +0.        ECS090
00200  77  CRED-CTR                PIC S9(9) COMP-3    VALUE +0.        ECS090
00201  77  REIN-CTR                PIC S9(9) COMP-3    VALUE +0.        ECS090
00202  77  CTBL-CTR                PIC S9(9) COMP-3    VALUE +0.        ECS090
00203  77  COMP-CTR                PIC S9(9) COMP-3    VALUE +0.        ECS090
00204  77  CCSW                    PIC 9               VALUE 0.         ECS090
00205                                                                   ECS090
00206  01  MISC-WORK.                                                   ECS090
00207      12  WS-ABEND-FILE-STATUS   PIC XX              VALUE ZEROS.  ECS090
00208      12  WS-ABEND-MESSAGE       PIC X(80)           VALUE SPACES. ECS090
00209      12  WS-RETURN-CODE         PIC S9(4)  COMP.                  ECS090
00210      12  WS-ZERO                PIC S9     VALUE ZERO COMP-3.     ECS090
00211      12  PGM-SUB                PIC S999 COMP       VALUE +090.   ECS090
00212      12  FIRST-TIME-SW          PIC X               VALUE 'Y'.    ECS090
00213          88  FIRST-TIME                          VALUE 'Y'.       ECS090
00214                                                                   ECS090
00215  01  FILE-STATUS.                                                 ECS090
00216      12  REIN-FILE-STATUS       PIC XX     VALUE ZERO.            ECS090
00217      12  COMM-FILE-STATUS       PIC XX     VALUE ZERO.            ECS090
00218      12  ERACCTT-FILE-STATUS    PIC XX     VALUE ZERO.            ECS090
00219                                                                   ECS090
00220  EJECT                                                            ECS090
00221  01  HEADINGS.                                                    ECS090
00222      12  HEAD-1.                                                  ECS090
00223          16  FILLER          PIC X(48)           VALUE SPACES.    ECS090
00224          16  FILLER          PIC X(28)           VALUE            ECS090
00225                 'SYSTEM BACKUP CONTROL TOTALS'.                   ECS090
00226          16  FILLER          PIC X(48)           VALUE SPACES.    ECS090
00227          16  FILLER          PIC X(8)            VALUE 'ECS-090'. ECS090
00228      12  HEAD-2.                                                  ECS090
00229          16  FILLER          PIC X(47)           VALUE SPACES.    ECS090
00230          16  HD-CLIENT       PIC X(30).                           ECS090
00231          16  FILLER          PIC X(47)           VALUE SPACES.    ECS090
00232          16  HD-IPL          PIC X(8)            VALUE SPACES.    ECS090
00233      12  HEAD-3.                                                  ECS090
00234          16  FILLER          PIC X(52)           VALUE SPACES.    ECS090
00235          16  HD-DATE         PIC X(18).                           ECS090
00236          16  FILLER          PIC X(42)           VALUE SPACES.    ECS090
00237          16  FILLER          PIC X(11)           VALUE            ECS090
00238                  'PAGE      1'.                                   ECS090
00239      12  HEAD-4.                                                  ECS090
00240          16  FILLER          PIC X(32)           VALUE            ECS090
00241                  '     FILE NAME                  '.              ECS090
00242          16  FILLER          PIC X(14)           VALUE            ECS090
00243                  '  RECORD COUNT'.                                ECS090
00244                                                                   ECS090
00245  01  P-REC.                                                       ECS090
00246      12  P-CCSW              PIC X.                               ECS090
00247      12  P-LINE.                                                  ECS090
00248          16  P-DESC          PIC X(32).                           ECS090
00249          16  P-CTR           PIC Z,ZZZ,ZZZ,ZZZ-.                  ECS090
00250          16  FILLER          PIC X(6).                            ECS090
00251          16  P-MSG           PIC X(40).                           ECS090
00252          16  FILLER          PIC X(40).                           ECS090
00253                                                                   ECS090
00254                              COPY ELCDTECX.                       ECS090
00255                                                                   ECS090
00256                              COPY ELCDTEVR.                       ECS090
00257  EJECT                                                            ECS090
00258  PROCEDURE DIVISION.                                              ECS090
00259                                                                   ECS090
00260  0000-INIT-RTN.                                                   ECS090
00261                              COPY ELCDTERX.                       ECS090
00262                                                                   ECS090
00263      MOVE COMPANY-NAME           TO HD-CLIENT.                    ECS090
00264      MOVE WS-CURRENT-DATE        TO HD-IPL.                       ECS090
00265      MOVE ALPH-DATE              TO HD-DATE.                      ECS090
00266                                                                   ECS090
00267      OPEN OUTPUT PRNT-FILE.                                       ECS090
00268                                                                   ECS090
00269      MOVE SPACES                 TO P-REC.                        ECS090
00270      MOVE 0                      TO CCSW.                         ECS090
00271                                                                   ECS090
00272      PERFORM 0200-HEAD-RTN    THRU 0299-EXIT.                     ECS090
00273                                                                   ECS090
00274      PERFORM 0300-BACKUP-DATE THRU 0399-EXIT.                     ECS090
00275      PERFORM 0400-BACKUP-ACCT THRU 0499-EXIT.                     ECS090
00276      PERFORM 0500-BACKUP-EXTR THRU 0599-EXIT.                     ECS090
00277      PERFORM 0600-BACKUP-EPEC THRU 0699-EXIT.                     ECS090
00278      PERFORM 0700-BACKUP-REIN THRU 0799-EXIT.                     ECS090
00279      PERFORM 0800-BACKUP-CTBL THRU 0899-EXIT.                     ECS090
00280      PERFORM 0900-BACKUP-CRED THRU 0999-EXIT.                     ECS090
00281      PERFORM 1099-BACKUP-COMP THRU 1999-EXIT.                     ECS090
00282                                                                   ECS090
00283      GO TO 2000-END-JOB.                                          ECS090
00284  EJECT                                                            ECS090
00285  0100-PRT-RTN.                                                    ECS090
00286      MOVE P-REC                  TO PRT-REC.                      ECS090
00287      MOVE P-CCSW                 TO CCSW.                         ECS090
00288      WRITE PRT-REC AFTER ADVANCING CCSW.                          ECS090
00289                                                                   ECS090
00290      MOVE SPACES                 TO P-REC.                        ECS090
00291      MOVE '0'                    TO P-CCSW.                       ECS090
00292                                                                   ECS090
00293      IF CCSW = '1'                                                ECS090
00294          MOVE +1                 TO LN-CTR                        ECS090
00295      ELSE                                                         ECS090
00296          IF CCSW = ' '                                            ECS090
00297              ADD +1 TO LN-CTR                                     ECS090
00298          ELSE                                                     ECS090
00299              IF CCSW = '0'                                        ECS090
00300                  ADD +2 TO LN-CTR                                 ECS090
00301              ELSE                                                 ECS090
00302                  ADD +3 TO LN-CTR.                                ECS090
00303                                                                   ECS090
00304  0199-EXIT.                                                       ECS090
00305      EXIT.                                                        ECS090
00306                                                                   ECS090
00307  0200-HEAD-RTN.                                                   ECS090
00308      MOVE '1'                    TO P-CCSW.                       ECS090
00309      MOVE HEAD-1                 TO P-LINE.                       ECS090
00310                                                                   ECS090
00311      PERFORM 0100-PRT-RTN THRU 0199-EXIT.                         ECS090
00312                                                                   ECS090
00313      MOVE ' '                    TO P-CCSW.                       ECS090
00314      MOVE HEAD-2                 TO P-LINE.                       ECS090
00315                                                                   ECS090
00316      PERFORM 0100-PRT-RTN THRU 0199-EXIT.                         ECS090
00317                                                                   ECS090
00318      MOVE HEAD-3                 TO P-LINE.                       ECS090
00319                                                                   ECS090
00320      PERFORM 0100-PRT-RTN THRU 0199-EXIT.                         ECS090
00321                                                                   ECS090
00322      MOVE HEAD-4                 TO P-LINE.                       ECS090
00323                                                                   ECS090
00324      PERFORM 0100-PRT-RTN THRU 0199-EXIT 2 TIMES.                 ECS090
00325                                                                   ECS090
00326  0299-EXIT.                                                       ECS090
00327      EXIT.                                                        ECS090
00328  EJECT                                                            ECS090
00329  0300-BACKUP-DATE.                                                ECS090
00330      OPEN INPUT  DISK-DATE                                        ECS090
00331           OUTPUT BKUP-DATE.                                       ECS090
00332 *  FIRST BACKUP - COMPLETE REWIND OF OUTPUT TAPE.                 ECS090
00333                                                                   ECS090
00334  0310-READ-DATE.                                                  ECS090
00335      READ DISK-DATE AT END                                        ECS090
00336          GO TO 0320-END-DATE.                                     ECS090
00337                                                                   ECS090
00338      ADD +1                      TO DATE-CTR.                     ECS090
00339      MOVE DATE-DISK              TO BK-DATE.                      ECS090
00340                                                                   ECS090
00341      WRITE BK-DATE.                                               ECS090
00342                                                                   ECS090
00343      GO TO 0310-READ-DATE.                                        ECS090
00344                                                                   ECS090
00345  0320-END-DATE.                                                   ECS090
00346      MOVE 'DATE CARD'            TO P-DESC.                       ECS090
00347      MOVE DATE-CTR               TO P-CTR.                        ECS090
00348      MOVE 'FILE BACKED UP'       TO P-MSG.                        ECS090
00349                                                                   ECS090
00350      PERFORM 0100-PRT-RTN THRU 0199-EXIT.                         ECS090
00351                                                                   ECS090
00352      CLOSE DISK-DATE.                                             ECS090
00353      CLOSE BKUP-DATE WITH NO REWIND.                              ECS090
00354                                                                   ECS090
00355  0399-EXIT.                                                       ECS090
00356      EXIT.                                                        ECS090
00357  EJECT                                                            ECS090
00358  0400-BACKUP-ACCT.                                                ECS090
00359      OPEN INPUT  ERACCTT                                          ECS090
00360           OUTPUT BKUP-ACCT WITH NO REWIND.                        ECS090
00361                                                                   ECS090
00362      IF ERACCTT-FILE-STATUS  = '00' OR '97'                       ECS090
00363          NEXT SENTENCE                                            ECS090
00364        ELSE                                                       ECS090
00365          MOVE ERACCTT-FILE-STATUS                                 ECS090
00366                                  TO WS-ABEND-FILE-STATUS          ECS090
00367          MOVE 'ERACCTT OPEN ERROR- '                              ECS090
00368                                  TO WS-ABEND-MESSAGE              ECS090
00369          GO TO ABEND-PGM.                                         ECS090
00370                                                                   ECS090
00371  0410-READ-ACCT.                                                  ECS090
00372      READ ERACCTT.                                                ECS090
00373                                                                   ECS090
00374      IF ERACCTT-FILE-STATUS = '10'                                ECS090
00375          GO TO 0420-END-ACCT.                                     ECS090
00376                                                                   ECS090
00377      IF ERACCTT-FILE-STATUS NOT = '00'                            ECS090
00378          MOVE ERACCTT-FILE-STATUS                                 ECS090
00379                                  TO WS-ABEND-FILE-STATUS          ECS090
00380          MOVE 'ERACCTT READ ERROR- '                              ECS090
00381                                  TO WS-ABEND-MESSAGE              ECS090
00382          GO TO ABEND-PGM.                                         ECS090
00383                                                                   ECS090
00384      ADD +1                      TO ACCT-CTR.                     ECS090
00385      MOVE ACCT-REC               TO BK-ACCT.                      ECS090
00386                                                                   ECS090
00387      WRITE BK-ACCT.                                               ECS090
00388                                                                   ECS090
00389      GO TO 0410-READ-ACCT.                                        ECS090
00390                                                                   ECS090
00391  0420-END-ACCT.                                                   ECS090
00392      MOVE 'ACCOUNT MASTER'       TO P-DESC.                       ECS090
00393      MOVE ACCT-CTR               TO P-CTR.                        ECS090
00394      MOVE 'FILE BACKED UP'       TO P-MSG.                        ECS090
00395                                                                   ECS090
00396      PERFORM 0100-PRT-RTN THRU 0199-EXIT.                         ECS090
00397                                                                   ECS090
00398      CLOSE ERACCTT.                                               ECS090
00399                                                                   ECS090
00400      IF ERACCTT-FILE-STATUS NOT = '00'                            ECS090
00401          MOVE ERACCTT-FILE-STATUS                                 ECS090
00402                                  TO WS-ABEND-FILE-STATUS          ECS090
00403          MOVE 'ERACCTT CLOSE ERROR- '                             ECS090
00404                                  TO WS-ABEND-MESSAGE              ECS090
00405          GO TO ABEND-PGM.                                         ECS090
00406                                                                   ECS090
00407      CLOSE BKUP-ACCT WITH NO REWIND.                              ECS090
00408                                                                   ECS090
00409  0499-EXIT.                                                       ECS090
00410      EXIT.                                                        ECS090
00411                                                                   ECS090
00412      EJECT                                                        ECS090
00413  0500-BACKUP-EXTR.                                                ECS090
00414      OPEN INPUT  EPECEXTR                                         ECS090
00415           OUTPUT BKUP-EXTR WITH NO REWIND.                        ECS090
00416                                                                   ECS090
00417  0510-READ-EXTR.                                                  ECS090
00418      READ EPECEXTR AT END                                         ECS090
00419          GO TO 0520-END-EXTR.                                     ECS090
00420                                                                   ECS090
00421      ADD +1 TO EXTR-CTR.                                          ECS090
00422      MOVE EXTR-REC               TO BK-EXTR.                      ECS090
00423                                                                   ECS090
00424      WRITE BK-EXTR.                                               ECS090
00425                                                                   ECS090
00426      GO TO 0510-READ-EXTR.                                        ECS090
00427                                                                   ECS090
00428  0520-END-EXTR.                                                   ECS090
00429      MOVE 'EXTRACT FILE'         TO P-DESC.                       ECS090
00430      MOVE EXTR-CTR               TO P-CTR.                        ECS090
00431      MOVE 'FILE BACKED UP'       TO P-MSG.                        ECS090
00432                                                                   ECS090
00433      PERFORM 0100-PRT-RTN THRU 0199-EXIT.                         ECS090
00434                                                                   ECS090
00435      CLOSE EPECEXTR.                                              ECS090
00436      CLOSE BKUP-EXTR WITH NO REWIND.                              ECS090
00437                                                                   ECS090
00438  0599-EXIT.                                                       ECS090
00439      EXIT.                                                        ECS090
00440   EJECT                                                           ECS090
00441  0600-BACKUP-EPEC.                                                ECS090
00442      OPEN OUTPUT BKUP-EPEC WITH NO REWIND.                        ECS090
00443                                                                   ECS090
00444      IF DTE-SYS-F-CLASIC-CREDIT = '1'                             ECS090
00445          MOVE 'EPEC FILE   '     TO P-DESC                        ECS090
00446          MOVE +0                 TO P-CTR                         ECS090
00447          MOVE 'NO EPEC BACKUP'   TO P-MSG                         ECS090
00448          PERFORM 0100-PRT-RTN THRU 0199-EXIT                      ECS090
00449          GO TO 0630-CLOSE-EPEC.                                   ECS090
00450                                                                   ECS090
00451      OPEN INPUT  DETEXTR.                                         ECS090
00452                                                                   ECS090
00453  0610-READ-EPEC.                                                  ECS090
00454      READ DETEXTR AT END                                          ECS090
00455          GO TO 0620-END-EPEC.                                     ECS090
00456                                                                   ECS090
00457      ADD +1 TO EPEC-CTR.                                          ECS090
00458      MOVE DETAIL-EPEC            TO BK-EPEC.                      ECS090
00459                                                                   ECS090
00460      WRITE BK-EPEC.                                               ECS090
00461                                                                   ECS090
00462      GO TO 0610-READ-EPEC.                                        ECS090
00463                                                                   ECS090
00464  0620-END-EPEC.                                                   ECS090
00465      MOVE 'EPEC FILE   '         TO P-DESC.                       ECS090
00466      MOVE EPEC-CTR               TO P-CTR.                        ECS090
00467      MOVE 'FILE BACKED UP'       TO P-MSG.                        ECS090
00468                                                                   ECS090
00469      PERFORM 0100-PRT-RTN THRU 0199-EXIT.                         ECS090
00470                                                                   ECS090
00471      CLOSE DETEXTR.                                               ECS090
00472                                                                   ECS090
00473  0630-CLOSE-EPEC.                                                 ECS090
00474      CLOSE BKUP-EPEC WITH NO REWIND.                              ECS090
00475                                                                   ECS090
00476  0699-EXIT.                                                       ECS090
00477      EXIT.                                                        ECS090
00478  EJECT                                                            ECS090
00479  0700-BACKUP-REIN.                                                ECS090
00480      OPEN OUTPUT BKUP-REIN WITH NO REWIND.                        ECS090
00481                                                                   ECS090
00482      IF DTE-REINSURANCE = ' '                                     ECS090
00483          MOVE LOW-VALUE         TO BK-REIN                        ECS090
00484          WRITE BK-REIN                                            ECS090
00485          MOVE 'REINSURANCE TABLES' TO P-DESC                      ECS090
00486          MOVE +1                TO P-CTR                          ECS090
00487          MOVE 'ZERO FILE USED'  TO P-MSG                          ECS090
00488          PERFORM 0100-PRT-RTN THRU 0199-EXIT                      ECS090
00489          GO TO 0730-CLOSE-REIN.                                   ECS090
00490                                                                   ECS090
00491      OPEN INPUT REIN-FILE.                                        ECS090
00492                                                                   ECS090
00493      IF REIN-FILE-STATUS  = '00' OR '97'                          ECS090
00494          NEXT SENTENCE                                            ECS090
00495        ELSE                                                       ECS090
00496          MOVE REIN-FILE-STATUS                                    ECS090
00497                                  TO WS-ABEND-FILE-STATUS          ECS090
00498          MOVE 'REIN OPEN ERROR- '                                 ECS090
00499                                  TO WS-ABEND-MESSAGE              ECS090
00500          GO TO ABEND-PGM.                                         ECS090
00501                                                                   ECS090
00502      MOVE LOW-VALUE TO REIN-KEY.                                  ECS090
00503                                                                   ECS090
00504  0710-READ-REIN.                                                  ECS090
00505      READ REIN-FILE AT END                                        ECS090
00506          GO TO 0720-END-REIN.                                     ECS090
00507                                                                   ECS090
00508      IF REIN-FILE-STATUS NOT = '00'                               ECS090
00509          MOVE REIN-FILE-STATUS                                    ECS090
00510                                  TO WS-ABEND-FILE-STATUS          ECS090
00511          MOVE 'REIN READ ERROR- '                                 ECS090
00512                                  TO WS-ABEND-MESSAGE              ECS090
00513          GO TO ABEND-PGM.                                         ECS090
00514                                                                   ECS090
00515      ADD +1 TO REIN-CTR.                                          ECS090
00516      MOVE REIN-REC               TO BK-REIN.                      ECS090
00517                                                                   ECS090
00518      WRITE BK-REIN.                                               ECS090
00519                                                                   ECS090
00520      GO TO 0710-READ-REIN.                                        ECS090
00521                                                                   ECS090
00522  0720-END-REIN.                                                   ECS090
00523      MOVE 'REINSURANCE TABLES'   TO P-DESC.                       ECS090
00524      MOVE REIN-CTR               TO P-CTR.                        ECS090
00525      MOVE 'FILE BACKED UP'       TO P-MSG.                        ECS090
00526                                                                   ECS090
00527      PERFORM 0100-PRT-RTN THRU 0199-EXIT.                         ECS090
00528                                                                   ECS090
00529      CLOSE REIN-FILE.                                             ECS090
00530                                                                   ECS090
00531      IF REIN-FILE-STATUS NOT = '00'                               ECS090
00532          MOVE REIN-FILE-STATUS                                    ECS090
00533                                  TO WS-ABEND-FILE-STATUS          ECS090
00534          MOVE 'REIN CLOSE ERROR- '                                ECS090
00535                                  TO WS-ABEND-MESSAGE              ECS090
00536          GO TO ABEND-PGM.                                         ECS090
00537                                                                   ECS090
00538  0730-CLOSE-REIN.                                                 ECS090
00539      CLOSE BKUP-REIN WITH NO REWIND.                              ECS090
00540                                                                   ECS090
00541  0799-EXIT.                                                       ECS090
00542      EXIT.                                                        ECS090
00543  EJECT                                                            ECS090
00544  0800-BACKUP-CTBL.                                                ECS090
00545      OPEN OUTPUT BKUP-CTBL WITH NO REWIND.                        ECS090
00546                                                                   ECS090
00547      IF DTE-COM-TBL-USED = '1'                                    ECS090
00548          MOVE LOW-VALUE          TO BK-CTBL                       ECS090
00549          WRITE BK-CTBL                                            ECS090
00550          MOVE +1                 TO P-CTR                         ECS090
00551          MOVE 'COMPENSATION TABLES' TO P-DESC                     ECS090
00552          MOVE 'ZERO FILE USED'   TO P-MSG                         ECS090
00553          PERFORM 0100-PRT-RTN THRU 0199-EXIT                      ECS090
00554          GO TO 0830-CLOSE-CTBL.                                   ECS090
00555                                                                   ECS090
00556      OPEN INPUT CTBL-FILE.                                        ECS090
00557                                                                   ECS090
00558      IF COMM-FILE-STATUS  = '00' OR '97'                          ECS090
00559          NEXT SENTENCE                                            ECS090
00560        ELSE                                                       ECS090
00561          MOVE COMM-FILE-STATUS                                    ECS090
00562                                  TO WS-ABEND-FILE-STATUS          ECS090
00563          MOVE 'COMM OPEN ERROR- '                                 ECS090
00564                                  TO WS-ABEND-MESSAGE              ECS090
00565          GO TO ABEND-PGM.                                         ECS090
00566                                                                   ECS090
00567      MOVE LOW-VALUES             TO COMM-KEY.                     ECS090
00568                                                                   ECS090
00569  0810-READ-CTBL.                                                  ECS090
00570      READ CTBL-FILE AT END                                        ECS090
00571          GO TO 0820-END-CTBL.                                     ECS090
00572                                                                   ECS090
00573      IF COMM-FILE-STATUS NOT = '00'                               ECS090
00574          MOVE COMM-FILE-STATUS                                    ECS090
00575                                  TO WS-ABEND-FILE-STATUS          ECS090
00576          MOVE 'COMM READ ERROR- '                                 ECS090
00577                                  TO WS-ABEND-MESSAGE              ECS090
00578          GO TO ABEND-PGM.                                         ECS090
00579                                                                   ECS090
00580      ADD +1 TO CTBL-CTR.                                          ECS090
00581      MOVE CTBL-REC               TO BK-CTBL.                      ECS090
00582                                                                   ECS090
00583      WRITE BK-CTBL.                                               ECS090
00584                                                                   ECS090
00585      GO TO 0810-READ-CTBL.                                        ECS090
00586                                                                   ECS090
00587  0820-END-CTBL.                                                   ECS090
00588      MOVE 'COMPENSATION TABLES'  TO P-DESC.                       ECS090
00589      MOVE CTBL-CTR               TO P-CTR.                        ECS090
00590      MOVE 'FILE BACKED UP'       TO P-MSG.                        ECS090
00591                                                                   ECS090
00592      PERFORM 0100-PRT-RTN THRU 0199-EXIT.                         ECS090
00593                                                                   ECS090
00594      CLOSE CTBL-FILE.                                             ECS090
00595                                                                   ECS090
00596      IF COMM-FILE-STATUS NOT = '00'                               ECS090
00597          MOVE COMM-FILE-STATUS                                    ECS090
00598                                  TO WS-ABEND-FILE-STATUS          ECS090
00599          MOVE 'COMM CLOSE ERROR- '                                ECS090
00600                                  TO WS-ABEND-MESSAGE              ECS090
00601          GO TO ABEND-PGM.                                         ECS090
00602                                                                   ECS090
00603  0830-CLOSE-CTBL.                                                 ECS090
00604      CLOSE BKUP-CTBL WITH NO REWIND.                              ECS090
00605                                                                   ECS090
00606  0899-EXIT.                                                       ECS090
00607      EXIT.                                                        ECS090
00608  EJECT                                                            ECS090
00609  0900-BACKUP-CRED.                                                ECS090
00610      OPEN OUTPUT BKUP-CRED WITH NO REWIND.                        ECS090
00611                                                                   ECS090
00612      IF DTE-SYS-F-CLASIC-CREDIT NOT = '1'                         ECS090
00613          MOVE 'CREDIT TRANS FILE'   TO P-DESC                     ECS090
00614          MOVE +0                    TO P-CTR                      ECS090
00615          MOVE 'NO CRED BACKUP'      TO P-MSG                      ECS090
00616          PERFORM 0100-PRT-RTN THRU 0199-EXIT                      ECS090
00617          GO TO 0930-CLOSE-CRED.                                   ECS090
00618                                                                   ECS090
00619      OPEN INPUT  CRED-FILE.                                       ECS090
00620                                                                   ECS090
00621  0910-READ-CRED.                                                  ECS090
00622      READ CRED-FILE AT END                                        ECS090
00623          GO TO 0920-END-CRED.                                     ECS090
00624                                                                   ECS090
00625      ADD +1 TO CRED-CTR.                                          ECS090
00626      MOVE CRED-REC                  TO BK-CRED.                   ECS090
00627                                                                   ECS090
00628      WRITE BK-CRED.                                               ECS090
00629                                                                   ECS090
00630      GO TO 0910-READ-CRED.                                        ECS090
00631                                                                   ECS090
00632  0920-END-CRED.                                                   ECS090
00633      MOVE 'CREDIT TRANS FILE'    TO P-DESC.                       ECS090
00634      MOVE CRED-CTR               TO P-CTR.                        ECS090
00635      MOVE 'FILE BACKED UP'       TO P-MSG.                        ECS090
00636                                                                   ECS090
00637      PERFORM 0100-PRT-RTN THRU 0199-EXIT.                         ECS090
00638                                                                   ECS090
00639      CLOSE CRED-FILE.                                             ECS090
00640                                                                   ECS090
00641  0930-CLOSE-CRED.                                                 ECS090
00642      CLOSE BKUP-CRED WITH LOCK.                                   ECS090
00643                                                                   ECS090
00644  0999-EXIT.                                                       ECS090
00645      EXIT.                                                        ECS090
00646  EJECT                                                            ECS090
00647  1099-BACKUP-COMP.                                                ECS090
00648      OPEN OUTPUT BKUP-COMP WITH NO REWIND.                        ECS090
00649                                                                   ECS090
00650      IF DTE-SYS-F-CLASIC-CREDIT NOT = '1'                         ECS090
00651          MOVE 'COMP MASTER FILE' TO P-DESC                        ECS090
00652          MOVE +0                 TO P-CTR                         ECS090
00653          MOVE 'NO COMP BACKUP'   TO P-MSG                         ECS090
00654          PERFORM 0100-PRT-RTN THRU 0199-EXIT                      ECS090
00655          GO TO 0930-CLOSE-COMP.                                   ECS090
00656                                                                   ECS090
00657      OPEN INPUT  COMP-FILE.                                       ECS090
00658                                                                   ECS090
00659  0910-READ-COMP.                                                  ECS090
00660      READ COMP-FILE AT END                                        ECS090
00661          GO TO 0920-END-COMP.                                     ECS090
00662                                                                   ECS090
00663      ADD +1 TO COMP-CTR.                                          ECS090
00664      MOVE COMP-MSTR-REC               TO BK-COMP.                 ECS090
00665                                                                   ECS090
00666      WRITE BK-COMP.                                               ECS090
00667                                                                   ECS090
00668      GO TO 0910-READ-COMP.                                        ECS090
00669                                                                   ECS090
00670  0920-END-COMP.                                                   ECS090
00671      MOVE 'COMP MASTER FILE'     TO P-DESC.                       ECS090
00672      MOVE COMP-CTR               TO P-CTR.                        ECS090
00673      MOVE 'FILE BACKED UP'       TO P-MSG.                        ECS090
00674                                                                   ECS090
00675      PERFORM 0100-PRT-RTN THRU 0199-EXIT.                         ECS090
00676                                                                   ECS090
00677      CLOSE COMP-FILE.                                             ECS090
00678                                                                   ECS090
00679  0930-CLOSE-COMP.                                                 ECS090
00680      CLOSE BKUP-COMP WITH LOCK.                                   ECS090
00681                                                                   ECS090
00682  1999-EXIT.                                                       ECS090
00683      EXIT.                                                        ECS090
00684  EJECT                                                            ECS090
00685  2000-END-JOB.                                                    ECS090
00686      CLOSE PRNT-FILE.                                             ECS090
00687      GOBACK.                                                      ECS090
00688                                                                   ECS090
00689  ABEND-PGM SECTION.                                               ECS090
00690                                    COPY ELCABEND.                 ECS090
