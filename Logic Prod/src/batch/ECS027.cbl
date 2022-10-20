00001  IDENTIFICATION DIVISION.                                         05/06/98
00002                                                                   ECS027
00003  PROGRAM-ID.                ECS027.                                  LV019
00004 *              PROGRAM CONVERTED BY                               ECS027
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ECS027
00006 *              CONVERSION DATE 01/19/95 14:38:50.                 ECS027
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          ECS027
00008 *                           VMOD=2.018.                           ECS027
00009                                                                   ECS027
00010 *AUTHOR.        LOGIC, INC.                                       ECS027
00011 *               DALLAS, TEXAS.                                    ECS027
00012 *DATE-COMPILED.                                                   ECS027
00013                                                                   ECS027
00014 *SECURITY.   *****************************************************ECS027
00015 *            *                                                   *ECS027
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ECS027
00017 *            *                                                   *ECS027
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS027
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ECS027
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *ECS027
00021 *            *                                                   *ECS027
00022 *            *****************************************************ECS027
00023                                                                   ECS027
00024 *REMARKS.                                                         ECS027
00025 *        THIS PROGRAM GENERATES NEW BUSINESS AND CLAIM            ECS027
00026 *        DETAIL EXTRACTS FOR THE PRODUCTION SUMMARY               ECS027
00027 *        REPORTER PROGRAM (ECS028).                               ECS027
00028                                                                   ECS027
052814******************************************************************
052814*                   C H A N G E   L O G
052814*
052814* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
052814*-----------------------------------------------------------------
052814*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
052814* EFFECTIVE    NUMBER
052814*-----------------------------------------------------------------
052814* 052814  CR2014012300001  PEMA  DCC CREDIT UNION CHANGES
052814******************************************************************
00029  ENVIRONMENT DIVISION.                                            ECS027
00030  INPUT-OUTPUT SECTION.                                            ECS027
00031  FILE-CONTROL.                                                    ECS027
00032                                                                   ECS027
00033      SELECT SORT-CALL        ASSIGN TO SYS001-UT-FBA1-S-SORTWK1.  ECS027
00034      SELECT SORT-CALL-C      ASSIGN TO SYS001-UT-FBA1-S-SORTWK1.  ECS027
00035      SELECT PROD-SUM-REQ                                          ECS027
00036                              ASSIGN TO SYS006-UR-2540R-S-SYS006.  ECS027
00037      SELECT PRINTR           ASSIGN TO SYS008-UR-1403-S-SYS008.   ECS027
00038      SELECT CERTS            ASSIGN TO SYS010-UT-2400-S-SYS010.   ECS027
00039      SELECT CLAIM-HIST       ASSIGN TO SYS011-UT-2400-S-SYS011.   ECS027
00040      SELECT EPEC             ASSIGN TO SYS012-UT-2400-S-SYS012.   ECS027
00041      SELECT EXTR-CLAIM       ASSIGN TO SYS013-UT-FBA1-S-SYS013.   ECS027
00042      SELECT PROD-DTL-EXT     ASSIGN TO SYS016-UT-FBA1-S-SYS016.   ECS027
00043      SELECT DISK-DATE        ASSIGN TO SYS019-UT-FBA1-S-SYS019.   ECS027
00044      SELECT ERACCTT          ASSIGN TO SYS025-FBA1-ERACCTT        ECS027
00045                              ORGANIZATION IS INDEXED              ECS027
00046                              ACCESS IS SEQUENTIAL                 ECS027
00047                              RECORD KEY IS AM-CONTROL-PRIMARY     ECS027
00048                              FILE STATUS IS ERACCT-FILE-STATUS.   ECS027
00049                                                                   ECS027
00050      SELECT ELCNTL           ASSIGN TO SYS009-FBA1-ELCNTL         ECS027
00051                              ACCESS       DYNAMIC                 ECS027
00052                              ORGANIZATION INDEXED                 ECS027
00053                              FILE STATUS  ELCNTL-FILE-STATUS      ECS027
00054                              RECORD KEY   CF-CONTROL-PRIMARY.     ECS027
00055                                                                   ECS027
00056  EJECT                                                            ECS027
00057  DATA DIVISION.                                                   ECS027
00058  FILE SECTION.                                                    ECS027
00059                                                                   ECS027
00060  SD  SORT-CALL.                                                   ECS027
00061                                                                   ECS027
00062  01  SRT-CLL.                                                     ECS027
00063      12  SRT-KEY             PIC X(91).                           ECS027
00064      12  SRT-DATA            PIC X(2149).                         ECS027
00065                                                                   ECS027
00066  SD  SORT-CALL-C.                                                 ECS027
00067                                                                   ECS027
00068  01  SRT-CLL-C.                                                   ECS027
00069      12  SRT-KEY-C           PIC X(31).                           ECS027
00070      12  SRT-DATA-C          PIC X(11).                           ECS027
00071                                                                   ECS027
00072  FD  PROD-SUM-REQ                                                 ECS027
00073      BLOCK CONTAINS 0 RECORDS
00074      RECORDING MODE F.                                            ECS027
00075                                                                   ECS027
00076  01  PROD-SUMMARY-REQUEST.                                        ECS027
00077      12  PSR-CLIENT-ID       PIC XXX.                             ECS027
00078      12  PSR-CARD-CODE       PIC X.                               ECS027
00079      12  FILLER              PIC X(76).                           ECS027
00080                                                                   ECS027
00081  FD  PRINTR                                                       ECS027
00082      RECORDING MODE F.                                            ECS027
00083                                                                   ECS027
00084  01  PRT.                                                         ECS027
00085      12  PRT-DATA.                                                ECS027
00086          16  PRT-CNTL        PIC X.                               ECS027
00087          16  FILLER          PIC X(132).                          ECS027
00088  EJECT                                                            ECS027
00089  FD  CERTS                                                        ECS027
00090      BLOCK CONTAINS 0 RECORDS
00091      RECORDING MODE F.                                            ECS027
00092                                                                   ECS027
00093                              COPY ECSCRT01.                       ECS027
00094  EJECT                                                            ECS027
00095  FD  CLAIM-HIST                                                   ECS027
00096      BLOCK CONTAINS 0 RECORDS
00097      RECORDING MODE F.                                            ECS027
00098                                                                   ECS027
00099  01  CLAIM-HISTORY.                                               ECS027
00100      12  FILLER              PIC X(510).                          ECS027
00101                                                                   ECS027
00102  FD  EPEC                                                         ECS027
00103      BLOCK CONTAINS 0 RECORDS
00104      RECORDING MODE F.                                            ECS027
00105                                                                   ECS027
00106  01  EPEC-RECORD.                                                 ECS027
00107      12  FILLER              PIC X(325).                          ECS027
00108                                                                   ECS027
00109  FD  EXTR-CLAIM                                                   ECS027
00110      BLOCK CONTAINS 0 RECORDS
00111      RECORDING MODE F.                                            ECS027
00112                                                                   ECS027
00113  01  CLAIM-EXTRACT-IO        PIC X(42).                           ECS027
00114  EJECT                                                            ECS027
00115  FD  ERACCTT.                                                     ECS027
00116                                                                   ECS027
00117                              COPY ERCACCT.                        ECS027
00118  EJECT                                                            ECS027
00119  FD  ELCNTL.                                                      ECS027
00120                                    COPY ELCCNTL.                  ECS027
00121  EJECT                                                            ECS027
00122                                                                   ECS027
00123  FD  PROD-DTL-EXT                                                 ECS027
00124      BLOCK CONTAINS 0 RECORDS
00125      RECORDING MODE F.                                            ECS027
00126                                                                   ECS027
00127  01  PROD-DETAIL-EXTRACT.                                         ECS027
00128      12  FILLER              PIC X(2240).                         ECS027
00129                                                                   ECS027
00130                                                                   ECS027
00131  FD  DISK-DATE                                                    ECS027
00132                              COPY ELCDTEFD.                       ECS027
00133  EJECT                                                            ECS027
00134  WORKING-STORAGE SECTION.                                         ECS027
00135  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      ECS027
00136  77  FILLER PIC X(32) VALUE '********************************'.   ECS027
00137  77  FILLER PIC X(32) VALUE '**  ECS027  WORKING STORAGE   **'.   ECS027
00138  77  FILLER PIC X(32) VALUE '***********VMOD=2.018 **********'.   ECS027
00139                                                                   ECS027
00140  77  PGM-SUB                 PIC S9(3)       VALUE +27 COMP.      ECS027
00141  77  BUS-CODE-IDX            PIC S9(4)                 COMP.      ECS027
00142  77  DH                      PIC S9(4)                 COMP.      ECS027
00143  77  STATE-IDX               PIC S9(4)                 COMP.      ECS027
00144  77  INDX                    PIC S9(4)                 COMP.      ECS027
00145  77  X1                      PIC S9(4)       VALUE +0  COMP.      ECS027
00146  77  X2                      PIC S9(4)       VALUE +0  COMP.      ECS027
00147  77  DETAIL-EXTRACT-COUNT    PIC S9(9)       VALUE +0  COMP-3.    ECS027
00148  77  CLAIM-SORT-COUNT        PIC S9(9)       VALUE +0  COMP-3.    ECS027
00149  77  CLAIM-WRITE-COUNT       PIC S9(9)       VALUE +0  COMP-3.    ECS027
00150  77  CLAIM-READ-COUNT        PIC S9(9)       VALUE +0  COMP-3.    ECS027
00151  77  CLAIM-USE-COUNT         PIC S9(9)       VALUE +0  COMP-3.    ECS027
00152  77  EPEC-READ-COUNT         PIC S9(9)       VALUE +0  COMP-3.    ECS027
00153  77  EPEC-USE-COUNT          PIC S9(9)       VALUE +0  COMP-3.    ECS027
00154  77  CERT-READ-COUNT         PIC S9(9)       VALUE +0  COMP-3.    ECS027
00155  77  CERT-USE-COUNT          PIC S9(9)       VALUE +0  COMP-3.    ECS027
00156  77  ACCT-READ-COUNT         PIC S9(9)       VALUE +0  COMP-3.    ECS027
00157  77  AMOUNT-1                PIC S9(11)V99   VALUE +0  COMP-3.    ECS027
00158  77  AMOUNT-2                PIC S9(11)V99   VALUE +0  COMP-3.    ECS027
00159  77  AMT-DIFF                PIC S9(11)V99   VALUE +0  COMP-3.    ECS027
00160  77  AMT-SUM                 PIC S9(11)V99   VALUE +0  COMP-3.    ECS027
00161                                                                   ECS027
00162  77  INDEX-VALID             PIC X     VALUE 'Y'.                 ECS027
00163  77  CLM-HIST-ERROR          PIC X     VALUE 'N'.                 ECS027
00164  77  REQUEST-EOF             PIC X     VALUE 'N'.                 ECS027
00165  77  EOF-TYPE-CODE           PIC X     VALUE 'N'.                 ECS027
00166  77  CERT-EOF                PIC X     VALUE 'N'.                 ECS027
00167  77  ACCT-EOF                PIC X     VALUE 'N'.                 ECS027
00168  77  CLAIM-EOF               PIC X     VALUE 'N'.                 ECS027
00169  77  EPEC-EOF                PIC X     VALUE 'N'.                 ECS027
00170  77  HEIRARCHY-DUPS-FOUND    PIC X     VALUE 'N'.                 ECS027
00171  77  EXTRACT-BUILT           PIC X     VALUE 'N'.                 ECS027
00172  77  ACCT-QUALIFIED          PIC X     VALUE 'Y'.                 ECS027
00173  77  CLAIM-EXTRACTS-SORTED   PIC X     VALUE 'N'.                 ECS027
00174  77  CLAIM-EXTRACTS-BUILT    PIC X     VALUE 'N'.                 ECS027
00175  77  CERT-DATA-BUILT         PIC X     VALUE 'N'.                 ECS027
00176  77  CLAIM-DATA-BUILT        PIC X     VALUE 'N'.                 ECS027
00177  77  EPEC-DATA-BUILT         PIC X     VALUE 'N'.                 ECS027
00178  77  CERT-WAS-READ           PIC X     VALUE 'N'.                 ECS027
00179  77  CLAIM-WAS-READ          PIC X     VALUE 'N'.                 ECS027
00180  77  EPEC-WAS-READ           PIC X     VALUE 'N'.                 ECS027
00181  77  ABEND-OPTION            PIC X     VALUE 'Y'.                 ECS027
00182  77  ABEND-CODE              PIC X(4)  VALUE ZEROS.               ECS027
00183  77  DEFAULT-TERM-GROUPING   PIC X(12) VALUE '242536374849'.      ECS027
00184  77  DEFAULT-AGE-GROUPING    PIC X(16) VALUE '2526353645465556'.  ECS027
00185  77  COUNT-DISPLAY           PIC ZZZ,ZZZ,ZZ9.                     ECS027
00186  77  WS-RETURN-CODE          PIC S9(4)  COMP.                     ECS027
00187  77  WS-ABEND-MESSAGE        PIC X(80).                           ECS027
00188  77  WS-ABEND-FILE-STATUS    PIC XX    VALUE ZEROS.               ECS027
00189  77  WS-ZERO                 PIC S9    VALUE +0  COMP-3.          ECS027
00190  77  ERACCT-FILE-STATUS      PIC XX    VALUE SPACES.              ECS027
00191  77  ELCNTL-FILE-STATUS      PIC XX    VALUE SPACES.              ECS027
00192                                                                   ECS027
00193  77  FIRST-TIME-SW           PIC X     VALUE 'Y'.                 ECS027
00194  EJECT                                                            ECS027
00195 *                                                                 ECS027
00196 *                                                                 ECS027
00197 * LEAVE THE ABOVE 2 LINE IN TO PREVENT LOSING THE LINE ABOVE      ECS027
00210  EJECT                                                            ECS027
00211  01  WS.                                                          ECS027
00212      12  WS-CUSTOMIZATION-FLAG           PIC X   VALUE SPACE.     ECS027
00213          88 CUSTOMIZATION-FOUND                  VALUE SPACE.     ECS027
00214      12  WS-CID-LOW-MDY                  PIC X(6).                   CL**6
00215      12  WS-CID-HIGH-MDY                 PIC X(6).                   CL**6
00216  01  WS-AM-REC.                                                   ECS027
00217      12  FILLER                          PIC XXX.                 ECS027
00218      12  WS-AM-CNTRL.                                             ECS027
00219          16  WS-AM-CARRIER               PIC X.                   ECS027
00220          16  WS-AM-GROUPING              PIC X(6).                ECS027
00221          16  WS-AM-STATE                 PIC XX.                  ECS027
00222          16  WS-AM-ACCOUNT               PIC X(10).               ECS027
00223          16  WS-AM-EXPIRE-DT             PIC 9(11) COMP-3.        ECS027
00224      12  FILLER                          PIC X(40).               ECS027
00225      12  WS-AM-EFFECT-DT                 PIC 9(11) COMP-3.        ECS027
00226      12  FILLER                          PIC X(12).               ECS027
00227      12  WS-AM-REPORT-CODE-1             PIC X(10).               ECS027
00228      12  WS-AM-REPORT-CODE-2             PIC X(10).               ECS027
00229      12  FILLER                          PIC X(10).               ECS027
00230      12  WS-AM-NAME                      PIC X(30).               ECS027
00231      12  FILLER                          PIC X(110).              ECS027
00232      12  WS-AM-COMM-STRUCTURE.                                    ECS027
00233          16  WS-AM-DEFN-1.                                        ECS027
00234              20  WS-AM-AGT-COMMS     OCCURS 10 TIMES.             ECS027
00235                  24  WS-AM-AGT.                                   ECS027
00236                      28  WS-AM-AGT-PREFIX    PIC X(4).            ECS027
00237                      28  WS-AM-AGT-PRIME     PIC X(6).            ECS027
00238                  24  WS-AM-COM-TYP           PIC X.               ECS027
00239                  24  FILLER                  PIC X(15).           ECS027
00240      12  FILLER                          PIC X(7).                ECS027
00241      12  WS-AM-GPCD                      PIC 99.                  ECS027
00242      12  FILLER                          PIC X.                   ECS027
00243      12  WS-AM-STATUS                    PIC X.                   ECS027
00244      12  FILLER                          PIC X(45).               ECS027
00245      12  WS-AM-USER-FIELDS.                                       ECS027
00246          16  WS-AM-FLD-1                 PIC XX.                  ECS027
00247          16  FILLER                      PIC X(8).                ECS027
00248      12  FILLER                          PIC X(6).                ECS027
00249      12  WS-AM-ANNIVERSARY-DATE          PIC 9(11)  COMP-3.          CL*19
00250      12  FILLER                          PIC X(73).               ECS027
00251      12  WS-AM-REI-GROUP-A               PIC X(6).                ECS027
00252      12  FILLER                          PIC X(40).               ECS027
00253      12  WS-AM-LF-RET                    PIC S9V9999    COMP-3.   ECS027
00254      12  WS-AM-AH-RET                    PIC S9V9999    COMP-3.   ECS027
00255      12  WS-AM-RET-GRP                   PIC X(6).                ECS027
00256      12  FILLER                          PIC X(1268).             ECS027
00257                                                                   ECS027
00258  COPY ELCDATE.                                                       CL*17
00259                                                                   ECS027
00260  EJECT                                                            ECS027
00261  01  CLAIM-EXTRACT.                                               ECS027
00262      12  CLAIM-EXTRACT-CONTROL.                                   ECS027
00263          16  CLM-ACCOUNT-CONTROL.                                 ECS027
00264              20  CLM-CARRIER             PIC X.                   ECS027
00265              20  CLM-GROUP               PIC X(6).                ECS027
00266              20  CLM-STATE               PIC XX.                  ECS027
00267              20  CLM-ACCOUNT             PIC X(10).               ECS027
00268          16  FILLER.                                              ECS027
00269              20  CLM-EFF-DATE            PIC 9(11) COMP-3.           CL*11
00270              20  CLM-PAID-DATE           PIC 9(11) COMP-3.           CL*11
00271      12  CLM-CLAIM-EXTR-DATA.                                     ECS027
00272          16  CLM-CLAIM-TYPE      PIC X.                           ECS027
00273              88  CLM-DTH                         VALUE '1', '3'.  ECS027
00274              88  CLM-AH                          VALUE '2', '4'.  ECS027
00275          16  CLM-AMT-PAID        PIC S9(7)V99              COMP-3.ECS027
00276          16  CLM-ORIG-TERM       PIC 999.                         ECS027
00277          16  CLM-ORIG-AGE        PIC 99.                          ECS027
00278  EJECT                                                            ECS027
00279                                  COPY ECSEPC01.                   ECS027
00280  EJECT                                                            ECS027
00281                                  COPY ECSEXT01.                   ECS027
00282  EJECT                                                            ECS027
00283                                  COPY ECSPSE01.                   ECS027
00284  EJECT                                                            ECS027
00285  01  SCD-SUMMARY-CONTROL-DATA.                                    ECS027
00286      12  SCD-CARD-1-DATA.                                         ECS027
00287          16  SCD-CLIENT-ID-1                 PIC XXX.             ECS027
00288          16  SCD-CARD-CODE-1                 PIC X.               ECS027
00289          16  SCD-STANDARD-REQUESTS.                               ECS027
00290              20  FILLER      OCCURS 11 TIMES                      ECS027
00291                      INDEXED BY SCDSR.                            ECS027
00292                  24  SCD-STD-REQ             PIC X.               ECS027
00293                      88  SCD-STD-VALID           VALUE ' ', 'X'.  ECS027
00294          16  SCD-STD-N-DYN-LIMITERS.                              ECS027
00295              20  SCD-ACCT-DT-RANGE-INDIC     PIC X.               ECS027
00296              20  SCD-GRAPH-REG               PIC X.               ECS027
00297                  88  SCD-GRAPH-OUT               VALUE ' ', 'Y'.  ECS027
00298                  88  SCD-NO-GRAPH                VALUE 'N'.       ECS027
00299              20  SCD-TERM-GROUPING.                               ECS027
00300                  24  SCD-TERM-1.                                  ECS027
00301                      28  SCD-TERM-1-HIGH     PIC 99.              ECS027
00302                  24  SCD-TERM-2.                                  ECS027
00303                      28  SCD-TERM-2-LOW      PIC 99.              ECS027
00304                      28  SCD-TERM-2-HIGH     PIC 99.              ECS027
00305                  24  SCD-TERM-3.                                  ECS027
00306                      28  SCD-TERM-3-LOW      PIC 99.              ECS027
00307                      28  SCD-TERM-3-HIGH     PIC 99.              ECS027
00308                  24  SCD-TERM-4.                                  ECS027
00309                      28  SCD-TERM-4-LOW      PIC 99.              ECS027
00310              20  SCD-AGE-GROUPING.                                ECS027
00311                  24  SCD-AGE-1.                                   ECS027
00312                      28  SCD-AGE-1-HIGH      PIC 99.              ECS027
00313                  24  SCD-AGE-2.                                   ECS027
00314                      28  SCD-AGE-2-LOW       PIC 99.              ECS027
00315                      28  SCD-AGE-2-HIGH      PIC 99.              ECS027
00316                  24  SCD-AGE-3.                                   ECS027
00317                      28  SCD-AGE-3-LOW       PIC 99.              ECS027
00318                      28  SCD-AGE-3-HIGH      PIC 99.              ECS027
00319                  24  SCD-AGE-4.                                   ECS027
00320                      28  SCD-AGE-4-LOW       PIC 99.              ECS027
00321                      28  SCD-AGE-4-HIGH      PIC 99.              ECS027
00322                  24  SCD-AGE-5.                                   ECS027
00323                      28  SCD-AGE-5-LOW       PIC 99.              ECS027
00324  EJECT                                                            ECS027
00325      12  SCD-CARD-2-DATA.                                         ECS027
00326          16  SCD-DYNAMIC-REQUESTS.                                ECS027
00327              20  SCD-CLIENT-ID-2             PIC XXX.             ECS027
00328              20  SCD-CARD-CODE-2             PIC X.               ECS027
00329              20  SCD-DYN-HEIRARCHY-LIST.                          ECS027
00330                  24  SCD-H-RPT-CD-1          PIC 9.               ECS027
00331                  24  SCD-H-CARRIER           PIC 9.               ECS027
00332                  24  SCD-H-GROUP             PIC 9.               ECS027
00333                  24  SCD-H-RPT-CD-2          PIC 9.               ECS027
00334                  24  SCD-H-STATE             PIC 9.               ECS027
00335                  24  SCD-H-RETRO-GROUP       PIC 9.               ECS027
00336                  24  SCD-H-ACCOUNT           PIC 9.               ECS027
00337                  24  SCD-H-REGION            PIC 9.               ECS027
00338                  24  SCD-H-BUSINESS-TYPE     PIC 9.               ECS027
00339              20  FILLER REDEFINES SCD-DYN-HEIRARCHY-LIST.         ECS027
00340                  24  FILLER  OCCURS 9 TIMES                       ECS027
00341                          INDEXED BY SCDDH.                        ECS027
00342                      28  SCD-D-HEIRARCHY-X.                       ECS027
00343                          32  SCD-D-HEIRARCHY PIC 9.               ECS027
00344                              88  SCD-DH-VALID    VALUE 1, 2, 3,   ECS027
00345                                                        4, 5, 6,   ECS027
00346                                                        7, 8, 9.   ECS027
00347              20  SCD-DYNAMIC-LIMITERS.                            ECS027
00348                  24  SCD-L-RPT-CD-1          PIC X(10).           ECS027
00349                  24  SCD-L-CARRIER           PIC X.               ECS027
00350                  24  SCD-L-GROUP             PIC X(6).            ECS027
00351                  24  SCD-L-RPT-CD-2          PIC X(10).           ECS027
00352                  24  SCD-L-STATE             PIC XX.              ECS027
00353                  24  SCD-L-RETRO-GROUP       PIC X(6).            ECS027
00354                  24  SCD-L-ACCOUNT           PIC X(10).           ECS027
00355                  24  SCD-L-REGION            PIC XX.              ECS027
00356                  24  SCD-BUSINESS-TYPE.                           ECS027
00357                      28  SCD-BUS-TYPE        PIC 99.              ECS027
00358                  24  SCD-CERT-ISSUE-DT-RANGE.                     ECS027
00359                      28  SCD-CID-LOW         PIC 9(11) COMP-3.       CL*12
00360                      28  SCD-CID-HIGH        PIC 9(11) COMP-3.       CL*12
00361      12  SCD-SYSTEM-GENERATED-DATA.                               ECS027
00362          16  SCD-ROLLING-12-MONTHS.                               ECS027
00363              20  FILLER      OCCURS 13 TIMES                      ECS027
00364                      INDEXED BY SCD12.                            ECS027
00365                  24  SCD-R12-CCYY            PIC 9(04).           ECS027
00366                  24  SCD-R12-CCYR REDEFINES                       ECS027
00367                      SCD-R12-CCYY.                                ECS027
00368                      28  SCD-R12-CC          PIC 99.              ECS027
00369                      28  SCD-R12-YEAR        PIC 99.              ECS027
00370                  24  SCD-R12-MONTH           PIC 99.                 CL*11
00371          16  SCD-RUN-DATE                    PIC 9(11)  COMP-3.      CL*11
00372                                                                      CL*11
00373  EJECT                                                            ECS027
00374  01  HOLD-AMOUNTS.                                                ECS027
00375      12  HOLD-COUNT          PIC S9(7)       COMP-3 VALUE ZEROS.  ECS027
00376      12  HOLD-PREM           PIC S9(9)V99    COMP-3 VALUE ZEROS.  ECS027
00377      12  HOLD-EARNED         PIC S9(9)V99    COMP-3 VALUE ZEROS.  ECS027
00378      12  HOLD-COMMIS         PIC S9(9)V99    COMP-3 VALUE ZEROS.  ECS027
00379      12  HOLD-CLM-INCUR      PIC S9(9)V99    COMP-3 VALUE ZEROS.  ECS027
00380      12  HOLD-CLM-PD         PIC S9(9)V99    COMP-3 VALUE ZEROS.  ECS027
00381      12  HOLD-COUNT-A        PIC S9(7)       COMP-3 VALUE ZEROS.  ECS027
00382      12  HOLD-PREM-A         PIC S9(9)V99    COMP-3 VALUE ZEROS.  ECS027
00383      12  HOLD-EARNED-A       PIC S9(9)V99    COMP-3 VALUE ZEROS.  ECS027
00384      12  HOLD-COMMIS-A       PIC S9(9)V99    COMP-3 VALUE ZEROS.  ECS027
00385      12  HOLD-CLM-INCUR-A    PIC S9(9)V99    COMP-3 VALUE ZEROS.  ECS027
00386      12  HOLD-CLM-PD-A       PIC S9(9)V99    COMP-3 VALUE ZEROS.  ECS027
00387                                                                   ECS027
00388  01  WSD-MONTH-ARRAY-AREA.                                        ECS027
00389      12  WSD-MONTH-ARRAY     OCCURS  13 TIMES                     ECS027
00390              INDEXED BY WSDM.                                     ECS027
00391 **       THE 1ST OCCURRENCE HOLDS THE ITD SUMS FOR THE MONTH    **ECS027
00392 **       PRIOR TO THE 1ST MONTH OF THE ROLLING 12 MONTHS.       **ECS027
00393 **       THUS THE FIRST QUARTER IS CALCULATED TO BE OCCURRENCE 4 *ECS027
00394 **                                             LESS OCCURRENCE 1.*ECS027
00395          16  WSD-L-COUNT         PIC S9(7)       COMP-3.          ECS027
00396          16  WSD-L-PREM          PIC S9(9)V99    COMP-3.          ECS027
00397          16  WSD-L-EARN-PREM     PIC S9(9)V99    COMP-3.          ECS027
00398          16  WSD-L-COMMIS        PIC S9(9)V99    COMP-3.          ECS027
00399          16  WSD-L-CLM-INCUR     PIC S9(9)V99    COMP-3.          ECS027
00400          16  WSD-L-CLM-PAID      PIC S9(9)V99    COMP-3.          ECS027
00401          16  WSD-A-COUNT         PIC S9(7)       COMP-3.          ECS027
00402          16  WSD-A-PREM          PIC S9(9)V99    COMP-3.          ECS027
00403          16  WSD-A-EARN-PREM     PIC S9(9)V99    COMP-3.          ECS027
00404          16  WSD-A-COMMIS        PIC S9(9)V99    COMP-3.          ECS027
00405          16  WSD-A-CLM-INCUR     PIC S9(9)V99    COMP-3.          ECS027
00406          16  WSD-A-CLM-PAID      PIC S9(9)V99    COMP-3.          ECS027
00407                                                                   ECS027
00408  01  HOLD-CLAS-TYPE.                                              ECS027
00409      12  HOLD-CLAS-I-BEN         PIC XX.                          ECS027
00410      12  HOLD-CLAS-I-AB3.                                         ECS027
00411          16  FILLER              PIC X.                           ECS027
00412          16  HOLD-CLAS-I-AB2     PIC XX.                          ECS027
00413      12  HOLD-CLAS-I-AB10        PIC X(10).                       ECS027
00414      12  HOLD-CLAS-I-JOINT       PIC X.                           ECS027
00415      12  HOLD-CLAS-I-RL-AH       PIC X.                           ECS027
00416      12  HOLD-CLAS-I-BAL         PIC X.                           ECS027
00417      12  HOLD-CLAS-I-EP          PIC X.                           ECS027
00418                                                                   ECS027
00419  01  REMIT-TO-AREA.                                               ECS027
00420      12  REMIT-TO-9          PIC 99.                              ECS027
00421  EJECT                                                            ECS027
00422  01  ACCUMULATION-CONTROL-WORK.                                   ECS027
00423      12  THE-TERM-X.                                              ECS027
00424          16  THE-TERM        PIC 9(3).                            ECS027
00425      12  THE-AGE             PIC 99.                              ECS027
00426                                                                   ECS027
00427  01  HOLD-ACCT-DATA.                                              ECS027
00428      12  H-ACCT-KEY.                                              ECS027
00429          16  H-ACCT-CARRIER      PIC X.                           ECS027
00430          16  H-ACCT-GROUP        PIC X(6).                        ECS027
00431          16  H-ACCT-STATE        PIC XX.                          ECS027
00432          16  H-ACCT-NUMBER       PIC X(10).                       ECS027
00433      12  H-ACCT-EXPIR-DATE       PIC 9(11) COMP-3.                   CL*11
00434                                                                   ECS027
00435  01  HOLD-CLAIM-ACCT-KEY.                                         ECS027
00436      12  H-CLM-ACCT-KEY.                                          ECS027
00437          16  H-CLM-ACCT-CARR         PIC X.                       ECS027
00438          16  H-CLM-ACCT-GROUP        PIC X(6).                    ECS027
00439          16  H-CLM-ACCT-STATE        PIC XX.                      ECS027
00440          16  H-CLM-ACCT-NUMBER.                                   ECS027
00441              20  H-CLM-ACCT-REGION   PIC XX.                      ECS027
00442              20  FILLER              PIC X(8).                    ECS027
00443      12  H-CLM-EFFECT-DATE           PIC 9(11) COMP-3.            ECS027
00444                                                                   ECS027
00445  01  HOLD-EPEC-ACCT-KEY.                                          ECS027
00446      12  H-EPEC-ACCT-KEY.                                         ECS027
00447          16  H-EPEC-ACCT-CARR        PIC X.                       ECS027
00448          16  H-EPEC-ACCT-GROUP       PIC X(6).                    ECS027
00449          16  H-EPEC-ACCT-STATE       PIC XX.                      ECS027
00450          16  H-EPEC-ACCT-NUMBER.                                  ECS027
00451              20  H-EPEC-ACCT-REGION  PIC XX.                      ECS027
00452              20  FILLER              PIC X(8).                    ECS027
00453      12  H-EPEC-DATE                 PIC 9(11) COMP-3.               CL*11
00454                                                                   ECS027
00455  01  HOLD-NEW-ACCT-DATA.                                          ECS027
00456      12  H-NEW-ACCT-KEY.                                          ECS027
00457          16  H-NEW-ACCT-CARRIER      PIC X.                       ECS027
00458          16  H-NEW-ACCT-GROUP        PIC X(6).                    ECS027
00459          16  H-NEW-ACCT-STATE        PIC XX.                      ECS027
00460          16  H-NEW-ACCT-NUMBER       PIC X(10).                   ECS027
00461      12  H-NEW-ACCT-EXPIR-DATE       PIC 9(11) COMP-3.               CL*11
00462                                                                   ECS027
00463  EJECT                                                            ECS027
00464  01  DATE-WORK-AREAS.                                                CL*11
00465      12  DATE-WORK.                                                  CL*11
00466          16  FILLER          PIC 999.                             ECS027
00467          16  DATE-CCYY       PIC 9(04).                           ECS027
00468          16  DATE-CCYR REDEFINES DATE-CCYY.                       ECS027
00469              20  DATE-CC     PIC 99.                              ECS027
00470              20  DATE-YR     PIC 99.                              ECS027
00471          16  DATE-MO         PIC 99.                                 CL**4
00472          16  DATE-DA         PIC 99.                                 CL**4
00473      12  DATE-WORK-N  REDEFINES                                      CL*11
00474          DATE-WORK           PIC 9(11).                              CL*11
00475                                                                   ECS027
00476  01  SORT-KEY-ARRAY.                                              ECS027
00477      12  FILLER          OCCURS 9 TIMES                           ECS027
00478              INDEXED BY SKA.                                      ECS027
00479          16  SKA-REPORT-ID       PIC X.                           ECS027
00480          16  SKA-KEY.                                             ECS027
00481              20  SKA-HEIR-1      PIC X(10).                       ECS027
00482              20  SKA-HEIR-2      PIC X(10).                       ECS027
00483              20  SKA-HEIR-3      PIC X(10).                       ECS027
00484              20  SKA-HEIR-4      PIC X(10).                       ECS027
00485              20  SKA-HEIR-5      PIC X(10).                       ECS027
00486              20  SKA-HEIR-6      PIC X(10).                       ECS027
00487              20  SKA-HEIR-7      PIC X(10).                       ECS027
00488              20  SKA-HEIR-8      PIC X(10).                       ECS027
00489              20  SKA-HEIR-9      PIC X(10).                       ECS027
00490          16  SKA-KEY-OCCURS REDEFINES SKA-KEY.                    ECS027
00491              20  FILLER  OCCURS 9 TIMES                           ECS027
00492                      INDEXED BY SKA2.                             ECS027
00493                  24  SKA-HEIR    PIC X(10).                       ECS027
00494  EJECT                                                            ECS027
00495  01  ERROR-WORK-AREA.                                             ECS027
00496      12  ERROR-INDICATOR     PIC X               VALUE 'N'.       ECS027
00497          88  EDITS-FAILED                        VALUE 'Y'.       ECS027
00498          88  EDITS-PASSED                        VALUE 'N'.       ECS027
00499      12  ERROR-DESCRIPTIONS.                                      ECS027
00500          16  DESC01          PIC X(25)           VALUE            ECS027
00501                  'NO TRANSACTION RECEIVED  '.                     ECS027
00502          16  DESC02          PIC X(25)           VALUE            ECS027
00503                  'UNAUTHORIZED REQUST      '.                     ECS027
00504          16  DESC03          PIC X(25)           VALUE            ECS027
00505                  'NO DATA IN REQUEST       '.                     ECS027
00506          16  DESC04          PIC X(25)           VALUE            ECS027
00507                  'REQUEST TYPE NOT KNOWN   '.                     ECS027
00508          16  DESC05          PIC X(25)           VALUE            ECS027
00509                  'STANDARD AND DYNAMIC REQ.'.                     ECS027
00510          16  DESC06          PIC X(25)           VALUE            ECS027
00511                  'MUST BE- X OR SPACE      '.                     ECS027
00512          16  DESC07          PIC X(25)           VALUE            ECS027
00513                  'UNKNOWN BUSINESS TYPE    '.                     ECS027
00514          16  DESC08          PIC X(25)           VALUE            ECS027
00515                  'MUST BE- G, I, B OR BLANK'.                     ECS027
00516          16  DESC09          PIC X(25)           VALUE            ECS027
00517                  'BEGIN DATE NOT GIVEN     '.                     ECS027
00518          16  DESC10          PIC X(25)           VALUE            ECS027
00519                  'END DATE NOT GIVEN       '.                     ECS027
00520          16  DESC11          PIC X(25)           VALUE            ECS027
00521                  'MUST BE-9.S OR VALID DATE'.                     ECS027
00522          16  DESC12          PIC X(25)           VALUE            ECS027
00523                  'BEGIN DATE AFTER END DATE'.                     ECS027
00524          16  DESC13          PIC X(25)           VALUE            ECS027
00525                  'DATE NOT NUMERIC         '.                     ECS027
00526          16  DESC14          PIC X(25)           VALUE            ECS027
00527                  'TERM VALUE NOT NUMERIC   '.                     ECS027
00528          16  DESC15          PIC X(25)           VALUE            ECS027
00529                  'IMBEDDED BLANK TERM GROUP'.                     ECS027
00530          16  DESC16          PIC X(25)           VALUE            ECS027
00531                  'TERM VALUES NOT TOGETHER '.                     ECS027
00532          16  DESC17          PIC X(25)           VALUE            ECS027
00533                  'TERM GROUP INCOMPLETE    '.                     ECS027
00534          16  DESC18          PIC X(25)           VALUE            ECS027
00535                  'TERM OVERLAP             '.                     ECS027
00536          16  DESC19          PIC X(25)           VALUE            ECS027
00537                  'AGE VALUE NOT NUMERIC    '.                     ECS027
00538          16  DESC20          PIC X(25)           VALUE            ECS027
00539                  'IMBEDDED BLANK AGE GROUP '.                     ECS027
00540          16  DESC21          PIC X(25)           VALUE            ECS027
00541                  'AGE VALUES NOT TOGETHER  '.                     ECS027
00542          16  DESC22          PIC X(25)           VALUE            ECS027
00543                  'AGE GROUP INCOMPLETE     '.                     ECS027
00544          16  DESC23          PIC X(25)           VALUE            ECS027
00545                  'AGE OVERLAP              '.                     ECS027
00546          16  DESC24          PIC X(25)           VALUE            ECS027
00547                  'MUST BE - 1 THRU 7       '.                     ECS027
00548          16  DESC25          PIC X(25)           VALUE            ECS027
00549                  'DUPLICATE HEIRARCHY(IES) '.                     ECS027
00550          16  DESC26          PIC X(25)           VALUE            ECS027
00551                  'INVALID STATE            '.                     ECS027
00552          16  DESC27          PIC X(25)           VALUE            ECS027
00553                  'INVALID DATE             '.                     ECS027
00554          16  DESC28          PIC X(25)           VALUE            ECS027
00555                  'MUST BE - Y, N OR BLANK  '.                     ECS027
00556          16  DESC29          PIC X(25)           VALUE            ECS027
00557                  'MUST BE BLANK, G, B OR N '.                     ECS027
00558          16  DESC30          PIC X(25)           VALUE            ECS027
00559                  'ACCT LEVEL NOT REQUESTED '.                     ECS027
00560      12  ERR-MSG.                                                 ECS027
00561          16  FILLER          PIC X(3)            VALUE '** '.     ECS027
00562          16  ERR-MSG-PRT     PIC X(25)           VALUE SPACES.    ECS027
00563                                                                   ECS027
00564  EJECT                                                            ECS027
00565  01  PRT-ERR-HDR-1.                                               ECS027
00566      12  FILLER              PIC X(52)           VALUE '1'.       ECS027
00567      12  FILLER              PIC X(31)           VALUE            ECS027
00568                                'PRODUCTION SUMMARY ERROR REPORT'. ECS027
00569      12  FILLER              PIC X(37)           VALUE SPACES.    ECS027
00570      12  FILLER              PIC X(10)           VALUE 'ECS027'.  ECS027
00571                                                                   ECS027
00572  01  PRT-ERR-HDR-2.                                               ECS027
00573      12  FILLER              PIC X(52)           VALUE ' '.       ECS027
00574      12  EH2-CO-NAME         PIC X(30).                           ECS027
00575      12  FILLER              PIC X(38)           VALUE SPACES.    ECS027
00576      12  EH2-RUN-DATE.                                            ECS027
00577          16  EH2-RUN-MO      PIC 99.                              ECS027
00578          16  FILLER          PIC X               VALUE '-'.       ECS027
00579          16  EH2-RUN-DAY     PIC 99.                              ECS027
00580          16  FILLER          PIC X               VALUE '-'.       ECS027
00581          16  EH2-RUN-YR      PIC 99.                              ECS027
00582                                                                   ECS027
00583  01  PRT-ERR-HDR-3.                                               ECS027
00584      12  FILLER              PIC X               VALUE '0'.       ECS027
00585      12  FILLER              PIC X(24)           VALUE            ECS027
00586              '   STANDARD REQUEST DATA'.                          ECS027
00587      12  FILLER              PIC X(78)           VALUE SPACES.    ECS027
00588      12  EH3-STD-DATA-MSG    PIC X(28)           VALUE SPACES.    ECS027
00589                                                                   ECS027
00590  01  PRT-ERR-HDR-4.                                               ECS027
00591      12  FILLER              PIC X               VALUE '0'.       ECS027
00592      12  FILLER              PIC X(32)           VALUE            ECS027
00593              '   STANDARD & DYNAMIC PARAMETERS'.                  ECS027
00594                                                                   ECS027
00595  01  PRT-ERR-HDR-5.                                               ECS027
00596      12  FILLER              PIC X               VALUE '0'.       ECS027
00597      12  FILLER              PIC X(23)           VALUE            ECS027
00598              '   DYNAMIC REQUEST DATA'.                           ECS027
00599                                                                   ECS027
00600  01  PRT-ERR-HDR-6.                                               ECS027
00601      12  FILLER                  PIC X           VALUE '0'.       ECS027
00602      12  FILLER                  PIC X(23)       VALUE            ECS027
00603                                  '      EDITABLE LIMITERS'.       ECS027
00604  EJECT                                                            ECS027
00605  01  PRT-ERR-DTL-1A.                                              ECS027
00606      12  FILLER                  PIC X           VALUE '0'.       ECS027
00607      12  FILLER                  PIC X(14)       VALUE            ECS027
00608                                            '      ACCOUNT-'.      ECS027
00609      12  ED1-STD-ACCT-INDIC      PIC X.                           ECS027
00610      12  FILLER                  PIC X(17)       VALUE            ECS027
00611                                          '   GENERAL AGENT-'.     ECS027
00612      12  ED1-STD-GA-INDIC        PIC X.                           ECS027
00613      12  FILLER                  PIC X(13)       VALUE            ECS027
00614                                              'ACCOUNT/G.A.-'.     ECS027
00615      12  ED1-STD-ACCT-GA-INDIC   PIC X.                           ECS027
00616      12  FILLER                  PIC X(9)        VALUE            ECS027
00617                                                  '   STATE-'.     ECS027
00618      12  ED1-STD-STATE-INDIC     PIC X.                           ECS027
00619      12  FILLER                  PIC X(11)       VALUE            ECS027
00620                                                 '   CARRIER-'.    ECS027
00621      12  ED1-STD-CARR-INDIC      PIC X.                           ECS027
00622      12  FILLER                  PIC X(9)        VALUE            ECS027
00623                                                   '   GROUP-'.    ECS027
00624      12  ED1-STD-GRP-INDIC       PIC X.                           ECS027
00625      12  FILLER                  PIC X(15)       VALUE            ECS027
00626                                             '   RETRO GROUP-'.    ECS027
00627      12  ED1-STD-RETRO-GRP-INDIC PIC X.                           ECS027
00628      12  FILLER                  PIC X(10)       VALUE            ECS027
00629                                                  '   REGION-'.    ECS027
00630      12  ED1-STD-REGION-INDIC    PIC X.                           ECS027
00631      12  FILLER                  PIC X(13)       VALUE            ECS027
00632                                                '   BUS. TYPE-'.   ECS027
00633      12  ED1-STD-BUS-TY-INDIC    PIC X.                           ECS027
00634      12  FILLER                  PIC X(12)       VALUE SPACES.    ECS027
00635      12  ED1-STD-RPT-CD-1-INDIC  PIC X.                           ECS027
00636      12  FILLER                  PIC X(14)       VALUE            ECS027
00637                                               'REPORT CODE 1-'.   ECS027
00638      12  ED1-STD-RPT-CD-2-INDIC  PIC X.                           ECS027
00639      12  FILLER                  PIC X(14)       VALUE            ECS027
00640                                               'REPORT CODE 2-'.   ECS027
00641                                                                   ECS027
00642  01  PRT-ERR-DTL-1B.                                              ECS027
00643      12  FILLER                  PIC X(104)      VALUE ' '.       ECS027
00644      12  ED1-STD-ERR-MSG         PIC X(28)       VALUE SPACES.    ECS027
00645                                                                   ECS027
00646  01  PRT-ERR-DTL-2.                                               ECS027
00647      12  FILLER                  PIC X           VALUE '0'.       ECS027
00648      12  FILLER                  PIC X(35)       VALUE            ECS027
00649              '      REPORT BY ACCOUNT DATE RANGE-'.               ECS027
00650      12  ED2-ACCT-DT-RANGE-INDIC PIC X.                           ECS027
00651      12  FILLER                  PIC X(67)       VALUE SPACES.    ECS027
00652      12  ED2-ACCT-DT-RANGE-MSG   PIC X(28)       VALUE SPACES.    ECS027
00653  EJECT                                                            ECS027
00654  01  PRT-ERR-DTL-4.                                               ECS027
00655      12  FILLER                  PIC X           VALUE ' '.       ECS027
00656      12  FILLER                  PIC X(35)       VALUE            ECS027
00657              '           GROSS &/OR NET OF REINS-'.               ECS027
00658      12  ED4-GROSS-NET-INDIC     PIC X.                           ECS027
00659      12  FILLER                  PIC X(67)       VALUE SPACES.    ECS027
00660      12  ED4-GROSS-NET-MSG       PIC X(28)       VALUE SPACES.    ECS027
00661                                                                   ECS027
00662  01  PRT-ERR-DTL-5.                                               ECS027
00663      12  FILLER                  PIC X           VALUE ' '.       ECS027
00664      12  FILLER                  PIC X(35)       VALUE            ECS027
00665              '      TERM GROUPINGS              -'.               ECS027
00666      12  FILLER.                                                  ECS027
00667          16  FILLER              PIC XX          VALUE '1-'.      ECS027
00668          16  ED5-TG-1-HIGH       PIC ZZ          VALUE ZEROS.     ECS027
00669          16  FILLER              PIC XX          VALUE SPACES.    ECS027
00670          16  ED5-TG-2-LOW        PIC ZZ          VALUE ZEROS.     ECS027
00671          16  FILLER              PIC X           VALUE '-'.       ECS027
00672          16  ED5-TG-2-HIGH       PIC ZZ          VALUE ZEROS.     ECS027
00673          16  FILLER              PIC XX          VALUE SPACES.    ECS027
00674          16  ED5-TG-3-LOW        PIC ZZ          VALUE ZEROS.     ECS027
00675          16  FILLER              PIC X           VALUE '-'.       ECS027
00676          16  ED5-TG-3-HIGH       PIC ZZ          VALUE ZEROS.     ECS027
00677          16  FILLER              PIC XX          VALUE SPACES.    ECS027
00678          16  ED5-TG-4-LOW        PIC ZZ          VALUE ZEROS.     ECS027
00679          16  FILLER              PIC X           VALUE '+'.       ECS027
00680      12  FILLER                  PIC X(45)       VALUE SPACES.    ECS027
00681      12  ED5-TERM-GRP-MSG        PIC X(28)       VALUE SPACES.    ECS027
00682                                                                   ECS027
00683  01  PRT-ERR-DTL-6.                                               ECS027
00684      12  FILLER                  PIC X           VALUE ' '.       ECS027
00685      12  FILLER                  PIC X(35)       VALUE            ECS027
00686              '      AGE  GROUPINGS              -'.               ECS027
00687      12  FILLER.                                                  ECS027
00688          16  FILLER              PIC XX          VALUE '1-'.      ECS027
00689          16  ED6-AG-1-HIGH       PIC ZZ          VALUE ZEROS.     ECS027
00690          16  FILLER              PIC XX          VALUE SPACES.    ECS027
00691          16  ED6-AG-2-LOW        PIC ZZ          VALUE ZEROS.     ECS027
00692          16  FILLER              PIC X           VALUE '-'.       ECS027
00693          16  ED6-AG-2-HIGH       PIC ZZ          VALUE ZEROS.     ECS027
00694          16  FILLER              PIC XX          VALUE SPACES.    ECS027
00695          16  ED6-AG-3-LOW        PIC ZZ          VALUE ZEROS.     ECS027
00696          16  FILLER              PIC X           VALUE '-'.       ECS027
00697          16  ED6-AG-3-HIGH       PIC ZZ          VALUE ZEROS.     ECS027
00698          16  FILLER              PIC XX          VALUE SPACES.    ECS027
00699          16  ED6-AG-4-LOW        PIC ZZ          VALUE ZEROS.     ECS027
00700          16  FILLER              PIC X           VALUE '-'.       ECS027
00701          16  ED6-AG-4-HIGH       PIC ZZ          VALUE ZEROS.     ECS027
00702          16  FILLER              PIC XX          VALUE SPACES.    ECS027
00703          16  ED6-AG-5-LOW        PIC ZZ          VALUE ZEROS.     ECS027
00704          16  FILLER              PIC X           VALUE '+'.       ECS027
00705      12  FILLER                  PIC X(45)       VALUE SPACES.    ECS027
00706      12  ED6-AGE-GRP-MSG         PIC X(28)       VALUE SPACES.    ECS027
00707  EJECT                                                            ECS027
00708  01  PRT-ERR-DTL-7.                                               ECS027
00709      12  FILLER                  PIC X           VALUE '0'.       ECS027
00710      12  FILLER                  PIC X(14)       VALUE            ECS027
00711              '      CARRIER-'.                                    ECS027
00712      12  ED7-CARR-HEIRARCHY      PIC X.                           ECS027
00713      12  FILLER                  PIC X(9)        VALUE            ECS027
00714              '   GROUP-'.                                         ECS027
00715      12  ED7-GROUP-HEIRARCHY     PIC X.                           ECS027
00716      12  FILLER                  PIC X(9)        VALUE            ECS027
00717              '   STATE-'.                                         ECS027
00718      12  ED7-STATE-HEIRARCHY     PIC X.                           ECS027
00719      12  FILLER                  PIC X(13)       VALUE            ECS027
00720              '   ACCOUNT  -'.                                     ECS027
00721      12  ED7-ACCT-HEIRARCHY      PIC X.                           ECS027
00722      12  FILLER                  PIC X(10)       VALUE            ECS027
00723              '   REGION-'.                                        ECS027
00724      12  ED7-REGION-HEIRARCHY    PIC X.                           ECS027
00725      12  FILLER                  PIC X(13)       VALUE            ECS027
00726              '   BUS. TYPE-'.                                     ECS027
00727      12  ED7-BUS-TY-HEIRARCHY    PIC X.                           ECS027
00728      12  FILLER                  PIC X(20)        VALUE SPACES.   ECS027
00729      12  ED7-HEIRARCHY-MSG       PIC X(28)       VALUE SPACES.    ECS027
00730                                                                   ECS027
00731  01  PRT-ERR-DTL-8.                                               ECS027
00732      12  FILLER                  PIC X           VALUE ' '.       ECS027
00733      12  FILLER                  PIC X(24)       VALUE SPACES.    ECS027
00734      12  FILLER                  PIC X(16)       VALUE            ECS027
00735              'STATE          -'.                                  ECS027
00736      12  ED8-STATE-LIMITER       PIC XX          VALUE SPACES.    ECS027
00737      12  FILLER                  PIC X(61)       VALUE SPACES.    ECS027
00738      12  ED8-ST-LIMIT-MSG        PIC X(28)       VALUE SPACES.    ECS027
00739                                                                   ECS027
00740  01  PRT-ERR-DTL-9.                                               ECS027
00741      12  FILLER                  PIC X           VALUE ' '.       ECS027
00742      12  FILLER                  PIC X(24)       VALUE SPACES.    ECS027
00743      12  FILLER                  PIC X(16)       VALUE            ECS027
00744              'BUS. TYPE      -'.                                  ECS027
00745      12  ED9-BUS-TY-LIMITER      PIC XX          VALUE SPACES.    ECS027
00746      12  FILLER                  PIC X(61)       VALUE SPACES.    ECS027
00747      12  ED9-BUS-TY-LIMIT-MSG    PIC X(28)       VALUE SPACES.    ECS027
00748  EJECT                                                            ECS027
00749  01  PRT-ERR-DTL-10.                                              ECS027
00750      12  FILLER                  PIC X           VALUE ' '.       ECS027
00751      12  FILLER                  PIC X(24)       VALUE SPACES.    ECS027
00752      12  FILLER                  PIC X(16)       VALUE            ECS027
00753              'CERT EFFEC FROM-'.                                  ECS027
00754      12  ED10-CERT-F-LIMITER     PIC X(6)        VALUE SPACES.    ECS027
00755      12  FILLER                  PIC X(57)       VALUE SPACES.    ECS027
00756      12  ED10-CERT-F-LIMIT-MSG   PIC X(28)       VALUE SPACES.    ECS027
00757                                                                   ECS027
00758  01  PRT-ERR-DTL-11.                                              ECS027
00759      12  FILLER                  PIC X           VALUE ' '.       ECS027
00760      12  FILLER                  PIC X(24)       VALUE SPACES.    ECS027
00761      12  FILLER                  PIC X(16)       VALUE            ECS027
00762              '           THRU-'.                                  ECS027
00763      12  ED11-CERT-T-LIMITER     PIC X(6)        VALUE SPACES.    ECS027
00764      12  FILLER                  PIC X(57)       VALUE SPACES.    ECS027
00765      12  ED11-CERT-T-LIMIT-MSG   PIC X(28)       VALUE SPACES.    ECS027
00766                                                                   ECS027
00767  01  PRT-ERR-DTL-12.                                              ECS027
00768      12  FILLER                  PIC X           VALUE ' '.       ECS027
00769      12  FILLER                  PIC XX          VALUE SPACES.    ECS027
00770      12  FILLER                  PIC X(16)       VALUE            ECS027
00771              'REPORT CODE 1  -'.                                  ECS027
00772      12  ED12-RPT-CD-1-MSG       PIC X(28)       VALUE SPACES.    ECS027
00773                                                                   ECS027
00774  01  PRT-ERR-DTL-13.                                              ECS027
00775      12  FILLER                  PIC X           VALUE ' '.       ECS027
00776      12  FILLER                  PIC XX          VALUE SPACES.    ECS027
00777      12  FILLER                  PIC X(16)       VALUE            ECS027
00778              'REPORT CODE 2  -'.                                  ECS027
00779      12  ED13-RPT-CD-2-MSG       PIC X(28)       VALUE SPACES.    ECS027
00780                                                                   ECS027
00781                                  COPY ELCDTECX.                   ECS027
00782                                                                   ECS027
00783                                  COPY ELCDTEVR.                   ECS027
00784                                                                   ECS027
00785                                  COPY ELCEXTVR.                   ECS027
00786                                                                   ECS027
00787                                  COPY ELCCRTVR.                      CL*14
00788                                                                      CL*14
00789                                  COPY ELCEPCVR.                      CL*14
00790                                                                   ECS027
00791                                  COPY ELCPSEVR.                   ECS027
00792  EJECT                                                            ECS027
00793  PROCEDURE DIVISION.                                              ECS027
00794                                                                   ECS027
00795  0001-MAINLINE.                                                   ECS027
00796      PERFORM 0100-INITIALIZATION     THRU 0100-EXIT.              ECS027
00797                                                                   ECS027
00798      PERFORM 0300-GET-N-EDIT-REQUEST THRU 0300-EXIT.              ECS027
00799                                                                   ECS027
00800      IF EDITS-PASSED                                              ECS027
00801          PERFORM 1000-BUILD-N-SORT-CLAIMS   THRU 1000-EXIT.       ECS027
00802                                                                   ECS027
00803      IF EDITS-PASSED                                              ECS027
00804          PERFORM 2000-BUILD-DETAIL-EXTRACTS THRU 2000-EXIT.       ECS027
00805                                                                   ECS027
00806      IF EDITS-PASSED                                              ECS027
00807          IF DETAIL-EXTRACT-COUNT NOT GREATER +0                   ECS027
00808              DISPLAY 'ECS027 E - NO EXTRACTS GENERATED'           ECS027
00809              PERFORM 9999-ABEND-DISPLAYS.                         ECS027
00810                                                                   ECS027
00811      PERFORM 0200-WRAP-UP THRU 0201-EXIT.                         ECS027
00812                                                                   ECS027
00813      GOBACK.                                                      ECS027
00814                                                                   ECS027
00815  0100-INITIALIZATION.                                             ECS027
00816                              COPY ELCDTERX.                       ECS027
00817                                                                   ECS027
00818      MOVE SPACES                 TO PROD-SUMMARY-DETAIL-EXTR      ECS027
00819                                     HOLD-ACCT-DATA                ECS027
00820                                     HOLD-CLAIM-ACCT-KEY           ECS027
00821                                     HOLD-EPEC-ACCT-KEY            ECS027
00822                                     HOLD-CLAS-TYPE.               ECS027
00823                                                                   ECS027
00824  0100-OPEN-CONTROL-FILE.                                          ECS027
00825      OPEN INPUT ELCNTL.                                           ECS027
00826                                                                   ECS027
00827      IF ELCNTL-FILE-STATUS  = '00' OR '97'                        ECS027
00828          NEXT SENTENCE                                            ECS027
00829        ELSE                                                       ECS027
00830          MOVE '**** ELCNTL OPEN ERROR ****'                       ECS027
00831                                  TO WS-ABEND-MESSAGE              ECS027
00832          MOVE ELCNTL-FILE-STATUS TO WS-ABEND-FILE-STATUS          ECS027
00833          GO TO ABEND-PGM.                                         ECS027
00834                                                                   ECS027
00835  0100-READ-REPORT-RECORD.                                         ECS027
00836      MOVE SPACES                 TO CF-CONTROL-PRIMARY.           ECS027
00837      MOVE DTE-CLIENT             TO CF-COMPANY-ID.                ECS027
00838      MOVE 'C'                    TO CF-RECORD-TYPE.               ECS027
00839      MOVE PGM-SUB                TO CF-CUSTOM-REPORT-NO.          ECS027
00840      MOVE +0                     TO CF-SEQUENCE-NO.               ECS027
00841                                                                   ECS027
00842      READ ELCNTL.                                                 ECS027
00843                                                                   ECS027
00844      IF ELCNTL-FILE-STATUS NOT = '00'                             ECS027
00845          MOVE 'X'                TO WS-CUSTOMIZATION-FLAG.        ECS027
00846                                                                   ECS027
00847  0100-EXIT.                                                       ECS027
00848      EXIT.                                                        ECS027
00849                                                                   ECS027
00850  0200-WRAP-UP.                                                    ECS027
00851 *                                                                 ECS027
00852 *                                                                 ECS027
00853 * LEAVE THE ABOVE 2 LINES IN TO PREVENT LOSING THE LINE ABOVE     ECS027
00862                                                                   ECS027
00863  0201-EXIT.                                                       ECS027
00864      EXIT.                                                        ECS027
00865  EJECT                                                            ECS027
00866  0300-GET-N-EDIT-REQUEST.                                         ECS027
00867                                                                      CL**4
00868      OPEN INPUT PROD-SUM-REQ.                                     ECS027
00869                                                                   ECS027
00870  0300-READ-INPUT-CARD.                                            ECS027
00871      READ PROD-SUM-REQ AT END                                     ECS027
00872          MOVE EOF-TYPE-CODE      TO REQUEST-EOF                   ECS027
00873          GO TO 0300-END-OF-DATA.                                  ECS027
00874                                                                   ECS027
00875      IF DTE-CLIENT NOT = PSR-CLIENT-ID                            ECS027
00876          GO TO 0300-READ-INPUT-CARD.                              ECS027
00877                                                                   ECS027
00878      MOVE 'Y'                    TO EOF-TYPE-CODE.                ECS027
00879                                                                   ECS027
00880      IF PSR-CARD-CODE = '1'                                       ECS027
00881          MOVE PROD-SUMMARY-REQUEST  TO PSC-CARD-1-DATA            ECS027
00882      ELSE                                                         ECS027
00883          MOVE PROD-SUMMARY-REQUEST  TO PSC-CARD-2-DATA.           ECS027
00884                                                                   ECS027
00885      GO TO 0300-READ-INPUT-CARD.                                  ECS027
00886                                                                   ECS027
00887  0300-END-OF-DATA.                                                ECS027
00888      IF REQUEST-EOF = 'N'                                         ECS027
00889          MOVE +0334                      TO  WS-RETURN-CODE       ECS027
00890          MOVE 'NO PRODUCTION REQUEST CARD(S) FOUND'  TO           ECS027
00891                                              WS-ABEND-MESSAGE     ECS027
00892          DISPLAY 'NO PRODUCTION REQUEST CARD(S) WERE FOUND.'      ECS027
00893          PERFORM 9999-ABEND-DISPLAYS                              ECS027
00894          GO TO ABEND-PGM.                                         ECS027
00895                                                                   ECS027
00896      CLOSE PROD-SUM-REQ.                                          ECS027
00897                                                                   ECS027
00898      PERFORM 0400-EDIT-THE-REQUEST THRU 0401-EXIT.                ECS027
00899                                                                   ECS027
00900      IF EDITS-FAILED                                              ECS027
00901          PERFORM 9000-ERROR-REPORT THRU 9001-EXIT.                ECS027
00902                                                                   ECS027
00903      IF EDITS-PASSED                                              ECS027
00904          OPEN OUTPUT PRINTR                                       ECS027
00905          MOVE PRT-ERR-HDR-1      TO PRT-DATA                      ECS027
00906          PERFORM 9200-PRINT                                       ECS027
00907          MOVE '-   NO ERRORS '   TO PRT-DATA                         CL**9
00908          PERFORM 9200-PRINT                                       ECS027
00909          CLOSE PRINTR.                                            ECS027
00910                                                                   ECS027
00911  0300-EXIT.                                                       ECS027
00912      EXIT.                                                        ECS027
00913  EJECT                                                            ECS027
00914  0400-EDIT-THE-REQUEST.                                           ECS027
00915      IF PROD-SUMMARY-CONTROL-DATA = SPACES                        ECS027
00916          MOVE DESC03             TO ERR-MSG-PRT                   ECS027
00917          MOVE ERR-MSG            TO EH3-STD-DATA-MSG              ECS027
00918          GO TO 0401-EXIT.                                         ECS027
00919                                                                   ECS027
00920      IF PSC-STANDARD-REQUESTS = SPACES                            ECS027
00921          IF PSC-DYNAMIC-REQUESTS = SPACES                         ECS027
00922              MOVE DESC04         TO ERR-MSG-PRT                   ECS027
00923              MOVE ERR-MSG        TO EH3-STD-DATA-MSG.             ECS027
00924                                                                   ECS027
00925      IF PSC-STANDARD-REQUESTS NOT = SPACES                        ECS027
00926          PERFORM 8900-EMPTY-LOOP                                  ECS027
00927              VARYING PSCSR FROM +1 BY +1                          ECS027
00928                  UNTIL NOT PSC-STD-VALID (PSCSR)                  ECS027
00929                      OR PSCSR = +11                               ECS027
00930          IF PSCSR = +11                                           ECS027
00931             AND PSC-STD-VALID (PSCSR)                             ECS027
00932              NEXT SENTENCE                                        ECS027
00933          ELSE                                                     ECS027
00934              MOVE DESC06         TO ERR-MSG-PRT                   ECS027
00935              MOVE ERR-MSG        TO ED1-STD-ERR-MSG.              ECS027
00936                                                                   ECS027
00937      IF PSC-ACCT-DT-RANGE-INDIC NOT = SPACE                       ECS027
00938          IF PSC-ACCT-DT-RANGE-INDIC NOT = 'Y'                     ECS027
00939              IF PSC-ACCT-DT-RANGE-INDIC NOT = 'N'                 ECS027
00940                  MOVE DESC28     TO ERR-MSG-PRT                   ECS027
00941                  MOVE ERR-MSG    TO ED2-ACCT-DT-RANGE-MSG.        ECS027
00942                                                                   ECS027
00943      IF PSC-ACCT-DT-RANGE-INDIC = SPACE                           ECS027
00944          MOVE 'N'                TO PSC-ACCT-DT-RANGE-INDIC.      ECS027
00945                                                                   ECS027
00946      IF PSC-ACCT-DT-RANGE-INDIC = 'Y'                             ECS027
00947          IF PSC-STANDARD-REQUESTS NOT = SPACES                    ECS027
00948              SET PSCSR TO +1                                      ECS027
00949              IF PSC-STD-REQ (PSCSR) NOT = 'X'                     ECS027
00950                  MOVE DESC30     TO ERR-MSG-PRT                   ECS027
00951                  MOVE ERR-MSG    TO ED2-ACCT-DT-RANGE-MSG         ECS027
00952              ELSE                                                 ECS027
00953                  NEXT SENTENCE                                    ECS027
00954          ELSE                                                     ECS027
00955              IF PSC-DYNAMIC-REQUESTS NOT = SPACES                 ECS027
00956                  SET PSCDH TO +9                                  ECS027
00957                  IF NOT PSC-DH-VALID (PSCDH)                      ECS027
00958                      MOVE DESC30     TO ERR-MSG-PRT               ECS027
00959                      MOVE ERR-MSG    TO ED2-ACCT-DT-RANGE-MSG.    ECS027
00960                                                                   ECS027
00961      IF PSC-GRAPH-REQ  = SPACE                                    ECS027
00962          MOVE 'Y'                TO PSC-GRAPH-REQ.                ECS027
00963  EJECT                                                               CL*11
00964      IF PSC-TERM-GROUPING NOT = SPACES                            ECS027
00965          IF PSC-TERM-1 NOT NUMERIC                                ECS027
00966              MOVE DESC14         TO ERR-MSG-PRT                   ECS027
00967              MOVE ERR-MSG        TO ED5-TERM-GRP-MSG.             ECS027
00968                                                                   ECS027
00969      IF PSC-TERM-GROUPING NOT = SPACES                            ECS027
00970          IF PSC-TERM-2 = SPACES                                   ECS027
00971              IF PSC-TERM-3 NOT = SPACES                           ECS027
00972                  MOVE DESC16     TO ERR-MSG-PRT                   ECS027
00973                  MOVE ERR-MSG    TO ED5-TERM-GRP-MSG              ECS027
00974              ELSE                                                 ECS027
00975                  IF PSC-TERM-4 NOT = SPACES                       ECS027
00976                      MOVE DESC16     TO ERR-MSG-PRT               ECS027
00977                      MOVE ERR-MSG    TO ED5-TERM-GRP-MSG          ECS027
00978                  ELSE                                             ECS027
00979                      NEXT SENTENCE                                ECS027
00980          ELSE                                                     ECS027
00981              IF PSC-TERM-2 NUMERIC                                ECS027
00982                  IF PSC-TERM-1 NUMERIC                            ECS027
00983                      IF PSC-TERM-2-LOW NOT GREATER                ECS027
00984                                            PSC-TERM-1-HIGH        ECS027
00985                          MOVE DESC18     TO ERR-MSG-PRT           ECS027
00986                          MOVE ERR-MSG    TO ED5-TERM-GRP-MSG      ECS027
00987                      ELSE                                         ECS027
00988                          IF PSC-TERM-2-LOW NOT LESS               ECS027
00989                                                PSC-TERM-2-HIGH    ECS027
00990                              MOVE DESC18     TO ERR-MSG-PRT       ECS027
00991                              MOVE ERR-MSG    TO ED5-TERM-GRP-MSG  ECS027
00992                          ELSE                                     ECS027
00993                              NEXT SENTENCE                        ECS027
00994                  ELSE                                             ECS027
00995                      IF PSC-TERM-2-LOW NOT LESS                   ECS027
00996                                            PSC-TERM-2-HIGH        ECS027
00997                          MOVE DESC18     TO ERR-MSG-PRT           ECS027
00998                          MOVE ERR-MSG    TO ED5-TERM-GRP-MSG      ECS027
00999                      ELSE                                         ECS027
01000                          NEXT SENTENCE                            ECS027
01001              ELSE                                                 ECS027
01002                  MOVE DESC14     TO ERR-MSG-PRT                   ECS027
01003                  MOVE ERR-MSG    TO ED5-TERM-GRP-MSG.             ECS027
01004                                                                      CL*11
01005      IF PSC-TERM-GROUPING NOT = SPACES                            ECS027
01006          IF PSC-TERM-3 = SPACES                                   ECS027
01007              IF PSC-TERM-4 NOT = SPACES                           ECS027
01008                  MOVE DESC16     TO ERR-MSG-PRT                   ECS027
01009                  MOVE ERR-MSG    TO ED5-TERM-GRP-MSG              ECS027
01010              ELSE                                                 ECS027
01011                  NEXT SENTENCE                                    ECS027
01012          ELSE                                                     ECS027
01013              IF PSC-TERM-3 NUMERIC                                ECS027
01014                  IF PSC-TERM-2 NUMERIC                            ECS027
01015                      IF PSC-TERM-3-LOW NOT GREATER                ECS027
01016                                            PSC-TERM-2-HIGH        ECS027
01017                          MOVE DESC18     TO ERR-MSG-PRT           ECS027
01018                          MOVE ERR-MSG    TO ED5-TERM-GRP-MSG      ECS027
01019                      ELSE                                         ECS027
01020                          IF PSC-TERM-3-LOW NOT LESS               ECS027
01021                                                PSC-TERM-3-HIGH    ECS027
01022                              MOVE DESC18     TO ERR-MSG-PRT       ECS027
01023                              MOVE ERR-MSG    TO ED5-TERM-GRP-MSG  ECS027
01024                          ELSE                                     ECS027
01025                              NEXT SENTENCE                        ECS027
01026                  ELSE                                             ECS027
01027                      IF PSC-TERM-3-LOW NOT LESS                   ECS027
01028                                            PSC-TERM-3-HIGH        ECS027
01029                          MOVE DESC18     TO ERR-MSG-PRT           ECS027
01030                          MOVE ERR-MSG    TO ED5-TERM-GRP-MSG      ECS027
01031                      ELSE                                         ECS027
01032                          NEXT SENTENCE                            ECS027
01033              ELSE                                                 ECS027
01034                  MOVE DESC14     TO ERR-MSG-PRT                   ECS027
01035                  MOVE ERR-MSG     TO ED5-TERM-GRP-MSG.            ECS027
01036                                                                   ECS027
01037      IF PSC-TERM-GROUPING NOT = SPACES                            ECS027
01038          IF PSC-TERM-4 NOT = SPACES                               ECS027
01039              IF PSC-TERM-4 NUMERIC                                ECS027
01040                  IF PSC-TERM-3 NUMERIC                            ECS027
01041                      IF PSC-TERM-4-LOW NOT GREATER                ECS027
01042                                            PSC-TERM-3-HIGH        ECS027
01043                          MOVE DESC18     TO ERR-MSG-PRT           ECS027
01044                          MOVE ERR-MSG    TO ED5-TERM-GRP-MSG      ECS027
01045                      ELSE                                         ECS027
01046                          NEXT SENTENCE                            ECS027
01047              ELSE                                                 ECS027
01048                  MOVE DESC14     TO ERR-MSG-PRT                   ECS027
01049                  MOVE ERR-MSG    TO ED5-TERM-GRP-MSG.             ECS027
01050                                                                   ECS027
01051      IF PSC-TERM-GROUPING = SPACES                                ECS027
01052          MOVE DEFAULT-TERM-GROUPING TO PSC-TERM-GROUPING.         ECS027
01053  EJECT                                                            ECS027
01054      IF PSC-AGE-GROUPING NOT = SPACES                             ECS027
01055          IF PSC-AGE-1 NOT NUMERIC                                 ECS027
01056              MOVE DESC19         TO ERR-MSG-PRT                   ECS027
01057              MOVE ERR-MSG        TO ED6-AGE-GRP-MSG.              ECS027
01058                                                                   ECS027
01059      IF PSC-AGE-GROUPING NOT = SPACES                             ECS027
01060          IF PSC-AGE-2 = SPACES                                    ECS027
01061              IF PSC-AGE-3 NOT = SPACES                            ECS027
01062                  MOVE DESC21     TO ERR-MSG-PRT                   ECS027
01063                  MOVE ERR-MSG    TO ED6-AGE-GRP-MSG               ECS027
01064              ELSE                                                 ECS027
01065                  IF PSC-AGE-4 NOT = SPACES                        ECS027
01066                      MOVE DESC21     TO ERR-MSG-PRT               ECS027
01067                      MOVE ERR-MSG    TO ED6-AGE-GRP-MSG           ECS027
01068                  ELSE                                             ECS027
01069                      NEXT SENTENCE                                ECS027
01070          ELSE                                                     ECS027
01071              IF PSC-AGE-2 NUMERIC                                 ECS027
01072                  IF PSC-AGE-1 NUMERIC                             ECS027
01073                      IF PSC-AGE-2-LOW NOT GREATER                 ECS027
01074                                           PSC-AGE-1-HIGH          ECS027
01075                          MOVE DESC23     TO ERR-MSG-PRT           ECS027
01076                          MOVE ERR-MSG    TO ED6-AGE-GRP-MSG       ECS027
01077                      ELSE                                         ECS027
01078                          IF PSC-AGE-2-LOW NOT LESS                ECS027
01079                                               PSC-AGE-2-HIGH      ECS027
01080                              MOVE DESC23     TO ERR-MSG-PRT       ECS027
01081                              MOVE ERR-MSG    TO ED6-AGE-GRP-MSG   ECS027
01082                          ELSE                                     ECS027
01083                              NEXT SENTENCE                        ECS027
01084                  ELSE                                             ECS027
01085                      IF PSC-AGE-2-LOW NOT LESS                    ECS027
01086                                           PSC-AGE-2-HIGH          ECS027
01087                          MOVE DESC23     TO ERR-MSG-PRT           ECS027
01088                          MOVE ERR-MSG    TO ED6-AGE-GRP-MSG       ECS027
01089                      ELSE                                         ECS027
01090                          NEXT SENTENCE                            ECS027
01091              ELSE                                                 ECS027
01092                  MOVE DESC19     TO ERR-MSG-PRT                   ECS027
01093                  MOVE ERR-MSG    TO ED6-AGE-GRP-MSG.              ECS027
01094                                                                      CL*11
01095      IF PSC-AGE-GROUPING NOT = SPACES                             ECS027
01096          IF PSC-AGE-3 = SPACES                                    ECS027
01097              IF PSC-AGE-4 NOT = SPACES                            ECS027
01098                  MOVE DESC21     TO ERR-MSG-PRT                   ECS027
01099                  MOVE ERR-MSG    TO ED6-AGE-GRP-MSG               ECS027
01100              ELSE                                                 ECS027
01101                  NEXT SENTENCE                                    ECS027
01102          ELSE                                                     ECS027
01103              IF PSC-AGE-3 NUMERIC                                 ECS027
01104                  IF PSC-AGE-2 NUMERIC                             ECS027
01105                      IF PSC-AGE-3-LOW NOT GREATER                 ECS027
01106                                           PSC-AGE-2-HIGH          ECS027
01107                          MOVE DESC23     TO ERR-MSG-PRT           ECS027
01108                          MOVE ERR-MSG    TO ED6-AGE-GRP-MSG       ECS027
01109                      ELSE                                         ECS027
01110                          IF PSC-AGE-3-LOW NOT LESS                ECS027
01111                                               PSC-AGE-3-HIGH      ECS027
01112                              MOVE DESC23     TO ERR-MSG-PRT       ECS027
01113                              MOVE ERR-MSG    TO ED6-AGE-GRP-MSG   ECS027
01114                          ELSE                                     ECS027
01115                              NEXT SENTENCE                        ECS027
01116                  ELSE                                             ECS027
01117                      IF PSC-AGE-3-LOW NOT LESS                    ECS027
01118                                           PSC-AGE-3-HIGH          ECS027
01119                          MOVE DESC23     TO ERR-MSG-PRT           ECS027
01120                          MOVE ERR-MSG    TO ED6-AGE-GRP-MSG       ECS027
01121                      ELSE                                         ECS027
01122                          NEXT SENTENCE                            ECS027
01123              ELSE                                                 ECS027
01124                  MOVE DESC19     TO ERR-MSG-PRT                   ECS027
01125                  MOVE ERR-MSG    TO ED6-AGE-GRP-MSG.              ECS027
01126                                                                      CL*11
01127      IF PSC-AGE-GROUPING NOT = SPACES                             ECS027
01128          IF PSC-AGE-4 = SPACES                                    ECS027
01129              IF PSC-AGE-5 NOT = SPACES                            ECS027
01130                  MOVE DESC21     TO ERR-MSG-PRT                   ECS027
01131                  MOVE ERR-MSG    TO ED6-AGE-GRP-MSG               ECS027
01132              ELSE                                                 ECS027
01133                  NEXT SENTENCE                                    ECS027
01134          ELSE                                                     ECS027
01135              IF PSC-AGE-4 NUMERIC                                 ECS027
01136                  IF PSC-AGE-3 NUMERIC                             ECS027
01137                      IF PSC-AGE-4-LOW NOT GREATER                 ECS027
01138                                           PSC-AGE-3-HIGH          ECS027
01139                          MOVE DESC23     TO ERR-MSG-PRT           ECS027
01140                          MOVE ERR-MSG    TO ED6-AGE-GRP-MSG       ECS027
01141                      ELSE                                         ECS027
01142                          IF PSC-AGE-4-LOW NOT LESS                ECS027
01143                                               PSC-AGE-4-HIGH      ECS027
01144                              MOVE DESC23     TO ERR-MSG-PRT       ECS027
01145                              MOVE ERR-MSG    TO ED6-AGE-GRP-MSG   ECS027
01146                          ELSE                                     ECS027
01147                              NEXT SENTENCE                        ECS027
01148                  ELSE                                             ECS027
01149                      IF PSC-AGE-4-LOW NOT LESS                    ECS027
01150                                           PSC-AGE-4-HIGH          ECS027
01151                          MOVE DESC23     TO ERR-MSG-PRT           ECS027
01152                          MOVE ERR-MSG    TO ED6-AGE-GRP-MSG       ECS027
01153                      ELSE                                         ECS027
01154                          NEXT SENTENCE                            ECS027
01155              ELSE                                                 ECS027
01156                  MOVE DESC19     TO ERR-MSG-PRT                   ECS027
01157                  MOVE ERR-MSG    TO ED6-AGE-GRP-MSG.              ECS027
01158                                                                   ECS027
01159      IF PSC-AGE-GROUPING NOT = SPACES                             ECS027
01160          IF PSC-AGE-5 NOT = SPACES                                ECS027
01161              IF PSC-AGE-5 NUMERIC                                 ECS027
01162                  IF PSC-AGE-4 NUMERIC                             ECS027
01163                      IF PSC-AGE-5-LOW NOT GREATER                 ECS027
01164                                           PSC-AGE-4-HIGH          ECS027
01165                          MOVE DESC23     TO ERR-MSG-PRT           ECS027
01166                          MOVE ERR-MSG    TO ED6-AGE-GRP-MSG       ECS027
01167                      ELSE                                         ECS027
01168                          NEXT SENTENCE                            ECS027
01169              ELSE                                                 ECS027
01170                  MOVE DESC19     TO ERR-MSG-PRT                   ECS027
01171                  MOVE ERR-MSG    TO ED6-AGE-GRP-MSG.              ECS027
01172                                                                   ECS027
01173      IF PSC-AGE-GROUPING = SPACES                                 ECS027
01174          MOVE DEFAULT-AGE-GROUPING   TO PSC-AGE-GROUPING.         ECS027
01175  EJECT                                                            ECS027
01176      IF PSC-DYN-HEIRARCHY-LIST NOT = SPACES                       ECS027
01177          PERFORM 8900-EMPTY-LOOP                                  ECS027
01178              VARYING PSCDH FROM +1 BY +1                          ECS027
01179                  UNTIL NOT PSC-DH-VALID (PSCDH)                   ECS027
01180                      OR PSCDH = +9                                ECS027
01181          IF PSCDH = +9                                            ECS027
01182             AND PSC-DH-VALID (PSCDH)                              ECS027
01183              SET PSCDH TO +1                                      ECS027
01184              PERFORM 0500-CHECK-HEIRARCHY-4-DUPS 8 TIMES             CL**9
01185              IF HEIRARCHY-DUPS-FOUND = 'Y'                        ECS027
01186                  MOVE DESC25     TO ERR-MSG-PRT                   ECS027
01187                  MOVE ERR-MSG    TO ED7-HEIRARCHY-MSG             ECS027
01188              ELSE                                                 ECS027
01189                  NEXT SENTENCE                                    ECS027
01190          ELSE                                                     ECS027
01191              MOVE DESC24         TO ERR-MSG-PRT                   ECS027
01192              MOVE ERR-MSG        TO ED7-HEIRARCHY-MSG.            ECS027
01193                                                                      CL**6
01194      MOVE PSC-CID-LOW                TO WS-CID-LOW-MDY.              CL**6
01195      MOVE PSC-CID-HIGH               TO WS-CID-HIGH-MDY.             CL**6
01196                                                                   ECS027
01197      IF PSC-CID-LOW = SPACES  OR  ZEROS                              CL**4
01198          MOVE ZEROS                  TO PSC-CID-LOW-N                CL**4
01199      ELSE                                                            CL**4
01200          MOVE PSC-CID-LOW            TO DC-GREG-DATE-1-MDY           CL**4
01201          MOVE '4'                    TO DC-OPTION-CODE               CL**4
01202          PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT                 CL**4
01203          IF NO-CONVERSION-ERROR                                      CL**4
01204              MOVE ' '                TO DC-OPTION-CODE               CL**4
01205              PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT             CL**4
01206              MOVE DC-GREG-DATE-CYMD  TO PSC-CID-LOW-N                CL**4
01207          ELSE                                                        CL**4
01208              MOVE ZEROS              TO PSC-CID-LOW-N                CL**4
01209              MOVE DESC27             TO ERR-MSG-PRT                  CL**4
01210              MOVE ERR-MSG            TO ED10-CERT-F-LIMIT-MSG.       CL**4
01211                                                                      CL**4
01212      IF PSC-CID-HIGH = SPACES                                        CL**4
01213          MOVE ZEROS                  TO PSC-CID-HIGH-N               CL**4
01214      ELSE                                                            CL**4
01215      IF PSC-CID-HIGH = '999999'                                      CL**4
01216          MOVE 99999999999            TO PSC-CID-HIGH-N               CL**4
01217      ELSE                                                            CL**4
01218          MOVE PSC-CID-HIGH           TO DC-GREG-DATE-1-MDY           CL**4
01219          MOVE '4'                    TO DC-OPTION-CODE               CL**4
01220          PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT                 CL**4
01221          IF NO-CONVERSION-ERROR                                      CL**4
01222              MOVE ' '                TO DC-OPTION-CODE               CL**4
01223              PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT             CL**4
01224              MOVE DC-GREG-DATE-CYMD  TO PSC-CID-HIGH-N               CL**4
01225          ELSE                                                        CL**4
01226              MOVE ZEROS              TO PSC-CID-HIGH-N               CL**4
01227              MOVE DESC27             TO ERR-MSG-PRT                  CL**4
01228              MOVE ERR-MSG            TO ED11-CERT-T-LIMIT-MSG.       CL**5
01229                                                                      CL**7
01230      IF (PSC-CID-LOW-N = ZEROS  AND                                  CL**7
01231          PSC-CID-HIGH-N = ZEROS)                                     CL**7
01232                  OR                                                  CL*11
01233         (PSC-CID-LOW-N = ZEROS  AND                                  CL*11
01234          PSC-CID-HIGH-N = 99999999999)                               CL*11
01235          NEXT SENTENCE                                               CL*10
01236      ELSE                                                            CL**4
01237          IF PSC-CID-LOW-N = ZEROS                                    CL**4
01238              MOVE DESC09             TO ERR-MSG-PRT                  CL**4
01239              MOVE ERR-MSG            TO ED10-CERT-F-LIMIT-MSG        CL**4
01240          ELSE                                                     ECS027
01241              IF PSC-CID-HIGH-N = ZEROS                               CL**4
01242                  MOVE DESC10         TO ERR-MSG-PRT                  CL**4
01243                  MOVE ERR-MSG        TO ED11-CERT-T-LIMIT-MSG        CL**5
01244              ELSE                                                 ECS027
01245                  IF PSC-CID-LOW-N GREATER THAN PSC-CID-HIGH-N        CL**4
01246                      MOVE DESC12     TO ERR-MSG-PRT                  CL**4
01247                      MOVE ERR-MSG    TO ED10-CERT-F-LIMIT-MSG.       CL**7
01248                                                                      CL**7
01249  EJECT                                                            ECS027
01250      IF PSC-BUSINESS-TYPE NOT = SPACES                            ECS027
01251          PERFORM 8900-EMPTY-LOOP                                  ECS027
01252              VARYING BUS-CODE-IDX FROM +1 BY +1                   ECS027
01253                  UNTIL PSC-BUS-TYPE = CLAS-BUSC-CODE              ECS027
01254                          (BUS-CODE-IDX)                           ECS027
01255                      OR BUS-CODE-IDX = +50                        ECS027
01256          IF PSC-BUS-TYPE = CLAS-BUSC-CODE (BUS-CODE-IDX)          ECS027
01257              NEXT SENTENCE                                        ECS027
01258          ELSE                                                     ECS027
01259              MOVE DESC07     TO ERR-MSG-PRT                       ECS027
01260              MOVE ERR-MSG    TO ED9-BUS-TY-LIMIT-MSG.             ECS027
01261                                                                   ECS027
01262      IF PSC-L-STATE NOT = SPACES                                  ECS027
01263          PERFORM 8900-EMPTY-LOOP                                  ECS027
01264              VARYING STATE-IDX FROM +1 BY +1                      ECS027
01265                  UNTIL PSC-L-STATE = (STATE-SUB (STATE-IDX)       ECS027
01266                      OR STATE-ABBR (STATE-IDX))                   ECS027
01267                      OR STATE-IDX = +75                           ECS027
01268          IF PSC-L-STATE = (STATE-SUB (STATE-IDX)                  ECS027
01269             OR STATE-ABBR (STATE-IDX))                            ECS027
01270              NEXT SENTENCE                                        ECS027
01271          ELSE                                                     ECS027
01272              MOVE DESC26     TO ERR-MSG-PRT                       ECS027
01273              MOVE ERR-MSG    TO ED8-ST-LIMIT-MSG.                 ECS027
01274                                                                   ECS027
01275      IF ERR-MSG-PRT NOT = SPACES                                  ECS027
01276          MOVE 'Y'                TO ERROR-INDICATOR.              ECS027
01277                                                                   ECS027
01278  0401-EXIT.                                                       ECS027
01279      EXIT.                                                        ECS027
01280                                                                   ECS027
01281  EJECT                                                               CL*11
01282  0500-CHECK-HEIRARCHY-4-DUPS.                                        CL**9
01283 *                        ---------------NOTE---------------       ECS027
01284 *                           DH = DYNAMIC HEIRARCHY INDEX          ECS027
01285 *                        (SECONDARY SUBSCRIPT TO FACILITATE       ECS027
01286 *                           DUPLICATE HEIRARCHY EDITING)          ECS027
01287      SET DH TO PSCDH.                                             ECS027
01288      ADD +1 TO DH.                                                ECS027
01289                                                                   ECS027
01290      PERFORM 0501-CHK-HRCHY-NEXT THRU 0501-EXIT                      CL*12
01291          UNTIL HEIRARCHY-DUPS-FOUND = 'Y'                         ECS027
01292              OR DH GREATER +9.                                    ECS027
01293                                                                   ECS027
01294      SET PSCDH UP BY +1.                                          ECS027
01295                                                                   ECS027
01296  0501-CHK-HRCHY-NEXT.                                                CL**9
01297      IF PSC-D-HEIRARCHY-X (PSCDH) NUMERIC                         ECS027
01298          IF PSC-D-HEIRARCHY-X (DH) NUMERIC                        ECS027
01299              IF PSC-D-HEIRARCHY (PSCDH) =                         ECS027
01300                      PSC-D-HEIRARCHY (DH)                         ECS027
01301                  MOVE 'Y'        TO HEIRARCHY-DUPS-FOUND.         ECS027
01302                                                                   ECS027
01303      ADD +1 TO DH.                                                ECS027
01304                                                                      CL**9
01305  0501-EXIT.                                                          CL**9
01306      EXIT.                                                           CL**9
01307                                                                   ECS027
01308  EJECT                                                               CL**9
01309  1000-BUILD-N-SORT-CLAIMS.                                        ECS027
01310      OPEN INPUT CLAIM-HIST.                                       ECS027
01311                                                                   ECS027
01312      SORT SORT-CALL-C ON ASCENDING KEY SRT-KEY-C                  ECS027
01313          INPUT PROCEDURE 1100-BUILD-CLAIM-HIST-EXTRS              ECS027
01314              THRU 1102-EXIT                                       ECS027
01315          OUTPUT PROCEDURE 1500-RETURN-CLAIM-HIST-EXTRS            ECS027
01316              THRU 1502-XIT.                                       ECS027
01317                                                                   ECS027
01318      IF SORT-RETURN NOT = ZERO                                    ECS027
01319          MOVE +0100                         TO  WS-RETURN-CODE    ECS027
01320          MOVE '1000 INTERNAL SORT ABORTED'  TO  WS-ABEND-MESSAGE  ECS027
01321          PERFORM 9999-ABEND-DISPLAYS                              ECS027
01322          GO TO ABEND-PGM.                                         ECS027
01323                                                                   ECS027
01324      CLOSE CLAIM-HIST.                                            ECS027
01325                                                                   ECS027
01326  1000-EXIT.                                                       ECS027
01327      EXIT.                                                        ECS027
01328                                                                      CL**9
01329                                                                      CL**9
01330                                                                      CL**9
01331  1100-BUILD-CLAIM-HIST-EXTRS     SECTION.                         ECS027
01332                                                                   ECS027
01333  1101-BUILD-CLAIM-HISTORY.                                        ECS027
01334      READ CLAIM-HIST INTO DETAIL-EXTRACT AT END                   ECS027
01335          MOVE 'Y'                TO CLAIM-EXTRACTS-SORTED         ECS027
01336          GO TO 1102-EXIT.                                         ECS027
01337                                                                   ECS027
01338      IF DE-RECORD-ID = 'DE'                                       ECS027
01339          IF DE-REIN = SPACE                                       ECS027
01340              IF DE-CLAIM                                          ECS027
01341                  PERFORM 1110-BLD-CLM-HIST-DATA                   ECS027
01342                      THRU 1111-EXIT                               ECS027
01343                  IF CLM-HIST-ERROR = 'N'                          ECS027
01344                      MOVE CLAIM-EXTRACT-CONTROL TO SRT-KEY-C      ECS027
01345                      MOVE CLM-CLAIM-EXTR-DATA   TO SRT-DATA-C     ECS027
01346                      RELEASE SRT-CLL-C                            ECS027
01347                      ADD +1      TO CLAIM-SORT-COUNT              ECS027
01348                  ELSE                                             ECS027
01349                      MOVE 'N'    TO CLM-HIST-ERROR.               ECS027
01350                                                                   ECS027
01351      GO TO 1101-BUILD-CLAIM-HISTORY.                              ECS027
01352                                                                   ECS027
01353  1102-EXIT.                                                       ECS027
01354      EXIT.                                                        ECS027
01355  EJECT                                                            ECS027
01356  1110-BLD-CLM-HIST-DATA.                                          ECS027
01357      MOVE SPACES                 TO CLAIM-EXTRACT.                ECS027
01358      MOVE DE-CARRIER             TO CLM-CARRIER.                  ECS027
01359      MOVE DE-GROUPING            TO CLM-GROUP.                    ECS027
01360      MOVE DE-STATE               TO CLM-STATE.                    ECS027
01361      MOVE DE-ACCOUNT             TO CLM-ACCOUNT.                  ECS027
01362      MOVE DE-TYPE                TO CLM-CLAIM-TYPE.               ECS027
01363                                                                   ECS027
01364      IF DE-EFF NOT NUMERIC                                        ECS027
01365          DISPLAY '**-ECS-027  DE-EFF NOT NUMERIC ...'             ECS027
01366          MOVE 'Y'                TO CLM-HIST-ERROR                ECS027
01367      ELSE                                                         ECS027
01368          MOVE DE-EFF             TO CLM-EFF-DATE.                    CL*11
01369                                                                   ECS027
01370      IF DE-PAY NOT NUMERIC                                        ECS027
01371          MOVE 'Y'                TO CLM-HIST-ERROR                ECS027
01372      ELSE                                                         ECS027
01373          MOVE DE-PAY             TO CLM-PAID-DATE.                   CL*11
01374                                                                   ECS027
01375      IF DE-CLAIM-AMT NOT NUMERIC                                  ECS027
01376          MOVE 'Y'                TO CLM-HIST-ERROR                ECS027
01377      ELSE                                                         ECS027
01378          MOVE DE-CLAIM-AMT       TO CLM-AMT-PAID.                 ECS027
01379                                                                   ECS027
01380      IF DE-AGE NOT NUMERIC                                        ECS027
01381          MOVE 'Y'                TO CLM-HIST-ERROR                ECS027
01382      ELSE                                                         ECS027
01383          MOVE DE-AGE             TO CLM-ORIG-AGE.                 ECS027
01384                                                                   ECS027
01385      IF DE-DTH OR DE-OB-DTH                                       ECS027
01386          IF DE-LF-TERM NOT NUMERIC                                ECS027
01387              MOVE 'Y'            TO  CLM-HIST-ERROR               ECS027
01388          ELSE                                                     ECS027
01389              MOVE DE-LF-TERM     TO  CLM-ORIG-TERM.                  CL*11
01390                                                                   ECS027
01391      IF DE-AH OR DE-OB-AH                                         ECS027
01392          IF DE-AH-TERM NOT NUMERIC                                ECS027
01393              MOVE 'Y'            TO  CLM-HIST-ERROR               ECS027
01394          ELSE                                                     ECS027
01395              MOVE DE-AH-TERM     TO  CLM-ORIG-TERM.                  CL*11
01396                                                                   ECS027
01397  1111-EXIT.                                                       ECS027
01398      EXIT.                                                        ECS027
01399  EJECT                                                            ECS027
01400  1500-RETURN-CLAIM-HIST-EXTRS SECTION.                            ECS027
01401      OPEN OUTPUT EXTR-CLAIM.                                      ECS027
01402                                                                   ECS027
01403  1501-RETURN-CLAIM.                                               ECS027
01404      RETURN SORT-CALL-C INTO CLAIM-EXTRACT AT END                 ECS027
01405          CLOSE EXTR-CLAIM                                         ECS027
01406          GO TO 1502-XIT.                                          ECS027
01407                                                                   ECS027
01408      WRITE CLAIM-EXTRACT-IO FROM CLAIM-EXTRACT.                   ECS027
01409                                                                   ECS027
01410      GO TO 1501-RETURN-CLAIM.                                     ECS027
01411                                                                   ECS027
01412  1502-XIT.                                                        ECS027
01413      EXIT.                                                        ECS027
01414                                                                   ECS027
01415  EJECT                                                               CL**9
01416  2000-BUILD-DETAIL-EXTRACTS SECTION.                              ECS027
01417      OPEN INPUT  CERTS                                            ECS027
01418                  EXTR-CLAIM                                       ECS027
01419                  EPEC                                             ECS027
01420                  ERACCTT.                                         ECS027
01421                                                                   ECS027
01422      IF ERACCT-FILE-STATUS NOT = '00' AND '97'                    ECS027
01423          MOVE 'ERROR OPENING ERACCT MSTR'  TO  WS-ABEND-MESSAGE   ECS027
01424          MOVE +0302                        TO  WS-RETURN-CODE     ECS027
01425          GO TO ABEND-PGM.                                         ECS027
01426                                                                   ECS027
01427      MOVE SPACES                 TO PSC-ROLLING-12-MONTHS.        ECS027
01428      SET  PSC12  TO +13.                                          ECS027
01429                                                                   ECS027
01430      PERFORM 2100-SET-ROLLING-12-MONTHS                           ECS027
01431          THRU 2101-EXIT 13 TIMES.                                 ECS027
01432                                                                   ECS027
01433      MOVE RUN-DATE               TO PSC-RUN-DATE.                    CL*11
01434                                                                   ECS027
01435      PERFORM 2200-LOAD-SAVE-CONTROL-DATA.                         ECS027
01436                                                                   ECS027
01437      PERFORM 3000-SORT-INPUT-PROCESS THRU 3002-EXIT.              ECS027
01438                                                                   ECS027
01439      CLOSE CERTS                                                  ECS027
01440            EXTR-CLAIM                                             ECS027
01441            EPEC                                                   ECS027
01442            ERACCTT                                                ECS027
01443            ELCNTL.                                                ECS027
01444                                                                   ECS027
01445      IF ERACCT-FILE-STATUS NOT = '00'                             ECS027
01446          MOVE 'ERROR CLOSING ERACCT MSTR'  TO  WS-ABEND-MESSAGE   ECS027
01447          MOVE +0302                        TO  WS-RETURN-CODE     ECS027
01448          GO TO ABEND-PGM.                                         ECS027
01449                                                                   ECS027
01450      IF ELCNTL-FILE-STATUS NOT = '00'                             ECS027
01451          MOVE '**** ELCNTL CLOSE ERROR ****'                      ECS027
01452                                  TO WS-ABEND-MESSAGE              ECS027
01453          MOVE ELCNTL-FILE-STATUS TO WS-ABEND-FILE-STATUS          ECS027
01454          GO TO ABEND-PGM.                                         ECS027
01455                                                                   ECS027
01456  2000-EXIT.                                                       ECS027
01457      EXIT.                                                        ECS027
01458  EJECT                                                            ECS027
01459  2100-SET-ROLLING-12-MONTHS.                                      ECS027
01460      IF PSC12 = +13                                               ECS027
01461          MOVE RUN-MO             TO PSC-R12-MONTH (PSC12)         ECS027
01462                                     DATE-MO                       ECS027
01463          MOVE RUN-CCYY           TO PSC-R12-CCYY (PSC12)          ECS027
01464                                     DATE-CCYY                     ECS027
01465      ELSE                                                         ECS027
01466          SUBTRACT +1 FROM DATE-MO                                 ECS027
01467          MOVE DATE-MO            TO PSC-R12-MONTH (PSC12)         ECS027
01468          MOVE DATE-CCYY          TO PSC-R12-CCYY (PSC12)          ECS027
01469          IF DATE-MO NOT GREATER ZEROS                             ECS027
01470              MOVE +12            TO PSC-R12-MONTH (PSC12)         ECS027
01471                                     DATE-MO                       ECS027
01472              SUBTRACT +1  FROM PSC-R12-CCYY (PSC12)               ECS027
01473              SUBTRACT +1  FROM DATE-CCYY.                            CL**3
01474                                                                   ECS027
01475      SET PSC12 DOWN BY +1.                                        ECS027
01476                                                                   ECS027
01477  2101-EXIT.                                                       ECS027
01478      EXIT.                                                        ECS027
01479                                                                   ECS027
01480  2200-LOAD-SAVE-CONTROL-DATA.                                     ECS027
01481       MOVE PSC-CARD-1-DATA       TO SCD-CARD-1-DATA.              ECS027
01482       MOVE PSC-CARD-2-DATA       TO SCD-CARD-2-DATA.              ECS027
01483       MOVE PSC-ROLLING-12-MONTHS TO SCD-ROLLING-12-MONTHS.        ECS027
01484       MOVE PSC-RUN-DATE          TO SCD-RUN-DATE.                    CL*12
01485                                                                   ECS027
01486  EJECT                                                               CL*11
01487  3000-SORT-INPUT-PROCESS SECTION.                                 ECS027
01488                                                                   ECS027
01489  3001-SORT-INPUT-PROCEDURE.                                       ECS027
01490      SORT SORT-CALL ON ASCENDING KEY SRT-KEY                      ECS027
01491          INPUT PROCEDURE 3100-BUILD-EXTRACT-PROCESS THRU          ECS027
01492                          3102-EXIT                                ECS027
01493          GIVING PROD-DTL-EXT.                                     ECS027
01494                                                                   ECS027
01495      IF SORT-RETURN NOT = ZEROES                                  ECS027
01496          MOVE +0101                         TO WS-RETURN-CODE     ECS027
01497          MOVE '3001 INTERNAL SORT ABORTED'  TO  WS-ABEND-MESSAGE  ECS027
01498          GO TO ABEND-PGM.                                         ECS027
01499                                                                   ECS027
01500  3002-EXIT.                                                       ECS027
01501      EXIT.                                                        ECS027
01502                                                                      CL*11
01503                                                                      CL*11
01504                                                                      CL*11
01505  3100-BUILD-EXTRACT-PROCESS SECTION.                              ECS027
01506      MOVE SPACE                      TO PSC-SUMMARY-CONTROL-INDIC.ECS027
01507      MOVE LOW-VALUES                 TO PSC-SORT-DATA.               CL*11
01508      MOVE PROD-SUMMARY-CONTROL-DATA  TO SRT-CLL.                  ECS027
01509      RELEASE SRT-CLL.                                             ECS027
01510                                                                   ECS027
01511  3101-BUILD-EXTRACT-PROCEDURE.                                    ECS027
01512      PERFORM 5100-READ-ACCOUNT THRU 5109-EXIT.                    ECS027
01513                                                                   ECS027
01514      IF ACCT-EOF = 'Y'                                            ECS027
01515          IF ACCT-QUALIFIED = 'N'                                  ECS027
01516             GO TO 3102-EXIT.                                      ECS027
01517                                                                   ECS027
01518      PERFORM 4300-INITIALIZATION-OF-ACCUMS.                       ECS027
01519                                                                   ECS027
01520      PERFORM 4400-ESTABLISH-SORT-KEY-ARRAY THRU 4409-EXIT.        ECS027
01521                                                                   ECS027
01522      PERFORM 4500-BUILD-EXTR-LEAD-DETAIL   THRU 4501-EXIT.        ECS027
01523                                                                   ECS027
01524      PERFORM 4110-BUILD-FROM-CERT-DATA     THRU 4111-EXIT         ECS027
01525              UNTIL CERT-DATA-BUILT = 'Y' OR                       ECS027
01526                    CERT-EOF        = 'Y'.                         ECS027
01527                                                                   ECS027
01528      PERFORM 4120-BUILD-FROM-CLAIM-DATA    THRU 4121-EXIT         ECS027
01529              UNTIL CLAIM-DATA-BUILT = 'Y' OR                      ECS027
01530                    CLAIM-EOF        = 'Y'.                        ECS027
01531                                                                   ECS027
01532      PERFORM 4130-BUILD-FROM-EPEC-DATA THRU 4131-EXIT             ECS027
01533              UNTIL EPEC-DATA-BUILT = 'Y' OR                       ECS027
01534                    EPEC-EOF        = 'Y'.                         ECS027
01535                                                                   ECS027
01536      PERFORM 4200-RELEASE-TO-SORT THRU 4209-EXIT                  ECS027
01537          VARYING SKA FROM +1 BY +1                                ECS027
01538              UNTIL SKA-KEY (SKA) = SPACES                         ECS027
01539                  OR SKA GREATER +9.                               ECS027
01540                                                                   ECS027
01541      MOVE 'N'                    TO EXTRACT-BUILT                 ECS027
01542                                     CERT-DATA-BUILT               ECS027
01543                                     CLAIM-DATA-BUILT              ECS027
01544                                     EPEC-DATA-BUILT.              ECS027
01545                                                                   ECS027
01546      IF ACCT-EOF  = 'N'                                           ECS027
01547          MOVE ACCOUNT-MASTER     TO  WS-AM-REC                    ECS027
01548          MOVE HOLD-NEW-ACCT-DATA TO  HOLD-ACCT-DATA               ECS027
01549          GO TO 3101-BUILD-EXTRACT-PROCEDURE.                      ECS027
01550                                                                   ECS027
01551  3102-EXIT.                                                       ECS027
01552      EXIT.                                                        ECS027
01553  EJECT                                                            ECS027
01554  4110-BUILD-FROM-CERT-DATA.                                       ECS027
01555      IF CERT-WAS-READ = 'Y'                                       ECS027
01556          MOVE 'N'                TO CERT-WAS-READ                 ECS027
01557       ELSE                                                        ECS027
01558          PERFORM 5000-READ-CERT THRU 5009-EXIT.                   ECS027
01559                                                                   ECS027
01560      IF CERT-EOF = 'Y'                                            ECS027
01561          GO TO 4111-EXIT.                                         ECS027
01562                                                                   ECS027
01563      IF CR-ACCT-CONTROL GREATER H-ACCT-KEY                        ECS027
01564          MOVE 'Y'                TO CERT-DATA-BUILT  CERT-WAS-READECS027
01565          GO TO 4111-EXIT.                                         ECS027
01566                                                                   ECS027
01567      IF CR-ACCT-CONTROL LESS H-ACCT-KEY                           ECS027
01568          GO TO 4111-EXIT.                                         ECS027
01569                                                                   ECS027
01570      IF SCD-ACCT-DT-RANGE-INDIC = 'Y'                             ECS027
01571          IF CR-DT NOT LESS H-ACCT-EXPIR-DATE                         CL*12
01572              MOVE 'Y'            TO CERT-DATA-BUILT  CERT-WAS-READECS027
01573              GO TO 4111-EXIT.                                     ECS027
01574                                                                   ECS027
01575      IF SCD-CID-LOW  NOT = ZEROS  OR                                 CL*11
01576         SCD-CID-HIGH NOT = ZEROS                                     CL*11
01577          IF CR-DT LESS SCD-CID-LOW  OR                               CL*11
01578             CR-DT GREATER SCD-CID-HIGH                               CL*11
01579              GO TO 4111-EXIT.                                     ECS027
01580                                                                   ECS027
01581      IF CR-DT GREATER SCD-RUN-DATE                                   CL*11
01582          GO TO 4110-BUILD-FROM-CERT-DATA.                         ECS027
01583                                                                   ECS027
01584      IF CR-ENTRY-STATUS = '9'                                     ECS027
01585          GO TO 4110-BUILD-FROM-CERT-DATA.                         ECS027
01586                                                                   ECS027
01587      ADD +1                      TO CERT-USE-COUNT.               ECS027
01588                                                                   ECS027
01589      IF CR-ENTRY-STATUS = '3' OR '5'                              ECS027
01590          MOVE ZEROS              TO CR-LFPRM  CR-AHPRM.           ECS027
01591                                                                   ECS027
01592      MOVE CR-DT                  TO WS-CR-DT-N.                      CL*15
01593                                                                      CL*15
01594      PERFORM 7100-LIFE-NET-PREM-ACCUMS THRU 7101-EXIT.            ECS027
01595                                                                   ECS027
01596      PERFORM 7150-AH-NET-PREM-ACCUMS   THRU 7151-EXIT.            ECS027
01597                                                                   ECS027
01598  4111-EXIT.                                                       ECS027
01599      EXIT.                                                        ECS027
01600  EJECT                                                            ECS027
01601  4120-BUILD-FROM-CLAIM-DATA.                                      ECS027
01602      IF CLAIM-WAS-READ = 'Y'                                      ECS027
01603          MOVE 'N'                TO CLAIM-WAS-READ                ECS027
01604       ELSE                                                        ECS027
01605          PERFORM 5200-READ-CLAIM THRU 5209-EXIT.                  ECS027
01606                                                                   ECS027
01607      IF CLAIM-EOF = 'Y'                                           ECS027
01608          GO TO 4121-EXIT.                                         ECS027
01609                                                                   ECS027
01610      IF H-CLM-ACCT-KEY GREATER H-ACCT-KEY                         ECS027
01611          MOVE 'Y'                TO CLAIM-DATA-BUILT              ECS027
01612                                     CLAIM-WAS-READ                ECS027
01613          GO TO 4121-EXIT.                                         ECS027
01614                                                                   ECS027
01615      IF H-CLM-ACCT-KEY LESS H-ACCT-KEY                            ECS027
01616          GO TO 4121-EXIT.                                         ECS027
01617                                                                   ECS027
01618      IF SCD-ACCT-DT-RANGE-INDIC = 'Y'                             ECS027
01619          IF CLM-EFF-DATE NOT LESS H-ACCT-EXPIR-DATE                  CL*12
01620              MOVE 'Y'            TO CLAIM-DATA-BUILT              ECS027
01621                                     CLAIM-WAS-READ                ECS027
01622              GO TO 4121-EXIT.                                     ECS027
01623                                                                   ECS027
01624      IF SCD-CID-LOW  NOT = ZEROS  OR                                 CL*11
01625         SCD-CID-HIGH NOT = ZEROS                                     CL*11
01626          IF CLM-EFF-DATE LESS SCD-CID-LOW  OR                        CL*11
01627             CLM-EFF-DATE GREATER SCD-CID-HIGH                        CL*11
01628              GO TO 4121-EXIT.                                     ECS027
01629                                                                   ECS027
01630      IF CLM-EFF-DATE GREATER SCD-RUN-DATE                            CL*11
01631          GO TO 4121-EXIT.                                         ECS027
01632                                                                   ECS027
01633      ADD +1                      TO CLAIM-USE-COUNT.              ECS027
01634                                                                   ECS027
01635      MOVE CLM-PAID-DATE          TO WS-DE-PAY-N.                     CL*13
01636                                                                      CL*13
01637      IF CLM-DTH                                                   ECS027
01638          PERFORM 7200-LIFE-CLAIM-AMT-ACCUMS THRU 7201-EXIT        ECS027
01639      ELSE                                                         ECS027
01640          PERFORM 7250-AH-CLAIM-AMT-ACCUMS   THRU 7251-EXIT.       ECS027
01641                                                                   ECS027
01642  4121-EXIT.                                                       ECS027
01643      EXIT.                                                        ECS027
01644  EJECT                                                            ECS027
01645  4130-BUILD-FROM-EPEC-DATA.                                       ECS027
01646      IF EPEC-WAS-READ = 'Y'                                       ECS027
01647          MOVE 'N'                TO EPEC-WAS-READ                 ECS027
01648      ELSE                                                         ECS027
01649          PERFORM 5300-READ-EPEC THRU 5309-EXIT.                   ECS027
01650                                                                   ECS027
01651      IF H-ACCT-KEY GREATER H-EPEC-ACCT-KEY                        ECS027
01652          GO TO 4131-EXIT.                                         ECS027
01653                                                                   ECS027
01654      IF EPEC-EOF = 'Y'                                            ECS027
01655         OR H-ACCT-KEY NOT = H-EPEC-ACCT-KEY                       ECS027
01656          MOVE 'Y'                TO EPEC-DATA-BUILT               ECS027
01657                                     EPEC-WAS-READ                 ECS027
01658          SET PSDM WSDM PSDGP     TO +1                            ECS027
01659          PERFORM 7090-ADD-HOLD-EPEC-AMTS THRU 7091-EXIT 12 TIMES  ECS027
01660          PERFORM 7098-SET-L12-YTD-ITD-AMTS                        ECS027
01661          GO TO 4131-EXIT.                                         ECS027
01662                                                                   ECS027
01663      IF SCD-ACCT-DT-RANGE-INDIC = 'Y'                             ECS027
01664          IF EP-EXP-DTE NOT = H-ACCT-EXPIR-DATE                       CL*12
01665              MOVE 'Y'            TO EPEC-DATA-BUILT               ECS027
01666                                     EPEC-WAS-READ                 ECS027
01667              SET PSDM WSDM PSDGP TO +1                            ECS027
01668              PERFORM 7090-ADD-HOLD-EPEC-AMTS                      ECS027
01669                 THRU 7091-EXIT 12 TIMES                           ECS027
01670              PERFORM 7098-SET-L12-YTD-ITD-AMTS                    ECS027
01671              GO TO 4131-EXIT.                                     ECS027
01672                                                                   ECS027
01673      IF EP-RUN-DTE GREATER SCD-RUN-DATE                              CL*11
01674          GO TO 4131-EXIT.                                         ECS027
01675                                                                   ECS027
01676      IF EP-REIN NOT = SPACE                                       ECS027
01677          GO TO 4131-EXIT.                                         ECS027
01678                                                                   ECS027
01679      ADD +1                      TO EPEC-USE-COUNT.               ECS027
01680      PERFORM 7000-EPEC-ACCUMS THRU 7001-EXIT.                     ECS027
01681                                                                   ECS027
01682  4131-EXIT.                                                       ECS027
01683      EXIT.                                                        ECS027
01684  EJECT                                                               CL*11
01685  4200-RELEASE-TO-SORT.                                            ECS027
01686      IF SKA-KEY (SKA) NOT = SPACES                                ECS027
01687         AND SKA NOT GREATER +9                                    ECS027
01688             IF SKA-REPORT-ID (SKA) = 'B' OR 'C'                   ECS027
01689                 MOVE SKA-HEIR-1 (SKA)   TO  PSD-GA.               ECS027
01690                                                                   ECS027
01691      IF SKA-KEY (SKA) NOT = SPACES                                ECS027
01692         AND SKA NOT GREATER +9                                    ECS027
01693          MOVE SKA-REPORT-ID (SKA)        TO PSD-STD-REQUEST-NUMBERECS027
01694          MOVE SKA-KEY (SKA)              TO PSD-SORT-DATA-ARRAY   ECS027
01695          MOVE PROD-SUMMARY-DETAIL-EXTR   TO SRT-CLL               ECS027
01696          RELEASE SRT-CLL                                          ECS027
01697          ADD +1                          TO DETAIL-EXTRACT-COUNT. ECS027
01698                                                                   ECS027
01699  4209-EXIT.                                                       ECS027
01700      EXIT.                                                        ECS027
01701  EJECT                                                            ECS027
01702  4300-INITIALIZATION-OF-ACCUMS.                                   ECS027
01703      SET PSDM TO +1.                                              ECS027
01704      PERFORM 4310-INIT-MONTH-ARRAY 7 TIMES.                       ECS027
01705                                                                   ECS027
01706      SET PSDT PSDA TO +1.                                         ECS027
01707      PERFORM 4320-INIT-TERM-AGE-ARRAY 35 TIMES.                   ECS027
01708                                                                   ECS027
01709      SET PSDGP TO +1.                                             ECS027
01710      PERFORM 4330-INIT-GRAPH-DATA 12 TIMES.                       ECS027
01711                                                                   ECS027
01712      SET WSDM TO +1.                                              ECS027
01713      PERFORM 4340-INIT-WSD-MONTH-ARRAY 13 TIMES.                  ECS027
01714                                                                   ECS027
01715      MOVE +0                     TO PSD-RETRO-AMT-PAID.           ECS027
01716                                                                   ECS027
01717  4310-INIT-MONTH-ARRAY.                                           ECS027
01718      MOVE ZEROS                  TO PSD-ML-COUNT          (PSDM)  ECS027
01719                                     PSD-ML-PREM           (PSDM)  ECS027
01720                                     PSD-ML-EARN-PREM      (PSDM)  ECS027
01721                                     PSD-ML-COMMIS         (PSDM)  ECS027
01722                                     PSD-ML-CLAIM-INCURRED (PSDM)  ECS027
01723                                     PSD-ML-CLAIMS-PAID    (PSDM)  ECS027
01724                                     PSD-MA-COUNT          (PSDM)  ECS027
01725                                     PSD-MA-PREM           (PSDM)  ECS027
01726                                     PSD-MA-EARN-PREM      (PSDM)  ECS027
01727                                     PSD-MA-COMMIS         (PSDM)  ECS027
01728                                     PSD-MA-CLAIM-INCURRED (PSDM)  ECS027
01729                                     PSD-MA-CLAIMS-PAID    (PSDM). ECS027
01730                                                                   ECS027
01731      SET PSDM UP BY +1.                                           ECS027
01732  EJECT                                                            ECS027
01733  4320-INIT-TERM-AGE-ARRAY.                                        ECS027
01734      IF PSDA GREATER +5                                           ECS027
01735          SET PSDA TO +1                                           ECS027
01736          SET PSDT UP BY +1.                                       ECS027
01737                                                                   ECS027
01738      IF PSDA = +1                                                 ECS027
01739          MOVE ZEROS              TO PSD-L-WTD-PREM       (PSDT)   ECS027
01740                                     PSD-L-AGE-COUNT-PREM (PSDT)   ECS027
01741                                     PSD-L-AGE-SUM-PREM   (PSDT)   ECS027
01742                                     PSD-L-WTD-CLAIMS     (PSDT)   ECS027
01743                                     PSD-L-AGE-COUNT-CLMS (PSDT)   ECS027
01744                                     PSD-L-AGE-SUM-CLMS   (PSDT)   ECS027
01745                                     PSD-A-WTD-PREM       (PSDT)   ECS027
01746                                     PSD-A-AGE-COUNT-PREM (PSDT)   ECS027
01747                                     PSD-A-AGE-SUM-PREM   (PSDT)   ECS027
01748                                     PSD-A-WTD-CLAIMS     (PSDT)   ECS027
01749                                     PSD-A-AGE-COUNT-CLMS (PSDT)   ECS027
01750                                     PSD-A-AGE-SUM-CLMS   (PSDT).  ECS027
01751                                                                   ECS027
01752      MOVE ZEROS                  TO PSD-L-PREM        (PSDT, PSDA)ECS027
01753                                     PSD-L-CLAIMS-PAID (PSDT, PSDA)ECS027
01754                                     PSD-A-PREM        (PSDT, PSDA)ECS027
01755                                     PSD-A-CLAIMS-PAID (PSDT, PSDA)ECS027
01756                                                                   ECS027
01757      SET PSDA UP BY +1.                                           ECS027
01758                                                                   ECS027
01759  4330-INIT-GRAPH-DATA.                                            ECS027
01760      MOVE ZEROS                  TO PSD-NET-WRITTEN-PREM (PSDGP)  ECS027
01761                                     PSD-EARNED-PREM      (PSDGP)  ECS027
01762                                     PSD-CLAIMS-PAID      (PSDGP). ECS027
01763                                                                   ECS027
01764      SET PSDGP UP BY +1.                                          ECS027
01765                                                                   ECS027
01766  4340-INIT-WSD-MONTH-ARRAY.                                       ECS027
01767      MOVE ZEROS                  TO WSD-L-COUNT     (WSDM)        ECS027
01768                                     WSD-L-PREM      (WSDM)        ECS027
01769                                     WSD-L-EARN-PREM (WSDM)        ECS027
01770                                     WSD-L-COMMIS    (WSDM)        ECS027
01771                                     WSD-L-CLM-INCUR (WSDM)        ECS027
01772                                     WSD-L-CLM-PAID  (WSDM)        ECS027
01773                                     WSD-A-COUNT     (WSDM)        ECS027
01774                                     WSD-A-PREM      (WSDM)        ECS027
01775                                     WSD-A-EARN-PREM (WSDM)        ECS027
01776                                     WSD-A-COMMIS    (WSDM)        ECS027
01777                                     WSD-A-CLM-INCUR (WSDM)        ECS027
01778                                     WSD-A-CLM-PAID  (WSDM).       ECS027
01779                                                                   ECS027
01780      SET WSDM UP BY +1.                                           ECS027
01781  EJECT                                                            ECS027
01782  4400-ESTABLISH-SORT-KEY-ARRAY.                                   ECS027
01783      MOVE SPACES                 TO SORT-KEY-ARRAY.               ECS027
01784                                                                   ECS027
01785      SET SKA  SCDSR  TO +1.                                       ECS027
01786                                                                   ECS027
01787      IF SCD-STANDARD-REQUESTS = SPACES                            ECS027
01788          GO TO 4405-DO-IT-FOR-DYNAMIC.                            ECS027
01789                                                                   ECS027
01790      IF SCD-STD-REQ (SCDSR) = 'X'                                 ECS027
01791 * 1--REPORT BY ACCOUNT (FULL KEY)--                               ECS027
01792          MOVE 'A'                   TO SKA-REPORT-ID (SKA)        ECS027
01793          MOVE WS-AM-CARRIER         TO SKA-HEIR-1    (SKA)        ECS027
01794          MOVE WS-AM-GROUPING        TO SKA-HEIR-2    (SKA)        ECS027
01795          MOVE WS-AM-STATE           TO SKA-HEIR-3    (SKA)        ECS027
01796          MOVE WS-AM-ACCOUNT         TO SKA-HEIR-4    (SKA)        ECS027
01797          IF SCD-ACCT-DT-RANGE-INDIC = 'Y'                         ECS027
01798              MOVE WS-AM-EXPIRE-DT      TO SKA-HEIR-5 (SKA)        ECS027
01799              SET SKA UP BY +1                                     ECS027
01800          ELSE                                                     ECS027
01801              SET SKA UP BY +1.                                    ECS027
01802                                                                   ECS027
01803      SET SCDSR UP BY +1.                                          ECS027
01804                                                                   ECS027
01805 * 2--REPORT BY GENERAL AGENT--                                    ECS027
01806      IF SCD-STD-REQ (SCDSR) = 'X'                                 ECS027
01807          PERFORM 4400-GA-GEN-OUT THRU 4400-EXIT                   ECS027
01808          VARYING INDX FROM 1 BY 1 UNTIL INDX GREATER 10.          ECS027
01809                                                                   ECS027
01810      GO TO 4400-GA-CONT.                                          ECS027
01811                                                                   ECS027
01812  4400-GA-GEN-OUT.                                                 ECS027
052814     IF WS-AM-COM-TYP (INDX) = 'O' OR 'P' OR 'G' OR 'B' or 'S'
01814          MOVE 'B'                TO SKA-REPORT-ID (SKA)           ECS027
01815          MOVE WS-AM-AGT (INDX)      TO SKA-HEIR-1 (SKA)           ECS027
01816          SET SKA UP BY +1.                                        ECS027
01817                                                                   ECS027
01818  4400-EXIT.                                                       ECS027
01819      EXIT.                                                        ECS027
01820                                                                   ECS027
01821  4400-GA-CONT.                                                    ECS027
01822      SET SCDSR UP BY +1.                                          ECS027
01823                                                                   ECS027
01824 * 3--REPORT BY ACCOUNT WITHIN G.A.                                ECS027
01825      IF SCD-STD-REQ (SCDSR) = 'X'                                 ECS027
01826          PERFORM 4401-GA-GEN-OUT THRU 4401-EXIT                   ECS027
01827          VARYING INDX FROM 1 BY 1 UNTIL INDX GREATER 10.          ECS027
01828                                                                   ECS027
01829      GO TO 4401-GA-CONT.                                          ECS027
01830                                                                   ECS027
01831  4401-GA-GEN-OUT.                                                 ECS027
052814     IF WS-AM-COM-TYP (INDX) = 'O' OR 'P' OR 'G' OR 'B' or 'S'
01833          MOVE 'C'                   TO SKA-REPORT-ID (SKA)        ECS027
01834          MOVE WS-AM-AGT (INDX)      TO SKA-HEIR-1    (SKA)        ECS027
01835          MOVE WS-AM-CARRIER         TO SKA-HEIR-2    (SKA)        ECS027
01836          MOVE WS-AM-GROUPING        TO SKA-HEIR-3    (SKA)        ECS027
01837          MOVE WS-AM-STATE           TO SKA-HEIR-4    (SKA)        ECS027
01838          MOVE WS-AM-ACCOUNT         TO SKA-HEIR-5    (SKA)        ECS027
01839          SET SKA UP BY +1.                                        ECS027
01840                                                                   ECS027
01841  4401-EXIT.                                                       ECS027
01842      EXIT.                                                        ECS027
01843                                                                   ECS027
01844  4401-GA-CONT.                                                    ECS027
01845      SET SCDSR UP BY +1.                                          ECS027
01846                                                                   ECS027
01847      IF SCD-STD-REQ (SCDSR) = 'X'                                 ECS027
01848 * 4--REPORT BY STATE - WITHIN GROUP, CARRIER--                    ECS027
01849          MOVE 'D'                   TO SKA-REPORT-ID (SKA)        ECS027
01850          MOVE WS-AM-CARRIER         TO SKA-HEIR-1    (SKA)        ECS027
01851          MOVE WS-AM-GROUPING        TO SKA-HEIR-2    (SKA)        ECS027
01852          MOVE WS-AM-STATE           TO SKA-HEIR-3    (SKA)        ECS027
01853          SET SKA UP BY +1.                                        ECS027
01854                                                                   ECS027
01855      SET SCDSR UP BY +1.                                          ECS027
01856                                                                   ECS027
01857      IF SCD-STD-REQ (SCDSR) = 'X'                                 ECS027
01858 * 5--REPORT BY CARRIER--                                          ECS027
01859          MOVE 'E'                   TO SKA-REPORT-ID (SKA)        ECS027
01860          MOVE WS-AM-CARRIER         TO SKA-HEIR-1    (SKA)        ECS027
01861          SET SKA UP BY +1.                                        ECS027
01862                                                                   ECS027
01863      SET SCDSR UP BY +1.                                          ECS027
01864                                                                   ECS027
01865      IF SCD-STD-REQ (SCDSR) = 'X'                                 ECS027
01866 * 6--REPORT BY GROUP - WITHIN CARRIER--                           ECS027
01867          MOVE 'F'                   TO SKA-REPORT-ID (SKA)        ECS027
01868          MOVE WS-AM-CARRIER         TO SKA-HEIR-1    (SKA)        ECS027
01869          MOVE WS-AM-GROUPING        TO SKA-HEIR-2    (SKA)        ECS027
01870          SET SKA UP BY +1.                                        ECS027
01871                                                                   ECS027
01872      SET SCDSR UP BY +1.                                          ECS027
01873                                                                   ECS027
01874      IF SCD-STD-REQ (SCDSR) = 'X'                                 ECS027
01875 * 7--REPORT BY RETRO GROUP--                                      ECS027
01876          MOVE 'G'                   TO SKA-REPORT-ID (SKA)        ECS027
01877          MOVE WS-AM-RET-GRP         TO SKA-HEIR-1    (SKA)        ECS027
01878          SET SKA UP BY +1.                                        ECS027
01879                                                                   ECS027
01880      SET SCDSR UP BY +1.                                          ECS027
01881                                                                   ECS027
01882      IF SCD-STD-REQ (SCDSR) = 'X'                                 ECS027
01883 * 8--REPORT BY REGION--                                           ECS027
01884          MOVE 'H'                   TO SKA-REPORT-ID (SKA)        ECS027
01885          MOVE WS-AM-FLD-1           TO SKA-HEIR-1    (SKA)        ECS027
01886          SET SKA UP BY +1.                                        ECS027
01887                                                                   ECS027
01888      SET SCDSR UP BY +1.                                          ECS027
01889                                                                   ECS027
01890      IF SCD-STD-REQ (SCDSR) = 'X'                                 ECS027
01891 * 9--REPORT BY BUSINESS TYPE--                                    ECS027
01892          MOVE 'I'                   TO SKA-REPORT-ID (SKA)        ECS027
01893          MOVE WS-AM-GPCD            TO SKA-HEIR-1    (SKA)        ECS027
01894          SET SKA UP BY +1.                                        ECS027
01895                                                                   ECS027
01896      SET SCDSR UP BY +1.                                          ECS027
01897                                                                   ECS027
01898      IF SCD-STD-REQ (SCDSR) = 'X'                                 ECS027
01899 * 10-REPORT BY REPORT CODE 1--                                    ECS027
01900          MOVE 'J'                    TO SKA-REPORT-ID (SKA)       ECS027
01901          MOVE WS-AM-REPORT-CODE-1    TO SKA-HEIR-1    (SKA)       ECS027
01902          SET SKA UP BY +1.                                        ECS027
01903                                                                   ECS027
01904      SET SCDSR UP BY +1.                                          ECS027
01905                                                                   ECS027
01906      IF SCD-STD-REQ (SCDSR) = 'X'                                 ECS027
01907 * 11-REPORT BY CARRIER - GROUP - REPORT CODE 2--                  ECS027
01908          MOVE 'K'                    TO SKA-REPORT-ID (SKA)       ECS027
01909          MOVE WS-AM-CARRIER          TO SKA-HEIR-1    (SKA)       ECS027
01910          MOVE WS-AM-GROUPING         TO SKA-HEIR-2    (SKA)       ECS027
01911          MOVE WS-AM-REPORT-CODE-2    TO SKA-HEIR-3    (SKA)       ECS027
01912          SET SKA UP BY +1.                                        ECS027
01913                                                                   ECS027
01914      IF SCD-DYN-HEIRARCHY-LIST = SPACES                           ECS027
01915          GO TO 4409-EXIT.                                         ECS027
01916      EJECT                                                        ECS027
01917  4405-DO-IT-FOR-DYNAMIC.                                          ECS027
01918      MOVE 'Z'                    TO SKA-REPORT-ID (SKA).          ECS027
01919 *                         -----REPORT CODE 1                      ECS027
01920      SET SCDDH TO +1.                                             ECS027
01921      IF SCD-D-HEIRARCHY-X (SCDDH) NOT = SPACE                     ECS027
01922          SET SKA2                  TO SCD-D-HEIRARCHY (SCDDH)     ECS027
01923          MOVE WS-AM-REPORT-CODE-1  TO SKA-HEIR (SKA, SKA2).       ECS027
01924 *                                               -----CARRIER      ECS027
01925      SET SCDDH UP BY +1.                                          ECS027
01926      IF SCD-D-HEIRARCHY-X (SCDDH) NOT = SPACE                     ECS027
01927          SET SKA2                TO SCD-D-HEIRARCHY (SCDDH)       ECS027
01928          MOVE WS-AM-CARRIER         TO SKA-HEIR (SKA, SKA2).      ECS027
01929 *                         -----GROUP                              ECS027
01930      SET SCDDH UP BY +1.                                          ECS027
01931      IF SCD-D-HEIRARCHY-X (SCDDH) NOT = SPACE                     ECS027
01932          SET SKA2                   TO SCD-D-HEIRARCHY (SCDDH)    ECS027
01933          MOVE WS-AM-GROUPING        TO SKA-HEIR (SKA, SKA2).      ECS027
01934 *                         -----REPORT CODE 2                      ECS027
01935      SET SCDDH UP BY +1.                                          ECS027
01936      IF SCD-D-HEIRARCHY-X (SCDDH) NOT = SPACE                     ECS027
01937          SET SKA2                   TO SCD-D-HEIRARCHY (SCDDH)    ECS027
01938          MOVE WS-AM-REPORT-CODE-2   TO SKA-HEIR (SKA, SKA2).      ECS027
01939 *                         -----STATE                              ECS027
01940      SET SCDDH UP BY +1.                                          ECS027
01941      IF SCD-D-HEIRARCHY-X (SCDDH) NOT = SPACE                     ECS027
01942          SET SKA2                   TO SCD-D-HEIRARCHY (SCDDH)    ECS027
01943          MOVE WS-AM-STATE           TO SKA-HEIR (SKA, SKA2).      ECS027
01944 *                         -----RETRO GROUP                        ECS027
01945      SET SCDDH UP BY +1.                                          ECS027
01946      IF SCD-D-HEIRARCHY-X (SCDDH) NOT = SPACE                     ECS027
01947          SET SKA2                   TO SCD-D-HEIRARCHY (SCDDH)    ECS027
01948          MOVE WS-AM-RET-GRP         TO SKA-HEIR (SKA, SKA2).      ECS027
01949 *                         -----ACCOUNT                            ECS027
01950      SET SCDDH UP BY +1.                                          ECS027
01951      IF SCD-D-HEIRARCHY-X (SCDDH) NOT = SPACE                     ECS027
01952          SET SKA2                   TO SCD-D-HEIRARCHY (SCDDH)    ECS027
01953          MOVE WS-AM-ACCOUNT         TO SKA-HEIR (SKA, SKA2).      ECS027
01954 *                         -----REGION                             ECS027
01955      SET SCDDH UP BY +1.                                          ECS027
01956      IF SCD-D-HEIRARCHY-X (SCDDH) NOT = SPACE                     ECS027
01957          SET SKA2                   TO SCD-D-HEIRARCHY (SCDDH)    ECS027
01958          MOVE WS-AM-FLD-1           TO SKA-HEIR (SKA, SKA2).      ECS027
01959 *                         -----BUSINESS TYPE                      ECS027
01960      SET SCDDH UP BY +1.                                          ECS027
01961      IF SCD-D-HEIRARCHY-X (SCDDH) NOT = SPACE                     ECS027
01962          SET SKA2                   TO SCD-D-HEIRARCHY (SCDDH)    ECS027
01963          MOVE WS-AM-GPCD            TO SKA-HEIR (SKA, SKA2).      ECS027
01964                                                                   ECS027
01965  4409-EXIT.                                                       ECS027
01966      EXIT.                                                        ECS027
01967  EJECT                                                            ECS027
01968  4500-BUILD-EXTR-LEAD-DETAIL.                                     ECS027
01969      MOVE SPACES                    TO PSD-LEAD-DETAIL.           ECS027
01970      MOVE WS-AM-REPORT-CODE-1       TO PSD-RPT-CD-1.              ECS027
01971      MOVE WS-AM-CARRIER             TO PSD-CARRIER.               ECS027
01972      MOVE WS-AM-GROUPING            TO PSD-GROUP.                 ECS027
01973      MOVE WS-AM-REPORT-CODE-2       TO PSD-RPT-CD-2.              ECS027
01974      MOVE WS-AM-STATE               TO PSD-STATE.                 ECS027
01975      MOVE WS-AM-ACCOUNT             TO PSD-ACCOUNT-NUMBER.        ECS027
01976      MOVE WS-AM-FLD-1               TO PSD-REGION.                ECS027
01977      MOVE WS-AM-EXPIRE-DT           TO PSD-ACCT-TERMIN-DATE.         CL*11
01978      MOVE WS-AM-NAME                TO PSD-NAME.                  ECS027
01979      MOVE WS-AM-STATUS              TO PSD-ACCT-STATUS.           ECS027
01980      MOVE WS-AM-EFFECT-DT           TO PSD-ACCT-CONTRACT-DATE.    ECS027
01981      MOVE WS-AM-ANNIVERSARY-DATE    TO PSD-ACCT-ANNIV-DATE.       ECS027
01982      MOVE WS-AM-GPCD                TO PSD-BUSINESS-TYPE.         ECS027
01983      MOVE WS-AM-RET-GRP             TO PSD-RETRO-GROUP.           ECS027
01984      MOVE WS-AM-REI-GROUP-A         TO PSD-REINS-CO.              ECS027
01985      MOVE +0                        TO PSD-RETRO-AMT-PAID.        ECS027
01986      MOVE WS-AM-LF-RET              TO PSD-ACCT-RETEN-LIFE.       ECS027
01987      MOVE WS-AM-AH-RET              TO PSD-ACCT-RETEN-AH.         ECS027
01988                                                                   ECS027
01989      PERFORM 8900-EMPTY-LOOP                                      ECS027
01990          VARYING INDX  FROM +1 BY +1                              ECS027
01991               UNTIL WS-AM-COM-TYP (INDX) = 'O' OR 'P'             ECS027
052814                           OR 'G' OR 'B' or 'S'
01993                      OR INDX = 10.                                ECS027
01994                                                                   ECS027
052814     IF WS-AM-COM-TYP (INDX) = 'O' OR 'P' OR 'G' OR 'B' or 'S'    ECS027
01996         MOVE WS-AM-AGT (INDX)       TO PSD-GA                     ECS027
01997       ELSE                                                        ECS027
01998         MOVE SPACES                 TO PSD-GA.                    ECS027
01999                                                                   ECS027
02000  4501-EXIT.                                                       ECS027
02001      EXIT.                                                        ECS027
02002  EJECT                                                            ECS027
02003  5000-READ-CERT.                                                  ECS027
02004      READ CERTS AT END                                            ECS027
02005          MOVE 'Y'                TO CERT-EOF                      ECS027
02006          GO TO 5009-EXIT.                                         ECS027
02007                                                                   ECS027
02008      ADD +1                      TO CERT-READ-COUNT.              ECS027
02009                                                                   ECS027
02010  5009-EXIT.                                                       ECS027
02011      EXIT.                                                        ECS027
02012                                                                   ECS027
02013  5100-READ-ACCOUNT.                                               ECS027
02014      IF ACCT-EOF = 'Y'                                            ECS027
02015          GO TO 5109-EXIT.                                         ECS027
02016                                                                   ECS027
02017      READ ERACCTT.                                                ECS027
02018                                                                   ECS027
02019      IF ERACCT-FILE-STATUS = '10'                                 ECS027
02020          MOVE 'Y'                TO ACCT-EOF                      ECS027
02021          GO TO 5109-EXIT.                                         ECS027
02022                                                                   ECS027
02023      IF ERACCT-FILE-STATUS NOT = '00'                             ECS027
02024          MOVE 'ERROR READING ERACCT MSTR'  TO  WS-ABEND-MESSAGE   ECS027
02025          MOVE +0301                        TO  WS-RETURN-CODE     ECS027
02026          GO TO ABEND-PGM.                                         ECS027
02027                                                                   ECS027
02028      ADD +1 TO ACCT-READ-COUNT.                                   ECS027
02029                                                                   ECS027
02030      IF FIRST-TIME-SW = 'Y'                                       ECS027
02031          MOVE 'N'                TO  FIRST-TIME-SW                ECS027
02032          MOVE AM-MSTR-CNTRL      TO  HOLD-ACCT-DATA.              ECS027
02033                                                                   ECS027
02034      IF SCD-ACCT-DT-RANGE-INDIC = 'N'                             ECS027
02035          IF AM-CONTROL-A = H-ACCT-KEY                             ECS027
02036              MOVE ACCOUNT-MASTER TO  WS-AM-REC                    ECS027
02037              GO TO 5100-READ-ACCOUNT                              ECS027
02038          ELSE                                                     ECS027
02039              NEXT SENTENCE                                        ECS027
02040      ELSE                                                         ECS027
02041          MOVE ACCOUNT-MASTER     TO WS-AM-REC.                    ECS027
02042                                                                   ECS027
02043      MOVE 'Y'                    TO ACCT-QUALIFIED.               ECS027
02044                                                                   ECS027
02045      IF SCD-L-RPT-CD-1 NOT = SPACE                                ECS027
02046          IF SCD-L-RPT-CD-1 NOT = WS-AM-REPORT-CODE-1              ECS027
02047              MOVE 'N'            TO ACCT-QUALIFIED.               ECS027
02048                                                                   ECS027
02049      IF SCD-L-CARRIER NOT = SPACE                                 ECS027
02050          IF SCD-L-CARRIER NOT = WS-AM-CARRIER                     ECS027
02051              MOVE 'N'            TO ACCT-QUALIFIED.               ECS027
02052                                                                   ECS027
02053      IF SCD-L-GROUP NOT = SPACES                                  ECS027
02054          IF SCD-L-GROUP NOT = WS-AM-GROUPING                      ECS027
02055              MOVE 'N'            TO ACCT-QUALIFIED.               ECS027
02056                                                                   ECS027
02057      IF SCD-L-RPT-CD-2 NOT = SPACE                                ECS027
02058          IF SCD-L-RPT-CD-2 NOT = WS-AM-REPORT-CODE-2              ECS027
02059              MOVE 'N'            TO ACCT-QUALIFIED.               ECS027
02060                                                                   ECS027
02061      IF SCD-L-STATE NOT = SPACES                                  ECS027
02062          IF SCD-L-STATE NOT = WS-AM-STATE                         ECS027
02063              MOVE 'N'            TO ACCT-QUALIFIED.               ECS027
02064                                                                   ECS027
02065      IF SCD-L-RETRO-GROUP NOT = SPACES                            ECS027
02066          IF SCD-L-RETRO-GROUP NOT = WS-AM-RET-GRP                 ECS027
02067              MOVE 'N'            TO ACCT-QUALIFIED.               ECS027
02068                                                                   ECS027
02069      IF SCD-L-ACCOUNT NOT = SPACES                                ECS027
02070          IF SCD-L-ACCOUNT NOT = WS-AM-ACCOUNT                     ECS027
02071              MOVE 'N'            TO ACCT-QUALIFIED.               ECS027
02072                                                                   ECS027
02073      IF SCD-L-REGION NOT = SPACES                                 ECS027
02074          IF SCD-L-REGION NOT = WS-AM-FLD-1                        ECS027
02075              MOVE 'N'            TO ACCT-QUALIFIED.               ECS027
02076                                                                   ECS027
02077      IF SCD-BUSINESS-TYPE NOT = SPACES                            ECS027
02078          IF SCD-BUS-TYPE NOT = WS-AM-GPCD                         ECS027
02079              MOVE 'N'            TO ACCT-QUALIFIED.               ECS027
02080                                                                   ECS027
02081 ***********   CHECK IF ACTIVE ACCOUNTS ONLY TO BE SELECTED        ECS027
02082      IF CUSTOMIZATION-FOUND                                       ECS027
02083          IF CF-ACTIVE-ACCOUNTS                                    ECS027
02084              IF WS-AM-STATUS NOT = '0'                            ECS027
02085                  MOVE 'N'            TO ACCT-QUALIFIED.           ECS027
02086 ***********                                                       ECS027
02087                                                                   ECS027
02088      IF ACCT-QUALIFIED = 'N'                                      ECS027
02089          MOVE ACCOUNT-MASTER     TO WS-AM-REC                     ECS027
02090          MOVE AM-MSTR-CNTRL      TO HOLD-ACCT-DATA                ECS027
02091          GO TO  5100-READ-ACCOUNT.                                ECS027
02092                                                                   ECS027
02093      MOVE AM-MSTR-CNTRL          TO HOLD-NEW-ACCT-DATA.           ECS027
02094                                                                   ECS027
02095  5109-EXIT.                                                       ECS027
02096      EXIT.                                                        ECS027
02097                                                                   ECS027
02098  5200-READ-CLAIM.                                                 ECS027
02099      READ EXTR-CLAIM INTO CLAIM-EXTRACT AT END                    ECS027
02100          MOVE 'Y'                TO CLAIM-EOF                     ECS027
02101          GO TO 5209-EXIT.                                         ECS027
02102                                                                   ECS027
02103      ADD +1 TO CLAIM-READ-COUNT.                                  ECS027
02104                                                                   ECS027
02105      MOVE CLM-ACCOUNT-CONTROL    TO H-CLM-ACCT-KEY.               ECS027
02106      MOVE CLM-EFF-DATE           TO H-CLM-EFFECT-DATE.               CL*11
02107                                                                   ECS027
02108  5209-EXIT.                                                       ECS027
02109      EXIT.                                                        ECS027
02110                                                                   ECS027
02111  5300-READ-EPEC.                                                  ECS027
02112      READ EPEC INTO EP-RECORD AT END                              ECS027
02113          MOVE 'Y'                TO EPEC-EOF                      ECS027
02114          GO TO 5309-EXIT.                                         ECS027
02115                                                                   ECS027
02116      ADD +1 TO EPEC-READ-COUNT.                                   ECS027
02117                                                                   ECS027
02118      IF EP-RECORD-ID = 'EP' OR 'EC'                               ECS027
02119          MOVE EP-CONTROL         TO HOLD-EPEC-ACCT-KEY            ECS027
02120          GO TO 5309-EXIT                                          ECS027
02121      ELSE                                                         ECS027
02122          GO TO 5300-READ-EPEC.                                    ECS027
02123                                                                   ECS027
02124  5309-EXIT.                                                       ECS027
02125      EXIT.                                                        ECS027
02126  EJECT                                                            ECS027
02127  7000-EPEC-ACCUMS.                                                ECS027
02128      MOVE ZEROS                  TO HOLD-COUNT    HOLD-COMMIS     ECS027
02129                                     HOLD-PREM     HOLD-CLM-INCUR  ECS027
02130                                     HOLD-EARNED   HOLD-CLM-PD.    ECS027
02131                                                                   ECS027
02132  7001-PROCESS-EARNINGS-REC.                                       ECS027
02133      IF EP-RECORD-ID NOT = 'EP'                                   ECS027
02134          MOVE +1                TO INDX                           ECS027
02135          GO TO 7002-PROCESS-COMM-REC.                             ECS027
02136                                                                   ECS027
02137      SUBTRACT EP-CNC-CNT FROM EP-ISS-CNT GIVING HOLD-COUNT.       ECS027
02138      SUBTRACT EP-CNC-PRM FROM EP-ISS-PRM GIVING HOLD-PREM.        ECS027
02139                                                                   ECS027
02140      IF EP-RCD-TYPE = LIFE-OVERRIDE-L1                            ECS027
02141             PERFORM 8900-EMPTY-LOOP                               ECS027
02142              VARYING CLAS-INDEXL FROM CLAS-STARTL BY +1           ECS027
02143                   UNTIL EP-BEN-CODE =                             ECS027
02144                         CLAS-I-BEN (CLAS-INDEXL)                  ECS027
02145                         OR CLAS-INDEXL = CLAS-MAXL                ECS027
02146              IF EP-BEN-CODE NOT = CLAS-I-BEN (CLAS-INDEXL)        ECS027
02147                  DISPLAY 'ECS027E 7000 -NO MATCH ON LIFE BENEFIT' ECS027
02148                          'TYPE- EP-BEN-CODE =' EP-BEN-CODE        ECS027
02149                  DISPLAY '             -EPEC REC. CONTROL='       ECS027
02150                      EP-CONTROL                                   ECS027
02151                  MOVE '7001 -NO MATCH ON LIFE BENEFIT' TO         ECS027
02152                                                WS-ABEND-MESSAGE   ECS027
02153                  MOVE +0401                TO  WS-RETURN-CODE     ECS027
02154                  GO TO ABEND-PGM.                                 ECS027
02155                                                                   ECS027
02156      IF EP-RCD-TYPE = AH-OVERRIDE-L1                              ECS027
02157             PERFORM 8900-EMPTY-LOOP                               ECS027
02158              VARYING CLAS-INDEXA FROM CLAS-STARTA BY +1           ECS027
02159                   UNTIL EP-BEN-CODE =                             ECS027
02160                         CLAS-I-BEN (CLAS-INDEXA)                  ECS027
02161                         OR CLAS-INDEXA = CLAS-MAXA                ECS027
02162              IF EP-BEN-CODE NOT = CLAS-I-BEN (CLAS-INDEXA)        ECS027
02163                  DISPLAY 'ECS027E 7000 -NO MATCH ON A/H BENEFIT'  ECS027
02164                          'TYPE- EP-BEN-CODE =' EP-BEN-CODE        ECS027
02165                  DISPLAY '             -EPEC REC. CONTROL='       ECS027
02166                      EP-CONTROL                                   ECS027
02167                  MOVE '7001 -NO MATCH ON A/H BENEFIT' TO          ECS027
02168                                             WS-ABEND-MESSAGE      ECS027
02169                  MOVE +0401             TO  WS-RETURN-CODE        ECS027
02170                  GO TO ABEND-PGM.                                 ECS027
02171                                                                   ECS027
02172      PERFORM 8900-EMPTY-LOOP                                      ECS027
02173         VARYING CLAS-INDEXS FROM CLAS-STARTS BY +1                ECS027
02174                   UNTIL EP-STATE = STATE-SUB (CLAS-INDEXS)  OR    ECS027
02175                                    CLAS-INDEXS = CLAS-MAXS.       ECS027
02176                                                                   ECS027
02177      IF (EP-RCD-TYPE = LIFE-OVERRIDE-L1  AND                      ECS027
02178                                    CLAS-I-EP (CLAS-INDEXL) = 'U') ECS027
02179          MOVE 'R'                TO CLAS-I-EP (CLAS-INDEXL).      ECS027
02180                                                                   ECS027
02181      IF (EP-RCD-TYPE = LIFE-OVERRIDE-L1  AND                      ECS027
02182                                    CLAS-I-EP (CLAS-INDEXL) = 'R') ECS027
02183              MOVE EP-PRM-78      TO HOLD-EARNED.                  ECS027
02184                                                                   ECS027
02185      IF (EP-RCD-TYPE = AH-OVERRIDE-L1  AND                        ECS027
02186                                    CLAS-I-EP (CLAS-INDEXA) = 'R') ECS027
02187           IF DTE-CLIENT = 'POS' AND EP-CARRIER = '2'              ECS027
02188               COMPUTE HOLD-EARNED =                               ECS027
02189                   ((EP-PRM-PR + EP-PRM-78) / +2)                  ECS027
02190           ELSE                                                    ECS027
02191              MOVE EP-PRM-78      TO HOLD-EARNED.                  ECS027
02192                                                                   ECS027
02193      IF EP-RCD-TYPE = LIFE-OVERRIDE-L1  AND                       ECS027
02194                            (CLAS-I-EP (CLAS-INDEXL) = 'T' OR 'N') ECS027
02195              MOVE EP-PRM-78      TO HOLD-EARNED.                  ECS027
02196                                                                   ECS027
02197      IF (EP-RCD-TYPE = LIFE-OVERRIDE-L1  AND                      ECS027
02198                               CLAS-I-EP (CLAS-INDEXL) = '1')  OR  ECS027
02199         (EP-RCD-TYPE = AH-OVERRIDE-L1  AND                        ECS027
02200                               CLAS-I-EP (CLAS-INDEXA) = '1')      ECS027
02201                  COMPUTE HOLD-EARNED =                            ECS027
02202                      (+.6667 * EP-PRM-PR) + (+.3333 * EP-PRM-78). ECS027
02203                                                                   ECS027
02204      IF (EP-RCD-TYPE = LIFE-OVERRIDE-L1  AND                      ECS027
02205                               CLAS-I-EP (CLAS-INDEXL) = 'P')  OR  ECS027
02206         (EP-RCD-TYPE = AH-OVERRIDE-L1  AND                        ECS027
02207                               CLAS-I-EP (CLAS-INDEXA) = 'P')  OR  ECS027
02208         (STATE-ABBR (CLAS-INDEXS) = 'WY')                         ECS027
02209                    MOVE EP-PRM-PR TO HOLD-EARNED.                 ECS027
02210                                                                   ECS027
02211      IF (EP-RCD-TYPE = LIFE-OVERRIDE-L1  AND                      ECS027
02212            CLAS-I-EP (CLAS-INDEXL) = 'B' OR 'K' OR 'L')           ECS027
02213                    MOVE EP-PRM-ST TO HOLD-EARNED.                 ECS027
02214                                                                   ECS027
02215      IF DTE-CLIENT = 'GIC'                                        ECS027
02216          IF EP-RCD-TYPE = AH-OVERRIDE-L1 AND                      ECS027
02217             CLAS-I-BAL (CLAS-INDEXA) NOT = 'B'                    ECS027
02218              COMPUTE HOLD-EARNED =                                ECS027
02219                   (+.80 * EP-PRM-PR) + (+.20 * EP-PRM-78).        ECS027
02220                                                                   ECS027
02221       IF DTE-CLIENT = 'ADL' OR 'ALA' OR 'FLB'                     ECS027
02222           IF EP-RCD-TYPE = AH-OVERRIDE-L1                         ECS027
02223               COMPUTE HOLD-EARNED =                               ECS027
02224                   (EP-PRM-PR + EP-PRM-78) / 2.                    ECS027
02225                                                                   ECS027
02226 ******* COMPUTE EARNED-PREMIUM ON MEAN BASIS                      ECS027
02227      IF (EP-RCD-TYPE = LIFE-OVERRIDE-L1  AND                      ECS027
02228                               CLAS-I-EP (CLAS-INDEXL) = 'M')  OR  ECS027
02229         (EP-RCD-TYPE = AH-OVERRIDE-L1  AND                        ECS027
02230                               CLAS-I-EP (CLAS-INDEXA) = 'M')      ECS027
02231               COMPUTE HOLD-EARNED =                               ECS027
02232                   ((EP-PRM-PR + EP-PRM-78) / +2).                 ECS027
02233 *******                                                           ECS027
02234                                                                   ECS027
02235       MOVE EP-CLM-AMT            TO HOLD-CLM-PD.                  ECS027
02236                                                                   ECS027
02237       PERFORM 8200-GET-CLAIM-INCURRED-AMT THRU 8201-EXIT.         ECS027
02238                                                                   ECS027
02239       MOVE AMT-SUM               TO HOLD-CLM-INCUR.               ECS027
02240                                                                   ECS027
02241  7002-PROCESS-COMM-REC.                                           ECS027
02242      IF EC-RECORD-ID NOT = 'EC'                                   ECS027
02243          GO TO 7003-PROCESS-CONT.                                 ECS027
02244                                                                   ECS027
052814     IF EC-AGT-TYPE (INDX) = ('C' OR 'D' OR 'F')
02246             COMPUTE HOLD-COMMIS  =  HOLD-COMMIS +                 ECS027
02247                        (EC-ISS-COMM (INDX) - EC-CNC-COMM (INDX)). ECS027
02248                                                                   ECS027
02249      ADD +1  TO  INDX.                                            ECS027
02250      IF INDX LESS THAN +6                                         ECS027
02251          GO TO 7002-PROCESS-COMM-REC.                             ECS027
02252                                                                   ECS027
02253  7003-PROCESS-CONT.                                               ECS027
02254      MOVE EP-RUN-DTE             TO WS-EP-RUN-DTE-N.                 CL*16
02255                                                                   ECS027
02256      MOVE 'Y'                    TO INDEX-VALID.                  ECS027
02257      PERFORM 8300-SET-WSDM-INDEX THRU 8301-EXIT.                  ECS027
02258                                                                   ECS027
02259      IF EP-PURGE NOT = 'P'                                        ECS027
02260         IF INDEX-VALID EQUAL 'Y'                                  ECS027
02261            PERFORM 7010-GET-RETROS-AMOUNT      THRU 7011-EXIT     ECS027
02262            ADD AMT-SUM TO PSD-RETRO-AMT-PAID                      ECS027
02263            PERFORM 7080-EPEC-ADDS-TO-HOLD-AMTS THRU 7081-EXIT.    ECS027
02264                                                                   ECS027
02265      IF EP-PURGE = 'P'                                            ECS027
02266         IF INDEX-VALID EQUAL 'Y'                                  ECS027
02267             PERFORM 7020-PURGE-ADDITIONS THRU 7021-EXIT.          ECS027
02268                                                                   ECS027
02269  7001-EXIT.                                                       ECS027
02270      EXIT.                                                        ECS027
02271                                                                   ECS027
02272  7010-GET-RETROS-AMOUNT.                                          ECS027
02273      IF EP-RECORD-ID = 'EP'                                       ECS027
02274          COMPUTE AMT-SUM = EP-RETRO-PAYMENTS                      ECS027
02275                          + EP-RETRO-OTH-COMM                      ECS027
02276                          - EP-RETRO-EXPENSES                      ECS027
02277      ELSE                                                         ECS027
02278          MOVE +0                 TO AMT-SUM.                      ECS027
02279                                                                   ECS027
02280  7011-EXIT.                                                       ECS027
02281      EXIT.                                                        ECS027
02282                                                                   ECS027
02283  7020-PURGE-ADDITIONS.                                            ECS027
02284      IF WSDM NOT GREATER +13                                      ECS027
02285          PERFORM 7080-EPEC-ADDS-TO-HOLD-AMTS THRU 7081-EXIT       ECS027
02286          SET WSDM UP BY +1                                        ECS027
02287          GO TO 7020-PURGE-ADDITIONS.                              ECS027
02288                                                                   ECS027
02289  7021-EXIT.                                                       ECS027
02290      EXIT.                                                        ECS027
02291                                                                   ECS027
02292  7080-EPEC-ADDS-TO-HOLD-AMTS.                                     ECS027
02293      IF EP-RECORD-ID = 'EP'                                       ECS027
02294          IF EP-RCD-TYPE = LIFE-OVERRIDE-L1                        ECS027
02295              ADD HOLD-COUNT      TO WSD-L-COUNT     (WSDM)        ECS027
02296              ADD HOLD-PREM       TO WSD-L-PREM      (WSDM)        ECS027
02297              ADD HOLD-EARNED     TO WSD-L-EARN-PREM (WSDM)        ECS027
02298              ADD HOLD-CLM-INCUR  TO WSD-L-CLM-INCUR (WSDM)        ECS027
02299              ADD HOLD-CLM-PD     TO WSD-L-CLM-PAID  (WSDM)        ECS027
02300          ELSE                                                     ECS027
02301              ADD HOLD-COUNT      TO WSD-A-COUNT     (WSDM)        ECS027
02302              ADD HOLD-PREM       TO WSD-A-PREM      (WSDM)        ECS027
02303              ADD HOLD-EARNED     TO WSD-A-EARN-PREM (WSDM)        ECS027
02304              ADD HOLD-CLM-INCUR  TO WSD-A-CLM-INCUR (WSDM)        ECS027
02305              ADD HOLD-CLM-PD     TO WSD-A-CLM-PAID  (WSDM)        ECS027
02306      ELSE                                                         ECS027
02307          IF EP-RCD-TYPE = LIFE-OVERRIDE-L1                        ECS027
02308              ADD HOLD-COMMIS     TO WSD-L-COMMIS    (WSDM)        ECS027
02309          ELSE                                                     ECS027
02310              ADD HOLD-COMMIS     TO WSD-A-COMMIS    (WSDM).       ECS027
02311                                                                   ECS027
02312  7081-EXIT.                                                       ECS027
02313      EXIT.                                                        ECS027
02314  EJECT                                                            ECS027
02315  7090-ADD-HOLD-EPEC-AMTS.                                         ECS027
02316      MOVE WSD-L-PREM (WSDM)      TO AMOUNT-1.                     ECS027
02317      SET WSDM UP BY +1.                                           ECS027
02318      MOVE WSD-L-PREM (WSDM)      TO AMOUNT-2.                     ECS027
02319      SUBTRACT AMOUNT-1 FROM AMOUNT-2 GIVING AMT-DIFF.             ECS027
02320      ADD AMT-DIFF TO PSD-NET-WRITTEN-PREM (PSDGP).                ECS027
02321                                                                   ECS027
02322      SET WSDM DOWN BY +1.                                         ECS027
02323      MOVE WSD-L-EARN-PREM (WSDM) TO AMOUNT-1.                     ECS027
02324      SET WSDM UP BY +1.                                           ECS027
02325      MOVE WSD-L-EARN-PREM (WSDM) TO AMOUNT-2.                     ECS027
02326      SUBTRACT AMOUNT-1 FROM AMOUNT-2 GIVING AMT-DIFF.             ECS027
02327      ADD AMT-DIFF TO PSD-EARNED-PREM (PSDGP).                     ECS027
02328                                                                   ECS027
02329      SET WSDM DOWN BY +1.                                         ECS027
02330      MOVE WSD-L-CLM-PAID (WSDM)  TO AMOUNT-1.                     ECS027
02331      SET WSDM UP BY +1.                                           ECS027
02332      MOVE WSD-L-CLM-PAID (WSDM)  TO AMOUNT-2.                     ECS027
02333      SUBTRACT AMOUNT-1 FROM AMOUNT-2 GIVING AMT-DIFF.             ECS027
02334      ADD AMT-DIFF TO PSD-CLAIMS-PAID (PSDGP).                     ECS027
02335                                                                   ECS027
02336      SET WSDM DOWN BY +1.                                         ECS027
02337      MOVE WSD-L-CLM-INCUR (WSDM) TO AMOUNT-1.                     ECS027
02338      SET WSDM UP BY +1.                                           ECS027
02339      MOVE WSD-L-CLM-INCUR (WSDM) TO AMOUNT-2.                     ECS027
02340      SUBTRACT AMOUNT-1 FROM AMOUNT-2 GIVING AMT-DIFF.             ECS027
02341      ADD AMT-DIFF TO PSD-CLAIMS-PAID (PSDGP).                     ECS027
02342                                                                   ECS027
02343      SET WSDM DOWN BY +1.                                         ECS027
02344      MOVE WSD-A-PREM (WSDM)      TO AMOUNT-1.                     ECS027
02345      SET WSDM UP BY +1.                                           ECS027
02346      MOVE WSD-A-PREM (WSDM)      TO AMOUNT-2.                     ECS027
02347      SUBTRACT AMOUNT-1 FROM AMOUNT-2 GIVING AMT-DIFF.             ECS027
02348      ADD AMT-DIFF TO PSD-NET-WRITTEN-PREM (PSDGP).                ECS027
02349                                                                   ECS027
02350      SET WSDM DOWN BY +1.                                         ECS027
02351      MOVE WSD-A-EARN-PREM (WSDM) TO AMOUNT-1.                     ECS027
02352      SET WSDM UP BY +1.                                           ECS027
02353      MOVE WSD-A-EARN-PREM (WSDM) TO AMOUNT-2.                     ECS027
02354      SUBTRACT AMOUNT-1 FROM AMOUNT-2 GIVING AMT-DIFF.             ECS027
02355      ADD AMT-DIFF TO PSD-EARNED-PREM (PSDGP).                     ECS027
02356                                                                   ECS027
02357      SET WSDM DOWN BY +1.                                         ECS027
02358      MOVE WSD-A-CLM-PAID (WSDM)  TO AMOUNT-1.                     ECS027
02359      SET WSDM UP BY +1.                                           ECS027
02360      MOVE WSD-A-CLM-PAID (WSDM)  TO AMOUNT-2.                     ECS027
02361      SUBTRACT AMOUNT-1 FROM AMOUNT-2 GIVING AMT-DIFF.             ECS027
02362      ADD AMT-DIFF TO PSD-CLAIMS-PAID (PSDGP).                     ECS027
02363                                                                   ECS027
02364      SET WSDM DOWN BY +1.                                         ECS027
02365      MOVE WSD-A-CLM-INCUR (WSDM) TO AMOUNT-1.                     ECS027
02366      SET WSDM UP BY +1.                                           ECS027
02367      MOVE WSD-A-CLM-INCUR (WSDM) TO AMOUNT-2.                     ECS027
02368      SUBTRACT AMOUNT-1 FROM AMOUNT-2 GIVING AMT-DIFF.             ECS027
02369      ADD AMT-DIFF TO PSD-CLAIMS-PAID (PSDGP).                     ECS027
02370                                                                   ECS027
02371      IF WSDM = +4 OR +7 OR +10 OR +13                             ECS027
02372          PERFORM 7095-ADD-QTR-AMTS                                ECS027
02373          SET PSDM  UP BY +1.                                      ECS027
02374                                                                   ECS027
02375      SET PSDGP UP BY +1.                                          ECS027
02376                                                                   ECS027
02377  7091-EXIT.                                                       ECS027
02378      EXIT.                                                        ECS027
02379  EJECT                                                            ECS027
02380  7095-ADD-QTR-AMTS.                                               ECS027
02381      SET WSDM DOWN BY +3.                                         ECS027
02382      MOVE WSD-L-COUNT (WSDM)     TO AMOUNT-1.                     ECS027
02383      SET WSDM UP BY +3.                                           ECS027
02384      MOVE WSD-L-COUNT (WSDM)     TO AMOUNT-2.                     ECS027
02385      SUBTRACT AMOUNT-1 FROM AMOUNT-2 GIVING AMT-DIFF.             ECS027
02386      ADD AMT-DIFF TO PSD-ML-COUNT (PSDM).                         ECS027
02387                                                                   ECS027
02388      SET WSDM DOWN BY +3.                                         ECS027
02389      MOVE WSD-L-PREM (WSDM)      TO AMOUNT-1.                     ECS027
02390      SET WSDM UP BY +3.                                           ECS027
02391      MOVE WSD-L-PREM (WSDM)      TO AMOUNT-2.                     ECS027
02392      SUBTRACT AMOUNT-1 FROM AMOUNT-2 GIVING AMT-DIFF.             ECS027
02393      ADD AMT-DIFF TO PSD-ML-PREM (PSDM).                          ECS027
02394                                                                   ECS027
02395      SET WSDM DOWN BY +3.                                         ECS027
02396      MOVE WSD-L-EARN-PREM (WSDM) TO AMOUNT-1.                     ECS027
02397      SET WSDM UP BY +3.                                           ECS027
02398      MOVE WSD-L-EARN-PREM (WSDM) TO AMOUNT-2.                     ECS027
02399      SUBTRACT AMOUNT-1 FROM AMOUNT-2 GIVING AMT-DIFF.             ECS027
02400      ADD AMT-DIFF TO PSD-ML-EARN-PREM (PSDM).                     ECS027
02401                                                                   ECS027
02402      SET WSDM DOWN BY +3.                                         ECS027
02403      MOVE WSD-L-COMMIS (WSDM)    TO AMOUNT-1.                     ECS027
02404      SET WSDM UP BY +3.                                           ECS027
02405      MOVE WSD-L-COMMIS (WSDM)    TO AMOUNT-2.                     ECS027
02406      SUBTRACT AMOUNT-1 FROM AMOUNT-2 GIVING AMT-DIFF.             ECS027
02407      ADD AMT-DIFF TO PSD-ML-COMMIS (PSDM).                        ECS027
02408                                                                   ECS027
02409      SET WSDM DOWN BY +3.                                         ECS027
02410      MOVE WSD-L-CLM-INCUR (WSDM) TO AMOUNT-1.                     ECS027
02411      SET WSDM UP BY +3.                                           ECS027
02412      MOVE WSD-L-CLM-INCUR (WSDM) TO AMOUNT-2.                     ECS027
02413      SUBTRACT AMOUNT-1 FROM AMOUNT-2 GIVING AMT-DIFF.             ECS027
02414      ADD AMT-DIFF TO PSD-ML-CLAIM-INCURRED (PSDM).                ECS027
02415                                                                   ECS027
02416      SET WSDM DOWN BY +3.                                         ECS027
02417      MOVE WSD-L-CLM-PAID (WSDM)  TO AMOUNT-1.                     ECS027
02418      SET WSDM UP BY +3.                                           ECS027
02419      MOVE WSD-L-CLM-PAID (WSDM)  TO AMOUNT-2.                     ECS027
02420      SUBTRACT AMOUNT-1 FROM AMOUNT-2 GIVING AMT-DIFF.             ECS027
02421      ADD AMT-DIFF TO PSD-ML-CLAIMS-PAID (PSDM).                   ECS027
02422                                                                   ECS027
02423      ADD PSD-ML-CLAIMS-PAID (PSDM)                                ECS027
02424             TO PSD-ML-CLAIM-INCURRED (PSDM).                      ECS027
02425                                                                   ECS027
02426      SET WSDM DOWN BY +3.                                         ECS027
02427      MOVE WSD-A-COUNT (WSDM)     TO AMOUNT-1.                     ECS027
02428      SET WSDM UP BY +3.                                           ECS027
02429      MOVE WSD-A-COUNT (WSDM)     TO AMOUNT-2.                     ECS027
02430      SUBTRACT AMOUNT-1 FROM AMOUNT-2 GIVING AMT-DIFF.             ECS027
02431      ADD AMT-DIFF TO PSD-MA-COUNT (PSDM).                         ECS027
02432                                                                   ECS027
02433      SET WSDM DOWN BY +3.                                         ECS027
02434      MOVE WSD-A-PREM (WSDM)      TO AMOUNT-1.                     ECS027
02435      SET WSDM UP BY +3.                                           ECS027
02436      MOVE WSD-A-PREM (WSDM)      TO AMOUNT-2.                     ECS027
02437      SUBTRACT AMOUNT-1 FROM AMOUNT-2 GIVING AMT-DIFF.             ECS027
02438      ADD AMT-DIFF TO PSD-MA-PREM (PSDM).                          ECS027
02439                                                                   ECS027
02440      SET WSDM DOWN BY +3.                                         ECS027
02441      MOVE WSD-A-EARN-PREM (WSDM) TO AMOUNT-1.                     ECS027
02442      SET WSDM UP BY +3.                                           ECS027
02443      MOVE WSD-A-EARN-PREM (WSDM) TO AMOUNT-2.                     ECS027
02444      SUBTRACT AMOUNT-1 FROM AMOUNT-2 GIVING AMT-DIFF.             ECS027
02445      ADD AMT-DIFF TO PSD-MA-EARN-PREM (PSDM).                     ECS027
02446                                                                   ECS027
02447      SET WSDM DOWN BY +3.                                         ECS027
02448      MOVE WSD-A-COMMIS (WSDM)    TO AMOUNT-1.                     ECS027
02449      SET WSDM UP BY +3.                                           ECS027
02450      MOVE WSD-A-COMMIS (WSDM)    TO AMOUNT-2.                     ECS027
02451      SUBTRACT AMOUNT-1 FROM AMOUNT-2 GIVING AMT-DIFF.             ECS027
02452      ADD AMT-DIFF TO PSD-MA-COMMIS (PSDM).                        ECS027
02453                                                                   ECS027
02454      SET WSDM DOWN BY +3.                                         ECS027
02455      MOVE WSD-A-CLM-INCUR (WSDM) TO AMOUNT-1.                     ECS027
02456      SET WSDM UP BY +3.                                           ECS027
02457      MOVE WSD-A-CLM-INCUR (WSDM) TO AMOUNT-2.                     ECS027
02458      SUBTRACT AMOUNT-1 FROM AMOUNT-2 GIVING AMT-DIFF.             ECS027
02459      ADD AMT-DIFF TO PSD-MA-CLAIM-INCURRED (PSDM).                ECS027
02460                                                                   ECS027
02461      SET WSDM DOWN BY +3.                                         ECS027
02462      MOVE WSD-A-CLM-PAID (WSDM)  TO AMOUNT-1.                     ECS027
02463      SET WSDM UP BY +3.                                           ECS027
02464      MOVE WSD-A-CLM-PAID (WSDM)  TO AMOUNT-2.                     ECS027
02465      SUBTRACT AMOUNT-1 FROM AMOUNT-2 GIVING AMT-DIFF.             ECS027
02466      ADD AMT-DIFF TO PSD-MA-CLAIMS-PAID (PSDM).                   ECS027
02467                                                                   ECS027
02468      ADD  PSD-MA-CLAIMS-PAID (PSDM)                               ECS027
02469                   TO PSD-MA-CLAIM-INCURRED (PSDM).                ECS027
02470                                                                   ECS027
02471  7098-SET-L12-YTD-ITD-AMTS.                                       ECS027
02472       SET WSDM TO +13.                                            ECS027
02473       SET PSDM TO +7.                                             ECS027
02474       MOVE WSD-L-COUNT     (WSDM) TO PSD-ML-COUNT          (PSDM).ECS027
02475       MOVE WSD-L-PREM      (WSDM) TO PSD-ML-PREM           (PSDM).ECS027
02476       MOVE WSD-L-EARN-PREM (WSDM) TO PSD-ML-EARN-PREM      (PSDM).ECS027
02477       MOVE WSD-L-COMMIS    (WSDM) TO PSD-ML-COMMIS         (PSDM).ECS027
02478       MOVE WSD-L-CLM-INCUR (WSDM) TO PSD-ML-CLAIM-INCURRED (PSDM).ECS027
02479       MOVE WSD-L-CLM-PAID  (WSDM) TO PSD-ML-CLAIMS-PAID    (PSDM).ECS027
02480       MOVE WSD-A-COUNT     (WSDM) TO PSD-MA-COUNT          (PSDM).ECS027
02481       MOVE WSD-A-PREM      (WSDM) TO PSD-MA-PREM           (PSDM).ECS027
02482       MOVE WSD-A-EARN-PREM (WSDM) TO PSD-MA-EARN-PREM      (PSDM).ECS027
02483       MOVE WSD-A-COMMIS    (WSDM) TO PSD-MA-COMMIS         (PSDM).ECS027
02484       MOVE WSD-A-CLM-INCUR (WSDM) TO PSD-MA-CLAIM-INCURRED (PSDM).ECS027
02485       MOVE WSD-A-CLM-PAID  (WSDM) TO PSD-MA-CLAIMS-PAID    (PSDM).ECS027
02486                                                                   ECS027
02487       ADD  PSD-MA-CLAIMS-PAID (PSDM)                              ECS027
02488                  TO PSD-MA-CLAIM-INCURRED (PSDM).                 ECS027
02489       ADD  PSD-ML-CLAIMS-PAID (PSDM)                              ECS027
02490                  TO PSD-ML-CLAIM-INCURRED (PSDM).                 ECS027
02491                                                                   ECS027
02492       SET WSDM TO +1.                                             ECS027
02493       MOVE WSD-MONTH-ARRAY (WSDM) TO HOLD-AMOUNTS.                ECS027
02494       SET WSDM TO +13.                                            ECS027
02495       SET PSDM TO +5.                                             ECS027
02496                                                                   ECS027
02497       SUBTRACT HOLD-COUNT       FROM WSD-L-COUNT     (WSDM)       ECS027
02498           GIVING PSD-ML-COUNT          (PSDM).                    ECS027
02499       SUBTRACT HOLD-PREM        FROM WSD-L-PREM      (WSDM)       ECS027
02500           GIVING PSD-ML-PREM           (PSDM).                    ECS027
02501       SUBTRACT HOLD-EARNED      FROM WSD-L-EARN-PREM (WSDM)       ECS027
02502           GIVING PSD-ML-EARN-PREM      (PSDM).                    ECS027
02503       SUBTRACT HOLD-COMMIS      FROM WSD-L-COMMIS    (WSDM)       ECS027
02504           GIVING PSD-ML-COMMIS         (PSDM).                    ECS027
02505       SUBTRACT HOLD-CLM-INCUR   FROM WSD-L-CLM-INCUR (WSDM)       ECS027
02506           GIVING PSD-ML-CLAIM-INCURRED (PSDM).                    ECS027
02507       SUBTRACT HOLD-CLM-PD      FROM WSD-L-CLM-PAID  (WSDM)       ECS027
02508           GIVING PSD-ML-CLAIMS-PAID    (PSDM).                    ECS027
02509       SUBTRACT HOLD-COUNT-A     FROM WSD-A-COUNT     (WSDM)       ECS027
02510           GIVING PSD-MA-COUNT          (PSDM).                    ECS027
02511       SUBTRACT HOLD-PREM-A      FROM WSD-A-PREM      (WSDM)       ECS027
02512           GIVING PSD-MA-PREM           (PSDM).                    ECS027
02513       SUBTRACT HOLD-EARNED-A    FROM WSD-A-EARN-PREM (WSDM)       ECS027
02514           GIVING PSD-MA-EARN-PREM      (PSDM).                    ECS027
02515       SUBTRACT HOLD-COMMIS-A    FROM WSD-A-COMMIS    (WSDM)       ECS027
02516           GIVING PSD-MA-COMMIS         (PSDM).                    ECS027
02517       SUBTRACT HOLD-CLM-INCUR-A FROM WSD-A-CLM-INCUR (WSDM)       ECS027
02518           GIVING PSD-MA-CLAIM-INCURRED (PSDM).                    ECS027
02519       SUBTRACT HOLD-CLM-PD-A    FROM WSD-A-CLM-PAID  (WSDM)       ECS027
02520           GIVING PSD-MA-CLAIMS-PAID    (PSDM).                    ECS027
02521       ADD PSD-MA-CLAIMS-PAID (PSDM)                               ECS027
02522                      TO PSD-MA-CLAIM-INCURRED (PSDM).             ECS027
02523       ADD PSD-ML-CLAIMS-PAID (PSDM)                               ECS027
02524                      TO PSD-ML-CLAIM-INCURRED (PSDM).             ECS027
02525                                                                   ECS027
02526   EJECT                                                           ECS027
02527       SET SCD12 TO +13.                                           ECS027
02528       MOVE SCD-R12-CCYY (SCD12) TO DATE-CCYY                      ECS027
02529                                                                   ECS027
02530       PERFORM 8900-EMPTY-LOOP                                     ECS027
02531           VARYING SCD12 FROM +12 BY -1                            ECS027
02532               UNTIL SCD-R12-CCYY (SCD12) LESS DATE-CCYY           ECS027
02533                   OR SCD12 = +1.                                  ECS027
02534                                                                   ECS027
02535       IF SCD12 = +1                                               ECS027
02536          AND SCD-R12-CCYY (SCD12) NOT LESS DATE-CCYY              ECS027
02537           SET WSDM TO +1                                          ECS027
02538        ELSE                                                       ECS027
02539           SET WSDM TO SCD12.                                      ECS027
02540                                                                   ECS027
02541       MOVE WSD-MONTH-ARRAY (WSDM) TO HOLD-AMOUNTS.                ECS027
02542       SET WSDM TO +13.                                            ECS027
02543       SET PSDM TO +6.                                             ECS027
02544                                                                   ECS027
02545       SUBTRACT HOLD-COUNT       FROM WSD-L-COUNT     (WSDM)       ECS027
02546         GIVING PSD-ML-COUNT          (PSDM).                      ECS027
02547       SUBTRACT HOLD-PREM        FROM WSD-L-PREM      (WSDM)       ECS027
02548         GIVING PSD-ML-PREM           (PSDM).                      ECS027
02549       SUBTRACT HOLD-EARNED      FROM WSD-L-EARN-PREM (WSDM)       ECS027
02550         GIVING PSD-ML-EARN-PREM      (PSDM).                      ECS027
02551       SUBTRACT HOLD-COMMIS      FROM WSD-L-COMMIS    (WSDM)       ECS027
02552         GIVING PSD-ML-COMMIS         (PSDM).                      ECS027
02553       SUBTRACT HOLD-CLM-INCUR   FROM WSD-L-CLM-INCUR (WSDM)       ECS027
02554         GIVING PSD-ML-CLAIM-INCURRED (PSDM).                      ECS027
02555       SUBTRACT HOLD-CLM-PD      FROM WSD-L-CLM-PAID  (WSDM)       ECS027
02556         GIVING PSD-ML-CLAIMS-PAID    (PSDM).                      ECS027
02557       SUBTRACT HOLD-COUNT-A     FROM WSD-A-COUNT     (WSDM)       ECS027
02558         GIVING PSD-MA-COUNT          (PSDM).                      ECS027
02559       SUBTRACT HOLD-PREM-A      FROM WSD-A-PREM      (WSDM)       ECS027
02560         GIVING PSD-MA-PREM           (PSDM).                      ECS027
02561       SUBTRACT HOLD-EARNED-A    FROM WSD-A-EARN-PREM (WSDM)       ECS027
02562         GIVING PSD-MA-EARN-PREM      (PSDM).                      ECS027
02563       SUBTRACT HOLD-COMMIS-A    FROM WSD-A-COMMIS    (WSDM)       ECS027
02564         GIVING PSD-MA-COMMIS         (PSDM).                      ECS027
02565       SUBTRACT HOLD-CLM-INCUR-A FROM WSD-A-CLM-INCUR (WSDM)       ECS027
02566         GIVING PSD-MA-CLAIM-INCURRED (PSDM).                      ECS027
02567       SUBTRACT HOLD-CLM-PD-A    FROM WSD-A-CLM-PAID  (WSDM)       ECS027
02568         GIVING PSD-MA-CLAIMS-PAID    (PSDM).                      ECS027
02569       ADD PSD-MA-CLAIMS-PAID (PSDM)                               ECS027
02570                      TO PSD-MA-CLAIM-INCURRED (PSDM).             ECS027
02571       ADD PSD-ML-CLAIMS-PAID (PSDM)                               ECS027
02572                      TO PSD-ML-CLAIM-INCURRED (PSDM).             ECS027
02573  EJECT                                                            ECS027
02574  7100-LIFE-NET-PREM-ACCUMS.                                       ECS027
02575      IF CR-LFPRM = ZEROS                                          ECS027
02576          GO TO 7101-EXIT.                                         ECS027
02577                                                                   ECS027
02578      SUBTRACT CR-LFRFND FROM CR-LFPRM GIVING AMOUNT-1.            ECS027
02579      MULTIPLY AMOUNT-1 BY CR-AGE      GIVING AMT-SUM.             ECS027
02580                                                                   ECS027
02581 *--ITD.                                                           ECS027
02582      MOVE 'ITD'                  TO THE-TERM-X.                   ECS027
02583      MOVE CR-AGE                 TO THE-AGE.                      ECS027
02584      PERFORM 8100-SET-PSDT-PSDA-INDICES THRU 8101-EXIT.           ECS027
02585                                                                   ECS027
02586      ADD AMOUNT-1 TO PSD-L-PREM (PSDT, PSDA).                     ECS027
02587      ADD AMT-SUM  TO PSD-L-WTD-PREM (PSDT).                       ECS027
02588      ADD CR-AGE   TO PSD-L-AGE-SUM-PREM (PSDT).                   ECS027
02589      ADD +1       TO PSD-L-AGE-COUNT-PREM (PSDT).                 ECS027
02590                                                                   ECS027
02591 *--YTD.                                                           ECS027
02592      IF CR-CCYY = RUN-CCYY                                           CL*11
02593          MOVE 'YTD'              TO THE-TERM-X                    ECS027
02594          MOVE CR-AGE             TO THE-AGE                       ECS027
02595          PERFORM 8100-SET-PSDT-PSDA-INDICES THRU 8101-EXIT        ECS027
02596          ADD AMOUNT-1 TO PSD-L-PREM (PSDT, PSDA)                  ECS027
02597          ADD AMT-SUM  TO PSD-L-WTD-PREM (PSDT)                    ECS027
02598          ADD CR-AGE   TO PSD-L-AGE-SUM-PREM (PSDT)                ECS027
02599          ADD +1       TO PSD-L-AGE-COUNT-PREM (PSDT).             ECS027
02600                                                                   ECS027
02601 *--ROLLING MONTH AND QUARTER                                      ECS027
02602      PERFORM 8900-EMPTY-LOOP                                      ECS027
02603          VARYING SCD12 FROM +2 BY +1                              ECS027
02604              UNTIL (CR-CCYY = SCD-R12-CCYY  (SCD12)                  CL*11
02605                  AND CR-MO = SCD-R12-MONTH (SCD12))               ECS027
02606                  OR SCD12 = +13.                                  ECS027
02607                                                                   ECS027
02608      IF CR-CCYY = SCD-R12-CCYY (SCD12)                               CL*11
02609         AND CR-MO = SCD-R12-MONTH (SCD12)                         ECS027
02610          MOVE CR-LF-TERM         TO THE-TERM                      ECS027
02611          MOVE CR-AGE             TO THE-AGE                       ECS027
02612          PERFORM 8100-SET-PSDT-PSDA-INDICES THRU 8101-EXIT        ECS027
02613          ADD AMOUNT-1 TO PSD-L-PREM (PSDT, PSDA)                  ECS027
02614          ADD AMT-SUM  TO PSD-L-WTD-PREM (PSDT)                    ECS027
02615          ADD CR-AGE   TO PSD-L-AGE-SUM-PREM (PSDT)                ECS027
02616          ADD +1       TO PSD-L-AGE-COUNT-PREM (PSDT)              ECS027
02617          SET PSDT     TO +5                                       ECS027
02618          ADD AMOUNT-1 TO PSD-L-PREM (PSDT, PSDA)                  ECS027
02619          ADD AMT-SUM  TO PSD-L-WTD-PREM (PSDT)                    ECS027
02620          ADD CR-AGE   TO PSD-L-AGE-SUM-PREM (PSDT)                ECS027
02621          ADD +1       TO PSD-L-AGE-COUNT-PREM (PSDT).             ECS027
02622                                                                   ECS027
02623  7101-EXIT.                                                       ECS027
02624      EXIT.                                                        ECS027
02625  EJECT                                                            ECS027
02626  7150-AH-NET-PREM-ACCUMS.                                         ECS027
02627      IF CR-AHPRM = ZEROS                                          ECS027
02628          GO TO 7151-EXIT.                                         ECS027
02629                                                                   ECS027
02630      SUBTRACT CR-AHRFND FROM CR-AHPRM GIVING AMOUNT-1.            ECS027
02631      MULTIPLY AMOUNT-1 BY CR-AGE      GIVING AMT-SUM.             ECS027
02632                                                                   ECS027
02633 *--ITD.                                                           ECS027
02634      MOVE 'ITD'                  TO THE-TERM-X.                   ECS027
02635      MOVE CR-AGE                 TO THE-AGE.                      ECS027
02636      PERFORM 8100-SET-PSDT-PSDA-INDICES THRU 8101-EXIT.           ECS027
02637      ADD AMOUNT-1 TO PSD-A-PREM (PSDT, PSDA).                     ECS027
02638      ADD AMT-SUM  TO PSD-A-WTD-PREM (PSDT).                       ECS027
02639      ADD +1       TO PSD-A-AGE-COUNT-PREM (PSDT).                 ECS027
02640      ADD CR-AGE   TO PSD-A-AGE-SUM-PREM (PSDT).                   ECS027
02641                                                                   ECS027
02642 *--YTD.                                                           ECS027
02643      IF CR-CCYY = RUN-CCYY                                           CL*11
02644          MOVE 'YTD'              TO THE-TERM-X                    ECS027
02645          MOVE CR-AGE             TO THE-AGE                       ECS027
02646          PERFORM 8100-SET-PSDT-PSDA-INDICES THRU 8101-EXIT        ECS027
02647          ADD AMOUNT-1 TO PSD-A-PREM (PSDT, PSDA)                  ECS027
02648          ADD AMT-SUM  TO PSD-A-WTD-PREM (PSDT)                    ECS027
02649          ADD +1       TO PSD-A-AGE-COUNT-PREM (PSDT)              ECS027
02650          ADD CR-AGE   TO PSD-A-AGE-SUM-PREM (PSDT).               ECS027
02651                                                                   ECS027
02652 *--ROLLING MONTH AND QUARTER                                      ECS027
02653      PERFORM 8900-EMPTY-LOOP                                      ECS027
02654          VARYING SCD12 FROM +2 BY +1                              ECS027
02655              UNTIL (CR-CCYY = SCD-R12-CCYY (SCD12)                   CL*11
02656                  AND CR-MO = SCD-R12-MONTH (SCD12))               ECS027
02657                  OR SCD12 = +13.                                  ECS027
02658                                                                   ECS027
02659      IF CR-CCYY = SCD-R12-CCYY (SCD12)                               CL*11
02660         AND CR-MO = SCD-R12-MONTH (SCD12)                         ECS027
02661          MOVE CR-AH-TERM  TO THE-TERM                             ECS027
02662          MOVE CR-AGE      TO THE-AGE                              ECS027
02663          PERFORM 8100-SET-PSDT-PSDA-INDICES THRU 8101-EXIT        ECS027
02664          ADD AMOUNT-1 TO PSD-A-PREM (PSDT, PSDA)                  ECS027
02665          ADD AMT-SUM  TO PSD-A-WTD-PREM (PSDT)                    ECS027
02666          ADD CR-AGE   TO PSD-A-AGE-SUM-PREM (PSDT)                ECS027
02667          ADD +1       TO PSD-A-AGE-COUNT-PREM (PSDT)              ECS027
02668          SET PSDT     TO +5                                       ECS027
02669          ADD AMOUNT-1 TO PSD-A-PREM (PSDT, PSDA)                  ECS027
02670          ADD AMT-SUM  TO PSD-A-WTD-PREM (PSDT)                    ECS027
02671          ADD CR-AGE   TO PSD-A-AGE-SUM-PREM (PSDT)                ECS027
02672          ADD +1       TO PSD-A-AGE-COUNT-PREM (PSDT).             ECS027
02673                                                                   ECS027
02674  7151-EXIT.                                                       ECS027
02675      EXIT.                                                        ECS027
02676  EJECT                                                            ECS027
02677  7200-LIFE-CLAIM-AMT-ACCUMS.                                      ECS027
02678      MULTIPLY CLM-AMT-PAID BY CLM-ORIG-AGE GIVING AMT-SUM.        ECS027
02679                                                                   ECS027
02680 *--ITD.                                                           ECS027
02681      MOVE 'ITD'                  TO THE-TERM-X.                   ECS027
02682      MOVE CLM-ORIG-AGE           TO THE-AGE.                      ECS027
02683      PERFORM 8100-SET-PSDT-PSDA-INDICES THRU 8101-EXIT.           ECS027
02684      ADD CLM-AMT-PAID TO PSD-L-CLAIMS-PAID (PSDT, PSDA).          ECS027
02685      ADD AMT-SUM      TO PSD-L-WTD-CLAIMS (PSDT).                 ECS027
02686      ADD CLM-ORIG-AGE TO PSD-L-AGE-SUM-CLMS (PSDT).               ECS027
02687      ADD +1           TO PSD-L-AGE-COUNT-CLMS (PSDT).             ECS027
02688                                                                   ECS027
02689      IF DE-PAY-CCYY = RUN-CCYY                                       CL*13
02690          MOVE 'YTD'              TO THE-TERM-X                    ECS027
02691          MOVE CLM-ORIG-AGE       TO THE-AGE                       ECS027
02692          PERFORM 8100-SET-PSDT-PSDA-INDICES THRU 8101-EXIT        ECS027
02693          ADD CLM-AMT-PAID TO PSD-L-CLAIMS-PAID (PSDT, PSDA)       ECS027
02694          ADD AMT-SUM      TO PSD-L-WTD-CLAIMS (PSDT)              ECS027
02695          ADD CLM-ORIG-AGE TO PSD-L-AGE-SUM-CLMS (PSDT)            ECS027
02696          ADD +1           TO PSD-L-AGE-COUNT-CLMS (PSDT).         ECS027
02697                                                                   ECS027
02698 *--ROLLING MONTH AND QUARTER                                      ECS027
02699      PERFORM 8900-EMPTY-LOOP                                      ECS027
02700          VARYING SCD12 FROM +2 BY +1                              ECS027
02701              UNTIL (DE-PAY-CCYY = SCD-R12-CCYY (SCD12)               CL*13
02702                  AND DE-PAY-MO = SCD-R12-MONTH (SCD12))              CL*13
02703                  OR SCD12 = +13.                                  ECS027
02704                                                                   ECS027
02705      IF DE-PAY-CCYY = SCD-R12-CCYY  (SCD12)  AND                     CL*13
02706         DE-PAY-MO = SCD-R12-MONTH (SCD12)                            CL*13
02707          MOVE CLM-ORIG-TERM      TO THE-TERM                      ECS027
02708          MOVE CLM-ORIG-AGE       TO THE-AGE                       ECS027
02709          PERFORM 8100-SET-PSDT-PSDA-INDICES THRU 8101-EXIT        ECS027
02710          ADD CLM-AMT-PAID TO PSD-L-CLAIMS-PAID (PSDT, PSDA)       ECS027
02711          ADD CLM-ORIG-AGE TO PSD-L-AGE-SUM-CLMS (PSDT)            ECS027
02712          ADD AMT-SUM      TO PSD-L-WTD-CLAIMS (PSDT)              ECS027
02713          ADD +1           TO PSD-L-AGE-COUNT-CLMS (PSDT)          ECS027
02714          SET PSDT         TO +5                                   ECS027
02715          ADD CLM-AMT-PAID TO PSD-L-CLAIMS-PAID (PSDT, PSDA)       ECS027
02716          ADD CLM-ORIG-AGE TO PSD-L-AGE-SUM-CLMS (PSDT)            ECS027
02717          ADD AMT-SUM      TO PSD-L-WTD-CLAIMS (PSDT)              ECS027
02718          ADD +1           TO PSD-L-AGE-COUNT-CLMS (PSDT).         ECS027
02719                                                                   ECS027
02720  7201-EXIT.                                                       ECS027
02721      EXIT.                                                        ECS027
02722  EJECT                                                            ECS027
02723  7250-AH-CLAIM-AMT-ACCUMS.                                        ECS027
02724      MULTIPLY CLM-AMT-PAID BY CLM-ORIG-AGE GIVING AMT-SUM.        ECS027
02725 *--ITD.                                                           ECS027
02726      MOVE 'ITD'                  TO THE-TERM-X.                   ECS027
02727      MOVE CLM-ORIG-AGE           TO THE-AGE.                      ECS027
02728      PERFORM 8100-SET-PSDT-PSDA-INDICES THRU 8101-EXIT.           ECS027
02729      ADD CLM-AMT-PAID TO PSD-A-CLAIMS-PAID (PSDT, PSDA).          ECS027
02730      ADD AMT-SUM      TO PSD-A-WTD-CLAIMS (PSDT).                 ECS027
02731      ADD CLM-ORIG-AGE TO PSD-A-AGE-SUM-CLMS (PSDT).               ECS027
02732      ADD +1           TO PSD-A-AGE-COUNT-CLMS (PSDT).             ECS027
02733                                                                   ECS027
02734 *--YTD.                                                           ECS027
02735      IF DE-PAY-CCYY = RUN-CCYY                                       CL*13
02736          MOVE 'YTD'              TO THE-TERM-X                    ECS027
02737          MOVE CLM-ORIG-AGE       TO THE-AGE                       ECS027
02738          PERFORM 8100-SET-PSDT-PSDA-INDICES THRU 8101-EXIT        ECS027
02739          ADD CLM-AMT-PAID TO PSD-A-CLAIMS-PAID (PSDT, PSDA)       ECS027
02740          ADD AMT-SUM      TO PSD-A-WTD-CLAIMS (PSDT)              ECS027
02741          ADD CLM-ORIG-AGE TO PSD-A-AGE-SUM-CLMS (PSDT)            ECS027
02742          ADD +1           TO PSD-A-AGE-COUNT-CLMS (PSDT).         ECS027
02743                                                                   ECS027
02744 *--ROLLING MONTH AND QUARTER                                      ECS027
02745      PERFORM 8900-EMPTY-LOOP                                      ECS027
02746          VARYING SCD12 FROM +2 BY +1                              ECS027
02747              UNTIL (DE-PAY-CCYY = SCD-R12-CCYY (SCD12)               CL*15
02748                  AND DE-PAY-MO = SCD-R12-MONTH (SCD12))              CL*13
02749                  OR SCD12 = +13.                                  ECS027
02750                                                                   ECS027
02751      IF DE-PAY-CCYY = SCD-R12-CCYY  (SCD12) AND                      CL*13
02752         DE-PAY-MO = SCD-R12-MONTH (SCD12)                            CL*13
02753          MOVE CLM-ORIG-TERM      TO THE-TERM                      ECS027
02754          MOVE CLM-ORIG-AGE       TO THE-AGE                       ECS027
02755          PERFORM 8100-SET-PSDT-PSDA-INDICES THRU 8101-EXIT        ECS027
02756          ADD CLM-AMT-PAID TO PSD-A-CLAIMS-PAID (PSDT, PSDA)       ECS027
02757          ADD CLM-ORIG-AGE TO PSD-A-AGE-SUM-CLMS (PSDT)            ECS027
02758          ADD AMT-SUM      TO PSD-A-WTD-CLAIMS (PSDT)              ECS027
02759          ADD +1           TO PSD-A-AGE-COUNT-CLMS (PSDT)          ECS027
02760          SET PSDT         TO +5                                   ECS027
02761          ADD CLM-AMT-PAID TO PSD-A-CLAIMS-PAID (PSDT, PSDA)       ECS027
02762          ADD CLM-ORIG-AGE TO PSD-A-AGE-SUM-CLMS (PSDT)            ECS027
02763          ADD AMT-SUM      TO PSD-A-WTD-CLAIMS (PSDT)              ECS027
02764          ADD +1           TO PSD-A-AGE-COUNT-CLMS (PSDT).         ECS027
02765                                                                   ECS027
02766  7251-EXIT.                                                       ECS027
02767      EXIT.                                                        ECS027
02768  EJECT                                                            ECS027
02769  8100-SET-PSDT-PSDA-INDICES.                                      ECS027
02770       IF THE-TERM-X = 'ITD'                                       ECS027
02771           SET PSDT TO +7                                          ECS027
02772       ELSE                                                        ECS027
02773           IF THE-TERM-X = 'YTD'                                   ECS027
02774               SET PSDT TO +6                                      ECS027
02775           ELSE                                                    ECS027
02776               IF THE-TERM NOT GREATER SCD-TERM-1-HIGH             ECS027
02777                   SET PSDT TO +1                                  ECS027
02778               ELSE                                                ECS027
02779                   IF  THE-TERM NOT LESS    SCD-TERM-2-LOW         ECS027
02780                   AND THE-TERM NOT GREATER SCD-TERM-2-HIGH        ECS027
02781                       SET PSDT TO +2                              ECS027
02782                   ELSE                                            ECS027
02783                       IF  THE-TERM NOT LESS    SCD-TERM-3-LOW     ECS027
02784                       AND THE-TERM NOT GREATER SCD-TERM-3-HIGH    ECS027
02785                        SET PSDT TO +3                             ECS027
02786                    ELSE                                           ECS027
02787                        SET PSDT TO +4.                            ECS027
02788                                                                   ECS027
02789      IF THE-AGE NOT GREATER SCD-AGE-1-HIGH                        ECS027
02790          SET PSDA TO +1.                                          ECS027
02791                                                                   ECS027
02792      IF THE-AGE NOT LESS SCD-AGE-2-LOW                            ECS027
02793         AND THE-AGE NOT GREATER SCD-AGE-2-HIGH                    ECS027
02794          SET PSDA TO +2.                                          ECS027
02795                                                                   ECS027
02796      IF THE-AGE NOT LESS SCD-AGE-3-LOW                            ECS027
02797         AND THE-AGE NOT GREATER SCD-AGE-3-HIGH                    ECS027
02798          SET PSDA TO +3.                                          ECS027
02799                                                                   ECS027
02800      IF THE-AGE NOT LESS SCD-AGE-4-LOW                            ECS027
02801         AND THE-AGE NOT GREATER SCD-AGE-4-HIGH                    ECS027
02802          SET PSDA TO +4.                                          ECS027
02803                                                                   ECS027
02804      IF THE-AGE NOT LESS SCD-AGE-5-LOW                            ECS027
02805          SET PSDA TO +5.                                          ECS027
02806                                                                   ECS027
02807  8101-EXIT.                                                       ECS027
02808      EXIT.                                                        ECS027
02809                                                                   ECS027
02810  8200-GET-CLAIM-INCURRED-AMT.                                     ECS027
02811      ADD EP-CLM-DU                                                ECS027
02812          EP-CLM-PV                                                ECS027
02813          EP-CLM-IBNR                                              ECS027
02814          EP-LOSS-RESV                                             ECS027
02815          EP-CLAIM-ADJ                                             ECS027
02816              GIVING AMT-SUM.                                      ECS027
02817                                                                   ECS027
02818  8201-EXIT.                                                       ECS027
02819      EXIT.                                                        ECS027
02820  EJECT                                                            ECS027
02821  8300-SET-WSDM-INDEX.                                             ECS027
02822      PERFORM 8900-EMPTY-LOOP                                      ECS027
02823          VARYING SCD12 FROM +1 BY +1                              ECS027
02824              UNTIL (EP-RUN-MO = SCD-R12-MONTH (SCD12)                CL*16
02825                  AND EP-RUN-CCYY = SCD-R12-CCYY (SCD12))             CL*16
02826                  OR SCD12 = +13.                                  ECS027
02827                                                                   ECS027
02828      IF EP-RUN-CCYY = SCD-R12-CCYY (SCD12)  AND                      CL*16
02829         EP-RUN-MO = SCD-R12-MONTH (SCD12)                            CL*16
02830          SET WSDM TO SCD12                                        ECS027
02831          MOVE 'Y'              TO INDEX-VALID                     ECS027
02832          GO TO 8301-EXIT.                                         ECS027
02833                                                                   ECS027
02834      IF EP-RUN-CCYY GREATER SCD-R12-CCYY (SCD12)                     CL*16
02835          MOVE 'N'              TO INDEX-VALID                     ECS027
02836          GO TO 8301-EXIT                                          ECS027
02837      ELSE                                                         ECS027
02838          IF EP-RUN-CCYY = SCD-R12-CCYY (SCD12)                       CL*16
02839              IF EP-RUN-MO GREATER SCD-R12-MONTH (SCD12)              CL*16
02840                  MOVE 'N'       TO INDEX-VALID                    ECS027
02841                  GO TO 8301-EXIT.                                 ECS027
02842                                                                   ECS027
02843      IF EP-PURGE = 'P'                                            ECS027
02844            MOVE 'Y'       TO INDEX-VALID                          ECS027
02845            SET WSDM SCD12 TO +1                                   ECS027
02846        ELSE                                                       ECS027
02847            MOVE 'N'       TO INDEX-VALID.                         ECS027
02848                                                                   ECS027
02849  8301-EXIT.                                                       ECS027
02850      EXIT.                                                        ECS027
02851                                                                   ECS027
02852  COPY ELCDCS.                                                     ECS027
02853                                                                   ECS027
02854  EJECT                                                            ECS027
02855  8900-EMPTY-LOOP.                                                 ECS027
02856      EXIT.                                                        ECS027
02857                                                                   ECS027
02858  8999-DUMMY-PARAGRAPH.                                            ECS027
02859      EXIT.                                                        ECS027
02860                                                                   ECS027
02861  9000-ERROR-REPORT SECTION.                                       ECS027
02862      PERFORM 9100-LOAD-ERROR-DETAIL THRU 9101-EXIT.               ECS027
02863                                                                   ECS027
02864      OPEN OUTPUT PRINTR.                                          ECS027
02865                                                                   ECS027
02866      MOVE PRT-ERR-HDR-1          TO PRT-DATA.                     ECS027
02867      PERFORM 9200-PRINT.                                          ECS027
02868      MOVE PRT-ERR-HDR-2          TO PRT-DATA.                     ECS027
02869      PERFORM 9200-PRINT.                                          ECS027
02870      MOVE PRT-ERR-HDR-3          TO PRT-DATA.                     ECS027
02871      PERFORM 9200-PRINT.                                          ECS027
02872      MOVE PRT-ERR-DTL-1A         TO PRT-DATA.                     ECS027
02873      PERFORM 9200-PRINT.                                          ECS027
02874                                                                   ECS027
02875      IF ED1-STD-ERR-MSG NOT = SPACES                              ECS027
02876          MOVE PRT-ERR-DTL-1B     TO PRT-DATA                      ECS027
02877          PERFORM 9200-PRINT.                                      ECS027
02878                                                                   ECS027
02879      MOVE PRT-ERR-HDR-4          TO PRT-DATA.                     ECS027
02880      PERFORM 9200-PRINT.                                          ECS027
02881      MOVE PRT-ERR-DTL-2          TO PRT-DATA.                     ECS027
02882      PERFORM 9200-PRINT.                                          ECS027
02883      MOVE PRT-ERR-DTL-4          TO PRT-DATA.                     ECS027
02884      PERFORM 9200-PRINT.                                          ECS027
02885      MOVE PRT-ERR-DTL-5          TO PRT-DATA.                     ECS027
02886      PERFORM 9200-PRINT.                                          ECS027
02887      MOVE PRT-ERR-DTL-6          TO PRT-DATA.                     ECS027
02888      PERFORM 9200-PRINT.                                          ECS027
02889      MOVE PRT-ERR-HDR-5          TO PRT-DATA.                     ECS027
02890      PERFORM 9200-PRINT.                                          ECS027
02891      MOVE PRT-ERR-DTL-7          TO PRT-DATA.                     ECS027
02892      PERFORM 9200-PRINT.                                          ECS027
02893      MOVE PRT-ERR-HDR-6          TO PRT-DATA.                     ECS027
02894      PERFORM 9200-PRINT.                                          ECS027
02895      MOVE PRT-ERR-DTL-8          TO PRT-DATA.                     ECS027
02896      PERFORM 9200-PRINT.                                          ECS027
02897      MOVE PRT-ERR-DTL-9          TO PRT-DATA.                     ECS027
02898      PERFORM 9200-PRINT.                                          ECS027
02899      MOVE PRT-ERR-DTL-10         TO PRT-DATA.                     ECS027
02900      PERFORM 9200-PRINT.                                          ECS027
02901      MOVE PRT-ERR-DTL-11         TO PRT-DATA.                     ECS027
02902      PERFORM 9200-PRINT.                                          ECS027
02903      MOVE PRT-ERR-DTL-12         TO PRT-DATA.                     ECS027
02904      PERFORM 9200-PRINT.                                          ECS027
02905      MOVE PRT-ERR-DTL-13         TO PRT-DATA.                     ECS027
02906      PERFORM 9200-PRINT.                                          ECS027
02907      CLOSE PRINTR.                                                ECS027
02908                                                                   ECS027
02909  9001-EXIT.                                                       ECS027
02910      EXIT.                                                        ECS027
02911  EJECT                                                            ECS027
02912  9100-LOAD-ERROR-DETAIL.                                          ECS027
02913      MOVE COMPANY-NAME           TO EH2-CO-NAME.                  ECS027
02914      MOVE RUN-MO                 TO EH2-RUN-MO.                   ECS027
02915      MOVE RUN-DA                 TO EH2-RUN-DAY.                  ECS027
02916      MOVE RUN-YR                 TO EH2-RUN-YR.                   ECS027
02917                                                                   ECS027
02918      SET PSCSR TO +1.                                             ECS027
02919      MOVE PSC-STD-REQ (PSCSR)     TO ED1-STD-ACCT-INDIC.          ECS027
02920      SET PSCSR UP BY +1.                                          ECS027
02921      MOVE PSC-STD-REQ (PSCSR)     TO ED1-STD-GA-INDIC.            ECS027
02922      SET PSCSR UP BY +1.                                          ECS027
02923      MOVE PSC-STD-REQ (PSCSR)     TO ED1-STD-ACCT-GA-INDIC.       ECS027
02924      SET PSCSR UP BY +1.                                          ECS027
02925      MOVE PSC-STD-REQ (PSCSR)     TO ED1-STD-STATE-INDIC.         ECS027
02926      SET PSCSR UP BY +1.                                          ECS027
02927      MOVE PSC-STD-REQ (PSCSR)     TO ED1-STD-CARR-INDIC.          ECS027
02928      SET PSCSR UP BY +1.                                          ECS027
02929      MOVE PSC-STD-REQ (PSCSR)     TO ED1-STD-GRP-INDIC.           ECS027
02930      SET PSCSR UP BY +1.                                          ECS027
02931      MOVE PSC-STD-REQ (PSCSR)     TO ED1-STD-RETRO-GRP-INDIC.     ECS027
02932      SET PSCSR UP BY +1.                                          ECS027
02933      MOVE PSC-STD-REQ (PSCSR)     TO ED1-STD-REGION-INDIC.        ECS027
02934      SET PSCSR UP BY +1.                                          ECS027
02935      MOVE PSC-STD-REQ (PSCSR)     TO ED1-STD-BUS-TY-INDIC.        ECS027
02936      SET PSCSR UP BY +1.                                          ECS027
02937      MOVE PSC-STD-REQ (PSCSR)     TO ED1-STD-RPT-CD-1-INDIC.      ECS027
02938      SET PSCSR UP BY +1.                                          ECS027
02939      MOVE PSC-STD-REQ (PSCSR)     TO ED1-STD-RPT-CD-2-INDIC.      ECS027
02940                                                                   ECS027
02941      MOVE PSC-ACCT-DT-RANGE-INDIC TO ED2-ACCT-DT-RANGE-INDIC.     ECS027
02942                                                                   ECS027
02943      MOVE PSC-TERM-1-HIGH         TO ED5-TG-1-HIGH.               ECS027
02944      MOVE PSC-TERM-2-LOW          TO ED5-TG-2-LOW.                ECS027
02945      MOVE PSC-TERM-2-HIGH         TO ED5-TG-2-HIGH.               ECS027
02946      MOVE PSC-TERM-3-LOW          TO ED5-TG-3-LOW.                ECS027
02947      MOVE PSC-TERM-3-HIGH         TO ED5-TG-3-HIGH.               ECS027
02948      MOVE PSC-TERM-4-LOW          TO ED5-TG-4-LOW.                ECS027
02949                                                                   ECS027
02950      MOVE PSC-AGE-1-HIGH          TO ED6-AG-1-HIGH.               ECS027
02951      MOVE PSC-AGE-2-LOW           TO ED6-AG-2-LOW.                ECS027
02952      MOVE PSC-AGE-2-HIGH          TO ED6-AG-2-HIGH.               ECS027
02953      MOVE PSC-AGE-3-LOW           TO ED6-AG-3-LOW.                ECS027
02954      MOVE PSC-AGE-3-HIGH          TO ED6-AG-3-HIGH.               ECS027
02955      MOVE PSC-AGE-4-LOW           TO ED6-AG-4-LOW.                ECS027
02956      MOVE PSC-AGE-4-HIGH          TO ED6-AG-4-HIGH.               ECS027
02957      MOVE PSC-AGE-5-LOW           TO ED6-AG-5-LOW.                ECS027
02958                                                                   ECS027
02959      SET PSCDH TO +1.                                             ECS027
02960      MOVE PSC-D-HEIRARCHY-X (PSCDH) TO ED7-CARR-HEIRARCHY.        ECS027
02961      SET PSCDH UP BY +1.                                          ECS027
02962      MOVE PSC-D-HEIRARCHY-X (PSCDH) TO ED7-GROUP-HEIRARCHY.       ECS027
02963  EJECT                                                            ECS027
02964      SET PSCDH UP BY +1.                                          ECS027
02965      MOVE PSC-D-HEIRARCHY-X (PSCDH) TO ED7-STATE-HEIRARCHY.       ECS027
02966      SET PSCDH UP BY +1.                                          ECS027
02967      MOVE PSC-D-HEIRARCHY-X (PSCDH) TO ED7-ACCT-HEIRARCHY.        ECS027
02968      SET PSCDH UP BY +1.                                          ECS027
02969      MOVE PSC-D-HEIRARCHY-X (PSCDH) TO ED7-REGION-HEIRARCHY.      ECS027
02970      SET PSCDH UP BY +1.                                          ECS027
02971      MOVE PSC-D-HEIRARCHY-X (PSCDH) TO ED7-BUS-TY-HEIRARCHY.      ECS027
02972                                                                   ECS027
02973      MOVE PSC-L-STATE               TO ED8-STATE-LIMITER.         ECS027
02974      MOVE PSC-BUSINESS-TYPE         TO ED9-BUS-TY-LIMITER.        ECS027
02975                                                                      CL**6
02976      MOVE WS-CID-LOW-MDY            TO ED10-CERT-F-LIMITER.          CL**6
02977      MOVE WS-CID-HIGH-MDY           TO ED11-CERT-T-LIMITER.          CL**6
02978                                                                   ECS027
02979  9101-EXIT.                                                       ECS027
02980      EXIT.                                                        ECS027
02981                                                                   ECS027
02982  9200-PRINT.                                                      ECS027
02983                                                                   ECS027
02984      IF PRT-CNTL IS EQUAL TO ' '                                  ECS027
02985        WRITE PRT AFTER ADVANCING 1 LINE                           ECS027
02986      ELSE                                                         ECS027
02987        IF PRT-CNTL IS EQUAL TO '0'                                ECS027
02988          WRITE PRT AFTER ADVANCING 2 LINES                        ECS027
02989        ELSE                                                       ECS027
02990          IF PRT-CNTL IS EQUAL TO '-'                              ECS027
02991            WRITE PRT AFTER ADVANCING 3 LINES                      ECS027
02992          ELSE                                                     ECS027
02993            WRITE PRT AFTER ADVANCING PAGE.                        ECS027
02994                                                                   ECS027
02995  9200-PRINT-EXIT.                                                 ECS027
02996  EJECT                                                            ECS027
02997  9999-ABEND-DISPLAYS             SECTION.                         ECS027
02998      DISPLAY '***** ****- - - - - -**- - - - - - - - - - - -****'.ECS027
02999      DISPLAY '***** ****- ECS-027 -**-ABNORMAL  TERMINATION-****'.ECS027
03000      DISPLAY '***** ****- - - - - -**- - - - - - - - - - - -****'.ECS027
03001      DISPLAY '*****'.                                             ECS027
03002      DISPLAY '***** RECORD COUNTS AT ABEND ARE -'.                ECS027
03003      MOVE DETAIL-EXTRACT-COUNT TO COUNT-DISPLAY.                  ECS027
03004      DISPLAY '***'.                                               ECS027
03005      DISPLAY '*** NUMBER OF PRODUCTION DETAIL EXTRACTS = '        ECS027
03006          COUNT-DISPLAY.                                           ECS027
03007      MOVE CERT-READ-COUNT      TO COUNT-DISPLAY.                  ECS027
03008      DISPLAY '***          NUMBER OF CERTIFICATES READ = '        ECS027
03009          COUNT-DISPLAY.                                           ECS027
03010      MOVE CERT-USE-COUNT       TO COUNT-DISPLAY.                  ECS027
03011      DISPLAY '***          NUMBER OF CERTIFICATES USED = '        ECS027
03012          COUNT-DISPLAY.                                           ECS027
03013      MOVE CLAIM-READ-COUNT     TO COUNT-DISPLAY.                  ECS027
03014      DISPLAY '***        NUMBER OF CLAIM PAYMENTS READ = '        ECS027
03015          COUNT-DISPLAY.                                           ECS027
03016      MOVE CLAIM-USE-COUNT      TO COUNT-DISPLAY.                  ECS027
03017      DISPLAY '***        NUMBER OF CLAIM PAYMENTS USED = '        ECS027
03018          COUNT-DISPLAY.                                           ECS027
03019      MOVE EPEC-READ-COUNT      TO COUNT-DISPLAY.                  ECS027
03020      DISPLAY '***          NUMBER OF EPEC RECORDS READ = '        ECS027
03021          COUNT-DISPLAY.                                           ECS027
03022      MOVE EPEC-USE-COUNT       TO COUNT-DISPLAY.                  ECS027
03023      DISPLAY '***          NUMBER OF EPEC RECORDS USED = '        ECS027
03024          COUNT-DISPLAY.                                           ECS027
03025      MOVE ACCT-READ-COUNT      TO COUNT-DISPLAY.                  ECS027
03026      DISPLAY '***              NUMBER OF ACCOUNTS READ = '        ECS027
03027          COUNT-DISPLAY.                                           ECS027
03028      DISPLAY '***** ****- - - - - -**- - - - - - - - - - - -****'.ECS027
03029                                                                   ECS027
03030  ABEND-PGM SECTION.                                               ECS027
03031                              COPY ELCABEND.                       ECS027
