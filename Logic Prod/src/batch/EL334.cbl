00001  IDENTIFICATION DIVISION.                                         04/18/98
00002                                                                   EL334
00003  PROGRAM-ID.                 EL334 .                                 LV006
00004 *              PROGRAM CONVERTED BY                               EL334
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL334
00006 *              CONVERSION DATE 04/18/95 07:47:52.                 EL334
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            EL334
00008 *                            VMOD=2.007.                          EL334
00009 *AUTHOR.     LOGIC INC.                                           EL334
00010 *            DALLAS, TEXAS.                                       EL334
00011                                                                   EL334
00011                                                                   EL334
00012 *DATE-COMPILED.                                                   EL334
00013                                                                   EL334
00014 *SECURITY.   *****************************************************EL334
00015 *            *                                                   *EL334
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *EL334
00017 *            *                                                   *EL334
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL334
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL334
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *EL334
00021 *            *                                                   *EL334
00022 *            *****************************************************EL334
00023                                                                   EL334
00024 *REMARKS.                                                         EL334
00025 *         THIS PROGRAM IS USED TO CONVERT THE CLAS CLAIMS HISTORY EL334
00026 *       MASTER TO THE APPROPRIATE CLAS-IC ON-LINE FILES.  THIS    EL334
00027 *       PROGRAM ALSO BUILDS A SKELETON CERTIFICATE RECORD FOR     EL334
00028 *       EVERY CLAIM ON FILE.                                      EL334
00029                                                                   EL334
00030  ENVIRONMENT DIVISION.                                            EL334
00031  CONFIGURATION SECTION.                                           EL334
00032  SPECIAL-NAMES.                                                   EL334
00033      C02 IS LCP-CH2                                               EL334
00034      C03 IS LCP-CH3                                               EL334
00035      C04 IS LCP-CH4                                               EL334
00036      C05 IS LCP-CH5                                               EL334
00037      C06 IS LCP-CH6                                               EL334
00038      C07 IS LCP-CH7                                               EL334
00039      C08 IS LCP-CH8                                               EL334
00040      C09 IS LCP-CH9                                               EL334
00041      C10 IS LCP-CH10                                              EL334
00042      C11 IS LCP-CH11                                              EL334
00043      C12 IS LCP-CH12                                              EL334
00044      S01 IS LCP-P01                                               EL334
00045      S02 IS LCP-P02.                                              EL334
00046                                                                   EL334
00047  INPUT-OUTPUT SECTION.                                            EL334
00048                                                                   EL334
00049  FILE-CONTROL.                                                    EL334
00050                                                                   EL334
00051      SELECT PRNTR                                                 EL334
00052          ASSIGN TO SYS008-UR-1403-S-SYS008.                       EL334
00053                                                                   EL334
00054      SELECT DISK-DATE                                             EL334
00055          ASSIGN TO SYS019-UT-FBA1-S-SYS019.                       EL334
00056                                                                   EL334
00057      SELECT FICH                                                  EL334
00058          ASSIGN TO SYS020-UT-2400-S-SYS020.                       EL334
00059                                                                   EL334
00060      SELECT CLAIM-EXTRACT-IN                                      EL334
00061          ASSIGN TO SYS010-UT-2400-S-SYS010.                       EL334
00062                                                                   EL334
00063      SELECT CLAS-CERT-IN                                          EL334
00064          ASSIGN TO SYS014-UT-2400-S-SYS014.                       EL334
00065                                                                   EL334
00066      SELECT ELMSTR                                                EL334
00067          ASSIGN TO SYS011-UT-FBA1-S-SYS011.                       EL334
00068                                                                   EL334
00069      SELECT CLAIM-OUTPUT-FILE                                     EL334
00070          ASSIGN TO SYS015-UT-FBA1-S-SYS015.                       EL334
00071                                                                   EL334
00072      SELECT ELTRLR                                                EL334
00073          ASSIGN TO SYS012-UT-FBA1-S-SYS012.                       EL334
00074                                                                   EL334
00075      SELECT TRAILER-OUTPUT-FILE                                   EL334
00076          ASSIGN TO SYS016-UT-FBA1-S-SYS016.                       EL334
00077                                                                   EL334
00078      SELECT CERT-OUTPUT-FILE                                      EL334
00079          ASSIGN TO SYS017-UT-FBA1-S-SYS017.                       EL334
00080                                                                   EL334
00081      SELECT ELREPT                                                EL334
00082          ASSIGN TO SYS018-FBA1-ELREPT                             EL334
00083          ORGANIZATION IS INDEXED                                  EL334
00084          ACCESS IS DYNAMIC                                        EL334
00085          RECORD KEY IS RF-CONTROL-PRIMARY                         EL334
00086          FILE STATUS IS DTE-VSAM-FLAGS.                           EL334
00087                                                                   EL334
00088      SELECT SORT-WORK-FILE                                        EL334
00089          ASSIGN TO SYS001-UT-FBA1-S-SORTWK1.                      EL334
00090                                                                   EL334
00091      SELECT CLAIM-SORT-WORK-FILE                                  EL334
00092          ASSIGN TO SYS001-UT-FBA1-S-SORTWK1.                      EL334
00093                                                                   EL334
00094      SELECT TRAILER-SORT-WORK-FILE                                EL334
00095          ASSIGN TO SYS001-UT-FBA1-S-SORTWK1.                      EL334
00096                                                                   EL334
00097      EJECT                                                        EL334
00098  DATA DIVISION.                                                   EL334
00099                                                                   EL334
00100  FILE SECTION.                                                    EL334
00101                                                                   EL334
00102  FD  PRNTR                                                        EL334
00103      RECORDING MODE F.                                            EL334
00104  01  PRT.                                                         EL334
00105      12 P-CTL              PIC X.                                 EL334
00106      12 P-DATA             PIC X(132).                            EL334
00107                                                                   EL334
00108  FD  ELMSTR                                                       EL334
00109      BLOCK CONTAINS 0 RECORDS
00110      RECORDING MODE F.                                            EL334
00111                                                                   EL334
00112                                  COPY ELCMSTR.                    EL334
00113                                                                   EL334
00114  FD  ELTRLR                                                       EL334
00115      BLOCK CONTAINS 0 RECORDS
00116      RECORDING MODE F.                                            EL334
00117                                                                   EL334
00118                                  COPY ELCTRLR.                    EL334
00119                                                                   EL334
00120  FD  TRAILER-OUTPUT-FILE                                          EL334
00121      RECORDING MODE IS F                                          EL334
00122      RECORDING MODE F.                                            EL334
00123                                                                   EL334
00124  01  TRAILER-OUTPUT-RECORD       PIC X(200).                      EL334
00125                                                                   EL334
00126  FD  CLAS-CERT-IN                COPY ECSCRIFD.                   EL334
00127                                                                   EL334
00128                                  COPY ECSCRT01 REPLACING          EL334
00129      CERTIFICATE-MASTER          BY  CLAS-CERT-MASTER.            EL334
00130  FD  DISK-DATE                   COPY ELCDTEFD.                   EL334
00131                                                                   EL334
00132  FD  FICH                        COPY ELCFCHFD.                   EL334
00133                                                                   EL334
00134  FD  ELREPT                      COPY ELCRPTFD.                   EL334
00135                                                                   EL334
00136                                  COPY ELCREPT.                    EL334
00137                                                                   EL334
00138  FD  CLAIM-EXTRACT-IN                                             EL334
00139      BLOCK CONTAINS 0 RECORDS
00140      RECORDING MODE F.                                            EL334
00141                                                                   EL334
00142  01  CLAIM-EXTRACT-RECORD        PIC X(510).                      EL334
00143                                                                   EL334
00144  FD  CLAIM-OUTPUT-FILE                                            EL334
00145      BLOCK CONTAINS 0 RECORDS
00146      RECORDING MODE F.                                            EL334
00147                                                                   EL334
00148  01  CLAIM-OUTPUT-RECORD         PIC X(350).                      EL334
00149                                                                   EL334
00150  FD  CERT-OUTPUT-FILE                                             EL334
00151      BLOCK CONTAINS 0 RECORDS
00152      RECORDING MODE F.                                            EL334
00153                                                                   EL334
00154                                  COPY ELCCERT.                    EL334
00155                                                                   EL334
00156      EJECT                                                        EL334
00157  SD  SORT-WORK-FILE.                                              EL334
00158                                                                   EL334
00159                                  COPY ECSEXT01.                   EL334
00160                                                                   EL334
00161      EJECT                                                        EL334
00162  SD  CLAIM-SORT-WORK-FILE.                                        EL334
00163                                                                   EL334
00164  01  SORT-WORK-RECORD.                                            EL334
00165      05  FILLER                      PIC XX.                      EL334
00166      05  SWR-CLAIM-KEY               PIC X(20).                   EL334
00167      05  FILLER                      PIC X(328).                  EL334
00168                                                                   EL334
00169  SD  TRAILER-SORT-WORK-FILE.                                      EL334
00170                                                                   EL334
00171  01  TRAILER-SORT-WORK-RECORD.                                    EL334
00172      05  FILLER                  PIC XX.                          EL334
00173      05  SWR-TRAILER-KEY         PIC X(22).                       EL334
00174      05  FILLER                  PIC X(176).                      EL334
00175                                                                   EL334
00176      EJECT                                                        EL334
00177  WORKING-STORAGE SECTION.                                         EL334
00178  77  LCP-ASA                       PIC X.                         EL334
00179  77  FILLER  PIC X(32) VALUE '********************************'.  EL334
00180  77  FILLER  PIC X(32) VALUE '*    EL334 WORKING-STORAGE     *'.  EL334
00181  77  FILLER  PIC X(32) VALUE '*********VMOD=2.007*************'.  EL334
00182                                                                   EL334
00183  77  PGM-SUB                     PIC S9(4)  COMP     VALUE +334.  EL334
00184  77  ABEND-CODE                  PIC X(4).                        EL334
00185  77  ABEND-OPTION                PIC X.                           EL334
00186  77  OLC-REPORT-NAME             PIC X(5)           VALUE 'EL334'.EL334
00187                                                                   EL334
00188  01  MISC-WORK-AREAS.                                             EL334
00189      05  X                       PIC X      VALUE SPACE.          EL334
00190      05  WS-CURRENT-BIN-DT       PIC XX     VALUE LOW-VALUES.     EL334
00191      05  WS-123180               PIC XX     VALUE X'797F'.        EL334
00192      05  WS-CLOSE-DT             PIC XX     VALUE LOW-VALUES.     EL334
00193                                                                   EL334
00194      05  SAVE-CONTROL            PIC X(36)  VALUE LOW-VALUES.     EL334
00195                                                                   EL334
00196      05  WS-SOC-SEC-NO.                                           EL334
00197          10  SSN-STATE           PIC XX.                          EL334
00198          10  SSN-ACCOUNT         PIC X(6).                        EL334
00199          10  FILLER              PIC X(3)    VALUE SPACES.        EL334
00200                                                                   EL334
00201      05  WS-CLAIM-NO             PIC X(7)    VALUE SPACES.        EL334
00202                                                                   EL334
00203      05  WS-ABEND-MESSAGE        PIC X(80)           VALUE SPACES.EL334
00204      05  WS-ABEND-FILE-STATUS    PIC XX              VALUE ZERO.  EL334
00205                                                                   EL334
00206      05  WS-PROCESSOR-ID.                                         EL334
00207          10  FILLER              PIC X(3) VALUE ALL 'Z'.          EL334
00208          10  WS-PROCESSOR-LAST-DIGIT  PIC X      VALUE SPACES.    EL334
00209                                                                   EL334
00210      05  WS-PROCESSOR-COUNT      PIC S9(4)V999 VALUE +0  COMP-3.  EL334
00211                                                                   EL334
00212      05  WS-PI-INDEX             PIC S9(4)   COMP.                EL334
00213                                                                   EL334
00214      05  WS-PI-CONSTANT          PIC X(36)   VALUE                EL334
00215          '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'.                  EL334
00216                                                                   EL334
00217      05  WS-PI-CHAR                  REDEFINES                    EL334
00218          WS-PI-CONSTANT          PIC X                            EL334
00219          OCCURS 36 TIMES.                                         EL334
00220                                                                   EL334
00221      05  WS-LAST-CLAIM-KEY       PIC X(20) VALUE LOW-VALUES.      EL334
00222      05  WS-LAST-TRAILER-KEY     PIC X(22) VALUE LOW-VALUES.      EL334
00223      05  WS-LAST-CERT-KEY        PIC X(33) VALUE LOW-VALUES.      EL334
00224                                                                   EL334
00225  01  MISC-SWITCHES.                                               EL334
00226      05  END-OF-CLAIMS-SW        PIC X      VALUE 'N'.            EL334
00227          88  END-OF-INPUT        VALUE 'Y'.                       EL334
00228      05  END-OF-CERTS-SW         PIC X      VALUE 'N'.            EL334
00229          88  END-OF-CERTS        VALUE 'Y'.                       EL334
00230                                                                   EL334
00231      05  WS-DISPLAY-COUNT        PIC Z,ZZZ,ZZ9-.                  EL334
00232                                                                   EL334
00233  01  COMP-3-WORK-AREA     COMP-3.                                 EL334
00234      05  WS-ZERO                 PIC S9      VALUE +0.            EL334
00235      05  ERR-COUNT               PIC S9(3)   VALUE +0.            EL334
00236      05  INC-ERR-CNT             PIC S9(7)   VALUE +0.            EL334
00237      05  RPT-ERR-CNT             PIC S9(7)   VALUE +0.            EL334
00238      05  EFF-ERR-CNT             PIC S9(7)   VALUE +0.            EL334
00239      05  PAY-ERR-CNT             PIC S9(7)   VALUE +0.            EL334
00240      05  PTO-ERR-CNT             PIC S9(7)   VALUE +0.            EL334
00241      05  PMT-COUNT               PIC S9(3)   VALUE +0.            EL334
00242      05  NOTE-COUNT              PIC S9(3)   VALUE +0.            EL334
00243      05  PROMPT-COUNT            PIC S9(3)   VALUE +0.            EL334
00244      05  INCCHG-COUNT            PIC S9(3)   VALUE +0.            EL334
00245      05  LINE-COUNT              PIC S9(3)   VALUE +99.           EL334
00246      05  PAGE-COUNT              PIC S9(3)   VALUE +0.            EL334
00247                                                                   EL334
00248      05  WS-END-SW               PIC S9      VALUE +0.            EL334
00249                                                                   EL334
00250      05  WS-CLAIMS-IN            PIC S9(7)   VALUE +0.            EL334
00251      05  WS-CERT-COUNT           PIC S9(7)   VALUE +0.            EL334
00252      05  WS-CLAIM-COUNT          PIC S9(7)   VALUE +0.            EL334
00253      05  WS-CLOSE-COUNT          PIC S9(7)   VALUE +0.            EL334
00254      05  WS-FINAL-COUNT          PIC S9(7)   VALUE +0.            EL334
00255      05  WS-TRLR-COUNT           PIC S9(7)   VALUE +0.            EL334
00256      05  WS-TRLRS-DELETED        PIC S9(7)   VALUE +0.            EL334
00257      05  WS-CERTS-DELETED        PIC S9(7)   VALUE +0.            EL334
00258      05  WS-CLAIMS-DELETED       PIC S9(7)   VALUE +0.            EL334
00259      05  WS-TRAILERS-INPUT       PIC S9(7)   VALUE +0.            EL334
00260      05  WS-DUP-TRAILER-COUNT    PIC S9(7)   VALUE +0.            EL334
00261      05  WS-CLAIMS-INPUT         PIC S9(7)   VALUE +0.            EL334
00262      05  WS-DUP-CLAIM-COUNT      PIC S9(7)   VALUE +0.            EL334
00263      05  WS-DUP-CERT-COUNT       PIC S9(7)   VALUE +0.            EL334
00264      05  WS-CLAIMS-OUTPUT        PIC S9(7)   VALUE +0.            EL334
00265      05  WS-CERTS-OUTPUT         PIC S9(7)   VALUE +0.            EL334
00266      05  WS-CERTS-DROPED         PIC S9(7)   VALUE +0.            EL334
00267      05  WS-TRAILERS-OUTPUT      PIC S9(7)   VALUE +0.            EL334
00268      05  WS-WORK                 PIC S9(7)   VALUE +0.            EL334
00269      05  WS-REMAINDER            PIC S9(7)   VALUE +0.            EL334
00270      05  WS-CLAIM-ATTACHED-COUNT PIC S9(3)   VALUE +0.            EL334
00271                                                                   EL334
00272  01  COMP-WORK-AREA       COMP.                                   EL334
00273      12  WS-RETURN-CODE          PIC S9(4)   VALUE ZERO.          EL334
00274      12  NAME-SUB1               PIC S9(4)   VALUE ZERO.          EL334
00275      12  NAME-SUB2               PIC S9(4)   VALUE ZERO.          EL334
00276      12  MSG-SUB                 PIC S9(4)   VALUE ZERO.          EL334
00277      12  SEQ-NUMBER              PIC S9(4)   VALUE ZERO.          EL334
00278                                                                   EL334
00279  01  HEADING-1.                                                   EL334
00280      12  FILLER                  PIC X(46)  VALUE SPACES.         EL334
00281      12  FILLER                  PIC X(16)  VALUE                 EL334
00282          'CLAS TO CLAS-IC '.                                      EL334
00283      12  FILLER                  PIC X(17)  VALUE                 EL334
00284          'CLAIMS CONVERSION'.                                     EL334
00285      12  FILLER                  PIC X(45)  VALUE SPACES.         EL334
00286      12  FILLER                  PIC X(8)   VALUE 'EL-334 '.      EL334
00287                                                                   EL334
00288                                                                   EL334
00289  01  HEADING-2.                                                   EL334
00290      12  FILLER                  PIC X(47)  VALUE SPACES.         EL334
00291      12  HD-CLIENT               PIC X(30).                       EL334
00292      12  FILLER                  PIC X(47)  VALUE SPACES.         EL334
00293      12  HD-RUN                  PIC X(8).                        EL334
00294                                                                   EL334
00295  01  HEADING-3.                                                   EL334
00296      12  FILLER                  PIC X(52)  VALUE SPACES.         EL334
00297      12  HD-DATE                 PIC X(18).                       EL334
00298      12  FILLER                  PIC X(42)  VALUE SPACES.         EL334
00299      12  FILLER                  PIC X(5)   VALUE 'PAGE '.        EL334
00300      12  HD-PAGE                 PIC ZZ,ZZZ.                      EL334
00301                                                                   EL334
00302  01  HEADING-4.                                                   EL334
00303      12  FILLER              PIC X(17)  VALUE '******** CLAIM IN'.EL334
00304      12  FILLER              PIC X(17)  VALUE 'FORMATION *******'.EL334
00305      12  FILLER              PIC X(17)  VALUE '*  ********CERT I'.EL334
00306      12  FILLER              PIC X(17)  VALUE 'NFORMATION ******'.EL334
00307      12  FILLER              PIC X(17)  VALUE '**   **** TRAILER'.EL334
00308      12  FILLER              PIC X(17)  VALUE ' COUNTS ******'.   EL334
00309                                                                   EL334
00310  01  HEADING-5.                                                   EL334
00311      12  FILLER              PIC X(17)  VALUE ' NUMBER TYPE STAT'.EL334
00312      12  FILLER              PIC X(17)  VALUE '  LN KND  INCURRE'.EL334
00313      12  FILLER              PIC X(17)  VALUE 'D   NUMBER   EFFE'.EL334
00314      12  FILLER              PIC X(17)  VALUE 'CTIVE CAR GROUP  '.EL334
00315      12  FILLER              PIC X(17)  VALUE 'ST   ACCOUNT     '.EL334
00316      12  FILLER              PIC X(17)  VALUE 'PMTS             '.EL334
00317      12  FILLER              PIC X(17)  VALUE '      ******  ERR'.EL334
00318      12  FILLER              PIC X(17)  VALUE 'ORS  ******'.      EL334
00319                                                                   EL334
00320  01  DETAIL-LINE.                                                 EL334
00321      12  DL-CLM-NO               PIC X(7).                        EL334
00322      12  FILLER                  PIC X(02)  VALUE SPACES.         EL334
00323      12  DL-CLM-TYPE             PIC X(5).                        EL334
00324      12  FILLER                  PIC X      VALUE SPACES.         EL334
00325      12  DL-CLM-STAT             PIC X.                           EL334
00326      12  FILLER                  PIC XX     VALUE SPACES.         EL334
00327      12  DL-CLM-KIND             PIC X(7).                        EL334
00328      12  FILLER                  PIC XX     VALUE SPACES.         EL334
00329      12  DL-CLM-IMM              PIC XX.                          EL334
00330      12  DL-CLM-DASH1            PIC X.                           EL334
00331      12  DL-CLM-IDD              PIC XX.                          EL334
00332      12  DL-CLM-DASH2            PIC X.                           EL334
00333      12  DL-CLM-IYY              PIC XX.                          EL334
00334      12  FILLER                  PIC X      VALUE SPACES.         EL334
00335      12  DL-CERT-NO              PIC X(11).                       EL334
00336      12  FILLER                  PIC X      VALUE SPACE.          EL334
00337      12  DL-CERT-EMM             PIC XX.                          EL334
00338      12  DL-CERT-DASH1           PIC X.                           EL334
00339      12  DL-CERT-EDD             PIC XX.                          EL334
00340      12  DL-CERT-DASH2           PIC X.                           EL334
00341      12  DL-CERT-EYY             PIC XX.                          EL334
00342      12  FILLER                  PIC XX     VALUE SPACES.         EL334
00343      12  DL-CERT-CARR            PIC X.                           EL334
00344      12  FILLER                  PIC XX     VALUE SPACES.         EL334
00345      12  DL-CERT-GRP             PIC X(6).                        EL334
00346      12  FILLER                  PIC X      VALUE SPACE.          EL334
00347      12  DL-CERT-ST              PIC XX.                          EL334
00348      12  FILLER                  PIC XX     VALUE SPACES.         EL334
00349      12  DL-CERT-ACCOUNT         PIC X(10).                       EL334
00350      12  FILLER                  PIC XX     VALUE SPACES.         EL334
00351      12  DL-TRLR-PMTS            PIC ZZZ9.                        EL334
00352      12  FILLER                  PIC X(5)   VALUE SPACES.         EL334
00353      12  DL-ERR-MSG              PIC X(20)  VALUE SPACES.         EL334
00354      12  FILLER                  PIC X(4)   VALUE SPACES.         EL334
00355      12  DL-ERR-COUNT            PIC ZZZ9.                        EL334
00356                                                                   EL334
00357                                  COPY ELCDATE.                       CL**6
00358                                                                   EL334
00359                                  COPY ELCDTECX.                      CL**2
00360                                                                   EL334
00361                                  COPY ELCDTEVR.                      CL**3
00362                                                                      CL**3
00363                                  COPY ELCCRTVR.                      CL**4
00364                                                                      CL**5
00365                                  COPY ELCEXTVR.                      CL**5
00366                                                                      CL**4
00367      EJECT                                                        EL334
00368  PROCEDURE DIVISION.                                              EL334
00369                                                                   EL334
00370  0000-LOAD-DATE-CARD.            COPY ELCDTERX.                      CL**2
00371                                                                   EL334
00372  0002-SELECT-AND-SORT.                                            EL334
00373      SORT SORT-WORK-FILE                                          EL334
00374          ON ASCENDING KEY DE-CONTROL                              EL334
00375                           DE-CNUM                                 EL334
00376                           DE-PAY                                  EL334
00377          INPUT PROCEDURE  0200-SELECT-EXTRACTS                    EL334
00378          OUTPUT PROCEDURE 0100-PROCESS-CLAIMS-HISTORY.            EL334
00379                                                                   EL334
00380      IF SORT-RETURN NOT = ZERO                                    EL334
00381          MOVE 'SORT FAILED - SORT' TO  WS-ABEND-MESSAGE           EL334
00382          MOVE SORT-RETURN          TO  WS-RETURN-CODE             EL334
00383          PERFORM ABEND-PGM.                                       EL334
00384                                                                   EL334
00385      SORT CLAIM-SORT-WORK-FILE                                    EL334
00386          ON ASCENDING KEY SWR-CLAIM-KEY                           EL334
00387          USING ELMSTR                                             EL334
00388          OUTPUT PROCEDURE 9400-DROP-DUP-CLAIMS.                   EL334
00389                                                                   EL334
00390      IF SORT-RETURN NOT = ZERO                                    EL334
00391          MOVE 'SORT FAILED - SORT CLAIMS' TO  WS-ABEND-MESSAGE    EL334
00392          MOVE SORT-RETURN        TO  WS-RETURN-CODE               EL334
00393          PERFORM ABEND-PGM.                                       EL334
00394                                                                   EL334
00395      SORT TRAILER-SORT-WORK-FILE                                  EL334
00396          ON ASCENDING KEY SWR-TRAILER-KEY                         EL334
00397          USING ELTRLR                                             EL334
00398          OUTPUT PROCEDURE 9500-DROP-DUP-TRAILERS.                 EL334
00399                                                                   EL334
00400      IF SORT-RETURN NOT = ZERO                                    EL334
00401          MOVE 'SORT FAILED - SORT TRAILERS' TO  WS-ABEND-MESSAGE  EL334
00402          MOVE SORT-RETURN        TO  WS-RETURN-CODE               EL334
00403          PERFORM ABEND-PGM.                                       EL334
00404                                                                   EL334
00405      GOBACK.                                                      EL334
00406                                                                   EL334
00407      EJECT                                                        EL334
00408  0100-PROCESS-CLAIMS-HISTORY SECTION.                             EL334
00409                                                                   EL334
00410      PERFORM 9700-OPEN-FILES.                                     EL334
00411                                                                   EL334
00412      RETURN SORT-WORK-FILE                                        EL334
00413         AT END                                                    EL334
00414          MOVE 'NO RECORDS EXTRACTED'  TO  WS-ABEND-MESSAGE        EL334
00415          PERFORM ABEND-PGM.                                       EL334
00416                                                                   EL334
00417      MOVE DE-CONTROL             TO SAVE-CONTROL.                 EL334
00418                                                                   EL334
00419      PERFORM 9050-READ-CERT THRU 9050-EXIT                        EL334
00420                                                                   EL334
00421      PERFORM 0500-CONVERT-CLAIMS-IN.                              EL334
00422                                                                   EL334
00423      PERFORM 9800-CLOSE-FILES.                                    EL334
00424                                                                   EL334
00425  0100-EXIT.                                                       EL334
00426                                                                   EL334
00427      EXIT.                                                        EL334
00428                                                                   EL334
00429      EJECT                                                        EL334
00430  0200-SELECT-EXTRACTS SECTION.                                    EL334
00431                                                                   EL334
00432       OPEN INPUT CLAIM-EXTRACT-IN.                                EL334
00433                                                                   EL334
00434  0250-READ-EXTRACT-LOOP.                                          EL334
00435       READ CLAIM-EXTRACT-IN INTO DETAIL-EXTRACT                   EL334
00436          AT END                                                   EL334
00437              CLOSE CLAIM-EXTRACT-IN                               EL334
00438              GO TO 0200-EXIT.                                     EL334
00439                                                                   EL334
00440      IF DE-REIN NOT = SPACE                                       EL334
00441          GO TO 0250-READ-EXTRACT-LOOP.                            EL334
00442                                                                   EL334
00443      IF DE-DTH OR DE-AH OR DE-OB-DTH OR DE-OB-AH                  EL334
00444          NEXT SENTENCE                                            EL334
00445       ELSE                                                        EL334
00446          GO TO 0250-READ-EXTRACT-LOOP.                            EL334
00447                                                                   EL334
00448      COPY ELCEXTM1.                                                  CL**4
00449                                                                      CL**4
00450      IF DE-DAYS-DISAB NOT NUMERIC                                 EL334
00451          MOVE ZERO               TO  DE-DAYS-DISAB.               EL334
00452                                                                   EL334
00453      RELEASE DETAIL-EXTRACT.                                      EL334
00454                                                                   EL334
00455      ADD +1 TO WS-CLAIMS-IN.                                      EL334
00456                                                                   EL334
00457      GO TO 0250-READ-EXTRACT-LOOP.                                EL334
00458                                                                   EL334
00459  0200-EXIT.                                                       EL334
00460      EXIT.                                                        EL334
00461                                                                   EL334
00462      EJECT                                                        EL334
00463  0500-CONVERT-CLAIMS-IN SECTION.                                  EL334
00464                                                                   EL334
00465      IF END-OF-INPUT OR END-OF-CERTS                              EL334
00466          GO TO 0599-EXIT                                          EL334
00467      ELSE                                                         EL334
00468          PERFORM 9050-READ-CERT THRU 9050-EXIT                    EL334
00469              UNTIL (CR-FULL-CONTROL NOT LESS THAN DE-CONTROL      EL334
00470              OR  END-OF-CERTS).                                   EL334
00471                                                                   EL334
00472      IF CR-FULL-CONTROL NOT = DE-CONTROL                          EL334
00473          PERFORM 1600-READ-CLAIM-WORK THRU 1699-EXIT              EL334
00474              UNTIL (DE-CONTROL NOT = SAVE-CONTROL                 EL334
00475              OR  END-OF-INPUT)                                    EL334
00476          MOVE DE-CONTROL  TO  SAVE-CONTROL                        EL334
00477          GO TO 0500-CONVERT-CLAIMS-IN.                            EL334
00478                                                                   EL334
00479      MOVE +4095                  TO SEQ-NUMBER.                   EL334
00480                                                                   EL334
00481      MOVE ZERO                   TO ERR-COUNT                     EL334
00482                                     PMT-COUNT.                    EL334
00483                                                                   EL334
00484      MOVE DE-CNUM                TO  WS-CLAIM-NO.                 EL334
00485                                                                   EL334
00486      PERFORM 1000-LOAD-ELMSTR.                                    EL334
00487                                                                   EL334
00488      PERFORM 1500-BUILD-PAYMENT-TRAILERS THRU 1599-EXIT.          EL334
00489                                                                   EL334
00490      PERFORM 6000-BUILD-ZERO-TRAILER.                             EL334
00491                                                                   EL334
00492      PERFORM 2800-ADD-NEW-CERT.                                   EL334
00493                                                                   EL334
00494      PERFORM 7000-PRINT-DETAIL.                                   EL334
00495                                                                   EL334
00496      PERFORM 9200-WRITE-ELMSTR.                                   EL334
00497                                                                   EL334
00498      IF END-OF-INPUT OR END-OF-CERTS                              EL334
00499          GO TO 0599-EXIT                                          EL334
00500       ELSE                                                        EL334
00501           GO TO 0500-CONVERT-CLAIMS-IN.                           EL334
00502                                                                   EL334
00503  0599-EXIT.                                                       EL334
00504      EXIT.                                                        EL334
00505                                                                   EL334
00506      EJECT                                                        EL334
00507  1000-LOAD-ELMSTR SECTION.                                        EL334
00508                                                                   EL334
00509      MOVE SPACES                 TO CLAIM-MASTER.                 EL334
00510                                                                   EL334
00511      MOVE 'CL'                   TO CL-RECORD-ID.                 EL334
00512                                                                   EL334
00513      MOVE DTE-CLASIC-COMPANY-CD  TO CL-COMPANY-CD                 EL334
00514                                     CL-COMPANY-CD-A1              EL334
00515                                     CL-COMPANY-CD-A2              EL334
00516                                     CL-COMPANY-CD-A4              EL334
00517                                     CL-COMPANY-CD-A5.             EL334
00518                                                                   EL334
00519      MOVE CR-CARRIER             TO CL-CARRIER                    EL334
00520                                     CL-CERT-CARRIER.              EL334
00521                                                                   EL334
00522      MOVE DE-CNUM                TO CL-CLAIM-NO.                  EL334
00523                                                                   EL334
00524      MOVE CR-CERT-NO             TO CL-CERT-NO                    EL334
00525                                     CL-CERT-NO-A4                 EL334
00526                                     CL-PRIME-CERT-NO.             EL334
00527                                                                   EL334
00528 ** POPULATE THE CREDIT-CARD NO WITH THE CERT NO.                  EL334
00529      MOVE CR-CERT-NO             TO CL-CCN-A5.                    EL334
00530                                                                   EL334
00531      MOVE CR-LNAME               TO CL-INSURED-LAST-NAME.         EL334
00532      MOVE CR-FNAME               TO CL-INSURED-1ST-NAME.          EL334
00533      MOVE CR-INIT                TO CL-INSURED-MID-INIT.          EL334
00534      MOVE CR-SOC-SEC             TO CL-SOC-SEC-NO.                EL334
00535                                                                   EL334
00536      IF CL-SSN-ACCOUNT = ZEROS OR SPACES                          EL334
00537         MOVE CR-STATE            TO CL-SSN-STATE                  EL334
00538         MOVE CR-ACCT-PRIME       TO CL-SSN-ACCOUNT                EL334
00539         MOVE CR-LNAME            TO CL-SSN-LN3.                   EL334
00540                                                                   EL334
00541      MOVE 'L334'                 TO CL-PROCESSOR-ID.              EL334
00542      IF DTE-CLIENT = 'CRI'                                        EL334
00543          MOVE DE-LOAN-OFFICER    TO CL-PROCESSOR-ID.              EL334
00544                                                                   EL334
00545      MOVE LOW-VALUES             TO CL-INSURED-BIRTH-DT.          EL334
00546      MOVE CR-SEX                 TO CL-INSURED-SEX-CD.            EL334
00547      MOVE SPACES                 TO CL-INSURED-OCC-CD.            EL334
00548      MOVE 'O'                    TO CL-CLAIM-STATUS.              EL334
00549                                                                   EL334
00550      IF DE-TYPE = '1' OR '3'                                      EL334
00551          MOVE LIFE-OVERRIDE-L1   TO CL-CLAIM-TYPE                 EL334
00552      ELSE                                                         EL334
00553          MOVE AH-OVERRIDE-L1     TO CL-CLAIM-TYPE.                EL334
00554                                                                   EL334
00555      IF DE-TYPE = '3' OR '4'                                      EL334
00556          MOVE '2'                TO CL-CLAIM-PREM-TYPE            EL334
00557      ELSE                                                         EL334
00558          MOVE '1'                TO CL-CLAIM-PREM-TYPE.           EL334
00559                                                                   EL334
00560      MOVE DE-INCUR-CC           TO DC-ALPHA-CEN-N.                EL334
00561      MOVE DE-INCUR-YR           TO DC-YMD-YEAR.                   EL334
00562      MOVE DE-INCUR-MO           TO DC-YMD-MONTH.                  EL334
00563      MOVE DE-INCUR-DA           TO DC-YMD-DAY.                    EL334
00564      MOVE '3'                    TO DC-OPTION-CODE.               EL334
00565      PERFORM 8500-DATE-CONVERSION.                                EL334
00566                                                                   EL334
00567      IF DATE-CONVERSION-ERROR                                     EL334
00568         ADD +1      TO ERR-COUNT  INC-ERR-CNT                     EL334
00569         MOVE LOW-VALUES         TO CL-INCURRED-DT                 EL334
00570                                    CL-REPORTED-DT                 EL334
00571                                    CL-FILE-ESTABLISH-DT           EL334
00572      ELSE                                                         EL334
00573         MOVE DC-BIN-DATE-1      TO CL-INCURRED-DT                 EL334
00574                                    CL-REPORTED-DT                 EL334
00575                                    CL-FILE-ESTABLISH-DT.          EL334
00576                                                                   EL334
00577      MOVE DE-REPORTED           TO DC-GREG-DATE-1-YMD.            EL334
00578      MOVE '3'                    TO DC-OPTION-CODE.               EL334
00579      PERFORM 8500-DATE-CONVERSION.                                EL334
00580                                                                   EL334
00581      IF NOT DATE-CONVERSION-ERROR                                 EL334
00582         MOVE DC-BIN-DATE-1      TO CL-REPORTED-DT                 EL334
00583                                    CL-FILE-ESTABLISH-DT           EL334
00584      ELSE                                                         EL334
00585         ADD +1      TO ERR-COUNT  RPT-ERR-CNT.                    EL334
00586                                                                   EL334
00587      MOVE LOW-VALUES             TO CL-EST-END-OF-DISAB-DT        EL334
00588                                     CL-LAST-PMT-DT                EL334
00589                                     CL-HISTORY-ARCHIVE-DT.        EL334
00590                                                                   EL334
00591      MOVE ZERO                   TO CL-LAST-PMT-AMT.              EL334
00592                                                                   EL334
00593      MOVE LOW-VALUES             TO CL-PAID-THRU-DT.              EL334
00594                                                                   EL334
00595      MOVE ZERO                   TO CL-ASSOC-CERT-TOTAL           EL334
00596                                     CL-ASSOC-CERT-SEQU            EL334
00597                                     CL-CLAIM-PAYMENT-STATUS.      EL334
00598                                                                   EL334
00599      MOVE ZERO                   TO CL-TOTAL-PAID-AMT.            EL334
00600      MOVE ZERO                   TO CL-NO-OF-PMTS-MADE.           EL334
00601      MOVE ZERO                   TO CL-NO-OF-DAYS-PAID.           EL334
00602      MOVE '1'                    TO CL-PMT-CALC-METHOD.           EL334
00603      MOVE DE-CLM-CAUSE           TO CL-CAUSE-CD.                  EL334
00604      MOVE LOW-VALUES             TO CL-LAST-ADD-ON-DT             EL334
00605                                     CL-LAST-REOPEN-DT             EL334
00606                                     CL-LAST-CLOSE-DT.             EL334
00607      MOVE '2'                    TO CL-CERT-ORIGIN.               EL334
00608      MOVE CR-GROUPING            TO CL-CERT-GROUPING.             EL334
00609      MOVE CR-STATE               TO CL-CERT-STATE.                EL334
00610      MOVE CR-ACCOUNT             TO CL-CERT-ACCOUNT.              EL334
00611      MOVE CR-CARRIER             TO CL-CERT-CARRIER.              EL334
00612      MOVE CR-YR                  TO DC-YMD-YEAR.                  EL334
00613      MOVE CR-MO                  TO DC-YMD-MONTH.                 EL334
00614      MOVE CR-DA                  TO DC-YMD-DAY.                   EL334
00615      MOVE CR-CC                  TO DC-ALPHA-CEN-N.               EL334
00616      MOVE '3'                    TO DC-OPTION-CODE.               EL334
00617      PERFORM 8500-DATE-CONVERSION.                                EL334
00618                                                                   EL334
00619      IF DATE-CONVERSION-ERROR                                     EL334
00620         ADD +1      TO ERR-COUNT  EFF-ERR-CNT                     EL334
00621         MOVE LOW-VALUES         TO CL-CERT-EFF-DT                 EL334
00622      ELSE                                                         EL334
00623         MOVE DC-BIN-DATE-1      TO CL-CERT-EFF-DT.                EL334
00624                                                                   EL334
00625      MOVE LOW-VALUES             TO CL-PURGED-DT                  EL334
00626                                     CL-RESTORED-DT                EL334
00627                                     CL-NEXT-AUTO-PAY-DT           EL334
00628                                     CL-NEXT-RESEND-DT             EL334
00629                                     CL-NEXT-FOLLOWUP-DT.          EL334
00630                                                                   EL334
00631      MOVE DE-PAY-YR              TO DC-YMD-YEAR.                  EL334
00632      MOVE DE-PAY-MO              TO DC-YMD-MONTH.                 EL334
00633      MOVE DE-PAY-DA              TO DC-YMD-DAY.                   EL334
00634      MOVE DE-PAY-CC              TO DC-ALPHA-CEN-N.               EL334
00635      MOVE '3'                    TO DC-OPTION-CODE.               EL334
00636      PERFORM 8500-DATE-CONVERSION.                                EL334
00637                                                                   EL334
00638      IF DATE-CONVERSION-ERROR                                     EL334
00639         MOVE CL-FILE-ESTABLISH-DT   TO CL-LAST-MAINT-DT           EL334
00640         MOVE '6'                    TO CL-LAST-MAINT-TYPE         EL334
00641      ELSE                                                         EL334
00642          MOVE DC-BIN-DATE-1      TO CL-LAST-MAINT-DT              EL334
00643          MOVE '1'                TO CL-LAST-MAINT-TYPE.           EL334
00644                                                                   EL334
00645      MOVE 'L334'                 TO CL-LAST-MAINT-USER.           EL334
00646      IF DTE-CLIENT = 'CRI'                                        EL334
00647          MOVE DE-LOAN-OFFICER    TO CL-LAST-MAINT-USER.           EL334
00648                                                                   EL334
00649      MOVE ZERO                   TO CL-LAST-MAINT-HHMMSS          EL334
00650                                     CL-LAST-INC-DT-CHANGE         EL334
00651                                     CL-AUTO-PAY-SEQ               EL334
00652                                     CL-FATAL-ERROR-CNT            EL334
00653                                     CL-INSURED-ADDR-CNT           EL334
00654                                     CL-ACCOUNT-ADDR-CNT           EL334
00655                                     CL-BENIF-ADDR-CNT             EL334
00656                                     CL-EMPLOYER-ADDR-CNT          EL334
00657                                     CL-DOCTOR-ADDR-CNT            EL334
00658                                     CL-OTHER-1-ADDR-CNT           EL334
00659                                     CL-OTHER-2-ADDR-CNT           EL334
00660                                     CL-FORCEABLE-ERROR-CNT.       EL334
00661                                                                   EL334
00662      MOVE +4095 TO CL-TRAILER-SEQ-CNT.                            EL334
00663                                                                   EL334
00664      ADD +1  TO  WS-CLAIM-COUNT.                                  EL334
00665                                                                   EL334
00666  1099-EXIT.                                                       EL334
00667      EXIT.                                                        EL334
00668                                                                   EL334
00669      EJECT                                                        EL334
00670  1500-BUILD-PAYMENT-TRAILERS SECTION.                             EL334
00671                                                                   EL334
00672      IF DE-CLAIM-AMT = ZEROS                                      EL334
00673          GO TO 1590-BYPASS-PYMT-TRAILER.                          EL334
00674                                                                   EL334
00675      ADD +1                  TO PMT-COUNT.                        EL334
00676      ADD DE-CLAIM-AMT        TO CL-TOTAL-PAID-AMT                 EL334
00677      MOVE SPACES             TO ACTIVITY-TRAILERS.                EL334
00678      MOVE 'AT'               TO AT-RECORD-ID.                     EL334
00679      MOVE CL-CONTROL-PRIMARY TO AT-CONTROL-PRIMARY.               EL334
00680      MOVE '2'                TO AT-TRAILER-TYPE.                  EL334
00681      SUBTRACT +1 FROM SEQ-NUMBER                                  EL334
00682                                                                   EL334
00683      MOVE SEQ-NUMBER         TO CL-TRAILER-SEQ-CNT                EL334
00684                                 AT-SEQUENCE-NO.                   EL334
00685      ADD +1                  TO CL-NO-OF-PMTS-MADE.               EL334
00686                                                                   EL334
00687      MOVE DE-PAY-YR              TO DC-YMD-YEAR.                  EL334
00688      MOVE DE-PAY-MO              TO DC-YMD-MONTH.                 EL334
00689      MOVE DE-PAY-DA              TO DC-YMD-DAY.                   EL334
00690      MOVE DE-PAY-CC              TO DC-ALPHA-CEN-N.               EL334
00691      MOVE '3'                    TO DC-OPTION-CODE.               EL334
00692      PERFORM 8500-DATE-CONVERSION.                                EL334
00693                                                                   EL334
00694      IF DATE-CONVERSION-ERROR                                     EL334
00695         ADD +1      TO ERR-COUNT  PAY-ERR-CNT                     EL334
00696         MOVE LOW-VALUES         TO AT-RECORDED-DT                 EL334
00697                                    AT-CHECK-WRITTEN-DT            EL334
00698                                    AT-PAYMENT-LAST-MAINT-DT       EL334
00699                                    CL-LAST-PMT-DT                 EL334
00700      ELSE                                                         EL334
00701         MOVE DC-BIN-DATE-1      TO AT-RECORDED-DT                 EL334
00702                                    AT-CHECK-WRITTEN-DT            EL334
00703                                    AT-PAYMENT-LAST-MAINT-DT       EL334
00704                                    CL-LAST-PMT-DT.                EL334
00705                                                                   EL334
00706      MOVE 'L334'                 TO AT-RECORDED-BY                EL334
00707                                     AT-PAYMENT-LAST-UPDATED-BY.   EL334
00708      IF DTE-CLIENT = 'CRI'                                        EL334
00709          MOVE CL-PROCESSOR-ID    TO AT-RECORDED-BY                EL334
00710                                     AT-PAYMENT-LAST-UPDATED-BY.   EL334
00711      MOVE ZERO                   TO AT-LAST-MAINT-HHMMSS.         EL334
00712                                                                   EL334
00713      MOVE DE-PAY-CODE           TO AT-PAYMENT-TYPE.               EL334
00714      INSPECT AT-PAYMENT-TYPE CONVERTING 'PFSXEC' TO '123469'.     EL334
00715                                                                   EL334
00716      DISPLAY AT-PAYMENT-TYPE.                                     EL334
00717      MOVE CL-CLAIM-TYPE          TO AT-CLAIM-TYPE.                EL334
00718      MOVE CL-CLAIM-PREM-TYPE     TO AT-CLAIM-PREM-TYPE.           EL334
00719      MOVE DE-CLAIM-AMT           TO AT-AMOUNT-PAID                EL334
00720                                     CL-LAST-PMT-AMT.              EL334
00721      MOVE DE-CHECK               TO AT-CHECK-NO.                  EL334
00722      MOVE LOW-VALUES             TO AT-PAID-FROM-DT               EL334
00723                                     AT-PAID-THRU-DT.              EL334
00724                                                                   EL334
00725      MOVE DE-PTO-CC             TO DC-ALPHA-CEN-N.                EL334
00726      MOVE DE-PTO-YR             TO DC-YMD-YEAR.                   EL334
00727      MOVE DE-PTO-MO             TO DC-YMD-MONTH.                  EL334
00728      MOVE DE-PTO-DA             TO DC-YMD-DAY.                    EL334
00729      MOVE '3'                   TO DC-OPTION-CODE.                EL334
00730      PERFORM 8500-DATE-CONVERSION.                                EL334
00731                                                                   EL334
00732      IF NO-CONVERSION-ERROR                                       EL334
00733         MOVE DC-BIN-DATE-1    TO AT-PAID-THRU-DT                  EL334
00734         MOVE AT-PAID-THRU-DT  TO  CL-PAID-THRU-DT                 EL334
00735      ELSE                                                         EL334
00736         ADD +1      TO ERR-COUNT  PTO-ERR-CNT.                    EL334
00737                                                                   EL334
00738      MOVE DE-DAYS-DISAB         TO AT-DAYS-IN-PERIOD.             EL334
00739      ADD AT-DAYS-IN-PERIOD      TO CL-NO-OF-DAYS-PAID.            EL334
00740      MOVE '3'                   TO AT-PAYEE-TYPE.                 EL334
00741      MOVE CL-INSURED-LAST-NAME  TO AT-PAYEES-NAME.                EL334
00742      MOVE '3'                   TO AT-PAYMENT-ORIGIN.             EL334
00743      MOVE LOW-VALUES             TO AT-TO-BE-WRITTEN-DT           EL334
00744                                     AT-VOID-DT.                   EL334
00745                                                                   EL334
00746      MOVE +99999999              TO AT-CHECK-QUE-CONTROL.         EL334
00747                                                                   EL334
00748      MOVE ZERO                   TO AT-ADDL-RESERVE               EL334
00749                                     AT-EXPENSE-PER-PMT.           EL334
00750      MOVE +0                     TO AT-ELIMINATION-DAYS.          EL334
00751                                                                   EL334
00752      MOVE AT-CHECK-WRITTEN-DT    TO AT-PMT-SELECT-DT              EL334
00753                                     AT-PMT-ACCEPT-DT.             EL334
00754                                                                   EL334
00755      MOVE LOW-VALUES             TO AT-VOID-SELECT-DT             EL334
00756                                     AT-VOID-ACCEPT-DT.            EL334
00757                                                                   EL334
00758      MOVE ZERO                   TO AT-PAYMENT-NOTE-SEQ-NO        EL334
00759                                     AT-DAILY-RATE                 EL334
00760                                     AT-CHECK-QUE-SEQUENCE.        EL334
00761                                                                   EL334
00762      IF AT-PAYMENT-TYPE = '2' OR '3'                              EL334
00763          ADD +1 TO WS-FINAL-COUNT                                 EL334
00764          MOVE 'C'                TO CL-CLAIM-STATUS               EL334
00765          MOVE CL-LAST-PMT-DT     TO CL-LAST-CLOSE-DT              EL334
00766          MOVE '1'                TO CL-LAST-CLOSE-REASON.         EL334
00767                                                                   EL334
00768      MOVE ZERO                   TO  AT-PREV-LAST-PMT-AMT         EL334
00769                                                                   EL334
00770      MOVE LOW-VALUES             TO  AT-PREV-PAID-THRU-DT         EL334
00771                                      AT-PREV-LAST-PMT-DT          EL334
00772                                                                   EL334
00773      PERFORM 9300-WRITE-TRAILER THRU 9399-EXIT.                   EL334
00774                                                                   EL334
00775  1590-BYPASS-PYMT-TRAILER.                                        EL334
00776                                                                   EL334
00777      PERFORM 1600-READ-CLAIM-WORK                                 EL334
00778                                                                   EL334
00779      IF DE-CONTROL = SAVE-CONTROL AND                             EL334
00780         DE-CNUM    = WS-CLAIM-NO                                  EL334
00781         GO TO 1500-BUILD-PAYMENT-TRAILERS                         EL334
00782       ELSE                                                        EL334
00783         MOVE DE-CONTROL TO SAVE-CONTROL.                          EL334
00784                                                                   EL334
00785  1599-EXIT.                                                       EL334
00786      EXIT.                                                        EL334
00787                                                                   EL334
00788  1600-READ-CLAIM-WORK SECTION.                                    EL334
00789                                                                   EL334
00790      RETURN SORT-WORK-FILE                                        EL334
00791         AT END     MOVE HIGH-VALUES TO DE-CONTROL                 EL334
00792                    MOVE 'Y' TO END-OF-CLAIMS-SW                   EL334
00793                    GO TO 1699-EXIT.                               EL334
00794                                                                   EL334
00795  1699-EXIT.                                                       EL334
00796      EXIT.                                                        EL334
00797      EJECT                                                        EL334
00798  2800-ADD-NEW-CERT SECTION.                                       EL334
00799                                                                   EL334
00800      MOVE SPACES  TO CERTIFICATE-MASTER.                          EL334
00801                                                                   EL334
00802      MOVE 'CM'                   TO CM-RECORD-ID.                 EL334
00803 * BUILD CONTROL PRIMARY                                           EL334
00804                                                                   EL334
00805      MOVE DTE-CLASIC-COMPANY-CD     TO CM-COMPANY-CD              EL334
00806                                        CM-COMPANY-CD-A1           EL334
00807                                        CM-COMPANY-CD-A2           EL334
00808                                        CM-COMPANY-CD-A4           EL334
00809                                        CM-COMPANY-CD-A5.          EL334
00810                                                                   EL334
00811      MOVE CR-CARRIER             TO CM-CARRIER.                   EL334
00812      MOVE CR-GROUPING            TO CM-GROUPING.                  EL334
00813      MOVE CR-STATE               TO CM-STATE.                     EL334
00814      MOVE CR-ACCOUNT             TO CM-ACCOUNT.                   EL334
00815      MOVE CR-CERT-NO             TO CM-CERT-PRIME.                EL334
00816      MOVE CR-CERT-SFX            TO CM-CERT-SFX.                  EL334
00817                                                                   EL334
00818      MOVE '3'   TO DC-OPTION-CODE.                                EL334
00819      MOVE CR-YR                  TO DC-YMD-YEAR.                  EL334
00820      MOVE CR-MO                  TO DC-YMD-MONTH.                 EL334
00821      MOVE CR-DA                  TO DC-YMD-DAY.                   EL334
00822      MOVE CR-CC                  TO DC-ALPHA-CEN-N.               EL334
00823      PERFORM 8500-DATE-CONVERSION                                 EL334
00824                                                                   EL334
00825      IF DC-ERROR-CODE NOT = SPACES                                EL334
00826         ADD +1      TO ERR-COUNT                                  EL334
00827         MOVE LOW-VALUES         TO  CM-CERT-EFF-DT                EL334
00828      ELSE                                                         EL334
00829         MOVE DC-BIN-DATE-1      TO CM-CERT-EFF-DT.                EL334
00830                                                                   EL334
00831      MOVE CM-CERT-NO      TO CM-CERT-NO-A4.                       EL334
00832                                                                   EL334
00833      IF CR-SOC-SEC  = SPACES                                      EL334
00834          MOVE CR-STATE           TO CM-SSN-STATE                  EL334
00835          MOVE CR-ACCOUNT         TO CM-SSN-ACCOUNT                EL334
00836          MOVE CR-INIT            TO CM-INSURED-INITIAL2-A2        EL334
00837          MOVE CR-1ST-INITIAL     TO CM-INSURED-INITIAL1-A2        EL334
00838          MOVE CR-LNAME           TO CM-PART-LAST-NAME-A2          EL334
00839      ELSE                                                         EL334
00840          MOVE CR-SOC-SEC         TO CM-SOC-SEC-NO.                EL334
00841                                                                   EL334
00842      IF CR-MEMBER-NO = SPACES                                     EL334
00843          MOVE CR-STATE           TO CM-MEMB-STATE                 EL334
00844          MOVE CR-ACCOUNT         TO CM-MEMB-ACCOUNT               EL334
00845          MOVE CR-INIT            TO CM-INSURED-INITIAL2-A5        EL334
00846          MOVE CR-1ST-INITIAL     TO CM-INSURED-INITIAL1-A5        EL334
00847          MOVE CR-LNAME           TO CM-PART-LAST-NAME-A5          EL334
00848      ELSE                                                         EL334
00849          MOVE CR-MEMBER-NO       TO CM-MEMBER-NO.                 EL334
00850                                                                   EL334
00851      MOVE CR-LNAME               TO CM-INSURED-LAST-NAME.         EL334
00852      MOVE CR-FNAME               TO CM-INSURED-FIRST-NAME.        EL334
00853      MOVE CR-1ST-INITIAL         TO CM-INSURED-INITIAL1.          EL334
00854      MOVE CR-INIT                TO  CM-INSURED-INITIAL2.         EL334
00855                                                                   EL334
00856      MOVE CR-JOINT-NAME          TO CM-JOINT-INSURED-NAME.        EL334
00857                                                                   EL334
00858      MOVE LOW-VALUES             TO CM-STATUS-DATA.               EL334
00859      ADD +1                      TO  WS-CLAIM-ATTACHED-COUNT      EL334
00860      MOVE WS-CLAIM-ATTACHED-COUNT TO CM-CLAIM-ATTACHED-COUNT.     EL334
00861      MOVE CR-AGE                 TO CM-INSURED-ISSUE-AGE.         EL334
00862                                                                   EL334
00863      IF CR-JOINT-AGE NUMERIC                                      EL334
00864           MOVE CR-JOINT-AGE      TO CM-INSURED-JOINT-AGE          EL334
00865       ELSE                                                        EL334
00866            MOVE ZEROS            TO CM-INSURED-JOINT-AGE.         EL334
00867                                                                   EL334
00868      MOVE CR-SEX                 TO CM-INSURED-SEX.               EL334
00869                                                                   EL334
00870 ******************************************************************EL334
00871 ****    START BUILDING LIFE COVERAGE ON ON-LINE CERT           ***EL334
00872 ******************************************************************EL334
00873                                                                   EL334
00874      MOVE CR-LFTYP               TO CM-LF-BENEFIT-CD.             EL334
00875      MOVE CR-LF-TERM             TO CM-LF-ORIG-TERM.              EL334
00876      MOVE CR-LF-CRIT-PERIOD      TO CM-LF-CRITICAL-PERIOD.        EL334
00877      MOVE CR-LF-TERM-IN-DAYS     TO CM-LF-TERM-IN-DAYS.           EL334
00878      MOVE CR-LF-DEV-CODE         TO CM-LF-DEV-CODE.               EL334
00879      MOVE CR-LF-DEV-PCT          TO CM-LF-DEV-PCT.                EL334
00880      MOVE CR-LFAMT               TO CM-LF-BENEFIT-AMT.            EL334
00881      MOVE CR-LFPRM               TO CM-LF-PREMIUM-AMT.            EL334
00882      MOVE CR-LFAMT-ALT           TO CM-LF-ALT-BENEFIT-AMT.        EL334
00883      MOVE CR-LFPRM-ALT           TO CM-LF-ALT-PREMIUM-AMT.        EL334
00884      MOVE CR-LF-NSP-PRM          TO CM-LF-NSP-PREMIUM-AMT.        EL334
00885      MOVE ZEROS                  TO CM-LF-REMAINING-AMT.          EL334
00886      MOVE CR-LFRFND              TO CM-LF-ITD-CANCEL-AMT.         EL334
00887      MOVE CR-DTHAMT              TO CM-LF-ITD-DEATH-AMT.          EL334
00888      IF CR-LFPRM-RATE NUMERIC                                     EL334
00889          MOVE CR-LFPRM-RATE      TO CM-LF-PREMIUM-RATE            EL334
00890      ELSE                                                         EL334
00891          MOVE ZEROS              TO CM-LF-PREMIUM-RATE.           EL334
00892      IF CR-LFPRM-RATE-ALT NUMERIC                                 EL334
00893          MOVE CR-LFPRM-RATE-ALT  TO CM-LF-ALT-PREMIUM-RATE        EL334
00894      ELSE                                                         EL334
00895          MOVE ZEROS              TO CM-LF-ALT-PREMIUM-RATE.       EL334
00896                                                                   EL334
00897 ******************************************************************EL334
00898 ****    START BUILDING A/H COVERAGE ON ON-LINE CERT            ***EL334
00899 ******************************************************************EL334
00900                                                                   EL334
00901      MOVE CR-AHTYP               TO CM-AH-BENEFIT-CD.             EL334
00902      MOVE CR-AH-TERM             TO CM-AH-ORIG-TERM.              EL334
00903      MOVE CR-AH-CRIT-PERIOD      TO CM-AH-CRITICAL-PERIOD.        EL334
00904      MOVE CR-AH-DEV-CODE         TO CM-AH-DEV-CODE.               EL334
00905      MOVE CR-AH-DEV-PCT          TO CM-AH-DEV-PCT.                EL334
00906      MOVE CR-AHAMT               TO CM-AH-BENEFIT-AMT.            EL334
00907      MOVE CR-AHPRM               TO CM-AH-PREMIUM-AMT.            EL334
00908      MOVE CR-AH-NSP-PRM          TO CM-AH-NSP-PREMIUM-AMT.        EL334
00909      MOVE CR-AHRFND              TO CM-AH-ITD-CANCEL-AMT.         EL334
00910      MOVE CR-DISAMT              TO CM-AH-ITD-AH-PMT.             EL334
00911      IF CR-AHPRM-RATE NUMERIC                                     EL334
00912          MOVE CR-AHPRM-RATE      TO CM-AH-PREMIUM-RATE            EL334
00913      ELSE                                                         EL334
00914          MOVE ZEROS              TO CM-AH-PREMIUM-RATE.           EL334
00915                                                                   EL334
00916      IF CR-AH-CURRENT-STATUS = '6'                                EL334
00917         MOVE CR-DISAMT           TO CM-AH-ITD-LUMP-PMT.           EL334
00918                                                                   EL334
00919      MOVE '3' TO DC-OPTION-CODE.                                  EL334
00920      MOVE CR-DIS-PTO-CC          TO  DC-ALPHA-CEN-N.              EL334
00921      MOVE CR-DIS-PTO-YR          TO  DC-YMD-YEAR.                 EL334
00922      MOVE CR-DIS-PTO-MO          TO  DC-YMD-MONTH.                EL334
00923      MOVE CR-DIS-PTO-DA          TO  DC-YMD-DAY.                  EL334
00924      PERFORM 8500-DATE-CONVERSION                                 EL334
00925                                                                   EL334
00926      IF DC-ERROR-CODE NOT = SPACES                                EL334
00927         ADD +1      TO ERR-COUNT                                  EL334
00928         MOVE LOW-VALUES         TO  CM-AH-PAID-THRU-DT            EL334
00929      ELSE                                                         EL334
00930         MOVE DC-BIN-DATE-1      TO CM-AH-PAID-THRU-DT.            EL334
00931                                                                   EL334
00932 ******************************************************************EL334
00933 ****    START BUILDING LOAN INFORMATION ON-LINE CERT           ***EL334
00934 ******************************************************************EL334
00935                                                                   EL334
00936      MOVE CR-LIVES               TO CM-LIVES.                     EL334
00937      MOVE CR-APR                 TO CM-LOAN-APR.                  EL334
00938      MOVE CR-PMT-FREQ            TO CM-PAY-FREQUENCY.             EL334
00939      MOVE CR-LOAN-TERM           TO CM-LOAN-TERM.                 EL334
00940      MOVE CR-RATING-CLASS        TO CM-RATE-CLASS.                EL334
00941      MOVE SPACES                 TO CM-BENEFICIARY.               EL334
00942      MOVE CR-POLICY-FORM-NO      TO CM-POLICY-FORM-NO.            EL334
00943      MOVE CR-PMT-EXTENSION-DAYS  TO CM-PMT-EXTENSION-DAYS.        EL334
00944      MOVE LOW-VALUES             TO CM-LAST-ADD-ON-DT.            EL334
00945      MOVE '1'                    TO CM-PREMIUM-TYPE.              EL334
00946      IF CR-IND-GRP = '1'                                          EL334
00947         MOVE 'I'                 TO CM-IND-GRP-TYPE               EL334
00948      ELSE                                                         EL334
00949          MOVE 'G'                TO CM-IND-GRP-TYPE.              EL334
00950                                                                   EL334
00951      IF CR-SKIP NOT NUMERIC                                       EL334
00952          MOVE ZEROS              TO CR-SKIP.                      EL334
00953                                                                   EL334
00954      IF CR-SKIP = 00                                              EL334
00955         MOVE SPACE               TO CM-SKIP-CODE.                 EL334
00956      IF CR-SKIP = 01                                              EL334
00957         MOVE '1'                 TO CM-SKIP-CODE.                 EL334
00958      IF CR-SKIP = 02                                              EL334
00959         MOVE '2'                 TO CM-SKIP-CODE.                 EL334
00960      IF CR-SKIP = 03                                              EL334
00961         MOVE '3'                 TO CM-SKIP-CODE.                 EL334
00962      IF CR-SKIP = 04                                              EL334
00963         MOVE '4'                 TO CM-SKIP-CODE.                 EL334
00964      IF CR-SKIP = 05                                              EL334
00965         MOVE '5'                 TO CM-SKIP-CODE.                 EL334
00966      IF CR-SKIP = 06                                              EL334
00967         MOVE '6'                 TO CM-SKIP-CODE.                 EL334
00968      IF CR-SKIP = 07                                              EL334
00969         MOVE '7'                 TO CM-SKIP-CODE.                 EL334
00970      IF CR-SKIP = 08                                              EL334
00971         MOVE '8'                 TO CM-SKIP-CODE.                 EL334
00972      IF CR-SKIP = 09                                              EL334
00973         MOVE '9'                 TO CM-SKIP-CODE.                 EL334
00974      IF CR-SKIP = 10                                              EL334
00975         MOVE 'A'                 TO CM-SKIP-CODE.                 EL334
00976      IF CR-SKIP = 11                                              EL334
00977         MOVE 'X'                 TO CM-SKIP-CODE.                 EL334
00978                                                                   EL334
00979                                                                   EL334
00980      MOVE SPACES                 TO CM-PAYMENT-MODE.              EL334
00981      MOVE SPACES                 TO CM-LOAN-NUMBER.               EL334
00982      MOVE +0                     TO CM-LOAN-BALANCE.              EL334
00983      MOVE CR-LOAN-OFFICER        TO CM-LOAN-OFFICER.              EL334
00984      MOVE CR-REIN-TABLE          TO CM-REIN-TABLE.                EL334
00985      MOVE CR-REIN-SPEC           TO CM-SPECIAL-REIN-CODE.         EL334
00986                                                                   EL334
00987      MOVE LOW-VALUES             TO CM-LF-LOAN-EXPIRE-DT          EL334
00988                                     CM-AH-LOAN-EXPIRE-DT.         EL334
00989                                                                   EL334
00990      IF CM-LF-BENEFIT-CD = ZEROS                                  EL334
00991         NEXT SENTENCE                                             EL334
00992      ELSE                                                         EL334
00993         MOVE '6' TO DC-OPTION-CODE                                EL334
00994         MOVE CM-CERT-EFF-DT      TO DC-BIN-DATE-1                 EL334
00995         MOVE CM-LF-ORIG-TERM     TO DC-ELAPSED-MONTHS             EL334
00996         PERFORM 8500-DATE-CONVERSION                              EL334
00997                                                                   EL334
00998         IF DC-ERROR-CODE NOT = SPACES                             EL334
00999            MOVE CM-CERT-EFF-DT     TO  CM-LF-LOAN-EXPIRE-DT       EL334
01000         ELSE                                                      EL334
01001            MOVE DC-BIN-DATE-2      TO CM-LF-LOAN-EXPIRE-DT.       EL334
01002                                                                   EL334
01003      IF CM-AH-BENEFIT-CD = ZEROS                                  EL334
01004         NEXT SENTENCE                                             EL334
01005      ELSE                                                         EL334
01006         MOVE '6' TO DC-OPTION-CODE                                EL334
01007         MOVE CM-CERT-EFF-DT      TO DC-BIN-DATE-1                 EL334
01008         MOVE CM-AH-ORIG-TERM     TO DC-ELAPSED-MONTHS             EL334
01009         PERFORM 8500-DATE-CONVERSION                              EL334
01010                                                                   EL334
01011         IF DC-ERROR-CODE NOT = SPACES                             EL334
01012            MOVE CM-CERT-EFF-DT     TO  CM-AH-LOAN-EXPIRE-DT       EL334
01013         ELSE                                                      EL334
01014            MOVE DC-BIN-DATE-2      TO CM-AH-LOAN-EXPIRE-DT.       EL334
01015                                                                   EL334
01016      MOVE '3' TO DC-OPTION-CODE.                                  EL334
01017      MOVE CR-LOAN-1ST-PMT-DT     TO  DC-GREG-DATE-1-YMD.          EL334
01018      PERFORM 8500-DATE-CONVERSION.                                EL334
01019                                                                   EL334
01020      IF DC-ERROR-CODE NOT = SPACES                                EL334
01021          MOVE LOW-VALUES         TO  CM-LOAN-1ST-PMT-DT           EL334
01022        ELSE                                                       EL334
01023          MOVE DC-BIN-DATE-1      TO CM-LOAN-1ST-PMT-DT.           EL334
01024                                                                   EL334
01025      MOVE CLAS-STARTA            TO CLAS-INDEXA.                  EL334
01026      MOVE CLAS-STARTL            TO CLAS-INDEXL.                  EL334
01027                                                                   EL334
01028  2830-LF-OB-ADJUST.                                               EL334
01029      IF CM-LF-BENEFIT-CD = ZERO                                   EL334
01030         GO TO 2830-AH-OB-ADJUST.                                  EL334
01031                                                                   EL334
01032      IF CLAS-INDEXL GREATER CLAS-MAXL                             EL334
01033          GO TO 2830-AH-OB-ADJUST.                                 EL334
01034                                                                   EL334
01035      IF CM-LF-BENEFIT-CD = CLAS-I-BEN (CLAS-INDEXL)               EL334
01036          IF CLAS-I-BAL (CLAS-INDEXL) = 'B'                        EL334
01037             MOVE '2' TO CM-PREMIUM-TYPE                           EL334
01038                         CL-CLAIM-PREM-TYPE                        EL334
01039             GO TO 2830-AH-OB-ADJUST.                              EL334
01040                                                                   EL334
01041      ADD +1 TO CLAS-INDEXL.                                       EL334
01042      GO TO 2830-LF-OB-ADJUST.                                     EL334
01043                                                                   EL334
01044  2830-AH-OB-ADJUST.                                               EL334
01045      IF CM-AH-BENEFIT-CD = ZERO                                   EL334
01046         GO TO 2830-BUILD-STATUS.                                  EL334
01047                                                                   EL334
01048      IF CLAS-INDEXA GREATER CLAS-MAXA                             EL334
01049          GO TO 2830-BUILD-STATUS.                                 EL334
01050                                                                   EL334
01051      IF CM-AH-BENEFIT-CD = CLAS-I-BEN (CLAS-INDEXA)               EL334
01052          IF CLAS-I-BAL (CLAS-INDEXA) = 'B'                        EL334
01053             MOVE '2' TO CM-PREMIUM-TYPE                           EL334
01054                         CL-CLAIM-PREM-TYPE                        EL334
01055             GO TO 2830-BUILD-STATUS.                              EL334
01056                                                                   EL334
01057      ADD +1 TO CLAS-INDEXA.                                       EL334
01058      GO TO 2830-AH-OB-ADJUST.                                     EL334
01059                                                                   EL334
01060  2830-BUILD-STATUS.                                               EL334
01061 ******************************************************************EL334
01062 ****    START BUILDING STATUS INFORMATION ON-LINE CERT         ***EL334
01063 ******************************************************************EL334
01064                                                                   EL334
01065      MOVE CR-ENTRY-STATUS        TO CM-ENTRY-STATUS.              EL334
01066                                                                   EL334
01067      MOVE '3' TO DC-OPTION-CODE.                                  EL334
01068      MOVE CR-ENTRY-CC            TO  DC-ALPHA-CEN-N.              EL334
01069      MOVE CR-ENTRY-YR            TO  DC-YMD-YEAR.                 EL334
01070      MOVE CR-ENTRY-MO            TO  DC-YMD-MONTH.                EL334
01071      MOVE CR-ENTRY-DA            TO  DC-YMD-DAY.                  EL334
01072      PERFORM 8500-DATE-CONVERSION                                 EL334
01073                                                                   EL334
01074      IF DC-ERROR-CODE = SPACES                                    EL334
01075         MOVE DC-BIN-DATE-1      TO  CM-ENTRY-DT                   EL334
01076      ELSE                                                         EL334
01077         MOVE '6' TO DC-OPTION-CODE                                EL334
01078         MOVE CM-CERT-EFF-DT      TO DC-BIN-DATE-1                 EL334
01079         MOVE +1                  TO DC-ELAPSED-MONTHS             EL334
01080         PERFORM 8500-DATE-CONVERSION                              EL334
01081                                                                   EL334
01082         IF DC-ERROR-CODE NOT = SPACES                             EL334
01083            MOVE CM-CERT-EFF-DT     TO  CM-ENTRY-DT                EL334
01084         ELSE                                                      EL334
01085            MOVE DC-BIN-DATE-2      TO CM-ENTRY-DT.                EL334
01086                                                                   EL334
01087      MOVE CR-LF-STATUS-AT-CANCEL TO CM-LF-STATUS-AT-CANCEL.       EL334
01088                                                                   EL334
01089      IF CR-LF-CANC-DT NOT = ZEROS                                 EL334
01090         MOVE '3'                 TO DC-OPTION-CODE                EL334
01091         MOVE CR-LF-CNC-CC        TO  DC-ALPHA-CEN-N               EL334
01092         MOVE CR-LF-CNC-YR        TO  DC-YMD-YEAR                  EL334
01093         MOVE CR-LF-CNC-MO        TO  DC-YMD-MONTH                 EL334
01094         MOVE CR-LF-CNC-DA        TO  DC-YMD-DAY                   EL334
01095         PERFORM 8500-DATE-CONVERSION                              EL334
01096         IF DC-ERROR-CODE = SPACES                                 EL334
01097            MOVE DC-BIN-DATE-1    TO  CM-LF-CANCEL-DT              EL334
01098         ELSE                                                      EL334
01099            MOVE LOW-VALUES       TO CM-LF-CANCEL-DT               EL334
01100      ELSE                                                         EL334
01101         MOVE LOW-VALUES          TO CM-LF-CANCEL-DT.              EL334
01102                                                                   EL334
01103      IF CR-LF-CANCEL-EXIT-DATE NOT = ZEROS                        EL334
01104         MOVE '3'                 TO DC-OPTION-CODE                EL334
01105         MOVE CR-LF-CEX-CC        TO  DC-ALPHA-CEN-N               EL334
01106         MOVE CR-LF-CEX-YR        TO  DC-YMD-YEAR                  EL334
01107         MOVE CR-LF-CEX-MO        TO  DC-YMD-MONTH                 EL334
01108         MOVE CR-LF-CEX-DA        TO  DC-YMD-DAY                   EL334
01109         PERFORM 8500-DATE-CONVERSION                              EL334
01110         IF DC-ERROR-CODE = SPACES                                 EL334
01111            MOVE DC-BIN-DATE-1    TO  CM-LF-CANCEL-EXIT-DT         EL334
01112         ELSE                                                      EL334
01113            MOVE LOW-VALUES       TO CM-LF-CANCEL-EXIT-DT          EL334
01114      ELSE                                                         EL334
01115         MOVE LOW-VALUES          TO CM-LF-CANCEL-EXIT-DT.         EL334
01116                                                                   EL334
01117      MOVE CR-LF-STATUS-AT-DEATH  TO CM-LF-STATUS-AT-DEATH.        EL334
01118                                                                   EL334
01119      IF CR-DTH-DT NOT = ZEROS                                     EL334
01120         MOVE '3'                 TO DC-OPTION-CODE                EL334
01121         MOVE CR-DTH-CC           TO  DC-ALPHA-CEN-N               EL334
01122         MOVE CR-DTH-YR           TO  DC-YMD-YEAR                  EL334
01123         MOVE CR-DTH-MO           TO  DC-YMD-MONTH                 EL334
01124         MOVE CR-DTH-DA           TO  DC-YMD-DAY                   EL334
01125         PERFORM 8500-DATE-CONVERSION                              EL334
01126         IF DC-ERROR-CODE = SPACES                                 EL334
01127            MOVE DC-BIN-DATE-1    TO  CM-LF-DEATH-DT               EL334
01128         ELSE                                                      EL334
01129            MOVE LOW-VALUES       TO CM-LF-DEATH-DT                EL334
01130      ELSE                                                         EL334
01131         MOVE LOW-VALUES          TO CM-LF-DEATH-DT.               EL334
01132                                                                   EL334
01133      IF CR-LF-CLAIM-EXIT-DATE NOT = ZEROS                         EL334
01134         MOVE '3'                 TO DC-OPTION-CODE                EL334
01135         MOVE CR-LF-DEX-CC        TO  DC-ALPHA-CEN-N               EL334
01136         MOVE CR-LF-DEX-YR        TO  DC-YMD-YEAR                  EL334
01137         MOVE CR-LF-DEX-MO        TO  DC-YMD-MONTH                 EL334
01138         MOVE CR-LF-DEX-DA        TO  DC-YMD-DAY                   EL334
01139         PERFORM 8500-DATE-CONVERSION                              EL334
01140         IF DC-ERROR-CODE = SPACES                                 EL334
01141            MOVE DC-BIN-DATE-1    TO  CM-LF-DEATH-EXIT-DT          EL334
01142         ELSE                                                      EL334
01143            MOVE LOW-VALUES       TO CM-LF-DEATH-EXIT-DT           EL334
01144      ELSE                                                         EL334
01145         MOVE LOW-VALUES          TO CM-LF-DEATH-EXIT-DT.          EL334
01146                                                                   EL334
01147      MOVE CR-LF-CURRENT-STATUS   TO CM-LF-CURRENT-STATUS.         EL334
01148                                                                   EL334
01149      MOVE CR-AH-STATUS-AT-CANCEL TO CM-AH-STATUS-AT-CANCEL.       EL334
01150                                                                   EL334
01151      IF CR-AH-CANC-DT NOT = ZEROS                                 EL334
01152         MOVE '3'                 TO DC-OPTION-CODE                EL334
01153         MOVE CR-AH-CNC-CC        TO  DC-ALPHA-CEN-N               EL334
01154         MOVE CR-AH-CNC-YR        TO  DC-YMD-YEAR                  EL334
01155         MOVE CR-AH-CNC-MO        TO  DC-YMD-MONTH                 EL334
01156         MOVE CR-AH-CNC-DA        TO  DC-YMD-DAY                   EL334
01157         PERFORM 8500-DATE-CONVERSION                              EL334
01158         IF DC-ERROR-CODE = SPACES                                 EL334
01159            MOVE DC-BIN-DATE-1    TO  CM-AH-CANCEL-DT              EL334
01160         ELSE                                                      EL334
01161            MOVE LOW-VALUES       TO CM-AH-CANCEL-DT               EL334
01162      ELSE                                                         EL334
01163         MOVE LOW-VALUES          TO CM-AH-CANCEL-DT.              EL334
01164                                                                   EL334
01165      IF CR-AH-CANCEL-EXIT-DATE NOT = ZEROS                        EL334
01166         MOVE '3'                 TO DC-OPTION-CODE                EL334
01167         MOVE CR-AH-CEX-CC        TO  DC-ALPHA-CEN-N               EL334
01168         MOVE CR-AH-CEX-YR        TO  DC-YMD-YEAR                  EL334
01169         MOVE CR-AH-CEX-MO        TO  DC-YMD-MONTH                 EL334
01170         MOVE CR-AH-CEX-DA        TO  DC-YMD-DAY                   EL334
01171         PERFORM 8500-DATE-CONVERSION                              EL334
01172         IF DC-ERROR-CODE = SPACES                                 EL334
01173            MOVE DC-BIN-DATE-1    TO  CM-AH-CANCEL-EXIT-DT         EL334
01174         ELSE                                                      EL334
01175            MOVE LOW-VALUES       TO CM-AH-CANCEL-EXIT-DT          EL334
01176      ELSE                                                         EL334
01177         MOVE LOW-VALUES          TO CM-AH-CANCEL-EXIT-DT.         EL334
01178                                                                   EL334
01179      MOVE CR-AH-STATUS-AT-SETTLEMENT TO                           EL334
01180                     CM-AH-STATUS-AT-SETTLEMENT.                   EL334
01181                                                                   EL334
01182      IF CR-AH-SETTLEMENT-EXIT-DATE NOT = ZEROS                    EL334
01183         MOVE '3'                 TO DC-OPTION-CODE                EL334
01184         MOVE CR-DIS-YR           TO  DC-YMD-YEAR                  EL334
01185         MOVE CR-DIS-MO           TO  DC-YMD-MONTH                 EL334
01186         MOVE CR-DIS-DA           TO  DC-YMD-DAY                   EL334
01187         MOVE CR-DIS-CC           TO  DC-ALPHA-CEN-N               EL334
01188         PERFORM 8500-DATE-CONVERSION                              EL334
01189         IF DC-ERROR-CODE = SPACES                                 EL334
01190            MOVE DC-BIN-DATE-1    TO  CM-AH-SETTLEMENT-DT          EL334
01191         ELSE                                                      EL334
01192            MOVE LOW-VALUES       TO CM-AH-SETTLEMENT-DT           EL334
01193      ELSE                                                         EL334
01194         MOVE LOW-VALUES          TO CM-AH-SETTLEMENT-DT.          EL334
01195                                                                   EL334
01196      IF CR-AH-SETTLEMENT-EXIT-DATE NOT = ZEROS                    EL334
01197         MOVE '3'                 TO DC-OPTION-CODE                EL334
01198         MOVE CR-AH-DEX-CC        TO  DC-ALPHA-CEN-N               EL334
01199         MOVE CR-AH-DEX-YR        TO  DC-YMD-YEAR                  EL334
01200         MOVE CR-AH-DEX-MO        TO  DC-YMD-MONTH                 EL334
01201         MOVE CR-AH-DEX-DA        TO  DC-YMD-DAY                   EL334
01202         PERFORM 8500-DATE-CONVERSION                              EL334
01203         IF DC-ERROR-CODE = SPACES                                 EL334
01204            MOVE DC-BIN-DATE-1    TO  CM-AH-SETTLEMENT-EXIT-DT     EL334
01205         ELSE                                                      EL334
01206            MOVE LOW-VALUES       TO CM-AH-SETTLEMENT-EXIT-DT      EL334
01207      ELSE                                                         EL334
01208         MOVE LOW-VALUES          TO CM-AH-SETTLEMENT-EXIT-DT.     EL334
01209                                                                   EL334
01210      MOVE CR-AH-CURRENT-STATUS   TO CM-AH-CURRENT-STATUS.         EL334
01211                                                                   EL334
01212      MOVE SPACES                 TO CM-CLAIM-INTERFACE-SW.        EL334
01213                                                                   EL334
01214      MOVE WS-CURRENT-BIN-DT      TO CM-LAST-MONTH-END.            EL334
01215                                                                   EL334
01216 ******************************************************************EL334
01217 ****    START BUILDING COMM INFORMATION ON-LINE CERT           ***EL334
01218 ******************************************************************EL334
01219                                                                   EL334
01220      MOVE CR-LCOM-L (1)          TO CM-LIFE-COMM-PCT.             EL334
01221      MOVE CR-LCOM-AH (1)         TO CM-AH-COMM-PCT.               EL334
01222                                                                   EL334
01223  2830-WRITE-CERT.                                                 EL334
01224      IF CM-CONTROL-PRIMARY NOT GREATER THAN WS-LAST-CERT-KEY      EL334
01225          ADD +1  TO  WS-DUP-CERT-COUNT                            EL334
01226          GO TO 2800-EXIT.                                         EL334
01227                                                                   EL334
01228      MOVE CM-CONTROL-PRIMARY     TO  WS-LAST-CERT-KEY.            EL334
01229                                                                   EL334
01230      WRITE CERTIFICATE-MASTER.                                    EL334
01231                                                                   EL334
01232  2800-EXIT.                                                       EL334
01233       EXIT.                                                       EL334
01234                                                                   EL334
01235      EJECT                                                        EL334
01236  6000-BUILD-ZERO-TRAILER SECTION.                                 EL334
01237                                                                   EL334
01238      MOVE SPACES                 TO ACTIVITY-TRAILERS.            EL334
01239                                                                   EL334
01240      MOVE 'AT'                   TO AT-RECORD-ID.                 EL334
01241                                                                   EL334
01242      MOVE CL-CONTROL-PRIMARY     TO AT-CONTROL-PRIMARY.           EL334
01243      MOVE ZERO                   TO AT-SEQUENCE-NO.               EL334
01244                                                                   EL334
01245      MOVE '1'                    TO AT-TRAILER-TYPE.              EL334
01246                                                                   EL334
01247      IF CL-FILE-ESTABLISH-DT NOT = LOW-VALUES                     EL334
01248          MOVE CL-FILE-ESTABLISH-DT TO AT-RECORDED-DT              EL334
01249      ELSE                                                         EL334
01250          MOVE WS-CURRENT-BIN-DT    TO AT-RECORDED-DT.             EL334
01251                                                                   EL334
01252      MOVE 'L334'                 TO AT-RECORDED-BY.               EL334
01253      IF DTE-CLIENT = 'CRI'                                        EL334
01254          MOVE CL-PROCESSOR-ID    TO AT-RECORDED-BY.               EL334
01255      MOVE ZERO                   TO AT-LAST-MAINT-HHMMSS.         EL334
01256                                                                   EL334
01257      MOVE '1'                    TO AT-MANUAL-SW                  EL334
01258                                     AT-FUTURE-SW                  EL334
01259                                     AT-PTC-SW                     EL334
01260                                     AT-IBNR-SW                    EL334
01261                                     AT-PTC-LF-SW                  EL334
01262                                     AT-CDT-ACCESS-METHOD.         EL334
01263                                                                   EL334
01264      MOVE ZEROS                  TO AT-PERCENT-OF-CDT             EL334
01265                                     AT-FUTURE-RESERVE             EL334
01266                                     AT-PAY-CURRENT-RESERVE        EL334
01267                                     AT-IBNR-RESERVE.              EL334
01268                                                                   EL334
01269      MOVE LOW-VALUE              TO  AT-LAST-COMPUTED-DT.         EL334
01270                                                                   EL334
01271      MOVE ZEROS                  TO  AT-CURRENT-MANUAL-RESERVE    EL334
01272                                      AT-INITIAL-MANUAL-RESERVE    EL334
01273                                      AT-ITD-ADDITIONAL-RESERVE    EL334
01274                                                                   EL334
01275      MOVE '1'                    TO  AT-EXPENSE-METHOD.           EL334
01276                                                                   EL334
01277      MOVE ZEROS                  TO  AT-EXPENSE-PERCENT           EL334
01278                                      AT-EXPENSE-DOLLAR            EL334
01279                                      AT-ITD-PAID-EXPENSES         EL334
01280                                      AT-ITD-CHARGEABLE-EXPENSE    EL334
01281                                      AT-ITD-LIFE-REFUNDS          EL334
01282                                      AT-ITD-AH-REFUNDS.           EL334
01283                                                                   EL334
01284      MOVE LOW-VALUES             TO AT-OPEN-CLOSE-DATE (1)        EL334
01285                                     AT-OPEN-CLOSE-DATE (2)        EL334
01286                                     AT-OPEN-CLOSE-DATE (3)        EL334
01287                                     AT-OPEN-CLOSE-DATE (4)        EL334
01288                                     AT-OPEN-CLOSE-DATE (5)        EL334
01289                                     AT-OPEN-CLOSE-DATE (6).       EL334
01290                                                                   EL334
01291      MOVE CL-FILE-ESTABLISH-DT   TO AT-OPEN-CLOSE-DATE (1).       EL334
01292      MOVE 'O'                    TO AT-OPEN-CLOSE-TYPE (1).       EL334
01293      MOVE 'NEW'                  TO AT-OPEN-CLOSE-REASON (1).     EL334
01294                                                                   EL334
01295      IF CLAIM-IS-CLOSED                                           EL334
01296          MOVE CL-LAST-CLOSE-DT   TO AT-OPEN-CLOSE-DATE (2)        EL334
01297          MOVE 'C'                TO AT-OPEN-CLOSE-TYPE (2)        EL334
01298          MOVE 'FINAL'            TO AT-OPEN-CLOSE-REASON (2)      EL334
01299      ELSE                                                         EL334
01300        IF DTE-CLIENT NOT = 'CRI'                                  EL334
01301          IF CL-LAST-MAINT-DT  LESS THAN  WS-CLOSE-DT  AND         EL334
01302             CL-LAST-PMT-DT    LESS THAN  WS-CLOSE-DT  AND         EL334
01303             CL-PAID-THRU-DT   LESS THAN  WS-CLOSE-DT              EL334
01304              ADD +1 TO WS-CLOSE-COUNT                             EL334
01305              MOVE '3'                TO CL-LAST-CLOSE-REASON      EL334
01306              MOVE WS-CURRENT-BIN-DT  TO CL-LAST-CLOSE-DT          EL334
01307                                         AT-OPEN-CLOSE-DATE (2)    EL334
01308              MOVE 'C'                TO CL-CLAIM-STATUS           EL334
01309                                         AT-OPEN-CLOSE-TYPE (2)    EL334
01310              MOVE 'AUTO '            TO AT-OPEN-CLOSE-REASON (2). EL334
01311                                                                   EL334
01312      PERFORM 9300-WRITE-TRAILER THRU 9399-EXIT.                   EL334
01313                                                                   EL334
01314  6099-EXIT.                                                       EL334
01315      EXIT.                                                        EL334
01316                                                                   EL334
01317      EJECT                                                        EL334
01318  7000-PRINT-DETAIL SECTION.                                       EL334
01319                                                                   EL334
01320      IF LINE-COUNT GREATER +60                                    EL334
01321          PERFORM 7200-PRINT-HEADINGS THRU 7299-EXIT.              EL334
01322                                                                   EL334
01323      MOVE SPACES                 TO DETAIL-LINE.                  EL334
01324      MOVE CL-CLAIM-NO            TO DL-CLM-NO.                    EL334
01325                                                                   EL334
01326      IF CL-CLAIM-TYPE = AH-OVERRIDE-L1                            EL334
01327          MOVE AH-OVERRIDE-L2     TO DL-CLM-TYPE                   EL334
01328        ELSE                                                       EL334
01329          MOVE LIFE-OVERRIDE-L2   TO DL-CLM-TYPE.                  EL334
01330                                                                   EL334
01331      MOVE CL-CLAIM-STATUS        TO DL-CLM-STAT.                  EL334
01332                                                                   EL334
01333      IF CL-CLAIM-PREM-TYPE = '1'                                  EL334
01334          MOVE 'SNG PRM'          TO DL-CLM-KIND                   EL334
01335      ELSE                                                         EL334
01336          IF CL-CLAIM-PREM-TYPE = '2'                              EL334
01337              MOVE ' O.B.'        TO DL-CLM-KIND                   EL334
01338          ELSE                                                     EL334
01339              MOVE 'OPN END'      TO DL-CLM-KIND.                  EL334
01340                                                                   EL334
01341      IF CL-CLAIM-TYPE = AH-OVERRIDE-L1                            EL334
01342          MOVE CR-DIS-MO              TO DL-CLM-IMM                EL334
01343          MOVE CR-DIS-DA              TO DL-CLM-IDD                EL334
01344          MOVE CR-DIS-YR              TO DL-CLM-IYY                EL334
01345       ELSE                                                        EL334
01346          MOVE CR-DTH-MO              TO DL-CLM-IMM                EL334
01347          MOVE CR-DTH-DA              TO DL-CLM-IDD                EL334
01348          MOVE CR-DTH-YR              TO DL-CLM-IYY.               EL334
01349                                                                   EL334
01350      MOVE '-'                    TO DL-CLM-DASH1                  EL334
01351                                     DL-CLM-DASH2.                 EL334
01352      MOVE CL-CERT-NO             TO DL-CERT-NO.                   EL334
01353      MOVE CR-MO                  TO DL-CERT-EMM.                  EL334
01354      MOVE CR-DA                  TO DL-CERT-EDD.                  EL334
01355      MOVE CR-YR                  TO DL-CERT-EYY.                  EL334
01356      MOVE '-'                    TO DL-CERT-DASH1  DL-CERT-DASH2. EL334
01357      MOVE CL-CARRIER             TO DL-CERT-CARR.                 EL334
01358      MOVE CL-CERT-GROUPING       TO DL-CERT-GRP.                  EL334
01359      MOVE CL-CERT-STATE          TO DL-CERT-ST.                   EL334
01360      MOVE CL-CERT-ACCOUNT        TO DL-CERT-ACCOUNT.              EL334
01361      MOVE PMT-COUNT              TO DL-TRLR-PMTS.                 EL334
01362      MOVE ERR-COUNT              TO DL-ERR-COUNT.                 EL334
01363                                                                   EL334
01364      MOVE DETAIL-LINE            TO P-DATA.                       EL334
01365      MOVE SPACES                 TO X.                            EL334
01366      PERFORM 7300-PRINT-RTN THRU 7399-EXIT.                       EL334
01367                                                                   EL334
01368      ADD +1 TO LINE-COUNT.                                        EL334
01369                                                                   EL334
01370  7099-EXIT.                                                       EL334
01371      EXIT.                                                        EL334
01372                                                                   EL334
01373      EJECT                                                        EL334
01374  7200-PRINT-HEADINGS SECTION.                                     EL334
01375                                                                   EL334
01376      ADD +1 TO PAGE-COUNT.                                        EL334
01377      MOVE PAGE-COUNT TO HD-PAGE.                                  EL334
01378                                                                   EL334
01379      MOVE +7   TO LINE-COUNT.                                     EL334
01380                                                                   EL334
01381      MOVE '1' TO X.                                               EL334
01382      MOVE HEADING-1 TO P-DATA.                                    EL334
01383      PERFORM 7300-PRINT-RTN THRU 7399-EXIT.                       EL334
01384                                                                   EL334
01385      MOVE SPACES                 TO X.                            EL334
01386      MOVE HEADING-2 TO P-DATA.                                    EL334
01387      PERFORM 7300-PRINT-RTN THRU 7399-EXIT.                       EL334
01388                                                                   EL334
01389      MOVE HEADING-3 TO P-DATA.                                    EL334
01390      PERFORM 7300-PRINT-RTN THRU 7399-EXIT.                       EL334
01391                                                                   EL334
01392      MOVE ZERO                   TO X.                            EL334
01393      MOVE HEADING-4 TO P-DATA.                                    EL334
01394      PERFORM 7300-PRINT-RTN THRU 7399-EXIT.                       EL334
01395                                                                   EL334
01396      MOVE SPACES                 TO X.                            EL334
01397      MOVE HEADING-5 TO P-DATA.                                    EL334
01398      PERFORM 7300-PRINT-RTN THRU 7399-EXIT.                       EL334
01399                                                                   EL334
01400      MOVE SPACES TO P-DATA.                                       EL334
01401      PERFORM 7300-PRINT-RTN THRU 7399-EXIT.                       EL334
01402                                                                   EL334
01403  7299-EXIT.                                                       EL334
01404      EXIT.                                                        EL334
01405                                                                   EL334
01406  7300-PRINT-RTN SECTION.         COPY ELCPRT2X.                   EL334
01407                                                                   EL334
01408  7399-EXIT.                                                       EL334
01409      EXIT.                                                        EL334
01410                                                                   EL334
01411  8500-DATE-CONVERSION SECTION.   COPY ELCDCS.                     EL334
01412                                                                   EL334
01413  9050-READ-CERT SECTION.                                          EL334
01414                                                                   EL334
01415      MOVE ZERO                   TO  WS-CLAIM-ATTACHED-COUNT.     EL334
01416                                                                   EL334
01417      IF END-OF-CERTS                                              EL334
01418          GO TO 9050-EXIT.                                         EL334
01419                                                                   EL334
01420      READ CLAS-CERT-IN                                            EL334
01421          AT END                                                   EL334
01422              MOVE 'Y'            TO END-OF-CERTS-SW               EL334
01423              MOVE HIGH-VALUES    TO CR-FULL-CONTROL               EL334
01424              GO TO 9050-EXIT.                                     EL334
01425                                                                   EL334
01426      COPY ELCCRTM1.                                                  CL**4
01427                                                                      CL**4
01428  9050-EXIT.                                                       EL334
01429      EXIT.                                                        EL334
01430                                                                   EL334
01431  9200-WRITE-ELMSTR SECTION.                                       EL334
01432      WRITE CLAIM-MASTER                                           EL334
01433          INVALID KEY                                              EL334
01434              MOVE 'ELMSTR OUT OF EXTENTS'  TO  WS-ABEND-MESSAGE   EL334
01435              PERFORM ABEND-PGM.                                   EL334
01436                                                                   EL334
01437      ADD +1  TO  WS-CLAIMS-OUTPUT.                                EL334
01438                                                                   EL334
01439  9299-EXIT.                                                       EL334
01440      EXIT.                                                        EL334
01441                                                                   EL334
01442  9300-WRITE-TRAILER SECTION.                                      EL334
01443                                                                   EL334
01444      WRITE ACTIVITY-TRAILERS                                      EL334
01445          INVALID KEY                                              EL334
01446              MOVE 'ELMSTR OUT OF EXTENTS'  TO  WS-ABEND-MESSAGE   EL334
01447              PERFORM ABEND-PGM.                                   EL334
01448                                                                   EL334
01449      ADD +1  TO  WS-TRLR-COUNT                                    EL334
01450                  WS-TRAILERS-OUTPUT.                              EL334
01451                                                                   EL334
01452  9399-EXIT.                                                       EL334
01453      EXIT.                                                        EL334
01454                                                                   EL334
01455      EJECT                                                        EL334
01456  9400-DROP-DUP-CLAIMS SECTION.                                    EL334
01457                                                                   EL334
01458      OPEN OUTPUT CLAIM-OUTPUT-FILE.                               EL334
01459                                                                   EL334
01460      MOVE ZERO                   TO  WS-CLAIMS-OUTPUT.            EL334
01461                                                                   EL334
01462  9410-DROP-DUP-CLAIMS.                                            EL334
01463      RETURN CLAIM-SORT-WORK-FILE INTO CLAIM-OUTPUT-RECORD         EL334
01464          AT END                                                   EL334
01465              GO TO 9490-DROP-DUP-CLAIMS.                          EL334
01466                                                                   EL334
01467      ADD +1  TO  WS-CLAIMS-INPUT.                                 EL334
01468                                                                   EL334
01469      IF SWR-CLAIM-KEY NOT GREATER THAN WS-LAST-CLAIM-KEY          EL334
01470          ADD +1  TO  WS-DUP-CLAIM-COUNT                           EL334
01471          GO TO 9410-DROP-DUP-CLAIMS.                              EL334
01472                                                                   EL334
01473      MOVE SWR-CLAIM-KEY      TO  WS-LAST-CLAIM-KEY.               EL334
01474                                                                   EL334
01475      ADD +1  TO  WS-CLAIMS-OUTPUT.                                EL334
01476                                                                   EL334
01477      WRITE CLAIM-OUTPUT-RECORD.                                   EL334
01478                                                                   EL334
01479      GO TO 9410-DROP-DUP-CLAIMS.                                  EL334
01480                                                                   EL334
01481  9490-DROP-DUP-CLAIMS.                                            EL334
01482      CLOSE CLAIM-OUTPUT-FILE.                                     EL334
01483                                                                   EL334
01484      DISPLAY '     '.                                             EL334
01485      DISPLAY '     '.                                             EL334
01486                                                                   EL334
01487      MOVE WS-CLAIMS-INPUT        TO  WS-DISPLAY-COUNT.            EL334
01488      DISPLAY 'CLAIMS INPUT     '  WS-DISPLAY-COUNT.               EL334
01489      DISPLAY 'CLAIMS INPUT     '  WS-DISPLAY-COUNT                EL334
01490                                  UPON CONSOLE.                    EL334
01491                                                                   EL334
01492      MOVE WS-DUP-CLAIM-COUNT     TO  WS-DISPLAY-COUNT.            EL334
01493      DISPLAY 'DUP CLAIMS DROPED'  WS-DISPLAY-COUNT.               EL334
01494      DISPLAY 'DUP CLAIMS DROPED'  WS-DISPLAY-COUNT                EL334
01495                                  UPON CONSOLE.                    EL334
01496                                                                   EL334
01497      MOVE WS-CLAIMS-OUTPUT       TO  WS-DISPLAY-COUNT.            EL334
01498      DISPLAY 'CLAIMS OUTPUT    '  WS-DISPLAY-COUNT.               EL334
01499      DISPLAY 'CLAIMS OUTPUT    '  WS-DISPLAY-COUNT                EL334
01500                                  UPON CONSOLE.                    EL334
01501                                                                   EL334
01502  9499-EXIT.                                                       EL334
01503      EXIT.                                                        EL334
01504                                                                   EL334
01505      EJECT                                                        EL334
01506  9500-DROP-DUP-TRAILERS SECTION.                                  EL334
01507                                                                   EL334
01508      OPEN OUTPUT TRAILER-OUTPUT-FILE.                             EL334
01509                                                                   EL334
01510      MOVE ZERO                   TO  WS-TRAILERS-OUTPUT.          EL334
01511                                                                   EL334
01512  9510-DROP-DUP-TRAILERS.                                          EL334
01513      RETURN TRAILER-SORT-WORK-FILE INTO TRAILER-OUTPUT-RECORD     EL334
01514          AT END                                                   EL334
01515              GO TO 9590-DROP-DUP-TRAILERS.                        EL334
01516                                                                   EL334
01517      ADD +1  TO  WS-TRAILERS-INPUT.                               EL334
01518                                                                   EL334
01519      IF SWR-TRAILER-KEY NOT GREATER THAN WS-LAST-TRAILER-KEY      EL334
01520          ADD +1  TO  WS-DUP-TRAILER-COUNT                         EL334
01521          GO TO 9510-DROP-DUP-TRAILERS.                            EL334
01522                                                                   EL334
01523      MOVE SWR-TRAILER-KEY  TO  WS-LAST-TRAILER-KEY.               EL334
01524                                                                   EL334
01525      ADD +1  TO  WS-TRAILERS-OUTPUT.                              EL334
01526                                                                   EL334
01527      WRITE TRAILER-OUTPUT-RECORD.                                 EL334
01528                                                                   EL334
01529      GO TO 9510-DROP-DUP-TRAILERS.                                EL334
01530                                                                   EL334
01531  9590-DROP-DUP-TRAILERS.                                          EL334
01532      CLOSE TRAILER-OUTPUT-FILE.                                   EL334
01533                                                                   EL334
01534      DISPLAY '     '.                                             EL334
01535      DISPLAY '     '.                                             EL334
01536                                                                   EL334
01537      MOVE WS-TRAILERS-INPUT        TO  WS-DISPLAY-COUNT.          EL334
01538      DISPLAY 'TRAILERS INPUT     '  WS-DISPLAY-COUNT.             EL334
01539      DISPLAY 'TRAILERS INPUT     '  WS-DISPLAY-COUNT              EL334
01540                                  UPON CONSOLE.                    EL334
01541                                                                   EL334
01542      MOVE WS-DUP-TRAILER-COUNT     TO  WS-DISPLAY-COUNT.          EL334
01543      DISPLAY 'DUP TRAILERS DROPED'  WS-DISPLAY-COUNT.             EL334
01544      DISPLAY 'DUP TRAILERS DROPED'  WS-DISPLAY-COUNT              EL334
01545                                  UPON CONSOLE.                    EL334
01546                                                                   EL334
01547      MOVE WS-TRAILERS-OUTPUT       TO  WS-DISPLAY-COUNT.          EL334
01548      DISPLAY 'TRAILERS OUTPUT    '  WS-DISPLAY-COUNT.             EL334
01549      DISPLAY 'TRAILERS OUTPUT    '  WS-DISPLAY-COUNT              EL334
01550                                  UPON CONSOLE.                    EL334
01551                                                                   EL334
01552  9599-EXIT.                                                       EL334
01553      EXIT.                                                        EL334
01554                                                                   EL334
01555      EJECT                                                        EL334
01556  9700-OPEN-FILES SECTION.                                         EL334
01557                                                                   EL334
01558      MOVE COMPANY-NAME    TO HD-CLIENT.                           EL334
01559      MOVE WS-CURRENT-DATE TO HD-RUN.                              EL334
01560      MOVE ALPH-DATE       TO HD-DATE.                             EL334
01561                                                                   EL334
01562      OPEN INPUT CLAS-CERT-IN                                      EL334
01563           OUTPUT ELMSTR                                           EL334
01564                  ELTRLR                                           EL334
01565                  CERT-OUTPUT-FILE                                 EL334
01566                  PRNTR.                                           EL334
01567                                                                   EL334
01568      MOVE '2'                    TO  DC-OPTION-CODE.              EL334
01569      MOVE WS-CURRENT-DATE        TO  DC-GREG-DATE-1-EDIT.         EL334
01570      PERFORM 8500-DATE-CONVERSION.                                EL334
01571      MOVE DC-BIN-DATE-1          TO  WS-CURRENT-BIN-DT.           EL334
01572                                                                   EL334
01573      MOVE '6'                    TO  DC-OPTION-CODE.              EL334
01574      MOVE WS-CURRENT-BIN-DT      TO  DC-BIN-DATE-1.               EL334
01575      MOVE -60                    TO  DC-ELAPSED-DAYS.             EL334
01576      MOVE ZEROS                  TO  DC-ELAPSED-MONTHS.           EL334
01577      PERFORM 8500-DATE-CONVERSION.                                EL334
01578      MOVE DC-BIN-DATE-2          TO  WS-CLOSE-DT.                 EL334
01579                                                                   EL334
01580  9730-EXIT.                                                       EL334
01581      EXIT.                                                        EL334
01582      EJECT                                                        EL334
01583  9800-CLOSE-FILES SECTION.                                        EL334
01584                                                                   EL334
01585      CLOSE CLAS-CERT-IN                                           EL334
01586            CERT-OUTPUT-FILE                                       EL334
01587            PRNTR                                                  EL334
01588            ELTRLR                                                 EL334
01589            ELMSTR.                                                EL334
01590                                                                   EL334
01591      DISPLAY '     '.                                             EL334
01592      DISPLAY '     '.                                             EL334
01593                                                                   EL334
01594      DISPLAY 'EOJ RECORD COUNTS'.                                 EL334
01595      DISPLAY 'EOJ RECORD COUNTS' UPON CONSOLE.                    EL334
01596                                                                   EL334
01597      MOVE WS-CLAIMS-IN           TO  WS-DISPLAY-COUNT.            EL334
01598      DISPLAY 'CLAIMS INPUT    ' WS-DISPLAY-COUNT ' ' WS-TIME.     EL334
01599      DISPLAY 'CLAIMS INPUT    ' WS-DISPLAY-COUNT ' ' WS-TIME      EL334
01600                                  UPON CONSOLE.                    EL334
01601                                                                   EL334
01602      MOVE WS-CLAIM-COUNT         TO  WS-DISPLAY-COUNT.            EL334
01603      DISPLAY 'CLAIMS ADDED    ' WS-DISPLAY-COUNT.                 EL334
01604      DISPLAY 'CLAIMS ADDED    ' WS-DISPLAY-COUNT                  EL334
01605                                  UPON CONSOLE.                    EL334
01606      DISPLAY '     '.                                             EL334
01607      MOVE WS-CLAIMS-OUTPUT       TO  WS-DISPLAY-COUNT.            EL334
01608      DISPLAY 'CLAIMS OUTPUT   ' WS-DISPLAY-COUNT.                 EL334
01609      DISPLAY 'CLAIMS OUTPUT   ' WS-DISPLAY-COUNT                  EL334
01610                                  UPON CONSOLE.                    EL334
01611      MOVE WS-FINAL-COUNT         TO  WS-DISPLAY-COUNT.            EL334
01612      DISPLAY '  CLOSED FINAL  ' WS-DISPLAY-COUNT.                 EL334
01613      MOVE WS-CLOSE-COUNT         TO  WS-DISPLAY-COUNT.            EL334
01614      DISPLAY '  AUTO CLOSED   ' WS-DISPLAY-COUNT.                 EL334
01615      DISPLAY '     '.                                             EL334
01616      DISPLAY 'DATE ERRORS  '.                                     EL334
01617      MOVE INC-ERR-CNT            TO  WS-DISPLAY-COUNT.            EL334
01618      DISPLAY '  INC DATE ERR  ' WS-DISPLAY-COUNT.                 EL334
01619      MOVE RPT-ERR-CNT            TO  WS-DISPLAY-COUNT.            EL334
01620      DISPLAY '  RPT DATE ERR  ' WS-DISPLAY-COUNT.                 EL334
01621      MOVE EFF-ERR-CNT            TO  WS-DISPLAY-COUNT.            EL334
01622      DISPLAY '  EFF DATE ERR  ' WS-DISPLAY-COUNT.                 EL334
01623      MOVE PAY-ERR-CNT            TO  WS-DISPLAY-COUNT.            EL334
01624      DISPLAY '  PAY DATE ERR  ' WS-DISPLAY-COUNT.                 EL334
01625      MOVE PTO-ERR-CNT            TO  WS-DISPLAY-COUNT.            EL334
01626      DISPLAY '  PTO DATE ERR  ' WS-DISPLAY-COUNT.                 EL334
01627      DISPLAY '     '.                                             EL334
01628      MOVE WS-TRLR-COUNT          TO  WS-DISPLAY-COUNT.            EL334
01629      DISPLAY 'TRAILERS ADDED  ' WS-DISPLAY-COUNT.                 EL334
01630      DISPLAY 'TRAILERS ADDED  ' WS-DISPLAY-COUNT                  EL334
01631                                  UPON CONSOLE.                    EL334
01632      MOVE WS-TRAILERS-OUTPUT     TO  WS-DISPLAY-COUNT.            EL334
01633      DISPLAY 'TRAILERS OUTPUT ' WS-DISPLAY-COUNT.                 EL334
01634      DISPLAY 'TRAILERS OUTPUT ' WS-DISPLAY-COUNT                  EL334
01635                                  UPON CONSOLE.                    EL334
01636                                                                   EL334
01637  9800-CLOSE-OTHER.               COPY ELCPRTCX.                   EL334
01638                                                                   EL334
01639  9800-EXIT.                                                       EL334
01640      EXIT.                                                        EL334
01641                                                                   EL334
01642  ABEND-PGM SECTION.              COPY ELCABEND SUPPRESS.          EL334
01643 /                                                                 EL334
01644  LCP-WRITE-POS-PRT SECTION.                                       EL334
01645      IF LCP-ASA = '+'                                             EL334
01646          WRITE PRT AFTER 0 LINE                                   EL334
01647      ELSE                                                         EL334
01648      IF LCP-ASA = ' '                                             EL334
01649          WRITE PRT AFTER ADVANCING 1 LINE                         EL334
01650      ELSE                                                         EL334
01651      IF LCP-ASA = '0'                                             EL334
01652          WRITE PRT AFTER ADVANCING 2 LINE                         EL334
01653      ELSE                                                         EL334
01654      IF LCP-ASA = '-'                                             EL334
01655          WRITE PRT AFTER ADVANCING 3 LINE                         EL334
01656      ELSE                                                         EL334
01657      IF LCP-ASA = '1'                                             EL334
01658          WRITE PRT AFTER ADVANCING PAGE                           EL334
01659      ELSE                                                         EL334
01660      IF LCP-ASA = '2'                                             EL334
01661          WRITE PRT AFTER ADVANCING LCP-CH2                        EL334
01662      ELSE                                                         EL334
01663      IF LCP-ASA = '3'                                             EL334
01664          WRITE PRT AFTER ADVANCING LCP-CH3                        EL334
01665      ELSE                                                         EL334
01666      IF LCP-ASA = '4'                                             EL334
01667          WRITE PRT AFTER ADVANCING LCP-CH4                        EL334
01668      ELSE                                                         EL334
01669      IF LCP-ASA = '5'                                             EL334
01670          WRITE PRT AFTER ADVANCING LCP-CH5                        EL334
01671      ELSE                                                         EL334
01672      IF LCP-ASA = '6'                                             EL334
01673          WRITE PRT AFTER ADVANCING LCP-CH6                        EL334
01674      ELSE                                                         EL334
01675      IF LCP-ASA = '7'                                             EL334
01676          WRITE PRT AFTER ADVANCING LCP-CH7                        EL334
01677      ELSE                                                         EL334
01678      IF LCP-ASA = '8'                                             EL334
01679          WRITE PRT AFTER ADVANCING LCP-CH8                        EL334
01680      ELSE                                                         EL334
01681      IF LCP-ASA = '9'                                             EL334
01682          WRITE PRT AFTER ADVANCING LCP-CH9                        EL334
01683      ELSE                                                         EL334
01684      IF LCP-ASA = 'A'                                             EL334
01685          WRITE PRT AFTER ADVANCING LCP-CH10                       EL334
01686      ELSE                                                         EL334
01687      IF LCP-ASA = 'B'                                             EL334
01688          WRITE PRT AFTER ADVANCING LCP-CH11                       EL334
01689      ELSE                                                         EL334
01690      IF LCP-ASA = 'C'                                             EL334
01691          WRITE PRT AFTER ADVANCING LCP-CH12                       EL334
01692      ELSE                                                         EL334
01693      IF LCP-ASA = 'V'                                             EL334
01694          WRITE PRT AFTER ADVANCING LCP-P01                        EL334
01695      ELSE                                                         EL334
01696      IF LCP-ASA = 'W'                                             EL334
01697          WRITE PRT AFTER ADVANCING LCP-P02                        EL334
01698      ELSE                                                         EL334
01699      DISPLAY 'ASA CODE ERROR'.                                    EL334
01700  LCP-WRITE-END-PRT.                                               EL334
01701      EXIT.                                                        EL334
01702                                                                   EL334
