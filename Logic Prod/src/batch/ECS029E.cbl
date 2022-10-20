00001  IDENTIFICATION DIVISION.                                         09/16/97
00002                                                                   ECS029
00003  PROGRAM-ID.                ECS029E.                                 LV001
00004 *              PROGRAM CONVERTED BY                               ECS029
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ECS029
00006 *              CONVERSION DATE 02/15/96 18:06:30.                 ECS029
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            ECS029
00008 *                            VMOD=2.011                           ECS029
00009 *AUTHOR.     LOGIC, INC.                                          ECS029
00010 *            DALLAS, TEXAS.                                       ECS029
00011                                                                   ECS029
00012 *DATE-COMPILED.                                                   ECS029
00013                                                                   ECS029
00014 *SECURITY.   *****************************************************ECS029
00015 *            *                                                   *ECS029
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ECS029
00017 *            *                                                   *ECS029
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS029
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ECS029
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *ECS029
00021 *            *                                                   *ECS029
00022 *            *****************************************************ECS029
00023                                                                   ECS029
00024 *REMARKS.                                                         ECS029
00025                                                                   ECS029
00026 *    PRINT CLAIM REGISTER OF REINSURED CLAIMS.                    ECS029
00027 *       DTE-PGM-PIC (029) MUST BE SET                             ECS029
00028 *            1 = INCEPTION TO DATE PAID CLAIMS                    ECS029
00029 *            2 = YTD PAID CLAIM REGISTER - PROCESS YR = RUN YR    ECS029
00030 *            3 = MTD PAID CLAIM REGISTER - PROCESS YRMO = RUN YRMOECS029
00031                                                                   ECS029
00032  ENVIRONMENT DIVISION.                                            ECS029
00033  INPUT-OUTPUT SECTION.                                            ECS029
00034  FILE-CONTROL.                                                    ECS029
00035                                                                   ECS029
00036      SELECT  EXTRACT     ASSIGN TO SYS018.
00037      SELECT  PRNTR       ASSIGN TO SYS008.
00038      SELECT  DISK-DATE   ASSIGN TO SYS019.
00039      SELECT  CLM-CARR    ASSIGN TO SYS015.
00040      SELECT  CLM-SRT     ASSIGN TO SORTWK1.
00041      SELECT  FICH        ASSIGN TO SYS020.
00042      SELECT  RTBL-FILE   ASSIGN TO ERRTBLT
00043                              ACCESS       SEQUENTIAL              ECS029
00044                              ORGANIZATION INDEXED                 ECS029
00045                              FILE STATUS  REIN-FILE-STATUS        ECS029
00046                              RECORD KEY   RE-CONTROL-PRIMARY.     ECS029
00047                                                                   ECS029
00048  EJECT                                                            ECS029
00049  DATA DIVISION.                                                   ECS029
00050  FILE SECTION.                                                    ECS029
00051                                                                   ECS029
00052  FD  EXTRACT                                                      ECS029
00053                              COPY ECSEXTFD.                       ECS029
00054                              COPY ECSEXT01.                       ECS029
00055                                                                   ECS029
00056      EJECT                                                        ECS029
00057  FD  DISK-DATE                                                    ECS029
00058                              COPY ELCDTEFD.                       ECS029
00059                                                                   ECS029
00060      EJECT                                                        ECS029
00061  FD  PRNTR                                                        ECS029
00062                              COPY ELCPRTFD.                       ECS029
00063  01  DTL.                                                         ECS029
00064      12  FILLER      PIC X.                                       ECS029
00065      12  P-ST        PIC XX.                                      ECS029
00066      12  FILLER      PIC XX.                                      ECS029
00067      12  P-ACCT      PIC X(10).                                   ECS029
00068      12  FILLER      PIC XX.                                      ECS029
00069      12  P-CERT      PIC X(11).                                   ECS029
00070      12  FILLER      PIC XX.                                      ECS029
00071      12  P-EMO       PIC 99.                                      ECS029
00072      12  P-EMOD      PIC X.                                       ECS029
00073      12  P-EDA       PIC 99.                                      ECS029
00074      12  P-EDAD      PIC X.                                       ECS029
00075      12  P-EYR       PIC 99.                                      ECS029
00076      12  FILLER      PIC XX.                                      ECS029
00077      12  P-NAME      PIC X(9).                                    ECS029
00078      12  FILLER      PIC X.                                       ECS029
00079      12  P-REI       PIC X(6).                                    ECS029
00080      12  FILLER      PIC X.                                       ECS029
00081      12  P-TYPE      PIC X(7).                                    ECS029
00082      12  P-IMO       PIC 99.                                      ECS029
00083      12  P-IMOD      PIC X.                                       ECS029
00084      12  P-IDA       PIC 99.                                      ECS029
00085      12  P-IDAD      PIC X.                                       ECS029
00086      12  P-IYR       PIC 99.                                      ECS029
00087      12  FILLER      PIC XXX.                                     ECS029
00088      12  P-PMO       PIC 99.                                      ECS029
00089      12  P-PMOD      PIC X.                                       ECS029
00090      12  P-PDA       PIC 99.                                      ECS029
00091      12  P-PDAD      PIC X.                                       ECS029
00092      12  P-PYR       PIC 99.                                      ECS029
00093      12  FILLER      PIC XX.                                      ECS029
00094      12  P-CLMNO     PIC X(7).                                    ECS029
00095      12  P-DTH       PIC Z,ZZZ,ZZZ.99-  BLANK WHEN ZERO.          ECS029
00096      12  P-DIS       PIC Z,ZZZ,ZZZ.99-  BLANK WHEN ZERO.          ECS029
00097      12  FILLER      PIC X(4).                                    ECS029
00098      12  P-AGE       PIC XX.                                      ECS029
00099      12  FILLER      PIC XX.                                      ECS029
00100      12  P-CHK-NO    PIC X(7).                                    ECS029
00101                                                                   ECS029
00102      EJECT                                                        ECS029
00103  FD  CLM-CARR                                                     ECS029
00104      BLOCK CONTAINS 0 RECORDS
00105      RECORDING MODE F.                                            ECS029
00106                                                                   ECS029
00107  01  CAR-REC.                                                     ECS029
00108      12  FILLER          PIC X(504).                              ECS029
00109      12  C-COPNY         PIC X(6).                                ECS029
00110                                                                   ECS029
00111  SD  CLM-SRT.                                                     ECS029
00112                                                                   ECS029
00113  01  SRT-REC.                                                     ECS029
00114      12  FILLER          PIC X(4).                                ECS029
00115      12  S-CAR           PIC X.                                   ECS029
00116      12  S-CO            PIC X(6).                                ECS029
00117      12  FILLER          PIC XX.                                  ECS029
00118      12  S-ACT           PIC X(10).                               ECS029
00119      12  FILLER          PIC X(348).                              ECS029
00120      12  S-CLM           PIC X(7).                                ECS029
00121      12  S-CHK           PIC X(7).                                ECS029
00122      12  FILLER          PIC X(119).                              ECS029
00123      12  S-COPNY         PIC X(6).                                ECS029
00124                                                                   ECS029
00125  FD  FICH                                                         ECS029
00126                              COPY ELCFCHFD.                       ECS029
00127      EJECT                                                        ECS029
00128  FD  RTBL-FILE.                                                   ECS029
00129                                                                   ECS029
00130                              COPY ERCREIN.                        ECS029
00131  EJECT                                                            ECS029
00132  WORKING-STORAGE SECTION.                                         ECS029
00133  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      ECS029
00134  01  LCP-CURRENT-DATE-68.                                         ECS029
00135      05  LCP-MONTH                 PIC X(2).                      ECS029
00136      05  FILLER                    PIC X VALUE '/'.               ECS029
00137      05  LCP-DAY1                  PIC X(2).                      ECS029
00138      05  FILLER                    PIC X VALUE '/'.               ECS029
00139      05  LCP-YEAR                  PIC X(2).                      ECS029
00140  01  LCP-DATE-NEW-74.                                             ECS029
00141      05  LCP-YEAR                  PIC X(2).                      ECS029
00142      05  LCP-MONTH                 PIC X(2).                      ECS029
00143      05  LCP-DAY1                  PIC X(2).                      ECS029
00144  77  FILLER  PIC X(32) VALUE '********************************'.  ECS029
00145  77  FILLER  PIC X(32) VALUE '    ECS029E WORKING STORAGE     '.  ECS029
00146  77  FILLER  PIC X(32) VALUE '*****VMOD=2.011 ****************'.  ECS029
00147                                                                   ECS029
00148  01  REIN-FILE-STATUS.                                            ECS029
00149      12  REIN-FILE-STATUS-1    PIC X.                             ECS029
00150      12  REIN-FILE-STATUS-2    PIC X.                             ECS029
00151                                                                   ECS029
00152  01  WS.                                                          ECS029
00153      12  WS-ABEND-FILE-STATUS  PIC X(4)            VALUE ZEROS.   ECS029
00154      12  WS-ABEND-MESSAGE      PIC X(80)           VALUE SPACES.  ECS029
00155      12  WS-RETURN-CODE        PIC S9(4)  COMP     VALUE ZEROS.   ECS029
00156      12  WS-ZERO               PIC S9              VALUE ZEROS.   ECS029
00157      12  PGM-SUB               PIC S999    COMP    VALUE +029.    ECS029
00158      12  WS-REIN-COMP-TBL.                                        ECS029
00159          16  WS-REIN-COMP-TBL-X OCCURS 1500 TIMES.                ECS029
00160              20  WS-RCT-COMP   PIC X(6).                          ECS029
00161              20  WS-RCT-NAME   PIC X(30).                         ECS029
00162              20  WS-RCT-CEDE   PIC X(30).                         ECS029
00163      12  WS-RCT-X              PIC S9(4)   COMP    VALUE +0.      ECS029
00164      12  WS-RCT-MAX            PIC S9(4)   COMP    VALUE +1500.   ECS029
00165      12  WORK-ABEND-CODE.                                         ECS029
00166          16  WAC-1             PIC X VALUE 'I'.                   ECS029
00167          16  WAC-2             PIC X VALUE 'I'.                   ECS029
00168          16  WAC-3-4.                                             ECS029
00169              20  WAC-3         PIC X VALUE 'I'.                   ECS029
00170              20  WAC-4         PIC X VALUE 'I'.                   ECS029
00171                                                                   ECS029
00172      EJECT                                                        ECS029
00173                              COPY ELCDTECX.                       ECS029
00174      EJECT                                                        ECS029
00175                              COPY ELCDTEVR.                       ECS029
00176      EJECT                                                        ECS029
00177                              COPY ELCEXTVR.                       ECS029
00178                                                                   ECS029
00179      EJECT                                                        ECS029
00180  01  HD1.                                                         ECS029
00181      12  FILLER                PIC X(44)      VALUE SPACES.       ECS029
00182      12  HD-KIND               PIC X(3)       VALUE 'ITD'.        ECS029
00183      12  FILLER                PIC X          VALUE SPACES.       ECS029
00184      12  FILLER                PIC X(24)      VALUE               ECS029
00185                                        'REINSURED CLAIMS   COMPA'.ECS029
00186      12  FILLER                PIC X(5)       VALUE 'NY - '.      ECS029
00187      12  HD-CARR               PIC X.                             ECS029
00188      12  HD-GROUP              PIC X(6).                          ECS029
00189      12  FILLER                PIC X(35)      VALUE SPACES.       ECS029
00190      12  FILLER                PIC X(8)       VALUE 'ECS029A '.   ECS029
00191                                                                   ECS029
00192  01  HD1-A.                                                       ECS029
00193      12  FILLER                PIC X(45)      VALUE SPACES.       ECS029
00194      12  FILLER                PIC X(29)      VALUE               ECS029
00195                                'CLAIMS FOR REINSURANCE CO  - '.   ECS029
00196      12  HD-1A-CARR            PIC X.                             ECS029
00197      12  HD-1A-GROUP           PIC X(6).                          ECS029
00198      12  FILLER                PIC X(38)      VALUE SPACES.       ECS029
00199      12  FILLER                PIC X(8)       VALUE 'ECS029B '.   ECS029
00200                                                                   ECS029
00201  01  HD2.                                                         ECS029
00202      12  HD2-TITLE             PIC X(13)      VALUE SPACES.       ECS029
00203      12  HD2-REIN-NM           PIC X(30)      VALUE SPACES.       ECS029
00204      12  FILLER                PIC X(4)       VALUE SPACES.       ECS029
00205      12  HD-CO                 PIC X(30).                         ECS029
00206      12  FILLER                PIC X(41)      VALUE SPACES.       ECS029
00207      12  HD-RD                 PIC X(8).                          ECS029
00208                                                                   ECS029
00209  01  HD3.                                                         ECS029
00210      12  HD3-TITLE             PIC X(13)      VALUE SPACES.       ECS029
00211      12  HD3-REIN-NM           PIC X(30)      VALUE SPACES.       ECS029
00212      12  FILLER                PIC X(10)      VALUE SPACES.       ECS029
00213      12  HD-DT                 PIC X(18).                         ECS029
00214      12  FILLER                PIC X(44)      VALUE SPACES.       ECS029
00215      12  FILLER                PIC X(5)       VALUE 'PAGE '.      ECS029
00216      12  HD-PG                 PIC ZZ,ZZ9.                        ECS029
00217                                                                   ECS029
00218  01  HD4.                                                         ECS029
00219      12  HD4-1       PIC X(25) VALUE 'STATE ACCOUNT      CERT'.   ECS029
00220      12  FILLER      PIC X(9)  VALUE '     EFFE'.                 ECS029
00221      12  FILLER      PIC X(24) VALUE  'CTIVE INSURED    REI  TY'. ECS029
00222      12  FILLER      PIC X(25) VALUE 'PE   INCURRED     DATE   '. ECS029
00223      12  FILLER      PIC X(25) VALUE '  CLAIM   ----AMOUNT OF C'. ECS029
00224      12  FILLER      PIC X(24) VALUE 'LAIM----     AGE  CHECK '.  ECS029
00225                                                                   ECS029
00226  01  HD4-A.                                                       ECS029
00227      12  HD4-2       PIC X(25) VALUE 'STATE ACCOUNT      CERT'.   ECS029
00228      12  FILLER      PIC X(9)  VALUE '     EFFE'.                 ECS029
00229      12  FILLER      PIC X(24) VALUE  'CTIVE     INSURED     TY'. ECS029
00230      12  FILLER      PIC X(25) VALUE 'PE   INCURRED     DATE   '. ECS029
00231      12  FILLER      PIC X(25) VALUE '  CHECK   ----AMOUNT OF C'. ECS029
00232      12  FILLER      PIC X(24) VALUE 'LAIM----     AGE  CLAIM '.  ECS029
00233                                                                   ECS029
00234  01  HD5.                                                         ECS029
00235      12  FILLER      PIC X(8)  VALUE SPACES.                      ECS029
00236      12  FILLER      PIC X(25) VALUE '                       DA'. ECS029
00237      12  FILLER      PIC X(25) VALUE 'TE                CMP    '. ECS029
00238      12  FILLER      PIC X(25) VALUE '       DATE       PAID   '. ECS029
00239      12  FILLER      PIC X(25) VALUE '  NUMBER      DEATH      '. ECS029
00240      12  FILLER      PIC X(24) VALUE ' DISAB.           NUMBER'.  ECS029
00241                                                                   ECS029
00242      EJECT                                                        ECS029
00243  01  COMP-3-AREA     COMP-3.                                      ECS029
00244      12  PG-NO                 PIC S9(5)           VALUE +0.      ECS029
00245      12  LN-CT                 PIC S9(5)           VALUE +0.      ECS029
00246      12  K1                    PIC S9(5)           VALUE +1.      ECS029
00247      12  CNT                   PIC S9(6)           VALUE +0.      ECS029
00248      12  T-DTH                 PIC S9(9)V99        VALUE +0.      ECS029
00249      12  T-DIS                 PIC S9(9)V99        VALUE +0.      ECS029
00250                                                                   ECS029
00251      12  G-TOTCNT              PIC S9(6)           VALUE +0.      ECS029
00252      12  G-TOTDTH              PIC S9(9)V99        VALUE +0.      ECS029
00253      12  G-TOTDIS              PIC S9(9)V99        VALUE +0.      ECS029
00254                                                                   ECS029
00255      12  A-T-CNT               PIC S9(6)           VALUE +0.      ECS029
00256      12  A-T-DTH               PIC S9(9)V99        VALUE +0.      ECS029
00257      12  A-T-DIS               PIC S9(9)V99        VALUE +0.      ECS029
00258                                                                   ECS029
00259      12  R-T-CNT               PIC S9(6)           VALUE +0.      ECS029
00260      12  R-T-DTH               PIC S9(9)V99        VALUE +0.      ECS029
00261      12  R-T-DIS               PIC S9(9)V99        VALUE +0.      ECS029
00262                                                                   ECS029
00263  01  MISC-WORK-AREA.                                              ECS029
00264      12  X                     PIC X               VALUE SPACE.   ECS029
00265      12  CLM-CTR               PIC 9(6)            VALUE 0.       ECS029
00266      12  CAR-SW                PIC X               VALUE SPACE.   ECS029
00267      12  SAV-CARR-CO.                                             ECS029
00268          16  SAV-CARRIER       PIC X               VALUE SPACES.  ECS029
00269          16  SAV-COMPANY       PIC X(6)            VALUE SPACES.  ECS029
00270      12  PREV-ACT              PIC X(10)           VALUE SPACES.  ECS029
00271      12  PREV-REI              PIC X(6)            VALUE SPACES.  ECS029
00272                                                                   ECS029
00273  01  TL-LN.                                                       ECS029
00274      12  FILLER                PIC X(8)            VALUE SPACES.  ECS029
00275      12  TLC                   PIC X(18)           VALUE          ECS029
00276                                              'TOTAL FOR COMPANY '.ECS029
00277      12  TL-MESS.                                                 ECS029
00278        14  TL-CARR             PIC X.                             ECS029
00279        14  TL-GROUP            PIC X(6).                          ECS029
00280        14  FILLER              PIC X(3)            VALUE SPACES.  ECS029
00281      12  FILLER                PIC X(32)           VALUE SPACES.  ECS029
00282      12  TL-CNT                PIC ZZZ,ZZ9.                       ECS029
00283      12  FILLER                PIC X(12)     VALUE ' CLAIMS FOR '.ECS029
00284      12  TL-DTH                PIC $$$,$$$,$$$.99-.               ECS029
00285      12  TL-DIS                PIC $$$,$$$,$$$.99-.               ECS029
00286                                                                   ECS029
00287  01  G-T-LN.                                                      ECS029
00288      12  FILL                  PIC X(8)            VALUE SPACE.   ECS029
00289      12  FILL                  PIC X(18)     VALUE 'GRAND TOTALS'.ECS029
00290      12  FILL                  PIC X(42)           VALUE SPACE.   ECS029
00291      12  GT-CNT                PIC ZZZ,ZZ9.                       ECS029
00292      12  FILLNT                PIC X(14)     VALUE ' CLAIMS FOR '.ECS029
00293      12  GT-DTH                PIC $$$,$$$,$$$.99-.               ECS029
00294      12  FILLER                PIC X               VALUE SPACES.  ECS029
00295      12  GT-DIS                PIC $$$,$$$,$$$.99-.               ECS029
00296                                                                   ECS029
00297      EJECT                                                        ECS029
00298  PROCEDURE DIVISION.                                              ECS029
00299  0100-SET-START.                                                  ECS029
00300                              COPY ELCDTERX SUPPRESS.              ECS029
00301                                                                   ECS029
00302      OPEN INPUT EXTRACT                                           ECS029
00303           OUTPUT PRNTR.                                           ECS029
00304                                                                   ECS029
00305      MOVE COMPANY-NAME           TO HD-CO.                        ECS029
00306      MOVE ALPH-DATE              TO HD-DT.                        ECS029
00307      ACCEPT  LCP-DATE-NEW-74 FROM DATE                            ECS029
00308      MOVE CORRESPONDING LCP-DATE-NEW-74 TO LCP-CURRENT-DATE-68    ECS029
00309      MOVE  LCP-CURRENT-DATE-68 TO HD-RD.                          ECS029
00310                                                                   ECS029
00311      IF DTE-PGM-OPT = '2'                                         ECS029
00312          MOVE 'YTD'              TO HD-KIND.                      ECS029
00313                                                                   ECS029
00314      IF DTE-PGM-OPT = '3'                                         ECS029
00315          MOVE 'MTD'              TO HD-KIND.                      ECS029
00316                                                                   ECS029
00317  0100-LOAD-REIN-COMP.                                             ECS029
00318      OPEN INPUT RTBL-FILE.                                        ECS029
00319                                                                   ECS029
00320      IF REIN-FILE-STATUS = '35'
              DISPLAY ' REIN STATUS 35 '
00321          GO TO 0400-CLOSE-ALL.                                    ECS029
00322                                                                   ECS029
00323      IF REIN-FILE-STATUS NOT = '00' AND '97'                      ECS029
00324         MOVE '2'                 TO WAC-1                         ECS029
00325         MOVE '1'                 TO WAC-2                         ECS029
00326         MOVE REIN-FILE-STATUS    TO WAC-3-4                       ECS029
00327         MOVE WORK-ABEND-CODE     TO WS-ABEND-FILE-STATUS          ECS029
00328         MOVE '**** BAD OPEN ON REINS FILE ****'                   ECS029
00329                                  TO WS-ABEND-MESSAGE              ECS029
00330         GO TO ABEND-PGM.                                          ECS029
00331                                                                   ECS029
00332      MOVE HIGH-VALUES            TO WS-REIN-COMP-TBL.             ECS029
00333                                                                   ECS029
00334  0100-LOAD-REIN-COMP-READ.                                        ECS029
00335      READ RTBL-FILE.                                              ECS029
00336      IF REIN-FILE-STATUS = '10'                                   ECS029
00337          CLOSE RTBL-FILE                                          ECS029
00338          IF REIN-FILE-STATUS = '00'                               ECS029
00339              GO TO 0110-SORT-IN                                   ECS029
00340          ELSE                                                     ECS029
00341              MOVE '2'            TO WAC-1                         ECS029
00342              MOVE '2'            TO WAC-2                         ECS029
00343              MOVE '**** BAD CLOSE ON REINS FILE ****'             ECS029
00344                                  TO WS-ABEND-MESSAGE              ECS029
00345              MOVE REIN-FILE-STATUS TO WAC-3-4                     ECS029
00346              MOVE WORK-ABEND-CODE  TO WS-ABEND-FILE-STATUS        ECS029
00347              GO TO ABEND-PGM.                                     ECS029
00348                                                                   ECS029
00349      IF REIN-FILE-STATUS NOT = '00'                               ECS029
00350          MOVE '2'                TO WAC-1                         ECS029
00351          MOVE '4'                TO WAC-2                         ECS029
00352          MOVE '**** BAD READ ON REINS FILE ****'                  ECS029
00353                                  TO WS-ABEND-MESSAGE              ECS029
00354          MOVE REIN-FILE-STATUS   TO WAC-3-4                       ECS029
00355          MOVE WORK-ABEND-CODE    TO WS-ABEND-FILE-STATUS          ECS029
00356          GO TO ABEND-PGM.                                         ECS029
00357                                                                   ECS029
00358      IF NOT RE-COMPANY-RECORD                                     ECS029
00359          GO TO 0100-LOAD-REIN-COMP-READ.                          ECS029
00360                                                                   ECS029
00361      ADD +1                      TO WS-RCT-X.                     ECS029
00362                                                                   ECS029
00363      IF WS-RCT-X GREATER WS-RCT-MAX                               ECS029
00364          MOVE '0'                TO WAC-1                         ECS029
00365          MOVE '2'                TO WAC-2                         ECS029
00366          MOVE '**** REINS COMPANY TABLE FULL ****'                ECS029
00367                                  TO WS-ABEND-MESSAGE              ECS029
00368          MOVE '01'               TO WAC-3-4                       ECS029
00369          MOVE WORK-ABEND-CODE    TO WS-ABEND-FILE-STATUS          ECS029
00370          GO TO ABEND-PGM.                                         ECS029
00371                                                                   ECS029
00372      MOVE RE-COMPANY             TO WS-RCT-COMP (WS-RCT-X).       ECS029
00373      MOVE RE-NAME                TO WS-RCT-NAME (WS-RCT-X).       ECS029
00374      MOVE RE-CEDE-NAME           TO WS-RCT-CEDE (WS-RCT-X).       ECS029
00375                                                                   ECS029
00376      GO TO 0100-LOAD-REIN-COMP-READ.                              ECS029
00377  EJECT                                                            ECS029
00378                                                                   ECS029
00379  0110-SORT-IN SECTION.                                            ECS029
00380      SORT CLM-SRT                                                 ECS029
00381          ASCENDING KEY S-CAR                                      ECS029
00382                        S-CO                                       ECS029
00383                        S-ACT                                      ECS029
00384                        S-CLM                                      ECS029
00385                        S-CHK                                      ECS029
00386          INPUT PROCEDURE 0120-GET-CLAIM THRU 0380-STEP-ONE-X      ECS029
00387          GIVING CLM-CARR.                                         ECS029
00388                                                                   ECS029
00389      IF SORT-RETURN NOT = ZEROS                                   ECS029
00390          MOVE +0101              TO WS-RETURN-CODE                ECS029
00391          GO TO ABEND-PGM.                                         ECS029
00392                                                                   ECS029
00393      GO TO 0390-STEP-TWO.                                         ECS029
00394                                                                   ECS029
00395  0120-GET-CLAIM  SECTION.                                         ECS029
00396                                                                   ECS029
00397      READ EXTRACT                                                 ECS029
00398          AT END  GO TO 0370-END-STEP.                             ECS029
00399                                                                   ECS029
00400      IF DE-RECORD-ID NOT = 'DE'                                   ECS029
00401          GO TO 0120-GET-CLAIM.                                    ECS029
00402                                                                   ECS029
00403      IF DE-REIN  = SPACE                                          ECS029
00404          GO TO 0120-GET-CLAIM.                                    ECS029
00405                                                                   ECS029
00406      IF NOT DE-CLAIM                                              ECS029
00407          GO TO 0120-GET-CLAIM.                                    ECS029

           IF DE-REINCO NOT = '145'
              GO TO 0120-GET-CLAIM
           END-IF
00408                                                                   ECS029
00409      COPY ELCEXTM1.                                               ECS029
00410                                                                   ECS029
00411      IF DTE-PGM-OPT = '2'                                         ECS029
00412          IF DE-CP-YR NOT = RUN-YR                                 ECS029
00413              GO TO 0120-GET-CLAIM.                                ECS029
00414                                                                   ECS029
00415      IF DTE-PGM-OPT = '3'                                         ECS029
00416          IF DE-CP-YR = RUN-YR  AND                                ECS029
00417             DE-CP-MO = RUN-MO                                     ECS029
00418              NEXT SENTENCE                                        ECS029
00419          ELSE                                                     ECS029
00420              GO TO 0120-GET-CLAIM.                                ECS029
00421                                                                   ECS029
00422  0130-ADD-CLM-CTR.                                                ECS029
00423      ADD 1                       TO CLM-CTR.                      ECS029
00424      IF PREV-ACT = SPACE                                          ECS029
00425          MOVE DE-ACCOUNT         TO PREV-ACT.                     ECS029
00426                                                                   ECS029
00427      IF PREV-REI = SPACE                                          ECS029
00428          MOVE DE-REI-COMP        TO PREV-REI.                     ECS029
00429                                                                   ECS029
00430  0140-MAIN-LOOP.                                                  ECS029
00431      IF SAV-CARR-CO = SPACES                                      ECS029
00432          GO TO 0200-SET-NEW.                                      ECS029
00433                                                                   ECS029
00434      IF DE-CARRIER  = SAV-CARRIER AND                             ECS029
00435         DE-GROUPING = SAV-COMPANY                                 ECS029
00436          NEXT SENTENCE                                            ECS029
00437      ELSE                                                         ECS029
00438          GO TO 0190-PRT-TOTALS.                                   ECS029
00439                                                                   ECS029
00440      IF DE-ACCOUNT NOT = PREV-ACT                                 ECS029
00441       IF CAR-SW = SPACE                                           ECS029
00442          PERFORM 0361-REIN-TOT THRU 0362-REIN-XIT                 ECS029
00443          PERFORM 0350-ACCT-TOT THRU 0360-ACCT-XIT                 ECS029
00444          GO TO 0150-CONT-RPT                                      ECS029
00445        ELSE                                                       ECS029
00446          PERFORM 0350-ACCT-TOT THRU 0360-ACCT-XIT                 ECS029
00447          GO TO 0150-CONT-RPT.                                     ECS029
00448                                                                   ECS029
00449      IF CAR-SW NOT = SPACE                                        ECS029
00450          GO TO 0150-CONT-RPT.                                     ECS029
00451                                                                   ECS029
00452      IF DE-REI-COMP NOT = PREV-REI                                ECS029
00453          PERFORM 0361-REIN-TOT THRU 0362-REIN-XIT.                ECS029
00454                                                                   ECS029
00455  0150-CONT-RPT.                                                   ECS029
00456      MOVE SPACES                 TO DTL.                          ECS029
00457      MOVE DE-STATE               TO P-ST.                         ECS029
00458      MOVE DE-ACCOUNT             TO P-ACCT.                       ECS029
00459      MOVE DE-CERT                TO P-CERT.                       ECS029
00460      MOVE DE-EF-MO               TO P-EMO.                        ECS029
00461      MOVE DE-EF-DA               TO P-EDA.                        ECS029
00462      MOVE DE-EF-YR               TO P-EYR.                        ECS029
00463      MOVE '-'                    TO P-EMOD  P-EDAD.               ECS029
00464      MOVE DE-LNAME               TO P-NAME.                       ECS029
00465      MOVE DE-REI-COMP            TO P-REI.                        ECS029
00466      MOVE DE-INCUR-MO            TO P-IMO.                        ECS029
00467      MOVE DE-INCUR-DA            TO P-IDA.                        ECS029
00468      MOVE DE-INCUR-YR            TO P-IYR.                        ECS029
00469      MOVE '-'                    TO P-IMOD  P-IDAD.               ECS029
00470      MOVE DE-PAY-MO              TO P-PMO.                        ECS029
00471      MOVE DE-PAY-DA              TO P-PDA.                        ECS029
00472      MOVE DE-PAY-YR              TO P-PYR.                        ECS029
00473      MOVE '-'                    TO P-PMOD  P-PDAD.               ECS029
00474      MOVE DE-CNUM                TO P-CLMNO.                      ECS029
00475      IF DE-OB-DTH OR DE-OB-AH                                     ECS029
00476          MOVE 'O.B. CERT.'       TO P-NAME.                       ECS029
00477      MOVE ZEROS                  TO P-DTH  P-DIS.                 ECS029
00478                                                                   ECS029
00479      IF  DE-DTH  OR  DE-OB-DTH                                    ECS029
00480          GO TO 0160-PT-DEATH.                                     ECS029
00481      IF  DE-AH  OR  DE-OB-AH                                      ECS029
00482          GO TO 0170-PT-DISAB.                                     ECS029
00483                                                                   ECS029
00484      MOVE 'UNKNO'                TO  P-TYPE.                      ECS029
00485                                                                   ECS029
00486      GO  TO   0180-PT-LST.                                        ECS029
00487                                                                   ECS029
00488  0160-PT-DEATH.                                                   ECS029
00489      MOVE 'DEATH'                TO P-TYPE.                       ECS029
00490      MOVE  DE-REI-CLAIM-AMT      TO  P-DTH.                       ECS029
00491      ADD  DE-REI-CLAIM-AMT       TO  T-DTH  A-T-DTH               ECS029
00492                                             R-T-DTH.              ECS029
00493      GO TO 0180-PT-LST.                                           ECS029
00494                                                                   ECS029
00495  0170-PT-DISAB.                                                   ECS029
00496      MOVE 'DISAB'                TO P-TYPE.                       ECS029
00497      IF DE-PAY-CODE = 'F'                                         ECS029
00498         MOVE 'AH-F '             TO P-TYPE.                       ECS029
00499      IF DE-PAY-CODE = 'P'                                         ECS029
00500         MOVE 'AH-P '             TO P-TYPE.                       ECS029
00501      IF DE-PAY-CODE = 'A'                                         ECS029
00502         MOVE 'AH-A '             TO P-TYPE.                       ECS029
00503      MOVE  DE-REI-CLAIM-AMT      TO  P-DIS.                       ECS029
00504      ADD  DE-REI-CLAIM-AMT       TO  T-DIS  A-T-DIS               ECS029
00505                                             R-T-DIS.              ECS029
00506                                                                   ECS029
00507  0180-PT-LST.                                                     ECS029
00508      MOVE  DE-ACCOUNT            TO PREV-ACT.                     ECS029
00509      MOVE  DE-REI-COMP           TO PREV-REI.                     ECS029
00510      MOVE  DE-CLM-AGE            TO P-AGE.                        ECS029
00511      MOVE  DE-CHECK              TO P-CHK-NO.                     ECS029
00512      MOVE ' '                    TO X.                            ECS029
00513      PERFORM 0300-PRT-RTN THRU 0310-PRT-XIT.                      ECS029
00514      ADD K1 TO LN-CT.                                             ECS029
00515      IF LN-CT GREATER THAN 52                                     ECS029
00516          PERFORM 0210-PT-HDNG.                                    ECS029
00517      ADD K1 TO CNT  A-T-CNT                                       ECS029
00518                     R-T-CNT.                                      ECS029
00519                                                                   ECS029
00520      IF CAR-SW = '9'                                              ECS029
00521          GO TO 0260-GET-CAR                                       ECS029
00522      ELSE                                                         ECS029
00523          PERFORM 0230-WRITE-CAR THRU 0240-W-CAR-END.              ECS029
00524                                                                   ECS029
00525      GO TO 0120-GET-CLAIM.                                        ECS029
00526                                                                   ECS029
00527  0190-PRT-TOTALS.                                                 ECS029
00528      IF CAR-SW = SPACE                                            ECS029
00529          PERFORM 0361-REIN-TOT THRU 0362-REIN-XIT                 ECS029
00530          PERFORM 0350-ACCT-TOT THRU 0360-ACCT-XIT                 ECS029
00531       ELSE                                                        ECS029
00532          PERFORM 0350-ACCT-TOT THRU 0360-ACCT-XIT.                ECS029
00533                                                                   ECS029
00534      MOVE 'TOTAL FOR COMPANY'    TO TLC.                          ECS029
00535      MOVE SPACES                 TO TL-MESS.                      ECS029
00536      MOVE SAV-CARRIER            TO TL-CARR.                      ECS029
00537      MOVE SAV-COMPANY            TO TL-GROUP.                     ECS029
00538      MOVE CNT                    TO TL-CNT.                       ECS029
00539      MOVE T-DTH                  TO TL-DTH.                       ECS029
00540      MOVE T-DIS                  TO TL-DIS.                       ECS029
00541      ADD T-DTH TO G-TOTDTH.                                       ECS029
00542      ADD T-DIS TO G-TOTDIS.                                       ECS029
00543      ADD CNT TO G-TOTCNT.                                         ECS029
00544      MOVE TL-LN                  TO P-DATA.                       ECS029
00545      MOVE '0' TO X.                                               ECS029
00546      PERFORM 0300-PRT-RTN THRU 0310-PRT-XIT.                      ECS029
00547      MOVE SPACES                 TO SAV-CARR-CO.                  ECS029
00548                                                                   ECS029
00549  0200-SET-NEW.                                                    ECS029
00550      IF CAR-SW = '9'                                              ECS029
00551          MOVE DE-CARRIER      TO HD-1A-CARR  TL-CARR  SAV-CARRIER ECS029
00552          MOVE DE-GROUPING     TO HD-1A-GROUP TL-GROUP SAV-COMPANY ECS029
00553          MOVE HD1-A           TO HD1                              ECS029
00554      ELSE                                                         ECS029
00555          MOVE DE-CARRIER      TO HD-CARR   TL-CARR   SAV-CARRIER  ECS029
00556          MOVE DE-GROUPING     TO HD-GROUP  TL-GROUP  SAV-COMPANY. ECS029
00557                                                                   ECS029
00558      MOVE DE-ACCOUNT          TO PREV-ACT.                        ECS029
00559      MOVE DE-REI-COMP         TO PREV-REI.                        ECS029
00560      MOVE ZEROS               TO CNT  T-DTH  T-DIS.               ECS029
00561                                                                   ECS029
00562      EJECT                                                        ECS029
00563  0210-PT-HDNG.                                                    ECS029
00564      IF CAR-SW = '9'                                              ECS029
00565          PERFORM 0220-ADD-REIN THRU 0220-ADD-EXIT
           ELSE
              MOVE ' DEVELOPED FOR EMERALD BAY REIN LTD SPC '
                                       TO HD2 (1:43)
           END-IF

00567      ADD K1 TO PG-NO.                                             ECS029
00568      MOVE PG-NO                  TO HD-PG.                        ECS029
00569      MOVE HD1                    TO P-DATA.                       ECS029
00570      MOVE '1'                    TO X.                            ECS029
00571      PERFORM 0300-PRT-RTN THRU 0310-PRT-XIT.                      ECS029
00572      MOVE HD2                    TO P-DATA.                       ECS029
00573      MOVE ' '                    TO X.                            ECS029
00574      PERFORM 0300-PRT-RTN THRU 0310-PRT-XIT.                      ECS029
00575      MOVE HD3                    TO P-DATA.                       ECS029
00576      MOVE ' '                    TO X.                            ECS029
00577      PERFORM 0300-PRT-RTN THRU 0310-PRT-XIT.                      ECS029
00578      MOVE HD4                    TO P-DATA.                       ECS029
00579      MOVE '0'                    TO X.                            ECS029
00580      PERFORM 0300-PRT-RTN THRU 0310-PRT-XIT.                      ECS029
00581      MOVE HD5                    TO P-DATA.                       ECS029
00582      MOVE ' '                    TO X.                            ECS029
00583      PERFORM 0300-PRT-RTN THRU 0310-PRT-XIT.                      ECS029
00584      MOVE SPACES                 TO P-DATA.                       ECS029
00585      MOVE ' '                    TO X.                            ECS029
00586      PERFORM 0300-PRT-RTN THRU 0310-PRT-XIT.                      ECS029
00587      MOVE SPACES                 TO P-DATA.                       ECS029
00588      MOVE ZERO                   TO LN-CT.                        ECS029
00589                                                                   ECS029
00590  0220-P-H-X.                                                      ECS029
00591      GO TO 0140-MAIN-LOOP.                                        ECS029
00592                                                                   ECS029
00593  0220-ADD-REIN.                                                   ECS029
00594      IF HD-1A-GROUP = 'TOTAL '                                    ECS029
00595         MOVE SPACES              TO HD2-TITLE    HD3-TITLE        ECS029
00596                                     HD2-REIN-NM  HD2-REIN-NM      ECS029
00597         GO TO 0220-ADD-EXIT.                                      ECS029
00598                                                                   ECS029
00599      MOVE 'CEDED TO   - '        TO HD2-TITLE.                    ECS029
00600      MOVE 'CEDED FROM - '        TO HD3-TITLE.                    ECS029
00601      MOVE +0                     TO WS-RCT-X.                     ECS029
00602                                                                   ECS029
00603  0220-REIN-NAME-LOOP.                                             ECS029
00604      ADD +1 TO WS-RCT-X.                                          ECS029
00605      IF WS-RCT-X GREATER WS-RCT-MAX OR                            ECS029
00606         WS-RCT-COMP (WS-RCT-X) = HIGH-VALUES                      ECS029
00607              MOVE 'NAME UNKNOWN' TO HD2-REIN-NM HD3-REIN-NM       ECS029
00608              GO TO 0220-ADD-EXIT.                                 ECS029
00609                                                                   ECS029
00610      IF SAV-COMPANY  = WS-RCT-COMP (WS-RCT-X)                     ECS029
00611          MOVE WS-RCT-NAME (WS-RCT-X) TO HD2-REIN-NM               ECS029
00612          MOVE WS-RCT-CEDE (WS-RCT-X) TO HD3-REIN-NM               ECS029
00613          GO TO 0220-ADD-EXIT.                                     ECS029
00614                                                                   ECS029
00615      GO TO 0220-REIN-NAME-LOOP.                                   ECS029
00616                                                                   ECS029
00617  0220-ADD-EXIT.                                                   ECS029
00618      EXIT.                                                        ECS029
00619                                                                   ECS029
00620      EJECT                                                        ECS029
00621  0230-WRITE-CAR.                                                  ECS029
00622      COPY ELCEXTM2.                                               ECS029
00623      MOVE DE-CLAIM-EXTRACT       TO SRT-REC.                      ECS029
00624      MOVE SPACES                 TO S-CAR.                        ECS029
00625      MOVE DE-REI-COMP            TO S-CO S-COPNY.                 ECS029
00626      RELEASE SRT-REC.                                             ECS029
00627                                                                   ECS029
00628  0240-W-CAR-END.                                                  ECS029
00629      EXIT.                                                        ECS029
00630                                                                   ECS029
00631  0250-PRNT-CARR.                                                  ECS029
00632      MOVE ZERO                   TO LN-CT  PG-NO                  ECS029
00633                                     T-DIS  T-DTH                  ECS029
00634                                     G-TOTDTH  G-TOTDIS  G-TOTCNT. ECS029
00635      MOVE 'TOTAL FOR REIN CO'    TO TLC.                          ECS029
00636      MOVE HD1-A                  TO HD1.                          ECS029
00637                                                                   ECS029
00638  0260-GET-CAR.                                                    ECS029
00639      READ CLM-CARR INTO DE-CLAIM-EXTRACT                          ECS029
00640          AT END  GO TO 0270-PRNT-CARR-XIT.                        ECS029
00641                                                                   ECS029
00642      COPY ELCEXTM1.                                               ECS029
00643                                                                   ECS029
00644      GO TO 0140-MAIN-LOOP.                                        ECS029
00645                                                                   ECS029
00646  0270-PRNT-CARR-XIT.                                              ECS029
00647      EXIT.                                                        ECS029
00648                                                                   ECS029
00649      EJECT                                                        ECS029
00650  0300-PRT-RTN.                                                    ECS029
00651                              COPY ELCPRT2.                        ECS029
00652  0310-PRT-XIT.    EXIT.                                           ECS029
00653                                                                   ECS029
00654      EJECT                                                        ECS029
00655  0330-G-TOTALS.                                                   ECS029
00656      IF CAR-SW = '9'                                              ECS029
00657          MOVE 'TOTAL'            TO HD-1A-GROUP                   ECS029
00658          MOVE HD1-A              TO HD1                           ECS029
00659      ELSE                                                         ECS029
00660          MOVE SPACE              TO HD-CARR                       ECS029
00661          MOVE 'TOTAL'            TO HD-GROUP.                     ECS029
00662                                                                   ECS029
00663      PERFORM 0210-PT-HDNG.                                        ECS029
00664      MOVE G-TOTCNT               TO GT-CNT.                       ECS029
00665      MOVE G-TOTDTH               TO GT-DTH.                       ECS029
00666      MOVE G-TOTDIS               TO GT-DIS.                       ECS029
00667      MOVE G-T-LN                 TO P-DATA.                       ECS029
00668      MOVE '0'                    TO X.                            ECS029
00669      PERFORM 0300-PRT-RTN THRU 0310-PRT-XIT.                      ECS029
00670                                                                   ECS029
00671  0340-END-G-TOTAL.                                                ECS029
00672       EXIT.                                                       ECS029
00673                                                                   ECS029
00674  0350-ACCT-TOT.                                                   ECS029
00675      MOVE 'TOTAL FOR ACCOUNT'    TO TLC.                          ECS029
00676      MOVE PREV-ACT               TO TL-MESS.                      ECS029
00677      MOVE SPACES                 TO P-DATA.                       ECS029
00678      MOVE A-T-CNT                TO TL-CNT.                       ECS029
00679      MOVE A-T-DTH                TO TL-DTH.                       ECS029
00680      MOVE A-T-DIS                TO TL-DIS.                       ECS029
00681      MOVE TL-LN                  TO P-DATA.                       ECS029
00682      MOVE '0'                    TO X.                            ECS029
00683      PERFORM 0300-PRT-RTN THRU 0310-PRT-XIT.                      ECS029
00684      ADD +3 TO LN-CT.                                             ECS029
00685      MOVE ZERO                   TO A-T-CNT                       ECS029
00686                                     A-T-DTH                       ECS029
00687                                     A-T-DIS.                      ECS029
00688                                                                   ECS029
00689  0360-ACCT-XIT.                                                   ECS029
00690      EXIT.                                                        ECS029
00691                                                                   ECS029
00692  0361-REIN-TOT.                                                   ECS029
00693      MOVE 'TOTAL FOR REI-CMP'    TO TLC.                          ECS029
00694      MOVE PREV-REI               TO TL-MESS.                      ECS029
00695      MOVE SPACES                 TO P-DATA.                       ECS029
00696      MOVE R-T-CNT                TO TL-CNT.                       ECS029
00697      MOVE R-T-DTH                TO TL-DTH.                       ECS029
00698      MOVE R-T-DIS                TO TL-DIS.                       ECS029
00699      MOVE TL-LN                  TO P-DATA.                       ECS029
00700      MOVE '0'                    TO X.                            ECS029
00701      PERFORM 0300-PRT-RTN THRU 0310-PRT-XIT.                      ECS029
00702      ADD +3 TO LN-CT.                                             ECS029
00703      MOVE ZERO                   TO R-T-CNT                       ECS029
00704                                     R-T-DTH                       ECS029
00705                                     R-T-DIS.                      ECS029
00706                                                                   ECS029
00707  0362-REIN-XIT.                                                   ECS029
00708      EXIT.                                                        ECS029
00709                                                                   ECS029
00710  0370-END-STEP.                                                   ECS029
00711      IF CLM-CTR = ZERO                                            ECS029
00712          MOVE ' NO CLAIMS INPUT RECEIVED FOR ECS-029' TO P-DATA   ECS029
00713          MOVE ' '                TO X                             ECS029
00714          PERFORM 0300-PRT-RTN THRU 0310-PRT-XIT                   ECS029
00715          GO TO 0380-STEP-ONE-X.                                   ECS029
00716                                                                   ECS029
00717      PERFORM 0190-PRT-TOTALS.                                     ECS029
00718      PERFORM 0330-G-TOTALS.                                       ECS029
00719                                                                   ECS029
00720  0380-STEP-ONE-X.                                                 ECS029
00721      EXIT.                                                        ECS029
00722                                                                   ECS029
00723      EJECT                                                        ECS029
00724  0390-STEP-TWO  SECTION.                                          ECS029
00725      IF CLM-CTR = ZERO                                            ECS029
00726          GO TO 0400-CLOSE-ALL.                                    ECS029
00727                                                                   ECS029
00728      OPEN INPUT CLM-CARR.                                         ECS029
00729                                                                   ECS029
00730      MOVE '9'                    TO CAR-SW.                       ECS029
00731      PERFORM 0250-PRNT-CARR THRU 0270-PRNT-CARR-XIT.              ECS029
00732      PERFORM 0190-PRT-TOTALS.                                     ECS029
00733      PERFORM 0330-G-TOTALS.                                       ECS029
00734                                                                   ECS029
00735  0400-CLOSE-ALL.                                                  ECS029
00736                              COPY ELCPRTC.                        ECS029
00737                                                                   ECS029
00738      CLOSE PRNTR  EXTRACT.                                        ECS029
00739                                                                   ECS029
00740      GOBACK.                                                      ECS029
00741                                                                   ECS029
00742  ABEND-PGM SECTION.                                               ECS029
00743                              COPY ELCABEND.                       ECS029
00744                                                                   ECS029
