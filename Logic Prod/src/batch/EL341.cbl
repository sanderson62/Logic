00001  IDENTIFICATION DIVISION.                                         04/18/98
00002                                                                   EL341
00003  PROGRAM-ID.                 EL341 .                                 LV006
00004 *              PROGRAM CONVERTED BY                               EL341
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL341
00006 *              CONVERSION DATE 03/11/96 16:01:21.                 EL341
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            EL341
00008 *                            VMOD=2.015                           EL341
00009                                                                   EL341
00009                                                                   EL341
00010 *AUTHOR.     LOGIC, INC.                                          EL341
00011 *            DALLAS, TEXAS.                                       EL341
00012                                                                   EL341
00013 *DATE-COMPILED.                                                   EL341
00014                                                                   EL341
00015 *SECURITY.   *****************************************************EL341
00016 *            *                                                   *EL341
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *EL341
00018 *            *                                                   *EL341
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL341
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL341
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *EL341
00022 *            *                                                   *EL341
00023 *            *****************************************************EL341
00024                                                                   EL341
00025 *REMARKS.                                                         EL341
00026 *    POST CLAIM PAYMENTS TO CLAS-IC ONLINE FILE                   EL341
00027                                                                   EL341
070714******************************************************************
070714*                   C H A N G E   L O G
070714*
070714* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
070714*-----------------------------------------------------------------
070714*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
070714* EFFECTIVE    NUMBER
070714*-----------------------------------------------------------------
070714* 070714  CR2013060600001  PEMA  AUTOMATE MONTH END BALANCING
070714******************************************************************
00028     EJECT                                                         EL341
00029  ENVIRONMENT DIVISION.                                            EL341
00030  INPUT-OUTPUT SECTION.                                            EL341
00031  FILE-CONTROL.                                                    EL341
00032                                                                   EL341
00033      SELECT  EXTRACT     ASSIGN TO SYS018-UT-2400-S-SYS018.       EL341
00034      SELECT  PRNTR       ASSIGN TO SYS008-UR-1403-S-SYS008.       EL341
00035      SELECT  DISK-DATE   ASSIGN TO SYS019-UT-FBA1-S-SYS019.       EL341
00036      SELECT  ELM-CARR    ASSIGN TO SYS015-UT-FBA1-S-SYS015.       EL341
00037      SELECT  ELM-SRT     ASSIGN TO SYS001-UT-FBA1-S-SORTWK1.      EL341
00038      SELECT  FICH        ASSIGN TO SYS020-UT-2400-S-SYS020.       EL341
00039                                                                      CL**5
00040      SELECT  ELTRLR      ASSIGN TO SYS012-FBA1-ELTRLR                CL**5
00041                          ORGANIZATION IS INDEXED                  EL341
00042                          ACCESS IS DYNAMIC                        EL341
00043                          RECORD KEY IS AT-CONTROL-PRIMARY         EL341
00044                          FILE STATUS IS ELTRLR-FILE-STATUS.       EL341
00045                                                                   EL341
00046      SELECT ELREPT       ASSIGN TO SYS018-FBA1-ELREPT             EL341
00047                          ORGANIZATION IS INDEXED                  EL341
00048                          ACCESS IS DYNAMIC                        EL341
00049                          RECORD KEY IS RF-CONTROL-PRIMARY         EL341
00050                          FILE STATUS IS DTE-VSAM-FLAGS.           EL341
00051                                                                   EL341
00052      SELECT ERMEBL       ASSIGN SYS024-FBA1-ERMEBL                   CL**5
00053                          ORGANIZATION INDEXED                     EL341
00054                          ACCESS DYNAMIC                           EL341
00055                          RECORD KEY ME-CONTROL-PRIMARY            EL341
00056                          FILE STATUS ERMEBL-FILE-STATUS.          EL341
00057      EJECT                                                        EL341
00058  DATA DIVISION.                                                   EL341
00059  FILE SECTION.                                                    EL341
00060                                                                   EL341
00061  FD  EXTRACT COPY ECSEXTFD.                                       EL341
00062                                                                   EL341
00063                              COPY ECSEXT01.                       EL341
00064                                                                   EL341
00065      EJECT                                                        EL341
00066  FD  ELTRLR.                                                      EL341
00067                                                                   EL341
00068                              COPY ELCTRLR.                        EL341
00069      EJECT                                                        EL341
00070  FD  DISK-DATE COPY ELCDTEFD.                                     EL341
00071                                                                   EL341
00072      EJECT                                                        EL341
00073  FD  PRNTR COPY ELCPRTFD.                                         EL341
00074                                                                   EL341
00075  01  DTL.                                                         EL341
00076      12  FILLER      PIC X.                                       EL341
00077      12  P-ST        PIC XX.                                      EL341
00078      12  FILLER      PIC X.                                       EL341
00079      12  P-ACCT      PIC X(10).                                   EL341
00080      12  FILLER      PIC X.                                       EL341
00081      12  P-CERT      PIC X(11).                                   EL341
00082      12  FILLER      PIC X.                                       EL341
00083      12  P-EMO       PIC 99.                                      EL341
00084      12  P-EMOD      PIC X.                                       EL341
00085      12  P-EDA       PIC 99.                                      EL341
00086      12  P-EDAD      PIC X.                                       EL341
00087      12  P-EYR       PIC 99.                                      EL341
00088      12  FILLER      PIC X.                                       EL341
00089      12  P-NAME      PIC X(14).                                   EL341
00090      12  FILLER      PIC X.                                       EL341
00091      12  P-INL       PIC XX.                                      EL341
00092      12  FILLER      PIC X.                                       EL341
00093      12  P-STATUS    PIC X(7).                                    EL341
00094      12  FILLER      PIC X.                                       EL341
00095      12  P-IMO       PIC 99.                                      EL341
00096      12  P-IMOD      PIC X.                                       EL341
00097      12  P-IDA       PIC 99.                                      EL341
00098      12  P-IDAD      PIC X.                                       EL341
00099      12  P-IYR       PIC 99.                                      EL341
00100      12  FILLER      PIC XX.                                      EL341
00101      12  P-PMO       PIC 99.                                      EL341
00102      12  P-PMOD      PIC X.                                       EL341
00103      12  P-PDA       PIC 99.                                      EL341
00104      12  P-PDAD      PIC X.                                       EL341
00105      12  P-PYR       PIC 99.                                      EL341
00106      12  FILLER      PIC X.                                       EL341
00107      12  P-CLMNO     PIC X(7).                                    EL341
00108      12  FILLER      PIC X.                                       EL341
00109      12  P-DTH       PIC ZZZZZ,ZZZ.99-  BLANK WHEN ZERO.          EL341
00110      12  FILLER      PIC X.                                       EL341
00111      12  P-DIS       PIC ZZZZ,ZZZ.99-   BLANK WHEN ZERO.          EL341
00112      12  FILLER      PIC X.                                       EL341
00113      12  P-CAUSE     PIC X(6).                                    EL341
00114      12  FILLER      PIC X.                                       EL341
00115      12  P-AGE       PIC XX.                                      EL341
00116      12  FILLER      PIC X.                                       EL341
00117      12  P-CHK-NO    PIC X(7).                                    EL341
00118                                                                   EL341
00119  01  DTL-RDF.                                                     EL341
00120      12  FILLER      PIC X(90).                                   EL341
00121      12  P-RMSG-LIFE PIC X(13).                                   EL341
00122      12  FILLER      PIC X.                                       EL341
00123      12  P-RMSG-AH   PIC X(13).                                   EL341
00124      12  FILLER      PIC X(16).                                   EL341
00125      EJECT                                                        EL341
00126                                                                   EL341
00127  FD  ELM-CARR                                                     EL341
00128      BLOCK CONTAINS 0 RECORDS
00129      RECORDING MODE F.                                            EL341
00130  01  CAR-REC.                                                     EL341
00131      12  FILLER          PIC X(507).                              EL341
00132      12  C-GROUPING-ALT  PIC X(3).                                EL341
00133      EJECT                                                        EL341
00134                                                                   EL341
00135  SD  ELM-SRT.                                                     EL341
00136  01  SRT-REC.                                                     EL341
00137      12  FILLER          PIC X(4).                                EL341
00138      12  S-CAR           PIC X.                                   EL341
00139      12  S-GROUPING      PIC X(6).                                EL341
00140      12  FILLER          PIC X(269).                              EL341
00141      12  S-CLM           PIC X(7).                                EL341
00142      12  S-CHK           PIC X(7).                                EL341
00143      12  FILLER          PIC X(210).                              EL341
00144      12  S-GROUPING-ALT  PIC X(6).                                EL341
00145                                                                   EL341
00146  FD  FICH                    COPY ELCFCHFD.                       EL341
00147                                                                   EL341
00148  FD  ELREPT                  COPY ELCRPTFD.                       EL341
00149                                                                   EL341
00150                              COPY ELCREPT.                        EL341
00151                                                                   EL341
00152  FD  ERMEBL.                                                      EL341
00153                         COPY ERCMEBL.                             EL341
00154                                                                   EL341
00155      EJECT                                                        EL341
00156  WORKING-STORAGE SECTION.                                         EL341
00157  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      EL341
00158  77  FILLER  PIC X(32) VALUE '********************************'.  EL341
00159  77  FILLER  PIC X(32) VALUE '*    EL341  WORKING STORAGE    *'.  EL341
00160  77  FILLER  PIC X(32) VALUE '*****VMOD=2.015*****************'.  EL341
00161                                                                   EL341
00162  77  ELM-CTR     PIC 9(6)    VALUE 0.                             EL341
00163                                                                   EL341
00164  01  MONTH-END-DATA.                                              EL341
00165      12  ME-START-DATE.                                           EL341
00166          16  ME-START-MO         PIC 99.                          EL341
00167          16  FILLER              PIC X.                           EL341
00168          16  ME-START-DA         PIC 99.                          EL341
00169          16  FILLER              PIC X.                           EL341
00170          16  ME-START-YR         PIC 99.                          EL341
00171      12  ME-CNDS-DATE            PIC 9(6).                        EL341
00172      12  ME-CNDS-DATE-R REDEFINES ME-CNDS-DATE.                   EL341
00173          16  ME-CNDS-MO     PIC 99.                               EL341
00174          16  ME-CNDS-DA     PIC 99.                               EL341
00175          16  ME-CNDS-YR     PIC 99.                               EL341
00176      12  ME-START-TIME           PIC 9(6).                        EL341
00177      12  ME-UPDATE-FLAG          PIC X           VALUE 'Y'.       EL341
00178          88  ME-DO-UPDATE                        VALUE 'Y'.       EL341
00179          88  ME-NO-UPDATE                        VALUE 'N'.       EL341
00180      12  ERMEBL-FILE-STATUS      PIC X(2).                        EL341
00181      12  MONTH-END-MOYR          PIC 9999 COMP.                   EL341
00182      12  MONTH-END-MOYR-R REDEFINES MONTH-END-MOYR                EL341
00183                   PIC XX.                                         EL341
00184                                                                   EL341
00185  01  DTE-INTERFACE-CODES.                                         EL341
00186      12  X                       PIC X           VALUE SPACE.     EL341
00187      12  PGM-SUB                 PIC S9(4)  COMP VALUE +341.      EL341
00188      12  ABEND-CODE              PIC 9999        VALUE ZERO.      EL341
00189      12  ABEND-OPTION            PIC X           VALUE SPACE.     EL341
00190      12  OLC-REPORT-NAME         PIC X(6)        VALUE 'EL341'.   EL341
00191      12  WS-ABEND-MESSAGE        PIC X(80)       VALUE SPACE.     EL341
00192      12  WS-ABEND-FILE-STATUS    PIC X(4)        VALUE SPACE.     EL341
00193      12  WS-RETURN-CODE          PIC S9(4)  COMP VALUE ZERO.      EL341
00194      12  WS-ZERO                 PIC S9   COMP-3 VALUE ZERO.      EL341
00195                                                                   EL341
00196  01  ELTRLR-FILE-STATUS.                                          EL341
00197          10  ELTRLR-FILE-STATUS1     PIC X.                       EL341
00198          10  ELTRLR-FILE-STATUS2     PIC X.                       EL341
00199                                                                   EL341
00200  01  ERROR-NF-COUNT                  PIC 9999   VALUE ZERO.       EL341
00201                                                                   EL341
00202  01  SAVE-POST-DATE                  PIC XX.                      EL341
00203                                                                   EL341
00204  01  WS-INITIALS.                                                 EL341
00205      05  WS-INITIALS1                   PIC X      VALUE SPACE.   EL341
00206      05  WS-INITIALS2                   PIC X      VALUE SPACE.   EL341
00207                                                                   EL341
00208  01  WS-AT-CONTROL-PRIMARY.                                       EL341
00209          16  WS-AT-COMPANY-CD           PIC X.                    EL341
00210          16  WS-AT-CARRIER              PIC X.                    EL341
00211          16  WS-AT-CLAIM-NO             PIC X(7).                 EL341
00212          16  WS-AT-CERT-NO.                                       EL341
00213              20  WS-AT-CERT-PRIME       PIC X(10).                EL341
00214              20  WS-AT-CERT-SFX         PIC X.                    EL341
00215          16  WS-AT-SEQUENCE-NO          PIC S9(4)   COMP VALUE +1.EL341
00216                                                                   EL341
00217      EJECT                                                        EL341
00218  01  HD1.                                                         EL341
00219      12  FILLER      PIC X(48)   VALUE SPACES.                    EL341
00220      12  HD1-CARR    PIC X(28)   VALUE                            EL341
00221       'CLAIMS POSTED FOR CARRIER - '.                             EL341
00222      12  HD-CARR     PIC X.                                       EL341
00223      12  FILLER      PIC X(42)   VALUE SPACES.                    EL341
00224      12  FILLER      PIC X(8)    VALUE 'EL341   '.                EL341
00225                                                                   EL341
00226  01  HD2.                                                         EL341
00227      12  FILLER      PIC X(47)   VALUE SPACES.                    EL341
00228      12  HD-CO       PIC X(30).                                   EL341
00229      12  FILLER      PIC X(42)   VALUE SPACES.                    EL341
00230      12  HD-RD       PIC X(8).                                    EL341
00231                                                                   EL341
00232  01  HD3.                                                         EL341
00233      12  FILLER      PIC X(53)   VALUE SPACES.                    EL341
00234      12  HD-DT       PIC X(18).                                   EL341
00235      12  FILLER      PIC X(48)   VALUE SPACES.                    EL341
00236      12  FILLER      PIC X(5)    VALUE 'PAGE '.                   EL341
00237      12  HD-PG       PIC ZZ,ZZ9.                                  EL341
00238                                                                   EL341
00239  01  HD4.                                                         EL341
00240      12  HD4-1   PIC X(25)  VALUE 'ST  ACCOUNT   CERTIFICATE'.    EL341
00241      12  FILLER  PIC X(11)  VALUE ' EFFECTIVE '.                  EL341
00242      12  FILLER  PIC X(21)  VALUE ' INSURED         STAT'.        EL341
00243      12  FILLER  PIC X(25)  VALUE 'US  INCURRED     DATE   C'.    EL341
00244      12  FILLER  PIC X(25)  VALUE 'LAIM   -----AMOUNT OF CLA'.    EL341
00245      12  FILLER  PIC X(25)  VALUE 'IM------ CAUSE AGE CHECK '.    EL341
00246                                                                   EL341
00247  01  HD5.                                                         EL341
00248      12  FILLER  PIC X(8)   VALUE SPACES.                         EL341
00249      12  FILLER  PIC X(25)  VALUE '                    DATE '.    EL341
00250      12  FILLER  PIC X(25)  VALUE '                         '.    EL341
00251      12  FILLER  PIC X(25)  VALUE '     DATE       PAID   NU'.    EL341
00252      12  FILLER  PIC X(10)  VALUE 'MBER      '.                   EL341
00253      12  HD5-LF  PIC X(6)   VALUE SPACES.                         EL341
00254      12  FILLER  PIC X(8)   VALUE SPACES.                         EL341
00255      12  HD5-AH  PIC X(6)   VALUE SPACES.                         EL341
00256      12  FILLER  PIC X(19)  VALUE '             NUMBER'.          EL341
00257                                                                   EL341
00258      EJECT                                                        EL341
00259  01  COMP-3-AREA     COMP-3.                                      EL341
00260      12  PG-NO       PIC S9(5)       VALUE +0.                    EL341
00261      12  LN-CT       PIC S9(5)       VALUE +0.                    EL341
00262      12  K1          PIC S9(5)       VALUE +1.                    EL341
00263      12  CNT         PIC S9(6)       VALUE +0.                    EL341
00264      12  CNT-R       PIC S9(6)       VALUE +0.                    EL341
00265      12  Z7          PIC S9(7)       VALUE +0.                    EL341
00266      12  T-DTH       PIC S9(7)V99    VALUE +0.                    EL341
00267      12  T-DIS       PIC S9(7)V99    VALUE +0.                    EL341
00268      12  T-DTH-R     PIC S9(7)V99    VALUE +0.                    EL341
00269      12  T-DIS-R     PIC S9(7)V99    VALUE +0.                    EL341
00270      12  Z9          PIC S9(7)V99    VALUE +0.                    EL341
00271      12  G-TOTCNT    PIC S9(6)       VALUE +0.                    EL341
00272      12  G-TOTDTH    PIC S9(7)V99    VALUE +0.                    EL341
00273      12  G-TOTDIS    PIC S9(7)V99    VALUE +0.                    EL341
00274      12  G-TOTCNT-R  PIC S9(7)V99   VALUE ZERO.                   EL341
00275      12  G-TOTDTH-R  PIC S9(7)V99    VALUE +0.                    EL341
00276      12  G-TOTDIS-R  PIC S9(7)V99    VALUE +0.                    EL341
00277      12  A-T-CNT     PIC S9(6)       VALUE +0.                    EL341
00278      12  A-T-DTH     PIC S9(7)V99    VALUE +0.                    EL341
00279      12  A-T-DIS     PIC S9(7)V99    VALUE +0.                    EL341
00280                                                                   EL341
00281  01  MISC-WORK-AREA.                                              EL341
00282      12  WK-DT       PIC 9(08)    VALUE 0.                           CL**2
00283      12  WK-DT-R REDEFINES WK-DT.                                    CL**2
00284          16  W-CC    PIC 99.                                      EL341
00285          16  W-YR    PIC 99.                                      EL341
00286          16  W-MO    PIC 99.                                      EL341
00287          16  W-DA    PIC 99.                                      EL341
00288      12  CAR-SW      PIC X        VALUE SPACE.                    EL341
00289      12  SAV-CARR    PIC X        VALUE SPACES.                   EL341
00290      12  PREV-ACT    PIC X(6)     VALUE SPACES.                   EL341
00291      12  SAV-PND-SW  PIC X.                                       EL341
00292      12  DAT-ERR-SW  PIC X        VALUE SPACE.                    EL341
00293                                                                   EL341
00294  01  TL-LN.                                                       EL341
00295      12  FILLER      PIC X(8)          VALUE SPACES.              EL341
00296      12  TLC         PIC X(18)         VALUE 'TOTAL FOR CARRIER '.EL341
00297      12  TL-CARR     PIC X.                                       EL341
00298      12  FILLER      PIC X(45)         VALUE SPACES.              EL341
00299      12  TL-CNT      PIC ZZZ,ZZ9.                                 EL341
00300      12  FILLER      PIC X(12)         VALUE ' CLAIMS FOR '.      EL341
00301      12  TL-DTH      PIC $(5),$$$.99-.                            EL341
00302      12  TL-DIS      PIC $(5),$$$.99-.                            EL341
00303                                                                   EL341
00304  01  TLR-LN.                                                      EL341
00305      12  FILLER      PIC X(8)        VALUE SPACES.                EL341
00306      12  FILLER      PIC X(30)                                    EL341
00307                           VALUE 'TOTAL REINSURANCE FOR CARRIER '. EL341
00308      12  TLR-CARR    PIC X.                                       EL341
00309      12  FILLER      PIC X(33)       VALUE SPACES.                EL341
00310      12  TLR-CNT     PIC ZZZ,ZZ9.                                 EL341
00311      12  FILLER      PIC X(12)       VALUE ' CLAIMS FOR '.        EL341
00312      12  TLR-DTH     PIC $(5),$$$.99-.                            EL341
00313      12  TLR-DIS     PIC $(5),$$$.99-.                            EL341
00314                                                                   EL341
00315  01  G-T-LN.                                                      EL341
00316      12  FILLER      PIC X(8)          VALUE  SPACE.              EL341
00317      12  FILLER      PIC X(18)         VALUE  'GRAND TOTALS'.     EL341
00318      12  FILLER      PIC X(41)         VALUE  SPACE.              EL341
00319      12  GT-CNT      PIC ZZZZ,ZZ9.                                EL341
00320      12  FILLNT      PIC X(12)         VALUE  ' CLAIMS FOR '.     EL341
00321      12  GT-DTH      PIC $(5),$$$.99-.                            EL341
00322      12  FILLER      PIC X(3)          VALUE  SPACES.             EL341
00323      12  GT-DIS      PIC $(5),$$$.99-.                            EL341
00324                                                                   EL341
00325  01  G-TR-LN.                                                     EL341
00326      12  FILLER      PIC X(8)          VALUE  SPACE.              EL341
00327      12  FILLER      PIC X(24)                                    EL341
00328                           VALUE  'GRAND TOTALS REINSURANCE'.      EL341
00329      12  FILLER      PIC X(35)         VALUE  SPACE.              EL341
00330      12  GTR-CNT     PIC ZZZZ,ZZ9.                                EL341
00331      12  FILLER      PIC X(12)         VALUE  ' CLAIMS FOR '.     EL341
00332      12  GTR-DTH     PIC $(5),$$$.99-.                            EL341
00333      12  FILLER      PIC X(3)          VALUE  SPACES.             EL341
00334      12  GTR-DIS     PIC $(5),$$$.99-.                            EL341
00335                                                                   EL341
00336  01  WARN-LN.                                                     EL341
00337      12  FILLER         PIC X(8)          VALUE  SPACE.           EL341
00338      12  WARN-LN-COUNT  PIC ZZZZ9.                                EL341
00339      12  FILLER         PIC X(120)        VALUE  SPACE.           EL341
00340                                                                   EL341
00341  01  ERROR-LN.                                                    EL341
00342      12  FILLER         PIC X(36)      VALUE SPACES.              EL341
00343      12  ERR-NOTE       PIC X(4).                                 EL341
00344      12  FILLER         PIC X(12)      VALUE ' ACCEPT DATE'.      EL341
00345      12  FILLER         PIC X(10)      VALUE SPACES.              EL341
00346      12  ERR-MO         PIC XX.                                   EL341
00347      12  FILLER         PIC X          VALUE '-'.                 EL341
00348      12  ERR-DA         PIC XX.                                   EL341
00349      12  FILLER         PIC X          VALUE '-'.                 EL341
00350      12  ERR-YR         PIC XX.                                   EL341
00351      12  FILLER         PIC X(63)      VALUE SPACES.              EL341
00352                                                                   EL341
00353      EJECT                                                        EL341
00354                              COPY ELCDTECX.                       EL341
00355      EJECT                                                        EL341
00356                              COPY ELCDTEVR.                       EL341
00357                                                                   EL341
00358                              COPY ELCDATE.                           CL**6
00359      EJECT                                                        EL341
00360  PROCEDURE DIVISION.                                              EL341
00361  CAPTURE-START.                                                   EL341
00362                                                                   EL341
00370  0100-SET-START. COPY ELCDTERX.                                   EL341
00371                                                                   EL341
00372      MOVE WS-TIME                TO ME-START-TIME.                EL341
00373      MOVE WS-CURRENT-DATE        TO ME-START-DATE.                EL341
00374      MOVE ME-START-MO            TO ME-CNDS-MO.                   EL341
00375      MOVE ME-START-DA            TO ME-CNDS-DA.                   EL341
00376      MOVE ME-START-YR            TO ME-CNDS-YR.                   EL341
00380      MOVE LIFE-OVERRIDE-L6       TO HD5-LF.                       EL341
00381      MOVE AH-OVERRIDE-L6         TO HD5-AH.                       EL341
00382                                                                   EL341
00388      OPEN INPUT EXTRACT                                           EL341
00389           OUTPUT PRNTR.                                           EL341
00390                                                                   EL341
00391      MOVE COMPANY-NAME           TO   HD-CO.                      EL341
00392      MOVE ALPH-DATE              TO   HD-DT.                      EL341
00393      MOVE WS-CURRENT-DATE        TO   HD-RD.                      EL341
00394      MOVE BIN-RUN-DATE           TO   SAVE-POST-DATE.             EL341
00395                                                                   EL341
00396  0110-SORT-IN SECTION.                                            EL341
00397                                                                   EL341
00398      SORT ELM-SRT                                                 EL341
00399          ASCENDING KEY S-CAR S-CLM S-CHK                          EL341
00400          INPUT PROCEDURE 0120-GET-CLAIM                           EL341
00401          GIVING ELM-CARR.                                         EL341
00402                                                                   EL341
00403      IF SORT-RETURN GREATER THAN ZERO                             EL341
00404          MOVE 'SORT FAILED'      TO  WS-ABEND-MESSAGE             EL341
00405          MOVE SORT-RETURN        TO  WS-RETURN-CODE               EL341
00406          PERFORM ABEND-PGM.                                       EL341
00407                                                                   EL341
00408      GO TO 0125-REPORT-POST-LOOP.                                 EL341
00409                                                                   EL341
00410      EJECT                                                        EL341
00411  0120-GET-CLAIM    SECTION.                                       EL341
00412                                                                   EL341
00413      READ EXTRACT                                                 EL341
00414          AT END  GO TO 0120-EXIT.                                 EL341
00415                                                                   EL341
00416      IF  VALID-DE-ID                                              EL341
00417          NEXT SENTENCE                                            EL341
00418      ELSE                                                         EL341
00419          GO TO 0120-GET-CLAIM.                                    EL341
00420                                                                   EL341
00421      IF NOT DE-CLAIM                                              EL341
00422          GO TO 0120-GET-CLAIM.                                    EL341
00423                                                                   EL341
00424      IF DE-ENTRY-STATUS EQUAL 'D' OR 'V'                          EL341
00425          GO TO 0120-GET-CLAIM.                                    EL341
00426                                                                   EL341
00427      IF  DE-REIN = 'R'                                            EL341
00428          IF  DE-ENTRY-STATUS = '9'                                EL341
00429              NEXT SENTENCE                                        EL341
00430          ELSE                                                     EL341
00431              GO TO 0120-GET-CLAIM.                                EL341
00432                                                                   EL341
00433      ADD     1                   TO ELM-CTR.                      EL341
00434      MOVE    DE-CLAIM-EXTRACT    TO SRT-REC.                      EL341
00435      MOVE    DE-GROUPING         TO S-GROUPING-ALT.               EL341
00436      MOVE    SPACES              TO S-GROUPING.                   EL341
00437      RELEASE SRT-REC.                                             EL341
00438      GO TO 0120-GET-CLAIM.                                        EL341
00439                                                                   EL341
00440  0120-EXIT.                                                       EL341
00441      EXIT.                                                        EL341
00442      EJECT                                                        EL341
00443  0125-REPORT-POST-LOOP  SECTION.                                  EL341
00444      OPEN INPUT ELM-CARR                                          EL341
00445           I-O   ELTRLR.                                           EL341
00446                                                                   EL341
00447                                                                   EL341
00448      IF ELTRLR-FILE-STATUS  = '00' OR '97'                        EL341
00449          NEXT SENTENCE                                            EL341
00450        ELSE                                                       EL341
00451          MOVE 'ERROR OCCURED OPEN - ELTRLR' TO  WS-ABEND-MESSAGE  EL341
00452          MOVE ELTRLR-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL341
00453          PERFORM ABEND-PGM.                                       EL341
00454                                                                   EL341
00455      PERFORM 0127-PRNT-CARR THRU 0129-EXIT.                       EL341
00456      PERFORM 0180-PRT-TOTALS.                                     EL341
00457      PERFORM 0360-G-TOTALS.                                       EL341
00458                                                                   EL341
00459      IF ERROR-NF-COUNT NOT = ZERO                                 EL341
00460          PERFORM 0200-PT-HDNG THRU 0210-HDNG-EXIT                 EL341
00461          PERFORM 0250-PT-WARNING 4 TIMES.                         EL341
00462                                                                   EL341
00463  0126-CLOSE-ALL. COPY ELCPRTCX.                                   EL341
00464                                                                   EL341
00465      CLOSE PRNTR                                                  EL341
00466            EXTRACT                                                EL341
00467            ELTRLR                                                 EL341
00468            ELM-CARR.                                              EL341
00469                                                                   EL341
00470      IF ELTRLR-FILE-STATUS NOT = ZERO                             EL341
00471          MOVE 'ERROR OCCURED CLOSE - ELTRLR'  TO  WS-ABEND-MESSAGEEL341
00472          MOVE ELTRLR-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL341
00473          PERFORM ABEND-PGM.                                       EL341
00474                                                                   EL341
070714     OPEN I-O ERMEBL.                                             EL341
070714                                                                  EL341
070714     IF ERMEBL-FILE-STATUS  = '00' OR '97'                        EL341
070714        continue
070714     ELSE
070714        MOVE 'N'                 TO ME-UPDATE-FLAG
070714     end-if
070714     MOVE DTE-CLIENT             TO ME-COMPANY.                   EL341
070714     COMPUTE MONTH-END-MOYR = RUN-CCYY * 12 + RUN-MO.             EL341
070714     MOVE MONTH-END-MOYR         TO ME-MOYR.                      EL341
070714     IF ME-DO-UPDATE                                              EL341
070714         READ ERMEBL INVALID KEY                                  EL341
070714              MOVE 'N'           TO ME-UPDATE-FLAG                EL341
070714         CLOSE ERMEBL.                                            EL341
070714                                                                  EL341
070714     ACCEPT WS-TIME-OF-DAY       FROM  TIME.                      EL341
070714                                                                  EL341
070714     IF ME-DO-UPDATE
070714        ADD 1                    TO ME-341-RUN-CT
070714        move G-TOTDTH            to me-341-clms-l
070714        move g-totdis            to me-341-clms-ah
070714        MOVE ERROR-NF-COUNT      TO ME-341-NOT-FOUND
070714        MOVE ME-CNDS-DATE        TO ME-341-RUN-DT
070714        REWRITE MONTH-END-BALANCES
070714        CLOSE ERMEBL
070714     end-if
00487                                                                   EL341
00488      GOBACK.                                                      EL341
00489      EJECT                                                        EL341
00490  0127-PRNT-CARR SECTION.                                          EL341
00491                                                                   EL341
00492      MOVE ZERO                   TO LN-CT.                        EL341
00493      MOVE Z9                     TO T-DIS                         EL341
00494                                     T-DTH                         EL341
00495                                     G-TOTDTH                      EL341
00496                                     G-TOTDIS                      EL341
00497                                     G-TOTCNT                      EL341
00498                                     T-DIS-R                       EL341
00499                                     T-DTH-R                       EL341
00500                                     G-TOTDTH-R                    EL341
00501                                     G-TOTDIS-R                    EL341
00502                                     G-TOTCNT-R.                   EL341
00503                                                                   EL341
00504  0128-GET-CAR.                                                    EL341
00505      READ ELM-CARR INTO DETAIL-EXTRACT                            EL341
00506           AT END  GO TO 0129-EXIT.                                EL341
00507                                                                   EL341
00508      IF  VALID-DE-ID                                              EL341
00509          NEXT SENTENCE                                            EL341
00510      ELSE                                                         EL341
00511          GO TO 0128-GET-CAR.                                      EL341
00512                                                                   EL341
00513      IF NOT DE-CLAIM                                              EL341
00514          GO TO 0128-GET-CAR.                                      EL341
00515                                                                   EL341
00516      PERFORM 0130-MAIN-LOOP.                                      EL341
00517                                                                   EL341
00518      GO TO 0128-GET-CAR.                                          EL341
00519                                                                   EL341
00520  0129-EXIT.                                                       EL341
00521      EXIT.                                                        EL341
00522      EJECT                                                        EL341
00523  0130-MAIN-LOOP  SECTION.                                         EL341
00524                                                                   EL341
00525      IF SAV-CARR = SPACES                                         EL341
00526          PERFORM 0190-SET-NEW.                                    EL341
00527                                                                   EL341
00528      IF  DE-CARRIER NOT = SAV-CARR                                EL341
00529          PERFORM 0180-PRT-TOTALS                                  EL341
00530          PERFORM 0190-SET-NEW.                                    EL341
00531                                                                   EL341
00532  0140-CONT-RPT.                                                   EL341
00533      MOVE SPACES                 TO  DTL.                         EL341
00534      MOVE DE-STATE               TO  P-ST.                        EL341
00535      MOVE DE-ACCOUNT             TO  P-ACCT.                      EL341
00536      MOVE DE-CERT                TO  P-CERT.                      EL341
00537      MOVE DE-EFF                 TO  WK-DT.                       EL341
00538      MOVE W-MO                   TO  P-EMO.                       EL341
00539      MOVE W-DA                   TO  P-EDA.                       EL341
00540      MOVE W-YR                   TO  P-EYR.                       EL341
00541      MOVE '-'                    TO  P-EMOD                       EL341
00542                                      P-EDAD.                      EL341
00543      MOVE DE-LNAME               TO  P-NAME.                      EL341
00544      MOVE DE-1ST-INIT-FNAME      TO WS-INITIALS1.                 EL341
00545      MOVE DE-INIT                TO WS-INITIALS2.                 EL341
00546      MOVE WS-INITIALS            TO P-INL.                        EL341
00547      MOVE DE-CNUM                TO P-CLMNO.                      EL341
00548                                                                   EL341
00549      IF DE-AUTO-GEND-REINS                                        EL341
00550          MOVE SPACES             TO P-STATUS                      EL341
00551      ELSE                                                         EL341
00552          MOVE 'POSTED'           TO P-STATUS.                     EL341
00553                                                                   EL341
00554      IF   DE-OB-DTH                                               EL341
00555        OR DE-OB-AH                                                EL341
00556           MOVE 'O.B. CERT.'      TO P-NAME                        EL341
00557           MOVE SPACES            TO P-INL.                        EL341
00558                                                                   EL341
00559      MOVE ZEROS                  TO P-DTH                         EL341
00560                                     P-DIS.                        EL341
00561                                                                   EL341
00562      IF   DE-DTH                                                  EL341
00563        OR DE-OB-DTH                                               EL341
00564           GO TO 0150-PT-DEATH.                                    EL341
00565                                                                   EL341
00566      IF   DE-AH                                                   EL341
00567        OR DE-OB-AH                                                EL341
00568           GO TO 0160-PT-DISAB.                                    EL341
00569                                                                   EL341
00570      GO TO 0170-PT-LST.                                              CL**5
00571                                                                   EL341
00572  0150-PT-DEATH.                                                   EL341
00573      IF  DE-REIN   = 'R'                                          EL341
00574          ADD DE-CLAIM-AMT TO T-DTH-R                              EL341
00575      ELSE                                                         EL341
00576          ADD DE-CLAIM-AMT TO T-DTH                                EL341
00577                              A-T-DTH.                             EL341
00578                                                                   EL341
00579      MOVE  DE-CLAIM-AMT          TO  P-DTH.                       EL341
00580      GO TO 0170-PT-LST.                                           EL341
00581                                                                   EL341
00582  0160-PT-DISAB.                                                   EL341
00583      IF  DE-REIN  = 'R'                                           EL341
00584          ADD DE-CLAIM-AMT TO T-DIS-R                              EL341
00585      ELSE                                                         EL341
00586          ADD DE-CLAIM-AMT TO T-DIS                                EL341
00587                              A-T-DIS.                             EL341
00588                                                                   EL341
00589      MOVE  DE-CLAIM-AMT          TO  P-DIS.                       EL341
00590                                                                   EL341
00591  0170-PT-LST.                                                     EL341
00592                                                                   EL341
00593      IF  DE-REIN  = 'R'                                           EL341
00594          IF (DE-DTH OR DE-OB-DTH)                                    CL**5
00595               MOVE '*** REINS ***'   TO   P-RMSG-AH               EL341
00596             ELSE                                                  EL341
00597          IF (DE-AH OR DE-OB-AH)                                      CL**5
00598               MOVE '*** REINS ***'   TO   P-RMSG-LIFE.            EL341
00599                                                                   EL341
00600      MOVE    DE-INCUR            TO  WK-DT.                       EL341
00601      MOVE    W-MO                TO  P-IMO.                       EL341
00602      MOVE    W-DA                TO  P-IDA.                       EL341
00603      MOVE    W-YR                TO  P-IYR.                       EL341
00604      MOVE    '-'                 TO  P-IMOD                       EL341
00605                                      P-IDAD.                      EL341
00606      MOVE    DE-CLM-AGE          TO  P-AGE.                       EL341
00607      MOVE    DE-PAY              TO  WK-DT.                       EL341
00608      MOVE    W-MO                TO  P-PMO.                       EL341
00609      MOVE    W-DA                TO  P-PDA.                       EL341
00610      MOVE    W-YR                TO  P-PYR.                       EL341
00611      MOVE    '-'                 TO  P-PMOD                       EL341
00612                                      P-PDAD.                      EL341
00613      MOVE    DE-CHECK            TO  P-CHK-NO.                    EL341
00614      MOVE    DE-CLM-CAUSE        TO  P-CAUSE.                     EL341
00615                                                                   EL341
00616      IF NOT DE-AUTO-GEND-REINS                                    EL341
00617          PERFORM 0340-POST-PAYMENT.                               EL341
00618                                                                   EL341
00619      MOVE    SPACE               TO  X.                           EL341
00620      PERFORM 0320-PRT-RTN THRU 0330-PRT-RTN-XIT.                  EL341
00621      ADD     K1                  TO  LN-CT.                       EL341
00622                                                                   EL341
00623      IF DAT-ERR-SW NOT = SPACE                                    EL341
00624          MOVE ERROR-LN           TO P-DATA                        EL341
00625          MOVE SPACE              TO  X                            EL341
00626          PERFORM 0320-PRT-RTN THRU 0330-PRT-RTN-XIT               EL341
00627          MOVE SPACE              TO DAT-ERR-SW                    EL341
00628          ADD K1                  TO LN-CT                         EL341
00629          MOVE SPACES             TO  P-DATA                       EL341
00630                                      X                            EL341
00631          PERFORM 0320-PRT-RTN THRU 0330-PRT-RTN-XIT               EL341
00632          ADD K1                  TO LN-CT                         EL341
00633          ADD K1                  TO ERROR-NF-COUNT.               EL341
00634                                                                   EL341
00635      IF LN-CT GREATER THAN 52                                     EL341
00636          PERFORM 0200-PT-HDNG THRU 0230-ALL-HDNG-E.               EL341
00637                                                                   EL341
00638      IF  DE-REIN  = 'R'                                           EL341
00639          ADD K1 TO CNT-R                                          EL341
00640      ELSE                                                         EL341
00641         ADD K1 TO CNT.                                            EL341
00642                                                                   EL341
00643      ADD K1 TO A-T-CNT.                                           EL341
00644                                                                   EL341
00645      EJECT                                                        EL341
00646  0180-PRT-TOTALS  SECTION.                                        EL341
00647                                                                   EL341
00648      MOVE    CNT                 TO  TL-CNT.                      EL341
00649      MOVE    T-DTH               TO  TL-DTH.                      EL341
00650      MOVE    T-DIS               TO  TL-DIS.                      EL341
00651      ADD     T-DTH               TO  G-TOTDTH.                    EL341
00652      ADD     T-DIS               TO  G-TOTDIS.                    EL341
00653      ADD     CNT                 TO  G-TOTCNT.                    EL341
00654      MOVE    TL-LN               TO  P-DATA.                      EL341
00655      MOVE    '0'                 TO  X.                           EL341
00656      PERFORM 0320-PRT-RTN THRU 0330-PRT-RTN-XIT.                  EL341
00657                                                                   EL341
00658      MOVE    SPACES              TO  P-DATA.                      EL341
00659      MOVE    CNT-R               TO  TLR-CNT.                     EL341
00660      MOVE    T-DTH-R             TO  TLR-DTH.                     EL341
00661      MOVE    T-DIS-R             TO  TLR-DIS.                     EL341
00662      ADD     T-DTH-R             TO  G-TOTDTH-R.                  EL341
00663      ADD     T-DIS-R             TO  G-TOTDIS-R.                  EL341
00664      ADD     CNT-R               TO  G-TOTCNT-R.                  EL341
00665      MOVE    TLR-LN              TO  P-DATA.                      EL341
00666      MOVE    '0'                 TO  X.                           EL341
00667      PERFORM 0320-PRT-RTN THRU 0330-PRT-RTN-XIT.                  EL341
00668      MOVE    SPACES              TO  SAV-CARR.                    EL341
00669                                                                   EL341
00670  0190-SET-NEW   SECTION.                                          EL341
00671                                                                   EL341
00672      MOVE DE-CARRIER             TO HD-CARR                       EL341
00673                                     TL-CARR                       EL341
00674                                     TLR-CARR                      EL341
00675                                     SAV-CARR.                     EL341
00676      MOVE Z7                     TO CNT                           EL341
00677                                     CNT-R.                        EL341
00678      MOVE Z9                     TO T-DTH                         EL341
00679                                     T-DIS                         EL341
00680                                     T-DTH-R                       EL341
00681                                     T-DIS-R.                      EL341
00682                                                                   EL341
00683  0200-PT-HDNG.                                                    EL341
00684      ADD     K1                  TO  PG-NO.                       EL341
00685      MOVE    PG-NO               TO  HD-PG                        EL341
00686      MOVE    HD1                 TO  P-DATA.                      EL341
00687      MOVE    '1'                 TO  X.                           EL341
00688      PERFORM 0320-PRT-RTN THRU 0330-PRT-RTN-XIT.                  EL341
00689      MOVE    HD2                 TO  P-DATA.                      EL341
00690      MOVE    SPACE               TO  X.                           EL341
00691      PERFORM 0320-PRT-RTN THRU 0330-PRT-RTN-XIT.                  EL341
00692      MOVE    HD3                 TO  P-DATA.                      EL341
00693      MOVE    SPACE               TO  X.                           EL341
00694      PERFORM 0320-PRT-RTN THRU 0330-PRT-RTN-XIT.                  EL341
00695                                                                   EL341
00696  0210-HDNG-EXIT.                                                  EL341
00697      EXIT.                                                        EL341
00698                                                                   EL341
00699  0220-DETAIL-HDNG.                                                EL341
00700      MOVE    HD4                 TO  P-DATA.                      EL341
00701      MOVE    '0'                 TO  X.                           EL341
00702      PERFORM 0320-PRT-RTN THRU 0330-PRT-RTN-XIT.                  EL341
00703      MOVE    HD5                 TO  P-DATA.                      EL341
00704      MOVE    SPACE               TO  X.                           EL341
00705      PERFORM 0320-PRT-RTN THRU 0330-PRT-RTN-XIT.                  EL341
00706      MOVE    SPACES              TO  P-DATA                       EL341
00707                                      X.                           EL341
00708      PERFORM 0320-PRT-RTN THRU 0330-PRT-RTN-XIT.                  EL341
00709      MOVE    SPACES              TO  P-DATA.                      EL341
00710      MOVE    ZERO                TO  LN-CT.                       EL341
00711                                                                   EL341
00712  0230-ALL-HDNG-E.                                                 EL341
00713      EXIT.                                                        EL341
00714      EJECT                                                        EL341
00715                                                                   EL341
00716  0250-PT-WARNING  SECTION.                                        EL341
00717                                                                   EL341
00718      MOVE ' ***************************************************'  EL341
00719                                  TO  P-DATA.                      EL341
00720      MOVE    '-'                 TO  X.                           EL341
00721      PERFORM 0320-PRT-RTN THRU 0330-PRT-RTN-XIT.                  EL341
00722      MOVE ' *** WARNING ... WARNING ... WARNING ... WARNING ***'  EL341
00723                                  TO  P-DATA.                      EL341
00724      MOVE    SPACE               TO  X.                           EL341
00725      PERFORM 0320-PRT-RTN THRU 0330-PRT-RTN-XIT.                  EL341
00726      MOVE ' ***                                             ***'  EL341
00727                                  TO  P-DATA.                      EL341
00728      MOVE    SPACE               TO  X.                           EL341
00729      PERFORM 0320-PRT-RTN THRU 0330-PRT-RTN-XIT.                  EL341
00730      MOVE ' ***         THIS  RUN  CONTAINS  ERRORS         ***'  EL341
00731                                  TO  P-DATA.                      EL341
00732      MOVE    SPACE               TO  X.                           EL341
00733      PERFORM 0320-PRT-RTN THRU 0330-PRT-RTN-XIT.                  EL341
00734      MOVE ' ***                                             ***'  EL341
00735                                  TO  P-DATA.                      EL341
00736      MOVE    SPACE               TO  X.                           EL341
00737      PERFORM 0320-PRT-RTN THRU 0330-PRT-RTN-XIT.                  EL341
00738      MOVE ' ***    XXXXX CLAIM PAYMENTS WERE NOT POSTED     ***'  EL341
00739                                  TO  WARN-LN.                     EL341
00740      MOVE ERROR-NF-COUNT         TO  WARN-LN-COUNT.               EL341
00741      MOVE WARN-LN                TO  P-DATA.                      EL341
00742      MOVE    SPACE               TO  X.                           EL341
00743      PERFORM 0320-PRT-RTN THRU 0330-PRT-RTN-XIT.                  EL341
00744      MOVE ' ***                                             ***'  EL341
00745                                  TO  P-DATA.                      EL341
00746      MOVE    SPACE               TO  X.                           EL341
00747      PERFORM 0320-PRT-RTN THRU 0330-PRT-RTN-XIT.                  EL341
00748      MOVE ' *** WARNING ... WARNING ... WARNING ... WARNING ***'  EL341
00749                                  TO  P-DATA.                      EL341
00750      MOVE    SPACE               TO  X.                           EL341
00751      PERFORM 0320-PRT-RTN THRU 0330-PRT-RTN-XIT.                  EL341
00752      MOVE ' ***************************************************'  EL341
00753                                  TO  P-DATA.                      EL341
00754      MOVE    SPACE               TO  X.                           EL341
00755      PERFORM 0320-PRT-RTN THRU 0330-PRT-RTN-XIT.                  EL341
00756                                                                   EL341
00757  0260-WARNING-EXIT.                                               EL341
00758      EXIT.                                                        EL341
00759                                                                   EL341
00760      EJECT                                                        EL341
00761  0320-PRT-RTN  SECTION. COPY ELCPRT2X.                            EL341
00762                                                                   EL341
00763  0330-PRT-RTN-XIT.                                                EL341
00764      EXIT.                                                        EL341
00765      EJECT                                                        EL341
00766  0340-POST-PAYMENT  SECTION.                                      EL341
00767                                                                   EL341
00768      MOVE LOW-VALUE              TO WS-AT-CONTROL-PRIMARY.        EL341
00769      MOVE DTE-CLASIC-COMPANY-CD  TO WS-AT-COMPANY-CD.             EL341
00770      MOVE DE-CERT                TO WS-AT-CERT-NO.                EL341
00771      MOVE DE-CNUM                TO WS-AT-CLAIM-NO.               EL341
00772                                                                   EL341
00773 **** CARRIER IS NOW REQUIRED.                                     EL341
00774 *                                                                 EL341
00775 *    IF  DTE-CLASIC-CLAIM-ACCESS = '2'                            EL341
00776      MOVE DE-CARRIER             TO WS-AT-CARRIER.                EL341
00777 *    ELSE                                                         EL341
00778 *        MOVE SPACE              TO WS-AT-CARRIER.                EL341
00779                                                                   EL341
00780      MOVE DE-PMT-TRAILER-SEQ     TO WS-AT-SEQUENCE-NO.            EL341
00781                                                                   EL341
00782      MOVE WS-AT-CONTROL-PRIMARY  TO AT-CONTROL-PRIMARY.           EL341
00783                                                                   EL341
00784  0344-READ-NEXT.                                                  EL341
00785      READ ELTRLR.                                                 EL341
00786                                                                   EL341
00787      IF ELTRLR-FILE-STATUS1 = '1'                                 EL341
00788           GO TO 0348-NOT-FOUND.                                   EL341
00789                                                                   EL341
00790      IF ELTRLR-FILE-STATUS = '23'                                 EL341
00791           GO TO 0348-NOT-FOUND.                                   EL341
00792                                                                   EL341
00793      IF ELTRLR-FILE-STATUS1 NOT = ZERO                            EL341
00794          MOVE 'ERROR OCCURED READ - ELTRLR'                       EL341
00795                                  TO  WS-ABEND-MESSAGE             EL341
00796          MOVE ELTRLR-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL341
00797          PERFORM ABEND-PGM.                                       EL341
00798                                                                   EL341
00799      IF DTE-CLASIC-COMPANY-CD NOT = AT-COMPANY-CD                 EL341
00800           GO TO 0348-NOT-FOUND.                                   EL341
00801                                                                   EL341
00802      IF NOT PAYMENT-TR                                            EL341
00803           GO TO 0348-NOT-FOUND.                                   EL341
00804                                                                   EL341
00805      IF WS-AT-CERT-NO NOT = AT-CERT-NO                            EL341
00806          GO TO 0348-NOT-FOUND.                                    EL341
00807                                                                   EL341
00808      IF WS-AT-CLAIM-NO NOT = AT-CLAIM-NO                          EL341
00809          GO TO 0348-NOT-FOUND.                                    EL341
00810                                                                   EL341
00811      MOVE DE-PAID-TO             TO DC-GREG-DATE-1-YMD.           EL341
00812      MOVE '3'                    TO DC-OPTION-CODE.               EL341
00813      PERFORM 8500-DATE-CONVERSION THRU 8590-EXIT.                 EL341
00814                                                                   EL341
00815      IF   (DE-CLAIM-AMT NEGATIVE                                  EL341
00816        OR DE-CLAIM-AMT = ZEROS)                                   EL341
00817           IF  VOID-NOT-SELECTED                                   EL341
00818               NEXT SENTENCE                                       EL341
00819           ELSE                                                    EL341
00820               IF AT-VOID-SELECT-DT NOT = SAVE-POST-DATE OR        EL341
00821                  AT-VOID-SELECT-DT NOT LESS THAN SAVE-POST-DATE   EL341
00822               GO TO 0346-VOID-ROUTINE.                            EL341
00823                                                                   EL341
00824  0345-PAYMENT-ROUTINE.                                            EL341
00825      IF  AT-PMT-ACCEPT-DT NOT = LOW-VALUES                        EL341
00826          IF  AT-PMT-ACCEPT-DT NOT = SAVE-POST-DATE                EL341
00827              GO TO 0349-PMT-DT-ERROR.                             EL341
00828                                                                   EL341
00829      MOVE SAVE-POST-DATE TO AT-PMT-ACCEPT-DT.                     EL341
00830      REWRITE  ACTIVITY-TRAILERS.                                  EL341
00831      GO TO 0359-EXIT.                                             EL341
00832                                                                   EL341
00833  0346-VOID-ROUTINE.                                               EL341
00834      IF  AT-VOID-ACCEPT-DT NOT = LOW-VALUES                       EL341
00835         IF  AT-VOID-ACCEPT-DT NOT = SAVE-POST-DATE                EL341
00836              GO TO 0350-VOID-DT-ERROR.                            EL341
00837                                                                   EL341
00838      MOVE SAVE-POST-DATE         TO AT-VOID-ACCEPT-DT.            EL341
00839      REWRITE  ACTIVITY-TRAILERS.                                  EL341
00840      GO TO 0359-EXIT.                                             EL341
00841                                                                   EL341
00842  0348-NOT-FOUND.                                                  EL341
00843      ADD 1 TO ERROR-NF-COUNT.                                     EL341
00844      MOVE 'NOT FND'              TO P-STATUS.                     EL341
00845      GO TO 0359-EXIT.                                             EL341
00846                                                                   EL341
00847  0349-PMT-DT-ERROR.                                               EL341
00848      MOVE 'NO POST'              TO P-STATUS.                     EL341
00849      MOVE 'PYMT'                 TO ERR-NOTE.                     EL341
00850      MOVE 'X'                    TO DAT-ERR-SW.                   EL341
00851      MOVE AT-PMT-ACCEPT-DT       TO DC-BIN-DATE-1.                EL341
00852      MOVE ' '                    TO DC-OPTION-CODE.               EL341
00853      PERFORM 8500-DATE-CONVERSION THRU 8590-EXIT.                 EL341
00854                                                                   EL341
00855      MOVE DC-GREG-DATE-CYMD      TO WK-DT.                           CL**3
00856      MOVE W-MO                   TO ERR-MO.                       EL341
00857      MOVE W-DA                   TO ERR-DA.                       EL341
00858      MOVE W-YR                   TO ERR-YR.                       EL341
00859      GO TO 0359-EXIT.                                             EL341
00860                                                                   EL341
00861  0350-VOID-DT-ERROR.                                              EL341
00862      MOVE 'NO POST'              TO P-STATUS.                     EL341
00863      MOVE 'VOID'                 TO ERR-NOTE.                     EL341
00864      MOVE 'X'                    TO DAT-ERR-SW.                   EL341
00865      MOVE AT-VOID-ACCEPT-DT      TO DC-BIN-DATE-1.                EL341
00866      MOVE ' '                    TO DC-OPTION-CODE.               EL341
00867      PERFORM 8500-DATE-CONVERSION THRU 8590-EXIT.                 EL341
00868      MOVE DC-GREG-DATE-CYMD      TO WK-DT.                           CL**4
00869      MOVE W-MO                   TO ERR-MO.                       EL341
00870      MOVE W-DA                   TO ERR-DA.                       EL341
00871      MOVE W-YR                   TO ERR-YR.                       EL341
00872      GO TO 0359-EXIT.                                             EL341
00873                                                                   EL341
00874  0359-EXIT.                                                       EL341
00875      EXIT.                                                        EL341
00876      EJECT                                                        EL341
00877                                                                   EL341
00878  0360-G-TOTALS  SECTION.                                          EL341
00879                                                                   EL341
00880      MOVE    'GRAND TOTAL'       TO  HD1-CARR.                    EL341
00881      MOVE    SPACES              TO  HD-CARR.                     EL341
00882      PERFORM 0200-PT-HDNG THRU 0230-ALL-HDNG-E.                   EL341
00883      MOVE    G-TOTCNT            TO  GT-CNT.                      EL341
00884      MOVE    G-TOTDTH            TO  GT-DTH.                      EL341
00885      MOVE    G-TOTDIS            TO  GT-DIS.                      EL341
00886      MOVE    G-T-LN              TO  P-DATA.                      EL341
00887      MOVE    '0'                 TO  X.                           EL341
00888      PERFORM 0320-PRT-RTN THRU 0330-PRT-RTN-XIT.                  EL341
00889                                                                   EL341
00890      MOVE    SPACES              TO  P-DATA.                      EL341
00891      MOVE    G-TOTCNT-R          TO  GTR-CNT.                     EL341
00892      MOVE    G-TOTDTH-R          TO  GTR-DTH.                     EL341
00893      MOVE    G-TOTDIS-R          TO  GTR-DIS.                     EL341
00894      MOVE    G-TR-LN             TO  P-DATA.                      EL341
00895      MOVE    '0'                 TO  X.                           EL341
00896      PERFORM 0320-PRT-RTN THRU 0330-PRT-RTN-XIT.                  EL341
00897                                                                   EL341
00898  0360-END-G-TOTAL.                                                EL341
00899      EXIT.                                                        EL341
00900                                                                   EL341
00901  8500-DATE-CONVERSION.                                            EL341
00902                                                                   EL341
00903      COPY ELCDCS.                                                 EL341
00904                                                                   EL341
00905  ABEND-PGM SECTION. COPY ELCABEND.                                EL341
00906                                                                   EL341
