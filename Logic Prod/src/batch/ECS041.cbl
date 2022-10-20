      $SET ALTER
00001  IDENTIFICATION DIVISION.                                         05/27/98
00002                                                                   ECS041
00003  PROGRAM-ID.                 ECS041.                                 LV053
00004 *              PROGRAM CONVERTED BY                               ECS041
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ECS041
00006 *              CONVERSION DATE 02/08/96 14:29:15.                 ECS041
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          ECS041
00008 *                            VMOD=2.022.                             CL*36
00008 *                            VMOD=2.023 CHANGES ADDED,               CL*36
00008 *                                       FIND ON 'LOGIC'.             CL*36
00009                                                                   ECS041
00010 *AUTHOR.        LOGIC, INC.                                       ECS041
00011 *               DALLAS, TEXAS.                                    ECS041
00012                                                                   ECS041
00013 *DATE-COMPILED.                                                   ECS041
00014                                                                   ECS041
00015 *SECURITY.   *****************************************************ECS041
00016 *            *                                                   *ECS041
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ECS041
00018 *            *                                                   *ECS041
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS041
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ECS041
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *ECS041
00022 *            *                                                   *ECS041
00023 *            *****************************************************ECS041
00024                                                                   ECS041
00025 *REMARKS.                                                         ECS041
00026 *        THIS PROGRAM WILL MAKE ADJUSTMENTS TO THE EP-EC FILE.    ECS041
012303******************************************************************
012303*                   C H A N G E   L O G
012303*
012303* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
012302*-----------------------------------------------------------------
012303*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
012303* EFFECTIVE    NUMBER
012303*-----------------------------------------------------------------
012303* 012303                   PEMA  ADD DCC TO IBNR CALC
040103* 040103   2003040100002  PEMA  FIX FEB DATE PROCESS FOR RETROS
122707* 122707   2007010300003  PEMA  CHANGE LF IBNR PCT TO .00045
122408* 122408 CR2008120300002  PEMA  ADD ADJUSTMENT PROCESSING
032612* 032612 CR2011110200001  PEMA  AHL CHANGES
070714* 070714  CR2013060600001  PEMA  AUTOMATE MONTH END BALANCING
012303******************************************************************
00027                                                                   ECS041
00028  ENVIRONMENT DIVISION.                                            ECS041
00029  INPUT-OUTPUT SECTION.                                            ECS041
00030  FILE-CONTROL.                                                    ECS041
00031                                                                   ECS041
00032      SELECT SORT-WORK        ASSIGN TO SYS001-DA-3380-S-SORTWK1.  ECS041
00033      SELECT INPUT-039        ASSIGN TO SYS002-UT-3380-S-SYS002.   ECS041
00034      SELECT SCS041-WORK      ASSIGN TO SYS003-UT-3380-S-SYS003.   ECS041
00035      SELECT DISK-INPUT       ASSIGN TO SYS004-UT-3380-S-SYS004.   ECS041
00036      SELECT PRNTR            ASSIGN TO SYS008-UR-1403-S-SYS008.   ECS041
00037      SELECT OLD-EPEC         ASSIGN TO SYS010-UT-2400-S-SYS010.   ECS041
00038      SELECT NEW-EPEC         ASSIGN TO SYS011-UT-2400-S-SYS011.   ECS041
00039      SELECT TAPE-IN          ASSIGN TO SYS012-UT-2400-S-SYS012.   ECS041
00040      SELECT ACCT-MSTR        ASSIGN TO SYS015-3380-ERACCTT        ECS041
00041                              ACCESS IS SEQUENTIAL                 ECS041
00042                              ORGANIZATION IS INDEXED              ECS041
00043                              FILE STATUS IS AM-FILE-STATUS        ECS041
00044                              RECORD KEY IS AM-CONTROL-PRIMARY.    ECS041
00045      SELECT DISK-DATE        ASSIGN TO SYS019-UT-3380-S-SYS019.   ECS041
00046      SELECT FICH             ASSIGN TO SYS020-UT-2400-S-SYS020.   ECS041
00047      SELECT ERMEBL           ASSIGN TO SYS024-3380-ERMEBL         ECS041
00048                              ORGANIZATION INDEXED                 ECS041
00049                              ACCESS DYNAMIC                       ECS041
00050                              RECORD KEY ME-CONTROL-PRIMARY        ECS041
00051                              FILE STATUS ERMEBL-FILE-STATUS.      ECS041
00052  EJECT                                                            ECS041
00053  DATA DIVISION.                                                   ECS041
00054  FILE SECTION.                                                    ECS041
00055                                                                   ECS041
00056  SD  SORT-WORK.                                                      CL**5
00057                                                                      CL**5
00058  01  SW-REC.                                                      ECS041
00059      12  SW-CONTROL-1.                                            ECS041
00060          16  SW-CNTRL-A.                                          ECS041
00061              20  SW-CARR     PIC X.                               ECS041
00062              20  SW-COMP     PIC X(6).                            ECS041
00063              20  SW-STATE    PIC XX.                              ECS041
00064              20  SW-ACCT     PIC X(10).                           ECS041
00065          16  SW-EXP-DT       PIC 9(11)  COMP-3.                      CL**5
00066          16  SW-EFF-DT       PIC 9(11)  COMP-3.                      CL**5
00067          16  SW-REI-CO       PIC X(6).                            ECS041
00068      12  SW-CONTROL-2.                                            ECS041
00069          16  SW-LF-AH        PIC X.                               ECS041
00070          16  SW-BEN-TYPE     PIC XX.                              ECS041
00071      12  SW-CARD-TYPE        PIC X.                               ECS041
00072      12  SW-RESV             PIC S9(9)V99     COMP-3.             ECS041
00073      12  SW-REM-AMT          PIC S9(9)V99     COMP-3.             ECS041
00074      12  SW-FUT-RESERVE      PIC S9(7)V99     COMP-3.             ECS041
00075      12  SW-PTC-RESERVE      PIC S9(7)V99     COMP-3.             ECS041
00076      12  SW-IBNR-RESERVE     PIC S9(7)V99     COMP-3.             ECS041
00077      12  SW-CLM-ADJ-AMT      PIC S9(7)V99     COMP-3.             ECS041
00078      12  SW-EXPENSES         PIC S9(7)V99     COMP-3.             ECS041
00079      12  SW-PAYMENTS         PIC S9(7)V99     COMP-3.             ECS041
00080      12  SW-OTH-COMMISSIONS  PIC S9(7)V99     COMP-3.             ECS041
00081      12  SW-REIN-PREM-ADJS   PIC S9(7)V99     COMP-3.             ECS041
00082      12  SW-A-DATE           PIC 9(07)        COMP-3.                CL**6
00083      12  SW-COMPANY-CD       PIC X.                               ECS041
00084  EJECT                                                            ECS041
00085  FD  INPUT-039                                                    ECS041
00086      BLOCK CONTAINS 0 RECORDS
00087      RECORDING MODE F.                                               CL*49
00088                                                                      CL*49
00089  01  INPUT-039-REC           PIC X(92).                           ECS041
00090                                                                   ECS041
00091  FD  SCS041-WORK                                                  ECS041
00092      BLOCK CONTAINS 0 RECORDS
00093      RECORDING MODE F.                                               CL*49
00094                                                                      CL*49
00095  01  WORK-REC                PIC X(98).                           ECS041
00096                                                                   ECS041
00097  FD  DISK-INPUT                                                   ECS041
00098      BLOCK CONTAINS 0 RECORDS
00099      RECORDING MODE F.                                               CL*49
00100                                                                      CL*49
00101  01  DISK-REC                PIC X(92).                           ECS041
00102  EJECT                                                            ECS041
00103  FD  PRNTR                                                        ECS041
00104                              COPY ELCPRTFD.                       ECS041
00105  EJECT                                                            ECS041
00106  FD  OLD-EPEC                                                     ECS041
00107      BLOCK CONTAINS 0 RECORDS
00108      RECORDING MODE F.                                               CL*49
00109                                                                      CL*49
00110  01  OE-REC.                                                      ECS041
00111      12  OE-RECORD-ID        PIC XX.                              ECS041
00112      12  FILLER              PIC XX.                              ECS041
00113      12  OE-CCSAD.                                                ECS041
00114          16  OE-CARR         PIC X.                               ECS041
00115          16  OE-COMP         PIC X(6).                            ECS041
00116          16  OE-STATE        PIC XX.                              ECS041
00117          16  OE-ACCT         PIC X(10).                           ECS041
00118          16  OE-DATES        PIC X(12).                           ECS041
00119      12  OE-REIN             PIC X(6).                            ECS041
00120      12  OE-LFAH-BEN.                                             ECS041
00121          16  OE-LAH          PIC X.                               ECS041
00122          16  OE-BEN-TYP      PIC XX.                              ECS041
00123      12  FILLER              PIC X(93).                           ECS041
00124      12  OE-CLM-DU           PIC S9(7)V99   COMP-3.               ECS041
00125      12  OE-CLM-PV           PIC S9(7)V99   COMP-3.               ECS041
00126      12  OE-CLM-IBNR         PIC S9(7)V99   COMP-3.               ECS041
00127      12  OE-LOSS-RESV        PIC S9(7)V99   COMP-3.               ECS041
00128      12  FILLER              PIC X(20).                           ECS041
00129      12  OE-MORT-RESV        PIC S9(11)V9(6) COMP-3.              ECS041
00130      12  OE-IN-FORCE         PIC S9(11)V99   COMP-3.              ECS041
00131      12  FILLER              PIC X(125).                          ECS041
00132      12  OE-PURGE            PIC X.                               ECS041
00133      12  OE-RUN-DT           PIC 9(11)      COMP-3.               ECS041
00134  EJECT                                                            ECS041
00135  FD  NEW-EPEC                                                     ECS041
00136      COPY ECSEPCFD.                                               ECS041
00137      COPY ECSEPC01.                                               ECS041
00138  EJECT                                                            ECS041
00139  FD  TAPE-IN                                                      ECS041
00140      BLOCK CONTAINS 0 RECORDS
00141      RECORDING MODE F.                                               CL*49
00142                                                                      CL*49
00143  01  TAPE-IN-REC             PIC X(200).                          ECS041
00144                                                                   ECS041
00145  FD  ACCT-MSTR.                                                      CL*49
00146                                                                      CL*49
00147      COPY ERCACCT.                                                ECS041
00148  EJECT                                                            ECS041
00149  FD  DISK-DATE                                                    ECS041
00150      COPY ELCDTEFD.                                               ECS041
00151                                                                   ECS041
00152  FD  FICH                                                         ECS041
00153      COPY ECSFICH.                                                ECS041
00154  EJECT                                                            ECS041
00155  FD  ERMEBL.                                                         CL*49
00156                                                                      CL*49
00157      COPY ERCMEBL.                                                ECS041
00158  EJECT                                                            ECS041
00159  WORKING-STORAGE SECTION.                                         ECS041
00160  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      ECS041
00161  77  FILLER  PIC X(32) VALUE '********************************'.  ECS041
00162  77  FILLER  PIC X(32) VALUE '     ECS041 WORKING STORAGE     '.  ECS041
00163  77  FILLER  PIC X(32) VALUE '******** VMOD=2.022 ************'.     CL*36
00164                                                                   ECS041
00163  77  FINDIT  PIC X(32) VALUE ' '.                                    CL*36
       77  WS-DIS-AMT                  PIC -9(7).99 VALUE ZEROS.
00164                                                                   ECS041
00165  01  MONTH-END-DATA.                                              ECS041
00166      12  ME-START-DATE.                                           ECS041
00167          16  ME-START-MO         PIC 99.                          ECS041
00168          16  FILLER              PIC X.                           ECS041
00169          16  ME-START-DA         PIC 99.                          ECS041
00170          16  FILLER              PIC X.                           ECS041
00171          16  ME-START-YR         PIC 99.                          ECS041
00172      12  ME-CNDS-DATE            PIC 9(6).                        ECS041
00173      12  ME-CNDS-DATE-R REDEFINES ME-CNDS-DATE.                   ECS041
00174          16  ME-CNDS-MO          PIC 99.                          ECS041
00175          16  ME-CNDS-DA          PIC 99.                          ECS041
00176          16  ME-CNDS-YR          PIC 99.                          ECS041
00177      12  ME-START-TIME           PIC 9(6).                        ECS041
00178      12  ME-UPDATE-FLAG          PIC X           VALUE 'Y'.       ECS041
00179          88  ME-DO-UPDATE                        VALUE 'Y'.       ECS041
00180          88  ME-NO-UPDATE                        VALUE 'N'.       ECS041
00181      12  ERMEBL-FILE-STATUS      PIC XX.                          ECS041
00182      12  AM-FILE-STATUS          PIC XX.                          ECS041
00183      12  MONTH-END-MOYR          PIC S9(5) COMP-3.                ECS041
00184                                                                      CL*40
00185      12  WS-SW-EXP-DT.                                               CL**5
00186          16  FILLER        PIC 999.                                  CL**5
00187          16  SW-EXP-CCYY   PIC 9(04).                                CL**5
00188          16  SW-EXP-CCYR REDEFINES SW-EXP-CCYY.                      CL**5
00189              24  SW-EXP-CC PIC 99.                                   CL**5
00190              24  SW-EXP-YR PIC 99.                                   CL**5
00191          16  SW-EXP-MO     PIC 99.                                   CL**5
00192          16  SW-EXP-DA     PIC 99.                                   CL**5
00193      12  WS-SW-EXP-DT-N REDEFINES                                    CL**5
00194            WS-SW-EXP-DT    PIC 9(11).                                CL**5
00195                                                                      CL*40
00196      12  WS-OE-RUN-DATE.                                             CL*48
00197          16  FILLER          PIC 999.                                CL*48
00198          16  OE-RUN-CCYY     PIC 9(04).                              CL*48
00199          16  OE-RUN-CCYR REDEFINES OE-RUN-CCYY.                      CL*48
00200              20  OE-RUN-CC   PIC 99.                                 CL*48
00201              20  OE-RUN-YR   PIC 99.                                 CL*48
00202          16  OE-RUN-MO       PIC 99.                                 CL*48
00203          16  OE-RUN-DA       PIC 99.                                 CL*48
00204      12  WS-OE-RUN-DT-N REDEFINES                                    CL*48
00205             WS-OE-RUN-DATE  PIC 9(11).                               CL*48
00206                                                                      CL*48
00207      12  WS-DATE-CCYYMM.                                             CL*48
00208          16  FILLER          PIC 9.                                  CL*45
00209          16  WS-DT-CCYY      PIC 9(04).                              CL*48
00210          16  WS-DT-CCYR REDEFINES WS-DT-CCYY.                        CL*48
00211              20  WS-DT-CC    PIC 99.                                 CL*48
00212              20  WS-DT-YR    PIC 99.                                 CL*48
00213          16  WS-DT-MO        PIC 99.                                 CL*48
00214      12  WS-DATE-CCYYMM-N  REDEFINES                                 CL*48
00215             WS-DATE-CCYYMM   PIC 9(07).                              CL*48
00216                                                                      CL*45
00217  01  HLD-041-RETROS              PIC S9(9)V99 COMP-3 VALUE ZERO.  ECS041
00218  01  HLD-041-REIN-ADJ            PIC S9(9)V99 COMP-3 VALUE ZERO.     CL*36
00219                                                                   ECS041
00220      COPY ELCDATE.                                                   CL*51
00221      EJECT                                                        ECS041
00222                                                                   ECS041
00223  01  MONTH-TABLE.                                                 ECS041
00224      12  FILLER              PIC X(24)           VALUE            ECS041
00225              '312831303130313130313031'.                          ECS041
00226                                                                   ECS041
00227  01  MONTH-TBL       REDEFINES MONTH-TABLE.                       ECS041
00228      12  FILLER          OCCURS 12 TIMES.                         ECS041
00229          16  MONTH-D         PIC  99.                             ECS041
00230                                                                   ECS041
00231  01  MISC.                                                        ECS041
00232      12  WS-RETURN-CODE      PIC S9(4)    COMP.                   ECS041
00233      12  WS-ABEND-MESSAGE    PIC X(80).                           ECS041
00234      12  WS-ABEND-FILE-STATUS PIC XX.                             ECS041
00235      12  WS-ZERO             PIC S9       COMP-3  VALUE +0.       ECS041
00236      12  CARD-EOF-SW         PIC X               VALUE SPACE.     ECS041
00237      12  SA                  PIC S999    COMP.                    ECS041
00238      12  X1                  PIC S999    COMP.                    ECS041
00239      12  X2                  PIC S999    COMP.                    ECS041
00240      12  X                   PIC X               VALUE ' '.       ECS041
00241      12  PGM-SUB             PIC S999    COMP-3  VALUE +41.       ECS041
00242      12  SUBA                PIC S999            VALUE +0.        ECS041
00243      12  PAGE-CNT            PIC S9(5)   COMP-3  VALUE ZERO.      ECS041
00244      12  LINE-CNT            PIC S999    COMP-3  VALUE +99.       ECS041
00245      12  WS-OE-CTL.                                               ECS041
00246          16  WS-OE-CCSAD     PIC X(31).                           ECS041
00247          16  WS-OE-REIN      PIC X(6).                            ECS041
00248          16  WS-OE-RUN-DT    PIC 9(07)  COMP-3.                      CL**6
00249          16  WS-OE-LFAH-BEN  PIC XXX.                             ECS041
00250      12  WS-SW-CTL.                                               ECS041
00251          16  WS-SW-CCSA.                                          ECS041
00252              20  WS-SW-CARR  PIC X.                               ECS041
00253              20  WS-SW-GROUP PIC X(6).                            ECS041
00254              20  WS-SW-STATE PIC XX.                              ECS041
00255              20  WS-SW-ACCT  PIC X(10).                           ECS041
00256          16  WS-SW-EXP       PIC 9(11) COMP-3.                       CL**6
00257          16  WS-SW-EFF       PIC 9(11) COMP-3.                       CL**6
00258          16  WS-SW-REIN      PIC X(6).                            ECS041
00259          16  WS-SW-DATE      PIC 9(07) COMP-3.                       CL**4
00260          16  WS-SW-LFAH      PIC X.                               ECS041
00261          16  WS-SW-BENTP     PIC XX.                              ECS041
00262      12  WS-SW-COMPANY-CD    PIC X(44).                           ECS041
00263      12  WS-SAVE-SW-CTL      PIC X(44).                           ECS041
00264      12  ERROR-FLAG          PIC X               VALUE ' '.       ECS041
00265      12  ITG-LIFE-TYPE       PIC XX.                              ECS041
00266          88  ITG-OB-COVERAGE              VALUE '39' THRU '73'.   ECS041
00267                                                                      CL*40
00268  EJECT                                                            ECS041
00269      COPY ERCREPY.                                                ECS041
00270      EJECT                                                        ECS041
00271  01  ADJUST-CARD.                                                 ECS041
00272      12  AC-COMPANY-CD       PIC X.                               ECS041
00273      12  AC-CARR             PIC X.                               ECS041
00274      12  AC-COMP             PIC X(6).                            ECS041
00275      12  AC-STATE            PIC XX.                              ECS041
00276      12  AC-ACCT             PIC X(10).                           ECS041
00277      12  AC-DATE             PIC 9(11)     COMP-3.                   CL*19
00278      12  AC-LF-AH            PIC X.                               ECS041
00279      12  AC-BEN-TYPE         PIC XX.                              ECS041
00280      12  AC-REI-CO           PIC X(6).                            ECS041
00281      12  AC-CARD-TYPE        PIC X.                               ECS041
00282      12  AC-RESV             PIC S9(9)V99    COMP-3.              ECS041
00283      12  AC-REM-AMT          PIC S9(9)V99    COMP-3.              ECS041
00284      12  AC-FUT-RESERVE      PIC S9(7)V99    COMP-3.              ECS041
00285      12  AC-PTC-RESERVE      PIC S9(7)V99    COMP-3.              ECS041
00286      12  AC-IBNR-RESERVE     PIC S9(7)V99    COMP-3.              ECS041
00287      12  AC-CLM-ADJ-AMT      PIC S9(7)V99    COMP-3.              ECS041
00288      12  AC-EXPENSES         PIC S9(7)V99    COMP-3.              ECS041
00289      12  AC-PAYMENTS         PIC S9(7)V99    COMP-3.              ECS041
00290      12  AC-OTH-COMMISSIONS  PIC S9(7)V99    COMP-3.              ECS041
00291      12  AC-REIN-PREM-ADJS   PIC S9(7)V99    COMP-3.              ECS041
00292      12  AC-A-PDATE          PIC 9(07)       COMP-3.                 CL*21
00293                                                                   ECS041
00294  01  ERROR-MESSAGES.                                              ECS041
00295      12  FILLER              PIC X(35)           VALUE            ECS041
00296              ' INVALID BENEFIT TYPE   '.                          ECS041
00297      12  FILLER              PIC X(35)           VALUE            ECS041
00298              ' CARD CODE NOT 1, 2, OR 3'.                         ECS041
00299      12  FILLER              PIC X(35)           VALUE            ECS041
00300              ' AMOUNT FIELDS NOT NUMERIC'.                        ECS041
00301      12  FILLER              PIC X(35)           VALUE            ECS041
00302              ' NO MATCHING ACCOUNT MASTER'.                       ECS041
00303      12  FILLER              PIC X(35)           VALUE            ECS041
00304              ' INVALID BENEFIT TYPE'.                             ECS041
00305      12  FILLER              PIC X(35)           VALUE            ECS041
00306              ' ADJUSTMENT MONTH NOT VALID'.                       ECS041
00307      12  FILLER              PIC X(35)           VALUE            ECS041
00308              ' ADJUSTMENT YEAR NOT VALID'.                        ECS041
00309      12  FILLER              PIC X(35)           VALUE            ECS041
00310              HIGH-VALUES.                                         ECS041
00311                                                                   ECS041
00312  01  ERR-MSGS        REDEFINES ERROR-MESSAGES.                    ECS041
00313      12  ERROR-MSG       OCCURS 7.                                ECS041
00314          16  ERR-IND         PIC X.                               ECS041
00315          16  FILLER          PIC X(34).                           ECS041
00316  EJECT                                                            ECS041
00317  01  PRT-LINES.                                                   ECS041
00318      12  HDR-1.                                                   ECS041
00319          16  FILLER          PIC X(42)           VALUE SPACES.    ECS041
00320          16  FILLER          PIC X(78)           VALUE            ECS041
00321                  'MISCELLANEOUS ADJUSTMENTS POSTING SUMMARY'.     ECS041
00322          16  FILLER          PIC X(8)            VALUE 'ECS041'.  ECS041
00323      12  HDR-2.                                                   ECS041
00324          16  FILLER          PIC X(47)           VALUE SPACES.    ECS041
00325          16  H2-COMP         PIC X(30).                           ECS041
00326          16  FILLER          PIC X(43)           VALUE SPACES.    ECS041
00327          16  H2-DATE         PIC X(8).                            ECS041
00328      12  HDR-3.                                                   ECS041
00329          16  FILLER          PIC X(53)           VALUE SPACES.    ECS041
00330          16  H3-DATE         PIC X(18).                           ECS041
00331          16  FILLER          PIC X(49)           VALUE SPACES.    ECS041
00332          16  FILLER          PIC X(5)            VALUE 'PAGE'.    ECS041
00333          16  H3-PAGE         PIC ZZ,ZZ9.                          ECS041
00334      12  DTL-1.                                                   ECS041
00335          16  FILLER          PIC X.                               ECS041
00336          16  D1-REIN         PIC X(6).                            ECS041
00337          16  FILLER          PIC X.                               ECS041
00338          16  D1-CARR         PIC X.                               ECS041
00339          16  FILLER          PIC X.                               ECS041
00340          16  D1-COMP         PIC X(6).                            ECS041
00341          16  FILLER          PIC X.                               ECS041
00342          16  D1-STATE        PIC XX.                              ECS041
00343          16  FILLER          PIC X.                               ECS041
00344          16  D1-ACCT         PIC X(10).                           ECS041
00345          16  FILLER          PIC X.                               ECS041
00346          16  D1-EXP-MO       PIC XX.                              ECS041
00347          16  D1-EXP-DA       PIC XX.                              ECS041
00348          16  D1-EXP-YR       PIC XX.                              ECS041
00349          16  FILLER          PIC X.                               ECS041
00350          16  D1-LF-AH        PIC X.                               ECS041
00351          16  FILLER          PIC X.                               ECS041
00352          16  D1-BEN-TYPE     PIC XX.                              ECS041
00353          16  FILLER          PIC X.                               ECS041
00354          16  D1-RUN-MO       PIC XX.                              ECS041
00355          16  D1-RUN-DA       PIC XX.                              ECS041
00356          16  D1-RUN-YR       PIC XX.                              ECS041
00357          16  FILLER          PIC X.                               ECS041
00358          16  D1-CARD-TYPE    PIC X.                               ECS041
00359          16  FILLER          PIC X.                               ECS041
00360          16  FILLER          PIC X.                               ECS041
00361          16  D1-PMO          PIC XX.                              ECS041
00362          16  FILLER          PIC X.                               ECS041
00363          16  D1-PYR          PIC XX.                              ECS041
00364          16  FILLER          PIC X.                               ECS041
00365          16  D1-MSG          PIC X(35).                           ECS041
00366      12  D1-DASHES.                                               ECS041
00367          16  FILLER          PIC X(50)           VALUE            ECS041
00368              '       - -      -  -          -      - -  -      -'.ECS041
00369          16  FILLER          PIC X(9)            VALUE            ECS041
00370              ' -   -  -'.                                         ECS041
00371      12  TOT-1.                                                   ECS041
00372          16  FILLER          PIC X.                               ECS041
00373          16  T1-DESC         PIC X(36).                           ECS041
00374          16  FILLER          PIC XXX             VALUE SPACES.    ECS041
00375          16  T1-CTR          PIC ZZZ,ZZZ,ZZ9-.                    ECS041
00376      12  TOT-2.                                                   ECS041
00377          16  FILLER          PIC X.                               ECS041
00378          16  FILLER          PIC X(36)           VALUE            ECS041
00379                  'MORTALITY RESERVE.................. '.          ECS041
00380          16  T2-MORT         PIC ZZ,ZZZ,ZZZ,ZZZ.999999.           ECS041
00381      12  TOT-3.                                                   ECS041
00382          16  FILLER          PIC X.                               ECS041
00383          16  T3-DESC         PIC X(36).                           ECS041
00384          16  T3-ACCUM        PIC ZZ,ZZZ,ZZZ,ZZZ.99.               ECS041
00385  EJECT                                                            ECS041
00386  01  CTR-DESCRIPTIONS.                                            ECS041
00387      12  FILLER              PIC X(35)           VALUE            ECS041
00388              'EPEC RECORDS READ..................'.               ECS041
00389      12  FILLER              PIC X(35)           VALUE            ECS041
00390              'EPEC RECORDS WRITTEN...............'.               ECS041
00391      12  FILLER              PIC X(35)           VALUE            ECS041
00392              'ADJUSTMENT TRANSACTIONS READ.......'.               ECS041
00393      12  FILLER              PIC X(35)           VALUE            ECS041
00394              'ADJUSTMENT TRANSACTIONS POSTED.....'.               ECS041
00395      12  FILLER              PIC X(35)           VALUE            ECS041
00396              'ADJUSTMENT TRANSACTIONS REJECTED...'.               ECS041
00397      12  FILLER              PIC X(35)           VALUE            ECS041
00398              'CLAIM COUNT........................'.               ECS041
00399      12  FILLER              PIC X(35)           VALUE            ECS041
00400              'CLAIM CERTIFICATE COUNT............'.               ECS041
00401      12  FILLER              PIC X(35)           VALUE            ECS041
00402              'ISSUED CERTIFICATE COUNT...........'.               ECS041
00403      12  FILLER              PIC X(35)           VALUE            ECS041
00404              'CANCELED CERTIFICATE COUNT.........'.               ECS041
00405                                                                   ECS041
00406  01  CTR-DESC-R      REDEFINES CTR-DESCRIPTIONS.                  ECS041
00407      12  CTR-DESC-R      OCCURS 9.                                ECS041
00408          16  CTR-DESC        PIC X(35).                           ECS041
00409  EJECT                                                            ECS041
00410  01  ACCUMULATOR-DESCRIPTION.                                     ECS041
00411      12  FILLER              PIC X(35)           VALUE            ECS041
00412              'INSURANCE IN FORCE.................'.               ECS041
00413      12  FILL1.                                                   ECS041
00414          16  ACCUM-DIS-OVER      PIC X(12)       VALUE            ECS041
00415              '            '.                                      ECS041
00416          16  FILLER              PIC X(23)       VALUE            ECS041
00417              ' IN FORCE..............'.                           ECS041
00418      12  FILLER              PIC X(35)           VALUE            ECS041
00419              'CLAIMS - DUE AND UNPAID............'.               ECS041
00420      12  FILLER              PIC X(35)           VALUE            ECS041
00421              'CLAIMS - PV........................'.               ECS041
00422      12  FILLER              PIC X(35)           VALUE            ECS041
00423              'CLAIMS - IBNR......................'.               ECS041
00424      12  FILLER              PIC X(35)           VALUE            ECS041
00425              'LOSS-RESERVE.......................'.               ECS041
00426      12  FILLER              PIC X(35)           VALUE            ECS041
00427              'CLAIM ADJUSTMENTS..................'.               ECS041
00428      12  FILLER              PIC X(35)           VALUE            ECS041
00429              'RETRO EXPENSES.....................'.               ECS041
00430      12  FILLER              PIC X(35)           VALUE            ECS041
00431              'RETRO PAYMENTS.....................'.               ECS041
00432      12  FILLER              PIC X(35)           VALUE            ECS041
00433              'RETRO OTHER COMMISSION.............'.               ECS041
00434      12  FILLER              PIC X(35)           VALUE            ECS041
00435              'REINSURANCE ADJUSTMENTS............'.               ECS041
00436      12  FILLER              PIC X(35)           VALUE            ECS041
00437              'ISSUED PREMIUM.....................'.               ECS041
00438      12  FILLER              PIC X(35)           VALUE            ECS041
00439              'CANCELED PREMIUM...................'.               ECS041
00440      12  FILLER              PIC X(35)           VALUE            ECS041
00441              'EARNED PREMIUM - RULE-78...........'.               ECS041
00442      12  FILLER              PIC X(35)           VALUE            ECS041
00443              'EARNED PREMIUM - PRO-RATA..........'.               ECS041
00444      12  FILLER              PIC X(35)           VALUE            ECS041
00445              'CLAIMS PAID........................'.               ECS041
00446      12  FILLER              PIC X(35)           VALUE            ECS041
00447              'ISSUED BENEFIT.....................'.               ECS041
00448      12  FILLER              PIC X(35)           VALUE            ECS041
00449              'CANCELED BENEFIT...................'.               ECS041
00450                                                                   ECS041
00451  01  ACCUMULATOR-DESC-R  REDEFINES ACCUMULATOR-DESCRIPTION.       ECS041
00452      12  ACCUMULATOR-DESC  OCCURS 18.                             ECS041
00453          16  ACCUM-DESC      PIC X(35).                           ECS041
00454  EJECT                                                            ECS041
00455  01  CNTRS       COMP-3.                                          ECS041
00456      12  EPEC-IN             PIC S9(9)           VALUE ZERO.      ECS041
00457      12  EPEC-OUT            PIC S9(9)           VALUE ZERO.      ECS041
00458      12  TRAN-IN             PIC S9(9)           VALUE ZERO.      ECS041
00459      12  TRAN-POST           PIC S9(9)           VALUE ZERO.      ECS041
00460      12  TRAN-REJ            PIC S9(9)           VALUE ZERO.      ECS041
00461      12  CLM-CNT             PIC S9(9)           VALUE ZERO.      ECS041
00462      12  CLM-CRT             PIC S9(9)           VALUE ZERO.      ECS041
00463      12  ISS-CNT             PIC S9(9)           VALUE ZERO.      ECS041
00464      12  CNC-CNT             PIC S9(9)           VALUE ZERO.      ECS041
00465                                                                   ECS041
00466  01  CNTRS-R         REDEFINES CNTRS.                             ECS041
00467      12  CTRS-R          OCCURS 9    COMP-3.                      ECS041
00468          16  CTRS            PIC S9(9).                           ECS041
00469                                                                   ECS041
00470  01  ACCUMULATORS    COMP-3.                                      ECS041
00471      12  IN-FORCE            PIC S9(11)V99       VALUE ZERO.      ECS041
00472      12  AH-IN-FORCE         PIC S9(11)V99       VALUE ZERO.      ECS041
00473      12  CLM-DU              PIC S9(11)V99       VALUE ZERO.      ECS041
00474      12  CLM-PV              PIC S9(11)V99       VALUE ZERO.      ECS041
00475      12  CLM-IBNR            PIC S9(11)V99       VALUE ZERO.      ECS041
00476      12  LOSS-RESV           PIC S9(11)V99       VALUE ZERO.      ECS041
00477      12  CLAIM-ADJ           PIC S9(11)V99       VALUE ZERO.      ECS041
00478      12  RETRO-EXPENSES      PIC S9(11)V99       VALUE ZERO.      ECS041
00479      12  RETRO-PAYMENTS      PIC S9(11)V99       VALUE ZERO.      ECS041
00480      12  RETRO-OTH-COMM      PIC S9(11)V99       VALUE ZERO.      ECS041
00481      12  REIN-ADJUST         PIC S9(11)V99       VALUE ZERO.      ECS041
00482      12  ISS-PRM             PIC S9(11)V99       VALUE ZERO.      ECS041
00483      12  CNC-PRM             PIC S9(11)V99       VALUE ZERO.      ECS041
00484      12  PRM-78              PIC S9(11)V99       VALUE ZERO.      ECS041
00485      12  PRM-PR              PIC S9(11)V99       VALUE ZERO.      ECS041
00486      12  CLM-AMT             PIC S9(11)V99       VALUE ZERO.      ECS041
00487      12  ISS-BEN             PIC S9(11)V99       VALUE ZERO.      ECS041
00488      12  CNC-BEN             PIC S9(11)V99       VALUE ZERO.      ECS041
00489                                                                   ECS041
00490  01  ACCUMULATORS-R  REDEFINES ACCUMULATORS.                      ECS041
00491      12  ACCUMS-R        OCCURS 18   COMP-3.                      ECS041
00492          16  ACCUMS          PIC S9(11)V99.                       ECS041
00493                                                                   ECS041
00494  01  MORT-RESV               PIC S9(11)V9(6) COMP-3  VALUE ZERO.  ECS041
00495                                                                   ECS041
00496      COPY ELCDTECX.                                                  CL*39
00497      COPY ELCDTEVR.                                                  CL*39
00498      COPY ELCACCTV.                                                  CL*39
00499      COPY ELCEPCVR.                                                  CL*39
LOGIC *    COPY ELCREPYV.                                                  CL*39
00501  EJECT                                                            ECS041
00502  PROCEDURE DIVISION.                                              ECS041
00503                                                                   ECS041
00504  0000-STANDARD-ROUTINE SECTION.                                   ECS041
00505                                                                   ECS041
00506      DISPLAY '****** THE FOLLOWING MESSAGES WERE CREATED BY '     ECS041
00507          'ECS041 ******'.                                         ECS041
00508                                                                   ECS041
00509  0001-READ-DATE-CARD.                                             ECS041
00510                              COPY ELCDTERX SUPPRESS.              ECS041
pemuni     open output scs041-work.
00511                                                                   ECS041
00512      MOVE WS-TIME                TO ME-START-TIME.                ECS041
00513      MOVE WS-CURRENT-DATE        TO ME-START-DATE.                ECS041
00514      MOVE ME-START-MO            TO ME-CNDS-MO.                   ECS041
00515      MOVE ME-START-DA            TO ME-CNDS-DA.                   ECS041
00516      MOVE ME-START-YR            TO ME-CNDS-YR.                   ECS041
00517                                                                   ECS041
00518      MOVE WS-CURRENT-DATE        TO H2-DATE.                      ECS041
00519      MOVE ALPH-DATE              TO H3-DATE.                      ECS041
00520      MOVE COMPANY-NAME           TO H2-COMP.                      ECS041
00521                                                                   ECS041
00522      MOVE AH-OVERRIDE-L12        TO ACCUM-DIS-OVER.               ECS041
00523                                                                   ECS041
00524      OPEN OUTPUT PRNTR.                                           ECS041
00525  EJECT                                                            ECS041
00526  0120-SORT-ROUTINE-1 SECTION.                                     ECS041
00527                                                                   ECS041
00528  0130-SORT-RTN-1.                                                 ECS041
00529      SORT SORT-WORK ON ASCENDING SW-CARR                          ECS041
00530                                  SW-COMP                          ECS041
00531                                  SW-STATE                         ECS041
00532                                  SW-ACCT                          ECS041
00533                                  SW-EXP-DT                        ECS041
00534          INPUT PROCEDURE 0160-SORT-INPUT THRU 0310-EXIT           ECS041
00535          OUTPUT PROCEDURE 0320-MATCH-TO-ACCT-MSTR                 ECS041
00536                           THRU 0390-EXIT.                         ECS041
00537                                                                   ECS041
00538      IF SORT-RETURN NOT = ZEROS                                   ECS041
00539         MOVE 0101                  TO WS-RETURN-CODE              ECS041
00540         MOVE ' ERROR IN SORT ONE ' TO WS-ABEND-MESSAGE            ECS041
00541         GO TO ABEND-PGM.                                          ECS041
00542      GO TO 0150-SORT-RTN-2.                                       ECS041
00543  EJECT                                                            ECS041
00544  0140-SORT-ROUTINE-2 SECTION.                                     ECS041
00545                                                                   ECS041
00546  0150-SORT-RTN-2.
PEMUNI     OPEN INPUT SCS041-WORK.
00547      SORT SORT-WORK ON ASCENDING SW-CONTROL-1                     ECS041
00548                                  SW-A-DATE                           CL**8
00549                                  SW-CONTROL-2                     ECS041
00550        INPUT  PROCEDURE 0400-SCS-WORK-RTN     THRU 0440-EXIT      ECS041
00551        OUTPUT PROCEDURE 0450-POST-ADJUSTMENTS THRU 0590-EXIT.     ECS041
00552                                                                   ECS041
00553      IF SORT-RETURN NOT = ZEROS                                   ECS041
00554         MOVE 0102    TO WS-RETURN-CODE                            ECS041
00555         MOVE ' ERROR IN SORT TWO ' TO WS-ABEND-MESSAGE            ECS041
00556         GO TO ABEND-PGM.                                          ECS041
00557                                                                   ECS041
00558      GO TO 0760-E-O-J.                                            ECS041
00559  EJECT                                                            ECS041
00560  0160-SORT-INPUT SECTION.                                         ECS041
00561                                                                   ECS041
00562  0170-OPEN-INPUT-1.                                               ECS041
00563      OPEN INPUT TAPE-IN.                                          ECS041
00564      IF DTE-PGM-OPT = '1' OR '4'                                  ECS041
00565          OPEN INPUT DISK-INPUT                                    ECS041
00566          GO TO 0200-READ-INPUT.                                   ECS041
00567                                                                   ECS041
00568  0180-ALTER-INPUT.                                                ECS041
00569      IF DTE-PGM-OPT = '3' OR '4'                                  ECS041
00570          ALTER 0200-READ-INPUT TO PROCEED TO 0220-READ-INPUT-039  ECS041
00571          OPEN INPUT INPUT-039                                     ECS041
00572          GO TO 0200-READ-INPUT.                                   ECS041
00573                                                                   ECS041
00574  0190-ALTER-INPUT-1.                                              ECS041
00575      ALTER 0200-READ-INPUT TO PROCEED TO 0230-READ-TAPE-IN.       ECS041
00576                                                                   ECS041
00577  0200-READ-INPUT.                                                 ECS041
00578      GO TO 0210-READ-DISK.                                        ECS041
00579                                                                   ECS041
00580  0210-READ-DISK.                                                  ECS041
00581      READ DISK-INPUT INTO ADJUST-CARD AT END                      ECS041
00582          CLOSE DISK-INPUT                                         ECS041
00583          GO TO 0180-ALTER-INPUT.                                  ECS041
00584                                                                   ECS041
00585      GO TO 0240-PROCESS-TRANSACTION.                              ECS041
00586                                                                   ECS041
00587  0220-READ-INPUT-039.                                             ECS041
00588      READ INPUT-039 INTO ADJUST-CARD AT END                       ECS041
00589          CLOSE INPUT-039                                          ECS041
00590          GO TO 0190-ALTER-INPUT-1.                                ECS041
00591                                                                   ECS041
00592      IF  DTE-CLIENT = 'NCL'                                       ECS041
00593          MOVE ZEROS                 TO AC-IBNR-RESERVE.           ECS041
00594                                                                   ECS041
00595      GO TO 0240-PROCESS-TRANSACTION.                              ECS041
00596                                                                   ECS041
00597  0230-READ-TAPE-IN.                                               ECS041
00598      READ TAPE-IN INTO PENDING-RETRO-REIN-ADJUSTMENTS AT END      ECS041
00599         GO TO 0300-END-INPUT-1.                                   ECS041
00600                                                                   ECS041
00601  0235-REFORMAT-REPY-RECORDS.                                      ECS041
00602                                                                      CL*41
00603      MOVE RP-COMPANY-CD  TO AC-COMPANY-CD.                        ECS041
00604      MOVE RP-SV-CARRIER  TO AC-CARR.                              ECS041
00605      MOVE RP-SV-GROUPING TO AC-COMP.                              ECS041
00606      MOVE RP-SV-STATE    TO AC-STATE.                             ECS041
00607      MOVE RP-ACCOUNT     TO AC-ACCT.                              ECS041
00608                                                                   ECS041
00609      IF RP-ACCOUNT-EFF-DT = LOW-VALUES OR SPACES OR ZEROS         ECS041
00610         MOVE ZEROS             TO AC-DATE                            CL*39
00611      ELSE                                                         ECS041
00612         MOVE RP-ACCOUNT-EFF-DT TO DC-BIN-DATE-1                   ECS041
TSTMOD        MOVE '0235-REFORMAT-REPY-RECORDS - 01' TO FINDIT          ECS041
00613         MOVE ' '               TO DC-OPTION-CODE                  ECS041
00614         PERFORM 8500-DATE-CONVERSION THRU 8590-EXIT               ECS041
00615         MOVE DC-GREG-DATE-CYMD TO AC-DATE.                           CL*39
00616                                                                   ECS041
00617      MOVE RP-INS-AMT-INFORCE    TO AC-REM-AMT.                    ECS041
00618      MOVE RP-LIFE-MORTALITY-AMT TO AC-RESV.                       ECS041
00619      MOVE RP-FUTURE-RESERVE     TO AC-FUT-RESERVE.                ECS041
00620      MOVE RP-PTC-RESERVE        TO AC-PTC-RESERVE.                ECS041
00621      MOVE RP-IBNR-RESERVE       TO AC-IBNR-RESERVE.               ECS041
00622      MOVE RP-CLAIM-ADJ-AMT      TO AC-CLM-ADJ-AMT.                ECS041
00623      MOVE RP-EXPENSES           TO AC-EXPENSES.                   ECS041
00624      MOVE RP-PAYMENTS           TO AC-PAYMENTS.                   ECS041
00625      MOVE RP-REIN-PREM-ADJ      TO AC-REIN-PREM-ADJS.                CL*39
00626      MOVE RP-OTHER-COMM         TO AC-OTH-COMMISSIONS.            ECS041
00627      MOVE RP-REIN-PREM-ADJ      TO AC-REIN-PREM-ADJS.             ECS041
00628                                                                   ECS041
00629      MOVE RP-BENEFIT-TYPE       TO AC-LF-AH.                      ECS041
00630      MOVE RP-BENEFIT-CD         TO AC-BEN-TYPE.                   ECS041
00631      MOVE RP-REIN-COMP-NO       TO AC-REI-CO.                     ECS041
00632      MOVE SPACE                 TO AC-CARD-TYPE.                     CL*41
00633                                                                      CL*45
00634 ***  MOVE RP-EPEC-ADJ-DT        TO AC-A-PDATE.                       CL*48
00635      MOVE ZEROS                 TO WS-DATE-CCYYMM-N.                 CL*48
LOGIC *    MOVE 19                    TO WS-DT-CC.                         CL*48
LOGIC *    MOVE RP-EPEC-ADJ-YR        TO WS-DT-YR.                         CL*48
LOGIC *    MOVE RP-EPEC-ADJ-MO        TO WS-DT-MO.                         CL*48
LOGIC      MOVE RP-ADJ-MO             TO WS-DT-MO.                         CL*48
LOGIC      MOVE RP-ADJ-YR             TO WS-DT-YR                          CL*48
LOGIC                                    DC-ALPHA-YEAR.                    CL*48
LOGIC      MOVE '7'                   TO DC-OPTION-CODE.
TSTMOD     MOVE '0235-REFORMAT-REPY-RECORDS -  02' TO FINDIT.           ECS041
LOGIC      PERFORM  8500-DATE-CONVERSION
LOGIC         THRU  8590-EXIT.
LOGIC      IF ONLY-CENTURY
LOGIC         MOVE  DC-ALPHA-CEN-N     TO WS-DT-CC
LOGIC      ELSE
LOGIC         MOVE  DC-ERROR-CODE      TO WS-ABEND-FILE-STATUS
LOGIC         MOVE 'CENTURY NOT FOUND' TO WS-ABEND-MESSAGE
LOGIC         GO TO ABEND-PGM.
LOGIC *
00639      MOVE WS-DATE-CCYYMM-N      TO AC-A-PDATE.                       CL*48
00640                                                                   ECS041
00641  0240-PROCESS-TRANSACTION.                                        ECS041
00642      ADD +1 TO TRAN-IN.                                           ECS041
00643                                                                   ECS041
00644      IF AC-RESV NOT NUMERIC         MOVE ZEROS TO AC-RESV.        ECS041
00645      IF AC-REM-AMT NOT NUMERIC      MOVE ZEROS TO AC-REM-AMT.     ECS041
00646      IF AC-FUT-RESERVE NOT NUMERIC  MOVE ZEROS TO AC-FUT-RESERVE. ECS041
00647      IF AC-PTC-RESERVE NOT NUMERIC  MOVE ZEROS TO AC-PTC-RESERVE. ECS041
00648      IF AC-IBNR-RESERVE NOT NUMERIC MOVE ZEROS TO AC-IBNR-RESERVE.ECS041
00649      IF AC-CLM-ADJ-AMT NOT NUMERIC  MOVE ZEROS TO AC-CLM-ADJ-AMT. ECS041
00650      IF AC-EXPENSES NOT NUMERIC     MOVE ZEROS TO AC-EXPENSES.    ECS041
00651      IF AC-PAYMENTS NOT NUMERIC     MOVE ZEROS TO AC-PAYMENTS.    ECS041
00652      IF AC-OTH-COMMISSIONS NOT NUMERIC                            ECS041
00653                                  MOVE ZEROS TO AC-OTH-COMMISSIONS.ECS041
00654      IF AC-REIN-PREM-ADJS NOT NUMERIC                             ECS041
00655                                  MOVE ZEROS TO AC-REIN-PREM-ADJS. ECS041
00656                                                                   ECS041
00657      MOVE AC-RESV            TO SW-RESV.                          ECS041
00658      MOVE AC-REM-AMT         TO SW-REM-AMT.                       ECS041
00659      MOVE AC-FUT-RESERVE     TO SW-FUT-RESERVE.                   ECS041
00660      MOVE AC-PTC-RESERVE     TO SW-PTC-RESERVE.                   ECS041
00661      MOVE AC-IBNR-RESERVE    TO SW-IBNR-RESERVE.                  ECS041
00662      MOVE AC-CLM-ADJ-AMT     TO SW-CLM-ADJ-AMT.                   ECS041
00663      MOVE AC-EXPENSES        TO SW-EXPENSES.                      ECS041
00664      MOVE AC-PAYMENTS        TO SW-PAYMENTS.                      ECS041
00665      MOVE AC-OTH-COMMISSIONS TO SW-OTH-COMMISSIONS.               ECS041
00666      MOVE AC-REIN-PREM-ADJS  TO SW-REIN-PREM-ADJS.                ECS041
00667      MOVE AC-COMPANY-CD      TO SW-COMPANY-CD.                    ECS041
00668      MOVE AC-CARR            TO SW-CARR.                          ECS041
00669      MOVE AC-COMP            TO SW-COMP.                          ECS041
00670      MOVE AC-STATE           TO SW-STATE.                         ECS041
00671      MOVE AC-ACCT            TO SW-ACCT.                          ECS041
00672      MOVE AC-LF-AH           TO SW-LF-AH.                         ECS041
00673      MOVE AC-REI-CO          TO SW-REI-CO.                        ECS041
00674      MOVE AC-BEN-TYPE        TO SW-BEN-TYPE.                      ECS041
00675      MOVE AC-CARD-TYPE       TO SW-CARD-TYPE.                     ECS041
00676                                                                   ECS041
00677      IF AC-DATE = 00099999999  OR  99999999999                       CL*50
00678          MOVE 99999999998    TO AC-DATE.                             CL*50
00679                                                                   ECS041
00680      MOVE AC-DATE            TO SW-EXP-DT.                           CL*39
00681      MOVE ZEROS              TO SW-EFF-DT.                           CL*41
00682      MOVE AC-A-PDATE         TO SW-A-DATE                            CL*39
00683                                 WS-DATE-CCYYMM-N.                    CL*48
00684                                                                   ECS041
00685      IF (WS-DT-MO LESS 01 OR GREATER 12)                             CL*48
00686          MOVE 'X' TO ERR-IND (6) ERROR-FLAG.                      ECS041
00687                                                                   ECS041
00688      IF WS-DT-CCYY GREATER THAN RUN-CCYY                             CL*48
00689          MOVE 'X' TO ERR-IND (7) ERROR-FLAG.                      ECS041
00690                                                                   ECS041
00691      IF SW-LF-AH NOT = LIFE-OVERRIDE-L1 AND AH-OVERRIDE-L1        ECS041
00692          MOVE 'X' TO ERR-IND (1) ERROR-FLAG                       ECS041
00693          GO TO 0280-CHECK-AMOUNT-FIELDS.                          ECS041
00694                                                                   ECS041
00695      INSPECT SW-BEN-TYPE CONVERTING ' ' TO '0'.                   ECS041
00696                                                                   ECS041
00697      IF SW-BEN-TYPE = '00'                                        ECS041
00698          IF SW-LF-AH = LIFE-OVERRIDE-L1                           ECS041
00699              MOVE CLAS-I-BEN (CLAS-STARTL) TO SW-BEN-TYPE         ECS041
00700              GO TO 0280-CHECK-AMOUNT-FIELDS                       ECS041
00701          ELSE                                                     ECS041
00702              MOVE CLAS-I-BEN (CLAS-STARTA) TO SW-BEN-TYPE         ECS041
00703              GO TO 0280-CHECK-AMOUNT-FIELDS.                      ECS041
00704                                                                   ECS041
00705      IF SW-LF-AH = AH-OVERRIDE-L1                                 ECS041
00706          MOVE CLAS-STARTA TO CLAS-INDEXA                          ECS041
00707          GO TO 0260-AH-LOOP.                                      ECS041
00708                                                                   ECS041
00709      IF SW-LF-AH = LIFE-OVERRIDE-L1                               ECS041
00710          MOVE CLAS-STARTL TO CLAS-INDEXL                          ECS041
00711          GO TO 0270-LIFE-LOOP.                                    ECS041
00712                                                                   ECS041
00713  0260-AH-LOOP.                                                    ECS041
00714      IF CLAS-MAXA NOT GREATER THAN ZEROS                          ECS041
00715          MOVE 'X' TO ERR-IND (5) ERROR-FLAG                       ECS041
00716          GO TO 0280-CHECK-AMOUNT-FIELDS.                          ECS041
00717                                                                   ECS041
00718      IF CLAS-INDEXA GREATER THAN CLAS-MAXA                        ECS041
00719          MOVE 'X' TO ERR-IND (5) ERROR-FLAG                       ECS041
00720          GO TO 0280-CHECK-AMOUNT-FIELDS.                          ECS041
00721                                                                   ECS041
00722      IF SW-BEN-TYPE NOT = CLAS-I-BEN (CLAS-INDEXA)                ECS041
00723          ADD +1 TO CLAS-INDEXA                                    ECS041
00724          GO TO 0260-AH-LOOP.                                      ECS041
00725                                                                   ECS041
00726      GO TO 0280-CHECK-AMOUNT-FIELDS.                              ECS041
00727                                                                   ECS041
00728  0270-LIFE-LOOP.                                                  ECS041
00729      IF CLAS-MAXL NOT GREATER THAN ZEROS                          ECS041
00730          MOVE 'X' TO ERR-IND (5) ERROR-FLAG                       ECS041
00731          GO TO 0280-CHECK-AMOUNT-FIELDS.                          ECS041
00732                                                                   ECS041
00733      IF CLAS-INDEXL GREATER THAN CLAS-MAXL                        ECS041
00734          MOVE 'X' TO ERR-IND (5) ERROR-FLAG                       ECS041
00735          GO TO 0280-CHECK-AMOUNT-FIELDS.                          ECS041
00736                                                                   ECS041
00737      IF SW-BEN-TYPE NOT = CLAS-I-BEN (CLAS-INDEXL)                ECS041
00738          ADD +1 TO CLAS-INDEXL                                    ECS041
00739          GO TO 0270-LIFE-LOOP.                                    ECS041
00740                                                                   ECS041
00741  0280-CHECK-AMOUNT-FIELDS.                                        ECS041
00742                                                                   ECS041
00743  0290-CHECK-ERRS.                                                 ECS041
00744      IF ERROR-FLAG NOT = SPACES                                   ECS041
00745          ADD +1 TO TRAN-REJ                                       ECS041
00746          ADD +1 TO LINE-CNT                                       ECS041
00747          PERFORM 0670-PRINT-ERRS THRU 0690-EXIT                   ECS041
00748      ELSE                                                         ECS041
00749          RELEASE SW-REC.                                          ECS041
00750                                                                   ECS041
00751      GO TO 0200-READ-INPUT.                                       ECS041
00752                                                                   ECS041
00753  0300-END-INPUT-1.                                                ECS041
00754      CLOSE TAPE-IN.                                               ECS041
00755                                                                   ECS041
00756  0310-EXIT.                                                       ECS041
00757      EXIT.                                                        ECS041
00758  EJECT                                                            ECS041
00759  0320-MATCH-TO-ACCT-MSTR SECTION.                                 ECS041
pemuni     OPEN INPUT  ACCT-MSTR.                                       ECS041
pemuni*         OUTPUT SCS041-WORK.                                     ECS041
00762                                                                   ECS041
00763      IF AM-FILE-STATUS NOT = '00' AND '97'                        ECS041
00764         MOVE ' ERROR ON ERACCTT, OPEN ' TO WS-ABEND-MESSAGE       ECS041
00765         MOVE AM-FILE-STATUS TO WS-ABEND-FILE-STATUS               ECS041
00766         GO TO ABEND-PGM.                                          ECS041
00767                                                                   ECS041
00768      PERFORM 0360-READ-ACCT-MSTR THRU 0370-EXIT.                  ECS041
00769                                                                   ECS041
00770  0330-RETURN-CARDS.                                               ECS041
00771      RETURN SORT-WORK AT END                                      ECS041
00772          GO TO 0380-MATCH-TO-ACCT-MSTR-END.                       ECS041
00773                                                                   ECS041
00774  0340-CHECK-ACCT-MSTR.                                            ECS041
00775      IF AM-CONTROL-A LESS THAN SW-CNTRL-A                         ECS041
00776          PERFORM 0360-READ-ACCT-MSTR THRU 0370-EXIT               ECS041
00777          GO TO 0340-CHECK-ACCT-MSTR.                              ECS041
00778                                                                   ECS041
00779      IF AM-CONTROL-A GREATER THAN SW-CNTRL-A                      ECS041
00780          MOVE 'X' TO ERR-IND (4) ERROR-FLAG                       ECS041
00781          GO TO 0350-CHECK-ERRORS.                                 ECS041
00782                                                                   ECS041
00783      IF AM-EXPIRE-DT NOT GREATER THAN SW-EXP-DT                      CL*10
00784          PERFORM 0360-READ-ACCT-MSTR THRU 0370-EXIT               ECS041
00785          GO TO 0340-CHECK-ACCT-MSTR.                              ECS041
00786                                                                   ECS041
00787      IF AM-EFFECT-DT GREATER THAN SW-EXP-DT                          CL*52
00788          MOVE 'X' TO ERR-IND (4) ERROR-FLAG.                      ECS041
00789                                                                   ECS041
00790  0350-CHECK-ERRORS.                                               ECS041
00791      IF ERROR-FLAG NOT = SPACES                                   ECS041
00792          ADD +1 TO TRAN-REJ                                       ECS041
00793          ADD +1 TO LINE-CNT                                       ECS041
00794          PERFORM 0670-PRINT-ERRS THRU 0690-EXIT                   ECS041
00795          GO TO 0330-RETURN-CARDS.                                 ECS041
00796                                                                   ECS041
00797      MOVE AM-EXPIRE-DT TO SW-EXP-DT.                              ECS041
00798      MOVE AM-EFFECT-DT TO SW-EFF-DT.                              ECS041
00799                                                                   ECS041
00800      WRITE WORK-REC FROM SW-REC.                                  ECS041
00801                                                                   ECS041
00802      GO TO 0330-RETURN-CARDS.                                     ECS041
00803                                                                   ECS041
00804  0360-READ-ACCT-MSTR.                                             ECS041
00805      READ ACCT-MSTR.                                              ECS041
00806                                                                   ECS041
00807      IF AM-FILE-STATUS = '10'                                     ECS041
00808         MOVE HIGH-VALUES TO AM-CONTROL-PRIMARY                    ECS041
00809         GO TO 0370-EXIT.                                          ECS041
00810                                                                   ECS041
00811      IF AM-FILE-STATUS NOT = '00'                                 ECS041
00812         MOVE AM-FILE-STATUS TO WS-ABEND-FILE-STATUS               ECS041
00813         MOVE ' ERROR ON ERACCTT, READ ' TO WS-ABEND-MESSAGE       ECS041
00814         GO TO ABEND-PGM.                                          ECS041
00815                                                                   ECS041
00816  0370-EXIT.                                                       ECS041
00817      EXIT.                                                        ECS041
00818                                                                   ECS041
00819  0380-MATCH-TO-ACCT-MSTR-END.                                     ECS041
00820      CLOSE SCS041-WORK  ACCT-MSTR.                                ECS041
00821                                                                   ECS041
00822      IF AM-FILE-STATUS NOT = '00'                                 ECS041
00823         MOVE AM-FILE-STATUS TO WS-ABEND-FILE-STATUS               ECS041
00824         MOVE ' ERROR ON ERACCTT, CLOSE ' TO WS-ABEND-MESSAGE      ECS041
00825         GO TO ABEND-PGM.                                          ECS041
00826                                                                   ECS041
00827  0390-EXIT.                                                       ECS041
00828      EXIT.                                                        ECS041
00829  EJECT                                                            ECS041
00830  0400-SCS-WORK-RTN SECTION.                                       ECS041
00831                                                                   ECS041
00832  0410-OPEN-INPUT-2.                                               ECS041
pemuni*    OPEN INPUT SCS041-WORK.                                      ECS041
pemuni*    display 'made ig'.                                           ECS041
00835  0420-RETURN-SCS041.                                              ECS041
00836      READ SCS041-WORK AT END                                      ECS041
00837          GO TO 0430-END-INPUT-2.                                  ECS041
00838                                                                   ECS041
00839      MOVE WORK-REC TO SW-REC.                                     ECS041
00840      RELEASE SW-REC.                                              ECS041
00841      GO TO 0420-RETURN-SCS041.                                    ECS041
00842                                                                   ECS041
00843  0430-END-INPUT-2.                                                ECS041
00844      CLOSE SCS041-WORK.                                           ECS041
00845                                                                   ECS041
00846  0440-EXIT.                                                       ECS041
00847      EXIT.                                                        ECS041
00848  EJECT                                                            ECS041
00849  0450-POST-ADJUSTMENTS SECTION.                                   ECS041
00850                                                                   ECS041
00851  0460-OPEN-OUTPUT-2.                                              ECS041
00852      OPEN INPUT  OLD-EPEC                                         ECS041
00853           OUTPUT NEW-EPEC.                                        ECS041
00854                                                                   ECS041
00855      PERFORM 0490-RETURN-WORK-FILE THRU 0500-EXIT.                ECS041
00856      PERFORM 0510-READ-EPEC        THRU 0520-EXIT.                ECS041
00857                                                                   ECS041
00858  0470-MATCH-TO-EPEC.                                              ECS041
00859      IF WS-OE-CTL LESS THAN WS-SW-CTL                             ECS041
00860          MOVE OE-REC TO EP-RECORD                                 ECS041
00861          PERFORM 0480-WRITE-EPEC                                  ECS041
00862          PERFORM 0510-READ-EPEC THRU 0520-EXIT                    ECS041
00863          GO TO 0470-MATCH-TO-EPEC.                                ECS041
00864                                                                   ECS041
00865      IF WS-OE-CTL GREATER THAN WS-SW-CTL                          ECS041
00866          PERFORM 0530-BUILD-EPEC THRU 0550-EXIT                   ECS041
00867          GO TO 0470-MATCH-TO-EPEC.                                ECS041
00868                                                                   ECS041
00869      IF WS-OE-CTL = HIGH-VALUES                                   ECS041
00870          GO TO 0580-END-OUTPUT-2.                                 ECS041
00871                                                                   ECS041
00872      MOVE OE-REC TO EP-RECORD.                                    ECS041
00873      PERFORM 0560-UPDATE-EPEC THRU 0570-EXIT.                     ECS041
00874      MOVE EP-RECORD TO OE-REC.                                    ECS041
00875      PERFORM 0490-RETURN-WORK-FILE THRU 0500-EXIT.                ECS041
00876      GO TO 0470-MATCH-TO-EPEC.                                    ECS041
00877                                                                   ECS041
00878  0480-WRITE-EPEC.                                                 ECS041
00879      ADD +1 TO EPEC-OUT.                                          ECS041
00880      WRITE EP-RECORD.                                             ECS041
00881                                                                   ECS041
00882  0490-RETURN-WORK-FILE.                                           ECS041
00883      RETURN SORT-WORK AT END                                      ECS041
00884          MOVE HIGH-VALUES   TO SW-REC.                            ECS041
00885                                                                   ECS041
00886      MOVE SW-REI-CO         TO WS-SW-REIN.                        ECS041
00887      MOVE SW-CNTRL-A        TO WS-SW-CCSA.                        ECS041
00888      MOVE SW-EXP-DT         TO WS-SW-EXP.                         ECS041
00889      MOVE SW-EFF-DT         TO WS-SW-EFF.                         ECS041
00890      MOVE SW-LF-AH          TO WS-SW-LFAH.                        ECS041
00891      MOVE SW-BEN-TYPE       TO WS-SW-BENTP.                       ECS041
00892      MOVE SW-A-DATE         TO WS-SW-DATE.                           CL*39
00893      MOVE SW-COMPANY-CD     TO WS-SW-COMPANY-CD.                  ECS041
00894                                                                   ECS041
00895  0500-EXIT.                                                       ECS041
00896      EXIT.                                                        ECS041
00897                                                                   ECS041
00898  0510-READ-EPEC.                                                  ECS041
00899      READ OLD-EPEC AT END                                         ECS041
00900          MOVE HIGH-VALUES TO WS-OE-CTL                            ECS041
00901          GO TO 0520-EXIT.                                         ECS041
00902                                                                   ECS041
00903      ADD +1 TO EPEC-IN.                                           ECS041
00904      MOVE OE-REIN           TO WS-OE-REIN.                           CL*41
00905      MOVE OE-CCSAD          TO WS-OE-CCSAD.                          CL*41
00906                                                                      CL*41
00907      MOVE OE-RUN-DT         TO WS-OE-RUN-DT-N.                       CL*42
00908      MOVE ZEROS             TO WS-DATE-CCYYMM-N.                     CL*48
00909      MOVE OE-RUN-CCYY       TO WS-DT-CCYY.                           CL*48
00910      MOVE OE-RUN-MO         TO WS-DT-MO.                             CL*48
00911      MOVE WS-DATE-CCYYMM-N  TO WS-OE-RUN-DT.                         CL*48
00912                                                                      CL*41
00913      MOVE OE-LFAH-BEN       TO WS-OE-LFAH-BEN.                       CL*41
00914                                                                   ECS041
00915  0515-CHECK-FOR-NEW-CTL.                                          ECS041
00916      IF WS-OE-CTL GREATER THAN WS-SW-CTL                          ECS041
00917          PERFORM 0530-BUILD-EPEC THRU 0550-EXIT                   ECS041
00918          GO TO 0515-CHECK-FOR-NEW-CTL.                            ECS041
00919                                                                   ECS041
00920      IF OE-RECORD-ID NOT = 'EP'                                   ECS041
00921          MOVE OE-REC TO EP-RECORD                                 ECS041
00922          PERFORM 0480-WRITE-EPEC                                  ECS041
00923          GO TO 0510-READ-EPEC.                                    ECS041
00924                                                                   ECS041
00925      IF OE-PURGE = 'P'                                            ECS041
00926          MOVE OE-REC TO EP-RECORD                                 ECS041
00927          PERFORM 0480-WRITE-EPEC                                  ECS041
00928          GO TO 0510-READ-EPEC.                                    ECS041
00929                                                                   ECS041
00930      IF  DTE-OPT-RESERVE-METHOD-AUTH                              ECS041
00931          GO TO 0520-EXIT.                                         ECS041
00932                                                                   ECS041
00933      IF DTE-CLIENT = 'HER'                                        ECS041
00934        IF OE-LAH = LIFE-OVERRIDE-L1                               ECS041
00935          IF OE-RUN-YR = RUN-YR AND                                ECS041
00936             OE-RUN-MO = RUN-MO                                    ECS041
00937              MOVE ZEROS TO OE-CLM-DU   OE-CLM-PV                  ECS041
00938                            OE-CLM-IBNR OE-LOSS-RESV.              ECS041
00939                                                                   ECS041
00940      IF DTE-CLIENT = 'PEK' OR 'PLI'                               ECS041
00941          IF OE-RUN-YR = RUN-YR AND                                ECS041
00942             OE-RUN-MO = RUN-MO                                    ECS041
00943              MOVE ZEROS TO OE-CLM-IBNR.                           ECS041
00944                                                                   ECS041
00945  0520-EXIT.                                                       ECS041
00946      EXIT.                                                        ECS041
00947                                                                   ECS041
00948  0530-BUILD-EPEC.                                                 ECS041
00949      MOVE SPACES           TO EP-RECORD.                          ECS041
00950      MOVE 'EP'             TO EP-RECORD-ID.                       ECS041
00951      MOVE WS-SW-COMPANY-CD TO EP-COMPANY-CD.                      ECS041
00952      MOVE WS-SW-REIN       TO EP-REI-CO.                          ECS041
00953                                                                   ECS041
00954      IF EP-REI-CO NOT = SPACES                                    ECS041
00955          MOVE 'R' TO EP-REIN.                                     ECS041
00956                                                                   ECS041
00957      MOVE WS-SW-CARR       TO EP-CARRIER.                         ECS041
00958      MOVE WS-SW-GROUP      TO EP-GROUPING.                        ECS041
00959      MOVE WS-SW-STATE      TO EP-STATE.                           ECS041
00960      MOVE WS-SW-ACCT       TO EP-ACCOUNT.                         ECS041
00961      MOVE WS-SW-EXP        TO EP-EXP-DTE.                            CL*39
00962      MOVE WS-SW-EFF        TO EP-EFF-DTE.                            CL*39
00963      MOVE WS-SW-LFAH       TO EP-RCD-TYPE.                        ECS041
00964      MOVE WS-SW-BENTP      TO EP-BEN-CODE.                        ECS041
00965                                                                      CL*39
00966      MOVE WS-SW-DATE       TO WS-DATE-CCYYMM-N.                      CL*48
00967      MOVE ZEROS            TO WS-EP-RUN-DTE-N.                       CL*39
00968      MOVE WS-DT-CC         TO EP-RUN-CC.                             CL*48
00969      MOVE WS-DT-YR         TO EP-RUN-YR.                             CL*48
00970      MOVE WS-DT-MO         TO EP-RUN-MO.                             CL*48
00971                                                                   ECS041
00972      MOVE EP-RUN-YR        TO HOLD-CEN-1-YY.                      ECS041
00973      MOVE EP-RUN-CC        TO HOLD-CEN-1-CC.                      ECS041
040103*    MOVE 'H'              TO DC-OPTION-CODE.                     ECS041
TSTMOD     MOVE '0530-BUILD-EPEC                ' TO FINDIT.            ECS041
040103     MOVE 'L'              TO DC-OPTION-CODE
040103     MOVE WS-EP-RUN-DTE-N  TO DC-GREG-DATE-CYMD
040103     MOVE 01               TO DC-CYMD-DAY  
LOGIC      PERFORM  8500-DATE-CONVERSION
LOGIC         THRU  8590-EXIT.
040103     IF NO-CONVERSION-ERROR
040103        MOVE DC-DAYS-IN-MONTH    TO EP-RUN-DA
040103     ELSE
040103        DISPLAY ' DATE CONVERT ERROR ON EP RUN DATE      '
040103     END-IF
040103*    IF DATE-CONVERSION-ERROR                                     ECS041
040103*       IF ONLY-LEAP-YEAR                                         ECS041
040103*          MOVE HOLD-CEN-1-DA TO MONTH-D (2)                      ECS041
040103*       ELSE                                                      ECS041
040103*          MOVE 'LEAP YEAR ERROR' TO WS-ABEND-MESSAGE             ECS041
040103*          MOVE DC-ERROR-CODE     TO WS-ABEND-FILE-STATUS         ECS041
040103*          GO TO ABEND-PGM.                                          CL*38
00982                                                                   ECS041
040103*    MOVE MONTH-D (EP-RUN-MO) TO EP-RUN-DA.                       ECS041
00984                                                                   ECS041
00985      MOVE WS-EP-RUN-DTE-N     TO EP-RUN-DTE.                         CL**2
00986                                                                   ECS041
00987      MOVE ZEROS TO EP-ISS-BEN         EP-CNC-BEN                  ECS041
00988                    EP-ISS-PRM         EP-CNC-PRM                  ECS041
00989                    EP-PRM-78          EP-PRM-PR                   ECS041
00990                    EP-CLM-AMT         EP-CLM-CNT                  ECS041
00991                    EP-CLM-CRT         EP-ISS-CNT                  ECS041
00992                    EP-CNC-CNT         EP-CLM-DU                   ECS041
00993                    EP-CLM-PV          EP-CLM-IBNR                 ECS041
00994                    EP-LOSS-RESV       EP-CLAIM-ADJ                ECS041
00995                    EP-RETRO-EXPENSES  EP-RETRO-PAYMENTS           ECS041
00996                    EP-RETRO-OTH-COMM  EP-MORT-RESV                ECS041
00997                    EP-IN-FORCE        EP-ADJUST                   ECS041
00998                    EP-CNC-BEN-GROSS   EP-CNC-PRM-GROSS            ECS041
00999                    EP-ISS-BEN-GROSS   EP-ISS-PRM-GROSS            ECS041
01000                    EP-PRM-ST          EP-PRM-78-ADJ               ECS041
01001                    EP-PRM-ST-ADJ      EP-PRM-PR-ADJ               ECS041
01002                    EP-AVG-AGE         EP-AVG-ORIG-TERM            ECS041
01003                    EP-WEIGHTED-AGE    EP-WEIGHTED-ORIG-TERM       ECS041
01004                    EP-AVG-REM-TERM                                ECS041
01005                    EP-HI-COV-DT       EP-HI-CERT                  ECS041
01006                    EP-LO-CERT         EP-LIFE-YEARS               ECS041
                         EP-PRM-TAX
01007                    EP-CLM-EXP         EP-INFORCE-CNT.             ECS041
01008                                                                   ECS041
01009      MOVE WS-SW-CTL TO WS-SAVE-SW-CTL.                            ECS041
01010                                                                   ECS041
01011  0540-POST-LOOP.                                                  ECS041
01012      PERFORM 0560-UPDATE-EPEC      THRU 0570-EXIT.                ECS041
01013      PERFORM 0490-RETURN-WORK-FILE THRU 0500-EXIT.                ECS041
01014                                                                   ECS041
01015      IF WS-SW-CTL = WS-SAVE-SW-CTL                                ECS041
01016          GO TO 0540-POST-LOOP.                                    ECS041
01017                                                                   ECS041
01018      PERFORM 0480-WRITE-EPEC.                                     ECS041
01019                                                                   ECS041
01020  0550-EXIT.                                                       ECS041
01021      EXIT.                                                        ECS041
01022  EJECT                                                            ECS041
01023  0560-UPDATE-EPEC.                                                ECS041
01024      ADD +1 TO TRAN-POST.                                         ECS041
01025      IF SW-REM-AMT GREATER THAN +0 AND                            ECS041
01026         SW-LF-AH = LIFE-OVERRIDE-L1                               ECS041
01027          PERFORM 0740-COMPUTE-LIFE-IBNR THRU 0740-EXIT.           ECS041
01028                                                                   ECS041
01029      IF SW-REM-AMT GREATER THAN +0 AND                            ECS041
01030         SW-LF-AH = AH-OVERRIDE-L1                                 ECS041
01031          PERFORM 0750-COMPUTE-AH-IBNR THRU 0750-EXIT.             ECS041
01032                                                                   ECS041
01033      ADD SW-RESV          TO EP-MORT-RESV MORT-RESV.              ECS041
01034      ADD SW-REM-AMT       TO EP-IN-FORCE AH-IN-FORCE.             ECS041
01035      ADD SW-FUT-RESERVE   TO EP-LOSS-RESV LOSS-RESV.              ECS041
01036      ADD SW-PTC-RESERVE   TO EP-CLM-DU CLM-DU.                    ECS041
01037      ADD SW-IBNR-RESERVE  TO EP-CLM-IBNR CLM-IBNR.                ECS041
01038      ADD SW-CLM-ADJ-AMT   TO EP-CLAIM-ADJ CLAIM-ADJ.              ECS041
01039      ADD SW-EXPENSES      TO EP-RETRO-EXPENSES RETRO-EXPENSES.    ECS041
01040      ADD SW-PAYMENTS      TO EP-RETRO-PAYMENTS RETRO-PAYMENTS.    ECS041
01041      ADD SW-OTH-COMMISSIONS TO EP-RETRO-OTH-COMM RETRO-OTH-COMM.  ECS041

122408     IF SW-CLM-ADJ-AMT NOT = ZEROS
122408        MOVE SW-CLM-ADJ-AMT      TO WS-DIS-AMT
122408        DISPLAY ' ABOUT TO ADD '
122408        WS-DIS-AMT ' TO ' EP-CONTROL ' ' EP-REI-CO
122408        ' ' EP-RCD-TYPE ' ' EP-BEN-CODE
122408     END-IF

122408     IF SW-REIN-PREM-ADJS NOT = ZEROS
122408        MOVE SW-REIN-PREM-ADJS   TO WS-DIS-AMT
122408        DISPLAY ' ABOUT TO ADD '
122408        WS-DIS-AMT ' TO ' EP-CONTROL ' ' EP-REI-CO
122408        ' ' EP-RCD-TYPE ' ' EP-BEN-CODE
122408     END-IF

01042      ADD SW-REIN-PREM-ADJS TO EP-ADJUST REIN-ADJUST.              ECS041
01043                                                                   ECS041
01044  0570-EXIT.                                                       ECS041
01045      EXIT.                                                        ECS041
01046                                                                   ECS041
01047  0580-END-OUTPUT-2.                                               ECS041
01048      CLOSE OLD-EPEC  NEW-EPEC.                                    ECS041
01049      PERFORM 0720-PRINT-TOTALS THRU 0730-EXIT.                    ECS041
01050                                                                   ECS041
01051      IF TRAN-IN = 0                                               ECS041
01052          MOVE 'NO INPUT TRANSACTIONS THIS CYCLE' TO P-DATA        ECS041
01053          MOVE '0' TO P-CTL                                        ECS041
01054          PERFORM 0610-PRT-RTN THRU 0620-EXIT.                     ECS041
01055                                                                   ECS041
01056  0590-EXIT.                                                       ECS041
01057      EXIT.                                                        ECS041
01058  EJECT                                                            ECS041
01059  0600-SUB-ROUTINE SECTION.                                        ECS041
01060                                                                   ECS041
01061  0610-PRT-RTN.                                                    ECS041
01062                              COPY ELCPRT2.                        ECS041
01063                                                                   ECS041
01064      IF X = SPACES                                                ECS041
01065         ADD  1              TO LINE-CNT                           ECS041
01066         IF X = 'O'                                                ECS041
01067             ADD  2          TO LINE-CNT                           ECS041
01068             IF X = '-'                                            ECS041
01069                ADD  3       TO LINE-CNT                           ECS041
01070                IF X = '1'                                         ECS041
01071                   MOVE 1    TO LINE-CNT.                          ECS041
01072                                                                   ECS041
01073  0620-EXIT.                                                       ECS041
01074      EXIT.                                                        ECS041
01075                                                                   ECS041
01076  0630-PRT-TOTS.                                                   ECS041
01077      MOVE CTR-DESC (SA) TO T1-DESC.                               ECS041
01078      MOVE CTRS (SA)     TO T1-CTR.                                ECS041
01079      MOVE TOT-1         TO PRT.                                   ECS041
01080      MOVE ' '           TO X.                                     ECS041
01081      PERFORM 0610-PRT-RTN THRU 0620-EXIT.                         ECS041
01082                                                                   ECS041
01083  0640-EXIT.                                                       ECS041
01084      EXIT.                                                        ECS041
01085                                                                   ECS041
01086  0650-PRT-CTRS.                                                   ECS041
01087      IF CTRS (SA) NOT = ZERO                                      ECS041
01088          MOVE CTR-DESC (SA) TO T1-DESC                            ECS041
01089          MOVE CTRS (SA)     TO T1-CTR                             ECS041
01090          MOVE TOT-1         TO PRT                                ECS041
01091          MOVE ' '           TO X                                  ECS041
01092          PERFORM 0610-PRT-RTN THRU 0620-EXIT.                     ECS041
01093                                                                   ECS041
01094  0660-EXIT.                                                       ECS041
01095      EXIT.                                                        ECS041
01096                                                                   ECS041
01097  0670-PRINT-ERRS.                                                 ECS041
01098      IF LINE-CNT GREATER THAN +50                                 ECS041
01099          MOVE ZERO     TO LINE-CNT                                ECS041
01100          ADD +1 TO PAGE-CNT                                       ECS041
01101          MOVE PAGE-CNT TO H3-PAGE                                 ECS041
01102          MOVE HDR-1    TO PRT                                     ECS041
01103          MOVE '1'      TO X                                       ECS041
01104          PERFORM 0610-PRT-RTN THRU 0620-EXIT                      ECS041
01105          MOVE HDR-2    TO PRT                                     ECS041
01106          MOVE ' '      TO X                                       ECS041
01107          PERFORM 0610-PRT-RTN THRU 0620-EXIT                      ECS041
01108          MOVE HDR-3    TO PRT                                     ECS041
01109          MOVE ' '      TO X                                       ECS041
01110          PERFORM 0610-PRT-RTN THRU 0620-EXIT                      ECS041
01111          MOVE SPACES   TO PRT                                     ECS041
01112          MOVE '0'      TO X                                       ECS041
01113          PERFORM 0610-PRT-RTN THRU 0620-EXIT.                     ECS041
01114                                                                   ECS041
01115      MOVE '0'          TO X.                                      ECS041
01116      MOVE D1-DASHES    TO DTL-1.                                  ECS041
01117      MOVE SW-REI-CO    TO D1-REIN.                                ECS041
01118      MOVE SW-CARR      TO D1-CARR.                                ECS041
01119      MOVE SW-COMP      TO D1-COMP.                                ECS041
01120      MOVE SW-STATE     TO D1-STATE.                               ECS041
01121      MOVE SW-ACCT      TO D1-ACCT.                                ECS041
01122      MOVE SW-EXP-DT    TO WS-SW-EXP-DT-N.                            CL*45
01123      MOVE SW-EXP-MO    TO D1-EXP-MO.                              ECS041
01124      MOVE SW-EXP-DA    TO D1-EXP-DA.                              ECS041
01125      MOVE SW-EXP-YR    TO D1-EXP-YR.                              ECS041
01126      MOVE SW-LF-AH     TO D1-LF-AH.                               ECS041
01127      MOVE SW-BEN-TYPE  TO D1-BEN-TYPE.                            ECS041
01128      MOVE RUN-MO       TO D1-RUN-MO.                              ECS041
01129      MOVE RUN-DA       TO D1-RUN-DA.                              ECS041
01130      MOVE RUN-YR       TO D1-RUN-YR.                              ECS041
01131      MOVE SW-CARD-TYPE TO D1-CARD-TYPE.                           ECS041
01132      MOVE SW-A-DATE    TO WS-DATE-CCYYMM-N.                          CL*48
01133      MOVE WS-DT-MO     TO D1-PMO.                                    CL*48
01134      MOVE WS-DT-YR     TO D1-PYR.                                    CL*48
01135      MOVE SPACES       TO ERROR-FLAG.                             ECS041
01136      MOVE +1           TO SA.                                     ECS041
01137                                                                   ECS041
01138  0680-ERR-LOOP.                                                   ECS041
01139      IF ERR-IND (SA) = HIGH-VALUES                                ECS041
01140          GO TO 0690-EXIT.                                         ECS041
01141                                                                   ECS041
01142      IF ERR-IND (SA) NOT = SPACES                                 ECS041
01143          MOVE SPACES         TO ERR-IND (SA)                      ECS041
01144          MOVE ERROR-MSG (SA) TO D1-MSG                            ECS041
01145          MOVE DTL-1          TO PRT                               ECS041
01146          PERFORM 0610-PRT-RTN THRU 0620-EXIT                      ECS041
01147          MOVE SPACES         TO DTL-1 X.                          ECS041
01148                                                                   ECS041
01149      ADD +1 TO SA.                                                ECS041
01150      GO TO 0680-ERR-LOOP.                                         ECS041
01151                                                                   ECS041
01152  0690-EXIT.                                                       ECS041
01153      EXIT.                                                        ECS041
01154                                                                   ECS041
01155  0700-PRT-ACCUMS.                                                 ECS041
01156      IF ACCUMS (SA) NOT = ZERO                                    ECS041
01157          MOVE ACCUM-DESC (SA) TO T3-DESC                          ECS041
01158          MOVE ACCUMS (SA)     TO T3-ACCUM                         ECS041
01159          MOVE TOT-3           TO PRT                              ECS041
01160          MOVE ' '             TO X                                ECS041
01161          PERFORM 0610-PRT-RTN THRU 0620-EXIT.                     ECS041
01162                                                                   ECS041
01163  0710-EXIT.                                                       ECS041
01164      EXIT.                                                        ECS041
01165                                                                   ECS041
01166  0720-PRINT-TOTALS.                                               ECS041
01167      ADD +1 TO PAGE-CNT.                                          ECS041
01168      MOVE PAGE-CNT TO H3-PAGE.                                    ECS041
01169      MOVE HDR-1    TO PRT.                                        ECS041
01170      MOVE '1'      TO X.                                          ECS041
01171      PERFORM 0610-PRT-RTN THRU 0620-EXIT.                         ECS041
01172      MOVE HDR-2    TO PRT.                                        ECS041
01173      MOVE ' '      TO X.                                          ECS041
01174      PERFORM 0610-PRT-RTN THRU 0620-EXIT.                         ECS041
01175      MOVE HDR-3    TO PRT.                                        ECS041
01176      MOVE ' '      TO X.                                          ECS041
01177      PERFORM 0610-PRT-RTN THRU 0620-EXIT.                         ECS041
01178      MOVE SPACES   TO PRT.                                        ECS041
01179      MOVE '0'      TO X.                                          ECS041
01180      PERFORM 0610-PRT-RTN  THRU 0620-EXIT.                        ECS041
01181                                                                   ECS041
01182      PERFORM 0630-PRT-TOTS THRU 0640-EXIT                         ECS041
01183          VARYING SA FROM +1 BY +1                                 ECS041
01184              UNTIL SA GREATER THAN +5.                            ECS041
01185                                                                   ECS041
01186      MOVE SPACES   TO PRT.                                        ECS041
01187      MOVE '0'      TO X.                                          ECS041
01188      PERFORM 0610-PRT-RTN THRU 0620-EXIT.                         ECS041
01189                                                                   ECS041
01190      IF MORT-RESV NOT = ZERO                                      ECS041
01191          MOVE MORT-RESV TO T2-MORT                                ECS041
01192          MOVE TOT-2     TO PRT                                    ECS041
01193          MOVE ' '       TO X                                      ECS041
01194          PERFORM 0610-PRT-RTN THRU 0620-EXIT.                     ECS041
01195                                                                   ECS041
01196      PERFORM 0700-PRT-ACCUMS THRU 0710-EXIT                       ECS041
01197          VARYING SA FROM +1 BY +1                                 ECS041
01198              UNTIL SA GREATER THAN +17.                           ECS041
01199                                                                   ECS041
01200      PERFORM 0650-PRT-CTRS THRU 0660-EXIT                         ECS041
01201          VARYING SA FROM +6 BY +1                                 ECS041
01202              UNTIL SA GREATER THAN +9.                            ECS041
01203                                                                   ECS041
01204      IF ME-DO-UPDATE                                              ECS041
01205          MOVE RETRO-PAYMENTS     TO HLD-041-RETROS                   CL*37
01206          MOVE REIN-ADJUST        TO HLD-041-REIN-ADJ.                CL*37
01207                                                                   ECS041
01208  0730-EXIT.                                                       ECS041
01209      EXIT.                                                        ECS041
01210  EJECT                                                            ECS041
01211  0740-COMPUTE-LIFE-IBNR.                                          ECS041
01212                                                                   ECS041
01213      IF  DTE-OPT-RESERVE-METHOD-AUTH                              ECS041
01214          GO TO 0740-EXIT.                                         ECS041
01215                                                                   ECS041
01216      IF DTE-CLIENT = 'FIA' OR 'UFR' OR 'XXT' OR 'XXX'             ECS041
01217              COMPUTE EP-CLM-IBNR ROUNDED = EP-CLM-IBNR +          ECS041
01218                   (SW-REM-AMT * .0005).                           ECS041
01219                                                                   ECS041
01220      IF DTE-CLIENT = 'TFS' OR 'NIS' OR 'AN1'                      ECS041
01221              COMPUTE EP-CLM-IBNR ROUNDED = EP-CLM-IBNR +          ECS041
01222                   (SW-REM-AMT * .00038)                           ECS041
01223              COMPUTE CLM-IBNR ROUNDED = CLM-IBNR +                ECS041
01224                   (SW-REM-AMT * .00038).                          ECS041
01225                                                                   ECS041
01226      IF DTE-CLIENT = 'BWS'                                        ECS041
01227              COMPUTE EP-CLM-IBNR ROUNDED = EP-CLM-IBNR +          ECS041
01228                   (SW-REM-AMT * .00075).                          ECS041

032612     if dte-client = 'AHL'
032612        compute ep-clm-ibnr rounded = ep-clm-ibnr +
032612           (sw-rem-amt * .00067)
032612     end-if

012303     IF DTE-CLIENT = 'CID'
01231         COMPUTE EP-CLM-IBNR ROUNDED = EP-CLM-IBNR +
122707           (SW-REM-AMT * .00045)
122707     END-IF
01233                                                                   ECS041
01234      IF DTE-CLIENT = 'LAP'                                        ECS041
01235          IF EP-STATE = 'WV'                                       ECS041
01236              COMPUTE EP-CLM-IBNR ROUNDED = EP-CLM-IBNR +          ECS041
01237                   (SW-REM-AMT * .0010)                            ECS041
01238          ELSE                                                     ECS041
01239              COMPUTE EP-CLM-IBNR ROUNDED = EP-CLM-IBNR +          ECS041
01240                   (SW-REM-AMT * .0005).                           ECS041
01241                                                                   ECS041
01242      IF DTE-CLIENT = 'ADL' OR 'FLB' OR 'ALA'                      ECS041
01243          COMPUTE EP-CLM-IBNR ROUNDED = EP-CLM-IBNR +              ECS041
01244                  (SW-REM-AMT * .00036)                            ECS041
01245          COMPUTE CLM-IBNR ROUNDED = CLM-IBNR +                    ECS041
01246                  (SW-REM-AMT * .00036).                           ECS041
01247                                                                   ECS041
01248      IF DTE-CLIENT = 'ITG'                                        ECS041
01249         MOVE SW-BEN-TYPE            TO ITG-LIFE-TYPE              ECS041
01250         IF NOT ITG-OB-COVERAGE                                    ECS041
01251              COMPUTE EP-CLM-IBNR ROUNDED = EP-CLM-IBNR +          ECS041
01252                      (SW-REM-AMT * .0007000)                      ECS041
01253              COMPUTE CLM-IBNR ROUNDED = CLM-IBNR +                ECS041
01254                      (SW-REM-AMT * .0007000).                     ECS041
01255                                                                   ECS041
01256      IF DTE-CLIENT = 'PEK'                                        ECS041
01257          COMPUTE EP-CLM-IBNR ROUNDED = EP-CLM-IBNR +              ECS041
01258                  (SW-REM-AMT * .0005596)                          ECS041
01259          COMPUTE CLM-IBNR ROUNDED = CLM-IBNR +                    ECS041
01260                  (SW-REM-AMT * .0005596).                         ECS041
01261                                                                   ECS041
01262  0740-EXIT.                                                       ECS041
01263      EXIT.                                                        ECS041
01264                                                                   ECS041
01265  0750-COMPUTE-AH-IBNR.                                            ECS041
01266                                                                   ECS041
01267      IF  DTE-OPT-RESERVE-METHOD-AUTH                              ECS041
01268          GO TO 0750-EXIT.                                         ECS041
01269                                                                   ECS041
01270 ***************************************************************   ECS041
01271 ***   PEK CALCULATES AH-IBNR BASED UPON .0301 OF UNEARNED    **      CL*53
01272 ***   A/H PREMIUM.                                           **   ECS041
01273 ***   THEREFORE ECS040 PASSES THE AH UNEARN                  **   ECS041
01274 ***   IN THE IN-FORCE FIELD                                  **   ECS041
01275 ***   THIS ROUTINE WILL ZERO OUT THE INFORCE AMT TO AVOID    **   ECS041
01276 ***   ECS041 ADDING UNEARN PREM TO REM. BENEFIT.             **   ECS041
01277 ***************************************************************   ECS041
01278                                                                   ECS041
01279      IF DTE-CLIENT = 'PEK'                                        ECS041
01280          COMPUTE EP-CLM-IBNR ROUNDED = EP-CLM-IBNR +              ECS041
01281                  (SW-REM-AMT * .0301)                                CL*53
01282          COMPUTE CLM-IBNR ROUNDED = CLM-IBNR +                    ECS041
01283                  (SW-REM-AMT * .0301)                                CL*53
01284          MOVE +0 TO SW-REM-AMT.                                   ECS041
01285                                                                   ECS041
01286      IF DTE-CLIENT EQUAL 'COM'                                    ECS041
01287         COMPUTE EP-CLM-IBNR ROUNDED EQUAL                         ECS041
01288             (SW-REM-AMT * .00142)                                 ECS041
01289         COMPUTE CLM-IBNR ROUNDED EQUAL CLM-IBNR +                 ECS041
01290             (SW-REM-AMT * .00142).                                ECS041
01291                                                                   ECS041
01292  0750-EXIT.                                                       ECS041
01293      EXIT.                                                        ECS041
01294  EJECT                                                            ECS041
01295  0760-E-O-J.                                                      ECS041
01296      CLOSE PRNTR.                                                 ECS041
01297                                                                   ECS041
01298  0770-CLOSE-FICH.                                                 ECS041
01299                              COPY ELCPRTC.                        ECS041
01300                                                                   ECS041
01301      OPEN I-O ERMEBL.                                             ECS041
01302                                                                   ECS041
01303      IF ERMEBL-FILE-STATUS NOT = ZERO AND '97'                    ECS041
01304          MOVE 'N'                TO ME-UPDATE-FLAG.               ECS041
01305                                                                   ECS041
01306      IF ME-DO-UPDATE                                              ECS041
01307          MOVE DTE-CLIENT         TO ME-COMPANY                    ECS041
01308          COMPUTE MONTH-END-MOYR = (RUN-CCYY * 12) + RUN-MO           CL*39
01309          MOVE MONTH-END-MOYR     TO ME-MOYR.                      ECS041
01310                                                                   ECS041
01311      IF ME-DO-UPDATE                                              ECS041
01312          READ ERMEBL INVALID KEY                                  ECS041
01313          MOVE 'N'                TO ME-UPDATE-FLAG                ECS041
01314          CLOSE ERMEBL.                                            ECS041
01315                                                                   ECS041
01316      IF ME-DO-UPDATE                                              ECS041
070714         MOVE EPEC-IN            TO ME-041-RECS-IN
070714         MOVE EPEC-OUT           TO ME-041-RECS-OUT
01318          MOVE ME-CNDS-DATE       TO ME-041-RUN-DT                 ECS041
01321          ADD 1                   TO ME-041-RUN-CT                 ECS041
01322          MOVE HLD-041-RETROS     TO ME-041-RETROS                 ECS041
01323          MOVE HLD-041-REIN-ADJ   TO ME-041-REIN-ADJ                  CL*36
01324          REWRITE MONTH-END-BALANCES                               ECS041
01325          CLOSE ERMEBL.                                            ECS041
01326                                                                   ECS041
01327      IF ME-DO-UPDATE                                              ECS041
01328          DISPLAY 'MONTH-END BALANCES POSTED'                      ECS041
01329        ELSE                                                       ECS041
01330          DISPLAY 'MONTH-END BALANCES NOT POSTED'.                 ECS041
01331                                                                   ECS041
01332      DISPLAY '****** THE END OF MESSAGES CREATED BY '             ECS041
01333          'ECS041 ******'                                          ECS041
01334                                                                   ECS041
01335      GOBACK.                                                      ECS041
01336                                                                   ECS041
01337  ABEND-PGM.                                                       ECS041
TSTMOD     DISPLAY 'ABEND ENTERED FROM ' FINDIT.                        ECS041
01338                          COPY ELCABEND SUPPRESS.                  ECS041
01339                                                                   ECS041
01340  8500-DATE-CONVERSION.                                            ECS041
01341      CALL 'ELDATCX' USING DATE-CONVERSION-DATA.                   ECS041
01342                                                                   ECS041
01343  8590-EXIT.                                                       ECS041
01344      EXIT.                                                        ECS041
