00001  IDENTIFICATION DIVISION.                                         03/19/98
00002                                                                   ECS018
00003  PROGRAM-ID.                ECS018.                                  LV003
00004 *              PROGRAM CONVERTED BY                               ECS018
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ECS018
00006 *              CONVERSION DATE 11/28/95 10:57:53.                 ECS018
00007 *           PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE             ECS018
00008 *                           VMOD=2.011.                           ECS018
00009                                                                   ECS018
00010 *AUTHOR.     LOGIC, INC.                                          ECS018
00011 *            DALLAS, TEXAS.                                       ECS018
00012                                                                   ECS018
00013 *DATE-COMPILED.                                                   ECS018
00014                                                                   ECS018
00015 *SECURITY.   *****************************************************ECS018
00016 *            *                                                   *ECS018
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ECS018
00018 *            *                                                   *ECS018
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS018
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ECS018
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *ECS018
00022 *            *                                                   *ECS018
00023 *            *****************************************************ECS018
00024                                                                   ECS018
00025 *REMARKS.                                                         ECS018
00026 *    PRINT PREMIUM AND COMMISSION ANALYSIS FOR RECALCULATED AMTS  ECS018
092602******************************************************************
092602*                   C H A N G E   L O G
092602*
092602* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
092602*-----------------------------------------------------------------
092602*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
092602* EFFECTIVE    NUMBER
092602*-----------------------------------------------------------------
092602* 092602    2002091900008  PEMA  INCREASE NUMBER OF MAXIMUM
092602*                                  BENEFIT CODES FROM 450 TO 900
013107* 013107    2006122700001  PEMA  ADD CARRIER SUMMARY TO REPORT
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
070714* 070714  CR2013060600001  PEMA  AUTOMATE MONTH END BALANCING
092602******************************************************************
00027                                                                   ECS018
00028  ENVIRONMENT DIVISION.                                            ECS018
00029  CONFIGURATION SECTION.                                           ECS018
00030  SPECIAL-NAMES.                                                   ECS018
00031      C02 IS LCP-CH2                                               ECS018
00032      C03 IS LCP-CH3                                               ECS018
00033      C04 IS LCP-CH4                                               ECS018
00034      C05 IS LCP-CH5                                               ECS018
00035      C06 IS LCP-CH6                                               ECS018
00036      C07 IS LCP-CH7                                               ECS018
00037      C08 IS LCP-CH8                                               ECS018
00038      C09 IS LCP-CH9                                               ECS018
00039      C10 IS LCP-CH10                                              ECS018
00040      C11 IS LCP-CH11                                              ECS018
00041      C12 IS LCP-CH12                                              ECS018
00042      S01 IS LCP-P01                                               ECS018
00043      S02 IS LCP-P02.                                              ECS018
00044  INPUT-OUTPUT SECTION.                                            ECS018
00045  FILE-CONTROL.                                                    ECS018
00046      SELECT EXTRACT          ASSIGN TO SYS016-UT-FBA1-S-SYS016.   ECS018
00047      SELECT DISK-DATE        ASSIGN TO SYS019-UT-FBA1-S-SYS019.   ECS018
00048      SELECT PRNTR            ASSIGN TO SYS008-UR-1403-S-SYS008.   ECS018
00049      SELECT FICH             ASSIGN TO SYS020-UT-2400-S-SYS020.   ECS018
00050      SELECT SORTFL           ASSIGN TO SYS001-UT-FBA1-S-SORTWK1.  ECS018
00051      SELECT ERMEBL                                                ECS018
00052              ASSIGN SYS024-FBA1-ERMEBL                            ECS018
00053              ORGANIZATION INDEXED                                 ECS018
00054              ACCESS DYNAMIC                                       ECS018
00055              RECORD KEY ME-CONTROL-PRIMARY                        ECS018
00056              FILE STATUS ERMEBL-FILE-STATUS.                      ECS018
00057      SELECT ERACCTT                                               ECS018
00058              ASSIGN SYS025-FBA1-ERACCTT                           ECS018
00059              ORGANIZATION INDEXED                                 ECS018
00060              ACCESS SEQUENTIAL                                    ECS018
00061              RECORD KEY AM-SEQ-KEY                                ECS018
00062              FILE STATUS ERACCT-FILE-STATUS.                      ECS018
00063  EJECT                                                            ECS018
00064  DATA DIVISION.                                                   ECS018
00065  FILE SECTION.                                                    ECS018
00066                                                                   ECS018
00067  FD  EXTRACT                                                      ECS018
00068      BLOCK CONTAINS 0 RECORDS
00069      RECORDING MODE F.                                            ECS018
00070                                                                   ECS018
00071  01  EXTR-RECORD.                                                 ECS018
011410     12  FILLER                      PIC X(165).
00073                                                                      CL**3
00074      EJECT                                                           CL**3
00075  FD  ERACCTT.                                                     ECS018
00076                                                                   ECS018
00077  01  AM-REC.                                                      ECS018
00078      12  FILLER                      PIC XX.                      ECS018
00079      12  AM-SEQ-KEY.                                              ECS018
00080          16  AM-SEQ                  PIC X(20).                   ECS018
00081          16  FILLER                  PIC X(6).                    ECS018
00082      12  FILLER                      PIC X(1972).                 ECS018
00083      EJECT                                                        ECS018
00084  FD  DISK-DATE                                                    ECS018
00085                                      COPY ELCDTEFD.               ECS018
00086                                                                   ECS018
00087  FD  PRNTR                                                        ECS018
00088                                      COPY ELCPRTFD.               ECS018
00089                                                                   ECS018
00090  FD  FICH                                                         ECS018
00091                                      COPY ELCFCHFD.               ECS018
00092      EJECT                                                        ECS018
00093  SD  SORTFL.                                                      ECS018
00094                                                                   ECS018
00095  01  SRT-REC.                                                     ECS018
00096      12  S-PARM                      PIC X(20).                   ECS018
011410     12  FILLER                      PIC X(145).
00098                                                                   ECS018
00099  FD  ERMEBL.                                                      ECS018
00100                                      COPY ERCMEBL.                ECS018
00101      EJECT                                                        ECS018
00102  WORKING-STORAGE SECTION.                                         ECS018
00103  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      ECS018
00104  77  LCP-ASA                       PIC X.                         ECS018
00105  77  FILLER  PIC X(32)   VALUE '********************************'.ECS018
00106  77  FILLER  PIC X(32)   VALUE '     ECS018 WORKING STORAGE     '.ECS018
00107 *77  FILLER  PIC X(32)   VALUE '*********** VMOD=2.011 *********'.ECS018
00108                                                                   ECS018
00109  77  X                               PIC X       VALUE SPACE.     ECS018
00110  77  SPACE-N                         PIC X       VALUE '1'.       ECS018
00111  77  SPACE-1                         PIC X       VALUE ' '.       ECS018
00112  77  SPACE-2                         PIC X       VALUE '0'.       ECS018
00113  77  SPACE-3                         PIC X       VALUE '-'.       ECS018
00114  77  X-NET                           PIC S9(9)V99 VALUE +0 COMP-3.ECS018
00115  77  X-TOTAL                         PIC S9(9)V99 VALUE +0 COMP-3.ECS018
00116  77  X1                              PIC S999    VALUE +0  COMP-3.ECS018
00117  77  X2                              PIC S999    VALUE +0  COMP-3.ECS018
00118  77  R1                              PIC S999    VALUE +0  COMP-3.ECS018
00119  77  R2                              PIC S999    VALUE +0  COMP-3.ECS018
00120  77  AH-SW                           PIC S9      VALUE +0  COMP-3.ECS018
00121  77  LIFE-SW                         PIC S9      VALUE +0  COMP-3.ECS018
00122  77  IND-SW                          PIC S9      VALUE +0  COMP-3.ECS018
00123  77  GRP-SW                          PIC S9      VALUE +0  COMP-3.ECS018
00124  77  PRT-SW                          PIC S9      VALUE +0  COMP-3.ECS018
00125  77  FST-SW                          PIC S9      VALUE +0  COMP-3.ECS018
00126  77  PGCTR                           PIC S9(5)   VALUE +0  COMP-3.ECS018
00127  77  LNCTR                           PIC S999    VALUE +0  COMP-3.ECS018
00128  77  SAVE-X2                         PIC S999    VALUE +0  COMP-3.ECS018
00129  77  HAVE-SEQ                        PIC X(20)   VALUE SPACES.    ECS018
00130  77  NEED-SEQ                        PIC X(19)   VALUE SPACES.    ECS018
00131  77  SAVE-NAME                       PIC X(30)   VALUE SPACES.    ECS018
00132  77  HOLD-BASE                       PIC Z(7).99 VALUE ZEROS.     ECS018
00133  77  HOLD-OVER                       PIC Z(7).99 VALUE ZEROS.     ECS018
00134  77  HOLD-AMT                        PIC Z(7).99 VALUE ZEROS.     ECS018
00135  77  AMT-Y                           PIC S9(7)V99 VALUE ZEROS.    ECS018
00136  77  AMT-1                           PIC S9(7)V99 VALUE ZEROS.    ECS018
070714 77  AMT-Y-OW                        PIC S9(7)V99 VALUE ZEROS.    ECS018
070714 77  AMT-1-OW                        PIC S9(7)V99 VALUE ZEROS.    ECS018
00137                                                                   ECS018
070714****=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=***
070714****                                                           ***
070714****   Month end balancing work area                           ***
070714****                                                           ***
070714****=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=***

00138  01  MONTH-END-DATA.                                              ECS018
00139      12  ME-START-DATE.                                           ECS018
00140          16  ME-START-MO             PIC 99.                      ECS018
00141          16  FILLER                  PIC X.                       ECS018
00142          16  ME-START-DA             PIC 99.                      ECS018
00143          16  FILLER                  PIC X.                       ECS018
00144          16  ME-START-YR             PIC 99.                      ECS018
00145      12  ME-CNDS-DATE                PIC 9(6).                    ECS018
00146      12  ME-CNDS-DATE-R REDEFINES ME-CNDS-DATE.                   ECS018
00147          16  ME-CNDS-MO              PIC 99.                      ECS018
00148          16  ME-CNDS-DA              PIC 99.                      ECS018
00149          16  ME-CNDS-YR              PIC 99.                      ECS018
00150      12  ME-START-TIME               PIC 9(6).                    ECS018
00151      12  ME-UPDATE-FLAG              PIC X       VALUE 'Y'.       ECS018
00152          88  ME-DO-UPDATE                        VALUE 'Y'.       ECS018
00153          88  ME-NO-UPDATE                        VALUE 'N'.       ECS018
00154      12  ERMEBL-FILE-STATUS          PIC XX.                      ECS018
00155      12  ERACCT-FILE-STATUS          PIC XX.                      ECS018
00156      12  MONTH-END-MOYR              PIC S9(5) COMP-3.            ECS018
073114     12  HLD-018-COMM-Y          PIC S9(9)V99  COMP-3 VALUE +0.
073114     12  HLD-018-COMM-1          PIC S9(9)V99  COMP-3 VALUE +0.
073114     12  HLD-018-OW-Y            PIC S9(9)V99  COMP-3 VALUE +0.
073114     12  HLD-018-OW-1            PIC S9(9)V99  COMP-3 VALUE +0.
00157  01  WS.                                                          ECS018
00158      12  WS-RETURN-CODE              PIC S9(4)              COMP. ECS018
00159      12  WS-ABEND-MESSAGE            PIC X(80).                   ECS018
00160      12  WS-ABEND-FILE-STATUS        PIC XX      VALUE ZEROS.     ECS018
00161      12  WS-ZERO                     PIC S9      VALUE +0  COMP-3.ECS018
00162      12  PGM-SUB                     PIC S999    VALUE +018 COMP. ECS018
00163                                                                      CL**2
00164      12  WS-PROCESSED-DATE.                                          CL**2
00165          16  FILLER                PIC 9.                            CL**2
00166          16  W-CC                  PIC 99.                           CL**2
00167          16  W-YR                  PIC 99.                           CL**2
00168          16  W-MO                  PIC 99.                           CL**2
00169      12  WS-PROCESSED-NUMERIC  REDEFINES                             CL**2
00170          WS-PROCESSED-DATE         PIC 9(7).                         CL**2
00171                                                                      CL**2
00172      EJECT                                                        ECS018
00173                                      COPY ELCDTECX.               ECS018
00174      EJECT                                                        ECS018
00175                                      COPY ELCDTEVR.               ECS018
00176      EJECT                                                        ECS018
00177                                      COPY ERCACCT.                ECS018
00178  EJECT                                                            ECS018
00179  01  HD1.                                                         ECS018
00180      12  FILLER                      PIC X(44)   VALUE SPACES.    ECS018
00181      12  FILLER                      PIC X(35)   VALUE            ECS018
00182          'PREMIUM AND COMMISSION RECALCULATED'.                   ECS018
00183      12  FILLER                      PIC X(40)   VALUE SPACES.    ECS018
00184      12  FILLER                      PIC X(6)    VALUE 'ECS018'.  ECS018
00185                                                                   ECS018
00186  01  HD2.                                                         ECS018
00187      12  SUB-HD2                     PIC X(47)   VALUE SPACES.    ECS018
00188      12  HD-CO                       PIC X(30).                   ECS018
00189      12  FILLER                      PIC X(42)   VALUE SPACES.    ECS018
00190      12  HD-RD                       PIC X(8).                    ECS018
00191                                                                   ECS018
00192  01  HD3.                                                         ECS018
00193      12  SUB-HD3                     PIC X(53)   VALUE SPACES.    ECS018
00194      12  HD-DT                       PIC X(18).                   ECS018
00195      12  FILLER                      PIC X(48)   VALUE SPACES.    ECS018
00196      12  FILLER                      PIC X(5)    VALUE 'PAGE '.   ECS018
00197      12  HD-PAGE                     PIC ZZ,ZZ9.                  ECS018
00198                                                                   ECS018
00199  01  HD4.                                                         ECS018
00200      12 HD-DESC                      PIC X(27).                   ECS018
00201      12 FILLER                       PIC X(45)   VALUE            ECS018
00202      '     * * * * * *  P R E M I U M  * * * * * * '.             ECS018
00203      12 FILLER                       PIC X(45)   VALUE            ECS018
00204      '     * * * *  C O M M I S S I O N  * * * * * '.             ECS018
00205      12 FILLER                       PIC X(15)   VALUE            ECS018
00206         '        CLAIMS '.                                        ECS018
00207                                                                   ECS018
00208  01  HD5.                                                         ECS018
00209      12 FILLER                       PIC X(27)   VALUE SPACES.    ECS018
00210      12 FILLER                       PIC X(45)   VALUE            ECS018
00211      '       WRITTEN      CANCELLED            NET '.             ECS018
00212      12 FILLER                       PIC X(45)   VALUE            ECS018
00213      '          BASE            O/W          TOTAL '.             ECS018
00214      12 FILLER                       PIC X(15)   VALUE SPACES.    ECS018
00215                                                                   ECS018
00216  01  SUB-HEADINGS.                                                ECS018
00217      03 HEAD2.                                                    ECS018
00218         05 FILLER                    PIC X(40)   VALUE            ECS018
00219      'CARR  GROUP  STATE                  ACCT'.                  ECS018
00220      03 ACCT-HDA REDEFINES HEAD2.                                 ECS018
00221         05 ACCT-HD2                  PIC X(40).                   ECS018
00222      03 ST-HDA REDEFINES HEAD2.                                   ECS018
00223         05 ST-HD2                    PIC X(28).                   ECS018
00224         05 FILLER                    PIC X(12).                   ECS018
00225      03 CO-HDA REDEFINES HEAD2.                                   ECS018
00226         05 CO-HD2                    PIC X(11).                   ECS018
00227         05 FILLER                    PIC X(29).                   ECS018
00228      03 CARR-HDA REDEFINES HEAD2.                                 ECS018
00229         05 CARR-HD2                  PIC X(4).                    ECS018
00230         05 FILLER                    PIC X(36).                   ECS018
00231                                                                   ECS018
00232      03 HEAD3.                                                    ECS018
00233         05 FILLER                    PIC XX      VALUE SPACES.    ECS018
00234         05 HD-CARR                   PIC X.                       ECS018
00235         05 FILLER                    PIC XXX     VALUE SPACES.    ECS018
00236         05 HD-GROUP                  PIC X(6).                    ECS018
00237         05 FILLER                    PIC X       VALUE SPACES.    ECS018
00238         05 HD-ST                     PIC XX.                      ECS018
00239         05 FILLER                    PIC X       VALUE SPACES.    ECS018
00240         05 HD-ST-NM                  PIC X(15).                   ECS018
00241         05 FILLER                    PIC XX      VALUE SPACES.    ECS018
00242         05 HD-ACCT                   PIC X(10).                   ECS018
00243      03 ACCT-HDB REDEFINES HEAD3.                                 ECS018
00244         05 ACCT-HD3                  PIC X(43).                   ECS018
00245      03 ST-HDB REDEFINES HEAD3.                                   ECS018
00246         05 ST-HD3                    PIC X(28).                   ECS018
00247         05 FILLER                    PIC X(15).                   ECS018
00248      03 CO-HDB REDEFINES HEAD3.                                   ECS018
00249         05 CO-HD3                    PIC X(13).                   ECS018
00250         05 FILLER                    PIC X(30).                   ECS018
00251      03 CARR-HDB REDEFINES HEAD3.                                 ECS018
00252         05 CARR-HD3                  PIC X(4).                    ECS018
00253         05 FILLER                    PIC X(39).                   ECS018
00254                                                                   ECS018
00255      03 SUMM-HD.                                                  ECS018
00256         05 HD-SUMM-CARR-DESC.                                     ECS018
00257            07 FILLER                 PIC X(8)    VALUE 'CARRIER '.ECS018
00258            07 HD-SUMM-CARR           PIC XXX.                     ECS018
00259            07 HD-SUMM-FINAL-DESC.                                 ECS018
00260               09 FILLER              PIC X(8)    VALUE '  STATE '.ECS018
00261               09 HD-SUMM-ST          PIC XXX.                     ECS018
00262               09 HD-SUMM-ST-NM       PIC X(15).                   ECS018
00263  EJECT                                                            ECS018
00264  01  WORK-REC.                                                    ECS018
00265      12  W-SEQ.                                                   ECS018
00266          16  W-CARR                PIC X.                         ECS018
00267          16  W-GROUP               PIC X(6).                      ECS018
00268          16  W-ST                  PIC XX.                        ECS018
00269          16  W-ACCT                PIC X(10).                     ECS018
00270          16  W-IG                  PIC 9.                         ECS018
00271          16  W-TYPE                PIC XXX.                       ECS018
00272      12  W-CODE                    PIC 9.                         ECS018
00273      12  W-AMTS.
00274          16  W-AMT                 PIC S9(9)V99  COMP-3.          ECS018
00275          16  W-BASE                PIC S9(7)V99  COMP-3.          ECS018
00276          16  W-OVER                PIC S9(7)V99  COMP-3.          ECS018
PEMMOD         16  W-DLR-INC             PIC S9(7)V99  COMP-3.
011410         16  W-LF-LMBA-FEE         PIC S9(7)V99  COMP-3.
011410         16  W-AH-LMBA-FEE         PIC S9(7)V99  COMP-3.
PEMMOD         16  W-BANK-FEE            PIC S9(7)V99  COMP-3.
070714         16  W-CSO-ADMIN           PIC S9(7)V99  COMP-3.
00277      12  W-PROCESSED               PIC 9(7)      COMP-3.             CL**2
00278      12  W-RECALC                  PIC X.                         ECS018
00279      12  W-ACCT-COM-TYPE           PIC X.                            CL**3
00280      12  W-OWRT-COM-TYPE           PIC X.                            CL**3
00281      12  W-DMD-RESIDENT-ST         PIC XX.                           CL**3
00282      12  FILLER                    PIC X.                            CL**3
011410     12  W-SPPDD-CLP         PIC S9(7)V99   COMP-3.
070714     12  FILLER              PIC X(85).
00283                                                                   ECS018
00284  01  CUR-SEQ.                                                     ECS018
00285      12  CUR-CARR                  PIC X.                         ECS018
00286      12  CUR-GROUP                 PIC X(6).                      ECS018
00287      12  CUR-ST                    PIC XX.                        ECS018
00288      12  CUR-ACCT                  PIC X(10).                     ECS018
00289      12  CUR-IG                    PIC 9.                         ECS018
00290      12  CUR-TYPE.                                                ECS018
00291          16  CUR-TYP               PIC XX.                        ECS018
00292          16  CUR-OB                PIC X.                         ECS018
00293                                                                   ECS018
00294  01  BLD-DESC.                                                    ECS018
00295      12  BLD-TOTAL                 PIC X(6)    VALUE SPACES.      ECS018
00296      12  BLD-DESC1                 PIC XX      VALUE SPACES.      ECS018
00297      12  FILLER                    PIC XX      VALUE SPACES.      ECS018
00298      12  B-SLASH1                  PIC X       VALUE SPACE.       ECS018
00299      12  FILLER                    PIC XX      VALUE SPACES.      ECS018
00300      12  BLD-DESC2                 PIC XX      VALUE SPACES.      ECS018
00301      12  FILLER                    PIC X       VALUE SPACE.       ECS018
00302                                                                   ECS018
00303  01  X-ZERO.                                                      ECS018
092602*    12  FILLER                    PIC X(27000).                  ECS018
092602     12  FILLER                    PIC X(54000).                  ECS018
00305                                                                   ECS018
00306  01  X-ZERO-5.                                                    ECS018
00307      12  FILLER                    PIC X(30).                     ECS018
00308                                                                   ECS018
00309  01  WX-POINTER.                                                  ECS018
00310      12  WX-LAH                    PIC 9.                         ECS018
00311      12  WX-TYPE.                                                 ECS018
00312          16  WX-TYP                PIC XX.                        ECS018
00313          16  WX-OB                 PIC X.                         ECS018
00314                                                                   ECS018
00315  01  WX-DESC.                                                     ECS018
00316      12  WX-DESC-10                PIC X(10).                     ECS018
00317      12  FILLER                    PIC XX.                        ECS018
00318      12  WX-DESC-OB                PIC X(4).                      ECS018
00319                                                                   ECS018
00320  01  X-POINTERS.                                                  ECS018
092602*    05  X-POINTER OCCURS 450 TIMES.                              ECS018
092602     05  X-POINTER OCCURS 900 TIMES.                              ECS018
00322          07  X-LAH                 PIC 9.                         ECS018
00323          07  X-TYPE.                                              ECS018
00324              09  X-TYP             PIC XX.                        ECS018
00325              09  X-OB              PIC X.                         ECS018
00326                                                                   ECS018
00327  01  X-DESCRIPTIONS.                                              ECS018
092602*    12  X-DESC OCCURS 450 TIMES.                                 ECS018
092602     12  X-DESC OCCURS 900 TIMES.                                 ECS018
00329          16  X-DESC-10             PIC X(10).                     ECS018
00330                                                                   ECS018
00331  01  X-TOTALS.                                                    ECS018
00332      12  X-TOTS OCCURS 2 TIMES.                                   ECS018
092602*        16  X-LEVEL OCCURS 450 TIMES.                            ECS018
092602         16  X-LEVEL OCCURS 900 TIMES.                            ECS018
00334              20  X-AMTS.                                          ECS018
00335                  24  FILLER        PIC X(30).                     ECS018
00336                                                                   ECS018
00337  01  X-DETL.                                                      ECS018
00338      12  X-ISSUE                   PIC S9(9)V99 VALUE +0 COMP-3.  ECS018
00339      12  X-CANCL                   PIC S9(9)V99 VALUE +0 COMP-3.  ECS018
00340      12  X-BASE                    PIC S9(9)V99 VALUE +0 COMP-3.  ECS018
00341      12  X-OVER                    PIC S9(9)V99 VALUE +0 COMP-3.  ECS018
00342      12  X-CLAIM                   PIC S9(9)V99 VALUE +0 COMP-3.  ECS018
00343                                                                   ECS018
00344  01  R-TOTALS.                                                    ECS018
00345      12  R-TOTS OCCURS 2 TIMES.                                   ECS018
092602*        16  R-LEVEL OCCURS 450 TIMES.                            ECS018
092602         16  R-LEVEL OCCURS 900 TIMES.                            ECS018
00347              20  R-AMTS.                                          ECS018
00348                  24  FILLER        PIC X(30).                     ECS018
00349                                                                   ECS018
00350  01  R-DETL.                                                      ECS018
00351      12  R-ISSUE                   PIC S9(9)V99 VALUE +0 COMP-3.  ECS018
00352      12  R-CANCL                   PIC S9(9)V99 VALUE +0 COMP-3.  ECS018
00353      12  R-BASE                    PIC S9(9)V99 VALUE +0 COMP-3.  ECS018
00354      12  R-OVER                    PIC S9(9)V99 VALUE +0 COMP-3.  ECS018
00355      12  R-CLAIM                   PIC S9(9)V99 VALUE +0 COMP-3.  ECS018
00356                                                                   ECS018
00357  01  ST-TOTALS.                                                   ECS018
092602     12  FILLER                    PIC X(54000).                  ECS018
00359                                                                   ECS018
00360  01  CO-TOTALS.                                                   ECS018
092602     12  FILLER                    PIC X(54000).                  ECS018
00362                                                                   ECS018
00363  01  CARR-TOTALS.                                                 ECS018
092602     12  FILLER                    PIC X(54000).                  ECS018
00365                                                                   ECS018
00366  01  FINAL-TOTALS.                                                ECS018
092602     12  FILLER                    PIC X(54000).                  ECS018
00368                                                                   ECS018
00369  01  SUB-TOTALS.                                                  ECS018
00370      12  S-ISSUE                   PIC S9(9)V99 VALUE +0 COMP-3.  ECS018
00371      12  S-CANCL                   PIC S9(9)V99 VALUE +0 COMP-3.  ECS018
00372      12  S-BASE                    PIC S9(9)V99 VALUE +0 COMP-3.  ECS018
00373      12  S-OVER                    PIC S9(9)V99 VALUE +0 COMP-3.  ECS018
00374      12  S-CLAIM                   PIC S9(9)V99 VALUE +0 COMP-3.  ECS018
00375                                                                   ECS018
00376  01  TOTALS.                                                      ECS018
00377      12  T-ISSUE                   PIC S9(9)V99 VALUE +0 COMP-3.  ECS018
00378      12  T-CANCL                   PIC S9(9)V99 VALUE +0 COMP-3.  ECS018
00379      12  T-BASE                    PIC S9(9)V99 VALUE +0 COMP-3.  ECS018
00380      12  T-OVER                    PIC S9(9)V99 VALUE +0 COMP-3.  ECS018
00381      12  T-CLAIM                   PIC S9(9)V99 VALUE +0 COMP-3.  ECS018
00382                                                                   ECS018
00383  01  P-REC.                                                       ECS018
00384      12  P-CCSW                    PIC X.                         ECS018
00385      12  P-LN.                                                    ECS018
00386          16  P-TYPE                PIC X(4).                      ECS018
00387          16  P-DESC                PIC X(16).                     ECS018
00388          16  FILLER                PIC X(7).                      ECS018
00389          16  P-ISSUE               PIC ZZZ,ZZZ,ZZZ.ZZ-.           ECS018
00390          16  P-CANCL               PIC ZZZ,ZZZ,ZZZ.ZZ-.           ECS018
00391          16  P-NET                 PIC ZZZ,ZZZ,ZZZ.ZZ-.           ECS018
00392          16  P-BASE                PIC ZZZ,ZZZ,ZZZ.ZZ-.           ECS018
00393          16  P-OVER                PIC ZZZ,ZZZ,ZZZ.ZZ-.           ECS018
00394          16  P-TOTAL               PIC ZZZ,ZZZ,ZZZ.ZZ-.           ECS018
00395          16  P-CLAIM               PIC ZZZ,ZZZ,ZZZ.ZZ-.           ECS018
00396  01  P-DET-Y.                                                     ECS018
00397      12  FILLER                    PIC X(32)   VALUE SPACES.      ECS018
00398      12  FILLER                    PIC X(60)   VALUE              ECS018
00399          'RECALC COMMISSIONS APPEARING ON BILLING STATEMENTS '.   ECS018
00400      12  FILLER                    PIC X(10)   VALUE SPACES.      ECS018
00401      12  P-AMT-Y                   PIC ZZZ,ZZZ,ZZZ.99-.           ECS018
00402  01  P-DET-1.                                                     ECS018
00403      12  FILLER                    PIC X(32)   VALUE SPACES.      ECS018
00404      12  FILLER                    PIC X(60)   VALUE              ECS018
00405          'RECALC COMMISSIONS NOT APPEARING ON BILLING STATEMENTS'.ECS018
00406      12  FILLER                    PIC X(10)   VALUE SPACES.      ECS018
00407      12  P-AMT-1                   PIC ZZZ,ZZZ,ZZZ.99-.           ECS018
070714 01  P-OW-Y.                                                      ECS018
070714     12  FILLER                    PIC X(32)   VALUE SPACES.      ECS018
070714     12  FILLER                    PIC X(60)   VALUE              ECS018
070714         'O/W RECALC COMMS APPEARING ON BILLING STATEMENTS '.     ECS018
070714     12  FILLER                    PIC X(10)   VALUE SPACES.      ECS018
070714     12  P-AMT-Y-OW                PIC ZZZ,ZZZ,ZZZ.99-.           ECS018
070714 01  P-OW-1.                                                      ECS018
070714     12  FILLER                    PIC X(32)   VALUE SPACES.      ECS018
070714     12  FILLER                    PIC X(60)   VALUE              ECS018
070714         'O/W RECALC COMMS NOT APPEARING ON BILLING STATEMENTS'.  ECS018
070714     12  FILLER                    PIC X(10)   VALUE SPACES.      ECS018
070714     12  P-AMT-1-OW                PIC ZZZ,ZZZ,ZZZ.99-.           ECS018
00408  EJECT                                                            ECS018
00409  PROCEDURE DIVISION.                                              ECS018
00410                                                                   ECS018
00411  CAPTURE-START.                                                   ECS018
00418                                                                   ECS018
00419  0100-SET-START.                                                  ECS018
00420                                  COPY ELCDTERX SUPPRESS.          ECS018
00421                                                                   ECS018
070714****=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=***
070714****                                                           ***
070714****   Set up the month-end auto balancing.                    ***
070714****                                                           ***
070714****=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=***

00422      MOVE WS-TIME                TO  ME-START-TIME.               ECS018
00423      MOVE WS-CURRENT-DATE        TO  ME-START-DATE.               ECS018
00424      MOVE ME-START-MO            TO  ME-CNDS-MO.                  ECS018
00425      MOVE ME-START-DA            TO  ME-CNDS-DA.                  ECS018
00426      MOVE ME-START-YR            TO  ME-CNDS-YR.                  ECS018
00436      MOVE WS-CURRENT-DATE        TO  HD-RD.                       ECS018
00437      MOVE COMPANY-NAME           TO  HD-CO.                       ECS018
00438      MOVE ALPH-DATE              TO  HD-DT.                       ECS018
00439                                                                   ECS018
00440  0110-SORT-RTN SECTION.                                           ECS018
00441      SORT SORTFL ON ASCENDING KEY S-PARM                          ECS018
00442          INPUT  PROCEDURE 0120-INPUT-RTN  THRU 0160-EXIT          ECS018
00443          OUTPUT PROCEDURE 0180-OUTPUT-RTN THRU 0810-EXIT.         ECS018
00444                                                                   ECS018
00445      IF SORT-RETURN NOT = ZEROS                                   ECS018
00446          MOVE +0101                      TO  WS-RETURN-CODE       ECS018
00447          MOVE 'INTERNAL SORT ABORTED'    TO  WS-ABEND-MESSAGE     ECS018
00448          GO TO ABEND-PGM.                                         ECS018
00449                                                                   ECS018
00450      GO TO 0830-EOJ-RTN.                                          ECS018
00451      EJECT                                                        ECS018
00452  0120-INPUT-RTN SECTION.                                          ECS018
00453      OPEN INPUT EXTRACT.                                          ECS018
00454                                                                   ECS018
00455  0130-READ-RTN.                                                   ECS018
00456      READ EXTRACT                                                 ECS018
00457          AT END GO TO 0150-END-INPUT.                             ECS018
00458                                                                   ECS018
00459      MOVE EXTR-RECORD            TO  SRT-REC.                     ECS018
00460                                                                   ECS018
00461  0140-RLS-RTN.                                                    ECS018
00462      RELEASE SRT-REC.                                             ECS018
00463      GO TO 0130-READ-RTN.                                         ECS018
00464                                                                   ECS018
00465  0150-END-INPUT.                                                  ECS018
00466      CLOSE EXTRACT.                                               ECS018
00467                                                                   ECS018
00468  0160-EXIT.                                                       ECS018
00469       EXIT.                                                       ECS018
00470  EJECT                                                            ECS018
00471  0180-OUTPUT-RTN SECTION.                                         ECS018
00472      OPEN INPUT ERACCTT                                           ECS018
00473          OUTPUT PRNTR.                                            ECS018
00474                                                                   ECS018
00475      IF ERACCT-FILE-STATUS  = '00' OR '97'                        ECS018
00476          NEXT SENTENCE                                            ECS018
00477        ELSE                                                       ECS018
00478          MOVE +0302                      TO  WS-RETURN-CODE       ECS018
00479          MOVE 'ERROR OPENING ACCT MSTR'  TO  WS-ABEND-MESSAGE     ECS018
00480          GO TO ABEND-PGM.                                         ECS018
00481                                                                   ECS018
00482      MOVE SPACES                 TO  P-REC.                       ECS018
00483      MOVE LOW-VALUES             TO  CUR-SEQ                      ECS018
00484                                      AM-SEQ                       ECS018
00485                                      HAVE-SEQ                     ECS018
00486                                      NEED-SEQ.                    ECS018
00487                                                                   ECS018
00488      PERFORM 0690-FORMAT-RTN  THRU 0740-EXIT.                     ECS018
00489      PERFORM 0430-RD-ACCT-RTN THRU 0440-EXIT 2 TIMES.             ECS018
00490      MOVE 0                      TO  FST-SW PGCTR.                ECS018
00491      MOVE +066                   TO  LNCTR.                       ECS018
00492                                                                   ECS018
00493  0190-RETURN-LOOP.                                                ECS018
00494      RETURN SORTFL                                                ECS018
00495          AT END GO TO 0800-END-OUTPUT.                            ECS018
00496                                                                   ECS018
00497      MOVE SRT-REC                TO  WORK-REC.                    ECS018
00498      MOVE W-PROCESSED            TO  WS-PROCESSED-NUMERIC.           CL**3
00499                                                                   ECS018
00500      IF W-CARR  =  HIGH-VALUE OR                                  ECS018
00501         W-GROUP =  HIGH-VALUE                                     ECS018
00502          NEXT SENTENCE                                            ECS018
00503      ELSE                                                         ECS018
00504          IF W-RECALC  =  'Y'                                      ECS018
00505              ADD W-BASE                  TO  AMT-Y                ECS018
070714             ADD W-OVER                  TO  AMT-Y-OW             ECS018
00507          ELSE                                                     ECS018
00508              ADD W-BASE                  TO  AMT-1                ECS018
070714             ADD W-OVER                  TO  AMT-1-OW.            ECS018
00510                                                                   ECS018
00511      IF W-SEQ NOT = CUR-SEQ                                       ECS018
00512          PERFORM 0230-BREAK-RTN THRU 0410-EXIT.                   ECS018
00513                                                                   ECS018
00514      GO TO 0200-ACCUM-ISSUE                                       ECS018
00515            0210-ACCUM-CANCL                                       ECS018
00516            0220-ACCUM-CLAIM                                       ECS018
00517         DEPENDING ON W-CODE.                                      ECS018
00518                                                                   ECS018
00519      DISPLAY 'INVALID CODE IN EXTRACT REC - ' W-CODE.             ECS018
00520      MOVE +0301                             TO  WS-RETURN-CODE.   ECS018
00521      MOVE 'INVALID CODE IN EXTRACT RECORD'  TO  WS-ABEND-MESSAGE. ECS018
00522      GO TO ABEND-PGM.                                             ECS018
00523      EJECT                                                        ECS018
00524  0200-ACCUM-ISSUE.                                                ECS018
00525      ADD W-AMT                   TO  X-ISSUE.                     ECS018
00526      ADD W-BASE                  TO  X-BASE.                      ECS018
00527      ADD W-OVER                  TO  X-OVER.                      ECS018
00528      GO TO 0190-RETURN-LOOP.                                      ECS018
00529                                                                   ECS018
00530  0210-ACCUM-CANCL.                                                ECS018
00531      ADD W-AMT                   TO  X-CANCL.                     ECS018
00532      ADD W-BASE                  TO  X-BASE.                      ECS018
00533      ADD W-OVER                  TO  X-OVER.                      ECS018
00534      GO TO 0190-RETURN-LOOP.                                      ECS018
00535                                                                   ECS018
00536  0220-ACCUM-CLAIM.                                                ECS018
00537      ADD W-AMT                   TO  X-CLAIM.                     ECS018
00538      GO TO 0190-RETURN-LOOP.                                      ECS018
00539                                                                   ECS018
00540  0230-BREAK-RTN.                                                  ECS018
00541      IF FST-SW = 0                                                ECS018
00542          MOVE 1                  TO  FST-SW                       ECS018
00543          GO TO 0330-INTL-FINAL.                                   ECS018
00544                                                                   ECS018
00545  0240-LINE-BREAK.                                                 ECS018
00546      MOVE X-DETL                 TO  X-AMTS (X1 X2).              ECS018
00547      MOVE W-TYPE                 TO  CUR-TYPE.                    ECS018
00548      MOVE W-IG                   TO  CUR-IG.                      ECS018
00549      IF CUR-SEQ = W-SEQ                                           ECS018
00550          GO TO 0390-INTL-LINE.                                    ECS018
00551                                                                   ECS018
00552      IF CUR-CARR = HIGH-VALUE                                     ECS018
00553          GO TO 0310-FINAL-SUMMARY.                                ECS018
00554                                                                   ECS018
00555      IF CUR-ACCT = HIGH-VALUE                                     ECS018
00556          GO TO 0290-CARR-SUMMARY.                                 ECS018
00557                                                                   ECS018
00558  0250-ACCT-BREAK.                                                 ECS018
00559      MOVE CUR-SEQ                TO  NEED-SEQ.                    ECS018
00560                                                                   ECS018
00561  0260-FIND-ACCT.                                                  ECS018
00562      IF NEED-SEQ GREATER THAN AM-CONTROL-A                        ECS018
00563          PERFORM 0430-RD-ACCT-RTN THRU 0440-EXIT                  ECS018
00564          GO TO 0260-FIND-ACCT.                                    ECS018
00565                                                                   ECS018
00566      IF NEED-SEQ = AM-CONTROL-A                                   ECS018
00567          MOVE AM-NAME            TO  SAVE-NAME                    ECS018
00568      ELSE                                                         ECS018
00569          MOVE 'INVALID ACCOUNT'  TO  SAVE-NAME.                   ECS018
00570                                                                   ECS018
00571 *  TOTAL LEVEL 1.                                                 ECS018
00572      MOVE ACCT-HD2               TO  SUB-HD2.                     ECS018
00573      MOVE ACCT-HD3               TO  SUB-HD3.                     ECS018
00574      MOVE ST-TOTALS              TO  R-TOTALS.                    ECS018
00575      PERFORM 0640-ROLL-RTN THRU 0680-EXIT.                        ECS018
00576                                                                   ECS018
00577      MOVE R-TOTALS               TO  ST-TOTALS.                   ECS018
00578                                                                   ECS018
00579      IF DTE-PGM-OPT NOT GREATER THAN '1'                          ECS018
00580          PERFORM 0460-BLD-RTN THRU 0490-EXIT.                     ECS018
00581                                                                   ECS018
00582      MOVE SPACES                 TO  SAVE-NAME.                   ECS018
00583      MOVE W-ACCT                 TO  CUR-ACCT.                    ECS018
00584                                                                   ECS018
00585      IF CUR-SEQ = W-SEQ                                           ECS018
00586          GO TO 0380-INTL-ACCT.                                    ECS018
00587                                                                   ECS018
00588  0270-ST-BREAK.                                                   ECS018
00589 *  TOTAL LEVEL 2.                                                 ECS018
00590      MOVE ST-HD2                 TO  SUB-HD2.                     ECS018
00591      MOVE ST-HD3                 TO  SUB-HD3.                     ECS018
00592      MOVE ST-TOTALS              TO  X-TOTALS.                    ECS018
00593      MOVE CO-TOTALS              TO  R-TOTALS.                    ECS018
00594      PERFORM 0640-ROLL-RTN THRU 0680-EXIT.                        ECS018
00595      MOVE R-TOTALS               TO  CO-TOTALS.                   ECS018
00596                                                                   ECS018
00597      IF DTE-PGM-OPT NOT GREATER THAN '2'                          ECS018
00598          PERFORM 0460-BLD-RTN THRU 0490-EXIT.                     ECS018
00599                                                                   ECS018
00600      MOVE W-ST                   TO  CUR-ST.                      ECS018
00601      IF CUR-SEQ = W-SEQ                                           ECS018
00602          GO TO 0360-INTL-ST.                                      ECS018
00603                                                                   ECS018
00604  0280-CO-BREAK.                                                   ECS018
00605 *  TOTAL LEVEL 3.                                                 ECS018
00606      MOVE CO-HD2                 TO  SUB-HD2.                     ECS018
00607      MOVE CO-HD3                 TO  SUB-HD3.                     ECS018
00608      MOVE CO-TOTALS              TO  X-TOTALS.                    ECS018
00609      MOVE CARR-TOTALS            TO  R-TOTALS.                    ECS018
00610      PERFORM 0640-ROLL-RTN THRU 0680-EXIT.                        ECS018
00611      MOVE R-TOTALS               TO  CARR-TOTALS.                 ECS018
00612                                                                   ECS018
00613      IF DTE-PGM-OPT NOT GREATER THAN '3'                          ECS018
00614          PERFORM 0460-BLD-RTN THRU 0490-EXIT.                     ECS018
00615                                                                   ECS018
00616      GO TO 0350-INTL-CO.                                          ECS018
00617                                                                   ECS018
00618  0290-CARR-SUMMARY.                                               ECS018
00619 *  TOTAL LEVEL 4.                                                 ECS018
013107     MOVE 'CARRIER, STATE SUMMARY'
013107                                 TO SUB-HD2
00621      MOVE HD-SUMM-CARR-DESC      TO  SUB-HD3.                     ECS018
00622                                                                   ECS018
00623      IF DTE-PGM-OPT NOT GREATER THAN '4'                          ECS018
00624          PERFORM 0460-BLD-RTN THRU 0490-EXIT.                     ECS018
00625                                                                   ECS018
00626      IF W-CARR = CUR-CARR                                         ECS018
00627          GO TO 0350-INTL-CO.                                      ECS018
00628                                                                   ECS018
00629  0300-CARR-BREAK.                                                 ECS018
00630 *  TOTAL LEVEL 5.                                                 ECS018
00631      MOVE CARR-HD2               TO  SUB-HD2.                     ECS018
013107     MOVE 'CARRIER SUMMARY'      TO  SUB-HD2
00632      MOVE CARR-HD3               TO  SUB-HD3.                     ECS018
00633      MOVE CARR-TOTALS            TO  X-TOTALS.                    ECS018
00634      MOVE FINAL-TOTALS           TO  R-TOTALS.                    ECS018
00635      PERFORM 0640-ROLL-RTN THRU 0680-EXIT.                        ECS018
00636      MOVE R-TOTALS               TO  FINAL-TOTALS.                ECS018
00637                                                                   ECS018
00638      IF DTE-PGM-OPT NOT GREATER THAN '5'                          ECS018
00639          PERFORM 0460-BLD-RTN THRU 0490-EXIT.                     ECS018
00640                                                                   ECS018
00641      GO TO 0340-INTL-CARR.                                        ECS018
00642                                                                   ECS018
00643  0310-FINAL-SUMMARY.                                              ECS018
00644 *  TOTAL LEVEL 6.                                                 ECS018
00645      MOVE 'FINAL SUMMARY'        TO  SUB-HD2.                     ECS018
00646      MOVE HD-SUMM-FINAL-DESC     TO  SUB-HD3.                     ECS018
00647                                                                   ECS018
00648      IF DTE-PGM-OPT NOT GREATER THAN '6'                          ECS018
00649          PERFORM 0460-BLD-RTN THRU 0490-EXIT.                     ECS018
00650                                                                   ECS018
00651      IF FST-SW NOT = 2                                            ECS018
00652          GO TO 0340-INTL-CARR.                                    ECS018
00653                                                                   ECS018
00654  0320-FINAL-BREAK.                                                ECS018
00655 *  TOTAL LEVEL 7.                                                 ECS018
00656      MOVE 'FINAL TOTALS'         TO  SUB-HD2.                     ECS018
00657      MOVE SPACES                 TO  SUB-HD3.                     ECS018
00658      MOVE FINAL-TOTALS           TO  X-TOTALS.                    ECS018
00659                                                                   ECS018
00660      IF DTE-PGM-OPT NOT GREATER THAN '7'                          ECS018
00661          PERFORM 0460-BLD-RTN THRU 0490-EXIT.                     ECS018
00662                                                                   ECS018
00663      GO TO 0410-EXIT.                                             ECS018
00664                                                                   ECS018
00665  0330-INTL-FINAL.                                                 ECS018
00666      PERFORM 0600-ZERO-RTN THRU 0630-EXIT.                        ECS018
00667      MOVE X-ZERO                 TO  FINAL-TOTALS.                ECS018
00668                                                                   ECS018
00669  0340-INTL-CARR.                                                  ECS018
00670      MOVE W-CARR                 TO  CUR-CARR                     ECS018
00671                                      HD-CARR                      ECS018
00672                                      HD-SUMM-CARR.                ECS018
00673      MOVE X-ZERO                 TO  CARR-TOTALS.                 ECS018
00674                                                                   ECS018
00675  0350-INTL-CO.                                                    ECS018
00676      MOVE W-GROUP                TO  CUR-GROUP HD-GROUP.          ECS018
00677      MOVE X-ZERO                 TO  CO-TOTALS.                   ECS018
00678                                                                   ECS018
00679  0360-INTL-ST.                                                    ECS018
00680      MOVE W-ST                   TO  CUR-ST                       ECS018
00681                                      HD-ST                        ECS018
00682                                      HD-SUMM-ST                   ECS018
00683                                      STATE-L.                     ECS018
00684      MOVE X-ZERO                 TO  ST-TOTALS.                   ECS018
00685                                                                   ECS018
00686      IF CLAS-MAXS NOT GREATER ZEROS                               ECS018
00687          MOVE 'INVALID STATE'    TO  HD-ST-NM HD-SUMM-ST-NM       ECS018
00688              GO TO 0380-INTL-ACCT.                                ECS018
00689                                                                   ECS018
00690      MOVE CLAS-STARTS            TO  CLAS-INDEXS.                 ECS018
00691      EJECT                                                        ECS018
00692  0370-FIND-ST-DESC.                                               ECS018
00693      IF CLAS-INDEXS GREATER CLAS-MAXS                             ECS018
00694          MOVE 'INVALID STATE'    TO  HD-ST-NM HD-SUMM-ST-NM       ECS018
00695              GO TO 0380-INTL-ACCT.                                ECS018
00696                                                                   ECS018
00697      IF STATE-SUB (CLAS-INDEXS) NOT = STATE-L                     ECS018
00698          ADD +1 TO CLAS-INDEXS                                    ECS018
00699              GO TO 0370-FIND-ST-DESC.                             ECS018
00700                                                                   ECS018
00701      MOVE STATE-PIC (CLAS-INDEXS) TO HD-ST-NM HD-SUMM-ST-NM.      ECS018
00702                                                                   ECS018
00703  0380-INTL-ACCT.                                                  ECS018
00704      MOVE W-ACCT                 TO  CUR-ACCT HD-ACCT.            ECS018
00705      MOVE X-ZERO                 TO  X-TOTALS.                    ECS018
00706                                                                   ECS018
00707  0390-INTL-LINE.                                                  ECS018
00708      MOVE W-IG                   TO  CUR-IG WX-LAH.               ECS018
00709      MOVE W-TYPE                 TO  CUR-TYPE WX-TYPE.            ECS018
00710      IF W-IG LESS THAN 3                                          ECS018
00711          MOVE 1                  TO  X1                           ECS018
00712        ELSE                                                       ECS018
00713          MOVE 2                  TO  X1.                          ECS018
00714                                                                   ECS018
00715      IF W-IG = 1 OR 3                                             ECS018
00716          MOVE 1                  TO  WX-LAH                       ECS018
00717        ELSE                                                       ECS018
00718          MOVE 2                  TO  WX-LAH.                      ECS018
00719                                                                   ECS018
00720      MOVE 1                      TO  X2.                          ECS018
00721                                                                   ECS018
00722  0400-LOOP-LINE.                                                  ECS018
00723      IF WX-POINTER = X-POINTER (X2)                               ECS018
00724          MOVE X-AMTS (X1 X2)     TO  X-DETL                       ECS018
00725          GO TO 0410-EXIT.                                         ECS018
00726                                                                   ECS018
00727      ADD 1 TO X2.                                                 ECS018
00728                                                                   ECS018
00729      IF X-TYPE (X2) = HIGH-VALUE                                  ECS018
00730          DISPLAY 'INVALID TYPE - ' WX-POINTER                     ECS018
00731          MOVE '0301'             TO  WS-RETURN-CODE               ECS018
00732          MOVE 'INVALID  TYPE'    TO  WS-ABEND-MESSAGE             ECS018
00733          GO TO ABEND-PGM.                                         ECS018
00734                                                                   ECS018
00735      GO TO 0400-LOOP-LINE.                                        ECS018
00736                                                                   ECS018
00737  0410-EXIT.                                                       ECS018
00738      EXIT.                                                        ECS018
00739  EJECT                                                            ECS018
00740  0430-RD-ACCT-RTN.                                                ECS018
00741      IF HAVE-SEQ = HIGH-VALUE                                     ECS018
00742          MOVE HIGH-VALUE         TO  AM-CONTROL-A                 ECS018
00743          GO TO 0440-EXIT.                                         ECS018
00744                                                                   ECS018
00745      MOVE AM-REC                 TO  ACCOUNT-MASTER.              ECS018
00746      READ ERACCTT.                                                ECS018
00747                                                                   ECS018
00748      IF ERACCT-FILE-STATUS IS = '10'                              ECS018
00749          MOVE HIGH-VALUE         TO  HAVE-SEQ                     ECS018
00750          GO TO 0440-EXIT.                                         ECS018
00751                                                                   ECS018
00752      IF ERACCT-FILE-STATUS  = '00' OR '97'                        ECS018
00753          NEXT SENTENCE                                            ECS018
00754        ELSE                                                       ECS018
00755          MOVE +0301                        TO  WS-RETURN-CODE     ECS018
00756          MOVE 'ERROR READING ACCT MASTER'  TO  WS-ABEND-MESSAGE   ECS018
00757          GO TO ABEND-PGM.                                         ECS018
00758                                                                   ECS018
00759      MOVE AM-SEQ                 TO  HAVE-SEQ.                    ECS018
00760      IF HAVE-SEQ = AM-CONTROL-A                                   ECS018
00761          GO TO 0430-RD-ACCT-RTN.                                  ECS018
00762                                                                   ECS018
00763  0440-EXIT.                                                       ECS018
00764       EXIT.                                                       ECS018
00765  EJECT                                                            ECS018
00766  0460-BLD-RTN.                                                    ECS018
00767      PERFORM 0750-HD-RTN THRU 0760-EXIT.                          ECS018
00768      MOVE 1                      TO  X1.                          ECS018
00769      MOVE 'INDIVIDUAL'           TO  HD-DESC.                     ECS018
00770      MOVE SPACE-3                TO  P-CCSW.                      ECS018
00771      PERFORM 0510-PRT-EXTRACT THRU 0570-EXIT.                     ECS018
00772                                                                   ECS018
00773      MOVE PRT-SW                 TO  IND-SW.                      ECS018
00774      MOVE 2                      TO  X1.                          ECS018
00775      MOVE 'GROUP'                TO  HD-DESC.                     ECS018
00776      MOVE SPACE-3                TO  P-CCSW.                      ECS018
00777      PERFORM 0510-PRT-EXTRACT THRU 0570-EXIT.                     ECS018
00778                                                                   ECS018
00779      MOVE PRT-SW                 TO  GRP-SW.                      ECS018
00780                                                                   ECS018
00781      IF IND-SW = 0 OR GRP-SW = 0                                  ECS018
00782          GO TO 0490-EXIT.                                         ECS018
00783                                                                   ECS018
00784      MOVE 0                      TO  X2.                          ECS018
00785                                                                   ECS018
00786  0470-TOTAL-LOOP.                                                 ECS018
00787      ADD 1 TO X2.                                                 ECS018
00788      IF X-POINTER (X2) = HIGH-VALUE                               ECS018
00789          GO TO 0480-EXIT.                                         ECS018
00790                                                                   ECS018
00791      MOVE X-AMTS (1 X2)          TO  X-DETL.                      ECS018
00792      MOVE X-AMTS (2 X2)          TO  R-DETL.                      ECS018
00793      ADD R-ISSUE                 TO  X-ISSUE.                     ECS018
00794      ADD R-CANCL                 TO  X-CANCL.                     ECS018
00795      ADD R-BASE                  TO  X-BASE.                      ECS018
00796      ADD R-OVER                  TO  X-OVER.                      ECS018
00797      ADD R-CLAIM                 TO  X-CLAIM.                     ECS018
00798      MOVE X-DETL                 TO  X-AMTS (1 X2).               ECS018
00799      GO TO 0470-TOTAL-LOOP.                                       ECS018
00800                                                                   ECS018
00801  0480-EXIT.                                                       ECS018
00802      MOVE 'TOTAL'                TO  HD-DESC.                     ECS018
00803      MOVE 1                      TO  X1.                          ECS018
00804      MOVE SPACE-3                TO  P-CCSW.                      ECS018
00805      PERFORM 0510-PRT-EXTRACT THRU 0570-EXIT.                     ECS018
00806                                                                   ECS018
00807  0490-EXIT.                                                       ECS018
00808      EXIT.                                                        ECS018
00809  EJECT                                                            ECS018
00810  0510-PRT-EXTRACT.                                                ECS018
00811      MOVE 0                      TO  PRT-SW LIFE-SW AH-SW X2.     ECS018
00812      MOVE X-ZERO-5               TO  SUB-TOTALS.                  ECS018
00813                                                                   ECS018
00814  0520-LOOP-LIFE.                                                  ECS018
00815      ADD 1 TO X2.                                                 ECS018
00816      IF X-POINTER (X2) = HIGH-VALUE                               ECS018
00817          GO TO 0530-OUT-LIFE.                                     ECS018
00818                                                                   ECS018
00819      IF X-LAH (X2) NOT = 1                                        ECS018
00820          SUBTRACT 1 FROM X2                                       ECS018
00821          GO TO 0530-OUT-LIFE.                                     ECS018
00822                                                                   ECS018
00823      MOVE X-AMTS (X1 X2)         TO  X-DETL.                      ECS018
00824                                                                   ECS018
00825      IF X-DETL = X-ZERO-5                                         ECS018
00826          GO TO 0520-LOOP-LIFE.                                    ECS018
00827                                                                   ECS018
00828      IF PRT-SW = 0                                                ECS018
00829          MOVE 1                  TO  PRT-SW                       ECS018
00830          MOVE 1                  TO  LIFE-SW                      ECS018
00831          MOVE HD4                TO  P-LN                         ECS018
00832          PERFORM 0770-PRT-RTN THRU 0790-EXIT                      ECS018
00833          MOVE HD5                TO  P-LN                         ECS018
00834          PERFORM 0770-PRT-RTN THRU 0790-EXIT                      ECS018
00835          MOVE SPACE-2            TO  P-CCSW.                      ECS018
00836                                                                   ECS018
00837      MOVE X-POINTER (X2)         TO  WX-POINTER.                  ECS018
00838      MOVE X-DESC (X2)            TO  WX-DESC.                     ECS018
00839                                                                   ECS018
00840      IF WX-OB = '1'                                               ECS018
00841          MOVE '1YR'              TO  WX-DESC-OB.                  ECS018
00842      IF WX-OB = '2'                                               ECS018
00843          MOVE 'REN'              TO  WX-DESC-OB.                  ECS018
00844                                                                   ECS018
00845      ADD X-ISSUE                 TO  S-ISSUE.                     ECS018
00846      ADD X-CANCL                 TO  S-CANCL.                     ECS018
00847      ADD X-BASE                  TO  S-BASE.                      ECS018
00848      ADD X-OVER                  TO  S-OVER.                      ECS018
00849      ADD X-CLAIM                 TO  S-CLAIM.                     ECS018
00850      PERFORM 0580-PRT-LINE THRU 0590-EXIT.                        ECS018
00851      GO TO 0520-LOOP-LIFE.                                        ECS018
00852                                                                   ECS018
00853  0530-OUT-LIFE.                                                   ECS018
00854      MOVE SUB-TOTALS             TO  TOTALS.                      ECS018
00855      IF LIFE-SW = 0                                               ECS018
00856          GO TO 0540-LOOP-AH.                                      ECS018
00857                                                                   ECS018
00858      MOVE 'TOTAL '               TO  BLD-TOTAL.                   ECS018
00859      MOVE LIFE-OVERRIDE-L2       TO  BLD-DESC1.                   ECS018
00860      MOVE SPACES                 TO  BLD-DESC2.                   ECS018
00861      MOVE SPACES                 TO  B-SLASH1.                    ECS018
00862      MOVE BLD-DESC               TO  WX-DESC.                     ECS018
00863      MOVE SPACES                 TO  WX-POINTER.                  ECS018
00864      MOVE SUB-TOTALS             TO  X-DETL.                      ECS018
00865      PERFORM 0580-PRT-LINE THRU 0590-EXIT.                        ECS018
00866                                                                   ECS018
00867      IF (SUB-HD2 = 'FINAL TOTALS')
00868 *       and (HD-DESC   = 'TOTAL ')
073114*            MOVE X-BASE         TO  HLD-018-COMM-L
00870              MOVE X-OVER         TO  HOLD-OVER                    ECS018
00871              DISPLAY 'LF HOLD OVER = ' HOLD-OVER                  ECS018
073114*            MOVE X-OVER         TO  HLD-018-OR-L
           end-if
00873                                                                   ECS018
00874      MOVE SPACE-2                TO  P-CCSW.                      ECS018
00875      MOVE X-ZERO-5               TO  SUB-TOTALS.                  ECS018
00876                                                                   ECS018
00877  0540-LOOP-AH.                                                    ECS018
00878      ADD 1 TO X2.                                                 ECS018
00879      IF X-POINTER (X2) = HIGH-VALUE                               ECS018
00880          GO TO 0550-OUT-AH.                                       ECS018
00881                                                                   ECS018
00882      MOVE X-AMTS (X1 X2)         TO  X-DETL.                      ECS018
00883      IF X-ZERO-5 = X-DETL                                         ECS018
00884          GO TO 0540-LOOP-AH.                                      ECS018
00885                                                                   ECS018
00886      IF PRT-SW = 0                                                ECS018
00887          MOVE 1                  TO  PRT-SW                       ECS018
00888          MOVE 1                  TO  AH-SW                        ECS018
00889          MOVE HD4                TO  P-LN                         ECS018
00890          PERFORM 0770-PRT-RTN THRU 0790-EXIT                      ECS018
00891          MOVE HD5                TO  P-LN                         ECS018
00892          PERFORM 0770-PRT-RTN THRU 0790-EXIT                      ECS018
00893          MOVE SPACE-2            TO  P-CCSW.                      ECS018
00894                                                                   ECS018
00895      IF AH-SW = 0                                                 ECS018
00896          MOVE 1                  TO  AH-SW.                       ECS018
00897                                                                   ECS018
00898      MOVE X-POINTER (X2)         TO  WX-POINTER.                  ECS018
00899      MOVE X-DESC (X2)            TO  WX-DESC.                     ECS018
00900                                                                   ECS018
00901      IF WX-OB = '1'                                               ECS018
00902          MOVE '1YR'              TO  WX-DESC-OB.                  ECS018
00903      IF WX-OB = '2'                                               ECS018
00904          MOVE 'REN'              TO  WX-DESC-OB.                  ECS018
00905                                                                   ECS018
00906      ADD X-ISSUE                 TO  S-ISSUE.                     ECS018
00907      ADD X-CANCL                 TO  S-CANCL.                     ECS018
00908      ADD X-BASE                  TO  S-BASE.                      ECS018
00909      ADD X-OVER                  TO  S-OVER.                      ECS018
00910      ADD X-CLAIM                 TO  S-CLAIM.                     ECS018
00911      PERFORM 0580-PRT-LINE THRU 0590-EXIT.                        ECS018
00912      GO TO 0540-LOOP-AH.                                          ECS018
00913                                                                   ECS018
00914  0550-OUT-AH.                                                     ECS018
00915      IF AH-SW = 0                                                 ECS018
00916          GO TO 0570-EXIT.                                         ECS018
00917                                                                   ECS018
00918      MOVE 'TOTAL '               TO  BLD-TOTAL.                   ECS018
00919      MOVE AH-OVERRIDE-L2         TO  BLD-DESC1.                   ECS018
00920      MOVE SPACES                 TO  BLD-DESC2.                   ECS018
00921      MOVE SPACES                 TO  B-SLASH1.                    ECS018
00922      MOVE BLD-DESC               TO  WX-DESC.                     ECS018
00923      MOVE SPACES                 TO  WX-POINTER.                  ECS018
00924      MOVE SUB-TOTALS             TO  X-DETL.                      ECS018
00925      PERFORM 0580-PRT-LINE THRU 0590-EXIT.                        ECS018
00926                                                                   ECS018
00927      IF (SUB-HD2 = 'FINAL TOTALS')
00928 *       (HD-DESC  = 'TOTAL ')
073114*            MOVE X-BASE         TO  HLD-018-COMM-AH
00930              MOVE X-OVER         TO  HOLD-OVER                    ECS018
00931              DISPLAY 'AH HOLD OVER = ' HOLD-OVER                  ECS018
073114*            MOVE X-OVER         TO  HLD-018-OR-AH
           end-if
00933                                                                   ECS018
00934      MOVE SPACE-2                TO  P-CCSW.                      ECS018
00935      ADD S-ISSUE                 TO  T-ISSUE.                     ECS018
00936      ADD S-CANCL                 TO  T-CANCL.                     ECS018
00937      ADD S-BASE                  TO  T-BASE.                      ECS018
00938      ADD S-OVER                  TO  T-OVER.                      ECS018
00939      ADD S-CLAIM                 TO  T-CLAIM.                     ECS018
00940                                                                   ECS018
00941  0560-PRT-TOTALS.                                                 ECS018
00942      IF LIFE-SW = 0                                               ECS018
00943          GO TO 0570-EXIT.                                         ECS018
00944                                                                   ECS018
00945      MOVE TOTALS                 TO  X-DETL.                      ECS018
00946      MOVE 'TOTAL '               TO  BLD-TOTAL.                   ECS018
00947      MOVE LIFE-OVERRIDE-L2       TO  BLD-DESC1.                   ECS018
00948      MOVE AH-OVERRIDE-L2         TO  BLD-DESC2.                   ECS018
00949      MOVE '/'                    TO  B-SLASH1.                    ECS018
00950      MOVE BLD-DESC               TO  WX-DESC.                     ECS018
00951      MOVE SPACES                 TO  WX-POINTER.                  ECS018
00952      PERFORM 0580-PRT-LINE THRU 0590-EXIT.                        ECS018
00953                                                                   ECS018
00954  0570-EXIT.             EXIT.                                     ECS018
00955      EJECT                                                        ECS018
00956  0580-PRT-LINE.                                                   ECS018
00957      IF LNCTR GREATER THAN +058                                   ECS018
00958          PERFORM 0750-HD-RTN THRU 0760-EXIT                       ECS018
00959          MOVE HD4                TO  P-LN                         ECS018
00960          MOVE SPACE-3            TO  P-CCSW                       ECS018
00961          PERFORM 0770-PRT-RTN THRU 0790-EXIT                      ECS018
00962          MOVE HD5                TO  P-LN                         ECS018
00963          PERFORM 0770-PRT-RTN THRU 0790-EXIT                      ECS018
00964          MOVE SPACE-2            TO  P-CCSW.                      ECS018
00965                                                                   ECS018
00966      MOVE WX-TYP                 TO  P-TYPE.                      ECS018
00967      MOVE WX-DESC                TO  P-DESC.                      ECS018
00968      MOVE X-ISSUE                TO  P-ISSUE.                     ECS018
00969      MOVE X-CANCL                TO  P-CANCL.                     ECS018
00970      MOVE X-BASE                 TO  P-BASE.                      ECS018
00971      MOVE X-OVER                 TO  P-OVER.                      ECS018
00972                                                                   ECS018
00973      MOVE X-CLAIM                TO  P-CLAIM.                     ECS018
00974      ADD X-ISSUE X-CANCL GIVING X-NET.                            ECS018
00975      ADD X-BASE  X-OVER  GIVING X-TOTAL.                          ECS018
00976      MOVE X-NET                  TO  P-NET.                       ECS018
00977      MOVE X-TOTAL                TO  P-TOTAL.                     ECS018
00978      PERFORM 0770-PRT-RTN THRU 0790-EXIT.                         ECS018
00979                                                                   ECS018
00980  0590-EXIT.                                                       ECS018
00981       EXIT.                                                       ECS018
00982      EJECT                                                        ECS018
00983  0600-ZERO-RTN.                                                   ECS018
00984      MOVE +0                     TO  X2 X-ISSUE X-CANCL           ECS018
00985                                      X-BASE X-OVER X-CLAIM.       ECS018
00986      MOVE X-DETL                 TO  X-ZERO-5.                    ECS018
00987                                                                   ECS018
00988  0610-ZERO-LOOP.                                                  ECS018
00989      ADD 1                       TO  X2.                          ECS018
092602     IF X2 GREATER THAN 900                                       ECS018
00991          GO TO 0620-ZERO-FILL.                                    ECS018
00992                                                                   ECS018
00993      MOVE X-ZERO-5               TO  X-AMTS (1 X2)                ECS018
00994                                      X-AMTS (2 X2).               ECS018
00995                                                                   ECS018
00996      GO TO 0610-ZERO-LOOP.                                        ECS018
00997                                                                   ECS018
00998  0620-ZERO-FILL.                                                  ECS018
00999      MOVE X-TOTALS               TO  X-ZERO.                      ECS018
01000                                                                   ECS018
01001  0630-EXIT.                                                       ECS018
01002       EXIT.                                                       ECS018
01003                                                                   ECS018
01004  0640-ROLL-RTN.                                                   ECS018
01005      MOVE 0                      TO  X2.                          ECS018
01006                                                                   ECS018
01007  0650-LOOP-X2.                                                    ECS018
01008      ADD 1                       TO  X2.                          ECS018
01009      IF X-POINTER (X2) = HIGH-VALUE                               ECS018
01010          GO TO 0680-EXIT.                                         ECS018
01011                                                                   ECS018
01012  0660-ROLL-INDIVIDUAL.                                            ECS018
01013      MOVE X-AMTS (1 X2)          TO  X-DETL.                      ECS018
01014      IF X-DETL = X-ZERO-5                                         ECS018
01015          GO TO 0670-ROLL-GROUP.                                   ECS018
01016                                                                   ECS018
01017      MOVE R-AMTS (1 X2)          TO  R-DETL.                      ECS018
01018      ADD X-ISSUE                 TO  R-ISSUE.                     ECS018
01019      ADD X-CANCL                 TO  R-CANCL.                     ECS018
01020      ADD X-BASE                  TO  R-BASE.                      ECS018
01021      ADD X-OVER                  TO  R-OVER.                      ECS018
01022      ADD X-CLAIM                 TO  R-CLAIM.                     ECS018
01023      MOVE R-DETL                 TO  R-AMTS (1 X2).               ECS018
01024                                                                   ECS018
01025  0670-ROLL-GROUP.                                                 ECS018
01026      MOVE X-AMTS (2 X2)          TO  X-DETL.                      ECS018
01027      IF X-DETL = X-ZERO-5                                         ECS018
01028          GO TO 0650-LOOP-X2.                                      ECS018
01029                                                                   ECS018
01030      MOVE R-AMTS (2 X2)          TO  R-DETL.                      ECS018
01031      ADD X-ISSUE                 TO  R-ISSUE.                     ECS018
01032      ADD X-CANCL                 TO  R-CANCL.                     ECS018
01033      ADD X-BASE                  TO  R-BASE.                      ECS018
01034      ADD X-OVER                  TO  R-OVER.                      ECS018
01035      ADD X-CLAIM                 TO  R-CLAIM.                     ECS018
01036      MOVE R-DETL                 TO  R-AMTS (2 X2).               ECS018
01037      GO TO 0650-LOOP-X2.                                          ECS018
01038                                                                   ECS018
01039  0680-EXIT.                                                       ECS018
01040       EXIT.                                                       ECS018
01041      EJECT                                                        ECS018
01042  0690-FORMAT-RTN.                                                 ECS018
01043      MOVE 0                      TO  X2.                          ECS018
01044      MOVE CLAS-STARTL            TO  CLAS-INDEXL.                 ECS018
01045      MOVE CLAS-STARTA            TO  CLAS-INDEXA.                 ECS018
01046      MOVE HIGH-VALUE             TO  X-POINTERS.                  ECS018
01047      MOVE SPACES                 TO  X-DESCRIPTIONS.              ECS018
01048      IF CLAS-MAXL = ZEROES                                        ECS018
01049          GO TO 0710-FORMAT-AH-RTN.                                ECS018
01050                                                                   ECS018
01051  0700-FORMAT-LIFE.                                                ECS018
01052      IF CLAS-INDEXL GREATER THAN CLAS-MAXL                        ECS018
01053          GO TO 0710-FORMAT-AH-RTN.                                ECS018
01054                                                                   ECS018
01055      ADD 1                       TO  X2.                          ECS018
01056      MOVE CLAS-I-BEN (CLAS-INDEXL)   TO  X-TYP (X2).              ECS018
01057      MOVE 1                      TO  X-LAH (X2).                  ECS018
01058      MOVE SPACES                 TO  X-OB (X2).                   ECS018
01059      MOVE CLAS-I-AB10 (CLAS-INDEXL)  TO  X-DESC (X2).             ECS018
01060      IF CLAS-I-BAL (CLAS-INDEXL) NOT = 'B'                        ECS018
01061          ADD 1                   TO  CLAS-INDEXL                  ECS018
01062          GO TO 0700-FORMAT-LIFE.                                  ECS018
01063                                                                   ECS018
01064      IF DTE-CLIENT NOT = 'DMD'                                    ECS018
01065          MOVE '1'                TO  X-OB (X2).                   ECS018
01066                                                                   ECS018
01067      MOVE X2                     TO  SAVE-X2.                     ECS018
01068      ADD 1                       TO  X2.                          ECS018
01069      MOVE X-POINTER (SAVE-X2)    TO  X-POINTER (X2).              ECS018
01070      MOVE X-DESC (SAVE-X2)       TO  X-DESC (X2).                 ECS018
01071                                                                   ECS018
01072      IF DTE-CLIENT NOT = 'DMD'                                    ECS018
01073          MOVE '2'                TO  X-OB (X2).                   ECS018
01074                                                                   ECS018
01075      ADD 1                       TO  CLAS-INDEXL.                 ECS018
01076      GO TO 0700-FORMAT-LIFE.                                      ECS018
01077                                                                   ECS018
01078  0710-FORMAT-AH-RTN.                                              ECS018
01079      IF CLAS-MAXA = ZEROS                                         ECS018
01080          GO TO 0730-FORMAT-SET.                                   ECS018
01081                                                                   ECS018
01082  0720-FORMAT-AH.                                                  ECS018
01083      IF CLAS-INDEXA GREATER THAN CLAS-MAXA                        ECS018
01084          GO TO 0730-FORMAT-SET.                                   ECS018
01085                                                                   ECS018
01086      ADD 1                       TO  X2.                          ECS018
01087      MOVE CLAS-I-BEN (CLAS-INDEXA)   TO  X-TYP (X2).              ECS018
01088      MOVE 2                      TO  X-LAH (X2).                  ECS018
01089      MOVE SPACES                 TO  X-OB (X2).                   ECS018
01090      MOVE CLAS-I-AB10 (CLAS-INDEXA)  TO  X-DESC (X2).             ECS018
01091      IF CLAS-I-BAL (CLAS-INDEXA) NOT = 'B'                        ECS018
01092          ADD 1                   TO  CLAS-INDEXA                  ECS018
01093          GO TO 0720-FORMAT-AH.                                    ECS018
01094                                                                   ECS018
01095      IF DTE-CLIENT NOT = 'DMD'                                    ECS018
01096          MOVE '1'                TO  X-OB (X2).                   ECS018
01097                                                                   ECS018
01098      MOVE X2                     TO  SAVE-X2.                     ECS018
01099      ADD 1                       TO  X2.                          ECS018
01100      MOVE X-POINTER (SAVE-X2)    TO  X-POINTER (X2).              ECS018
01101                                                                   ECS018
01102      IF DTE-CLIENT NOT = 'DMD'                                    ECS018
01103          MOVE '2'                TO  X-OB (X2).                   ECS018
01104                                                                   ECS018
01105      MOVE X-DESC (SAVE-X2)       TO  X-DESC (X2).                 ECS018
01106      ADD 1                       TO  CLAS-INDEXA.                 ECS018
01107      GO TO 0720-FORMAT-AH.                                        ECS018
01108                                                                   ECS018
01109  0730-FORMAT-SET.                                                 ECS018
01110                                                                   ECS018
092602     IF X2 GREATER THAN 900                                       ECS018
01112          MOVE +0201                      TO  WS-RETURN-CODE       ECS018
01113          MOVE 'PROGRAM TABLE EXCEEDED'   TO  WS-ABEND-MESSAGE     ECS018
01114          GO TO ABEND-PGM.                                         ECS018
01115                                                                   ECS018
01116  0740-EXIT.                                                       ECS018
01117       EXIT.                                                       ECS018
01118      EJECT                                                        ECS018
01119  0750-HD-RTN.                                                     ECS018
01120      ADD +1                      TO  PGCTR.                       ECS018
01121      MOVE PGCTR                  TO  HD-PAGE.                     ECS018
01122      MOVE HD1                    TO  P-LN.                        ECS018
01123      MOVE SPACE-N                TO  P-CCSW.                      ECS018
01124      PERFORM 0770-PRT-RTN THRU 0790-EXIT.                         ECS018
01125      MOVE HD2                    TO  P-LN.                        ECS018
01126      PERFORM 0770-PRT-RTN THRU 0790-EXIT.                         ECS018
01127      MOVE HD3                    TO  P-LN.                        ECS018
01128      PERFORM 0770-PRT-RTN THRU 0790-EXIT.                         ECS018
01129      MOVE SAVE-NAME              TO  P-LN.                        ECS018
01130      PERFORM 0770-PRT-RTN THRU 0790-EXIT.                         ECS018
01131      MOVE +5                     TO  LNCTR.                       ECS018
01132                                                                   ECS018
01133  0760-EXIT.                                                       ECS018
01134       EXIT.                                                       ECS018
01135                                                                   ECS018
01136  0770-PRT-RTN.                                                    ECS018
01137      MOVE P-CCSW                 TO  X P-CTL.                     ECS018
01138      MOVE P-LN                   TO  P-DATA.                      ECS018
01139      MOVE SPACE-1                TO  P-REC.                       ECS018
01140                                                                   ECS018
01141      IF X = SPACE-1                                               ECS018
01142          ADD 1                   TO  LNCTR                        ECS018
01143      ELSE                                                         ECS018
01144          IF X = SPACE-2                                           ECS018
01145              ADD 2               TO  LNCTR                        ECS018
01146          ELSE                                                     ECS018
01147              IF X = SPACE-3                                       ECS018
01148                  ADD 3           TO  LNCTR.                       ECS018
01149                                                                   ECS018
01150  0780-PRT-COPY.                                                   ECS018
01151                                  COPY ELCPRT2.                    ECS018
01152  0790-EXIT.                                                       ECS018
01153       EXIT.                                                       ECS018
01154      EJECT                                                        ECS018
01155  0800-END-OUTPUT.                                                 ECS018
01156      IF FST-SW = +0                                               ECS018
01157          PERFORM 0750-HD-RTN THRU 0760-EXIT                       ECS018
01158          MOVE 'NO RECALCULATED TRANSACTIONS THIS MONTH' TO P-LN   ECS018
01159          MOVE SPACE-2            TO  P-CCSW                       ECS018
01160          PERFORM 0770-PRT-RTN THRU 0790-EXIT                      ECS018
01161      ELSE                                                         ECS018
01162          MOVE LOW-VALUE          TO  W-SEQ                        ECS018
01163          MOVE 2                  TO  FST-SW                       ECS018
01164          PERFORM 0230-BREAK-RTN THRU 0410-EXIT.                   ECS018
01165                                                                   ECS018
01166      CLOSE ERACCTT.                                               ECS018
01167                                                                   ECS018
01168  0810-EXIT.                                                       ECS018
01169       EXIT.                                                       ECS018
01170                                                                   ECS018
01171      EJECT                                                        ECS018
01172  0820-END-OF-JOB SECTION.                                         ECS018
01173                                                                   ECS018
01174  0830-EOJ-RTN.                                                    ECS018
01175          MOVE SPACE-3            TO  P-CCSW.                      ECS018
01176          MOVE AMT-Y              TO  P-AMT-Y.                     ECS018
01177          MOVE P-DET-Y            TO  P-LN.                        ECS018
01178          PERFORM 0770-PRT-RTN THRU 0790-EXIT                      ECS018
01179          MOVE SPACE-2            TO  P-CCSW.                      ECS018
01180          MOVE AMT-1              TO  P-AMT-1.                     ECS018
01181          MOVE P-DET-1            TO  P-LN.                        ECS018
01182          PERFORM 0770-PRT-RTN THRU 0790-EXIT                      ECS018

070714     MOVE SPACE-2                TO P-CCSW
070714     MOVE AMT-Y-OW               TO P-AMT-Y-OW
070714     MOVE P-OW-Y                 TO P-LN
070714     PERFORM 0770-PRT-RTN THRU   0790-EXIT
070714     MOVE SPACE-2                TO P-CCSW
070714     MOVE AMT-1-OW               TO P-AMT-1-OW
070714     MOVE P-OW-1                 TO P-LN
070714     PERFORM 0770-PRT-RTN THRU   0790-EXIT

01184      COPY ELCPRTC.                                                ECS018
01185                                                                   ECS018
01186      CLOSE PRNTR.                                                 ECS018
01187                                                                   ECS018
070714     OPEN I-O ERMEBL.                                             ECS018
070714                                                                  ECS018
070714     IF ERMEBL-FILE-STATUS  = '00' OR '97'                        ECS018
070714         NEXT SENTENCE                                            ECS018
070714       ELSE                                                       ECS018
070714         MOVE 'N'                TO  ME-UPDATE-FLAG.              ECS018
070714     MOVE DTE-CLIENT             TO  ME-COMPANY.                  ECS018
070714                                                                  ECS018
070714     COMPUTE MONTH-END-MOYR = RUN-CCYY * 12 + RUN-MO.             ECS018
070714     MOVE MONTH-END-MOYR         TO  ME-MOYR.                     ECS018
070714     IF ME-DO-UPDATE                                              ECS018
070714         READ ERMEBL INVALID KEY                                  ECS018
070714             MOVE 'N'            TO  ME-UPDATE-FLAG               ECS018
070714             CLOSE ERMEBL.                                        ECS018
070714                                                                  ECS018
070714     IF ME-DO-UPDATE                                              ECS018
070714        compute me-018-comm-y    = amt-y
070714        compute me-018-comm-1    = amt-1
070714        compute me-018-ow-y      = amt-y-ow
070714        compute me-018-ow-1      = amt-1-ow
070714         display ' base ow Y ' me-018-comm-y ' ' me-018-ow-y
070714         display ' base ow 1 ' me-018-comm-1 ' ' me-018-ow-1
070714         MOVE ME-CNDS-DATE       TO  ME-018-RUN-DT                ECS018
070714         ACCEPT WS-TIME-OF-DAY   FROM TIME                        ECS018
070714         ADD 1                   TO  ME-018-RUN-CT                ECS018
070714         REWRITE MONTH-END-BALANCES                               ECS018
070714         display ' me rewrite ' ermebl-file-status
070714         CLOSE ERMEBL.                                            ECS018
01196                                                                   ECS018
01197      MOVE ZEROS  TO RETURN-CODE.                                  ECS018
01198      GOBACK.                                                      ECS018
01199                                                                   ECS018
01200  ABEND-PGM SECTION.                                               ECS018
01201                                  COPY ELCABEND SUPPRESS.          ECS018
01202 /                                                                 ECS018
01203  LCP-WRITE-POS-PRT SECTION.                                       ECS018
01204      IF LCP-ASA = '+'                                             ECS018
01205          WRITE PRT AFTER 0 LINE                                   ECS018
01206      ELSE                                                         ECS018
01207      IF LCP-ASA = ' '                                             ECS018
01208          WRITE PRT AFTER ADVANCING 1 LINE                         ECS018
01209      ELSE                                                         ECS018
01210      IF LCP-ASA = '0'                                             ECS018
01211          WRITE PRT AFTER ADVANCING 2 LINE                         ECS018
01212      ELSE                                                         ECS018
01213      IF LCP-ASA = '-'                                             ECS018
01214          WRITE PRT AFTER ADVANCING 3 LINE                         ECS018
01215      ELSE                                                         ECS018
01216      IF LCP-ASA = '1'                                             ECS018
01217          WRITE PRT AFTER ADVANCING PAGE                           ECS018
01218      ELSE                                                         ECS018
01219      IF LCP-ASA = '2'                                             ECS018
01220          WRITE PRT AFTER ADVANCING LCP-CH2                        ECS018
01221      ELSE                                                         ECS018
01222      IF LCP-ASA = '3'                                             ECS018
01223          WRITE PRT AFTER ADVANCING LCP-CH3                        ECS018
01224      ELSE                                                         ECS018
01225      IF LCP-ASA = '4'                                             ECS018
01226          WRITE PRT AFTER ADVANCING LCP-CH4                        ECS018
01227      ELSE                                                         ECS018
01228      IF LCP-ASA = '5'                                             ECS018
01229          WRITE PRT AFTER ADVANCING LCP-CH5                        ECS018
01230      ELSE                                                         ECS018
01231      IF LCP-ASA = '6'                                             ECS018
01232          WRITE PRT AFTER ADVANCING LCP-CH6                        ECS018
01233      ELSE                                                         ECS018
01234      IF LCP-ASA = '7'                                             ECS018
01235          WRITE PRT AFTER ADVANCING LCP-CH7                        ECS018
01236      ELSE                                                         ECS018
01237      IF LCP-ASA = '8'                                             ECS018
01238          WRITE PRT AFTER ADVANCING LCP-CH8                        ECS018
01239      ELSE                                                         ECS018
01240      IF LCP-ASA = '9'                                             ECS018
01241          WRITE PRT AFTER ADVANCING LCP-CH9                        ECS018
01242      ELSE                                                         ECS018
01243      IF LCP-ASA = 'A'                                             ECS018
01244          WRITE PRT AFTER ADVANCING LCP-CH10                       ECS018
01245      ELSE                                                         ECS018
01246      IF LCP-ASA = 'B'                                             ECS018
01247          WRITE PRT AFTER ADVANCING LCP-CH11                       ECS018
01248      ELSE                                                         ECS018
01249      IF LCP-ASA = 'C'                                             ECS018
01250          WRITE PRT AFTER ADVANCING LCP-CH12                       ECS018
01251      ELSE                                                         ECS018
01252      IF LCP-ASA = 'V'                                             ECS018
01253          WRITE PRT AFTER ADVANCING LCP-P01                        ECS018
01254      ELSE                                                         ECS018
01255      IF LCP-ASA = 'W'                                             ECS018
01256          WRITE PRT AFTER ADVANCING LCP-P02                        ECS018
01257      ELSE                                                         ECS018
01258      DISPLAY 'ASA CODE ERROR'.                                    ECS018
01259  LCP-WRITE-END-PRT.                                               ECS018
01260      EXIT.                                                        ECS018
