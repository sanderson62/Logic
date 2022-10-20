00001  IDENTIFICATION DIVISION.                                         03/19/98
00002                                                                   ECS019
00003  PROGRAM-ID.                 ECS019.                                 LV004
00004 *              PROGRAM CONVERTED BY                               ECS019
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ECS019
00006 *              CONVERSION DATE 11/28/95 10:59:12.                 ECS019
00007 *           PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE             ECS019
00008 *                            VMOD=2.014.                          ECS019
00009                                                                   ECS019
00010 *AUTHOR.     LOGIC, INC.                                          ECS019
00011 *            DALLAS, TEXAS.                                       ECS019
00012                                                                   ECS019
00013 *DATE-COMPILED.                                                   ECS019
00014                                                                   ECS019
00015 *SECURITY.   *****************************************************ECS019
00016 *            *                                                   *ECS019
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ECS019
00018 *            *                                                   *ECS019
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS019
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ECS019
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *ECS019
00022 *            *                                                   *ECS019
00023 *            *****************************************************ECS019
00024                                                                   ECS019
00025 *REMARKS.                                                         ECS019
00026 *          PRINT PREMIUM AND COMMISSION ANALYSIS.                 ECS019
102501******************************************************************
102501*                   C H A N G E   L O G
102501*
102501* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
102501*-----------------------------------------------------------------
102501*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
102501* EFFECTIVE    NUMBER
102501*-----------------------------------------------------------------
102501* 102501    2001100100006  SMVA  REPLACE LOW-VALUES WITH SPACES
092602* 092602    2002091900008  PEMA  INCREASE NUMBER OF MAXIMUM
092602*                                  BENEFIT CODES FROM 450 TO 900
040504* 040504  CR2003080800002  PEMA  ADD DEALER INCENTIVE PROCESSING
013107* 013107    2006122700001  PEMA  ADD CARRIER SUMMARY TO HARDCOPY
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
070714* 070714  CR2013060600001  PEMA  AUTOMATE MONTH END BALANCING
092602******************************************************************
00027                                                                   ECS019
00028  ENVIRONMENT DIVISION.                                            ECS019
00029  CONFIGURATION SECTION.                                           ECS019
00030  SPECIAL-NAMES.                                                   ECS019
00031      C02 IS LCP-CH2                                               ECS019
00032      C03 IS LCP-CH3                                               ECS019
00033      C04 IS LCP-CH4                                               ECS019
00034      C05 IS LCP-CH5                                               ECS019
00035      C06 IS LCP-CH6                                               ECS019
00036      C07 IS LCP-CH7                                               ECS019
00037      C08 IS LCP-CH8                                               ECS019
00038      C09 IS LCP-CH9                                               ECS019
00039      C10 IS LCP-CH10                                              ECS019
00040      C11 IS LCP-CH11                                              ECS019
00041      C12 IS LCP-CH12                                              ECS019
00042      S01 IS LCP-P01                                               ECS019
00043      S02 IS LCP-P02.                                              ECS019
00044  INPUT-OUTPUT SECTION.                                            ECS019
00045  FILE-CONTROL.                                                    ECS019
00046      SELECT HISTORY-IN       ASSIGN TO
      *       '/data/test/seqfiles/dummy_file'.
              SYS010.
00047      SELECT HISTORY-OUT      ASSIGN TO
      *       '/data/test/seqfiles/ZI.XX.EXTR019'.
              SYS011.
       
00048      SELECT EXTRACT          ASSIGN TO
      *       '/data/test/seqfiles/DC.WW.PRCMEXTR'.
              SYS016.
00049      SELECT DISK-DATE        ASSIGN TO
      *       '/data/test/seqfiles/DC.DD.ER.DATECARD'.
              SYS019.
00050      SELECT PRNTR            ASSIGN TO
      *       '/data/test/seqfiles/ZI.ECS019.SYS008'.
              SYS008.
00051      SELECT FICH             ASSIGN TO
      *       '/data/test/seqfiles/DC.EX.FICH019'.
              SYS020.
00052      SELECT SORTFL           ASSIGN TO
      *       '/data/test/seqfiles/ZI.SORT'.
              SORTWK1.
00053                                                                   ECS019
00054      SELECT ERMEBL           ASSIGN TO
      *       '/data/test/seqfiles/ERMEBL.DAT'
                  ERMEBL
00056              ORGANIZATION INDEXED                                 ECS019
00057              ACCESS DYNAMIC                                       ECS019
00058              RECORD KEY ME-CONTROL-PRIMARY                        ECS019
00059              FILE STATUS ERMEBL-FILE-STATUS.                      ECS019
00060                                                                   ECS019
00061      SELECT ERACCTT          ASSIGN TO
      *       '/data/test/seqfiles/ERACCTT.DAT'
                  ERACCTT
00063              ORGANIZATION INDEXED                                 ECS019
00064              ACCESS IS DYNAMIC                                    ECS019
00065              RECORD KEY AM-CONTROL-PRIMARY                        ECS019
00066              FILE STATUS ERACCT-FILE-STATUS.                      ECS019
00067                                                                   ECS019
00068  EJECT                                                            ECS019
00069  DATA DIVISION.                                                   ECS019
00070  FILE SECTION.                                                    ECS019
00071                                                                   ECS019
00072  FD  HISTORY-IN                                                   ECS019
00073      BLOCK CONTAINS 0 RECORDS
00074      RECORDING MODE IS F.                                         ECS019
00075                                                                   ECS019
011410 01  HIST-IN-RECORD                  PIC X(165).
00077                                                                   ECS019
00078  FD  HISTORY-OUT                                                  ECS019
00079      BLOCK CONTAINS 0 RECORDS
00080      RECORDING MODE IS F.                                         ECS019
00081                                                                   ECS019
011410 01  HISTORY-RECORD                  PIC X(165).
00083                                                                      CL**3
00084      EJECT                                                        ECS019
00085  FD  EXTRACT                                                      ECS019
00086      BLOCK CONTAINS 0 RECORDS
00087      RECORDING MODE IS F.                                         ECS019
00088                                                                   ECS019
011410 01  EXTR-RECORD                     PIC X(165).
00090                                                                      CL**3
00091      EJECT                                                        ECS019
00092  FD  ERACCTT.                                                     ECS019
00093                                                                   ECS019
00094                                      COPY ERCACCT.                ECS019
00095      EJECT                                                        ECS019
00096  FD  DISK-DATE                                                    ECS019
00097                                      COPY ELCDTEFD.               ECS019
00098      EJECT                                                        ECS019
00099  FD  PRNTR                                                        ECS019
00100                                      COPY ELCPRTFD.               ECS019
00101                                                                   ECS019
00102  FD  FICH                                                         ECS019
00103                                      COPY ELCFCHFD.               ECS019
00104      EJECT                                                        ECS019
00105  SD  SORTFL.                                                      ECS019
00106                                                                   ECS019
00107  01  SRT-REC.                                                     ECS019
00108      12  SRT-REPORT-CD-1             PIC X(10).                   ECS019
00109      12  SRT-WORK-REC.                                            ECS019
00110          16  S-PARM                  PIC X(20).                   ECS019
040504         16  FILLER                  PIC X(39).
011410         16  FILLER                  PIC X(106).
00113                                                                   ECS019
00114  FD  ERMEBL.                                                      ECS019
00115                                                                   ECS019
00116                                      COPY ERCMEBL.                ECS019
00117  EJECT                                                            ECS019
00118  WORKING-STORAGE SECTION.                                         ECS019
00119  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      ECS019
00120  77  LCP-ASA                       PIC X.                         ECS019
00121  77  FILLER  PIC X(32) VALUE '********************************'.  ECS019
00122  77  FILLER  PIC X(32) VALUE '     ECS019 WORKING STORAGE     '.  ECS019
00123  77  FILLER  PIC X(32) VALUE '***********VMOD=2.014***********'.  ECS019
00124                                                                   ECS019
00125  77  X                               PIC X       VALUE SPACE.     ECS019
00126  77  SPACE-N                         PIC X       VALUE '1'.       ECS019
00127  77  SPACE-1                         PIC X       VALUE ' '.       ECS019
00128  77  SPACE-2                         PIC X       VALUE '0'.       ECS019
00129  77  SPACE-3                         PIC X       VALUE '-'.       ECS019
00130  77  X-NET                           PIC S9(9)V99          COMP-3.ECS019
00131  77  X-TOTAL                         PIC S9(9)V99          COMP-3.ECS019
00132  77  X1                              PIC S999              COMP-3.ECS019
00133  77  X2                              PIC S999              COMP-3.ECS019
00134  77  R1                              PIC S999              COMP-3.ECS019
00135  77  R2                              PIC S999              COMP-3.ECS019
00136  77  RECD-CTR                        PIC S9(4) VALUE +0.          ECS019
00137  77  AH-SW                           PIC S9                COMP-3.ECS019
00138  77  LIFE-SW                         PIC S9                COMP-3.ECS019
00139  77  IND-SW                          PIC S9                COMP-3.ECS019
00140  77  GRP-SW                          PIC S9                COMP-3.ECS019
00141  77  PRT-SW                          PIC S9                COMP-3.ECS019
00142  77  FST-SW                          PIC S9                COMP-3.ECS019
00143  77  PGCTR                           PIC S9(5)             COMP-3.ECS019
00144  77  LNCTR                           PIC S999              COMP-3.ECS019
00145  77  SAVE-X2                         PIC S999              COMP-3.ECS019
00146  77  HAVE-SEQ                        PIC X(20).                   ECS019
00147  77  NEED-SEQ                        PIC X(19).                   ECS019
00148  77  SAVE-NAME                       PIC X(30)   VALUE SPACES.    ECS019
00149  77  SAVE-REPORT-CD-1                PIC X(10)   VALUE SPACES.    ECS019
00150  77  SAVE-GROUP                      PIC X(06).                   ECS019
00151                                                                   ECS019
00152  01  MONTH-END-DATA.                                              ECS019
00153      12  ME-START-DATE.                                           ECS019
00154          16  ME-START-MO             PIC 99.                      ECS019
00155          16  FILLER                  PIC X.                       ECS019
00156          16  ME-START-DA             PIC 99.                      ECS019
00157          16  FILLER                  PIC X.                       ECS019
00158          16  ME-START-YR             PIC 99.                      ECS019
00159      12  ME-CNDS-DATE                PIC 9(6).                    ECS019
00160      12  ME-CNDS-DATE-R REDEFINES ME-CNDS-DATE.                   ECS019
00161          16  ME-CNDS-MO              PIC 99.                      ECS019
00162          16  ME-CNDS-DA              PIC 99.                      ECS019
00163          16  ME-CNDS-YR              PIC 99.                      ECS019
00164      12  ME-START-TIME               PIC 9(6).                    ECS019
00165      12  ME-UPDATE-FLAG              PIC X       VALUE 'Y'.       ECS019
00166          88  ME-DO-UPDATE                        VALUE 'Y'.       ECS019
00167          88  ME-NO-UPDATE                        VALUE 'N'.       ECS019
00168      12  ERMEBL-FILE-STATUS          PIC XX.                      ECS019
00169      12  ERACCT-FILE-STATUS          PIC XX.                      ECS019
00170      12  MONTH-END-MOYR              PIC S9(5)             COMP-3.ECS019
00171      12  HLD-019-PREM-L              PIC S9(9)V99 VALUE +0 COMP-3.ECS019
00172      12  HLD-019-PREM-AH             PIC S9(9)V99 VALUE +0 COMP-3.ECS019
00173      12  HLD-019-REF-L               PIC S9(9)V99 VALUE +0 COMP-3.ECS019
00174      12  HLD-019-REF-AH              PIC S9(9)V99 VALUE +0 COMP-3.ECS019
00175      12  HLD-019-COMM-L              PIC S9(9)V99 VALUE +0 COMP-3.ECS019
00176      12  HLD-019-COMM-AH             PIC S9(9)V99 VALUE +0 COMP-3.ECS019
00177      12  HLD-019-OR-L                PIC S9(9)V99 VALUE +0 COMP-3.ECS019
00178      12  HLD-019-OR-AH               PIC S9(9)V99 VALUE +0 COMP-3.ECS019
070714     12  HLD-019-CLMS-L              PIC S9(9)V99 VALUE +0 COMP-3.ECS019
070714     12  HLD-019-CLMS-AH             PIC S9(9)V99 VALUE +0 COMP-3.ECS019
00179                                                                   ECS019
00180  01  WS.                                                          ECS019
00181      12  REPORT-OPTIONS              PIC X        VALUE '1'.      ECS019
00182          88  CURRENT-MONTH                        VALUE '1'.      ECS019
00183          88  QTR-TO-DATE                          VALUE '2'.      ECS019
00184          88  YEAR-TO-DATE                         VALUE '3'.      ECS019
00185          88  LAST-TWELVE                          VALUE '4'.      ECS019
00186      12  BEGIN-DATE.                                                 CL**4
00187          16  BEGIN-CCYY              PIC 9(04)   VALUE 0.         ECS019
00188          16  BEGIN-CCYR REDEFINES BEGIN-CCYY.                     ECS019
00189              20  BEGIN-CC            PIC 99.                      ECS019
00190              20  BEGIN-YR            PIC 99.                      ECS019
00191          16  BEGIN-MO                PIC 99      VALUE 0.         ECS019
00192              88  1ST-QTR                         VALUES 01 02 03. ECS019
00193              88  2ND-QTR                         VALUES 04 05 06. ECS019
00194              88  3RD-QTR                         VALUES 07 08 09. ECS019
00195              88  4TH-QTR                         VALUES 10 11 12. ECS019
00196                                                                      CL**3
00197      12  WS-PROCESS-DATE.                                            CL**3
00198          16  FILLER                  PIC 9.                          CL**3
00199          16  WS-PROCESS-CCYYMM.                                      CL**3
00200              20  W-CC                PIC 99.                         CL**3
00201              20  W-YR                PIC 99.                         CL**3
00202              20  W-MO                PIC 99.                         CL**3
00203      12  WS-PROCESS-NUMERIC  REDEFINES                               CL**3
00204          WS-PROCESS-DATE             PIC 9(7).                       CL**3
00205                                                                      CL**3
00206      12  WS-RETURN-CODE              PIC S9(4)   VALUE ZEROS COMP.ECS019
00207      12  WS-ABEND-MESSAGE            PIC X(80).                   ECS019
00208      12  WS-ABEND-FILE-STATUS        PIC XX      VALUE ZEROS.     ECS019
00209      12  WS-ZERO                     PIC S9      VALUE +0  COMP-3.ECS019
00210      12  PGM-SUB                     PIC S999    VALUE +019  COMP.ECS019
00211      EJECT                                                        ECS019
00212                                      COPY ELCDTECX.               ECS019
00213      EJECT                                                        ECS019
00214                                      COPY ELCDTEVR.               ECS019
00215      EJECT                                                        ECS019
00216  01  HD1.                                                         ECS019
00217      12  HD1-RPT-CD-1-HDG        PIC X(10)   VALUE SPACES.        ECS019
00218      12  HD1-FILL                PIC XXX     VALUE ' : '.         ECS019
00219      12  HD1-RPT-CD-1            PIC X(10)   VALUE SPACES.        ECS019
00220      12  FILLER                  PIC X(12)   VALUE SPACES.        ECS019
00221      12  FILLER      PIC X(24)   VALUE 'PREMIUM AND COMMISSION D'.ECS019
00222      12  FILLER                  PIC X(11)   VALUE 'ISTRIBUTION'. ECS019
00223      12  HD1-OPTION              PIC X(18)                        ECS019
00224                         VALUE ' -  CURRENT MONTH '.               ECS019
00225      12  HD1-OPTION-2            PIC X(31)   VALUE SPACES.        ECS019
00226      12  FILLER                  PIC X(8)    VALUE 'ECS019  '.    ECS019
00227                                                                   ECS019
00228  01  HD2.                                                         ECS019
00229      12  SUB-HD2                 PIC X(47)   VALUE SPACES.        ECS019
00230      12  HD-CO                   PIC X(30).                       ECS019
00231      12  FILLER                  PIC X(42)   VALUE SPACES.        ECS019
00232      12  HD-RD                   PIC X(8).                        ECS019
00233                                                                   ECS019
00234  01  HD3.                                                         ECS019
00235      12  SUB-HD3                 PIC X(53)   VALUE SPACES.        ECS019
00236      12  HD-DT                   PIC X(18).                       ECS019
00237      12  FILLER                  PIC X(48)   VALUE SPACES.        ECS019
00238      12  FILLER                  PIC X(5)    VALUE 'PAGE '.       ECS019
00239      12  HD-PAGE                 PIC ZZ,ZZ9.                      ECS019
00240                                                                   ECS019
00241  01  HD4.                                                         ECS019
00242      12 HD-DESC                  PIC X(24).                       ECS019
00243      12 FILLER                   PIC X(45)   VALUE                ECS019
00244      '     * * * * * *  P R E M I U M  * * * * * * '.             ECS019
00245      12 FILLER                   PIC X(45)   VALUE                ECS019
00246      '     * * * *  C O M M I S S I O N  * * * * * '.             ECS019
00247      12 FILLER           PIC X(15) VALUE '        CLAIMS '.
00248                                                                   ECS019
00249  01  HD5.                                                         ECS019
040504     12 FILLER                   PIC X(27)   VALUE SPACES.        ECS019
00251      12 FILLER                   PIC X(45)   VALUE                ECS019
00252      '       WRITTEN      CANCELLED        NET     '.             ECS019
00253      12 FILLER                   PIC X(45)   VALUE                ECS019
00254 *    '     BASE         O/W       LMBA FEE    INCENTIVE'.         ECS019
00254      '          BASE            O/W       MISC FEES'.
00255      12 FILLER                   PIC X(15)   VALUE SPACES.        ECS019
00256                                                                   ECS019
00257  01  SUB-HEADINGS.                                                ECS019
00258      03 HEAD2.                                                    ECS019
00259         05 FILLER                PIC X(40)   VALUE                ECS019
00260      'CARR  GROUP  STATE                  ACCT'.                  ECS019
00261      03 ACCT-HDA REDEFINES HEAD2.                                 ECS019
00262         05 ACCT-HD2              PIC X(40).                       ECS019
00263      03 ST-HDA REDEFINES HEAD2.                                   ECS019
00264         05 ST-HD2                PIC X(28).                       ECS019
00265         05 FILLER                PIC X(12).                       ECS019
00266      03 CO-HDA REDEFINES HEAD2.                                   ECS019
00267         05 CO-HD2                PIC X(11).                       ECS019
00268         05 FILLER                PIC X(29).                       ECS019
00269      03 CARR-HDA REDEFINES HEAD2.                                 ECS019
00270         05 CARR-HD2              PIC X(4).                        ECS019
00271         05 FILLER                PIC X(36).                       ECS019
00272      03 RPT-CD-1-HD2.                                             ECS019
00273         05 FILLER                PIC X(40)   VALUE                ECS019
00274      'REPORT CODE 1   '.                                          ECS019
00275                                                                   ECS019
00276      03 HEAD3.                                                    ECS019
00277         05 FILLER                PIC XX      VALUE SPACES.        ECS019
00278         05 HD-CARR               PIC X.                           ECS019
00279         05 FILLER                PIC XXX     VALUE SPACES.        ECS019
00280         05 HD-GROUP              PIC X(6).                        ECS019
00281         05 FILLER                PIC X       VALUE SPACES.        ECS019
00282         05 HD-ST                 PIC XX.                          ECS019
00283         05 FILLER                PIC X       VALUE SPACES.        ECS019
00284         05 HD-ST-NM              PIC X(15).                       ECS019
00285         05 FILLER                PIC XX      VALUE SPACES.        ECS019
00286         05 HD-ACCT               PIC X(10).                       ECS019
00287      03 ACCT-HDB REDEFINES HEAD3.                                 ECS019
00288         05 ACCT-HD3              PIC X(43).                       ECS019
00289      03 ST-HDB REDEFINES HEAD3.                                   ECS019
00290         05 ST-HD3                PIC X(28).                       ECS019
00291         05 FILLER                PIC X(15).                       ECS019
00292      03 CO-HDB REDEFINES HEAD3.                                   ECS019
00293         05 CO-HD3                PIC X(13).                       ECS019
00294         05 FILLER                PIC X(30).                       ECS019
00295      03 CARR-HDB REDEFINES HEAD3.                                 ECS019
00296         05 CARR-HD3              PIC X(4).                        ECS019
00297         05 FILLER                PIC X(39).                       ECS019
00298      03 RPT-CD-1-HD3.                                             ECS019
00299         05 FILLER                PIC XX      VALUE SPACES.        ECS019
00300         05 HD-RPT-CD-1           PIC X(10).                       ECS019
00301                                                                   ECS019
00302      03 SUMM-HD.                                                  ECS019
00303         05 HD-SUMM-CARR-DESC.                                     ECS019
00304            07 FILLER             PIC X(8)    VALUE 'CARRIER '.    ECS019
00305            07 HD-SUMM-CARR       PIC XXX.                         ECS019
00306            07 HD-SUMM-FINAL-DESC.                                 ECS019
00307               09 FILLER          PIC X(8)    VALUE '  STATE '.    ECS019
00308               09 HD-SUMM-ST      PIC XXX.                         ECS019
00309               09 HD-SUMM-ST-NM   PIC X(15).                       ECS019
00310  EJECT                                                            ECS019
00311  01  WORK-REC.                                                    ECS019
00312      12  W-SEQ.                                                   ECS019
00313          16  W-ACCT-CNTL.                                         ECS019
00314              20  W-CARR              PIC X.                       ECS019
00315              20  W-GROUP             PIC X(6).                    ECS019
00316              20  W-ST                PIC XX.                      ECS019
00317              20  W-ACCT              PIC X(10).                   ECS019
00318          16  W-IG                    PIC 9.                       ECS019
00319          16  W-TYPE                  PIC XXX.                     ECS019
00320      12  W-CODE                      PIC 9.                       ECS019
00321      12  W-AMTS.
00322          16  W-AMT                   PIC S9(9)V99  COMP-3.        ECS019
00323          16  W-BASE                  PIC S9(7)V99  COMP-3.        ECS019
00324          16  W-OVER                  PIC S9(7)V99  COMP-3.        ECS019
040504         16  W-DLR-INC               PIC S9(7)V99  COMP-3.
011410         16  W-LF-LMBA-FEE           PIC S9(7)V99  COMP-3.
011410         16  W-AH-LMBA-FEE           PIC S9(7)V99  COMP-3.
               16  W-BANK-FEE              PIC S9(7)V99  COMP-3.
               16  W-CSO-ADMIN             PIC S9(7)V99  COMP-3.
00325      12  W-PROCESS-DATE              PIC 9(7)      COMP-3.           CL**3
00326      12  FILLER                      PIC X(6).
011410     12  W-SPPDD-CLP                 PIC S9(7)V99  COMP-3.
011410     12  FILLER                      PIC X(85).
00327                                                                   ECS019
00328  01  CUR-SEQ.                                                     ECS019
00329      12  CUR-ACCT-CNTL.                                           ECS019
00330          16  CUR-CARR                PIC X.                       ECS019
00331          16  CUR-GROUP               PIC X(6).                    ECS019
00332          16  CUR-ST                  PIC XX.                      ECS019
00333          16  CUR-ACCT                PIC X(10).                   ECS019
00334      12  CUR-IG                      PIC 9.                       ECS019
00335      12  CUR-TYPE.                                                ECS019
00336          16  CUR-TYP                 PIC XX.                      ECS019
00337          16  CUR-OB                  PIC X.                       ECS019
00338                                                                   ECS019
00339  01  BLD-DESC.                                                    ECS019
00340      12  BLD-TOTAL                   PIC X(6)    VALUE SPACES.    ECS019
00341      12  BLD-DESC1                   PIC XX      VALUE SPACES.    ECS019
00342      12  FILLER                      PIC XX      VALUE SPACES.    ECS019
00343      12  B-SLASH1                    PIC X       VALUE SPACE.     ECS019
00344      12  FILLER                      PIC XX      VALUE SPACES.    ECS019
00345      12  BLD-DESC2                   PIC XX      VALUE SPACES.    ECS019
00346      12  FILLER                      PIC X       VALUE SPACE.     ECS019
00347                                                                   ECS019
00348  01  X-ZERO.                                                      ECS019
011410     12  FILLER                      PIC X(108000).               ECS019
00350                                                                   ECS019
00351  01  X-ZERO-5.                                                    ECS019
011410     12  FILLER                      PIC X(60).
00353                                                                   ECS019
00354  01  WX-POINTER.                                                  ECS019
00355      12  WX-LAH                      PIC 9.                       ECS019
00356      12  WX-TYPE.                                                 ECS019
00357          16  WX-TYP                  PIC XX.                      ECS019
00358          16  WX-OB                   PIC X.                       ECS019
00359                                                                   ECS019
00360  01  WX-DESC.                                                     ECS019
00361      12  WX-DESC-10                  PIC X(10).                   ECS019
00362      12  FILLER                      PIC XX.                      ECS019
00363      12  WX-DESC-OB                  PIC X(4).                    ECS019
00364                                                                   ECS019
00365  01  X-POINTERS.                                                  ECS019
092602     12  X-POINTER OCCURS 900 TIMES.                              ECS019
00367          16  X-LAH                   PIC 9.                       ECS019
00368          16  X-TYPE.                                              ECS019
00369              20  X-TYP               PIC XX.                      ECS019
00370              20  X-OB                PIC X.                       ECS019
00371                                                                   ECS019
00372  01  X-DESCRIPTIONS.                                              ECS019
092602     12  X-DESC OCCURS 900 TIMES.                                 ECS019
00374          16  X-DESC-10               PIC X(10).                   ECS019
00375                                                                   ECS019
00376  01  X-TOTALS.                                                    ECS019
00377      12  X-TOTS OCCURS 2 TIMES.                                   ECS019
092602         16  X-LEVEL OCCURS 900 TIMES.                            ECS019
00379              20  X-AMTS.                                          ECS019
011410                 24  FILLER          PIC X(60).                   ECS019
00381                                                                   ECS019
00382  01  X-DETL.                                                      ECS019
00383      12  X-ISSUE                     PIC S9(9)V99 COMP-3.         ECS019
00384      12  X-CANCL                     PIC S9(9)V99 COMP-3.         ECS019
00385      12  X-BASE                      PIC S9(9)V99 COMP-3.         ECS019
00386      12  X-OVER                      PIC S9(9)V99 COMP-3.         ECS019
00387      12  X-CLAIM                     PIC S9(9)V99 COMP-3.         ECS019
040504     12  X-DLR-INC                   PIC S9(9)V99 COMP-3.
011410     12  X-LF-LMBA-FEE               PIC S9(9)V99 COMP-3.
011410     12  X-AH-LMBA-FEE               PIC S9(9)V99 COMP-3.
           12  X-BANK-FEE                  PIC S9(9)V99 COMP-3.
           12  X-CSO-ADMIN                 PIC S9(9)V99 COMP-3.
00388                                                                   ECS019
00389  01  R-TOTALS.                                                    ECS019
00390      12  R-TOTS OCCURS 2 TIMES.                                   ECS019
092602         16  R-LEVEL OCCURS 900 TIMES.                            ECS019
00392              20  R-AMTS.                                          ECS019
011410                 24  FILLER          PIC X(60).                   ECS019
00394                                                                   ECS019
00395  01  R-DETL.                                                      ECS019
00396      12  R-ISSUE                     PIC S9(9)V99 COMP-3.         ECS019
00397      12  R-CANCL                     PIC S9(9)V99 COMP-3.         ECS019
00398      12  R-BASE                      PIC S9(9)V99 COMP-3.         ECS019
00399      12  R-OVER                      PIC S9(9)V99 COMP-3.         ECS019
00400      12  R-CLAIM                     PIC S9(9)V99 COMP-3.         ECS019
040504     12  R-DLR-INC                   PIC S9(9)V99 COMP-3.
011410     12  R-LF-LMBA-FEE               PIC S9(9)V99 COMP-3.
011410     12  R-AH-LMBA-FEE               PIC S9(9)V99 COMP-3.
           12  R-BANK-FEE                  PIC S9(9)V99 COMP-3.
           12  R-CSO-ADMIN                 PIC S9(9)V99 COMP-3.
00401                                                                   ECS019
00402  01  ST-TOTALS.                                                   ECS019
011410     12  FILLER                      PIC X(108000).               ECS019
00404                                                                   ECS019
00405  01  CO-TOTALS.                                                   ECS019
011410     12  FILLER                      PIC X(108000).               ECS019
00407                                                                   ECS019
00408  01  CARR-TOTALS.                                                 ECS019
011410     12  FILLER                      PIC X(108000).               ECS019
00410                                                                   ECS019
00411  01  RPT-CD-1-TOTALS.                                             ECS019
011410     12  FILLER                      PIC X(108000).               ECS019
00413                                                                   ECS019
00414  01  FINAL-TOTALS.                                                ECS019
011410     12  FILLER                      PIC X(108000).               ECS019
00416                                                                   ECS019
00417  01  SUB-TOTALS.                                                  ECS019
00418      12  S-ISSUE                     PIC S9(9)V99 COMP-3.         ECS019
00419      12  S-CANCL                     PIC S9(9)V99 COMP-3.         ECS019
00420      12  S-BASE                      PIC S9(9)V99 COMP-3.         ECS019
00421      12  S-OVER                      PIC S9(9)V99 COMP-3.         ECS019
00422      12  S-CLAIM                     PIC S9(9)V99 COMP-3.         ECS019
040504     12  S-DLR-INC                   PIC S9(9)V99 COMP-3.
011410     12  S-LF-LMBA-FEE               PIC S9(9)V99 COMP-3.
011410     12  S-AH-LMBA-FEE               PIC S9(9)V99 COMP-3.
           12  S-BANK-FEE                  PIC S9(9)V99 COMP-3.
           12  S-CSO-ADMIN                 PIC S9(9)V99 COMP-3.
00423                                                                   ECS019
00424  01  TOTALS.                                                      ECS019
00425      12  T-ISSUE                     PIC S9(9)V99 COMP-3.         ECS019
00426      12  T-CANCL                     PIC S9(9)V99 COMP-3.         ECS019
00427      12  T-BASE                      PIC S9(9)V99 COMP-3.         ECS019
00428      12  T-OVER                      PIC S9(9)V99 COMP-3.         ECS019
00429      12  T-CLAIM                     PIC S9(9)V99 COMP-3.         ECS019
040504     12  T-DLR-INC                   PIC S9(9)V99 COMP-3.
011410     12  T-LF-LMBA-FEE               PIC S9(9)V99 COMP-3.
011410     12  T-AH-LMBA-FEE               PIC S9(9)V99 COMP-3.
           12  T-BANK-FEE                  PIC S9(9)V99 COMP-3.
           12  T-CSO-ADMIN                 PIC S9(9)V99 COMP-3.
00430                                                                   ECS019
00431  01  P-REC.                                                       ECS019
00432      12  P-CCSW                      PIC X.                       ECS019
00433      12  P-LN.                                                    ECS019
00434          16  P-TYPE                  PIC X(4).                    ECS019
00435          16  P-DESC                  PIC X(16).                   ECS019
00436          16  FILLER                  PIC X(7).                    ECS019
00437          16  P-ISSUE                 PIC ZZZ,ZZZ,ZZZ.ZZ-.         ECS019
00438          16  P-CANCL                 PIC ZZZ,ZZZ,ZZZ.ZZ-.         ECS019
00439          16  P-NET                   PIC ZZZ,ZZZ,ZZZ.ZZ-.         ECS019
00440          16  P-BASE                  PIC ZZZ,ZZZ,ZZZ.ZZ-.         ECS019
00441          16  P-OVER                  PIC ZZZ,ZZZ,ZZZ.ZZ-.         ECS019
00442          16  P-TOTAL                 PIC ZZZ,ZZZ,ZZZ.ZZ-.         ECS019
00443          16  P-CLAIM                 PIC ZZZ,ZZZ,ZZZ.ZZ-.         ECS019
00431 *01  P-REC.                                                       ECS019
00432 *    12  P-CCSW                      PIC X.                       ECS019
00433 *    12  P-LN.                                                    ECS019
00434 *        16  P-TYPE                  PIC XXX.
00435 *        16  P-DESC                  PIC X(10).
00436 *        16  FILLER                  PIC XX.
00437 *        16  P-ISSUE                 PIC ZZ,ZZZ,ZZZ.ZZ-.
00438 *        16  P-CANCL                 PIC Z,ZZZ,ZZZ.ZZ-.
00439 *        16  P-NET                   PIC ZZ,ZZZ,ZZZ.ZZ-.
00440 *        16  P-BASE                  PIC Z,ZZZ,ZZZ.ZZ-.
00441 *        16  P-OVER                  PIC Z,ZZZ,ZZZ.ZZ-.
      *        16  P-TOTAL                 PIC ZZ,ZZZ,ZZZ.ZZ-.
      *        16  P-LMBA-FEE              PIC ZZ,ZZZ,ZZZ.ZZ-.
040504*        16  P-DLR-INC               PIC Z,ZZZ,ZZZ.ZZ-.
00443 *        16  P-CLAIM                 PIC ZZZ,ZZZ,ZZZ.ZZ-.
040504*        16  FILLER                  PIC X(8).
00444  EJECT                                                            ECS019
00445  PROCEDURE DIVISION.                                              ECS019
00446                                                                   ECS019
00447  CAPTURE-START.                                                   ECS019
00448  0000-SET-START.                                                  ECS019
00449                                  COPY ELCDTERX SUPPRESS.          ECS019
pemuni     open OUTPUT HISTORY-OUT.                                     ECS019
00451      MOVE WS-TIME                TO  ME-START-TIME.               ECS019
00452      MOVE WS-CURRENT-DATE        TO  ME-START-DATE.               ECS019
00453      MOVE ME-START-MO            TO  ME-CNDS-MO.                  ECS019
00454      MOVE ME-START-DA            TO  ME-CNDS-DA.                  ECS019
00455      MOVE ME-START-YR            TO  ME-CNDS-YR.                  ECS019
00456                                                                   ECS019
00457  0090-INITIALIZATION.                                             ECS019
00458      MOVE WS-CURRENT-DATE        TO  HD-RD.                       ECS019
00459      MOVE COMPANY-NAME           TO  HD-CO.                       ECS019
00460      MOVE ALPH-DATE              TO  HD-DT.                       ECS019
00461      MOVE RUN-MO                 TO  BEGIN-MO.                    ECS019
00462      COMPUTE BEGIN-CCYY = RUN-CCYY - 1.                           ECS019
00463                                                                   ECS019
00464      MOVE SPACES                 TO CUR-SEQ.                      ECS019
00465                                                                   ECS019
00466  0100-SET-REPORT-FORMAT.                                          ECS019
00467                                                                   ECS019
00468      MOVE DTE-FMT-OPT                TO  REPORT-OPTIONS.          ECS019
00469                                                                   ECS019
00470      IF QTR-TO-DATE                                               ECS019
00471          MOVE ' - QUARTER TO DATE'   TO  HD1-OPTION.              ECS019
00472                                                                   ECS019
00473      IF YEAR-TO-DATE                                              ECS019
00474          MOVE ' -  YEAR TO DATE  '   TO  HD1-OPTION.              ECS019
00475                                                                   ECS019
00476      IF LAST-TWELVE                                               ECS019
00477          MOVE ' - LAST 12 MONTHS '   TO  HD1-OPTION.              ECS019
00478                                                                   ECS019
00479  EJECT                                                            ECS019
00480  0110-SORT-ROUTINE SECTION.                                       ECS019
00481      SORT SORTFL                                                  ECS019
00482              ON ASCENDING KEY SRT-REPORT-CD-1 S-PARM              ECS019
00483          INPUT PROCEDURE 0120-INPUT-RTN  THRU 0179-INPUT-XIT      ECS019
00484         OUTPUT PROCEDURE 0180-OUTPUT-RTN THRU 0810-OUTPUT-XIT.    ECS019
00485                                                                   ECS019
00486      IF SORT-RETURN NOT = ZEROES                                  ECS019
00487          MOVE +0101                     TO  WS-RETURN-CODE        ECS019
00488          MOVE 'INTERNAL SORT ABORTED'   TO  WS-ABEND-MESSAGE      ECS019
00489          GO TO ABEND-PGM.                                         ECS019
00490                                                                   ECS019
00491      GO TO 0830-EOJ-RTN.                                          ECS019
00492      EJECT                                                        ECS019
00493  0120-INPUT-RTN SECTION.                                          ECS019
00494      OPEN INPUT EXTRACT  HISTORY-IN                               ECS019
00495                 ERACCTT                                           ECS019
pemuni*         OUTPUT HISTORY-OUT.                                     ECS019
00497                                                                   ECS019
00498      IF ERACCT-FILE-STATUS  = '00' OR '97'                        ECS019
00499          NEXT SENTENCE                                            ECS019
00500        ELSE                                                       ECS019
00501          MOVE +0302                      TO  WS-RETURN-CODE       ECS019
00502          MOVE 'ERROR OPENING ACCT MSTR'  TO  WS-ABEND-MESSAGE     ECS019
00503          GO TO ABEND-PGM.                                         ECS019
00504                                                                   ECS019
00505  0130-READ-EXTRACT.                                               ECS019
00506      READ EXTRACT INTO WORK-REC                                   ECS019
00507                   AT END GO TO 0140-READ-HISTORY.                 ECS019
00508                                                                   ECS019
00509      MOVE W-PROCESS-DATE         TO WS-PROCESS-DATE.                 CL**3
00510                                                                      CL**3
00511      PERFORM 0150-RLS-RTN       THRU 0159-RLS-XIT.                ECS019
00512      PERFORM 0160-WRITE-HISTORY THRU 0169-WRITE-XIT.              ECS019
00513                                                                   ECS019
00514        GO TO 0130-READ-EXTRACT.                                   ECS019
00515                                                                   ECS019
00516  0140-READ-HISTORY.                                               ECS019
00517      READ HISTORY-IN INTO WORK-REC                                ECS019
00518                   AT END GO TO 0170-CLOSE-FILES.                  ECS019
00519                                                                   ECS019
00520      MOVE W-PROCESS-DATE         TO WS-PROCESS-NUMERIC               CL**3
00521                                                                      CL**3
00522      IF W-YR = RUN-YR AND                                         ECS019
00523         W-MO = RUN-MO                                             ECS019
00524          GO TO 0140-READ-HISTORY.                                 ECS019
00525                                                                   ECS019
00526      IF WS-PROCESS-CCYYMM NOT GREATER THAN BEGIN-DATE                CL**4
00527          GO TO 0140-READ-HISTORY.                                 ECS019
00528                                                                   ECS019
00529      PERFORM 0160-WRITE-HISTORY THRU 0169-WRITE-XIT.              ECS019
00530                                                                   ECS019
00531      IF CURRENT-MONTH                                             ECS019
00532          GO TO 0140-READ-HISTORY.                                 ECS019
00533                                                                   ECS019
00534      IF LAST-TWELVE                                               ECS019
00535          PERFORM 0150-RLS-RTN THRU 0159-RLS-XIT                   ECS019
00536          GO TO 0140-READ-HISTORY.                                 ECS019
00537                                                                   ECS019
00538      IF W-YR NOT = RUN-YR                                         ECS019
00539          GO TO 0140-READ-HISTORY.                                 ECS019
00540                                                                   ECS019
00541      IF YEAR-TO-DATE                                              ECS019
00542          PERFORM 0150-RLS-RTN THRU 0159-RLS-XIT                   ECS019
00543          GO TO 0140-READ-HISTORY.                                 ECS019
00544                                                                   ECS019
00545      IF 1ST-QTR                                                   ECS019
00546          IF W-MO = 01  OR  02  OR  03                             ECS019
00547              PERFORM 0150-RLS-RTN THRU 0159-RLS-XIT               ECS019
00548              GO TO 0140-READ-HISTORY.                             ECS019
00549                                                                   ECS019
00550      IF 2ND-QTR                                                   ECS019
00551          IF W-MO = 04  OR  05  OR  06                             ECS019
00552              PERFORM 0150-RLS-RTN THRU 0159-RLS-XIT               ECS019
00553              GO TO 0140-READ-HISTORY.                             ECS019
00554                                                                   ECS019
00555      IF 3RD-QTR                                                   ECS019
00556          IF W-MO = 07  OR  08  OR  09                             ECS019
00557              PERFORM 0150-RLS-RTN THRU 0159-RLS-XIT               ECS019
00558              GO TO 0140-READ-HISTORY.                             ECS019
00559                                                                   ECS019
00560      IF 4TH-QTR                                                   ECS019
00561          IF W-MO = 10  OR  11  OR  12                             ECS019
00562              PERFORM 0150-RLS-RTN THRU 0159-RLS-XIT.              ECS019
00563                                                                   ECS019
00564      GO TO 0140-READ-HISTORY.                                     ECS019
00565                                                                   ECS019
00566  0150-RLS-RTN.                                                    ECS019
00567                                                                   ECS019
102501     MOVE SPACES                 TO SRT-REPORT-CD-1.
00569                                                                   ECS019
00570      IF DTE-TOT-OPT EQUAL 2                                       ECS019
00571          IF W-ACCT NOT EQUAL HIGH-VALUES                          ECS019
00572              IF W-ACCT-CNTL NOT = CUR-ACCT-CNTL                   ECS019
00573                  MOVE W-SEQ             TO CUR-SEQ                ECS019
00574                  PERFORM 0430-RD-ACCT-RTN THRU 0440-RD-ACCT-XIT.  ECS019
00575                                                                   ECS019
00576      IF DTE-TOT-OPT EQUAL 2                                       ECS019
00577          IF W-CARR EQUAL HIGH-VALUES                              ECS019
00578              MOVE HIGH-VALUES       TO SRT-REPORT-CD-1            ECS019
00579          ELSE                                                     ECS019
00580              MOVE SAVE-REPORT-CD-1  TO SRT-REPORT-CD-1.           ECS019
00581                                                                   ECS019
00582                                                                   ECS019
00583      MOVE WORK-REC               TO SRT-WORK-REC.                 ECS019
00584      RELEASE SRT-REC.                                             ECS019
00585                                                                   ECS019
00586  0159-RLS-XIT.                                                    ECS019
00587      EXIT.                                                        ECS019
00588                                                                   ECS019
00589  0160-WRITE-HISTORY.                                              ECS019
00590      WRITE HISTORY-RECORD FROM WORK-REC.                          ECS019
00591  0169-WRITE-XIT.                                                  ECS019
00592      EXIT.                                                        ECS019
00593                                                                   ECS019
00594  0170-CLOSE-FILES.                                                ECS019
00595      CLOSE EXTRACT  HISTORY-IN  HISTORY-OUT.                      ECS019
00596  0179-INPUT-XIT.                                                  ECS019
00597      EXIT.                                                        ECS019
00598  EJECT                                                            ECS019
00599  0180-OUTPUT-RTN SECTION.                                         ECS019
00600      OPEN OUTPUT PRNTR.                                           ECS019
00601      MOVE SPACES                 TO  P-REC                        ECS019
00602                                      SAVE-REPORT-CD-1.            ECS019

102501     MOVE SPACES                 TO  CUR-SEQ.
00604                                                                   ECS019
00605      PERFORM 0690-FORMAT-RTN  THRU 0740-FORMAT-XIT.               ECS019
00606                                                                   ECS019
00607      MOVE 0                      TO  FST-SW PGCTR.                ECS019
00608      MOVE +066                   TO  LNCTR.                       ECS019
00609                                                                   ECS019
00610      IF DTE-TOT-OPT EQUAL 2                                       ECS019
00611          MOVE CLAS-REPORT-CD1-CAPTION TO HD1-RPT-CD-1-HDG.        ECS019
00612                                                                   ECS019
00613      IF DTE-CLIENT NOT EQUAL 'HER'                                ECS019
00614          MOVE SPACES             TO HD1-OPTION-2.                 ECS019
00615                                                                   ECS019
00616  0190-RETURN-LOOP.                                                ECS019
00617      RETURN SORTFL                                                ECS019
00618          AT END GO TO 0800-END-OUTPUT.                            ECS019
00619                                                                   ECS019
00620      MOVE SRT-WORK-REC           TO  WORK-REC.                    ECS019
00621      MOVE W-PROCESS-DATE         TO  WS-PROCESS-DATE.                CL**3
00622                                                                   ECS019
00623      IF DTE-TOT-OPT EQUAL 2                                       ECS019
00624          IF SRT-REPORT-CD-1 NOT EQUAL SAVE-REPORT-CD-1            ECS019
00625             PERFORM 0230-BREAK-RTN THRU 0410-BREAK-XIT.           ECS019
00626                                                                   ECS019
00627      IF W-SEQ NOT = CUR-SEQ                                       ECS019
00628         PERFORM 0230-BREAK-RTN THRU 0410-BREAK-XIT.               ECS019
00629                                                                   ECS019
00630      GO TO 0200-ACCUM-ISSUE                                       ECS019
00631            0210-ACCUM-CANCL                                       ECS019
00632            0220-ACCUM-CLAIM                                       ECS019
00633         DEPENDING ON W-CODE.                                      ECS019
00634                                                                   ECS019
00635      DISPLAY 'INVALID CODE IN EXTRACT REC - ' W-CODE.             ECS019
00636      MOVE +0301                             TO  WS-RETURN-CODE    ECS019
00637      MOVE 'INVALID CODE IN EXTRACT RECORD'  TO WS-ABEND-MESSAGE.  ECS019
00638      GO TO ABEND-PGM.                                             ECS019
00639      EJECT                                                        ECS019
00640  0200-ACCUM-ISSUE.                                                ECS019
00641      ADD W-AMT                   TO  X-ISSUE.                     ECS019
00642      ADD W-BASE                  TO  X-BASE.                      ECS019
00643      ADD W-OVER                  TO  X-OVER.                      ECS019
040504     ADD W-DLR-INC               TO  X-DLR-INC
011410     ADD W-LF-LMBA-FEE           TO  X-LF-LMBA-FEE
011410     ADD W-AH-LMBA-FEE           TO  X-AH-LMBA-FEE
           ADD W-BANK-FEE              TO  X-BANK-FEE
           ADD W-CSO-ADMIN             TO  X-CSO-ADMIN
00644      GO TO 0190-RETURN-LOOP.                                      ECS019
00645                                                                   ECS019
00646  0210-ACCUM-CANCL.                                                ECS019
00647      ADD W-AMT                   TO  X-CANCL.                     ECS019
00648      ADD W-BASE                  TO  X-BASE.                      ECS019
00649      ADD W-OVER                  TO  X-OVER.                      ECS019
040504     ADD W-DLR-INC               TO  X-DLR-INC
011410     ADD W-LF-LMBA-FEE           TO  X-LF-LMBA-FEE
011410     ADD W-AH-LMBA-FEE           TO  X-AH-LMBA-FEE
           ADD W-BANK-FEE              TO  X-BANK-FEE
           ADD W-CSO-ADMIN             TO  X-CSO-ADMIN
00650      GO TO 0190-RETURN-LOOP.                                      ECS019
00651                                                                   ECS019
00652  0220-ACCUM-CLAIM.                                                ECS019
00653      ADD W-AMT                   TO  X-CLAIM.                     ECS019
00654      GO TO 0190-RETURN-LOOP.                                      ECS019
00655                                                                   ECS019
00656  0230-BREAK-RTN.                                                  ECS019
00657      IF FST-SW = 0                                                ECS019
00658          MOVE 1                  TO  FST-SW                       ECS019
00659          GO TO 0330-INTL-FINAL.                                   ECS019
00660                                                                   ECS019
00661  0240-LINE-BREAK.                                                 ECS019
00662      MOVE X-DETL                 TO  X-AMTS (X1 X2).              ECS019
00663      MOVE W-TYPE                 TO  CUR-TYPE.                    ECS019
00664      MOVE W-IG                   TO  CUR-IG.                      ECS019
00665                                                                   ECS019
00666      IF CUR-SEQ = W-SEQ                                           ECS019
00667          GO TO 0390-INTL-LINE.                                    ECS019
00668                                                                   ECS019
00669      IF CUR-CARR = HIGH-VALUE                                     ECS019
00670          GO TO 0310-FINAL-SUMMARY.                                ECS019
00671                                                                   ECS019
00672      IF CUR-ACCT = HIGH-VALUE                                     ECS019
00673          GO TO 0290-CARR-SUMMARY.                                 ECS019
00674                                                                   ECS019
00675  0250-ACCT-BREAK.                                                 ECS019
00676                                                                   ECS019
00677      IF DTE-CLIENT IS EQUAL TO 'HER'                              ECS019
00678          MOVE CUR-GROUP          TO  SAVE-GROUP.                  ECS019
00679                                                                   ECS019
00680      PERFORM 0430-RD-ACCT-RTN THRU 0440-RD-ACCT-XIT.              ECS019
00681                                                                   ECS019
00682      IF AM-CONTROL-PRIMARY EQUAL HIGH-VALUES AND                  ECS019
00683         ERACCT-FILE-STATUS NOT = '10'                             ECS019
00684          MOVE 'INVALID ACCOUNT'  TO  SAVE-NAME.                   ECS019
00685                                                                   ECS019
00686      IF DTE-CLIENT IS EQUAL TO 'HER'                              ECS019
00687          MOVE SAVE-GROUP         TO  CUR-GROUP.                   ECS019
00688                                                                   ECS019
00689 *  TOTAL LEVEL 1.                                                 ECS019
00690      MOVE ACCT-HD2               TO  SUB-HD2.                     ECS019
00691      MOVE ACCT-HD3               TO  SUB-HD3.                     ECS019
00692      MOVE ST-TOTALS              TO  R-TOTALS.                    ECS019
00693      PERFORM 0640-ROLL-RTN THRU 0680-ROLL-XIT.                    ECS019
00694      MOVE R-TOTALS               TO  ST-TOTALS.                   ECS019
00695                                                                   ECS019
00696      IF DTE-PGM-OPT NOT GREATER THAN '1'                          ECS019
00697          PERFORM 0460-BLD-RTN THRU 0490-BLD-XIT.                  ECS019
00698                                                                   ECS019
00699      MOVE SPACES                 TO  SAVE-NAME.                   ECS019
00700      MOVE W-ACCT                 TO  CUR-ACCT.                    ECS019
00701      IF CUR-SEQ = W-SEQ                                           ECS019
00702          GO TO 0380-INTL-ACCT.                                    ECS019
00703                                                                   ECS019
00704  0270-ST-BREAK.                                                   ECS019
00705 *  TOTAL LEVEL 2.                                                 ECS019
00706      MOVE ST-HD2                 TO  SUB-HD2.                     ECS019
00707      MOVE ST-HD3                 TO  SUB-HD3.                     ECS019
00708      MOVE ST-TOTALS              TO  X-TOTALS.                    ECS019
00709      MOVE CO-TOTALS              TO  R-TOTALS.                    ECS019
00710      PERFORM 0640-ROLL-RTN THRU 0680-ROLL-XIT.                    ECS019
00711      MOVE R-TOTALS               TO  CO-TOTALS.                   ECS019
00712                                                                   ECS019
00713      IF DTE-PGM-OPT NOT GREATER THAN '2'                          ECS019
00714          PERFORM 0460-BLD-RTN THRU 0490-BLD-XIT.                  ECS019
00715                                                                   ECS019
00716      MOVE W-ST                   TO  CUR-ST.                      ECS019
00717      IF CUR-SEQ = W-SEQ                                           ECS019
00718          GO TO 0360-INTL-ST.                                      ECS019
00719                                                                   ECS019
00720  0280-CO-BREAK.                                                   ECS019
00721 *  TOTAL LEVEL 3.                                                 ECS019
00722      MOVE CO-HD2                 TO  SUB-HD2.                     ECS019
00723      MOVE CO-HD3                 TO  SUB-HD3.                     ECS019
00724      MOVE CO-TOTALS              TO  X-TOTALS.                    ECS019
00725      MOVE CARR-TOTALS            TO  R-TOTALS.                    ECS019
00726      PERFORM 0640-ROLL-RTN THRU 0680-ROLL-XIT.                    ECS019
00727      MOVE R-TOTALS               TO  CARR-TOTALS.                 ECS019
00728                                                                   ECS019
00729      IF DTE-PGM-OPT NOT GREATER THAN '3'                          ECS019
00730          PERFORM 0460-BLD-RTN THRU 0490-BLD-XIT.                  ECS019
00731                                                                   ECS019
00732      GO TO 0350-INTL-CO.                                          ECS019
00733                                                                   ECS019
00734  0290-CARR-SUMMARY.                                               ECS019
00735 *  TOTAL LEVEL 4.                                                 ECS019
013107     MOVE 'CARRIER, STATE SUMMARY'
013107                                 TO SUB-HD2
00737      MOVE HD-SUMM-CARR-DESC      TO  SUB-HD3.                     ECS019
00738                                                                   ECS019
00739      IF DTE-PGM-OPT NOT GREATER THAN '4'                          ECS019
00740          PERFORM 0460-BLD-RTN THRU 0490-BLD-XIT.                  ECS019
00741                                                                   ECS019
00742      IF DTE-TOT-OPT EQUAL 2                                       ECS019
00743          IF SRT-REPORT-CD-1 NOT EQUAL SAVE-REPORT-CD-1            ECS019
00744              GO TO 0300-CARR-BREAK.                               ECS019
00745                                                                   ECS019
00746      IF W-CARR = CUR-CARR                                         ECS019
00747          GO TO 0350-INTL-CO.                                      ECS019
00748                                                                   ECS019
00749  0300-CARR-BREAK.                                                 ECS019
00750 *  TOTAL LEVEL 5.                                                 ECS019
00751      MOVE CARR-HD2               TO  SUB-HD2.                     ECS019
013107     MOVE 'CARRIER SUMMARY'      TO  SUB-HD2.                     ECS019
00752      MOVE CARR-HD3               TO  SUB-HD3.                     ECS019
00753      MOVE CARR-TOTALS            TO  X-TOTALS.                    ECS019
00754                                                                   ECS019
00755      IF DTE-TOT-OPT EQUAL 2                                       ECS019
00756          MOVE RPT-CD-1-TOTALS    TO  R-TOTALS                     ECS019
00757      ELSE                                                         ECS019
00758          MOVE FINAL-TOTALS       TO  R-TOTALS.                    ECS019
00759                                                                   ECS019
00760      PERFORM 0640-ROLL-RTN THRU 0680-ROLL-XIT.                    ECS019
00761                                                                   ECS019
00762      IF DTE-TOT-OPT EQUAL 2                                       ECS019
00763          MOVE R-TOTALS           TO  RPT-CD-1-TOTALS              ECS019
00764      ELSE                                                         ECS019
00765          MOVE R-TOTALS           TO  FINAL-TOTALS.                ECS019
00766                                                                   ECS019
00767      IF DTE-PGM-OPT NOT GREATER THAN '5'                          ECS019
00768          PERFORM 0460-BLD-RTN THRU 0490-BLD-XIT.                  ECS019
00769                                                                   ECS019
00770      IF DTE-TOT-OPT EQUAL 2                                       ECS019
00771          IF SRT-REPORT-CD-1 NOT EQUAL SAVE-REPORT-CD-1            ECS019
00772              GO TO 0305-REPORT-CD-1-BREAK.                        ECS019
00773                                                                   ECS019
00774      GO TO 0340-INTL-CARR.                                        ECS019
00775                                                                   ECS019
00776  0305-REPORT-CD-1-BREAK.                                          ECS019
00777 *  TOTAL LEVEL 6.                                                 ECS019
00778      MOVE SPACES                 TO  SUB-HD2.                     ECS019
00779      MOVE SPACES                 TO  SUB-HD3.                     ECS019
00780      MOVE RPT-CD-1-TOTALS        TO  X-TOTALS.                    ECS019
00781      MOVE FINAL-TOTALS           TO  R-TOTALS.                    ECS019
00782      PERFORM 0640-ROLL-RTN THRU 0680-ROLL-XIT.                    ECS019
00783      MOVE R-TOTALS               TO  FINAL-TOTALS.                ECS019
00784                                                                   ECS019
00785      PERFORM 0460-BLD-RTN THRU 0490-BLD-XIT.                      ECS019
00786                                                                   ECS019
00787      GO TO 0335-INTL-RPT-CD-1.                                    ECS019
00788                                                                   ECS019
00789  0310-FINAL-SUMMARY.                                              ECS019
00790 *  TOTAL LEVEL 7.                                                 ECS019
00791      MOVE 'FINAL SUMMARY'        TO  SUB-HD2.                     ECS019
00792      MOVE HD-SUMM-FINAL-DESC     TO  SUB-HD3.                     ECS019
00793      MOVE SPACES                 TO HD1-RPT-CD-1-HDG              ECS019
00794                                     HD1-RPT-CD-1 HD1-FILL.        ECS019
00795                                                                   ECS019
00796      IF DTE-PGM-OPT NOT GREATER THAN '6'                          ECS019
00797          PERFORM 0460-BLD-RTN THRU 0490-BLD-XIT.                  ECS019
00798                                                                   ECS019
00799      IF FST-SW NOT = 2                                            ECS019
00800          GO TO 0340-INTL-CARR.                                    ECS019
00801                                                                   ECS019
00802  0320-FINAL-BREAK.                                                ECS019
00803 *  TOTAL LEVEL 7.                                                 ECS019
00804      MOVE 'FINAL TOTALS'         TO  SUB-HD2.                     ECS019
00805      MOVE SPACES                 TO  SUB-HD3.                     ECS019
00806      MOVE FINAL-TOTALS           TO  X-TOTALS.                    ECS019
00807                                                                   ECS019
00808      IF DTE-FICH = '1'                                            ECS019
00809          MOVE '2'                TO DTE-FICH.                     ECS019
00810                                                                   ECS019
00811      IF DTE-PGM-OPT NOT GREATER THAN '7'                          ECS019
00812          PERFORM 0460-BLD-RTN THRU 0490-BLD-XIT.                  ECS019
00813                                                                   ECS019
00814      GO TO 0410-BREAK-XIT.                                        ECS019
00815                                                                   ECS019
00816  0330-INTL-FINAL.                                                 ECS019
00817      PERFORM 0600-ZERO-RTN THRU 0630-ZERO-XIT.                    ECS019
00818      MOVE X-ZERO                 TO  FINAL-TOTALS.                ECS019
00819                                                                   ECS019
00820  0335-INTL-RPT-CD-1.                                              ECS019
00821      MOVE SRT-REPORT-CD-1        TO  SAVE-REPORT-CD-1             ECS019
00822                                      HD-RPT-CD-1                  ECS019
00823                                      HD1-RPT-CD-1.                ECS019
00824      MOVE X-ZERO                 TO  RPT-CD-1-TOTALS.             ECS019
00825                                                                   ECS019
00826  0340-INTL-CARR.                                                  ECS019
00827                                                                   ECS019
00828      MOVE W-CARR        TO CUR-CARR HD-CARR HD-SUMM-CARR.         ECS019
00829                                                                   ECS019
00830      MOVE X-ZERO                 TO  CARR-TOTALS.                 ECS019
00831                                                                   ECS019
00832  0350-INTL-CO.                                                    ECS019
00833      MOVE W-GROUP                TO  CUR-GROUP HD-GROUP.          ECS019
00834      MOVE X-ZERO                 TO  CO-TOTALS.                   ECS019
00835                                                                   ECS019
00836  0360-INTL-ST.                                                    ECS019
00837      MOVE W-ST                   TO  CUR-ST HD-ST                 ECS019
00838                                      HD-SUMM-ST  STATE-L.         ECS019
00839      MOVE X-ZERO                 TO  ST-TOTALS.                   ECS019
00840                                                                   ECS019
00841      IF CLAS-MAXS NOT GREATER ZEROS                               ECS019
00842          MOVE 'INVALID STATE'    TO  HD-ST-NM HD-SUMM-ST-NM       ECS019
00843              GO TO 0380-INTL-ACCT.                                ECS019
00844                                                                   ECS019
00845      MOVE CLAS-STARTS            TO  CLAS-INDEXS.                 ECS019
00846      EJECT                                                        ECS019
00847  0370-FIND-ST-DESC.                                               ECS019
00848      IF CLAS-INDEXS GREATER CLAS-MAXS                             ECS019
00849          MOVE 'INVALID STATE'    TO  HD-ST-NM HD-SUMM-ST-NM       ECS019
00850              GO TO 0380-INTL-ACCT.                                ECS019
00851                                                                   ECS019
00852      IF STATE-SUB (CLAS-INDEXS) NOT = STATE-L                     ECS019
00853          ADD +1                  TO  CLAS-INDEXS                  ECS019
00854              GO TO 0370-FIND-ST-DESC.                             ECS019
00855                                                                   ECS019
00856      MOVE STATE-PIC (CLAS-INDEXS)    TO  HD-ST-NM HD-SUMM-ST-NM.  ECS019
00857                                                                   ECS019
00858  0380-INTL-ACCT.                                                  ECS019
00859      MOVE W-ACCT                 TO  CUR-ACCT HD-ACCT.            ECS019
00860      MOVE X-ZERO                 TO  X-TOTALS.                    ECS019
00861                                                                   ECS019
00862  0390-INTL-LINE.                                                  ECS019
00863      MOVE W-IG                   TO  CUR-IG WX-LAH.               ECS019
00864      MOVE W-TYPE                 TO  CUR-TYPE WX-TYPE.            ECS019
00865                                                                   ECS019
00866      IF W-IG LESS THAN 3                                          ECS019
00867          MOVE 1                  TO  X1                           ECS019
00868         ELSE                                                      ECS019
00869          MOVE 2                  TO  X1.                          ECS019
00870                                                                   ECS019
00871      IF W-IG = 1 OR 3                                             ECS019
00872          MOVE 1                  TO  WX-LAH                       ECS019
00873        ELSE                                                       ECS019
00874          MOVE 2                  TO  WX-LAH.                      ECS019
00875                                                                   ECS019
00876      MOVE 1                      TO  X2.                          ECS019
00877                                                                   ECS019
00878  0400-LOOP-LINE.                                                  ECS019
00879      IF WX-POINTER = X-POINTER (X2)                               ECS019
00880          MOVE X-AMTS (X1 X2)     TO  X-DETL                       ECS019
00881          GO TO 0410-BREAK-XIT.                                    ECS019
00882                                                                   ECS019
00883      ADD 1                       TO  X2.                          ECS019
00884                                                                   ECS019
00885      IF X-TYPE (X2) = HIGH-VALUE                                  ECS019
00886          DISPLAY 'INVALID TYPE - ' WX-POINTER                     ECS019
00887          MOVE +0301              TO  WS-RETURN-CODE               ECS019
00888          MOVE 'INVALID TYPE - '  TO  WS-ABEND-MESSAGE             ECS019
00889          GO TO ABEND-PGM.                                         ECS019
00890                                                                   ECS019
00891      GO TO 0400-LOOP-LINE.                                        ECS019
00892                                                                   ECS019
00893  0410-BREAK-XIT.                                                  ECS019
00894      EXIT.                                                        ECS019
00895  EJECT                                                            ECS019
00896  0430-RD-ACCT-RTN.                                                ECS019
00897                                                                   ECS019
00898      MOVE SPACES                 TO SAVE-REPORT-CD-1              ECS019
00899                                     SAVE-NAME.

102501     MOVE SPACES                 TO AM-CONTROL-PRIMARY.

00902      MOVE DTE-CLASIC-COMPANY-CD  TO AM-COMPANY-CD.                ECS019
00903                                                                   ECS019
00904      MOVE CUR-CARR               TO AM-CARRIER.                   ECS019
00905                                                                   ECS019
00906      IF (DTE-CLIENT EQUAL 'HER') AND                              ECS019
00907         (CUR-GROUP EQUAL 'BEFORE' OR ' AFTER')                    ECS019
00908            MOVE '600000'         TO AM-GROUPING                   ECS019
00909                                     CUR-GROUP                     ECS019
00910      ELSE                                                         ECS019
00911         MOVE CUR-GROUP           TO AM-GROUPING.                  ECS019
00912                                                                   ECS019
00913      MOVE CUR-ST                 TO AM-STATE.                     ECS019
00914      MOVE CUR-ACCT               TO AM-ACCOUNT
           MOVE ZEROS TO AM-EXPIRE-DT
00915                                                                   ECS019
00916      START ERACCTT                                                ECS019
00917          KEY NOT LESS THAN AM-CONTROL-PRIMARY.                    ECS019
00918                                                                   ECS019
00919      IF ERACCT-FILE-STATUS NOT = '00'                             ECS019
               DISPLAY 'CAR ' CUR-CARR ' GRP ' CUR-GROUP ' ST '
                 CUR-ST ' ACT ' CUR-ACCT ' EXP ' AM-EXPIRE-DT
00920          MOVE +0300                        TO  WS-RETURN-CODE     ECS019
00921          MOVE 'START ERROR ACCT MASTER'    TO  WS-ABEND-MESSAGE   ECS019
00922          MOVE ERACCT-FILE-STATUS TO  WS-ABEND-FILE-STATUS         ECS019
00923          GO TO ABEND-PGM.                                         ECS019
00924                                                                   ECS019
00925  0440-READ-ERACCTT-NEXT.                                          ECS019
00926                                                                   ECS019
00927      READ ERACCTT NEXT.                                           ECS019
00928                                                                   ECS019
00929      IF ERACCT-FILE-STATUS = '10'                                 ECS019
00930          MOVE HIGH-VALUES        TO AM-CONTROL-PRIMARY            ECS019
00931          GO TO 0440-RD-ACCT-XIT.                                  ECS019
00932                                                                   ECS019
00933      IF ERACCT-FILE-STATUS NOT = '00'                             ECS019
00934          MOVE +0301                        TO  WS-RETURN-CODE     ECS019
00935          MOVE 'ERROR READING ACCT MASTER'  TO  WS-ABEND-MESSAGE   ECS019
00936          MOVE ERACCT-FILE-STATUS TO  WS-ABEND-FILE-STATUS         ECS019
00937          GO TO ABEND-PGM.                                         ECS019
00938                                                                   ECS019
00939      IF AM-COMPANY-CD GREATER THAN DTE-CLASIC-COMPANY-CD          ECS019
00940          MOVE HIGH-VALUES        TO AM-CONTROL-PRIMARY            ECS019
00941          GO TO 0440-RD-ACCT-XIT.                                  ECS019
00942                                                                   ECS019
00943      IF CUR-ACCT-CNTL = AM-CONTROL-A                              ECS019
00944          MOVE AM-REPORT-CODE-1   TO SAVE-REPORT-CD-1              ECS019
00945          MOVE AM-NAME            TO SAVE-NAME                     ECS019
00946          GO TO 0440-READ-ERACCTT-NEXT.                            ECS019
00947                                                                   ECS019
00948  0440-RD-ACCT-XIT.                                                ECS019
00949      EXIT.                                                        ECS019
00950  EJECT                                                            ECS019
00951  0460-BLD-RTN.                                                    ECS019
00952      PERFORM 0750-HD-RTN THRU 0760-HD-XIT.                        ECS019
00953      MOVE 1                      TO  X1.                          ECS019
00954      MOVE 'INDIVIDUAL'           TO  HD-DESC.                     ECS019
00955      MOVE SPACE-3                TO  P-CCSW.                      ECS019
00956      PERFORM 0510-PRT-EXTRACT THRU 0570-PRT-EXTRACT-XIT.          ECS019
00957      MOVE PRT-SW                 TO  IND-SW.                      ECS019
00958      MOVE 2                      TO  X1.                          ECS019
00959      MOVE 'GROUP'                TO  HD-DESC.                     ECS019
00960      MOVE SPACE-3                TO  P-CCSW.                      ECS019
00961      PERFORM 0510-PRT-EXTRACT THRU 0570-PRT-EXTRACT-XIT.          ECS019
00962                                                                   ECS019
00963      MOVE PRT-SW                 TO  GRP-SW.                      ECS019
00964                                                                   ECS019
00965      IF IND-SW = 0 OR                                             ECS019
00966         GRP-SW = 0                                                ECS019
00967          GO TO 0490-BLD-XIT.                                      ECS019
00968                                                                   ECS019
00969      MOVE 0                      TO  X2.                          ECS019
00970                                                                   ECS019
00971  0470-TOTAL-LOOP.                                                 ECS019
00972      ADD 1                       TO  X2.                          ECS019
00973      IF X-POINTER (X2) = HIGH-VALUE                               ECS019
00974          GO TO 0480-TOTAL-XIT.                                    ECS019
00975                                                                   ECS019
00976      MOVE X-AMTS (1 X2)          TO  X-DETL.                      ECS019
00977      MOVE X-AMTS (2 X2)          TO  R-DETL.                      ECS019
00978      ADD R-ISSUE                 TO  X-ISSUE.                     ECS019
00979      ADD R-CANCL                 TO  X-CANCL.                     ECS019
00980      ADD R-BASE                  TO  X-BASE.                      ECS019
00981      ADD R-OVER                  TO  X-OVER.                      ECS019
040504     ADD R-DLR-INC               TO  X-DLR-INC
011410     ADD R-LF-LMBA-FEE           TO  X-LF-LMBA-FEE
011410     ADD R-AH-LMBA-FEE           TO  X-AH-LMBA-FEE
           ADD R-BANK-FEE              TO  X-BANK-FEE
           ADD R-CSO-ADMIN             TO  X-CSO-ADMIN
00982      ADD R-CLAIM                 TO  X-CLAIM.                     ECS019
00983      MOVE X-DETL                 TO  X-AMTS (1 X2).               ECS019
00984      GO TO 0470-TOTAL-LOOP.                                       ECS019
00985                                                                   ECS019
00986  0480-TOTAL-XIT.                                                  ECS019
00987      MOVE 'TOTAL'                TO  HD-DESC.                     ECS019
00988      MOVE 1                      TO  X1.                          ECS019
00989      MOVE SPACE-3                TO  P-CCSW.                      ECS019
00990      PERFORM 0510-PRT-EXTRACT THRU 0570-PRT-EXTRACT-XIT.          ECS019
00991                                                                   ECS019
00992  0490-BLD-XIT.                                                    ECS019
00993      EXIT.                                                        ECS019
00994  EJECT                                                            ECS019
00995  0510-PRT-EXTRACT.                                                ECS019
00996      MOVE 0                      TO  PRT-SW LIFE-SW AH-SW X2.     ECS019
00997      MOVE X-ZERO-5               TO  SUB-TOTALS.                  ECS019
00998                                                                   ECS019
00999  0520-LOOP-LIFE.                                                  ECS019
01000      ADD 1                       TO  X2.                          ECS019
01001                                                                   ECS019
01002      IF X-POINTER (X2) = HIGH-VALUE                               ECS019
01003          GO TO 0530-OUT-LIFE.                                     ECS019
01004                                                                   ECS019
01005      IF X-LAH (X2) NOT = 1                                        ECS019
01006          SUBTRACT 1 FROM X2                                       ECS019
01007          GO TO 0530-OUT-LIFE.                                     ECS019
01008                                                                   ECS019
01009      MOVE X-AMTS (X1 X2)         TO  X-DETL.                      ECS019
01010                                                                   ECS019
01011      IF X-DETL = X-ZERO-5                                         ECS019
01012          GO TO 0520-LOOP-LIFE.                                    ECS019
01013                                                                   ECS019
01014      IF PRT-SW = 0                                                ECS019
01015          MOVE 1                  TO  PRT-SW                       ECS019
01016          MOVE 1                  TO  LIFE-SW                      ECS019
01017          MOVE HD4                TO  P-LN                         ECS019
01018          PERFORM 0770-PRT-RTN THRU 0790-PRT-XIT                   ECS019
01019          MOVE HD5                TO  P-LN                         ECS019
01020          PERFORM 0770-PRT-RTN THRU 0790-PRT-XIT                   ECS019
01021          MOVE SPACE-2            TO  P-CCSW.                      ECS019
01022                                                                   ECS019
01023      MOVE X-POINTER (X2)         TO  WX-POINTER.                  ECS019
01024      MOVE X-DESC (X2)            TO  WX-DESC.                     ECS019
01025                                                                   ECS019
01026      IF WX-OB = '1'                                               ECS019
01027          MOVE '1YR'              TO  WX-DESC-OB.                  ECS019
01028      IF WX-OB = '2'                                               ECS019
01029          MOVE 'REN'              TO  WX-DESC-OB.                  ECS019
01030                                                                   ECS019
01031      ADD X-ISSUE                 TO  S-ISSUE.                     ECS019
01032      ADD X-CANCL                 TO  S-CANCL.                     ECS019
01033      ADD X-BASE                  TO  S-BASE.                      ECS019
01034      ADD X-OVER                  TO  S-OVER.                      ECS019
040504     ADD X-DLR-INC               TO  S-DLR-INC
011410     ADD X-LF-LMBA-FEE           TO  S-LF-LMBA-FEE
011410     ADD X-AH-LMBA-FEE           TO  S-AH-LMBA-FEE
           ADD X-BANK-FEE              TO  S-BANK-FEE
           ADD X-CSO-ADMIN             TO  S-CSO-ADMIN
01035      ADD X-CLAIM                 TO  S-CLAIM.                     ECS019
01036      PERFORM 0580-PRT-LINE THRU 0590-PRT-LINE-XIT.                ECS019
01037      GO TO 0520-LOOP-LIFE.                                        ECS019
01038                                                                   ECS019
01039  0530-OUT-LIFE.                                                   ECS019
01040      MOVE SUB-TOTALS             TO  TOTALS.                      ECS019
01041                                                                   ECS019
01042      IF LIFE-SW = 0                                               ECS019
01043          GO TO 0540-LOOP-AH.                                      ECS019
01044                                                                   ECS019
01045      MOVE SPACES                 TO  BLD-DESC.                    ECS019
01046      MOVE 'TOTAL '               TO  BLD-TOTAL.                   ECS019
01047      MOVE LIFE-OVERRIDE-L2       TO  BLD-DESC1.                   ECS019
01048      MOVE SPACES                 TO  B-SLASH1                     ECS019
01049                                      BLD-DESC2.                   ECS019
01050      MOVE BLD-DESC               TO  WX-DESC.                     ECS019
01051      MOVE SPACES                 TO  WX-POINTER.                  ECS019
01052      MOVE SUB-TOTALS             TO  X-DETL.                      ECS019
01053      PERFORM 0580-PRT-LINE THRU 0590-PRT-LINE-XIT.                ECS019
01054      MOVE SPACE-2                TO  P-CCSW.                      ECS019
01055      MOVE X-ZERO-5               TO  SUB-TOTALS.                  ECS019
01056      MOVE SPACES                 TO  BLD-DESC.                    ECS019
01057                                                                   ECS019
01058  0540-LOOP-AH.                                                    ECS019
01059      ADD 1                       TO  X2.                          ECS019
01060      IF X-POINTER (X2) = HIGH-VALUE                               ECS019
01061          GO TO 0550-OUT-AH.                                       ECS019
01062                                                                   ECS019
01063      MOVE X-AMTS (X1 X2)         TO  X-DETL.                      ECS019
01064      IF X-ZERO-5 = X-DETL                                         ECS019
01065          GO TO 0540-LOOP-AH.                                      ECS019
01066                                                                   ECS019
01067      IF PRT-SW = 0                                                ECS019
01068          MOVE 1                  TO  PRT-SW                       ECS019
01069          MOVE 1                  TO  AH-SW                        ECS019
01070          MOVE HD4                TO  P-LN                         ECS019
01071          PERFORM 0770-PRT-RTN THRU 0790-PRT-XIT                   ECS019
01072          MOVE HD5                TO  P-LN                         ECS019
01073          PERFORM 0770-PRT-RTN THRU 0790-PRT-XIT                   ECS019
01074          MOVE SPACE-2            TO  P-CCSW.                      ECS019
01075                                                                   ECS019
01076      IF AH-SW = 0                                                 ECS019
01077          MOVE 1                  TO  AH-SW.                       ECS019
01078                                                                   ECS019
01079      MOVE X-POINTER (X2)         TO  WX-POINTER.                  ECS019
01080      MOVE X-DESC (X2)            TO  WX-DESC.                     ECS019
01081                                                                   ECS019
01082      IF WX-OB = '1'                                               ECS019
01083          MOVE '1YR'              TO  WX-DESC-OB.                  ECS019
01084      IF WX-OB = '2'                                               ECS019
01085          MOVE 'REN'              TO  WX-DESC-OB.                  ECS019
01086                                                                   ECS019
01087      ADD X-ISSUE                 TO  S-ISSUE.                     ECS019
01088      ADD X-CANCL                 TO  S-CANCL.                     ECS019
01089      ADD X-BASE                  TO  S-BASE.                      ECS019
01090      ADD X-OVER                  TO  S-OVER.                      ECS019
040504     ADD X-DLR-INC               TO  S-DLR-INC
011410     ADD X-LF-LMBA-FEE           TO  S-LF-LMBA-FEE
011410     ADD X-AH-LMBA-FEE           TO  S-AH-LMBA-FEE
           ADD X-BANK-FEE              TO  S-BANK-FEE
           ADD X-CSO-ADMIN             TO  S-CSO-ADMIN
01091      ADD X-CLAIM                 TO  S-CLAIM.                     ECS019
01092      PERFORM 0580-PRT-LINE THRU 0590-PRT-LINE-XIT.                ECS019
01093      GO TO 0540-LOOP-AH.                                          ECS019
01094                                                                   ECS019
01095  0550-OUT-AH.                                                     ECS019
01096      IF AH-SW = 0                                                 ECS019
01097          GO TO 0570-PRT-EXTRACT-XIT.                              ECS019
01098                                                                   ECS019
01099      MOVE SPACES                 TO  BLD-DESC.                    ECS019
01100      MOVE 'TOTAL '               TO  BLD-TOTAL.                   ECS019
01101      MOVE AH-OVERRIDE-L2         TO  BLD-DESC1.                   ECS019
01102      MOVE SPACES                 TO  B-SLASH1                     ECS019
01103                                      BLD-DESC2.                   ECS019
01104      MOVE BLD-DESC               TO  WX-DESC.                     ECS019
01105      MOVE SPACES                 TO  WX-POINTER.                  ECS019
01106      MOVE SUB-TOTALS             TO  X-DETL.                      ECS019
01107      PERFORM 0580-PRT-LINE THRU 0590-PRT-LINE-XIT.                ECS019
01108      MOVE SPACE-2                TO  P-CCSW.                      ECS019
01109      ADD S-ISSUE                 TO  T-ISSUE.                     ECS019
01110      ADD S-CANCL                 TO  T-CANCL.                     ECS019
01111      ADD S-BASE                  TO  T-BASE.                      ECS019
01112      ADD S-OVER                  TO  T-OVER.                      ECS019
040504     ADD S-DLR-INC               TO  T-DLR-INC
011410     ADD S-LF-LMBA-FEE           TO  T-LF-LMBA-FEE
011410     ADD S-AH-LMBA-FEE           TO  T-AH-LMBA-FEE
           ADD S-BANK-FEE              TO  T-BANK-FEE
           ADD S-CSO-ADMIN             TO  T-CSO-ADMIN
01113      ADD S-CLAIM                 TO  T-CLAIM.                     ECS019
01114                                                                   ECS019
01115  0560-PRT-TOTALS.                                                 ECS019
01116      IF LIFE-SW = 0                                               ECS019
01117          GO TO 0570-PRT-EXTRACT-XIT.                              ECS019
01118                                                                   ECS019
01119      MOVE SPACES                 TO  BLD-DESC.                    ECS019
01120      MOVE TOTALS                 TO  X-DETL.                      ECS019
01121      MOVE 'TOTAL '               TO  BLD-TOTAL.                   ECS019
01122      MOVE LIFE-OVERRIDE-L2       TO  BLD-DESC1.                   ECS019
01123      MOVE '/'                    TO  B-SLASH1.                    ECS019
01124      MOVE AH-OVERRIDE-L2         TO  BLD-DESC2.                   ECS019
01125      MOVE BLD-DESC               TO  WX-DESC.                     ECS019
01126      MOVE SPACES                 TO  WX-POINTER.                  ECS019
01127      PERFORM 0580-PRT-LINE THRU 0590-PRT-LINE-XIT.                ECS019
01128                                                                   ECS019
01129  0570-PRT-EXTRACT-XIT.                                            ECS019
01130      EXIT.                                                        ECS019
01131      EJECT                                                        ECS019
01132  0580-PRT-LINE.                                                   ECS019
01133      IF LNCTR GREATER THAN +058                                   ECS019
01134          PERFORM 0750-HD-RTN THRU 0760-HD-XIT                     ECS019
01135          MOVE HD4                TO  P-LN                         ECS019
01136          MOVE SPACE-3            TO  P-CCSW                       ECS019
01137          PERFORM 0770-PRT-RTN THRU 0790-PRT-XIT                   ECS019
01138          MOVE HD5                TO  P-LN                         ECS019
01139          PERFORM 0770-PRT-RTN THRU 0790-PRT-XIT                   ECS019
01140          MOVE SPACE-2            TO  P-CCSW.                      ECS019
01141                                                                   ECS019
01142      MOVE WX-TYP                 TO  P-TYPE.                      ECS019
01143      MOVE WX-DESC                TO  P-DESC.                      ECS019
01144      MOVE X-ISSUE                TO  P-ISSUE.                     ECS019
01145      MOVE X-CANCL                TO  P-CANCL.                     ECS019
01146      MOVE X-BASE                 TO  P-BASE.                      ECS019
01147      MOVE X-OVER                 TO  P-OVER.                      ECS019
011410     COMPUTE P-TOTAL = X-LF-LMBA-FEE + X-AH-LMBA-FEE +
011410        X-DLR-INC + X-BANK-FEE + X-CSO-ADMIN
      *    MOVE X-LMBA-FEE             TO  P-LMBA-FEE
040504*    MOVE X-DLR-INC              TO  P-DLR-INC
01148                                                                   ECS019
01149      IF SUB-HD2 = 'FINAL TOTALS' AND                              ECS019
01150          BLD-TOTAL = 'TOTAL ' AND                                 ECS019
01151          BLD-DESC1 = LIFE-OVERRIDE-L2 AND                         ECS019
01152          BLD-DESC2 = SPACES                                       ECS019
01153              MOVE X-ISSUE        TO  HLD-019-PREM-L               ECS019
01154              MULTIPLY X-CANCL BY -1 GIVING HLD-019-REF-L          ECS019
01155              MOVE X-BASE         TO  HLD-019-COMM-L               ECS019
070714             compute hld-019-or-l =
050115                x-over + x-cso-admin + x-dlr-inc + x-lf-lmba-fee
01156 *            MOVE X-OVER         TO  HLD-019-OR-L
070714             move x-claim        to  hld-019-clms-l
070714     end-if
01157                                                                   ECS019
01158      IF SUB-HD2 = 'FINAL TOTALS' AND                              ECS019
01159          BLD-TOTAL = 'TOTAL ' AND                                 ECS019
01160          BLD-DESC1 = AH-OVERRIDE-L2 AND                           ECS019
01161          BLD-DESC2 = SPACES                                       ECS019
01162              MOVE X-ISSUE        TO  HLD-019-PREM-AH              ECS019
01163              MULTIPLY -1 BY X-CANCL GIVING HLD-019-REF-AH         ECS019
01164              MOVE X-BASE         TO  HLD-019-COMM-AH              ECS019
070714             compute hld-019-or-ah =
050115                x-over + x-cso-admin + x-dlr-inc + x-ah-lmba-fee
01165 *            MOVE X-OVER         TO  HLD-019-OR-AH
070714             move x-claim        to  hld-019-clms-ah
070714     end-if
01166                                                                   ECS019
01167      MOVE X-CLAIM                TO  P-CLAIM.                     ECS019
01168      ADD X-ISSUE X-CANCL GIVING X-NET.                            ECS019
01169      ADD X-BASE X-OVER GIVING X-TOTAL.                            ECS019
01170      MOVE X-NET                  TO  P-NET.                       ECS019
01171 *    MOVE X-TOTAL                TO  P-TOTAL.                     ECS019
01172      PERFORM 0770-PRT-RTN THRU 0790-PRT-XIT.                      ECS019
01173                                                                   ECS019
01174  0590-PRT-LINE-XIT.                                               ECS019
01175      EXIT.                                                        ECS019
01176      EJECT                                                        ECS019
01177  0600-ZERO-RTN.                                                   ECS019
01178      MOVE +0                     TO  X2 X-ISSUE X-CANCL           ECS019
011410                                     X-DLR-INC X-LF-LMBA-FEE
011410                                     X-AH-LMBA-FEE X-BANK-FEE
                                           X-CSO-ADMIN
01179                                      X-BASE  X-OVER  X-CLAIM.     ECS019
01180      MOVE X-DETL                 TO  X-ZERO-5.                    ECS019
01181                                                                   ECS019
01182  0610-ZERO-LOOP.                                                  ECS019
01183      ADD 1                       TO  X2.                          ECS019
092602     IF X2 GREATER THAN 900                                       ECS019
01185          GO TO 0620-ZERO-FILL.                                    ECS019
01186      MOVE X-ZERO-5               TO  X-AMTS (1 X2)                ECS019
01187                                      X-AMTS (2 X2).               ECS019
01188      GO TO 0610-ZERO-LOOP.                                        ECS019
01189                                                                   ECS019
01190  0620-ZERO-FILL.                                                  ECS019
01191      MOVE X-TOTALS               TO  X-ZERO.                      ECS019
01192                                                                   ECS019
01193  0630-ZERO-XIT.                                                   ECS019
01194      EXIT.                                                        ECS019
01195                                                                   ECS019
01196  0640-ROLL-RTN.                                                   ECS019
01197      MOVE 0                      TO  X2.                          ECS019
01198                                                                   ECS019
01199  0650-LOOP-X2.                                                    ECS019
01200      ADD 1                       TO  X2.                          ECS019
01201      IF X-POINTER (X2) = HIGH-VALUE                               ECS019
01202          GO TO 0680-ROLL-XIT.                                     ECS019
01203                                                                   ECS019
01204  0660-ROLL-INDIVIDUAL.                                            ECS019
01205      MOVE X-AMTS (1 X2)          TO  X-DETL.                      ECS019
01206      IF X-DETL = X-ZERO-5                                         ECS019
01207          GO TO 0670-ROLL-GROUP.                                   ECS019
01208                                                                   ECS019
01209      MOVE R-AMTS (1 X2)          TO  R-DETL.                      ECS019
01210      ADD X-ISSUE                 TO  R-ISSUE.                     ECS019
01211      ADD X-CANCL                 TO  R-CANCL.                     ECS019
01212      ADD X-BASE                  TO  R-BASE.                      ECS019
01213      ADD X-OVER                  TO  R-OVER.                      ECS019
040504     ADD X-DLR-INC               TO  R-DLR-INC
011410     ADD X-LF-LMBA-FEE           TO  R-LF-LMBA-FEE
011410     ADD X-AH-LMBA-FEE           TO  R-AH-LMBA-FEE
           ADD X-BANK-FEE              TO  R-BANK-FEE
           ADD X-CSO-ADMIN             TO  R-CSO-ADMIN
01214      ADD X-CLAIM                 TO  R-CLAIM.                     ECS019
01215      MOVE R-DETL                 TO  R-AMTS (1 X2).               ECS019
01216                                                                   ECS019
01217  0670-ROLL-GROUP.                                                 ECS019
01218      MOVE X-AMTS (2 X2)          TO  X-DETL.                      ECS019
01219      IF X-DETL = X-ZERO-5                                         ECS019
01220          GO TO 0650-LOOP-X2.                                      ECS019
01221                                                                   ECS019
01222      MOVE R-AMTS (2 X2)          TO  R-DETL.                      ECS019
01223      ADD X-ISSUE                 TO  R-ISSUE.                     ECS019
01224      ADD X-CANCL                 TO  R-CANCL.                     ECS019
01225      ADD X-BASE                  TO  R-BASE.                      ECS019
01226      ADD X-OVER                  TO  R-OVER.                      ECS019
040504     ADD X-DLR-INC               TO  R-DLR-INC
011410     ADD X-LF-LMBA-FEE           TO  R-LF-LMBA-FEE
011410     ADD X-AH-LMBA-FEE           TO  R-AH-LMBA-FEE
           ADD X-BANK-FEE              TO  R-BANK-FEE
           ADD X-CSO-ADMIN             TO  R-CSO-ADMIN
01227      ADD X-CLAIM                 TO  R-CLAIM.                     ECS019
01228      MOVE R-DETL                 TO  R-AMTS (2 X2).               ECS019
01229      GO TO 0650-LOOP-X2.                                          ECS019
01230                                                                   ECS019
01231  0680-ROLL-XIT.                                                   ECS019
01232      EXIT.                                                        ECS019
01233      EJECT                                                        ECS019
01234  0690-FORMAT-RTN.                                                 ECS019
01235      MOVE 0 TO X2.                                                ECS019
01236      MOVE CLAS-STARTL            TO  CLAS-INDEXL.                 ECS019
01237      MOVE CLAS-STARTA            TO  CLAS-INDEXA.                 ECS019
01238      MOVE HIGH-VALUE             TO  X-POINTERS.                  ECS019
01239      MOVE SPACES                 TO  X-DESCRIPTIONS.              ECS019
01240      IF CLAS-MAXL = ZEROES                                        ECS019
01241          GO TO 0710-FORMAT-AH-RTN.                                ECS019
01242                                                                   ECS019
01243  0700-FORMAT-LIFE.                                                ECS019
01244      IF CLAS-INDEXL GREATER THAN CLAS-MAXL                        ECS019
01245          GO TO 0710-FORMAT-AH-RTN.                                ECS019
01246                                                                   ECS019
01247      ADD 1                       TO  X2.                          ECS019
01248      MOVE CLAS-I-BEN (CLAS-INDEXL)   TO  X-TYP (X2).              ECS019
01249      MOVE 1                      TO  X-LAH (X2).                  ECS019
01250      MOVE SPACES                 TO  X-OB (X2).                   ECS019
01251      MOVE CLAS-I-AB10 (CLAS-INDEXL)  TO  X-DESC (X2).             ECS019
01252                                                                   ECS019
01253      IF CLAS-I-BAL (CLAS-INDEXL) NOT = 'B'                        ECS019
01254          ADD 1                   TO  CLAS-INDEXL                  ECS019
01255          GO TO 0700-FORMAT-LIFE.                                  ECS019
01256                                                                   ECS019
01257      MOVE '1'                    TO  X-OB (X2).                   ECS019
01258      MOVE X2                     TO  SAVE-X2.                     ECS019
01259      ADD 1                       TO  X2.                          ECS019
01260      MOVE X-POINTER (SAVE-X2)    TO  X-POINTER (X2).              ECS019
01261      MOVE X-DESC (SAVE-X2)       TO  X-DESC (X2).                 ECS019
01262      MOVE '2'                    TO  X-OB (X2).                   ECS019
01263      ADD 1                       TO  CLAS-INDEXL.                 ECS019
01264      GO TO 0700-FORMAT-LIFE.                                      ECS019
01265                                                                   ECS019
01266  0710-FORMAT-AH-RTN.                                              ECS019
01267      IF CLAS-MAXA = ZEROES                                        ECS019
01268          GO TO 0730-FORMAT-SET.                                   ECS019
01269                                                                   ECS019
01270  0720-FORMAT-AH.                                                  ECS019
01271      IF CLAS-INDEXA GREATER THAN CLAS-MAXA                        ECS019
01272          GO TO 0730-FORMAT-SET.                                   ECS019
01273                                                                   ECS019
01274      ADD 1                       TO  X2.                          ECS019
01275      MOVE CLAS-I-BEN (CLAS-INDEXA)   TO  X-TYP (X2).              ECS019
01276      MOVE 2                      TO  X-LAH (X2).                  ECS019
01277      MOVE SPACES                 TO  X-OB (X2).                   ECS019
01278      MOVE CLAS-I-AB10 (CLAS-INDEXA)  TO  X-DESC (X2).             ECS019
01279                                                                   ECS019
01280      IF CLAS-I-BAL (CLAS-INDEXA) NOT = 'B'                        ECS019
01281          ADD 1                   TO  CLAS-INDEXA                  ECS019
01282          GO TO 0720-FORMAT-AH.                                    ECS019
01283                                                                   ECS019
01284      MOVE '1'                    TO  X-OB (X2).                   ECS019
01285      MOVE X2                     TO  SAVE-X2.                     ECS019
01286      ADD 1                       TO  X2.                          ECS019
01287      MOVE X-POINTER (SAVE-X2)    TO  X-POINTER (X2).              ECS019
01288      MOVE '2'                    TO  X-OB (X2).                   ECS019
01289      MOVE X-DESC (SAVE-X2)       TO  X-DESC (X2).                 ECS019
01290      ADD 1                       TO  CLAS-INDEXA.                 ECS019
01291      GO TO 0720-FORMAT-AH.                                        ECS019
01292                                                                   ECS019
01293  0730-FORMAT-SET.                                                 ECS019
01294                                                                   ECS019
092602     IF X2 GREATER THAN 900                                       ECS019
01296          MOVE +0201                     TO  WS-RETURN-CODE        ECS019
01297          MOVE 'PROGRAM TABLE EXCEEDED'  TO  WS-ABEND-MESSAGE      ECS019
01298          GO TO ABEND-PGM.                                         ECS019
01299                                                                   ECS019
01300  0740-FORMAT-XIT.                                                 ECS019
01301      EXIT.                                                        ECS019
01302      EJECT                                                        ECS019
01303  0750-HD-RTN.                                                     ECS019
01304      ADD +1                      TO  PGCTR.                       ECS019
01305      MOVE PGCTR                  TO  HD-PAGE.                     ECS019
01306      MOVE HD1                    TO  P-LN.                        ECS019
01307      MOVE SPACE-N                TO  P-CCSW.                      ECS019
01308      PERFORM 0770-PRT-RTN THRU 0790-PRT-XIT.                      ECS019
01309      MOVE HD2                    TO  P-LN.                        ECS019
01310      PERFORM 0770-PRT-RTN THRU 0790-PRT-XIT.                      ECS019
01311      MOVE HD3                    TO  P-LN.                        ECS019
01312      PERFORM 0770-PRT-RTN THRU 0790-PRT-XIT.                      ECS019
01313      MOVE SAVE-NAME              TO  P-LN.                        ECS019
01314      PERFORM 0770-PRT-RTN THRU 0790-PRT-XIT.                      ECS019
01315      MOVE +5                     TO  LNCTR.                       ECS019
01316                                                                   ECS019
01317  0760-HD-XIT.                                                     ECS019
01318      EXIT.                                                        ECS019
01319                                                                   ECS019
01320  0770-PRT-RTN.                                                    ECS019
01321      MOVE P-CCSW                 TO  X P-CTL.                     ECS019
01322      MOVE P-LN                   TO  P-DATA.                      ECS019
01323      MOVE SPACE-1                TO  P-REC.                       ECS019
01324                                                                   ECS019
01325      IF X = SPACE-1                                               ECS019
01326          ADD 1                   TO  LNCTR                        ECS019
01327      ELSE                                                         ECS019
01328          IF X = SPACE-2                                           ECS019
01329              ADD 2               TO  LNCTR                        ECS019
01330          ELSE                                                     ECS019
01331              IF X = SPACE-3                                       ECS019
01332                  ADD 3           TO  LNCTR.                       ECS019
01333                                                                   ECS019
01334  0780-PRT-COPY.                                                   ECS019
CIDMOD                                 COPY PRTN019.                    ECS019
CIDMOD*                                COPY ELCPRT2.                    ECS019
01336  0790-PRT-XIT.                                                    ECS019
01337      EXIT.                                                        ECS019
01338      EJECT                                                        ECS019
01339  0800-END-OUTPUT.                                                 ECS019
01340      IF FST-SW = +0                                               ECS019
01341          PERFORM 0750-HD-RTN THRU 0760-HD-XIT                     ECS019
01342          MOVE 'NO PREMIUM OR COMMISSION TRANSACTIONS' TO  P-LN    ECS019
01343          MOVE SPACE-2            TO  P-CCSW                       ECS019
01344          PERFORM 0770-PRT-RTN THRU 0790-PRT-XIT                   ECS019
01345      ELSE
102501         MOVE SPACES             TO  W-SEQ
01347          MOVE HIGH-VALUES        TO  SRT-REPORT-CD-1              ECS019
01348          MOVE 2                  TO  FST-SW                       ECS019
01349          PERFORM 0230-BREAK-RTN THRU 0410-BREAK-XIT.              ECS019
01350                                                                   ECS019
01351      CLOSE ERACCTT.                                               ECS019
01352                                                                   ECS019
01353  0810-OUTPUT-XIT.                                                 ECS019
01354      EXIT.                                                        ECS019
01355                                                                   ECS019
01356  0820-END-OF-JOB SECTION.                                         ECS019
01357                                                                   ECS019
01358   0830-EOJ-RTN.                                                   ECS019
01359                                  COPY ELCPRTC.                    ECS019
01360      CLOSE PRNTR.                                                 ECS019
01361                                                                   ECS019
01362      OPEN I-O ERMEBL.                                             ECS019
01363                                                                   ECS019
01364      IF ERMEBL-FILE-STATUS NOT = ZERO AND '97'                    ECS019
01365          MOVE 'N'                TO  ME-UPDATE-FLAG.              ECS019
01366                                                                   ECS019
01367      MOVE DTE-CLIENT             TO  ME-COMPANY.                  ECS019
01368      COMPUTE MONTH-END-MOYR = RUN-CCYY * 12 + RUN-MO.             ECS019
01369      MOVE MONTH-END-MOYR         TO  ME-MOYR.                     ECS019
01370                                                                   ECS019
01371      IF ME-DO-UPDATE                                              ECS019
01372          READ ERMEBL INVALID KEY                                  ECS019
01373          MOVE 'N'                TO  ME-UPDATE-FLAG               ECS019
01374          CLOSE ERMEBL.                                            ECS019
01375                                                                   ECS019
01376      IF ME-DO-UPDATE                                              ECS019
01377          MOVE HLD-019-PREM-L     TO  ME-019-PREM-L                ECS019
01378          MOVE HLD-019-PREM-AH    TO  ME-019-PREM-AH               ECS019
01379          MOVE HLD-019-REF-L      TO  ME-019-REF-L                 ECS019
01380          MOVE HLD-019-REF-AH     TO  ME-019-REF-AH                ECS019
01381          MOVE HLD-019-COMM-L     TO  ME-019-COMM-L                ECS019
01382          MOVE HLD-019-COMM-AH    TO  ME-019-COMM-AH               ECS019
01383          MOVE HLD-019-OR-L       TO  ME-019-OR-L                  ECS019
01384          MOVE HLD-019-OR-AH      TO  ME-019-OR-AH                 ECS019
070714         MOVE HLD-019-CLMS-L     TO  ME-019-CLMS-L
070714         MOVE HLD-019-CLMS-AH    TO  ME-019-CLMS-AH
01385 *        MOVE ME-START-TIME      TO  ME-019-START                 ECS019
01386          MOVE ME-CNDS-DATE       TO  ME-019-RUN-DT                ECS019
01387          ACCEPT WS-TIME-OF-DAY   FROM TIME                        ECS019
01388 *        MOVE WS-TIME            TO  ME-019-END                   ECS019
01389          ADD 1                   TO  ME-019-RUN-CT.               ECS019
01390                                                                   ECS019
01391      IF ME-DO-UPDATE                                              ECS019
01392          REWRITE MONTH-END-BALANCES                               ECS019
01393          CLOSE ERMEBL.                                            ECS019
01394                                                                   ECS019
01395      GOBACK.                                                      ECS019
01396                                                                   ECS019
01397                                                                   ECS019
01398  ABEND-PGM.                                                       ECS019
01399                                  COPY ELCABEND SUPPRESS.          ECS019
01400 /                                                                 ECS019
01401  LCP-WRITE-POS-PRT SECTION.                                       ECS019
01402      IF LCP-ASA = '+'                                             ECS019
01403          WRITE PRT AFTER 0 LINE                                   ECS019
01404      ELSE                                                         ECS019
01405      IF LCP-ASA = ' '                                             ECS019
01406          WRITE PRT AFTER ADVANCING 1 LINE                         ECS019
01407      ELSE                                                         ECS019
01408      IF LCP-ASA = '0'                                             ECS019
01409          WRITE PRT AFTER ADVANCING 2 LINE                         ECS019
01410      ELSE                                                         ECS019
01411      IF LCP-ASA = '-'                                             ECS019
01412          WRITE PRT AFTER ADVANCING 3 LINE                         ECS019
01413      ELSE                                                         ECS019
01414      IF LCP-ASA = '1'                                             ECS019
01415          WRITE PRT AFTER ADVANCING PAGE                           ECS019
01416      ELSE                                                         ECS019
01417      IF LCP-ASA = '2'                                             ECS019
01418          WRITE PRT AFTER ADVANCING LCP-CH2                        ECS019
01419      ELSE                                                         ECS019
01420      IF LCP-ASA = '3'                                             ECS019
01421          WRITE PRT AFTER ADVANCING LCP-CH3                        ECS019
01422      ELSE                                                         ECS019
01423      IF LCP-ASA = '4'                                             ECS019
01424          WRITE PRT AFTER ADVANCING LCP-CH4                        ECS019
01425      ELSE                                                         ECS019
01426      IF LCP-ASA = '5'                                             ECS019
01427          WRITE PRT AFTER ADVANCING LCP-CH5                        ECS019
01428      ELSE                                                         ECS019
01429      IF LCP-ASA = '6'                                             ECS019
01430          WRITE PRT AFTER ADVANCING LCP-CH6                        ECS019
01431      ELSE                                                         ECS019
01432      IF LCP-ASA = '7'                                             ECS019
01433          WRITE PRT AFTER ADVANCING LCP-CH7                        ECS019
01434      ELSE                                                         ECS019
01435      IF LCP-ASA = '8'                                             ECS019
01436          WRITE PRT AFTER ADVANCING LCP-CH8                        ECS019
01437      ELSE                                                         ECS019
01438      IF LCP-ASA = '9'                                             ECS019
01439          WRITE PRT AFTER ADVANCING LCP-CH9                        ECS019
01440      ELSE                                                         ECS019
01441      IF LCP-ASA = 'A'                                             ECS019
01442          WRITE PRT AFTER ADVANCING LCP-CH10                       ECS019
01443      ELSE                                                         ECS019
01444      IF LCP-ASA = 'B'                                             ECS019
01445          WRITE PRT AFTER ADVANCING LCP-CH11                       ECS019
01446      ELSE                                                         ECS019
01447      IF LCP-ASA = 'C'                                             ECS019
01448          WRITE PRT AFTER ADVANCING LCP-CH12                       ECS019
01449      ELSE                                                         ECS019
01450      IF LCP-ASA = 'V'                                             ECS019
01451          WRITE PRT AFTER ADVANCING LCP-P01                        ECS019
01452      ELSE                                                         ECS019
01453      IF LCP-ASA = 'W'                                             ECS019
01454          WRITE PRT AFTER ADVANCING LCP-P02                        ECS019
01455      ELSE                                                         ECS019
01456      DISPLAY 'ASA CODE ERROR'.                                    ECS019
01457  LCP-WRITE-END-PRT.                                               ECS019
01458      EXIT.                                                        ECS019
