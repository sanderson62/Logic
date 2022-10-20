00001  IDENTIFICATION DIVISION.                                         04/18/98
00002                                                                   EL523
00003  PROGRAM-ID.                 EL523 .                                 LV013
00004 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            EL523
00005 *                            VMOD=2.020.                          EL523
00006 *                                                                 EL523
CIDMOD* THERE ARE CID MODS IN THIS PROGRAM - 'EL523'.                   EL523
CIDMOD*                                                                 EL523
00007 *AUTHOR.        LOGIC, INC.                                       EL523
00008 *               DALLAS, TEXAS.                                    EL523
00009                                                                   EL523
00010 *DATE-COMPILED.                                                   EL523
00011                                                                   EL523
00012 *SECURITY.   *****************************************************EL523
00013 *            *                                                   *EL523
00014 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *EL523
00015 *            *                                                   *EL523
00016 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL523
00017 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL523
00018 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC, INC.     *EL523
00019 *            *                                                   *EL523
00020 *            *****************************************************EL523
00021                                                                   EL523
00022 *REMARKS.                                                         EL523
00023 *        LIST OF ALL ACCOUNTS WITH OR WITHOUT BUSINESS -          EL523
00024 *        REPORTING NET PREMIUMS BY LIFE, A&H, TOTAL AND           EL523
00025 *        COMMISSIONS WITH BREAKS FOR STATE, CARRIER,              EL523
00026 *        COMPANY AND ACCOUNT.                                     EL523
00027 *                                                                 EL523
00028 *  CLIENT MON BREAKS ON CARRIER AND REPORT CODE 2 ONLY ---        EL523
00029 *                                                                 EL523
00030 *        ALSO TOTALS NON-PROCESSABLE BUSINESS FOR CLIENT          EL523
00031 *        AND FLAGS ACCOUNTS WITH NO BUSINESS SUBMITTED.           EL523
122002******************************************************************
122002*                   C H A N G E   L O G
122002*
122002* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
122002*-----------------------------------------------------------------
122002*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
122002* EFFECTIVE    NUMBER
122002*-----------------------------------------------------------------
122002* 122002                   PEMA  ADD MONTHLY PRODUCT PROCESSING  
100703* 100703                   PEMA  ADD SUPER GAP PROCESSING
062104* 062104    2004050700001  SMVA  ADD NEW FILE TO AUTOMATE ME BALANCING
102004* 102004    2003031400002  PEMA  ADD NEW STATUS CODE
092705* 092705  CR2005050300006  PEMA  ADD SPP LEASES
022808* 022808  CR2007083100002  PEMA  ADD 'F' ACCT STATUS
031811* 031811  CR2011012700001  PEMA  ADD ACCT STATUS S - SUSPENDED
091911* 091911  IR2011090200003  PEMA  FIX LF PREM PROB WITH SPP
030612* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
060112* 060112                   PEMA  add - in balance amounts
060613* 060613  IR2013060600002  PEMA  include d and v for canc.
070714* 070714  CR2013060600001  PEMA  AUTOMATE MONTH END BALANCING
010716* 010716  CR2015082500001  PEMA  VPP CHANGES
021916* 021916  CR2014010900001  TANA  ADD ACCT STATUS D,L,R,P
062121* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
122002******************************************************************
00032  EJECT                                                            EL523
00033  ENVIRONMENT DIVISION.                                            EL523
00034  INPUT-OUTPUT SECTION.                                            EL523
00035  FILE-CONTROL.                                                    EL523
00036                                                                   EL523
00037      SELECT SORT1-FILE       ASSIGN TO SYS001-UT-FBA1-S-SORTWK1.  EL523
00038      SELECT SORT2-FILE       ASSIGN TO SYS001-UT-FBA1-S-SORTWK1.  EL523
00039      SELECT PRNTR            ASSIGN TO SYS008-UR-1403-S-SYS008.   EL523
00040      SELECT ERACCT           ASSIGN TO SYS015-FBA1-ERACCT         EL523
00041                              ORGANIZATION IS INDEXED              EL523
00042                              ACCESS IS DYNAMIC                    EL523
00043                              RECORD KEY IS AM-CONTROL-PRIMARY     EL523
00044                              ALTERNATE RECORD KEY IS              EL523
00045                                      AM-CONTROL-BY-VAR-GRP        EL523
00046                              FILE STATUS IS ERACCT-FILE-STATUS.   EL523
00047      SELECT ACCT-XTRACT      ASSIGN TO SYS017-UT-FBA1-S-SYS017.   EL523
062104     SELECT ME-EL523-BALANCE ASSIGN TO SYS011
062104                             ORGANIZATION IS LINE SEQUENTIAL.

00048      SELECT ELREPT           ASSIGN TO SYS018-FBA1-ELREPT         EL523
00049                              ORGANIZATION IS INDEXED              EL523
00050                              ACCESS IS DYNAMIC                    EL523
00051                              RECORD KEY IS RF-CONTROL-PRIMARY     EL523
00052                              FILE STATUS IS DTE-VSAM-FLAGS.       EL523
00053      SELECT DISK-DATE        ASSIGN TO SYS019-FBA1-S-SYS019.      EL523
00054      SELECT FICH             ASSIGN TO SYS020-UT-2400-S-SYS020.   EL523
00055      SELECT ERPNDB           ASSIGN TO SYS022-FBA1-ERPNDB         EL523
00056                              ORGANIZATION IS INDEXED              EL523
00057                              ACCESS IS DYNAMIC                    EL523
00058                              RECORD KEY IS PB-CONTROL-PRIMARY     EL523
00059                              FILE STATUS IS ERPNDB-FILE-STATUS.   EL523
070714     SELECT  ERMEBL          ASSIGN SYS024-FBA1-ERMEBL      
070714                             ORGANIZATION INDEXED           
070714                             ACCESS DYNAMIC                 
070714                             RECORD KEY ME-CONTROL-PRIMARY  
070714                             FILE STATUS ERMEBL-FILE-STATUS.
00060  EJECT                                                            EL523
00061  DATA DIVISION.                                                   EL523
00062  FILE SECTION.                                                    EL523
00063                                                                   EL523
00064  SD  SORT1-FILE.                                                  EL523
00065                                                                   EL523
00066  01  SORT-REC1.                                                   EL523
00067      12  SORT-KEY1           PIC X(30).                           EL523
00068      12  FILLER              PIC X(241).                          EL523
00069                                                                   EL523
00070  SD  SORT2-FILE.                                                  EL523
00071                                                                   EL523
00072  01  SORT-REC2.                                                   EL523
00073      12  SORT-KEY2           PIC X(29).                           EL523
00074      12  FILLER              PIC X(345).                          EL523
00075  EJECT                                                            EL523
00076  FD  PRNTR                                                        EL523
00077                              COPY ELCPRTFD.                       EL523
00078  EJECT                                                            EL523
00079  FD  ERACCT.                                                      EL523
00080                              COPY ERCACCT.                        EL523
00081  EJECT                                                            EL523
00082  FD  ACCT-XTRACT                                                  EL523
00083      BLOCK CONTAINS 0 RECORDS
00084      RECORDING MODE F.                                            EL523
00085                                                                   EL523
00086  01  ACCT-X-REC              PIC X(374).                          EL523

062104 FD  ME-EL523-BALANCE
062104     RECORDING MODE IS F
062104     BLOCK CONTAINS 0 RECORDS.
062104 01  ME-EL523-BALANCE-REC    PIC X(83).

00087  EJECT                                                            EL523
00088  FD  ELREPT.                                                      EL523
00089                              COPY ELCREPT.                        EL523
00090  EJECT                                                            EL523
00091  FD  DISK-DATE                                                    EL523
00092                              COPY ELCDTEFD.                       EL523
00093  EJECT                                                            EL523
00094  FD  FICH                                                         EL523
00095                              COPY ELCFCHFD.                       EL523
00096  EJECT                                                            EL523
00097  FD  ERPNDB.                                                      EL523
00098                              COPY ERCPNDB.                        EL523
070714 FD  ERMEBL.
070714                                 COPY ERCMEBL.
00099  EJECT                                                            EL523
00100  WORKING-STORAGE SECTION.                                         EL523
00101  77  FILLER  PIC X(32)   VALUE '********************************'.EL523
00102  77  FILLER  PIC X(32)   VALUE '*     EL523  WORKING STORAGE   *'.EL523
00103  77  FILLER  PIC X(32)   VALUE '**********VMOD=2.020 ***********'.EL523
100703 77  CNC-FACT                PIC S9(3)V9(7) COMP-3 VALUE +0.
00104                                                                   EL523
CIDMOD 01  PRINT-SW                    PIC X           VALUE 'N'.       EL523
CIDMOD                                                                  EL523
00105  01  FILLER          COMP-3.                                      EL523
00106      12  WS-ELAPSED-MONTHS       PIC S9(05)      VALUE +0.        EL523
00107      12  WS-LINE-COUNT           PIC S9(3)       VALUE +99.       EL523
00108      12  WS-LINE-COUNT-MAX       PIC S9(3)       VALUE +60.       EL523
00109      12  WS-LINE-COUNT-TST       PIC S9(3)       VALUE +60.       EL523
00110      12  WS-PAGE                 PIC S9(5)       VALUE ZERO.      EL523
00111      12  WS-REPORT-SW            PIC S9          VALUE +1.        EL523
00112      12  WS-RECORD-COUNT         PIC S9(9)       VALUE ZERO.      EL523
00113      12  WS-RETURN-CODE          PIC S9(3)       VALUE ZERO.      EL523
00114      12  WS-ZERO                 PIC S9          VALUE ZERO.      EL523
00115      12  WS-MONTHS-WORK          PIC S9(3)       VALUE ZERO.      EL523
00116      12  WS-MONTH-END            PIC S9          VALUE ZERO.      EL523
00117          88  THIS-IS-NOT-MONTH-END               VALUE ZERO.      EL523
00118          88  THIS-IS-MONTH-END                   VALUE +1.        EL523
00119      12  WS-RECORDS-RELEASED     PIC S9(7)       VALUE ZERO.      EL523
00120      12  WS-RECORDS-RETURNED     PIC S9(7)       VALUE ZERO.      EL523
00121                                                                   EL523
00122  01  FILLER      COMP    SYNC.                                    EL523
00123      12  S1                      PIC S9(4)       VALUE +00.       EL523
00124      12  PGM-SUB                 PIC S9(4)       VALUE +523.      EL523
00125      12  WS-INDEX                PIC S9(4)       VALUE ZERO.      EL523
00126      12  WS-LENGTH  REDEFINES                                     EL523
00127          WS-INDEX                PIC S9(4).                       EL523
00128      12  WS-BIN-DATE-WORK-X.                                      EL523
00129          16  WS-BIN-DATE-WORK    PIC S9(4).                       EL523
00130                                                                   EL523
00131  01  FILLER.                                                      EL523
100703     12  WS-AH-CATEGORY              PIC X       VALUE ' '.
100703     12  CLASS-LOOK                  PIC XX      VALUE '  '.
00132      12  WS-AM-LO-DT                 PIC 9(11)   VALUE 0.            CL**3
00133      12  WS-AM-HI-DT                 PIC 9(11)   VALUE 0.            CL**3
00134      12  WS-SAVE-RPT2                PIC X(10)   VALUE SPACES.    EL523
00135      12  X                           PIC X.                       EL523
00136      12  SPACE-NP                    PIC X       VALUE '1'.       EL523
00137      12  SPACE-1                     PIC X       VALUE ' '.       EL523
00138      12  SPACE-2                     PIC X       VALUE '0'.       EL523
00139      12  SPACE-3                     PIC X       VALUE '-'.       EL523
00140      12  HEADERS-PRINTED-SW          PIC X.                       EL523
00141          88  HEADER-PRINTED                      VALUE 'Y'.       EL523
00142          88  HEADER-NOT-PRINTED                  VALUE 'N'.       EL523
00143      12  CARRIER-TOTAL-SW            PIC X       VALUE 'Y'.       EL523
00144          88  PRT-CARRIER-TOTAL                   VALUE 'Y'.       EL523
00145      12  GROUP-TOTAL-SW              PIC X       VALUE 'Y'.       EL523
00146          88  PRT-GROUP-TOTAL                     VALUE 'Y'.       EL523
00147      12  STATE-TOTAL-SW              PIC X       VALUE 'Y'.       EL523
00148          88  PRT-STATE-TOTAL                     VALUE 'Y'.       EL523
00149      12  FIRST-ACCT-SW               PIC X       VALUE 'Y'.       EL523
00150          88  FIRST-ACCT                          VALUE 'Y'.       EL523
00151      12  FIRST-SW                    PIC X       VALUE '0'.       EL523
00152          88  FIRST-TIME                          VALUE '0'.       EL523
00153          88  LAST-TIME                           VALUE '2'.       EL523
00154      12  FIRST-RETURN-SW             PIC X       VALUE 'Y'.       EL523
00155          88  FIRST-RETURN                        VALUE 'Y'.       EL523
00156      12  DATES-SW                    PIC X       VALUE '0'.       EL523
00157          88  NO-DATES                            VALUE '0'.       EL523
00158      12  PEND-SW                     PIC X       VALUE '0'.       EL523
00159          88  GOOD-PEND                           VALUE '0'.       EL523
00160      12  FIRST-DAT-SW                PIC X       VALUE '0'.       EL523
00161          88  FIRST-DAT                           VALUE '0'.       EL523
00162      12  BREAK-SWITCH                PIC X       VALUE '0'.       EL523
00163          88  CARR-BREAK                          VALUE '1'.       EL523
00164          88  COMP-BREAK                          VALUE '2'.       EL523
00165      12  ZERO-3-4-SWITCH             PIC X       VALUE 'N'.       EL523
00166          88  ZERO-3-4-ON                         VALUE 'Y'.       EL523
00167      12  ACCT-REC-FOUND-SW           PIC X       VALUE 'Y'.       EL523
00168          88  ACCT-REC-FOUND                      VALUE 'Y'.       EL523
00169      12  PREV-CARRIER                PIC X.                       EL523
00170      12  ABEND-OPTION                PIC X.                       EL523
00171      12  WS-LAST-COMPANY-CD          PIC X       VALUE LOW-VALUE. EL523
00172      12  WS-LAST-CARRIER             PIC X       VALUE LOW-VALUE. EL523
00173      12  WS-COMPANY-CD               PIC X.                       EL523
00174      12  WS-INITIALS.                                             EL523
00175          16  WS-INITIAL1             PIC X.                       EL523
00176          16  WS-INITIAL2             PIC X.                       EL523
00177      12  WS-RUN-DATE-BIN             PIC XX      VALUE LOW-VALUE. EL523
00178      12  WS-SAVE-LAST-DT             PIC XX      VALUE LOW-VALUE. EL523
00179      12  WS-ABEND-FILE-STATUS        PIC XX      VALUE ZERO.      EL523
00180      12  ELCNTL-FILE-STATUS          PIC XX      VALUE ZERO.      EL523
00181      12  ERACCT-FILE-STATUS          PIC XX      VALUE ZERO.      EL523
00182      12  ERPNDB-FILE-STATUS          PIC XX      VALUE ZERO.      EL523
00183      12  WS-MONTH-END-DATE           PIC XX      VALUE LOW-VALUE. EL523
00184      12  PREV-STATE                  PIC XX.                      EL523
00185      12  SAV-OLD-ST-NO               PIC XX      VALUE SPACES.    EL523
00186      12  SAV-NEW-ST-NO               PIC XX      VALUE SPACES.    EL523
00187      12  PREV-GROUP                  PIC X(6).                    EL523
00188      12  PREV-RPT2                   PIC X(10).                   EL523
00189      12  SAV-OLD-GROUP               PIC X(6)    VALUE SPACES.    EL523
00190      12  SAV-OLD-RPT2                PIC X(10)   VALUE SPACES.    EL523
00191      12  SAV-NEW-GROUP               PIC X(6)    VALUE SPACES.    EL523
00192      12  SAV-NEW-RPT2                PIC X(10)   VALUE SPACES.    EL523
00193      12  WS-COMPANY-ID               PIC XXX.                     EL523
00194      12  ABEND-CODE                  PIC X(4).                    EL523
00195      12  WS-DEL-BIN1                 PIC 9999-.                   EL523
00196      12  WS-WORK-DATE1               PIC 9(6).                    EL523
00197      12  WS-WORK-DATE1-X  REDEFINES  WS-WORK-DATE1.               EL523
00198          16  WS-WORK-MM1             PIC 99.                      EL523
00199          16  WS-WORK-DD1             PIC 99.                      EL523
00200          16  WS-WORK-YY1             PIC 99.                      EL523
00201      12  WS-WORK-DATE2               PIC 9(6).                    EL523
00202      12  WS-WORK-DATE2-X  REDEFINES  WS-WORK-DATE2.               EL523
00203          16  WS-WORK-MM2             PIC 99.                      EL523
00204          16  WS-WORK-DD2             PIC 99.                      EL523
00205          16  WS-WORK-YY2             PIC 99.                      EL523
00206      12  HOLD-LO-DATE                PIC 9(11)   COMP-3.             CL**8
00207      12  HOLD-HI-DATE                PIC 9(11)   COMP-3.             CL**8
00208      12  WS-DISPLAY-TIME             PIC 99B99B99.                EL523
00209      12  OLC-REPORT-NAME             PIC X(8)    VALUE 'EL523'.   EL523
00210      12  WS-PROGRAM-OPTIONS.                                      EL523
00211          16  WS-FREQUENCY            PIC X(4).                    EL523
00212          16  WS-PRINT-OPTION         PIC X.                       EL523
00213          16  WS-FORMAT-OPTION        PIC X.                       EL523
00214          16  WS-PROCESS-OPTION       PIC X.                       EL523
00215          16  WS-TOTAL-OPTION         PIC X.                       EL523
00216      12  WS-DISPLAY-COUNT            PIC Z,ZZZ,ZZ9-.              EL523
00217      12  ACCT-CONTROL.                                            EL523
00218          16  ACC-CAR-CNTRL           PIC X       VALUE SPACES.    EL523
00219          16  ACC-RPT2-CNTRL          PIC X(10)   VALUE SPACES.    EL523
00220          16  ACC-GRP-CNTRL           PIC X(6)    VALUE SPACES.    EL523
00221          16  ACC-ST-CNTRL            PIC XX      VALUE SPACES.    EL523
00222          16  ACC-ACCT-CNTRL          PIC X(10)   VALUE SPACES.    EL523
00223      12  SAVE-CONTROL.                                            EL523
00224          16  SAV-CAR-CNTRL           PIC X       VALUE SPACES.    EL523
00225          16  SAV-RPT2-CNTRL          PIC X(10)   VALUE SPACES.    EL523
00226          16  SAV-GRP-CNTRL           PIC X(6)    VALUE SPACES.    EL523
00227          16  SAV-ST-CNTRL            PIC XX      VALUE SPACES.    EL523
00228          16  SAV-ACCT-CNTRL          PIC X(10)   VALUE SPACES.    EL523
00229      12  WS-COUNT                    PIC ZZZ,ZZZ,ZZ9-.            EL523
00230      12  WS-DISPLAY-AMOUNT           PIC Z,ZZZ,ZZ9.99-.           EL523
00231      12  WS-AMOUNT                   PIC ZZZ,ZZZ,ZZ9.99-.         EL523
00232      12  WS-PHONETIC-WORK-AREA.                                   EL523
00233          16  WS-PWA-PHONETIC-NAME    PIC X(4).                    EL523
00234          16  WS-PWA-NAME             PIC X(16).                   EL523
00235          16  WS-PWA-LANGUAGE         PIC X.                       EL523
00236      12  WS-COMPANY-NAME.                                         EL523
00237          16  WS-CN-CHAR              PIC X                        EL523
00238                  OCCURS 30 TIMES     INDEXED BY CN1.              EL523
00239      12  WS-COMPANY-NAME2.                                        EL523
00240          16  WS-CN2-CHAR             PIC X                        EL523
00241                  OCCURS 30 TIMES     INDEXED BY CN2.              EL523
00242      12  WS-FILE-ERROR-MESSAGE.                                   EL523
00243          16  FILLER                  PIC X(24)   VALUE            EL523
00244                  'ERROR OCCURED OPENING - '.                      EL523
00245          16  WS-FEM-FILE-NAME        PIC X(8).                    EL523
00246      12  SAV-OLD-STATE               PIC X(36)   VALUE SPACES.    EL523
00247      12  SAV-NEW-STATE               PIC X(36)   VALUE SPACES.    EL523
00248      12  WS-ABEND-MESSAGE            PIC X(80)   VALUE SPACES.    EL523
00249      12  WS-SAVE-PRINT-RECORD        PIC X(133)  VALUE SPACES.    EL523
00250      12  WS-ACT-LO-DATE.                                             CL**6
00251          16  FILLER              PIC 999.                            CL**6
00252          16  ACT-LO-CC           PIC 99.                             CL**6
00253          16  ACT-LO-YR           PIC 99.                             CL**6
00254          16  ACT-LO-MO           PIC 99.                             CL**6
00255          16  ACT-LO-DA           PIC 99.                             CL**6
00256      12  WS-ACT-LO-DATE-N REDEFINES                                  CL**6
00257             WS-ACT-LO-DATE       PIC 9(11).                          CL**6
00258      12  WS-ACT-HI-DATE.                                             CL**6
00259          16  FILLER              PIC 999.                            CL**6
00260          16  ACT-HI-CC           PIC 99.                             CL**6
00261          16  ACT-HI-YR           PIC 99.                             CL**6
00262          16  ACT-HI-MO           PIC 99.                             CL**6
00263          16  ACT-HI-DA           PIC 99.                             CL**6
00264      12  WS-ACT-HI-DATE-N REDEFINES                                  CL**6
00265             WS-ACT-HI-DATE       PIC 9(11).                          CL**6
00266      12  WS-ACT-AM-HI-DT.                                            CL**9
00267          16  FILLER              PIC 999.                            CL**7
00268          16  ACT-AM-HI-DT-CYMD   PIC 9(08).                          CL**7
00269      12  WS-ACT-AM-HI-DT-N REDEFINES                                 CL**7
00270             WS-ACT-AM-HI-DT      PIC 9(11).                          CL**7
00271      12  WS-REPT-XTR-EFF-DATE.                                       CL**7
00272          16  FILLER                      PIC 999.                    CL**7
00273          16  REPT-XTR-EFF-CC             PIC 99.                     CL**7
00274          16  REPT-XTR-EFF-YR             PIC 99.                     CL**7
00275          16  REPT-XTR-EFF-MO             PIC 99.                     CL**7
00276          16  REPT-XTR-EFF-DA             PIC 99.                     CL**7
00277      12  WS-REPT-XTR-EFF-DATE-N  REDEFINES                           CL**7
00278             WS-REPT-XTR-EFF-DATE         PIC 9(11).                  CL**7
00279      12  WS-HOLD-LO-DATE.                                            CL**8
00280          16  FILLER                  PIC 999     VALUE ZEROS.        CL**8
00281          16  HOLD-LO-CC              PIC 99      VALUE ZEROS.        CL**8
00282          16  HOLD-LO-YR              PIC 99      VALUE ZEROS.        CL**8
00283          16  HOLD-LO-MO              PIC 99      VALUE ZEROS.        CL**8
00284          16  HOLD-LO-DA              PIC 99      VALUE ZEROS.        CL**8
00285      12  WS-HOLD-HI-DATE.                                            CL**8
00286          16  FILLER                  PIC 999     VALUE ZEROS.        CL**8
00287          16  HOLD-HI-CC              PIC 99      VALUE ZEROS.        CL**8
00288          16  HOLD-HI-YR              PIC 99      VALUE ZEROS.        CL**8
00289          16  HOLD-HI-MO              PIC 99      VALUE ZEROS.        CL**8
00290          16  HOLD-HI-DA              PIC 99      VALUE ZEROS.        CL**8
00291                                                                   EL523
      ****=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=***
      ****                                                           ***
      ****   Month end balancing work area                           ***
      ****                                                           ***
      ****=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=***

070714 01  MONTH-END-DATA.                                          
070714     12  ME-START-DATE.                                       
070714         16  ME-START-MO     PIC  99.                         
070714         16  FILLER          PIC  X.                          
070714         16  ME-START-DA     PIC  99.                         
070714         16  FILLER          PIC  X.                          
070714         16  ME-START-YR     PIC  99.                         
070714     12  ME-CNDS-DATE        PIC  9(6).                       
070714     12  ME-CNDS-DATE-R  REDEFINES  ME-CNDS-DATE.             
070714         16  ME-CNDS-MO      PIC  99.                         
070714         16  ME-CNDS-DA      PIC  99.                         
070714         16  ME-CNDS-YR      PIC  99.                         
070714     12  ME-START-TIME       PIC  9(6).                       
070714     12  ME-UPDATE-FLAG      PIC  X          VALUE 'Y'.       
070714         88  ME-DO-UPDATE                    VALUE 'Y'.       
070714         88  ME-NO-UPDATE                    VALUE 'N'.       
070714     12  ERMEBL-FILE-STATUS  PIC  XX.                         
070714     12  MONTH-END-MOYR      PIC  9(4)                 COMP.  
070714     12  hld-523-prem-tot    pic s9(9)v99 comp-3 value +0.

00292  01  REPORT-XTRACT.                                               EL523
00293      12  REPT-XTR-CONTROL.                                        EL523
00294          16  REPT-XTR-PREFIX             PIC X.                   EL523
00295          16  REPT-XTR-RPT2               PIC X(10).               EL523
00296          16  REPT-XTR-GROUP              PIC X(6).                EL523
00297          16  REPT-XTR-ST                 PIC XX.                  EL523
00298          16  REPT-XTR-ACCT               PIC X(10).               EL523
00299      12  REPT-XTR-REC-TYP                PIC X.                   EL523
00300          88  REPT-HEADER-RECORD                  VALUE '0'.       EL523
00301          88  REPT-DETAIL-RECORD                  VALUE '1'.       EL523
00302      12  REPT-XTR-EFF-DATE               PIC 9(11) COMP-3.           CL**7
00303      12  REPT-XTR-AM-EFF-DT              PIC XX.                  EL523
00304      12  REPT-XTR-AM-LO-DT        PIC 9(11)  COMP-3.                 CL**6
00305      12  REPT-XTR-AM-HI-DT        PIC 9(11)  COMP-3.                 CL**6
00306      12  REPT-GOOD-BUS                   PIC X.                   EL523
00307      12  REPT-BAD-BUS                    PIC X.                   EL523
00308      12  REPT-ACTIVE                     PIC X.                   EL523
00309      12  REPT-AMOUNT-FIELDS.                                      EL523
00310          16  REPT-XTR-GOOD-LF-AMT        PIC S9(9)V99 COMP-3.     EL523
00311          16  REPT-XTR-GOOD-LF-OB         PIC S9(9)V99 COMP-3.     EL523
00312          16  REPT-XTR-GOOD-LF-CAN        PIC S9(9)V99 COMP-3.     EL523
00313          16  REPT-XTR-GOOD-LF-COM        PIC S9(9)V99 COMP-3.     EL523
00314          16  REPT-XTR-GOOD-AH-AMT        PIC S9(9)V99 COMP-3.     EL523
00315          16  REPT-XTR-GOOD-AH-OB         PIC S9(9)V99 COMP-3.     EL523
00316          16  REPT-XTR-GOOD-AH-CAN        PIC S9(9)V99 COMP-3.     EL523
00317          16  REPT-XTR-GOOD-AH-COM        PIC S9(9)V99 COMP-3.     EL523
00318          16  REPT-XTR-BAD-LF-AMT         PIC S9(9)V99 COMP-3.     EL523
00319          16  REPT-XTR-BAD-LF-OB          PIC S9(9)V99 COMP-3.     EL523
00320          16  REPT-XTR-BAD-LF-CAN         PIC S9(9)V99 COMP-3.     EL523
00321          16  REPT-XTR-BAD-LF-COM         PIC S9(9)V99 COMP-3.     EL523
00322          16  REPT-XTR-BAD-AH-AMT         PIC S9(9)V99 COMP-3.     EL523
00323          16  REPT-XTR-BAD-AH-OB          PIC S9(9)V99 COMP-3.     EL523
00324          16  REPT-XTR-BAD-AH-CAN         PIC S9(9)V99 COMP-3.     EL523
00325          16  REPT-XTR-BAD-AH-COM         PIC S9(9)V99 COMP-3.     EL523
00326      12  REPT-COUNT-FIELDS.                                       EL523
00327          16  REPT-XTR-GOOD-SP-I-CNT      PIC S9(7)    COMP-3.     EL523
00328          16  REPT-XTR-GOOD-OB-I-CNT      PIC S9(7)    COMP-3.     EL523
00329          16  REPT-XTR-GOOD-LF-I-CNT      PIC S9(7)    COMP-3.     EL523
00330          16  REPT-XTR-GOOD-LF-OB-CNT     PIC S9(7)    COMP-3.     EL523
00331          16  REPT-XTR-GOOD-C-CNT         PIC S9(7)    COMP-3.     EL523
00332          16  REPT-XTR-GOOD-LF-C-CNT      PIC S9(7)    COMP-3.     EL523
00333          16  REPT-XTR-GOOD-AH-I-CNT      PIC S9(7)    COMP-3.     EL523
00334          16  REPT-XTR-GOOD-AH-OB-CNT     PIC S9(7)    COMP-3.     EL523
00335          16  REPT-XTR-GOOD-AH-C-CNT      PIC S9(7)    COMP-3.     EL523
00336          16  REPT-XTR-BAD-SP-I-CNT       PIC S9(7)    COMP-3.     EL523
00337          16  REPT-XTR-BAD-OB-I-CNT       PIC S9(7)    COMP-3.     EL523
00338          16  REPT-XTR-BAD-LF-I-CNT       PIC S9(7)    COMP-3.     EL523
00339          16  REPT-XTR-BAD-LF-OB-CNT      PIC S9(7)    COMP-3.     EL523
00340          16  REPT-XTR-BAD-C-CNT          PIC S9(7)    COMP-3.     EL523
00341          16  REPT-XTR-BAD-LF-C-CNT       PIC S9(7)    COMP-3.     EL523
00342          16  REPT-XTR-BAD-AH-I-CNT       PIC S9(7)    COMP-3.     EL523
00343          16  REPT-XTR-BAD-AH-OB-CNT      PIC S9(7)    COMP-3.     EL523
00344          16  REPT-XTR-BAD-AH-C-CNT       PIC S9(7)    COMP-3.     EL523
00345      12  REPT-XTR-NAME                   PIC X(30).               EL523
00346      12  REPT-XTR-TEL-NO.                                         EL523
00347          16  REPT-XTR-AREA-CODE          PIC 999.                 EL523
00348          16  REPT-XTR-TEL-PRE            PIC 999.                 EL523
00349          16  REPT-XTR-TEL-NBR            PIC 9(4).                EL523
00350      12  REPT-XTR-REPORT-CODE-2          PIC X(10).               EL523
00351                                                                   EL523
062104 01  WS-ME-BAL-GOOD-WRITTEN              PIC S9(09)V99 VALUE +0.
062104 01  WS-ME-BAL-BAD-WRITTEN               PIC S9(09)V99 VALUE +0.
062104 01  WS-ME-BAL-GOOD-OUTSTBAL             PIC S9(09)V99 VALUE +0.
062104 01  WS-ME-BAL-BAD-OUTSTBAL              PIC S9(09)V99 VALUE +0.
062104 01  WS-ME-BAL-TOT-OUTSTBAL              PIC S9(09)V99 VALUE +0.

062104 01  WS-BALANCE-DESCRIPTION1             PIC X(50)  VALUE
062104     'Processable Written & Outstanding Balance'.

062104 01  WS-BALANCE-DESCRIPTION2             PIC X(50)  VALUE
062104     'Non-Processable Written & Outstanding Balance'.

062104 01  WS-BALANCE-DESCRIPTION3             PIC X(50)  VALUE
062104     'Total Written & Outstanding Balance'.

062104 01  WS-BALANCE-DESCRIPTION4             PIC X(50)  VALUE
062104     'Processable Cancelled'.

062104 01  WS-BALANCE-DESCRIPTION5             PIC X(50)  VALUE
062104     'Non-Processable Cancelled'.

062104 01  WS-BALANCE-DESCRIPTION6             PIC X(50)  VALUE
062104     'Total Cancelled'.

062104 01  WS-BALANCE-DESCRIPTION7             PIC X(50)  VALUE
062104     'Processable Net Premium'.

062104 01  WS-BALANCE-DESCRIPTION8             PIC X(50)  VALUE
062104     'Non-Processable Net Premium'.

062104 01  WS-BALANCE-DESCRIPTION9             PIC X(50)  VALUE
062104     'Total Net Premium'.

062104 01  WS-BALANCE-DESCRIPTION10            PIC X(50)  VALUE
062104     'Processable Net Commission'.

062104 01  WS-BALANCE-DESCRIPTION11            PIC X(50)  VALUE
062104     'Non-Processable Net Commission'.

062104 01  WS-BALANCE-DESCRIPTION12            PIC X(50)  VALUE
062104     'Total Net Commission'.


062104 01  WS-ME-BALANCE-REC.
062104     12  WS-ME-BAL-JOB           PIC X(11)  VALUE SPACES.
062104     12  WS-ME-BAL-DELIM1        PIC X(01)  VALUE ';'.
062104     12  WS-ME-BAL-STEP          PIC X(08)  VALUE 'EL523   '.
062104     12  WS-ME-BAL-DELIM2        PIC X(01)  VALUE ';'.
060112     12  ws-me-bal-amt           pic -9(10).
062104*    12  WS-ME-BAL-AMT           PIC ZZZ,ZZZ,ZZ9.
062104     12  WS-ME-BAL-DELIM3        PIC X(01)  VALUE ';'.
062104     12  WS-ME-BAL-DESCRIP       PIC X(50)  VALUE SPACES.

00352  01  ACT-RECORD.                                                  EL523
00353      12  ACT-CONTROL.                                             EL523
00354          16  ACT-PREFIX          PIC X.                           EL523
00355          16  ACT-RPT2            PIC X(10).                       EL523
00356          16  ACT-GROUP           PIC X(6).                        EL523
00357          16  ACT-ST              PIC XX.                          EL523
00358          16  ACT-ACCT            PIC X(10).                       EL523
00359      12  ACT-NAME                PIC X(30).                       EL523
00360      12  ACT-EFFECTIVE-DT        PIC X(02).                       EL523
00361      12  ACT-TEL-NO.                                              EL523
00362          16  ACT-AREA-CODE       PIC 999.                         EL523
00363          16  ACT-TEL-PRE         PIC 999.                         EL523
00364          16  ACT-TEL-NBR         PIC 9(4).                        EL523
00365      12  ACT-DATE-FLAG           PIC X.                           EL523
00366          88  HAS-DATES                           VALUE '1'.       EL523
00367          88  HAS-NO-DATES                        VALUE '2'.       EL523
00368      12  ACT-LO-DATE             PIC 9(11)  COMP-3.                  CL**6
00369      12  ACT-HI-DATE             PIC 9(11)  COMP-3.                  CL**6
00370      12  ACT-AM-HI-DT            PIC 9(11)  COMP-3.                  CL**7
00371      12  ACT-AM-LO-DT            PIC 9(11)  COMP-3.                  CL**7
00372      12  ACT-BUSINESS-FLAG       PIC X.                           EL523
00373          88  HAS-BUSINESS                        VALUE '1'.       EL523
00374          88  HAS-NO-BUSINESS                     VALUE '2'.       EL523
00375      12  ACT-ACTIVE              PIC X.                           EL523
00376      12  ACT-TOT-FIELDS  COMP-3.                                  EL523
00377          16  ACT-GOOD-LF-AMT     PIC S9(9)V99    VALUE +0.        EL523
00378          16  ACT-GOOD-LF-OB      PIC S9(9)V99    VALUE +0.        EL523
00379          16  ACT-GOOD-LF-CAN     PIC S9(9)V99    VALUE +0.        EL523
00380          16  ACT-GOOD-LF-NET     PIC S9(9)V99    VALUE +0.        EL523
00381          16  ACT-GOOD-LF-COM     PIC S9(9)V99    VALUE +0.        EL523
00382          16  ACT-GOOD-AH-AMT     PIC S9(9)V99    VALUE +0.        EL523
00383          16  ACT-GOOD-AH-OB      PIC S9(9)V99    VALUE +0.        EL523
00384          16  ACT-GOOD-AH-CAN     PIC S9(9)V99    VALUE +0.        EL523
00385          16  ACT-GOOD-AH-NET     PIC S9(9)V99    VALUE +0.        EL523
00386          16  ACT-GOOD-AH-COM     PIC S9(9)V99    VALUE +0.        EL523
00387          16  ACT-BAD-LF-AMT      PIC S9(9)V99    VALUE +0.        EL523
00388          16  ACT-BAD-LF-OB       PIC S9(9)V99    VALUE +0.        EL523
00389          16  ACT-BAD-LF-CAN      PIC S9(9)V99    VALUE +0.        EL523
00390          16  ACT-BAD-LF-NET      PIC S9(9)V99    VALUE +0.        EL523
00391          16  ACT-BAD-LF-COM      PIC S9(9)V99    VALUE +0.        EL523
00392          16  ACT-BAD-AH-AMT      PIC S9(9)V99    VALUE +0.        EL523
00393          16  ACT-BAD-AH-OB       PIC S9(9)V99    VALUE +0.        EL523
00394          16  ACT-BAD-AH-CAN      PIC S9(9)V99    VALUE +0.        EL523
00395          16  ACT-BAD-AH-NET      PIC S9(9)V99    VALUE +0.        EL523
00396          16  ACT-BAD-AH-COM      PIC S9(9)V99    VALUE +0.        EL523
00397          16  ACT-ALL-LF-AMT      PIC S9(9)V99    VALUE +0.        EL523
00398          16  ACT-ALL-LF-OB       PIC S9(9)V99    VALUE +0.        EL523
00399          16  ACT-ALL-LF-CAN      PIC S9(9)V99    VALUE +0.        EL523
00400          16  ACT-ALL-LF-NET      PIC S9(9)V99    VALUE +0.        EL523
00401          16  ACT-ALL-LF-COM      PIC S9(9)V99    VALUE +0.        EL523
00402          16  ACT-ALL-AH-AMT      PIC S9(9)V99    VALUE +0.        EL523
00403          16  ACT-ALL-AH-OB       PIC S9(9)V99    VALUE +0.        EL523
00404          16  ACT-ALL-AH-CAN      PIC S9(9)V99    VALUE +0.        EL523
00405          16  ACT-ALL-AH-NET      PIC S9(9)V99    VALUE +0.        EL523
00406          16  ACT-ALL-AH-COM      PIC S9(9)V99    VALUE +0.        EL523
00407      12  ACT-CNT-FIELDS  COMP-3.                                  EL523
00408          16  ACT-GOOD-SP-I-CNT   PIC S9(7)       VALUE +0.        EL523
00409          16  ACT-GOOD-OB-I-CNT   PIC S9(7)       VALUE +0.        EL523
00410          16  ACT-GOOD-LF-I-CNT   PIC S9(7)       VALUE +0.        EL523
00411          16  ACT-GOOD-LF-OB-CNT  PIC S9(7)       VALUE +0.        EL523
00412          16  ACT-GOOD-C-CNT      PIC S9(7)       VALUE +0.        EL523
00413          16  ACT-GOOD-LF-C-CNT   PIC S9(7)       VALUE +0.        EL523
00414          16  ACT-GOOD-AH-I-CNT   PIC S9(7)       VALUE +0.        EL523
00415          16  ACT-GOOD-AH-OB-CNT  PIC S9(7)       VALUE +0.        EL523
00416          16  ACT-GOOD-AH-C-CNT   PIC S9(7)       VALUE +0.        EL523
00417          16  ACT-BAD-SP-I-CNT    PIC S9(7)       VALUE +0.        EL523
00418          16  ACT-BAD-OB-I-CNT    PIC S9(7)       VALUE +0.        EL523
00419          16  ACT-BAD-LF-I-CNT    PIC S9(7)       VALUE +0.        EL523
00420          16  ACT-BAD-LF-OB-CNT   PIC S9(7)       VALUE +0.        EL523
00421          16  ACT-BAD-C-CNT       PIC S9(7)       VALUE +0.        EL523
00422          16  ACT-BAD-LF-C-CNT    PIC S9(7)       VALUE +0.        EL523
00423          16  ACT-BAD-AH-I-CNT    PIC S9(7)       VALUE +0.        EL523
00424          16  ACT-BAD-AH-OB-CNT   PIC S9(7)       VALUE +0.        EL523
00425          16  ACT-BAD-AH-C-CNT    PIC S9(7)       VALUE +0.        EL523
00426          16  ACT-ALL-LF-I-CNT    PIC S9(7)       VALUE +0.        EL523
00427          16  ACT-ALL-LF-OB-CNT   PIC S9(7)       VALUE +0.        EL523
00428          16  ACT-ALL-LF-C-CNT    PIC S9(7)       VALUE +0.        EL523
00429          16  ACT-ALL-AH-I-CNT    PIC S9(7)       VALUE +0.        EL523
00430          16  ACT-ALL-AH-OB-CNT   PIC S9(7)       VALUE +0.        EL523
00431          16  ACT-ALL-AH-C-CNT    PIC S9(7)       VALUE +0.        EL523
00432  EJECT                                                            EL523
00433  01  REPORT-TOTAL-AREA   COMP-3.                                  EL523
00434      12  ST-LF-AMT-TOT-GOOD          PIC S9(9)V99    VALUE ZEROS. EL523
00435      12  ST-LF-OB-TOT-GOOD           PIC S9(9)V99    VALUE ZEROS. EL523
00436      12  ST-LF-CAN-TOT-GOOD          PIC S9(9)V99    VALUE ZEROS. EL523
00437      12  ST-LF-NET-TOT-GOOD          PIC S9(9)V99    VALUE ZEROS. EL523
00438      12  ST-LF-COM-TOT-GOOD          PIC S9(9)V99    VALUE ZEROS. EL523
00439      12  ST-SP-I-CNT-TOT-GOOD        PIC S9(7)       VALUE ZEROS. EL523
00440      12  ST-OB-I-CNT-TOT-GOOD        PIC S9(7)       VALUE ZEROS. EL523
00441      12  ST-C-CNT-TOT-GOOD           PIC S9(7)       VALUE ZEROS. EL523
00442      12  ST-LF-I-CNT-TOT-GOOD        PIC S9(7)       VALUE ZEROS. EL523
00443      12  ST-LF-OB-CNT-TOT-GOOD       PIC S9(7)       VALUE ZEROS. EL523
00444      12  ST-LF-C-CNT-TOT-GOOD        PIC S9(7)       VALUE ZEROS. EL523
00445                                                                   EL523
00446      12  ST-AH-AMT-TOT-GOOD          PIC S9(9)V99    VALUE ZEROS. EL523
00447      12  ST-AH-OB-TOT-GOOD           PIC S9(9)V99    VALUE ZEROS. EL523
00448      12  ST-AH-CAN-TOT-GOOD          PIC S9(9)V99    VALUE ZEROS. EL523
00449      12  ST-AH-NET-TOT-GOOD          PIC S9(9)V99    VALUE ZEROS. EL523
00450      12  ST-AH-COM-TOT-GOOD          PIC S9(9)V99    VALUE ZEROS. EL523
00451      12  ST-AH-I-CNT-TOT-GOOD        PIC S9(7)       VALUE ZEROS. EL523
00452      12  ST-AH-OB-CNT-TOT-GOOD       PIC S9(7)       VALUE ZEROS. EL523
00453      12  ST-AH-C-CNT-TOT-GOOD        PIC S9(7)       VALUE ZEROS. EL523
00454                                                                   EL523
00455      12  ST-LF-AMT-TOT-BAD           PIC S9(9)V99    VALUE ZEROS. EL523
00456      12  ST-LF-OB-TOT-BAD            PIC S9(9)V99    VALUE ZEROS. EL523
00457      12  ST-LF-CAN-TOT-BAD           PIC S9(9)V99    VALUE ZEROS. EL523
00458      12  ST-LF-NET-TOT-BAD           PIC S9(9)V99    VALUE ZEROS. EL523
00459      12  ST-LF-COM-TOT-BAD           PIC S9(9)V99    VALUE ZEROS. EL523
00460      12  ST-SP-I-CNT-TOT-BAD         PIC S9(7)       VALUE ZEROS. EL523
00461      12  ST-OB-I-CNT-TOT-BAD         PIC S9(7)       VALUE ZEROS. EL523
00462      12  ST-C-CNT-TOT-BAD            PIC S9(7)       VALUE ZEROS. EL523
00463      12  ST-LF-I-CNT-TOT-BAD         PIC S9(7)       VALUE ZEROS. EL523
00464      12  ST-LF-OB-CNT-TOT-BAD        PIC S9(7)       VALUE ZEROS. EL523
00465      12  ST-LF-C-CNT-TOT-BAD         PIC S9(7)       VALUE ZEROS. EL523
00466                                                                   EL523
00467      12  ST-AH-AMT-TOT-BAD           PIC S9(9)V99    VALUE ZEROS. EL523
00468      12  ST-AH-OB-TOT-BAD            PIC S9(9)V99    VALUE ZEROS. EL523
00469      12  ST-AH-CAN-TOT-BAD           PIC S9(9)V99    VALUE ZEROS. EL523
00470      12  ST-AH-NET-TOT-BAD           PIC S9(9)V99    VALUE ZEROS. EL523
00471      12  ST-AH-COM-TOT-BAD           PIC S9(9)V99    VALUE ZEROS. EL523
00472      12  ST-AH-I-CNT-TOT-BAD         PIC S9(7)       VALUE ZEROS. EL523
00473      12  ST-AH-OB-CNT-TOT-BAD        PIC S9(7)       VALUE ZEROS. EL523
00474      12  ST-AH-C-CNT-TOT-BAD         PIC S9(7)       VALUE ZEROS. EL523
00475                                                                   EL523
00476      12  ST-LF-AMT-TOT-ALL           PIC S9(9)V99    VALUE ZEROS. EL523
00477      12  ST-LF-OB-TOT-ALL            PIC S9(9)V99    VALUE ZEROS. EL523
00478      12  ST-LF-CAN-TOT-ALL           PIC S9(9)V99    VALUE ZEROS. EL523
00479      12  ST-LF-NET-TOT-ALL           PIC S9(9)V99    VALUE ZEROS. EL523
00480      12  ST-LF-COM-TOT-ALL           PIC S9(9)V99    VALUE ZEROS. EL523
00481      12  ST-LF-I-CNT-TOT-ALL         PIC S9(7)       VALUE ZEROS. EL523
00482      12  ST-LF-OB-CNT-TOT-ALL        PIC S9(7)       VALUE ZEROS. EL523
00483      12  ST-LF-C-CNT-TOT-ALL         PIC S9(7)       VALUE ZEROS. EL523
00484                                                                   EL523
00485      12  ST-AH-AMT-TOT-ALL           PIC S9(9)V99    VALUE ZEROS. EL523
00486      12  ST-AH-OB-TOT-ALL            PIC S9(9)V99    VALUE ZEROS. EL523
00487      12  ST-AH-CAN-TOT-ALL           PIC S9(9)V99    VALUE ZEROS. EL523
00488      12  ST-AH-NET-TOT-ALL           PIC S9(9)V99    VALUE ZEROS. EL523
00489      12  ST-AH-COM-TOT-ALL           PIC S9(9)V99    VALUE ZEROS. EL523
00490      12  ST-AH-I-CNT-TOT-ALL         PIC S9(7)       VALUE ZEROS. EL523
00491      12  ST-AH-OB-CNT-TOT-ALL        PIC S9(7)       VALUE ZEROS. EL523
00492      12  ST-AH-C-CNT-TOT-ALL         PIC S9(7)       VALUE ZEROS. EL523
00493                                                                   EL523
00494      12  CO-LF-AMT-TOT-GOOD          PIC S9(9)V99    VALUE ZEROS. EL523
00495      12  CO-LF-OB-TOT-GOOD           PIC S9(9)V99    VALUE ZEROS. EL523
00496      12  CO-LF-CAN-TOT-GOOD          PIC S9(9)V99    VALUE ZEROS. EL523
00497      12  CO-LF-NET-TOT-GOOD          PIC S9(9)V99    VALUE ZEROS. EL523
00498      12  CO-LF-COM-TOT-GOOD          PIC S9(9)V99    VALUE ZEROS. EL523
00499      12  CO-SP-I-CNT-TOT-GOOD        PIC S9(7)       VALUE ZEROS. EL523
00500      12  CO-OB-I-CNT-TOT-GOOD        PIC S9(7)       VALUE ZEROS. EL523
00501      12  CO-C-CNT-TOT-GOOD           PIC S9(7)       VALUE ZEROS. EL523
00502      12  CO-LF-I-CNT-TOT-GOOD        PIC S9(7)       VALUE ZEROS. EL523
00503      12  CO-LF-OB-CNT-TOT-GOOD       PIC S9(7)       VALUE ZEROS. EL523
00504      12  CO-LF-C-CNT-TOT-GOOD        PIC S9(7)       VALUE ZEROS. EL523
00505                                                                   EL523
00506      12  CO-AH-AMT-TOT-GOOD          PIC S9(9)V99    VALUE ZEROS. EL523
00507      12  CO-AH-OB-TOT-GOOD           PIC S9(9)V99    VALUE ZEROS. EL523
00508      12  CO-AH-CAN-TOT-GOOD          PIC S9(9)V99    VALUE ZEROS. EL523
00509      12  CO-AH-NET-TOT-GOOD          PIC S9(9)V99    VALUE ZEROS. EL523
00510      12  CO-AH-COM-TOT-GOOD          PIC S9(9)V99    VALUE ZEROS. EL523
00511      12  CO-AH-I-CNT-TOT-GOOD        PIC S9(7)       VALUE ZEROS. EL523
00512      12  CO-AH-OB-CNT-TOT-GOOD       PIC S9(7)       VALUE ZEROS. EL523
00513      12  CO-AH-C-CNT-TOT-GOOD        PIC S9(7)       VALUE ZEROS. EL523
00514                                                                   EL523
00515      12  CO-LF-AMT-TOT-BAD           PIC S9(9)V99    VALUE ZEROS. EL523
00516      12  CO-LF-OB-TOT-BAD            PIC S9(9)V99    VALUE ZEROS. EL523
00517      12  CO-LF-CAN-TOT-BAD           PIC S9(9)V99    VALUE ZEROS. EL523
00518      12  CO-LF-NET-TOT-BAD           PIC S9(9)V99    VALUE ZEROS. EL523
00519      12  CO-LF-COM-TOT-BAD           PIC S9(9)V99    VALUE ZEROS. EL523
00520      12  CO-SP-I-CNT-TOT-BAD         PIC S9(7)       VALUE ZEROS. EL523
00521      12  CO-OB-I-CNT-TOT-BAD         PIC S9(7)       VALUE ZEROS. EL523
00522      12  CO-C-CNT-TOT-BAD            PIC S9(7)       VALUE ZEROS. EL523
00523      12  CO-LF-I-CNT-TOT-BAD         PIC S9(7)       VALUE ZEROS. EL523
00524      12  CO-LF-OB-CNT-TOT-BAD        PIC S9(7)       VALUE ZEROS. EL523
00525      12  CO-LF-C-CNT-TOT-BAD         PIC S9(7)       VALUE ZEROS. EL523
00526                                                                   EL523
00527      12  CO-AH-AMT-TOT-BAD           PIC S9(9)V99    VALUE ZEROS. EL523
00528      12  CO-AH-OB-TOT-BAD            PIC S9(9)V99    VALUE ZEROS. EL523
00529      12  CO-AH-CAN-TOT-BAD           PIC S9(9)V99    VALUE ZEROS. EL523
00530      12  CO-AH-NET-TOT-BAD           PIC S9(9)V99    VALUE ZEROS. EL523
00531      12  CO-AH-COM-TOT-BAD           PIC S9(9)V99    VALUE ZEROS. EL523
00532      12  CO-AH-I-CNT-TOT-BAD         PIC S9(7)       VALUE ZEROS. EL523
00533      12  CO-AH-OB-CNT-TOT-BAD        PIC S9(7)       VALUE ZEROS. EL523
00534      12  CO-AH-C-CNT-TOT-BAD         PIC S9(7)       VALUE ZEROS. EL523
00535                                                                   EL523
00536      12  CO-LF-AMT-TOT-ALL           PIC S9(9)V99    VALUE ZEROS. EL523
00537      12  CO-LF-OB-TOT-ALL            PIC S9(9)V99    VALUE ZEROS. EL523
00538      12  CO-LF-CAN-TOT-ALL           PIC S9(9)V99    VALUE ZEROS. EL523
00539      12  CO-LF-NET-TOT-ALL           PIC S9(9)V99    VALUE ZEROS. EL523
00540      12  CO-LF-COM-TOT-ALL           PIC S9(9)V99    VALUE ZEROS. EL523
00541      12  CO-LF-I-CNT-TOT-ALL         PIC S9(7)       VALUE ZEROS. EL523
00542      12  CO-LF-OB-CNT-TOT-ALL        PIC S9(7)       VALUE ZEROS. EL523
00543      12  CO-LF-C-CNT-TOT-ALL         PIC S9(7)       VALUE ZEROS. EL523
00544                                                                   EL523
00545      12  CO-AH-AMT-TOT-ALL           PIC S9(9)V99    VALUE ZEROS. EL523
00546      12  CO-AH-OB-TOT-ALL            PIC S9(9)V99    VALUE ZEROS. EL523
00547      12  CO-AH-CAN-TOT-ALL           PIC S9(9)V99    VALUE ZEROS. EL523
00548      12  CO-AH-NET-TOT-ALL           PIC S9(9)V99    VALUE ZEROS. EL523
00549      12  CO-AH-COM-TOT-ALL           PIC S9(9)V99    VALUE ZEROS. EL523
00550      12  CO-AH-I-CNT-TOT-ALL         PIC S9(7)       VALUE ZEROS. EL523
00551      12  CO-AH-OB-CNT-TOT-ALL        PIC S9(7)       VALUE ZEROS. EL523
00552      12  CO-AH-C-CNT-TOT-ALL         PIC S9(7)       VALUE ZEROS. EL523
00553                                                                   EL523
00554      12  CAR-LF-AMT-TOT-GOOD         PIC S9(9)V99    VALUE ZEROS. EL523
00555      12  CAR-LF-OB-TOT-GOOD          PIC S9(9)V99    VALUE ZEROS. EL523
00556      12  CAR-LF-CAN-TOT-GOOD         PIC S9(9)V99    VALUE ZEROS. EL523
00557      12  CAR-LF-NET-TOT-GOOD         PIC S9(9)V99    VALUE ZEROS. EL523
00558      12  CAR-LF-COM-TOT-GOOD         PIC S9(9)V99    VALUE ZEROS. EL523
00559      12  CAR-SP-I-CNT-TOT-GOOD       PIC S9(7)       VALUE ZEROS. EL523
00560      12  CAR-OB-I-CNT-TOT-GOOD       PIC S9(7)       VALUE ZEROS. EL523
00561      12  CAR-C-CNT-TOT-GOOD          PIC S9(7)       VALUE ZEROS. EL523
00562      12  CAR-LF-I-CNT-TOT-GOOD       PIC S9(7)       VALUE ZEROS. EL523
00563      12  CAR-LF-OB-CNT-TOT-GOOD      PIC S9(7)       VALUE ZEROS. EL523
00564      12  CAR-LF-C-CNT-TOT-GOOD       PIC S9(7)       VALUE ZEROS. EL523
00565                                                                   EL523
00566      12  CAR-AH-AMT-TOT-GOOD         PIC S9(9)V99    VALUE ZEROS. EL523
00567      12  CAR-AH-OB-TOT-GOOD          PIC S9(9)V99    VALUE ZEROS. EL523
00568      12  CAR-AH-CAN-TOT-GOOD         PIC S9(9)V99    VALUE ZEROS. EL523
00569      12  CAR-AH-NET-TOT-GOOD         PIC S9(9)V99    VALUE ZEROS. EL523
00570      12  CAR-AH-COM-TOT-GOOD         PIC S9(9)V99    VALUE ZEROS. EL523
00571      12  CAR-AH-I-CNT-TOT-GOOD       PIC S9(7)       VALUE ZEROS. EL523
00572      12  CAR-AH-OB-CNT-TOT-GOOD      PIC S9(7)       VALUE ZEROS. EL523
00573      12  CAR-AH-C-CNT-TOT-GOOD       PIC S9(7)       VALUE ZEROS. EL523
00574                                                                   EL523
00575      12  CAR-LF-AMT-TOT-BAD          PIC S9(9)V99    VALUE ZEROS. EL523
00576      12  CAR-LF-OB-TOT-BAD           PIC S9(9)V99    VALUE ZEROS. EL523
00577      12  CAR-LF-CAN-TOT-BAD          PIC S9(9)V99    VALUE ZEROS. EL523
00578      12  CAR-LF-NET-TOT-BAD          PIC S9(9)V99    VALUE ZEROS. EL523
00579      12  CAR-LF-COM-TOT-BAD          PIC S9(9)V99    VALUE ZEROS. EL523
00580      12  CAR-SP-I-CNT-TOT-BAD        PIC S9(7)       VALUE ZEROS. EL523
00581      12  CAR-OB-I-CNT-TOT-BAD        PIC S9(7)       VALUE ZEROS. EL523
00582      12  CAR-C-CNT-TOT-BAD           PIC S9(7)       VALUE ZEROS. EL523
00583      12  CAR-LF-I-CNT-TOT-BAD        PIC S9(7)       VALUE ZEROS. EL523
00584      12  CAR-LF-OB-CNT-TOT-BAD       PIC S9(7)       VALUE ZEROS. EL523
00585      12  CAR-LF-C-CNT-TOT-BAD        PIC S9(7)       VALUE ZEROS. EL523
00586                                                                   EL523
00587      12  CAR-AH-AMT-TOT-BAD          PIC S9(9)V99    VALUE ZEROS. EL523
00588      12  CAR-AH-OB-TOT-BAD           PIC S9(9)V99    VALUE ZEROS. EL523
00589      12  CAR-AH-CAN-TOT-BAD          PIC S9(9)V99    VALUE ZEROS. EL523
00590      12  CAR-AH-NET-TOT-BAD          PIC S9(9)V99    VALUE ZEROS. EL523
00591      12  CAR-AH-COM-TOT-BAD          PIC S9(9)V99    VALUE ZEROS. EL523
00592      12  CAR-AH-I-CNT-TOT-BAD        PIC S9(7)       VALUE ZEROS. EL523
00593      12  CAR-AH-OB-CNT-TOT-BAD       PIC S9(7)       VALUE ZEROS. EL523
00594      12  CAR-AH-C-CNT-TOT-BAD        PIC S9(7)       VALUE ZEROS. EL523
00595                                                                   EL523
00596      12  CAR-LF-AMT-TOT-ALL          PIC S9(9)V99    VALUE ZEROS. EL523
00597      12  CAR-LF-OB-TOT-ALL           PIC S9(9)V99    VALUE ZEROS. EL523
00598      12  CAR-LF-CAN-TOT-ALL          PIC S9(9)V99    VALUE ZEROS. EL523
00599      12  CAR-LF-NET-TOT-ALL          PIC S9(9)V99    VALUE ZEROS. EL523
00600      12  CAR-LF-COM-TOT-ALL          PIC S9(9)V99    VALUE ZEROS. EL523
00601      12  CAR-LF-I-CNT-TOT-ALL        PIC S9(7)       VALUE ZEROS. EL523
00602      12  CAR-LF-OB-CNT-TOT-ALL       PIC S9(7)       VALUE ZEROS. EL523
00603      12  CAR-LF-C-CNT-TOT-ALL        PIC S9(7)       VALUE ZEROS. EL523
00604                                                                   EL523
00605      12  CAR-AH-AMT-TOT-ALL          PIC S9(9)V99    VALUE ZEROS. EL523
00606      12  CAR-AH-OB-TOT-ALL           PIC S9(9)V99    VALUE ZEROS. EL523
00607      12  CAR-AH-CAN-TOT-ALL          PIC S9(9)V99    VALUE ZEROS. EL523
00608      12  CAR-AH-NET-TOT-ALL          PIC S9(9)V99    VALUE ZEROS. EL523
00609      12  CAR-AH-COM-TOT-ALL          PIC S9(9)V99    VALUE ZEROS. EL523
00610      12  CAR-AH-I-CNT-TOT-ALL        PIC S9(7)       VALUE ZEROS. EL523
00611      12  CAR-AH-OB-CNT-TOT-ALL       PIC S9(7)       VALUE ZEROS. EL523
00612      12  CAR-AH-C-CNT-TOT-ALL        PIC S9(7)       VALUE ZEROS. EL523
00613                                                                   EL523
00614      12  FIN-LF-AMT-TOT-GOOD         PIC S9(9)V99    VALUE ZEROS. EL523
00615      12  FIN-LF-OB-TOT-GOOD          PIC S9(9)V99    VALUE ZEROS. EL523
00616      12  FIN-LF-CAN-TOT-GOOD         PIC S9(9)V99    VALUE ZEROS. EL523
00617      12  FIN-LF-NET-TOT-GOOD         PIC S9(9)V99    VALUE ZEROS. EL523
00618      12  FIN-LF-COM-TOT-GOOD         PIC S9(9)V99    VALUE ZEROS. EL523
00619      12  FIN-SP-I-CNT-TOT-GOOD       PIC S9(7)       VALUE ZEROS. EL523
00620      12  FIN-OB-I-CNT-TOT-GOOD       PIC S9(7)       VALUE ZEROS. EL523
00621      12  FIN-C-CNT-TOT-GOOD          PIC S9(7)       VALUE ZEROS. EL523
00622      12  FIN-LF-I-CNT-TOT-GOOD       PIC S9(7)       VALUE ZEROS. EL523
00623      12  FIN-LF-OB-CNT-TOT-GOOD      PIC S9(7)       VALUE ZEROS. EL523
00624      12  FIN-LF-C-CNT-TOT-GOOD       PIC S9(7)       VALUE ZEROS. EL523
00625                                                                   EL523
00626      12  FIN-AH-AMT-TOT-GOOD         PIC S9(9)V99    VALUE ZEROS. EL523
00627      12  FIN-AH-OB-TOT-GOOD          PIC S9(9)V99    VALUE ZEROS. EL523
00628      12  FIN-AH-CAN-TOT-GOOD         PIC S9(9)V99    VALUE ZEROS. EL523
00629      12  FIN-AH-NET-TOT-GOOD         PIC S9(9)V99    VALUE ZEROS. EL523
00630      12  FIN-AH-COM-TOT-GOOD         PIC S9(9)V99    VALUE ZEROS. EL523
00631      12  FIN-AH-I-CNT-TOT-GOOD       PIC S9(7)       VALUE ZEROS. EL523
00632      12  FIN-AH-OB-CNT-TOT-GOOD      PIC S9(7)       VALUE ZEROS. EL523
00633      12  FIN-AH-C-CNT-TOT-GOOD       PIC S9(7)       VALUE ZEROS. EL523
00634                                                                   EL523
00635      12  FIN-LF-AMT-TOT-BAD          PIC S9(9)V99    VALUE ZEROS. EL523
00636      12  FIN-LF-OB-TOT-BAD           PIC S9(9)V99    VALUE ZEROS. EL523
00637      12  FIN-LF-CAN-TOT-BAD          PIC S9(9)V99    VALUE ZEROS. EL523
00638      12  FIN-LF-NET-TOT-BAD          PIC S9(9)V99    VALUE ZEROS. EL523
00639      12  FIN-LF-COM-TOT-BAD          PIC S9(9)V99    VALUE ZEROS. EL523
00640      12  FIN-SP-I-CNT-TOT-BAD        PIC S9(7)       VALUE ZEROS. EL523
00641      12  FIN-OB-I-CNT-TOT-BAD        PIC S9(7)       VALUE ZEROS. EL523
00642      12  FIN-C-CNT-TOT-BAD           PIC S9(7)       VALUE ZEROS. EL523
00643      12  FIN-LF-I-CNT-TOT-BAD        PIC S9(7)       VALUE ZEROS. EL523
00644      12  FIN-LF-OB-CNT-TOT-BAD       PIC S9(7)       VALUE ZEROS. EL523
00645      12  FIN-LF-C-CNT-TOT-BAD        PIC S9(7)       VALUE ZEROS. EL523
00646                                                                   EL523
00647      12  FIN-AH-AMT-TOT-BAD          PIC S9(9)V99    VALUE ZEROS. EL523
00648      12  FIN-AH-OB-TOT-BAD           PIC S9(9)V99    VALUE ZEROS. EL523
00649      12  FIN-AH-CAN-TOT-BAD          PIC S9(9)V99    VALUE ZEROS. EL523
00650      12  FIN-AH-NET-TOT-BAD          PIC S9(9)V99    VALUE ZEROS. EL523
00651      12  FIN-AH-COM-TOT-BAD          PIC S9(9)V99    VALUE ZEROS. EL523
00652      12  FIN-AH-I-CNT-TOT-BAD        PIC S9(7)       VALUE ZEROS. EL523
00653      12  FIN-AH-OB-CNT-TOT-BAD       PIC S9(7)       VALUE ZEROS. EL523
00654      12  FIN-AH-C-CNT-TOT-BAD        PIC S9(7)       VALUE ZEROS. EL523
00655                                                                   EL523
00656      12  FIN-LF-AMT-TOT-ALL          PIC S9(9)V99    VALUE ZEROS. EL523
00657      12  FIN-LF-OB-TOT-ALL           PIC S9(9)V99    VALUE ZEROS. EL523
00658      12  FIN-LF-CAN-TOT-ALL          PIC S9(9)V99    VALUE ZEROS. EL523
00659      12  FIN-LF-NET-TOT-ALL          PIC S9(9)V99    VALUE ZEROS. EL523
00660      12  FIN-LF-COM-TOT-ALL          PIC S9(9)V99    VALUE ZEROS. EL523
00661      12  FIN-LF-I-CNT-TOT-ALL        PIC S9(7)       VALUE ZEROS. EL523
00662      12  FIN-LF-OB-CNT-TOT-ALL       PIC S9(7)       VALUE ZEROS. EL523
00663      12  FIN-LF-C-CNT-TOT-ALL        PIC S9(7)       VALUE ZEROS. EL523
00664                                                                   EL523
00665      12  FIN-AH-AMT-TOT-ALL          PIC S9(9)V99    VALUE ZEROS. EL523
00666      12  FIN-AH-OB-TOT-ALL           PIC S9(9)V99    VALUE ZEROS. EL523
00667      12  FIN-AH-CAN-TOT-ALL          PIC S9(9)V99    VALUE ZEROS. EL523
00668      12  FIN-AH-NET-TOT-ALL          PIC S9(9)V99    VALUE ZEROS. EL523
00669      12  FIN-AH-COM-TOT-ALL          PIC S9(9)V99    VALUE ZEROS. EL523
00670      12  FIN-AH-I-CNT-TOT-ALL        PIC S9(7)       VALUE ZEROS. EL523
00671      12  FIN-AH-OB-CNT-TOT-ALL       PIC S9(7)       VALUE ZEROS. EL523
00672      12  FIN-AH-C-CNT-TOT-ALL        PIC S9(7)       VALUE ZEROS. EL523
00673  EJECT                                                            EL523
00674  01  WS-HEADING1.                                                 EL523
00675      12  FILLER                  PIC X(42)       VALUE '1'.       EL523
00676      12  WS-H1-TITLE             PIC X(50)       VALUE SPACES.    EL523
00677      12  FILLER                  PIC X(28)       VALUE SPACES.    EL523
00678      12  WS-H1-REPORT-NUMBER     PIC X(8)        VALUE 'EL523   '.EL523
00679      12  FILLER                  PIC X(5)        VALUE SPACES.    EL523
00680                                                                   EL523
00681  01  WS-HEADING2.                                                 EL523
00682      12  FILLER              PIC X(48)           VALUE SPACES.    EL523
00683      12  WS-H2-CLIENT-NAME   PIC X(30).                           EL523
00684      12  FILLER              PIC X(42)           VALUE SPACES.    EL523
00685      12  WS-H2-DATE          PIC X(8).                            EL523
00686      12  FILLER              PIC X(5)            VALUE SPACES.    EL523
00687                                                                   EL523
00688  01  WS-HEADING3.                                                 EL523
00689      12  FILLER              PIC X(54)           VALUE SPACES.    EL523
00690      12  WS-H3-DATE          PIC X(18).                           EL523
00691      12  FILLER              PIC X(48)           VALUE SPACES.    EL523
00692      12  FILLER              PIC X(5)            VALUE 'PAGE'.    EL523
00693      12  WS-H3-PAGE          PIC ZZ,ZZZ.                          EL523
00694      12  FILLER              PIC XX              VALUE SPACES.    EL523
00695                                                                   EL523
00696  01  WS-HEADING4.                                                 EL523
00697      12  FILLER              PIC X      VALUE '0'.                EL523
00698      12  WS-H4-CARRIER-CONST PIC X(13)           VALUE            EL523
00699              '  CARRIER - '.                                      EL523
00700      12  WS-H4-CARRIER-NO    PIC X.                               EL523
00701      12  WS-H4-CARRIER-DASH  PIC XXX             VALUE ' - '.     EL523
00702      12  WS-H4-CARRIER       PIC X(36)           VALUE SPACES.    EL523
00703                                                                   EL523
00704  01  WS-HEADING5.                                                 EL523
00705      12  WS-H5-GROUP-CONST   PIC X(13)           VALUE            EL523
00706              '   GROUP   - '.                                     EL523
00707      12  WS-H5-GROUP         PIC X(6).                            EL523
00708                                                                   EL523
00709  01  WS-HEADING5A.                                                EL523
00710      12  FILLER              PIC X(13)           VALUE            EL523
00711              '   RPT CD2 - '.                                     EL523
00712      12  WS-H5A-RPT2         PIC X(10).                           EL523
00713                                                                   EL523
00714  01  WS-HEADING6.                                                 EL523
00715      12  WS-H6-STATE-CONST   PIC X(13)           VALUE            EL523
00716              '   STATE   - '.                                     EL523
00717      12  WS-H6-STATE-NO      PIC XX.                              EL523
00718      12  FILLER              PIC XXX             VALUE ' - '.     EL523
00719      12  WS-H6-STATE         PIC X(36)           VALUE SPACES.    EL523
00720                                                                   EL523
00721  01  WS-HEADING7.                                                 EL523
00722      12  FILLER              PIC X(45)           VALUE            EL523
00723              '0  ACCT                                      '.     EL523
00724      12  FILLER              PIC X(44)           VALUE            EL523
00725              '                           PROCESSABLE      '.      EL523
00726      12  FILLER              PIC X(44)           VALUE            EL523
00727              '   NON PROCESSABLE       ALL BUSINESS ON    '.      EL523
00728                                                                   EL523
00729  01  WS-OPTION3-HEADER7.                                          EL523
00730      12  FILLER              PIC X(45)           VALUE            EL523
00731              '0  ACCT                                      '.     EL523
00732      12  FILLER              PIC X(88)           VALUE SPACES.    EL523
00733                                                                   EL523
00734  01  WS-HEADING8.                                                 EL523
00735      12  FILLER              PIC X(45)           VALUE            EL523
00736              '  NUMBER   ACCOUNT NAME                      '.     EL523
00737      12  FILLER              PIC X(44)           VALUE            EL523
00738              '                             BUSINESS       '.      EL523
00739      12  FILLER              PIC X(44)           VALUE            EL523
00740              '       BUSINESS            PENDING FILE     '.      EL523
00741                                                                   EL523
00742  01  WS-OPTION3-HEADER8.                                          EL523
00743      12  FILLER              PIC X(45)           VALUE            EL523
00744              '  NUMBER   ACCOUNT NAME                      '.     EL523
00745      12  FILLER              PIC X(88)           VALUE SPACES.    EL523
00746                                                                   EL523
00747  01  WS-HEADING9.                                                 EL523
00748      12  FILLER              PIC X(45)           VALUE            EL523
00749              '         LO CERT   HIGH CERT  PHONE NUMBER   '.     EL523
00750      12  FILLER              PIC X(44)           VALUE            EL523
00751              '                       COUNT         AMOUNT '.      EL523
00752      12  FILLER              PIC X(44)           VALUE            EL523
00753              ' COUNT         AMOUNT  COUNT         AMOUNT '.      EL523
00754                                                                   EL523
00755  01  WS-OPTION3-HEADER9.                                          EL523
00756      12  FILLER              PIC X(45)           VALUE            EL523
00757              '         LO CERT   HIGH CERT  PHONE NUMBER   '.     EL523
00758      12  FILLER              PIC X(88)           VALUE SPACES.    EL523
00759                                                                   EL523
00760  01  WS-DETAIL1.                                                  EL523
00761      12  WS-D1-LN.                                                EL523
00762          16  WS-D1-ACCOUNT       PIC X(10).                       EL523
00763          16  FILLER              PIC X.                           EL523
00764          16  WS-D1-NAME          PIC X(30).                       EL523
00765          16  FILLER              PIC X.                           EL523
00766      12  WS-D2-LN  REDEFINES  WS-D1-LN.                           EL523
00767          16  FILLER              PIC X(8).                        EL523
00768          16  WS-D1-LO-MO         PIC XX.                          EL523
00769          16  WS-D1-DASH-1        PIC X.                           EL523
00770          16  WS-D1-LO-DA         PIC XX.                          EL523
00771          16  WS-D1-DASH-2        PIC X.                           EL523
00772          16  WS-D1-LO-YR         PIC XX.                          EL523
00773          16  FILLER              PIC XX.                          EL523
00774          16  WS-D1-HI-MO         PIC XX.                          EL523
00775          16  WS-D1-DASH-3        PIC X.                           EL523
00776          16  WS-D1-HI-DA         PIC XX.                          EL523
00777          16  WS-D1-DASH-4        PIC X.                           EL523
00778          16  WS-D1-HI-YR         PIC XX.                          EL523
00779          16  FILLER              PIC XX.                          EL523
00780          16  WS-D1-PHONE.                                         EL523
00781              20  WS-D1-AREA      PIC X(3).                        EL523
00782              20  WS-D1-DASH1     PIC X.                           EL523
00783              20  WS-D1-PREFIX    PIC X(3).                        EL523
00784              20  WS-D1-DASH2     PIC X.                           EL523
00785              20  WS-D1-NO        PIC X(4).                        EL523
00786          16  FILLER              PIC XX.                          EL523
00787                                                                   EL523
00788      12  WS-D1-LN-REST.                                           EL523
00789          16  WS-D1-LN-FILL1      PIC X(6).                        EL523
00790          16  FILLER              PIC X.                           EL523
00791          16  WS-D1-LN-FILL2      PIC X(14).                       EL523
00792          16  FILLER              PIC X(3).                        EL523
00793          16  WS-D1-CNT-1         PIC ZZ,ZZ9-.                     EL523
00794          16  WS-D1-AMT-1         PIC ZZZ,ZZZ,ZZZ.99-.             EL523
00795          16  WS-D1-CNT-2         PIC ZZ,ZZ9-.                     EL523
00796          16  WS-D1-AMT-2         PIC ZZZ,ZZZ,ZZZ.99-.             EL523
00797          16  WS-D1-CNT-3         PIC ZZ,ZZ9-.                     EL523
00798          16  WS-D1-AMT-3         PIC ZZZ,ZZZ,ZZZ.99-.             EL523
00799                                                                   EL523
00800  01  TOTAL-MESSAGES.                                              EL523
00801      12  TOT-MSG-1           PIC X(44)           VALUE            EL523
00802              ' STATE TOTALS                               '.      EL523
00803      12  TOT-MSG-2           PIC X(44)           VALUE            EL523
00804              '     GROUPING TOTALS                        '.      EL523
00805      12  TOT-MSG-2A          PIC X(44)           VALUE            EL523
00806              '       REPT CD 2 TOTALS                     '.      EL523
00807      12  TOT-MSG-3           PIC X(44)           VALUE            EL523
00808              '         CARRIER TOTALS                     '.      EL523
00809      12  TOT-MSG-4           PIC X(44)           VALUE            EL523
00810              ' FINAL TOTALS                               '.      EL523
00811  EJECT                                                            EL523
00812                              COPY ELCDTECX.                       EL523
00813  EJECT                                                            EL523
00814                              COPY ELCDTEVR.                       EL523
00815  EJECT                                                            EL523
00816                              COPY ELCACCTV.                       EL523
00817  EJECT                                                            EL523
00818                              COPY ELCDATE.                           CL*13
00819  EJECT                                                            EL523
00820  PROCEDURE DIVISION.                                              EL523
00821                                                                   EL523
00822  0000-DATE-CARD-READ SECTION.                                     EL523
00823                              COPY ELCDTERX.                       EL523
00824                                                                   EL523
      ****=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=***
      ****                                                           ***
      ****   Set up the month-end auto balancing.                    ***
      ****   Turn it off if this is a daily run.                     ***
      ****                                                           ***
      ****=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=***

070714     MOVE WS-TIME                TO ME-START-TIME
070714     MOVE WS-CURRENT-DATE        TO ME-START-DATE
070714     MOVE ME-START-MO            TO ME-CNDS-MO
070714     MOVE ME-START-DA            TO ME-CNDS-DA
070714     MOVE ME-START-YR            TO ME-CNDS-YR

00825      MOVE WS-CURRENT-DATE           TO  WS-H2-DATE                EL523
00826      MOVE COMPANY-NAME              TO  WS-H2-CLIENT-NAME         EL523
00827      MOVE ALPH-DATE                 TO  WS-H3-DATE.               EL523
00828                                                                   EL523
00829      MOVE    BIN-RUN-DATE           TO  WS-RUN-DATE-BIN.          EL523
00830                                                                   EL523
00831  1000-MAIN-LOGIC SECTION.                                         EL523
00832      PERFORM 8800-OPEN-FILES.                                     EL523
00833                                                                   EL523

062104     IF DTE-CLIENT = 'CID'
062104        MOVE 'CILGM10'          TO  WS-ME-BAL-JOB
062104     ELSE
030612       IF DTE-CLIENT = 'AHL'
030612         MOVE 'AHLGM10'          TO  WS-ME-BAL-JOB
062121       else
062121         if dte-client = 'FNL'
062121           move 'FLLGM10'        to  ws-me-bal-job
030612         ELSE
062104           MOVE 'CIDCLGM10'      TO  WS-ME-BAL-JOB
062121         end-if
030612       END-IF
062104     END-IF.

00834      SORT SORT1-FILE ON ASCENDING KEY SORT-KEY1                   EL523
00835          INPUT PROCEDURE 1100-INPUT-ROUTINE THRU 1299-EXIT        EL523
00836          OUTPUT PROCEDURE 1300-REPORT-PRINT THRU 1999-EXIT.       EL523
00837                                                                   EL523
00838      IF SORT-RETURN NOT = ZEROS                                   EL523
00839          MOVE  'SORT 1 FAILED'  TO  WS-ABEND-MESSAGE              EL523
00840          MOVE    SORT-RETURN    TO  WS-RETURN-CODE                EL523
00841          GO TO ABEND-PGM.                                         EL523
00842                                                                   EL523
00843      GO TO 2000-SORT-ACCT-XTRACT.                                 EL523
00844  EJECT                                                            EL523
00845  1100-INPUT-ROUTINE SECTION.                                      EL523
00846      MOVE LOW-VALUES             TO  SAVE-CONTROL                 EL523
00847                                      AM-CONTROL-PRIMARY.          EL523
00848      MOVE DTE-CLASIC-COMPANY-CD  TO  AM-COMPANY-CD.               EL523
00849                                                                   EL523
00850      START ERACCT                                                 EL523
00851          KEY IS GREATER THAN AM-CONTROL-PRIMARY.                  EL523
00852                                                                   EL523
00853      IF ERACCT-FILE-STATUS NOT = ZEROS                            EL523
00854          MOVE    'ERROR OCCURED START INITIAL - ERACCT'           EL523
00855                                      TO  WS-ABEND-MESSAGE         EL523
00856          MOVE    ERACCT-FILE-STATUS  TO  WS-ABEND-FILE-STATUS     EL523
00857          GO TO ABEND-PGM.                                         EL523
00858                                                                   EL523
00859  1110-READ-ACCOUNT.                                               EL523
00860      READ ERACCT NEXT RECORD.                                     EL523
00861                                                                   EL523
00862      COPY ELCACCTI.                                               EL523
00863                                                                   EL523
00864      IF ERACCT-FILE-STATUS = '10'                                 EL523
00865          GO TO 1199-END-ACCT-PHASE.                               EL523
00866                                                                   EL523
00867      IF ERACCT-FILE-STATUS NOT = ZEROS                            EL523
00868          MOVE    'ERROR OCCURED READ NEXT - ERACCT'               EL523
00869                                      TO  WS-ABEND-MESSAGE         EL523
00870          MOVE    ERACCT-FILE-STATUS  TO  WS-ABEND-FILE-STATUS     EL523
00871          GO TO ABEND-PGM.                                         EL523
00872                                                                   EL523
00873      IF AM-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD                 EL523
00874          GO TO 1199-END-ACCT-PHASE.                               EL523
00875                                                                   EL523
00876      MOVE AM-CARRIER   TO  ACC-CAR-CNTRL.                         EL523
00877      MOVE AM-GROUPING  TO  ACC-GRP-CNTRL.                         EL523
00878      MOVE AM-STATE     TO  ACC-ST-CNTRL.                          EL523
00879      MOVE AM-ACCOUNT   TO  ACC-ACCT-CNTRL.                        EL523
00880      MOVE SPACES       TO  ACC-RPT2-CNTRL.                        EL523
00881                                                                   EL523
00882      IF AM-HI-CERT-DATE NOT NUMERIC                               EL523
00883         MOVE ZEROS               TO AM-HI-CERT-DATE                  CL**4
00884                                     WS-AM-HI-CERT-DATE.              CL**4
00885                                                                   EL523
00886      IF AM-LO-CERT-DATE NOT NUMERIC                               EL523
00887         MOVE ZEROS               TO AM-LO-CERT-DATE                  CL**2
00888                                     WS-AM-LO-CERT-DATE.              CL**4
00889                                                                   EL523
00890      IF DTE-PGM-OPT = 2                                           EL523
00891          GO TO 1120-USE-THIS-CONTROL.                             EL523
00892                                                                   EL523
00893      IF DTE-PGM-OPT = 3                                           EL523
00894          MOVE SPACES TO  ACC-GRP-CNTRL                            EL523
00895          MOVE 'N'    TO  GROUP-TOTAL-SW                           EL523
00896          GO TO 1120-USE-THIS-CONTROL.                             EL523
00897                                                                   EL523
00898      IF DTE-COMP-VG = SPACE                                       EL523
00899          MOVE SPACES TO  ACC-CAR-CNTRL                            EL523
00900                          ACC-GRP-CNTRL                            EL523
00901                          WS-H4-CARRIER-CONST                      EL523
00902                          WS-H4-CARRIER-DASH                       EL523
00903                          WS-H5-GROUP-CONST                        EL523
00904          MOVE 'N'    TO  CARRIER-TOTAL-SW                         EL523
00905                          GROUP-TOTAL-SW                           EL523
00906      ELSE                                                         EL523
00907          IF DTE-COMP-VG = '2'                                     EL523
00908              MOVE SPACES TO  ACC-GRP-CNTRL                        EL523
00909              MOVE 'N'    TO  GROUP-TOTAL-SW                       EL523
00910              MOVE SPACES TO  WS-H5-GROUP-CONST                    EL523
00911          ELSE                                                     EL523
00912              IF DTE-COMP-VG = '3'                                 EL523
00913                  MOVE SPACES TO  ACC-CAR-CNTRL                    EL523
00914                                  ACC-GRP-CNTRL                    EL523
00915                                  ACC-ST-CNTRL                     EL523
00916                                  WS-H4-CARRIER-CONST              EL523
00917                                  WS-H4-CARRIER-DASH               EL523
00918                                  WS-H5-GROUP-CONST                EL523
00919                                  WS-H6-STATE-CONST                EL523
00920                  MOVE 'N'    TO  CARRIER-TOTAL-SW                 EL523
00921                                  GROUP-TOTAL-SW                   EL523
00922                                  STATE-TOTAL-SW                   EL523
00923              ELSE                                                 EL523
00924                  IF DTE-COMP-VG = '4'                             EL523
00925                      MOVE SPACES TO  ACC-GRP-CNTRL                EL523
00926                                      ACC-ST-CNTRL                 EL523
00927                                      WS-H5-GROUP-CONST            EL523
00928                                      WS-H6-STATE-CONST            EL523
00929                      MOVE 'N'    TO  GROUP-TOTAL-SW               EL523
00930                                      STATE-TOTAL-SW.              EL523
00931                                                                   EL523
00932  1120-USE-THIS-CONTROL.                                           EL523
00933      IF FIRST-ACCT                                                EL523
00934          MOVE 'N'                TO  FIRST-ACCT-SW                EL523
00935          MOVE ACCT-CONTROL       TO  SAVE-CONTROL.                EL523
00936                                                                   EL523
00937      IF ACCT-CONTROL EQUAL SAVE-CONTROL                           EL523
00938         IF AM-HI-CERT-DATE NOT EQUAL ZEROS                        EL523
00939            MOVE AM-HI-CERT-DATE  TO WS-AM-HI-DT.                  EL523
00940                                                                   EL523
00941      IF ACCT-CONTROL EQUAL SAVE-CONTROL                           EL523
00942         IF AM-LO-CERT-DATE NOT EQUAL ZEROS                        EL523
00943            IF REPT-XTR-AM-LO-DT EQUAL ZEROS                       EL523
00944               MOVE AM-LO-CERT-DATE                                EL523
00945                                  TO WS-AM-LO-DT.                  EL523
00946                                                                   EL523
00947      IF ACCT-CONTROL = SAVE-CONTROL                               EL523
00948          MOVE SPACES             TO  REPORT-XTRACT                EL523
00949          MOVE ACCT-CONTROL       TO  REPT-XTR-CONTROL             EL523
00950          MOVE '0'                TO  REPT-XTR-REC-TYP             EL523
00951          MOVE AM-NAME            TO  REPT-XTR-NAME                EL523
00952          MOVE AM-EFFECTIVE-DT    TO  REPT-XTR-AM-EFF-DT           EL523
00953          MOVE WS-AM-HI-DT        TO  REPT-XTR-AM-HI-DT            EL523
00954          MOVE WS-AM-LO-DT        TO  REPT-XTR-AM-LO-DT            EL523
00955          MOVE AM-TEL-NO          TO  REPT-XTR-TEL-NO              EL523
00956          MOVE ZEROS              TO  REPT-XTR-EFF-DATE            EL523
00957          MOVE AM-REPORT-CODE-2   TO  REPT-XTR-REPORT-CODE-2       EL523
102004         EVALUATE AM-STATUS
                  WHEN '1'
                     MOVE 'I'          TO REPT-ACTIVE
                  WHEN '2'
                     MOVE 'T'          TO REPT-ACTIVE
                  WHEN '3'
                     MOVE 'C'          TO REPT-ACTIVE
022808            WHEN '4'
022808               MOVE 'F'          TO REPT-ACTIVE
031811            WHEN '5'
031811               MOVE 'S'          TO REPT-ACTIVE
021916            WHEN '6'
021916               MOVE 'D'          TO REPT-ACTIVE
021916            WHEN '7'
021916               MOVE 'L'          TO REPT-ACTIVE
021916            WHEN '8'
021916               MOVE 'R'          TO REPT-ACTIVE
021916            WHEN '9'
021916               MOVE 'P'          TO REPT-ACTIVE
                  WHEN OTHER
                     MOVE 'A'          TO REPT-ACTIVE
               END-EVALUATE
               GO TO 1110-READ-ACCOUNT
00958 *        IF AM-STATUS = '0' OR 'A'                                EL523
00959 *            MOVE 'A'            TO  REPT-ACTIVE                  EL523
00960 *            GO TO 1110-READ-ACCOUNT                              EL523
00961 *        ELSE                                                     EL523
00962 *            MOVE 'I'            TO  REPT-ACTIVE                  EL523
00963 *            GO TO 1110-READ-ACCOUNT                              EL523
00964      ELSE                                                         EL523
00965          PERFORM 1250-CLEAR-XTRACT THRU 1259-EXIT                 EL523
00966          PERFORM 1240-RELEASE-SORT-REC THRU 1249-EXIT             EL523
00967          MOVE ACCT-CONTROL  TO  SAVE-CONTROL.                     EL523
00968                                                                   EL523
00969      MOVE SPACES            TO  REPORT-XTRACT.                    EL523
00970      MOVE ACCT-CONTROL      TO  REPT-XTR-CONTROL.                 EL523
00971      MOVE '0'               TO  REPT-XTR-REC-TYP.                 EL523
00972      MOVE AM-NAME           TO  REPT-XTR-NAME.                    EL523
00973      MOVE AM-EFFECTIVE-DT   TO  REPT-XTR-AM-EFF-DT                EL523
00974      MOVE AM-HI-CERT-DATE   TO  REPT-XTR-AM-HI-DT                 EL523
00975      MOVE AM-LO-CERT-DATE   TO  REPT-XTR-AM-LO-DT                 EL523
00976      MOVE AM-TEL-NO         TO  REPT-XTR-TEL-NO.                  EL523
00977      MOVE ZEROS             TO  REPT-XTR-EFF-DATE.                EL523
00978      MOVE AM-REPORT-CODE-2  TO  REPT-XTR-REPORT-CODE-2.           EL523
00979                                                                   EL523
102004     EVALUATE AM-STATUS
              WHEN '1'
                 MOVE 'I'              TO REPT-ACTIVE
              WHEN '2'
                 MOVE 'T'              TO REPT-ACTIVE
              WHEN '3'
                 MOVE 'C'              TO REPT-ACTIVE
022808        WHEN '4'
022808           MOVE 'F'              TO REPT-ACTIVE
031811        WHEN '5'
031811           MOVE 'S'              TO REPT-ACTIVE
021916        WHEN '6'
021916           MOVE 'D'              TO REPT-ACTIVE
021916        WHEN '7'
021916           MOVE 'L'              TO REPT-ACTIVE
021916        WHEN '8'
021916           MOVE 'R'              TO REPT-ACTIVE
021916        WHEN '9'
021916           MOVE 'P'              TO REPT-ACTIVE
              WHEN OTHER
                 MOVE 'A'              TO REPT-ACTIVE
           END-EVALUATE
           
00980 *    IF AM-STATUS = '0' OR 'A'                                    EL523
00981 *        MOVE 'A'       TO  REPT-ACTIVE                           EL523
00982 *    ELSE                                                         EL523
00983 *        MOVE 'I'       TO  REPT-ACTIVE.                          EL523
00984                                                                   EL523
00985      GO TO 1110-READ-ACCOUNT.                                     EL523
00986                                                                   EL523
00987  1199-END-ACCT-PHASE.                                             EL523
00988      PERFORM 1250-CLEAR-XTRACT THRU 1259-EXIT.                    EL523
00989                                                                   EL523
00990      PERFORM 1240-RELEASE-SORT-REC THRU 1249-EXIT.                EL523
00991                                                                   EL523
00992      MOVE LOW-VALUES             TO  PB-CONTROL-PRIMARY.          EL523
00993      MOVE DTE-CLASIC-COMPANY-CD  TO  PB-COMPANY-CD.               EL523
00994                                                                   EL523
00995      START ERPNDB                                                 EL523
00996          KEY IS GREATER THAN PB-CONTROL-PRIMARY.                  EL523
00997                                                                   EL523
00998      IF ERPNDB-FILE-STATUS = '23'                                 EL523
00999          GO TO 1299-EXIT.                                         EL523
01000                                                                   EL523
01001      IF ERPNDB-FILE-STATUS NOT = ZEROS                            EL523
01002          MOVE 'ERROR OCCURED START INITIAL - ERPNDB'              EL523
01003                                   TO  WS-ABEND-MESSAGE            EL523
01004          MOVE ERPNDB-FILE-STATUS  TO  WS-ABEND-FILE-STATUS        EL523
01005          GO TO ABEND-PGM.                                         EL523
01006  EJECT                                                            EL523
01007  1200-READ-PENDING.                                               EL523
01008      READ ERPNDB NEXT RECORD.                                     EL523
01009                                                                   EL523
01010      IF ERPNDB-FILE-STATUS = '10'                                 EL523
01011          GO TO 1299-EXIT.                                         EL523
01012                                                                   EL523
01013      IF ERPNDB-FILE-STATUS NOT = ZEROS                            EL523
01014          MOVE 'ERROR OCCURED READ NEXT - ERPNDB'                  EL523
01015                                   TO  WS-ABEND-MESSAGE            EL523
01016          MOVE ERPNDB-FILE-STATUS  TO  WS-ABEND-FILE-STATUS        EL523
01017          GO TO ABEND-PGM.                                         EL523
01018                                                                   EL523
01019      IF PB-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD                 EL523
01020          GO TO 1299-EXIT.                                         EL523
01021                                                                   EL523
01022      IF PB-CREDIT-ACCEPT-DT = WS-RUN-DATE-BIN OR LOW-VALUES       EL523
01023          NEXT SENTENCE                                            EL523
01024      ELSE                                                         EL523
01025          GO TO 1200-READ-PENDING.                                 EL523
01026                                                                   EL523
01027      IF PB-CREDIT-SELECT-DT GREATER THAN WS-RUN-DATE-BIN          EL523
01028          GO TO 1200-READ-PENDING.                                 EL523
01029                                                                   EL523
01030      IF PB-ISSUE OR                                               EL523
01031         PB-CANCELLATION                                           EL523
01032          NEXT SENTENCE                                            EL523
01033      ELSE                                                         EL523
01034          GO TO 1200-READ-PENDING.                                 EL523
01035                                                                   EL523
01036      IF PB-ALT-CHG-SEQ-NO NOT = ZEROS                             EL523
01037          GO TO 1200-READ-PENDING.                                 EL523
01038                                                                   EL523
01039      IF PB-RECORD-ON-HOLD   OR                                    EL523
01040         PB-RECORD-RETURNED  OR                                    EL523
01041         PB-FATAL-ERRORS     OR                                    EL523
01042         PB-UNFORCED-ERRORS                                        EL523
01043           MOVE '1' TO PEND-SW.                                    EL523
01044                                                                   EL523
01045      IF PB-OVERRIDE-LIFE    OR                                    EL523
01046         PB-OVERRIDE-BOTH                                          EL523
01047             IF PB-ISSUE                                           EL523
01048                MOVE PB-I-LF-PREM-CALC TO PB-I-LF-PREMIUM-AMT      EL523
01049                MOVE PB-I-LF-ALT-PREM-CALC                         EL523
01050                                       TO PB-I-LF-ALT-PREMIUM-AMT  EL523
01051             ELSE                                                  EL523
01052                MOVE PB-C-LF-REF-CALC TO PB-C-LF-CANCEL-AMT.       EL523
01053                                                                   EL523
01054      IF PB-OVERRIDE-AH      OR                                    EL523
01055         PB-OVERRIDE-BOTH                                          EL523
01056             IF PB-ISSUE                                           EL523
01057                MOVE PB-I-AH-PREM-CALC TO PB-I-AH-PREMIUM-AMT      EL523
01058             ELSE                                                  EL523
01059                MOVE PB-C-AH-REF-CALC  TO PB-C-AH-CANCEL-AMT.      EL523
01060                                                                   EL523
01061      MOVE SPACES                TO  REPORT-XTRACT.                EL523
01062      MOVE PB-SV-CARRIER         TO  REPT-XTR-PREFIX.              EL523
01063      MOVE PB-SV-GROUPING        TO  REPT-XTR-GROUP.               EL523
01064      MOVE PB-SV-STATE           TO  REPT-XTR-ST.                  EL523
01065      MOVE PB-ACCOUNT            TO  REPT-XTR-ACCT.                EL523
01066      MOVE '1'                   TO  REPT-XTR-REC-TYP.             EL523
01067      MOVE 'UNKNOWN ACCOUNT'     TO  REPT-XTR-NAME.                EL523
01068      MOVE ZEROS                 TO  REPT-XTR-TEL-NO.              EL523
01069                                                                   EL523
01070      IF DTE-PGM-OPT = 2                                           EL523
01071          GO TO 1210-USE-THIS-CONTROL.                             EL523
01072                                                                   EL523
01073      IF DTE-PGM-OPT = 3                                           EL523
01074          MOVE SPACES TO  REPT-XTR-GROUP                           EL523
01075          GO TO 1210-USE-THIS-CONTROL.                             EL523
01076                                                                   EL523
01077      IF DTE-COMP-VG = SPACE                                       EL523
01078          MOVE ' '    TO  REPT-XTR-PREFIX                          EL523
01079          MOVE SPACES TO  REPT-XTR-GROUP                           EL523
01080      ELSE                                                         EL523
01081          IF DTE-COMP-VG = '2'                                     EL523
01082              MOVE SPACES TO  REPT-XTR-GROUP                       EL523
01083          ELSE                                                     EL523
01084              IF DTE-COMP-VG = '3'                                 EL523
01085                  MOVE ' '    TO  REPT-XTR-PREFIX                  EL523
01086                  MOVE SPACES TO  REPT-XTR-GROUP                   EL523
01087                  MOVE '  '   TO  REPT-XTR-ST                      EL523
01088              ELSE                                                 EL523
01089                  IF DTE-COMP-VG = '4'                             EL523
01090                      MOVE SPACES TO  REPT-XTR-GROUP               EL523
01091                      MOVE '  '   TO  REPT-XTR-ST.                 EL523
01092                                                                   EL523
01093  1210-USE-THIS-CONTROL.                                           EL523
01094      PERFORM 1250-CLEAR-XTRACT THRU 1259-EXIT.                    EL523
100703
010716     IF DTE-CLIENT = 'DCC' or 'VPP'
              MOVE ' '                 TO WS-AH-CATEGORY
              IF PB-ISSUE
                 MOVE PB-I-AH-BENEFIT-CD
                                       TO CLASS-LOOK
              ELSE
                 IF PB-CANCELLATION
                    MOVE PB-CI-AH-BENEFIT-CD
                                       TO CLASS-LOOK
                 END-IF
              END-IF
              IF CLASS-LOOK NOT = '  ' AND '00'
                 PERFORM VARYING
                    CLAS-INDEXA FROM CLAS-STARTA BY +1 UNTIL
                    (CLASS-LOOK = CLAS-I-BEN (CLAS-INDEXA))
                    OR (CLAS-INDEXA > CLAS-MAXA)
                 END-PERFORM
                 IF CLASS-LOOK = CLAS-I-BEN (CLAS-INDEXA)
                    MOVE CLAS-I-BEN-CATEGORY (CLAS-INDEXA)
                                       TO WS-AH-CATEGORY
                 END-IF
              END-IF
100703     END-IF

01095                                                                   EL523
01096      IF PB-ISSUE                                                  EL523
01097          IF GOOD-PEND                                             EL523
01098              MOVE '1'  TO  REPT-GOOD-BUS                          EL523
01099              IF PB-I-OB                                           EL523
01100                  PERFORM 1260-COUNT-ISSUES THRU 1269-EXIT         EL523
01101                  IF PB-REIN-ONLY-CERT                             EL523
01102                    OR PB-REISSUED-CERT                            EL523
122002                   OR PB-MONTHLY-CERT
01103                    OR CLASIC-CREATED-CERT                         EL523
01104                    OR PB-POLICY-IS-DECLINED                       EL523
01105                    OR PB-POLICY-IS-VOIDED                         EL523
01106                      MOVE ZEROS  TO  REPT-XTR-GOOD-LF-OB          EL523
01107                                      REPT-XTR-GOOD-LF-COM         EL523
01108                                      REPT-XTR-GOOD-AH-OB          EL523
01109                                      REPT-XTR-GOOD-AH-COM         EL523
01110                  ELSE                                             EL523
01111                      MOVE PB-I-LF-PREMIUM-AMT                     EL523
01112                                  TO  REPT-XTR-GOOD-LF-OB          EL523
01113                      ADD PB-I-LF-ALT-PREMIUM-AMT                  EL523
01114                                  TO  REPT-XTR-GOOD-LF-OB          EL523
01115                      COMPUTE REPT-XTR-GOOD-LF-COM  ROUNDED        EL523
01116                                  =  REPT-XTR-GOOD-LF-OB           EL523
01117                                  *  PB-I-LIFE-COMMISSION          EL523
01118                      MOVE PB-I-AH-PREMIUM-AMT                     EL523
01119                                  TO  REPT-XTR-GOOD-AH-OB          EL523
01120                      COMPUTE REPT-XTR-GOOD-AH-COM  ROUNDED        EL523
01121                                  =  REPT-XTR-GOOD-AH-OB           EL523
01122                                  *  PB-I-AH-COMMISSION            EL523
100703                 END-IF
01123              ELSE                                                 EL523
01124                  PERFORM 1260-COUNT-ISSUES  THRU  1269-EXIT       EL523
01125                  IF PB-REIN-ONLY-CERT                             EL523
01126                    OR PB-REISSUED-CERT                            EL523
122002                   OR PB-MONTHLY-CERT
01127                    OR CLASIC-CREATED-CERT                         EL523
01128                    OR PB-POLICY-IS-DECLINED                       EL523
01129                    OR PB-POLICY-IS-VOIDED                         EL523
01130                      MOVE ZEROS  TO  REPT-XTR-GOOD-LF-AMT         EL523
01131                                      REPT-XTR-GOOD-LF-COM         EL523
01132                                      REPT-XTR-GOOD-AH-AMT         EL523
01133                                      REPT-XTR-GOOD-AH-COM         EL523
01134                  ELSE                                             EL523
100703                    IF PB-I-LF-BENEFIT-CD NOT = '  ' AND '00'
091911                       AND 'DD'
01135                        MOVE PB-I-LF-PREMIUM-AMT
01136                                  TO  REPT-XTR-GOOD-LF-AMT         EL523
01137                        ADD PB-I-LF-ALT-PREMIUM-AMT
01138                                  TO  REPT-XTR-GOOD-LF-AMT         EL523
01139                        COMPUTE REPT-XTR-GOOD-LF-COM  ROUNDED
01140                                  =  REPT-XTR-GOOD-LF-AMT          EL523
01141                                  *  PB-I-LIFE-COMMISSION          EL523
100703                    END-IF 
01142                      MOVE PB-I-AH-PREMIUM-AMT                     EL523
01143                                  TO REPT-XTR-GOOD-AH-AMT          EL523
092705                     IF WS-AH-CATEGORY = 'G' OR 'L'
100703                        COMPUTE REPT-XTR-GOOD-AH-COM =
100703                           REPT-XTR-GOOD-AH-AMT -
100703                              PB-I-LF-ALT-PREMIUM-AMT
040504                              - PB-I-ADDL-CLP
100703                     ELSE
01144                         COMPUTE REPT-XTR-GOOD-AH-COM  ROUNDED
01145                                  =  REPT-XTR-GOOD-AH-AMT          EL523
01146                                  *  PB-I-AH-COMMISSION            EL523
100703                     END-IF
100703                 END-IF
100703             END-IF
01147          ELSE                                                     EL523
01148              PERFORM 1220-CHECK-I-NUMERIC THRU 1229-EXIT
100703         END-IF
100703     END-IF
01149                                                                   EL523
01150      IF PB-CANCELLATION                                           EL523
01151          IF GOOD-PEND                                             EL523
01152              PERFORM 1270-COUNT-CANCELS THRU 1279-EXIT            EL523
01153              IF PB-CI-ENTRY-STATUS = '9' OR 'D' OR 'V'            EL523
01154                  MOVE ZEROS      TO  REPT-XTR-GOOD-LF-CAN         EL523
01155                                      REPT-XTR-GOOD-LF-COM         EL523
01156                                      REPT-XTR-GOOD-AH-CAN         EL523
01157                                      REPT-XTR-GOOD-AH-COM         EL523
01158                  MOVE '1'        TO  REPT-GOOD-BUS                EL523
01159              ELSE                                                 EL523
01160                  MOVE PB-C-LF-CANCEL-AMT                          EL523
01161                                  TO  REPT-XTR-GOOD-LF-CAN         EL523
01162                  COMPUTE REPT-XTR-GOOD-LF-COM  ROUNDED            EL523
01163                                  =  (REPT-XTR-GOOD-LF-CAN         EL523
01164                                  *  PB-CI-LIFE-COMMISSION) *  -1  EL523
01165                  MOVE PB-C-AH-CANCEL-AMT                          EL523
01166                                  TO  REPT-XTR-GOOD-AH-CAN         EL523
092705                 IF WS-AH-CATEGORY = 'G' OR 'L'
100703                    COMPUTE CNC-FACT = PB-C-AH-CANCEL-AMT / 
100703                       PB-CI-AH-PREMIUM-AMT
100703                    COMPUTE REPT-XTR-GOOD-AH-COM ROUNDED =
100703                    (CNC-FACT * (PB-CI-AH-PREMIUM-AMT - 
100703                       PB-CI-LF-ALT-PREMIUM-AMT
040504                       - PB-CI-ADDL-CLP)) * -1
100703                 ELSE
01167                     COMPUTE REPT-XTR-GOOD-AH-COM  ROUNDED
01168                                  =  (REPT-XTR-GOOD-AH-CAN         EL523
01169                                  *  PB-CI-AH-COMMISSION) *  -1    EL523
100703                 END-IF
01170                  MOVE '1'        TO  REPT-GOOD-BUS                EL523
                   END-IF
01171          ELSE                                                     EL523
01172              PERFORM 1230-CHECK-C-NUMERIC THRU 1239-EXIT
               END-IF
           END-IF
01173                                                                   EL523
01174      IF PB-ISSUE                                                  EL523
01175          IF PB-REIN-ONLY-CERT                                     EL523
01176            OR PB-REISSUED-CERT                                    EL523
122002           OR PB-MONTHLY-CERT
01177            OR CLASIC-CREATED-CERT                                 EL523
01178            OR PB-POLICY-IS-DECLINED                               EL523
01179            OR PB-POLICY-IS-VOIDED                                 EL523
01180              MOVE ZEROS          TO  REPT-XTR-EFF-DATE            EL523
01181              GO TO 1215-CONTINUE.                                 EL523
01182                                                                   EL523
01183      IF PB-ISSUE                                                  EL523
01184          IF PB-CERT-EFF-DT = LOW-VALUES OR SPACES                 EL523
01185              MOVE ZEROS               TO  REPT-XTR-EFF-DATE       EL523
01186          ELSE                                                     EL523
01187              MOVE ' '                 TO  DC-OPTION-CODE          EL523
01188              MOVE PB-CERT-EFF-DT      TO  DC-BIN-DATE-1           EL523
01189              PERFORM 8500-DATE-CONVERSION THRU 8590-EXIT          EL523
01190              MOVE DC-ALPHA-CEN-N      TO  REPT-XTR-EFF-CC         EL523
01191              MOVE DC-YMD-YEAR         TO  REPT-XTR-EFF-YR         EL523
01192              MOVE DC-YMD-MONTH        TO  REPT-XTR-EFF-MO         EL523
01193              MOVE DC-YMD-DAY          TO  REPT-XTR-EFF-DA         EL523
01194              MOVE WS-REPT-XTR-EFF-DATE-N TO REPT-XTR-EFF-DATE        CL*10
01195      ELSE                                                         EL523
01196          MOVE ZEROS                   TO  REPT-XTR-EFF-DATE.      EL523
01197                                                                   EL523
01198  1215-CONTINUE.                                                   EL523
01199      MOVE '0'  TO  PEND-SW.                                       EL523
01200                                                                   EL523
01201      PERFORM 1240-RELEASE-SORT-REC THRU 1249-EXIT.                EL523
01202                                                                   EL523
01203      GO TO 1200-READ-PENDING.                                     EL523
01204                                                                   EL523
01205  1220-CHECK-I-NUMERIC.                                            EL523
01206      IF PB-I-OB                                                   EL523
01207          MOVE +1                 TO  REPT-XTR-BAD-OB-I-CNT        EL523
01208      ELSE                                                         EL523
01209          MOVE +1                 TO  REPT-XTR-BAD-SP-I-CNT.       EL523
01210                                                                   EL523
01211      IF PB-I-LF-PREMIUM-AMT  NUMERIC                              EL523
01212        AND PB-I-LF-ALT-PREMIUM-AMT  NUMERIC                       EL523
01213        AND PB-I-LIFE-COMMISSION  NUMERIC                          EL523
01214          IF PB-I-LF-PREMIUM-AMT  NOT =  ZEROS                     EL523
01215            OR PB-I-LF-ALT-PREMIUM-AMT  NOT =  ZEROS               EL523
01216              IF PB-I-OB                                           EL523
01217                  MOVE +1         TO  REPT-XTR-BAD-LF-OB-CNT       EL523
01218                  IF PB-REIN-ONLY-CERT                             EL523
01219                    OR PB-REISSUED-CERT                            EL523
122002                   OR PB-MONTHLY-CERT
01220                    OR CLASIC-CREATED-CERT                         EL523
01221                    OR PB-POLICY-IS-DECLINED                       EL523
01222                    OR PB-POLICY-IS-VOIDED                         EL523
01223                      MOVE ZEROS  TO  REPT-XTR-BAD-LF-OB           EL523
01224                                      REPT-XTR-BAD-LF-COM          EL523
01225                  ELSE                                             EL523
01226                      MOVE PB-I-LF-PREMIUM-AMT                     EL523
01227                                  TO  REPT-XTR-BAD-LF-OB           EL523
01228                      ADD PB-I-LF-ALT-PREMIUM-AMT                  EL523
01229                                  TO  REPT-XTR-BAD-LF-OB           EL523
01230                      COMPUTE REPT-XTR-BAD-LF-COM  ROUNDED         EL523
01231                                  =  REPT-XTR-BAD-LF-OB            EL523
01232                                  *  PB-I-LIFE-COMMISSION          EL523
100703                 END-IF
01233              ELSE                                                 EL523
100703                IF PB-I-LF-BENEFIT-CD NOT = '  ' AND '00'
091911                   AND 'DD'
01234                    MOVE +1       TO  REPT-XTR-BAD-LF-I-CNT
100703                END-IF
01235                  IF PB-REIN-ONLY-CERT                             EL523
01236                    OR PB-REISSUED-CERT                            EL523
122002                   OR PB-MONTHLY-CERT
01237                    OR CLASIC-CREATED-CERT                         EL523
01238                    OR PB-POLICY-IS-DECLINED                       EL523
01239                    OR PB-POLICY-IS-VOIDED                         EL523
01240                      MOVE ZEROS  TO  REPT-XTR-BAD-LF-AMT          EL523
01241                                      REPT-XTR-BAD-LF-COM          EL523
01242                  ELSE                                             EL523
100703                    IF PB-I-LF-BENEFIT-CD NOT = '00' AND '  '
091911                       AND 'DD'
01243                        MOVE PB-I-LF-PREMIUM-AMT
01244                                  TO  REPT-XTR-BAD-LF-AMT          EL523
01245                        ADD PB-I-LF-ALT-PREMIUM-AMT
01246                                  TO  REPT-XTR-BAD-LF-AMT          EL523
01247                        COMPUTE REPT-XTR-BAD-LF-COM  ROUNDED
01248                                  =  REPT-XTR-BAD-LF-AMT           EL523
01249                                  *  PB-I-LIFE-COMMISSION          EL523
100703                    END-IF
100703                 END-IF
100703             END-IF
01250          ELSE                                                     EL523
01251              MOVE ZEROS          TO  REPT-XTR-BAD-LF-OB-CNT       EL523
01252                                      REPT-XTR-BAD-LF-I-CNT        EL523
01253                                      REPT-XTR-BAD-LF-COM          EL523
01254                                      REPT-XTR-BAD-LF-OB           EL523
01255                                      REPT-XTR-BAD-LF-AMT          EL523
100703         END-IF
01256      ELSE                                                         EL523
01257          MOVE ZEROS              TO  REPT-XTR-BAD-LF-OB-CNT       EL523
01258                                      REPT-XTR-BAD-LF-I-CNT        EL523
01259                                      REPT-XTR-BAD-LF-COM          EL523
01260                                      REPT-XTR-BAD-LF-OB           EL523
01261                                      REPT-XTR-BAD-LF-AMT
100703     END-IF
01262                                                                   EL523
01263      IF PB-I-AH-PREMIUM-AMT  NUMERIC                              EL523
01264        AND PB-I-AH-COMMISSION  NUMERIC                            EL523
01265          IF PB-I-AH-PREMIUM-AMT  NOT =  ZEROS                     EL523
01266              IF PB-I-OB                                           EL523
01267                  MOVE +1          TO  REPT-XTR-BAD-AH-OB-CNT      EL523
01268                  IF PB-REIN-ONLY-CERT                             EL523
01269                    OR PB-REISSUED-CERT                            EL523
122002                   OR PB-MONTHLY-CERT
01270                    OR CLASIC-CREATED-CERT                         EL523
01271                    OR PB-POLICY-IS-DECLINED                       EL523
01272                    OR PB-POLICY-IS-VOIDED                         EL523
01273                      MOVE ZEROS  TO  REPT-XTR-BAD-AH-OB           EL523
01274                                      REPT-XTR-BAD-AH-COM          EL523
01275                  ELSE                                             EL523
01276                      MOVE PB-I-AH-PREMIUM-AMT                     EL523
01277                                  TO  REPT-XTR-BAD-AH-OB           EL523
01278                      COMPUTE REPT-XTR-BAD-AH-COM  ROUNDED         EL523
01279                                  =  REPT-XTR-BAD-AH-OB            EL523
01280                                  *  PB-I-AH-COMMISSION            EL523
100703                 END-IF
01281              ELSE                                                 EL523
01282                  MOVE +1         TO  REPT-XTR-BAD-AH-I-CNT        EL523
01283                  IF PB-REIN-ONLY-CERT                             EL523
01284                    OR PB-REISSUED-CERT                            EL523
122002                   OR PB-MONTHLY-CERT
01285                    OR CLASIC-CREATED-CERT                         EL523
01286                    OR PB-POLICY-IS-DECLINED                       EL523
01287                    OR PB-POLICY-IS-VOIDED                         EL523
01288                      MOVE ZEROS  TO  REPT-XTR-BAD-AH-AMT          EL523
01289                                      REPT-XTR-BAD-AH-COM          EL523
01290                  ELSE                                             EL523
01291                      MOVE PB-I-AH-PREMIUM-AMT                     EL523
01292                                  TO  REPT-XTR-BAD-AH-AMT          EL523
092705                     IF WS-AH-CATEGORY = 'G' OR 'L'
100703                        COMPUTE REPT-XTR-BAD-AH-COM =
100703                           REPT-XTR-BAD-AH-AMT -
100703                             PB-I-LF-ALT-PREMIUM-AMT
                                    - PB-I-ADDL-CLP
100703                     ELSE
01293                         COMPUTE REPT-XTR-BAD-AH-COM  ROUNDED
01294                                  =  REPT-XTR-BAD-AH-AMT           EL523
01295                                  *  PB-I-AH-COMMISSION            EL523
100703                     END-IF
10703                  END-IF
100703             END-IF
01296          ELSE                                                     EL523
01297              MOVE ZEROS          TO  REPT-XTR-BAD-AH-OB-CNT       EL523
01298                                      REPT-XTR-BAD-AH-I-CNT        EL523
01299                                      REPT-XTR-BAD-AH-COM          EL523
01300                                      REPT-XTR-BAD-AH-OB           EL523
01301                                      REPT-XTR-BAD-AH-AMT          EL523
100703         END-IF
01302      ELSE                                                         EL523
01303          MOVE ZEROS              TO  REPT-XTR-BAD-AH-OB-CNT       EL523
01304                                      REPT-XTR-BAD-AH-I-CNT        EL523
01305                                      REPT-XTR-BAD-AH-COM          EL523
01306                                      REPT-XTR-BAD-AH-OB           EL523
01307                                      REPT-XTR-BAD-AH-AMT
100703     END-IF
01308                                                                   EL523
01309      MOVE '1'  TO  REPT-BAD-BUS.                                  EL523
01310                                                                   EL523
01311  1229-EXIT.                                                       EL523
01312      EXIT.                                                        EL523
01313                                                                   EL523
01314  1230-CHECK-C-NUMERIC.                                            EL523
01315      MOVE +1                     TO  REPT-XTR-BAD-C-CNT.          EL523
01316                                                                   EL523
01317      IF PB-C-LF-CANCEL-AMT  NUMERIC                               EL523
01318        AND PB-CI-LIFE-COMMISSION  NUMERIC                         EL523
01319          IF PB-C-LF-CANCEL-AMT  NOT =  ZEROS                      EL523
01320              MOVE +1             TO  REPT-XTR-BAD-LF-C-CNT        EL523
060613             IF PB-CI-ENTRY-STATUS = '9'
01322                  MOVE ZEROS      TO  REPT-XTR-BAD-LF-CAN          EL523
01323                                      REPT-XTR-BAD-LF-COM          EL523
01324              ELSE                                                 EL523
01325                  MOVE PB-C-LF-CANCEL-AMT                          EL523
01326                                  TO  REPT-XTR-BAD-LF-CAN          EL523
01327                  COMPUTE REPT-XTR-BAD-LF-COM  ROUNDED             EL523
01328                                  =  (REPT-XTR-BAD-LF-CAN          EL523
01329                                  *  PB-CI-LIFE-COMMISSION)  *  -1 EL523
01330          ELSE                                                     EL523
01331              MOVE ZEROS          TO  REPT-XTR-BAD-LF-C-CNT        EL523
01332                                      REPT-XTR-BAD-LF-COM          EL523
01333                                      REPT-XTR-BAD-LF-CAN          EL523
01334      ELSE                                                         EL523
01335          MOVE ZEROS              TO  REPT-XTR-BAD-LF-C-CNT        EL523
01336                                      REPT-XTR-BAD-LF-COM          EL523
01337                                      REPT-XTR-BAD-LF-CAN.         EL523
01338                                                                   EL523
01339      IF PB-C-AH-CANCEL-AMT  NUMERIC                               EL523
01340        AND PB-CI-AH-COMMISSION  NUMERIC                           EL523
01341          IF PB-C-AH-CANCEL-AMT  NOT =  ZEROS                      EL523
01342              MOVE +1             TO  REPT-XTR-BAD-AH-C-CNT        EL523
060613             IF PB-CI-ENTRY-STATUS = '9'
01344                  MOVE ZEROS      TO  REPT-XTR-BAD-AH-CAN          EL523
01345                                      REPT-XTR-BAD-AH-COM          EL523
01346              ELSE                                                 EL523
01347                  MOVE PB-C-AH-CANCEL-AMT                          EL523
01348                                  TO  REPT-XTR-BAD-AH-CAN          EL523
092705                 IF WS-AH-CATEGORY = 'G' OR 'L'
100703                    COMPUTE CNC-FACT = PB-C-AH-CANCEL-AMT / 
100703                       PB-CI-AH-PREMIUM-AMT
100703                    COMPUTE REPT-XTR-BAD-AH-COM ROUNDED =
100703                    (CNC-FACT * (PB-CI-AH-PREMIUM-AMT - 
100703                       PB-CI-LF-ALT-PREMIUM-AMT
040504                       - PB-CI-ADDL-CLP)) * -1
100703                 ELSE
01349                     COMPUTE REPT-XTR-BAD-AH-COM  ROUNDED
01350                                  =  (REPT-XTR-BAD-AH-CAN          EL523
01351                                  *  PB-CI-AH-COMMISSION)  *  -1   EL523
100703                 END-IF
100703             END-IF
01352          ELSE                                                     EL523
01353              MOVE ZEROS          TO  REPT-XTR-BAD-AH-C-CNT        EL523
01354                                      REPT-XTR-BAD-AH-COM          EL523
01355                                      REPT-XTR-BAD-AH-CAN          EL523
100703         END-IF
01356      ELSE                                                         EL523
01357          MOVE ZEROS              TO  REPT-XTR-BAD-AH-C-CNT        EL523
01358                                      REPT-XTR-BAD-AH-COM          EL523
01359                                      REPT-XTR-BAD-AH-CAN
100703     END-IF
01360                                                                   EL523
01361      MOVE '1'                    TO  REPT-BAD-BUS.                EL523
01362                                                                   EL523
01363  1239-EXIT.                                                       EL523
01364      EXIT.                                                        EL523
01365                                                                   EL523
01366  1240-RELEASE-SORT-REC.                                           EL523
01367      RELEASE SORT-REC1 FROM REPORT-XTRACT.                        EL523
01368                                                                   EL523
01369  1249-EXIT.                                                       EL523
01370      EXIT.                                                        EL523
01371                                                                   EL523
01372  1250-CLEAR-XTRACT.                                               EL523
01373      MOVE ZEROS  TO  REPT-XTR-GOOD-LF-AMT  REPT-XTR-GOOD-AH-AMT   EL523
01374                      REPT-XTR-GOOD-LF-OB   REPT-XTR-GOOD-AH-OB    EL523
01375                      REPT-XTR-GOOD-LF-CAN  REPT-XTR-GOOD-AH-CAN   EL523
01376                      REPT-XTR-GOOD-LF-COM  REPT-XTR-GOOD-AH-COM   EL523
01377                      REPT-XTR-BAD-LF-AMT   REPT-XTR-BAD-AH-AMT    EL523
01378                      REPT-XTR-BAD-LF-OB    REPT-XTR-BAD-AH-OB     EL523
01379                      REPT-XTR-BAD-LF-CAN   REPT-XTR-BAD-AH-CAN    EL523
01380                      REPT-XTR-BAD-LF-COM   REPT-XTR-BAD-AH-COM    EL523
01381                      REPT-XTR-GOOD-LF-I-CNT                       EL523
01382                      REPT-XTR-GOOD-LF-OB-CNT                      EL523
01383                      REPT-XTR-GOOD-LF-C-CNT                       EL523
01384                      REPT-XTR-GOOD-AH-I-CNT                       EL523
01385                      REPT-XTR-GOOD-AH-OB-CNT                      EL523
01386                      REPT-XTR-GOOD-AH-C-CNT                       EL523
01387                      REPT-XTR-BAD-LF-I-CNT                        EL523
01388                      REPT-XTR-BAD-LF-OB-CNT                       EL523
01389                      REPT-XTR-BAD-LF-C-CNT                        EL523
01390                      REPT-XTR-BAD-AH-I-CNT                        EL523
01391                      REPT-XTR-BAD-AH-OB-CNT                       EL523
01392                      REPT-XTR-BAD-AH-C-CNT                        EL523
01393                      REPT-XTR-EFF-DATE                            EL523
01394                      REPT-XTR-GOOD-SP-I-CNT REPT-XTR-BAD-SP-I-CNT EL523
01395                      REPT-XTR-GOOD-OB-I-CNT REPT-XTR-BAD-OB-I-CNT EL523
01396                      REPT-XTR-GOOD-C-CNT    REPT-XTR-BAD-C-CNT.   EL523
01397                                                                   EL523
01398      MOVE SPACES TO  REPT-GOOD-BUS                                EL523
01399                      REPT-BAD-BUS.                                EL523
01400                                                                   EL523
01401  1259-EXIT.                                                       EL523
01402      EXIT.                                                        EL523
01403                                                                   EL523
01404  1260-COUNT-ISSUES.                                               EL523
01405      IF PB-I-OB                                                   EL523
01406          GO TO 1265-OB-COUNTS.                                    EL523
01407                                                                   EL523
01408      MOVE +1 TO REPT-XTR-GOOD-SP-I-CNT.                           EL523
01409                                                                   EL523
091911     IF PB-I-LF-BENEFIT-CD NOT = '  ' AND '00' AND 'DD'
01410         IF (PB-I-LF-PREMIUM-AMT NOT = ZEROS)
01411            OR (PB-I-LF-ALT-PREMIUM-AMT NOT = ZEROS)
01412            MOVE +1  TO  REPT-XTR-GOOD-LF-I-CNT
100703        END-IF
100703     END-IF
01413                                                                   EL523
01414      IF PB-I-AH-PREMIUM-AMT NOT = ZEROS                           EL523
01415           MOVE +1  TO  REPT-XTR-GOOD-AH-I-CNT.                    EL523
01416                                                                   EL523
01417      GO TO 1269-EXIT.                                             EL523
01418                                                                   EL523
01419  1265-OB-COUNTS.                                                  EL523
01420      MOVE +1 TO REPT-XTR-GOOD-OB-I-CNT.                           EL523
01421                                                                   EL523
01422      IF PB-I-LF-PREMIUM-AMT NOT = ZEROS                           EL523
01423        OR PB-I-LF-ALT-PREMIUM-AMT NOT = ZEROS                     EL523
01424          MOVE +1  TO  REPT-XTR-GOOD-LF-OB-CNT.                    EL523
01425                                                                   EL523
01426      IF PB-I-AH-PREMIUM-AMT NOT = ZEROS                           EL523
01427          MOVE +1  TO  REPT-XTR-GOOD-AH-OB-CNT.                    EL523
01428                                                                   EL523
01429  1269-EXIT.                                                       EL523
01430      EXIT.                                                        EL523
01431                                                                   EL523
01432  1270-COUNT-CANCELS.                                              EL523
01433      MOVE +1 TO REPT-XTR-GOOD-C-CNT.                              EL523
01434                                                                   EL523
01435      IF PB-C-LF-CANCEL-AMT NOT = ZEROS                            EL523
01436          MOVE +1  TO  REPT-XTR-GOOD-LF-C-CNT.                     EL523
01437                                                                   EL523
01438      IF PB-C-AH-CANCEL-AMT NOT = ZEROS                            EL523
01439          MOVE +1  TO  REPT-XTR-GOOD-AH-C-CNT.                     EL523
01440                                                                   EL523
01441  1279-EXIT.                                                       EL523
01442      EXIT.                                                        EL523
01443                                                                   EL523
01444  1299-EXIT.                                                       EL523
01445      EXIT.                                                        EL523
01446  EJECT                                                            EL523
01447  1300-REPORT-PRINT SECTION.                                       EL523
01448      MOVE LOW-VALUES  TO  PREV-CARRIER  PREV-STATE  ACT-CONTROL.  EL523
01449                                                                   EL523
01450  1400-RETURN-SORT-REC.                                            EL523
01451      RETURN SORT1-FILE INTO REPORT-XTRACT AT END                  EL523
01452          MOVE HIGH-VALUES TO REPORT-XTRACT.                       EL523
01453                                                                   EL523
01454      IF REPT-HEADER-RECORD                                        EL523
01455          MOVE REPT-XTR-REPORT-CODE-2  TO WS-SAVE-RPT2.            EL523
01456                                                                   EL523
01457      IF ACT-CONTROL = LOW-VALUES                                  EL523
01458          PERFORM 1500-BUILD-ACT THRU 1599-EXIT                    EL523
01459          GO TO 1400-RETURN-SORT-REC.                              EL523
01460                                                                   EL523
01461      IF REPT-XTR-CONTROL = ACT-CONTROL                            EL523
01462          PERFORM 1700-BUILD-ACTIVE THRU 1799-EXIT                 EL523
01463          GO TO 1400-RETURN-SORT-REC.                              EL523
01464                                                                   EL523
01465      IF NO-DATES                                                  EL523
01466          MOVE '2'           TO  ACT-DATE-FLAG                     EL523
01467          MOVE ZEROS         TO  ACT-LO-DATE                       EL523
01468                                 ACT-HI-DATE                       EL523
01469      ELSE                                                         EL523
01470          MOVE '1'           TO  ACT-DATE-FLAG                     EL523
01471          MOVE HOLD-LO-DATE  TO  ACT-LO-DATE                       EL523
01472          MOVE HOLD-HI-DATE  TO  ACT-HI-DATE.                      EL523
01473                                                                   EL523
01474      IF DTE-CLIENT = 'MON'                                        EL523
01475          MOVE WS-SAVE-RPT2            TO ACT-RPT2                 EL523
01476      ELSE                                                         EL523
01477          MOVE SPACES                  TO ACT-RPT2.                EL523
01478                                                                   EL523
01479      WRITE ACCT-X-REC FROM ACT-RECORD.                            EL523
01480                                                                   EL523
01481      IF REPT-XTR-CONTROL = HIGH-VALUES                            EL523
01482          GO TO 1900-END-PHASE-2.                                  EL523
01483                                                                   EL523
01484      MOVE LOW-VALUES  TO  ACT-CONTROL.                            EL523
01485      MOVE '0'         TO  DATES-SW  FIRST-DAT-SW.                 EL523
01486                                                                   EL523
01487      PERFORM 1600-BUILD-NON-ACTIVE THRU 1699-EXIT.                EL523
01488      PERFORM 1500-BUILD-ACT        THRU 1599-EXIT.                EL523
01489                                                                   EL523
01490      GO TO 1400-RETURN-SORT-REC.                                  EL523
01491                                                                   EL523
01492  1500-BUILD-ACT.                                                  EL523
01493      IF REPT-HEADER-RECORD OR                                     EL523
01494         ACT-CONTROL = LOW-VALUES                                  EL523
01495          NEXT SENTENCE                                            EL523
01496      ELSE                                                         EL523
01497          GO TO 1510-ADD-REPT-TO-ACT.                              EL523
01498                                                                   EL523
01499      MOVE REPT-XTR-CONTROL       TO  ACT-CONTROL.                 EL523
01500                                                                   EL523
01501      IF DTE-CLIENT = 'MON'                                        EL523
01502          MOVE REPT-XTR-REPORT-CODE-2  TO ACT-RPT2                 EL523
01503      ELSE                                                         EL523
01504          MOVE SPACES                  TO ACT-RPT2.                EL523
01505                                                                   EL523
01506      MOVE REPT-XTR-NAME          TO  ACT-NAME.                    EL523
01507      MOVE REPT-XTR-AM-EFF-DT     TO  ACT-EFFECTIVE-DT.               CL**7
01508      MOVE REPT-XTR-AM-HI-DT      TO  ACT-AM-HI-DT                 EL523
01509                                      WS-ACT-AM-HI-DT.                CL**7
01510      MOVE REPT-XTR-AM-LO-DT      TO  ACT-AM-LO-DT.                   CL**7
01511      MOVE REPT-XTR-TEL-NO        TO  ACT-TEL-NO.                  EL523
01512      MOVE REPT-ACTIVE            TO  ACT-ACTIVE.                  EL523
01513                                                                   EL523
01514  1510-ADD-REPT-TO-ACT.                                            EL523
01515      IF REPT-HEADER-RECORD                                        EL523
01516          PERFORM 1600-BUILD-NON-ACTIVE THRU 1699-EXIT             EL523
01517          GO TO 1599-EXIT.                                         EL523
01518                                                                   EL523
01519      PERFORM 1700-BUILD-ACTIVE THRU 1799-EXIT.                    EL523
01520                                                                   EL523
01521  1599-EXIT.                                                       EL523
01522      EXIT.                                                        EL523
01523                                                                   EL523
01524  1600-BUILD-NON-ACTIVE.                                           EL523
01525      MOVE ZEROS  TO  ACT-GOOD-LF-AMT     ACT-GOOD-LF-OB           EL523
01526                      ACT-GOOD-LF-CAN     ACT-GOOD-LF-NET          EL523
01527                      ACT-GOOD-AH-AMT     ACT-GOOD-AH-OB           EL523
01528                      ACT-GOOD-AH-CAN     ACT-GOOD-AH-NET          EL523
01529                      ACT-GOOD-LF-COM     ACT-GOOD-AH-COM          EL523
01530                      ACT-GOOD-LF-I-CNT   ACT-GOOD-LF-OB-CNT       EL523
01531                      ACT-GOOD-LF-C-CNT   ACT-GOOD-AH-I-CNT        EL523
01532                      ACT-GOOD-AH-OB-CNT  ACT-GOOD-AH-C-CNT        EL523
01533                      ACT-BAD-LF-AMT      ACT-BAD-LF-OB            EL523
01534                      ACT-BAD-LF-CAN      ACT-BAD-LF-NET           EL523
01535                      ACT-BAD-AH-AMT      ACT-BAD-AH-OB            EL523
01536                      ACT-BAD-AH-CAN      ACT-BAD-AH-NET           EL523
01537                      ACT-BAD-LF-COM      ACT-BAD-AH-COM           EL523
01538                      ACT-BAD-LF-I-CNT    ACT-BAD-LF-OB-CNT        EL523
01539                      ACT-BAD-LF-C-CNT    ACT-BAD-AH-I-CNT         EL523
01540                      ACT-BAD-AH-OB-CNT   ACT-BAD-AH-C-CNT         EL523
01541                      ACT-ALL-LF-AMT      ACT-ALL-LF-OB            EL523
01542                      ACT-ALL-LF-CAN      ACT-ALL-LF-NET           EL523
01543                      ACT-ALL-AH-AMT      ACT-ALL-AH-OB            EL523
01544                      ACT-ALL-AH-CAN      ACT-ALL-AH-NET           EL523
01545                      ACT-ALL-LF-COM      ACT-ALL-AH-COM           EL523
01546                      ACT-ALL-LF-I-CNT    ACT-ALL-LF-OB-CNT        EL523
01547                      ACT-ALL-LF-C-CNT    ACT-ALL-AH-I-CNT         EL523
01548                      ACT-ALL-AH-OB-CNT   ACT-ALL-AH-C-CNT         EL523
01549                      ACT-LO-DATE         ACT-HI-DATE              EL523
01550                      ACT-GOOD-SP-I-CNT  ACT-GOOD-OB-I-CNT         EL523
01551                      ACT-GOOD-C-CNT     ACT-BAD-SP-I-CNT          EL523
01552                      ACT-BAD-OB-I-CNT   ACT-BAD-C-CNT.            EL523
01553                                                                   EL523
01554      MOVE '2'    TO  ACT-BUSINESS-FLAG                            EL523
01555                      ACT-DATE-FLAG.                               EL523
01556                                                                   EL523
01557  1699-EXIT.                                                       EL523
01558      EXIT.                                                        EL523
01559                                                                   EL523
01560  1700-BUILD-ACTIVE.                                               EL523
01561      IF REPT-HEADER-RECORD                                        EL523
01562          MOVE '2'  TO  ACT-BUSINESS-FLAG                          EL523
01563      ELSE                                                         EL523
01564          MOVE '1'  TO  ACT-BUSINESS-FLAG.                         EL523
01565                                                                   EL523
01566      IF REPT-XTR-EFF-DATE = ZEROS                                 EL523
01567          GO TO 1710-CONTINUE.                                     EL523
01568                                                                   EL523
01569      IF FIRST-DAT                                                 EL523
01570          MOVE '1'                TO  DATES-SW                     EL523
01571                                      FIRST-DAT-SW                 EL523
01572          MOVE REPT-XTR-EFF-DATE  TO  HOLD-LO-DATE  HOLD-HI-DATE   EL523
01573      ELSE                                                         EL523
01574          PERFORM 1800-SET-HI-LO THRU 1899-EXIT.                   EL523
01575                                                                   EL523
01576  1710-CONTINUE.                                                   EL523
01577      COMPUTE  ACT-GOOD-LF-AMT  =  ACT-GOOD-LF-AMT                 EL523
01578                                +  REPT-XTR-GOOD-LF-AMT.           EL523
01579      COMPUTE  ACT-GOOD-LF-OB   =  ACT-GOOD-LF-OB                  EL523
01580                                +  REPT-XTR-GOOD-LF-OB.            EL523
01581      COMPUTE  ACT-GOOD-LF-CAN  =  ACT-GOOD-LF-CAN                 EL523
01582                                +  REPT-XTR-GOOD-LF-CAN.           EL523
01583      COMPUTE  ACT-GOOD-LF-NET  =  ACT-GOOD-LF-AMT                 EL523
01584                                +  ACT-GOOD-LF-OB                  EL523
01585                                -  ACT-GOOD-LF-CAN.                EL523
01586      COMPUTE  ACT-GOOD-LF-COM  =  ACT-GOOD-LF-COM                 EL523
01587                                +  REPT-XTR-GOOD-LF-COM.           EL523
01588                                                                   EL523
01589      COMPUTE  ACT-GOOD-SP-I-CNT   =  ACT-GOOD-SP-I-CNT            EL523
01590                                   +  REPT-XTR-GOOD-SP-I-CNT.      EL523
01591      COMPUTE  ACT-GOOD-OB-I-CNT   =  ACT-GOOD-OB-I-CNT            EL523
01592                                   +  REPT-XTR-GOOD-OB-I-CNT.      EL523
01593      COMPUTE  ACT-GOOD-C-CNT      =  ACT-GOOD-C-CNT               EL523
01594                                   +  REPT-XTR-GOOD-C-CNT.         EL523
01595      COMPUTE  ACT-GOOD-LF-I-CNT   =  ACT-GOOD-LF-I-CNT            EL523
01596                                   +  REPT-XTR-GOOD-LF-I-CNT.      EL523
01597      COMPUTE  ACT-GOOD-LF-OB-CNT  =  ACT-GOOD-LF-OB-CNT           EL523
01598                                   +  REPT-XTR-GOOD-LF-OB-CNT.     EL523
01599      COMPUTE  ACT-GOOD-LF-C-CNT   =  ACT-GOOD-LF-C-CNT            EL523
01600                                   +  REPT-XTR-GOOD-LF-C-CNT.      EL523
01601                                                                   EL523
01602      COMPUTE  ACT-GOOD-AH-AMT  =  ACT-GOOD-AH-AMT                 EL523
01603                                +  REPT-XTR-GOOD-AH-AMT.           EL523
01604      COMPUTE  ACT-GOOD-AH-OB   =  ACT-GOOD-AH-OB                  EL523
01605                                +  REPT-XTR-GOOD-AH-OB.            EL523
01606      COMPUTE  ACT-GOOD-AH-CAN  =  ACT-GOOD-AH-CAN                 EL523
01607                                +  REPT-XTR-GOOD-AH-CAN.           EL523
01608      COMPUTE  ACT-GOOD-AH-NET  =  ACT-GOOD-AH-AMT                 EL523
01609                                +  ACT-GOOD-AH-OB                  EL523
01610                                -  ACT-GOOD-AH-CAN.                EL523
01611      COMPUTE  ACT-GOOD-AH-COM  =  ACT-GOOD-AH-COM                 EL523
01612                                +  REPT-XTR-GOOD-AH-COM.           EL523
01613                                                                   EL523
01614      COMPUTE  ACT-GOOD-AH-I-CNT   =  ACT-GOOD-AH-I-CNT            EL523
01615                                   +  REPT-XTR-GOOD-AH-I-CNT.      EL523
01616      COMPUTE  ACT-GOOD-AH-OB-CNT  =  ACT-GOOD-AH-OB-CNT           EL523
01617                                   +  REPT-XTR-GOOD-AH-OB-CNT.     EL523
01618      COMPUTE  ACT-GOOD-AH-C-CNT   =  ACT-GOOD-AH-C-CNT            EL523
01619                                   +  REPT-XTR-GOOD-AH-C-CNT.      EL523
01620                                                                   EL523
01621  1720-CONTINUE.                                                   EL523
01622      COMPUTE  ACT-BAD-LF-AMT  =  ACT-BAD-LF-AMT                   EL523
01623                               +  REPT-XTR-BAD-LF-AMT.             EL523
01624      COMPUTE  ACT-BAD-LF-OB   =  ACT-BAD-LF-OB                    EL523
01625                               +  REPT-XTR-BAD-LF-OB.              EL523
01626      COMPUTE  ACT-BAD-LF-CAN  =  ACT-BAD-LF-CAN                   EL523
01627                               +  REPT-XTR-BAD-LF-CAN.             EL523
01628      COMPUTE  ACT-BAD-LF-NET  =  ACT-BAD-LF-AMT                   EL523
01629                               +  ACT-BAD-LF-OB                    EL523
01630                               -  ACT-BAD-LF-CAN.                  EL523
01631      COMPUTE  ACT-BAD-LF-COM  =  ACT-BAD-LF-COM                   EL523
01632                               +  REPT-XTR-BAD-LF-COM.             EL523
01633                                                                   EL523
01634      COMPUTE  ACT-BAD-SP-I-CNT   =  ACT-BAD-SP-I-CNT              EL523
01635                                  +  REPT-XTR-BAD-SP-I-CNT.        EL523
01636      COMPUTE  ACT-BAD-OB-I-CNT   =  ACT-BAD-OB-I-CNT              EL523
01637                                  +  REPT-XTR-BAD-OB-I-CNT.        EL523
01638      COMPUTE  ACT-BAD-C-CNT      =  ACT-BAD-C-CNT                 EL523
01639                                  +  REPT-XTR-BAD-C-CNT.           EL523
01640      COMPUTE  ACT-BAD-LF-I-CNT   =  ACT-BAD-LF-I-CNT              EL523
01641                                  +  REPT-XTR-BAD-LF-I-CNT.        EL523
01642      COMPUTE  ACT-BAD-LF-OB-CNT  =  ACT-BAD-LF-OB-CNT             EL523
01643                                  +  REPT-XTR-BAD-LF-OB-CNT.       EL523
01644      COMPUTE  ACT-BAD-LF-C-CNT   =  ACT-BAD-LF-C-CNT              EL523
01645                                  +  REPT-XTR-BAD-LF-C-CNT.        EL523
01646                                                                   EL523
01647      COMPUTE  ACT-BAD-AH-AMT  =  ACT-BAD-AH-AMT                   EL523
01648                               +  REPT-XTR-BAD-AH-AMT.             EL523
01649      COMPUTE  ACT-BAD-AH-OB   =  ACT-BAD-AH-OB                    EL523
01650                               +  REPT-XTR-BAD-AH-OB.              EL523
01651      COMPUTE  ACT-BAD-AH-CAN  =  ACT-BAD-AH-CAN                   EL523
01652                               +  REPT-XTR-BAD-AH-CAN.             EL523
01653      COMPUTE  ACT-BAD-AH-NET  =  ACT-BAD-AH-AMT                   EL523
01654                               +  ACT-BAD-AH-OB                    EL523
01655                               -  ACT-BAD-AH-CAN.                  EL523
01656      COMPUTE  ACT-BAD-AH-COM  =  ACT-BAD-AH-COM                   EL523
01657                               +  REPT-XTR-BAD-AH-COM.             EL523
01658                                                                   EL523
01659      COMPUTE  ACT-BAD-AH-I-CNT   =  ACT-BAD-AH-I-CNT              EL523
01660                                  +  REPT-XTR-BAD-AH-I-CNT.        EL523
01661      COMPUTE  ACT-BAD-AH-OB-CNT  =  ACT-BAD-AH-OB-CNT             EL523
01662                                  +  REPT-XTR-BAD-AH-OB-CNT.       EL523
01663      COMPUTE  ACT-BAD-AH-C-CNT   =  ACT-BAD-AH-C-CNT              EL523
01664                                  +  REPT-XTR-BAD-AH-C-CNT.        EL523
01665                                                                   EL523
01666      COMPUTE  ACT-ALL-LF-AMT  =  ACT-ALL-LF-AMT                   EL523
01667                               +  REPT-XTR-GOOD-LF-AMT             EL523
01668                               +  REPT-XTR-BAD-LF-AMT.             EL523
01669      COMPUTE  ACT-ALL-LF-OB   =  ACT-ALL-LF-OB                    EL523
01670                               +  REPT-XTR-GOOD-LF-OB              EL523
01671                               +  REPT-XTR-BAD-LF-OB.              EL523
01672      COMPUTE  ACT-ALL-LF-CAN  =  ACT-ALL-LF-CAN                   EL523
01673                               +  REPT-XTR-GOOD-LF-CAN             EL523
01674                               +  REPT-XTR-BAD-LF-CAN.             EL523
01675      COMPUTE  ACT-ALL-LF-NET  =  ACT-ALL-LF-NET                   EL523
01676                               +  (REPT-XTR-GOOD-LF-AMT            EL523
01677                               +  REPT-XTR-GOOD-LF-OB              EL523
01678                               -  REPT-XTR-GOOD-LF-CAN)            EL523
01679                               +  (REPT-XTR-BAD-LF-AMT             EL523
01680                               +  REPT-XTR-BAD-LF-OB               EL523
01681                               -  REPT-XTR-BAD-LF-CAN).            EL523
01682      COMPUTE  ACT-ALL-LF-COM  =  ACT-ALL-LF-COM                   EL523
01683                               +  REPT-XTR-GOOD-LF-COM             EL523
01684                               +  REPT-XTR-BAD-LF-COM.             EL523
01685                                                                   EL523
01686      COMPUTE  ACT-ALL-LF-I-CNT   =  ACT-ALL-LF-I-CNT              EL523
01687                                  +  REPT-XTR-GOOD-LF-I-CNT        EL523
01688                                  +  REPT-XTR-BAD-LF-I-CNT.        EL523
01689      COMPUTE  ACT-ALL-LF-OB-CNT  =  ACT-ALL-LF-OB-CNT             EL523
01690                                  +  REPT-XTR-GOOD-LF-OB-CNT       EL523
01691                                  +  REPT-XTR-BAD-LF-OB-CNT.       EL523
01692      COMPUTE  ACT-ALL-LF-C-CNT   =  ACT-ALL-LF-C-CNT              EL523
01693                                  +  REPT-XTR-GOOD-LF-C-CNT        EL523
01694                                  +  REPT-XTR-BAD-LF-C-CNT.        EL523
01695                                                                   EL523
01696      COMPUTE  ACT-ALL-AH-AMT  =  ACT-ALL-AH-AMT                   EL523
01697                               +  REPT-XTR-GOOD-AH-AMT             EL523
01698                               +  REPT-XTR-BAD-AH-AMT.             EL523
01699      COMPUTE  ACT-ALL-AH-OB   =  ACT-ALL-AH-OB                    EL523
01700                               +  REPT-XTR-GOOD-AH-OB              EL523
01701                               +  REPT-XTR-BAD-AH-OB.              EL523
01702      COMPUTE  ACT-ALL-AH-CAN  =  ACT-ALL-AH-CAN                   EL523
01703                               +  REPT-XTR-GOOD-AH-CAN             EL523
01704                               +  REPT-XTR-BAD-AH-CAN.             EL523
01705      COMPUTE  ACT-ALL-AH-NET  =  ACT-ALL-AH-NET                   EL523
01706                               +  (REPT-XTR-GOOD-AH-AMT            EL523
01707                               +  REPT-XTR-GOOD-AH-OB              EL523
01708                               -  REPT-XTR-GOOD-AH-CAN)            EL523
01709                               +  (REPT-XTR-BAD-AH-AMT             EL523
01710                               +  REPT-XTR-BAD-AH-OB               EL523
01711                               -  REPT-XTR-BAD-AH-CAN).            EL523
01712      COMPUTE  ACT-ALL-AH-COM  =  ACT-ALL-AH-COM                   EL523
01713                               +  REPT-XTR-GOOD-AH-COM             EL523
01714                               +  REPT-XTR-BAD-AH-COM.             EL523
01715                                                                   EL523
01716      COMPUTE  ACT-ALL-AH-I-CNT   =  ACT-ALL-AH-I-CNT              EL523
01717                                  +  REPT-XTR-GOOD-AH-I-CNT        EL523
01718                                  +  REPT-XTR-BAD-AH-I-CNT.        EL523
01719      COMPUTE  ACT-ALL-AH-OB-CNT  =  ACT-ALL-AH-OB-CNT             EL523
01720                                  +  REPT-XTR-GOOD-AH-OB-CNT       EL523
01721                                  +  REPT-XTR-BAD-AH-OB-CNT.       EL523
01722      COMPUTE  ACT-ALL-AH-C-CNT   =  ACT-ALL-AH-C-CNT              EL523
01723                                  +  REPT-XTR-GOOD-AH-C-CNT        EL523
01724                                  +  REPT-XTR-BAD-AH-C-CNT.        EL523
01725                                                                   EL523
01726  1799-EXIT.                                                       EL523
01727      EXIT.                                                        EL523
01728                                                                   EL523
01729  1800-SET-HI-LO.                                                  EL523
01730      IF REPT-XTR-EFF-DATE LESS THAN HOLD-LO-DATE                  EL523
01731          MOVE REPT-XTR-EFF-DATE  TO  HOLD-LO-DATE.                EL523
01732                                                                   EL523
01733      IF REPT-XTR-EFF-DATE GREATER THAN HOLD-HI-DATE               EL523
01734          MOVE REPT-XTR-EFF-DATE  TO  HOLD-HI-DATE.                EL523
01735                                                                   EL523
01736      MOVE '1'  TO  DATES-SW.                                      EL523
01737                                                                   EL523
01738  1899-EXIT.                                                       EL523
01739      EXIT.                                                        EL523
01740                                                                   EL523
01741  1900-END-PHASE-2.                                                EL523
01742      CLOSE ACCT-XTRACT.                                           EL523
01743                                                                   EL523
01744  1999-EXIT.                                                       EL523
01745       EXIT.                                                       EL523
01746  EJECT                                                            EL523
01747  2000-SORT-ACCT-XTRACT  SECTION.                                  EL523
01748                                                                   EL523
01749      MOVE LOW-VALUES  TO  PREV-CARRIER  PREV-GROUP                EL523
01750                           PREV-STATE    PREV-RPT2.                EL523
01751                                                                   EL523
01752      SORT SORT2-FILE ON ASCENDING KEY SORT-KEY2                   EL523
01753          USING  ACCT-XTRACT                                       EL523
01754          OUTPUT PROCEDURE 2000-READ-ACCT-XTRACT THRU 2999-EXIT.   EL523
01755                                                                   EL523
01756      IF SORT-RETURN NOT = ZEROS                                   EL523
01757          MOVE 'SORT 2 FAILED'  TO  WS-ABEND-MESSAGE               EL523
01758          MOVE    SORT-RETURN   TO  WS-RETURN-CODE                 EL523
01759          GO TO ABEND-PGM.                                         EL523
01760                                                                   EL523
01761      PERFORM 8900-CLOSE-FILES.                                    EL523
01762                                                                   EL523
01763      GOBACK.                                                      EL523
01764  EJECT                                                            EL523
01765  2000-READ-ACCT-XTRACT  SECTION.                                  EL523
01766      RETURN SORT2-FILE INTO ACT-RECORD AT END                     EL523
01767          GO TO 2900-FINAL-TOTALS.                                 EL523
01768                                                                   EL523
01769  2100-PRINT-DETAIL.                                               EL523
01770                                                                   EL523
01771      IF DTE-FMT-OPT  =  3                                         EL523
01772        IF FIRST-RETURN                                            EL523
01773          MOVE 'N'          TO  FIRST-RETURN-SW                    EL523
01774                                HEADERS-PRINTED-SW                 EL523
01775          MOVE ACT-PREFIX   TO  PREV-CARRIER                       EL523
01776          MOVE ACT-GROUP    TO  PREV-GROUP                         EL523
01777          MOVE ACT-RPT2     TO  PREV-RPT2                          EL523
01778          MOVE ACT-ST       TO  PREV-STATE.                        EL523
01779                                                                   EL523
01780      IF (DTE-FMT-OPT  =  3)         AND                           EL523
01781         (DTE-CLIENT  NOT =  'POS')  AND                           EL523
01782         (ACT-ACTIVE  NOT = 'A')                                   EL523
01783             GO TO 2000-READ-ACCT-XTRACT.                          EL523
01784                                                                   EL523
01785      IF DTE-FMT-OPT  =  3                                         EL523
01786         IF ACT-PREFIX = PREV-CARRIER                              EL523
01787            IF HAS-NO-BUSINESS                                     EL523
01788                MOVE  'N'    TO  HEADERS-PRINTED-SW                EL523
01789            ELSE                                                   EL523
01790                GO TO 2000-READ-ACCT-XTRACT                        EL523
01791         ELSE                                                      EL523
01792            IF HAS-NO-BUSINESS                                     EL523
01793                MOVE  'Y'     TO  HEADERS-PRINTED-SW.              EL523
01794                                                                   EL523
01795      IF ACT-PREFIX = PREV-CARRIER                                 EL523
01796          NEXT SENTENCE                                            EL523
01797      ELSE                                                         EL523
01798          MOVE    '1'  TO  BREAK-SWITCH                            EL523
01799          PERFORM 2200-CARRIER-BREAK THRU 2299-EXIT                EL523
01800          MOVE    '0'  TO  BREAK-SWITCH.                           EL523
01801                                                                   EL523
01802      IF DTE-FMT-OPT  =  3                                         EL523
01803         IF ACT-RPT2   = PREV-RPT2                                 EL523
01804            IF HAS-NO-BUSINESS                                     EL523
01805                MOVE  'N'    TO  HEADERS-PRINTED-SW                EL523
01806            ELSE                                                   EL523
01807                GO TO 2000-READ-ACCT-XTRACT                        EL523
01808         ELSE                                                      EL523
01809            IF HAS-NO-BUSINESS                                     EL523
01810                MOVE  'Y'    TO  HEADERS-PRINTED-SW.               EL523
01811                                                                   EL523
01812      IF ACT-RPT2 = PREV-RPT2                                      EL523
01813          NEXT SENTENCE                                            EL523
01814      ELSE                                                         EL523
01815          MOVE    '2'  TO  BREAK-SWITCH                            EL523
01816          PERFORM 2300-GROUP-BREAK THRU 2399-EXIT                  EL523
01817          MOVE    '0'  TO  BREAK-SWITCH.                           EL523
01818                                                                   EL523
01819      IF DTE-FMT-OPT  =  3                                         EL523
01820         IF ACT-GROUP  = PREV-GROUP                                EL523
01821            IF HAS-NO-BUSINESS                                     EL523
01822                MOVE  'N'    TO  HEADERS-PRINTED-SW                EL523
01823            ELSE                                                   EL523
01824                GO TO 2000-READ-ACCT-XTRACT                        EL523
01825         ELSE                                                      EL523
01826            IF HAS-NO-BUSINESS                                     EL523
01827                MOVE  'Y'    TO  HEADERS-PRINTED-SW.               EL523
01828                                                                   EL523
01829      IF ACT-GROUP = PREV-GROUP                                    EL523
01830         NEXT SENTENCE                                             EL523
01831      ELSE                                                         EL523
01832          MOVE    '2'  TO  BREAK-SWITCH                            EL523
01833          PERFORM 2300-GROUP-BREAK THRU 2399-EXIT                  EL523
01834          MOVE    '0'  TO  BREAK-SWITCH.                           EL523
01835                                                                   EL523
01836      IF DTE-FMT-OPT = 3                                           EL523
01837         IF ACT-ST  =  PREV-STATE                                  EL523
01838            IF HAS-NO-BUSINESS                                     EL523
01839                MOVE  'N'    TO  HEADERS-PRINTED-SW                EL523
01840            ELSE                                                   EL523
01841                GO TO 2000-READ-ACCT-XTRACT                        EL523
01842         ELSE                                                      EL523
01843            IF HAS-NO-BUSINESS                                     EL523
01844                MOVE  'Y'    TO  HEADERS-PRINTED-SW.               EL523
01845                                                                   EL523
01846      IF ACT-ST = PREV-STATE                                       EL523
01847          NEXT SENTENCE                                            EL523
01848      ELSE                                                         EL523
01849          PERFORM 2400-STATE-BREAK THRU 2499-EXIT.                 EL523
01850                                                                   EL523
01851      MOVE SPACES    TO  WS-DETAIL1.                               EL523
01852      MOVE ACT-ACCT  TO  WS-D1-ACCOUNT.                            EL523
01853      MOVE ACT-NAME  TO  WS-D1-NAME.                               EL523
01854                                                                   EL523
01855      IF DTE-FMT-OPT  =  2                                         EL523
01856          IF HAS-NO-BUSINESS                                       EL523
01857              GO TO 2000-READ-ACCT-XTRACT.                         EL523
01858                                                                   EL523
01859      IF DTE-FMT-OPT  =   3                                        EL523
01860         IF HAS-NO-BUSINESS                                        EL523
01861            IF DTE-CLIENT  =  'POS'                                EL523
01862               NEXT SENTENCE                                       EL523
01863            ELSE                                                   EL523
01864               GO TO 2101-CONTINUE-PROCESS                         EL523
01865         ELSE                                                      EL523
01866             GO TO 2000-READ-ACCT-XTRACT.                          EL523
01867                                                                   EL523
01868      IF DTE-FMT-OPT NOT EQUAL 3                                   EL523
01869         GO TO 2101-CONTINUE-PROCESS.                              EL523
01870                                                                   EL523
01871      IF HAS-BUSINESS                                              EL523
01872         GO TO 2000-READ-ACCT-XTRACT.                              EL523
01873                                                                   EL523
01874      MOVE +0                     TO WS-ELAPSED-MONTHS.               CL*11
01875                                                                   EL523
01876      MOVE ACT-EFFECTIVE-DT       TO DC-BIN-DATE-1.                   CL*11
01877      MOVE WS-RUN-DATE-BIN        TO DC-BIN-DATE-2.                   CL*11
01878      MOVE '1'                    TO DC-OPTION-CODE.                  CL*11
01879      PERFORM 8500-DATE-CONVERSION.                                   CL*11
01880      IF NO-CONVERSION-ERROR                                       EL523
01881         MOVE DC-ELAPSED-MONTHS   TO WS-ELAPSED-MONTHS.            EL523
01882                                                                   EL523
01883      IF ACT-AM-HI-DT EQUAL ZEROS                                     CL**7
01884         MOVE +7                  TO DC-ELAPSED-MONTHS             EL523
01885      ELSE                                                         EL523
01886         MOVE +0                  TO DC-ELAPSED-MONTHS             EL523
01887                                     DC-ELAPSED-DAYS               EL523
01888         MOVE ACT-AM-HI-DT-CYMD   TO DC-GREG-DATE-CYMD             EL523
01889         MOVE 'L'                 TO DC-OPTION-CODE                EL523
01890         PERFORM 8500-DATE-CONVERSION                              EL523
01891         IF NO-CONVERSION-ERROR                                    EL523
01892            MOVE WS-RUN-DATE-BIN  TO DC-BIN-DATE-2                 EL523
01893            MOVE '1'              TO DC-OPTION-CODE                EL523
01894            PERFORM 8500-DATE-CONVERSION.                          EL523
01895                                                                   EL523
01896      IF (WS-ELAPSED-MONTHS LESS THAN +6) OR                       EL523
01897         (DC-ELAPSED-MONTHS LESS THAN +6)                          EL523
01898         NEXT SENTENCE                                             EL523
01899      ELSE                                                         EL523
01900         GO TO 2000-READ-ACCT-XTRACT.                              EL523
01901                                                                   EL523
01902  2101-CONTINUE-PROCESS.                                           EL523
01903                                                                   EL523
01904      IF DTE-CLIENT NOT = 'MON'                                    EL523
01905          GO TO 2102-SKIP.                                         EL523
01906                                                                   EL523
01907      MOVE 'Y'                TO ACCT-REC-FOUND-SW.                EL523
01908      MOVE SPACES             TO AM-CONTROL-BY-VAR-GRP.            EL523
01909      MOVE DTE-CLASIC-COMPANY-CD TO AM-COMPANY-CD-A1.              EL523
01910      MOVE ACT-ACCT           TO AM-VG-ACCOUNT.                    EL523
01911      MOVE LOW-VALUES         TO AM-VG-DATE.                       EL523
01912      MOVE HIGH-VALUES        TO AM-VG-EXPIRATION-DT.              EL523
01913                                                                   EL523
01914      START ERACCT KEY = AM-CONTROL-BY-VAR-GRP                     EL523
01915          INVALID KEY                                              EL523
01916            MOVE 99999999999  TO ACT-LO-DATE                          CL*12
01917                                 ACT-HI-DATE                       EL523
01918            MOVE 'N'          TO ACCT-REC-FOUND-SW.                EL523
01919                                                                   EL523
01920      IF ACCT-REC-FOUND                                            EL523
01921          READ ERACCT NEXT RECORD.                                 EL523
01922          COPY ELCACCTI.                                           EL523
01923                                                                   EL523
01924  2102-SKIP.                                                       EL523
01925      IF HAS-NO-BUSINESS                                           EL523
01926         IF DTE-FMT-OPT  =  3                                      EL523
01927            GO TO 2105-LINE-2                                         CL*11
01928         ELSE                                                      EL523
01929            MOVE 'NO BUSINESS SUBMITTED THIS PERIOD'               EL523
01930                            TO  WS-D1-LN-REST                      EL523
01931            GO TO 2105-LINE-2.                                        CL*11
01932                                                                   EL523
01933      MOVE LIFE-OVERRIDE-L6   TO  WS-D1-LN-FILL1.                  EL523
01934      MOVE 'WRITTEN'          TO  WS-D1-LN-FILL2.                  EL523
01935      MOVE ACT-GOOD-LF-I-CNT  TO  WS-D1-CNT-1.                     EL523
01936      MOVE ACT-GOOD-LF-AMT    TO  WS-D1-AMT-1.                     EL523
01937      MOVE ACT-BAD-LF-I-CNT   TO  WS-D1-CNT-2.                     EL523
01938      MOVE ACT-BAD-LF-AMT     TO  WS-D1-AMT-2.                     EL523
01939      MOVE ACT-ALL-LF-I-CNT   TO  WS-D1-CNT-3.                     EL523
01940      MOVE ACT-ALL-LF-AMT     TO  WS-D1-AMT-3.                     EL523
01941                                                                   EL523
01942      COMPUTE  ST-SP-I-CNT-TOT-GOOD  =  ST-SP-I-CNT-TOT-GOOD       EL523
01943                                     +  ACT-GOOD-SP-I-CNT.         EL523
01944      COMPUTE  ST-LF-I-CNT-TOT-GOOD  =  ST-LF-I-CNT-TOT-GOOD       EL523
01945                                     +  ACT-GOOD-LF-I-CNT.         EL523
01946      COMPUTE  ST-LF-AMT-TOT-GOOD    =  ST-LF-AMT-TOT-GOOD         EL523
01947                                     +  ACT-GOOD-LF-AMT.           EL523
01948      COMPUTE  ST-SP-I-CNT-TOT-BAD   =  ST-SP-I-CNT-TOT-BAD        EL523
01949                                     +  ACT-BAD-SP-I-CNT.          EL523
01950      COMPUTE  ST-LF-I-CNT-TOT-BAD   =  ST-LF-I-CNT-TOT-BAD        EL523
01951                                     +  ACT-BAD-LF-I-CNT.          EL523
01952      COMPUTE  ST-LF-AMT-TOT-BAD     =  ST-LF-AMT-TOT-BAD          EL523
01953                                     +  ACT-BAD-LF-AMT.            EL523
01954      COMPUTE  ST-LF-I-CNT-TOT-ALL   =  ST-LF-I-CNT-TOT-ALL        EL523
01955                                     +  ACT-ALL-LF-I-CNT.          EL523
01956      COMPUTE  ST-LF-AMT-TOT-ALL     =  ST-LF-AMT-TOT-ALL          EL523
01957                                     +  ACT-ALL-LF-AMT.            EL523
01958                                                                   EL523
01959  2105-LINE-2.                                                     EL523
01960      IF HAS-NO-BUSINESS                                           EL523
01961          COMPUTE  WS-LINE-COUNT-TST  =  WS-LINE-COUNT  +  +2      EL523
01962      ELSE                                                         EL523
01963          COMPUTE  WS-LINE-COUNT-TST  =  WS-LINE-COUNT  +  +19.    EL523
01964                                                                   EL523
01965      IF WS-LINE-COUNT-TST GREATER THAN +57                        EL523
01966         IF DTE-FMT-OPT =  3                                       EL523
01967          MOVE 'ACCOUNTS WITH NO BUSINESS SUBMITTED FOR THE PERIOD'EL523
01968                                       TO  WS-H1-TITLE             EL523
01969            PERFORM WRITE-HEADINGS                                 EL523
01970         ELSE                                                      EL523
01971          MOVE '            NET PREMIUM REPORT'                       CL*11
01972                                       TO  WS-H1-TITLE             EL523
01973            PERFORM WRITE-HEADINGS.                                EL523
01974                                                                   EL523
01975      IF HAS-NO-BUSINESS                                           EL523
01976          MOVE SPACE-2  TO  P-CTL                                  EL523
01977      ELSE                                                         EL523
01978          MOVE SPACE-3  TO  P-CTL.                                 EL523
01979                                                                   EL523
01980      MOVE WS-DETAIL1  TO  P-DATA.                                 EL523
01981                                                                   EL523
01982      PERFORM WRITE-A-LINE.                                        EL523
01983                                                                   EL523
01984      MOVE SPACES  TO  WS-DETAIL1.                                 EL523
01985                                                                   EL523
01986      IF DTE-FMT-OPT EQUAL 3                                       EL523
01987         MOVE ACT-AM-HI-DT        TO ACT-HI-DATE                   EL523
01988         MOVE ACT-AM-LO-DT        TO ACT-LO-DATE.                  EL523
01989                                                                   EL523
01990      IF (HAS-NO-DATES)               AND                          EL523
01991         (DTE-CLIENT NOT EQUAL 'MON') AND                          EL523
01992         (DTE-FMT-OPT NOT EQUAL 3)                                 EL523
01993         MOVE SPACES      TO  WS-D2-LN                             EL523
01994      ELSE                                                         EL523
01995          MOVE ACT-LO-DATE TO  WS-ACT-LO-DATE-N                       CL**6
01996          MOVE ACT-LO-MO   TO  WS-D1-LO-MO                         EL523
01997          MOVE ACT-LO-DA   TO  WS-D1-LO-DA                         EL523
01998          MOVE ACT-LO-YR   TO  WS-D1-LO-YR                         EL523
01999          MOVE '-'         TO  WS-D1-DASH-1  WS-D1-DASH-2          EL523
02000          MOVE ACT-HI-DATE TO  WS-ACT-HI-DATE-N                       CL**6
02001          MOVE ACT-HI-MO   TO  WS-D1-HI-MO                         EL523
02002          MOVE ACT-HI-DA   TO  WS-D1-HI-DA                         EL523
02003          MOVE ACT-HI-YR   TO  WS-D1-HI-YR                         EL523
02004          MOVE '-'         TO  WS-D1-DASH-3  WS-D1-DASH-4.         EL523
02005                                                                   EL523
02006      IF DTE-CLIENT = 'MON' AND                                    EL523
02007         ACCT-REC-FOUND                                            EL523
02008          MOVE AM-LO-MO    TO  WS-D1-LO-MO                         EL523
02009          MOVE AM-LO-DA    TO  WS-D1-LO-DA                         EL523
02010          MOVE AM-LO-YR    TO  WS-D1-LO-YR                         EL523
02011          MOVE '-'         TO  WS-D1-DASH-1  WS-D1-DASH-2          EL523
02012          MOVE AM-HI-MO    TO  WS-D1-HI-MO                         EL523
02013          MOVE AM-HI-DA    TO  WS-D1-HI-DA                         EL523
02014          MOVE AM-HI-YR    TO  WS-D1-HI-YR                         EL523
02015          MOVE '-'         TO  WS-D1-DASH-3  WS-D1-DASH-4.         EL523
02016                                                                   EL523
02017      IF ACT-TEL-NO NOT NUMERIC                                    EL523
02018          MOVE SPACES  TO  WS-D1-PHONE                             EL523
02019      ELSE                                                         EL523
02020          IF ACT-TEL-NO GREATER THAN ZERO                          EL523
02021              MOVE ACT-AREA-CODE  TO  WS-D1-AREA                   EL523
02022              MOVE ACT-TEL-PRE    TO  WS-D1-PREFIX                 EL523
02023              MOVE ACT-TEL-NBR    TO  WS-D1-NO                     EL523
02024              MOVE '-'            TO  WS-D1-DASH1  WS-D1-DASH2     EL523
02025          ELSE                                                     EL523
02026              MOVE SPACES         TO  WS-D1-PHONE.                 EL523
02027                                                                   EL523
02028      IF HAS-NO-BUSINESS                                           EL523
02029          GO TO 2107-LINE-2A.                                      EL523
02030                                                                   EL523
02031      MOVE SPACES              TO  WS-D1-LN-FILL1.                 EL523
02032      MOVE 'OUTSTND BAL'       TO  WS-D1-LN-FILL2.                 EL523
02033      MOVE ACT-GOOD-LF-OB-CNT  TO  WS-D1-CNT-1.                    EL523
02034      MOVE ACT-GOOD-LF-OB      TO  WS-D1-AMT-1.                    EL523
02035      MOVE ACT-BAD-LF-OB-CNT   TO  WS-D1-CNT-2.                    EL523
02036      MOVE ACT-BAD-LF-OB       TO  WS-D1-AMT-2.                    EL523
02037      MOVE ACT-ALL-LF-OB-CNT   TO  WS-D1-CNT-3.                    EL523
02038      MOVE ACT-ALL-LF-OB       TO  WS-D1-AMT-3.                    EL523
02039                                                                   EL523
02040      COMPUTE  ST-OB-I-CNT-TOT-GOOD   =  ST-OB-I-CNT-TOT-GOOD      EL523
02041                                      +  ACT-GOOD-OB-I-CNT.        EL523
02042      COMPUTE  ST-LF-OB-CNT-TOT-GOOD  =  ST-LF-OB-CNT-TOT-GOOD     EL523
02043                                      +  ACT-GOOD-LF-OB-CNT.       EL523
02044      COMPUTE  ST-LF-OB-TOT-GOOD      =  ST-LF-OB-TOT-GOOD         EL523
02045                                      +  ACT-GOOD-LF-OB.           EL523
02046      COMPUTE  ST-OB-I-CNT-TOT-BAD    =  ST-OB-I-CNT-TOT-BAD       EL523
02047                                      +  ACT-BAD-OB-I-CNT.         EL523
02048      COMPUTE  ST-LF-OB-CNT-TOT-BAD   =  ST-LF-OB-CNT-TOT-BAD      EL523
02049                                      +  ACT-BAD-LF-OB-CNT.        EL523
02050      COMPUTE  ST-LF-OB-TOT-BAD       =  ST-LF-OB-TOT-BAD          EL523
02051                                      +  ACT-BAD-LF-OB.            EL523
02052      COMPUTE  ST-LF-OB-CNT-TOT-ALL   =  ST-LF-OB-CNT-TOT-ALL      EL523
02053                                      +  ACT-ALL-LF-OB-CNT.        EL523
02054      COMPUTE  ST-LF-OB-TOT-ALL       =  ST-LF-OB-TOT-ALL          EL523
02055                                      +  ACT-ALL-LF-OB.            EL523
02056                                                                   EL523
02057  2107-LINE-2A.                                                    EL523
02058      MOVE WS-DETAIL1  TO  P-DATA.                                 EL523
02059      MOVE SPACE-1     TO  P-CTL.                                  EL523
02060                                                                   EL523
02061      PERFORM WRITE-A-LINE.                                        EL523
02062                                                                   EL523
02063      MOVE SPACES      TO  WS-DETAIL1.                             EL523
02064                                                                   EL523
02065  2110-LINE-3.                                                     EL523

02066      IF DTE-PGM-OPT  =  2                                         EL523
              EVALUATE ACT-ACTIVE
                 WHEN 'I'
                    MOVE 'INACTIVE'    TO WS-D1-ACCOUNT
                 WHEN 'T'
                    MOVE 'TRANSFER'    TO WS-D1-ACCOUNT
                 WHEN 'C'
                    MOVE 'CANCELLED'   TO WS-D1-ACCOUNT
                 WHEN 'F'
                    MOVE 'FROZEN'      TO WS-D1-ACCOUNT
                 WHEN 'S'
                    MOVE 'SUSPENDED'   TO WS-D1-ACCOUNT
                 WHEN OTHER
                    MOVE 'ACTIVE'      TO WS-D1-ACCOUNT
              END-EVALUATE
           ELSE
              MOVE SPACES              TO WS-D1-ACCOUNT
           END-IF
           
02067 *        IF ACT-ACTIVE = 'A'                                      EL523
02068 *            MOVE 'ACTIVE'       TO  WS-D1-ACCOUNT                EL523
02069 *        ELSE                                                     EL523
02070 *            MOVE 'INACTIVE'     TO  WS-D1-ACCOUNT                EL523
02071 *    ELSE                                                         EL523
02072 *        MOVE SPACES             TO  WS-D1-ACCOUNT.               EL523
02073                                                                   EL523
02074      IF HAS-NO-BUSINESS                                           EL523
02075          GO TO 2112-LINE-3A.                                      EL523
02076                                                                   EL523
02077      MOVE SPACES             TO  WS-D1-LN-FILL1.                  EL523
02078      MOVE 'CANCELLED'        TO  WS-D1-LN-FILL2.                  EL523
02079      MOVE ACT-GOOD-LF-C-CNT  TO  WS-D1-CNT-1.                     EL523
02080      MOVE ACT-GOOD-LF-CAN    TO  WS-D1-AMT-1.                     EL523
02081      MOVE ACT-BAD-LF-C-CNT   TO  WS-D1-CNT-2.                     EL523
02082      MOVE ACT-BAD-LF-CAN     TO  WS-D1-AMT-2.                     EL523
02083      MOVE ACT-ALL-LF-C-CNT   TO  WS-D1-CNT-3.                     EL523
02084      MOVE ACT-ALL-LF-CAN     TO  WS-D1-AMT-3.                     EL523
02085                                                                   EL523
02086      COMPUTE  ST-C-CNT-TOT-GOOD     =  ST-C-CNT-TOT-GOOD          EL523
02087                                     +  ACT-GOOD-C-CNT.            EL523
02088      COMPUTE  ST-LF-C-CNT-TOT-GOOD  =  ST-LF-C-CNT-TOT-GOOD       EL523
02089                                     +  ACT-GOOD-LF-C-CNT.         EL523
02090      COMPUTE  ST-LF-CAN-TOT-GOOD    =  ST-LF-CAN-TOT-GOOD         EL523
02091                                     +  ACT-GOOD-LF-CAN.           EL523
02092      COMPUTE  ST-C-CNT-TOT-BAD      =  ST-C-CNT-TOT-BAD           EL523
02093                                     +  ACT-BAD-C-CNT.             EL523
02094      COMPUTE  ST-LF-C-CNT-TOT-BAD   =  ST-LF-C-CNT-TOT-BAD        EL523
02095                                     +  ACT-BAD-LF-C-CNT.          EL523
02096      COMPUTE  ST-LF-CAN-TOT-BAD     =  ST-LF-CAN-TOT-BAD          EL523
02097                                     +  ACT-BAD-LF-CAN.            EL523
02098      COMPUTE  ST-LF-C-CNT-TOT-ALL   =  ST-LF-C-CNT-TOT-ALL        EL523
02099                                     +  ACT-ALL-LF-C-CNT.          EL523
02100      COMPUTE  ST-LF-CAN-TOT-ALL     =  ST-LF-CAN-TOT-ALL          EL523
02101                                     +  ACT-ALL-LF-CAN.            EL523
02102                                                                   EL523
02103  2112-LINE-3A.                                                    EL523
02104      MOVE WS-DETAIL1  TO  P-DATA.                                 EL523
02105      MOVE SPACE-1     TO  P-CTL.                                  EL523
02106                                                                   EL523
02107      PERFORM WRITE-A-LINE.                                        EL523
02108                                                                   EL523
02109      MOVE SPACES      TO  WS-DETAIL1.                             EL523
02110                                                                   EL523
02111      IF HAS-NO-BUSINESS                                           EL523
02112          GO TO 2000-READ-ACCT-XTRACT.                             EL523
02113                                                                   EL523
02114  2115-LINE-4.                                                     EL523
02115      MOVE SPACES           TO  WS-D1-LN-FILL1.                    EL523
02116      MOVE 'NET PREMIUM'    TO  WS-D1-LN-FILL2.                    EL523
02117      MOVE ACT-GOOD-LF-NET  TO  WS-D1-AMT-1.                       EL523
02118      MOVE ACT-BAD-LF-NET   TO  WS-D1-AMT-2.                       EL523
02119      MOVE ACT-ALL-LF-NET   TO  WS-D1-AMT-3.                       EL523
02120                                                                   EL523
02121      COMPUTE  ST-LF-NET-TOT-GOOD  =  ST-LF-NET-TOT-GOOD           EL523
02122                                   +  ACT-GOOD-LF-NET.             EL523
02123      COMPUTE  ST-LF-NET-TOT-BAD   =  ST-LF-NET-TOT-BAD            EL523
02124                                   +  ACT-BAD-LF-NET.              EL523
02125      COMPUTE  ST-LF-NET-TOT-ALL   =  ST-LF-NET-TOT-ALL            EL523
02126                                   +  ACT-ALL-LF-NET.              EL523
02127                                                                   EL523
02128      MOVE WS-DETAIL1  TO  P-DATA.                                 EL523
02129      MOVE SPACE-1     TO  P-CTL.                                  EL523
02130                                                                   EL523
02131      PERFORM WRITE-A-LINE.                                        EL523
02132                                                                   EL523
02133      MOVE SPACES      TO  WS-DETAIL1.                             EL523
02134                                                                   EL523
02135  2120-LINE-5.                                                     EL523
02136      MOVE SPACES            TO  WS-D1-LN-FILL1.                   EL523
02137      MOVE 'NET COMMISSION'  TO  WS-D1-LN-FILL2.                   EL523
02138      MOVE ACT-GOOD-LF-COM   TO  WS-D1-AMT-1.                      EL523
02139      MOVE ACT-BAD-LF-COM    TO  WS-D1-AMT-2.                      EL523
02140      MOVE ACT-ALL-LF-COM    TO  WS-D1-AMT-3.                      EL523
02141                                                                   EL523
02142      COMPUTE  ST-LF-COM-TOT-GOOD  =  ST-LF-COM-TOT-GOOD           EL523
02143                                   +  ACT-GOOD-LF-COM.             EL523
02144      COMPUTE  ST-LF-COM-TOT-BAD   =  ST-LF-COM-TOT-BAD            EL523
02145                                   +  ACT-BAD-LF-COM.              EL523
02146      COMPUTE  ST-LF-COM-TOT-ALL   =  ST-LF-COM-TOT-ALL            EL523
02147                                   +  ACT-ALL-LF-COM.              EL523
02148                                                                   EL523
02149      MOVE WS-DETAIL1  TO  P-DATA.                                 EL523
02150      MOVE SPACE-1     TO  P-CTL.                                  EL523
02151                                                                   EL523
02152      PERFORM WRITE-A-LINE.                                        EL523
02153                                                                   EL523
02154      MOVE SPACES      TO  WS-DETAIL1.                             EL523
02155                                                                   EL523
02156  2125-LINE-6.                                                     EL523
02157      MOVE AH-OVERRIDE-L6     TO  WS-D1-LN-FILL1.                  EL523
02158      MOVE 'WRITTEN'          TO  WS-D1-LN-FILL2.                  EL523
02159      MOVE ACT-GOOD-AH-I-CNT  TO  WS-D1-CNT-1.                     EL523
02160      MOVE ACT-GOOD-AH-AMT    TO  WS-D1-AMT-1.                     EL523
02161      MOVE ACT-BAD-AH-I-CNT   TO  WS-D1-CNT-2.                     EL523
02162      MOVE ACT-BAD-AH-AMT     TO  WS-D1-AMT-2.                     EL523
02163      MOVE ACT-ALL-AH-I-CNT   TO  WS-D1-CNT-3.                     EL523
02164      MOVE ACT-ALL-AH-AMT     TO  WS-D1-AMT-3.                     EL523
02165                                                                   EL523
02166      COMPUTE  ST-AH-I-CNT-TOT-GOOD  =  ST-AH-I-CNT-TOT-GOOD       EL523
02167                                     +  ACT-GOOD-AH-I-CNT.         EL523
02168      COMPUTE  ST-AH-AMT-TOT-GOOD    =  ST-AH-AMT-TOT-GOOD         EL523
02169                                     +  ACT-GOOD-AH-AMT.           EL523
02170      COMPUTE  ST-AH-I-CNT-TOT-BAD   =  ST-AH-I-CNT-TOT-BAD        EL523
02171                                     +  ACT-BAD-AH-I-CNT.          EL523
02172      COMPUTE  ST-AH-AMT-TOT-BAD     =  ST-AH-AMT-TOT-BAD          EL523
02173                                     +  ACT-BAD-AH-AMT.            EL523
02174      COMPUTE  ST-AH-I-CNT-TOT-ALL   =  ST-AH-I-CNT-TOT-ALL        EL523
02175                                     +  ACT-ALL-AH-I-CNT.          EL523
02176      COMPUTE  ST-AH-AMT-TOT-ALL     =  ST-AH-AMT-TOT-ALL          EL523
02177                                     +  ACT-ALL-AH-AMT.            EL523
02178                                                                   EL523
02179      MOVE WS-DETAIL1  TO  P-DATA.                                 EL523
02180      MOVE SPACE-2     TO  P-CTL.                                  EL523
02181                                                                   EL523
02182      PERFORM WRITE-A-LINE.                                        EL523
02183                                                                   EL523
02184      MOVE SPACES      TO  WS-DETAIL1.                             EL523
02185                                                                   EL523
02186  2130-LINE-7.                                                     EL523
02187      MOVE SPACES              TO  WS-D1-LN-FILL1.                 EL523
02188      MOVE 'OUTSTND BAL'       TO  WS-D1-LN-FILL2.                 EL523
02189      MOVE ACT-GOOD-AH-OB-CNT  TO  WS-D1-CNT-1.                    EL523
02190      MOVE ACT-GOOD-AH-OB      TO  WS-D1-AMT-1.                    EL523
02191      MOVE ACT-BAD-AH-OB-CNT   TO  WS-D1-CNT-2.                    EL523
02192      MOVE ACT-BAD-AH-OB       TO  WS-D1-AMT-2.                    EL523
02193      MOVE ACT-ALL-AH-OB-CNT   TO  WS-D1-CNT-3.                    EL523
02194      MOVE ACT-ALL-AH-OB       TO  WS-D1-AMT-3.                    EL523
02195                                                                   EL523
02196      COMPUTE  ST-AH-OB-CNT-TOT-GOOD  =  ST-AH-OB-CNT-TOT-GOOD     EL523
02197                                      +  ACT-GOOD-AH-OB-CNT.       EL523
02198      COMPUTE  ST-AH-OB-TOT-GOOD      =  ST-AH-OB-TOT-GOOD         EL523
02199                                      +  ACT-GOOD-AH-OB.           EL523
02200      COMPUTE  ST-AH-OB-CNT-TOT-BAD   =  ST-AH-OB-CNT-TOT-BAD      EL523
02201                                      +  ACT-BAD-AH-OB-CNT.        EL523
02202      COMPUTE  ST-AH-OB-TOT-BAD       =  ST-AH-OB-TOT-BAD          EL523
02203                                      +  ACT-BAD-AH-OB.            EL523
02204      COMPUTE  ST-AH-OB-CNT-TOT-ALL   =  ST-AH-OB-CNT-TOT-ALL      EL523
02205                                      +  ACT-ALL-AH-OB-CNT.        EL523
02206      COMPUTE  ST-AH-OB-TOT-ALL       =  ST-AH-OB-TOT-ALL          EL523
02207                                      +  ACT-ALL-AH-OB.            EL523
02208                                                                   EL523
02209      MOVE WS-DETAIL1  TO  P-DATA.                                 EL523
02210      MOVE SPACE-1     TO  P-CTL.                                  EL523
02211                                                                   EL523
02212      PERFORM WRITE-A-LINE.                                        EL523
02213                                                                   EL523
02214      MOVE SPACES      TO  WS-DETAIL1.                             EL523
02215                                                                   EL523
02216  2135-LINE-8.                                                     EL523
02217      MOVE SPACES             TO  WS-D1-LN-FILL1.                  EL523
02218      MOVE 'CANCELLED'        TO  WS-D1-LN-FILL2.                  EL523
02219      MOVE ACT-GOOD-AH-C-CNT  TO  WS-D1-CNT-1.                     EL523
02220      MOVE ACT-GOOD-AH-CAN    TO  WS-D1-AMT-1.                     EL523
02221      MOVE ACT-BAD-AH-C-CNT   TO  WS-D1-CNT-2.                     EL523
02222      MOVE ACT-BAD-AH-CAN     TO  WS-D1-AMT-2.                     EL523
02223      MOVE ACT-ALL-AH-C-CNT   TO  WS-D1-CNT-3.                     EL523
02224      MOVE ACT-ALL-AH-CAN     TO  WS-D1-AMT-3.                     EL523
02225                                                                   EL523
02226      COMPUTE  ST-AH-C-CNT-TOT-GOOD  =  ST-AH-C-CNT-TOT-GOOD       EL523
02227                                     +  ACT-GOOD-AH-C-CNT.         EL523
02228      COMPUTE  ST-AH-CAN-TOT-GOOD    =  ST-AH-CAN-TOT-GOOD         EL523
02229                                     +  ACT-GOOD-AH-CAN.           EL523
02230      COMPUTE  ST-AH-C-CNT-TOT-BAD   =  ST-AH-C-CNT-TOT-BAD        EL523
02231                                     +  ACT-BAD-AH-C-CNT.          EL523
02232      COMPUTE  ST-AH-CAN-TOT-BAD     =  ST-AH-CAN-TOT-BAD          EL523
02233                                     +  ACT-BAD-AH-CAN.            EL523
02234      COMPUTE  ST-AH-C-CNT-TOT-ALL   =  ST-AH-C-CNT-TOT-ALL        EL523
02235                                     +  ACT-ALL-AH-C-CNT.          EL523
02236      COMPUTE  ST-AH-CAN-TOT-ALL     =  ST-AH-CAN-TOT-ALL          EL523
02237                                     +  ACT-ALL-AH-CAN.            EL523
02238                                                                   EL523
02239      MOVE WS-DETAIL1  TO  P-DATA.                                 EL523
02240      MOVE SPACE-1     TO  P-CTL.                                  EL523
02241                                                                   EL523
02242      PERFORM WRITE-A-LINE.                                        EL523
02243                                                                   EL523
02244      MOVE SPACES      TO  WS-DETAIL1.                             EL523
02245                                                                   EL523
02246  2140-LINE-9.                                                     EL523
02247      MOVE SPACES           TO  WS-D1-LN-FILL1.                    EL523
02248      MOVE 'NET PREMIUM'    TO  WS-D1-LN-FILL2.                    EL523
02249      MOVE ACT-GOOD-AH-NET  TO  WS-D1-AMT-1.                       EL523
02250      MOVE ACT-BAD-AH-NET   TO  WS-D1-AMT-2.                       EL523
02251      MOVE ACT-ALL-AH-NET   TO  WS-D1-AMT-3.                       EL523
02252                                                                   EL523
02253      COMPUTE  ST-AH-NET-TOT-GOOD  =  ST-AH-NET-TOT-GOOD           EL523
02254                                   +  ACT-GOOD-AH-NET.             EL523
02255      COMPUTE  ST-AH-NET-TOT-BAD   =  ST-AH-NET-TOT-BAD            EL523
02256                                   +  ACT-BAD-AH-NET.              EL523
02257      COMPUTE  ST-AH-NET-TOT-ALL   =  ST-AH-NET-TOT-ALL            EL523
02258                                   +  ACT-ALL-AH-NET.              EL523
02259                                                                   EL523
02260      MOVE WS-DETAIL1  TO  P-DATA.                                 EL523
02261      MOVE SPACE-1     TO  P-CTL.                                  EL523
02262                                                                   EL523
02263      PERFORM WRITE-A-LINE.                                        EL523
02264                                                                   EL523
02265      MOVE SPACES      TO  WS-DETAIL1.                             EL523
02266                                                                   EL523
02267  2145-LINE-10.                                                    EL523
02268      MOVE SPACES            TO  WS-D1-LN-FILL1.                   EL523
02269      MOVE 'NET COMMISSION'  TO  WS-D1-LN-FILL2.                   EL523
02270      MOVE ACT-GOOD-AH-COM   TO  WS-D1-AMT-1.                      EL523
02271      MOVE ACT-BAD-AH-COM    TO  WS-D1-AMT-2.                      EL523
02272      MOVE ACT-ALL-AH-COM    TO  WS-D1-AMT-3.                      EL523
02273                                                                   EL523
02274      COMPUTE  ST-AH-COM-TOT-GOOD  =  ST-AH-COM-TOT-GOOD           EL523
02275                                   +  ACT-GOOD-AH-COM.             EL523
02276      COMPUTE  ST-AH-COM-TOT-BAD   =  ST-AH-COM-TOT-BAD            EL523
02277                                   +  ACT-BAD-AH-COM.              EL523
02278      COMPUTE  ST-AH-COM-TOT-ALL   =  ST-AH-COM-TOT-ALL            EL523
02279                                   +  ACT-ALL-AH-COM.              EL523
02280                                                                   EL523
02281      MOVE WS-DETAIL1  TO  P-DATA.                                 EL523
02282      MOVE SPACE-1     TO  P-CTL.                                  EL523
02283                                                                   EL523
02284      PERFORM WRITE-A-LINE.                                        EL523
02285                                                                   EL523
02286      MOVE SPACES      TO  WS-DETAIL1.                             EL523
02287                                                                   EL523
02288  2150-LINE-11.                                                    EL523
02289      MOVE 'TOTAL '   TO  WS-D1-LN-FILL1.                          EL523
02290      MOVE 'WRITTEN'  TO  WS-D1-LN-FILL2.                          EL523
02291      MOVE ACT-GOOD-SP-I-CNT   TO WS-D1-CNT-1.                     EL523
02292                                                                   EL523
02293      COMPUTE  WS-D1-AMT-1  =  ACT-GOOD-LF-AMT                     EL523
02294                            +  ACT-GOOD-AH-AMT.                    EL523
02295                                                                   EL523
02296      MOVE ACT-BAD-SP-I-CNT    TO WS-D1-CNT-2.                     EL523
02297                                                                   EL523
02298      COMPUTE  WS-D1-AMT-2  =  ACT-BAD-LF-AMT                      EL523
02299                            +  ACT-BAD-AH-AMT.                     EL523
02300      COMPUTE  WS-D1-CNT-3  =  ACT-GOOD-SP-I-CNT                   EL523
02301                            +  ACT-BAD-SP-I-CNT.                   EL523
02302      COMPUTE  WS-D1-AMT-3  =  ACT-ALL-LF-AMT                      EL523
02303                            +  ACT-ALL-AH-AMT.                     EL523
02304                                                                   EL523
02305      MOVE WS-DETAIL1  TO  P-DATA.                                 EL523
02306      MOVE SPACE-2     TO  P-CTL.                                  EL523
02307                                                                   EL523
02308      PERFORM WRITE-A-LINE.                                        EL523
02309                                                                   EL523
02310      MOVE SPACES      TO  WS-DETAIL1.                             EL523
02311                                                                   EL523
02312  2155-LINE-12.                                                    EL523
02313      MOVE SPACES         TO  WS-D1-LN-FILL1.                      EL523
02314      MOVE 'OUTSTND BAL'  TO  WS-D1-LN-FILL2.                      EL523
02315      MOVE ACT-GOOD-OB-I-CNT   TO WS-D1-CNT-1.                     EL523
02316                                                                   EL523
02317      COMPUTE  WS-D1-AMT-1  =  ACT-GOOD-LF-OB  +  ACT-GOOD-AH-OB.  EL523
02318                                                                   EL523
02319      MOVE ACT-BAD-OB-I-CNT    TO WS-D1-CNT-2.                     EL523
02320                                                                   EL523
02321      COMPUTE  WS-D1-AMT-2  =  ACT-BAD-LF-OB   +  ACT-BAD-AH-OB.   EL523
02322      COMPUTE  WS-D1-CNT-3  =  ACT-GOOD-OB-I-CNT                   EL523
02323                            +  ACT-BAD-OB-I-CNT.                   EL523
02324      COMPUTE  WS-D1-AMT-3  =  ACT-ALL-LF-OB   +  ACT-ALL-AH-OB.   EL523
02325                                                                   EL523
02326      MOVE WS-DETAIL1  TO  P-DATA.                                 EL523
02327      MOVE SPACE-1     TO  P-CTL.                                  EL523
02328                                                                   EL523
02329      PERFORM WRITE-A-LINE.                                        EL523
02330                                                                   EL523
02331      MOVE SPACES      TO  WS-DETAIL1.                             EL523
02332                                                                   EL523
02333  2160-LINE-13.                                                    EL523
02334      MOVE SPACES       TO  WS-D1-LN-FILL1.                        EL523
02335      MOVE 'CANCELLED'  TO  WS-D1-LN-FILL2.                        EL523
02336      MOVE ACT-GOOD-C-CNT      TO WS-D1-CNT-1.                     EL523
02337                                                                   EL523
02338      COMPUTE  WS-D1-AMT-1  =  ACT-GOOD-LF-CAN                     EL523
02339                            +  ACT-GOOD-AH-CAN.                    EL523
02340                                                                   EL523
02341      MOVE ACT-BAD-C-CNT       TO WS-D1-CNT-2.                     EL523
02342                                                                   EL523
02343      COMPUTE  WS-D1-AMT-2  =  ACT-BAD-LF-CAN                      EL523
02344                            +  ACT-BAD-AH-CAN.                     EL523
02345      COMPUTE  WS-D1-CNT-3  =  ACT-GOOD-C-CNT                      EL523
02346                            +  ACT-BAD-C-CNT.                      EL523
02347      COMPUTE  WS-D1-AMT-3  =  ACT-ALL-LF-CAN                      EL523
02348                            +  ACT-ALL-AH-CAN.                     EL523
02349                                                                   EL523
02350      MOVE WS-DETAIL1  TO  P-DATA.                                 EL523
02351      MOVE SPACE-1     TO  P-CTL.                                  EL523
02352                                                                   EL523
02353      PERFORM WRITE-A-LINE.                                        EL523
02354                                                                   EL523
02355      MOVE SPACES      TO  WS-DETAIL1.                             EL523
02356                                                                   EL523
02357  2165-LINE-14.                                                    EL523
02358      MOVE SPACES         TO  WS-D1-LN-FILL1.                      EL523
02359      MOVE 'NET PREMIUM'  TO  WS-D1-LN-FILL2.                      EL523
02360                                                                   EL523
02361      COMPUTE  WS-D1-AMT-1  =  ACT-GOOD-LF-NET                     EL523
02362                            +  ACT-GOOD-AH-NET.                    EL523
02363      COMPUTE  WS-D1-AMT-2  =  ACT-BAD-LF-NET                      EL523
02364                            +  ACT-BAD-AH-NET.                     EL523
02365      COMPUTE  WS-D1-AMT-3  =  ACT-ALL-LF-NET                      EL523
02366                            +  ACT-ALL-AH-NET.                     EL523
02367                                                                   EL523
02368      MOVE WS-DETAIL1  TO  P-DATA.                                 EL523
02369      MOVE SPACE-1     TO  P-CTL.                                  EL523
02370                                                                   EL523
02371      PERFORM WRITE-A-LINE.                                        EL523
02372                                                                   EL523
02373      MOVE SPACES      TO  WS-DETAIL1.                             EL523
02374                                                                   EL523
02375  2170-LINE-15.                                                    EL523
02376      MOVE SPACES            TO  WS-D1-LN-FILL1.                   EL523
02377      MOVE 'NET COMMISSION'  TO  WS-D1-LN-FILL2.                   EL523
02378                                                                   EL523
02379      COMPUTE  WS-D1-AMT-1  =  ACT-GOOD-LF-COM                     EL523
02380                            +  ACT-GOOD-AH-COM.                    EL523
02381      COMPUTE  WS-D1-AMT-2  =  ACT-BAD-LF-COM                      EL523
02382                            +  ACT-BAD-AH-COM.                     EL523
02383      COMPUTE  WS-D1-AMT-3  =  ACT-ALL-LF-COM                      EL523
02384                            +  ACT-ALL-AH-COM.                     EL523
02385                                                                   EL523
02386      MOVE WS-DETAIL1  TO  P-DATA.                                 EL523
02387      MOVE SPACE-1     TO  P-CTL.                                  EL523
02388                                                                   EL523
02389      PERFORM WRITE-A-LINE.                                        EL523
02390                                                                   EL523
02391      MOVE SPACES      TO  WS-DETAIL1.                             EL523
02392                                                                   EL523
02393  2199-EXIT.                                                       EL523
02394      GO TO 2000-READ-ACCT-XTRACT.                                 EL523
02395  EJECT                                                            EL523
02396  2200-CARRIER-BREAK.                                              EL523
02397      PERFORM 2300-GROUP-BREAK THRU 2399-EXIT.                     EL523
02398                                                                   EL523
02399      IF FIRST-TIME                                                EL523
02400          MOVE '1'  TO  FIRST-SW                                   EL523
02401          GO TO 2280-SET-CARRIER.                                  EL523
02402                                                                   EL523
02403      IF DTE-FMT-OPT  =  3                                         EL523
02404            GO TO 2280-SET-CARRIER.                                EL523
02405                                                                   EL523
02406      PERFORM 2500-FLIP-STATE   THRU 2599-EXIT.                    EL523
02407      PERFORM 2700-FLIP-GROUP   THRU 2799-EXIT.                    EL523
02408                                                                   EL523
02409      IF PRT-CARRIER-TOTAL                                         EL523
02410          NEXT SENTENCE                                            EL523
02411      ELSE                                                         EL523
02412          GO TO 2272-ROLL-TOTALS.                                  EL523
02413                                                                   EL523
02414      COMPUTE  WS-LINE-COUNT-TST  =  WS-LINE-COUNT  +  +19.        EL523
02415                                                                   EL523
02416      IF WS-LINE-COUNT-TST GREATER THAN +57                        EL523
02417          MOVE '            NET PREMIUM REPORT'                       CL*11
02418                                       TO  WS-H1-TITLE             EL523
02419          PERFORM WRITE-HEADINGS.                                  EL523
02420                                                                   EL523
02421      MOVE SPACES   TO  P-DATA.                                    EL523
02422      MOVE SPACE-2  TO  P-CTL.                                     EL523
02423                                                                   EL523
02424      PERFORM WRITE-A-LINE.                                        EL523
02425                                                                   EL523
02426      MOVE TOT-MSG-3              TO  WS-D1-LN.                    EL523
02427      MOVE LIFE-OVERRIDE-L6       TO  WS-D1-LN-FILL1.              EL523
02428      MOVE 'WRITTEN'              TO  WS-D1-LN-FILL2.              EL523
02429      MOVE CAR-LF-I-CNT-TOT-GOOD  TO  WS-D1-CNT-1.                 EL523
02430      MOVE CAR-LF-AMT-TOT-GOOD    TO  WS-D1-AMT-1.                 EL523
02431      MOVE CAR-LF-I-CNT-TOT-BAD   TO  WS-D1-CNT-2.                 EL523
02432      MOVE CAR-LF-AMT-TOT-BAD     TO  WS-D1-AMT-2.                 EL523
02433      MOVE CAR-LF-I-CNT-TOT-ALL   TO  WS-D1-CNT-3.                 EL523
02434      MOVE CAR-LF-AMT-TOT-ALL     TO  WS-D1-AMT-3.                 EL523
02435      MOVE WS-DETAIL1             TO  P-DATA.                      EL523
02436      MOVE SPACE-3                TO  P-CTL.                       EL523
02437                                                                   EL523
02438      PERFORM WRITE-A-LINE.                                        EL523
02439                                                                   EL523
02440      MOVE SPACES               TO  WS-DETAIL1.                    EL523
02441                                                                   EL523
02442  2205-LINE-2.                                                     EL523
02443      MOVE SPACES                  TO  WS-D1-LN-FILL1.             EL523
02444      MOVE 'OUTSTND BAL'           TO  WS-D1-LN-FILL2.             EL523
02445      MOVE CAR-LF-OB-CNT-TOT-GOOD  TO  WS-D1-CNT-1.                EL523
02446      MOVE CAR-LF-OB-TOT-GOOD      TO  WS-D1-AMT-1.                EL523
02447      MOVE CAR-LF-OB-CNT-TOT-BAD   TO  WS-D1-CNT-2.                EL523
02448      MOVE CAR-LF-OB-TOT-BAD       TO  WS-D1-AMT-2.                EL523
02449      MOVE CAR-LF-OB-CNT-TOT-ALL   TO  WS-D1-CNT-3.                EL523
02450      MOVE CAR-LF-OB-TOT-ALL       TO  WS-D1-AMT-3.                EL523
02451      MOVE WS-DETAIL1              TO  P-DATA.                     EL523
02452      MOVE SPACE-1                 TO  P-CTL.                      EL523
02453                                                                   EL523
02454      PERFORM WRITE-A-LINE.                                        EL523
02455                                                                   EL523
02456      MOVE SPACES              TO  WS-DETAIL1.                     EL523
02457                                                                   EL523
02458  2210-LINE-3.                                                     EL523
02459      MOVE SPACES                 TO  WS-D1-LN-FILL1.              EL523
02460      MOVE 'CANCELLED'            TO  WS-D1-LN-FILL2.              EL523
02461      MOVE CAR-LF-C-CNT-TOT-GOOD  TO  WS-D1-CNT-1.                 EL523
02462      MOVE CAR-LF-CAN-TOT-GOOD    TO  WS-D1-AMT-1.                 EL523
02463      MOVE CAR-LF-C-CNT-TOT-BAD   TO  WS-D1-CNT-2.                 EL523
02464      MOVE CAR-LF-CAN-TOT-BAD     TO  WS-D1-AMT-2.                 EL523
02465      MOVE CAR-LF-C-CNT-TOT-ALL   TO  WS-D1-CNT-3.                 EL523
02466      MOVE CAR-LF-CAN-TOT-ALL     TO  WS-D1-AMT-3.                 EL523
02467      MOVE WS-DETAIL1             TO  P-DATA.                      EL523
02468      MOVE SPACE-1                TO  P-CTL.                       EL523
02469                                                                   EL523
02470      PERFORM WRITE-A-LINE.                                        EL523
02471                                                                   EL523
02472      MOVE SPACES               TO  WS-DETAIL1.                    EL523
02473                                                                   EL523
02474  2215-LINE-4.                                                     EL523
02475      MOVE SPACES               TO  WS-D1-LN-FILL1.                EL523
02476      MOVE 'NET PREMIUM'        TO  WS-D1-LN-FILL2.                EL523
02477      MOVE CAR-LF-NET-TOT-GOOD  TO  WS-D1-AMT-1.                   EL523
02478      MOVE CAR-LF-NET-TOT-BAD   TO  WS-D1-AMT-2.                   EL523
02479      MOVE CAR-LF-NET-TOT-ALL   TO  WS-D1-AMT-3.                   EL523
02480      MOVE WS-DETAIL1           TO  P-DATA.                        EL523
02481      MOVE SPACE-1              TO  P-CTL.                         EL523
02482                                                                   EL523
02483      PERFORM WRITE-A-LINE.                                        EL523
02484                                                                   EL523
02485      MOVE SPACES               TO  WS-DETAIL1.                    EL523
02486                                                                   EL523
02487  2220-LINE-5.                                                     EL523
02488      MOVE SPACES               TO  WS-D1-LN-FILL1.                EL523
02489      MOVE 'NET COMMISSION'     TO  WS-D1-LN-FILL2.                EL523
02490      MOVE CAR-LF-COM-TOT-GOOD  TO  WS-D1-AMT-1.                   EL523
02491      MOVE CAR-LF-COM-TOT-BAD   TO  WS-D1-AMT-2.                   EL523
02492      MOVE CAR-LF-COM-TOT-ALL   TO  WS-D1-AMT-3.                   EL523
02493      MOVE WS-DETAIL1           TO  P-DATA.                        EL523
02494      MOVE SPACE-1              TO  P-CTL.                         EL523
02495                                                                   EL523
02496      PERFORM WRITE-A-LINE.                                        EL523
02497                                                                   EL523
02498      MOVE SPACES               TO  WS-DETAIL1.                    EL523
02499                                                                   EL523
02500  2225-LINE-6.                                                     EL523
02501      MOVE AH-OVERRIDE-L6         TO  WS-D1-LN-FILL1.              EL523
02502      MOVE 'WRITTEN'              TO  WS-D1-LN-FILL2.              EL523
02503      MOVE CAR-AH-I-CNT-TOT-GOOD  TO  WS-D1-CNT-1.                 EL523
02504      MOVE CAR-AH-AMT-TOT-GOOD    TO  WS-D1-AMT-1.                 EL523
02505      MOVE CAR-AH-I-CNT-TOT-BAD   TO  WS-D1-CNT-2.                 EL523
02506      MOVE CAR-AH-AMT-TOT-BAD     TO  WS-D1-AMT-2.                 EL523
02507      MOVE CAR-AH-I-CNT-TOT-ALL   TO  WS-D1-CNT-3.                 EL523
02508      MOVE CAR-AH-AMT-TOT-ALL     TO  WS-D1-AMT-3.                 EL523
02509      MOVE WS-DETAIL1             TO  P-DATA.                      EL523
02510      MOVE SPACE-2                TO  P-CTL.                       EL523
02511                                                                   EL523
02512      PERFORM WRITE-A-LINE.                                        EL523
02513                                                                   EL523
02514      MOVE SPACES               TO  WS-DETAIL1.                    EL523
02515                                                                   EL523
02516  2230-LINE-7.                                                     EL523
02517      MOVE SPACES                  TO  WS-D1-LN-FILL1.             EL523
02518      MOVE 'OUTSTND BAL'           TO  WS-D1-LN-FILL2.             EL523
02519      MOVE CAR-AH-OB-CNT-TOT-GOOD  TO  WS-D1-CNT-1.                EL523
02520      MOVE CAR-AH-OB-TOT-GOOD      TO  WS-D1-AMT-1.                EL523
02521      MOVE CAR-AH-OB-CNT-TOT-BAD   TO  WS-D1-CNT-2.                EL523
02522      MOVE CAR-AH-OB-TOT-BAD       TO  WS-D1-AMT-2.                EL523
02523      MOVE CAR-AH-OB-CNT-TOT-ALL   TO  WS-D1-CNT-3.                EL523
02524      MOVE CAR-AH-OB-TOT-ALL       TO  WS-D1-AMT-3.                EL523
02525      MOVE WS-DETAIL1              TO  P-DATA.                     EL523
02526      MOVE SPACE-1                 TO  P-CTL.                      EL523
02527                                                                   EL523
02528      PERFORM WRITE-A-LINE.                                        EL523
02529                                                                   EL523
02530      MOVE SPACES              TO  WS-DETAIL1.                     EL523
02531                                                                   EL523
02532  2235-LINE-8.                                                     EL523
02533      MOVE SPACES                 TO  WS-D1-LN-FILL1.              EL523
02534      MOVE 'CANCELLED'            TO  WS-D1-LN-FILL2.              EL523
02535      MOVE CAR-AH-C-CNT-TOT-GOOD  TO  WS-D1-CNT-1.                 EL523
02536      MOVE CAR-AH-CAN-TOT-GOOD    TO  WS-D1-AMT-1.                 EL523
02537      MOVE CAR-AH-C-CNT-TOT-BAD   TO  WS-D1-CNT-2.                 EL523
02538      MOVE CAR-AH-CAN-TOT-BAD     TO  WS-D1-AMT-2.                 EL523
02539      MOVE CAR-AH-C-CNT-TOT-ALL   TO  WS-D1-CNT-3.                 EL523
02540      MOVE CAR-AH-CAN-TOT-ALL     TO  WS-D1-AMT-3.                 EL523
02541      MOVE WS-DETAIL1             TO  P-DATA.                      EL523
02542      MOVE SPACE-1                TO  P-CTL.                       EL523
02543                                                                   EL523
02544      PERFORM WRITE-A-LINE.                                        EL523
02545                                                                   EL523
02546      MOVE SPACES               TO  WS-DETAIL1.                    EL523
02547                                                                   EL523
02548  2240-LINE-9.                                                     EL523
02549      MOVE SPACES               TO  WS-D1-LN-FILL1.                EL523
02550      MOVE 'NET PREMIUM'        TO  WS-D1-LN-FILL2.                EL523
02551      MOVE CAR-AH-NET-TOT-GOOD  TO  WS-D1-AMT-1.                   EL523
02552      MOVE CAR-AH-NET-TOT-BAD   TO  WS-D1-AMT-2.                   EL523
02553      MOVE CAR-AH-NET-TOT-ALL   TO  WS-D1-AMT-3.                   EL523
02554      MOVE WS-DETAIL1           TO  P-DATA.                        EL523
02555      MOVE SPACE-1              TO  P-CTL.                         EL523
02556                                                                   EL523
02557      PERFORM WRITE-A-LINE.                                        EL523
02558                                                                   EL523
02559      MOVE SPACES               TO  WS-DETAIL1.                    EL523
02560                                                                   EL523
02561  2245-LINE-10.                                                    EL523
02562      MOVE SPACES               TO  WS-D1-LN-FILL1.                EL523
02563      MOVE 'NET COMMISSION'     TO  WS-D1-LN-FILL2.                EL523
02564      MOVE CAR-AH-COM-TOT-GOOD  TO  WS-D1-AMT-1.                   EL523
02565      MOVE CAR-AH-COM-TOT-BAD   TO  WS-D1-AMT-2.                   EL523
02566      MOVE CAR-AH-COM-TOT-ALL   TO  WS-D1-AMT-3.                   EL523
02567      MOVE WS-DETAIL1           TO  P-DATA.                        EL523
02568      MOVE SPACE-1              TO  P-CTL.                         EL523
02569                                                                   EL523
02570      PERFORM WRITE-A-LINE.                                        EL523
02571                                                                   EL523
02572      MOVE SPACES               TO  WS-DETAIL1.                    EL523
02573                                                                   EL523
02574  2250-LINE-11.                                                    EL523
02575      MOVE 'TOTAL '    TO  WS-D1-LN-FILL1.                         EL523
02576      MOVE 'WRITTEN'  TO  WS-D1-LN-FILL2.                          EL523
02577      MOVE CAR-SP-I-CNT-TOT-GOOD TO WS-D1-CNT-1.                   EL523
02578                                                                   EL523
02579      COMPUTE  WS-D1-AMT-1  =  CAR-LF-AMT-TOT-GOOD                 EL523
02580                            +  CAR-AH-AMT-TOT-GOOD.                EL523
02581                                                                   EL523
02582      MOVE CAR-SP-I-CNT-TOT-BAD  TO WS-D1-CNT-2.                   EL523
02583                                                                   EL523
02584      COMPUTE  WS-D1-AMT-2  =  CAR-LF-AMT-TOT-BAD                  EL523
02585                            +  CAR-AH-AMT-TOT-BAD.                 EL523
02586      COMPUTE  WS-D1-CNT-3  =  CAR-SP-I-CNT-TOT-GOOD               EL523
02587                            +  CAR-SP-I-CNT-TOT-BAD.               EL523
02588      COMPUTE  WS-D1-AMT-3  =  CAR-LF-AMT-TOT-ALL                  EL523
02589                            +  CAR-AH-AMT-TOT-ALL.                 EL523
02590                                                                   EL523
02591      MOVE WS-DETAIL1 TO  P-DATA.                                  EL523
02592      MOVE SPACE-2    TO  P-CTL.                                   EL523
02593                                                                   EL523
02594      PERFORM WRITE-A-LINE.                                        EL523
02595                                                                   EL523
02596      MOVE SPACES     TO  WS-DETAIL1.                              EL523
02597                                                                   EL523
02598  2255-LINE-12.                                                    EL523
02599      MOVE SPACES         TO  WS-D1-LN-FILL1.                      EL523
02600      MOVE 'OUTSTND BAL'  TO  WS-D1-LN-FILL2.                      EL523
02601                                                                   EL523
02602      MOVE CAR-OB-I-CNT-TOT-GOOD TO WS-D1-CNT-1.                   EL523
02603                                                                   EL523
02604      COMPUTE  WS-D1-AMT-1  =  CAR-LF-OB-TOT-GOOD                  EL523
02605                            +  CAR-AH-OB-TOT-GOOD.                 EL523
02606                                                                   EL523
02607      MOVE CAR-OB-I-CNT-TOT-BAD  TO WS-D1-CNT-2.                   EL523
02608                                                                   EL523
02609      COMPUTE  WS-D1-AMT-2  =  CAR-LF-OB-TOT-BAD                   EL523
02610                            +  CAR-AH-OB-TOT-BAD.                  EL523
02611      COMPUTE  WS-D1-CNT-3  =  CAR-OB-I-CNT-TOT-GOOD               EL523
02612                            +  CAR-OB-I-CNT-TOT-BAD.               EL523
02613      COMPUTE WS-D1-AMT-3   =  CAR-LF-OB-TOT-ALL                   EL523
02614                            +  CAR-AH-OB-TOT-ALL.                  EL523
02615                                                                   EL523
02616      MOVE WS-DETAIL1     TO  P-DATA.                              EL523
02617      MOVE SPACE-1        TO  P-CTL.                               EL523
02618                                                                   EL523
02619      PERFORM WRITE-A-LINE.                                        EL523
02620                                                                   EL523
02621      MOVE SPACES         TO  WS-DETAIL1.                          EL523
02622                                                                   EL523
02623  2260-LINE-13.                                                    EL523
02624      MOVE SPACES       TO  WS-D1-LN-FILL1.                        EL523
02625      MOVE 'CANCELLED'  TO  WS-D1-LN-FILL2.                        EL523
02626      MOVE CAR-C-CNT-TOT-GOOD  TO WS-D1-CNT-1.                     EL523
02627                                                                   EL523
02628      COMPUTE  WS-D1-AMT-1  =  CAR-LF-CAN-TOT-GOOD                 EL523
02629                            +  CAR-AH-CAN-TOT-GOOD.                EL523
02630                                                                   EL523
02631      MOVE CAR-C-CNT-TOT-BAD   TO WS-D1-CNT-2.                     EL523
02632                                                                   EL523
02633      COMPUTE  WS-D1-AMT-2  =  CAR-LF-CAN-TOT-BAD                  EL523
02634                            +  CAR-AH-CAN-TOT-BAD.                 EL523
02635      COMPUTE  WS-D1-CNT-3  =  CAR-C-CNT-TOT-GOOD                  EL523
02636                            +  CAR-C-CNT-TOT-BAD.                  EL523
02637      COMPUTE  WS-D1-AMT-3  =  CAR-LF-CAN-TOT-ALL                  EL523
02638                            +  CAR-AH-CAN-TOT-ALL.                 EL523
02639                                                                   EL523
02640      MOVE WS-DETAIL1   TO  P-DATA.                                EL523
02641      MOVE SPACE-1      TO  P-CTL.                                 EL523
02642                                                                   EL523
02643      PERFORM WRITE-A-LINE.                                        EL523
02644                                                                   EL523
02645      MOVE SPACES       TO  WS-DETAIL1.                            EL523
02646                                                                   EL523
02647  2265-LINE-14.                                                    EL523
02648      MOVE SPACES         TO  WS-D1-LN-FILL1.                      EL523
02649      MOVE 'NET PREMIUM'  TO  WS-D1-LN-FILL2.                      EL523
02650                                                                   EL523
02651      COMPUTE  WS-D1-AMT-1  =  CAR-LF-NET-TOT-GOOD                 EL523
02652                            +  CAR-AH-NET-TOT-GOOD.                EL523
02653      COMPUTE  WS-D1-AMT-2  =  CAR-LF-NET-TOT-BAD                  EL523
02654                            +  CAR-AH-NET-TOT-BAD.                 EL523
02655      COMPUTE  WS-D1-AMT-3  =  CAR-LF-NET-TOT-ALL                  EL523
02656                            +  CAR-AH-NET-TOT-ALL.                 EL523
02657                                                                   EL523
02658      MOVE WS-DETAIL1     TO  P-DATA.                              EL523
02659      MOVE SPACE-1        TO  P-CTL.                               EL523
02660                                                                   EL523
02661      PERFORM WRITE-A-LINE.                                        EL523
02662                                                                   EL523
02663      MOVE SPACES         TO  WS-DETAIL1.                          EL523
02664                                                                   EL523
02665  2270-LINE-15.                                                    EL523
02666      MOVE SPACES            TO  WS-D1-LN-FILL1.                   EL523
02667      MOVE 'NET COMMISSION'  TO  WS-D1-LN-FILL2.                   EL523
02668                                                                   EL523
02669      COMPUTE  WS-D1-AMT-1  =  CAR-LF-COM-TOT-GOOD                 EL523
02670                            +  CAR-AH-COM-TOT-GOOD.                EL523
02671      COMPUTE  WS-D1-AMT-2  =  CAR-LF-COM-TOT-BAD                  EL523
02672                            +  CAR-AH-COM-TOT-BAD.                 EL523
02673      COMPUTE  WS-D1-AMT-3  =  CAR-LF-COM-TOT-ALL                  EL523
02674                            +  CAR-AH-COM-TOT-ALL.                 EL523
02675                                                                   EL523
02676      MOVE WS-DETAIL1        TO  P-DATA.                           EL523
02677      MOVE SPACE-1           TO  P-CTL.                            EL523
02678                                                                   EL523
02679      PERFORM WRITE-A-LINE.                                        EL523
02680                                                                   EL523
02681      MOVE SPACES            TO  WS-DETAIL1.                       EL523
02682                                                                   EL523
02683  2272-ROLL-TOTALS.                                                EL523
02684      COMPUTE  FIN-LF-AMT-TOT-GOOD  =  FIN-LF-AMT-TOT-GOOD         EL523
02685                                    +  CAR-LF-AMT-TOT-GOOD.        EL523
02686      COMPUTE  FIN-LF-AMT-TOT-BAD   =  FIN-LF-AMT-TOT-BAD          EL523
02687                                    +  CAR-LF-AMT-TOT-BAD.         EL523
02688      COMPUTE  FIN-LF-AMT-TOT-ALL   =  FIN-LF-AMT-TOT-ALL          EL523
02689                                    +  CAR-LF-AMT-TOT-ALL.         EL523
02690                                                                   EL523
02691      COMPUTE  FIN-SP-I-CNT-TOT-GOOD  =  FIN-SP-I-CNT-TOT-GOOD     EL523
02692                                      +  CAR-SP-I-CNT-TOT-GOOD.    EL523
02693      COMPUTE  FIN-LF-I-CNT-TOT-GOOD  =  FIN-LF-I-CNT-TOT-GOOD     EL523
02694                                      +  CAR-LF-I-CNT-TOT-GOOD.    EL523
02695      COMPUTE  FIN-SP-I-CNT-TOT-BAD   =  FIN-SP-I-CNT-TOT-BAD      EL523
02696                                      +  CAR-SP-I-CNT-TOT-BAD.     EL523
02697      COMPUTE  FIN-LF-I-CNT-TOT-BAD   =  FIN-LF-I-CNT-TOT-BAD      EL523
02698                                      +  CAR-LF-I-CNT-TOT-BAD.     EL523
02699      COMPUTE  FIN-LF-I-CNT-TOT-ALL   =  FIN-LF-I-CNT-TOT-ALL      EL523
02700                                      +  CAR-LF-I-CNT-TOT-ALL.     EL523
02701                                                                   EL523
02702      COMPUTE  FIN-LF-OB-TOT-GOOD  =  FIN-LF-OB-TOT-GOOD           EL523
02703                                   +  CAR-LF-OB-TOT-GOOD.          EL523
02704      COMPUTE  FIN-LF-OB-TOT-BAD   =  FIN-LF-OB-TOT-BAD            EL523
02705                                   +  CAR-LF-OB-TOT-BAD.           EL523
02706      COMPUTE  FIN-LF-OB-TOT-ALL   =  FIN-LF-OB-TOT-ALL            EL523
02707                                   +  CAR-LF-OB-TOT-ALL.           EL523
02708                                                                   EL523
02709      COMPUTE  FIN-OB-I-CNT-TOT-GOOD  =  FIN-OB-I-CNT-TOT-GOOD     EL523
02710                                      +  CAR-OB-I-CNT-TOT-GOOD.    EL523
02711      COMPUTE  FIN-LF-OB-CNT-TOT-GOOD  =  FIN-LF-OB-CNT-TOT-GOOD   EL523
02712                                       +  CAR-LF-OB-CNT-TOT-GOOD.  EL523
02713      COMPUTE  FIN-OB-I-CNT-TOT-BAD   =  FIN-OB-I-CNT-TOT-BAD      EL523
02714                                      +  CAR-OB-I-CNT-TOT-BAD.     EL523
02715      COMPUTE  FIN-LF-OB-CNT-TOT-BAD   =  FIN-LF-OB-CNT-TOT-BAD    EL523
02716                                       +  CAR-LF-OB-CNT-TOT-BAD.   EL523
02717      COMPUTE  FIN-LF-OB-CNT-TOT-ALL   =  FIN-LF-OB-CNT-TOT-ALL    EL523
02718                                       +  CAR-LF-OB-CNT-TOT-ALL.   EL523
02719                                                                   EL523
02720      COMPUTE  FIN-LF-CAN-TOT-GOOD  =  FIN-LF-CAN-TOT-GOOD         EL523
02721                                    +  CAR-LF-CAN-TOT-GOOD.        EL523
02722      COMPUTE  FIN-LF-CAN-TOT-BAD   =  FIN-LF-CAN-TOT-BAD          EL523
02723                                    +  CAR-LF-CAN-TOT-BAD.         EL523
02724      COMPUTE  FIN-LF-CAN-TOT-ALL   =  FIN-LF-CAN-TOT-ALL          EL523
02725                                    +  CAR-LF-CAN-TOT-ALL.         EL523
02726                                                                   EL523
02727      COMPUTE  FIN-C-CNT-TOT-GOOD     =  FIN-C-CNT-TOT-GOOD        EL523
02728                                      +  CAR-C-CNT-TOT-GOOD.       EL523
02729      COMPUTE  FIN-LF-C-CNT-TOT-GOOD  =  FIN-LF-C-CNT-TOT-GOOD     EL523
02730                                      +  CAR-LF-C-CNT-TOT-GOOD.    EL523
02731      COMPUTE  FIN-C-CNT-TOT-BAD      =  FIN-C-CNT-TOT-BAD         EL523
02732                                      +  CAR-C-CNT-TOT-BAD.        EL523
02733      COMPUTE  FIN-LF-C-CNT-TOT-BAD   =  FIN-LF-C-CNT-TOT-BAD      EL523
02734                                      +  CAR-LF-C-CNT-TOT-BAD.     EL523
02735      COMPUTE  FIN-LF-C-CNT-TOT-ALL   =  FIN-LF-C-CNT-TOT-ALL      EL523
02736                                      +  CAR-LF-C-CNT-TOT-ALL.     EL523
02737                                                                   EL523
02738      COMPUTE  FIN-LF-NET-TOT-GOOD  =  FIN-LF-NET-TOT-GOOD         EL523
02739                                    +  CAR-LF-NET-TOT-GOOD.        EL523
02740      COMPUTE  FIN-LF-NET-TOT-BAD   =  FIN-LF-NET-TOT-BAD          EL523
02741                                    +  CAR-LF-NET-TOT-BAD.         EL523
02742      COMPUTE  FIN-LF-NET-TOT-ALL   =  FIN-LF-NET-TOT-ALL          EL523
02743                                    +  CAR-LF-NET-TOT-ALL.         EL523
02744                                                                   EL523
02745      COMPUTE  FIN-LF-COM-TOT-GOOD  =  FIN-LF-COM-TOT-GOOD         EL523
02746                                    +  CAR-LF-COM-TOT-GOOD.        EL523
02747      COMPUTE  FIN-LF-COM-TOT-BAD   =  FIN-LF-COM-TOT-BAD          EL523
02748                                    +  CAR-LF-COM-TOT-BAD.         EL523
02749      COMPUTE  FIN-LF-COM-TOT-ALL   =  FIN-LF-COM-TOT-ALL          EL523
02750                                    +  CAR-LF-COM-TOT-ALL.         EL523
02751                                                                   EL523
02752      COMPUTE  FIN-AH-AMT-TOT-GOOD  =  FIN-AH-AMT-TOT-GOOD         EL523
02753                                    +  CAR-AH-AMT-TOT-GOOD.        EL523
02754      COMPUTE  FIN-AH-AMT-TOT-BAD   =  FIN-AH-AMT-TOT-BAD          EL523
02755                                    +  CAR-AH-AMT-TOT-BAD.         EL523
02756      COMPUTE  FIN-AH-AMT-TOT-ALL   =  FIN-AH-AMT-TOT-ALL          EL523
02757                                    +  CAR-AH-AMT-TOT-ALL.         EL523
02758                                                                   EL523
02759      COMPUTE  FIN-AH-I-CNT-TOT-GOOD  =  FIN-AH-I-CNT-TOT-GOOD     EL523
02760                                      +  CAR-AH-I-CNT-TOT-GOOD.    EL523
02761      COMPUTE  FIN-AH-I-CNT-TOT-BAD   =  FIN-AH-I-CNT-TOT-BAD      EL523
02762                                      +  CAR-AH-I-CNT-TOT-BAD.     EL523
02763      COMPUTE  FIN-AH-I-CNT-TOT-ALL   =  FIN-AH-I-CNT-TOT-ALL      EL523
02764                                      +  CAR-AH-I-CNT-TOT-ALL.     EL523
02765                                                                   EL523
02766      COMPUTE  FIN-AH-OB-TOT-GOOD  =  FIN-AH-OB-TOT-GOOD           EL523
02767                                   +  CAR-AH-OB-TOT-GOOD.          EL523
02768      COMPUTE  FIN-AH-OB-TOT-BAD   =  FIN-AH-OB-TOT-BAD            EL523
02769                                   +  CAR-AH-OB-TOT-BAD.           EL523
02770      COMPUTE  FIN-AH-OB-TOT-ALL   =  FIN-AH-OB-TOT-ALL            EL523
02771                                   +  CAR-AH-OB-TOT-ALL.           EL523
02772                                                                   EL523
02773      COMPUTE  FIN-AH-OB-CNT-TOT-GOOD  =  FIN-AH-OB-CNT-TOT-GOOD   EL523
02774                                       +  CAR-AH-OB-CNT-TOT-GOOD.  EL523
02775      COMPUTE  FIN-AH-OB-CNT-TOT-BAD   =  FIN-AH-OB-CNT-TOT-BAD    EL523
02776                                       +  CAR-AH-OB-CNT-TOT-BAD.   EL523
02777      COMPUTE  FIN-AH-OB-CNT-TOT-ALL   =  FIN-AH-OB-CNT-TOT-ALL    EL523
02778                                       +  CAR-AH-OB-CNT-TOT-ALL.   EL523
02779                                                                   EL523
02780      COMPUTE  FIN-AH-CAN-TOT-GOOD  =  FIN-AH-CAN-TOT-GOOD         EL523
02781                                    +  CAR-AH-CAN-TOT-GOOD.        EL523
02782      COMPUTE  FIN-AH-CAN-TOT-BAD   =  FIN-AH-CAN-TOT-BAD          EL523
02783                                    +  CAR-AH-CAN-TOT-BAD.         EL523
02784      COMPUTE  FIN-AH-CAN-TOT-ALL   =  FIN-AH-CAN-TOT-ALL          EL523
02785                                    +  CAR-AH-CAN-TOT-ALL.         EL523
02786                                                                   EL523
02787      COMPUTE  FIN-AH-C-CNT-TOT-GOOD  =  FIN-AH-C-CNT-TOT-GOOD     EL523
02788                                      +  CAR-AH-C-CNT-TOT-GOOD.    EL523
02789      COMPUTE  FIN-AH-C-CNT-TOT-BAD   =  FIN-AH-C-CNT-TOT-BAD      EL523
02790                                      +  CAR-AH-C-CNT-TOT-BAD.     EL523
02791      COMPUTE  FIN-AH-C-CNT-TOT-ALL   =  FIN-AH-C-CNT-TOT-ALL      EL523
02792                                      +  CAR-AH-C-CNT-TOT-ALL.     EL523
02793                                                                   EL523
02794      COMPUTE  FIN-AH-NET-TOT-GOOD  =  FIN-AH-NET-TOT-GOOD         EL523
02795                                    +  CAR-AH-NET-TOT-GOOD.        EL523
02796      COMPUTE  FIN-AH-NET-TOT-BAD   =  FIN-AH-NET-TOT-BAD          EL523
02797                                    +  CAR-AH-NET-TOT-BAD.         EL523
02798      COMPUTE  FIN-AH-NET-TOT-ALL   =  FIN-AH-NET-TOT-ALL          EL523
02799                                    +  CAR-AH-NET-TOT-ALL.         EL523
02800                                                                   EL523
02801      COMPUTE  FIN-AH-COM-TOT-GOOD  =  FIN-AH-COM-TOT-GOOD         EL523
02802                                    +  CAR-AH-COM-TOT-GOOD.        EL523
02803      COMPUTE  FIN-AH-COM-TOT-BAD   =  FIN-AH-COM-TOT-BAD          EL523
02804                                    +  CAR-AH-COM-TOT-BAD.         EL523
02805      COMPUTE  FIN-AH-COM-TOT-ALL   =  FIN-AH-COM-TOT-ALL          EL523
02806                                    +  CAR-AH-COM-TOT-ALL.         EL523
02807                                                                   EL523
02808  2275-ZERO.                                                       EL523
02809      MOVE ZEROS  TO  CAR-LF-AMT-TOT-GOOD  CAR-LF-OB-TOT-GOOD      EL523
02810                      CAR-LF-CAN-TOT-GOOD  CAR-LF-NET-TOT-GOOD     EL523
02811                      CAR-AH-AMT-TOT-GOOD  CAR-AH-OB-TOT-GOOD      EL523
02812                      CAR-AH-CAN-TOT-GOOD  CAR-AH-NET-TOT-GOOD     EL523
02813                      CAR-LF-COM-TOT-GOOD  CAR-AH-COM-TOT-GOOD     EL523
02814                      CAR-LF-I-CNT-TOT-GOOD                        EL523
02815                      CAR-LF-OB-CNT-TOT-GOOD                       EL523
02816                      CAR-LF-C-CNT-TOT-GOOD                        EL523
02817                      CAR-AH-I-CNT-TOT-GOOD                        EL523
02818                      CAR-AH-OB-CNT-TOT-GOOD                       EL523
02819                      CAR-AH-C-CNT-TOT-GOOD                        EL523
02820                      CAR-LF-AMT-TOT-BAD  CAR-LF-OB-TOT-BAD        EL523
02821                      CAR-LF-CAN-TOT-BAD  CAR-LF-NET-TOT-BAD       EL523
02822                      CAR-AH-AMT-TOT-BAD  CAR-AH-OB-TOT-BAD        EL523
02823                      CAR-AH-CAN-TOT-BAD  CAR-AH-NET-TOT-BAD       EL523
02824                      CAR-LF-COM-TOT-BAD  CAR-AH-COM-TOT-BAD       EL523
02825                      CAR-LF-I-CNT-TOT-BAD                         EL523
02826                      CAR-LF-OB-CNT-TOT-BAD                        EL523
02827                      CAR-LF-C-CNT-TOT-BAD                         EL523
02828                      CAR-AH-I-CNT-TOT-BAD                         EL523
02829                      CAR-AH-OB-CNT-TOT-BAD                        EL523
02830                      CAR-AH-C-CNT-TOT-BAD                         EL523
02831                      CAR-LF-AMT-TOT-ALL  CAR-LF-OB-TOT-ALL        EL523
02832                      CAR-LF-CAN-TOT-ALL  CAR-LF-NET-TOT-ALL       EL523
02833                      CAR-AH-AMT-TOT-ALL  CAR-AH-OB-TOT-ALL        EL523
02834                      CAR-AH-CAN-TOT-ALL  CAR-AH-NET-TOT-ALL       EL523
02835                      CAR-LF-COM-TOT-ALL  CAR-AH-COM-TOT-ALL       EL523
02836                      CAR-LF-I-CNT-TOT-ALL                         EL523
02837                      CAR-LF-OB-CNT-TOT-ALL                        EL523
02838                      CAR-LF-C-CNT-TOT-ALL                         EL523
02839                      CAR-AH-I-CNT-TOT-ALL                         EL523
02840                      CAR-AH-OB-CNT-TOT-ALL                        EL523
02841                      CAR-AH-C-CNT-TOT-ALL                         EL523
02842                      CAR-SP-I-CNT-TOT-GOOD CAR-SP-I-CNT-TOT-BAD   EL523
02843                      CAR-OB-I-CNT-TOT-GOOD CAR-OB-I-CNT-TOT-BAD   EL523
02844                      CAR-C-CNT-TOT-GOOD    CAR-C-CNT-TOT-BAD.     EL523
02845                                                                   EL523
02846      PERFORM 2600-FLOP-STATE   THRU 2699-EXIT.                    EL523
02847      PERFORM 2800-FLOP-GROUP   THRU 2899-EXIT.                    EL523
02848                                                                   EL523
02849  2280-SET-CARRIER.                                                EL523
02850      MOVE ACT-PREFIX  TO  PREV-CARRIER  WS-H4-CARRIER-NO.         EL523
02851                                                                   EL523
02852      IF CARRIER-SUB (1) = SPACE                                   EL523
02853          GO TO 2290-OUT-S-C-LOOP.                                 EL523
02854                                                                   EL523
02855      MOVE +0  TO  S1.                                             EL523
02856                                                                   EL523
02857  2285-S-C-LOOP.                                                   EL523
02858      ADD  +1  TO  S1.                                             EL523
02859                                                                   EL523
02860      IF S1 GREATER THAN CLAS-MAXCN                                EL523
02861          IF PRT-CARRIER-TOTAL                                     EL523
02862              MOVE 'UNKNOWN CARRIER'  TO  WS-H4-CARRIER            EL523
02863              GO TO 2290-OUT-S-C-LOOP                              EL523
02864          ELSE                                                     EL523
02865              MOVE SPACES             TO  WS-H4-CARRIER            EL523
02866              GO TO 2290-OUT-S-C-LOOP.                             EL523
02867                                                                   EL523
02868      IF ACT-PREFIX = CARRIER-SUB (S1)                             EL523
02869          MOVE CARRIER-PIC (S1)  TO  WS-H4-CARRIER                 EL523
02870          GO TO 2290-OUT-S-C-LOOP.                                 EL523
02871                                                                   EL523
02872      GO TO 2285-S-C-LOOP.                                         EL523
02873                                                                   EL523
02874  2290-OUT-S-C-LOOP.                                               EL523
02875      IF LAST-TIME                                                 EL523
02876          MOVE SPACES  TO  WS-HEADING4                             EL523
02877                           WS-HEADING5                             EL523
02878                           WS-HEADING5A                            EL523
02879                           WS-HEADING6.                            EL523
02880                                                                   EL523
02881      IF DTE-FMT-OPT  =  3                                         EL523
02882        IF HEADERS-PRINTED-SW  =  'Y'                              EL523
02883          MOVE 'ACCOUNTS WITH NO BUSINESS SUBMITTED FOR THE PERIOD'EL523
02884                                       TO  WS-H1-TITLE             EL523
02885            PERFORM WRITE-HEADINGS                                 EL523
02886            GO TO 2299-EXIT                                        EL523
02887        ELSE                                                       EL523
02888            GO TO 2299-EXIT.                                       EL523
02889                                                                   EL523
02890      IF WS-H4-CARRIER EQUAL SPACES AND                            EL523
02891         WS-H5-GROUP EQUAL SPACES   AND                            EL523
02892         WS-H6-STATE-NO EQUAL SPACES                               EL523
02893         NEXT SENTENCE                                             EL523
02894      ELSE                                                         EL523
02895          MOVE '            NET PREMIUM REPORT                    'EL523
02896                                       TO  WS-H1-TITLE             EL523
02897         PERFORM WRITE-HEADINGS.                                   EL523
02898                                                                   EL523
02899  2299-EXIT.                                                       EL523
02900      EXIT.                                                        EL523
02901  EJECT                                                            EL523
02902  2300-GROUP-BREAK.                                                EL523
02903      PERFORM 2400-STATE-BREAK THRU 2499-EXIT.                     EL523
02904                                                                   EL523
02905      IF FIRST-TIME                                                EL523
02906          GO TO 2380-SET-GROUP.                                    EL523
02907                                                                   EL523
02908      MOVE WS-H5-GROUP   TO  SAV-OLD-GROUP.                        EL523
02909      MOVE WS-H5A-RPT2   TO  SAV-OLD-RPT2.                         EL523
02910                                                                   EL523
02911      IF DTE-FMT-OPT  =  3                                         EL523
02912            GO TO 2380-SET-GROUP.                                  EL523
02913                                                                   EL523
02914      PERFORM 2500-FLIP-STATE THRU 2599-EXIT.                      EL523
02915                                                                   EL523
02916      IF DTE-CLIENT NOT = 'MON'                                    EL523
02917          IF PRT-GROUP-TOTAL                                       EL523
02918              NEXT SENTENCE                                        EL523
02919          ELSE                                                     EL523
02920              GO TO 2372-ROLL-TOTALS.                              EL523
02921                                                                   EL523
02922      COMPUTE  WS-LINE-COUNT-TST  =  WS-LINE-COUNT  +  +19.        EL523
02923                                                                   EL523
02924      IF WS-LINE-COUNT-TST GREATER THAN +57                        EL523
02925          MOVE '            NET PREMIUM REPORT                    'EL523
02926                                       TO  WS-H1-TITLE             EL523
02927          PERFORM WRITE-HEADINGS.                                  EL523
02928                                                                   EL523
02929      MOVE SPACES   TO  P-DATA.                                    EL523
02930      MOVE SPACE-2  TO  P-CTL.                                     EL523
02931                                                                   EL523
02932      PERFORM WRITE-A-LINE.                                        EL523
02933                                                                   EL523
02934      MOVE TOT-MSG-2             TO  WS-D1-LN.                     EL523
02935      MOVE LIFE-OVERRIDE-L6      TO  WS-D1-LN-FILL1.               EL523
02936      MOVE 'WRITTEN'             TO  WS-D1-LN-FILL2.               EL523
02937      MOVE CO-LF-I-CNT-TOT-GOOD  TO  WS-D1-CNT-1.                  EL523
02938      MOVE CO-LF-AMT-TOT-GOOD    TO  WS-D1-AMT-1.                  EL523
02939      MOVE CO-LF-I-CNT-TOT-BAD   TO  WS-D1-CNT-2.                  EL523
02940      MOVE CO-LF-AMT-TOT-BAD     TO  WS-D1-AMT-2.                  EL523
02941      MOVE CO-LF-I-CNT-TOT-ALL   TO  WS-D1-CNT-3.                  EL523
02942      MOVE CO-LF-AMT-TOT-ALL     TO  WS-D1-AMT-3.                  EL523
02943      MOVE WS-DETAIL1            TO  P-DATA.                       EL523
02944      MOVE SPACE-3               TO  P-CTL.                        EL523
02945                                                                   EL523
02946      PERFORM WRITE-A-LINE.                                        EL523
02947                                                                   EL523
02948      MOVE SPACES              TO  WS-DETAIL1.                     EL523
02949                                                                   EL523
02950  2305-LINE-2.                                                     EL523
02951      MOVE SPACES                 TO  WS-D1-LN-FILL1.              EL523
02952      MOVE 'OUTSTND BAL'          TO  WS-D1-LN-FILL2.              EL523
02953      MOVE CO-LF-OB-CNT-TOT-GOOD  TO  WS-D1-CNT-1.                 EL523
02954      MOVE CO-LF-OB-TOT-GOOD      TO  WS-D1-AMT-1.                 EL523
02955      MOVE CO-LF-OB-CNT-TOT-BAD   TO  WS-D1-CNT-2.                 EL523
02956      MOVE CO-LF-OB-TOT-BAD       TO  WS-D1-AMT-2.                 EL523
02957      MOVE CO-LF-OB-CNT-TOT-ALL   TO  WS-D1-CNT-3.                 EL523
02958      MOVE CO-LF-OB-TOT-ALL       TO  WS-D1-AMT-3.                 EL523
02959      MOVE WS-DETAIL1             TO  P-DATA.                      EL523
02960      MOVE SPACE-1                TO  P-CTL.                       EL523
02961                                                                   EL523
02962      PERFORM WRITE-A-LINE.                                        EL523
02963                                                                   EL523
02964      MOVE SPACES             TO  WS-DETAIL1.                      EL523
02965                                                                   EL523
02966  2310-LINE-3.                                                     EL523
02967      MOVE SPACES                TO  WS-D1-LN-FILL1.               EL523
02968      MOVE 'CANCELLED'           TO  WS-D1-LN-FILL2.               EL523
02969      MOVE CO-LF-C-CNT-TOT-GOOD  TO  WS-D1-CNT-1.                  EL523
02970      MOVE CO-LF-CAN-TOT-GOOD    TO  WS-D1-AMT-1.                  EL523
02971      MOVE CO-LF-C-CNT-TOT-BAD   TO  WS-D1-CNT-2.                  EL523
02972      MOVE CO-LF-CAN-TOT-BAD     TO  WS-D1-AMT-2.                  EL523
02973      MOVE CO-LF-C-CNT-TOT-ALL   TO  WS-D1-CNT-3.                  EL523
02974      MOVE CO-LF-CAN-TOT-ALL     TO  WS-D1-AMT-3.                  EL523
02975      MOVE WS-DETAIL1            TO  P-DATA.                       EL523
02976      MOVE SPACE-1               TO  P-CTL.                        EL523
02977                                                                   EL523
02978      PERFORM WRITE-A-LINE.                                        EL523
02979                                                                   EL523
02980      MOVE SPACES              TO  WS-DETAIL1.                     EL523
02981                                                                   EL523
02982  2315-LINE-4.                                                     EL523
02983      MOVE SPACES              TO  WS-D1-LN-FILL1.                 EL523
02984      MOVE 'NET PREMIUM'       TO  WS-D1-LN-FILL2.                 EL523
02985      MOVE CO-LF-NET-TOT-GOOD  TO  WS-D1-AMT-1.                    EL523
02986      MOVE CO-LF-NET-TOT-BAD   TO  WS-D1-AMT-2.                    EL523
02987      MOVE CO-LF-NET-TOT-ALL   TO  WS-D1-AMT-3.                    EL523
02988      MOVE WS-DETAIL1          TO  P-DATA.                         EL523
02989      MOVE SPACE-1             TO  P-CTL.                          EL523
02990                                                                   EL523
02991      PERFORM WRITE-A-LINE.                                        EL523
02992                                                                   EL523
02993      MOVE SPACES              TO  WS-DETAIL1.                     EL523
02994                                                                   EL523
02995  2320-LINE-5.                                                     EL523
02996      MOVE SPACES              TO  WS-D1-LN-FILL1.                 EL523
02997      MOVE 'NET COMMISSION'    TO  WS-D1-LN-FILL2.                 EL523
02998      MOVE CO-LF-COM-TOT-GOOD  TO  WS-D1-AMT-1.                    EL523
02999      MOVE CO-LF-COM-TOT-BAD   TO  WS-D1-AMT-2.                    EL523
03000      MOVE CO-LF-COM-TOT-ALL   TO  WS-D1-AMT-3.                    EL523
03001      MOVE WS-DETAIL1          TO  P-DATA.                         EL523
03002      MOVE SPACE-1             TO  P-CTL.                          EL523
03003                                                                   EL523
03004      PERFORM WRITE-A-LINE.                                        EL523
03005                                                                   EL523
03006      MOVE SPACES              TO  WS-DETAIL1.                     EL523
03007                                                                   EL523
03008  2325-LINE-6.                                                     EL523
03009      MOVE AH-OVERRIDE-L6        TO  WS-D1-LN-FILL1.               EL523
03010      MOVE 'WRITTEN'             TO  WS-D1-LN-FILL2.               EL523
03011      MOVE CO-AH-I-CNT-TOT-GOOD  TO  WS-D1-CNT-1.                  EL523
03012      MOVE CO-AH-AMT-TOT-GOOD    TO  WS-D1-AMT-1.                  EL523
03013      MOVE CO-AH-I-CNT-TOT-BAD   TO  WS-D1-CNT-2.                  EL523
03014      MOVE CO-AH-AMT-TOT-BAD     TO  WS-D1-AMT-2.                  EL523
03015      MOVE CO-AH-I-CNT-TOT-ALL   TO  WS-D1-CNT-3.                  EL523
03016      MOVE CO-AH-AMT-TOT-ALL     TO  WS-D1-AMT-3.                  EL523
03017      MOVE WS-DETAIL1            TO  P-DATA.                       EL523
03018      MOVE SPACE-2               TO  P-CTL.                        EL523
03019                                                                   EL523
03020      PERFORM WRITE-A-LINE.                                        EL523
03021                                                                   EL523
03022      MOVE SPACES              TO  WS-DETAIL1.                     EL523
03023                                                                   EL523
03024  2330-LINE-7.                                                     EL523
03025      MOVE SPACES                 TO  WS-D1-LN-FILL1.              EL523
03026      MOVE 'OUTSTND BAL'          TO  WS-D1-LN-FILL2.              EL523
03027      MOVE CO-AH-OB-CNT-TOT-GOOD  TO  WS-D1-CNT-1.                 EL523
03028      MOVE CO-AH-OB-TOT-GOOD      TO  WS-D1-AMT-1.                 EL523
03029      MOVE CO-AH-OB-CNT-TOT-BAD   TO  WS-D1-CNT-2.                 EL523
03030      MOVE CO-AH-OB-TOT-BAD       TO  WS-D1-AMT-2.                 EL523
03031      MOVE CO-AH-OB-CNT-TOT-ALL   TO  WS-D1-CNT-3.                 EL523
03032      MOVE CO-AH-OB-TOT-ALL       TO  WS-D1-AMT-3.                 EL523
03033      MOVE WS-DETAIL1             TO  P-DATA.                      EL523
03034      MOVE SPACE-1                TO  P-CTL.                       EL523
03035                                                                   EL523
03036      PERFORM WRITE-A-LINE.                                        EL523
03037                                                                   EL523
03038      MOVE SPACES             TO  WS-DETAIL1.                      EL523
03039                                                                   EL523
03040  2335-LINE-8.                                                     EL523
03041      MOVE SPACES                TO  WS-D1-LN-FILL1.               EL523
03042      MOVE 'CANCELLED'           TO  WS-D1-LN-FILL2.               EL523
03043      MOVE CO-AH-C-CNT-TOT-GOOD  TO  WS-D1-CNT-1.                  EL523
03044      MOVE CO-AH-CAN-TOT-GOOD    TO  WS-D1-AMT-1.                  EL523
03045      MOVE CO-AH-C-CNT-TOT-BAD   TO  WS-D1-CNT-2.                  EL523
03046      MOVE CO-AH-CAN-TOT-BAD     TO  WS-D1-AMT-2.                  EL523
03047      MOVE CO-AH-C-CNT-TOT-ALL   TO  WS-D1-CNT-3.                  EL523
03048      MOVE CO-AH-CAN-TOT-ALL     TO  WS-D1-AMT-3.                  EL523
03049      MOVE WS-DETAIL1            TO  P-DATA.                       EL523
03050      MOVE SPACE-1               TO  P-CTL.                        EL523
03051                                                                   EL523
03052      PERFORM WRITE-A-LINE.                                        EL523
03053                                                                   EL523
03054      MOVE SPACES              TO  WS-DETAIL1.                     EL523
03055                                                                   EL523
03056  2340-LINE-9.                                                     EL523
03057      MOVE SPACES              TO  WS-D1-LN-FILL1.                 EL523
03058      MOVE 'NET PREMIUM'       TO  WS-D1-LN-FILL2.                 EL523
03059      MOVE CO-AH-NET-TOT-GOOD  TO  WS-D1-AMT-1.                    EL523
03060      MOVE CO-AH-NET-TOT-BAD   TO  WS-D1-AMT-2.                    EL523
03061      MOVE CO-AH-NET-TOT-ALL   TO  WS-D1-AMT-3.                    EL523
03062      MOVE WS-DETAIL1          TO  P-DATA.                         EL523
03063      MOVE SPACE-1             TO  P-CTL.                          EL523
03064                                                                   EL523
03065      PERFORM WRITE-A-LINE.                                        EL523
03066                                                                   EL523
03067      MOVE SPACES              TO  WS-DETAIL1.                     EL523
03068                                                                   EL523
03069  2345-LINE-10.                                                    EL523
03070      MOVE SPACES              TO  WS-D1-LN-FILL1.                 EL523
03071      MOVE 'NET COMMISSION'    TO  WS-D1-LN-FILL2.                 EL523
03072      MOVE CO-AH-COM-TOT-GOOD  TO  WS-D1-AMT-1.                    EL523
03073      MOVE CO-AH-COM-TOT-BAD   TO  WS-D1-AMT-2.                    EL523
03074      MOVE CO-AH-COM-TOT-ALL   TO  WS-D1-AMT-3.                    EL523
03075      MOVE WS-DETAIL1          TO  P-DATA.                         EL523
03076      MOVE SPACE-1             TO  P-CTL.                          EL523
03077                                                                   EL523
03078      PERFORM WRITE-A-LINE.                                        EL523
03079                                                                   EL523
03080      MOVE SPACES              TO  WS-DETAIL1.                     EL523
03081                                                                   EL523
03082  2350-LINE-11.                                                    EL523
03083      MOVE 'TOTAL '             TO WS-D1-LN-FILL1.                 EL523
03084      MOVE 'WRITTEN'            TO WS-D1-LN-FILL2.                 EL523
03085      MOVE CO-SP-I-CNT-TOT-GOOD TO WS-D1-CNT-1.                    EL523
03086                                                                   EL523
03087      COMPUTE  WS-D1-AMT-1  =  CO-LF-AMT-TOT-GOOD                  EL523
03088                            +  CO-AH-AMT-TOT-GOOD.                 EL523
03089                                                                   EL523
03090      MOVE CO-SP-I-CNT-TOT-BAD  TO WS-D1-CNT-2.                    EL523
03091                                                                   EL523
03092      COMPUTE  WS-D1-AMT-2  =  CO-LF-AMT-TOT-BAD                   EL523
03093                            +  CO-AH-AMT-TOT-BAD.                  EL523
03094      COMPUTE  WS-D1-CNT-3  =  CO-SP-I-CNT-TOT-GOOD                EL523
03095                            +  CO-SP-I-CNT-TOT-BAD.                EL523
03096      COMPUTE  WS-D1-AMT-3  =  CO-LF-AMT-TOT-ALL                   EL523
03097                            +  CO-AH-AMT-TOT-ALL.                  EL523
03098                                                                   EL523
03099      MOVE WS-DETAIL1  TO  P-DATA.                                 EL523
03100      MOVE SPACE-2     TO  P-CTL.                                  EL523
03101                                                                   EL523
03102      PERFORM WRITE-A-LINE.                                        EL523
03103                                                                   EL523
03104      MOVE SPACES      TO  WS-DETAIL1.                             EL523
03105                                                                   EL523
03106  2355-LINE-12.                                                    EL523
03107      MOVE SPACES               TO WS-D1-LN-FILL1.                 EL523
03108      MOVE 'OUTSTND BAL'        TO WS-D1-LN-FILL2.                 EL523
03109      MOVE CO-OB-I-CNT-TOT-GOOD TO WS-D1-CNT-1.                    EL523
03110                                                                   EL523
03111      COMPUTE  WS-D1-AMT-1  =  CO-LF-OB-TOT-GOOD                   EL523
03112                            +  CO-AH-OB-TOT-GOOD.                  EL523
03113                                                                   EL523
03114      MOVE CO-OB-I-CNT-TOT-BAD  TO WS-D1-CNT-2.                    EL523
03115                                                                   EL523
03116      COMPUTE  WS-D1-AMT-2  =  CO-LF-OB-TOT-BAD                    EL523
03117                            +  CO-AH-OB-TOT-BAD.                   EL523
03118      COMPUTE  WS-D1-CNT-3  =  CO-OB-I-CNT-TOT-GOOD                EL523
03119                            +  CO-OB-I-CNT-TOT-BAD.                EL523
03120      COMPUTE  WS-D1-AMT-3  =  CO-LF-OB-TOT-ALL                    EL523
03121                            +  CO-AH-OB-TOT-ALL.                   EL523
03122                                                                   EL523
03123      MOVE WS-DETAIL1     TO  P-DATA.                              EL523
03124      MOVE SPACE-1        TO  P-CTL.                               EL523
03125                                                                   EL523
03126      PERFORM WRITE-A-LINE.                                        EL523
03127                                                                   EL523
03128      MOVE SPACES         TO  WS-DETAIL1.                          EL523
03129                                                                   EL523
03130  2360-LINE-13.                                                    EL523
03131      MOVE SPACES              TO WS-D1-LN-FILL1.                  EL523
03132      MOVE 'CANCELLED'         TO WS-D1-LN-FILL2.                  EL523
03133      MOVE CO-C-CNT-TOT-GOOD   TO WS-D1-CNT-1.                     EL523
03134                                                                   EL523
03135      COMPUTE  WS-D1-AMT-1  =  CO-LF-CAN-TOT-GOOD                  EL523
03136                            +  CO-AH-CAN-TOT-GOOD.                 EL523
03137                                                                   EL523
03138      MOVE CO-C-CNT-TOT-BAD    TO WS-D1-CNT-2.                     EL523
03139                                                                   EL523
03140      COMPUTE  WS-D1-AMT-2  =  CO-LF-CAN-TOT-BAD                   EL523
03141                            +  CO-AH-CAN-TOT-BAD.                  EL523
03142      COMPUTE  WS-D1-CNT-3  =  CO-C-CNT-TOT-GOOD                   EL523
03143                            +  CO-C-CNT-TOT-BAD.                   EL523
03144      COMPUTE  WS-D1-AMT-3  =  CO-LF-CAN-TOT-ALL                   EL523
03145                            +  CO-AH-CAN-TOT-ALL.                  EL523
03146                                                                   EL523
03147      MOVE WS-DETAIL1   TO  P-DATA.                                EL523
03148      MOVE SPACE-1      TO  P-CTL.                                 EL523
03149                                                                   EL523
03150      PERFORM WRITE-A-LINE.                                        EL523
03151                                                                   EL523
03152      MOVE SPACES       TO  WS-DETAIL1.                            EL523
03153                                                                   EL523
03154  2365-LINE-14.                                                    EL523
03155      MOVE SPACES         TO  WS-D1-LN-FILL1.                      EL523
03156      MOVE 'NET PREMIUM'  TO  WS-D1-LN-FILL2.                      EL523
03157                                                                   EL523
03158      COMPUTE  WS-D1-AMT-1  =  CO-LF-NET-TOT-GOOD                  EL523
03159                            +  CO-AH-NET-TOT-GOOD.                 EL523
03160      COMPUTE  WS-D1-AMT-2  =  CO-LF-NET-TOT-BAD                   EL523
03161                            +  CO-AH-NET-TOT-BAD.                  EL523
03162      COMPUTE  WS-D1-AMT-3  =  CO-LF-NET-TOT-ALL                   EL523
03163                            +  CO-AH-NET-TOT-ALL.                  EL523
03164                                                                   EL523
03165      MOVE WS-DETAIL1     TO  P-DATA.                              EL523
03166      MOVE SPACE-1        TO  P-CTL.                               EL523
03167                                                                   EL523
03168      PERFORM WRITE-A-LINE.                                        EL523
03169                                                                   EL523
03170      MOVE SPACES         TO  WS-DETAIL1.                          EL523
03171                                                                   EL523
03172  2370-LINE-15.                                                    EL523
03173      MOVE SPACES            TO  WS-D1-LN-FILL1.                   EL523
03174      MOVE 'NET COMMISSION'  TO  WS-D1-LN-FILL2.                   EL523
03175                                                                   EL523
03176      COMPUTE  WS-D1-AMT-1  =  CO-LF-COM-TOT-GOOD                  EL523
03177                            +  CO-AH-COM-TOT-GOOD.                 EL523
03178      COMPUTE  WS-D1-AMT-2  =  CO-LF-COM-TOT-BAD                   EL523
03179                            +  CO-AH-COM-TOT-BAD.                  EL523
03180      COMPUTE  WS-D1-AMT-3  =  CO-LF-COM-TOT-ALL                   EL523
03181                            +  CO-AH-COM-TOT-ALL.                  EL523
03182                                                                   EL523
03183      MOVE WS-DETAIL1        TO  P-DATA.                           EL523
03184      MOVE SPACE-1           TO  P-CTL.                            EL523
03185                                                                   EL523
03186      PERFORM WRITE-A-LINE.                                        EL523
03187                                                                   EL523
03188      MOVE SPACES            TO  WS-DETAIL1.                       EL523
03189                                                                   EL523
03190  2372-ROLL-TOTALS.                                                EL523
03191      COMPUTE  CAR-LF-AMT-TOT-GOOD  =  CAR-LF-AMT-TOT-GOOD         EL523
03192                                    +  CO-LF-AMT-TOT-GOOD.         EL523
03193      COMPUTE  CAR-LF-AMT-TOT-BAD   =  CAR-LF-AMT-TOT-BAD          EL523
03194                                    +  CO-LF-AMT-TOT-BAD.          EL523
03195      COMPUTE  CAR-LF-AMT-TOT-ALL   =  CAR-LF-AMT-TOT-ALL          EL523
03196                                    +  CO-LF-AMT-TOT-ALL.          EL523
03197                                                                   EL523
03198      COMPUTE  CAR-SP-I-CNT-TOT-GOOD  =  CAR-SP-I-CNT-TOT-GOOD     EL523
03199                                      +  CO-SP-I-CNT-TOT-GOOD.     EL523
03200      COMPUTE  CAR-LF-I-CNT-TOT-GOOD  =  CAR-LF-I-CNT-TOT-GOOD     EL523
03201                                      +  CO-LF-I-CNT-TOT-GOOD.     EL523
03202      COMPUTE  CAR-SP-I-CNT-TOT-BAD   =  CAR-SP-I-CNT-TOT-BAD      EL523
03203                                      +  CO-SP-I-CNT-TOT-BAD.      EL523
03204      COMPUTE  CAR-LF-I-CNT-TOT-BAD   =  CAR-LF-I-CNT-TOT-BAD      EL523
03205                                      +  CO-LF-I-CNT-TOT-BAD.      EL523
03206      COMPUTE  CAR-LF-I-CNT-TOT-ALL   =  CAR-LF-I-CNT-TOT-ALL      EL523
03207                                      +  CO-LF-I-CNT-TOT-ALL.      EL523
03208                                                                   EL523
03209      COMPUTE  CAR-LF-OB-TOT-GOOD  =  CAR-LF-OB-TOT-GOOD           EL523
03210                                   +  CO-LF-OB-TOT-GOOD.           EL523
03211      COMPUTE  CAR-LF-OB-TOT-BAD   =  CAR-LF-OB-TOT-BAD            EL523
03212                                   +  CO-LF-OB-TOT-BAD.            EL523
03213      COMPUTE  CAR-LF-OB-TOT-ALL   =  CAR-LF-OB-TOT-ALL            EL523
03214                                   +  CO-LF-OB-TOT-ALL.            EL523
03215                                                                   EL523
03216      COMPUTE  CAR-OB-I-CNT-TOT-GOOD  =  CAR-OB-I-CNT-TOT-GOOD     EL523
03217                                      +  CO-OB-I-CNT-TOT-GOOD.     EL523
03218      COMPUTE  CAR-LF-OB-CNT-TOT-GOOD  =  CAR-LF-OB-CNT-TOT-GOOD   EL523
03219                                       +  CO-LF-OB-CNT-TOT-GOOD.   EL523
03220      COMPUTE  CAR-OB-I-CNT-TOT-BAD   =  CAR-OB-I-CNT-TOT-BAD      EL523
03221                                      +  CO-OB-I-CNT-TOT-BAD.      EL523
03222      COMPUTE  CAR-LF-OB-CNT-TOT-BAD   =  CAR-LF-OB-CNT-TOT-BAD    EL523
03223                                       +  CO-LF-OB-CNT-TOT-BAD.    EL523
03224      COMPUTE  CAR-LF-OB-CNT-TOT-ALL   =  CAR-LF-OB-CNT-TOT-ALL    EL523
03225                                       +  CO-LF-OB-CNT-TOT-ALL.    EL523
03226                                                                   EL523
03227      COMPUTE  CAR-LF-CAN-TOT-GOOD  =  CAR-LF-CAN-TOT-GOOD         EL523
03228                                    +  CO-LF-CAN-TOT-GOOD.         EL523
03229      COMPUTE  CAR-LF-CAN-TOT-BAD   =  CAR-LF-CAN-TOT-BAD          EL523
03230                                    +  CO-LF-CAN-TOT-BAD.          EL523
03231      COMPUTE  CAR-LF-CAN-TOT-ALL   =  CAR-LF-CAN-TOT-ALL          EL523
03232                                    +  CO-LF-CAN-TOT-ALL.          EL523
03233                                                                   EL523
03234      COMPUTE  CAR-C-CNT-TOT-GOOD     =  CAR-C-CNT-TOT-GOOD        EL523
03235                                      +  CO-C-CNT-TOT-GOOD.        EL523
03236      COMPUTE  CAR-LF-C-CNT-TOT-GOOD  =  CAR-LF-C-CNT-TOT-GOOD     EL523
03237                                      +  CO-LF-C-CNT-TOT-GOOD.     EL523
03238      COMPUTE  CAR-C-CNT-TOT-BAD      =  CAR-C-CNT-TOT-BAD         EL523
03239                                      +  CO-C-CNT-TOT-BAD.         EL523
03240      COMPUTE  CAR-LF-C-CNT-TOT-BAD   =  CAR-LF-C-CNT-TOT-BAD      EL523
03241                                      +  CO-LF-C-CNT-TOT-BAD.      EL523
03242      COMPUTE  CAR-LF-C-CNT-TOT-ALL   =  CAR-LF-C-CNT-TOT-ALL      EL523
03243                                      +  CO-LF-C-CNT-TOT-ALL.      EL523
03244                                                                   EL523
03245      COMPUTE  CAR-LF-NET-TOT-GOOD  =  CAR-LF-NET-TOT-GOOD         EL523
03246                                    +  CO-LF-NET-TOT-GOOD.         EL523
03247      COMPUTE  CAR-LF-NET-TOT-BAD   =  CAR-LF-NET-TOT-BAD          EL523
03248                                    +  CO-LF-NET-TOT-BAD.          EL523
03249      COMPUTE  CAR-LF-NET-TOT-ALL   =  CAR-LF-NET-TOT-ALL          EL523
03250                                    +  CO-LF-NET-TOT-ALL.          EL523
03251                                                                   EL523
03252      COMPUTE  CAR-LF-COM-TOT-GOOD  =  CAR-LF-COM-TOT-GOOD         EL523
03253                                    +  CO-LF-COM-TOT-GOOD.         EL523
03254      COMPUTE  CAR-LF-COM-TOT-BAD   =  CAR-LF-COM-TOT-BAD          EL523
03255                                    +  CO-LF-COM-TOT-BAD.          EL523
03256      COMPUTE  CAR-LF-COM-TOT-ALL   =  CAR-LF-COM-TOT-ALL          EL523
03257                                    +  CO-LF-COM-TOT-ALL.          EL523
03258                                                                   EL523
03259      COMPUTE  CAR-AH-AMT-TOT-GOOD  =  CAR-AH-AMT-TOT-GOOD         EL523
03260                                    +  CO-AH-AMT-TOT-GOOD.         EL523
03261      COMPUTE  CAR-AH-AMT-TOT-BAD   =  CAR-AH-AMT-TOT-BAD          EL523
03262                                    +  CO-AH-AMT-TOT-BAD.          EL523
03263      COMPUTE  CAR-AH-AMT-TOT-ALL   =  CAR-AH-AMT-TOT-ALL          EL523
03264                                    +  CO-AH-AMT-TOT-ALL.          EL523
03265                                                                   EL523
03266      COMPUTE  CAR-AH-I-CNT-TOT-GOOD  =  CAR-AH-I-CNT-TOT-GOOD     EL523
03267                                      +  CO-AH-I-CNT-TOT-GOOD.     EL523
03268      COMPUTE  CAR-AH-I-CNT-TOT-BAD   =  CAR-AH-I-CNT-TOT-BAD      EL523
03269                                      +  CO-AH-I-CNT-TOT-BAD.      EL523
03270      COMPUTE  CAR-AH-I-CNT-TOT-ALL   =  CAR-AH-I-CNT-TOT-ALL      EL523
03271                                      +  CO-AH-I-CNT-TOT-ALL.      EL523
03272                                                                   EL523
03273      COMPUTE  CAR-AH-OB-TOT-GOOD  =  CAR-AH-OB-TOT-GOOD           EL523
03274                                   +  CO-AH-OB-TOT-GOOD.           EL523
03275      COMPUTE  CAR-AH-OB-TOT-BAD   =  CAR-AH-OB-TOT-BAD            EL523
03276                                   +  CO-AH-OB-TOT-BAD.            EL523
03277      COMPUTE  CAR-AH-OB-TOT-ALL   =  CAR-AH-OB-TOT-ALL            EL523
03278                                   +  CO-AH-OB-TOT-ALL.            EL523
03279                                                                   EL523
03280      COMPUTE  CAR-AH-OB-CNT-TOT-GOOD  =  CAR-AH-OB-CNT-TOT-GOOD   EL523
03281                                       +  CO-AH-OB-CNT-TOT-GOOD.   EL523
03282      COMPUTE  CAR-AH-OB-CNT-TOT-BAD   =  CAR-AH-OB-CNT-TOT-BAD    EL523
03283                                       +  CO-AH-OB-CNT-TOT-BAD.    EL523
03284      COMPUTE  CAR-AH-OB-CNT-TOT-ALL   =  CAR-AH-OB-CNT-TOT-ALL    EL523
03285                                       +  CO-AH-OB-CNT-TOT-ALL.    EL523
03286                                                                   EL523
03287      COMPUTE  CAR-AH-CAN-TOT-GOOD  =  CAR-AH-CAN-TOT-GOOD         EL523
03288                                    +  CO-AH-CAN-TOT-GOOD.         EL523
03289      COMPUTE  CAR-AH-CAN-TOT-BAD   =  CAR-AH-CAN-TOT-BAD          EL523
03290                                    +  CO-AH-CAN-TOT-BAD.          EL523
03291      COMPUTE  CAR-AH-CAN-TOT-ALL   =  CAR-AH-CAN-TOT-ALL          EL523
03292                                    +  CO-AH-CAN-TOT-ALL.          EL523
03293                                                                   EL523
03294      COMPUTE  CAR-AH-C-CNT-TOT-GOOD  =  CAR-AH-C-CNT-TOT-GOOD     EL523
03295                                      +  CO-AH-C-CNT-TOT-GOOD.     EL523
03296      COMPUTE  CAR-AH-C-CNT-TOT-BAD   =  CAR-AH-C-CNT-TOT-BAD      EL523
03297                                      +  CO-AH-C-CNT-TOT-BAD.      EL523
03298      COMPUTE  CAR-AH-C-CNT-TOT-ALL   =  CAR-AH-C-CNT-TOT-ALL      EL523
03299                                      +  CO-AH-C-CNT-TOT-ALL.      EL523
03300                                                                   EL523
03301      COMPUTE  CAR-AH-NET-TOT-GOOD  =  CAR-AH-NET-TOT-GOOD         EL523
03302                                    +  CO-AH-NET-TOT-GOOD.         EL523
03303      COMPUTE  CAR-AH-NET-TOT-BAD   =  CAR-AH-NET-TOT-BAD          EL523
03304                                    +  CO-AH-NET-TOT-BAD.          EL523
03305      COMPUTE  CAR-AH-NET-TOT-ALL   =  CAR-AH-NET-TOT-ALL          EL523
03306                                    +  CO-AH-NET-TOT-ALL.          EL523
03307                                                                   EL523
03308      COMPUTE  CAR-AH-COM-TOT-GOOD  =  CAR-AH-COM-TOT-GOOD         EL523
03309                                    +  CO-AH-COM-TOT-GOOD.         EL523
03310      COMPUTE  CAR-AH-COM-TOT-BAD   =  CAR-AH-COM-TOT-BAD          EL523
03311                                    +  CO-AH-COM-TOT-BAD.          EL523
03312      COMPUTE  CAR-AH-COM-TOT-ALL   =  CAR-AH-COM-TOT-ALL          EL523
03313                                    +  CO-AH-COM-TOT-ALL.          EL523
03314                                                                   EL523
03315  2375-ZERO.                                                       EL523
03316      MOVE ZEROS  TO  CO-LF-AMT-TOT-GOOD  CO-LF-OB-TOT-GOOD        EL523
03317                      CO-LF-CAN-TOT-GOOD  CO-LF-NET-TOT-GOOD       EL523
03318                      CO-AH-AMT-TOT-GOOD  CO-AH-OB-TOT-GOOD        EL523
03319                      CO-AH-CAN-TOT-GOOD  CO-AH-NET-TOT-GOOD       EL523
03320                      CO-LF-COM-TOT-GOOD  CO-AH-COM-TOT-GOOD       EL523
03321                      CO-LF-I-CNT-TOT-GOOD                         EL523
03322                      CO-LF-OB-CNT-TOT-GOOD                        EL523
03323                      CO-LF-C-CNT-TOT-GOOD                         EL523
03324                      CO-AH-I-CNT-TOT-GOOD                         EL523
03325                      CO-AH-OB-CNT-TOT-GOOD                        EL523
03326                      CO-AH-C-CNT-TOT-GOOD                         EL523
03327                      CO-LF-AMT-TOT-BAD  CO-LF-OB-TOT-BAD          EL523
03328                      CO-LF-CAN-TOT-BAD  CO-LF-NET-TOT-BAD         EL523
03329                      CO-AH-AMT-TOT-BAD  CO-AH-OB-TOT-BAD          EL523
03330                      CO-AH-CAN-TOT-BAD  CO-AH-NET-TOT-BAD         EL523
03331                      CO-LF-COM-TOT-BAD  CO-AH-COM-TOT-BAD         EL523
03332                      CO-LF-I-CNT-TOT-BAD                          EL523
03333                      CO-LF-OB-CNT-TOT-BAD                         EL523
03334                      CO-LF-C-CNT-TOT-BAD                          EL523
03335                      CO-AH-I-CNT-TOT-BAD                          EL523
03336                      CO-AH-OB-CNT-TOT-BAD                         EL523
03337                      CO-AH-C-CNT-TOT-BAD                          EL523
03338                      CO-LF-AMT-TOT-ALL  CO-LF-OB-TOT-ALL          EL523
03339                      CO-LF-CAN-TOT-ALL  CO-LF-NET-TOT-ALL         EL523
03340                      CO-AH-AMT-TOT-ALL  CO-AH-OB-TOT-ALL          EL523
03341                      CO-AH-CAN-TOT-ALL  CO-AH-NET-TOT-ALL         EL523
03342                      CO-LF-COM-TOT-ALL  CO-AH-COM-TOT-ALL         EL523
03343                      CO-LF-I-CNT-TOT-ALL                          EL523
03344                      CO-LF-OB-CNT-TOT-ALL                         EL523
03345                      CO-LF-C-CNT-TOT-ALL                          EL523
03346                      CO-AH-I-CNT-TOT-ALL                          EL523
03347                      CO-AH-OB-CNT-TOT-ALL                         EL523
03348                      CO-AH-C-CNT-TOT-ALL                          EL523
03349                      CO-SP-I-CNT-TOT-GOOD CO-SP-I-CNT-TOT-BAD     EL523
03350                      CO-OB-I-CNT-TOT-GOOD CO-OB-I-CNT-TOT-BAD     EL523
03351                      CO-C-CNT-TOT-GOOD     CO-C-CNT-TOT-BAD.      EL523
03352                                                                   EL523
03353      PERFORM 2600-FLOP-STATE THRU 2699-EXIT.                      EL523
03354                                                                   EL523
03355  2380-SET-GROUP.                                                  EL523
03356      MOVE ACT-GROUP  TO  PREV-GROUP  WS-H5-GROUP.                 EL523
03357      MOVE ACT-RPT2   TO  PREV-RPT2   WS-H5A-RPT2.                 EL523
03358                                                                   EL523
03359      IF FIRST-TIME OR                                             EL523
03360         LAST-TIME OR                                              EL523
03361         CARR-BREAK                                                EL523
03362          GO TO 2399-EXIT.                                         EL523
03363                                                                   EL523
03364      IF DTE-FMT-OPT  =  3                                         EL523
03365        IF HEADERS-PRINTED-SW  =  'Y'                              EL523
03366          MOVE 'ACCOUNTS WITH NO BUSINESS SUBMITTED FOR THE PERIOD'EL523
03367                                       TO  WS-H1-TITLE             EL523
03368            PERFORM WRITE-HEADINGS                                 EL523
03369            GO TO 2399-EXIT                                        EL523
03370        ELSE                                                       EL523
03371            GO TO 2399-EXIT.                                       EL523
03372                                                                   EL523
03373      MOVE '            NET PREMIUM REPORT'                           CL*11
03374                                       TO  WS-H1-TITLE.            EL523
03375      PERFORM WRITE-HEADINGS.                                      EL523
03376                                                                   EL523
03377  2399-EXIT.                                                       EL523
03378      EXIT.                                                        EL523
03379  EJECT                                                            EL523
03380  2400-STATE-BREAK.                                                EL523
03381                                                                   EL523
03382      IF FIRST-TIME                                                EL523
03383          GO TO 2480-SET-STATE.                                    EL523
03384                                                                   EL523
03385      MOVE WS-H6-STATE-NO  TO  SAV-OLD-ST-NO.                      EL523
03386      MOVE WS-H6-STATE     TO  SAV-OLD-STATE.                      EL523
03387                                                                   EL523
03388      IF DTE-FMT-OPT  =  3                                         EL523
03389            GO TO 2480-SET-STATE.                                  EL523
03390                                                                   EL523
03391      IF PRT-STATE-TOTAL                                           EL523
03392          NEXT SENTENCE                                            EL523
03393      ELSE                                                         EL523
03394          GO TO 2472-ROLL-TOTALS.                                  EL523
03395                                                                   EL523
03396      IF (ST-LF-NET-TOT-ALL EQUAL +0)  AND                         EL523
03397         (ST-AH-NET-TOT-ALL EQUAL +0)                              EL523
03398         GO TO 2472-ROLL-TOTALS.                                   EL523
03399                                                                   EL523
03400      COMPUTE  WS-LINE-COUNT-TST  =  WS-LINE-COUNT  +  +19.        EL523
03401                                                                   EL523
03402      IF WS-LINE-COUNT-TST GREATER THAN +57                        EL523
03403          MOVE '            NET PREMIUM REPORT'                       CL*11
03404                                       TO  WS-H1-TITLE             EL523
03405          PERFORM WRITE-HEADINGS.                                  EL523
03406                                                                   EL523
03407      MOVE TOT-MSG-1             TO  WS-D1-LN.                     EL523
03408      MOVE LIFE-OVERRIDE-L6      TO  WS-D1-LN-FILL1.               EL523
03409      MOVE 'WRITTEN'             TO  WS-D1-LN-FILL2.               EL523
03410      MOVE ST-LF-I-CNT-TOT-GOOD  TO  WS-D1-CNT-1.                  EL523
03411      MOVE ST-LF-AMT-TOT-GOOD    TO  WS-D1-AMT-1.                  EL523
03412      MOVE ST-LF-I-CNT-TOT-BAD   TO  WS-D1-CNT-2.                  EL523
03413      MOVE ST-LF-AMT-TOT-BAD     TO  WS-D1-AMT-2.                  EL523
03414      MOVE ST-LF-I-CNT-TOT-ALL   TO  WS-D1-CNT-3.                  EL523
03415      MOVE ST-LF-AMT-TOT-ALL     TO  WS-D1-AMT-3.                  EL523
03416      MOVE WS-DETAIL1            TO  P-DATA.                       EL523
03417      MOVE SPACE-3               TO  P-CTL.                        EL523
03418                                                                   EL523
03419      PERFORM WRITE-A-LINE.                                        EL523
03420                                                                   EL523
03421      MOVE SPACES              TO  WS-DETAIL1.                     EL523
03422                                                                   EL523
03423  2405-LINE-2.                                                     EL523
03424      MOVE SPACES                 TO  WS-D1-LN-FILL1.              EL523
03425      MOVE 'OUTSTND BAL'          TO  WS-D1-LN-FILL2.              EL523
03426      MOVE ST-LF-OB-CNT-TOT-GOOD  TO  WS-D1-CNT-1.                 EL523
03427      MOVE ST-LF-OB-TOT-GOOD      TO  WS-D1-AMT-1.                 EL523
03428      MOVE ST-LF-OB-CNT-TOT-BAD   TO  WS-D1-CNT-2.                 EL523
03429      MOVE ST-LF-OB-TOT-BAD       TO  WS-D1-AMT-2.                 EL523
03430      MOVE ST-LF-OB-CNT-TOT-ALL   TO  WS-D1-CNT-3.                 EL523
03431      MOVE ST-LF-OB-TOT-ALL       TO  WS-D1-AMT-3.                 EL523
03432      MOVE WS-DETAIL1             TO  P-DATA.                      EL523
03433      MOVE SPACE-1                TO  P-CTL.                       EL523
03434                                                                   EL523
03435      PERFORM WRITE-A-LINE.                                        EL523
03436                                                                   EL523
03437      MOVE SPACES             TO  WS-DETAIL1.                      EL523
03438                                                                   EL523
03439  2410-LINE-3.                                                     EL523
03440      MOVE SPACES                TO  WS-D1-LN-FILL1.               EL523
03441      MOVE 'CANCELLED'           TO  WS-D1-LN-FILL2.               EL523
03442      MOVE ST-LF-C-CNT-TOT-GOOD  TO  WS-D1-CNT-1.                  EL523
03443      MOVE ST-LF-CAN-TOT-GOOD    TO  WS-D1-AMT-1.                  EL523
03444      MOVE ST-LF-C-CNT-TOT-BAD   TO  WS-D1-CNT-2.                  EL523
03445      MOVE ST-LF-CAN-TOT-BAD     TO  WS-D1-AMT-2.                  EL523
03446      MOVE ST-LF-C-CNT-TOT-ALL   TO  WS-D1-CNT-3.                  EL523
03447      MOVE ST-LF-CAN-TOT-ALL     TO  WS-D1-AMT-3.                  EL523
03448      MOVE WS-DETAIL1            TO  P-DATA.                       EL523
03449      MOVE SPACE-1               TO  P-CTL.                        EL523
03450                                                                   EL523
03451      PERFORM WRITE-A-LINE.                                        EL523
03452                                                                   EL523
03453      MOVE SPACES              TO  WS-DETAIL1.                     EL523
03454                                                                   EL523
03455  2415-LINE-4.                                                     EL523
03456      MOVE SPACES              TO  WS-D1-LN-FILL1.                 EL523
03457      MOVE 'NET PREMIUM'       TO  WS-D1-LN-FILL2.                 EL523
03458      MOVE ST-LF-NET-TOT-GOOD  TO  WS-D1-AMT-1.                    EL523
03459      MOVE ST-LF-NET-TOT-BAD   TO  WS-D1-AMT-2.                    EL523
03460      MOVE ST-LF-NET-TOT-ALL   TO  WS-D1-AMT-3.                    EL523
03461      MOVE WS-DETAIL1          TO  P-DATA.                         EL523
03462      MOVE SPACE-1             TO  P-CTL.                          EL523
03463                                                                   EL523
03464      PERFORM WRITE-A-LINE.                                        EL523
03465                                                                   EL523
03466      MOVE SPACES              TO  WS-DETAIL1.                     EL523
03467                                                                   EL523
03468  2420-LINE-5.                                                     EL523
03469      MOVE SPACES              TO  WS-D1-LN-FILL1.                 EL523
03470      MOVE 'NET COMMISSION'    TO  WS-D1-LN-FILL2.                 EL523
03471      MOVE ST-LF-COM-TOT-GOOD  TO  WS-D1-AMT-1.                    EL523
03472      MOVE ST-LF-COM-TOT-BAD   TO  WS-D1-AMT-2.                    EL523
03473      MOVE ST-LF-COM-TOT-ALL   TO  WS-D1-AMT-3.                    EL523
03474      MOVE WS-DETAIL1          TO  P-DATA.                         EL523
03475      MOVE SPACE-1             TO  P-CTL.                          EL523
03476                                                                   EL523
03477      PERFORM WRITE-A-LINE.                                        EL523
03478                                                                   EL523
03479      MOVE SPACES              TO  WS-DETAIL1.                     EL523
03480                                                                   EL523
03481  2425-LINE-6.                                                     EL523
03482      MOVE AH-OVERRIDE-L6        TO  WS-D1-LN-FILL1.               EL523
03483      MOVE 'WRITTEN'             TO  WS-D1-LN-FILL2.               EL523
03484      MOVE ST-AH-I-CNT-TOT-GOOD  TO  WS-D1-CNT-1.                  EL523
03485      MOVE ST-AH-AMT-TOT-GOOD    TO  WS-D1-AMT-1.                  EL523
03486      MOVE ST-AH-I-CNT-TOT-BAD   TO  WS-D1-CNT-2.                  EL523
03487      MOVE ST-AH-AMT-TOT-BAD     TO  WS-D1-AMT-2.                  EL523
03488      MOVE ST-AH-I-CNT-TOT-ALL   TO  WS-D1-CNT-3.                  EL523
03489      MOVE ST-AH-AMT-TOT-ALL     TO  WS-D1-AMT-3.                  EL523
03490      MOVE WS-DETAIL1            TO  P-DATA.                       EL523
03491      MOVE SPACE-2               TO  P-CTL.                        EL523
03492                                                                   EL523
03493      PERFORM WRITE-A-LINE.                                        EL523
03494                                                                   EL523
03495      MOVE SPACES              TO  WS-DETAIL1.                     EL523
03496                                                                   EL523
03497  2430-LINE-7.                                                     EL523
03498      MOVE SPACES                 TO  WS-D1-LN-FILL1.              EL523
03499      MOVE 'OUTSTND BAL'          TO  WS-D1-LN-FILL2.              EL523
03500      MOVE ST-AH-OB-CNT-TOT-GOOD  TO  WS-D1-CNT-1.                 EL523
03501      MOVE ST-AH-OB-TOT-GOOD      TO  WS-D1-AMT-1.                 EL523
03502      MOVE ST-AH-OB-CNT-TOT-BAD   TO  WS-D1-CNT-2.                 EL523
03503      MOVE ST-AH-OB-TOT-BAD       TO  WS-D1-AMT-2.                 EL523
03504      MOVE ST-AH-OB-CNT-TOT-ALL   TO  WS-D1-CNT-3.                 EL523
03505      MOVE ST-AH-OB-TOT-ALL       TO  WS-D1-AMT-3.                 EL523
03506      MOVE WS-DETAIL1             TO  P-DATA.                      EL523
03507      MOVE SPACE-1                TO  P-CTL.                       EL523
03508                                                                   EL523
03509      PERFORM WRITE-A-LINE.                                        EL523
03510                                                                   EL523
03511      MOVE SPACES             TO  WS-DETAIL1.                      EL523
03512                                                                   EL523
03513  2435-LINE-8.                                                     EL523
03514      MOVE SPACES                TO  WS-D1-LN-FILL1.               EL523
03515      MOVE 'CANCELLED'           TO  WS-D1-LN-FILL2.               EL523
03516      MOVE ST-AH-C-CNT-TOT-GOOD  TO  WS-D1-CNT-1.                  EL523
03517      MOVE ST-AH-CAN-TOT-GOOD    TO  WS-D1-AMT-1.                  EL523
03518      MOVE ST-AH-C-CNT-TOT-BAD   TO  WS-D1-CNT-2.                  EL523
03519      MOVE ST-AH-CAN-TOT-BAD     TO  WS-D1-AMT-2.                  EL523
03520      MOVE ST-AH-C-CNT-TOT-ALL   TO  WS-D1-CNT-3.                  EL523
03521      MOVE ST-AH-CAN-TOT-ALL     TO  WS-D1-AMT-3.                  EL523
03522      MOVE WS-DETAIL1            TO  P-DATA.                       EL523
03523      MOVE SPACE-1               TO  P-CTL.                        EL523
03524                                                                   EL523
03525      PERFORM WRITE-A-LINE.                                        EL523
03526                                                                   EL523
03527      MOVE SPACES              TO  WS-DETAIL1.                     EL523
03528                                                                   EL523
03529  2440-LINE-9.                                                     EL523
03530      MOVE SPACES              TO  WS-D1-LN-FILL1.                 EL523
03531      MOVE 'NET PREMIUM'       TO  WS-D1-LN-FILL2.                 EL523
03532      MOVE ST-AH-NET-TOT-GOOD  TO  WS-D1-AMT-1.                    EL523
03533      MOVE ST-AH-NET-TOT-BAD   TO  WS-D1-AMT-2.                    EL523
03534      MOVE ST-AH-NET-TOT-ALL   TO  WS-D1-AMT-3.                    EL523
03535      MOVE WS-DETAIL1          TO  P-DATA.                         EL523
03536      MOVE SPACE-1             TO  P-CTL.                          EL523
03537                                                                   EL523
03538      PERFORM WRITE-A-LINE.                                        EL523
03539                                                                   EL523
03540      MOVE SPACES              TO  WS-DETAIL1.                     EL523
03541                                                                   EL523
03542  2445-LINE-10.                                                    EL523
03543      MOVE SPACES              TO  WS-D1-LN-FILL1.                 EL523
03544      MOVE 'NET COMMISSION'    TO  WS-D1-LN-FILL2.                 EL523
03545      MOVE ST-AH-COM-TOT-GOOD  TO  WS-D1-AMT-1.                    EL523
03546      MOVE ST-AH-COM-TOT-BAD   TO  WS-D1-AMT-2.                    EL523
03547      MOVE ST-AH-COM-TOT-ALL   TO  WS-D1-AMT-3.                    EL523
03548      MOVE WS-DETAIL1          TO  P-DATA.                         EL523
03549      MOVE SPACE-1             TO  P-CTL.                          EL523
03550                                                                   EL523
03551      PERFORM WRITE-A-LINE.                                        EL523
03552                                                                   EL523
03553      MOVE SPACES              TO  WS-DETAIL1.                     EL523
03554                                                                   EL523
03555  2450-LINE-11.                                                    EL523
03556      MOVE 'TOTAL '             TO  WS-D1-LN-FILL1.                EL523
03557      MOVE 'WRITTEN'            TO  WS-D1-LN-FILL2.                EL523
03558      MOVE ST-SP-I-CNT-TOT-GOOD TO WS-D1-CNT-1.                    EL523
03559                                                                   EL523
03560      COMPUTE  WS-D1-AMT-1  =  ST-LF-AMT-TOT-GOOD                  EL523
03561                            +  ST-AH-AMT-TOT-GOOD.                 EL523
03562                                                                   EL523
03563      MOVE ST-SP-I-CNT-TOT-BAD  TO WS-D1-CNT-2.                    EL523
03564                                                                   EL523
03565      COMPUTE  WS-D1-AMT-2  =  ST-LF-AMT-TOT-BAD                   EL523
03566                            +  ST-AH-AMT-TOT-BAD.                  EL523
03567      COMPUTE  WS-D1-CNT-3  =  ST-SP-I-CNT-TOT-GOOD                EL523
03568                            +  ST-SP-I-CNT-TOT-BAD.                EL523
03569      COMPUTE  WS-D1-AMT-3  =  ST-LF-AMT-TOT-ALL                   EL523
03570                            +  ST-AH-AMT-TOT-ALL.                  EL523
03571                                                                   EL523
03572      MOVE WS-DETAIL1  TO  P-DATA.                                 EL523
03573      MOVE SPACE-2     TO  P-CTL.                                  EL523
03574                                                                   EL523
03575      PERFORM WRITE-A-LINE.                                        EL523
03576                                                                   EL523
03577      MOVE SPACES      TO  WS-DETAIL1.                             EL523
03578                                                                   EL523
03579  2455-LINE-12.                                                    EL523
03580      MOVE SPACES               TO  WS-D1-LN-FILL1.                EL523
03581      MOVE 'OUTSTND BAL'        TO  WS-D1-LN-FILL2.                EL523
03582      MOVE ST-OB-I-CNT-TOT-GOOD TO WS-D1-CNT-1.                    EL523
03583                                                                   EL523
03584      COMPUTE  WS-D1-AMT-1  =  ST-LF-OB-TOT-GOOD                   EL523
03585                            +  ST-AH-OB-TOT-GOOD.                  EL523
03586                                                                   EL523
03587      MOVE ST-OB-I-CNT-TOT-BAD  TO WS-D1-CNT-2.                    EL523
03588                                                                   EL523
03589      COMPUTE  WS-D1-AMT-2  =  ST-LF-OB-TOT-BAD                    EL523
03590                            +  ST-AH-OB-TOT-BAD.                   EL523
03591      COMPUTE  WS-D1-CNT-3  =  ST-OB-I-CNT-TOT-GOOD                EL523
03592                            +  ST-OB-I-CNT-TOT-BAD.                EL523
03593      COMPUTE  WS-D1-AMT-3  =  ST-LF-OB-TOT-ALL                    EL523
03594                            +  ST-AH-OB-TOT-ALL.                   EL523
03595                                                                   EL523
03596      MOVE WS-DETAIL1     TO  P-DATA.                              EL523
03597      MOVE SPACE-1        TO  P-CTL.                               EL523
03598                                                                   EL523
03599      PERFORM WRITE-A-LINE.                                        EL523
03600                                                                   EL523
03601      MOVE SPACES         TO  WS-DETAIL1.                          EL523
03602                                                                   EL523
03603  2460-LINE-13.                                                    EL523
03604      MOVE SPACES              TO  WS-D1-LN-FILL1.                 EL523
03605      MOVE 'CANCELLED'         TO  WS-D1-LN-FILL2.                 EL523
03606      MOVE ST-C-CNT-TOT-GOOD   TO WS-D1-CNT-1.                     EL523
03607                                                                   EL523
03608      COMPUTE  WS-D1-AMT-1  =  ST-LF-CAN-TOT-GOOD                  EL523
03609                            +  ST-AH-CAN-TOT-GOOD.                 EL523
03610                                                                   EL523
03611      MOVE ST-C-CNT-TOT-BAD    TO WS-D1-CNT-2.                     EL523
03612                                                                   EL523
03613      COMPUTE  WS-D1-AMT-2  =  ST-LF-CAN-TOT-BAD                   EL523
03614                            +  ST-AH-CAN-TOT-BAD.                  EL523
03615      COMPUTE  WS-D1-CNT-3  =  ST-C-CNT-TOT-GOOD                   EL523
03616                            +  ST-C-CNT-TOT-BAD.                   EL523
03617      COMPUTE  WS-D1-AMT-3  =  ST-LF-CAN-TOT-ALL                   EL523
03618                            +  ST-AH-CAN-TOT-ALL.                  EL523
03619                                                                   EL523
03620      MOVE WS-DETAIL1   TO  P-DATA.                                EL523
03621      MOVE SPACE-1      TO  P-CTL.                                 EL523
03622                                                                   EL523
03623      PERFORM WRITE-A-LINE.                                        EL523
03624                                                                   EL523
03625      MOVE SPACES       TO  WS-DETAIL1.                            EL523
03626                                                                   EL523
03627  2465-LINE-14.                                                    EL523
03628      MOVE SPACES         TO  WS-D1-LN-FILL1.                      EL523
03629      MOVE 'NET PREMIUM'  TO  WS-D1-LN-FILL2.                      EL523
03630                                                                   EL523
03631      COMPUTE  WS-D1-AMT-1  =  ST-LF-NET-TOT-GOOD                  EL523
03632                            +  ST-AH-NET-TOT-GOOD.                 EL523
03633      COMPUTE  WS-D1-AMT-2  =  ST-LF-NET-TOT-BAD                   EL523
03634                            +  ST-AH-NET-TOT-BAD.                  EL523
03635      COMPUTE  WS-D1-AMT-3  =  ST-LF-NET-TOT-ALL                   EL523
03636                            +  ST-AH-NET-TOT-ALL.                  EL523
03637                                                                   EL523
03638      MOVE WS-DETAIL1     TO  P-DATA.                              EL523
03639      MOVE SPACE-1        TO  P-CTL.                               EL523
03640                                                                   EL523
03641      PERFORM WRITE-A-LINE.                                        EL523
03642                                                                   EL523
03643      MOVE SPACES         TO  WS-DETAIL1.                          EL523
03644                                                                   EL523
03645  2470-LINE-15.                                                    EL523
03646      MOVE SPACES            TO  WS-D1-LN-FILL1.                   EL523
03647      MOVE 'NET COMMISSION'  TO  WS-D1-LN-FILL2.                   EL523
03648                                                                   EL523
03649      COMPUTE  WS-D1-AMT-1  =  ST-LF-COM-TOT-GOOD                  EL523
03650                            +  ST-AH-COM-TOT-GOOD.                 EL523
03651      COMPUTE  WS-D1-AMT-2  =  ST-LF-COM-TOT-BAD                   EL523
03652                            +  ST-AH-COM-TOT-BAD.                  EL523
03653      COMPUTE  WS-D1-AMT-3  =  ST-LF-COM-TOT-ALL                   EL523
03654                            +  ST-AH-COM-TOT-ALL.                  EL523
03655                                                                   EL523
03656      MOVE WS-DETAIL1        TO  P-DATA.                           EL523
03657      MOVE SPACE-1           TO  P-CTL.                            EL523
03658                                                                   EL523
03659      PERFORM WRITE-A-LINE.                                        EL523
03660                                                                   EL523
03661      MOVE SPACES            TO  WS-DETAIL1.                       EL523
03662                                                                   EL523
03663  2472-ROLL-TOTALS.                                                EL523
03664      COMPUTE  CO-LF-AMT-TOT-GOOD  =  CO-LF-AMT-TOT-GOOD           EL523
03665                                   +  ST-LF-AMT-TOT-GOOD.          EL523
03666      COMPUTE  CO-LF-AMT-TOT-BAD   =  CO-LF-AMT-TOT-BAD            EL523
03667                                   +  ST-LF-AMT-TOT-BAD.           EL523
03668      COMPUTE  CO-LF-AMT-TOT-ALL   =  CO-LF-AMT-TOT-ALL            EL523
03669                                   +  ST-LF-AMT-TOT-ALL.           EL523
03670                                                                   EL523
03671      COMPUTE  CO-SP-I-CNT-TOT-GOOD  =  CO-SP-I-CNT-TOT-GOOD       EL523
03672                                     +  ST-SP-I-CNT-TOT-GOOD.      EL523
03673      COMPUTE  CO-LF-I-CNT-TOT-GOOD  =  CO-LF-I-CNT-TOT-GOOD       EL523
03674                                     +  ST-LF-I-CNT-TOT-GOOD.      EL523
03675      COMPUTE  CO-SP-I-CNT-TOT-BAD   =  CO-SP-I-CNT-TOT-BAD        EL523
03676                                     +  ST-SP-I-CNT-TOT-BAD.       EL523
03677      COMPUTE  CO-LF-I-CNT-TOT-BAD   =  CO-LF-I-CNT-TOT-BAD        EL523
03678                                     +  ST-LF-I-CNT-TOT-BAD.       EL523
03679      COMPUTE  CO-LF-I-CNT-TOT-ALL   =  CO-LF-I-CNT-TOT-ALL        EL523
03680                                     +  ST-LF-I-CNT-TOT-ALL.       EL523
03681                                                                   EL523
03682      COMPUTE  CO-LF-OB-TOT-GOOD  =  CO-LF-OB-TOT-GOOD             EL523
03683                                  +  ST-LF-OB-TOT-GOOD.            EL523
03684      COMPUTE  CO-LF-OB-TOT-BAD   =  CO-LF-OB-TOT-BAD              EL523
03685                                  +  ST-LF-OB-TOT-BAD.             EL523
03686      COMPUTE  CO-LF-OB-TOT-ALL   =  CO-LF-OB-TOT-ALL              EL523
03687                                  +  ST-LF-OB-TOT-ALL.             EL523
03688                                                                   EL523
03689      COMPUTE  CO-OB-I-CNT-TOT-GOOD  =  CO-OB-I-CNT-TOT-GOOD       EL523
03690                                     +  ST-OB-I-CNT-TOT-GOOD.      EL523
03691      COMPUTE  CO-LF-OB-CNT-TOT-GOOD  =  CO-LF-OB-CNT-TOT-GOOD     EL523
03692                                      +  ST-LF-OB-CNT-TOT-GOOD.    EL523
03693      COMPUTE  CO-OB-I-CNT-TOT-BAD   =  CO-OB-I-CNT-TOT-BAD        EL523
03694                                     +  ST-OB-I-CNT-TOT-BAD.       EL523
03695      COMPUTE  CO-LF-OB-CNT-TOT-BAD   =  CO-LF-OB-CNT-TOT-BAD      EL523
03696                                      +  ST-LF-OB-CNT-TOT-BAD.     EL523
03697      COMPUTE  CO-LF-OB-CNT-TOT-ALL   =  CO-LF-OB-CNT-TOT-ALL      EL523
03698                                      +  ST-LF-OB-CNT-TOT-ALL.     EL523
03699                                                                   EL523
03700      COMPUTE  CO-LF-CAN-TOT-GOOD  =  CO-LF-CAN-TOT-GOOD           EL523
03701                                   +  ST-LF-CAN-TOT-GOOD.          EL523
03702      COMPUTE  CO-LF-CAN-TOT-BAD   =  CO-LF-CAN-TOT-BAD            EL523
03703                                   +  ST-LF-CAN-TOT-BAD.           EL523
03704      COMPUTE  CO-LF-CAN-TOT-ALL   =  CO-LF-CAN-TOT-ALL            EL523
03705                                   +  ST-LF-CAN-TOT-ALL.           EL523
03706                                                                   EL523
03707      COMPUTE  CO-C-CNT-TOT-GOOD     =  CO-C-CNT-TOT-GOOD          EL523
03708                                     +  ST-C-CNT-TOT-GOOD.         EL523
03709      COMPUTE  CO-LF-C-CNT-TOT-GOOD  =  CO-LF-C-CNT-TOT-GOOD       EL523
03710                                     +  ST-LF-C-CNT-TOT-GOOD.      EL523
03711      COMPUTE  CO-C-CNT-TOT-BAD      =  CO-C-CNT-TOT-BAD           EL523
03712                                     +  ST-C-CNT-TOT-BAD.          EL523
03713      COMPUTE  CO-LF-C-CNT-TOT-BAD   =  CO-LF-C-CNT-TOT-BAD        EL523
03714                                     +  ST-LF-C-CNT-TOT-BAD.       EL523
03715      COMPUTE  CO-LF-C-CNT-TOT-ALL   =  CO-LF-C-CNT-TOT-ALL        EL523
03716                                     +  ST-LF-C-CNT-TOT-ALL.       EL523
03717                                                                   EL523
03718      COMPUTE  CO-LF-NET-TOT-GOOD  =  CO-LF-NET-TOT-GOOD           EL523
03719                                   +  ST-LF-NET-TOT-GOOD.          EL523
03720      COMPUTE  CO-LF-NET-TOT-BAD   =  CO-LF-NET-TOT-BAD            EL523
03721                                   +  ST-LF-NET-TOT-BAD.           EL523
03722      COMPUTE  CO-LF-NET-TOT-ALL   =  CO-LF-NET-TOT-ALL            EL523
03723                                   +  ST-LF-NET-TOT-ALL.           EL523
03724                                                                   EL523
03725      COMPUTE  CO-LF-COM-TOT-GOOD  =  CO-LF-COM-TOT-GOOD           EL523
03726                                   +  ST-LF-COM-TOT-GOOD.          EL523
03727      COMPUTE  CO-LF-COM-TOT-BAD   =  CO-LF-COM-TOT-BAD            EL523
03728                                   +  ST-LF-COM-TOT-BAD.           EL523
03729      COMPUTE  CO-LF-COM-TOT-ALL   =  CO-LF-COM-TOT-ALL            EL523
03730                                   +  ST-LF-COM-TOT-ALL.           EL523
03731                                                                   EL523
03732      COMPUTE  CO-AH-AMT-TOT-GOOD  =  CO-AH-AMT-TOT-GOOD           EL523
03733                                   +  ST-AH-AMT-TOT-GOOD.          EL523
03734      COMPUTE  CO-AH-AMT-TOT-BAD   =  CO-AH-AMT-TOT-BAD            EL523
03735                                   +  ST-AH-AMT-TOT-BAD.           EL523
03736      COMPUTE  CO-AH-AMT-TOT-ALL   =  CO-AH-AMT-TOT-ALL            EL523
03737                                   +  ST-AH-AMT-TOT-ALL.           EL523
03738                                                                   EL523
03739      COMPUTE  CO-AH-I-CNT-TOT-GOOD  =  CO-AH-I-CNT-TOT-GOOD       EL523
03740                                     +  ST-AH-I-CNT-TOT-GOOD.      EL523
03741      COMPUTE  CO-AH-I-CNT-TOT-BAD   =  CO-AH-I-CNT-TOT-BAD        EL523
03742                                     +  ST-AH-I-CNT-TOT-BAD.       EL523
03743      COMPUTE  CO-AH-I-CNT-TOT-ALL   =  CO-AH-I-CNT-TOT-ALL        EL523
03744                                     +  ST-AH-I-CNT-TOT-ALL.       EL523
03745                                                                   EL523
03746      COMPUTE  CO-AH-OB-TOT-GOOD  =  CO-AH-OB-TOT-GOOD             EL523
03747                                  +  ST-AH-OB-TOT-GOOD.            EL523
03748      COMPUTE  CO-AH-OB-TOT-BAD   =  CO-AH-OB-TOT-BAD              EL523
03749                                  +  ST-AH-OB-TOT-BAD.             EL523
03750      COMPUTE  CO-AH-OB-TOT-ALL   =  CO-AH-OB-TOT-ALL              EL523
03751                                  +  ST-AH-OB-TOT-ALL.             EL523
03752                                                                   EL523
03753      COMPUTE  CO-AH-OB-CNT-TOT-GOOD  =  CO-AH-OB-CNT-TOT-GOOD     EL523
03754                                      +  ST-AH-OB-CNT-TOT-GOOD.    EL523
03755      COMPUTE  CO-AH-OB-CNT-TOT-BAD   =  CO-AH-OB-CNT-TOT-BAD      EL523
03756                                      +  ST-AH-OB-CNT-TOT-BAD.     EL523
03757      COMPUTE  CO-AH-OB-CNT-TOT-ALL   =  CO-AH-OB-CNT-TOT-ALL      EL523
03758                                      +  ST-AH-OB-CNT-TOT-ALL.     EL523
03759                                                                   EL523
03760      COMPUTE  CO-AH-CAN-TOT-GOOD  =  CO-AH-CAN-TOT-GOOD           EL523
03761                                   +  ST-AH-CAN-TOT-GOOD.          EL523
03762      COMPUTE  CO-AH-CAN-TOT-BAD   =  CO-AH-CAN-TOT-BAD            EL523
03763                                   +  ST-AH-CAN-TOT-BAD.           EL523
03764      COMPUTE  CO-AH-CAN-TOT-ALL   =  CO-AH-CAN-TOT-ALL            EL523
03765                                   +  ST-AH-CAN-TOT-ALL.           EL523
03766                                                                   EL523
03767      COMPUTE  CO-AH-C-CNT-TOT-GOOD  =  CO-AH-C-CNT-TOT-GOOD       EL523
03768                                     +  ST-AH-C-CNT-TOT-GOOD.      EL523
03769      COMPUTE  CO-AH-C-CNT-TOT-BAD   =  CO-AH-C-CNT-TOT-BAD        EL523
03770                                     +  ST-AH-C-CNT-TOT-BAD.       EL523
03771      COMPUTE  CO-AH-C-CNT-TOT-ALL   =  CO-AH-C-CNT-TOT-ALL        EL523
03772                                     +  ST-AH-C-CNT-TOT-ALL.       EL523
03773                                                                   EL523
03774      COMPUTE  CO-AH-NET-TOT-GOOD  =  CO-AH-NET-TOT-GOOD           EL523
03775                                   +  ST-AH-NET-TOT-GOOD.          EL523
03776      COMPUTE  CO-AH-NET-TOT-BAD   =  CO-AH-NET-TOT-BAD            EL523
03777                                   +  ST-AH-NET-TOT-BAD.           EL523
03778      COMPUTE  CO-AH-NET-TOT-ALL   =  CO-AH-NET-TOT-ALL            EL523
03779                                   +  ST-AH-NET-TOT-ALL.           EL523
03780                                                                   EL523
03781      COMPUTE  CO-AH-COM-TOT-GOOD  =  CO-AH-COM-TOT-GOOD           EL523
03782                                   +  ST-AH-COM-TOT-GOOD.          EL523
03783      COMPUTE  CO-AH-COM-TOT-BAD   =  CO-AH-COM-TOT-BAD            EL523
03784                                   +  ST-AH-COM-TOT-BAD.           EL523
03785      COMPUTE  CO-AH-COM-TOT-ALL   =  CO-AH-COM-TOT-ALL            EL523
03786                                   +  ST-AH-COM-TOT-ALL.           EL523
03787                                                                   EL523
03788  2475-ZERO.                                                       EL523
03789      MOVE ZEROS  TO  ST-LF-AMT-TOT-GOOD  ST-LF-OB-TOT-GOOD        EL523
03790                      ST-LF-CAN-TOT-GOOD  ST-LF-NET-TOT-GOOD       EL523
03791                      ST-AH-AMT-TOT-GOOD  ST-AH-OB-TOT-GOOD        EL523
03792                      ST-AH-CAN-TOT-GOOD  ST-AH-NET-TOT-GOOD       EL523
03793                      ST-LF-COM-TOT-GOOD  ST-AH-COM-TOT-GOOD       EL523
03794                      ST-LF-I-CNT-TOT-GOOD                         EL523
03795                      ST-LF-OB-CNT-TOT-GOOD                        EL523
03796                      ST-LF-C-CNT-TOT-GOOD                         EL523
03797                      ST-AH-I-CNT-TOT-GOOD                         EL523
03798                      ST-AH-OB-CNT-TOT-GOOD                        EL523
03799                      ST-AH-C-CNT-TOT-GOOD                         EL523
03800                      ST-LF-AMT-TOT-BAD  ST-LF-OB-TOT-BAD          EL523
03801                      ST-LF-CAN-TOT-BAD  ST-LF-NET-TOT-BAD         EL523
03802                      ST-AH-AMT-TOT-BAD  ST-AH-OB-TOT-BAD          EL523
03803                      ST-AH-CAN-TOT-BAD  ST-AH-NET-TOT-BAD         EL523
03804                      ST-LF-COM-TOT-BAD  ST-AH-COM-TOT-BAD         EL523
03805                      ST-LF-I-CNT-TOT-BAD                          EL523
03806                      ST-LF-OB-CNT-TOT-BAD                         EL523
03807                      ST-LF-C-CNT-TOT-BAD                          EL523
03808                      ST-AH-I-CNT-TOT-BAD                          EL523
03809                      ST-AH-OB-CNT-TOT-BAD                         EL523
03810                      ST-AH-C-CNT-TOT-BAD                          EL523
03811                      ST-LF-AMT-TOT-ALL  ST-LF-OB-TOT-ALL          EL523
03812                      ST-LF-CAN-TOT-ALL  ST-LF-NET-TOT-ALL         EL523
03813                      ST-AH-AMT-TOT-ALL  ST-AH-OB-TOT-ALL          EL523
03814                      ST-AH-CAN-TOT-ALL  ST-AH-NET-TOT-ALL         EL523
03815                      ST-LF-COM-TOT-ALL  ST-AH-COM-TOT-ALL         EL523
03816                      ST-LF-I-CNT-TOT-ALL                          EL523
03817                      ST-LF-OB-CNT-TOT-ALL                         EL523
03818                      ST-LF-C-CNT-TOT-ALL                          EL523
03819                      ST-AH-I-CNT-TOT-ALL                          EL523
03820                      ST-AH-OB-CNT-TOT-ALL                         EL523
03821                      ST-AH-C-CNT-TOT-ALL                          EL523
03822                      ST-SP-I-CNT-TOT-GOOD ST-SP-I-CNT-TOT-BAD     EL523
03823                      ST-OB-I-CNT-TOT-GOOD ST-OB-I-CNT-TOT-BAD     EL523
03824                      ST-C-CNT-TOT-GOOD     ST-C-CNT-TOT-BAD.      EL523
03825                                                                   EL523
03826  2480-SET-STATE.                                                  EL523
03827      MOVE ACT-ST       TO  PREV-STATE  WS-H6-STATE-NO.            EL523
03828      MOVE CLAS-STARTS  TO  S1.                                    EL523
03829                                                                   EL523
03830  2485-S-S-LOOP.                                                   EL523
03831      IF ACT-ST = STATE-SUB (S1)                                   EL523
03832          MOVE STATE-PIC (S1)  TO  WS-H6-STATE                     EL523
03833          GO TO 2490-OUT-LOOP.                                     EL523
03834                                                                   EL523
03835      ADD  +1  TO  S1.                                             EL523
03836                                                                   EL523
03837      IF S1 GREATER THAN CLAS-MAXS                                 EL523
03838          MOVE ACT-ST  TO  WS-H6-STATE                             EL523
03839          GO TO 2490-OUT-LOOP.                                     EL523
03840                                                                   EL523
03841      GO TO 2485-S-S-LOOP.                                         EL523
03842                                                                   EL523
03843  2490-OUT-LOOP.                                                   EL523
03844                                                                   EL523
03845      IF FIRST-TIME OR                                             EL523
03846         LAST-TIME OR                                              EL523
03847         CARR-BREAK OR                                             EL523
03848         COMP-BREAK OR                                             EL523
03849         (WS-H6-STATE EQUAL SPACES OR LOW-VALUES)                  EL523
03850          GO TO 2499-EXIT.                                         EL523
03851                                                                   EL523
03852      IF DTE-FMT-OPT  =  3                                         EL523
03853        IF HEADERS-PRINTED-SW  =  'Y'                              EL523
03854          IF HAS-NO-BUSINESS                                       EL523
03855          MOVE 'ACCOUNTS WITH NO BUSINESS SUBMITTED FOR THE PERIOD'EL523
03856                                       TO  WS-H1-TITLE             EL523
03857              PERFORM WRITE-HEADINGS                               EL523
03858              GO TO 2499-EXIT                                      EL523
03859          ELSE                                                     EL523
03860              GO TO 2499-EXIT                                      EL523
03861        ELSE                                                       EL523
03862            GO TO 2499-EXIT.                                       EL523
03863                                                                   EL523
03864      MOVE '            NET PREMIUM REPORT'                           CL*11
03865                                       TO  WS-H1-TITLE.            EL523
03866      PERFORM WRITE-HEADINGS.                                      EL523
03867                                                                   EL523
03868  2499-EXIT.                                                       EL523
03869      EXIT.                                                        EL523
03870  EJECT                                                            EL523
03871  2500-FLIP-STATE.                                                 EL523
03872      MOVE WS-H6-STATE-NO  TO  SAV-NEW-ST-NO.                      EL523
03873      MOVE WS-H6-STATE     TO  SAV-NEW-STATE.                      EL523
03874      MOVE SAV-OLD-ST-NO   TO  WS-H6-STATE-NO.                     EL523
03875      MOVE SAV-OLD-STATE   TO  WS-H6-STATE.                        EL523
03876                                                                   EL523
03877  2599-EXIT.                                                       EL523
03878      EXIT.                                                        EL523
03879                                                                   EL523
03880  2600-FLOP-STATE.                                                 EL523
03881      MOVE SAV-NEW-ST-NO  TO  WS-H6-STATE-NO.                      EL523
03882      MOVE SAV-NEW-STATE  TO  WS-H6-STATE.                         EL523
03883                                                                   EL523
03884  2699-EXIT.                                                       EL523
03885      EXIT.                                                        EL523
03886                                                                   EL523
03887  2700-FLIP-GROUP.                                                 EL523
03888      MOVE WS-H5-GROUP     TO  SAV-NEW-GROUP.                      EL523
03889      MOVE SAV-OLD-GROUP   TO  WS-H5-GROUP.                        EL523
03890      MOVE WS-H5A-RPT2     TO  SAV-NEW-RPT2.                       EL523
03891      MOVE SAV-OLD-RPT2    TO  WS-H5A-RPT2.                        EL523
03892                                                                   EL523
03893  2799-EXIT.                                                       EL523
03894      EXIT.                                                        EL523
03895                                                                   EL523
03896  2800-FLOP-GROUP.                                                 EL523
03897      MOVE SAV-NEW-GROUP  TO  WS-H5-GROUP.                         EL523
03898      MOVE SAV-NEW-RPT2   TO  WS-H5A-RPT2.                         EL523
03899                                                                   EL523
03900  2899-EXIT.                                                       EL523
03901      EXIT.                                                        EL523
03902  EJECT                                                            EL523
03903  2900-FINAL-TOTALS.                                               EL523
03904                                                                   EL523
CIDMOD     MOVE  'N'  TO  PRINT-SW.                                     EL523
CIDMOD                                                                  EL523
03905      IF DTE-FMT-OPT  =  3                                         EL523
03906         GO TO 2999-EXIT.                                          EL523
03907                                                                   EL523
03908      MOVE '2'  TO  FIRST-SW.                                      EL523
03909                                                                   EL523
03910      PERFORM 2200-CARRIER-BREAK THRU 2299-EXIT.                   EL523
03911                                                                   EL523
CIDMOD     MOVE  'Y'  TO  PRINT-SW.                                     EL523
CIDMOD                                                                  EL523
CIDMOD     PERFORM WRITE-HEADINGS.                                      EL523
CIDMOD                                                                  EL523
03912      MOVE TOT-MSG-4              TO  WS-D1-LN.                    EL523
03913      MOVE LIFE-OVERRIDE-L6       TO  WS-D1-LN-FILL1.              EL523
03914      MOVE 'WRITTEN'              TO  WS-D1-LN-FILL2.              EL523
03915      MOVE FIN-LF-I-CNT-TOT-GOOD  TO  WS-D1-CNT-1.                 EL523
03916      MOVE FIN-LF-AMT-TOT-GOOD    TO  WS-D1-AMT-1.                 EL523
03917      MOVE FIN-LF-I-CNT-TOT-BAD   TO  WS-D1-CNT-2.                 EL523
03918      MOVE FIN-LF-AMT-TOT-BAD     TO  WS-D1-AMT-2.                 EL523
03919      MOVE FIN-LF-I-CNT-TOT-ALL   TO  WS-D1-CNT-3.                 EL523
03920      MOVE FIN-LF-AMT-TOT-ALL     TO  WS-D1-AMT-3.                 EL523
03921      MOVE WS-DETAIL1             TO  P-DATA.                      EL523
03922      MOVE SPACE-3                TO  P-CTL.                       EL523
03923                                                                   EL523
03924      PERFORM WRITE-A-LINE.                                        EL523
03925                                                                   EL523
03926      MOVE SPACES      TO  WS-DETAIL1.                             EL523
03927                                                                   EL523
03928  2905-LINE-2.                                                     EL523
03929      MOVE SPACES                  TO  WS-D1-LN-FILL1.             EL523
03930      MOVE 'OUTSTND BAL'           TO  WS-D1-LN-FILL2.             EL523
03931      MOVE FIN-LF-OB-CNT-TOT-GOOD  TO  WS-D1-CNT-1.                EL523
03932      MOVE FIN-LF-OB-TOT-GOOD      TO  WS-D1-AMT-1.                EL523
03933      MOVE FIN-LF-OB-CNT-TOT-BAD   TO  WS-D1-CNT-2.                EL523
03934      MOVE FIN-LF-OB-TOT-BAD       TO  WS-D1-AMT-2.                EL523
03935      MOVE FIN-LF-OB-CNT-TOT-ALL   TO  WS-D1-CNT-3.                EL523
03936      MOVE FIN-LF-OB-TOT-ALL       TO  WS-D1-AMT-3.                EL523
03937      MOVE WS-DETAIL1              TO  P-DATA.                     EL523
03938      MOVE SPACE-1                 TO  P-CTL.                      EL523
03939                                                                   EL523
03940      PERFORM WRITE-A-LINE.                                        EL523
03941                                                                   EL523
03942      MOVE SPACES      TO  WS-DETAIL1.                             EL523
03943                                                                   EL523
03944  2910-LINE-3.                                                     EL523
03945      MOVE SPACES                 TO  WS-D1-LN-FILL1.              EL523
03946      MOVE 'CANCELLED'            TO  WS-D1-LN-FILL2.              EL523
03947      MOVE FIN-LF-C-CNT-TOT-GOOD  TO  WS-D1-CNT-1.                 EL523
03948      MOVE FIN-LF-CAN-TOT-GOOD    TO  WS-D1-AMT-1.                 EL523
03949      MOVE FIN-LF-C-CNT-TOT-BAD   TO  WS-D1-CNT-2.                 EL523
03950      MOVE FIN-LF-CAN-TOT-BAD     TO  WS-D1-AMT-2.                 EL523
03951      MOVE FIN-LF-C-CNT-TOT-ALL   TO  WS-D1-CNT-3.                 EL523
03952      MOVE FIN-LF-CAN-TOT-ALL     TO  WS-D1-AMT-3.                 EL523
03953      MOVE WS-DETAIL1             TO  P-DATA.                      EL523
03954      MOVE SPACE-1                TO  P-CTL.                       EL523
03955                                                                   EL523
03956      PERFORM WRITE-A-LINE.                                        EL523
03957                                                                   EL523
03958      MOVE SPACES      TO  WS-DETAIL1.                             EL523
03959                                                                   EL523
03960  2915-LINE-4.                                                     EL523
03961      MOVE SPACES               TO  WS-D1-LN-FILL1.                EL523
03962      MOVE 'NET PREMIUM'        TO  WS-D1-LN-FILL2.                EL523
03963      MOVE FIN-LF-NET-TOT-GOOD  TO  WS-D1-AMT-1.                   EL523
03964      MOVE FIN-LF-NET-TOT-BAD   TO  WS-D1-AMT-2.                   EL523
03965      MOVE FIN-LF-NET-TOT-ALL   TO  WS-D1-AMT-3.                   EL523
03966                                                                   EL523
03967      MOVE WS-DETAIL1  TO  P-DATA.                                 EL523
03968      MOVE SPACE-1     TO  P-CTL.                                  EL523
03969                                                                   EL523
03970      PERFORM WRITE-A-LINE.                                        EL523
03971                                                                   EL523
03972      MOVE SPACES      TO  WS-DETAIL1.                             EL523
03973                                                                   EL523
03974  2920-LINE-5.                                                     EL523
03975      MOVE SPACES               TO  WS-D1-LN-FILL1.                EL523
03976      MOVE 'NET COMMISSION'     TO  WS-D1-LN-FILL2.                EL523
03977      MOVE FIN-LF-COM-TOT-GOOD  TO  WS-D1-AMT-1.                   EL523
03978      MOVE FIN-LF-COM-TOT-BAD   TO  WS-D1-AMT-2.                   EL523
03979      MOVE FIN-LF-COM-TOT-ALL   TO  WS-D1-AMT-3.                   EL523
03980                                                                   EL523
03981      MOVE WS-DETAIL1  TO  P-DATA.                                 EL523
03982      MOVE SPACE-1     TO  P-CTL.                                  EL523
03983                                                                   EL523
03984      PERFORM WRITE-A-LINE.                                        EL523
03985                                                                   EL523
03986      MOVE SPACES      TO  WS-DETAIL1.                             EL523
03987                                                                   EL523
03988  2925-LINE-6.                                                     EL523
03989      MOVE AH-OVERRIDE-L6         TO  WS-D1-LN-FILL1.              EL523
03990      MOVE 'WRITTEN'              TO  WS-D1-LN-FILL2.              EL523
03991      MOVE FIN-AH-I-CNT-TOT-GOOD  TO  WS-D1-CNT-1.                 EL523
03992      MOVE FIN-AH-AMT-TOT-GOOD    TO  WS-D1-AMT-1.                 EL523
03993      MOVE FIN-AH-I-CNT-TOT-BAD   TO  WS-D1-CNT-2.                 EL523
03994      MOVE FIN-AH-AMT-TOT-BAD     TO  WS-D1-AMT-2.                 EL523
03995      MOVE FIN-AH-I-CNT-TOT-ALL   TO  WS-D1-CNT-3.                 EL523
03996      MOVE FIN-AH-AMT-TOT-ALL     TO  WS-D1-AMT-3.                 EL523
03997      MOVE WS-DETAIL1             TO  P-DATA.                      EL523
03998      MOVE SPACE-2                TO  P-CTL.                       EL523
03999                                                                   EL523
04000      PERFORM WRITE-A-LINE.                                        EL523
04001                                                                   EL523
04002      MOVE SPACES      TO  WS-DETAIL1.                             EL523
04003                                                                   EL523
04004  2930-LINE-7.                                                     EL523
04005      MOVE SPACES                  TO  WS-D1-LN-FILL1.             EL523
04006      MOVE 'OUTSTND BAL'           TO  WS-D1-LN-FILL2.             EL523
04007      MOVE FIN-AH-OB-CNT-TOT-GOOD  TO  WS-D1-CNT-1.                EL523
04008      MOVE FIN-AH-OB-CNT-TOT-BAD   TO  WS-D1-CNT-2.                EL523
04009      MOVE FIN-AH-OB-CNT-TOT-ALL   TO  WS-D1-CNT-3.                EL523
04010      MOVE FIN-AH-OB-TOT-GOOD      TO  WS-D1-AMT-1.                EL523
04011      MOVE FIN-AH-OB-TOT-BAD       TO  WS-D1-AMT-2.                EL523
04012      MOVE FIN-AH-OB-TOT-ALL       TO  WS-D1-AMT-3.                EL523
04013      MOVE WS-DETAIL1              TO  P-DATA.                     EL523
04014      MOVE SPACE-1                 TO  P-CTL.                      EL523
04015                                                                   EL523
04016      PERFORM WRITE-A-LINE.                                        EL523
04017                                                                   EL523
04018      MOVE SPACES      TO  WS-DETAIL1.                             EL523
04019                                                                   EL523
04020  2935-LINE-8.                                                     EL523
04021      MOVE SPACES                 TO  WS-D1-LN-FILL1.              EL523
04022      MOVE 'CANCELLED'            TO  WS-D1-LN-FILL2.              EL523
04023      MOVE FIN-AH-C-CNT-TOT-GOOD  TO  WS-D1-CNT-1.                 EL523
04024      MOVE FIN-AH-C-CNT-TOT-BAD   TO  WS-D1-CNT-2.                 EL523
04025      MOVE FIN-AH-C-CNT-TOT-ALL   TO  WS-D1-CNT-3.                 EL523
04026      MOVE FIN-AH-CAN-TOT-GOOD    TO  WS-D1-AMT-1.                 EL523
04027      MOVE FIN-AH-CAN-TOT-BAD     TO  WS-D1-AMT-2.                 EL523
04028      MOVE FIN-AH-CAN-TOT-ALL     TO  WS-D1-AMT-3.                 EL523
04029      MOVE WS-DETAIL1             TO  P-DATA.                      EL523
04030      MOVE SPACE-1                TO  P-CTL.                       EL523
04031                                                                   EL523
04032      PERFORM WRITE-A-LINE.                                        EL523
04033                                                                   EL523
04034      MOVE SPACES      TO  WS-DETAIL1.                             EL523
04035                                                                   EL523
04036  2940-LINE-9.                                                     EL523
04037      MOVE SPACES               TO  WS-D1-LN-FILL1.                EL523
04038      MOVE 'NET PREMIUM'        TO  WS-D1-LN-FILL2.                EL523
04039      MOVE FIN-AH-NET-TOT-GOOD  TO  WS-D1-AMT-1.                   EL523
04040      MOVE FIN-AH-NET-TOT-BAD   TO  WS-D1-AMT-2.                   EL523
04041      MOVE FIN-AH-NET-TOT-ALL   TO  WS-D1-AMT-3.                   EL523
04042                                                                   EL523
04043      MOVE WS-DETAIL1  TO  P-DATA.                                 EL523
04044      MOVE SPACE-1     TO  P-CTL.                                  EL523
04045                                                                   EL523
04046      PERFORM WRITE-A-LINE.                                        EL523
04047                                                                   EL523
04048      MOVE SPACES      TO  WS-DETAIL1.                             EL523
04049                                                                   EL523
04050  2945-LINE-10.                                                    EL523
04051      MOVE SPACES               TO  WS-D1-LN-FILL1.                EL523
04052      MOVE 'NET COMMISSION'     TO  WS-D1-LN-FILL2.                EL523
04053      MOVE FIN-AH-COM-TOT-GOOD  TO  WS-D1-AMT-1.                   EL523
04054      MOVE FIN-AH-COM-TOT-BAD   TO  WS-D1-AMT-2.                   EL523
04055      MOVE FIN-AH-COM-TOT-ALL   TO  WS-D1-AMT-3.                   EL523
04056                                                                   EL523
04057      MOVE WS-DETAIL1  TO  P-DATA.                                 EL523
04058      MOVE SPACE-1     TO  P-CTL.                                  EL523
04059                                                                   EL523
04060      PERFORM WRITE-A-LINE.                                        EL523
04061                                                                   EL523
04062      MOVE SPACES      TO  WS-DETAIL1.                             EL523
04063                                                                   EL523
04064  2950-LINE-11.                                                    EL523
04065      MOVE 'TOTAL '              TO WS-D1-LN-FILL1.                EL523
04066      MOVE 'WRITTEN'             TO WS-D1-LN-FILL2.                EL523
04067      MOVE FIN-SP-I-CNT-TOT-GOOD TO WS-D1-CNT-1.                   EL523
04068                                                                   EL523
04069      COMPUTE  WS-D1-AMT-1  =  FIN-LF-AMT-TOT-GOOD                 EL523
04070                            +  FIN-AH-AMT-TOT-GOOD.                EL523
062104     MOVE WS-D1-AMT-1           TO WS-ME-BAL-GOOD-WRITTEN.
04071                                                                   EL523
04072      MOVE FIN-SP-I-CNT-TOT-BAD  TO WS-D1-CNT-2.                   EL523
04073                                                                   EL523
04074      COMPUTE  WS-D1-AMT-2  =  FIN-LF-AMT-TOT-BAD                  EL523
04075                            +  FIN-AH-AMT-TOT-BAD.                 EL523
062104     MOVE WS-D1-AMT-2           TO WS-ME-BAL-BAD-WRITTEN.
04076      COMPUTE  WS-D1-CNT-3  =  FIN-SP-I-CNT-TOT-GOOD               EL523
04077                            +  FIN-SP-I-CNT-TOT-BAD.               EL523
04078      COMPUTE  WS-D1-AMT-3  =  FIN-LF-AMT-TOT-ALL                  EL523
04079                            +  FIN-AH-AMT-TOT-ALL.                 EL523
04080                                                                   EL523
04081      MOVE WS-DETAIL1  TO  P-DATA.                                 EL523
04082      MOVE SPACE-2     TO  P-CTL.                                  EL523
04083                                                                   EL523
04084      PERFORM WRITE-A-LINE.                                        EL523
04085                                                                   EL523
04086      MOVE SPACES      TO  WS-DETAIL1.                             EL523
04087                                                                   EL523
04088  2955-LINE-12.                                                    EL523
04089      MOVE SPACES                TO WS-D1-LN-FILL1.                EL523
04090      MOVE 'OUTSTND BAL'         TO WS-D1-LN-FILL2.                EL523
04091      MOVE FIN-OB-I-CNT-TOT-GOOD TO WS-D1-CNT-1.                   EL523
04092                                                                   EL523
04093      COMPUTE  WS-D1-AMT-1  =  FIN-LF-OB-TOT-GOOD                  EL523
04094                            +  FIN-AH-OB-TOT-GOOD.                 EL523

062104     MOVE WS-D1-AMT-1             TO WS-ME-BAL-GOOD-OUTSTBAL.
062104     COMPUTE WS-ME-BAL-AMT = WS-ME-BAL-GOOD-WRITTEN +
062104                             WS-ME-BAL-GOOD-OUTSTBAL.
062104     MOVE WS-BALANCE-DESCRIPTION1 TO WS-ME-BAL-DESCRIP.
062104     WRITE ME-EL523-BALANCE-REC   FROM WS-ME-BALANCE-REC.

04095                                                                   EL523
04096      MOVE FIN-OB-I-CNT-TOT-BAD  TO WS-D1-CNT-2.                   EL523
04097                                                                   EL523
04098      COMPUTE  WS-D1-AMT-2  =  FIN-LF-OB-TOT-BAD                   EL523
04099                            +  FIN-AH-OB-TOT-BAD.                  EL523

062104     MOVE WS-D1-AMT-2             TO WS-ME-BAL-BAD-OUTSTBAL.       
062104     COMPUTE WS-ME-BAL-AMT = WS-ME-BAL-BAD-WRITTEN +
062104                             WS-ME-BAL-BAD-OUTSTBAL.
062104     MOVE WS-BALANCE-DESCRIPTION2 TO WS-ME-BAL-DESCRIP.
062104     WRITE ME-EL523-BALANCE-REC   FROM WS-ME-BALANCE-REC.


04100      COMPUTE  WS-D1-CNT-3  =  FIN-OB-I-CNT-TOT-GOOD               EL523
04101                            +  FIN-OB-I-CNT-TOT-BAD.               EL523
04102      COMPUTE  WS-D1-AMT-3  =  FIN-LF-OB-TOT-ALL                   EL523
04103                            +  FIN-AH-OB-TOT-ALL.                  EL523

062104     MOVE WS-D1-AMT-3             TO WS-ME-BAL-TOT-OUTSTBAL. 
062104     COMPUTE WS-ME-BAL-AMT = WS-ME-BAL-GOOD-WRITTEN +
062104                             WS-ME-BAL-BAD-WRITTEN  + 
062104                             WS-ME-BAL-TOT-OUTSTBAL.
062104     MOVE WS-BALANCE-DESCRIPTION3 TO WS-ME-BAL-DESCRIP.
062104     WRITE ME-EL523-BALANCE-REC   FROM WS-ME-BALANCE-REC.
04104                                                                   EL523
04105      MOVE WS-DETAIL1  TO  P-DATA.                                 EL523
04106      MOVE SPACE-1     TO  P-CTL.                                  EL523
04107                                                                   EL523
04108      PERFORM WRITE-A-LINE.                                        EL523
04109                                                                   EL523
04110      MOVE SPACES      TO  WS-DETAIL1.                             EL523
04111                                                                   EL523
04112  2960-LINE-13.                                                    EL523
04113      MOVE SPACES              TO WS-D1-LN-FILL1.                  EL523
04114      MOVE 'CANCELLED'         TO WS-D1-LN-FILL2.                  EL523
04115      MOVE FIN-C-CNT-TOT-GOOD  TO WS-D1-CNT-1.                     EL523
04116                                                                   EL523
04117      COMPUTE  WS-D1-AMT-1  =  FIN-LF-CAN-TOT-GOOD                 EL523
04118                            +  FIN-AH-CAN-TOT-GOOD.                EL523
04119                                                                   EL523
062104     MOVE WS-D1-AMT-1             TO WS-ME-BAL-AMT.
062104     MOVE WS-BALANCE-DESCRIPTION4 TO WS-ME-BAL-DESCRIP.
062104     WRITE ME-EL523-BALANCE-REC   FROM WS-ME-BALANCE-REC.

04120      MOVE FIN-C-CNT-TOT-BAD   TO WS-D1-CNT-2.                     EL523
04121                                                                   EL523
04122      COMPUTE  WS-D1-AMT-2  =  FIN-LF-CAN-TOT-BAD                  EL523
04123                            +  FIN-AH-CAN-TOT-BAD.                 EL523

062104     MOVE WS-D1-AMT-2             TO WS-ME-BAL-AMT.
062104     MOVE WS-BALANCE-DESCRIPTION5 TO WS-ME-BAL-DESCRIP.
062104     WRITE ME-EL523-BALANCE-REC   FROM WS-ME-BALANCE-REC.

04124      COMPUTE  WS-D1-CNT-3  =  FIN-C-CNT-TOT-GOOD                  EL523
04125                            +  FIN-C-CNT-TOT-BAD.                  EL523
04126      COMPUTE  WS-D1-AMT-3  =  FIN-LF-CAN-TOT-ALL                  EL523
04127                            +  FIN-AH-CAN-TOT-ALL.                 EL523
04128                                                                   EL523

062104     MOVE WS-D1-AMT-3              TO WS-ME-BAL-AMT.
062104     MOVE WS-BALANCE-DESCRIPTION6  TO WS-ME-BAL-DESCRIP.
062104     WRITE ME-EL523-BALANCE-REC    FROM WS-ME-BALANCE-REC.

04129      MOVE WS-DETAIL1  TO  P-DATA.                                 EL523
04130      MOVE SPACE-1     TO  P-CTL.                                  EL523
04131                                                                   EL523
04132      PERFORM WRITE-A-LINE.                                        EL523
04133                                                                   EL523
04134      MOVE SPACES      TO  WS-DETAIL1.                             EL523
04135                                                                   EL523
04136  2965-LINE-14.                                                    EL523
04137      MOVE SPACES         TO  WS-D1-LN-FILL1.                      EL523
04138      MOVE 'NET PREMIUM'  TO  WS-D1-LN-FILL2.                      EL523
04139                                                                   EL523
04140      COMPUTE  WS-D1-AMT-1  =  FIN-LF-NET-TOT-GOOD                 EL523
04141                            +  FIN-AH-NET-TOT-GOOD.                EL523

070714     COMPUTE hld-523-PREM-TOT  =
070714        FIN-LF-NET-TOT-GOOD +  FIN-AH-NET-TOT-GOOD

062104     MOVE WS-D1-AMT-1             TO WS-ME-BAL-AMT.
062104     MOVE WS-BALANCE-DESCRIPTION7 TO WS-ME-BAL-DESCRIP.
062104     WRITE ME-EL523-BALANCE-REC   FROM WS-ME-BALANCE-REC.

04142      COMPUTE  WS-D1-AMT-2  =  FIN-LF-NET-TOT-BAD                  EL523
04143                            +  FIN-AH-NET-TOT-BAD.                 EL523
062104     MOVE WS-D1-AMT-2             TO WS-ME-BAL-AMT.
062104     MOVE WS-BALANCE-DESCRIPTION8 TO WS-ME-BAL-DESCRIP.
062104     WRITE ME-EL523-BALANCE-REC   FROM WS-ME-BALANCE-REC.

04144      COMPUTE  WS-D1-AMT-3  =  FIN-LF-NET-TOT-ALL                  EL523
04145                            +  FIN-AH-NET-TOT-ALL.                 EL523

062104     MOVE WS-D1-AMT-3              TO WS-ME-BAL-AMT.
062104     MOVE WS-BALANCE-DESCRIPTION9  TO WS-ME-BAL-DESCRIP.
062104     WRITE ME-EL523-BALANCE-REC    FROM WS-ME-BALANCE-REC.
04146                                                                   EL523
04147      MOVE WS-DETAIL1  TO  P-DATA.                                 EL523
04148      MOVE SPACE-1     TO  P-CTL.                                  EL523
04149                                                                   EL523
04150      PERFORM WRITE-A-LINE.                                        EL523
04151                                                                   EL523
04152  2970-LINE-15.                                                    EL523
04153      MOVE SPACES            TO  WS-D1-LN-FILL1.                   EL523
04154      MOVE 'NET COMMISSION'  TO  WS-D1-LN-FILL2.                   EL523
04155                                                                   EL523
04156      COMPUTE  WS-D1-AMT-1  =  FIN-LF-COM-TOT-GOOD                 EL523
04157                            +  FIN-AH-COM-TOT-GOOD.                EL523

062104     MOVE WS-D1-AMT-1              TO WS-ME-BAL-AMT.
062104     MOVE WS-BALANCE-DESCRIPTION10 TO WS-ME-BAL-DESCRIP.
062104     WRITE ME-EL523-BALANCE-REC    FROM WS-ME-BALANCE-REC.

04158      COMPUTE  WS-D1-AMT-2  =  FIN-LF-COM-TOT-BAD                  EL523
04159                            +  FIN-AH-COM-TOT-BAD.                 EL523

062104     MOVE WS-D1-AMT-2              TO WS-ME-BAL-AMT.
062104     MOVE WS-BALANCE-DESCRIPTION11 TO WS-ME-BAL-DESCRIP.
062104     WRITE ME-EL523-BALANCE-REC    FROM WS-ME-BALANCE-REC.

04160      COMPUTE  WS-D1-AMT-3  =  FIN-LF-COM-TOT-ALL                  EL523
04161                            +  FIN-AH-COM-TOT-ALL.                 EL523
04162                                                                   EL523

062104     MOVE WS-D1-AMT-3              TO WS-ME-BAL-AMT.
062104     MOVE WS-BALANCE-DESCRIPTION12 TO WS-ME-BAL-DESCRIP.
062104     WRITE ME-EL523-BALANCE-REC    FROM WS-ME-BALANCE-REC.

04163      MOVE WS-DETAIL1  TO  P-DATA.                                 EL523
04164      MOVE SPACE-1     TO  P-CTL.                                  EL523
04165                                                                   EL523
04166      PERFORM WRITE-A-LINE.                                        EL523
04167                                                                   EL523
04168  2999-EXIT.                                                       EL523
04169      EXIT.                                                        EL523
04170  EJECT                                                            EL523
04171  8500-DATE-CONVERSION SECTION.   COPY ELCDCS.                     EL523
04172  EJECT                                                            EL523
04173  WRITE-A-LINE SECTION.           COPY ELCWAL.                     EL523
04174  EJECT                                                            EL523
04175  WRITE-HEADINGS SECTION.                                          EL523
04176  WHS-010.                                                         EL523
04177                                                                   EL523
04178      ADD +1  TO  WS-PAGE.                                         EL523
04179      MOVE WS-PAGE                TO  WS-H3-PAGE.                  EL523
04180      MOVE PRT                    TO  WS-SAVE-PRINT-RECORD.        EL523
04181                                                                   EL523
04182      MOVE WS-HEADING1            TO  PRT.                         EL523
04183      MOVE '1'                    TO  X.                           EL523
04184      PERFORM WRITE-PRINTER.                                       EL523
04185                                                                   EL523
04186      MOVE WS-HEADING2            TO  PRT.                         EL523
04187      MOVE ' '                    TO  X.                           EL523
04188      PERFORM WRITE-PRINTER.                                       EL523
04189                                                                   EL523
04190      MOVE WS-HEADING3            TO  PRT.                         EL523
04191      MOVE ' '                    TO  X.                           EL523
04192      PERFORM WRITE-PRINTER.                                       EL523
04193                                                                   EL523
04194      MOVE WS-HEADING4            TO  PRT.                         EL523
04195      MOVE ' '                    TO  X.                           EL523
04196      PERFORM WRITE-PRINTER.                                       EL523
04197                                                                   EL523
04198      IF DTE-CLIENT = 'MON'                                        EL523
04199          MOVE WS-HEADING5A       TO  PRT                          EL523
04200      ELSE                                                         EL523
04201          MOVE WS-HEADING5        TO  PRT.                         EL523
04202                                                                   EL523
04203      PERFORM WRITE-PRINTER.                                       EL523
04204                                                                   EL523
04205      IF DTE-CLIENT = 'MON'                                        EL523
04206          NEXT SENTENCE                                            EL523
04207      ELSE                                                         EL523
04208          MOVE WS-HEADING6        TO  PRT                          EL523
04209          PERFORM WRITE-PRINTER.                                   EL523
04210                                                                   EL523
04211      IF DTE-FMT-OPT  =  3                                         EL523
04212         MOVE WS-OPTION3-HEADER7  TO  PRT                          EL523
04213         PERFORM WRITE-PRINTER                                     EL523
04214      ELSE                                                         EL523
04215         MOVE WS-HEADING7         TO  PRT                          EL523
04216         PERFORM WRITE-PRINTER.                                    EL523
04217                                                                   EL523
04218      IF DTE-FMT-OPT  =  3                                         EL523
04219         MOVE WS-OPTION3-HEADER8  TO  PRT                          EL523
04220         PERFORM WRITE-PRINTER                                     EL523
04221      ELSE                                                         EL523
04222         MOVE WS-HEADING8         TO  PRT                          EL523
04223         PERFORM WRITE-PRINTER.                                    EL523
04224                                                                   EL523
04225      IF DTE-FMT-OPT  =  3                                         EL523
04226         MOVE WS-OPTION3-HEADER9  TO  PRT                          EL523
04227         PERFORM WRITE-PRINTER                                     EL523
04228      ELSE                                                         EL523
04229         MOVE WS-HEADING9         TO  PRT                          EL523
04230         PERFORM WRITE-PRINTER.                                    EL523
04231                                                                   EL523
04232      MOVE SPACES                 TO  PRT.                         EL523
04233      PERFORM WRITE-PRINTER.                                       EL523
04234                                                                   EL523
04235      MOVE +12 TO WS-LINE-COUNT.                                   EL523
04236                                                                   EL523
04237                                  COPY ELCWHS2.                    EL523
04238  EJECT                                                            EL523
04239  WRITE-PRINTER SECTION.          COPY ELCWPS.                     EL523
04240                                                                   EL523
CIDMOD*                                COPY ELCPRT2X.                   EL523
CIDMOD*                                                                 EL523
CIDMOD* COPYBOOK 'ELCPRT2X' COPIED INTO PROGRAM AS 'HARDCODE' TO        EL523
CIDMOD* ALLOW MODS TO BE MADE FOR THIS PROGRAM ONLY.                    EL523
CIDMOD*                                                                 EL523
00001 ******************************************************************04/15/98
00002 *                                                                *ELCPRT2X
00003 *                            ELCPRT2X.                           *   LV002
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE               CL**2
00005 *                            VMOD=2.002                          *   CL**2
00006 *                                                                *ELCPRT2X
00007 *  OUTPUT METHOD BASED ON DATE CARD PRINT OPTION -               *ELCPRT2X
00008 *                                                                *ELCPRT2X
00009 *    USE COPY CODE ELCPRTFD FOR PRINT FD (PRNTR)                 *ELCPRT2X
00010 *                  ELCFCHFD FOR FICHE FD (FICH)                  *ELCPRT2X
00011 *                  ELCRPTFD FOR ONLINE REPORT FILE FD (ELREPT)   *ELCPRT2X
00012 *                  ELCREPT FOR ONLINE SAVE FILE (01)             *ELCPRT2X
00013 *                  ELCPRTCX TO CLOSE FICH AND REPORT FILES       *ELCPRT2X
00014 *                                                                *ELCPRT2X
00015 *    * NOTE * IF OUTPUT NOT ELIGIBLE FOR ONLINE RETRIEVAL - USE  *ELCPRT2X
00016 *             ELCPRT2 ROUTINE AND ASSOCIATED FD.                 *ELCPRT2X
00017 ******************************************************************ELCPRT2X
00018                                                                   ELCPRT2X
00019      IF DTE-FICH NOT = SPACE AND                                  ELCPRT2X
00020          FICH-OPEN   = SPACE                                      ELCPRT2X
00021          MOVE 'X'                TO  FICH-OPEN                    ELCPRT2X
00022          OPEN OUTPUT FICH.                                        ELCPRT2X
00023                                                                   ELCPRT2X
00024      IF DTE-PRT-OPT = 'S' OR 'T'                                  ELCPRT2X
00025          IF (REPT-OPEN = SPACE) AND (DTE-ABEND-CD-1 = SPACE)      ELCPRT2X
00026              OPEN I-O ELREPT                                      ELCPRT2X
00027              IF DTE-F-1 NOT = ZERO AND                            ELCPRT2X
00028                 DTE-VSAM-FLAGS NOT = '97'                         ELCPRT2X
00029                  MOVE DTE-VSAM-FLAGS  TO  WS-ABEND-FILE-STATUS    ELCPRT2X
00030                  MOVE 'ERROR OCCURED OPEN - ELREPT'               ELCPRT2X
00031                                  TO  WS-ABEND-MESSAGE             ELCPRT2X
00032                  GO TO ABEND-PGM                                  ELCPRT2X
00033              ELSE                                                 ELCPRT2X
00034                  MOVE '1'                   TO REPT-OPEN          ELCPRT2X
00035                  MOVE DTE-CLASIC-COMPANY-CD TO RF-COMPANY-CD      ELCPRT2X
00036                  MOVE '1'                   TO RF-RECORD-TYPE     ELCPRT2X
00037                  MOVE OLC-REPORT-NAME       TO RF-REPORT-ID       ELCPRT2X
00038                  MOVE ZERO                  TO RF-LINE-NUMBER     ELCPRT2X
00039                  START ELREPT  KEY NOT LESS RF-CONTROL-PRIMARY    ELCPRT2X
00040                  PERFORM DTE-REPORT-DELETE THRU DTE-DELETE-EXIT   ELCPRT2X
00041                  MOVE DTE-CLASIC-COMPANY-CD TO RF-COMPANY-CD      ELCPRT2X
00042                  MOVE '2'                   TO RF-RECORD-TYPE     ELCPRT2X
00043                  MOVE OLC-REPORT-NAME       TO RF-REPORT-ID       ELCPRT2X
00044                  MOVE ZERO                  TO RF-LINE-NUMBER     ELCPRT2X
00045                  START ELREPT  KEY NOT LESS RF-CONTROL-PRIMARY    ELCPRT2X
00046                  PERFORM DTE-REPORT-DELETE THRU DTE-DELETE-EXIT   ELCPRT2X
00047                  MOVE DTE-CLASIC-COMPANY-CD TO RF-COMPANY-CD      ELCPRT2X
00048                  MOVE '1'                   TO RF-RECORD-TYPE     ELCPRT2X
00049                  MOVE OLC-REPORT-NAME       TO RF-REPORT-ID       ELCPRT2X
00050                  MOVE SPACES                TO RF-REPORT-LINE-133.ELCPRT2X
00051                                                                   ELCPRT2X
00052      IF DTE-ABEND-CD-1 = '81' AND                                 ELCPRT2X
00053         DTE-PRT-OPT    = 'S'                                      ELCPRT2X
00054          MOVE +0302                  TO  WS-RETURN-CODE           ELCPRT2X
00055          GO TO ABEND-PGM.                                         ELCPRT2X
00056                                                                   ELCPRT2X
00057      IF DTE-PRT-OPT = 'S' OR 'T'                                  ELCPRT2X
00058          MOVE X                      TO  RF-CTL-CHAR-133          ELCPRT2X
00059          MOVE P-DATA                 TO  RF-DATA-133              ELCPRT2X
00060              IF DTE-ABEND-CD-1 = SPACES                           ELCPRT2X
00061                  ADD +1              TO  DTE-TOT-LINES            ELCPRT2X
00062                  MOVE DTE-TOT-LINES  TO  RF-LINE-NUMBER           ELCPRT2X
00063                  WRITE REPORT-SAVE-FILE                           ELCPRT2X
00064                      INVALID KEY                                  ELCPRT2X
00065                          MOVE '88'   TO  DTE-ABEND-CD-1           ELCPRT2X
00066                          CLOSE ELREPT                             ELCPRT2X
00067                          MOVE SPACE  TO  REPT-OPEN.               ELCPRT2X
00068                                                                   ELCPRT2X
00069      IF DTE-FICH NOT = SPACE                                      ELCPRT2X
00070          MOVE X                      TO  P-CTL                    ELCPRT2X
00071          WRITE FICH-REC FROM PRT.                                 ELCPRT2X
00072                                                                   ELCPRT2X
CIDMOD     IF FIRST-SW  =  '2'                                          ELCPRT2X
CIDMOD         NEXT SENTENCE                                            ELCPRT2X
CIDMOD       ELSE                                                       ELCPRT2X
CIDMOD         GO TO DTE-PRINT-EXIT.                                    ELCPRT2X
CIDMOD                                                                  ELCPRT2X
CIDMOD     IF PRINT-SW  =  'Y'                                          ELCPRT2X
CIDMOD         NEXT SENTENCE                                            ELCPRT2X
CIDMOD       ELSE                                                       ELCPRT2X
CIDMOD         GO TO DTE-PRINT-EXIT.                                    ELCPRT2X
CIDMOD                                                                  ELCPRT2X
00073      IF DTE-PRT-OPT = 'P' OR 'B' OR 'T'                           ELCPRT2X
00074        MOVE X                        TO  P-CTL                    ELCPRT2X
00075        IF P-CTL = ' '                                             ELCPRT2X
00076          WRITE PRT AFTER ADVANCING 1 LINE                         ELCPRT2X
00077        ELSE                                                       ELCPRT2X
00078          IF P-CTL = '0'                                           ELCPRT2X
00079            WRITE PRT AFTER ADVANCING 2 LINES                      ELCPRT2X
00080          ELSE                                                     ELCPRT2X
00081            IF P-CTL = '-'                                         ELCPRT2X
00082              WRITE PRT AFTER ADVANCING 3 LINES                    ELCPRT2X
00083            ELSE                                                   ELCPRT2X
00084              WRITE PRT AFTER ADVANCING PAGE.                      ELCPRT2X
00085                                                                   ELCPRT2X
00086      GO TO DTE-PRINT-EXIT.                                        ELCPRT2X
00087                                                                   ELCPRT2X
00088  DTE-REPORT-DELETE.                                               ELCPRT2X
00089      IF DTE-F-1 NOT = ZERO                                        ELCPRT2X
00090          MOVE ZERO                   TO  DTE-VSAM-FLAGS           ELCPRT2X
00091          GO TO DTE-DELETE-EXIT.                                   ELCPRT2X
00092                                                                   ELCPRT2X
00093      READ ELREPT   NEXT RECORD                                    ELCPRT2X
00094            AT END   GO TO DTE-DELETE-EXIT.                        ELCPRT2X
00095                                                                   ELCPRT2X
00096      IF DTE-CLASIC-COMPANY-CD = RF-COMPANY-CD  AND                ELCPRT2X
00097         OLC-REPORT-NAME       = RF-REPORT-ID                      ELCPRT2X
00098          DELETE ELREPT RECORD                                     ELCPRT2X
00099          GO TO DTE-REPORT-DELETE.                                 ELCPRT2X
00100                                                                   ELCPRT2X
00101  DTE-DELETE-EXIT.                                                 ELCPRT2X
00102      EXIT.                                                        ELCPRT2X
00103                                                                   ELCPRT2X
00104  DTE-PRINT-EXIT.                                                  ELCPRT2X
00105      EXIT.                                                        ELCPRT2X
00106 ******************************************************************ELCPRT2X
CIDMOD*                                                                 EL523
04242                                                                   EL523
04243  WPS-EXIT.                                                        EL523
04244      EXIT.                                                        EL523
04245  EJECT                                                            EL523
04246  8800-OPEN-FILES SECTION.                                         EL523
04247      OPEN INPUT  ERACCT                                           EL523
04248                  ERPNDB                                           EL523
04249           OUTPUT ACCT-XTRACT                                      EL523
062104                 ME-EL523-BALANCE
04250                  PRNTR.                                           EL523
04251                                                                   EL523
04252      IF ERACCT-FILE-STATUS  = '00' OR '97'                        EL523
04253          NEXT SENTENCE                                            EL523
04254      ELSE                                                         EL523
04255          MOVE 'ERROR OCCURED ON OPEN - ERACCT'                    EL523
04256                                   TO  WS-ABEND-MESSAGE            EL523
04257          MOVE ERACCT-FILE-STATUS  TO  WS-ABEND-FILE-STATUS        EL523
04258          GO TO ABEND-PGM.                                         EL523
04259                                                                   EL523
04260      IF ERPNDB-FILE-STATUS  = '00' OR '97'                        EL523
04261          NEXT SENTENCE                                            EL523
04262      ELSE                                                         EL523
04263          MOVE 'ERROR OCCURED ON OPEN - ERPNDB'                    EL523
04264                                   TO  WS-ABEND-MESSAGE            EL523
04265          MOVE ERPNDB-FILE-STATUS  TO  WS-ABEND-FILE-STATUS        EL523
04266          GO TO ABEND-PGM.                                         EL523
04267                                                                   EL523
04268      IF DTE-CLIENT = 'MON'                                        EL523
04269          MOVE TOT-MSG-2A          TO TOT-MSG-2.                   EL523
04270  EJECT                                                            EL523
04271  8900-CLOSE-FILES SECTION.       COPY ELCPRTCX.                   EL523
04272                                                                   EL523
04273      CLOSE ERACCT                                                 EL523
04274            ERPNDB                                                 EL523
062104           ME-EL523-BALANCE
04275            PRNTR.                                                 EL523
04276                                                                   EL523
04277      IF ERACCT-FILE-STATUS NOT = ZEROS                            EL523
04278          MOVE 'ERROR OCCURED CLOSE - ERACCT'                      EL523
04279                                   TO  WS-ABEND-MESSAGE            EL523
04280          MOVE ERACCT-FILE-STATUS  TO  WS-ABEND-FILE-STATUS        EL523
04281          GO TO ABEND-PGM.                                         EL523
04282                                                                   EL523
04283      IF ERPNDB-FILE-STATUS NOT = ZEROS                            EL523
04284          MOVE 'ERROR OCCURED CLOSE - ERPNDB'                      EL523
04285                                   TO  WS-ABEND-MESSAGE            EL523
04286          MOVE ERPNDB-FILE-STATUS  TO  WS-ABEND-FILE-STATUS        EL523
04287          GO TO ABEND-PGM.                                         EL523

070714     OPEN I-O ERMEBL.                                  
070714                                                       
070714     IF (ERMEBL-FILE-STATUS <> ZERO)
070714        AND (ERMEBL-FILE-STATUS <> '97')
070714        MOVE 'N'                 TO ME-UPDATE-FLAG
              display ' me open ' ermebl-file-status
070714     end-if
070714     MOVE DTE-CLIENT             TO ME-COMPANY
070714                                                           
070714     COMPUTE MONTH-END-MOYR  =
070714        RUN-CCYY  *  12  +  RUN-MO
070714                                                           
070714     MOVE MONTH-END-MOYR         TO ME-MOYR
070714                                                           
070714     IF ME-DO-UPDATE                                       
070714        READ ERMEBL
070714        if ermebl-file-status <> '00'
070714           MOVE 'N'              TO ME-UPDATE-FLAG    
070714           CLOSE ERMEBL
070714        end-if
              display ' me read ' ermebl-file-status
070714     end-if
070714                                                           
070714     IF ME-DO-UPDATE
              move hld-523-prem-tot to me-523-prem-tot
              display ' me update ' me-523-PREM-TOT
070714        REWRITE MONTH-END-BALANCES
              display ' me rewrite ' ermebl-file-status
070714        CLOSE ERMEBL
070714     end-if

           .
04289  ABEND-PGM SECTION.              COPY ELCABEND.                   EL523
