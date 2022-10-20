00001  IDENTIFICATION DIVISION.                                         09/02/97
00002                                                                   ECS061
00003  PROGRAM-ID.                ECS061.                                  LV001
00004 *              PROGRAM CONVERTED BY                               ECS061
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ECS061
00006 *              CONVERSION DATE 07/25/94 15:25:52.                 ECS061
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          ECS061
00008 *                           VMOD=2.030.                           ECS061
00009                                                                   ECS061
00010 *AUTHOR.        LOGIC, INC.                                       ECS061
00011 *               DALLAS, TEXAS.                                    ECS061
00012                                                                   ECS061
00013 *DATE-COMPILED.                                                   ECS061
00014                                                                   ECS061
00015 *SECURITY.   *****************************************************ECS061
00016 *            *                                                   *ECS061
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ECS061
00018 *            *                                                   *ECS061
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS061
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ECS061
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *ECS061
00022 *            *                                                   *ECS061
00023 *            *****************************************************ECS061
00024                                                                   ECS061
00025 *REMARKS.                                                         ECS061
00026 *        UPDATE ACCOUNTING AND COMMISSION FILE.                   ECS061
00027 *                                                                 ECS061
00028 *    PROGRAM OPTIONS                                              ECS061
00029 *                                                                 ECS061
00030 *    1 - NORMAL CONTROL            - SORT BY EFF DATE / CERT      ECS061
00031 *    2 - ZERO CARRIER              - SORT BY EFF DATE / CERT      ECS061
00032 *    3 - ZERO COMPANY              - SORT BY EFF DATE / CERT      ECS061
00033 *    4 - ZERO CARRIER AND COMPANY  - SORT BY EFF DATE / CERT      ECS061
00034 *    5 - NORMAL CONTROL            - SORT BY CERT / EFF DATE      ECS061
00035 *    6 - ZERO CARRIER              - SORT BY CERT / EFF DATE      ECS061
00036 *    7 - ZERO COMPANY              - SORT BY CERT / EFF DATE      ECS061
00037 *    8 - ZERO CARRIER AND COMPANY  - SORT BY CERT / EFF DATE      ECS061
122002******************************************************************
122002*                   C H A N G E   L O G
122002*
122002* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
122002*-----------------------------------------------------------------
122002*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
122002* EFFECTIVE    NUMBER
122002*-----------------------------------------------------------------
122002* 122002                   PEMA  ADD DCC PROCESSING            
060303* 060303                   SMVA  STOP RECALC ON LITTLE CERTS 
070203* 070203                   SMVA  DONT COUNT OWRITES ON LITTLE CERTS
060205* 060205                   PEMA  ADD COMP 'B' TYPE PROCESSING
032406* 032406  CR2006022800001  AJRA  SET CO-FIRST-WRITTEN-DT TO LOW-VALUES
082707* 082707    2007071200001  PEMA  ADD CHECK DATE TO PYAJ REC
090707* 090707  CR2006050800002  PEMA  ADD CODE FOR NEW AGENT TYPE
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
020310* 020310  CR2008100900004  PEMA  ADD REF4 EXTRACT PROCESSING
020411* 020411  CR2010122100002  PEMA  REMOVE SUFFIX FROM ACCT NAMES
091911* 091911  IR2011090200003  PEMA  FIX LF PREM PROB WITH SPP
080612* 080612  CR2012042700005  PEMA  ADD OVER 120 DAYS FOR AHL
070714* 070714  CR2013060600001  PEMA  AUTOMATE MONTH END BALANCING
050115* 050115  IR2015050100001  PEMA  CHG OW TO INCLUDE ALL FEES
010716* 010716  CR2015082500001  PEMA  VPP CHANGES
122002******************************************************************
00038  EJECT                                                            ECS061
00039  ENVIRONMENT DIVISION.                                            ECS061
00040  INPUT-OUTPUT SECTION.                                            ECS061
00041  FILE-CONTROL.                                                    ECS061
00042                                                                   ECS061
00043      SELECT  SORT-CARD       ASSIGN TO SYS001-DA-FBA1-S-SORTWK1.  ECS061
00044      SELECT  SORT-COMM       ASSIGN TO SYS001-DA-FBA1-S-SORTWK1.  ECS061
00045      SELECT  PRNTR           ASSIGN TO SYS008-UR-1403-S-SYS008.   ECS061
00046      SELECT  COMM-FILE       ASSIGN TO SYS010-UT-2400-S-SYS010.   ECS061
00047      SELECT  COMM-MSTR-IN    ASSIGN TO SYS011-UT-2400-S-SYS011.   ECS061
00048      SELECT  PYADJ-FILE      ASSIGN TO SYS012-UT-2400-S-SYS012.   ECS061
00049      SELECT  TAPE-FILE       ASSIGN TO SYS013-UT-2400-S-SYS013.   ECS061
00050      SELECT  COMM-MSTR-OUT   ASSIGN TO SYS015-UT-FBA1-S-SYS015.   ECS061
00051      SELECT  PYMT-FILE       ASSIGN TO SYS017-UT-FBA1-S-SYS017.   ECS061
00052      SELECT  NEW-A-C         ASSIGN TO SYS018-UT-FBA1-S-SYS018.   ECS061
00053      SELECT  DISK-DATE       ASSIGN TO SYS019-UT-FBA1-S-SYS019.   ECS061
00054      SELECT  FICH            ASSIGN TO SYS020-UT-2400-S-SYS020.   ECS061
00055      SELECT  ERMEBL          ASSIGN SYS024-FBA1-ERMEBL            ECS061
00056                              ORGANIZATION INDEXED                 ECS061
00057                              ACCESS DYNAMIC                       ECS061
00058                              RECORD KEY ME-CONTROL-PRIMARY        ECS061
00059                              FILE STATUS ERMEBL-FILE-STATUS.      ECS061
00060  EJECT                                                            ECS061
00061  DATA DIVISION.                                                   ECS061
00062  FILE SECTION.                                                    ECS061
00063                                                                   ECS061
00064  SD  SORT-CARD.                                                   ECS061
00065  01  SORT-CARD-REC.                                               ECS061
00066      12  SORT-CARD-CONTROL.                                       ECS061
00067          16  SORT-CARD-CODE      PIC  XXX.                        ECS061
00068          16  SORT-CARD-CARR      PIC  X.                          ECS061
00069          16  SORT-CARD-GROUP     PIC  X(6).                       ECS061
00070          16  SORT-CARD-RESP      PIC  X(10).                      ECS061
00071          16  SORT-CARD-ACCT      PIC  X(10).                      ECS061
060205     12  SORT-CO-TYPE            PIC  X.
060205     12  FILLER                  PIC  X(37).
00073      12  SORT-CARD-TYPE          PIC  X.                          ECS061
00074      12  FILLER                  PIC  X(11).                      ECS061
00075  EJECT                                                            ECS061
00076  SD  SORT-COMM.                                                   ECS061
00077  01  SORT-COMM-REC.                                               ECS061
00078      12  SCP-RECORD-ID           PIC  XX.                         ECS061
00079      12  FILLER                  PIC  XX.                         ECS061
00080      12  SCP-CARRIER             PIC  X.                          ECS061
00081      12  SCP-GROUPING            PIC  X(6).                       ECS061
00082      12  SCP-ACCOUNT             PIC  X(10).                      ECS061
00083      12  SCP-REMIT               PIC  X(10).                      ECS061
00084      12  SCP-TRANS               PIC  X.                          ECS061
00085      12  SCP-STATE               PIC  XX.                         ECS061
00086      12  SCP-EFF                 PIC  X(6).                       ECS061
00087      12  SCP-CERT                PIC  X(11).                      ECS061
00088      12  FILLER                  PIC  X(219).                     ECS061
00089  EJECT                                                            ECS061
00090  FD  PRNTR                                                        ECS061
00091                              COPY ELCPRTFD.                       ECS061
00092  EJECT                                                            ECS061
00093  FD  COMM-FILE                                                    ECS061
00094      BLOCK CONTAINS 0 RECORDS
00095      RECORDING MODE F.                                            ECS061
00096  01  COMM-REC.                                                    ECS061
00097      12  FILLER                  PIC  XXXX.                       ECS061
00098      12  CT-CARR-GROUP.                                           ECS061
00099          16  CT-CARRIER          PIC  X.                          ECS061
00100          16  CT-GROUPING         PIC  X(6).                       ECS061
00101      12  CT-ACCOUNT              PIC  X(10).                      ECS061
00102      12  CT-REMIT                PIC  X(10).                      ECS061
00103      12  CT-TRANS                PIC  X.                          ECS061
00104      12  FILLER                  PIC  X(238).                     ECS061
00105  EJECT                                                            ECS061
00106  FD  COMM-MSTR-IN                                                 ECS061
00107                              COPY ECSCOIFD.                       ECS061
00108  EJECT                                                            ECS061
00109  FD  PYADJ-FILE                                                   ECS061
00110      BLOCK CONTAINS 0 RECORDS
00111      RECORDING MODE F.                                            ECS061
00112  01  PYADJ-REC                   PIC  X(80).                      ECS061
00113  EJECT                                                            ECS061
00114  FD  TAPE-FILE                                                    ECS061
00115      BLOCK CONTAINS 0 RECORDS
00116      RECORDING MODE F.                                            ECS061
00117  01  TAPE-REC                    PIC  X(80).                      ECS061
00118  EJECT                                                            ECS061
00119  FD  COMM-MSTR-OUT                                                ECS061
00120                              COPY ECSCOOFD.                       ECS061
00121  EJECT                                                            ECS061
00122  FD  PYMT-FILE                                                    ECS061
00123      BLOCK CONTAINS 0 RECORDS
00124      RECORDING MODE F.                                            ECS061
00125  01  PYMT-REC                    PIC  X(80).                      ECS061
00126  EJECT                                                            ECS061
00127  FD  NEW-A-C                                                      ECS061
00128                              COPY ECSCOMFD.                       ECS061
00129  EJECT                                                            ECS061
00130  FD  DISK-DATE                                                    ECS061
00131                              COPY ELCDTEFD.                       ECS061
00132  EJECT                                                            ECS061
00133  FD  FICH                                                         ECS061
00134                              COPY ELCFCHFD.                       ECS061
00135  EJECT                                                            ECS061
00136  FD  ERMEBL.                                                      ECS061
00137                              COPY ERCMEBL.                        ECS061
00138  EJECT                                                            ECS061
00139  WORKING-STORAGE SECTION.                                         ECS061
00140  77  FILLER PIC  X(32) VALUE '********************************'.  ECS061
00141  77  FILLER PIC  X(32) VALUE '*    ECS061 WORKING-STORAGE    *'.  ECS061
00142  77  FILLER PIC  X(32) VALUE '**********VMOD=2.030************'.  ECS061
00143                                                                   ECS061
00144  77  SORT-OPTION-SW          PIC S9          VALUE +1   COMP-3.   ECS061
00145  77  BUILD-SW                PIC S9          VALUE +0   COMP-3.   ECS061
00146  77  LNCTR                   PIC S9(3)       VALUE +066 COMP-3.   ECS061
00147  77  PGM-SUB                 PIC S9(3)       VALUE +061 COMP-3.   ECS061
00148  77  PGCTR                   PIC S9(5)       VALUE +0   COMP-3.   ECS061
00149  77  ERROR-SW                PIC  X          VALUE SPACE.         ECS061
00150      88  CLA-ERROR                           VALUE '*'.           ECS061
00151      88  CLT-ERROR                           VALUE '*'.           ECS061
00152  77  BREAK-SW                PIC  X          VALUE SPACE.         ECS061
00153      88  BATCH-BREAK                         VALUE '*'.           ECS061
00154  77  SPACE-NP                PIC  X          VALUE '1'.           ECS061
00155  77  SPACE-1                 PIC  X          VALUE ' '.           ECS061
00156  77  SPACE-2                 PIC  X          VALUE '0'.           ECS061
00157  77  SPACE-3                 PIC  X          VALUE '-'.           ECS061
00158  77  X                       PIC  X          VALUE '1'.           ECS061
00159  77  LAST-CONTROL            PIC  X(29)      VALUE LOW-VALUE.     ECS061
020411 77  N1                      PIC S999        VALUE +0 COMP-3.
020411 77  WS-OLD-NAME             PIC X(30)       VALUE SPACES.
00160                                                                   ECS061
00161  01  MONTH-END-DATA.                                              ECS061
00162      12  ME-START-DATE.                                           ECS061
00163          16  ME-START-MO     PIC  99.                             ECS061
00164          16  FILLER          PIC  X.                              ECS061
00165          16  ME-START-DA     PIC  99.                             ECS061
00166          16  FILLER          PIC  X.                              ECS061
00167          16  ME-START-YR     PIC  99.                             ECS061
00168      12  ME-CNDS-DATE        PIC  9(6).                           ECS061
00169      12  ME-CNDS-DATE-R  REDEFINES  ME-CNDS-DATE.                 ECS061
00170          16  ME-CNDS-MO      PIC  99.                             ECS061
00171          16  ME-CNDS-DA      PIC  99.                             ECS061
00172          16  ME-CNDS-YR      PIC  99.                             ECS061
00173      12  ME-START-TIME       PIC  9(6).                           ECS061
00174      12  ME-UPDATE-FLAG      PIC  X          VALUE 'Y'.           ECS061
00175          88  ME-DO-UPDATE                    VALUE 'Y'.           ECS061
00176          88  ME-NO-UPDATE                    VALUE 'N'.           ECS061
00177      12  ERMEBL-FILE-STATUS  PIC  XX.                             ECS061
00178      12  MONTH-END-MOYR      PIC  9(4)                 COMP.      ECS061
070714     12  hld-061-PREM            PIC S9(9)V99  COMP-3 value +0.          
070714     12  hld-061-COMM            PIC S9(9)V99  COMP-3 value +0.          
070714     12  hld-061-OR              PIC S9(9)V99  COMP-3 value +0.          
070714     12  hld-061-PY-ADJ          PIC S9(9)V99  COMP-3 value +0.          
070714     12  hld-061-COMM-RCALC      PIC S9(9)V99  COMP-3 value +0.          
070714     12  hld-061-OR-RCALC        PIC S9(9)V99  COMP-3 value +0.          
070714     12  hld-061-PREM-RCALC      PIC S9(9)V99  COMP-3 value +0.          
070714     12  hld-061-CLMS            PIC S9(9)V99  COMP-3 value +0.
00179                                                                   ECS061
00180  01  CARD-REC.                                                    ECS061
00181      12  CARD-CODE           PIC  XXX.                            ECS061
00182          88  CLA-CARD                        VALUE 'CLA'.         ECS061
00183          88  CLT-CARD                        VALUE 'CLT'.         ECS061
00184      12  CARD-CARRIER        PIC  X.                              ECS061
00185      12  CARD-GROUPING       PIC  X(6).                           ECS061
00186      12  CARD-RESP           PIC  X(10).                          ECS061
00187      12  CARD-ACCT           PIC  X(10).                          ECS061
060205     12  CARD-CO-TYPE        PIC  X.
060205     12  FILLER              PIC  X(49).
00189  EJECT                                                            ECS061
00190  01  MISC-WS.                                                     ECS061
00191      12  PRE-SEQ.                                                 ECS061
00192          16  PRE-CARRIER             PIC  X.                      ECS061
00193          16  PRE-GROUPING            PIC  X(6).                   ECS061
00194      12  CUR-SEQ.                                                 ECS061
00195          16  CUR-CARRIER             PIC  X.                      ECS061
00196          16  CUR-GROUPING            PIC  X(6).                   ECS061
00197      12  A-CONTROL.                                               ECS061
00198          16  AC-COMPANY-CD           PIC  X.                      ECS061
00199          16  AC-CONTROL.                                          ECS061
00200              20  AC-CARR-GROUP.                                   ECS061
00201                  24  AC-CARRIER      PIC  X.                      ECS061
00202                  24  AC-GROUPING     PIC  X(6).                   ECS061
00203              20  AC-RESP-NO          PIC  X(10).                  ECS061
00204              20  AC-ACCOUNT          PIC  X(10).                  ECS061
00205          16  AC-TYPE                 PIC  X.                      ECS061
00206      12  W-CONTROL.                                               ECS061
00207          16  WC-COMPANY-CD           PIC  X.                      ECS061
00208          16  WC-CONTROL.                                          ECS061
00209              20  WC-CARR-GROUP.                                   ECS061
00210                  24  WC-CARRIER      PIC  X.                      ECS061
00211                  24  WC-GROUPING     PIC  X(6).                   ECS061
00212              20  WC-RESP-NO          PIC  X(10).                  ECS061
00213              20  WC-ACCOUNT          PIC  X(10).                  ECS061
00214          16  WC-TYPE                 PIC  X.                      ECS061
00215      12  AGT-SRCH                    PIC  X(29)                   ECS061
00216                                              VALUE LOW-VALUE.     ECS061
00217      12  WORK-AMT                    PIC S9(9)V99                 ECS061
00218                                              VALUE +0   COMP-3.   ECS061
00219      12  WS-RETURN-CODE              PIC S9(4)         COMP.      ECS061
00220      12  WS-ABEND-MESSAGE            PIC  X(80).                  ECS061
00221      12  WS-ABEND-FILE-STATUS        PIC  XX                      ECS061
00222                                              VALUE ZEROS.         ECS061
00223      12  WS-ZERO                     PIC S9                       ECS061
00224                                              VALUE ZERO COMP-3.   ECS061
00225                                                                   ECS061
00226  01  COMPANY-NAME-TABLES.                                         ECS061
00227      12  CARR-GROUP-CNT      PIC S999        VALUE +0 COMP.       ECS061
00228      12  CARR-GROUP-TYPE     OCCURS    200 TIMES.                 ECS061
00229          16  C-G-CARR        PIC X.                               ECS061
00230          16  C-G-GROUP       PIC X(6).                            ECS061
00231          16  C-G-NAME        PIC X(30).                           ECS061
00232      12  CARR-CNT            PIC S999        VALUE +0 COMP.       ECS061
00233      12  CARR-TYPE           OCCURS    200 TIMES.                 ECS061
00234          16  C-CARR          PIC X.                               ECS061
00235          16  C-NAME          PIC X(30).                           ECS061
00236      12  NAME-IDX            PIC S999        VALUE +0 COMP.       ECS061
00237      12  VALID-COMPANY-NAME  PIC X(30)       VALUE SPACES.        ECS061
00238      12  SEARCH-CARR-GROUP.                                       ECS061
00239          16  SEARCH-CARR     PIC X.                               ECS061
00240          16  SEARCH-GROUP    PIC X(6).                            ECS061
00241                                                                   ECS061
00242  EJECT                                                            ECS061
00243  01  A-CARD.                                                      ECS061
00244      12  A-ID                PIC  XXX.                            ECS061
00245      12  A-SEQ.                                                   ECS061
00246          16  A-CARR-GROUP.                                        ECS061
00247              20  A-CARRIER   PIC  X.                              ECS061
00248              20  A-GROUPING  PIC  X(6).                           ECS061
00249          16  A-REMIT         PIC  X(10).                          ECS061
00250          16  A-ACCOUNT       PIC  X(10).                          ECS061
060205     12  A-CO-TYPE           PIC  X.
082707     12  A-MAINT-DATE        PIC 9(6).
082707     12  FILLER              PIC  X.
00252      12  A-DESC              PIC  X(30).                          ECS061
00253      12  A-TYPE              PIC  X.                              ECS061
00254          88  A-VALID-TYPE       VALUE 'R' 'D' 'C' 'S' 'T'         ECS061
00255                                       'U' 'X' 'Y' 'Z'             ECS061
00256                                       'F'.                        ECS061
00257          88  A-VALID-DMD-TYPE   VALUE 'R' 'D' 'C' 'S' 'T'         ECS061
00258                                       'U' 'X' 'Y' 'Z'             ECS061
00259                                       'F' 'A' 'B'.                ECS061
00260      12  A-AMT               PIC S9(7)V99.                        ECS061
00261      12  BW-AMT  REDEFINES                                        ECS061
00262          A-AMT               PIC  X(9).                           ECS061
00263      12  FILLER              PIC  X.                              ECS061
00264      12  A-BILL-FLAG         PIC  X.                              ECS061
00265          88  A-BILLED                        VALUE 'B'.           ECS061
00266                                                                   ECS061
00267  01  WORK-REC.                                                    ECS061
00268      12  FILLER              PIC  XXXX.                           ECS061
00269      12  W-CARR-GROUP.                                            ECS061
00270          16  W-CARRIER       PIC  X.                              ECS061
00271          16  W-GROUPING      PIC  X(6).                           ECS061
00272      12  W-ACCOUNT           PIC  X(10).                          ECS061
00273      12  W-RESP-NO           PIC  X(10).                          ECS061
00274      12  W-TRANS             PIC  X.                              ECS061
           12  FILLER              PIC  X(40).
           12  W-COM-TYPE          PIC  X.
           12  FILLER              PIC  X(197).
00275 *    12  FILLER              PIC  X(238).                         ECS061
00276  EJECT                                                            ECS061
00277                              COPY ECSCOM01.                       ECS061
00278  EJECT                                                            ECS061
00279                              COPY ERCCOMP.                        ECS061
00280  EJECT                                                            ECS061
00281  01  TOTALS-WORK-AREA.                                            ECS061
00282                                                                   ECS061
00283 * BATCH-N    - # OF A B R S C D X Z F BATCH CARDS  - CLT          ECS061
00284 * BATCH-A    - TOTAL OF A BATCH CARDS          - CLT              ECS061
00285 * BATCH-B    - TOTAL OF B BATCH CARDS          - CLT              ECS061
00286 * BATCH-R    - TOTAL OF R BATCH CARDS          - CLT              ECS061
00287 * BATCH-S    - TOTAL OF S BATCH CARDS          - CLT              ECS061
00288 * BATCH-T    - TOTAL OF T BATCH CARDS          - CLT              ECS061
00289 * BATCH-C    - TOTAL OF C BATCH CARDS          - CLT              ECS061
00290 * BATCH-D    - TOTAL OF D BATCH CARDS          - CLT              ECS061
00291 * BATCH-F    - TOTAL OF F BATCH CARDS          - CLT              ECS061
00292 * BATCH-X    - TOTAL OF X BATCH CARDS          - CLT              ECS061
00293 * BATCH-Z    - TOTAL OF Z BATCH CARDS          - CLT              ECS061
00294 * N          - NUMBER OF A B R S C D X Z CARDS - CLA              ECS061
00295 * A          - TOTAL OF A CARDS                - CLA              ECS061
00296 * B          - TOTAL OF B CARDS                - CLA              ECS061
00297 * R          - TOTAL OF R CARDS                - CLA              ECS061
00298 * S          - TOTAL OF S CARDS                - CLA              ECS061
00299 * C          - TOTAL OF C CARDS                - CLA              ECS061
00300 * D          - TOTAL OF D CARDS                - CLA              ECS061
00301 * F          - TOTAL OF F CARDS                - CLA              ECS061
00302 * X          - TOTAL OF X CARDS                - CLA              ECS061
00303 * Z          - TOTAL OF Z CARDS                - CLA              ECS061
00304 * BATCH-N-B  - # OF A B R S C D X Z F BATCH CARDS BILLED - CLT    ECS061
00305 * BATCH-A-B  - TOTAL OF A BATCH CARDS BILLED   - CLT              ECS061
00306 * BATCH-B-B  - TOTAL OF B BATCH CARDS BILLED   - CLT              ECS061
00307 * BATCH-R-B  - TOTAL OF R BATCH CARDS BILLED   - CLT              ECS061
00308 * BATCH-S-B  - TOTAL OF S BATCH CARDS BILLED   - CLT              ECS061
00309 * BATCH-T-B  - TOTAL OF S BATCH CARDS BILLED   - CLT              ECS061
00310 * BATCH-C-B  - TOTAL OF C BATCH CARDS BILLED   - CLT              ECS061
00311 * BATCH-D-B  - TOTAL OF D BATCH CARDS BILLED   - CLT              ECS061
00312 * BATCH-F-B  - TOTAL OF F BATCH CARDS BILLED   - CLT              ECS061
00313 * BATCH-X-B  - TOTAL OF X BATCH CARDS BILLED   - CLT              ECS061
00314 * BATCH-Z-B  - TOTAL OF Z BATCH CARDS BILLED   - CLT              ECS061
00315 * N-B        - NUMBER OF A B R S C D X Z CARDS BILLED - CLA       ECS061
00316 * R-A        - TOTAL OF A CARDS BILLED         - CLA              ECS061
00317 * R-B        - TOTAL OF B CARDS BILLED         - CLA              ECS061
00318 * R-B        - TOTAL OF R CARDS BILLED         - CLA              ECS061
00319 * S-B        - TOTAL OF S CARDS BILLED         - CLA              ECS061
00320 * C-B        - TOTAL OF C CARDS BILLED         - CLA              ECS061
00321 * D-B        - TOTAL OF D CARDS BILLED         - CLA              ECS061
00322 * F-B        - TOTAL OF F CARDS BILLED         - CLA              ECS061
00323 * X-B        - TOTAL OF X CARDS BILLED         - CLA              ECS061
00324 * Z-B        - TOTAL OF Z CARDS BILLED         - CLA              ECS061
00325 * TRAN-N-B   - NUMBER OF ACCT TRANS BILLED - COMM TRAN TAPE       ECS061
00326 * TRAN-N-OW-B- NUMBER OF O/W  TRANS BILLED - COMM TRAN TAPE       ECS061
00327 * TRAN-P-B   - STANDARD PREMIUM BILLED            - COMM TRAN TAPEECS061
00328 * TRAN-C-B   -      PRODUCERS COMPENSATION BILLED - COMM TRAN TAPEECS061
00329 * TRAN-O-B   -      OVERWRITE COMPENSATION BILLED - COMM TRAN TAPEECS061
00330 * TRAN-N     - NUMBER OF ACCT TRANS        - COMMISSION TRAN TAPE ECS061
00331 * TRAN-N-OW  - NUMBER OF O/W  TRANS        - COMMISSION TRAN TAPE ECS061
00332 * TRAN-P     - STANDARD PREMIUM            - COMMISSION TRAN TAPE ECS061
00333 * TRAN-C     -      PRODUCERS COMPENSATION - COMMISSION TRAN TAPE ECS061
00334 * TRAN-O     -      OVERWRITE COMPENSATION - COMMISSION TRAN TAPE ECS061
00335 * RECALC-P   - RECALCULATED PREMIUM        - COMMISSION TRAN TAPE ECS061
00336 * RECALC-C   -      PRODUCERS COMPENSATION - COMMISSION TRAN TAPE ECS061
00337 * RECALC-O   -      OVERWRITE COMPENSATION - COMMISSION TRAN TAPE ECS061
00338 * TRAN-T     - TOTAL PAID CLAIMS           - COMMISSION TRAN TAPE ECS061
00339                                                                   ECS061
00340      12  COMPANY-TOTALS  COMP-3.                                  ECS061
00341          16  COMP-BATCH-N        PIC S9(9)           VALUE +0.    ECS061
00342          16  COMP-BATCH-A        PIC S9(9)V99        VALUE +0.    ECS061
00343          16  COMP-BATCH-B        PIC S9(9)V99        VALUE +0.    ECS061
00344          16  COMP-BATCH-R        PIC S9(9)V99        VALUE +0.    ECS061
00345          16  COMP-BATCH-D        PIC S9(9)V99        VALUE +0.    ECS061
00346          16  COMP-BATCH-C        PIC S9(9)V99        VALUE +0.    ECS061
00347          16  COMP-BATCH-S        PIC S9(9)V99        VALUE +0.    ECS061
00348          16  COMP-BATCH-T        PIC S9(9)V99        VALUE +0.    ECS061
00349          16  COMP-BATCH-U        PIC S9(9)V99        VALUE +0.    ECS061
00350          16  COMP-BATCH-X        PIC S9(9)V99        VALUE +0.    ECS061
00351          16  COMP-BATCH-Y        PIC S9(9)V99        VALUE +0.    ECS061
00352          16  COMP-BATCH-Z        PIC S9(9)V99        VALUE +0.    ECS061
00353          16  COMP-BATCH-F        PIC S9(9)V99        VALUE +0.    ECS061
00354          16  COMP-N              PIC S9(9)           VALUE +0.    ECS061
00355          16  COMP-A              PIC S9(9)V99        VALUE +0.    ECS061
00356          16  COMP-B              PIC S9(9)V99        VALUE +0.    ECS061
00357          16  COMP-R              PIC S9(9)V99        VALUE +0.    ECS061
00358          16  COMP-D              PIC S9(9)V99        VALUE +0.    ECS061
00359          16  COMP-C              PIC S9(9)V99        VALUE +0.    ECS061
00360          16  COMP-S              PIC S9(9)V99        VALUE +0.    ECS061
00361          16  COMP-T              PIC S9(9)V99        VALUE +0.    ECS061
00362          16  COMP-U              PIC S9(9)V99        VALUE +0.    ECS061
00363          16  COMP-X              PIC S9(9)V99        VALUE +0.    ECS061
00364          16  COMP-Y              PIC S9(9)V99        VALUE +0.    ECS061
00365          16  COMP-Z              PIC S9(9)V99        VALUE +0.    ECS061
00366          16  COMP-F              PIC S9(9)V99        VALUE +0.    ECS061
00367          16  COMP-TRAN-N         PIC S9(9)           VALUE +0.    ECS061
00368          16  COMP-TRAN-N-OW      PIC S9(9)           VALUE +0.    ECS061
00369          16  COMP-TRAN-P         PIC S9(9)V99        VALUE +0.    ECS061
00370          16  COMP-TRAN-C         PIC S9(9)V99        VALUE +0.    ECS061
00371          16  COMP-TRAN-O         PIC S9(9)V99        VALUE +0.    ECS061
               16  COMP-TRAN-FEE       PIC S9(9)V99        VALUE +0.
00372          16  COMP-RECALC-P       PIC S9(9)V99        VALUE +0.    ECS061
00373          16  COMP-RECALC-C       PIC S9(9)V99        VALUE +0.    ECS061
00374          16  COMP-RECALC-O       PIC S9(9)V99        VALUE +0.    ECS061
00375          16  COMP-TRAN-T         PIC S9(9)V99        VALUE +0.    ECS061
00376          16  COMP-BATCH-N-B      PIC S9(9)           VALUE +0.    ECS061
00377          16  COMP-BATCH-A-B      PIC S9(9)V99        VALUE +0.    ECS061
00378          16  COMP-BATCH-B-B      PIC S9(9)V99        VALUE +0.    ECS061
00379          16  COMP-BATCH-R-B      PIC S9(9)V99        VALUE +0.    ECS061
00380          16  COMP-BATCH-D-B      PIC S9(9)V99        VALUE +0.    ECS061
00381          16  COMP-BATCH-C-B      PIC S9(9)V99        VALUE +0.    ECS061
00382          16  COMP-BATCH-S-B      PIC S9(9)V99        VALUE +0.    ECS061
00383          16  COMP-BATCH-T-B      PIC S9(9)V99        VALUE +0.    ECS061
00384          16  COMP-BATCH-U-B      PIC S9(9)V99        VALUE +0.    ECS061
00385          16  COMP-BATCH-X-B      PIC S9(9)V99        VALUE +0.    ECS061
00386          16  COMP-BATCH-Y-B      PIC S9(9)V99        VALUE +0.    ECS061
00387          16  COMP-BATCH-Z-B      PIC S9(9)V99        VALUE +0.    ECS061
00388          16  COMP-BATCH-F-B      PIC S9(9)V99        VALUE +0.    ECS061
00389          16  COMP-N-B            PIC S9(9)           VALUE +0.    ECS061
00390          16  COMP-A-B            PIC S9(9)V99        VALUE +0.    ECS061
00391          16  COMP-B-B            PIC S9(9)V99        VALUE +0.    ECS061
00392          16  COMP-R-B            PIC S9(9)V99        VALUE +0.    ECS061
00393          16  COMP-D-B            PIC S9(9)V99        VALUE +0.    ECS061
00394          16  COMP-C-B            PIC S9(9)V99        VALUE +0.    ECS061
00395          16  COMP-S-B            PIC S9(9)V99        VALUE +0.    ECS061
00396          16  COMP-T-B            PIC S9(9)V99        VALUE +0.    ECS061
00397          16  COMP-U-B            PIC S9(9)V99        VALUE +0.    ECS061
00398          16  COMP-X-B            PIC S9(9)V99        VALUE +0.    ECS061
00399          16  COMP-Y-B            PIC S9(9)V99        VALUE +0.    ECS061
00400          16  COMP-Z-B            PIC S9(9)V99        VALUE +0.    ECS061
00401          16  COMP-F-B            PIC S9(9)V99        VALUE +0.    ECS061
00402          16  COMP-TRAN-N-B       PIC S9(9)           VALUE +0.    ECS061
00403          16  COMP-TRAN-N-OW-B    PIC S9(9)           VALUE +0.    ECS061
00404          16  COMP-TRAN-P-B       PIC S9(9)V99        VALUE +0.    ECS061
00405          16  COMP-TRAN-C-B       PIC S9(9)V99        VALUE +0.    ECS061
00406          16  COMP-TRAN-O-B       PIC S9(9)V99        VALUE +0.    ECS061
00407                                                                   ECS061
00408      12  CARRIER-TOTALS  COMP-3.                                  ECS061
00409          16  CARR-BATCH-N        PIC S9(9)           VALUE +0.    ECS061
00410          16  CARR-BATCH-A        PIC S9(9)V99        VALUE +0.    ECS061
00411          16  CARR-BATCH-B        PIC S9(9)V99        VALUE +0.    ECS061
00412          16  CARR-BATCH-R        PIC S9(9)V99        VALUE +0.    ECS061
00413          16  CARR-BATCH-D        PIC S9(9)V99        VALUE +0.    ECS061
00414          16  CARR-BATCH-C        PIC S9(9)V99        VALUE +0.    ECS061
00415          16  CARR-BATCH-S        PIC S9(9)V99        VALUE +0.    ECS061
00416          16  CARR-BATCH-T        PIC S9(9)V99        VALUE +0.    ECS061
00417          16  CARR-BATCH-U        PIC S9(9)V99        VALUE +0.    ECS061
00418          16  CARR-BATCH-X        PIC S9(9)V99        VALUE +0.    ECS061
00419          16  CARR-BATCH-Y        PIC S9(9)V99        VALUE +0.    ECS061
00420          16  CARR-BATCH-Z        PIC S9(9)V99        VALUE +0.    ECS061
00421          16  CARR-BATCH-F        PIC S9(9)V99        VALUE +0.    ECS061
00422          16  CARR-N              PIC S9(9)           VALUE +0.    ECS061
00423          16  CARR-A              PIC S9(9)V99        VALUE +0.    ECS061
00424          16  CARR-B              PIC S9(9)V99        VALUE +0.    ECS061
00425          16  CARR-R              PIC S9(9)V99        VALUE +0.    ECS061
00426          16  CARR-D              PIC S9(9)V99        VALUE +0.    ECS061
00427          16  CARR-C              PIC S9(9)V99        VALUE +0.    ECS061
00428          16  CARR-S              PIC S9(9)V99        VALUE +0.    ECS061
00429          16  CARR-T              PIC S9(9)V99        VALUE +0.    ECS061
00430          16  CARR-U              PIC S9(9)V99        VALUE +0.    ECS061
00431          16  CARR-X              PIC S9(9)V99        VALUE +0.    ECS061
00432          16  CARR-Y              PIC S9(9)V99        VALUE +0.    ECS061
00433          16  CARR-Z              PIC S9(9)V99        VALUE +0.    ECS061
00434          16  CARR-F              PIC S9(9)V99        VALUE +0.    ECS061
00435          16  CARR-TRAN-N         PIC S9(9)           VALUE +0.    ECS061
00436          16  CARR-TRAN-N-OW      PIC S9(9)           VALUE +0.    ECS061
00437          16  CARR-TRAN-P         PIC S9(9)V99        VALUE +0.    ECS061
00438          16  CARR-TRAN-C         PIC S9(9)V99        VALUE +0.    ECS061
00439          16  CARR-TRAN-O         PIC S9(9)V99        VALUE +0.    ECS061
               16  CARR-TRAN-FEE       PIC S9(9)V99        VALUE +0.
00440          16  CARR-RECALC-P       PIC S9(9)V99        VALUE +0.    ECS061
00441          16  CARR-RECALC-C       PIC S9(9)V99        VALUE +0.    ECS061
00442          16  CARR-RECALC-O       PIC S9(9)V99        VALUE +0.    ECS061
00443          16  CARR-TRAN-T         PIC S9(9)V99        VALUE +0.    ECS061
00444          16  CARR-BATCH-N-B      PIC S9(9)           VALUE +0.    ECS061
00445          16  CARR-BATCH-A-B      PIC S9(9)V99        VALUE +0.    ECS061
00446          16  CARR-BATCH-B-B      PIC S9(9)V99        VALUE +0.    ECS061
00447          16  CARR-BATCH-R-B      PIC S9(9)V99        VALUE +0.    ECS061
00448          16  CARR-BATCH-D-B      PIC S9(9)V99        VALUE +0.    ECS061
00449          16  CARR-BATCH-C-B      PIC S9(9)V99        VALUE +0.    ECS061
00450          16  CARR-BATCH-S-B      PIC S9(9)V99        VALUE +0.    ECS061
00451          16  CARR-BATCH-T-B      PIC S9(9)V99        VALUE +0.    ECS061
00452          16  CARR-BATCH-U-B      PIC S9(9)V99        VALUE +0.    ECS061
00453          16  CARR-BATCH-X-B      PIC S9(9)V99        VALUE +0.    ECS061
00454          16  CARR-BATCH-Y-B      PIC S9(9)V99        VALUE +0.    ECS061
00455          16  CARR-BATCH-Z-B      PIC S9(9)V99        VALUE +0.    ECS061
00456          16  CARR-BATCH-F-B      PIC S9(9)V99        VALUE +0.    ECS061
00457          16  CARR-N-B            PIC S9(9)           VALUE +0.    ECS061
00458          16  CARR-A-B            PIC S9(9)V99        VALUE +0.    ECS061
00459          16  CARR-B-B            PIC S9(9)V99        VALUE +0.    ECS061
00460          16  CARR-R-B            PIC S9(9)V99        VALUE +0.    ECS061
00461          16  CARR-D-B            PIC S9(9)V99        VALUE +0.    ECS061
00462          16  CARR-C-B            PIC S9(9)V99        VALUE +0.    ECS061
00463          16  CARR-S-B            PIC S9(9)V99        VALUE +0.    ECS061
00464          16  CARR-T-B            PIC S9(9)V99        VALUE +0.    ECS061
00465          16  CARR-U-B            PIC S9(9)V99        VALUE +0.    ECS061
00466          16  CARR-X-B            PIC S9(9)V99        VALUE +0.    ECS061
00467          16  CARR-Y-B            PIC S9(9)V99        VALUE +0.    ECS061
00468          16  CARR-Z-B            PIC S9(9)V99        VALUE +0.    ECS061
00469          16  CARR-F-B            PIC S9(9)V99        VALUE +0.    ECS061
00470          16  CARR-TRAN-N-B       PIC S9(9)           VALUE +0.    ECS061
00471          16  CARR-TRAN-N-OW-B    PIC S9(9)           VALUE +0.    ECS061
00472          16  CARR-TRAN-P-B       PIC S9(9)V99        VALUE +0.    ECS061
00473          16  CARR-TRAN-C-B       PIC S9(9)V99        VALUE +0.    ECS061
00474          16  CARR-TRAN-O-B       PIC S9(9)V99        VALUE +0.    ECS061
00475                                                                   ECS061
00476      12  FINAL-TOTALS    COMP-3.                                  ECS061
00477          16  FINL-BATCH-N        PIC S9(9)           VALUE +0.    ECS061
00478          16  FINL-BATCH-A        PIC S9(9)V99        VALUE +0.    ECS061
00479          16  FINL-BATCH-B        PIC S9(9)V99        VALUE +0.    ECS061
00480          16  FINL-BATCH-R        PIC S9(9)V99        VALUE +0.    ECS061
00481          16  FINL-BATCH-D        PIC S9(9)V99        VALUE +0.    ECS061
00482          16  FINL-BATCH-C        PIC S9(9)V99        VALUE +0.    ECS061
00483          16  FINL-BATCH-S        PIC S9(9)V99        VALUE +0.    ECS061
00484          16  FINL-BATCH-T        PIC S9(9)V99        VALUE +0.    ECS061
00485          16  FINL-BATCH-U        PIC S9(9)V99        VALUE +0.    ECS061
00486          16  FINL-BATCH-X        PIC S9(9)V99        VALUE +0.    ECS061
00487          16  FINL-BATCH-Y        PIC S9(9)V99        VALUE +0.    ECS061
00488          16  FINL-BATCH-Z        PIC S9(9)V99        VALUE +0.    ECS061
00489          16  FINL-BATCH-F        PIC S9(9)V99        VALUE +0.    ECS061
00490          16  FINL-N              PIC S9(9)           VALUE +0.    ECS061
00491          16  FINL-A              PIC S9(9)V99        VALUE +0.    ECS061
00492          16  FINL-B              PIC S9(9)V99        VALUE +0.    ECS061
00493          16  FINL-R              PIC S9(9)V99        VALUE +0.    ECS061
00494          16  FINL-D              PIC S9(9)V99        VALUE +0.    ECS061
00495          16  FINL-C              PIC S9(9)V99        VALUE +0.    ECS061
00496          16  FINL-S              PIC S9(9)V99        VALUE +0.    ECS061
00497          16  FINL-T              PIC S9(9)V99        VALUE +0.    ECS061
00498          16  FINL-U              PIC S9(9)V99        VALUE +0.    ECS061
00499          16  FINL-X              PIC S9(9)V99        VALUE +0.    ECS061
00500          16  FINL-Y              PIC S9(9)V99        VALUE +0.    ECS061
00501          16  FINL-Z              PIC S9(9)V99        VALUE +0.    ECS061
00502          16  FINL-F              PIC S9(9)V99        VALUE +0.    ECS061
00503          16  FINL-TRAN-N         PIC S9(9)           VALUE +0.    ECS061
00504          16  FINL-TRAN-N-OW      PIC S9(9)           VALUE +0.    ECS061
00505          16  FINL-TRAN-P         PIC S9(9)V99        VALUE +0.    ECS061
00506          16  FINL-TRAN-C         PIC S9(9)V99        VALUE +0.    ECS061
00507          16  FINL-TRAN-O         PIC S9(9)V99        VALUE +0.    ECS061
               16  FINL-TRAN-FEE       PIC S9(9)V99        VALUE +0.
00508          16  FINL-RECALC-P       PIC S9(9)V99        VALUE +0.    ECS061
00509          16  FINL-RECALC-C       PIC S9(9)V99        VALUE +0.    ECS061
00510          16  FINL-RECALC-O       PIC S9(9)V99        VALUE +0.    ECS061
00511          16  FINL-TRAN-T         PIC S9(9)V99        VALUE +0.    ECS061
00512          16  FINL-BATCH-N-B      PIC S9(9)           VALUE +0.    ECS061
00513          16  FINL-BATCH-A-B      PIC S9(9)V99        VALUE +0.    ECS061
00514          16  FINL-BATCH-B-B      PIC S9(9)V99        VALUE +0.    ECS061
00515          16  FINL-BATCH-R-B      PIC S9(9)V99        VALUE +0.    ECS061
00516          16  FINL-BATCH-D-B      PIC S9(9)V99        VALUE +0.    ECS061
00517          16  FINL-BATCH-C-B      PIC S9(9)V99        VALUE +0.    ECS061
00518          16  FINL-BATCH-S-B      PIC S9(9)V99        VALUE +0.    ECS061
00519          16  FINL-BATCH-T-B      PIC S9(9)V99        VALUE +0.    ECS061
00520          16  FINL-BATCH-U-B      PIC S9(9)V99        VALUE +0.    ECS061
00521          16  FINL-BATCH-X-B      PIC S9(9)V99        VALUE +0.    ECS061
00522          16  FINL-BATCH-Y-B      PIC S9(9)V99        VALUE +0.    ECS061
00523          16  FINL-BATCH-Z-B      PIC S9(9)V99        VALUE +0.    ECS061
00524          16  FINL-BATCH-F-B      PIC S9(9)V99        VALUE +0.    ECS061
00525          16  FINL-N-B            PIC S9(9)           VALUE +0.    ECS061
00526          16  FINL-A-B            PIC S9(9)V99        VALUE +0.    ECS061
00527          16  FINL-B-B            PIC S9(9)V99        VALUE +0.    ECS061
00528          16  FINL-R-B            PIC S9(9)V99        VALUE +0.    ECS061
00529          16  FINL-D-B            PIC S9(9)V99        VALUE +0.    ECS061
00530          16  FINL-C-B            PIC S9(9)V99        VALUE +0.    ECS061
00531          16  FINL-S-B            PIC S9(9)V99        VALUE +0.    ECS061
00532          16  FINL-T-B            PIC S9(9)V99        VALUE +0.    ECS061
00533          16  FINL-U-B            PIC S9(9)V99        VALUE +0.    ECS061
00534          16  FINL-X-B            PIC S9(9)V99        VALUE +0.    ECS061
00535          16  FINL-Y-B            PIC S9(9)V99        VALUE +0.    ECS061
00536          16  FINL-Z-B            PIC S9(9)V99        VALUE +0.    ECS061
00537          16  FINL-F-B            PIC S9(9)V99        VALUE +0.    ECS061
00538          16  FINL-TRAN-N-B       PIC S9(9)           VALUE +0.    ECS061
00539          16  FINL-TRAN-N-OW-B    PIC S9(9)           VALUE +0.    ECS061
00540          16  FINL-TRAN-P-B       PIC S9(9)V99        VALUE +0.    ECS061
00541          16  FINL-TRAN-C-B       PIC S9(9)V99        VALUE +0.    ECS061
00542          16  FINL-TRAN-O-B       PIC S9(9)V99        VALUE +0.    ECS061
00543                                                                   ECS061
00544      12  X-TOTALS        COMP-3.                                  ECS061
00545          16  X-BATCH-N           PIC S9(9)           VALUE +0.    ECS061
00546          16  X-BATCH-A           PIC S9(9)V99        VALUE +0.    ECS061
00547          16  X-BATCH-B           PIC S9(9)V99        VALUE +0.    ECS061
00548          16  X-BATCH-R           PIC S9(9)V99        VALUE +0.    ECS061
00549          16  X-BATCH-D           PIC S9(9)V99        VALUE +0.    ECS061
00550          16  X-BATCH-C           PIC S9(9)V99        VALUE +0.    ECS061
00551          16  X-BATCH-S           PIC S9(9)V99        VALUE +0.    ECS061
00552          16  X-BATCH-T           PIC S9(9)V99        VALUE +0.    ECS061
00553          16  X-BATCH-U           PIC S9(9)V99        VALUE +0.    ECS061
00554          16  X-BATCH-X           PIC S9(9)V99        VALUE +0.    ECS061
00555          16  X-BATCH-Y           PIC S9(9)V99        VALUE +0.    ECS061
00556          16  X-BATCH-Z           PIC S9(9)V99        VALUE +0.    ECS061
00557          16  X-BATCH-F           PIC S9(9)V99        VALUE +0.    ECS061
00558          16  X-N                 PIC S9(9)           VALUE +0.    ECS061
00559          16  X-A                 PIC S9(9)V99        VALUE +0.    ECS061
00560          16  X-B                 PIC S9(9)V99        VALUE +0.    ECS061
00561          16  X-R                 PIC S9(9)V99        VALUE +0.    ECS061
00562          16  X-D                 PIC S9(9)V99        VALUE +0.    ECS061
00563          16  X-C                 PIC S9(9)V99        VALUE +0.    ECS061
00564          16  X-S                 PIC S9(9)V99        VALUE +0.    ECS061
00565          16  X-T                 PIC S9(9)V99        VALUE +0.    ECS061
00566          16  X-U                 PIC S9(9)V99        VALUE +0.    ECS061
00567          16  X-X                 PIC S9(9)V99        VALUE +0.    ECS061
00568          16  X-Y                 PIC S9(9)V99        VALUE +0.    ECS061
00569          16  X-Z                 PIC S9(9)V99        VALUE +0.    ECS061
00570          16  X-F                 PIC S9(9)V99        VALUE +0.    ECS061
00571          16  X-TRAN-N            PIC S9(9)           VALUE +0.    ECS061
00572          16  X-TRAN-N-OW         PIC S9(9)           VALUE +0.    ECS061
00573          16  X-TRAN-P            PIC S9(9)V99        VALUE +0.    ECS061
00574          16  X-TRAN-C            PIC S9(9)V99        VALUE +0.    ECS061
00575          16  X-TRAN-O            PIC S9(9)V99        VALUE +0.    ECS061
               16  X-TRAN-FEE          PIC S9(9)V99        VALUE +0.
00576          16  X-RECALC-P          PIC S9(9)V99        VALUE +0.    ECS061
00577          16  X-RECALC-C          PIC S9(9)V99        VALUE +0.    ECS061
00578          16  X-RECALC-O          PIC S9(9)V99        VALUE +0.    ECS061
00579          16  X-TRAN-T            PIC S9(9)V99        VALUE +0.    ECS061
00580          16  X-BATCH-N-B         PIC S9(9)           VALUE +0.    ECS061
00581          16  X-BATCH-A-B         PIC S9(9)V99        VALUE +0.    ECS061
00582          16  X-BATCH-B-B         PIC S9(9)V99        VALUE +0.    ECS061
00583          16  X-BATCH-R-B         PIC S9(9)V99        VALUE +0.    ECS061
00584          16  X-BATCH-D-B         PIC S9(9)V99        VALUE +0.    ECS061
00585          16  X-BATCH-C-B         PIC S9(9)V99        VALUE +0.    ECS061
00586          16  X-BATCH-S-B         PIC S9(9)V99        VALUE +0.    ECS061
00587          16  X-BATCH-T-B         PIC S9(9)V99        VALUE +0.    ECS061
00588          16  X-BATCH-U-B         PIC S9(9)V99        VALUE +0.    ECS061
00589          16  X-BATCH-X-B         PIC S9(9)V99        VALUE +0.    ECS061
00590          16  X-BATCH-Y-B         PIC S9(9)V99        VALUE +0.    ECS061
00591          16  X-BATCH-Z-B         PIC S9(9)V99        VALUE +0.    ECS061
00592          16  X-BATCH-F-B         PIC S9(9)V99        VALUE +0.    ECS061
00593          16  X-N-B               PIC S9(9)           VALUE +0.    ECS061
00594          16  X-A-B               PIC S9(9)V99        VALUE +0.    ECS061
00595          16  X-B-B               PIC S9(9)V99        VALUE +0.    ECS061
00596          16  X-R-B               PIC S9(9)V99        VALUE +0.    ECS061
00597          16  X-D-B               PIC S9(9)V99        VALUE +0.    ECS061
00598          16  X-C-B               PIC S9(9)V99        VALUE +0.    ECS061
00599          16  X-S-B               PIC S9(9)V99        VALUE +0.    ECS061
00600          16  X-T-B               PIC S9(9)V99        VALUE +0.    ECS061
00601          16  X-U-B               PIC S9(9)V99        VALUE +0.    ECS061
00602          16  X-X-B               PIC S9(9)V99        VALUE +0.    ECS061
00603          16  X-Y-B               PIC S9(9)V99        VALUE +0.    ECS061
00604          16  X-Z-B               PIC S9(9)V99        VALUE +0.    ECS061
00605          16  X-F-B               PIC S9(9)V99        VALUE +0.    ECS061
00606          16  X-TRAN-N-B          PIC S9(9)           VALUE +0.    ECS061
00607          16  X-TRAN-N-OW-B       PIC S9(9)           VALUE +0.    ECS061
00608          16  X-TRAN-P-B          PIC S9(9)V99        VALUE +0.    ECS061
00609          16  X-TRAN-C-B          PIC S9(9)V99        VALUE +0.    ECS061
00610          16  X-TRAN-O-B          PIC S9(9)V99        VALUE +0.    ECS061
00611                                                                   ECS061
00612      12  MASTER-CONTROL-TOTALS   COMP-3.                          ECS061
00613          16  CTL-MSTR-RECS-IN    PIC S9(9)           VALUE +0.    ECS061
00614          16  CTL-MSTR-RECS-ADD   PIC S9(9)           VALUE +0.    ECS061
00615          16  CTL-MSTR-RECS-DEL   PIC S9(9)           VALUE +0.    ECS061
00616          16  CTL-MSTR-RECS-OUT   PIC S9(9)           VALUE +0.    ECS061
00617          16  CTL-YTD-IN          PIC S9(9)V99        VALUE +0.    ECS061
00618          16  CTL-YTD-ADJ         PIC S9(9)V99        VALUE +0.    ECS061
00619          16  CTL-YTD-OUT         PIC S9(9)V99        VALUE +0.    ECS061
00620  EJECT                                                            ECS061
00621  01  HEADINGS.                                                    ECS061
00622      12  HD1.                                                     ECS061
00623          16  FILLER          PIC  X(45)      VALUE SPACES.        ECS061
00624          16  FILLER          PIC  X(34)      VALUE                ECS061
00625                  'ACCOUNTING AND COMPENSATION UPDATE'.            ECS061
00626          16  FILLER          PIC  X(45)      VALUE SPACES.        ECS061
00627          16  FILLER          PIC  X(8)       VALUE 'ECS-061'.     ECS061
00628                                                                   ECS061
00629      12  HD2.                                                     ECS061
00630          16  FILLER          PIC  X(47)      VALUE SPACES.        ECS061
00631          16  HD-CO           PIC  X(30).                          ECS061
00632          16  FILLER          PIC  X(47)      VALUE SPACES.        ECS061
00633          16  HD-RD           PIC  X(8).                           ECS061
00634                                                                   ECS061
00635      12  HD3.                                                     ECS061
00636          16  HD2A            PIC  X(6)       VALUE SPACES.        ECS061
00637          16  FILLER          PIC  X(47)      VALUE SPACES.        ECS061
00638          16  HD-DT           PIC  X(18).                          ECS061
00639          16  FILLER          PIC  X(41)      VALUE SPACES.        ECS061
00640          16  FILLER          PIC  X(5)       VALUE 'PAGE '.       ECS061
00641          16  HD-PG           PIC ZZ,ZZ9.                          ECS061
00642                                                                   ECS061
00643      12  HD3A.                                                    ECS061
00644          16  FILLER          PIC  X(44)      VALUE SPACES.        ECS061
00645          16  FILLER          PIC  X(44)      VALUE                ECS061
00646                  '                          MONTH END BILLING '.  ECS061
00647          16  FILLER          PIC  X(44)      VALUE                ECS061
00648                  '                PREVIOUSLY BILLED           '.  ECS061
00649                                                                   ECS061
00650      12  HD4.                                                     ECS061
00651          16  FILLER          PIC  X(44)      VALUE                ECS061
00652                  '              FINANCIAL   ACCOUNT'.             ECS061
00653          16  FILLER          PIC  X(44)      VALUE SPACES.        ECS061
00654          16  FILLER          PIC  X(44)      VALUE SPACES.        ECS061
00655                                                                   ECS061
00656      12  HD4A.                                                    ECS061
00657          16  FILLER          PIC  X(44)      VALUE SPACES.        ECS061
00658          16  FILLER          PIC  X(44)      VALUE                ECS061
00659                  '                        COUNT          AMOUN'.  ECS061
00660          16  FILLER          PIC  X(44)      VALUE                ECS061
00661                  'T             COUNT          AMOUNT         '.  ECS061
00662                                                                   ECS061
00663      12  HD5.                                                     ECS061
00664          16  FILLER          PIC  X(44)      VALUE                ECS061
00665                  'CAR  GROUPING  RESPONS     NUMBER          C'.  ECS061
00666          16  FILLER          PIC  X(44)      VALUE                ECS061
00667                  'OMMENT OR DESCRIPTION        TYPE       AMOU'.  ECS061
00668          16  FILLER          PIC  X(44)      VALUE                ECS061
00669                  'NT                   ERROR MESSAGE'.            ECS061
00670  EJECT                                                            ECS061
00671  01  P-REC.                                                       ECS061
00672      12  P-CCSW                  PIC  X.                          ECS061
00673      12  P-LINE.                                                  ECS061
00674          16  FILLER              PIC  X(132).                     ECS061
00675      12  P-LINE-1  REDEFINES  P-LINE.                             ECS061
00676          16  FILLER              PIC  X.                          ECS061
00677          16  P-CARRIER           PIC  X.                          ECS061
00678          16  FILLER              PIC  XXX.                        ECS061
00679          16  P-GROUPING          PIC  X(6).                       ECS061
00680          16  FILLER              PIC  XXX.                        ECS061
00681          16  P-RESP-NO           PIC  X(10).                      ECS061
00682          16  FILLER              PIC  X.                          ECS061
00683          16  P-ACCOUNT           PIC  X(10).                      ECS061
00684          16  FILLER              PIC  X.                          ECS061
00685          16  P-MSG-X.                                             ECS061
00686              20  FILLER          PIC  X(7).                       ECS061
00687              20  P-DESC          PIC  X(30).                      ECS061
00688          16  FILLER              PIC  XX.                         ECS061
00689          16  P-TYPE              PIC  X.                          ECS061
00690          16  FILLER              PIC  XX.                         ECS061
00691          16  P-AMT               PIC ZZZ,ZZZ,ZZZ.ZZ-.             ECS061
00692          16  P-BAD-AMT-X  REDEFINES  P-AMT.                       ECS061
00693              20  FILLER          PIC  X(6).                       ECS061
00694              20  P-BAD-AMT       PIC  X(9).                       ECS061
00695          16  FILLER              PIC  X.                          ECS061
00696          16  P-COMM              PIC  X.                          ECS061
00697          16  FILLER              PIC  XX.                         ECS061
00698          16  P-BILL-STATUS       PIC  X.                          ECS061
00699          16  FILLER              PIC  X.                          ECS061
00700          16  P-MSG               PIC  X(33).                      ECS061
00701      12  P-LINE-2  REDEFINES  P-LINE.                             ECS061
00702          16  PT-MSG.                                              ECS061
00703              20  FILLER          PIC  X(17).                      ECS061
00704              20  PT-MSG1.                                         ECS061
00705                  24  PT-CARRIER  PIC  X.                          ECS061
00706                  24  FILLER      PIC  X.                          ECS061
00707                  24  PT-GROUPING PIC  X(6).                       ECS061
00708                  24  FILLER      PIC  X.                          ECS061
00709                  24  PT-MSG2     PIC  X(35).                      ECS061
00710                  24  FILLER      PIC  X(3).                       ECS061
00711          16  PT-CTR              PIC ZZZ,ZZZ,ZZZ-.                ECS061
00712          16  FILLER              PIC  X(2).                       ECS061
00713          16  PT-AMT              PIC ZZZ,ZZZ,ZZZ.ZZ-.             ECS061
00714          16  FILLER              PIC  X(5).                       ECS061
00715          16  PT-CTR-B            PIC ZZZ,ZZZ,ZZZ-.                ECS061
00716          16  FILLER              PIC  X(2).                       ECS061
00717          16  PT-AMT-B            PIC ZZZ,ZZZ,ZZZ.ZZ-.             ECS061
00718          16  FILLER              PIC  X(5).                       ECS061
00719  EJECT                                                            ECS061
00720                              COPY ELCDATE.                        ECS061
00721  EJECT                                                            ECS061
00722                              COPY ELCDTECX.                       ECS061
00723  EJECT                                                            ECS061
00724                              COPY ELCDTEVR.                       ECS061
00725  EJECT                                                            ECS061
00726  PROCEDURE DIVISION.                                              ECS061
00727                                                                   ECS061
00728  0000-CAPTURE-START SECTION.
pemuni
pemuni     open OUTPUT COMM-MSTR-OUT.
00734                                                                   ECS061
00735  0000-STANDARD-COPY-RTN SECTION.                                  ECS061
00736
00737  0001-COPY-RTN.                                                   ECS061
00738                              COPY ELCDTERX.                       ECS061
00740      MOVE WS-TIME                TO  ME-START-TIME.               ECS061
00741      MOVE WS-CURRENT-DATE        TO  ME-START-DATE.               ECS061
00742      MOVE ME-START-MO            TO  ME-CNDS-MO.                  ECS061
00743      MOVE ME-START-DA            TO  ME-CNDS-DA.                  ECS061
00744      MOVE ME-START-YR            TO  ME-CNDS-YR.                  ECS061
00745                                                                   ECS061
00757      OPEN OUTPUT PRNTR.                                           ECS061
00758                                                                   ECS061
00759      MOVE SPACE-NP               TO  P-REC.                       ECS061
00760      MOVE WS-CURRENT-DATE        TO  HD-RD.                       ECS061
00761      MOVE COMPANY-NAME           TO  HD-CO.                       ECS061
00762      MOVE ALPH-DATE              TO  HD-DT.                       ECS061
00763                                                                   ECS061
00764      GO TO 1010-SORT-CARDS.                                       ECS061
00765  EJECT                                                            ECS061
00766  1000-SORT-CARD-RTN SECTION.                                      ECS061
00767                                                                   ECS061
00768  1010-SORT-CARDS.                                                 ECS061
00769      IF DTE-PGM-OPT GREATER 4 AND LESS 9                          ECS061
00770          MOVE +2                 TO  SORT-OPTION-SW               ECS061
00771          SUBTRACT  4  FROM  DTE-PGM-OPT.                          ECS061
00772                                                                   ECS061
00773 *   1 - 4    SORT OPTION 1.                                       ECS061
00774 *   5 - 8    SORT OPTION 2.                                       ECS061
00775                                                                   ECS061
00776      SORT SORT-CARD  ON ASCENDING  SORT-CARD-CARR                 ECS061
00777                                    SORT-CARD-GROUP                ECS061
00778                                    SORT-CARD-RESP                 ECS061
00779                                    SORT-CARD-ACCT                 ECS061
00780                                    SORT-CARD-TYPE                 ECS061
00781                                    SORT-CARD-CODE                 ECS061
00782          INPUT PROCEDURE 2000-CARD-INPUT-RTN    THRU  2999-EXIT   ECS061
00783          OUTPUT PROCEDURE 3000-CARD-OUTPUT-RTN  THRU  3999-EXIT.  ECS061
00784                                                                   ECS061
00785      IF SORT-RETURN NOT = ZERO                                    ECS061
00786          MOVE '0101'             TO  WS-RETURN-CODE               ECS061
00787          MOVE 'INTERNAL SORT 01 ABORTED'                          ECS061
00788                                  TO  WS-ABEND-MESSAGE             ECS061
00789          GO TO ABEND-PGM.                                         ECS061
00790                                                                   ECS061
00791      GO TO 4010-SORT-COMM-RTN.                                    ECS061
00792  EJECT                                                            ECS061
00793  2000-CARD-INPUT-RTN SECTION.                                     ECS061
00794                                                                   ECS061
00795  2010-CARD-INPUT-RTN.                                             ECS061
00796      OPEN INPUT TAPE-FILE.                                        ECS061
00797                                                                   ECS061
00798  2100-READ-CARD-RTN.                                              ECS061
00799      READ TAPE-FILE  INTO  CARD-REC                               ECS061
00800          AT END                                                   ECS061
00801              GO TO 2900-END-CARD-INPUT.                           ECS061
00802                                                                   ECS061
00803  2200-CHECK-CARD.                                                 ECS061
00804      IF DTE-CLIENT = 'WDS'                                        ECS061
00805          IF CARD-CARRIER = '2'                                    ECS061
00806              MOVE '1'            TO  CARD-CARRIER.                ECS061
00807                                                                   ECS061
00808 * OPTION 1 - USE FULL CONTROL.                                    ECS061
00809 * OPTION 2 - ZERO CARRIER.                                        ECS061
00810                                                                   ECS061
00811      IF DTE-PGM-OPT = 2                                           ECS061
00812          MOVE '0'                TO  CARD-CARRIER.                ECS061
00813                                                                   ECS061
00814 * OPTION 3 - ZERO COMPANY.                                        ECS061
00815                                                                   ECS061
00816      IF DTE-PGM-OPT = 3                                           ECS061
00817          MOVE '000000'           TO  CARD-GROUPING.               ECS061
00818                                                                   ECS061
00819 * OPTION 4 - ZERO CARRIER AND COMPANY.                            ECS061
00820                                                                   ECS061
00821      IF DTE-PGM-OPT = 4                                           ECS061
00822          MOVE '0'                TO  CARD-CARRIER                 ECS061
00823          MOVE '000000'           TO  CARD-GROUPING.               ECS061
00824                                                                   ECS061
00825      IF DTE-CLIENT = 'ACI'                                        ECS061
00826          MOVE '5'                TO  CARD-CARRIER                 ECS061
00827          MOVE '000000'           TO  CARD-GROUPING.               ECS061
00828                                                                   ECS061
00829      IF DTE-CLIENT = 'ACI'                                        ECS061
00830          IF (CARD-RESP LESS '0000007000')                         ECS061
00831            OR  (CARD-RESP GREATER '0000007999')                   ECS061
00832              MOVE '1'            TO  CARD-CARRIER.                ECS061
00833                                                                   ECS061
00834      IF CLT-CARD                                                  ECS061
00835          MOVE LOW-VALUES         TO  CARD-RESP  CARD-ACCT.        ECS061
00836                                                                   ECS061
00837      IF CARD-ACCT = ZEROS OR SPACES                               ECS061
00838          MOVE LOW-VALUES         TO  CARD-ACCT.                   ECS061
00839                                                                   ECS061
010716     IF (DTE-CLIENT = 'DCC' or 'VPP')
              AND (CARD-CO-TYPE = ' ')
              AND (CARD-ACCT = LOW-VALUES)
              MOVE 'B' TO CARD-CO-TYPE
           END-IF
00840      RELEASE SORT-CARD-REC  FROM  CARD-REC.                       ECS061
00841                                                                   ECS061
00842      GO TO 2100-READ-CARD-RTN.                                    ECS061
00843                                                                   ECS061
00844  2900-END-CARD-INPUT.                                             ECS061
00845      CLOSE TAPE-FILE.                                             ECS061
00846                                                                   ECS061
00847  2999-EXIT.                                                       ECS061
00848      EXIT.                                                        ECS061
00849  EJECT                                                            ECS061
00850  3000-CARD-OUTPUT-RTN SECTION.                                    ECS061
00851                                                                   ECS061
00852  3010-CARD-OUTPUT-RTN.                                            ECS061
00853      OPEN OUTPUT PYMT-FILE.                                       ECS061
00854                                                                   ECS061
00855  3200-RETURN-CARDS.                                               ECS061
00856      RETURN SORT-CARD  INTO  PYMT-REC                             ECS061
00857          AT END                                                   ECS061
00858              GO TO 3900-END-CARD-RTN.                             ECS061
00859                                                                   ECS061
00860      WRITE PYMT-REC.                                              ECS061
00861                                                                   ECS061
00862      GO TO 3200-RETURN-CARDS.                                     ECS061
00863                                                                   ECS061
00864  3900-END-CARD-RTN.                                               ECS061
00865      CLOSE PYMT-FILE.                                             ECS061
00866                                                                   ECS061
00867  3999-EXIT.                                                       ECS061
00868      EXIT.                                                        ECS061
00869  EJECT                                                            ECS061
00870  4000-SORT-COMM-RTN SECTION.                                      ECS061
00871                                                                   ECS061
00872  4010-SORT-COMM-RTN.                                              ECS061
00873      GO TO 4100-SORT-OPTION-ONE                                   ECS061
00874            4200-SORT-OPTION-TWO                                   ECS061
00875                DEPENDING ON SORT-OPTION-SW.                       ECS061
00876                                                                   ECS061
00877  4100-SORT-OPTION-ONE.                                            ECS061
00878      SORT SORT-COMM  ON ASCENDING  SCP-CARRIER                    ECS061
00879                                    SCP-GROUPING                   ECS061
00880                                    SCP-REMIT                      ECS061
00881                                    SCP-ACCOUNT                    ECS061
00882                                    SCP-RECORD-ID                  ECS061
00883                                    SCP-EFF                        ECS061
00884                                    SCP-CERT                       ECS061
00885          INPUT PROCEDURE 5000-COMM-INPUT-RTN    THRU  5999-EXIT   ECS061
00886          OUTPUT PROCEDURE 6000-MATCH-MERGE-RTN  THRU  7999-EXIT.  ECS061
00887                                                                   ECS061
00888      GO TO 4900-TEST-RETURN-RTN.                                  ECS061
00889                                                                   ECS061
00890  4200-SORT-OPTION-TWO.                                            ECS061
00891      SORT SORT-COMM  ON ASCENDING  SCP-CARRIER                    ECS061
00892                                    SCP-GROUPING                   ECS061
00893                                    SCP-REMIT                      ECS061
00894                                    SCP-ACCOUNT                    ECS061
00895                                    SCP-RECORD-ID                  ECS061
00896                                    SCP-CERT                       ECS061
00897                                    SCP-EFF                        ECS061
00898          INPUT PROCEDURE 5000-COMM-INPUT-RTN    THRU  5999-EXIT   ECS061
00899          OUTPUT PROCEDURE 6000-MATCH-MERGE-RTN  THRU  7999-EXIT.  ECS061
00900                                                                   ECS061
00901      GO TO 4900-TEST-RETURN-RTN.                                  ECS061
00902                                                                   ECS061
00903  4900-TEST-RETURN-RTN.                                            ECS061
00904      IF SORT-RETURN NOT = 0                                       ECS061
00905          MOVE '0102'             TO  WS-RETURN-CODE               ECS061
00906          MOVE 'INTERNAL SORT 02 ABORTED'                          ECS061
00907                                  TO  WS-ABEND-MESSAGE             ECS061
00908          GO TO ABEND-PGM.                                         ECS061
00909                                                                   ECS061
00910      GO TO 9200-E-O-J.                                            ECS061
00911  EJECT                                                            ECS061
00912  5000-COMM-INPUT-RTN SECTION.                                     ECS061
00913                                                                   ECS061
00914  5010-COMM-INPUT-RTN.                                             ECS061
00915      OPEN INPUT COMM-FILE.                                        ECS061
00916                                                                   ECS061
00917  5200-READ-COMM-RTN.                                              ECS061
00918      READ COMM-FILE                                               ECS061
00919          AT END                                                   ECS061
00920              GO TO 5900-END-COMM-RTN.                             ECS061
00921                                                                   ECS061
00922      IF DTE-CLIENT = 'WDS'                                        ECS061
00923          IF CT-CARRIER = '2'                                      ECS061
00924              MOVE '1'            TO  CT-CARRIER.                  ECS061
00925                                                                   ECS061
00926 * OPTION 1 - USE FULL CONTROL.                                    ECS061
00927 * OPTION 2 - ZERO CARRIER.                                        ECS061
00928                                                                   ECS061
00929      IF DTE-PGM-OPT = 2                                           ECS061
00930          MOVE '0'                TO  CT-CARRIER.                  ECS061
00931                                                                   ECS061
00932 * OPTION 3 - ZERO COMPANY.                                        ECS061
00933                                                                   ECS061
00934      IF DTE-PGM-OPT = 3                                           ECS061
00935          MOVE '000000'           TO  CT-GROUPING.                 ECS061
00936                                                                   ECS061
00937 * OPTION 4 - ZERO COMPANY AND CARR.                               ECS061
00938                                                                   ECS061
00939      IF DTE-PGM-OPT = 4                                           ECS061
00940          MOVE '0'                TO  CT-CARRIER                   ECS061
00941          MOVE '000000'           TO  CT-GROUPING.                 ECS061
00942                                                                   ECS061
00943      IF DTE-CLIENT = 'ACI'                                        ECS061
00944          MOVE '5'                TO  CT-CARRIER                   ECS061
00945          MOVE '000000'           TO  CT-GROUPING.                 ECS061
00946                                                                   ECS061
00947      IF DTE-CLIENT = 'ACI'                                        ECS061
00948          IF (CT-REMIT LESS '007000')                              ECS061
00949            OR  (CT-REMIT GREATER '007999')                        ECS061
00950              MOVE '1'            TO  CT-CARRIER.                  ECS061
00951                                                                   ECS061
00952      RELEASE SORT-COMM-REC  FROM  COMM-REC.                       ECS061
00953                                                                   ECS061
00954      GO TO 5200-READ-COMM-RTN.                                    ECS061
00955                                                                   ECS061
00956  5900-END-COMM-RTN.                                               ECS061
00957      CLOSE COMM-FILE.                                             ECS061
00958                                                                   ECS061
00959  5999-EXIT.                                                       ECS061
00960      EXIT.                                                        ECS061
00961  EJECT                                                            ECS061
00962  6000-MATCH-MERGE-RTN SECTION.                                    ECS061
00963                                                                   ECS061
00964  6020-OPEN-FILES.                                                 ECS061
00965      OPEN INPUT  PYMT-FILE  COMM-MSTR-IN                          ECS061
pemuni*         OUTPUT NEW-A-C    COMM-MSTR-OUT  PYADJ-FILE.            ECS061
pemuni          OUTPUT NEW-A-C    PYADJ-FILE.                           ECS061
00967                                                                   ECS061
00968      MOVE LOW-VALUE              TO  WORK-REC                     ECS061
00969                                      A-CARD                       ECS061
00970                                      PRE-SEQ                      ECS061
00971                                      CUR-SEQ                      ECS061
00972                                      COMPENSATION-MASTER          ECS061
00973                                      COMP-IN-RECORD               ECS061
00974                                      COMP-OUT-RECORD              ECS061
00975                                      A-CONTROL                    ECS061
00976                                      W-CONTROL                    ECS061
00977                                      AGT-SRCH.                    ECS061
00978                                                                   ECS061
00979      PERFORM 7000-READ-PYMT-RTN  THRU  7199-EXIT.                 ECS061
00980                                                                   ECS061
00981      PERFORM 7200-RETURN-COMM-RTN  THRU  7299-EXIT.               ECS061
00982                                                                   ECS061
00983  6040-MATCH-MERGE.                                                ECS061
00984      IF A-CONTROL LESS W-CONTROL                                  ECS061
00985          MOVE +0                 TO  BUILD-SW                     ECS061
00986          MOVE A-CONTROL          TO  AGT-SRCH                     ECS061
00987          MOVE AC-CARR-GROUP      TO  CUR-SEQ                      ECS061
00988      ELSE                                                         ECS061
00989          MOVE +1                 TO  BUILD-SW                     ECS061
00990          MOVE W-CONTROL          TO  AGT-SRCH                     ECS061
00991          MOVE WC-CARR-GROUP      TO  CUR-SEQ.                     ECS061
00992                                                                   ECS061
00993 *   A COMMISSION TRANSACTION WILL FORCE THE SETUP OF A COMMISSION ECS061
00994 * MASTER. THIS WILL ALLOW A COMM TRANSACTION (AND MATCHING PYMT)  ECS061
00995 * EVEN THOUGH THERE MAY BE NO COMMISSION MASTER.                  ECS061
00996                                                                   ECS061
00997      IF CUR-SEQ NOT = PRE-SEQ                                     ECS061
00998          PERFORM 6600-BATCH-BREAK-RTN  THRU  6699-EXIT            ECS061
00999          MOVE SPACE  TO  BREAK-SW.                                ECS061
01000                                                                   ECS061
01001      IF A-ID = 'CLT'                                              ECS061
01002        AND  A-CARR-GROUP = CUR-SEQ                                ECS061
01003          PERFORM 6500-BATCH-CARD-RTN  THRU  6599-EXIT             ECS061
01004          GO TO 6199-CARD-LO-RETURN.                               ECS061
01005                                                                   ECS061
01006      IF AGT-SRCH NOT = CO-CONTROL-PRIMARY                         ECS061
01007          PERFORM 6300-MATCH-MSTR-RTN  THRU  6399-EXIT.            ECS061
01008                                                                   ECS061
01009      IF W-CONTROL LESS A-CONTROL                                  ECS061
01010          GO TO 6200-COMM-LO.                                      ECS061
01011                                                                   ECS061
01012      IF A-CONTROL = HIGH-VALUES                                   ECS061
01013          PERFORM 7600-FINAL-RTN  THRU  7699-EXIT                  ECS061
01014          GO TO 7900-END-MERGE.                                    ECS061
01015                                                                   ECS061
01016      GO TO 6100-CARD-LO.                                          ECS061
01017  EJECT                                                            ECS061
01018  6100-CARD-LO.                                                    ECS061
01019      INSPECT BW-AMT REPLACING ALL ' ' BY '0'.                     ECS061
01020                                                                   ECS061
01021      PERFORM 6400-CARD-PRINT-RTN  THRU  6499-EXIT.                ECS061
01022                                                                   ECS061
01023      IF A-ID NOT = 'CLA'                                          ECS061
01024          MOVE 'INVALID CARD CODE'  TO  P-MSG                      ECS061
01025          MOVE '*'                  TO  ERROR-SW                   ECS061
01026          PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                   ECS061
01027                                                                   ECS061
01028      IF NOT VALID-CO-ID                                           ECS061
01029          MOVE 'NO COMPENSATION MASTER'  TO  P-MSG                 ECS061
01030          MOVE '*'                       TO  ERROR-SW              ECS061
01031          PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                   ECS061
01032                                                                   ECS061
01033      IF CLA-ERROR                                                 ECS061
01034          GO TO 6199-CARD-LO-RETURN.                               ECS061
01035                                                                   ECS061
01036      MOVE SPACES                 TO  CP-AC-RECORD.                ECS061
01037      MOVE '%%'                   TO  CP-RECORD-ID.                ECS061
01038      MOVE A-CARR-GROUP           TO  CP-CARR-GROUP.               ECS061
01039      MOVE A-ACCOUNT              TO  CP-ACCOUNT.                  ECS061
01040      MOVE A-REMIT                TO  CP-REMIT.                    ECS061
01041      MOVE '5'                    TO  CP-TRANS.                    ECS061
01042      MOVE SPACES                 TO  CP-CERT.                     ECS061
01043      MOVE A-DESC                 TO  CP-AC-DESC.                  ECS061
01044      MOVE ZEROS                  TO  CP-AC-PMT  CP-AC-CHG         ECS061
01045                                      CP-AC-CRD  CP-AC-ADJ.        ECS061
082707     MOVE A-MAINT-DATE           TO  CP-AC-MAINT-DATE
01046      MOVE A-BILL-FLAG            TO  CP-BILL-STATUS.              ECS061
01047                                                                   ECS061
01048      IF A-TYPE = 'A'                                              ECS061
01049          MOVE A-AMT              TO  CP-AC-PMT                    ECS061
01050          IF A-BILLED                                              ECS061
01051              ADD A-AMT           TO  COMP-A-B                     ECS061
01052          ELSE                                                     ECS061
01053              ADD A-AMT           TO  COMP-A.                      ECS061
01054                                                                   ECS061
01055      IF A-TYPE = 'B'                                              ECS061
01056          MOVE A-AMT              TO  CP-AC-PMT                    ECS061
01057          IF A-BILLED                                              ECS061
01058              ADD A-AMT           TO  COMP-B-B                     ECS061
01059          ELSE                                                     ECS061
01060              ADD A-AMT           TO  COMP-B.                      ECS061
01061                                                                   ECS061
01062      IF A-TYPE = 'R'                                              ECS061
01063          MOVE A-AMT              TO  CP-AC-PMT                    ECS061
01064          IF A-BILLED                                              ECS061
01065              ADD A-AMT           TO  COMP-R-B                     ECS061
01066          ELSE                                                     ECS061
01067              ADD A-AMT           TO  COMP-R.                      ECS061
01068                                                                   ECS061
01069      IF A-TYPE = 'S'                                              ECS061
01070          MOVE A-AMT              TO  CP-AC-PMT                    ECS061
01071          IF A-BILLED                                              ECS061
01072              ADD A-AMT           TO  COMP-S-B                     ECS061
01073          ELSE                                                     ECS061
01074              ADD A-AMT           TO  COMP-S.                      ECS061
01075                                                                   ECS061
01076      IF A-TYPE = 'T'                                              ECS061
01077          MOVE A-AMT              TO  CP-AC-PMT                    ECS061
01078          IF A-BILLED                                              ECS061
01079              ADD A-AMT           TO  COMP-T-B                     ECS061
01080          ELSE                                                     ECS061
01081              ADD A-AMT           TO  COMP-T.                      ECS061
01082                                                                   ECS061
01083      IF A-TYPE = 'D'                                              ECS061
01084          MOVE A-AMT              TO  CP-AC-PMT                    ECS061
01085          IF A-BILLED                                              ECS061
01086              ADD A-AMT           TO  COMP-D-B                     ECS061
01087          ELSE                                                     ECS061
01088              ADD A-AMT           TO  COMP-D.                      ECS061
01089                                                                   ECS061
01090      IF A-TYPE = 'C'                                              ECS061
01091          MOVE A-AMT              TO  CP-AC-CHG                    ECS061
01092          IF A-BILLED                                              ECS061
01093              ADD A-AMT           TO  COMP-C-B                     ECS061
01094          ELSE                                                     ECS061
01095              ADD A-AMT           TO  COMP-C.                      ECS061
01096                                                                   ECS061
01097      IF A-TYPE = 'U'                                              ECS061
01098          MOVE A-AMT              TO  CP-AC-CHG                    ECS061
01099          IF A-BILLED                                              ECS061
01100              ADD A-AMT           TO  COMP-U-B                     ECS061
01101          ELSE                                                     ECS061
01102              ADD A-AMT           TO  COMP-U.                      ECS061
01103                                                                   ECS061
01104      IF A-TYPE = 'Z'                                              ECS061
01105          MOVE A-AMT              TO  CP-AC-PMT                    ECS061
01106          IF A-BILLED                                              ECS061
01107              ADD A-AMT           TO  COMP-Z-B                     ECS061
01108          ELSE                                                     ECS061
01109              ADD A-AMT           TO  COMP-Z.                      ECS061
01110                                                                   ECS061
01111      IF A-TYPE = 'X'                                              ECS061
01112          ADD A-AMT               TO  CTL-YTD-ADJ                  ECS061
01113          IF A-BILLED                                              ECS061
01114              ADD A-AMT           TO  COMP-X-B                     ECS061
01115          ELSE                                                     ECS061
01116              ADD A-AMT           TO  COMP-X                       ECS061
01117              IF CO-ACCOUNT-TYPE                                   ECS061
01118                  ADD A-AMT       TO  CO-YTD-COM                   ECS061
01119              ELSE                                                 ECS061
01120                  ADD A-AMT       TO  CO-YTD-OV.                   ECS061
01121                                                                   ECS061
01122      IF A-TYPE = 'Y'                                              ECS061
01123          SUBTRACT A-AMT  FROM  CTL-YTD-ADJ                        ECS061
01124          IF A-BILLED                                              ECS061
01125              SUBTRACT A-AMT  FROM  COMP-X-B                       ECS061
01126          ELSE                                                     ECS061
01127              SUBTRACT A-AMT  FROM  COMP-X                         ECS061
01128              IF CO-ACCOUNT-TYPE                                   ECS061
01129                  SUBTRACT A-AMT  FROM  CO-YTD-COM                 ECS061
01130              ELSE                                                 ECS061
01131                  SUBTRACT A-AMT  FROM  CO-YTD-OV.                 ECS061
01132                                                                   ECS061
01133      IF A-TYPE = 'F'                                              ECS061
01134          MOVE A-AMT              TO  CP-AC-CHG                    ECS061
01135          IF A-BILLED                                              ECS061
01136              ADD A-AMT           TO  COMP-F-B                     ECS061
01137          ELSE                                                     ECS061
01138              ADD A-AMT           TO  COMP-F.                      ECS061
01139                                                                   ECS061
01140      IF A-BILLED                                                  ECS061
01141          ADD +1                  TO  COMP-N-B                     ECS061
01142      ELSE                                                         ECS061
01143          ADD +1                  TO  COMP-N.                      ECS061
01144                                                                   ECS061
01145      IF A-TYPE = 'X'  OR  'Y'                                     ECS061
01146          MOVE 'YTD COMPENSATION ADJUSTED'  TO  P-MSG.             ECS061
01147                                                                   ECS061
01148      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
01149                                                                   ECS061
01150      IF A-TYPE = ('X' OR  'Y' OR 'F')                             ECS061
01151          GO TO 6199-CARD-LO-RETURN                                ECS061
01152      ELSE                                                         ECS061
01153          IF A-BILLED                                              ECS061
01154              IF CP-ACCOUNT = LOW-VALUES OR                        ECS061
01155                 CP-ACCOUNT = CP-REMIT                             ECS061
01156                  GO TO 6199-CARD-LO-RETURN.                       ECS061
01157                                                                   ECS061
01158      IF DTE-CLIENT = 'DMD'                                        ECS061
01159        IF A-TYPE = 'A' OR 'B'                                     ECS061
01160            GO TO 6199-CARD-LO-RETURN.                             ECS061
01161                                                                   ECS061
01162      PERFORM 7500-WRT-COMM-PREM-RTN THRU 7599-EXIT.               ECS061
01163                                                                   ECS061
01164  6199-CARD-LO-RETURN.                                             ECS061
01165      PERFORM 6800-WRITE-PYADJ    THRU  6899-EXIT.                 ECS061
01166                                                                   ECS061
01167      PERFORM 7000-READ-PYMT-RTN  THRU  7199-EXIT.                 ECS061
01168                                                                   ECS061
01169      GO TO 6040-MATCH-MERGE.                                      ECS061
01170  EJECT                                                            ECS061
01171  6200-COMM-LO.                                                    ECS061
01172      MOVE WORK-REC               TO  CP-RECORD.                   ECS061
01173                                                                   ECS061
01174      IF CP-CLAIM                                                  ECS061
01175          GO TO 6260-COMM-CLAIMS.                                  ECS061
01176                                                                   ECS061
01177      IF CP-OVERWT    OR                                           ECS061
01178         CP-RC-OVERWT                                              ECS061
070203         IF NOT CP-MONTHLY-ISSUE
01179              GO TO 6240-COMM-OW
070203         END-IF
070203     END-IF.
01180                                                                   ECS061
070203     IF NOT CP-MONTHLY-ISSUE
01181          IF CP-BILLED           
01182              ADD +1              TO  COMP-TRAN-N-B                ECS061
01183          ELSE    
01184              ADD +1              TO  COMP-TRAN-N
070203         END-IF
070203     END-IF.
01185                                                                   ECS061
01186  6220-COMM-WA.                                                    ECS061
122002     IF (CP-ISSUE OR CP-CANCEL)
122002        IF NOT CP-MONTHLY-ISSUE
01189            IF CP-BILLED                                           ECS061
100703              IF CP-LF-TYPE NOT = '00' AND '  ' AND LOW-VALUES
01190                  ADD CP-LF-PRM      TO  COMP-TRAN-P-B
01191                  ADD CP-LF-PRM-ALT  TO  COMP-TRAN-P-B
01193                  ADD CP-LF-COM      TO  COMP-TRAN-C-B
01194                  ADD CP-LF-COM-ALT  TO  COMP-TRAN-C-B
100703              END-IF
01192               ADD CP-AH-PRM      TO  COMP-TRAN-P-B
01195               ADD CP-AH-COM      TO  COMP-TRAN-C-B
01196            ELSE                                                   ECS061
100703              IF CP-LF-TYPE NOT = '00' AND '  ' AND LOW-VALUES
01197                  ADD CP-LF-PRM      TO  COMP-TRAN-P
01198                  ADD CP-LF-PRM-ALT  TO  COMP-TRAN-P
01200                  ADD CP-LF-COM      TO  COMP-TRAN-C
01201                  ADD CP-LF-COM-ALT  TO  COMP-TRAN-C
100703              END-IF
01199               ADD CP-AH-PRM      TO  COMP-TRAN-P
01202               ADD CP-AH-COM      TO  COMP-TRAN-C
122002           END-IF
122002        END-IF
01203      ELSE
060303        IF NOT CP-MONTHLY-ISSUE
100703           IF CP-LF-TYPE NOT = '00' AND '  ' AND LOW-VALUES
01204               ADD CP-LF-PRM        TO  COMP-RECALC-P
01205               ADD CP-LF-PRM-ALT    TO  COMP-RECALC-P
01207               ADD CP-LF-COM        TO  COMP-RECALC-C
01208               ADD CP-LF-COM-ALT    TO  COMP-RECALC-C
100703           END-IF
01206            ADD CP-AH-PRM         TO  COMP-RECALC-P
01209            ADD CP-AH-COM         TO  COMP-RECALC-C
              END-IF
           END-IF
01210                                                                   ECS061
01211      GO TO 6280-COMM-WRT.                                         ECS061
01212                                                                   ECS061
01213  6240-COMM-OW.                                                    ECS061
01214      IF CP-OW-BILLED                                              ECS061
01215          ADD +1                  TO  COMP-TRAN-N-OW-B             ECS061
01216      ELSE                                                         ECS061
01217          ADD +1                  TO  COMP-TRAN-N-OW.              ECS061
01218                                                                   ECS061
01219      IF CP-OVERWT
01220         IF CP-OW-BILLED
01221            ADD CP-OW-LF-COM-BILLED
01222                                  TO COMP-TRAN-O-B
01223            ADD CP-OW-AH-COM-BILLED
01224                                  TO COMP-TRAN-O-B
01225         ELSE
011410           IF CP-OW-COM-TYPE = 'B' OR 'I' OR 'L' OR 'J' OR 'A'
091911                OR 'N'
                    ADD CP-OW-LF-COM   TO COMP-TRAN-FEE
                    ADD CP-OW-LF-COM-ALT
                                       TO COMP-TRAN-FEE
                    ADD CP-OW-AH-COM   TO COMP-TRAN-FEE
                 ELSE
01226               ADD CP-OW-LF-COM   TO COMP-TRAN-O
01227               ADD CP-OW-LF-COM-ALT
01228                                  TO COMP-TRAN-O
01229               ADD CP-OW-AH-COM   TO COMP-TRAN-O
                 END-IF
              END-IF
01230      ELSE
01231         ADD CP-OW-LF-COM         TO COMP-RECALC-O
01232         ADD CP-OW-LF-COM-ALT     TO COMP-RECALC-O
01233         ADD CP-OW-AH-COM         TO COMP-RECALC-O
           END-IF

01235      GO TO 6280-COMM-WRT.                                         ECS061
01236                                                                   ECS061
01237  6260-COMM-CLAIMS.                                                ECS061
01238      ADD +1                      TO  COMP-TRAN-N.                 ECS061
01239                                                                   ECS061
01240      IF CP-LF-TYPE NOT = SPACES                                   ECS061
01241          ADD CP-CLM-LF-AMT       TO  COMP-TRAN-T.                 ECS061
01242                                                                   ECS061
01243      IF CP-AH-TYPE NOT = SPACES                                   ECS061
01244          ADD CP-CLM-AH-AMT       TO  COMP-TRAN-T.                 ECS061
01245                                                                   ECS061
01246      PERFORM 7500-WRT-COMM-PREM-RTN THRU 7599-EXIT.               ECS061
01247                                                                   ECS061
01248      GO TO 6299-COMM-LO-RETURN.                                   ECS061
01249                                                                   ECS061
01250  6280-COMM-WRT.                                                   ECS061
01251      IF CP-OVERWT  OR                                             ECS061
01252         CP-RC-OVERWT                                              ECS061
01253          NEXT SENTENCE                                            ECS061
01254      ELSE                                                         ECS061
01255          IF CP-BILLED                                             ECS061
01256              IF CP-ACCOUNT = CP-REMIT                             ECS061
01257                  GO TO 6299-COMM-LO-RETURN.                       ECS061
01258                                                                   ECS061
01259      IF CP-OVERWT  OR                                             ECS061
01260         CP-RC-OVERWT                                              ECS061
01261          IF CP-OW-BILLED                                          ECS061
01262              GO TO 6299-COMM-LO-RETURN.                           ECS061
01263                                                                   ECS061
01264      PERFORM 7500-WRT-COMM-PREM-RTN THRU 7599-EXIT.               ECS061
01265                                                                   ECS061
01266  6299-COMM-LO-RETURN.                                             ECS061
01267      PERFORM 7200-RETURN-COMM-RTN  THRU  7299-EXIT.               ECS061
01268                                                                   ECS061
01269      GO TO 6040-MATCH-MERGE.                                      ECS061
01270  EJECT                                                            ECS061
01271  6300-MATCH-MSTR-RTN.                                             ECS061
01272      PERFORM 7400-WRT-MSTR-RTN  THRU  7499-EXIT.                  ECS061
01273                                                                   ECS061
01274      IF CO-AUTO-GENERATED-THIS-RUN                                ECS061
01275          MOVE COMP-IN-RECORD     TO  COMPENSATION-MASTER          ECS061
01276          PERFORM 7700-CHECK-RUN-MONTH  THRU  7799-EXIT            ECS061
01277      ELSE                                                         ECS061
01278          PERFORM 7300-READ-MSTR-RTN  THRU  7399-EXIT.             ECS061
01279                                                                   ECS061
01280      IF AGT-SRCH = CO-CONTROL-PRIMARY                             ECS061
01281          GO TO 6399-EXIT.                                         ECS061
01282                                                                   ECS061
01283      IF AGT-SRCH GREATER CO-CONTROL-PRIMARY                       ECS061
01284          GO TO 6300-MATCH-MSTR-RTN.                               ECS061
01285                                                                   ECS061
01286  6320-BUILD-MSTR.                                                 ECS061
01287      MOVE SPACES TO COMPENSATION-MASTER.                          ECS061
01288                                                                   ECS061
01289      IF BUILD-SW = +0                                             ECS061
01290          MOVE LOW-VALUE          TO  CO-RECORD-ID                 ECS061
01291      ELSE                                                         ECS061
01292          MOVE 'CO'               TO  CO-RECORD-ID                 ECS061
01293          ADD +1                  TO  CTL-MSTR-RECS-ADD.           ECS061
01294                                                                   ECS061
01295      MOVE AGT-SRCH               TO  CO-CONTROL-PRIMARY.          ECS061
01296                                                                   ECS061
01297      MOVE CO-CARR-GROUP          TO  SEARCH-CARR-GROUP.           ECS061
01298                                                                   ECS061
01299      MOVE 'X'                    TO  CO-INTERNAL-CONTROL-1.       ECS061
01300      MOVE 'N'                    TO  CO-INTERNAL-CONTROL-2.       ECS061
01301      MOVE 'Y'                    TO  CO-BALANCE-CONTROL.          ECS061
01302      MOVE 'COMPUTER GENERATED MASTER '                            ECS061
01303                                  TO  CO-ACCT-NAME.                ECS061
01304      MOVE 'NEED MAILING NAME'    TO  CO-MAIL-NAME.                ECS061
01305      MOVE 'NEED ADDRESS LINE 1'  TO  CO-ADDR-1.                   ECS061
01306      MOVE 'NEED ADDRESS LINE 2'  TO  CO-ADDR-2.                   ECS061
01307      MOVE 'NEED CITY,    STATE'  TO  CO-ADDR-3.                   ECS061
01308      MOVE 'ZIP'                  TO  CO-ZIP.                      ECS061
01309      MOVE BIN-RUN-DATE           TO  CO-LAST-MAINT-DT.            ECS061
01310      MOVE DTE-CLIENT             TO  CO-LAST-MAINT-USER.          ECS061
01311      ACCEPT WS-TIME-OF-DAY   FROM TIME.                           ECS061
01312      MOVE WS-TIME                TO  CO-LAST-MAINT-HHMMSS.        ECS061
01313      MOVE ZERO                   TO  CO-BAL-FWD                   ECS061
01314                                      CO-CUR-COM                   ECS061
01315                                      CO-CUR-CHG                   ECS061
01316                                      CO-END-BAL                   ECS061
01317                                      CO-CUR                       ECS061
01318                                      CO-OV30                      ECS061
01319                                      CO-OV60                      ECS061
01320                                      CO-OV90
080612                                     co-ov120
01321                                      CO-YTD-COM                   ECS061
01322                                      CO-YTD-OV                    ECS061
01323                                      CO-YTD-PAID-COM              ECS061
01324                                      CO-YTD-PAID-OV               ECS061
01325                                      CO-LF-CLM-AMT                ECS061
01326                                      CO-AH-CLM-AMT                ECS061
01327                                      CO-CUR-FICA                  ECS061
01328                                      CO-YTD-FICA                  ECS061
01329                                      CO-CUR-OVR-UNDR              ECS061
01330                                      CO-YTD-OVR-UNDR              ECS061
01331                                      CO-LAST-STMT-DT.             ECS061
01332                                                                   ECS061
01333      MOVE ZEROS                  TO  CO-LAST-ACTIVITY-DATE        ECS061
01334                                      CO-CURRENT-LAST-STMT-DT      ECS061
01335                                      CO-CUR-PMT                   ECS061
01336                                      CO-CURRENT-BAL-FWD           ECS061
01337                                      CO-CURRENT-CUR-COM           ECS061
01338                                      CO-CURRENT-CUR-CHG           ECS061
01339                                      CO-CURRENT-CUR-PMT           ECS061
01340                                      CO-CURRENT-END-BAL           ECS061
01341                                      CO-CURRENT-CUR               ECS061
01342                                      CO-CURRENT-OV30              ECS061
01343                                      CO-CURRENT-OV60              ECS061
01344                                      CO-CURRENT-OV90
080612                                     co-current-ov120
01345                                      CO-CURRENT-YTD-COM           ECS061
01346                                      CO-CURRENT-YTD-OV.           ECS061
032406     MOVE LOW-VALUES             TO  CO-FIRST-WRITTEN-DT. 

01348      IF LNCTR GREATER +056                                        ECS061
01349          PERFORM 8600-CARR-GROUP-NAME-RTN THRU 8699-EXIT          ECS061
01350          MOVE VALID-COMPANY-NAME TO HD-CO                         ECS061
01351          PERFORM 8800-HD-RTN  THRU  8899-EXIT.                    ECS061
01352                                                                   ECS061
01353      MOVE SPACE-2                TO  P-CCSW.                      ECS061
01354      MOVE CO-CARRIER             TO  P-CARRIER.                   ECS061
01355      MOVE CO-GROUPING            TO  P-GROUPING.                  ECS061
01356      MOVE CO-RESP-NO             TO  P-RESP-NO.                   ECS061
01357                                                                   ECS061
01358      IF CO-ACCOUNT NOT = LOW-VALUE                                ECS061
01359          MOVE CO-ACCOUNT         TO  P-ACCOUNT.                   ECS061
01360                                                                   ECS061
01361      MOVE CO-ACCT-NAME           TO  P-MSG-X.                     ECS061
01362      MOVE CO-TYPE                TO  P-TYPE.                      ECS061
01363                                                                   ECS061
01364      IF BUILD-SW = +0                                             ECS061
01365          MOVE 'PAY/ADJ CARD UNMATCHED'  TO  P-MSG-X               ECS061
01366          MOVE 'NO MASTER GENERATED'     TO  P-MSG                 ECS061
01367      ELSE                                                         ECS061
01368          MOVE 'TAKE NECESSARY ACTION - UPDATE'  TO  P-MSG.        ECS061
01369                                                                   ECS061
01370      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
01371                                                                   ECS061
01372      MOVE SPACE-2                TO  P-CCSW.                      ECS061
01373                                                                   ECS061
01374  6399-EXIT.                                                       ECS061
01375      EXIT.                                                        ECS061
01376  EJECT                                                            ECS061
01377  6400-CARD-PRINT-RTN.                                             ECS061
01378      MOVE A-CARR-GROUP           TO  SEARCH-CARR-GROUP.           ECS061
01379                                                                   ECS061
01380      IF LNCTR GREATER +056                                        ECS061
01381          PERFORM 8600-CARR-GROUP-NAME-RTN THRU 8699-EXIT          ECS061
01382          MOVE VALID-COMPANY-NAME TO  HD-CO                        ECS061
01383          PERFORM 8800-HD-RTN  THRU  8899-EXIT.                    ECS061
01384                                                                   ECS061
01385      MOVE A-CARRIER              TO  P-CARRIER.                   ECS061
01386      MOVE A-GROUPING             TO  P-GROUPING.                  ECS061
01387                                                                   ECS061
01388      IF A-ACCOUNT NOT = LOW-VALUES                                ECS061
01389          MOVE A-ACCOUNT          TO  P-ACCOUNT.                   ECS061
01390                                                                   ECS061
01391      IF A-REMIT NOT = LOW-VALUES                                  ECS061
01392          MOVE A-REMIT            TO  P-RESP-NO.                   ECS061
01393                                                                   ECS061
010716     examine a-desc replacing all low-values by spaces
01394      MOVE A-DESC                 TO  P-DESC.                      ECS061
01395      MOVE A-TYPE                 TO  P-TYPE.                      ECS061
01396      MOVE A-BILL-FLAG            TO  P-BILL-STATUS.               ECS061
01397                                                                   ECS061
01398      IF DTE-CLIENT = 'DMD'                                        ECS061
01399         IF NOT A-VALID-DMD-TYPE                                   ECS061
01400            MOVE 'INVALID CARD TYPE'  TO  P-MSG                    ECS061
01401            MOVE '*'                  TO  ERROR-SW                 ECS061
01402            PERFORM 8900-PRT-RTN  THRU  8999-EXIT                  ECS061
01403            GO TO 6410-CONTINUE                                    ECS061
01404          ELSE                                                     ECS061
01405            GO TO 6410-CONTINUE.                                   ECS061
01406                                                                   ECS061
01407      IF NOT A-VALID-TYPE                                          ECS061
01408          MOVE 'INVALID CARD TYPE'  TO  P-MSG                      ECS061
01409          MOVE '*'                  TO  ERROR-SW                   ECS061
01410          PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                   ECS061
01411                                                                   ECS061
01412  6410-CONTINUE.                                                   ECS061
01413      IF A-AMT NUMERIC  AND                                        ECS061
01414         A-AMT NOT = ZERO                                          ECS061
01415          MOVE A-AMT              TO  P-AMT                        ECS061
01416      ELSE                                                         ECS061
01417          MOVE BW-AMT             TO  P-BAD-AMT                    ECS061
01418          MOVE 'INVALID AMOUNT'   TO  P-MSG                        ECS061
01419          MOVE '*'                TO  ERROR-SW                     ECS061
01420          PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                   ECS061
01421                                                                   ECS061
01422  6499-EXIT.                                                       ECS061
01423      EXIT.                                                        ECS061
01424                                                                   ECS061
01425  6500-BATCH-CARD-RTN.                                             ECS061
01426      INSPECT BW-AMT REPLACING ALL ' ' BY '0'.                     ECS061
01427                                                                   ECS061
01428      PERFORM 6400-CARD-PRINT-RTN  THRU  6499-EXIT.                ECS061
01429                                                                   ECS061
01430      ADD +1                      TO  COMP-BATCH-N.                ECS061
01431                                                                   ECS061
01432      IF CLT-ERROR                                                 ECS061
01433          GO TO 6599-EXIT.                                         ECS061
01434                                                                   ECS061
01435      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
01436                                                                   ECS061
01437      IF A-TYPE = 'A'                                              ECS061
01438          IF A-BILLED                                              ECS061
01439              ADD A-AMT           TO  COMP-BATCH-A-B               ECS061
01440          ELSE                                                     ECS061
01441              ADD A-AMT           TO  COMP-BATCH-A.                ECS061
01442                                                                   ECS061
01443      IF A-TYPE = 'B'                                              ECS061
01444          IF A-BILLED                                              ECS061
01445              ADD A-AMT           TO  COMP-BATCH-B-B               ECS061
01446          ELSE                                                     ECS061
01447              ADD A-AMT           TO  COMP-BATCH-B.                ECS061
01448                                                                   ECS061
01449      IF A-TYPE = 'R'                                              ECS061
01450          IF A-BILLED                                              ECS061
01451              ADD A-AMT           TO  COMP-BATCH-R-B               ECS061
01452          ELSE                                                     ECS061
01453              ADD A-AMT           TO  COMP-BATCH-R.                ECS061
01454                                                                   ECS061
01455      IF A-TYPE = 'S'                                              ECS061
01456          IF A-BILLED                                              ECS061
01457              ADD A-AMT           TO  COMP-BATCH-S-B               ECS061
01458          ELSE                                                     ECS061
01459              ADD A-AMT           TO  COMP-BATCH-S.                ECS061
01460                                                                   ECS061
01461      IF A-TYPE = 'D'                                              ECS061
01462          IF A-BILLED                                              ECS061
01463              ADD A-AMT           TO  COMP-BATCH-D-B               ECS061
01464          ELSE                                                     ECS061
01465              ADD A-AMT           TO  COMP-BATCH-D.                ECS061
01466                                                                   ECS061
01467      IF A-TYPE = 'T'                                              ECS061
01468          IF A-BILLED                                              ECS061
01469              ADD A-AMT           TO  COMP-BATCH-T-B               ECS061
01470          ELSE                                                     ECS061
01471              ADD A-AMT           TO  COMP-BATCH-T.                ECS061
01472                                                                   ECS061
01473      IF A-TYPE = 'C'                                              ECS061
01474          IF A-BILLED                                              ECS061
01475              ADD A-AMT           TO  COMP-BATCH-C-B               ECS061
01476          ELSE                                                     ECS061
01477              ADD A-AMT           TO  COMP-BATCH-C.                ECS061
01478                                                                   ECS061
01479      IF A-TYPE = 'U'                                              ECS061
01480          IF A-BILLED                                              ECS061
01481              ADD A-AMT           TO  COMP-BATCH-U-B               ECS061
01482          ELSE                                                     ECS061
01483              ADD A-AMT           TO  COMP-BATCH-U.                ECS061
01484                                                                   ECS061
01485      IF A-TYPE = 'Z'                                              ECS061
01486          IF A-BILLED                                              ECS061
01487              ADD A-AMT           TO  COMP-BATCH-Z-B               ECS061
01488          ELSE                                                     ECS061
01489              ADD A-AMT           TO  COMP-BATCH-Z.                ECS061
01490                                                                   ECS061
01491      IF A-TYPE = 'X'                                              ECS061
01492          IF A-BILLED                                              ECS061
01493              ADD A-AMT           TO  COMP-BATCH-X-B               ECS061
01494          ELSE                                                     ECS061
01495              ADD A-AMT           TO  COMP-BATCH-X.                ECS061
01496                                                                   ECS061
01497      IF A-TYPE = 'Y'                                              ECS061
01498          IF A-BILLED                                              ECS061
01499              SUBTRACT A-AMT  FROM  COMP-BATCH-X-B                 ECS061
01500          ELSE                                                     ECS061
01501              SUBTRACT A-AMT  FROM  COMP-BATCH-X.                  ECS061
01502                                                                   ECS061
01503      IF A-TYPE = 'F'                                              ECS061
01504          IF A-BILLED                                              ECS061
01505              ADD A-AMT           TO  COMP-BATCH-F-B               ECS061
01506          ELSE                                                     ECS061
01507              ADD A-AMT           TO  COMP-BATCH-F.                ECS061
01508                                                                   ECS061
01509  6599-EXIT.                                                       ECS061
01510      EXIT.                                                        ECS061
01511                                                                   ECS061
01512  6600-BATCH-BREAK-RTN.                                            ECS061
01513      IF PRE-SEQ = LOW-VALUE                                       ECS061
01514          GO TO 6670-BREAK-CARRIER.                                ECS061
01515                                                                   ECS061
01516      MOVE '*'                    TO  BREAK-SW.                    ECS061
01517                                                                   ECS061
01518      MOVE PRE-SEQ                TO SEARCH-CARR-GROUP.            ECS061
01519      PERFORM 8600-CARR-GROUP-NAME-RTN THRU 8699-EXIT.             ECS061
01520      MOVE VALID-COMPANY-NAME     TO HD-CO.                        ECS061
01521                                                                   ECS061
01522      PERFORM 8800-HD-RTN  THRU  8899-EXIT.                        ECS061
01523                                                                   ECS061
01524      MOVE 'GROUPING TOTALS - '   TO  PT-MSG.                      ECS061
01525      MOVE PRE-CARRIER            TO  PT-CARRIER.                  ECS061
01526      MOVE PRE-GROUPING           TO  PT-GROUPING.                 ECS061
01527      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
01528                                                                   ECS061
01529      MOVE COMPANY-TOTALS         TO  X-TOTALS.                    ECS061
01530                                                                   ECS061
01531      PERFORM 6700-PRINT-TOTALS-RTN  THRU  6799-EXIT.              ECS061
01532                                                                   ECS061
01533      ADD COMP-N                  TO  CARR-N.                      ECS061
01534      ADD COMP-A                  TO  CARR-A.                      ECS061
01535      ADD COMP-B                  TO  CARR-B.                      ECS061
01536      ADD COMP-R                  TO  CARR-R.                      ECS061
01537      ADD COMP-D                  TO  CARR-D.                      ECS061
01538      ADD COMP-C                  TO  CARR-C.                      ECS061
01539      ADD COMP-S                  TO  CARR-S.                      ECS061
01540      ADD COMP-T                  TO  CARR-T.                      ECS061
01541      ADD COMP-U                  TO  CARR-U.                      ECS061
01542      ADD COMP-X                  TO  CARR-X.                      ECS061
01543      ADD COMP-Y                  TO  CARR-Y.                      ECS061
01544      ADD COMP-Z                  TO  CARR-Z.                      ECS061
01545      ADD COMP-F                  TO  CARR-F.                      ECS061
01546      ADD COMP-BATCH-N            TO  CARR-BATCH-N.                ECS061
01547      ADD COMP-BATCH-A            TO  CARR-BATCH-A.                ECS061
01548      ADD COMP-BATCH-B            TO  CARR-BATCH-B.                ECS061
01549      ADD COMP-BATCH-R            TO  CARR-BATCH-R.                ECS061
01550      ADD COMP-BATCH-D            TO  CARR-BATCH-D.                ECS061
01551      ADD COMP-BATCH-C            TO  CARR-BATCH-C.                ECS061
01552      ADD COMP-BATCH-S            TO  CARR-BATCH-S.                ECS061
01553      ADD COMP-BATCH-T            TO  CARR-BATCH-T.                ECS061
01554      ADD COMP-BATCH-U            TO  CARR-BATCH-U.                ECS061
01555      ADD COMP-BATCH-X            TO  CARR-BATCH-X.                ECS061
01556      ADD COMP-BATCH-Y            TO  CARR-BATCH-Y.                ECS061
01557      ADD COMP-BATCH-Z            TO  CARR-BATCH-Z.                ECS061
01558      ADD COMP-BATCH-F            TO  CARR-BATCH-F.                ECS061
01559      ADD COMP-TRAN-N             TO  CARR-TRAN-N.                 ECS061
01560      ADD COMP-TRAN-N-OW          TO  CARR-TRAN-N-OW.              ECS061
01561      ADD COMP-TRAN-P             TO  CARR-TRAN-P.                 ECS061
01562      ADD COMP-TRAN-C             TO  CARR-TRAN-C.                 ECS061
01563      ADD COMP-TRAN-O             TO  CARR-TRAN-O.                 ECS061
           ADD COMP-TRAN-FEE           TO  CARR-TRAN-FEE
01564      ADD COMP-RECALC-P           TO  CARR-RECALC-P.               ECS061
01565      ADD COMP-RECALC-C           TO  CARR-RECALC-C.               ECS061
01566      ADD COMP-RECALC-O           TO  CARR-RECALC-O.               ECS061
01567      ADD COMP-TRAN-T             TO  CARR-TRAN-T.                 ECS061
01568      ADD COMP-N-B                TO  CARR-N-B.                    ECS061
01569      ADD COMP-R-B                TO  CARR-A-B.                    ECS061
01570      ADD COMP-B-B                TO  CARR-B-B.                    ECS061
01571      ADD COMP-R-B                TO  CARR-R-B.                    ECS061
01572      ADD COMP-D-B                TO  CARR-D-B.                    ECS061
01573      ADD COMP-C-B                TO  CARR-C-B.                    ECS061
01574      ADD COMP-S-B                TO  CARR-S-B.                    ECS061
01575      ADD COMP-T-B                TO  CARR-T-B.                    ECS061
01576      ADD COMP-U-B                TO  CARR-U-B.                    ECS061
01577      ADD COMP-X-B                TO  CARR-X-B.                    ECS061
01578      ADD COMP-Y-B                TO  CARR-Y-B.                    ECS061
01579      ADD COMP-Z-B                TO  CARR-Z-B.                    ECS061
01580      ADD COMP-F-B                TO  CARR-F-B.                    ECS061
01581      ADD COMP-BATCH-N-B          TO  CARR-BATCH-N-B.              ECS061
01582      ADD COMP-BATCH-A-B          TO  CARR-BATCH-A-B.              ECS061
01583      ADD COMP-BATCH-B-B          TO  CARR-BATCH-B-B.              ECS061
01584      ADD COMP-BATCH-R-B          TO  CARR-BATCH-R-B.              ECS061
01585      ADD COMP-BATCH-D-B          TO  CARR-BATCH-D-B.              ECS061
01586      ADD COMP-BATCH-C-B          TO  CARR-BATCH-C-B.              ECS061
01587      ADD COMP-BATCH-S-B          TO  CARR-BATCH-S-B.              ECS061
01588      ADD COMP-BATCH-T-B          TO  CARR-BATCH-T-B.              ECS061
01589      ADD COMP-BATCH-U-B          TO  CARR-BATCH-U-B.              ECS061
01590      ADD COMP-BATCH-X-B          TO  CARR-BATCH-X-B.              ECS061
01591      ADD COMP-BATCH-Y-B          TO  CARR-BATCH-Y-B.              ECS061
01592      ADD COMP-BATCH-Z-B          TO  CARR-BATCH-Z-B.              ECS061
01593      ADD COMP-BATCH-F-B          TO  CARR-BATCH-F-B.              ECS061
01594      ADD COMP-TRAN-N-B           TO  CARR-TRAN-N-B.               ECS061
01595      ADD COMP-TRAN-N-OW-B        TO  CARR-TRAN-N-OW-B.            ECS061
01596      ADD COMP-TRAN-P-B           TO  CARR-TRAN-P-B.               ECS061
01597      ADD COMP-TRAN-C-B           TO  CARR-TRAN-C-B.               ECS061
01598      ADD COMP-TRAN-O-B           TO  CARR-TRAN-O-B.               ECS061
01599                                                                   ECS061
01600      IF CUR-CARRIER = PRE-CARRIER                                 ECS061
01601          GO TO 6680-BREAK-COMPANY.                                ECS061
01602                                                                   ECS061
01603      MOVE PRE-SEQ                TO SEARCH-CARR-GROUP.            ECS061
01604      PERFORM 8700-CARR-NAME-RTN THRU 8799-EXIT.                   ECS061
01605      MOVE VALID-COMPANY-NAME     TO HD-CO.                        ECS061
01606      PERFORM 8800-HD-RTN  THRU  8899-EXIT.                        ECS061
01607                                                                   ECS061
01608      MOVE 'CARRIER TOTALS - '    TO  PT-MSG.                      ECS061
01609      MOVE PRE-CARRIER            TO  PT-CARRIER.                  ECS061
01610      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
01611                                                                   ECS061
01612      MOVE CARRIER-TOTALS         TO  X-TOTALS.                    ECS061
01613      PERFORM 6700-PRINT-TOTALS-RTN  THRU  6799-EXIT.              ECS061
01614                                                                   ECS061
01615      ADD CARR-N                  TO  FINL-N.                      ECS061
01616      ADD CARR-A                  TO  FINL-A.                      ECS061
01617      ADD CARR-B                  TO  FINL-B.                      ECS061
01618      ADD CARR-R                  TO  FINL-R.                      ECS061
01619      ADD CARR-D                  TO  FINL-D.                      ECS061
01620      ADD CARR-C                  TO  FINL-C.                      ECS061
01621      ADD CARR-S                  TO  FINL-S.                      ECS061
01622      ADD CARR-T                  TO  FINL-T.                      ECS061
01623      ADD CARR-U                  TO  FINL-U.                      ECS061
01624      ADD CARR-X                  TO  FINL-X.                      ECS061
01625      ADD CARR-Y                  TO  FINL-Y.                      ECS061
01626      ADD CARR-Z                  TO  FINL-Z.                      ECS061
01627      ADD CARR-F                  TO  FINL-F.                      ECS061
01628      ADD CARR-BATCH-N            TO  FINL-BATCH-N.                ECS061
01629      ADD CARR-BATCH-A            TO  FINL-BATCH-A.                ECS061
01630      ADD CARR-BATCH-B            TO  FINL-BATCH-B.                ECS061
01631      ADD CARR-BATCH-R            TO  FINL-BATCH-R.                ECS061
01632      ADD CARR-BATCH-D            TO  FINL-BATCH-D.                ECS061
01633      ADD CARR-BATCH-C            TO  FINL-BATCH-C.                ECS061
01634      ADD CARR-BATCH-S            TO  FINL-BATCH-S.                ECS061
01635      ADD CARR-BATCH-T            TO  FINL-BATCH-T.                ECS061
01636      ADD CARR-BATCH-U            TO  FINL-BATCH-U.                ECS061
01637      ADD CARR-BATCH-X            TO  FINL-BATCH-X.                ECS061
01638      ADD CARR-BATCH-Y            TO  FINL-BATCH-Y.                ECS061
01639      ADD CARR-BATCH-Z            TO  FINL-BATCH-Z.                ECS061
01640      ADD CARR-BATCH-F            TO  FINL-BATCH-F.                ECS061
01641      ADD CARR-TRAN-N             TO  FINL-TRAN-N.                 ECS061
01642      ADD CARR-TRAN-N-OW          TO  FINL-TRAN-N-OW.              ECS061
01643      ADD CARR-TRAN-P             TO  FINL-TRAN-P.                 ECS061
01644      ADD CARR-TRAN-C             TO  FINL-TRAN-C.                 ECS061
01645      ADD CARR-TRAN-O             TO  FINL-TRAN-O.                 ECS061
           ADD CARR-TRAN-FEE           TO  FINL-TRAN-FEE
01646      ADD CARR-RECALC-P           TO  FINL-RECALC-P.               ECS061
01647      ADD CARR-RECALC-C           TO  FINL-RECALC-C.               ECS061
01648      ADD CARR-RECALC-O           TO  FINL-RECALC-O.               ECS061
01649      ADD CARR-TRAN-T             TO  FINL-TRAN-T.                 ECS061
01650      ADD CARR-N-B                TO  FINL-N-B.                    ECS061
01651      ADD CARR-A-B                TO  FINL-A-B.                    ECS061
01652      ADD CARR-B-B                TO  FINL-B-B.                    ECS061
01653      ADD CARR-R-B                TO  FINL-R-B.                    ECS061
01654      ADD CARR-D-B                TO  FINL-D-B.                    ECS061
01655      ADD CARR-C-B                TO  FINL-C-B.                    ECS061
01656      ADD CARR-S-B                TO  FINL-S-B.                    ECS061
01657      ADD CARR-T-B                TO  FINL-T-B.                    ECS061
01658      ADD CARR-U-B                TO  FINL-U-B.                    ECS061
01659      ADD CARR-X-B                TO  FINL-X-B.                    ECS061
01660      ADD CARR-Y-B                TO  FINL-Y-B.                    ECS061
01661      ADD CARR-Z-B                TO  FINL-Z-B.                    ECS061
01662      ADD CARR-F-B                TO  FINL-F-B.                    ECS061
01663      ADD CARR-BATCH-N-B          TO  FINL-BATCH-N-B.              ECS061
01664      ADD CARR-BATCH-A-B          TO  FINL-BATCH-A-B.              ECS061
01665      ADD CARR-BATCH-B-B          TO  FINL-BATCH-B-B.              ECS061
01666      ADD CARR-BATCH-R-B          TO  FINL-BATCH-R-B.              ECS061
01667      ADD CARR-BATCH-D-B          TO  FINL-BATCH-D-B.              ECS061
01668      ADD CARR-BATCH-C-B          TO  FINL-BATCH-C-B.              ECS061
01669      ADD CARR-BATCH-S-B          TO  FINL-BATCH-S-B.              ECS061
01670      ADD CARR-BATCH-T-B          TO  FINL-BATCH-T-B.              ECS061
01671      ADD CARR-BATCH-U-B          TO  FINL-BATCH-U-B.              ECS061
01672      ADD CARR-BATCH-X-B          TO  FINL-BATCH-X-B.              ECS061
01673      ADD CARR-BATCH-Y-B          TO  FINL-BATCH-Y-B.              ECS061
01674      ADD CARR-BATCH-Z-B          TO  FINL-BATCH-Z-B.              ECS061
01675      ADD CARR-BATCH-F-B          TO  FINL-BATCH-F-B.              ECS061
01676      ADD CARR-TRAN-N-B           TO  FINL-TRAN-N-B.               ECS061
01677      ADD CARR-TRAN-N-OW-B        TO  FINL-TRAN-N-OW-B.            ECS061
01678      ADD CARR-TRAN-P-B           TO  FINL-TRAN-P-B.               ECS061
01679      ADD CARR-TRAN-C-B           TO  FINL-TRAN-C-B.               ECS061
01680      ADD CARR-TRAN-O-B           TO  FINL-TRAN-O-B.               ECS061
01681                                                                   ECS061
01682      IF CUR-CARRIER NOT = HIGH-VALUE                              ECS061
01683          GO TO 6670-BREAK-CARRIER.                                ECS061
01684                                                                   ECS061
01685      MOVE COMPANY-NAME           TO  HD-CO.                       ECS061
01686      PERFORM 8800-HD-RTN  THRU  8899-EXIT.                        ECS061
01687                                                                   ECS061
01688      MOVE 'FINAL TOTALS'         TO  PT-MSG.                      ECS061
01689      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
01690                                                                   ECS061
01691      MOVE FINAL-TOTALS           TO  X-TOTALS.                    ECS061
01692                                                                   ECS061
01693      IF ME-DO-UPDATE                                              ECS061
070714        MOVE FINL-TRAN-P         TO hld-061-PREM
070714        MOVE FINL-TRAN-C         TO hld-061-COMM
010716        if dte-client = 'DCC' or 'VPP'
050115           compute hld-061-or =
050115              finl-tran-o + finl-tran-fee
050115        else
070714           MOVE FINL-TRAN-O      TO hld-061-OR
050115        end-if
070714        MOVE FINL-RECALC-P       TO hld-061-PREM-RCALC
070714        MOVE FINL-RECALC-C       TO hld-061-COMM-RCALC
070714        MOVE FINL-RECALC-O       TO hld-061-OR-RCALC
070714        move FINL-TRAN-T         to hld-061-clms
01700         IF DTE-CLIENT = 'UCL'
070714           ADD FINL-TRAN-P-B     TO hld-061-PREM
070714           ADD FINL-TRAN-C-B     TO hld-061-COMM
070714        end-if
070714     end-if
01703                                                                   ECS061
01704      PERFORM 6700-PRINT-TOTALS-RTN  THRU  6799-EXIT.              ECS061
01705                                                                   ECS061
01706      GO TO 6699-EXIT.                                             ECS061
01707                                                                   ECS061
01708  6670-BREAK-CARRIER.                                              ECS061
01709      MOVE CUR-CARRIER            TO  PRE-CARRIER.                 ECS061
01710      INITIALIZE CARRIER-TOTALS.                                   ECS061
01711                                                                   ECS061
01712  6680-BREAK-COMPANY.                                              ECS061
01713      MOVE CUR-GROUPING           TO  PRE-GROUPING.                ECS061
01714      INITIALIZE COMPANY-TOTALS.                                   ECS061
01715      MOVE +066                   TO  LNCTR.                       ECS061
01716                                                                   ECS061
01717      MOVE PRE-SEQ                TO SEARCH-CARR-GROUP.            ECS061
01718      PERFORM 8600-CARR-GROUP-NAME-RTN THRU 8699-EXIT.             ECS061
01719      MOVE VALID-COMPANY-NAME     TO HD-CO.                        ECS061
01720                                                                   ECS061
01721  6699-EXIT.                                                       ECS061
01722      EXIT.                                                        ECS061
01723  EJECT                                                            ECS061
01724  6700-PRINT-TOTALS-RTN.                                           ECS061
01725      IF DTE-CLIENT = 'DMD'                                        ECS061
01726          MOVE 'A - COMM ADJ - POSITIVE'  TO  PT-MSG2              ECS061
01727          MOVE X-A                        TO  PT-AMT               ECS061
01728          MOVE X-A-B                      TO  PT-AMT-B             ECS061
01729          PERFORM 8900-PRT-RTN  THRU  8999-EXIT                    ECS061
01730          MOVE 'B - COMM ADJ - NEGATIVE'  TO  PT-MSG2              ECS061
01731          MOVE X-B                        TO  PT-AMT               ECS061
01732          MOVE X-B-B                      TO  PT-AMT-B             ECS061
01733          PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                   ECS061
01734                                                                   ECS061
01735      MOVE 'R - REMITTANCE RECEIVED'  TO  PT-MSG2.                 ECS061
01736      MOVE X-R                        TO  PT-AMT.                  ECS061
01737      MOVE X-R-B                      TO  PT-AMT-B.                ECS061
01738      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
01739                                                                   ECS061
01740      MOVE 'S - REMITTANCE ADJUSTED'  TO  PT-MSG2.                 ECS061
01741      MOVE X-S                        TO  PT-AMT.                  ECS061
01742      MOVE X-S-B                      TO  PT-AMT-B.                ECS061
01743      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
01744                                                                   ECS061
01745      MOVE 'D - DEPOSIT            '  TO  PT-MSG2.                 ECS061
01746      MOVE X-D                        TO  PT-AMT.                  ECS061
01747      MOVE X-D-B                      TO  PT-AMT-B.                ECS061
01748      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
01749                                                                   ECS061
01750      MOVE 'T - DEPOSIT ADJUSTED   '  TO  PT-MSG2.                 ECS061
01751      MOVE X-T                        TO  PT-AMT.                  ECS061
01752      MOVE X-T-B                      TO  PT-AMT-B.                ECS061
01753      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
01754                                                                   ECS061
01755      MOVE 'C - CHARGE TO AGENT    '  TO  PT-MSG2.                 ECS061
01756      MOVE X-C                        TO  PT-AMT.                  ECS061
01757      MOVE X-C-B                      TO  PT-AMT-B.                ECS061
01758      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
01759                                                                   ECS061
01760      MOVE 'U - CHARGES ADJUSTED    ' TO  PT-MSG2.                 ECS061
01761      MOVE X-U                        TO  PT-AMT.                  ECS061
01762      MOVE X-U-B                      TO  PT-AMT-B.                ECS061
01763      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
01764                                                                   ECS061
01765      MOVE 'PAYMENTS AND ADJUSTMENTS (NET CASH)'                   ECS061
01766                                  TO  PT-MSG1.                     ECS061
01767                                                                   ECS061
01768      COMPUTE WORK-AMT  = (X-R  +  X-S)  +  (X-D  +  X-T)          ECS061
01769                        + (X-A  -  X-B)                            ECS061
01770                        - (X-C  +  X-U).                           ECS061
01771                                                                   ECS061
01772      MOVE WORK-AMT               TO  PT-AMT.                      ECS061
01773                                                                   ECS061
01774      COMPUTE WORK-AMT  = (X-R-B  +  X-S-B)  +                     ECS061
01775                          (X-D-B  +  X-T-B)  +                     ECS061
01776                          (X-A-B  -  X-B-B)  -                     ECS061
01777                          (X-C-B  +  X-U-B).                       ECS061
01778                                                                   ECS061
01779      MOVE WORK-AMT               TO  PT-AMT-B.                    ECS061
01780      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
01781                                                                   ECS061
01782      MOVE SPACE-2                    TO  P-CCSW.                  ECS061
01783      MOVE 'Z - ADJUST NET DUE      ' TO  PT-MSG2.                 ECS061
01784      MOVE X-Z                        TO  PT-AMT.                  ECS061
01785      MOVE X-Z-B                      TO  PT-AMT-B.                ECS061
01786      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
01787                                                                   ECS061
01788      MOVE 'PAYMENTS AND ADJUSTMENTS (NET BALANCE)'                ECS061
01789                                  TO  PT-MSG1.                     ECS061
01790                                                                   ECS061
01791      COMPUTE WORK-AMT  = (X-R  +  X-S)  +  (X-D  +  X-T)          ECS061
01792                        + (X-A  -  X-B)                            ECS061
01793                        - (X-C  +  X-U)  +  X-Z.                   ECS061
01794                                                                   ECS061
01795      MOVE WORK-AMT               TO  PT-AMT                       ECS061
070714                                     hld-061-PY-ADJ
01797                                                                   ECS061
01798      COMPUTE WORK-AMT  = (X-R-B  +  X-S-B)  +                     ECS061
01799                          (X-D-B  +  X-T-B)  +                     ECS061
01800                          (X-A-B  -  X-B-B)  -                     ECS061
01801                          (X-C-B  +  X-U-B)  +                     ECS061
01802                          (X-Z-B).                                 ECS061
01803                                                                   ECS061
01804      MOVE WORK-AMT               TO  PT-AMT-B.                    ECS061
01805      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
01806                                                                   ECS061
01807      MOVE SPACE-2                    TO  P-CCSW.                  ECS061
01808      MOVE 'F - FICA AMOUNT ENTERED ' TO  PT-MSG2.                 ECS061
01809      MOVE X-F                        TO  PT-AMT.                  ECS061
01810      MOVE X-F-B                      TO  PT-AMT-B.                ECS061
01811      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
01812                                                                   ECS061
01813      MOVE 'X OR Y - ADJ YTD COMP   ' TO  PT-MSG2.                 ECS061
01814      MOVE X-X                        TO  PT-AMT.                  ECS061
01815      MOVE X-X-B                      TO  PT-AMT-B.                ECS061
01816      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
01817                                                                   ECS061
01818      MOVE 'TRANSACTION GROSS TOTALS' TO  PT-MSG1.                 ECS061
01819      MOVE X-N                        TO  PT-CTR.                  ECS061
01820                                                                   ECS061
01821      COMPUTE WORK-AMT  =  X-R  +  X-D  +  X-C                     ECS061
01822                        +  X-A  -  X-B                             ECS061
01823                        +  X-S  +  X-T  +  X-U                     ECS061
01824                        +  X-X  +  X-Y  +  X-Z                     ECS061
01825                        +  X-F.                                    ECS061
01826                                                                   ECS061
01827      MOVE WORK-AMT               TO  PT-AMT.                      ECS061
01828      MOVE X-N-B                  TO  PT-CTR-B.                    ECS061
01829                                                                   ECS061
01830      COMPUTE WORK-AMT  =  X-R-B  +  X-D-B  +  X-C-B               ECS061
01831                        +  X-A-B  -  X-B-B                         ECS061
01832                        +  X-S-B  +  X-T-B  +  X-U-B               ECS061
01833                        +  X-X-B  +  X-Y-B  +  X-Z-B               ECS061
01834                        +  X-F-B.                                  ECS061
01835                                                                   ECS061
01836      MOVE WORK-AMT               TO  PT-AMT-B                     ECS061
01837      MOVE SPACE-2                TO  P-CCSW.                      ECS061
01838      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
01839                                                                   ECS061
01840      IF X-BATCH-N   = +0   AND                                    ECS061
01841         X-BATCH-N-B = +0                                          ECS061
01842          GO TO 6790-PRINT-TOTAL-TRANS.                            ECS061
01843                                                                   ECS061
01844      MOVE SPACE-2                   TO  P-CCSW.                   ECS061
01845      MOVE 'BATCH TOTALS SUBMITTED'  TO  PT-MSG1.                  ECS061
01846      MOVE X-BATCH-N                 TO  PT-CTR.                   ECS061
01847                                                                   ECS061
01848      COMPUTE WORK-AMT  =  X-BATCH-R  +  X-BATCH-D  +  X-BATCH-C   ECS061
01849                        +  X-BATCH-A  -  X-BATCH-B                 ECS061
01850                        +  X-BATCH-S  +  X-BATCH-T  +  X-BATCH-U   ECS061
01851                        +  X-BATCH-X  +  X-BATCH-Y  +  X-BATCH-Z   ECS061
01852                        +  X-BATCH-F.                              ECS061
01853                                                                   ECS061
01854      MOVE WORK-AMT               TO  PT-AMT.                      ECS061
01855      MOVE X-BATCH-N-B            TO  PT-CTR-B.                    ECS061
01856                                                                   ECS061
01857      COMPUTE WORK-AMT  =  X-BATCH-R-B + X-BATCH-D-B + X-BATCH-C-B ECS061
01858                        +  X-BATCH-A-B - X-BATCH-B-B               ECS061
01859                        +  X-BATCH-S-B + X-BATCH-T-B + X-BATCH-U-B ECS061
01860                        +  X-BATCH-X-B + X-BATCH-Y-B + X-BATCH-Z-B ECS061
01861                        +  X-BATCH-F-B.                            ECS061
01862                                                                   ECS061
01863      MOVE WORK-AMT               TO  PT-AMT-B.                    ECS061
01864      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
01865                                                                   ECS061
01866      IF DTE-CLIENT = 'DMD'                                        ECS061
01867          MOVE 'A - COMM ADJ - POSITIVE'  TO  PT-MSG2              ECS061
01868          MOVE X-BATCH-A                  TO  PT-AMT               ECS061
01869          MOVE X-BATCH-A-B                TO  PT-AMT-B             ECS061
01870          PERFORM 8900-PRT-RTN  THRU  8999-EXIT                    ECS061
01871          MOVE 'B - COMM ADJ - NEGATIVE'  TO  PT-MSG2              ECS061
01872          MOVE X-BATCH-B                  TO  PT-AMT               ECS061
01873          MOVE X-BATCH-B-B                TO  PT-AMT-B             ECS061
01874          PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                   ECS061
01875                                                                   ECS061
01876      MOVE 'R - REMITTANCE RECEIVED'  TO  PT-MSG2.                 ECS061
01877      MOVE X-BATCH-R                  TO  PT-AMT.                  ECS061
01878      MOVE X-BATCH-R-B                TO  PT-AMT-B.                ECS061
01879      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
01880                                                                   ECS061
01881      MOVE 'S - REMITTANCE ADJUSTED'  TO  PT-MSG2.                 ECS061
01882      MOVE X-BATCH-S                  TO  PT-AMT.                  ECS061
01883      MOVE X-BATCH-S-B                TO  PT-AMT-B.                ECS061
01884      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
01885                                                                   ECS061
01886      MOVE 'D - DEPOSIT            '  TO  PT-MSG2.                 ECS061
01887      MOVE X-BATCH-D                  TO  PT-AMT.                  ECS061
01888      MOVE X-BATCH-D-B                TO  PT-AMT-B.                ECS061
01889      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
01890                                                                   ECS061
01891      MOVE 'D - DEPOSIT ADJUSTED   '  TO  PT-MSG2.                 ECS061
01892      MOVE X-BATCH-T                  TO  PT-AMT.                  ECS061
01893      MOVE X-BATCH-T-B                TO  PT-AMT-B.                ECS061
01894      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
01895                                                                   ECS061
01896      MOVE 'C - CHARGE TO AGENT'  TO  PT-MSG2.                     ECS061
01897      MOVE X-BATCH-C              TO  PT-AMT.                      ECS061
01898      MOVE X-BATCH-C-B            TO  PT-AMT-B.                    ECS061
01899      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
01900                                                                   ECS061
01901      MOVE 'U - CHARGES ADJUSTED'  TO  PT-MSG2.                    ECS061
01902      MOVE X-BATCH-U               TO  PT-AMT.                     ECS061
01903      MOVE X-BATCH-U-B             TO  PT-AMT-B.                   ECS061
01904      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
01905                                                                   ECS061
01906      MOVE 'PAYMENTS AND ADJUSTMENTS (NET CASH)'                   ECS061
01907                                  TO  PT-MSG1.                     ECS061
01908      COMPUTE WORK-AMT  = (X-BATCH-R + X-BATCH-S) +                ECS061
01909                          (X-BATCH-A - X-BATCH-B) +                ECS061
01910                          (X-BATCH-D + X-BATCH-T) -                ECS061
01911                          (X-BATCH-C + X-BATCH-U).                 ECS061
01912      MOVE WORK-AMT               TO  PT-AMT.                      ECS061
01913      COMPUTE WORK-AMT  = (X-BATCH-R-B + X-BATCH-S-B) +            ECS061
01914                          (X-BATCH-A-B - X-BATCH-B-B) +            ECS061
01915                          (X-BATCH-D-B + X-BATCH-T-B) -            ECS061
01916                          (X-BATCH-C-B + X-BATCH-U-B).             ECS061
01917      MOVE WORK-AMT               TO  PT-AMT-B.                    ECS061
01918      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
01919                                                                   ECS061
01920      MOVE 'Z - ADJUST NET DUE '  TO  PT-MSG2.                     ECS061
01921      MOVE X-BATCH-Z              TO  PT-AMT.                      ECS061
01922      MOVE X-BATCH-Z-B            TO  PT-AMT-B.                    ECS061
01923      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
01924                                                                   ECS061
01925      MOVE 'PAYMENTS AND ADJUSTMENTS (NET BALANCE)'                ECS061
01926                                  TO  PT-MSG1.                     ECS061
01927      COMPUTE WORK-AMT  = (X-BATCH-R-B + X-BATCH-S-B) +            ECS061
01928                          (X-BATCH-A-B - X-BATCH-B-B) +            ECS061
01929                          (X-BATCH-D-B + X-BATCH-T-B) -            ECS061
01930                          (X-BATCH-C-B + X-BATCH-U-B) +            ECS061
01931                          (X-BATCH-Z-B).                           ECS061
01932      MOVE WORK-AMT               TO  PT-AMT.                      ECS061
01933      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
01934                                                                   ECS061
01935      MOVE 'X - ADD TO TYD COMP    '  TO  PT-MSG2.                 ECS061
01936      MOVE X-BATCH-X                  TO  PT-AMT.                  ECS061
01937      MOVE X-BATCH-X-B                TO  PT-AMT-B.                ECS061
01938      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
01939                                                                   ECS061
01940      MOVE 'Y- SUBT FROM YTD COMP  '  TO  PT-MSG2.                 ECS061
01941      MOVE X-BATCH-Y                  TO  PT-AMT.                  ECS061
01942      MOVE X-BATCH-Y-B                TO  PT-AMT-B.                ECS061
01943      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
01944                                                                   ECS061
01945      MOVE 'F - FICA AMOUNT ENTERED'  TO  PT-MSG2.                 ECS061
01946      MOVE X-BATCH-F                  TO  PT-AMT.                  ECS061
01947      MOVE X-BATCH-F-B                TO  PT-AMT-B.                ECS061
01948      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
01949                                                                   ECS061
01950  6780-PRINT-BATCH-DIFFERENCES.                                    ECS061
01951      SUBTRACT X-A    FROM  X-BATCH-A.                             ECS061
01952      SUBTRACT X-B    FROM  X-BATCH-B.                             ECS061
01953      SUBTRACT X-R    FROM  X-BATCH-R.                             ECS061
01954      SUBTRACT X-D    FROM  X-BATCH-D.                             ECS061
01955      SUBTRACT X-C    FROM  X-BATCH-C.                             ECS061
01956      SUBTRACT X-S    FROM  X-BATCH-S.                             ECS061
01957      SUBTRACT X-T    FROM  X-BATCH-T.                             ECS061
01958      SUBTRACT X-U    FROM  X-BATCH-U.                             ECS061
01959      SUBTRACT X-X    FROM  X-BATCH-X.                             ECS061
01960      SUBTRACT X-Y    FROM  X-BATCH-Y.                             ECS061
01961      SUBTRACT X-Z    FROM  X-BATCH-Z.                             ECS061
01962      SUBTRACT X-F    FROM  X-BATCH-F.                             ECS061
01963      SUBTRACT X-A-B  FROM  X-BATCH-A-B.                           ECS061
01964      SUBTRACT X-B-B  FROM  X-BATCH-B-B.                           ECS061
01965      SUBTRACT X-R-B  FROM  X-BATCH-R-B.                           ECS061
01966      SUBTRACT X-D-B  FROM  X-BATCH-D-B.                           ECS061
01967      SUBTRACT X-C-B  FROM  X-BATCH-C-B.                           ECS061
01968      SUBTRACT X-S-B  FROM  X-BATCH-S-B.                           ECS061
01969      SUBTRACT X-T-B  FROM  X-BATCH-T-B.                           ECS061
01970      SUBTRACT X-U-B  FROM  X-BATCH-U-B.                           ECS061
01971      SUBTRACT X-X-B  FROM  X-BATCH-X-B.                           ECS061
01972      SUBTRACT X-Y-B  FROM  X-BATCH-Y-B.                           ECS061
01973      SUBTRACT X-Z-B  FROM  X-BATCH-Z-B.                           ECS061
01974      SUBTRACT X-F-B  FROM  X-BATCH-F-B.                           ECS061
01975                                                                   ECS061
01976      IF X-BATCH-R = ZEROS                                         ECS061
01977        AND  X-BATCH-A = ZEROS                                     ECS061
01978        AND  X-BATCH-B = ZEROS                                     ECS061
01979        AND  X-BATCH-D = ZEROS                                     ECS061
01980        AND  X-BATCH-C = ZEROS                                     ECS061
01981        AND  X-BATCH-S = ZEROS                                     ECS061
01982        AND  X-BATCH-T = ZEROS                                     ECS061
01983        AND  X-BATCH-U = ZEROS                                     ECS061
01984        AND  X-BATCH-X = ZEROS                                     ECS061
01985        AND  X-BATCH-Y = ZEROS                                     ECS061
01986        AND  X-BATCH-Z = ZEROS                                     ECS061
01987        AND  X-BATCH-F = ZEROS                                     ECS061
01988        AND  X-BATCH-A-B = ZEROS                                   ECS061
01989        AND  X-BATCH-B-B = ZEROS                                   ECS061
01990        AND  X-BATCH-R-B = ZEROS                                   ECS061
01991        AND  X-BATCH-D-B = ZEROS                                   ECS061
01992        AND  X-BATCH-C-B = ZEROS                                   ECS061
01993        AND  X-BATCH-S-B = ZEROS                                   ECS061
01994        AND  X-BATCH-T-B = ZEROS                                   ECS061
01995        AND  X-BATCH-U-B = ZEROS                                   ECS061
01996        AND  X-BATCH-X-B = ZEROS                                   ECS061
01997        AND  X-BATCH-Y-B = ZEROS                                   ECS061
01998        AND  X-BATCH-Z-B = ZEROS                                   ECS061
01999        AND  X-BATCH-F-B = ZEROS                                   ECS061
02000          GO TO 6790-PRINT-TOTAL-TRANS.                            ECS061
02001                                                                   ECS061
02002      MOVE SPACE-2                TO  P-CCSW.                      ECS061
02003      MOVE 'DIFFERENCE'           TO  PT-MSG1.                     ECS061
02004                                                                   ECS061
02005      COMPUTE WORK-AMT  =  X-BATCH-R  +  X-BATCH-D  +  X-BATCH-C   ECS061
02006                        +  X-BATCH-A  -  X-BATCH-B                 ECS061
02007                        +  X-BATCH-S  +  X-BATCH-T  +  X-BATCH-U   ECS061
02008                        +  X-BATCH-X  +  X-BATCH-Y  +  X-BATCH-Z   ECS061
02009                        +  X-BATCH-F                               ECS061
02010                        +  X-BATCH-A-B  -  X-BATCH-B-B             ECS061
02011                        +  X-BATCH-R-B  +  X-BATCH-D-B             ECS061
02012                        +  X-BATCH-C-B  +  X-BATCH-S-B             ECS061
02013                        +  X-BATCH-T-B  +  X-BATCH-U-B             ECS061
02014                        +  X-BATCH-X-B  +  X-BATCH-Y-B             ECS061
02015                        +  X-BATCH-Z-B  +  X-BATCH-F-B.            ECS061
02016                                                                   ECS061
02017      MOVE WORK-AMT               TO  PT-AMT.                      ECS061
02018      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
02019                                                                   ECS061
02020      IF DTE-CLIENT = 'DMD'                                        ECS061
02021          MOVE 'A - COMM ADJ - POSITIVE'  TO  PT-MSG2              ECS061
02022          COMPUTE WORK-AMT  =  X-BATCH-A  +  X-BATCH-A-B           ECS061
02023          MOVE WORK-AMT                   TO  PT-AMT               ECS061
02024          PERFORM 8900-PRT-RTN  THRU  8999-EXIT                    ECS061
02025          MOVE 'B - COMM ADJ - NEGATIVE'  TO  PT-MSG2              ECS061
02026          COMPUTE WORK-AMT  =  X-BATCH-B  +  X-BATCH-B-B           ECS061
02027          MOVE WORK-AMT                   TO  PT-AMT               ECS061
02028          PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                   ECS061
02029                                                                   ECS061
02030      MOVE 'R - REMITTANCE RECEIVED'  TO  PT-MSG2.                 ECS061
02031      COMPUTE WORK-AMT  =  X-BATCH-R  +  X-BATCH-R-B.              ECS061
02032      MOVE WORK-AMT               TO  PT-AMT.                      ECS061
02033      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
02034                                                                   ECS061
02035      MOVE 'S - REMITTANCE ADJUSTED'  TO  PT-MSG2.                 ECS061
02036      COMPUTE WORK-AMT  =  X-BATCH-S  +  X-BATCH-S-B.              ECS061
02037      MOVE WORK-AMT               TO  PT-AMT.                      ECS061
02038      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
02039                                                                   ECS061
02040      MOVE 'D - DEPOSITS           '  TO  PT-MSG2.                 ECS061
02041      COMPUTE WORK-AMT  =  X-BATCH-D  +  X-BATCH-D-B.              ECS061
02042      MOVE WORK-AMT               TO  PT-AMT.                      ECS061
02043      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
02044                                                                   ECS061
02045      MOVE 'T - DEPOSITS ADJUSTED  '  TO  PT-MSG2.                 ECS061
02046      COMPUTE WORK-AMT  =  X-BATCH-T  +  X-BATCH-T-B.              ECS061
02047      MOVE WORK-AMT               TO  PT-AMT.                      ECS061
02048      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
02049                                                                   ECS061
02050      MOVE 'C - CHARGE TO AGENT'  TO  PT-MSG2.                     ECS061
02051      COMPUTE WORK-AMT  =  X-BATCH-C  +  X-BATCH-C-B.              ECS061
02052      MOVE WORK-AMT               TO  PT-AMT.                      ECS061
02053      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
02054                                                                   ECS061
02055      MOVE 'U - CHARGES ADJUSTED'  TO  PT-MSG2.                    ECS061
02056      COMPUTE WORK-AMT  =  X-BATCH-U  +  X-BATCH-U-B.              ECS061
02057      MOVE WORK-AMT               TO  PT-AMT.                      ECS061
02058      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
02059                                                                   ECS061
02060      MOVE 'Z - ADJUST NET DUE '  TO  PT-MSG2.                     ECS061
02061      COMPUTE WORK-AMT  =  X-BATCH-F  +  X-BATCH-F-B.              ECS061
02062      MOVE WORK-AMT               TO  PT-AMT.                      ECS061
02063      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
02064                                                                   ECS061
02065      MOVE 'X - ADD TO YTD COMP  '  TO  PT-MSG2.                   ECS061
02066      COMPUTE WORK-AMT  =  X-BATCH-X  +  X-BATCH-X-B.              ECS061
02067      MOVE WORK-AMT               TO  PT-AMT.                      ECS061
02068      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
02069                                                                   ECS061
02070      MOVE 'Y - SUBT FROM YTD COMP '  TO  PT-MSG2.                 ECS061
02071      COMPUTE WORK-AMT  =  X-BATCH-Y  +  X-BATCH-Y-B.              ECS061
02072      MOVE WORK-AMT               TO  PT-AMT.                      ECS061
02073      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
02074                                                                   ECS061
02075      MOVE 'F - FICA AMOUNT ENTERED'  TO  PT-MSG2.                 ECS061
02076      COMPUTE WORK-AMT  =  X-BATCH-F  +  X-BATCH-F-B.              ECS061
02077      MOVE WORK-AMT               TO  PT-AMT.                      ECS061
02078      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
02079                                                                   ECS061
02080  6790-PRINT-TOTAL-TRANS.                                          ECS061
02081      MOVE 'STANDARD COMPENSATION'  TO  PT-MSG1.                   ECS061
02082      COMPUTE WORK-AMT  =  X-TRAN-C  +  X-TRAN-O.                  ECS061
02083      MOVE WORK-AMT               TO  PT-AMT.                      ECS061
02084      MOVE SPACE-2                TO  P-CCSW.                      ECS061
02085      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
02086                                                                   ECS061
02087      MOVE 'PRODUCERS'            TO  PT-MSG2.                     ECS061
02088      MOVE X-TRAN-C               TO  PT-AMT.                      ECS061
02089      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
02090                                                                   ECS061
02091      MOVE 'OVERWRITE'            TO  PT-MSG2.                     ECS061
02092      MOVE X-TRAN-O               TO  PT-AMT.                      ECS061
02093      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
02094                                                                   ECS061
02091      MOVE 'MISC FEES'            TO PT-MSG2
02092      MOVE X-TRAN-FEE             TO PT-AMT
02093      PERFORM 8900-PRT-RTN        THRU 8999-EXIT
02094                                                                   ECS061
02095      MOVE 'PREMIUM'              TO  PT-MSG2.                     ECS061
02096      MOVE X-TRAN-P               TO  PT-AMT.                      ECS061
02097      MOVE SPACE-2                TO  P-CCSW.                      ECS061
02098      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
02099                                                                   ECS061
02100      MOVE 'RECALCULATED COMPENSATION'  TO  PT-MSG1.               ECS061
02101      COMPUTE WORK-AMT  =  X-RECALC-C  +  X-RECALC-O.              ECS061
02102      MOVE WORK-AMT               TO  PT-AMT.                      ECS061
02103      MOVE SPACE-2                TO  P-CCSW.                      ECS061
02104      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
02105                                                                   ECS061
02106      MOVE 'PRODUCERS'            TO  PT-MSG2.                     ECS061
02107      MOVE X-RECALC-C             TO  PT-AMT.                      ECS061
02108      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
02109                                                                   ECS061
02110      MOVE 'OVERWRITE'            TO  PT-MSG2.                     ECS061
02111      MOVE X-RECALC-O             TO  PT-AMT.                      ECS061
02112      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
02113                                                                   ECS061
02114      MOVE 'PREMIUM'              TO  PT-MSG2.                     ECS061
02115      MOVE X-RECALC-P             TO  PT-AMT.                      ECS061
02116      MOVE SPACE-2                TO  P-CCSW.                      ECS061
02117      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
02118                                                                   ECS061
02119      MOVE 'BILLED COMPENSATION'  TO  PT-MSG1.                     ECS061
02120      COMPUTE WORK-AMT  =  X-TRAN-C-B  +  X-TRAN-O-B.              ECS061
02121      MOVE WORK-AMT               TO  PT-AMT.                      ECS061
02122      MOVE SPACE-2                TO  P-CCSW.                      ECS061
02123      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
02124                                                                   ECS061
02125      MOVE 'PRODUCERS'            TO  PT-MSG2.                     ECS061
02126      MOVE X-TRAN-C-B             TO  PT-AMT.                      ECS061
02127      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
02128                                                                   ECS061
02129      MOVE 'OVERWRITE'            TO  PT-MSG2.                     ECS061
02130      MOVE X-TRAN-O-B             TO  PT-AMT.                      ECS061
02131      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
02132                                                                   ECS061
02133      MOVE 'PREMIUM'              TO  PT-MSG2.                     ECS061
02134      MOVE X-TRAN-P-B             TO  PT-AMT.                      ECS061
02135      MOVE SPACE-2                TO  P-CCSW.                      ECS061
02136      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
02137                                                                   ECS061
02138      MOVE 'TOTAL COMPENSATION'   TO  PT-MSG1.                     ECS061
02139      COMPUTE WORK-AMT  =  X-TRAN-N  +  X-TRAN-N-B.                ECS061
02140      MOVE WORK-AMT               TO  PT-CTR.                      ECS061
02141      COMPUTE WORK-AMT  =  X-RECALC-C  +  X-RECALC-O               ECS061
02142                        +  X-TRAN-C    +  X-TRAN-O                 ECS061
02143                        +  X-TRAN-C-B  +  X-TRAN-O-B.              ECS061
02144      MOVE WORK-AMT               TO  PT-AMT.                      ECS061
02145      MOVE SPACE-2                TO  P-CCSW.                      ECS061
02146      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
02147                                                                   ECS061
02148      MOVE 'PRODUCERS'            TO  PT-MSG2.                     ECS061
02149      COMPUTE WORK-AMT  =  X-RECALC-C  +  X-TRAN-C  +  X-TRAN-C-B. ECS061
02150      MOVE WORK-AMT               TO  PT-AMT.                      ECS061
02151      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
02152                                                                   ECS061
02153      MOVE 'OVERWRITE'            TO  PT-MSG2.                     ECS061
02154      COMPUTE WORK-AMT  =  X-RECALC-O  +  X-TRAN-O  +  X-TRAN-O-B. ECS061
02155      MOVE WORK-AMT               TO  PT-AMT.                      ECS061
02156      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061

           MOVE 'MISC FEES'            TO  PT-MSG2.                     ECS061
           MOVE X-TRAN-FEE             TO WORK-AMT
           MOVE WORK-AMT               TO  PT-AMT.                      ECS061
           PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
02157                                                                   ECS061
02158      MOVE 'PREMIUM'              TO  PT-MSG2.                     ECS061
02159      COMPUTE WORK-AMT  =  X-RECALC-P  +  X-TRAN-P  +  X-TRAN-P-B. ECS061
02160      MOVE WORK-AMT               TO  PT-AMT.                      ECS061
02161      MOVE SPACE-2                TO  P-CCSW.                      ECS061
02162      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
02163                                                                   ECS061
02164      MOVE 'TOTAL CLAIMS '        TO  PT-MSG1.                     ECS061
02165      MOVE X-TRAN-T               TO  PT-AMT.                      ECS061
02166      MOVE SPACE-2                TO  P-CCSW.                      ECS061
02167      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
02168                                                                   ECS061
02169  6799-EXIT.                                                       ECS061
02170      EXIT.                                                        ECS061
02171  EJECT                                                            ECS061
02172  6800-WRITE-PYADJ.                                                ECS061
02173      IF DTE-CLIENT = 'DMD'                                        ECS061
02174        IF A-TYPE = 'A' OR 'B'                                     ECS061
02175            GO TO 6899-EXIT.                                       ECS061
02176                                                                   ECS061
02177      WRITE PYADJ-REC  FROM  A-CARD.                               ECS061
02178                                                                   ECS061
02179  6899-EXIT.                                                       ECS061
02180      EXIT.                                                        ECS061
02181                                                                   ECS061
02182  7000-READ-PYMT-RTN.                                              ECS061
02183      READ PYMT-FILE  INTO  A-CARD                                 ECS061
02184          AT END                                                   ECS061
02185              MOVE HIGH-VALUES    TO  A-CARD.                      ECS061
02186                                                                   ECS061
02187      MOVE A-SEQ                  TO  AC-CONTROL.                  ECS061
02188      MOVE DTE-CLASIC-COMPANY-CD  TO  AC-COMPANY-CD.               ECS061
02189      MOVE 'A'                    TO  AC-TYPE.                     ECS061
02190                                                                   ECS061
02191      IF AC-ACCOUNT = LOW-VALUES                                   ECS061
02192         MOVE 'G'                 TO AC-TYPE
060205        IF A-CO-TYPE = 'B' OR 'G'
060205           MOVE A-CO-TYPE        TO AC-TYPE
060205        END-IF
           END-IF
02193                                                                   ECS061
02194      IF AC-ACCOUNT = HIGH-VALUES                                  ECS061
02195          MOVE HIGH-VALUES        TO  AC-COMPANY-CD                ECS061
02196                                      AC-TYPE.                     ECS061
02197                                                                   ECS061
02198      MOVE SPACE                  TO  ERROR-SW.                    ECS061
02199                                                                   ECS061
02200  7199-EXIT.                                                       ECS061
02201      EXIT.                                                        ECS061
02202                                                                   ECS061
02203  7200-RETURN-COMM-RTN.                                            ECS061
02204      RETURN SORT-COMM  INTO  WORK-REC                             ECS061
02205          AT END                                                   ECS061
02206              MOVE HIGH-VALUES    TO  WORK-REC.                    ECS061
02207                                                                   ECS061
02208      MOVE DTE-CLASIC-COMPANY-CD  TO  WC-COMPANY-CD.               ECS061
02209      MOVE W-CARRIER              TO  WC-CARRIER.                  ECS061
02210      MOVE W-GROUPING             TO  WC-GROUPING.                 ECS061
02211      MOVE W-RESP-NO              TO  WC-RESP-NO.                  ECS061
02212      MOVE W-ACCOUNT              TO  WC-ACCOUNT.                  ECS061
02213      MOVE 'A'                    TO  WC-TYPE.                     ECS061
02214                                                                   ECS061
02215      IF WC-ACCOUNT = LOW-VALUES                                   ECS061
02216         MOVE 'G'                 TO WC-TYPE
010716        IF DTE-CLIENT = 'DCC' or 'VPP'
060205           IF W-COM-TYPE = 'B'
060205              MOVE 'B'           TO WC-TYPE
060205           END-IF
060205        END-IF
060205     END-IF
02217                                                                   ECS061
02218      IF WC-ACCOUNT = HIGH-VALUES                                  ECS061
02219          MOVE HIGH-VALUES        TO  WC-COMPANY-CD                ECS061
02220                                      WC-TYPE.                     ECS061
02221                                                                   ECS061
02222  7299-EXIT.                                                       ECS061
02223      EXIT.                                                        ECS061
02224                                                                   ECS061
02225  7300-READ-MSTR-RTN.                                              ECS061
02226                                                                   ECS061
02227      IF COI-ID = HIGH-VALUE                                       ECS061
02228          GO TO 7399-EXIT.                                         ECS061
02229                                                                   ECS061
02230      READ COMM-MSTR-IN                                            ECS061
02231          AT END                                                   ECS061
02232              MOVE HIGH-VALUE     TO  COMP-IN-RECORD               ECS061
02233                                      COMPENSATION-MASTER          ECS061
02234              GO TO 7399-EXIT.                                     ECS061
02235                                                                   ECS061
02236      MOVE 'CO'                   TO  COI-ID.                      ECS061
02237                                                                   ECS061
02238      IF COI-AUTO-GENERATED-THIS-RUN                               ECS061
02239          MOVE 'Y'                TO  COI-INTERNAL-CONTROL-1.      ECS061
02240                                                                   ECS061
02241      MOVE 'N'                    TO  COI-INTERNAL-CONTROL-2.      ECS061
02242      MOVE COMP-IN-RECORD         TO  COMPENSATION-MASTER.         ECS061
02243      ADD +1                      TO  CTL-MSTR-RECS-IN.            ECS061
02244                                                                   ECS061
02245      IF CO-COMPANY-TYPE                                           ECS061
02246          IF CO-GROUPING EQUAL SPACES OR LOW-VALUES                ECS061
02247              ADD +1              TO  CARR-CNT                     ECS061
02248              MOVE CO-CARRIER     TO  C-CARR (CARR-CNT)            ECS061
02249              MOVE CO-ACCT-NAME   TO  C-NAME  (CARR-CNT)           ECS061
02250          ELSE                                                     ECS061
02251              ADD +1              TO  CARR-GROUP-CNT               ECS061
02252              MOVE CO-CARRIER     TO  C-G-CARR (CARR-GROUP-CNT)    ECS061
02253              MOVE CO-GROUPING    TO  C-G-GROUP(CARR-GROUP-CNT)    ECS061
02254              MOVE CO-ACCT-NAME   TO  C-G-NAME (CARR-GROUP-CNT).   ECS061
02255                                                                   ECS061
02256      IF CO-LAST-STMT-DT NOT NUMERIC                               ECS061
02257          MOVE ZEROS              TO  CO-LAST-STMT-DT.             ECS061
02258                                                                   ECS061
02259      IF CO-LF-CLM-AMT NOT NUMERIC                                 ECS061
02260          MOVE +0                 TO  CO-LF-CLM-AMT.               ECS061
02261                                                                   ECS061
02262      IF CO-AH-CLM-AMT NOT NUMERIC                                 ECS061
02263          MOVE +0                 TO  CO-AH-CLM-AMT.               ECS061
02264                                                                   ECS061
02265      IF CO-CUR-FICA NOT NUMERIC                                   ECS061
02266          MOVE +0                 TO  CO-CUR-FICA.                 ECS061
02267                                                                   ECS061
02268      IF CO-YTD-FICA NOT NUMERIC                                   ECS061
02269          MOVE +0                 TO  CO-YTD-FICA.                 ECS061
02270                                                                   ECS061
02271      IF CO-CUR-OVR-UNDR NOT NUMERIC                               ECS061
02272          MOVE +0                 TO  CO-CUR-OVR-UNDR.             ECS061
02273                                                                   ECS061
02274      IF CO-YTD-OVR-UNDR NOT NUMERIC                               ECS061
02275          MOVE +0                 TO  CO-YTD-OVR-UNDR.             ECS061
02276                                                                   ECS061
02277      IF CO-YTD-PAID-COM NOT NUMERIC                               ECS061
02278          MOVE +0                 TO  CO-YTD-PAID-COM.             ECS061
02279                                                                   ECS061
02280      IF CO-YTD-PAID-OV  NOT NUMERIC                               ECS061
02281          MOVE +0                 TO  CO-YTD-PAID-OV.              ECS061
02282                                                                   ECS061
02283      IF CO-AUTO-GENERATED                                         ECS061
02284          IF CO-YTD-OV  = +0  AND                                  ECS061
02285             CO-YTD-COM = +0  AND                                  ECS061
02286             CO-YTD-PAID-COM = +0  AND                             ECS061
02287             CO-YTD-PAID-OV  = +0  AND                             ECS061
02288             CO-END-BAL = +0  AND                                  ECS061
02289             CO-YTD-OVR-UNDR = +0                                  ECS061
02290              ADD +1              TO  CTL-MSTR-RECS-DEL            ECS061
02291              GO TO 7300-READ-MSTR-RTN.                            ECS061
02292                                                                   ECS061
02293      PERFORM 7700-CHECK-RUN-MONTH  THRU  7799-EXIT.               ECS061
02294                                                                   ECS061
02295      ADD CO-YTD-COM              TO  CTL-YTD-IN.                  ECS061
02296      ADD CO-YTD-OV               TO  CTL-YTD-IN.                  ECS061
02297                                                                   ECS061
02298  7399-EXIT.                                                       ECS061
02299      EXIT.                                                        ECS061
02300                                                                   ECS061
02301  7400-WRT-MSTR-RTN.                                               ECS061
02302      IF NOT VALID-CO-ID                                           ECS061
02303          GO TO 7499-EXIT.                                         ECS061
02304                                                                   ECS061
02305      IF CO-CONTROL-PRIMARY NOT GREATER LAST-CONTROL               ECS061
02306          DISPLAY '*****************************************'      ECS061
02307          DISPLAY 'COMP RECORD KEY = '  CO-CONTROL-PRIMARY         ECS061
02308          DISPLAY '*****************************************'      ECS061
CIDMOD         DISPLAY 'SYS015 SEQUENCE ERROR'                          ECS061
CIDMOD         DISPLAY '0615  -  WS-RETURN-CODE'                        ECS061
CIDMOD         GO TO 7499-EXIT.                                         ECS061
CIDMOD*        MOVE '0615'             TO  WS-RETURN-CODE               ECS061
CIDMOD*        MOVE 'SYS015 SEQUENCE ERROR'                             ECS061
CIDMOD*                                TO  WS-ABEND-MESSAGE             ECS061
CIDMOD*        GO TO ABEND-PGM.                                         ECS061
02313                                                                   ECS061
02314      MOVE CO-CONTROL-PRIMARY     TO  LAST-CONTROL.                ECS061
02315                                                                   ECS061
02316      ADD +1                      TO  CTL-MSTR-RECS-OUT.           ECS061
02317                                                                   ECS061
02318      IF CO-COMPANY-TYPE                                           ECS061
02319          GO TO 7450-WRT-MSTR.                                     ECS061
02320                                                                   ECS061
02321      ADD CO-YTD-COM              TO  CTL-YTD-OUT.                 ECS061
02322      ADD CO-YTD-OV               TO  CTL-YTD-OUT.                 ECS061
02323                                                                   ECS061
02324      IF CO-LAST-STMT-DT NOT NUMERIC                               ECS061
02325          MOVE ZEROS              TO  CO-LAST-STMT-DT.             ECS061
02326                                                                   ECS061
02327      IF CO-LAST-STMT-YEAR = RUN-YR                                ECS061
02328        AND  CO-LAST-STMT-MONTH = RUN-MO                           ECS061
02329          GO TO 7420-BYPASS-AGING.                                 ECS061
02330                                                                   ECS061
080612     if dte-client = 'AHL'
080612        add co-ov90              to co-ov120
080612        move co-ov60             to co-ov90
080612     else
02331         ADD CO-OV60              TO  CO-OV90
081612     end-if
02332                                                                   ECS061
02333      MOVE CO-OV30                TO  CO-OV60.                     ECS061
02334      MOVE CO-CUR                 TO  CO-OV30.                     ECS061
02335                                                                   ECS061
02336  7420-BYPASS-AGING.                                               ECS061
02337      MOVE CO-END-BAL             TO  CO-BAL-FWD.                  ECS061
02338      MOVE +0                     TO  CO-CUR                       ECS061
02339                                      CO-CUR-COM                   ECS061
02340                                      CO-CUR-CHG                   ECS061
02341                                      CO-CUR-PMT                   ECS061
02342                                      CO-CUR-OVR-UNDR              ECS061
02343                                      CO-END-BAL                   ECS061
02344                                      CO-LF-CLM-AMT                ECS061
02345                                      CO-AH-CLM-AMT.               ECS061
02346                                                                   ECS061
020210     MOVE SPACES                 TO CO-STMT-TYPE

02347      IF CO-BAL-FWD NOT GREATER +0                                 ECS061
02348          MOVE +0                 TO  CO-OV30  CO-OV60             ECS061
080612                                     CO-OV90  co-ov120
080612     end-if
02350                                                                   ECS061
02351      IF DTE-CLIENT = 'OHL'  OR  'MAA'                             ECS061
02352          MOVE +0                 TO  CO-BAL-FWD.                  ECS061
02353                                                                   ECS061
02354  7450-WRT-MSTR.                                                   ECS061

020411     PERFORM VARYING N1 FROM +30 BY -1 UNTIL
020411        (N1 < +5)
020411        OR (CO-ACCT-NAME (N1:1) = '*')
020411     END-PERFORM
020411     IF N1 > +4
020411        MOVE CO-ACCT-NAME        TO WS-OLD-NAME
020411        MOVE SPACES TO CO-ACCT-NAME (N1:30 - N1 + 1)
020411        DISPLAY 'NAME BEFORE ' WS-OLD-NAME ' AFTER ' CO-ACCT-NAME
020411     END-IF

02355      MOVE COMPENSATION-MASTER    TO  COMP-OUT-RECORD.             ECS061
02356                                                                   ECS061
02357      WRITE COMP-OUT-RECORD.                                       ECS061
02358                                                                   ECS061
02359  7499-EXIT.                                                       ECS061
02360      EXIT.                                                        ECS061
02361                                                                   ECS061
02362  7500-WRT-COMM-PREM-RTN.                                          ECS061
02363                                                                   ECS061
02364      WRITE COMM-PREM-RECORD  FROM  CP-RECORD.                     ECS061
02365                                                                   ECS061
02366  7599-EXIT.                                                       ECS061
02367      EXIT.                                                        ECS061
02368                                                                   ECS061
02369  7600-FINAL-RTN.                                                  ECS061
02370      PERFORM 8800-HD-RTN  THRU  8899-EXIT.                        ECS061
02371                                                                   ECS061
02372      MOVE SPACE-2                           TO  P-CCSW.           ECS061
02373      MOVE 'COMPENSATION MASTER RECORDS IN'  TO  PT-MSG.           ECS061
02374      MOVE CTL-MSTR-RECS-IN                  TO  PT-CTR.           ECS061
02375      MOVE CTL-YTD-IN                        TO  PT-AMT.           ECS061
02376                                                                   ECS061
02377      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
02378                                                                   ECS061
02379  7620-FINAL-ADJ.                                                  ECS061
02380      IF CTL-YTD-ADJ = +0                                          ECS061
02381          GO TO 7630-FINAL-ADD.                                    ECS061
02382                                                                   ECS061
02383      MOVE ' X OR Y - ADJUSTMENTS'  TO  PT-MSG.                    ECS061
02384      MOVE CTL-YTD-ADJ              TO  PT-AMT.                    ECS061
02385                                                                   ECS061
02386      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
02387                                                                   ECS061
02388  7630-FINAL-ADD.                                                  ECS061
02389      IF CTL-MSTR-RECS-ADD = +0                                    ECS061
02390          GO TO 7640-FINAL-DEL.                                    ECS061
02391                                                                   ECS061
02392      MOVE ' COMPUTER GENERATED RECORDS ADDED'  TO  PT-MSG.        ECS061
02393      MOVE CTL-MSTR-RECS-ADD                    TO  PT-CTR.        ECS061
02394                                                                   ECS061
02395      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
02396                                                                   ECS061
02397  7640-FINAL-DEL.                                                  ECS061
02398      IF CTL-MSTR-RECS-DEL = +0                                    ECS061
02399          GO TO 7650-FINAL-OUT.                                    ECS061
02400                                                                   ECS061
02401      MOVE ' COMPUTER GENERATED RECORDS DELETED'  TO  PT-MSG.      ECS061
02402      MOVE CTL-MSTR-RECS-DEL                      TO  PT-CTR.      ECS061
02403                                                                   ECS061
02404      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
02405                                                                   ECS061
02406  7650-FINAL-OUT.                                                  ECS061
02407      MOVE 'COMPENSATION MASTER RECORDS OUT'  TO  PT-MSG.          ECS061
02408      MOVE CTL-MSTR-RECS-OUT                  TO  PT-CTR.          ECS061
02409      MOVE CTL-YTD-OUT                        TO  PT-AMT.          ECS061
02410                                                                   ECS061
02411      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
02412                                                                   ECS061
02413  7699-EXIT.                                                       ECS061
02414      EXIT.                                                        ECS061
02415  EJECT                                                            ECS061
02416  7700-CHECK-RUN-MONTH.                                            ECS061
02417      IF RUN-MO NOT = 01                                           ECS061
02418          GO TO 7799-EXIT.                                         ECS061
02419                                                                   ECS061
02420      MOVE +0                     TO  CO-YTD-COM                   ECS061
02421                                      CO-YTD-OV                    ECS061
02422                                      CO-YTD-PAID-COM              ECS061
02423                                      CO-YTD-PAID-OV               ECS061
02424                                      CO-YTD-FICA                  ECS061
02425                                      CO-YTD-OVR-UNDR.             ECS061
02426                                                                   ECS061
02427                                                                   ECS061
02428      IF CO-LAST-STMT-DT NOT NUMERIC                               ECS061
02429          MOVE ZEROS              TO  CO-LAST-STMT-DT.             ECS061
02430                                                                   ECS061
02431      IF CO-LAST-STMT-YEAR  = RUN-YR  AND                          ECS061
02432         CO-LAST-STMT-MONTH = RUN-MO                               ECS061
02433          NEXT SENTENCE                                            ECS061
02434      ELSE                                                         ECS061
02435          GO TO 7799-EXIT.                                         ECS061
02436                                                                   ECS061
02437      IF CO-ACCOUNT-TYPE                                           ECS061
02438          MOVE CO-CURRENT-CUR-COM                                  ECS061
02439                                  TO  CO-YTD-COM.                  ECS061
02440                                                                   ECS061
02441      IF CO-GEN-AGENT-TYPE                                         ECS061
02442          MOVE CO-CURRENT-CUR-COM                                  ECS061
02443                                  TO  CO-YTD-OV.                   ECS061
02444                                                                   ECS061
02445  7799-EXIT.                                                       ECS061
02446      EXIT.                                                        ECS061
02447                                                                   ECS061
02448  7900-END-MERGE.                                                  ECS061
02449      CLOSE COMM-MSTR-IN  COMM-MSTR-OUT  NEW-A-C                   ECS061
02450            PYMT-FILE     PYADJ-FILE.                              ECS061
02451                                                                   ECS061
02452  7999-EXIT.                                                       ECS061
02453      EXIT.                                                        ECS061
02454  EJECT                                                            ECS061
02455  8500-DATE-CONVERSION.                                            ECS061
02456                              COPY ELCDCS.                         ECS061
02457                                                                   ECS061
02458  8599-EXIT.                                                       ECS061
02459      EXIT.                                                        ECS061
02460                                                                   ECS061
02461      EJECT                                                        ECS061
02462  8600-CARR-GROUP-NAME-RTN.                                        ECS061
02463      MOVE +1                     TO NAME-IDX.                     ECS061
02464                                                                   ECS061
02465  8610-CARR-GROUP-LOOP.                                            ECS061
02466      IF NAME-IDX GREATER CARR-GROUP-CNT                           ECS061
02467          PERFORM 8700-CARR-NAME-RTN THRU 8799-EXIT                ECS061
02468              GO TO 8699-EXIT.                                     ECS061
02469                                                                   ECS061
02470      IF C-G-CARR (NAME-IDX) = SEARCH-CARR AND                     ECS061
02471         C-G-GROUP(NAME-IDX) = SEARCH-GROUP                        ECS061
02472             MOVE C-G-NAME (NAME-IDX)                              ECS061
02473                                  TO VALID-COMPANY-NAME            ECS061
02474      ELSE                                                         ECS061
02475          ADD +1                  TO NAME-IDX                      ECS061
02476              GO TO 8610-CARR-GROUP-LOOP.                          ECS061
02477                                                                   ECS061
02478  8699-EXIT.                                                       ECS061
02479       EXIT.                                                       ECS061
02480                                                                   ECS061
02481  8700-CARR-NAME-RTN.                                              ECS061
02482      MOVE +1                     TO NAME-IDX.                     ECS061
02483                                                                   ECS061
02484  8710-CARR-LOOP.                                                  ECS061
02485      IF NAME-IDX GREATER CARR-CNT                                 ECS061
02486          MOVE COMPANY-NAME       TO VALID-COMPANY-NAME            ECS061
02487              GO TO 8799-EXIT.                                     ECS061
02488                                                                   ECS061
02489      IF C-CARR (NAME-IDX) = SEARCH-CARR                           ECS061
02490          MOVE C-NAME (NAME-IDX)  TO VALID-COMPANY-NAME            ECS061
02491      ELSE                                                         ECS061
02492          ADD +1                  TO NAME-IDX                      ECS061
02493          GO TO 8710-CARR-LOOP.                                    ECS061
02494                                                                   ECS061
02495  8799-EXIT.                                                       ECS061
02496       EXIT.                                                       ECS061
02497  EJECT                                                            ECS061
02498  8800-HD-RTN.                                                     ECS061
02499      ADD +1                      TO  PGCTR.                       ECS061
02500      MOVE PGCTR                  TO  HD-PG.                       ECS061
02501      MOVE SPACE-NP               TO  P-CCSW.                      ECS061
02502      MOVE HD1                    TO  P-LINE.                      ECS061
02503      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
02504                                                                   ECS061
02505      MOVE HD2                    TO  P-LINE.                      ECS061
02506      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
02507                                                                   ECS061
02508      MOVE HD3                    TO  P-LINE.                      ECS061
02509      PERFORM 8900-PRT-RTN  THRU  8999-EXIT.                       ECS061
02510                                                                   ECS061
02511      IF BATCH-BREAK                                               ECS061
02512          MOVE HD3A               TO  P-LINE                       ECS061
02513          MOVE SPACE-2            TO  P-CCSW                       ECS061
02514          PERFORM 8900-PRT-RTN  THRU  8999-EXIT                    ECS061
02515          MOVE HD4A               TO  P-LINE                       ECS061
02516          MOVE SPACE-1            TO  P-CCSW                       ECS061
02517          PERFORM 8900-PRT-RTN  THRU  8999-EXIT                    ECS061
02518          MOVE SPACE-2            TO  P-CCSW                       ECS061
02519      ELSE                                                         ECS061
02520          MOVE HD4                TO  P-LINE                       ECS061
02521          MOVE SPACE-2            TO  P-CCSW                       ECS061
02522          PERFORM 8900-PRT-RTN  THRU  8999-EXIT                    ECS061
02523          MOVE HD5                TO  P-LINE                       ECS061
02524          PERFORM 8900-PRT-RTN  THRU  8999-EXIT                    ECS061
02525          MOVE SPACE-3            TO  P-CCSW.                      ECS061
02526                                                                   ECS061
02527  8899-EXIT.                                                       ECS061
02528      EXIT.                                                        ECS061
02529                                                                   ECS061
02530  8900-PRT-RTN.                                                    ECS061
02531      MOVE P-REC                  TO  PRT.                         ECS061
02532      MOVE P-CCSW                 TO  X.                           ECS061
02533                                                                   ECS061
02534      IF P-CCSW = SPACE-1                                          ECS061
02535          ADD +1                  TO  LNCTR                        ECS061
02536      ELSE                                                         ECS061
02537          IF P-CCSW = SPACE-2                                      ECS061
02538              ADD +2              TO  LNCTR                        ECS061
02539          ELSE                                                     ECS061
02540              IF P-CCSW = SPACE-3                                  ECS061
02541                  ADD +3          TO  LNCTR                        ECS061
02542              ELSE                                                 ECS061
02543                  IF P-CCSW = SPACE-NP                             ECS061
02544                      MOVE +0     TO  LNCTR.                       ECS061
02545                                                                   ECS061
02546      MOVE SPACE-1                TO  P-REC.                       ECS061
02547                                                                   ECS061
02548  8920-COPY-PRT.                                                   ECS061
02549                              COPY ELCPRT2.                        ECS061
02550                                                                   ECS061
02551  8999-EXIT.                                                       ECS061
02552      EXIT.                                                        ECS061
02553  EJECT                                                            ECS061
02554  9000-END-OF-JOB SECTION.                                         ECS061
02555                                                                   ECS061
02556  9200-E-O-J.                                                      ECS061
02557      CLOSE PRNTR.                                                 ECS061
02558                                                                   ECS061
02559  9500-CLOSE-FICH.                                                 ECS061
02560                              COPY ELCPRTC.                        ECS061
02561                                                                   ECS061
02562  9999-STOP-RUN.                                                   ECS061

070714     OPEN I-O ERMEBL.                                             ECS061
070714                                                                  ECS061
070714     IF ERMEBL-FILE-STATUS NOT = ZERO and '97'
070714        MOVE 'N'                 TO ME-UPDATE-FLAG
070714     end-if
070714     MOVE DTE-CLIENT             TO ME-COMPANY
070714                                                                  ECS061
070714     COMPUTE MONTH-END-MOYR  =  RUN-CCYY  *  12  +  RUN-MO.       ECS061
070714                                                                  ECS061
070714     MOVE MONTH-END-MOYR         TO  ME-MOYR.                     ECS061
070714                                                                  ECS061
070714     IF ME-DO-UPDATE                                              ECS061
070714        READ ERMEBL INVALID KEY
070714           MOVE 'N'              TO ME-UPDATE-FLAG
070714           CLOSE ERMEBL
070714     end-if
070714                                                                  ECS061
070714     IF ME-DO-UPDATE                                              ECS061
070714        move hld-061-PREM        to me-061-PREM       
070714        move hld-061-COMM        to me-061-COMM       
070714        move hld-061-OR          to me-061-OR         
070714        move hld-061-PY-ADJ      to me-061-PY-ADJ     
070714        move hld-061-COMM-RCALC  to me-061-COMM-RCALC 
070714        move hld-061-OR-RCALC    to me-061-OR-RCALC   
070714        move hld-061-PREM-RCALC  to me-061-PREM-RCALC 
070714        move hld-061-CLMS        to me-061-CLMS       
070714        MOVE ME-CNDS-DATE       TO  ME-061-RUN-DT                
070714        ACCEPT WS-TIME-OF-DAY   FROM TIME                     
070714        ADD 1                   TO  ME-061-RUN-CT             
070714        REWRITE MONTH-END-BALANCES                            
070714        CLOSE ERMEBL
070714     end-if
02571                                                                   ECS061
02572      GOBACK.                                                      ECS061
02573                                                                   ECS061
02574  ABEND-PGM SECTION.                                               ECS061
02575                              COPY ELCABEND.                       ECS061
