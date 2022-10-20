00001  IDENTIFICATION DIVISION.                                         03/06/98
00002                                                                   ECS064
00003  PROGRAM-ID.                ECS064.                                  LV002
00004 *              PROGRAM CONVERTED BY                               ECS064
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ECS064
00006 *              CONVERSION DATE 02/08/96 18:14:35.                 ECS064
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          ECS064
00008 *                           VMOD=2.005.                           ECS064
00009                                                                   ECS064
00010 *AUTHOR.        LOGIC, INC.                                       ECS064
00011 *               DALLAS, TEXAS.                                    ECS064
00012                                                                   ECS064
00013 *DATE-COMPILED.                                                   ECS064
00014                                                                   ECS064
00015 *SECURITY.   *****************************************************ECS064
00016 *            *                                                   *ECS064
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ECS064
00018 *            *                                                   *ECS064
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS064
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ECS064
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *ECS064
00022 *            *                                                   *ECS064
00023 *            *****************************************************ECS064
00024                                                                   ECS064
00025 *REMARKS.                                                         ECS064
00026 *        THIS PROGRAM PRINTS THE AGED ACCOUNTS RECEIVABLE.        ECS064
062104******************************************************************
062104*                   C H A N G E   L O G
062104*
062104* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
062104*-----------------------------------------------------------------
062104*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
062104* EFFECTIVE    NUMBER
062104*-----------------------------------------------------------------
062104* 062104    2004050700001  SMVA  ADD NEW FILE TO AUTOMATE ME BALANCING
030612* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
070714* 070714  CR2013060600001  PEMA  AUTOMATE MONTH END BALANCING
062121* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
062104******************************************************************
00027                                                                   ECS064
00028  ENVIRONMENT DIVISION.                                            ECS064
00029  INPUT-OUTPUT SECTION.                                            ECS064
00030  FILE-CONTROL.                                                    ECS064
00031                                                                   ECS064
00032      SELECT PRNTR            ASSIGN TO SYS008-UR-1403-S-SYS008.   ECS064
00033      SELECT COMM-MSTR-IN     ASSIGN TO SYS010-UT-2400-S-SYS010.   ECS064

062104     SELECT ME50-ECS064-BALANCE
062104                             ASSIGN TO SYS012
062104                             ORGANIZATION IS LINE SEQUENTIAL.

00034      SELECT DISK-DATE        ASSIGN TO SYS019-UT-FBA1-S-SYS019.   ECS064
00035      SELECT FICH             ASSIGN TO SYS020-UT-2400-S-SYS020.   ECS064
070714     SELECT  ERMEBL          ASSIGN SYS024-FBA1-ERMEBL      
070714                             ORGANIZATION INDEXED           
070714                             ACCESS DYNAMIC                 
070714                             RECORD KEY ME-CONTROL-PRIMARY  
070714                             FILE STATUS ERMEBL-FILE-STATUS.
00036  EJECT                                                            ECS064
00037  DATA DIVISION.                                                   ECS064
00038  FILE SECTION.                                                    ECS064
00039                                                                   ECS064
00040  FD  PRNTR                                                        ECS064
00041                              COPY ELCPRTFD.                       ECS064
00042  EJECT                                                            ECS064
00043  FD  COMM-MSTR-IN                                                 ECS064
00044                              COPY ECSCOIFD.                       ECS064
00045  EJECT                                                            ECS064

062104 FD  ME50-ECS064-BALANCE
062104     RECORDING MODE IS F
062104     BLOCK CONTAINS 0 RECORDS.
062104 01  ME50-ECS064-BALANCE-REC    PIC X(95).

00046  FD  DISK-DATE                                                    ECS064
00047                              COPY ELCDTEFD.                       ECS064
00048  EJECT                                                            ECS064
00049  FD  FICH                                                         ECS064
00050                              COPY ELCFCHFD.                       ECS064
070714 FD  ERMEBL.
070714                                 COPY ERCMEBL.
00051  EJECT                                                            ECS064
00052  WORKING-STORAGE SECTION.                                         ECS064
00053  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      ECS064
00054  77  FILLER PIC X(32) VALUE '********************************'.   ECS064
00055  77  FILLER PIC X(32) VALUE '*            ECS064            *'.   ECS064
00056  77  FILLER PIC X(32) VALUE '*********** VMOD=2.005 *********'.   ECS064
00057                                                                   ECS064
00058  77  PGM-SUB                 PIC S9(03)      VALUE +64  COMP-3.   ECS064
00059  77  SPACE-NP                PIC  X(01)      VALUE '1'.           ECS064
00060  77  SPACE-1                 PIC  X(01)      VALUE ' '.           ECS064
00061  77  SPACE-2                 PIC  X(01)      VALUE '0'.           ECS064
00062  77  SPACE-3                 PIC  X(01)      VALUE '-'.           ECS064
00063  77  X                       PIC  X(01)      VALUE '1'.           ECS064
00064                                                                   ECS064
00065  01  REQUIRED-STORAGE.                                            ECS064
00066      12  WS-RETURN-CODE          PIC S9(04)             COMP.     ECS064
00067      12  WS-ABEND-MESSAGE        PIC  X(80).                      ECS064
00068      12  WS-ABEND-FILE-STATUS    PIC  X(02)  VALUE ZEROS.         ECS064
00069      12  WS-ZERO                 PIC S9(01)  VALUE ZERO COMP-3.   ECS064
00070  EJECT                                                            ECS064
00071                              COPY ERCCOMP.                        ECS064


062104 01  WS-BAL50-DESCRIPTION          PIC X(50)  VALUE
062104     'ECS064 End Bal should match EL509 Current Bal     '.

062104 01  WS-ME50-BALANCE-REC.
062104     12  WS-ME50-BAL-JOB           PIC X(11)  VALUE SPACES.
062104     12  WS-ME50-BAL-DELIM1        PIC X(01)  VALUE ';'.
062104     12  WS-ME50-BAL-STEP          PIC X(08)  VALUE 'ECS064  '.
062104     12  WS-ME50-BAL-DELIM2        PIC X(01)  VALUE ';'.
062104     12  WS-ME50-BAL-AMT-LOW       PIC ZZZ,ZZZ,ZZ9.
062104     12  WS-ME50-BAL-DELIM3        PIC X(01)  VALUE ';'.
062104     12  WS-ME50-BAL-AMT-HIGH      PIC ZZZ,ZZZ,ZZ9.
062104     12  WS-ME50-BAL-DELIM4        PIC X(01)  VALUE ';'.
062104     12  WS-ME50-BAL-DESCRIP       PIC X(50)  VALUE SPACES.

00073  01  COMP-WORKING-AMOUNTS.                                        ECS064
00074      12  COMP-3-AREA     COMP-3.                                  ECS064
00075          16  COMP-AMT-BEG    PIC S9(09)V9(02).                    ECS064
00076          16  COMP-AMT-COM    PIC S9(09)V9(02).                    ECS064
00077          16  COMP-AMT-CHG    PIC S9(09)V9(02).                    ECS064
00078          16  COMP-AMT-PMT    PIC S9(09)V9(02).                    ECS064
00079          16  COMP-AMT-END    PIC S9(09)V9(02).                    ECS064
00080          16  COMP-AMT-CUR    PIC S9(09)V9(02).                    ECS064
00081          16  COMP-AMT-OV30   PIC S9(09)V9(02).                    ECS064
00082          16  COMP-AMT-OV60   PIC S9(09)V9(02).                    ECS064
00083          16  COMP-AMT-OV90   PIC S9(09)V9(02).                    ECS064
00084  01  MISC-WORKING-STORAGE.                                        ECS064
00085      12  COMP-3-AREA     COMP-3.                                  ECS064
00086          16  PGCTR           PIC S9(05)      VALUE +0.            ECS064
00087          16  LNCTR           PIC S9(03)      VALUE +66.           ECS064
00088      12  GA-TOTALS       COMP-3.                                  ECS064
00089          16  G-CTR           PIC S9(07).                          ECS064
00090          16  G-BEG           PIC S9(09)V9(02).                    ECS064
00091          16  G-CHG           PIC S9(09)V9(02).                    ECS064
00092          16  G-PMT           PIC S9(09)V9(02).                    ECS064
00093          16  G-END           PIC S9(09)V9(02).                    ECS064
00094          16  G-OV30          PIC S9(09)V9(02).                    ECS064
00095          16  G-OV60          PIC S9(09)V9(02).                    ECS064
00096          16  G-OV90          PIC S9(09)V9(02).                    ECS064
00097          16  G-DBT           PIC S9(09)V9(02).                    ECS064
00098          16  G-CRD           PIC S9(09)V9(02).                    ECS064
00099      12  SUB-TOTALS      COMP-3.                                  ECS064
00100          16  S-BEG           PIC S9(09)V9(02).                    ECS064
00101          16  S-CHG           PIC S9(09)V9(02).                    ECS064
00102          16  S-PMT           PIC S9(09)V9(02).                    ECS064
00103          16  S-END           PIC S9(09)V9(02).                    ECS064
00104          16  S-OV30          PIC S9(09)V9(02).                    ECS064
00105          16  S-OV60          PIC S9(09)V9(02).                    ECS064
00106          16  S-OV90          PIC S9(09)V9(02).                    ECS064
00107          16  S-DBT           PIC S9(09)V9(02).                    ECS064
00108          16  S-CRD           PIC S9(09)V9(02).                    ECS064
00109      12  CARRIER-TOTALS  COMP-3.                                  ECS064
00110          16  C-BEG           PIC S9(09)V9(02).                    ECS064
00111          16  C-CHG           PIC S9(09)V9(02).                    ECS064
00112          16  C-PMT           PIC S9(09)V9(02).                    ECS064
00113          16  C-END           PIC S9(09)V9(02).                    ECS064
00114          16  C-OV30          PIC S9(09)V9(02).                    ECS064
00115          16  C-OV60          PIC S9(09)V9(02).                    ECS064
00116          16  C-OV90          PIC S9(09)V9(02).                    ECS064
00117          16  C-DBT           PIC S9(09)V9(02).                    ECS064
00118          16  C-CRD           PIC S9(09)V9(02).                    ECS064
00119      12  FINAL-TOTALS    COMP-3.                                  ECS064
00120          16  F-BEG           PIC S9(09)V9(02).                    ECS064
00121          16  F-CHG           PIC S9(09)V9(02).                    ECS064
00122          16  F-PMT           PIC S9(09)V9(02).                    ECS064
00123          16  F-END           PIC S9(09)V9(02).                    ECS064
00124          16  F-OV30          PIC S9(09)V9(02).                    ECS064
00125          16  F-OV60          PIC S9(09)V9(02).                    ECS064
00126          16  F-OV90          PIC S9(09)V9(02).                    ECS064
00127          16  F-DBT           PIC S9(09)V9(02).                    ECS064
00128          16  F-CRD           PIC S9(09)V9(02).                    ECS064
00129      12  CUR-CTL-1.                                               ECS064
00130          16  CUR-CARR-GROUP.                                      ECS064
00131              20  CUR-CARR    PIC  X(01).                          ECS064
00132              20  CUR-GROUP   PIC  X(06).                          ECS064
00133          16  CUR-RESP        PIC  X(10).                          ECS064
00134      12  PRE-CONTROL.                                             ECS064
00135          16  PRE-CTL-1.                                           ECS064
00136              20  PRE-CARR-GROUP.                                  ECS064
00137                  24  PRE-CARR    PIC  X(01).                      ECS064
00138                  24  PRE-GROUP   PIC  X(06).                      ECS064
00139              20  PRE-RESP        PIC  X(10).                      ECS064
00140          16  PRE-CTL-2.                                           ECS064
00141              20  PRE-ACCT        PIC  X(10).                      ECS064
00142      12  BALANCE-SW          PIC  X(01).                          ECS064
00143          88  ACCOUNT-CARRIES-BALANCE         VALUE 'A'.           ECS064
00144          88  AGENT-CARRIES-BALANCE           VALUE 'G'.           ECS064
00145      12  SAVE-COMPANY-NAME   PIC  X(30).                          ECS064
00146  EJECT                                                            ECS064
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
070714     12  HLD-064-BEG-BAL         PIC S9(9)V99  COMP-3 VALUE +0.
070714     12  HLD-064-END-BAL         PIC S9(9)V99  COMP-3 VALUE +0.

00147  01  HD1.                                                         ECS064
00148      12  FILLER              PIC  X(44)      VALUE SPACES.        ECS064
00149      12  FILLER              PIC  X(44)      VALUE                ECS064
00150              '          AGED ACCOUNTS RECEIVABLE          '.      ECS064
00151      12  FILLER              PIC  X(31)      VALUE SPACES.        ECS064
00152      12  FILLER              PIC  X(08)      VALUE 'ECS064'.      ECS064
00153                                                                   ECS064
00154  01  HD2.                                                         ECS064
00155      12  FILLER              PIC  X(51)      VALUE SPACES.        ECS064
00156      12  HD-CO               PIC  X(30).                          ECS064
00157      12  FILLER              PIC  X(38)      VALUE SPACES.        ECS064
00158      12  HD-RUN-DT           PIC  X(08)      VALUE SPACES.        ECS064
00159                                                                   ECS064
00160  01  HD3.                                                         ECS064
00161      12  FILLER              PIC  X(57)      VALUE SPACES.        ECS064
00162      12  HD-DT               PIC  X(18).                          ECS064
00163      12  FILLER              PIC  X(44)      VALUE SPACES.        ECS064
00164      12  FILLER              PIC  X(05)      VALUE 'PAGE '.       ECS064
00165      12  HD-PG               PIC ZZ,ZZ9.                          ECS064
00166                                                                   ECS064
00167  01  HD4.                                                         ECS064
00168      12  FILLER              PIC  X(44)      VALUE                ECS064
00169              '-- ACCOUNT NUMBERS --                BEGINNI'.      ECS064
00170      12  FILLER              PIC  X(44)      VALUE                ECS064
00171              'NG      CURRENT      PAYMENTS       ENDING  '.      ECS064
00172      12  FILLER              PIC  X(44)      VALUE                ECS064
00173              '  ---------------- A G I N G ---------------'.      ECS064
00174                                                                   ECS064
00175  01  HD5.                                                         ECS064
00176      12  FILLER              PIC  X(44)      VALUE                ECS064
00177              '-------- ACCOUNT NAME --------        BALANC'.      ECS064
00178      12  FILLER              PIC  X(44)      VALUE                ECS064
00179              'E       CHARGES     ADJUSTMENTS     BALANCE '.      ECS064
00180      12  FILLER              PIC  X(44)      VALUE                ECS064
00181              '      OVER 30       OVER 60       OVER 90   '.      ECS064
00182  EJECT                                                            ECS064
00183  01  P-REC.                                                       ECS064
00184      12  P-CCSW              PIC  X(01).                          ECS064
00185      12  P-LINE.                                                  ECS064
00186          16 FILLER           PIC  X(132).                         ECS064
00187      12  P-LINE-1 REDEFINES P-LINE.                               ECS064
00188          16  P-MSG           PIC  X(07).                          ECS064
00189          16  FILLER          PIC  X(01).                          ECS064
00190          16  P-DASH          PIC  X(01).                          ECS064
00191          16  FILLER          PIC  X(01).                          ECS064
00192          16  P-CARR          PIC  X(01).                          ECS064
00193          16  FILLER          PIC  X(01).                          ECS064
00194          16  P-GROUP         PIC  X(06).                          ECS064
00195          16  FILLER          PIC  X(04).                          ECS064
00196          16  P-TOT-TITLE     PIC  X(12).                          ECS064
00197          16  FILLER          PIC  X(98).                          ECS064
00198      12  P-LINE-2 REDEFINES P-LINE.                               ECS064
00199          16  P-RESP          PIC  X(10).                          ECS064
00200          16  FILLER          PIC  X(01).                          ECS064
00201          16  P-ACCT          PIC  X(10).                          ECS064
00202          16  FILLER          PIC  X(111).                         ECS064
00203      12  P-LINE-3 REDEFINES P-LINE.                               ECS064
00204          16  P-NAME          PIC  X(30).                          ECS064
00205          16  FILLER          PIC  X(04).                          ECS064
00206          16  P-BEG           PIC ZZ,ZZZ,ZZZ.ZZ-.                  ECS064
00207          16  P-CHG           PIC ZZ,ZZZ,ZZZ.ZZ-.                  ECS064
00208          16  P-PMT           PIC ZZ,ZZZ,ZZZ.ZZ-.                  ECS064
00209          16  P-END           PIC ZZ,ZZZ,ZZZ.ZZ-.                  ECS064
00210          16  P-OV30          PIC ZZ,ZZZ,ZZZ.ZZ-.                  ECS064
00211          16  P-OV60          PIC ZZ,ZZZ,ZZZ.ZZ-.                  ECS064
00212          16  P-OV90          PIC ZZ,ZZZ,ZZZ.ZZ-.                  ECS064
00213      12  T-LINE-1 REDEFINES P-LINE.                               ECS064
00214          16  T-NAME          PIC  X(30).                          ECS064
00215          16  FILLER          PIC  X(03).                          ECS064
00216          16  T-BEG           PIC ZZZ,ZZZ,ZZZ.ZZ-.                 ECS064
00217          16  FILLER          PIC  X(13).                          ECS064
00218          16  T-PMT           PIC ZZZ,ZZZ,ZZZ.ZZ-.                 ECS064
00219          16  FILLER          PIC  X(13).                          ECS064
00220          16  T-OV30          PIC ZZZ,ZZZ,ZZZ.ZZ-.                 ECS064
00221          16  FILLER          PIC  X(13).                          ECS064
00222          16  T-OV90          PIC ZZZ,ZZZ,ZZZ.ZZ-.                 ECS064
00223      12  T-LINE-2 REDEFINES P-LINE.                               ECS064
00224          16  FILLER          PIC  X(47).                          ECS064
00225          16  T-CHG           PIC ZZZ,ZZZ,ZZZ.ZZ-.                 ECS064
00226          16  FILLER          PIC  X(13).                          ECS064
00227          16  T-END           PIC ZZZ,ZZZ,ZZZ.ZZ-.                 ECS064
00228          16  FILLER          PIC  X(13).                          ECS064
00229          16  T-OV60          PIC ZZZ,ZZZ,ZZZ.ZZ-.                 ECS064
00230          16  FILLER          PIC  X(14).                          ECS064
00231  EJECT                                                            ECS064
00232                              COPY ELCDTECX.                       ECS064
00233  EJECT                                                            ECS064
00234                              COPY ELCDTEVR.                       ECS064
00235  EJECT                                                            ECS064
00236  PROCEDURE DIVISION.                                              ECS064
00237                                                                   ECS064
00238  0000-STANDARD-RTN.                                               ECS064
00239                              COPY ELCDTERX.                       ECS064
      ****=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=***
      ****                                                           ***
      ****   Set up the month-end auto balancing.                    ***
      ****                                                           ***
      ****=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=***

070714     MOVE WS-TIME                TO ME-START-TIME
070714     MOVE WS-CURRENT-DATE        TO ME-START-DATE
070714     MOVE ME-START-MO            TO ME-CNDS-MO
070714     MOVE ME-START-DA            TO ME-CNDS-DA
070714     MOVE ME-START-YR            TO ME-CNDS-YR

           .
00241  1000-INITIALIZE-OUTPUT.                                          ECS064
00242      OPEN INPUT  COMM-MSTR-IN                                     ECS064
00243           OUTPUT PRNTR
062104                 ME50-ECS064-BALANCE.
00244                                                                   ECS064
00245      MOVE LOW-VALUE              TO  COMPENSATION-MASTER          ECS064
00246                                      COMP-IN-RECORD               ECS064
00247                                      CUR-CTL-1                    ECS064
00248                                      PRE-CONTROL.                 ECS064
00249      MOVE COMPANY-NAME           TO  HD-CO.                       ECS064
00250      MOVE SPACES                 TO  SAVE-COMPANY-NAME.           ECS064
00251      MOVE ALPH-DATE              TO  HD-DT.                       ECS064
00252      MOVE WS-CURRENT-DATE        TO  HD-RUN-DT.                   ECS064
00253      MOVE SPACE-NP               TO  P-REC.                       ECS064

062104     IF DTE-CLIENT = 'CID'
062104         MOVE 'CILGM35'          TO  WS-ME50-BAL-JOB
062104     ELSE
030612       IF DTE-CLIENT = 'AHL'
030612         MOVE 'AHLGM35'          TO  WS-ME50-BAL-JOB
030612       ELSE
062121        if dte-client = 'FNL'
062121           MOVE 'FLLGM35'        to  ws-me50-bal-job
062121        else
062104         MOVE 'CIDCLGM35'        TO  WS-ME50-BAL-JOB
062121        end-if
030612       END-IF
062104     END-IF.
00254                                                                   ECS064
00255      GO TO 2000-PROCESS-RTN.                                      ECS064
00256  EJECT                                                            ECS064
00257  2000-PROCESS-RTN.                                                ECS064
00258      PERFORM 8000-MSTR-CONTROL-RTN  THRU  8099-EXIT.              ECS064
00259                                                                   ECS064
00260      IF CO-CTL-1  IS NOT EQUAL TO  CUR-CTL-1                      ECS064
00261          PERFORM 6000-BREAK-RTN  THRU  6999-EXIT.                 ECS064
00262                                                                   ECS064
00263      IF CO-CTL-1  IS EQUAL TO  HIGH-VALUE                         ECS064
00264          GO TO 9990-E-O-J.                                        ECS064
00265                                                                   ECS064
00266      IF CO-COMPANY-TYPE                                           ECS064
00267          GO TO 2000-PROCESS-RTN.                                  ECS064
00268                                                                   ECS064
00269 ******************************************************************ECS064
00270 *    APPLIES TO A/R USERS.                                        ECS064
00271 *    DTE TOTAL OPTIONS IS TO ALLOW PROGRAM TO USE AMOUNTS THAT    ECS064
00272 *    REPRESENT EITHER THE LAST MONTH END RUN OR THE LAST A/R CYCLEECS064
00273 *    THAT WAS RUN (OPTION 2).                                     ECS064
00274 ******************************************************************ECS064
00275                                                                   ECS064
00276      IF DTE-SYS-G-AR-USED NOT = '1' OR                            ECS064
00277         DTE-TOT-OPT = '1'                                         ECS064
00278          MOVE CO-BAL-FWD         TO  COMP-AMT-BEG                 ECS064
00279          MOVE CO-CUR-COM         TO  COMP-AMT-COM                 ECS064
00280          MOVE CO-CUR-CHG         TO  COMP-AMT-CHG                 ECS064
00281          MOVE CO-CUR-PMT         TO  COMP-AMT-PMT                 ECS064
00282          MOVE CO-END-BAL         TO  COMP-AMT-END                 ECS064
00283          MOVE CO-CUR             TO  COMP-AMT-CUR                 ECS064
00284          MOVE CO-OV30            TO  COMP-AMT-OV30                ECS064
00285          MOVE CO-OV60            TO  COMP-AMT-OV60                ECS064
00286          MOVE CO-OV90            TO  COMP-AMT-OV90                ECS064
00287       ELSE                                                        ECS064
00288          MOVE CO-CURRENT-BAL-FWD TO  COMP-AMT-BEG                 ECS064
00289          MOVE CO-CURRENT-CUR-COM TO  COMP-AMT-COM                 ECS064
00290          MOVE CO-CURRENT-CUR-CHG TO  COMP-AMT-CHG                 ECS064
00291          MOVE CO-CURRENT-CUR-PMT TO  COMP-AMT-PMT                 ECS064
00292          MOVE CO-CURRENT-END-BAL TO  COMP-AMT-END                 ECS064
00293          MOVE CO-CURRENT-CUR     TO  COMP-AMT-CUR                 ECS064
00294          MOVE CO-CURRENT-OV30    TO  COMP-AMT-OV30                ECS064
00295          MOVE CO-CURRENT-OV60    TO  COMP-AMT-OV60                ECS064
00296          MOVE CO-CURRENT-OV90    TO  COMP-AMT-OV90.               ECS064
00297                                                                   ECS064
00298      IF COMP-AMT-BEG      IS EQUAL TO +0                          ECS064
00299        AND  COMP-AMT-COM  IS EQUAL TO +0                          ECS064
00300        AND  COMP-AMT-CHG  IS EQUAL TO +0                          ECS064
00301        AND  COMP-AMT-PMT  IS EQUAL TO +0                          ECS064
00302        AND  COMP-AMT-END  IS EQUAL TO +0                          ECS064
00303        AND  COMP-AMT-CUR  IS EQUAL TO +0                          ECS064
00304        AND  COMP-AMT-OV30 IS EQUAL TO +0                          ECS064
00305        AND  COMP-AMT-OV60 IS EQUAL TO +0                          ECS064
00306        AND  COMP-AMT-OV90 IS EQUAL TO +0                          ECS064
00307          GO TO 2000-PROCESS-RTN.                                  ECS064
00308                                                                   ECS064
00309      SUBTRACT COMP-AMT-COM       FROM  COMP-AMT-CHG.              ECS064
00310                                                                   ECS064
00311      IF LNCTR  IS GREATER THAN  +056                              ECS064
00312          PERFORM 8600-HD-RTN  THRU  8699-EXIT.                    ECS064
00313                                                                   ECS064
00314      MOVE CO-RESP-NO             TO  P-RESP.                      ECS064
00315      MOVE SPACE-2                TO  P-CCSW.                      ECS064
00316                                                                   ECS064
00317      IF CO-ACCOUNT  IS NOT EQUAL TO  LOW-VALUE                    ECS064
00318          MOVE CO-ACCOUNT         TO  P-ACCT.                      ECS064
00319                                                                   ECS064
00320      PERFORM 8800-PRT-RTN  THRU  8800-EXIT.                       ECS064
00321                                                                   ECS064
00322      MOVE CO-ACCT-NAME           TO  P-NAME.                      ECS064
00323      MOVE COMP-AMT-BEG           TO  P-BEG.                       ECS064
00324      MOVE COMP-AMT-CHG           TO  P-CHG.                       ECS064
00325      MOVE COMP-AMT-PMT           TO  P-PMT.                       ECS064
00326      MOVE COMP-AMT-END           TO  P-END.                       ECS064
00327      MOVE COMP-AMT-OV30          TO  P-OV30.                      ECS064
00328      MOVE COMP-AMT-OV60          TO  P-OV60.                      ECS064
00329      MOVE COMP-AMT-OV90          TO  P-OV90.                      ECS064
00330                                                                   ECS064
00331      PERFORM 8800-PRT-RTN  THRU  8800-EXIT.                       ECS064
00332                                                                   ECS064
00333      ADD +1                      TO  G-CTR.                       ECS064
00334      ADD COMP-AMT-BEG            TO  G-BEG.                       ECS064
00335      ADD COMP-AMT-CHG            TO  G-CHG.                       ECS064
00336      ADD COMP-AMT-PMT            TO  G-PMT.                       ECS064
00337      ADD COMP-AMT-END            TO  G-END.                       ECS064
00338      ADD COMP-AMT-OV30           TO  G-OV30.                      ECS064
00339      ADD COMP-AMT-OV60           TO  G-OV60.                      ECS064
00340      ADD COMP-AMT-OV90           TO  G-OV90.                      ECS064
00341                                                                   ECS064
00342      IF COMP-AMT-END IS GREATER THAN ZERO                         ECS064
00343          ADD COMP-AMT-END        TO  G-DBT                        ECS064
00344      ELSE                                                         ECS064
00345          ADD COMP-AMT-END        TO  G-CRD.                       ECS064
00346                                                                   ECS064
00347      GO TO 2000-PROCESS-RTN.                                      ECS064
00348  EJECT                                                            ECS064
00349  6000-BREAK-RTN.                                                  ECS064
00350      IF CUR-CTL-1  IS EQUAL TO  LOW-VALUE                         ECS064
00351          GO TO 6500-INITIALIZE-ALL.                               ECS064
00352                                                                   ECS064
00353  6100-AGENT-BREAK.                                                ECS064
00354      ADD G-BEG                   TO  S-BEG.                       ECS064
00355      ADD G-CHG                   TO  S-CHG.                       ECS064
00356      ADD G-PMT                   TO  S-PMT.                       ECS064
00357      ADD G-END                   TO  S-END.                       ECS064
00358      ADD G-OV30                  TO  S-OV30.                      ECS064
00359      ADD G-OV60                  TO  S-OV60.                      ECS064
00360      ADD G-OV90                  TO  S-OV90.                      ECS064
00361      ADD G-DBT                   TO  S-DBT.                       ECS064
00362      ADD G-CRD                   TO  S-CRD.                       ECS064
00363                                                                   ECS064
00364      IF G-CTR  IS LESS THAN  +2                                   ECS064
00365          GO TO 6200-COMPANY-BREAK.                                ECS064
00366                                                                   ECS064
00367      MOVE 'GENERAL AGENT TOTALS'  TO  P-NAME.                     ECS064
00368                                                                   ECS064
00369      PERFORM 8800-PRT-RTN  THRU  8800-EXIT.                       ECS064
00370                                                                   ECS064
00371      MOVE G-BEG                   TO  T-BEG.                      ECS064
00372      MOVE G-PMT                   TO  T-PMT.                      ECS064
00373      MOVE G-OV30                  TO  T-OV30.                     ECS064
00374      MOVE G-OV90                  TO  T-OV90.                     ECS064
00375                                                                   ECS064
00376      PERFORM 8800-PRT-RTN  THRU  8800-EXIT.                       ECS064
00377                                                                   ECS064
00378      MOVE G-CHG                   TO  T-CHG.                      ECS064
00379      MOVE G-END                   TO  T-END.                      ECS064
00380      MOVE G-OV60                  TO  T-OV60.                     ECS064
00381                                                                   ECS064
00382      PERFORM 8800-PRT-RTN  THRU  8800-EXIT.                       ECS064
00383                                                                   ECS064
00384      MOVE SPACE-2 TO P-CCSW.                                      ECS064
00385                                                                   ECS064
00386  6200-COMPANY-BREAK.                                              ECS064
00387      IF CUR-CARR-GROUP  IS EQUAL TO  CO-CARR-GROUP                ECS064
00388          GO TO 6800-INITIALIZE-GA.                                ECS064
00389                                                                   ECS064
00390      MOVE 'COMPANY'              TO  P-MSG.                       ECS064
00391      MOVE CUR-CARR               TO  P-CARR.                      ECS064
00392      MOVE CUR-GROUP              TO  P-GROUP.                     ECS064
00393      MOVE ' TOTALS'              TO  P-TOT-TITLE.                 ECS064
00394                                                                   ECS064
00395      PERFORM 8800-PRT-RTN  THRU  8800-EXIT.                       ECS064
00396                                                                   ECS064
00397      MOVE S-BEG                  TO  T-BEG.                       ECS064
00398      MOVE S-PMT                  TO  T-PMT.                       ECS064
00399      MOVE S-OV30                 TO  T-OV30.                      ECS064
00400      MOVE S-OV90                 TO  T-OV90.                      ECS064
00401      MOVE SPACE-2                TO  P-CCSW.                      ECS064
00402                                                                   ECS064
00403      PERFORM 8800-PRT-RTN  THRU  8800-EXIT.                       ECS064
00404                                                                   ECS064
00405      MOVE S-CHG                  TO  T-CHG.                       ECS064
00406      MOVE S-END                  TO  T-END.                       ECS064
00407      MOVE S-OV60                 TO  T-OV60.                      ECS064
00408                                                                   ECS064
00409      PERFORM 8800-PRT-RTN  THRU  8800-EXIT.                       ECS064
00410                                                                   ECS064
00411      MOVE 'TOTAL DEBIT BALANCE'  TO  P-NAME.                      ECS064
00412      MOVE S-DBT                  TO  P-END.                       ECS064
00413      MOVE SPACE-2                TO  P-CCSW.                      ECS064
00414                                                                   ECS064
00415      PERFORM 8800-PRT-RTN  THRU  8800-EXIT.                       ECS064
00416                                                                   ECS064
00417      MOVE 'TOTAL CREDIT BALANCE'  TO  P-NAME.                     ECS064
00418      MOVE S-CRD                   TO  P-END.                      ECS064
00419                                                                   ECS064
00420      PERFORM 8800-PRT-RTN  THRU  8800-EXIT.                       ECS064
00421                                                                   ECS064
00422      ADD S-BEG                   TO  C-BEG.                       ECS064
00423      ADD S-CHG                   TO  C-CHG.                       ECS064
00424      ADD S-PMT                   TO  C-PMT.                       ECS064
00425      ADD S-END                   TO  C-END.                       ECS064
00426      ADD S-OV30                  TO  C-OV30.                      ECS064
00427      ADD S-OV60                  TO  C-OV60.                      ECS064
00428      ADD S-OV90                  TO  C-OV90.                      ECS064
00429      ADD S-DBT                   TO  C-DBT.                       ECS064
00430      ADD S-CRD                   TO  C-CRD.                       ECS064
00431                                                                   ECS064
00432  6300-CARRIER-BREAK.                                              ECS064
00433      IF CO-CARRIER  IS EQUAL  TO CUR-CARR                         ECS064
00434          GO TO 6700-INITIALIZE-COMPANY.                           ECS064
00435                                                                   ECS064
00436      MOVE SPACES                 TO  CUR-GROUP.                   ECS064
00437                                                                   ECS064
00438      PERFORM 8600-HD-RTN  THRU  8699-EXIT.                        ECS064
00439                                                                   ECS064
00440      MOVE 'CARRIER'              TO  P-MSG.                       ECS064
00441      MOVE CUR-CARR               TO  P-CARR.                      ECS064
00442      MOVE ' TOTALS'              TO  P-TOT-TITLE.                 ECS064
00443                                                                   ECS064
00444      PERFORM 8800-PRT-RTN  THRU  8800-EXIT.                       ECS064
00445                                                                   ECS064
00446      MOVE C-BEG                  TO  T-BEG.                       ECS064
00447      MOVE C-PMT                  TO  T-PMT.                       ECS064
00448      MOVE C-OV30                 TO  T-OV30.                      ECS064
00449      MOVE C-OV90                 TO  T-OV90.                      ECS064
00450      MOVE SPACE-2                TO  P-CCSW.                      ECS064
00451                                                                   ECS064
00452      PERFORM 8800-PRT-RTN  THRU  8800-EXIT.                       ECS064
00453                                                                   ECS064
00454      MOVE C-CHG                  TO  T-CHG.                       ECS064
00455      MOVE C-END                  TO  T-END.                       ECS064
00456      MOVE C-OV60                 TO  T-OV60.                      ECS064
00457                                                                   ECS064
00458      PERFORM 8800-PRT-RTN  THRU  8800-EXIT.                       ECS064
00459                                                                   ECS064
00460      MOVE 'TOTAL DEBIT BALANCE'  TO  P-NAME.                      ECS064
00461      MOVE C-DBT                  TO  P-END.                       ECS064
00462      MOVE SPACE-2                TO  P-CCSW.                      ECS064
00463                                                                   ECS064
00464      PERFORM 8800-PRT-RTN  THRU  8800-EXIT.                       ECS064
00465                                                                   ECS064
00466      MOVE 'TOTAL CREDIT BALANCE'  TO  P-NAME.                     ECS064
00467      MOVE C-CRD                   TO  P-END.                      ECS064
00468                                                                   ECS064
00469      PERFORM 8800-PRT-RTN  THRU  8800-EXIT.                       ECS064
00470                                                                   ECS064
00471      ADD C-BEG                   TO  F-BEG.                       ECS064
00472      ADD C-CHG                   TO  F-CHG.                       ECS064
00473      ADD C-PMT                   TO  F-PMT.                       ECS064
00474      ADD C-END                   TO  F-END.                       ECS064
00475      ADD C-OV30                  TO  F-OV30.                      ECS064
00476      ADD C-OV60                  TO  F-OV60.                      ECS064
00477      ADD C-OV90                  TO  F-OV90.                      ECS064
00478      ADD C-DBT                   TO  F-DBT.                       ECS064
00479      ADD C-CRD                   TO  F-CRD.                       ECS064
00480                                                                   ECS064
00481  6030-FINAL-BREAK.                                                ECS064
00482      IF CO-CTL-1  IS NOT EQUAL TO  HIGH-VALUE                     ECS064
00483          GO TO 6600-INITIALIZE-CARRIER.                           ECS064
00484                                                                   ECS064
00485      MOVE SPACES                 TO  CUR-CARR.                    ECS064
00486                                                                   ECS064
00487      PERFORM 8600-HD-RTN  THRU  8699-EXIT.                        ECS064
00488                                                                   ECS064
00489      MOVE 'FINAL TOTALS'         TO  P-TOT-TITLE.                 ECS064
00490                                                                   ECS064
00491      PERFORM 8800-PRT-RTN  THRU  8800-EXIT.                       ECS064
00492                                                                   ECS064
00493      MOVE F-BEG                  TO  T-BEG.                       ECS064
00494      MOVE F-PMT                  TO  T-PMT.                       ECS064
00495      MOVE F-OV30                 TO  T-OV30.                      ECS064
00496      MOVE F-OV90                 TO  T-OV90.                      ECS064
00497      MOVE SPACE-2                TO  P-CCSW.                      ECS064
00498                                                                   ECS064
00499      PERFORM 8800-PRT-RTN  THRU  8800-EXIT.                       ECS064
00500                                                                   ECS064
00501      MOVE F-CHG                  TO  T-CHG.                       ECS064
00502      MOVE F-END                  TO  T-END.                       ECS064
070714     compute hld-064-beg-bal = f-beg
070714     compute hld-064-end-bal = f-end
00503      MOVE F-OV60                 TO  T-OV60.                      ECS064
00504                                                                   ECS064
00505      PERFORM 8800-PRT-RTN  THRU  8800-EXIT.                       ECS064

062104     MOVE F-END                  TO WS-ME50-BAL-AMT-LOW.
062104     MOVE F-END                  TO WS-ME50-BAL-AMT-HIGH.
062104     MOVE WS-BAL50-DESCRIPTION   TO WS-ME50-BAL-DESCRIP.
062104     WRITE ME50-ECS064-BALANCE-REC FROM WS-ME50-BALANCE-REC.
00506                                                                   ECS064
00507      MOVE 'TOTAL DEBIT BALANCE'  TO  P-NAME.                      ECS064
00508      MOVE F-DBT                  TO  P-END.                       ECS064
00509      MOVE SPACE-2                TO  P-CCSW.                      ECS064
00510                                                                   ECS064
00511      PERFORM 8800-PRT-RTN  THRU  8800-EXIT.                       ECS064
00512                                                                   ECS064
00513      MOVE 'TOTAL CREDIT BALANCE'  TO  P-NAME.                     ECS064
00514      MOVE F-CRD                   TO  P-END.                      ECS064
00515                                                                   ECS064
00516      PERFORM 8800-PRT-RTN  THRU  8800-EXIT.                       ECS064
00517                                                                   ECS064
00518      GO TO 6999-EXIT.                                             ECS064
00519                                                                   ECS064
00520  6500-INITIALIZE-ALL.                                             ECS064
00521      MOVE +0                     TO  F-BEG   F-CHG                ECS064
00522                                      F-PMT   F-END                ECS064
00523                                      F-OV30  F-OV60               ECS064
00524                                      F-OV90  F-DBT                ECS064
00525                                      F-CRD.                       ECS064
00526                                                                   ECS064
00527  6600-INITIALIZE-CARRIER.                                         ECS064
00528      MOVE CO-CARRIER             TO  CUR-CARR.                    ECS064
00529      MOVE +0                     TO  C-BEG   C-CHG                ECS064
00530                                      C-PMT   C-END                ECS064
00531                                      C-OV30  C-OV60               ECS064
00532                                      C-OV90  C-DBT                ECS064
00533                                      C-CRD.                       ECS064
00534                                                                   ECS064
00535      IF SAVE-COMPANY-NAME  IS EQUAL TO  SPACES                    ECS064
00536          MOVE COMPANY-NAME       TO  HD-CO                        ECS064
00537      ELSE                                                         ECS064
00538          MOVE SAVE-COMPANY-NAME  TO  HD-CO.                       ECS064
00539                                                                   ECS064
00540  6700-INITIALIZE-COMPANY.                                         ECS064
00541      MOVE CO-GROUPING            TO  CUR-GROUP.                   ECS064
00542      MOVE +0                     TO  S-BEG   S-CHG                ECS064
00543                                      S-PMT   S-END                ECS064
00544                                      S-OV30  S-OV60               ECS064
00545                                      S-OV90  S-DBT                ECS064
00546                                      S-CRD.                       ECS064
00547                                                                   ECS064
00548      IF SAVE-COMPANY-NAME  IS EQUAL TO  SPACES                    ECS064
00549          MOVE COMPANY-NAME       TO  HD-CO                        ECS064
00550      ELSE                                                         ECS064
00551          MOVE SAVE-COMPANY-NAME  TO  HD-CO.                       ECS064
00552                                                                   ECS064
00553      MOVE +066                   TO  LNCTR.                       ECS064
00554                                                                   ECS064
00555  6800-INITIALIZE-GA.                                              ECS064
00556      MOVE CO-RESP-NO             TO  CUR-RESP.                    ECS064
00557      MOVE +0                     TO  G-CTR.                       ECS064
00558      MOVE +0                     TO  G-BEG   G-CHG                ECS064
00559                                      G-PMT   G-END                ECS064
00560                                      G-OV30  G-OV60               ECS064
00561                                      G-OV90  G-DBT                ECS064
00562                                      G-CRD.                       ECS064
00563                                                                   ECS064
00564  6999-EXIT.                                                       ECS064
00565      EXIT.                                                        ECS064
00566  EJECT                                                            ECS064
00567  8000-MSTR-CONTROL-RTN.                                           ECS064
00568      READ COMM-MSTR-IN                                            ECS064
00569          AT END                                                   ECS064
00570              MOVE HIGH-VALUE     TO  COMP-IN-RECORD               ECS064
00571                                      COMPENSATION-MASTER          ECS064
00572              GO TO 8099-EXIT.                                     ECS064
00573                                                                   ECS064
00574      MOVE COMP-IN-RECORD         TO  COMPENSATION-MASTER.         ECS064
00575                                                                   ECS064
00576      IF CO-CTL-1  IS EQUAL TO  PRE-CTL-1                          ECS064
00577          GO TO 8050-SET-NEW.                                      ECS064
00578                                                                   ECS064
00579  8010-RESET-CARRIER.                                              ECS064
00580      IF PRE-CARR  IS EQUAL TO  CO-CARRIER                         ECS064
00581          GO TO 8020-RESET-COMPANY.                                ECS064
00582                                                                   ECS064
00583      MOVE CO-CARRIER             TO  PRE-CARR.                    ECS064
00584      MOVE SPACES                 TO  SAVE-COMPANY-NAME.           ECS064
00585                                                                   ECS064
00586  8020-RESET-COMPANY.                                              ECS064
00587      IF PRE-GROUP  IS EQUAL TO  CO-GROUPING                       ECS064
00588          GO TO 8030-RESET-AGENT.                                  ECS064
00589                                                                   ECS064
00590      MOVE CO-GROUPING            TO  PRE-GROUP.                   ECS064
00591                                                                   ECS064
00592  8030-RESET-AGENT.                                                ECS064
00593      MOVE CO-RESP-NO             TO  PRE-RESP.                    ECS064
00594      MOVE 'A'                    TO  BALANCE-SW.                  ECS064
00595                                                                   ECS064
00596  8050-SET-NEW.                                                    ECS064
00597      IF CO-COMPANY-TYPE                                           ECS064
00598          GO TO 8080-SET-COMPANY.                                  ECS064
00599                                                                   ECS064
00600      IF CO-GEN-AGENT-TYPE                                         ECS064
00601          GO TO 8070-SET-AGENT.                                    ECS064
00602                                                                   ECS064
00603      IF NOT CO-ACCOUNT-TYPE                                       ECS064
00604          MOVE '0302'              TO  WS-RETURN-CODE              ECS064
00605          MOVE 'FATAL FILE ERROR'  TO  WS-ABEND-MESSAGE            ECS064
00606          GO TO ABEND-PGM.                                         ECS064
00607                                                                   ECS064
00608  8060-SET-ACCOUNT.                                                ECS064
00609      IF CO-NO-BALANCE                                             ECS064
00610          GO TO 8000-MSTR-CONTROL-RTN.                             ECS064
00611                                                                   ECS064
00612      GO TO 8099-EXIT.                                             ECS064
00613                                                                   ECS064
00614  8070-SET-AGENT.                                                  ECS064
00615      MOVE 'G'                    TO  BALANCE-SW.                  ECS064
00616                                                                   ECS064
00617      GO TO 8099-EXIT.                                             ECS064
00618                                                                   ECS064
00619  8080-SET-COMPANY.                                                ECS064
00620      MOVE CO-ACCT-NAME           TO  SAVE-COMPANY-NAME.           ECS064
00621                                                                   ECS064
00622      GO TO 8099-EXIT.                                             ECS064
00623                                                                   ECS064
00624  8099-EXIT.                                                       ECS064
00625      EXIT.                                                        ECS064
00626  EJECT                                                            ECS064
00627  8600-HD-RTN.                                                     ECS064
00628      MOVE HD1                    TO  P-LINE.                      ECS064
00629      MOVE SPACE-NP               TO  P-CCSW.                      ECS064
00630                                                                   ECS064
00631      PERFORM 8800-PRT-RTN  THRU  8800-EXIT.                       ECS064
00632                                                                   ECS064
00633      ADD +1                      TO  PGCTR.                       ECS064
00634                                                                   ECS064
00635      MOVE PGCTR                  TO  HD-PG.                       ECS064
00636      MOVE HD2                    TO  P-LINE.                      ECS064
00637      MOVE SPACE-1                TO  P-CCSW.                      ECS064
00638                                                                   ECS064
00639      PERFORM 8800-PRT-RTN  THRU  8800-EXIT.                       ECS064
00640                                                                   ECS064
00641      MOVE HD3                    TO  P-LINE.                      ECS064
00642      MOVE SPACE-1                TO  P-CCSW.                      ECS064
00643                                                                   ECS064
00644      PERFORM 8800-PRT-RTN  THRU  8800-EXIT.                       ECS064
00645                                                                   ECS064
00646      MOVE SPACE-2                TO  P-CCSW.                      ECS064
00647      MOVE 'COMPANY'              TO  P-MSG.                       ECS064
00648      MOVE CUR-CARR               TO  P-CARR.                      ECS064
00649      MOVE CUR-GROUP              TO  P-GROUP.                     ECS064
00650      MOVE '-'                    TO  P-DASH.                      ECS064
00651                                                                   ECS064
00652      PERFORM 8800-PRT-RTN  THRU  8800-EXIT.                       ECS064
00653                                                                   ECS064
00654      MOVE SPACE-2                TO  P-CCSW.                      ECS064
00655      MOVE HD4                    TO  P-LINE.                      ECS064
00656                                                                   ECS064
00657      PERFORM 8800-PRT-RTN  THRU  8800-EXIT.                       ECS064
00658                                                                   ECS064
00659      MOVE HD5                    TO  P-LINE.                      ECS064
00660                                                                   ECS064
00661      PERFORM 8800-PRT-RTN  THRU  8800-EXIT.                       ECS064
00662                                                                   ECS064
00663      MOVE +10                    TO  LNCTR.                       ECS064
00664      MOVE SPACE-2                TO  P-CCSW.                      ECS064
00665                                                                   ECS064
00666  8699-EXIT.                                                       ECS064
00667      EXIT.                                                        ECS064
00668                                                                   ECS064
00669  8800-PRT-RTN.                                                    ECS064
00670      MOVE P-REC                  TO  PRT.                         ECS064
00671      MOVE P-CCSW                 TO  X.                           ECS064
00672                                                                   ECS064
00673      IF P-CCSW  IS EQUAL TO  SPACE-1                              ECS064
00674          ADD +1                  TO  LNCTR                        ECS064
00675      ELSE                                                         ECS064
00676          IF P-CCSW  IS EQUAL TO  SPACE-2                          ECS064
00677              ADD +2              TO  LNCTR                        ECS064
00678          ELSE                                                     ECS064
00679              IF P-CCSW  IS EQUAL TO  SPACE-3                      ECS064
00680                  ADD +3          TO  LNCTR.                       ECS064
00681                                                                   ECS064
00682      MOVE SPACES                 TO  P-REC.                       ECS064
00683      MOVE SPACE-1                TO  P-CCSW.                      ECS064
00684                                                                   ECS064
00685  8850-COPY-PRT-RTN.                                               ECS064
00686                              COPY ELCPRT2.                        ECS064
00687                                                                   ECS064
00688  8800-EXIT.                                                       ECS064
00689      EXIT.                                                        ECS064
00690  EJECT                                                            ECS064
00691  9990-E-O-J.                                                      ECS064
00692                              COPY ELCPRTC.                        ECS064
00693                                                                   ECS064
00694  9995-CLOSE.                                                      ECS064
00695      CLOSE COMM-MSTR-IN                                           ECS064
00696            PRNTR
062104           ME50-ECS064-BALANCE.
00697                                                                   ECS064
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
070714     IF ME-DO-UPDATE
              move hld-064-BEG-BAL     to ME-064-BEG-BAL
              move hld-064-END-BAL     to ME-064-END-BAL
              display ' me update ' me-064-end-bal ' ' me-064-beg-bal
070714        ACCEPT WS-TIME-OF-DAY FROM TIME
070714        REWRITE MONTH-END-BALANCES
              display ' me rewrite ' ermebl-file-status
070714        CLOSE ERMEBL
070714     end-if

00698      GOBACK.                                                      ECS064
00699                                                                   ECS064
00700  ABEND-PGM SECTION.                                               ECS064
00701                              COPY ELCABEND.                       ECS064
