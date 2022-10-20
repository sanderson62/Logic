00001  IDENTIFICATION DIVISION.                                         06/06/97
00002                                                                   EL008
00003  PROGRAM-ID.                 EL008 .                                 LV019
00004 *              PROGRAM CONVERTED BY                                  CL*17
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL*17
00006 *              CONVERSION DATE 09/26/95 15:49:18.                    CL*17
00007 *                            VMOD=2.019.                             CL*19
00008 *                                                                 EL008
00009 *AUTHOR.     LOGIC INC.                                              CL*17
00010 *            DALLAS, TEXAS.                                          CL*17
00023 *REMARKS.                                                            CL**9
00024 *         THIS PROGRAM IS STARTED EITHER FROM EL150, EL130, EL1602   CL*18
00025 *         OR EL180-  IF START IS MADE FROM EL150 OR EL130,           CL*10
00026 *         A SINGLE CLAIM (IN INTERFACE BLOCK) IS PRINTED             CL**9
00027 *         IF START IS MADE FROM EL180, ELACTQ FILE IS SCANNED        CL**9
00028 *         AND ALL CLAIMS LEFT IN QUE FOR PRINTING WILL BE            CL**9
00029 *         PRINTED AND THE PRINT FLAG IS TURNED OFF FROM ACTQ.        CL**9
00030                                                                   EL008
00031 *       INPUT FILES-   ELMSTR-   CLAIM MASTER                        CL**9
00032 *                      ELACTQ-   ACTIVITY QUE                        CL**9
00033 *                      ELTRLR-   ACTIVITY TRAILERS                   CL**9
00034 *                      ELCNTL-   CONTROL FILE                        CL**9
00035 *                      ELCERT-   CERTIFICATE MASTER                  CL**9
00036 *                      ERACCT-   ACCOUNT MASTER                      CL**9
00037 *                      ELBENE-   BENEFICIARY MASTER                  CL**9
00038                                                                   EL008
00039 *       OUTPUT-                  CLAIM STATUS REPORT                 CL**9
00040 *                                (OPEN/CLOSE HISTORY)                CL**9
00041 *                      ELACTQ-   ACTIVITY QUE                        CL**9
00042                                                                   EL008
00043 *       COMMARERA-     RECEIVED FROM THE CALLING PROGRAM             CL**9
00044 *                      EL130 OR EL150 HAS THE CLAIM TO BE PRINTED,   CL*10
00045 *                      AND ENTRY CODE 1 FOR SHORT FORM AND           CL**9
00046 *                      2  FOR THE LONG FORM--                        CL**9
00047 *                      EL180 WILL DIRECT THE PRGRAM TO SCAN          CL**9
00048 *                      THE ACTQ FILE FOR ALL CLAIMS TO BE PRINTED.   CL**9
00049 *                      EL1602 - ?????                                CL*18
121802******************************************************************
121802*                   C H A N G E   L O G
121802*
121802* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
121802*-----------------------------------------------------------------
121802*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
121802* EFFECTIVE    NUMBER
121802*-----------------------------------------------------------------
121802* 121802    2001061800003  SMVA  ADD PROCESSING FOR NEW CLM TYPE I
091113* 091113    2013091100001  AJRA  CHANGE ACTION TYPE LIKE SCREEN
101713* 101713    2013091100001  AJRA  SPECIAL 95 TRLR FOR DCC
052614* 052614    2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
020816* 020816  CR2015082500001  PEMA  ADD PROCESSING FOR NEW COMP VPP
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
080322* 080322  CR2021100800003  TANA  Add B and H claim types
121802******************************************************************
00050      EJECT                                                        EL008
00051  ENVIRONMENT DIVISION.                                            EL008
00052  DATA DIVISION.                                                   EL008
00053  WORKING-STORAGE SECTION.                                         EL008
00054  77  FILLER  PIC X(32) VALUE '********************************'.  EL008
00055  77  FILLER  PIC X(32) VALUE '     EL008  WORKING-STORAGE     '.  EL008
00056  77  FILLER  PIC X(32) VALUE '********* VMOD=2.019 ***********'.     CL*19
00057                                                                      CL*18
00058  77  THIS-PGM               PIC X(5) VALUE 'EL008'.                  CL*18
00059                                                                   EL008
00060  77  WS-ELMSTR-EOF-SW       PIC X    VALUE SPACE.                    CL*17
00061      88  END-OF-MSTR-FILE            VALUE 'E'.                   EL008
00062                                                                   EL008
00063  77  WS-ELACTQ-EOF-SW       PIC X    VALUE SPACE.                    CL*17
00064      88  END-OF-ACTQ-FILE            VALUE 'E'.                   EL008
00065                                                                   EL008
00066  77  WS-ELTRLR-EOF-SW       PIC X    VALUE SPACE.                    CL*17
00067      88  END-OF-TRLR-FILE            VALUE 'E'.                   EL008
00068                                                                   EL008
00069  77  WS-FULL-PRINT-SW       PIC X    VALUE SPACE.                 EL008
00070      88  FULL-PRINT-REQUIRED         VALUE 'X'.                   EL008
00071                                                                   EL008
00072  77  WS-TRAILER-KEY-CHG-SW  PIC X    VALUE SPACE.                 EL008
00073      88  TRAILER-KEY-CHANGED         VALUE 'X'.                   EL008
00074                                                                   EL008
00075  77  END-OC-TABLE-SW        PIC X    VALUE SPACE.                 EL008
00076      88  END-OC-TABLE                VALUE 'E'.                   EL008
00077      88  OC-TABLE-LOADED             VALUE 'X'.                   EL008
00078                                                                   EL008
00079  77  END-AD-TABLE-SW        PIC X    VALUE SPACE.                 EL008
00080      88  END-AD-TABLE                VALUE 'E'.                   EL008
00081      88  AD-TABLE-LOADED             VALUE 'X'.                   EL008
00082                                                                   EL008
00083  77  END-AP-TABLE-SW        PIC X    VALUE SPACE.                 EL008
00084      88  END-AP-TABLE                VALUE 'E'.                   EL008
00085      88  AP-TABLE-LOADED             VALUE 'X'.                   EL008
00086                                                                   EL008
00087  77  WS-ACTQ-READ-ERROR-SW PIC X    VALUE SPACE.                  EL008
00088      88  ACTQ-READ-ERROR           VALUE 'X'.                     EL008
00089                                                                   EL008
00090  77  WS-CNTL-ERROR-SW      PIC X    VALUE SPACE.                  EL008
00091      88  NO-COMPANY-RECORD         VALUE 'X'.                     EL008
00092                                                                   EL008
00093  77  WS-MSTR-READ-ERROR-SW PIC X    VALUE SPACE.                  EL008
00094      88  MSTR-READ-ERROR           VALUE 'X'.                     EL008
00095                                                                   EL008
00096  77  WS-CERT-READ-ERROR-SW PIC X    VALUE SPACE.                  EL008
00097      88  CERT-READ-ERROR           VALUE 'X'.                     EL008
00098                                                                   EL008
00099  77  WS-PLCY-READ-ERROR-SW   PIC X    VALUE SPACE.                   CL*15
00100      88  PLCY-READ-ERROR              VALUE 'X'.                     CL*15
00101                                                                      CL*15
00102  77  WS-PLAN-READ-ERROR-SW   PIC X    VALUE SPACE.                   CL*15
00103      88  PLAN-READ-ERROR              VALUE 'X'.                     CL*15
00104                                                                      CL*15
00105  77  WS-TRLR-BROWSE-ERROR-SW PIC X    VALUE SPACE.                EL008
00106      88  TRLR-BROWSE-ERROR           VALUE 'X'.                   EL008
00107                                                                   EL008
00108  77  WS-ACCT-BROWSE-SW       PIC X    VALUE SPACE.                EL008
00109      88  ACCT-BROWSE-OKAY            VALUE 'X'.                   EL008
00110                                                                   EL008
00111  77  WS-BENE-BROWSE-SW       PIC X    VALUE SPACE.                EL008
00112      88  BENE-BROWSE-OKAY            VALUE 'X'.                   EL008
00113                                                                      CL*15
00114  77  WS-PRODUCER-BROWSE-SW   PIC X    VALUE SPACE.                   CL*15
00115      88  PRODUCER-BROWSE-OKAY         VALUE 'X'.                     CL*15
00116                                                                   EL008
00117  77  WS-LN-SW               PIC X    VALUE SPACE.                 EL008
00118      88  LAST-NAME-LOADED            VALUE 'X'.                   EL008
00119                                                                   EL008
00120  77  WS-FN-SW               PIC X    VALUE SPACE.                 EL008
00121      88  FIRST-NAME-LOADED           VALUE 'X'.                   EL008
00122                                                                   EL008
00123  77  WS-CANC-DT             PIC XX   VALUE LOW-VALUES.            EL008
00124  77  WS-STATUS              PIC X    VALUE SPACES.                EL008
00125                                                                   EL008
00126  77  OC-SUB                 PIC  S9(4)  VALUE ZEROS COMP.         EL008
00127                                                                   EL008
00128  77  SUB1                   PIC S9(4)   VALUE ZEROS COMP.         EL008
091113
091113 77  WS-SUB                 PIC 9       VALUE 0.
101713
101713 77  A1                     PIC S999 COMP-3 VALUE +0.
00129                                                                   EL008
00130  77  WS-RECORD-TYPE         PIC X     VALUE SPACE.                EL008
00131                                                                   EL008
00132  77  WS-SAVED-FORM-NO       PIC X(12) VALUE SPACE.                EL008
00133                                                                      CL*18
00134  77  WS-SAVED-PI-COMPANY-ID PIC X(04) VALUE SPACE.                   CL*18
00135                                                                   EL008
00136  01  WS-DATE-AREA.                                                EL008
00137      05  SAVE-DATE           PIC X(8)    VALUE SPACES.            EL008
00138      05  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.            EL008
00139                                                                   EL008
00140      05  WS-PHONE-BRKDN              PIC 9(11).                   EL008
00141      05  FILLER   REDEFINES WS-PHONE-BRKDN.                       EL008
00142          10  FILLER                  PIC 9.                       EL008
00143          10  WS-PH-1                 PIC 999.                     EL008
00144          10  WS-PH-2                 PIC 999.                     EL008
00145          10  WS-PH-3                 PIC 9999.                    EL008
00146                                                                   EL008
00147      05  WS-PHONE-EDIT.                                           EL008
00148          10  WS-PH-ED-1              PIC XXX.                     EL008
00149          10  FILLER                  PIC X   VALUE '-'.           EL008
00150          10  WS-PH-ED-2              PIC XXX.                     EL008
00151          10  FILLER                  PIC X   VALUE '-'.           EL008
00152          10  WS-PH-ED-3              PIC XXXX.                    EL008
00153                                                                   EL008
00154      05  WS-ZIP-WORK.                                                CL*12
00155          10  WS-ZIP-PRIME        PIC X(5).                           CL*12
00156          10  WS-ZIP-DASH         PIC X.                              CL*12
00157          10  WS-ZIP-PLUS4        PIC X(4).                           CL*12
00158      05  WS-CANADIAN-ZIP-WORK  REDEFINES  WS-ZIP-WORK.               CL*12
00159          10  WS-CAN-POSTAL-1     PIC XXX.                            CL*12
00160          10  FILLER              PIC X.                              CL*12
00161          10  WS-CAN-POSTAL-2     PIC XXX.                            CL*12
00162          10  FILLER              PIC XXX.                            CL*12
00163      05  WS-WORK-PHONE           PIC X(10)  VALUE ZEROS.          EL008
00164      05  WS-NUMERIC-PHONE REDEFINES WS-WORK-PHONE                 EL008
00165                                  PIC 9(10).                       EL008
00166      05  SSP                         PIC X  VALUE ' '.            EL008
00167      05  DSP                         PIC X  VALUE '0'.            EL008
00168      05  TSP                         PIC X  VALUE '-'.            EL008
00169      05  TPG                         PIC X  VALUE '1'.            EL008
00170                                                                   EL008
00171      05  WS-NEXT-TRAN            PIC X(4).                        EL008
00172      05  WS-TERMINAL-ID.                                          EL008
00173          10  WS-TERM-PREFIX      PIC XX.                          EL008
00174          10  FILLER              PIC XX.                          EL008
00175                                                                   EL008
00176      05  WS-LAST-NAME    PIC X(15).                               EL008
00177      05  FILLER REDEFINES WS-LAST-NAME.                           EL008
00178          10  WS-L-BYTE   PIC X  OCCURS 15                         EL008
00179                          INDEXED BY L-IND.                        EL008
00180      05  WS-FIRST-NAME   PIC X(12).                               EL008
00181      05  FILLER REDEFINES WS-FIRST-NAME.                          EL008
00182          10  WS-F-BYTE   PIC X  OCCURS 12                         EL008
00183                          INDEXED BY F-IND.                        EL008
00184      05  PREVIOUS-BYTE          PIC X  VALUE SPACES.              EL008
00185                                                                   EL008
00186      05  WS-AGE             PIC 9(04)  VALUE ZEROS.                  CL*15
00187      05  WS-AGE-R REDEFINES WS-AGE.                                  CL*15
00188          10  WS-AGE-1-2     PIC 9(02).                               CL*15
00189          10  WS-AGE-3-4     PIC 9(02).                               CL*15
00190                                                                      CL*15
00191      05  WS-ACCESS.                                               EL008
00192          10  FILLER         PIC XX  VALUE SPACES.                 EL008
00193          10  WS-BEN-CODE    PIC XX.                                  CL**5
00194                                                                   EL008
00195      05  WS-AT-CONTROL-PRIMARY.                                   EL008
00196          10  WS-AT-CONTROL-WO-SEQ.                                EL008
00197              15  WS-AT-COMPANY-CD   PIC X.                        EL008
00198              15  WS-AT-CARRIER      PIC X.                        EL008
00199              15  WS-AT-CLAIM-NO     PIC X(7).                     EL008
00200              15  WS-AT-CERT-NO      PIC X(11).                    EL008
00201          10  WS-AT-SEQ-NO           PIC S9(4) COMP.               EL008
00202                                                                   EL008
00203      05  WS-NEW-AT-CONTROL-PRIMARY.                               EL008
00204          10  WS-NEW-AT-CONTROL-WO-SEQ  PIC X(20).                 EL008
00205          10  FILLER                    PIC S9(4) COMP.            EL008
00206                                                                   EL008
00207      05  WS-AT-CONTROL-SAVED.                                     EL008
00208          10  WS-AT-CONTROL-SAVED-WO-SEQ  PIC X(20).               EL008
00209          10  FILLER                    PIC S9(4) COMP.            EL008
00210                                                                   EL008
00211      05  WS-CF-CONTROL-PRIMARY.                                   EL008
00212          10  WS-CF-COMPANY-ID   PIC XXX.                          EL008
00213          10  WS-CF-RECORD-TYPE  PIC X.                            EL008
00214          10  WS-CF-PROCESSOR    PIC X(4).                         EL008
00215          10  WS-CF-SEQUENCE-NO  PIC S9(4) COMP.                   EL008
00216                                                                   EL008
00217      05  WS-CL-CONTROL-PRIMARY.                                   EL008
00218          10  WS-CL-COMPANY-CD   PIC X.                            EL008
00219          10  WS-CL-CARRIER      PIC X.                            EL008
00220          10  WS-CL-CLAIM-NO     PIC X(7).                         EL008
00221          10  WS-CL-CERT-NO      PIC X(11).                        EL008
00222                                                                   EL008
00223      05  WS-LAST-AQ-KEY         PIC X(20) VALUE SPACES.           EL008
00224      05  WS-AQ-CONTROL-PRIMARY.                                   EL008
00225          10  WS-AQ-COMPANY-CD   PIC X.                            EL008
00226          10  WS-AQ-CARRIER      PIC X.                            EL008
00227          10  WS-AQ-CLAIM-NO     PIC X(7).                         EL008
00228          10  WS-AQ-CERT-NO      PIC X(11).                        EL008
00229                                                                   EL008
00230      05  WS-CM-CONTROL-PRIMARY.                                   EL008
00231          10  WS-CM-COMPANY-CD   PIC X.                            EL008
00232          10  WS-CM-CERT-DATA    PIC X(21).                        EL008
00233          10  WS-CM-CERT-NO      PIC X(11).                        EL008
00234                                                                   EL008
00235      05  WS-AM-CONTROL-PRIMARY.                                   EL008
00236          10  WS-AM-COMPANY-CD    PIC X(1).                        EL008
00237          10  WS-AM-CARRIER       PIC X(1).                        EL008
00238          10  WS-AM-GROUPING      PIC X(6).                        EL008
00239          10  WS-AM-STATE         PIC X(2).                        EL008
00240          10  WS-AM-ACCOUNT       PIC X(10).                       EL008
00241          10  WS-AM-EXPIRATION-DT PIC X(2).                        EL008
00242                                                                   EL008
00243      05  WS-BE-CONTROL-PRIMARY.                                   EL008
00244          10  WS-BE-COMPANY-CD    PIC X(01).                          CL**5
00245          10  WS-BE-RECORD-TYPE   PIC X(01).                          CL**5
00246          10  WS-BE-BENEFICIARY   PIC X(10).                          CL**5
00247                                                                   EL008
00248      05  WS-PM-CONTROL-PRIMARY.                                      CL*15
00249          10  WS-PM-COMPANY-CD    PIC X(01).                          CL*15
00250          10  WS-PM-POLICY-DATA   PIC X(21).                          CL*15
00251          10  WS-PM-REFERENCE-NO  PIC X(20).                          CL*15
00252                                                                      CL*15
00253      05  WS-PP-CONTROL-PRIMARY.                                      CL*15
00254          10  WS-PP-COMPANY-CD    PIC X(01).                          CL*15
00255          10  WS-PP-CARRIER       PIC X(01).                          CL*15
00256          10  WS-PP-GROUPING      PIC X(06).                          CL*15
00257          10  WS-PP-STATE         PIC X(02).                          CL*15
00258          10  WS-PP-PRODUCER      PIC X(10).                          CL*15
00259          10  WS-PP-PLAN-CODE     PIC X(02).                          CL*15
00260          10  WS-PP-REV-NO        PIC 9(03).                          CL*15
00261                                                                      CL*15
00262      05  WS-PD-CONTROL-PRIMARY.                                      CL*15
00263          10  WS-PD-COMPANY-CD    PIC X(01).                          CL*15
00264          10  WS-PD-CARRIER       PIC X(01).                          CL*15
00265          10  WS-PD-GROUPING      PIC X(06).                          CL*15
00266          10  WS-PD-STATE         PIC X(02).                          CL*15
00267          10  WS-PD-PRODUCER      PIC X(10).                          CL*15
00268          10  WS-PD-EXPIRATION-DT PIC X(02).                          CL*15
CIDMOD*---- THIS IS USED SO THAT EL008 WILL COMPILE, THE                     000
CIDMOD*---- PI-PRINTER-ID IS USED BY THE CHECK WRITER EL176 AND         ND   000
CIDMOD*---- EL177                                                            000
CIDMOD     05  WS-CHECK-ID.                                                  000
CIDMOD         10  PI-PRINTER-ID PIC X(04).                                  000
00269                                                                      CL*15
00270      EJECT                                                        EL008
00271  01  PRINT-CONTROL-WORK-AREA.                                     EL008
00272      05  WS-MM-DD-YY.                                             EL008
00273          10  WS-MM     PIC 99.                                    EL008
00274          10  WS-DD     PIC 99.                                    EL008
00275          10  WS-YY     PIC 99.                                    EL008
00276                                                                   EL008
00277      05  WS-TERM-DATE.                                            EL008
00278          10  WS-TERM-MM   PIC 999  VALUE ZEROS.                   EL008
00279          10  WS-TERM-YY   PIC 999  VALUE ZEROS.                   EL008
00280                                                                   EL008
00281      05  WS-REM-DATE.                                             EL008
00282          10  WS-REM-MM    PIC S999  VALUE ZEROS.                  EL008
00283                                                                   EL008
00284      05  WS-PAGE-CNT      PIC 9(4)   VALUE 1.                     EL008
00285                                                                   EL008
00286      05  WS-BIN-CURRENT-DT    PIC XX.                             EL008
00287                                                                   EL008
00288  01  HEAD-LINE-2.                                                 EL008
00289      05  FILLER             PIC X(31) VALUE SPACE.                EL008
00290      05  FILLER             PIC X(16) VALUE                       EL008
00291                             '- CLAIM STATUS -'.                   EL008
00292      05  FILLER             PIC X(19) VALUE SPACES.                  CL*15
00293      05  HEAD-REPORT-NO     PIC X(10) VALUE SPACES.                  CL*15
00294                                                                   EL008
00295  01  HEAD-LINE-3.                                                 EL008
00296      05  FILLER             PIC X(25) VALUE SPACE.                EL008
00297      05  HEAD-COMPANY       PIC X(30) VALUE SPACES.               EL008
00298      05  FILLER             PIC X(12) VALUE SPACES.                  CL*15
00299      05  HEAD-RUN-DATE      PIC X(8).                             EL008
00300                                                                   EL008
00301  01  HEAD-LINE-4.                                                 EL008
00302      05  FILLER             PIC X(31) VALUE SPACE.                EL008
00303      05  HEAD-RUN-DATE-FULL PIC X(18).                            EL008
00304      05  FILLER             PIC X(18) VALUE SPACES.                  CL*15
00305      05  FILLER             PIC X(4) VALUE 'PAGE'.                EL008
00306      05  HEAD-PAGE-NO       PIC ZZZ9.                             EL008
00307                                                                   EL008
00308  01  HEAD-LINE-7.                                                 EL008
00309      05  FILLER             PIC X(40) VALUE                       EL008
00310         'CLAIM NO CARR  CERT NO    TYPE  STATUS'.                    CL*17
00311      05  FILLER             PIC X(40) VALUE                       EL008
00312         'NAME OF INSURED                PROCESSOR'.               EL008
00313                                                                   EL008
00314  01  LINE-9.                                                      EL008
00315      05  FILLER             PIC X     VALUE SPACES.               EL008
00316      05  P-CLAIM-NO         PIC X(7).                             EL008
00317      05  FILLER             PIC X(3)  VALUE SPACES.               EL008
00318      05  P-CARR             PIC X.                                EL008
00319      05  FILLER             PIC X(2)  VALUE SPACES.               EL008
00320      05  P-CERT-NO          PIC X(11).                            EL008
00321      05  FILLER             PIC XX    VALUE SPACES.               EL008
00322      05  P-TYPE             PIC XXXX.                             EL008
00323      05  FILLER             PIC XX    VALUE  SPACES.              EL008
00324      05  P-STATUS           PIC X(6).                             EL008
00325      05  FILLER             PIC X(2)  VALUE   SPACES.             EL008
00326      05  P-NAME-GRP         PIC X(31).                            EL008
00327      05  P-NAME-ARR REDEFINES P-NAME-GRP                          EL008
00328                             OCCURS 31  INDEXED BY P-IND.          EL008
00329          10  P-NAME         PIC X.                                EL008
00330      05  FILLER             PIC X(3) VALUE  SPACES.               EL008
00331      05  P-PROCESSOR        PIC X(4).                             EL008
00332                                                                      CL*17
00333  01  HEAD-LINE-10.                                                   CL*17
00334      05  FILLER             PIC X(20) VALUE                          CL*17
00335         'CREDIT CARD NUMBER:'.                                       CL*17
00336      05  HEAD-CREDIT-CARD   PIC X(16).                               CL*17
00337                                                                   EL008
00338  01  LINE-12.                                                     EL008
00339      05  L-12-PD-THRU       PIC X(13) VALUE                          CL**5
00340          'PAID THRU  - '.                                         EL008
00341      05  P-PAID-THRU-DT     PIC X(8).                             EL008
00342      05  FILLER             PIC X(5)  VALUE  SPACES.              EL008
00343      05  FILLER             PIC X(15) VALUE                       EL008
00344          'LAST PMT AMT - '.                                       EL008
00345      05  P-LAST-PMT-AMT     PIC Z(7).99.                          EL008
00346      05  FILLER             PIC X(3)  VALUE  SPACES.              EL008
00347      05  FILLER             PIC X(16) VALUE                       EL008
00348          'LAST PMT DATE - '.                                      EL008
00349      05  P-LAST-PMT-DT      PIC X(8).                             EL008
00350                                                                   EL008
00351  01  LINE-13.                                                     EL008
00352      05  FILLER             PIC X(13) VALUE                       EL008
00353          'INCURRED   - '.                                         EL008
00354      05  P-INCURRED-DT      PIC X(8).                             EL008
00355      05  FILLER             PIC X(5) VALUE  SPACES.               EL008
00356      05  FILLER             PIC X(15) VALUE                       EL008
00357          'REPORTED     - '.                                       EL008
00358      05  P-REPORTED-DT      PIC X(8).                             EL008
00359      05  FILLER             PIC X(5) VALUE  SPACES.               EL008
00360      05  FILLER             PIC X(16) VALUE                       EL008
00361          'ESTABLISHED   - '.                                      EL008
00362      05  P-ESTAB-DT         PIC X(8).                             EL008
00363                                                                   EL008
00364  01  LINE-14.                                                     EL008
00365      05  FILLER             PIC X(13) VALUE                       EL008
00366          'NEXT AUTO  - '.                                         EL008
00367      05  P-NEXT-AUTO-DT     PIC X(8).                             EL008
00368      05  FILLER             PIC X(5) VALUE  SPACES.               EL008
00369      05  FILLER             PIC X(15) VALUE                       EL008
00370          'PMTS MADE    - '.                                       EL008
00371      05  P-PMTS-MADE        PIC XXX.                              EL008
00372      05  FILLER             PIC X(10) VALUE  SPACES.              EL008
00373      05  FILLER             PIC X(16) VALUE                       EL008
00374          'PREMIUM TYPE  - '.                                      EL008
00375      05  P-PREM-TYPE        PIC X(7).                             EL008
00376                                                                   EL008
00377  01  LINE-15.                                                     EL008
00378      05  FILLER             PIC X(13) VALUE                       EL008
00379          'COVERAGE   - '.                                         EL008
00380      05  P-COVERAGE         PIC X(10).                            EL008
00381      05  FILLER             PIC X(3) VALUE   SPACES.              EL008
00382      05  L15-ISSUE-LIT      PIC X(15) VALUE                          CL*15
00383          'CERT ISSUE   - '.                                       EL008
00384      05  P-CERT-ISSUE-DT    PIC X(8).                             EL008
00385      05  FILLER             PIC X(5) VALUE  SPACES.                  CL*11
00386      05  FILLER             PIC X(15) VALUE                          CL*11
00387          'ORIG BENEFIT  -'.                                          CL*11
00388      05  P-ORIG-BENEF-AMT   PIC Z(8).99.                          EL008
00389                                                                   EL008
00390  01  LINE-16.                                                     EL008
00391      05  FILLER             PIC X(13) VALUE                       EL008
00392          'EXPIRE DATE- '.                                         EL008
00393      05  P-EXPIRE           PIC X(12).                            EL008
00394      05  FILLER             PIC X(1)  VALUE  SPACES.              EL008
00395      05  FILLER             PIC X(15) VALUE                       EL008
00396          'TERM/REMAIN  - '.                                       EL008
00397      05  P-TERM             PIC ZZ9.                              EL008
00398      05  FILLER             PIC X     VALUE '/'.                  EL008
00399      05  P-REM              PIC ZZ9.                              EL008
00400      05  FILLER             PIC X(6)  VALUE  SPACES.              EL008
00401      05  L16-STATUS-LIT     PIC X(16) VALUE                          CL*15
00402          'CERT STATUS   - '.                                      EL008
00403      05  P-CERT-STAT        PIC X(8).                             EL008
00404                                                                   EL008
00405  01  LINE-17.                                                     EL008
00406      05  FILLER             PIC X(13) VALUE                       EL008
00407          'TOTAL PAID - '.                                         EL008
00408      05  P-TOT-PAID         PIC Z(8).99.                          EL008
00409      05  FILLER             PIC X(2)  VALUE  SPACES.              EL008
00410      05  FILLER             PIC X(15) VALUE                       EL008
00411          'CAUSE/DIAG   - '.                                       EL008
00412      05  P-CAUSE-DIAG       PIC X(26).                            EL008
00413                                                                   EL008
00414  01  LINE-19.                                                     EL008
00415      05  FILLER             PIC X(13) VALUE                       EL008
00416          'TOT EXPENSE- '.                                         EL008
00417      05  P-TOT-EXPENSE      PIC ZZZZZZZ.99.                       EL008
00418      05  FILLER             PIC X(3)  VALUE  SPACES.              EL008
00419      05  FILLER             PIC X(15) VALUE                       EL008
00420          'ORIG MANUAL  - '.                                       EL008
00421      05  P-ORIG-MANUAL      PIC ZZZZZZ.99.                        EL008
00422      05  FILLER             PIC X(4)  VALUE  SPACES.              EL008
00423      05  L19-ACCT-LIT       PIC X(16) VALUE                          CL*15
00424          'ACCOUNT       - '.                                      EL008
00425      05  P-ACCT             PIC X(10).                            EL008
00426                                                                   EL008
00427  01  LINE-20.                                                     EL008
00428      05  FILLER             PIC X(13) VALUE                       EL008
00429          'CHG EXPENSE- '.                                         EL008
00430      05  P-CHG-EXPENSE      PIC ZZZZZZ.99.                        EL008
00431      05  FILLER             PIC X(4)  VALUE  SPACES.              EL008
00432      05  FILLER             PIC X(15) VALUE                       EL008
00433          'REM MANUAL   - '.                                       EL008
00434      05  P-REM-MANUAL       PIC ZZZZZZ.99.                        EL008
00435      05  FILLER             PIC X(4)  VALUE  SPACES.              EL008
00436      05  FILLER             PIC X(16) VALUE                       EL008
00437          'STATE         - '.                                      EL008
00438      05  P-STATE            PIC X(8).                             EL008
00439                                                                   EL008
00440  01  LINE-21.                                                     EL008
00441      05  L21-CANC-LIT       PIC X(13) VALUE                          CL*15
00442          'CERT CANCEL- '.                                         EL008
00443      05  P-CERT-CANC-DT     PIC X(8).                             EL008
00444      05  FILLER             PIC X(5)  VALUE  SPACES.              EL008
00445      05  FILLER             PIC X(15) VALUE                       EL008
00446          'ADDL RESERVE - '.                                       EL008
00447      05  P-ADD-RESERVE      PIC ZZZZZZ.99.                        EL008
00448      05  FILLER             PIC X(4)  VALUE  SPACES.              EL008
00449      05  FILLER             PIC X(16) VALUE                       EL008
00450          'GROUPING      - '.                                      EL008
00451      05  P-GROUP            PIC X(8).                             EL008
00452                                                                   EL008
00453  01  LINE-22.                                                     EL008
00454      05  FILLER             PIC X(13) VALUE                       EL008
00455          'REIN CODE  - '.                                         EL008
00456      05  P-REIN-CODE        PIC X(3).                             EL008
00457      05  FILLER             PIC X(10) VALUE  SPACES.              EL008
00458      05  FILLER             PIC X(15) VALUE                       EL008
00459          'CERT BATCH   - '.                                       EL008
00460      05  P-CERT-BATCH       PIC X(6).                             EL008
00461      05  FILLER             PIC X(7)  VALUE  SPACES.              EL008
00462      05  L22-ENTRY-LIT      PIC X(16) VALUE                          CL*15
00463          'CERT ENTRY    - '.                                      EL008
00464      05  P-CERT-ENTRY.                                            EL008
00465          10  P-CERT-ENTRY-MM  PIC XX.                             EL008
00466          10  P-CERT-SL        PIC X   VALUE '/'.                  EL008
00467          10  P-CERT-ENTRY-YY  PIC XX.                             EL008
00468                                                                   EL008
00469  01  LINE-23.                                                     EL008
00470      05  FILLER             PIC X(13) VALUE                       EL008
00471          'MEMBER NO. - '.                                         EL008
00472      05  P-MEMBER-NO        PIC X(12).                            EL008
00473      05  FILLER             PIC X(16) VALUE                       EL008
00474          ' INSURED AGE  - '.                                      EL008
00475      05  P-INSURED-AGE      PIC XX.                               EL008
00476      05  FILLER             PIC X(11) VALUE SPACES.               EL008
00477      05  FILLER             PIC X(16) VALUE                       EL008
00478          'INSURED SEX   - '.                                      EL008
00479      05  P-INSURED-SEX      PIC XX.                               EL008
00480                                                                   EL008
00481  01  LINE-23A.                                                    EL008
00482      05  P-USER-HD          PIC X(13) VALUE                       EL008
00483          'USER CODE  - '.                                         EL008
00484      05  P-USER-CODE        PIC X.                                EL008
00485      05  FILLER             PIC X(12) VALUE SPACES.                  CL**6
00486      05  P-PURGED-STMT      PIC X(15) VALUE 'PURGED DATE  - '.       CL**6
00487      05  P-PURGED-DATE      PIC X(08) VALUE SPACES.                  CL**6
00488                                                                   EL008
00489  01  LINE-25.                                                     EL008
00490      05  FILLER             PIC X(41) VALUE                       EL008
00491          '- - CHRONOLOGICAL LISTING OF ACTIVITY - -'.             EL008
00492                                                                   EL008
00493  01  LINE-26.                                                     EL008
00494      05  FILLER             PIC X(20) VALUE                       EL008
00495          'TYPE    RECORDED  BY'.                                  EL008
00496 ****************                                                  EL008
091113 01  PAYMENT-DESCRIPTION-TABLE.
091113     12  FILLER              PIC X(11)   VALUE 'PARTIAL PMT'.
091113     12  FILLER              PIC X(11)   VALUE 'FINAL PMT  '.
091113     12  FILLER              PIC X(11)   VALUE 'LUMP SM PMT'.
091113     12  FILLER              PIC X(11)   VALUE 'ADDITNL PMT'.
091113     12  FILLER              PIC X(11)   VALUE 'CHGABLE EXP'.
091113     12  FILLER              PIC X(11)   VALUE 'NON-CHG EXP'.
091113     12  FILLER              PIC X(11)   VALUE 'LF PRM RFND'.
091113     12  FILLER              PIC X(11)   VALUE 'AH PRM RFND'.
091113     12  FILLER              PIC X(11)   VALUE 'ENTRY CORR '.
091113 01  PAYMENT-DESC-R   REDEFINES PAYMENT-DESCRIPTION-TABLE.
091113     12  PAY-DESC            PIC X(11)   OCCURS 9.

00497  01  P-PAY-LINE-1.                                                EL008
091113     05  P-PAY-ACT-TYPE     PIC X(11) VALUE SPACES.
091113*        'PAYMENT '.                                              EL008
091113     05  FILLER             PIC X(1)  VALUE SPACE.
00500      05  P-PAY-PMT-DT       PIC X(8).                             EL008
00501      05  FILLER             PIC X     VALUE SPACES.               EL008
00502      05  P-PAY-BY           PIC X(4).                             EL008
00503      05  FILLER             PIC X(10) VALUE                       EL008
00504          '  PAYEE - '.                                            EL008
00505      05  P-PAY-PAYEE        PIC X(30).                            EL008
00506      05  FILLER             PIC X(13) VALUE                       EL008
00507          '  PAYEE CD - '.                                         EL008
00508      05  P-PAY-PAYEE-CD     PIC X(02).                               CL**6
00509                                                                   EL008
00510  01  P-PAY-LINE-2.                                                EL008
00511      05  FILLER             PIC X(23) VALUE  SPACES.              EL008
00512      05  FILLER             PIC X(9)  VALUE                       EL008
00513          'AMOUNT - '.                                             EL008
00514      05  P-PAY-PMT-AMT      PIC Z(7).99.                          EL008
00515      05  FILLER             PIC X(10) VALUE                       EL008
00516          '  CHECK - '.                                            EL008
00517      05  P-PAY-CHECK        PIC X(7).                             EL008
00518      05  FILLER             PIC X(12) VALUE                       EL008
00519          '  WRITTEN - '.                                          EL008
00520      05  P-PAY-WRIT-DT      PIC X(8).                             EL008
00521                                                                   EL008
00522  01  P-PAY-LINE-3.                                                EL008
00523      05  FILLER             PIC X(23) VALUE  SPACES.              EL008
00524      05  FILLER             PIC X(9)  VALUE                       EL008
00525          'TYPE   - '.                                             EL008
00526      05  P-PAY-TYPE         PIC X(22).                            EL008
00527      05  FILLER             PIC X(11) VALUE                       EL008
00528          '  ORIGIN - '.                                           EL008
00529      05  P-PAY-ORIGIN       PIC X(7).                             EL008
00530                                                                   EL008
00531  01  P-PAY-LINE-4.                                                EL008
00532      05  FILLER             PIC X(23) VALUE  SPACES.              EL008
00533      05  FILLER             PIC X(9)  VALUE                       EL008
00534          'RESERVE- '.                                             EL008
00535      05  P-PAY-RESERVE      PIC Z(6).99.                          EL008
00536      05  FILLER             PIC X(12) VALUE                       EL008
00537          '  EXPENSE - '.                                          EL008
00538      05  P-PAY-EXPEN        PIC Z(5).99.                          EL008
00539      05  FILLER             PIC X(11) VALUE                       EL008
00540          '  CREDIT - '.                                           EL008
00541      05  P-PAY-CREDIT-DT    PIC X(8).                             EL008
00542                                                                   EL008
00543  01  P-PAY-LINE-5A.                                               EL008
00544      05  FILLER             PIC X(23) VALUE  SPACES.              EL008
00545      05  FILLER             PIC X(7)  VALUE                       EL008
00546          'VOID - '.                                               EL008
00547      05  P-PAY-VOID-DT      PIC X(8).                             EL008
00548      05  FILLER             PIC X(11) VALUE                       EL008
00549          '  REASON - '.                                           EL008
00550      05  P-PAY-REASON       PIC X(30).                            EL008
00551                                                                   EL008
00552  01  P-PAY-LINE-5B.                                               EL008
00553      05  FILLER             PIC X(23) VALUE  SPACES.              EL008
00554      05  FILLER             PIC X(7)  VALUE                       EL008
00555          'FROM - '.                                               EL008
00556      05  P-PAY-FROM-DT      PIC X(8).                             EL008
00557      05  L-5B-PD-THRU       PIC X(8)  VALUE                          CL**5
00558          ' THRU - '.                                              EL008
00559      05  P-PAY-THRU-DT      PIC X(8).                             EL008
00560      05  FILLER             PIC X(8)  VALUE                       EL008
00561          '  DAYS -'.                                              EL008
00562      05  P-PAY-DAYS         PIC ZZZ9.                             EL008
00563      05  FILLER             PIC X(8)  VALUE                       EL008
00564          '  RATE -'.                                              EL008
00565      05  P-PAY-RATE         PIC ZZ9.99.                           EL008
00566                                                                   EL008
00567  01  P-PAY-LINE-5C.                                               EL008
00568      05  FILLER             PIC X(23) VALUE  SPACES.              EL008
00569      05  FILLER             PIC X(15) VALUE 'EXPENSE TYPE - '.    EL008
00570      05  P-EXP-TYPE         PIC X.                                EL008
00571      05  FILLER             PIC X(41) VALUE SPACES.               EL008
00572                                                                   EL008
00573  01  P-LET-LINE-1.                                                EL008
091113     05  P-LET-ACT-TYPE     PIC X(11) VALUE SPACES.
091113*        'LETTER  '.                                              EL008
091113     05  FILLER             PIC X     VALUE  SPACE.               EL008
00576      05  P-LET-LET-DT       PIC X(8).                             EL008
00577      05  FILLER             PIC X     VALUE SPACE.                EL008
00578      05  P-LET-BY           PIC X(4).                             EL008
00579      05  FILLER             PIC X(14) VALUE                       EL008
00580          '  ADDRESSEE - '.                                        EL008
00581      05  P-LET-ADSEE        PIC X(30).                            EL008
00582      05  FILLER             PIC X(8) VALUE                        EL008
00583          ' CODE - '.                                              EL008
091113     05  P-LET-ADSEE-CD     PIC X(3).                             EL008
00585                                                                   EL008
00586  01  P-LET-LINE-2.                                                EL008
00587      05  FILLER             PIC X(23) VALUE  SPACES.              EL008
00588      05  FILLER             PIC X(7)  VALUE                       EL008
00589          'FORM - '.                                               EL008
00590      05  P-LET-FORM         PIC X(4).                             EL008
00591      05  FILLER             PIC X(9)  VALUE                       EL008
00592          '  SENT - '.                                             EL008
00593      05  P-LET-SENT-DT      PIC X(8).                             EL008
00594      05  FILLER             PIC X(12) VALUE                       EL008
00595          '  RE-SEND - '.                                          EL008
00596      05  P-LET-SEND-DT      PIC X(8).                             EL008
00597                                                                   EL008
00598  01  P-LET-LINE-3.                                                EL008
00599      05  FILLER             PIC X(23) VALUE  SPACES.              EL008
00600      05  FILLER             PIC X(12) VALUE                       EL008
00601          'FOLLOW UP - '.                                          EL008
00602      05  P-LET-FOL-DT       PIC X(8).                             EL008
00603      05  FILLER             PIC X(13) VALUE                       EL008
00604          '  ANSWERED - '.                                         EL008
00605      05  P-LET-ANS-DT       PIC X(8).                             EL008
00606                                                                   EL008
00607  01  P-LET-LINE-4.                                                EL008
00608      05  FILLER             PIC X(23) VALUE  SPACES.              EL008
00609      05  FILLER             PIC X(9)  VALUE                       EL008
00610          'ORIGIN - '.                                             EL008
00611      05  P-LET-ORIGIN       PIC X(7).                             EL008
00612      05  FILLER             PIC X(12) VALUE                       EL008
00613          '  ARCHIVE - '.                                          EL008
00614      05  P-LET-ARCH         PIC 9(6).                             EL008
00615                                                                   EL008
00616  01  P-LET-LINE-5.                                                EL008
00617      05  FILLER             PIC X(10) VALUE                       EL008
00618          '     RE - '.                                            EL008
00619      05  P-LET-RE           PIC X(70).                            EL008
00620                                                                   EL008
00621  01  P-NOT-LINE-1.                                                EL008
091113     05  P-NOT-ACT-TYPE     PIC X(11) VALUE  SPACES.
091113*        'NOTES   '.                                              EL008
091113     05  FILLER             PIC X     VALUE  SPACE.               EL008
00624      05  P-NOT-NOTE-DT      PIC X(8).                             EL008
00625      05  FILLER             PIC X     VALUE  SPACE.               EL008
00626      05  P-NOT-BY           PIC X(4).                             EL008
00627                                                                   EL008
00628  01  P-NOT-LINE-2.                                                EL008
00629      05  FILLER             PIC X(10) VALUE  SPACES.              EL008
00630      05  P-NOT-TEXT-1       PIC X(70).                            EL008
00631                                                                   EL008
00632  01  P-NOT-LINE-3.                                                EL008
00633      05  FILLER             PIC X(10) VALUE  SPACES.              EL008
00634      05  P-NOT-TEXT-2       PIC X(70).                            EL008
00635                                                                   EL008
00636  01  P-PRO-LINE-1.                                                EL008
091113     05  FILLER             PIC X(11) VALUE                       EL008
091113         'REMINDER   '.                                           EL008
091113     05  FILLER             PIC X     VALUE  SPACE.               EL008
00639      05  P-PRO-NOTE-DT      PIC X(8).                             EL008
00640      05  FILLER             PIC X     VALUE  SPACE.               EL008
00641      05  P-PRO-BY           PIC X(4).                             EL008
00642      05  FILLER             PIC X(17) VALUE                       EL008
00643          '  START NOTIFY - '.                                     EL008
00644      05  P-PRO-START-DT     PIC X(8).                             EL008
00645      05  FILLER             PIC X(15) VALUE                       EL008
00646          '  END NOTIFY - '.                                       EL008
00647      05  P-PRO-END-DT       PIC X(8).                             EL008
00648                                                                   EL008
00649  01  P-PRO-LINE-2.                                                EL008
00650      05  FILLER             PIC X(10) VALUE  SPACES.              EL008
00651      05  P-PRO-TEXT-1       PIC X(70).                            EL008
00652                                                                   EL008
00653  01  P-PRO-LINE-3.                                                EL008
00654      05  FILLER             PIC X(10) VALUE  SPACES.              EL008
00655      05  P-PRO-TEXT-2       PIC X(70).                            EL008
00656                                                                   EL008
00657  01  P-DEN-LINE-1.                                                EL008
091113     05  FILLER             PIC X(11) VALUE                       EL008
091113         'DENIAL     '.                                           EL008
091113     05  FILLER             PIC X     VALUE  SPACE.               EL008
00660      05  P-DEN-DEN-DT       PIC X(8).                             EL008
00661      05  FILLER             PIC X     VALUE  SPACE.               EL008
00662      05  P-DEN-BY           PIC X(4).                             EL008
00663      05  FILLER             PIC X(17) VALUE                       EL008
00664          '  RECONSIDERED - '.                                     EL008
00665      05  P-DEN-RECON-DT     PIC X(8).                             EL008
00666      05  FILLER             PIC X(7)  VALUE                       EL008
00667          '  CODE-'.                                               EL008
00668      05  P-DEN-CODE         PIC X(4).                             EL008
00669                                                                   EL008
00670  01  P-DEN-LINE-2.                                                EL008
00671      05  FILLER             PIC X(10) VALUE  SPACES.              EL008
00672      05  P-DEN-TEXT-1       PIC X(60).                            EL008
00673                                                                   EL008
00674  01  P-DEN-LINE-3.                                                EL008
00675      05  FILLER             PIC X(10) VALUE  SPACES.              EL008
00676      05  P-DEN-TEXT-2       PIC X(60).                            EL008
00677                                                                   EL008
00678  01  P-CHG-LINE-1.                                                EL008
091113     05  FILLER             PIC X(11) VALUE                       EL008
091113         'INCUR CHG  '.                                           EL008
091113     05  FILLER             PIC X     VALUE  SPACE.               EL008
00681      05  P-CHG-REC-DT       PIC X(8).                             EL008
00682      05  FILLER             PIC X     VALUE SPACES.               EL008
00683      05  P-CHG-BY           PIC X(4).                             EL008
00684      05  FILLER             PIC X(1)  VALUE  SPACES.              EL008
00685      05  FILLER             PIC X(8)  VALUE                       EL008
00686          'INCURED-'.                                              EL008
00687      05  P-CHG-INC-DT       PIC X(8).                             EL008
00688      05  FILLER             PIC X     VALUE  SPACES.              EL008
00689      05  L-1-PD-THRU        PIC X(8)  VALUE                          CL**5
00690          'PD THRU-'.                                              EL008
00691      05  P-CHG-PAID-TO-DT   PIC X(8).                             EL008
00692      05  FILLER             PIC X(15) VALUE                       EL008
00693          '  INIT MAN RES-'.                                       EL008
00694      05  P-CHG-INIT-RES     PIC Z(7).99.                          EL008
00695                                                                   EL008
00696  01  P-CHG-LINE-2.                                                EL008
00697      05  FILLER             PIC X(22) VALUE  SPACES.              EL008
00698      05  FILLER             PIC X(9)  VALUE                       EL008
00699          'REPORTED-'.                                             EL008
00700      05  P-CHG-REP-DT       PIC X(8).                             EL008
00701      05  FILLER             PIC X(9)  VALUE                       EL008
00702          ' TOT PD -'.                                             EL008
00703      05  P-CHG-TOT-PD       PIC Z(5).99.                          EL008
00704      05  FILLER             PIC X(14) VALUE                       EL008
00705          ' CUR MAN RES -'.                                        EL008
00706      05  P-CHG-CUR-RES      PIC Z(7).99.                          EL008
00707                                                                   EL008
00708  01  P-CHG-LINE-3.                                                EL008
00709      05  FILLER             PIC X(13) VALUE                       EL008
00710          'TOT EXPENSE -'.                                         EL008
00711      05  P-CHG-TOT-EXP      PIC Z(6).99.                          EL008
00712      05  FILLER             PIC X(10) VALUE                       EL008
00713          ' CREATED -'.                                            EL008
00714      05  P-CHG-CREAT-DT     PIC X(8).                             EL008
00715      05  FILLER             PIC X(10) VALUE                       EL008
00716          '  DAYS PD-'.                                            EL008
00717      05  P-CHG-DAYS-PD      PIC Z(5).                             EL008
00718      05  FILLER             PIC X(5)  VALUE  SPACE.               EL008
00719      05  FILLER             PIC X(10) VALUE                       EL008
00720          'ADD- RES -'.                                            EL008
00721      05  P-CHG-ADD-RES      PIC Z(7).99.                          EL008
00722                                                                   EL008
00723  01  P-CHG-LINE-4.                                                EL008
00724      05  FILLER             PIC X(13) VALUE                       EL008
00725          'CHG EXPENSE -'.                                         EL008
00726      05  P-CHG-CHG-EXP      PIC Z(6).99.                          EL008
00727      05  FILLER             PIC X(10) VALUE                       EL008
00728          ' LAST PMT-'.                                            EL008
00729      05  P-CHG-LAST-PMT-DT  PIC X(8).                             EL008
00730      05  FILLER             PIC X     VALUE  SPACES.              EL008
00731      05  FILLER             PIC X(8)  VALUE                       EL008
00732          'PMTS   -'.                                              EL008
00733      05  P-CHG-PMTS         PIC ZZ9.                              EL008
00734      05  FILLER             PIC X(7)  VALUE   SPACES.             EL008
00735      05  FILLER             PIC X(17) VALUE                       EL008
00736          'TOT TRLRS   -    '.                                     EL008
00737      05  P-CHG-TOT-TRLRS    PIC ZZZ9.                             EL008
00738                                                                   EL008
00739  01  P-2-LINE-6.                                                  EL008
00740      05  FILLER             PIC X(28) VALUE                       EL008
00741          '- - OPEN / CLOSE HISTORY - -'.                          EL008
00742                                                                   EL008
00743  01  P-2-LINE-8.                                                  EL008
00744      05  FILLER             PIC X(27) VALUE                       EL008
00745          '  DATE    OPEN/CLOSE  CAUSE'.                           EL008
00746                                                                   EL008
00747  01  P-2-HIS-DETAIL.                                              EL008
00748      05  P-2-HIS-DATE       PIC X(8).                             EL008
00749      05  FILLER             PIC X(4)  VALUE SPACES.               EL008
00750      05  P-2-HIS-OPCL       PIC X(6).                             EL008
00751      05  FILLER             PIC X(4)  VALUE SPACES.               EL008
00752      05  P-2-HIS-CAUSE      PIC X(10).                            EL008
00753                                                                   EL008
00754  01  P-2-AUT-LINE-1.                                              EL008
00755      05  FILLER             PIC X(35) VALUE                       EL008
00756          '- - AUTOMATIC PAYMENT SCHEDULES - -'.                   EL008
00757                                                                   EL008
00758  01  P-2-AUT-LINE-2.                                              EL008
00759      05  FILLER             PIC X(22) VALUE                       EL008
00760          'ESTABLISHED ON      - '.                                EL008
00761      05  P-2-EST-DT         PIC X(8).                             EL008
00762      05  FILLER             PIC X(10)   VALUE SPACES.             EL008
00763      05  FILLER             PIC X(22)   VALUE                     EL008
00764          'ESTABLISHED BY      - '.                                EL008
00765      05  P-2-EST-BY         PIC X(8).                             EL008
00766                                                                   EL008
00767  01  P-2-AUT-LINE-3.                                              EL008
00768      05  FILLER             PIC X(22)   VALUE                     EL008
00769          'EFFECTIVE DATE      - '.                                EL008
00770      05  P-2-EFF-DT         PIC X(8).                             EL008
00771      05  FILLER             PIC X(10)   VALUE SPACES.             EL008
00772      05  FILLER             PIC X(22)   VALUE                     EL008
00773          'ENDED / REPLACED    - '.                                EL008
00774      05  P-2-END-DT         PIC X(8).                             EL008
00775                                                                   EL008
00776  01  P-2-AUT-LINE-4.                                              EL008
00777      05  P-2-1ST-PMT-DT     PIC X(22)   VALUE                        CL*11
00778          'FIRST PAYMENT DATE  - '.                                EL008
00779      05  P-2-1ST-PMT-ON     PIC X(8).                             EL008
00780      05  FILLER             PIC X(10)   VALUE SPACES.             EL008
00781      05  P-2-LST-PMT-DT     PIC X(22)   VALUE                        CL*11
00782          'LAST PAYMENT ON     - '.                                EL008
00783      05  P-2-LST-PMT-ON     PIC X(8).                             EL008
00784                                                                   EL008
00785  01  P-2-AUT-LINE-5.                                              EL008
00786      05  FILLER             PIC X(22)   VALUE                     EL008
00787          'FIRST PAYMENT AMT   - '.                                EL008
00788      05  P-2-1ST-PMT        PIC ZZZZ,ZZZ.99.                      EL008
00789      05  FILLER             PIC X(7)    VALUE SPACES.             EL008
00790      05  FILLER             PIC X(22)   VALUE                     EL008
00791          'REGULAR PAYMENT AMT - '.                                EL008
00792      05  P-2-REG-PMT        PIC ZZZZ,ZZZ.99.                      EL008
00793                                                                   EL008
00794  01  P-2-AUT-LINE-6.                                              EL008
00795      05  FILLER             PIC X(22)   VALUE                     EL008
00796          'DAYS IN 1ST PERIOD  - '.                                EL008
00797      05  P-2-DAYS-1ST       PIC 9(4).                             EL008
00798 *    05  FILLER             PIC X(14)   VALUE SPACES.             EL008
00799 *    05  FILLER             PIC X(22)   VALUE                     EL008
00800 *        'DAYS IN REGULAR PMT - '.                                EL008
00801 *    05  P-2-DAYS-REG       PIC X(3).                             EL008
00802                                                                   EL008
00803  01  P-2-AUT-LINE-7.                                              EL008
00804      05  FILLER             PIC X(22)   VALUE                     EL008
00805          'LAST PAYMENT FINAL  - '.                                EL008
00806      05  P-2-LAST-FINAL     PIC X(3).                             EL008
00807      05  FILLER             PIC X(15)   VALUE SPACES.             EL008
00808      05  FILLER             PIC X(22)   VALUE                     EL008
00809          'PAYEE               - '.                                EL008
00810      05  P-2-PAYEE          PIC X(17).                            EL008
00811                                                                   EL008
00812  01  P-2-AUT-LINE-8.                                              EL008
00813      05  FILLER             PIC X(40)   VALUE  SPACES.            EL008
00814      05  FILLER             PIC X(22)   VALUE                     EL008
00815          'MONTHS BETWEEN PMTS - '.                                EL008
00816      05  P-2-MOS-BET        PIC 9(3).                             EL008
00817                                                                   EL008
00818  01  P-2-ADD-LINE-1.                                              EL008
00819      05  FILLER             PIC X(25)   VALUE                     EL008
00820          '- - ADDRESSES ON FILE - -'.                             EL008
00821                                                                   EL008
00822  01  P-2-ADD-LINE-2.                                              EL008
00823      05  FILLER             PIC X(7)    VALUE                     EL008
00824          'TYPE - '.                                               EL008
00825      05  P-2-ADD-TYPE       PIC X(15).                            EL008
00826      05  FILLER             PIC X(2)    VALUE SPACES.             EL008
00827      05  FILLER             PIC X(7)    VALUE                     EL008
00828          'CODE - '.                                               EL008
00829      05  P-2-ADD-CODE       PIC X.                                EL008
00830      05  FILLER             PIC X(02)   VALUE SPACES.                CL*15
00831      05  FILLER             PIC X(15)   VALUE                     EL008
00832          'MAIL TO NAME - '.                                       EL008
00833      05  P-2-ADD-NAME       PIC X(30).                            EL008
00834                                                                   EL008
00835  01  P-2-ADD-LINE-3.                                              EL008
00836      05  FILLER             PIC X(34)   VALUE  SPACES.               CL*15
00837      05  FILLER             PIC X(13)   VALUE                     EL008
00838          'ADDRESS 1  - '.                                         EL008
00839      05  P-2-ADD-ADDR-1     PIC X(30).                            EL008
00840                                                                   EL008
00841  01  P-2-ADD-LINE-4.                                              EL008
00842      05  FILLER             PIC X(34)   VALUE  SPACES.               CL*15
00843      05  FILLER             PIC X(13)   VALUE                     EL008
00844          'ADDRESS 2  - '.                                         EL008
00845      05  P-2-ADD-ADDR-2     PIC X(30).                            EL008
00846                                                                   EL008
00847  01  P-2-ADD-LINE-5.                                              EL008
00848      05  FILLER             PIC X(34)   VALUE  SPACES.               CL*15
00849      05  FILLER             PIC X(13)   VALUE                     EL008
00850          'CITY STATE - '.                                         EL008
00851      05  P-2-ADD-CITY       PIC X(30).                            EL008
00852                                                                   EL008
00853  01  P-2-ADD-LINE-6.                                              EL008
00854      05  FILLER             PIC X(34)   VALUE  SPACES.               CL*15
00855      05  FILLER             PIC X(13)   VALUE                     EL008
00856          'ZIP  PHONE - '.                                         EL008
00857      05  P-2-ADD-ZIP        PIC X(10).                               CL*12
00858      05  FILLER             PIC X(8)    VALUE   SPACES.              CL*12
00859      05  P-2-ADD-PHONE      PIC X(12).                            EL008
00860                                                                   EL008
00861                                                                   EL008
00862  01  P-FORM-LINE-1.                                               EL008
091113     05  FILLER              PIC X(11)  VALUE                     EL008
091113         'FORM CTL'.                                              EL008
091113     05  FILLER             PIC X     VALUE  SPACE.               EL008
00865      05  P-FORM-LET-DT       PIC X(8).                            EL008
00866      05  FILLER              PIC X      VALUE SPACE.              EL008
00867      05  P-FORM-BY           PIC X(4).                            EL008
00868      05  FILLER              PIC X(14)  VALUE                     EL008
00869          '  ADDRESSEE - '.                                        EL008
00870      05  P-FORM-ADSEE        PIC X(30).                           EL008
00871      05  FILLER              PIC X(8)   VALUE                     EL008
00872          ' CODE - '.                                              EL008
091113     05  P-FORM-ADSEE-CD     PIC X(3).                            EL008
00874                                                                   EL008
00875  01  P-FORM-LINE-2.                                               EL008
00876      05  FILLER              PIC X(23)  VALUE  SPACES.            EL008
00877      05  FILLER              PIC X(7)   VALUE                     EL008
00878          'FORM - '.                                               EL008
00879      05  P-FORM-FORM         PIC X(4).                            EL008
00880      05  FILLER              PIC X(9)   VALUE                     EL008
00881          '  SENT - '.                                             EL008
00882      05  P-FORM-SENT-DT      PIC X(8).                            EL008
00883      05  FILLER              PIC X(12)  VALUE                     EL008
00884          '  RE-SEND - '.                                          EL008
00885      05  P-FORM-SEND-DT      PIC X(8).                            EL008
00886                                                                   EL008
00887  01  P-FORM-LINE-3.                                               EL008
00888      05  FILLER              PIC X(23)  VALUE  SPACES.            EL008
00889      05  FILLER              PIC X(12)  VALUE                     EL008
00890          'FOLLOW UP - '.                                          EL008
00891      05  P-FORM-FOL-DT       PIC X(8).                            EL008
00892      05  FILLER              PIC X(25)  VALUE                        CL**5
00893          '     CLAIMANT ANSWERED - '.                                CL**5
00894      05  P-CLM-FORM-ANS-DT   PIC X(8).                               CL**5
00895                                                                      CL**5
00896  01  P-FORM-LINE-4.                                                  CL**5
00897      05  FILLER              PIC X(23)  VALUE  SPACES.               CL**5
00898      05  P-PHY-FORM-COMM     PIC X(17)  VALUE                        CL**5
00899          'PHY.  ANSWERED - '.                                        CL**8
00900      05  P-PHY-FORM-ANS-DT   PIC X(8).                               CL**5
00901      05  P-EMP-FORM-COMM     PIC X(20)  VALUE                        CL**5
00902          '   EMP.  ANSWERED - '.                                     CL**5
00903      05  P-EMP-FORM-ANS-DT   PIC X(8).                               CL**5
00904                                                                   EL008
00905  01  P-FORM-LINE-5.                                               EL008
00906      05  FILLER             PIC X(23)   VALUE SPACES.             EL008
00907      05  FILLER             PIC X(15)   VALUE                     EL008
00908          'INSTRUCTIONS - '.                                       EL008
00909      05  P-FORM-INSTRUCT    PIC X(28).                            EL008
00910                                                                   EL008
00911  01  P-FORM-LINE-6.                                               EL008
00912      05  FILLER             PIC X(38)   VALUE SPACES.             EL008
00913      05  P-FORM-INSTRUCT-1  PIC X(28).                            EL008
00914                                                                   EL008
00915  01  P-FORM-LINE-7.                                               EL008
00916      05  FILLER             PIC X(23)   VALUE SPACES.             EL008
00917      05  FILLER             PIC X(16)   VALUE                     EL008
00918          'RELATED CLAIM - '.                                      EL008
00919      05  P-FORM-CLAIM       PIC X(7).                             EL008
00920      05  FILLER             PIC X(12)   VALUE                     EL008
00921          '  CARRIER - '.                                          EL008
00922      05  P-FORM-CARRIER     PIC X.                                EL008
00923      05  FILLER             PIC X(9)    VALUE                     EL008
00924          '  CERT - '.                                             EL008
00925      05  P-FORM-CERT        PIC X(8).                             EL008
00926                                                                   EL008
00927  01  AUTO-PAY-TABLE.                                              EL008
00928      05  AUTO-PAY-RECORD    OCCURS 10 TIMES  INDEXED BY AP-INDEX. EL008
00929          10  AP-TBL-EST-DT             PIC XX.                    EL008
00930          10  AP-TBL-EST-BY             PIC XXXX.                  EL008
00931          10  AP-TBL-SCHED-START-DT     PIC XX.                    EL008
00932          10  AP-TBL-SCHED-END-DT       PIC XX.                    EL008
00933          10  AP-TBL-TERM-DT            PIC XX.                    EL008
00934          10  AP-TBL-LAST-TYPE          PIC X.                     EL008
00935          10  AP-TBL-FIRST-AMT          PIC S9(7)V99 COMP-3.       EL008
00936          10  AP-TBL-FIRST-DAYS         PIC S9(4) COMP.            EL008
00937          10  AP-TBL-FIRST-DT           PIC XX.                    EL008
00938          10  AP-TBL-REG-AMT            PIC S9(7)V99  COMP-3.      EL008
00939          10  AP-TBL-REG-MO             PIC S9(4) COMP.            EL008
00940          10  AP-TBL-INT-MO             PIC XX.                    EL008
00941                                                                   EL008
00942  01  ADDRESS-TABLE.                                               EL008
00943      05  ADDRESS-RECORD    OCCURS 60 TIMES  INDEXED BY AD-INDEX.     CL**6
00944          10  AD-TBL-TYPE               PIC X.                     EL008
00945          10  AD-TBL-NAME               PIC X(30).                 EL008
00946          10  AD-TBL-ADDR-1             PIC X(30).                 EL008
00947          10  AD-TBL-ADDR-2             PIC X(30).                 EL008
00948          10  AD-TBL-CITY               PIC X(30).                 EL008
00949          10  AD-TBL-ZIP                PIC X(10).                    CL*12
00950          10  AD-TBL-PHONE              PIC 9(11) COMP-3.          EL008
00951                                                                   EL008
00952  01  OPEN-CLOSE-TABLE.                                            EL008
00953      05  AUTO-PAY-RECORD    OCCURS 6 TIMES  INDEXED BY OC-INDEX.  EL008
00954          10  OC-TBL-OPCL-DT            PIC XX.                    EL008
00955          10  OC-TBL-OPCL-TYPE          PIC X.                     EL008
00956          10  OC-TBL-OPCL-REASON        PIC X(5).                  EL008
00957      EJECT                                                        EL008
00958                                      COPY ELCDMD34.                  CL*18
00959      EJECT                                                           CL*18
00960                                      COPY ELCAID.                    CL*13
00961  01  PF-AID REDEFINES DFHAID.                                     EL008
00962      05  FILLER                      PIC X(8).                    EL008
00963      05  PF-VALUES  OCCURS 24    PIC X.                           EL008
00964      EJECT                                                        EL008
00965                                  COPY ELCALGND.                      CL*13
00966      EJECT                                                        EL008
00967                                  COPY ELPRTCVD.                      CL*13
00968      EJECT                                                        EL008
00969                                  COPY ELCINTF.                       CL*13
00970      12  WS-INT-BLK REDEFINES PI-PROGRAM-WORK-AREA.                  CL**2
00971          16  FILLER                 PIC X.                           CL*12
00972          16  WS-PI-NAME             PIC X(30).                       CL*12
00973          16  FILLER                 PIC X(609).                      CL*17
00974                                                                      CL**2
00975      EJECT                                                        EL008
00976                                  COPY ELCATTR.                       CL*13
00977      EJECT                                                        EL008
00978                                  COPY ELCDATE.                       CL*13
00979      EJECT                                                        EL008
00980                                  COPY ELCEMIB.                       CL*13
00981      EJECT                                                        EL008
00982                                  COPY ELCCALC.                       CL*13
00983      EJECT                                                        EL008
00984  LINKAGE SECTION.                                                 EL008
00985  01  DFHCOMMAREA                 PIC X(1024).                     EL008
00986 *01 PARM-LIST .                                                      CL*17
00987 *    05  FILLER                  PIC S9(8) COMP.                     CL*17
00988 *    05  CNTL-POINTER            PIC S9(8) COMP.                     CL*17
00989 *    05  MSTR-POINTER            PIC S9(8) COMP.                     CL*17
00990 *    05  TRLR-POINTER            PIC S9(8) COMP.                     CL*17
00991 *    05  CERT-POINTER            PIC S9(8) COMP.                     CL*17
00992 *    05  ACTQ-POINTER            PIC S9(8) COMP.                     CL*17
00993 *    05  ACCT-POINTER            PIC S9(8) COMP.                     CL*17
00994 *    05  BENE-POINTER            PIC S9(8) COMP.                     CL*17
00995 *    05  PLCY-POINTER            PIC S9(8) COMP.                     CL*17
00996 *    05  PLAN-POINTER            PIC S9(8) COMP.                     CL*17
00997 *    05  PROD-POINTER            PIC S9(8) COMP.                     CL*17
00998      EJECT                                                        EL008
00999                                  COPY ELCCNTL.                       CL*13
01000      EJECT                                                        EL008
01001                                  COPY ELCMSTR.                       CL*13
01002      EJECT                                                        EL008
01003                                  COPY ELCTRLR.                       CL*13
01004      EJECT                                                        EL008
01005                                  COPY ELCCERT.                       CL*13
01006      EJECT                                                        EL008
01007                                  COPY ELCACTQ.                       CL*13
01008      EJECT                                                        EL008
01009                                  COPY ERCACCT.                       CL*13
01010      EJECT                                                        EL008
01011                                  COPY ELCBENE.                       CL*13
01012      EJECT                                                        EL008
01013                                  COPY MPCPLCY.                       CL*15
01014      EJECT                                                           CL*15
01015                                  COPY MPCPLAN.                       CL*15
01016      EJECT                                                           CL*15
01017                                  COPY MPCPROD.                       CL*15
01018      EJECT                                                           CL*15
01019  PROCEDURE DIVISION.                                              EL008
01020      MOVE EIBDATE               TO DC-JULIAN-YYDDD.               EL008
01021      MOVE '5'                   TO DC-OPTION-CODE.                EL008
01022      PERFORM 8100-DATE-RTN  THRU  8100-EXIT.                      EL008
01023      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.                    EL008
01024      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.                EL008
01025                                                                   EL008
01026  0001-PROCESSING-EXITS.                                           EL008
01027 *    MOVE DFHCOMMAREA  TO  PROGRAM-INTERFACE-BLOCK.               EL008
01028                                                                   EL008
01029      EXEC CICS  HANDLE CONDITION                                  EL008
01030             ERROR    (8300-ABEND)                                 EL008
01031             PGMIDERR (8900-PGMIDERR)                              EL008
01032             ENDDATA  (9999-FINALIZE)                              EL008
01033      END-EXEC.                                                    EL008
01034                                                                   EL008
01035      MOVE SPACES                 TO DL34-PROCESS-TYPE.               CL*18
01036                                                                      CL*18
01037  0002-PROCESSING-MAINLINE.                                        EL008
01038      EXEC CICS  RETRIEVE                                          EL008
01039                 INTO    (PROGRAM-INTERFACE-BLOCK)                 EL008
01040                 LENGTH  (PI-COMM-LENGTH)                          EL008
01041      END-EXEC.                                                    EL008
01042                                                                      CL*18
01043 * DLO034 OPEN WHEN DMD OR CID                                        CL*18
01044      MOVE PI-COMPANY-ID          TO WS-SAVED-PI-COMPANY-ID.          CL*18
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'                               CL*18
01046          IF DL34-PROCESS-TYPE IS EQUAL TO SPACES                     CL*18
01047              MOVE 'O'                TO DL34-PROCESS-TYPE            CL*18
01048              MOVE PI-COMPANY-ID      TO DL34-COMPANY-ID              CL*18
01049              MOVE THIS-PGM           TO DL34-PRINT-PROGRAM-ID        CL*18
01050              MOVE PI-PROCESSOR-ID    TO DL34-USERID                  CL*18
01051              MOVE SPACES             TO DL34-PRINT-LINE              CL*18
01052              MOVE PI-ALT-DMD-PRT-ID  TO DL34-OVERRIDE-PRINTER-ID     CL*18
01053                                                                      CL*18
01054              EXEC CICS LINK                                          CL*18
01055                  PROGRAM    ('DLO034')                               CL*18
01056                  COMMAREA   (DLO034-COMMUNICATION-AREA)              CL*18
01057                  LENGTH     (DLO034-REC-LENGTH)                      CL*18
01058              END-EXEC                                                CL*18
01059                                                                      CL*18
01060              IF DL34-RETURN-CODE NOT = 'OK'                          CL*18
01061                  MOVE  '**DLO034 OPEN ERROR - ABORT**'               CL*18
01062                                  TO WS-PASSED-DATA                   CL*18
01063                  GO TO 9999-FINALIZE.                                CL*18
01064                                                                   EL008
01065      PERFORM 4000-INITIALIZE  THRU 4000-EXIT.                     EL008
01066                                                                   EL008
01067      IF PI-CALLING-PROGRAM  = 'EL180'                             EL008
01068          MOVE 'EL180 ONL'        TO  HEAD-REPORT-NO               EL008
01069          MOVE LOW-VALUES         TO  WS-AQ-CONTROL-PRIMARY        EL008
01070          MOVE PI-COMPANY-CD      TO  WS-AQ-COMPANY-CD             EL008
01071          PERFORM  0020-PROCESS-ACTQ  THRU 0020-EXIT               EL008
01072           UNTIL  END-OF-ACTQ-FILE                                 EL008
01073      ELSE                                                         EL008
01074          PERFORM  0021-PROCESS-PI    THRU 0021-EXIT.              EL008
01075                                                                   EL008
01076      GO TO 0002-PROCESSING-MAINLINE.                              EL008
01077                                                                   EL008
01078  0002-EXIT.                                                       EL008
01079      EJECT                                                        EL008
01080                                                                   EL008
01081  0020-PROCESS-ACTQ.                                               EL008
01082      PERFORM  1450-BROWSE-ACTQ   THRU 1450-EXIT.                  EL008
01083                                                                   EL008
01084  0020-READNEXT-ACTQ.                                              EL008
01085      PERFORM 1400-READNEXT-ACTQ     THRU 1400-EXIT.               EL008
01086                                                                   EL008
01087      IF END-OF-ACTQ-FILE                                          EL008
01088          MOVE TPG      TO WS-PASSED-CNTL-CHAR                     EL008
01089          MOVE  SPACES  TO WS-PASSED-DATA                          EL008
01090          PERFORM 1455-ENDBR-ACTQ  THRU 1455-EXIT                  EL008
01091          GO TO 0020-EXIT.                                         EL008
01092                                                                   EL008
01093      IF ACTQ-READ-ERROR                                           EL008
01094          MOVE    '*** ERROR ON READ ACTQ ***' TO WS-PASSED-DATA   EL008
01095          GO TO 9999-FINALIZE.                                     EL008
01096                                                                   EL008
01097      IF PENDING-FULL-PRINT OR                                     EL008
01098         PENDING-PART-PRINT                                        EL008
01099          NEXT SENTENCE                                            EL008
01100      ELSE                                                         EL008
01101          GO TO 0020-READNEXT-ACTQ.                                EL008
01102                                                                   EL008
01103      IF WS-AQ-CONTROL-PRIMARY = WS-LAST-AQ-KEY                    EL008
01104         GO TO 0020-READNEXT-ACTQ.                                 EL008
01105                                                                   EL008
01106      MOVE WS-AQ-CONTROL-PRIMARY TO WS-LAST-AQ-KEY.                EL008
01107                                                                   EL008
01108      EXEC CICS ENDBR                                              EL008
01109           DATASET   ('ELACTQ')                                    EL008
01110      END-EXEC.                                                    EL008
01111                                                                   EL008
01112      EXEC CICS READ                                               EL008
01113           DATASET   ('ELACTQ')                                    EL008
01114           RIDFLD    (WS-AQ-CONTROL-PRIMARY)                       EL008
01115           SET       (ADDRESS OF ACTIVITY-QUE)                        CL*17
01116      END-EXEC.                                                    EL008
01117                                                                   EL008
01118      MOVE WS-PI-NAME              TO WS-UNALIGNED-FIELD.          EL008
01119      MOVE SPACES                  TO WS-ALIGNED-FIELD.            EL008
01120      MOVE +30                     TO WS-NAME-LENGTH.              EL008
01121      PERFORM ELCALGNP             THRU ELCALGNP-EXIT.             EL008
01122      MOVE WS-ALIGNED-FIELD        TO HEAD-COMPANY.                EL008
01123                                                                   EL008
01124 **   BUILD MSTR KEY                                               EL008
01125      MOVE  AQ-CONTROL-PRIMARY  TO WS-CL-CONTROL-PRIMARY.          EL008
01126                                                                   EL008
01127 ***  BUILD TRLR KEY                                               EL008
01128      MOVE AQ-CONTROL-PRIMARY TO  WS-AT-CONTROL-PRIMARY.           EL008
01129      MOVE ZEROS              TO  WS-AT-SEQ-NO.                    EL008
01130                                                                   EL008
01131      IF PENDING-FULL-PRINT                                        EL008
01132          MOVE 'X' TO WS-FULL-PRINT-SW.                            EL008
01133                                                                   EL008
01134      PERFORM  0022-PROCESS-3-FILES  THRU 0022-EXIT.               EL008
01135                                                                   EL008
01136      MOVE AQ-CONTROL-PRIMARY  TO WS-AQ-CONTROL-PRIMARY.           EL008
01137      PERFORM 1480-READ-UPDATE-ACTQ   THRU 1480-EXIT.              EL008
01138      MOVE SPACES              TO AQ-PENDING-STATUS-FLAG.          EL008
01139                                                                   EL008
01140      IF AQ-PENDING-LETTER-FLAG   = SPACE  AND                     EL008
01141         AQ-PENDING-PAYMENT-FLAG  = SPACE                          EL008
01142          PERFORM 1800-DELETE-ACTQ  THRU 1800-EXIT                 EL008
01143      ELSE                                                         EL008
01144          PERFORM 1850-REWRITE-ACTQ  THRU 1850-EXIT.                  CL**8
01145                                                                   EL008
01146      MOVE 'X'                    TO  WS-PROG-END.                    CL*10
CIDMOD     IF PI-COMPANY-ID = 'DMD' OR 'XXX'
CIDMOD        CONTINUE
CIDMDO     ELSE
CIDMOD        PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         CL*10
01148      EXEC CICS SYNCPOINT                                             CL*10
01149          END-EXEC.                                                   CL*10
01150                                                                   EL008
01151  0020-EXIT.                                                       EL008
01152       EXIT.                                                       EL008
01153      EJECT                                                        EL008
01154  0021-PROCESS-PI.                                                 EL008
01155 **     READ CNTL RECORD TO OBTAIN COMPANY NAME                    EL008
01156      MOVE PI-COMPANY-ID        TO WS-CF-COMPANY-ID.               EL008
01157      MOVE SPACES               TO WS-CF-PROCESSOR.                EL008
01158      MOVE ZEROS                TO WS-CF-SEQUENCE-NO.              EL008
01159      MOVE '1'                  TO WS-CF-RECORD-TYPE.              EL008
01160                                                                   EL008
01161      MOVE SPACES               TO WS-CNTL-ERROR-SW.               EL008
01162      PERFORM 1300-READ-CNTL       THRU 1300-EXIT.                 EL008
01163                                                                   EL008
01164      IF NO-COMPANY-RECORD                                         EL008
01165          MOVE    '*** ERROR NO COMP REC  ***' TO WS-PASSED-DATA   EL008
01166          GO TO 9999-FINALIZE.                                     EL008
01167                                                                   EL008
01168      MOVE CF-CL-MAIL-TO-NAME      TO WS-UNALIGNED-FIELD.          EL008
01169      MOVE SPACES                  TO WS-ALIGNED-FIELD.            EL008
01170      MOVE +30                     TO WS-NAME-LENGTH.              EL008
01171      PERFORM ELCALGNP              THRU ELCALGNP-EXIT.            EL008
01172      MOVE WS-ALIGNED-FIELD         TO HEAD-COMPANY.               EL008
01173                                                                   EL008
01174      MOVE SPACES TO   END-OC-TABLE-SW                             EL008
01175                       END-AD-TABLE-SW                             EL008
01176                       END-AP-TABLE-SW.                            EL008
01177                                                                   EL008
01178      IF PI-CALLING-PROGRAM IS EQUAL TO 'EL1602'                      CL*15
01179          MOVE 'EL1602 ONL'       TO  HEAD-REPORT-NO                  CL*15
01180      ELSE                                                            CL*15
01181          MOVE 'EL150 ONL'        TO  HEAD-REPORT-NO.                 CL*15
01182                                                                   EL008
01183 *       BUILD TRAILER KEY AND MSTR KEY                            EL008
01184      MOVE PI-COMPANY-CD      TO  WS-AT-COMPANY-CD                 EL008
01185                                  WS-CL-COMPANY-CD.                EL008
01186      MOVE PI-CARRIER         TO  WS-AT-CARRIER                    EL008
01187                                  WS-CL-CARRIER.                   EL008
01188      MOVE PI-CLAIM-NO        TO  WS-AT-CLAIM-NO                   EL008
01189                                  WS-CL-CLAIM-NO.                  EL008
01190      MOVE PI-CERT-NO         TO  WS-AT-CERT-NO                    EL008
01191                                  WS-CL-CERT-NO.                   EL008
01192      MOVE ZEROS              TO  WS-AT-SEQ-NO.                    EL008
01193                                                                   EL008
01194      IF PI-ENTRY-CD-1 NOT  = '1'                                  EL008
01195          MOVE 'X'  TO WS-FULL-PRINT-SW.                           EL008
01196                                                                   EL008
01197      PERFORM 0022-PROCESS-3-FILES   THRU 0022-EXIT.               EL008
01198                                                                   EL008
01199  0021-EXIT.                                                       EL008
01200       EXIT.                                                       EL008
01201      EJECT                                                        EL008
01202  0022-PROCESS-3-FILES.                                            EL008
01203      PERFORM  1550-READ-MSTR  THRU 1550-EXIT.                     EL008
01204                                                                   EL008
01205      IF MSTR-READ-ERROR                                           EL008
01206          MOVE    '*** ERROR ON READ MSTR ***' TO WS-PASSED-DATA   EL008
01207          MOVE ' '                TO AQ-PENDING-PAYMENT-FLAG          CL*13
01208                                     AQ-PENDING-LETTER-FLAG           CL*13
01209                                     AQ-PENDING-STATUS-FLAG           CL*13
01210                                     WS-MSTR-READ-ERROR-SW            CL*13
01211          GO TO 0022-EXIT.                                            CL*13
01212                                                                   EL008
01213      MOVE SPACES                 TO P-CAUSE-DIAG.                    CL**5
01214      MOVE WS-CL-CONTROL-PRIMARY  TO WS-AT-CONTROL-PRIMARY.           CL**5
01215      MOVE +90   TO WS-AT-SEQ-NO.                                     CL**5
01216                                                                      CL**5
01217      EXEC CICS  HANDLE CONDITION                                     CL**5
01218             NOTFND  (0022-BYPASS)                                    CL**5
01219      END-EXEC.                                                       CL**5
01220                                                                      CL**5
01221      EXEC CICS READ                                                  CL**5
01222           DATASET ('ELTRLR')                                         CL**5
01223           SET     (ADDRESS OF ACTIVITY-TRAILERS)                     CL*17
01224           RIDFLD  (WS-AT-CONTROL-PRIMARY)                            CL**5
01225      END-EXEC.                                                       CL**5
01226                                                                      CL**5
01227      MOVE AT-INFO-LINE-1         TO P-CAUSE-DIAG.                    CL**5
01228                                                                      CL**5
01229  0022-BYPASS.                                                        CL**5
01230                                                                      CL**5
01231      MOVE 0 TO WS-PAGE-CNT.                                       EL008
01232                                                                   EL008
01233      SET AP-INDEX   TO 1.                                         EL008
01234      SET AD-INDEX   TO 1.                                         EL008
01235      SET OC-INDEX   TO 1.                                         EL008
01236                                                                   EL008
01237      MOVE +0    TO WS-AT-SEQ-NO.                                     CL**5
01238      PERFORM 2110-MOVE-CLAIM-INFO  THRU 2110-EXIT.                EL008
01239                                                                   EL008
01240 ***  BUILDIN CERT KEY                                             EL008
01241                                                                   EL008
01242      IF CL-SYSTEM-IDENTIFIER IS EQUAL TO 'CV'                        CL*15
01243          MOVE CL-COMPANY-CD      TO  WS-PM-COMPANY-CD                CL*15
01244          MOVE CL-CERT-KEY-DATA   TO  WS-PM-POLICY-DATA               CL*15
01245          MOVE CL-CV-REFERENCE-NO TO  WS-PM-REFERENCE-NO              CL*15
01246      ELSE                                                         EL008
01247          MOVE CL-COMPANY-CD      TO  WS-CM-COMPANY-CD                CL*15
01248          MOVE CL-CERT-KEY-DATA   TO  WS-CM-CERT-DATA                 CL*15
01249          MOVE CL-CERT-NO         TO  WS-CM-CERT-NO.                  CL*15
01250                                                                      CL*15
01251 ***  READ CERT/POLICY MASTER                                         CL*15
01252                                                                      CL*15
01253      IF CL-SYSTEM-IDENTIFIER IS EQUAL TO 'CV'                        CL*15
01254          PERFORM 1605-READ-EMPLCY THRU 1605-EXIT                     CL*15
01255      ELSE                                                            CL*15
01256          PERFORM 1600-READ-CERT   THRU 1600-EXIT.                    CL*15
01257                                                                      CL*15
01258      IF CL-SYSTEM-IDENTIFIER IS EQUAL TO 'CV'                        CL*15
01259          IF PLCY-READ-ERROR                                          CL*15
01260              PERFORM 2120-NO-CERT-INFO THRU 2120-EXIT                CL*15
01261          ELSE                                                        CL*15
01262              PERFORM 2140-MOVE-PLCY-INFO THRU 2140-EXIT              CL*15
01263      ELSE                                                            CL*15
01264          IF CERT-READ-ERROR                                          CL*15
01265              PERFORM 2120-NO-CERT-INFO  THRU 2120-EXIT               CL*15
01266          ELSE                                                        CL*15
01267              PERFORM 2130-MOVE-CERT-INFO THRU 2130-EXIT.             CL*15
01268                                                                   EL008
01269      PERFORM  1700-BROWSE-TRLR   THRU 1700-EXIT.                  EL008
01270                                                                   EL008
01271      IF TRLR-BROWSE-ERROR                                         EL008
01272         GO TO 0022-BYPASS-TRLRS.                                     CL**6
01273                                                                   EL008
01274 *      TRAILER 0 IS THE FIRST REC AFTER START                     EL008
01275      PERFORM  1750-READNEXT-TRLR   THRU 1750-EXIT.                EL008
01276                                                                   EL008
01277      PERFORM 2150-MOVE-TR0-INFO    THRU 2150-EXIT.                EL008
01278                                                                   EL008
01279 ***  NOW ALL 3 FILES HAVE BEEN READ                               EL008
01280                                                                   EL008
01281  0022-BYPASS-TRLRS.                                                  CL**6
01282                                                                      CL**6
01283      PERFORM 2300-PRINT-TO-LINE-26  THRU 2300-EXIT.               EL008
01284                                                                   EL008
01285      MOVE AT-CONTROL-PRIMARY TO WS-AT-CONTROL-SAVED.              EL008
01286      MOVE SPACE              TO WS-TRAILER-KEY-CHG-SW.            EL008
01287                                                                      CL**6
01288      IF NOT TRLR-BROWSE-ERROR                                        CL**6
01289         PERFORM 2500-PROCESS-N-PRINT-TRAILERS THRU 2500-EXIT         CL**6
01290            UNTIL TRAILER-KEY-CHANGED                                 CL**8
01291         ELSE                                                         CL**8
01292         MOVE '* TRLRS HAVE BEEN PURGED    *' TO WS-PASSED-DATA       CL**8
01293         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                          CL**8
01294         GO TO 0022-END.                                              CL**8
01295                                                                   EL008
01296 ******     BUILD ERACCT/EMPROD KEY                                   CL*15
01297                                                                      CL*15
01298      IF CL-SYSTEM-IDENTIFIER IS EQUAL TO 'CV'                        CL*15
01299          IF NOT PLCY-READ-ERROR                                      CL*15
01300              IF FULL-PRINT-REQUIRED                                  CL*15
01301                  MOVE PM-CONTROL-PRIMARY TO  WS-PD-CONTROL-PRIMARY   CL*15
01302                  PERFORM 1150-READ-AND-BUILD-PROD THRU 1199-EXIT     CL*15
01303              ELSE                                                    CL*15
01304                  NEXT SENTENCE                                       CL*15
01305          ELSE                                                        CL*15
01306              NEXT SENTENCE                                           CL*15
01307      ELSE                                                            CL*15
01308          IF NOT CERT-READ-ERROR                                      CL*15
01309              IF FULL-PRINT-REQUIRED                                  CL*15
01310                  MOVE CM-CONTROL-PRIMARY TO WS-AM-CONTROL-PRIMARY    CL*15
01311                  PERFORM 1100-READ-AND-BUILD-ACCT THRU 1149-EXIT.    CL*15
01312                                                                   EL008
01313 *******     BUILD ELBENE KEY                                      EL008
01314      IF FULL-PRINT-REQUIRED                                       EL008
01315         MOVE CL-COMPANY-CD  TO WS-BE-COMPANY-CD                   EL008
01316         MOVE 'B'            TO WS-BE-RECORD-TYPE                     CL**5
01317         MOVE CL-BENEFICIARY TO WS-BE-BENEFICIARY                  EL008
01318         PERFORM 1200-READ-AND-BUILD-BENE THRU 1299-EXIT.          EL008
01319                                                                   EL008
01320      IF FULL-PRINT-REQUIRED                                       EL008
01321          PERFORM 2700-PRINT-OPEN-CLOSE-HISTORY THRU 2700-EXIT.    EL008
01322                                                                   EL008
01323  0022-END.                                                           CL**8
01324      IF NOT TRLR-BROWSE-ERROR                                        CL**6
01325         PERFORM  1701-ENDBR-TRLR      THRU 1701-EXIT.                CL**6
01326                                                                   EL008
01327  0022-EXIT.                                                       EL008
01328       EXIT.                                                       EL008
01329                                                                   EL008
01330      EJECT                                                        EL008
01331  1100-READ-AND-BUILD-ACCT.                                        EL008
01332      EXEC CICS  HANDLE CONDITION                                  EL008
01333             NOTFND  (1140-ENDBR)                                  EL008
01334             ENDFILE (1140-ENDBR)                                  EL008
01335      END-EXEC.                                                    EL008
01336                                                                   EL008
01337      EXEC CICS  STARTBR                                           EL008
01338             DATASET  ('ERACCT')                                   EL008
01339             RIDFLD   (WS-AM-CONTROL-PRIMARY)                      EL008
01340             GTEQ                                                  EL008
01341      END-EXEC.                                                    EL008
01342                                                                   EL008
01343      MOVE 'X'         TO WS-ACCT-BROWSE-SW.                          CL**2
01344                                                                   EL008
01345  1110-READ-NEXT.                                                  EL008
01346      EXEC CICS  READNEXT                                          EL008
01347             SET      (ADDRESS OF ACCOUNT-MASTER)                     CL*17
01348             DATASET  ('ERACCT')                                   EL008
01349             RIDFLD   (WS-AM-CONTROL-PRIMARY)                      EL008
01350      END-EXEC.                                                    EL008
01351                                                                   EL008
01352  1120-CHECK-IF-EQUAL.                                             EL008
01353      IF CM-COMPANY-CD = AM-COMPANY-CD AND                         EL008
01354         CM-CARRIER    = AM-CARRIER    AND                         EL008
01355         CM-GROUPING   = AM-GROUPING   AND                         EL008
01356         CM-STATE      = AM-STATE      AND                         EL008
01357         CM-ACCOUNT    = AM-ACCOUNT                                EL008
01358            NEXT SENTENCE                                          EL008
01359      ELSE                                                         EL008
01360         GO TO 1149-EXIT.                                             CL*15
01361                                                                   EL008
01362      IF CM-CERT-EFF-DT NOT LESS THAN AM-EXPIRATION-DT             EL008
01363         GO TO 1110-READ-NEXT.                                     EL008
01364                                                                   EL008
01365      IF CM-CERT-EFF-DT NOT LESS THAN AM-EFFECTIVE-DT              EL008
01366         NEXT SENTENCE                                             EL008
01367      ELSE                                                         EL008
01368         GO TO 1110-READ-NEXT.                                     EL008
01369                                                                   EL008
01370  1130-BUILD-ERACCT-ADDR.                                          EL008
01371      MOVE 'X' TO END-AD-TABLE-SW.                                 EL008
01372      MOVE '8'                    TO AD-TBL-TYPE   (AD-INDEX).     EL008
01373      MOVE AM-NAME                TO AD-TBL-NAME   (AD-INDEX).     EL008
01374      MOVE AM-ADDRS               TO AD-TBL-ADDR-1 (AD-INDEX).     EL008
01375      MOVE SPACES                 TO AD-TBL-ADDR-2 (AD-INDEX).     EL008
051810     MOVE SPACES                 TO AD-TBL-CITY   (AD-INDEX).     EL008
051810     STRING AM-ADDR-CITY ' ' AM-ADDR-STATE
051810        DELIMITED BY '  ' INTO AD-TBL-CITY (AD-INDEX)
051810     END-STRING
01377                                                                      CL*12
01378      MOVE SPACES                 TO WS-ZIP-WORK.                     CL*12
01379      IF AM-CANADIAN-POST-CODE                                        CL*12
01380          MOVE AM-CAN-POSTAL-1    TO WS-CAN-POSTAL-1                  CL*12
01381          MOVE AM-CAN-POSTAL-2    TO WS-CAN-POSTAL-2                  CL*12
01382      ELSE                                                            CL*12
01383          MOVE AM-ZIP-PRIME       TO WS-ZIP-PRIME                     CL*12
01384          IF AM-ZIP-PLUS4 NOT = SPACES  AND  ZEROS                    CL*12
01385              MOVE '-'            TO WS-ZIP-DASH                      CL*12
01386              MOVE AM-ZIP-PLUS4   TO WS-ZIP-PLUS4.                    CL*12
01387      MOVE WS-ZIP-WORK            TO AD-TBL-ZIP    (AD-INDEX).        CL*12
01388                                                                   EL008
01389      MOVE AM-TEL-NO              TO WS-WORK-PHONE.                EL008
01390      INSPECT WS-WORK-PHONE CONVERTING SPACES TO '0'.                 CL*17
01391      MOVE WS-NUMERIC-PHONE       TO AD-TBL-PHONE (AD-INDEX).      EL008
01392      SET AD-INDEX UP BY 1.                                        EL008
01393                                                                   EL008
01394  1140-ENDBR.                                                      EL008
01395      IF ACCT-BROWSE-OKAY                                          EL008
01396         EXEC CICS  ENDBR                                          EL008
01397              DATASET  ('ERACCT')                                  EL008
01398         END-EXEC.                                                    CL*15
01399                                                                      CL*15
01400  1149-EXIT.                                                          CL*15
01401       EXIT.                                                          CL*15
01402                                                                      CL*15
01403      EJECT                                                           CL*15
01404  1150-READ-AND-BUILD-PROD.                                           CL*15
01405      EXEC CICS  HANDLE CONDITION                                     CL*15
01406          NOTFND    (1190-ENDBR)                                      CL*15
01407          ENDFILE   (1190-ENDBR)                                      CL*15
01408      END-EXEC.                                                       CL*15
01409                                                                      CL*15
01410      EXEC CICS  STARTBR                                              CL*15
01411          DATASET  ('MPPROD')                                         CL*15
01412          RIDFLD   (WS-PD-CONTROL-PRIMARY)                            CL*15
01413          GTEQ                                                        CL*15
01414      END-EXEC.                                                       CL*15
01415                                                                      CL*15
01416      MOVE 'X'                    TO  WS-PRODUCER-BROWSE-SW.          CL*15
01417                                                                      CL*15
01418  1160-READ-NEXT.                                                     CL*15
01419      EXEC CICS  READNEXT                                             CL*15
01420          SET       (ADDRESS OF PRODUCER-MASTER)                      CL*17
01421          DATASET   ('MPPROD')                                        CL*15
01422          RIDFLD    (WS-PD-CONTROL-PRIMARY)                           CL*15
01423      END-EXEC.                                                       CL*15
01424                                                                      CL*15
01425  1170-CHECK-IF-EQUAL.                                                CL*15
01426                                                                      CL*15
01427      IF PM-COMPANY-CD = PD-COMPANY-CD AND                            CL*15
01428         PM-CARRIER    = PD-CARRIER    AND                            CL*15
01429         PM-GROUPING   = PD-GROUPING   AND                            CL*15
01430         PM-STATE      = PD-STATE      AND                            CL*15
01431         PM-PRODUCER   = PD-PRODUCER                                  CL*15
01432          NEXT SENTENCE                                               CL*15
01433      ELSE                                                            CL*15
01434          GO TO 1199-EXIT.                                            CL*15
01435                                                                      CL*15
01436      IF PM-POLICY-EFF-DT NOT LESS THAN PD-EXPIRE-DATE                CL*15
01437          GO TO 1160-READ-NEXT.                                       CL*15
01438                                                                      CL*15
01439      IF PM-POLICY-EFF-DT NOT LESS THAN PD-EFFECT-DATE                CL*15
01440          NEXT SENTENCE                                               CL*15
01441      ELSE                                                            CL*15
01442          GO TO 1160-READ-NEXT.                                       CL*15
01443                                                                      CL*15
01444  1180-BUILD-EMPROD-ADDR.                                             CL*15
01445                                                                      CL*15
01446      MOVE 'X'                    TO  END-AD-TABLE-SW.                CL*15
01447      MOVE '8'                    TO  AD-TBL-TYPE   (AD-INDEX).       CL*15
01448      MOVE PD-NAME                TO  AD-TBL-NAME   (AD-INDEX).       CL*15
01449      MOVE PD-ADDRS               TO  AD-TBL-ADDR-1 (AD-INDEX).       CL*15
01450      MOVE SPACES                 TO  AD-TBL-ADDR-2 (AD-INDEX).       CL*15
01451      MOVE PD-CITY                TO  AD-TBL-CITY   (AD-INDEX).       CL*15
01452                                                                      CL*15
01453      MOVE SPACES                 TO  WS-ZIP-WORK.                    CL*15
01454      MOVE PD-ZIP-PRIME           TO  WS-ZIP-PRIME                    CL*15
01455      IF PD-ZIP-PLUS4 NOT = SPACES  AND  ZEROS                        CL*15
01456          MOVE '-'                TO  WS-ZIP-DASH                     CL*15
01457          MOVE PD-ZIP-PLUS4       TO  WS-ZIP-PLUS4.                   CL*15
01458      MOVE WS-ZIP-WORK            TO  AD-TBL-ZIP    (AD-INDEX).       CL*15
01459                                                                      CL*15
01460      MOVE PD-TEL-NO              TO  WS-WORK-PHONE.                  CL*15
01461      INSPECT WS-WORK-PHONE CONVERTING SPACES TO '0'.                 CL*17
01462      MOVE WS-NUMERIC-PHONE       TO  AD-TBL-PHONE (AD-INDEX).        CL*15
01463      SET AD-INDEX UP BY 1.                                           CL*15
01464                                                                      CL*15
01465  1190-ENDBR.                                                         CL*15
01466      IF PRODUCER-BROWSE-OKAY                                         CL*15
01467         EXEC CICS  ENDBR                                             CL*15
01468              DATASET  ('MPPROD')                                     CL*15
01469         END-EXEC.                                                 EL008
01470                                                                   EL008
01471  1199-EXIT.                                                       EL008
01472       EXIT.                                                       EL008
01473      EJECT                                                        EL008
01474                                                                   EL008
01475  1200-READ-AND-BUILD-BENE.                                        EL008
01476      EXEC CICS  HANDLE CONDITION                                  EL008
01477             NOTFND  (1299-EXIT)                                   EL008
01478             ENDFILE (1299-EXIT)                                   EL008
01479      END-EXEC.                                                    EL008
01480                                                                   EL008
01481  1210-READ.                                                       EL008
01482      EXEC CICS  READ                                              EL008
01483             SET      (ADDRESS OF BENEFICIARY-MASTER)                 CL*17
01484             DATASET  ('ELBENE')                                   EL008
01485             RIDFLD   (WS-BE-CONTROL-PRIMARY)                      EL008
01486             EQUAL                                                 EL008
01487      END-EXEC.                                                    EL008
01488                                                                   EL008
01489  1220-BUILD-CLBENE-ADDR.                                          EL008
01490      MOVE 'X' TO END-AD-TABLE-SW.                                 EL008
01491      MOVE '9'                    TO AD-TBL-TYPE   (AD-INDEX).     EL008
01492      MOVE BE-MAIL-TO-NAME        TO AD-TBL-NAME   (AD-INDEX).     EL008
01493      MOVE BE-ADDRESS-LINE-1      TO AD-TBL-ADDR-1 (AD-INDEX).     EL008
01494      MOVE BE-ADDRESS-LINE-2      TO AD-TBL-ADDR-2 (AD-INDEX).     EL008
051810     MOVE SPACES                 TO AD-TBL-CITY   (AD-INDEX).     EL008
051810     STRING BE-CITY ' ' BE-STATE
051810        DELIMITED BY '  ' INTO AD-TBL-CITY (AD-INDEX)
051810     END-STRING
01496                                                                      CL*12
01497      MOVE SPACES                 TO WS-ZIP-WORK.                     CL*12
01498      IF BE-CANADIAN-POST-CODE                                        CL*12
01499          MOVE BE-CAN-POSTAL-1    TO WS-CAN-POSTAL-1                  CL*12
01500          MOVE BE-CAN-POSTAL-2    TO WS-CAN-POSTAL-2                  CL*12
01501      ELSE                                                            CL*12
01502          MOVE BE-ZIP-PRIME       TO WS-ZIP-PRIME                     CL*12
01503          IF BE-ZIP-PLUS4 NOT = SPACES  AND  ZEROS                    CL*12
01504              MOVE '-'            TO WS-ZIP-DASH                      CL*12
01505              MOVE BE-ZIP-PLUS4   TO WS-ZIP-PLUS4.                    CL*12
01506      MOVE WS-ZIP-WORK            TO AD-TBL-ZIP    (AD-INDEX).        CL*12
01507                                                                      CL*12
01508      IF BE-PHONE-NO NOT NUMERIC                                      CL**3
01509         MOVE ZEROS TO BE-PHONE-NO.                                   CL**3
01510      MOVE BE-PHONE-NO            TO AD-TBL-PHONE  (AD-INDEX).     EL008
01511                                                                   EL008
01512  1299-EXIT.                                                       EL008
01513       EXIT.                                                       EL008
01514      EJECT                                                        EL008
01515  1300-READ-CNTL.                                                  EL008
01516 ******************************************************************   CL*15
01517 ***          I/O REQUESTS AGAINST THE CONTROL FILE             ***   CL*15
01518 ******************************************************************   CL*15
01519                                                                      CL*15
01520      EXEC CICS  HANDLE CONDITION                                  EL008
01521             NOTFND  (1301-NOTFND)                                 EL008
01522      END-EXEC.                                                    EL008
01523                                                                   EL008
01524      EXEC CICS  READ                                              EL008
01525             SET      (ADDRESS OF CONTROL-FILE)                       CL*17
01526             DATASET  ('ELCNTL')                                   EL008
01527             RIDFLD   (WS-CF-CONTROL-PRIMARY)                      EL008
01528      END-EXEC.                                                    EL008
01529                                                                   EL008
01530      GO TO 1300-EXIT.                                             EL008
01531                                                                   EL008
01532  1301-NOTFND.                                                     EL008
01533      MOVE  'X'  TO WS-CNTL-ERROR-SW.                              EL008
01534                                                                   EL008
01535  1300-EXIT.                                                       EL008
01536       EXIT.                                                       EL008
01537                                                                   EL008
01538      EJECT                                                           CL*15
01539  1400-READNEXT-ACTQ.                                              EL008
01540 ******************************************************************   CL*15
01541 ***       I/O REQUESTS AGAINST THE ACTIVITY QUE FILE           ***   CL*15
01542 ******************************************************************   CL*15
01543                                                                      CL*15
01544      EXEC CICS  HANDLE CONDITION                                  EL008
01545             NOTFND  (1401-NOTFND)                                 EL008
01546             ENDFILE (1402-EOF)                                    EL008
01547      END-EXEC.                                                    EL008
01548                                                                   EL008
01549      EXEC CICS  READNEXT                                          EL008
01550             SET      (ADDRESS OF ACTIVITY-QUE)                       CL*17
01551             DATASET  ('ELACTQ')                                   EL008
01552             RIDFLD   (WS-AQ-CONTROL-PRIMARY)                      EL008
01553      END-EXEC.                                                    EL008
01554                                                                   EL008
01555      IF AQ-COMPANY-CD NOT = PI-COMPANY-CD                         EL008
01556         GO TO 1402-EOF.                                           EL008
01557                                                                   EL008
01558      GO TO 1400-EXIT.                                             EL008
01559                                                                   EL008
01560  1401-NOTFND.                                                     EL008
01561      MOVE  'X'  TO WS-ACTQ-READ-ERROR-SW.                         EL008
01562      GO TO 1400-EXIT.                                             EL008
01563                                                                   EL008
01564  1402-EOF.                                                        EL008
01565      MOVE  'E'  TO WS-ELACTQ-EOF-SW.                                 CL*17
01566      GO TO 1400-EXIT.                                             EL008
01567                                                                   EL008
01568  1400-EXIT.                                                       EL008
01569       EXIT.                                                       EL008
01570                                                                   EL008
01571  1450-BROWSE-ACTQ.                                                EL008
01572      EXEC CICS  HANDLE CONDITION                                  EL008
01573             NOTFND  (1451-NOTFND)                                 EL008
01574      END-EXEC.                                                    EL008
01575                                                                   EL008
01576      EXEC CICS  STARTBR                                           EL008
01577             DATASET  ('ELACTQ')                                   EL008
01578             RIDFLD   (WS-AQ-CONTROL-PRIMARY)                      EL008
01579             GTEQ                                                  EL008
01580      END-EXEC.                                                    EL008
01581                                                                   EL008
01582      GO TO 1450-EXIT.                                             EL008
01583                                                                   EL008
01584  1451-NOTFND.                                                     EL008
01585      MOVE    '*** ERROR ON START ACTQ ***' TO WS-PASSED-DATA      EL008
01586      GO TO 9999-FINALIZE.                                         EL008
01587                                                                   EL008
01588  1450-EXIT.                                                       EL008
01589       EXIT.                                                       EL008
01590                                                                   EL008
01591  1455-ENDBR-ACTQ.                                                 EL008
01592      EXEC CICS  ENDBR                                             EL008
01593             DATASET  ('ELACTQ')                                   EL008
01594      END-EXEC.                                                    EL008
01595                                                                   EL008
01596  1455-EXIT.                                                       EL008
01597       EXIT.                                                       EL008
01598                                                                   EL008
01599  1480-READ-UPDATE-ACTQ.                                           EL008
01600      EXEC CICS  HANDLE CONDITION                                  EL008
01601             NOTFND  (1481-NOTFND)                                 EL008
01602      END-EXEC.                                                    EL008
01603                                                                   EL008
01604      EXEC CICS  READ                                              EL008
01605             SET      (ADDRESS OF ACTIVITY-QUE)                       CL*17
01606             DATASET  ('ELACTQ')                                   EL008
01607             RIDFLD   (WS-AQ-CONTROL-PRIMARY)                      EL008
01608             UPDATE                                                EL008
01609      END-EXEC.                                                    EL008
01610                                                                   EL008
01611      GO TO 1480-EXIT.                                             EL008
01612                                                                   EL008
01613  1481-NOTFND.                                                     EL008
01614      MOVE  'X'  TO WS-ACTQ-READ-ERROR-SW.                         EL008
01615                                                                   EL008
01616  1480-EXIT.                                                       EL008
01617       EXIT.                                                       EL008
01618                                                                   EL008
01619      EJECT                                                           CL*15
01620  1550-READ-MSTR.                                                  EL008
01621 ******************************************************************   CL*15
01622 ***       I/O REQUESTS AGAINST THE CLAIM MASTER FILE           ***   CL*15
01623 ******************************************************************   CL*15
01624                                                                      CL*15
01625      EXEC CICS  HANDLE CONDITION                                  EL008
01626             NOTFND  (1551-NOTFND)                                 EL008
01627             ENDFILE (1552-EOF)                                    EL008
01628      END-EXEC.                                                    EL008
01629                                                                   EL008
01630      EXEC CICS  READ                                              EL008
01631             SET      (ADDRESS OF CLAIM-MASTER)                       CL*17
01632             DATASET  ('ELMSTR')                                   EL008
01633             RIDFLD   (WS-CL-CONTROL-PRIMARY)                      EL008
01634      END-EXEC.                                                    EL008
01635                                                                   EL008
01636      GO TO 1550-EXIT.                                             EL008
01637                                                                   EL008
01638  1552-EOF.                                                        EL008
01639      MOVE  'E'  TO WS-ELMSTR-EOF-SW.                                 CL*17
01640      GO TO 1550-EXIT.                                             EL008
01641                                                                   EL008
01642  1551-NOTFND.                                                     EL008
01643      MOVE  'X'  TO WS-MSTR-READ-ERROR-SW.                         EL008
01644                                                                   EL008
01645  1550-EXIT.                                                       EL008
01646       EXIT.                                                       EL008
01647                                                                   EL008
01648      EJECT                                                           CL*15
01649  1600-READ-CERT.                                                  EL008
01650 ******************************************************************   CL*15
01651 ***   I/O REQUESTS AGAINST THE CERTIFICATE MASTER FILE         ***   CL*15
01652 ******************************************************************   CL*15
01653                                                                      CL*15
01654      EXEC CICS  HANDLE CONDITION                                  EL008
01655             NOTFND  (1601-NOTFND)                                 EL008
01656      END-EXEC.                                                    EL008
01657                                                                   EL008
01658      EXEC CICS  READ                                              EL008
01659             SET      (ADDRESS OF CERTIFICATE-MASTER)                 CL*17
01660             DATASET  ('ELCERT')                                   EL008
01661             RIDFLD   (WS-CM-CONTROL-PRIMARY)                      EL008
01662      END-EXEC.                                                    EL008
01663                                                                   EL008
01664      GO TO 1600-EXIT.                                             EL008
01665                                                                   EL008
01666  1601-NOTFND.                                                     EL008
01667      MOVE  'X'  TO WS-CERT-READ-ERROR-SW.                         EL008
01668                                                                   EL008
01669  1600-EXIT.                                                       EL008
01670       EXIT.                                                       EL008
01671                                                                   EL008
01672  1605-READ-EMPLCY.                                                   CL*15
01673 ******************************************************************   CL*15
01674 ***   I/O REQUESTS AGAINST THE CONVENIENCE POLICY MASTER FILE  ***   CL*15
01675 ******************************************************************   CL*15
01676                                                                      CL*15
01677      EXEC CICS  HANDLE CONDITION                                     CL*15
01678             NOTFND  (1605-NOTFND)                                    CL*15
01679      END-EXEC.                                                       CL*15
01680                                                                      CL*15
01681      EXEC CICS  READ                                                 CL*15
01682             SET      (ADDRESS OF POLICY-MASTER)                      CL*17
01683             DATASET  ('MPPLCY')                                      CL*15
01684             RIDFLD   (WS-PM-CONTROL-PRIMARY)                         CL*15
01685      END-EXEC.                                                       CL*15
01686                                                                      CL*15
01687      GO TO 1605-EXIT.                                                CL*15
01688                                                                      CL*15
01689  1605-NOTFND.                                                        CL*15
01690      MOVE  'X'                   TO  WS-PLCY-READ-ERROR-SW.          CL*15
01691                                                                      CL*15
01692  1605-EXIT.                                                          CL*15
01693       EXIT.                                                          CL*15
01694                                                                      CL*15
01695  1610-READ-EMPLAN.                                                   CL*15
01696 ******************************************************************   CL*15
01697 ***   I/O REQUESTS AGAINST THE CONVENIENCE PRODUCER PLAN       ***   CL*15
01698 ***   MASTER FILE                                              ***   CL*15
01699 ******************************************************************   CL*15
01700                                                                      CL*15
01701      EXEC CICS  HANDLE CONDITION                                     CL*15
01702             NOTFND  (1610-NOTFND)                                    CL*15
01703      END-EXEC.                                                       CL*15
01704                                                                      CL*15
01705      EXEC CICS  READ                                                 CL*15
01706             SET      (ADDRESS OF PRODUCER-PLANS)                     CL*17
01707             DATASET  ('MPPLAN')                                      CL*15
01708             RIDFLD   (WS-PP-CONTROL-PRIMARY)                         CL*15
01709      END-EXEC.                                                       CL*15
01710                                                                      CL*15
01711      GO TO 1610-EXIT.                                                CL*15
01712                                                                      CL*15
01713  1610-NOTFND.                                                        CL*15
01714      MOVE  'X'                   TO  WS-PLAN-READ-ERROR-SW.          CL*15
01715                                                                      CL*15
01716  1610-EXIT.                                                          CL*15
01717       EXIT.                                                          CL*15
01718                                                                      CL*15
01719      EJECT                                                           CL*15
01720  1700-BROWSE-TRLR.                                                EL008
01721 ******************************************************************   CL*15
01722 ***       I/O REQUESTS AGAINST THE ACTIVITY TRAILER FILE       ***   CL*15
01723 ******************************************************************   CL*15
01724                                                                      CL*15
01725      EXEC CICS  HANDLE CONDITION                                  EL008
01726             NOTFND  (1701-NOTFND)                                 EL008
01727      END-EXEC.                                                    EL008
01728                                                                   EL008
01729      EXEC CICS  STARTBR                                           EL008
01730             DATASET  ('ELTRLR')                                   EL008
01731             RIDFLD   (WS-AT-CONTROL-PRIMARY)                      EL008
01732             EQUAL                                                    CL**6
01733      END-EXEC.                                                    EL008
01734                                                                   EL008
01735      MOVE SPACE TO WS-TRLR-BROWSE-ERROR-SW.                          CL**8
01736                                                                      CL**8
01737      GO TO 1700-EXIT.                                             EL008
01738  1701-NOTFND.                                                     EL008
01739      MOVE  'X'  TO WS-TRLR-BROWSE-ERROR-SW.                       EL008
01740                                                                   EL008
01741  1700-EXIT.                                                       EL008
01742       EXIT.                                                       EL008
01743                                                                   EL008
01744  1701-ENDBR-TRLR.                                                 EL008
01745      EXEC CICS  ENDBR                                             EL008
01746             DATASET  ('ELTRLR')                                   EL008
01747      END-EXEC.                                                    EL008
01748                                                                   EL008
01749  1701-EXIT.                                                       EL008
01750       EXIT.                                                       EL008
01751                                                                   EL008
01752  1750-READNEXT-TRLR.                                              EL008
01753      EXEC CICS  HANDLE CONDITION                                  EL008
01754             ENDFILE (1752-ENDFILE)                                EL008
01755      END-EXEC.                                                    EL008
01756                                                                   EL008
01757      EXEC CICS  READNEXT                                          EL008
01758             DATASET  ('ELTRLR')                                   EL008
01759             RIDFLD   (WS-AT-CONTROL-PRIMARY)                      EL008
01760             SET      (ADDRESS OF ACTIVITY-TRAILERS)                  CL*17
01761      END-EXEC.                                                    EL008
01762                                                                   EL008
01763      GO TO 1750-EXIT.                                             EL008
01764                                                                   EL008
01765  1752-ENDFILE.                                                    EL008
01766      MOVE  'E'  TO WS-ELTRLR-EOF-SW.                                 CL*17
01767                                                                   EL008
01768  1750-EXIT.                                                       EL008
01769       EXIT.                                                       EL008
01770                                                                   EL008
01771      EJECT                                                           CL*15
01772  1800-DELETE-ACTQ.                                                EL008
01773 ******************************************************************   CL*15
01774 ***     MORE I/O REQUESTS AGAINST THE ACTIVITY QUE FILE        ***   CL*15
01775 ******************************************************************   CL*15
01776                                                                      CL*15
01777      EXEC CICS  HANDLE CONDITION                                  EL008
01778             ERROR   (1801-ERROR)                                  EL008
01779      END-EXEC.                                                    EL008
01780                                                                   EL008
01781      EXEC CICS  DELETE                                            EL008
01782             DATASET  ('ELACTQ')                                   EL008
01783      END-EXEC.                                                    EL008
01784                                                                   EL008
01785      GO TO 1800-EXIT.                                             EL008
01786                                                                   EL008
01787  1801-ERROR.                                                      EL008
01788      MOVE    '*** ERROR UPDATING ACTQ ***' TO WS-PASSED-DATA.     EL008
01789      GO TO 9999-FINALIZE.                                         EL008
01790                                                                   EL008
01791  1800-EXIT. EXIT.                                                 EL008
01792 *********                                                         EL008
01793  1850-REWRITE-ACTQ.                                               EL008
01794      EXEC CICS  HANDLE CONDITION                                  EL008
01795             ERROR   (1851-ERROR)                                  EL008
01796      END-EXEC.                                                    EL008
01797                                                                   EL008
01798      EXEC CICS  REWRITE                                           EL008
01799             DATASET  ('ELACTQ')                                   EL008
01800             FROM     (ACTIVITY-QUE)                               EL008
01801      END-EXEC.                                                    EL008
01802                                                                   EL008
01803      GO TO 1850-EXIT.                                             EL008
01804                                                                   EL008
01805  1851-ERROR.                                                      EL008
01806          MOVE    '*** ERROR UPDATING ACTQ ***' TO WS-PASSED-DATA  EL008
01807          GO TO 9999-FINALIZE.                                     EL008
01808                                                                   EL008
01809  1850-EXIT.                                                       EL008
01810       EXIT.                                                       EL008
01811                                                                   EL008
01812      EJECT                                                           CL*15
uktdel*ALIGN-RTN.                      COPY ELCALGNP.                   EL008
uktins ALIGN-RTN.
uktins     COPY ELCALGNP.
01814      EJECT                                                           CL*15
01815  2110-MOVE-CLAIM-INFO.                                            EL008
01816      MOVE CL-CLAIM-NO          TO P-CLAIM-NO.                     EL008
01817      MOVE CL-CARRIER           TO P-CARR.                         EL008
01818      MOVE CL-CERT-NO           TO P-CERT-NO.                      EL008
01819      MOVE CL-CCN               TO HEAD-CREDIT-CARD.                  CL*17
01820      MOVE CL-INSURED-SEX-CD    TO P-INSURED-SEX.                  EL008
01821      MOVE SPACES               TO P-STATUS     P-TYPE.            EL008
01822                                                                   EL008
121802     EVALUATE TRUE

121802        WHEN CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1
01824            MOVE PI-AH-OVERRIDE-L2
                                       TO P-TYPE

121802        WHEN CL-CLAIM-TYPE = 'I'
121802           MOVE ' IU '            TO P-TYPE

121802        WHEN CL-CLAIM-TYPE = 'G'
121802           MOVE 'GAP '            TO P-TYPE
052614
052614        WHEN CL-CLAIM-TYPE = 'F'
052614           MOVE 'FAM '            TO P-TYPE
100518
100518        WHEN CL-CLAIM-TYPE = 'O'
100518           MOVE 'OTH '            TO P-TYPE

080322        WHEN CL-CLAIM-TYPE = 'B'
080322           MOVE ' BRV  '          TO P-TYPE
080322
080322        WHEN CL-CLAIM-TYPE = 'H'
080322           MOVE ' HOSP '          TO P-TYPE


121802        WHEN CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1
01827            MOVE PI-LIFE-OVERRIDE-L2
                                       TO P-TYPE

121802     END-EVALUATE
01828                                                                   EL008
01829      IF CLAIM-IS-CLOSED                                           EL008
01830          MOVE 'CLOSED'  TO P-STATUS.                              EL008
01831                                                                   EL008
01832      IF CLAIM-IS-OPEN                                             EL008
01833          MOVE 'OPEN'    TO P-STATUS.                              EL008
01834                                                                   EL008
01835      PERFORM 2111-ARRANGE-NAME  THRU 2111-EXIT.                   EL008
01836                                                                   EL008
01837      MOVE CL-PROCESSOR-ID TO P-PROCESSOR.                         EL008
01838      MOVE CL-LAST-PMT-AMT TO P-LAST-PMT-AMT.                      EL008
01839                                                                   EL008
01840      IF CL-PAID-THRU-DT  = LOW-VALUES OR SPACES                   EL008
01841          MOVE SPACE TO P-PAID-THRU-DT                             EL008
01842          IF PI-USES-PAID-TO                                          CL*11
01843             MOVE 'PAID  TO   - ' TO L-12-PD-THRU                     CL*11
01844          ELSE                                                        CL*11
01845             MOVE 'PAID THRU  - ' TO L-12-PD-THRU                     CL*11
01846      ELSE                                                         EL008
01847          IF NOT PI-USES-PAID-TO                                      CL**5
01848             MOVE CL-PAID-THRU-DT TO DC-BIN-DATE-1                    CL**5
01849             MOVE ' '             TO DC-OPTION-CODE                   CL**5
01850             PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    CL**5
01851             MOVE DC-GREG-DATE-1-EDIT  TO P-PAID-THRU-DT              CL**5
01852          ELSE                                                        CL**5
01853             MOVE 'PAID  TO   - ' TO L-12-PD-THRU                     CL**5
01854             MOVE CL-PAID-THRU-DT TO DC-BIN-DATE-1                    CL**5
01855             MOVE +1  TO DC-ELAPSED-DAYS                              CL**5
01856             MOVE +0  TO DC-ELAPSED-MONTHS                            CL**5
01857             MOVE '6' TO DC-OPTION-CODE                               CL**5
01858             PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    CL**5
01859             MOVE DC-GREG-DATE-1-EDIT  TO P-PAID-THRU-DT.             CL**5
01860                                                                   EL008
01861      IF CL-LAST-PMT-DT = LOW-VALUES OR SPACES                     EL008
01862          MOVE SPACE TO P-LAST-PMT-DT                              EL008
01863      ELSE                                                         EL008
01864          MOVE CL-LAST-PMT-DT  TO DC-BIN-DATE-1                    EL008
01865          MOVE ' '             TO DC-OPTION-CODE                   EL008
01866          PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    EL008
01867          MOVE DC-GREG-DATE-1-EDIT  TO P-LAST-PMT-DT.              EL008
01868                                                                   EL008
01869      IF CL-INCURRED-DT = LOW-VALUES OR SPACES                     EL008
01870          MOVE SPACE TO P-INCURRED-DT                              EL008
01871      ELSE                                                         EL008
01872          MOVE CL-INCURRED-DT  TO DC-BIN-DATE-1                    EL008
01873          MOVE ' '             TO DC-OPTION-CODE                   EL008
01874          PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    EL008
01875          MOVE DC-GREG-DATE-1-EDIT  TO P-INCURRED-DT.              EL008
01876                                                                   EL008
01877      IF CL-REPORTED-DT = LOW-VALUES OR SPACES                     EL008
01878          MOVE SPACE TO P-REPORTED-DT                              EL008
01879      ELSE                                                         EL008
01880          MOVE CL-REPORTED-DT TO DC-BIN-DATE-1                     EL008
01881          MOVE ' '            TO DC-OPTION-CODE                    EL008
01882          PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    EL008
01883          MOVE DC-GREG-DATE-1-EDIT  TO P-REPORTED-DT               EL008
01884                                                                   EL008
01885      IF CL-FILE-ESTABLISH-DT = LOW-VALUES OR SPACES               EL008
01886          MOVE SPACE TO P-ESTAB-DT                                 EL008
01887      ELSE                                                         EL008
01888          MOVE CL-FILE-ESTABLISH-DT TO DC-BIN-DATE-1               EL008
01889          MOVE ' '                  TO DC-OPTION-CODE              EL008
01890          PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    EL008
01891          MOVE DC-GREG-DATE-1-EDIT    TO P-ESTAB-DT.               EL008
01892                                                                   EL008
01893      IF CL-NEXT-AUTO-PAY-DT = LOW-VALUES OR SPACES                EL008
01894          MOVE SPACES   TO P-NEXT-AUTO-DT                          EL008
01895      ELSE                                                         EL008
01896          IF PI-USES-PAID-TO                                          CL*11
01897              MOVE CL-NEXT-AUTO-PAY-DT    TO  DC-BIN-DATE-1           CL*11
01898              MOVE '6'                    TO  DC-OPTION-CODE          CL*11
01899              MOVE +1                     TO  DC-ELAPSED-DAYS         CL*11
01900              MOVE +0                     TO  DC-ELAPSED-MONTHS       CL*11
01901              PERFORM 8100-DATE-RTN      THRU 8100-EXIT               CL*11
01902              IF NO-CONVERSION-ERROR                                  CL*11
01903                  MOVE DC-GREG-DATE-1-EDIT TO P-NEXT-AUTO-DT          CL*11
01904              ELSE                                                    CL*11
01905                  MOVE SPACES             TO  P-NEXT-AUTO-DT          CL*11
01906          ELSE                                                        CL*11
01907              MOVE CL-NEXT-AUTO-PAY-DT    TO  DC-BIN-DATE-1           CL*11
01908              MOVE ' '                    TO  DC-OPTION-CODE          CL*11
01909              MOVE +0                     TO  DC-ELAPSED-DAYS         CL*11
01910                                             DC-ELAPSED-MONTHS        CL*11
01911              PERFORM 8100-DATE-RTN       THRU 8100-EXIT              CL*11
01912              IF NO-CONVERSION-ERROR                                  CL*11
01913                  MOVE DC-GREG-DATE-1-EDIT TO P-NEXT-AUTO-DT          CL*11
01914              ELSE                                                    CL*11
01915                  MOVE SPACES             TO  P-NEXT-AUTO-DT.         CL*11
01916                                                                   EL008
01917      MOVE CL-NO-OF-PMTS-MADE     TO P-PMTS-MADE.                  EL008
01918                                                                   EL008
01919      MOVE SPACES  TO P-PREM-TYPE.                                 EL008
01920      IF SINGLE-PREMIUM                                            EL008
01921          MOVE 'SINGLE'           TO P-PREM-TYPE.                  EL008
01922                                                                   EL008
01923      IF O-B-COVERAGE                                              EL008
01924          MOVE 'OB COVG'          TO P-PREM-TYPE.                  EL008
01925                                                                   EL008
01926      IF OPEN-END-COVERAGE                                         EL008
01927          MOVE 'OPN END'          TO P-PREM-TYPE.                  EL008
01928                                                                   EL008
01929      MOVE CL-TOTAL-PAID-AMT      TO P-TOT-PAID.                   EL008
01930                                                                      CL**6
01931      MOVE SPACES              TO P-PURGED-STMT                       CL**6
01932                                  P-PURGED-DATE.                      CL**6
01933                                                                      CL**6
01934      IF CL-PURGED-DT NOT EQUAL LOW-VALUES                            CL**6
01935         MOVE CL-PURGED-DT         TO DC-BIN-DATE-1                   CL**6
01936         MOVE ' '                  TO DC-OPTION-CODE                  CL**6
01937         MOVE +0                   TO DC-ELAPSED-DAYS                 CL*11
01938                                      DC-ELAPSED-MONTHS               CL*11
01939         PERFORM 8100-DATE-RTN  THRU 8100-EXIT                        CL**6
01940         MOVE 'PURGED DATE  - '    TO P-PURGED-STMT                   CL**6
01941         MOVE DC-GREG-DATE-1-EDIT  TO P-PURGED-DATE.                  CL**6
01942                                                                   EL008
01943  2110-EXIT.                                                       EL008
01944       EXIT.                                                       EL008
01945                                                                   EL008
01946      EJECT                                                           CL*15
01947  2111-ARRANGE-NAME.                                               EL008
01948      MOVE SPACES                TO P-NAME-GRP.                    EL008
01949      MOVE CL-INSURED-LAST-NAME  TO WS-LAST-NAME.                  EL008
01950      MOVE CL-INSURED-1ST-NAME   TO WS-FIRST-NAME.                 EL008
01951                                                                   EL008
01952      SET L-IND  TO 1.                                             EL008
01953      SET F-IND  TO 1.                                             EL008
01954      SET P-IND  TO 1.                                             EL008
01955                                                                   EL008
01956      MOVE SPACES  TO WS-LN-SW                                     EL008
01957                      WS-FN-SW                                     EL008
01958                      PREVIOUS-BYTE.                               EL008
01959                                                                   EL008
01960      IF WS-LAST-NAME = SPACE                                      EL008
01961          NEXT SENTENCE                                            EL008
01962      ELSE                                                         EL008
01963          PERFORM 2111A-LOAD-LAST-NAME  THRU 2111A-EXIT            EL008
01964           UNTIL  LAST-NAME-LOADED                                 EL008
01965           OR     L-IND  GREATER THAN 15.                          EL008
01966                                                                   EL008
01967      MOVE ',' TO P-NAME (P-IND).                                  EL008
01968      SET P-IND  UP  BY 2.                                         EL008
01969      IF WS-FIRST-NAME = SPACE                                     EL008
01970          NEXT SENTENCE                                            EL008
01971      ELSE                                                         EL008
01972          PERFORM 2111B-LOAD-FIRST-NAME  THRU 2111B-EXIT           EL008
01973           UNTIL  FIRST-NAME-LOADED                                EL008
01974           OR     F-IND  GREATER THAN 12.                          EL008
01975                                                                   EL008
01976      IF CL-INSURED-MID-INIT NOT = SPACES                          EL008
01977          MOVE ','  TO P-NAME (P-IND)                              EL008
01978          SET P-IND  UP BY 1                                       EL008
01979          MOVE CL-INSURED-MID-INIT  TO P-NAME (P-IND)              EL008
01980      ELSE                                                         EL008
01981          NEXT SENTENCE.                                           EL008
01982                                                                   EL008
01983  2111-EXIT.                                                       EL008
01984       EXIT.                                                       EL008
01985                                                                   EL008
01986  2111A-LOAD-LAST-NAME.                                            EL008
01987      IF  WS-L-BYTE (L-IND) = SPACE                                EL008
01988          IF PREVIOUS-BYTE = SPACE                                 EL008
01989              MOVE 'X' TO WS-LN-SW                                 EL008
01990              SET P-IND  DOWN  BY 1                                EL008
01991              GO TO 2111A-EXIT                                     EL008
01992          ELSE                                                     EL008
01993              MOVE SPACE TO PREVIOUS-BYTE                          EL008
01994      ELSE                                                         EL008
01995          MOVE WS-L-BYTE (L-IND)  TO P-NAME (P-IND)                EL008
01996                                     PREVIOUS-BYTE.                EL008
01997                                                                   EL008
01998      SET L-IND  UP BY 1.                                          EL008
01999      SET P-IND  UP BY 1.                                          EL008
02000                                                                   EL008
02001  2111A-EXIT.                                                      EL008
02002        EXIT.                                                      EL008
02003                                                                   EL008
02004  2111B-LOAD-FIRST-NAME.                                           EL008
02005      IF  WS-F-BYTE (F-IND) = SPACE                                EL008
02006          IF PREVIOUS-BYTE = SPACE                                 EL008
02007              MOVE 'X' TO WS-FN-SW                                 EL008
02008              SET P-IND  DOWN BY 1                                 EL008
02009              GO TO 2111B-EXIT                                     EL008
02010          ELSE                                                     EL008
02011              MOVE SPACE TO PREVIOUS-BYTE                          EL008
02012      ELSE                                                         EL008
02013          MOVE WS-F-BYTE (F-IND)  TO P-NAME (P-IND)                EL008
02014                                     PREVIOUS-BYTE.                EL008
02015                                                                   EL008
02016      SET F-IND  UP BY 1.                                          EL008
02017      SET P-IND  UP BY 1.                                          EL008
02018                                                                   EL008
02019  2111B-EXIT.                                                      EL008
02020        EXIT.                                                      EL008
02021                                                                   EL008
02022      EJECT                                                           CL*15
02023  2120-NO-CERT-INFO.                                               EL008
02024      MOVE SPACES     TO P-COVERAGE                                EL008
02025                         P-EXPIRE                                  EL008
02026                         WS-CANC-DT                                EL008
02027                         P-CERT-ISSUE-DT                           EL008
02028                         P-CERT-ENTRY-MM  P-CERT-SL                EL008
02029                         P-CERT-ENTRY-YY.                          EL008
02030      MOVE ZERO       TO P-TERM                                    EL008
02031                         P-REM.                                    EL008
02032      MOVE SPACES     TO P-CERT-STAT                               EL008
02033                         P-ACCT                                    EL008
02034                         P-STATE                                   EL008
02035                         P-GROUP                                   EL008
02036                         P-REIN-CODE                               EL008
02037                         P-CERT-CANC-DT                            EL008
02038                         P-MEMBER-NO                               EL008
02039                         P-CERT-BATCH.                             EL008
02040      MOVE ZERO       TO P-ORIG-BENEF-AMT.                         EL008
02041                                                                   EL008
02042  2120-EXIT.                                                       EL008
02043       EXIT.                                                       EL008
02044                                                                   EL008
02045      EJECT                                                           CL*15
02046  2130-MOVE-CERT-INFO.                                             EL008
02047      MOVE ZEROS                    TO WS-BEN-CODE.                EL008
02048      MOVE CM-INSURED-ISSUE-AGE     TO P-INSURED-AGE.              EL008
02049      MOVE CM-USER-FIELD            TO P-USER-CODE.                EL008
02050                                                                   EL008
02051      IF CM-LF-ALT-BENEFIT-AMT NOT NUMERIC                            CL*16
02052          MOVE ZEROS                TO CM-LF-ALT-BENEFIT-AMT.         CL*16
02053                                                                      CL*16
052614     IF CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1 OR 'I' OR 'G' OR 'F'
02055          MOVE  CM-AH-BENEFIT-CD      TO  WS-BEN-CODE                 CL**9
02056          MOVE  '5'                   TO  WS-CF-RECORD-TYPE           CL**9
02057          MOVE  CM-AH-ORIG-TERM       TO  P-TERM                      CL**9
02058                                          CP-ORIGINAL-TERM            CL**9
02059          MOVE CM-AH-LOAN-EXPIRE-DT   TO  DC-BIN-DATE-1               CL**9
02060          MOVE  CM-AH-BENEFIT-AMT     TO  P-ORIG-BENEF-AMT            CL**9
02061          MOVE  CM-AH-CURRENT-STATUS  TO  WS-STATUS                   CL**9
02062          MOVE WS-BIN-CURRENT-DT      TO  CP-VALUATION-DT             CL**9
02063      ELSE                                                         EL008
02064          MOVE  CM-LF-BENEFIT-CD      TO  WS-BEN-CODE                 CL**9
02065          MOVE  '4'                   TO  WS-CF-RECORD-TYPE           CL**9
02066          MOVE  CM-LF-ORIG-TERM       TO  P-TERM                      CL**9
02067                                          CP-ORIGINAL-TERM            CL**9
02068          MOVE CM-LF-LOAN-EXPIRE-DT   TO  DC-BIN-DATE-1               CL**9
02069          COMPUTE P-ORIG-BENEF-AMT =                                  CL*16
02070                 CM-LF-BENEFIT-AMT + CM-LF-ALT-BENEFIT-AMT            CL*16
02071          MOVE  CM-LF-CURRENT-STATUS  TO  WS-STATUS                   CL**9
02072          MOVE CL-INCURRED-DT         TO  CP-VALUATION-DT
           END-IF
02073                                                                   EL008
02074      MOVE ' '                       TO DC-OPTION-CODE.               CL**7
02075      PERFORM 8100-DATE-RTN  THRU 8100-EXIT.                          CL**7
02076      MOVE DC-GREG-DATE-1-EDIT       TO P-EXPIRE.                     CL**7
02077                                                                      CL**7
02078      MOVE CM-CERT-EFF-DT           TO CP-CERT-EFF-DT              EL008
02079                                                                   EL008
02080      MOVE 'CERT ENTRY    - '        TO L22-ENTRY-LIT.                CL*15
02081      IF CM-ENTRY-DT = LOW-VALUES OR SPACES                        EL008
02082          MOVE SPACES TO    P-CERT-ENTRY-YY   P-CERT-SL            EL008
02083                            P-CERT-ENTRY-MM                        EL008
02084      ELSE                                                         EL008
02085          MOVE CM-ENTRY-DT           TO DC-BIN-DATE-1              EL008
02086          MOVE ' '                   TO DC-OPTION-CODE             EL008
02087          PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    EL008
02088          MOVE DC-GREG-DATE-1-MDY    TO WS-MM-DD-YY                EL008
02089          MOVE WS-MM                 TO P-CERT-ENTRY-MM            EL008
02090          MOVE WS-YY                 TO P-CERT-ENTRY-YY            EL008
02091          MOVE '/' TO P-CERT-SL.                                   EL008
02092                                                                   EL008
02093      MOVE 'CERT ISSUE   - '         TO L15-ISSUE-LIT.                CL*15
02094      MOVE CM-CERT-EFF-DT            TO DC-BIN-DATE-1.             EL008
02095      MOVE ' '                       TO DC-OPTION-CODE.            EL008
02096      PERFORM 8100-DATE-RTN  THRU 8100-EXIT.                       EL008
02097      MOVE DC-GREG-DATE-1-EDIT       TO P-CERT-ISSUE-DT.           EL008
02098                                                                      CL**4
02099      MOVE CM-LOAN-1ST-PMT-DT        TO CP-FIRST-PAY-DATE.            CL**4
02100                                                                   EL008
02101 **     READ CNTL RECORD TO OBTAIN FREE LOOK DAYS                     CL*19
02102                                                                      CL*19
02103      MOVE PI-COMPANY-ID             TO WS-CF-COMPANY-ID.             CL*19
02104      MOVE PI-STATE                  TO WS-CF-PROCESSOR.              CL*19
02105      MOVE ZEROS                     TO WS-CF-SEQUENCE-NO.            CL*19
02106      MOVE '3'                       TO WS-CF-RECORD-TYPE.            CL*19
02107      MOVE SPACES                    TO WS-CNTL-ERROR-SW.             CL*19
02108                                                                      CL*19
02109      PERFORM 1300-READ-CNTL  THRU 1300-EXIT.                         CL*19
02110                                                                      CL*19
02111      IF NO-COMPANY-RECORD                                            CL*19
02112         MOVE    '*** ERROR NO STATE REC ***' TO WS-PASSED-DATA       CL*19
02113         GO TO 9999-FINALIZE                                          CL*19
02114      ELSE                                                            CL*19
02115         MOVE CF-ST-FREE-LOOK-PERIOD TO CP-FREE-LOOK.                 CL*19
02116                                                                      CL*19
02117      MOVE PI-REM-TRM-CALC-OPTION    TO CP-REM-TRM-CALC-OPTION.       CL*12
02118      MOVE PI-COMPANY-ID             TO CP-COMPANY-ID.             EL008
02119      MOVE '4'                       TO CP-REM-TERM-METHOD.        EL008
02120      PERFORM 9800-LINK-REM-TERM THRU 9800-EXIT.                   EL008
02121                                                                   EL008
02122      IF CP-REMAINING-TERM-3 NOT GREATER THAN ZEROS                EL008
02123         MOVE ZEROS      TO  P-REM                                 EL008
02124        ELSE                                                       EL008
02125         MOVE CP-REMAINING-TERM-3 TO  P-REM.                       EL008
02126                                                                      CL*15
02127      MOVE SPACES           TO P-CERT-STAT.                           CL*15
02128                                                                   EL008
02129      IF WS-STATUS = '1' OR '4'                                    EL008
02130         IF CP-REMAINING-TERM-3  NOT GREATER THAN ZEROS            EL008
02131            MOVE 'EXPIRED'  TO P-CERT-STAT                         EL008
02132           ELSE                                                    EL008
02133            MOVE 'ACTIVE'   TO P-CERT-STAT.                        EL008
02134                                                                   EL008
02135      IF WS-STATUS = '2'                                           EL008
02136          MOVE 'PEND'     TO P-CERT-STAT.                          EL008
02137                                                                   EL008
02138      IF WS-STATUS = '3'                                           EL008
02139          MOVE 'RESTORE ' TO P-CERT-STAT.                          EL008
02140                                                                   EL008
02141      IF WS-STATUS = '5'                                           EL008
02142          MOVE 'REISSUE ' TO P-CERT-STAT.                          EL008
02143                                                                   EL008
02144      IF WS-STATUS = '6'                                           EL008
02145          MOVE 'LMP DIS'  TO P-CERT-STAT.                          EL008
02146                                                                   EL008
02147      IF WS-STATUS = '7'                                           EL008
02148          MOVE 'DEATH'    TO P-CERT-STAT.                             CL*14
02149                                                                   EL008
02150      IF WS-STATUS = '8'                                           EL008
02151          MOVE 'CANCEL'   TO P-CERT-STAT.                             CL*14
02152                                                                   EL008
02153      IF WS-STATUS = '9'                                           EL008
02154          MOVE 'RE-ONLY ' TO P-CERT-STAT.                          EL008
02155                                                                      CL*14
02156      IF WS-STATUS = 'D'                                              CL*14
02157          MOVE 'DECLINE'  TO P-CERT-STAT.                             CL*14
02158                                                                      CL*14
02159      IF WS-STATUS = 'V'                                              CL*14
02160          MOVE 'VOID'     TO P-CERT-STAT.                             CL*14
02161                                                                   EL008
02162      MOVE SPACES                 TO WS-CANC-DT.                   EL008
02163                                                                   EL008
052614     IF CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1 OR 'I' OR 'G' OR 'F'
02165         GO TO 2130-AH-CHECK
121802     END-IF
02166                                                                   EL008
02167      IF CM-LF-CURRENT-STATUS = '8'                                EL008
02168         IF CM-LF-CANCEL-DT NOT = LOW-VALUES                       EL008
02169             MOVE CM-LF-CANCEL-DT TO WS-CANC-DT.                   EL008
02170                                                                   EL008
02171      IF CM-LF-CURRENT-STATUS = '7'                                EL008
02172         IF CM-LF-DEATH-DT NOT = LOW-VALUES                        EL008
02173             MOVE CM-LF-DEATH-DT     TO WS-CANC-DT.                EL008
02174                                                                   EL008
02175      GO TO 2130-CONV-DATE.                                        EL008
02176                                                                   EL008
02177  2130-AH-CHECK.                                                   EL008
02178      IF CM-AH-CURRENT-STATUS = '8'                                EL008
02179         IF CM-AH-CANCEL-DT NOT = LOW-VALUES                       EL008
02180             MOVE CM-AH-CANCEL-DT TO WS-CANC-DT.                   EL008
02181                                                                   EL008
02182      IF CM-AH-CURRENT-STATUS = '6' OR '7'                         EL008
02183         IF CM-AH-SETTLEMENT-DT NOT = LOW-VALUES                   EL008
02184             MOVE CM-AH-SETTLEMENT-DT TO WS-CANC-DT.               EL008
02185                                                                   EL008
02186  2130-CONV-DATE.                                                  EL008
02187      MOVE 'CERT CANCEL-'       TO L21-CANC-LIT.                      CL*15
02188      IF WS-CANC-DT = LOW-VALUES OR SPACES  OR ZEROS               EL008
02189          MOVE SPACES TO P-CERT-CANC-DT                            EL008
02190      ELSE                                                         EL008
02191          MOVE WS-CANC-DT TO DC-BIN-DATE-1                         EL008
02192          MOVE ' '        TO DC-OPTION-CODE                        EL008
02193          PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    EL008
02194          MOVE DC-GREG-DATE-1-EDIT   TO P-CERT-CANC-DT.            EL008
02195                                                                   EL008
02196      MOVE 'ACCOUNT       - '        TO L19-ACCT-LIT.                 CL*15
02197      MOVE CM-ACCOUNT     TO P-ACCT.                               EL008
02198      MOVE CM-STATE       TO P-STATE.                              EL008
02199      MOVE CM-GROUPING    TO P-GROUP.                              EL008
02200      MOVE CM-REIN-TABLE  TO P-REIN-CODE.                          EL008
02201      MOVE CM-ENTRY-BATCH TO P-CERT-BATCH.                         EL008
02202      MOVE CM-MEMBER-NO   TO P-MEMBER-NO.                          EL008
02203                                                                   EL008
02204  2130-GET-BENEFIT-DESC.                                           EL008
02205      IF WS-BEN-CODE = ZERO                                        EL008
02206          MOVE '** NONE **' TO P-COVERAGE                          EL008
02207          GO TO 2130-EXIT.                                         EL008
02208                                                                   EL008
02209      MOVE WS-ACCESS            TO WS-CF-PROCESSOR.                EL008
02210      MOVE PI-COMPANY-ID        TO WS-CF-COMPANY-ID.               EL008
02211      MOVE +0                   TO WS-CF-SEQUENCE-NO.              EL008
02212      MOVE SPACES               TO WS-CNTL-ERROR-SW.               EL008
02213                                                                   EL008
02214      EXEC CICS HANDLE CONDITION                                   EL008
02215           ENDFILE (2130-EXIT)                                     EL008
02216           NOTFND  (2130-EXIT)                                     EL008
02217      END-EXEC.                                                    EL008
02218                                                                   EL008
02219      EXEC CICS READ                                               EL008
02220           DATASET ('ELCNTL')                                      EL008
02221           SET     (ADDRESS OF CONTROL-FILE)                          CL*17
02222           RIDFLD  (WS-CF-CONTROL-PRIMARY)                         EL008
02223           GTEQ                                                    EL008
02224      END-EXEC.                                                    EL008
02225                                                                   EL008
02226      IF WS-CF-COMPANY-ID  NOT = CF-COMPANY-ID  OR                 EL008
02227         WS-CF-RECORD-TYPE NOT = CF-RECORD-TYPE                    EL008
02228              GO TO 2130-EXIT.                                     EL008
02229                                                                   EL008
02230      PERFORM 2135-DUMMY THRU 2135-EXIT                               CL*15
02231          VARYING SUB1 FROM 1 BY 1 UNTIL                           EL008
02232          ((SUB1 GREATER 8) OR                                     EL008
02233               (CF-BENEFIT-CODE (SUB1) = WS-BEN-CODE)).               CL**5
02234                                                                   EL008
02235      IF SUB1 NOT = 9                                              EL008
02236          MOVE CF-BENEFIT-DESCRIP (SUB1) TO P-COVERAGE             EL008
02237      ELSE                                                         EL008
02238          MOVE 'CD MISSING'   TO P-COVERAGE.                       EL008
02239                                                                   EL008
02240  2130-EXIT.                                                       EL008
02241       EXIT.                                                       EL008
02242                                                                   EL008
02243  2135-DUMMY.                                                         CL*15
02244  2135-EXIT.                                                          CL*15
02245       EXIT.                                                          CL*15
02246       EJECT                                                          CL*15
02247  2140-MOVE-PLCY-INFO.                                                CL*15
02248                                                                      CL*15
02249      MOVE PM-COMPANY-CD              TO  WS-PP-COMPANY-CD.           CL*15
02250      MOVE PM-CARRIER                 TO  WS-PP-CARRIER.              CL*15
02251      MOVE PM-GROUPING                TO  WS-PP-GROUPING.             CL*15
02252      MOVE PM-STATE                   TO  WS-PP-STATE.                CL*15
02253      MOVE PM-PRODUCER                TO  WS-PP-PRODUCER.             CL*15
02254      MOVE PM-INS-PLAN-CD             TO  WS-PP-PLAN-CODE.            CL*15
02255      MOVE PM-INS-PLAN-REVISION       TO  WS-PP-REV-NO.               CL*15
02256                                                                      CL*15
02257      PERFORM 1610-READ-EMPLAN THRU 1610-EXIT.                        CL*15
02258                                                                      CL*15
02259      IF PLAN-READ-ERROR                                              CL*15
02260          MOVE '** NONE **'           TO  P-COVERAGE                  CL*15
02261          GO TO 2140-CONT-MOVE.                                       CL*15
02262                                                                      CL*15
02263      MOVE PP-PLAN-ABBREV             TO  P-COVERAGE.                 CL*15
02264      MOVE PP-REFUND-CALC             TO  CP-EARNING-METHOD           CL*15
02265                                          CP-RATING-METHOD.           CL*15
02266                                                                      CL*15
02267      IF PP-BENEFIT-IS-LEVEL                                          CL*15
02268          MOVE 'L'                    TO  CP-BENEFIT-TYPE             CL*15
02269      ELSE                                                            CL*15
02270          MOVE 'R'                    TO  CP-BENEFIT-TYPE.            CL*15
02271                                                                      CL*15
02272  2140-CONT-MOVE.                                                     CL*15
02273                                                                      CL*15
02274      MOVE PM-INS-PLAN-CD             TO  WS-BEN-CODE.                CL*15
02275      MOVE PM-INSURED-ISSUE-AGE       TO  WS-AGE.                     CL*15
02276      MOVE WS-AGE-3-4                 TO  P-INSURED-AGE.              CL*15
02277      MOVE PM-CURRENT-STATUS          TO  WS-STATUS.                  CL*15
02278      MOVE PM-LOAN-TERM               TO  P-TERM                      CL*15
02279                                          CP-ORIGINAL-TERM.           CL*15
02280                                                                      CL*15
052614     IF CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1 OR 'I' OR 'G' OR 'F'
02282          MOVE PM-INS-MONTH-BENEFIT   TO  P-ORIG-BENEF-AMT            CL*15
02283          MOVE WS-BIN-CURRENT-DT      TO  CP-VALUATION-DT             CL*15
02284      ELSE                                                            CL*15
02285          MOVE PM-INS-TOTAL-BENEFIT   TO  P-ORIG-BENEF-AMT            CL*15
02286          MOVE CL-INCURRED-DT         TO  CP-VALUATION-DT
           END-IF
02287                                                                      CL*15
02288      MOVE PM-INS-TERMINATION-DT      TO  DC-BIN-DATE-1.              CL*15
02289      MOVE ' '                        TO  DC-OPTION-CODE.             CL*15
02290      PERFORM 8100-DATE-RTN  THRU 8100-EXIT.                          CL*15
02291      MOVE DC-GREG-DATE-1-EDIT        TO  P-EXPIRE.                   CL*15
02292                                                                      CL*15
02293      MOVE PM-POLICY-EFF-DT           TO  CP-CERT-EFF-DT.             CL*15
02294                                                                      CL*15
02295      MOVE 'PLCY ENTRY    - '         TO  L22-ENTRY-LIT.              CL*15
02296      IF PM-ENTRY-DT = LOW-VALUES OR SPACES                           CL*15
02297          MOVE SPACES                 TO  P-CERT-ENTRY-YY             CL*15
02298                                          P-CERT-SL                   CL*15
02299                                          P-CERT-ENTRY-MM             CL*15
02300      ELSE                                                            CL*15
02301          MOVE PM-ENTRY-DT            TO  DC-BIN-DATE-1               CL*15
02302          MOVE ' '                    TO  DC-OPTION-CODE              CL*15
02303          PERFORM 8100-DATE-RTN  THRU 8100-EXIT                       CL*15
02304          MOVE DC-GREG-DATE-1-MDY     TO  WS-MM-DD-YY                 CL*15
02305          MOVE WS-MM                  TO  P-CERT-ENTRY-MM             CL*15
02306          MOVE WS-YY                  TO  P-CERT-ENTRY-YY             CL*15
02307          MOVE '/'                    TO  P-CERT-SL.                  CL*15
02308                                                                      CL*15
02309      MOVE 'PLCY ISSUE   - '          TO  L15-ISSUE-LIT.              CL*15
02310      MOVE PM-POLICY-EFF-DT           TO  DC-BIN-DATE-1.              CL*15
02311      MOVE ' '                        TO  DC-OPTION-CODE.             CL*15
02312      PERFORM 8100-DATE-RTN  THRU 8100-EXIT.                          CL*15
02313      MOVE DC-GREG-DATE-1-EDIT        TO  P-CERT-ISSUE-DT.            CL*15
02314                                                                      CL*15
02315      IF PM-AH-MORT-PLAN                                              CL*15
02316          MOVE '3'                    TO  CP-REM-TERM-METHOD          CL*15
02317          MOVE PM-LOAN-DT             TO  CP-FIRST-PAY-DATE           CL*15
02318      ELSE                                                            CL*15
02319          MOVE '2'                    TO  CP-REM-TERM-METHOD          CL*15
02320          MOVE PM-POLICY-EFF-DT       TO  CP-FIRST-PAY-DATE.          CL*15
02321                                                                      CL*19
02322 **     READ CNTL RECORD TO OBTAIN FREE LOOK DAYS                     CL*19
02323                                                                      CL*19
02324      MOVE PI-COMPANY-ID              TO WS-CF-COMPANY-ID.            CL*19
02325      MOVE PI-STATE                   TO WS-CF-PROCESSOR.             CL*19
02326      MOVE ZEROS                      TO WS-CF-SEQUENCE-NO.           CL*19
02327      MOVE '3'                        TO WS-CF-RECORD-TYPE.           CL*19
02328      MOVE SPACES                     TO WS-CNTL-ERROR-SW.            CL*19
02329                                                                      CL*19
02330      PERFORM 1300-READ-CNTL  THRU 1300-EXIT.                         CL*19
02331                                                                      CL*19
02332      IF NO-COMPANY-RECORD                                            CL*19
02333         MOVE    '*** ERROR NO STATE REC ***' TO WS-PASSED-DATA       CL*19
02334         GO TO 9999-FINALIZE                                          CL*19
02335      ELSE                                                            CL*19
02336         MOVE CF-ST-FREE-LOOK-PERIOD  TO CP-FREE-LOOK.                CL*19
02337                                                                      CL*15
02338      MOVE '1'                        TO  CP-REM-TRM-CALC-OPTION.     CL*15
02339      MOVE PI-COMPANY-ID              TO  CP-COMPANY-ID.              CL*15
02340      MOVE PM-COMPANY-CD              TO  CP-COMPANY-CD.              CL*15
02341                                                                      CL*15
02342      PERFORM 9800-LINK-REM-TERM THRU 9800-EXIT.                      CL*15
02343                                                                      CL*15
02344      IF PM-AH-MORT-PLAN                                              CL*15
02345          MOVE CP-REMAINING-TERM-1        TO  P-REM                   CL*15
02346      ELSE                                                            CL*15
02347          IF (PI-COMPANY-ID IS EQUAL TO 'CIG' OR 'CUK')               CL*15
02348              COMPUTE CP-REMAINING-TERM-3 = CP-REMAINING-TERM-3 + 1   CL*15
02349              MOVE CP-REMAINING-TERM-3    TO  P-REM                   CL*15
02350          ELSE                                                        CL*15
02351              MOVE CP-REMAINING-TERM-3    TO  P-REM.                  CL*15
02352                                                                      CL*15
02353      MOVE SPACES                     TO  P-CERT-STAT.                CL*15
02354      MOVE 'PLCY STATUS   - '         TO  L16-STATUS-LIT.             CL*15
02355                                                                      CL*15
02356      IF WS-STATUS IS EQUAL TO '0'                                    CL*15
02357          MOVE 'LAPSED'               TO  P-CERT-STAT.                CL*15
02358      IF WS-STATUS IS EQUAL TO '1'                                    CL*15
02359          MOVE 'ACTIVE'               TO  P-CERT-STAT.                CL*15
02360      IF WS-STATUS IS EQUAL TO '2'                                    CL*15
02361          MOVE 'PEND'                 TO  P-CERT-STAT.                CL*15
02362      IF WS-STATUS IS EQUAL TO '3'                                    CL*15
02363          MOVE 'DECLIN'               TO  P-CERT-STAT.                CL*15
02364      IF (WS-STATUS IS EQUAL TO '4' OR '9')                           CL*15
02365          MOVE 'PNDCNC'               TO  P-CERT-STAT.                CL*15
02366      IF WS-STATUS IS EQUAL TO '5'                                    CL*15
02367          MOVE 'PNDISS'               TO  P-CERT-STAT.                CL*15
02368      IF WS-STATUS IS EQUAL TO '6'                                    CL*15
02369          MOVE 'CLAIM'                TO  P-CERT-STAT.                CL*15
02370      IF WS-STATUS IS EQUAL TO '7'                                    CL*15
02371          MOVE 'CANCEL'               TO  P-CERT-STAT.                CL*15
02372      IF WS-STATUS IS EQUAL TO '8'                                    CL*15
02373          MOVE 'PNDUNW'               TO  P-CERT-STAT.                CL*15
02374      IF WS-STATUS IS EQUAL TO 'C'                                    CL*15
02375          MOVE 'TRNSFR'               TO  P-CERT-STAT.                CL*15
02376      IF WS-STATUS IS EQUAL TO 'F'                                    CL*15
02377          MOVE 'SETTLE'               TO  P-CERT-STAT.                CL*15
02378      IF WS-STATUS IS EQUAL TO 'T'                                    CL*15
02379          MOVE 'TRMNAT'               TO  P-CERT-STAT.                CL*15
02380                                                                      CL*15
02381      MOVE SPACES                     TO  WS-CANC-DT.                 CL*15
02382      MOVE 'PLCY CANCEL-'             TO  L21-CANC-LIT.               CL*15
02383                                                                      CL*15
02384      IF PM-CURRENT-STATUS IS EQUAL TO '7'                            CL*15
02385          IF PM-CANCEL-DT IS NOT EQUAL TO LOW-VALUES                  CL*15
02386              MOVE PM-CANCEL-DT       TO  WS-CANC-DT.                 CL*15
02387                                                                      CL*15
02388      IF (WS-CANC-DT EQUAL LOW-VALUES OR SPACES OR ZEROS)             CL*15
02389          MOVE SPACES                 TO  P-CERT-CANC-DT              CL*15
02390      ELSE                                                            CL*15
02391          MOVE WS-CANC-DT             TO  DC-BIN-DATE-1               CL*15
02392          MOVE ' '                    TO  DC-OPTION-CODE              CL*15
02393          PERFORM 8100-DATE-RTN  THRU 8100-EXIT                       CL*15
02394          MOVE DC-GREG-DATE-1-EDIT    TO  P-CERT-CANC-DT.             CL*15
02395                                                                      CL*15
02396      MOVE 'PRODUCER      - '         TO  L19-ACCT-LIT.               CL*15
02397      MOVE PM-PRODUCER                TO  P-ACCT.                     CL*15
02398      MOVE PM-STATE                   TO  P-STATE.                    CL*15
02399      MOVE PM-GROUPING                TO  P-GROUP.                    CL*15
02400                                                                      CL*15
02401  2140-EXIT.                                                       EL008
02402       EXIT.                                                       EL008
02403                                                                   EL008
02404      EJECT                                                           CL*15
02405  2150-MOVE-TR0-INFO.                                              EL008
02406      IF NOT RESERVE-EXPENSE-TR                                    EL008
02407          MOVE  '* TRAILER 0 NOT FIRST, ABORT*' TO WS-PASSED-DATA  EL008
02408          GO TO 9999-FINALIZE                                      EL008
02409      ELSE                                                         EL008
02410          MOVE AT-ITD-PAID-EXPENSES      TO  P-TOT-EXPENSE         EL008
02411          MOVE AT-INITIAL-MANUAL-RESERVE TO  P-ORIG-MANUAL         EL008
02412          MOVE AT-ITD-CHARGEABLE-EXPENSE TO  P-CHG-EXPENSE         EL008
02413          MOVE AT-CURRENT-MANUAL-RESERVE TO  P-REM-MANUAL          EL008
02414          MOVE AT-ITD-ADDITIONAL-RESERVE TO  P-ADD-RESERVE.        EL008
02415                                                                   EL008
02416      IF FULL-PRINT-REQUIRED                                       EL008
02417          PERFORM  2510-MOVE-OC-HISTORY  THRU 2510-EXIT.           EL008
02418                                                                   EL008
02419  2150-EXIT.                                                       EL008
02420       EXIT.                                                       EL008
02421                                                                   EL008
02422      EJECT                                                           CL*15
02423  2300-PRINT-TO-LINE-26.                                           EL008
02424      MOVE  ZEROS  TO WS-PAGE-CNT.                                 EL008
02425      PERFORM 4013-HEADING-RTN  THRU 4013-EXIT.                    EL008
02426                                                                   EL008
02427      MOVE TSP             TO WS-PASSED-CNTL-CHAR.                 EL008
02428      MOVE  HEAD-LINE-7    TO WS-PASSED-DATA.                      EL008
02429      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL008
02430                                                                   EL008
02431      MOVE DSP             TO WS-PASSED-CNTL-CHAR.                 EL008
02432      MOVE       LINE-9    TO WS-PASSED-DATA.                      EL008
02433      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL008
02434                                                                   EL008
02435      MOVE SSP             TO WS-PASSED-CNTL-CHAR.                    CL*17
02436      MOVE  HEAD-LINE-10   TO WS-PASSED-DATA.                         CL*17
02437      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                           CL*17
02438                                                                      CL*17
02439      MOVE DSP             TO WS-PASSED-CNTL-CHAR.                    CL*17
02440      MOVE       LINE-12   TO WS-PASSED-DATA.                      EL008
02441      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL008
02442                                                                   EL008
02443      MOVE SSP             TO WS-PASSED-CNTL-CHAR.                 EL008
02444      MOVE       LINE-13   TO WS-PASSED-DATA.                      EL008
02445      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL008
02446                                                                   EL008
02447      MOVE SSP             TO WS-PASSED-CNTL-CHAR.                 EL008
02448      MOVE       LINE-14   TO WS-PASSED-DATA.                      EL008
02449      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL008
02450                                                                   EL008
02451      MOVE SSP             TO WS-PASSED-CNTL-CHAR.                 EL008
02452      MOVE       LINE-15   TO WS-PASSED-DATA.                      EL008
02453      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL008
02454                                                                   EL008
02455      MOVE SSP             TO WS-PASSED-CNTL-CHAR.                 EL008
02456      MOVE       LINE-16   TO WS-PASSED-DATA.                      EL008
02457      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL008
02458                                                                   EL008
02459      MOVE SSP             TO WS-PASSED-CNTL-CHAR.                 EL008
02460      MOVE       LINE-17   TO WS-PASSED-DATA.                      EL008
02461      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL008
02462                                                                   EL008
02463      MOVE DSP             TO WS-PASSED-CNTL-CHAR.                 EL008
02464      MOVE       LINE-19   TO WS-PASSED-DATA.                      EL008
02465      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL008
02466                                                                   EL008
02467      MOVE SSP             TO WS-PASSED-CNTL-CHAR.                 EL008
02468      MOVE       LINE-20   TO WS-PASSED-DATA.                      EL008
02469      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL008
02470                                                                   EL008
02471      MOVE SSP             TO WS-PASSED-CNTL-CHAR.                 EL008
02472      MOVE       LINE-21   TO WS-PASSED-DATA.                      EL008
02473      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL008
02474                                                                   EL008
02475      MOVE SSP             TO WS-PASSED-CNTL-CHAR.                 EL008
02476      MOVE       LINE-22   TO WS-PASSED-DATA.                      EL008
02477      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL008
02478                                                                   EL008
02479      MOVE SSP             TO WS-PASSED-CNTL-CHAR.                 EL008
02480      MOVE       LINE-23   TO WS-PASSED-DATA.                      EL008
02481      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL008
02482                                                                   EL008
02483      MOVE SSP             TO WS-PASSED-CNTL-CHAR.                 EL008
02484      MOVE       LINE-23A  TO WS-PASSED-DATA.                      EL008
02485      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL008
02486                                                                   EL008
02487      MOVE DSP             TO WS-PASSED-CNTL-CHAR.                 EL008
02488      MOVE       LINE-25   TO WS-PASSED-DATA.                      EL008
02489      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL008
02490                                                                   EL008
02491      MOVE SSP             TO WS-PASSED-CNTL-CHAR.                 EL008
02492      MOVE       LINE-26   TO WS-PASSED-DATA.                      EL008
02493      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL008
02494                                                                   EL008
02495  2300-EXIT.                                                       EL008
02496       EXIT.                                                       EL008
02497      EJECT                                                        EL008
02498  2500-PROCESS-N-PRINT-TRAILERS.                                   EL008
02499 **   ADD  1  TO WS-AT-SEQ-NO.                                     EL008
02500      PERFORM  1750-READNEXT-TRLR  THRU 1750-EXIT.                 EL008
02501                                                                   EL008
02502      IF END-OF-TRLR-FILE                                          EL008
02503          MOVE  'X'  TO  WS-TRAILER-KEY-CHG-SW                     EL008
02504          GO TO 2500-EXIT.                                         EL008
02505                                                                   EL008
02506      MOVE AT-CONTROL-PRIMARY   TO  WS-NEW-AT-CONTROL-PRIMARY.     EL008
02507      IF WS-NEW-AT-CONTROL-WO-SEQ  NOT =                           EL008
02508                                   WS-AT-CONTROL-SAVED-WO-SEQ      EL008
02509          MOVE 'X'  TO WS-TRAILER-KEY-CHG-SW                       EL008
02510          GO TO 2500-EXIT.                                         EL008
02511                                                                   EL008
02512 *TR2                                                              EL008
02513      IF PAYMENT-TR                                                EL008
02514          PERFORM 2520-PROCESS-TR2   THRU 2520-EXIT                EL008
02515          GO TO 2500-EXIT.                                         EL008
02516 *TR3                                                              EL008
02517      IF AUTO-PAY-TR          AND                                  EL008
02518         FULL-PRINT-REQUIRED  AND                                  EL008
02519         AP-INDEX  LESS THAN 11                                    EL008
02520          PERFORM 2530-PROCESS-TR3  THRU 2530-EXIT                 EL008
02521          GO TO 2500-EXIT.                                         EL008
02522 *TR4                                                              EL008
02523      IF CORRESPONDENCE-TR                                         EL008
02524          PERFORM 2540-PROCESS-TR4    THRU 2540-EXIT               EL008
02525          GO TO 2500-EXIT.                                         EL008
02526 *TR5                                                              EL008
02527      IF ADDRESS-TR            AND                                 EL008
02528         FULL-PRINT-REQUIRED   AND                                 EL008
02529         AD-INDEX  LESS THAN 60                                       CL**6
02530          PERFORM 2550-PROCESS-TR5    THRU 2550-EXIT               EL008
02531          GO TO 2500-EXIT.                                         EL008
02532 *TR6                                                              EL008
02533      IF GENERAL-INFO-TR                                           EL008
02534          PERFORM 2560-PROCESS-TR6     THRU 2560-EXIT              EL008
02535          GO TO 2500-EXIT.                                         EL008
02536 *TR7                                                              EL008
02537      IF AUTO-PROMPT-TR                                            EL008
02538          PERFORM 2570-PROCESS-TR7     THRU 2570-EXIT              EL008
02539          GO TO 2500-EXIT.                                         EL008
02540 *TR8                                                              EL008
02541      IF DENIAL-TR                                                 EL008
02542          PERFORM 2580-PROCESS-TR8    THRU 2580-EXIT               EL008
02543          GO TO 2500-EXIT.                                         EL008
02544 *TR9                                                              EL008
02545      IF INCURRED-CHG-TR                                           EL008
02546          PERFORM 2590-PROCESS-TR9    THRU 2590-EXIT               EL008
02547          GO TO 2500-EXIT.                                         EL008
02548 *TRA                                                              EL008
02549      IF FORM-CONTROL-TR                                           EL008
02550          PERFORM 2600-PROCESS-TRA    THRU 2600-EXIT               EL008
02551          GO TO 2500-EXIT.                                         EL008
02552                                                                   EL008
02553  2500-EXIT.                                                       EL008
02554       EXIT.                                                       EL008
02555                                                                   EL008
02556  2510-MOVE-OC-HISTORY.                                            EL008
02557      MOVE  1  TO OC-SUB.                                          EL008
02558      SET  OC-INDEX TO 1.                                          EL008
02559      PERFORM  2511-LOAD-OC-TABLE  THRU 2511-EXIT  6 TIMES.        EL008
02560                                                                   EL008
02561  2510-EXIT.                                                       EL008
02562       EXIT.                                                       EL008
02563                                                                   EL008
02564  2511-LOAD-OC-TABLE.                                              EL008
02565      IF AT-OPEN-CLOSE-DATE (OC-SUB) = SPACES OR LOW-VALUES        EL008
02566           ADD 1 TO OC-SUB                                         EL008
02567           GO TO 2511-EXIT.                                        EL008
02568                                                                   EL008
02569      MOVE  'X'  TO END-OC-TABLE-SW.                               EL008
02570                                                                   EL008
02571      MOVE AT-OPEN-CLOSE-DATE (OC-SUB)   TO                        EL008
02572                         OC-TBL-OPCL-DT (OC-INDEX).                EL008
02573      MOVE AT-OPEN-CLOSE-TYPE (OC-SUB)   TO                        EL008
02574                         OC-TBL-OPCL-TYPE (OC-INDEX).              EL008
02575      MOVE AT-OPEN-CLOSE-REASON (OC-SUB) TO                        EL008
02576                         OC-TBL-OPCL-REASON (OC-INDEX).            EL008
02577                                                                   EL008
02578      SET OC-INDEX  UP BY 1.                                       EL008
02579      ADD 1  TO OC-SUB.                                            EL008
02580                                                                   EL008
02581  2511-EXIT.                                                       EL008
02582       EXIT.                                                       EL008
02583                                                                   EL008
02584      EJECT                                                           CL*15
02585  2520-PROCESS-TR2.                                                EL008
02586 ******************************************************************   CL*15
02587 ***          BUILD PAYMENT INFORMATION                         ***   CL*15
02588 ******************************************************************   CL*15
02589                                                                      CL*15
02590      IF  WS-LINE-CNT  GREATER THAN  50                            EL008
02591          PERFORM  4013-HEADING-RTN  THRU 4013-EXIT                EL008
02592          PERFORM  1014-HEADING-CONT THRU 1014-EXIT.               EL008
02593                                                                   EL008
091113     IF AT-PAYMENT-TYPE = 'T'
091113         MOVE 'TRANSFR PMT' TO P-PAY-ACT-TYPE
091113     ELSE
091113       IF AT-PAYMENT-TYPE = 'I'
091113          MOVE 'INTERST PMT' TO P-PAY-ACT-TYPE
091113       ELSE
091113          MOVE AT-PAYMENT-TYPE TO WS-SUB
091113          IF WS-SUB < 1 OR > 6
091113              MOVE 2 TO WS-SUB
091113          END-IF
091113          MOVE PAY-DESC (WS-SUB) TO P-PAY-ACT-TYPE
091113       END-IF
091113     END-IF.
091113
02594      MOVE AT-PAYEES-NAME   TO P-PAY-PAYEE.                        EL008
02595      MOVE AT-PAYEE-TYPE-CD TO P-PAY-PAYEE-CD.                     EL008
02596                                                                   EL008
02597      IF AT-RECORDED-DT  = LOW-VALUES OR SPACES                    EL008
02598          MOVE SPACE TO P-PAY-PMT-DT                               EL008
02599      ELSE                                                         EL008
02600          MOVE  AT-RECORDED-DT      TO DC-BIN-DATE-1               EL008
02601          MOVE  ' '   TO DC-OPTION-CODE                            EL008
02602          PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    EL008
02603          MOVE DC-GREG-DATE-1-EDIT  TO P-PAY-PMT-DT.               EL008
02604                                                                   EL008
02605      MOVE AT-RECORDED-BY  TO P-PAY-BY.                            EL008
02606                                                                   EL008
02607      IF AT-CHECK-WRITTEN-DT  = LOW-VALUES OR SPACES               EL008
02608          MOVE ZEROS TO P-PAY-WRIT-DT                              EL008
02609      ELSE                                                         EL008
02610          MOVE AT-CHECK-WRITTEN-DT TO  DC-BIN-DATE-1               EL008
02611          MOVE  ' '                TO DC-OPTION-CODE               EL008
02612          PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    EL008
02613          MOVE DC-GREG-DATE-1-EDIT TO  P-PAY-WRIT-DT.              EL008
02614                                                                   EL008
02615      MOVE AT-CHECK-NO         TO  P-PAY-CHECK.                    EL008
02616      MOVE AT-AMOUNT-PAID      TO  P-PAY-PMT-AMT.                  EL008
02617      MOVE AT-ADDL-RESERVE     TO  P-PAY-RESERVE.                  EL008
02618      MOVE AT-EXPENSE-PER-PMT  TO  P-PAY-EXPEN.                    EL008
02619                                                                   EL008
02620      IF AT-PMT-ACCEPT-DT  = LOW-VALUES OR SPACES                  EL008
02621          MOVE SPACES             TO P-PAY-CREDIT-DT               EL008
02622      ELSE                                                         EL008
02623          MOVE AT-PMT-ACCEPT-DT   TO DC-BIN-DATE-1                 EL008
02624          MOVE  ' '               TO DC-OPTION-CODE                EL008
02625          PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    EL008
02626          MOVE DC-GREG-DATE-1-EDIT TO  P-PAY-CREDIT-DT.            EL008
02627                                                                   EL008
02628      IF AT-VOID-DT = LOW-VALUES OR SPACES                         EL008
02629          MOVE SPACES              TO  P-PAY-VOID-DT               EL008
02630                                       P-PAY-REASON                EL008
02631      ELSE                                                         EL008
02632          MOVE AT-VOID-DT          TO  DC-BIN-DATE-1               EL008
02633          MOVE  ' '                TO DC-OPTION-CODE               EL008
02634          PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    EL008
02635          MOVE DC-GREG-DATE-1-EDIT TO  P-PAY-VOID-DT               EL008
02636          MOVE AT-VOID-REASON      TO  P-PAY-REASON.               EL008
02637                                                                   EL008
02638      IF AT-PAYMENT-TYPE EQUAL '5' OR '6'                             CL**5
02639         MOVE AT-EXPENSE-TYPE    TO P-EXP-TYPE                        CL**5
02640         GO TO 2521-CONTINUE.                                         CL**5
02641                                                                      CL**5
02642      MOVE AT-PAID-FROM-DT     TO DC-BIN-DATE-1                       CL**5
02643      MOVE  ' '                TO DC-OPTION-CODE                      CL**5
02644      PERFORM 8100-DATE-RTN    THRU 8100-EXIT                         CL**5
02645      MOVE DC-GREG-DATE-1-EDIT TO  P-PAY-FROM-DT                      CL**5
02646                                                                      CL**5
02647      IF NOT PI-USES-PAID-TO                                          CL**5
02648         MOVE AT-PAID-THRU-DT     TO DC-BIN-DATE-1                    CL**5
02649         MOVE  ' '                TO DC-OPTION-CODE                   CL**5
02650         PERFORM 8100-DATE-RTN    THRU 8100-EXIT                      CL**5
02651         MOVE DC-GREG-DATE-1-EDIT TO  P-PAY-THRU-DT                   CL**5
02652      ELSE                                                            CL**5
02653         MOVE '  TO  - '          TO L-5B-PD-THRU                     CL**5
02654         MOVE AT-PAID-THRU-DT     TO DC-BIN-DATE-1                    CL**5
02655         MOVE +1                  TO DC-ELAPSED-DAYS                  CL**5
02656         MOVE +0                  TO DC-ELAPSED-MONTHS                CL**5
02657         MOVE  '6'                TO DC-OPTION-CODE                   CL**5
02658         PERFORM 8100-DATE-RTN    THRU 8100-EXIT                      CL**5
02659         MOVE DC-GREG-DATE-1-EDIT TO  P-PAY-THRU-DT.                  CL**5
02660                                                                      CL**5
02661      MOVE AT-DAYS-IN-PERIOD   TO  P-PAY-DAYS                         CL**5
02662      MOVE AT-DAILY-RATE       TO  P-PAY-RATE.                        CL**5
02663                                                                      CL**5
02664  2521-CONTINUE.                                                      CL**5
02665                                                                   EL008
02666      IF ONLINE-MANUAL-PMT                                         EL008
02667           MOVE 'ONLINE'   TO P-PAY-ORIGIN.                        EL008
02668      IF ONLINE-AUTO-PMT                                           EL008
02669           MOVE 'ONLINE'   TO P-PAY-ORIGIN.                        EL008
02670      IF OFFLINE-PMT                                               EL008
02671           MOVE 'OFFLINE'  TO P-PAY-ORIGIN.                        EL008
02672                                                                   EL008
02673      IF AT-CV-PMT-CODE IS NOT EQUAL TO ' '                           CL*15
02674          GO TO 2522-CONTINUE.                                        CL*15
02675                                                                      CL*15
02676      IF PARTIAL-PAYMENT                                           EL008
02677           MOVE 'PARTIAL PAYMENT'         TO P-PAY-TYPE.           EL008
02678      IF FINAL-PAYMENT                                             EL008
02679           MOVE 'FINAL PAYMENT'           TO P-PAY-TYPE.           EL008
02680      IF LUMP-SUM-PAYMENT                                          EL008
02681           MOVE 'LUMP SUM PAYMENT'        TO P-PAY-TYPE.           EL008
02682      IF ADDITIONAL-PAYMENT                                        EL008
02683           MOVE 'ADDITIONAL PAYMENT'      TO P-PAY-TYPE.           EL008
02684      IF CHARGEABLE-EXPENSE                                        EL008
02685           MOVE 'CHARGEABLE EXPENSE'      TO P-PAY-TYPE.           EL008
02686      IF NON-CHARGEABLE-EXPENSE                                    EL008
02687           MOVE 'NON CHARGEABLE EXPENSE'  TO P-PAY-TYPE.           EL008
02688                                                                   EL008
02689      MOVE DSP           TO WS-PASSED-CNTL-CHAR.                   EL008
02690      MOVE  P-PAY-LINE-1 TO WS-PASSED-DATA.                        EL008
02691      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL008
02692                                                                   EL008
02693      MOVE SSP           TO WS-PASSED-CNTL-CHAR.                   EL008
02694      MOVE  P-PAY-LINE-2 TO WS-PASSED-DATA.                        EL008
02695      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL008
02696                                                                   EL008
02697      MOVE SSP           TO WS-PASSED-CNTL-CHAR.                   EL008
02698      MOVE  P-PAY-LINE-3 TO WS-PASSED-DATA.                        EL008
02699      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL008
02700                                                                   EL008
02701      MOVE SSP                    TO WS-PASSED-CNTL-CHAR.          EL008
02702      MOVE  P-PAY-LINE-4          TO WS-PASSED-DATA.               EL008
02703      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL008
02704                                                                   EL008
02705      IF AT-PAYMENT-TYPE = '5' OR '6'                              EL008
02706          MOVE SSP                TO WS-PASSED-CNTL-CHAR           EL008
02707          MOVE P-PAY-LINE-5C      TO WS-PASSED-DATA                EL008
02708          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                      EL008
02709      ELSE                                                         EL008
02710          IF P-PAY-VOID-DT = SPACES                                EL008
02711              MOVE SSP            TO WS-PASSED-CNTL-CHAR           EL008
02712              MOVE  P-PAY-LINE-5B TO WS-PASSED-DATA                EL008
02713              PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT                 EL008
02714          ELSE                                                     EL008
02715              MOVE SSP            TO WS-PASSED-CNTL-CHAR           EL008
02716              MOVE  P-PAY-LINE-5A TO WS-PASSED-DATA                EL008
02717              PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT                 EL008
02718              MOVE SSP            TO WS-PASSED-CNTL-CHAR           EL008
02719              MOVE  P-PAY-LINE-5B TO WS-PASSED-DATA                EL008
02720              PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                EL008
02721                                                                   EL008
02722      GO TO 2520-EXIT.                                                CL*15
02723                                                                      CL*15
02724  2522-CONTINUE.                                                      CL*15
02725                                                                      CL*15
02726      IF AT-CV-PMT-CODE IS EQUAL TO '1'                               CL*15
02727          MOVE 'FULL DEATH PAYMENT'       TO  P-PAY-TYPE.             CL*15
02728      IF AT-CV-PMT-CODE IS EQUAL TO '2'                               CL*15
02729          MOVE 'HALF DEATH PAYMENT'       TO  P-PAY-TYPE.             CL*15
02730      IF AT-CV-PMT-CODE IS EQUAL TO '3'                               CL*15
02731          MOVE 'FULL AD&D PAYMENT'        TO  P-PAY-TYPE.             CL*15
02732      IF AT-CV-PMT-CODE IS EQUAL TO '4'                               CL*15
02733          MOVE 'HALF AD&D PAYMENT'        TO  P-PAY-TYPE.             CL*15
02734      IF AT-CV-PMT-CODE IS EQUAL TO '5'                               CL*15
02735          MOVE 'FULL RIDER PAYMENT'       TO  P-PAY-TYPE.             CL*15
02736      IF AT-CV-PMT-CODE IS EQUAL TO '6'                               CL*15
02737          MOVE 'HALF RIDER PAYMENT'       TO  P-PAY-TYPE.             CL*15
02738      IF AT-CV-PMT-CODE IS EQUAL TO '7'                               CL*15
02739          MOVE 'NON CHARGEABLE EXPENSE'   TO  P-PAY-TYPE.             CL*15
02740      IF AT-CV-PMT-CODE IS EQUAL TO '8'                               CL*15
02741          MOVE 'ADDITIONAL PAYMENT'       TO  P-PAY-TYPE.             CL*15
02742                                                                      CL*15
02743      MOVE DSP                        TO  WS-PASSED-CNTL-CHAR.        CL*15
02744      MOVE P-PAY-LINE-1               TO  WS-PASSED-DATA.             CL*15
02745      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                            CL*15
02746                                                                      CL*15
02747      MOVE SSP                        TO  WS-PASSED-CNTL-CHAR.        CL*15
02748      MOVE P-PAY-LINE-2               TO  WS-PASSED-DATA.             CL*15
02749      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                            CL*15
02750                                                                      CL*15
02751      MOVE SSP                        TO  WS-PASSED-CNTL-CHAR.        CL*15
02752      MOVE P-PAY-LINE-3               TO  WS-PASSED-DATA.             CL*15
02753      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                            CL*15
02754                                                                      CL*15
02755      MOVE SSP                        TO  WS-PASSED-CNTL-CHAR.        CL*15
02756      MOVE P-PAY-LINE-4               TO  WS-PASSED-DATA.             CL*15
02757      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                            CL*15
02758                                                                      CL*15
02759      IF AT-CV-PMT-CODE IS EQUAL TO '8'                               CL*15
02760          MOVE SSP                    TO  WS-PASSED-CNTL-CHAR         CL*15
02761          MOVE P-PAY-LINE-5C          TO  WS-PASSED-DATA              CL*15
02762          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                         CL*15
02763      ELSE                                                            CL*15
02764          IF P-PAY-VOID-DT IS EQUAL TO SPACES                         CL*15
02765              MOVE SSP                TO  WS-PASSED-CNTL-CHAR         CL*15
02766              MOVE P-PAY-LINE-5B      TO  WS-PASSED-DATA              CL*15
02767              PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                     CL*15
02768          ELSE                                                        CL*15
02769              MOVE SSP                TO  WS-PASSED-CNTL-CHAR         CL*15
02770              MOVE P-PAY-LINE-5A      TO  WS-PASSED-DATA              CL*15
02771              PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                     CL*15
02772              MOVE P-PAY-LINE-5B      TO  WS-PASSED-DATA              CL*15
02773              PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                    CL*15
02774                                                                      CL*15
02775  2520-EXIT.                                                       EL008
02776       EXIT.                                                       EL008
02777                                                                   EL008
02778      EJECT                                                           CL*15
02779  2530-PROCESS-TR3.                                                EL008
02780 ******************************************************************   CL*15
02781 ***          BUILD AUTO PAYMENT SCHEDULE INFORMATION           ***   CL*15
02782 ******************************************************************   CL*15
02783                                                                      CL*15
02784      MOVE  'X'                    TO END-AP-TABLE-SW.             EL008
02785      MOVE AT-RECORDED-DT          TO AP-TBL-EST-DT (AP-INDEX).    EL008
02786      MOVE AT-RECORDED-BY          TO AP-TBL-EST-BY (AP-INDEX).    EL008
02787      MOVE AT-TERMINATED-DT        TO AP-TBL-TERM-DT (AP-INDEX).   EL008
02788      MOVE AT-SCHEDULE-START-DT TO                                 EL008
02789                        AP-TBL-SCHED-START-DT (AP-INDEX).          EL008
02790      MOVE AT-SCHEDULE-END-DT      TO                              EL008
02791                        AP-TBL-SCHED-END-DT (AP-INDEX).            EL008
02792      MOVE AT-LAST-PMT-TYPE        TO AP-TBL-LAST-TYPE (AP-INDEX). EL008
02793      MOVE AT-FIRST-PMT-AMT        TO AP-TBL-FIRST-AMT (AP-INDEX). EL008
02794      MOVE AT-DAYS-IN-1ST-PMT      TO AP-TBL-FIRST-DAYS (AP-INDEX).EL008
02795      MOVE AT-1ST-PAY-THRU-DT      TO AP-TBL-FIRST-DT (AP-INDEX).  EL008
02796      MOVE AT-REGULAR-PMT-AMT      TO AP-TBL-REG-AMT (AP-INDEX).   EL008
02797      MOVE AT-INTERVAL-MONTHS      TO AP-TBL-INT-MO (AP-INDEX).    EL008
02798      SET  AP-INDEX  UP BY 1.                                      EL008
02799                                                                   EL008
02800  2530-EXIT.                                                       EL008
02801       EXIT.                                                       EL008
02802                                                                   EL008
02803      EJECT                                                           CL*15
02804  2540-PROCESS-TR4.                                                EL008
02805 ******************************************************************   CL*15
02806 ***          BUILD CORRESPONDENCE INFORMATION                  ***   CL*15
02807 ******************************************************************   CL*15
02808                                                                      CL*15
02809      IF  WS-LINE-CNT  GREATER THAN  50                            EL008
02810          PERFORM  4013-HEADING-RTN  THRU 4013-EXIT                EL008
02811          PERFORM  1014-HEADING-CONT THRU 1014-EXIT.               EL008
02812                                                                   EL008
02813      MOVE AT-RECORDED-DT      TO DC-BIN-DATE-1                    EL008
02814      MOVE  ' '                TO DC-OPTION-CODE                   EL008
02815      PERFORM 8100-DATE-RTN  THRU 8100-EXIT                        EL008
02816      MOVE DC-GREG-DATE-1-EDIT TO  P-LET-LET-DT.                   EL008
02817      MOVE AT-RECORDED-BY      TO  P-LET-BY.                       EL008
02818                                                                   EL008
02819      MOVE AT-ADDRESEE-TYPE           TO P-LET-ADSEE-CD            EL008
02820      MOVE AT-STD-LETTER-FORM         TO P-LET-FORM.               EL008
02821      MOVE AT-LETTER-ARCHIVE-NO       TO P-LET-ARCH.               EL008
02822      MOVE AT-REASON-TEXT             TO P-LET-RE.                 EL008
02823                                                                   EL008
02824      IF AT-LETTER-SENT-DT = LOW-VALUES OR SPACES                  EL008
091113         MOVE 'MAIL RCVD ' TO P-LET-ACT-TYPE
02825          MOVE ZEROS TO P-LET-SENT-DT                              EL008
02826      ELSE                                                         EL008
091113         MOVE 'LETTER    ' TO P-LET-ACT-TYPE
02827          MOVE AT-LETTER-SENT-DT      TO DC-BIN-DATE-1             EL008
02828          MOVE  ' '                   TO DC-OPTION-CODE            EL008
02829          PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    EL008
02830          MOVE DC-GREG-DATE-1-EDIT TO  P-LET-SENT-DT.              EL008
02831                                                                   EL008
02832      IF AT-AUTO-RE-SEND-DT = LOW-VALUES OR SPACES                 EL008
02833          MOVE ZEROS TO P-LET-SEND-DT                              EL008
02834      ELSE                                                         EL008
02835          MOVE AT-AUTO-RE-SEND-DT     TO DC-BIN-DATE-1             EL008
02836          MOVE  ' '                   TO DC-OPTION-CODE            EL008
02837          PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    EL008
02838          MOVE DC-GREG-DATE-1-EDIT TO  P-LET-SEND-DT.              EL008
02839                                                                   EL008
02840      IF AT-RECEIPT-FOLLOW-UP = LOW-VALUES OR SPACES               EL008
02841          MOVE SPACE TO P-LET-FOL-DT                               EL008
02842      ELSE                                                         EL008
02843          MOVE AT-RECEIPT-FOLLOW-UP     TO DC-BIN-DATE-1           EL008
02844          MOVE  ' '                     TO DC-OPTION-CODE          EL008
02845          PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    EL008
02846          MOVE DC-GREG-DATE-1-EDIT TO  P-LET-FOL-DT.               EL008
02847                                                                   EL008
02848      IF AT-LETTER-ANSWERED-DT  = LOW-VALUES OR SPACES             EL008
02849          MOVE SPACE TO P-LET-ANS-DT                               EL008
02850      ELSE                                                         EL008
02851          MOVE AT-LETTER-ANSWERED-DT    TO DC-BIN-DATE-1           EL008
02852          MOVE  ' '                     TO DC-OPTION-CODE          EL008
02853          PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    EL008
02854          MOVE DC-GREG-DATE-1-EDIT TO  P-LET-ANS-DT.               EL008
02855                                                                   EL008
02856      MOVE SPACES  TO P-LET-ORIGIN                                 EL008
02857                      P-LET-ADSEE.                                 EL008
02858                                                                   EL008
02859      IF  ONLINE-CREATION                                          EL008
02860           MOVE 'ONLINE '       TO P-LET-ORIGIN.                   EL008
02861      IF  OFFLINE-CREATION                                         EL008
02862           MOVE 'OFFLINE'       TO P-LET-ORIGIN.                   EL008
02863      IF  INSURED-ADDRESEE                                         EL008
02864           MOVE 'INSURED'       TO P-LET-ADSEE.                    EL008
02865      IF  BENEFICIARY-ADDRESEE                                     EL008
02866           MOVE 'BENEFICIARY'   TO P-LET-ADSEE.                    EL008
02867      IF  ACCOUNT-ADDRESEE                                         EL008
02868           MOVE 'ACCOUNT'       TO P-LET-ADSEE.                    EL008
02869      IF  PHYSICIAN-ADDRESEE                                       EL008
02870           MOVE 'PHYSICIAN'     TO P-LET-ADSEE.                    EL008
02871      IF  EMPLOYER-ADDRESEE                                        EL008
02872           MOVE 'EMPLOYER'      TO P-LET-ADSEE.                    EL008
02873      IF  OTHER-ADDRESEE-1                                         EL008
02874           MOVE 'OTHER 1'       TO P-LET-ADSEE.                    EL008
02875      IF  OTHER-ADDRESEE-2                                         EL008
02876           MOVE 'OTHER 2'       TO P-LET-ADSEE.                    EL008
02877                                                                   EL008
02878      MOVE DSP           TO WS-PASSED-CNTL-CHAR.                   EL008
02879      MOVE  P-LET-LINE-1 TO WS-PASSED-DATA.                        EL008
02880      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL008
02881                                                                   EL008
02882      MOVE SSP           TO WS-PASSED-CNTL-CHAR.                   EL008
02883      MOVE  P-LET-LINE-2 TO WS-PASSED-DATA.                        EL008
02884      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL008
02885                                                                   EL008
02886      MOVE SSP           TO WS-PASSED-CNTL-CHAR.                   EL008
02887      MOVE  P-LET-LINE-3 TO WS-PASSED-DATA.                        EL008
02888      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL008
02889                                                                   EL008
02890      MOVE SSP           TO WS-PASSED-CNTL-CHAR.                   EL008
02891      MOVE  P-LET-LINE-4 TO WS-PASSED-DATA.                        EL008
02892      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL008
02893                                                                   EL008
02894      IF P-LET-RE = SPACES                                         EL008
02895          NEXT SENTENCE                                            EL008
02896      ELSE                                                         EL008
02897          MOVE SSP           TO WS-PASSED-CNTL-CHAR                EL008
02898          MOVE  P-LET-LINE-5 TO WS-PASSED-DATA                     EL008
02899          PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                    EL008
02900                                                                   EL008
02901  2540-EXIT.                                                       EL008
02902       EXIT.                                                       EL008
02903                                                                   EL008
02904      EJECT                                                           CL*15
02905  2550-PROCESS-TR5.                                                EL008
02906 ******************************************************************   CL*15
02907 ***          BUILD ADDRESS INFORMATION                         ***   CL*15
02908 ******************************************************************   CL*15
02909                                                                      CL*15
02910      MOVE  'X'                  TO END-AD-TABLE-SW.               EL008
02911      MOVE  AT-ADDRESS-TYPE      TO AD-TBL-TYPE    (AD-INDEX).     EL008
02912      MOVE  AT-MAIL-TO-NAME      TO AD-TBL-NAME    (AD-INDEX).     EL008
02913      MOVE  AT-ADDRESS-LINE-1    TO AD-TBL-ADDR-1  (AD-INDEX).     EL008
02914      MOVE  AT-ADDRESS-LINE-2    TO AD-TBL-ADDR-2  (AD-INDEX).     EL008
051810     MOVE  SPACES               TO AD-TBL-CITY    (AD-INDEX).     EL008
051810     STRING AT-CITY ' ' AT-STATE
051810        DELIMITED BY '  ' INTO AD-TBL-CITY (AD-INDEX)
051810     END-STRING
02916                                                                      CL*12
02917      MOVE SPACES                 TO WS-ZIP-WORK.                     CL*12
02918      IF AT-CANADIAN-POST-CODE                                        CL*12
02919          MOVE AT-CAN-POSTAL-1    TO WS-CAN-POSTAL-1                  CL*12
02920          MOVE AT-CAN-POSTAL-2    TO WS-CAN-POSTAL-2                  CL*12
02921      ELSE                                                            CL*12
02922          MOVE AT-ZIP-CODE        TO WS-ZIP-PRIME                     CL*12
02923          IF AT-ZIP-PLUS4 NOT = SPACES  AND  ZEROS                    CL*12
02924              MOVE '-'            TO WS-ZIP-DASH                      CL*12
02925              MOVE AT-ZIP-PLUS4   TO WS-ZIP-PLUS4.                    CL*12
02926      MOVE WS-ZIP-WORK            TO AD-TBL-ZIP    (AD-INDEX).        CL*12
02927                                                                      CL*12
02928      MOVE  AT-PHONE-NO          TO AD-TBL-PHONE   (AD-INDEX).     EL008
02929      SET AD-INDEX  UP BY 1.                                       EL008
02930                                                                   EL008
02931  2550-EXIT.                                                       EL008
02932       EXIT.                                                       EL008
02933                                                                   EL008
02934      EJECT                                                           CL*15
02935  2560-PROCESS-TR6.                                                EL008
02936 ******************************************************************   CL*15
02937 ***          BUILD CLAIM NOTES INFORMATION                     ***   CL*15
02938 ******************************************************************   CL*15
02939                                                                      CL*15
02940      IF  WS-LINE-CNT  GREATER THAN  50                            EL008
02941          PERFORM  4013-HEADING-RTN  THRU 4013-EXIT                EL008
02942          PERFORM  1014-HEADING-CONT THRU 1014-EXIT.               EL008
02943                                                                   EL008
091113     IF AT-MAINT-NOTE
091113         MOVE 'MAINT NOTES'     TO P-NOT-ACT-TYPE
091113     ELSE
091113       IF AT-CALL-NOTE
091113          IF AT-PHONE-CALL-IN
091113             MOVE 'CALL  IN   '  TO P-NOT-ACT-TYPE
091113          ELSE
091113             MOVE 'CALL  OUT  '  TO P-NOT-ACT-TYPE
091113          END-IF
091113       ELSE
091113         IF AT-CERT-CHANGE
091113             MOVE 'CERT CHANGE'  TO P-NOT-ACT-TYPE
091113         ELSE
091113           IF AT-APPROVAL-NOTE
091113              MOVE 'APPROVL REV' TO P-NOT-ACT-TYPE
091113           ELSE
091113             IF AT-NOTE-FILE-NOTE
091113                MOVE 'NOTE & FILE' TO P-NOT-ACT-TYPE
091113             ELSE
091113                MOVE 'NOTE       ' TO P-NOT-ACT-TYPE
091113             END-IF
091113           END-IF
091113         END-IF
091113       END-IF
091113     END-IF.
091113
091113     IF AT-SEQUENCE-NO = +90
091113         MOVE 'DIAGNOSIS  '     TO P-NOT-ACT-TYPE
091113     END-IF.
091113
091113     IF AT-SEQUENCE-NO = +91
091113         MOVE 'LOAN INFO  '     TO P-NOT-ACT-TYPE
091113     END-IF.
091113
091113     IF AT-SEQUENCE-NO = +92
091113         MOVE 'SPEC REVIEW'     TO P-NOT-ACT-TYPE
091113     END-IF.
091113
091113     IF AT-SEQUENCE-NO = +93
091113         MOVE 'VERIFY SSN '     TO P-NOT-ACT-TYPE
091113     END-IF.
091113
091113     IF AT-SEQUENCE-NO = +94
091113         MOVE 'CAUSL STATE'     TO P-NOT-ACT-TYPE
091113     END-IF.
091113
02944      MOVE  AT-INFO-LINE-1       TO P-NOT-TEXT-1.                  EL008
02945      MOVE  AT-INFO-LINE-2       TO P-NOT-TEXT-2.                  EL008
02946      MOVE  AT-RECORDED-BY       TO P-NOT-BY.                      EL008
02947                                                                   EL008
02948      IF AT-RECORDED-DT = LOW-VALUES OR SPACES                     EL008
02949          MOVE SPACES TO P-NOT-NOTE-DT                             EL008
02950      ELSE                                                         EL008
02951          MOVE  AT-RECORDED-DT    TO DC-BIN-DATE-1                 EL008
02952          MOVE  ' '               TO DC-OPTION-CODE                EL008
02953          PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    EL008
02954          MOVE DC-GREG-DATE-1-EDIT TO  P-NOT-NOTE-DT.              EL008
02955                                                                   EL008
101713     IF (AT-SEQUENCE-NO = +95)
020816        AND (PI-COMPANY-ID = 'DCC' OR 'VPP')
101713         PERFORM VARYING A1 FROM +1 BY +1 UNTIL
101713              AT-NOTE-ERROR-NO (A1) = SPACES
101713              MOVE AT-NOTE-ERROR-NO (A1)
101713                                 TO EMI-ERROR
101713              MOVE '2'           TO EMI-SWITCH1
101713              IF AT-NOTE-ERROR-NO (A1) = '1653'
101713                 EVALUATE TRUE
101713                    WHEN CL-CLAIM-TYPE = 'L'
101713                       MOVE '  LF  '
101713                              TO EMI-CLAIM-TYPE
101713                    WHEN CL-CLAIM-TYPE = 'I'
101713                       MOVE '  IU  '
101713                              TO EMI-CLAIM-TYPE
052614                    WHEN CL-CLAIM-TYPE = 'F'
052614                       MOVE '  FL  '
052614                              TO EMI-CLAIM-TYPE
080322                    WHEN CL-CLAIM-TYPE = 'B'
080322                       MOVE ' BR  '          TO EMI-CLAIM-TYPE
080322
080322                    WHEN CL-CLAIM-TYPE = 'H'
080322                       MOVE ' HS '          TO EMI-CLAIM-TYPE
100518                    WHEN CL-CLAIM-TYPE = 'O'
100518                       MOVE '  OT  '
100518                              TO EMI-CLAIM-TYPE
101713                    WHEN OTHER
101713                       MOVE '  AH  '
101713                              TO EMI-CLAIM-TYPE
101713                 END-EVALUATE
101713              END-IF
101713              PERFORM 9900-ERROR-FORMAT
101713                                 THRU 9900-EXIT
101713              MOVE EMI-LINE1 (8:64)
101713                             TO P-NOT-TEXT-1
101713              MOVE SPACES    TO P-NOT-TEXT-2
101713
101713             MOVE DSP           TO WS-PASSED-CNTL-CHAR
101713             MOVE  P-NOT-LINE-1 TO WS-PASSED-DATA
101713             PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT
101713
101713             MOVE SSP           TO WS-PASSED-CNTL-CHAR
101713             MOVE  P-NOT-LINE-2 TO WS-PASSED-DATA
101713             PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT
101713
101713             MOVE SSP           TO WS-PASSED-CNTL-CHAR
101713             MOVE  P-NOT-LINE-3 TO WS-PASSED-DATA
101713             PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT
101713
101713         END-PERFORM
101713         GO TO 2560-EXIT
101713     END-IF.
101713
02956      MOVE DSP           TO WS-PASSED-CNTL-CHAR.                   EL008
02957      MOVE  P-NOT-LINE-1 TO WS-PASSED-DATA.                        EL008
02958      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL008
02959                                                                   EL008
02960      MOVE SSP           TO WS-PASSED-CNTL-CHAR.                   EL008
02961      MOVE  P-NOT-LINE-2 TO WS-PASSED-DATA.                        EL008
02962      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL008
02963                                                                   EL008
02964      MOVE SSP           TO WS-PASSED-CNTL-CHAR.                   EL008
02965      MOVE  P-NOT-LINE-3 TO WS-PASSED-DATA.                        EL008
02966      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL008
02967                                                                   EL008
02968  2560-EXIT.                                                       EL008
02969       EXIT.                                                       EL008
02970                                                                   EL008
02971      EJECT                                                           CL*15
02972  2570-PROCESS-TR7.                                                EL008
02973 ******************************************************************   CL*15
02974 ***          BUILD AUTO PROMPT INFORMATION                     ***   CL*15
02975 ******************************************************************   CL*15
02976                                                                      CL*15
02977      IF  WS-LINE-CNT  GREATER THAN  50                            EL008
02978          PERFORM  4013-HEADING-RTN  THRU 4013-EXIT                EL008
02979          PERFORM  1014-HEADING-CONT THRU 1014-EXIT.               EL008
02980                                                                   EL008
02981      MOVE  AT-PROMPT-LINE-1     TO P-PRO-TEXT-1.                  EL008
02982      MOVE  AT-PROMPT-LINE-2     TO P-PRO-TEXT-2.                  EL008
02983      MOVE  AT-RECORDED-BY       TO P-PRO-BY.                      EL008
02984                                                                   EL008
02985      IF AT-RECORDED-DT = LOW-VALUES OR SPACES                     EL008
02986          MOVE SPACES TO P-PRO-NOTE-DT                             EL008
02987      ELSE                                                         EL008
02988          MOVE  AT-RECORDED-DT    TO DC-BIN-DATE-1                 EL008
02989          MOVE  ' '               TO DC-OPTION-CODE                EL008
02990          PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    EL008
02991          MOVE DC-GREG-DATE-1-EDIT TO  P-PRO-NOTE-DT.              EL008
02992                                                                   EL008
02993      IF AT-PROMPT-START-DT = LOW-VALUES OR SPACES                 EL008
02994          MOVE SPACE               TO P-PRO-START-DT               EL008
02995      ELSE                                                         EL008
02996          MOVE AT-PROMPT-START-DT  TO DC-BIN-DATE-1                EL008
02997          MOVE  ' '                TO DC-OPTION-CODE               EL008
02998          PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    EL008
02999          MOVE DC-GREG-DATE-1-EDIT TO  P-PRO-START-DT.             EL008
03000                                                                   EL008
03001      IF AT-PROMPT-END-DT = LOW-VALUES OR SPACES                   EL008
03002          MOVE SPACE               TO P-PRO-END-DT                 EL008
03003      ELSE                                                         EL008
03004          MOVE AT-PROMPT-END-DT    TO DC-BIN-DATE-1                EL008
03005          MOVE  ' '                TO DC-OPTION-CODE               EL008
03006          PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    EL008
03007          MOVE DC-GREG-DATE-1-EDIT TO  P-PRO-END-DT.               EL008
03008                                                                   EL008
03009      MOVE DSP           TO WS-PASSED-CNTL-CHAR.                   EL008
03010      MOVE  P-PRO-LINE-1 TO WS-PASSED-DATA.                        EL008
03011      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL008
03012                                                                   EL008
03013      MOVE SSP           TO WS-PASSED-CNTL-CHAR.                   EL008
03014      MOVE  P-PRO-LINE-2 TO WS-PASSED-DATA.                        EL008
03015      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL008
03016                                                                   EL008
03017      MOVE SSP           TO WS-PASSED-CNTL-CHAR.                   EL008
03018      MOVE  P-PRO-LINE-3 TO WS-PASSED-DATA.                        EL008
03019      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL008
03020                                                                   EL008
03021  2570-EXIT.                                                       EL008
03022       EXIT.                                                       EL008
03023                                                                   EL008
03024      EJECT                                                           CL*15
03025  2580-PROCESS-TR8.                                                EL008
03026 ******************************************************************   CL*15
03027 ***          BUILD DENIAL INFORMATION                          ***   CL*15
03028 ******************************************************************   CL*15
03029                                                                      CL*15
03030      IF  WS-LINE-CNT  GREATER THAN  50                            EL008
03031          PERFORM  4013-HEADING-RTN  THRU 4013-EXIT                EL008
03032          PERFORM  1014-HEADING-CONT THRU 1014-EXIT.               EL008
03033                                                                   EL008
03034      IF AT-DENIAL-DT = LOW-VALUES OR SPACES                       EL008
03035          MOVE SPACE               TO P-DEN-DEN-DT                 EL008
03036      ELSE                                                         EL008
03037          MOVE AT-DENIAL-DT        TO DC-BIN-DATE-1                EL008
03038          MOVE  ' '                TO DC-OPTION-CODE               EL008
03039          PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    EL008
03040          MOVE DC-GREG-DATE-1-EDIT TO P-DEN-DEN-DT.                EL008
03041                                                                   EL008
03042      MOVE AT-RECORDED-BY          TO P-DEN-BY.                    EL008
03043                                                                   EL008
03044      IF AT-RETRACTION-DT = LOW-VALUES OR SPACES                   EL008
03045          MOVE SPACE                  TO P-DEN-RECON-DT            EL008
03046      ELSE                                                         EL008
03047          MOVE AT-RETRACTION-DT       TO DC-BIN-DATE-1             EL008
03048          MOVE  ' '                    TO DC-OPTION-CODE           EL008
03049          PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    EL008
03050          MOVE DC-GREG-DATE-1-EDIT TO  P-DEN-RECON-DT.             EL008
03051                                                                   EL008
03052      MOVE AT-DENIAL-REASON-CODE      TO P-DEN-CODE.               EL008
03053      MOVE AT-DENIAL-INFO-1           TO P-DEN-TEXT-1.             EL008
03054      MOVE AT-DENIAL-INFO-2           TO P-DEN-TEXT-2.             EL008
03055                                                                   EL008
03056      MOVE DSP           TO WS-PASSED-CNTL-CHAR.                   EL008
03057      MOVE  P-DEN-LINE-1 TO WS-PASSED-DATA.                        EL008
03058      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL008
03059                                                                   EL008
03060      MOVE SSP           TO WS-PASSED-CNTL-CHAR.                   EL008
03061      MOVE  P-DEN-LINE-2 TO WS-PASSED-DATA.                        EL008
03062      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL008
03063                                                                   EL008
03064      MOVE SSP           TO WS-PASSED-CNTL-CHAR.                   EL008
03065      MOVE  P-DEN-LINE-3 TO WS-PASSED-DATA.                        EL008
03066      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL008
03067                                                                   EL008
03068  2580-EXIT.                                                       EL008
03069       EXIT.                                                       EL008
03070                                                                   EL008
03071      EJECT                                                           CL*15
03072  2590-PROCESS-TR9.                                                EL008
03073 ******************************************************************   CL*15
03074 ***          BUILD RESERVE/EXPENSE/HISTORY INFORMATION         ***   CL*15
03075 ******************************************************************   CL*15
03076                                                                      CL*15
03077      IF  WS-LINE-CNT  GREATER THAN  50                            EL008
03078          PERFORM  4013-HEADING-RTN  THRU 4013-EXIT                EL008
03079          PERFORM  1014-HEADING-CONT THRU 1014-EXIT.               EL008
03080                                                                   EL008
03081      MOVE  AT-OLD-INIT-MAN-RESV      TO P-CHG-INIT-RES.           EL008
03082      MOVE  AT-OLD-TOTAL-PAID         TO P-CHG-TOT-PD.             EL008
03083      MOVE  AT-OLD-CURRENT-MAN-RESV   TO P-CHG-CUR-RES.            EL008
03084      MOVE  AT-OLD-ITD-PAID-EXPENSE   TO P-CHG-TOT-EXP.            EL008
03085      MOVE  AT-OLD-DAYS-PAID          TO P-CHG-DAYS-PD.            EL008
03086      MOVE  AT-OLD-ADDL-MAN-RESV      TO P-CHG-ADD-RES.            EL008
03087      MOVE  AT-OLD-CHARGABLE-EXPENSE  TO P-CHG-CHG-EXP.            EL008
03088      MOVE  AT-OLD-NO-OF-PMTS         TO P-CHG-PMTS.               EL008
03089      MOVE  AT-TRAILER-CNT-AT-CHG     TO P-CHG-TOT-TRLRS.          EL008
03090      MOVE  AT-RECORDED-BY            TO P-CHG-BY.                 EL008
03091                                                                   EL008
03092      MOVE  AT-RECORDED-DT            TO DC-BIN-DATE-1             EL008
03093      MOVE  ' '                       TO DC-OPTION-CODE            EL008
03094      PERFORM 8100-DATE-RTN  THRU 8100-EXIT                        EL008
03095      MOVE DC-GREG-DATE-1-EDIT        TO P-CHG-REC-DT.             EL008
03096                                                                   EL008
03097      IF AT-OLD-INCURRED-DT = LOW-VALUES OR SPACES                 EL008
03098          MOVE ZEROS                  TO P-CHG-INC-DT              EL008
03099      ELSE                                                         EL008
03100          MOVE  AT-OLD-INCURRED-DT    TO DC-BIN-DATE-1             EL008
03101          MOVE  ' '                   TO DC-OPTION-CODE            EL008
03102          PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    EL008
03103          MOVE DC-GREG-DATE-1-EDIT    TO P-CHG-INC-DT.             EL008
03104                                                                   EL008
03105      IF AT-OLD-PAID-THRU-DT = LOW-VALUES OR SPACES                EL008
03106         MOVE ZEROS                  TO P-CHG-PAID-TO-DT              CL**5
03107      ELSE                                                         EL008
03108         IF NOT PI-USES-PAID-TO                                       CL**5
03109            MOVE  AT-OLD-PAID-THRU-DT   TO DC-BIN-DATE-1              CL**5
03110            MOVE  ' '                   TO DC-OPTION-CODE             CL**5
03111            PERFORM 8100-DATE-RTN  THRU 8100-EXIT                     CL**5
03112            MOVE DC-GREG-DATE-1-EDIT    TO P-CHG-PAID-TO-DT           CL**5
03113         ELSE                                                         CL**5
03114            MOVE 'PD TO - ' TO L-1-PD-THRU                            CL**5
03115            MOVE  AT-OLD-PAID-THRU-DT   TO DC-BIN-DATE-1              CL**5
03116            MOVE +1                     TO DC-ELAPSED-DAYS            CL**5
03117            MOVE +0                     TO DC-ELAPSED-MONTHS          CL**5
03118            MOVE  '6'                   TO DC-OPTION-CODE             CL**5
03119            PERFORM 8100-DATE-RTN  THRU 8100-EXIT                     CL**5
03120            MOVE DC-GREG-DATE-1-EDIT    TO P-CHG-PAID-TO-DT.          CL**5
03121                                                                   EL008
03122      IF AT-OLD-REPORTED-DT = LOW-VALUES OR SPACES                 EL008
03123          MOVE ZEROS TO P-CHG-REP-DT                               EL008
03124      ELSE                                                         EL008
03125          MOVE AT-OLD-REPORTED-DT     TO DC-BIN-DATE-1             EL008
03126          MOVE  ' '                   TO DC-OPTION-CODE            EL008
03127          PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    EL008
03128          MOVE DC-GREG-DATE-1-EDIT    TO P-CHG-REP-DT.             EL008
03129                                                                   EL008
03130      IF AT-OLD-ESTABLISHED-DT = LOW-VALUES OR SPACES              EL008
03131          MOVE ZEROS                  TO P-CHG-CREAT-DT            EL008
03132      ELSE                                                         EL008
03133          MOVE  AT-OLD-ESTABLISHED-DT TO DC-BIN-DATE-1             EL008
03134          MOVE  ' '                   TO DC-OPTION-CODE            EL008
03135          PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    EL008
03136          MOVE DC-GREG-DATE-1-EDIT    TO P-CHG-CREAT-DT.           EL008
03137                                                                   EL008
03138      IF AT-LAST-PMT-MADE-DT = LOW-VALUES OR SPACES                EL008
03139          MOVE ZEROS TO P-CHG-LAST-PMT-DT                          EL008
03140      ELSE                                                         EL008
03141          MOVE  AT-LAST-PMT-MADE-DT   TO DC-BIN-DATE-1             EL008
03142          MOVE  ' '                   TO DC-OPTION-CODE            EL008
03143          PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    EL008
03144          MOVE DC-GREG-DATE-1-EDIT    TO P-CHG-LAST-PMT-DT.        EL008
03145                                                                   EL008
03146      MOVE DSP           TO WS-PASSED-CNTL-CHAR.                   EL008
03147      MOVE  P-CHG-LINE-1 TO WS-PASSED-DATA.                        EL008
03148      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL008
03149                                                                   EL008
03150      MOVE SSP           TO WS-PASSED-CNTL-CHAR.                   EL008
03151      MOVE  P-CHG-LINE-2 TO WS-PASSED-DATA.                        EL008
03152      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL008
03153                                                                   EL008
03154      MOVE SSP           TO WS-PASSED-CNTL-CHAR.                   EL008
03155      MOVE  P-CHG-LINE-3 TO WS-PASSED-DATA.                        EL008
03156      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL008
03157                                                                   EL008
03158      MOVE SSP           TO WS-PASSED-CNTL-CHAR.                   EL008
03159      MOVE  P-CHG-LINE-4 TO WS-PASSED-DATA.                        EL008
03160      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL008
03161                                                                   EL008
03162  2590-EXIT.                                                       EL008
03163       EXIT.                                                       EL008
03164                                                                   EL008
03165      EJECT                                                           CL*15
03166  2600-PROCESS-TRA.                                                EL008
03167 ******************************************************************   CL*15
03168 ***       BUILD CONTINUING CLAIM FORM INFORMATION              ***   CL*15
03169 ******************************************************************   CL*15
03170                                                                      CL*15
03171      IF WS-LINE-CNT GREATER THAN 50                               EL008
03172          PERFORM  4013-HEADING-RTN  THRU 4013-EXIT                EL008
03173          PERFORM  1014-HEADING-CONT THRU 1014-EXIT.               EL008
03174                                                                   EL008
03175      MOVE AT-RECORDED-DT           TO DC-BIN-DATE-1               EL008
03176      MOVE  ' '                     TO DC-OPTION-CODE              EL008
03177      PERFORM 8100-DATE-RTN  THRU 8100-EXIT                        EL008
03178      MOVE DC-GREG-DATE-1-EDIT      TO P-FORM-LET-DT.              EL008
03179      MOVE AT-RECORDED-BY           TO P-FORM-BY.                  EL008
03180      MOVE AT-FORM-ADDRESS          TO P-FORM-ADSEE-CD.            EL008
03181                                                                   EL008
03182      IF INITIAL-FORM                                              EL008
03183         MOVE 'INIT'                TO P-FORM-FORM                 EL008
03184        ELSE                                                       EL008
03185         MOVE 'PROG'                TO P-FORM-FORM.                EL008
03186                                                                   EL008
03187      IF AT-FORM-SEND-ON-DT = LOW-VALUES OR SPACES                 EL008
03188          MOVE SPACES                  TO P-FORM-SENT-DT           EL008
03189      ELSE                                                         EL008
03190          MOVE AT-FORM-SEND-ON-DT      TO DC-BIN-DATE-1            EL008
03191          MOVE  ' '                    TO DC-OPTION-CODE           EL008
03192          PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    EL008
03193          MOVE DC-GREG-DATE-1-EDIT     TO  P-FORM-SENT-DT.         EL008
03194                                                                   EL008
03195                                                                   EL008
03196      IF AT-FORM-RE-SEND-DT = LOW-VALUES OR SPACES                 EL008
03197          MOVE SPACES                  TO P-FORM-SEND-DT           EL008
03198      ELSE                                                         EL008
03199          MOVE AT-FORM-RE-SEND-DT      TO DC-BIN-DATE-1            EL008
03200          MOVE  ' '                    TO DC-OPTION-CODE           EL008
03201          PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    EL008
03202          MOVE DC-GREG-DATE-1-EDIT     TO  P-FORM-SEND-DT.         EL008
03203                                                                   EL008
03204                                                                   EL008
03205      IF AT-FORM-FOLLOW-UP-DT = LOW-VALUES OR SPACES               EL008
03206          MOVE SPACE                   TO P-FORM-FOL-DT            EL008
03207      ELSE                                                         EL008
03208          MOVE AT-FORM-FOLLOW-UP-DT    TO DC-BIN-DATE-1            EL008
03209          MOVE  ' '                    TO DC-OPTION-CODE           EL008
03210          PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    EL008
03211          MOVE DC-GREG-DATE-1-EDIT     TO  P-FORM-FOL-DT.          EL008
03212                                                                   EL008
03213      IF AT-FORM-ANSWERED-DT = LOW-VALUES OR SPACES                EL008
03214          MOVE SPACE                   TO P-CLM-FORM-ANS-DT           CL**5
03215      ELSE                                                         EL008
03216          MOVE AT-FORM-ANSWERED-DT     TO DC-BIN-DATE-1            EL008
03217          MOVE  ' '                    TO DC-OPTION-CODE           EL008
03218          PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    EL008
03219          MOVE DC-GREG-DATE-1-EDIT     TO P-CLM-FORM-ANS-DT.          CL**5
03220                                                                      CL**5
03221      MOVE SPACES                      TO P-FORM-LINE-4               CL**5
03222                                                                      CL**5
03223      IF AT-EMP-FORM-ANSWERED-DT = LOW-VALUES OR SPACES               CL**5
03224          MOVE SPACES                  TO P-EMP-FORM-ANS-DT           CL**5
03225          MOVE SPACES                  TO P-EMP-FORM-COMM             CL**5
03226      ELSE                                                            CL**5
03227          MOVE '   EMP.  ANSWERED - '  TO P-EMP-FORM-COMM             CL**5
03228          MOVE AT-EMP-FORM-ANSWERED-DT TO DC-BIN-DATE-1               CL**5
03229          MOVE  ' '                    TO DC-OPTION-CODE              CL**5
03230          PERFORM 8100-DATE-RTN  THRU 8100-EXIT                       CL**5
03231          MOVE DC-GREG-DATE-1-EDIT     TO P-EMP-FORM-ANS-DT.          CL**5
03232                                                                      CL**5
03233      IF AT-PHY-FORM-ANSWERED-DT = LOW-VALUES OR SPACES               CL**5
03234          MOVE SPACES                  TO P-PHY-FORM-ANS-DT           CL**5
03235          MOVE SPACES                  TO P-PHY-FORM-COMM             CL**5
03236      ELSE                                                            CL**5
03237          MOVE 'PHY.  ANSWERED - '     TO P-PHY-FORM-COMM             CL**5
03238          MOVE AT-PHY-FORM-ANSWERED-DT TO DC-BIN-DATE-1               CL**5
03239          MOVE  ' '                    TO DC-OPTION-CODE              CL**5
03240          PERFORM 8100-DATE-RTN  THRU 8100-EXIT                       CL**5
03241          MOVE DC-GREG-DATE-1-EDIT     TO P-PHY-FORM-ANS-DT.          CL**5
03242                                                                   EL008
03243      MOVE SPACES                      TO P-FORM-ADSEE.            EL008
03244                                                                   EL008
03245      IF  FORM-TO-INSURED                                          EL008
03246           MOVE 'INSURED'              TO P-FORM-ADSEE.            EL008
03247      IF  FORM-TO-ACCOUNT                                          EL008
03248           MOVE 'ACCOUNT'              TO P-FORM-ADSEE.            EL008
03249      IF  FORM-TO-OTHER-1                                          EL008
03250           MOVE 'OTHER 1'              TO P-FORM-ADSEE.            EL008
03251      IF  FORM-TO-OTHER-2                                          EL008
03252           MOVE 'OTHER 2'              TO P-FORM-ADSEE.            EL008
03253                                                                   EL008
03254      IF PROGRESS-FORM                                             EL008
03255         MOVE AT-INSTRUCT-LN-1    TO P-FORM-INSTRUCT               EL008
03256         MOVE AT-INSTRUCT-LN-2    TO P-FORM-INSTRUCT-1.            EL008
03257                                                                   EL008
03258      MOVE DSP                    TO WS-PASSED-CNTL-CHAR.          EL008
03259      MOVE  P-FORM-LINE-1         TO WS-PASSED-DATA.               EL008
03260      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL008
03261                                                                   EL008
03262      MOVE SSP                    TO WS-PASSED-CNTL-CHAR.          EL008
03263      MOVE  P-FORM-LINE-2         TO WS-PASSED-DATA.               EL008
03264      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL008
03265                                                                   EL008
03266      MOVE SSP                    TO WS-PASSED-CNTL-CHAR.          EL008
03267      MOVE  P-FORM-LINE-3         TO WS-PASSED-DATA.               EL008
03268      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL008
03269                                                                   EL008
03270      IF P-FORM-LINE-4 NOT EQUAL SPACES                               CL**5
03271         MOVE SSP                    TO WS-PASSED-CNTL-CHAR           CL**5
03272         MOVE  P-FORM-LINE-4         TO WS-PASSED-DATA                CL**5
03273         PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        CL**5
03274                                                                      CL**5
03275      IF INITIAL-FORM OR AT-INSTRUCT-LN-1 = SPACES                 EL008
03276         GO TO 2600-CHECK-RELATED-CLAIMS.                          EL008
03277                                                                   EL008
03278      MOVE SSP                    TO WS-PASSED-CNTL-CHAR.          EL008
03279      MOVE  P-FORM-LINE-5         TO WS-PASSED-DATA.               EL008
03280      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL008
03281                                                                   EL008
03282      IF AT-INSTRUCT-LN-2 NOT = SPACES                             EL008
03283         MOVE SSP                 TO WS-PASSED-CNTL-CHAR           EL008
03284         MOVE  P-FORM-LINE-6      TO WS-PASSED-DATA                EL008
03285         PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                     EL008
03286                                                                   EL008
03287      IF AT-INSTRUCT-LN-3 NOT = SPACES                             EL008
03288         MOVE AT-INSTRUCT-LN-3    TO P-FORM-INSTRUCT-1             EL008
03289         MOVE SSP                 TO WS-PASSED-CNTL-CHAR           EL008
03290         MOVE  P-FORM-LINE-6      TO WS-PASSED-DATA                EL008
03291         PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                     EL008
03292                                                                   EL008
03293  2600-CHECK-RELATED-CLAIMS.                                       EL008
03294                                                                      CL**5
03295      IF AT-RELATED-1 NOT = SPACES                                 EL008
03296         MOVE AT-REL-CLAIM-1      TO P-FORM-CLAIM                  EL008
03297         MOVE AT-REL-CARR-1       TO P-FORM-CARRIER                EL008
03298         MOVE AT-REL-CERT-1       TO P-FORM-CERT                   EL008
03299         MOVE SSP                 TO WS-PASSED-CNTL-CHAR           EL008
03300         MOVE P-FORM-LINE-7       TO WS-PASSED-DATA                EL008
03301         PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                     EL008
03302                                                                   EL008
03303      IF AT-RELATED-2 NOT = SPACES                                 EL008
03304         MOVE AT-REL-CLAIM-2      TO P-FORM-CLAIM                  EL008
03305         MOVE AT-REL-CARR-2       TO P-FORM-CARRIER                EL008
03306         MOVE AT-REL-CERT-2       TO P-FORM-CERT                   EL008
03307         MOVE SSP                 TO WS-PASSED-CNTL-CHAR           EL008
03308         MOVE P-FORM-LINE-7       TO WS-PASSED-DATA                EL008
03309         PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                     EL008
03310                                                                   EL008
03311  2600-EXIT.                                                       EL008
03312       EXIT.                                                       EL008
03313                                                                   EL008
03314      EJECT                                                        EL008
03315  2700-PRINT-OPEN-CLOSE-HISTORY.                                   EL008
03316      IF OC-TABLE-LOADED  OR                                       EL008
03317         AD-TABLE-LOADED  OR                                       EL008
03318         AP-TABLE-LOADED                                           EL008
03319          NEXT SENTENCE                                            EL008
03320      ELSE                                                         EL008
03321          GO TO 2700-EXIT.                                         EL008
03322                                                                   EL008
03323      PERFORM 4013-HEADING-RTN   THRU 4013-EXIT.                   EL008
03324      PERFORM 1014-HEADING-CONT  THRU 1014-EXIT.                   EL008
03325                                                                   EL008
03326      MOVE SSP           TO WS-PASSED-CNTL-CHAR.                   EL008
03327      MOVE  P-2-LINE-6   TO WS-PASSED-DATA.                        EL008
03328      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL008
03329                                                                   EL008
03330      MOVE SSP           TO WS-PASSED-CNTL-CHAR.                   EL008
03331      MOVE  P-2-LINE-8   TO WS-PASSED-DATA.                        EL008
03332      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL008
03333                                                                   EL008
03334      SET  AP-INDEX  TO 1.                                         EL008
03335      SET  AD-INDEX  TO 1.                                         EL008
03336      SET  OC-INDEX  TO 1.                                         EL008
03337                                                                   EL008
03338      IF OC-TABLE-LOADED                                           EL008
03339          PERFORM  2710-PROCESS-OC  THRU 2710-EXIT                 EL008
03340           UNTIL  END-OC-TABLE                                     EL008
03341           OR OC-INDEX GREATER THAN 6                              EL008
03342          MOVE SPACES  TO WS-PASSED-DATA                           EL008
03343          MOVE SSP     TO WS-PASSED-CNTL-CHAR                      EL008
03344          PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                    EL008
03345                                                                   EL008
03346      MOVE SPACES TO END-OC-TABLE-SW.                              EL008
03347                                                                   EL008
03348      IF AP-TABLE-LOADED                                           EL008
03349          PERFORM  2720-PROCESS-AP  THRU 2720-EXIT                 EL008
03350           UNTIL  END-AP-TABLE                                     EL008
03351           OR AP-INDEX GREATER THAN 10                             EL008
03352          MOVE SPACES  TO WS-PASSED-DATA                           EL008
03353          MOVE SSP     TO WS-PASSED-CNTL-CHAR                      EL008
03354          PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                    EL008
03355                                                                   EL008
03356      MOVE SPACES TO END-AP-TABLE-SW.                              EL008
03357                                                                   EL008
03358      IF AD-TABLE-LOADED                                           EL008
03359          PERFORM  2730-PROCESS-AD  THRU 2730-EXIT                 EL008
03360           UNTIL  END-AD-TABLE                                     EL008
03361           OR AD-INDEX GREATER THAN  60                               CL**6
03362          MOVE SPACES  TO WS-PASSED-DATA                           EL008
03363          MOVE SSP     TO WS-PASSED-CNTL-CHAR                      EL008
03364          PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                    EL008
03365                                                                   EL008
03366      MOVE SPACES TO END-AD-TABLE-SW.                              EL008
03367                                                                   EL008
03368  2700-EXIT.                                                       EL008
03369       EXIT.                                                       EL008
03370                                                                   EL008
03371  2710-PROCESS-OC.                                                 EL008
03372      IF OC-TBL-OPCL-DT (OC-INDEX)  = SPACES OR LOW-VALUES         EL008
03373          MOVE   'E'  TO END-OC-TABLE-SW                           EL008
03374          GO TO 2710-EXIT.                                         EL008
03375                                                                   EL008
03376      MOVE SPACES    TO P-2-HIS-OPCL.                              EL008
03377                                                                   EL008
03378      IF  OC-TBL-OPCL-TYPE (OC-INDEX) = 'O'                        EL008
03379          MOVE 'OPEN '   TO P-2-HIS-OPCL.                          EL008
03380      IF  OC-TBL-OPCL-TYPE (OC-INDEX) = 'C'                        EL008
03381          MOVE 'CLOSE'   TO P-2-HIS-OPCL.                          EL008
03382                                                                   EL008
03383      MOVE OC-TBL-OPCL-REASON (OC-INDEX)   TO P-2-HIS-CAUSE.       EL008
03384                                                                   EL008
03385      MOVE OC-TBL-OPCL-DT (OC-INDEX) TO DC-BIN-DATE-1.             EL008
03386      MOVE  ' '                      TO DC-OPTION-CODE.            EL008
03387      PERFORM 8100-DATE-RTN  THRU 8100-EXIT.                       EL008
03388      MOVE DC-GREG-DATE-1-EDIT TO  P-2-HIS-DATE.                   EL008
03389                                                                   EL008
03390      MOVE SSP              TO WS-PASSED-CNTL-CHAR.                EL008
03391      MOVE P-2-HIS-DETAIL   TO WS-PASSED-DATA.                     EL008
03392      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL008
03393                                                                   EL008
03394      MOVE SPACES  TO  OC-TBL-OPCL-TYPE   (OC-INDEX)               EL008
03395                       OC-TBL-OPCL-DT     (OC-INDEX)               EL008
03396                       OC-TBL-OPCL-REASON (OC-INDEX).              EL008
03397                                                                   EL008
03398      SET  OC-INDEX   UP BY 1.                                     EL008
03399                                                                   EL008
03400  2710-EXIT.                                                       EL008
03401       EXIT.                                                       EL008
03402                                                                   EL008
03403      EJECT                                                           CL*15
03404  2720-PROCESS-AP.                                                 EL008
03405      IF AP-TBL-SCHED-START-DT (AP-INDEX) = SPACES                 EL008
03406          MOVE   'E'  TO END-AP-TABLE-SW                           EL008
03407          GO TO 2720-EXIT.                                         EL008
03408                                                                   EL008
03409      IF  WS-LINE-CNT  GREATER THAN  50                            EL008
03410          PERFORM  4013-HEADING-RTN  THRU 4013-EXIT.               EL008
03411                                                                   EL008
03412      MOVE AP-TBL-EST-BY     (AP-INDEX)    TO P-2-EST-BY.          EL008
03413      MOVE AP-TBL-FIRST-DAYS (AP-INDEX)    TO P-2-DAYS-1ST.        EL008
03414      MOVE AP-TBL-FIRST-AMT  (AP-INDEX)    TO P-2-1ST-PMT.         EL008
03415      MOVE AP-TBL-REG-AMT    (AP-INDEX)    TO P-2-REG-PMT.         EL008
03416      MOVE AP-TBL-FIRST-DAYS (AP-INDEX)    TO P-2-DAYS-1ST.        EL008
03417                                                                   EL008
03418      MOVE AP-TBL-EST-DT (AP-INDEX)        TO DC-BIN-DATE-1.       EL008
03419      MOVE  ' '                            TO DC-OPTION-CODE.      EL008
03420      PERFORM 8100-DATE-RTN  THRU 8100-EXIT.                       EL008
03421      MOVE DC-GREG-DATE-1-EDIT             TO P-2-EST-DT.          EL008
03422                                                                   EL008
03423      IF AP-TBL-SCHED-START-DT (AP-INDEX)  = LOW-VALUES OR         EL008
03424                                             SPACES                EL008
03425          MOVE ZEROS TO P-2-EFF-DT                                 EL008
03426      ELSE                                                         EL008
03427          MOVE AP-TBL-SCHED-START-DT (AP-INDEX) TO  DC-BIN-DATE-1     CL*11
03428          MOVE  ' '                           TO  DC-OPTION-CODE      CL*11
03429          MOVE +0                             TO  DC-ELAPSED-DAYS     CL*11
03430                                                  DC-ELAPSED-MONTHS   CL*11
03431          PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    EL008
03432          IF NO-CONVERSION-ERROR                                      CL*11
03433              MOVE DC-GREG-DATE-1-EDIT        TO  P-2-EFF-DT          CL*11
03434          ELSE                                                        CL*11
03435              MOVE SPACES                     TO  P-2-EFF-DT.         CL*11
03436                                                                   EL008
03437      IF AP-TBL-SCHED-END-DT (AP-INDEX) = LOW-VALUES OR SPACES     EL008
03438          MOVE ZEROS                          TO  P-2-END-DT          CL*11
03439          IF PI-USES-PAID-TO                                          CL*11
03440              MOVE 'LAST PAY TO DATE    - '   TO  P-2-LST-PMT-DT      CL*11
03441          ELSE                                                        CL*11
03442              MOVE 'LAST PAY THRU DATE  - '   TO  P-2-LST-PMT-DT      CL*11
03443      ELSE                                                         EL008
03444          IF PI-USES-PAID-TO                                          CL*11
03445              MOVE 'LAST PAY TO DATE    - '   TO  P-2-LST-PMT-DT      CL*11
03446              MOVE AP-TBL-SCHED-END-DT (AP-INDEX) TO DC-BIN-DATE-1    CL*11
03447              MOVE '6'                        TO  DC-OPTION-CODE      CL*11
03448              MOVE +1                         TO  DC-ELAPSED-DAYS     CL*11
03449              MOVE +0                         TO  DC-ELAPSED-MONTHS   CL*11
03450              PERFORM 8100-DATE-RTN THRU 8100-EXIT                    CL*11
03451              IF NO-CONVERSION-ERROR                                  CL*11
03452                  MOVE DC-GREG-DATE-1-EDIT    TO  P-2-LST-PMT-ON      CL*11
03453              ELSE                                                    CL*11
03454                  MOVE SPACES                 TO  P-2-LST-PMT-ON      CL*11
03455          ELSE                                                        CL*11
03456              MOVE 'LAST PAY THRU DATE  - '   TO  P-2-LST-PMT-DT      CL*11
03457              MOVE AP-TBL-SCHED-END-DT (AP-INDEX) TO DC-BIN-DATE-1    CL*11
03458              MOVE  ' '                       TO  DC-OPTION-CODE      CL*11
03459              MOVE +0                         TO  DC-ELAPSED-DAYS     CL*11
03460                                                  DC-ELAPSED-MONTHS   CL*11
03461              PERFORM 8100-DATE-RTN  THRU 8100-EXIT                   CL*11
03462              IF NO-CONVERSION-ERROR                                  CL*11
03463                  MOVE DC-GREG-DATE-1-EDIT    TO  P-2-LST-PMT-ON      CL*11
03464              ELSE                                                    CL*11
03465                  MOVE SPACES                 TO  P-2-LST-PMT-ON.     CL*11
03466                                                                   EL008
03467      IF AP-TBL-FIRST-DT (AP-INDEX) = LOW-VALUES OR SPACES         EL008
03468          MOVE SPACES                         TO  P-2-1ST-PMT-ON      CL*11
03469          IF PI-USES-PAID-TO                                          CL*11
03470              MOVE 'FIRST PAY TO DATE   - '   TO  P-2-1ST-PMT-DT      CL*11
03471          ELSE                                                        CL*11
03472              MOVE 'FIRST PAY THRU DATE - '   TO  P-2-1ST-PMT-DT      CL*11
03473      ELSE                                                         EL008
03474          IF PI-USES-PAID-TO                                          CL*11
03475              MOVE 'FIRST PAY TO DATE   - '   TO  P-2-1ST-PMT-DT      CL*11
03476              MOVE AP-TBL-FIRST-DT (AP-INDEX) TO  DC-BIN-DATE-1       CL*11
03477              MOVE '6'                        TO  DC-OPTION-CODE      CL*11
03478              MOVE +1                         TO  DC-ELAPSED-DAYS     CL*11
03479              MOVE +0                         TO  DC-ELAPSED-MONTHS   CL*11
03480              PERFORM 8100-DATE-RTN THRU 8100-EXIT                    CL*11
03481              IF NO-CONVERSION-ERROR                                  CL*11
03482                  MOVE DC-GREG-DATE-1-EDIT    TO  P-2-1ST-PMT-ON      CL*11
03483              ELSE                                                    CL*11
03484                  MOVE SPACES                 TO  P-2-1ST-PMT-ON      CL*11
03485          ELSE                                                        CL*11
03486              MOVE 'FIRST PAY THRU DATE - '   TO  P-2-1ST-PMT-DT      CL*11
03487              MOVE AP-TBL-FIRST-DT (AP-INDEX) TO  DC-BIN-DATE-1       CL*11
03488              MOVE  ' '                       TO  DC-OPTION-CODE      CL*11
03489              MOVE +0                         TO  DC-ELAPSED-DAYS     CL*11
03490                                                  DC-ELAPSED-MONTHS   CL*11
03491              PERFORM 8100-DATE-RTN  THRU 8100-EXIT                   CL*11
03492              IF NO-CONVERSION-ERROR                                  CL*11
03493                  MOVE DC-GREG-DATE-1-EDIT    TO  P-2-1ST-PMT-ON      CL*11
03494              ELSE                                                    CL*11
03495                  MOVE SPACES                 TO  P-2-1ST-PMT-ON.     CL*11
03496                                                                   EL008
03497      IF AP-TBL-TERM-DT (AP-INDEX) = LOW-VALUES OR SPACES          EL008
03498          MOVE SPACES                    TO P-2-END-DT             EL008
03499      ELSE                                                         EL008
03500          MOVE AP-TBL-TERM-DT (AP-INDEX) TO DC-BIN-DATE-1          EL008
03501          MOVE +0                        TO DC-ELAPSED-DAYS           CL*11
03502                                            DC-ELAPSED-MONTHS         CL*11
03503          MOVE  ' '                      TO DC-OPTION-CODE         EL008
03504          PERFORM 8100-DATE-RTN  THRU 8100-EXIT                    EL008
03505          MOVE DC-GREG-DATE-1-EDIT       TO P-2-END-DT.            EL008
03506                                                                   EL008
03507      MOVE AP-TBL-INT-MO  (AP-INDEX)    TO P-2-MOS-BET.            EL008
03508                                                                   EL008
03509      MOVE  SPACES   TO  P-2-PAYEE.                                EL008
03510                                                                   EL008
03511      IF  INSURED-PAID-AUTO                                        EL008
03512          MOVE  'INSURED PAID AUTO'  TO  P-2-PAYEE.                EL008
03513      IF  BENEFICIARY-PAID-AUTO                                    EL008
03514          MOVE  'BENEF-Y PAID AUTO'  TO  P-2-PAYEE.                EL008
03515      IF  ACCOUNT-PAID-AUTO                                        EL008
03516          MOVE  'ACCOUNT PAID AUTO'  TO  P-2-PAYEE.                EL008
03517      IF  OTHER-1-PAID-AUTO                                        EL008
03518          MOVE  'OTHER-1 PAID AUTO'  TO  P-2-PAYEE.                EL008
03519      IF  OTHER-2-PAID-AUTO                                        EL008
03520          MOVE  'OTHER-2 PAID AUTO'  TO  P-2-PAYEE.                EL008
03521                                                                   EL008
03522      IF  AP-TBL-LAST-TYPE (AP-INDEX)  = '1'                       EL008
03523          MOVE  'YES'   TO P-2-LAST-FINAL                          EL008
03524      ELSE                                                         EL008
03525          MOVE  'NO '   TO P-2-LAST-FINAL                          EL008
03526                                                                   EL008
03527      MOVE DSP             TO WS-PASSED-CNTL-CHAR.                 EL008
03528      MOVE P-2-AUT-LINE-1  TO WS-PASSED-DATA.                      EL008
03529      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL008
03530                                                                   EL008
03531      MOVE SSP             TO WS-PASSED-CNTL-CHAR.                 EL008
03532      MOVE P-2-AUT-LINE-2  TO WS-PASSED-DATA.                      EL008
03533      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL008
03534                                                                   EL008
03535      MOVE SSP             TO WS-PASSED-CNTL-CHAR.                 EL008
03536      MOVE P-2-AUT-LINE-3  TO WS-PASSED-DATA.                      EL008
03537      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL008
03538                                                                   EL008
03539      MOVE SSP             TO WS-PASSED-CNTL-CHAR.                 EL008
03540      MOVE P-2-AUT-LINE-4  TO WS-PASSED-DATA.                      EL008
03541      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL008
03542                                                                   EL008
03543      MOVE SSP             TO WS-PASSED-CNTL-CHAR.                 EL008
03544      MOVE P-2-AUT-LINE-5  TO WS-PASSED-DATA.                      EL008
03545      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL008
03546                                                                   EL008
03547      MOVE SSP             TO WS-PASSED-CNTL-CHAR.                 EL008
03548      MOVE P-2-AUT-LINE-6  TO WS-PASSED-DATA.                      EL008
03549      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL008
03550                                                                   EL008
03551      MOVE SSP             TO WS-PASSED-CNTL-CHAR.                 EL008
03552      MOVE P-2-AUT-LINE-7  TO WS-PASSED-DATA.                      EL008
03553      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL008
03554                                                                   EL008
03555      MOVE SSP             TO WS-PASSED-CNTL-CHAR.                 EL008
03556      MOVE P-2-AUT-LINE-8  TO WS-PASSED-DATA.                      EL008
03557      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL008
03558                                                                   EL008
03559      MOVE SPACES TO AP-TBL-EST-DT         (AP-INDEX)              EL008
03560                     AP-TBL-EST-BY         (AP-INDEX)              EL008
03561                     AP-TBL-SCHED-START-DT (AP-INDEX)              EL008
03562                     AP-TBL-SCHED-END-DT   (AP-INDEX)              EL008
03563                     AP-TBL-TERM-DT        (AP-INDEX)              EL008
03564                     AP-TBL-LAST-TYPE      (AP-INDEX)              EL008
03565                     AP-TBL-FIRST-DT       (AP-INDEX)              EL008
03566                     AP-TBL-INT-MO         (AP-INDEX).             EL008
03567                                                                   EL008
03568       MOVE ZEROS  TO AP-TBL-FIRST-AMT     (AP-INDEX)              EL008
03569                      AP-TBL-FIRST-DAYS    (AP-INDEX)              EL008
03570                      AP-TBL-REG-AMT       (AP-INDEX)              EL008
03571                      AP-TBL-REG-MO        (AP-INDEX).             EL008
03572                                                                   EL008
03573      SET  AP-INDEX   UP BY 1.                                     EL008
03574                                                                   EL008
03575  2720-EXIT.                                                       EL008
03576       EXIT.                                                       EL008
03577                                                                   EL008
03578      EJECT                                                           CL*15
03579  2730-PROCESS-AD.                                                 EL008
03580      IF AD-TBL-TYPE (AD-INDEX) = SPACES                           EL008
03581          MOVE   'E'  TO END-AD-TABLE-SW                           EL008
03582          GO TO 2730-EXIT.                                         EL008
03583                                                                   EL008
03584      IF  WS-LINE-CNT  GREATER THAN  50                            EL008
03585          PERFORM  4013-HEADING-RTN  THRU 4013-EXIT.               EL008
03586                                                                   EL008
03587      MOVE AD-TBL-TYPE   (AD-INDEX)     TO P-2-ADD-CODE.           EL008
03588      MOVE AD-TBL-NAME   (AD-INDEX)     TO P-2-ADD-NAME.           EL008
03589      MOVE AD-TBL-ADDR-1 (AD-INDEX)     TO P-2-ADD-ADDR-1.         EL008
03590      MOVE AD-TBL-ADDR-2 (AD-INDEX)     TO P-2-ADD-ADDR-2.         EL008
03591      MOVE AD-TBL-CITY   (AD-INDEX)     TO P-2-ADD-CITY.           EL008
03592      MOVE AD-TBL-ZIP    (AD-INDEX)     TO P-2-ADD-ZIP.            EL008
03593      MOVE AD-TBL-PHONE  (AD-INDEX)     TO WS-PHONE-BRKDN.         EL008
03594      MOVE WS-PH-1                      TO WS-PH-ED-1.             EL008
03595      MOVE WS-PH-2                      TO WS-PH-ED-2.             EL008
03596      MOVE WS-PH-3                      TO WS-PH-ED-3.             EL008
03597      MOVE WS-PHONE-EDIT                TO P-2-ADD-PHONE.          EL008
03598                                                                   EL008
03599      IF AD-TBL-TYPE  (AD-INDEX)   = 'I'                              CL**6
03600          MOVE 'INSURED'        TO P-2-ADD-TYPE.                   EL008
03601      IF AD-TBL-TYPE  (AD-INDEX)   = 'B'                              CL**6
03602          MOVE 'BENEFICIARY'    TO P-2-ADD-TYPE.                   EL008
03603      IF AD-TBL-TYPE  (AD-INDEX)   = 'A'                              CL**6
03604          MOVE 'ACCOUNT'        TO P-2-ADD-TYPE.                   EL008
03605      IF AD-TBL-TYPE  (AD-INDEX)   = 'P'                              CL**6
03606          MOVE 'PHYSICIAN'      TO P-2-ADD-TYPE.                   EL008
03607      IF AD-TBL-TYPE  (AD-INDEX)   = 'E'                              CL**6
03608          MOVE 'EMPLOYER'       TO P-2-ADD-TYPE.                   EL008
03609      IF AD-TBL-TYPE  (AD-INDEX)   = 'O'                              CL**6
03610          MOVE 'OTHER 1'        TO P-2-ADD-TYPE.                   EL008
03611      IF AD-TBL-TYPE  (AD-INDEX)   = 'Q'                              CL**6
03612          MOVE 'OTHER 2'        TO P-2-ADD-TYPE.                   EL008
03613      IF AD-TBL-TYPE  (AD-INDEX)   = '8'                           EL008
03614          IF CL-SYSTEM-IDENTIFIER IS EQUAL TO 'CV'                    CL*15
03615              MOVE 'FROM PROD MSTR'   TO P-2-ADD-TYPE                 CL*15
03616          ELSE                                                        CL*15
03617              MOVE 'FROM ACCT MSTR'   TO P-2-ADD-TYPE.                CL*15
03618      IF AD-TBL-TYPE  (AD-INDEX)   = '9'                           EL008
03619          MOVE 'FROM BENE MSTR'    TO P-2-ADD-TYPE.                EL008
03620                                                                   EL008
03621      MOVE DSP             TO WS-PASSED-CNTL-CHAR.                 EL008
03622      MOVE P-2-ADD-LINE-1  TO WS-PASSED-DATA.                      EL008
03623      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL008
03624                                                                   EL008
03625      MOVE SSP             TO WS-PASSED-CNTL-CHAR.                 EL008
03626      MOVE P-2-ADD-LINE-2  TO WS-PASSED-DATA.                      EL008
03627      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL008
03628                                                                   EL008
03629      MOVE SSP             TO WS-PASSED-CNTL-CHAR.                 EL008
03630      MOVE P-2-ADD-LINE-3  TO WS-PASSED-DATA.                      EL008
03631      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL008
03632                                                                   EL008
03633      MOVE SSP             TO WS-PASSED-CNTL-CHAR.                 EL008
03634      MOVE P-2-ADD-LINE-4  TO WS-PASSED-DATA.                      EL008
03635      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL008
03636                                                                   EL008
03637      MOVE SSP             TO WS-PASSED-CNTL-CHAR.                 EL008
03638      MOVE P-2-ADD-LINE-5  TO WS-PASSED-DATA.                      EL008
03639      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL008
03640                                                                   EL008
03641      MOVE SSP             TO WS-PASSED-CNTL-CHAR.                 EL008
03642      MOVE P-2-ADD-LINE-6  TO WS-PASSED-DATA.                      EL008
03643      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL008
03644                                                                   EL008
03645      MOVE  SPACES  TO AD-TBL-TYPE   (AD-INDEX)                    EL008
03646                       AD-TBL-NAME   (AD-INDEX)                    EL008
03647                       AD-TBL-ADDR-1 (AD-INDEX)                    EL008
03648                       AD-TBL-ADDR-2 (AD-INDEX)                    EL008
03649                       AD-TBL-CITY   (AD-INDEX).                   EL008
03650       MOVE ZEROS TO   AD-TBL-ZIP    (AD-INDEX)                    EL008
03651                       AD-TBL-PHONE  (AD-INDEX).                   EL008
03652                                                                   EL008
03653      SET  AD-INDEX   UP BY 1.                                     EL008
03654                                                                   EL008
03655  2730-EXIT.                                                       EL008
03656       EXIT.                                                       EL008
03657                                                                   EL008
03658      EJECT                                                           CL*15
03659  4000-INITIALIZE.                                                 EL008
03660      MOVE SPACES TO   END-OC-TABLE-SW                             EL008
03661                       END-AD-TABLE-SW                             EL008
03662                       END-AP-TABLE-SW.                            EL008
03663                                                                   EL008
03664      MOVE SAVE-DATE         TO HEAD-RUN-DATE.                     EL008
03665                                                                   EL008
03666      MOVE 0  TO WS-PAGE-CNT.                                      EL008
03667                                                                   EL008
03668      MOVE +80  TO WS-LINE-LEN.                                    EL008
03669                                                                   EL008
03670      SET  AP-INDEX   TO 1.                                        EL008
03671      SET  AD-INDEX   TO 1.                                        EL008
03672      SET  OC-INDEX   TO 1.                                        EL008
03673                                                                   EL008
03674      PERFORM 4001-CLEAR-OC-TABLE  6   TIMES.                      EL008
03675      PERFORM 4002-CLEAR-AP-TABLE  10  TIMES.                      EL008
03676      PERFORM 4003-CLEAR-AD-TABLE  60  TIMES.                         CL**6
03677                                                                   EL008
03678      MOVE SAVE-DATE     TO DC-GREG-DATE-1-EDIT                    EL008
03679                            HEAD-RUN-DATE.                         EL008
03680                                                                   EL008
03681      MOVE '2' TO DC-OPTION-CODE.                                  EL008
03682      PERFORM 8100-DATE-RTN  THRU 8100-EXIT.                       EL008
03683      MOVE DC-GREG-DATE-1-ALPHA  TO WS-UNALIGNED-FIELD.            EL008
03684      MOVE DC-BIN-DATE-1         TO WS-BIN-CURRENT-DT              EL008
03685      MOVE +18 TO WS-NAME-LENGTH.                                  EL008
03686      PERFORM ELCALGNP              THRU ELCALGNP-EXIT.            EL008
03687      MOVE WS-ALIGNED-FIELD      TO HEAD-RUN-DATE-FULL.            EL008
03688                                                                   EL008
03689  4000-EXIT.                                                       EL008
03690       EXIT.                                                       EL008
03691                                                                   EL008
03692  4001-CLEAR-OC-TABLE.                                             EL008
03693      MOVE SPACES  TO  OC-TBL-OPCL-DT     (OC-INDEX)               EL008
03694                       OC-TBL-OPCL-TYPE   (OC-INDEX)               EL008
03695                       OC-TBL-OPCL-REASON (OC-INDEX).              EL008
03696      SET  OC-INDEX   UP BY 1.                                     EL008
03697                                                                   EL008
03698  4001-EXIT.                                                       EL008
03699       EXIT.                                                       EL008
03700                                                                   EL008
03701  4002-CLEAR-AP-TABLE.                                             EL008
03702      MOVE SPACES TO AP-TBL-EST-DT         (AP-INDEX)              EL008
03703                     AP-TBL-EST-BY         (AP-INDEX)              EL008
03704                     AP-TBL-SCHED-START-DT (AP-INDEX)              EL008
03705                     AP-TBL-SCHED-END-DT   (AP-INDEX)              EL008
03706                     AP-TBL-TERM-DT        (AP-INDEX)              EL008
03707                     AP-TBL-LAST-TYPE      (AP-INDEX)              EL008
03708                     AP-TBL-FIRST-DT       (AP-INDEX)              EL008
03709                     AP-TBL-INT-MO         (AP-INDEX).             EL008
03710                                                                   EL008
03711       MOVE ZEROS  TO AP-TBL-FIRST-AMT     (AP-INDEX)              EL008
03712                      AP-TBL-FIRST-DAYS    (AP-INDEX)              EL008
03713                      AP-TBL-REG-AMT       (AP-INDEX)              EL008
03714                      AP-TBL-REG-MO        (AP-INDEX).             EL008
03715                                                                   EL008
03716      SET  AP-INDEX   UP BY 1.                                     EL008
03717                                                                   EL008
03718  4002-EXIT.                                                       EL008
03719       EXIT.                                                       EL008
03720                                                                   EL008
03721  4003-CLEAR-AD-TABLE.                                             EL008
03722      MOVE  SPACES  TO AD-TBL-TYPE   (AD-INDEX)                    EL008
03723                       AD-TBL-NAME   (AD-INDEX)                    EL008
03724                       AD-TBL-ADDR-1 (AD-INDEX)                    EL008
03725                       AD-TBL-ADDR-2 (AD-INDEX)                    EL008
03726                       AD-TBL-CITY   (AD-INDEX).                   EL008
03727       MOVE ZEROS TO   AD-TBL-ZIP    (AD-INDEX)                    EL008
03728                       AD-TBL-PHONE  (AD-INDEX).                   EL008
03729                                                                   EL008
03730      SET  AD-INDEX   UP BY 1.                                     EL008
03731                                                                   EL008
03732  4003-EXIT.                                                       EL008
03733       EXIT.                                                       EL008
03734                                                                   EL008
03735  4013-HEADING-RTN.                                                EL008
03736      ADD  1  TO  WS-PAGE-CNT.                                     EL008
03737      MOVE WS-PAGE-CNT   TO HEAD-PAGE-NO.                          EL008
03738      MOVE TPG           TO WS-PASSED-CNTL-CHAR.                   EL008
03739      MOVE  HEAD-LINE-2  TO WS-PASSED-DATA.                        EL008
03740      PERFORM ELPRTCVP   THRU ELPRTCVP-EXIT.                       EL008
03741                                                                   EL008
03742      MOVE SSP           TO WS-PASSED-CNTL-CHAR.                   EL008
03743      MOVE  HEAD-LINE-3  TO WS-PASSED-DATA.                        EL008
03744      PERFORM ELPRTCVP   THRU ELPRTCVP-EXIT.                       EL008
03745                                                                   EL008
03746      MOVE SSP           TO WS-PASSED-CNTL-CHAR.                   EL008
03747      MOVE  HEAD-LINE-4  TO WS-PASSED-DATA.                        EL008
03748      PERFORM ELPRTCVP   THRU ELPRTCVP-EXIT.                       EL008
03749                                                                   EL008
03750  4013-EXIT.                                                       EL008
03751       EXIT.                                                       EL008
03752                                                                   EL008
03753  1014-HEADING-CONT.                                               EL008
03754      MOVE DSP           TO WS-PASSED-CNTL-CHAR.                   EL008
03755      MOVE  LINE-25      TO WS-PASSED-DATA.                        EL008
03756      PERFORM ELPRTCVP   THRU ELPRTCVP-EXIT.                       EL008
03757                                                                   EL008
03758      MOVE SSP           TO WS-PASSED-CNTL-CHAR.                   EL008
03759      MOVE  LINE-26      TO WS-PASSED-DATA.                        EL008
03760      PERFORM ELPRTCVP   THRU ELPRTCVP-EXIT.                       EL008
03761                                                                   EL008
03762  1014-EXIT.                                                       EL008
03763       EXIT.                                                       EL008
03764                                                                   EL008
03765       EJECT                                                          CL*15
uktdel*PRINT-RTN.  COPY ELPRTCVP.                                       EL008
uktins PRINT-RTN.
uktins     COPY ELPRTCVP.
03767                                                                   EL008
03768      EJECT                                                           CL*15
03769  8100-DATE-RTN.                                                   EL008
03770      EXEC CICS LINK                                               EL008
03771             PROGRAM  ('ELDATCV')                                  EL008
03772             COMMAREA (DATE-CONVERSION-DATA)                       EL008
03773             LENGTH   (DC-COMM-LENGTH)                             EL008
03774      END-EXEC.                                                    EL008
03775                                                                   EL008
03776  8100-EXIT.                                                       EL008
03777       EXIT.                                                       EL008
03778                                                                   EL008
03779  8300-ABEND.                                                      EL008
03780      MOVE SPACES                 TO WS-PASSED-DATA.               EL008
03781      MOVE DFHEIBLK               TO WS-PASSED-DATA.               EL008
03782      EXEC CICS LINK                                               EL008
03783          PROGRAM   ('EL004')                                      EL008
03784          COMMAREA  (WS-PASSED-DATA)                               EL008
03785          LENGTH    (72)                                           EL008
03786      END-EXEC.                                                    EL008
03787                                                                   EL008
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'                               CL*18
03789          IF DL34-RETURN-CODE NOT = 'OK'                              CL*18
03790              MOVE  '**DLO034 OPEN ERROR - ABORT**'                   CL*18
03791                                  TO WS-PASSED-DATA.                  CL*18
03792                                                                      CL*18
03793      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL008
03794                                                                      CL*18
03795      GO TO 9999-FINALIZE.                                         EL008
03796                                                                   EL008
03797  8900-PGMIDERR.                                                   EL008
03798      MOVE '* PROG NOT FOUND, NOTIFY DATA PROCESSING *'            EL008
03799                           TO WS-PASSED-DATA.                      EL008
03800      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL008
03801      GO TO 9999-FINALIZE.                                         EL008
03802                                                                   EL008
03803  8900-EXIT.                                                       EL008
03804      EJECT                                                        EL008
03805                                                                   EL008
03806  8999-RETURN-CICS.                                                EL008
03807      MOVE WS-SAVED-PI-COMPANY-ID TO PI-COMPANY-ID.                   CL*18
03808      IF PI-CALLING-PROGRAM = 'EL180'                              EL008
03809         MOVE 'EX50'              TO WS-NEXT-TRAN                  EL008
03810        ELSE                                                       EL008
03811         MOVE 'EX23'              TO WS-NEXT-TRAN.                 EL008
03812                                                                   EL008
03813      MOVE EIBTRMID               TO WS-TERMINAL-ID                EL008
03814      IF WS-TERM-PREFIX = 'DU'                                     EL008
03815         EXEC CICS RETURN                                          EL008
03816              TRANSID  (WS-NEXT-TRAN)                              EL008
03817              COMMAREA (PROGRAM-INTERFACE-BLOCK)                   EL008
03818              LENGTH   (PI-COMM-LENGTH)                            EL008
03819         END-EXEC                                                  EL008
03820        ELSE                                                       EL008
03821         EXEC CICS  RETURN                                         EL008
03822         END-EXEC.                                                 EL008
03823                                                                   EL008
03824  8999-EXIT.                                                       EL008
03825       EXIT.                                                       EL008
03826                                                                   EL008
03827  9800-LINK-REM-TERM.                                              EL008
03828      EXEC CICS LINK                                               EL008
03829          PROGRAM    ('ELRTRM')                                    EL008
03830          COMMAREA   (CALCULATION-PASS-AREA)                       EL008
03831          LENGTH     (CP-COMM-LENGTH)                              EL008
03832          END-EXEC.                                                EL008
03833                                                                   EL008
03834  9800-EXIT.                                                       EL008
03835       EXIT.                                                       EL008
03836                                                                   EL008
101713 9900-ERROR-FORMAT.
101713      IF NOT EMI-ERRORS-COMPLETE
101713          EXEC CICS LINK
101713              PROGRAM    ('EL001')
101713              COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
101713              LENGTH     (EMI-COMM-LENGTH)
101713          END-EXEC.
101713
101713 9900-EXIT.
101713      EXIT.
101713
03837  9999-FINALIZE.                                                   EL008
03838                                                                      CL*18
03839      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL008
CIDMOD     MOVE 'X'  TO WS-PROG-END.                                       EL008
CIDMOD     IF PI-COMPANY-ID = 'DMD' OR 'XXX'
CIDMOD        CONTINUE
CIDMOD     ELSE
CIDMOD        PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT                         EL008
CIDMOD     END-IF
03842                                                                      CL*18
03843 * DLO034 CLOSE                                                       CL*18
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'                               CL*18
03845          MOVE 'C'                TO DL34-PROCESS-TYPE                CL*18
03846          MOVE PI-COMPANY-ID      TO DL34-COMPANY-ID                  CL*18
03847          MOVE THIS-PGM           TO DL34-PRINT-PROGRAM-ID            CL*18
03848          MOVE PI-PROCESSOR-ID    TO DL34-USERID                      CL*18
03849          MOVE SPACES             TO DL34-PRINT-LINE                  CL*18
03850                                     DL34-OVERRIDE-PRINTER-ID         CL*18
03851          EXEC CICS LINK                                              CL*18
03852              PROGRAM    ('DLO034')                                   CL*18
03853              COMMAREA   (DLO034-COMMUNICATION-AREA)                  CL*18
03854              LENGTH     (DLO034-REC-LENGTH)                          CL*18
03855          END-EXEC                                                    CL*18
03856                                                                      CL*18
03857          IF DL34-RETURN-CODE NOT = 'OK'                              CL*18
03858              MOVE  '**DLO034 CLOSE ERROR - ABORT**'                  CL*18
03859                                      TO WS-PASSED-DATA               CL*18
03860              GO TO 9999-FINALIZE.                                    CL*18
03861                                                                      CL*18
03862      GO TO 8999-RETURN-CICS.                                      EL008
03863                                                                   EL008
03864  9999-EXIT.                                                       EL008
03865      GOBACK.                                                      EL008
