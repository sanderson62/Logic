00001  IDENTIFICATION DIVISION.                                         03/12/98
00002                                                                   EL511
00003  PROGRAM-ID.                 EL511 .                                 LV011
00004 *              PROGRAM CONVERTED BY                                  CL**9
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**9
00006 *              CONVERSION DATE 02/20/96 15:26:43.                    CL**9
00007 *                            VMOD=2.010.                             CL*10
00008 *                                                                 EL511
00009 *AUTHOR.     LOGIC INC.                                              CL**9
00010 *            DALLAS, TEXAS.                                          CL**9
00011 *DATE-COMPILED.                                                      CL**9
00012                                                                   EL511
00013 *SECURITY.   *****************************************************   CL**9
00014 *            *                                                   *   CL**9
00015 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**9
00016 *            *                                                   *   CL**9
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**9
00018 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**9
00019 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**9
00020 *            *                                                   *   CL**9
00021 *            *****************************************************   CL**9
00022                                                                   EL511
00023 *REMARKS.                                                            CL**9
00024 *       GENERAL FUNCTION IS TO CONVERT THE OLD SCS CARD INPUT        CL**9
00025 *       FORMAT ( 'I' TYPE ISSUES, 'R' TYPE REMITTANCES, 'A+6' TYPE   CL**9
00026 *       CANCELS AND 'A+8' TYPE CLAIMS. CLAIMS ARE WRITTEN TO A       CL**9
00027 *       SEPARATE FILE FROM ISSUES AND CANCELS. BATCH REMIT RECORDS   CL**9
00028 *       ARE CREATED ON ISSUE/CANCEL ACCOUNT BREAKS.                  CL**9
00029                                                                   EL511
00030  ENVIRONMENT DIVISION.                                            EL511
00031  INPUT-OUTPUT SECTION.                                            EL511
00032  FILE-CONTROL.                                                    EL511
00033                                                                   EL511
00034      SELECT  OLD-INPUT       ASSIGN TO SYS010-UT-3380-S-SYS010.   EL511
00035      SELECT  ISS-CAN-OUTPUT  ASSIGN TO SYS011-UT-3380-S-SYS011.   EL511
00036      SELECT  CLAIM-OUTPUT    ASSIGN TO SYS012-UT-3380-S-SYS012.   EL511
00037      SELECT  DISK-DATE       ASSIGN TO SYS019-UT-3380-S-SYS019.   EL511
00038      SELECT  PRINTER         ASSIGN TO SYS008-UR-1403-S-SYS008.   EL511
00039      SELECT  FICH            ASSIGN TO SYS020-UT-2400-S-SYS020.   EL511
00040      SELECT  SORT-FILE       ASSIGN TO SYS001-UT-3380-S-SORTWK1.  EL511
00041      SELECT  ELREPT          ASSIGN TO SYS023-3380-ELREPT         EL511
00042                              ORGANIZATION IS INDEXED              EL511
00043                              ACCESS IS DYNAMIC                    EL511
00044                              RECORD KEY IS RF-CONTROL-PRIMARY     EL511
00045                              FILE STATUS IS DTE-VSAM-FLAGS.       EL511
00046                                                                   EL511
00047  DATA DIVISION.                                                   EL511
00048  FILE SECTION.                                                    EL511
00049                                                                   EL511
00050  FD  OLD-INPUT                                                    EL511
00051      BLOCK CONTAINS 0 RECORDS
00052      RECORDING MODE F.                                               CL**9
00053                                                                      CL**9
00054  01  INPUT-RECORD.                                                EL511
00055      12  INP-ST                  PIC XX.                          EL511
00056      12  INP-ACCOUNT             PIC X(6).                        EL511
00057      12  INP-EFF-DT              PIC X(6).                        EL511
00058      12  INP-CERT                PIC X(8).                        EL511
00059      12  INP-TRAN-1              PIC X.                           EL511
00060      12  FILLER                  PIC X(51).                       EL511
00061      12  INP-CARR                PIC X.                           EL511
00062      12  INP-GROUP               PIC XXX.                         EL511
00063      12  INP-FORCE-CD            PIC X.                           EL511
00064      12  INP-TRAN-2              PIC X.                           EL511
00065                                                                   EL511
00066  FD  ISS-CAN-OUTPUT                                               EL511
00067      BLOCK CONTAINS 0 RECORDS
00068      RECORDING MODE F.                                               CL**9
00069                                                                      CL**9
00070  01  ISS-CAN-RECORD             PIC X(80).                        EL511
00071                                                                   EL511
00072  FD  CLAIM-OUTPUT                                                 EL511
00073      BLOCK CONTAINS 0 RECORDS
00074      RECORDING MODE F.                                               CL**9
00075                                                                      CL**9
00076  01  CLAIM-RECORD               PIC X(80).                        EL511
00077                                                                   EL511
00078      EJECT                                                        EL511
00079  FD  DISK-DATE                  COPY ELCDTEFD.                    EL511
00080                                                                   EL511
00081      EJECT                                                        EL511
00082  FD  PRINTER                                                      EL511
00083                                 COPY ELCPRTFD.                    EL511
00084      EJECT                                                        EL511
00085  FD  FICH                                                         EL511
00086                                 COPY ELCFCHFD.                    EL511
00087      EJECT                                                        EL511
00088  FD  ELREPT                                                          CL**4
00089                                 COPY ELCRPTFD.                    EL511
00090                                 COPY ELCREPT.                     EL511
00091                                                                   EL511
00092  SD  SORT-FILE.                                                      CL**9
00093                                                                      CL**9
00094  01  SORT-RECORD.                                                 EL511
00095      12  SR-CONTROL.                                              EL511
00096          16  SR-BAT-SEQ-NO           PIC S9(4) COMP.              EL511
00097          16  SR-REC-SEQ-NO           PIC S9(4) COMP.              EL511
00098              88  BATCH-HEADER                        VALUE +0.    EL511
00099      12  SR-PENDING-RECORD           PIC X(80).                   EL511
00100      EJECT                                                        EL511
00101                                                                   EL511
00102  WORKING-STORAGE SECTION.                                         EL511
00103  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.         CL**9
00104  77  LCP-ONCTR-01                  PIC S9(8) COMP-3 VALUE ZERO.      CL**9
00105                                                                   EL511
00106  77  FILLER  PIC X(32) VALUE '********************************'.  EL511
00107  77  FILLER  PIC X(32) VALUE '      EL511 WORKING-STORAGE     '.  EL511
00108  77  FILLER  PIC X(32) VALUE '***********VMOD=2.010***********'.     CL*10
00109                                                                   EL511
00110  01  WS-MISC.                                                     EL511
00111      12  PGM-SUB                     PIC S9(4) COMP   VALUE +511. EL511
00112      12  WS-RETURN-CODE              PIC S9(4) COMP.              EL511
00113      12  WS-ABEND-MESSAGE            PIC X(80).                   EL511
00114      12  WS-ABEND-FILE-STATUS        PIC XX           VALUE ZERO. EL511
00115      12  WS-ZERO                     PIC S9    COMP-3 VALUE ZERO. EL511
00116      12  OLC-REPORT-NAME             PIC X(5)  VALUE 'EL511'.     EL511
00117                                                                   EL511
00118      12  WS-SAVE-KEY.                                             EL511
00119          16  WS-SAVE-COMP.                                        EL511
00120              20  WS-SAVE-CARRIER     PIC X     VALUE SPACES.      EL511
00121              20  WS-SAVE-GROUPING    PIC X(6)  VALUE SPACES.      EL511
00122          16  WS-SAVE-STATE           PIC XX    VALUE SPACES.      EL511
00123          16  WS-SAVE-ACCOUNT         PIC X(10) VALUE SPACES.      EL511
00124                                                                   EL511
00125      12  WS-FIRST-TIME-SW            PIC X     VALUE '0'.            CL**3
00126                                                                   EL511
00127      12  WS-CURRENT-KEY.                                          EL511
00128          16  WSC-CARR                PIC X.                       EL511
00129          16  WSC-GROUP               PIC XXX.                     EL511
00130          16  WSC-ST                  PIC XX.                      EL511
00131          16  WSC-ACCOUNT             PIC X(6).                    EL511
00132          16  WSC-EFF-DT              PIC X(6).                    EL511
00133          16  WSC-CERT                PIC X(8).                    EL511
00134                                                                   EL511
00135      12  WS-PRIOR-KEY.                                            EL511
00136          16  WSP-CARR                PIC X.                       EL511
00137          16  WSP-GROUP               PIC XXX.                     EL511
00138          16  WSP-ST                  PIC XX.                      EL511
00139          16  WSP-ACCOUNT             PIC X(6).                    EL511
00140          16  WSP-EFF-DT              PIC X(6).                    EL511
00141          16  WSP-CERT                PIC X(8).                    EL511
00142                                                                   EL511
00143      12  WS-ACCOUNT.                                              EL511
00144          16  WS-ACCOUNT-ZERO         PIC X(4)  VALUE ZERO.           CL**5
00145          16  WS-ACCOUNT-PRIME        PIC X(6).                    EL511
00146      12  WS-GROUP.                                                EL511
00147          16  FILLER                  PIC X(3)  VALUE ZERO.        EL511
00148          16  WS-GROUP-PRIME          PIC X(3).                    EL511
00149      12  WS-CERT-NO.                                              EL511
00150          16  FILLER                  PIC X(3)  VALUE ZERO.        EL511
00151          16  WS-CERT-PRIME           PIC X(8).                    EL511
00152      12  WS-CHECK-NO.                                                CL**4
00153          16  FILLER                  PIC X(2)  VALUE ZERO.           CL**4
00154          16  WS-CHECK-PRIME          PIC X(5).                       CL**4
00155                                                                   EL511
00156      12  WS-LINE-CNT                 PIC S9(6) VALUE +99 COMP-3.  EL511
00157      12  WS-PAGE-CNT                 PIC S9(6) VALUE +0  COMP-3.  EL511
00158      12  X                           PIC X.                       EL511
00159      12  WS-ALL-NINES                PIC 9(6)V99 VALUE 999999.99.    CL**4
00160      12  WS-SEQ-NO                   PIC S9(4) VALUE +0  COMP.    EL511
00161      12  WS-BAT-NO                   PIC S9(4) VALUE +0  COMP.    EL511
00162      12  WS-NO-RECORDS-RELEASED      PIC S9(7) VALUE +0  COMP-3.  EL511
00163      12  WS-OUTPUT-RECORDS           PIC S9(7) VALUE +0  COMP-3.  EL511
00164      12  WS-OUTPUT-CLAIMS            PIC S9(7) VALUE +0  COMP-3.     CL**4
00165      12  WS-OUTPUT-RESERVES          PIC S9(7) VALUE +0  COMP-3.     CL**4
00166      12  WS-NO-BATCHES               PIC S9(7) VALUE +0  COMP-3.  EL511
00167      12  WS-DISP-RECORDS-RELEASED    PIC Z,ZZZ,ZZ9.               EL511
00168      12  WS-DISP-OUTPUT-RECORDS      PIC Z,ZZZ,ZZ9.               EL511
00169      12  WS-DISP-NO-BATCHES          PIC Z,ZZZ,ZZ9.               EL511
00170      12  WS-DISP-OUTPUT-CLAIMS       PIC Z,ZZZ,ZZ9.                  CL**4
00171      12  WS-DISP-OUTPUT-RESERVES     PIC Z,ZZZ,ZZ9.                  CL**4
00172                                                                   EL511
00173      12  WS-CANCEL-CNT               PIC S9(5)    VALUE +0 COMP-3.EL511
00174      12  WS-ISSUE-CNT                PIC S9(5)    VALUE +0 COMP-3.EL511
00175                                                                   EL511
00176      12  WS-LF-CANCEL-AMT            PIC S9(7)V99 VALUE +0 COMP-3.EL511
00177      12  WS-AH-CANCEL-AMT            PIC S9(7)V99 VALUE +0 COMP-3.EL511
00178      12  WS-LF-ISSUE-AMT             PIC S9(7)V99 VALUE +0 COMP-3.EL511
00179      12  WS-AH-ISSUE-AMT             PIC S9(7)V99 VALUE +0 COMP-3.EL511
00180      12  WS-HOLD-CANCEL-CNT          PIC S9(5)    VALUE +0 COMP-3.   CL**6
00181      12  WS-HOLD-ISSUE-CNT           PIC S9(5)    VALUE +0 COMP-3.   CL**6
00182      12  WS-HOLD-LF-ISSUE-AMT        PIC S9(7)V99 VALUE +0 COMP-3.   CL**6
00183      12  WS-HOLD-AH-ISSUE-AMT        PIC S9(7)V99 VALUE +0 COMP-3.   CL**6
00184      12  WS-HOLD-AH-CANCEL-AMT       PIC S9(7)V99 VALUE +0 COMP-3.   CL**6
00185      12  WS-HOLD-LF-CANCEL-AMT       PIC S9(7)V99 VALUE +0 COMP-3.   CL**6
00186      12  FIRST-R-RECORD-SW           PIC X VALUE 'Y'.                CL**6
00187          88 FIRST-R-RECORD                 VALUE 'Y'.                CL**6
00188                                                                   EL511
00189      12 SPACE-NP                     PIC X VALUE '1'.             EL511
00190      12 SPACE-1                      PIC X VALUE ' '.             EL511
00191      12 SPACE-2                      PIC X VALUE '0'.             EL511
00192      12 SPACE-3                      PIC X VALUE '-'.             EL511
00193      EJECT                                                        EL511
00194                                  COPY ECSCRD01.                      CL**6
00195      EJECT                                                        EL511
00196                                  COPY ERCPNDCI.                      CL**6
00197                                                                   EL511
00198      EJECT                                                        EL511
00199                                  COPY ERCPNDBI.                      CL**6
00200                                                                   EL511
00201      EJECT                                                        EL511
00202                                                                   EL511
00203  01  OLD-RECORD-1.                                                EL511
00204      12  PB1-RECORD-BODY                       PIC X(80).         EL511
00205      12  PB1-ISSUE-RECORD                      REDEFINES          EL511
00206          PB1-RECORD-BODY.                                         EL511
00207          16  PB1-I-STATE                       PIC XX.            EL511
00208          16  PB1-I-ACCOUNT                     PIC X(6).          EL511
00209          16  PB1-I-CERT-EFF-DT.                                   EL511
00210              20  PB1-I-CERT-EFF-MO             PIC S99.           EL511
00211              20  PB1-I-CERT-EFF-DA             PIC S99.           EL511
00212              20  PB1-I-CERT-EFF-YR             PIC S99.           EL511
00213          16  PB1-I-CERT-EFF-DT-X               REDEFINES          EL511
00214              PB1-I-CERT-EFF-DT                 PIC X(6).          EL511
00215          16  PB1-I-CERT-NO.                                       EL511
00216              20  PB1-I-CERT-PRIME              PIC X(7).          EL511
00217              20  PB1-I-CERT-SFX                PIC X.             EL511
00218          16  PB1-I-CERT-FORM                   PIC X(52).         EL511
00219          16  PB1-I-CERT-FORM-1                 REDEFINES          EL511
00220              PB1-I-CERT-FORM.                                     EL511
00221              20  PB1-I-CERT-ISSUE.                                EL511
00222                  24  PB1-I-INSURED-NAME.                          EL511
00223                      28  FILLER                PIC X(11).         EL511
CIDMOD*                    28  PB1-I-IG-OVERRIDE     PIC X.             EL511
00225                  24  PB1-I-INSURED-INITIALS.                      EL511
00226                      28  PB1-I-1ST-INIT        PIC X.             EL511
00227                      28  PB1-I-MIDDLE-INIT     PIC X.             EL511
CIDMOD                 24  PB1-I-IG-OVERRIDE         PIC X.             EL511
00228                  24  PB1-I-INSURED-SEX         PIC X.             EL511
00229                  24  PB1-I-INSURED-AGE         PIC S99.           EL511
00230                  24  PB1-I-INSURED-AGE-X       REDEFINES          EL511
00231                      PB1-I-INSURED-AGE         PIC XX.            EL511
00232              20  PB1-I-CERT-OB-ISSUE           REDEFINES          EL511
00233                  PB1-I-CERT-ISSUE.                                EL511
00234                  24  PB1-I-OB-LF-RT            PIC S9V9999.       EL511
00235                  24  PB1-I-OB-LF-RT-X          REDEFINES          EL511
00236                      PB1-I-OB-LF-RT            PIC X(5).          EL511
00237                  24  PB1-I-OB-AH-RT            PIC S9V9999.       EL511
00238                  24  PB1-I-OB-AH-RT-X          REDEFINES          EL511
00239                      PB1-I-OB-AH-RT            PIC X(5).          EL511
00240                  24  FILLER                    PIC X(7).          EL511
00241              20  PB1-I-ORIG-TERM               PIC S9(3).         EL511
00242              20  PB1-I-ORIG-TERM-X             REDEFINES          EL511
00243                  PB1-I-ORIG-TERM               PIC XXX.           EL511
00244              20  PB1-I-LF-BENEFIT-TYPE         PIC XX.            EL511
00245              20  PB1-I-LF-BENEFIT-AMT          PIC S9(6)V99.      EL511
00246              20  PB1-I-LF-BENEFIT-AMT-X        REDEFINES          EL511
00247                  PB1-I-LF-BENEFIT-AMT          PIC X(8).          EL511
00248              20  PB1-I-LOB-BENEFIT-AMT         REDEFINES          EL511
00249                  PB1-I-LF-BENEFIT-AMT          PIC S9(8).         EL511
00250              20  PB1-I-LF-PREM-AMT             PIC S9(4)V99.      EL511
00251              20  PB1-I-LF-PREM-AMT-X           REDEFINES          EL511
00252                  PB1-I-LF-PREM-AMT             PIC X(6).          EL511
00253              20  PB1-I-AH-BENEFIT-TYPE         PIC XX.            EL511
00254              20  PB1-I-AH-BENEFIT-AMT          PIC S9(5)V99.      EL511
00255              20  PB1-I-AH-BENEFIT-AMT-X        REDEFINES          EL511
00256                  PB1-I-AH-BENEFIT-AMT          PIC X(7).          EL511
00257              20  PB1-I-AH-OB-BENEFIT-AMT       REDEFINES          EL511
00258                  PB1-I-AH-BENEFIT-AMT          PIC S9(7).         EL511
00259              20  PB1-I-AH-PREM-AMT             PIC S9(4)V99.      EL511
00260              20  PB1-I-AH-PREM-AMT-X           REDEFINES          EL511
00261                  PB1-I-AH-PREM-AMT             PIC X(6).          EL511
00262              20  PB1-I-REIN-CODE               PIC X.             EL511
00263          16  PB1-CARRIER                       PIC X.             EL511
00264          16  PB1-GROUP                         PIC XXX.           EL511
00265          16  PB1-I-FORCE-CD                    PIC X.             EL511
00266              88  ISSUE-FORM-2                  VALUE ' ' '2'.     EL511
00267              88  ISSUE-FORM-3                  VALUE '3'.         EL511
00268              88  ISSUE-FORM-4                  VALUE '4'.         EL511
00269              88  ISSUE-FORM-5                  VALUE '5'.         EL511
00270          16  PB1-I-TRANS-TYPE                  PIC X.             EL511
00271                                                                   EL511
00272      EJECT                                                        EL511
00273  01  OLD-RECORD-2.                                                EL511
00274      12  PB2-RECORD-BODY                       PIC X(80).         EL511
00275      12  PB2-ISSUE-RECORD                      REDEFINES          EL511
00276          PB2-RECORD-BODY.                                         EL511
00277              20  FILLER                        PIC X(22).         EL511
00278              20  PB2-I-SOC-SEC-NO              PIC X(11).         EL511
00279              20  PB2-I-LOAN-APR                PIC 9(3)V9(4).     EL511
00280              20  PB2-I-LOAN-APR-X              REDEFINES          EL511
00281                  PB2-I-LOAN-APR                PIC X(7).          EL511
00282                                                                   EL511
00283              20  PB2-I-INDV-GRP-CD             PIC X.             EL511
00284              20  PB2-I-PAY-FREQUENCY           PIC 99.            EL511
00285              20  PB2-I-PAY-FREQUENCY-X         REDEFINES          EL511
00286                  PB2-I-PAY-FREQUENCY           PIC XX.            EL511
00287              20  PB2-I-SKIP-CODE               PIC XX.            EL511
00288              20  PB2-I-BIRTHDAY.                                  EL511
00289                  24  PB2-I-BIRTH-MO            PIC S99.           EL511
00290                  24  PB2-I-BIRTH-DA            PIC S99.           EL511
00291                  24  PB2-I-BIRTH-YR            PIC S99.           EL511
00292              20  PB2-I-BIRTHDAY-X              REDEFINES          EL511
00293                  PB2-I-BIRTHDAY                PIC X(6).          EL511
00294              20  PB2-I-TERM-AS-DAYS            REDEFINES          EL511
00295                  PB2-I-BIRTHDAY.                                  EL511
00296                  24  FILLER                    PIC XX.            EL511
00297                  24  PB2-I-TERM-IN-DAYS        PIC S9(4).         EL511
00298              20  PB2-I-SPOUSE-ISSUE-AGE        PIC S99.           EL511
00299              20  PB2-I-SPOUSE-ISSUE-AGE-X      REDEFINES          EL511
00300                  PB2-I-SPOUSE-ISSUE-AGE        PIC XX.            EL511
00301              20  PB2-I-EXTENTION-DAYS          REDEFINES          EL511
00302                  PB2-I-SPOUSE-ISSUE-AGE        PIC S99.           EL511
00303                                                                   EL511
00304              20  PB2-I-SIG-SW                  PIC X.             EL511
00305              20  FILLER                        PIC X.             EL511
00306              20  PB2-I-RATE-CLASS              PIC XX.            EL511
00307              20  PB2-I-LOAN-TERM               PIC S999.          EL511
00308              20  PB2-I-LOAN-TERM-X             REDEFINES          EL511
00309                  PB2-I-LOAN-TERM               PIC XXX.           EL511
00310              20  PB2-I-LOAN-OFFICER            REDEFINES          EL511
00311                  PB2-I-LOAN-TERM               PIC XXX.           EL511
00312              20  FILLER                        PIC XX.            EL511
00313              20  PB2-I-MEMBER-NO               PIC X(12).         EL511
00314              20  FILLER                        PIC X(6).          EL511
00315                                                                   EL511
00316      EJECT                                                        EL511
00317  01  OLD-RECORD-3.                                                EL511
00318      12  PB3-RECORD-BODY                       PIC X(80).         EL511
00319      12  PB3-ISSUE-RECORD                      REDEFINES          EL511
00320          PB3-RECORD-BODY.                                         EL511
00321              20  FILLER                        PIC X(22).         EL511
00322              20  PB3-I-INSURED-LAST-NAME       PIC X(15).         EL511
00323              20  PB3-I-INSURED-FIRST-NAME      PIC X(10).         EL511
00324              20  PB3-I-INSURED-MID-INIT        PIC X.             EL511
00325              20  PB3-I-INSURED-BIRTHDAY.                          EL511
00326                  24  PB3-I-BTH-MONTH           PIC 99.            EL511
00327                  24  PB3-I-BTH-DAY             PIC 99.            EL511
00328                  24  PB3-I-BTH-YEAR            PIC 99.            EL511
00329              20  PB3-I-INSURED-SOC-SEC-NO      PIC X(11).         EL511
00330              20  FILLER                        PIC X(15).            CL**9
00331                                                                   EL511
00332      EJECT                                                        EL511
00333  01  OLD-RECORD-4.                                                EL511
00334      12  PB4-RECORD-BODY                       PIC X(80).         EL511
00335      12  PB4-ISSUE-RECORD                      REDEFINES          EL511
00336          PB4-RECORD-BODY.                                         EL511
00337              20  FILLER                        PIC X(22).         EL511
00338              20  PB4-I-INSURED-ADDRESS-1       PIC X(30).         EL511
00339              20  PB4-I-INSURED-ADDRESS-2       PIC X(22).         EL511
00340              20  FILLER                        PIC X(6).          EL511
00341                                                                   EL511
00342      EJECT                                                        EL511
00343  01  OLD-RECORD-5.                                                EL511
00344      12  PB5-RECORD-BODY                       PIC X(80).         EL511
00345      12  PB5-ISSUE-RECORD                      REDEFINES          EL511
00346          PB5-RECORD-BODY.                                         EL511
00347              20  FILLER                        PIC X(22).         EL511
00348              20  PB5-I-INSURED-CITY-STATE      PIC X(30).         EL511
00349              20  PB5-I-INSURED-ZIP-CODE        PIC X(9).          EL511
00350              20  PB5-I-INSURED-PHONE-NO        PIC X(10).         EL511
00351              20  FILLER                        PIC X(9).             CL**9
00352      EJECT                                                        EL511
00353  01  PB1-CANCEL-RECORD.                                           EL511
00354          16  PB1-C-STATE                       PIC XX.            EL511
00355          16  PB1-C-ACCOUNT                     PIC X(6).          EL511
00356          16  PB1-C-CERT-EFF-DT.                                   EL511
00357              20  PB1-C-CERT-EFF-MO             PIC S99.           EL511
00358              20  PB1-C-CERT-EFF-DA             PIC S99.           EL511
00359              20  PB1-C-CERT-EFF-YR             PIC S99.           EL511
00360          16  PB1-C-CERT-EFF-DT-X               REDEFINES          EL511
00361              PB1-C-CERT-EFF-DT                 PIC X(6).          EL511
00362          16  PB1-C-CERT-NO.                                       EL511
00363              20  PB1-C-CERT-PRIME              PIC X(7).          EL511
00364              20  PB1-C-CERT-SFX                PIC X.             EL511
00365          16  PB1-C-TRANS-CD                    PIC X.             EL511
00366          16  PB1-C-CANCEL-DATE.                                   EL511
00367              20  PB1-C-CAN-MO                  PIC S99.           EL511
00368              20  PB1-C-CAN-DA                  PIC S99.           EL511
00369              20  PB1-C-CAN-YR                  PIC S99.           EL511
00370          16  PB1-C-CANCEL-DATE-X               REDEFINES          EL511
00371              PB1-C-CANCEL-DATE                 PIC X(6).          EL511
00372          16  PB1-C-LF-PREM-REFUND              PIC S9(4)V99.      EL511
00373          16  PB1-C-LF-PREM-REFUND-X            REDEFINES          EL511
00374              PB1-C-LF-PREM-REFUND              PIC X(6).          EL511
00375          16  PB1-C-AH-PREM-REFUND              PIC S9(4)V99.      EL511
00376          16  PB1-C-AH-PREM-REFUND-X            REDEFINES          EL511
00377              PB1-C-AH-PREM-REFUND              PIC X(6).          EL511
00378          16  FILLER                            PIC X.             EL511
00379          16  PB1-C-INSURED-NAME                PIC X(14).         EL511
00380          16  FILLER                            PIC X(18).         EL511
00381          16  PB1-C-COMP-CD                     PIC X(4).          EL511
00382          16  PB1-C-FORCE-CD                    PIC X.             EL511
00383          16  PB1-C-TRANS-TYPE                  PIC X.             EL511
00384                                                                   EL511
00385      EJECT                                                        EL511
00386                                                                   EL511
00387  01  CLAIM-REC-LAYOUT.                                            EL511
00388      12  PC1-STATE                   PIC XX.                      EL511
00389      12  PC1-ACCOUNT-NO              PIC X(6).                    EL511
00390      12  PC1-EFF-DATE.                                            EL511
00391          16  PC1-EFF-MO              PIC 99.                      EL511
00392          16  PC1-EFF-DA              PIC 99.                      EL511
00393          16  PC1-EFF-YR              PIC 99.                      EL511
00394      12  PC1-CERT-NO.                                             EL511
00395          16  PC1-CERT-PREFIX         PIC X(7).                    EL511
00396          16  PC1-CERT-SUFFIX         PIC X.                       EL511
00397      12  PC1-TRANS                   PIC X.                       EL511
00398          88  PC1-VALID-TRANS             VALUE '7'.               EL511
00399          88  PC1-PMT                     VALUE '7'.               EL511
00400          88  PC1-CHANGE                  VALUE '9'.               EL511
00401      12  PC1-CODE                    PIC X.                       EL511
00402          88  PC1-VALID-CODE              VALUE '1' THRU '4'.      EL511
00403          88  PC1-LIFE                    VALUE '1'.               EL511
00404          88  PC1-DISAB                   VALUE '2'.               EL511
00405          88  PC1-OB-LIFE                 VALUE '3'.               EL511
00406          88  PC1-OB-DISAB                VALUE '4'.               EL511
00407      12  PC1-OVERLAY-AREA.                                        EL511
00408          16  PC1-INC-DATE.                                        EL511
00409              20  PC1-INC-MO          PIC 99.                      EL511
00410              20  PC1-INC-DA          PIC 99.                      EL511
00411              20  PC1-INC-YR          PIC 99.                      EL511
00412          16  PC1-RPT-DATE.                                        EL511
00413              20  PC1-RPT-MO          PIC 99.                      EL511
00414              20  PC1-RPT-DA          PIC 99.                      EL511
00415              20  PC1-RPT-YR          PIC 99.                      EL511
00416          16  PC1-PAID-DATE.                                       EL511
00417              20  PC1-PAID-MO         PIC 99.                      EL511
00418              20  PC1-PAID-DA         PIC 99.                      EL511
00419              20  PC1-PAID-YR         PIC 99.                      EL511
00420          16  PC1-PD-THRU-DATE.                                    EL511
00421              20  PC1-PD-THRU-MO      PIC 99.                      EL511
00422              20  PC1-PD-THRU-DA      PIC 99.                      EL511
00423              20  PC1-PD-THRU-YR      PIC 99.                      EL511
00424          16  PC1-PAID-AMT            PIC S9(6)V99.                EL511
00425          16  PC1-AMT REDEFINES PC1-PAID-AMT                       EL511
00426                                      PIC X(8).                    EL511
00427      12  RESERVE-OVERLAY-AREA REDEFINES PC1-OVERLAY-AREA.         EL511
00428          16  PC1-IBNR                PIC S9(6)V99.                EL511
00429          16  PC1-IBNR-X   REDEFINES PC1-IBNR                      EL511
00430                                      PIC X(8).                    EL511
00431          16  PC1-PTC                 PIC S9(6)V99.                EL511
00432          16  PC1-PTC-X    REDEFINES PC1-PTC                       EL511
00433                                      PIC X(8).                    EL511
00434          16  PC1-FUTURE              PIC S9(6)V99.                EL511
00435          16  PC1-FUTURE-X REDEFINES PC1-FUTURE                    EL511
00436                                      PIC X(8).                    EL511
00437          16  PC1-NO-R                PIC X(7).                    EL511
00438          16  FILLER                  PIC X.                       EL511
00439                                                                   EL511
00440      12  PC1-NO                      PIC X(7).                    EL511
00441      12  PC1-A-INC-DATE REDEFINES PC1-NO.                            CL**4
00442          16  PC1-R-INC-DATE          PIC X(6).                       CL**4
00443          16  FILLER                  PIC X.                          CL**4
00444      12  PC1-CHECK-NO                PIC X(5).                    EL511
00445      12  PC1-DAYS-DISAB              PIC S9(3).                   EL511
00446      12  PC1-DAYS REDEFINES PC1-DAYS-DISAB.                       EL511
00447          16  PC1-CAUSE               PIC XX.                      EL511
00448          16  FILLER                  PIC X.                       EL511
00449      12  PC1-AGE-DTH.                                             EL511
00450          16  PC1-AGE                 PIC 99.                      EL511
00451      12  PC1-TYPE-PMT                PIC X.                       EL511
00452          88  PC1-VALID-TYPE-PMT          VALUE '1' THRU '6' '9'   EL511
00453              'P' 'F' 'L' 'A' 'C' 'N' 'V'.                         EL511
00454          88  PC1-PARTIAL                 VALUE '1' 'P'.           EL511
00455          88  PC1-FINAL                   VALUE '2' 'F'.           EL511
00456          88  PC1-LUMP-SUM                VALUE '3' 'L'.           EL511
00457          88  PC1-ADDITIONAL              VALUE '4' 'A'.           EL511
00458          88  PC1-CHARGEABLE              VALUE '5' 'C'.           EL511
00459          88  PC1-NON-CHARGEABLE          VALUE '6' 'N'.           EL511
00460          88  PC1-VOIDED                  VALUE '9' 'V'.           EL511
00461      12  PC1-CARR-GROUP.                                          EL511
00462          16  PC1-CARR                PIC X.                       EL511
00463          16  PC1-GROUP               PIC X(3).                    EL511
00464      12  PC1-FORCE-CD                PIC X.                       EL511
00465          88  PC1-VALID-FORCE             VALUE ' ' '0' '6'        EL511
00466                                                '7' '8'.           EL511
00467      12  PC1-ACTION                  PIC X(1).                    EL511
00468          88  PC1-VALID-ACTION            VALUE 'A' 'Y'.           EL511
00469          88  ACTION-ADD-CHG              VALUE 'A'.               EL511
00470          88  ACTION-DELETE               VALUE 'D'.               EL511
00471          88  RESERVE-INPUT               VALUE 'Y'.               EL511
00472                                                                   EL511
00473  01  WS-HEADING1.                                                 EL511
00474      12  FILLER                      PIC X(48) VALUE SPACES.         CL**4
00475      12  FILLER                      PIC X(26) VALUE              EL511
00476          'INPUT DATA CONVERSION LIST'.                            EL511
00477      12  FILLER                      PIC X(47) VALUE SPACES.         CL**9
00478      12  FILLER                      PIC X(8) VALUE ' EL511'.        CL**9
00479      12  FILLER                      PIC X(5) VALUE SPACES.          CL**9
00480                                                                   EL511
00481  01  WS-HEADING2.                                                 EL511
00482      12  FILLER                      PIC X(45) VALUE SPACES.      EL511
00483      12  WS-H2-CLIENT-NAME           PIC X(30) VALUE SPACES.      EL511
00484      12  FILLER                      PIC X(49) VALUE SPACES.      EL511
00485      12  WS-H2-DATE                  PIC X(8).                    EL511
00486                                                                   EL511
00487  01  WS-HEADING3.                                                 EL511
00488      12  FILLER                      PIC X(52) VALUE SPACES.      EL511
00489      12  WS-H3-DATE                  PIC X(18).                   EL511
00490      12  FILLER                      PIC X(41) VALUE SPACES.      EL511
00491      12  FILLER                      PIC X(4) VALUE 'PAGE'.       EL511
00492      12  WS-H3-PAGE                  PIC ZZZ9.                    EL511
00493                                                                   EL511
00494  01  WS-HEADING4.                                                 EL511
00495      12  FILLER                      PIC X(2)  VALUE  '  '.       EL511
00496      12  FILLER                      PIC X(40) VALUE              EL511
00497                       '....5...10...15...20...25...30...35...40'. EL511
00498      12  FILLER                      PIC X(40) VALUE              EL511
00499                       '...45...50...55...60...65...70...75...80'. EL511
00500                                                                   EL511
00501  01  WS-DETAIL1.                                                  EL511
00502      12  FILLER                      PIC XX    VALUE SPACES.      EL511
00503      12  D1-CARD-IMAGE               PIC X(80).                   EL511
00504      12  FILLER                      PIC XXX   VALUE SPACES.      EL511
00505      12  D1-MESSAGE                  PIC X(25).                   EL511
00506      12  FILLER                      PIC X(4)  VALUE SPACES.      EL511
00507      12  D1-B-CNT                    PIC 99999 BLANK WHEN ZERO.   EL511
00508      12  FILLER                      PIC X(4)  VALUE SPACES.      EL511
00509      12  D1-R-CNT                    PIC 99999 BLANK WHEN ZERO.   EL511
00510                                                                   EL511
00511  01  WS-DETAIL2.                                                  EL511
00512      12  FILLER                      PIC XX    VALUE SPACES.      EL511
00513      12  D2-COMMENT                  PIC X(30).                   EL511
00514      12  FILLER                      PIC XXX   VALUE SPACES.      EL511
00515      12  D2-COUNT                    PIC ZZZ,ZZ9.                 EL511
00516      12  FILLER                      PIC X(90) VALUE SPACES.      EL511
00517                                                                   EL511
00518                                  COPY ELCDTECX.                      CL*10
00519                                                                      CL*10
00520                                  COPY ELCDTEVR.                      CL*10
00521                                                                   EL511
00522      EJECT                                                        EL511
00523  PROCEDURE DIVISION.                                              EL511
00524                                                                   EL511
00525  0000-LOAD-ENVIRONMENT-FILE.                                      EL511
00526                                  COPY ELCDTERX.                      CL*10
00527                                                                   EL511
00528  0000-OPEN-ROUTINE SECTION.                                       EL511
00529      OPEN INPUT OLD-INPUT                                         EL511
00530           OUTPUT ISS-CAN-OUTPUT                                   EL511
00531                  CLAIM-OUTPUT                                     EL511
00532                  PRINTER.                                         EL511
00533                                                                   EL511
00534      SORT SORT-FILE                                               EL511
00535          ON ASCENDING KEY SR-CONTROL                              EL511
00536              INPUT PROCEDURE  1000-SORT-INPUT-PROCEDURE           EL511
00537              OUTPUT PROCEDURE 2000-SORT-OUTPUT-PROCEDURE.         EL511
00538                                                                   EL511
00539      MOVE WS-NO-RECORDS-RELEASED    TO D2-COUNT.                  EL511
00540      MOVE 'NO. RECORDS RELEASED - ' TO D2-COMMENT.                EL511
00541      MOVE WS-DETAIL2                TO P-DATA.                    EL511
00542      MOVE SPACE-NP                  TO X.                            CL**5
00543      PERFORM 4000-PRINT THRU 4999-EXIT.                           EL511
00544      MOVE WS-OUTPUT-RECORDS         TO D2-COUNT.                  EL511
00545      MOVE 'NO. OF DETAIL RECORDS -' TO D2-COMMENT.                EL511
00546      MOVE WS-DETAIL2                TO P-DATA.                    EL511
00547      MOVE SPACE-1                   TO X.                         EL511
00548      PERFORM 4000-PRINT THRU 4999-EXIT.                           EL511
00549      MOVE WS-NO-BATCHES             TO D2-COUNT.                  EL511
00550      MOVE 'NO. BATCHES RELEASED - ' TO D2-COMMENT.                EL511
00551      MOVE WS-DETAIL2                TO P-DATA.                       CL**4
00552      MOVE SPACE-1                   TO X.                            CL**4
00553      PERFORM 4000-PRINT THRU 4999-EXIT.                              CL**4
00554      MOVE WS-OUTPUT-CLAIMS          TO D2-COUNT.                     CL**4
00555      MOVE 'NO. CLAIMS RELEASED - '  TO D2-COMMENT.                   CL**4
00556      MOVE WS-DETAIL2                TO P-DATA.                       CL**4
00557      MOVE SPACE-1                   TO X.                            CL**4
00558      PERFORM 4000-PRINT THRU 4999-EXIT.                              CL**4
00559      MOVE WS-OUTPUT-RESERVES        TO D2-COUNT.                     CL**4
00560      MOVE 'NO. RESERVES RELEASED- ' TO D2-COMMENT.                   CL**4
00561      MOVE WS-DETAIL2                TO P-DATA.                    EL511
00562      MOVE SPACE-1                   TO X.                         EL511
00563      PERFORM 4000-PRINT THRU 4999-EXIT.                           EL511
00564                                                                   EL511
00565      CLOSE OLD-INPUT                                              EL511
00566            ISS-CAN-OUTPUT                                         EL511
00567            CLAIM-OUTPUT                                           EL511
00568            PRINTER.                                               EL511
00569  0000-CLOSE-2.                                                    EL511
00570                                  COPY ELCPRTCX.                   EL511
00571                                                                   EL511
00572      GOBACK.                                                         CL**9
00573                                                                   EL511
00574  0000-EXIT.                                                       EL511
00575      EXIT.                                                        EL511
00576      EJECT                                                        EL511
00577                                                                   EL511
00578  1000-SORT-INPUT-PROCEDURE SECTION.                               EL511
00579      MOVE SPACES                 TO PB1-CANCEL-RECORD             EL511
00580                                     OLD-RECORD-1                  EL511
00581                                     OLD-RECORD-2                  EL511
00582                                     OLD-RECORD-3                  EL511
00583                                     OLD-RECORD-4                  EL511
00584                                     OLD-RECORD-5.                 EL511
00585                                                                   EL511
00586  1010-SIP.                                                        EL511
00587      READ OLD-INPUT                                               EL511
00588          AT END                                                   EL511
00589              MOVE '9'            TO WS-FIRST-TIME-SW                 CL**3
00590               GO TO 1020-REC-OUT.                                 EL511
00591                                                                   EL511
00592      IF WS-LINE-CNT GREATER +60                                   EL511
00593          PERFORM 5000-HEADINGS THRU 5999-EXIT.                    EL511
00594                                                                   EL511
00595      IF INP-ST = '$$'                                             EL511
00596          GO TO 1010-SIP.                                          EL511
00597                                                                   EL511
00598      MOVE SPACES                 TO WS-DETAIL1.                   EL511
00599      MOVE INPUT-RECORD           TO D1-CARD-IMAGE.                EL511
00600                                                                   EL511
00601      IF INP-TRAN-2 = 'W'                                          EL511
00602          MOVE INPUT-RECORD       TO W-CARD                        EL511
00603          MOVE SPACES             TO I-CARD                        EL511
00604          MOVE W-STATE-NO         TO I-STATE-NO                    EL511
00605          MOVE W-ACCT-NO          TO I-ACCT-NO                     EL511
00606          MOVE W-CERT-NO          TO I-CERT-NO                     EL511
00607          MOVE W-NAME             TO I-NAME                        EL511
00608          MOVE W-INIT             TO I-INIT                        EL511
00609          MOVE W-SEX              TO I-SEX                         EL511
00610          MOVE W-COMP-NO          TO I-COMP-NO                     EL511
00611          MOVE W-AGEX             TO I-AGEX                        EL511
00612          MOVE W-EFFECTIVE-DT     TO I-EFFECTIVE-DT                EL511
00613          MOVE W-LIFE-FIELDS      TO I-LIFE-FIELDS                 EL511
00614          MOVE W-TERMX            TO I-TERMX                       EL511
00615          MOVE W-AH-FIELDS        TO I-AH-FIELDS                   EL511
00616          MOVE W-REIN-CODE        TO I-REIN-CODE                   EL511
00617          MOVE W-ENTRY-TYPE       TO I-ENTRY-TYPE                  EL511
00618          MOVE 'I'                TO I-TRANSACTION                 EL511
00619          MOVE I-CARD             TO INPUT-RECORD.                 EL511
00620                                                                   EL511
00621      IF INP-TRAN-2 = 'I' OR 'A' OR 'R' OR 'L' OR 'Y'                 CL**4
00622         NEXT SENTENCE                                             EL511
00623        ELSE                                                       EL511
00624           MOVE 'INVALID TRAN CODE' TO D1-MESSAGE                  EL511
00625           MOVE WS-DETAIL1          TO P-DATA                      EL511
00626           MOVE SPACE               TO X                           EL511
00627           ADD +1 TO WS-LINE-CNT                                   EL511
00628           PERFORM 4000-PRINT THRU 4999-EXIT                       EL511
00629           GO TO 1010-SIP.                                         EL511
00630                                                                   EL511
00631      IF DTE-CLIENT = 'MON'                                           CL**6
00632       IF INP-TRAN-2 = 'R' AND                                        CL**6
00633         FIRST-R-RECORD                                               CL**6
00634           MOVE 'N'                 TO FIRST-R-RECORD-SW              CL**6
00635           MOVE INPUT-RECORD        TO R-CARD                         CL**6
00636           MOVE R-LF-PREM           TO WS-LF-ISSUE-AMT                CL**6
00637           MOVE R-LF-PREM-CAN       TO WS-LF-CANCEL-AMT               CL**6
00638           MOVE R-AH-PREM           TO WS-AH-ISSUE-AMT                CL**6
00639           MOVE R-AH-PREM-CAN       TO WS-AH-CANCEL-AMT               CL**6
00640           MOVE R-CERT-COUNT        TO WS-ISSUE-CNT                   CL**6
00641           MOVE R-CANC-COUNT        TO WS-CANCEL-CNT.                 CL**6
00642                                                                      CL**6
00643      IF INP-TRAN-2 NOT = 'R'                                      EL511
00644          NEXT SENTENCE                                            EL511
00645        ELSE                                                       EL511
00646          IF DTE-CLIENT = 'MON'                                       CL**6
00647             MOVE INPUT-RECORD        TO R-CARD                       CL**6
00648             MOVE R-LF-PREM           TO WS-HOLD-LF-ISSUE-AMT         CL**6
00649             MOVE R-LF-PREM-CAN       TO WS-HOLD-LF-CANCEL-AMT        CL**6
00650             MOVE R-AH-PREM           TO WS-HOLD-AH-ISSUE-AMT         CL**6
00651             MOVE R-AH-PREM-CAN       TO WS-HOLD-AH-CANCEL-AMT        CL**6
00652             MOVE R-CERT-COUNT        TO WS-HOLD-ISSUE-CNT            CL**6
00653             MOVE R-CANC-COUNT        TO WS-HOLD-CANCEL-CNT           CL**6
00654             MOVE 'REMITTANCE USED'   TO D1-MESSAGE                   CL**6
00655             MOVE WS-DETAIL1          TO P-DATA                       CL**6
00656             MOVE SPACE               TO X                            CL**6
00657             ADD +1 TO WS-LINE-CNT                                    CL**6
00658             PERFORM 4000-PRINT THRU 4999-EXIT                        CL**6
00659             GO TO 1010-SIP                                           CL**6
00660           ELSE                                                       CL**6
00661             MOVE 'REMITTANCE BYPASSED' TO D1-MESSAGE                 CL**6
00662             MOVE WS-DETAIL1          TO P-DATA                       CL**6
00663             MOVE SPACE               TO X                            CL**6
00664             ADD +1 TO WS-LINE-CNT                                    CL**6
00665             PERFORM 4000-PRINT THRU 4999-EXIT                        CL**6
00666             GO TO 1010-SIP.                                          CL**6
00667                                                                   EL511
00668      IF INP-TRAN-1 = '7'  AND                                     EL511
00669        (INP-TRAN-2 = 'A' OR 'Y')                                     CL**4
00670          PERFORM 1800-CLAIM-OUT THRU 1899-EXIT                    EL511
00671            GO TO 1010-SIP.                                        EL511
00672                                                                   EL511
00673      MOVE INP-CARR               TO WSC-CARR.                     EL511
00674      MOVE INP-GROUP              TO WSC-GROUP.                    EL511
00675      MOVE INP-ST                 TO WSC-ST.                       EL511
00676      MOVE INP-ACCOUNT            TO WSC-ACCOUNT.                  EL511
00677      MOVE INP-EFF-DT             TO WSC-EFF-DT.                   EL511
00678      MOVE INP-CERT               TO WSC-CERT.                     EL511
00679                                                                   EL511
00680      IF DTE-COMP-VG = '1'                                         EL511
00681          NEXT SENTENCE                                            EL511
00682       ELSE                                                        EL511
00683          MOVE SPACES             TO WSC-GROUP                     EL511
00684          IF DTE-COMP-VG = SPACE                                   EL511
00685              MOVE SPACE          TO WSC-CARR                      EL511
00686           ELSE                                                    EL511
00687              IF DTE-COMP-VG = '3'                                 EL511
00688                 MOVE SPACE       TO WSC-CARR                      EL511
00689                                     WSC-ST                        EL511
00690               ELSE                                                EL511
00691                  IF DTE-COMP-VG = '4'                             EL511
00692                     MOVE SPACE   TO WSC-ST.                       EL511
00693                                                                   EL511
00694      IF LCP-ONCTR-01 =  0                                            CL**9
00695          ADD 1 TO LCP-ONCTR-01                                       CL**9
00696         MOVE WS-CURRENT-KEY      TO WS-PRIOR-KEY.                 EL511
00697                                                                   EL511
00698      IF WSC-CARR     NOT = WSP-CARR   OR                          EL511
00699         WSC-ST       NOT = WSP-ST     OR                          EL511
00700         WSC-GROUP    NOT = WSP-GROUP  OR                          EL511
00701         WSC-ACCOUNT  NOT = WSP-ACCOUNT                            EL511
00702            PERFORM 1200-ISSUE-OUT  THRU 1499-EXIT                 EL511
00703            PERFORM 1500-CANCEL-OUT THRU 1799-EXIT                 EL511
00704            MOVE SPACES           TO PB1-CANCEL-RECORD             EL511
00705            MOVE SPACES           TO OLD-RECORD-1                  EL511
00706            PERFORM 3000-FORMAT-REMITTANCE.                        EL511
00707                                                                   EL511
00708  1020-REC-OUT.                                                    EL511
00709      IF WS-CURRENT-KEY NOT = WS-PRIOR-KEY  OR                     EL511
00710         WS-FIRST-TIME-SW = '9'                                    EL511
00711            PERFORM 1200-ISSUE-OUT  THRU 1499-EXIT                 EL511
00712            PERFORM 1500-CANCEL-OUT THRU 1799-EXIT                 EL511
00713            MOVE SPACES           TO PB1-CANCEL-RECORD             EL511
00714                                     OLD-RECORD-1                  EL511
00715                                     OLD-RECORD-2                  EL511
00716                                     OLD-RECORD-3                  EL511
00717                                     OLD-RECORD-4                  EL511
00718                                     OLD-RECORD-5                  EL511
00719            MOVE WS-CURRENT-KEY   TO WS-PRIOR-KEY.                 EL511
00720                                                                   EL511
00721      IF WS-FIRST-TIME-SW = '9'                                    EL511
00722         PERFORM 3000-FORMAT-REMITTANCE                            EL511
00723         GO TO 1999-EXIT.                                          EL511
00724                                                                   EL511
00725      IF INP-TRAN-1 = '6' AND                                      EL511
00726         INP-TRAN-2 = 'A'                                          EL511
00727          MOVE 'INPUT ACCEPTED'   TO D1-MESSAGE                    EL511
00728          MOVE WS-DETAIL1         TO P-DATA                        EL511
00729          MOVE SPACE              TO X                             EL511
00730          ADD +1 TO WS-LINE-CNT                                    EL511
00731          PERFORM 4000-PRINT THRU 4999-EXIT                        EL511
00732          MOVE INPUT-RECORD       TO PB1-CANCEL-RECORD             EL511
00733            GO TO 1010-SIP.                                        EL511
00734                                                                   EL511
00735      IF INP-TRAN-2 = 'I'                                          EL511
00736          MOVE 'INPUT ACCEPTED'   TO D1-MESSAGE                    EL511
00737          MOVE WS-DETAIL1         TO P-DATA                        EL511
00738          MOVE SPACE              TO X                             EL511
00739          ADD +1 TO WS-LINE-CNT                                    EL511
00740          PERFORM 4000-PRINT THRU 4999-EXIT                        EL511
00741          MOVE INPUT-RECORD       TO OLD-RECORD-1                  EL511
00742          GO TO 1010-SIP.                                          EL511
00743                                                                   EL511
00744      IF INP-TRAN-2   = 'L'  AND                                   EL511
00745         INP-FORCE-CD = ' '                                        EL511
00746          MOVE 'INPUT ACCEPTED'   TO D1-MESSAGE                    EL511
00747          MOVE WS-DETAIL1         TO P-DATA                        EL511
00748          MOVE SPACE              TO X                             EL511
00749          ADD +1 TO WS-LINE-CNT                                    EL511
00750          PERFORM 4000-PRINT THRU 4999-EXIT                        EL511
00751          MOVE INPUT-RECORD       TO OLD-RECORD-2                  EL511
00752          GO TO 1010-SIP.                                          EL511
00753                                                                   EL511
00754      IF INP-TRAN-2   = 'L' AND                                    EL511
00755         INP-FORCE-CD = '2'                                        EL511
00756          MOVE 'INPUT ACCEPTED'   TO D1-MESSAGE                    EL511
00757          MOVE WS-DETAIL1         TO P-DATA                        EL511
00758          MOVE SPACE              TO X                             EL511
00759          ADD +1 TO WS-LINE-CNT                                    EL511
00760          PERFORM 4000-PRINT THRU 4999-EXIT                        EL511
00761          MOVE INPUT-RECORD       TO OLD-RECORD-2                  EL511
00762          GO TO 1010-SIP.                                          EL511
00763                                                                   EL511
00764      IF INP-TRAN-2   = 'L'  AND                                   EL511
00765         INP-FORCE-CD = '3'                                        EL511
00766          MOVE 'INPUT ACCEPTED'   TO D1-MESSAGE                    EL511
00767          MOVE WS-DETAIL1         TO P-DATA                        EL511
00768          MOVE SPACE              TO X                             EL511
00769          ADD +1 TO WS-LINE-CNT                                    EL511
00770          PERFORM 4000-PRINT THRU 4999-EXIT                        EL511
00771          MOVE INPUT-RECORD       TO OLD-RECORD-3                  EL511
00772          GO TO 1010-SIP.                                          EL511
00773                                                                   EL511
00774      IF INP-TRAN-2   = 'L'  AND                                   EL511
00775         INP-FORCE-CD = '4'                                        EL511
00776          MOVE 'INPUT ACCEPTED'   TO D1-MESSAGE                    EL511
00777          MOVE WS-DETAIL1         TO P-DATA                        EL511
00778          MOVE SPACE              TO X                             EL511
00779          ADD +1 TO WS-LINE-CNT                                    EL511
00780          PERFORM 4000-PRINT THRU 4999-EXIT                        EL511
00781          MOVE INPUT-RECORD       TO OLD-RECORD-4                  EL511
00782          GO TO 1010-SIP.                                          EL511
00783                                                                   EL511
00784      IF INP-TRAN-2   = 'L'  AND                                   EL511
00785         INP-FORCE-CD = '5'                                        EL511
00786          MOVE 'INPUT ACCEPTED'   TO D1-MESSAGE                    EL511
00787          MOVE WS-DETAIL1         TO P-DATA                        EL511
00788          MOVE SPACE              TO X                             EL511
00789          ADD +1 TO WS-LINE-CNT                                    EL511
00790          PERFORM 4000-PRINT THRU 4999-EXIT                        EL511
00791          MOVE INPUT-RECORD       TO OLD-RECORD-5                  EL511
00792          GO TO 1010-SIP.                                          EL511
00793                                                                   EL511
00794      MOVE 'INVALID TRAN CODE'    TO D1-MESSAGE.                   EL511
00795      MOVE WS-DETAIL1             TO P-DATA.                       EL511
00796      MOVE SPACE-1                TO X.                            EL511
00797      PERFORM 4000-PRINT THRU 4999-EXIT.                           EL511
00798      GO TO 1010-SIP.                                              EL511
00799                                                                   EL511
00800  1200-ISSUE-OUT.                                                  EL511
00801      IF OLD-RECORD-1 = SPACES                                     EL511
00802          GO TO 1499-EXIT.                                         EL511
00803                                                                   EL511
00804      IF DTE-CLIENT NOT = 'MON'                                       CL**6
00805          ADD +1 TO WS-ISSUE-CNT.                                     CL**6
00806                                                                   EL511
00807      DISPLAY 'OLD I  = '  OLD-RECORD-1.                           EL511
00808      DISPLAY 'OLD L  = '  OLD-RECORD-2.                           EL511
00809      DISPLAY 'OLD I3 = '  OLD-RECORD-3.                           EL511
00810      DISPLAY 'OLD I4 = '  OLD-RECORD-4.                           EL511
00811      DISPLAY 'OLD I5 = '  OLD-RECORD-5.                           EL511
00812                                                                   EL511
00813  1225-ISSUE-SEQ-1.                                                EL511
00814      MOVE SPACES                  TO PBI-RECORD-BODY.             EL511
00815      INSPECT PB1-I-CERT-PRIME REPLACING ALL ' ' BY ZERO.             CL**9
00816      MOVE PB1-I-CERT-NO           TO WS-CERT-PRIME.               EL511
00817      MOVE WS-CERT-NO              TO PBI-CERT-NO.                 EL511
00818      MOVE PB1-I-CERT-EFF-DT       TO PBI-CERT-EFF-DT.             EL511
00819      MOVE '2'                     TO PBI-TRANS-TYPE.              EL511
00820      MOVE '1'                     TO PBI-SEQUENCE.                EL511
00821      MOVE PB1-I-INSURED-NAME      TO PBI-I-INS-LAST-NAME.         EL511
00822                                                                   EL511
00823      IF PB3-I-INSURED-LAST-NAME NOT = SPACES                      EL511
00824          MOVE PB3-I-INSURED-LAST-NAME TO PBI-I-INS-LAST-NAME.     EL511
00825                                                                   EL511
00826      MOVE PB1-I-1ST-INIT          TO PBI-I-INS-1ST-INIT.          EL511
00827      IF PB3-I-INSURED-FIRST-NAME NOT = SPACES                     EL511
00828          MOVE PB3-I-INSURED-FIRST-NAME TO PBI-I-INS-1ST-NAME.     EL511
00829                                                                   EL511
00830      MOVE PB1-I-MIDDLE-INIT       TO PBI-I-INS-MIDDLE-INIT.       EL511
00831      MOVE PB1-I-INSURED-AGE-X     TO PBI-I-INSURED-AGE-X.         EL511
00832      MOVE PB1-I-INSURED-SEX       TO PBI-I-INSURED-SEX.           EL511
00833      MOVE PB2-I-SOC-SEC-NO        TO PBI-I-SOC-SEC-NO.            EL511
00834                                                                   EL511
00835      IF PB3-I-INSURED-SOC-SEC-NO NOT = SPACES                     EL511
00836          MOVE PB3-I-INSURED-SOC-SEC-NO TO PBI-I-SOC-SEC-NO.       EL511
00837      MOVE PB2-I-MEMBER-NO         TO PBI-I-MEMBER-NO.             EL511
00838                                                                   EL511
00839      IF DTE-KEY-BIRTH = '1'                                       EL511
00840          MOVE PB2-I-BIRTHDAY-X    TO PBI-I-BIRTHDAY-X.            EL511
00841      IF PB1-I-FORCE-CD = 'R'                                      EL511
00842          MOVE 'R'                 TO PBI-I-ENTRY-CD.              EL511
00843      IF PB1-I-FORCE-CD = 'E'                                      EL511
00844          MOVE 'E'                 TO PBI-I-ENTRY-CD.              EL511
00845      IF PB1-I-FORCE-CD = 'V'                                         CL**8
00846          MOVE 'V'                 TO PBI-I-ENTRY-CD.                 CL**8
00847      IF PB1-I-FORCE-CD = 'D'                                         CL**8
00848          MOVE 'D'                 TO PBI-I-ENTRY-CD.                 CL**8
00849      IF PB1-I-FORCE-CD = 'U'                                         CL**8
00850          MOVE 'U'                 TO PBI-I-ENTRY-CD.                 CL**8
00851      IF PB1-I-FORCE-CD NOT = 'E' AND 'R'                          EL511
00852          MOVE PB1-I-FORCE-CD      TO PBI-I-FORCE-CD.              EL511
00853                                                                   EL511
00854      ADD +1                       TO WS-SEQ-NO.                   EL511
00855      MOVE WS-SEQ-NO               TO SR-REC-SEQ-NO.               EL511
00856      MOVE WS-BAT-NO               TO SR-BAT-SEQ-NO.               EL511
00857      MOVE PBI-ISSUE-RECORD        TO SR-PENDING-RECORD.           EL511
00858      RELEASE SORT-RECORD.                                         EL511
00859      ADD +1 TO WS-NO-RECORDS-RELEASED.                            EL511
00860                                                                   EL511
00861  1250-ISSUE-SEQ-2.                                                EL511
00862      MOVE SPACES                  TO PBI-ISSUE-RECORD.            EL511
00863      IF DTE-JT-AGE = '1'                                          EL511
00864          MOVE PB2-I-SPOUSE-ISSUE-AGE-X TO PBI-I-JOINT-AGE-X.      EL511
00865      IF DTE-CLIENT = 'XXX' OR 'LGX'                               EL511
00866          MOVE PB2-I-MEMBER-NO     TO PBI-I-JNT-LAST-NAME.         EL511
00867                                                                   EL511
00868      IF PBI-ISSUE-RECORD = SPACES                                 EL511
00869          GO TO 1275-ISSUE-SEQ-3.                                  EL511
00870                                                                   EL511
00871      ADD +1                       TO WS-SEQ-NO.                   EL511
00872      INSPECT PB1-I-CERT-PRIME REPLACING ALL ' ' BY ZERO.             CL**9
00873      MOVE PB1-I-CERT-NO           TO WS-CERT-PRIME.               EL511
00874      MOVE WS-CERT-NO              TO PBI-CERT-NO.                 EL511
00875      MOVE PB1-I-CERT-EFF-DT       TO PBI-CERT-EFF-DT.             EL511
00876      MOVE '2'                     TO PBI-TRANS-TYPE.              EL511
00877      MOVE '2'                     TO PBI-SEQUENCE.                EL511
00878      MOVE WS-SEQ-NO               TO SR-REC-SEQ-NO.               EL511
00879      MOVE WS-BAT-NO               TO SR-BAT-SEQ-NO.               EL511
00880      MOVE PBI-ISSUE-RECORD        TO SR-PENDING-RECORD.           EL511
00881                                                                   EL511
00882      RELEASE SORT-RECORD.                                         EL511
00883      ADD +1 TO WS-NO-RECORDS-RELEASED.                            EL511
00884                                                                   EL511
00885  1275-ISSUE-SEQ-3.                                                EL511
00886      MOVE SPACES                  TO PBI-ISSUE-RECORD.            EL511
00887      MOVE PB1-I-LF-BENEFIT-TYPE   TO PBI-I-LF-BENEFIT-TYPE.       EL511
00888      INSPECT PB1-I-LF-BENEFIT-AMT-X REPLACING ALL ' ' BY ZERO.       CL**9
00889                                                                   EL511
00890      IF PB1-I-LF-BENEFIT-AMT NOT NUMERIC                          EL511
00891           MOVE PB1-I-LF-BENEFIT-AMT-X TO PB1-I-LF-BENEFIT-AMT-X   EL511
00892        ELSE                                                       EL511
00893           MOVE PB1-I-LF-BENEFIT-AMT  TO PBI-I-LF-BENEFIT-AMT.     EL511
00894                                                                   EL511
00895      INSPECT PB1-I-LF-PREM-AMT-X REPLACING ALL ' ' BY ZERO.          CL**9
00896      IF PB1-I-LF-PREM-AMT NOT NUMERIC                             EL511
00897           MOVE PB1-I-LF-PREM-AMT-X TO PB1-I-LF-PREM-AMT-X         EL511
00898        ELSE                                                       EL511
00899           MOVE PB1-I-LF-PREM-AMT  TO PBI-I-LF-PREM-AMT.           EL511
00900                                                                   EL511
00901      INSPECT PB1-I-ORIG-TERM-X REPLACING ALL ' ' BY ZERO.            CL**9
00902      IF PB1-I-LF-BENEFIT-TYPE NOT = SPACES AND ZEROS              EL511
00903          MOVE PB1-I-ORIG-TERM-X   TO PBI-I-LF-TERM-X.             EL511
00904                                                                   EL511
00905      IF PBI-ISSUE-RECORD = SPACES                                 EL511
00906          GO TO 1300-ISSUE-SEQ-4.                                  EL511
00907                                                                   EL511
00908      IF DTE-CLIENT NOT = 'MON'                                       CL**6
00909       IF PB1-I-LF-PREM-AMT NUMERIC                                   CL**6
00910          ADD PB1-I-LF-PREM-AMT    TO WS-LF-ISSUE-AMT.             EL511
00911                                                                   EL511
00912      ADD +1                       TO WS-SEQ-NO.                   EL511
00913      INSPECT PB1-I-CERT-PRIME REPLACING ALL ' ' BY ZERO.             CL**9
00914      MOVE PB1-I-CERT-NO           TO WS-CERT-PRIME.               EL511
00915      MOVE WS-CERT-NO              TO PBI-CERT-NO.                 EL511
00916      MOVE PB1-I-CERT-EFF-DT       TO PBI-CERT-EFF-DT.             EL511
00917      MOVE '2'                     TO PBI-TRANS-TYPE.              EL511
00918      MOVE '3'                     TO PBI-SEQUENCE.                EL511
00919      MOVE WS-SEQ-NO               TO SR-REC-SEQ-NO.               EL511
00920      MOVE WS-BAT-NO               TO SR-BAT-SEQ-NO.               EL511
00921      MOVE PBI-ISSUE-RECORD        TO SR-PENDING-RECORD.           EL511
00922                                                                   EL511
00923      RELEASE SORT-RECORD.                                         EL511
00924      ADD +1 TO WS-NO-RECORDS-RELEASED.                            EL511
00925                                                                   EL511
00926  1300-ISSUE-SEQ-4.                                                EL511
00927      MOVE SPACES                  TO PBI-ISSUE-RECORD.            EL511
00928                                                                      CL**6
00929      IF DTE-CLIENT = 'MON'                                           CL**6
00930        MOVE PB1-I-AH-BENEFIT-TYPE TO PBI-I-AH-BENEFIT-TYPE           CL**7
00931       ELSE                                                           CL**6
00932        MOVE PB1-I-AH-BENEFIT-TYPE TO PBI-I-AH-BENEFIT-NO.            CL**7
00933                                                                   EL511
00934      INSPECT PB1-I-AH-BENEFIT-AMT-X REPLACING ALL ' ' BY ZERO.       CL**9
00935      IF PB1-I-AH-BENEFIT-AMT NOT NUMERIC                          EL511
00936           MOVE PB1-I-AH-BENEFIT-AMT-X TO PB1-I-AH-BENEFIT-AMT-X   EL511
00937        ELSE                                                       EL511
00938           MOVE PB1-I-AH-BENEFIT-AMT  TO PBI-I-AH-BENEFIT-AMT.     EL511
00939                                                                   EL511
00940      INSPECT PB1-I-AH-PREM-AMT-X REPLACING ALL ' ' BY ZERO.          CL**9
00941      IF PB1-I-AH-PREM-AMT NOT NUMERIC                             EL511
00942           MOVE PB1-I-AH-PREM-AMT-X TO PB1-I-AH-PREM-AMT-X         EL511
00943        ELSE                                                       EL511
00944           MOVE PB1-I-AH-PREM-AMT  TO PBI-I-AH-PREM-AMT.           EL511
00945                                                                   EL511
00946      INSPECT PB1-I-ORIG-TERM-X REPLACING ALL ' ' BY ZERO.            CL**9
00947      IF PB1-I-AH-BENEFIT-TYPE NOT = SPACES AND ZEROS              EL511
00948          MOVE PB1-I-ORIG-TERM-X   TO PBI-I-AH-TERM-X.             EL511
00949                                                                   EL511
00950      IF PBI-ISSUE-RECORD = SPACES                                 EL511
00951          GO TO 1325-ISSUE-SEQ-5.                                  EL511
00952                                                                   EL511
00953      IF DTE-CLIENT NOT = 'MON'                                       CL**6
00954       IF PB1-I-AH-PREM-AMT NUMERIC                                   CL**6
00955          ADD PB1-I-AH-PREM-AMT    TO WS-AH-ISSUE-AMT.             EL511
00956                                                                   EL511
00957      ADD +1                       TO WS-SEQ-NO.                   EL511
00958      INSPECT PB1-I-CERT-PRIME REPLACING ALL ' ' BY ZERO.             CL**9
00959      MOVE PB1-I-CERT-NO           TO WS-CERT-PRIME.               EL511
00960      MOVE WS-CERT-NO              TO PBI-CERT-NO.                 EL511
00961      MOVE PB1-I-CERT-EFF-DT       TO PBI-CERT-EFF-DT.             EL511
00962      MOVE '2'                     TO PBI-TRANS-TYPE.              EL511
00963      MOVE '4'                     TO PBI-SEQUENCE.                EL511
00964      MOVE WS-SEQ-NO               TO SR-REC-SEQ-NO.               EL511
00965      MOVE WS-BAT-NO               TO SR-BAT-SEQ-NO.               EL511
00966      MOVE PBI-ISSUE-RECORD        TO SR-PENDING-RECORD.           EL511
00967                                                                   EL511
00968      RELEASE SORT-RECORD.                                         EL511
00969      ADD +1 TO WS-NO-RECORDS-RELEASED.                            EL511
00970                                                                   EL511
00971  1325-ISSUE-SEQ-5.                                                EL511
00972      MOVE SPACES                  TO PBI-ISSUE-RECORD.            EL511
00973      MOVE PB2-I-LOAN-OFFICER      TO PBI-I-LOAN-OFFICER.          EL511
00974      MOVE PB2-I-LOAN-TERM-X       TO PBI-I-LOAN-TERM-X.           EL511
00975      MOVE PB2-I-INDV-GRP-CD       TO PBI-I-INDV-GRP-CD.           EL511
00976      MOVE PB1-I-REIN-CODE         TO PBI-I-REIN-CODE.             EL511
00977      MOVE PB2-I-SIG-SW            TO PBI-I-SIG-SW.                EL511
00978                                                                   EL511
00979      INSPECT PB2-I-LOAN-APR-X REPLACING ALL ' ' BY ZERO.             CL**9
00980      IF PB2-I-LOAN-APR-X  NOT NUMERIC                             EL511
00981           MOVE PB2-I-LOAN-APR-X   TO PBI-I-LOAN-APR-X             EL511
00982        ELSE                                                       EL511
00983           MOVE PB2-I-LOAN-APR     TO PBI-I-LOAN-APR.              EL511
00984                                                                   EL511
00985      MOVE PB2-I-PAY-FREQUENCY-X   TO PBI-I-PAY-FREQUENCY-X.       EL511
00986      MOVE PB2-I-RATE-CLASS        TO PBI-I-RATE-CLASS.            EL511
00987      MOVE PB2-I-SKIP-CODE         TO PBI-I-SKIP-CODE.             EL511
00988                                                                   EL511
00989      IF DTE-JT-AGE NOT = '1'                                      EL511
00990          IF PB2-I-EXTENTION-DAYS NUMERIC                          EL511
00991              MOVE PB2-I-EXTENTION-DAYS TO PBI-I-EXT-DAYS.         EL511
00992                                                                   EL511
00993      IF PBI-ISSUE-RECORD = SPACES                                 EL511
00994          GO TO 1350-ISSUE-SEQ-6.                                  EL511
00995                                                                   EL511
00996      ADD +1                       TO WS-SEQ-NO.                   EL511
00997      INSPECT PB1-I-CERT-PRIME REPLACING ALL ' ' BY ZERO.             CL**9
00998      MOVE PB1-I-CERT-NO           TO WS-CERT-PRIME.               EL511
00999      MOVE WS-CERT-NO              TO PBI-CERT-NO.                 EL511
01000      MOVE PB1-I-CERT-EFF-DT       TO PBI-CERT-EFF-DT.             EL511
01001      MOVE '2'                     TO PBI-TRANS-TYPE.              EL511
01002      MOVE '5'                     TO PBI-SEQUENCE.                EL511
01003      MOVE WS-SEQ-NO               TO SR-REC-SEQ-NO.               EL511
01004      MOVE WS-BAT-NO               TO SR-BAT-SEQ-NO.               EL511
01005      MOVE PBI-ISSUE-RECORD        TO SR-PENDING-RECORD.           EL511
01006                                                                   EL511
01007      RELEASE SORT-RECORD.                                         EL511
01008      ADD +1 TO WS-NO-RECORDS-RELEASED.                            EL511
01009                                                                   EL511
01010  1350-ISSUE-SEQ-6.                                                EL511
01011      MOVE SPACES                  TO PBI-ISSUE-RECORD.            EL511
01012      MOVE PB4-I-INSURED-ADDRESS-1 TO PBI-I-INSURED-ADDRESS-1.     EL511
01013      MOVE PB4-I-INSURED-ADDRESS-2 TO PBI-I-INSURED-ADDRESS-2.     EL511
01014                                                                   EL511
01015      IF PBI-ISSUE-RECORD = SPACES                                 EL511
01016          GO TO 1375-ISSUE-SEQ-7.                                  EL511
01017                                                                   EL511
01018      ADD +1                       TO WS-SEQ-NO.                   EL511
01019      INSPECT PB1-I-CERT-PRIME REPLACING ALL ' ' BY ZERO.             CL**9
01020      MOVE PB1-I-CERT-NO           TO WS-CERT-PRIME.               EL511
01021      INSPECT PB1-I-CERT-PRIME REPLACING ALL ' ' BY ZERO.             CL**9
01022      MOVE WS-CERT-NO              TO PBI-CERT-NO.                 EL511
01023      MOVE PB1-I-CERT-EFF-DT       TO PBI-CERT-EFF-DT.             EL511
01024      MOVE '2'                     TO PBI-TRANS-TYPE.              EL511
01025      MOVE '6'                     TO PBI-SEQUENCE.                EL511
01026      MOVE WS-SEQ-NO               TO SR-REC-SEQ-NO.               EL511
01027      MOVE WS-BAT-NO               TO SR-BAT-SEQ-NO.               EL511
01028      MOVE PBI-ISSUE-RECORD        TO SR-PENDING-RECORD.           EL511
01029                                                                   EL511
01030      RELEASE SORT-RECORD.                                         EL511
01031      ADD +1 TO WS-NO-RECORDS-RELEASED.                            EL511
01032                                                                   EL511
01033  1375-ISSUE-SEQ-7.                                                EL511
01034      MOVE SPACES                   TO PBI-ISSUE-RECORD.           EL511
01035      MOVE PB5-I-INSURED-CITY-STATE TO PBI-I-INSURED-CITY-STATE.   EL511
01036      MOVE PB5-I-INSURED-ZIP-CODE   TO PBI-I-INSURED-ZIP-CODE.     EL511
01037      MOVE PB5-I-INSURED-PHONE-NO   TO PBI-I-INSURED-PHONE-NO.     EL511
01038                                                                   EL511
01039      IF PBI-ISSUE-RECORD = SPACES                                 EL511
01040          GO TO 1499-EXIT.                                         EL511
01041                                                                   EL511
01042      ADD +1                       TO WS-SEQ-NO.                   EL511
01043      INSPECT PB1-I-CERT-PRIME REPLACING ALL ' ' BY ZERO.             CL**9
01044      MOVE PB1-I-CERT-NO           TO WS-CERT-PRIME.               EL511
01045      MOVE WS-CERT-NO              TO PBI-CERT-NO.                 EL511
01046      MOVE PB1-I-CERT-EFF-DT       TO PBI-CERT-EFF-DT.             EL511
01047      MOVE '2'                     TO PBI-TRANS-TYPE.              EL511
01048      MOVE '7'                     TO PBI-SEQUENCE.                EL511
01049      MOVE WS-SEQ-NO               TO SR-REC-SEQ-NO.               EL511
01050      MOVE WS-BAT-NO               TO SR-BAT-SEQ-NO.               EL511
01051      MOVE PBI-ISSUE-RECORD        TO SR-PENDING-RECORD.           EL511
01052                                                                   EL511
01053      RELEASE SORT-RECORD.                                         EL511
01054      ADD +1 TO WS-NO-RECORDS-RELEASED.                            EL511
01055                                                                   EL511
01056  1499-EXIT.                                                       EL511
01057      EXIT.                                                        EL511
01058      EJECT                                                        EL511
01059                                                                   EL511
01060  1500-CANCEL-OUT.                                                 EL511
01061      IF PB1-CANCEL-RECORD = SPACES                                EL511
01062           GO TO 1799-EXIT.                                        EL511
01063                                                                   EL511
01064 *    MOVE WSP-CARR                TO SR-CARRIER.                  EL511
01065 *    MOVE WSP-GROUP               TO SR-GROUPING.                 EL511
01066 *    MOVE WSP-ST                  TO SR-STATE.                    EL511
01067 *    MOVE WSP-ACCOUNT             TO SR-ACCOUNT.                  EL511
01068                                                                   EL511
01069      MOVE SPACES                  TO PBI-CANCEL-RECORD.           EL511
01070      INSPECT PB1-C-AH-PREM-REFUND-X REPLACING ALL ' ' BY ZERO.       CL**9
01071      INSPECT PB1-C-LF-PREM-REFUND-X REPLACING ALL ' ' BY ZERO.       CL**9
01072                                                                   EL511
01073      IF PB1-C-AH-PREM-REFUND NOT NUMERIC                          EL511
01074          MOVE ZERO                TO PB1-C-AH-PREM-REFUND.        EL511
01075      IF PB1-C-LF-PREM-REFUND NOT NUMERIC                          EL511
01076          MOVE ZERO                TO PB1-C-LF-PREM-REFUND.        EL511
01077                                                                   EL511
01078      MOVE PB1-C-CANCEL-DATE-X     TO PBI-C-AH-CANCEL-DATE-X.      EL511
01079      MOVE PB1-C-CANCEL-DATE-X     TO PBI-C-LF-CANCEL-DATE-X.      EL511
01080      MOVE PB1-C-AH-PREM-REFUND    TO PBI-C-AH-PREM-REFUND.        EL511
01081      MOVE PB1-C-LF-PREM-REFUND    TO PBI-C-LF-PREM-REFUND.        EL511
01082                                                                   EL511
01083      IF DTE-CLIENT NOT = 'MON'                                       CL**6
01084          ADD PBI-C-AH-PREM-REFUND  TO WS-AH-CANCEL-AMT               CL**6
01085          ADD PBI-C-LF-PREM-REFUND  TO WS-LF-CANCEL-AMT.              CL**6
01086                                                                   EL511
01087      IF PB1-C-LF-PREM-REFUND = ZERO                               EL511
01088          MOVE ZERO                TO PBI-C-LF-CANCEL-DATE-X.         CL*10
01089      IF PB1-C-AH-PREM-REFUND = ZERO                               EL511
01090          MOVE ZERO                TO PBI-C-AH-CANCEL-DATE-X.         CL*10
01091                                                                      CL**6
01092      IF DTE-CLIENT NOT = 'MON'                                       CL**6
01093          ADD +1 TO WS-CANCEL-CNT.                                    CL**6
01094                                                                      CL**6
01095      MOVE PB1-C-INSURED-NAME      TO PBI-C-INSURED-NAME.          EL511
01096      MOVE PB1-C-FORCE-CD          TO PBI-C-FORCE-CD.              EL511
01097                                                                   EL511
01098      ADD +1                       TO WS-SEQ-NO.                   EL511
01099      INSPECT PB1-C-CERT-PRIME REPLACING ALL ' ' BY ZERO.             CL**9
01100      MOVE PB1-C-CERT-NO           TO WS-CERT-PRIME.               EL511
01101      MOVE WS-CERT-NO              TO PBI-CERT-NO.                 EL511
01102      MOVE PB1-C-CERT-EFF-DT       TO PBI-CERT-EFF-DT.             EL511
01103      MOVE '3'                     TO PBI-TRANS-TYPE.              EL511
01104      MOVE '1'                     TO PBI-SEQUENCE.                EL511
01105      MOVE WS-SEQ-NO               TO SR-REC-SEQ-NO.               EL511
01106      MOVE WS-BAT-NO               TO SR-BAT-SEQ-NO.               EL511
01107      MOVE PBI-ISSUE-RECORD        TO SR-PENDING-RECORD.           EL511
01108                                                                   EL511
01109      RELEASE SORT-RECORD.                                         EL511
01110      ADD +1 TO WS-NO-RECORDS-RELEASED.                            EL511
01111                                                                   EL511
01112  1799-EXIT.                                                       EL511
01113      EXIT.                                                        EL511
01114      EJECT                                                        EL511
01115                                                                   EL511
01116  1800-CLAIM-OUT.                                                  EL511
01117      MOVE 'INPUT ACCEPTED'        TO D1-MESSAGE.                     CL**4
01118      MOVE INPUT-RECORD            TO D1-CARD-IMAGE.                  CL**4
01119      MOVE WS-DETAIL1              TO P-DATA.                         CL**4
01120      MOVE SPACE                   TO X.                              CL**4
01121      ADD +1                       TO WS-LINE-CNT                     CL**4
01122      PERFORM 4000-PRINT   THRU 4999-EXIT.                            CL**4
01123      MOVE INPUT-RECORD            TO CLAIM-REC-LAYOUT.               CL**4
01124                                                                      CL**4
01125      MOVE SPACES                  TO PENDING-CLAIMS-IN.           EL511
01126      MOVE PC1-STATE               TO PCI-STATE.                   EL511
01127      MOVE PC1-ACCOUNT-NO          TO WS-ACCOUNT-PRIME.            EL511
01128                                                                      CL**5
01129      IF DTE-CLIENT = 'FLA'                                           CL**5
01130          MOVE ZEROS               TO WS-ACCOUNT-ZERO                 CL**5
01131          IF WS-ACCOUNT = '000033901W'                                CL**5
01132              MOVE '000339013A'    TO WS-ACCOUNT.                     CL**5
01133                                                                      CL**5
01134      MOVE WS-ACCOUNT              TO PCI-ACCOUNT-NO.              EL511
01135      INSPECT PCI-ACCOUNT-NO REPLACING ALL ' ' BY ZERO.               CL**9
01136                                                                   EL511
01137      MOVE PC1-CARR                TO PCI-CARRIER.                 EL511
01138                                                                      CL**4
01139      IF DTE-CLIENT = 'FLA'                                           CL**4
01140          MOVE PC1-GROUP           TO PCI-CAUSE                       CL**4
01141          MOVE ZEROS               TO PC1-GROUP.                      CL**4
01142                                                                      CL**4
01143      MOVE PC1-GROUP               TO WS-GROUP-PRIME.              EL511
01144      MOVE WS-GROUP                TO PCI-GROUPING.                EL511
01145      INSPECT PCI-GROUPING REPLACING ALL ' ' BY ZERO.                 CL**9
01146                                                                   EL511
01147      MOVE PC1-FORCE-CD            TO PCI-FORCE-CD.                EL511
01148      MOVE PC1-CERT-NO             TO WS-CERT-PRIME.               EL511
01149      MOVE WS-CERT-NO              TO PCI-CERT-NO.                 EL511
01150      INSPECT PCI-CERT-PREFIX REPLACING ALL ' ' BY ZERO.              CL**9
01151                                                                   EL511
01152      MOVE PC1-EFF-DATE            TO PCI-EFF-DATE.                EL511
01153      MOVE PC1-CODE                TO PCI-CODE.                    EL511
01154                                                                   EL511
01155      IF RESERVE-INPUT                                             EL511
01156           GO TO 1850-RESERVE-OUT.                                 EL511
01157                                                                   EL511
01158      MOVE PC1-CHECK-NO            TO WS-CHECK-PRIME                  CL**4
01159      MOVE WS-CHECK-NO             TO PCI-CHECK-NO.                   CL**4
01160                                                                      CL**4
01161      MOVE PC1-NO                  TO PCI-CLAIM-NO.                EL511
01162      MOVE PC1-TYPE-PMT            TO PCI-TYPE-PMT.                EL511
01163      IF PCI-TYPE-PMT = 'C'                                           CL**4
01164          MOVE 'A'                 TO PCI-TYPE-PMT.                   CL**4
01165                                                                      CL**4
01166      IF PCI-TYPE-PMT = 'E'                                           CL**4
01167          MOVE 'C'                 TO PCI-TYPE-PMT.                   CL**4
01168                                                                      CL**4
01169      IF PCI-TYPE-PMT = 'S'                                           CL**4
01170          MOVE 'L'                 TO PCI-TYPE-PMT.                   CL**4
01171                                                                      CL**4
01172      IF PCI-TYPE-PMT = 'X'                                           CL**4
01173          MOVE 'A'                 TO PCI-TYPE-PMT.                   CL**4
01174                                                                      CL**4
01175      INSPECT PC1-AMT REPLACING ALL ' ' BY ZERO.                      CL**9
01176                                                                   EL511
01177      IF PC1-PAID-AMT NOT NUMERIC                                  EL511
01178          MOVE ZERO                TO PC1-PAID-AMT.                EL511
01179      MOVE PC1-PAID-AMT            TO PCI-PAID-AMT.                EL511
01180                                                                      CL**4
01181      IF PC1-DAYS-DISAB NUMERIC                                       CL**4
01182          MOVE PC1-DAYS-DISAB      TO PCI-DAYS-DISAB                  CL**4
01183          ELSE                                                        CL**4
01184          MOVE ZEROS               TO PCI-DAYS-DISAB.                 CL**4
01185                                                                      CL**4
01186      MOVE PC1-AGE-DTH             TO PCI-AGE-DTH.                 EL511
01187                                                                      CL**5
01188      IF DTE-CLIENT NOT = 'FLA'                                       CL**5
01189          MOVE PC1-CAUSE           TO PCI-CAUSE.                      CL**5
01190                                                                      CL**5
01191      MOVE '4'                     TO PCI-RECORD-TYPE.             EL511
01192      MOVE '1'                     TO PCI-RECORD-SEQUENCE.         EL511
01193                                                                   EL511
01194      MOVE SPACES                  TO WS-DETAIL1.                  EL511
01195      MOVE PENDING-CLAIMS-IN       TO D1-CARD-IMAGE.               EL511
01196      MOVE 'CLAIM-OUT SEQ 1'       TO D1-MESSAGE.                  EL511
01197      MOVE WS-DETAIL1              TO P-DATA.                      EL511
01198      MOVE SPACE-1                 TO X.                           EL511
01199      ADD +1                       TO WS-LINE-CNT.                    CL**4
01200      PERFORM 4000-PRINT THRU 4999-EXIT.                           EL511
01201      WRITE CLAIM-RECORD FROM PENDING-CLAIMS-IN.                   EL511
01202      ADD +1                       TO WS-OUTPUT-CLAIMS.               CL**4
01203                                                                   EL511
01204      MOVE PC1-INC-DATE            TO PCI-INC-DATE.                EL511
01205      MOVE PC1-RPT-DATE            TO PCI-RPT-DATE.                EL511
01206      MOVE PC1-PAID-DATE           TO PCI-PAID-DATE.               EL511
01207      MOVE PC1-PD-THRU-DATE        TO PCI-PD-THRU-DATE.            EL511
01208      MOVE '4'                     TO PCI-RECORD-TYPE.             EL511
01209      MOVE '2'                     TO PCI-RECORD-SEQUENCE.         EL511
01210                                                                   EL511
01211      MOVE SPACES                  TO WS-DETAIL1.                  EL511
01212      MOVE PENDING-CLAIMS-IN       TO D1-CARD-IMAGE.               EL511
01213      MOVE 'CLAIM-OUT SEQ 2'       TO D1-MESSAGE.                  EL511
01214      MOVE WS-DETAIL1              TO P-DATA.                      EL511
01215      MOVE SPACE-1                 TO X.                           EL511
01216      ADD +1                       TO WS-LINE-CNT.                    CL**4
01217      PERFORM 4000-PRINT THRU 4999-EXIT.                           EL511
01218      WRITE CLAIM-RECORD FROM PENDING-CLAIMS-IN.                   EL511
01219                                                                   EL511
01220      GO TO 1899-EXIT.                                             EL511
01221                                                                   EL511
01222  1850-RESERVE-OUT.                                                EL511
01223      INSPECT PC1-FUTURE-X REPLACING ALL ' ' BY ZERO.                 CL**9
01224      INSPECT PC1-IBNR-X REPLACING ALL ' ' BY ZERO.                   CL**9
01225      INSPECT PC1-PTC-X REPLACING ALL ' ' BY ZERO.                    CL**9
01226      IF PC1-FUTURE NOT NUMERIC                                    EL511
01227          MOVE ZERO                TO PC1-FUTURE.                  EL511
01228      IF PC1-IBNR   NOT NUMERIC                                    EL511
01229          MOVE ZERO                TO PC1-IBNR.                    EL511
01230      IF PC1-PTC    NOT NUMERIC                                    EL511
01231          MOVE ZERO                TO PC1-PTC.                     EL511
01232                                                                   EL511
01233      MOVE PC1-NO-R                TO PCI-CLAIM-NO.                EL511
01234      MOVE PC1-PTC                 TO PCI-PTC.                     EL511
01235      MOVE PC1-IBNR                TO PCI-IBNR.                    EL511
01236      MOVE PC1-FUTURE              TO PCI-FUTURE.                  EL511
01237      MOVE PC1-R-INC-DATE          TO PCI-INC-DATE-R.                 CL**4
01238      MOVE PC1-CODE                TO PCI-RESERVE-CODE.               CL**4
01239      MOVE '5'                     TO PCI-RECORD-TYPE.             EL511
01240      MOVE '1'                     TO PCI-RECORD-SEQUENCE.         EL511
01241                                                                   EL511
01242      MOVE SPACES                  TO WS-DETAIL1.                  EL511
01243      MOVE PENDING-CLAIMS-IN       TO D1-CARD-IMAGE.               EL511
01244      MOVE 'RESERVE-OUT SEQ 1'     TO D1-MESSAGE.                  EL511
01245      MOVE WS-DETAIL1              TO P-DATA.                      EL511
01246      MOVE SPACE-1                 TO X.                           EL511
01247      ADD +1                       TO WS-LINE-CNT.                    CL**4
01248      PERFORM 4000-PRINT THRU 4999-EXIT.                           EL511
01249      WRITE CLAIM-RECORD FROM PENDING-CLAIMS-IN.                   EL511
01250                                                                   EL511
01251      ADD +1                       TO WS-OUTPUT-RESERVES.             CL**4
01252  1899-EXIT.                                                       EL511
01253      EXIT.                                                        EL511
01254                                                                   EL511
01255  1999-EXIT.                                                       EL511
01256      EXIT.                                                        EL511
01257      EJECT                                                        EL511
01258                                                                   EL511
01259  2000-SORT-OUTPUT-PROCEDURE SECTION.                              EL511
01260      PERFORM 5000-HEADINGS THRU 5999-EXIT.                        EL511
01261                                                                   EL511
01262      IF WS-NO-RECORDS-RELEASED GREATER +0                         EL511
01263          NEXT SENTENCE                                            EL511
01264      ELSE                                                         EL511
01265          DISPLAY 'NO PENDING RECORDS RELEASED TO SORT'            EL511
01266          GO TO 2000-EXIT.                                         EL511
01267                                                                   EL511
01268  2010-CONT.                                                       EL511
01269      RETURN SORT-FILE                                             EL511
01270          AT END                                                   EL511
01271              GO TO 2000-EXIT.                                     EL511
01272                                                                   EL511
01273      IF WS-LINE-CNT GREATER +60                                   EL511
01274          PERFORM 5000-HEADINGS THRU 5999-EXIT.                    EL511
01275                                                                   EL511
01276      MOVE SR-PENDING-RECORD    TO  ISS-CAN-RECORD                 EL511
01277                                    D1-CARD-IMAGE.                 EL511
01278      MOVE SR-BAT-SEQ-NO        TO  D1-B-CNT.                      EL511
01279      MOVE SR-REC-SEQ-NO        TO  D1-R-CNT.                      EL511
01280                                                                   EL511
01281      IF BATCH-HEADER                                              EL511
01282          MOVE 'START OF BATCH' TO  D1-MESSAGE                     EL511
01283          MOVE SPACE-3          TO  X                              EL511
01284          ADD +3 TO WS-LINE-CNT                                    EL511
01285      ELSE                                                         EL511
01286          MOVE '              ' TO  D1-MESSAGE                     EL511
01287          ADD +1 TO WS-LINE-CNT                                    EL511
01288          MOVE SPACE-1          TO  X.                             EL511
01289                                                                   EL511
01290      MOVE WS-DETAIL1           TO  P-DATA.                        EL511
01291      PERFORM 4000-PRINT THRU 4999-EXIT.                           EL511
01292                                                                   EL511
01293      WRITE ISS-CAN-RECORD.                                        EL511
01294      ADD +1 TO WS-OUTPUT-RECORDS.                                 EL511
01295                                                                   EL511
01296      GO TO 2010-CONT.                                             EL511
01297                                                                   EL511
01298  2000-EXIT.                                                       EL511
01299      EXIT.                                                        EL511
01300      EJECT                                                        EL511
01301                                                                   EL511
01302  3000-FORMAT-REMITTANCE SECTION.                                  EL511
01303      MOVE SPACES               TO PBI-BATCH-RECORD.               EL511
01304      MOVE RUN-DATE             TO PBI-B-BATCH-DT.                 EL511
01305      MOVE DTE-CLIENT           TO PBI-CLIENT-ID.                  EL511
01306      MOVE '1'                  TO PBI-B-TRANS-TYPE.               EL511
01307      MOVE '0'                  TO PBI-B-SEQUENCE.                 EL511
01308                                                                   EL511
01309      MOVE WSP-CARR             TO PBI-CARRIER.                    EL511
01310      MOVE WSP-GROUP            TO WS-GROUP-PRIME.                 EL511
01311      MOVE WS-GROUP             TO PBI-GROUPING.                      CL**2
01312      MOVE WSP-ST               TO PBI-STATE.                      EL511
01313      MOVE WSP-ACCOUNT          TO WS-ACCOUNT-PRIME.               EL511
01314                                                                      CL**5
01315      IF DTE-CLIENT = 'FLA'                                           CL**5
01316          MOVE ZEROS            TO WS-ACCOUNT-ZERO                    CL**5
01317          IF WS-ACCOUNT = '000033901W'                                CL**5
01318              MOVE '000339013A' TO WS-ACCOUNT.                        CL**5
01319                                                                      CL**5
01320      MOVE WS-ACCOUNT           TO PBI-ACCOUNT.                    EL511
01321      INSPECT PBI-GROUPING REPLACING ALL ' ' BY ZERO.                 CL**9
01322      INSPECT PBI-ACCOUNT REPLACING ALL ' ' BY ZERO.                  CL**9
01323                                                                   EL511
01324      MOVE WS-CANCEL-CNT        TO PBI-B-CERT-CAN-COUNT.           EL511
01325      MOVE WS-ISSUE-CNT         TO PBI-B-CERT-ISS-COUNT.           EL511
01326      MOVE WS-LF-CANCEL-AMT     TO PBI-B-LF-PRM-CANCELLED.         EL511
01327      MOVE WS-LF-ISSUE-AMT      TO PBI-B-LF-PRM-WRITTEN.           EL511
01328      MOVE WS-AH-CANCEL-AMT     TO PBI-B-AH-PRM-CANCELLED.         EL511
01329      MOVE WS-AH-ISSUE-AMT      TO PBI-B-AH-PRM-WRITTEN.           EL511
01330                                                                   EL511
01331      MOVE PBI-BATCH-RECORD     TO SR-PENDING-RECORD.              EL511
01332                                                                   EL511
01333      IF DTE-CLIENT = 'MON'                                           CL**6
01334          MOVE WS-HOLD-LF-ISSUE-AMT   TO WS-LF-ISSUE-AMT              CL**6
01335          MOVE WS-HOLD-LF-CANCEL-AMT  TO WS-LF-CANCEL-AMT             CL**6
01336          MOVE WS-HOLD-AH-ISSUE-AMT   TO WS-AH-ISSUE-AMT              CL**6
01337          MOVE WS-HOLD-AH-CANCEL-AMT  TO WS-AH-CANCEL-AMT             CL**6
01338          MOVE WS-HOLD-ISSUE-CNT      TO WS-ISSUE-CNT                 CL**6
01339          MOVE WS-HOLD-CANCEL-CNT     TO WS-CANCEL-CNT.               CL**6
01340                                                                      CL**6
01341      MOVE +0                   TO SR-REC-SEQ-NO                   EL511
01342                                   WS-SEQ-NO.                         CL**6
01343      IF DTE-CLIENT NOT = 'MON'                                       CL**6
01344          MOVE +0               TO WS-CANCEL-CNT                      CL**6
01345                                   WS-ISSUE-CNT                    EL511
01346                                   WS-LF-ISSUE-AMT                 EL511
01347                                   WS-LF-CANCEL-AMT                EL511
01348                                   WS-AH-ISSUE-AMT                 EL511
01349                                   WS-AH-CANCEL-AMT.               EL511
01350                                                                   EL511
01351      MOVE WS-BAT-NO            TO SR-BAT-SEQ-NO.                  EL511
01352      ADD +1                    TO WS-NO-RECORDS-RELEASED          EL511
01353                                   WS-NO-BATCHES                   EL511
01354                                   WS-BAT-NO.                      EL511
01355      RELEASE SORT-RECORD.                                         EL511
01356                                                                   EL511
01357  3000-EXIT.                                                       EL511
01358      EXIT.                                                        EL511
01359      EJECT                                                        EL511
01360                                                                   EL511
01361  4000-PRINT    SECTION.                                           EL511
01362                               COPY ELCPRT2X.                         CL**9
01363  4999-EXIT.                                                       EL511
01364      EXIT.                                                        EL511
01365      EJECT                                                        EL511
01366                                                                   EL511
01367  5000-HEADINGS   SECTION.                                         EL511
01368      ADD +1                       TO WS-PAGE-CNT.                 EL511
01369      MOVE +5                      TO WS-LINE-CNT.                 EL511
01370      MOVE WS-CURRENT-DATE         TO WS-H2-DATE.                     CL**3
01371      MOVE COMPANY-NAME            TO WS-H2-CLIENT-NAME.           EL511
01372      MOVE ALPH-DATE               TO WS-H3-DATE.                  EL511
01373                                                                   EL511
01374      MOVE WS-PAGE-CNT             TO WS-H3-PAGE.                  EL511
01375      MOVE WS-HEADING1             TO P-DATA.                      EL511
01376      MOVE SPACE-NP                TO X.                           EL511
01377      PERFORM 4000-PRINT THRU 4999-EXIT.                           EL511
01378      MOVE WS-HEADING2             TO P-DATA.                      EL511
01379      MOVE SPACE-1                 TO X.                           EL511
01380      PERFORM 4000-PRINT THRU 4999-EXIT.                           EL511
01381      MOVE WS-HEADING3             TO P-DATA.                      EL511
01382      MOVE SPACE-1                 TO X.                           EL511
01383      PERFORM 4000-PRINT THRU 4999-EXIT.                           EL511
01384      MOVE WS-HEADING4             TO P-DATA.                      EL511
01385      MOVE SPACE-2                 TO X.                           EL511
01386      PERFORM 4000-PRINT THRU 4999-EXIT.                           EL511
01387                                                                   EL511
01388  5999-EXIT.                                                       EL511
01389      EXIT.                                                        EL511
01390                                                                   EL511
01391  ABEND-PGM       SECTION.                                         EL511
01392      COPY ELCABEND.                                                  CL*11
