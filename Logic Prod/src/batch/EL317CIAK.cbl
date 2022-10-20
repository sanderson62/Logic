00001  IDENTIFICATION DIVISION.                                         04/18/98
00002                                                                   EL317
00003  PROGRAM-ID.                 EL317CI.                                LV003
00004 *              PROGRAM CONVERTED BY                               EL317
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL317
00006 *              CONVERSION DATE 02/02/95 10:55:54.                 EL317
00007 *           PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE             EL317
00008 *                           VMOD=2.020                            EL317
00009                                                                   EL317
00009                                                                   EL317
00010 *AUTHOR.     LOGIC, INC.                                          EL317
00011 *            DALLAS, TEXAS.                                       EL317
00015 *SECURITY.   *****************************************************EL317
00016 *            *                                                   *EL317
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *EL317
00018 *            *                                                   *EL317
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL317
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL317
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *EL317
00022 *            *                                                   *EL317
00023 *            *****************************************************EL317
00012                                                                   EL317
00025 *REMARKS.                                                         EL317
00026 *        THIS PROGRAM PRINTS A REGISTER OF ALL CHECKS ISSUED      EL317
00027 *    IN THE REPORTED MONTH.   BOTH ONLINE CREATED AND OFFLINE     EL317
00028 *    CHECKS ARE REPORTED.                                         EL317
091808**********************************************************************
091808**************           N  O  T  E        ***************************
091808*****THIS PROGRAM IS A COPY OF EL317CI BUT DOES PROCESSING ONLY  *****
091808*****FOR ALASKA.  ANY CHANGES MADE TO THIS PROGRAM SHOULD ALSO BE*****
091808*****MADE TO EL317CI.                                            *****
091808**********************************************************************
00029                                                                   EL317
00030 *            PROGRAM                                              EL317
00031 *             OPTION    DESCRIPTION                               EL317
00032                                                                   EL317
00033 *               1       MONTHLY CHECK REGISTER                    EL317
00034 *               2       DAILY CHECK REGISTER                      EL317
092602******************************************************************
092602*                   C H A N G E   L O G
092602*
092602* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
092602*-----------------------------------------------------------------
092602*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
092602* EFFECTIVE    NUMBER
092602*-----------------------------------------------------------------
022106* 022106    2004040700004  PEMA  ADD LIFE CLAIM INTEREST
080307* 080307    2007032100001  PEMA  VOID BELOW MINIMUM PMTS
091808* 091808    2007022800002  AJRA  PROCESS ALASKA INTEREST CHECKS
051810* 051810 CR2010042900001   PEMA  SPEARATE CITY AND STATE
081110* 081110   2010010400003   AJRA  FIX VENDOR REC CITY STATE ZIP
072110* 072110    2009122800001  AJRA  ADD EXTRACT FOR NAPERSOFT
092010* 092010    2009122800001  AJRA  MISC NAPERSOFT CHANGES
032012* 032012    2011110200001  AJRA  ADD AHL CLAIM NO
032212* 032212    2011110200001  PEMA  AHL CHANGES
041712* 041712  IR2012041600002  AJRA  USE AHL IN T REC WHEN AHL
041812* 041812  IR2012041600004  AJRA  ZERO OUT BYPASSED PMT AMOUNT
111912* 111912  IR2012111900001  AJRA  FIX VENDOR REC ADDRESS 
092602******************************************************************

00037  ENVIRONMENT DIVISION.
00038  CONFIGURATION SECTION.

00040  INPUT-OUTPUT SECTION.

00042  FILE-CONTROL.

00044      SELECT REPORTS-EXTRACT-FILE
00045                             ASSIGN TO SYS010.

           SELECT ELACTQ          ASSIGN TO ELACTQ
                                  ORGANIZATION IS INDEXED
                                  ACCESS IS DYNAMIC
                                  RECORD KEY IS AQ-CONTROL-PRIMARY
                                  FILE STATUS IS ELACTQ-FILE-STATUS.

           SELECT ELMSTR          ASSIGN TO ELMSTR
                                  ORGANIZATION IS INDEXED
                                  ACCESS IS DYNAMIC
                                  RECORD KEY IS CL-CONTROL-PRIMARY
                                  FILE STATUS IS ELMSTR-FILE-STATUS.

           SELECT ELTRLR          ASSIGN TO ELTRLR
                                  ORGANIZATION IS INDEXED
                                  ACCESS IS DYNAMIC
                                  RECORD KEY IS AT-CONTROL-PRIMARY
                                  FILE STATUS IS ELTRLR-FILE-STATUS.


00047      SELECT DISK-DATE       ASSIGN TO SYS019.
00048      SELECT PRNTR           ASSIGN TO SYS008.
00049      SELECT FICH            ASSIGN TO SYS020.
00050      SELECT FREEDOM-INT-FILE
                                  ASSIGN TO SYS021
              ORGANIZATION IS LINE SEQUENTIAL.
072110
072110     SELECT NAPERSOFT-LETTERS ASSIGN TO SYS022
072110                              ORGANIZATION LINE SEQUENTIAL.

00062  DATA DIVISION.

00064  FILE SECTION.

00066  FD  REPORTS-EXTRACT-FILE        COPY ELCEXTFD.

00068                                  COPY ELCEXTR.

       FD  ELACTQ.
                                       COPY ELCACTQ.

       FD  ELMSTR.
                                       COPY ELCMSTR.

       FD  ELTRLR.
                                       COPY ELCTRLR.

00071  FD  FREEDOM-INT-FILE
00072      BLOCK CONTAINS 0 RECORDS
00073      RECORDING MODE F.

       01  FREEDOM-INT-RECORD          PIC X(400).

00101  FD  DISK-DATE                   COPY ELCDTEFD.

00104  FD  PRNTR                       COPY ELCPRTFD.

00107  FD  FICH                        COPY ELCFCHFD.
072110
072110 FD  NAPERSOFT-LETTERS
072110     LABEL RECORDS ARE STANDARD
072110     RECORDING MODE IS F
072110     BLOCK CONTAINS 0 RECORDS.
072110 01  NAPERSOFT-LETTER           PIC X(1500).

00114  WORKING-STORAGE SECTION.
00115  01  LCP-ABND-CODE               PIC S999 COMP VALUE +519.
00116  77  LCP-ONCTR-01                PIC S9(8) COMP-3 VALUE ZERO.
00117  77  LCP-ASA                     PIC X.

00119  77  FILLER  PIC X(32)   VALUE '********************************'.EL317
00120  77  FILLER  PIC X(32)   VALUE '*    EL317CI WORKING STORAGE   *'.EL317
00121  77  FILLER  PIC X(32)   VALUE '********* VMOD=2.001 ***********'.EL317
       77  ELACTQ-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
       77  ELMSTR-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
       77  ELTRLR-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
       77  WS-CURRENT-BIN-DT           PIC XX  VALUE LOW-VALUES.
072110 77  WS-PARM-BIN-DT              PIC XX  VALUE LOW-VALUES.
       77  WS-EOF-SW                   PIC X   VALUE SPACES.
           88  END-OF-INPUT                    VALUE 'Y'.

       77  WS-RECS-IN                  PIC 9(7) VALUE ZEROS.
       77  S1                          PIC S999 COMP-3 VALUE +0.
       77  WS-PMT-AMT                  PIC S9(7)V99 COMP-3 VALUE +0.
       77  WS-SKIP-CNT                 PIC 9(5) VALUE ZEROS.
       77  WS-VOID-SW                  PIC X  VALUE SPACES.
           88  PREV-VOID                      VALUE 'Y'.
       77  WS-PROCESS-PMT-SW           PIC X.
           88  PROCESS-PMT                VALUE 'Y'.
       77  WS-PROCESS-RERUN-SW         PIC X.
           88  PROCESS-RERUN              VALUE 'Y'.
       01  WS-TRANSACTION-RECORD       PIC X(400).
       01  WS-CHECK-DES-RECORD         PIC X(400).
       01  WS-DISTRIBUTION-RECORD      PIC X(400).
       01  WS-PAYEE-ADDRESS-RECORD     PIC X(400).
       01  WS-ALPHA-1099-RECORD        PIC X(400).
       01  WS-VOUCH-ADDR-1099-RECORD   PIC X(400).

       01  WS-PREV-KEY.
           05  WS-PREV-CARRIER         PIC X      VALUE LOW-VALUES.
           05  WS-PREV-CLAIM-NO        PIC X(7)   VALUE LOW-VALUES.
           05  WS-PREV-CERT-NO         PIC X(11)  VALUE LOW-VALUES.
       01  WS-COMP-KEY.
           05  WS-COMP-CARRIER         PIC X.
           05  WS-COMP-CLAIM-NO        PIC X(7).
           05  WS-COMP-CERT-NO         PIC X(11).
       01  WS-INVOICE-NO.
           05  WS-INVOICE-MM           PIC 99.
           05  WS-INVOICE-DD           PIC 99.
           05  WS-INVOICE-CLM-NO       PIC X(7).
           05  FILLER                  PIC XX    VALUE SPACES.
       01  WS-REC-GRP-CD.
           05  WS-REC-GRP-DA           PIC X.
           05  WS-REC-GRP-SEQ-NO       PIC 999    VALUE 000.
       01  WS-USER-DEFINED.
           05  WS-USER-CARRIER         PIC X.
           05  WS-USER-CLAIM-NO        PIC X(7).
           05  WS-USER-CERT-NO         PIC X(11).
           05  WS-USER-TRLR-SEQ-NO     PIC X(5).
00123  01  FILLER                          COMP-3.                      EL317
00124      05  WS-LINE-COUNT               PIC S9(3)       VALUE +99.   EL317
00125      05  WS-LINE-COUNT-MAX           PIC S9(3)       VALUE +60.   EL317
00126      05  WS-PAGE                     PIC S9(5)       VALUE ZERO.  EL317
00127      05  WS-REPORT-SW                PIC S9          VALUE ZERO.  EL317
00128      05  WS-HEADING-SW               PIC S9          VALUE ZERO.  EL317
00129      05  WS-PRINT-SW                 PIC S9          VALUE ZERO.  EL317
00130      05  WS-RECORD-COUNT             PIC S9(9)       VALUE ZERO.  EL317
00131      05  WS-RETURN-CODE              PIC S9(3)       VALUE ZERO.  EL317
00132      05  WS-ZERO                     PIC S9          VALUE ZERO.  EL317
00133                                                                   EL317
00134      05  WS-INCURRED-AGE             PIC S9(3)       VALUE ZERO.  EL317
00135      05  WS-YEAR REDEFINES                                        EL317
00136          WS-INCURRED-AGE             PIC S9(3).                   EL317
00137                                                                   EL317
00138      05  WS-AMOUNT                   PIC S9(7)V99 VALUE ZERO.     EL317

00141  01  FILLER                          COMP SYNC.                   EL317
00142      05  PGM-SUB                     PIC S9(4)       VALUE +317.  EL317
00143      05  WS-INDEX                    PIC S9(4)       VALUE ZERO.  EL317
00144                                                                   EL317
00145  01  FILLER.                                                      EL317
00146      05  ABEND-CODE                  PIC X(4).                    EL317
00147      05  ABEND-OPTION                PIC X.                       EL317
00148      05  OLC-REPORT-NAME             PIC X(5)      VALUE 'EL317'. EL317
00149      05  X                           PIC X           VALUE SPACE. EL317
00150                                                                   EL317
00151      05  WS-SAVE-PRINT-RECORD        PIC X(133)      VALUE SPACES.EL317
00152                                                                   EL317
00153      05  WS-LAST-CARRIER             PIC X           VALUE SPACES.EL317
00154                                                                   EL317
00155      05  WS-ABEND-MESSAGE            PIC X(80)       VALUE SPACES.EL317
00156                                                                   EL317
00157      05  WS-LAST-MONTH               PIC 99          VALUE ZERO.  EL317
00158      05  WS-LAST-MONTH-X REDEFINES                                EL317
00159          WS-LAST-MONTH               PIC XX.                      EL317
00160                                                                   EL317
00161      05  WS-MONTH                    PIC XX          VALUE ZERO.  EL317
00162                                                                   EL317
00163      05  WS-ABEND-FILE-STATUS        PIC XX          VALUE ZERO.  EL317
00164      05  WS-FIRST-TIME-SWITCH        PIC X           VALUE 'Y'.   EL317
00165          88  WS-FIRST-TIME                           VALUE 'Y'.   EL317
00166                                                                   EL317
00167      05  WS-DATE-WORK.                                            EL317
00168          10  WS-DW-MONTH             PIC 99.                      EL317
00169          10  FILLER                  PIC X.                       EL317
00170          10  WS-DW-DAY               PIC 99.                      EL317
00171          10  FILLER                  PIC X.                       EL317
00172          10  WS-DW-YEAR              PIC 99.                      EL317
091808
092010     05  WS-PAYEE-SEQUENCE           PIC 99.
092010     05  FILLER REDEFINES WS-PAYEE-SEQUENCE.
092010         10  WS-PAYEE-SEQ1           PIC 9.
092010         10  WS-PAYEE-SEQ2           PIC 9.
092010     05  WS-PAYEE-CODE.
092010         10  WS-PAYEE-CD-TYPE        PIC X.
092010         10  WS-PAYEE-CD-SEQ         PIC 9.
091808*    05  WS-PAYEE-STATE              PIC X(2)  VALUE LOW-VALUES.
091808     05  WS-PREV-PAYEE-STATE         PIC X(2)  VALUE LOW-VALUES.
091808     05  WS-SUB                      PIC S9(3) COMP-3 VALUE +0.
091808     05  WS-BEG-SUB                  PIC S9(3) COMP-3 VALUE +0.
091808     05  WS-END-SUB                  PIC S9(3) COMP-3 VALUE +0.
091808     05  WS-STATE-LENGTH             PIC S9(3) COMP-3 VALUE +0.
091808     05  WS-PAYEE-STATE-FOUND        PIC X(01)        VALUE 'N'.
091808         88 PAYEE-STATE-FOUND                         VALUE 'Y'.
051810     05  WS-PAYEE-CITY-STATE.
051810         10  WS-PAYEE-CITY           PIC X(28).
051810         10  WS-PAYEE-STATE          PIC XX.
091808*    05  FILLER  REDEFINES WS-PAYEE-CITY-STATE.
091808*        10  WS-PAYEE-CITY-ST OCCURS 30 TIMES PIC X(1).
00173                                                                   EL317
00174      EJECT                                                        EL317
                                       COPY ELCVOCH.
00204  01  FILLER.                                                      EL317
00206      05  WS-TA-LIFE-OVERIDE-L6       PIC X(6)        VALUE SPACE. EL317
00207      05  FILLER                      PIC X(24)       VALUE        EL317
00208          ' PAYMENTS'.                                             EL317
00209      05  FILLER                      COMP-3.                      EL317
00210          10  CT-CURR-LF-PMTS-AMT           PIC S9(9)V99  VALUE +0.EL317
00211          10  CT-CURR-LF-PMTS-CNT           PIC S9(7)     VALUE +0.EL317
00212                                                                   EL317
00216          10  CT-CURR-LF-PMTS-AMT-BM        PIC S9(9)V99  VALUE +0.EL317
00217          10  CT-CURR-LF-PMTS-CNT-BM        PIC S9(7)     VALUE +0.EL317
00218                                                                   EL317
00394  01  WS-HEADING1.                                                 EL317
00395      05  FILLER                      PIC X(49)       VALUE '1'.   EL317
00396      05  WS-H1-TITLE                 PIC X(71)       VALUE        EL317
00397          'DAILY CLAIM INTEREST PAYMENTS'.
00398      05  WS-H1-REPORT-NUMBER         PIC X(07)  VALUE 'EL317CI'.
00399                                                                   EL317
00400  01  WS-HEADING2.                                                 EL317
00401      05  FILLER                      PIC X(12)       VALUE        EL317
00402          '  CARRIER -'.                                           EL317
00403      05  WS-H2-CARRIER               PIC X           VALUE SPACES.EL317
00404      05  FILLER                      PIC X(32)       VALUE SPACES.EL317
00405      05  WS-H2-CLIENT-NAME           PIC X(75)       VALUE SPACES.EL317
00406      05  WS-H2-DATE                  PIC X(8).                    EL317
00407                                                                   EL317
00408  01  WS-HEADING3.                                                 EL317
00409      05  FILLER                      PIC XX          VALUE SPACES.EL317
00410      05  WS-H3-CARRIER-NAME          PIC X(30)       VALUE SPACES.EL317
00411      05  FILLER                      PIC X(21)       VALUE SPACES.EL317
00412      05  WS-H3-DATE                  PIC X(67)       VALUE SPACES.EL317
00413      05  FILLER                      PIC X(5)        VALUE 'PAGE'.EL317
00414      05  WS-H3-PAGE                  PIC ZZ,ZZ9.                  EL317
00415      05  FILLER                      PIC X(02)       VALUE SPACES.EL317
00416                                                                   EL317
00417  01  WS-HEADING4.                                                 EL317
00418      05  FILLER                      PIC X(8)        VALUE        EL317
00419          '-  CHECK'.                                              EL317
00420      05  FILLER                      PIC X(17)       VALUE SPACE. EL317
00421      05  FILLER                      PIC X(8)        VALUE        EL317
00422          'DATE    '.                                              EL317
00423      05  FILLER                      PIC X(9)        VALUE        EL317
00424          'PAY TYPE '.                                             EL317
00425      05  FILLER                      PIC X(13)       VALUE        EL317
00426          'CLAIMANT NAME'.                                         EL317
00427      05  FILLER                      PIC X(13)       VALUE SPACE. EL317
00428      05  FILLER                      PIC X(19)       VALUE        EL317
00429          'CLAIM NO INCURRED  '.                                   EL317
00430      05  FILLER                      PIC X(46)       VALUE        EL317
071403         'CAR  CERT NO   EFFECTIVE  ST  ACCOUNT   GROUP '.        EL317
00431 *        '************** CERTIFICATE DATA ************* '.        EL317
00432                                                                   EL317
00433  01  WS-HEADING5.                                                 EL317
00434      05  FILLER                      PIC X(52)       VALUE        EL317
00435          '  NUMBER      AMOUNT     PAID    PAYEE NAME AND ADDR'.  EL317
00436      05  FILLER                      PIC X(15)       VALUE
               'ESS'.
           05  FILLER                      PIC X(51)   VALUE SPACES.
           05  FILLER                      PIC X(10)  VALUE
               'SOC SEC NO'.

00452  01  WS-HEADING6.                                                 EL317
00453      05  FILLER                      PIC X(133)      VALUE        EL317
00454          '0 * * * * * * CARRIER TOTALS * * * * * *'.              EL317

00462  01  WS-HEADING8.                                                 EL317
00463      05  FILLER                      PIC X(47)   VALUE SPACES.    EL317
00464      05  FILLER                      PIC X(13)   VALUE            EL317
00465                                  'CURRENT MONTH'.                 EL317
00466      05  FILLER                      PIC X(73)   VALUE SPACES.    EL317
00467                                                                   EL317
00468  01  WS-HEADING9.                                                 EL317
00469      05  FILLER                      PIC X(43)    VALUE SPACES.   EL317
00470      05  FILLER                      PIC X(5)     VALUE 'COUNT'.  EL317
00471      05  FILLER                      PIC X(10)    VALUE SPACES.   EL317
00472      05  FILLER                      PIC X(06)    VALUE 'AMOUNT'. EL317
00473      05  FILLER                      PIC X(69)    VALUE SPACES.   EL317

00483  01  WS-DETAIL1.                                                  EL317
00484      05  FILLER                      PIC X.                       EL317
00485      05  WS-D1-CHECK-NUMBER          PIC X(7).                    EL317
00486      05  WS-D1-RESERVE-FLAG          PIC X.                       EL317
00487      05  WS-D1-AMOUNT                PIC Z,ZZZ,ZZ9.99-.           EL317
00488      05  FILLER                      PIC X.                       EL317
00489      05  WS-D1-DATE-PAID.                                         EL317
00490          10  WS-D1-MONTH-PAID        PIC 99.                      EL317
00491          10  FILLER                  PIC X.                       EL317
00492          10  WS-D1-DAY-PAID          PIC 99.                      EL317
00493          10  FILLER                  PIC X.                       EL317
00494          10  WS-D1-YEAR-PAID         PIC 99.                      EL317
00495                                                                   EL317
00496      05  FILLER                      PIC X.                       EL317
00497      05  WS-D1-PAY-TYPE.                                          EL317
00498          10  WS-D1-PT-1              PIC XX.                      EL317
00499          10  WS-D1-PT-1A             PIC X VALUE '-'.             EL317
00500          10  WS-D1-PT-2              PIC X(6).                    EL317
00501                                                                   EL317
00502      05  FILLER                      PIC X.                       EL317
00503      05  WS-D1-CLAIMANT-NAME         PIC X(25).                   EL317
00504      05  FILLER                      PIC X.                       EL317
00505      05  WS-D1-CLAIM-NO              PIC X(7).                    EL317
00506      05  FILLER                      PIC X.                       EL317
00507      05  WS-D1-INCURRED-DATE         PIC X(8).                    EL317
00508      05  FILLER                      PIC X(3).                    EL317
00509      05  WS-D1-CARRIER               PIC X.                       EL317
00510      05  FILLER                      PIC XX.                      EL317
00511      05  WS-D1-CERT-NO               PIC X(11).                   EL317
00512      05  FILLER                      PIC X.                       EL317
00513      05  WS-D1-EFFECTIVE-DATE        PIC X(8).                    EL317
00514      05  FILLER                      PIC X(3).                    EL317
00515      05  WS-D1-STATE                 PIC XX.                      EL317
00516      05  FILLER                      PIC X.                       EL317
00517      05  WS-D1-ACCOUNT               PIC X(10).                   EL317
00518      05  FILLER                      PIC X.                       EL317
00519      05  WS-D1-GROUP                 PIC X(6).                    EL317
00520                                                                   EL317
00522  01  WS-DETAIL2.
00525      05  FILLER                      PIC X.
00530      05  WS-D2-PAYEE-TYPE            PIC X(11).                   EL317
           05  FILLER                      PIC X.
           05  WS-D2-PAYEE-NAME            PIC X(30).
           05  FILLER                      PIC X.
           05  WS-D2-PAYEE-ADDR1           PIC X(30).
           05  FILLER                      PIC X.
           05  WS-D2-PAYEE-ADDR2           PIC X(30).
           05  FILLER                      PIC X.
           05  WS-D2-SOC-SEC-NO            PIC X(11).
           05  FILLER                      PIC X.
           05  WS-D2-SSN-COMMENT           PIC X(13).
00536                                                                   EL317
00536                                                                   EL317
00538  01  WS-TOTAL-LINE1.
00540      05  FILLER                      PIC XX.                      EL317
00541      05  WS-T1-DESCRIPTION.                                       EL317
00542          10  WS-T1-BENEFIT-CODE      PIC XX.                      EL317
00543          10  FILLER                  PIC X(4).                    EL317
00544          10  WS-T1-BENEFIT-DESC      PIC X(10).                   EL317
00545          10  FILLER                  PIC X(16).                   EL317
00546                                                                   EL317
00547      05  FILLER REDEFINES                                         EL317
00548          WS-T1-DESCRIPTION.                                       EL317
00549          10  FILLER                  PIC X(6).                    EL317
00550          10  WS-T1-TOTAL-DESCRIPTION PIC X(26).                   EL317
00551                                                                   EL317
00552      05  FILLER REDEFINES                                         EL317
00553          WS-T1-DESCRIPTION.                                       EL317
00554          10  FILLER                  PIC X(8).                    EL317
00555          10  WS-T1-VOIDED-DESCRIPTION PIC X(24).                  EL317
00556                                                                   EL317
00557      05  FILLER REDEFINES                                         EL317
00558          WS-T1-DESCRIPTION.                                       EL317
00559          10  FILLER                  PIC X(4).                    EL317
00560          10  WS-T1-NET-DESCRIPTION PIC X(28).                     EL317
00561                                                                   EL317
00562      05  FILLER                      PIC X.                       EL317
00563      05  WS-T1-DASH                  PIC X.                       EL317
00564      05  FILLER                      PIC X.                       EL317
00565      05  WS-T1-CURR-COUNT            PIC ZZZ,ZZZ,ZZ9-.            EL317
00566      05  FILLER                      PIC X.                       EL317
00567      05  WS-T1-CURR-AMOUNT           PIC ZZZ,ZZZ,ZZ9.99-.         EL317
00568      05  FILLER                      PIC X.                       EL317
00569                                                                   EL317
00570  01  WS-TOTAL-LINE2.
00572      05  FILLER                      PIC XX.                      EL317
00573      05  WS-T2-DESCRIPTION.                                       EL317
00574          10  FILLER                  PIC X(6).                    EL317
00575          10  WS-T2-PAYEE-DESC        PIC X(11).                   EL317
00576          10  FILLER                  PIC X(15).                   EL317
00577                                                                   EL317
00578      05  FILLER REDEFINES                                         EL317
00579          WS-T2-DESCRIPTION.                                       EL317
00580          10  FILLER                  PIC X(6).                    EL317
00581          10  WS-T2-TOTAL-DESCRIPTION PIC X(26).                   EL317
00582                                                                   EL317
00583      05  FILLER REDEFINES                                         EL317
00584          WS-T2-DESCRIPTION.                                       EL317
00585          10  FILLER                  PIC X(8).                    EL317
00586          10  WS-T2-VOIDED-DESCRIPTION PIC X(24).                  EL317
00587                                                                   EL317
00588      05  FILLER REDEFINES                                         EL317
00589          WS-T2-DESCRIPTION.                                       EL317
00590          10  FILLER                  PIC X(4).                    EL317
00591          10  WS-T2-NET-DESCRIPTION PIC X(28).                     EL317
00592                                                                   EL317
00593      05  FILLER                      PIC X.                       EL317
00594      05  WS-T2-DASH                  PIC X.                       EL317
00595      05  FILLER                      PIC X.                       EL317
00596      05  WS-T2-CURR-COUNT            PIC ZZZ,ZZZ,ZZ9-.            EL317
00597      05  FILLER                      PIC X.                       EL317
00598      05  WS-T2-CURR-AMOUNT           PIC ZZZ,ZZZ,ZZ9.99-.         EL317
00599      05  FILLER                      PIC X.                       EL317
00600                                                                   EL317
00601  01  WS-NO-ACTIVITY.                                              EL317
00602      05  FILLER                      PIC X(30).                   EL317
00603      05  FILLER                      PIC X(30) VALUE              EL317
00604            'NO ACTIVITY WAS FOUND FOR THIS'.                      EL317
00605      05  FILLER                      PIC X(30) VALUE              EL317
00606            ' DATE RANGE                   '.                      EL317
00607                                                                   EL317
072110
072110 01  NAP-LETTER-RECORD.
072110     05  NLR-CYCLE-DATE       PIC X(8).
072110     05  NLR-TAB00            PIC X.
072110     05  NLR-LETTER-ID        PIC X(4).
072110     05  NLR-TAB01            PIC X.
072110     05  NLR-CARRIER          PIC X.
072110     05  NLR-TAB02            PIC X.
072110     05  NLR-STATE-CODE       PIC X(2).
072110     05  NLR-TAB03            PIC X.
072110     05  NLR-CLAIM-NO         PIC X(7).
072110     05  NLR-TAB04            PIC X.
072110     05  NLR-CERT-NO          PIC X(11).
072110     05  NLR-TAB05            PIC X.
072110     05  NLR-ACCT-NO          PIC X(10).
072110     05  NLR-TAB06            PIC X.
072110     05  NLR-CERT-EFF-DT      PIC X(10).
072110     05  NLR-TAB07            PIC X.
072110     05  NLR-CLAIM-INCUR-DT   PIC X(10).
072110     05  NLR-TAB08            PIC X.
072110     05  NLR-DRAFT-DT         PIC X(10).
072110     05  NLR-TAB09            PIC X.
072110     05  NLR-DRAFT-NO         PIC X(10).
072110     05  NLR-TAB10            PIC X.
072110     05  NLR-DRAFT-ORDER      PIC X(5).
072110     05  NLR-TAB11            PIC X.
072110     05  NLR-DRAFT-AMOUNT     PIC -9(7).99.
072110     05  NLR-TAB12            PIC X.
072110     05  NLR-PAID-FROM-DT     PIC X(10).
072110     05  NLR-TAB13            PIC X.
072110     05  NLR-PAID-THRU-DT     PIC X(10).
072110     05  NLR-TAB14            PIC X.
072110     05  NLR-FORM-DUE-DT      PIC X(10).
072110     05  NLR-TAB15            PIC X.
072110     05  NLR-PAYEE-NAME       PIC X(30).
072110     05  NLR-TAB16            PIC X.
072110     05  NLR-PAYEE-ADDR1      PIC X(40).
072110     05  NLR-TAB17            PIC X.
072110     05  NLR-PAYEE-ADDR2      PIC X(40).
072110     05  NLR-TAB18            PIC X.
072110     05  NLR-PAYEE-CITY       PIC X(40).
072110     05  NLR-TAB19            PIC X.
072110     05  NLR-PAYEE-STATE      PIC X(2).
072110     05  NLR-TAB20            PIC X.
072110     05  NLR-PAYEE-ZIP        PIC X(9).
072110     05  NLR-TAB21            PIC X.
072110     05  NLR-INS-LAST-NAME    PIC X(15).
072110     05  NLR-TAB22            PIC X.
072110     05  NLR-INS-FIRST-NAME   PIC X(15).
072110     05  NLR-TAB23            PIC X.
072110     05  NLR-INS-ADDR1        PIC X(40).
072110     05  NLR-TAB24            PIC X.
072110     05  NLR-INS-ADDR2        PIC X(40).
072110     05  NLR-TAB25            PIC X.
072110     05  NLR-INS-CITY         PIC X(40).
072110     05  NLR-TAB26            PIC X.
072110     05  NLR-INS-STATE        PIC X(2).
072110     05  NLR-TAB27            PIC X.
072110     05  NLR-INS-ZIP          PIC X(9).
072110     05  NLR-TAB28            PIC X.
072110     05  NLR-ACCT-NAME        PIC X(30).
072110     05  NLR-TAB29            PIC X.
072110     05  NLR-ACCT-ADDR1       PIC X(40).
072110     05  NLR-TAB30            PIC X.
072110     05  NLR-ACCT-ADDR2       PIC X(40).
072110     05  NLR-TAB31            PIC X.
072110     05  NLR-ACCT-CITY        PIC X(40).
072110     05  NLR-TAB32            PIC X.
072110     05  NLR-ACCT-STATE       PIC X(2).
072110     05  NLR-TAB33            PIC X.
072110     05  NLR-ACCT-ZIP         PIC X(9).
072110     05  NLR-TAB34            PIC X.
072110     05  NLR-BENE-NAME        PIC X(30).
072110     05  NLR-TAB35            PIC X.
072110     05  NLR-BENE-ADDR1       PIC X(40).
072110     05  NLR-TAB36            PIC X.
072110     05  NLR-BENE-ADDR2       PIC X(40).
072110     05  NLR-TAB37            PIC X.
072110     05  NLR-BENE-CITY        PIC X(40).
072110     05  NLR-TAB38            PIC X.
072110     05  NLR-BENE-STATE       PIC X(2).
072110     05  NLR-TAB39            PIC X.
072110     05  NLR-BENE-ZIP         PIC X(9).
072110     05  NLR-TAB40            PIC X.
072110     05  NLR-NO-OF-PMTS       PIC ZZ9.
072110     05  NLR-TAB41            PIC X.
072110     05  NLR-TOTAL-PAID-AMT   PIC -9(7).99.
072110     05  NLR-TAB42            PIC X.
072110     05  NLR-PREV-PAID-THRU-DT PIC X(10).
072110     05  NLR-TAB43            PIC X.
072110     05  NLR-PREV-TOT-PAID-AMT PIC -9(7).99.
072110     05  NLR-TAB44            PIC X.
072110     05  NLR-PREV-NO-OF-PMTS  PIC ZZ9.
072110     05  NLR-TAB45            PIC X.
072110     05  NLR-MONTHLY-BENEFIT  PIC -9(7).99.
072110     05  NLR-TAB46            PIC X.
072110     05  NLR-LOAN-NO          PIC X(25).
072110     05  NLR-TAB47            PIC X.
072110     05  NLR-EOB-LOAN-NO      PIC X(25).
072110     05  NLR-TAB48            PIC X.
072110     05  NLR-COV-LET-ID       PIC X(4).
072110     05  NLR-TAB49            PIC X.
072110     05  NLR-SURVEY-ID        PIC X(4).
072110     05  NLR-TAB50            PIC X.
072110     05  NLR-DCC-EVENT        PIC X(12).
072110     05  NLR-TAB51            PIC X.
072110     05  NLR-EOB-ID           PIC X(4).
072110     05  NLR-TAB52            PIC X.
072110     05  NLR-ENCLOSURE-CD     PIC X(3).
072110     05  NLR-TAB53            PIC X.
072110     05  NLR-DRAFT-NOTE-1     PIC X(60).
072110     05  NLR-TAB54            PIC X.
072110     05  NLR-DRAFT-NOTE-2     PIC X(60).
072110     05  NLR-TAB55            PIC X.
072110     05  NLR-DRAFT-NOTE-3     PIC X(4).
072110     05  NLR-TAB56            PIC X.
072110     05  NLR-DRAFT-NOTE-4     PIC X(4).
072110     05  NLR-TAB57            PIC X.
072110     05  NLR-DRAFT-NOTE-5     PIC X(4).
072110     05  NLR-TAB58            PIC X.
072110     05  NLR-DRAFT-NOTE-6     PIC X(4).
072110     05  NLR-TAB59            PIC X.
072110     05  NLR-DRAFT-NOTE-7     PIC X(4).
072110     05  NLR-TAB60            PIC X.
072110     05  NLR-DRAFT-NOTE-8     PIC X(4).
072110     05  NLR-TAB61            PIC X.
072110     05  NLR-DRAFT-NOTE-9     PIC X(4).
072110     05  NLR-TAB62            PIC X.
072110     05  NLR-DRAFT-NOTE-10    PIC X(4).
072110     05  NLR-TAB63            PIC X.
072110     05  NLR-DRAFT-NOTE-11    PIC X(4).
072110     05  NLR-TAB64            PIC X.
072110     05  NLR-DRAFT-NOTE-12    PIC X(4).
072110     05  NLR-TAB65            PIC X.
072110     05  NLR-DRAFT-NOTE-13    PIC X(4).
072110     05  NLR-TAB66            PIC X.
072110     05  NLR-DRAFT-NOTE-14    PIC X(4).
072110     05  NLR-TAB67            PIC X.
072110     05  NLR-CLAIM-TYPE       PIC X.
072110     05  NLR-TAB68            PIC X.
072110     05  NLR-PMT-TYPE         PIC X.
072110     05  NLR-TAB69            PIC X.
072110     05  NLR-PLAN-CODE        PIC X(4).
072110     05  NLR-TAB70            PIC X.
072110     05  NLR-BARCODE          PIC X(25).
072110     05  NLR-TAB71            PIC X.
072110     05  NLR-LIFE-INT-RATE    PIC 99.9(5).
072110     05  NLR-TAB72            PIC X.
072110     05  NLR-INS-SEX-CODE     PIC X.
072110     05  NLR-TAB73            PIC X.
072110     05  NLR-AUTO-PAY         PIC X.
072110     05  NLR-TAB74            PIC X.
072110     05  NLR-AUTO-PAY-END-DT  PIC X(10).
072110     05  NLR-TAB75            PIC X.
072110     05  NLR-CERT-EXP-DT      PIC X(10).
072110     05  NLR-TAB76            PIC X.
072110     05  NLR-PAYEE-NAME-1     PIC X(30).
072110     05  NLR-TAB77            PIC X.
072110     05  NLR-TOTAL-AMT-PAID-1 PIC -9(7).99.
072110     05  NLR-TAB78            PIC X.
072110     05  NLR-PAID-FROM-1      PIC X(10).
072110     05  NLR-TAB79            PIC X.
072110     05  NLR-PAID-THRU-1      PIC X(10).
072110     05  NLR-TAB80            PIC X.
072110     05  NLR-PAYEE-NAME-2     PIC X(30).
072110     05  NLR-TAB81            PIC X.
072110     05  NLR-TOTAL-AMT-PAID-2 PIC -9(7).99.
072110     05  NLR-TAB82            PIC X.
072110     05  NLR-PAID-FROM-2      PIC X(10).
072110     05  NLR-TAB83            PIC X.
072110     05  NLR-PAID-THRU-2      PIC X(10).
072110     05  NLR-TAB84            PIC X.
072110     05  NLR-PAYEE-NAME-3     PIC X(30).
072110     05  NLR-TAB85            PIC X.
072110     05  NLR-TOTAL-AMT-PAID-3 PIC -9(7).99.
072110     05  NLR-TAB86            PIC X.
072110     05  NLR-PAID-FROM-3      PIC X(10).
072110     05  NLR-TAB87            PIC X.
072110     05  NLR-PAID-THRU-3      PIC X(10).
072110     05  NLR-TAB88            PIC X.
072110     05  NLR-PAYEE-NAME-4     PIC X(30).
072110     05  NLR-TAB89            PIC X.
072110     05  NLR-TOTAL-AMT-PAID-4 PIC -9(7).99.
072110     05  NLR-TAB90            PIC X.
072110     05  NLR-PAID-FROM-4      PIC X(10).
072110     05  NLR-TAB91            PIC X.
072110     05  NLR-PAID-THRU-4      PIC X(10).
072110     05  NLR-TAB92            PIC X.
092010     05  NLR-NEXTBUSDATE      PIC X(10).
092010     05  NLR-TAB93            PIC X.
092010     05  NLR-PAYEE-CODE       PIC XX.
092010     05  NLR-TAB94            PIC X.
032012     05  NLR-AHL-CLAIM-NO     PIC X(9).
032012     05  NLR-TAB95            PIC X.
072110     05  NLR-LAST-BYTE        PIC X.

                                COPY ELCFUNDT.
00608                           COPY ELCDATE.                              CL**3
00610                           COPY ELCDTECX.                          EL317
00612                           COPY ELCDTEVR.                          EL317

072110                            
072110 LINKAGE SECTION.
072110
072110 01  PARM.
072110     05  PARM-LENGTH        COMP     PIC S9(04)    VALUE ZEROS.
072110     05  PARM-CYCLE-DATE             PIC X(08)     VALUE SPACES.
072110
072110 PROCEDURE DIVISION USING PARM.
072110
072110      IF PARM-CYCLE-DATE = SPACES
072110          DISPLAY 'MISSING CYCLE DATE PARM'
072110          GO TO ABEND-PGM
072110      END-IF.

00616  0000-DATE-CARD-READ SECTION.    COPY ELCDTERX.

00628  0000-MAIN-LOGIC SECTION.

00630      PERFORM OPEN-FILES          THRU OFS-EXIT
           PERFORM 1000-INIT           THRU 1000-EXIT

00632      PERFORM 3000-PRINT-REPORT   THRU 3000-EXIT UNTIL
              END-OF-INPUT

           IF WS-RECS-IN > 0
091808       AND WS-PREV-PAYEE-STATE = 'AK'           
              PERFORM 3600-CREATE-FREEDOM
                                       THRU 3600-EXIT
           END-IF

           PERFORM 4000-PRINT-TOTALS   THRU 4900-EXIT

00634      PERFORM CLOSE-FILES         THRU CFS-EXIT.

00636      GOBACK.

       1000-INIT.

           MOVE FUNCTION CURRENT-DATE  TO FUNCTION-DATE.
           DISPLAY ' WS-FN-DATE ' WS-FN-DATE
           MOVE WS-FN-DATE             TO DC-GREG-DATE-CYMD
           MOVE 'L'                    TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-BIN-DATE-1       TO WS-CURRENT-BIN-DT
           ELSE
              DISPLAY 'BAD CURRENT DATE ' WS-FN-DATE
              PERFORM ABEND-PGM
           END-IF
072110           
072110     MOVE PARM-CYCLE-DATE        TO DC-GREG-DATE-CYMD
072110     MOVE 'L'                    TO DC-OPTION-CODE
072110     PERFORM 8500-DATE-CONVERSION
072110                                 THRU 8590-EXIT
072110     IF NO-CONVERSION-ERROR
072110        MOVE DC-BIN-DATE-1       TO WS-PARM-BIN-DT
072110     ELSE
072110     DISPLAY 'BAD PARM CYCLE DATE ' PARM-CYCLE-DATE
072110        PERFORM ABEND-PGM
072110     END-IF
00622      MOVE LIFE-OVERRIDE-L6       TO WS-TA-LIFE-OVERIDE-L6.

           MOVE SPACES                 TO TRANSACTION-RECORD
                                          CHECK-DES-RECORD
                                          DISTRIBUTION-RECORD
                                          PAYEE-ADDRESS-RECORD
                                          ALPHA-1099-RECORD
                                          VOUCH-ADDR-1099-RECORD
                                          
           MOVE 'T'                    TO TR-RECORD-ID
           MOVE 'F'                    TO CD-RECORD-ID
           MOVE 'D'                    TO DR-RECORD-ID
           MOVE 'A'                    TO PR-RECORD-ID
           MOVE '1'                    TO AR-RECORD-ID
           MOVE '2'                    TO VR-RECORD-ID
           MOVE WS-FN-DA (2:1)         TO WS-REC-GRP-DA
           MOVE 'PSEUDO-INT'           TO TR-VENDOR-ID
                                          CD-VENDOR-ID
                                          DR-VENDOR-ID
                                          PR-VENDOR-ID
                                          AR-VENDOR-ID
                                          VR-VENDOR-ID
           MOVE 'P'                    TO TR-CHECK-TYPE
           MOVE 'R'                    TO TR-TRAN-TYPE
041712     IF DTE-CLIENT = 'AHL'
041712         MOVE 'AHL'              TO TR-CSO
041712     ELSE
041712         MOVE 'CSO'              TO TR-CSO
041712     END-IF

           STRING WS-FN-MO WS-FN-DA WS-FN-CCYR DELIMITED BY SIZE
              INTO TR-INVOICE-DATE
           END-STRING
              
           MOVE 'IMM'                  TO TR-TERMS-CODE
           MOVE 001                    TO CD-SEQ-NO
                                          DR-SEQ-NO
           MOVE 'E'                    TO DR-DIST-TYPE (1)
032212     if dte-client = 'AHL'
032212        MOVE '6146000100A10A10CRLIFS'
032212                                 TO DR-ACCT-NO (1)
032212     else
032212        MOVE '6146000100026620CRLIFS'
032212                                 TO DR-ACCT-NO (1)
032212     end-if
           MOVE 'AP'                   TO DR-SOURCE (1)
           MOVE 'S'                    TO DR-SUSPENSE (1) (1:1)
           MOVE 'INT'                  TO AR-CATEGORY
                                          VR-CATEGORY
           MOVE 'S'                    TO AR-TAX-ID-TYPE
           MOVE 'Y'                    TO AR-ACCUM-BY-ACCT
           MOVE 'U'                    TO VR-ADDRESS-TYPE
           MOVE ZEROS                  TO TR-NON-DIS-AMT
                                          TR-DIS-PCT
                                          TR-DIS-AMT
                                          TR-TAX-PCT
                                          TR-TAX-ADJ
                                          TR-TAX-AMT
                                          DR-USE-TAX-AMT (1)
           
           MOVE '+'                    TO TR-NON-DIS-AMT-SIGN
                                          TR-DIS-AMT-SIGN
                                          TR-TAX-ADJ-SIGN
                                          TR-TAX-AMT-SIGN
                                          DR-USE-TAX-AMT-SIGN (1)

           MOVE TRANSACTION-RECORD     TO WS-TRANSACTION-RECORD
           MOVE CHECK-DES-RECORD       TO WS-CHECK-DES-RECORD
           MOVE DISTRIBUTION-RECORD    TO WS-DISTRIBUTION-RECORD
           MOVE PAYEE-ADDRESS-RECORD   TO WS-PAYEE-ADDRESS-RECORD
           MOVE ALPHA-1099-RECORD      TO WS-ALPHA-1099-RECORD
           MOVE VOUCH-ADDR-1099-RECORD TO WS-VOUCH-ADDR-1099-RECORD

      *    MOVE ZEROS                  TO WS-LINE-COUNT
      *    MOVE '1<HTML>'              TO PRT
      *    PERFORM WRITE-A-LINE
      *    MOVE ' <HEAD>'              TO PRT
      *    PERFORM WRITE-A-LINE
      *    MOVE ' <PRE>'               TO PRT
      *    PERFORM WRITE-A-LINE
           MOVE +99                    TO WS-LINE-COUNT

           PERFORM 1200-READ-EXTR      THRU 1200-EXIT
           IF NOT END-OF-INPUT
              MOVE 'N'                 TO WS-FIRST-TIME-SWITCH
              MOVE EX-SB-CARRIER       TO WS-LAST-CARRIER
              PERFORM 8100-GET-CARRIER-NAME
                                       THRU 8190-EXIT
           END-IF
      *    MOVE WS-COMP-KEY            TO WS-PREV-KEY
072110     PERFORM 1050-INIT-NAPER     THRU 1050-EXIT
          
           .
       1000-EXIT.
           EXIT.
           
072110 1050-INIT-NAPER.
072110       MOVE SPACES               TO NAP-LETTER-RECORD.
072110       MOVE ZEROS                TO NLR-DRAFT-AMOUNT
072110                                    NLR-NO-OF-PMTS
072110                                    NLR-TOTAL-PAID-AMT
072110                                    NLR-PREV-TOT-PAID-AMT
072110                                    NLR-PREV-NO-OF-PMTS
072110                                    NLR-MONTHLY-BENEFIT
072110                                    NLR-LIFE-INT-RATE
072110                                    NLR-TOTAL-AMT-PAID-1
072110                                    NLR-TOTAL-AMT-PAID-2
072110                                    NLR-TOTAL-AMT-PAID-3
072110                                    NLR-TOTAL-AMT-PAID-4.
072110       MOVE X'A2'                TO NLR-TAB00
072110                                    NLR-TAB01
072110                                    NLR-TAB02
072110                                    NLR-TAB03
072110                                    NLR-TAB04
072110                                    NLR-TAB05
072110                                    NLR-TAB06
072110                                    NLR-TAB07
072110                                    NLR-TAB08
072110                                    NLR-TAB09
072110                                    NLR-TAB10
072110                                    NLR-TAB11
072110                                    NLR-TAB12
072110                                    NLR-TAB13
072110                                    NLR-TAB14
072110                                    NLR-TAB15
072110                                    NLR-TAB16
072110                                    NLR-TAB17
072110                                    NLR-TAB18
072110                                    NLR-TAB19
072110                                    NLR-TAB20
072110                                    NLR-TAB21
072110                                    NLR-TAB22
072110                                    NLR-TAB23
072110                                    NLR-TAB24
072110                                    NLR-TAB25
072110                                    NLR-TAB26
072110                                    NLR-TAB27
072110                                    NLR-TAB28
072110                                    NLR-TAB29
072110                                    NLR-TAB30
072110                                    NLR-TAB31
072110                                    NLR-TAB32
072110                                    NLR-TAB33
072110                                    NLR-TAB34
072110                                    NLR-TAB35
072110                                    NLR-TAB36
072110                                    NLR-TAB37
072110                                    NLR-TAB38
072110                                    NLR-TAB39
072110                                    NLR-TAB40
072110                                    NLR-TAB41
072110                                    NLR-TAB42
072110                                    NLR-TAB43
072110                                    NLR-TAB44
072110                                    NLR-TAB45
072110                                    NLR-TAB46
072110                                    NLR-TAB47
072110                                    NLR-TAB48
072110                                    NLR-TAB49
072110                                    NLR-TAB50
072110                                    NLR-TAB51
072110                                    NLR-TAB52
072110                                    NLR-TAB53
072110                                    NLR-TAB54
072110                                    NLR-TAB55
072110                                    NLR-TAB56
072110                                    NLR-TAB57
072110                                    NLR-TAB58
072110                                    NLR-TAB59
072110                                    NLR-TAB60
072110                                    NLR-TAB61
072110                                    NLR-TAB62
072110                                    NLR-TAB63
072110                                    NLR-TAB64
072110                                    NLR-TAB65
072110                                    NLR-TAB66
072110                                    NLR-TAB67
072110                                    NLR-TAB68
072110                                    NLR-TAB69
072110                                    NLR-TAB70
072110                                    NLR-TAB71
072110                                    NLR-TAB72
072110                                    NLR-TAB73
072110                                    NLR-TAB74
072110                                    NLR-TAB75
072110                                    NLR-TAB76
072110                                    NLR-TAB77
072110                                    NLR-TAB78
072110                                    NLR-TAB79
072110                                    NLR-TAB80
072110                                    NLR-TAB81
072110                                    NLR-TAB82
072110                                    NLR-TAB83
072110                                    NLR-TAB84
072110                                    NLR-TAB85
072110                                    NLR-TAB86
072110                                    NLR-TAB87
072110                                    NLR-TAB88
072110                                    NLR-TAB89
072110                                    NLR-TAB90
072110                                    NLR-TAB91
092010                                    NLR-TAB92
092010                                    NLR-TAB93
092010                                    NLR-TAB94
032012                                    NLR-TAB95.
072110       MOVE PARM-CYCLE-DATE      TO NLR-CYCLE-DATE.
072110
072110 1050-EXIT.
072110     EXIT.
           
       1200-READ-EXTR.

           READ REPORTS-EXTRACT-FILE AT END
              MOVE HIGH-VALUES         TO EX-SB-CARRIER
              SET END-OF-INPUT         TO TRUE
           END-READ

           IF NOT END-OF-INPUT
              ADD 1                    TO WS-RECS-IN
              MOVE EX-SB-CARRIER       TO WS-COMP-CARRIER
              MOVE EX-BA-CLAIM-NO      TO WS-COMP-CLAIM-NO
              MOVE EX-BA-CERT-NO       TO WS-COMP-CERT-NO
           END-IF

           .
       1200-EXIT.
           EXIT.
           
00639  3000-PRINT-REPORT.

           PERFORM 3100-READ-ELTRLR    THRU 3100-EXIT
           IF (PROCESS-PMT)
              OR (PROCESS-RERUN)
              PERFORM 3500-G-L-PROCESSING
                                       THRU 3500-EXIT
091808        IF WS-PAYEE-STATE = 'AK'
                 PERFORM 3200-PRINT-REPORT
                                       THRU 3200-EXIT
091808        END-IF                                       
           END-IF

      *    IF EX-BA-PMT-RECORDED-DT NOT = WS-CURRENT-BIN-DT
      *       GO TO 3000-PRINT-REPORT
      *    END-IF

           PERFORM 1200-READ-EXTR      THRU 1200-EXIT

           .
       3000-EXIT.
           EXIT.
           
       3100-READ-ELTRLR.

           MOVE SPACES                 TO WS-PROCESS-PMT-SW
                                          WS-PROCESS-RERUN-SW
           MOVE DTE-CLASIC-COMPANY-CD  TO AT-COMPANY-CD
           MOVE EX-SB-CARRIER          TO AT-CARRIER
           MOVE EX-BA-CLAIM-NO         TO AT-CLAIM-NO
           MOVE EX-BA-CERT-NO          TO AT-CERT-NO
           MOVE EX-BA-PMT-SEQ-NO       TO AT-SEQUENCE-NO
           
           READ ELTRLR
           IF ELTRLR-FILE-STATUS = '00'
              IF (AT-INT-PMT-SELECT-DT = SPACES OR LOW-VALUES)
      *          AND (AT-CHECK-WRITTEN-DT = LOW-VALUES)
      *          AND (AT-CHECK-NO = SPACES)
                 AND (AT-VOID-DT = LOW-VALUES)
                 SET PROCESS-PMT       TO TRUE
              END-IF
      *       IF AT-INT-PMT-SELECT-DT = WS-PARM-BIN-DT
              IF (AT-INT-PMT-SELECT-DT = BIN-RUN-DATE)
                 AND (AT-VOID-DT = LOW-VALUES)
                 SET PROCESS-RERUN     TO TRUE
                 DISPLAY ' ABOUT TO PROCESS A RERUN ' WS-COMP-KEY
              END-IF
           END-IF

           .
       3100-EXIT.
           EXIT.

00730  3200-PRINT-REPORT.                                               EL317
00731                                                                   EL317
00732 *    NOTE ******************************************************* EL317
00733 *         *          DETAIL RECORD PROCESSING                   * EL317
00734 *         *******************************************************.EL317

00708      IF EX-SB-CARRIER = WS-LAST-CARRIER                           EL317
00709         CONTINUE
           ELSE
00719         MOVE EX-SB-CARRIER       TO WS-LAST-CARRIER
00720         PERFORM 8100-GET-CARRIER-NAME
                                       THRU 8190-EXIT
              MOVE +99                 TO WS-LINE-COUNT
           END-IF
              
00743      MOVE '0'                   TO  WS-DETAIL1.                   EL317
00744      MOVE EX-SB-CHECK-NO        TO  WS-D1-CHECK-NUMBER.           EL317
00745                                                                   EL317
      *    IF EX-BA-VOID-DT NOT = LOW-VALUES
      *       COMPUTE WS-D1-AMOUNT = EX-BA-PAYMENT-AMOUNT * -1
      *    ELSE
00748 *       MOVE EX-BA-PAYMENT-AMOUNT
      *                                TO  WS-D1-AMOUNT
      *    END-IF
00749                                                                   EL317
           MOVE WS-PMT-AMT             TO WS-D1-AMOUNT
00750      IF EX-BA-CHECK-WRITTEN-DT NOT = LOW-VALUES                   EL317
00751          MOVE EX-BA-CHECK-WRITTEN-DT TO  DC-BIN-DATE-1            EL317
00752          MOVE SPACES                 TO  DC-OPTION-CODE           EL317
               PERFORM 8500-DATE-CONVERSION
                                       THRU 8590-EXIT
00754          MOVE DC-GREG-DATE-1-EDIT    TO  WS-D1-DATE-PAID.         EL317

00764      MOVE 'INTEREST'             TO WS-D1-PAY-TYPE

00839      MOVE EX-BA-INSURED-LAST-NAME
                                       TO WS-D1-CLAIMANT-NAME

00787      MOVE EX-BA-CLAIM-NO     TO  WS-D1-CLAIM-NO.                  EL317
00788                                                                   EL317
00789      IF EX-BA-INCURRED-DT NOT = LOW-VALUES                        EL317
00790          MOVE EX-BA-INCURRED-DT    TO DC-BIN-DATE-1               EL317
00791          MOVE SPACES               TO DC-OPTION-CODE              EL317
               PERFORM 8500-DATE-CONVERSION
                                       THRU 8590-EXIT
00793          MOVE DC-GREG-DATE-1-EDIT  TO WS-D1-INCURRED-DATE.        EL317
00794                                                                   EL317
00795      MOVE EX-SB-CARRIER         TO  WS-D1-CARRIER.                EL317
00796      MOVE EX-BA-CERT-NO         TO  WS-D1-CERT-NO.                EL317
00797      MOVE EX-BA-CERT-EFF-DT     TO DC-BIN-DATE-1.                 EL317
00798      MOVE SPACES                TO  DC-OPTION-CODE.               EL317
           PERFORM 8500-DATE-CONVERSION
                                       THRU 8590-EXIT
00800      MOVE DC-GREG-DATE-1-EDIT   TO  WS-D1-EFFECTIVE-DATE.         EL317
00801      MOVE EX-BA-STATE           TO  WS-D1-STATE.                  EL317
00802      MOVE EX-BA-ACCOUNT         TO  WS-D1-ACCOUNT.                EL317
00803      MOVE EX-BA-GROUPING        TO  WS-D1-GROUP.                  EL317


           MOVE SPACES                TO  WS-DETAIL2

           MOVE EX-BA-PAYEES-NAME  TO  WS-D2-PAYEE-NAME

      *    MOVE DTE-CLASIC-COMPANY-CD  TO AT-COMPANY-CD
      *    MOVE EX-SB-CARRIER          TO AT-CARRIER
      *    MOVE EX-BA-CLAIM-NO         TO AT-CLAIM-NO
      *    MOVE EX-BA-CERT-NO          TO AT-CERT-NO
      *    MOVE EX-BA-PAYEE-ADDR-SEQ   TO AT-SEQUENCE-NO
           
      *    READ ELTRLR
           IF (ELTRLR-FILE-STATUS = '00')
              AND (AT-TRAILER-TYPE = '5')
              MOVE AT-ADDRESS-LINE-1   TO WS-D2-PAYEE-ADDR1
051810        MOVE SPACES              TO WS-D2-PAYEE-ADDR2
051810        STRING AT-CITY ' ' AT-STATE ' ' AT-ZIP
051810           DELIMITED BY '  '  INTO WS-D2-PAYEE-ADDR2
051810        END-STRING
      *       STRING AT-CITY-STATE AT-ZIP DELIMITED BY SIZE
      *          INTO WS-D2-PAYEE-ADDR2
      *       END-STRING
           ELSE
              MOVE 'INVALID ADDRESS '  TO WS-D2-PAYEE-ADDR1
                                          WS-D2-PAYEE-ADDR2
           END-IF

122402     EVALUATE TRUE
122402        WHEN EX-BA-PAYEE-TYPE-CD = 'I'
00820            MOVE 'INSURED'        TO WS-D2-PAYEE-TYPE

122402        WHEN EX-BA-PAYEE-TYPE-CD = 'B'
00823            MOVE 'BENEFICIARY'    TO WS-D2-PAYEE-TYPE

122402        WHEN EX-BA-PAYEE-TYPE-CD = 'A'
00826            MOVE 'ACCOUNT'        TO WS-D2-PAYEE-TYPE

122402        WHEN EX-BA-PAYEE-TYPE-CD = 'O'
00829            MOVE 'OTHER 1'        TO WS-D2-PAYEE-TYPE

122402        WHEN EX-BA-PAYEE-TYPE-CD = 'Q' 
00832            MOVE 'OTHER 2'        TO WS-D2-PAYEE-TYPE

122402        WHEN EX-BA-PAYEE-TYPE-CD = 'P'  
00835            MOVE 'PHYSICIAN'      TO WS-D2-PAYEE-TYPE

122402        WHEN OTHER
00837            MOVE EX-BA-PAYEE-TYPE-CD TO WS-D2-PAYEE-TYPE
122402     END-EVALUATE

           IF EX-BA-SOC-SEC-NO (4:1) = '-'
              MOVE EX-BA-SOC-SEC-NO    TO WS-D2-SOC-SEC-NO
           ELSE
              IF EX-BA-SOC-SEC-NO (1:9) NOT NUMERIC
                 MOVE ' INVALID SSN '  TO WS-D2-SSN-COMMENT
              END-IF
              STRING EX-BA-SOC-SEC-NO (1:3) '-' EX-BA-SOC-SEC-NO (4:2)
                 '-' EX-BA-SOC-SEC-NO (6:4) DELIMITED BY SIZE
                                       INTO WS-D2-SOC-SEC-NO
              END-STRING
           END-IF

           .
       3200-EXIT.
           EXIT.
           
       3500-G-L-PROCESSING.

           IF WS-PREV-KEY = LOW-VALUES
              MOVE WS-COMP-KEY         TO WS-PREV-KEY
           END-IF
091808
041812     IF WS-PAYEE-STATE <> 'AK'
041812        MOVE +0                  TO WS-PMT-AMT           
091808        MOVE WS-COMP-KEY         TO WS-PREV-KEY
091808     END-IF
091808     MOVE WS-PAYEE-STATE TO WS-PREV-PAYEE-STATE

           IF (WS-COMP-KEY NOT = WS-PREV-KEY)
              PERFORM 3600-CREATE-FREEDOM
                                       THRU 3600-EXIT
              MOVE WS-COMP-KEY         TO WS-PREV-KEY
           END-IF

      *    IF EX-BA-VOID-DT NOT = LOW-VALUES
      *       SET PREV-VOID            TO TRUE
      *    END-IF

072110     MOVE 'DFTL'                 TO NLR-LETTER-ID
072110     MOVE 'DEOB'                 TO NLR-EOB-ID
072110     MOVE 'LINT'                 TO NLR-COV-LET-ID
           MOVE SPACES                 TO WS-USER-DEFINED
           MOVE EX-SB-CARRIER          TO WS-USER-CARRIER
072110                                    NLR-CARRIER
           MOVE EX-BA-CLAIM-NO         TO WS-USER-CLAIM-NO
072110                                    NLR-CLAIM-NO
           MOVE EX-BA-CERT-NO          TO WS-USER-CERT-NO
072110                                    NLR-CERT-NO
           MOVE WS-USER-DEFINED        TO TR-USER-DEFINED
072110     MOVE EX-BA-ACCOUNT          TO NLR-ACCT-NO
072110     MOVE EX-BA-ACCOUNT-NAME     TO NLR-ACCT-NAME
072110     MOVE EX-BA-CERT-EFF-DT      TO DC-BIN-DATE-1
072110     MOVE SPACES                 TO DC-OPTION-CODE
072110     PERFORM 8500-DATE-CONVERSION THRU 8590-EXIT
072110     IF DC-ERROR-CODE EQUAL TO SPACES
072110         MOVE DC-GREG-DATE-A-EDIT TO NLR-CERT-EFF-DT
072110     END-IF
072110     MOVE BIN-RUN-DATE           TO DC-BIN-DATE-1
072110     MOVE SPACES                 TO DC-OPTION-CODE
072110     PERFORM 8500-DATE-CONVERSION THRU 8590-EXIT
072110     IF DC-ERROR-CODE EQUAL TO SPACES
072110         MOVE DC-GREG-DATE-A-EDIT TO NLR-DRAFT-DT
072110     END-IF

           MOVE WS-FN-MO               TO WS-INVOICE-MM
           MOVE WS-FN-DA               TO WS-INVOICE-DD
           MOVE EX-BA-CLAIM-NO         TO WS-INVOICE-CLM-NO
           MOVE EX-BA-STATE            TO DR-ACCT-STATE (1)
072110                                    NLR-STATE-CODE           
           MOVE WS-INVOICE-NO          TO TR-INVOICE-NO
                                          CD-INVOICE-NO
                                          DR-INVOICE-NO
                                          PR-INVOICE-NO
                                          AR-INVOICE-NO
                                          VR-INVOICE-NO
           
           COMPUTE WS-PMT-AMT = WS-PMT-AMT + EX-BA-PAYMENT-AMOUNT
           MOVE WS-PMT-AMT             TO TR-INVOICE-AMT
                                          CD-INVOICE-AMT (1)
                                          DR-INVOICE-AMT (1)
                                          AR-INVOICE-AMT (1)
072110                                    NLR-DRAFT-AMOUNT
072110     MOVE AT-INT-RATE            TO NLR-LIFE-INT-RATE
072110     MOVE ZEROS                  TO NLR-DRAFT-NO
072110                                    NLR-DRAFT-ORDER
072110     MOVE EX-BA-CLAIM-TYPE       TO NLR-CLAIM-TYPE
072110     MOVE EX-BA-PAYMENT-TYPE     TO NLR-PMT-TYPE
072110     MOVE 'LIFE'                 TO NLR-PLAN-CODE
                                         
           IF WS-PMT-AMT < ZEROS
              MOVE '-'                 TO TR-INVOICE-AMT-SIGN
                                          CD-INVOICE-AMT-SIGN (1)
                                          DR-INVOICE-AMT-SIGN (1)
                                          DR-INVOICE-AMT-SIGN (2)
           ELSE
              MOVE '+'                 TO TR-INVOICE-AMT-SIGN
                                          CD-INVOICE-AMT-SIGN (1)
                                          DR-INVOICE-AMT-SIGN (1)
                                          AR-INVOICE-AMT-SIGN (1)
           END-IF

           STRING 'CK ' 'CC ' EX-BA-RECORDED-BY ' INT ON LIFE'
              DELIMITED BY SIZE INTO TR-VOUCHER-REF
           END-STRING

           STRING ' INSURED : ' EX-BA-INSURED-LAST-NAME
              DELIMITED BY SIZE INTO CD-DESC (1)
           END-STRING

072110     UNSTRING EX-BA-INSURED-LAST-NAME DELIMITED BY ', ' OR ' '
072110             INTO NLR-INS-LAST-NAME, NLR-INS-FIRST-NAME
072110     END-UNSTRING
072110
           MOVE EX-BA-PAYEES-NAME      TO PR-PAYEE-NAME
                                          VR-VENDOR-NAME (1)
072110                                    NLR-PAYEE-NAME
092010
092010     MOVE EX-BA-PAYEE-TYPE-CD    TO WS-PAYEE-CD-TYPE
092010     MOVE EX-BA-PAYEE-ADDR-SEQ   TO WS-PAYEE-SEQUENCE
092010     MOVE WS-PAYEE-SEQ2          TO WS-PAYEE-CD-SEQ
092010     MOVE WS-PAYEE-CODE          TO NLR-PAYEE-CODE

           IF (NOT PROCESS-RERUN)
              AND (AT-INT-PMT-SELECT-DT = SPACES OR LOW-VALUES)
              MOVE BIN-RUN-DATE        TO AT-INT-PMT-SELECT-DT
      *       MOVE WS-CURRENT-BIN-DT   TO AT-INT-PMT-SELECT-DT
              DISPLAY ' ABOUT TO REWRITE ' WS-COMP-KEY
PEMTST        REWRITE ACTIVITY-TRAILERS
              IF ELTRLR-FILE-STATUS NOT = '00'
                 DISPLAY ' REWRITE PROBLEMS ' ELTRLR-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

072110     MOVE DTE-CLASIC-COMPANY-CD  TO AT-COMPANY-CD
072110     MOVE EX-SB-CARRIER          TO AT-CARRIER
072110     MOVE EX-BA-CLAIM-NO         TO AT-CLAIM-NO
072110     MOVE EX-BA-CERT-NO          TO AT-CERT-NO
072110     MOVE EX-BA-INSURED-ADDR-SEQ TO AT-SEQUENCE-NO
072110     
072110     READ ELTRLR
072110     IF (ELTRLR-FILE-STATUS = '00')
072110        AND (AT-TRAILER-TYPE = '5')
072110          MOVE AT-ADDRESS-LINE-1 TO NLR-INS-ADDR1
072110          MOVE AT-ADDRESS-LINE-2 TO NLR-INS-ADDR2
072110          MOVE AT-CITY           TO NLR-INS-CITY
072110          MOVE AT-STATE          TO NLR-INS-STATE
072110          MOVE AT-ZIP            TO NLR-INS-ZIP
072110     END-IF
072110     MOVE EX-BA-INSURED-SEX-CD   TO NLR-INS-SEX-CODE
072110
           MOVE DTE-CLASIC-COMPANY-CD  TO AT-COMPANY-CD
           MOVE EX-SB-CARRIER          TO AT-CARRIER
           MOVE EX-BA-CLAIM-NO         TO AT-CLAIM-NO
           MOVE EX-BA-CERT-NO          TO AT-CERT-NO
           MOVE EX-BA-PAYEE-ADDR-SEQ   TO AT-SEQUENCE-NO
           
           READ ELTRLR
           IF (ELTRLR-FILE-STATUS = '00')
              AND (AT-TRAILER-TYPE = '5')
092010
092010        MOVE AT-ADDRESS-LINE-1   TO NLR-PAYEE-ADDR1
092010        MOVE AT-ADDRESS-LINE-2   TO NLR-PAYEE-ADDR2
092010        MOVE AT-CITY             TO NLR-PAYEE-CITY
092010        MOVE AT-STATE            TO NLR-PAYEE-STATE
092010        MOVE AT-ZIP              TO NLR-PAYEE-ZIP
092010
              MOVE AT-ADDRESS-LINE-1   TO PR-ADDRESS (1)
                                          VR-VENDOR-ADDRESS (1)
              MOVE AT-ADDRESS-LINE-2   TO PR-ADDRESS (2)
051810        MOVE SPACES              TO PR-ADDRESS (3)
051810        STRING AT-CITY ' ' AT-STATE ' ' AT-ZIP
051810           DELIMITED BY '  ' INTO PR-ADDRESS (3)
051810        END-STRING
      *       PERFORM VARYING S1 FROM +30 BY -1 UNTIL
      *          (S1 < +2)
      *          OR (AT-CITY-STATE (S1:1) NOT = SPACES)
      *       END-PERFORM
      *       IF S1 > +1
      *          IF S1 < +30
      *             ADD +1             TO S1
      *          END-IF
      *          STRING AT-CITY-STATE (1:S1) AT-ZIP DELIMITED BY SIZE
      *             INTO PR-ADDRESS (3)
      *          END-STRING
      *       ELSE
      *          MOVE AT-ZIP           TO PR-ADDRESS (3)
      *       END-IF
              MOVE SPACES              TO PR-ADDRESS (4)
051810        MOVE SPACES              TO VR-VENDOR-ADDRESS (2)
111912*        STRING AT-CITY ' ' AT-STATE
111912*           DELIMITED BY '  ' INTO VR-VENDOR-ADDRESS (2)
111912*        END-STRING
111912        MOVE AT-CITY             TO VR-VENDOR-ADDRESS (2) (1:29)
111912        MOVE AT-STATE            TO VR-VENDOR-ADDRESS (2) (30:2)
111912        MOVE AT-ZIP              TO VR-VENDOR-ADDRESS (2) (32:9)
      *       STRING AT-CITY-STATE AT-ZIP DELIMITED BY SIZE
      *          INTO VR-VENDOR-ADDRESS (2)
      *       END-STRING
              IF PR-PAYEE-NAME = SPACES
                 MOVE AT-MAIL-TO-NAME  TO PR-PAYEE-NAME
              END-IF
              IF PR-ADDRESS (2) = SPACES
                 MOVE PR-ADDRESS (3)   TO PR-ADDRESS (2)
                 MOVE SPACES           TO PR-ADDRESS (3)
              END-IF
091808              
051810        MOVE AT-CITY             TO WS-PAYEE-CITY
051810        MOVE AT-STATE            TO WS-PAYEE-STATE
091808*       MOVE AT-CITY-STATE       TO WS-PAYEE-CITY-STATE              
091808        IF WS-PREV-PAYEE-STATE = LOW-VALUES
091808             MOVE WS-PAYEE-STATE TO WS-PREV-PAYEE-STATE
091808        END-IF
091808                                        
           ELSE
              DISPLAY ' TRAILER RECORD NOT FOUND ' EX-BA-CLAIM-NO
                 ' ' EX-SB-CARRIER ' ' EX-BA-CERT-NO
           END-IF
           
           IF (EX-BA-SOC-SEC-NO (4:1) = '-')
              AND (EX-BA-SOC-SEC-NO (7:1) = '-')
              DISPLAY ' SOC SEC FIX BEFORE ' EX-BA-SOC-SEC-NO
              MOVE EX-BA-SOC-SEC-NO (5:2)
                                       TO EX-BA-SOC-SEC-NO (4:2)
              MOVE EX-BA-SOC-SEC-NO (8:4)
                                       TO EX-BA-SOC-SEC-NO (6:4)
              MOVE ZEROS               TO EX-BA-SOC-SEC-NO (10:2)
              DISPLAY ' SOC SEC FIX AFTER  ' EX-BA-SOC-SEC-NO
           END-IF

           IF EX-BA-SOC-SEC-NO (1:9) NOT NUMERIC
              MOVE ZEROS               TO EX-BA-SOC-SEC-NO (1:9)
           END-IF
           
           MOVE EX-BA-SOC-SEC-NO (1:9) TO AR-TAX-ID-NO
                                          DR-SUSPENSE (1) (2:9)

           .
01013  3500-EXIT.
01014      EXIT.
       3600-CREATE-FREEDOM.

           IF (PREV-VOID)
              OR (WS-PMT-AMT = +0)
              OR (WS-PMT-AMT < +1.00)
              CONTINUE
           ELSE
           ADD 1                       TO WS-REC-GRP-SEQ-NO
           MOVE WS-REC-GRP-CD          TO TR-REC-GRP-CODE
                                          CD-REC-GRP-CODE
                                          DR-REC-GRP-CODE
                                          PR-REC-GRP-CODE
                                          AR-REC-GRP-CODE
                                          VR-REC-GRP-CODE

           WRITE FREEDOM-INT-RECORD    FROM TRANSACTION-RECORD
      *    WRITE FREEDOM-INT-RECORD    FROM CHECK-DES-RECORD
           WRITE FREEDOM-INT-RECORD    FROM DISTRIBUTION-RECORD
           MOVE 'P'                    TO DR-DIST-TYPE (1)
091808*    MOVE '1108124700000000000000'

032212     if dte-client = 'AHL'
032212        MOVE '1108121010000000000000'
032212                                 TO DR-ACCT-NO (1)
032212     else
032212        MOVE '1108124700000000000000'
032212                                 TO DR-ACCT-NO (1)
032212     end-if
           MOVE '00'                   TO DR-ACCT-STATE (1)
           MOVE SPACES                 TO DR-SUSPENSE (1)
           WRITE FREEDOM-INT-RECORD    FROM DISTRIBUTION-RECORD
           WRITE FREEDOM-INT-RECORD    FROM PAYEE-ADDRESS-RECORD
           WRITE FREEDOM-INT-RECORD    FROM ALPHA-1099-RECORD
           WRITE FREEDOM-INT-RECORD    FROM VOUCH-ADDR-1099-RECORD

           MOVE WS-TRANSACTION-RECORD     TO TRANSACTION-RECORD
           MOVE WS-CHECK-DES-RECORD       TO CHECK-DES-RECORD
           MOVE WS-DISTRIBUTION-RECORD    TO DISTRIBUTION-RECORD
           MOVE WS-PAYEE-ADDRESS-RECORD   TO PAYEE-ADDRESS-RECORD
           MOVE WS-ALPHA-1099-RECORD      TO ALPHA-1099-RECORD
           MOVE WS-VOUCH-ADDR-1099-RECORD TO VOUCH-ADDR-1099-RECORD

      *    MOVE WS-DETAIL1             TO PRT
      *    PERFORM WRITE-A-LINE
           
      *    MOVE WS-DETAIL2             TO PRT
      *    PERFORM WRITE-A-LINE

           ADD +1                      TO CT-CURR-LF-PMTS-CNT
           ADD WS-PMT-AMT              TO CT-CURR-LF-PMTS-AMT

072110***NO LONGER NEED TO GENERATE LETTER
072110*    PERFORM 3700-CREATE-CORRESPONDENCE
072110*                                THRU 3700-EXIT
           PERFORM 3650-UPDATE-TOT-INT THRU 3650-EXIT
072110
072110     MOVE 'E'                    TO NLR-LAST-BYTE
072110     INSPECT NAP-LETTER-RECORD REPLACING ALL '|' BY ' '
072110     INSPECT NAP-LETTER-RECORD REPLACING ALL X'A2' BY '|'
072110     WRITE NAPERSOFT-LETTER    FROM NAP-LETTER-RECORD
072110     PERFORM 1050-INIT-NAPER   THRU 1050-EXIT        
072110   
           END-IF

           IF (WS-PMT-AMT < +1.00)
              AND (WS-PMT-AMT NOT = ZEROS)
              MOVE ' ** BELOW MINIMUM ** '
                                       TO WS-D2-PAYEE-ADDR2
              MOVE ' PMT VOIDED  '     TO WS-D2-SSN-COMMENT
              ADD +1                   TO CT-CURR-LF-PMTS-CNT-BM
              ADD WS-PMT-AMT           TO CT-CURR-LF-PMTS-AMT-BM
              PERFORM 3610-VOID-BELOW-MINIMUM
                                       THRU 3610-EXIT
              PERFORM 3620-BUILD-NOTE-TRLR
                                       THRU 3620-EXIT
           END-IF

           IF (PREV-VOID)
              OR (WS-PMT-AMT = +0)
              CONTINUE
           ELSE
              MOVE WS-DETAIL1          TO PRT
              PERFORM WRITE-A-LINE
           
              MOVE WS-DETAIL2          TO PRT
              PERFORM WRITE-A-LINE
           END-IF

           MOVE +0                     TO WS-PMT-AMT
           MOVE ' '                    TO WS-VOID-SW

           .
       3600-EXIT.
           EXIT.

       3610-VOID-BELOW-MINIMUM.

           MOVE DTE-CLASIC-COMPANY-CD  TO AT-COMPANY-CD
           MOVE WS-PREV-KEY            TO AT-CONTROL-PRIMARY (2:19)
           MOVE +100                   TO AT-SEQUENCE-NO

           START ELTRLR KEY >= AT-CONTROL-PRIMARY
           
           IF ELTRLR-FILE-STATUS = '10' OR '23'
              DISPLAY ' PMT NOT FOUND S ' WS-PREV-KEY
              DISPLAY ' OR FINISHED WITH CLAIM ' WS-PREV-KEY
              GO TO 3610-EXIT
           ELSE
              IF ELTRLR-FILE-STATUS NOT = '00'
                 MOVE 'ERROR OCCURED START - ELTRLR'
                                       TO WS-ABEND-MESSAGE
                 MOVE ELTRLR-FILE-STATUS
                                       TO WS-ABEND-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       3610-READ-ELTRLR-NEXT.

           READ ELTRLR NEXT RECORD

           IF ELTRLR-FILE-STATUS = '10' OR '23'
              DISPLAY ' PMT NOT FOUND R ' WS-PREV-KEY
              GO TO 3610-EXIT
           ELSE
              IF ELTRLR-FILE-STATUS NOT = '00'
                 MOVE 'ERROR OCCURED READ  - ELTRLR'
                                       TO WS-ABEND-MESSAGE
                 MOVE ELTRLR-FILE-STATUS
                                       TO WS-ABEND-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           IF AT-CONTROL-PRIMARY (2:19) NOT = WS-PREV-KEY
              DISPLAY ' FINISHED WITH CLM ' WS-PREV-KEY
              GO TO 3610-EXIT
           END-IF

           IF AT-CONTROL-PRIMARY (2:19) = WS-PREV-KEY
              AND (PAYMENT-TR)
              AND (LIFE-INTEREST)
              AND (AT-CHECK-WRITTEN-DT = LOW-VALUES)
              AND (AT-PMT-ACCEPT-DT = LOW-VALUES)
              AND (AT-CHECK-NO = SPACES)
              AND (AT-VOID-DT = LOW-VALUES)
              MOVE BIN-RUN-DATE        TO AT-VOID-DT
              MOVE 'INTEREST PMT BELOW MINIMUM'
                                       TO AT-VOID-REASON
              MOVE BIN-RUN-DATE        TO AT-VOID-SELECT-DT
                                          AT-VOID-ACCEPT-DT
              DISPLAY '**** PAYMENT VOIDED ****' WS-PREV-KEY
              DISPLAY ' REWRITE HERE ' AT-CONTROL-PRIMARY (2:19) 
PEMTST        REWRITE  ACTIVITY-TRAILERS
           END-IF

           GO TO 3610-READ-ELTRLR-NEXT

           .
       3610-EXIT.
           EXIT.

       3620-BUILD-NOTE-TRLR.

           MOVE DTE-CLASIC-COMPANY-CD  TO CL-COMPANY-CD
           MOVE WS-PREV-KEY            TO CL-CONTROL-PRIMARY (2:19)

           READ ELMSTR
           
           IF ELMSTR-FILE-STATUS = '00'
              SUBTRACT +1              FROM CL-TRAILER-SEQ-CNT
              MOVE SPACES              TO ACTIVITY-TRAILERS
              MOVE CL-CONTROL-PRIMARY  TO AT-CONTROL-PRIMARY
              MOVE CL-TRAILER-SEQ-CNT  TO AT-SEQUENCE-NO
              MOVE 'AT'                TO AT-RECORD-ID
              MOVE '6'                 TO AT-TRAILER-TYPE
              MOVE BIN-RUN-DATE        TO AT-RECORDED-DT
                                          AT-GEN-INFO-LAST-MAINT-DT
              MOVE 'AUTO'              TO AT-RECORDED-BY
                                          AT-GEN-INFO-LAST-UPDATED-BY
              MOVE +180000             TO AT-LAST-MAINT-HHMMSS
              MOVE 'CALCULATED INTEREST IS LESS THAN MINIMUM'
                                       TO AT-INFO-LINE-1
              MOVE 'ENTRIES VOIDED AND INTEREST NOT PAID    '
                                       TO AT-INFO-LINE-2
              WRITE ACTIVITY-TRAILERS
              IF ELTRLR-FILE-STATUS = '00'
                 REWRITE CLAIM-MASTER
                 IF ELMSTR-FILE-STATUS NOT = '00'
                    DISPLAY ' ERROR - ELMSTR - REWRITE NOTE TRLR '
                       WS-PREV-KEY
                    PERFORM ABEND-PGM
                 END-IF
              END-IF
           ELSE
              DISPLAY ' CLAIM NOT FOUND ' WS-PREV-KEY
              PERFORM ABEND-PGM
           END-IF

           .
       3620-EXIT.
           EXIT.

       3650-UPDATE-TOT-INT.

           MOVE DTE-CLASIC-COMPANY-CD  TO CL-COMPANY-CD
           MOVE WS-PREV-KEY            TO CL-CONTROL-PRIMARY (2:19)

           READ ELMSTR
           
           IF ELMSTR-FILE-STATUS = '00'
              IF CL-TOTAL-INT-PAID NOT NUMERIC
                 MOVE +0               TO CL-TOTAL-INT-PAID
              END-IF
              COMPUTE CL-TOTAL-INT-PAID = CL-TOTAL-INT-PAID + WS-PMT-AMT
              REWRITE CLAIM-MASTER
              IF ELMSTR-FILE-STATUS NOT = '00'
                 DISPLAY ' ERROR - ELMSTR - REWRITE TOT INT '
                    WS-PREV-KEY
                 PERFORM ABEND-PGM
              END-IF
           ELSE
              DISPLAY ' CLAIM NOT FOUND ' WS-PREV-KEY
              PERFORM ABEND-PGM
           END-IF

           .
       3650-EXIT.
           EXIT.

       3700-CREATE-CORRESPONDENCE.

           MOVE DTE-CLASIC-COMPANY-CD  TO AQ-COMPANY-CD
           MOVE WS-PREV-KEY            TO AQ-CONTROL-PRIMARY (2:19)

           READ ELACTQ

           EVALUATE ELACTQ-FILE-STATUS
              WHEN '23'
                 PERFORM 3710-ADD-ELACTQ
                                       THRU 3710-EXIT
              WHEN '00'
                 PERFORM 3720-UPDATE-ELACTQ
                                       THRU 3720-EXIT
              WHEN OTHER
                 DISPLAY ' ERROR ON ELACTQ - READ ' ELACTQ-FILE-STATUS
                 PERFORM ABEND-PGM
           END-EVALUATE

           .
       3700-EXIT.
           EXIT.

       3710-ADD-ELACTQ.

           MOVE 'AQ'                   TO  ACTIVITY-QUE

           MOVE DTE-CLASIC-COMPANY-CD  TO AQ-COMPANY-CD
           MOVE WS-PREV-KEY            TO AQ-CONTROL-PRIMARY (2:19)
           MOVE '1'                    TO AQ-PENDING-LETTER-FLAG
           MOVE +0                     TO AQ-PAYMENT-COUNTER
                                          AQ-PMT-UNAPPROVED-COUNT

           MOVE LOW-VALUES             TO AQ-RESEND-DATE
                                          AQ-FOLLOWUP-DATE

           MOVE +3170                  TO AQ-LAST-UPDATED-BY
           MOVE 'LIIN'                 TO AQ-AUTO-LETTER

PEMTST     DISPLAY ' ABOUT TO ADD LIIN LETTER ' WS-PREV-KEY
PEMTST*    MOVE '00' TO ELACTQ-FILE-STATUS
PEMTST     WRITE ACTIVITY-QUE

           EVALUATE ELACTQ-FILE-STATUS
              WHEN '00'
                 DISPLAY ' LIIN LETTER ADDED FOR ' WS-PREV-KEY
              WHEN OTHER
                 DISPLAY ' ERROR ON ELACTQ - WRITE ' ELACTQ-FILE-STATUS
                 PERFORM ABEND-PGM
           END-EVALUATE

           .
       3710-EXIT.
           EXIT.

       3720-UPDATE-ELACTQ.

           IF (AQ-PENDING-LETTER-FLAG = '1')
              AND (AQ-AUTO-LETTER NOT = SPACES)
              DISPLAY ' LETTER ALREADY QUEUED ' WS-PREV-KEY
           ELSE
              MOVE '1'                 TO AQ-PENDING-LETTER-FLAG
              MOVE +3170               TO AQ-LAST-UPDATED-BY
              MOVE 'LIIN'              TO AQ-AUTO-LETTER

              MOVE LOW-VALUES          TO AQ-RESEND-DATE
                                       AQ-FOLLOWUP-DATE

PEMTST        DISPLAY ' ABOUT TO UPDATE LIIN LETTER ' WS-PREV-KEY
PEMTST*       MOVE '00' TO ELACTQ-FILE-STATUS
PEMTST        REWRITE ACTIVITY-QUE

              EVALUATE ELACTQ-FILE-STATUS
                 WHEN '00'
                    DISPLAY ' LIIN LETTER UPDATED FOR ' WS-PREV-KEY
                 WHEN OTHER
                    DISPLAY ' ERROR ON ELACTQ - REWRITE '
                       ELACTQ-FILE-STATUS
                    PERFORM ABEND-PGM
              END-EVALUATE
           END-IF

           .
       3720-EXIT.
           EXIT.

01191  4000-PRINT-TOTALS.
01192                                                                   EL317
01193      MOVE +99                    TO  WS-LINE-COUNT.               EL317
01194      MOVE +1                     TO  WS-HEADING-SW.               EL317
01195                                                                   EL317
01196      IF WS-REPORT-SW = +1                                         EL317
01197          MOVE SPACES             TO  WS-H2-CARRIER                EL317
01198          MOVE 'ALL CARRIERS '    TO  WS-H3-CARRIER-NAME           EL317
01199          MOVE '0 * * * * * * * GRAND TOTALS * * * * * * *'        EL317
01200                                  TO  WS-HEADING6.                 EL317

           MOVE '0 TOTAL PAYMENTS INTERFACED '
                                       TO WS-TOTAL-LINE1
01351 *    MOVE SPACES                 TO WS-T1-TOTAL-DESCRIPTION
01353      MOVE CT-CURR-LF-PMTS-AMT    TO WS-T1-CURR-AMOUNT
01355      MOVE CT-CURR-LF-PMTS-CNT    TO WS-T1-CURR-COUNT
01357      MOVE WS-TOTAL-LINE1         TO PRT
01358      PERFORM WRITE-A-LINE

           MOVE '  TOTAL PAYMENTS BELOW MINIMUM '
                                       TO WS-TOTAL-LINE1
           MOVE CT-CURR-LF-PMTS-AMT-BM TO WS-T1-CURR-AMOUNT
           MOVE CT-CURR-LF-PMTS-CNT-BM TO WS-T1-CURR-COUNT
           MOVE WS-TOTAL-LINE1         TO PRT
           PERFORM WRITE-A-LINE

           DISPLAY ' VOIDS SKIPPED ' WS-SKIP-CNT
      *    MOVE ' </PRE>'              TO PRT
      *    PERFORM WRITE-A-LINE
      *    MOVE ' </HEAD>'             TO PRT
      *    PERFORM WRITE-A-LINE
      *    MOVE ' </HTML>'             TO PRT
      *    PERFORM WRITE-A-LINE

           .
01554  4900-EXIT.                                                       EL317
01555      EXIT.                                                        EL317
01556                                                                   EL317
01821  8100-GET-CARRIER-NAME.
01822                                                                   EL317
01823      MOVE +1                     TO  WS-INDEX.                    EL317
01824      MOVE WS-LAST-CARRIER        TO  WS-H2-CARRIER.               EL317
01825                                                                   EL317
01826  8110-GET-CARRIER-NAME.                                           EL317
01827                                                                   EL317
01828      IF WS-LAST-CARRIER = CARRIER-SUB (WS-INDEX)                  EL317
01829          MOVE CARRIER-PIC (WS-INDEX) TO WS-H3-CARRIER-NAME        EL317
01830      ELSE                                                         EL317
01831          IF WS-INDEX < +25                                        EL317
01832              ADD +1  TO  WS-INDEX                                 EL317
01833              GO TO 8110-GET-CARRIER-NAME.                         EL317
01834                                                                   EL317
01835  8190-EXIT.                                                       EL317
01836      EXIT.                                                        EL317
01837                                                                   EL317
01838  8500-DATE-CONVERSION. COPY ELCDCS.
01839                                                                   EL317
01840  WRITE-A-LINE SECTION.         COPY ELCWAL.                       EL317
01841                                                                   EL317
01842  WRITE-HEADINGS SECTION.                                          EL317
01843  WHS-010.                                                         EL317
01844      IF LCP-ONCTR-01 =  0                                         EL317
01845          ADD 1 TO LCP-ONCTR-01                                    EL317
01846          MOVE WS-CURRENT-DATE    TO  WS-H2-DATE                   EL317
01847          MOVE COMPANY-NAME       TO  WS-H2-CLIENT-NAME            EL317
01848          MOVE ALPH-DATE          TO  WS-H3-DATE.                  EL317
01849                                                                   EL317
01850      ADD +1  TO  WS-PAGE.                                         EL317
01851      MOVE WS-PAGE                TO  WS-H3-PAGE.                  EL317
01852      MOVE PRT                    TO  WS-SAVE-PRINT-RECORD.        EL317
01853      MOVE ZERO                   TO  WS-LINE-COUNT.               EL317
01854                                                                   EL317
01855      MOVE WS-HEADING1            TO  PRT.                         EL317
01856      MOVE '1'                    TO  X.                           EL317
01857      PERFORM WRITE-PRINTER.                                       EL317
01858                                                                   EL317
01859      MOVE WS-HEADING2            TO  PRT.                         EL317
01860      MOVE ' '                    TO  X.                           EL317
01861      PERFORM WRITE-PRINTER.                                       EL317
01862                                                                   EL317
01863      MOVE WS-HEADING3            TO  PRT.                         EL317
01864      MOVE ' '                    TO  X.                           EL317
01865      PERFORM WRITE-PRINTER.                                       EL317
01866                                                                   EL317
01867      MOVE WS-HEADING4            TO  PRT.                         EL317
01868      MOVE ' '                    TO  X.                           EL317
01869      PERFORM WRITE-PRINTER.                                       EL317
01870                                                                   EL317
01871      MOVE WS-HEADING5            TO  PRT.                         EL317
01872      PERFORM WRITE-PRINTER.                                       EL317
01873                                                                   EL317
01874      MOVE +10                    TO  WS-LINE-COUNT.               EL317
01875                                                                   EL317
01876      IF WS-HEADING-SW NOT = ZERO                                  EL317
01877          MOVE WS-HEADING6        TO  PRT                          EL317
01878          PERFORM WRITE-PRINTER.                                   EL317
01879                                                                   EL317
01888      IF WS-HEADING-SW NOT = ZERO                                  EL317
01889          MOVE WS-HEADING8    TO  PRT                              EL317
01890          PERFORM WRITE-PRINTER                                    EL317
01891          MOVE WS-HEADING9    TO  PRT                              EL317
01892          PERFORM WRITE-PRINTER                                    EL317
01893          MOVE +15            TO  WS-LINE-COUNT.                   EL317
01894                                                                   EL317
01895      IF WS-HEADING-SW = +2 OR +3                                  EL317
01896          ADD +5  TO  WS-LINE-COUNT.                               EL317
01897                                                                   EL317
01898  WHS-020. COPY ELCWHS2.                                           EL317
01899                                                                   EL317
01900  WRITE-PRINTER SECTION. COPY ELCWPS.                              EL317
01901                                                                   EL317
01902  WPS-020. COPY ELCPRT2.
01903                                                                   EL317
01904  OPEN-FILES SECTION.

01907      OPEN INPUT REPORTS-EXTRACT-FILE
           OPEN I-O   ELTRLR ELACTQ ELMSTR
01908           OUTPUT FREEDOM-INT-FILE
072110                 NAPERSOFT-LETTERS
01909                  PRNTR.                                           EL317

           IF ELTRLR-FILE-STATUS NOT = '00' AND '97'
              DISPLAY ' ELTRLR - ERROR - OPEN  ' ELTRLR-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ELMSTR-FILE-STATUS NOT = '00' AND '97'
              DISPLAY ' ELMSTR - ERROR - OPEN  ' ELMSTR-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ELACTQ-FILE-STATUS NOT = '00' AND '97'
              DISPLAY ' ELACTQ - ERROR - OPEN  ' ELACTQ-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

01931      MOVE RUN-MO                  TO  DC-MDY-MONTH                   CL**2
01933      MOVE RUN-DA                  TO  DC-MDY-DAY                     CL**2
01935      MOVE RUN-YR                  TO  DC-MDY-YEAR                    CL**2
01937      MOVE '4'                     TO  DC-OPTION-CODE.                CL**2
01938      PERFORM 8500-DATE-CONVERSION.                                EL317
01951                                                                   EL317
01956                                                                   EL317
01972                                                                   EL317
01973  OFS-EXIT.                                                        EL317
01974      EXIT.                                                        EL317
01975                                                                   EL317
01976  CLOSE-FILES.
01977                                                                   EL317
01978  CFS-010. COPY ELCPRTC.

01980      CLOSE REPORTS-EXTRACT-FILE ELTRLR ELACTQ
01981            FREEDOM-INT-FILE ELMSTR
072110           NAPERSOFT-LETTERS
01982            PRNTR.                                                 EL317

           IF ELTRLR-FILE-STATUS NOT = '00'
              DISPLAY ' ELTRLR - ERROR - CLOSE ' ELTRLR-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ELMSTR-FILE-STATUS NOT = '00'
              DISPLAY ' ELMSTR - ERROR - CLOSE ' ELMSTR-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ELACTQ-FILE-STATUS NOT = '00'
              DISPLAY ' ELACTQ - ERROR - CLOSE ' ELACTQ-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

          .
01987  CFS-EXIT.                                                        EL317
01988      EXIT.                                                        EL317
01989                                                                   EL317
01990  ABEND-PGM SECTION. COPY ELCABEND.                                EL317
01991                                                                   EL317
