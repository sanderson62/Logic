00001  IDENTIFICATION DIVISION.                                         04/18/98
00002                                                                   EL317
00003  PROGRAM-ID.                 EL317 .                                 LV003
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
092602* 092602    2002091900008  PEMA  INCREASE NUMBER OF MAXIMUM
092602*                                  BENEFIT CODES FROM 300 TO 900
122402* 122402    2001061800003  SMVA  ADD PROCESSING FOR NEW CLM TYP I
122402*                                FOR DCC
071403* 071403    2003031400004  SMVA  ADD PAID THRU DATE TO REPORT 
051104* 051104                   SMVA  ADD REPORT CODE 2 TO REPORT 
052804* 052804  CR2004051200002  SMVA  ADD REPORT CODE 1 TO REPORT 
121203* 121203    2003080800002  SMVA  ADD PROCESSING FOR NEW CLM TYP G
022106* 022106    2004040700004  PEMA  ADD LIFE CLAIM INTEREST
052912* 052912  IR2012052400001  PEMA  CORRECT PMT 6 CODING
052614* 052614  CR2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
070714* 070714  CR2013060600001  PEMA  AUTOMATE MONTH END BALANCING
060115* 060115  IR2015060200001  PEMA  ADD IU AND FL CLAIM TYPES
010716* 010716  CR2015082500001  PEMA  VPP CHANGES
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
122018* 122018  IR2018120300001  TANA  INCLUDE OTHER CLAIM IN ME CHKPT
022122* 022122  CR2021100800003  PEMA  Add B and H claim types
092602******************************************************************
00035                                                                   EL317
00036      EJECT                                                        EL317
00037  ENVIRONMENT DIVISION.                                            EL317
00038  CONFIGURATION SECTION.                                           EL317
00039                                                                   EL317
00040  INPUT-OUTPUT SECTION.                                            EL317
00041                                                                   EL317
00042  FILE-CONTROL.                                                    EL317
00043                                                                   EL317
00044      SELECT REPORTS-EXTRACT-FILE                                  EL317
00045                                ASSIGN TO SYS010-UT-2400-S-SYS010. EL317
00046                                                                   EL317
00047      SELECT DISK-DATE          ASSIGN TO SYS019-FBA1-S-SYS019.    EL317
00048      SELECT PRNTR              ASSIGN TO SYS008-UR-1403-S-SYS008. EL317
00049      SELECT FICH               ASSIGN TO SYS020-UT-2400-S-SYS020. EL317
00050      SELECT ABC-INTERFACE-FILE ASSIGN TO SYS021-UT-FBA1-S-SYS021.
           SELECT RPT-EXTRACT        ASSIGN TO SYS022
               ORGANIZATION LINE SEQUENTIAL.
00051                                                                   EL317
00052      SELECT GL-DAILY-CLAIM-PAYMENTS                               EL317
00053                                ASSIGN TO SYS011-UT-FBA1-S-SYS011. EL317
00054                                                                   EL317
00055      SELECT ELREPT             ASSIGN TO SYS018-FBA1-ELREPT       EL317
00056                                ORGANIZATION IS INDEXED            EL317
00057                                ACCESS IS DYNAMIC                  EL317
00058                                RECORD KEY IS RF-CONTROL-PRIMARY   EL317
00059                                FILE STATUS IS DTE-VSAM-FLAGS.     EL317
00060                                                                   EL317
070714     SELECT  ERMEBL          ASSIGN SYS024-FBA1-ERMEBL      
070714                             ORGANIZATION INDEXED           
070714                             ACCESS DYNAMIC                 
070714                             RECORD KEY ME-CONTROL-PRIMARY  
070714                             FILE STATUS ERMEBL-FILE-STATUS.
00061      EJECT                                                        EL317
00062  DATA DIVISION.                                                   EL317
00063                                                                   EL317
00064  FILE SECTION.                                                    EL317
00065                                                                   EL317
00066  FD  REPORTS-EXTRACT-FILE   COPY ELCEXTFD.                        EL317
00067                                                                   EL317
00068                             COPY ELCEXTR.                         EL317
00069                                                                   EL317
00070      EJECT                                                        EL317
00071  FD  ABC-INTERFACE-FILE                                           EL317
00072      BLOCK CONTAINS 0 RECORDS
00073      RECORDING MODE F.                                            EL317
00074                                                                   EL317
00075  01  ABC-INTERFACE-RECORD.                                        EL317
00076      05  ABC-CONSTANT                PIC X(11).                   EL317
00077      05  ABC-DATE.                                                EL317
00078          10  ABC-DATE-MONTH          PIC XX.                         CL**2
00079          10  ABC-DATE-DAY            PIC XX.                         CL**2
00080          10  ABC-DATE-YEAR           PIC XX.                         CL**2
00081      05  ABC-ACCOUNT                 PIC X(7).                    EL317
00082 *        88  LIFE BENEFITS                        VALUE '5071000'.EL317
00083 *        88  A&H BENEFITS                         VALUE '5072000'.EL317
00084 *        88  CASH                                 VALUE '1266000'.EL317
00085      05  FILLER                      PIC X(21).                   EL317
00086      05  ABC-DESC                    PIC X(20).                   EL317
00087      05  ABC-AMOUNT                  PIC 9(8)V99.                 EL317
00088      05  ABC-DEBIT-CREDIT            PIC X.                       EL317
00089 *        88  DEBIT                                VALUE 'D'.      EL317
00090 *        88  CREDIT                               VALUE 'C'.      EL317
00091                                                                   EL317
00092      05  FILLER                      PIC X(4).                    EL317
00093                                                                   EL317

       FD  RPT-EXTRACT
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE F.

       01  RPT-EXTRACT-RECORD          PIC X(350).

00095  FD  GL-DAILY-CLAIM-PAYMENTS                                      EL317
00096      BLOCK CONTAINS 0 RECORDS
00097      RECORDING MODE F.                                            EL317
00098                                                                   EL317
00099                              COPY GLCCLMS.                        EL317
00100      EJECT                                                        EL317
00101  FD  DISK-DATE               COPY ELCDTEFD.                       EL317
00102                                                                   EL317
00103      EJECT                                                        EL317
00104  FD  PRNTR                   COPY ELCPRTFD.                       EL317
00105                                                                   EL317
00106      EJECT                                                        EL317
00107  FD  FICH                    COPY ELCFCHFD.                       EL317
00108                                                                   EL317
00109      EJECT                                                        EL317
00110  FD  ELREPT.                                                      EL317
00111                              COPY ELCREPT.                        EL317
070714 FD  ERMEBL.
070714                                 COPY ERCMEBL.
00112                                                                   EL317
00114  WORKING-STORAGE SECTION.                                         EL317
00115  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      EL317
00116  77  LCP-ONCTR-01                  PIC S9(8) COMP-3 VALUE ZERO.   EL317
00117  77  LCP-ASA                       PIC X.                         EL317
00118                                                                   EL317
00119  77  FILLER  PIC X(32)   VALUE '********************************'.EL317
00120  77  FILLER  PIC X(32)   VALUE '*     EL317  WORKING STORAGE   *'.EL317
00121  77  FILLER  PIC X(32)   VALUE '********* VMOD=2.020 ***********'.EL317
00122                                                                   EL317
       01  WS-INIT-EXTRACT             PIC X(350).
       01  EXTRACT-RECORD.
           05  EXT-CHECK-NO            PIC X(7).
           05  EXT-DEL01               PIC X.
           05  EXT-AMOUNT              PIC ------9.99.
           05  EXT-DEL02               PIC X.
           05  EXT-DATE-PAID           PIC X(10).
           05  EXT-DEL03               PIC X.
           05  EXT-PAY-TYPE1           PIC XX.
           05  EXT-DEL04               PIC X.
           05  EXT-PAY-TYPE2           PIC X(8).
           05  EXT-DEL05               PIC X.
           05  EXT-PAYEE-NAME          PIC X(25).
           05  EXT-DEL06               PIC X.
           05  EXT-CLAIM-NO            PIC X(7).
           05  EXT-DEL07               PIC X.
           05  EXT-INCURRED-DATE       PIC X(10).
           05  EXT-DEL08               PIC X.
           05  EXT-CARRIER             PIC X.
           05  EXT-DEL09               PIC X.
           05  EXT-CERT-NO             PIC X(11).
           05  EXT-DEL10               PIC X.
           05  EXT-EFF-DATE            PIC X(10).
           05  EXT-DEL11               PIC X.
           05  EXT-STATE               PIC XX.
           05  EXT-DEL12               PIC X.
           05  EXT-ACCOUNT             PIC X(10).
           05  EXT-DEL13               PIC X.
           05  EXT-GROUP               PIC X(6).
           05  EXT-DEL14               PIC X.
           05  EXT-ORIGIN              PIC X(10).
           05  EXT-DEL15               PIC X.
           05  EXT-PAYEE-TYPE          PIC X(25).
           05  EXT-DEL16               PIC X.
           05  EXT-LAST-NAME           PIC X(20).
           05  EXT-DEL17               PIC X.
           05  EXT-PAID-THRU-DT        PIC X(10).
           05  EXT-DEL18               PIC X.
           05  EXT-REPORT-CODE-1       PIC X(10).
           05  EXT-DEL19               PIC X.
           05  EXT-REPORT-CODE-2       PIC X(10).
           05  EXT-DEL20               PIC X.
           05  EXT-MESSAGE             PIC X(15).
           05  EXT-DEL21               PIC X.
           05  EXT-RPT-DATE            PIC X(10).
           05  EXT-DEL22               PIC X.
           05  EXT-EOR                 PIC X.
           
070714****=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=***
070714****                                                           ***
070714****   Month end balancing work area                           ***
070714****                                                           ***
070714****=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=***
070714
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
070714     12  hld-317-clms-tot    pic s9(9)v99 comp-3 value +0.

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
00139                                                                   EL317
00140      EJECT                                                        EL317
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
00173                                                                   EL317
00174      EJECT                                                        EL317
00175  01  WS-3800-CARD.                                                EL317
00176      12  3800-LITERAL            PIC X(6)    VALUE '0LG00 '.      EL317
00177      12  3800-DATE.                                               EL317
00178          16  3800-MONTH          PIC 99.                          EL317
00179          16  3800-DAY            PIC 99.                          EL317
00180          16  3800-YEAR           PIC 99.                          EL317
00181      12  3800-VOUCHER-ID         PIC X(5)    VALUE '*****'.       EL317
00182      12  3800-JULIAN-DATE.                                        EL317
00183          16  3800-JULIAN-YEAR    PIC 99.                          EL317
00184          16  3800-JULIAN-DAY     PIC S999.                        EL317
00185      12  3800-GENERAL-LEDGER     PIC X(7).                        EL317
00186      12  3800-U-LITERAL          PIC X       VALUE 'U'.           EL317
00187      12  3800-AMOUNT             PIC 9(7)V99.                     EL317
00188      12  3800-DEBIT-CREDIT       PIC X.                           EL317
00189      12  3800-CHECK-NUMBER       PIC 9(6).                        EL317
00190      12  3800-DESCRIPTION.                                        EL317
00191          16  3800-ACCOUNT        PIC X(6).                        EL317
00192          16  3800-CERT-NO        PIC X(6)    JUSTIFIED RIGHT.     EL317
00193          16  3800-DESC-L-H       PIC X.                           EL317
00194          16  3800-DESC-C-U-P     PIC X       VALUE 'C'.           EL317
00195          16  FILLER              PIC X(4)    VALUE SPACES.        EL317
00196          16  3800-SYSTEM         PIC X       VALUE 'C'.           EL317
00197      12  FILLER                  PIC X(5)    VALUE SPACES.        EL317
00198      12  3800-STATE              PIC XX      VALUE SPACES.        EL317
00199      12  3800-COMPANY            PIC XXX.                         EL317
00200      12  FILLER                  PIC X       VALUE SPACES.        EL317
00201      12  3800-MISC-MONTH-YEAR    PIC X(4)    VALUE '0000'.        EL317
00202                                                                   EL317
00203      EJECT                                                        EL317
00204  01  FILLER.                                                      EL317
00205    02  TOTALS-AREA.                                               EL317
122402*****Index1 area   Life
00206      05  WS-TA-LIFE-OVERIDE-L6       PIC X(6)        VALUE SPACE. EL317
00207      05  FILLER                      PIC X(24)       VALUE        EL317
00208          ' PAYMENTS'.                                             EL317
00209      05  FILLER                      COMP-3.                      EL317
00210          10  CT-CURR-LF-PMTS-AMT           PIC S9(9)V99  VALUE +0.EL317
00211          10  CT-CURR-LF-PMTS-CNT           PIC S9(7)     VALUE +0.EL317
00212                                                                   EL317
00213          10  FT-CURR-LF-PMTS-AMT           PIC S9(9)V99  VALUE +0.EL317
00214          10  FT-CURR-LF-PMTS-CNT           PIC S9(7)     VALUE +0.EL317
00215                                                                   EL317
00216          10  CT-CURR-LF-PMTS-VOIDED-AMT    PIC S9(9)V99  VALUE +0.EL317
00217          10  CT-CURR-LF-PMTS-VOIDED-CNT    PIC S9(7)     VALUE +0.EL317
00218                                                                   EL317
00219          10  FT-CURR-LF-PMTS-VOIDED-AMT    PIC S9(9)V99  VALUE +0.EL317
00220          10  FT-CURR-LF-PMTS-VOIDED-CNT    PIC S9(7)     VALUE +0.EL317

122402         10  FILLER                        PIC S9(9)V99  VALUE +0.EL317
122402         10  FILLER                        PIC S9(7)     VALUE +0.EL317
00218                                                                   EL317
122402         10  FILLER                        PIC S9(9)V99  VALUE +0.EL317
122402         10  FILLER                        PIC S9(7)     VALUE +0.EL317
121203                                                                  EL317
121203         10  FILLER                        PIC S9(9)V99  VALUE +0.EL317
121203         10  FILLER                        PIC S9(7)     VALUE +0.EL317
121203                                                                  EL317
121203         10  FILLER                        PIC S9(9)V99  VALUE +0.EL317
121203         10  FILLER                        PIC S9(7)     VALUE +0.EL317
052614
052614         10  FILLER                        PIC S9(9)V99  VALUE +0.
052614         10  FILLER                        PIC S9(7)     VALUE +0.
052614
052614         10  FILLER                        PIC S9(9)V99  VALUE +0.
052614         10  FILLER                        PIC S9(7)     VALUE +0.
100518
022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.
022122
022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.
022122
022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.
022122
022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.
100518
100518         10  FILLER                        PIC S9(9)V99  VALUE +0.
100518         10  FILLER                        PIC S9(7)     VALUE +0.
100518
100518         10  FILLER                        PIC S9(9)V99  VALUE +0.
100518         10  FILLER                        PIC S9(7)     VALUE +0.
00221                                                                   EL317
122402*****Index2 area   ah
00222      05  WS-TA-AH-OVERIDE-L6         PIC X(6)        VALUE SPACE. EL317
00223      05  FILLER                      PIC X(24)       VALUE        EL317
00224          ' PAYMENTS'.                                             EL317
00225      05  FILLER                      COMP-3.                      EL317
00226          10  CT-CURR-AH-PMTS-AMT           PIC S9(9)V99  VALUE +0.EL317
00227          10  CT-CURR-AH-PMTS-CNT           PIC S9(7)     VALUE +0.EL317
00228                                                                   EL317
00229          10  FT-CURR-AH-PMTS-AMT           PIC S9(9)V99  VALUE +0.EL317
00230          10  FT-CURR-AH-PMTS-CNT           PIC S9(7)     VALUE +0.EL317
00231                                                                   EL317
00232          10  CT-CURR-AH-PMTS-VOIDED-AMT    PIC S9(9)V99  VALUE +0.EL317
00233          10  CT-CURR-AH-PMTS-VOIDED-CNT    PIC S9(7)     VALUE +0.EL317
00234                                                                   EL317
00235          10  FT-CURR-AH-PMTS-VOIDED-AMT    PIC S9(9)V99  VALUE +0.EL317
00236          10  FT-CURR-AH-PMTS-VOIDED-CNT    PIC S9(7)     VALUE +0.EL317
00231                                                                   EL317
122402         10  FILLER                        PIC S9(9)V99  VALUE +0.EL317
122402         10  FILLER                        PIC S9(7)     VALUE +0.EL317
00234                                                                   EL317
122402         10  FILLER                        PIC S9(9)V99  VALUE +0.EL317
122402         10  FILLER                        PIC S9(7)     VALUE +0.EL317
121203                                                                  EL317
121203         10  FILLER                        PIC S9(9)V99  VALUE +0.EL317
121203         10  FILLER                        PIC S9(7)     VALUE +0.EL317
121203                                                                  EL317
121203         10  FILLER                        PIC S9(9)V99  VALUE +0.EL317
121203         10  FILLER                        PIC S9(7)     VALUE +0.EL317
052614
052614         10  FILLER                        PIC S9(9)V99  VALUE +0.
052614         10  FILLER                        PIC S9(7)     VALUE +0.
052614
052614         10  FILLER                        PIC S9(9)V99  VALUE +0.
052614         10  FILLER                        PIC S9(7)     VALUE +0.
100518
022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.
022122
022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.
022122
022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.
022122
022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.

100518         10  FILLER                        PIC S9(9)V99  VALUE +0.
100518         10  FILLER                        PIC S9(7)     VALUE +0.
100518
100518         10  FILLER                        PIC S9(9)V99  VALUE +0.
100518         10  FILLER                        PIC S9(7)     VALUE +0.
00237                                                                   EL317
122402*****Index3 area DCC only  iu
122402     05  WS-TA-IU-OVERIDE-L6         PIC X(6)        VALUE SPACE. EL317
122402     05  FILLER                      PIC X(24)       VALUE        EL317
122402         ' PAYMENTS'.                                             EL317
122402     05  FILLER                      COMP-3.                      EL317
122402         10  CT-CURR-IU-PMTS-AMT           PIC S9(9)V99  VALUE +0.EL317
122402         10  CT-CURR-IU-PMTS-CNT           PIC S9(7)     VALUE +0.EL317
122402                                                                  EL317
122402         10  FT-CURR-IU-PMTS-AMT           PIC S9(9)V99  VALUE +0.EL317
122402         10  FT-CURR-IU-PMTS-CNT           PIC S9(7)     VALUE +0.EL317
122402                                                                  EL317
122402         10  CT-CURR-IU-PMTS-VOIDED-AMT    PIC S9(9)V99  VALUE +0.EL317
122402         10  CT-CURR-IU-PMTS-VOIDED-CNT    PIC S9(7)     VALUE +0.EL317
122402                                                                  EL317
122402         10  FT-CURR-IU-PMTS-VOIDED-AMT    PIC S9(9)V99  VALUE +0.EL317
122402         10  FT-CURR-IU-PMTS-VOIDED-CNT    PIC S9(7)     VALUE +0.EL317
122402                                                                  EL317
122402         10  FILLER                        PIC S9(9)V99  VALUE +0.EL317
122402         10  FILLER                        PIC S9(7)     VALUE +0.EL317
122402                                                                  EL317
122402         10  FILLER                        PIC S9(9)V99  VALUE +0.EL317
122402         10  FILLER                        PIC S9(7)     VALUE +0.EL317
121203                                                                  EL317
121203         10  FILLER                        PIC S9(9)V99  VALUE +0.EL317
121203         10  FILLER                        PIC S9(7)     VALUE +0.EL317
121203                                                                  EL317
121203         10  FILLER                        PIC S9(9)V99  VALUE +0.EL317
121203         10  FILLER                        PIC S9(7)     VALUE +0.EL317
052614
052614         10  FILLER                        PIC S9(9)V99  VALUE +0.
052614         10  FILLER                        PIC S9(7)     VALUE +0.
052614
052614         10  FILLER                        PIC S9(9)V99  VALUE +0.
052614         10  FILLER                        PIC S9(7)     VALUE +0.
100518
022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.
022122
022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.
022122
022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.
022122
022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.

100518         10  FILLER                        PIC S9(9)V99  VALUE +0.
100518         10  FILLER                        PIC S9(7)     VALUE +0.
100518
100518         10  FILLER                        PIC S9(9)V99  VALUE +0.
100518         10  FILLER                        PIC S9(7)     VALUE +0.
00237                                                                   EL317
121203*****Index4 area DCC only  gap
121203     05  WS-TA-GP-OVERIDE-L6         PIC X(6)        VALUE SPACE. EL317
121203     05  FILLER                      PIC X(24)       VALUE        EL317
121203         ' PAYMENTS'.                                             EL317
121203     05  FILLER                      COMP-3.                      EL317
121203         10  CT-CURR-GP-PMTS-AMT           PIC S9(9)V99  VALUE +0.EL317
121203         10  CT-CURR-GP-PMTS-CNT           PIC S9(7)     VALUE +0.EL317
121203                                                                  EL317
121203         10  FT-CURR-GP-PMTS-AMT           PIC S9(9)V99  VALUE +0.EL317
121203         10  FT-CURR-GP-PMTS-CNT           PIC S9(7)     VALUE +0.EL317
121203                                                                  EL317
121203         10  CT-CURR-GP-PMTS-VOIDED-AMT    PIC S9(9)V99  VALUE +0.EL317
121203         10  CT-CURR-GP-PMTS-VOIDED-CNT    PIC S9(7)     VALUE +0.EL317
121203                                                                  EL317
121203         10  FT-CURR-GP-PMTS-VOIDED-AMT    PIC S9(9)V99  VALUE +0.EL317
121203         10  FT-CURR-GP-PMTS-VOIDED-CNT    PIC S9(7)     VALUE +0.EL317
121203                                                                  EL317
121203         10  FILLER                        PIC S9(9)V99  VALUE +0.EL317
121203         10  FILLER                        PIC S9(7)     VALUE +0.EL317
121203                                                                  EL317
121203         10  FILLER                        PIC S9(9)V99  VALUE +0.EL317
121203         10  FILLER                        PIC S9(7)     VALUE +0.EL317
121203                                                                  EL317
121203         10  FILLER                        PIC S9(9)V99  VALUE +0.EL317
121203         10  FILLER                        PIC S9(7)     VALUE +0.EL317
121203                                                                  EL317
121203         10  FILLER                        PIC S9(9)V99  VALUE +0.EL317
121203         10  FILLER                        PIC S9(7)     VALUE +0.EL317
052614
052614         10  FILLER                        PIC S9(9)V99  VALUE +0.
052614         10  FILLER                        PIC S9(7)     VALUE +0.
052614
052614         10  FILLER                        PIC S9(9)V99  VALUE +0.
052614         10  FILLER                        PIC S9(7)     VALUE +0.
100518
022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.
022122
022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.
022122
022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.
022122
022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.

100518         10  FILLER                        PIC S9(9)V99  VALUE +0.
100518         10  FILLER                        PIC S9(7)     VALUE +0.
100518
100518         10  FILLER                        PIC S9(9)V99  VALUE +0.
100518         10  FILLER                        PIC S9(7)     VALUE +0.
121203                                                                  EL317
052614*****Index5 area DCC only  family leave
052614     05  WS-TA-FL-OVERIDE-L6         PIC X(6)        VALUE SPACE.
052614     05  FILLER                      PIC X(24)       VALUE
052614         ' PAYMENTS'.
052614     05  FILLER                      COMP-3.
052614         10  CT-CURR-FL-PMTS-AMT           PIC S9(9)V99  VALUE +0.
052614         10  CT-CURR-FL-PMTS-CNT           PIC S9(7)     VALUE +0.
052614
052614         10  FT-CURR-FL-PMTS-AMT           PIC S9(9)V99  VALUE +0.
052614         10  FT-CURR-FL-PMTS-CNT           PIC S9(7)     VALUE +0.
052614
052614         10  CT-CURR-FL-PMTS-VOIDED-AMT    PIC S9(9)V99  VALUE +0.
052614         10  CT-CURR-FL-PMTS-VOIDED-CNT    PIC S9(7)     VALUE +0.
052614
052614         10  FT-CURR-FL-PMTS-VOIDED-AMT    PIC S9(9)V99  VALUE +0.
052614         10  FT-CURR-FL-PMTS-VOIDED-CNT    PIC S9(7)     VALUE +0.
052614
052614         10  FILLER                        PIC S9(9)V99  VALUE +0.
052614         10  FILLER                        PIC S9(7)     VALUE +0.
052614
052614         10  FILLER                        PIC S9(9)V99  VALUE +0.
052614         10  FILLER                        PIC S9(7)     VALUE +0.
052614
052614         10  FILLER                        PIC S9(9)V99  VALUE +0.
052614         10  FILLER                        PIC S9(7)     VALUE +0.
052614
052614         10  FILLER                        PIC S9(9)V99  VALUE +0.
052614         10  FILLER                        PIC S9(7)     VALUE +0.
052614
052614         10  FILLER                        PIC S9(9)V99  VALUE +0.
052614         10  FILLER                        PIC S9(7)     VALUE +0.
052614
052614         10  FILLER                        PIC S9(9)V99  VALUE +0.
052614         10  FILLER                        PIC S9(7)     VALUE +0.
052614
022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.
022122
022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.
022122
022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.
022122
022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.

100518         10  FILLER                        PIC S9(9)V99  VALUE +0.
100518         10  FILLER                        PIC S9(7)     VALUE +0.
100518
100518         10  FILLER                        PIC S9(9)V99  VALUE +0.
100518         10  FILLER                        PIC S9(7)     VALUE +0.

022122*****Index6 area  bereavement
022122     05  WS-TA-BR-OVERIDE-L6         PIC X(6)        VALUE SPACE.
022122     05  FILLER                      PIC X(24)       VALUE
022122         ' PAYMENTS'.
022122     05  FILLER                      COMP-3.
022122         10  CT-CURR-BR-PMTS-AMT           PIC S9(9)V99  VALUE +0.
022122         10  CT-CURR-BR-PMTS-CNT           PIC S9(7)     VALUE +0.
022122
022122         10  FT-CURR-BR-PMTS-AMT           PIC S9(9)V99  VALUE +0.
022122         10  FT-CURR-BR-PMTS-CNT           PIC S9(7)     VALUE +0.
022122
022122         10  CT-CURR-BR-PMTS-VOIDED-AMT    PIC S9(9)V99  VALUE +0.
022122         10  CT-CURR-BR-PMTS-VOIDED-CNT    PIC S9(7)     VALUE +0.
022122
022122         10  FT-CURR-BR-PMTS-VOIDED-AMT    PIC S9(9)V99  VALUE +0.
022122         10  FT-CURR-BR-PMTS-VOIDED-CNT    PIC S9(7)     VALUE +0.
022122
022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.
022122
022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.
022122
022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.
022122
022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.
022122
022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.
022122
022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.
022122
022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.
022122
022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.
022122
022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.
022122
022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.

022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.
022122
022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.
022122

022122*****Index7 area  hospital
022122     05  WS-TA-HS-OVERIDE-L6         PIC X(6)        VALUE SPACE.
022122     05  FILLER                      PIC X(24)       VALUE
022122         ' PAYMENTS'.
022122     05  FILLER                      COMP-3.
022122         10  CT-CURR-HS-PMTS-AMT           PIC S9(9)V99  VALUE +0.
022122         10  CT-CURR-HS-PMTS-CNT           PIC S9(7)     VALUE +0.
022122
022122         10  FT-CURR-HS-PMTS-AMT           PIC S9(9)V99  VALUE +0.
022122         10  FT-CURR-HS-PMTS-CNT           PIC S9(7)     VALUE +0.
022122
022122         10  CT-CURR-HS-PMTS-VOIDED-AMT    PIC S9(9)V99  VALUE +0.
022122         10  CT-CURR-HS-PMTS-VOIDED-CNT    PIC S9(7)     VALUE +0.
022122
022122         10  FT-CURR-HS-PMTS-VOIDED-AMT    PIC S9(9)V99  VALUE +0.
022122         10  FT-CURR-HS-PMTS-VOIDED-CNT    PIC S9(7)     VALUE +0.
022122
022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.
022122
022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.
022122
022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.
022122
022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.
022122
022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.
022122
022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.
022122
022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.
022122
022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.
022122
022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.
022122
022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.

022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.
022122
022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.
022122

100518*****Index8 area   other
100518     05  WS-TA-OT-OVERIDE-L6         PIC X(6)        VALUE SPACE.
100518     05  FILLER                      PIC X(24)       VALUE
100518         ' PAYMENTS'.
100518     05  FILLER                      COMP-3.
100518         10  CT-CURR-OT-PMTS-AMT           PIC S9(9)V99  VALUE +0.
100518         10  CT-CURR-OT-PMTS-CNT           PIC S9(7)     VALUE +0.
100518
100518         10  FT-CURR-OT-PMTS-AMT           PIC S9(9)V99  VALUE +0.
100518         10  FT-CURR-OT-PMTS-CNT           PIC S9(7)     VALUE +0.
100518
100518         10  CT-CURR-OT-PMTS-VOIDED-AMT    PIC S9(9)V99  VALUE +0.
100518         10  CT-CURR-OT-PMTS-VOIDED-CNT    PIC S9(7)     VALUE +0.
100518
100518         10  FT-CURR-OT-PMTS-VOIDED-AMT    PIC S9(9)V99  VALUE +0.
100518         10  FT-CURR-OT-PMTS-VOIDED-CNT    PIC S9(7)     VALUE +0.
100518
100518         10  FILLER                        PIC S9(9)V99  VALUE +0.
100518         10  FILLER                        PIC S9(7)     VALUE +0.
100518
100518         10  FILLER                        PIC S9(9)V99  VALUE +0.
100518         10  FILLER                        PIC S9(7)     VALUE +0.
100518
100518         10  FILLER                        PIC S9(9)V99  VALUE +0.
100518         10  FILLER                        PIC S9(7)     VALUE +0.
100518
100518         10  FILLER                        PIC S9(9)V99  VALUE +0.
100518         10  FILLER                        PIC S9(7)     VALUE +0.
100518
100518         10  FILLER                        PIC S9(9)V99  VALUE +0.
100518         10  FILLER                        PIC S9(7)     VALUE +0.
100518
100518         10  FILLER                        PIC S9(9)V99  VALUE +0.
100518         10  FILLER                        PIC S9(7)     VALUE +0.
100518
022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.
022122
022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.
022122
022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.
022122
022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.

100518         10  FILLER                        PIC S9(9)V99  VALUE +0.
100518         10  FILLER                        PIC S9(7)     VALUE +0.
100518
100518         10  FILLER                        PIC S9(9)V99  VALUE +0.
100518         10  FILLER                        PIC S9(7)     VALUE +0.
100518


100518*****Index7 area
00238      05  FILLER                      PIC X(30)       VALUE        EL317
00239          'CHARGED EXPENSES'.                                      EL317
00240      05  FILLER                      COMP-3.                      EL317
00241          10  CT-CURR-CHG-EXP-AMT           PIC S9(9)V99  VALUE +0.EL317
00242          10  CT-CURR-CHG-EXP-CNT           PIC S9(7)     VALUE +0.EL317
00243                                                                   EL317
00244          10  FT-CURR-CHG-EXP-AMT           PIC S9(9)V99  VALUE +0.EL317
00245          10  FT-CURR-CHG-EXP-CNT           PIC S9(7)     VALUE +0.EL317
00246                                                                   EL317
00247          10  CT-CURR-CHG-EXP-VOIDED-AMT    PIC S9(9)V99  VALUE +0.EL317
00248          10  CT-CURR-CHG-EXP-VOIDED-CNT    PIC S9(7)     VALUE +0.EL317
00249                                                                   EL317
00250          10  FT-CURR-CHG-EXP-VOIDED-AMT    PIC S9(9)V99  VALUE +0.EL317
00251          10  FT-CURR-CHG-EXP-VOIDED-CNT    PIC S9(7)     VALUE +0.EL317
00246                                                                   EL317
122402         10  FILLER                        PIC S9(9)V99  VALUE +0.EL317
122402         10  FILLER                        PIC S9(7)     VALUE +0.EL317
00249                                                                   EL317
122402         10  FILLER                        PIC S9(9)V99  VALUE +0.EL317
122402         10  FILLER                        PIC S9(7)     VALUE +0.EL317
121203                                                                  EL317
121203         10  FILLER                        PIC S9(9)V99  VALUE +0.EL317
121203         10  FILLER                        PIC S9(7)     VALUE +0.EL317
121203                                                                  EL317
121203         10  FILLER                        PIC S9(9)V99  VALUE +0.EL317
121203         10  FILLER                        PIC S9(7)     VALUE +0.EL317
052614
052614         10  FILLER                        PIC S9(9)V99  VALUE +0.
052614         10  FILLER                        PIC S9(7)     VALUE +0.
052614
052614         10  FILLER                        PIC S9(9)V99  VALUE +0.
052614         10  FILLER                        PIC S9(7)     VALUE +0.
100518
022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.
022122
022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.
022122
022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.
022122
022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.

100518         10  FILLER                        PIC S9(9)V99  VALUE +0.
100518         10  FILLER                        PIC S9(7)     VALUE +0.
100518
100518         10  FILLER                        PIC S9(9)V99  VALUE +0.
100518         10  FILLER                        PIC S9(7)     VALUE +0.
00252                                                                   EL317
100518*****Index8 area
00253      05  FILLER                      PIC X(30)       VALUE        EL317
00254          'OTHER EXPENSES'.                                        EL317
00255      05  FILLER                      COMP-3.                      EL317
00256          10  CT-CURR-OTHER-EXP-AMT         PIC S9(9)V99  VALUE +0.EL317
00257          10  CT-CURR-OTHER-EXP-CNT         PIC S9(7)     VALUE +0.EL317
00258                                                                   EL317
00259          10  FT-CURR-OTHER-EXP-AMT         PIC S9(9)V99  VALUE +0.EL317
00260          10  FT-CURR-OTHER-EXP-CNT         PIC S9(7)     VALUE +0.EL317
00261                                                                   EL317
00262          10  CT-CURR-OTHER-EXP-VOIDED-AMT  PIC S9(9)V99  VALUE +0.EL317
00263          10  CT-CURR-OTHER-EXP-VOIDED-CNT  PIC S9(7)     VALUE +0.EL317
00264                                                                   EL317
00265          10  FT-CURR-OTHER-EXP-VOIDED-AMT  PIC S9(9)V99  VALUE +0.EL317
00266          10  FT-CURR-OTHER-EXP-VOIDED-CNT  PIC S9(7)     VALUE +0.EL317
00261                                                                   EL317
122402         10  FILLER                        PIC S9(9)V99  VALUE +0.EL317
122402         10  FILLER                        PIC S9(7)     VALUE +0.EL317
00264                                                                   EL317
122402         10  FILLER                        PIC S9(9)V99  VALUE +0.EL317
122402         10  FILLER                        PIC S9(7)     VALUE +0.EL317
121203                                                                  EL317
121203         10  FILLER                        PIC S9(9)V99  VALUE +0.EL317
121203         10  FILLER                        PIC S9(7)     VALUE +0.EL317
121203                                                                  EL317
121203         10  FILLER                        PIC S9(9)V99  VALUE +0.EL317
121203         10  FILLER                        PIC S9(7)     VALUE +0.EL317
052614
052614         10  FILLER                        PIC S9(9)V99  VALUE +0.
052614         10  FILLER                        PIC S9(7)     VALUE +0.
052614
052614         10  FILLER                        PIC S9(9)V99  VALUE +0.
052614         10  FILLER                        PIC S9(7)     VALUE +0.
100518
022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.
022122
022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.
022122
022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.
022122
022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.

100518         10  FILLER                        PIC S9(9)V99  VALUE +0.
100518         10  FILLER                        PIC S9(7)     VALUE +0.
100518
100518         10  FILLER                        PIC S9(9)V99  VALUE +0.
100518         10  FILLER                        PIC S9(7)     VALUE +0.
00267                                                                   EL317
100518*****Index9 area
00268      05  FILLER                      PIC X(30)       VALUE        EL317
00269          'CHECKS WRITTEN'.                                        EL317
00270      05  FILLER                      COMP-3.                      EL317
00271          10  CT-CURR-CHECKS-AMT            PIC S9(9)V99  VALUE +0.EL317
00272          10  CT-CURR-CHECKS-CNT            PIC S9(7)     VALUE +0.EL317
00273                                                                   EL317
00274          10  FT-CURR-CHECKS-AMT            PIC S9(9)V99  VALUE +0.EL317
00275          10  FT-CURR-CHECKS-CNT            PIC S9(7)     VALUE +0.EL317
00276                                                                   EL317
00277          10  CT-CURR-CHECKS-VOIDED-AMT     PIC S9(9)V99  VALUE +0.EL317
00278          10  CT-CURR-CHECKS-VOIDED-CNT     PIC S9(7)     VALUE +0.EL317
00279                                                                   EL317
00280          10  FT-CURR-CHECKS-VOIDED-AMT     PIC S9(9)V99  VALUE +0.EL317
00281          10  FT-CURR-CHECKS-VOIDED-CNT     PIC S9(7)     VALUE +0.EL317
00276                                                                   EL317
122402         10  FILLER                        PIC S9(9)V99  VALUE +0.EL317
122402         10  FILLER                        PIC S9(7)     VALUE +0.EL317
00279                                                                   EL317
122402         10  FILLER                        PIC S9(9)V99  VALUE +0.EL317
122402         10  FILLER                        PIC S9(7)     VALUE +0.EL317
121203                                                                  EL317
121203         10  FILLER                        PIC S9(9)V99  VALUE +0.EL317
121203         10  FILLER                        PIC S9(7)     VALUE +0.EL317
121203                                                                  EL317
121203         10  FILLER                        PIC S9(9)V99  VALUE +0.EL317
121203         10  FILLER                        PIC S9(7)     VALUE +0.EL317
052614
052614         10  FILLER                        PIC S9(9)V99  VALUE +0.
052614         10  FILLER                        PIC S9(7)     VALUE +0.
052614
052614         10  FILLER                        PIC S9(9)V99  VALUE +0.
052614         10  FILLER                        PIC S9(7)     VALUE +0.
100518
022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.
022122
022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.
022122
022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.
022122
022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.

100518         10  FILLER                        PIC S9(9)V99  VALUE +0.
100518         10  FILLER                        PIC S9(7)     VALUE +0.
100518
100518         10  FILLER                        PIC S9(9)V99  VALUE +0.
100518         10  FILLER                        PIC S9(7)     VALUE +0.
00276                                                                   EL317
100518*****Index10 area
00283      05  FILLER                      PIC X(30)       VALUE        EL317
00284          'NET PAID ONLINE'.                                       EL317
00285      05  FILLER                      COMP-3.                      EL317
00286          10  CT-CURR-NET-PAID-ONLINE-AMT-LF PIC S9(9)V99 VALUE +0.EL317
00287          10  CT-CURR-NET-PAID-ONLINE-CNT-LF PIC S9(7)    VALUE +0.EL317

00288          10  FT-CURR-NET-PAID-ONLINE-AMT-LF PIC S9(9)V99 VALUE +0.EL317
00289          10  FT-CURR-NET-PAID-ONLINE-CNT-LF PIC S9(7)    VALUE +0.EL317

00290          10  CT-CURR-NET-PAID-ONLINE-AMT-AH PIC S9(9)V99 VALUE +0.EL317
00291          10  CT-CURR-NET-PAID-ONLINE-CNT-AH PIC S9(7)    VALUE +0.EL317

00292          10  FT-CURR-NET-PAID-ONLINE-AMT-AH PIC S9(9)V99 VALUE +0.EL317
00293          10  FT-CURR-NET-PAID-ONLINE-CNT-AH PIC S9(7)    VALUE +0.EL317

122402         10  CT-CURR-NET-PAID-ONLINE-AMT-IU PIC S9(9)V99 VALUE +0.EL317
122402         10  CT-CURR-NET-PAID-ONLINE-CNT-IU PIC S9(7)    VALUE +0.EL317

122402         10  FT-CURR-NET-PAID-ONLINE-AMT-IU PIC S9(9)V99 VALUE +0.EL317
122402         10  FT-CURR-NET-PAID-ONLINE-CNT-IU PIC S9(7)    VALUE +0.EL317

121203         10  CT-CURR-NET-PAID-ONLINE-AMT-GP PIC S9(9)V99 VALUE +0.EL317
121203         10  CT-CURR-NET-PAID-ONLINE-CNT-GP PIC S9(7)    VALUE +0.EL317
121203
121203         10  FT-CURR-NET-PAID-ONLINE-AMT-GP PIC S9(9)V99 VALUE +0.EL317
121203         10  FT-CURR-NET-PAID-ONLINE-CNT-GP PIC S9(7)    VALUE +0.EL317
052614
052614         10  CT-CURR-NET-PAID-ONLINE-AMT-FL PIC S9(9)V99 VALUE +0.
052614         10  CT-CURR-NET-PAID-ONLINE-CNT-FL PIC S9(7)    VALUE +0.
052614
052614         10  FT-CURR-NET-PAID-ONLINE-AMT-FL PIC S9(9)V99 VALUE +0.
052614         10  FT-CURR-NET-PAID-ONLINE-CNT-FL PIC S9(7)    VALUE +0.
100518
022122         10  CT-CURR-NET-PAID-ONLINE-AMT-BR PIC S9(9)V99 VALUE +0.
022122         10  CT-CURR-NET-PAID-ONLINE-CNT-BR PIC S9(7)    VALUE +0.
022122
022122         10  FT-CURR-NET-PAID-ONLINE-AMT-BR PIC S9(9)V99 VALUE +0.
022122         10  FT-CURR-NET-PAID-ONLINE-CNT-BR PIC S9(7)    VALUE +0.
022122
022122         10  CT-CURR-NET-PAID-ONLINE-AMT-HS PIC S9(9)V99 VALUE +0.
022122         10  CT-CURR-NET-PAID-ONLINE-CNT-HS PIC S9(7)    VALUE +0.
022122
022122         10  FT-CURR-NET-PAID-ONLINE-AMT-HS PIC S9(9)V99 VALUE +0.
022122         10  FT-CURR-NET-PAID-ONLINE-CNT-HS PIC S9(7)    VALUE +0.
100518
100518         10  CT-CURR-NET-PAID-ONLINE-AMT-OT PIC S9(9)V99 VALUE +0.
100518         10  CT-CURR-NET-PAID-ONLINE-CNT-OT PIC S9(7)    VALUE +0.
100518
100518         10  FT-CURR-NET-PAID-ONLINE-AMT-OT PIC S9(9)V99 VALUE +0.
100518         10  FT-CURR-NET-PAID-ONLINE-CNT-OT PIC S9(7)    VALUE +0.
00294                                                                   EL317
100518*****Index11 area
00295      05  FILLER                      PIC X(30)       VALUE        EL317
00296          'NET PAID OFFLINE'.                                      EL317
00297      05  FILLER                      COMP-3.                      EL317
00298          10  CT-CURR-NET-PD-OFFLINE-AMT-LF PIC S9(9)V99  VALUE +0.EL317
00299          10  CT-CURR-NET-PD-OFFLINE-CNT-LF PIC S9(7)     VALUE +0.EL317

00300          10  FT-CURR-NET-PD-OFFLINE-AMT-LF PIC S9(9)V99  VALUE +0.EL317
00301          10  FT-CURR-NET-PD-OFFLINE-CNT-LF PIC S9(7)     VALUE +0.EL317

00302          10  CT-CURR-NET-PD-OFFLINE-AMT-AH PIC S9(9)V99  VALUE +0.EL317
00303          10  CT-CURR-NET-PD-OFFLINE-CNT-AH PIC S9(7)     VALUE +0.EL317

00304          10  FT-CURR-NET-PD-OFFLINE-AMT-AH PIC S9(9)V99  VALUE +0.EL317
00305          10  FT-CURR-NET-PD-OFFLINE-CNT-AH PIC S9(7)     VALUE +0.EL317

122402         10  CT-CURR-NET-PD-OFFLINE-AMT-IU PIC S9(9)V99  VALUE +0.EL317
122402         10  CT-CURR-NET-PD-OFFLINE-CNT-IU PIC S9(7)     VALUE +0.EL317

122402         10  FT-CURR-NET-PD-OFFLINE-AMT-IU PIC S9(9)V99  VALUE +0.EL317
122402         10  FT-CURR-NET-PD-OFFLINE-CNT-IU PIC S9(7)     VALUE +0.EL317
121203
121203         10  CT-CURR-NET-PD-OFFLINE-AMT-GP PIC S9(9)V99  VALUE +0.EL317
121203         10  CT-CURR-NET-PD-OFFLINE-CNT-GP PIC S9(7)     VALUE +0.EL317
121203
121203         10  FT-CURR-NET-PD-OFFLINE-AMT-GP PIC S9(9)V99  VALUE +0.EL317
121203         10  FT-CURR-NET-PD-OFFLINE-CNT-GP PIC S9(7)     VALUE +0.EL317
052614
052614         10  CT-CURR-NET-PD-OFFLINE-AMT-FL PIC S9(9)V99  VALUE +0.
052614         10  CT-CURR-NET-PD-OFFLINE-CNT-FL PIC S9(7)     VALUE +0.
052614
052614         10  FT-CURR-NET-PD-OFFLINE-AMT-FL PIC S9(9)V99  VALUE +0.
052614         10  FT-CURR-NET-PD-OFFLINE-CNT-FL PIC S9(7)     VALUE +0.
100518
022122         10  CT-CURR-NET-PD-OFFLINE-AMT-BR PIC S9(9)V99  VALUE +0.
022122         10  CT-CURR-NET-PD-OFFLINE-CNT-BR PIC S9(7)     VALUE +0.
022122
022122         10  FT-CURR-NET-PD-OFFLINE-AMT-BR PIC S9(9)V99  VALUE +0.
022122         10  FT-CURR-NET-PD-OFFLINE-CNT-BR PIC S9(7)     VALUE +0.
022122
022122         10  CT-CURR-NET-PD-OFFLINE-AMT-HS PIC S9(9)V99  VALUE +0.
022122         10  CT-CURR-NET-PD-OFFLINE-CNT-HS PIC S9(7)     VALUE +0.
022122
022122         10  FT-CURR-NET-PD-OFFLINE-AMT-HS PIC S9(9)V99  VALUE +0.
022122         10  FT-CURR-NET-PD-OFFLINE-CNT-HS PIC S9(7)     VALUE +0.
100518
100518         10  CT-CURR-NET-PD-OFFLINE-AMT-OT PIC S9(9)V99  VALUE +0.
100518         10  CT-CURR-NET-PD-OFFLINE-CNT-OT PIC S9(7)     VALUE +0.
100518
100518         10  FT-CURR-NET-PD-OFFLINE-AMT-OT PIC S9(9)V99  VALUE +0.
100518         10  FT-CURR-NET-PD-OFFLINE-CNT-OT PIC S9(7)     VALUE +0.
00306                                                                   EL317
100518*****Index12 area
00307      05  FILLER                      PIC X(30)       VALUE        EL317
00308          'PREVIOUS MONTHS PAYMENTS'.                              EL317
00309      05  FILLER                      COMP-3.                      EL317
00310          10  CT-PREV-CHECKS-AMT            PIC S9(9)V99  VALUE +0.EL317
00311          10  CT-PREV-CHECKS-CNT            PIC S9(7)     VALUE +0.EL317
00312                                                                   EL317
00313          10  FT-PREV-CHECKS-AMT            PIC S9(9)V99  VALUE +0.EL317
00314          10  FT-PREV-CHECKS-CNT            PIC S9(7)     VALUE +0.EL317
00315                                                                   EL317
00316          10  CT-PREV-CHECKS-VOIDED-AMT     PIC S9(9)V99  VALUE +0.EL317
00317          10  CT-PREV-CHECKS-VOIDED-CNT     PIC S9(7)     VALUE +0.EL317
00318                                                                   EL317
00319          10  FT-PREV-CHECKS-VOIDED-AMT     PIC S9(9)V99  VALUE +0.EL317
00320          10  FT-PREV-CHECKS-VOIDED-CNT     PIC S9(7)     VALUE +0.EL317
00315                                                                   EL317
122402         10  FILLER                        PIC S9(9)V99  VALUE +0.EL317
122402         10  FILLER                        PIC S9(7)     VALUE +0.EL317
00318                                                                   EL317
122402         10  FILLER                        PIC S9(9)V99  VALUE +0.EL317
122402         10  FILLER                        PIC S9(7)     VALUE +0.EL317
121203                                                                  EL317
121203         10  FILLER                        PIC S9(9)V99  VALUE +0.EL317
121203         10  FILLER                        PIC S9(7)     VALUE +0.EL317
121203                                                                  EL317
121203         10  FILLER                        PIC S9(9)V99  VALUE +0.EL317
121203         10  FILLER                        PIC S9(7)     VALUE +0.EL317
052614
052614         10  FILLER                        PIC S9(9)V99  VALUE +0.
052614         10  FILLER                        PIC S9(7)     VALUE +0.
052614
052614         10  FILLER                        PIC S9(9)V99  VALUE +0.
052614         10  FILLER                        PIC S9(7)     VALUE +0.
100518
022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.
022122
022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.
022122
022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.
022122
022122         10  FILLER                        PIC S9(9)V99  VALUE +0.
022122         10  FILLER                        PIC S9(7)     VALUE +0.

100518         10  FILLER                        PIC S9(9)V99  VALUE +0.
100518         10  FILLER                        PIC S9(7)     VALUE +0.
100518
100518         10  FILLER                        PIC S9(9)V99  VALUE +0.
100518         10  FILLER                        PIC S9(7)     VALUE +0.
00315                                                                   EL317
00321                                                                   EL317
00322    02  FILLER  REDEFINES                                          EL317
022122       TOTALS-AREA                   OCCURS 14 TIMES
00324        INDEXED BY TOTAL-INDEX1.                                   EL317
00325                                                                   EL317
00326      05  TOTALS-DESC                 PIC X(30).                   EL317
00327                                                                   EL317
00328      05  TOTAL-AMOUNTS               COMP-3.                      EL317
100518         10  FILLER                  OCCURS 16 TIMES              EL317
122402             INDEXED BY TOTAL-INDEX2 TOTAL-INDEX3 TOTAL-INDEX4
100518                        TOTAL-INDEX5 TOTAL-INDEX6 TOTAL-INDEX7
022122                        TOTAL-INDEX8 TOTAL-INDEX9
00331                         TOTAL-INDEX-ONE TOTAL-INDEX-TWO           EL317
00332                         TOTAL-INDEX-THREE TOTAL-INDEX-FOUR
122402                        TOTAL-INDEX-FIVE  TOTAL-INDEX-SIX
121203                        TOTAL-INDEX-SEVEN TOTAL-INDEX-EIGHT
100518                        TOTAL-INDEX-NINE TOTAL-INDEX-TEN
100518                        TOTAL-INDEX-ELEVEN TOTAL-INDEX-TWELVE  
022122                        TOTAL-INDEX-THIRTEEN TOTAL-INDEX-FOURTEEN
022122                        TOTAL-INDEX-FIFTEEN TOTAL-INDEX-SIXTEEN.
00333              15  TOTALS-CURR-AMT     PIC S9(9)V99.                EL317
00334              15  TOTALS-CURR-CNT     PIC S9(7).                   EL317
00335                                                                   EL317
00336    02  FILLER.                                                    EL317
00337      05  FILLER                      COMP-3.                      EL317
092602         10  CT-CURR-BENEFIT-TOTALS           OCCURS 900 TIMES    EL317
00339                                               INDEXED BY CT-INDEX.EL317
00340              15  CT-CURR-BENEFIT-AMT          PIC S9(9)V99.       EL317
00341              15  CT-CURR-BENEFIT-CNT          PIC S9(7).          EL317
00342                                                                   EL317
00343              15  FT-CURR-BENEFIT-AMT          PIC S9(9)V99.       EL317
00344              15  FT-CURR-BENEFIT-CNT          PIC S9(7).          EL317
00345                                                                   EL317
00346              15  CT-CURR-BENEFIT-VOIDED-AMT   PIC S9(9)V99.       EL317
00347              15  CT-CURR-BENEFIT-VOIDED-CNT   PIC S9(7).          EL317
00348                                                                   EL317
00349              15  FT-CURR-BENEFIT-VOIDED-AMT   PIC S9(9)V99.       EL317
00350              15  FT-CURR-BENEFIT-VOIDED-CNT   PIC S9(7).          EL317
00351                                                                   EL317
022122     05  WS-TOTAL-INDEX-MAX          PIC S9(4)       VALUE +14    EL317
00353                                      COMP SYNC.                   EL317
00354                                                                   EL317
00355      EJECT                                                        EL317
00356    02  PAYEE-TYPE-TOTALS.                                         EL317
00357      05  FILLER                  COMP-3.                          EL317
00358        10  PAYEE-TYPE-TOTALS               OCCURS 6 TIMES         EL317
00359                              INDEXED BY INSD-NDX  BENE-NDX        EL317
00360                                         ACCT-NDX  PHYS-NDX        EL317
00361                                         OTH1-NDX  OTH2-NDX        EL317
00362                                                   TOT-NDX1.       EL317
00363                                                                   EL317
100518         15  PAYEE-TYPE-COVG               OCCURS 6 TIMES         EL317
122402                    INDEXED BY LF-NDX  AH-NDX  IU-NDX TOT-NDX2
100518                               GP-NDX  FL-NDX  OT-NDX
022122                               BR-NDX  HS-NDX.
00366            20  CPT-PMT-CNTS                PIC S9(7).             EL317
00367            20  CPT-PMT-AMTS                PIC S9(9)V99.          EL317
00368                                                                   EL317
00369            20  FPT-PMT-CNTS                PIC S9(7).             EL317
00370            20  FPT-PMT-AMTS                PIC S9(9)V99.          EL317
00371                                                                   EL317
00372            20  CPT-PMT-VOID-CNTS           PIC S9(7).             EL317
00373            20  CPT-PMT-VOID-AMTS           PIC S9(9)V99.          EL317
00374                                                                   EL317
00375            20  FPT-PMT-VOID-CNTS           PIC S9(7).             EL317
00376            20  FPT-PMT-VOID-AMTS           PIC S9(9)V99.          EL317
00377                                                                   EL317
00378    02  REINSURANCE-TYPE-TOTALS.                                   EL317
00379      05  FILLER                  COMP-3.                          EL317
00380          10  CR-PMT-CNTS                   PIC S9(7)     VALUE +0.EL317
00381          10  CR-PMT-AMTS                   PIC S9(9)V99  VALUE +0.EL317
00382                                                                   EL317
00383          10  CR-PMT-VOID-CNTS              PIC S9(7)     VALUE +0.EL317
00384          10  CR-PMT-VOID-AMTS              PIC S9(9)V99  VALUE +0.EL317
00385                                                                   EL317
00386          10  FR-PMT-CNTS                   PIC S9(7)     VALUE +0.EL317
00387          10  FR-PMT-AMTS                   PIC S9(9)V99  VALUE +0.EL317
00388                                                                   EL317
00389          10  FR-PMT-VOID-CNTS              PIC S9(7)     VALUE +0.EL317
00390          10  FR-PMT-VOID-AMTS              PIC S9(9)V99  VALUE +0.EL317
00391                                                                   EL317
00392                                                                   EL317
00393      EJECT                                                        EL317
00394  01  WS-HEADING1.                                                 EL317
00395      05  FILLER                      PIC X(49)       VALUE '1'.   EL317
00396      05  WS-H1-TITLE                 PIC X(71)       VALUE        EL317
00397          'MONTHLY CHECK REGISTER'.                                EL317
00398      05  WS-H1-REPORT-NUMBER         PIC X(05)      VALUE 'EL317'.EL317
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
00425      05  FILLER                      PIC X(12)       VALUE        EL317
00426          'PAYEE NAME /'.                                          EL317
00427      05  FILLER                      PIC X(14)       VALUE SPACE. EL317
00428      05  FILLER                      PIC X(19)       VALUE        EL317
00429          'CLAIM NO INCURRED  '.                                   EL317
00430      05  FILLER                      PIC X(46)       VALUE        EL317
071403         'CAR  CERT NO   EFFECTIVE  ST  ACCOUNT   GROUP '.        EL317
00431 *        '************** CERTIFICATE DATA ************* '.        EL317
00432                                                                   EL317
00433  01  WS-HEADING5.                                                 EL317
00434      05  FILLER                      PIC X(52)       VALUE        EL317
00435          '  NUMBER      AMOUNT     PAID    ORIGIN         TYPE'.  EL317
00436      05  FILLER                      PIC X(15)       VALUE SPACE. EL317
052804     05  FILLER                      PIC X(17)       VALUE        EL317
052804         'INSURED LAST NAME'.                                     EL317
052804     05  FILLER                      PIC X(17)       VALUE SPACES.
052804     05  FILLER                      PIC X(09)       VALUE        EL317
052804         'PAID THRU'.   
052804     05  FILLER                      PIC X(02)       VALUE SPACES.
052804     05  FILLER                      PIC X(10)       VALUE
052804         'RPT CODE 1'.
052804     05  FILLER                      PIC X(01)       VALUE SPACES.
051104     05  FILLER                      PIC X(10)       VALUE 
051104         'RPT CODE 2'.

00452  01  WS-HEADING6.                                                 EL317
00453      05  FILLER                      PIC X(133)      VALUE        EL317
00454          '0 * * * * * * CARRIER TOTALS * * * * * *'.              EL317
00455  01  WS-HEADING7.                                                 EL317
00456      05  FILLER                      PIC X(8)        VALUE        EL317
00457          '0 * * * '.                                              EL317
00458      05  WS7-OVERIDE-VALUE           PIC X(6)        VALUE SPACE. EL317
00459      05  FILLER                      PIC X(119)      VALUE        EL317
00460          'TOTALS BY BENEFIT TYPE * * *'.                          EL317
00461                                                                   EL317
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
00474                                                                   EL317
122402*01  WS-HEADINGA.                                                 EL317
122402*    05  FILLER                      PIC X(8)        VALUE        EL317
122402*        '0 * * * '.                                              EL317
122402*    05  WSA-OVERIDE-VALUE           PIC X(6)        VALUE SPACE. EL317
122402*    05  FILLER                      PIC X(119)      VALUE        EL317
122402*        ' TOTALS BY PAYEE TYPE * * *'.                           EL317
00481                                                                   EL317
00482      EJECT                                                        EL317
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
00503      05  WS-D1-PAYEE-NAME            PIC X(25).                   EL317
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
00521      EJECT                                                        EL317
00522  01  WS-DETAIL2 REDEFINES                                         EL317
00523      WS-DETAIL1.                                                  EL317
00524                                                                   EL317
00525      05  FILLER                      PIC X(9).                    EL317
00526      05  WS-D2-MESSAGE               PIC X(15).                   EL317
00527      05  FILLER                      PIC X(9).                    EL317
00528      05  WS-D2-ORIGIN                PIC X(7).                    EL317
00529      05  FILLER                      PIC XX.                      EL317
00530      05  WS-D2-PAYEE-TYPE            PIC X(11).                   EL317
00531      05  FILLER                      PIC X(15).                   EL317
00532      05  WS-D2-INSURED-LAST-NAME     PIC X(15).                   EL317
00533      05  FILLER                      PIC X(3).                    EL317
052804     05  WS-D2-MESSAGE2              PIC X(13).                   EL317
071403     05  FILLER                      PIC X(03).                   EL317
071403     05  WS-D2-PAID-THRU-DT          PIC X(08).                   EL317
052804     05  FILLER                      PIC X(02).
052804     05  WS-D2-REPORT-CODE-1         PIC X(10).                   EL317
052804     05  FILLER                      PIC X(01).
051104     05  WS-D2-REPORT-CODE-2         PIC X(10).                   EL317
00536                                                                   EL317
00537      EJECT                                                        EL317
00538  01  WS-TOTAL-LINE1 REDEFINES                                     EL317
00539      WS-DETAIL1.                                                  EL317
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
00570  01  WS-TOTAL-LINE2 REDEFINES                                     EL317
00571      WS-DETAIL1.                                                  EL317
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
00608                           COPY ELCDATE.                              CL**3
00609      EJECT                                                        EL317
00610                           COPY ELCDTECX.                          EL317
00611      EJECT                                                        EL317
00612                           COPY ELCDTEVR.                          EL317
00613      EJECT                                                        EL317
00614  PROCEDURE DIVISION.                                              EL317
00615                                                                   EL317
00616  0000-DATE-CARD-READ SECTION. COPY ELCDTERX.                      EL317

070714****=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=***
070714****                                                           ***
070714****   Set up the month-end auto balancing.                    ***
070714****   Turn it off if this is a daily run.                     ***
070714****                                                           ***
070714****=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=***
070714
070714     MOVE WS-TIME                TO ME-START-TIME
070714     MOVE WS-CURRENT-DATE        TO ME-START-DATE
070714     MOVE ME-START-MO            TO ME-CNDS-MO
070714     MOVE ME-START-DA            TO ME-CNDS-DA
070714     MOVE ME-START-YR            TO ME-CNDS-YR

00618 **************************************************************    EL317
00619 * INITIALIZE WORKING STORAGE TOTALS AREA VARIABLE NAMES           EL317
00620 **************************************************************    EL317
00621                                                                   EL317
00622      MOVE LIFE-OVERRIDE-L6 TO WS-TA-LIFE-OVERIDE-L6.              EL317
00623      MOVE AH-OVERRIDE-L6   TO WS-TA-AH-OVERIDE-L6.                EL317
122402     MOVE '  IU  '         TO WS-TA-IU-OVERIDE-L6
121203     MOVE ' GAP  '         TO WS-TA-GP-OVERIDE-L6
052614     MOVE ' FAM  '         TO WS-TA-FL-OVERIDE-L6
022122     MOVE ' BRV  '         TO WS-TA-BR-OVERIDE-L6
022122     MOVE ' HOS  '         TO WS-TA-HS-OVERIDE-L6
100518     MOVE ' OTHER'         TO WS-TA-OT-OVERIDE-L6
00624                                                                   EL317
00625      IF DTE-PRC-OPT = '2'                                         EL317
00626          MOVE 'DAILY CHECK REGISTER' TO WS-H1-TITLE.              EL317
00627                                                                   EL317
00628  1000-MAIN-LOGIC SECTION.                                         EL317
00629                                                                   EL317
00630      PERFORM OPEN-FILES.                                          EL317
00631                                                                   EL317
00632      PERFORM 3000-PRINT-REPORT.                                   EL317
00633                                                                   EL317
00634      PERFORM CLOSE-FILES.                                         EL317
00635                                                                   EL317
00636      GOBACK.                                                      EL317
00637                                                                   EL317
00638      EJECT                                                        EL317
00639  3000-PRINT-REPORT SECTION.                                       EL317
00640                                                                   EL317
00641  3100-PRINT-REPORT.                                               EL317
00642      READ REPORTS-EXTRACT-FILE                                    EL317
00643          AT END                                                   EL317
00644              MOVE HIGH-VALUES    TO  EX-SB-CARRIER                EL317
00645              GO TO 3110-PRINT-REPORT.                             EL317
00646                                                                   EL317
00647      IF EX-POSITIONING-CODE < '9'                                 EL317
00648          GO TO 3100-PRINT-REPORT.                                 EL317
00649                                                                   EL317
00650      IF EX-EXTRACT-CODE < 'B'                                     EL317
00651          GO TO 3100-PRINT-REPORT.                                 EL317
00652                                                                   EL317
00653      IF EX-EXTRACT-CODE > 'B'                                     EL317
00654          MOVE HIGH-VALUES  TO  EX-SB-CARRIER                      EL317
00655          GO TO  3110-PRINT-REPORT.                                EL317
00656                                                                   EL317
00657      IF EX-COMPANY-CD < DTE-CLASIC-COMPANY-CD                     EL317
00658          GO TO 3100-PRINT-REPORT.                                 EL317
00659                                                                   EL317
00660      IF EX-COMPANY-CD > DTE-CLASIC-COMPANY-CD                     EL317
00661          MOVE HIGH-VALUES  TO  EX-SB-CARRIER                      EL317
00662          GO TO  3110-PRINT-REPORT.                                EL317
00663                                                                   EL317
00664      IF EX-RECORD-TYPE > 'A'                                      EL317
00665          MOVE HIGH-VALUES  TO  EX-SB-CARRIER                      EL317
00666          GO TO 3110-PRINT-REPORT.                                 EL317

022106     IF EX-BA-LIFE-INTEREST
022106        GO TO 3100-PRINT-REPORT
022106     END-IF

00672      IF DTE-PRC-OPT NOT = '2'                                     EL317
00673          GO TO 3110-PRINT-REPORT.                                 EL317
00674                                                                   EL317
00675      IF EX-BA-PAYMENT-ORIGIN NOT = '3'                            EL317
00676        IF EX-BA-CHECK-WRITTEN-DT > EX-BA-LAST-PROCESS-DT          EL317
00677          GO TO 3110-PRINT-REPORT.                                 EL317
00678                                                                   EL317
00679      IF EX-BA-PAYMENT-ORIGIN = '3'                                EL317
00680        IF EX-BA-PMT-RECORDED-DT > EX-BA-LAST-PROCESS-DT           EL317
00681          GO TO 3110-PRINT-REPORT.                                 EL317
00682                                                                   EL317
00683      IF EX-BA-VOID-DT > EX-BA-LAST-PROCESS-DT                     EL317
00684          GO TO 3110-PRINT-REPORT.                                 EL317
00685                                                                   EL317
00686      GO TO 3100-PRINT-REPORT.                                     EL317
00687                                                                   EL317
00688      EJECT                                                        EL317
00689  3110-PRINT-REPORT.                                               EL317
00690      IF WS-FIRST-TIME                                             EL317
00691          MOVE 'N'               TO  WS-FIRST-TIME-SWITCH          EL317
00692          MOVE EX-SB-CARRIER     TO  WS-LAST-CARRIER               EL317
00693          PERFORM 8100-GET-CARRIER-NAME.                           EL317
00694                                                                   EL317
00695      IF EX-SB-CARRIER = HIGH-VALUES                               EL317
00696         IF WS-RECORD-COUNT NOT > ZERO                             EL317
00697           MOVE WS-NO-ACTIVITY         TO  PRT                     EL317
00698           PERFORM WRITE-A-LINE                                    EL317
00699           GO TO 3600-EXIT.                                        EL317
00700                                                                   EL317
00701      ADD +1  TO  WS-RECORD-COUNT.                                 EL317
00702                                                                   EL317
00703      EJECT                                                        EL317
00704 *    NOTE ******************************************************* EL317
00705 *         *          CONTROL BREAK LOGIC FOR CARRIER            * EL317
00706 *         *******************************************************.EL317
00707                                                                   EL317
00708      IF EX-SB-CARRIER = WS-LAST-CARRIER                           EL317
00709         GO TO 3200-PRINT-REPORT.                                  EL317
00710                                                                   EL317
00711      PERFORM 4000-PRINT-TOTALS.                                   EL317
00712                                                                   EL317
00713      EJECT                                                        EL317
00714 *    NOTE ******************************************************* EL317
00715 *         *          CONTROL BREAK LOGIC FOR FINAL              * EL317
00716 *         *******************************************************.EL317
00717                                                                   EL317
00718      IF EX-SB-CARRIER NOT = HIGH-VALUES                           EL317
00719          MOVE EX-SB-CARRIER   TO  WS-LAST-CARRIER                 EL317
00720          PERFORM 8100-GET-CARRIER-NAME                            EL317
00721          GO TO 3200-PRINT-REPORT.                                 EL317
00722                                                                   EL317
00723      MOVE +1            TO WS-REPORT-SW.                          EL317
00724                                                                   EL317
00725      PERFORM 4000-PRINT-TOTALS.                                   EL317
00726                                                                   EL317
00727      GO TO  3600-EXIT.                                            EL317
00728                                                                   EL317
00729      EJECT                                                        EL317
00730  3200-PRINT-REPORT.                                               EL317
00731                                                                   EL317
00732 *    NOTE ******************************************************* EL317
00733 *         *          DETAIL RECORD PROCESSING                   * EL317
00734 *         *******************************************************.EL317
00735                                                                   EL317
           MOVE WS-INIT-EXTRACT       TO EXTRACT-RECORD
00743      MOVE '0'                   TO  WS-DETAIL1.                   EL317
00744      MOVE EX-SB-CHECK-NO        TO  WS-D1-CHECK-NUMBER
                                          EXT-CHECK-NO
00745                                                                   EL317
00746 *    MOVE EX-BA-RESERVE-FLAG    TO  WS-D1-RESERVE-FLAG.           EL317
00747                                                                   EL317
00748      MOVE EX-BA-PAYMENT-AMOUNT  TO  WS-D1-AMOUNT
                                          EXT-AMOUNT
00749                                                                   EL317
00750      IF EX-BA-CHECK-WRITTEN-DT NOT = LOW-VALUES                   EL317
00751          MOVE EX-BA-CHECK-WRITTEN-DT TO  DC-BIN-DATE-1            EL317
00752          MOVE SPACES                 TO  DC-OPTION-CODE           EL317
00753          PERFORM 8500-DATE-CONVERSION                             EL317
00754          MOVE DC-GREG-DATE-1-EDIT    TO  WS-D1-DATE-PAID
                                               EXT-DATE-PAID
           END-IF
00755                                                                   EL317
122402     EVALUATE TRUE
122402     WHEN EX-BA-CLAIM-TYPE = LIFE-OVERRIDE-L1  
00757          MOVE LIFE-OVERRIDE-L2  TO  WS-D1-PT-1                    EL317

122402     WHEN EX-BA-CLAIM-TYPE = AH-OVERRIDE-L1
00759          MOVE AH-OVERRIDE-L2    TO  WS-D1-PT-1

122402     WHEN EX-BA-CLAIM-TYPE = 'I'
122402         MOVE 'IU'              TO  WS-D1-PT-1
121203
121203     WHEN EX-BA-CLAIM-TYPE = 'G'
121203         MOVE 'GP'              TO  WS-D1-PT-1
052614
052614     WHEN EX-BA-CLAIM-TYPE = 'F'
052614         MOVE 'FL'              TO  WS-D1-PT-1
100518
100518     WHEN EX-BA-CLAIM-TYPE = 'O'
100518         MOVE 'OT'              TO  WS-D1-PT-1

022122     WHEN EX-BA-CLAIM-TYPE = 'B'
022122         MOVE 'BR'              TO  WS-D1-PT-1
022122
022122     WHEN EX-BA-CLAIM-TYPE = 'H'
022122         MOVE 'HS'              TO  WS-D1-PT-1

122402     END-EVALUATE
           MOVE WS-D1-PT-1             TO EXT-PAY-TYPE1
00760                                                                   EL317
00761      MOVE '-'  TO  WS-D1-PT-1A.                                   EL317
00762                                                                   EL317
122402     EVALUATE TRUE
122402     WHEN EX-BA-PAYMENT-TYPE = '1'  
00764          MOVE 'PART  '                TO WS-D1-PT-2               EL317

122402     WHEN EX-BA-PAYMENT-TYPE = '2'  
00767          MOVE 'FINAL '                TO WS-D1-PT-2               EL317

122402     WHEN EX-BA-PAYMENT-TYPE = '3'
00770          MOVE 'LUMP  '                TO WS-D1-PT-2               EL317

122402     WHEN EX-BA-PAYMENT-TYPE = '4'                                  EL317
00773          MOVE 'ADDL  '                TO WS-D1-PT-2               EL317

122402     WHEN EX-BA-PAYMENT-TYPE = '5'                                  EL317
00776          MOVE 'CHGEXP'                TO WS-D1-PT-2               EL317

122402     WHEN EX-BA-PAYMENT-TYPE = '6'                                  EL317
00779          MOVE 'NONCHG'                TO WS-D1-PT-2               EL317

122402     WHEN EX-BA-PAYMENT-TYPE = '7' OR '8'                           EL317
00782          MOVE 'REFUND'                TO WS-D1-PT-2               EL317

122402     WHEN OTHER
00784          MOVE EX-BA-PAYMENT-TYPE      TO WS-D1-PT-2
122402     END-EVALUATE
           MOVE WS-D1-PT-2             TO EXT-PAY-TYPE2
00785                                                                   EL317
00786      MOVE EX-BA-PAYEES-NAME  TO  WS-D1-PAYEE-NAME
                                       EXT-PAYEE-NAME
00787      MOVE EX-BA-CLAIM-NO     TO  WS-D1-CLAIM-NO
                                       EXT-CLAIM-NO
00788                                                                   EL317
00789      IF EX-BA-INCURRED-DT NOT = LOW-VALUES                        EL317
00790          MOVE EX-BA-INCURRED-DT    TO DC-BIN-DATE-1               EL317
00791          MOVE SPACES               TO DC-OPTION-CODE              EL317
00792          PERFORM 8500-DATE-CONVERSION                             EL317
00793          MOVE DC-GREG-DATE-1-EDIT  TO WS-D1-INCURRED-DATE
                                            EXT-INCURRED-DATE
00794                                                                   EL317
00795      MOVE EX-SB-CARRIER         TO  WS-D1-CARRIER
                                          EXT-CARRIER
00796      MOVE EX-BA-CERT-NO         TO  WS-D1-CERT-NO
                                          EXT-CERT-NO
00797      MOVE EX-BA-CERT-EFF-DT     TO DC-BIN-DATE-1.                 EL317
00798      MOVE SPACES                TO  DC-OPTION-CODE.               EL317
00799      PERFORM 8500-DATE-CONVERSION.                                EL317
00800      MOVE DC-GREG-DATE-1-EDIT   TO  WS-D1-EFFECTIVE-DATE
                                          EXT-EFF-DATE
00801      MOVE EX-BA-STATE           TO  WS-D1-STATE
                                          EXT-STATE
00802      MOVE EX-BA-ACCOUNT         TO  WS-D1-ACCOUNT
                                          EXT-ACCOUNT
00803      MOVE EX-BA-GROUPING        TO  WS-D1-GROUP
                                          EXT-GROUP
00804      MOVE WS-DETAIL1            TO  PRT.                          EL317
00805      PERFORM WRITE-A-LINE.                                        EL317
00806      MOVE SPACES                TO  WS-DETAIL1.                   EL317
00807                                                                   EL317
122402     EVALUATE TRUE
122402     WHEN EX-BA-PAYMENT-ORIGIN = '1'
00809          MOVE 'ONLINE'              TO WS-D2-ORIGIN               EL317

122402     WHEN EX-BA-PAYMENT-ORIGIN = '2'                                EL317
00812          MOVE 'AUTO'                TO WS-D2-ORIGIN               EL317

122402     WHEN EX-BA-PAYMENT-ORIGIN = '3'                  
00815          MOVE 'OFFLINE'             TO WS-D2-ORIGIN               EL317

122402     WHEN OTHER
00817          MOVE EX-BA-PAYMENT-ORIGIN  TO WS-D2-ORIGIN
122402     END-EVALUATE
           MOVE WS-D2-ORIGIN           TO EXT-ORIGIN
00818                                                                   EL317
122402     EVALUATE TRUE
122402     WHEN EX-BA-PAYEE-TYPE-CD = 'I'   
00820          MOVE 'INSURED'           TO WS-D2-PAYEE-TYPE             EL317

122402     WHEN EX-BA-PAYEE-TYPE-CD = 'B'                                 EL317
00823          MOVE 'BENEFICIARY'       TO WS-D2-PAYEE-TYPE             EL317

122402     WHEN EX-BA-PAYEE-TYPE-CD = 'A'                                 EL317
00826          MOVE 'ACCOUNT'           TO WS-D2-PAYEE-TYPE             EL317

122402     WHEN EX-BA-PAYEE-TYPE-CD = 'O'                                 EL317
00829          MOVE 'OTHER 1'           TO WS-D2-PAYEE-TYPE             EL317

122402     WHEN EX-BA-PAYEE-TYPE-CD = 'Q' 
00832          MOVE 'OTHER 2'           TO WS-D2-PAYEE-TYPE             EL317

122402     WHEN EX-BA-PAYEE-TYPE-CD = 'P'  
00835          MOVE 'PHYSICIAN'         TO WS-D2-PAYEE-TYPE             EL317

122402     WHEN OTHER
00837          MOVE EX-BA-PAYEE-TYPE-CD TO WS-D2-PAYEE-TYPE
122402     END-EVALUATE
           MOVE WS-D2-PAYEE-TYPE       TO EXT-PAYEE-TYPE
00838                                                                   EL317
00839      MOVE EX-BA-INSURED-LAST-NAME TO WS-D2-INSURED-LAST-NAME
                                           EXT-LAST-NAME
00840                                                                   EL317
00841      IF EX-BA-VOID-DT NOT = LOW-VALUES                            EL317
00842          MOVE '*** V O I D ***'  TO  WS-D2-MESSAGE
                                           EXT-MESSAGE
           END-IF
00843                                                                   EL317
00844      IF EX-BA-CERT-STATUS = '9'                                   EL317
052804         MOVE '** REIN ONLY '  TO WS-D2-MESSAGE2.                 EL317
00846                                                                   EL317
071403     IF EX-BA-PAID-THRU-DT NOT = LOW-VALUES
071403         MOVE EX-BA-PAID-THRU-DT  TO DC-BIN-DATE-1    
071403         MOVE SPACES              TO DC-OPTION-CODE   
071403         PERFORM 8500-DATE-CONVERSION                             EL317
071403         MOVE DC-GREG-DATE-1-EDIT TO  WS-D2-PAID-THRU-DT
                                            EXT-PAID-THRU-DT
051104     END-IF
051104
052804     MOVE EX-BA-REPORT-CODE-1     TO WS-D2-REPORT-CODE-1
                                           EXT-REPORT-CODE-1
051104     MOVE EX-BA-REPORT-CODE-2     TO WS-D2-REPORT-CODE-2
                                           EXT-REPORT-CODE-2

00847      MOVE WS-DETAIL1   TO  PRT.                                   EL317
00848      PERFORM WRITE-A-LINE
           WRITE RPT-EXTRACT-RECORD    FROM EXTRACT-RECORD
00849                                                                   EL317
00850      EJECT                                                        EL317
00851 *    NOTE ******************************************************* EL317
00852 *         *                                                     * EL317
00853 *         *      ACCUMULATE THE PAYMENT AMOUNT AND COUNTS FOR   * EL317
00854 *         *  THE CURRENT PAY PERIOD AND ALSO FOR ANY PAYMENTS   * EL317
00855 *         *  PRIOR TO THE CURRENT PAY PERIOD.                   * EL317
00856 *         *                                                     * EL317
00857 *         *      PAYMENT                                        * EL317
00858 *         *       TYPE     MEANING                              * EL317
00859 *         *                                                     * EL317
00860 *         *        1       PARTIAL PAYMENT                      * EL317
00861 *         *        2       FINAL PAYMENT                        * EL317
00862 *         *        3       LUMP SUM PAYMENT                     * EL317
00863 *         *        4       ADDITIONAL PAYMENT                   * EL317
00864 *         *        5       CHARGABLE EXPENSE PAYMENT            * EL317
00865 *         *        6       NON-CHARGEABLE EXPENSE PAYMENT       * EL317
00866 *         *        7       LIFE PREMIUM REFUND                  * EL317
00867 *         *        8       A & H PREMIUM REFUND                 * EL317
00868 *         *******************************************************.EL317
00869                                                                   EL317
00870 *    NOTE ******************************************************* EL317
00871 *         *     IF CLAIM IS REINS ONLY, ACCUMULATE REINSURANCE  * EL317
00872 *         *     TOTALS, BUT BYPASS G/L PROCESSING.              * EL317
00873 *         *******************************************************.EL317
00874                                                                   EL317
00875      IF EX-BA-CERT-STATUS = '9'                                   EL317
00876          PERFORM 3900-ACCUM-REINSURANCE THRU 3999-EXIT            EL317
00877          GO TO 3100-PRINT-REPORT.                                 EL317
00878                                                                   EL317
00879  3200-CURRENT.                                                    EL317
00880                                                                   EL317
00881      IF EX-BA-PAYMENT-TYPE = '1' OR '2' OR '3' OR '4'             EL317
122402         EVALUATE TRUE
122402         WHEN EX-BA-CLAIM-TYPE = AH-OVERRIDE-L1 AND 
122402              EX-BA-VOID-DT = LOW-VALUES   
00884              ADD +1          TO  CT-CURR-AH-PMTS-CNT    
00885              ADD EX-BA-PAYMENT-AMOUNT                  
00886                              TO  CT-CURR-AH-PMTS-AMT  

122402         WHEN EX-BA-CLAIM-TYPE = 'I' AND 
122402              EX-BA-VOID-DT = LOW-VALUES   
122402             ADD +1          TO  CT-CURR-IU-PMTS-CNT    
122402             ADD EX-BA-PAYMENT-AMOUNT                  
122402                             TO  CT-CURR-IU-PMTS-AMT  
121203      
121203         WHEN EX-BA-CLAIM-TYPE = 'G' AND 
121203              EX-BA-VOID-DT = LOW-VALUES   
121203             ADD +1          TO  CT-CURR-GP-PMTS-CNT    
121203             ADD EX-BA-PAYMENT-AMOUNT                  
121203                             TO  CT-CURR-GP-PMTS-AMT  
052614
052614         WHEN EX-BA-CLAIM-TYPE = 'F' AND 
052614              EX-BA-VOID-DT = LOW-VALUES   
052614             ADD +1          TO  CT-CURR-FL-PMTS-CNT    
052614             ADD EX-BA-PAYMENT-AMOUNT                  
052614                             TO  CT-CURR-FL-PMTS-AMT  
100518
100518         WHEN EX-BA-CLAIM-TYPE = 'O' AND
100518              EX-BA-VOID-DT = LOW-VALUES
100518             ADD +1          TO  CT-CURR-OT-PMTS-CNT
100518             ADD EX-BA-PAYMENT-AMOUNT
100518                             TO  CT-CURR-OT-PMTS-AMT

022122         WHEN EX-BA-CLAIM-TYPE = 'B' AND
022122              EX-BA-VOID-DT = LOW-VALUES
022122             ADD +1          TO  CT-CURR-BR-PMTS-CNT
022122             ADD EX-BA-PAYMENT-AMOUNT
022122                             TO  CT-CURR-BR-PMTS-AMT
022122
022122         WHEN EX-BA-CLAIM-TYPE = 'H' AND
022122              EX-BA-VOID-DT = LOW-VALUES
022122             ADD +1          TO  CT-CURR-HS-PMTS-CNT
022122             ADD EX-BA-PAYMENT-AMOUNT
022122                             TO  CT-CURR-HS-PMTS-AMT
122402      
122402         WHEN EX-BA-CLAIM-TYPE = AH-OVERRIDE-L1 AND
122402              EX-BA-VOID-DT NOT = LOW-VALUES
00888              ADD +1          TO  CT-CURR-AH-PMTS-VOIDED-CNT  
00889              ADD EX-BA-PAYMENT-AMOUNT                       
00890                              TO  CT-CURR-AH-PMTS-VOIDED-AMT

122402         WHEN EX-BA-CLAIM-TYPE = 'I' AND
122402              EX-BA-VOID-DT NOT = LOW-VALUES
122402             ADD +1          TO  CT-CURR-IU-PMTS-VOIDED-CNT  
122402             ADD EX-BA-PAYMENT-AMOUNT                       
122402                             TO  CT-CURR-IU-PMTS-VOIDED-AMT
121203
121203         WHEN EX-BA-CLAIM-TYPE = 'G' AND
121203              EX-BA-VOID-DT NOT = LOW-VALUES
121203             ADD +1          TO  CT-CURR-GP-PMTS-VOIDED-CNT  
121203             ADD EX-BA-PAYMENT-AMOUNT                       
121203                             TO  CT-CURR-GP-PMTS-VOIDED-AMT
052614
052614         WHEN EX-BA-CLAIM-TYPE = 'F' AND
052614              EX-BA-VOID-DT NOT = LOW-VALUES
052614             ADD +1          TO  CT-CURR-FL-PMTS-VOIDED-CNT  
052614             ADD EX-BA-PAYMENT-AMOUNT                       
052614                             TO  CT-CURR-FL-PMTS-VOIDED-AMT
100518
100518         WHEN EX-BA-CLAIM-TYPE = 'O' AND
100518              EX-BA-VOID-DT NOT = LOW-VALUES
100518             ADD +1          TO  CT-CURR-OT-PMTS-VOIDED-CNT
100518             ADD EX-BA-PAYMENT-AMOUNT
100518                             TO  CT-CURR-OT-PMTS-VOIDED-AMT

100518         WHEN EX-BA-CLAIM-TYPE = 'B' AND
100518              EX-BA-VOID-DT NOT = LOW-VALUES
100518             ADD +1          TO  CT-CURR-BR-PMTS-VOIDED-CNT
100518             ADD EX-BA-PAYMENT-AMOUNT
100518                             TO  CT-CURR-BR-PMTS-VOIDED-AMT

100518         WHEN EX-BA-CLAIM-TYPE = 'H' AND
100518              EX-BA-VOID-DT NOT = LOW-VALUES
100518             ADD +1          TO  CT-CURR-HS-PMTS-VOIDED-CNT
100518             ADD EX-BA-PAYMENT-AMOUNT
100518                             TO  CT-CURR-HS-PMTS-VOIDED-AMT

122402         WHEN EX-BA-CLAIM-TYPE = LIFE-OVERRIDE-L1 AND
122402              EX-BA-VOID-DT = LOW-VALUES            
00893              ADD +1          TO  CT-CURR-LF-PMTS-CNT   
00894              ADD EX-BA-PAYMENT-AMOUNT                 
00895                              TO  CT-CURR-LF-PMTS-AMT 

122402         WHEN EX-BA-CLAIM-TYPE = LIFE-OVERRIDE-L1 AND
122402              EX-BA-VOID-DT NOT = LOW-VALUES            
00893              ADD +1          TO  CT-CURR-LF-PMTS-VOIDED-CNT   
00894              ADD EX-BA-PAYMENT-AMOUNT                 
00895                              TO  CT-CURR-LF-PMTS-VOIDED-AMT
122402         END-EVALUATE

00900      ELSE                                                         EL317
00901          IF  EX-BA-PAYMENT-TYPE = '5'                             EL317
00902              IF EX-BA-VOID-DT = LOW-VALUES                        EL317
00903                  ADD +1  TO  CT-CURR-CHG-EXP-CNT                  EL317
00904                  ADD EX-BA-PAYMENT-AMOUNT                         EL317
00905                          TO  CT-CURR-CHG-EXP-AMT                  EL317
00906              ELSE                                                 EL317
00907                  ADD +1  TO  CT-CURR-CHG-EXP-VOIDED-CNT           EL317
00908                  ADD EX-BA-PAYMENT-AMOUNT                         EL317
00909                          TO  CT-CURR-CHG-EXP-VOIDED-AMT           EL317
121203             END-IF
00910          ELSE                                                     EL317
00911              IF EX-BA-PAYMENT-TYPE = '6'                          EL317
00912                  IF EX-BA-VOID-DT = LOW-VALUES                    EL317
00913                      ADD +1  TO  CT-CURR-OTHER-EXP-CNT            EL317
00914                      ADD EX-BA-PAYMENT-AMOUNT                     EL317
00915                              TO  CT-CURR-OTHER-EXP-AMT            EL317
052912                 ELSE
00917                     ADD +1  TO  CT-CURR-OTHER-EXP-VOIDED-CNT
00918                     ADD EX-BA-PAYMENT-AMOUNT
00919                          TO  CT-CURR-OTHER-EXP-VOIDED-AMT
                       END-IF
121203             END-IF
121203         END-IF
121203     END-IF.    
00920                                                                   EL317
00921      IF EX-BA-VOID-DT = LOW-VALUES                                EL317
00922          IF EX-BA-PAYMENT-ORIGIN = '1' OR '2'                     EL317
122402            EVALUATE TRUE
122402            WHEN EX-BA-CLAIM-TYPE = LIFE-OVERRIDE-L1  
00924                 ADD EX-BA-PAYMENT-AMOUNT                          EL317
00925                         TO  CT-CURR-NET-PAID-ONLINE-AMT-LF        EL317
00926                 ADD +1  TO  CT-CURR-NET-PAID-ONLINE-CNT-LF        EL317

122402            WHEN EX-BA-CLAIM-TYPE = AH-OVERRIDE-L1
00928                 ADD EX-BA-PAYMENT-AMOUNT                          EL317
00929                         TO  CT-CURR-NET-PAID-ONLINE-AMT-AH        EL317
00930                 ADD +1  TO  CT-CURR-NET-PAID-ONLINE-CNT-AH        EL317

122402            WHEN EX-BA-CLAIM-TYPE = 'I'
122402                ADD EX-BA-PAYMENT-AMOUNT                          EL317
122402                        TO  CT-CURR-NET-PAID-ONLINE-AMT-IU        EL317
00930                 ADD +1  TO  CT-CURR-NET-PAID-ONLINE-CNT-IU        EL317
121203
121203            WHEN EX-BA-CLAIM-TYPE = 'G'
121203                ADD EX-BA-PAYMENT-AMOUNT                          EL317
121203                        TO  CT-CURR-NET-PAID-ONLINE-AMT-GP        EL317
121203                ADD +1  TO  CT-CURR-NET-PAID-ONLINE-CNT-GP        EL317
052614
052614            WHEN EX-BA-CLAIM-TYPE = 'F'
052614                ADD EX-BA-PAYMENT-AMOUNT
052614                        TO  CT-CURR-NET-PAID-ONLINE-AMT-FL
052614                ADD +1  TO  CT-CURR-NET-PAID-ONLINE-CNT-FL
100518
022122            WHEN EX-BA-CLAIM-TYPE = 'B'
022122                ADD EX-BA-PAYMENT-AMOUNT
022122                        TO  CT-CURR-NET-PAID-ONLINE-AMT-BR
022122                ADD +1  TO  CT-CURR-NET-PAID-ONLINE-CNT-BR
022122
022122            WHEN EX-BA-CLAIM-TYPE = 'H'
022122                ADD EX-BA-PAYMENT-AMOUNT
022122                        TO  CT-CURR-NET-PAID-ONLINE-AMT-HS
022122                ADD +1  TO  CT-CURR-NET-PAID-ONLINE-CNT-HS
100518
100518            WHEN EX-BA-CLAIM-TYPE = 'O'
100518                ADD EX-BA-PAYMENT-AMOUNT
100518                        TO  CT-CURR-NET-PAID-ONLINE-AMT-OT
100518                ADD +1  TO  CT-CURR-NET-PAID-ONLINE-CNT-OT
122402            END-EVALUATE

00931          ELSE                                                     EL317
122402            EVALUATE TRUE
122402            WHEN EX-BA-CLAIM-TYPE = LIFE-OVERRIDE-L1                EL317
00933                ADD EX-BA-PAYMENT-AMOUNT                           EL317
00934                        TO  CT-CURR-NET-PD-OFFLINE-AMT-LF          EL317
00935                ADD +1  TO  CT-CURR-NET-PD-OFFLINE-CNT-LF          EL317

122402            WHEN EX-BA-CLAIM-TYPE = AH-OVERRIDE-L1
00937                ADD EX-BA-PAYMENT-AMOUNT                           EL317
00938                        TO  CT-CURR-NET-PD-OFFLINE-AMT-AH          EL317
00939                ADD +1  TO  CT-CURR-NET-PD-OFFLINE-CNT-AH          EL317

122402            WHEN EX-BA-CLAIM-TYPE = 'I'
00937                ADD EX-BA-PAYMENT-AMOUNT                           EL317
00938                        TO  CT-CURR-NET-PD-OFFLINE-AMT-IU          EL317
00939                ADD +1  TO  CT-CURR-NET-PD-OFFLINE-CNT-IU

121203            WHEN EX-BA-CLAIM-TYPE = 'G'
121203               ADD EX-BA-PAYMENT-AMOUNT                           EL317
121203                       TO  CT-CURR-NET-PD-OFFLINE-AMT-GP          EL317
121203               ADD +1  TO  CT-CURR-NET-PD-OFFLINE-CNT-GP
052614
052614            WHEN EX-BA-CLAIM-TYPE = 'F'
052614               ADD EX-BA-PAYMENT-AMOUNT
052614                       TO  CT-CURR-NET-PD-OFFLINE-AMT-FL
052614               ADD +1  TO  CT-CURR-NET-PD-OFFLINE-CNT-FL
100518
022122            WHEN EX-BA-CLAIM-TYPE = 'B'
022122               ADD EX-BA-PAYMENT-AMOUNT
022122                       TO  CT-CURR-NET-PD-OFFLINE-AMT-BR
022122               ADD +1  TO  CT-CURR-NET-PD-OFFLINE-CNT-BR
022122
022122            WHEN EX-BA-CLAIM-TYPE = 'H'
022122               ADD EX-BA-PAYMENT-AMOUNT
022122                       TO  CT-CURR-NET-PD-OFFLINE-AMT-HS
022122               ADD +1  TO  CT-CURR-NET-PD-OFFLINE-CNT-HS
100518
100518            WHEN EX-BA-CLAIM-TYPE = 'O'
100518               ADD EX-BA-PAYMENT-AMOUNT
100518                       TO  CT-CURR-NET-PD-OFFLINE-AMT-OT
100518               ADD +1  TO  CT-CURR-NET-PD-OFFLINE-CNT-OT
122402            END-EVALUATE
121203         END-IF
00940      ELSE                                                         EL317
00941          IF EX-BA-PAYMENT-ORIGIN = '1' OR '2'                     EL317
122402            EVALUATE TRUE
122402            WHEN EX-BA-CLAIM-TYPE = LIFE-OVERRIDE-L1  
00943                SUBTRACT EX-BA-PAYMENT-AMOUNT                      EL317
00944                             FROM  CT-CURR-NET-PAID-ONLINE-AMT-LF  EL317
00945                SUBTRACT +1  FROM  CT-CURR-NET-PAID-ONLINE-CNT-LF  EL317

122402            WHEN EX-BA-CLAIM-TYPE = AH-OVERRIDE-L1
00947                SUBTRACT EX-BA-PAYMENT-AMOUNT                      EL317
00948                             FROM  CT-CURR-NET-PAID-ONLINE-AMT-AH  EL317
00949                SUBTRACT +1  FROM  CT-CURR-NET-PAID-ONLINE-CNT-AH  EL317

122402            WHEN EX-BA-CLAIM-TYPE = 'I'            
122402               SUBTRACT EX-BA-PAYMENT-AMOUNT                      EL317
122402                            FROM  CT-CURR-NET-PAID-ONLINE-AMT-IU  EL317
122402               SUBTRACT +1  FROM  CT-CURR-NET-PAID-ONLINE-CNT-IU  EL317
121203
121203            WHEN EX-BA-CLAIM-TYPE = 'G'            
121203               SUBTRACT EX-BA-PAYMENT-AMOUNT                      EL317
121203                            FROM  CT-CURR-NET-PAID-ONLINE-AMT-GP  EL317
121203               SUBTRACT +1  FROM  CT-CURR-NET-PAID-ONLINE-CNT-GP  EL317
052614
052614            WHEN EX-BA-CLAIM-TYPE = 'F'
052614               SUBTRACT EX-BA-PAYMENT-AMOUNT
052614                            FROM  CT-CURR-NET-PAID-ONLINE-AMT-FL
052614               SUBTRACT +1  FROM  CT-CURR-NET-PAID-ONLINE-CNT-FL
100518
022122            WHEN EX-BA-CLAIM-TYPE = 'B'
022122               SUBTRACT EX-BA-PAYMENT-AMOUNT
022122                            FROM  CT-CURR-NET-PAID-ONLINE-AMT-BR
022122               SUBTRACT +1  FROM  CT-CURR-NET-PAID-ONLINE-CNT-BR
022122
022122            WHEN EX-BA-CLAIM-TYPE = 'H'
022122               SUBTRACT EX-BA-PAYMENT-AMOUNT
022122                            FROM  CT-CURR-NET-PAID-ONLINE-AMT-HS
022122               SUBTRACT +1  FROM  CT-CURR-NET-PAID-ONLINE-CNT-HS
100518
100518            WHEN EX-BA-CLAIM-TYPE = 'O'
100518               SUBTRACT EX-BA-PAYMENT-AMOUNT
100518                            FROM  CT-CURR-NET-PAID-ONLINE-AMT-OT
100518               SUBTRACT +1  FROM  CT-CURR-NET-PAID-ONLINE-CNT-OT
122402            END-EVALUATE

00950          ELSE                                                     EL317
122402            EVALUATE TRUE
122402            WHEN EX-BA-CLAIM-TYPE = LIFE-OVERRIDE-L1                EL317
00952                SUBTRACT EX-BA-PAYMENT-AMOUNT                      EL317
00953                             FROM  CT-CURR-NET-PD-OFFLINE-AMT-LF   EL317
00954                SUBTRACT +1  FROM  CT-CURR-NET-PD-OFFLINE-CNT-LF   EL317

122402            WHEN EX-BA-CLAIM-TYPE = AH-OVERRIDE-L1
00956                SUBTRACT EX-BA-PAYMENT-AMOUNT                      EL317
00957                             FROM  CT-CURR-NET-PD-OFFLINE-AMT-AH   EL317
00958                SUBTRACT +1  FROM  CT-CURR-NET-PD-OFFLINE-CNT-AH
00959                                                                   EL317
122402            WHEN EX-BA-CLAIM-TYPE = 'I'           
122402               SUBTRACT EX-BA-PAYMENT-AMOUNT                      EL317
122402                            FROM  CT-CURR-NET-PD-OFFLINE-AMT-IU   EL317
122402               SUBTRACT +1  FROM  CT-CURR-NET-PD-OFFLINE-CNT-IU
121203
121203            WHEN EX-BA-CLAIM-TYPE = 'G'           
121203               SUBTRACT EX-BA-PAYMENT-AMOUNT                      EL317
121203                            FROM  CT-CURR-NET-PD-OFFLINE-AMT-GP   EL317
121203               SUBTRACT +1  FROM  CT-CURR-NET-PD-OFFLINE-CNT-GP
052614
052614            WHEN EX-BA-CLAIM-TYPE = 'F'
052614               SUBTRACT EX-BA-PAYMENT-AMOUNT
052614                            FROM  CT-CURR-NET-PD-OFFLINE-AMT-FL
052614               SUBTRACT +1  FROM  CT-CURR-NET-PD-OFFLINE-CNT-FL
100518
022122            WHEN EX-BA-CLAIM-TYPE = 'B'
022122               SUBTRACT EX-BA-PAYMENT-AMOUNT
022122                            FROM  CT-CURR-NET-PD-OFFLINE-AMT-BR
022122               SUBTRACT +1  FROM  CT-CURR-NET-PD-OFFLINE-CNT-BR
022122
022122            WHEN EX-BA-CLAIM-TYPE = 'H'
022122               SUBTRACT EX-BA-PAYMENT-AMOUNT
022122                            FROM  CT-CURR-NET-PD-OFFLINE-AMT-HS
022122               SUBTRACT +1  FROM  CT-CURR-NET-PD-OFFLINE-CNT-HS
100518
100518            WHEN EX-BA-CLAIM-TYPE = 'O'
100518               SUBTRACT EX-BA-PAYMENT-AMOUNT
100518                            FROM  CT-CURR-NET-PD-OFFLINE-AMT-OT
100518               SUBTRACT +1  FROM  CT-CURR-NET-PD-OFFLINE-CNT-OT
122402            END-EVALUATE
121203         END-IF 
121203     END-IF.         
00959                                                                   EL317
052614     IF EX-BA-CLAIM-TYPE = AH-OVERRIDE-L1 OR 'I' OR 'G' OR 'F'
022122        OR 'B' OR 'H'
00961          PERFORM 3700-ACCUMULATE-BENEFIT                          EL317
00962                  VARYING CLAS-INDEX FROM CLAS-STARTA BY +1        EL317
00963                  UNTIL CLAS-INDEX > CLAS-MAXA                     EL317
00964      ELSE                                                         EL317
00965          PERFORM 3700-ACCUMULATE-BENEFIT                          EL317
00966                  VARYING CLAS-INDEX FROM CLAS-STARTL BY +1        EL317
00967                  UNTIL CLAS-INDEX > CLAS-MAXL
121203     END-IF.
00968                                                                   EL317
00972  3500-G-L-PROCESSING.                                             EL317
00973                                                                   EL317
00974 *    NOTE ******************************************************* EL317
00975 *         *          DETAIL G/L ENTRY PROCESSING                * EL317
00976 *         *    ADD CLIENT TEST FOR CLIENTS NEEDING GL OUTPUT    * EL317
00977 *         *******************************************************.EL317
00978                                                                   EL317
122402*    IF DTE-CLIENT = 'XXX'                                        EL317
122402*        NEXT SENTENCE                                            EL317
122402*    ELSE                                                         EL317
00982          GO TO 3100-PRINT-REPORT.                                 EL317
00983                                                                   EL317
122402*    MOVE EX-SB-CHECK-NO         TO  3800-CHECK-NUMBER.           EL317
122402*    MOVE EX-BA-ACCOUNT          TO  3800-ACCOUNT.                EL317
122402*    MOVE EX-BA-CERT-NO-PRE      TO  3800-CERT-NO.                EL317
122402*    MOVE EX-BA-CLAIM-TYPE       TO  3800-DESC-L-H.               EL317
122402*    MOVE EX-BA-STATE            TO  3800-STATE.                  EL317
122402*    MOVE EX-BA-PAYMENT-AMOUNT   TO  3800-AMOUNT.                 EL317
122402*                                                                 EL317
122402*    IF (EX-BA-PAYMENT-AMOUNT < ZERO)   OR                        EL317
122402*       (EX-BA-VOID-DT NOT = LOW-VALUES)                          EL317
122402*         MOVE 'C'  TO  3800-DEBIT-CREDIT                         EL317
122402*    ELSE                                                         EL317
122402*         MOVE 'D'  TO  3800-DEBIT-CREDIT.                        EL317
122402*                                                                 EL317
122402*    MOVE '2310948' TO  3800-GENERAL-LEDGER.                      EL317
122402*                                                                 EL317
122402*    WRITE ABC-INTERFACE-RECORD FROM WS-3800-CARD.                EL317
122402*                                                                 EL317
122402*    IF (EX-BA-PAYMENT-AMOUNT < ZERO)   OR                        EL317
122402*       (EX-BA-VOID-DT NOT = LOW-VALUES)                          EL317
122402*        MOVE 'D'                TO  3800-DEBIT-CREDIT            EL317
122402*    ELSE                                                         EL317
122402*        MOVE 'C'                TO  3800-DEBIT-CREDIT.           EL317
122402*                                                                 EL317
122402*    MOVE '1360145'          TO  3800-GENERAL-LEDGER.             EL317
122402*                                                                 EL317
122402*    WRITE ABC-INTERFACE-RECORD FROM WS-3800-CARD.                EL317
122402*                                                                 EL317
122402*    GO TO 3100-PRINT-REPORT.                                     EL317
01012                                                                   EL317
01013  3600-EXIT.                                                       EL317
01014      EXIT.                                                        EL317
01015                                                                   EL317
01016      EJECT                                                        EL317
01017  3700-ACCUMULATE-BENEFIT SECTION.                                 EL317
01018                                                                   EL317
01019      IF EX-BA-BENEFIT-CD NOT = CLAS-I-BEN (CLAS-INDEX)            EL317
01020          GO TO 3799-EXIT.                                         EL317
01021                                                                   EL317
01022      IF EX-BA-VOID-DT = LOW-VALUES                                EL317
01023          ADD EX-BA-PAYMENT-AMOUNT                                 EL317
01024                      TO  CT-CURR-BENEFIT-AMT (CLAS-INDEX)         EL317
01025          ADD +1  TO  CT-CURR-BENEFIT-CNT (CLAS-INDEX)             EL317
01026      ELSE                                                         EL317
01027          ADD EX-BA-PAYMENT-AMOUNT                                 EL317
01028                      TO  CT-CURR-BENEFIT-VOIDED-AMT (CLAS-INDEX)  EL317
01029          ADD +1  TO  CT-CURR-BENEFIT-VOIDED-CNT (CLAS-INDEX).     EL317
01030                                                                   EL317
092602     ADD +900                    TO  CLAS-INDEX.                  EL317
01032                                                                   EL317
01033  3799-EXIT.                                                       EL317
01034      EXIT.                                                        EL317
01035                                                                   EL317
01036      EJECT                                                        EL317
01037  3800-ACCUMULATE-PAYEE-TOTALS SECTION.                            EL317
01038                                                                   EL317
121203     IF EX-BA-CLAIM-TYPE NOT = AH-OVERRIDE-L1 AND 'I' AND 'G'
052614            AND 'F' AND 'B' AND 'H'
01040          GO TO 3850-ACCUM-LIFE-PAYEE                              EL317
01041      ELSE                                                         EL317
01042          IF EX-BA-VOID-DT NOT = LOW-VALUES                        EL317
01043              GO TO 3825-ACCUM-AH-VOID
121203     END-IF.
01044                                                                   EL317
122402     EVALUATE TRUE
122402     WHEN EX-BA-PAYEE-TYPE-CD = 'I' AND
122402          EX-BA-CLAIM-TYPE = AH-OVERRIDE-L1
01046          ADD +1              TO  CPT-PMT-CNTS (INSD-NDX AH-NDX)   EL317
01047          ADD EX-BA-PAYMENT-AMOUNT                                 EL317
01048                              TO  CPT-PMT-AMTS (INSD-NDX AH-NDX)   EL317

122402     WHEN EX-BA-PAYEE-TYPE-CD = 'I' AND
122402          EX-BA-CLAIM-TYPE = 'I'           
122402         ADD +1              TO  CPT-PMT-CNTS (INSD-NDX IU-NDX)   EL317
122402         ADD EX-BA-PAYMENT-AMOUNT                                 EL317
122402                             TO  CPT-PMT-AMTS (INSD-NDX IU-NDX)   EL317

121203     WHEN EX-BA-PAYEE-TYPE-CD = 'I' AND
121203          EX-BA-CLAIM-TYPE = 'G'           
121203         ADD +1              TO  CPT-PMT-CNTS (INSD-NDX GP-NDX)   EL317
121203         ADD EX-BA-PAYMENT-AMOUNT                                 EL317
121203                             TO  CPT-PMT-AMTS (INSD-NDX GP-NDX)   EL317
052614
052614     WHEN EX-BA-PAYEE-TYPE-CD = 'I' AND
052614          EX-BA-CLAIM-TYPE = 'F'
052614         ADD +1              TO  CPT-PMT-CNTS (INSD-NDX FL-NDX)
052614         ADD EX-BA-PAYMENT-AMOUNT
052614                             TO  CPT-PMT-AMTS (INSD-NDX FL-NDX)

022122     WHEN EX-BA-PAYEE-TYPE-CD = 'I' AND
022122          EX-BA-CLAIM-TYPE = 'B'
022122         ADD +1              TO  CPT-PMT-CNTS (INSD-NDX BR-NDX)
022122         ADD EX-BA-PAYMENT-AMOUNT
022122                             TO  CPT-PMT-AMTS (INSD-NDX BR-NDX)
022122
022122     WHEN EX-BA-PAYEE-TYPE-CD = 'I' AND
022122          EX-BA-CLAIM-TYPE = 'H'
022122         ADD +1              TO  CPT-PMT-CNTS (INSD-NDX HS-NDX)
022122         ADD EX-BA-PAYMENT-AMOUNT
022122                             TO  CPT-PMT-AMTS (INSD-NDX HS-NDX)

122402     WHEN EX-BA-PAYEE-TYPE-CD = 'B' AND 
122402          EX-BA-CLAIM-TYPE = AH-OVERRIDE-L1
01051          ADD +1              TO  CPT-PMT-CNTS (BENE-NDX AH-NDX)   EL317
01052          ADD EX-BA-PAYMENT-AMOUNT                                 EL317
01053                              TO  CPT-PMT-AMTS (BENE-NDX AH-NDX)   EL317

122402     WHEN EX-BA-PAYEE-TYPE-CD = 'B' AND 
122402          EX-BA-CLAIM-TYPE = 'I'           
122402         ADD +1              TO  CPT-PMT-CNTS (BENE-NDX IU-NDX)   EL317
122402         ADD EX-BA-PAYMENT-AMOUNT                                 EL317
122402                             TO  CPT-PMT-AMTS (BENE-NDX IU-NDX)   EL317

121203     WHEN EX-BA-PAYEE-TYPE-CD = 'B' AND 
121203          EX-BA-CLAIM-TYPE = 'G'           
121203         ADD +1              TO  CPT-PMT-CNTS (BENE-NDX GP-NDX)   EL317
121203         ADD EX-BA-PAYMENT-AMOUNT                                 EL317
121203                             TO  CPT-PMT-AMTS (BENE-NDX GP-NDX)   EL317
052614
052614     WHEN EX-BA-PAYEE-TYPE-CD = 'B' AND
052614          EX-BA-CLAIM-TYPE = 'F'
052614         ADD +1              TO  CPT-PMT-CNTS (BENE-NDX FL-NDX)
052614         ADD EX-BA-PAYMENT-AMOUNT
052614                             TO  CPT-PMT-AMTS (BENE-NDX FL-NDX)

022122     WHEN EX-BA-PAYEE-TYPE-CD = 'B' AND
022122          EX-BA-CLAIM-TYPE = 'B'
022122         ADD +1              TO  CPT-PMT-CNTS (BENE-NDX BR-NDX)
022122         ADD EX-BA-PAYMENT-AMOUNT
022122                             TO  CPT-PMT-AMTS (BENE-NDX BR-NDX)
022122
022122     WHEN EX-BA-PAYEE-TYPE-CD = 'B' AND
022122          EX-BA-CLAIM-TYPE = 'H'
022122         ADD +1              TO  CPT-PMT-CNTS (BENE-NDX HS-NDX)
022122         ADD EX-BA-PAYMENT-AMOUNT
022122                             TO  CPT-PMT-AMTS (BENE-NDX HS-NDX)

122402     WHEN EX-BA-PAYEE-TYPE-CD = 'A' AND
122402          EX-BA-CLAIM-TYPE = AH-OVERRIDE-L1 
01056          ADD +1              TO  CPT-PMT-CNTS (ACCT-NDX AH-NDX)   EL317
01057          ADD EX-BA-PAYMENT-AMOUNT                                 EL317
01058                              TO  CPT-PMT-AMTS (ACCT-NDX AH-NDX)   EL317

122402     WHEN EX-BA-PAYEE-TYPE-CD = 'A' AND
122402          EX-BA-CLAIM-TYPE = 'I'            
122402         ADD +1              TO  CPT-PMT-CNTS (ACCT-NDX IU-NDX)   EL317
122402         ADD EX-BA-PAYMENT-AMOUNT                                 EL317
122402                             TO  CPT-PMT-AMTS (ACCT-NDX IU-NDX)   EL317

122402     WHEN EX-BA-PAYEE-TYPE-CD = 'P' AND
122402          EX-BA-CLAIM-TYPE = AH-OVERRIDE-L1 
01061          ADD +1              TO  CPT-PMT-CNTS (PHYS-NDX AH-NDX)   EL317
01062          ADD EX-BA-PAYMENT-AMOUNT                                 EL317
01063                              TO  CPT-PMT-AMTS (PHYS-NDX AH-NDX)   EL317

121203     WHEN EX-BA-PAYEE-TYPE-CD = 'A' AND
121203          EX-BA-CLAIM-TYPE = 'G'            
121203         ADD +1              TO  CPT-PMT-CNTS (ACCT-NDX GP-NDX)   EL317
121203         ADD EX-BA-PAYMENT-AMOUNT                                 EL317
121203                             TO  CPT-PMT-AMTS (ACCT-NDX GP-NDX)   EL317
052614
052614     WHEN EX-BA-PAYEE-TYPE-CD = 'A' AND
052614          EX-BA-CLAIM-TYPE = 'F'
052614         ADD +1              TO  CPT-PMT-CNTS (ACCT-NDX FL-NDX)
052614         ADD EX-BA-PAYMENT-AMOUNT
052614                             TO  CPT-PMT-AMTS (ACCT-NDX FL-NDX)

022122     WHEN EX-BA-PAYEE-TYPE-CD = 'A' AND
022122          EX-BA-CLAIM-TYPE = 'B'
022122         ADD +1              TO  CPT-PMT-CNTS (ACCT-NDX BR-NDX)
022122         ADD EX-BA-PAYMENT-AMOUNT
022122                             TO  CPT-PMT-AMTS (ACCT-NDX BR-NDX)
022122
022122     WHEN EX-BA-PAYEE-TYPE-CD = 'A' AND
022122          EX-BA-CLAIM-TYPE = 'H'
022122         ADD +1              TO  CPT-PMT-CNTS (ACCT-NDX HS-NDX)
022122         ADD EX-BA-PAYMENT-AMOUNT
022122                             TO  CPT-PMT-AMTS (ACCT-NDX HS-NDX)

122402     WHEN EX-BA-PAYEE-TYPE-CD = 'P' AND
122402          EX-BA-CLAIM-TYPE = 'I'            
122402         ADD +1              TO  CPT-PMT-CNTS (PHYS-NDX IU-NDX)   EL317
122402         ADD EX-BA-PAYMENT-AMOUNT                                 EL317
122402                             TO  CPT-PMT-AMTS (PHYS-NDX IU-NDX)   EL317

121203     WHEN EX-BA-PAYEE-TYPE-CD = 'P' AND
121203          EX-BA-CLAIM-TYPE = 'G'            
121203         ADD +1              TO  CPT-PMT-CNTS (PHYS-NDX GP-NDX)   EL317
121203         ADD EX-BA-PAYMENT-AMOUNT                                 EL317
121203                             TO  CPT-PMT-AMTS (PHYS-NDX GP-NDX)   EL317
052614
052614     WHEN EX-BA-PAYEE-TYPE-CD = 'P' AND
052614          EX-BA-CLAIM-TYPE = 'F'
052614         ADD +1              TO  CPT-PMT-CNTS (PHYS-NDX FL-NDX)
052614         ADD EX-BA-PAYMENT-AMOUNT
052614                             TO  CPT-PMT-AMTS (PHYS-NDX FL-NDX)

022122     WHEN EX-BA-PAYEE-TYPE-CD = 'P' AND
022122          EX-BA-CLAIM-TYPE = 'B'
022122         ADD +1              TO  CPT-PMT-CNTS (PHYS-NDX BR-NDX)
022122         ADD EX-BA-PAYMENT-AMOUNT
022122                             TO  CPT-PMT-AMTS (PHYS-NDX BR-NDX)
022122
022122     WHEN EX-BA-PAYEE-TYPE-CD = 'P' AND
022122          EX-BA-CLAIM-TYPE = 'H'
022122         ADD +1              TO  CPT-PMT-CNTS (PHYS-NDX HS-NDX)
022122         ADD EX-BA-PAYMENT-AMOUNT
022122                             TO  CPT-PMT-AMTS (PHYS-NDX HS-NDX)

122402     WHEN EX-BA-PAYEE-TYPE-CD = 'O' AND
122402          EX-BA-CLAIM-TYPE = AH-OVERRIDE-L1 
01066          ADD +1              TO  CPT-PMT-CNTS (OTH1-NDX AH-NDX)   EL317
01067          ADD EX-BA-PAYMENT-AMOUNT                                 EL317
01068                              TO  CPT-PMT-AMTS (OTH1-NDX AH-NDX)   EL317

122402     WHEN EX-BA-PAYEE-TYPE-CD = 'O' AND
122402          EX-BA-CLAIM-TYPE = 'I'            
122402         ADD +1              TO  CPT-PMT-CNTS (OTH1-NDX IU-NDX)   EL317
122402         ADD EX-BA-PAYMENT-AMOUNT                                 EL317
122402                             TO  CPT-PMT-AMTS (OTH1-NDX IU-NDX)   EL317
 
121203     WHEN EX-BA-PAYEE-TYPE-CD = 'O' AND
121203          EX-BA-CLAIM-TYPE = 'G'            
121203         ADD +1              TO  CPT-PMT-CNTS (OTH1-NDX GP-NDX)   EL317
121203         ADD EX-BA-PAYMENT-AMOUNT                                 EL317
121203                             TO  CPT-PMT-AMTS (OTH1-NDX GP-NDX)   EL317
052614
052614     WHEN EX-BA-PAYEE-TYPE-CD = 'O' AND
052614          EX-BA-CLAIM-TYPE = 'F'
052614         ADD +1              TO  CPT-PMT-CNTS (OTH1-NDX FL-NDX)
052614         ADD EX-BA-PAYMENT-AMOUNT
052614                             TO  CPT-PMT-AMTS (OTH1-NDX FL-NDX)
 
022122     WHEN EX-BA-PAYEE-TYPE-CD = 'O' AND
022122          EX-BA-CLAIM-TYPE = 'B'
022122         ADD +1              TO  CPT-PMT-CNTS (OTH1-NDX BR-NDX)
022122         ADD EX-BA-PAYMENT-AMOUNT
022122                             TO  CPT-PMT-AMTS (OTH1-NDX BR-NDX)
022122
022122     WHEN EX-BA-PAYEE-TYPE-CD = 'O' AND
022122          EX-BA-CLAIM-TYPE = 'H'
022122         ADD +1              TO  CPT-PMT-CNTS (OTH1-NDX HS-NDX)
022122         ADD EX-BA-PAYMENT-AMOUNT
022122                             TO  CPT-PMT-AMTS (OTH1-NDX HS-NDX)
 
122402     WHEN EX-BA-CLAIM-TYPE = AH-OVERRIDE-L1
01070          ADD +1              TO  CPT-PMT-CNTS (OTH2-NDX AH-NDX)   EL317
01071          ADD EX-BA-PAYMENT-AMOUNT                                 EL317
01072                              TO  CPT-PMT-AMTS (OTH2-NDX AH-NDX)
01073                                                                   EL317
122402     WHEN EX-BA-CLAIM-TYPE = 'I'           
122402         ADD +1              TO  CPT-PMT-CNTS (OTH2-NDX IU-NDX)   EL317
122402         ADD EX-BA-PAYMENT-AMOUNT                                 EL317
122402                             TO  CPT-PMT-AMTS (OTH2-NDX IU-NDX)
121203
121203     WHEN EX-BA-CLAIM-TYPE = 'G'           
121203         ADD +1              TO  CPT-PMT-CNTS (OTH2-NDX GP-NDX)   EL317
121203         ADD EX-BA-PAYMENT-AMOUNT                                 EL317
121203                             TO  CPT-PMT-AMTS (OTH2-NDX GP-NDX)
052614
052614     WHEN EX-BA-CLAIM-TYPE = 'F'
052614         ADD +1              TO  CPT-PMT-CNTS (OTH2-NDX FL-NDX)
052614         ADD EX-BA-PAYMENT-AMOUNT
052614                             TO  CPT-PMT-AMTS (OTH2-NDX FL-NDX)

022122     WHEN EX-BA-CLAIM-TYPE = 'B'
022122         ADD +1              TO  CPT-PMT-CNTS (OTH2-NDX BR-NDX)
022122         ADD EX-BA-PAYMENT-AMOUNT
022122                             TO  CPT-PMT-AMTS (OTH2-NDX BR-NDX)
022122
022122     WHEN EX-BA-CLAIM-TYPE = 'H'
022122         ADD +1              TO  CPT-PMT-CNTS (OTH2-NDX HS-NDX)
022122         ADD EX-BA-PAYMENT-AMOUNT
022122                             TO  CPT-PMT-AMTS (OTH2-NDX HS-NDX)
122402     END-EVALUATE.
01073                                                                   EL317
01074  3825-ACCUM-AH-VOID.                                              EL317
01075                                                                   EL317
122402     EVALUATE TRUE
122402     WHEN EX-BA-PAYEE-TYPE-CD = 'I' AND
122402          EX-BA-CLAIM-TYPE = AH-OVERRIDE-L1   
01077          ADD +1         TO  CPT-PMT-VOID-CNTS (INSD-NDX AH-NDX)   EL317
01078          ADD EX-BA-PAYMENT-AMOUNT                                 EL317
01079                         TO  CPT-PMT-VOID-AMTS (INSD-NDX AH-NDX)   EL317

122402     WHEN EX-BA-PAYEE-TYPE-CD = 'I' AND
122402          EX-BA-CLAIM-TYPE = 'I'              
122402         ADD +1         TO  CPT-PMT-VOID-CNTS (INSD-NDX IU-NDX)   EL317
122402         ADD EX-BA-PAYMENT-AMOUNT                                 EL317
122402                        TO  CPT-PMT-VOID-AMTS (INSD-NDX IU-NDX)   EL317
121203
121203     WHEN EX-BA-PAYEE-TYPE-CD = 'I' AND
121203          EX-BA-CLAIM-TYPE = 'G'              
121203         ADD +1         TO  CPT-PMT-VOID-CNTS (INSD-NDX GP-NDX)   EL317
121203         ADD EX-BA-PAYMENT-AMOUNT                                 EL317
121203                        TO  CPT-PMT-VOID-AMTS (INSD-NDX GP-NDX)   EL317
052614
052614     WHEN EX-BA-PAYEE-TYPE-CD = 'I' AND
052614          EX-BA-CLAIM-TYPE = 'F'
052614         ADD +1         TO  CPT-PMT-VOID-CNTS (INSD-NDX FL-NDX)
052614         ADD EX-BA-PAYMENT-AMOUNT
052614                        TO  CPT-PMT-VOID-AMTS (INSD-NDX FL-NDX)
100518
022122     WHEN EX-BA-PAYEE-TYPE-CD = 'I' AND
022122          EX-BA-CLAIM-TYPE = 'B'
022122         ADD +1         TO  CPT-PMT-VOID-CNTS (INSD-NDX BR-NDX)
022122         ADD EX-BA-PAYMENT-AMOUNT
022122                        TO  CPT-PMT-VOID-AMTS (INSD-NDX BR-NDX)
022122
022122     WHEN EX-BA-PAYEE-TYPE-CD = 'I' AND
022122          EX-BA-CLAIM-TYPE = 'H'
022122         ADD +1         TO  CPT-PMT-VOID-CNTS (INSD-NDX HS-NDX)
022122         ADD EX-BA-PAYMENT-AMOUNT
022122                        TO  CPT-PMT-VOID-AMTS (INSD-NDX HS-NDX)
100518
100518     WHEN EX-BA-PAYEE-TYPE-CD = 'I' AND
100518          EX-BA-CLAIM-TYPE = 'O'
100518         ADD +1         TO  CPT-PMT-VOID-CNTS (INSD-NDX OT-NDX)
100518         ADD EX-BA-PAYMENT-AMOUNT
100518                        TO  CPT-PMT-VOID-AMTS (INSD-NDX OT-NDX)

122402     WHEN EX-BA-PAYEE-TYPE-CD = 'B' AND
122402          EX-BA-CLAIM-TYPE = AH-OVERRIDE-L1  
01082          ADD +1         TO  CPT-PMT-VOID-CNTS (BENE-NDX AH-NDX)   EL317
01083          ADD EX-BA-PAYMENT-AMOUNT                                 EL317
01084                         TO  CPT-PMT-VOID-AMTS (BENE-NDX AH-NDX)   EL317

122402     WHEN EX-BA-PAYEE-TYPE-CD = 'B' AND
122402          EX-BA-CLAIM-TYPE = 'I'             
122402         ADD +1         TO  CPT-PMT-VOID-CNTS (BENE-NDX IU-NDX)   EL317
122402         ADD EX-BA-PAYMENT-AMOUNT                                 EL317
122402                        TO  CPT-PMT-VOID-AMTS (BENE-NDX IU-NDX)   EL317
121203
121203     WHEN EX-BA-PAYEE-TYPE-CD = 'B' AND
121203          EX-BA-CLAIM-TYPE = 'G'             
121203         ADD +1         TO  CPT-PMT-VOID-CNTS (BENE-NDX GP-NDX)   EL317
121203         ADD EX-BA-PAYMENT-AMOUNT                                 EL317
121203                        TO  CPT-PMT-VOID-AMTS (BENE-NDX GP-NDX)   EL317
052614
052614     WHEN EX-BA-PAYEE-TYPE-CD = 'B' AND
052614          EX-BA-CLAIM-TYPE = 'F'
052614         ADD +1         TO  CPT-PMT-VOID-CNTS (BENE-NDX FL-NDX)
052614         ADD EX-BA-PAYMENT-AMOUNT
052614                        TO  CPT-PMT-VOID-AMTS (BENE-NDX FL-NDX)
100518
022122     WHEN EX-BA-PAYEE-TYPE-CD = 'B' AND
022122          EX-BA-CLAIM-TYPE = 'B'
022122         ADD +1         TO  CPT-PMT-VOID-CNTS (BENE-NDX BR-NDX)
022122         ADD EX-BA-PAYMENT-AMOUNT
022122                        TO  CPT-PMT-VOID-AMTS (BENE-NDX BR-NDX)
022122
022122     WHEN EX-BA-PAYEE-TYPE-CD = 'B' AND
022122          EX-BA-CLAIM-TYPE = 'H'
022122         ADD +1         TO  CPT-PMT-VOID-CNTS (BENE-NDX HS-NDX)
022122         ADD EX-BA-PAYMENT-AMOUNT
022122                        TO  CPT-PMT-VOID-AMTS (BENE-NDX HS-NDX)
100518
100518     WHEN EX-BA-PAYEE-TYPE-CD = 'B' AND
100518          EX-BA-CLAIM-TYPE = 'O'
100518         ADD +1         TO  CPT-PMT-VOID-CNTS (BENE-NDX OT-NDX)
100518         ADD EX-BA-PAYMENT-AMOUNT
100518                        TO  CPT-PMT-VOID-AMTS (BENE-NDX OT-NDX)

122402     WHEN EX-BA-PAYEE-TYPE-CD = 'A' AND
122402          EX-BA-CLAIM-TYPE = AH-OVERRIDE-L1 
01087          ADD +1         TO  CPT-PMT-VOID-CNTS (ACCT-NDX AH-NDX)   EL317
01088          ADD EX-BA-PAYMENT-AMOUNT                                 EL317
01089                         TO  CPT-PMT-VOID-AMTS (ACCT-NDX AH-NDX)   EL317

122402     WHEN EX-BA-PAYEE-TYPE-CD = 'A' AND
122402          EX-BA-CLAIM-TYPE = 'I'            
122402         ADD +1         TO  CPT-PMT-VOID-CNTS (ACCT-NDX IU-NDX)   EL317
122402         ADD EX-BA-PAYMENT-AMOUNT                                 EL317
122402                        TO  CPT-PMT-VOID-AMTS (ACCT-NDX IU-NDX)   EL317
121203
121203     WHEN EX-BA-PAYEE-TYPE-CD = 'A' AND
121203          EX-BA-CLAIM-TYPE = 'G'            
121203         ADD +1         TO  CPT-PMT-VOID-CNTS (ACCT-NDX GP-NDX)   EL317
121203         ADD EX-BA-PAYMENT-AMOUNT                                 EL317
121203                        TO  CPT-PMT-VOID-AMTS (ACCT-NDX GP-NDX)   EL317
052614
052614     WHEN EX-BA-PAYEE-TYPE-CD = 'A' AND
052614          EX-BA-CLAIM-TYPE = 'F'
052614         ADD +1         TO  CPT-PMT-VOID-CNTS (ACCT-NDX FL-NDX)
052614         ADD EX-BA-PAYMENT-AMOUNT
052614                        TO  CPT-PMT-VOID-AMTS (ACCT-NDX FL-NDX)
100518
022122     WHEN EX-BA-PAYEE-TYPE-CD = 'A' AND
022122          EX-BA-CLAIM-TYPE = 'B'
022122         ADD +1         TO  CPT-PMT-VOID-CNTS (ACCT-NDX BR-NDX)
022122         ADD EX-BA-PAYMENT-AMOUNT
022122                        TO  CPT-PMT-VOID-AMTS (ACCT-NDX BR-NDX)
022122
022122     WHEN EX-BA-PAYEE-TYPE-CD = 'A' AND
022122          EX-BA-CLAIM-TYPE = 'H'
022122         ADD +1         TO  CPT-PMT-VOID-CNTS (ACCT-NDX HS-NDX)
022122         ADD EX-BA-PAYMENT-AMOUNT
022122                        TO  CPT-PMT-VOID-AMTS (ACCT-NDX HS-NDX)
100518
100518     WHEN EX-BA-PAYEE-TYPE-CD = 'A' AND
100518          EX-BA-CLAIM-TYPE = 'O'
100518         ADD +1         TO  CPT-PMT-VOID-CNTS (ACCT-NDX OT-NDX)
100518         ADD EX-BA-PAYMENT-AMOUNT
100518                        TO  CPT-PMT-VOID-AMTS (ACCT-NDX OT-NDX)

122402     WHEN EX-BA-PAYEE-TYPE-CD = 'P' AND
122402          EX-BA-CLAIM-TYPE = AH-OVERRIDE-L1
01092          ADD +1         TO  CPT-PMT-VOID-CNTS (PHYS-NDX AH-NDX)   EL317
01093          ADD EX-BA-PAYMENT-AMOUNT                                 EL317
01094                         TO  CPT-PMT-VOID-AMTS (PHYS-NDX AH-NDX)   EL317

122402     WHEN EX-BA-PAYEE-TYPE-CD = 'P' AND
122402          EX-BA-CLAIM-TYPE = 'I'            
122402         ADD +1         TO  CPT-PMT-VOID-CNTS (PHYS-NDX IU-NDX)   EL317
122402         ADD EX-BA-PAYMENT-AMOUNT                                 EL317
122402                        TO  CPT-PMT-VOID-AMTS (PHYS-NDX IU-NDX)   EL317
121203
121203     WHEN EX-BA-PAYEE-TYPE-CD = 'P' AND
121203          EX-BA-CLAIM-TYPE = 'G'            
121203         ADD +1         TO  CPT-PMT-VOID-CNTS (PHYS-NDX GP-NDX)   EL317
121203         ADD EX-BA-PAYMENT-AMOUNT                                 EL317
121203                        TO  CPT-PMT-VOID-AMTS (PHYS-NDX GP-NDX)   EL317
052614
052614     WHEN EX-BA-PAYEE-TYPE-CD = 'P' AND
052614          EX-BA-CLAIM-TYPE = 'F'
052614         ADD +1         TO  CPT-PMT-VOID-CNTS (PHYS-NDX FL-NDX)
052614         ADD EX-BA-PAYMENT-AMOUNT
052614                        TO  CPT-PMT-VOID-AMTS (PHYS-NDX FL-NDX)
100518
022122     WHEN EX-BA-PAYEE-TYPE-CD = 'P' AND
022122          EX-BA-CLAIM-TYPE = 'B'
022122         ADD +1         TO  CPT-PMT-VOID-CNTS (PHYS-NDX BR-NDX)
022122         ADD EX-BA-PAYMENT-AMOUNT
022122                        TO  CPT-PMT-VOID-AMTS (PHYS-NDX BR-NDX)
022122
022122     WHEN EX-BA-PAYEE-TYPE-CD = 'P' AND
022122          EX-BA-CLAIM-TYPE = 'H'
022122         ADD +1         TO  CPT-PMT-VOID-CNTS (PHYS-NDX HS-NDX)
022122         ADD EX-BA-PAYMENT-AMOUNT
022122                        TO  CPT-PMT-VOID-AMTS (PHYS-NDX HS-NDX)
100518
100518     WHEN EX-BA-PAYEE-TYPE-CD = 'P' AND
100518          EX-BA-CLAIM-TYPE = 'O'
100518         ADD +1         TO  CPT-PMT-VOID-CNTS (PHYS-NDX OT-NDX)
100518         ADD EX-BA-PAYMENT-AMOUNT
100518                        TO  CPT-PMT-VOID-AMTS (PHYS-NDX OT-NDX)

122402     WHEN EX-BA-PAYEE-TYPE-CD = 'O' AND
122402          EX-BA-CLAIM-TYPE = AH-OVERRIDE-L1
01097          ADD +1         TO  CPT-PMT-VOID-CNTS (OTH1-NDX AH-NDX)   EL317
01098          ADD EX-BA-PAYMENT-AMOUNT                                 EL317
01099                         TO  CPT-PMT-VOID-AMTS (OTH1-NDX AH-NDX)   EL317

122402     WHEN EX-BA-PAYEE-TYPE-CD = 'O' AND
122402          EX-BA-CLAIM-TYPE = 'I'             
122402         ADD +1         TO  CPT-PMT-VOID-CNTS (OTH1-NDX IU-NDX)   EL317
122402         ADD EX-BA-PAYMENT-AMOUNT                                 EL317
122402                        TO  CPT-PMT-VOID-AMTS (OTH1-NDX IU-NDX)   EL317
121203
121203     WHEN EX-BA-PAYEE-TYPE-CD = 'O' AND
121203          EX-BA-CLAIM-TYPE = 'G'             
121203         ADD +1         TO  CPT-PMT-VOID-CNTS (OTH1-NDX GP-NDX)   EL317
121203         ADD EX-BA-PAYMENT-AMOUNT                                 EL317
121203                        TO  CPT-PMT-VOID-AMTS (OTH1-NDX GP-NDX)   EL317
052614
052614     WHEN EX-BA-PAYEE-TYPE-CD = 'O' AND
052614          EX-BA-CLAIM-TYPE = 'F'
052614         ADD +1         TO  CPT-PMT-VOID-CNTS (OTH1-NDX FL-NDX)
052614         ADD EX-BA-PAYMENT-AMOUNT
052614                        TO  CPT-PMT-VOID-AMTS (OTH1-NDX FL-NDX)
100518
022122     WHEN EX-BA-PAYEE-TYPE-CD = 'O' AND
022122          EX-BA-CLAIM-TYPE = 'B'
022122         ADD +1         TO  CPT-PMT-VOID-CNTS (OTH1-NDX BR-NDX)
022122         ADD EX-BA-PAYMENT-AMOUNT
022122                        TO  CPT-PMT-VOID-AMTS (OTH1-NDX BR-NDX)
022122
022122     WHEN EX-BA-PAYEE-TYPE-CD = 'O' AND
022122          EX-BA-CLAIM-TYPE = 'H'
022122         ADD +1         TO  CPT-PMT-VOID-CNTS (OTH1-NDX HS-NDX)
022122         ADD EX-BA-PAYMENT-AMOUNT
022122                        TO  CPT-PMT-VOID-AMTS (OTH1-NDX HS-NDX)
100518
100518     WHEN EX-BA-PAYEE-TYPE-CD = 'O' AND
100518          EX-BA-CLAIM-TYPE = 'O'
100518         ADD +1         TO  CPT-PMT-VOID-CNTS (OTH1-NDX OT-NDX)
100518         ADD EX-BA-PAYMENT-AMOUNT
100518                        TO  CPT-PMT-VOID-AMTS (OTH1-NDX OT-NDX)

122402     WHEN EX-BA-CLAIM-TYPE = AH-OVERRIDE-L1
01101          ADD +1         TO  CPT-PMT-VOID-CNTS (OTH2-NDX AH-NDX)   EL317
01102          ADD EX-BA-PAYMENT-AMOUNT                                 EL317
01103                         TO  CPT-PMT-VOID-AMTS (OTH2-NDX AH-NDX)
01104                                                                   EL317
122402     WHEN EX-BA-CLAIM-TYPE = 'I'            
122402         ADD +1         TO  CPT-PMT-VOID-CNTS (OTH2-NDX IU-NDX)   EL317
122402         ADD EX-BA-PAYMENT-AMOUNT                                 EL317
122402                        TO  CPT-PMT-VOID-AMTS (OTH2-NDX IU-NDX)
121203
121203     WHEN EX-BA-CLAIM-TYPE = 'G'            
121203         ADD +1         TO  CPT-PMT-VOID-CNTS (OTH2-NDX GP-NDX)   EL317
121203         ADD EX-BA-PAYMENT-AMOUNT                                 EL317
121203                        TO  CPT-PMT-VOID-AMTS (OTH2-NDX GP-NDX)
052614
052614     WHEN EX-BA-CLAIM-TYPE = 'F'
052614         ADD +1         TO  CPT-PMT-VOID-CNTS (OTH2-NDX FL-NDX)
052614         ADD EX-BA-PAYMENT-AMOUNT
052614                        TO  CPT-PMT-VOID-AMTS (OTH2-NDX FL-NDX)
100518
022122     WHEN EX-BA-CLAIM-TYPE = 'B'
022122         ADD +1         TO  CPT-PMT-VOID-CNTS (OTH2-NDX BR-NDX)
022122         ADD EX-BA-PAYMENT-AMOUNT
022122                        TO  CPT-PMT-VOID-AMTS (OTH2-NDX BR-NDX)
022122
022122     WHEN EX-BA-CLAIM-TYPE = 'H'
022122         ADD +1         TO  CPT-PMT-VOID-CNTS (OTH2-NDX HS-NDX)
022122         ADD EX-BA-PAYMENT-AMOUNT
022122                        TO  CPT-PMT-VOID-AMTS (OTH2-NDX HS-NDX)
100518
100518     WHEN EX-BA-CLAIM-TYPE = 'O'
100518         ADD +1         TO  CPT-PMT-VOID-CNTS (OTH2-NDX OT-NDX)
100518         ADD EX-BA-PAYMENT-AMOUNT
100518                        TO  CPT-PMT-VOID-AMTS (OTH2-NDX OT-NDX)
122402     END-EVALUATE.
01104                                                                   EL317
01105      GO TO 3899-EXIT.                                             EL317
01106                                                                   EL317
01107      EJECT                                                        EL317
01108  3850-ACCUM-LIFE-PAYEE.                                           EL317
01109                                                                   EL317
01110      IF EX-BA-VOID-DT NOT = LOW-VALUES                            EL317
01111          GO TO 3875-ACCUM-LF-VOID.                                EL317
01112                                                                   EL317
01113      IF EX-BA-PAYEE-TYPE-CD = 'I'
100518       IF EX-BA-CLAIM-TYPE = 'O'
100518         ADD +1              TO  CPT-PMT-CNTS (INSD-NDX OT-NDX)
100518         ADD EX-BA-PAYMENT-AMOUNT
100518                             TO  CPT-PMT-AMTS (INSD-NDX OT-NDX)
100518       ELSE
01114          ADD +1              TO  CPT-PMT-CNTS (INSD-NDX LF-NDX)   EL317
01115          ADD EX-BA-PAYMENT-AMOUNT                                 EL317
01116                              TO  CPT-PMT-AMTS (INSD-NDX LF-NDX)   EL317
01117      ELSE                                                         EL317
01118      IF EX-BA-PAYEE-TYPE-CD = 'B'                                 EL317
100518       IF EX-BA-CLAIM-TYPE = 'O'
100518         ADD +1              TO  CPT-PMT-CNTS (BENE-NDX OT-NDX)
100518         ADD EX-BA-PAYMENT-AMOUNT
100518                             TO  CPT-PMT-AMTS (BENE-NDX OT-NDX)
100518       ELSE
01119          ADD +1              TO  CPT-PMT-CNTS (BENE-NDX LF-NDX)   EL317
01120          ADD EX-BA-PAYMENT-AMOUNT                                 EL317
01121                              TO  CPT-PMT-AMTS (BENE-NDX LF-NDX)   EL317
01122      ELSE                                                         EL317
01123      IF EX-BA-PAYEE-TYPE-CD = 'A'                                 EL317
100518       IF EX-BA-CLAIM-TYPE = 'O'
100518         ADD +1              TO  CPT-PMT-CNTS (ACCT-NDX OT-NDX)
100518         ADD EX-BA-PAYMENT-AMOUNT
100518                             TO  CPT-PMT-AMTS (ACCT-NDX OT-NDX)
100518       ELSE
01124          ADD +1              TO  CPT-PMT-CNTS (ACCT-NDX LF-NDX)   EL317
01125          ADD EX-BA-PAYMENT-AMOUNT                                 EL317
01126                              TO  CPT-PMT-AMTS (ACCT-NDX LF-NDX)   EL317
01127      ELSE                                                         EL317
01128      IF EX-BA-PAYEE-TYPE-CD = 'P'                                 EL317
100518       IF EX-BA-CLAIM-TYPE = 'O'
100518         ADD +1              TO  CPT-PMT-CNTS (PHYS-NDX OT-NDX)
100518         ADD EX-BA-PAYMENT-AMOUNT
100518                             TO  CPT-PMT-AMTS (PHYS-NDX OT-NDX)
100518       ELSE
01129          ADD +1              TO  CPT-PMT-CNTS (PHYS-NDX LF-NDX)   EL317
01130          ADD EX-BA-PAYMENT-AMOUNT                                 EL317
01131                              TO  CPT-PMT-AMTS (PHYS-NDX LF-NDX)   EL317
01132      ELSE                                                         EL317
01133      IF EX-BA-PAYEE-TYPE-CD = 'O'                                 EL317
100518       IF EX-BA-CLAIM-TYPE = 'O'
100518         ADD +1              TO  CPT-PMT-CNTS (OTH1-NDX OT-NDX)
100518         ADD EX-BA-PAYMENT-AMOUNT
100518                             TO  CPT-PMT-AMTS (OTH1-NDX OT-NDX)
100518       ELSE
01134          ADD +1              TO  CPT-PMT-CNTS (OTH1-NDX LF-NDX)   EL317
01135          ADD EX-BA-PAYMENT-AMOUNT                                 EL317
01136                              TO  CPT-PMT-AMTS (OTH1-NDX LF-NDX)   EL317
01137      ELSE                                                         EL317
100518       IF EX-BA-CLAIM-TYPE = 'O'
100518         ADD +1              TO  CPT-PMT-CNTS (OTH2-NDX OT-NDX)
100518         ADD EX-BA-PAYMENT-AMOUNT
100518                             TO  CPT-PMT-AMTS (OTH2-NDX OT-NDX)
100518       ELSE

01138          ADD +1              TO  CPT-PMT-CNTS (OTH2-NDX LF-NDX)   EL317
01139          ADD EX-BA-PAYMENT-AMOUNT                                 EL317
01140                              TO  CPT-PMT-AMTS (OTH2-NDX LF-NDX).  EL317
01141                                                                   EL317
01142      GO TO 3899-EXIT.                                             EL317
01143                                                                   EL317
01144  3875-ACCUM-LF-VOID.                                              EL317
01145                                                                   EL317
01146      IF EX-BA-PAYEE-TYPE-CD = 'I'                                 EL317
100518       IF EX-BA-CLAIM-TYPE = 'O'
100518         ADD +1         TO  CPT-PMT-VOID-CNTS (INSD-NDX OT-NDX)
100518         ADD EX-BA-PAYMENT-AMOUNT
100518                        TO  CPT-PMT-VOID-AMTS (INSD-NDX OT-NDX)
100518       ELSE
01147          ADD +1         TO  CPT-PMT-VOID-CNTS (INSD-NDX LF-NDX)   EL317
01148          ADD EX-BA-PAYMENT-AMOUNT                                 EL317
01149                         TO  CPT-PMT-VOID-AMTS (INSD-NDX LF-NDX)   EL317
01150      ELSE                                                         EL317
01151      IF EX-BA-PAYEE-TYPE-CD = 'B'                                 EL317
100518       IF EX-BA-CLAIM-TYPE = 'O'
100518         ADD +1         TO  CPT-PMT-VOID-CNTS (BENE-NDX OT-NDX)
100518         ADD EX-BA-PAYMENT-AMOUNT
100518                        TO  CPT-PMT-VOID-AMTS (BENE-NDX OT-NDX)
100518       ELSE
01152          ADD +1         TO  CPT-PMT-VOID-CNTS (BENE-NDX LF-NDX)   EL317
01153          ADD EX-BA-PAYMENT-AMOUNT                                 EL317
01154                         TO  CPT-PMT-VOID-AMTS (BENE-NDX LF-NDX)   EL317
01155      ELSE                                                         EL317
01156      IF EX-BA-PAYEE-TYPE-CD = 'A'                                 EL317
100518       IF EX-BA-CLAIM-TYPE = 'O'
100518         ADD +1         TO  CPT-PMT-VOID-CNTS (ACCT-NDX OT-NDX)
100518         ADD EX-BA-PAYMENT-AMOUNT
100518                        TO  CPT-PMT-VOID-AMTS (ACCT-NDX OT-NDX)
100518       ELSE
01157          ADD +1         TO  CPT-PMT-VOID-CNTS (ACCT-NDX LF-NDX)   EL317
01158          ADD EX-BA-PAYMENT-AMOUNT                                 EL317
01159                         TO  CPT-PMT-VOID-AMTS (ACCT-NDX LF-NDX)   EL317
01160      ELSE                                                         EL317
01161      IF EX-BA-PAYEE-TYPE-CD = 'P'                                 EL317
100518       IF EX-BA-CLAIM-TYPE = 'O'
100518         ADD +1         TO  CPT-PMT-VOID-CNTS (PHYS-NDX OT-NDX)
100518         ADD EX-BA-PAYMENT-AMOUNT
100518                        TO  CPT-PMT-VOID-AMTS (PHYS-NDX OT-NDX)
100518       ELSE
01162          ADD +1         TO  CPT-PMT-VOID-CNTS (PHYS-NDX LF-NDX)   EL317
01163          ADD EX-BA-PAYMENT-AMOUNT                                 EL317
01164                         TO  CPT-PMT-VOID-AMTS (PHYS-NDX LF-NDX)   EL317
01165      ELSE                                                         EL317
01166      IF EX-BA-PAYEE-TYPE-CD = 'O'                                 EL317
100518       IF EX-BA-CLAIM-TYPE = 'O'
100518         ADD +1         TO  CPT-PMT-VOID-CNTS (OTH1-NDX OT-NDX)
100518         ADD EX-BA-PAYMENT-AMOUNT
100518                        TO  CPT-PMT-VOID-AMTS (OTH1-NDX OT-NDX)
100518       ELSE
01167          ADD +1         TO  CPT-PMT-VOID-CNTS (OTH1-NDX LF-NDX)   EL317
01168          ADD EX-BA-PAYMENT-AMOUNT                                 EL317
01169                         TO  CPT-PMT-VOID-AMTS (OTH1-NDX LF-NDX)   EL317
01170      ELSE                                                         EL317
100518       IF EX-BA-CLAIM-TYPE = 'O'
100518         ADD +1         TO  CPT-PMT-VOID-CNTS (OTH2-NDX OT-NDX)
100518         ADD EX-BA-PAYMENT-AMOUNT
100518                        TO  CPT-PMT-VOID-AMTS (OTH2-NDX OT-NDX)
100518       ELSE
01171          ADD +1         TO  CPT-PMT-VOID-CNTS (OTH2-NDX LF-NDX)   EL317
01172          ADD EX-BA-PAYMENT-AMOUNT                                 EL317
01173                         TO  CPT-PMT-VOID-AMTS (OTH2-NDX LF-NDX).  EL317
01174                                                                   EL317
01175  3899-EXIT.                                                       EL317
01176      EXIT.                                                        EL317
01177                                                                   EL317
01178  3900-ACCUM-REINSURANCE.                                          EL317
01179                                                                   EL317
01180      IF EX-BA-VOID-DT NOT = LOW-VALUES                            EL317
01181          ADD +1                      TO  CR-PMT-VOID-CNTS         EL317
01182          ADD EX-BA-PAYMENT-AMOUNT    TO  CR-PMT-VOID-AMTS         EL317
01183      ELSE                                                         EL317
01184          ADD +1                      TO  CR-PMT-CNTS              EL317
01185          ADD EX-BA-PAYMENT-AMOUNT    TO  CR-PMT-AMTS.             EL317
01186                                                                   EL317
01187  3999-EXIT.                                                       EL317
01188      EXIT.                                                        EL317
01189                                                                   EL317
01190      EJECT                                                        EL317
01191  4000-PRINT-TOTALS SECTION.                                       EL317
01192                                                                   EL317
01193      MOVE +99                    TO  WS-LINE-COUNT.               EL317
01194      MOVE +1                     TO  WS-HEADING-SW.               EL317
01195                                                                   EL317
01196      IF WS-REPORT-SW = +1                                         EL317
01197          MOVE SPACES             TO  WS-H2-CARRIER                EL317
01198          MOVE 'ALL CARRIERS '    TO  WS-H3-CARRIER-NAME           EL317
01199          MOVE '0 * * * * * * * GRAND TOTALS * * * * * * *'        EL317
01200                                  TO  WS-HEADING6.                 EL317
01201                                                                   EL317
01202      IF WS-REPORT-SW = ZERO                                       EL317
01203          GO TO 4100-PRINT-TOTALS.                                 EL317
01204                                                                   EL317
122402*    IF DTE-CLIENT NOT = 'POS'                                    EL317
01206          GO TO 4100-PRINT-TOTALS.                                 EL317
01207                                                                   EL317
01208 *    NOTE ******************************************************* EL317
01209 *         *          LIFE BENEFITS G/L ENTRY                    * EL317
01210 *         *******************************************************.EL317
01211                                                                   EL317
122402*    MOVE '01CCCLAIMS '          TO  ABC-INTERFACE-RECORD.        EL317
122402*    MOVE RUN-YR                 TO  ABC-DATE-YEAR.               EL317
122402*    MOVE RUN-MO                 TO  ABC-DATE-MONTH.              EL317
122402*    MOVE RUN-DA                 TO  ABC-DATE-DAY.                EL317
122402*    MOVE '5071000'              TO  ABC-ACCOUNT.                 EL317
122402*                                                                 EL317
122402*    SUBTRACT CT-CURR-LF-PMTS-VOIDED-AMT                          EL317
122402*        FROM CT-CURR-LF-PMTS-AMT GIVING WS-AMOUNT.               EL317
122402*                                                                 EL317
122402*    MOVE WS-AMOUNT              TO  ABC-AMOUNT.                  EL317
122402*                                                                 EL317
122402*    IF WS-AMOUNT < ZERO                                          EL317
122402*        MOVE 'C'                TO  ABC-DEBIT-CREDIT             EL317
122402*      ELSE                                                       EL317
122402*        MOVE 'D'                TO  ABC-DEBIT-CREDIT.            EL317
122402*                                                                 EL317
122402*    MOVE 'CREDIT CLAIMS'        TO  ABC-DESC.                    EL317
122402*                                                                 EL317
122402*    WRITE ABC-INTERFACE-RECORD.                                  EL317
01231                                                                   EL317
01232 *    NOTE ******************************************************* EL317
01233 *         *           A&H BENEFITS G/L ENTRY                    * EL317
01234 *         *******************************************************.EL317
01235                                                                   EL317
122402*    MOVE '01CCCLAIMS '          TO  ABC-INTERFACE-RECORD.        EL317
122402*    MOVE RUN-YR                 TO  ABC-DATE-YEAR.               EL317
122402*    MOVE RUN-MO                 TO  ABC-DATE-MONTH.              EL317
122402*    MOVE RUN-DA                 TO  ABC-DATE-DAY.                EL317
01240                                                                   EL317
122402*    MOVE '5072000'              TO  ABC-ACCOUNT.                 EL317
122402*                                                                 EL317
122402*    SUBTRACT CT-CURR-AH-PMTS-VOIDED-AMT                          EL317
122402*        FROM CT-CURR-AH-PMTS-AMT GIVING WS-AMOUNT.               EL317
01245                                                                   EL317
122402*    MOVE WS-AMOUNT              TO  ABC-AMOUNT.                  EL317
01247                                                                   EL317
122402*    IF WS-AMOUNT < ZERO                                          EL317
122402*        MOVE 'C'                TO  ABC-DEBIT-CREDIT             EL317
122402*    ELSE                                                         EL317
122402*        MOVE 'D'                TO  ABC-DEBIT-CREDIT.            EL317
01252                                                                   EL317
122402*    MOVE 'CREDIT CLAIMS'        TO  ABC-DESC.                    EL317
122402*                                                                 EL317
122402*    WRITE ABC-INTERFACE-RECORD.                                  EL317
122402*                                                                 EL317
01257 *    NOTE ******************************************************* EL317
01258 *         *                   CASH G/L ENTRY                    * EL317
01259 *         *******************************************************.EL317
01260                                                                   EL317
122402*    MOVE '01CCCLAIMS '          TO  ABC-INTERFACE-RECORD.        EL317
122402*    MOVE RUN-YR                 TO  ABC-DATE-YEAR.               EL317
122402*    MOVE RUN-MO                 TO  ABC-DATE-MONTH.              EL317
122402*    MOVE RUN-DA                 TO  ABC-DATE-DAY.                EL317
01265                                                                   EL317
122402*    MOVE '1266000'              TO  ABC-ACCOUNT.                 EL317
122402*                                                                 EL317
122402*    COMPUTE WS-AMOUNT = (CT-CURR-LF-PMTS-AMT -                   EL317
122402*                         CT-CURR-LF-PMTS-VOIDED-AMT) +           EL317
122402*                        (CT-CURR-AH-PMTS-AMT -                   EL317
122402*                         CT-CURR-AH-PMTS-VOIDED-AMT).            EL317
01272                                                                   EL317
122402*    MOVE WS-AMOUNT              TO  ABC-AMOUNT.                  EL317
01274                                                                   EL317
122402*    IF WS-AMOUNT < ZERO                                          EL317
122402*        MOVE 'D'                TO  ABC-DEBIT-CREDIT             EL317
122402*      ELSE                                                       EL317
122402*        MOVE 'C'                TO  ABC-DEBIT-CREDIT.            EL317
01279                                                                   EL317
122402*    MOVE 'CREDIT CLAIMS'        TO  ABC-DESC.                    EL317
122402*                                                                 EL317
122402*    WRITE ABC-INTERFACE-RECORD.                                  EL317
01283                                                                   EL317
01284      EJECT                                                        EL317
01285  4100-PRINT-TOTALS.                                               EL317
01286      ADD CT-CURR-LF-PMTS-AMT                                      EL317
01287          CT-CURR-AH-PMTS-AMT                                      EL317
122402         CT-CURR-IU-PMTS-AMT                                      EL317
121203         CT-CURR-GP-PMTS-AMT                                      EL317
022122         CT-CURR-FL-PMTS-AMT                                      EL317
022122         CT-CURR-BR-PMTS-AMT                                      EL317
022122         CT-CURR-HS-PMTS-AMT                                      EL317
100518         CT-CURR-OT-PMTS-AMT
01288          CT-CURR-CHG-EXP-AMT                                      EL317
01289          CT-CURR-OTHER-EXP-AMT  GIVING CT-CURR-CHECKS-AMT.        EL317
01290                                                                   EL317
01291      ADD CT-CURR-LF-PMTS-CNT                                      EL317
01292          CT-CURR-AH-PMTS-CNT                                      EL317
122402         CT-CURR-IU-PMTS-CNT                                      EL317
121203         CT-CURR-GP-PMTS-CNT                                      EL317
022122         CT-CURR-FL-PMTS-CNT                                      EL317
022122         CT-CURR-BR-PMTS-CNT                                      EL317
022122         CT-CURR-HS-PMTS-CNT                                      EL317
100518         CT-CURR-OT-PMTS-CNT
01293          CT-CURR-CHG-EXP-CNT                                      EL317
01294          CT-CURR-OTHER-EXP-CNT  GIVING CT-CURR-CHECKS-CNT.        EL317
01295                                                                   EL317
01296      ADD CT-CURR-LF-PMTS-VOIDED-AMT                               EL317
01297          CT-CURR-AH-PMTS-VOIDED-AMT                               EL317
122402         CT-CURR-IU-PMTS-VOIDED-AMT                               EL317
121203         CT-CURR-GP-PMTS-VOIDED-AMT                               EL317
022122         CT-CURR-FL-PMTS-VOIDED-AMT                               EL317
022122         CT-CURR-BR-PMTS-VOIDED-AMT                               EL317
022122         CT-CURR-HS-PMTS-VOIDED-AMT                               EL317
100518         CT-CURR-OT-PMTS-VOIDED-AMT
01298          CT-CURR-CHG-EXP-VOIDED-AMT                               EL317
01299          CT-CURR-OTHER-EXP-VOIDED-AMT                             EL317
01300                                  GIVING CT-CURR-CHECKS-VOIDED-AMT.EL317
01301      ADD CT-CURR-LF-PMTS-VOIDED-CNT                               EL317
01302          CT-CURR-AH-PMTS-VOIDED-CNT                               EL317
122402         CT-CURR-IU-PMTS-VOIDED-CNT                               EL317
121203         CT-CURR-GP-PMTS-VOIDED-CNT                               EL317
022122         CT-CURR-FL-PMTS-VOIDED-CNT                               EL317
022122         CT-CURR-BR-PMTS-VOIDED-CNT                               EL317
022122         CT-CURR-HS-PMTS-VOIDED-CNT                               EL317
100518         CT-CURR-OT-PMTS-VOIDED-CNT
01303          CT-CURR-CHG-EXP-VOIDED-CNT                               EL317
01304          CT-CURR-OTHER-EXP-VOIDED-CNT                             EL317
01305                                  GIVING CT-CURR-CHECKS-VOIDED-CNT.EL317
01306                                                                   EL317
070714     if ws-report-sw = +1
070714        if me-do-update
070714           compute hld-317-clms-tot =
070714           (ft-curr-lf-pmts-amt - ft-curr-lf-pmts-voided-amt) +
122018           (ft-curr-ot-pmts-amt - ft-curr-ot-pmts-voided-amt) +
070714           (ft-curr-ah-pmts-amt - ft-curr-ah-pmts-voided-amt) +
060115           (ft-curr-iu-pmts-amt - ft-curr-iu-pmts-voided-amt) +
060115           (ft-curr-fl-pmts-amt - ft-curr-fl-pmts-voided-amt) +
022122           (ft-curr-br-pmts-amt - ft-curr-br-pmts-voided-amt) +
022122           (ft-curr-hs-pmts-amt - ft-curr-hs-pmts-voided-amt)
070714        end-if
070714     end-if
01307      SET TOTAL-INDEX1 TO +1.                                      EL317
01308                                                                   EL317
01309      IF WS-REPORT-SW = ZERO                                       EL317
01310          SET TOTAL-INDEX2 TO +1                                   EL317
01311      ELSE                                                         EL317
01312          SET TOTAL-INDEX2 TO +2.                                  EL317
01313                                                                   EL317
01314      SET TOTAL-INDEX3 TO TOTAL-INDEX2.                            EL317
01315      SET TOTAL-INDEX3 UP BY +2.                                   EL317
122402     SET TOTAL-INDEX4 TO TOTAL-INDEX3.
122402     SET TOTAL-INDEX4 UP BY +2.
121203     SET TOTAL-INDEX5 TO TOTAL-INDEX4.
121203     SET TOTAL-INDEX5 UP BY +2.
052614     SET TOTAL-INDEX6 TO TOTAL-INDEX5.
052614     SET TOTAL-INDEX6 UP BY +2.
100518     SET TOTAL-INDEX7 TO TOTAL-INDEX6.
100518     SET TOTAL-INDEX7 UP BY +2.
022122     SET TOTAL-INDEX8 TO TOTAL-INDEX7.
022122     SET TOTAL-INDEX8 UP BY +2.
022122     SET TOTAL-INDEX9 TO TOTAL-INDEX8.
022122     SET TOTAL-INDEX9 UP BY +2.
01316                                                                   EL317
01317      EJECT                                                        EL317
01318  4200-PRINT-TOTALS.                                               EL317
01319                                                                   EL317
022122     IF TOTAL-INDEX1  NOT = +13
122402         CONTINUE
01322      ELSE                                                         EL317
122402         GO TO 4200-PROCESS-OFFLINE
121203     END-IF.
122402
052614     IF TOTAL-INDEX1 = +3 OR +4 OR +5 *> IU GP FL
010716        IF DTE-CLIENT = 'DCC' or 'VPP'
122402            CONTINUE
122402        ELSE
122402            SET TOTAL-INDEX1 UP BY +1
122402            GO TO 4200-PRINT-TOTALS
122402        END-IF
122402     END-IF.
01324                                                                   EL317
022122     IF TOTAL-INDEX1 = +12
01326          MOVE '0 NET PAID ONLINE LIFE' TO  WS-TOTAL-LINE1         EL317
100518         ADD TOTALS-CURR-AMT (TOTAL-INDEX1 TOTAL-INDEX2)          EL317
100518           TOTALS-CURR-AMT (TOTAL-INDEX1 TOTAL-INDEX7)            EL317
100518                                    GIVING WS-T1-CURR-AMOUNT      EL317
100518         ADD TOTALS-CURR-CNT (TOTAL-INDEX1 TOTAL-INDEX2)          EL317
100518            TOTALS-CURR-CNT (TOTAL-INDEX1 TOTAL-INDEX7)           EL317
100518                                    GIVING WS-T1-CURR-COUNT       EL317
01331          MOVE WS-TOTAL-LINE1           TO  PRT                    EL317
01332          PERFORM WRITE-A-LINE                                     EL317

01333          MOVE '  NET PAID ONLINE A&H'  TO  WS-TOTAL-LINE1         EL317
01334          MOVE TOTALS-CURR-AMT (TOTAL-INDEX1 TOTAL-INDEX3)         EL317
01335                                        TO  WS-T1-CURR-AMOUNT      EL317
01336          MOVE TOTALS-CURR-CNT (TOTAL-INDEX1 TOTAL-INDEX3)         EL317
01337                                        TO  WS-T1-CURR-COUNT       EL317
01338          MOVE WS-TOTAL-LINE1           TO  PRT                    EL317
01339          PERFORM WRITE-A-LINE                                     EL317

010716         IF DTE-CLIENT = 'DCC' or 'VPP'
122402             MOVE '  NET PAID ONLINE IU'   TO WS-TOTAL-LINE1   
122402             MOVE TOTALS-CURR-AMT (TOTAL-INDEX1 TOTAL-INDEX4)  
122402                                           TO WS-T1-CURR-AMOUNT 
122402             MOVE TOTALS-CURR-CNT (TOTAL-INDEX1 TOTAL-INDEX4)   
122402                                           TO WS-T1-CURR-COUNT
122402             MOVE WS-TOTAL-LINE1           TO PRT            
122402             PERFORM WRITE-A-LINE                            
121203
121203             MOVE '  NET PAID ONLINE GP'   TO WS-TOTAL-LINE1   
121203             MOVE TOTALS-CURR-AMT (TOTAL-INDEX1 TOTAL-INDEX5)  
121203                                           TO WS-T1-CURR-AMOUNT 
121203             MOVE TOTALS-CURR-CNT (TOTAL-INDEX1 TOTAL-INDEX5)   
121203                                           TO WS-T1-CURR-COUNT
121203             MOVE WS-TOTAL-LINE1           TO PRT            
121203             PERFORM WRITE-A-LINE                            
052614
052614             MOVE '  NET PAID ONLINE FL'   TO WS-TOTAL-LINE1   
052614             MOVE TOTALS-CURR-AMT (TOTAL-INDEX1 TOTAL-INDEX6)  
052614                                           TO WS-T1-CURR-AMOUNT 
052614             MOVE TOTALS-CURR-CNT (TOTAL-INDEX1 TOTAL-INDEX6)   
052614                                           TO WS-T1-CURR-COUNT
052614             MOVE WS-TOTAL-LINE1           TO PRT            
052614             PERFORM WRITE-A-LINE                            
122402         END-IF

01340          MOVE '  TOTAL'                TO  WS-TOTAL-LINE1         EL317
01341          MOVE TOTALS-DESC (TOTAL-INDEX1)                          EL317
01342                                        TO  WS-T1-TOTAL-DESCRIPTIONEL317
01343          ADD TOTALS-CURR-AMT (TOTAL-INDEX1 TOTAL-INDEX2),
122402             TOTALS-CURR-AMT (TOTAL-INDEX1 TOTAL-INDEX3),
121203             TOTALS-CURR-AMT (TOTAL-INDEX1 TOTAL-INDEX4),
121203             TOTALS-CURR-AMT (TOTAL-INDEX1 TOTAL-INDEX5)
052614             TOTALS-CURR-AMT (TOTAL-INDEX1 TOTAL-INDEX6)
100518             TOTALS-CURR-AMT (TOTAL-INDEX1 TOTAL-INDEX7)
022122             TOTALS-CURR-AMT (TOTAL-INDEX1 TOTAL-INDEX8)
022122             TOTALS-CURR-AMT (TOTAL-INDEX1 TOTAL-INDEX9)
                   GIVING WS-T1-CURR-AMOUNT
01346          MOVE WS-TOTAL-LINE1           TO  PRT                    EL317
01347          PERFORM WRITE-A-LINE                                     EL317
01348          GO TO 4200-CONTINUE                                      EL317
01349      ELSE                                                         EL317
01350          MOVE '0 TOTAL'                TO  WS-TOTAL-LINE1         EL317
01351          MOVE TOTALS-DESC (TOTAL-INDEX1)                          EL317
01352                                      TO  WS-T1-TOTAL-DESCRIPTION  EL317
01353          MOVE TOTALS-CURR-AMT (TOTAL-INDEX1 TOTAL-INDEX2)         EL317
01354                                        TO  WS-T1-CURR-AMOUNT      EL317
01355          MOVE TOTALS-CURR-CNT (TOTAL-INDEX1 TOTAL-INDEX2)         EL317
01356                                        TO  WS-T1-CURR-COUNT       EL317
01357          MOVE WS-TOTAL-LINE1           TO  PRT                    EL317
01358          PERFORM WRITE-A-LINE.                                    EL317
01359                                                                   EL317
122402 4200-PROCESS-OFFLINE. 
01361                                                                   EL317
100518     IF TOTAL-INDEX1 = +11                                        EL317
01363          MOVE '0 NET PAID OFFLINE LIFE'  TO  WS-TOTAL-LINE1       EL317
01364          MOVE TOTALS-CURR-AMT (TOTAL-INDEX1 TOTAL-INDEX2)         EL317
01365                                          TO  WS-T1-CURR-AMOUNT    EL317
01366          MOVE TOTALS-CURR-CNT (TOTAL-INDEX1 TOTAL-INDEX2)         EL317
01367                                          TO  WS-T1-CURR-COUNT     EL317
01368          MOVE WS-TOTAL-LINE1             TO  PRT                  EL317
01369          PERFORM WRITE-A-LINE                                     EL317

01370          MOVE '  NET PAID OFFLINE A&H'   TO  WS-TOTAL-LINE1       EL317
01371          MOVE TOTALS-CURR-AMT (TOTAL-INDEX1 TOTAL-INDEX3)         EL317
01372                                          TO  WS-T1-CURR-AMOUNT    EL317
01373          MOVE TOTALS-CURR-CNT (TOTAL-INDEX1 TOTAL-INDEX3)         EL317
01374                                          TO  WS-T1-CURR-COUNT     EL317
01375          MOVE WS-TOTAL-LINE1             TO  PRT                  EL317
01376          PERFORM WRITE-A-LINE                                     EL317

010716         IF DTE-CLIENT = 'DCC' or 'VPP'
122402             MOVE '  NET PAID OFFLINE IU'    TO WS-TOTAL-LINE1      
122402             MOVE TOTALS-CURR-AMT (TOTAL-INDEX1 TOTAL-INDEX4)      
122402                                             TO WS-T1-CURR-AMOUNT 
122402             MOVE TOTALS-CURR-CNT (TOTAL-INDEX1 TOTAL-INDEX4)    
122402                                             TO WS-T1-CURR-COUNT
122402             MOVE WS-TOTAL-LINE1             TO PRT            
122402             PERFORM WRITE-A-LINE                             
121203
121203             MOVE '  NET PAID OFFLINE GP'    TO WS-TOTAL-LINE1      
121203             MOVE TOTALS-CURR-AMT (TOTAL-INDEX1 TOTAL-INDEX5)      
121203                                             TO WS-T1-CURR-AMOUNT 
121203             MOVE TOTALS-CURR-CNT (TOTAL-INDEX1 TOTAL-INDEX5)    
121203                                             TO WS-T1-CURR-COUNT
121203             MOVE WS-TOTAL-LINE1             TO PRT            
121203             PERFORM WRITE-A-LINE                             
052614
052614             MOVE '  NET PAID OFFLINE FL'    TO WS-TOTAL-LINE1      
052614             MOVE TOTALS-CURR-AMT (TOTAL-INDEX1 TOTAL-INDEX6)      
052614                                             TO WS-T1-CURR-AMOUNT 
052614             MOVE TOTALS-CURR-CNT (TOTAL-INDEX1 TOTAL-INDEX6)    
052614                                             TO WS-T1-CURR-COUNT
052614             MOVE WS-TOTAL-LINE1             TO PRT            
052614             PERFORM WRITE-A-LINE                             
122402         END-IF

01377          MOVE '  TOTAL'                  TO  WS-TOTAL-LINE1       EL317
01378          MOVE TOTALS-DESC (TOTAL-INDEX1)                          EL317
01379                                     TO  WS-T1-TOTAL-DESCRIPTION   EL317
122402         ADD TOTALS-CURR-AMT (TOTAL-INDEX1 TOTAL-INDEX2),
122402             TOTALS-CURR-AMT (TOTAL-INDEX1 TOTAL-INDEX3),
121203             TOTALS-CURR-AMT (TOTAL-INDEX1 TOTAL-INDEX4),
121203             TOTALS-CURR-AMT (TOTAL-INDEX1 TOTAL-INDEX5)
052614             TOTALS-CURR-AMT (TOTAL-INDEX1 TOTAL-INDEX6)
100518             TOTALS-CURR-AMT (TOTAL-INDEX1 TOTAL-INDEX7)
01382                                 GIVING WS-T1-CURR-AMOUNT          EL317
01383          MOVE WS-TOTAL-LINE1             TO  PRT                  EL317
01384          PERFORM WRITE-A-LINE                                     EL317
01385      ELSE                                                         EL317
01386          MOVE '  VOIDED'                 TO WS-TOTAL-LINE1        EL317
01387          MOVE TOTALS-DESC (TOTAL-INDEX1)                          EL317
01388                                    TO WS-T1-VOIDED-DESCRIPTION    EL317
01389          MOVE TOTALS-CURR-AMT (TOTAL-INDEX1 TOTAL-INDEX3)         EL317
01390                                          TO  WS-T1-CURR-AMOUNT    EL317
01391          MOVE TOTALS-CURR-CNT (TOTAL-INDEX1 TOTAL-INDEX3)         EL317
01392                                          TO  WS-T1-CURR-COUNT     EL317
01393          MOVE WS-TOTAL-LINE1             TO  PRT                  EL317
01394          PERFORM WRITE-A-LINE.                                    EL317
01395                                                                   EL317
01396  4200-CONTINUE.                                                   EL317
01397                                                                   EL317
022122     IF TOTAL-INDEX1 = +12 OR +13                                 EL317
01399          GO TO 4300-PRINT-TOTALS.                                 EL317
01400                                                                   EL317
01401      MOVE '  NET'              TO WS-TOTAL-LINE1.                 EL317
01402                                                                   EL317
01403      MOVE TOTALS-DESC (TOTAL-INDEX1)  TO  WS-T1-NET-DESCRIPTION.  EL317
01404                                                                   EL317
01405      SUBTRACT TOTALS-CURR-AMT (TOTAL-INDEX1 TOTAL-INDEX3)         EL317
01406          FROM TOTALS-CURR-AMT (TOTAL-INDEX1 TOTAL-INDEX2)         EL317
01407                                  GIVING WS-T1-CURR-AMOUNT.        EL317
01408      MOVE WS-TOTAL-LINE1         TO  PRT.                         EL317
01409      PERFORM WRITE-A-LINE.                                        EL317
01410                                                                   EL317
01411  4300-PRINT-TOTALS.                                               EL317
01412                                                                   EL317
01413      ADD TOTALS-CURR-AMT (TOTAL-INDEX1 TOTAL-INDEX-ONE)           EL317
01414       TO TOTALS-CURR-AMT (TOTAL-INDEX1 TOTAL-INDEX-TWO).          EL317
01415      ADD TOTALS-CURR-CNT (TOTAL-INDEX1 TOTAL-INDEX-ONE)           EL317
01416       TO TOTALS-CURR-CNT (TOTAL-INDEX1 TOTAL-INDEX-TWO).          EL317
01417                                                                   EL317
01418      ADD TOTALS-CURR-AMT (TOTAL-INDEX1 TOTAL-INDEX-THREE)         EL317
01419       TO TOTALS-CURR-AMT (TOTAL-INDEX1 TOTAL-INDEX-FOUR).         EL317
01420      ADD TOTALS-CURR-CNT (TOTAL-INDEX1 TOTAL-INDEX-THREE)         EL317
01421       TO TOTALS-CURR-CNT (TOTAL-INDEX1 TOTAL-INDEX-FOUR).         EL317

122402     ADD TOTALS-CURR-AMT (TOTAL-INDEX1 TOTAL-INDEX-FIVE)          EL317
122402      TO TOTALS-CURR-AMT (TOTAL-INDEX1 TOTAL-INDEX-SIX).          EL317
122402     ADD TOTALS-CURR-CNT (TOTAL-INDEX1 TOTAL-INDEX-FIVE)          EL317
122402      TO TOTALS-CURR-CNT (TOTAL-INDEX1 TOTAL-INDEX-SIX).          EL317
121203
121203     ADD TOTALS-CURR-AMT (TOTAL-INDEX1 TOTAL-INDEX-SEVEN)         EL317
121203      TO TOTALS-CURR-AMT (TOTAL-INDEX1 TOTAL-INDEX-EIGHT).        EL317
121203     ADD TOTALS-CURR-CNT (TOTAL-INDEX1 TOTAL-INDEX-SEVEN)         EL317
121203      TO TOTALS-CURR-CNT (TOTAL-INDEX1 TOTAL-INDEX-EIGHT).        EL317
052614
052614     ADD TOTALS-CURR-AMT (TOTAL-INDEX1 TOTAL-INDEX-NINE)
052614      TO TOTALS-CURR-AMT (TOTAL-INDEX1 TOTAL-INDEX-TEN).
052614     ADD TOTALS-CURR-CNT (TOTAL-INDEX1 TOTAL-INDEX-NINE)
052614      TO TOTALS-CURR-CNT (TOTAL-INDEX1 TOTAL-INDEX-TEN).
100518
100518     ADD TOTALS-CURR-AMT (TOTAL-INDEX1 TOTAL-INDEX-ELEVEN)
100518      TO TOTALS-CURR-AMT (TOTAL-INDEX1 TOTAL-INDEX-TWELVE).
100518     ADD TOTALS-CURR-CNT (TOTAL-INDEX1 TOTAL-INDEX-ELEVEN)
100518      TO TOTALS-CURR-CNT (TOTAL-INDEX1 TOTAL-INDEX-TWELVE).

022122     ADD TOTALS-CURR-AMT (TOTAL-INDEX1 TOTAL-INDEX-THIRTEEN)
022122      TO TOTALS-CURR-AMT (TOTAL-INDEX1 TOTAL-INDEX-FOURTEEN).
022122     ADD TOTALS-CURR-CNT (TOTAL-INDEX1 TOTAL-INDEX-THIRTEEN)
022122      TO TOTALS-CURR-CNT (TOTAL-INDEX1 TOTAL-INDEX-FOURTEEN).
022122
022122     ADD TOTALS-CURR-AMT (TOTAL-INDEX1 TOTAL-INDEX-FIFTEEN)
022122      TO TOTALS-CURR-AMT (TOTAL-INDEX1 TOTAL-INDEX-SIXTEEN).
022122     ADD TOTALS-CURR-CNT (TOTAL-INDEX1 TOTAL-INDEX-FIFTEEN)
022122      TO TOTALS-CURR-CNT (TOTAL-INDEX1 TOTAL-INDEX-SIXTEEN).
01417                                                                   EL317
01422                                                                   EL317
01423      MOVE ZERO TO TOTALS-CURR-AMT (TOTAL-INDEX1 TOTAL-INDEX-ONE)  EL317
01424                  TOTALS-CURR-CNT (TOTAL-INDEX1 TOTAL-INDEX-ONE)   EL317
01425                  TOTALS-CURR-AMT (TOTAL-INDEX1 TOTAL-INDEX-THREE) EL317
01426                  TOTALS-CURR-CNT (TOTAL-INDEX1 TOTAL-INDEX-THREE) EL317
122402                 TOTALS-CURR-AMT (TOTAL-INDEX1 TOTAL-INDEX-FIVE)
122402                 TOTALS-CURR-CNT (TOTAL-INDEX1 TOTAL-INDEX-FIVE)
121203                 TOTALS-CURR-AMT (TOTAL-INDEX1 TOTAL-INDEX-SEVEN)
121203                 TOTALS-CURR-CNT (TOTAL-INDEX1 TOTAL-INDEX-SEVEN)
052614                 TOTALS-CURR-AMT (TOTAL-INDEX1 TOTAL-INDEX-NINE)
052614                 TOTALS-CURR-CNT (TOTAL-INDEX1 TOTAL-INDEX-NINE)
100518                 TOTALS-CURR-AMT (TOTAL-INDEX1 TOTAL-INDEX-ELEVEN)
100518                 TOTALS-CURR-CNT (TOTAL-INDEX1 TOTAL-INDEX-ELEVEN)
100518               TOTALS-CURR-AMT (TOTAL-INDEX1 TOTAL-INDEX-THIRTEEN)
100518               TOTALS-CURR-CNT (TOTAL-INDEX1 TOTAL-INDEX-THIRTEEN)
100518                TOTALS-CURR-AMT (TOTAL-INDEX1 TOTAL-INDEX-FIFTEEN)
100518               TOTALS-CURR-CNT (TOTAL-INDEX1 TOTAL-INDEX-FIFTEEN).
01427                                                                   EL317
01428      IF TOTAL-INDEX1 < WS-TOTAL-INDEX-MAX                         EL317
01429          SET TOTAL-INDEX1 UP BY +1                                EL317
01430          GO TO 4200-PRINT-TOTALS.                                 EL317
01431                                                                   EL317
01432      EJECT                                                        EL317
01433 *    NOTE ******************************************************* EL317
01434 *         *          PRINT THE REINSURANCE TOTALS               * EL317
01435 *         *******************************************************.EL317
01436                                                                   EL317
01437      MOVE '0 TOTAL REINSURANCE PAYMENTS'   TO  WS-TOTAL-LINE1.    EL317
01438                                                                   EL317
01439      IF WS-REPORT-SW = ZERO                                       EL317
01440          MOVE CR-PMT-CNTS                  TO  WS-T1-CURR-COUNT   EL317
01441          MOVE CR-PMT-AMTS                  TO  WS-T1-CURR-AMOUNT  EL317
01442      ELSE                                                         EL317
01443          MOVE FR-PMT-CNTS                  TO  WS-T1-CURR-COUNT   EL317
01444          MOVE FR-PMT-AMTS                  TO  WS-T1-CURR-AMOUNT. EL317
01445                                                                   EL317
01446      MOVE WS-TOTAL-LINE1                   TO  PRT.               EL317
01447      PERFORM WRITE-A-LINE.                                        EL317
01448                                                                   EL317
01449      MOVE '  VOIDED  REINSURANCE PAYMENTS' TO  WS-TOTAL-LINE1.    EL317
01450                                                                   EL317
01451      IF WS-REPORT-SW = ZERO                                       EL317
01452          MOVE CR-PMT-VOID-CNTS             TO  WS-T1-CURR-COUNT   EL317
01453          MOVE CR-PMT-VOID-AMTS             TO  WS-T1-CURR-AMOUNT  EL317
01454      ELSE                                                         EL317
01455          MOVE FR-PMT-VOID-CNTS             TO  WS-T1-CURR-COUNT   EL317
01456          MOVE FR-PMT-VOID-AMTS             TO  WS-T1-CURR-AMOUNT. EL317
01457                                                                   EL317
01458      MOVE WS-TOTAL-LINE1                   TO  PRT.               EL317
01459      PERFORM WRITE-A-LINE.                                        EL317
01460                                                                   EL317
01461      MOVE '  NET REINSURANCE PAYMENTS'    TO  WS-TOTAL-LINE1.     EL317
01462                                                                   EL317
01463      IF WS-REPORT-SW = ZERO                                       EL317
01464          SUBTRACT CR-PMT-VOID-CNTS FROM CR-PMT-CNTS               EL317
01465                                      GIVING   WS-T1-CURR-COUNT    EL317
01466          SUBTRACT CR-PMT-VOID-AMTS FROM CR-PMT-AMTS               EL317
01467                                      GIVING   WS-T1-CURR-AMOUNT   EL317
01468      ELSE                                                         EL317
01469          SUBTRACT FR-PMT-VOID-CNTS FROM FR-PMT-CNTS               EL317
01470                                      GIVING   WS-T1-CURR-COUNT    EL317
01471          SUBTRACT FR-PMT-VOID-AMTS FROM FR-PMT-AMTS               EL317
01472                                      GIVING   WS-T1-CURR-AMOUNT.  EL317
01473                                                                   EL317
01474      MOVE WS-TOTAL-LINE1         TO  PRT.                         EL317
01475      PERFORM WRITE-A-LINE.                                        EL317
01476                                                                   EL317
01477      IF WS-REPORT-SW = ZERO                                       EL317
01478          ADD CR-PMT-CNTS                  TO  FR-PMT-CNTS         EL317
01479          ADD CR-PMT-AMTS                  TO  FR-PMT-AMTS         EL317
01480          ADD CR-PMT-VOID-CNTS             TO  FR-PMT-VOID-CNTS    EL317
01481          ADD CR-PMT-VOID-AMTS             TO  FR-PMT-VOID-AMTS    EL317
01482          MOVE ZEROS                       TO  CR-PMT-CNTS         EL317
01483                                               CR-PMT-AMTS         EL317
01484                                               CR-PMT-VOID-CNTS    EL317
01485                                               CR-PMT-VOID-AMTS.   EL317
01486                                                                   EL317
01487      EJECT                                                        EL317
01488 *    NOTE ******************************************************* EL317
01489 *         *          PRINT THE TOTALS BY BENEFIT TYPE           * EL317
01490 *         *******************************************************.EL317
01491                                                                   EL317
01492      MOVE +99                    TO  WS-LINE-COUNT.               EL317
01493      MOVE +2                     TO  WS-HEADING-SW.               EL317
01494                                                                   EL317
01495      MOVE LIFE-OVERRIDE-L6       TO  WS7-OVERIDE-VALUE.           EL317
01496                                                                   EL317
01497      PERFORM 5000-PRINT-BENEFIT-TOTALS                            EL317
01498          VARYING CLAS-INDEX FROM CLAS-STARTL BY +1                EL317
01499              UNTIL CLAS-INDEX > CLAS-MAXL.                        EL317
01500                                                                   EL317
01501      MOVE +99                    TO  WS-LINE-COUNT.               EL317
01502      MOVE AH-OVERRIDE-L6         TO  WS7-OVERIDE-VALUE.           EL317
01503                                                                   EL317
01504      PERFORM 5000-PRINT-BENEFIT-TOTALS                            EL317
01505          VARYING CLAS-INDEX FROM CLAS-STARTA BY +1                EL317
01506              UNTIL CLAS-INDEX > CLAS-MAXA.                        EL317
01507                                                                   EL317
01508      MOVE ZERO                   TO  WS-HEADING-SW.               EL317
01509      MOVE +99                    TO  WS-LINE-COUNT.               EL317
01510                                                                   EL317
01511 *    NOTE ******************************************************* EL317
01512 *         *          PRINT THE TOTALS BY PAYEE TYPE             * EL317
01513 *         *******************************************************.EL317
01514                                                                   EL317
122402*    IF DTE-CLIENT NOT = 'FGL'                                    EL317
01516          GO TO 4900-EXIT.                                         EL317
01517                                                                   EL317
122402*    MOVE +99                    TO  WS-LINE-COUNT.               EL317
122402*    MOVE +3                     TO  WS-HEADING-SW.               EL317
01520                                                                   EL317
122402*    MOVE LIFE-OVERRIDE-L6       TO  WSA-OVERIDE-VALUE.           EL317
01522                                                                   EL317
122402*    PERFORM 6000-PRINT-PAYEE-TOTALS THRU 6099-EXIT               EL317
122402*            VARYING TOT-NDX1 FROM +1 BY +1                       EL317
122402*            UNTIL TOT-NDX1 > +6                                  EL317
122402*            AFTER TOT-NDX2 FROM +1 BY +1                         EL317
122402*            UNTIL TOT-NDX2 > +1.                                 EL317
01528                                                                   EL317
122402*    MOVE +99                    TO  WS-LINE-COUNT.               EL317
122402*    MOVE AH-OVERRIDE-L6         TO  WSA-OVERIDE-VALUE.           EL317
01531                                                                   EL317
01532                                                                   EL317
122402*    PERFORM 6000-PRINT-PAYEE-TOTALS THRU 6099-EXIT               EL317
122402*            VARYING TOT-NDX1 FROM +1 BY +1                       EL317
122402*            UNTIL TOT-NDX1 > +6                                  EL317
122402*            AFTER TOT-NDX2 FROM +2 BY +1                         EL317
122402*            UNTIL TOT-NDX2 > +2.                                 EL317
01538                                                                   EL317
122402*    MOVE ZERO                   TO  WS-HEADING-SW.               EL317
122402*    MOVE +99                    TO  WS-LINE-COUNT.               EL317
01541                                                                   EL317
122402*    PERFORM 7000-ACCUM-FINAL-TOTALS THRU 7099-EXIT               EL317
122402*            VARYING TOT-NDX1 FROM +1 BY +1                       EL317
122402*            UNTIL TOT-NDX1 > +6                                  EL317
122402*            AFTER TOT-NDX2 FROM +1 BY +1                         EL317
122402*            UNTIL TOT-NDX2 > +2.                                 EL317
01547                                                                   EL317
122402*    PERFORM 7100-INIT-CARRIER-VALUES THRU 7199-EXIT              EL317
122402*            VARYING TOT-NDX1 FROM +1 BY +1                       EL317
122402*            UNTIL TOT-NDX1 > +6                                  EL317
122402*            AFTER TOT-NDX2 FROM +1 BY +1                         EL317
122402*            UNTIL TOT-NDX2 > +2.                                 EL317
01553                                                                   EL317
01554  4900-EXIT.                                                       EL317
01555      EXIT.                                                        EL317
01556                                                                   EL317
01557      EJECT                                                        EL317
01558  5000-PRINT-BENEFIT-TOTALS SECTION.                               EL317
01559                                                                   EL317
01560 *    NOTE ******************************************************* EL317
01561 *         *       THIS SECTION PRINT THE TOTALS FOR THE LIFE    * EL317
01562 *         *  AND A&H BENEFITS.                                  * EL317
01563 *         *******************************************************.EL317
01564                                                                   EL317
01565      IF WS-REPORT-SW = ZERO AND                                   EL317
01566          CT-CURR-BENEFIT-AMT (CLAS-INDEX) = ZERO AND              EL317
01567          CT-CURR-BENEFIT-CNT (CLAS-INDEX) = ZERO AND              EL317
01568          CT-CURR-BENEFIT-VOIDED-AMT (CLAS-INDEX) = ZERO AND       EL317
01569          CT-CURR-BENEFIT-VOIDED-CNT (CLAS-INDEX) = ZERO           EL317
01570              GO TO 5099-EXIT.                                     EL317
01571                                                                   EL317
01572      IF WS-REPORT-SW NOT = ZERO AND                               EL317
01573          FT-CURR-BENEFIT-AMT (CLAS-INDEX) = ZERO AND              EL317
01574          FT-CURR-BENEFIT-CNT (CLAS-INDEX) = ZERO AND              EL317
01575          FT-CURR-BENEFIT-VOIDED-AMT (CLAS-INDEX) = ZERO AND       EL317
01576          FT-CURR-BENEFIT-VOIDED-CNT (CLAS-INDEX) = ZERO           EL317
01577              GO TO 5099-EXIT.                                     EL317
01578                                                                   EL317
01579      MOVE '0'                    TO  WS-TOTAL-LINE1.              EL317
01580                                                                   EL317
01581      MOVE CLAS-I-BEN (CLAS-INDEX)  TO  WS-T1-BENEFIT-CODE.        EL317
01582      MOVE CLAS-I-AB10 (CLAS-INDEX) TO  WS-T1-BENEFIT-DESC.        EL317
01583                                                                   EL317
01584      IF WS-REPORT-SW = ZERO                                       EL317
01585          MOVE CT-CURR-BENEFIT-AMT (CLAS-INDEX)                    EL317
01586                                  TO  WS-T1-CURR-AMOUNT            EL317
01587          MOVE CT-CURR-BENEFIT-CNT (CLAS-INDEX)                    EL317
01588                                  TO  WS-T1-CURR-COUNT             EL317
01589      ELSE                                                         EL317
01590          MOVE FT-CURR-BENEFIT-AMT (CLAS-INDEX)                    EL317
01591                                  TO  WS-T1-CURR-AMOUNT            EL317
01592          MOVE FT-CURR-BENEFIT-CNT (CLAS-INDEX)                    EL317
01593                                  TO  WS-T1-CURR-COUNT.            EL317
01594                                                                   EL317
01595      MOVE WS-TOTAL-LINE1         TO  PRT.                         EL317
01596      PERFORM WRITE-A-LINE.                                        EL317
01597      MOVE SPACES                 TO  WS-TOTAL-LINE1.              EL317
01598      MOVE '   VOIDED'            TO  WS-T1-BENEFIT-DESC.          EL317
01599                                                                   EL317
01600      IF WS-REPORT-SW = ZERO                                       EL317
01601          MOVE CT-CURR-BENEFIT-VOIDED-AMT (CLAS-INDEX)             EL317
01602                                  TO  WS-T1-CURR-AMOUNT            EL317
01603          MOVE CT-CURR-BENEFIT-VOIDED-CNT (CLAS-INDEX)             EL317
01604                                  TO  WS-T1-CURR-COUNT             EL317
01605      ELSE                                                         EL317
01606          MOVE FT-CURR-BENEFIT-VOIDED-AMT (CLAS-INDEX)             EL317
01607                                  TO  WS-T1-CURR-AMOUNT            EL317
01608          MOVE FT-CURR-BENEFIT-VOIDED-CNT (CLAS-INDEX)             EL317
01609                                  TO  WS-T1-CURR-COUNT.            EL317
01610                                                                   EL317
01611      MOVE WS-TOTAL-LINE1      TO  PRT.                            EL317
01612      PERFORM WRITE-A-LINE.                                        EL317
01613      MOVE SPACES              TO  WS-TOTAL-LINE1.                 EL317
01614      MOVE '   NET'            TO  WS-T1-BENEFIT-DESC.             EL317
01615                                                                   EL317
01616      IF WS-REPORT-SW = ZERO                                       EL317
01617          SUBTRACT CT-CURR-BENEFIT-VOIDED-AMT (CLAS-INDEX)         EL317
01618              FROM CT-CURR-BENEFIT-AMT (CLAS-INDEX)                EL317
01619                                  GIVING  WS-T1-CURR-AMOUNT        EL317
01620      ELSE                                                         EL317
01621          SUBTRACT FT-CURR-BENEFIT-VOIDED-AMT (CLAS-INDEX)         EL317
01622              FROM FT-CURR-BENEFIT-AMT (CLAS-INDEX)                EL317
01623                                  GIVING  WS-T1-CURR-AMOUNT.       EL317
01624                                                                   EL317
01625      MOVE WS-TOTAL-LINE1         TO  PRT.                         EL317
01626      PERFORM WRITE-A-LINE.                                        EL317
01627                                                                   EL317
01628      ADD  CT-CURR-BENEFIT-AMT (CLAS-INDEX)                        EL317
01629        TO FT-CURR-BENEFIT-AMT (CLAS-INDEX).                       EL317
01630      ADD  CT-CURR-BENEFIT-CNT (CLAS-INDEX)                        EL317
01631        TO FT-CURR-BENEFIT-CNT (CLAS-INDEX).                       EL317
01632      ADD  CT-CURR-BENEFIT-VOIDED-AMT (CLAS-INDEX)                 EL317
01633        TO FT-CURR-BENEFIT-VOIDED-AMT (CLAS-INDEX).                EL317
01634      ADD  CT-CURR-BENEFIT-VOIDED-CNT (CLAS-INDEX)                 EL317
01635        TO FT-CURR-BENEFIT-VOIDED-CNT (CLAS-INDEX).                EL317
01636                                                                   EL317
01637      MOVE ZERO  TO  CT-CURR-BENEFIT-AMT (CLAS-INDEX)              EL317
01638                     CT-CURR-BENEFIT-CNT (CLAS-INDEX).             EL317
01639                                                                   EL317
01640      MOVE ZERO  TO  CT-CURR-BENEFIT-VOIDED-AMT (CLAS-INDEX)       EL317
01641                     CT-CURR-BENEFIT-VOIDED-CNT (CLAS-INDEX).      EL317
01642                                                                   EL317
01643  5099-EXIT.                                                       EL317
01644      EXIT.                                                        EL317
01645                                                                   EL317
01646      EJECT                                                        EL317
01647  6000-PRINT-PAYEE-TOTALS SECTION.                                 EL317
01648                                                                   EL317
01649 *    NOTE ******************************************************* EL317
01650 *         *       THIS SECTION PRINTS THE PAYEE TYPE TOTALS     * EL317
01651 *         *       FOR LIFE AND A&H BENEFITS                     * EL317
01652 *         *******************************************************.EL317
01653                                                                   EL317
01654      MOVE '0'                    TO  WS-TOTAL-LINE2.              EL317
01655      MOVE 'ERROR'                TO  WS-T2-PAYEE-DESC.            EL317
01656                                                                   EL317
01657      IF TOT-NDX1 = +1                                             EL317
01658          MOVE 'INSURED'      TO  WS-T2-PAYEE-DESC                 EL317
01659      ELSE                                                         EL317
01660      IF TOT-NDX1 = +2                                             EL317
01661          MOVE 'BENEFICIARY'  TO  WS-T2-PAYEE-DESC                 EL317
01662      ELSE                                                         EL317
01663      IF TOT-NDX1 = +3                                             EL317
01664          MOVE 'ACCOUNT'      TO  WS-T2-PAYEE-DESC                 EL317
01665      ELSE                                                         EL317
01666      IF TOT-NDX1 = +4                                             EL317
01667          MOVE 'PHYSICIAN'    TO  WS-T2-PAYEE-DESC                 EL317
01668      ELSE                                                         EL317
01669      IF TOT-NDX1 = +5                                             EL317
01670          MOVE 'OTHER 1'      TO  WS-T2-PAYEE-DESC                 EL317
01671      ELSE                                                         EL317
01672      IF TOT-NDX1 = +6                                             EL317
01673          MOVE 'OTHER 2'      TO  WS-T2-PAYEE-DESC.                EL317
01674                                                                   EL317
01675      IF WS-REPORT-SW = ZERO                                       EL317
01676          MOVE CPT-PMT-CNTS (TOT-NDX1 TOT-NDX2)                    EL317
01677                                  TO  WS-T2-CURR-COUNT             EL317
01678          MOVE CPT-PMT-AMTS (TOT-NDX1 TOT-NDX2)                    EL317
01679                                  TO  WS-T2-CURR-AMOUNT            EL317
01680      ELSE                                                         EL317
01681          MOVE FPT-PMT-CNTS (TOT-NDX1 TOT-NDX2)                    EL317
01682                                  TO  WS-T2-CURR-COUNT             EL317
01683          MOVE FPT-PMT-AMTS (TOT-NDX1 TOT-NDX2)                    EL317
01684                                  TO  WS-T2-CURR-AMOUNT.           EL317
01685                                                                   EL317
01686      MOVE WS-TOTAL-LINE2         TO  PRT.                         EL317
01687      PERFORM WRITE-A-LINE.                                        EL317
01688                                                                   EL317
01689      MOVE SPACES                 TO  WS-TOTAL-LINE2.              EL317
01690      MOVE '     VOIDED'          TO  WS-T2-PAYEE-DESC.            EL317
01691                                                                   EL317
01692      IF WS-REPORT-SW = ZERO                                       EL317
01693          MOVE CPT-PMT-VOID-CNTS (TOT-NDX1 TOT-NDX2)               EL317
01694                                      TO  WS-T2-CURR-COUNT         EL317
01695          MOVE CPT-PMT-VOID-AMTS (TOT-NDX1 TOT-NDX2)               EL317
01696                                      TO  WS-T2-CURR-AMOUNT        EL317
01697      ELSE                                                         EL317
01698          MOVE FPT-PMT-VOID-CNTS (TOT-NDX1 TOT-NDX2)               EL317
01699                                      TO  WS-T2-CURR-COUNT         EL317
01700          MOVE FPT-PMT-VOID-AMTS (TOT-NDX1 TOT-NDX2)               EL317
01701                                      TO  WS-T2-CURR-AMOUNT.       EL317
01702                                                                   EL317
01703      MOVE WS-TOTAL-LINE2      TO  PRT.                            EL317
01704      PERFORM WRITE-A-LINE.                                        EL317
01705                                                                   EL317
01706      MOVE SPACES              TO  WS-TOTAL-LINE2.                 EL317
01707      MOVE '     NET'          TO  WS-T1-BENEFIT-DESC.             EL317
01708                                                                   EL317
01709      IF WS-REPORT-SW = ZERO                                       EL317
01710          SUBTRACT CPT-PMT-VOID-CNTS (TOT-NDX1 TOT-NDX2)           EL317
01711              FROM CPT-PMT-CNTS (TOT-NDX1 TOT-NDX2)                EL317
01712                                  GIVING  WS-T2-CURR-COUNT         EL317
01713          SUBTRACT CPT-PMT-VOID-AMTS (TOT-NDX1 TOT-NDX2)           EL317
01714              FROM CPT-PMT-AMTS (TOT-NDX1 TOT-NDX2)                EL317
01715                                  GIVING  WS-T2-CURR-AMOUNT        EL317
01716      ELSE                                                         EL317
01717          SUBTRACT FPT-PMT-VOID-CNTS (TOT-NDX1 TOT-NDX2)           EL317
01718              FROM FPT-PMT-CNTS (TOT-NDX1 TOT-NDX2)                EL317
01719                                  GIVING  WS-T2-CURR-COUNT         EL317
01720          SUBTRACT FPT-PMT-VOID-AMTS (TOT-NDX1 TOT-NDX2)           EL317
01721              FROM FPT-PMT-AMTS (TOT-NDX1 TOT-NDX2)                EL317
01722                                  GIVING  WS-T2-CURR-AMOUNT.       EL317
01723                                                                   EL317
01724      MOVE WS-TOTAL-LINE2        TO  PRT.                          EL317
01725      PERFORM WRITE-A-LINE.                                        EL317
01726                                                                   EL317
01727  6099-EXIT.                                                       EL317
01728      EXIT.                                                        EL317
01729                                                                   EL317
01730      EJECT                                                        EL317
01731  6100-BUILD-GL-CLAIM-PAYMENT  SECTION.                            EL317
01732                                                                   EL317
01733      MOVE SPACES                 TO  CLAIM-PAYMENT-RECORD.        EL317
01734      MOVE 'CP'                   TO  CP-RECORD-ID.                EL317
01735      MOVE EX-POSITIONING-CODE    TO  CP-POSITIONING-CODE.         EL317
01736      MOVE EX-EXTRACT-CODE        TO  CP-EXTRACT-CODE.             EL317
01737      MOVE EX-COMPANY-CD          TO  CP-COMPANY-CD.               EL317
01738      MOVE EX-COMPANY-ID          TO  CP-COMPANY-ID.               EL317
01739      MOVE EX-RECORD-TYPE         TO  CP-RECORD-TYPE.              EL317
01740      MOVE EX-SB-CARRIER          TO  CP-SB-CARRIER.               EL317
01741      MOVE EX-SB-CHECK-NO         TO  CP-SB-CHECK-NO.              EL317
01742      MOVE EX-BA-CERT-NO          TO  CP-BA-CERT-NO.               EL317
01743      MOVE EX-BA-CLAIM-NO         TO  CP-BA-CLAIM-NO.              EL317
01744      MOVE EX-BA-STATE            TO  CP-BA-STATE.                 EL317
01745      MOVE EX-BA-ACCOUNT          TO  CP-BA-ACCOUNT.               EL317
01746      MOVE EX-BA-ACCOUNT-NAME     TO  CP-BA-ACCOUNT-NAME.          EL317
01747      MOVE EX-BA-GROUPING         TO  CP-BA-GROUPING.              EL317
01748      MOVE EX-BA-CERT-EFF-DT      TO  CP-BA-CERT-EFF-DT.           EL317
01749      MOVE EX-BA-CLAIM-TYPE       TO  CP-BA-CLAIM-TYPE.            EL317
01750      MOVE EX-BA-CLAIM-PREM-TYPE  TO  CP-BA-CLAIM-PREM-TYPE.       EL317
01751      MOVE EX-BA-PAYMENT-TYPE     TO  CP-BA-PAYMENT-TYPE.          EL317
01752      MOVE EX-BA-PAID-FROM-DT     TO  CP-BA-PAID-FROM-DT.          EL317
01753      MOVE EX-BA-PAID-THRU-DT     TO  CP-BA-PAID-THRU-DT.          EL317
01754      MOVE EX-BA-INCURRED-DT      TO  CP-BA-INCURRED-DT.           EL317
01755      MOVE EX-BA-REPORTED-DT      TO  CP-BA-REPORTED-DT.           EL317
01756      MOVE EX-BA-CHECK-WRITTEN-DT TO  CP-BA-CHECK-WRITTEN-DT.      EL317
01757      MOVE EX-BA-PAYMENT-AMOUNT   TO  CP-BA-PAYMENT-AMOUNT.        EL317
01758      MOVE EX-BA-INSURED-AGE      TO  CP-BA-INSURED-AGE.           EL317
01759      MOVE EX-BA-INSURED-LAST-NAME TO CP-BA-INSURED-LAST-NAME.     EL317
01760      MOVE EX-BA-DAYS-IN-PERIOD   TO  CP-BA-DAYS-IN-PERIOD.        EL317
01761      MOVE EX-BA-PAYEE-TYPE-CD    TO  CP-BA-PAYEE-TYPE-CD.         EL317
01762      MOVE EX-BA-PAYEES-NAME      TO  CP-BA-PAYEES-NAME.           EL317
01763      MOVE EX-BA-VOID-DT          TO  CP-BA-VOID-DT.               EL317
01764      MOVE EX-BA-CLAIM-STATUS     TO  CP-BA-CLAIM-STATUS.          EL317
01765      MOVE EX-BA-ORIG-TERM        TO  CP-BA-ORIG-TERM.             EL317
01766      MOVE EX-BA-MEMBER-NUMBER    TO  CP-BA-MEMBER-NUMBER.         EL317
01767      MOVE EX-BA-SOC-SEC-NO       TO  CP-BA-SOC-SEC-NO.            EL317
01768      MOVE EX-BA-PAYMENT-ORIGIN   TO  CP-BA-PAYMENT-ORIGIN.        EL317
01769      MOVE EX-BA-RESERVE-FLAG     TO  CP-BA-RESERVE-FLAG.          EL317
01770      MOVE EX-BA-BENEFIT-CD       TO  CP-BA-BENEFIT-CD.            EL317
01771      MOVE EX-BA-EXPENSE-TYPE     TO  CP-BA-EXPENSE-TYPE.          EL317
01772      MOVE EX-BA-EXPENSE-PER-PMT  TO  CP-BA-EXPENSE-PER-PMT.       EL317
01773      MOVE EX-BA-CERT-STATUS      TO  CP-BA-CERT-STATUS.           EL317
01774      MOVE EX-BA-IND-GRP-TYPE     TO  CP-BA-IND-GRP-TYPE.          EL317
01775                                                                   EL317
01776      WRITE CLAIM-PAYMENT-RECORD.                                  EL317
01777                                                                   EL317
01778  6199-EXIT.                                                       EL317
01779      EXIT.                                                        EL317
01780                                                                   EL317
01781      EJECT                                                        EL317
01782  7000-ACCUM-FINAL-TOTALS SECTION.                                 EL317
01783                                                                   EL317
01784      ADD  CPT-PMT-CNTS  (TOT-NDX1 TOT-NDX2) TO                    EL317
01785           FPT-PMT-CNTS  (TOT-NDX1 TOT-NDX2).                      EL317
01786      ADD  CPT-PMT-AMTS  (TOT-NDX1 TOT-NDX2) TO                    EL317
01787           FPT-PMT-AMTS  (TOT-NDX1 TOT-NDX2).                      EL317
01788                                                                   EL317
01789      ADD CPT-PMT-VOID-CNTS   (TOT-NDX1 TOT-NDX2) TO               EL317
01790          FPT-PMT-VOID-CNTS   (TOT-NDX1 TOT-NDX2).                 EL317
01791      ADD CPT-PMT-VOID-AMTS   (TOT-NDX1 TOT-NDX2) TO               EL317
01792          FPT-PMT-VOID-AMTS   (TOT-NDX1 TOT-NDX2).                 EL317
01793                                                                   EL317
01794  7099-EXIT.                                                       EL317
01795      EXIT.                                                        EL317
01796                                                                   EL317
01797  7100-INIT-CARRIER-VALUES SECTION.                                EL317
01798                                                                   EL317
01799      MOVE ZEROS  TO  CPT-PMT-CNTS          (TOT-NDX1 TOT-NDX2)    EL317
01800                      CPT-PMT-AMTS          (TOT-NDX1 TOT-NDX2)    EL317
01801                      CPT-PMT-VOID-CNTS     (TOT-NDX1 TOT-NDX2)    EL317
01802                      CPT-PMT-VOID-AMTS     (TOT-NDX1 TOT-NDX2).   EL317
01803  7199-EXIT.                                                       EL317
01804      EXIT.                                                        EL317
01805                                                                   EL317
01806  7200-INIT-PAYEE-TOTAL-VALUES SECTION.                            EL317
01807                                                                   EL317
01808      MOVE ZEROS  TO  CPT-PMT-CNTS          (TOT-NDX1 TOT-NDX2)    EL317
01809                      CPT-PMT-AMTS          (TOT-NDX1 TOT-NDX2)    EL317
01810                      CPT-PMT-VOID-CNTS     (TOT-NDX1 TOT-NDX2)    EL317
01811                      CPT-PMT-VOID-AMTS     (TOT-NDX1 TOT-NDX2)    EL317
01812                                                                   EL317
01813                      FPT-PMT-CNTS          (TOT-NDX1 TOT-NDX2)    EL317
01814                      FPT-PMT-AMTS          (TOT-NDX1 TOT-NDX2)    EL317
01815                      FPT-PMT-VOID-CNTS     (TOT-NDX1 TOT-NDX2)    EL317
01816                      FPT-PMT-VOID-AMTS     (TOT-NDX1 TOT-NDX2).   EL317
01817  7299-EXIT.                                                       EL317
01818      EXIT.                                                        EL317
01819                                                                   EL317
01820      EJECT                                                        EL317
01821  8100-GET-CARRIER-NAME SECTION.                                   EL317
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
01838  8500-DATE-CONVERSION SECTION. COPY ELCDCS.                       EL317
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
01880      IF WS-HEADING-SW = +2                                        EL317
01881          MOVE WS-HEADING7        TO  PRT                          EL317
01882          PERFORM WRITE-PRINTER.                                   EL317
01883                                                                   EL317
122402*    IF WS-HEADING-SW = +3                                        EL317
122402*        MOVE WS-HEADINGA        TO  PRT                          EL317
122402*        PERFORM WRITE-PRINTER.                                   EL317
01887                                                                   EL317
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
01902  WPS-020. COPY ELCPRT2X.                                          EL317
01903                                                                   EL317
01904  OPEN-FILES SECTION.                                              EL317
01905                                                                   EL317
01906  OFS-010.                                                         EL317
01907      OPEN INPUT REPORTS-EXTRACT-FILE                              EL317
01908           OUTPUT ABC-INTERFACE-FILE RPT-EXTRACT
01909                  PRNTR.                                           EL317
01910                                                                   EL317
122402*    IF DTE-CLIENT = 'NCL' OR 'NCX'                               EL317
122402*        OPEN OUTPUT GL-DAILY-CLAIM-PAYMENTS.                     EL317
01913                                                                   EL317
01914      SET CT-INDEX TO +1.                                          EL317
01915                                                                   EL317
01916  OFS-020.                                                         EL317
01917      MOVE ZERO  TO  CT-CURR-BENEFIT-AMT (CT-INDEX)                EL317
01918                     CT-CURR-BENEFIT-CNT (CT-INDEX)                EL317
01919                     FT-CURR-BENEFIT-AMT (CT-INDEX)                EL317
01920                     FT-CURR-BENEFIT-CNT (CT-INDEX).               EL317
01921                                                                   EL317
01922      MOVE ZERO  TO  CT-CURR-BENEFIT-VOIDED-AMT (CT-INDEX)         EL317
01923                     CT-CURR-BENEFIT-VOIDED-CNT (CT-INDEX)         EL317
01924                     FT-CURR-BENEFIT-VOIDED-AMT (CT-INDEX)         EL317
01925                     FT-CURR-BENEFIT-VOIDED-CNT (CT-INDEX).        EL317
01926                                                                   EL317
092602     IF CT-INDEX < +900                                           EL317
01928          SET CT-INDEX UP BY +1                                    EL317
01929          GO TO OFS-020.                                           EL317
01930                                                                   EL317
01931      MOVE RUN-MO                  TO  DC-MDY-MONTH                   CL**2
01932                                       3800-MONTH.                    CL**2
01933      MOVE RUN-DA                  TO  DC-MDY-DAY                     CL**2
01934                                       3800-DAY.                      CL**2
01935      MOVE RUN-YR                  TO  DC-MDY-YEAR                    CL**2
01936                                       3800-YEAR.                     CL**2
01937      MOVE '4'                     TO  DC-OPTION-CODE.                CL**2
01938      PERFORM 8500-DATE-CONVERSION.                                EL317
01939      MOVE DC-JULIAN-DATE          TO  3800-JULIAN-DATE.              CL**2
01940                                                                   EL317
122402*    IF DTE-CLIENT = 'CIM'                                        EL317
122402*        MOVE 'WCO'       TO  3800-COMPANY                        EL317
122402*    ELSE                                                         EL317
122402*    IF DTE-CLIENT = 'UFL' OR 'UFR'                               EL317
122402*        MOVE 'UFL'       TO  3800-COMPANY                        EL317
122402*    ELSE                                                         EL317
122402*    IF DTE-CLIENT = 'WFL'                                        EL317
122402*        MOVE 'FLA'       TO  3800-COMPANY                        EL317
122402*    ELSE                                                         EL317
01950          MOVE DTE-CLIENT  TO  3800-COMPANY.                       EL317
01951                                                                   EL317
01952      SET TOTAL-INDEX-ONE   TO +1.                                 EL317
01953      SET TOTAL-INDEX-TWO   TO +2.                                 EL317
01954      SET TOTAL-INDEX-THREE TO +3.                                 EL317
01955      SET TOTAL-INDEX-FOUR  TO +4.                                 EL317
122402     SET TOTAL-INDEX-FIVE  TO +5.
122402     SET TOTAL-INDEX-SIX   TO +6.
121203     SET TOTAL-INDEX-SEVEN TO +7.
121203     SET TOTAL-INDEX-EIGHT TO +8.
052614     SET TOTAL-INDEX-NINE  TO +9.
052614     SET TOTAL-INDEX-TEN   TO +10.
100518     SET TOTAL-INDEX-ELEVEN  TO +11.
100518     SET TOTAL-INDEX-TWELVE  TO +12.
022122     SET TOTAL-INDEX-THIRTEEN  TO +13.
022122     SET TOTAL-INDEX-FOURTEEN  TO +14.
022122     SET TOTAL-INDEX-FIFTEEN   TO +15.
022122     SET TOTAL-INDEX-SIXTEEN   TO +16.
01956                                                                   EL317
01957  OFS-030.                                                         EL317
01958      PERFORM 7200-INIT-PAYEE-TOTAL-VALUES THRU 7299-EXIT          EL317
01959              VARYING TOT-NDX1 FROM +1 BY +1                       EL317
01960              UNTIL TOT-NDX1 > +6                                  EL317
01961              AFTER TOT-NDX2 FROM +1 BY +1                         EL317
01962              UNTIL TOT-NDX2 > +2.                                 EL317
01963                                                                   EL317
01964      SET INSD-NDX          TO +1.                                 EL317
01965      SET BENE-NDX          TO +2.                                 EL317
01966      SET ACCT-NDX          TO +3.                                 EL317
01967      SET PHYS-NDX          TO +4.                                 EL317
01968      SET OTH1-NDX          TO +5.                                 EL317
01969      SET OTH2-NDX          TO +6.                                 EL317
01970      SET LF-NDX            TO +1.                                 EL317
01971      SET AH-NDX            TO +2.                                 EL317
122402     SET IU-NDX            TO +3.
121203     SET GP-NDX            TO +4.
052614     SET FL-NDX            TO +5.
022122     SET BR-NDX            TO +6.
022122     SET HS-NDX            TO +7.
100518     SET OT-NDX            TO +8.

           MOVE SPACES           TO EXTRACT-RECORD
           MOVE ';'              TO EXT-DEL01
                                    EXT-DEL02
                                    EXT-DEL03
                                    EXT-DEL04
                                    EXT-DEL05
                                    EXT-DEL06
                                    EXT-DEL07
                                    EXT-DEL08
                                    EXT-DEL09
                                    EXT-DEL10
                                    EXT-DEL11
                                    EXT-DEL12
                                    EXT-DEL13
                                    EXT-DEL14
                                    EXT-DEL15
                                    EXT-DEL16
                                    EXT-DEL17
                                    EXT-DEL18
                                    EXT-DEL19
                                    EXT-DEL20
                                    EXT-DEL21
                                    EXT-DEL22
           MOVE 'E'                    TO EXT-EOR
           STRING RUN-MO '/' RUN-DA '/' RUN-CCYY
              DELIMITED BY SIZE INTO EXT-RPT-DATE
           END-STRING
           MOVE EXTRACT-RECORD         TO WS-INIT-EXTRACT

          .
01973  OFS-EXIT.                                                        EL317
01974      EXIT.                                                        EL317
01975                                                                   EL317
01976  CLOSE-FILES SECTION.                                             EL317
01977                                                                   EL317
01978  CFS-010. COPY ELCPRTCX.                                          EL317
01979                                                                   EL317
01980      CLOSE REPORTS-EXTRACT-FILE                                   EL317
01981            ABC-INTERFACE-FILE                                     EL317
01982            PRNTR RPT-EXTRACT
01983                                                                   EL317
122402*    IF DTE-CLIENT = 'NCL' OR 'NCX'                               EL317
122402*        CLOSE  GL-DAILY-CLAIM-PAYMENTS.                          EL317

070714****=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=***
070714****                                                           ***
070714****   Set up the month-end auto balancing.                    ***
070714****   Bypass if this is a daily run.                          ***
070714****                                                           ***
070714****=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=***
070714
070714     IF DTE-PRC-OPT = '2'
070714        go to cfs-exit
070714     end-if

070714     OPEN I-O ERMEBL.                                  
070714                                                       
070714     IF (ERMEBL-FILE-STATUS <> ZERO)
070714        AND (ERMEBL-FILE-STATUS <> '97')
070714        MOVE 'N'                 TO ME-UPDATE-FLAG
070714        display ' me open ' ermebl-file-status
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
070714           display ' me read ' ermebl-file-status
070714           MOVE 'N'              TO ME-UPDATE-FLAG    
070714           CLOSE ERMEBL
070714        end-if
070714     end-if
070714                                                           
070714     IF ME-DO-UPDATE
070714        move hld-317-clms-tot    to me-317-clms-tot
070714        display ' update me ' me-317-clms-tot
070714        REWRITE MONTH-END-BALANCES
070714        CLOSE ERMEBL
070714     end-if

           .
01987  CFS-EXIT.                                                        EL317
01988      EXIT.                                                        EL317
01989                                                                   EL317
01990  ABEND-PGM SECTION. COPY ELCABEND.                                EL317
01991                                                                   EL317
