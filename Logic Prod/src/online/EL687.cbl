00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL687 .
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 09/20/95 08:42:38.
00007 *                            VMOD=2.019
00008 *
00009 *AUTHOR.    LOGIC, INC.
00010 *           DALLAS, TEXAS.
00011
00012 *DATE-COMPILED.
00013
00014 *SECURITY.   *****************************************************
00015 *            *                                                   *
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
00017 *            *                                                   *
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
00021 *            *                                                   *
00022 *            *****************************************************
00023
00024 *REMARKS.   TRANSACTION - EXG3
00025
00026 *        IN ORDER TO START THE PRINTING OF CHECKS, THIS FUNCTION
00027 *    IS USED TO QUALIFY THE CONTROL BATCHES TO BE PRINTED AND
00028 *    SPECIFY A PRINT TIME.  THE CHECK WRITER PROGRAM (EL688) IS
00029 *    STARTED BY THIS PROGRAM.
00030
00031 *    SCREENS     - EL687A - CHECK WRITER
00032
00033 *    ENTERED BY  - EL671  - REPORT MENU
00034
00035 *    EXIT TO     - EL671  - RESULT OF CLEAR
00036
00037 *    INPUT FILES - ERCHKQ - CHECK QUEUE
00038 *                  ERPYAJ - PENDING PAYMENTS AND ADJUSTMENTS
00039 *                  ELCNTL - CONTROL FILE
00040
00041 *    OUTPUT FILES - ERPYAJ - PENDING PAYMENTS AND ADJUSTMENTS
00042 *                   ERCHEK - CHECK MAINTENANCE
00043 *                   ERCHKQ - CHECK QUEUE
00044
00045 *    COMMAREA    - PASSED.  PRINT TIME PASSED TO CHECK WRITER AS
00046 *                  A 2 BYTE COMP NUMBER IN THE FIRST TWO BYTES OF
00047 *                  THE WORK AREA.  FORM TYPE IS PASSED AS
00048 *                  ENTRY-CD-1.
030612******************************************************************
030612*                   C H A N G E   L O G
030612*
030612* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
030612*-----------------------------------------------------------------
030612*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
030612* EFFECTIVE    NUMBER
030612*-----------------------------------------------------------------
030612* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
030612******************************************************************
00049
00050      EJECT
00051  ENVIRONMENT DIVISION.
00052
00053  DATA DIVISION.
00054
00055  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00056  77  LCP-WS-ADDR-COMP              PIC S9(8) COMP.
00057  77  LCP-WS-ADDR-PNTR              REDEFINES LCP-WS-ADDR-COMP
00058                                    USAGE POINTER.
00059  77  LCP-ONCTR-01                  PIC S9(8) COMP-3 VALUE ZERO.
00060  01  LCP-CURRENT-DATE-68.
00061      05  LCP-MONTH                 PIC X(2).
00062      05  FILLER                    PIC X VALUE '/'.
00063      05  LCP-DAY1                  PIC X(2).
00064      05  FILLER                    PIC X VALUE '/'.
00065      05  LCP-YEAR                  PIC X(2).
00066  01  LCP-CICS-DATE                 PIC 9(15).
00067
00068  77  FILLER  PIC X(32)  VALUE '********************************'.
00069  77  FILLER  PIC X(32)  VALUE '*    EL687 WORKING STORAGE     *'.
00070  77  FILLER  PIC X(32)  VALUE '************ V/M 2.019 *********'.
00071
00072  01  FILLER                          COMP-3.
00073      05  WS-NOT-FOUND                PIC S9          VALUE ZERO.
00074      05  WS-PRINTER-STARTED-SW       PIC S9          VALUE ZERO.
00075      05  WS-READNEXT-SW              PIC S9          VALUE ZERO.
00076      05  WS-LAST-ERROR-COUNT         PIC S9(3)       VALUE ZERO.
00077      05  WS-COMPLETED-SUCCESSFUL     PIC S9          VALUE ZERO.
00078        88  TRANSACTION-SUCCESSFUL                    VALUE +1 +2.
00079        88  CHECKS-WITHOUT-ADDRESSES                  VALUE +2.
00080
00081      05  WS-TIME-WORK                PIC S9(7)       VALUE ZERO.
00082      05  WS-TIME                     REDEFINES
00083          WS-TIME-WORK                PIC S9(3)V9(4).
00084      05  WS-HHMM                     REDEFINES
00085          WS-TIME-WORK                PIC S9(5)V99.
00086
00087      05  WS-PENDING-PAYMENTS-BROWSE-SW  PIC S9       VALUE ZERO.
00088      05  WS-CHECK-QUEUE-BROWSE-SW    PIC S9          VALUE ZERO.
00089      05  WS-CHECK-MAINT-BROWSE-SW    PIC S9          VALUE ZERO.
00090
00091      05  WS-NOT-RELEASED-COUNT       PIC S9(5)       VALUE ZERO.
00092      05  WS-NOT-RELEASED-AMOUNT      PIC S9(9)V99 VALUE ZERO.
00093      05  WS-RELEASED-COUNT           PIC S9(5)       VALUE ZERO.
00094      05  WS-RELEASED-AMOUNT          PIC S9(9)V99 VALUE ZERO.
00095
00096
00097      EJECT
00098  01  FILLER                          COMP SYNC.
00099      05  WS-TS-LENGTH                PIC S9(4)       VALUE +750.
00100      05  WS-ERCHKQ-LENGTH            PIC S9(4)       VALUE +100.
00101      05  WS-KEY-LENGTH               PIC S9(4)       VALUE ZERO.
00102
00103      05  WS-CHECK-QUE-COUNTER        PIC S9(8)       VALUE ZERO.
00104      05  WS-CHECK-COUNTER            PIC S9(4)       VALUE +10.
00105
00106      05  WS-LAST-CONTROL-GROUP       PIC S9(8)       VALUE ZERO.
00107      05  WS-TIMES-PRINTED            PIC S9(4)       VALUE ZERO.
00108
00109      05  WS-JOURNAL-FILE-ID          PIC S9(4)       VALUE +1.
00110      05  WS-JOURNAL-RECORD-LENGTH    PIC S9(4)       VALUE +773.
00111
00112      05  WS-SEQUENCE-NUMBER          PIC S9(4)       VALUE ZERO.
00113      05  WS-BEGIN-NUMBER             PIC S9(4)       VALUE ZERO.
00114
00115      05  WS-CQFCBAR                  PIC S9(9)       VALUE ZERO.
00116      05  WS-INDEX                    PIC S9(4)       VALUE ZERO.
00117
00118      EJECT
00119  01  FILLER.
00120
00121      05  WS-CONTROL-FILE-KEY.
00122          10  WS-CFK-COMPANY-ID       PIC X(3)        VALUE SPACES.
00123          10  WS-CFK-RECORD-TYPE      PIC X           VALUE SPACES.
00124          10  FILLER                  PIC XX          VALUE SPACES.
00125          10  WS-CFK-BENEFIT-NO       PIC XX          VALUE SPACES.
00126          10  WS-CFK-SEQUENCE-NO      PIC S9(4)       VALUE ZERO
00127                                      COMP.
00128
00129      05  WS-PENDING-PAYMENTS-KEY.
00130          10  WS-PPK-COMPANY-CD       PIC X.
00131          10  WS-PPK-CARRIER          PIC X.
00132          10  WS-PPK-GROUPING         PIC X(6).
00133          10  WS-PPK-FIN-RESP         PIC X(10).
00134          10  WS-PPK-ACCOUNT          PIC X(10).
00135          10  WS-PPK-FILE-SEQ-NO      PIC S9(8)      COMP.
00136          10  WS-PPK-RECORD-TYPE      PIC X.
00137
00138      05  WS-CHECK-MAINT-KEY.
00139          10  WS-CMK-COMPANY-CD       PIC X.
00140          10  WS-CMK-CARRIER          PIC X.
00141          10  WS-CMK-GROUPING         PIC X(6).
00142          10  WS-CMK-STATE            PIC X(2).
00143          10  WS-CMK-ACCOUNT          PIC X(10).
00144          10  WS-CMK-CERT-EFF-DT      PIC X(2).
00145          10  WS-CMK-CERT-NO          PIC X(11).
00146          10  WS-CMK-SEQ-NO           PIC S9(4)      COMP.
00147
00148      05  WS-CHECK-QUEUE-KEY.
00149          10  WS-CQK-COMPANY-CD       PIC X.
00150          10  WS-CQK-CONTROL-NUMBER   PIC S9(8)
00151                                      COMP.
00152          10  WS-CQK-SEQUENCE-NUMBER  PIC S9(4)
00153                                      COMP.
00154
00155      05  WS-LAST-CHECK-QUEUE-KEY     PIC X(7) VALUE LOW-VALUE.
00156
00157      05  WS-COMPENSATION-MASTER-KEY.
00158          10  WS-CM-COMPANY-CD        PIC X.
00159          10  WS-CM-CARRIER           PIC X.
00160          10  WS-CM-GROUPING          PIC X(6).
00161          10  WS-CM-FIN-RESP          PIC X(10).
00162          10  WS-CM-ACCOUNT           PIC X(10).
00163          10  WS-CM-TYPE              PIC X.
00164
00165      EJECT
00166      05  WS-MAPSET-NAME              PIC X(8)      VALUE 'EL687S'.
00167      05  WS-MAP-NAME                 PIC X(8)      VALUE 'EL687A'.
00168
00169      05  FILLER                      REDEFINES
00170          WS-MAP-NAME.
00171          20  FILLER                  PIC XX.
00172          20  WS-MAP-NUMBER           PIC X(6).
00173
00174      05  WS-PROGRAM-ID               PIC X(8)      VALUE 'EL687'.
00175      05  WS-CHECK-QUEUE-DSID         PIC X(8)      VALUE 'ERCHKQ'.
00176      05  WS-ENQ-COMPANY-ID           PIC X(3)      VALUE  ZERO.
00177      05  WS-PENDING-PAYMENTS-DSID    PIC X(8)      VALUE 'ERPYAJ'.
00178      05  WS-CONTROL-FILE-DSID        PIC X(8)      VALUE 'ELCNTL'.
00179      05  WS-COMPENSATION-MASTER-DSID PIC X(8)      VALUE 'ERCOMP'.
00180      05  WS-CHECK-MAINT-DSID         PIC X(8)      VALUE 'ERCHEK'.
00181      05  WS-JOURNAL-TYPE-ID          PIC XX        VALUE 'ER'.
00182
00183      05  WS-LOW-VALUES               PIC X VALUE LOW-VALUES.
00184      05  WS-SPACES                   PIC X           VALUE SPACES.
00185
00186      05  WS-CURRENT-DATE             PIC XX VALUE LOW-VALUES.
00187
00188      05  WS-TRANS-ID                 PIC X(4)        VALUE 'EXG3'.
00189      05  WS-CHECK-WRITER-TRANS-ID    PIC X(4)        VALUE 'EXG4'.
00190
00191      05  WS-BENEFIT-NO               PIC XX          VALUE SPACES.
00192      05  WS-KIND.
00193          10  WS-RETRO-DAYS           PIC 99.
00194          10  WS-RETRO-ELIM           PIC X.
00195      05  WS-TEXT-MESSAGE-LENGTH      PIC S9(4)       VALUE +70
00196                                      COMP SYNC.
00197      05  WS-TEXT-MESSAGE             PIC X(70)       VALUE SPACES.
00198      05  WS-TEMP-STORAGE-ITEM        PIC S9(4)       VALUE ZERO
00199                                      COMP SYNC.
00200      05  WS-TEMP-STORAGE-KEY.
00201          10  WS-TSK-TERM-ID          PIC X(4)        VALUE SPACES.
00202          10  WS-TSK-TIME             PIC S9(7)       VALUE ZERO
00203                                      COMP-3.
00204      05  WS-MINUTES                  PIC S99         VALUE ZERO.
00205      05  WS-GREATEST-CHECK-NUMBER    PIC 9(7)        VALUE ZERO.
00206      05  WS-GREATEST-CHECK-NUMBER-X  REDEFINES
00207          WS-GREATEST-CHECK-NUMBER    PIC X(7).
00208      05  WS-CHECK-NUMBER             PIC 9(7)        VALUE ZERO.
00209      05  WS-CHECK-NUMBER-X           REDEFINES
00210          WS-CHECK-NUMBER             PIC X(7).
00211      05  WS-CHECK-NO.
00212          10  FILLER                  PIC X           VALUE ZERO.
00213          10  WS-CHECK-WORK           PIC X(6).
00214      05  WS-ACKNO                    PIC 9(7)        VALUE ZERO.
00215      05  WS-ACKNO-X                  REDEFINES
00216          WS-ACKNO                    PIC X(7).
00217      05  WS-PAYMENT-TYPE             PIC X           VALUE ZERO.
00218      05  WS-PAYEE-TYPE-CD            PIC X           VALUE ZERO.
00219
00220      05  WS-INIT-CONTROL-GROUP.
00221          10  FILLER              PIC X               VALUE SPACES.
00222          10  FILLER              PIC X(7) VALUE LOW-VALUES.
00223
00224      05  WS-SSN.
00225          10  WS-SSN-STATE            PIC XX.
00226          10  WS-SSN-ACCOUNT          PIC X(6).
00227          10  WS-SSN-LN3              PIC X(3).
00228
00229      05  WS-MEMBER-NUMBER.
00230          10  WS-MEMBER-STATE         PIC XX.
00231          10  WS-MEMBER-ACCOUNT       PIC X(6).
00232          10  WS-MEMBER-LN4           PIC X(4).
00233
00234      05  WS-OLD-CHECK-QUEUE-RECORD   PIC X(100)      VALUE SPACES.
00235      05  WS-NEW-CHECK-QUEUE-RECORD   PIC X(100)      VALUE SPACES.
00236
00237      EJECT
00238      05  WS-ERROR-NUMBERS.
00239          10  ER-0002                 PIC 9(4)        VALUE 0002.
00240          10  ER-0004                 PIC 9(4)        VALUE 0004.
00241          10  ER-0008                 PIC 9(4)        VALUE 0008.
00242          10  ER-0029                 PIC 9(4)        VALUE 0029.
00243          10  ER-0330                 PIC 9(4)        VALUE 0330.
00244          10  ER-0361                 PIC 9(4)        VALUE 0361.
00245          10  ER-0362                 PIC 9(4)        VALUE 0362.
00246          10  ER-0363                 PIC 9(4)        VALUE 0363.
00247          10  ER-0364                 PIC 9(4)        VALUE 0364.
00248          10  ER-0365                 PIC 9(4)        VALUE 0365.
00249          10  ER-0366                 PIC 9(4)        VALUE 0366.
00250          10  ER-0367                 PIC 9(4)        VALUE 0367.
00251          10  ER-0368                 PIC 9(4)        VALUE 0368.
00252          10  ER-0369                 PIC 9(4)        VALUE 0369.
00253          10  ER-0370                 PIC 9(4)        VALUE 0370.
00254          10  ER-0371                 PIC 9(4)        VALUE 0371.
00255          10  ER-0379                 PIC 9(4)        VALUE 0379.
00256          10  ER-0380                 PIC 9(4)        VALUE 0380.
00257          10  ER-0381                 PIC 9(4)        VALUE 0381.
00258          10  ER-0382                 PIC 9(4)        VALUE 0382.
00259          10  ER-0383                 PIC 9(4)        VALUE 0383.
00260          10  ER-0385                 PIC 9(4)        VALUE 0385.
00261          10  ER-0387                 PIC 9(4)        VALUE 0387.
00262          10  ER-0389                 PIC 9(4)        VALUE 0389.
00263          10  ER-0390                 PIC 9(4)        VALUE 0390.
00264          10  ER-0391                 PIC 9(4)        VALUE 0391.
00265          10  ER-0392                 PIC 9(4)        VALUE 0392.
00266          10  ER-0393                 PIC 9(4)        VALUE 0393.
00267          10  ER-0394                 PIC 9(4)        VALUE 0394.
00268          10  ER-0395                 PIC 9(4)        VALUE 0395.
00269          10  ER-0490                 PIC 9(4)        VALUE 0490.
00270          10  ER-3130                 PIC 9(4)        VALUE 3130.
00271
00272
00273      EJECT
00274 *                            COPY ELCINTF.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCINTF.                            *
00004 *                            VMOD=2.017                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = C.I.C.S. COMMON DATA AREA                 *
00007 *                                                                *
00008 *       LENGTH = 1024                                            *
00009 *                                                                *
00010 ******************************************************************
00011  01  PROGRAM-INTERFACE-BLOCK.
00012      12  PI-COMM-LENGTH                PIC S9(4) COMP VALUE +1024.
00013      12  PI-CALLING-PROGRAM              PIC X(8).
00014      12  PI-SAVED-PROGRAM-1              PIC X(8).
00015      12  PI-SAVED-PROGRAM-2              PIC X(8).
00016      12  PI-SAVED-PROGRAM-3              PIC X(8).
00017      12  PI-SAVED-PROGRAM-4              PIC X(8).
00018      12  PI-SAVED-PROGRAM-5              PIC X(8).
00019      12  PI-SAVED-PROGRAM-6              PIC X(8).
00020      12  PI-RETURN-TO-PROGRAM            PIC X(8).
00021      12  PI-COMPANY-ID                   PIC XXX.
00022      12  PI-COMPANY-CD                   PIC X.
00023
00024      12  PI-COMPANY-PASSWORD             PIC X(8).
00025
00026      12  PI-JOURNAL-FILE-ID              PIC S9(4) COMP.
00027
00028      12  PI-CONTROL-IN-PROGRESS.
00029          16  PI-CARRIER                  PIC X.
00030          16  PI-GROUPING                 PIC X(6).
00031          16  PI-STATE                    PIC XX.
00032          16  PI-ACCOUNT                  PIC X(10).
00033          16  PI-PRODUCER REDEFINES PI-ACCOUNT
00034                                          PIC X(10).
00035          16  PI-CLAIM-CERT-GRP.
00036              20  PI-CLAIM-NO             PIC X(7).
00037              20  PI-CERT-NO.
00038                  25  PI-CERT-PRIME       PIC X(10).
00039                  25  PI-CERT-SFX         PIC X.
00040              20  PI-CERT-EFF-DT          PIC XX.
00041          16  PI-PLAN-DATA REDEFINES PI-CLAIM-CERT-GRP.
00042              20  PI-PLAN-CODE            PIC X(2).
00043              20  PI-REVISION-NUMBER      PIC X(3).
00044              20  PI-PLAN-EFF-DT          PIC X(2).
00045              20  PI-PLAN-EXP-DT          PIC X(2).
00046              20  FILLER                  PIC X(11).
00047          16  PI-OE-REFERENCE-1 REDEFINES PI-CLAIM-CERT-GRP.
00048              20  PI-OE-REFERENCE-1.
00049                  25  PI-OE-REF-1-PRIME   PIC X(18).
00050                  25  PI-OE-REF-1-SUFF    PIC XX.
00051
00052      12  PI-SESSION-IN-PROGRESS          PIC X.
00053          88  CLAIM-SESSION                   VALUE '1'.
00054          88  CREDIT-SESSION                  VALUE '2'.
00055          88  WARRANTY-SESSION                VALUE '3'.
00056          88  MORTGAGE-SESSION                VALUE '4'.
00057          88  GENERAL-LEDGER-SESSION          VALUE '5'.
00058
00059
00060 *THE FOLLOWING TWO FIELDS ARE USED ONLY WITH MULTI COMPANY CLIENTS
00061
00062      12  PI-ORIGINAL-COMPANY-ID          PIC X(3).
00063      12  PI-ORIGINAL-COMPANY-CD          PIC X.
00064
00065      12  PI-CREDIT-USER                  PIC X.
00066          88  PI-NOT-CREDIT-USER              VALUE 'N'.
00067          88  PI-HAS-CLAS-IC-CREDIT           VALUE 'Y'.
00068
00069      12  PI-CLAIM-USER                   PIC X.
00070          88  PI-NOT-CLAIM-USER               VALUE 'N'.
00071          88  PI-HAS-CLAS-IC-CLAIM            VALUE 'Y'.
00072
00073      12  PI-PROCESSOR-SYS-ACCESS         PIC X.
00074          88  PI-ACCESS-TO-BOTH-SYSTEMS       VALUE ' '.
00075          88  PI-ACCESS-TO-ALL-SYSTEMS        VALUE ' '.
00076          88  PI-ACCESS-TO-CLAIM-ONLY         VALUE '1'.
00077          88  PI-ACCESS-TO-CREDIT-ONLY        VALUE '2'.
00078          88  PI-ACCESS-TO-MORTGAGE-ONLY      VALUE '3'.
00079
00080      12  PI-PROCESSOR-ID                 PIC X(4).
00081
00082      12  PI-PROCESSOR-PASSWORD           PIC X(11).
00083
00084      12  PI-MEMBER-CAPTION               PIC X(10).
00085
00086      12  PI-PROCESSOR-USER-ALMIGHTY      PIC X.
00087          88  PI-USER-ALMIGHTY-YES            VALUE 'Y'.
00088
00089      12  PI-LIFE-OVERRIDE-L1             PIC X.
00090      12  PI-LIFE-OVERRIDE-L2             PIC XX.
00091      12  PI-LIFE-OVERRIDE-L6             PIC X(6).
00092      12  PI-LIFE-OVERRIDE-L12            PIC X(12).
00093
00094      12  PI-AH-OVERRIDE-L1               PIC X.
00095      12  PI-AH-OVERRIDE-L2               PIC XX.
00096      12  PI-AH-OVERRIDE-L6               PIC X(6).
00097      12  PI-AH-OVERRIDE-L12              PIC X(12).
00098
00099      12  PI-NEW-SYSTEM                   PIC X(2).
00100
00101      12  PI-PRIMARY-CERT-NO              PIC X(11).
00102      12  PI-CLAIM-PAID-THRU-TO           PIC X(01).
00103          88  PI-USES-PAID-TO                 VALUE '1'.
00104      12  PI-CRDTCRD-SYSTEM.
00105          16  PI-CRDTCRD-USER             PIC X.
00106              88  PI-NOT-CRDTCRD-USER         VALUE 'N'.
00107              88  PI-HAS-CLAS-IC-CRDTCRD      VALUE 'Y'.
00108          16  PI-CC-MONTH-END-DT          PIC XX.
00109      12  PI-PROCESSOR-PRINTER            PIC X(4).
00110
00111      12  PI-OE-REFERENCE-2.
00112          16  PI-OE-REF-2-PRIME           PIC X(10).
00113          16  PI-OE-REF-2-SUFF            PIC X.
00114
00115      12  PI-REM-TRM-CALC-OPTION          PIC X.
00116
00117      12  PI-LANGUAGE-TYPE                PIC X.
00118              88  PI-LANGUAGE-IS-ENG          VALUE 'E'.
00119              88  PI-LANGUAGE-IS-FR           VALUE 'F'.
00120              88  PI-LANGUAGE-IS-SPAN         VALUE 'S'.
00121
00122      12  PI-POLICY-LINKAGE-IND           PIC X.
00123          88  PI-USE-POLICY-LINKAGE           VALUE 'Y'.
00124          88  PI-POLICY-LINKAGE-NOT-USED      VALUE 'N'
00125                                                    LOW-VALUES.
00126
00127      12  PI-ALT-DMD-PRT-ID               PIC X(4).
00128      12  PI-CLAIM-PW-SESSION             PIC X(1).
00129          88  PI-CLAIM-CREDIT                 VALUE '1'.
00130          88  PI-CLAIM-CONVEN                 VALUE '2'.
00131      12  FILLER                          PIC X(4).
00132
00133      12  PI-SYSTEM-LEVEL                 PIC X(145).
00134
00135      12  PI-CLAIMS-CREDIT-LEVEL          REDEFINES
00136          PI-SYSTEM-LEVEL.
00137
00138          16  PI-ENTRY-CODES.
00139              20  PI-ENTRY-CD-1           PIC X.
00140              20  PI-ENTRY-CD-2           PIC X.
00141
00142          16  PI-RETURN-CODES.
00143              20  PI-RETURN-CD-1          PIC X.
00144              20  PI-RETURN-CD-2          PIC X.
00145
00146          16  PI-UPDATE-STATUS-SAVE.
00147              20  PI-UPDATE-BY            PIC X(4).
00148              20  PI-UPDATE-HHMMSS        PIC S9(7)     COMP-3.
00149
00150          16  PI-LOWER-CASE-LETTERS       PIC X.
00151              88  LOWER-CASE-LETTERS-USED     VALUE 'Y'.
00152
00153 *        16  PI-CLAIM-ACCESS-CONTROL     PIC X.
00154 *            88  CLAIM-NO-UNIQUE             VALUE '1'.
00155 *            88  CARRIER-CLM-CNTL            VALUE '2'.
00156
00157          16  PI-CERT-ACCESS-CONTROL      PIC X.
00158              88  ST-ACCNT-CNTL               VALUE ' '.
00159              88  CARR-GROUP-ST-ACCNT-CNTL    VALUE '1'.
00160              88  CARR-ST-ACCNT-CNTL          VALUE '2'.
00161              88  ACCNT-CNTL                  VALUE '3'.
00162              88  CARR-ACCNT-CNTL             VALUE '4'.
00163
00164          16  PI-PROCESSOR-CAP-LIST.
00165              20  PI-SYSTEM-CONTROLS.
00166                 24 PI-SYSTEM-DISPLAY     PIC X.
00167                  88  SYSTEM-DISPLAY-CAP      VALUE 'Y'.
00168                 24 PI-SYSTEM-MODIFY      PIC X.
00169                  88  SYSTEM-MODIFY-CAP       VALUE 'Y'.
00170              20  FILLER                  PIC XX.
00171              20  PI-DISPLAY-CAP          PIC X.
00172                  88  DISPLAY-CAP             VALUE 'Y'.
00173              20  PI-MODIFY-CAP           PIC X.
00174                  88  MODIFY-CAP              VALUE 'Y'.
00175              20  PI-MSG-AT-LOGON-CAP     PIC X.
00176                  88  MSG-AT-LOGON-CAP        VALUE 'Y'.
00177              20  PI-FORCE-CAP            PIC X.
00178                  88  FORCE-CAP               VALUE 'Y'.
00179
00180          16  PI-PROGRAM-CONTROLS.
00181              20  PI-PGM-PRINT-OPT        PIC X.
00182              20  PI-PGM-FORMAT-OPT       PIC X.
00183              20  PI-PGM-PROCESS-OPT      PIC X.
00184              20  PI-PGM-TOTALS-OPT       PIC X.
00185
00186          16  PI-HELP-INTERFACE.
00187              20  PI-LAST-ERROR-NO        PIC X(4).
00188              20  PI-CURRENT-SCREEN-NO    PIC X(4).
00189
00190          16  PI-CARRIER-CONTROL-LEVEL    PIC X.
00191              88  CONTROL-IS-ACTUAL-CARRIER   VALUE SPACE.
00192
00193          16  PI-CR-CONTROL-IN-PROGRESS.
00194              20  PI-CR-CARRIER           PIC X.
00195              20  PI-CR-GROUPING          PIC X(6).
00196              20  PI-CR-STATE             PIC XX.
00197              20  PI-CR-ACCOUNT           PIC X(10).
00198              20  PI-CR-FIN-RESP          PIC X(10).
00199              20  PI-CR-TYPE              PIC X.
00200
00201          16  PI-CR-BATCH-NUMBER          PIC X(6).
00202
00203          16  PI-CR-MONTH-END-DT          PIC XX.
00204
00205          16  PI-CAR-GROUP-ACCESS-CNTL    PIC X.
00206              88  PI-USE-ACTUAL-CARRIER       VALUE ' '.
00207              88  PI-ZERO-CARRIER             VALUE '1'.
00208              88  PI-ZERO-GROUPING            VALUE '2'.
00209              88  PI-ZERO-CAR-GROUP           VALUE '3'.
00210
00211          16  PI-CARRIER-SECURITY         PIC X.
00212              88  PI-NO-CARRIER-SECURITY      VALUE ' '.
00213
00214          16  PI-ACCOUNT-SECURITY         PIC X(10).
00215              88  PI-NO-ACCOUNT-SECURITY      VALUE SPACES.
00216              88  PI-NO-PRODUCER-SECURITY     VALUE SPACES.
00217
00218          16  PI-CODE-SECURITY REDEFINES PI-ACCOUNT-SECURITY.
00219              20  PI-ACCESS-CODE          OCCURS 10 TIMES
00220                                          INDEXED BY PI-ACCESS-NDX
00221                                          PIC X.
00222
00223          16  PI-GA-BILLING-CONTROL       PIC X.
00224              88  PI-GA-BILLING               VALUE '1'.
00225
00226          16  PI-MAIL-PROCESSING          PIC X.
00227              88  PI-MAIL-YES                 VALUE 'Y'.
00228
00229          16  PI-SECURITY-TEMP-STORE-ID   PIC X(8).
00230
00231          16  PI-AR-SYSTEM.
00232              20  PI-AR-PROCESSING-CNTL   PIC X.
00233                  88  PI-AR-PROCESSING        VALUE 'Y'.
00234              20  PI-AR-SUMMARY-CODE      PIC X(6).
00235              20  PI-AR-MONTH-END-DT      PIC XX.
00236
00237          16  PI-MP-SYSTEM.
00238              20  PI-MORTGAGE-USER            PIC X.
00239                  88  PI-NOT-MORTGAGE-USER            VALUE 'N'.
00240                  88  PI-HAS-CLAS-IC-MORTGAGE         VALUE 'Y'.
00241              20  PI-MORTGAGE-ACCESS-CONTROL  PIC X.
00242                  88  PI-MP-ST-PROD-CNTL              VALUE ' '.
00243                  88  PI-MP-CARR-GRP-ST-PROD-CNTL     VALUE '1'.
00244                  88  PI-MP-CARR-ST-PROD-CNTL         VALUE '2'.
00245                  88  PI-MP-PROD-CNTL                 VALUE '3'.
00246                  88  PI-MP-CARR-PROD-CNTL            VALUE '4'.
00247              20  PI-MP-MONTH-END-DT          PIC XX.
00248              20  PI-MP-REFERENCE-NO.
00249                  24  PI-MP-REFERENCE-PRIME   PIC X(18).
00250                  24  PI-MP-REFERENCE-SFX     PIC XX.
00251
00252          16  PI-LABEL-CONTROL            PIC X(01).
00253              88  PI-CREATE-LABELS                    VALUE 'Y'.
00254              88  PI-BYPASS-LABELS                    VALUE 'N'.
00255
00256          16  PI-BILL-GROUPING-CODE       PIC X(01).
00257              88  PI-CO-HAS-BILL-GROUPING             VALUE 'Y'.
00258
00259          16  PI-RATE-DEV-AUTHORIZATION   PIC X(01).
00260              88  PI-RATE-DEV-AUTHORIZED              VALUE 'Y'.
00261              88  PI-RATE-DEV-NOT-AUTHORIZED          VALUE 'N'.
00262
00263          16  FILLER                      PIC X(14).
00264
00265      12  PI-PROGRAM-WORK-AREA            PIC X(640).
00266 ******************************************************************
00275 *                            COPY ERC687PI.
00001 *****************************************************************
00002 *                                                               *
00002 *                                                               *
00003 *                            ERC687PI                           *
00004 *                            VMOD=2.004                         *
00005 *                                                               *
00006 *****************************************************************.
00007
00008      12  FILLER                      REDEFINES
00009          PI-PROGRAM-WORK-AREA.
00010
00011          16  PI-TEMP-STORAGE-KEY.
00012              20  PI-TSK-TERM-ID      PIC X(4).
00013              20  PI-TSK-TIME         PIC S9(7)     COMP-3.
00014
00015          16  PI-PROCESSING-SW        PIC S9        COMP-3.
00016
00017          16  PI-NUMBER-OF-ALIGNMENT-CHECKS PIC S9  COMP-3.
00018
00019          16  PI-ALIGNMENT-CONTROL-GROUP  PIC S9(8) COMP.
00020
00021          16  PI-ALIGNMENT-SEQUENCE-NO  PIC S9(4)   COMP.
00022
00023          16  PI-NUMBER-OF-CONTROL-GROUPS  PIC S9(4) COMP.
00024
00025          16  PI-CONTROL-GROUPS       COMP
00026              OCCURS 4 TIMES          INDEXED BY PI-INDEX.
00027
00028              20  PI-CONTROL-GROUP    PIC S9(8).
00029              20  PI-HIGH-SEQUENCE    PIC S9(4).
00030
00031          16  PI-CHECK-PRINTER-ID     PIC X(4).
00032
00033          16  PI-PRINTER-STARTED-SW   PIC S9      COMP-3.
00034
00035          16  PI-ASSIGN-CHECK-NUMBERS PIC X.
00036
00037          16  PI-COMPANY-ADDRESS.
00038              20  PI-COMPANY-NAME             PIC X(30).
00039              20  PI-COMPANY-ADDRESS-LINE1    PIC X(30).
00040              20  PI-COMPANY-ADDRESS-LINE2    PIC X(30).
00041              20  PI-COMPANY-ADDRESS-LINE3    PIC X(30).
00042              20  PI-COMPANY-CITY-ST          PIC X(30).
00043              20  PI-COMPANY-ZIP-CODE.
00044                  24  PI-COMPANY-ZIP-PRIME.
00045                      26  PI-ZIP-PRI-1ST      PIC X.
00046                          88  PI-CO-CANADIAN-POST-CODE
00047                                           VALUE 'A' THRU 'Z'.
00048                      26  FILLER              PIC X(4).
00049                  24  PI-COMPANY-ZIP-PLUS4    PIC X(4).
00050              20  PI-CANADIAN-POSTAL-CODE REDEFINES
00051                  PI-COMPANY-ZIP-CODE.
00052                  24  PI-CAN-POSTAL-1         PIC XXX.
00053                  24  PI-CAN-POSTAL-2         PIC XXX.
00054                  24  FILLER                  PIC XXX.
00055              20  PI-COMPANY-PHONE-NUMBER     PIC S9(11) COMP-3.
00056          16  PI-SYSID                PIC X(4).
00057
00058          16  PI-TEMP-STORAGE-ITEM    PIC S9(4) COMP SYNC.
00059          16  PI-LETTERS-IND          PIC X.
00060
00061          16  FILLER                  PIC X(419).
00062
00276      EJECT
00277 *                            COPY ERCCPA.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCCPA                              *
00004 *                            VMOD=2.005                          *
00005 *                                                                *
00006 *   DESCRIPTION:  DATA TO BE PASSED TO CHECK WRITER ROUTINE.     *
00007 *   LENGTH: 700 CHARACTERS                                       *
00008 *                                                                *
00009 ******************************************************************
00010
00011  01  CHECK-PASS-AREA.
00012      12  CPA-ALIGNMENT               PIC S9(3)     COMP-3.
00013      12  CPA-CARRIER                 PIC X.
00014      12  CPA-GROUPING                PIC X(6).
00015      12  CPA-ACCOUNT                 PIC X(10).
00016      12  CPA-FIN-RESP                PIC X(10).
00017      12  CPA-STATE                   PIC XX.
00018      12  CPA-CERT-NO                 PIC X(11).
00019
00020      12  CPA-CAR-NAME                PIC X(30).
00021      12  CPA-CAR-ADDRESS-LINE1       PIC X(30).
00022      12  CPA-CAR-ADDRESS-LINE2       PIC X(30).
00023      12  CPA-CAR-ADDRESS-LINE3       PIC X(30).
00024      12  CPA-CAR-CITY-STATE          PIC X(30).
00025      12  CPA-CAR-ZIP-CODE            PIC S9(9)     COMP-3.
00026      12  CPA-PAYMENT-TYPE            PIC X.
00027      12  CPA-PAYMENT-BY              PIC X(4).
00028      12  CPA-CHECK-DATE              PIC X(2).
00029      12  CPA-CHECK-NUMBER            PIC X(7).
00030      12  CPA-AMOUNT-PAID             PIC S9(9)V99  COMP-3.
00031      12  CPA-AMOUNT-PAID-TO-DATE     PIC S9(9)V99  COMP-3.
00032      12  CPA-INCURRED-DT             PIC XX.
00033      12  CPA-REPORTED-DT             PIC XX.
00034      12  CPA-PAID-THRU-DT            PIC XX.
00035      12  CPA-PAID-FROM-DT            PIC XX.
00036      12  CPA-PAID-DT                 PIC XX.
00037
00038      12  CPA-PAYEE-NAME              PIC X(30).
00039      12  CPA-PAYEE-IN-CARE-OF        PIC X(30).
00040      12  CPA-PAYEE-ADDRESS-LINE1     PIC X(30).
00041      12  CPA-PAYEE-ADDRESS-LINE2     PIC X(30).
00042      12  CPA-PAYEE-CITY-ST           PIC X(30).
00043      12  CPA-PAYEE-ZIP-CODE.
00044          16  CPA-PAYEE-ZIP-PRIME.
00045              20  CPA-PAYEE-PRI-1ST   PIC X.
00046                  88  CPA-PAYEE-CAN-POST-CODE   VALUE 'A' THRU 'Z'.
00047              20  FILLER              PIC X(4).
00048          16  CPA-PAYEE-ZIP-PLUS4     PIC X(4).
00049      12  CPA-PAYEE-CAN-POSTAL-CODE  REDEFINES  CPA-PAYEE-ZIP-CODE.
00050          16  CPA-PAYEE-POST-1        PIC XXX.
00051          16  CPA-PAYEE-POST-2        PIC XXX.
00052          16  FILLER                  PIC XXX.
00053      12  CPA-PAYEE-PHONE-NO          PIC S9(11)   COMP-3.
00054
00055      12  CPA-SOC-SEC-NO              PIC X(11).
00056      12  CPA-MEMBER-NUMBER           PIC X(12).
00057      12  CPA-NO-OF-PMTS-MADE         PIC S9(3)    COMP-3.
00058      12  CPA-GENERAL-LEDGER          PIC X(7).
00059      12  CPA-CANC-DT                 PIC XX.
00060      12  CPA-LF-REFUND               PIC S9(7)V99  COMP-3.
00061      12  CPA-AH-REFUND               PIC S9(7)V99  COMP-3.
00062      12  CPA-INSURED-NAME            PIC X(28).
00063      12  CPA-CERT-EFF-DT             PIC XX.
00064      12  CPA-DEDUCT-WITHHELD         PIC S9(5)V99  COMP-3.
00065      12  CPA-ADDITIONAL-CHARGE       PIC S9(5)V99  COMP-3.
00066      12  FILLER                      PIC X(26).
00067      12  CPA-REST-OF-RECORD          PIC X(250).
00068
00069      12  CPA-CHECK-STUBS             REDEFINES
00070          CPA-REST-OF-RECORD.
00071          16  CPA-STUB1               PIC X(50).
00072          16  CPA-STUB2               PIC X(50).
00073          16  CPA-STUB3               PIC X(50).
00074          16  CPA-STUB4               PIC X(50).
00075          16  CPA-STUB5               PIC X(50).
00076
00278      EJECT
00279 *                            COPY ELCNWA.
00001 *****************************************************************
00002 *                                                               *
00002 *                                                               *
00003 *                            ELCNWA.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003                         *
00006 *                                                               *
00007 *            M O V E   N A M E   W O R K   A R E A.             *
00008 *                                                               *
00009 *****************************************************************.
00010
00011  01  WS-NAME-WORK-AREA.
00012      05  WS-INSURED-LAST-NAME        PIC X(15).
00013      05  WS-INSURED-1ST-NAME         PIC X(12).
00014      05  WS-INSURED-MID-INIT         PIC X.
00015
00016      05  WS-NAME-WORK.
00017          10  WS-NW                   PIC X
00018              OCCURS 30 TIMES INDEXED BY NWA-INDEX.
00019
00020      05  WS-NAME-WORK2.
00021          10  WS-NW2                  PIC X
00022              OCCURS 20 TIMES INDEXED BY NWA-INDEX2 NWA-INDEX3
00023                                         NWA-INDEX0.
00024
00025      05  WS-NAME-SW                  PIC S9          VALUE ZERO
00026                                      COMP-3.
00027
00280      EJECT
00281 *                            COPY EL687S.
       01  EL687AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  ADATEL PIC S9(0004) COMP.
           05  ADATEF PIC  X(0001).
           05  FILLER REDEFINES ADATEF.
               10  ADATEA PIC  X(0001).
           05  ADATEI PIC  X(0008).
      *    -------------------------------
           05  ATIMEL PIC S9(0004) COMP.
           05  ATIMEF PIC  X(0001).
           05  FILLER REDEFINES ATIMEF.
               10  ATIMEA PIC  X(0001).
           05  ATIMEI PIC  X(0005).
      *    -------------------------------
           05  AOPTIONL PIC S9(0004) COMP.
           05  AOPTIONF PIC  X(0001).
           05  FILLER REDEFINES AOPTIONF.
               10  AOPTIONA PIC  X(0001).
           05  AOPTIONI PIC  X(0001).
      *    -------------------------------
           05  ACG01L PIC S9(0004) COMP.
           05  ACG01F PIC  X(0001).
           05  FILLER REDEFINES ACG01F.
               10  ACG01A PIC  X(0001).
           05  ACG01I PIC  X(0007).
      *    -------------------------------
           05  ACG02L PIC S9(0004) COMP.
           05  ACG02F PIC  X(0001).
           05  FILLER REDEFINES ACG02F.
               10  ACG02A PIC  X(0001).
           05  ACG02I PIC  X(0007).
      *    -------------------------------
           05  ACG03L PIC S9(0004) COMP.
           05  ACG03F PIC  X(0001).
           05  FILLER REDEFINES ACG03F.
               10  ACG03A PIC  X(0001).
           05  ACG03I PIC  X(0007).
      *    -------------------------------
           05  ACG04L PIC S9(0004) COMP.
           05  ACG04F PIC  X(0001).
           05  FILLER REDEFINES ACG04F.
               10  ACG04A PIC  X(0001).
           05  ACG04I PIC  X(0007).
      *    -------------------------------
           05  AALIGNL PIC S9(0004) COMP.
           05  AALIGNF PIC  X(0001).
           05  FILLER REDEFINES AALIGNF.
               10  AALIGNA PIC  X(0001).
           05  AALIGNI PIC  X(0001).
      *    -------------------------------
           05  ACKNOL PIC S9(0004) COMP.
           05  ACKNOF PIC  X(0001).
           05  FILLER REDEFINES ACKNOF.
               10  ACKNOA PIC  X(0001).
           05  ACKNOI PIC  X(0007).
      *    -------------------------------
           05  AACNL PIC S9(0004) COMP.
           05  AACNF PIC  X(0001).
           05  FILLER REDEFINES AACNF.
               10  AACNA PIC  X(0001).
           05  AACNI PIC  X(0001).
      *    -------------------------------
           05  APRTL PIC S9(0004) COMP.
           05  APRTF PIC  X(0001).
           05  FILLER REDEFINES APRTF.
               10  APRTA PIC  X(0001).
           05  APRTI PIC  X(0004).
      *    -------------------------------
           05  AEMSG1L PIC S9(0004) COMP.
           05  AEMSG1F PIC  X(0001).
           05  FILLER REDEFINES AEMSG1F.
               10  AEMSG1A PIC  X(0001).
           05  AEMSG1I PIC  X(0079).
      *    -------------------------------
           05  AEMSG2L PIC S9(0004) COMP.
           05  AEMSG2F PIC  X(0001).
           05  FILLER REDEFINES AEMSG2F.
               10  AEMSG2A PIC  X(0001).
           05  AEMSG2I PIC  X(0079).
      *    -------------------------------
           05  AEMSG3L PIC S9(0004) COMP.
           05  AEMSG3F PIC  X(0001).
           05  FILLER REDEFINES AEMSG3F.
               10  AEMSG3A PIC  X(0001).
           05  AEMSG3I PIC  X(0079).
      *    -------------------------------
           05  APFKL PIC S9(0004) COMP.
           05  APFKF PIC  X(0001).
           05  FILLER REDEFINES APFKF.
               10  APFKA PIC  X(0001).
           05  APFKI PIC  9(2).
       01  EL687AO REDEFINES EL687AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AOPTIONO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACG01O PIC  9999999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACG02O PIC  9999999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACG03O PIC  9999999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACG04O PIC  9999999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AALIGNO PIC  9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACKNOO PIC  9999999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AACNO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APRTO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEMSG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEMSG2O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEMSG3O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APFKO PIC  99.
      *    -------------------------------
00282  01  FILLER                          REDEFINES
00283      EL687AI.
00284
00285      05  FILLER                      PIC X(35).
00286
00287      05  FILLER                      OCCURS 4 TIMES
00288                                      INDEXED BY EL687A-INDEX.
00289
00290          10  EL687A-CONTROL-GROUP-LENGTH   PIC S9(4)
00291                                            COMP.
00292          10  EL687A-CONTROL-GROUP-ATTRB    PIC X.
00293          10  EL687A-CONTROL-GROUP          PIC 9(7).
00294          10  EL687A-CONTROL-GROUP-X        REDEFINES
00295              EL687A-CONTROL-GROUP          PIC X(7).
00296
00297      EJECT
00298 *                            COPY ELCJPFX.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCJPFX.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.002                          *
00006 *                                                                *
00007 *    USER DATA FOR SYSTEM JOURNAL RECORDS  JOURNAL I.D. = "EL"   *
00008 *                                                                *
00009 *     ALL RECORDS ARE JOURNALED FOR ERROR RECOVERY               *
00010 *     FILES JOURNALED FOR AUDIT TRAIL (BEFORE CHANGE) ARE -      *
00011 *        ELCNTL - CONTROL FILE                                   *
00012 *        ELMSTR - CLAIM MASTERS                                  *
00013 *        ELTRLR - ACTIVITY TRAILERS                              *
00014 *        ELCHKQ - CHECK QUE                                      *
00015 ******************************************************************
00016  01  JOURNAL-RECORD.
00017      12  JP-USER-ID                  PIC X(4).
00018      12  JP-FILE-ID                  PIC X(8).
00019      12  JP-PROGRAM-ID               PIC X(8).
00020      12  JP-RECORD-TYPE              PIC X.
00021          88 JP-ADD              VALUE 'A'.
00022          88 JP-BEFORE-CHANGE    VALUE 'B'.
00023          88 JP-AFTER-CHANGE     VALUE 'C'.
00024          88 JP-DELETE           VALUE 'D'.
00025          88 JP-GENERIC-DELETE   VALUE 'G'.
00026          88 JP-KEY-CHG-DELETE   VALUE 'K'.
00027          88 JP-KEY-CHG-ADD      VALUE 'N'.
00028      12  JP-GENERIC-KEY-LENGTH       PIC S9(4)   COMP.
00029      12  JP-RECORD-AREA
00030
00031
00299                                      PIC X(750).
00300
00301      EJECT
00302 *                            COPY ELCEMIB.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCEMIB.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.005                          *
00006 *                                                                *
00007 *    STANDARD CLAS-IC ERROR MESSAGE COMMUNICATIONS AREA          *
00008 *                                                                *
00009 ******************************************************************
00010  01  ERROR-MESSAGE-INTERFACE-BLOCK.
00011      12  EMI-COMM-LENGTH         PIC S9(4)    VALUE +400 COMP.
00012      12  EMI-NUMBER-OF-LINES     PIC 9        VALUE 1.
00013      12  EMI-ERROR               PIC 9(4)     VALUE ZEROS.
00014      12  EMI-SUB                 PIC 99       VALUE 1 COMP-3.
00015      12  EMI-NOTE-CTR            PIC 999      VALUE 0 COMP-3.
00016      12  EMI-WARNING-CTR         PIC 999      VALUE 0 COMP-3.
00017      12  EMI-FORCABLE-CTR        PIC 999      VALUE 0 COMP-3.
00018      12  EMI-FATAL-CTR           PIC 999      VALUE 0 COMP-3.
00019      12  EMI-SWITCH1             PIC X        VALUE '1'.
00020          88  EMI-NO-ERRORS                    VALUE '1'.
00021          88  EMI-ERRORS-NOT-COMPLETE          VALUE '2'.
00022          88  EMI-ERRORS-COMPLETE              VALUE '3'.
00023      12  EMI-SWITCH2             PIC X        VALUE '1'.
00024          88  EMI-FORMAT-CODES-ONLY            VALUE '2'.
00025      12  EMI-SWITCH-AREA-1       PIC X        VALUE '1'.
00026          88  EMI-AREA1-EMPTY                  VALUE '1'.
00027          88  EMI-AREA1-FULL                   VALUE '2'.
00028      12  EMI-SWITCH-AREA-2       PIC X        VALUE '1'.
00029          88  EMI-AREA2-EMPTY                  VALUE '1'.
00030          88  EMI-AREA2-FULL                   VALUE '2'.
00031      12  EMI-ACTION-SWITCH       PIC X        VALUE ' '.
00032          88  EMI-PROCESS-ALL-ERRORS           VALUE ' '.
00033          88  EMI-BYPASS-NOTES                 VALUE 'N'.
00034          88  EMI-BYPASS-WARNINGS              VALUE 'W'.
00035          88  EMI-BYPASS-FORCABLES             VALUE 'F'.
00036          88  EMI-BYPASS-FATALS                VALUE 'X'.
00037      12  EMI-ERROR-LINES.
00038          16  EMI-LINE1           PIC X(72)   VALUE SPACES.
00039          16  EMI-LINE2           PIC X(72)   VALUE SPACES.
00040          16  EMI-LINE3           PIC X(72)   VALUE SPACES.
00041          16  EMI-CODE-LINE REDEFINES EMI-LINE3.
00042              20  EMI-ERR-CODES OCCURS 10 TIMES.
00043                  24  EMI-ERR-NUM         PIC X(4).
00044                  24  EMI-FILLER          PIC X.
00045                  24  EMI-SEV             PIC X.
00046                  24  FILLER              PIC X.
00047              20  FILLER                  PIC X(02).
00048      12  EMI-ERR-LINES REDEFINES EMI-ERROR-LINES.
00049          16  EMI-MESSAGE-AREA OCCURS 3 TIMES INDEXED BY EMI-INDX.
00050              20  EMI-ERROR-NUMBER    PIC X(4).
00051              20  EMI-FILL            PIC X.
00052              20  EMI-SEVERITY        PIC X.
00053              20  FILLER              PIC X.
00054              20  EMI-ERROR-TEXT.
00055                  24  EMI-TEXT-VARIABLE   PIC X(10).
00056                  24  FILLER          PIC X(55).
00057      12  EMI-SEVERITY-SAVE           PIC X.
00058          88  EMI-NOTE                    VALUE 'N'.
00059          88  EMI-WARNING                 VALUE 'W'.
00060          88  EMI-FORCABLE                VALUE 'F'.
00061          88  EMI-FATAL                   VALUE 'X'.
00062      12  EMI-MESSAGE-FLAG            PIC X.
00063          88  EMI-MESSAGE-FORMATTED       VALUE 'Y'.
00064          88  EMI-NO-MESSAGE-FORMATTED    VALUE 'N'.
00065      12  EMI-ROLL-SWITCH             PIC X       VALUE SPACES.
00066      12  EMI-LANGUAGE-IND            PIC X       VALUE SPACES.
00067          88  EMI-LANGUAGE-IS-FR                  VALUE 'F'.
00068          88  EMI-LANGUAGE-IS-ENG                 VALUE 'E'.
00069          88  EMI-LANGUAGE-IS-SPAN                VALUE 'S'.
00070      12  FILLER                      PIC X(137)  VALUE SPACES.
00071      12  EMI-DATE-FIELD              PIC X(06)   VALUE SPACES.
00072      12  EMI-CLIENT-ID               PIC X(3)    VALUE SPACES.
00073      12  EMI-LIFE-OVERRIDE-L6        PIC X(6).
00074      12  EMI-AH-OVERRIDE-L6          PIC X(6).
00303
00304      EJECT
00305 *                            COPY ELCDATE.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCDATE.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003
00006 *                                                                *
00007 *                                                                *
00008 *   DESCRIPTION:  DATA PASSED TO DATE CONVERSION ROUTINE.        *
00009 *                 LENGTH = 200                                   *
00010 ******************************************************************
00011
00012  01  DATE-CONVERSION-DATA.
00013      12  DC-COMM-LENGTH                PIC S9(4) COMP VALUE +200.
00014      12  DC-OPTION-CODE                PIC X.
00015          88  BIN-TO-GREG                VALUE ' '.
00016          88  ELAPSED-BETWEEN-BIN        VALUE '1'.
00017          88  EDIT-GREG-TO-BIN           VALUE '2'.
00018          88  YMD-GREG-TO-BIN            VALUE '3'.
00019          88  MDY-GREG-TO-BIN            VALUE '4'.
00020          88  JULIAN-TO-BIN              VALUE '5'.
00021          88  BIN-PLUS-ELAPSED           VALUE '6'.
00022          88  FIND-CENTURY               VALUE '7'.
00023          88  ELAPSED-BETWEEN-BIN-3      VALUE '8'.
00024          88  EDIT-GREG-TO-BIN-3         VALUE '9'.
00025          88  YMD-GREG-TO-BIN-3          VALUE 'A'.
00026          88  MDY-GREG-TO-BIN-3          VALUE 'B'.
00027          88  JULIAN-TO-BIN-3            VALUE 'C'.
00028          88  BIN-PLUS-ELAPSED-3         VALUE 'D'.
00029          88  JULIAN-EXPANDED-TO-BIN     VALUE 'E'.
00030          88  JULIAN-EXPANDED-TO-BIN-3   VALUE 'F'.
00031          88  BIN-TO-JULIAN-EXPANDED     VALUE 'G'.
00032          88  JULIAN-EXPANDED            VALUE 'E', 'F', 'G'.
00033          88  CHECK-LEAP-YEAR            VALUE 'H'.
00034          88  BIN-3-TO-GREG              VALUE 'I'.
00035          88  CYMD-GREG-TO-BIN-3         VALUE 'J'.
00036          88  MDCY-GREG-TO-BIN-3         VALUE 'K'.
00037          88  CYMD-GREG-TO-BIN           VALUE 'L'.
00038          88  MDCY-GREG-TO-BIN           VALUE 'M'.
00039          88  MDY-GREG-TO-JULIAN         VALUE 'N'.
00040          88  MDCY-GREG-TO-JULIAN        VALUE 'O'.
00041          88  YMD-GREG-TO-JULIAN         VALUE 'P'.
00042          88  CYMD-GREG-TO-JULIAN        VALUE 'Q'.
00043          88  THREE-CHARACTER-BIN
00044                   VALUES  '8' '9' 'A' 'B' 'C' 'D' 'I' 'J' 'K'.
00045          88  GREGORIAN-TO-BIN
00046                   VALUES '2' '3' '4' '9' 'A' 'B' 'J' 'K' 'L' 'M'.
00047          88  BIN-TO-GREGORIAN
00048                   VALUES ' ' '1' 'I' '8' 'G'.
00049          88  JULIAN-TO-BINARY
00050                   VALUES '5' 'C' 'E' 'F'.
00051      12  DC-ERROR-CODE                 PIC X.
00052          88  NO-CONVERSION-ERROR        VALUE ' '.
00053          88  DATE-CONVERSION-ERROR
00054                   VALUES '1' '2' '3' '4' '5' '9' 'A' 'B' 'C'.
00055          88  DATE-IS-ZERO               VALUE '1'.
00056          88  DATE-IS-NON-NUMERIC        VALUE '2'.
00057          88  DATE-IS-INVALID            VALUE '3'.
00058          88  DATE1-GREATER-DATE2        VALUE '4'.
00059          88  ELAPSED-PLUS-NEGATIVE      VALUE '5'.
00060          88  DATE-INVALID-OPTION        VALUE '9'.
00061          88  INVALID-CENTURY            VALUE 'A'.
00062          88  ONLY-CENTURY               VALUE 'B'.
00063          88  ONLY-LEAP-YEAR             VALUE 'C'.
00064          88  VALID-CENTURY-LEAP-YEAR    VALUE 'B', 'C'.
00065      12  DC-END-OF-MONTH               PIC X.
00066          88  CALCULATE-END-OF-MONTH     VALUE '1'.
00067      12  DC-CENTURY-ADJUSTMENT         PIC X   VALUE SPACES.
00068          88  USE-NORMAL-PROCESS         VALUE ' '.
00069          88  ADJUST-DOWN-100-YRS        VALUE '1'.
00070          88  ADJUST-UP-100-YRS          VALUE '2'.
00071      12  FILLER                        PIC X.
00072      12  DC-CONVERSION-DATES.
00073          16  DC-BIN-DATE-1             PIC XX.
00074          16  DC-BIN-DATE-2             PIC XX.
00075          16  DC-GREG-DATE-1-EDIT       PIC X(08).
00076          16  DC-GREG-DATE-1-EDIT-R REDEFINES
00077                        DC-GREG-DATE-1-EDIT.
00078              20  DC-EDIT1-MONTH        PIC 99.
00079              20  SLASH1-1              PIC X.
00080              20  DC-EDIT1-DAY          PIC 99.
00081              20  SLASH1-2              PIC X.
00082              20  DC-EDIT1-YEAR         PIC 99.
00083          16  DC-GREG-DATE-2-EDIT       PIC X(08).
00084          16  DC-GREG-DATE-2-EDIT-R REDEFINES
00085                      DC-GREG-DATE-2-EDIT.
00086              20  DC-EDIT2-MONTH        PIC 99.
00087              20  SLASH2-1              PIC X.
00088              20  DC-EDIT2-DAY          PIC 99.
00089              20  SLASH2-2              PIC X.
00090              20  DC-EDIT2-YEAR         PIC 99.
00091          16  DC-GREG-DATE-1-YMD        PIC 9(06).
00092          16  DC-GREG-DATE-1-YMD-R  REDEFINES
00093                      DC-GREG-DATE-1-YMD.
00094              20  DC-YMD-YEAR           PIC 99.
00095              20  DC-YMD-MONTH          PIC 99.
00096              20  DC-YMD-DAY            PIC 99.
00097          16  DC-GREG-DATE-1-MDY        PIC 9(06).
00098          16  DC-GREG-DATE-1-MDY-R REDEFINES
00099                       DC-GREG-DATE-1-MDY.
00100              20  DC-MDY-MONTH          PIC 99.
00101              20  DC-MDY-DAY            PIC 99.
00102              20  DC-MDY-YEAR           PIC 99.
00103          16  DC-GREG-DATE-1-ALPHA.
00104              20  DC-ALPHA-MONTH        PIC X(10).
00105              20  DC-ALPHA-DAY          PIC 99.
00106              20  FILLER                PIC XX.
00107              20  DC-ALPHA-CENTURY.
00108                  24 DC-ALPHA-CEN-N     PIC 99.
00109              20  DC-ALPHA-YEAR         PIC 99.
00110          16  DC-ELAPSED-MONTHS         PIC S9(4)     COMP.
00111          16  DC-ODD-DAYS-OVER          PIC S9(4)     COMP.
00112          16  DC-ELAPSED-DAYS           PIC S9(4)     COMP.
00113          16  DC-JULIAN-DATE            PIC 9(05).
00114          16  DC-JULIAN-YYDDD REDEFINES DC-JULIAN-DATE
00115                                        PIC 9(05).
00116          16  DC-JULIAN-DT REDEFINES DC-JULIAN-DATE.
00117              20  DC-JULIAN-YEAR        PIC 99.
00118              20  DC-JULIAN-DAYS        PIC 999.
00119          16  DC-DAYS-IN-MONTH          PIC S9(3)       COMP-3.
00120          16  DC-DAY-OF-WEEK            PIC S9  VALUE ZERO COMP-3.
00121          16  DC-DAY-OF-WEEK2           PIC S9  VALUE ZERO COMP-3.
00122      12  DATE-CONVERSION-VARIBLES.
00123          16  HOLD-CENTURY-1            PIC 9(11) VALUE 0.
00124          16  HOLD-CENTURY-1-SPLIT REDEFINES HOLD-CENTURY-1.
00125              20  FILLER                PIC 9(3).
00126              20  HOLD-CEN-1-CCYY.
00127                  24  HOLD-CEN-1-CC     PIC 99.
00128                  24  HOLD-CEN-1-YY     PIC 99.
00129              20  HOLD-CEN-1-MO         PIC 99.
00130              20  HOLD-CEN-1-DA         PIC 99.
00131          16  HOLD-CENTURY-1-R   REDEFINES HOLD-CENTURY-1.
00132              20  HOLD-CEN-1-R-MO       PIC 99.
00133              20  HOLD-CEN-1-R-DA       PIC 99.
00134              20  HOLD-CEN-1-R-CCYY.
00135                  24  HOLD-CEN-1-R-CC   PIC 99.
00136                  24  HOLD-CEN-1-R-YY   PIC 99.
00137              20  FILLER                PIC 9(3).
00138          16  HOLD-CENTURY-1-X.
00139              20  FILLER                PIC X(3)  VALUE SPACES.
00140              20  HOLD-CEN-1-X-CCYY.
00141                  24  HOLD-CEN-1-X-CC   PIC XX VALUE SPACES.
00142                  24  HOLD-CEN-1-X-YY   PIC XX VALUE SPACES.
00143              20  HOLD-CEN-1-X-MO       PIC XX VALUE SPACES.
00144              20  HOLD-CEN-1-X-DA       PIC XX VALUE SPACES.
00145          16  HOLD-CENTURY-1-R-X REDEFINES HOLD-CENTURY-1-X.
00146              20  HOLD-CEN-1-R-X-MO     PIC XX.
00147              20  HOLD-CEN-1-R-X-DA     PIC XX.
00148              20  HOLD-CEN-1-R-X-CCYY.
00149                  24  HOLD-CEN-1-R-X-CC PIC XX.
00150                  24  HOLD-CEN-1-R-X-YY PIC XX.
00151              20  FILLER                PIC XXX.
00152          16  DC-BIN-DATE-EXPAND-1      PIC XXX.
00153          16  DC-BIN-DATE-EXPAND-2      PIC XXX.
00154          16  DC-JULIAN-DATE-1          PIC 9(07).
00155          16  DC-JULIAN-DATE-1-R REDEFINES DC-JULIAN-DATE-1.
00156              20  DC-JULIAN-1-CCYY.
00157                  24  DC-JULIAN-1-CC    PIC 99.
00158                  24  DC-JULIAN-1-YR    PIC 99.
00159              20  DC-JULIAN-DA-1        PIC 999.
00160          16  DC-JULIAN-DATE-2          PIC 9(07).
00161          16  DC-JULIAN-DATE-2-R REDEFINES DC-JULIAN-DATE-2.
00162              20  DC-JULIAN-2-CCYY.
00163                  24  DC-JULIAN-2-CC    PIC 99.
00164                  24  DC-JULIAN-2-YR    PIC 99.
00165              20  DC-JULIAN-DA-2        PIC 999.
00166          16  DC-GREG-DATE-A-EDIT.
00167              20  DC-EDITA-MONTH        PIC 99.
00168              20  SLASHA-1              PIC X VALUE '/'.
00169              20  DC-EDITA-DAY          PIC 99.
00170              20  SLASHA-2              PIC X VALUE '/'.
00171              20  DC-EDITA-CCYY.
00172                  24  DC-EDITA-CENT     PIC 99.
00173                  24  DC-EDITA-YEAR     PIC 99.
00174          16  DC-GREG-DATE-B-EDIT.
00175              20  DC-EDITB-MONTH        PIC 99.
00176              20  SLASHB-1              PIC X VALUE '/'.
00177              20  DC-EDITB-DAY          PIC 99.
00178              20  SLASHB-2              PIC X VALUE '/'.
00179              20  DC-EDITB-CCYY.
00180                  24  DC-EDITB-CENT     PIC 99.
00181                  24  DC-EDITB-YEAR     PIC 99.
00182          16  DC-GREG-DATE-CYMD         PIC 9(08).
00183          16  DC-GREG-DATE-CYMD-R REDEFINES
00184                               DC-GREG-DATE-CYMD.
00185              20  DC-CYMD-CEN           PIC 99.
00186              20  DC-CYMD-YEAR          PIC 99.
00187              20  DC-CYMD-MONTH         PIC 99.
00188              20  DC-CYMD-DAY           PIC 99.
00189          16  DC-GREG-DATE-MDCY         PIC 9(08).
00190          16  DC-GREG-DATE-MDCY-R REDEFINES
00191                               DC-GREG-DATE-MDCY.
00192              20  DC-MDCY-MONTH         PIC 99.
00193              20  DC-MDCY-DAY           PIC 99.
00194              20  DC-MDCY-CEN           PIC 99.
00195              20  DC-MDCY-YEAR          PIC 99.
CIDMOD    12  DC-FORCE-EL310-DATE-SW         PIC X    VALUE SPACE.
CIDMOD        88  DC-FORCE-EL310-DATE                 VALUE 'Y'.
CIDMOD    12  DC-EL310-DATE                  PIC X(21).
CIDMOD    12  FILLER                         PIC X(28).
00306
00307      EJECT
00308 *                            COPY ELCLOGOF.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCLOGOF.                           *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *             STANDARD CLAS-IC LOGOFF TEXT AREA                  *
00007 *                                                                *
00008 ******************************************************************
00009  01  CLASIC-LOGOFF.
00010      12  LOGOFF-LENGTH       PIC S9(4)   VALUE +185   COMP.
00011      12  LOGOFF-TEXT.
00012          16  FILLER          PIC X(5)    VALUE SPACES.
00013          16  LOGOFF-MSG.
00014              20  LOGOFF-PGM  PIC X(8)    VALUE SPACES.
00015              20  FILLER      PIC X       VALUE SPACES.
00016              20  LOGOFF-FILL PIC X(66)   VALUE SPACES.
00017          16  FILLER          PIC X(80)
00018            VALUE '* YOU ARE NOW LOGGED OFF'.
00019          16  FILLER          PIC X(7)    VALUE '* LOGIC'.
00020          16  FILLER          PIC X       VALUE QUOTE.
00021          16  LOGOFF-SYS-MSG  PIC X(17)
00022            VALUE 'S CLAS-IC SYSTEM '.
00023      12  TEXT-MESSAGES.
00024          16  UNACCESS-MSG    PIC X(29)
00025              VALUE  'UNAUTHORIZED ACCESS ATTEMPTED'.
00026          16  PGMIDERR-MSG    PIC X(17)
00027              VALUE 'PROGRAM NOT FOUND'.
00309
00310      EJECT
00311 *                            COPY ELCATTR.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCATTR.                            *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *             LIST OF STANDARD ATTRIBUTE VALUES                  *
00007 *                                                                *
00008 *   THE DATA NAMES IN THIS COPY BOOK WERE ASSIGNED AS FOLLOWS:   *
00009 *                                                                *
00010 *                   POS 1   P=PROTECTED                          *
00011 *                           U=UNPROTECTED                        *
00012 *                           S=ASKIP                              *
00013 *                   POS 2   A=ALPHA/NUMERIC                      *
00014 *                           N=NUMERIC                            *
00015 *                   POS 3   N=NORMAL                             *
00016 *                           B=BRIGHT                             *
00017 *                           D=DARK                               *
00018 *                   POS 4-5 ON=MODIFIED DATA TAG ON              *
00019 *                           OF=MODIFIED DATA TAG OFF             *
00020 *                                                                *
CIDMOD*  NO  CID  MODS  IN  COPYBOOK  ELCATTR                          *
00021 ******************************************************************
00022  01  ATTRIBUTE-LIST.
00023      12  AL-PABOF            PIC X       VALUE 'Y'.
00024      12  AL-PABON            PIC X       VALUE 'Z'.
00025      12  AL-PADOF            PIC X       VALUE '%'.
00026      12  AL-PADON            PIC X       VALUE '_'.
00027      12  AL-PANOF            PIC X       VALUE '-'.
00028      12  AL-PANON            PIC X       VALUE '/'.
00029      12  AL-SABOF            PIC X       VALUE '8'.
00030      12  AL-SABON            PIC X       VALUE '9'.
00031      12  AL-SADOF            PIC X       VALUE '@'.
00032      12  AL-SADON            PIC X       VALUE QUOTE.
00033      12  AL-SANOF            PIC X       VALUE '0'.
00034      12  AL-SANON            PIC X       VALUE '1'.
00035      12  AL-UABOF            PIC X       VALUE 'H'.
00036      12  AL-UABON            PIC X       VALUE 'I'.
00037      12  AL-UADOF            PIC X       VALUE '<'.
00038      12  AL-UADON            PIC X       VALUE '('.
00039      12  AL-UANOF            PIC X       VALUE ' '.
00040      12  AL-UANON            PIC X       VALUE 'A'.
00041      12  AL-UNBOF            PIC X       VALUE 'Q'.
00042      12  AL-UNBON            PIC X       VALUE 'R'.
00043      12  AL-UNDOF            PIC X       VALUE '*'.
00044      12  AL-UNDON            PIC X       VALUE ')'.
00045      12  AL-UNNOF            PIC X       VALUE '&'.
00046      12  AL-UNNON            PIC X       VALUE 'J'.
00312
00313      EJECT
00314 *                            COPY ELCAID.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCAID.                             *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   DESCRIPTION:  ATTENTION IDENTIFER CHARACTERS.                *
CIDMOD*                                                                *
CIDMOD*  NO  CID  MODS  IN  COPYBOOK  ELCAID                           *
051007*  051007  2007041300002 Change PF22 from x'D5' to x'5B'
00007 ******************************************************************
00008
00009  01  DFHAID.
00010    02  DFHNULL   PIC  X  VALUE  ' '.
00011    02  DFHENTER  PIC  X  VALUE  QUOTE.
00012    02  DFHCLEAR  PIC  X  VALUE  '_'.
00013    02  DFHPEN    PIC  X  VALUE  '='.
00014    02  DFHOPID   PIC  X  VALUE  'W'.
00015    02  DFHPA1    PIC  X  VALUE  '%'.
00016    02  DFHPA2    PIC  X  VALUE  '>'.
00017    02  DFHPA3    PIC  X  VALUE  ','.
00018    02  DFHPF1    PIC  X  VALUE  '1'.
00019    02  DFHPF2    PIC  X  VALUE  '2'.
00020    02  DFHPF3    PIC  X  VALUE  '3'.
00021    02  DFHPF4    PIC  X  VALUE  '4'.
00022    02  DFHPF5    PIC  X  VALUE  '5'.
00023    02  DFHPF6    PIC  X  VALUE  '6'.
00024    02  DFHPF7    PIC  X  VALUE  '7'.
00025    02  DFHPF8    PIC  X  VALUE  '8'.
00026    02  DFHPF9    PIC  X  VALUE  '9'.
00027    02  DFHPF10   PIC  X  VALUE  ':'.
00028    02  DFHPF11   PIC  X  VALUE  '#'.
00029    02  DFHPF12   PIC  X  VALUE  '@'.
00030    02  DFHPF13   PIC  X  VALUE  'A'.
00031    02  DFHPF14   PIC  X  VALUE  'B'.
00032    02  DFHPF15   PIC  X  VALUE  'C'.
00033    02  DFHPF16   PIC  X  VALUE  'D'.
00034    02  DFHPF17   PIC  X  VALUE  'E'.
00035    02  DFHPF18   PIC  X  VALUE  'F'.
00036    02  DFHPF19   PIC  X  VALUE  'G'.
00037    02  DFHPF20   PIC  X  VALUE  'H'.
00038    02  DFHPF21   PIC  X  VALUE  'I'.
051007*00039    02  DFHPF22   PIC  X  VALUE  '?'.
051007   02  DFHPF22   PIC  X  VALUE  '['.
00040    02  DFHPF23   PIC  X  VALUE  '.'.
00041    02  DFHPF24   PIC  X  VALUE  '<'.
00042    02  DFHMSRE   PIC  X  VALUE  'X'.
00043    02  DFHSTRF   PIC  X  VALUE  'h'.
00044    02  DFHTRIG   PIC  X  VALUE  '"'.
00315
00316  01  FILLER                      REDEFINES
00317      DFHAID.
00318
00319      05  FILLER                      PIC X(8).
00320
00321      05  PF-VALUES                   PIC X
00322          OCCURS 24 TIMES.
00323
00324      EJECT
      ****************************************************************
      *                                                               
      * Copyright (c) 2007 by Clerity Solutions, Inc.                 
      * All rights reserved.                                          
      *                                                               
      ****************************************************************
       01  DFHEIV.                                                    
         02  DFHEIV0               PIC X(35).                         
         02  DFHEIV1               PIC X(08).                         
         02  DFHEIV2               PIC X(08).                         
         02  DFHEIV3               PIC X(08).                         
         02  DFHEIV4               PIC X(06).                         
         02  DFHEIV5               PIC X(04).                         
         02  DFHEIV6               PIC X(04).                         
         02  DFHEIV7               PIC X(02).                         
         02  DFHEIV8               PIC X(02).                         
         02  DFHEIV9               PIC X(01).                         
         02  DFHEIV10              PIC S9(7) COMP-3.                  
         02  DFHEIV11              PIC S9(4) COMP SYNC.               
         02  DFHEIV12              PIC S9(4) COMP SYNC.               
         02  DFHEIV13              PIC S9(4) COMP SYNC.               
         02  DFHEIV14              PIC S9(4) COMP SYNC.               
         02  DFHEIV15              PIC S9(4) COMP SYNC.               
         02  DFHEIV16              PIC S9(9) COMP SYNC.               
         02  DFHEIV17              PIC X(04).                         
         02  DFHEIV18              PIC X(04).                         
         02  DFHEIV19              PIC X(04).                         
         02  DFHEIV20              USAGE IS POINTER.                  
         02  DFHEIV21              USAGE IS POINTER.                  
         02  DFHEIV22              USAGE IS POINTER.                  
         02  DFHEIV23              USAGE IS POINTER.                  
         02  DFHEIV24              USAGE IS POINTER.                  
         02  DFHEIV25              PIC S9(9) COMP SYNC.               
         02  DFHEIV26              PIC S9(9) COMP SYNC.               
         02  DFHEIV27              PIC S9(9) COMP SYNC.               
         02  DFHEIV28              PIC S9(9) COMP SYNC.               
         02  DFHEIV97              PIC S9(7) COMP-3 VALUE ZERO.       
         02  DFHEIV98              PIC S9(4) COMP SYNC VALUE ZERO.    
         02  FILLER                PIC X(02).                         
         02  DFHEIV99              PIC X(08) VALUE SPACE.             
         02  DFHEIVL0              PIC X(48) VALUE SPACE.             
         02  DFHEIVL1              PIC X(48) VALUE SPACE.             
         02  DFHEIVL2              PIC X(48) VALUE SPACE.             
         02  DFHEIVL3              PIC X(48) VALUE SPACE.             
       LINKAGE  SECTION.
      *****************************************************************
      *                                                               *
      * Copyright (c) 2007 by Clerity Solutions, Inc.                 *
      * All rights reserved.                                          *
      *                                                               *
      *****************************************************************
       01  dfheiblk.
           02  eibtime          pic s9(7) comp-3.
           02  eibdate          pic s9(7) comp-3.
           02  eibtrnid         pic x(4).
           02  eibtaskn         pic s9(7) comp-3.
           02  eibtrmid         pic x(4).
           02  dfheigdi         pic s9(4) comp.
           02  eibcposn         pic s9(4) comp.
           02  eibcalen         pic s9(4) comp.
           02  eibaid           pic x(1).
           02  eibfiller1       pic x(1).
           02  eibfn            pic x(2).
           02  eibfiller2       pic x(2).
           02  eibrcode         pic x(6).
           02  eibfiller3       pic x(2).
           02  eibds            pic x(8).
           02  eibreqid         pic x(8).
           02  eibrsrce         pic x(8).
           02  eibsync          pic x(1).
           02  eibfree          pic x(1).
           02  eibrecv          pic x(1).
           02  eibsend          pic x(1).
           02  eibatt           pic x(1).
           02  eibeoc           pic x(1).
           02  eibfmh           pic x(1).
           02  eibcompl         pic x(1).
           02  eibsig           pic x(1).
           02  eibconf          pic x(1).
           02  eiberr           pic x(1).
           02  eibrldbk         pic x(1).
           02  eiberrcd         pic x(4).
           02  eibsynrb         pic x(1).
           02  eibnodat         pic x(1).
           02  eibfiller5       pic x(2).
           02  eibresp          pic 9(09) comp.
           02  eibresp2         pic 9(09) comp.
           02  dfheigdj         pic s9(4) comp.
           02  dfheigdk         pic s9(4) comp.
00326
00327  01  DFHCOMMAREA                     PIC X(1024).
00328
00329 *01 DFHBLLDS                         COMP SYNC.
00330 *    05  BLLCBAR                     PIC S9(9).
00331 *    05  CQFCBAR                     PIC S9(9).
00332 *    05  CFCBAR                      PIC S9(9).
00333 *    05  PPFCBAR                     PIC S9(9).
00334 *    05  CMFCBAR                     PIC S9(9).
00335 *    05  CHFCBAR                     PIC S9(9).
00336
00337      EJECT
00338 *                            COPY ERCCHKQ.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCCHKQ                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.005                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = CHECK QUE FILE FOR THE CREDIT SYSTEM      *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 100  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ERCHKQ                         RKP=2,LEN=7    *
00013 *       ALTERNATE PATH  = NONE                                   *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00017 ******************************************************************
00018  01  CHECK-QUE.
00019      12  CQ-RECORD-ID                PIC XX.
00020          88  VALID-CQ-ID                     VALUE 'CQ'.
00021
00022      12  CQ-CONTROL-PRIMARY.
00023          16  CQ-COMPANY-CD           PIC X.
00024          16  CQ-CONTROL-NUMBER       PIC S9(8)       COMP.
00025          16  CQ-SEQUENCE-NUMBER      PIC S9(4)       COMP.
00026
00027      12  CQ-ENTRY-TYPE               PIC X.
00028              88  CHECK-ON-QUE           VALUE 'Q'.
00029              88  ALIGNMENT-CHECK        VALUE 'A'.
00030              88  MANUAL-CHECK           VALUE 'M'.
00031              88  SPOILED-CHECK          VALUE 'S'.
00032              88  VOIDED-CHECK           VALUE 'V'.
00033              88  PAYMENT-ABORTED        VALUE 'X'.
00034
00035      12  CQ-CREDIT-MASTER-CNTL       PIC X(50).
00036
00037      12  CQ-CREDIT-PYAJ-CNTL         REDEFINES
00038          CQ-CREDIT-MASTER-CNTL.
00039          16  CQ-PYAJ-CARRIER         PIC X.
00040          16  CQ-PYAJ-GROUPING        PIC X(6).
00041          16  CQ-PYAJ-FIN-RESP        PIC X(10).
00042          16  CQ-PYAJ-ACCOUNT         PIC X(10).
00043          16  CQ-PYAJ-SEQ             PIC S9(8)  COMP.
00044          16  CQ-PYAJ-REC-TYPE        PIC X.
00045          16  FILLER                  PIC X(18).
00046
00047      12  CQ-CREDIT-CHEK-CNTL         REDEFINES
00048          CQ-CREDIT-MASTER-CNTL.
00049          16  CQ-CHEK-CARRIER         PIC X.
00050          16  CQ-CHEK-GROUPING        PIC X(6).
00051          16  CQ-CHEK-STATE           PIC XX.
00052          16  CQ-CHEK-ACCOUNT         PIC X(10).
00053          16  CQ-CHEK-CERT-EFF-DT     PIC XX.
00054          16  CQ-CHEK-CERT-NO.
00055              20  CQ-CHEK-CERT-PRIME  PIC X(10).
00056              20  CQ-CHEK-CERT-SFX    PIC X.
00057          16  CQ-CHEK-SEQ-NO          PIC S9(4)       COMP.
00058          16  CQ-CHEK-FIN-RESP        PIC X(10).
00059          16  FILLER                  PIC X(06).
00060
00061      12  CQ-CHECK-NUMBER             PIC X(7).
00062      12  CQ-CHECK-AMOUNT             PIC S9(7)V99    COMP-3.
00063      12  CQ-PAYMENT-TYPE             PIC X.
00064              88  CQ-BILLING-CREDIT         VALUE '1'.
00065              88  CQ-REFUND-PMT             VALUE '2'.
00066              88  CQ-CHECK-MAINT-PMT        VALUE '3'.
00067      12  CQ-VOID-INDICATOR           PIC X.
00068              88  CHECK-IS-VOID             VALUE 'V'.
00069      12  CQ-TIMES-PRINTED            PIC S9(4)       COMP.
00070      12  CQ-PRINT-AT-HHMM            PIC S9(4)       COMP.
00071      12  CQ-CHECK-BY-USER            PIC X(4).
00072      12  CQ-PRE-NUMBERING-SW         PIC X.
00073        88  CHECKS-WERE-NOT-PRE-NUMBERED    VALUE SPACE.
00074        88  CHECKS-WERE-PRE-NUMBERED        VALUE '1'.
00075
00076      12  CQ-CHECK-WRITTEN-DT         PIC XX.
00077      12  CQ-LAST-UPDATED-BY          PIC S9(4)       COMP.
00078      12  CQ-ACCOUNT-AGENT            PIC X(10).
00079      12  CQ-CHECK-VOIDED-DT          PIC XX.
00080
00081      12  CQ-LETTERS-IND              PIC X.
00082          88  CQ-LETTERS-REQUIRED           VALUE 'Y'.
00083
00084 ******************************************************************
00339
00340      EJECT
00341 *                            COPY ELCCNTL.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCCNTL.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.059                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = SYSTEM CONTROL FILE                       *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 750  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELCNTL                        RKP=2,LEN=10    *
00013 *       ALTERNATE INDEX = NONE                                   *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00017 ******************************************************************
082503*                   C H A N G E   L O G
082503*
082503* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
082503*-----------------------------------------------------------------
082503*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
082503* EFFECTIVE    NUMBER
082503*-----------------------------------------------------------------
082503* 082503                   PEMA  ADD BENEFIT GROUP
100703* 100703    2003080800002  PEMA  ADD SUPERGAP PROCESSING
033104* 033104    2003080800002  PEMA  ADD GAP NON REFUNDABLE OPTION
092705* 092705    2005050300006  PEMA  ADD SPP LEASES
031808* 031808    2006032200004  AJRA  ADD APPROVAL LEVEL 4
071508* 071508  CR2007110500003  PEMA  ADD NH INTEREST REFUND PROCESSING
091808* 091808    2008022800002  AJRA  ADD CHECK NUMBER TO STATE CNTL FO
011410* 011410    2009061500002  AJRA  ADD REFUND IND FOR AH AND DEATH C
061511* 061511    2011042000002  AJRA  ADD IND TO VERIFY 2ND BENEFICIARY
082503******************************************************************
00018 *
00019  01  CONTROL-FILE.
00020      12  CF-RECORD-ID                       PIC XX.
00021          88  VALID-CF-ID                        VALUE 'CF'.
00022
00023      12  CF-CONTROL-PRIMARY.
00024          16  CF-COMPANY-ID                  PIC XXX.
00025          16  CF-RECORD-TYPE                 PIC X.
00026              88  CF-COMPANY-MASTER              VALUE '1'.
00027              88  CF-PROCESSOR-MASTER            VALUE '2'.
00028              88  CF-STATE-MASTER                VALUE '3'.
00029              88  CF-LF-BENEFIT-MASTER           VALUE '4'.
00030              88  CF-AH-BENEFIT-MASTER           VALUE '5'.
00031              88  CF-CARRIER-MASTER              VALUE '6'.
00032              88  CF-MORTALITY-MASTER            VALUE '7'.
00033              88  CF-BUSINESS-TYPE-MASTER        VALUE '8'.
00034              88  CF-TERMINAL-MASTER             VALUE '9'.
00035              88  CF-AH-EDIT-MASTER              VALUE 'A'.
00036              88  CF-CREDIBILITY-FACTOR-MASTER   VALUE 'B'.
00037              88  CF-CUSTOM-REPORT-MASTER        VALUE 'C'.
00038              88  CF-MORTGAGE-HT-WT-CHART        VALUE 'H'.
00039              88  CF-LIFE-EDIT-MASTER            VALUE 'L'.
00040              88  CF-MORTGAGE-PLAN-MASTER        VALUE 'M'.
00041              88  CF-MORTGAGE-COMPANY-MASTER     VALUE 'N'.
00042              88  CF-REMINDERS-MASTER            VALUE 'R'.
00043              88  CF-AUTO-ACTIVITY-MASTER        VALUE 'T'.
00044          16  CF-ACCESS-CD-GENL              PIC X(4).
00045          16  CF-ACCESS-OF-PROCESSOR  REDEFINES CF-ACCESS-CD-GENL.
00046              20  CF-PROCESSOR               PIC X(4).
00047          16  CF-ACCESS-OF-STATE  REDEFINES  CF-ACCESS-CD-GENL.
00048              20  CF-STATE-CODE              PIC XX.
00049              20  FILLER                     PIC XX.
00050          16  CF-ACCESS-OF-BENEFIT  REDEFINES  CF-ACCESS-CD-GENL.
00051              20  FILLER                     PIC XX.
00052              20  CF-HI-BEN-IN-REC           PIC XX.
00053          16  CF-ACCESS-OF-CARRIER  REDEFINES  CF-ACCESS-CD-GENL.
00054              20  FILLER                     PIC XXX.
00055              20  CF-CARRIER-CNTL            PIC X.
00056          16  CF-ACCESS-OF-BUS-TYPE REDEFINES  CF-ACCESS-CD-GENL.
00057              20  FILLER                     PIC XX.
00058              20  CF-HI-TYPE-IN-REC          PIC 99.
00059          16  CF-ACCESS-OF-CRDB-TBL REDEFINES  CF-ACCESS-CD-GENL.
00060              20  CF-CRDB-TABLE-INDICATOR    PIC X.
00061                  88  CF-CRDB-NAIC-TABLE         VALUE '9'.
00062              20  CF-CRDB-BENEFIT-TYPE       PIC X.
00063              20  CF-CRDB-WAITING-PERIOD     PIC XX.
00064          16  CF-ACCESS-OF-CUST-RPT REDEFINES  CF-ACCESS-CD-GENL.
00065              20  FILLER                     PIC X.
00066              20  CF-CUSTOM-REPORT-NO        PIC 999.
00067          16  CF-ACCESS-OF-PLAN   REDEFINES  CF-ACCESS-CD-GENL.
00068              20  FILLER                     PIC XX.
00069              20  CF-MORTGAGE-PLAN           PIC XX.
00070          16  CF-SEQUENCE-NO                 PIC S9(4)   COMP.
00071
00072      12  CF-LAST-MAINT-DT                   PIC XX.
00073      12  CF-LAST-MAINT-BY                   PIC X(4).
00074      12  CF-LAST-MAINT-HHMMSS               PIC S9(6)   COMP-3.
00075
00076      12  CF-RECORD-BODY                     PIC X(728).
00077
00078
00079 ****************************************************************
00080 *             COMPANY MASTER RECORD                            *
00081 ****************************************************************
00082
00083      12  CF-COMPANY-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00084          16  CF-COMPANY-ADDRESS.
00085              20  CF-CL-MAIL-TO-NAME         PIC X(30).
00086              20  CF-CL-IN-CARE-OF           PIC X(30).
00087              20  CF-CL-ADDR-LINE-1          PIC X(30).
00088              20  CF-CL-ADDR-LINE-2          PIC X(30).
00089              20  CF-CL-CITY-STATE           PIC X(30).
00090              20  CF-CL-ZIP-CODE-NUM         PIC 9(9)    COMP-3.
00091              20  CF-CL-PHONE-NO             PIC 9(11)   COMP-3.
00092          16  CF-COMPANY-CD                  PIC X.
00093          16  CF-COMPANY-PASSWORD            PIC X(8).
00094          16  CF-SECURITY-OPTION             PIC X.
00095              88  ALL-SECURITY                   VALUE '1'.
00096              88  COMPANY-VERIFY                 VALUE '2'.
00097              88  PROCESSOR-VERIFY               VALUE '3'.
00098              88  NO-SECURITY                    VALUE '4'.
00099              88  ALL-BUT-TERM                   VALUE '5'.
00100          16  CF-CARRIER-CONTROL-LEVEL       PIC X.
00101              88  USE-ACTUAL-CARRIER             VALUE SPACE.
00102          16  CF-LGX-INTERFACE-CNTL          PIC X.
00103              88  LGX-TIME-SHR-COMPANY           VALUE '1'.
00104          16  CF-INFORCE-LOCATION            PIC X.
00105              88  CERTS-ARE-ONLINE               VALUE '1'.
00106              88  CERTS-ARE-OFFLINE              VALUE '2'.
00107              88  NO-CERTS-AVAILABLE             VALUE '3'.
00108          16  CF-LOWER-CASE-LETTERS          PIC X.
00109          16  CF-CERT-ACCESS-CONTROL         PIC X.
00110              88  CF-ST-ACCNT-CNTL               VALUE ' '.
00111              88  CF-CARR-GROUP-ST-ACCNT-CNTL    VALUE '1'.
00112              88  CF-CARR-ST-ACCNT-CNTL          VALUE '2'.
00113              88  CF-ACCNT-CNTL                  VALUE '3'.
00114              88  CF-CARR-ACCNT-CNTL             VALUE '4'.
00115
00116          16  CF-FORMS-PRINTER-ID            PIC X(4).
00117          16  CF-CHECK-PRINTER-ID            PIC X(4).
00118
00119          16  CF-LGX-CREDIT-USER             PIC X.
00120              88  CO-IS-NOT-USER                 VALUE 'N'.
00121              88  CO-HAS-CLAS-IC-CREDIT          VALUE 'Y'.
00122
00123          16 CF-CREDIT-CALC-CODES.
00124              20  CF-CR-REM-TERM-CALC PIC X.
00125                88  CR-EARN-AFTER-15TH           VALUE '1'.
00126                88  CR-EARN-ON-HALF-MO           VALUE '2'.
00127                88  CR-EARN-ON-1ST-DAY           VALUE '3'.
00128                88  CR-EARN-ON-FULL-MO           VALUE '4'.
00129                88  CR-EARN-WITH-NO-DAYS         VALUE '5'.
00130                88  CR-EARN-AFTER-14TH           VALUE '6'.
00131                88  CR-EARN-AFTER-16TH           VALUE '7'.
00132              20  CF-CR-R78-METHOD           PIC X.
00133                88  USE-TERM-PLUS-ONE            VALUE SPACE.
00134                88  DONT-USE-PLUS-ONE            VALUE '1'.
00135
00136          16  CF-CLAIM-CONTROL-COUNTS.
00137              20  CF-CO-CLAIM-COUNTER        PIC S9(8)   COMP.
00138                  88  CO-CLM-COUNT-RESET         VALUE +99999.
00139
00140              20  CF-CO-ARCHIVE-COUNTER      PIC S9(8)   COMP.
00141                  88  CO-ARCHIVE-COUNT-RESET     VALUE +999999.
00142
00143              20  CF-CO-CHECK-COUNTER        PIC S9(8)   COMP.
00144                  88  CO-CHECK-COUNT-RESET       VALUE +9999999.
00145
00146              20  CF-CO-CHECK-QUE-COUNTER    PIC S9(8)   COMP.
00147                  88  CO-QUE-COUNT-RESET         VALUE +9999999.
00148
00149          16  CF-CURRENT-MONTH-END           PIC XX.
00150
00151          16  CF-CO-CALC-QUOTE-TOLERANCE.
00152              20  CF-CO-TOL-CLAIM            PIC S999V99   COMP-3.
00153              20  CF-CO-TOL-PREM             PIC S999V99   COMP-3.
00154              20  CF-CO-TOL-REFUND           PIC S999V99   COMP-3.
00155              20  CF-CO-CLAIM-REJECT-SW      PIC X.
00156                  88 CO-WARN-IF-CLAIM-OUT        VALUE SPACE.
00157                  88 CO-FORCE-IF-CLAIM-OUT       VALUE '1'.
00158              20  CF-CO-PREM-REJECT-SW       PIC X.
00159                  88 CO-WARN-IF-PREM-OUT         VALUE SPACE.
00160                  88 CO-FORCE-IF-PREM-OUT        VALUE '1'.
00161              20  CF-CO-REF-REJECT-SW        PIC X.
00162                  88 CO-WARN-IF-REF-OUT          VALUE SPACE.
00163                  88 CO-FORCE-IF-REF-OUT         VALUE '1'.
00164
00165          16  CF-CO-REPORTING-DT             PIC XX.
00166          16  CF-CO-REPORTING-MONTH-DT       PIC XX.
00167          16  CF-CO-REPORTING-MONTH-END-SW   PIC X.
00168            88  CF-CO-NOT-MONTH-END              VALUE SPACES.
00169            88  CF-CO-MONTH-END                  VALUE '1'.
00170
00171          16  CF-LGX-CLAIM-USER              PIC X.
00172              88  CO-IS-NOT-CLAIM-USER           VALUE 'N'.
00173              88  CO-HAS-CLAS-IC-CLAIM           VALUE 'Y'.
00174
00175          16  CF-CREDIT-EDIT-CONTROLS.
00176              20  CF-MIN-PREMIUM             PIC S999V99   COMP-3.
00177              20  CF-MIN-AGE                 PIC 99.
00178              20  CF-DEFAULT-AGE             PIC 99.
00179              20  CF-MIN-TERM                PIC S999      COMP-3.
00180              20  CF-MAX-TERM                PIC S999      COMP-3.
00181              20  CF-DEFAULT-SEX             PIC X.
00182              20  CF-JOINT-AGE-INPUT         PIC X.
00183                  88 CF-JOINT-AGE-IS-INPUT       VALUE '1'.
00184              20  CF-BIRTH-DATE-INPUT        PIC X.
00185                  88 CF-BIRTH-DATE-IS-INPUT      VALUE '1'.
00186              20  CF-CAR-GROUP-ACCESS-CNTL   PIC X.
00187                  88  CF-USE-ACTUAL-CARRIER      VALUE ' '.
00188                  88  CF-ZERO-CARRIER            VALUE '1'.
00189                  88  CF-ZERO-GROUPING           VALUE '2'.
00190                  88  CF-ZERO-CAR-GROUP          VALUE '3'.
00191              20  CF-EDIT-SW                 PIC X.
00192                  88  CF-START-EDIT-TONIGHT      VALUE '1'.
00193              20  CF-EDIT-RESTART-BATCH      PIC X(6).
00194              20  CF-CR-PR-METHOD            PIC X.
00195                88  USE-NORMAL-PR-METHOD         VALUE SPACE.
00196                88  ADJUST-ORIG-TERM-BY-5        VALUE '1'.
00197              20  FILLER                     PIC X.
00198
00199          16  CF-CREDIT-MISC-CONTROLS.
00200              20  CF-REIN-TABLE-SW           PIC X.
00201                  88 REIN-TABLES-ARE-USED        VALUE '1'.
00202              20  CF-COMP-TABLE-SW           PIC X.
00203                  88 COMP-TABLES-ARE-USED        VALUE '1'.
00204              20  CF-EXPERIENCE-RETENTION-AGE
00205                                             PIC S9        COMP-3.
00206              20  CF-CONVERSION-DT           PIC XX.
00207              20  CF-COMP-WRITE-OFF-AMT      PIC S999V99   COMP-3.
00208              20  CF-RUN-FREQUENCY-SW        PIC X.
00209                  88 CO-IS-PROCESSED-MONTHLY     VALUE SPACE.
00210                  88 CO-IS-PROCESSED-ON-QTR      VALUE '1'.
00211
00212              20  CF-CR-CHECK-NO-CONTROL.
00213                  24  CF-CR-CHECK-NO-METHOD    PIC X.
00214                      88  CR-CHECK-NO-MANUAL       VALUE '1'.
00215                      88  CR-CHECK-NO-AUTO-SEQ     VALUE '2'.
00216                      88  CR-CHECK-NO-AT-PRINT     VALUE '4'.
00217                  24  CF-CR-CHECK-COUNTER      PIC S9(8)   COMP.
00218                      88  CR-CHECK-CNT-RESET-VALUE VALUE +999999.
00219
00220                  24  CF-CR-CHECK-COUNT       REDEFINES
00221                      CF-CR-CHECK-COUNTER      PIC X(4).
00222
00223                  24  CF-CR-CHECK-QUE-COUNTER  PIC S9(8)  COMP.
00224                      88  CR-QUE-COUNT-RESET      VALUE +9999999.
00225
00226                  24  CF-CR-CHECK-QUE-COUNT   REDEFINES
00227                      CF-CR-CHECK-QUE-COUNTER  PIC X(4).
00228                  24  CF-MAIL-PROCESSING       PIC X.
00229                      88  MAIL-PROCESSING          VALUE 'Y'.
00230
00231          16  CF-MISC-SYSTEM-CONTROL.
00232              20  CF-SYSTEM-C                 PIC X.
00233                  88  CONFIRMATION-SYS-USED       VALUE '1'.
00234              20  CF-SYSTEM-D                 PIC X.
00235                  88  DAILY-BILL-SYS-USED         VALUE '1'.
00236              20  CF-SOC-SEC-NO-SW            PIC X.
00237                  88  SOC-SEC-NO-USED             VALUE '1'.
00238              20  CF-MEMBER-NO-SW             PIC X.
00239                  88  MEMBER-NO-USED              VALUE '1'.
00240              20  CF-TAX-ID-NUMBER            PIC X(11).
00241              20  CF-JOURNAL-FILE-ID          PIC S9(4) COMP.
00242              20  CF-PAYMENT-APPROVAL-SW      PIC X.
00243                  88  CF-PMT-APPROVAL-USED        VALUE 'Y' 'G'.
00244                  88  CF-NO-APPROVAL              VALUE ' ' 'N'.
00245                  88  CF-ALL-APPROVED             VALUE 'Y'.
00246                  88  CF-GRADUATED-APPROVAL       VALUE 'G'.
00247              20  CF-SYSTEM-E                 PIC X.
00248                  88  CF-AR-SYSTEM-USED           VALUE 'Y'.
00249
00250          16  CF-LGX-LIFE-USER               PIC X.
00251              88  CO-IS-NOT-LIFE-USER            VALUE 'N'.
00252              88  CO-HAS-CLAS-IC-LIFE            VALUE 'Y'.
00253
00254          16  CF-CR-MONTH-END-DT             PIC XX.
00255
00256          16  CF-FILE-MAINT-DATES.
00257              20  CF-LAST-BATCH-NO           PIC S9(8)   COMP.
00258                  88  CF-LAST-BATCH-RESET        VALUE +999999.
00259              20  CF-LAST-BATCH       REDEFINES
00260                  CF-LAST-BATCH-NO               PIC X(4).
00261              20  CF-RATES-FILE-MAINT-DT         PIC XX.
00262              20  CF-RATES-FILE-CREATE-DT        PIC XX.
00263              20  CF-COMMISSION-TAB-MAINT-DT     PIC XX.
00264              20  CF-COMMISSION-TAB-CREATE-DT    PIC XX.
00265              20  CF-ACCOUNT-MSTR-MAINT-DT       PIC XX.
00266              20  CF-ACCOUNT-MSTR-CREATE-DT      PIC XX.
00267              20  CF-REINSURANCE-TAB-MAINT-DT    PIC XX.
00268              20  CF-REINSURANCE-TAB-CREATE-DT   PIC XX.
00269              20  CF-COMPENSATION-MSTR-MAINT-DT  PIC XX.
00270              20  CF-COMPENSATION-MSTR-CREATE-DT PIC XX.
00271
00272          16  CF-NEXT-COMPANY-ID             PIC XXX.
00273          16  FILLER                         PIC X.
00274
00275          16  CF-ALT-MORT-CODE               PIC X(4).
00276          16  CF-MEMBER-CAPTION              PIC X(10).
00277
00278          16  CF-LIFE-ACCESS-CONTROL         PIC X.
00279              88  CF-LIFE-ST-ACCNT-CNTL          VALUE ' '.
00280              88  CF-LIFE-CARR-GRP-ST-ACCNT-CNTL VALUE '1'.
00281              88  CF-LIFE-CARR-ST-ACCNT-CNTL     VALUE '2'.
00282              88  CF-LIFE-ACCNT-CNTL             VALUE '3'.
00283              88  CF-LIFE-CARR-ACCNT-CNTL        VALUE '4'.
00284
00285          16  CF-STARTING-ARCH-NO            PIC S9(8) COMP.
00286
00287          16  CF-LIFE-OVERRIDE-L1            PIC X.
00288          16  CF-LIFE-OVERRIDE-L2            PIC XX.
00289          16  CF-LIFE-OVERRIDE-L6            PIC X(6).
00290          16  CF-LIFE-OVERRIDE-L12           PIC X(12).
00291
00292          16  CF-AH-OVERRIDE-L1              PIC X.
00293          16  CF-AH-OVERRIDE-L2              PIC XX.
00294          16  CF-AH-OVERRIDE-L6              PIC X(6).
00295          16  CF-AH-OVERRIDE-L12             PIC X(12).
00296
00297          16  CF-REPORT-CD1-CAPTION          PIC X(10).
00298          16  CF-REPORT-CD2-CAPTION          PIC X(10).
00299
00300          16  CF-CLAIM-CUTOFF-DATE           PIC XX.
00301          16  CF-AR-LAST-EL860-DT            PIC XX.
00302          16  CF-MP-MONTH-END-DT             PIC XX.
00303
00304          16  CF-MAX-NUM-PMTS-CHECK          PIC 99.
00305          16  CF-CLAIM-PAID-THRU-TO          PIC X.
00306              88  CF-CLAIM-PAID-TO               VALUE '1'.
00307
00308          16  CF-AR-MONTH-END-DT             PIC XX.
00309
00310          16  CF-CRDTCRD-USER                PIC X.
00311              88  CO-IS-NOT-CRDTCRD-USER         VALUE 'N'.
00312              88  CO-HAS-CLAS-IC-CRDTCRD         VALUE 'Y'.
00313
00314          16  CF-CC-MONTH-END-DT             PIC XX.
00315
00316          16  CF-PRINT-ADDRESS-LABELS        PIC X.
00317
00318          16  CF-MORTALITY-AGE-CALC-METHOD   PIC X.
00319              88  CF-USE-TABLE-ASSIGNED-METHOD   VALUE '1' ' '.
00320              88  CF-USE-ALL-AGE-LAST            VALUE '2'.
00321              88  CF-USE-ALL-AGE-NEAR            VALUE '3'.
00322          16  CF-CO-TOL-PREM-PCT             PIC S9V9(4)   COMP-3.
00323          16  CF-CO-TOL-REFUND-PCT           PIC S9V9(4)   COMP-3.
00324          16  CF-CO-TOL-CAP                  PIC S9(3)V99  COMP-3.
00325          16  CF-CO-RESERVE-OPTION-SWITCH    PIC  X.
00326              88  OPTIONAL-RESERVE-METHOD-AUTH    VALUE 'Y'.
00327              88  OPTIONAL-RESERVE-MTHD-NOT-AUTH  VALUE ' ' 'N'.
00328          16  CF-CO-IBNR-LAG-MONTHS          PIC S9(3)     COMP-3.
00329          16  CF-CO-CIDA-TABLE-DISCOUNT-PCT  PIC S9V9(4)   COMP-3.
00330          16  CF-CO-CRDB-TABLE-SELECTION     PIC  X.
00331              88  NIAC-CREDIBILITY-TABLE          VALUE '9'.
00332          16  CF-CO-ST-CALL-RPT-CNTL         PIC  X.
00333
00334          16  CF-CL-ZIP-CODE.
00335              20  CF-CL-ZIP-PRIME.
00336                  24  CF-CL-ZIP-1ST          PIC X.
00337                      88  CF-CL-CAN-POST-CODE  VALUE 'A' THRU 'Z'.
00338                  24  FILLER                 PIC X(4).
00339              20  CF-CL-ZIP-PLUS4            PIC X(4).
00340          16  CF-CL-CANADIAN-POSTAL-CODE REDEFINES CF-CL-ZIP-CODE.
00341              20  CF-CL-CAN-POSTAL-1         PIC XXX.
00342              20  CF-CL-CAN-POSTAL-2         PIC XXX.
00343              20  FILLER                     PIC XXX.
00344
00345          16  CF-CO-CALCULATION-INTEREST     PIC S9V9(4)  COMP-3.
00346          16  CF-CO-IBNR-AH-FACTOR           PIC S9V9(4)  COMP-3.
00347          16  CF-CO-IBNR-LIFE-FACTOR         PIC S9V9(4)  COMP-3.
00348          16  CF-CO-OPTION-START-DATE        PIC XX.
00349          16  CF-REM-TRM-CALC-OPTION         PIC X.
00350            88  CF-VALID-REM-TRM-OPTION          VALUE '1' '2'
00351                                                       '3' '4'.
00352            88  CF-CONSIDER-EXTENSION            VALUE '3' '4'.
00353            88  CF-30-DAY-MONTH                  VALUE '1' '3'.
00354            88  CF-NO-EXT-30-DAY-MONTH           VALUE '1'.
00355            88  CF-NO-EXT-ACTUAL-DAYS            VALUE '2'.
00356            88  CF-EXT-30-DAY-MONTH              VALUE '3'.
00357            88  CF-EXT-ACTUAL-DAYS               VALUE '4'.
00358
00359          16  CF-DEFAULT-APR                 PIC S999V9(4) COMP-3.
00360
00361          16  CF-PAYMENT-APPROVAL-LEVELS.
00362              20  CF-LIFE-PAY-APP-LEVEL-1    PIC S9(7)   COMP-3.
00363              20  CF-LIFE-PAY-APP-LEVEL-2    PIC S9(7)   COMP-3.
00364              20  CF-LIFE-PAY-APP-LEVEL-3    PIC S9(7)   COMP-3.
00365              20  CF-AH-PAY-APP-LEVEL-1      PIC S9(7)   COMP-3.
00366              20  CF-AH-PAY-APP-LEVEL-2      PIC S9(7)   COMP-3.
00367              20  CF-AH-PAY-APP-LEVEL-3      PIC S9(7)   COMP-3.
00368
00369          16  CF-END-USER-REPORTING-USER     PIC X.
00370              88  CO-NO-END-USER-REPORTING       VALUE 'N'.
00371              88  CO-USES-END-USER-REPORTING     VALUE 'Y'.
00372
00373          16  CF-CLAIMS-CHECK-RECON-USER     PIC X.
00374              88  CO-NO-USE-CLAIMS-RECON         VALUE 'N'.
00375              88  CO-USES-CLAIMS-RECON           VALUE 'Y'.
00376
00377          16  CF-CLAIMS-LAST-PROCESS-DT      PIC XX.
00378
071508         16  CF-CREDIT-REF-SSN-CNT          PIC S9(5)  COMP-3.
00379          16  FILLER                         PIC X.
00380
00381          16  CF-CREDIT-ARCHIVE-CNTL.
00382              20  CF-CREDIT-LAST-ARCH-NUM    PIC S9(9)  COMP-3.
00383              20  CF-CREDIT-START-ARCH-NUM   PIC S9(9)  COMP-3.
00384              20  CF-CREDIT-ARCH-PURGE-YR    PIC S9     COMP-3.
00385
00386          16  CF-CR-PRINT-ADDRESS-LABELS     PIC X.
00387
00388          16  CF-CLAIMS-AUDIT-CHANGES        PIC X.
00389              88  CO-NO-USE-AUDIT-CHANGES        VALUE 'N'.
00390              88  CO-USES-AUDIT-CHANGES          VALUE 'Y'.
00391
00392          16  CF-CLAIMS-CREDIT-CARD-INDEX    PIC X.
00393              88  CO-NO-USE-CREDIT-CARD-INDEX    VALUE 'N'.
00394              88  CO-USES-CREDIT-CARD-INDEX      VALUE 'Y'.
00395
00396          16  CF-CLAIMS-LETTER-MAINT-DAYS    PIC 99.
00397
00398          16  CF-CO-ACH-ID-CODE              PIC  X.
00399              88  CF-CO-ACH-ICD-IRS-EIN          VALUE '1'.
00400              88  CF-CO-ACH-ICD-DUNS             VALUE '2'.
00401              88  CF-CO-ACH-ICD-USER-NO          VALUE '3'.
00402          16  CF-CO-ACH-CLAIM-SEND-NAME      PIC X(23).
00403          16  CF-CO-ACH-CLAIM-BK-NO          PIC X(09).
00404          16  CF-CO-ACH-ADMIN-SEND-NAME      PIC X(23).
00405          16  CF-CO-ACH-ADMIN-NO             PIC X(09).
00406          16  CF-CO-ACH-RECV-NAME            PIC X(23).
00407          16  CF-CO-ACH-RECV-NO              PIC X(08).
00408          16  CF-CO-ACH-ORIGINATOR-NO        PIC X(08).
00409          16  CF-CO-ACH-COMPANY-ID           PIC X(09).
00410          16  CF-CO-ACH-TRACE-NO             PIC 9(07) COMP.
00411                  88  CO-ACH-TRACE-NO-RESET      VALUE 9999999.
00412          16  CF-CO-ACH-TRACE-SPACE REDEFINES
00413                  CF-CO-ACH-TRACE-NO         PIC X(4).
00414
00415          16  CF-CO-OVER-SHORT.
00416              20 CF-CO-OVR-SHT-AMT           PIC S999V99   COMP-3.
00417              20 CF-CO-OVR-SHT-PCT           PIC S9V9(4)   COMP-3.
00418
031808*         16  FILLER                         PIC X(102).
031808         16  CF-PAYMENT-APPROVAL-LEVELS-2.
031808             20  CF-LIFE-PAY-APP-LEVEL-4    PIC S9(7)   COMP-3.
031808             20  CF-AH-PAY-APP-LEVEL-4      PIC S9(7)   COMP-3.
031808
031808         16  CF-AH-APPROVAL-DAYS.
031808             20  CF-AH-APP-DAY-LEVEL-1     PIC S9(5)   COMP-3.
031808             20  CF-AH-APP-DAY-LEVEL-2     PIC S9(5)   COMP-3.
031808             20  CF-AH-APP-DAY-LEVEL-3     PIC S9(5)   COMP-3.
031808             20  CF-AH-APP-DAY-LEVEL-4     PIC S9(5)   COMP-3.
031808
031808         16  FILLER                         PIC X(82).
00421 ****************************************************************
00422 *             PROCESSOR/USER RECORD                            *
00423 ****************************************************************
00424
00425      12  CF-PROCESSOR-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00426          16  CF-PROCESSOR-NAME              PIC X(30).
00427          16  CF-PROCESSOR-PASSWORD          PIC X(11).
00428          16  CF-PROCESSOR-TITLE             PIC X(26).
00429          16  CF-MESSAGE-AT-LOGON-CAP        PIC X.
00430                  88  MESSAGE-YES                VALUE 'Y'.
00431                  88  MESSAGE-NO                 VALUE ' ' 'N'.
00432
00433 *****************************************************
00434 ****  OCCURRENCE (1) CREDIT APPLICATIONS         ****
00435 ****  OCCURRENCE (2) CLAIMS APPLICATIONS         ****
00436 ****  OCCURRENCE (3) CREDIT CARD APPLICATIONS    ****
00437 ****  OCCURRENCE (4) ACCT RECV APPLICATIONS      ****
00438 *****************************************************
00439
00440          16  CF-SYSTEM-SECURITY  OCCURS  4 TIMES.
00441              20  CF-ADMINISTRATION-CONTROLS PIC XX.
00442              20  CF-APPLICATION-FORCE       PIC X.
00443              20  CF-INDIVIDUAL-APP.
00444                  24  CF-APP-SWITCHES  OCCURS  44 TIMES.
00445                      28  CF-BROWSE-APP      PIC X.
00446                      28  CF-UPDATE-APP      PIC X.
00447
00448          16  CF-CURRENT-TERM-ON             PIC X(4).
00449          16  CF-PROCESSOR-LIMITS-CLAIMS.
00450              20  CF-PROC-CALC-AMT-TOL       PIC S999V99   COMP-3.
00451              20  CF-PROC-MAX-REG-PMT        PIC S9(7)V99  COMP-3.
00452              20  CF-PROC-MAX-REG-DAYS       PIC S999      COMP-3.
00453              20  CF-PROC-MAX-AUTO-PMT       PIC S9(7)V99  COMP-3.
00454              20  CF-PROC-MAX-AUTO-MOS       PIC S999      COMP-3.
00455              20  CF-PROC-CALC-DAYS-TOL      PIC S999      COMP-3.
00456              20  CF-PROC-MAX-LF-PMT         PIC S9(7)V99  COMP-3.
00457          16  CF-PROCESSOR-CARRIER           PIC X.
00458              88  NO-CARRIER-SECURITY            VALUE ' '.
00459          16  CF-PROCESSOR-ACCOUNT           PIC X(10).
00460              88  NO-ACCOUNT-SECURITY            VALUE SPACES.
00461          16  CF-PROCESSOR-LIFE-ACCESS       PIC X.
00462              88  PROCESSOR-HAS-LIFE-ACCESS      VALUE 'Y'.
00463          16  CF-PROCESSOR-USER-ALMIGHTY     PIC X.
00464              88  PROCESSOR-USER-IS-ALMIGHTY     VALUE 'Y'.
00465
00466          16  CF-PROC-SYS-ACCESS-SW.
00467              20  CF-PROC-CREDIT-CLAIMS-SW.
00468                  24  CF-PROC-SYS-ACCESS-CREDIT  PIC X.
00469                      88  ACCESS-TO-CREDIT           VALUE 'Y'.
00470                  24  CF-PROC-SYS-ACCESS-CLAIMS  PIC X.
00471                      88  ACCESS-TO-CLAIMS           VALUE 'Y'.
00472              20  CF-PROC-CREDIT-CLAIMS   REDEFINES
00473                  CF-PROC-CREDIT-CLAIMS-SW       PIC XX.
00474                  88  ACCESS-TO-CLAIM-CREDIT         VALUE 'YY'.
00475              20  CF-PROC-LIFE-GNRLDGR-SW.
00476                  24  CF-PROC-SYS-ACCESS-LIFE    PIC X.
00477                      88  ACCESS-TO-LIFE             VALUE 'Y'.
00478                  24  CF-PROC-SYS-ACCESS-GNRLDGR PIC X.
00479                      88  ACCESS-TO-GNRLDGR          VALUE 'Y'.
00480              20  CF-PROC-LIFE-GNRLDGR    REDEFINES
00481                  CF-PROC-LIFE-GNRLDGR-SW        PIC XX.
00482                  88  ACCESS-TO-LIFE-GNRLDGR         VALUE 'YY'.
00483          16  CF-PROC-SYS-ACCESS-ALL      REDEFINES
00484              CF-PROC-SYS-ACCESS-SW              PIC X(4).
00485              88  ACCESS-TO-ALL-SYSTEMS              VALUE 'YYYY'.
00486          16  CF-PROCESSOR-PRINTER               PIC X(4).
00487
00488          16  CF-APPROVAL-LEVEL                  PIC X.
00489              88  APPROVAL-LEVEL-1                   VALUE '1'.
00490              88  APPROVAL-LEVEL-2                   VALUE '2'.
00491              88  APPROVAL-LEVEL-3                   VALUE '3'.
031808             88  APPROVAL-LEVEL-4                   VALUE '4'.
00492
00493          16  CF-PROC-MAX-EXP-PMT            PIC S9(7)V99  COMP-3.
00494
00495          16  CF-LANGUAGE-TYPE                   PIC X.
00496              88  CF-LANG-IS-ENG                     VALUE 'E'.
00497              88  CF-LANG-IS-FR                      VALUE 'F'.
00498          16  FILLER                             PIC  X(240).
00499
00500 ****************************************************************
00501 *             PROCESSOR/REMINDERS RECORD                       *
00502 ****************************************************************
00503
00504      12  CF-PROCESSOR-REMINDER-REC  REDEFINES  CF-RECORD-BODY.
00505          16  CF-PROCESSOR-REMINDERS  OCCURS 8 TIMES.
00506              20  CF-START-REMIND-DT         PIC XX.
00507              20  CF-END-REMIND-DT           PIC XX.
00508              20  CF-REMINDER-TEXT           PIC X(50).
00509          16  FILLER                         PIC X(296).
00510
00511
00512 ****************************************************************
00513 *             STATE MASTER RECORD                              *
00514 ****************************************************************
00515
00516      12  CF-STATE-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00517          16  CF-STATE-ABBREVIATION          PIC XX.
00518          16  CF-STATE-NAME                  PIC X(25).
00519          16  CF-ST-CALC-INTEREST            PIC S9V9(4)   COMP-3.
00520          16  CF-ST-CALC-QUOTE-TOLERANCE.
00521              20  CF-ST-TOL-CLAIM            PIC S999V99   COMP-3.
00522              20  CF-ST-TOL-PREM             PIC S999V99   COMP-3.
00523              20  CF-ST-TOL-REFUND           PIC S999V99   COMP-3.
00524              20  CF-ST-CLAIM-REJECT-SW      PIC X.
00525                  88 ST-WARN-IF-CLAIM-OUT        VALUE SPACE.
00526                  88 ST-FORCE-IF-CLAIM-OUT       VALUE '1'.
00527              20  CF-ST-PREM-REJECT-SW       PIC X.
00528                  88 ST-WARN-IF-PREM-OUT         VALUE SPACE.
00529                  88 ST-FORCE-IF-PREM-OUT        VALUE '1'.
00530              20  CF-ST-REF-REJECT-SW        PIC X.
00531                  88 ST-WARN-IF-REF-OUT          VALUE SPACE.
00532                  88 ST-FORCE-IF-REF-OUT         VALUE '1'.
00533          16  CF-ST-LF-EXP-PCT               PIC S999V9(4) COMP-3.
00534          16  CF-ST-AH-EXP-PCT               PIC S999V9(4) COMP-3.
00535          16  CF-ST-REFUND-RULES.
00536              20  CF-ST-REFUND-MIN           PIC S999V99    COMP-3.
00537              20  CF-ST-REFUND-DAYS-FIRST    PIC 99.
00538              20  CF-ST-REFUND-DAYS-SUBSEQ   PIC 99.
00539          16  CF-ST-FST-PMT-EXTENSION.
00540              20  CF-ST-FST-PMT-DAYS-MAX     PIC 999.
00541              20  CF-ST-FST-PMT-DAYS-CHG     PIC X.
00542                  88  CF-ST-EXT-NO-CHG           VALUE ' '.
00543                  88  CF-ST-EXT-CHG-LF           VALUE '1'.
00544                  88  CF-ST-EXT-CHG-AH           VALUE '2'.
00545                  88  CF-ST-EXT-CHG-LF-AH        VALUE '3'.
00546          16  CF-ST-STATE-CALL.
00547              20  CF-ST-CALL-UNEARNED        PIC X.
00548              20  CF-ST-CALL-RPT-CNTL        PIC X.
00549              20  CF-ST-CALL-RATE-DEV        PIC XXX.
00550          16  CF-REPLACEMENT-LAW-SW          PIC X.
00551              88  CF-REPLACEMENT-LAW-APPLIES     VALUE 'Y'.
00552              88  CF-REPL-LAW-NOT-APPLICABLE     VALUE 'N'.
00553          16  CF-REPLACEMENT-LETTER          PIC X(4).
00554          16  CF-ST-TOL-PREM-PCT             PIC S9V9999 COMP-3.
00555          16  CF-ST-TOL-REF-PCT              PIC S9V9999 COMP-3.
00556          16  CF-ST-TARGET-LOSS-RATIO        PIC S9V9(4) COMP-3.
00557          16  CF-ST-SPLIT-PAYMENT            PIC X.
00558          16  FILLER                         PIC X.
00559          16  CF-STATE-BENEFIT-CNTL  OCCURS 50 TIMES.
00560              20  CF-ST-BENEFIT-CD           PIC XX.
00561              20  CF-ST-BENEFIT-KIND         PIC X.
00562                  88  CF-ST-LIFE-KIND            VALUE 'L'.
00563                  88  CF-ST-AH-KIND              VALUE 'A'.
00564              20  CF-ST-REM-TERM-CALC        PIC X.
00565                  88  ST-REM-TERM-NOT-USED       VALUE SPACE.
00566                  88  ST-EARN-AFTER-15TH         VALUE '1'.
00567                  88  ST-EARN-ON-HALF-MO         VALUE '2'.
00568                  88  ST-EARN-ON-1ST-DAY         VALUE '3'.
00569                  88  ST-EARN-ON-FULL-MO         VALUE '4'.
00570                  88  ST-EARN-WITH-NO-DAYS       VALUE '5'.
00571                  88  ST-EARN-AFTER-14TH         VALUE '6'.
00572                  88  ST-EARN-AFTER-16TH         VALUE '7'.
00573
00574              20  CF-ST-REFUND-CALC          PIC X.
00575                  88  ST-REFUND-NOT-USED         VALUE SPACE.
00576                  88  ST-REFD-BY-R78             VALUE '1'.
00577                  88  ST-REFD-BY-PRO-RATA        VALUE '2'.
00578                  88  ST-REFD-AS-CALIF           VALUE '3'.
00579                  88  ST-REFD-AS-TEXAS           VALUE '4'.
00580                  88  ST-REFD-IS-NET-PAY         VALUE '5'.
00581                  88  ST-REFD-ANTICIPATION       VALUE '6'.
00582                  88  ST-REFD-UTAH               VALUE '7'.
00583                  88  ST-REFD-SUM-OF-DIGITS      VALUE '9'.
00584                  88  ST-REFD-REG-BALLOON        VALUE 'B'.
033104                 88  ST-REFD-GAP-NON-REFUND     VALUE 'G'.
00585
00586              20  CF-ST-EARNING-CALC         PIC X.
00587                  88  ST-EARNING-NOT-USED        VALUE SPACE.
00588                  88  ST-EARN-BY-R78             VALUE '1'.
00589                  88  ST-EARN-BY-PRO-RATA        VALUE '2'.
00590                  88  ST-EARN-AS-CALIF           VALUE '3'.
00591                  88  ST-EARN-AS-TEXAS           VALUE '4'.
00592                  88  ST-EARN-IS-NET-PAY         VALUE '5'.
00593                  88  ST-EARN-ANTICIPATION       VALUE '6'.
00594                  88  ST-EARN-MEAN               VALUE '8'.
00595                  88  ST-EARN-REG-BALLOON        VALUE 'B'.
00596
00597              20  CF-ST-OVRD-EARNINGS-CALC   PIC X.
00598                  88  ST-OVERRIDE-NOT-USED       VALUE SPACE.
00599                  88  ST-OVRD-BY-R78             VALUE '1'.
00600                  88  ST-OVRD-BY-PRO-RATA        VALUE '2'.
00601                  88  ST-OVRD-AS-CALIF           VALUE '3'.
00602                  88  ST-OVRD-AS-TEXAS           VALUE '4'.
00603                  88  ST-OVRD-IS-NET-PAY         VALUE '5'.
00604                  88  ST-OVRD-ANTICIPATION       VALUE '6'.
00605                  88  ST-OVRD-MEAN               VALUE '8'.
00606                  88  ST-OVRD-REG-BALLOON        VALUE 'B'.
00607              20  FILLER                     PIC X.
00608
00609          16  CF-ST-COMMISSION-CAPS.
00610              20  CF-ST-COMM-CAP-SL          PIC S9V9(4) COMP-3.
00611              20  CF-ST-COMM-CAP-JL          PIC S9V9(4) COMP-3.
00612              20  CF-ST-COMM-CAP-SA          PIC S9V9(4) COMP-3.
00613              20  CF-ST-COMM-CAP-JA          PIC S9V9(4) COMP-3.
00614          16  CF-COMM-CAP-LIMIT-TO           PIC X.
00615                  88  ST-LIMIT-TO-ACCOUNT        VALUE 'A'.
00616
00617          16  CF-ST-RES-TAX-PCT              PIC S9V9(4) COMP-3.
00618
00619          16  CF-ST-STATUTORY-INTEREST.
00620              20  CF-ST-STAT-DATE-FROM       PIC X.
00621                  88  ST-STAT-FROM-INCURRED      VALUE 'I'.
00622                  88  ST-STAT-FROM-REPORTED      VALUE 'R'.
00623              20  CF-ST-NO-DAYS-ELAPSED      PIC 99.
00624              20  CF-ST-STAT-INTEREST        PIC S9V9(4) COMP-3.
00625              20  CF-ST-STAT-INTEREST-1      PIC S9V9(4) COMP-3.
00626              20  CF-ST-STAT-INTEREST-2      PIC S9V9(4) COMP-3.
00627              20  CF-ST-STAT-INTEREST-3      PIC S9V9(4) COMP-3.
00628
00629          16  CF-ST-OVER-SHORT.
00630              20 CF-ST-OVR-SHT-AMT           PIC S999V99 COMP-3.
00631              20 CF-ST-OVR-SHT-PCT           PIC S9V9(4) COMP-3.
00632
00633          16  CF-ST-FREE-LOOK-PERIOD         PIC S9(3)   COMP-3.
00634
CIDMOD         16  CF-ST-RT-CALC                  PIC X.
CIDMOD
PEMMOD         16  CF-ST-LF-PREM-TAX              PIC S9V9(4) COMP-3.
PEMMOD         16  CF-ST-AH-PREM-TAX-I            PIC S9V9(4) COMP-3.
PEMMOD         16  CF-ST-AH-PREM-TAX-G            PIC S9V9(4) COMP-3.
PEMMOD         16  CF-ST-RF-LR-CALC               PIC X.
PEMMOD         16  CF-ST-RF-LL-CALC               PIC X.
PEMMOD         16  CF-ST-RF-LN-CALC               PIC X.
PEMMOD         16  CF-ST-RF-AH-CALC               PIC X.
PEMMOD         16  CF-ST-RF-CP-CALC               PIC X.
PEMMOD*        16  FILLER                         PIC X(206).
091808*CIDMOD         16  FILLER                         PIC X(192).
091808         16  CF-ST-CHECK-COUNTER            PIC S9(8)   COMP.
091808             88  CF-ST-CHECK-CNT-RESET      VALUE +9999999.
011410         16  CF-ST-REF-AH-DEATH-IND         PIC X.
061511         16  CF-ST-VFY-2ND-BENE             PIC X.
061511         16  FILLER                         PIC X(186).
00636
00637 ****************************************************************
00638 *             BENEFIT MASTER RECORD                            *
00639 ****************************************************************
00640
00641      12  CF-BENEFIT-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00642          16  CF-BENEFIT-CONTROLS  OCCURS 8 TIMES.
00643              20  CF-BENEFIT-CODE            PIC XX.
00644              20  CF-BENEFIT-NUMERIC  REDEFINES
00645                  CF-BENEFIT-CODE            PIC XX.
00646              20  CF-BENEFIT-ALPHA           PIC XXX.
00647              20  CF-BENEFIT-DESCRIP         PIC X(10).
00648              20  CF-BENEFIT-COMMENT         PIC X(10).
00649
00650              20  CF-LF-COVERAGE-TYPE        PIC X.
00651                  88  CF-REDUCING                VALUE 'R'.
00652                  88  CF-LEVEL                   VALUE 'L' 'P'.
00653
00654              20  CF-SPECIAL-CALC-CD         PIC X.
00655                  88  CF-ALTERNATE-NET-PAY       VALUE 'A'.
00656                  88  CF-NP-0-MO-INT             VALUE 'A'.
00657                  88  CF-OB-OFFLINE-RESERVED     VALUE 'B'.
00658                  88  CF-CRITICAL-PERIOD         VALUE 'C'.
00659                  88  CF-TERM-IN-DAYS            VALUE 'D'.
00660                  88  CF-USE-PREMIUM-AS-ENTERED  VALUE 'E'.
00661                  88  CF-FARM-PLAN               VALUE 'F'.
00662                  88  CF-RATE-AS-STANDARD        VALUE 'G'.
00663                  88  CF-2-MTH-INTEREST          VALUE 'I'.
00664                  88  CF-3-MTH-INTEREST          VALUE 'J'.
00665                  88  CF-4-MTH-INTEREST          VALUE 'K'.
00666                  88  CF-BALLOON-LAST-PMT        VALUE 'L'.
00667                  88  CF-MORTGAGE-PROCESSING     VALUE 'M'.
00668                  88  CF-PRUDENTIAL              VALUE 'P'.
00669                  88  CF-OUTSTANDING-BAL         VALUE 'O'.
00670                  88  CF-TRUNCATED-LIFE          VALUE 'T'.
00671                  88  CF-TRUNCATED-LIFE-ONE      VALUE 'U'.
00672                  88  CF-TRUNCATED-LIFE-TWO      VALUE 'V'.
00673                  88  CF-NET-PAY-SIMPLE          VALUE 'S'.
00674                  88  CF-SUMMARY-PROCESSING      VALUE 'Z'.
00675
00676              20  CF-JOINT-INDICATOR         PIC X.
00677                  88  CF-JOINT-COVERAGE          VALUE 'J'.
00678
082603*            20  FILLER                     PIC X(12).
082603             20  FILLER                     PIC X(11).
082503             20  CF-BENEFIT-CATEGORY        PIC X.
00680              20  CF-LOAN-TYPE               PIC X(8).
00681
00682              20  CF-CO-REM-TERM-CALC        PIC X.
00683                  88  CO-EARN-AFTER-15TH         VALUE '1'.
00684                  88  CO-EARN-ON-HALF-MO         VALUE '2'.
00685                  88  CO-EARN-ON-1ST-DAY         VALUE '3'.
00686                  88  CO-EARN-ON-FULL-MO         VALUE '4'.
00687                  88  CO-EARN-WITH-NO-DAYS       VALUE '5'.
00688
00689              20  CF-CO-EARNINGS-CALC        PIC X.
00690                  88  CO-EARN-BY-R78             VALUE '1'.
00691                  88  CO-EARN-BY-PRO-RATA        VALUE '2'.
00692                  88  CO-EARN-AS-CALIF           VALUE '3'.
00693                  88  CO-EARN-AS-TEXAS           VALUE '4'.
00694                  88  CO-EARN-IS-NET-PAY         VALUE '5'.
00695                  88  CO-EARN-ANTICIPATION       VALUE '6'.
00696                  88  CO-EARN-AS-MEAN            VALUE '8'.
00697                  88  CO-EARN-AS-REG-BALLOON     VALUE 'B'.
00698
00699              20  CF-CO-REFUND-CALC          PIC X.
00700                  88  CO-REFUND-NOT-USED         VALUE SPACE.
00701                  88  CO-REFD-BY-R78             VALUE '1'.
00702                  88  CO-REFD-BY-PRO-RATA        VALUE '2'.
00703                  88  CO-REFD-AS-CALIF           VALUE '3'.
00704                  88  CO-REFD-AS-TEXAS           VALUE '4'.
00705                  88  CO-REFD-IS-NET-PAY         VALUE '5'.
00706                  88  CO-REFD-ANTICIPATION       VALUE '6'.
00707                  88  CO-REFD-MEAN               VALUE '8'.
00708                  88  CO-REFD-SUM-OF-DIGITS      VALUE '9'.
00709                  88  CO-REFD-AS-REG-BALLOON     VALUE 'B'.
033104                 88  CO-REFD-GAP-NON-REFUND     VALUE 'G'.
00710
00711              20  CF-CO-OVRD-EARNINGS-CALC   PIC X.
00712                  88  CO-OVERRIDE-NOT-USED       VALUE SPACE.
00713                  88  CO-OVRD-BY-R78             VALUE '1'.
00714                  88  CO-OVRD-BY-PRO-RATA        VALUE '2'.
00715                  88  CO-OVRD-AS-CALIF           VALUE '3'.
00716                  88  CO-OVRD-AS-TEXAS           VALUE '4'.
00717                  88  CO-OVRD-IS-NET-PAY         VALUE '5'.
00718                  88  CO-OVRD-ANTICIPATION       VALUE '6'.
00719                  88  CO-OVRD-MEAN               VALUE '8'.
00720                  88  CO-OVRD-AS-REG-BALLOON     VALUE 'B'.
00721
00722              20  CF-CO-BEN-I-G-CD           PIC X.
00723                  88  CO-BEN-I-G-NOT-USED        VALUE SPACE.
00724                  88  CO-BEN-I-G-IS-INDV         VALUE 'I'.
00725                  88  CO-BEN-I-G-IS-GRP          VALUE 'G'.
00726
00727          16  FILLER                         PIC X(304).
00728
00729
00730 ****************************************************************
00731 *             CARRIER MASTER RECORD                            *
00732 ****************************************************************
00733
00734      12  CF-CARRIER-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00735          16  CF-ADDRESS-DATA.
00736              20  CF-MAIL-TO-NAME            PIC X(30).
00737              20  CF-IN-CARE-OF              PIC X(30).
00738              20  CF-ADDRESS-LINE-1          PIC X(30).
00739              20  CF-ADDRESS-LINE-2          PIC X(30).
00740              20  CF-CITY-STATE              PIC X(30).
00741              20  CF-ZIP-CODE-NUM            PIC 9(9)      COMP-3.
00742              20  CF-PHONE-NO                PIC 9(11)     COMP-3.
00743
00744          16  CF-CLAIM-NO-CONTROL.
00745              20  CF-CLAIM-NO-METHOD         PIC X.
00746                  88  CLAIM-NO-MANUAL            VALUE '1'.
00747                  88  CLAIM-NO-Y-M-SEQ           VALUE '2'.
00748                  88  CLAIM-NO-SEQ               VALUE '3'.
00749                  88  CLAIM-NO-ALPHA-SEQ         VALUE '5'.
00750              20  CF-CLAIM-COUNTER           PIC S9(8)   COMP.
00751                  88  CLAIM-CNT-RESET-IF-SEQ     VALUE +9999999.
00752                  88  CLAIM-CNT-RESET-IF-YRMO    VALUE +99999.
00753                  88  CLAIM-CNT-RESET-IF-YRALPHA VALUE +9999.
00754
00755          16  CF-CHECK-NO-CONTROL.
00756              20  CF-CHECK-NO-METHOD         PIC X.
00757                  88  CHECK-NO-MANUAL            VALUE '1'.
00758                  88  CHECK-NO-AUTO-SEQ          VALUE '2'.
00759                  88  CHECK-NO-CARR-SEQ          VALUE '3'.
00760                  88  CHECK-NO-AT-PRINT          VALUE '4'.
00761              20  CF-CHECK-COUNTER           PIC S9(8)   COMP.
00762                  88  CHECK-CNT-RESET-VALUE      VALUE +999999.
00763
00764          16  CF-DOMICILE-STATE              PIC XX.
00765
00766          16  CF-EXPENSE-CONTROLS.
00767              20  CF-EXPENSE-METHOD          PIC X.
00768                  88  EXPENSE-CALC-MANUAL        VALUE '1'.
00769                  88  DOLLARS-PER-PMT            VALUE '2'.
00770                  88  PERCENT-OF-PAYMENT         VALUE '3'.
00771                  88  DOLLARS-PER-MONTH          VALUE '4'.
00772              20  CF-EXPENSE-PERCENT         PIC S999V99   COMP-3.
00773              20  CF-EXPENSE-DOLLAR          PIC S999V99   COMP-3.
00774
00775          16  CF-CORRESPONDENCE-CONTROL.
00776              20  CF-LETTER-RESEND-OPT       PIC X.
00777                  88  LETTERS-NOT-ARCHIVED       VALUE SPACE.
00778                  88  LETTERS-ARE-ARCHIVED       VALUE '1'.
00779              20  FILLER                     PIC X(4).
00780
00781          16  CF-RESERVE-CONTROLS.
00782              20  CF-MANUAL-SW               PIC X.
00783                  88  CF-MANUAL-RESERVES-USED    VALUE '1'.
00784              20  CF-FUTURE-SW               PIC X.
00785                  88  CF-FUTURE-RESERVES-USED    VALUE '1'.
00786              20  CF-PTC-SW                  PIC X.
00787                  88  CF-PAY-TO-CURRENT-USED     VALUE '1'.
00788              20  CF-IBNR-SW                 PIC X.
00789                  88  CF-IBNR-RESERVES-USED      VALUE '1'.
00790              20  CF-PTC-LF-SW               PIC X.
00791                  88  CF-LF-PTC-USED             VALUE '1'.
00792              20  CF-CDT-ACCESS-METHOD       PIC X.
00793                  88  CF-CDT-ROUND-NEAR          VALUE '1'.
00794                  88  CF-CDT-ROUND-HIGH          VALUE '2'.
00795                  88  CF-CDT-INTERPOLATED        VALUE '3'.
00796              20  CF-PERCENT-OF-CDT          PIC S999V99   COMP-3.
00797
00798          16  CF-CLAIM-CALC-METHOD           PIC X.
00799              88  360-PLUS-MONTHS                VALUE '1'.
00800              88  365-PLUS-MONTHS                VALUE '2'.
00801              88  FULL-MONTHS-ACTUAL-DAY         VALUE '3'.
00802              88  360-DAILY                      VALUE '4'.
00803              88  365-DAILY                      VALUE '5'.
00804
00805          16  CF-LAST-ALPHA-CHARACTER        PIC X.
00806          16  FILLER                         PIC X(11).
00807
00808          16  CF-LIMIT-AMOUNTS.
00809              20  CF-CALC-AMT-TOL            PIC S999V99   COMP-3.
00810              20  CF-MAX-REG-PMT             PIC S9(7)V99  COMP-3.
00811              20  CF-MAX-REG-DAYS            PIC S999      COMP-3.
00812              20  CF-MAX-AUTO-PMT            PIC S9(7)V99  COMP-3.
00813              20  CF-MAX-AUTO-MOS            PIC S999      COMP-3.
00814              20  CF-CALC-DAYS-TOL           PIC S999      COMP-3.
00815              20  CF-CR-TOL-PREM             PIC S999V99   COMP-3.
00816              20  CF-CR-TOL-REFUND           PIC S999V99   COMP-3.
00817              20  CF-CR-TOL-PREM-PCT         PIC S9V9(4)   COMP-3.
00818              20  CF-CR-TOL-REFUND-PCT       PIC S9V9(4)   COMP-3.
00819
00820          16  CF-DAYS-BEFORE-CLOSED          PIC S999      COMP-3.
00821          16  CF-MONTHS-BEFORE-PURGED        PIC S999      COMP-3.
00822          16  CF-IBNR-PERCENT                PIC S9V9(4)   COMP-3.
00823
00824          16  CF-ZIP-CODE.
00825              20  CF-ZIP-PRIME.
00826                  24  CF-ZIP-1ST             PIC X.
00827                      88  CF-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'.
00828                  24  FILLER                 PIC X(4).
00829              20  CF-ZIP-PLUS4               PIC X(4).
00830          16  CF-CANADIAN-POSTAL-CODE REDEFINES CF-ZIP-CODE.
00831              20  CF-CAN-POSTAL-1            PIC XXX.
00832              20  CF-CAN-POSTAL-2            PIC XXX.
00833              20  FILLER                     PIC XXX.
00834
00835          16  CF-IBNR-UEPRM-PERCENT          PIC S9V9(4) COMP-3.
00836          16  CF-IBNR-R78-PERCENT            PIC S9V9(4) COMP-3.
00837          16  CF-IBNR-PRO-PERCENT            PIC S9V9(4) COMP-3.
00838
00839          16  CF-RATING-SWITCH               PIC X.
00840              88  CF-PERFORM-RATING              VALUE ' ' 'Y'.
00841              88  CF-NO-RATING                   VALUE 'N'.
00842
00843          16  CF-BUILD-RETRIEVE-AFTER-MONTHS PIC 99.
00844
00845          16  CF-CARRIER-OVER-SHORT.
00846              20 CF-CR-OVR-SHT-AMT           PIC S999V99   COMP-3.
00847              20 CF-CR-OVR-SHT-PCT           PIC S9V9(4)   COMP-3.
00848
100703         16  CF-CARRIER-CLP-TOL-PCT         PIC S9V9(4)   COMP-3.
100703         16  CF-SECPAY-SWITCH               PIC X.
100703             88  CF-SECURE-PAY-CARRIER          VALUE 'Y'.
100703             88  CF-NO-SECURE-PAY               VALUE ' ' 'N'.
092705         16  CF-CARRIER-LEASE-COMM          PIC S9(5)V99  COMP-3.
092705         16  FILLER                         PIC X(448).
100703*        16  FILLER                         PIC X(452).
00850
00851
00852 ****************************************************************
00853 *             MORTALITY MASTER RECORD                          *
00854 ****************************************************************
00855
00856      12  CF-MORTALITY-MASTER-REC REDEFINES  CF-RECORD-BODY.
00857          16  CF-MORT-TABLE-LINE OCCURS  9  TIMES
00858                                 INDEXED BY CF-MORT-NDX.
00859              20  CF-MORT-TABLE              PIC X(5).
00860              20  CF-MORT-TABLE-TYPE         PIC X.
00861                  88  CF-MORT-JOINT              VALUE 'J'.
00862                  88  CF-MORT-SINGLE             VALUE 'S'.
00863                  88  CF-MORT-COMBINED           VALUE 'C'.
00864                  88  CF-MORT-TYPE-VALID-C       VALUE 'J' 'S'.
00865                  88  CF-MORT-TYPE-VALID-M       VALUE 'J' 'S' 'C'.
00866              20  CF-MORT-INTEREST           PIC SV9(4)  COMP-3.
00867              20  CF-MORT-AGE-METHOD         PIC XX.
00868                  88  CF-AGE-LAST                VALUE 'AL'.
00869                  88  CF-AGE-NEAR                VALUE 'AN'.
00870              20  CF-MORT-RESERVE-ADJUSTMENT PIC S9V9(4) COMP-3.
00871              20  CF-MORT-ADJUSTMENT-DIRECTION
00872                                             PIC X.
00873                  88  CF-MINUS                   VALUE '-'.
00874                  88  CF-PLUS                    VALUE '+'.
00875              20  CF-MORT-JOINT-FACTOR       PIC S9V9(4) COMP-3.
00876              20  CF-MORT-JOINT-CODE         PIC X.
00877                  88  CF-VALID-JOINT-CODE        VALUE 'A' 'V'.
00878              20  CF-MORT-PC-Q               PIC X.
00879                  88  CF-VALID-PC-Q              VALUE 'Y' 'N' ' '.
00880              20  CF-MORT-TABLE-CODE         PIC X(4).
00881              20  CF-MORT-COMMENTS           PIC X(15).
00882              20  FILLER                     PIC X(14).
00883
00884          16  FILLER                         PIC X(251).
00885
00886
00887 ****************************************************************
00888 *             BUSSINESS TYPE MASTER RECORD                     *
00889 ****************************************************************
00890
00891      12  CF-BUSINESS-TYPE-MASTER-REC REDEFINES  CF-RECORD-BODY.
00892 * FIRST ENTRY IS TYPE 01.. LAST IS TYPE 20
00893 * RECORD 02 IS TYPES 21-40..RECORD 03 IS 41-60..04 IS 61-80
00894 * AND RECORD 05 IS TYPES 81-99
00895          16  CF-TYPE-DESCRIPTIONS   OCCURS  20  TIMES.
00896              20  CF-BUSINESS-TITLE          PIC  X(19).
00897              20  CF-BUS-MOD-ST-TRGT-LOSS-RATIO
00898                                             PIC S9V9(4) COMP-3.
00899              20  CF-BUS-EXCL-ST-CALL        PIC  X.
00900              20  FILLER                     PIC  X.
00901          16  FILLER                         PIC  X(248).
00902
00903
00904 ****************************************************************
00905 *             TERMINAL MASTER RECORD                           *
00906 ****************************************************************
00907
00908      12  CF-TERMINAL-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00909
00910          16  CF-COMPANY-TERMINALS.
00911              20  CF-TERMINAL-ID  OCCURS 120 TIMES
00912                                   PIC X(4).
00913          16  FILLER               PIC X(248).
00914
00915
00916 ****************************************************************
00917 *             LIFE EDIT MASTER RECORD                          *
00918 ****************************************************************
00919
00920      12  CF-LIFE-EDIT-MASTER-REC REDEFINES  CF-RECORD-BODY.
00921          16  CF-LIFE-EDIT-ENTRIES   OCCURS 120  TIMES.
00922              20  CF-LIFE-CODE-IN            PIC XX.
00923              20  CF-LIFE-CODE-OUT           PIC XX.
00924          16  FILLER                         PIC X(248).
00925
00926
00927 ****************************************************************
00928 *             AH EDIT MASTER RECORD                            *
00929 ****************************************************************
00930
00931      12  CF-AH-EDIT-MASTER-REC REDEFINES  CF-RECORD-BODY.
00932          16  CF-AH-EDIT-ENTRIES   OCCURS  96  TIMES.
00933              20  CF-AH-CODE-IN              PIC XXX.
00934              20  CF-AH-CODE-OUT             PIC XX.
00935          16  FILLER                         PIC X(248).
00936
00937
00938 ****************************************************************
00939 *             CREDIBILITY TABLES                               *
00940 ****************************************************************
00941
00942      12  CF-CREDIBILITY-MASTER-REC REDEFINES  CF-RECORD-BODY.
00943          16  CF-CRDB-ENTRY   OCCURS 36 TIMES
00944                              INDEXED BY CF-CRDB-NDX.
00945              20  CF-CRDB-FROM               PIC S9(7)   COMP-3.
00946              20  CF-CRDB-TO                 PIC S9(7)   COMP-3.
00947              20  CF-CREDIBILITY-FACTOR      PIC S9V9(4) COMP-3.
00948          16  FILLER                         PIC  X(332).
00949
00950
00951 ****************************************************************
00952 *             REPORT CUSTOMIZATION RECORD                      *
00953 ****************************************************************
00954
00955      12  CF-CUSTOM-REPORT-REC  REDEFINES  CF-RECORD-BODY.
00956          16  CF-ACCOUNT-MASTER-STATUS       PIC X.
00957              88  CF-ACTIVE-ACCOUNTS             VALUE 'A'.
00958              88  CF-INACTIVE-ACCOUNTS           VALUE 'I'.
121307             88  CF-CANCELLED-ACCOUNTS          VALUE 'C'.
00959 **** NOTE: INACTIVE WILL INCLUDE ACCOUNT MASTER CODED WITH ****
00960 ****       A T-TRANSFER.                                   ****
00961              88  CF-ALL-ACCOUNTS                VALUE 'B'.
00962
00963          16  FILLER                         PIC XX.
00964
00965          16  CF-CARRIER-CNTL-OPT.
00966              20  CF-CARRIER-OPT-SEQ         PIC 9.
00967                  88  CF-CARRIER-OPT-USED        VALUE 1 THRU 6.
00968                  88  CF-CARRIER-OPT-NOT-USED    VALUE 0.
00969              20  CF-CARRIER-SELECT OCCURS 3 TIMES
00970                                             PIC X.
00971          16  CF-GROUP-CNTL-OPT.
00972              20  CF-GROUP-OPT-SEQ           PIC 9.
00973                  88  CF-GROUP-OPT-USED          VALUE 1 THRU 6.
00974                  88  CF-GROUP-OPT-NOT-USED      VALUE 0.
00975              20  CF-GROUP-SELECT OCCURS 3 TIMES
00976                                             PIC X(6).
00977          16  CF-STATE-CNTL-OPT.
00978              20  CF-STATE-OPT-SEQ           PIC 9.
00979                  88  CF-STATE-OPT-USED          VALUE 1 THRU 6.
00980                  88  CF-STATE-OPT-NOT-USED      VALUE 0.
00981              20  CF-STATE-SELECT OCCURS 3 TIMES
00982                                             PIC XX.
00983          16  CF-ACCOUNT-CNTL-OPT.
00984              20  CF-ACCOUNT-OPT-SEQ         PIC 9.
00985                  88  CF-ACCOUNT-OPT-USED        VALUE 1 THRU 6.
00986                  88  CF-ACCOUNT-OPT-NOT-USED    VALUE 0.
00987              20  CF-ACCOUNT-SELECT OCCURS 3 TIMES
00988                                             PIC X(10).
00989          16  CF-BUS-TYP-CNTL-OPT.
00990              20  CF-BUS-TYP-OPT-SEQ         PIC 9.
00991                  88  CF-BUS-TYP-OPT-USED        VALUE 1 THRU 6.
00992                  88  CF-BUS-TYP-OPT-NOT-USED    VALUE 0.
00993              20  CF-BUS-TYP-SELECT OCCURS 3 TIMES
00994                                             PIC XX.
00995          16  CF-LF-TYP-CNTL-OPT.
00996              20  CF-LF-TYP-OPT-SEQ          PIC 9.
00997                  88  CF-LF-TYP-OPT-USED         VALUE 1 THRU 6.
00998                  88  CF-LF-TYP-OPT-NOT-USED     VALUE 0.
00999              20  CF-BUS-LF-SELECT OCCURS 3 TIMES
01000                                             PIC XX.
01001          16  CF-AH-TYP-CNTL-OPT.
01002              20  CF-AH-TYP-OPT-SEQ          PIC 9.
01003                  88  CF-AH-TYP-OPT-USED         VALUE 1 THRU 6.
01004                  88  CF-AH-TYP-OPT-NOT-USED     VALUE 0.
01005              20  CF-BUS-AH-SELECT OCCURS 3 TIMES
01006                                             PIC XX.
01007          16  CF-REPTCD1-CNTL-OPT.
01008              20  CF-REPTCD1-OPT-SEQ         PIC 9.
01009                  88  CF-REPTCD1-OPT-USED        VALUE 1 THRU 6.
01010                  88  CF-REPTCD1-OPT-NOT-USED    VALUE 0.
01011              20  CF-REPTCD1-SELECT OCCURS 3 TIMES
01012                                             PIC X(10).
01013          16  CF-REPTCD2-CNTL-OPT.
01014              20  CF-REPTCD2-OPT-SEQ         PIC 9.
01015                  88  CF-REPTCD2-OPT-USED        VALUE 1 THRU 6.
01016                  88  CF-REPTCD2-OPT-NOT-USED    VALUE 0.
01017              20  CF-REPTCD2-SELECT OCCURS 3 TIMES
01018                                             PIC X(10).
01019          16  CF-USER1-CNTL-OPT.
01020              20  CF-USER1-OPT-SEQ           PIC 9.
01021                  88  CF-USER1-OPT-USED          VALUE 1 THRU 6.
01022                  88  CF-USER1-OPT-NOT-USED      VALUE 0.
01023              20  CF-USER1-SELECT OCCURS 3 TIMES
01024                                             PIC X(10).
01025          16  CF-USER2-CNTL-OPT.
01026              20  CF-USER2-OPT-SEQ           PIC 9.
01027                  88  CF-USER2-OPT-USED          VALUE 1 THRU 6.
01028                  88  CF-USER2-OPT-NOT-USED      VALUE 0.
01029              20  CF-USER2-SELECT OCCURS 3 TIMES
01030                                             PIC X(10).
01031          16  CF-USER3-CNTL-OPT.
01032              20  CF-USER3-OPT-SEQ           PIC 9.
01033                  88  CF-USER3-OPT-USED          VALUE 1 THRU 6.
01034                  88  CF-USER3-OPT-NOT-USED      VALUE 0.
01035              20  CF-USER3-SELECT OCCURS 3 TIMES
01036                                             PIC X(10).
01037          16  CF-USER4-CNTL-OPT.
01038              20  CF-USER4-OPT-SEQ           PIC 9.
01039                  88  CF-USER4-OPT-USED          VALUE 1 THRU 6.
01040                  88  CF-USER4-OPT-NOT-USED      VALUE 0.
01041              20  CF-USER4-SELECT OCCURS 3 TIMES
01042                                             PIC X(10).
01043          16  CF-USER5-CNTL-OPT.
01044              20  CF-USER5-OPT-SEQ           PIC 9.
01045                  88  CF-USER5-OPT-USED          VALUE 1 THRU 6.
01046                  88  CF-USER5-OPT-NOT-USED      VALUE 0.
01047              20  CF-USER5-SELECT OCCURS 3 TIMES
01048                                             PIC X(10).
01049          16  CF-REINS-CNTL-OPT.
01050              20  CF-REINS-OPT-SEQ           PIC 9.
01051                  88  CF-REINS-OPT-USED          VALUE 1 THRU 6.
01052                  88  CF-REINS-OPT-NOT-USED      VALUE 0.
01053              20  CF-REINS-SELECT OCCURS 3 TIMES.
01054                  24  CF-REINS-PRIME         PIC XXX.
01055                  24  CF-REINS-SUB           PIC XXX.
01056
01057          16  CF-AGENT-CNTL-OPT.
01058              20  CF-AGENT-OPT-SEQ           PIC 9.
01059                  88  CF-AGENT-OPT-USED          VALUE 1 THRU 6.
01060                  88  CF-AGENT-OPT-NOT-USED      VALUE 0.
01061              20  CF-AGENT-SELECT OCCURS 3 TIMES
01062                                             PIC X(10).
01063
01064          16  FILLER                         PIC X(43).
01065
01066          16  CF-LOSS-RATIO-SELECT.
01067              20  CF-SEL-LO-LOSS-RATIO       PIC S999V99  COMP-3.
01068              20  CF-SEL-HI-LOSS-RATIO       PIC S999V99  COMP-3.
01069          16  CF-ENTRY-DATE-SELECT.
01070              20  CF-SEL-LO-ENTRY-DATE       PIC XX.
01071              20  CF-SEL-HI-ENTRY-DATE       PIC XX.
01072          16  CF-EFFECTIVE-DATE-SELECT.
01073              20  CF-SEL-LO-EFFECTIVE-DATE   PIC XX.
01074              20  CF-SEL-HI-EFFECTIVE-DATE   PIC XX.
01075
01076          16  CF-EXCEPTION-LIST-IND          PIC X.
01077              88  CF-EXCEPTION-LIST-REQUESTED VALUE 'Y'.
01078
01079          16  FILLER                         PIC X(318).
01080
01081 ****************************************************************
01082 *                  EXCEPTION REPORTING RECORD                  *
01083 ****************************************************************
01084
01085      12  CF-EXCEPTION-REPORT-REC REDEFINES   CF-RECORD-BODY.
01086          16  CF-ACCOUNTS-LT-ONE-YEAR        PIC X.
01087              88  CF-EXCEPTION-ACCTS-WITHIN-ONE  VALUE 'Y'.
01088
01089          16  CF-COMBINED-LIFE-AH-OPT.
01090              20  CF-ISS-COUNT-DIFF          PIC S9(05)     COMP-3.
01091              20  CF-SINGLE-MO-PREM-PCT      PIC S9(02).
01092              20  CF-EARN-PREM-DECR-PCT      PIC S9(02).
01093              20  CF-CANCELLATION-RATIO      PIC S9(02).
01094
01095          16  CF-LIFE-OPT.
01096              20  CF-LF-LOSS-RATIO-PCT       PIC S9(03)     COMP-3.
01097              20  CF-LF-LTM-LOSS-RATIO       PIC S9(03)     COMP-3.
01098              20  CF-LF-PERIOD-PROFIT        PIC S9(03)     COMP-3.
01099              20  CF-LF-LTM-PROFIT-PCT       PIC S9(02)V9   COMP-3.
01100              20  CF-LF-LTM-INFORCE-DECR     PIC S9(02)V9   COMP-3.
01101              20  CF-LF-LTM-TERM-CHG         PIC S9(02)V9   COMP-3.
01102              20  CF-LF-TERM-AVG-WEIGHTED    PIC S9(02)V9   COMP-3.
01103              20  CF-LF-LTM-AGE-PCT          PIC S9(02)V9   COMP-3.
01104              20  CF-LF-AGE-AVG-WEIGHTED     PIC S9(02)V9   COMP-3.
01105              20  CF-LF-AVG-AGE-MAX          PIC S9(02).
01106
01107          16  CF-AH-OPT.
01108              20  CF-AH-LOSS-RATIO-PCT       PIC S9(03)     COMP-3.
01109              20  CF-AH-LTM-LOSS-RATIO       PIC S9(03)     COMP-3.
01110              20  CF-AH-PERIOD-PROFIT        PIC S9(03)     COMP-3.
01111              20  CF-AH-LTM-PROFIT-PCT       PIC S9(02)V9   COMP-3.
01112              20  CF-AH-LTM-INFORCE-DECR     PIC S9(02)V9   COMP-3.
01113              20  CF-AH-LTM-TERM-CHG         PIC S9(02)V9   COMP-3.
01114              20  CF-AH-TERM-AVG-WEIGHTED    PIC S9(02)V9   COMP-3.
01115              20  CF-AH-LTM-AGE-PCT          PIC S9(02)V9   COMP-3.
01116              20  CF-AH-AGE-AVG-WEIGHTED     PIC S9(02)V9   COMP-3.
01117              20  CF-AH-AVG-AGE-MAX          PIC S9(02).
01118
01119          16  CF-ACCT-ZERO-MONTH-PRODUCTION PIC X.
01120              88  CF-ACCT-CURRENT-MONTH-ACT      VALUE 'A'.
01121              88  CF-ACCT-WITH-NO-PRODUCTION     VALUE 'B'.
01122              88  CF-ACCT-WITH-ISSUE-ACTIVITY    VALUE 'C'.
01123
01124          16  CF-RETENTION-LIMIT             PIC S9(7)      COMP-3.
01125
01126          16  FILLER                         PIC X(673).
01127
01128
01129 ****************************************************************
01130 *             MORTGAGE SYSTEM PLAN RECORD                      *
01131 ****************************************************************
01132
01133      12  CF-MORTGAGE-PLAN-MASTER  REDEFINES  CF-RECORD-BODY.
01134          16  CF-PLAN-TYPE                   PIC X.
01135              88  CF-LIFE-MORT-PLAN             VALUE 'L'.
01136              88  CF-DISAB-MORT-PLAN            VALUE 'D'.
01137              88  CF-AD-D-MORT-PLAN             VALUE 'A'.
01138          16  CF-PLAN-ABBREV                 PIC XXX.
01139          16  CF-PLAN-DESCRIPT               PIC X(10).
01140          16  CF-PLAN-NOTES                  PIC X(20).
01141          16  CF-PLAN-ESTABLISH-DATE         PIC XX.
01142          16  CF-PLAN-UNDERWRITING.
01143              20  CF-PLAN-TERM-DATA.
01144                  24  CF-MINIMUM-TERM        PIC S999      COMP-3.
01145                  24  CF-MAXIMUM-TERM        PIC S999      COMP-3.
01146              20  CF-PLAN-AGE-DATA.
01147                  24  CF-MINIMUM-AGE         PIC S999      COMP-3.
01148                  24  CF-MAXIMUM-AGE         PIC S999      COMP-3.
01149                  24  CF-MAXIMUM-ATTAIN-AGE  PIC S999      COMP-3.
01150              20  CF-PLAN-BENEFIT-DATA.
01151                  24  CF-MINIMUM-BENEFIT     PIC S9(7)V99  COMP-3.
01152                  24  CF-MAXIMUM-BENEFIT     PIC S9(7)V99  COMP-3.
01153                  24  CF-MAXIMUM-MONTHLY-BENEFIT
01154                                             PIC S9(7)V99  COMP-3.
01155          16  CF-PLAN-POLICY-FORMS.
01156              20  CF-POLICY-FORM             PIC X(12).
01157              20  CF-MASTER-APPLICATION      PIC X(12).
01158              20  CF-MASTER-POLICY           PIC X(12).
01159          16  CF-PLAN-RATING.
01160              20  CF-RATE-CODE               PIC X(5).
01161              20  CF-SEX-RATING              PIC X.
01162                  88  CF-PLAN-NOT-SEX-RATED     VALUE '1'.
01163                  88  CF-PLAN-SEX-RATED         VALUE '2'.
01164              20  CF-SUB-STD-PCT             PIC S9V9999   COMP-3.
01165              20  CF-SUB-STD-TYPE            PIC X.
01166                  88  CF-PCT-OF-PREM            VALUE '1'.
01167                  88  CF-PCT-OF-BENE            VALUE '2'.
01168          16  CF-PLAN-PREM-TOLERANCES.
01169              20  CF-PREM-TOLERANCE          PIC S999      COMP-3.
01170              20  CF-PREM-TOLERANCE-PCT      PIC SV999     COMP-3.
01171          16  CF-PLAN-PYMT-TOLERANCES.
01172              20  CF-PYMT-TOLERANCE          PIC S999      COMP-3.
01173              20  CF-PYMT-TOLERANCE-PCT      PIC SV999     COMP-3.
01174          16  CF-PLAN-MISC-DATA.
01175              20  FILLER                     PIC X.
01176              20  CF-FREE-EXAM-DAYS          PIC S999      COMP-3.
01177              20  CF-RETRO-RETENTION         PIC S9V9999   COMP-3.
01178          16  CF-MORT-PLAN-USE-CTR           PIC S999      COMP-3.
01179          16  CF-PLAN-IND-GRP                PIC X.
01180              88  CF-MORT-INDIV-PLAN            VALUE 'I'
01181                                                      '1'.
01182              88  CF-MORT-GROUP-PLAN            VALUE 'G'
01183                                                      '2'.
01184          16  CF-MIB-SEARCH-SW               PIC X.
01185              88  CF-MIB-SEARCH-ALL             VALUE '1'.
01186              88  CF-MIB-SEARCH-NONE            VALUE '2'.
01187              88  CF-MIB-SEARCH-EXCEEDED        VALUE '3'.
01188              88  CF-MIB-SEARCH-VALID      VALUES ARE '1' '2' '3'.
01189          16  CF-ALPHA-SEARCH-SW             PIC X.
01190              88  CF-MIB-ALPHA-ALL              VALUE '1'.
01191              88  CF-MIB-ALPHA-NONE             VALUE '2'.
01192              88  CF-MIB-APLHA-EXCEEDED         VALUE '3'.
01193              88  CF-CLIENT-ALPHA-ALL           VALUE 'A'.
01194              88  CF-CLIENT-ALPHA-NONE          VALUE 'B'.
01195              88  CF-CLIENT-APLHA-EXCEEDED      VALUE 'C'.
01196              88  CF-BOTH-ALPHA-ALL             VALUE 'X'.
01197              88  CF-BOTH-ALPHA-NONE            VALUE 'Y'.
01198              88  CF-BOTH-APLHA-EXCEEDED        VALUE 'Z'.
01199              88  CF-ALPHA-SEARCH-VALID    VALUES ARE '1' '2' '3'
01200                                                      'A' 'B' 'C'
01201                                                      'X' 'Y' 'Z'.
01202          16  CF-EFF-DT-RULE-SW              PIC X.
01203              88  CF-EFF-DT-ENTER               VALUE 'E'.
01204              88  CF-EFF-DT-MONTH               VALUE 'M'.
01205              88  CF-EFF-DT-QTR                 VALUE 'Q'.
01206              88  CF-EFF-DT-SEMI                VALUE 'S'.
01207              88  CF-EFF-DT-ANN                 VALUE 'A'.
01208          16  FILLER                         PIC X(4).
01209          16  CF-HEALTH-QUESTIONS            PIC X.
01210              88  CF-VALID-QUESTIONS-CNT VALUES ARE '0' THRU '9'.
01211          16  CF-GRACE-PERIOD                PIC S999      COMP-3.
01212          16  CF-NUMBER-LAPSE-NOTICES        PIC S999      COMP-3.
01213          16  CF-PLAN-SNGL-JNT               PIC X.
01214              88  CF-COMBINED-PLAN              VALUE 'C'.
01215              88  CF-JNT-PLAN                   VALUE 'J'.
01216              88  CF-SNGL-PLAN                  VALUE 'S'.
01217          16  CF-DAYS-TO-1ST-NOTICE          PIC  99.
01218          16  CF-DAYS-TO-2ND-NOTICE          PIC  99.
01219          16  CF-DAYS-TO-3RD-NOTICE          PIC  99.
01220          16  CF-DAYS-TO-4TH-NOTICE          PIC  99.
01221          16  CF-RERATE-CNTL                 PIC  X.
01222              88  CF-RERATE-WITH-ISSUE-AGE       VALUE '1'.
01223              88  CF-RERATE-WITH-CURRENT-AGE     VALUE '2'.
01224              88  CF-DO-NOT-RERATE               VALUE '3' ' '.
01225              88  CF-AUTO-RECALC                 VALUE '4'.
01226          16  CF-BENEFIT-TYPE                PIC  X.
01227              88  CF-BENEFIT-IS-LEVEL            VALUE '1'.
01228              88  CF-BENEFIT-REDUCES             VALUE '2'.
01229          16  CF-POLICY-FEE                  PIC S999V99
01230                                                     COMP-3.
01231          16  CF-1ST-NOTICE-FORM             PIC  X(04).
01232          16  CF-2ND-NOTICE-FORM             PIC  X(04).
01233          16  CF-3RD-NOTICE-FORM             PIC  X(04).
01234          16  CF-4TH-NOTICE-FORM             PIC  X(04).
01235          16  FILLER                         PIC  X(32).
01236          16  CF-TERMINATION-FORM            PIC  X(04).
01237          16  FILLER                         PIC  X(08).
01238          16  CF-CLAIM-CAP                   PIC S9(7)V99
01239                                                        COMP-3.
01240          16  CF-REOCCURRING-DISABILITY-PRD  PIC S999   COMP-3.
01241          16  CF-ISSUE-LETTER                PIC  X(4).
01242          16  CF-YEARS-TO-NEXT-RERATE        PIC  99.
01243          16  CF-DEPENDENT-COVERAGE          PIC  X.
01244              88  CF-YES-DEP-COV                 VALUE 'Y'.
01245              88  CF-NO-DEP-COV             VALUES ARE 'N' ' '.
01246          16  CF-MP-REFUND-CALC              PIC X.
01247              88  CF-MP-REFUND-NOT-USED          VALUE SPACE.
01248              88  CF-MP-REFD-BY-R78              VALUE '1'.
01249              88  CF-MP-REFD-BY-PRO-RATA         VALUE '2'.
01250              88  CF-MP-REFD-AS-CALIF            VALUE '3'.
01251              88  CF-MP-REFD-AS-TEXAS            VALUE '4'.
01252              88  CF-MP-REFD-IS-NET-PAY          VALUE '5'.
01253              88  CF-MP-REFD-ANTICIPATION        VALUE '6'.
01254              88  CF-MP-REFD-MEAN                VALUE '8'.
01255          16  CF-ALT-RATE-CODE               PIC  X(5).
01256
01257
01258          16  FILLER                         PIC X(498).
01259 ****************************************************************
01260 *             MORTGAGE COMPANY MASTER RECORD                   *
01261 ****************************************************************
01262
01263      12  CF-MORTG-COMPANY-MASTER-REC  REDEFINES  CF-RECORD-BODY.
01264          16  CF-MORTG-ALT-MORT-CODE         PIC X(4).
01265          16  CF-MORTG-ACCESS-CONTROL        PIC X.
01266              88  CF-MORT-ST-PROD-CNTL                VALUE ' '.
01267              88  CF-MORT-CARR-GRP-ST-PROD-CNTL       VALUE '1'.
01268              88  CF-MORT-CARR-ST-PROD-CNTL           VALUE '2'.
01269              88  CF-MORT-PROD-CNTL                   VALUE '3'.
01270              88  CF-MORT-CARR-PROD-CNTL              VALUE '4'.
01271
01272          16  CF-MORTG-CONVERSION-DATE       PIC XX.
01273          16  CF-MORTG-RATE-FILE-MAINT-DATE  PIC XX.
01274          16  CF-MORTG-RATE-FILE-CREAT-DATE  PIC XX.
01275          16  CF-MORTG-PROD-FILE-MAINT-DATE  PIC XX.
01276          16  CF-MORTG-PROD-FILE-CREAT-DATE  PIC XX.
01277
01278          16  CF-MP-POLICY-LINKAGE-IND       PIC X(1).
01279              88  CF-MP-PLCY-LINKAGE-USED     VALUE 'Y'.
01280          16  CF-MP-RECON-USE-IND            PIC X(1).
01281              88  CF-MP-USE-RECON             VALUE 'Y'.
01282          16  CF-MORTG-CHECK-NO-COUNTER      PIC 9(6).
01283              88  CF-MP-CHECK-CNT-RESET-VALUE VALUE 999999.
01284          16  CF-MP-REPORT-LANGUAGE-IND      PIC X(1).
01285              88  CF-MP-LANGUAGE-IS-ENG       VALUE 'E'.
01286              88  CF-MP-LANGUAGE-IS-FR        VALUE 'F'.
01287          16  FILLER                         PIC X(1).
01288          16  CF-MORTG-CHECK-QUEUE-COUNTER   PIC 9(6).
01289              88  CF-MP-CHKQ-CNT-RESET-VALUE  VALUE 999999.
01290          16  CF-MORTG-MIB-VERSION           PIC X.
01291              88  CF-MORTG-MIB-BATCH         VALUE '1'.
01292              88  CF-MORTG-MIB-ONLINE        VALUE '2'.
01293              88  CF-MORTG-MIB-BOTH          VALUE '3'.
01294          16  CF-MORTG-ALT-MIB-SEARCH-CNTL.
01295              20  CF-MORTG-MIB-LNAME-SEARCH  PIC X.
01296                  88  CF-MIB-LAST-NAME-SEARCH     VALUE 'Y'.
01297              20  CF-MORTG-MIB-FNAME-SEARCH  PIC X.
01298                  88  CF-MIB-FIRST-NAME-SEARCH    VALUE 'Y'.
01299              20  CF-MORTG-MIB-MNAME-SEARCH  PIC X.
01300                  88  CF-MIB-MIDDLE-NAME-SEARCH   VALUE 'Y'.
01301              20  CF-MORTG-MIB-BDATE-SEARCH  PIC X.
01302                  88  CF-MIB-BIRTH-DATE-SEARCH    VALUE 'Y'.
01303              20  CF-MORTG-MIB-BSTATE-SEARCH PIC X.
01304                  88  CF-MIB-BIRTH-STATE-SEARCH   VALUE 'Y'.
01305              20  CF-MORTG-MIB-RSTATE-SEARCH PIC X.
01306                  88  CF-MIB-RESIDNT-STATE-SEARCH VALUE 'Y'.
01307          16  CF-MORTG-MIB-COMPANY-SYMBOL    PIC XXX.
01308          16  FILLER                         PIC X(7).
01309          16  CF-MORTG-DESTINATION-SYMBOL.
01310              20  CF-MORTG-MIB-COMM          PIC X(5).
01311              20  CF-MORTG-MIB-TERM          PIC X(5).
01312          16  CF-ASSIGN-POLICY-NO-SW         PIC X(01).
01313              88  CF-ASSIGN-POLICY-NO             VALUE 'Y'.
01314          16  FILLER                         PIC X(03).
01315          16  CF-MP-CHECK-NO-CONTROL.
01316              20  CF-MP-CHECK-NO-METHOD      PIC X(01).
01317                  88  CF-MP-CHECK-NO-MANUAL     VALUE '1'.
01318                  88  CF-MP-CHECK-NO-AUTO-SEQ   VALUE '2'
01319                                                 ' ' LOW-VALUES.
01320                  88  CF-MP-CHECK-NO-PRE-PRINTED
01321                                                VALUE '3'.
01322          16  CF-MORTG-LOAN-SHIFT-IND        PIC X(01).
01323          16  CF-MORTG-SOLICITATION-NUM      PIC S9(17) COMP-3.
01324          16  CF-MORTG-ALT-ALPHA-SEARCH-CNTL.
01325              20  CF-MORTG-ALP-LNAME-SEARCH  PIC X.
01326                  88  CF-ALPHA-LAST-NAME-SEARCH      VALUE 'Y'.
01327              20  CF-MORTG-ALP-FNAME-SEARCH  PIC X.
01328                  88  CF-ALPHA-FIRST-NAME-SEARCH     VALUE 'Y'.
01329              20  CF-MORTG-ALP-MNAME-SEARCH  PIC X.
01330                  88  CF-ALPHA-MIDDLE-NAME-SEARCH    VALUE 'Y'.
01331              20  CF-MORTG-ALP-BDATE-SEARCH  PIC X.
01332                  88  CF-ALPHA-BIRTH-DATE-SEARCH     VALUE 'Y'.
01333              20  CF-MORTG-ALP-BSTATE-SEARCH PIC X.
01334                  88  CF-ALPHA-BIRTH-STATE-SEARCH    VALUE 'Y'.
01335              20  CF-MORTG-ALP-RSTATE-SEARCH PIC X.
01336                  88  CF-ALPHA-RESIDNT-STATE-SEARCH  VALUE 'Y'.
01337          16  CF-MORTG-BILLING-AREA.
01338              20  CF-MORTG-BILL-CYCLE   OCCURS  5  TIMES
01339                                             PIC X.
01340          16  CF-MORTG-MONTH-END-DT          PIC XX.
01341          16  CF-MORTG-CURRENT-ARCH-NUM      PIC S9(8)  COMP.
01342          16  CF-MORTG-START-ARCH-NUM        PIC S9(8)  COMP.
01343          16  CF-MORTG-MIB-DEST-SW           PIC X.
01344              88 CF-MORTG-MIB-COMM-DEST              VALUE '1'.
01345              88 CF-MORTG-MIB-TERM-DEST              VALUE '2'.
01346          16  FILLER                         PIC X.
01347          16  CF-MORTG-LABEL-CONTROL         PIC X.
01348              88 CF-MORTG-CREATE-LABELS              VALUE 'Y'.
01349              88 CF-MORTG-BYPASS-LABELS              VALUE 'N'.
01350          16  CF-ACH-ORIGINATING-DFI-ID      PIC X(8).
01351          16  FILLER                         PIC X(8).
01352          16  CF-ACH-SENDING-DFI-NAME        PIC X(23).
01353          16  CF-ACH-RECVING-DFI-ROUTING-NO  PIC X(8).
01354          16  CF-ACH-RECVING-DFI-NAME        PIC X(23).
01355          16  CF-ACH-COMPANY-ID.
01356              20  CF-ACH-ID-CODE-DESIGNATOR  PIC X.
01357                  88  CF-ACH-ICD-IRS-EIN             VALUE '1'.
01358                  88  CF-ACH-ICD-DUNS                VALUE '3'.
01359                  88  CF-ACH-ICD-USER-ASSIGNED-NO    VALUE '9'.
01360              20  CF-ACH-COMPANY-ID-NO       PIC X(9).
01361          16  CF-MORTG-BILL-GROUPING-CODE    PIC X.
01362              88  CF-MORTG-CO-HAS-GROUPING           VALUE 'Y'.
01363          16  CF-RATE-DEV-AUTHORIZATION      PIC X.
01364              88  CF-RATE-DEV-AUTHORIZED             VALUE 'Y'.
01365              88  CF-RATE-DEV-NOT-AUTHORIZED         VALUE 'N'.
01366          16  CF-ACH-SENDING-DFI-ROUTING-NO  PIC X(9).
01367          16  CF-CBA-FILE-CREATE-NUM         PIC 9(4).
01368          16  FILLER                         PIC X(536).
01369
01370 ****************************************************************
01371 *             MORTGAGE HEIGHT - WEIGHT CHARTS                  *
01372 ****************************************************************
01373
01374      12  CF-FEMALE-HT-WT-REC  REDEFINES CF-RECORD-BODY.
01375          16  CF-FEMALE-HT-WT-INFO OCCURS 30 TIMES.
01376              20  CF-FEMALE-HEIGHT.
01377                  24  CF-FEMALE-FT           PIC 99.
01378                  24  CF-FEMALE-IN           PIC 99.
01379              20  CF-FEMALE-MIN-WT           PIC 999.
01380              20  CF-FEMALE-MAX-WT           PIC 999.
01381          16  FILLER                         PIC X(428).
01382
01383      12  CF-MALE-HT-WT-REC    REDEFINES CF-RECORD-BODY.
01384          16  CF-MALE-HT-WT-INFO   OCCURS 30 TIMES.
01385              20  CF-MALE-HEIGHT.
01386                  24  CF-MALE-FT             PIC 99.
01387                  24  CF-MALE-IN             PIC 99.
01388              20  CF-MALE-MIN-WT             PIC 999.
01389              20  CF-MALE-MAX-WT             PIC 999.
01390          16  FILLER                         PIC X(428).
01391 ******************************************************************
01392 *             AUTOMATIC ACTIVITY RECORD                          *
01393 ******************************************************************
01394      12  CF-AUTO-ACTIVITY-REC REDEFINES CF-RECORD-BODY.
01395          16  CF-SYSTEM-DEFINED-ACTIVITY OCCURS 09 TIMES.
01396              20  CF-SYS-ACTIVE-SW           PIC X(01).
01397              20  CF-SYS-LETTER-ID           PIC X(04).
01398              20  CF-SYS-RESEND-DAYS         PIC 9(03).
01399              20  CF-SYS-FOLLOW-UP-DAYS      PIC 9(03).
01400              20  CF-SYS-RESET-SW            PIC X(01).
01401              20  CF-SYS-REPORT-DAYS         PIC 9(03).
01402              20  CF-SYS-EACH-DAY-AFTER-SW   PIC X(01).
01403
01404          16  FILLER                         PIC X(50).
01405
01406          16  CF-USER-DEFINED-ACTIVITY  OCCURS 08 TIMES.
01407              20  CF-USER-ACTIVE-SW          PIC X(01).
01408              20  CF-USER-LETTER-ID          PIC X(04).
01409              20  CF-USER-RESEND-DAYS        PIC 9(03).
01410              20  CF-USER-FOLLOW-UP-DAYS     PIC 9(03).
01411              20  CF-USER-RESET-SW           PIC X(01).
01412              20  CF-USER-REPORT-DAYS        PIC 9(03).
01413              20  CF-USER-EACH-DAY-AFTER-SW  PIC X(01).
01414              20  CF-USER-ACTIVITY-DESC      PIC X(20).
01415
01416          16  FILLER                         PIC X(246).
00342
00343      EJECT
00344 *                            COPY ERCPYAJ.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ERCPYAJ                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.015                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = PENDING PAYMENT AND ADJUSTMENTS           *
00008 *                                                                *
00009 *                                                                *
00010 *   FILE TYPE = VSAM,KSDS                                        *
00011 *   RECORD SIZE = 200  RECFORM = FIXED                           *
00012 *                                                                *
00013 *   BASE CLUSTER = ERPYAJ                         RKP=2,LEN=33   *
00014 *       ALTERNATE PATHS = NONE                                   *
00015 *                                                                *
00016 *   LOG = YES                                                    *
00017 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00018 ******************************************************************
042303******************************************************************
042303*                   C H A N G E   L O G
042303*
042303* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
042303*-----------------------------------------------------------------
042303*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
042303* EFFECTIVE    NUMBER
042303*-----------------------------------------------------------------
042303* 042303                   PEMA ADD PROCESSING FOR DUE PREM ADJS
060205* 060205                   PEMA ADD ERCOMP TYPE TO ERPYAJ
042303******************************************************************
00019
00020  01  PENDING-PAY-ADJ.
00021      12  PY-RECORD-ID                     PIC XX.
00022          88  VALID-PY-ID                        VALUE 'PY'.
00023
00024      12  PY-CONTROL-PRIMARY.
00025          16  PY-COMPANY-CD                PIC X.
00026          16  PY-CARRIER                   PIC X.
00027          16  PY-GROUPING                  PIC X(6).
00028          16  PY-FIN-RESP                  PIC X(10).
00029          16  PY-ACCOUNT                   PIC X(10).
00030          16  PY-PRODUCER REDEFINES PY-ACCOUNT
00031                                           PIC X(10).
00032          16  PY-FILE-SEQ-NO               PIC S9(8)     COMP.
00033          16  PY-RECORD-TYPE               PIC X.
00034              88  PY-REMIT-RECEIVED            VALUE 'R'.
00035              88  PY-DEPOSIT                   VALUE 'D'.
00036              88  PY-CHARGE-TO-AGENT           VALUE 'C'.
00037              88  PY-ADJ-REM-RECEIVED          VALUE 'S'.
00038              88  PY-ADJ-DEPOSIT               VALUE 'T'.
00039              88  PY-ADJ-CHG-TO-AGT            VALUE 'U'.
00040              88  PY-ADD-TO-YTD-COMP           VALUE 'X'.
00041              88  PY-SUBTRACT-YTD-COMP         VALUE 'Y'.
00042              88  PY-ADD-TO-BALANCE            VALUE 'Z'.
00043              88  PY-FICA-ENTRY                VALUE 'F'.
00044              88  PY-REMIT-IND-GROUPING        VALUE 'G'.
00045              88  PY-POLICY-FEE                VALUE 'W'.
042303             88  PY-DUE-PREM-ADJ              VALUE 'P'.
00046
00047      12  PY-PYMT-TYPE                     PIC X.
00048              88  PY-NEW-BUS-PYMT              VALUE 'B'.
00049              88  PY-REINS-PYMT                VALUE 'R'.
00050              88  PY-EXP-PYMT                  VALUE 'E'.
00051
00052      12  PY-BIL-INV                       PIC X(6).
00053      12  PY-REF-NO                        PIC X(12).
00054
00055      12  PY-LAST-MAINT-DT                 PIC XX.
00056      12  PY-LAST-MAINT-BY                 PIC X(4).
00057      12  PY-LAST-MAINT-HHMMSS             PIC S9(6)     COMP-3.
00058
00059      12  PY-PYADJ-RECORD.
00060          16  PY-ENTRY-AMT                 PIC S9(7)V99  COMP-3.
00061          16  PY-ENTRY-COMMENT             PIC X(30).
CIDMOD         16  PY-GL-DATA      REDEFINES PY-ENTRY-COMMENT.
CIDMOD             20  PY-GL-ACCOUNT            PIC X(10).
CIDMOD             20  PY-GL-STATE              PIC X(02).
CIDMOD             20  PY-GL-CANC-SW            PIC X(01).
CIDMOD                 88  PY-GL-CANC-SW-ON     VALUE 'Y'.
CIDMOD                 88  PY-GL-CANC-SW-OFF    VALUE 'N'.
CIDMOD             20  PY-GL-COMMENT            PIC X(10).
CIDMOD             20  FILLER      REDEFINES PY-GL-COMMENT.
CIDMOD                 24  PY-GL-CHECK-NO       PIC 9(06).
CIDMOD                 24  FILLER               PIC X(04).
CIDMOD             20  FILLER                   PIC X(07).
00074          16  PY-SAVE-ACCOUNT              PIC X(10).
00075          16  PY-SAVE-TYPE                 PIC X(01).
00076
00077          16  PY-LETTERS.
00078              20  PY-LETTER OCCURS 3 TIMES
00079                            INDEXED BY PY-LET-NDX
00080                                           PIC X(04).
00081
060205         16  PY-ERCOMP-TYPE               PIC X.
060205             88  PY-ACCOUNT-TYPE              VALUE 'A'.
060205             88  PY-GA-TYPE                   VALUE 'G'.
060205             88  PY-BANK-TYPE                 VALUE 'B'.
060205         16  FILLER                       PIC X(05).
00083
00084      12  PY-RECORD-STATUS.
00085          16  PY-CREDIT-SELECT-DT          PIC XX.
00086          16  PY-CREDIT-ACCEPT-DT          PIC XX.
00087          16  PY-BILLED-DATE               PIC XX.
00088          16  PY-REPORTED-DT               PIC XX.
00089          16  PY-PMT-APPLIED               PIC X.
00090              88  PY-ACCOUNT-PMT               VALUE 'A'.
00091              88  PY-GA-PMT                    VALUE 'G'.
00092              88  PY-OVWRITE-PMT               VALUE 'O'.
00093              88  PY-NON-AR-PMT                VALUE 'N'.
00094          16  FILLER                       PIC X(5).
00095          16  PY-INPUT-DT                  PIC XX.
00096          16  PY-CHECK-NUMBER              PIC X(6).
00097          16  PY-VOID-SW                   PIC X.
00098              88  PY-CHECK-VOIDED              VALUE 'V'.
00099          16  PY-CHECK-ORIGIN-SW           PIC X.
00100              88  PY-BILLING-CHECK             VALUE 'B'.
00101              88  PY-REFUND-CHECK              VALUE 'R'.
00102              88  PY-GA-CHECK                  VALUE 'G'.
00103              88  PY-CHECK-WRITTEN             VALUE 'W'.
00104              88  PY-CHECK-REVERSAL            VALUE 'V'.
00105          16  PY-CHECK-WRITTEN-DT          PIC XX.
00106          16  PY-CHECK-QUE-CONTROL         PIC S9(8) COMP.
00107          16  PY-CHECK-QUE-SEQUENCE        PIC S9(4) COMP.
00108          16  PY-BILL-FLAG                 PIC X.
00109              88  PY-BILLED                    VALUE 'B'.
00110          16  PY-AR-FLAG                   PIC X.
00111              88  PY-AR-CYCLE                  VALUE 'C'.
00112              88  PY-AR-MONTH-END              VALUE 'M'.
00113          16  PY-AR-DATE                   PIC XX.
00114
00115      12  PY-GL-CODES.
00116          16  PY-GL-DB                     PIC X(14).
00117          16  PY-GL-CR                     PIC X(14).
00118          16  PY-GL-FLAG                   PIC X.
00119          16  PY-GL-DATE                   PIC XX.
00120
00121      12  PY-CANCEL-FEE-FLAG               PIC X(2).
00122      12  FILLER                           PIC X(3).
00123 ******************************************************************
00345
00346      EJECT
00347 *                            COPY ERCCOMP.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCCOMP                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.019                          *
00006 *                                                                *
00007 *   ONLINE CREDIT SYSTEM                                         *
00008 *                                                                *
00009 *   FILE DESCRIPTION = COMPENSATION MASTER                       *
00010 *                                                                *
00011 *   FILE TYPE = VSAM,KSDS                                        *
00012 *   RECORD SIZE = 700   RECFORM = FIXED                          *
00013 *                                                                *
00014 *   BASE CLUSTER NAME = ERCOMP                   RKP=2,LEN=29    *
00015 *       ALTERNATE PATH = NONE                                    *
00016 *                                                                *
00017 *   LOG = NO                                                     *
00018 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00019 *                                                                *
00020 ******************************************************************
100703*                   C H A N G E   L O G
100703*
100703* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
100703*-----------------------------------------------------------------
100703*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
100703* EFFECTIVE    NUMBER
100703*-----------------------------------------------------------------
100703* 100703    2003080800002  PEMA  ADD SUPERGAP PROCESSING
041105* 041105    2005031100003  PEMA  ADD TYPE CODE FOR BANKS
092205* 092205    2005050300006  PEMA  ADD LEASE FEE
032406* 032406    2006022800001  AJRA  ADD FIRST WRITTEN DATE
072406* 072406    2006022400001  PEMA  ADD REF EDIT FLD ON B RECS
062907* 062907    2004020600003  PEMA  ADD WITHOLDING PERCENT
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
020310* 020310  CR2008100900004  PEMA  ADD REF4 EXTRACT PROCESSING
100703******************************************************************
00021
00022  01  COMPENSATION-MASTER.
00023      12  CO-RECORD-ID                          PIC XX.
00024          88  VALID-CO-ID                          VALUE 'CO'.
00025
00026      12  CO-CONTROL-PRIMARY.
00027          16  CO-COMPANY-CD                     PIC X.
00028          16  CO-CONTROL.
00029              20  CO-CTL-1.
00030                  24  CO-CARR-GROUP.
00031                      28  CO-CARRIER            PIC X.
00032                      28  CO-GROUPING.
00033                          32  CO-GROUP-PREFIX   PIC XXX.
00034                          32  CO-GROUP-PRIME    PIC XXX.
00035                  24  CO-RESP-NO.
00036                      28  CO-RESP-PREFIX        PIC X(4).
00037                      28  CO-RESP-PRIME         PIC X(6).
00038              20  CO-CTL-2.
00039                  24  CO-ACCOUNT.
00040                      28  CO-ACCT-PREFIX        PIC X(4).
00041                      28  CO-ACCT-PRIME         PIC X(6).
00042          16  CO-TYPE                           PIC X.
00043              88  CO-COMPANY-TYPE                  VALUE 'C'.
041105             88  CO-GEN-AGENT-TYPE     VALUE 'G' 'B'.
00045              88  CO-ACCOUNT-TYPE                  VALUE 'A'.
00046
00047      12  CO-MAINT-INFORMATION.
00048          16  CO-LAST-MAINT-DT                  PIC XX.
00049          16  CO-LAST-MAINT-HHMMSS              PIC S9(7)  COMP-3.
00050          16  CO-LAST-MAINT-USER                PIC X(4).
011410     12  FILLER                                PIC XX.
020210     12  CO-STMT-TYPE                          PIC XXX.
011410     12  CO-COMP-TYPE                          PIC X.
011410         88  CO-COMP-IS-SPPDD                    VALUE '1'.
           12  CO-STMT-OWNER                         PIC X(4).
00053      12  CO-BALANCE-CONTROL                    PIC X.
00054          88  CO-CARRY-BALANCE                     VALUE 'Y'.
00055          88  CO-NO-BALANCE                        VALUE 'N'.
00056
00057      12  CO-INTERNAL-CONTROL-1                 PIC X.
00058          88  CO-AUTO-GENERATED-THIS-RUN           VALUE 'X'.
00059          88  CO-AUTO-GENERATED                    VALUE 'Y'.
00060          88  CO-NOT-AUTO-GENERATED                VALUE 'N'.
00061
00062      12  CO-INTERNAL-CONTROL-2                 PIC X.
00063          88  CO-STATEMENT-THIS-RUN                VALUE 'Y'.
00064          88  CO-NO-STATEMENT-THIS-RUN             VALUE 'N'.
00065
062907     12  CO-GA-WITHOLD-PCT                     PIC S9V9999 COMP-3.
062907     12  CO-GA-DIRECT-DEP                      PIC X.
062907     12  CO-FUTURE-SPACE                       PIC X.
062907         88  CO-FUTURE-NOT-USED                   VALUE ' '.
00068
00069      12  CO-ACCT-NAME                          PIC X(30).
00070      12  CO-MAIL-NAME                          PIC X(30).
00071      12  CO-ADDR-1                             PIC X(30).
00072      12  CO-ADDR-2                             PIC X(30).
CIDMOD     12  CO-ADDR-3.
               16  CO-ADDR-CITY                      PIC X(27).
               16  CO-ADDR-STATE                     PIC XX.
CIDMOD     12  CO-CSO-1099                           PIC X.
00074      12  CO-ZIP.
00075          16  CO-ZIP-PRIME.
00076              20  CO-ZIP-PRI-1ST                PIC X.
00077                  88  CO-CANADIAN-POST-CODE  VALUE 'A' THRU 'Z'.
00078              20  FILLER                        PIC X(4).
00079          16  CO-ZIP-PLUS4                      PIC X(4).
00080      12  CO-CANADIAN-POSTAL-CODE  REDEFINES  CO-ZIP.
00081          16  CO-CAN-POSTAL-1                   PIC XXX.
00082          16  CO-CAN-POSTAL-2                   PIC XXX.
00083          16  FILLER                            PIC XXX.
00084      12  CO-SOC-SEC                            PIC X(13).
00085      12  CO-TELEPHONE.
00086          16  CO-AREA-CODE                      PIC XXX.
00087          16  CO-PREFIX                         PIC XXX.
00088          16  CO-PHONE                          PIC X(4).
00089
00090      12  CO-ROLADEX-PRINT-DT                   PIC XX.
00091
00092      12  CO-AR-BAL-LEVEL                       PIC X.
00093          88  CO-AR-REF-LVL                        VALUE '1'.
00094          88  CO-AR-BILL-REF-LVL                   VALUE '1'.
00095          88  CO-AR-BILL-LVL                       VALUE '2'.
00096          88  CO-AR-AGT-LVL                        VALUE '3'.
00097          88  CO-AR-FR-LVL                         VALUE '4'.
00098
00099      12  CO-AR-NORMAL-PRINT                    PIC X.
00100          88  CO-AR-BILL-IS-PRINTED                VALUE 'Y'.
00101          88  CO-AR-BILL-NOT-PRINTED               VALUE 'N'.
00102
00103      12  CO-AR-SUMMARY-CODE                    PIC X(6).
00104
00105      12  CO-AR-REPORTING                       PIC X.
00106          88  CO-AR-NET-REPORT                     VALUE 'N'.
00107          88  CO-AR-GROSS-REPORT                   VALUE 'G'.
00108
00109      12  CO-AR-PULL-CHECK                      PIC X.
00110          88  CO-AR-CHECKS-PULLED                  VALUE 'Y'.
00111          88  CO-AR-CHECKS-NOT-PULLED              VALUE 'N'.
00112
00113      12  CO-AR-BALANCE-PRINT                   PIC X.
00114          88  CO-AR-PRINT-NO-BALANCE               VALUE 'N'.
00115
00116      12  CO-AR-LAST-RUN-CODE                   PIC X.
00117          88  CO-AR-LAST-RUN-ANNUAL                VALUE 'A'.
00118          88  CO-AR-LAST-RUN-CYCLE                 VALUE 'C'.
00119          88  CO-AR-LAST-RUN-EOM                   VALUE 'M'.
00120
00121      12  CO-LAST-EOM-STMT-DT                   PIC XX.
00122
00123      12  CO-USER-CODE                          PIC X.
00124      12  CO-REPORT-GROUP-ID                    PIC X(12).
00125
00126 ******************************************************************
00127 *    FOR A/R USERS THE FOLLOWING FIELDS CONTAIN THE TOTALS AS OF
00128 *    THE LAST MONTH END RUN.
00129 ******************************************************************
00130
00131      12  CO-LAST-ACTIVITY-DATE.
00132          16  CO-ACT-YEAR                       PIC 99.
00133          16  CO-ACT-MONTH                      PIC 99.
00134          16  CO-ACT-DAY                        PIC 99.
00135
00136      12  CO-LAST-STMT-DT.
00137          16  CO-LAST-STMT-YEAR                 PIC 99.
00138          16  CO-LAST-STMT-MONTH                PIC 99.
00139          16  CO-LAST-STMT-DAY                  PIC 99.
00140
00141      12  CO-MO-END-TOTALS.
00142          16  CO-MONTHLY-TOTALS.
00143              20  CO-BAL-FWD                PIC S9(7)V99   COMP-3.
00144              20  CO-CUR-COM                PIC S9(7)V99   COMP-3.
00145              20  CO-CUR-CHG                PIC S9(7)V99   COMP-3.
00146              20  CO-CUR-PMT                PIC S9(7)V99   COMP-3.
00147              20  CO-END-BAL                PIC S9(7)V99   COMP-3.
00148
00149          16  CO-AGING-TOTALS.
00150              20  CO-CUR                    PIC S9(7)V99   COMP-3.
00151              20  CO-OV30                   PIC S9(7)V99   COMP-3.
00152              20  CO-OV60                   PIC S9(7)V99   COMP-3.
00153              20  CO-OV90                   PIC S9(7)V99   COMP-3.
00154
00155          16  CO-YTD-TOTALS.
00156              20  CO-YTD-COM                PIC S9(7)V99   COMP-3.
00157              20  CO-YTD-OV                 PIC S9(7)V99   COMP-3.
00158
00159          16  CO-OVER-UNDER-TOTALS.
00160              20  CO-CUR-OVR-UNDR           PIC S9(7)V99   COMP-3.
00161              20  CO-YTD-OVR-UNDR           PIC S9(7)V99   COMP-3.
00162
00163      12  CO-MISCELLANEOUS-TOTALS.
00164          16  CO-FICA-TOTALS.
00165              20  CO-CUR-FICA               PIC S9(7)V99   COMP-3.
00166              20  CO-YTD-FICA               PIC S9(7)V99   COMP-3.
00167
00168          16  CO-CLAIM-TOTALS.
00169              20  CO-LF-CLM-AMT             PIC S9(9)V99   COMP-3.
00170              20  CO-AH-CLM-AMT             PIC S9(9)V99   COMP-3.
00171
00172 ******************************************************************
00173 *    FOR A/R USERS THE FOLLOWING FIELDS CONTAIN TOTALS THAT
00174 *    REPRESENT CURRENT MONTH (TOTALS OF CYCLES).
00175 ******************************************************************
00176
00177      12  CO-CURRENT-TOTALS.
00178          16  CO-CURRENT-LAST-STMT-DT.
00179              20  CO-CURRENT-LAST-STMT-YEAR     PIC 99.
00180              20  CO-CURRENT-LAST-STMT-MONTH    PIC 99.
00181              20  CO-CURRENT-LAST-STMT-DAY      PIC 99.
00182
00183          16  CO-CURRENT-MONTHLY-TOTALS.
00184              20  CO-CURRENT-BAL-FWD        PIC S9(7)V99   COMP-3.
00185              20  CO-CURRENT-CUR-COM        PIC S9(7)V99   COMP-3.
00186              20  CO-CURRENT-CUR-CHG        PIC S9(7)V99   COMP-3.
00187              20  CO-CURRENT-CUR-PMT        PIC S9(7)V99   COMP-3.
00188              20  CO-CURRENT-END-BAL        PIC S9(7)V99   COMP-3.
00189
00190          16  CO-CURRENT-AGING-TOTALS.
00191              20  CO-CURRENT-CUR            PIC S9(7)V99   COMP-3.
00192              20  CO-CURRENT-OV30           PIC S9(7)V99   COMP-3.
00193              20  CO-CURRENT-OV60           PIC S9(7)V99   COMP-3.
00194              20  CO-CURRENT-OV90           PIC S9(7)V99   COMP-3.
00195
00196          16  CO-CURRENT-YTD-TOTALS.
00197              20  CO-CURRENT-YTD-COM        PIC S9(7)V99   COMP-3.
00198              20  CO-CURRENT-YTD-OV         PIC S9(7)V99   COMP-3.
00199
00200      12  CO-PAID-COMM-TOTALS.
00201          16  CO-YTD-PAID-COMMS.
00202              20  CO-YTD-PAID-COM           PIC S9(7)V99   COMP-3.
00203              20  CO-YTD-PAID-OV            PIC S9(7)V99   COMP-3.
00204
00205      12  CO-CURRENT-MONTH-ACTIVITY         PIC X.
00206          88  CO-HAS-CURR-MONTH-ACTIVITY       VALUE 'Y'.
00207          88  CO-NO-CURR-MONTH-ACTIVITY        VALUE 'N'.
00208
00209      12  CO-DELINQUENT-LETTER-CODE         PIC X.
00210          88  CO-ACCOUNT-1ST-LETTER            VALUE 'A'.
00211          88  CO-ACCOUNT-2ND-LETTER            VALUE 'B'.
00212          88  CO-AGENT-1ST-LETTER              VALUE 'B'.
00213          88  CO-AGENT-2ND-LETTER              VALUE 'G'.
00214          88  CO-OVERWRITE-LETTER              VALUE 'O'.
00215          88  CO-MEMO-TO-REGION-MGR            VALUE 'M'.
00216          88  CO-FINAL-LETTER                  VALUE 'F'.
00217          88  CO-RECONCILING                   VALUE 'R'.
00218          88  CO-PHONE-CALL                    VALUE 'P'.
00219          88  CO-LEGAL                         VALUE 'L'.
00220          88  CO-COLLECTION-AGENCY             VALUE 'C'.
00221          88  CO-WRITE-OFF                     VALUE 'W'.
00222          88  CO-NO-ACTION                     VALUE 'N' ' '.
00223
00224      12  CO-CSR-CODE                       PIC X(4).
00225
00226      12  CO-GA-STATUS-INFO.
00227          16  CO-GA-EFFECTIVE-DT            PIC XX.
00228          16  CO-GA-TERMINATION-DT          PIC XX.
00229          16  CO-GA-STATUS-CODE             PIC X.
00230              88  CO-GA-ACTIVE                 VALUE 'A'.
00231              88  CO-GA-INACTIVE               VALUE 'I'.
00232              88  CO-GA-PENDING                VALUE 'P'.
00233          16  CO-GA-COMMENTS.
00234              20  CO-GA-COMMENT-1           PIC X(40).
00235              20  CO-GA-COMMENT-2           PIC X(40).
00236              20  CO-GA-COMMENT-3           PIC X(40).
00237              20  CO-GA-COMMENT-4           PIC X(40).
00238
00239      12  CO-RPTCD2                         PIC X(10).
00240
00241      12  CO-TYPE-AGENT                     PIC X(01).
00242          88  CO-CORPORATION                   VALUE 'C'.
00243          88  CO-PARTNERSHIP                   VALUE 'P'.
00244          88  CO-SOLE-PROPRIETOR               VALUE 'S'.
00245          88  CO-TRUST                         VALUE 'T'.
00246          88  CO-UNKNOWN                       VALUE ' ' 'X'.
00247
00248      12  CO-FAXNO.
00249          16  CO-FAX-AREA-CODE                  PIC XXX.
00250          16  CO-FAX-PREFIX                     PIC XXX.
00251          16  CO-FAX-PHONE                      PIC X(4).
00252
00253      12  CO-BANK-INFORMATION.
00254          16  CO-BANK-TRANSIT-NO                PIC X(8).
00255          16  CO-BANK-TRANSIT-NON REDEFINES
00256              CO-BANK-TRANSIT-NO                PIC 9(8).
00257
00258          16  CO-BANK-ACCOUNT-NUMBER            PIC X(17).
           12  CO-MISC-DEDUCT-INFO REDEFINES
                        CO-BANK-INFORMATION.
               16  CO-MD-GL-ACCT                     PIC X(10).
               16  CO-MD-DIV                         PIC XX.
               16  CO-MD-CENTER                      PIC X(4).
               16  CO-MD-AMT                        PIC S9(5)V99 COMP-3.
092707         16  CO-CREATE-AP-CHECK                PIC X.
092707         16  CO-DELIVER-CK-TO-MEL              PIC X.
092707         16  FILLER                            PIC XXX.
00259      12  CO-ACH-STATUS                         PIC X.
00260          88  CO-ACH-ACTIVE                         VALUE 'A'.
00261          88  CO-ACH-PENDING                        VALUE 'P'.
00262
CIDMOD     12  CO-BILL-SW                            PIC X.
CIDMOD     12  CO-CONTROL-NAME                       PIC X(30).
092205     12  CO-MAX-BANK-FEE-LEASE                 PIC S999V99 COMP-3.
111504     12  CO-MAX-BANK-FEE                       PIC S999V99 COMP-3.
100703     12  CO-CLP-STATE                          PIC XX.
032406     12  CO-FIRST-WRITTEN-DT                   PIC XX.
072406     12  CO-SPP-REFUND-EDIT                    PIC X.
00264
00265 ******************************************************************
00348
00349      EJECT
00350 *                            COPY ERCCHEK.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCCHEK                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.008                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = CHECK RECORDS                             *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 600    RECFORM = FIXED                         *
00011 *                                                                *
00012 *   BASE CLUSTER NAME = ERCHEK             RKP=2,LEN=35          *
00013 *       ALTERNATE INDEX = NONE                                   *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00017 ******************************************************************
00018  01  CHECK-RECORDS.
00019      12  CH-RECORD-ID                      PIC XX.
00020          88  VALID-CH-ID                      VALUE 'CH'.
00021
00022      12  CH-CONTROL-PRIMARY.
00023          16  CH-COMPANY-CD                 PIC X.
00024          16  CH-CARRIER                    PIC X.
00025          16  CH-GROUPING                   PIC X(6).
00026          16  CH-STATE                      PIC XX.
00027          16  CH-ACCOUNT                    PIC X(10).
00028          16  CH-CERT-EFF-DT                PIC XX.
00029          16  CH-CERT-NO.
00030              20  CH-CERT-PRIME             PIC X(10).
00031              20  CH-CERT-SFX               PIC X.
00032          16  CH-SEQUENCE-NO                PIC S9(4)     COMP.
00033
00034      12  CH-RECORDED-DT                    PIC XX.
00035      12  CH-RECORDED-BY                    PIC X(4).
00036      12  CH-LAST-MAINT-HHMMSS              PIC S9(6)     COMP-3.
00037
00038      12  CH-AMOUNT-PAID                    PIC S9(7)V99  COMP-3.
00039      12  CH-CHECK-NO                       PIC X(7).
00040      12  CH-REASON-FOR-CHECK               PIC X(25).
00041      12  CH-CHECK-WRITTEN-DT               PIC XX.
00042      12  CH-OFFLINE-CHECK-IND              PIC X.
00043          88  MANUAL-CHECK-WRITTEN             VALUE 'Y'.
00044
00045      12  CH-PAYEE-INFO.
00046          16  CH-PAYEE-NAME-1               PIC X(30).
00047          16  CH-PAYEE-NAME-2               PIC X(30).
00048          16  CH-PAYEE-ADDRESS-1            PIC X(30).
00049          16  CH-PAYEE-ADDRESS-2            PIC X(30).
00050          16  CH-PAYEE-CITY-ST              PIC X(30).
00051          16  CH-PAYEE-ZIP-CODE.
00052              20  CH-PAYEE-ZIP.
00053                  24  CH-ZIP-PRI-1ST        PIC X.
00054                      88  CH-CANADIAN-POST-CODE
00055                                            VALUES 'A' THRU 'Z'.
00056                  24  FILLER                PIC X(4).
00057              20  CH-PAYEE-ZIP-EXT          PIC X(4).
00058          16  CH-CANADIAN-POSTAL-CODE REDEFINES CH-PAYEE-ZIP-CODE.
00059              20  CH-CAN-POSTAL-1           PIC XXX.
00060              20  CH-CAN-POSTAL-2           PIC XXX.
00061              20  FILLER                    PIC XXX.
00062
00063      12  CH-CHECK-STUB-TEXT.
00064          16  CH-STUB-LINE-1                PIC X(50).
00065          16  CH-STUB-LINE-2                PIC X(50).
00066          16  CH-STUB-LINE-3                PIC X(50).
00067          16  FILLER.
00068            18  CH-STUB-LINE-4              PIC X(20).
00069            18  CH-LIENHOLDER-NAME          PIC X(30).
00070
00071      12  CH-COMPENSATION-CONTROL.
00072          16  CH-COMP-CARRIER               PIC X.
00073          16  CH-COMP-GROUPING              PIC X(6).
00074          16  CH-COMP-FIN-RESP              PIC X(10).
00075          16  CH-COMP-ACCOUNT               PIC X(10).
00076
00077      12  CH-CREDIT-SELECT-DT               PIC XX.
00078      12  CH-CREDIT-ACCEPT-DT               PIC XX.
00079      12  CH-PAYEE-CODE                     PIC X(6).
00080
00081      12  CH-VOID-DATA.
00082          20  CH-VOID-DT                    PIC XX.
00083          20  CH-VOID-BY                    PIC X(4).
00084          20  CH-VOID-REASON                PIC X(25).
00085
00086      12  CH-CHECK-QUE-CONTROL              PIC S9(8)     COMP.
00087              88  PAYMENT-NOT-QUEUED           VALUE ZERO.
00088      12  CH-CHECK-QUE-SEQUENCE             PIC S9(4)     COMP.
00089
00090      12  CH-CHECK-REFERENCE                PIC X(12).
00091      12  CH-CHECK-ORIGIN-SW                PIC X.
00092              88  CH-REFUND-CHECK              VALUE 'R'.
00093              88  CH-MAINT-CHECK               VALUE 'M'.
00094
00095      12  CH-CANC-DT                        PIC XX.
00096      12  CH-LF-REFUND                      PIC S9(7)V99  COMP-3.
00097      12  CH-AH-REFUND                      PIC S9(7)V99  COMP-3.
00098
00099      12  CH-INSURED-NAME                   PIC X(28).
00100
00101      12  CH-DEDUCT-WITHHELD                PIC S9(5)V99  COMP-3.
00102      12  CH-ADDITIONAL-CHARGE              PIC S9(5)V99  COMP-3.
00103
00104      12  CH-LETTER-TABLE.
00105          16  CH-LETTERS OCCURS 3 TIMES
00106                         INDEXED BY CH-LT-NDX
00107                                            PIC X(04).
00108
00109      12  FILLER                            PIC X(07).
00110
00351
00352      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA CHECK-QUE
                                CONTROL-FILE PENDING-PAY-ADJ
                                COMPENSATION-MASTER CHECK-RECORDS.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL687' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00354
00355      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.
00356
00357 *    NOTE *******************************************************
00358 *         *      ACCESS TO THIS MODULE CAN BE ONLY FROM AN XCTL *
00359 *         *  FROM ANOTHER MODULE.                               *
00360 *         *******************************************************.
00361
00362      MOVE EIBDATE                TO  DC-JULIAN-YYDDD
00363      MOVE '5'                    TO  DC-OPTION-CODE
00364      PERFORM 8500-DATE-CONVERSION
00365      MOVE DC-BIN-DATE-1          TO  WS-CURRENT-DATE
00366
00367      IF EIBCALEN NOT GREATER THAN ZERO
00368          MOVE UNACCESS-MSG       TO  LOGOFF-MSG
00369          GO TO 8300-SEND-TEXT.
00370
00371      
      * EXEC CICS HANDLE CONDITION
00372 *        PGMIDERR   (9600-PGMIDERR)
00373 *        NOTFND     (0180-MAIN-LOGIC)
00374 *        ENDFILE    (0190-MAIN-LOGIC)
00375 *        TERMIDERR  (0900-TERMIDERR)
00376 *        ENQBUSY    (0910-ENQ-BUSY)
00377 *        ERROR      (9990-ERROR) END-EXEC.
      *    MOVE '"$LI''[).              ! " #00003589' TO DFHEIV0
           MOVE X'22244C49275B292E20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303033353839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00378
00379      MOVE +3                     TO  EMI-NUMBER-OF-LINES
00380      MOVE +2                     TO  EMI-SWITCH2.
00381
00382      EJECT
00383  0010-MAIN-LOGIC.
00384      IF PI-CALLING-PROGRAM NOT = WS-PROGRAM-ID
00385          IF PI-RETURN-TO-PROGRAM NOT = WS-PROGRAM-ID
00386              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-6
00387              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-5
00388              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-4
00389              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-3
00390              MOVE PI-SAVED-PROGRAM-1   TO  PI-SAVED-PROGRAM-2
00391              MOVE PI-RETURN-TO-PROGRAM TO  PI-SAVED-PROGRAM-1
00392              MOVE PI-CALLING-PROGRAM   TO  PI-RETURN-TO-PROGRAM
00393              MOVE WS-PROGRAM-ID        TO  PI-CALLING-PROGRAM
00394            ELSE
00395              MOVE PI-RETURN-TO-PROGRAM TO  PI-CALLING-PROGRAM
00396              MOVE PI-SAVED-PROGRAM-1   TO  PI-RETURN-TO-PROGRAM
00397              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-1
00398              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-2
00399              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-3
00400              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-4
00401              MOVE PI-SAVED-PROGRAM-6   TO  PI-SAVED-PROGRAM-5
00402              MOVE SPACES               TO  PI-SAVED-PROGRAM-6
00403        ELSE
00404          GO TO 0040-MAIN-LOGIC.
00405
00406  0015-MAIN-LOGIC.
00407 *    NOTE *******************************************************
00408 *         *      INITIALIZE THE WORK FIELDS FOR THE PROGRAM     *
00409 *         *  INTERFACE BLOCK FOR THIS MODULE.                   *
00410 *         *******************************************************.
00411
00412      MOVE SPACES                 TO  PI-PROGRAM-WORK-AREA.
00413
00414      MOVE ZEROS                  TO  PI-NUMBER-OF-CONTROL-GROUPS
00415                                      PI-NUMBER-OF-ALIGNMENT-CHECKS
00416                                      PI-ALIGNMENT-CONTROL-GROUP
00417                                      PI-ALIGNMENT-SEQUENCE-NO
00418                                      PI-PROCESSING-SW
00419                                      PI-CONTROL-GROUP (1)
00420                                      PI-CONTROL-GROUP (2)
00421                                      PI-CONTROL-GROUP (3)
00422                                      PI-CONTROL-GROUP (4)
00423                                      PI-HIGH-SEQUENCE (1)
00424                                      PI-HIGH-SEQUENCE (2)
00425                                      PI-HIGH-SEQUENCE (3)
00426                                      PI-HIGH-SEQUENCE (4)
00427                                      PI-COMPANY-ZIP-CODE
00428                                      PI-COMPANY-PHONE-NUMBER.
00429
00430      MOVE WS-PRINTER-STARTED-SW  TO  PI-PRINTER-STARTED-SW.
00431      MOVE WS-TEMP-STORAGE-KEY    TO  PI-TEMP-STORAGE-KEY.
00432
00433      MOVE LOW-VALUES             TO  EL687AI.
00434      MOVE -1                     TO  AOPTIONL
00435
00436      
      * EXEC CICS ASSIGN
00437 *        SYSID (PI-SYSID)
00438 *        END-EXEC.
      *    MOVE '"(         $          "   #00003654' TO DFHEIV0
           MOVE X'222820202020202020202024' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202220' TO DFHEIV0(13:12)
           MOVE X'2020233030303033363534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SYSID
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00439
00440      PERFORM 8100-SEND-INITIAL-MAP.
00441
00442      EJECT
00443  0040-MAIN-LOGIC.
00444 *    NOTE *******************************************************
00445 *         *      AFTER THE FIRST TIME THROUGH THE PROPER ATTEN- *
00446 *         *  TION KEY USAGE NEEDS TO BE CHECKED FOR VALIDITY    *
00447 *         *  BEFORE ANY FURTHER PROCESSING CAN BE DONE.         *
00448 *         *******************************************************.
00449
00450      IF EIBAID = DFHCLEAR
00451          GO TO 9400-CLEAR.
00452
00453      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00454          MOVE LOW-VALUES         TO  EL687AI
00455          MOVE ER-0008            TO  EMI-ERROR
00456          MOVE -1                 TO  APFKL
00457          PERFORM 8200-SEND-DATAONLY.
00458
00459      
      * EXEC CICS RECEIVE
00460 *        INTO   (EL687AI)
00461 *        MAPSET (WS-MAPSET-NAME)
00462 *        MAP    (WS-MAP-NAME) END-EXEC.
           MOVE LENGTH OF
            EL687AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00003677' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033363737' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL687AI, 
                 DFHEIV11, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00463
00464      IF APFKL IS GREATER THAN ZERO
00465          IF EIBAID NOT = DFHENTER
00466              MOVE ER-0004        TO  EMI-ERROR
00467              MOVE AL-UNBOF       TO  APFKA
00468              MOVE -1             TO  APFKL
00469              PERFORM 8200-SEND-DATAONLY
00470            ELSE
00471              IF APFKO GREATER ZERO AND LESS '25'
00472                  MOVE PF-VALUES (APFKI)  TO  EIBAID
00473                ELSE
00474                  MOVE ER-0029        TO  EMI-ERROR
00475                  MOVE AL-UNBOF       TO  APFKA
00476                  MOVE -1             TO  APFKL
00477                  PERFORM 8200-SEND-DATAONLY.
00478
00479      IF EIBAID IS = DFHPF12
00480          MOVE 'EL010'            TO  WS-PROGRAM-ID
00481          GO TO 9300-XCTL.
00482
00483      IF EIBAID IS = DFHPF23
00484          GO TO 9000-RETURN-CICS.
00485
00486      IF EIBAID IS = DFHPF24
00487          MOVE 'EL126'            TO  WS-PROGRAM-ID
00488          GO TO 9300-XCTL.
00489
00490      IF EIBAID NOT = DFHENTER
00491          MOVE ER-0008            TO  EMI-ERROR
00492          MOVE -1                 TO  APFKL
00493          PERFORM 8200-SEND-DATAONLY.
00494
00495      IF PI-PROCESSING-SW NOT = ZERO
00496          GO TO 0240-MAIN-LOGIC.
00497
00498      EJECT
00499  0100-MAIN-LOGIC.
00500 *    NOTE *******************************************************
00501 *         *          SYNTAX CHECK THE MAP FIELDS                *
00502 *         *******************************************************.
00503
00504      IF AOPTIONL NOT GREATER THAN ZERO
00505          MOVE -1                 TO  AOPTIONL
00506          MOVE AL-UNBON           TO  AOPTIONA
00507          MOVE ER-0002            TO  EMI-ERROR
00508          PERFORM 8200-SEND-DATAONLY.
00509
00510      IF (AOPTIONI GREATER ZERO AND LESS '4')
00511        OR
00512          (PI-COMPANY-ID = 'POS' AND
00513           AOPTIONI = ('2' OR '3'))
00514              MOVE AL-UNNON           TO  AOPTIONA
00515            ELSE
00516              MOVE -1                 TO  AOPTIONL
00517              MOVE AL-UNBON           TO  AOPTIONA
00518              MOVE ER-0330            TO  EMI-ERROR
00519              PERFORM 9900-ERROR-FORMAT.
00520
00521      IF AALIGNL GREATER THAN ZERO
00522          IF AALIGNI IS NUMERIC
00523              MOVE AALIGNO  TO  PI-NUMBER-OF-ALIGNMENT-CHECKS
00524              MOVE AL-UNNON       TO  AALIGNA
00525            ELSE
00526              MOVE ER-0365        TO  EMI-ERROR
00527              MOVE -1             TO  AALIGNL
00528              MOVE AL-UNBON       TO  AALIGNA
00529              PERFORM 9900-ERROR-FORMAT.
00530
00531      IF ACKNOL GREATER THAN ZERO
00532          IF ACKNOI IS NUMERIC
00533              MOVE AL-UNNON       TO  ACKNOA
00534              MOVE ACKNOI         TO  WS-CHECK-NUMBER-X
00535            ELSE
00536              MOVE ER-0366        TO  EMI-ERROR
00537              MOVE -1             TO  ACKNOL
00538              MOVE AL-UNBON       TO  ACKNOA
00539              PERFORM 9900-ERROR-FORMAT
00540        ELSE
00541          MOVE ZERO               TO  WS-CHECK-NUMBER.
00542
00543      IF AACNL GREATER THAN ZERO
00544          MOVE AACNI              TO  PI-ASSIGN-CHECK-NUMBERS
00545          IF AACNI = ('Y' OR 'N')
00546              MOVE AL-UANON       TO  AACNA
00547              MOVE AACNI         TO  PI-ENTRY-CD-1
00548            ELSE
00549              MOVE AL-UABON       TO  AACNA
00550              MOVE -1             TO  AACNL
00551              MOVE ER-0367        TO  EMI-ERROR
00552              PERFORM 9900-ERROR-FORMAT
00553        ELSE
00554          MOVE AL-UABOF           TO  AACNA
00555          MOVE -1                 TO  AACNL
00556          MOVE ER-0368            TO  EMI-ERROR
00557          PERFORM 9900-ERROR-FORMAT.
00558
00559      IF AACNI = 'Y'
00560        AND ACKNOL NOT GREATER THAN ZERO
00561          MOVE -1                 TO  ACKNOL
00562          MOVE AL-UNBOF           TO  ACKNOA
00563          MOVE ER-0392            TO  EMI-ERROR
00564          PERFORM 9900-ERROR-FORMAT.
00565
00566      IF AACNI = 'N'
00567        AND ACKNOL GREATER THAN ZERO
00568          MOVE -1                 TO  ACKNOL
00569          MOVE AL-UNBON           TO  ACKNOA
00570          MOVE ER-0393            TO  EMI-ERROR
00571          PERFORM 9900-ERROR-FORMAT.
00572
00573      EJECT
00574 *    NOTE *******************************************************
00575 *         *      CHECK THE VALIDITY OF ANY CONTROL GROUPS       *
00576 *         *  ENTERED.                                           *
00577 *         *******************************************************.
00578
00579      SET EL687A-INDEX
00580          PI-INDEX TO +1.
00581
00582  0120-MAIN-LOGIC.
00583      IF EL687A-CONTROL-GROUP-LENGTH (EL687A-INDEX)
00584                                  NOT GREATER THAN ZERO
00585          MOVE AL-UNNOF  TO  EL687A-CONTROL-GROUP-ATTRB
00586                                                     (EL687A-INDEX)
00587          GO TO 0190-MAIN-LOGIC.
00588
00589      IF EL687A-CONTROL-GROUP (EL687A-INDEX) IS NOT NUMERIC
00590          MOVE AL-UNBON  TO  EL687A-CONTROL-GROUP-ATTRB
00591                                                     (EL687A-INDEX)
00592          MOVE -1  TO  EL687A-CONTROL-GROUP-LENGTH (EL687A-INDEX)
00593          MOVE ER-0369            TO  EMI-ERROR
00594          PERFORM 9900-ERROR-FORMAT
00595          GO TO 0190-MAIN-LOGIC.
00596
00597      MOVE EL687A-CONTROL-GROUP (EL687A-INDEX)
00598                                  TO  PI-CONTROL-GROUP (PI-INDEX)
00599      SET PI-INDEX UP BY +1
00600      MOVE AL-UNNON  TO  EL687A-CONTROL-GROUP-ATTRB (EL687A-INDEX)
00601
00602      IF PI-INDEX IS GREATER THAN +2
00603        AND PI-CONTROL-GROUP (PI-INDEX - 2)
00604                      NOT LESS THAN PI-CONTROL-GROUP (PI-INDEX - 1)
00605          MOVE ER-0385            TO  EMI-ERROR
00606          PERFORM 9900-ERROR-FORMAT
00607          MOVE -1 TO EL687A-CONTROL-GROUP-LENGTH (EL687A-INDEX)
00608          MOVE AL-UNBON TO EL687A-CONTROL-GROUP-ATTRB
00609                                                    (EL687A-INDEX).
00610
00611      MOVE ZERO                   TO  WS-NOT-FOUND
00612
00613      MOVE LOW-VALUES             TO  WS-CHECK-QUEUE-KEY
00614
00615      MOVE PI-COMPANY-CD          TO  WS-CQK-COMPANY-CD
00616      MOVE EL687A-CONTROL-GROUP (EL687A-INDEX)
00617                                  TO  WS-CQK-CONTROL-NUMBER
00618
00619      IF WS-CHECK-QUEUE-BROWSE-SW = ZERO
00620          
      * EXEC CICS STARTBR
00621 *            DATASET   (WS-CHECK-QUEUE-DSID)
00622 *            RIDFLD    (WS-CHECK-QUEUE-KEY)
00623 *            GENERIC   EQUAL
00624 *            KEYLENGTH (5) END-EXEC
           MOVE 5
             TO DFHEIV11
           MOVE 0
             TO DFHEIV12
      *    MOVE '&,   KG    E          &   #00003838' TO DFHEIV0
           MOVE X'262C2020204B472020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383338' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-DSID, 
                 WS-CHECK-QUEUE-KEY, 
                 DFHEIV11, 
                 DFHEIV12, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00625          MOVE +1                 TO  WS-CHECK-QUEUE-BROWSE-SW
00626        ELSE
00627      
      * EXEC CICS RESETBR
00628 *        DATASET   (WS-CHECK-QUEUE-DSID)
00629 *        RIDFLD    (WS-CHECK-QUEUE-KEY)
00630 *        GENERIC   EQUAL
00631 *        KEYLENGTH (5) END-EXEC.
           MOVE 5
             TO DFHEIV11
           MOVE 0
             TO DFHEIV12
      *    MOVE '&4   KG    E          &   #00003845' TO DFHEIV0
           MOVE X'26342020204B472020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383435' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-DSID, 
                 WS-CHECK-QUEUE-KEY, 
                 DFHEIV11, 
                 DFHEIV12, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00632
00633  0130-MAIN-LOGIC.
00634      
      * EXEC CICS READNEXT
00635 *        DATASET (WS-CHECK-QUEUE-DSID)
00636 *        RIDFLD  (WS-CHECK-QUEUE-KEY)
00637 *        SET     (ADDRESS OF CHECK-QUE) END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00003852' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CHECK-QUEUE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CHECK-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00638
00639      IF WS-CQK-COMPANY-CD NOT = PI-COMPANY-CD
00640          GO TO 0170-MAIN-LOGIC.
00641
00642      IF WS-CQK-CONTROL-NUMBER NOT = EL687A-CONTROL-GROUP
00643                                                     (EL687A-INDEX)
00644          GO TO 0170-MAIN-LOGIC.
00645
00646      IF CQ-ENTRY-TYPE NOT = 'Q'
00647          GO TO 0130-MAIN-LOGIC.
00648
00649      IF PI-COMPANY-ID = 'POS'
00650          MOVE CQ-PYAJ-CARRIER    TO  PI-CARRIER.
00651
00652      IF AOPTIONI = '2'
00653        AND CQ-TIMES-PRINTED NOT GREATER THAN ZERO
00654          GO TO 0190-MAIN-LOGIC.
00655
00656      IF AOPTIONI = '3'
00657        AND CQ-TIMES-PRINTED GREATER THAN ZERO
00658          GO TO 0190-MAIN-LOGIC.
00659
00660      GO TO 0130-MAIN-LOGIC.
00661
00662  0170-MAIN-LOGIC.
00663      MOVE ER-0394                TO  EMI-ERROR
00664      PERFORM 9900-ERROR-FORMAT
00665      MOVE -1 TO EL687A-CONTROL-GROUP-LENGTH (EL687A-INDEX)
00666      MOVE AL-UNBON TO EL687A-CONTROL-GROUP-ATTRB (EL687A-INDEX)
00667
00668      GO TO 0190-MAIN-LOGIC.
00669
00670  0180-MAIN-LOGIC.
00671      MOVE ER-0387                TO  EMI-ERROR
00672      PERFORM 9900-ERROR-FORMAT
00673      MOVE -1 TO EL687A-CONTROL-GROUP-LENGTH (EL687A-INDEX)
00674      MOVE AL-UNBON TO EL687A-CONTROL-GROUP-ATTRB (EL687A-INDEX).
00675
00676  0190-MAIN-LOGIC.
00677      IF EL687A-INDEX LESS THAN +4
00678          SET EL687A-INDEX UP BY +1
00679          GO TO 0120-MAIN-LOGIC.
00680
00681      IF WS-CHECK-QUEUE-BROWSE-SW NOT = ZERO
00682          MOVE ZERO               TO  WS-CHECK-QUEUE-BROWSE-SW
00683          
      * EXEC CICS ENDBR
00684 *            DATASET (WS-CHECK-QUEUE-DSID) END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00003901' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393031' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00685
00686      IF EMI-FATAL-CTR GREATER THAN ZERO
00687          PERFORM 8200-SEND-DATAONLY.
00688
00689      IF AOPTIONI = ('2' OR '3')
00690        AND PI-INDEX NOT GREATER THAN +1
00691          MOVE -1                 TO  ACG01L
00692          MOVE AL-UNBOF  TO  ACG01A ACG02A ACG03A ACG04A
00693          MOVE ER-0370            TO  EMI-ERROR
00694          PERFORM 9900-ERROR-FORMAT.
00695
00696      IF PI-INDEX GREATER THAN +1
00697          NEXT SENTENCE
00698        ELSE
00699          GO TO 0200-MAIN-LOGIC.
00700
00701      SET PI-INDEX DOWN BY +1
00702      SET PI-NUMBER-OF-CONTROL-GROUPS TO PI-INDEX
00703      SET PI-INDEX
00704          EL687A-INDEX TO +1.
00705
00706  0195-MAIN-LOGIC.
00707      IF PI-CONTROL-GROUP (PI-INDEX) GREATER THAN ZERO
00708          MOVE PI-CONTROL-GROUP (PI-INDEX)
00709                      TO  EL687A-CONTROL-GROUP (EL687A-INDEX)
00710          MOVE AL-UNNON TO EL687A-CONTROL-GROUP-ATTRB
00711                                                     (EL687A-INDEX)
00712        ELSE
00713          MOVE WS-INIT-CONTROL-GROUP
00714                      TO  EL687A-CONTROL-GROUP (EL687A-INDEX)
00715          MOVE AL-UNNOF TO EL687A-CONTROL-GROUP-ATTRB
00716                                                    (EL687A-INDEX).
00717
00718      IF PI-INDEX LESS THAN +4
00719          SET PI-INDEX
00720              EL687A-INDEX UP BY +1
00721          GO TO 0195-MAIN-LOGIC.
00722
00723      SET EL687A-INDEX
00724          PI-INDEX TO +1.
00725      EJECT
00726  0200-MAIN-LOGIC.
00727 *    NOTE *******************************************************
00728 *         *      ALL OF THE SYNTAX CHECKS HAVE BEEN SUCCESSFUL- *
00729 *         *  NOW DO THE PRE-EDIT.                               *
00730 *         *                                                     *
00731 *         *      BEFORE A CHECK BATCH IS QUEUED FOR PRINT, A    *
00732 *         *  PRE-EDIT IS DONE TO ASSURE CONSISTENCY IN          *
00733 *         *  PROCESSING.  THIS EDIT CONSISTS OF THE FOLLOWING   *
00734 *         *  STEPS:                                             *
00735 *         *                                                     *
00736 *         *  1. IF A STARTING CHECK NUMBER HAS BEEN ENTERED,    *
00737 *         *     THE NUMBER IS COMPARED TO OTHER NUMBERS IN THE  *
00738 *         *     FILE FOR OVERLAPS AND GAPS.                     *
00739 *         *                                                     *
00740 *         *  2. IF ANY CHECKS IN THE RELEASED GROUPS HAVE       *
00741 *         *     ALREADY BEEN QUEUED FOR PRINT OR PRINTED.       *
00742 *         *                                                     *
00743 *         *  3. IF PRE-NUMBERING IS NOT USED THAT ALL CHECKS    *
00744 *         *     HAVE A CHECK NUMBER ASSIGNED.                   *
00745 *         *                                                     *
00746 *         *  4. IF DUPLICATE CHECK NUMBERS ARE ASSIGNED.        *
00747 *         *                                                     *
00748 *         *      BEFORE A CHECK BATCH IS QUEUED FOR RE-PRINT, A *
00749 *         *  PRE-EDIT IS DONE TO ASSURE CONSISTENCY IN          *
00750 *         *  PROCESSING.  THIS EDIT CONSISTS OF THE FOLLOWING   *
00751 *         *  STEPS:                                             *
00752 *         *                                                     *
00753 *         *  1. ALL CHECKS IN THE INDICATED GROUP(S) MUST HAVE  *
00754 *         *     BEEN PREVIOUSLY PRINTED.                        *
00755 *         *                                                     *
00756 *         *  2. IF THE PRE-NUMBERING SWITCH IS SET IN ANY RECORD*
00757 *         *     IT MUST BE SET IN ALL RECORDS.                  *
00758 *         *                                                     *
00759 *         *  3. IF A STARTING CHECK NUMBER HAS BEEN ENTERED,    *
00760 *         *     THE NUMBER IS COMPARED TO OTHER NUMBERS IN THE  *
00761 *         *     FILE FOR OVERLAPS AND GAPS.                     *
00762 *         *******************************************************.
00763
00764      MOVE PI-COMPANY-ID          TO  WS-ENQ-COMPANY-ID.
00765
00766      
      * EXEC CICS ENQ
00767 *        RESOURCE (WS-CHECK-QUEUE-DSID)
00768 *        LENGTH   (11) END-EXEC.
           MOVE 11
             TO DFHEIV11
      *    MOVE '2$L                   $   #00003984' TO DFHEIV0
           MOVE X'32244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393834' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00769
00770      
      * EXEC CICS HANDLE CONDITION
00771 *        NOTFND  (0225-MAIN-LOGIC)
00772 *        ENDFILE (0230-MAIN-LOGIC) END-EXEC.
      *    MOVE '"$I''                  ! # #00003988' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303033393838' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00773
00774      MOVE LOW-VALUES             TO  WS-CHECK-QUEUE-KEY.
00775      MOVE +5                     TO  WS-KEY-LENGTH.
00776
00777      MOVE PI-COMPANY-CD          TO  WS-CQK-COMPANY-CD.
00778
00779      
      * EXEC CICS STARTBR
00780 *        DATASET (WS-CHECK-QUEUE-DSID)
00781 *        RIDFLD  (WS-CHECK-QUEUE-KEY)
00782 *        GTEQ    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00003997' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393937' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-DSID, 
                 WS-CHECK-QUEUE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00783
00784  0210-MAIN-LOGIC.
00785      MOVE EMI-FATAL-CTR         TO  WS-LAST-ERROR-COUNT.
00786
00787      
      * EXEC CICS READNEXT
00788 *        DATASET (WS-CHECK-QUEUE-DSID)
00789 *        RIDFLD  (WS-CHECK-QUEUE-KEY)
00790 *        SET     (ADDRESS OF CHECK-QUE) END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00004005' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303034303035' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CHECK-QUEUE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CHECK-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00791
00792      IF CQ-COMPANY-CD NOT = PI-COMPANY-CD
00793          GO TO 0230-MAIN-LOGIC.
00794
00795 *    IF CQ-ENTRY-TYPE NOT = 'Q'
00796 *        GO TO 0210-MAIN-LOGIC.
00797
00798      IF  (AOPTIONI = '1' OR '2')
00799              AND
00800          CQ-ENTRY-TYPE NOT = 'Q'
00801          GO TO 0210-MAIN-LOGIC.
00802
00803 *    NOTE *******************************************************
00804 *         *      SAVE THE CHECK NUMBER SO AT THE END OF THE     *
00805 *         *  BROWSE YOU CAN CHECK FOR GAPS OR OVERLAPS.         *
00806 *         *******************************************************.
00807
00808      IF CQ-CHECK-NUMBER GREATER THAN WS-GREATEST-CHECK-NUMBER-X
00809          MOVE CQ-CHECK-NUMBER    TO  WS-GREATEST-CHECK-NUMBER-X.
00810
00811 *    NOTE *******************************************************
00812 *         *      IF YOU ARE PROCESSING BY GROUPS BYPASS ALL     *
00813 *         *  RECORDS IF NOT IN SPECIFIED GROUPS.  SAVE THE HIGH *
00814 *         *  SEQUENCE NUMBER IN EACH GROUP FOR REPRINT.         *
00815 *         *******************************************************.
00816
00817      IF AOPTIONI = '2' OR '3'
00818          NEXT SENTENCE
00819        ELSE
00820          GO TO 0213-MAIN-LOGIC.
00821
00822      SET PI-INDEX
00823          EL687A-INDEX TO +1.
00824
00825  0212-MAIN-LOGIC.
00826      IF CQ-CONTROL-NUMBER  = PI-CONTROL-GROUP (PI-INDEX)
00827          IF CQ-SEQUENCE-NUMBER GREATER PI-HIGH-SEQUENCE (PI-INDEX)
00828              MOVE CQ-SEQUENCE-NUMBER
00829                                  TO PI-HIGH-SEQUENCE (PI-INDEX)
00830              GO TO 0215-MAIN-LOGIC
00831            ELSE
00832              GO TO 0215-MAIN-LOGIC.
00833
00834      IF PI-INDEX LESS THAN +4
00835          SET PI-INDEX
00836              EL687A-INDEX UP BY +1
00837          GO TO 0212-MAIN-LOGIC.
00838
00839      GO TO 0210-MAIN-LOGIC.
00840
00841  0213-MAIN-LOGIC.
00842 *    NOTE *******************************************************
00843 *         *      IF YOU ARE PRINTING ALL CONTROL GROUPS BYPASS  *
00844 *         *  THE CONTROL GROUPS THAT HAVE ALREADY BEEN PRINTED. *
00845 *         *******************************************************.
00846
00847      IF CQ-CONTROL-NUMBER NOT = WS-LAST-CONTROL-GROUP
00848          MOVE CQ-CONTROL-NUMBER  TO  WS-LAST-CONTROL-GROUP
00849          MOVE CQ-TIMES-PRINTED   TO  WS-TIMES-PRINTED.
00850
00851      IF WS-TIMES-PRINTED GREATER THAN ZERO
00852          GO TO 0210-MAIN-LOGIC.
00853
00854      EJECT
00855  0215-MAIN-LOGIC.
00856
CIDMOD*    IF  AOPTIONI = '3'
CIDMOD     IF AOPTIONI = '1' OR '2'
00858          GO TO 0220-MAIN-LOGIC.
00859
00860      IF CQ-TIMES-PRINTED GREATER THAN ZERO
00861         MOVE -1 TO EL687A-CONTROL-GROUP-LENGTH (EL687A-INDEX)
00862         MOVE AL-UNBON TO EL687A-CONTROL-GROUP-ATTRB (EL687A-INDEX)
00863         MOVE ER-0379             TO  EMI-ERROR
00864         PERFORM 9900-ERROR-FORMAT.
00865
00866      IF AACNI = 'Y'
00867        AND CQ-CHECK-NUMBER NOT = SPACES
00868          MOVE ER-0382            TO  EMI-ERROR
00869          PERFORM 9900-ERROR-FORMAT
00870          MOVE -1                 TO  AACNL
00871          MOVE AL-UABON           TO  AACNA
00872      ELSE
00873          IF AACNI = 'N'
00874            AND CQ-CHECK-NUMBER = SPACES
00875              MOVE -1             TO  AACNL
00876              MOVE AL-UABON       TO  AACNA
00877              MOVE ER-0383        TO  EMI-ERROR
00878              PERFORM 9900-ERROR-FORMAT.
00879
00880      GO TO 0210-MAIN-LOGIC.
00881
00882  0220-MAIN-LOGIC.
00883
00884      IF  CQ-ENTRY-TYPE NOT = 'Q'
00885          GO TO 0210-MAIN-LOGIC.
00886
00887      IF CQ-TIMES-PRINTED NOT GREATER THAN ZERO
00888         MOVE -1 TO EL687A-CONTROL-GROUP-LENGTH (EL687A-INDEX)
00889         MOVE AL-UNBON TO EL687A-CONTROL-GROUP-ATTRB (EL687A-INDEX)
00890         MOVE ER-0389             TO  EMI-ERROR
00891         PERFORM 9900-ERROR-FORMAT.
00892
00893      IF CQ-PRE-NUMBERING-SW = '1'
00894        AND AACNI = 'N'
00895          MOVE -1                 TO  AACNL
00896          MOVE AL-UABON           TO  AACNA
00897          MOVE ER-0390            TO  EMI-ERROR
00898          PERFORM 9900-ERROR-FORMAT.
00899
00900      IF CQ-PRE-NUMBERING-SW = SPACES
00901        AND AACNI = 'Y'
00902          MOVE -1                 TO  AACNL
00903          MOVE AL-UABON           TO  AACNA
00904          MOVE ER-0391            TO  EMI-ERROR
00905          PERFORM 9900-ERROR-FORMAT.
00906
00907      GO TO 0210-MAIN-LOGIC.
00908
00909  0225-MAIN-LOGIC.
00910      MOVE ER-0490                TO  EMI-ERROR
00911      MOVE -1                     TO  AOPTIONL
00912      PERFORM 8200-SEND-DATAONLY
00913      PERFORM 9100-RETURN-TRAN.
00914
00915      EJECT
00916  0230-MAIN-LOGIC.
00917      
      * EXEC CICS ENDBR
00918 *        DATASET (WS-CHECK-QUEUE-DSID) END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00004136' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00919
00920      IF EMI-FATAL-CTR GREATER THAN ZERO
00921          PERFORM 8200-SEND-DATAONLY.
00922
00923 *    NOTE *******************************************************
00924 *         *      READ THE COMPANY RECORD FROM THE CONTROL FILE  *
00925 *         *  TO GET THE CICS/VS PRINTER TERMINAL ID AND CHECK   *
00926 *         *  TO SEE IF THE PRINTER HAS BEEN SPECIFIED.          *
00927 *         *******************************************************.
00928
00929      MOVE PI-COMPANY-ID          TO  WS-CONTROL-FILE-KEY.
00930      MOVE '1'                    TO  WS-CFK-RECORD-TYPE.
00931      MOVE ZERO                   TO  WS-CFK-SEQUENCE-NO.
00932
00933      
      * EXEC CICS READ
00934 *        DATASET (WS-CONTROL-FILE-DSID)
00935 *        RIDFLD  (WS-CONTROL-FILE-KEY)
00936 *        SET    (ADDRESS OF CONTROL-FILE) END-EXEC.
      *    MOVE '&"S        E          (   #00004152' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CONTROL-FILE-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CONTROL-FILE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00937
00938      IF CF-CHECK-PRINTER-ID = SPACES
00939          MOVE ER-0371            TO  EMI-ERROR
00940          MOVE -1                 TO  APFKL
00941          PERFORM 9900-ERROR-FORMAT.
00942
00943      MOVE SPACES                 TO  PI-ALT-DMD-PRT-ID.
00944      IF APRTL GREATER THAN ZEROS
00945          MOVE AL-UANON           TO  APRTA
00946          MOVE APRTI              TO  PI-CHECK-PRINTER-ID
00947                                      PI-ALT-DMD-PRT-ID
00948      ELSE
00949          MOVE CF-CHECK-PRINTER-ID
00950                                  TO  PI-CHECK-PRINTER-ID.
00951
00952      MOVE CF-COMPANY-ADDRESS     TO  PI-COMPANY-ADDRESS.
00953
00954      IF AACNI = 'Y'
00955          IF WS-GREATEST-CHECK-NUMBER-X NOT LESS THAN ACKNOI
00956              MOVE ER-0380        TO  EMI-ERROR
00957              PERFORM 9900-ERROR-FORMAT
00958              MOVE -1             TO  ACKNOL
00959              MOVE AL-UNBON       TO  ACKNOA
00960          ELSE
00961              SUBTRACT +1 FROM ACKNOO GIVING WS-ACKNO
00962              IF WS-GREATEST-CHECK-NUMBER-X NOT = WS-ACKNO-X
00963                  MOVE ER-0381    TO  EMI-ERROR
00964                  PERFORM 9900-ERROR-FORMAT
00965                  MOVE -1         TO  ACKNOL
00966                  MOVE AL-UNBON   TO  ACKNOA
00967              ELSE
00968                  MOVE AL-SANON   TO  ACKNOA.
00969
00970      IF EMI-FATAL-CTR GREATER THAN ZERO
00971          PERFORM 8200-SEND-DATAONLY.
00972
00973      MOVE AL-SANON               TO  AOPTIONA
00974                                      ACG01A
00975                                      ACG02A
00976                                      ACG03A
00977                                      ACG04A
00978                                      AACNA
00979                                      APRTA.
00980
00981      MOVE +1                     TO  PI-PROCESSING-SW
00982                                      AALIGNL.
00983
00984      IF PI-NUMBER-OF-ALIGNMENT-CHECKS GREATER THAN ZERO
00985          MOVE ER-0361            TO  EMI-ERROR
00986        ELSE
00987          MOVE ER-0362            TO  EMI-ERROR.
00988
00989      PERFORM 8200-SEND-DATAONLY.
00990
00991      
      * EXEC CICS HANDLE AID
00992 *        CLEAR (9400-CLEAR)
00993 *        PA1   (0040-MAIN-LOGIC)
00994 *        PA2   (0040-MAIN-LOGIC)
00995 *        PA3   (0040-MAIN-LOGIC) END-EXEC.
      *    MOVE '"&=!"#               V! $ #00004210' TO DFHEIV0
           MOVE X'22263D212223202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020562120' TO DFHEIV0(13:12)
           MOVE X'2420233030303034323130' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00996
00997      
      * EXEC CICS SYNCPOINT
00998 *        END-EXEC.
      *    MOVE '6"                    !   #00004216' TO DFHEIV0
           MOVE X'362220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00999
01000      GO TO 0040-MAIN-LOGIC.
01001
01002      EJECT
01003  0240-MAIN-LOGIC.
01004      IF AALIGNL GREATER THAN ZERO
01005          IF AALIGNI IS NUMERIC
01006              MOVE AALIGNO  TO  PI-NUMBER-OF-ALIGNMENT-CHECKS
01007              MOVE AL-UNNON       TO  AALIGNA
01008            ELSE
01009              MOVE ER-0365        TO  EMI-ERROR
01010              MOVE -1             TO  AALIGNL
01011              MOVE AL-UNBON       TO  AALIGNA
01012              PERFORM 8200-SEND-DATAONLY
01013              GO TO 0040-MAIN-LOGIC.
01014
01015      IF PI-NUMBER-OF-ALIGNMENT-CHECKS NOT GREATER THAN ZERO
01016          GO TO 0300-MAIN-LOGIC.
01017
01018      IF PI-ALIGNMENT-CONTROL-GROUP GREATER THAN ZERO
01019          GO TO 0245-MAIN-LOGIC.
01020
01021      MOVE PI-COMPANY-ID          TO  WS-CONTROL-FILE-KEY.
01022      MOVE '1'                    TO  WS-CFK-RECORD-TYPE.
01023      MOVE ZERO                   TO  WS-CFK-SEQUENCE-NO.
01024
01025      
      * EXEC CICS READ UPDATE
01026 *        DATASET (WS-CONTROL-FILE-DSID)
01027 *        RIDFLD  (WS-CONTROL-FILE-KEY)
01028 *        SET    (ADDRESS OF CONTROL-FILE) END-EXEC.
      *    MOVE '&"S        EU         (   #00004244' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CONTROL-FILE-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CONTROL-FILE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01029
01030      ADD +1  TO  CF-CR-CHECK-QUE-COUNTER.
01031
01032      IF CR-QUE-COUNT-RESET
01033          MOVE +1                 TO  CF-CR-CHECK-QUE-COUNTER.
01034
01035      MOVE CF-CR-CHECK-QUE-COUNTER TO PI-ALIGNMENT-CONTROL-GROUP.
01036
01037      MOVE CONTROL-FILE           TO  JP-RECORD-AREA.
01038      MOVE WS-CONTROL-FILE-DSID   TO  JP-FILE-ID.
01039      MOVE 'C'                    TO  JP-RECORD-TYPE.
01040
01041      
      * EXEC CICS REWRITE
01042 *        DATASET (WS-CONTROL-FILE-DSID)
01043 *        FROM    (CONTROL-FILE) END-EXEC.
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00004260' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CONTROL-FILE-DSID, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01044
01045      PERFORM 8400-LOG-JOURNAL-RECORD.
01046
01047      MOVE EIBTRMID               TO  PI-TSK-TERM-ID.
01048      MOVE EIBTIME                TO  PI-TSK-TIME.
01049
030612     IF PI-COMPANY-ID = 'DMD' OR 'CID' OR 'AHL'
01051 *        MOVE EIBTRMID       TO PI-CHECK-PRINTER-ID
01052          
      * EXEC CICS START
01053 *            TRANSID (WS-CHECK-WRITER-TRANS-ID)
01054 *            FROM    (PROGRAM-INTERFACE-BLOCK)
01055 *            LENGTH  (PI-COMM-LENGTH)
01056 *            TERMID  (PI-CHECK-PRINTER-ID)
01057 *        END-EXEC
      *    MOVE '0( LF                 0   #00004271' TO DFHEIV0
           MOVE X'3028204C4620202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020203020' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323731' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 WS-CHECK-WRITER-TRANS-ID, 
                 DFHEIV99, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01058      ELSE
01059          
      * EXEC CICS START
01060 *            TRANSID (WS-CHECK-WRITER-TRANS-ID)
01061 *            FROM    (PROGRAM-INTERFACE-BLOCK)
01062 *            LENGTH  (PI-COMM-LENGTH)
01063 *            TERMID  (PI-CHECK-PRINTER-ID)
01064 *        END-EXEC.
      *    MOVE '0( LFT                0   #00004278' TO DFHEIV0
           MOVE X'3028204C4654202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020203020' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 WS-CHECK-WRITER-TRANS-ID, 
                 DFHEIV99, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 PI-CHECK-PRINTER-ID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01065
01066      MOVE +1                     TO  PI-PRINTER-STARTED-SW.
01067
01068      
      * EXEC CICS GETMAIN
01069 *        SET     (ADDRESS OF CHECK-QUE)
01070 *        LENGTH  (WS-ERCHKQ-LENGTH)
01071 *        INITIMG (WS-SPACES) END-EXEC.
      *    MOVE ',"IL                  $   #00004287' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323837' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 WS-ERCHKQ-LENGTH, 
                 WS-SPACES
           SET ADDRESS OF CHECK-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01072
01073  0245-MAIN-LOGIC.
01074      MOVE SPACES                 TO  CHECK-QUE.
01075
01076      MOVE 'CQ'                   TO  CQ-RECORD-ID.
01077
01078      MOVE PI-COMPANY-CD          TO  CQ-COMPANY-CD.
01079      MOVE 'A'                    TO  CQ-ENTRY-TYPE.
01080
01081      MOVE PI-ALIGNMENT-CONTROL-GROUP  TO  CQ-CONTROL-NUMBER.
01082
01083      MOVE ZERO                   TO  CQ-CHECK-AMOUNT.
01084      MOVE +1                     TO  CQ-TIMES-PRINTED.
01085      MOVE WS-CURRENT-DATE        TO  CQ-CHECK-WRITTEN-DT.
01086
01087      MOVE WS-CHECK-QUEUE-DSID    TO  JP-FILE-ID.
01088      MOVE 'A'                    TO  JP-RECORD-TYPE.
01089
01090      MOVE +6870                  TO  CQ-LAST-UPDATED-BY.
01091
01092  0250-MAIN-LOGIC.
01093      IF AACNI = 'Y'
01094          MOVE WS-CHECK-NUMBER-X  TO  CQ-CHECK-NUMBER
01095          ADD +1  TO  WS-CHECK-NUMBER
01096          MOVE '1'                TO  CQ-PRE-NUMBERING-SW.
01097
01098      MOVE PI-ALIGNMENT-SEQUENCE-NO  TO  CQ-SEQUENCE-NUMBER.
01099      ADD +1  TO  PI-ALIGNMENT-SEQUENCE-NO.
01100
01101      MOVE SPACES                 TO  CHECK-PASS-AREA.
01102
01103      MOVE +1                     TO  CPA-ALIGNMENT.
01104      MOVE CQ-CHECK-NUMBER        TO  CPA-CHECK-NUMBER.
01105
01106      PERFORM 0800-PRINT-CHECK.
01107
01108      IF AACNI = 'Y'
01109          MOVE CHECK-QUE          TO  JP-RECORD-AREA
01110          
      * EXEC CICS WRITE
01111 *            DATASET (WS-CHECK-QUEUE-DSID)
01112 *            RIDFLD  (CQ-CONTROL-PRIMARY)
01113 *            FROM    (CHECK-QUE) END-EXEC
           MOVE LENGTH OF
            CHECK-QUE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00004329' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333239' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-DSID, 
                 CHECK-QUE, 
                 DFHEIV11, 
                 CQ-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01114          PERFORM 8400-LOG-JOURNAL-RECORD.
01115
01116      SUBTRACT +1 FROM PI-NUMBER-OF-ALIGNMENT-CHECKS.
01117
01118      IF PI-NUMBER-OF-ALIGNMENT-CHECKS IS GREATER THAN ZERO
01119          GO TO 0250-MAIN-LOGIC.
01120
01121      MOVE SPACES                 TO  AALIGNI.
01122      MOVE AL-UNNOF               TO  AALIGNA.
01123      MOVE -1                     TO  AALIGNL.
01124      MOVE ER-0362                TO  EMI-ERROR.
01125      PERFORM 8200-SEND-DATAONLY.
01126
01127      
      * EXEC CICS SYNCPOINT
01128 *        END-EXEC.
      *    MOVE '6"                    !   #00004346' TO DFHEIV0
           MOVE X'362220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01129
01130      GO TO 0040-MAIN-LOGIC.
01131
01132      EJECT
01133  0300-MAIN-LOGIC.
01134      IF PI-PRINTER-STARTED-SW = ZERO
01135          MOVE +1                 TO  PI-PRINTER-STARTED-SW
01136          MOVE EIBTRMID TO PI-TSK-TERM-ID
01137          MOVE EIBTIME  TO PI-TSK-TIME
01138
030612         IF PI-COMPANY-ID = 'DMD' OR 'CID' OR 'AHL'
01140 *            MOVE EIBTRMID       TO PI-CHECK-PRINTER-ID
01141              
      * EXEC CICS START
01142 *                TRANSID (WS-CHECK-WRITER-TRANS-ID)
01143 *                FROM    (PROGRAM-INTERFACE-BLOCK)
01144 *                LENGTH  (PI-COMM-LENGTH)
01145 *                TERMID  (PI-CHECK-PRINTER-ID)
01146 *            END-EXEC
      *    MOVE '0( LF                 0   #00004360' TO DFHEIV0
           MOVE X'3028204C4620202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020203020' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 WS-CHECK-WRITER-TRANS-ID, 
                 DFHEIV99, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01147          ELSE
01148              
      * EXEC CICS START
01149 *                TRANSID (WS-CHECK-WRITER-TRANS-ID)
01150 *                FROM    (PROGRAM-INTERFACE-BLOCK)
01151 *                LENGTH  (PI-COMM-LENGTH)
01152 *                TERMID  (PI-CHECK-PRINTER-ID)
01153 *            END-EXEC.
      *    MOVE '0( LFT                0   #00004367' TO DFHEIV0
           MOVE X'3028204C4654202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020203020' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 WS-CHECK-WRITER-TRANS-ID, 
                 DFHEIV99, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 PI-CHECK-PRINTER-ID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01154
01155      SET PI-INDEX TO +1.
01156
01157      MOVE LOW-VALUES             TO  WS-CHECK-QUEUE-KEY.
01158      MOVE +5                     TO  WS-KEY-LENGTH.
01159
01160      MOVE PI-COMPANY-CD          TO  WS-CQK-COMPANY-CD.
01161
01162  0310-MAIN-LOGIC.
01163      IF AOPTIONI = '1'
01164          
      * EXEC CICS STARTBR
01165 *            DATASET (WS-CHECK-QUEUE-DSID)
01166 *            RIDFLD  (WS-CHECK-QUEUE-KEY)
01167 *            GTEQ    END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00004383' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333833' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-DSID, 
                 WS-CHECK-QUEUE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01168        ELSE
01169          MOVE PI-CONTROL-GROUP (PI-INDEX) TO WS-CQK-CONTROL-NUMBER
01170          ADD +1  PI-HIGH-SEQUENCE (PI-INDEX)
01171              GIVING WS-SEQUENCE-NUMBER
01172          MOVE ZERO               TO  WS-CQK-SEQUENCE-NUMBER
01173          IF WS-CHECK-QUEUE-BROWSE-SW = ZERO
01174              
      * EXEC CICS STARTBR
01175 *                DATASET   (WS-CHECK-QUEUE-DSID)
01176 *                RIDFLD    (WS-CHECK-QUEUE-KEY)
01177 *                KEYLENGTH (WS-KEY-LENGTH)
01178 *                GENERIC   EQUAL END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,   KG    E          &   #00004393' TO DFHEIV0
           MOVE X'262C2020204B472020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-DSID, 
                 WS-CHECK-QUEUE-KEY, 
                 WS-KEY-LENGTH, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01179              MOVE +1             TO  WS-CHECK-QUEUE-BROWSE-SW
01180            ELSE
01181              
      * EXEC CICS RESETBR
01182 *                DATASET   (WS-CHECK-QUEUE-DSID)
01183 *                RIDFLD    (WS-CHECK-QUEUE-KEY)
01184 *                KEYLENGTH (WS-KEY-LENGTH)
01185 *                GENERIC   EQUAL END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&4   KG    E          &   #00004400' TO DFHEIV0
           MOVE X'26342020204B472020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-DSID, 
                 WS-CHECK-QUEUE-KEY, 
                 WS-KEY-LENGTH, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01186
01187      EJECT
01188  0320-MAIN-LOGIC.
01189      
      * EXEC CICS HANDLE CONDITION
01190 *        NOTFND
01191 *        ENDFILE (0390-MAIN-LOGIC) END-EXEC.
      *    MOVE '"$?''                  ! % #00004408' TO DFHEIV0
           MOVE X'2224B9272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303034343038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01192
01193  0325-MAIN-LOGIC.
01194
01195      
      * EXEC CICS READNEXT
01196 *        DATASET (WS-CHECK-QUEUE-DSID)
01197 *        RIDFLD  (WS-CHECK-QUEUE-KEY)
01198 *        SET     (ADDRESS OF CHECK-QUE) END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00004414' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343134' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CHECK-QUEUE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CHECK-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01199
01200      IF WS-CHECK-QUEUE-KEY NOT GREATER WS-LAST-CHECK-QUEUE-KEY
01201          GO TO 0325-MAIN-LOGIC.
01202
01203      MOVE WS-CHECK-QUEUE-KEY    TO WS-LAST-CHECK-QUEUE-KEY.
01204
01205      IF CQ-COMPANY-CD NOT = PI-COMPANY-CD
01206          GO TO 0390-MAIN-LOGIC.
01207
01208      IF AOPTIONI = ('2' OR '3')
01209          IF CQ-CONTROL-NUMBER = PI-CONTROL-GROUP (PI-INDEX)
01210           AND CQ-SEQUENCE-NUMBER NOT GREATER THAN PI-HIGH-SEQUENCE
01211                                                        (PI-INDEX)
01212              NEXT SENTENCE
01213            ELSE
01214              GO TO 0390-MAIN-LOGIC.
01215
01216      IF CQ-ENTRY-TYPE NOT = 'Q'
01217          GO TO 0325-MAIN-LOGIC.
01218
01219      IF (AOPTIONI NOT = '3' AND
01220          CQ-TIMES-PRINTED GREATER THAN ZERO)
01221        OR
01222         (AOPTIONI = '3' AND
01223          CQ-TIMES-PRINTED NOT GREATER THAN ZERO)
01224              GO TO 0325-MAIN-LOGIC.
01225
01226      
      * EXEC CICS ENDBR
01227 *        DATASET (WS-CHECK-QUEUE-DSID) END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00004445' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343435' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01228
01229      
      * EXEC CICS READ UPDATE
01230 *        DATASET (WS-CHECK-QUEUE-DSID)
01231 *        RIDFLD  (WS-CHECK-QUEUE-KEY)
01232 *        SET     (ADDRESS OF CHECK-QUE) END-EXEC.
      *    MOVE '&"S        EU         (   #00004448' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CHECK-QUEUE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CHECK-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01233
01234      MOVE CHECK-QUE              TO  WS-OLD-CHECK-QUEUE-RECORD.
01235
01236      IF AACNI = 'Y'
01237          MOVE WS-CHECK-NUMBER-X  TO  CQ-CHECK-NUMBER
01238          ADD +1  TO  WS-CHECK-NUMBER
01239          MOVE '1'                TO  CQ-PRE-NUMBERING-SW.
01240
01241 *    NOTE *******************************************************
01242 *         *                PRINT THE CHECK                      *
01243 *         *******************************************************.
01244
01245      IF CQ-CHECK-MAINT-PMT OR
01246         CQ-REFUND-PMT
01247          GO TO 0350-MAIN-LOGIC.
01248
01249 *    NOTE *******************************************************
01250 *         *                PRINT BILLING CHECKS                 *
01251 *         *******************************************************.
01252
01253      MOVE SPACES                 TO  CHECK-PASS-AREA.
01254      MOVE LOW-VALUES             TO  WS-COMPENSATION-MASTER-KEY
01255                                      WS-PENDING-PAYMENTS-KEY.
01256      MOVE PI-COMPANY-CD          TO  WS-CM-COMPANY-CD
01257                                      WS-PPK-COMPANY-CD.
01258      MOVE CQ-PYAJ-CARRIER        TO  WS-CM-CARRIER
01259                                      CPA-CARRIER
01260                                      WS-PPK-CARRIER.
01261      MOVE CQ-PYAJ-GROUPING       TO  WS-CM-GROUPING
01262                                      CPA-GROUPING
01263                                      WS-PPK-GROUPING.
01264      MOVE CQ-PYAJ-FIN-RESP       TO  WS-CM-FIN-RESP
01265                                      CPA-FIN-RESP
01266                                      WS-PPK-FIN-RESP.
01267      MOVE CQ-PYAJ-ACCOUNT        TO  WS-CM-ACCOUNT
01268                                      CPA-ACCOUNT
01269                                      WS-PPK-ACCOUNT.
01270      MOVE CQ-PYAJ-SEQ            TO  WS-PPK-FILE-SEQ-NO.
01271      MOVE 'C'                    TO  WS-PPK-RECORD-TYPE.
01272
01273      IF CQ-PYAJ-ACCOUNT NOT = LOW-VALUES
01274         MOVE 'A'                 TO  WS-CM-TYPE
01275      ELSE
01276         MOVE 'G'                 TO  WS-CM-TYPE.
01277
01278      
      * EXEC CICS READ
01279 *        DATASET (WS-COMPENSATION-MASTER-DSID)
01280 *        RIDFLD  (WS-COMPENSATION-MASTER-KEY)
01281 *        SET     (ADDRESS OF COMPENSATION-MASTER) END-EXEC.
      *    MOVE '&"S        E          (   #00004497' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343937' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-COMPENSATION-MASTER-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-COMPENSATION-MASTER-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMPENSATION-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01282
01283      
      * EXEC CICS READ UPDATE
01284 *        DATASET (WS-PENDING-PAYMENTS-DSID)
01285 *        RIDFLD  (WS-PENDING-PAYMENTS-KEY)
01286 *        SET     (ADDRESS OF PENDING-PAY-ADJ) END-EXEC.
      *    MOVE '&"S        EU         (   #00004502' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034353032' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-PENDING-PAYMENTS-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-PENDING-PAYMENTS-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-PAY-ADJ TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01287
01288      MOVE ZERO                   TO  CPA-ALIGNMENT.
01289      MOVE SPACES                 TO  CPA-CERT-NO.
01290      MOVE ZEROS                  TO  WS-CHECK-NO.
01291      MOVE CQ-PAYMENT-TYPE        TO  CPA-PAYMENT-TYPE.
01292      MOVE CQ-CHECK-BY-USER       TO  CPA-PAYMENT-BY.
01293      MOVE CQ-CHECK-NUMBER        TO  CPA-CHECK-NUMBER
01294                                      WS-CHECK-NO.
01295      MOVE WS-CHECK-WORK          TO  PY-CHECK-NUMBER.
01296      MOVE CQ-CHECK-AMOUNT        TO  CPA-AMOUNT-PAID.
01297      MOVE ZEROS                  TO  CPA-AMOUNT-PAID-TO-DATE
01298                                      CPA-NO-OF-PMTS-MADE
01299                                      CPA-LF-REFUND
01300                                      CPA-AH-REFUND
01301                                      CPA-DEDUCT-WITHHELD
01302                                      CPA-ADDITIONAL-CHARGE.
01303      MOVE PY-REPORTED-DT         TO  CPA-REPORTED-DT.
01304      MOVE LOW-VALUES             TO  CPA-INCURRED-DT
01305                                      CPA-PAID-THRU-DT
01306                                      CPA-PAID-FROM-DT
01307                                      CPA-PAID-DT
01308                                      CPA-CERT-EFF-DT.
01309      MOVE ZEROS                  TO  CPA-CAR-ZIP-CODE.
01310      MOVE CO-MAIL-NAME           TO  CPA-PAYEE-IN-CARE-OF.
01311      MOVE CO-ACCT-NAME           TO  CPA-PAYEE-NAME.
01312      MOVE CO-ADDR-1              TO  CPA-PAYEE-ADDRESS-LINE1.
01313      MOVE CO-ADDR-2              TO  CPA-PAYEE-ADDRESS-LINE2.
01314 *    MOVE CO-ADDR-3              TO  CPA-PAYEE-CITY-ST.
           STRING CO-ADDR-CITY ' ' CO-ADDR-STATE
              DELIMITED BY '  ' INTO CPA-PAYEE-CITY-ST
           END-STRING
01315      MOVE CO-TELEPHONE           TO  CPA-PAYEE-PHONE-NO.
01316      MOVE CO-ZIP                 TO  CPA-PAYEE-ZIP-CODE.
01317
01318      IF NOT CO-CANADIAN-POST-CODE
01319          INSPECT CPA-PAYEE-ZIP-CODE REPLACING ALL ' ' BY ZEROS
01320          IF CPA-PAYEE-ZIP-PRIME NUMERIC
01321              MOVE SPACES         TO CPA-PAYEE-ZIP-PLUS4
01322          ELSE
01323              MOVE ZEROS          TO CPA-PAYEE-ZIP-CODE.
01324
01325      MOVE SPACES                 TO  WS-SSN-STATE
01326                                      WS-MEMBER-STATE
01327                                      CPA-MEMBER-NUMBER
01328                                      WS-SSN-LN3
01329                                      WS-MEMBER-LN4.
01330      MOVE CO-ACCT-PRIME          TO  WS-SSN-ACCOUNT
01331                                      WS-MEMBER-ACCOUNT.
01332      MOVE CO-SOC-SEC             TO  CPA-SOC-SEC-NO.
01333      MOVE WS-CURRENT-DATE        TO  CPA-CHECK-DATE
01334                                      CQ-CHECK-WRITTEN-DT
01335                                      PY-CHECK-WRITTEN-DT.
01336
01337      IF  PY-LETTER (1) GREATER THAN SPACES
01338          MOVE 'Y'                TO  PI-LETTERS-IND
01339
01340      ELSE
01341          MOVE LOW-VALUES         TO  PI-LETTERS-IND.
01342
01343      IF PI-COMPANY-ID = 'ITY'
01344          MOVE PY-ENTRY-COMMENT   TO  CPA-STUB5.
01345
01346      IF AOPTIONI = '3'
01347        AND CQ-PRE-NUMBERING-SW = '1'
01348          MOVE WS-SEQUENCE-NUMBER TO  PY-CHECK-QUE-SEQUENCE.
01349
01350      MOVE PENDING-PAY-ADJ        TO  JP-RECORD-AREA.
01351
01352      MOVE +6870                  TO  CQ-LAST-UPDATED-BY.
01353
01354      
      * EXEC CICS REWRITE
01355 *        DATASET (WS-PENDING-PAYMENTS-DSID)
01356 *        FROM    (PENDING-PAY-ADJ)  END-EXEC.
           MOVE LENGTH OF
            PENDING-PAY-ADJ
             TO DFHEIV11
      *    MOVE '&& L                  %   #00004576' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303034353736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-PENDING-PAYMENTS-DSID, 
                 PENDING-PAY-ADJ, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01357
01358      MOVE WS-PENDING-PAYMENTS-DSID TO  JP-FILE-ID.
01359      MOVE 'C'                      TO  JP-RECORD-TYPE.
01360      PERFORM 8400-LOG-JOURNAL-RECORD.
01361
01362      GO TO 0375-MAIN-LOGIC.
01363
01364      EJECT
01365  0350-MAIN-LOGIC.
01366 *    NOTE *******************************************************
01367 *         *         PRINT REFUND OR MISCELLANEOUS CHECKS        *
01368 *         *******************************************************.
01369
01370      MOVE SPACES                 TO  CHECK-PASS-AREA.
01371      MOVE ZERO                   TO  CPA-ALIGNMENT.
01372      MOVE LOW-VALUES             TO  WS-CHECK-MAINT-KEY.
01373      MOVE PI-COMPANY-CD          TO  WS-CMK-COMPANY-CD.
01374      MOVE CQ-CHEK-CARRIER        TO  WS-CMK-CARRIER
01375                                      CPA-CARRIER.
01376      MOVE CQ-CHEK-GROUPING       TO  WS-CMK-GROUPING
01377                                      CPA-GROUPING.
01378      MOVE CQ-CHEK-STATE          TO  WS-CMK-STATE
01379                                      CPA-STATE.
01380      MOVE CQ-CHEK-ACCOUNT        TO  WS-CMK-ACCOUNT
01381                                      CPA-ACCOUNT.
01382      MOVE CQ-CHEK-CERT-NO        TO  WS-CMK-CERT-NO
01383                                      CPA-CERT-NO.
01384      MOVE CQ-CHEK-CERT-EFF-DT    TO  WS-CMK-CERT-EFF-DT.
01385      MOVE CQ-CHEK-SEQ-NO         TO  WS-CMK-SEQ-NO.
01386
01387      
      * EXEC CICS READ UPDATE
01388 *        DATASET (WS-CHECK-MAINT-DSID)
01389 *        RIDFLD  (WS-CHECK-MAINT-KEY)
01390 *        SET     (ADDRESS OF CHECK-RECORDS) END-EXEC.
      *    MOVE '&"S        EU         (   #00004609' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034363039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-MAINT-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CHECK-MAINT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CHECK-RECORDS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01391
01392      IF CH-LF-REFUND NOT NUMERIC
01393          MOVE ZEROS              TO  CH-LF-REFUND.
01394      IF CH-AH-REFUND NOT NUMERIC
01395          MOVE ZEROS              TO  CH-AH-REFUND.
01396      IF CH-DEDUCT-WITHHELD NOT NUMERIC
01397          MOVE ZEROS              TO  CH-DEDUCT-WITHHELD.
01398      IF CH-ADDITIONAL-CHARGE NOT NUMERIC
01399          MOVE ZEROS              TO  CH-ADDITIONAL-CHARGE.
01400
01401      MOVE ZERO                   TO  CPA-ALIGNMENT.
01402      MOVE CQ-PAYMENT-TYPE        TO  CPA-PAYMENT-TYPE.
01403      MOVE CQ-CHECK-BY-USER       TO  CPA-PAYMENT-BY.
01404      MOVE CQ-CHECK-NUMBER        TO  CPA-CHECK-NUMBER
01405                                      CH-CHECK-NO.
01406      MOVE CQ-CHECK-AMOUNT        TO  CPA-AMOUNT-PAID.
01407      MOVE ZEROS                  TO  CPA-AMOUNT-PAID-TO-DATE
01408                                      CPA-NO-OF-PMTS-MADE.
01409      MOVE CH-RECORDED-DT         TO  CPA-REPORTED-DT.
01410      MOVE LOW-VALUES             TO  CPA-INCURRED-DT
01411                                      CPA-PAID-THRU-DT
01412                                      CPA-PAID-FROM-DT
01413                                      CPA-PAID-DT.
01414      MOVE ZEROS                  TO  CPA-CAR-ZIP-CODE.
01415      MOVE CH-PAYEE-NAME-1        TO  CPA-PAYEE-NAME.
01416      MOVE CH-PAYEE-NAME-2        TO  CPA-PAYEE-IN-CARE-OF.
01417      MOVE CH-PAYEE-ADDRESS-1     TO  CPA-PAYEE-ADDRESS-LINE1.
01418
01419      IF CH-PAYEE-CITY-ST EQUAL SPACES
01420          MOVE CH-PAYEE-ADDRESS-2
01421                                  TO  CPA-PAYEE-CITY-ST
01422          MOVE SPACES             TO  CPA-PAYEE-ADDRESS-LINE2
01423      ELSE
01424          MOVE CH-PAYEE-ADDRESS-2
01425                                  TO  CPA-PAYEE-ADDRESS-LINE2
01426          MOVE CH-PAYEE-CITY-ST   TO  CPA-PAYEE-CITY-ST.
01427
01428      MOVE CH-STUB-LINE-1         TO  CPA-STUB1.
01429      MOVE CH-STUB-LINE-2         TO  CPA-STUB2.
01430      MOVE CH-STUB-LINE-3         TO  CPA-STUB3.
01431      MOVE CH-STUB-LINE-4         TO  CPA-STUB4.
01432      MOVE CH-PAYEE-ZIP-CODE      TO  CPA-PAYEE-ZIP-CODE.
01433
01434      IF CQ-REFUND-PMT
01435          MOVE CH-CANC-DT         TO  CPA-CANC-DT
01436          MOVE CH-CERT-EFF-DT     TO  CPA-CERT-EFF-DT
01437          MOVE CH-INSURED-NAME    TO  CPA-INSURED-NAME
01438      ELSE
01439          MOVE LOW-VALUES         TO  CPA-CANC-DT
01440                                      CPA-CERT-EFF-DT
01441          MOVE SPACES             TO  CPA-INSURED-NAME.
01442
01443      MOVE CH-LF-REFUND           TO  CPA-LF-REFUND.
01444      MOVE CH-AH-REFUND           TO  CPA-AH-REFUND.
01445      MOVE CH-DEDUCT-WITHHELD     TO  CPA-DEDUCT-WITHHELD.
01446      MOVE CH-ADDITIONAL-CHARGE   TO  CPA-ADDITIONAL-CHARGE.
01447
01448      MOVE SPACES                 TO  WS-SSN-STATE
01449                                      WS-MEMBER-STATE
01450                                      CPA-MEMBER-NUMBER
01451                                      WS-SSN-LN3
01452                                      WS-MEMBER-LN4.
01453      MOVE CO-ACCT-PRIME          TO  WS-SSN-ACCOUNT
01454                                      WS-MEMBER-ACCOUNT.
01455      MOVE CO-SOC-SEC             TO  CPA-SOC-SEC-NO.
01456      MOVE WS-CURRENT-DATE        TO  CPA-CHECK-DATE
01457                                      CQ-CHECK-WRITTEN-DT
01458                                      CH-CHECK-WRITTEN-DT.
01459      IF AOPTIONI = '3'
01460        AND CQ-PRE-NUMBERING-SW = '1'
01461          MOVE WS-SEQUENCE-NUMBER TO  CH-CHECK-QUE-SEQUENCE.
01462
01463      MOVE CHECK-RECORDS          TO  JP-RECORD-AREA.
01464
01465      IF  CH-LETTERS (1) GREATER THAN SPACES
01466          MOVE 'Y'                TO  PI-LETTERS-IND
01467
01468      ELSE
01469          MOVE LOW-VALUES         TO  PI-LETTERS-IND.
01470
01471      MOVE +6870                  TO  CQ-LAST-UPDATED-BY.
01472
01473      
      * EXEC CICS REWRITE
01474 *        DATASET (WS-CHECK-MAINT-DSID)
01475 *        FROM    (CHECK-RECORDS)  END-EXEC.
           MOVE LENGTH OF
            CHECK-RECORDS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00004695' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303034363935' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-MAINT-DSID, 
                 CHECK-RECORDS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01476
01477      MOVE WS-CHECK-MAINT-DSID    TO  JP-FILE-ID.
01478      MOVE 'C'                    TO  JP-RECORD-TYPE.
01479      PERFORM 8400-LOG-JOURNAL-RECORD.
01480
01481      EJECT
01482
01483  0375-MAIN-LOGIC.
01484      ADD +1  TO  CQ-TIMES-PRINTED.
01485
01486      IF AOPTIONI = '3'
01487        AND CQ-PRE-NUMBERING-SW = '1'
01488          MOVE CHECK-QUE          TO  WS-NEW-CHECK-QUEUE-RECORD
01489          MOVE WS-OLD-CHECK-QUEUE-RECORD  TO  CHECK-QUE
01490          MOVE 'S'                TO  CQ-ENTRY-TYPE.
01491
01492      MOVE +6870                  TO  CQ-LAST-UPDATED-BY.
01493
01494      MOVE 'C'                    TO  JP-RECORD-TYPE.
01495      MOVE CHECK-QUE              TO  JP-RECORD-AREA.
01496      MOVE WS-CHECK-QUEUE-DSID    TO  JP-FILE-ID.
01497      MOVE PI-LETTERS-IND         TO  CQ-LETTERS-IND.
01498
01499      
      * EXEC CICS REWRITE
01500 *        DATASET (WS-CHECK-QUEUE-DSID)
01501 *        FROM    (CHECK-QUE) END-EXEC.
           MOVE LENGTH OF
            CHECK-QUE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00004721' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-DSID, 
                 CHECK-QUE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01502
01503      PERFORM 8400-LOG-JOURNAL-RECORD.
01504
01505      IF AOPTIONI NOT = '3'
01506          GO TO 0380-MAIN-LOGIC.
01507
01508      IF LCP-ONCTR-01 =  0
01509          ADD 1 TO LCP-ONCTR-01
01510                               ,
01511          
      * EXEC CICS GETMAIN
01512 *            SET    (WS-CQFCBAR)
01513 *            LENGTH  (WS-ERCHKQ-LENGTH)
01514 *            END-EXEC.
      *    MOVE '," L                  $   #00004733' TO DFHEIV0
           MOVE X'2C22204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373333' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CQFCBAR, 
                 WS-ERCHKQ-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01515
01516      MOVE WS-CQFCBAR             TO LCP-WS-ADDR-COMP
01517      SET ADDRESS OF CHECK-QUE TO LCP-WS-ADDR-PNTR.
01518
01519      MOVE WS-NEW-CHECK-QUEUE-RECORD  TO  CHECK-QUE.
01520
01521      IF CQ-PRE-NUMBERING-SW NOT = '1'
01522          GO TO 0380-MAIN-LOGIC.
01523
01524      MOVE 'Q'                    TO  CQ-ENTRY-TYPE.
01525      MOVE PI-LETTERS-IND         TO  CQ-LETTERS-IND.
01526
01527      MOVE WS-SEQUENCE-NUMBER     TO  CQ-SEQUENCE-NUMBER.
01528
01529      ADD +1  TO  WS-SEQUENCE-NUMBER.
01530
01531      MOVE +6870                  TO  CQ-LAST-UPDATED-BY.
01532
01533      MOVE CHECK-QUE              TO  JP-RECORD-AREA.
01534
01535      
      * EXEC CICS WRITE
01536 *        DATASET (WS-CHECK-QUEUE-DSID)
01537 *        RIDFLD  (CQ-CONTROL-PRIMARY)
01538 *        FROM    (CHECK-QUE) END-EXEC.
           MOVE LENGTH OF
            CHECK-QUE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00004757' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373537' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-DSID, 
                 CHECK-QUE, 
                 DFHEIV11, 
                 CQ-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01539
01540      MOVE 'A'                    TO  JP-RECORD-TYPE.
01541      PERFORM 8400-LOG-JOURNAL-RECORD.
01542
01543  0380-MAIN-LOGIC.
01544      PERFORM 0800-PRINT-CHECK.
01545
01546      
      * EXEC CICS STARTBR
01547 *        DATASET (WS-CHECK-QUEUE-DSID)
01548 *        RIDFLD  (WS-CHECK-QUEUE-KEY)
01549 *        EQUAL   END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         E          &   #00004768' TO DFHEIV0
           MOVE X'262C20202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-DSID, 
                 WS-CHECK-QUEUE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01550
01551      GO TO 0320-MAIN-LOGIC.
01552
01553      EJECT
01554  0390-MAIN-LOGIC.
01555      IF AOPTIONI NOT = '1'
01556        AND PI-INDEX LESS THAN PI-NUMBER-OF-CONTROL-GROUPS
01557          SET PI-INDEX UP BY +1
01558          MOVE LOW-VALUES         TO WS-LAST-CHECK-QUEUE-KEY
01559          GO TO 0310-MAIN-LOGIC.
01560
01561      
      * EXEC CICS ENDBR
01562 *        DATASET (WS-CHECK-QUEUE-DSID) END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00004783' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373833' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01563
01564      IF PI-CHECK-PRINTER-ID NOT = 'R2T7'
01565          PERFORM 0700-END-PRINT.
01566
01567      MOVE PI-PRINTER-STARTED-SW  TO  WS-PRINTER-STARTED-SW.
01568      MOVE PI-TEMP-STORAGE-KEY    TO  WS-TEMP-STORAGE-KEY.
01569      ADD +1  TO  WS-COMPLETED-SUCCESSFUL.
01570      GO TO 0015-MAIN-LOGIC.
01571
01572  0700-END-PRINT SECTION.
01573      MOVE HIGH-VALUES            TO  CHECK-PASS-AREA.
01574      MOVE +1                     TO  WS-TS-LENGTH.
01575
01576      PERFORM 0800-PRINT-CHECK.
01577
01578      MOVE ZERO                   TO  PI-PRINTER-STARTED-SW.
01579
01580  0700-EXIT.
01581      EXIT.
01582
01583  0800-PRINT-CHECK SECTION.
01584      
      * EXEC CICS WRITEQ TS
01585 *         QUEUE  (PI-TEMP-STORAGE-KEY)
01586 *         ITEM   (WS-TEMP-STORAGE-ITEM)
01587 *         FROM   (CHECK-PASS-AREA)
01588 *         LENGTH (WS-TS-LENGTH) END-EXEC.
      *    MOVE '*" I                  ''   #00004806' TO DFHEIV0
           MOVE X'2A2220492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383036' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-TEMP-STORAGE-KEY, 
                 CHECK-PASS-AREA, 
                 WS-TS-LENGTH, 
                 WS-TEMP-STORAGE-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01589
01590  0800-EXIT.
01591      EXIT.
01592
01593      EJECT
01594  0900-TERMIDERR SECTION.
01595
01596      
      * EXEC CICS SYNCPOINT
01597 *        ROLLBACK END-EXEC.
      *    MOVE '6"R                   !   #00004818' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01598
01599      MOVE ER-0371                TO  EMI-ERROR.
01600      MOVE -1                     TO  APFKL.
01601      PERFORM 8200-SEND-DATAONLY.
01602      PERFORM 9100-RETURN-TRAN.
01603
01604  0910-ENQ-BUSY.
01605      MOVE ER-0395                TO  EMI-ERROR.
01606      MOVE -1                     TO  AOPTIONL.
01607      PERFORM 8200-SEND-DATAONLY.
01608      PERFORM 9100-RETURN-TRAN.
01609
01610      EJECT
01611 *5000-MOVE-NAME SECTION. COPY ELCMNS.
01612
01613      EJECT
01614  8100-SEND-INITIAL-MAP SECTION.
01615
01616      IF EMI-ERROR NOT = ZERO
01617          PERFORM 9900-ERROR-FORMAT
01618        ELSE
01619          IF TRANSACTION-SUCCESSFUL
01620              PERFORM 9900-ERROR-FORMAT
01621              IF CHECKS-WITHOUT-ADDRESSES
01622                  MOVE ER-0364    TO  EMI-ERROR
01623                  PERFORM 9900-ERROR-FORMAT.
01624
01625      MOVE EIBTIME                TO  WS-TIME-WORK.
01626      
      * EXEC CICS ASKTIME ABSTIME(LCP-CICS-DATE)
01627 *    END-EXEC
      *    MOVE '0"A                   "   #00004848' TO DFHEIV0
           MOVE X'302241202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202220' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCP-CICS-DATE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01628      
      * EXEC CICS FORMATTIME
01629 *              ABSTIME(LCP-CICS-DATE)
01630 *              YYMMDD(LCP-CURRENT-DATE-68)
01631 *              DATESEP('/')
01632 *    END-EXEC
           MOVE '/' TO DFHEIV9
      *    MOVE 'j$((   "              $   #00004850' TO DFHEIV0
           MOVE X'6A2428282020202220202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383530' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCP-CICS-DATE, 
                 LCP-CURRENT-DATE-68, 
                 DFHEIV9
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01633
01634      MOVE LCP-CURRENT-DATE-68 TO ADATEO.
01635      MOVE WS-TIME                TO  ATIMEO.
01636      MOVE EMI-MESSAGE-AREA (1)   TO  AEMSG1O.
01637      MOVE EMI-MESSAGE-AREA (2)   TO  AEMSG2O.
01638      MOVE EMI-MESSAGE-AREA (3)   TO  AEMSG3O.
01639
01640      
      * EXEC CICS SEND
01641 *        FROM   (EL687AI)
01642 *        MAPSET (WS-MAPSET-NAME)
01643 *        MAP    (WS-MAP-NAME)
01644 *        CURSOR ERASE END-EXEC.
           MOVE LENGTH OF
            EL687AI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00004862' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL687AI, 
                 DFHEIV12, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01645
01646      PERFORM 9100-RETURN-TRAN.
01647
01648  8100-EXIT.
01649      EXIT.
01650
01651      EJECT
01652  8200-SEND-DATAONLY SECTION.
01653
01654      IF EMI-ERROR NOT = 3130
01655          PERFORM 9900-ERROR-FORMAT
01656        ELSE
01657          IF TRANSACTION-SUCCESSFUL
01658              PERFORM 9900-ERROR-FORMAT
01659              IF CHECKS-WITHOUT-ADDRESSES
01660                  MOVE ER-0364    TO  EMI-ERROR
01661                  PERFORM 9900-ERROR-FORMAT.
01662
01663      MOVE EIBTIME                TO  WS-TIME-WORK.
01664      
      * EXEC CICS ASKTIME ABSTIME(LCP-CICS-DATE)
01665 *    END-EXEC
      *    MOVE '0"A                   "   #00004886' TO DFHEIV0
           MOVE X'302241202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202220' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383836' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCP-CICS-DATE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01666      
      * EXEC CICS FORMATTIME
01667 *              ABSTIME(LCP-CICS-DATE)
01668 *              YYMMDD(LCP-CURRENT-DATE-68)
01669 *              DATESEP('/')
01670 *    END-EXEC
           MOVE '/' TO DFHEIV9
      *    MOVE 'j$((   "              $   #00004888' TO DFHEIV0
           MOVE X'6A2428282020202220202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383838' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCP-CICS-DATE, 
                 LCP-CURRENT-DATE-68, 
                 DFHEIV9
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01671
01672      MOVE LCP-CURRENT-DATE-68 TO ADATEO.
01673      MOVE WS-TIME                TO  ATIMEO.
01674      MOVE EMI-MESSAGE-AREA (1)   TO  AEMSG1O.
01675      MOVE EMI-MESSAGE-AREA (2)   TO  AEMSG2O.
01676      MOVE EMI-MESSAGE-AREA (3)   TO  AEMSG3O.
01677
01678      
      * EXEC CICS SEND DATAONLY
01679 *        FROM   (EL687AI)
01680 *        MAPSET (WS-MAPSET-NAME)
01681 *        MAP    (WS-MAP-NAME)
01682 *        CURSOR END-EXEC.
           MOVE LENGTH OF
            EL687AI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00004900' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303034393030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL687AI, 
                 DFHEIV12, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01683
01684      IF PI-PROCESSING-SW = ZERO
01685          PERFORM 9100-RETURN-TRAN.
01686
01687      MOVE ZERO                   TO  EMI-SUB
01688                                      EMI-NOTE-CTR
01689                                      EMI-WARNING-CTR
01690                                      EMI-FORCABLE-CTR
01691                                      EMI-FATAL-CTR.
01692
01693      MOVE '1'                    TO  EMI-SWITCH-AREA-1
01694                                      EMI-SWITCH-AREA-2.
01695
01696      MOVE SPACES                 TO  EMI-ERROR-LINES.
01697
01698  8200-EXIT.
01699      EXIT.
01700
01701      EJECT
01702  8300-SEND-TEXT SECTION.
01703
01704      IF PI-PRINTER-STARTED-SW NOT = ZERO
01705          PERFORM 0700-END-PRINT.
01706
01707      
      * EXEC CICS SEND TEXT
01708 *        FROM   (LOGOFF-TEXT)
01709 *        LENGTH (LOGOFF-LENGTH)
01710 *        ERASE  FREEKB END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00004929' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303034393239' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LOGOFF-TEXT, 
                 LOGOFF-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01711
01712      
      * EXEC CICS RETURN
01713 *        END-EXEC.
      *    MOVE '.(                    &   #00004934' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034393334' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01714
01715
01716  8300-EXIT.
01717      EXIT.
01718
01719      EJECT
01720  8400-LOG-JOURNAL-RECORD SECTION.
01721
01722      MOVE PI-PROCESSOR-ID        TO  JP-USER-ID.
01723      MOVE WS-PROGRAM-ID          TO  JP-PROGRAM-ID.
01724
pemuni*    EXEC CICS JOURNAL
pemuni*        JFILEID (PI-JOURNAL-FILE-ID)
pemuni*        JTYPEID (WS-JOURNAL-TYPE-ID)
pemuni*        FROM    (JOURNAL-RECORD)
pemuni*        LENGTH  (WS-JOURNAL-RECORD-LENGTH) END-EXEC.
01730
01731  8400-EXIT.
01732      EXIT.
01733
01734      EJECT
01735  8500-DATE-CONVERSION SECTION.
01736
01737      
      * EXEC CICS LINK
01738 *        PROGRAM  ('ELDATCV')
01739 *        COMMAREA (DATE-CONVERSION-DATA)
01740 *        LENGTH   (DC-COMM-LENGTH) END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   ''   #00004959' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034393539' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01741
01742
01743  8500-EXIT.
01744      EXIT.
01745
01746      EJECT
01747  8700-LOCATE-BENEFIT SECTION.
01748
01749      
      * EXEC CICS HANDLE CONDITION
01750 *        NOTFND (8700-EXIT) END-EXEC.
      *    MOVE '"$I                   ! & #00004971' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303034393731' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01751
01752      MOVE SPACES                 TO  WS-KIND.
01753
01754      MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.
01755      MOVE WS-BENEFIT-NO          TO  WS-CFK-BENEFIT-NO.
01756
01757      
      * EXEC CICS READ
01758 *        DATASET (WS-CONTROL-FILE-DSID)
01759 *        RIDFLD  (WS-CONTROL-FILE-KEY)
01760 *        SET     (ADDRESS OF CONTROL-FILE)
01761 *        GTEQ    END-EXEC.
      *    MOVE '&"S        G          (   #00004979' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034393739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CONTROL-FILE-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CONTROL-FILE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01762
01763      IF WS-CFK-COMPANY-ID NOT = CF-COMPANY-ID
01764        OR WS-CFK-RECORD-TYPE NOT = CF-RECORD-TYPE
01765          GO TO 8700-EXIT.
01766
01767      MOVE +1                     TO  WS-INDEX.
01768
01769  8700-LOOKUP-BENEFIT.
01770      IF WS-BENEFIT-NO = CF-BENEFIT-CODE (WS-INDEX)
01771          MOVE CF-BENEFIT-ALPHA (WS-INDEX)  TO  WS-KIND
01772          GO TO 8700-EXIT.
01773
01774      IF CF-BENEFIT-CODE (WS-INDEX) NOT LESS CF-HI-BEN-IN-REC
01775          GO TO 8700-EXIT.
01776
01777      IF WS-INDEX LESS THAN +8
01778          ADD +1  TO  WS-INDEX
01779          GO TO 8700-LOOKUP-BENEFIT.
01780
01781  8700-EXIT.
01782      EXIT.
01783
01784      EJECT
01785  9000-RETURN-CICS SECTION.
01786
01787      MOVE 'EL005'                TO  WS-PROGRAM-ID.
01788      MOVE EIBAID                 TO  PI-ENTRY-CD-1.
01789      PERFORM 9300-XCTL.
01790
01791  9000-EXIT.
01792      EXIT.
01793
01794  9100-RETURN-TRAN SECTION.
01795
01796      MOVE EMI-ERROR-NUMBER (1)  TO  PI-LAST-ERROR-NO.
01797      MOVE WS-MAP-NUMBER          TO  PI-CURRENT-SCREEN-NO.
01798
01799      
      * EXEC CICS RETURN
01800 *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
01801 *        LENGTH   (PI-COMM-LENGTH)
01802 *        TRANSID  (WS-TRANS-ID) END-EXEC.
      *    MOVE '.(CT                  &   #00005021' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01803
01804  9100-EXIT.
01805      EXIT.
01806
01807  9300-XCTL SECTION.
01808
01809      IF PI-PRINTER-STARTED-SW NOT = ZERO
01810          PERFORM 0700-END-PRINT.
01811
01812      MOVE DFHENTER               TO  EIBAID.
01813
01814      
      * EXEC CICS XCTL
01815 *        PROGRAM  (WS-PROGRAM-ID)
01816 *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
01817 *        LENGTH   (PI-COMM-LENGTH) END-EXEC.
      *    MOVE '.$C                   $   #00005036' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-PROGRAM-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01818
01819  9300-EXIT.
01820      EXIT.
01821
01822      EJECT
01823  9400-CLEAR SECTION.
01824
01825      MOVE PI-RETURN-TO-PROGRAM  TO  WS-PROGRAM-ID.
01826      PERFORM 9300-XCTL.
01827
01828  9400-EXIT.
01829      EXIT.
01830
01831  9600-PGMIDERR SECTION.
01832
01833      
      * EXEC CICS HANDLE CONDITION
01834 *        PGMIDERR (8300-SEND-TEXT) END-EXEC.
      *    MOVE '"$L                   ! '' #00005055' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303035303535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01835
01836      MOVE WS-PROGRAM-ID          TO  PI-CALLING-PROGRAM.
01837
01838      MOVE 'EL005'                TO  WS-PROGRAM-ID
01839                                      LOGOFF-PGM.
01840      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.
01841      MOVE SPACES                 TO  PI-ENTRY-CD-1.
01842      PERFORM 9300-XCTL.
01843
01844  9600-EXIT.
01845      EXIT.
01846
01847  9900-ERROR-FORMAT SECTION.
01848
01849      IF EMI-ERRORS-COMPLETE
01850          MOVE ER-3130            TO  EMI-ERROR
01851          GO TO 9900-EXIT.
01852
01853      
      * EXEC CICS LINK
01854 *        PROGRAM  ('EL001')
01855 *        COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)
01856 *        LENGTH   (EMI-COMM-LENGTH) END-EXEC.
           MOVE 'EL001' TO DFHEIV1
      *    MOVE '."C                   ''   #00005075' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303735' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01857
01858      MOVE ER-3130                TO  EMI-ERROR.
01859
01860  9900-EXIT.
01861      EXIT.
01862
01863      EJECT
01864  9990-ERROR SECTION.
01865
01866      MOVE DFHEIBLK TO EMI-LINE1.
01867      
      * EXEC CICS LINK
01868 *        PROGRAM  ('EL004')
01869 *        COMMAREA (EMI-LINE1)
01870 *        LENGTH   (72) END-EXEC.
           MOVE 'EL004' TO DFHEIV1
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   ''   #00005089' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01871      PERFORM 8200-SEND-DATAONLY.
01872      GO TO 9100-RETURN-TRAN.
01873
01874  9990-EXIT.
01875      EXIT.
01876
01877  9999-LAST-PARAGRAPH SECTION.
01878
01879      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL687' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
01880

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL687' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9600-PGMIDERR,
                     0180-MAIN-LOGIC,
                     0190-MAIN-LOGIC,
                     0900-TERMIDERR,
                     0910-ENQ-BUSY,
                     9990-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 0225-MAIN-LOGIC,
                     0230-MAIN-LOGIC
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 9400-CLEAR,
                     0040-MAIN-LOGIC,
                     0040-MAIN-LOGIC,
                     0040-MAIN-LOGIC
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 9999-DFHBACK,
                     0390-MAIN-LOGIC
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 8700-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL687' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
