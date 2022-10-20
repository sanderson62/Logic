00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL1323.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 02/13/96 09:46:23.
00007 *                            VMOD=2.008.
00008 *
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
00024 *REMARKS.
00025
00026 *        THIS PROGRAM PROVIDES THE FULL DISPLAY OF A CERTIFICATE
00027
00028 *    SCREENS     - EL132C - CERTIFICATE DISPLAY
00029
00030 *    ENTERED BY  - EL132 - CERTIFICATE LOOKUP
00031
00032 *    EXIT TO     - CALLING PROGRAM
00033
00034 *    INPUT FILE  - ELCERT - CERTIFICATE INFORCE FILE
00035 *                  ELCNTL - CONTROL FILE
00036 *                  MPPLCY - CONVENIENCE POLICY MASTER FILE
00037 *                  MPPLAN - CONVENIENCE PRODUCER PLAN MASTER FILE
00038
00039 *    OUTPUT FILE - NONE
00040
00041 *    COMMAREA    - PASSED.  IF A CERTIFICATE IS SELECTED, THE
00042 *                  CONTROL OF THAT CERTIFICATE IS PLACED IN THE
00043 *                  APPROPRIATE FIELDS OF THE COMMAAREA FOR
00044 *                  REFERENCE BY SUCCESSIVE PROGRAMS.  THE PROGRAM
00045 *                  WORK AREA OF THE COMMAREA IS USED TO PASS THE
00046 *                  RECORD KEY INFORMATION NEEDED BY EL1322 TO
00047 *                  LOCATE THE CERTIFICATE.
00048
00049 *    NARRATIVE   - FIRST ENTRY IS VIA AN XCTL FROM EL1322 OR EL150
00050 *                  FIRST ENTRY, USE THE KEY TO THE CERTIFICATE
00051 *                  MASTER PASSED IN THE COMMAREA TO DISPLAY THE
00052 *                  CERTIFICATE AND RETURN WITH THE TRANSACTION OF
00053 *                  THE CALLING PROGRAM.
101501******************************************************************
101501*                   C H A N G E   L O G
101501*
101501* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
101501*-----------------------------------------------------------------
101501*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
101501* EFFECTIVE    NUMBER
101501*-----------------------------------------------------------------
101501* 101501    2001100100006  SMVA  ADD USERID TO SCREEN HEADER
122002* 122002                   PEMA  ADD MONTHLY PRODUCT PROCESSING
101501******************************************************************
00054
00055      EJECT
00056  ENVIRONMENT DIVISION.
00057
00058  DATA DIVISION.
00059
00060  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00061
00062  77  FILLER  PIC X(32)  VALUE '********************************'.
00063  77  FILLER  PIC X(32)  VALUE '*   EL1323 WORKING STORAGE     *'.
00064  77  FILLER  PIC X(32)  VALUE '********** VMOD=2.008 **********'.
00065
00066  01  WS-DATE-AREA.
00067      12  SAVE-DATE               PIC X(8)    VALUE SPACES.
00068      12  SAVE-BIN-DATE           PIC X(2)    VALUE SPACES.
00069
00070
00071  01  ERROR-MESSAGES.
00072      12  ER-0133                 PIC X(4)  VALUE '0133'.
00073      12  ER-0205                 PIC X(4)  VALUE '0205'.
00074      12  ER-2848                 PIC X(4)  VALUE '2848'.
00075      12  ER-9288                 PIC X(4)  VALUE '9288'.
00076
00077  01  FILLER    COMP-3.
00078
00079      12  WS-TIME-WORK            PIC S9(7)   VALUE ZERO.
00080      12  WS-TIME      REDEFINES
00081          WS-TIME-WORK            PIC S9(3)V9(4).
00082
00083      12  WS-ELAPSED-MONTHS       PIC S9(3)   VALUE ZERO.
00084
00085  01  FILLER.
00086
00087      12  WS-CONTROL-FILE-KEY.
00088          16  WS-CFK-COMPANY-ID   PIC X(3)    VALUE SPACES.
00089          16  WS-CFK-RECORD-TYPE  PIC X       VALUE ZERO.
00090 *            88  LF-BENEFIT-MASTER           VALUE '4'.
00091 *            88  AH-BENEFIT-MASTER           VALUE '5'.
00092          16  WS-CFK-ACCESS-TYPE.
00093              20 WS-CFK-STATE-ACCESS.
00094                 24  WS-CFK-STATE    PIC XX.
00095                 24  FILLER          PIC XX.
00096              20  WS-CFK-BENEFIT-NO REDEFINES WS-CFK-STATE-ACCESS.
00097                 24  FILLER          PIC XX.
00098                 24  WS-CFK-BENEFIT  PIC XX.
00099          16  WS-CFK-SEQUENCE-NO  PIC S9(4)   VALUE +0    COMP.
00100
00101      12  WS-CLAIM-KEY.
00102          16  WS-CL-COMPANY-CD    PIC X.
00103          16  WS-CL-CARRIER       PIC X.
00104          16  WS-CL-CLAIM-NO      PIC X(7).
00105          16  WS-CL-CERT-NO       PIC X(11).
00106
00107      12  WS-ELTRLR-KEY.
00108          16  FILLER              PIC X(20).
00109          16  WS-ELTRLR-SEQ-NO    PIC S9(04) COMP.
00110
00111      12  WS-CERTIFICATE-KEY.
00112          16  WS-CK-COMPANY-CD    PIC X.
00113          16  WS-CK-CARRIER       PIC X.
00114          16  WS-CK-GROUPING      PIC X(6).
00115          16  WS-CK-STATE         PIC XX.
00116          16  WS-CK-ACCOUNT       PIC X(10).
00117          16  WS-CK-CERT-EFF-DT   PIC XX.
00118          16  WS-CK-CERT-NO.
00119              20  WS-CK-CERT-PRIME PIC X(10).
00120              20  WS-CK-CERT-SFX  PIC X.
00121
00122      12  EMPLCY-KEY.
00123          16  EMPLCY-COMPANY-CD   PIC X(01).
00124          16  EMPLCY-CARRIER      PIC X(01).
00125          16  EMPLCY-GROUPING     PIC X(06).
00126          16  EMPLCY-STATE        PIC X(02).
00127          16  EMPLCY-PRODUCER     PIC X(10).
00128          16  EMPLCY-EFF-DT       PIC X(02).
00129          16  EMPLCY-REFERENCE-NO PIC X(20).
00130
00131      12  EMPLAN-KEY.
00132          16  EMPLAN-COMPANY-CD   PIC X(01).
00133          16  EMPLAN-CARRIER      PIC X(01).
00134          16  EMPLAN-GROUPING     PIC X(06).
00135          16  EMPLAN-STATE        PIC X(02).
00136          16  EMPLAN-PRODUCER     PIC X(10).
00137          16  EMPLAN-PLAN-CODE    PIC X(02).
00138          16  EMPLAN-REV-NO       PIC 9(03).
00139
00140      12  WS-BENEFIT-NO           PIC XX      VALUE ZERO.
00141      12  WS-KIND                 PIC X(3)    VALUE SPACES.
00142      12  WS-NAME-WORK            PIC X(3)    VALUE SPACES.
00143      12  WS-MAPSET-NAME          PIC X(8)    VALUE 'EL132S  '.
00144      12  WS-MAP-NAME             PIC X(8)    VALUE 'EL132C  '.
00145
00146      12  FILLER       REDEFINES
00147          WS-MAP-NAME.
00148          16  FILLER              PIC XX.
00149          16  WS-MAP-NUMBER       PIC X(4).
00150          16  FILLER              PIC XX.
00151
00152      12  WS-SAVE-CURRENT-DATE    PIC XX.
00153      12  WS-PROGRAM-ID           PIC X(8)  VALUE 'EL1323  '.
00154
00155      12  WS-CONTROL-FILE-DSID    PIC X(8) VALUE 'ELCNTL  '.
00156      12  WS-CERTIFICATE-MASTER-DSID  PIC X(8) VALUE 'ELCERT  '.
00157      12  WS-CLAIM-MASTER-DSID    PIC X(8) VALUE 'ELMSTR'.
00158      12  WS-ACTIVITY-TRAILERS-DSID   PIC X(8) VALUE 'ELTRLR  '.
00159      12  WS-EMPLCY-DSID          PIC X(8)    VALUE 'MPPLCY'.
00160      12  WS-EMPLAN-DSID          PIC X(8)    VALUE 'MPPLAN'.
00161
00162      12  WS-TRANS-ID             PIC X(4)    VALUE 'XXXX'.
00163
00164      12  WS-AGE                  PIC 9(04)  VALUE ZEROS.
00165      12  WS-AGE-R REDEFINES WS-AGE.
00166          16  WS-AGE-1-2          PIC 9(02).
00167          16  WS-AGE-3-4          PIC 9(02).
00168
00169      12  WS-RESPONSE             PIC S9(8) COMP.
00170          88  WS-RESP-NORMAL                VALUE +00.
00171          88  WS-RESP-NOTFND                VALUE +13.
00172
00173      12  WS-INDEX                PIC S9(4)   VALUE +0 SYNC COMP.
00174      12  WS-WORK-SEQU.
00175          16  FILLER              PIC X(01) VALUE SPACES.
00176          16  WS-CLM-POSITION     PIC 9(02).
00177          16  FILLER              PIC X(04) VALUE ' OF '.
00178          16  WS-CLM-TOTAL        PIC 9(02).
00179          16  FILLER              PIC X(01) VALUE SPACES.
00180
00181      EJECT
00182 *    COPY ELCINTF.
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
00183
00184      EJECT
00185 *    COPY ELCDATE.
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
00186
00187      EJECT
00188 *    COPY EL132S.
       01  EL132BI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  BDATEL PIC S9(0004) COMP.
           05  BDATEF PIC  X(0001).
           05  FILLER REDEFINES BDATEF.
               10  BDATEA PIC  X(0001).
           05  BDATEI PIC  X(0008).
      *    -------------------------------
           05  BTIMEL PIC S9(0004) COMP.
           05  BTIMEF PIC  X(0001).
           05  FILLER REDEFINES BTIMEF.
               10  BTIMEA PIC  X(0001).
           05  BTIMEI PIC  X(0005).
      *    -------------------------------
           05  BHEAD1L PIC S9(0004) COMP.
           05  BHEAD1F PIC  X(0001).
           05  FILLER REDEFINES BHEAD1F.
               10  BHEAD1A PIC  X(0001).
           05  BHEAD1I PIC  X(0038).
      *    -------------------------------
           05  BCOMPL PIC S9(0004) COMP.
           05  BCOMPF PIC  X(0001).
           05  FILLER REDEFINES BCOMPF.
               10  BCOMPA PIC  X(0001).
           05  BCOMPI PIC  X(0003).
      *    -------------------------------
           05  BUSERIDL PIC S9(0004) COMP.
           05  BUSERIDF PIC  X(0001).
           05  FILLER REDEFINES BUSERIDF.
               10  BUSERIDA PIC  X(0001).
           05  BUSERIDI PIC  X(0004).
      *    -------------------------------
           05  BCERTL PIC S9(0004) COMP.
           05  BCERTF PIC  X(0001).
           05  FILLER REDEFINES BCERTF.
               10  BCERTA PIC  X(0001).
           05  BCERTI PIC  X(0015).
      *    -------------------------------
           05  BNUM01L PIC S9(0004) COMP.
           05  BNUM01F PIC  X(0001).
           05  FILLER REDEFINES BNUM01F.
               10  BNUM01A PIC  X(0001).
           05  BNUM01I PIC  X(0002).
      *    -------------------------------
           05  BNAME01L PIC S9(0004) COMP.
           05  BNAME01F PIC  X(0001).
           05  FILLER REDEFINES BNAME01F.
               10  BNAME01A PIC  X(0001).
           05  BNAME01I PIC  X(0022).
      *    -------------------------------
           05  BAGE01L PIC S9(0004) COMP.
           05  BAGE01F PIC  X(0001).
           05  FILLER REDEFINES BAGE01F.
               10  BAGE01A PIC  X(0001).
           05  BAGE01I PIC  X(0002).
      *    -------------------------------
           05  BSTA01L PIC S9(0004) COMP.
           05  BSTA01F PIC  X(0001).
           05  FILLER REDEFINES BSTA01F.
               10  BSTA01A PIC  X(0001).
           05  BSTA01I PIC  X(0001).
      *    -------------------------------
           05  BIDT01L PIC S9(0004) COMP.
           05  BIDT01F PIC  X(0001).
           05  FILLER REDEFINES BIDT01F.
               10  BIDT01A PIC  X(0001).
           05  BIDT01I PIC  X(0008).
      *    -------------------------------
           05  BTYPE01L PIC S9(0004) COMP.
           05  BTYPE01F PIC  X(0001).
           05  FILLER REDEFINES BTYPE01F.
               10  BTYPE01A PIC  X(0001).
           05  BTYPE01I PIC  X(0001).
      *    -------------------------------
           05  BCARR01L PIC S9(0004) COMP.
           05  BCARR01F PIC  X(0001).
           05  FILLER REDEFINES BCARR01F.
               10  BCARR01A PIC  X(0001).
           05  BCARR01I PIC  X(0001).
      *    -------------------------------
           05  BCLAM01L PIC S9(0004) COMP.
           05  BCLAM01F PIC  X(0001).
           05  FILLER REDEFINES BCLAM01F.
               10  BCLAM01A PIC  X(0001).
           05  BCLAM01I PIC  X(0007).
      *    -------------------------------
           05  BCERT01L PIC S9(0004) COMP.
           05  BCERT01F PIC  X(0001).
           05  FILLER REDEFINES BCERT01F.
               10  BCERT01A PIC  X(0001).
           05  BCERT01I PIC  X(0016).
      *    -------------------------------
           05  BACCT01L PIC S9(0004) COMP.
           05  BACCT01F PIC  X(0001).
           05  FILLER REDEFINES BACCT01F.
               10  BACCT01A PIC  X(0001).
           05  BACCT01I PIC  X(0010).
      *    -------------------------------
           05  BNUM02L PIC S9(0004) COMP.
           05  BNUM02F PIC  X(0001).
           05  FILLER REDEFINES BNUM02F.
               10  BNUM02A PIC  X(0001).
           05  BNUM02I PIC  X(0002).
      *    -------------------------------
           05  BNAME02L PIC S9(0004) COMP.
           05  BNAME02F PIC  X(0001).
           05  FILLER REDEFINES BNAME02F.
               10  BNAME02A PIC  X(0001).
           05  BNAME02I PIC  X(0022).
      *    -------------------------------
           05  BAGE02L PIC S9(0004) COMP.
           05  BAGE02F PIC  X(0001).
           05  FILLER REDEFINES BAGE02F.
               10  BAGE02A PIC  X(0001).
           05  BAGE02I PIC  X(0002).
      *    -------------------------------
           05  BSTA02L PIC S9(0004) COMP.
           05  BSTA02F PIC  X(0001).
           05  FILLER REDEFINES BSTA02F.
               10  BSTA02A PIC  X(0001).
           05  BSTA02I PIC  X(0001).
      *    -------------------------------
           05  BIDT02L PIC S9(0004) COMP.
           05  BIDT02F PIC  X(0001).
           05  FILLER REDEFINES BIDT02F.
               10  BIDT02A PIC  X(0001).
           05  BIDT02I PIC  X(0008).
      *    -------------------------------
           05  BTYPE02L PIC S9(0004) COMP.
           05  BTYPE02F PIC  X(0001).
           05  FILLER REDEFINES BTYPE02F.
               10  BTYPE02A PIC  X(0001).
           05  BTYPE02I PIC  X(0001).
      *    -------------------------------
           05  BCARR02L PIC S9(0004) COMP.
           05  BCARR02F PIC  X(0001).
           05  FILLER REDEFINES BCARR02F.
               10  BCARR02A PIC  X(0001).
           05  BCARR02I PIC  X(0001).
      *    -------------------------------
           05  BCLAM02L PIC S9(0004) COMP.
           05  BCLAM02F PIC  X(0001).
           05  FILLER REDEFINES BCLAM02F.
               10  BCLAM02A PIC  X(0001).
           05  BCLAM02I PIC  X(0007).
      *    -------------------------------
           05  BCERT02L PIC S9(0004) COMP.
           05  BCERT02F PIC  X(0001).
           05  FILLER REDEFINES BCERT02F.
               10  BCERT02A PIC  X(0001).
           05  BCERT02I PIC  X(0016).
      *    -------------------------------
           05  BACCT02L PIC S9(0004) COMP.
           05  BACCT02F PIC  X(0001).
           05  FILLER REDEFINES BACCT02F.
               10  BACCT02A PIC  X(0001).
           05  BACCT02I PIC  X(0010).
      *    -------------------------------
           05  BNUM03L PIC S9(0004) COMP.
           05  BNUM03F PIC  X(0001).
           05  FILLER REDEFINES BNUM03F.
               10  BNUM03A PIC  X(0001).
           05  BNUM03I PIC  X(0002).
      *    -------------------------------
           05  BNAME03L PIC S9(0004) COMP.
           05  BNAME03F PIC  X(0001).
           05  FILLER REDEFINES BNAME03F.
               10  BNAME03A PIC  X(0001).
           05  BNAME03I PIC  X(0022).
      *    -------------------------------
           05  BAGE03L PIC S9(0004) COMP.
           05  BAGE03F PIC  X(0001).
           05  FILLER REDEFINES BAGE03F.
               10  BAGE03A PIC  X(0001).
           05  BAGE03I PIC  X(0002).
      *    -------------------------------
           05  BSTA03L PIC S9(0004) COMP.
           05  BSTA03F PIC  X(0001).
           05  FILLER REDEFINES BSTA03F.
               10  BSTA03A PIC  X(0001).
           05  BSTA03I PIC  X(0001).
      *    -------------------------------
           05  BIDT03L PIC S9(0004) COMP.
           05  BIDT03F PIC  X(0001).
           05  FILLER REDEFINES BIDT03F.
               10  BIDT03A PIC  X(0001).
           05  BIDT03I PIC  X(0008).
      *    -------------------------------
           05  BTYPE03L PIC S9(0004) COMP.
           05  BTYPE03F PIC  X(0001).
           05  FILLER REDEFINES BTYPE03F.
               10  BTYPE03A PIC  X(0001).
           05  BTYPE03I PIC  X(0001).
      *    -------------------------------
           05  BCARR03L PIC S9(0004) COMP.
           05  BCARR03F PIC  X(0001).
           05  FILLER REDEFINES BCARR03F.
               10  BCARR03A PIC  X(0001).
           05  BCARR03I PIC  X(0001).
      *    -------------------------------
           05  BCLAM03L PIC S9(0004) COMP.
           05  BCLAM03F PIC  X(0001).
           05  FILLER REDEFINES BCLAM03F.
               10  BCLAM03A PIC  X(0001).
           05  BCLAM03I PIC  X(0007).
      *    -------------------------------
           05  BCERT03L PIC S9(0004) COMP.
           05  BCERT03F PIC  X(0001).
           05  FILLER REDEFINES BCERT03F.
               10  BCERT03A PIC  X(0001).
           05  BCERT03I PIC  X(0016).
      *    -------------------------------
           05  BACCT03L PIC S9(0004) COMP.
           05  BACCT03F PIC  X(0001).
           05  FILLER REDEFINES BACCT03F.
               10  BACCT03A PIC  X(0001).
           05  BACCT03I PIC  X(0010).
      *    -------------------------------
           05  BNUM04L PIC S9(0004) COMP.
           05  BNUM04F PIC  X(0001).
           05  FILLER REDEFINES BNUM04F.
               10  BNUM04A PIC  X(0001).
           05  BNUM04I PIC  X(0002).
      *    -------------------------------
           05  BNAME04L PIC S9(0004) COMP.
           05  BNAME04F PIC  X(0001).
           05  FILLER REDEFINES BNAME04F.
               10  BNAME04A PIC  X(0001).
           05  BNAME04I PIC  X(0022).
      *    -------------------------------
           05  BAGE04L PIC S9(0004) COMP.
           05  BAGE04F PIC  X(0001).
           05  FILLER REDEFINES BAGE04F.
               10  BAGE04A PIC  X(0001).
           05  BAGE04I PIC  X(0002).
      *    -------------------------------
           05  BSTA04L PIC S9(0004) COMP.
           05  BSTA04F PIC  X(0001).
           05  FILLER REDEFINES BSTA04F.
               10  BSTA04A PIC  X(0001).
           05  BSTA04I PIC  X(0001).
      *    -------------------------------
           05  BIDT04L PIC S9(0004) COMP.
           05  BIDT04F PIC  X(0001).
           05  FILLER REDEFINES BIDT04F.
               10  BIDT04A PIC  X(0001).
           05  BIDT04I PIC  X(0008).
      *    -------------------------------
           05  BTYPE04L PIC S9(0004) COMP.
           05  BTYPE04F PIC  X(0001).
           05  FILLER REDEFINES BTYPE04F.
               10  BTYPE04A PIC  X(0001).
           05  BTYPE04I PIC  X(0001).
      *    -------------------------------
           05  BCARR04L PIC S9(0004) COMP.
           05  BCARR04F PIC  X(0001).
           05  FILLER REDEFINES BCARR04F.
               10  BCARR04A PIC  X(0001).
           05  BCARR04I PIC  X(0001).
      *    -------------------------------
           05  BCLAM04L PIC S9(0004) COMP.
           05  BCLAM04F PIC  X(0001).
           05  FILLER REDEFINES BCLAM04F.
               10  BCLAM04A PIC  X(0001).
           05  BCLAM04I PIC  X(0007).
      *    -------------------------------
           05  BCERT04L PIC S9(0004) COMP.
           05  BCERT04F PIC  X(0001).
           05  FILLER REDEFINES BCERT04F.
               10  BCERT04A PIC  X(0001).
           05  BCERT04I PIC  X(0016).
      *    -------------------------------
           05  BACCT04L PIC S9(0004) COMP.
           05  BACCT04F PIC  X(0001).
           05  FILLER REDEFINES BACCT04F.
               10  BACCT04A PIC  X(0001).
           05  BACCT04I PIC  X(0010).
      *    -------------------------------
           05  BNUM05L PIC S9(0004) COMP.
           05  BNUM05F PIC  X(0001).
           05  FILLER REDEFINES BNUM05F.
               10  BNUM05A PIC  X(0001).
           05  BNUM05I PIC  X(0002).
      *    -------------------------------
           05  BNAME05L PIC S9(0004) COMP.
           05  BNAME05F PIC  X(0001).
           05  FILLER REDEFINES BNAME05F.
               10  BNAME05A PIC  X(0001).
           05  BNAME05I PIC  X(0022).
      *    -------------------------------
           05  BAGE05L PIC S9(0004) COMP.
           05  BAGE05F PIC  X(0001).
           05  FILLER REDEFINES BAGE05F.
               10  BAGE05A PIC  X(0001).
           05  BAGE05I PIC  X(0002).
      *    -------------------------------
           05  BSTA05L PIC S9(0004) COMP.
           05  BSTA05F PIC  X(0001).
           05  FILLER REDEFINES BSTA05F.
               10  BSTA05A PIC  X(0001).
           05  BSTA05I PIC  X(0001).
      *    -------------------------------
           05  BIDT05L PIC S9(0004) COMP.
           05  BIDT05F PIC  X(0001).
           05  FILLER REDEFINES BIDT05F.
               10  BIDT05A PIC  X(0001).
           05  BIDT05I PIC  X(0008).
      *    -------------------------------
           05  BTYPE05L PIC S9(0004) COMP.
           05  BTYPE05F PIC  X(0001).
           05  FILLER REDEFINES BTYPE05F.
               10  BTYPE05A PIC  X(0001).
           05  BTYPE05I PIC  X(0001).
      *    -------------------------------
           05  BCARR05L PIC S9(0004) COMP.
           05  BCARR05F PIC  X(0001).
           05  FILLER REDEFINES BCARR05F.
               10  BCARR05A PIC  X(0001).
           05  BCARR05I PIC  X(0001).
      *    -------------------------------
           05  BCLAM05L PIC S9(0004) COMP.
           05  BCLAM05F PIC  X(0001).
           05  FILLER REDEFINES BCLAM05F.
               10  BCLAM05A PIC  X(0001).
           05  BCLAM05I PIC  X(0007).
      *    -------------------------------
           05  BCERT05L PIC S9(0004) COMP.
           05  BCERT05F PIC  X(0001).
           05  FILLER REDEFINES BCERT05F.
               10  BCERT05A PIC  X(0001).
           05  BCERT05I PIC  X(0016).
      *    -------------------------------
           05  BACCT05L PIC S9(0004) COMP.
           05  BACCT05F PIC  X(0001).
           05  FILLER REDEFINES BACCT05F.
               10  BACCT05A PIC  X(0001).
           05  BACCT05I PIC  X(0010).
      *    -------------------------------
           05  BNUM06L PIC S9(0004) COMP.
           05  BNUM06F PIC  X(0001).
           05  FILLER REDEFINES BNUM06F.
               10  BNUM06A PIC  X(0001).
           05  BNUM06I PIC  X(0002).
      *    -------------------------------
           05  BNAME06L PIC S9(0004) COMP.
           05  BNAME06F PIC  X(0001).
           05  FILLER REDEFINES BNAME06F.
               10  BNAME06A PIC  X(0001).
           05  BNAME06I PIC  X(0022).
      *    -------------------------------
           05  BAGE06L PIC S9(0004) COMP.
           05  BAGE06F PIC  X(0001).
           05  FILLER REDEFINES BAGE06F.
               10  BAGE06A PIC  X(0001).
           05  BAGE06I PIC  X(0002).
      *    -------------------------------
           05  BSTA06L PIC S9(0004) COMP.
           05  BSTA06F PIC  X(0001).
           05  FILLER REDEFINES BSTA06F.
               10  BSTA06A PIC  X(0001).
           05  BSTA06I PIC  X(0001).
      *    -------------------------------
           05  BIDT06L PIC S9(0004) COMP.
           05  BIDT06F PIC  X(0001).
           05  FILLER REDEFINES BIDT06F.
               10  BIDT06A PIC  X(0001).
           05  BIDT06I PIC  X(0008).
      *    -------------------------------
           05  BTYPE06L PIC S9(0004) COMP.
           05  BTYPE06F PIC  X(0001).
           05  FILLER REDEFINES BTYPE06F.
               10  BTYPE06A PIC  X(0001).
           05  BTYPE06I PIC  X(0001).
      *    -------------------------------
           05  BCARR06L PIC S9(0004) COMP.
           05  BCARR06F PIC  X(0001).
           05  FILLER REDEFINES BCARR06F.
               10  BCARR06A PIC  X(0001).
           05  BCARR06I PIC  X(0001).
      *    -------------------------------
           05  BCLAM06L PIC S9(0004) COMP.
           05  BCLAM06F PIC  X(0001).
           05  FILLER REDEFINES BCLAM06F.
               10  BCLAM06A PIC  X(0001).
           05  BCLAM06I PIC  X(0007).
      *    -------------------------------
           05  BCERT06L PIC S9(0004) COMP.
           05  BCERT06F PIC  X(0001).
           05  FILLER REDEFINES BCERT06F.
               10  BCERT06A PIC  X(0001).
           05  BCERT06I PIC  X(0016).
      *    -------------------------------
           05  BACCT06L PIC S9(0004) COMP.
           05  BACCT06F PIC  X(0001).
           05  FILLER REDEFINES BACCT06F.
               10  BACCT06A PIC  X(0001).
           05  BACCT06I PIC  X(0010).
      *    -------------------------------
           05  BNUM07L PIC S9(0004) COMP.
           05  BNUM07F PIC  X(0001).
           05  FILLER REDEFINES BNUM07F.
               10  BNUM07A PIC  X(0001).
           05  BNUM07I PIC  X(0002).
      *    -------------------------------
           05  BNAME07L PIC S9(0004) COMP.
           05  BNAME07F PIC  X(0001).
           05  FILLER REDEFINES BNAME07F.
               10  BNAME07A PIC  X(0001).
           05  BNAME07I PIC  X(0022).
      *    -------------------------------
           05  BAGE07L PIC S9(0004) COMP.
           05  BAGE07F PIC  X(0001).
           05  FILLER REDEFINES BAGE07F.
               10  BAGE07A PIC  X(0001).
           05  BAGE07I PIC  X(0002).
      *    -------------------------------
           05  BSTA07L PIC S9(0004) COMP.
           05  BSTA07F PIC  X(0001).
           05  FILLER REDEFINES BSTA07F.
               10  BSTA07A PIC  X(0001).
           05  BSTA07I PIC  X(0001).
      *    -------------------------------
           05  BIDT07L PIC S9(0004) COMP.
           05  BIDT07F PIC  X(0001).
           05  FILLER REDEFINES BIDT07F.
               10  BIDT07A PIC  X(0001).
           05  BIDT07I PIC  X(0008).
      *    -------------------------------
           05  BTYPE07L PIC S9(0004) COMP.
           05  BTYPE07F PIC  X(0001).
           05  FILLER REDEFINES BTYPE07F.
               10  BTYPE07A PIC  X(0001).
           05  BTYPE07I PIC  X(0001).
      *    -------------------------------
           05  BCARR07L PIC S9(0004) COMP.
           05  BCARR07F PIC  X(0001).
           05  FILLER REDEFINES BCARR07F.
               10  BCARR07A PIC  X(0001).
           05  BCARR07I PIC  X(0001).
      *    -------------------------------
           05  BCLAM07L PIC S9(0004) COMP.
           05  BCLAM07F PIC  X(0001).
           05  FILLER REDEFINES BCLAM07F.
               10  BCLAM07A PIC  X(0001).
           05  BCLAM07I PIC  X(0007).
      *    -------------------------------
           05  BCERT07L PIC S9(0004) COMP.
           05  BCERT07F PIC  X(0001).
           05  FILLER REDEFINES BCERT07F.
               10  BCERT07A PIC  X(0001).
           05  BCERT07I PIC  X(0016).
      *    -------------------------------
           05  BACCT07L PIC S9(0004) COMP.
           05  BACCT07F PIC  X(0001).
           05  FILLER REDEFINES BACCT07F.
               10  BACCT07A PIC  X(0001).
           05  BACCT07I PIC  X(0010).
      *    -------------------------------
           05  BNUM08L PIC S9(0004) COMP.
           05  BNUM08F PIC  X(0001).
           05  FILLER REDEFINES BNUM08F.
               10  BNUM08A PIC  X(0001).
           05  BNUM08I PIC  X(0002).
      *    -------------------------------
           05  BNAME08L PIC S9(0004) COMP.
           05  BNAME08F PIC  X(0001).
           05  FILLER REDEFINES BNAME08F.
               10  BNAME08A PIC  X(0001).
           05  BNAME08I PIC  X(0022).
      *    -------------------------------
           05  BAGE08L PIC S9(0004) COMP.
           05  BAGE08F PIC  X(0001).
           05  FILLER REDEFINES BAGE08F.
               10  BAGE08A PIC  X(0001).
           05  BAGE08I PIC  X(0002).
      *    -------------------------------
           05  BSTA08L PIC S9(0004) COMP.
           05  BSTA08F PIC  X(0001).
           05  FILLER REDEFINES BSTA08F.
               10  BSTA08A PIC  X(0001).
           05  BSTA08I PIC  X(0001).
      *    -------------------------------
           05  BIDT08L PIC S9(0004) COMP.
           05  BIDT08F PIC  X(0001).
           05  FILLER REDEFINES BIDT08F.
               10  BIDT08A PIC  X(0001).
           05  BIDT08I PIC  X(0008).
      *    -------------------------------
           05  BTYPE08L PIC S9(0004) COMP.
           05  BTYPE08F PIC  X(0001).
           05  FILLER REDEFINES BTYPE08F.
               10  BTYPE08A PIC  X(0001).
           05  BTYPE08I PIC  X(0001).
      *    -------------------------------
           05  BCARR08L PIC S9(0004) COMP.
           05  BCARR08F PIC  X(0001).
           05  FILLER REDEFINES BCARR08F.
               10  BCARR08A PIC  X(0001).
           05  BCARR08I PIC  X(0001).
      *    -------------------------------
           05  BCLAM08L PIC S9(0004) COMP.
           05  BCLAM08F PIC  X(0001).
           05  FILLER REDEFINES BCLAM08F.
               10  BCLAM08A PIC  X(0001).
           05  BCLAM08I PIC  X(0007).
      *    -------------------------------
           05  BCERT08L PIC S9(0004) COMP.
           05  BCERT08F PIC  X(0001).
           05  FILLER REDEFINES BCERT08F.
               10  BCERT08A PIC  X(0001).
           05  BCERT08I PIC  X(0016).
      *    -------------------------------
           05  BACCT08L PIC S9(0004) COMP.
           05  BACCT08F PIC  X(0001).
           05  FILLER REDEFINES BACCT08F.
               10  BACCT08A PIC  X(0001).
           05  BACCT08I PIC  X(0010).
      *    -------------------------------
           05  BNUM09L PIC S9(0004) COMP.
           05  BNUM09F PIC  X(0001).
           05  FILLER REDEFINES BNUM09F.
               10  BNUM09A PIC  X(0001).
           05  BNUM09I PIC  X(0002).
      *    -------------------------------
           05  BNAME09L PIC S9(0004) COMP.
           05  BNAME09F PIC  X(0001).
           05  FILLER REDEFINES BNAME09F.
               10  BNAME09A PIC  X(0001).
           05  BNAME09I PIC  X(0022).
      *    -------------------------------
           05  BAGE09L PIC S9(0004) COMP.
           05  BAGE09F PIC  X(0001).
           05  FILLER REDEFINES BAGE09F.
               10  BAGE09A PIC  X(0001).
           05  BAGE09I PIC  X(0002).
      *    -------------------------------
           05  BSTA09L PIC S9(0004) COMP.
           05  BSTA09F PIC  X(0001).
           05  FILLER REDEFINES BSTA09F.
               10  BSTA09A PIC  X(0001).
           05  BSTA09I PIC  X(0001).
      *    -------------------------------
           05  BIDT09L PIC S9(0004) COMP.
           05  BIDT09F PIC  X(0001).
           05  FILLER REDEFINES BIDT09F.
               10  BIDT09A PIC  X(0001).
           05  BIDT09I PIC  X(0008).
      *    -------------------------------
           05  BTYPE09L PIC S9(0004) COMP.
           05  BTYPE09F PIC  X(0001).
           05  FILLER REDEFINES BTYPE09F.
               10  BTYPE09A PIC  X(0001).
           05  BTYPE09I PIC  X(0001).
      *    -------------------------------
           05  BCARR09L PIC S9(0004) COMP.
           05  BCARR09F PIC  X(0001).
           05  FILLER REDEFINES BCARR09F.
               10  BCARR09A PIC  X(0001).
           05  BCARR09I PIC  X(0001).
      *    -------------------------------
           05  BCLAM09L PIC S9(0004) COMP.
           05  BCLAM09F PIC  X(0001).
           05  FILLER REDEFINES BCLAM09F.
               10  BCLAM09A PIC  X(0001).
           05  BCLAM09I PIC  X(0007).
      *    -------------------------------
           05  BCERT09L PIC S9(0004) COMP.
           05  BCERT09F PIC  X(0001).
           05  FILLER REDEFINES BCERT09F.
               10  BCERT09A PIC  X(0001).
           05  BCERT09I PIC  X(0016).
      *    -------------------------------
           05  BACCT09L PIC S9(0004) COMP.
           05  BACCT09F PIC  X(0001).
           05  FILLER REDEFINES BACCT09F.
               10  BACCT09A PIC  X(0001).
           05  BACCT09I PIC  X(0010).
      *    -------------------------------
           05  BNUM10L PIC S9(0004) COMP.
           05  BNUM10F PIC  X(0001).
           05  FILLER REDEFINES BNUM10F.
               10  BNUM10A PIC  X(0001).
           05  BNUM10I PIC  X(0002).
      *    -------------------------------
           05  BNAME10L PIC S9(0004) COMP.
           05  BNAME10F PIC  X(0001).
           05  FILLER REDEFINES BNAME10F.
               10  BNAME10A PIC  X(0001).
           05  BNAME10I PIC  X(0022).
      *    -------------------------------
           05  BAGE10L PIC S9(0004) COMP.
           05  BAGE10F PIC  X(0001).
           05  FILLER REDEFINES BAGE10F.
               10  BAGE10A PIC  X(0001).
           05  BAGE10I PIC  X(0002).
      *    -------------------------------
           05  BSTA10L PIC S9(0004) COMP.
           05  BSTA10F PIC  X(0001).
           05  FILLER REDEFINES BSTA10F.
               10  BSTA10A PIC  X(0001).
           05  BSTA10I PIC  X(0001).
      *    -------------------------------
           05  BIDT10L PIC S9(0004) COMP.
           05  BIDT10F PIC  X(0001).
           05  FILLER REDEFINES BIDT10F.
               10  BIDT10A PIC  X(0001).
           05  BIDT10I PIC  X(0008).
      *    -------------------------------
           05  BTYPE10L PIC S9(0004) COMP.
           05  BTYPE10F PIC  X(0001).
           05  FILLER REDEFINES BTYPE10F.
               10  BTYPE10A PIC  X(0001).
           05  BTYPE10I PIC  X(0001).
      *    -------------------------------
           05  BCARR10L PIC S9(0004) COMP.
           05  BCARR10F PIC  X(0001).
           05  FILLER REDEFINES BCARR10F.
               10  BCARR10A PIC  X(0001).
           05  BCARR10I PIC  X(0001).
      *    -------------------------------
           05  BCLAM10L PIC S9(0004) COMP.
           05  BCLAM10F PIC  X(0001).
           05  FILLER REDEFINES BCLAM10F.
               10  BCLAM10A PIC  X(0001).
           05  BCLAM10I PIC  X(0007).
      *    -------------------------------
           05  BCERT10L PIC S9(0004) COMP.
           05  BCERT10F PIC  X(0001).
           05  FILLER REDEFINES BCERT10F.
               10  BCERT10A PIC  X(0001).
           05  BCERT10I PIC  X(0016).
      *    -------------------------------
           05  BACCT10L PIC S9(0004) COMP.
           05  BACCT10F PIC  X(0001).
           05  FILLER REDEFINES BACCT10F.
               10  BACCT10A PIC  X(0001).
           05  BACCT10I PIC  X(0010).
      *    -------------------------------
           05  BNUM11L PIC S9(0004) COMP.
           05  BNUM11F PIC  X(0001).
           05  FILLER REDEFINES BNUM11F.
               10  BNUM11A PIC  X(0001).
           05  BNUM11I PIC  X(0002).
      *    -------------------------------
           05  BNAME11L PIC S9(0004) COMP.
           05  BNAME11F PIC  X(0001).
           05  FILLER REDEFINES BNAME11F.
               10  BNAME11A PIC  X(0001).
           05  BNAME11I PIC  X(0022).
      *    -------------------------------
           05  BAGE11L PIC S9(0004) COMP.
           05  BAGE11F PIC  X(0001).
           05  FILLER REDEFINES BAGE11F.
               10  BAGE11A PIC  X(0001).
           05  BAGE11I PIC  X(0002).
      *    -------------------------------
           05  BSTA11L PIC S9(0004) COMP.
           05  BSTA11F PIC  X(0001).
           05  FILLER REDEFINES BSTA11F.
               10  BSTA11A PIC  X(0001).
           05  BSTA11I PIC  X(0001).
      *    -------------------------------
           05  BIDT11L PIC S9(0004) COMP.
           05  BIDT11F PIC  X(0001).
           05  FILLER REDEFINES BIDT11F.
               10  BIDT11A PIC  X(0001).
           05  BIDT11I PIC  X(0008).
      *    -------------------------------
           05  BTYPE11L PIC S9(0004) COMP.
           05  BTYPE11F PIC  X(0001).
           05  FILLER REDEFINES BTYPE11F.
               10  BTYPE11A PIC  X(0001).
           05  BTYPE11I PIC  X(0001).
      *    -------------------------------
           05  BCARR11L PIC S9(0004) COMP.
           05  BCARR11F PIC  X(0001).
           05  FILLER REDEFINES BCARR11F.
               10  BCARR11A PIC  X(0001).
           05  BCARR11I PIC  X(0001).
      *    -------------------------------
           05  BCLAM11L PIC S9(0004) COMP.
           05  BCLAM11F PIC  X(0001).
           05  FILLER REDEFINES BCLAM11F.
               10  BCLAM11A PIC  X(0001).
           05  BCLAM11I PIC  X(0007).
      *    -------------------------------
           05  BCERT11L PIC S9(0004) COMP.
           05  BCERT11F PIC  X(0001).
           05  FILLER REDEFINES BCERT11F.
               10  BCERT11A PIC  X(0001).
           05  BCERT11I PIC  X(0016).
      *    -------------------------------
           05  BACCT11L PIC S9(0004) COMP.
           05  BACCT11F PIC  X(0001).
           05  FILLER REDEFINES BACCT11F.
               10  BACCT11A PIC  X(0001).
           05  BACCT11I PIC  X(0010).
      *    -------------------------------
           05  BNUM12L PIC S9(0004) COMP.
           05  BNUM12F PIC  X(0001).
           05  FILLER REDEFINES BNUM12F.
               10  BNUM12A PIC  X(0001).
           05  BNUM12I PIC  X(0002).
      *    -------------------------------
           05  BNAME12L PIC S9(0004) COMP.
           05  BNAME12F PIC  X(0001).
           05  FILLER REDEFINES BNAME12F.
               10  BNAME12A PIC  X(0001).
           05  BNAME12I PIC  X(0022).
      *    -------------------------------
           05  BAGE12L PIC S9(0004) COMP.
           05  BAGE12F PIC  X(0001).
           05  FILLER REDEFINES BAGE12F.
               10  BAGE12A PIC  X(0001).
           05  BAGE12I PIC  X(0002).
      *    -------------------------------
           05  BSTA12L PIC S9(0004) COMP.
           05  BSTA12F PIC  X(0001).
           05  FILLER REDEFINES BSTA12F.
               10  BSTA12A PIC  X(0001).
           05  BSTA12I PIC  X(0001).
      *    -------------------------------
           05  BIDT12L PIC S9(0004) COMP.
           05  BIDT12F PIC  X(0001).
           05  FILLER REDEFINES BIDT12F.
               10  BIDT12A PIC  X(0001).
           05  BIDT12I PIC  X(0008).
      *    -------------------------------
           05  BTYPE12L PIC S9(0004) COMP.
           05  BTYPE12F PIC  X(0001).
           05  FILLER REDEFINES BTYPE12F.
               10  BTYPE12A PIC  X(0001).
           05  BTYPE12I PIC  X(0001).
      *    -------------------------------
           05  BCARR12L PIC S9(0004) COMP.
           05  BCARR12F PIC  X(0001).
           05  FILLER REDEFINES BCARR12F.
               10  BCARR12A PIC  X(0001).
           05  BCARR12I PIC  X(0001).
      *    -------------------------------
           05  BCLAM12L PIC S9(0004) COMP.
           05  BCLAM12F PIC  X(0001).
           05  FILLER REDEFINES BCLAM12F.
               10  BCLAM12A PIC  X(0001).
           05  BCLAM12I PIC  X(0007).
      *    -------------------------------
           05  BCERT12L PIC S9(0004) COMP.
           05  BCERT12F PIC  X(0001).
           05  FILLER REDEFINES BCERT12F.
               10  BCERT12A PIC  X(0001).
           05  BCERT12I PIC  X(0016).
      *    -------------------------------
           05  BACCT12L PIC S9(0004) COMP.
           05  BACCT12F PIC  X(0001).
           05  FILLER REDEFINES BACCT12F.
               10  BACCT12A PIC  X(0001).
           05  BACCT12I PIC  X(0010).
      *    -------------------------------
           05  BNUM13L PIC S9(0004) COMP.
           05  BNUM13F PIC  X(0001).
           05  FILLER REDEFINES BNUM13F.
               10  BNUM13A PIC  X(0001).
           05  BNUM13I PIC  X(0002).
      *    -------------------------------
           05  BNAME13L PIC S9(0004) COMP.
           05  BNAME13F PIC  X(0001).
           05  FILLER REDEFINES BNAME13F.
               10  BNAME13A PIC  X(0001).
           05  BNAME13I PIC  X(0022).
      *    -------------------------------
           05  BAGE13L PIC S9(0004) COMP.
           05  BAGE13F PIC  X(0001).
           05  FILLER REDEFINES BAGE13F.
               10  BAGE13A PIC  X(0001).
           05  BAGE13I PIC  X(0002).
      *    -------------------------------
           05  BSTA13L PIC S9(0004) COMP.
           05  BSTA13F PIC  X(0001).
           05  FILLER REDEFINES BSTA13F.
               10  BSTA13A PIC  X(0001).
           05  BSTA13I PIC  X(0001).
      *    -------------------------------
           05  BIDT13L PIC S9(0004) COMP.
           05  BIDT13F PIC  X(0001).
           05  FILLER REDEFINES BIDT13F.
               10  BIDT13A PIC  X(0001).
           05  BIDT13I PIC  X(0008).
      *    -------------------------------
           05  BTYPE13L PIC S9(0004) COMP.
           05  BTYPE13F PIC  X(0001).
           05  FILLER REDEFINES BTYPE13F.
               10  BTYPE13A PIC  X(0001).
           05  BTYPE13I PIC  X(0001).
      *    -------------------------------
           05  BCARR13L PIC S9(0004) COMP.
           05  BCARR13F PIC  X(0001).
           05  FILLER REDEFINES BCARR13F.
               10  BCARR13A PIC  X(0001).
           05  BCARR13I PIC  X(0001).
      *    -------------------------------
           05  BCLAM13L PIC S9(0004) COMP.
           05  BCLAM13F PIC  X(0001).
           05  FILLER REDEFINES BCLAM13F.
               10  BCLAM13A PIC  X(0001).
           05  BCLAM13I PIC  X(0007).
      *    -------------------------------
           05  BCERT13L PIC S9(0004) COMP.
           05  BCERT13F PIC  X(0001).
           05  FILLER REDEFINES BCERT13F.
               10  BCERT13A PIC  X(0001).
           05  BCERT13I PIC  X(0016).
      *    -------------------------------
           05  BACCT13L PIC S9(0004) COMP.
           05  BACCT13F PIC  X(0001).
           05  FILLER REDEFINES BACCT13F.
               10  BACCT13A PIC  X(0001).
           05  BACCT13I PIC  X(0010).
      *    -------------------------------
           05  BNUM14L PIC S9(0004) COMP.
           05  BNUM14F PIC  X(0001).
           05  FILLER REDEFINES BNUM14F.
               10  BNUM14A PIC  X(0001).
           05  BNUM14I PIC  X(0002).
      *    -------------------------------
           05  BNAME14L PIC S9(0004) COMP.
           05  BNAME14F PIC  X(0001).
           05  FILLER REDEFINES BNAME14F.
               10  BNAME14A PIC  X(0001).
           05  BNAME14I PIC  X(0022).
      *    -------------------------------
           05  BAGE14L PIC S9(0004) COMP.
           05  BAGE14F PIC  X(0001).
           05  FILLER REDEFINES BAGE14F.
               10  BAGE14A PIC  X(0001).
           05  BAGE14I PIC  X(0002).
      *    -------------------------------
           05  BSTA14L PIC S9(0004) COMP.
           05  BSTA14F PIC  X(0001).
           05  FILLER REDEFINES BSTA14F.
               10  BSTA14A PIC  X(0001).
           05  BSTA14I PIC  X(0001).
      *    -------------------------------
           05  BIDT14L PIC S9(0004) COMP.
           05  BIDT14F PIC  X(0001).
           05  FILLER REDEFINES BIDT14F.
               10  BIDT14A PIC  X(0001).
           05  BIDT14I PIC  X(0008).
      *    -------------------------------
           05  BTYPE14L PIC S9(0004) COMP.
           05  BTYPE14F PIC  X(0001).
           05  FILLER REDEFINES BTYPE14F.
               10  BTYPE14A PIC  X(0001).
           05  BTYPE14I PIC  X(0001).
      *    -------------------------------
           05  BCARR14L PIC S9(0004) COMP.
           05  BCARR14F PIC  X(0001).
           05  FILLER REDEFINES BCARR14F.
               10  BCARR14A PIC  X(0001).
           05  BCARR14I PIC  X(0001).
      *    -------------------------------
           05  BCLAM14L PIC S9(0004) COMP.
           05  BCLAM14F PIC  X(0001).
           05  FILLER REDEFINES BCLAM14F.
               10  BCLAM14A PIC  X(0001).
           05  BCLAM14I PIC  X(0007).
      *    -------------------------------
           05  BCERT14L PIC S9(0004) COMP.
           05  BCERT14F PIC  X(0001).
           05  FILLER REDEFINES BCERT14F.
               10  BCERT14A PIC  X(0001).
           05  BCERT14I PIC  X(0016).
      *    -------------------------------
           05  BACCT14L PIC S9(0004) COMP.
           05  BACCT14F PIC  X(0001).
           05  FILLER REDEFINES BACCT14F.
               10  BACCT14A PIC  X(0001).
           05  BACCT14I PIC  X(0010).
      *    -------------------------------
           05  BNUM15L PIC S9(0004) COMP.
           05  BNUM15F PIC  X(0001).
           05  FILLER REDEFINES BNUM15F.
               10  BNUM15A PIC  X(0001).
           05  BNUM15I PIC  X(0002).
      *    -------------------------------
           05  BNAME15L PIC S9(0004) COMP.
           05  BNAME15F PIC  X(0001).
           05  FILLER REDEFINES BNAME15F.
               10  BNAME15A PIC  X(0001).
           05  BNAME15I PIC  X(0022).
      *    -------------------------------
           05  BAGE15L PIC S9(0004) COMP.
           05  BAGE15F PIC  X(0001).
           05  FILLER REDEFINES BAGE15F.
               10  BAGE15A PIC  X(0001).
           05  BAGE15I PIC  X(0002).
      *    -------------------------------
           05  BSTA15L PIC S9(0004) COMP.
           05  BSTA15F PIC  X(0001).
           05  FILLER REDEFINES BSTA15F.
               10  BSTA15A PIC  X(0001).
           05  BSTA15I PIC  X(0001).
      *    -------------------------------
           05  BIDT15L PIC S9(0004) COMP.
           05  BIDT15F PIC  X(0001).
           05  FILLER REDEFINES BIDT15F.
               10  BIDT15A PIC  X(0001).
           05  BIDT15I PIC  X(0008).
      *    -------------------------------
           05  BTYPE15L PIC S9(0004) COMP.
           05  BTYPE15F PIC  X(0001).
           05  FILLER REDEFINES BTYPE15F.
               10  BTYPE15A PIC  X(0001).
           05  BTYPE15I PIC  X(0001).
      *    -------------------------------
           05  BCARR15L PIC S9(0004) COMP.
           05  BCARR15F PIC  X(0001).
           05  FILLER REDEFINES BCARR15F.
               10  BCARR15A PIC  X(0001).
           05  BCARR15I PIC  X(0001).
      *    -------------------------------
           05  BCLAM15L PIC S9(0004) COMP.
           05  BCLAM15F PIC  X(0001).
           05  FILLER REDEFINES BCLAM15F.
               10  BCLAM15A PIC  X(0001).
           05  BCLAM15I PIC  X(0007).
      *    -------------------------------
           05  BCERT15L PIC S9(0004) COMP.
           05  BCERT15F PIC  X(0001).
           05  FILLER REDEFINES BCERT15F.
               10  BCERT15A PIC  X(0001).
           05  BCERT15I PIC  X(0016).
      *    -------------------------------
           05  BACCT15L PIC S9(0004) COMP.
           05  BACCT15F PIC  X(0001).
           05  FILLER REDEFINES BACCT15F.
               10  BACCT15A PIC  X(0001).
           05  BACCT15I PIC  X(0010).
      *    -------------------------------
           05  BNUM16L PIC S9(0004) COMP.
           05  BNUM16F PIC  X(0001).
           05  FILLER REDEFINES BNUM16F.
               10  BNUM16A PIC  X(0001).
           05  BNUM16I PIC  X(0002).
      *    -------------------------------
           05  BNAME16L PIC S9(0004) COMP.
           05  BNAME16F PIC  X(0001).
           05  FILLER REDEFINES BNAME16F.
               10  BNAME16A PIC  X(0001).
           05  BNAME16I PIC  X(0022).
      *    -------------------------------
           05  BAGE16L PIC S9(0004) COMP.
           05  BAGE16F PIC  X(0001).
           05  FILLER REDEFINES BAGE16F.
               10  BAGE16A PIC  X(0001).
           05  BAGE16I PIC  X(0002).
      *    -------------------------------
           05  BSTA16L PIC S9(0004) COMP.
           05  BSTA16F PIC  X(0001).
           05  FILLER REDEFINES BSTA16F.
               10  BSTA16A PIC  X(0001).
           05  BSTA16I PIC  X(0001).
      *    -------------------------------
           05  BIDT16L PIC S9(0004) COMP.
           05  BIDT16F PIC  X(0001).
           05  FILLER REDEFINES BIDT16F.
               10  BIDT16A PIC  X(0001).
           05  BIDT16I PIC  X(0008).
      *    -------------------------------
           05  BTYPE16L PIC S9(0004) COMP.
           05  BTYPE16F PIC  X(0001).
           05  FILLER REDEFINES BTYPE16F.
               10  BTYPE16A PIC  X(0001).
           05  BTYPE16I PIC  X(0001).
      *    -------------------------------
           05  BCARR16L PIC S9(0004) COMP.
           05  BCARR16F PIC  X(0001).
           05  FILLER REDEFINES BCARR16F.
               10  BCARR16A PIC  X(0001).
           05  BCARR16I PIC  X(0001).
      *    -------------------------------
           05  BCLAM16L PIC S9(0004) COMP.
           05  BCLAM16F PIC  X(0001).
           05  FILLER REDEFINES BCLAM16F.
               10  BCLAM16A PIC  X(0001).
           05  BCLAM16I PIC  X(0007).
      *    -------------------------------
           05  BCERT16L PIC S9(0004) COMP.
           05  BCERT16F PIC  X(0001).
           05  FILLER REDEFINES BCERT16F.
               10  BCERT16A PIC  X(0001).
           05  BCERT16I PIC  X(0016).
      *    -------------------------------
           05  BACCT16L PIC S9(0004) COMP.
           05  BACCT16F PIC  X(0001).
           05  FILLER REDEFINES BACCT16F.
               10  BACCT16A PIC  X(0001).
           05  BACCT16I PIC  X(0010).
      *    -------------------------------
           05  BSELL PIC S9(0004) COMP.
           05  BSELF PIC  X(0001).
           05  FILLER REDEFINES BSELF.
               10  BSELA PIC  X(0001).
           05  BSELI PIC  9(2).
      *    -------------------------------
           05  BEMSG1L PIC S9(0004) COMP.
           05  BEMSG1F PIC  X(0001).
           05  FILLER REDEFINES BEMSG1F.
               10  BEMSG1A PIC  X(0001).
           05  BEMSG1I PIC  X(0079).
      *    -------------------------------
           05  BPFKL PIC S9(0004) COMP.
           05  BPFKF PIC  X(0001).
           05  FILLER REDEFINES BPFKF.
               10  BPFKA PIC  X(0001).
           05  BPFKI PIC  99.
      *    -------------------------------
           05  BPFK5L PIC S9(0004) COMP.
           05  BPFK5F PIC  X(0001).
           05  FILLER REDEFINES BPFK5F.
               10  BPFK5A PIC  X(0001).
           05  BPFK5I PIC  X(0013).
      *    -------------------------------
           05  BPFK7L PIC S9(0004) COMP.
           05  BPFK7F PIC  X(0001).
           05  FILLER REDEFINES BPFK7F.
               10  BPFK7A PIC  X(0001).
           05  BPFK7I PIC  X(0011).
      *    -------------------------------
           05  BPF2L PIC S9(0004) COMP.
           05  BPF2F PIC  X(0001).
           05  FILLER REDEFINES BPF2F.
               10  BPF2A PIC  X(0001).
           05  BPF2I PIC  X(0030).
      *    -------------------------------
           05  BPFK6L PIC S9(0004) COMP.
           05  BPFK6F PIC  X(0001).
           05  FILLER REDEFINES BPFK6F.
               10  BPFK6A PIC  X(0001).
           05  BPFK6I PIC  X(0013).
       01  EL132BO REDEFINES EL132BI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BHEAD1O PIC  X(0038).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCOMPO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BUSERIDO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCERTO PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNUM01O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNAME01O PIC  X(0022).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAGE01O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTA01O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BIDT01O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTYPE01O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCARR01O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCLAM01O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCERT01O PIC  X(0016).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BACCT01O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNUM02O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNAME02O PIC  X(0022).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAGE02O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTA02O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BIDT02O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTYPE02O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCARR02O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCLAM02O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCERT02O PIC  X(0016).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BACCT02O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNUM03O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNAME03O PIC  X(0022).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAGE03O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTA03O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BIDT03O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTYPE03O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCARR03O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCLAM03O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCERT03O PIC  X(0016).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BACCT03O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNUM04O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNAME04O PIC  X(0022).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAGE04O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTA04O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BIDT04O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTYPE04O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCARR04O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCLAM04O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCERT04O PIC  X(0016).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BACCT04O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNUM05O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNAME05O PIC  X(0022).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAGE05O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTA05O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BIDT05O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTYPE05O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCARR05O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCLAM05O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCERT05O PIC  X(0016).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BACCT05O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNUM06O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNAME06O PIC  X(0022).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAGE06O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTA06O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BIDT06O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTYPE06O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCARR06O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCLAM06O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCERT06O PIC  X(0016).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BACCT06O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNUM07O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNAME07O PIC  X(0022).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAGE07O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTA07O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BIDT07O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTYPE07O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCARR07O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCLAM07O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCERT07O PIC  X(0016).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BACCT07O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNUM08O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNAME08O PIC  X(0022).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAGE08O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTA08O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BIDT08O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTYPE08O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCARR08O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCLAM08O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCERT08O PIC  X(0016).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BACCT08O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNUM09O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNAME09O PIC  X(0022).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAGE09O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTA09O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BIDT09O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTYPE09O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCARR09O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCLAM09O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCERT09O PIC  X(0016).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BACCT09O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNUM10O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNAME10O PIC  X(0022).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAGE10O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTA10O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BIDT10O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTYPE10O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCARR10O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCLAM10O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCERT10O PIC  X(0016).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BACCT10O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNUM11O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNAME11O PIC  X(0022).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAGE11O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTA11O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BIDT11O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTYPE11O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCARR11O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCLAM11O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCERT11O PIC  X(0016).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BACCT11O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNUM12O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNAME12O PIC  X(0022).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAGE12O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTA12O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BIDT12O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTYPE12O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCARR12O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCLAM12O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCERT12O PIC  X(0016).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BACCT12O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNUM13O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNAME13O PIC  X(0022).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAGE13O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTA13O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BIDT13O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTYPE13O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCARR13O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCLAM13O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCERT13O PIC  X(0016).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BACCT13O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNUM14O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNAME14O PIC  X(0022).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAGE14O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTA14O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BIDT14O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTYPE14O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCARR14O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCLAM14O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCERT14O PIC  X(0016).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BACCT14O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNUM15O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNAME15O PIC  X(0022).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAGE15O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTA15O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BIDT15O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTYPE15O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCARR15O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCLAM15O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCERT15O PIC  X(0016).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BACCT15O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNUM16O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNAME16O PIC  X(0022).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAGE16O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTA16O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BIDT16O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTYPE16O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCARR16O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCLAM16O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCERT16O PIC  X(0016).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BACCT16O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSELO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BEMSG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPFKO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPFK5O PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPFK7O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPF2O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPFK6O PIC  X(0013).
      *    -------------------------------
       01  EL132AI REDEFINES EL132BI.
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
           05  AHEAD1L PIC S9(0004) COMP.
           05  AHEAD1F PIC  X(0001).
           05  FILLER REDEFINES AHEAD1F.
               10  AHEAD1A PIC  X(0001).
           05  AHEAD1I PIC  X(0038).
      *    -------------------------------
           05  ACOMPL PIC S9(0004) COMP.
           05  ACOMPF PIC  X(0001).
           05  FILLER REDEFINES ACOMPF.
               10  ACOMPA PIC  X(0001).
           05  ACOMPI PIC  X(0003).
      *    -------------------------------
           05  AUSERIDL PIC S9(0004) COMP.
           05  AUSERIDF PIC  X(0001).
           05  FILLER REDEFINES AUSERIDF.
               10  AUSERIDA PIC  X(0001).
           05  AUSERIDI PIC  X(0004).
      *    -------------------------------
           05  ACLAIML PIC S9(0004) COMP.
           05  ACLAIMF PIC  X(0001).
           05  FILLER REDEFINES ACLAIMF.
               10  ACLAIMA PIC  X(0001).
           05  ACLAIMI PIC  X(0007).
      *    -------------------------------
           05  ACARIERL PIC S9(0004) COMP.
           05  ACARIERF PIC  X(0001).
           05  FILLER REDEFINES ACARIERF.
               10  ACARIERA PIC  X(0001).
           05  ACARIERI PIC  X(0001).
      *    -------------------------------
           05  ACERTNOL PIC S9(0004) COMP.
           05  ACERTNOF PIC  X(0001).
           05  FILLER REDEFINES ACERTNOF.
               10  ACERTNOA PIC  X(0001).
           05  ACERTNOI PIC  X(0010).
      *    -------------------------------
           05  ACERTSXL PIC S9(0004) COMP.
           05  ACERTSXF PIC  X(0001).
           05  FILLER REDEFINES ACERTSXF.
               10  ACERTSXA PIC  X(0001).
           05  ACERTSXI PIC  X(0001).
      *    -------------------------------
           05  ALNAMEL PIC S9(0004) COMP.
           05  ALNAMEF PIC  X(0001).
           05  FILLER REDEFINES ALNAMEF.
               10  ALNAMEA PIC  X(0001).
           05  ALNAMEI PIC  X(0015).
      *    -------------------------------
           05  AFNAMEL PIC S9(0004) COMP.
           05  AFNAMEF PIC  X(0001).
           05  FILLER REDEFINES AFNAMEF.
               10  AFNAMEA PIC  X(0001).
           05  AFNAMEI PIC  X(0012).
      *    -------------------------------
           05  AMINITL PIC S9(0004) COMP.
           05  AMINITF PIC  X(0001).
           05  FILLER REDEFINES AMINITF.
               10  AMINITA PIC  X(0001).
           05  AMINITI PIC  X(0001).
      *    -------------------------------
           05  AACCTL PIC S9(0004) COMP.
           05  AACCTF PIC  X(0001).
           05  FILLER REDEFINES AACCTF.
               10  AACCTA PIC  X(0001).
           05  AACCTI PIC  X(0010).
      *    -------------------------------
           05  AOPT3L PIC S9(0004) COMP.
           05  AOPT3F PIC  X(0001).
           05  FILLER REDEFINES AOPT3F.
               10  AOPT3A PIC  X(0001).
           05  AOPT3I PIC  X(0014).
      *    -------------------------------
           05  ASSOPTL PIC S9(0004) COMP.
           05  ASSOPTF PIC  X(0001).
           05  FILLER REDEFINES ASSOPTF.
               10  ASSOPTA PIC  X(0001).
           05  ASSOPTI PIC  X(0023).
      *    -------------------------------
           05  ASSNL PIC S9(0004) COMP.
           05  ASSNF PIC  X(0001).
           05  FILLER REDEFINES ASSNF.
               10  ASSNA PIC  X(0001).
           05  ASSNI PIC  X(0011).
      *    -------------------------------
           05  ACRTNOL PIC S9(0004) COMP.
           05  ACRTNOF PIC  X(0001).
           05  FILLER REDEFINES ACRTNOF.
               10  ACRTNOA PIC  X(0001).
           05  ACRTNOI PIC  X(0010).
      *    -------------------------------
           05  ACRTSXL PIC S9(0004) COMP.
           05  ACRTSXF PIC  X(0001).
           05  FILLER REDEFINES ACRTSXF.
               10  ACRTSXA PIC  X(0001).
           05  ACRTSXI PIC  X(0001).
      *    -------------------------------
           05  ACCNH1L PIC S9(0004) COMP.
           05  ACCNH1F PIC  X(0001).
           05  FILLER REDEFINES ACCNH1F.
               10  ACCNH1A PIC  X(0001).
           05  ACCNH1I PIC  X(0014).
      *    -------------------------------
           05  ACCNH2L PIC S9(0004) COMP.
           05  ACCNH2F PIC  X(0001).
           05  FILLER REDEFINES ACCNH2F.
               10  ACCNH2A PIC  X(0001).
           05  ACCNH2I PIC  X(0019).
      *    -------------------------------
           05  ACCNNOL PIC S9(0004) COMP.
           05  ACCNNOF PIC  X(0001).
           05  FILLER REDEFINES ACCNNOF.
               10  ACCNNOA PIC  X(0001).
           05  ACCNNOI PIC  X(0016).
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
           05  APFKL PIC S9(0004) COMP.
           05  APFKF PIC  X(0001).
           05  FILLER REDEFINES APFKF.
               10  APFKA PIC  X(0001).
           05  APFKI PIC  99.
      *    -------------------------------
           05  APFK5L PIC S9(0004) COMP.
           05  APFK5F PIC  X(0001).
           05  FILLER REDEFINES APFK5F.
               10  APFK5A PIC  X(0001).
           05  APFK5I PIC  X(0019).
      *    -------------------------------
           05  APFK6L PIC S9(0004) COMP.
           05  APFK6F PIC  X(0001).
           05  FILLER REDEFINES APFK6F.
               10  APFK6A PIC  X(0001).
           05  APFK6I PIC  X(0021).
       01  EL132AO REDEFINES EL132BI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHEAD1O PIC  X(0038).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACOMPO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AUSERIDO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLAIMO PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARIERO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACERTNOO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACERTSXO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALNAMEO PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AFNAMEO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMINITO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AACCTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AOPT3O PIC  X(0014).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ASSOPTO PIC  X(0023).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ASSNO PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACRTNOO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACRTSXO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCNH1O PIC  X(0014).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCNH2O PIC  X(0019).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCNNOO PIC  X(0016).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEMSG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEMSG2O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APFKO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APFK5O PIC  X(0019).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APFK6O PIC  X(0021).
      *    -------------------------------
       01  EL132CI REDEFINES EL132BI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  CDATEL PIC S9(0004) COMP.
           05  CDATEF PIC  X(0001).
           05  FILLER REDEFINES CDATEF.
               10  CDATEA PIC  X(0001).
           05  CDATEI PIC  X(0008).
      *    -------------------------------
           05  CTIMEL PIC S9(0004) COMP.
           05  CTIMEF PIC  X(0001).
           05  FILLER REDEFINES CTIMEF.
               10  CTIMEA PIC  X(0001).
           05  CTIMEI PIC  X(0005).
      *    -------------------------------
           05  SEQUL PIC S9(0004) COMP.
           05  SEQUF PIC  X(0001).
           05  FILLER REDEFINES SEQUF.
               10  SEQUA PIC  X(0001).
           05  SEQUI PIC  X(0010).
      *    -------------------------------
           05  CCOMPL PIC S9(0004) COMP.
           05  CCOMPF PIC  X(0001).
           05  FILLER REDEFINES CCOMPF.
               10  CCOMPA PIC  X(0001).
           05  CCOMPI PIC  X(0003).
      *    -------------------------------
           05  CUSERIDL PIC S9(0004) COMP.
           05  CUSERIDF PIC  X(0001).
           05  FILLER REDEFINES CUSERIDF.
               10  CUSERIDA PIC  X(0001).
           05  CUSERIDI PIC  X(0004).
      *    -------------------------------
           05  CCLAIML PIC S9(0004) COMP.
           05  CCLAIMF PIC  X(0001).
           05  FILLER REDEFINES CCLAIMF.
               10  CCLAIMA PIC  X(0001).
           05  CCLAIMI PIC  X(0007).
      *    -------------------------------
           05  CCARIERL PIC S9(0004) COMP.
           05  CCARIERF PIC  X(0001).
           05  FILLER REDEFINES CCARIERF.
               10  CCARIERA PIC  X(0001).
           05  CCARIERI PIC  X(0001).
      *    -------------------------------
           05  CCERTNOL PIC S9(0004) COMP.
           05  CCERTNOF PIC  X(0001).
           05  FILLER REDEFINES CCERTNOF.
               10  CCERTNOA PIC  X(0001).
           05  CCERTNOI PIC  X(0010).
      *    -------------------------------
           05  CCERTSXL PIC S9(0004) COMP.
           05  CCERTSXF PIC  X(0001).
           05  FILLER REDEFINES CCERTSXF.
               10  CCERTSXA PIC  X(0001).
           05  CCERTSXI PIC  X(0001).
      *    -------------------------------
           05  CTYPEL PIC S9(0004) COMP.
           05  CTYPEF PIC  X(0001).
           05  FILLER REDEFINES CTYPEF.
               10  CTYPEA PIC  X(0001).
           05  CTYPEI PIC  X(0001).
      *    -------------------------------
           05  PCERTNOL PIC S9(0004) COMP.
           05  PCERTNOF PIC  X(0001).
           05  FILLER REDEFINES PCERTNOF.
               10  PCERTNOA PIC  X(0001).
           05  PCERTNOI PIC  X(0010).
      *    -------------------------------
           05  PSUFXL PIC S9(0004) COMP.
           05  PSUFXF PIC  X(0001).
           05  FILLER REDEFINES PSUFXF.
               10  PSUFXA PIC  X(0001).
           05  PSUFXI PIC  X(0001).
      *    -------------------------------
           05  CSTATL PIC S9(0004) COMP.
           05  CSTATF PIC  X(0001).
           05  FILLER REDEFINES CSTATF.
               10  CSTATA PIC  X(0001).
           05  CSTATI PIC  X(0001).
      *    -------------------------------
           05  CINCREDL PIC S9(0004) COMP.
           05  CINCREDF PIC  X(0001).
           05  FILLER REDEFINES CINCREDF.
               10  CINCREDA PIC  X(0001).
           05  CINCREDI PIC  X(0008).
      *    -------------------------------
           05  CREPORTL PIC S9(0004) COMP.
           05  CREPORTF PIC  X(0001).
           05  FILLER REDEFINES CREPORTF.
               10  CREPORTA PIC  X(0001).
           05  CREPORTI PIC  X(0008).
      *    -------------------------------
           05  CCAUSCDL PIC S9(0004) COMP.
           05  CCAUSCDF PIC  X(0001).
           05  FILLER REDEFINES CCAUSCDF.
               10  CCAUSCDA PIC  X(0001).
           05  CCAUSCDI PIC  X(0006).
      *    -------------------------------
           05  CESTENDL PIC S9(0004) COMP.
           05  CESTENDF PIC  X(0001).
           05  FILLER REDEFINES CESTENDF.
               10  CESTENDA PIC  X(0001).
           05  CESTENDI PIC  X(0008).
      *    -------------------------------
           05  CCAUSEL PIC S9(0004) COMP.
           05  CCAUSEF PIC  X(0001).
           05  FILLER REDEFINES CCAUSEF.
               10  CCAUSEA PIC  X(0001).
           05  CCAUSEI PIC  X(0060).
      *    -------------------------------
           05  CBENEL PIC S9(0004) COMP.
           05  CBENEF PIC  X(0001).
           05  FILLER REDEFINES CBENEF.
               10  CBENEA PIC  X(0001).
           05  CBENEI PIC  X(0010).
      *    -------------------------------
           05  CBDATEL PIC S9(0004) COMP.
           05  CBDATEF PIC  X(0001).
           05  FILLER REDEFINES CBDATEF.
               10  CBDATEA PIC  X(0001).
           05  CBDATEI PIC  X(0008).
      *    -------------------------------
           05  CSSNL PIC S9(0004) COMP.
           05  CSSNF PIC  X(0001).
           05  FILLER REDEFINES CSSNF.
               10  CSSNA PIC  X(0001).
           05  CSSNI PIC  X(0011).
      *    -------------------------------
           05  CSEXL PIC S9(0004) COMP.
           05  CSEXF PIC  X(0001).
           05  FILLER REDEFINES CSEXF.
               10  CSEXA PIC  X(0001).
           05  CSEXI PIC  X(0001).
      *    -------------------------------
           05  CLNAMEL PIC S9(0004) COMP.
           05  CLNAMEF PIC  X(0001).
           05  FILLER REDEFINES CLNAMEF.
               10  CLNAMEA PIC  X(0001).
           05  CLNAMEI PIC  X(0015).
      *    -------------------------------
           05  CFNAMEL PIC S9(0004) COMP.
           05  CFNAMEF PIC  X(0001).
           05  FILLER REDEFINES CFNAMEF.
               10  CFNAMEA PIC  X(0001).
           05  CFNAMEI PIC  X(0012).
      *    -------------------------------
           05  CMNAMEL PIC S9(0004) COMP.
           05  CMNAMEF PIC  X(0001).
           05  FILLER REDEFINES CMNAMEF.
               10  CMNAMEA PIC  X(0001).
           05  CMNAMEI PIC  X(0001).
      *    -------------------------------
           05  LOANNOL PIC S9(0004) COMP.
           05  LOANNOF PIC  X(0001).
           05  FILLER REDEFINES LOANNOF.
               10  LOANNOA PIC  X(0001).
           05  LOANNOI PIC  X(0008).
      *    -------------------------------
           05  CLNMEL PIC S9(0004) COMP.
           05  CLNMEF PIC  X(0001).
           05  FILLER REDEFINES CLNMEF.
               10  CLNMEA PIC  X(0001).
           05  CLNMEI PIC  X(0015).
      *    -------------------------------
           05  CFNMEL PIC S9(0004) COMP.
           05  CFNMEF PIC  X(0001).
           05  FILLER REDEFINES CFNMEF.
               10  CFNMEA PIC  X(0001).
           05  CFNMEI PIC  X(0012).
      *    -------------------------------
           05  CINITL PIC S9(0004) COMP.
           05  CINITF PIC  X(0001).
           05  FILLER REDEFINES CINITF.
               10  CINITA PIC  X(0001).
           05  CINITI PIC  X(0001).
      *    -------------------------------
           05  LOANBALL PIC S9(0004) COMP.
           05  LOANBALF PIC  X(0001).
           05  FILLER REDEFINES LOANBALF.
               10  LOANBALA PIC  X(0001).
           05  LOANBALI PIC  9(7)V99.
      *    -------------------------------
           05  CPROCL PIC S9(0004) COMP.
           05  CPROCF PIC  X(0001).
           05  FILLER REDEFINES CPROCF.
               10  CPROCA PIC  X(0001).
           05  CPROCI PIC  X(0004).
      *    -------------------------------
           05  CSUPRL PIC S9(0004) COMP.
           05  CSUPRF PIC  X(0001).
           05  FILLER REDEFINES CSUPRF.
               10  CSUPRA PIC  X(0001).
           05  CSUPRI PIC  X(0001).
      *    -------------------------------
           05  CPRICDL PIC S9(0004) COMP.
           05  CPRICDF PIC  X(0001).
           05  FILLER REDEFINES CPRICDF.
               10  CPRICDA PIC  X(0001).
           05  CPRICDI PIC  X(0001).
      *    -------------------------------
           05  FILETOL PIC S9(0004) COMP.
           05  FILETOF PIC  X(0001).
           05  FILLER REDEFINES FILETOF.
               10  FILETOA PIC  X(0001).
           05  FILETOI PIC  X(0004).
      *    -------------------------------
           05  CPTHHDGL PIC S9(0004) COMP.
           05  CPTHHDGF PIC  X(0001).
           05  FILLER REDEFINES CPTHHDGF.
               10  CPTHHDGA PIC  X(0001).
           05  CPTHHDGI PIC  X(0010).
      *    -------------------------------
           05  CPDTHRUL PIC S9(0004) COMP.
           05  CPDTHRUF PIC  X(0001).
           05  FILLER REDEFINES CPDTHRUF.
               10  CPDTHRUA PIC  X(0001).
           05  CPDTHRUI PIC  X(0008).
      *    -------------------------------
           05  CTOTPDL PIC S9(0004) COMP.
           05  CTOTPDF PIC  X(0001).
           05  FILLER REDEFINES CTOTPDF.
               10  CTOTPDA PIC  X(0001).
           05  CTOTPDI PIC  9(7)V99.
      *    -------------------------------
           05  CNODAYSL PIC S9(0004) COMP.
           05  CNODAYSF PIC  X(0001).
           05  FILLER REDEFINES CNODAYSF.
               10  CNODAYSA PIC  X(0001).
           05  CNODAYSI PIC  9(5).
      *    -------------------------------
           05  CNOPMTSL PIC S9(0004) COMP.
           05  CNOPMTSF PIC  X(0001).
           05  FILLER REDEFINES CNOPMTSF.
               10  CNOPMTSA PIC  X(0001).
           05  CNOPMTSI PIC  9(4).
      *    -------------------------------
           05  CESTABL PIC S9(0004) COMP.
           05  CESTABF PIC  X(0001).
           05  FILLER REDEFINES CESTABF.
               10  CESTABA PIC  X(0001).
           05  CESTABI PIC  X(0008).
      *    -------------------------------
           05  COCCL PIC S9(0004) COMP.
           05  COCCF PIC  X(0001).
           05  FILLER REDEFINES COCCF.
               10  COCCA PIC  X(0001).
           05  COCCI PIC  X(0002).
      *    -------------------------------
           05  CMNTDTEL PIC S9(0004) COMP.
           05  CMNTDTEF PIC  X(0001).
           05  FILLER REDEFINES CMNTDTEF.
               10  CMNTDTEA PIC  X(0001).
           05  CMNTDTEI PIC  X(0008).
      *    -------------------------------
           05  CMNTYPEL PIC S9(0004) COMP.
           05  CMNTYPEF PIC  X(0001).
           05  FILLER REDEFINES CMNTYPEF.
               10  CMNTYPEA PIC  X(0001).
           05  CMNTYPEI PIC  X(0006).
      *    -------------------------------
           05  CCARRL PIC S9(0004) COMP.
           05  CCARRF PIC  X(0001).
           05  FILLER REDEFINES CCARRF.
               10  CCARRA PIC  X(0001).
           05  CCARRI PIC  X(0001).
      *    -------------------------------
           05  CGRPL PIC S9(0004) COMP.
           05  CGRPF PIC  X(0001).
           05  FILLER REDEFINES CGRPF.
               10  CGRPA PIC  X(0001).
           05  CGRPI PIC  X(0006).
      *    -------------------------------
           05  CSTATEL PIC S9(0004) COMP.
           05  CSTATEF PIC  X(0001).
           05  FILLER REDEFINES CSTATEF.
               10  CSTATEA PIC  X(0001).
           05  CSTATEI PIC  X(0002).
      *    -------------------------------
           05  CACCNTL PIC S9(0004) COMP.
           05  CACCNTF PIC  X(0001).
           05  FILLER REDEFINES CACCNTF.
               10  CACCNTA PIC  X(0001).
           05  CACCNTI PIC  X(0010).
      *    -------------------------------
           05  CEFFDTL PIC S9(0004) COMP.
           05  CEFFDTF PIC  X(0001).
           05  FILLER REDEFINES CEFFDTF.
               10  CEFFDTA PIC  X(0001).
           05  CEFFDTI PIC  X(0008).
      *    -------------------------------
           05  CISAGEL PIC S9(0004) COMP.
           05  CISAGEF PIC  X(0001).
           05  FILLER REDEFINES CISAGEF.
               10  CISAGEA PIC  X(0001).
           05  CISAGEI PIC  X(0002).
      *    -------------------------------
           05  CAPRL PIC S9(0004) COMP.
           05  CAPRF PIC  X(0001).
           05  FILLER REDEFINES CAPRF.
               10  CAPRA PIC  X(0001).
           05  CAPRI PIC  9(4)V9(4).
      *    -------------------------------
           05  CPFREQL PIC S9(0004) COMP.
           05  CPFREQF PIC  X(0001).
           05  FILLER REDEFINES CPFREQF.
               10  CPFREQA PIC  X(0001).
           05  CPFREQI PIC  99.
      *    -------------------------------
           05  CINDGRPL PIC S9(0004) COMP.
           05  CINDGRPF PIC  X(0001).
           05  FILLER REDEFINES CINDGRPF.
               10  CINDGRPA PIC  X(0001).
           05  CINDGRPI PIC  X(0001).
      *    -------------------------------
           05  CPREMTPL PIC S9(0004) COMP.
           05  CPREMTPF PIC  X(0001).
           05  FILLER REDEFINES CPREMTPF.
               10  CPREMTPA PIC  X(0001).
           05  CPREMTPI PIC  X(0001).
      *    -------------------------------
           05  CREINCDL PIC S9(0004) COMP.
           05  CREINCDF PIC  X(0001).
           05  FILLER REDEFINES CREINCDF.
               10  CREINCDA PIC  X(0001).
           05  CREINCDI PIC  X(0003).
      *    -------------------------------
           05  CJLNMEL PIC S9(0004) COMP.
           05  CJLNMEF PIC  X(0001).
           05  FILLER REDEFINES CJLNMEF.
               10  CJLNMEA PIC  X(0001).
           05  CJLNMEI PIC  X(0015).
      *    -------------------------------
           05  CJFNMEL PIC S9(0004) COMP.
           05  CJFNMEF PIC  X(0001).
           05  FILLER REDEFINES CJFNMEF.
               10  CJFNMEA PIC  X(0001).
           05  CJFNMEI PIC  X(0012).
      *    -------------------------------
           05  CJINITL PIC S9(0004) COMP.
           05  CJINITF PIC  X(0001).
           05  FILLER REDEFINES CJINITF.
               10  CJINITA PIC  X(0001).
           05  CJINITI PIC  X(0001).
      *    -------------------------------
           05  CJAGEL PIC S9(0004) COMP.
           05  CJAGEF PIC  X(0001).
           05  FILLER REDEFINES CJAGEF.
               10  CJAGEA PIC  X(0001).
           05  CJAGEI PIC  X(0002).
      *    -------------------------------
           05  LCVDSCRL PIC S9(0004) COMP.
           05  LCVDSCRF PIC  X(0001).
           05  FILLER REDEFINES LCVDSCRF.
               10  LCVDSCRA PIC  X(0001).
           05  LCVDSCRI PIC  X(0006).
      *    -------------------------------
           05  LCVKINDL PIC S9(0004) COMP.
           05  LCVKINDF PIC  X(0001).
           05  FILLER REDEFINES LCVKINDF.
               10  LCVKINDA PIC  X(0001).
           05  LCVKINDI PIC  X(0003).
      *    -------------------------------
           05  LCVCDL PIC S9(0004) COMP.
           05  LCVCDF PIC  X(0001).
           05  FILLER REDEFINES LCVCDF.
               10  LCVCDA PIC  X(0001).
           05  LCVCDI PIC  99.
      *    -------------------------------
           05  LCVOTRML PIC S9(0004) COMP.
           05  LCVOTRMF PIC  X(0001).
           05  FILLER REDEFINES LCVOTRMF.
               10  LCVOTRMA PIC  X(0001).
           05  LCVOTRMI PIC  999.
      *    -------------------------------
           05  LCVRTRML PIC S9(0004) COMP.
           05  LCVRTRMF PIC  X(0001).
           05  FILLER REDEFINES LCVRTRMF.
               10  LCVRTRMA PIC  X(0001).
           05  LCVRTRMI PIC  X(0003).
      *    -------------------------------
           05  LCVBENEL PIC S9(0004) COMP.
           05  LCVBENEF PIC  X(0001).
           05  FILLER REDEFINES LCVBENEF.
               10  LCVBENEA PIC  X(0001).
           05  LCVBENEI PIC  9(9)V99.
      *    -------------------------------
           05  LCVFORML PIC S9(0004) COMP.
           05  LCVFORMF PIC  X(0001).
           05  FILLER REDEFINES LCVFORMF.
               10  LCVFORMA PIC  X(0001).
           05  LCVFORMI PIC  X(0012).
      *    -------------------------------
           05  LCVCNDTL PIC S9(0004) COMP.
           05  LCVCNDTF PIC  X(0001).
           05  FILLER REDEFINES LCVCNDTF.
               10  LCVCNDTA PIC  X(0001).
           05  LCVCNDTI PIC  X(0008).
      *    -------------------------------
           05  LCVEXITL PIC S9(0004) COMP.
           05  LCVEXITF PIC  X(0001).
           05  FILLER REDEFINES LCVEXITF.
               10  LCVEXITA PIC  X(0001).
           05  LCVEXITI PIC  X(0008).
      *    -------------------------------
           05  LCVSTATL PIC S9(0004) COMP.
           05  LCVSTATF PIC  X(0001).
           05  FILLER REDEFINES LCVSTATF.
               10  LCVSTATA PIC  X(0001).
           05  LCVSTATI PIC  X(0006).
      *    -------------------------------
           05  ACVDSCRL PIC S9(0004) COMP.
           05  ACVDSCRF PIC  X(0001).
           05  FILLER REDEFINES ACVDSCRF.
               10  ACVDSCRA PIC  X(0001).
           05  ACVDSCRI PIC  X(0006).
      *    -------------------------------
           05  ACVKINDL PIC S9(0004) COMP.
           05  ACVKINDF PIC  X(0001).
           05  FILLER REDEFINES ACVKINDF.
               10  ACVKINDA PIC  X(0001).
           05  ACVKINDI PIC  X(0003).
      *    -------------------------------
           05  ACVCDL PIC S9(0004) COMP.
           05  ACVCDF PIC  X(0001).
           05  FILLER REDEFINES ACVCDF.
               10  ACVCDA PIC  X(0001).
           05  ACVCDI PIC  99.
      *    -------------------------------
           05  ACVOTRML PIC S9(0004) COMP.
           05  ACVOTRMF PIC  X(0001).
           05  FILLER REDEFINES ACVOTRMF.
               10  ACVOTRMA PIC  X(0001).
           05  ACVOTRMI PIC  999.
      *    -------------------------------
           05  ACVRTRML PIC S9(0004) COMP.
           05  ACVRTRMF PIC  X(0001).
           05  FILLER REDEFINES ACVRTRMF.
               10  ACVRTRMA PIC  X(0001).
           05  ACVRTRMI PIC  X(0003).
      *    -------------------------------
           05  ACVBENEL PIC S9(0004) COMP.
           05  ACVBENEF PIC  X(0001).
           05  FILLER REDEFINES ACVBENEF.
               10  ACVBENEA PIC  X(0001).
           05  ACVBENEI PIC  9(9)V99.
      *    -------------------------------
           05  ACVFORML PIC S9(0004) COMP.
           05  ACVFORMF PIC  X(0001).
           05  FILLER REDEFINES ACVFORMF.
               10  ACVFORMA PIC  X(0001).
           05  ACVFORMI PIC  X(0012).
      *    -------------------------------
           05  ACVCNDTL PIC S9(0004) COMP.
           05  ACVCNDTF PIC  X(0001).
           05  FILLER REDEFINES ACVCNDTF.
               10  ACVCNDTA PIC  X(0001).
           05  ACVCNDTI PIC  X(0008).
      *    -------------------------------
           05  ACVEXITL PIC S9(0004) COMP.
           05  ACVEXITF PIC  X(0001).
           05  FILLER REDEFINES ACVEXITF.
               10  ACVEXITA PIC  X(0001).
           05  ACVEXITI PIC  X(0008).
      *    -------------------------------
           05  ACVSTATL PIC S9(0004) COMP.
           05  ACVSTATF PIC  X(0001).
           05  FILLER REDEFINES ACVSTATF.
               10  ACVSTATA PIC  X(0001).
           05  ACVSTATI PIC  X(0006).
      *    -------------------------------
           05  CEMSG1L PIC S9(0004) COMP.
           05  CEMSG1F PIC  X(0001).
           05  FILLER REDEFINES CEMSG1F.
               10  CEMSG1A PIC  X(0001).
           05  CEMSG1I PIC  X(0072).
      *    -------------------------------
           05  CPFKEYL PIC S9(0004) COMP.
           05  CPFKEYF PIC  X(0001).
           05  FILLER REDEFINES CPFKEYF.
               10  CPFKEYA PIC  X(0001).
           05  CPFKEYI PIC  99.
      *    -------------------------------
           05  CEMSG2L PIC S9(0004) COMP.
           05  CEMSG2F PIC  X(0001).
           05  FILLER REDEFINES CEMSG2F.
               10  CEMSG2A PIC  X(0001).
           05  CEMSG2I PIC  X(0072).
       01  EL132CO REDEFINES EL132BI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SEQUO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCOMPO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CUSERIDO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCLAIMO PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCARIERO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCERTNOO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCERTSXO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CTYPEO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PCERTNOO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PSUFXO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSTATO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CINCREDO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CREPORTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCAUSCDO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CESTENDO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCAUSEO PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CBENEO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CBDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSSNO PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSEXO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLNAMEO PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CFNAMEO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CMNAMEO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LOANNOO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLNMEO PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CFNMEO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CINITO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LOANBALO PIC  ZZZZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CPROCO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSUPRO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CPRICDO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FILETOO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CPTHHDGO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CPDTHRUO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CTOTPDO PIC  Z(06).99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CNODAYSO PIC  ZZZ99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CNOPMTSO PIC  ZZ99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CESTABO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COCCO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CMNTDTEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CMNTYPEO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCARRO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CGRPO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSTATEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CACCNTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CEFFDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CISAGEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CAPRO PIC  9(3).9(4).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CPFREQO PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CINDGRPO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CPREMTPO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CREINCDO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CJLNMEO PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CJFNMEO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CJINITO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CJAGEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LCVDSCRO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LCVKINDO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LCVCDO PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LCVOTRMO PIC  999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LCVRTRMO PIC  999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LCVBENEO PIC  ZZZZZZZZ.ZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LCVFORMO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LCVCNDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LCVEXITO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LCVSTATO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACVDSCRO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACVKINDO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACVCDO PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACVOTRMO PIC  999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACVRTRMO PIC  999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACVBENEO PIC  ZZZZZZZZ.ZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACVFORMO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACVCNDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACVEXITO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACVSTATO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CEMSG1O PIC  X(0072).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CPFKEYO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CEMSG2O PIC  X(0072).
      *    -------------------------------
00189
00190      EJECT
00191 *    COPY ELCEMIB.
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
00192
00193      EJECT
00194 *    COPY ELCCALC.
00001 ******************************************************************
00002 *                                                                *
00003 *                           ELCCALC.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.025                          *
00006 *                                                                *
00007 *   DESCRIPTION:  DATA TO BE PASSED TO REMAINING TERM ROUTINE    *
00008 *                 REMAINING AMOUNT ROUTINE, LOSS RESERVE ROUTINE *
00009 *                 REFUND CALCULATIONS ROUTINE, EARNINGS CALCU -  *
00010 *                 LATIONS ROUTINE, AND THE RATING ROUTINE.       *
00011 *                                                                *
00012 *  PASSED TO ELRTRM                                              *
00013 *  -----------------                                             *
00014 *  METHOD CODE (I.E. FULL MONTH, HALF ADJ, ETC)                  *
00015 *  ORIGINAL TERM                                                 *
00016 *  BEGINNING DATE                                                *
00017 *  ENDING DATE                                                   *
00018 *  COMPANY I.D.                                                  *
00019 *  ACCOUNT MASTER USER FIELD                                     *
00020 *  PROCESS SWITCH (CANCEL, CLAIM)                                *
00021 *  FREE LOOK DAYS                                                *
00022 *                                                                *
00023 *  RETURNED FROM ELRTRM                                          *
00024 *  ---------------------                                         *
00025 *  REMAINING TERM 1 - USED FOR EARNINGS                          *
00026 *  REMAINING TERM 2 - USED FOR BENEFIT CALCULATIONS              *
00027 *  REMAINING TERM 3 - USED FOR CLAIM BENEFITS                    *
00028 *  ODD DAYS - REMAINING DAYS PAST FULL MONTHS                    *
00029 *----------------------------------------------------------------*
00030 *  PASSED TO ELRAMT                                              *
00031 *  ----------------                                              *
00032 *  REMAINING TERM 1 OR 2 OR 3 (FROM ELRTRM)                      *
00033 *  ORIGINAL TERM (ADJUSTED IF SKIP MONTHS ARE USED)              *
00034 *  ORIGINAL AMOUNT                                               *
00035 *  ALTERNATE BENEFIT (BALLON)                                    *
00036 *  A.P.R. - NET PAY ONLY                                         *
00037 *  METHOD
00038 *  PAYMENT FREQUENCY - FOR FARM PLAN                             *
00039 *  COMPANY I.D.                                                  *
00040 *  BENEFIT TYPE                                                  *
00041 *                                                                *
00042 *  RETURNED FROM ELRAMT                                          *
00043 *  --------------------                                          *
00044 *  REMAINING AMOUNT 1 - CURRENT                                  *
00045 *  REMAINING AMOUNT 2 - PREVIOUS MONTH                           *
00046 *  REMAINING AMOUNT FACTOR
00047 *----------------------------------------------------------------*
00048 *  PASSED TO ELRESV                                              *
00049 *  -----------------                                             *
00050 *  CERTIFICATE EFFECTIVE DATE                                    *
00051 *  VALUATION DATE                                                *
00052 *  PAID THRU DATE                                                *
00053 *  BENEFIT                                                       *
00054 *  INCURRED DATE                                                 *
00055 *  REPORTED DATE                                                 *
00056 *  ISSUE AGE                                                     *
00057 *  TERM                                                          *
00058 *  CDT PERCENT                                                   *
00059 *  CDT METHOD (I.E. INTERPOLATED, AVERAGE, ETC)                  *
00060 * *CLAIM TYPE (LIFE, A/H)                                        *
00061 * *REMAINING BENEFIT (FROM ELRAMT)                               *
00062 * *ONLY FIELDS REQUIRED FOR LIFE CLAIMS                          *
00063 *                                                                *
00064 *  RETURNED FROM ELRESV                                          *
00065 *  --------------------                                          *
00066 *  CDT TABLE USED                                                *
00067 *  CDT FACTOR USED                                               *
00068 *  PAY TO CURRENT RESERVE                                        *
00069 *  I.B.N.R. - A/H ONLY                                           *
00070 *  FUTURE (ACCRUED) AH ONLY                                      *
00071 *----------------------------------------------------------------*
00072 *  PASSED TO ELRATE                                              *
00073 *  ----------------                                              *
00074 *  CERT ISSUE DATE                                               *
00075 *  ORIGINAL TERM (ADJUSTED IF SKIP MONTHS ARE USED)              *
00076 *  TERM OR EXT DAYS  (DAY TERM FOR SP CALC = 'D', ELSE EXT DAYS) *
00077 *  CAPPED TERM   (ONLY FOR TRUNCATED LIFE)                       *
00078 *  STATE CODE (CLIENT DEFINED)                                   *
00079 *  STATE CODE (STANDARD P.O. ABBRV)                              *
00080 *  CLASS CODE (FROM CERT OR ACCOUNT IF CERT ZERO OR SPACES)      *
00081 *  DEVIATION CODE                                                *
00082 *  ISSUE AGE                                                     *
00083 *  ORIGINAL BENEFIT AMOUNT                                       *
00084 *  RATING BENEFIT AMT (TOTAL BENEFIT AMT FOR BALLOONS)           *
00085 *  PROCESS TYPE (ISSUE OR CANCEL)                                *
00086 *  BENEFIT KIND (LIFE OR A/H)                                    *
00087 *  A.P.R.                                                        *
00088 *  METHOD
00089 *  SPECIAL METHOD - (SPECIAL CODE FROM BENEFIT RECORD)           *
00090 *  PAYMENT FREQUENCY  (FOR TEXAS IRREGULAR)                      *
00091 *  COMPANY I.D. (3 CHARACTER)                                    *
00092 *  BENEFIT CODE                                                  *
00093 *  BENEFIT OVERRIDE CODE                                         *
00094 *  MAXIMUM MONTHLY BENEFIT (FROM ACCT MASTER - CSL ONLY)         *
00095 *  MAXIMUM TOTAL BENEFIT (FROM ACCT MASTER - CSL ONLY)           *
00096 *  JOINT INDICATOR (CSL ONLY)                                    *
00097 *  FIRST PAYMENT DATE (CSL ONLY)                                 *
00098 *  PERIODIC PAYMENT AMOUNT (IN CP-REMAINING-TERM - CSL ONLY)     *
00099 *                                                                *
00100 *  RETURNED FROM ELRATE                                          *
00101 *  --------------------                                          *
00102 *  CALCULATED PREMIUM                                            *
00103 *  PREMIUM RATE                                                  *
00104 *  MORTALITY CODE                                                *
00105 *  MAX ATTAINED AGE                                              *
00106 *  MAX AGE                                                       *
00107 *  MAX TERM                                                      *
00108 *  MAX MONTHLY BENEFIT                                           *
00109 *  MAX TOTAL BENIFIT                                             *
00110 *  COMPOSITE RATE (OPEN-END ONLY)                                *
00111 *----------------------------------------------------------------*
00112 *  PASSED TO ELRFND                                              *
00113 *  ----------------                                              *
00114 *  CERT ISSUE DATE                                               *
00115 *  REFUND DATE                                                   *
00116 *  RULE OF 78 OPTION (FROM CONTROL RECORD)                       *
00117 *  ORIGINAL TERM (ADJUSTED IF SKIP MONTHS ARE USED)              *
00118 *  TERM OR EXT DAYS  (DAY TERM FOR SP CALC = 'D', ELSE EXT DAYS) *
00119 *  REMAINING TERM (REMAINING TERM 1 FROM ELTERM)                 *
00120 *  STATE CODE (CLIENT DEFINED)                                   *
00121 *  STATE CODE (STANDARD P.O. ABBRV)                              *
00122 *  CLASS CODE (FROM CERT OR ACCOUNT IF CERT ZERO OR SPACES)      *
00123 *  DEVIATION CODE                                                *
00124 *  ISSUE AGE                                                     *
00125 *  ORIGINAL BENEFIT AMOUNT                                       *
00126 *  RATING BENEFIT AMT (TOTAL BENEFIT AMT FOR BALLOONS)           *
00127 *  PROCESS TYPE (CANCEL)                                         *
00128 *  BENEFIT KIND (LIFE OR A/H)                                    *
00129 *  A.P.R.                                                        *
00130 *  EARNING METHOD - (CODE FROM BENEFIT, STATE OR ACCOUNT RECORD) *
00131 *  RATING METHOD -  (CODE FROM BENEFIT)                          *
00132 *  SPECIAL METHOD - (SPECIAL CODE FROM BENEFIT RECORD)           *
00133 *  PAYMENT FREQUENCY  (FOR TEXAS IRREGULAR)                      *
00134 *  COMPANY I.D. (3 CHARACTER)                                    *
00135 *  BENEFIT CODE                                                  *
00136 *  BENEFIT OVERRIDE CODE                                         *
00137 *                                                                *
00138 *  RETURNED FROM ELRFND                                          *
00139 *  --------------------                                          *
00140 *  CALCULATED REFUND                                             *
00141 *----------------------------------------------------------------*
00142 *  PASSED TO ELEARN                                              *
00143 *  ----------------                                              *
00144 *  CERT ISSUE DATE                                               *
00145 *  ORIGINAL TERM (ADJUSTED IF SKIP MONTHS ARE USED)              *
00146 *  REMAINING TERM (REMAINING TERM 1 FROM ELTERM)                 *
00147 *  RULE OF 78 OPTION (FROM CONTROL RECORD)                       *
00148 *  STATE CODE (CLIENT DEFINED)                                   *
00149 *  STATE CODE (STANDARD P.O. ABBRV)                              *
00150 *  CLASS CODE (FROM CERT OR ACCOUNT IF CERT ZERO OR SPACES)      *
00151 *  DEVIATION CODE                                                *
00152 *  ISSUE AGE                                                     *
00153 *  ORIGINAL BENEFIT AMOUNT                                       *
00154 *  BENEFIT KIND (LIFE OR A/H)                                    *
00155 *  A.P.R.                                                        *
00156 *  METHOD - (EARNING CODE FROM BENEFIT RECORD)                   *
00157 *  SPECIAL METHOD - (SPECIAL CODE FROM BENEFIT RECORD)           *
00158 *  PAYMENT FREQUENCY  (FOR TEXAS IRREGULAR)                      *
00159 *  COMPANY I.D. (3 CHARACTER)                                    *
00160 *  BENEFIT CODE                                                  *
00161 *  BENEFIT OVERRIDE CODE                                         *
00162 *                                                                *
00163 *  RETURNED FROM ELEARN                                          *
00164 *  --------------------                                          *
00165 *  INDICATED  EARNINGS                                           *
00166 *----------------------------------------------------------------*
00167 *                 LENGTH = 450                                   *
00168 *                                                                *
00169 ******************************************************************
010303******************************************************************
010303*                   C H A N G E   L O G
010303*
010303* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
010303*-----------------------------------------------------------------
010303*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
010303* EFFECTIVE    NUMBER
010303*-----------------------------------------------------------------
010303* 010303    2001061800003  PEMA  ADD DCC/MONTHLY PROCESSING
033104* 033104    2003080800002  PEMA  ADD GAP NON REFUNDABLE OPTION
101807* 101807    2007100100007  PEMA  EXPAND CLM RESERVE FIELDS
010410* 010410    2008021200005  PEMA  ADD FIELDS FOR MN NET PAY BALLOON
010410* 010410    2009050700003  PEMA  ADD FIELDS FOR SPP-DD
041310* 041310  CR2008021200005  PEMA  ADD CODE FOR MN LEVEL
041710* 041710    2007111300001  AJRA  ADD CLAIM CALC SW FOR SC NP+6
010303******************************************************************
00170
00171  01  CALCULATION-PASS-AREA.
00172      12  CP-COMM-LENGTH            PIC S9(4)         VALUE +450
00173                                      COMP.
00174
00175      12  CP-RETURN-CODE            PIC X             VALUE ZERO.
00176        88  NO-CP-ERROR                             VALUE ZERO.
00177        88  CP-ERROR-OCCURED VALUE '1' '2' '3' '4' '5' '6' '7' '8'
00178                                   '9' 'A' 'B' 'C' 'D' 'E' 'H'.
00179        88  CP-ERROR-IN-AMOUNTS                     VALUE '1'.
00180        88  CP-ERROR-IN-DATES                       VALUE '2'.
00181        88  CP-ERROR-IN-OPTIONS                     VALUE '3'.
00182        88  CP-ERROR-IN-TERMS                       VALUE '4'.
00183        88  CP-ERROR-IN-FREQUENCY                   VALUE '5'.
00184        88  CP-ERROR-RATE-NOT-FOUND                 VALUE '6'.
00185        88  CP-ERROR-RATE-IS-ZERO                   VALUE '7'.
00186        88  CP-ERROR-AMT-OUTSIDE-LIMIT              VALUE '8'.
00187        88  CP-ERROR-TERM-OUTSIDE-LIMIT             VALUE '9'.
00188        88  CP-ERROR-AGE-OUTSIDE-LIMIT              VALUE 'A'.
00189        88  CP-ERROR-ATT-OUTSIDE-LIMIT              VALUE 'B'.
00190        88  CP-ERROR-TOT-OUTSIDE-LIMIT              VALUE 'C'.
00191        88  CP-ERROR-RATE-FILE-NOTOPEN              VALUE 'D'.
00192        88  CP-ERROR-ISSUE-AGE-ZERO                 VALUE 'E'.
00193        88  CP-ERROR-NO-LIMITS-CRI                  VALUE 'F'.
00194        88  CP-ERROR-DIV-BY-ZERO                    VALUE 'G'.
00195        88  CP-ERROR-LOAN-TERM                      VALUE 'H'.
00196
00197      12  CP-RETURN-CODE-2          PIC X             VALUE ZERO.
00198        88  NO-CP-ERROR-2                           VALUE ZERO.
00199 ***********************  INPUT AREAS ****************************
00200
00201      12  CP-CALCULATION-AREA.
00202          16  CP-ACCOUNT-NUMBER     PIC X(10)       VALUE SPACES.
00203          16  CP-CERT-EFF-DT        PIC XX.
00204          16  CP-VALUATION-DT       PIC XX.
00205          16  CP-PAID-THRU-DT       PIC XX.
00206          16  CP-BENEFIT-TYPE       PIC X.
00207            88  CP-AH                               VALUE 'A' 'D'
00208                                                    'I' 'U'.
00209            88  CP-REDUCING-LIFE                    VALUE 'R'.
00210            88  CP-LEVEL-LIFE                       VALUE 'L' 'P'.
00211          16  CP-INCURRED-DT        PIC XX.
00212          16  CP-REPORTED-DT        PIC XX.
00213          16  CP-ACCT-FLD-5         PIC XX            VALUE SPACE.
00214          16  CP-COMPANY-ID         PIC XXX           VALUE SPACE.
00215          16  CP-ISSUE-AGE          PIC S9(3)         VALUE ZERO
00216                                      COMP-3.
00217          16  CP-CDT-PERCENT        PIC S9(3)V99      VALUE ZERO
00218                                      COMP-3.
00219          16  CP-CDT-METHOD         PIC X.
00220            88  CP-CDT-ROUND-NEAR                   VALUE '1'.
00221            88  CP-CDT-ROUND-HIGH                   VALUE '2'.
00222            88  CP-CDT-INTERPOLATED                 VALUE '3'.
00223          16  CP-CLAIM-TYPE         PIC X.
00224            88  CP-AH-CLAIM                         VALUE 'A'.
00225            88  CP-LIFE-CLAIM                       VALUE 'L'.
00226          16  CP-ORIGINAL-TERM      PIC S9(3)         VALUE ZERO
00227                                      COMP-3.
00228          16  CP-ORIGINAL-BENEFIT   PIC S9(9)V99      VALUE ZERO
00229                                      COMP-3.
00230          16  CP-ORIGINAL-PREMIUM   PIC S9(7)V99      VALUE ZERO
00231                                      COMP-3.
00232          16  CP-REMAINING-TERM     PIC S9(3)V99      VALUE ZERO
00233                                      COMP-3.
00234          16  CP-REMAINING-BENEFIT  PIC S9(9)V99      VALUE ZERO
00235                                      COMP-3.
00236          16  CP-LOAN-APR           PIC S9(3)V9(4)    VALUE ZERO
00237                                      COMP-3.
00238          16  CP-PAY-FREQUENCY      PIC S9(3)         VALUE ZERO
00239                                      COMP-3.
00240          16  CP-REM-TERM-METHOD    PIC X.
00241            88  CP-EARN-AFTER-15TH                  VALUE '1'.
00242            88  CP-EARN-ON-HALF-MONTH               VALUE '2'.
00243            88  CP-EARN-ON-1ST-DAY                  VALUE '3'.
00244            88  CP-EARN-ON-FULL-MONTH               VALUE '4'.
00245            88  CP-EARN-WITH-NO-DAYS                VALUE '5'.
00246            88  CP-EARN-AFTER-14TH                  VALUE '6'.
00247            88  CP-EARN-AFTER-16TH                  VALUE '7'.
00248          16  CP-EARNING-METHOD     PIC X.
00249            88  CP-EARN-BY-R78                      VALUE '1' 'R'.
00250            88  CP-EARN-BY-PRORATA                  VALUE '2' 'P'.
00251            88  CP-EARN-AS-CALIF                    VALUE '3' 'C'.
00252            88  CP-EARN-AS-TEXAS                    VALUE '4' 'T'.
00253            88  CP-EARN-AS-FARM-PLAN                VALUE '4' 'T'.
00254            88  CP-EARN-AS-NET-PAY                  VALUE '5' 'N'.
00255            88  CP-EARN-ANTICIPATION                VALUE '6' 'A'.
00256            88  CP-EARN-AS-MEAN                     VALUE '8' 'M'.
00257            88  CP-EARN-AS-SUM-OF-DIGITS            VALUE '9'.
00258            88  CP-EARN-AS-REG-BALLOON              VALUE 'B'.
033104           88  CP-GAP-NON-REFUNDABLE               VALUE 'G'.
033104           88  CP-GAP-ACTUARIAL                    VALUE 'S'.
00259          16  CP-PROCESS-TYPE       PIC X.
00260            88  CP-CLAIM                            VALUE '1'.
00261            88  CP-CANCEL                           VALUE '2'.
00262            88  CP-ISSUE                            VALUE '3'.
00263          16  CP-SPECIAL-CALC-CD    PIC X.
00264            88  CP-OUTSTANDING-BAL              VALUE 'O'.
00265            88  CP-1-MTH-INTEREST               VALUE ' '.
00266            88  CP-0-MTH-INTEREST               VALUE 'A'.
00267            88  CP-OB-OFFLINE-RESERVED          VALUE 'B'.
00268            88  CP-CRITICAL-PERIOD              VALUE 'C'.
00269            88  CP-TERM-IS-DAYS                 VALUE 'D'.
00270            88  CP-USE-PREM-AS-ENTERED          VALUE 'E'.
00271            88  CP-FARM-PLAN                    VALUE 'F'.
00272            88  CP-RATE-AS-STANDARD             VALUE 'G'.
00273            88  CP-2-MTH-INTEREST               VALUE 'I'.
00274            88  CP-3-MTH-INTEREST               VALUE 'J'.
00275            88  CP-4-MTH-INTEREST               VALUE 'K'.
00276            88  CP-BALLOON-LAST-PMT             VALUE 'L'.
00277            88  CP-MORTGAGE-REC                 VALUE 'M'.
00278            88  CP-OUTSTANDING-BALANCE          VALUE 'O'.
00279            88  CP-NET-PAY-PRUDENTIAL           VALUE 'P'.
00280            88  CP-NET-PAY-SIMPLE               VALUE 'S'.
00281            88  CP-TRUNCATED-LIFE               VALUE 'T' 'U' 'V'
00282                                                      'W' 'X'.
00283            88  CP-TRUNCATE-0-MTH               VALUE 'T'.
00284            88  CP-TRUNCATE-1-MTH               VALUE 'U'.
00285            88  CP-TRUNCATE-2-MTH               VALUE 'V'.
00286            88  CP-TRUNCATE-3-MTH               VALUE 'W'.
00287            88  CP-TRUNCATE-4-MTH               VALUE 'X'.
00288            88  CP-SUMMARY-REC                  VALUE 'Z'.
00289            88  CP-PROPERTY-BENEFIT             VALUE '2'.
00290            88  CP-UNEMPLOYMENT-BENEFIT         VALUE '3'.
00291            88  CP-AD-D-BENEFIT                 VALUE '4'.
00292            88  CP-CSL-METH-1                   VALUE '5'.
00293            88  CP-CSL-METH-2                   VALUE '6'.
00294            88  CP-CSL-METH-3                   VALUE '7'.
00295            88  CP-CSL-METH-4                   VALUE '8'.
00296
00297          16  CP-LOAN-TERM          PIC S9(3)       VALUE ZERO
00298                                      COMP-3.
00299          16  CP-CLASS-CODE         PIC XX          VALUE ZERO.
00300          16  CP-DEVIATION-CODE     PIC XXX         VALUE ZERO.
00301          16  CP-STATE              PIC XX          VALUE SPACE.
00302          16  CP-STATE-STD-ABBRV    PIC XX          VALUE SPACE.
00303          16  CP-BENEFIT-CD         PIC XX          VALUE ZERO.
00304            88  CP-CSL-VALID-NP-BENEFIT-CD VALUES '12' '13'
00305                '34' '35' '36' '37' '44' '45' '46' '47' '72' '73'.
00306          16  CP-R78-OPTION         PIC X.
00307            88  CP-TERM-TIMES-TERM-PLUS-1           VALUE ' '.
00308            88  CP-TERM-TIMES-TERM                  VALUE '1'.
00309
00310          16  CP-COMPANY-CD         PIC X             VALUE SPACE.
00311          16  CP-IBNR-RESERVE-SW    PIC X.
00312          16  CP-CLAIM-STATUS       PIC X.
00313          16  CP-RATE-FILE          PIC X.
00314          16  CP-TERM-OR-EXT-DAYS   PIC S9(05)        VALUE ZERO
00315                                      COMP-3.
00316
00317          16  CP-LIFE-OVERRIDE-CODE PIC X.
00318          16  CP-AH-OVERRIDE-CODE   PIC X.
00319
00320          16  CP-RATE-DEV-PCT       PIC S9V9(6)       VALUE ZERO
00321                                      COMP-3.
00322          16  CP-CRITICAL-MONTHS    PIC S9(3)         VALUE ZERO
00323                                      COMP-3.
00324          16  CP-ALTERNATE-BENEFIT  PIC S9(9)V99      VALUE ZERO
00325                                      COMP-3.
00326          16  CP-ALTERNATE-PREMIUM  PIC S9(7)V99      VALUE ZERO
00327                                      COMP-3.
00328
00329          16  CP-PAID-FROM-DATE     PIC X(02).
00330          16  CP-CLAIM-CALC-METHOD  PIC X(01).
00331          16  CP-EXT-DAYS-CALC      PIC X.
00332            88  CP-EXT-NO-CHG                   VALUE ' '.
00333            88  CP-EXT-CHG-LF                   VALUE '1'.
00334            88  CP-EXT-CHG-AH                   VALUE '2'.
00335            88  CP-EXT-CHG-LF-AH                VALUE '3'.
00336          16  CP-DOMICILE-STATE     PIC XX.
00337          16  CP-CARRIER            PIC X.
00338          16  CP-REIN-FLAG          PIC X.
00339          16  CP-REM-TRM-CALC-OPTION PIC X.
00340            88  VALID-REM-TRM-CALC-OPTION    VALUE '1'
00341                       '2' '3' '4' '5'.
00342            88  CP-CALC-OPTION-DEFAULT       VALUE '4'.
00343            88  CP-CONSIDER-EXTENSION        VALUE '3' '4' '5'.
00344            88  CP-30-DAY-MONTH              VALUE '1' '3' '5'.
00345            88  CP-NO-EXT-30-DAY-MONTH       VALUE '1'.
00346            88  CP-NO-EXT-ACTUAL-DAYS        VALUE '2'.
00347            88  CP-EXT-30-DAY-MONTH          VALUE '3'.
00348            88  CP-EXT-ACTUAL-DAYS           VALUE '4'.
                 88  CP-USE-EXP-AND-1ST-PMT       VALUE '5'.
00349          16  CP-SIG-SWITCH         PIC X.
00350          16  CP-RATING-METHOD      PIC X.
00351            88  CP-RATE-AS-R78                      VALUE '1' 'R'.
00352            88  CP-RATE-AS-PRORATA                  VALUE '2' 'P'.
00353            88  CP-RATE-AS-CALIF                    VALUE '3' 'C'.
00354            88  CP-RATE-AS-TEXAS                    VALUE '4' 'T'.
00355            88  CP-RATE-AS-FARM-PLAN                VALUE '4' 'T'.
00356            88  CP-RATE-AS-NET-PAY                  VALUE '5' 'N'.
00357            88  CP-RATE-AS-ANTICIPATION             VALUE '6' 'A'.
00358            88  CP-RATE-AS-MEAN                     VALUE '8' 'M'.
00359            88  CP-RATE-AS-REG-BALLOON              VALUE 'B'.
00360          16  CP-SALES-TAX          PIC S9V9999     VALUE  ZEROS
00361                                      COMP-3.
090803         16  CP-BEN-CATEGORY       PIC X.
011904         16  CP-DCC-LF-RATE        PIC S99V9(5) COMP-3 VALUE +0.
               16  CP-DCC-ACT-COMM REDEFINES CP-DCC-LF-RATE
                                         PIC S99V9(5) COMP-3.
011904         16  CP-DCC-AH-RATE        PIC S99V9(5) COMP-3 VALUE +0.
               16  CP-DCC-PMF-COMM REDEFINES CP-DCC-AH-RATE
                                         PIC S99V9(5) COMP-3.
080305         16  CP-DAYS-TO-1ST-PMT    PIC S999     COMP-3 VALUE +0.
               16  CP-AH-BALLOON-SW      PIC X  VALUE ' '.
041310         16  CP-EXPIRE-DT          PIC XX.
041710         16  CP-LF-CLAIM-CALC-SW   PIC X  VALUE ' '.
041710         16  FILLER                PIC X(35).
090803*        16  FILLER                PIC X(50).
00363
00364 ***************    OUTPUT FROM ELRESV   ************************
00365
00366          16  CP-CDT-TABLE          PIC 9             VALUE ZERO.
00367
00368          16  CP-CDT-FACTOR         PIC S9(5)V9(6)    VALUE ZERO
00369                                      COMP-3.
101807         16  CP-PTC-RESERVE        PIC S9(7)V99   VALUE ZERO
101807                                     COMP-3.
101807         16  CP-IBNR-RESERVE       PIC S9(7)V99   VALUE ZERO
101807                                     COMP-3.
101807         16  CP-FUTURE-RESERVE     PIC S9(7)V99   VALUE ZERO
101807                                     COMP-3.
101807         16  FILLER                PIC X(09).
00377 ***************    OUTPUT FROM ELRTRM   *************************
00378
00379          16  CP-REMAINING-TERM-1   PIC S9(4)V9    VALUE ZERO
00380                                      COMP-3.
00381          16  CP-REMAINING-TERM-2   PIC S9(4)V9    VALUE ZERO
00382                                      COMP-3.
00383          16  CP-REMAINING-TERM-3   PIC S9(4)V9    VALUE ZERO
00384                                      COMP-3.
00385          16  CP-ODD-DAYS           PIC S9(3)      VALUE ZERO
00386                                      COMP-3.
00387          16  FILLER                PIC X(12).
00388
00389 ***************    OUTPUT FROM ELRAMT   *************************
00390
00391          16  CP-REMAINING-AMT      PIC S9(9)V99   VALUE ZERO
00392                                      COMP-3.
00393          16  CP-REMAINING-AMT-PRV  PIC S9(9)V99   VALUE ZERO
00394                                      COMP-3.
00395          16  FILLER                PIC X(12).
00396
00397 ***************    OUTPUT FROM ELRATE   *************************
00398
00399          16  CP-CALC-PREMIUM       PIC S9(7)V99   VALUE ZERO
00400                                      COMP-3.
00401          16  CP-PREMIUM-RATE       PIC S9(2)V9(5) VALUE ZERO
00402                                      COMP-3.
00403          16  CP-MORTALITY-CODE     PIC X(4).
00404          16  CP-RATE-EDIT-FLAG     PIC X.
00405              88  CP-RATES-NEED-APR                  VALUE '1'.
00406          16  CP-COMPOSITE-RATE     PIC S99V999    VALUE ZERO
00407                                      COMP-3.
00408          16  CP-POLICY-FEE         PIC S9(3)V99 VALUE +0 COMP-3.
032905         16  CP-LF-PREM            PIC S9(7)V99 VALUE +0 COMP-3.
               16  CP-LF-BALLOON-PREM REDEFINES CP-LF-PREM
                                         PIC S9(7)V99 COMP-3.
00409          16  FILLER                PIC X(07).
00410
00411 ***************    OUTPUT FROM ELRFND   *************************
00412
00413          16  CP-CALC-REFUND        PIC S9(7)V99   VALUE ZERO
00414                                      COMP-3.
00415          16  CP-REFUND-TYPE-USED   PIC X.
00416            88  CP-R-AS-R78                         VALUE '1'.
00417            88  CP-R-AS-PRORATA                     VALUE '2'.
00418            88  CP-R-AS-CALIF                       VALUE '3'.
00419            88  CP-R-AS-TEXAS                       VALUE '4'.
00420            88  CP-R-AS-FARM-PLAN                   VALUE '4'.
00421            88  CP-R-AS-NET-PAY                     VALUE '5'.
00422            88  CP-R-AS-ANTICIPATION                VALUE '6'.
00423            88  CP-R-AS-MEAN                        VALUE '8'.
00424            88  CP-R-AS-SUM-OF-DIGITS               VALUE '9'.
033104           88  CP-R-AS-GAP-NON-REFUND              VALUE 'G'.
033104           88  CP-R-AS-GAP-ACTUARIAL               VALUE 'S'.
00425          16  FILLER                PIC X(12).
00426
00427 ***************    OUTPUT FROM ELEARN   *************************
00428
00429          16  CP-R78-U-PRM          PIC S9(7)V99   VALUE ZERO
00430                                      COMP-3.
00431          16  CP-R78-U-PRM-ALT      PIC S9(7)V99   VALUE ZERO
00432                                      COMP-3.
00433          16  CP-PRORATA-U-PRM      PIC S9(7)V99   VALUE ZERO
00434                                      COMP-3.
00435          16  CP-PRORATA-U-PRM-ALT  PIC S9(7)V99   VALUE ZERO
00436                                      COMP-3.
00437          16  CP-STATE-U-PRM        PIC S9(7)V99   VALUE ZERO
00438                                      COMP-3.
00439          16  CP-DOMICILE-U-PRM     PIC S9(7)V99   VALUE ZERO
00440                                      COMP-3.
00441          16  CP-EARNING-TYPE-USED  PIC X.
00442            88  CP-E-AS-SPECIAL                     VALUE 'S'.
00443            88  CP-E-AS-R78                         VALUE '1'.
00444            88  CP-E-AS-PRORATA                     VALUE '2'.
00445            88  CP-E-AS-TEXAS                       VALUE '4'.
00446            88  CP-E-AS-FARM-PLAN                   VALUE '4'.
00447            88  CP-E-AS-NET-PAY                     VALUE '5'.
00448            88  CP-E-AS-ANTICIPATION                VALUE '6'.
00449            88  CP-E-AS-MEAN                        VALUE '8'.
00450            88  CP-E-AS-SUM-OF-DIGITS               VALUE '9'.
00451          16  FILLER                PIC X(12).
00452
00453 ***************    OUTPUT FROM ELPMNT   *************************
00454
00455          16  CP-ACTUAL-DAYS        PIC S9(05)     VALUE ZERO
00456                                      COMP-3.
00457          16  CP-CLAIM-PAYMENT      PIC S9(7)V99   VALUE ZERO
00458                                      COMP-3.
00459          16  FILLER                PIC X(12).
00460
00461 ***************   MISC WORK AREAS    *****************************
00462          16  CP-TOTAL-PAID         PIC S9(7)V99   VALUE ZERO
00463                                      COMP-3.
00464          16  CP-R-MAX-ATT-AGE      PIC S9(3)      VALUE ZERO
00465                                      COMP-3.
00466          16  CP-R-MAX-AGE          PIC S9(3)      VALUE ZERO
00467                                      COMP-3.
00468          16  CP-R-MAX-TERM         PIC S9(5)      VALUE ZERO
00469                                      COMP-3.
00470          16  CP-R-MAX-TOT-BEN      PIC S9(7)V99   VALUE ZERO
00471                                      COMP-3.
00472          16  CP-R-MAX-MON-BEN      PIC S9(7)V99   VALUE ZERO
00473                                      COMP-3.
00474          16  CP-IO-FUNCTION        PIC X          VALUE SPACE.
00475              88  OPEN-RATE-FILE                   VALUE 'O'.
00476              88  CLOSE-RATE-FILE                  VALUE 'C'.
00477              88  IO-ERROR                         VALUE 'E'.
00478
00479          16  CP-FIRST-PAY-DATE     PIC XX.
00480
00481          16  CP-JOINT-INDICATOR    PIC X.
00482
00483          16  CP-RESERVE-REMAINING-TERM
00484                                    PIC S9(4)V9    VALUE ZERO
00485                                      COMP-3.
00486
00487          16  CP-INSURED-BIRTH-DT   PIC XX.
00488
00489          16  CP-INCURRED-AGE       PIC S9(3)      VALUE ZERO
00490                                      COMP-3.
00491
00492          16  CP-MONTHLY-PAYMENT    PIC S9(5)V99   VALUE ZERO
00493                                      COMP-3.
00494
00495          16  CP-RATING-BENEFIT-AMT PIC S9(9)V99   VALUE ZERO
00496                                      COMP-3.
00497
00498          16  CP-ODD-DAYS-TO-PMT    PIC S9(3)      VALUE ZERO
00499                                      COMP-3.
00500
00501          16  CP-MNTHS-TO-FIRST-PMT PIC S9(3)      VALUE ZERO
00502                                      COMP-3.
00503
00504          16  CP-REMAMT-FACTOR      PIC S9(4)V9(9) VALUE ZEROS
00505                                      COMP-3.
00506
00507          16  CP-FREE-LOOK          PIC S9(3)      VALUE ZERO
00508                                      COMP-3.
00509
00510          16  CP-ROA-REFUND         PIC X          VALUE 'N'.
00511              88  CP-ROA-PREM-AT-REFUND            VALUE 'Y'.
00512
010303         16  CP-NET-BENEFIT-AMT    PIC S9(9)V99   VALUE ZERO
010303                                     COMP-3.
041710         16  CP-SCNP-6MO-AMT       PIC S9(9)V99   VALUE ZERO
041710                                     COMP-3.
041710         16  FILLER                PIC X(17).
00514 ******************************************************************
00195
00196      EJECT
00197 *    COPY ELCLOGOF.
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
00198
00199      EJECT
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
00201
00202  01  DFHCOMMAREA                  PIC X(1024).
00203
00204      EJECT
00205 *    COPY ELCCERT.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCCERT.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.013                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = CERTIFICATE MASTER                        *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 450  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELCERT                         RKP=2,LEN=33   *
00013 *       ALTERNATE PATH1 = ELCERT2 (BY NAME)       RKP=35,LEN=18  *
00014 *       ALTERNATE PATH2 = ELCERT3 (BY SOC SEC NO) RKP=53,LEN=12  *
00015 *       ALTERNATE PATH3 = ELCERT5 (BY CERT NO.)   RKP=65,LEN=12  *
00016 *       ALTERNATE PATH4 = ELCERT6 (BY MEMBER NO.) RKP=77,LEN=13  *
00017 *                                                                *
00018 *   LOG = YES                                                    *
00019 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
122002******************************************************************
122002*                   C H A N G E   L O G
122002*
122002* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
122002*-----------------------------------------------------------------
122002*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
122002* EFFECTIVE    NUMBER
122002*-----------------------------------------------------------------
122002* 122002                   PEMA  ADD MONTHLY PRODUCT PROCESSING
040504* 040504  CR2003080800002  PEMA  ADD DEALER INCENTIVE PROCESSING
061405* 061405  CR2005060300001  PEMA  ADD CLP STATE PROCESS FOR DCC
110105* 110105    2005071200004  PEMA  INCREASE SIZE OF LOAN OFFICER
072308* 072308  CR2007110500003  PEMA  ADD NH REFUND INTEREST PROCESSING
102109* 102109  CR2008100900003  AJRA  ADD CLAIM CERT NOTE IND
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
122002******************************************************************
00021
00022  01  CERTIFICATE-MASTER.
00023      12  CM-RECORD-ID                      PIC XX.
00024          88  VALID-CM-ID                      VALUE 'CM'.
00025
00026      12  CM-CONTROL-PRIMARY.
00027          16  CM-COMPANY-CD                 PIC X.
00028          16  CM-CARRIER                    PIC X.
00029          16  CM-GROUPING.
00030              20  CM-GROUPING-PREFIX        PIC X(3).
00031              20  CM-GROUPING-PRIME         PIC X(3).
00032          16  CM-STATE                      PIC XX.
00033          16  CM-ACCOUNT.
00034              20  CM-ACCOUNT-PREFIX         PIC X(4).
00035              20  CM-ACCOUNT-PRIME          PIC X(6).
00036          16  CM-CERT-EFF-DT                PIC XX.
00037          16  CM-CERT-NO.
00038              20  CM-CERT-PRIME             PIC X(10).
00039              20  CM-CERT-SFX               PIC X.
00040
00041      12  CM-CONTROL-BY-NAME.
00042          16  CM-COMPANY-CD-A1              PIC X.
00043          16  CM-INSURED-LAST-NAME          PIC X(15).
00044          16  CM-INSURED-INITIALS.
00045              20  CM-INSURED-INITIAL1       PIC X.
00046              20  CM-INSURED-INITIAL2       PIC X.
00047
00048      12  CM-CONTROL-BY-SSN.
00049          16  CM-COMPANY-CD-A2              PIC X.
00050          16  CM-SOC-SEC-NO.
00051              20  CM-SSN-STATE              PIC XX.
00052              20  CM-SSN-ACCOUNT            PIC X(6).
00053              20  CM-SSN-LN3.
00054                  25  CM-INSURED-INITIALS-A2.
00055                      30 CM-INSURED-INITIAL1-A2   PIC X.
00056                      30 CM-INSURED-INITIAL2-A2   PIC X.
00057                  25 CM-PART-LAST-NAME-A2         PIC X.
00058
00059      12  CM-CONTROL-BY-CERT-NO.
00060          16  CM-COMPANY-CD-A4              PIC X.
00061          16  CM-CERT-NO-A4                 PIC X(11).
00062
00063      12  CM-CONTROL-BY-MEMB.
00064          16  CM-COMPANY-CD-A5              PIC X.
00065          16  CM-MEMBER-NO.
00066              20  CM-MEMB-STATE             PIC XX.
00067              20  CM-MEMB-ACCOUNT           PIC X(6).
00068              20  CM-MEMB-LN4.
00069                  25  CM-INSURED-INITIALS-A5.
00070                      30 CM-INSURED-INITIAL1-A5   PIC X.
00071                      30 CM-INSURED-INITIAL2-A5   PIC X.
00072                  25 CM-PART-LAST-NAME-A5         PIC XX.
00073
00074      12  CM-INSURED-PROFILE-DATA.
00075          16  CM-INSURED-FIRST-NAME.
00076              20  CM-INSURED-1ST-INIT       PIC X.
00077              20  FILLER                    PIC X(9).
00078          16  CM-INSURED-ISSUE-AGE          PIC 99.
00079          16  CM-INSURED-SEX                PIC X.
00080              88  CM-SEX-MALE                  VALUE 'M'.
00081              88  CM-SEX-FEMAL                 VALUE 'F'.
00082          16  CM-INSURED-JOINT-AGE          PIC 99.
00083          16  CM-JOINT-INSURED-NAME.
00084              20  CM-JT-LAST-NAME           PIC X(15).
00085              20  CM-JT-FIRST-NAME.
00086                  24  CM-JT-1ST-INIT        PIC X.
00087                  24  FILLER                PIC X(9).
00088              20  CM-JT-INITIAL             PIC X.
00089
00090      12  CM-LIFE-DATA.
00091          16  CM-LF-BENEFIT-CD              PIC XX.
00092          16  CM-LF-ORIG-TERM               PIC S999      COMP-3.
00093          16  CM-LF-CRITICAL-PERIOD         PIC S999      COMP-3.
00094          16  CM-LF-TERM-IN-DAYS            PIC S9(5)     COMP-3.
00095          16  CM-LF-DEV-CODE                PIC XXX.
00096          16  CM-LF-DEV-PCT                 PIC S9V9(6)   COMP-3.
00097          16  CM-LF-BENEFIT-AMT             PIC S9(9)V99  COMP-3.
00098          16  CM-LF-PREMIUM-AMT             PIC S9(7)V99  COMP-3.
00099          16  CM-LF-ALT-BENEFIT-AMT         PIC S9(9)V99  COMP-3.
00100          16  CM-LF-ALT-PREMIUM-AMT         PIC S9(7)V99  COMP-3.
00101          16  CM-LF-NSP-PREMIUM-AMT         PIC S9(7)V99  COMP-3.
00102          16  CM-LF-REMAINING-AMT           PIC S9(9)V99  COMP-3.
00103          16  CM-LF-ITD-CANCEL-AMT          PIC S9(7)V99  COMP-3.
00104          16  CM-LF-ITD-DEATH-AMT           PIC S9(9)V99  COMP-3.
00105          16  CM-LF-PREMIUM-RATE            PIC S99V9(5)  COMP-3.
00106          16  CM-LF-POLICY-FEE              PIC S9(3)V99  COMP-3.
00107          16  CM-LF-ALT-PREMIUM-RATE        PIC S99V9(5)  COMP-3.
00108          16  FILLER                        PIC XX.
00109
00110      12  CM-AH-DATA.
00111          16  CM-AH-BENEFIT-CD              PIC XX.
00112          16  CM-AH-ORIG-TERM               PIC S999      COMP-3.
00113          16  CM-AH-CRITICAL-PERIOD         PIC S999      COMP-3.
00114          16  CM-AH-DEV-CODE                PIC XXX.
00115          16  CM-AH-DEV-PCT                 PIC S9V9(6)   COMP-3.
00116          16  CM-AH-BENEFIT-AMT             PIC S9(7)V99  COMP-3.
00117          16  CM-AH-PREMIUM-AMT             PIC S9(7)V99  COMP-3.
00118          16  CM-AH-NSP-PREMIUM-AMT         PIC S9(7)V99  COMP-3.
00119          16  CM-AH-ITD-CANCEL-AMT          PIC S9(7)V99  COMP-3.
00120          16  CM-AH-ITD-LUMP-PMT            PIC S9(7)V99  COMP-3.
00121          16  CM-AH-ITD-AH-PMT              PIC S9(9)V99  COMP-3.
00122          16  CM-AH-PAID-THRU-DT            PIC XX.
00123              88  NO-AH-CLAIMS-PAID            VALUE LOW-VALUE.
00124          16  CM-AH-PREMIUM-RATE            PIC S99V9(5)  COMP-3.
00125          16  CM-AH-POLICY-FEE              PIC S9(3)V99  COMP-3.
00126          16  CM-AH-CEDED-BENEFIT           PIC S9(7)V99  COMP-3.
00127          16  FILLER                        PIC X.
00128
00129      12  CM-LOAN-INFORMATION.
00130          16  CM-LIVES                      PIC S9(7)     COMP-3.
00131          16  CM-BILLED                     PIC S9(7)     COMP-3.
00132          16  CM-LOAN-APR                   PIC S999V9(4) COMP-3.
00133          16  CM-PAY-FREQUENCY              PIC S99.
00134          16  CM-LOAN-TERM                  PIC S999      COMP-3.
00135          16  CM-RATE-CLASS                 PIC XX.
00136          16  CM-BENEFICIARY                PIC X(25).
00137          16  CM-POLICY-FORM-NO             PIC X(12).
00138          16  CM-PMT-EXTENSION-DAYS         PIC S999      COMP-3.
00139          16  CM-LAST-ADD-ON-DT             PIC XX.
00140          16  CM-DEDUCTIBLE-AMOUNTS.
00141              20  CM-CLAIM-DEDUCT-WITHHELD  PIC S9(5)V99  COMP-3.
00142              20  CM-CANCEL-DEDUCT-WITHHELD PIC S9(5)V99  COMP-3.
00143          16  CM-RESIDENT-RATE REDEFINES CM-DEDUCTIBLE-AMOUNTS.
00144              20  CM-RESIDENT-STATE         PIC XX.
00145              20  CM-RATE-CODE              PIC X(4).
00146              20  FILLER                    PIC XX.
110105         16  FILLER REDEFINES CM-DEDUCTIBLE-AMOUNTS.
110105             20  CM-LOAN-OFFICER           PIC X(5).
110105             20  FILLER                    PIC XXX.
00147          16  CM-CSR-CODE                   PIC XXX.
00148          16  CM-UNDERWRITING-CODE          PIC X.
00149              88  CM-POLICY-UNDERWRITTEN       VALUE 'Y'.
081606         16  CM-POST-CARD-IND              PIC X.
072308         16  CM-NH-INTERFACE-SW            PIC X.
00151          16  CM-PREMIUM-TYPE               PIC X.
00152              88  CM-SING-PRM                  VALUE '1'.
00153              88  CM-O-B-COVERAGE              VALUE '2'.
00154              88  CM-OPEN-END                  VALUE '3'.
00155          16  CM-IND-GRP-TYPE               PIC X.
00156              88  CM-INDIVIDUAL                VALUE 'I'.
00157              88  CM-GROUP                     VALUE 'G'.
00158          16  CM-SKIP-CODE                  PIC X.
00159              88  NO-MONTHS-SKIPPED            VALUE SPACE.
00160              88  SKIP-JULY                    VALUE '1'.
00161              88  SKIP-AUGUST                  VALUE '2'.
00162              88  SKIP-SEPTEMBER               VALUE '3'.
00163              88  SKIP-JULY-AUG                VALUE '4'.
00164              88  SKIP-AUG-SEPT                VALUE '5'.
00165              88  SKIP-JULY-AUG-SEPT           VALUE '6'.
00166              88  SKIP-JUNE-JULY-AUG           VALUE '7'.
00167              88  SKIP-JUNE                    VALUE '8'.
00168              88  SKIP-JUNE-JULY               VALUE '9'.
00169              88  SKIP-AUG-SEPT-OCT            VALUE 'A'.
00170              88  SKIP-BI-WEEKLY-3RD-PMT       VALUE 'X'.
00171          16  CM-PAYMENT-MODE               PIC X.
00172              88  PAY-MONTHLY                  VALUE SPACE.
00173              88  PAY-WEEKLY                   VALUE '1'.
00174              88  PAY-SEMI-MONTHLY             VALUE '2'.
00175              88  PAY-BI-WEEKLY                VALUE '3'.
00176              88  PAY-SEMI-ANUALLY             VALUE '4'.
00177          16  CM-LOAN-NUMBER                PIC X(8).
00178          16  CM-LOAN-BALANCE               PIC S9(7)V99  COMP-3.
110105         16  CM-OLD-LOF                    PIC XXX.
00179 *        16  CM-LOAN-OFFICER               PIC XXX.
00180          16  CM-REIN-TABLE                 PIC XXX.
00181          16  CM-SPECIAL-REIN-CODE          PIC X.
00182          16  CM-LF-LOAN-EXPIRE-DT          PIC XX.
00183          16  CM-AH-LOAN-EXPIRE-DT          PIC XX.
00184          16  CM-LOAN-1ST-PMT-DT            PIC XX.
00185
00186      12  CM-STATUS-DATA.
00187          16  CM-ENTRY-STATUS               PIC X.
00188          16  CM-ENTRY-DT                   PIC XX.
00189
00190          16  CM-LF-STATUS-AT-CANCEL        PIC X.
00191          16  CM-LF-CANCEL-DT               PIC XX.
00192          16  CM-LF-CANCEL-EXIT-DT          PIC XX.
00193
00194          16  CM-LF-STATUS-AT-DEATH         PIC X.
00195          16  CM-LF-DEATH-DT                PIC XX.
00196          16  CM-LF-DEATH-EXIT-DT           PIC XX.
00197
00198          16  CM-LF-CURRENT-STATUS          PIC X.
00199              88  CM-LF-POLICY-IS-ACTIVE       VALUE '1' '2' '3'
00200                                                 'M' '4' '5' '9'.
00201              88  CM-LF-NORMAL-ENTRY           VALUE '1'.
00202              88  CM-LF-POLICY-PENDING         VALUE '2'.
00203              88  CM-LF-POLICY-IS-RESTORE      VALUE '3'.
00204              88  CM-LF-CONVERSION-ENTRY       VALUE '4'.
00205              88  CM-LF-POLICY-IS-REISSUE      VALUE '5'.
122002             88  CM-LF-POLICY-IS-MONTHLY      VALUE 'M'.
00206              88  CM-LF-LUMP-SUM-DISAB         VALUE '6'.
00207              88  CM-LF-DEATH-CLAIM-APPLIED    VALUE '7'.
00208              88  CM-LF-CANCEL-APPLIED         VALUE '8'.
00209              88  CM-LF-IS-REIN-ONLY           VALUE '9'.
00210              88  CM-LF-DECLINED               VALUE 'D'.
00211              88  CM-LF-VOIDED                 VALUE 'V'.
00212
00213          16  CM-AH-STATUS-AT-CANCEL        PIC X.
00214          16  CM-AH-CANCEL-DT               PIC XX.
00215          16  CM-AH-CANCEL-EXIT-DT          PIC XX.
00216
00217          16  CM-AH-STATUS-AT-SETTLEMENT    PIC X.
00218          16  CM-AH-SETTLEMENT-DT           PIC XX.
00219          16  CM-AH-SETTLEMENT-EXIT-DT      PIC XX.
00220
00221          16  CM-AH-CURRENT-STATUS          PIC X.
00222              88  CM-AH-POLICY-IS-ACTIVE       VALUE '1' '2' '3'
00223                                                 'M' '4' '5' '9'.
00224              88  CM-AH-NORMAL-ENTRY           VALUE '1'.
00225              88  CM-AH-POLICY-PENDING         VALUE '2'.
00226              88  CM-AH-POLICY-IS-RESTORE      VALUE '3'.
00227              88  CM-AH-CONVERSION-ENTRY       VALUE '4'.
00228              88  CM-AH-POLICY-IS-REISSUE      VALUE '5'.
122002             88  CM-AH-POLICY-IS-MONTHLY      VALUE 'M'.
00229              88  CM-AH-LUMP-SUM-DISAB         VALUE '6'.
00230              88  CM-AH-DEATH-CLAIM-APPLIED    VALUE '7'.
00231              88  CM-AH-CANCEL-APPLIED         VALUE '8'.
00232              88  CM-AH-IS-REIN-ONLY           VALUE '9'.
00233              88  CM-AH-DECLINED               VALUE 'D'.
00234              88  CM-AH-VOIDED                 VALUE 'V'.
00235
00236          16  CM-CLAIM-INTERFACE-SW         PIC X.
00237              88  NO-CLAIM-ATTACHED            VALUE SPACE.
00238              88  CERT-AND-CLAIM-ONLINE        VALUE '1'.
00239              88  CERT-WAS-CREATED-FOR-CLAIM   VALUE '2'.
00240          16  CM-CLAIM-ATTACHED-COUNT       PIC S9(4)     COMP.
00241
00242          16  CM-ENTRY-BATCH                PIC X(6).
00243          16  CM-LF-EXIT-BATCH              PIC X(6).
00244          16  CM-AH-EXIT-BATCH              PIC X(6).
00245          16  CM-LAST-MONTH-END             PIC XX.
00246
00247      12  CM-NOTE-SW                        PIC X.
00248          88  CERT-NOTES-ARE-NOT-PRESENT       VALUE ' '.
00249          88  CERT-NOTES-PRESENT               VALUE '1'.
00250          88  BILLING-NOTES-PRESENT            VALUE '2'.
00251          88  CERT-BILLING-NOTES-PRESENT       VALUE '3'.
102109         88  CLAIM-NOTES-PRESENT              VALUE '4'.
102109         88  CLAIM-CERT-NOTES-PRESENT         VALUE '5'.
102109         88  CLAIM-BILLING-NOTES-PRESENT      VALUE '6'.
102109         88  CLAIM-CERT-BILL-NOTES-PRESENT    VALUE '7'.
00252      12  CM-COMP-EXCP-SW                   PIC X.
00253          88  COMPENSATION-SAME-AS-ACCT        VALUE ' '.
00254          88  THIS-CERT-HAS-ERCOMM-ENTRY       VALUE '1'.
00255      12  CM-INSURED-ADDRESS-SW             PIC X.
00256          88  INSURED-ADDR-NOT-PRESENT         VALUE ' '.
00257          88  INSURED-ADDR-PRESENT             VALUE '1'.
00258
011410*    12  CM-LF-CEDED-BENEFIT               PIC S9(7)V99   COMP-3.
011410     12  CM-LF-CLP                         PIC S9(5)V99   COMP-3.
011410     12  FILLER                            PIC X.
00260
011410*    12  CM-ISS-MICROFILM-NO               PIC S9(9)      COMP-3.
011410     12  CM-AH-CLP                         PIC S9(5)V99   COMP-3.
011410     12  FILLER                            PIC X.
072308*    12  CM-CAN-MICROFILM-NO               PIC S9(9)      COMP-3.
072308     12  CM-NH-INT-ON-REFS                 PIC S9(7)V99   COMP-3.
00263
00264      12  CM-CREDIT-INTERFACE-SW-1          PIC X.
00265          88  CERT-ADDED-BATCH                 VALUE ' '.
00266          88  CERT-ADDED-ONLINE                VALUE '1'.
00267          88  CERT-PEND-ISSUE-ERROR            VALUE '2'.
00268          88  CERT-PURGED-OFFLINE              VALUE '3'.
00269          88  CERT-PEND-ISSUE-RETURNED         VALUE '4'.
00270      12  CM-CREDIT-INTERFACE-SW-2          PIC X.
00271          88  CERT-AS-LOADED                   VALUE ' '.
00272          88  CERT-CANCELLED-ONLINE            VALUE '1'.
00273          88  CERT-CLAIM-ONLINE                VALUE '2'.
00274          88  CERT-CLAIM-CANCEL-ONLINE         VALUE '3'.
00275          88  CERT-PEND-CANCEL-ERROR           VALUE '4'.
00276          88  CERT-PEND-CANCEL-VOID            VALUE '5'.
00277          88  CERT-PEND-CAN-VOID-ERROR         VALUE '6'.
00278          88  CERT-PEND-CANCEL-RETURNED        VALUE '7'.
00279
00280      12  CM-ACCOUNT-COMM-PCTS.
00281          16  CM-LIFE-COMM-PCT              PIC SV9(5)    COMP-3.
00282          16  CM-AH-COMM-PCT                PIC SV9(5)    COMP-3.
00283
00284      12  CM-USER-FIELD                     PIC X.
040504     12  CM-ADDL-CLP                       PIC S9(5)V99  COMP-3.
061405     12  CM-CLP-STATE                      PIC XX.
061405     12  CM-USER-RESERVED                  PIC XXX.
00286 ******************************************************************
00206      EJECT
00207 *    COPY ELCMSTR.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCMSTR.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.012                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = CLAIM MASTER FILE                         *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 350  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELMSTR                         RKP=2,LEN=20   *
00013 *       ALTERNATE PATH1 = ELMSTR2 (BY NAME)       RKP=22,LEN=29  *
00014 *       ALTERNATE PATH2 = ELMSTR3 (BY SOC SEC NO) RKP=51,LEN=12  *
00015 *       ALTERNATE PATH3 = ELMSTR5 (BY CERT NO)    RKP=63,LEN=12  *
00016 *       ALTERNATE PATH4 = ELMSTR6 (BY CREDIT CARD NO)            *
00017 *                                                 RKP=75,LEN=21  *
00018 *                                                                *
00019 *   **** NOTE ****                                               *
00020 *             ANY CHANGES TO THIS COPYBOOK MUST ALSO BE          *
00021 *             IMPLEMENTED IN COPYBOOK ELCRETR (RETRIEVE MASTER)  *
00022 *                                                                *
00023 *   LOG = YES                                                    *
00024 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
120503******************************************************************
120503*                   C H A N G E   L O G
120503*
120503* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
120503*-----------------------------------------------------------------
120503*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
120503* EFFECTIVE    NUMBER
120503*-----------------------------------------------------------------
120503* 120503    2003080800002  SMVA  INITIAL SECURE PAY CHANGES
080307* 080307    2007032100001  PEMA  ADD TOTAL INTEREST PAID FIELD
00025 ******************************************************************
00026  01  CLAIM-MASTER.
00027      12  CL-RECORD-ID                PIC XX.
00028          88  VALID-CL-ID         VALUE 'CL'.
00029
00030      12  CL-CONTROL-PRIMARY.
00031          16  CL-COMPANY-CD           PIC X.
00032          16  CL-CARRIER              PIC X.
00033          16  CL-CLAIM-NO             PIC X(7).
00034          16  CL-CERT-NO.
00035              20  CL-CERT-PRIME       PIC X(10).
00036              20  CL-CERT-SFX         PIC X.
00037
00038      12  CL-CONTROL-BY-NAME.
00039          16  CL-COMPANY-CD-A1        PIC X.
00040          16  CL-INSURED-LAST-NAME    PIC X(15).
00041          16  CL-INSURED-NAME.
00042              20  CL-INSURED-1ST-NAME PIC X(12).
00043              20  CL-INSURED-MID-INIT PIC X.
00044
00045      12  CL-CONTROL-BY-SSN.
00046          16  CL-COMPANY-CD-A2        PIC X.
00047          16  CL-SOC-SEC-NO.
00048              20  CL-SSN-STATE        PIC XX.
00049              20  CL-SSN-ACCOUNT      PIC X(6).
00050              20  CL-SSN-LN3          PIC X(3).
00051
00052      12  CL-CONTROL-BY-CERT-NO.
00053          16  CL-COMPANY-CD-A4        PIC X.
00054          16  CL-CERT-NO-A4.
00055              20  CL-CERT-A4-PRIME    PIC X(10).
00056              20  CL-CERT-A4-SFX      PIC X.
00057
00058      12  CL-CONTROL-BY-CCN.
00059          16  CL-COMPANY-CD-A5        PIC X.
00060          16  CL-CCN-A5.
00061              20  CL-CCN.
00062                  24  CL-CCN-PREFIX-A5 PIC X(4).
00063                  24  CL-CCN-PRIME-A5 PIC X(12).
00064              20  CL-CCN-FILLER-A5    PIC X(4).
00065
00066      12  CL-INSURED-PROFILE-DATA.
00067          16  CL-INSURED-BIRTH-DT     PIC XX.
00068          16  CL-INSURED-SEX-CD       PIC X.
00069              88  INSURED-IS-MALE        VALUE 'M'.
00070              88  INSURED-IS-FEMALE      VALUE 'F'.
00071              88  INSURED-SEX-UNKNOWN    VALUE ' '.
00072          16  CL-INSURED-OCC-CD       PIC X(6).
00073          16  FILLER                  PIC X(5).
00074
00075      12  CL-PROCESSING-INFO.
00076          16  CL-PROCESSOR-ID         PIC X(4).
00077          16  CL-CLAIM-STATUS         PIC X.
00078              88  CLAIM-IS-OPEN          VALUE 'O'.
00079              88  CLAIM-IS-CLOSED        VALUE 'C'.
00080          16  CL-CLAIM-TYPE           PIC X.
00081 *            88  AH-CLAIM               VALUE 'A'.
00082 *            88  LIFE-CLAIM             VALUE 'L'.
00083 *            88  PROPERTY-CLAIM         VALUE 'P'.
00084 *            88  IUI-CLAIM              VALUE 'I'.
120503*            88  GAP-CLAIM              VALUE 'G'.
00085          16  CL-CLAIM-PREM-TYPE      PIC X.
00086              88  SINGLE-PREMIUM         VALUE '1'.
00087              88  O-B-COVERAGE           VALUE '2'.
00088              88  OPEN-END-COVERAGE      VALUE '3'.
00089          16  CL-INCURRED-DT          PIC XX.
00090          16  CL-REPORTED-DT          PIC XX.
00091          16  CL-FILE-ESTABLISH-DT    PIC XX.
00092          16  CL-EST-END-OF-DISAB-DT  PIC XX.
00093          16  CL-LAST-PMT-DT          PIC XX.
00094          16  CL-LAST-PMT-AMT         PIC S9(7)V99  COMP-3.
00095          16  CL-PAID-THRU-DT         PIC XX.
00096          16  CL-TOTAL-PAID-AMT       PIC S9(7)V99  COMP-3.
00097          16  CL-NO-OF-PMTS-MADE      PIC S9(3)     COMP-3.
00098          16  CL-NO-OF-DAYS-PAID      PIC S9(4)     COMP.
00099          16  CL-PMT-CALC-METHOD      PIC X.
00100              88  CL-360-DAY-YR          VALUE '1'.
00101              88  CL-365-DAY-YR          VALUE '2'.
00102              88  CL-FULL-MONTHS         VALUE '3'.
00103          16  CL-CAUSE-CD             PIC X(6).
00104
00105          16  CL-PRIME-CERT-NO.
00106              20  CL-PRIME-CERT-PRIME PIC X(10).
00107              20  CL-PRIME-CERT-SFX   PIC X.
00108
00109          16  CL-SYSTEM-IDENTIFIER    PIC XX.
00110              88  CL-CREDIT-CLAIM        VALUE 'CR'.
00111              88  CL-CONVENIENCE-CLAIM   VALUE 'CV'.
00112
00113          16  CL-MICROFILM-NO         PIC X(10).
00114          16  CL-PROG-FORM-TYPE       PIC X.
00115          16  CL-LAST-ADD-ON-DT       PIC XX.
00116
00117          16  CL-LAST-REOPEN-DT       PIC XX.
00118          16  CL-LAST-CLOSE-DT        PIC XX.
00119          16  CL-LAST-CLOSE-REASON    PIC X(01).
00120              88  FINAL-PAID             VALUE '1'.
00121              88  CLAIM-DENIED           VALUE '2'.
00122              88  AUTO-CLOSE             VALUE '3'.
00123              88  MANUAL-CLOSE           VALUE '4'.
00124              88  BENEFITS-CHANGED       VALUE 'C'.
00125              88  SETUP-ERRORS           VALUE 'E'.
00126          16  CL-ASSOC-CERT-SEQU      PIC S99.
00127          16  CL-ASSOC-CERT-TOTAL     PIC S99.
00128          16  CL-CLAIM-PAYMENT-STATUS PIC 9.
00129              88  PAYMENT-IN-PREP        VALUE 1 THRU 9.
080307         16  CL-TOTAL-INT-PAID       PIC S9(5)V99 COMP-3.
080307         16  FILLER                  PIC X.
00131
00132      12  CL-CERTIFICATE-DATA.
00133          16  CL-CERT-ORIGIN          PIC X.
00134              88  CERT-WAS-ONLINE        VALUE '1'.
00135              88  CERT-WAS-CREATED       VALUE '2'.
00136              88  COVERAGE-WAS-ADDED     VALUE '3'.
00137          16  CL-CERT-KEY-DATA.
00138              20  CL-CERT-CARRIER     PIC X.
00139              20  CL-CERT-GROUPING    PIC X(6).
00140              20  CL-CERT-STATE       PIC XX.
00141              20  CL-CERT-ACCOUNT.
00142                  24  CL-CERT-ACCOUNT-PREFIX PIC X(4).
00143                  24  CL-CERT-ACCOUNT-PRIME  PIC X(6).
00144              20  CL-CERT-EFF-DT      PIC XX.
00145
00146      12  CL-STATUS-CONTROLS.
00147          16  CL-PRIORITY-CD          PIC X.
00148              88  CONFIDENTIAL-DATA      VALUE '8'.
00149              88  HIGHEST-PRIORITY       VALUE '9'.
00150          16  CL-SUPV-ATTN-CD         PIC X.
00151              88  SUPV-NOT-REQUIRED      VALUE ' ' 'N'.
00152              88  SUPV-IS-REQUIRED       VALUE 'Y'.
00153          16  CL-PURGED-DT            PIC XX.
00154          16  CL-RESTORED-DT          PIC XX.
00155          16  CL-NEXT-AUTO-PAY-DT     PIC XX.
00156          16  CL-NEXT-RESEND-DT       PIC XX.
00157          16  CL-NEXT-FOLLOWUP-DT     PIC XX.
00158          16  FILLER                  PIC XX.
00159          16  CL-LAST-MAINT-DT        PIC XX.
00160          16  CL-LAST-MAINT-USER      PIC X(4).
00161          16  CL-LAST-MAINT-HHMMSS    PIC S9(6)     COMP-3.
00162          16  CL-LAST-MAINT-TYPE      PIC X.
00163              88  CLAIM-SET-UP           VALUE ' '.
00164              88  PAYMENT-MADE           VALUE '1'.
00165              88  LETTER-SENT            VALUE '2'.
00166              88  MASTER-WAS-ALTERED     VALUE '3'.
00167              88  MASTER-WAS-RESTORED    VALUE '4'.
00168              88  INCURRED-DATE-CHANGED  VALUE '5'.
00169              88  FILE-CONVERTED         VALUE '6'.
00170              88  CHANGE-OF-BENEFITS     VALUE 'C'.
00171              88  ERROR-CORRECTION       VALUE 'E'.
00172          16  CL-RELATED-CLAIM-NO     PIC X(7).
00173          16  CL-HISTORY-ARCHIVE-DT   PIC XX.
00174          16  CL-BENEFICIARY          PIC X(10).
00175          16  CL-FILE-ESTABLISHED-BY  PIC X(4).
120808         16  CL-DENIAL-TYPE          PIC X.
                   88  CL-TYPE-DENIAL          VALUE '1'.
                   88  CL-TYPE-RESCISSION      VALUE '2'.
                   88  CL-TYPE-REFORMATION     VALUE '3'.
                   88  CL-TYPE-REF-TO-RES      VALUE '4'.
                   88  CL-TYPE-RECONSIDERED    VALUE '5'.
120808         16  FILLER                  PIC X(5).
00177
00178      12  CL-TRAILER-CONTROLS.
00179          16  CL-TRAILER-SEQ-CNT      PIC S9(4)     COMP.
00180              88  CL-1ST-TRL-AVAIL       VALUE +4095.
00181              88  CL-LAST-TRL-AVAIL      VALUE +100.
00182              88  CL-RESV-EXP-HIST-TRLR  VALUE +0.
00183          16  CL-LAST-INC-DT-CHANGE   PIC S9(4)     COMP.
00184          16  FILLER                  PIC XX.
00185          16  CL-AUTO-PAY-SEQ         PIC S9(4)     COMP.
00186          16  CL-ADDRESS-TRAILER-CNT.
00187              20  CL-INSURED-ADDR-CNT  PIC S9(1).
00188                  88  NO-INSURED-AVAILABLE    VALUE ZERO.
00189              20  CL-ACCOUNT-ADDR-CNT  PIC S9(1).
00190                  88  ACCOUNT-IS-ONLINE       VALUE ZERO.
00191              20  CL-BENIF-ADDR-CNT    PIC S9(1).
00192                  88  BENEFICIARY-IS-ONLINE   VALUE ZERO.
00193              20  CL-EMPLOYER-ADDR-CNT PIC S9(1).
00194                  88  NO-EMPLOY-AVAILABLE     VALUE ZERO.
00195              20  CL-DOCTOR-ADDR-CNT   PIC S9(1).
00196                  88  NO-DOCTOR-AVAILABLE     VALUE ZERO.
00197              20  CL-OTHER-1-ADDR-CNT  PIC S9(1).
00198                  88  NO-OTHER-1-ADDRESSES    VALUE ZERO.
00199              20  CL-OTHER-2-ADDR-CNT  PIC S9(1).
00200                  88  NO-OTHER-2-ADDRESSES    VALUE ZERO.
00201
00202      12  CL-CV-REFERENCE-NO.
00203          16  CL-CV-REFNO-PRIME       PIC X(18).
00204          16  CL-CV-REFNO-SFX         PIC XX.
00205
00206      12  CL-FILE-LOCATION            PIC X(4).
00207
00208      12  CL-PROCESS-ERRORS.
00209          16  CL-FATAL-ERROR-CNT      PIC S9(4)     COMP.
00210              88  NO-FATAL-ERRORS        VALUE ZERO.
00211          16  CL-FORCEABLE-ERROR-CNT  PIC S9(4)     COMP.
00212              88  NO-FORCABLE-ERRORS     VALUE ZERO.
00213
00214      12  CL-PRODUCT-CD               PIC X.
00215
00216      12  CL-CURRENT-KEY-DATA.
00217          16  CL-CURRENT-CARRIER      PIC X.
00218          16  CL-CURRENT-GROUPING     PIC X(6).
00219          16  CL-CURRENT-STATE        PIC XX.
00220          16  CL-CURRENT-ACCOUNT      PIC X(10).
00221
00222      12  CL-ASSOCIATES               PIC X.
00223          88  CL-ASSOC-NO-INTERFACE      VALUE 'A'.
00224          88  CL-ASSOC-INTERFACE         VALUE 'I'.
00225          88  CL-NON-ASSOC-NO-INTERFACE  VALUE 'N'.
00226          88  CL-NON-ASSOC-INTERFACE     VALUE 'M'.
00227
00228      12  CL-ACTIVITY-CODE            PIC 99.
00229      12  CL-ACTIVITY-MAINT-DT        PIC XX.
00230      12  CL-ACTIVITY-MAINT-TYPE      PIC X(4).
00231
00232      12  CL-LAPSE-REPORT-CODE        PIC 9.
00233      12  CL-LAG-REPORT-CODE          PIC 9.
00234      12  CL-LOAN-TYPE                PIC XX.
00235      12  CL-LEGAL-STATE              PIC XX.
00236
CIDMOD*    12  FILLER                      PIC X(5).
CIDMOD     12  CL-YESNOSW                  PIC X.
CIDMOD     12  FILLER                      PIC X(4).
00208      EJECT
00209 *    COPY ELCCNTL.
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
011410         16  FILLER                         PIC X(187).
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
00210      EJECT
00211 *    COPY ELCTRLR.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCTRLR.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.014                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = ACTIVITY TRAILER FILE                     *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 200    RECFORM = FIXED                         *
00011 *                                                                *
00012 *   BASE CLUSTER NAME = ELTRLR             RKP=2,LEN=22          *
00013 *       ALTERNATE INDEX = NONE                                   *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
120503******************************************************************
120503*                   C H A N G E   L O G
120503*
120503* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
120503*-----------------------------------------------------------------
120503*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
120503* EFFECTIVE    NUMBER
120503*-----------------------------------------------------------------
120503* 120503    2003080800002  SMVA  INITIAL SECURE PAY CHANGES
022106* 022106    2004040700004  PEMA  ADD LIFE CLAIM INTEREST
050506* 050506    2006030600001  AJRA  ADD DENIAL PROOF DATE
062806* 062806    2006030600001  AJRA  ADD PAYMENT PROOF DATE
080106* 080106    2006052500001  AJRA  ADD N AND R NOTE TYPES
041807* 041807    2006032200004  AJRA  ADD APPROVED BY TO PAYMENT
082807* 082807    2007032100001  PEMA  ADD INT RATE TO PMT TRLR
101807* 101807  IR2007100100007  PEMA  EXPAND SIZE OF CLM RESERVE FLDS
070909* 070909    2009060400001  AJRA  ADD AUTO PAY END LETTER
040110* 040110  CR2009070600002  AJRA  ADD RESEND LETTER ID TO LETTER
071910* 071910  CR2009122800001  PEMA  ADD EOB SWITCHES
102610* 102610    2009122800001  AJRA  ADD STOP DATE TO LETTER
00017 ******************************************************************
00018  01  ACTIVITY-TRAILERS.
00019      12  AT-RECORD-ID                    PIC XX.
00020          88  VALID-AT-ID                       VALUE 'AT'.
00021
00022      12  AT-CONTROL-PRIMARY.
00023          16  AT-COMPANY-CD               PIC X.
00024          16  AT-CARRIER                  PIC X.
00025          16  AT-CLAIM-NO                 PIC X(7).
00026          16  AT-CERT-NO.
00027              20  AT-CERT-PRIME           PIC X(10).
00028              20  AT-CERT-SFX             PIC X.
00029          16  AT-SEQUENCE-NO              PIC S9(4)     COMP.
00030              88  AT-1ST-TRL-AVAIL             VALUE +4095.
00031              88  AT-LAST-TRL-AVAIL            VALUE +100.
00032              88  AT-RESV-EXP-HIST-TRL         VALUE +0.
00033              88  AT-INSURED-ADDR-TRL          VALUE +1 THRU +9.
00034              88  AT-BENEFICIARY-ADDR-TRL      VALUE +11 THRU +19.
00035              88  AT-ACCOUNT-ADDR-TRL          VALUE +21 THRU +29.
00036              88  AT-PHYSICIAN-ADDR-TRL        VALUE +31 THRU +39.
00037              88  AT-EMPLOYERS-ADDR-TRL        VALUE +41 THRU +49.
00038              88  AT-OTHER-1-ADDR-TRL          VALUE +51 THRU +59.
00039              88  AT-OTHER-2-ADDR-TRL          VALUE +61 THRU +69.
00040              88  AT-DIAGNOSIS-TRL             VALUE +90.
022106             88  AT-BENEFICIARY-TRL           VALUE +91.
022106             88  AT-SPECIAL-REVIEW-TRL        VALUE +92.
00041
00042      12  AT-TRAILER-TYPE                 PIC X.
00043          88  RESERVE-EXPENSE-TR               VALUE '1'.
00044          88  PAYMENT-TR                       VALUE '2'.
00045          88  AUTO-PAY-TR                      VALUE '3'.
00046          88  CORRESPONDENCE-TR                VALUE '4'.
00047          88  ADDRESS-TR                       VALUE '5'.
00048          88  GENERAL-INFO-TR                  VALUE '6'.
00049          88  AUTO-PROMPT-TR                   VALUE '7'.
00050          88  DENIAL-TR                        VALUE '8'.
00051          88  INCURRED-CHG-TR                  VALUE '9'.
00052          88  FORM-CONTROL-TR                  VALUE 'A'.
00053
00054      12  AT-RECORDED-DT                  PIC XX.
00055      12  AT-RECORDED-BY                  PIC X(4).
00056      12  AT-LAST-MAINT-HHMMSS            PIC S9(6)     COMP-3.
00057
00058      12  AT-TRAILER-BODY                 PIC X(165).
00059
00060      12  AT-RESERVE-EXPENSE-TR  REDEFINES  AT-TRAILER-BODY.
00061          16  AT-RESERVE-CONTROLS.
00062              20  AT-MANUAL-SW            PIC X.
00063                  88  AT-MANUAL-RESERVES-USED VALUE '1'.
00064              20  AT-FUTURE-SW            PIC X.
00065                  88  AT-FUTURE-RESERVES-USED VALUE '1'.
00066              20  AT-PTC-SW               PIC X.
00067                  88  AT-PAY-TO-CURRENT-USED  VALUE '1'.
00068              20  AT-IBNR-SW              PIC X.
00069                  88  AT-IBNR-RESERVES-USED   VALUE '1'.
00070              20  AT-PTC-LF-SW            PIC X.
00071                  88  AT-LF-PTC-USED          VALUE '1'.
00072              20  AT-CDT-ACCESS-METHOD    PIC X.
00073                  88  AT-CDT-ROUND-NEAR       VALUE '1'.
00074                  88  AT-CDT-ROUND-HIGH       VALUE '2'.
00075                  88  AT-CDT-INTERPOLATED     VALUE '3'.
00076              20  AT-PERCENT-OF-CDT       PIC S9(3)V99    COMP-3.
00077          16  AT-LAST-COMPUTED-DT         PIC XX.
101807         16  AT-FUTURE-RESERVE           PIC S9(7)V99    COMP-3.
101807         16  AT-PAY-CURRENT-RESERVE      PIC S9(7)V99    COMP-3.
101807         16  AT-IBNR-RESERVE             PIC S9(7)V99    COMP-3.
101807         16  AT-INITIAL-MANUAL-RESERVE   PIC S9(7)V99    COMP-3.
101807         16  AT-CURRENT-MANUAL-RESERVE   PIC S9(7)V99    COMP-3.
101807         16  AT-ITD-ADDITIONAL-RESERVE   PIC S9(7)V99    COMP-3.
00084          16  AT-EXPENSE-CONTROLS.
00085              20  AT-EXPENSE-METHOD       PIC X.
00086                  88  NO-EXPENSE-CALCULATED    VALUE '1'.
00087                  88  FLAT-DOLLAR-PER-PMT      VALUE '2'.
00088                  88  PERCENT-OF-PMT           VALUE '3'.
00089                  88  DOLLAR-PER-OPEN-MONTH    VALUE '4'.
00090              20  AT-EXPENSE-PERCENT      PIC S9(3)V99    COMP-3.
00091              20  AT-EXPENSE-DOLLAR       PIC S9(3)V99    COMP-3.
00092          16  AT-ITD-PAID-EXPENSES        PIC S9(5)V99    COMP-3.
00093          16  AT-ITD-CHARGEABLE-EXPENSE   PIC S9(5)V99    COMP-3.
00094
00095          16  AT-ITD-LIFE-REFUNDS         PIC S9(5)V99    COMP-3.
00096          16  AT-ITD-AH-REFUNDS           PIC S9(5)V99    COMP-3.
00097
101807*        16  FILLER                      PIC X(53).
101807         16  FILLER                      PIC X(47).
00099
00100          16  AT-RESERVES-LAST-MAINT-DT   PIC XX.
00101          16  AT-RESERVES-LAST-UPDATED-BY PIC X(4).
00102
00103          16  AT-OPEN-CLOSE-HISTORY OCCURS 6 TIMES.
00104              20  AT-OPEN-CLOSE-DATE      PIC XX.
00105              20  AT-OPEN-CLOSE-TYPE      PIC X.
00106 *                    C = CLOSED
00107 *                    O = OPEN
00108              20  AT-OPEN-CLOSE-REASON    PIC X(5).
00109 *                   REASONS = ALTER, AUTO, FINAL, NEW, FORCE
00110
00111      12  AT-PAYMENT-TR  REDEFINES  AT-TRAILER-BODY.
00112          16  AT-PAYMENT-TYPE             PIC X.
00113              88  PARTIAL-PAYMENT                VALUE '1'.
00114              88  FINAL-PAYMENT                  VALUE '2'.
00115              88  LUMP-SUM-PAYMENT               VALUE '3'.
00116              88  ADDITIONAL-PAYMENT             VALUE '4'.
00117              88  CHARGEABLE-EXPENSE             VALUE '5'.
00118              88  NON-CHARGEABLE-EXPENSE         VALUE '6'.
00119              88  VOIDED-PAYMENT                 VALUE '9'.
00120              88  TRANSFER                       VALUE 'T'.
022106             88  LIFE-INTEREST                  VALUE 'I'.
00121
00122          16  AT-CLAIM-TYPE               PIC X.
00123              88  PAID-FOR-AH                    VALUE 'A'.
00124              88  PAID-FOR-LIFE                  VALUE 'L'.
00124              88  PAID-FOR-IUI                   VALUE 'I'.
120503             88  PAID-FOR-GAP                   VALUE 'G'.
00125          16  AT-CLAIM-PREM-TYPE          PIC X.
00126              88  AT-SINGLE-PREMIUM              VALUE '1'.
00127              88  AT-O-B-COVERAGE                VALUE '2'.
00128              88  AT-OPEN-END-COVERAGE           VALUE '3'.
00129          16  AT-AMOUNT-PAID              PIC S9(7)V99  COMP-3.
00130          16  AT-CHECK-NO                 PIC X(7).
00131          16  AT-PAID-FROM-DT             PIC XX.
00132          16  AT-PAID-THRU-DT             PIC XX.
00133          16  AT-DAYS-IN-PERIOD           PIC S9(4)     COMP.
00134          16  FILLER                      PIC X.
00135          16  AT-PAYEES-NAME              PIC X(30).
00136          16  AT-PAYMENT-ORIGIN           PIC X.
00137              88  ONLINE-MANUAL-PMT              VALUE '1'.
00138              88  ONLINE-AUTO-PMT                VALUE '2'.
00139              88  OFFLINE-PMT                    VALUE '3'.
00140          16  AT-CHECK-WRITTEN-DT         PIC XX.
00141          16  AT-TO-BE-WRITTEN-DT         PIC XX.
00142          16  AT-VOID-DATA.
00143              20  AT-VOID-DT              PIC XX.
041807*00144       20  AT-VOID-REASON          PIC X(30).
041807             20  AT-VOID-REASON          PIC X(26).
041807         16  AT-PMT-APPROVED-BY          PIC X(04).
00145          16  AT-ADDL-RESERVE             PIC S9(5)V99  COMP-3.
00146          16  AT-EXPENSE-PER-PMT          PIC S9(5)V99  COMP-3.
082807         16  AT-INT-RATE REDEFINES AT-EXPENSE-PER-PMT
082807                                         PIC S99V9(5)  COMP-3.
00147          16  AT-CREDIT-INTERFACE.
00148              20  AT-PMT-SELECT-DT        PIC XX.
00149                  88  PAYMENT-NOT-SELECTED  VALUE LOW-VALUE.
00150              20  AT-PMT-ACCEPT-DT        PIC XX.
00151                  88  PAYMENT-NOT-ACCEPTED  VALUE LOW-VALUE.
00152              20  AT-VOID-SELECT-DT       PIC XX.
00153                  88  VOID-NOT-SELECTED     VALUE LOW-VALUE.
00154              20  AT-VOID-ACCEPT-DT       PIC XX.
00155                  88  VOID-NOT-ACCEPTED     VALUE LOW-VALUE.
00156
00157          16  AT-CHECK-QUE-CONTROL        PIC S9(8)     COMP.
00158                  88  PAYMENT-NOT-QUEUED           VALUE ZERO.
00159                  88  CONVERSION-PAYMENT           VALUE +99999999.
00160          16  AT-CHECK-QUE-SEQUENCE       PIC S9(4)     COMP.
00161
00162          16  AT-FORCE-CONTROL            PIC X.
00163              88  PAYMENT-WAS-FORCED           VALUE '1'.
00164          16  AT-PREV-LAST-PMT-DT         PIC XX.
00165          16  AT-PREV-PAID-THRU-DT        PIC XX.
00166          16  AT-PREV-LAST-PMT-AMT        PIC S9(7)V99  COMP-3.
00167          16  AT-ELIMINATION-DAYS         PIC S999      COMP-3.
00168          16  AT-DAILY-RATE               PIC S9(3)V99  COMP-3.
00169          16  AT-BENEFIT-TYPE             PIC X.
00170
00171          16  AT-EXPENSE-TYPE             PIC X.
00172          16  AT-PAYMENT-APPROVAL-SW      PIC X.
00173
00174          16  AT-PAYEE-TYPE-CD.
00175              20  AT-PAYEE-TYPE           PIC X.
00176                  88  INSURED-PAID           VALUE 'I'.
00177                  88  BENEFICIARY-PAID       VALUE 'B'.
00178                  88  ACCOUNT-PAID           VALUE 'A'.
00179                  88  OTHER-1-PAID           VALUE 'O'.
00180                  88  OTHER-2-PAID           VALUE 'Q'.
00181                  88  DOCTOR-PAID            VALUE 'P'.
00182                  88  EMPLOYER-PAID          VALUE 'E'.
00183              20  AT-PAYEE-SEQ            PIC X.
00184
00185          16  AT-CASH-PAYMENT             PIC X.
00186          16  AT-GROUPED-PAYMENT          PIC X.
00187          16  AT-PAYMENT-NOTE-SEQ-NO      PIC S9(4)       COMP.
00188          16  AT-APPROVAL-LEVEL-REQD      PIC X.
00189          16  AT-APPROVED-LEVEL           PIC X.
00190          16  AT-VOID-TYPE                PIC X.
00191              88  AT-PAYMENT-WAS-STOPPED     VALUE 'S'.
00192              88  AT-PAYMENT-WAS-VOIDED      VALUE 'V'.
00193          16  AT-AIG-UNEMP-IND            PIC X.
00194              88  AT-AIG-UNEMPLOYMENT-PMT    VALUE 'U'.
00195          16  AT-ASSOCIATES               PIC X.
00196              88  AT-AIG-INTERFACE           VALUE 'I' 'N'.
00197              88  AT-AIG-NON-INTERFACE       VALUE 'A' 'M'.
00198
00199          16  AT-FORM-CTL-SEQ-NO          PIC S9(4)       COMP.
00200          16  AT-CV-PMT-CODE              PIC X.
00201              88  FULL-DEATH-PAYMENT         VALUE '1'.
00202              88  HALF-DEATH-PAYMENT         VALUE '2'.
00203              88  FULL-ADD-PAYMENT           VALUE '3'.
00204              88  HALF-ADD-PAYMENT           VALUE '4'.
00205              88  FULL-RIDER-PAYMENT         VALUE '5'.
00206              88  HALF-RIDER-PAYMENT         VALUE '6'.
00207              88  NON-CHG-EXP-PAYMENT        VALUE '7'.
00208              88  ADDL-PAYMENT               VALUE '8'.
00209
00210          16  AT-EOB-CODE1                PIC XXX.
00211          16  AT-EOB-CODE2                PIC XXX.
00212          16  AT-EOB-CODE3                PIC XXX.
00213          16  AT-EOB-CODE4                PIC XXX.
               16  FILLER REDEFINES AT-EOB-CODE4.
                   20  AT-INT-PMT-SELECT-DT    PIC XX.
                   20  FILLER                  PIC X.
00214          16  AT-EOB-CODE5                PIC XXX.
062806         16  FILLER REDEFINES AT-EOB-CODE5.
062806             20  AT-PMT-PROOF-DT         PIC XX.
062806             20  FILLER                  PIC X.
00215
071910         16  AT-PRINT-EOB-WITH-CHECK     PIC X.
071910             88  AT-PRINT-EOB            VALUE 'Y'.
00217
00218          16  AT-PAYMENT-LAST-MAINT-DT    PIC XX.
00219          16  AT-PAYMENT-LAST-UPDATED-BY  PIC X(4).
00220
00221      12  AT-AUTO-PAY-TR  REDEFINES  AT-TRAILER-BODY.
00222          16  AT-SCHEDULE-START-DT        PIC XX.
00223          16  AT-SCHEDULE-END-DT          PIC XX.
00224          16  AT-TERMINATED-DT            PIC XX.
00225          16  AT-LAST-PMT-TYPE            PIC X.
00226              88  LAST-PMT-IS-FINAL              VALUE 'F'.
00227              88  LAST-PMT-IS-PARTIAL            VALUE 'P'.
00228          16  AT-FIRST-PMT-DATA.
00229              20  AT-FIRST-PMT-AMT        PIC S9(7)V99  COMP-3.
00230              20  AT-DAYS-IN-1ST-PMT      PIC S9(4)     COMP.
00231              20  AT-1ST-PAY-THRU-DT      PIC XX.
00232          16  AT-REGULAR-PMT-DATA.
00233              20  AT-REGULAR-PMT-AMT      PIC S9(7)V99  COMP-3.
00234              20  AT-DAYS-IN-REG-PMT      PIC S9(4)     COMP.
00235              20  AT-INTERVAL-MONTHS      PIC S9(4)     COMP.
00236          16  AT-AUTO-PAYEE-CD.
00237              20  AT-AUTO-PAYEE-TYPE      PIC X.
00238                  88  INSURED-PAID-AUTO      VALUE 'I'.
00239                  88  BENEFICIARY-PAID-AUTO  VALUE 'B'.
00240                  88  ACCOUNT-PAID-AUTO      VALUE 'A'.
00241                  88  OTHER-1-PAID-AUTO      VALUE 'O'.
00242                  88  OTHER-2-PAID-AUTO      VALUE 'Q'.
00243              20  AT-AUTO-PAYEE-SEQ       PIC X.
00244          16  AT-AUTO-PAY-DAY             PIC 99.
00245          16  AT-AUTO-CASH                PIC X.
00246              88  AT-CASH                      VALUE 'Y'.
00247              88  AT-NON-CASH                  VALUE 'N'.
070909*        16  FILLER                      PIC X(129).
070909         16  AT-AUTO-END-LETTER          PIC X(4).
070909         16  FILLER                      PIC X(125).
00249
00250          16  AT-AUTO-PAY-LAST-MAINT-DT   PIC XX.
00251          16  AT-AUTO-PAY-LAST-UPDATED-BY PIC X(4).
00252
00253      12  AT-CORRESPONDENCE-TR  REDEFINES  AT-TRAILER-BODY.
00254          16  AT-LETTER-SENT-DT           PIC XX.
00255          16  AT-RECEIPT-FOLLOW-UP        PIC XX.
00256          16  AT-AUTO-RE-SEND-DT          PIC XX.
00257          16  AT-LETTER-ANSWERED-DT       PIC XX.
00258          16  AT-LETTER-ARCHIVE-NO        PIC S9(8)     COMP.
00259          16  AT-LETTER-ORIGIN            PIC X.
00260              88  ONLINE-CREATION              VALUE '1' '3'.
00261              88  OFFLINE-CREATION             VALUE '2' '4'.
                   88  NAPER-ONLINE-CREATION        VALUE '3'.
                   88  NAPER-OFFLINE-CREATION       VALUE '4'.
00262          16  AT-STD-LETTER-FORM          PIC X(4).
00263          16  AT-REASON-TEXT              PIC X(70).
00264          16  AT-ADDRESS-REC-SEQ-NO       PIC S9(4)     COMP.
00265          16  AT-ADDRESEE-TYPE            PIC X.
00266               88  INSURED-ADDRESEE            VALUE 'I'.
00267               88  BENEFICIARY-ADDRESEE        VALUE 'B'.
00268               88  ACCOUNT-ADDRESEE            VALUE 'A'.
00269               88  PHYSICIAN-ADDRESEE          VALUE 'P'.
00270               88  EMPLOYER-ADDRESEE           VALUE 'E'.
00271               88  OTHER-ADDRESEE-1            VALUE 'O'.
00272               88  OTHER-ADDRESEE-2            VALUE 'Q'.
00273          16  AT-ADDRESSEE-NAME           PIC X(30).
00274          16  AT-INITIAL-PRINT-DATE       PIC XX.
00275          16  AT-RESEND-PRINT-DATE        PIC XX.
00276          16  AT-CORR-SOL-UNSOL           PIC X.
00277          16  AT-LETTER-PURGED-DT         PIC XX.
CIDMOD*
CIDMOD*FOLLOWING CID CHGS REENTERED AS DMD CHGS OVERLAID THEM.
CIDMOD*
CIDMOD         16  AT-CSO-REDEFINITION.
040110             20  AT-RESEND-LETTER-FORM   PIC X(4).
040110             20  AT-AUTO-CLOSE-IND       PIC X(1).
040110             20  AT-LETTER-TO-BENE       PIC X(1).
102610             20  AT-STOP-LETTER-DT       PIC X(2).
102610             20  FILLER                  PIC X(19).
040110*             20  FILLER                  PIC X(27).
CIDMOD             20  AT-CSO-LETTER-STATUS    PIC X.
CIDMOD                 88  AT-CSO-LETTER-ONLINE    VALUE '1'.
CIDMOD                 88  AT-CSO-LETTER-PURGED    VALUE '2'.
CIDMOD                 88  AT-CSO-LETTER-RELOADED  VALUE '3'.
CIDMOD             20  AT-CSO-LETTER-PURGE-DATE   PIC XX.
CIDMOD             20  AT-CSO-LETTER-RELOAD-DATE  PIC XX.
CIDMOD*
CIDMOD*FOLLOWING DMD CHGS COMMENTED OUT AS THEY OVERLAY CID MODS NEEDED
CIDMOD*
CIDMOD*        16  FILLER                      PIC X(26).
CIDMOD*
CIDMOD*        16  AT-DMD-BSR-CODE             PIC X.
CIDMOD*            88  AT-AUTOMATED-BSR              VALUE 'A'.
CIDMOD*            88  AT-NON-AUTOMATED-BSR          VALUE 'B' ' '.
CIDMOD*
CIDMOD*        16  AT-DMD-LETTER-STATUS        PIC X.
CIDMOD*            88  AT-DMD-LETTER-ONLINE          VALUE '1'.
CIDMOD*            88  AT-DMD-LETTER-PURGED          VALUE '2'.
CIDMOD*            88  AT-DMD-LETTER-RELOADED        VALUE '3'.
CIDMOD*        16  AT-DMD-LETTER-PURGE-DT      PIC XX.
CIDMOD*        16  AT-DMD-LETTER-RELOAD-DT     PIC XX.
00290
00291          16  AT-CORR-LAST-MAINT-DT       PIC XX.
00292          16  AT-CORR-LAST-UPDATED-BY     PIC X(4).
00293
00294      12  AT-ADDRESS-TR  REDEFINES  AT-TRAILER-BODY.
00295          16  AT-ADDRESS-TYPE             PIC X.
00296              88  INSURED-ADDRESS               VALUE 'I'.
00297              88  BENEFICIARY-ADDRESS           VALUE 'B'.
00298              88  ACCOUNT-ADDRESS               VALUE 'A'.
00299              88  PHYSICIAN-ADDRESS             VALUE 'P'.
00300              88  EMPLOYER-ADDRESS              VALUE 'E'.
00301              88  OTHER-ADDRESS-1               VALUE 'O'.
00302              88  OTHER-ADDRESS-2               VALUE 'Q'.
00303          16  AT-MAIL-TO-NAME             PIC X(30).
00304          16  AT-ADDRESS-LINE-1           PIC X(30).
00305          16  AT-ADDRESS-LINE-2           PIC X(30).
00306          16  AT-CITY-STATE.
                   20  AT-CITY                 PIC X(28).
                   20  AT-STATE                PIC XX.
00307          16  AT-ZIP.
00308              20  AT-ZIP-CODE.
00309                  24  AT-ZIP-1ST          PIC X.
00310                      88  AT-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'.
00311                  24  FILLER              PIC X(4).
00312              20  AT-ZIP-PLUS4            PIC X(4).
00313          16  AT-CANADIAN-POSTAL-CODE  REDEFINES  AT-ZIP.
00314              20  AT-CAN-POSTAL-1         PIC XXX.
00315              20  AT-CAN-POSTAL-2         PIC XXX.
00316              20  FILLER                  PIC XXX.
00317          16  AT-PHONE-NO                 PIC 9(11)     COMP-3.
00318          16  FILLER                      PIC X(23).
00319          16  AT-ADDRESS-LAST-MAINT-DT    PIC XX.
00320          16  AT-ADDRESS-LAST-UPDATED-BY  PIC X(4).
00321
00322      12  AT-GENERAL-INFO-TR  REDEFINES  AT-TRAILER-BODY.
00323          16  AT-INFO-LINE-1              PIC X(60).
00324          16  AT-INFO-LINE-2              PIC X(60).
00325          16  AT-INFO-TRAILER-TYPE        PIC X.
00326              88  AT-PAYMENT-NOTE         VALUE 'P'.
00327              88  AT-CALL-NOTE            VALUE 'C'.
00328              88  AT-MAINT-NOTE           VALUE 'M'.
00329              88  AT-CERT-CHANGE          VALUE 'X'.
080106             88  AT-APPROVAL-NOTE        VALUE 'R'.
080106             88  AT-NOTE-FILE-NOTE       VALUE 'N'.
00330          16  AT-CALL-TYPE                PIC X.
00331              88  AT-PHONE-CALL-IN        VALUE 'I'.
00332              88  AT-PHONE-CALL-OUT       VALUE 'O'.
00333          16  AT-NOTE-CONTINUATION        PIC X.
00334              88  AT-CONTINUED-NOTE       VALUE 'X'.
071910         16  AT-EOB-CODES-EXIST          PIC X.
071910             88  AT-EOB-CODES-PRESENT    VALUE 'Y'.
00335          16  FILLER                      PIC X(35).
00336          16  AT-GEN-INFO-LAST-MAINT-DT   PIC XX.
00337          16  AT-GEN-INFO-LAST-UPDATED-BY PIC X(4).
00338
00339      12  AT-AUTO-PROMPT-TR  REDEFINES  AT-TRAILER-BODY.
00340          16  AT-PROMPT-LINE-1            PIC X(60).
00341          16  AT-PROMPT-LINE-2            PIC X(60).
00342          16  AT-PROMPT-START-DT          PIC XX.
00343          16  AT-PROMPT-END-DT            PIC XX.
00344          16  FILLER                      PIC X(35).
00345          16  AT-PROMPT-LAST-MAINT-DT     PIC XX.
00346          16  AT-PROMPT-LAST-UPDATED-BY   PIC X(4).
00347
00348      12  AT-DENIAL-INFO-TR  REDEFINES  AT-TRAILER-BODY.
00349          16  AT-DENIAL-INFO-1            PIC X(60).
00350          16  AT-DENIAL-INFO-2            PIC X(60).
00351          16  AT-DENIAL-DT                PIC XX.
00352          16  AT-RETRACTION-DT            PIC XX.
00353          16  AT-DENIAL-REASON-CODE       PIC X(4).
050506*         16  FILLER                      PIC X(31).
050506         16  AT-DENIAL-PROOF-DT          PIC XX.
050506         16  FILLER                      PIC X(29).
00355          16  AT-DENIAL-LAST-MAINT-DT     PIC XX.
00356          16  AT-DENIAL-LAST-UPDATED-BY   PIC X(4).
00357
00358      12  AT-INCURRED-CHG-TR  REDEFINES  AT-TRAILER-BODY.
00359          16  AT-OLD-INCURRED-DT          PIC XX.
00360          16  AT-OLD-REPORTED-DT          PIC XX.
00361          16  AT-OLD-ESTABLISHED-DT       PIC XX.
00362          16  AT-OLD-TOTAL-PAID           PIC S9(7)V99     COMP-3.
00363          16  AT-OLD-DAYS-PAID            PIC S9(4)        COMP.
00364          16  AT-OLD-NO-OF-PMTS           PIC S9(3)        COMP-3.
00365          16  AT-OLD-PAID-THRU-DT         PIC XX.
00366          16  AT-LAST-PMT-MADE-DT         PIC XX.
00367          16  FILLER                      PIC X(26).
00368          16  AT-OLD-DIAG-CODE            PIC X(6).
00369          16  AT-TRAILER-CNT-AT-CHG       PIC S9(4)        COMP.
00370          16  AT-OLD-ITD-PAID-EXPENSE     PIC S9(5)V99     COMP-3.
00371          16  AT-OLD-CHARGABLE-EXPENSE    PIC S9(5)V99     COMP-3.
00372          16  AT-OLD-INIT-MAN-RESV        PIC S9(7)V99     COMP-3.
00373          16  AT-OLD-CURRENT-MAN-RESV     PIC S9(7)V99     COMP-3.
00374          16  AT-OLD-ADDL-MAN-RESV        PIC S9(7)V99     COMP-3.
00375          16  AT-OLD-DIAG-DESCRIP         PIC X(60).
00376          16  FILLER                      PIC X(25).
00377          16  AT-INCURRED-LAST-UPDATED-BY PIC X(4).
00378
00379      12  AT-FORM-CONTROL-TR  REDEFINES  AT-TRAILER-BODY.
00380          16  AT-FORM-SEND-ON-DT          PIC XX.
00381          16  AT-FORM-FOLLOW-UP-DT        PIC XX.
00382          16  AT-FORM-RE-SEND-DT          PIC XX.
00383          16  AT-FORM-ANSWERED-DT         PIC XX.
00384          16  AT-FORM-PRINTED-DT          PIC XX.
00385          16  AT-FORM-REPRINT-DT          PIC XX.
00386          16  AT-FORM-TYPE                PIC X.
00387              88  INITIAL-FORM                  VALUE '1'.
00388              88  PROGRESS-FORM                 VALUE '2'.
00389          16  AT-INSTRUCT-LN-1            PIC X(28).
00390          16  AT-INSTRUCT-LN-2            PIC X(28).
00391          16  AT-INSTRUCT-LN-3            PIC X(28).
00392          16  AT-FORM-ADDR-SEQ-NO         PIC S9(4)      COMP.
00393          16  AT-FORM-ADDRESS             PIC X.
00394              88  FORM-TO-INSURED              VALUE 'I'.
00395              88  FORM-TO-ACCOUNT              VALUE 'A'.
00396              88  FORM-TO-OTHER-1              VALUE 'O'.
00397              88  FORM-TO-OTHER-2              VALUE 'Q'.
00398          16  AT-RELATED-1.
00399              20 AT-REL-CARR-1            PIC X.
00400              20 AT-REL-CLAIM-1           PIC X(7).
00401              20 AT-REL-CERT-1            PIC X(11).
00402          16  AT-RELATED-2.
00403              20 AT-REL-CARR-2            PIC X.
00404              20 AT-REL-CLAIM-2           PIC X(7).
00405              20 AT-REL-CERT-2            PIC X(11).
00406          16  AT-EMP-FORM-SEND-ON-DT      PIC XX.
00407          16  AT-PHY-FORM-SEND-ON-DT      PIC XX.
00408          16  AT-EMP-FORM-ANSWERED-DT     PIC XX.
00409          16  AT-PHY-FORM-ANSWERED-DT     PIC XX.
00410          16  AT-FORM-REM-PRINT-DT        PIC XX.
102610         16  AT-STOP-FORM-DT             PIC X(2).
00411
102610         16  FILLER                      PIC X(09).
00413          16  AT-FORM-LAST-MAINT-DT       PIC XX.
00414          16  AT-FORM-LAST-UPDATED-BY     PIC X(4).
00415 ******************************************************************
00212      EJECT
00213 *    COPY MPCPLCY.
00001 ******************************************************************
00002 *                                                                *
00003 *                           MPCPLCY                              *
00004 *                            VMOD=1.024                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = POLICY MASTER                             *
00007 *                                                                *
00008 *   FILE TYPE = VSAM,KSDS                                        *
00009 *   RECORD SIZE = 1200 RECFORM = FIXED                           *
00010 *                                                                *
00011 *   BASE CLUSTER = MPPLCY                         RKP=2,LEN=42   *
00012 *       ALTERNATE PATH2 = ** NOT USED **                         *
00013 *       ALTERNATE PATH3 = MPPLCY3 (BY INSD SS NO) RKP=44,LEN=16  *
00014 *       ALTERNATE PATH4 = MPPLCY4 (BY REF. NO.)   RKP=60,LEN=25  *
00015 *       ALTERNATE PATH5 = MPPLCY5 (BY ACCOUNT )   RKP=85,LEN=27  *
00016 *       ALTERNATE PATH6 = MPPLCY6 (BY TRANSIT )   RKP=112,LEN=15 *
00017 *       ALTERNATE PATH7 = MPPLCY7 (BY LOAN NO.)   RKP=127,LEN=27 *
00018 *                                                                *
00019 *   LOG = YES                                                    *
00020 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00021 ******************************************************************
00022 **WARNING*********************************************************
00023 **ANY CHANGES TO THIS COPY BOOK MAY NEED CORRESPONDING CHANGES****
00024 **TO THE FOLLOWING COPY BOOKS: MPCPOLUP                          *
00025 **                             MPCPHSTD                          *
00026 **                             MPCPHSTC                          *
00027 **                             MPCPHSTT                          *
00028 **                                                               *
00029 ******************************************************************
00030
00031  01  POLICY-MASTER.
00032      12  PM-RECORD-ID                      PIC XX.
00033          88  VALID-PM-ID                      VALUE 'PM'.
00034
00035 ******************************************************************
00036 *   BASE CLUSTER = MPPLCY         (BASE KEY)      RKP=2,LEN=42   *
00037 ******************************************************************
00038
00039      12  PM-CONTROL-PRIMARY.
00040          16  PM-PRODUCER-PRIMARY.
00041              20  PM-PROD-PRIMARY.
00042                  24  PM-COMPANY-CD         PIC X.
00043                  24  PM-CGSP-KEY.
00044                      28  PM-CARRIER        PIC X.
00045                      28  PM-GROUPING.
00046                          32  PM-GROUPING-PREFIX
00047                                            PIC X(3).
00048                          32  PM-GROUPING-PRIME
00049                                            PIC X(3).
00050                      28  PM-STATE          PIC X(2).
00051                      28  PM-PRODUCER.
00052                          32  PM-PRODUCER-PREFIX
00053                                            PIC X(4).
00054                          32  PM-PRODUCER-PRIME
00055                                            PIC X(6).
00056              20  PM-POLICY-EFF-DT              PIC XX.
00057          16  PM-REFERENCE-NUMBER.
00058              20  PM-REFNO-PRIME            PIC X(18).
00059              20  PM-REFNO-SFX              PIC XX.
00060
00061 ******************************************************************
00062 *       ALTERNATE PATH3 = MPPLCY3 (BY INSD SS NO) RKP=44,LEN=16  *
00063 ******************************************************************
00064
00065      12  PM-CONTROL-BY-SSN.
00066          16  PM-COMPANY-CD-A3              PIC X.
00067          16  PM-SOC-SEC-NO.
00068              20  PM-SSN-STATE              PIC XX.
00069              20  PM-SSN-PRODUCER           PIC X(6).
00070              20  PM-SSN-LN3.
00071                  25  PM-INSURED-INITIALS-A3.
00072                      30 PM-INSURED-INITIAL1-A3 PIC X.
00073                      30 PM-INSURED-INITIAL2-A3 PIC X.
00074                  25 PM-PART-LAST-NAME-A3         PIC X.
00075          16  PM-DATE-A3                     PIC XX.
00076          16  PM-TIME-A3                     PIC S9(04)   COMP.
00077
00078 ******************************************************************
00079 *       ALTERNATE PATH4 = MPPLCY4 (BY REFRENCE)   RKP=60,LEN=25  *
00080 ******************************************************************
00081
00082      12  PM-CONTROL-BY-POLICY-NO.
00083          16  PM-COMPANY-CD-A4              PIC X.
00084          16  PM-POLICY-NO-A4.
00085              20  PM-POLICY-PRIME-A4        PIC X(18).
00086              20  PM-POLICY-SFX-A4          PIC XX.
00087          16  PM-DATE-A4                    PIC XX.
00088          16  PM-TIME-A4                    PIC S9(04)   COMP.
00089
00090 ******************************************************************
00091 *       ALTERNATE PATH5 = MPPLCY5 (BY ACCOUNT NO) RKP=85,LEN=27  *
00092 ******************************************************************
00093
00094      12  PM-CONTROL-BY-ACCOUNT.
00095          16  PM-COMPANY-CD-A5              PIC X.
00096          16  PM-BANK-ACCOUNT-NUMBER        PIC X(20).
00097          16  PM-DATE-A5                    PIC XX.
00098          16  PM-TIME-A5                    PIC S9(07)   COMP.
00099
00100 ******************************************************************
00101 *       ALTERNATE PATH6 = MPPLCY6 (BY TRANSIT NO) RKP=112,LEN=15 *
00102 ******************************************************************
00103
00104      12  PM-CONTROL-BY-TRANSIT.
00105          16  PM-COMPANY-CD-A6              PIC X.
00106          16  PM-BANK-TRANSIT-NUMBER.
00107              20  PM-FEDERAL-NUMBER         PIC X(4).
00108              20  PM-BANK-NUMBER            PIC X(4).
00109          16  PM-DATE-A6                    PIC XX.
00110          16  PM-TIME-A6                    PIC S9(07)   COMP.
00111
00112 ******************************************************************
00113 *       ALTERNATE PATH7 = MPPLCY7 (BY LOAN NO)    RKP=127,LEN=27 *
00114 ******************************************************************
00115
00116      12  PM-CONTROL-BY-LOAN-NO.
00117          16  PM-COMPANY-CD-A7              PIC X.
00118          16  PM-LOAN-NUMBER                PIC X(20).
00119          16  PM-DATE-A7                    PIC XX.
00120          16  PM-TIME-A7                    PIC S9(07)   COMP.
00121
00122 ******************************************************************
00123 *                 FILE SYNCHRONIZATION DATA                      *
00124 ******************************************************************
00125
00126      12  FILLER                            PIC X(05).
00127      12  PM-FILE-SYNCH-DATA.
00128          16  PM-LAST-CHANGE-DT             PIC XX.
00129          16  PM-LAST-CHANGE-TIME           PIC S9(7)    COMP.
00130          16  PM-LAST-CHANGE-PROCESSOR      PIC X(4).
00131      12  FILLER                            PIC X(05).
00132
00133 ******************************************************************
00134 *                    INSUREDS PROFILE DATA                       *
00135 ******************************************************************
00136
00137      12  PM-INSURED-PROFILE-DATA.
00138          16  PM-INSURED-NAME.
00139              20  PM-INSURED-LAST-NAME     PIC X(15).
00140              20  PM-INSURED-FIRST-NAME.
00141                  24  PM-INSURED-1ST-INIT PIC X.
00142                  24  FILLER               PIC X(9).
00143              20  PM-INSURED-MIDDLE-INIT PIC X.
00144          16  PM-INSURED-ADDRESS.
00145              20  PM-ADDRESS-LINE-1         PIC X(30).
00146              20  PM-ADDRESS-LINE-2         PIC X(30).
00147              20  PM-CITY                   PIC X(25).
00148              20  PM-RESIDENT-STATE         PIC XX.
00149              20  PM-ZIP-CD.
00150                  24  PM-ZIP-FIRST-FIVE     PIC X(5).
00151                  24  PM-ZIP-PLUS-FOUR      PIC X(4).
00152          16  PM-INSURED-PERSONAL.
00153              20  PM-INSURED-OCC-CLASS      PIC X.
00154                  88  PM-PREFERRED            VALUE '1'.
00155                  88  PM-STANDARD             VALUE '2'.
00156                  88  PM-HAZARDOUS            VALUE '3'.
00157                  88  PM-VERY-HAZARDOUS       VALUE '4'.
00158                  88  PM-EXTREME-HAZARDOUS VALUE '5'.
00159                  88  PM-NOT-OCC              VALUE '6'.
00160                  88  PM-OCC-UNKNOWN          VALUE '9'.
00161              20  PM-INSURED-OCC-CD         PIC X(3).
00162              20  PM-INSURED-OCC-CD-NUM REDEFINES
00163                  PM-INSURED-OCC-CD         PIC 9(3).
00164              20  PM-INSURED-SEX            PIC X.
00165                  88  PM-INSURED-SEX-MALE      VALUE 'M'.
00166                  88  PM-INSURED-SEX-FEMALE VALUE 'F'.
00167              20  PM-INSURED-BIRTH-DT       PIC XX.
00168              20  PM-INSURED-ISSUE-AGE      PIC S9(3)     COMP-3.
00169              20  PM-INSURED-HEIGHT-FT      PIC S9(3)     COMP-3.
00170              20  PM-INSURED-HEIGHT-IN      PIC S9(3)     COMP-3.
00171              20  PM-INSURED-WEIGHT         PIC S9(3)     COMP-3.
00172              20  PM-INSURED-BIRTH-STATE PIC XX.
00173              20  PM-INSURED-PHONE-NO       PIC X(13).
00174              20  PM-INSURED-RATED-AGE      PIC S9(3)     COMP-3.
00175          16  PM-INS-LANGUAGE-IND           PIC X(01).
00176              88  PM-ENGLISH                           VALUE 'E'.
00177              88  PM-FRENCH                            VALUE 'F'.
00178              88  PM-SPANISH                           VALUE 'S'.
00179          16  PM-INSURED-TOT-BENEFIT        PIC S9(7)V99  COMP-3.
00180
00181          16  PM-INSURED-AGE-IND            PIC X(01).
00182              88  PM-INSURED-AGE-75-REACHED            VALUE 'Y'.
00183      12  FILLER                            PIC X(13).
00184
00185 ******************************************************************
00186 *                JOINT INSUREDS PROFILE DATA                     *
00187 ******************************************************************
00188
00189      12  PM-JOINT-PROFILE-DATA.
00190          16  PM-JOINT-NAME.
00191              20  PM-JOINT-LAST-NAME        PIC X(15).
00192              20  PM-JOINT-FIRST-NAME.
00193                  24  PM-JOINT-1ST-INIT     PIC X.
00194                  24  FILLER                PIC X(9).
00195              20  PM-JOINT-MIDDLE-INIT      PIC X.
00196          16  PM-JOINT-SOC-SEC-NO.
00197              20  PM-JT-SSN-STATE           PIC XX.
00198              20  PM-JT-SSN-PRODUCER        PIC X(6).
00199              20  PM-JT-SSN-LN3.
00200                  25  PM-JT-INSURED-INITIALS-A3.
00201                      30 PM-JT-INSURED-INITIAL1-A3 PIC X.
00202                      30 PM-JT-INSURED-INITIAL2-A3 PIC X.
00203                  25 PM-JT-PART-LAST-NAME-A3        PIC X.
00204          16  PM-JOINT-PERSONAL.
00205              20  PM-JOINT-OCC-CLASS        PIC X.
00206                  88 PM-JNT-PREFERRED          VALUE '1'.
00207                  88 PM-JNT-STANDARD           VALUE '2'.
00208                  88 PM-JNT-HAZARDOUS          VALUE '3'.
00209                  88 PM-JNT-VERY-HAZARDOUS     VALUE '4'.
00210                  88 PM-JNT-EXTREME-HAZARDOUS VALUE '5'.
00211                  88 PM-JNT-NOT-OCC            VALUE '6'.
00212                  88 PM-JNT-OCC-UNKNOWN        VALUE '9'.
00213              20  PM-JOINT-OCC-CD           PIC X(3).
00214              20  PM-JOINT-SEX              PIC X.
00215                  88  PM-JOINT-SEX-MALE        VALUE 'M'.
00216                  88  PM-JOINT-SEX-FEMALE      VALUE 'F'.
00217              20  PM-JOINT-BIRTH-DT         PIC XX.
00218              20  PM-JOINT-ISSUE-AGE        PIC S9(3)     COMP-3.
00219              20  PM-JOINT-HEIGHT-FT        PIC S9(3)     COMP-3.
00220              20  PM-JOINT-HEIGHT-IN        PIC S9(3)     COMP-3.
00221              20  PM-JOINT-WEIGHT           PIC S9(3)     COMP-3.
00222              20  PM-JOINT-BIRTH-STATE      PIC XX.
00223              20  PM-JOINT-RATED-AGE        PIC S9(3)     COMP-3.
00224          16  PM-JOINT-TOT-BENEFIT          PIC S9(7)V99  COMP-3.
00225          16  PM-JOINT-AGE-IND              PIC X(01).
00226              88  PM-JOINT-AGE-75-REACHED              VALUE 'Y'.
00227
00228      12  FILLER                            PIC X(12).
00229
00230 ******************************************************************
00231 *                  INSURANCE COVERAGE DATA                       *
00232 ******************************************************************
00233
00234      12  PM-INS-COVERAGE-DATA.
00235          16  PM-FREE-PERIOD                PIC S9(03)    COMP-3.
00236          16  PM-LOAN-TERM                  PIC S9(3)     COMP-3.
00237          16  PM-LOAN-APR                   PIC S9V9999   COMP-3.
00238          16  PM-LOAN-DT                    PIC XX.
00239          16  PM-LOAN-PYMT                  PIC S9(5)V99  COMP-3.
00240          16  PM-LOAN-BALC                  PIC S9(7)V99  COMP-3.
00241          16  PM-INS-BENEFIT-MONTHS         PIC S9(3)     COMP-3.
00242          16  PM-INS-MONTH-BENEFIT          PIC S9(7)V99  COMP-3.
00243          16  PM-INS-TOTAL-BENEFIT          PIC S9(7)V99  COMP-3.
00244          16  PM-INS-PLAN-TYPE              PIC X.
00245              88  PM-AH-MORT-PLAN              VALUE 'A'.
00246              88  PM-AD-D-MORT-PLAN            VALUE 'E'.
00247              88  PM-DISMEM-MORT-PLAN          VALUE 'D'.
00248              88  PM-LIFE-MORT-PLAN            VALUE 'L'.
00249          16  PM-INS-PLAN-CD                PIC XX.
00250          16  PM-INS-PLAN-REVISION          PIC X(3).
00251          16  PM-INS-POLICY-FORM            PIC X(12).
00252          16  PM-INS-MSTR-POLICY.
00253              20  PM-FREE-TYPE              PIC X(04).
00254              20  FILLER                    PIC X(08).
00255          16  PM-INS-MSTR-APP.
00256              20  FILLER                    PIC X(11).
00257              20  PM-INS-B-C-TYPE           PIC X(01).
00258          16  PM-INS-RATE-CD                PIC X(5).
00259          16  PM-INS-SEX-RATING             PIC X.
00260              88  PM-NOT-SEX-RATED              VALUE '1'.
00261              88  PM-SEX-RATED                  VALUE '2'.
00262          16  PM-INS-SUBSTANDARD-PCT        PIC S9V9999   COMP-3.
00263          16  PM-INS-SUBSTANDARD-TYPE       PIC X.
00264          16  PM-INS-TERMINATION-DT         PIC XX.
00265          16  PM-INS-MONTH-PREMIUM      PIC S9(5)V999999  COMP-3.
00266          16  PM-INS-CALC-MO-PREM       PIC S9(5)V999999  COMP-3.
00267          16  PM-REINSURANCE-TABLE          PIC X(3).
00268          16  PM-MORTALITY-CD               PIC X(4).
00269          16  PM-INS-TYPE                   PIC X.
00270              88  PM-INDIVIDUAL                VALUES ARE '1' 'I'.
00271              88  PM-GROUP                     VALUES ARE '2' 'G'.
00272          16  PM-LOAN-OFFICER               PIC X(5).
00273          16  PM-POLICY-FEE                 PIC S9(3)V99 COMP-3.
00274          16  PM-DEPENDENT-COUNT            PIC S99      COMP-3.
00275          16  PM-CWA-AMOUNT                 PIC S9(5)V99  COMP-3.
00276          16  PM-LAST-AUTO-RERATE-DT        PIC XX.
00277          16  PM-PREM-FINANCED-SW           PIC X.
00278              88  PM-PREM-FINANCED              VALUE 'Y'.
00279              88  PM-PREM-NOT-FINANCED          VALUE 'N'.
00280
00281          16  PM-INS-TERM-LETTER-IND        PIC X.
00282              88  PM-TERM-INITIALIZED           VALUE 'Y'.
00283          16  PM-INS-UNDERWRITER-MAX-BEN PIC S9(7)V99     COMP-3.
00284      12  FILLER                            PIC X(11).
00285
00286 ******************************************************************
00287 *                    POLICY BILLING DATA                         *
00288 ******************************************************************
00289
00290      12  PM-BILLING-DATA.
00291          16  PM-BILLING-MODE               PIC X(1).
00292              88  PM-ANNUAL                    VALUE '1'.
00293              88  PM-SEMI-ANNUAL               VALUE '2'.
00294              88  PM-QUARTERLY                 VALUE '3'.
00295              88  PM-MONTHLY                   VALUE '4'.
00296              88  PM-BI-MONTHLY                VALUE '5'.
00297              88  PM-SINGLE-PREM               VALUE '6'.
00298          16  PM-BILLING-SCHEDULE           PIC X(1).
00299          16  PM-BILLING-SW                 PIC X(1).
00300              88  PM-FIRST-BILLING             VALUE 'Y'.
00301              88  PM-PAID-IN-ADVANCE           VALUE 'A'.
00302              88  PM-POLICY-FEE-REFUNDED       VALUE 'F'.
00303          16  PM-BILLING-TYPE               PIC X(1).
00304              88  PM-LIST-BILL                 VALUE '1'.
00305              88  PM-TAPE-BILL                 VALUE '2'.
00306              88  PM-TAPE-LIST-BILL            VALUE '3'.
00307              88  PM-GROUP-BILL          VALUE ARE '1' '2' '3'.
00308              88  PM-DIRECT-BILL               VALUE '4'.
00309              88  PM-PAC-BILL            VALUE ARE '5' 'C' 'S'.
00310              88  PM-CHARGE-CARD-BILL          VALUE '6'.
00311              88  PM-INDIV-BILL
00312                                   VALUE ARE '4' '5' '6' 'C' 'S'.
00313              88  PM-GRP-PLCY-BILL             VALUE '7'.
00314              88  PM-GRP-PLCY-PAC              VALUE '8'.
00315              88  PM-GRP-PLCY-CR-CRD           VALUE '9'.
00316              88  PM-GRP-PLCY            VALUE ARE '7' '8' '9'.
00317              88  PM-GRP-PROD                  VALUE 'A'.
00318              88  PM-EFT-CHECKING              VALUE 'C'.
00319              88  PM-EFT-SAVINGS               VALUE 'S'.
00320          16  PM-PAYMENT-AMT                PIC S9(5)V99  COMP-3.
00321          16  PM-OVER-SHORT-AMT             PIC S9(5)V99  COMP-3.
00322          16  PM-LAST-BILL-DT               PIC XX.
00323          16  PM-LAST-BILL-AMT              PIC S9(5)V99  COMP-3.
00324          16  PM-BILL-TO-DT                 PIC XX.
00325          16  PM-LAST-PYMT-DT               PIC XX.
00326          16  PM-PAID-TO-DT                 PIC XX.
00327          16  PM-PYMT-INVOICE-NUMBER        PIC X(6).
00328          16  PM-MONTHS-PAID                PIC S9(3)     COMP-3.
00329          16  PM-TOTAL-PREM-RECVD           PIC S9(7)V99  COMP-3.
00330          16  PM-BILLING-GROUPING-CODE      PIC X(6).
00331          16  PM-CHARGE-CARD-EXP-DT         PIC X(2).
00332          16  PM-CHARGE-CARD-TYPE           PIC X(2).
00333              88  PM-VISA                      VALUE 'VI'.
00334              88  PM-MSTR-CARD                 VALUE 'MC'.
00335              88  PM-DINERS-CLUB               VALUE 'DN'.
00336              88  PM-DISCOVER                  VALUE 'DS'.
00337              88  PM-CARTE-BLANCHE             VALUE 'CB'.
00338              88  PM-AMERICAN-EXPRESS          VALUE 'AE'.
00339          16  PM-BILL-INVOICE-NUMBER        PIC X(6).
00340          16  PM-BILL-DAY                   PIC S99       COMP-3.
00341          16  PM-RES-PREM-TAX           PIC S9(3)V999999  COMP-3.
00342      12  FILLER                            PIC X(15).
00343
00344 ******************************************************************
00345 *                     CLAIM PAYMENT DATA                         *
00346 ******************************************************************
00347
00348      12  PM-CLAIM-PAYMENT-DATA.
00349          16  PM-CLAIM-BENEFICIARY-NAME     PIC X(25).
00350          16  PM-CLAIM-INTERFACE-SW         PIC X.
00351              88  PM-NO-CLAIM-ATTACHED         VALUE SPACE.
00352              88  PM-POLICY-AND-CLAIM-ONLINE VALUE '1'.
00353              88  PM-POLICY-CREATED-FOR-CLAIM VALUE '2'.
00354              88  PM-CLAIM-CLOSED              VALUE '3'.
00355              88  PM-ACTIVE-CLAIM              VALUE '1' '2'.
00356              88  PM-CLAIM-ATTACHED            VALUE '1' '2' '3'.
00357          16  PM-CLAIM-INCURRED-DT          PIC XX.
00358          16  PM-CLAIM-PAID-TO-DT           PIC XX.
00359          16  PM-CLAIM-PAYMENT-CNT          PIC S9(3)     COMP-3.
00360          16  PM-CLAIM-LAST-PAYMENT-AMT     PIC S9(7)V99  COMP-3.
00361          16  PM-CLAIM-EXPENSES-ITD         PIC S9(7)V99  COMP-3.
00362          16  PM-CLAIM-PAYMENTS-ITD         PIC S9(7)V99  COMP-3.
00363          16  PM-CLAIM-ACCUMULATOR          PIC S9(7)V99  COMP-3.
00364          16  PM-CLAIM-ATTACH-CNT           PIC S9(3)     COMP-3.
00365          16  PM-CLAIM-LIFE-ITD             PIC S9(7)V99  COMP-3.
00366          16  PM-CLAIM-AH-ITD               PIC S9(7)V99  COMP-3.
00367          16  PM-CLAIM-RIDER-ITD            PIC S9(7)V99  COMP-3.
00368
00369      12  FILLER                            PIC X(03).
00370
00371 ******************************************************************
00372 *                POLICY STATUS AND DISPOSITION                   *
00373 ******************************************************************
00374
00375      12  PM-STATUS-DISPOSITION-DATA.
00376          16  PM-ISSUE-EOM-DT               PIC XX.
00377          16  PM-REPLACEMENT-SWITCH         PIC X.
00378          16  PM-APPL-SIGN-DT               PIC XX.
00379          16  PM-UNDERWRITER                PIC X(3).
00380          16  PM-ENTRY-PROCESSOR            PIC X(4).
00381          16  PM-ENTRY-STATUS               PIC X.
00382              88  PM-NORMAL                    VALUE '1'.
00383              88  PM-TAKE-OVER                 VALUE '2'.
00384              88  PM-CONVERSION                VALUE '4'.
00385              88  PM-RE-ISSUE                  VALUE '5'.
00386              88  PM-REINSURANCE-ONLY          VALUE '9'.
00387          16  PM-ENTRY-DT                   PIC XX.
00388          16  PM-ENTRY-TIME                 PIC S9(7) COMP-3.
00389          16  PM-EXIT-DT                    PIC XX.
00390          16  PM-CURRENT-STATUS             PIC X.
00391              88  PM-LAPSE                     VALUE '0'.
00392              88  PM-ACTIVE                    VALUE '1'.
00393              88  PM-PENDING-ISSUE             VALUE '2'.
00394              88  PM-DECLINED                  VALUE '3'.
00395              88  PM-PENDING-CANCEL            VALUE '4'.
00396              88  PM-PENDING-ISSUE-ERROR       VALUE '5'.
00397              88  PM-CLAIM-APPLIED             VALUE '6'.
00398              88  PM-CANCEL                    VALUE '7'.
00399              88  PM-PENDING-UNWTR-REVW        VALUE '8'.
00400              88  PM-PENDING-CANCEL-ERROR      VALUE '9'.
00401              88  PM-CANCEL-TRANSFER           VALUE 'C'.
00402              88  PM-CLAIM-SETTLEMENT          VALUE 'F'.
00403              88  PM-TERMINATE                 VALUE 'T'.
00404 ** NOTE TYPE 1 IS ANYTHING THAT IS OR HAS BEEN ACTIVE.  TYPE 2 IS
00405 ** EVERYTHING ELSE.  IF YOU ADD A STATUS ADD THE VALUE TO ONE OF
00406 ** THESE GROUPS.
00407              88  PM-TYPE-STAT-1
00408                      VALUES ARE '0' '1' '4' '6' '7' '9'
00409                                 'C' 'F' 'T'.
00410              88  PM-TYPE-STAT-2
00411                      VALUES ARE '2' '3' '5' '8'.
00412              88  PM-BILLABLE-STATUS VALUES ARE '0' '1' '6'.
00413              88  PM-PENDING-STATUS
00414                                 VALUES ARE '2' '4' '5' '8' '9'.
00415              88  PM-PENDING-ISSUE-STATUS
00416                                 VALUES ARE '2' '5' '8'.
00417              88  PM-CANCEL-STATUS
00418                                 VALUES ARE '4' '7' '9' 'C'.
00419          16  PM-CANCEL-CAUSE-CD            PIC X(3).
00420          16  PM-CANCEL-DT                  PIC XX.
00421          16  PM-REFUND-AMT                 PIC S9(5)V99  COMP-3.
00422          16  PM-CALC-REFUND-AMT            PIC S9(5)V99  COMP-3.
00423          16  PM-DECLINE-CD                 PIC X(3).
00424          16  PM-DECLINE-DT                 PIC XX.
00425          16  PM-LAST-LAPSE-DT              PIC XX.
00426          16  PM-LAST-REINSTATE-DT          PIC XX.
00427          16  PM-SECURITY-ACCESS-CODE       PIC X.
00428          16  PM-PREV-CONTROL-PRIMARY.
00429              20  PM-PREV-COMPANY-CD             PIC X.
00430              20  PM-PREV-CARRIER                PIC X.
00431              20  PM-PREV-GROUPING.
00432                  24  PM-PREV-GROUPING-PREFIX PIC X(3).
00433                  24  PM-PREV-GROUPING-PRIME     PIC X(3).
00434              20  PM-PREV-STATE                  PIC XX.
00435              20  PM-PREV-PRODUCER.
00436                  24  PM-PREV-PRODUCER-PREFIX PIC X(4).
00437                  24  PM-PREV-PRODUCER-PRIME     PIC X(6).
00438              20  PM-PREV-POLICY-EFF-DT          PIC XX.
00439              20  PM-PREV-REFERENCE-NUMBER.
00440                  24  PM-PREV-REFNO-PRIME        PIC X(18).
00441                  24  PM-PREV-REFNO-SFX          PIC XX.
00442          16  PM-ACTION-DT                  PIC XX.
00443          16  PM-ACTION-CODE                PIC X(3).
00444          16  PM-ACTION-DT-2                PIC XX.
00445          16  PM-ACTION-CODE-2              PIC X(3).
00446          16  PM-ACTION-DT-3                PIC XX.
00447          16  PM-ACTION-CODE-3              PIC X(3).
00448          16  PM-ACTION-DT-4                PIC XX.
00449          16  PM-ACTION-CODE-4              PIC X(3).
00450          16  PM-ACTION-DT-5                PIC XX.
00451          16  PM-ACTION-CODE-5              PIC X(3).
00452
00453          16  PM-KEY-CHANGE                 PIC X.
00454                  88  PM-NO-KEY-CHG      VALUES ARE ' ' 'N'.
00455                  88  PM-KEY-CHG              VALUE 'Y'.
00456          16  PM-KEY-CHANGE-DT              PIC XX.
00457
00458          16  PM-RTI-INDICATOR              PIC X.
00459          16  PM-REASON-CODE                PIC X(3).
00460          16  PM-IN-OUT-PROCESSING-IND      PIC X(1).
00461              88  PM-IN-OUT-PROCESSING      VALUE 'Y'.
00462              88  PM-NOT-IN-OUT-PROCESSING  VALUE SPACES.
00463
00464      12  FILLER                            PIC X(12).
00465
00466 ******************************************************************
00467 *                 AGENT AND COMMISSION DATA                      *
00468 ******************************************************************
00469
00470      12  PM-COMMISSION-DATA.
00471          16  PM-REMIT-TO                   PIC S9(3) COMP-3.
00472          16  PM-COMM-CHANGE-SW             PIC X.
00473                  88  PM-COMMISSION-CHANGE     VALUE 'Y'.
00474          16  PM-AGENT-INFORMATION OCCURS     5 TIMES.
00475              20  PM-AGENT-NUMBER           PIC X(10).
00476              20  PM-AGENT-TYPE             PIC X.
00477                  88  PM-PRODUCER-LEVEL-AGENT
00478                                               VALUES ARE 'C' 'D'.
00479                  88  PM-AGENT-GROSS           VALUE 'C'.
00480                  88  PM-AGENT-REINS           VALUE 'R'.
00481                  88  PM-AGENT-GROSS-REINS     VALUE 'D'.
00482                  88  PM-OVERWRITE-GROSS       VALUE 'O'.
00483                  88  PM-OVERWRITE-GROSS-REINS VALUE 'P'.
00484                  88  PM-OVERWRITE-REINS       VALUE 'T'.
00485                  88  PM-REINS-ONLY            VALUE 'W'.
00486              20  PM-COMMISSION-BILL-PAID PIC X(1).
00487                  88  PM-GENERATE-BILL         VALUE 'B'.
00488                  88  PM-GENERATE-PAID         VALUE 'P'.
00489              20  PM-AGENT-COMP-1ST-YEAR PIC S99V999.
00490              20  PM-COMP-1ST-YEAR-TYPE     PIC X(1).
00491                  88  PM-COMP-1ST-YEAR-PERCENT VALUE '1'.
00492                  88  PM-COMP-1ST-YEAR-DOLLARS VALUE '2'.
00493                  88  PM-COMP-1ST-YEAR-NOT-USED VALUE '3'.
00494              20  PM-RENEWAL-DATA.
00495                  24  PM-AGENT-RENEWAL-DATA OCCURS 6 TIMES.
00496                      28  PM-RENEW-MONTHS     PIC S999    COMP-3.
00497                      28  PM-RENEW-COMMISSION
00498                                              PIC S99V999 COMP-3.
00499                      28  PM-RENEW-TYPE       PIC X(1).
00500                          88  PM-COMP-RENEW-PERCENT      VALUE '1'.
00501                          88  PM-COMP-RENEW-DOLLARS      VALUE '2'.
00502                          88  PM-COMP-RENEW-NOT-USED     VALUE '3'.
00503              20  PM-COMP-RECALC-FLAG       PIC X(1).
00504                  88  PM-BYPASS-RECALC         VALUE 'N'.
00505      12  FILLER                            PIC X(20).
00506 ******************************************************************
00507 *             CUSTOMER DATA                                      *
00508 ******************************************************************
00509      12  PM-CUSTOMER-ID                    PIC X(20).
00510 ******************************************************************
00511      12  FILLER                            PIC X(43).
00512 ******************************************************************
00214      EJECT
00215 *    COPY MPCPLAN.
00001 ******************************************************************
00002 *                                                                *
00003 *                            MPCPLAN                             *
00004 *                            VMOD=1.012                          *
00005 *                                                                *
00006 *   MORTGAGE SYSTEM PRODUCER PLAN MASTER FILE.                   *
00007 *                                                                *
00008 *   THIS COPYBOOK IS USED FOR THE ONLINE                         *
00009 *   PLAN CODE MASTER FILE.                                       *
00010 *                                                                *
00011 *   FILE DESCRIPTION = PRODUCER PLAN MASTER                      *
00012 *                                                                *
00013 *   FILE TYPE = VSAM,KSDS                                        *
00014 *   RECORD SIZE = 450  RECFORM = FIX                             *
00015 *                                                                *
00016 *   BASE CLUSTER NAME = MPPLAN                    RKP=2,LEN=25   *
00017 *       ALTERNATE PATH1 = MPPLAN2 (ALT GROUPING) RKP=47,LEN=25   *
00018 *                                                                *
00019 *   LOG = NO                                                     *
00020 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00021 *                                                                *
00022 *                                                                *
00023 ******************************************************************
00024
00025  01  PRODUCER-PLANS.
00026      12  PP-RECORD-ID                      PIC  X(02).
00027          88  VALID-PP-ID                      VALUE 'PP'.
00028
00029 ******************************************************************
00030 *   BASE CLUSTER NAME = MPPLAN                    RKP=2,LEN=25   *
00031 ******************************************************************
00032
00033      12  PP-CONTROL-PRIMARY.
00034          16  PP-PROD-PRIMARY.
00035              20  PP-COMPANY-CD             PIC  X(01).
00036              20  PP-CONTROL-A.
00037                  24  PP-CARRIER            PIC  X(01).
00038                  24  PP-GROUPING.
00039                      28  PP-GROUPING-PREFIX
00040                                            PIC  X(03).
00041                      28  PP-GROUPING-PRIME PIC  X(03).
00042                  24  PP-STATE              PIC  X(02).
00043                  24  PP-PRODUCER.
00044                      28  PP-PRODUCER-PREFIX
00045                                            PIC  X(04).
00046                      28  PP-PRODUCER-PRIME PIC  X(06).
00047          16  PP-PRODUCER-PLAN.
00048              20  PP-PLAN-CODE              PIC  X(02).
00049              20  PP-PLAN-REVISION          PIC  9(03).
00050      12  FILLER                            PIC  X(20).
00051
00052 ******************************************************************
00053 *      ALTERNATE PATH1 = MPPLAN2 (ALT GROUPING) RKP=47,LEN=25    *
00054 ******************************************************************
00055
00056      12  PP-CONTROL-BY-VAR-GRP.
00057          16  PP-COMPANY-CD-A1              PIC  X(01).
00058          16  PP-VG-CARRIER                 PIC  X(01).
00059          16  PP-VG-GROUPING                PIC  X(06).
00060          16  PP-VG-STATE                   PIC  X(02).
00061          16  PP-VG-PRODUCER                PIC  X(10).
00062          16  PP-VG-PLAN-CODE               PIC  X(02).
00063          16  PP-VG-PLAN-REVISION           PIC  X(03).
00064      12  FILLER                            PIC  X(20).
00065
00066 ******************************************************************
00067 *                PRODUCER SECURITY DATA                          *
00068 ******************************************************************
00069
00070      12  PP-SECURITY-ACCESS-CODE           PIC  X(01).
00071      12  PP-POLICY-CNT                     PIC S9(07)    COMP-3.
00072
00073 ******************************************************************
00074 *                FILE SYNCHRONIZATION DATA                       *
00075 ******************************************************************
00076
00077      12  PP-MAINT-INFORMATION.
00078          16  PP-LAST-MAINT-DATE            PIC  X(02).
00079          16  PP-LAST-MAINT-HHMMSS          PIC S9(07)    COMP-3.
00080          16  PP-LAST-MAINT-USER            PIC  X(04).
00081      12  FILLER                            PIC  X(10).
00082
00083 ******************************************************************
00084 *                   CRITICAL FILE DATES                          *
00085 ******************************************************************
00086
00087      12  PP-PLAN-DATES.
00088          16  PP-PLAN-EFFECT-DATE           PIC  X(02).
00089          16  PP-PLAN-EXPIRE-DATE           PIC  X(02).
00090
00091      12  FILLER                            PIC  X(10).
00092
00093 ******************************************************************
00094 *                GENERAL INFORMATION                             *
00095 ******************************************************************
00096
00097      12  PP-GENERAL-INFORMATION.
00098          16  PP-ALPHA-SEARCH-SW            PIC  X(01).
00099              88  PP-MIB-ALPHA-ALL              VALUE '1'.
00100              88  PP-MIB-ALPHA-NONE             VALUE '2'.
00101              88  PP-MIB-ALPHA-EXCEEDED         VALUE '3'.
00102              88  PP-CLIENT-ALPHA-ALL           VALUE 'A'.
00103              88  PP-CLIENT-ALPHA-NONE          VALUE 'B'.
00104              88  PP-CLIENT-ALPHA-EXCEEDED      VALUE 'C'.
00105              88  PP-BOTH-ALPHA-ALL             VALUE 'X'.
00106              88  PP-BOTH-ALPHA-NONE            VALUE 'Y'.
00107              88  PP-BOTH-ALPHA-EXCEEDED        VALUE 'Z'.
00108              88  PP-ALPHA-SEARCH-VALID    VALUES ARE '1' '2' '3'
00109                                                      'A' 'B' 'C'
00110                                                      'X' 'Y' 'Z'.
00111          16  PP-BENEFIT-TYPE               PIC  X(01).
00112              88  PP-BENEFIT-IS-LEVEL            VALUE '1'.
00113              88  PP-BENEFIT-REDUCES             VALUE '2'.
00114          16  PP-DAYS-TO-1ST-NOTICE         PIC  9(02).
00115          16  PP-DAYS-TO-2ND-NOTICE         PIC  9(02).
00116          16  PP-DAYS-TO-3RD-NOTICE         PIC  9(02).
00117          16  PP-DAYS-TO-4TH-NOTICE         PIC  9(02).
00118          16  PP-EFF-DT-RULE-SW             PIC  X(01).
00119              88  PP-EFF-DT-ENTER               VALUE 'E'.
00120              88  PP-EFF-DT-MONTH               VALUE 'M'.
00121              88  PP-EFF-DT-QTR                 VALUE 'Q'.
00122              88  PP-EFF-DT-SEMI                VALUE 'S'.
00123              88  PP-EFF-DT-ANN                 VALUE 'A'.
00124          16  PP-FREE-EXAM-DAYS             PIC S9(03)   COMP-3.
00125          16  PP-GRACE-PERIOD               PIC S9(03)   COMP-3.
00126          16  PP-HEALTH-QUESTIONS           PIC  9(01).
00127          16  PP-NUMBER-LAPSE-NOTICES       PIC S9(03)   COMP-3.
00128          16  PP-MIB-SEARCH-SW              PIC  X(01).
00129              88  PP-MIB-SEARCH-ALL             VALUE '1'.
00130              88  PP-MIB-SEARCH-NONE            VALUE '2'.
00131              88  PP-MIB-SEARCH-EXCEEDED        VALUE '3'.
00132              88  PP-MIB-SEARCH-VALID      VALUES ARE '1' '2' '3'.
00133          16  PP-PLAN-ABBREV                PIC  X(03).
00134          16  PP-PLAN-AGES.
00135              20  PP-MINIMUM-AGE            PIC S9(03)   COMP-3.
00136              20  PP-MAXIMUM-AGE            PIC S9(03)   COMP-3.
00137              20  PP-MAXIMUM-ATTAIN-AGE     PIC S9(03)   COMP-3.
00138          16  PP-PLAN-BENEFITS.
00139              20  PP-CLAIM-CAP              PIC S9(07)V99 COMP-3.
00140              20  PP-MINIMUM-BENEFIT        PIC S9(07)V99 COMP-3.
00141              20  PP-MAXIMUM-BENEFIT        PIC S9(07)V99 COMP-3.
00142              20  PP-MAXIMUM-MONTHLY-BENEFIT
00143                                            PIC S9(07)V99 COMP-3.
00144          16  PP-PLAN-DESCRIPTION           PIC  X(10).
00145          16  PP-POLICY-FEE                 PIC S9(03)V9(02)
00146                                                         COMP-3.
00147          16  PP-PLAN-IND-GRP               PIC  X(01).
00148          16  PP-PLAN-SNGL-JNT              PIC  X(01).
00149              88  PP-COMBINED-PLAN             VALUE 'C'.
00150              88  PP-JNT-PLAN                  VALUE 'J'.
00151              88  PP-SNGL-PLAN                 VALUE 'S'.
00152          16  PP-PLAN-TERMS.
00153              20  PP-MINIMUM-TERM           PIC S9(03)   COMP-3.
00154              20  PP-MAXIMUM-TERM           PIC S9(03)   COMP-3.
00155          16  PP-PLAN-TYPE                  PIC  X(01).
00156              88  PP-AH-MORT-PLAN              VALUE 'A'.
00157              88  PP-AD-D-MORT-PLAN            VALUE 'E'.
00158              88  PP-DISMEM-MORT-PLAN          VALUE 'D'.
00159              88  PP-LIFE-MORT-PLAN            VALUE 'L'.
00160          16  PP-PREMIUM-TOLERANCES.
00161              20  PP-PREM-TOLERANCE         PIC S9(03)   COMP-3.
00162              20  PP-PREM-TOLERANCE-PCT     PIC SV9(03)  COMP-3.
00163          16  PP-RATE-CODE                  PIC  X(05).
00164          16  PP-REOCCURRING-DISABILITY-PRD PIC S9(03)   COMP-3.
00165          16  PP-REPLACEMENT-LAW-SW         PIC  X(01).
00166              88  PP-NO-REPLACE                VALUE '1'.
00167              88  PP-REPLACE-APPLIES           VALUE '2'.
00168              88  PP-VALID-REPLACEMENT-LAW     VALUE '1' '2'.
00169          16  PP-RETRO-RETENTION            PIC S9V9(04) COMP-3.
00170          16  PP-RERATE-CNTL                PIC  X(01).
00171              88  PP-RERATE-WITH-ISSUE-AGE       VALUE '1'.
00172              88  PP-RERATE-WITH-CURRENT-AGE     VALUE '2'.
00173              88  PP-DO-NOT-RERATE               VALUE '3' ' '.
00174              88  PP-AUTO-RECALC                 VALUE '4'.
00175          16  PP-SEX-RATING                 PIC  X(01).
00176              88  PP-NOT-SEX-RATED             VALUE '1'.
00177              88  PP-SEX-RATED                 VALUE '2'.
00178          16  PP-SUBSTANDARD-DATA.
00179              20  PP-SUBSTANDARD-PERCENT    PIC S9(01)V9(04).
00180              20  PP-SUBSTANDARD-TYPE       PIC  X(01).
00181                  88  PP-PCT-OF-BENEFIT        VALUE '1'.
00182                  88  PP-PCT-OF-PREMIUM        VALUE '2'.
00183                  88  PP-NOT-APPLICABLE        VALUE '3'.
00184          16  PP-YEARS-TO-NEXT-RERATE       PIC  9(02).
00185          16  PP-DEPENDANT-COVERAGE         PIC  X(01).
00186              88  PP-DEP-COVERED               VALUE 'Y'.
00187              88  PP-DEP-NOT-COVERED           VALUE 'N' ' '.
00188          16  PP-REFUND-CALC                PIC  X(01).
00189              88  PP-RFND-MP-REFUND     VALUES ARE ' ' LOW-VALUES.
00190              88  PP-RFND-BY-R78               VALUE '1'.
00191              88  PP-RFND-BY-PRO-RATA          VALUE '2'.
00192              88  PP-RFND-AS-CALIF             VALUE '3'.
00193              88  PP-RFND-AS-TEXAS             VALUE '4'.
00194              88  PP-RFND-IS-NET-PAY           VALUE '5'.
00195              88  PP-RFND-ANTICIPATION         VALUE '6'.
00196              88  PP-RFND-MEAN                 VALUE '8'.
00197              88  PP-VALID-REFUND       VALUES ARE ' ' '1' '2' '3'
00198                                                   '4' '5' '6' '8'
00199                                                   LOW-VALUES.
00200          16  PP-ALT-RATE-CODE              PIC  X(05).
00201
00202      12  FILLER                            PIC  X(39).
00203
00204 ******************************************************************
00205 *                     PLAN FORMS AND LETTERS                     *
00206 ******************************************************************
00207
00208      12  PP-PLAN-MASTER-FORMS.
00209          16  PP-POLICY-FORM                PIC  X(12).
00210          16  PP-MASTER-APPLICATION         PIC  X(12).
00211          16  PP-MASTER-POLICY              PIC  X(12).
00212      12  PP-DELINQUENCY-NOTICE-FORMS.
00213          16  PP-1ST-NOTICE-FORM            PIC  X(04).
00214          16  PP-2ND-NOTICE-FORM            PIC  X(04).
00215          16  PP-3RD-NOTICE-FORM            PIC  X(04).
00216          16  PP-4TH-NOTICE-FORM            PIC  X(04).
00217      12  FILLER                            PIC  X(32).
00218      12  PP-TERMINATION-FORM               PIC  X(04).
00219      12  FILLER                            PIC  X(08).
00220      12  PP-ISSUE-LETTER                   PIC  X(04).
00221
00222      12  FILLER                            PIC  X(80).
00223 ******************************************************************
00216      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA
                                CERTIFICATE-MASTER CLAIM-MASTER
                                CONTROL-FILE ACTIVITY-TRAILERS
                                POLICY-MASTER PRODUCER-PLANS.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL1323' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00218
00219      MOVE EIBDATE               TO DC-JULIAN-YYDDD.
00220      MOVE '5'                   TO DC-OPTION-CODE.
00221      PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
00222      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.
00223      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.
00224
00225
00226      IF EIBCALEN NOT GREATER THAN ZERO
00227          MOVE UNACCESS-MSG       TO  LOGOFF-MSG
00228          GO TO 8300-SEND-TEXT.
00229
00230
00231      
      * EXEC CICS HANDLE CONDITION
00232 *        ERROR (9990-ERROR) END-EXEC.
      *    MOVE '"$.                   ! " #00007260' TO DFHEIV0
           MOVE X'22242E202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303037323630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00233
00234      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.
00235
00236      IF PI-RETURN-TO-PROGRAM NOT EQUAL WS-PROGRAM-ID
00237          MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-6
00238          MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-5
00239          MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-4
00240          MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-3
00241          MOVE PI-SAVED-PROGRAM-1   TO  PI-SAVED-PROGRAM-2
00242          MOVE PI-RETURN-TO-PROGRAM TO  PI-SAVED-PROGRAM-1
00243          MOVE PI-CALLING-PROGRAM   TO  PI-RETURN-TO-PROGRAM
00244          MOVE WS-PROGRAM-ID        TO  PI-CALLING-PROGRAM
00245        ELSE
00246          MOVE PI-RETURN-TO-PROGRAM TO  PI-CALLING-PROGRAM
00247          MOVE PI-SAVED-PROGRAM-1   TO  PI-RETURN-TO-PROGRAM
00248          MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-1
00249          MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-2
00250          MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-3
00251          MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-4
00252          MOVE PI-SAVED-PROGRAM-6   TO  PI-SAVED-PROGRAM-5
00253          MOVE SPACES               TO  PI-SAVED-PROGRAM-6.
00254
00255      MOVE PI-COMPANY-CD          TO  WS-CL-COMPANY-CD
00256      MOVE PI-CARRIER             TO  WS-CL-CARRIER
00257      MOVE PI-CLAIM-NO            TO  WS-CL-CLAIM-NO
00258      MOVE PI-CERT-NO             TO  WS-CL-CERT-NO
00259
00260      
      * EXEC CICS HANDLE CONDITION
00261 *        NOTFND (8100-CLAIM-NOT-FOUND)
00262 *    END-EXEC.
      *    MOVE '"$I                   ! # #00007289' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303037323839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00263
00264      MOVE LOW-VALUES             TO  EL132CI.
00265      MOVE SAVE-DATE              TO  CDATEO
00266      MOVE EIBTIME                TO  WS-TIME-WORK
00267      MOVE WS-TIME                TO  CTIMEO
00268      MOVE PI-COMPANY-ID          TO  CCOMPO.
101501     MOVE PI-PROCESSOR-ID        TO  CUSERIDO.
00269
00270      
      * EXEC CICS READ
00271 *        DATASET (WS-CLAIM-MASTER-DSID)
00272 *        RIDFLD  (WS-CLAIM-KEY)
00273 *        SET     (ADDRESS OF CLAIM-MASTER)
00274 *    END-EXEC.
      *    MOVE '&"S        E          (   #00007300' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037333030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CLAIM-MASTER-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CLAIM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00275
00276      MOVE LOW-VALUES             TO  EL132CI.
00277
00278      MOVE SAVE-DATE              TO  CDATEO
00279      MOVE EIBTIME                TO  WS-TIME-WORK
00280      MOVE WS-TIME                TO  CTIMEO
00281      MOVE PI-COMPANY-ID          TO  CCOMPO.
101501     MOVE PI-PROCESSOR-ID        TO  CUSERIDO.
00282
00283      MOVE CL-CLAIM-NO            TO  CCLAIMO
00284      MOVE CL-CLAIM-TYPE          TO  CTYPEO
00285      MOVE CL-CERT-NO             TO  CCERTNOO
00286      MOVE CL-CERT-SFX            TO  CCERTSXO
00287      MOVE CL-CARRIER             TO  CCARIERO
00288      MOVE CL-CLAIM-STATUS        TO  CSTATO
00289      MOVE CL-PROCESSOR-ID        TO  CPROCO
00290      MOVE CL-INSURED-LAST-NAME   TO  CLNAMEO
00291                                      WS-NAME-WORK
00292      MOVE CL-INSURED-1ST-NAME    TO  CFNAMEO
00293      MOVE CL-INSURED-MID-INIT    TO  CMNAMEO
00294
00295      MOVE CL-ASSOC-CERT-SEQU     TO  WS-CLM-POSITION.
00296      MOVE CL-ASSOC-CERT-TOTAL    TO  WS-CLM-TOTAL.
00297      MOVE WS-WORK-SEQU           TO  SEQUO.
00298
00299      IF CL-INSURED-BIRTH-DT NOT = LOW-VALUES
00300          MOVE SPACES                 TO  DC-OPTION-CODE
00301          MOVE CL-INSURED-BIRTH-DT    TO  DC-BIN-DATE-1
00302          PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
00303          MOVE DC-GREG-DATE-1-EDIT    TO  CBDATEO.
00304
00305      IF CL-SSN-STATE NOT = CL-CERT-STATE
00306        OR CL-SSN-ACCOUNT NOT = CL-CERT-ACCOUNT-PRIME
00307          MOVE CL-SOC-SEC-NO      TO  CSSNO.
00308
00309      MOVE CL-INSURED-OCC-CD      TO  COCCO
00310      MOVE CL-BENEFICIARY         TO  CBENEO
00311
00312
00313      MOVE CL-CAUSE-CD            TO  CCAUSCDO
00314
00315      IF CL-EST-END-OF-DISAB-DT NOT = LOW-VALUES
00316          MOVE CL-EST-END-OF-DISAB-DT TO  DC-BIN-DATE-1
00317          MOVE SPACES                 TO  DC-OPTION-CODE
00318          PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
00319          MOVE DC-GREG-DATE-1-EDIT    TO  CESTENDO.
00320
00321      IF CL-PAID-THRU-DT NOT = LOW-VALUES
00322         MOVE CL-PAID-THRU-DT        TO  DC-BIN-DATE-1
00323         MOVE SPACES                 TO  DC-OPTION-CODE
00324         PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
00325         MOVE DC-GREG-DATE-1-EDIT    TO  CPDTHRUO
00326         IF PI-USES-PAID-TO
00327            MOVE CL-PAID-THRU-DT        TO  DC-BIN-DATE-1
00328            MOVE '6'                    TO  DC-OPTION-CODE
00329            MOVE +1                     TO DC-ELAPSED-DAYS
00330            MOVE +0                     TO DC-ELAPSED-MONTHS
00331            PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
00332            MOVE DC-GREG-DATE-1-EDIT    TO  CPDTHRUO.
00333
00334      MOVE CL-TOTAL-PAID-AMT      TO  CTOTPDO
00335      MOVE CL-NO-OF-DAYS-PAID     TO  CNODAYSO
00336      MOVE CL-NO-OF-PMTS-MADE     TO  CNOPMTSO
00337      MOVE CL-PRIME-CERT-PRIME    TO  PCERTNOO.
00338      MOVE CL-PRIME-CERT-SFX      TO  PSUFXO.
00339
00340      IF CL-INCURRED-DT NOT = LOW-VALUES
00341          MOVE CL-INCURRED-DT         TO  DC-BIN-DATE-1
00342          MOVE SPACES                 TO  DC-OPTION-CODE
00343          PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
00344          MOVE DC-GREG-DATE-1-EDIT    TO  CINCREDO.
00345
00346      IF CL-REPORTED-DT NOT = LOW-VALUES
00347          MOVE CL-REPORTED-DT         TO  DC-BIN-DATE-1
00348          MOVE SPACES                 TO  DC-OPTION-CODE
00349          PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
00350          MOVE DC-GREG-DATE-1-EDIT    TO  CREPORTO.
00351
00352      IF CL-FILE-ESTABLISH-DT NOT = LOW-VALUES
00353          MOVE CL-FILE-ESTABLISH-DT   TO  DC-BIN-DATE-1
00354          MOVE SPACES                 TO  DC-OPTION-CODE
00355          PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
00356          MOVE DC-GREG-DATE-1-EDIT    TO  CESTABO.
00357
00358 *    IF CL-LAST-PMT-DT NOT = LOW-VALUES
00359 *        MOVE CL-LAST-PMT-DT         TO  DC-BIN-DATE-1
00360 *        MOVE SPACES                 TO  DC-OPTION-CODE
00361          PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
00362 *        MOVE DC-GREG-DATE-1-EDIT    TO  ELSTPMTO.
00363
00364 *    MOVE CL-LAST-PMT-AMT        TO  CPMTAMTO
00365
00366      IF CL-LAST-MAINT-DT NOT = LOW-VALUES
00367          MOVE CL-LAST-MAINT-DT       TO  DC-BIN-DATE-1
00368          MOVE SPACES                 TO  DC-OPTION-CODE
00369          PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
00370          MOVE DC-GREG-DATE-1-EDIT    TO  CMNTDTEO.
00371
00372      IF CL-LAST-MAINT-TYPE = SPACES
00373         MOVE 'SET UP'           TO  CMNTYPEO
00374      ELSE
00375      IF CL-LAST-MAINT-TYPE = '1'
00376         MOVE 'PMT   '           TO  CMNTYPEO
00377      ELSE
00378      IF CL-LAST-MAINT-TYPE = '2'
00379         MOVE 'LETTER'           TO  CMNTYPEO
00380      ELSE
00381      IF CL-LAST-MAINT-TYPE = '3'
00382         MOVE 'CHANGE'           TO  CMNTYPEO
00383      ELSE
00384      IF CL-LAST-MAINT-TYPE = '4'
00385         MOVE 'RESTOR'           TO  CMNTYPEO
00386      ELSE
00387      IF CL-LAST-MAINT-TYPE = '5'
00388         MOVE 'INC DT'           TO  CMNTYPEO
00389      ELSE
00390      IF CL-LAST-MAINT-TYPE = '6'
00391         MOVE 'CONV  '           TO  CMNTYPEO
00392      ELSE
00393         MOVE 'CONV  '           TO  CMNTYPEO.
00394
00395      MOVE CL-PRIORITY-CD         TO  CPRICDO
00396      MOVE CL-SUPV-ATTN-CD        TO  CSUPRO
00397      MOVE CL-FILE-LOCATION       TO  FILETOO.
00398      MOVE CL-INSURED-SEX-CD      TO  CSEXO.
00399
00400      MOVE CL-CONTROL-PRIMARY     TO  WS-ELTRLR-KEY.
00401      MOVE +90                    TO  WS-ELTRLR-SEQ-NO.
00402
00403      
      * EXEC CICS HANDLE CONDITION
00404 *        NOTFND (0010-READ-CERT-MASTER)
00405 *    END-EXEC.
      *    MOVE '"$I                   ! $ #00007434' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303037343334' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00406
00407      
      * EXEC CICS READ
00408 *        DATASET (WS-ACTIVITY-TRAILERS-DSID)
00409 *        RIDFLD  (WS-ELTRLR-KEY)
00410 *        SET     (ADDRESS OF ACTIVITY-TRAILERS)
00411 *    END-EXEC.
      *    MOVE '&"S        E          (   #00007438' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037343338' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00412
00413      IF AT-TRAILER-TYPE EQUAL '6'
00414         MOVE AT-INFO-LINE-1      TO  CCAUSEO.
00415
00416  0010-READ-CERT-MASTER.
00417
00418      IF CL-SYSTEM-IDENTIFIER IS EQUAL TO 'CV'
00419          GO TO 0100-READ-POLICY-MASTER.
00420
00421      MOVE SPACES                 TO  WS-CERTIFICATE-KEY
00422      MOVE CL-COMPANY-CD          TO  WS-CK-COMPANY-CD
00423      MOVE CL-CERT-CARRIER        TO  WS-CK-CARRIER
00424      MOVE CL-CERT-GROUPING       TO  WS-CK-GROUPING
00425      MOVE CL-CERT-STATE          TO  WS-CK-STATE
00426      MOVE CL-CERT-ACCOUNT        TO  WS-CK-ACCOUNT
00427      MOVE CL-CERT-NO             TO  WS-CK-CERT-NO
00428      MOVE CL-CERT-EFF-DT         TO  WS-CK-CERT-EFF-DT
00429
00430      
      * EXEC CICS HANDLE CONDITION
00431 *        NOTFND (8100-CERT-NOT-FOUND)
00432 *    END-EXEC.
      *    MOVE '"$I                   ! % #00007461' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303037343631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00433
00434      
      * EXEC CICS READ
00435 *        DATASET (WS-CERTIFICATE-MASTER-DSID)
00436 *        RIDFLD  (WS-CERTIFICATE-KEY)
00437 *        SET     (ADDRESS OF CERTIFICATE-MASTER)
00438 *    END-EXEC.
      *    MOVE '&"S        E          (   #00007465' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037343635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CERTIFICATE-MASTER-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CERTIFICATE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00439
00440      IF CM-CERT-EFF-DT NOT = LOW-VALUES
00441          MOVE CM-CERT-EFF-DT         TO  DC-BIN-DATE-1
00442          MOVE SPACES                 TO  DC-OPTION-CODE
00443          PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
00444          MOVE DC-GREG-DATE-1-EDIT    TO  CEFFDTO.
00445
00446      MOVE CM-ACCOUNT             TO  CACCNTO
00447      MOVE CM-STATE               TO  CSTATEO
00448      MOVE CM-CARRIER             TO  CCARRO
00449      MOVE CM-GROUPING            TO  CGRPO
00450      MOVE CM-INSURED-LAST-NAME   TO  CLNMEO
00451      MOVE CM-INSURED-FIRST-NAME  TO  CFNMEO
00452      MOVE CM-INSURED-INITIAL2    TO  CINITO
00453      MOVE CM-INSURED-ISSUE-AGE   TO  CISAGEO
00454      MOVE CM-INSURED-JOINT-AGE   TO  CJAGEO.
00455
00456      MOVE CM-JT-LAST-NAME        TO  CJLNMEO
00457      MOVE CM-JT-FIRST-NAME       TO  CJFNMEO
00458      MOVE CM-JT-INITIAL          TO  CJINITO
00459
00460      MOVE SAVE-BIN-DATE          TO WS-SAVE-CURRENT-DATE
00461
00462 *** READ STATE MASTER RECORD FOR FREE LOOK PERIOD ***
00463      MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.
00464      MOVE SPACES                 TO  WS-CFK-ACCESS-TYPE.
00465      MOVE WS-CK-STATE            TO  WS-CFK-STATE.
00466      MOVE '3'                    TO  WS-CFK-RECORD-TYPE.
00467      MOVE ZERO                   TO  WS-CFK-SEQUENCE-NO.
00468
00469      
      * EXEC CICS READ
00470 *        DATASET (WS-CONTROL-FILE-DSID)
00471 *        RIDFLD  (WS-CONTROL-FILE-KEY)
00472 *        SET     (ADDRESS OF CONTROL-FILE)
00473 *        RESP    (WS-RESPONSE)
00474 *    END-EXEC.
      *    MOVE '&"S        E          (  N#00007500' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303037353030' TO DFHEIV0(25:11)
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
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00475
00476      IF WS-RESP-NOTFND
00477         MOVE ER-2848            TO  EMI-ERROR
00478         GO TO 1000-SEND-AND-RETURN
00479      ELSE
00480         MOVE CF-ST-FREE-LOOK-PERIOD
00481                                  TO CP-FREE-LOOK.
00482
00483 *    IF CL-CLAIM-TYPE EQUAL PI-AH-OVERRIDE-L1
00484 *       GO TO 0020-CHECK-AH.
00485
00486      MOVE PI-LIFE-OVERRIDE-L6    TO  LCVDSCRO.
00487      MOVE '4'                    TO  WS-CFK-RECORD-TYPE
00488      MOVE CM-LF-BENEFIT-CD       TO  WS-BENEFIT-NO
00489                                      LCVCDO
00490      PERFORM 8700-LOCATE-BENEFIT THRU 8700-EXIT
00491      MOVE WS-KIND                TO  LCVKINDO
00492      MOVE CM-LF-ORIG-TERM        TO  LCVOTRMO
00493                                      CP-ORIGINAL-TERM.
00494      MOVE CM-CERT-EFF-DT         TO CP-CERT-EFF-DT
00495      MOVE CM-LOAN-1ST-PMT-DT     TO CP-FIRST-PAY-DATE
00496      MOVE WS-SAVE-CURRENT-DATE   TO CP-VALUATION-DT
00497      MOVE '4'                    TO CP-REM-TERM-METHOD
00498      MOVE PI-COMPANY-ID          TO CP-COMPANY-ID
00499      MOVE PI-REM-TRM-CALC-OPTION TO CP-REM-TRM-CALC-OPTION.
00500      PERFORM 9800-LINK-REM-TERM THRU 9800-EXIT
00501      MOVE CP-REMAINING-TERM-3    TO LCVRTRMO
00502
00503      MOVE CM-LF-BENEFIT-AMT      TO  LCVBENEO
00504      MOVE CM-POLICY-FORM-NO      TO  LCVFORMO
00505
00506      IF CM-LF-CURRENT-STATUS = '8'
00507         IF CM-LF-CANCEL-DT NOT = LOW-VALUES
00508            MOVE CM-LF-CANCEL-DT TO DC-BIN-DATE-1
00509            MOVE ' ' TO DC-OPTION-CODE
00510            PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
00511            IF NOT DATE-CONVERSION-ERROR
00512               MOVE DC-GREG-DATE-1-EDIT TO LCVCNDTO.
00513
00514      IF CM-LF-CURRENT-STATUS = '7'
00515         IF CM-LF-DEATH-DT NOT = LOW-VALUES
00516             MOVE CM-LF-DEATH-DT TO DC-BIN-DATE-1
00517             MOVE ' ' TO DC-OPTION-CODE
00518             PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
00519             IF NOT DATE-CONVERSION-ERROR
00520                 MOVE DC-GREG-DATE-1-EDIT TO LCVCNDTO.
00521
00522      IF CM-LF-CURRENT-STATUS EQUAL '1' OR '4'
00523         IF CP-REMAINING-TERM-3 EQUAL ZERO
00524            MOVE 'EXPIRED'        TO LCVSTATO
00525         ELSE
00526            MOVE 'ACTIVE'         TO LCVSTATO.
00527
00528      IF CM-LF-CURRENT-STATUS = '3'
00529         MOVE 'RESTORE'           TO LCVSTATO.
00530      IF CM-LF-CURRENT-STATUS = '5'
00531         MOVE 'REISSUE'           TO LCVSTATO.
122002     IF CM-LF-CURRENT-STATUS = 'M'
122002        MOVE 'MONTHLY'           TO LCVSTATO.
00532      IF CM-LF-CURRENT-STATUS = '6'
00533         MOVE 'LMP DIS'           TO LCVSTATO.
00534      IF CM-LF-CURRENT-STATUS = '7'
00535         MOVE 'DEATH  '           TO LCVSTATO.
00536      IF CM-LF-CURRENT-STATUS = '8'
00537         MOVE 'CANCEL '           TO LCVSTATO.
00538      IF CM-LF-CURRENT-STATUS = '9'
00539         MOVE 'RE-ONLY'           TO LCVSTATO.
00540
00541      IF CM-LF-CURRENT-STATUS = 'V'
00542         MOVE 'VOID   '           TO LCVSTATO.
00543      IF CM-LF-CURRENT-STATUS = 'D'
00544         MOVE 'DECLINE'           TO LCVSTATO.
00545
00546      IF CM-LF-CURRENT-STATUS EQUAL '7'
00547         MOVE CM-LF-DEATH-EXIT-DT       TO  DC-BIN-DATE-1
00548         PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
00549         MOVE DC-GREG-DATE-1-EDIT TO  LCVEXITO.
00550
00551      IF CM-LF-CURRENT-STATUS EQUAL '8'
00552         MOVE CM-LF-CANCEL-EXIT-DT       TO  DC-BIN-DATE-1
00553         PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
00554         MOVE DC-GREG-DATE-1-EDIT TO  LCVEXITO.
00555      EJECT
00556  0020-CHECK-AH.
00557
00558 *    IF CL-CLAIM-TYPE NOT EQUAL PI-AH-OVERRIDE-L1
00559 *       GO TO 0030-FINISH-CERT.
00560
00561      MOVE '5'                    TO  WS-CFK-RECORD-TYPE
00562      MOVE CM-AH-BENEFIT-CD       TO  WS-BENEFIT-NO
00563                                      ACVCDO
00564      PERFORM 8700-LOCATE-BENEFIT THRU 8700-EXIT
00565      MOVE WS-KIND                TO  ACVKINDO
00566      MOVE PI-AH-OVERRIDE-L6      TO  ACVDSCRO.
00567      MOVE CM-AH-ORIG-TERM        TO  ACVOTRMO
00568                                      CP-ORIGINAL-TERM.
00569      MOVE CM-CERT-EFF-DT         TO CP-CERT-EFF-DT
00570      MOVE CM-LOAN-1ST-PMT-DT     TO CP-FIRST-PAY-DATE
00571      MOVE WS-SAVE-CURRENT-DATE   TO CP-VALUATION-DT
00572      MOVE '4'                    TO CP-REM-TERM-METHOD
00573      MOVE PI-COMPANY-ID          TO CP-COMPANY-ID
00574      MOVE PI-REM-TRM-CALC-OPTION TO CP-REM-TRM-CALC-OPTION.
00575      PERFORM 9800-LINK-REM-TERM THRU 9800-EXIT
00576      MOVE CP-REMAINING-TERM-3    TO ACVRTRMO
00577
00578      MOVE CM-AH-BENEFIT-AMT      TO ACVBENEO
00579
00580      IF CM-AH-CURRENT-STATUS = '8'
00581         IF CM-AH-CANCEL-DT NOT = LOW-VALUES
00582             MOVE CM-AH-CANCEL-DT TO DC-BIN-DATE-1
00583             MOVE ' ' TO DC-OPTION-CODE
00584             PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
00585             IF NOT DATE-CONVERSION-ERROR
00586                 MOVE DC-GREG-DATE-1-EDIT TO ACVCNDTO.
00587
00588      IF CM-AH-CURRENT-STATUS = '8'
00589         IF CM-AH-CANCEL-EXIT-DT NOT = LOW-VALUES
00590             MOVE CM-AH-CANCEL-EXIT-DT TO DC-BIN-DATE-1
00591             MOVE ' ' TO DC-OPTION-CODE
00592             PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
00593             IF NOT DATE-CONVERSION-ERROR
00594                 MOVE DC-GREG-DATE-1-EDIT TO ACVEXITO.
00595
00596      IF CM-AH-CURRENT-STATUS = '6' OR '7'
00597         IF CM-AH-SETTLEMENT-DT NOT = LOW-VALUES
00598             MOVE CM-AH-SETTLEMENT-DT TO DC-BIN-DATE-1
00599             MOVE ' ' TO DC-OPTION-CODE
00600             PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
00601             IF NOT DATE-CONVERSION-ERROR
00602                 MOVE DC-GREG-DATE-1-EDIT TO ACVCNDTO.
00603
00604      IF CM-AH-CURRENT-STATUS = '6'
00605         IF CM-AH-SETTLEMENT-EXIT-DT NOT = LOW-VALUES
00606             MOVE CM-AH-SETTLEMENT-EXIT-DT TO DC-BIN-DATE-1
00607             MOVE ' ' TO DC-OPTION-CODE
00608             PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
00609             IF NOT DATE-CONVERSION-ERROR
00610                 MOVE DC-GREG-DATE-1-EDIT TO ACVEXITO.
00611
00612      IF CM-AH-CURRENT-STATUS = '1' OR = '4'
00613         MOVE 'ACTIVE'            TO ACVSTATO.
00614      IF CM-AH-CURRENT-STATUS = '3'
00615         MOVE 'RESTORE'           TO ACVSTATO.
00616      IF CM-AH-CURRENT-STATUS = '5'
00617         MOVE 'REISSUE'           TO ACVSTATO.
122002     IF CM-AH-CURRENT-STATUS = 'M'
122002        MOVE 'MONTHLY'           TO ACVSTATO.
00618      IF CM-AH-CURRENT-STATUS = '6'
00619         MOVE 'LMP DIS'           TO ACVSTATO.
00620      IF CM-AH-CURRENT-STATUS = '7'
00621         MOVE 'DEATH  '           TO ACVSTATO.
00622      IF CM-AH-CURRENT-STATUS = '8'
00623         MOVE 'CANCEL '           TO ACVSTATO.
00624      IF CM-AH-CURRENT-STATUS = '9'
00625         MOVE 'RE-ONLY'           TO ACVSTATO.
00626      IF CM-AH-CURRENT-STATUS = 'V'
00627         MOVE 'VOID   '           TO ACVSTATO.
00628      IF CM-AH-CURRENT-STATUS = 'D'
00629         MOVE 'DECLINE'           TO ACVSTATO.
00630
00631      MOVE CM-POLICY-FORM-NO      TO  ACVFORMO.
00632
00633  0030-FINISH-CERT.
00634
00635      MOVE CM-LOAN-NUMBER         TO  LOANNOO.
00636      MOVE CM-LOAN-BALANCE        TO  LOANBALO.
00637      MOVE CM-LOAN-APR            TO  CAPRO
00638      MOVE CM-PAY-FREQUENCY       TO  CPFREQO
00639      MOVE CM-IND-GRP-TYPE        TO  CINDGRPO
00640      IF CM-SING-PRM
00641         MOVE 'SP'                TO  CPREMTPO
00642      ELSE
00643      IF CM-O-B-COVERAGE
00644         MOVE 'OB'                TO  CPREMTPO
00645      ELSE
00646      IF CM-OPEN-END
00647         MOVE 'OE'                TO  CPREMTPO
00648      ELSE
00649         MOVE CM-PREMIUM-TYPE     TO  CPREMTPO.
00650
00651      MOVE CM-REIN-TABLE          TO  CREINCDO.
00652
00653      GO TO 1000-SEND-AND-RETURN.
00654
00655      EJECT
00656  0100-READ-POLICY-MASTER.
00657
00658      MOVE CL-COMPANY-CD          TO  EMPLCY-COMPANY-CD.
00659      MOVE CL-CERT-CARRIER        TO  EMPLCY-CARRIER.
00660      MOVE CL-CERT-GROUPING       TO  EMPLCY-GROUPING.
00661      MOVE CL-CERT-STATE          TO  EMPLCY-STATE.
00662      MOVE CL-CERT-ACCOUNT        TO  EMPLCY-PRODUCER.
00663      MOVE CL-CV-REFERENCE-NO     TO  EMPLCY-REFERENCE-NO.
00664      MOVE CL-CERT-EFF-DT         TO  EMPLCY-EFF-DT.
00665
00666      
      * EXEC CICS HANDLE CONDITION
00667 *        NOTFND   (8100-EMPLCY-NOT-FOUND)
00668 *    END-EXEC.
      *    MOVE '"$I                   ! & #00007701' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303037373031' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00669
00670      
      * EXEC CICS READ
00671 *        DATASET   (WS-EMPLCY-DSID)
00672 *        RIDFLD    (EMPLCY-KEY)
00673 *        SET       (ADDRESS OF POLICY-MASTER)
00674 *    END-EXEC.
      *    MOVE '&"S        E          (   #00007705' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037373035' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-EMPLCY-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 EMPLCY-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF POLICY-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00675
00676      IF (PM-POLICY-EFF-DT IS NOT EQUAL TO SPACES AND LOW-VALUES)
00677          MOVE PM-POLICY-EFF-DT           TO  DC-BIN-DATE-1
00678          MOVE ' '                        TO  DC-OPTION-CODE
00679          PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
00680          IF NO-CONVERSION-ERROR
00681              MOVE DC-GREG-DATE-1-EDIT    TO  CEFFDTO
00682          ELSE
00683              MOVE SPACES                 TO  CEFFDTO.
00684
00685      MOVE PM-PRODUCER                TO  CACCNTO.
00686      MOVE PM-STATE                   TO  CSTATEO.
00687      MOVE PM-CARRIER                 TO  CCARRO.
00688      MOVE PM-GROUPING                TO  CGRPO.
00689      MOVE PM-INSURED-LAST-NAME       TO  CLNMEO.
00690      MOVE PM-INSURED-FIRST-NAME      TO  CFNMEO.
00691      MOVE PM-INSURED-MIDDLE-INIT     TO  CINITO.
00692      MOVE PM-INSURED-ISSUE-AGE       TO  WS-AGE.
00693      MOVE WS-AGE-3-4                 TO  CISAGEO.
00694
00695      MOVE PM-JOINT-LAST-NAME         TO  CJLNMEO.
00696      MOVE PM-JOINT-FIRST-NAME        TO  CJFNMEO.
00697      MOVE PM-JOINT-MIDDLE-INIT       TO  CJINITO.
00698      MOVE PM-JOINT-ISSUE-AGE         TO  WS-AGE.
00699      MOVE WS-AGE-3-4                 TO  CJAGEO.
00700
00701  0100-READ-EMPLAN.
00702
00703      MOVE PM-COMPANY-CD              TO  EMPLAN-COMPANY-CD.
00704      MOVE PM-CARRIER                 TO  EMPLAN-CARRIER.
00705      MOVE PM-GROUPING                TO  EMPLAN-GROUPING.
00706      MOVE PM-STATE                   TO  EMPLAN-STATE.
00707      MOVE PM-PRODUCER                TO  EMPLAN-PRODUCER.
00708      MOVE PM-INS-PLAN-CD             TO  EMPLAN-PLAN-CODE.
00709      MOVE PM-INS-PLAN-REVISION       TO  EMPLAN-REV-NO.
00710
00711      
      * EXEC CICS HANDLE CONDITION
00712 *        NOTFND   (0100-READ-CONT)
00713 *    END-EXEC.
      *    MOVE '"$I                   ! '' #00007746' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303037373436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00714
00715      
      * EXEC CICS READ
00716 *        DATASET   (WS-EMPLAN-DSID)
00717 *        RIDFLD    (EMPLAN-KEY)
00718 *        SET       (ADDRESS OF PRODUCER-PLANS)
00719 *    END-EXEC.
      *    MOVE '&"S        E          (   #00007750' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037373530' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-EMPLAN-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 EMPLAN-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PRODUCER-PLANS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00720
00721      IF PP-BENEFIT-IS-LEVEL
00722          MOVE 'L'                    TO  CP-BENEFIT-TYPE
00723      ELSE
00724          MOVE 'R'                    TO  CP-BENEFIT-TYPE.
00725
00726  0100-READ-CONT.
00727
00728 *** READ STATE MASTER RECORD FOR FREE LOOK PERIOD ***
00729      MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.
00730      MOVE SPACES                 TO  WS-CFK-ACCESS-TYPE.
00731      MOVE PM-STATE               TO  WS-CFK-STATE.
00732      MOVE '3'                    TO  WS-CFK-RECORD-TYPE.
00733      MOVE ZERO                   TO  WS-CFK-SEQUENCE-NO.
00734
00735      
      * EXEC CICS READ
00736 *        DATASET (WS-CONTROL-FILE-DSID)
00737 *        RIDFLD  (WS-CONTROL-FILE-KEY)
00738 *        SET     (ADDRESS OF CONTROL-FILE)
00739 *        RESP    (WS-RESPONSE)
00740 *    END-EXEC.
      *    MOVE '&"S        E          (  N#00007770' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303037373730' TO DFHEIV0(25:11)
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
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00741
00742      IF WS-RESP-NOTFND
00743         MOVE ER-2848            TO  EMI-ERROR
00744         GO TO 1000-SEND-AND-RETURN
00745      ELSE
00746         MOVE CF-ST-FREE-LOOK-PERIOD
00747                                  TO CP-FREE-LOOK.
00748
00749      IF PM-INS-PLAN-TYPE IS EQUAL TO 'A'
00750          GO TO 0110-SHOW-AH.
00751
00752      MOVE PI-LIFE-OVERRIDE-L6        TO  LCVDSCRO.
00753      MOVE PP-PLAN-ABBREV             TO  LCVKINDO.
00754      MOVE PM-INS-PLAN-CD             TO  LCVCDO.
00755
00756      MOVE PP-REFUND-CALC             TO  CP-EARNING-METHOD
00757                                          CP-RATING-METHOD.
00758      MOVE PM-LOAN-TERM               TO  LCVOTRMO
00759                                          CP-ORIGINAL-TERM
00760                                          CP-LOAN-TERM.
00761      MOVE SAVE-BIN-DATE              TO  CP-VALUATION-DT.
00762      MOVE PM-POLICY-EFF-DT           TO  CP-CERT-EFF-DT
00763                                          CP-FIRST-PAY-DATE.
00764      MOVE PM-STATE                   TO  CP-STATE.
00765      MOVE 'A'                        TO  CP-SPECIAL-CALC-CD.
00766      MOVE '2'                        TO  CP-PROCESS-TYPE
00767                                          CP-REM-TERM-METHOD.
00768      MOVE '1'                        TO  CP-REM-TRM-CALC-OPTION.
00769      MOVE PI-COMPANY-ID              TO  CP-COMPANY-ID.
00770      MOVE PM-COMPANY-CD              TO  CP-COMPANY-CD.
00771
00772      PERFORM 9800-LINK-REM-TERM THRU 9800-EXIT.
00773
00774      IF (PI-COMPANY-ID IS EQUAL TO 'CIG' OR 'CUK')
00775          COMPUTE CP-REMAINING-TERM-3 = CP-REMAINING-TERM-3 + 1
00776          MOVE CP-REMAINING-TERM-3    TO  LCVRTRMO
00777      ELSE
00778          MOVE CP-REMAINING-TERM-3    TO  LCVRTRMO.
00779
00780      MOVE PM-INS-TOTAL-BENEFIT       TO  LCVBENEO.
00781      MOVE PM-INS-POLICY-FORM         TO  LCVFORMO.
00782
00783      IF PM-CANCEL-STATUS
00784          IF PM-CANCEL-DT IS NOT EQUAL TO LOW-VALUES
00785              MOVE PM-CANCEL-DT       TO  DC-BIN-DATE-1
00786              MOVE ' '                TO  DC-OPTION-CODE
00787              PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
00788              IF NOT DATE-CONVERSION-ERROR
00789                  MOVE DC-GREG-DATE-1-EDIT    TO  LCVCNDTO.
00790
00791      IF (PM-EXIT-DT IS NOT EQUAL TO LOW-VALUES AND SPACES)
00792          MOVE ' '                    TO  DC-OPTION-CODE
00793          MOVE PM-EXIT-DT             TO  DC-BIN-DATE-1
00794          PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
00795          IF NOT DATE-CONVERSION-ERROR
00796              MOVE DC-GREG-DATE-1-EDIT        TO  LCVEXITO.
00797
00798      IF PM-CURRENT-STATUS IS EQUAL TO '0'
00799          MOVE 'LAPSED'               TO  LCVSTATO.
00800      IF PM-CURRENT-STATUS IS EQUAL TO '1'
00801          MOVE 'ACTIVE'               TO  LCVSTATO.
00802      IF PM-CURRENT-STATUS IS EQUAL TO '2'
00803          MOVE 'PEND'                 TO  LCVSTATO.
00804      IF PM-CURRENT-STATUS IS EQUAL TO '3'
00805          MOVE 'DECLIN'               TO  LCVSTATO.
00806      IF (PM-CURRENT-STATUS IS EQUAL TO '4' OR '9')
00807          MOVE 'PNDCNC'               TO  LCVSTATO.
00808      IF PM-CURRENT-STATUS IS EQUAL TO '5'
00809          MOVE 'PNDISS'               TO  LCVSTATO.
00810      IF PM-CURRENT-STATUS IS EQUAL TO '6'
00811          MOVE 'CLAIM'                TO  LCVSTATO.
00812      IF PM-CURRENT-STATUS IS EQUAL TO '7'
00813          MOVE 'CANCEL'               TO  LCVSTATO.
00814      IF PM-CURRENT-STATUS IS EQUAL TO '8'
00815          MOVE 'PNDUNW'               TO  LCVSTATO.
00816      IF PM-CURRENT-STATUS IS EQUAL TO 'C'
00817          MOVE 'TRNSFR'               TO  LCVSTATO.
00818      IF PM-CURRENT-STATUS IS EQUAL TO 'F'
00819          MOVE 'SETTLE'               TO  LCVSTATO.
00820      IF PM-CURRENT-STATUS IS EQUAL TO 'T'
00821          MOVE 'TRMNAT'               TO  LCVSTATO.
00822
00823      GO TO 0120-FINISH-POLICY.
00824
00825  0110-SHOW-AH.
00826
00827      MOVE PI-AH-OVERRIDE-L6          TO  ACVDSCRO.
00828      MOVE PP-PLAN-ABBREV             TO  ACVKINDO.
00829      MOVE PM-INS-PLAN-CD             TO  ACVCDO.
00830
00831      MOVE PP-REFUND-CALC             TO  CP-EARNING-METHOD
00832                                          CP-RATING-METHOD.
00833      MOVE PM-LOAN-TERM               TO  ACVOTRMO
00834                                          CP-ORIGINAL-TERM
00835                                          CP-LOAN-TERM.
00836      MOVE SAVE-BIN-DATE              TO  CP-VALUATION-DT.
00837      MOVE PM-POLICY-EFF-DT           TO  CP-CERT-EFF-DT
00838      MOVE PM-LOAN-DT                 TO  CP-FIRST-PAY-DATE.
00839      MOVE PM-STATE                   TO  CP-STATE.
00840      MOVE 'A'                        TO  CP-SPECIAL-CALC-CD.
00841      MOVE '2'                        TO  CP-PROCESS-TYPE
00842      MOVE '3'                        TO  CP-REM-TERM-METHOD.
00843      MOVE '1'                        TO  CP-REM-TRM-CALC-OPTION.
00844      MOVE PI-COMPANY-ID              TO  CP-COMPANY-ID.
00845      MOVE PM-COMPANY-CD              TO  CP-COMPANY-CD.
00846
00847      PERFORM 9800-LINK-REM-TERM THRU 9800-EXIT.
00848
00849      MOVE CP-REMAINING-TERM-1        TO  ACVRTRMO.
00850
00851      MOVE PM-INS-MONTH-BENEFIT       TO  ACVBENEO.
00852      MOVE PM-INS-POLICY-FORM         TO  ACVFORMO.
00853
00854      IF PM-CANCEL-STATUS
00855          IF PM-CANCEL-DT IS NOT EQUAL TO LOW-VALUES
00856              MOVE PM-CANCEL-DT       TO  DC-BIN-DATE-1
00857              MOVE ' '                TO  DC-OPTION-CODE
00858              PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
00859              IF NOT DATE-CONVERSION-ERROR
00860                  MOVE DC-GREG-DATE-1-EDIT    TO  ACVCNDTO.
00861
00862      IF (PM-EXIT-DT IS NOT EQUAL TO LOW-VALUES AND SPACES)
00863          MOVE ' '                    TO  DC-OPTION-CODE
00864          MOVE PM-EXIT-DT             TO  DC-BIN-DATE-1
00865          PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
00866          IF NOT DATE-CONVERSION-ERROR
00867              MOVE DC-GREG-DATE-1-EDIT        TO  ACVEXITO.
00868
00869      IF PM-CURRENT-STATUS IS EQUAL TO '0'
00870          MOVE 'LAPSED'               TO  ACVSTATO.
00871      IF PM-CURRENT-STATUS IS EQUAL TO '1'
00872          MOVE 'ACTIVE'               TO  ACVSTATO.
00873      IF PM-CURRENT-STATUS IS EQUAL TO '2'
00874          MOVE 'PEND'                 TO  ACVSTATO.
00875      IF PM-CURRENT-STATUS IS EQUAL TO '3'
00876          MOVE 'DECLIN'               TO  ACVSTATO.
00877      IF (PM-CURRENT-STATUS IS EQUAL TO '4' OR '9')
00878          MOVE 'PNDCNC'               TO  ACVSTATO.
00879      IF PM-CURRENT-STATUS IS EQUAL TO '5'
00880          MOVE 'PNDISS'               TO  ACVSTATO.
00881      IF PM-CURRENT-STATUS IS EQUAL TO '6'
00882          MOVE 'CLAIM'                TO  ACVSTATO.
00883      IF PM-CURRENT-STATUS IS EQUAL TO '7'
00884          MOVE 'CANCEL'               TO  ACVSTATO.
00885      IF PM-CURRENT-STATUS IS EQUAL TO '8'
00886          MOVE 'PNDUNW'               TO  ACVSTATO.
00887      IF PM-CURRENT-STATUS IS EQUAL TO 'C'
00888          MOVE 'TRNSFR'               TO  ACVSTATO.
00889      IF PM-CURRENT-STATUS IS EQUAL TO 'F'
00890          MOVE 'SETTLE'               TO  ACVSTATO.
00891      IF PM-CURRENT-STATUS IS EQUAL TO 'T'
00892          MOVE 'TRMNAT'               TO  ACVSTATO.
00893
00894  0120-FINISH-POLICY.
00895
00896      MOVE PM-LOAN-NUMBER             TO  LOANNOO.
00897      MOVE PM-LOAN-BALC               TO  LOANBALO.
00898      MOVE PM-LOAN-APR                TO  CAPRO.
00899      MOVE PM-BILLING-MODE            TO  CPREMTPO.
00900
00901      EJECT
00902  1000-SEND-AND-RETURN.
00903
00904      IF EMI-ERROR NOT = ZERO
00905          
      * EXEC CICS LINK
00906 *            PROGRAM  ('EL001')
00907 *            COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)
00908 *            LENGTH   (EMI-COMM-LENGTH)
00909 *        END-EXEC.
           MOVE 'EL001' TO DFHEIV1
      *    MOVE '."C                   ''   #00007940' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303037393430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00910
00911      MOVE EMI-MESSAGE-AREA (1) TO  CEMSG1O.
00912      MOVE -1                     TO CPFKEYL.
00913
00914      IF PI-USES-PAID-TO
00915         MOVE 'PAID TO  :' TO CPTHHDGO.
00916
00917      
      * EXEC CICS SEND
00918 *        MAPSET (WS-MAPSET-NAME)
00919 *        MAP    (WS-MAP-NAME)
00920 *        FROM   (EL132CI)
00921 *        CURSOR
00922 *        ERASE
00923 *    END-EXEC.
           MOVE LENGTH OF
            EL132CI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00007952' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303037393532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL132CI, 
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
           
00924
00925      
      * EXEC CICS RETURN
00926 *        TRANSID  (EIBTRNID)
00927 *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
00928 *        LENGTH   (PI-COMM-LENGTH)
00929 *    END-EXEC.
      *    MOVE '.(CT                  &   #00007960' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303037393630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EIBTRNID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00930
00931      EJECT
00932  8100-CLAIM-NOT-FOUND.
00933
00934      MOVE ER-0133                   TO  EMI-ERROR
00935      GO TO 1000-SEND-AND-RETURN.
00936
00937  8100-CERT-NOT-FOUND.
00938
00939      MOVE ER-0205                   TO  EMI-ERROR
00940      GO TO 1000-SEND-AND-RETURN.
00941
00942
00943  8100-EMPLCY-NOT-FOUND.
00944
00945      MOVE ER-9288                   TO  EMI-ERROR.
00946      GO TO 1000-SEND-AND-RETURN.
00947
00948      EJECT
00949  8300-SEND-TEXT.
00950
00951      
      * EXEC CICS SEND TEXT
00952 *        FROM   (LOGOFF-TEXT)
00953 *        LENGTH (LOGOFF-LENGTH)
00954 *        ERASE
00955 *        FREEKB
00956 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00007986' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303037393836' TO DFHEIV0(25:11)
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
           
00957
00958      
      * EXEC CICS RETURN
00959 *        END-EXEC.
      *    MOVE '.(                    &   #00007993' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303037393933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00960
00961  8300-EXIT.
00962
00963      EXIT.
00964
00965      EJECT
00966  8500-DATE-CONVERSION.
00967
00968      
      * EXEC CICS LINK
00969 *        PROGRAM  ('ELDATCV')
00970 *        COMMAREA (DATE-CONVERSION-DATA)
00971 *        LENGTH   (DC-COMM-LENGTH)
00972 *    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   ''   #00008003' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303038303033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00973
00974  8500-EXIT.
00975      EXIT.
00976
00977  8700-LOCATE-BENEFIT.
00978
00979      
      * EXEC CICS HANDLE CONDITION
00980 *        NOTFND (8700-EXIT)
00981 *    END-EXEC.
      *    MOVE '"$I                   ! ( #00008014' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303038303134' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00982
00983      MOVE SPACES                 TO  WS-KIND.
00984
00985      MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID
00986      MOVE SPACES                 TO  WS-CFK-ACCESS-TYPE
00987      MOVE WS-BENEFIT-NO          TO  WS-CFK-BENEFIT
00988
00989      
      * EXEC CICS READ
00990 *        DATASET (WS-CONTROL-FILE-DSID)
00991 *        RIDFLD  (WS-CONTROL-FILE-KEY)
00992 *        SET     (ADDRESS OF CONTROL-FILE)
00993 *        GTEQ
00994 *    END-EXEC.
      *    MOVE '&"S        G          (   #00008024' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038303234' TO DFHEIV0(25:11)
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
           
00995
00996      IF (WS-CFK-COMPANY-ID NOT EQUAL CF-COMPANY-ID)
00997        OR
00998         (WS-CFK-RECORD-TYPE NOT EQUAL CF-RECORD-TYPE)
00999         GO TO 8700-EXIT.
01000
01001      MOVE +1                     TO  WS-INDEX.
01002
01003  8700-LOOKUP-BENEFIT.
01004
01005      IF WS-BENEFIT-NO = CF-BENEFIT-CODE (WS-INDEX)
01006         MOVE CF-BENEFIT-ALPHA (WS-INDEX)  TO  WS-KIND
01007         GO TO 8700-EXIT.
01008
01009      IF CF-BENEFIT-CODE (WS-INDEX) NOT LESS CF-HI-BEN-IN-REC
01010          GO TO 8700-EXIT.
01011
01012      IF WS-INDEX LESS THAN +8
01013          ADD +1  TO  WS-INDEX
01014          GO TO 8700-LOOKUP-BENEFIT.
01015
01016  8700-EXIT.
01017
01018      EXIT.
01019      EJECT
01020  9800-LINK-REM-TERM.
01021
01022      
      * EXEC CICS LINK
01023 *        PROGRAM('ELRTRM')
01024 *        COMMAREA(CALCULATION-PASS-AREA)
01025 *        LENGTH(CP-COMM-LENGTH)
01026 *        END-EXEC.
           MOVE 'ELRTRM' TO DFHEIV1
      *    MOVE '."C                   ''   #00008057' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303038303537' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CALCULATION-PASS-AREA, 
                 CP-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01027
01028  9800-EXIT.
01029      EXIT.
01030
01031  9990-ERROR.
01032
01033      MOVE DFHEIBLK               TO EMI-LINE1
01034      
      * EXEC CICS LINK
01035 *        PROGRAM   ('EL004')
01036 *        COMMAREA  (EMI-LINE1)
01037 *        LENGTH    (72)
01038 *        END-EXEC.
           MOVE 'EL004' TO DFHEIV1
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   ''   #00008069' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303038303639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01039
01040      GO TO 1000-SEND-AND-RETURN.
01041


       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1323' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9990-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 8100-CLAIM-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 0010-READ-CERT-MASTER
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 8100-CERT-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 8100-EMPLCY-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 0100-READ-CONT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 8700-EXIT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1323' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
