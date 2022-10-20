00001  IDENTIFICATION DIVISION.                                         04/20/98
00002                                                                   EL524
00003  PROGRAM-ID.                 EL524 .                                 LV009
00004 *              PROGRAM CONVERTED BY                               EL524
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL524
00006 *              CONVERSION DATE 04/01/95 12:30:35.                 EL524
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            EL524
00008 *                            VMOD=2.035                           EL524
CIDMOD*                                                                 EL524
CIDMOD*  THERE ARE NO CIDMODS MADE TO 'EL524' BASE SYSTEM CODE.         EL524
00009                                                                   EL524
00010 *AUTHOR.        LOGIC,INC.                                        EL524
00011 *               DALLAS, TEXAS.                                    EL524
00025 *REMARKS.                                                         EL524
00026 *        THIS PROGRAM SELECTS PAYMENTS AND RESERVE DATA FROM THE  EL524
00027 *        CLAS-IC CLAIMS SYSTEM AND FORMATS IT INTO PENDING CLAIMS EL524
00028 *        RECORDS FOR THE LOGIC CLAS-IC CREDIT SYSTEM.             EL524
022403******************************************************************
022403*                   C H A N G E   L O G
022403*
022403* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
022403*-----------------------------------------------------------------
022403*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
022403* EFFECTIVE    NUMBER
022403*-----------------------------------------------------------------
022403* 022403    2001061800003  SMVA  ADD PROC FOR NEW CLM TYP I & G
062104* 062104    2004050700001  SMVA  ADD NEW FILE TO AUTOMATE ME BALANCING
080609* 080609  IR2009080400001  PEMA  CORRECT PREV MONTHS TOTALS
030612* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
100112* 100112  CR2012083000001  PEMA  REMOVE REFERENCE TO ERRNO
052614* 052614  CR2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
070714* 070714  CR2013060600001  PEMA  AUTOMATE MONTH END BALANCING
012518* 012518  IR2018012400001  PEMA  correct sequenceing on voids
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
122018* 122018  IR2018120300001  TANA  INCLUDE OTHER CLAIM IN ME CHKPT
062121* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
022122* 022122  CR2021100800003  PEMA  Add B and H claim types
022403******************************************************************
00029                                                                   EL524
00030  EJECT                                                            EL524
00031  ENVIRONMENT DIVISION.                                            EL524
00032                                                                   EL524
00033  INPUT-OUTPUT SECTION.                                            EL524
00034                                                                   EL524
00035  FILE-CONTROL.                                                    EL524
00036                                                                   EL524
00037      SELECT PRNTR            ASSIGN TO SYS008-UR-1403-S-SYS008.

00042      SELECT ELREPT           ASSIGN TO SYS018-FBA1-ELREPT         EL524
00043                              ORGANIZATION INDEXED                 EL524
00044                              ACCESS       DYNAMIC                 EL524
00045                              RECORD KEY   RF-CONTROL-PRIMARY      EL524
00046                              FILE STATUS  DTE-VSAM-FLAGS.         EL524
00038                                                                   EL524
pemuni     SELECT REPORTS-EXTRACT-FILE 
                                   ASSIGN TO SYS010.      

062104     SELECT ME-EL524-BALANCE ASSIGN TO SYS011
062104                             ORGANIZATION IS LINE SEQUENTIAL.
00041                                                                   EL524
00048      SELECT DISK-DATE        ASSIGN TO SYS019-FBA1-S-SYS019.      EL524
00049                                                                   EL524
00050      SELECT FICH             ASSIGN TO SYS020-UT-2400-S-SYS020.   EL524
00051                                                                   EL524
00052      SELECT ERPNDC           ASSIGN TO SYS021-FBA1-ERPNDC         EL524
00053                              ORGANIZATION INDEXED                 EL524
00054                              ACCESS       DYNAMIC                 EL524
00055                              RECORD KEY   PC-CONTROL-PRIMARY      EL524
00056                              FILE STATUS  ERPNDC-FILE-STATUS.     EL524
00057                                                                   EL524
00058      SELECT ERPNDB           ASSIGN TO SYS022-FBA1-ERPNDB         EL524
00059                              ORGANIZATION INDEXED                 EL524
00060                              ACCESS       DYNAMIC                 EL524
00061                              RECORD KEY   PB-CONTROL-PRIMARY      EL524
00062                              FILE STATUS  ERPNDB-FILE-STATUS.     EL524
00063                                                                   EL524
00064      SELECT ELCERT           ASSIGN TO SYS023-FBA1-ELCERT         EL524
00065                              ORGANIZATION INDEXED                 EL524
00066                              ACCESS       DYNAMIC                 EL524
00067                              RECORD KEY   CM-CONTROL-PRIMARY      EL524
00068                              FILE STATUS  ELCERT-FILE-STATUS.     EL524
00069                                                                   EL524
00070      SELECT ERACCT           ASSIGN TO SYS025-FBA1-ERACCT         EL524
00071                              ORGANIZATION INDEXED                 EL524
00072                              ACCESS       DYNAMIC                 EL524
00073                              RECORD KEY   AM-CONTROL-PRIMARY      EL524
00074                              FILE STATUS  ERACCT-FILE-STATUS.     EL524
00075                                                                   EL524
00076      SELECT ERMEBL           ASSIGN SYS024-FBA1-ERMEBL            EL524
00077                              ORGANIZATION INDEXED                 EL524
00078                              ACCESS       DYNAMIC                 EL524
00079                              RECORD KEY   ME-CONTROL-PRIMARY      EL524
00080                              FILE STATUS  ERMEBL-FILE-STATUS.     EL524
00081  EJECT                                                            EL524
00082  DATA DIVISION.                                                   EL524
00083                                                                   EL524
00084  FILE SECTION.                                                    EL524
00085                                                                   EL524
00086  FD  PRNTR                                                        EL524
00087                              COPY ELCPRTFD.                       EL524
00088  EJECT                                                            EL524
00089  FD  REPORTS-EXTRACT-FILE                                         EL524
00090                              COPY ELCEXTFD.                       EL524
00091      COPY ELCEXTR.                                                EL524

062104 FD  ME-EL524-BALANCE
062104     RECORDING MODE IS F
062104     BLOCK CONTAINS 0 RECORDS.
062104 01  ME-EL524-BALANCE-REC    PIC X(83).

00093  FD  ELREPT                                                       EL524
00094                              COPY ELCRPTFD.                       EL524
00095      COPY ELCREPT.                                                EL524
00096  EJECT                                                            EL524
00097  FD  DISK-DATE                                                    EL524
00098                              COPY ELCDTEFD.                          CL**5
00099  EJECT                                                            EL524
00100  FD  FICH                                                         EL524
00101                              COPY ELCFCHFD.                       EL524
00102  EJECT                                                            EL524
00103  FD  ERPNDC.                                                         CL**4
00104                                                                      CL**4
00105      COPY ERCPNDC.                                                EL524
00106  EJECT                                                            EL524
00107  FD  ERPNDB.                                                         CL**4
00108                                                                      CL**4
00109      COPY ERCPNDB.                                                EL524
00110  EJECT                                                            EL524
00111  FD  ELCERT.                                                         CL**4
00112                                                                      CL**4
00113      COPY ELCCERT.                                                EL524
00114  EJECT                                                            EL524
00115  FD  ERACCT.                                                         CL**4
00116                                                                      CL**4
00117      COPY ERCACCT.                                                EL524
00118  EJECT                                                            EL524
00119  FD  ERMEBL.                                                         CL**4
00120                                                                      CL**4
00121      COPY ERCMEBL.                                                EL524
00122  EJECT                                                            EL524
00123  WORKING-STORAGE SECTION.                                         EL524
00124  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      EL524
00125  77  LCP-ONCTR-01                  PIC S9(3) COMP-3 VALUE ZERO.   EL524
00126  77  LCP-ONCTR-02                  PIC S9(3) COMP-3 VALUE ZERO.   EL524
00127  77  FILLER PIC X(32) VALUE '********************************'.   EL524
00128  77  FILLER PIC X(32) VALUE '*     EL524 WORKING-STORAGE    *'.   EL524
00129  77  FILLER PIC X(32) VALUE '********** VMOD=2.035 **********'.   EL524
00130
pemuni 01  cobdir               pic x(100) value spaces.
pemuni 01  errno is external pic 9(9) comp-5.
pemuni 01  env-name             pic x(100).
pemuni 01  return-pointer usage pointer.

070714****=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=***
070714****                                                           ***
070714****   Month end balancing work area                           ***
070714****                                                           ***
070714****=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=***

00131  01  MONTH-END-DATA.                                              EL524
00132      12  ME-START-DATE.                                           EL524
00133          16  ME-START-MO         PIC 99.                          EL524
00134          16  FILLER              PIC X.                           EL524
00135          16  ME-START-DA         PIC 99.                          EL524
00136          16  FILLER              PIC X.                           EL524
00137          16  ME-START-YR         PIC 99.                          EL524
00138      12  ME-CNDS-DATE            PIC 9(6).                        EL524
00139      12  ME-CNDS-DATE-R REDEFINES ME-CNDS-DATE.                   EL524
00140          16  ME-CNDS-MO          PIC 99.                          EL524
00141          16  ME-CNDS-DA          PIC 99.                          EL524
00142          16  ME-CNDS-YR          PIC 99.                          EL524
00143      12  ME-START-TIME           PIC 9(6).                        EL524
00144      12  ME-UPDATE-FLAG          PIC X VALUE 'Y'.                 EL524
00145          88  ME-DO-UPDATE        VALUE 'Y'.                       EL524
00146          88  ME-NO-UPDATE        VALUE 'N'.                       EL524
00147      12  ERMEBL-FILE-STATUS      PIC XX.                          EL524
00148      12  MONTH-END-MOYR          PIC 9999 COMP.                   EL524
070714     12  hld-524-CLMS-L          PIC S9(9)V99  COMP-3 value +0.          
070714     12  hld-524-CLMS-AH         PIC S9(9)V99  COMP-3 value +0.          
070714     12  hld-524-RESV-L          PIC S9(9)V99  COMP-3 value +0.          
070714     12  hld-524-RESV-AH         PIC S9(9)V99  COMP-3 value +0.          
070714     12  hld-524-CLMS-TOT-CM     PIC S9(9)V99  comp-3 value +0.
00149                                                                   EL524
062104 01  WS-BALANCE-DESCRIPTION        PIC X(50)  VALUE
062104     'Total Claims Paid'.

062104 01  WS-ME-BALANCE-REC.
062104     12  WS-ME-BAL-JOB           PIC X(11)  VALUE SPACES.
062104     12  WS-ME-BAL-DELIM1        PIC X(01)  VALUE ';'.
062104     12  WS-ME-BAL-STEP          PIC X(08)  VALUE 'EL524   '.
062104     12  WS-ME-BAL-DELIM2        PIC X(01)  VALUE ';'.
062104     12  WS-ME-BAL-AMT           PIC ZZZ,ZZZ,ZZ9. 
062104     12  WS-ME-BAL-DELIM3        PIC X(01)  VALUE ';'.
062104     12  WS-ME-BAL-DESCRIP       PIC X(50)  VALUE SPACES.


00150  01  FILLER                          COMP-3.                      EL524
00151      12  WS-LINE-COUNT               PIC S9(3)   VALUE +99.       EL524
00152      12  WS-LINE-COUNT-MAX           PIC S9(3)   VALUE +56.       EL524
00153      12  WS-PAGE                     PIC S9(5)   VALUE ZERO.      EL524
00154      12  WS-REPORT-SW                PIC S9      VALUE +1.        EL524
00155      12  WS-RECORD-COUNT             PIC S9(9)   VALUE ZERO.      EL524
00156      12  WS-RETURN-CODE              PIC S9(3)   VALUE ZERO.      EL524
00157      12  WS-ZERO                     PIC S9      VALUE ZERO.      EL524
00158      12  WS-TRANS-OUTPUT-COUNT       PIC S9(7)   VALUE ZERO.      EL524
00159      12  WS-LIFE-TOTALS.                                          EL524
00160          16  WS-LIFE-CURR-AMOUNT     PIC S9(7)V99 VALUE ZERO.     EL524
00161          16  WS-LIFE-CURR-PMT-COUNT  PIC S9(7)    VALUE ZERO.     EL524
00162          16  WS-LIFE-PREV-AMOUNT     PIC S9(7)V99 VALUE ZERO.     EL524
00163          16  WS-LIFE-PREV-PMT-COUNT  PIC S9(7)    VALUE ZERO.     EL524
00164          16  WS-LIFE-MANUAL          PIC S9(9)V99 VALUE ZERO.     EL524
00165          16  WS-LIFE-IBNR            PIC S9(9)V99 VALUE ZERO.     EL524
00166          16  WS-LIFE-PTC             PIC S9(9)V99 VALUE ZERO.     EL524
00167          16  WS-LIFE-FUTURE          PIC S9(9)V99 VALUE ZERO.     EL524
00168          16  WS-LIFE-EXPENSES        PIC S9(9)V99 VALUE ZERO.     EL524

00169      12  WS-AH-TOTALS.                                            EL524
00170          16  WS-AH-CURR-AMOUNT       PIC S9(7)V99 VALUE ZERO.     EL524
00171          16  WS-AH-CURR-PMT-COUNT    PIC S9(7)    VALUE ZERO.     EL524
00172          16  WS-AH-PREV-AMOUNT       PIC S9(7)V99 VALUE ZERO.     EL524
00173          16  WS-AH-PREV-PMT-COUNT    PIC S9(7)    VALUE ZERO.     EL524
00174          16  WS-AH-MANUAL            PIC S9(9)V99 VALUE ZERO.     EL524
00175          16  WS-AH-IBNR              PIC S9(9)V99 VALUE ZERO.     EL524
00176          16  WS-AH-PTC               PIC S9(9)V99 VALUE ZERO.     EL524
00177          16  WS-AH-FUTURE            PIC S9(9)V99 VALUE ZERO.     EL524
00178          16  WS-AH-EXPENSES          PIC S9(9)V99 VALUE ZERO.     EL524
00179                                                                   EL524
022403     12  WS-IU-TOTALS.                                            EL524
022403         16  WS-IU-CURR-AMOUNT       PIC S9(7)V99 VALUE ZERO.     EL524
022403         16  WS-IU-CURR-PMT-COUNT    PIC S9(7)    VALUE ZERO.     EL524
022403         16  WS-IU-PREV-AMOUNT       PIC S9(7)V99 VALUE ZERO.     EL524
022403         16  WS-IU-PREV-PMT-COUNT    PIC S9(7)    VALUE ZERO.     EL524
022403         16  WS-IU-MANUAL            PIC S9(9)V99 VALUE ZERO.     EL524
022403         16  WS-IU-IBNR              PIC S9(9)V99 VALUE ZERO.     EL524
022403         16  WS-IU-PTC               PIC S9(9)V99 VALUE ZERO.     EL524
022403         16  WS-IU-FUTURE            PIC S9(9)V99 VALUE ZERO.     EL524
022403         16  WS-IU-EXPENSES          PIC S9(9)V99 VALUE ZERO.     EL524
00179                                                                   EL524
022403     12  WS-GP-TOTALS.                                            EL524
022403         16  WS-GP-CURR-AMOUNT       PIC S9(7)V99 VALUE ZERO.     EL524
022403         16  WS-GP-CURR-PMT-COUNT    PIC S9(7)    VALUE ZERO.     EL524
022403         16  WS-GP-PREV-AMOUNT       PIC S9(7)V99 VALUE ZERO.     EL524
022403         16  WS-GP-PREV-PMT-COUNT    PIC S9(7)    VALUE ZERO.     EL524
022403         16  WS-GP-MANUAL            PIC S9(9)V99 VALUE ZERO.     EL524
022403         16  WS-GP-IBNR              PIC S9(9)V99 VALUE ZERO.     EL524
022403         16  WS-GP-PTC               PIC S9(9)V99 VALUE ZERO.     EL524
022403         16  WS-GP-FUTURE            PIC S9(9)V99 VALUE ZERO.     EL524
022403         16  WS-GP-EXPENSES          PIC S9(9)V99 VALUE ZERO.     EL524
052614                                                                   EL524
052614     12  WS-FL-TOTALS.                                            EL524
052614         16  WS-FL-CURR-AMOUNT       PIC S9(7)V99 VALUE ZERO.     EL524
052614         16  WS-FL-CURR-PMT-COUNT    PIC S9(7)    VALUE ZERO.     EL524
052614         16  WS-FL-PREV-AMOUNT       PIC S9(7)V99 VALUE ZERO.     EL524
052614         16  WS-FL-PREV-PMT-COUNT    PIC S9(7)    VALUE ZERO.     EL524
052614         16  WS-FL-MANUAL            PIC S9(9)V99 VALUE ZERO.     EL524
052614         16  WS-FL-IBNR              PIC S9(9)V99 VALUE ZERO.     EL524
052614         16  WS-FL-PTC               PIC S9(9)V99 VALUE ZERO.     EL524
052614         16  WS-FL-FUTURE            PIC S9(9)V99 VALUE ZERO.     EL524
052614         16  WS-FL-EXPENSES          PIC S9(9)V99 VALUE ZERO.     EL524
100518                                                                   EL524
022122     12  WS-BR-TOTALS.                                            EL524
022122         16  WS-BR-CURR-AMOUNT       PIC S9(7)V99 VALUE ZERO.     EL524
022122         16  WS-BR-CURR-PMT-COUNT    PIC S9(7)    VALUE ZERO.     EL524
022122         16  WS-BR-PREV-AMOUNT       PIC S9(7)V99 VALUE ZERO.     EL524
022122         16  WS-BR-PREV-PMT-COUNT    PIC S9(7)    VALUE ZERO.     EL524
022122         16  WS-BR-MANUAL            PIC S9(9)V99 VALUE ZERO.     EL524
022122         16  WS-BR-IBNR              PIC S9(9)V99 VALUE ZERO.     EL524
022122         16  WS-BR-PTC               PIC S9(9)V99 VALUE ZERO.     EL524
022122         16  WS-BR-FUTURE            PIC S9(9)V99 VALUE ZERO.     EL524
022122         16  WS-BR-EXPENSES          PIC S9(9)V99 VALUE ZERO.     EL524

022122     12  WS-HS-TOTALS.                                            EL524
022122         16  WS-HS-CURR-AMOUNT       PIC S9(7)V99 VALUE ZERO.     EL524
022122         16  WS-HS-CURR-PMT-COUNT    PIC S9(7)    VALUE ZERO.     EL524
022122         16  WS-HS-PREV-AMOUNT       PIC S9(7)V99 VALUE ZERO.     EL524
022122         16  WS-HS-PREV-PMT-COUNT    PIC S9(7)    VALUE ZERO.     EL524
022122         16  WS-HS-MANUAL            PIC S9(9)V99 VALUE ZERO.     EL524
022122         16  WS-HS-IBNR              PIC S9(9)V99 VALUE ZERO.     EL524
022122         16  WS-HS-PTC               PIC S9(9)V99 VALUE ZERO.     EL524
022122         16  WS-HS-FUTURE            PIC S9(9)V99 VALUE ZERO.     EL524
022122         16  WS-HS-EXPENSES          PIC S9(9)V99 VALUE ZERO.     EL524
100518                                                                   EL524
100518     12  WS-OT-TOTALS.                                            EL524
100518         16  WS-OT-CURR-AMOUNT       PIC S9(7)V99 VALUE ZERO.     EL524
100518         16  WS-OT-CURR-PMT-COUNT    PIC S9(7)    VALUE ZERO.     EL524
100518         16  WS-OT-PREV-AMOUNT       PIC S9(7)V99 VALUE ZERO.     EL524
100518         16  WS-OT-PREV-PMT-COUNT    PIC S9(7)    VALUE ZERO.     EL524
100518         16  WS-OT-MANUAL            PIC S9(9)V99 VALUE ZERO.     EL524
100518         16  WS-OT-IBNR              PIC S9(9)V99 VALUE ZERO.     EL524
100518         16  WS-OT-PTC               PIC S9(9)V99 VALUE ZERO.     EL524
100518         16  WS-OT-FUTURE            PIC S9(9)V99 VALUE ZERO.     EL524
100518         16  WS-OT-EXPENSES          PIC S9(9)V99 VALUE ZERO.     EL524
00179                                                                   EL524
00180  01  WS-CARR-TABLE                   SYNC.                        EL524
00181      12  WS-CARR-TABLE-ENTRY           OCCURS 30 TIMES            EL524
00182              INDEXED BY CARRIER-INDEX                             EL524
00183                         CARRIER-INDEX-MAX.                        EL524
00184          16  WS-CARRIER                  PIC X.                   EL524
00185          16  WS-LIFE-TOTALS                  COMP-3.              EL524
00186              20  WS-CARR-AH-CURR-AMOUNT      PIC S9(7)V99.        EL524
00187              20  WS-CARR-AH-CURR-PMT-COUNT   PIC S9(7).           EL524
00188              20  WS-CARR-AH-PREV-AMOUNT      PIC S9(7)V99.        EL524
00189              20  WS-CARR-AH-PREV-PMT-COUNT   PIC S9(7).           EL524
022403             20  WS-CARR-IU-CURR-AMOUNT      PIC S9(7)V99.        EL524
022403             20  WS-CARR-IU-CURR-PMT-COUNT   PIC S9(7).           EL524
022403             20  WS-CARR-IU-PREV-AMOUNT      PIC S9(7)V99.        EL524
022403             20  WS-CARR-IU-PREV-PMT-COUNT   PIC S9(7).           EL524
022403             20  WS-CARR-GP-CURR-AMOUNT      PIC S9(7)V99.        EL524
022403             20  WS-CARR-GP-CURR-PMT-COUNT   PIC S9(7).           EL524
022403             20  WS-CARR-GP-PREV-AMOUNT      PIC S9(7)V99.        EL524
022403             20  WS-CARR-GP-PREV-PMT-COUNT   PIC S9(7).           EL524
052614             20  WS-CARR-FL-CURR-AMOUNT      PIC S9(7)V99.
052614             20  WS-CARR-FL-CURR-PMT-COUNT   PIC S9(7).
052614             20  WS-CARR-FL-PREV-AMOUNT      PIC S9(7)V99.
052614             20  WS-CARR-FL-PREV-PMT-COUNT   PIC S9(7).
022122             20  WS-CARR-BR-CURR-AMOUNT      PIC S9(7)V99.
022122             20  WS-CARR-BR-CURR-PMT-COUNT   PIC S9(7).
022122             20  WS-CARR-BR-PREV-AMOUNT      PIC S9(7)V99.
022122             20  WS-CARR-BR-PREV-PMT-COUNT   PIC S9(7).
022122             20  WS-CARR-HS-CURR-AMOUNT      PIC S9(7)V99.
022122             20  WS-CARR-HS-CURR-PMT-COUNT   PIC S9(7).
022122             20  WS-CARR-HS-PREV-AMOUNT      PIC S9(7)V99.
022122             20  WS-CARR-HS-PREV-PMT-COUNT   PIC S9(7).
100518             20  WS-CARR-OT-CURR-AMOUNT      PIC S9(7)V99.
100518             20  WS-CARR-OT-CURR-PMT-COUNT   PIC S9(7).
100518             20  WS-CARR-OT-PREV-AMOUNT      PIC S9(7)V99.
100518             20  WS-CARR-OT-PREV-PMT-COUNT   PIC S9(7).
00190              20  WS-CARR-LIFE-CURR-AMOUNT    PIC S9(7)V99.        EL524
00191              20  WS-CARR-LIFE-CURR-PMT-COUNT PIC S9(7).           EL524
00192              20  WS-CARR-LIFE-PREV-AMOUNT    PIC S9(7)V99.        EL524
00193              20  WS-CARR-LIFE-PREV-PMT-COUNT PIC S9(7).           EL524
00194              20  WS-CARR-MANUAL              PIC S9(7)V99.        EL524
00195              20  WS-CARR-IBNR                PIC S9(7)V99.        EL524
00196              20  WS-CARR-PTC                 PIC S9(7)V99.        EL524
00197              20  WS-CARR-FUTURE              PIC S9(7)V99.        EL524
00198      12  FILLER                        COMP.                      EL524
00199          16  WS-CARR-TABLE-MAX        PIC S9(4)   VALUE +30.      EL524
00200                                                                   EL524
00201  01  FILLER                          COMP SYNC.                   EL524
00202      12  PGM-SUB                     PIC S9(4)   VALUE +524.      EL524
00203      12  WS-INDEX                    PIC S9(4)   VALUE ZERO.      EL524
00204                                                                   EL524
00205  01  FILLER.                                                      EL524
00206      12  WS-SAVE-ACCOUNT-KEY         PIC X(19) VALUE LOW-VALUES.  EL524
00207      12  ERACCT-FILE-STATUS          PIC X(2)  VALUE ZEROS.       EL524
00208      12  ELCERT-FILE-STATUS          PIC X(2)  VALUE ZEROS.       EL524
00209      12  WS-RECORD-SEQUENCE          PIC S9(4) COMP VALUE +1.     EL524
00210      12  ABEND-CODE                  PIC X(4).                    EL524
00211      12  ABEND-OPTION                PIC X.                       EL524
00212      12  OLC-REPORT-NAME             PIC X(5)  VALUE 'EL524'.     EL524
00213      12  X                           PIC X     VALUE SPACE.       EL524
00214      12  WS-COMPANY-CD               PIC X     VALUE SPACE.       EL524
00215      12  WS-CREDIT-USER-SW           PIC X     VALUE SPACE.       EL524
00216          88  WS-CO-HAS-CLASIC-CREDIT           VALUE '2'.         EL524
00217      12  ELEXTR-EOF-SW               PIC X     VALUE SPACE.       EL524
00218          88  ELEXTR-EOF                        VALUE 'Y'.         EL524
00219      12  FIRST-TIME-SW               PIC X     VALUE 'Y'.         EL524
00220          88  FIRST-TIME                        VALUE 'Y'.         EL524
00221      12  WS-SAVE-PRINT-RECORD        PIC X(133)  VALUE SPACES.    EL524
00222      12  WS-DAYS-DISAB               PIC 9(3)   VALUE ZERO.       EL524
00223      12  WS-LAST-CARRIER             PIC X      VALUE SPACES.     EL524
00224      12  WS-ABEND-MESSAGE            PIC X(80)  VALUE SPACES.     EL524
00225      12  WS-ABEND-FILE-STATUS        PIC XX     VALUE ZERO.       EL524
00226                                                                      CL**6
00227      12  WS-DATE-PAID-EDIT           PIC X(10).                      CL**8
00228      12  WS-PMT-SELECT-DATE.                                         CL**7
00229          16  WS-PMT-SELECT-MO        PIC 99.                      EL524
00230          16  FILLER                  PIC X.                       EL524
00231          16  WS-PMT-SELECT-DA        PIC 99.                      EL524
00232          16  FILLER                  PIC X.                       EL524
00233          16  WS-PMT-SELECT-CCYY      PIC 9(4).                       CL**7
00234      12  WS-VOID-SELECT-DATE.                                     EL524
00235          16  WS-VOID-SELECT-MO       PIC 99.                      EL524
00236          16  FILLER                  PIC X.                       EL524
00237          16  WS-VOID-SELECT-DA       PIC 99.                      EL524
00238          16  FILLER                  PIC X.                       EL524
00239          16  WS-VOID-SELECT-CCYY     PIC 9(4).                       CL**7
00240      12  WS-FILE-ERROR-MESSAGE.                                   EL524
00241          16  FILLER                  PIC X(24)  VALUE             EL524
00242              'ERROR OCCURED OPENING - '.                          EL524
00243          16  WS-FEM-FILE-NAME        PIC X(8).                    EL524
00244      12  WS-ISSUE-BATCH.                                          EL524
00245          16  FILLER                  PIC X(3)        VALUE '#CL'. EL524
00246          16  FILLER                  PIC X           VALUE '0'.   EL524
00247          16  WS-BATCH-NUMBER         PIC S9(4)  COMP VALUE +1.    EL524
00248      12  WS-COMP-BATCH.                                           EL524
00249          16  WS-COMP-FIRST           PIC X(3)        VALUE SPACES.EL524
00250          16  FILLER                  PIC X(3)        VALUE SPACES.EL524
00251      12  ERPNDC-FILE-STATUS          PIC XX          VALUE ZERO.  EL524
00252      12  ERPNDB-FILE-STATUS          PIC XX          VALUE ZERO.  EL524
00253      12  ELCNTL-FILE-STATUS          PIC XX          VALUE ZERO.  EL524
00254  EJECT                                                            EL524
00255  01  WS-HEADING1.                                                 EL524
00256      12  FILLER                      PIC X(43)   VALUE '1'.       EL524
00257      12  WS-H1-TITLE                 PIC X(76)   VALUE               CL**2
00258          'EXTRACTED CLAIM PAYMENTS AND RESERVES'.                 EL524
00259      12  WS-H1-REPORT-NUMBER         PIC X(8) VALUE 'EL524  '.       CL**3
00260                                                                   EL524
00261  01  WS-HEADING2.                                                 EL524
00262      12  FILLER                      PIC X(46)   VALUE SPACES.    EL524
00263      12  WS-H2-CLIENT-NAME           PIC X(73)   VALUE SPACES.       CL**2
00264      12  WS-H2-DATE                  PIC X(8).                    EL524
00265                                                                   EL524
00266  01  WS-HEADING3.                                                 EL524
00267      12  FILLER                      PIC X(53)   VALUE SPACES.    EL524
00268      12  WS-H3-DATE                  PIC X(66)   VALUE SPACES.       CL**2
00269      12  FILLER                      PIC X(5)    VALUE 'PAGE'.    EL524
00270      12  WS-H3-PAGE                  PIC ZZ,ZZ9.                  EL524
00271                                                                   EL524
00272  01  WS-HEADING4.                                                 EL524
00273      12  FILLER                      PIC X(45)   VALUE            EL524
00274              '0                                          **'.     EL524
00275      12  FILLER                      PIC X(44)   VALUE            EL524
00276              '********** P A Y M E N T S ************  ---'.      EL524
00277      12  FILLER                      PIC X(44)   VALUE            EL524
00278              '------------ R E S E R V E S ---------------'.      EL524
00279                                                                   EL524
00280  01  WS-HEADING5.                                                 EL524
00281      12  FILLER                      PIC X(49)   VALUE            EL524
00282              '   CLAIM        CERT       ACCOUNT      CLM  PAID'. EL524
00283      12  FILLER                      PIC X(50)   VALUE            EL524
00284              '    CHECK'.                                         EL524
00285      12  FILLER                      PIC X(6)    VALUE 'PAY TO'.  EL524
00286                                                                   EL524
00287  01  WS-HEADING6.                                                 EL524
00288      12  FILLER                      PIC X(44)   VALUE            EL524
00289              '  NUMBER CAR   NUMBER      NUMBER    ST TYP'.       EL524
00290      12  FILLER                      PIC X(44)   VALUE            EL524
00291              ' DATE   NUMBER        AMOUNT    TYPE'.              EL524
00292      12  FILLER                      PIC X(33)   VALUE            EL524
00293              'MANUAL     CURRENT      FUTURE   '.                 EL524
00294      12  WS-HD6-IBNR                 PIC X(09)   VALUE            EL524
00295              '     IBNR'.                                         EL524
00296                                                                   EL524
00297  01  WS-TOTAL-DESCRIPTION.                                        EL524
00298      05  WS-TD-FILLER1               PIC X(15)  VALUE SPACES.     EL524
00299      05  WS-TD-LF-AH                 PIC X(7)   VALUE SPACES.     EL524
00300      05  FILLER                      PIC X(6)   VALUE 'TOTALS'.   EL524
00301                                                                   EL524
00302  01  WS-TOTAL2-DESCRIPTION.                                       EL524
00303      05  WS-TD2-FILLER1              PIC X(9)   VALUE SPACES.     EL524
00304      05  WS-TD2-LF-AH                PIC X(6)   VALUE SPACES.     EL524
00305                                                                   EL524
00306  01  WS-DETAIL1.                                                  EL524
00307      12  WS-D1-CARRIAGE-CONTROL      PIC X.                       EL524
00308      12  WS-D1-CLAIM-NO              PIC X(7).                    EL524
00309      12  FILLER                      PIC XX.                      EL524
00310      12  WS-D1-CARRIER               PIC X.                       EL524
00311      12  FILLER                      PIC XX.                      EL524
00312      12  WS-D1-CERT-NO               PIC X(11).                   EL524
00313      12  WS-D1-CREATE-CERT-FLAG      PIC X.                       EL524
00314      12  FILLER                      PIC X.                       EL524
00315      12  WS-D1-ACCOUNT               PIC X(10).                   EL524
00316      12  FILLER                      PIC X.                       EL524
00317      12  WS-D1-STATE                 PIC XX.                      EL524
00318      12  FILLER                      PIC X.                       EL524
00319      12  WS-D1-CLAIM-TYPE            PIC XX.                      EL524
00320      12  FILLER                      PIC X.                       EL524
00321      12  WS-D1-DATE-PAID.                                         EL524
00322          16  WS-D1-MONTH-PAID        PIC XX.                      EL524
00323          16  FILLER                  PIC X.                       EL524
00324          16  WS-D1-DAY-PAID          PIC XX.                      EL524
00325          16  FILLER                  PIC X.                       EL524
00326          16  WS-D1-YEAR-PAID         PIC XX.                      EL524
00327      12  FILLER                      PIC X.                       EL524
00328      12  WS-D1-CHECK-NUMBER          PIC X(7).                    EL524
00329      12  WS-D1-CURR-MO-FLAG          PIC X.                       EL524
00330      12  WS-D1-AMOUNT                PIC Z,ZZZ,ZZ9.99-.           EL524
00331      12  FILLER                      PIC X.                       EL524
00332      12  WS-D1-TYPE                  PIC X(7).                    EL524
00333      12  FILLER                      PIC X.                       EL524
00334      12  WS-OFF                      PIC XXX.                     EL524
00335      12  WS-D1-MANUAL-RESERVE        PIC ZZZZ,ZZ9.99-.            EL524
00336      12  WS-D1-PAY-TO-CURR-RESERVE   PIC ZZZZ,ZZ9.99-.            EL524
00337      12  WS-D1-FUTURE-RESERVE        PIC ZZZZ,ZZ9.99-.            EL524
00338      12  WS-D1-IBNR-RESERVE          PIC ZZZZ,ZZ9.99-.            EL524
00339      12  WS-D1-IBNR-RESERVE-X REDEFINES WS-D1-IBNR-RESERVE        EL524
00340                                      PIC X(12).                   EL524
00341                                                                   EL524
00342  01  WS-DETAIL2                      REDEFINES                    EL524
00343      WS-DETAIL1.                                                  EL524
00344      12  FILLER                      PIC X(37).                   EL524
00345      12  WS-D2-VOIDED                PIC X(7).                    EL524
00346                                                                   EL524
00347  01  WS-TOTAL-LINE1                  REDEFINES                    EL524
00348      WS-DETAIL1.                                                  EL524
00349      12  FILLER                      PIC X(22).                   EL524
00350      12  WS-T1-DESCRIPTION           PIC X(28).                   EL524
00351      12  WS-T1-COUNT                 PIC Z,ZZZ,ZZ9-.              EL524
00352      12  WS-T1-AMOUNT                PIC Z,ZZZ,ZZ9.99-.           EL524
00353      12  FILLER                      PIC X(8).                    EL524
00354      12  WS-T1-MANUAL-RESERVE        PIC ZZZZZ,ZZ9.99-.           EL524
00355      12  WS-T1-PAY-TO-CURR-RESERVE   PIC ZZZZZ,ZZ9.99-.           EL524
00356      12  WS-T1-FUTURE-RESERVE        PIC ZZZZZ,ZZ9.99-.           EL524
00357      12  WS-T1-IBNR-RESERVE          PIC ZZZZZ,ZZ9.99-.           EL524
00358      12  WS-T1-IBNR-RESERVE-X REDEFINES WS-T1-IBNR-RESERVE        EL524
00359                                      PIC X(13).                   EL524
00360                                                                   EL524
00361  01  WS-TOTAL-LINE2                  REDEFINES                    EL524
00362      WS-DETAIL1.                                                  EL524
00363      12  FILLER                      PIC X.                       EL524
00364      12  WS-T2-CARRIER               PIC X.                       EL524
00365      12  FILLER                      PIC X.                       EL524
00366      12  WS-T2-CARR-NAME             PIC X(28).                   EL524
00367      12  FILLER                      PIC X.                       EL524
00368      12  WS-T2-DESCRIPTION           PIC X(18).                   EL524
00369      12  WS-T2-COUNT                 PIC Z,ZZZ,ZZ9-.              EL524
00370      12  WS-T2-AMOUNT                PIC Z,ZZZ,ZZ9.99-.           EL524
00371      12  FILLER                      PIC X(8).                    EL524
00372      12  WS-T2-MANUAL-RESERVES       PIC ZZZZZ,ZZ9.99-.           EL524
00373      12  WS-T2-PAY-TO-CURR-RESERVES  PIC ZZZZZ,ZZ9.99-.           EL524
00374      12  WS-T2-FUTURE-RESERVES       PIC ZZZZZ,ZZ9.99-.           EL524
00375      12  WS-T2-IBNR-RESERVES         PIC ZZZZZ,ZZ9.99-.           EL524
00376      12  WS-T2-IBNR-RESERVES-X REDEFINES WS-T2-IBNR-RESERVES      EL524
00377                                      PIC X(13).                   EL524
00378  EJECT                                                            EL524
00379      COPY ELCDATE.                                                EL524
00380                                                                   EL524
00381  01  WS-DATE-CHECK-AREA.                                          EL524
00382      12  WS-JAN-1ST-1984-GREG-DT     PIC X(8) VALUE '01/01/84'.   EL524
00383      12  WS-JAN-1ST-1984-BIN-DT      PIC XX   VALUE X'7E01'.      EL524
00384                                                                   EL524
00385      COPY ELCDTECX.                                                  CL**5
00386                                                                   EL524
00387      COPY ELCDTEVR.                                                  CL**5
00388
pemuni linkage section.
pemuni 01  name-buff       pic x(100).
                                                                        EL524
00389  EJECT                                                            EL524
00390  PROCEDURE DIVISION.                                              EL524
00391                                                                   EL524
00392  0000-START.

pemuni     OPEN INPUT REPORTS-EXTRACT-FILE.                             EL524
                                                                        EL524
00399  0000-DATE-CARD-READ SECTION.    COPY ELCDTERX.                      CL**5
00400                                                                   EL524
070714****=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=***
070714****                                                           ***
070714****   Set up the month-end auto balancing.                    ***
070714****                                                           ***
070714****=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=***
070714
070714     MOVE WS-TIME                TO ME-START-TIME.                EL524
070714     MOVE WS-CURRENT-DATE        TO ME-START-DATE.                EL524
070714     MOVE ME-START-MO            TO ME-CNDS-MO.                   EL524
070714     MOVE ME-START-DA            TO ME-CNDS-DA.                   EL524
070714     MOVE ME-START-YR            TO ME-CNDS-YR.                   EL524
00415      IF DTE-SYS-F-CLASIC-CREDIT NOT = 'Y'                         EL524
00416          DISPLAY 'EL524 - COMPANY NOT ONLINE CREDIT USER *'       EL524
00417                  DTE-CLIENT '*' UPON CONSOLE                      EL524
00418          GO TO 1760-STOP-RUN
062104     END-IF.
00419                                                                   EL524
00420      EJECT                                                        EL524
00421  1000-MAIN-LOGIC SECTION.
                                                                        EL524
pemuni     string "DD_SYS010" low-values delimited by size into
               env-name
           end-string

           call "cobgetenv" using env-name returning return-pointer

           if return-pointer = null
              display "cobdir not found"
              goback
           end-if
           set address of name-buff to return-pointer
           string name-buff delimited by low-values into cobdir
           end-string
pemuni     display ' return from cobgetenv 'cobdir

00423      PERFORM OPEN-FILES.                                          EL524

062104     IF DTE-CLIENT = 'CID'
062104         MOVE 'CILGM10'          TO  WS-ME-BAL-JOB
062104     ELSE
030612       IF DTE-CLIENT = 'AHL'
030612         MOVE 'AHLGM10'          TO  WS-ME-BAL-JOB
030612       ELSE
062121         if dte-client = 'FNL'
062121           move 'FLLGM10'        TO  WS-ME-BAL-JOB
062121         ELSE
062104          MOVE 'CIDCLGM10'        TO  WS-ME-BAL-JOB
062121         END-IF
030612       END-IF
062104     END-IF.

00424                                                                   EL524
00425      PERFORM 7000-DELETE-COMPANY-FROM-FILE.                       EL524
00426                                                                   EL524
00427      IF  DTE-OPT-RESERVE-METHOD-AUTH                              EL524
00428          MOVE SPACES             TO WS-HD6-IBNR.                  EL524
00429                                                                   EL524
00430  1100-READ-EXTRACTS.                                              EL524
00431      READ REPORTS-EXTRACT-FILE                                    EL524
00432          AT END                                                   EL524
00433              MOVE 'Y'            TO ELEXTR-EOF-SW                 EL524
00434              GO TO 1500-END-PROCESSING.                           EL524
00435                                                                   EL524
00436      ADD +1  TO  WS-RECORD-COUNT.                                 EL524
00437                                                                   EL524
00438      IF EX-COMPANY-CD LESS THAN DTE-CLASIC-COMPANY-CD             EL524
00439          GO TO 1100-READ-EXTRACTS.                                EL524
00440                                                                   EL524
00441      IF EX-EXTRACT-CODE GREATER 'A'  OR                           EL524
00442         EX-RECORD-TYPE  GREATER 'B'  OR                           EL524
00443         EX-COMPANY-CD   GREATER DTE-CLASIC-COMPANY-CD             EL524
00444          GO TO 1500-END-PROCESSING.                               EL524
00445                                                                   EL524
00446      IF FIRST-TIME                                                EL524
00447          MOVE SPACE           TO  FIRST-TIME-SW                   EL524
00448          MOVE EX-AA-CARRIER   TO  WS-LAST-CARRIER                 EL524
00449                                   WS-CARRIER (1)                  EL524
00450          MOVE ZERO            TO  WS-CARR-AH-CURR-AMOUNT      (1) EL524
00451                                   WS-CARR-AH-CURR-PMT-COUNT   (1) EL524
00452                                   WS-CARR-AH-PREV-AMOUNT      (1) EL524
00453                                   WS-CARR-AH-PREV-PMT-COUNT   (1) EL524
022403                                  WS-CARR-IU-CURR-AMOUNT      (1) EL524
022403                                  WS-CARR-IU-CURR-PMT-COUNT   (1) EL524
022403                                  WS-CARR-IU-PREV-AMOUNT      (1) EL524
022403                                  WS-CARR-IU-PREV-PMT-COUNT   (1) EL524
022403                                  WS-CARR-GP-CURR-AMOUNT      (1) EL524
022403                                  WS-CARR-GP-CURR-PMT-COUNT   (1) EL524
022403                                  WS-CARR-GP-PREV-AMOUNT      (1) EL524
022403                                  WS-CARR-GP-PREV-PMT-COUNT   (1) EL524
052614                                  WS-CARR-FL-CURR-AMOUNT      (1)
052614                                  WS-CARR-FL-CURR-PMT-COUNT   (1)
052614                                  WS-CARR-FL-PREV-AMOUNT      (1)
052614                                  WS-CARR-FL-PREV-PMT-COUNT   (1)
022122                                  WS-CARR-BR-CURR-AMOUNT      (1)
022122                                  WS-CARR-BR-CURR-PMT-COUNT   (1)
022122                                  WS-CARR-BR-PREV-AMOUNT      (1)
022122                                  WS-CARR-BR-PREV-PMT-COUNT   (1)
022122                                  WS-CARR-HS-CURR-AMOUNT      (1)
022122                                  WS-CARR-HS-CURR-PMT-COUNT   (1)
022122                                  WS-CARR-HS-PREV-AMOUNT      (1)
022122                                  WS-CARR-HS-PREV-PMT-COUNT   (1)
100518                                  WS-CARR-OT-CURR-AMOUNT      (1)
100518                                  WS-CARR-OT-CURR-PMT-COUNT   (1)
100518                                  WS-CARR-OT-PREV-AMOUNT      (1)
100518                                  WS-CARR-OT-PREV-PMT-COUNT   (1)
00454                                   WS-CARR-LIFE-CURR-AMOUNT    (1) EL524
00455                                   WS-CARR-LIFE-CURR-PMT-COUNT (1) EL524
00456                                   WS-CARR-LIFE-PREV-AMOUNT    (1) EL524
00457                                   WS-CARR-LIFE-PREV-PMT-COUNT (1) EL524
00458                                   WS-CARR-MANUAL              (1) EL524
00459                                   WS-CARR-IBNR                (1) EL524
00460                                   WS-CARR-PTC                 (1) EL524
00461                                   WS-CARR-FUTURE              (1) EL524
00462          SET CARRIER-INDEX                                        EL524
00463              CARRIER-INDEX-MAX TO +1.                             EL524
00464                                                                   EL524
00465      IF EX-AA-CARRIER = WS-LAST-CARRIER                           EL524
00466          GO TO 1300-CHECK-RECORD-TYPE.                            EL524
00467                                                                   EL524
00468      SET CARRIER-INDEX TO +1.                                     EL524
00469                                                                   EL524
00470      MOVE EX-AA-CARRIER          TO  WS-LAST-CARRIER.             EL524
00471                                                                   EL524
00472      EJECT                                                        EL524
00473  1200-CHECK-CARRIER.                                              EL524
00474      IF WS-LAST-CARRIER = WS-CARRIER (CARRIER-INDEX)              EL524
00475          GO TO 1300-CHECK-RECORD-TYPE.                            EL524
00476                                                                   EL524
00477      IF CARRIER-INDEX LESS CARRIER-INDEX-MAX                      EL524
00478          SET CARRIER-INDEX UP BY +1                               EL524
00479          GO TO 1200-CHECK-CARRIER.                                EL524
00480                                                                   EL524
00481      IF CARRIER-INDEX NOT LESS WS-CARR-TABLE-MAX                  EL524
00482          MOVE 'CARRIER TABLE LIMITS EXCEEDED' TO WS-ABEND-MESSAGE EL524
00483          GO TO ABEND-PGM.                                         EL524
00484                                                                   EL524
00485      SET CARRIER-INDEX                                            EL524
00486          CARRIER-INDEX-MAX UP BY +1.                              EL524
00487                                                                   EL524
00488      MOVE WS-LAST-CARRIER  TO  WS-CARRIER       (CARRIER-INDEX).  EL524
00489                                                                   EL524
00490      MOVE ZEROS TO  WS-CARR-AH-CURR-AMOUNT      (CARRIER-INDEX)   EL524
00491                     WS-CARR-AH-CURR-PMT-COUNT   (CARRIER-INDEX)   EL524
00492                     WS-CARR-AH-PREV-AMOUNT      (CARRIER-INDEX)   EL524
00493                     WS-CARR-AH-PREV-PMT-COUNT   (CARRIER-INDEX)   EL524
022403                    WS-CARR-IU-CURR-AMOUNT      (CARRIER-INDEX)   EL524
022403                    WS-CARR-IU-CURR-PMT-COUNT   (CARRIER-INDEX)   EL524
022403                    WS-CARR-IU-PREV-AMOUNT      (CARRIER-INDEX)   EL524
022403                    WS-CARR-IU-PREV-PMT-COUNT   (CARRIER-INDEX)   EL524
022403                    WS-CARR-GP-CURR-AMOUNT      (CARRIER-INDEX)   EL524
022403                    WS-CARR-GP-CURR-PMT-COUNT   (CARRIER-INDEX)   EL524
022403                    WS-CARR-GP-PREV-AMOUNT      (CARRIER-INDEX)   EL524
022403                    WS-CARR-GP-PREV-PMT-COUNT   (CARRIER-INDEX)   EL524
052614                    WS-CARR-FL-CURR-AMOUNT      (CARRIER-INDEX)
052614                    WS-CARR-FL-CURR-PMT-COUNT   (CARRIER-INDEX)
052614                    WS-CARR-FL-PREV-AMOUNT      (CARRIER-INDEX)
052614                    WS-CARR-FL-PREV-PMT-COUNT   (CARRIER-INDEX)
022122                    WS-CARR-BR-CURR-AMOUNT      (CARRIER-INDEX)
022122                    WS-CARR-BR-CURR-PMT-COUNT   (CARRIER-INDEX)
022122                    WS-CARR-BR-PREV-AMOUNT      (CARRIER-INDEX)
022122                    WS-CARR-BR-PREV-PMT-COUNT   (CARRIER-INDEX)
022122                    WS-CARR-HS-CURR-AMOUNT      (CARRIER-INDEX)
022122                    WS-CARR-HS-CURR-PMT-COUNT   (CARRIER-INDEX)
022122                    WS-CARR-HS-PREV-AMOUNT      (CARRIER-INDEX)
022122                    WS-CARR-HS-PREV-PMT-COUNT   (CARRIER-INDEX)
100518                    WS-CARR-OT-CURR-AMOUNT      (CARRIER-INDEX)
100518                    WS-CARR-OT-CURR-PMT-COUNT   (CARRIER-INDEX)
100518                    WS-CARR-OT-PREV-AMOUNT      (CARRIER-INDEX)
100518                    WS-CARR-OT-PREV-PMT-COUNT   (CARRIER-INDEX)
00494                     WS-CARR-LIFE-CURR-AMOUNT    (CARRIER-INDEX)   EL524
00495                     WS-CARR-LIFE-CURR-PMT-COUNT (CARRIER-INDEX)   EL524
00496                     WS-CARR-LIFE-PREV-AMOUNT    (CARRIER-INDEX)   EL524
00497                     WS-CARR-LIFE-PREV-PMT-COUNT (CARRIER-INDEX)   EL524
00498                     WS-CARR-MANUAL              (CARRIER-INDEX)   EL524
00499                     WS-CARR-IBNR                (CARRIER-INDEX)   EL524
00500                     WS-CARR-PTC                 (CARRIER-INDEX)   EL524
00501                     WS-CARR-FUTURE              (CARRIER-INDEX).  EL524
00502                                                                   EL524
00503      EJECT                                                        EL524
00504  1300-CHECK-RECORD-TYPE.                                          EL524
00505      IF EX-RECORD-TYPE NOT = 'A'                                  EL524
00506          GO TO 1400-RECORD-TYPE-B.                                EL524
00507                                                                   EL524
00508      IF EX-AA-SYSTEM-IDENTIFIER IS EQUAL TO 'CV'                  EL524
00509          GO TO 1100-READ-EXTRACTS.                                EL524
00510                                                                   EL524
00511      IF  DTE-OPT-RESERVE-METHOD-AUTH                              EL524
00512          GO TO 1370-USE-OPTIONAL-DATA.                            EL524
00513                                                                   EL524
00514      IF EX-AA-CERT-STATUS EQUAL 'D' OR 'V'                        EL524
00515          GO TO 1100-READ-EXTRACTS.                                EL524
00516                                                                   EL524
00517      IF EX-AA-MANUAL-RESERVE      = ZERO   AND                    EL524
00518         EX-AA-PAY-CURRENT-RESERVE = ZERO   AND                    EL524
00519         EX-AA-IBNR-RESERVE        = ZERO   AND                    EL524
00520         EX-AA-FUTURE-RESERVE      = ZERO                          EL524
00521          GO TO 1100-READ-EXTRACTS.                                EL524
00522                                                                   EL524
00523      IF EX-AA-CERT-STATUS = '9'                                   EL524
00524         GO TO 1350-MOVE-PRINT-LINE.                               EL524
00525                                                                   EL524
022403     EVALUATE TRUE
022403     WHEN EX-AA-CLAIM-TYPE = AH-OVERRIDE-L1    
00527          ADD EX-AA-MANUAL-RESERVE       TO  WS-AH-MANUAL          EL524
00528          ADD EX-AA-PAY-CURRENT-RESERVE  TO  WS-AH-PTC             EL524
00529          ADD EX-AA-IBNR-RESERVE         TO  WS-AH-IBNR            EL524
00530          ADD EX-AA-FUTURE-RESERVE       TO  WS-AH-FUTURE          EL524

022403     WHEN EX-AA-CLAIM-TYPE = 'I'               
022403         ADD EX-AA-MANUAL-RESERVE       TO  WS-IU-MANUAL          EL524
022403         ADD EX-AA-PAY-CURRENT-RESERVE  TO  WS-IU-PTC             EL524
022403         ADD EX-AA-IBNR-RESERVE         TO  WS-IU-IBNR            EL524
022403         ADD EX-AA-FUTURE-RESERVE       TO  WS-IU-FUTURE          EL524

022403     WHEN EX-AA-CLAIM-TYPE = 'G'
022403         ADD EX-AA-MANUAL-RESERVE       TO  WS-GP-MANUAL
022403         ADD EX-AA-PAY-CURRENT-RESERVE  TO  WS-GP-PTC
022403         ADD EX-AA-IBNR-RESERVE         TO  WS-GP-IBNR
022403         ADD EX-AA-FUTURE-RESERVE       TO  WS-GP-FUTURE
052614
052614     WHEN EX-AA-CLAIM-TYPE = 'F'
052614         ADD EX-AA-MANUAL-RESERVE       TO  WS-FL-MANUAL
052614         ADD EX-AA-PAY-CURRENT-RESERVE  TO  WS-FL-PTC
052614         ADD EX-AA-IBNR-RESERVE         TO  WS-FL-IBNR
052614         ADD EX-AA-FUTURE-RESERVE       TO  WS-FL-FUTURE
100518
022122     WHEN EX-AA-CLAIM-TYPE = 'B'
022122         ADD EX-AA-MANUAL-RESERVE       TO  WS-BR-MANUAL
022122         ADD EX-AA-PAY-CURRENT-RESERVE  TO  WS-BR-PTC
022122         ADD EX-AA-IBNR-RESERVE         TO  WS-BR-IBNR
022122         ADD EX-AA-FUTURE-RESERVE       TO  WS-BR-FUTURE
022122
022122     WHEN EX-AA-CLAIM-TYPE = 'H'
022122         ADD EX-AA-MANUAL-RESERVE       TO  WS-HS-MANUAL
022122         ADD EX-AA-PAY-CURRENT-RESERVE  TO  WS-HS-PTC
022122         ADD EX-AA-IBNR-RESERVE         TO  WS-HS-IBNR
022122         ADD EX-AA-FUTURE-RESERVE       TO  WS-HS-FUTURE
100518
100518     WHEN EX-AA-CLAIM-TYPE = 'O'
100518         ADD EX-AA-MANUAL-RESERVE       TO  WS-OT-MANUAL
100518         ADD EX-AA-PAY-CURRENT-RESERVE  TO  WS-OT-PTC
100518         ADD EX-AA-IBNR-RESERVE         TO  WS-OT-IBNR
100518         ADD EX-AA-FUTURE-RESERVE       TO  WS-OT-FUTURE

022403     WHEN EX-AA-CLAIM-TYPE = LIFE-OVERRIDE-L1
00532          ADD EX-AA-MANUAL-RESERVE       TO  WS-LIFE-MANUAL        EL524
00533          ADD EX-AA-PAY-CURRENT-RESERVE  TO  WS-LIFE-PTC           EL524
00534          ADD EX-AA-IBNR-RESERVE         TO  WS-LIFE-IBNR          EL524
00535          ADD EX-AA-FUTURE-RESERVE       TO  WS-LIFE-FUTURE
022403     END-EVALUATE.
00536                                                                   EL524
00537      ADD EX-AA-MANUAL-RESERVE TO WS-CARR-MANUAL (CARRIER-INDEX).  EL524
00538      ADD EX-AA-PAY-CURRENT-RESERVE                                EL524
00539                               TO WS-CARR-PTC    (CARRIER-INDEX).  EL524
00540      ADD EX-AA-IBNR-RESERVE   TO WS-CARR-IBNR   (CARRIER-INDEX).  EL524
00541      ADD EX-AA-FUTURE-RESERVE TO WS-CARR-FUTURE (CARRIER-INDEX).  EL524
00542                                                                   EL524
00543  1350-MOVE-PRINT-LINE.                                            EL524
00544      MOVE SPACES                 TO  WS-DETAIL1.                  EL524
00545                                                                   EL524
00546      MOVE EX-SA-CLAIM-NO         TO  WS-D1-CLAIM-NO.              EL524
00547      MOVE EX-AA-CARRIER          TO  WS-D1-CARRIER.               EL524
00548      MOVE EX-SA-CERT-NO          TO  WS-D1-CERT-NO.               EL524
00549      MOVE EX-AA-ACCOUNT          TO  WS-D1-ACCOUNT.               EL524
00550      MOVE EX-AA-STATE            TO  WS-D1-STATE.                 EL524
00551                                                                   EL524
022403     EVALUATE TRUE
022403     WHEN EX-AA-CLAIM-TYPE = AH-OVERRIDE-L1    
00553          MOVE AH-OVERRIDE-L2     TO  WS-D1-CLAIM-TYPE             EL524

022403     WHEN EX-AA-CLAIM-TYPE = 'I'               
022403         MOVE 'IU'               TO  WS-D1-CLAIM-TYPE             EL524

022403     WHEN EX-AA-CLAIM-TYPE = 'G'               
022403         MOVE 'GP'               TO  WS-D1-CLAIM-TYPE             EL524
052614
052614     WHEN EX-AA-CLAIM-TYPE = 'F'
052614         MOVE 'FL'               TO  WS-D1-CLAIM-TYPE
100518
022122     WHEN EX-AA-CLAIM-TYPE = 'B'
022122         MOVE 'BR'               TO  WS-D1-CLAIM-TYPE
022122
022122     WHEN EX-AA-CLAIM-TYPE = 'H'
022122         MOVE 'HS'               TO  WS-D1-CLAIM-TYPE
100518
100518     WHEN EX-AA-CLAIM-TYPE = 'O'
100518         MOVE 'OT'               TO  WS-D1-CLAIM-TYPE

022403     WHEN EX-AA-CLAIM-TYPE = LIFE-OVERRIDE-L1
00555          MOVE LIFE-OVERRIDE-L2   TO  WS-D1-CLAIM-TYPE
022403     END-EVALUATE.
00556                                                                   EL524
00557      MOVE EX-AA-MANUAL-RESERVE   TO  WS-D1-MANUAL-RESERVE.        EL524
00558      MOVE EX-AA-IBNR-RESERVE     TO  WS-D1-IBNR-RESERVE.          EL524
00559      MOVE EX-AA-PAY-CURRENT-RESERVE                               EL524
00560                                  TO  WS-D1-PAY-TO-CURR-RESERVE.   EL524
00561      MOVE EX-AA-FUTURE-RESERVE TO WS-D1-FUTURE-RESERVE.           EL524
00562      GO TO 1390-PRINT-DETAIL-LINE.                                EL524
00563                                                                   EL524
00564  1370-USE-OPTIONAL-DATA.                                          EL524
00565                                                                   EL524
00566      IF  EX-AA-MANUAL-RSV-OPT = ZERO                              EL524
00567              AND                                                  EL524
00568          EX-AA-PAY-CURRENT-RSV-OPT = ZERO                         EL524
00569              AND                                                  EL524
00570          EX-AA-IBNR-RSV-OPT = ZERO                                EL524
00571              AND                                                  EL524
00572          EX-AA-FUTURE-RSV-OPT = ZERO                              EL524
00573          GO TO 1100-READ-EXTRACTS.                                EL524
00574                                                                   EL524
00575      IF  EX-AA-CERT-STATUS = '9'                                  EL524
00576          GO TO 1380-MOVE-PRINT-LINE.                              EL524
00577                                                                   EL524
022403     EVALUATE TRUE
022403     WHEN EX-AA-CLAIM-TYPE = AH-OVERRIDE-L1     
00579          ADD EX-AA-MANUAL-RSV-OPT                                 EL524
00580                                  TO WS-AH-MANUAL                  EL524
00581          ADD EX-AA-PAY-CURRENT-RSV-OPT                            EL524
00582                                  TO WS-AH-PTC                     EL524
00583          MOVE ZEROS              TO WS-AH-IBNR                    EL524
00584          ADD EX-AA-FUTURE-RSV-OPT                                 EL524
00585                                  TO WS-AH-FUTURE                  EL524
022403     WHEN EX-AA-CLAIM-TYPE = 'I'                
022403         ADD EX-AA-MANUAL-RSV-OPT                                 EL524
022403                                 TO WS-IU-MANUAL                  EL524
022403         ADD EX-AA-PAY-CURRENT-RSV-OPT                            EL524
022403                                 TO WS-IU-PTC                     EL524
022403         MOVE ZEROS              TO WS-IU-IBNR                    EL524
022403         ADD EX-AA-FUTURE-RSV-OPT                                 EL524
022403                                 TO WS-IU-FUTURE                  EL524

022403     WHEN EX-AA-CLAIM-TYPE = 'G'                
022403         ADD EX-AA-MANUAL-RSV-OPT                                 EL524
022403                                 TO WS-GP-MANUAL                  EL524
022403         ADD EX-AA-PAY-CURRENT-RSV-OPT                            EL524
022403                                 TO WS-GP-PTC                     EL524
022403         MOVE ZEROS              TO WS-GP-IBNR                    EL524
022403         ADD EX-AA-FUTURE-RSV-OPT                                 EL524
022403                                 TO WS-GP-FUTURE                  EL524
052614
052614     WHEN EX-AA-CLAIM-TYPE = 'F'
052614         ADD EX-AA-MANUAL-RSV-OPT
052614                                 TO WS-FL-MANUAL
052614         ADD EX-AA-PAY-CURRENT-RSV-OPT
052614                                 TO WS-FL-PTC
052614         MOVE ZEROS              TO WS-FL-IBNR
052614         ADD EX-AA-FUTURE-RSV-OPT
052614                                 TO WS-FL-FUTURE
100518
022122     WHEN EX-AA-CLAIM-TYPE = 'B'
022122         ADD EX-AA-MANUAL-RSV-OPT
022122                                 TO WS-BR-MANUAL
022122         ADD EX-AA-PAY-CURRENT-RSV-OPT
022122                                 TO WS-BR-PTC
022122         MOVE ZEROS              TO WS-BR-IBNR
022122         ADD EX-AA-FUTURE-RSV-OPT
022122                                 TO WS-BR-FUTURE
022122
022122     WHEN EX-AA-CLAIM-TYPE = 'H'
022122         ADD EX-AA-MANUAL-RSV-OPT
022122                                 TO WS-HS-MANUAL
022122         ADD EX-AA-PAY-CURRENT-RSV-OPT
022122                                 TO WS-HS-PTC
022122         MOVE ZEROS              TO WS-HS-IBNR
022122         ADD EX-AA-FUTURE-RSV-OPT
022122                                 TO WS-HS-FUTURE
100518
100518     WHEN EX-AA-CLAIM-TYPE = 'O'
100518         ADD EX-AA-MANUAL-RSV-OPT
100518                                 TO WS-OT-MANUAL
100518         ADD EX-AA-PAY-CURRENT-RSV-OPT
100518                                 TO WS-OT-PTC
100518         MOVE ZEROS              TO WS-OT-IBNR
100518         ADD EX-AA-FUTURE-RSV-OPT
100518                                 TO WS-OT-FUTURE

022403     WHEN EX-AA-CLAIM-TYPE = LIFE-OVERRIDE-L1
00587          ADD EX-AA-MANUAL-RSV-OPT                                 EL524
00588                                  TO WS-LIFE-MANUAL                EL524
00589          ADD EX-AA-PAY-CURRENT-RSV-OPT                            EL524
00590                                  TO WS-LIFE-PTC                   EL524
00591          MOVE ZEROS              TO WS-LIFE-IBNR                  EL524
00592          ADD EX-AA-FUTURE-RSV-OPT                                 EL524
00593                                  TO WS-LIFE-FUTURE
022403     END-EVALUATE.
00594                                                                   EL524
00595      ADD EX-AA-MANUAL-RSV-OPT                                     EL524
00596          TO WS-CARR-MANUAL (CARRIER-INDEX).                       EL524
00597      ADD EX-AA-PAY-CURRENT-RSV-OPT                                EL524
00598                               TO WS-CARR-PTC (CARRIER-INDEX).     EL524
00599      MOVE ZEROS               TO WS-CARR-IBNR (CARRIER-INDEX).    EL524
00600      ADD EX-AA-FUTURE-RSV-OPT TO WS-CARR-FUTURE (CARRIER-INDEX).  EL524
00601                                                                   EL524
00602  1380-MOVE-PRINT-LINE.                                            EL524
00603                                                                   EL524
00604      MOVE SPACES                 TO WS-DETAIL1.                   EL524
00605                                                                   EL524
00606      MOVE EX-SA-CLAIM-NO         TO WS-D1-CLAIM-NO.               EL524
00607      MOVE EX-AA-CARRIER          TO WS-D1-CARRIER.                EL524
00608      MOVE EX-SA-CERT-NO          TO WS-D1-CERT-NO.                EL524
00609      MOVE EX-AA-ACCOUNT          TO WS-D1-ACCOUNT.                EL524
00610      MOVE EX-AA-STATE            TO WS-D1-STATE.                  EL524
00611                                                                   EL524
022403     EVALUATE TRUE
022403     WHEN EX-AA-CLAIM-TYPE = AH-OVERRIDE-L1   
00613          MOVE AH-OVERRIDE-L2     TO WS-D1-CLAIM-TYPE              EL524

022403     WHEN EX-AA-CLAIM-TYPE = 'I'              
00613          MOVE 'IU'               TO WS-D1-CLAIM-TYPE              EL524

022403     WHEN EX-AA-CLAIM-TYPE = 'G'
00613          MOVE 'GP'               TO WS-D1-CLAIM-TYPE
052614
052614     WHEN EX-AA-CLAIM-TYPE = 'F'
052614         MOVE 'FL'               TO WS-D1-CLAIM-TYPE
100518
022122     WHEN EX-AA-CLAIM-TYPE = 'B'
022122         MOVE 'BR'               TO WS-D1-CLAIM-TYPE
022122
022122     WHEN EX-AA-CLAIM-TYPE = 'H'
022122         MOVE 'HS'               TO WS-D1-CLAIM-TYPE
100518
100518     WHEN EX-AA-CLAIM-TYPE = 'O'
100518         MOVE 'OT'               TO WS-D1-CLAIM-TYPE

022403     WHEN EX-AA-CLAIM-TYPE = LIFE-OVERRIDE-L1   
00615          MOVE LIFE-OVERRIDE-L2   TO WS-D1-CLAIM-TYPE
022403     END-EVALUATE.
00616                                                                   EL524
00617      MOVE EX-AA-MANUAL-RSV-OPT   TO WS-D1-MANUAL-RESERVE.         EL524
00618      MOVE EX-AA-PAY-CURRENT-RSV-OPT                               EL524
00619                                  TO WS-D1-PAY-TO-CURR-RESERVE.    EL524
00620      MOVE EX-AA-FUTURE-RSV-OPT   TO WS-D1-FUTURE-RESERVE.         EL524
00621      MOVE SPACES                 TO WS-D1-IBNR-RESERVE-X.         EL524
00622                                                                   EL524
00623  1390-PRINT-DETAIL-LINE.                                          EL524
00624                                                                   EL524
00625      MOVE WS-DETAIL1             TO  PRT.                         EL524
00626      PERFORM WRITE-A-LINE.                                        EL524
00627                                                                   EL524
00628      PERFORM 2000-CREATE-CLAIM-TRANSACTION.                       EL524
00629                                                                   EL524
00630      GO TO 1100-READ-EXTRACTS.                                    EL524
00631      EJECT                                                        EL524
00632                                                                   EL524
00633  1400-RECORD-TYPE-B.                                              EL524
00634 ******************************************************************EL524
00635 *         BYPASS ALL CLAIM PAYMENTS THAT HAVE A PAID DATE        *EL524
00636 *                PRIOR TO JAN. 1ST 1984.                         *EL524
00637 ******************************************************************EL524
00638                                                                   EL524
00639      IF EX-AB-SYSTEM-IDENTIFIER IS EQUAL TO 'CV'                  EL524
00640          GO TO 1100-READ-EXTRACTS.                                EL524
00641                                                                   EL524
00642      IF EX-AB-CHECK-WRITTEN-DT LESS THAN WS-JAN-1ST-1984-BIN-DT   EL524
00643         GO TO 1100-READ-EXTRACTS.                                 EL524
00644                                                                   EL524
00645      IF EX-AB-CHECK-WRITTEN-DT GREATER THAN BIN-RUN-DATE             CL**7
00646         GO TO 1100-READ-EXTRACTS.                                 EL524
00647                                                                   EL524
00648      IF EX-AB-CERT-STATUS EQUAL 'D' OR 'V'                        EL524
00649         GO TO 1100-READ-EXTRACTS.                                 EL524
00650                                                                   EL524
00651      MOVE SPACES                 TO  WS-DETAIL1.                  EL524
00652                                                                   EL524
00653      IF EX-AB-CHECK-WRITTEN-DT NOT = LOW-VALUES                   EL524
00654          MOVE EX-AB-CHECK-WRITTEN-DT TO  DC-BIN-DATE-1            EL524
00655          MOVE SPACES                 TO  DC-OPTION-CODE           EL524
00656          PERFORM 8500-DATE-CONVERSION                             EL524
00657          MOVE DC-GREG-DATE-1-EDIT    TO  WS-D1-DATE-PAID             CL**7
00658          MOVE DC-GREG-DATE-A-EDIT    TO  WS-DATE-PAID-EDIT.          CL**7
00659                                                                   EL524
00660      IF EX-AB-CERT-STATUS = '9'                                   EL524
00661         GO TO 1420-CONTINUE.                                      EL524
00662                                                                   EL524
00663      MOVE ZEROS                      TO  WS-PMT-SELECT-DATE          CL**7
00664                                          WS-VOID-SELECT-DATE.     EL524
00665                                                                   EL524
00666      IF EX-AB-PMT-SELECT-DT NOT = LOW-VALUES AND SPACES           EL524
00667          MOVE EX-AB-PMT-SELECT-DT    TO  DC-BIN-DATE-1               CL**7
00668          MOVE SPACES                 TO  DC-OPTION-CODE           EL524
00669          PERFORM 8500-DATE-CONVERSION                             EL524
00670          MOVE DC-GREG-DATE-A-EDIT    TO  WS-PMT-SELECT-DATE          CL**7
00671      ELSE                                                         EL524
00672          MOVE WS-DATE-PAID-EDIT      TO  WS-PMT-SELECT-DATE.         CL**7
00673                                                                   EL524
00674      IF EX-AB-VOID-SELECT-DT NOT = LOW-VALUES AND SPACES          EL524
00675          MOVE EX-AB-VOID-SELECT-DT   TO  DC-BIN-DATE-1               CL**7
00676          MOVE SPACES                 TO  DC-OPTION-CODE           EL524
00677          PERFORM 8500-DATE-CONVERSION                             EL524
00678          MOVE DC-GREG-DATE-A-EDIT    TO  WS-VOID-SELECT-DATE         CL**7
00679      ELSE                                                         EL524
00680          IF EX-AB-VOID-DT NOT = LOW-VALUES AND SPACES             EL524
00681              MOVE EX-AB-VOID-DT        TO  DC-BIN-DATE-1          EL524
00682              MOVE SPACES               TO  DC-OPTION-CODE         EL524
00683              PERFORM 8500-DATE-CONVERSION                         EL524
00684              MOVE DC-GREG-DATE-A-EDIT  TO  WS-VOID-SELECT-DATE.      CL**7
00685                                                                   EL524
022403*    IF EX-AB-CLAIM-TYPE NOT = AH-OVERRIDE-L1                     EL524
022403*        GO TO 1410-LIFE.                                         EL524

022403     EVALUATE TRUE
022403     WHEN EX-AB-CLAIM-TYPE = AH-OVERRIDE-L1  
               CONTINUE

022403     WHEN EX-AB-CLAIM-TYPE = LIFE-OVERRIDE-L1  
022403         PERFORM 1410-LIFE THRU 1410-EXIT
022403         GO TO 1420-CONTINUE

022403     WHEN EX-AB-CLAIM-TYPE = 'I'
022403         PERFORM 1415-UNEMPLOYMENT THRU 1415-EXIT
022403         GO TO 1420-CONTINUE

022403     WHEN EX-AB-CLAIM-TYPE = 'G'
022403         PERFORM 1417-GAP        THRU 1417-EXIT
022403         GO TO 1420-CONTINUE
052614
052614     WHEN EX-AB-CLAIM-TYPE = 'F'
052614         PERFORM 1419-FAM        THRU 1419-EXIT
052614         GO TO 1420-CONTINUE
100518
022122     WHEN EX-AB-CLAIM-TYPE = 'B'
022122         PERFORM 141B-BRV        THRU 141B-EXIT
022122         GO TO 1420-CONTINUE
022122
022122     WHEN EX-AB-CLAIM-TYPE = 'H'
022122         PERFORM 141C-HOS        THRU 141C-EXIT
022122         GO TO 1420-CONTINUE
100518
100518     WHEN EX-AB-CLAIM-TYPE = 'O'
100518         PERFORM 141A-OTH        THRU 141A-EXIT
100518         GO TO 1420-CONTINUE

022403     END-EVALUATE.
00688                                                                   EL524
080609*    IF (WS-PMT-SELECT-MO = RUN-MO  AND                           EL524
080609*        WS-PMT-SELECT-CCYY = RUN-CCYY)               OR             CL**7
080609*               (WS-VOID-SELECT-MO = RUN-MO  AND                  EL524
080609*                WS-VOID-SELECT-CCYY = RUN-CCYY)                     CL**7
080609*        ADD EX-AB-PAYMENT-AMOUNT  TO  WS-AH-CURR-AMOUNT          EL524
080609*                        WS-CARR-AH-CURR-AMOUNT (CARRIER-INDEX)   EL524
080609*        ADD +1  TO  WS-AH-CURR-PMT-COUNT                         EL524
080609*                    WS-CARR-AH-CURR-PMT-COUNT (CARRIER-INDEX)    EL524
080609*      ELSE                                                       EL524
080609*        ADD EX-AB-PAYMENT-AMOUNT  TO  WS-AH-PREV-AMOUNT          EL524
080609*                        WS-CARR-AH-PREV-AMOUNT (CARRIER-INDEX)   EL524
080609*        ADD +1  TO  WS-AH-PREV-PMT-COUNT                         EL524
080609*                    WS-CARR-AH-PREV-PMT-COUNT (CARRIER-INDEX).   EL524
080609*                                                                 EL524

080609     IF (WS-PMT-SELECT-DATE NOT = ZEROS)
080609        AND (EX-AB-VOID-DT = LOW-VALUES)
080609        IF (WS-PMT-SELECT-MO = RUN-MO)
080609           AND (WS-PMT-SELECT-CCYY = RUN-CCYY)
080609           ADD EX-AB-PAYMENT-AMOUNT TO WS-AH-CURR-AMOUNT
080609               WS-CARR-AH-CURR-AMOUNT (CARRIER-INDEX)
080609           ADD +1                   TO WS-AH-CURR-PMT-COUNT
080609               WS-CARR-AH-CURR-PMT-COUNT (CARRIER-INDEX)
080609        ELSE
080609           ADD EX-AB-PAYMENT-AMOUNT TO WS-AH-PREV-AMOUNT
080609               WS-CARR-AH-PREV-AMOUNT (CARRIER-INDEX)
080609           ADD +1                   TO  WS-AH-PREV-PMT-COUNT
080609               WS-CARR-AH-PREV-PMT-COUNT (CARRIER-INDEX)
080609        END-IF
080609     END-IF

080609     IF (WS-VOID-SELECT-DATE NOT = ZEROS)
080609        AND (EX-AB-VOID-DT NOT = LOW-VALUES)
080609        IF (WS-VOID-SELECT-MO = RUN-MO)
080609           AND (WS-VOID-SELECT-CCYY = RUN-CCYY)
080609           ADD EX-AB-PAYMENT-AMOUNT TO WS-AH-CURR-AMOUNT
080609               WS-CARR-AH-CURR-AMOUNT (CARRIER-INDEX)
080609           ADD +1                   TO WS-AH-CURR-PMT-COUNT
080609               WS-CARR-AH-CURR-PMT-COUNT (CARRIER-INDEX)
080609        ELSE
080609           ADD EX-AB-PAYMENT-AMOUNT TO  WS-AH-PREV-AMOUNT
080609               WS-CARR-AH-PREV-AMOUNT (CARRIER-INDEX)
080609           ADD +1                   TO WS-AH-PREV-PMT-COUNT
080609               WS-CARR-AH-PREV-PMT-COUNT (CARRIER-INDEX)
080609        END-IF
080609     END-IF

00703      IF EX-AB-PAYMENT-TYPE = '5'                                  EL524
00704          ADD EX-AB-PAYMENT-AMOUNT  TO  WS-AH-EXPENSES.            EL524
00705                                                                   EL524
00706      GO TO 1420-CONTINUE.                                         EL524
00707                                                                   EL524
00708  1410-LIFE.                                                       EL524
00709      IF EX-AB-PAYMENT-TYPE = '5'                                  EL524
00710         ADD EX-AB-PAYMENT-AMOUNT  TO  WS-LIFE-EXPENSES.           EL524
00711                                                                   EL524
00712      IF (WS-PMT-SELECT-MO = RUN-MO  AND                           EL524
00713          WS-PMT-SELECT-CCYY = RUN-CCYY)               OR             CL**7
00714                 (WS-VOID-SELECT-MO = RUN-MO  AND                  EL524
00715                  WS-VOID-SELECT-CCYY = RUN-CCYY)                     CL**7
00716          ADD EX-AB-PAYMENT-AMOUNT  TO                             EL524
00717                          WS-LIFE-CURR-AMOUNT                      EL524
00718                          WS-CARR-LIFE-CURR-AMOUNT (CARRIER-INDEX) EL524
00719          ADD +1  TO  WS-LIFE-CURR-PMT-COUNT                       EL524
00720                      WS-CARR-LIFE-CURR-PMT-COUNT (CARRIER-INDEX)  EL524
00721        ELSE                                                       EL524
00722          ADD EX-AB-PAYMENT-AMOUNT  TO                             EL524
00723                          WS-LIFE-PREV-AMOUNT                      EL524
00724                          WS-CARR-LIFE-PREV-AMOUNT (CARRIER-INDEX) EL524
00725          ADD +1  TO  WS-LIFE-PREV-PMT-COUNT                       EL524
00726                      WS-CARR-LIFE-PREV-PMT-COUNT (CARRIER-INDEX). EL524
022403
022403 1410-EXIT.
022403     EXIT.

022403 1415-UNEMPLOYMENT.                                               EL524
022403 
022403     IF (WS-PMT-SELECT-MO = RUN-MO  AND                           EL524
022403         WS-PMT-SELECT-CCYY = RUN-CCYY)               OR             CL**7
022403                (WS-VOID-SELECT-MO = RUN-MO  AND                  EL524
022403                 WS-VOID-SELECT-CCYY = RUN-CCYY)                     CL**7
022403         ADD EX-AB-PAYMENT-AMOUNT  TO  WS-IU-CURR-AMOUNT          EL524
022403                     WS-CARR-IU-CURR-AMOUNT (CARRIER-INDEX)   
022403         ADD +1                    TO  WS-IU-CURR-PMT-COUNT 
022403                     WS-CARR-IU-CURR-PMT-COUNT (CARRIER-INDEX)    EL524
022403     ELSE      
022403         ADD EX-AB-PAYMENT-AMOUNT  TO  WS-IU-PREV-AMOUNT          EL524
022403                         WS-CARR-IU-PREV-AMOUNT (CARRIER-INDEX)   EL524
022403         ADD +1                    TO  WS-IU-PREV-PMT-COUNT  
022403                     WS-CARR-IU-PREV-PMT-COUNT (CARRIER-INDEX).   EL524
022403                                                                  EL524
022403     IF EX-AB-PAYMENT-TYPE = '5'                                  EL524
022403         ADD EX-AB-PAYMENT-AMOUNT  TO  WS-IU-EXPENSES.            EL524
022403                                                                  EL524
022403
022403 1415-EXIT.
022403     EXIT.
022403 1417-GAP.
022403
022403     IF (WS-PMT-SELECT-MO = RUN-MO  AND
022403         WS-PMT-SELECT-CCYY = RUN-CCYY)               OR
022403                (WS-VOID-SELECT-MO = RUN-MO  AND
022403                 WS-VOID-SELECT-CCYY = RUN-CCYY)
022403         ADD EX-AB-PAYMENT-AMOUNT  TO  WS-GP-CURR-AMOUNT
022403                     WS-CARR-GP-CURR-AMOUNT (CARRIER-INDEX)
022403         ADD +1                    TO  WS-GP-CURR-PMT-COUNT
022403                     WS-CARR-GP-CURR-PMT-COUNT (CARRIER-INDEX)
022403     ELSE
022403         ADD EX-AB-PAYMENT-AMOUNT  TO  WS-GP-PREV-AMOUNT
022403                         WS-CARR-GP-PREV-AMOUNT (CARRIER-INDEX)
022403         ADD +1                    TO  WS-GP-PREV-PMT-COUNT
022403                     WS-CARR-GP-PREV-PMT-COUNT (CARRIER-INDEX).
022403
022403     IF EX-AB-PAYMENT-TYPE = '5'
022403         ADD EX-AB-PAYMENT-AMOUNT  TO  WS-GP-EXPENSES.
022403
022403
022403 1417-EXIT.
022403     EXIT.
052614 1419-FAM.
052614
052614     IF (WS-PMT-SELECT-MO = RUN-MO  AND
052614         WS-PMT-SELECT-CCYY = RUN-CCYY)               OR
052614                (WS-VOID-SELECT-MO = RUN-MO  AND
052614                 WS-VOID-SELECT-CCYY = RUN-CCYY)
052614         ADD EX-AB-PAYMENT-AMOUNT  TO  WS-FL-CURR-AMOUNT
052614                     WS-CARR-FL-CURR-AMOUNT (CARRIER-INDEX)
052614         ADD +1                    TO  WS-FL-CURR-PMT-COUNT
052614                     WS-CARR-FL-CURR-PMT-COUNT (CARRIER-INDEX)
052614     ELSE
052614         ADD EX-AB-PAYMENT-AMOUNT  TO  WS-FL-PREV-AMOUNT
052614                         WS-CARR-FL-PREV-AMOUNT (CARRIER-INDEX)
052614         ADD +1                    TO  WS-FL-PREV-PMT-COUNT
052614                     WS-CARR-FL-PREV-PMT-COUNT (CARRIER-INDEX).
052614
052614     IF EX-AB-PAYMENT-TYPE = '5'
052614         ADD EX-AB-PAYMENT-AMOUNT  TO  WS-FL-EXPENSES.
052614
052614
052614 1419-EXIT.
052614     EXIT.
100518 141A-OTH.
100518
100518     IF (WS-PMT-SELECT-MO = RUN-MO  AND
100518         WS-PMT-SELECT-CCYY = RUN-CCYY)               OR
100518                (WS-VOID-SELECT-MO = RUN-MO  AND
100518                 WS-VOID-SELECT-CCYY = RUN-CCYY)
100518         ADD EX-AB-PAYMENT-AMOUNT  TO  WS-OT-CURR-AMOUNT
100518                     WS-CARR-OT-CURR-AMOUNT (CARRIER-INDEX)
100518         ADD +1                    TO  WS-OT-CURR-PMT-COUNT
100518                     WS-CARR-OT-CURR-PMT-COUNT (CARRIER-INDEX)
100518     ELSE
100518         ADD EX-AB-PAYMENT-AMOUNT  TO  WS-OT-PREV-AMOUNT
100518                         WS-CARR-OT-PREV-AMOUNT (CARRIER-INDEX)
100518         ADD +1                    TO  WS-OT-PREV-PMT-COUNT
100518                     WS-CARR-OT-PREV-PMT-COUNT (CARRIER-INDEX).
100518
100518     IF EX-AB-PAYMENT-TYPE = '5'
100518         ADD EX-AB-PAYMENT-AMOUNT  TO  WS-OT-EXPENSES.
100518
100518
100518 141A-EXIT.
100518     EXIT.

022122 141B-BRV.
022122
022122     IF (WS-PMT-SELECT-MO = RUN-MO  AND
022122         WS-PMT-SELECT-CCYY = RUN-CCYY)               OR
022122                (WS-VOID-SELECT-MO = RUN-MO  AND
022122                 WS-VOID-SELECT-CCYY = RUN-CCYY)
022122         ADD EX-AB-PAYMENT-AMOUNT  TO  WS-BR-CURR-AMOUNT
022122                     WS-CARR-BR-CURR-AMOUNT (CARRIER-INDEX)
022122         ADD +1                    TO  WS-BR-CURR-PMT-COUNT
022122                     WS-CARR-BR-CURR-PMT-COUNT (CARRIER-INDEX)
022122     ELSE
022122         ADD EX-AB-PAYMENT-AMOUNT  TO  WS-BR-PREV-AMOUNT
022122                         WS-CARR-BR-PREV-AMOUNT (CARRIER-INDEX)
022122         ADD +1                    TO  WS-BR-PREV-PMT-COUNT
022122                     WS-CARR-BR-PREV-PMT-COUNT (CARRIER-INDEX).
022122
022122     IF EX-AB-PAYMENT-TYPE = '5'
022122         ADD EX-AB-PAYMENT-AMOUNT  TO  WS-BR-EXPENSES.
022122
022122
022122 141B-EXIT.
022122     EXIT.

022122 141C-HOS.
022122
022122     IF (WS-PMT-SELECT-MO = RUN-MO  AND
022122         WS-PMT-SELECT-CCYY = RUN-CCYY)               OR
022122                (WS-VOID-SELECT-MO = RUN-MO  AND
022122                 WS-VOID-SELECT-CCYY = RUN-CCYY)
022122         ADD EX-AB-PAYMENT-AMOUNT  TO  WS-HS-CURR-AMOUNT
022122                     WS-CARR-HS-CURR-AMOUNT (CARRIER-INDEX)
022122         ADD +1                    TO  WS-HS-CURR-PMT-COUNT
022122                     WS-CARR-HS-CURR-PMT-COUNT (CARRIER-INDEX)
022122     ELSE
022122         ADD EX-AB-PAYMENT-AMOUNT  TO  WS-HS-PREV-AMOUNT
022122                         WS-CARR-HS-PREV-AMOUNT (CARRIER-INDEX)
022122         ADD +1                    TO  WS-HS-PREV-PMT-COUNT
022122                     WS-CARR-HS-PREV-PMT-COUNT (CARRIER-INDEX).
022122
022122     IF EX-AB-PAYMENT-TYPE = '5'
022122         ADD EX-AB-PAYMENT-AMOUNT  TO  WS-HS-EXPENSES.
022122
022122
022122 141C-EXIT.
022122     EXIT.

00728  1420-CONTINUE.                                                   EL524
00729                                                                   EL524
00730      MOVE EX-AB-CLAIM-NO         TO  WS-D1-CLAIM-NO.              EL524
00731      MOVE EX-AB-CARRIER          TO  WS-D1-CARRIER.               EL524
00732      MOVE EX-AB-CERT-NO          TO  WS-D1-CERT-NO.               EL524
00733                                                                   EL524
00734      IF EX-AB-1ST-PAYMENT-SW NOT = SPACES                         EL524
00735          MOVE '*'                TO  WS-D1-CREATE-CERT-FLAG.      EL524
00736                                                                   EL524
00737      IF EX-COMPANY-ID = 'FIA'                                     EL524
00738         IF EX-AB-CARRIER = '2' OR '4' OR 'B'                      EL524
00739            MOVE '*'              TO  WS-D1-CREATE-CERT-FLAG       EL524
00740         ELSE                                                      EL524
00741            MOVE SPACES           TO  WS-D1-CREATE-CERT-FLAG.      EL524
00742                                                                   EL524
00743      MOVE EX-AA-ACCOUNT          TO  WS-D1-ACCOUNT.               EL524
00744      MOVE EX-AA-STATE            TO  WS-D1-STATE.                 EL524
00745                                                                   EL524
022403     EVALUATE TRUE 
022403     WHEN EX-AB-CLAIM-TYPE = AH-OVERRIDE-L1   
00747          MOVE AH-OVERRIDE-L2     TO  WS-D1-CLAIM-TYPE             EL524

022403     WHEN EX-AB-CLAIM-TYPE = LIFE-OVERRIDE-L1   
00749          MOVE LIFE-OVERRIDE-L2   TO  WS-D1-CLAIM-TYPE

022403     WHEN EX-AB-CLAIM-TYPE = 'I'
022403         MOVE 'IU'               TO  WS-D1-CLAIM-TYPE

022403     WHEN EX-AB-CLAIM-TYPE = 'G'
022403         MOVE 'GP'               TO  WS-D1-CLAIM-TYPE
052614
052614     WHEN EX-AB-CLAIM-TYPE = 'F'
052614         MOVE 'FL'               TO  WS-D1-CLAIM-TYPE
100518
022122     WHEN EX-AB-CLAIM-TYPE = 'B'
022122         MOVE 'BR'               TO  WS-D1-CLAIM-TYPE
022122
022122     WHEN EX-AB-CLAIM-TYPE = 'H'
022122         MOVE 'HS'               TO  WS-D1-CLAIM-TYPE
100518
100518     WHEN EX-AB-CLAIM-TYPE = 'O'
100518         MOVE 'OT'               TO  WS-D1-CLAIM-TYPE

022403     END-EVALUATE.

00750                                                                   EL524
00751      MOVE EX-AB-CHECK-NO         TO  WS-D1-CHECK-NUMBER.          EL524
00752      MOVE EX-AB-PAYMENT-AMOUNT   TO  WS-D1-AMOUNT.                EL524
00753                                                                   EL524
00754      IF EX-AB-PAYMENT-TYPE = '1'                                  EL524
00755         MOVE 'PARTIAL'  TO  WS-D1-TYPE                            EL524
00756      ELSE                                                         EL524
00757          IF EX-AB-PAYMENT-TYPE = '2'                              EL524
00758             MOVE 'FINAL'    TO  WS-D1-TYPE                        EL524
00759          ELSE                                                     EL524
00760              IF EX-AB-PAYMENT-TYPE = '3'                          EL524
00761                 MOVE 'LUMP'     TO  WS-D1-TYPE                    EL524
00762              ELSE                                                 EL524
00763                  IF EX-AB-PAYMENT-TYPE = '4'                      EL524
00764                     MOVE 'ADDL'     TO  WS-D1-TYPE                EL524
00765                  ELSE                                             EL524
00766                      IF EX-AB-PAYMENT-TYPE = '5'                  EL524
00767                         MOVE 'CHG EXP'  TO  WS-D1-TYPE            EL524
00768                      ELSE                                         EL524
00769                          IF EX-AB-PAYMENT-TYPE = '6'              EL524
00770                             MOVE 'NON CHG'  TO  WS-D1-TYPE        EL524
00771                          ELSE                                     EL524
00772                             IF EX-AB-PAYMENT-TYPE = '7' OR '8'    EL524
00773                                MOVE 'REFUND'   TO  WS-D1-TYPE     EL524
00774                             ELSE                                  EL524
00775                                 MOVE EX-AB-PAYMENT-TYPE           EL524
00776                                      TO WS-D1-TYPE.               EL524
00777                                                                   EL524
00778      IF EX-AB-OFFLINE-PMT                                         EL524
00779          MOVE 'OFF'              TO WS-OFF                        EL524
00780       ELSE                                                        EL524
00781          MOVE SPACES             TO WS-OFF.                       EL524
00782                                                                   EL524
00783      MOVE WS-DETAIL1             TO  PRT.                         EL524
00784      PERFORM WRITE-A-LINE.                                        EL524
00785                                                                   EL524
00786      IF EX-AB-VOID-DT NOT = LOW-VALUES                            EL524
00787          MOVE SPACES               TO  WS-DETAIL1                 EL524
00788          MOVE 'VOIDED'             TO  WS-D2-VOIDED               EL524
00789          MOVE EX-AB-VOID-DT        TO  DC-BIN-DATE-1              EL524
00790          MOVE SPACES               TO  DC-OPTION-CODE             EL524
00791          PERFORM 8500-DATE-CONVERSION                             EL524
00792          MOVE DC-GREG-DATE-1-EDIT  TO  WS-D1-DATE-PAID            EL524
00793          MOVE WS-DETAIL1           TO  PRT                        EL524
00794          PERFORM WRITE-A-LINE.                                    EL524
00795                                                                   EL524
00796      PERFORM 2000-CREATE-CLAIM-TRANSACTION.                       EL524
00797                                                                   EL524
00798      GO TO 1100-READ-EXTRACTS.                                    EL524
00799                                                                   EL524
00800  EJECT                                                            EL524
00801  1500-END-PROCESSING.                                             EL524
00802      MOVE '-'                    TO  WS-TOTAL-LINE1.              EL524
00803                                                                   EL524
00804      MOVE 'CURRENT MONTH'        TO  WS-TD-FILLER1.               EL524
00805      MOVE LIFE-OVERRIDE-L6       TO  WS-TD-LF-AH.                 EL524
00806      MOVE WS-TOTAL-DESCRIPTION   TO  WS-T1-DESCRIPTION.           EL524
00807                                                                   EL524
00808      MOVE WS-LIFE-CURR-AMOUNT    TO  WS-T1-AMOUNT.                EL524
00809      MOVE WS-LIFE-CURR-PMT-COUNT TO  WS-T1-COUNT.                 EL524
00810      MOVE WS-LIFE-MANUAL         TO  WS-T1-MANUAL-RESERVE.        EL524
00811      MOVE WS-LIFE-PTC            TO  WS-T1-PAY-TO-CURR-RESERVE.   EL524
00812      MOVE WS-LIFE-FUTURE         TO  WS-T1-FUTURE-RESERVE.        EL524
00813                                                                   EL524
00814      IF  DTE-OPT-RESERVE-METHOD-AUTH                              EL524
00815          MOVE SPACES             TO WS-T1-IBNR-RESERVE-X          EL524
00816                                                                   EL524
00817      ELSE                                                         EL524
00818          MOVE WS-LIFE-IBNR       TO WS-T1-IBNR-RESERVE.           EL524
00819                                                                   EL524
070714     COMPUTE hld-524-CLMS-L  = WS-LIFE-CURR-AMOUNT                EL524
00821                             + WS-LIFE-PREV-AMOUNT                 EL524
100518                            + WS-OT-CURR-AMOUNT
100518                            + WS-OT-PREV-AMOUNT.
00822                                                                   EL524
070714     COMPUTE hld-524-CLMS-AH = WS-AH-CURR-AMOUNT                  EL524
00824                             + WS-AH-PREV-AMOUNT
022403                            + WS-IU-CURR-AMOUNT                   EL524
022403                            + WS-IU-PREV-AMOUNT
052614                            + WS-FL-CURR-AMOUNT
052614                            + WS-FL-PREV-AMOUNT
022122                            + WS-BR-CURR-AMOUNT
022122                            + WS-BR-PREV-AMOUNT
022122                            + WS-HS-CURR-AMOUNT
022122                            + WS-HS-PREV-AMOUNT
022403                            + WS-GP-CURR-AMOUNT                   EL524
022403                            + WS-GP-PREV-AMOUNT.                  EL524
00825                                                                   EL524
070714     COMPUTE hld-524-RESV-L  = WS-LIFE-MANUAL                     EL524
00827                             + WS-LIFE-IBNR                        EL524
00828                             + WS-LIFE-PTC                         EL524
00829                             + WS-LIFE-FUTURE                      EL524
100518                            + WS-OT-MANUAL
100518                            + WS-OT-IBNR
100518                            + WS-OT-PTC
100518                            + WS-OT-FUTURE
00830                                                                   EL524
070714     COMPUTE hld-524-RESV-AH = WS-AH-MANUAL                       EL524
00832                             + WS-AH-IBNR                          EL524
00833                             + WS-AH-PTC                           EL524
00834                             + WS-AH-FUTURE
022403                            + WS-IU-MANUAL                        EL524
00832                             + WS-IU-IBNR                          EL524
00833                             + WS-IU-PTC                           EL524
00834                             + WS-IU-FUTURE
052614                            + WS-FL-MANUAL
052614                            + WS-FL-IBNR
052614                            + WS-FL-PTC
052614                            + WS-FL-FUTURE
022122                            + WS-BR-MANUAL
022122                            + WS-BR-IBNR
022122                            + WS-BR-PTC
022122                            + WS-BR-FUTURE
022122                            + WS-HS-MANUAL
022122                            + WS-HS-IBNR
022122                            + WS-HS-PTC
022122                            + WS-HS-FUTURE
022403                            + WS-GP-MANUAL                        EL524
00832                             + WS-GP-IBNR                          EL524
00833                             + WS-GP-PTC                           EL524
00834                             + WS-GP-FUTURE.                       EL524
00835                                                                   EL524
00836      MOVE WS-TOTAL-LINE1         TO  PRT.                         EL524
00837      PERFORM WRITE-A-LINE.                                        EL524
00838                                                                   EL524
00839      MOVE SPACES                 TO  WS-TOTAL-LINE1.              EL524
00840                                                                   EL524
00841      MOVE 'PREVIOUS MONTH'       TO  WS-TD-FILLER1.               EL524
00842      MOVE LIFE-OVERRIDE-L6       TO  WS-TD-LF-AH.                 EL524
00843      MOVE WS-TOTAL-DESCRIPTION   TO  WS-T1-DESCRIPTION.           EL524
00844                                                                   EL524
00845      MOVE WS-LIFE-PREV-AMOUNT    TO  WS-T1-AMOUNT.                EL524
00846      MOVE WS-LIFE-PREV-PMT-COUNT TO  WS-T1-COUNT.                 EL524
00847      MOVE WS-TOTAL-LINE1         TO  PRT.                         EL524
00848      PERFORM WRITE-A-LINE.                                        EL524
00849                                                                   EL524
00850      MOVE SPACES                 TO  WS-TOTAL-LINE1.              EL524
00851                                                                   EL524
00852      MOVE '     EXPENSES'        TO  WS-T1-DESCRIPTION.           EL524
00853      MOVE WS-LIFE-EXPENSES       TO  WS-T1-AMOUNT.                EL524
00854                                                                   EL524
00855      MOVE WS-TOTAL-LINE1         TO  PRT.                         EL524
00856      PERFORM WRITE-A-LINE.                                        EL524
00857                                                                   EL524
00858      MOVE '0'                    TO  WS-TOTAL-LINE1.              EL524
00859                                                                   EL524
00860      MOVE 'CURRENT MONTH'        TO  WS-TD-FILLER1.               EL524
00861      MOVE AH-OVERRIDE-L6         TO  WS-TD-LF-AH.                 EL524
00862      MOVE WS-TOTAL-DESCRIPTION   TO  WS-T1-DESCRIPTION.           EL524
00863                                                                   EL524
00864      MOVE WS-AH-CURR-AMOUNT      TO  WS-T1-AMOUNT.                EL524
00865      MOVE WS-AH-CURR-PMT-COUNT   TO  WS-T1-COUNT.                 EL524
00866      MOVE WS-AH-MANUAL           TO  WS-T1-MANUAL-RESERVE.        EL524
00867      MOVE WS-AH-PTC              TO  WS-T1-PAY-TO-CURR-RESERVE.   EL524
00868      MOVE WS-AH-FUTURE           TO  WS-T1-FUTURE-RESERVE.        EL524
00869                                                                   EL524
00870      IF  DTE-OPT-RESERVE-METHOD-AUTH                              EL524
00871          MOVE SPACES             TO WS-T1-IBNR-RESERVE-X          EL524
00872      ELSE                                                         EL524
00873          MOVE WS-AH-IBNR         TO WS-T1-IBNR-RESERVE.           EL524
00874                                                                   EL524
00875      MOVE WS-TOTAL-LINE1         TO  PRT.                         EL524
00876      PERFORM WRITE-A-LINE.                                        EL524
00877                                                                   EL524
00878      MOVE SPACES                 TO  WS-TOTAL-LINE1.              EL524
00879                                                                   EL524
00880      MOVE 'PREVIOUS MONTH'       TO  WS-TD-FILLER1.               EL524
00881      MOVE AH-OVERRIDE-L6         TO  WS-TD-LF-AH.                 EL524
00882      MOVE WS-TOTAL-DESCRIPTION   TO  WS-T1-DESCRIPTION.           EL524
00883                                                                   EL524
00884      MOVE WS-AH-PREV-AMOUNT      TO  WS-T1-AMOUNT.                EL524
00885      MOVE WS-AH-PREV-PMT-COUNT   TO  WS-T1-COUNT.                 EL524
00886      MOVE WS-TOTAL-LINE1         TO  PRT.                         EL524
00887      PERFORM WRITE-A-LINE.                                        EL524
00888                                                                   EL524
00889      MOVE SPACES                 TO  WS-TOTAL-LINE1.              EL524
00890                                                                   EL524
00891      MOVE '     EXPENSES'        TO  WS-T1-DESCRIPTION.           EL524
00892      MOVE WS-AH-EXPENSES         TO  WS-T1-AMOUNT.                EL524
00893                                                                   EL524
00894      MOVE WS-TOTAL-LINE1         TO  PRT.                         EL524
00895      PERFORM WRITE-A-LINE.                                        EL524
00896                                                                   EL524
022403*    MOVE '-'                    TO  WS-TOTAL-LINE1.              EL524
022403     MOVE '0'                    TO  WS-TOTAL-LINE1.              EL524
022403                                                                  EL524
022403     MOVE 'CURRENT MONTH'        TO  WS-TD-FILLER1.               EL524
022403     MOVE 'IU'                   TO  WS-TD-LF-AH.                 EL524
022403     MOVE WS-TOTAL-DESCRIPTION   TO  WS-T1-DESCRIPTION.           EL524
022403                                                                  EL524
022403     MOVE WS-IU-CURR-AMOUNT      TO  WS-T1-AMOUNT.                EL524
022403     MOVE WS-IU-CURR-PMT-COUNT   TO  WS-T1-COUNT.                 EL524
022403     MOVE WS-IU-MANUAL           TO  WS-T1-MANUAL-RESERVE.        EL524
022403     MOVE WS-IU-PTC              TO  WS-T1-PAY-TO-CURR-RESERVE.   EL524
022403     MOVE WS-IU-FUTURE           TO  WS-T1-FUTURE-RESERVE.        EL524
022403                                                                  EL524
022403     IF  DTE-OPT-RESERVE-METHOD-AUTH                              EL524
022403         MOVE SPACES             TO WS-T1-IBNR-RESERVE-X          EL524
022403     ELSE                                                         EL524
022403         MOVE WS-IU-IBNR         TO WS-T1-IBNR-RESERVE.           EL524
022403                                                                  EL524
022403     MOVE WS-TOTAL-LINE1         TO  PRT.                         EL524
022403     PERFORM WRITE-A-LINE.                                        EL524
022403                                                                  EL524
022403     MOVE SPACES                 TO  WS-TOTAL-LINE1.              EL524
022403                                                                  EL524
022403     MOVE 'PREVIOUS MONTH'       TO  WS-TD-FILLER1.               EL524
022403     MOVE 'IU'                   TO  WS-TD-LF-AH.                 EL524
022403     MOVE WS-TOTAL-DESCRIPTION   TO  WS-T1-DESCRIPTION.           EL524
022403                                                                  EL524
022403     MOVE WS-IU-PREV-AMOUNT      TO  WS-T1-AMOUNT.                EL524
022403     MOVE WS-IU-PREV-PMT-COUNT   TO  WS-T1-COUNT.                 EL524
022403     MOVE WS-TOTAL-LINE1         TO  PRT.                         EL524
022403     PERFORM WRITE-A-LINE.                                        EL524
022403                                                                  EL524
022403     MOVE SPACES                 TO  WS-TOTAL-LINE1.              EL524
022403                                                                  EL524
022403     MOVE '     EXPENSES'        TO  WS-T1-DESCRIPTION.           EL524
022403     MOVE WS-IU-EXPENSES         TO  WS-T1-AMOUNT.                EL524
022403                                                                  EL524
022403     MOVE WS-TOTAL-LINE1         TO  PRT.                         EL524
022403     PERFORM WRITE-A-LINE.                                        EL524
022403                                                                  EL524
022403*    MOVE '-'                    TO  WS-TOTAL-LINE1.              EL524
022403     MOVE '0'                    TO  WS-TOTAL-LINE1.              EL524
022403                                                                  EL524
022403     MOVE 'CURRENT MONTH'        TO  WS-TD-FILLER1.               EL524
022403     MOVE 'GP'                   TO  WS-TD-LF-AH.                 EL524
022403     MOVE WS-TOTAL-DESCRIPTION   TO  WS-T1-DESCRIPTION.           EL524
022403                                                                  EL524
022403     MOVE WS-GP-CURR-AMOUNT      TO  WS-T1-AMOUNT.                EL524
022403     MOVE WS-GP-CURR-PMT-COUNT   TO  WS-T1-COUNT.                 EL524
022403     MOVE WS-GP-MANUAL           TO  WS-T1-MANUAL-RESERVE.        EL524
022403     MOVE WS-GP-PTC              TO  WS-T1-PAY-TO-CURR-RESERVE.   EL524
022403     MOVE WS-GP-FUTURE           TO  WS-T1-FUTURE-RESERVE.        EL524
022403                                                                  EL524
022403     IF  DTE-OPT-RESERVE-METHOD-AUTH                              EL524
022403         MOVE SPACES             TO WS-T1-IBNR-RESERVE-X          EL524
022403     ELSE                                                         EL524
022403         MOVE WS-GP-IBNR         TO WS-T1-IBNR-RESERVE.           EL524
022403                                                                  EL524
022403     MOVE WS-TOTAL-LINE1         TO  PRT.                         EL524
022403     PERFORM WRITE-A-LINE.                                        EL524
022403                                                                  EL524
022403     MOVE SPACES                 TO  WS-TOTAL-LINE1.              EL524
022403                                                                  EL524
022403     MOVE 'PREVIOUS MONTH'       TO  WS-TD-FILLER1.               EL524
022403     MOVE 'GP'                   TO  WS-TD-LF-AH.                 EL524
022403     MOVE WS-TOTAL-DESCRIPTION   TO  WS-T1-DESCRIPTION.           EL524
022403                                                                  EL524
022403     MOVE WS-GP-PREV-AMOUNT      TO  WS-T1-AMOUNT.                EL524
022403     MOVE WS-GP-PREV-PMT-COUNT   TO  WS-T1-COUNT.                 EL524
022403     MOVE WS-TOTAL-LINE1         TO  PRT.                         EL524
022403     PERFORM WRITE-A-LINE.                                        EL524
022403                                                                  EL524
022403     MOVE SPACES                 TO  WS-TOTAL-LINE1.              EL524
022403                                                                  EL524
022403     MOVE '     EXPENSES'        TO  WS-T1-DESCRIPTION.           EL524
022403     MOVE WS-GP-EXPENSES         TO  WS-T1-AMOUNT.                EL524
022403                                                                  EL524
022403     MOVE WS-TOTAL-LINE1         TO  PRT.                         EL524
022403     PERFORM WRITE-A-LINE.                                        EL524
022403                                                                  EL524
052614     MOVE '0'                    TO  WS-TOTAL-LINE1.
052614
052614     MOVE 'CURRENT MONTH'        TO  WS-TD-FILLER1.
052614     MOVE 'FL'                   TO  WS-TD-LF-AH.
052614     MOVE WS-TOTAL-DESCRIPTION   TO  WS-T1-DESCRIPTION.
052614
052614     MOVE WS-FL-CURR-AMOUNT      TO  WS-T1-AMOUNT.
052614     MOVE WS-FL-CURR-PMT-COUNT   TO  WS-T1-COUNT.
052614     MOVE WS-FL-MANUAL           TO  WS-T1-MANUAL-RESERVE.
052614     MOVE WS-FL-PTC              TO  WS-T1-PAY-TO-CURR-RESERVE.
052614     MOVE WS-FL-FUTURE           TO  WS-T1-FUTURE-RESERVE.
052614
052614     IF  DTE-OPT-RESERVE-METHOD-AUTH
052614         MOVE SPACES             TO WS-T1-IBNR-RESERVE-X
052614     ELSE
052614         MOVE WS-FL-IBNR         TO WS-T1-IBNR-RESERVE.
052614
052614     MOVE WS-TOTAL-LINE1         TO  PRT.
052614     PERFORM WRITE-A-LINE.
052614
052614     MOVE SPACES                 TO  WS-TOTAL-LINE1.
052614
052614     MOVE 'PREVIOUS MONTH'       TO  WS-TD-FILLER1.
052614     MOVE 'FL'                   TO  WS-TD-LF-AH.
052614     MOVE WS-TOTAL-DESCRIPTION   TO  WS-T1-DESCRIPTION.
052614
052614     MOVE WS-FL-PREV-AMOUNT      TO  WS-T1-AMOUNT.
052614     MOVE WS-FL-PREV-PMT-COUNT   TO  WS-T1-COUNT.
052614     MOVE WS-TOTAL-LINE1         TO  PRT.
052614     PERFORM WRITE-A-LINE.
052614
052614     MOVE SPACES                 TO  WS-TOTAL-LINE1.
052614
052614     MOVE '     EXPENSES'        TO  WS-T1-DESCRIPTION.
052614     MOVE WS-FL-EXPENSES         TO  WS-T1-AMOUNT.
052614
052614     MOVE WS-TOTAL-LINE1         TO  PRT.
052614     PERFORM WRITE-A-LINE.
052614
022122     MOVE '0'                    TO  WS-TOTAL-LINE1.
022122
022122     MOVE 'CURRENT MONTH'        TO  WS-TD-FILLER1.
022122     MOVE 'BR'                   TO  WS-TD-LF-AH.
022122     MOVE WS-TOTAL-DESCRIPTION   TO  WS-T1-DESCRIPTION.
022122
022122     MOVE WS-BR-CURR-AMOUNT      TO  WS-T1-AMOUNT.
022122     MOVE WS-BR-CURR-PMT-COUNT   TO  WS-T1-COUNT.
022122     MOVE WS-BR-MANUAL           TO  WS-T1-MANUAL-RESERVE.
022122     MOVE WS-BR-PTC              TO  WS-T1-PAY-TO-CURR-RESERVE.
022122     MOVE WS-BR-FUTURE           TO  WS-T1-FUTURE-RESERVE.
022122
022122     IF  DTE-OPT-RESERVE-METHOD-AUTH
022122         MOVE SPACES             TO WS-T1-IBNR-RESERVE-X
022122     ELSE
022122         MOVE WS-BR-IBNR         TO WS-T1-IBNR-RESERVE.
022122
022122     MOVE WS-TOTAL-LINE1         TO  PRT.
022122     PERFORM WRITE-A-LINE.
022122
022122     MOVE SPACES                 TO  WS-TOTAL-LINE1.
022122
022122     MOVE 'PREVIOUS MONTH'       TO  WS-TD-FILLER1.
022122     MOVE 'BR'                   TO  WS-TD-LF-AH.
022122     MOVE WS-TOTAL-DESCRIPTION   TO  WS-T1-DESCRIPTION.
022122
022122     MOVE WS-BR-PREV-AMOUNT      TO  WS-T1-AMOUNT.
022122     MOVE WS-BR-PREV-PMT-COUNT   TO  WS-T1-COUNT.
022122     MOVE WS-TOTAL-LINE1         TO  PRT.
022122     PERFORM WRITE-A-LINE.
022122
022122     MOVE SPACES                 TO  WS-TOTAL-LINE1.
022122
022122     MOVE '     EXPENSES'        TO  WS-T1-DESCRIPTION.
022122     MOVE WS-BR-EXPENSES         TO  WS-T1-AMOUNT.
022122
022122     MOVE WS-TOTAL-LINE1         TO  PRT.
022122     PERFORM WRITE-A-LINE.
022122
022122     MOVE '0'                    TO  WS-TOTAL-LINE1.
022122
022122     MOVE 'CURRENT MONTH'        TO  WS-TD-FILLER1.
022122     MOVE 'HS'                   TO  WS-TD-LF-AH.
022122     MOVE WS-TOTAL-DESCRIPTION   TO  WS-T1-DESCRIPTION.
022122
022122     MOVE WS-HS-CURR-AMOUNT      TO  WS-T1-AMOUNT.
022122     MOVE WS-HS-CURR-PMT-COUNT   TO  WS-T1-COUNT.
022122     MOVE WS-HS-MANUAL           TO  WS-T1-MANUAL-RESERVE.
022122     MOVE WS-HS-PTC              TO  WS-T1-PAY-TO-CURR-RESERVE.
022122     MOVE WS-HS-FUTURE           TO  WS-T1-FUTURE-RESERVE.
022122
022122     IF  DTE-OPT-RESERVE-METHOD-AUTH
022122         MOVE SPACES             TO WS-T1-IBNR-RESERVE-X
022122     ELSE
022122         MOVE WS-HS-IBNR         TO WS-T1-IBNR-RESERVE.
022122
022122     MOVE WS-TOTAL-LINE1         TO  PRT.
022122     PERFORM WRITE-A-LINE.
022122
022122     MOVE SPACES                 TO  WS-TOTAL-LINE1.
022122
022122     MOVE 'PREVIOUS MONTH'       TO  WS-TD-FILLER1.
022122     MOVE 'HS'                   TO  WS-TD-LF-AH.
022122     MOVE WS-TOTAL-DESCRIPTION   TO  WS-T1-DESCRIPTION.
022122
022122     MOVE WS-HS-PREV-AMOUNT      TO  WS-T1-AMOUNT.
022122     MOVE WS-HS-PREV-PMT-COUNT   TO  WS-T1-COUNT.
022122     MOVE WS-TOTAL-LINE1         TO  PRT.
022122     PERFORM WRITE-A-LINE.
022122
022122     MOVE SPACES                 TO  WS-TOTAL-LINE1.
022122
022122     MOVE '     EXPENSES'        TO  WS-T1-DESCRIPTION.
022122     MOVE WS-HS-EXPENSES         TO  WS-T1-AMOUNT.
022122
022122     MOVE WS-TOTAL-LINE1         TO  PRT.
022122     PERFORM WRITE-A-LINE.
022122
100518     MOVE '0'                    TO  WS-TOTAL-LINE1.
100518
100518     MOVE 'CURRENT MONTH'        TO  WS-TD-FILLER1.
100518     MOVE 'OT'                   TO  WS-TD-LF-AH.
100518     MOVE WS-TOTAL-DESCRIPTION   TO  WS-T1-DESCRIPTION.
100518
100518     MOVE WS-OT-CURR-AMOUNT      TO  WS-T1-AMOUNT.
100518     MOVE WS-OT-CURR-PMT-COUNT   TO  WS-T1-COUNT.
100518     MOVE WS-OT-MANUAL           TO  WS-T1-MANUAL-RESERVE.
100518     MOVE WS-OT-PTC              TO  WS-T1-PAY-TO-CURR-RESERVE.
100518     MOVE WS-OT-FUTURE           TO  WS-T1-FUTURE-RESERVE.
100518
100518     IF  DTE-OPT-RESERVE-METHOD-AUTH
100518         MOVE SPACES             TO WS-T1-IBNR-RESERVE-X
100518     ELSE
100518         MOVE WS-OT-IBNR         TO WS-T1-IBNR-RESERVE.
100518
100518     MOVE WS-TOTAL-LINE1         TO  PRT.
100518     PERFORM WRITE-A-LINE.
100518
100518     MOVE SPACES                 TO  WS-TOTAL-LINE1.
100518
100518     MOVE 'PREVIOUS MONTH'       TO  WS-TD-FILLER1.
100518     MOVE 'OT'                   TO  WS-TD-LF-AH.
100518     MOVE WS-TOTAL-DESCRIPTION   TO  WS-T1-DESCRIPTION.
100518
100518     MOVE WS-OT-PREV-AMOUNT      TO  WS-T1-AMOUNT.
100518     MOVE WS-OT-PREV-PMT-COUNT   TO  WS-T1-COUNT.
100518     MOVE WS-TOTAL-LINE1         TO  PRT.
100518     PERFORM WRITE-A-LINE.
100518
100518     MOVE SPACES                 TO  WS-TOTAL-LINE1.
100518
100518     MOVE '     EXPENSES'        TO  WS-T1-DESCRIPTION.
100518     MOVE WS-OT-EXPENSES         TO  WS-T1-AMOUNT.
100518
100518     MOVE WS-TOTAL-LINE1         TO  PRT.
100518     PERFORM WRITE-A-LINE.
100518
022403     MOVE '-'                    TO  WS-TOTAL-LINE1.              EL524
00898                                                                   EL524
00899      MOVE 'CURRENT MONTH TOTALS' TO  WS-T1-DESCRIPTION.           EL524
00900                                                                      CL**4
00901      ADD WS-LIFE-CURR-AMOUNT                                      EL524
022403         WS-AH-CURR-AMOUNT WS-GP-CURR-AMOUNT
052614         WS-FL-CURR-AMOUNT
022122         WS-BR-CURR-AMOUNT
022122         WS-HS-CURR-AMOUNT
100518         WS-OT-CURR-AMOUNT
022403         WS-IU-CURR-AMOUNT    GIVING WS-T1-AMOUNT.     

070714     compute hld-524-clms-tot-cm = ws-life-curr-amount +
070714        ws-ah-curr-amount + ws-gp-curr-amount +
122018        ws-fl-curr-amount + ws-iu-curr-amount + ws-ot-curr-amount
022122        + WS-BR-CURR-AMOUNT + WS-HS-CURR-AMOUNT
062104
062104     MOVE WS-T1-AMOUNT           TO  WS-ME-BAL-AMT. 
062104     MOVE WS-BALANCE-DESCRIPTION TO  WS-ME-BAL-DESCRIP.
062104     WRITE ME-EL524-BALANCE-REC  FROM WS-ME-BALANCE-REC.
00903                                                                   EL524
00904      ADD WS-LIFE-CURR-PMT-COUNT                                   EL524
022403         WS-AH-CURR-PMT-COUNT WS-GP-CURR-PMT-COUNT
052614         WS-FL-CURR-PMT-COUNT
022122         WS-BR-CURR-PMT-COUNT
022122         WS-HS-CURR-PMT-COUNT
100518         WS-OT-CURR-PMT-COUNT
022403         WS-IU-CURR-PMT-COUNT GIVING WS-T1-COUNT.    
00906                                                                   EL524
00907      ADD WS-LIFE-MANUAL                                           EL524
100518         WS-AH-MANUAL  WS-GP-MANUAL  WS-FL-MANUAL  WS-OT-MANUAL
022122         WS-BR-MANUAL  WS-HS-MANUAL
022403         WS-IU-MANUAL         GIVING WS-T1-MANUAL-RESERVE.   
00909                                                                   EL524
00910      IF  DTE-OPT-RESERVE-METHOD-AUTH                              EL524
00911          MOVE SPACES          TO WS-T1-IBNR-RESERVE-X         
00912      ELSE                                                         EL524
00913          ADD WS-LIFE-IBNR                                         EL524
100518             WS-AH-IBNR WS-GP-IBNR WS-FL-IBNR WS-OT-IBNR
022122             WS-BR-IBNR WS-HS-IBNR
022403             WS-IU-IBNR       GIVING WS-T1-IBNR-RESERVE.     
00915                                                                   EL524
00916      ADD WS-LIFE-PTC                                              EL524
100518         WS-AH-PTC  WS-GP-PTC  WS-FL-PTC  WS-OT-PTC
022122         WS-BR-PTC  WS-HS-PTC
022403         WS-IU-PTC            GIVING WS-T1-PAY-TO-CURR-RESERVE.
00918                                                                   EL524
00919      ADD WS-LIFE-FUTURE                                           EL524
100518         WS-AH-FUTURE   WS-GP-FUTURE  WS-FL-FUTURE WS-OT-FUTURE
022122         WS-BR-FUTURE   WS-HS-FUTURE
022403         WS-IU-FUTURE         GIVING WS-T1-FUTURE-RESERVE.     
00921                                                                   EL524
00922      MOVE WS-TOTAL-LINE1      TO  PRT.    
00923      PERFORM WRITE-A-LINE.                                        EL524
00924                                                                   EL524
00925      MOVE '-'                 TO  WS-TOTAL-LINE1.    
00926                                                                   EL524
00927      MOVE 'PREVIOUS MONTH TOTALS' TO  WS-T1-DESCRIPTION.          EL524
00928                                                                   EL524
00929      ADD WS-LIFE-PREV-AMOUNT                                      EL524
022403         WS-AH-PREV-AMOUNT   WS-GP-PREV-AMOUNT
052614         WS-FL-PREV-AMOUNT
022122         WS-BR-PREV-AMOUNT
022122         WS-HS-PREV-AMOUNT
100518         WS-OT-PREV-AMOUNT
022403         WS-IU-PREV-AMOUNT    GIVING WS-T1-AMOUNT.    
00931                                                                   EL524
00932      ADD WS-LIFE-PREV-PMT-COUNT                                   EL524
022403         WS-AH-PREV-PMT-COUNT  WS-GP-PREV-PMT-COUNT
052614         WS-FL-PREV-PMT-COUNT
022122         WS-BR-PREV-PMT-COUNT
022122         WS-HS-PREV-PMT-COUNT
100518         WS-OT-PREV-PMT-COUNT
022403         WS-IU-PREV-PMT-COUNT GIVING WS-T1-COUNT.    
00934                                                                   EL524
00935      MOVE WS-TOTAL-LINE1      TO  PRT.      
00936      PERFORM WRITE-A-LINE.                                        EL524
00937                                                                   EL524
00938      MOVE SPACES              TO  WS-TOTAL-LINE1.       
00939                                                                   EL524
00940      MOVE ' TOTAL EXPENSES'   TO  WS-T1-DESCRIPTION.     
00941                                                                   EL524
022403     ADD WS-AH-EXPENSES 
052614         WS-LIFE-EXPENSES  WS-GP-EXPENSES  WS-FL-EXPENSES
022122         WS-OT-EXPENSES WS-BR-EXPENSES WS-HS-EXPENSES
022403         WS-IU-EXPENSES       GIVING WS-T1-AMOUNT.
00944                                                                   EL524
00945      MOVE WS-TOTAL-LINE1      TO  PRT.       
00946      PERFORM WRITE-A-LINE.                                        EL524
00947                                                                   EL524
00948      MOVE '-'                 TO  WS-TOTAL-LINE1.    
00949                                                                   EL524
00950      MOVE 'GRAND TOTALS'      TO  WS-T1-DESCRIPTION. 
00951                                                                   EL524
00952      COMPUTE WS-T1-AMOUNT = WS-LIFE-CURR-AMOUNT                   EL524
00953                           + WS-LIFE-PREV-AMOUNT                   EL524
00954                           + WS-AH-CURR-AMOUNT                     EL524
00955                           + WS-AH-PREV-AMOUNT
022403                          + WS-IU-CURR-AMOUNT
022403                          + WS-IU-PREV-AMOUNT
052614                          + WS-FL-CURR-AMOUNT
052614                          + WS-FL-PREV-AMOUNT
022122                          + WS-BR-CURR-AMOUNT
022122                          + WS-BR-PREV-AMOUNT
022122                          + WS-HS-CURR-AMOUNT
022122                          + WS-HS-PREV-AMOUNT
100518                          + WS-OT-CURR-AMOUNT
100518                          + WS-OT-PREV-AMOUNT
022403                          + WS-GP-CURR-AMOUNT
022403                          + WS-GP-PREV-AMOUNT. 
00956                                                                   EL524
00957      COMPUTE WS-T1-COUNT  = WS-LIFE-CURR-PMT-COUNT                EL524
00958                           + WS-LIFE-PREV-PMT-COUNT                EL524
00959                           + WS-AH-CURR-PMT-COUNT                  EL524
00960                           + WS-AH-PREV-PMT-COUNT
022403                          + WS-IU-CURR-PMT-COUNT                  EL524
022403                          + WS-IU-PREV-PMT-COUNT
052314                          + WS-FL-CURR-PMT-COUNT
052614                          + WS-FL-PREV-PMT-COUNT
022122                          + WS-BR-CURR-PMT-COUNT
022122                          + WS-BR-PREV-PMT-COUNT
022122                          + WS-HS-CURR-PMT-COUNT
022122                          + WS-HS-PREV-PMT-COUNT
100518                          + WS-OT-CURR-PMT-COUNT
100518                          + WS-OT-PREV-PMT-COUNT
022403                          + WS-GP-CURR-PMT-COUNT                  EL524
022403                          + WS-GP-PREV-PMT-COUNT.

00961                                                                   EL524
00962      ADD WS-LIFE-MANUAL                                           EL524
100518         WS-AH-MANUAL WS-GP-MANUAL WS-FL-MANUAL WS-OT-MANUAL
022122         WS-BR-MANUAL WS-HS-MANUAL
022403         WS-IU-MANUAL         GIVING WS-T1-MANUAL-RESERVE.  
00964                                                                   EL524
00965      IF  DTE-OPT-RESERVE-METHOD-AUTH                              EL524
00966          MOVE SPACES          TO WS-T1-IBNR-RESERVE-X    
00967      ELSE                                                         EL524
00968          ADD WS-LIFE-IBNR                                         EL524
100518             WS-AH-IBNR    WS-GP-IBNR   WS-FL-IBNR  WS-OT-IBNR
022122             WS-BR-IBNR WS-HS-IBNR
022403             WS-IU-IBNR       GIVING WS-T1-IBNR-RESERVE.    
00970                                                                   EL524
00971      ADD WS-LIFE-PTC                                              EL524
100518         WS-AH-PTC   WS-GP-PTC   WS-FL-PTC   WS-OT-PTC
022122         WS-BR-PTC WS-HS-PTC
022403         WS-IU-PTC            GIVING WS-T1-PAY-TO-CURR-RESERVE.
00973                                                                   EL524
00974      ADD WS-LIFE-FUTURE                                           EL524
100518         WS-AH-FUTURE    WS-GP-FUTURE   WS-FL-FUTURE WS-OT-FUTURE
022122         WS-BR-FUTURE WS-HS-FUTURE
022403         WS-IU-FUTURE         GIVING WS-T1-FUTURE-RESERVE. 
00976                                                                   EL524
00977      MOVE WS-TOTAL-LINE1         TO  PRT.                         EL524
00978      PERFORM WRITE-A-LINE.                                        EL524
00979                                                                   EL524
00980      MOVE ZEROS                  TO WS-LIFE-CURR-AMOUNT           EL524
00981                                     WS-LIFE-CURR-PMT-COUNT        EL524
00982                                     WS-LIFE-PREV-AMOUNT           EL524
00983                                     WS-LIFE-PREV-PMT-COUNT        EL524
00984                                     WS-LIFE-MANUAL                EL524
00985                                     WS-LIFE-IBNR                  EL524
00986                                     WS-LIFE-PTC                   EL524
00987                                     WS-LIFE-FUTURE                EL524
00988                                     WS-LIFE-EXPENSES              EL524
00989                                     WS-AH-CURR-AMOUNT             EL524
00990                                     WS-AH-CURR-PMT-COUNT          EL524
00991                                     WS-AH-PREV-AMOUNT             EL524
00992                                     WS-AH-PREV-PMT-COUNT          EL524
00993                                     WS-AH-MANUAL                  EL524
00994                                     WS-AH-IBNR                    EL524
00995                                     WS-AH-PTC                     EL524
00996                                     WS-AH-FUTURE                  EL524
00997                                     WS-AH-EXPENSES
022403                                    WS-IU-CURR-AMOUNT             EL524
022403                                    WS-IU-CURR-PMT-COUNT          EL524
022403                                    WS-IU-PREV-AMOUNT             EL524
022403                                    WS-IU-PREV-PMT-COUNT          EL524
022403                                    WS-IU-MANUAL                  EL524
022403                                    WS-IU-IBNR                    EL524
022403                                    WS-IU-PTC                     EL524
022403                                    WS-IU-FUTURE                  EL524
022403                                    WS-IU-EXPENSES

052614                                    WS-FL-CURR-AMOUNT
052614                                    WS-FL-CURR-PMT-COUNT
052614                                    WS-FL-PREV-AMOUNT
052614                                    WS-FL-PREV-PMT-COUNT
052614                                    WS-FL-MANUAL
052614                                    WS-FL-IBNR
052614                                    WS-FL-PTC
052614                                    WS-FL-FUTURE
052614                                    WS-FL-EXPENSES

022122                                    WS-BR-CURR-AMOUNT
022122                                    WS-BR-CURR-PMT-COUNT
022122                                    WS-BR-PREV-AMOUNT
022122                                    WS-BR-PREV-PMT-COUNT
022122                                    WS-BR-MANUAL
022122                                    WS-BR-IBNR
022122                                    WS-BR-PTC
022122                                    WS-BR-FUTURE
022122                                    WS-BR-EXPENSES
022122
022122                                    WS-HS-CURR-AMOUNT
022122                                    WS-HS-CURR-PMT-COUNT
022122                                    WS-HS-PREV-AMOUNT
022122                                    WS-HS-PREV-PMT-COUNT
022122                                    WS-HS-MANUAL
022122                                    WS-HS-IBNR
022122                                    WS-HS-PTC
022122                                    WS-HS-FUTURE
022122                                    WS-HS-EXPENSES

100518                                    WS-OT-CURR-AMOUNT
100518                                    WS-OT-CURR-PMT-COUNT
100518                                    WS-OT-PREV-AMOUNT
100518                                    WS-OT-PREV-PMT-COUNT
100518                                    WS-OT-MANUAL
100518                                    WS-OT-IBNR
100518                                    WS-OT-PTC
100518                                    WS-OT-FUTURE
100518                                    WS-OT-EXPENSES
022403                                    WS-GP-CURR-AMOUNT             EL524
022403                                    WS-GP-CURR-PMT-COUNT          EL524
022403                                    WS-GP-PREV-AMOUNT             EL524
022403                                    WS-GP-PREV-PMT-COUNT          EL524
022403                                    WS-GP-MANUAL                  EL524
022403                                    WS-GP-IBNR                    EL524
022403                                    WS-GP-PTC                     EL524
022403                                    WS-GP-FUTURE                  EL524
022403                                    WS-GP-EXPENSES.               EL524
00998                                                                   EL524
00999      IF WS-LAST-CARRIER = SPACE                                   EL524
01000          GO TO 1750-CLOSE-FILES.                                  EL524
01001                                                                   EL524
01002      MOVE +99                      TO  WS-LINE-COUNT.             EL524
01003                                                                   EL524
01004      MOVE '-* * * CARRIER TOTALS'  TO  PRT.                       EL524
01005      PERFORM WRITE-A-LINE.                                        EL524
01006                                                                   EL524
01007      SET CARRIER-INDEX TO +1.                                     EL524
01008                                                                   EL524
01009  1600-CARRIER-TOTALS.                                             EL524
01010      IF LCP-ONCTR-01 =  0                                         EL524
01011          ADD 1 TO LCP-ONCTR-01                                    EL524
01012          MOVE '-'                TO  WS-TOTAL-LINE2               EL524
01013        ELSE                                                       EL524
01014          MOVE '0'                TO  WS-TOTAL-LINE2.              EL524
01015                                                                   EL524
01016      MOVE WS-CARRIER (CARRIER-INDEX)  TO  WS-LAST-CARRIER         EL524
01017                                           WS-T2-CARRIER.          EL524
01018                                                                   EL524
01019      MOVE 'CURRENT'              TO  WS-TD2-FILLER1.              EL524
01020      MOVE AH-OVERRIDE-L6         TO  WS-TD2-LF-AH.                EL524
01021      MOVE WS-TOTAL2-DESCRIPTION  TO  WS-T2-DESCRIPTION.           EL524
01022                                                                   EL524
01023      MOVE WS-CARR-AH-CURR-AMOUNT (CARRIER-INDEX)                  EL524
01024                                  TO  WS-T2-AMOUNT.                EL524
01025      MOVE WS-CARR-AH-CURR-PMT-COUNT (CARRIER-INDEX)               EL524
01026                                  TO  WS-T2-COUNT.                 EL524
01027      MOVE WS-CARR-MANUAL (CARRIER-INDEX)                          EL524
01028                                  TO  WS-T2-MANUAL-RESERVES.       EL524
01029      MOVE WS-CARR-PTC    (CARRIER-INDEX)                          EL524
01030                                  TO  WS-T2-PAY-TO-CURR-RESERVES.  EL524
01031      MOVE WS-CARR-FUTURE (CARRIER-INDEX)                          EL524
01032                                  TO  WS-T2-FUTURE-RESERVES.       EL524
01033                                                                   EL524
01034      IF  DTE-OPT-RESERVE-METHOD-AUTH                              EL524
01035          MOVE SPACES             TO WS-T2-IBNR-RESERVES-X         EL524
01036      ELSE                                                         EL524
01037          MOVE WS-CARR-IBNR (CARRIER-INDEX)                        EL524
01038                                  TO WS-T2-IBNR-RESERVES.          EL524
01039                                                                   EL524
01040      MOVE WS-TOTAL-LINE2         TO  PRT.                         EL524
01041      PERFORM WRITE-A-LINE.                                        EL524
01042                                                                   EL524
01043      MOVE SPACES                 TO  WS-TOTAL-LINE2.              EL524
01044                                                                   EL524
01045      MOVE 'PREVIOUS'             TO  WS-TD2-FILLER1.              EL524
01046      MOVE AH-OVERRIDE-L6         TO  WS-TD2-LF-AH.                EL524
01047      MOVE WS-TOTAL2-DESCRIPTION  TO  WS-T2-DESCRIPTION.           EL524
01048                                                                   EL524
01049      MOVE WS-CARR-AH-PREV-AMOUNT (CARRIER-INDEX)                  EL524
01050                                  TO  WS-T2-AMOUNT.                EL524
01051      MOVE WS-CARR-AH-PREV-PMT-COUNT (CARRIER-INDEX)               EL524
01052                                  TO  WS-T2-COUNT.                 EL524
01053                                                                   EL524
01054      MOVE WS-TOTAL-LINE2         TO  PRT.                         EL524
01055      PERFORM WRITE-A-LINE.                                        EL524
01056                                                                   EL524
022403     MOVE 'CURRENT'              TO  WS-TD2-FILLER1.              EL524
022403     MOVE 'IU    '               TO  WS-TD2-LF-AH.                EL524
022403     MOVE WS-TOTAL2-DESCRIPTION  TO  WS-T2-DESCRIPTION.           EL524
022403                                                                  EL524
022403     MOVE WS-CARR-IU-CURR-AMOUNT (CARRIER-INDEX)                  EL524
022403                                 TO  WS-T2-AMOUNT.                EL524
022403     MOVE WS-CARR-IU-CURR-PMT-COUNT (CARRIER-INDEX)               EL524
022403                                 TO  WS-T2-COUNT.                 EL524

022403     MOVE WS-TOTAL-LINE2         TO  PRT.                         EL524
022403     PERFORM WRITE-A-LINE.                                        EL524
022403                                                                  EL524
022403     MOVE SPACES                 TO  WS-TOTAL-LINE2.              EL524
022403                                                                  EL524
022403     MOVE 'PREVIOUS'             TO  WS-TD2-FILLER1.              EL524
022403     MOVE '  IU  '               TO  WS-TD2-LF-AH.                EL524
022403     MOVE WS-TOTAL2-DESCRIPTION  TO  WS-T2-DESCRIPTION.           EL524
022403                                                                  EL524
022403     MOVE WS-CARR-IU-PREV-AMOUNT (CARRIER-INDEX)                  EL524
022403                                 TO  WS-T2-AMOUNT.                EL524
022403     MOVE WS-CARR-IU-PREV-PMT-COUNT (CARRIER-INDEX)               EL524
022403                                 TO  WS-T2-COUNT.                 EL524
022403                                                                  EL524
022403     MOVE WS-TOTAL-LINE2         TO  PRT.                         EL524
022403     PERFORM WRITE-A-LINE.                                        EL524
01056                                                                   EL524
022403     MOVE 'CURRENT'              TO  WS-TD2-FILLER1.              EL524
022403     MOVE ' GAP  '               TO  WS-TD2-LF-AH.                EL524
022403     MOVE WS-TOTAL2-DESCRIPTION  TO  WS-T2-DESCRIPTION.           EL524
022403                                                                  EL524
022403     MOVE WS-CARR-GP-CURR-AMOUNT (CARRIER-INDEX)                  EL524
022403                                 TO  WS-T2-AMOUNT.                EL524
022403     MOVE WS-CARR-GP-CURR-PMT-COUNT (CARRIER-INDEX)               EL524
022403                                 TO  WS-T2-COUNT.                 EL524

022403     MOVE WS-TOTAL-LINE2         TO  PRT.                         EL524
022403     PERFORM WRITE-A-LINE.                                        EL524
022403                                                                  EL524
022403     MOVE SPACES                 TO  WS-TOTAL-LINE2.              EL524
022403                                                                  EL524
022403     MOVE 'PREVIOUS'             TO  WS-TD2-FILLER1.              EL524
022403     MOVE ' GAP  '               TO  WS-TD2-LF-AH.                EL524
022403     MOVE WS-TOTAL2-DESCRIPTION  TO  WS-T2-DESCRIPTION.           EL524
022403                                                                  EL524
022403     MOVE WS-CARR-GP-PREV-AMOUNT (CARRIER-INDEX)                  EL524
022403                                 TO  WS-T2-AMOUNT.                EL524
022403     MOVE WS-CARR-GP-PREV-PMT-COUNT (CARRIER-INDEX)               EL524
022403                                 TO  WS-T2-COUNT.                 EL524
022403                                                                  EL524
022403     MOVE WS-TOTAL-LINE2         TO  PRT.                         EL524
022403     PERFORM WRITE-A-LINE.                                        EL524
052614
052614     MOVE 'CURRENT'              TO  WS-TD2-FILLER1.
052614     MOVE ' FAM  '               TO  WS-TD2-LF-AH.
052614     MOVE WS-TOTAL2-DESCRIPTION  TO  WS-T2-DESCRIPTION.
052614
052614     MOVE WS-CARR-FL-CURR-AMOUNT (CARRIER-INDEX)
052614                                 TO  WS-T2-AMOUNT.
052614     MOVE WS-CARR-FL-CURR-PMT-COUNT (CARRIER-INDEX)
052614                                 TO  WS-T2-COUNT.
052614
052614     MOVE WS-TOTAL-LINE2         TO  PRT.
052614     PERFORM WRITE-A-LINE.
052614
052614     MOVE SPACES                 TO  WS-TOTAL-LINE2.
052614
052614     MOVE 'PREVIOUS'             TO  WS-TD2-FILLER1.
052614     MOVE ' FAM  '               TO  WS-TD2-LF-AH.
052614     MOVE WS-TOTAL2-DESCRIPTION  TO  WS-T2-DESCRIPTION.
052614
052614     MOVE WS-CARR-FL-PREV-AMOUNT (CARRIER-INDEX)
052614                                 TO  WS-T2-AMOUNT.
052614     MOVE WS-CARR-FL-PREV-PMT-COUNT (CARRIER-INDEX)
052614                                 TO  WS-T2-COUNT.
052614
052614     MOVE WS-TOTAL-LINE2         TO  PRT.
052614     PERFORM WRITE-A-LINE.

022122     MOVE 'CURRENT'              TO  WS-TD2-FILLER1.
022122     MOVE ' BRV  '               TO  WS-TD2-LF-AH.
022122     MOVE WS-TOTAL2-DESCRIPTION  TO  WS-T2-DESCRIPTION.
022122
022122     MOVE WS-CARR-BR-CURR-AMOUNT (CARRIER-INDEX)
022122                                 TO  WS-T2-AMOUNT.
022122     MOVE WS-CARR-BR-CURR-PMT-COUNT (CARRIER-INDEX)
022122                                 TO  WS-T2-COUNT.
022122
022122     MOVE WS-TOTAL-LINE2         TO  PRT.
022122     PERFORM WRITE-A-LINE.
022122
022122     MOVE SPACES                 TO  WS-TOTAL-LINE2.
022122
022122     MOVE 'PREVIOUS'             TO  WS-TD2-FILLER1.
022122     MOVE ' BRV  '               TO  WS-TD2-LF-AH.
022122     MOVE WS-TOTAL2-DESCRIPTION  TO  WS-T2-DESCRIPTION.
022122
022122     MOVE WS-CARR-BR-PREV-AMOUNT (CARRIER-INDEX)
022122                                 TO  WS-T2-AMOUNT.
022122     MOVE WS-CARR-BR-PREV-PMT-COUNT (CARRIER-INDEX)
022122                                 TO  WS-T2-COUNT.
022122
022122     MOVE WS-TOTAL-LINE2         TO  PRT.
022122     PERFORM WRITE-A-LINE.

022122
022122     MOVE 'CURRENT'              TO  WS-TD2-FILLER1.
022122     MOVE ' HOS  '               TO  WS-TD2-LF-AH.
022122     MOVE WS-TOTAL2-DESCRIPTION  TO  WS-T2-DESCRIPTION.
022122
022122     MOVE WS-CARR-HS-CURR-AMOUNT (CARRIER-INDEX)
022122                                 TO  WS-T2-AMOUNT.
022122     MOVE WS-CARR-HS-CURR-PMT-COUNT (CARRIER-INDEX)
022122                                 TO  WS-T2-COUNT.
022122
022122     MOVE WS-TOTAL-LINE2         TO  PRT.
022122     PERFORM WRITE-A-LINE.
022122
022122     MOVE SPACES                 TO  WS-TOTAL-LINE2.
022122
022122     MOVE 'PREVIOUS'             TO  WS-TD2-FILLER1.
022122     MOVE ' HOS  '               TO  WS-TD2-LF-AH.
022122     MOVE WS-TOTAL2-DESCRIPTION  TO  WS-T2-DESCRIPTION.
022122
022122     MOVE WS-CARR-HS-PREV-AMOUNT (CARRIER-INDEX)
022122                                 TO  WS-T2-AMOUNT.
022122     MOVE WS-CARR-HS-PREV-PMT-COUNT (CARRIER-INDEX)
022122                                 TO  WS-T2-COUNT.
022122
022122     MOVE WS-TOTAL-LINE2         TO  PRT.
022122     PERFORM WRITE-A-LINE.
100518
100518     MOVE 'CURRENT'              TO  WS-TD2-FILLER1.
100518     MOVE ' OTH  '               TO  WS-TD2-LF-AH.
100518     MOVE WS-TOTAL2-DESCRIPTION  TO  WS-T2-DESCRIPTION.
100518
100518     MOVE WS-CARR-OT-CURR-AMOUNT (CARRIER-INDEX)
100518                                 TO  WS-T2-AMOUNT.
100518     MOVE WS-CARR-OT-CURR-PMT-COUNT (CARRIER-INDEX)
100518                                 TO  WS-T2-COUNT.
100518
100518     MOVE WS-TOTAL-LINE2         TO  PRT.
100518     PERFORM WRITE-A-LINE.
100518
100518     MOVE SPACES                 TO  WS-TOTAL-LINE2.
100518
100518     MOVE 'PREVIOUS'             TO  WS-TD2-FILLER1.
100518     MOVE ' OTH  '               TO  WS-TD2-LF-AH.
100518     MOVE WS-TOTAL2-DESCRIPTION  TO  WS-T2-DESCRIPTION.
100518
100518     MOVE WS-CARR-OT-PREV-AMOUNT (CARRIER-INDEX)
100518                                 TO  WS-T2-AMOUNT.
100518     MOVE WS-CARR-OT-PREV-PMT-COUNT (CARRIER-INDEX)
100518                                 TO  WS-T2-COUNT.
100518
100518     MOVE WS-TOTAL-LINE2         TO  PRT.
100518     PERFORM WRITE-A-LINE.
01056                                                                   EL524
01057      MOVE 'CURRENT'              TO  WS-TD2-FILLER1.              EL524
01058      MOVE LIFE-OVERRIDE-L6       TO  WS-TD2-LF-AH.                EL524
01059      MOVE WS-TOTAL2-DESCRIPTION  TO  WS-T2-DESCRIPTION.           EL524
01060                                                                   EL524
01061      MOVE WS-CARR-LIFE-CURR-AMOUNT (CARRIER-INDEX)                EL524
01062                                  TO  WS-T2-AMOUNT.                EL524
01063      MOVE WS-CARR-LIFE-CURR-PMT-COUNT (CARRIER-INDEX)             EL524
01064                                  TO  WS-T2-COUNT.                 EL524
01065      MOVE WS-TOTAL-LINE2         TO  PRT.                         EL524
01066      PERFORM WRITE-A-LINE.                                        EL524
01067                                                                   EL524
01068      MOVE 'PREVIOUS'             TO  WS-TD2-FILLER1.              EL524
01069      MOVE LIFE-OVERRIDE-L6       TO  WS-TD2-LF-AH.                EL524
01070      MOVE WS-TOTAL2-DESCRIPTION  TO  WS-T2-DESCRIPTION.           EL524
01071                                                                   EL524
01072      MOVE WS-CARR-LIFE-PREV-AMOUNT (CARRIER-INDEX)                EL524
01073                                  TO  WS-T2-AMOUNT.                EL524
01074      MOVE WS-CARR-LIFE-PREV-PMT-COUNT (CARRIER-INDEX)             EL524
01075                                  TO  WS-T2-COUNT.                 EL524
01076                                                                   EL524
01077      MOVE WS-TOTAL-LINE2         TO  PRT.                         EL524
01078      PERFORM WRITE-A-LINE.                                        EL524
01079                                                                   EL524
01080      IF CARRIER-INDEX LESS THAN CARRIER-INDEX-MAX                 EL524
01081          SET CARRIER-INDEX UP BY +1                               EL524
01082          GO TO 1600-CARRIER-TOTALS.                               EL524
01083                                                                   EL524
01084  1750-CLOSE-FILES.                                                EL524
01085      PERFORM CLOSE-FILES.                                         EL524
01086                                                                   EL524
01087  1760-STOP-RUN.                                                   EL524

070714     OPEN I-O ERMEBL.                                             EL524
070714     IF ERMEBL-FILE-STATUS  = '00' OR '97'                        EL524
070714         NEXT SENTENCE                                            EL524
070714     ELSE     
070714         MOVE 'N' TO ME-UPDATE-FLAG
070714     END-IF.
070714     MOVE DTE-CLIENT             TO ME-COMPANY.                   EL524
070714     COMPUTE MONTH-END-MOYR = (RUN-CCYY * 12) + RUN-MO.              CL**7
070714     MOVE MONTH-END-MOYR         TO ME-MOYR.                      EL524
070714                                                                  EL524
070714     IF ME-DO-UPDATE                                              EL524
070714         READ ERMEBL INVALID KEY                                  EL524
070714         MOVE 'N' TO ME-UPDATE-FLAG                               EL524
070714         CLOSE ERMEBL
070714     END-IF.
070714                                                                  EL524
070714     IF ME-DO-UPDATE                                              EL524
070714        move hld-524-CLMS-L      to me-524-CLMS-L     
070714        move hld-524-CLMS-AH     to me-524-CLMS-AH    
070714        move hld-524-RESV-L      to me-524-RESV-L     
070714        move hld-524-RESV-AH     to me-524-RESV-AH    
070714        move hld-524-CLMS-TOT-CM to me-524-CLMS-TOT-CM
122018        display ' me rewrite 524 tot cm ' me-524-CLMS-TOT-CM

070714        MOVE ME-START-TIME       TO ME-524-START           
070714        MOVE ME-CNDS-DATE        TO ME-524-RUN-DT           
070714        ACCEPT WS-TIME-OF-DAY   FROM  TIME                 
070714        MOVE WS-TIME             TO ME-524-END              
070714        ADD 1                    TO ME-524-RUN-CT           
070714        REWRITE MONTH-END-BALANCES                         
070714        display ' me rewrite ' ermebl-file-status
070714        CLOSE ERMEBL.
01096                                                                   EL524
01097      GOBACK.                                                      EL524
01098                                                                   EL524
01099  EJECT                                                            EL524
01100  2000-CREATE-CLAIM-TRANSACTION SECTION.                           EL524
01101      MOVE SPACES                 TO  PENDING-CLAIMS.              EL524
01102                                                                   EL524
01103      MOVE 'PC'                   TO  PC-RECORD-ID.                EL524
01104      MOVE EX-COMPANY-CD          TO  PC-COMPANY-CD.               EL524
01105      MOVE EX-AA-CARRIER          TO  PC-SV-CARRIER.               EL524
01106      MOVE EX-AA-GROUPING         TO  PC-SV-GROUPING.              EL524
01107      MOVE EX-AA-STATE            TO  PC-SV-STATE.                 EL524
01108      MOVE EX-AA-ACCOUNT          TO  PC-ACCOUNT.                  EL524
01109                                                                   EL524
01110      IF DTE-COMP-VG = '1'                                         EL524
01111          MOVE EX-AA-CARRIER            TO  PC-CARRIER             EL524
01112          MOVE EX-AA-GROUPING           TO  PC-GROUPING            EL524
01113          MOVE EX-AA-STATE              TO  PC-STATE               EL524
01114      ELSE                                                         EL524
01115          IF DTE-COMP-VG = '2'                                     EL524
01116              MOVE EX-AA-CARRIER        TO  PC-CARRIER             EL524
01117              MOVE EX-AA-STATE          TO  PC-STATE               EL524
01118          ELSE                                                     EL524
01119              IF DTE-COMP-VG = '4'                                 EL524
01120                  MOVE EX-AA-CARRIER    TO  PC-CARRIER             EL524
01121              ELSE                                                 EL524
01122                  IF DTE-COMP-VG = ' '                             EL524
01123                      MOVE EX-AA-STATE  TO  PC-STATE.              EL524
01124                                                                   EL524
01125      MOVE EX-AA-CERT-EFF-DT      TO  PC-CERT-EFF-DT.              EL524
01126      MOVE WS-RECORD-SEQUENCE     TO  PC-RECORD-SEQUENCE.          EL524
01127      ADD +1                      TO  WS-RECORD-SEQUENCE.          EL524
01128                                                                   EL524
01129      MOVE BIN-RUN-DATE           TO  PC-LAST-MAINT-DT                CL**7
01130                                      PC-INPUT-DT                  EL524
01131                                      PC-CREDIT-SELECT-DT.         EL524
01132                                                                   EL524
01133      MOVE EX-COMPANY-ID          TO  PC-LAST-MAINT-BY             EL524
01134                                      PC-COMPANY-ID.               EL524
01135                                                                   EL524
01136      ACCEPT WS-TIME-OF-DAY       FROM  TIME.                      EL524
01137                                                                   EL524
01138      MOVE WS-TIME                TO  PC-LAST-MAINT-HHMMSS.        EL524
01139                                                                   EL524
01140      MOVE ZEROS                  TO  PC-CC-INSURED-AGE            EL524
01141                                      PC-CC-ORIG-TERM              EL524
01142                                      PC-CC-LF-BENEFIT-CD          EL524
01143                                      PC-CC-LIFE-BENEFIT-AMT       EL524
01144                                      PC-CC-ALT-LF-BENEFIT-AMT     EL524
01145                                      PC-CC-LIFE-PREMIUM           EL524
01146                                      PC-CC-AH-BENEFIT-CD          EL524
01147                                      PC-CC-AH-BENEFIT-AMT         EL524
01148                                      PC-CC-AH-PREMIUM-AMT         EL524
01149                                      PC-CC-PAY-FREQUENCY          EL524
01150                                      PC-CC-LOAN-APR               EL524
01151                                      PC-CC-CAPPED-TERM            EL524
01152                                      PC-CC-PRIOR-LUMP-PMT         EL524
01153                                      PC-CC-PRIOR-DEATH-AMT        EL524
01154                                      PC-REMAINING-BENEFIT         EL524
01155                                      PC-REMAINING-TERM.           EL524
01156                                                                   EL524
01157      MOVE LOW-VALUES             TO  PC-CC-CANCEL-DT              EL524
01158                                      PC-CC-DEATH-DT               EL524
01159                                      PC-CC-SETTLEMENT-DT          EL524
01160                                      PC-CREDIT-ACCEPT-DT.         EL524
01161                                                                   EL524
01162      IF EX-RECORD-TYPE NOT = 'A'                                  EL524
01163          GO TO 2010-PROCESS-PAYMENTS.                             EL524
01164                                                                   EL524
01165      MOVE EX-SA-CERT-NO          TO  PC-CERT-NO.                  EL524
01166      MOVE EX-SA-CLAIM-NO         TO  PC-CLAIM-NO.                 EL524
01167      MOVE '2'                    TO  PC-RECORD-TYPE.              EL524
01168                                                                   EL524
100518     IF  EX-AA-CLAIM-TYPE  = LIFE-OVERRIDE-L1 OR 'O'              EL524
01170          MOVE EX-AA-CERT-STATUS TO PC-CC-LF-POLICY-STATUS         EL524
01171      ELSE                                                         EL524
01172          MOVE EX-AA-CERT-STATUS TO PC-CC-AH-POLICY-STATUS.        EL524
01173                                                                   EL524
100518     IF  EX-AA-CLAIM-TYPE = LIFE-OVERRIDE-L1 OR 'O'               EL524
01175          IF  EX-AA-CLAIM-PREM-TYPE = '2'                          EL524
01176              MOVE '3'  TO  PC-CLAIM-TYPE                          EL524
01177          ELSE                                                     EL524
01178              MOVE '1'  TO  PC-CLAIM-TYPE                          EL524
01179      ELSE                                                         EL524
01180          IF  EX-AA-CLAIM-PREM-TYPE = '2'                          EL524
01181              MOVE '4'  TO  PC-CLAIM-TYPE                          EL524
01182          ELSE                                                     EL524
01183              MOVE '2'  TO  PC-CLAIM-TYPE.                         EL524
01184                                                                   EL524
01185      IF  DTE-OPT-RESERVE-METHOD-AUTH                              EL524
01186          MOVE EX-AA-PAY-CURRENT-RSV-OPT                           EL524
01187                                  TO PC-PTC-RESERVE-AMT            EL524
01188          MOVE EX-AA-FUTURE-RSV-OPT                                EL524
01189                                  TO PC-FUTURE-RESERVE-AMT         EL524
01190          MOVE EX-AA-MANUAL-RSV-OPT                                EL524
01191                                  TO PC-MANUAL-RESERVE-AMT         EL524
01192          MOVE ZEROS              TO PC-IBNR-RESERVE-AMT           EL524
01193      ELSE                                                         EL524
01194          MOVE EX-AA-PAY-CURRENT-RESERVE                           EL524
01195                                  TO PC-PTC-RESERVE-AMT            EL524
01196          MOVE EX-AA-FUTURE-RESERVE                                EL524
01197                                  TO PC-FUTURE-RESERVE-AMT         EL524
01198          MOVE EX-AA-MANUAL-RESERVE                                EL524
01199                                  TO PC-MANUAL-RESERVE-AMT         EL524
01200          MOVE EX-AA-IBNR-RESERVE TO PC-IBNR-RESERVE-AMT.          EL524
01201                                                                   EL524
01202      MOVE EX-AA-INCURRED-DT      TO  PC-INCURRED-DT.              EL524
01203      MOVE EX-AA-REPORTED-DT      TO  PC-REPORTED-DT.              EL524
01204      MOVE EX-AA-PAID-THRU-DT     TO  PC-PAID-THRU-DT.             EL524
01205      MOVE EX-AA-REMAINING-TERM   TO  PC-REMAINING-TERM
           MOVE EX-AA-REMAINING-BENEFIT TO PC-REMAINING-BENEFIT
01206                                                                   EL524
01207      MOVE ZEROS                  TO  PC-NO-OF-DAYS-PAID           EL524
01208                                      PC-CLAIM-PAYMENT             EL524
01209                                      PC-AGE-AT-CLAIM.             EL524
01210                                                                   EL524
01211      MOVE LOW-VALUES             TO  PC-PAYMENT-DT.               EL524
01212                                                                   EL524
01213      MOVE EX-COMPANY-CD          TO  CM-COMPANY-CD.               EL524
01214      MOVE EX-AA-CARRIER          TO  CM-CARRIER.                  EL524
01215      MOVE EX-AA-GROUPING         TO  CM-GROUPING.                 EL524
01216      MOVE EX-AA-STATE            TO  CM-STATE.                    EL524
01217      MOVE EX-AA-ACCOUNT          TO  CM-ACCOUNT.                  EL524
01218      MOVE EX-AA-CERT-EFF-DT      TO  CM-CERT-EFF-DT.              EL524
01219      MOVE EX-SA-CERT-NO          TO  CM-CERT-NO.                  EL524
01220                                                                   EL524
01221      READ ELCERT.                                                 EL524
01222                                                                   EL524
01223      IF  ELCERT-FILE-STATUS = '23'                                EL524
01224          MOVE 'E' TO PC-ERR-FLAG (43)                             EL524
01225          MOVE 'X' TO PC-FATAL-FLAG                                EL524
01226          GO TO 2005-CONTINUE                                      EL524
01227      ELSE                                                         EL524
01228          IF  ELCERT-FILE-STATUS NOT = ZEROS                       EL524
01229              MOVE 'ERROR OCCURRED READ - ELCERT' TO               EL524
01230                   WS-ABEND-MESSAGE                                EL524
01231              MOVE ELCERT-FILE-STATUS TO WS-ABEND-FILE-STATUS      EL524
01232              GO TO ABEND-PGM                                      EL524
01233          ELSE                                                     EL524
070105             MOVE CM-CLP-STATE TO PC-CC-CLP-STATE
01234              IF DTE-CLIENT IS EQUAL TO 'MON'                      EL524
01235                  NEXT SENTENCE                                    EL524
01236              ELSE                                                 EL524
01237                  IF  EX-AA-CLAIM-PREM-TYPE = '2' OR '3'           EL524
01238                      GO TO 2005-CONTINUE.                         EL524
01239                                                                   EL524
01240 *****CERT IN ERROR OR BEING RETURNED                              EL524
01241      IF CM-CREDIT-INTERFACE-SW-1 = '2' OR  '4'                    EL524
01242         MOVE 'E' TO PC-ERR-FLAG (67)                              EL524
01243         MOVE 'X' TO PC-FATAL-FLAG.                                EL524
01244                                                                   EL524
01245 *****CERT PURGED FROM OFFLINE                                     EL524
01246      IF CM-CREDIT-INTERFACE-SW-1 = '3'                            EL524
01247         MOVE 'E' TO PC-ERR-FLAG (70)                              EL524
01248         MOVE 'X' TO PC-FATAL-FLAG.                                EL524
01249                                                                   EL524
01250      IF CM-CLAIM-INTERFACE-SW = '2'                               EL524
01251         MOVE 'E'              TO PC-ERR-FLAG (59)                 EL524
01252         MOVE 'X'              TO PC-FATAL-FLAG.                   EL524
01253                                                                   EL524
01254      IF CM-ENTRY-DT GREATER THAN BIN-RUN-DATE                        CL**7
01255         MOVE 'E'              TO PC-ERR-FLAG (71)                 EL524
01256         MOVE 'X'              TO PC-FATAL-FLAG.                   EL524
01257                                                                   EL524
01258  2005-CONTINUE.                                                   EL524
01259                                                                   EL524
01260      GO TO 2015-CONTINUE.                                         EL524
01261                                                                   EL524
01262  2010-PROCESS-PAYMENTS.                                           EL524
01263      MOVE EX-AB-CERT-NO          TO  PC-CERT-NO.                  EL524
01264      MOVE EX-AB-CLAIM-NO         TO  PC-CLAIM-NO.                 EL524
01265      MOVE EX-AB-CHECK-NO         TO  PC-CHECK-NO.                 EL524
01266      INSPECT PC-CHECK-NO REPLACING LEADING ' ' BY '0'.            EL524
01267      MOVE '1'                    TO  PC-RECORD-TYPE.              EL524
01268                                                                   EL524
100518     IF  EX-AB-CLAIM-TYPE = LIFE-OVERRIDE-L1 OR 'O'               EL524
01270          MOVE EX-AB-CERT-STATUS TO PC-CC-LF-POLICY-STATUS         EL524
01271      ELSE                                                         EL524
01272          MOVE EX-AB-CERT-STATUS TO PC-CC-AH-POLICY-STATUS.        EL524
01273                                                                   EL524
100518     IF  EX-AB-CLAIM-TYPE = LIFE-OVERRIDE-L1 OR 'O'               EL524
01275          IF  EX-AB-CLAIM-PREM-TYPE = '2'                          EL524
01276              MOVE '3'            TO  PC-CLAIM-TYPE                EL524
01277          ELSE                                                     EL524
01278              MOVE '1'            TO  PC-CLAIM-TYPE                EL524
01279      ELSE                                                         EL524
01280          IF  EX-AB-CLAIM-PREM-TYPE = '2'                          EL524
01281              MOVE '4'            TO  PC-CLAIM-TYPE                EL524
01282          ELSE                                                     EL524
01283              MOVE '2'            TO  PC-CLAIM-TYPE.               EL524

01285      MOVE EX-AB-PAYMENT-TYPE     TO  PC-PAYMENT-TYPE
012518     if ex-ab-void-dt <> low-values
012518        move '9'                 to pc-payment-type
012518     end-if

01287      MOVE EX-AB-INCURRED-DT      TO  PC-INCURRED-DT.              EL524
01288      MOVE EX-AB-REPORTED-DT      TO  PC-REPORTED-DT.              EL524
01289      MOVE EX-AB-CHECK-WRITTEN-DT TO  PC-PAYMENT-DT.               EL524
01290      MOVE EX-AB-PAID-THRU-DT     TO  PC-PAID-THRU-DT.             EL524
01291      MOVE EX-AB-PAYMENT-AMOUNT   TO  PC-CLAIM-PAYMENT.            EL524
01292                                                                   EL524
01293      MOVE EX-AB-DAYS-IN-PERIOD   TO  PC-NO-OF-DAYS-PAID.          EL524
01294      MOVE EX-AB-INCURRED-AGE     TO  PC-AGE-AT-CLAIM.             EL524
01295      MOVE EX-AB-CAUSE-CD         TO  PC-CAUSE-CODE.               EL524
01296                                                                   EL524
01297      MOVE ZEROS                  TO  PC-FUTURE-RESERVE-AMT        EL524
01298                                      PC-IBNR-RESERVE-AMT          EL524
01299                                      PC-PTC-RESERVE-AMT           EL524
01300                                      PC-MANUAL-RESERVE-AMT.       EL524
01301                                                                   EL524
01302      MOVE EX-COMPANY-CD          TO CM-COMPANY-CD.                EL524
01303      MOVE EX-AB-CARRIER          TO CM-CARRIER.                   EL524
01304      MOVE EX-AB-GROUPING         TO CM-GROUPING.                  EL524
01305      MOVE EX-AB-STATE            TO CM-STATE.                     EL524
01306      MOVE EX-AB-ACCOUNT          TO CM-ACCOUNT.                   EL524
01307      MOVE EX-AB-CERT-EFF-DT      TO CM-CERT-EFF-DT.               EL524
01308      MOVE EX-AB-CERT-NO          TO CM-CERT-NO.                   EL524
01309      MOVE EX-AB-TRAILER-SEQ-NO   TO PC-TRLR-SEQ-NO.               EL524
01310                                                                   EL524
01311      READ ELCERT.                                                 EL524
01312                                                                   EL524
01313      IF  ELCERT-FILE-STATUS = '23'                                EL524
01314          MOVE 'E' TO PC-ERR-FLAG (43)                             EL524
01315          MOVE 'X' TO PC-FATAL-FLAG                                EL524
01316          GO TO 2015-CONTINUE                                      EL524
01317      ELSE                                                         EL524
01318          IF  ELCERT-FILE-STATUS NOT = ZEROS                       EL524
01319              MOVE 'ERROR OCCURRED READ - ELCERT' TO               EL524
01320                   WS-ABEND-MESSAGE                                EL524
01321              MOVE ELCERT-FILE-STATUS TO WS-ABEND-FILE-STATUS      EL524
01322              GO TO ABEND-PGM                                      EL524
01323          ELSE                                                     EL524
070105             MOVE CM-CLP-STATE TO PC-CC-CLP-STATE
01324              IF DTE-CLIENT IS EQUAL TO 'MON'                      EL524
01325                  NEXT SENTENCE                                    EL524
01326              ELSE                                                 EL524
01327                  IF (EX-AB-CLAIM-PREM-TYPE = '2' OR '3') AND      EL524
01328                     (CM-PREMIUM-TYPE NOT = '1')                   EL524
01329                      GO TO 2015-CONTINUE.                         EL524
01330                                                                   EL524
01331 *****CERT IN ERROR OR BEING RETURNED                              EL524
01332      IF CM-CREDIT-INTERFACE-SW-1 = '2' OR '4'                     EL524
01333         MOVE 'E' TO PC-ERR-FLAG (67)                              EL524
01334         MOVE 'X' TO PC-FATAL-FLAG.                                EL524
01335                                                                   EL524
01336 *****CERT PURGED FROM OFFLINE                                     EL524
01337      IF CM-CREDIT-INTERFACE-SW-1 = '3'                            EL524
01338         MOVE 'E' TO PC-ERR-FLAG (70)                              EL524
01339         MOVE 'X' TO PC-FATAL-FLAG.                                EL524
01340                                                                   EL524
01341      IF CM-CLAIM-INTERFACE-SW = '2'                               EL524
01342         MOVE 'E'              TO PC-ERR-FLAG (59)                 EL524
01343         MOVE 'X'              TO PC-FATAL-FLAG.                   EL524
01344                                                                   EL524
01345      IF CM-ENTRY-DT GREATER THAN BIN-RUN-DATE                        CL**7
01346         MOVE 'E'              TO PC-ERR-FLAG (71)                 EL524
01347         MOVE 'X'              TO PC-FATAL-FLAG.                   EL524
01348                                                                   EL524
01349  2015-CONTINUE.                                                   EL524
01350                                                                   EL524
01351 ******** CHECK IF COVERAGE EXISTS ON CERT                         EL524
01352 ******** IF NOT, DO NOT PROCESS PAYMENT                           EL524
01353                                                                   EL524
01354      IF EX-RECORD-TYPE EQUAL 'B'                                  EL524
100518        IF (EX-AB-CLAIM-TYPE = LIFE-OVERRIDE-L1 OR 'O')  AND      EL524
01356            (CM-LF-BENEFIT-CD = '00')                              EL524
01357            MOVE 'E'              TO PC-ERR-FLAG (18)              EL524
01358            MOVE 'X'              TO PC-FATAL-FLAG                 EL524
01359         ELSE                                                      EL524
052614           IF (EX-AB-CLAIM-TYPE = AH-OVERRIDE-L1 OR 'I' OR 'G'
022122               OR 'F' OR 'B' OR 'H')
01361               AND (CM-AH-BENEFIT-CD = '00')
01362               MOVE 'E'           TO PC-ERR-FLAG (24)              EL524
01363               MOVE 'X'           TO PC-FATAL-FLAG                 EL524
01364            ELSE                                                   EL524
01365               NEXT SENTENCE                                       EL524
01366      ELSE                                                         EL524
100518        IF (EX-AA-CLAIM-TYPE = LIFE-OVERRIDE-L1 OR 'O')  AND      EL524
01368            (CM-LF-BENEFIT-CD = '00')                              EL524
01369            MOVE 'E'              TO PC-ERR-FLAG (18)              EL524
01370            MOVE 'X'              TO PC-FATAL-FLAG                 EL524
01371         ELSE                                                      EL524
052614           IF (EX-AA-CLAIM-TYPE = AH-OVERRIDE-L1 OR 'I' OR 'G'
022122               OR 'F' OR 'B' OR 'H')
01373               AND (CM-AH-BENEFIT-CD = '00')
01374               MOVE 'E'           TO PC-ERR-FLAG (24)              EL524
01375               MOVE 'X'           TO PC-FATAL-FLAG.                EL524
01376                                                                   EL524
01377      IF (CM-CLAIM-INTERFACE-SW = '2') AND                         EL524
01378         (PC-FATAL-FLAG NOT EQUAL 'X')                             EL524
01379         NEXT SENTENCE                                             EL524
01380      ELSE                                                         EL524
01381         GO TO 2025-END-ACCOUNT-READ.                              EL524
01382                                                                   EL524
01383      MOVE CM-CONTROL-PRIMARY TO AM-CONTROL-PRIMARY                EL524
01384      MOVE LOW-VALUES         TO AM-CNTRL-B                        EL524
01385      MOVE CM-CERT-EFF-DT     TO AM-EXPIRATION-DT                  EL524
01386      MOVE AM-CONTROL-A       TO WS-SAVE-ACCOUNT-KEY.              EL524
01387                                                                   EL524
01388      START ERACCT KEY IS NOT LESS THAN AM-CONTROL-PRIMARY         EL524
01389                                                                   EL524
01390      IF ERACCT-FILE-STATUS EQUAL '23' OR '10'                     EL524
01391         GO TO 2020-ACCOUNT-NOT-FOUND.                             EL524
01392                                                                   EL524
01393      IF ERACCT-FILE-STATUS NOT EQUAL '00'                         EL524
01394        MOVE  '*** EL524  ERACCT START ERROR - JOB WILL ABEND'     EL524
01395              TO WS-ABEND-MESSAGE                                  EL524
01396        MOVE ERACCT-FILE-STATUS TO WS-ABEND-FILE-STATUS            EL524
01397        GO TO ABEND-PGM.                                           EL524
01398                                                                   EL524
01399  2017-READ-ACCOUNT-NEXT.                                          EL524
01400                                                                   EL524
01401      READ ERACCT NEXT RECORD.                                     EL524
01402                                                                   EL524
01403      IF ERACCT-FILE-STATUS NOT EQUAL '00'                         EL524
01404        MOVE  '*** EL524  ERACCT READ ERROR - JOB WILL ABEND'      EL524
01405              TO WS-ABEND-MESSAGE                                  EL524
01406        MOVE ERACCT-FILE-STATUS TO WS-ABEND-FILE-STATUS            EL524
01407        GO TO ABEND-PGM.                                           EL524
01408                                                                   EL524
01409      IF AM-COMPANY-CD NOT EQUAL CM-COMPANY-CD                     EL524
01410         GO TO 2020-ACCOUNT-NOT-FOUND.                             EL524
01411                                                                   EL524
01412      IF AM-CONTROL-A NOT EQUAL WS-SAVE-ACCOUNT-KEY                EL524
01413         GO TO 2020-ACCOUNT-NOT-FOUND.                             EL524
01414                                                                   EL524
01415      IF (CM-CERT-EFF-DT LESS THAN AM-EXPIRATION-DT) AND           EL524
01416         (CM-CERT-EFF-DT NOT LESS THAN AM-EFFECTIVE-DT)            EL524
01417         GO TO 2025-END-ACCOUNT-READ.                              EL524
01418                                                                   EL524
01419      IF CM-CERT-EFF-DT EQUAL AM-EXPIRATION-DT                     EL524
01420         GO TO 2017-READ-ACCOUNT-NEXT.                             EL524
01421                                                                   EL524
01422  2020-ACCOUNT-NOT-FOUND.                                          EL524
01423                                                                   EL524
01424      MOVE 'E' TO PC-ERR-FLAG (51)                                 EL524
01425      MOVE 'X' TO PC-FATAL-FLAG.                                   EL524
01426                                                                   EL524
01427  2025-END-ACCOUNT-READ.                                           EL524
01428                                                                   EL524
01429      PERFORM 3000-WRITE-TRANSACTION.                              EL524
01430                                                                   EL524
01431      IF PC-FATAL-FLAG = 'X' OR                                    EL524
01432         CM-PREMIUM-TYPE EQUAL '1'                                 EL524
01433         GO TO 2090-EXIT.                                          EL524
01434                                                                   EL524
01435      IF DTE-CLIENT EQUAL 'CSL'           AND                      EL524
01436         CM-PREMIUM-TYPE EQUAL '2'        AND                      EL524
01437         CM-CLAIM-INTERFACE-SW EQUAL '2'  AND                      EL524
01438         CM-REIN-TABLE EQUAL 'RRR'                                 EL524
01439          GO TO 2090-EXIT.                                         EL524
01440                                                                   EL524
01441      MOVE SPACES                 TO  PENDING-BUSINESS.            EL524
01442                                                                   EL524
01443      MOVE 'PB'                   TO  PB-RECORD-ID.                EL524
01444      MOVE EX-COMPANY-CD          TO  PB-COMPANY-CD                EL524
01445                                      PB-COMPANY-CD-A1.            EL524
01446      MOVE WS-ISSUE-BATCH         TO  PB-ENTRY-BATCH.              EL524
01447      MOVE +1                     TO  PB-BATCH-SEQ-NO.             EL524
01448      MOVE ZEROS                  TO  PB-BATCH-CHG-SEQ-NO          EL524
01449                                      PB-ALT-CHG-SEQ-NO.           EL524
01450      MOVE EX-AB-CARRIER          TO  PB-SV-CARRIER.               EL524
01451      MOVE EX-AB-GROUPING         TO  PB-SV-GROUPING.              EL524
01452      MOVE EX-AB-STATE            TO  PB-SV-STATE.                 EL524
01453      MOVE EX-AB-ACCOUNT          TO  PB-ACCOUNT.                  EL524
01454                                                                   EL524
01455      IF DTE-COMP-VG = '1'                                         EL524
01456          MOVE EX-AB-CARRIER            TO  PB-CARRIER             EL524
01457          MOVE EX-AB-GROUPING           TO  PB-GROUPING            EL524
01458          MOVE EX-AB-STATE              TO  PB-STATE               EL524
01459      ELSE                                                         EL524
01460          IF DTE-COMP-VG = '2'                                     EL524
01461              MOVE EX-AB-CARRIER        TO  PB-CARRIER             EL524
01462              MOVE EX-AB-STATE          TO  PB-STATE               EL524
01463          ELSE                                                     EL524
01464              IF DTE-COMP-VG = '4'                                 EL524
01465                  MOVE EX-AB-CARRIER    TO  PB-CARRIER             EL524
01466              ELSE                                                 EL524
01467                  IF DTE-COMP-VG = ' '                             EL524
01468                      MOVE EX-AB-STATE  TO  PB-STATE.              EL524
01469                                                                   EL524
01470      MOVE EX-AB-CERT-EFF-DT      TO  PB-CERT-EFF-DT.              EL524
01471      MOVE CM-LF-LOAN-EXPIRE-DT   TO  PB-I-LF-EXPIRE-DT.           EL524
01472      MOVE CM-AH-LOAN-EXPIRE-DT   TO  PB-I-AH-EXPIRE-DT.           EL524
01473      MOVE CM-CERT-NO             TO  PB-CERT-NO.                  EL524
01474      MOVE 1                      TO  PB-RECORD-TYPE               EL524
01475                                      PB-CERT-ORIGIN.              EL524
01476      MOVE BIN-RUN-DATE           TO  PB-LAST-MAINT-DT                CL**7
01477                                      PB-INPUT-DT.                 EL524
01478      MOVE EX-COMPANY-ID          TO  PB-LAST-MAINT-BY             EL524
01479                                      PB-COMPANY-ID                EL524
01480                                      PB-INPUT-BY.                 EL524
01481                                                                   EL524
01482      ACCEPT WS-TIME-OF-DAY       FROM  TIME.                      EL524
01483                                                                   EL524
01484      MOVE WS-TIME                TO  PB-LAST-MAINT-HHMMSS.        EL524
01485                                                                   EL524
01486      MOVE ZEROS                  TO  PB-I-LOAN-TERM               EL524
01487                                      PB-I-LF-TERM                 EL524
01488                                      PB-I-AH-TERM                 EL524
01489                                      PB-I-PAY-FREQUENCY           EL524
01490                                      PB-I-NO-OF-PAYMENTS          EL524
01491                                      PB-I-LF-BENEFIT-CD           EL524
01492                                      PB-I-LF-INPUT-CD             EL524
01493                                      PB-I-LF-PREMIUM-AMT          EL524
01494                                      PB-I-LF-ALT-PREMIUM-AMT      EL524
01495                                      PB-I-LF-PREM-CALC            EL524
01496                                      PB-I-LF-ALT-PREM-CALC        EL524
01497                                      PB-I-LF-BENEFIT-AMT          EL524
01498                                      PB-I-LF-ALT-BENEFIT-AMT      EL524
01499                                      PB-I-AGE                     EL524
01500                                      PB-I-LF-RATE                 EL524
01501                                      PB-I-LF-ALT-RATE             EL524
01502                                      PB-I-LF-REI-RATE             EL524
01503                                      PB-I-LF-ALT-REI-RATE         EL524
01504                                      PB-I-LF-CRIT-PER             EL524
01505                                      PB-I-AH-CRIT-PER             EL524
01506                                      PB-I-AH-BENEFIT-CD           EL524
01507                                      PB-I-AH-INPUT-CD             EL524
01508                                      PB-I-AH-PREMIUM-AMT          EL524
01509                                      PB-I-AH-PREM-CALC            EL524
01510                                      PB-I-AH-RATE                 EL524
01511                                      PB-I-AH-REI-RATE             EL524
01512                                      PB-I-AH-RATE-TRM             EL524
01513                                      PB-I-AH-BENEFIT-AMT          EL524
01514                                      PB-I-RATE-DEV-PCT-LF         EL524
01515                                      PB-I-RATE-DEV-PCT-AH         EL524
01516                                      PB-I-BUSINESS-TYPE           EL524
01517                                      PB-I-LIFE-COMMISSION         EL524
01518                                      PB-I-JOINT-COMMISSION        EL524
01519                                      PB-I-JOINT-AGE               EL524
01520                                      PB-I-AH-COMMISSION           EL524
01521                                      PB-I-LOAN-APR                EL524
01522                                      PB-I-CURR-SEQ                EL524
01523                                      PB-I-EXTENTION-DAYS          EL524
01524                                      PB-I-TERM-IN-DAYS            EL524
01525                                      PB-I-LIVES                   EL524
01526                                      PB-I-NUM-BILLED              EL524
01527                                      PB-I-STATE-TAX               EL524
01528                                      PB-I-MUNI-TAX                EL524
01529                                      PB-CHG-COUNT                 EL524
01530                                      PB-LF-BILLED-AMTS            EL524
01531                                      PB-AH-BILLED-AMTS            EL524
01532                                      PB-CALC-TOLERANCE            EL524
01533                                      PB-NO-OF-ERRORS.             EL524
01534                                                                   EL524
01535      MOVE LOW-VALUES             TO  PB-CREDIT-ACCEPT-DT          EL524
01536                                      PB-BILLED-DT                 EL524
01537                                      PB-ACCT-EFF-DT               EL524
01538                                      PB-ACCT-EXP-DT               EL524
01539                                      PB-I-BIRTHDAY                EL524
01540                                      PB-I-LAST-ADD-ON-DT          EL524
01541                                      PB-CREDIT-SELECT-DT          EL524
01542                                      PB-COMMON-ERRORS.            EL524
01543                                                                   EL524
01544      MOVE CM-SPECIAL-REIN-CODE   TO  PB-I-SPECIAL-REIN-CODE.      EL524
01545      MOVE '2'                    TO  PB-I-INDV-GRP-CD.            EL524
01546      MOVE 'B'                    TO  PB-I-OB-FLAG.                EL524
01547      MOVE '5'                    TO  PB-I-ENTRY-STATUS.           EL524
01548      MOVE 'E'                    TO  PB-BATCH-ENTRY.              EL524
01549      MOVE CM-MEMBER-NO           TO  PB-I-MEMBER-NO.              EL524
01550      MOVE CM-INSURED-LAST-NAME   TO  PB-I-INSURED-LAST-NAME.      EL524
01551      MOVE CM-INSURED-FIRST-NAME  TO  PB-I-INSURED-FIRST-NAME.     EL524
01552      MOVE CM-INSURED-INITIAL2    TO  PB-I-INSURED-MIDDLE-INIT.    EL524
01553      MOVE CM-INSURED-ISSUE-AGE   TO  PB-I-AGE.                    EL524
01554                                                                   EL524
01555      IF CM-LF-PREMIUM-RATE NOT NUMERIC                            EL524
01556         MOVE +0 TO CM-LF-PREMIUM-RATE.                            EL524
01557                                                                   EL524
01558      IF CM-LF-ALT-PREMIUM-RATE NOT NUMERIC                        EL524
01559         MOVE +0 TO CM-LF-ALT-PREMIUM-RATE.                        EL524
01560                                                                   EL524
01561      IF CM-AH-PREMIUM-RATE NOT NUMERIC                            EL524
01562         MOVE +0 TO CM-AH-PREMIUM-RATE.                            EL524
01563                                                                   EL524
01564      IF CM-LF-BENEFIT-CD NOT EQUAL ZEROS                          EL524
01565         MOVE CM-LF-BENEFIT-CD TO PB-I-LF-INPUT-CD                 EL524
01566                                  PB-I-LF-BENEFIT-CD               EL524
01567         MOVE CM-LF-ORIG-TERM  TO PB-I-LF-TERM                     EL524
01568         MOVE CM-LF-BENEFIT-AMT TO PB-I-LF-BENEFIT-AMT             EL524
01569         MOVE CM-LF-PREMIUM-AMT TO PB-I-LF-PREMIUM-AMT             EL524
01570         MOVE CM-LF-PREMIUM-RATE TO PB-I-LF-RATE.                  EL524
01571         MOVE CM-LF-ALT-PREMIUM-RATE                               EL524
01572                                 TO PB-I-LF-ALT-RATE.              EL524
01573                                                                   EL524
01574      IF CM-AH-BENEFIT-CD NOT EQUAL ZEROS                          EL524
01575         MOVE CM-AH-BENEFIT-CD TO PB-I-AH-INPUT-CD                 EL524
01576                                  PB-I-AH-BENEFIT-CD               EL524
01577         MOVE CM-AH-ORIG-TERM  TO PB-I-AH-TERM                     EL524
01578         MOVE CM-AH-BENEFIT-AMT TO PB-I-AH-BENEFIT-AMT             EL524
01579         MOVE CM-AH-PREMIUM-AMT TO PB-I-AH-PREMIUM-AMT             EL524
01580         MOVE CM-AH-PREMIUM-RATE TO PB-I-AH-RATE.                  EL524
01581                                                                   EL524
01582      IF CM-LAST-ADD-ON-DT NOT = ZEROS  AND  SPACES                EL524
01583          MOVE CM-LAST-ADD-ON-DT TO PB-I-LAST-ADD-ON-DT.           EL524
01584                                                                   EL524
01585      MOVE PB-COMPANY-CD        TO PB-CSR-COMPANY-CD.              EL524
01586      MOVE PB-ENTRY-BATCH       TO PB-CSR-ENTRY-BATCH.             EL524
01587      MOVE AM-CSR-CODE          TO PB-CSR-ID.                      EL524
01588      MOVE PB-BATCH-SEQ-NO      TO PB-CSR-BATCH-SEQ-NO.            EL524
01589      MOVE +0                   TO PB-CSR-BATCH-CHG-SEQ-NO.        EL524
01590                                                                   EL524
01591      IF DTE-CLIENT EQUAL 'DMD'                                    EL524
01592         MOVE CM-LIVES          TO PB-I-LIVES                      EL524
01593         MOVE CM-RESIDENT-STATE TO PB-I-RESIDENT-STATE             EL524
01594         MOVE CM-RATE-CODE      TO PB-I-RATE-CODE                  EL524
01595         MOVE CM-BILLED         TO PB-I-NUM-BILLED.                EL524
01596                                                                   EL524
01597      MOVE PB-CONTROL-PRIMARY   TO PB-CONTROL-BY-ORIG-BATCH        EL524
01598                                                                   EL524
01599      WRITE PENDING-BUSINESS.                                      EL524
01600      IF ERPNDB-FILE-STATUS = '22'                                 EL524
01601          GO TO 2090-EXIT.                                         EL524
01602                                                                   EL524
01603      IF ERPNDB-FILE-STATUS NOT = ZEROS                            EL524
01604          MOVE  '*** EL524  ERPNDB WRITE ERROR - JOB WILL ABEND'   EL524
01605              TO WS-ABEND-MESSAGE                                  EL524
01606          MOVE ERPNDB-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL524
01607          GO TO ABEND-PGM.                                         EL524
01608                                                                   EL524
01609      MOVE SPACES                 TO  PENDING-BUSINESS.            EL524
01610                                                                   EL524
01611      MOVE 'PB'                   TO  PB-RECORD-ID.                EL524
01612      MOVE EX-COMPANY-CD          TO  PB-COMPANY-CD                EL524
01613                                      PB-COMPANY-CD-A1.            EL524
01614      MOVE WS-ISSUE-BATCH         TO  PB-ENTRY-BATCH               EL524
01615                                      PB-CERT-NO.                  EL524
01616      ADD +1                      TO  WS-BATCH-NUMBER.             EL524
01617      MOVE +9999                  TO  PB-BATCH-SEQ-NO.             EL524
01618      MOVE ZEROS                  TO  PB-BATCH-CHG-SEQ-NO          EL524
01619                                      PB-ALT-CHG-SEQ-NO.           EL524
01620                                                                   EL524
01621      MOVE EX-AB-CARRIER          TO  PB-SV-CARRIER.               EL524
01622      MOVE EX-AB-GROUPING         TO  PB-SV-GROUPING.              EL524
01623      MOVE EX-AB-STATE            TO  PB-SV-STATE.                 EL524
01624      MOVE EX-AB-ACCOUNT          TO  PB-ACCOUNT.                  EL524
01625                                                                   EL524
01626      IF DTE-COMP-VG = '1'                                         EL524
01627          MOVE EX-AB-CARRIER            TO  PB-CARRIER             EL524
01628          MOVE EX-AB-GROUPING           TO  PB-GROUPING            EL524
01629          MOVE EX-AB-STATE              TO  PB-STATE               EL524
01630      ELSE                                                         EL524
01631          IF DTE-COMP-VG = '2'                                     EL524
01632              MOVE EX-AB-CARRIER        TO  PB-CARRIER             EL524
01633              MOVE EX-AB-STATE          TO  PB-STATE               EL524
01634          ELSE                                                     EL524
01635              IF DTE-COMP-VG = '4'                                 EL524
01636                  MOVE EX-AB-CARRIER    TO  PB-CARRIER             EL524
01637              ELSE                                                 EL524
01638                  IF DTE-COMP-VG = ' '                             EL524
01639                      MOVE EX-AB-STATE  TO  PB-STATE.              EL524
01640                                                                   EL524
01641      MOVE HIGH-VALUES            TO  PB-CERT-EFF-DT.              EL524
01642      MOVE 9                      TO  PB-RECORD-TYPE.              EL524
01643      MOVE BIN-RUN-DATE           TO  PB-LAST-MAINT-DT                CL**7
01644                                      PB-INPUT-DT.                 EL524
01645      MOVE EX-COMPANY-ID          TO  PB-LAST-MAINT-BY             EL524
01646                                      PB-COMPANY-ID                EL524
01647                                      PB-INPUT-BY.                 EL524
01648                                                                   EL524
01649      ACCEPT WS-TIME-OF-DAY       FROM  TIME.                      EL524
01650                                                                   EL524
01651      MOVE WS-TIME                TO  PB-LAST-MAINT-HHMMSS.        EL524
01652                                                                   EL524
01653      MOVE ZEROS                  TO  PB-B-LF-ISS-PRM-REMITTED     EL524
01654                                      PB-B-LF-ISS-PRM-ENTERED      EL524
01655                                      PB-B-AH-ISS-PRM-REMITTED     EL524
01656                                      PB-B-AH-ISS-PRM-ENTERED      EL524
01657                                      PB-B-ISSUE-CNT-REMITTED      EL524
01658                                      PB-B-ISSUE-CNT-ENTERED       EL524
01659                                      PB-B-CANCEL-CNT-REMITTED     EL524
01660                                      PB-B-CANCEL-CNT-ENTERED      EL524
01661                                      PB-B-LF-CAN-PRM-REMITTED     EL524
01662                                      PB-B-LF-CAN-PRM-ENTERED      EL524
01663                                      PB-B-AH-CAN-PRM-REMITTED     EL524
01664                                      PB-B-AH-CAN-PRM-ENTERED      EL524
01665                                      PB-B-LF-ISS-PRM-COMPUTED     EL524
01666                                      PB-B-LF-CAN-PRM-COMPUTED     EL524
01667                                      PB-B-AH-ISS-PRM-COMPUTED     EL524
01668                                      PB-B-AH-CAN-PRM-COMPUTED     EL524
01669                                      PB-LF-BILLED-AMTS            EL524
01670                                      PB-AH-BILLED-AMTS            EL524
01671                                      PB-CHG-COUNT                 EL524
01672                                      PB-CALC-TOLERANCE            EL524
01673                                      PB-NO-OF-ERRORS.             EL524
01674                                                                   EL524
01675      MOVE LOW-VALUES             TO  PB-CREDIT-ACCEPT-DT          EL524
01676                                      PB-BILLED-DT                 EL524
01677                                      PB-ACCT-EFF-DT               EL524
01678                                      PB-ACCT-EXP-DT               EL524
01679                                      PB-I-LAST-ADD-ON-DT          EL524
01680                                      PB-COMMON-ERRORS             EL524
01681                                      PB-CREDIT-SELECT-DT.         EL524
01682                                                                   EL524
01683      MOVE PB-COMPANY-CD          TO  PB-CSR-COMPANY-CD.           EL524
01684      MOVE PB-ENTRY-BATCH         TO  PB-CSR-ENTRY-BATCH.          EL524
01685      MOVE AM-CSR-CODE            TO  PB-CSR-ID.                   EL524
01686      MOVE PB-BATCH-SEQ-NO        TO  PB-CSR-BATCH-SEQ-NO.         EL524
01687      MOVE +0                     TO  PB-CSR-BATCH-CHG-SEQ-NO.     EL524
01688                                                                   EL524
01689      MOVE PB-CONTROL-PRIMARY   TO PB-CONTROL-BY-ORIG-BATCH        EL524
01690                                                                   EL524
01691      WRITE PENDING-BUSINESS.                                      EL524
01692                                                                   EL524
01693      IF ERPNDB-FILE-STATUS = '22'                                 EL524
01694          GO TO 2090-EXIT.                                         EL524
01695                                                                   EL524
01696      IF ERPNDB-FILE-STATUS NOT = ZEROS                            EL524
01697          MOVE  '*** EL524  ERPNDB WRITE ERROR - JOB WILL ABEND'   EL524
01698              TO WS-ABEND-MESSAGE                                  EL524
01699          MOVE ERPNDB-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL524
01700          GO TO ABEND-PGM.                                         EL524
01701                                                                   EL524
01702  2090-EXIT.                                                       EL524
01703      EXIT.                                                        EL524
01704                                                                   EL524
01705  EJECT                                                            EL524
01706  3000-WRITE-TRANSACTION SECTION.                                  EL524
01707      WRITE PENDING-CLAIMS.                                        EL524
01708      ADD +1  TO  WS-TRANS-OUTPUT-COUNT.                           EL524
01709                                                                   EL524
01710      IF ERPNDC-FILE-STATUS NOT = ZEROS                            EL524
01711          MOVE  '*** EL524  ERPNDC WRITE ERROR - JOB WILL ABEND'   EL524
01712              TO WS-ABEND-MESSAGE                                  EL524
01713          MOVE ERPNDC-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL524
01714          GO TO ABEND-PGM.                                         EL524
01715                                                                   EL524
01716  3090-EXIT.                                                       EL524
01717      EXIT.                                                        EL524
01718                                                                   EL524
01719  EJECT                                                            EL524
01720  7000-DELETE-COMPANY-FROM-FILE SECTION.                           EL524
01721      MOVE SPACES                 TO PC-CONTROL-PRIMARY.           EL524
01722      MOVE DTE-CLASIC-COMPANY-CD  TO PC-COMPANY-CD.                EL524
01723                                                                   EL524
01724      START ERPNDC                                                 EL524
01725          KEY GREATER THAN PC-CONTROL-PRIMARY.                     EL524
01726                                                                   EL524
01727      IF ERPNDC-FILE-STATUS = '23'                                 EL524
01728          GO TO 7200-DELETE-CRPNDB.                                EL524
01729                                                                   EL524
01730      IF ERPNDC-FILE-STATUS NOT = ZEROS                            EL524
01731          MOVE  '*** EL524  ERPNDC START ERROR - JOB WILL ABEND'   EL524
01732              TO WS-ABEND-MESSAGE                                  EL524
01733          MOVE ERPNDC-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL524
01734          GO TO ABEND-PGM.                                         EL524
01735                                                                   EL524
01736  7100-READ-LOOP.                                                  EL524
01737      READ ERPNDC NEXT RECORD.                                     EL524
01738                                                                   EL524
01739      IF ERPNDC-FILE-STATUS = '10'                                 EL524
01740          GO TO 7200-DELETE-CRPNDB.                                EL524
01741                                                                   EL524
01742      IF ERPNDC-FILE-STATUS NOT = ZEROS                            EL524
01743          MOVE  '*** EL524  ERPNDC READ ERROR - JOB WILL ABEND'    EL524
01744              TO WS-ABEND-MESSAGE                                  EL524
01745          MOVE ERPNDC-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL524
01746          GO TO ABEND-PGM.                                         EL524
01747                                                                   EL524
01748      IF PC-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD                 EL524
01749          GO TO 7200-DELETE-CRPNDB.                                EL524
01750                                                                   EL524
01751      DELETE ERPNDC RECORD.                                        EL524
01752                                                                   EL524
01753      IF ERPNDC-FILE-STATUS NOT = ZEROS                            EL524
01754          MOVE  '*** EL524  ERPNDC DELETE ERROR - JOB WILL ABEND'  EL524
01755              TO WS-ABEND-MESSAGE                                  EL524
01756          MOVE ERPNDC-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL524
01757          GO TO ABEND-PGM.                                         EL524
01758                                                                   EL524
01759      GO TO 7100-READ-LOOP.                                        EL524
01760                                                                   EL524
01761  7200-DELETE-CRPNDB.                                              EL524
01762      MOVE SPACES                 TO PB-CONTROL-PRIMARY.           EL524
01763      MOVE DTE-CLASIC-COMPANY-CD  TO PB-COMPANY-CD.                EL524
01764      MOVE '#CL'                  TO PB-ENTRY-BATCH.               EL524
01765                                                                   EL524
01766      START ERPNDB                                                 EL524
01767          KEY GREATER THAN PB-CONTROL-PRIMARY.                     EL524
01768                                                                   EL524
01769      IF ERPNDB-FILE-STATUS = '23'                                 EL524
01770          GO TO 7900-EXIT.                                         EL524
01771                                                                   EL524
01772      IF ERPNDB-FILE-STATUS NOT = ZEROS                            EL524
01773          MOVE  '*** EL524  ERPNDB START ERROR - JOB WILL ABEND'   EL524
01774              TO WS-ABEND-MESSAGE                                  EL524
01775          MOVE ERPNDB-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL524
01776          GO TO ABEND-PGM.                                         EL524
01777                                                                   EL524
01778  7300-READ-LOOP.                                                  EL524
01779      READ ERPNDB NEXT RECORD.                                     EL524
01780                                                                   EL524
01781      IF ERPNDB-FILE-STATUS = '10'                                 EL524
01782          GO TO 7900-EXIT.                                         EL524
01783                                                                   EL524
01784      IF ERPNDB-FILE-STATUS NOT = ZEROS                            EL524
01785          MOVE '*** EL524  ERPNDB READ ERROR - JOB WILL ABEND'     EL524
01786              TO WS-ABEND-MESSAGE                                  EL524
01787          MOVE ERPNDB-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL524
01788          GO TO ABEND-PGM.                                         EL524
01789                                                                   EL524
01790      IF PB-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD                 EL524
01791          GO TO 7900-EXIT.                                         EL524
01792                                                                   EL524
01793      MOVE PB-ENTRY-BATCH         TO WS-COMP-BATCH.                EL524
01794                                                                   EL524
01795      IF WS-COMP-FIRST NOT = '#CL'                                 EL524
01796          GO TO 7900-EXIT.                                         EL524
01797                                                                   EL524
01798      DELETE ERPNDB RECORD.                                        EL524
01799                                                                   EL524
01800      IF ERPNDB-FILE-STATUS NOT = ZEROS                            EL524
01801          MOVE  '*** EL524  ERPNDB DELETE ERROR - JOB WILL ABEND'  EL524
01802              TO WS-ABEND-MESSAGE                                  EL524
01803          MOVE ERPNDB-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL524
01804          GO TO ABEND-PGM.                                         EL524
01805                                                                   EL524
01806      GO TO 7300-READ-LOOP.                                        EL524
01807                                                                   EL524
01808  7900-EXIT.                                                       EL524
01809      EXIT.                                                        EL524
01810  EJECT                                                            EL524
01811  8500-DATE-CONVERSION SECTION.   COPY ELCDCS.                     EL524
01812                                                                   EL524
01813                                                                   EL524
01814  EJECT                                                            EL524
01815                                                                   EL524
01816  WRITE-A-LINE SECTION.       COPY ELCWAL.                         EL524
01817                                                                   EL524
01818  WRITE-HEADINGS SECTION.                                          EL524
01819 ***************************************************************** EL524
01820 *                                                               * EL524
01821 *                            ELCWHS1.                           * EL524
01822 *                                                               * EL524
01823 *    THIS SECTION CONTROLS THE WRITING OF THE HEADINGS          * EL524
01824 *****************************************************************.EL524
01825  WHS-010.                                                         EL524
01826                                                                   EL524
01827      IF LCP-ONCTR-02 EQUAL +0                                     EL524
01828         ADD +1                   TO  LCP-ONCTR-02                 EL524
01829         MOVE WS-CURRENT-DATE     TO  WS-H2-DATE                   EL524
01830         MOVE COMPANY-NAME        TO  WS-H2-CLIENT-NAME            EL524
01831         MOVE ALPH-DATE           TO  WS-H3-DATE.                  EL524
01832                                                                   EL524
01833      ADD +1  TO  WS-PAGE.                                         EL524
01834      MOVE WS-PAGE                TO  WS-H3-PAGE.                  EL524
01835      MOVE PRT                    TO  WS-SAVE-PRINT-RECORD.        EL524
01836      MOVE ZERO                   TO  WS-LINE-COUNT.               EL524
01837                                                                   EL524
01838      MOVE WS-HEADING1            TO  PRT.                         EL524
01839      MOVE '1'                    TO  X.                           EL524
01840      PERFORM WRITE-PRINTER.                                       EL524
01841                                                                   EL524
01842      MOVE WS-HEADING2            TO  PRT.                         EL524
01843      MOVE ' '                    TO  X.                           EL524
01844      PERFORM WRITE-PRINTER.                                       EL524
01845                                                                   EL524
01846      MOVE WS-HEADING3            TO  PRT.                         EL524
01847      MOVE ' '                    TO  X.                           EL524
01848      PERFORM WRITE-PRINTER.                                       EL524
01849                                                                   EL524
01850      MOVE WS-HEADING4            TO  PRT.                         EL524
01851      MOVE ' '                    TO  X.                           EL524
01852      PERFORM WRITE-PRINTER.                                       EL524
01853                                                                   EL524
01854      MOVE WS-HEADING5            TO  PRT.                         EL524
01855      PERFORM WRITE-PRINTER.                                       EL524
01856                                                                   EL524
01857      MOVE WS-HEADING6            TO  PRT.                         EL524
01858      PERFORM WRITE-PRINTER.                                       EL524
01859                                                                   EL524
01860      MOVE +7                     TO  WS-LINE-COUNT.               EL524
01861                                                                   EL524
01862  WHS-020.                    COPY ELCWHS2.                        EL524
01863                                                                   EL524
01864                                                                   EL524
01865  WRITE-PRINTER SECTION.      COPY ELCWPS.                         EL524
01866                                                                   EL524
01867                              COPY ELCPRT2X.                       EL524
01868                                                                   EL524
01869  EJECT                                                            EL524
01870  OPEN-FILES SECTION.                                              EL524
01871  OFS-010.                                                         EL524
01873      OPEN INPUT ELCERT                                            EL524
01874                 ERACCT                                            EL524
01875           I-O   ERPNDC                                            EL524
01876                 ERPNDB                                            EL524
01877          OUTPUT PRNTR
062104                ME-EL524-BALANCE.
01878                                                                   EL524
01879      IF ELCERT-FILE-STATUS  = '00' OR '97'                        EL524
01880         NEXT SENTENCE                                             EL524
01881      ELSE                                                         EL524
01882         MOVE  '*** EL524  ELCERT OPEN ERROR - JOB WILL ABEND'     EL524
01883              TO WS-ABEND-MESSAGE                                  EL524
01884         MOVE ELCERT-FILE-STATUS TO WS-ABEND-FILE-STATUS           EL524
01885         GO TO ABEND-PGM.                                          EL524
01886                                                                   EL524
01887      IF ERACCT-FILE-STATUS  = '00' OR '97'                        EL524
01888         NEXT SENTENCE                                             EL524
01889      ELSE                                                         EL524
01890         MOVE  '*** EL524  ERACCT OPEN ERROR - JOB WILL ABEND'     EL524
01891              TO WS-ABEND-MESSAGE                                  EL524
01892         MOVE ERACCT-FILE-STATUS TO WS-ABEND-FILE-STATUS           EL524
01893         GO TO ABEND-PGM.                                          EL524
01894                                                                   EL524
01895      IF ERPNDC-FILE-STATUS  = '00' OR '97'                        EL524
01896          NEXT SENTENCE                                            EL524
01897        ELSE                                                       EL524
01898          MOVE  '*** EL524  ERPNDC OPEN ERROR - JOB WILL ABEND'    EL524
01899              TO WS-ABEND-MESSAGE                                  EL524
01900          MOVE ERPNDC-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL524
01901          GO TO ABEND-PGM.                                         EL524
01902                                                                   EL524
01903      IF ERPNDB-FILE-STATUS  = '00' OR '97'                        EL524
01904          NEXT SENTENCE                                            EL524
01905        ELSE                                                       EL524
01906          MOVE  '*** EL524  ERPNDB OPEN ERROR - JOB WILL ABEND'    EL524
01907              TO WS-ABEND-MESSAGE                                  EL524
01908          MOVE ERPNDB-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL524
01909          GO TO ABEND-PGM.                                         EL524
01910                                                                   EL524
01911  OFS-EXIT.                                                        EL524
01912      EXIT.                                                        EL524
01913                                                                   EL524
01914  EJECT                                                            EL524
01915  CLOSE-FILES SECTION.        COPY ELCPRTCX.                          CL**5
01916                                                                   EL524
01917  CFS-010.                                                         EL524
01918      CLOSE REPORTS-EXTRACT-FILE                                   EL524
01919            ERPNDC                                                 EL524
01920            ERPNDB                                                 EL524
01921            ELCERT                                                 EL524
01922            PRNTR
062104           ME-EL524-BALANCE.
01923                                                                   EL524
01924      IF ERPNDC-FILE-STATUS NOT = ZEROS                            EL524
01925          MOVE  '*** EL524  ERPNDC CLOSE ERROR - JOB WILL ABEND'   EL524
01926              TO WS-ABEND-MESSAGE                                  EL524
01927          MOVE ERPNDC-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL524
01928          GO TO ABEND-PGM.                                         EL524
01929                                                                   EL524
01930      IF ERPNDB-FILE-STATUS NOT = ZEROS                            EL524
01931          MOVE  '*** EL524  ERPNDB CLOSE ERROR - JOB WILL ABEND'   EL524
01932              TO WS-ABEND-MESSAGE                                  EL524
01933          MOVE ERPNDB-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL524
01934          GO TO ABEND-PGM.                                         EL524
01935                                                                   EL524
01936  CFS-EXIT.                                                        EL524
01937      EXIT.                                                        EL524
01938                                                                   EL524
01939  ABEND-PGM SECTION.          COPY ELCABEND.                          CL**5
