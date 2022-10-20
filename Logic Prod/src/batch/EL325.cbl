00001  IDENTIFICATION DIVISION.                                         04/18/98
00002                                                                   EL325
00003  PROGRAM-ID.                 EL325 .                                 LV003
00004 *              PROGRAM CONVERTED BY                               EL325
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL325
00006 *              CONVERSION DATE 02/14/96 13:41:22.                 EL325
00007 *           PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE             EL325
00008 *                            VMOD=2.014                           EL325
00009                                                                   EL325
00009                                                                   EL325
00010 *AUTHOR.     LOGIC, INC.                                          EL325
00011 *            DALLAS, TEXAS.                                       EL325
00015 *SECURITY.   *****************************************************EL325
00016 *            *                                                   *EL325
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *EL325
00018 *            *                                                   *EL325
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL325
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL325
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *EL325
00022 *            *                                                   *EL325
00023 *            *****************************************************EL325
00012                                                                   EL325
00025 *REMARKS.                                                         EL325
00026 *        THIS REGISTER DETAILS CLAIM PAYMENTS BY ACCOUNT.         EL325
00027 *    FREQUENTLY THIS REPORT WILL BE SENT TO THE ACCOUNT OR THE    EL325
00028 *    SERVICING GENERAL AGENT FOR REVIEW.  IF A YEAR-TO-DATE       EL325
00029 *    VERSION IS REQUIRED, THE EXTRACT PROGRAM EL310 - MUST BE RUN EL325
00030 *    WITH PROCESS OPTION 2.                                       EL325
122702******************************************************************
122702*                   C H A N G E   L O G
122702*
122702* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
122702*-----------------------------------------------------------------
122702*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
122702* EFFECTIVE    NUMBER
122702*-----------------------------------------------------------------
122702* 122702    2001061800003  SMVA  ADD PROCESSING FOR NEW CLM TYP I
122702*                                FOR DCC
121203* 121203    2003080800002  SMVA  ADD PROCESSING FOR NEW CLM TYP G
022106* 022106    2004040700004  PEMA  ADD LIFE CLAIM INTEREST
051810* 051810 CR2010042900001   PEMA  SPEARATE CITY AND STATE
052614* 052614    2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
070714* 070714  CR2013060600001  PEMA  AUTOMATE MONTH END BALANCING
010716* 010716  CR2015082500001  PEMA  VPP CHANGES
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
022122* 022122  CR2021100800003  PEMA  Add B and H claim types
122702******************************************************************
00031                                                                   EL325
00032      EJECT                                                        EL325
00033  ENVIRONMENT DIVISION.                                            EL325
00034                                                                   EL325
00035  INPUT-OUTPUT SECTION.                                            EL325
00036                                                                   EL325
00037  FILE-CONTROL.                                                    EL325
00038                                                                   EL325
00039      SELECT REPORTS-EXTRACT-FILE                                  EL325
00040          ASSIGN TO SYS010-UT-2400-S-SYS010.                       EL325
00041                                                                   EL325
00042      SELECT SORT-WORK-FILE                                        EL325
00043          ASSIGN TO SYS001-UT-FBA1-S-SORTWK1.                      EL325
00044                                                                   EL325
00045      SELECT DISK-DATE        ASSIGN TO SYS019-UT-FBA1-S-SYS019.   EL325
00046                                                                   EL325
00047      SELECT PRNTR            ASSIGN TO SYS008-UR-1403-S-SYS008.   EL325
00048                                                                   EL325
00049      SELECT FICH             ASSIGN TO SYS020-UT-2400-S-SYS020.   EL325
00050                                                                   EL325
00051      SELECT ERACCT           ASSIGN TO SYS018-FBA1-ERACCT2        EL325
00052                              ORGANIZATION IS INDEXED              EL325
00053                              ACCESS IS DYNAMIC                    EL325
00054                              RECORD KEY IS AM-CONTROL-BY-VAR-GRP  EL325
00055                              FILE STATUS IS AM-FILE-STATUS.       EL325
00056                                                                   EL325
00057      SELECT ELREPT           ASSIGN TO SYS018-FBA1-ELREPT         EL325
00058                              ORGANIZATION IS INDEXED              EL325
00059                              ACCESS IS DYNAMIC                    EL325
00060                              RECORD KEY IS RF-CONTROL-PRIMARY     EL325
00061                              FILE STATUS IS DTE-VSAM-FLAGS.       EL325
00062                                                                   EL325
00063      SELECT MPPROD           ASSIGN TO SYS021-FBA1-MPPROD2        EL325
00064                              ORGANIZATION IS INDEXED              EL325
00065                              ACCESS IS DYNAMIC                    EL325
00066                              RECORD KEY IS PD-CONTROL-BY-VAR-GRP  EL325
00067                              FILE STATUS IS PD-FILE-STATUS.       EL325
070714     SELECT  ERMEBL          ASSIGN SYS024-FBA1-ERMEBL      
070714                             ORGANIZATION INDEXED           
070714                             ACCESS DYNAMIC                 
070714                             RECORD KEY ME-CONTROL-PRIMARY  
070714                             FILE STATUS ERMEBL-FILE-STATUS.
00068      EJECT                                                        EL325
00069  DATA DIVISION.                                                   EL325
00070                                                                   EL325
00071  FILE SECTION.                                                    EL325
00072                                                                   EL325
00073  FD  REPORTS-EXTRACT-FILE COPY ELCEXTFD.                          EL325
00074                              COPY ELCEXTR.                        EL325
00075                                                                   EL325
00076      EJECT                                                        EL325
00077  SD  SORT-WORK-FILE.                                              EL325
00078                              COPY ELCEXTR REPLACING               EL325
00079      EX-RECORD-ID                BY  SWR-RECORD-ID                EL325
00080      VALID-EX-ID                 BY  VALID-SWR-ID                 EL325
00081                                                                   EL325
00082      EX-SORT-KEY-AREAS           BY  SWR-SORT-KEY-AREAS           EL325
00083      EX-POSITIONING-CODE         BY  SWR-POSITIONING-CODE         EL325
00084      EX-EXTRACT-CODE             BY  SWR-EXTRACT-CODE             EL325
00085      EX-COMPANY-CD               BY  SWR-COMPANY-CD               EL325
00086      EX-COMPANY-ID               BY  SWR-COMPANY-ID               EL325
00087      EX-RECORD-TYPE              BY  SWR-RECORD-TYPE              EL325
00088                                                                   EL325
00089      EX-SORT-KEY-B               BY  SWR-SORT-KEY-B               EL325
00090      EX-SB-CARRIER               BY  SWR-SB-CARRIER               EL325
00091      EX-SB-CHECK-NO              BY  SWR-SB-CHECK-NO              EL325
00092                                                                   EL325
00093      EX-EXTRACT-B-RECORD-A       BY  SWR-EXTRACT-B-RECORD-A       EL325
00094      EX-BA-CERT-NO               BY  SWR-BA-CERT-NO               EL325
00095      EX-BA-CLAIM-NO              BY  SWR-BA-CLAIM-NO              EL325
00096      EX-BA-STATE                 BY  SWR-BA-STATE                 EL325
00097      EX-BA-ACCOUNT               BY  SWR-BA-ACCOUNT               EL325
00098      EX-BA-ACCOUNT-NAME          BY  SWR-BA-ACCOUNT-NAME          EL325
00099      EX-BA-GROUPING              BY  SWR-BA-GROUPING              EL325
00100      EX-BA-CERT-EFF-DT           BY  SWR-BA-CERT-EFF-DT           EL325
00101      EX-BA-CLAIM-TYPE            BY  SWR-BA-CLAIM-TYPE            EL325
00102      EX-BA-CLAIM-PREM-TYPE       BY  SWR-BA-CLAIM-PREM-TYPE       EL325
00103      EX-BA-SINGLE-PREM           BY  SWR-BA-SINGLE-PREM           EL325
00104      EX-BA-O-B-COVERAGE          BY  SWR-BA-O-B-COVERAGE          EL325
00105      EX-BA-OPEN-END-COVERAGE     BY  SWR-BA-OPEN-END-COVERAGE     EL325
00106      EX-BA-PAYMENT-TYPE          BY  SWR-BA-PAYMENT-TYPE          EL325
00107      EX-BA-PARTIAL-PAYMENT       BY  SWR-BA-PARTIAL-PAYMENT       EL325
00108      EX-BA-FINAL-PAYMENT         BY  SWR-BA-FINAL-PAYMENT         EL325
00109      EX-BA-LUMP-SUM-PAYMENT      BY  SWR-BA-LUMP-SUM-PAYMENT      EL325
00110      EX-BA-ADDITIONAL-PAYMENT    BY  SWR-BA-ADDITIONAL-PAYMENT    EL325
00111      EX-BA-CHARGABLE-EXPENSE     BY  SWR-BA-CHARGABLE-EXPENSE     EL325
00112      EX-BA-NON-CHARGEABLE-EXPENSE BY SWR-BA-NON-CHARGEABLE-EXPENSEEL325
00113      EX-BA-LIFE-PREMIUM-REFUND   BY  SWR-BA-LIFE-PREMIUM-REFUND   EL325
00114      EX-BA-AH-PREMIUM-REFUND     BY  SWR-BA-AH-PREMIUM-REFUND     EL325
00115      EX-BA-PAID-FROM-DT          BY  SWR-BA-PAID-FROM-DT          EL325
00116      EX-BA-PAID-THRU-DT          BY  SWR-BA-PAID-THRU-DT          EL325
00117      EX-BA-INCURRED-DT           BY  SWR-BA-INCURRED-DT           EL325
00118      EX-BA-REPORTED-DT           BY  SWR-BA-REPORTED-DT           EL325
00119      EX-BA-CHECK-WRITTEN-DT      BY  SWR-BA-CHECK-WRITTEN-DT      EL325
00120      EX-BA-PAYMENT-AMOUNT        BY  SWR-BA-PAYMENT-AMOUNT        EL325
00121      EX-BA-INSURED-AGE           BY  SWR-BA-INSURED-AGE           EL325
00122      EX-BA-INSURED-LAST-NAME     BY  SWR-BA-INSURED-LAST-NAME     EL325
00123      EX-BA-INSURED-1ST-NAME      BY  SWR-BA-INSURED-1ST-NAME      EL325
00124      EX-BA-INSURED-MID-INIT      BY  SWR-BA-INSURED-MID-INIT      EL325
00125      EX-BA-DAYS-IN-PERIOD        BY  SWR-BA-DAYS-IN-PERIOD        EL325
00126      EX-BA-PAYEE-TYPE-CD         BY  SWR-BA-PAYEE-TYPE-CD         EL325
00127      EX-BA-INSURED-PAID          BY  SWR-BA-INSURED-PAID          EL325
00128      EX-BA-BENEFICIARY-PAID      BY  SWR-BA-BENEFICIARY-PAID      EL325
00129      EX-BA-ACCOUNT-PAID          BY  SWR-BA-ACCOUNT-PAID          EL325
00130      EX-BA-OTHER-1-PAID          BY  SWR-BA-OTHER-1-PAID          EL325
00131      EX-BA-OTHER-2-PAID          BY  SWR-BA-OTHER-2-PAID          EL325
00132      EX-BA-PAYEES-NAME           BY  SWR-BA-PAYEES-NAME           EL325
00133      EX-BA-VOID-DT               BY  SWR-BA-VOID-DT               EL325
00134      EX-BA-CLAIM-STATUS          BY  SWR-BA-CLAIM-STATUS          EL325
00135      EX-BA-ORIG-TERM             BY  SWR-BA-ORIG-TERM             EL325
00136      EX-BA-MEMBER-NUMBER         BY  SWR-BA-MEMBER-NUMBER         EL325
00137      EX-BA-SOC-SEC-NO            BY  SWR-BA-SOC-SEC-NO            EL325
00138      EX-BA-PAYMENT-ORIGIN        BY  SWR-BA-PAYMENT-ORIGIN        EL325
00139      EX-BA-ONLINE-MANUAL-PMT     BY  SWR-BA-ONLINE-MANUAL-PMT     EL325
00140      EX-BA-ONLINE-AUTO-PMT       BY  SWR-BA-ONLINE-AUTO-PMT       EL325
00141      EX-BA-OFFLINE-PMT           BY  SWR-BA-OFFLINE-PMT           EL325
00142      EX-BA-CERT-STATUS           BY  SWR-BA-CERT-STATUS           EL325
00143      EX-BA-SYSTEM-IDENTIFIER     BY  SWR-BA-SYSTEM-IDENTIFIER.    EL325
00144                                                                   EL325
00145      EJECT                                                        EL325
00146  FD  ERACCT.                                                      EL325
00147                              COPY ERCACCT.                        EL325
00148                                                                   EL325
00149  FD  DISK-DATE               COPY ELCDTEFD.                       EL325
00150                                                                   EL325
00151  FD  PRNTR                   COPY ELCPRTFD.                       EL325
00152                                                                   EL325
00153  FD  FICH                    COPY ELCFCHFD.                       EL325
00154                                                                   EL325
00155  FD  ELREPT                  COPY ELCRPTFD.                       EL325
00156                                                                   EL325
00157                              COPY ELCREPT.                        EL325
00158                                                                   EL325
00159  FD  MPPROD.                                                      EL325
00160                              COPY MPCPROD.                        EL325
070714 FD  ERMEBL.
070714                                 COPY ERCMEBL.
00161      EJECT                                                        EL325
00162  WORKING-STORAGE SECTION.                                         EL325
00163  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      EL325
00164                                                                   EL325
00165  77  FILLER  PIC X(32)   VALUE '********************************'.EL325
00166  77  FILLER  PIC X(32)   VALUE '*     EL325  WORKING STORAGE   *'.EL325
00167  77  FILLER  PIC X(32)   VALUE '********** VMOD=2.014 **********'.EL325
00168                                                                   EL325
00169  01  FILLER                          COMP-3.                      EL325
00170      05  WS-LINE-COUNT               PIC S9(3)       VALUE +99.   EL325
00171      05  WS-LINE-COUNT-MAX           PIC S9(3)       VALUE +55.   EL325
00172      05  WS-PAGE                     PIC S9(5)       VALUE ZERO.  EL325
00173      05  WS-REPORT-SW                PIC S9          VALUE ZERO.  EL325
00174      05  WS-PRINT-SW                 PIC S9          VALUE ZERO.  EL325
00175      05  WS-RECORD-COUNT             PIC S9(9)       VALUE ZERO.  EL325
00176      05  WS-RECORDS-RELEASED         PIC S9(9)       VALUE ZERO.  EL325
00177      05  WS-RETURN-CODE              PIC S9(3)       VALUE ZERO.  EL325
00178      05  WS-HEADING-SW               PIC S9          VALUE ZERO.  EL325
00179      05  WS-ZERO                     PIC S9          VALUE ZERO.  EL325
00180                                                                   EL325
00181      05  WS-ACCT-COUNT               PIC S9(7)         VALUE ZERO.EL325
00182      05  WS-ACCT-AH-COUNT            PIC S9(7)         VALUE ZERO.EL325
00183      05  WS-ACCT-LF-COUNT            PIC S9(7)         VALUE ZERO.EL325
122702     05  WS-ACCT-IU-COUNT            PIC S9(7)         VALUE ZERO.EL325
121203     05  WS-ACCT-GP-COUNT            PIC S9(7)         VALUE ZERO.EL325
052614     05  WS-ACCT-FL-COUNT            PIC S9(7)         VALUE ZERO.
022122     05  WS-ACCT-BR-COUNT            PIC S9(7)         VALUE ZERO.
022122     05  WS-ACCT-HS-COUNT            PIC S9(7)         VALUE ZERO.
100518     05  WS-ACCT-OT-COUNT            PIC S9(7)         VALUE ZERO.
00184      05  WS-ACCT-TOTAL               PIC S9(7)V99      VALUE ZERO.EL325
00185      05  WS-ACCT-AH-TOTAL            PIC S9(7)V99      VALUE ZERO.EL325
00186      05  WS-ACCT-LF-TOTAL            PIC S9(7)V99      VALUE ZERO.EL325
122702     05  WS-ACCT-IU-TOTAL            PIC S9(7)V99      VALUE ZERO.EL325
121203     05  WS-ACCT-GP-TOTAL            PIC S9(7)V99      VALUE ZERO.EL325
052614     05  WS-ACCT-FL-TOTAL            PIC S9(7)V99      VALUE ZERO.
022122     05  WS-ACCT-BR-TOTAL            PIC S9(7)V99      VALUE ZERO.
022122     05  WS-ACCT-HS-TOTAL            PIC S9(7)V99      VALUE ZERO.
100518     05  WS-ACCT-OT-TOTAL            PIC S9(7)V99      VALUE ZERO.
00187      05  WS-ACCT-COUNT-VOID          PIC S9(7)         VALUE ZERO.EL325
00188      05  WS-ACCT-AH-COUNT-VOID       PIC S9(7)         VALUE ZERO.EL325
00189      05  WS-ACCT-LF-COUNT-VOID       PIC S9(7)         VALUE ZERO.EL325
122702     05  WS-ACCT-IU-COUNT-VOID       PIC S9(7)         VALUE ZERO.EL325
121203     05  WS-ACCT-GP-COUNT-VOID       PIC S9(7)         VALUE ZERO.EL325
052614     05  WS-ACCT-FL-COUNT-VOID       PIC S9(7)         VALUE ZERO.
022122     05  WS-ACCT-BR-COUNT-VOID       PIC S9(7)         VALUE ZERO.
022122     05  WS-ACCT-HS-COUNT-VOID       PIC S9(7)         VALUE ZERO.
100518     05  WS-ACCT-OT-COUNT-VOID       PIC S9(7)         VALUE ZERO.
00190      05  WS-ACCT-TOTAL-VOID          PIC S9(7)V99      VALUE ZERO.EL325
00191      05  WS-ACCT-AH-TOTAL-VOID       PIC S9(7)V99      VALUE ZERO.EL325
00192      05  WS-ACCT-LF-TOTAL-VOID       PIC S9(7)V99      VALUE ZERO.EL325
122702     05  WS-ACCT-IU-TOTAL-VOID       PIC S9(7)V99      VALUE ZERO.EL325
121203     05  WS-ACCT-GP-TOTAL-VOID       PIC S9(7)V99      VALUE ZERO.EL325
100518     05  WS-ACCT-FL-TOTAL-VOID       PIC S9(7)V99      VALUE ZERO.
022122     05  WS-ACCT-BR-TOTAL-VOID       PIC S9(7)V99      VALUE ZERO.
022122     05  WS-ACCT-HS-TOTAL-VOID       PIC S9(7)V99      VALUE ZERO.
100518     05  WS-ACCT-OT-TOTAL-VOID       PIC S9(7)V99      VALUE ZERO.
00193      05  WS-CARRIER-COUNT            PIC S9(7)         VALUE ZERO.EL325
00194      05  WS-CARRIER-AH-COUNT         PIC S9(7)         VALUE ZERO.EL325
00195      05  WS-CARRIER-LF-COUNT         PIC S9(7)         VALUE ZERO.EL325
122702     05  WS-CARRIER-IU-COUNT         PIC S9(7)         VALUE ZERO.EL325
121203     05  WS-CARRIER-GP-COUNT         PIC S9(7)         VALUE ZERO.EL325
052614     05  WS-CARRIER-FL-COUNT         PIC S9(7)         VALUE ZERO.
022122     05  WS-CARRIER-BR-COUNT         PIC S9(7)         VALUE ZERO.
022122     05  WS-CARRIER-HS-COUNT         PIC S9(7)         VALUE ZERO.
100518     05  WS-CARRIER-OT-COUNT         PIC S9(7)         VALUE ZERO.
00196      05  WS-CARRIER-TOTAL            PIC S9(7)V99      VALUE ZERO.EL325
00197      05  WS-CARRIER-AH-TOTAL         PIC S9(7)V99      VALUE ZERO.EL325
00198      05  WS-CARRIER-LF-TOTAL         PIC S9(7)V99      VALUE ZERO.EL325
122702     05  WS-CARRIER-IU-TOTAL         PIC S9(7)V99      VALUE ZERO.EL325
121203     05  WS-CARRIER-GP-TOTAL         PIC S9(7)V99      VALUE ZERO.EL325
052614     05  WS-CARRIER-FL-TOTAL         PIC S9(7)V99      VALUE ZERO.
022122     05  WS-CARRIER-BR-TOTAL         PIC S9(7)V99      VALUE ZERO.
022122     05  WS-CARRIER-HS-TOTAL         PIC S9(7)V99      VALUE ZERO.
100518     05  WS-CARRIER-OT-TOTAL         PIC S9(7)V99      VALUE ZERO.
00199      05  WS-CARRIER-COUNT-VOID       PIC S9(7)         VALUE ZERO.EL325
00200      05  WS-CARRIER-AH-COUNT-VOID    PIC S9(7)         VALUE ZERO.EL325
00201      05  WS-CARRIER-LF-COUNT-VOID    PIC S9(7)         VALUE ZERO.EL325
122702     05  WS-CARRIER-IU-COUNT-VOID    PIC S9(7)         VALUE ZERO.EL325
121203     05  WS-CARRIER-GP-COUNT-VOID    PIC S9(7)         VALUE ZERO.EL325
052614     05  WS-CARRIER-FL-COUNT-VOID    PIC S9(7)         VALUE ZERO.
022122     05  WS-CARRIER-BR-COUNT-VOID    PIC S9(7)         VALUE ZERO.
022122     05  WS-CARRIER-HS-COUNT-VOID    PIC S9(7)         VALUE ZERO.
100518     05  WS-CARRIER-OT-COUNT-VOID    PIC S9(7)         VALUE ZERO.
00202      05  WS-CARRIER-TOTAL-VOID       PIC S9(7)V99      VALUE ZERO.EL325
00203      05  WS-CARRIER-AH-TOTAL-VOID    PIC S9(7)V99      VALUE ZERO.EL325
00204      05  WS-CARRIER-LF-TOTAL-VOID    PIC S9(7)V99      VALUE ZERO.EL325
122702     05  WS-CARRIER-IU-TOTAL-VOID    PIC S9(7)V99      VALUE ZERO.EL325
121203     05  WS-CARRIER-GP-TOTAL-VOID    PIC S9(7)V99      VALUE ZERO.EL325
052614     05  WS-CARRIER-FL-TOTAL-VOID    PIC S9(7)V99      VALUE ZERO.
022122     05  WS-CARRIER-BR-TOTAL-VOID    PIC S9(7)V99      VALUE ZERO.
022122     05  WS-CARRIER-HS-TOTAL-VOID    PIC S9(7)V99      VALUE ZERO.
100518     05  WS-CARRIER-OT-TOTAL-VOID    PIC S9(7)V99      VALUE ZERO.
00205      05  WS-FINAL-COUNT              PIC S9(7)         VALUE ZERO.EL325
00206      05  WS-FINAL-AH-COUNT           PIC S9(7)         VALUE ZERO.EL325
00207      05  WS-FINAL-LF-COUNT           PIC S9(7)         VALUE ZERO.EL325
122702     05  WS-FINAL-IU-COUNT           PIC S9(7)         VALUE ZERO.EL325
121203     05  WS-FINAL-GP-COUNT           PIC S9(7)         VALUE ZERO.EL325
052614     05  WS-FINAL-FL-COUNT           PIC S9(7)         VALUE ZERO.
022122     05  WS-FINAL-BR-COUNT           PIC S9(7)         VALUE ZERO.
022122     05  WS-FINAL-HS-COUNT           PIC S9(7)         VALUE ZERO.
100518     05  WS-FINAL-OT-COUNT           PIC S9(7)         VALUE ZERO.
00208      05  WS-FINAL-TOTAL              PIC S9(7)V99      VALUE ZERO.EL325
00209      05  WS-FINAL-AH-TOTAL           PIC S9(7)V99      VALUE ZERO.EL325
00210      05  WS-FINAL-LF-TOTAL           PIC S9(7)V99      VALUE ZERO.EL325
122702     05  WS-FINAL-IU-TOTAL           PIC S9(7)V99      VALUE ZERO.EL325
121203     05  WS-FINAL-GP-TOTAL           PIC S9(7)V99      VALUE ZERO.EL325
052614     05  WS-FINAL-FL-TOTAL           PIC S9(7)V99      VALUE ZERO.
022122     05  WS-FINAL-BR-TOTAL           PIC S9(7)V99      VALUE ZERO.
022122     05  WS-FINAL-HS-TOTAL           PIC S9(7)V99      VALUE ZERO.
100518     05  WS-FINAL-OT-TOTAL           PIC S9(7)V99      VALUE ZERO.
00211      05  WS-FINAL-COUNT-VOID         PIC S9(7)         VALUE ZERO.EL325
00212      05  WS-FINAL-AH-COUNT-VOID      PIC S9(7)         VALUE ZERO.EL325
00213      05  WS-FINAL-LF-COUNT-VOID      PIC S9(7)         VALUE ZERO.EL325
122702     05  WS-FINAL-IU-COUNT-VOID      PIC S9(7)         VALUE ZERO.EL325
121203     05  WS-FINAL-GP-COUNT-VOID      PIC S9(7)         VALUE ZERO.EL325
052614     05  WS-FINAL-FL-COUNT-VOID      PIC S9(7)         VALUE ZERO.
022122     05  WS-FINAL-BR-COUNT-VOID      PIC S9(7)         VALUE ZERO.
022122     05  WS-FINAL-HS-COUNT-VOID      PIC S9(7)         VALUE ZERO.
100518     05  WS-FINAL-OT-COUNT-VOID      PIC S9(7)         VALUE ZERO.
00214      05  WS-FINAL-TOTAL-VOID         PIC S9(7)V99      VALUE ZERO.EL325
00215      05  WS-FINAL-AH-TOTAL-VOID      PIC S9(7)V99      VALUE ZERO.EL325
00216      05  WS-FINAL-LF-TOTAL-VOID      PIC S9(7)V99      VALUE ZERO.EL325
122702     05  WS-FINAL-IU-TOTAL-VOID      PIC S9(7)V99      VALUE ZERO.EL325
121203     05  WS-FINAL-GP-TOTAL-VOID      PIC S9(7)V99      VALUE ZERO.EL325
052614     05  WS-FINAL-FL-TOTAL-VOID      PIC S9(7)V99      VALUE ZERO.
022122     05  WS-FINAL-BR-TOTAL-VOID      PIC S9(7)V99      VALUE ZERO.
022122     05  WS-FINAL-HS-TOTAL-VOID      PIC S9(7)V99      VALUE ZERO.
100518     05  WS-FINAL-OT-TOTAL-VOID      PIC S9(7)V99      VALUE ZERO.
00217                                                                   EL325
00218      05  WS-CARRIER-REINS-COUNT      PIC S9(7)         VALUE ZERO.EL325
00219      05  WS-CARRIER-REINS-TOTAL      PIC S9(7)V99      VALUE ZERO.EL325
00220      05  WS-CARRIER-REINS-COUNT-VOID PIC S9(7)         VALUE ZERO.EL325
00221      05  WS-CARRIER-REINS-TOTAL-VOID PIC S9(7)V99      VALUE ZERO.EL325
00222                                                                   EL325
00223      05  WS-FINAL-REINS-COUNT        PIC S9(7)         VALUE ZERO.EL325
00224      05  WS-FINAL-REINS-TOTAL        PIC S9(7)V99      VALUE ZERO.EL325
00225      05  WS-FINAL-REINS-COUNT-VOID   PIC S9(7)         VALUE ZERO.EL325
00226      05  WS-FINAL-REINS-TOTAL-VOID   PIC S9(7)V99      VALUE ZERO.EL325
00227                                                                   EL325
00228  01  FILLER                          COMP SYNC.                   EL325
00229      05  PGM-SUB                     PIC S9(4)       VALUE +325.  EL325
00230      05  WS-INDEX                    PIC S9(4)       VALUE ZERO.  EL325
00231                                                                   EL325
00232  01  FILLER.                                                      EL325
00233      05  ABEND-CODE                  PIC X(4).                    EL325
00234      05  ABEND-OPTION                PIC X.                       EL325
00235      05  OLC-REPORT-NAME             PIC X(5) VALUE 'EL325'.      EL325
00236      05  X                           PIC X           VALUE SPACE. EL325
00237                                                                   EL325
00238      05  WS-FIRST-TIME-SW            PIC X(01)       VALUE 'Y'.   EL325
00239          88  FIRST-TIME                              VALUE 'Y'.   EL325
00240                                                                   EL325
00241      05  WS-WORK-DATE.                                            EL325
00242          10  WS-WORK-MO          PIC 99.                          EL325
00243          10  FILLER              PIC X.                           EL325
00244          10  WS-WORK-DA          PIC 99.                          EL325
00245          10  FILLER              PIC X.                           EL325
00246          10  WS-WORK-YR          PIC 99.                          EL325
00247                                                                   EL325
00248      05  WS-SAVE-PRINT-RECORD        PIC X(133)      VALUE SPACES.EL325
00249                                                                   EL325
00250      05  WS-LAST-CONTROL.                                         EL325
00251          07  WS-LAST-COMPANY             PIC X      VALUE SPACES. EL325
00252          07  WS-LAST-CARRIER             PIC X      VALUE SPACES. EL325
00253          07  WS-LAST-GROUPING            PIC X(6)   VALUE SPACES. EL325
00254          07  WS-LAST-STATE               PIC XX     VALUE SPACES. EL325
00255          07  WS-LAST-ACCOUNT             PIC X(10)  VALUE SPACES. EL325
00256                                                                   EL325
00257      05  WS-LAST-VG-CONTROL.                                      EL325
00258          07  WS-VG-LAST-COMPANY          PIC X      VALUE SPACES. EL325
00259          07  WS-VG-LAST-CARRIER          PIC X      VALUE SPACES. EL325
00260          07  WS-VG-LAST-GROUPING         PIC X(6)   VALUE SPACES. EL325
00261          07  WS-VG-LAST-STATE            PIC XX     VALUE SPACES. EL325
00262          07  WS-VG-LAST-ACCOUNT          PIC X(10)  VALUE SPACES. EL325
00263                                                                   EL325
00264      05  WS-CONTROL-CARRIER              PIC X      VALUE SPACES. EL325
00265      05  WS-CONTROL-ACCOUNT              PIC X(10) VALUE SPACES.  EL325
00266                                                                   EL325
00267      05  WS-ABEND-MESSAGE            PIC X(80)       VALUE SPACES.EL325
00268                                                                   EL325
00269      05  WS-ABEND-FILE-STATUS        PIC XX          VALUE ZERO.  EL325
00270                                                                   EL325
00271      05  AM-FILE-STATUS              PIC XX          VALUE ZERO.  EL325
00272      05  PD-FILE-STATUS              PIC XX          VALUE ZERO.  EL325
00273                                                                   EL325
00274      05  WS-FILE-ERROR-MESSAGE.                                   EL325
00275          10  FILLER                  PIC X(24)       VALUE        EL325
00276              'ERROR OCCURED OPENING - '.                          EL325
00277          10  WS-FEM-FILE-NAME        PIC X(8).                    EL325

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
070714     12  hld-325-clms-tot    pic s9(9)v99 comp-3 value +0.

00280  01  WS-HEADING1.                                                 EL325
00281      05  FILLER                      PIC X(50)       VALUE '1'.   EL325
00282      05  WS-H1-TITLE                 PIC X(70)       VALUE        EL325
00283          'CLAIM PAYMENT REGISTER'.                                EL325
00284      05  WS-H1-REPORT-NUMBER         PIC X(05) VALUE 'EL325'.     EL325
00285      05  WS-H1-REPORT-TYPE           PIC X(01)       VALUE ' '.   EL325
00286      05  FILLER                      PIC X(07)       VALUE SPACES.EL325
00287                                                                   EL325
00288  01  WS-HEADING2.                                                 EL325
00289      05  FILLER                      PIC X(46)       VALUE SPACES.EL325
00290      05  WS-H2-CLIENT-NAME           PIC X(74)       VALUE SPACES.EL325
00291      05  WS-H2-DATE                  PIC X(8)        VALUE SPACES.EL325
00292      05  FILLER                      PIC X(05)       VALUE SPACES.EL325
00293                                                                   EL325
00294  01  WS-HEADING3.                                                 EL325
00295      05  FILLER                      PIC X(51)       VALUE SPACES.EL325
00296      05  WS-H3-DATE                  PIC X(69)       VALUE SPACES.EL325
00297      05  FILLER                      PIC X(5)        VALUE 'PAGE'.EL325
00298      05  WS-H3-PAGE                  PIC ZZ,ZZ9.                  EL325
00299      05  FILLER                      PIC X(02)       VALUE SPACES.EL325
00300                                                                   EL325
00301  01  WS-HEADING4.                                                 EL325
00302      05  FILLER                      PIC X(11)       VALUE        EL325
00303          '  CARRIER -'.                                           EL325
00304      05  WS-H4-CARRIER               PIC X           VALUE SPACES.EL325
00305      05  FILLER                      PIC X(121)      VALUE SPACES.EL325
00306                                                                   EL325
00307  01  WS-HEADING5.                                                 EL325
00308      05  FILLER                      PIC X(11)       VALUE SPACES.EL325
00309      05  WS-H5-CARRIER-NAME          PIC X(30)       VALUE SPACES.EL325
00310      05  FILLER                      PIC X(92)       VALUE SPACES.EL325
00311                                                                   EL325
00312  01  WS-HEADING6.                                                 EL325
00313      05  FILLER                      PIC X(11)       VALUE        EL325
00314          '  ACCOUNT -'.                                           EL325
00315      05  WS-H6-ACCOUNT               PIC X(10)       VALUE SPACES.EL325
00316      05  FILLER                      PIC X(116)      VALUE SPACES.EL325
00317                                                                   EL325
00318  01  WS-HEADING7A.                                                EL325
00319      05  FILLER                      PIC X           VALUE '0'.   EL325
00320      05  FILLER                      PIC X(10)       VALUE SPACES.EL325
00321      05  WS-H7-ACCOUNT-NAME          PIC X(30)       VALUE SPACES.EL325
00322      05  FILLER                      PIC X(92)       VALUE SPACES.EL325
00323                                                                   EL325
00324  01  WS-HEADING7B.                                                EL325
00325      05  FILLER                      PIC X(11)       VALUE SPACES.EL325
00326      05  WS-H7-ACCOUNT-PERSON        PIC X(30)       VALUE SPACES.EL325
00327      05  FILLER                      PIC X(92)       VALUE SPACES.EL325
00328                                                                   EL325
00329  01  WS-HEADING7C.                                                EL325
00330      05  FILLER                      PIC X(11)       VALUE SPACES.EL325
00331      05  WS-H7-ACCOUNT-ADDRESS       PIC X(30)       VALUE SPACES.EL325
00332      05  FILLER                      PIC X(92)       VALUE SPACES.EL325
00333                                                                   EL325
00334  01  WS-HEADING7D.                                                EL325
00335      05  FILLER                      PIC X(11)       VALUE SPACES.EL325
00336      05  WS-H7-ACCOUNT-CITY-ST       PIC X(21)       VALUE SPACES.EL325
00337      05  WS-H7-ACCOUNT-ZIP-PRIME     PIC X(5)        VALUE SPACES.EL325
00338      05  FILLER                      PIC X(1)        VALUE SPACES.EL325
00339      05  WS-H7-ACCOUNT-ZIP-PLUS4     PIC X(4)        VALUE SPACES.EL325
00340      05  FILLER                      PIC X(90)       VALUE SPACES.EL325
00341                                                                   EL325
00342  01  WS-HEADING8.                                                 EL325
00343      05  FILLER                      PIC X(8)        VALUE        EL325
00344          '-  CHECK'.                                              EL325
00345      05  FILLER                      PIC X(17)       VALUE SPACE. EL325
00346      05  FILLER                      PIC X(15)       VALUE        EL325
00347          'DATE    PAYMENT'.                                       EL325
00348      05  FILLER                      PIC X(32)       VALUE SPACE. EL325
00349      05  FILLER                      PIC X(23)       VALUE        EL325
00350          'CLAIM  INCURRED   CLAIM'.                               EL325
00351      05  FILLER                      PIC X(18)       VALUE SPACE. EL325
00352      05  FILLER                      PIC X(9)        VALUE        EL325
00353          'EFFECTIVE'.                                             EL325
00354      05  FILLER                      PIC X(11)       VALUE SPACE. EL325
00355                                                                   EL325
00356  01  WS-HEADING9.                                                 EL325
00357      05  FILLER                      PIC X(40)        VALUE       EL325
00358          '  NUMBER      AMOUNT     PAID     TYPE  '.              EL325
00359      05  FILLER                      PIC X(10)        VALUE       EL325
00360          '  INSURED '.                                            EL325
00361      05  FILLER                      PIC X(23)        VALUE SPACE.EL325
00362      05  FILLER                      PIC X(05)        VALUE       EL325
00363          'TYPE '.                                                 EL325
00364      05  EL325A-HDG                  PIC X(09)        VALUE       EL325
00365                'PAID THRU'.                                       EL325
00366      05  FILLER                      PIC X(22)        VALUE       EL325
00367          ' NUMBER  STAT  CERT NO'.                                EL325
00368      05  FILLER                      PIC X(23)        VALUE       EL325
00369          '   EXPIRES   ST GROUP  '.                               EL325
00370                                                                   EL325
00371      EJECT                                                        EL325
00372  01  WS-DETAIL1.                                                  EL325
00373      05  FILLER                      PIC X.                       EL325
00374      05  WS-D1-CHECK-NO              PIC X(7).                    EL325
00375      05  FILLER                      PIC X.                       EL325
00376      05  WS-D1-AMOUNT                PIC Z,ZZZ,ZZ9.99-.           EL325
00377      05  FILLER                      PIC X.                       EL325
00378      05  WS-D1-DATE-PAID             PIC X(8).                    EL325
00379      05  FILLER                      PIC X.                       EL325
00380      05  WS-D1-PAYMENT-TYPE          PIC X(8).                    EL325
00381      05  FILLER                      PIC X.                       EL325
00382      05  WS-D1-INSURED               PIC X(30).                   EL325
00383      05  FILLER                      PIC X.                       EL325
00384      05  WS-D1-CLAIM-TYPE            PIC X(6).                    EL325
00385      05  FILLER                      PIC X.                       EL325
00386      05  WS-D1-INCURRED-DATE         PIC X(8).                    EL325
00387                                                                   EL325
00388      05  WS-D1-PAID-THRU-DATE   REDEFINES                         EL325
00389          WS-D1-INCURRED-DATE   PIC X(8).                          EL325
00390      05  FILLER                      PIC X(2).                    EL325
00391      05  WS-D1-CLAIM-NO              PIC X(7).                    EL325
00392      05  FILLER                      PIC X(2).                    EL325
00393      05  WS-D1-STATUS                PIC X.                       EL325
00394      05  FILLER                      PIC X(2).                    EL325
00395      05  WS-D1-CERT-NO               PIC X(11).                   EL325
00396      05  FILLER                      PIC X.                       EL325
00397      05  WS-D1-EFFECTIVE-DATE        PIC X(8).                    EL325
00398                                                                   EL325
00399      05  WS-D1-EXPIRE-DATE    REDEFINES                           EL325
00400          WS-D1-EFFECTIVE-DATE PIC X(8).                           EL325
00401      05  FILLER                      PIC X(2).                    EL325
00402      05  WS-D1-STATE                 PIC X(2).                    EL325
00403      05  FILLER                      PIC X.                       EL325
00404      05  WS-D1-GROUP                 PIC X(6).                    EL325
00405      05  FILLER                      PIC X.                       EL325
00406                                                                   EL325
00407  01  FILLER                          REDEFINES                    EL325
00408      WS-DETAIL1.                                                  EL325
00409      05  FILLER                      PIC X(23).                   EL325
00410      05  WS-D2-MESSAGE               PIC X(15).                   EL325
00411                                                                   EL325
00412  01  FILLER                          REDEFINES                    EL325
00413      WS-DETAIL1.                                                  EL325
00414      05  FILLER                      PIC X(01).                   EL325
00415      05  WS-T1-MESSAGE               PIC X(22).                   EL325
00416      05  WS-T1-COUNT                 PIC ZZ,ZZ9-.     
00417      05  WS-T1-AMOUNT                PIC Z,ZZZ,ZZ9.99-.           EL325
00418      05  FILLER                      PIC X(02).                   EL325
00419      05  WS-TL-MESSAGE               PIC X(08).                   EL325
00420      05  WS-TL-COUNT                 PIC ZZ,ZZ9-.     
00421      05  WS-TL-AMOUNT                PIC Z,ZZZ,ZZ9.99-.           EL325
00422      05  FILLER                      PIC X(02).                   EL325
00423      05  WS-TA-MESSAGE               PIC X(08).                   EL325
00424      05  WS-TA-COUNT                 PIC ZZ,ZZ9-.      
00425      05  WS-TA-AMOUNT                PIC Z,ZZZ,ZZ9.99-.           EL325
122702     05  FILLER                      PIC X(02).                   EL325
122702     05  WS-TU-MESSAGE               PIC X(08).                   EL325
122702     05  WS-TU-COUNT                 PIC ZZ,ZZ9-.     
122702     05  WS-TU-AMOUNT                PIC Z,ZZZ,ZZ9.99-.           EL325
052614
052614 01  FILLER                          REDEFINES
052614     WS-DETAIL1.
052614     05  FILLER                      PIC X(01).
052614     05  FILLER                      PIC X(72).
121203     05  FILLER                      PIC X(02).                   EL325
121203     05  WS-TG-MESSAGE               PIC X(08).                   EL325
121203     05  WS-TG-COUNT                 PIC ZZ,ZZ9-.     
121203     05  WS-TG-AMOUNT                PIC Z,ZZZ,ZZ9.99-.           EL325
052614     05  FILLER                      PIC X(02).
052614     05  WS-TF-MESSAGE               PIC X(08).
052614     05  WS-TF-COUNT                 PIC ZZ,ZZ9-.
052614     05  WS-TF-AMOUNT                PIC Z,ZZZ,ZZ9.99-.

022122 01  FILLER                          REDEFINES
022122     WS-DETAIL1.
022122     05  FILLER                      PIC X(01).
022122     05  FILLER                      PIC X(72).
022122     05  FILLER                      PIC X(02).                   EL325
022122     05  WS-TB-MESSAGE               PIC X(08).                   EL325
022122     05  WS-TB-COUNT                 PIC ZZ,ZZ9-.     
022122     05  WS-TB-AMOUNT                PIC Z,ZZZ,ZZ9.99-.           EL325
022122     05  FILLER                      PIC X(02).
022122     05  WS-TH-MESSAGE               PIC X(08).
022122     05  WS-TH-COUNT                 PIC ZZ,ZZ9-.
022122     05  WS-TH-AMOUNT                PIC Z,ZZZ,ZZ9.99-.

00428                              COPY ELCDTECX.                       EL325
00429      EJECT                                                        EL325
00430                              COPY ELCDTEVR.                       EL325
00431                                                                   EL325
00432                              COPY ELCDATE.                           CL**3
00433                                                                   EL325
00434      EJECT                                                        EL325
00435  PROCEDURE DIVISION.                                              EL325
00436                                                                   EL325
00437  0000-DATE-CARD-READ SECTION. COPY ELCDTERX.                      EL325
00438                                                                   EL325
070714****=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=***
070714****                                                           ***
070714****   Set up the month-end auto balancing.                    ***
070714****                                                           ***
070714****=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=***
070714
070714     MOVE WS-TIME                TO ME-START-TIME
070714     MOVE WS-CURRENT-DATE        TO ME-START-DATE
070714     MOVE ME-START-MO            TO ME-CNDS-MO
070714     MOVE ME-START-DA            TO ME-CNDS-DA
070714     MOVE ME-START-YR            TO ME-CNDS-YR

           .
00439  1000-MAIN-LOGIC SECTION.                                         EL325
00440                                                                   EL325
00441      PERFORM OPEN-FILES.                                          EL325
00442                                                                   EL325
00443      IF DTE-CLAIM-PAID-THRU-TO EQUAL '1'                          EL325
00444         MOVE 'PAID  TO ' TO EL325A-HDG.                           EL325
00445                                                                   EL325
00446      SORT SORT-WORK-FILE                                          EL325
00447          ON ASCENDING KEY SWR-SB-CARRIER                          EL325
00448                           SWR-BA-ACCOUNT                          EL325
00449                           SWR-BA-INSURED-LAST-NAME                EL325
00450                           SWR-BA-CLAIM-NO                         EL325
00451                           SWR-SB-CHECK-NO                         EL325
00452          INPUT PROCEDURE  2000-SORT-INPUT-PROCEDURE               EL325
00453          OUTPUT PROCEDURE 3000-SORT-OUTPUT-PROCEDURE.             EL325
00454                                                                   EL325
00455      IF (SORT-RETURN > ZEROS)
022804        AND (WS-RECORDS-RELEASED NOT = +0)
               DISPLAY 'INPUT RECORD COUNT IS ' WS-RECORD-COUNT
               DISPLAY 'RELSE RECORD COUNT IS ' WS-RECORDS-RELEASED
00456          MOVE 'SORT FAILED'      TO  WS-ABEND-MESSAGE             EL325
00457          MOVE SORT-RETURN        TO  WS-RETURN-CODE               EL325
00458          GO TO ABEND-PGM
022804     END-IF.
00459                                                                   EL325
00460      PERFORM CLOSE-FILES.                                         EL325
00461                                                                   EL325
00462      GOBACK.                                                      EL325
00463                                                                   EL325
00464      EJECT                                                        EL325
00465  2000-SORT-INPUT-PROCEDURE SECTION.                               EL325
00466 *    NOTE ******************************************************* EL325
00467 *         *      THIS SECTION SELECTS ONLY THE EXTRACT A RECORD * EL325
00468 *         *  A FOR THIS REPORT.                                 * EL325
00469 *         *******************************************************.EL325
00470                                                                   EL325
00471  2100-SORT-INPUT-PROCEDURE.                                       EL325
00472      READ REPORTS-EXTRACT-FILE                                    EL325
00473          AT END                                                   EL325
00474              GO TO 2900-EXIT.                                     EL325
00475                                                                   EL325
00476      ADD +1  TO  WS-RECORD-COUNT.                                 EL325
00477                                                                   EL325
00478      IF EX-POSITIONING-CODE LESS THAN '9'                         EL325
00479          GO TO 2100-SORT-INPUT-PROCEDURE.                         EL325
00480                                                                   EL325
00481      IF EX-EXTRACT-CODE LESS THAN 'B'                             EL325
00482          GO TO 2100-SORT-INPUT-PROCEDURE.                         EL325
00483                                                                   EL325
00484      IF EX-EXTRACT-CODE GREATER THAN 'B'                          EL325
00485          GO TO 2900-EXIT.                                         EL325
00486                                                                   EL325
00487      IF EX-COMPANY-CD LESS THAN DTE-CLASIC-COMPANY-CD             EL325
00488          GO TO 2100-SORT-INPUT-PROCEDURE.                         EL325
00489                                                                   EL325
00490      IF EX-COMPANY-CD GREATER THAN DTE-CLASIC-COMPANY-CD          EL325
00491          GO TO 2900-EXIT.                                         EL325
00492                                                                   EL325
00493      IF EX-RECORD-TYPE GREATER THAN 'A'                           EL325
00494          GO TO 2900-EXIT.                                         EL325
00495
022106     IF EX-BA-PAYMENT-TYPE = 'I'
022106        GO TO 2100-SORT-INPUT-PROCEDURE
022106     END-IF
00496      .
00497  2200-SORT-INPUT-PROCEDURE.                                       EL325
00498      MOVE     REPORTS-EXTRACT-RECORD     IN  REPORTS-EXTRACT-FILE EL325
00499               TO REPORTS-EXTRACT-RECORD  IN  SORT-WORK-FILE.      EL325
00500      RELEASE REPORTS-EXTRACT-RECORD      IN SORT-WORK-FILE.       EL325
00501                                                                   EL325
00502      IF SORT-RETURN > ZEROS
00503         MOVE 'ERROR OCCURED SORT - RELEASE'
                                       TO WS-ABEND-MESSAGE
00504         MOVE SORT-RETURN         TO WS-RETURN-CODE
00505         GO TO ABEND-PGM
           END-IF
00506                                                                   EL325
00507      ADD +1  TO  WS-RECORDS-RELEASED.                             EL325
00508                                                                   EL325
00509      GO TO 2100-SORT-INPUT-PROCEDURE.                             EL325
00510                                                                   EL325
00511  2900-EXIT.                                                       EL325
00512      EXIT.                                                        EL325
00513                                                                   EL325
00514      EJECT                                                        EL325
00515  3000-SORT-OUTPUT-PROCEDURE SECTION.                              EL325
00516      IF WS-RECORDS-RELEASED NOT GREATER THAN ZERO                 EL325
00517          MOVE '1NO REPORT RECORDS FOR EL325'  TO  PRT             EL325
00518          PERFORM WRITE-PRINTER                                    EL325
00519          GO TO 3900-EXIT.                                         EL325
00520                                                                   EL325
00521  3100-SORT-OUTPUT-PROCEDURE.                                      EL325
00522      RETURN SORT-WORK-FILE                                        EL325
00523          AT END                                                   EL325
00524              MOVE HIGH-VALUES    TO  SWR-SB-CARRIER               EL325
00525                                      SWR-BA-ACCOUNT.              EL325
00526                                                                   EL325
00527      IF SORT-RETURN GREATER THAN ZERO                             EL325
00528          MOVE 'ERROR OCCURED SORT - RETURN'  TO  WS-ABEND-MESSAGE EL325
00529          MOVE SORT-RETURN                    TO  WS-RETURN-CODE   EL325
00530          GO TO ABEND-PGM.                                         EL325
00531                                                                   EL325
00532      IF WS-FIRST-TIME-SW IS EQUAL TO 'Y'                          EL325
00533          MOVE 'N'                  TO  WS-FIRST-TIME-SW           EL325
00534          MOVE DTE-CLASIC-COMPANY-CD                               EL325
00535                                    TO  WS-LAST-COMPANY            EL325
00536          MOVE SWR-SB-CARRIER       TO  WS-LAST-CARRIER            EL325
00537          PERFORM 8100-GET-CARRIER-NAME                            EL325
00538          MOVE SWR-BA-ACCOUNT       TO  WS-LAST-ACCOUNT            EL325
00539                                        WS-H6-ACCOUNT              EL325
00540          MOVE SWR-BA-STATE         TO  WS-LAST-STATE              EL325
00541          MOVE SWR-BA-GROUPING      TO  WS-LAST-GROUPING           EL325
00542          IF SWR-BA-SYSTEM-IDENTIFIER IS EQUAL TO 'CV'             EL325
00543              PERFORM 8300-GET-PRODUCER-ADDR                       EL325
00544              MOVE SWR-SB-CARRIER   TO  WS-CONTROL-CARRIER         EL325
00545              MOVE SWR-BA-ACCOUNT   TO  WS-CONTROL-ACCOUNT         EL325
00546          ELSE                                                     EL325
00547              PERFORM 8200-GET-ACCT-ADDR                           EL325
00548              MOVE SWR-SB-CARRIER   TO  WS-CONTROL-CARRIER         EL325
00549              MOVE SWR-BA-ACCOUNT   TO  WS-CONTROL-ACCOUNT.        EL325
00550                                                                   EL325
00551      EJECT                                                        EL325
00552      IF    WS-CONTROL-CARRIER = SWR-SB-CARRIER                    EL325
00553        AND WS-CONTROL-ACCOUNT = SWR-BA-ACCOUNT                    EL325
00554            GO TO 3200-SORT-OUTPUT-PROCEDURE.                      EL325
00555                                                                   EL325
00556      COMPUTE WS-ACCT-COUNT  =  WS-ACCT-AH-COUNT +                 EL325
122702                               WS-ACCT-LF-COUNT +
121203                               WS-ACCT-IU-COUNT +
052614                               WS-ACCT-FL-COUNT +
100518                               WS-ACCT-OT-COUNT +
121203                               WS-ACCT-GP-COUNT.
00558      COMPUTE WS-ACCT-TOTAL  =  WS-ACCT-AH-TOTAL +                 EL325
122702                               WS-ACCT-LF-TOTAL +
121203                               WS-ACCT-IU-TOTAL +
052614                               WS-ACCT-FL-TOTAL +
100518                               WS-ACCT-OT-TOTAL +
121203                               WS-ACCT-GP-TOTAL.
00560                                                                      CL**2
00561      MOVE '-'                    TO  WS-DETAIL1.                  EL325
00562      MOVE 'TOTAL ACCOUNT'        TO  WS-T1-MESSAGE.               EL325
00563      MOVE WS-ACCT-COUNT          TO  WS-T1-COUNT.                 EL325
00564      MOVE WS-ACCT-TOTAL          TO  WS-T1-AMOUNT.                EL325
00565      MOVE AH-OVERRIDE-L6         TO  WS-TA-MESSAGE.               EL325
00566      MOVE WS-ACCT-AH-COUNT       TO  WS-TA-COUNT.                 EL325
00567      MOVE WS-ACCT-AH-TOTAL       TO  WS-TA-AMOUNT.                EL325
00568      MOVE LIFE-OVERRIDE-L6       TO  WS-TL-MESSAGE.               EL325
100518     ADD WS-ACCT-LF-COUNT      WS-ACCT-OT-COUNT                   EL325
100518        GIVING WS-TL-COUNT.                                       EL325
100518     ADD WS-ACCT-LF-TOTAL      WS-ACCT-OT-TOTAL                   EL325
100518        GIVING WS-TL-AMOUNT.                                      EL325
010716     IF DTE-CLIENT = 'DCC' or 'VPP'
122702         MOVE 'IU    '           TO  WS-TU-MESSAGE                EL325
122702         MOVE WS-ACCT-IU-COUNT   TO  WS-TU-COUNT                  EL325
122702         MOVE WS-ACCT-IU-TOTAL   TO  WS-TU-AMOUNT 
122702     END-IF
00571      MOVE WS-DETAIL1             TO  PRT.                         EL325
00572      PERFORM WRITE-A-LINE.                                        EL325
00573                                                                   EL325
00574      COMPUTE WS-ACCT-COUNT-VOID  =  WS-ACCT-AH-COUNT-VOID +       EL325
122702                                    WS-ACCT-LF-COUNT-VOID +
121203                                    WS-ACCT-IU-COUNT-VOID +
052614                                    WS-ACCT-FL-COUNT-VOID +
100518                                    WS-ACCT-OT-COUNT-VOID +
121203                                    WS-ACCT-GP-COUNT-VOID.
00576                                                                   EL325
00577      COMPUTE WS-ACCT-TOTAL-VOID  =  WS-ACCT-AH-TOTAL-VOID +       EL325
122702                                    WS-ACCT-LF-TOTAL-VOID +
121203                                    WS-ACCT-IU-TOTAL-VOID +
052614                                    WS-ACCT-FL-TOTAL-VOID +
100518                                    WS-ACCT-OT-TOTAL-VOID +
121203                                    WS-ACCT-GP-TOTAL-VOID.
00579                                                                   EL325
00580      IF  WS-ACCT-COUNT-VOID NOT = ZERO                            EL325
00581          MOVE     SPACES                  TO   WS-DETAIL1         EL325
00582          MOVE     '      VOID '           TO   WS-T1-MESSAGE      EL325
00583          MOVE     WS-ACCT-COUNT-VOID      TO   WS-T1-COUNT        EL325
00584          MOVE     WS-ACCT-TOTAL-VOID      TO   WS-T1-AMOUNT       EL325
00585          MOVE     '    VOID'              TO   WS-TA-MESSAGE      EL325
00586          MOVE     WS-ACCT-AH-COUNT-VOID   TO   WS-TA-COUNT        EL325
00587          MOVE     WS-ACCT-AH-TOTAL-VOID   TO   WS-TA-AMOUNT       EL325
00588          MOVE     '    VOID'              TO   WS-TL-MESSAGE      EL325
100518         ADD WS-ACCT-LF-COUNT-VOID  WS-ACCT-OT-COUNT-VOID         EL325
100518            GIVING WS-TL-COUNT                                    EL325
100518         ADD WS-ACCT-LF-TOTAL-VOID  WS-ACCT-OT-TOTAL-VOID         EL325
100518            GIVING WS-TL-AMOUNT                                   EL325
010716         IF DTE-CLIENT = 'DCC' or 'VPP'
122702             MOVE '    VOID'              TO   WS-TU-MESSAGE      EL325
122702             MOVE WS-ACCT-IU-COUNT-VOID   TO   WS-TU-COUNT        EL325
122702             MOVE WS-ACCT-IU-TOTAL-VOID   TO   WS-TU-AMOUNT
122702         END-IF
00591          MOVE     WS-DETAIL1              TO   PRT                EL325
00592          PERFORM  WRITE-A-LINE                                    EL325
00593          MOVE     SPACES                  TO   WS-DETAIL1         EL325
00594          MOVE     '      NET    '         TO   WS-T1-MESSAGE      EL325
00595          SUBTRACT WS-ACCT-TOTAL-VOID FROM WS-ACCT-TOTAL           EL325
00596                   GIVING  WS-T1-AMOUNT                            EL325
00597          MOVE     '     NET'              TO   WS-TA-MESSAGE      EL325
00598          SUBTRACT WS-ACCT-AH-TOTAL-VOID FROM WS-ACCT-AH-TOTAL     EL325
00599                   GIVING  WS-TA-AMOUNT                            EL325
00600          MOVE     '     NET'              TO   WS-TL-MESSAGE      EL325
100518         COMPUTE WS-TL-AMOUNT =                                   EL325
100518            WS-ACCT-LF-TOTAL + WS-ACCT-OT-TOTAL                   EL325
100518            - WS-ACCT-LF-TOTAL-VOID                               EL325
100518            - WS-ACCT-OT-TOTAL-VOID                               EL325
010716         IF DTE-CLIENT = 'DCC' or 'VPP'
122702             MOVE '     NET'              TO   WS-TU-MESSAGE      EL325
122702             SUBTRACT WS-ACCT-IU-TOTAL-VOID
122702                 FROM WS-ACCT-IU-TOTAL   
122702                 GIVING WS-TU-AMOUNT       
122702         END-IF
00603          MOVE     WS-DETAIL1              TO   PRT                EL325
00604          PERFORM  WRITE-A-LINE.                                   EL325
052614
052614**WRITE GAP AND FAM
010716     IF DTE-CLIENT = 'DCC' or 'VPP'
052614         MOVE SPACES             TO  WS-DETAIL1
052614         IF WS-ACCT-COUNT-VOID NOT = ZERO 
052614            MOVE '0'             TO  WS-DETAIL1
052614         END-IF
052614
052614         MOVE 'GAP   '           TO  WS-TG-MESSAGE
052614         MOVE WS-ACCT-GP-COUNT   TO  WS-TG-COUNT
052614         MOVE WS-ACCT-GP-TOTAL   TO  WS-TG-AMOUNT
052614
052614         MOVE 'FAM   '           TO  WS-TF-MESSAGE
052614         MOVE WS-ACCT-FL-COUNT   TO  WS-TF-COUNT
052614         MOVE WS-ACCT-FL-TOTAL   TO  WS-TF-AMOUNT
052614         MOVE WS-DETAIL1         TO  PRT
052614         PERFORM WRITE-A-LINE
052614
052614         IF  WS-ACCT-COUNT-VOID NOT = ZERO
052614             MOVE     SPACES     TO   WS-DETAIL1
052614
052614             MOVE '    VOID'              TO   WS-TG-MESSAGE
052614             MOVE WS-ACCT-GP-COUNT-VOID   TO   WS-TG-COUNT
052614             MOVE WS-ACCT-GP-TOTAL-VOID   TO   WS-TG-AMOUNT
052614
052614             MOVE '    VOID'              TO   WS-TF-MESSAGE
052614             MOVE WS-ACCT-FL-COUNT-VOID   TO   WS-TF-COUNT
052614             MOVE WS-ACCT-FL-TOTAL-VOID   TO   WS-TF-AMOUNT
052614
052614             MOVE     WS-DETAIL1          TO   PRT
052614             PERFORM  WRITE-A-LINE
052614
052614             MOVE     SPACES              TO   WS-DETAIL1
052614
052614             MOVE '     NET'              TO   WS-TG-MESSAGE
052614             SUBTRACT WS-ACCT-GP-TOTAL-VOID
052614                 FROM WS-ACCT-GP-TOTAL
052614                 GIVING WS-TG-AMOUNT
052614
052614             MOVE '     NET'              TO   WS-TF-MESSAGE
052614             SUBTRACT WS-ACCT-FL-TOTAL-VOID
052614                 FROM WS-ACCT-FL-TOTAL
052614                 GIVING WS-TF-AMOUNT
052614
052614             MOVE     WS-DETAIL1          TO   PRT
052614             PERFORM  WRITE-A-LINE
052614         END-IF
052614     END-IF.
00605                                                                   EL325

022122**WRITE BRV AND HOS
022122     IF DTE-CLIENT = 'DCC' or 'VPP'
022122         MOVE SPACES             TO  WS-DETAIL1
022122         IF WS-ACCT-COUNT-VOID NOT = ZERO 
022122            MOVE '0'             TO  WS-DETAIL1
022122         END-IF
022122
022122         MOVE 'BRV   '           TO  WS-TB-MESSAGE
022122         MOVE WS-ACCT-BR-COUNT   TO  WS-TB-COUNT
022122         MOVE WS-ACCT-BR-TOTAL   TO  WS-TB-AMOUNT
022122
022122         MOVE 'HOS   '           TO  WS-TH-MESSAGE
022122         MOVE WS-ACCT-HS-COUNT   TO  WS-TH-COUNT
022122         MOVE WS-ACCT-HS-TOTAL   TO  WS-TH-AMOUNT
022122         MOVE WS-DETAIL1         TO  PRT
022122         PERFORM WRITE-A-LINE
022122
022122         IF  WS-ACCT-COUNT-VOID NOT = ZERO
022122             MOVE     SPACES     TO   WS-DETAIL1
022122
022122             MOVE '    VOID'              TO   WS-TB-MESSAGE
022122             MOVE WS-ACCT-BR-COUNT-VOID   TO   WS-TB-COUNT
022122             MOVE WS-ACCT-BR-TOTAL-VOID   TO   WS-TB-AMOUNT
022122
022122             MOVE '    VOID'              TO   WS-TH-MESSAGE
022122             MOVE WS-ACCT-HS-COUNT-VOID   TO   WS-TH-COUNT
022122             MOVE WS-ACCT-HS-TOTAL-VOID   TO   WS-TH-AMOUNT
022122
022122             MOVE     WS-DETAIL1          TO   PRT
022122             PERFORM  WRITE-A-LINE
022122
022122             MOVE     SPACES              TO   WS-DETAIL1
022122
022122             MOVE '     NET'              TO   WS-TG-MESSAGE
022122             SUBTRACT WS-ACCT-BR-TOTAL-VOID
022122                 FROM WS-ACCT-BR-TOTAL
022122                 GIVING WS-TB-AMOUNT
022122
022122             MOVE '     NET'              TO   WS-TF-MESSAGE
022122             SUBTRACT WS-ACCT-HS-TOTAL-VOID
022122                 FROM WS-ACCT-HS-TOTAL
022122                 GIVING WS-TH-AMOUNT
022122
022122             MOVE     WS-DETAIL1          TO   PRT
022122             PERFORM  WRITE-A-LINE
022122         END-IF
022122     END-IF

           .
00607  3120-SORT-OUTPUT-PROCEDURE.                                      EL325
00608      ADD WS-ACCT-AH-TOTAL          TO  WS-CARRIER-AH-TOTAL.    
00609      ADD WS-ACCT-AH-COUNT          TO  WS-CARRIER-AH-COUNT.   
00610      ADD WS-ACCT-AH-TOTAL-VOID     TO  WS-CARRIER-AH-TOTAL-VOID.
00611      ADD WS-ACCT-AH-COUNT-VOID     TO  WS-CARRIER-AH-COUNT-VOID.
00612      ADD WS-ACCT-LF-TOTAL          TO  WS-CARRIER-LF-TOTAL.       EL325
00613      ADD WS-ACCT-LF-COUNT          TO  WS-CARRIER-LF-COUNT.       EL325
00614      ADD WS-ACCT-LF-TOTAL-VOID     TO  WS-CARRIER-LF-TOTAL-VOID.  EL325
00615      ADD WS-ACCT-LF-COUNT-VOID     TO  WS-CARRIER-LF-COUNT-VOID.  EL325
122702     ADD WS-ACCT-IU-TOTAL          TO  WS-CARRIER-IU-TOTAL.       EL325
122702     ADD WS-ACCT-IU-COUNT          TO  WS-CARRIER-IU-COUNT.       EL325
122702     ADD WS-ACCT-IU-TOTAL-VOID     TO  WS-CARRIER-IU-TOTAL-VOID.  EL325
122702     ADD WS-ACCT-IU-COUNT-VOID     TO  WS-CARRIER-IU-COUNT-VOID.  EL325
121203     ADD WS-ACCT-GP-TOTAL          TO  WS-CARRIER-GP-TOTAL.       EL325
121203     ADD WS-ACCT-GP-COUNT          TO  WS-CARRIER-GP-COUNT.       EL325
121203     ADD WS-ACCT-GP-TOTAL-VOID     TO  WS-CARRIER-GP-TOTAL-VOID.  EL325
121203     ADD WS-ACCT-GP-COUNT-VOID     TO  WS-CARRIER-GP-COUNT-VOID.  EL325

052614     ADD WS-ACCT-FL-TOTAL          TO  WS-CARRIER-FL-TOTAL.
052614     ADD WS-ACCT-FL-COUNT          TO  WS-CARRIER-FL-COUNT.
052614     ADD WS-ACCT-FL-TOTAL-VOID     TO  WS-CARRIER-FL-TOTAL-VOID.
052614     ADD WS-ACCT-FL-COUNT-VOID     TO  WS-CARRIER-FL-COUNT-VOID.

022122     ADD WS-ACCT-BR-TOTAL          TO  WS-CARRIER-BR-TOTAL.
022122     ADD WS-ACCT-BR-COUNT          TO  WS-CARRIER-BR-COUNT.
022122     ADD WS-ACCT-BR-TOTAL-VOID     TO  WS-CARRIER-BR-TOTAL-VOID.
022122     ADD WS-ACCT-BR-COUNT-VOID     TO  WS-CARRIER-BR-COUNT-VOID.
022122
022122     ADD WS-ACCT-HS-TOTAL          TO  WS-CARRIER-HS-TOTAL.
022122     ADD WS-ACCT-HS-COUNT          TO  WS-CARRIER-HS-COUNT.
022122     ADD WS-ACCT-HS-TOTAL-VOID     TO  WS-CARRIER-HS-TOTAL-VOID.
022122     ADD WS-ACCT-HS-COUNT-VOID     TO  WS-CARRIER-HS-COUNT-VOID.

100518     ADD WS-ACCT-OT-TOTAL          TO  WS-CARRIER-OT-TOTAL.
100518     ADD WS-ACCT-OT-COUNT          TO  WS-CARRIER-OT-COUNT.
100518     ADD WS-ACCT-OT-TOTAL-VOID     TO  WS-CARRIER-OT-TOTAL-VOID.
100518     ADD WS-ACCT-OT-COUNT-VOID     TO  WS-CARRIER-OT-COUNT-VOID.
00616                                                                   EL325
00617      MOVE ZERO                   TO  WS-ACCT-AH-TOTAL             EL325
00618                                      WS-ACCT-LF-TOTAL             EL325
122702                                     WS-ACCT-IU-TOTAL             EL325
052614                                     WS-ACCT-FL-TOTAL
022122                                     WS-ACCT-BR-TOTAL
022122                                     WS-ACCT-HS-TOTAL
100518                                     WS-ACCT-OT-TOTAL
121203                                     WS-ACCT-GP-TOTAL             EL325
00619                                      WS-ACCT-AH-COUNT             EL325
00620                                      WS-ACCT-LF-COUNT             EL325
122702                                     WS-ACCT-IU-COUNT
052614                                     WS-ACCT-FL-COUNT
022122                                     WS-ACCT-BR-COUNT
022122                                     WS-ACCT-HS-COUNT
100518                                     WS-ACCT-OT-COUNT
121203                                     WS-ACCT-GP-COUNT.            EL325
00621                                                                   EL325
00622      MOVE ZERO                   TO  WS-ACCT-AH-TOTAL-VOID        EL325
00623                                      WS-ACCT-LF-TOTAL-VOID        EL325
122702                                     WS-ACCT-IU-TOTAL-VOID        EL325
052614                                     WS-ACCT-FL-TOTAL-VOID
022122                                     WS-ACCT-BR-TOTAL-VOID
022122                                     WS-ACCT-HS-TOTAL-VOID
100518                                     WS-ACCT-OT-TOTAL-VOID
121203                                     WS-ACCT-GP-TOTAL-VOID        EL325
00624                                      WS-ACCT-AH-COUNT-VOID        EL325
00625                                      WS-ACCT-LF-COUNT-VOID        EL325
122702                                     WS-ACCT-IU-COUNT-VOID
052614                                     WS-ACCT-FL-COUNT-VOID
022122                                     WS-ACCT-BR-COUNT-VOID
022122                                     WS-ACCT-HS-COUNT-VOID
100518                                     WS-ACCT-OT-COUNT-VOID
121203                                     WS-ACCT-GP-COUNT-VOID.       EL325
00626                                                                   EL325
00627      MOVE +99                    TO  WS-LINE-COUNT.               EL325
00628                                                                   EL325
00629      IF SWR-BA-ACCOUNT NOT = HIGH-VALUES                          EL325
00630          MOVE DTE-CLASIC-COMPANY-CD                               EL325
00631                                    TO  WS-LAST-COMPANY            EL325
00632          MOVE SWR-SB-CARRIER       TO  WS-LAST-CARRIER            EL325
00633          MOVE SWR-BA-ACCOUNT       TO  WS-LAST-ACCOUNT            EL325
00634                                        WS-H6-ACCOUNT              EL325
00635          MOVE SWR-BA-STATE         TO  WS-LAST-STATE              EL325
00636          MOVE SWR-BA-GROUPING      TO  WS-LAST-GROUPING           EL325
122702         IF SWR-BA-SYSTEM-IDENTIFIER IS EQUAL TO 'CV'             EL325
122702             PERFORM 8300-GET-PRODUCER-ADDR                       EL325
122702             MOVE SWR-BA-ACCOUNT   TO  WS-CONTROL-ACCOUNT         EL325
122702         ELSE                                                     EL325
00641              PERFORM 8200-GET-ACCT-ADDR 
00642              MOVE SWR-BA-ACCOUNT   TO  WS-CONTROL-ACCOUNT
               END-IF
           END-IF
00643                                                                   EL325
00644      EJECT                                                        EL325
00645      IF SWR-SB-CARRIER = WS-CONTROL-CARRIER                       EL325
00646          GO TO 3200-SORT-OUTPUT-PROCEDURE.                        EL325
00647                                                                   EL325
00648      COMPUTE WS-CARRIER-COUNT = WS-CARRIER-AH-COUNT +             EL325
00649                                 WS-CARRIER-LF-COUNT +             EL325
121203                                WS-CARRIER-IU-COUNT +             EL325
052614                                WS-CARRIER-FL-COUNT +
022122                                WS-CARRIER-BR-COUNT +
022122                                WS-CARRIER-HS-COUNT +
100518                                WS-CARRIER-OT-COUNT +
121203                                WS-CARRIER-GP-COUNT.              EL325
00650      COMPUTE WS-CARRIER-TOTAL = WS-CARRIER-AH-TOTAL +             EL325
00651                                 WS-CARRIER-LF-TOTAL +             EL325
121203                                WS-CARRIER-IU-TOTAL +             EL325
052614                                WS-CARRIER-FL-TOTAL +
022122                                WS-CARRIER-BR-TOTAL +
022122                                WS-CARRIER-HS-TOTAL +
100518                                WS-CARRIER-OT-TOTAL +
121203                                WS-CARRIER-GP-TOTAL.              EL325
00652                                                                      CL**2
00653      MOVE '-'                     TO  WS-DETAIL1.                  EL325
00654      MOVE +1                      TO  WS-HEADING-SW.               EL325
00655      MOVE 'TOTAL CARRIER'         TO  WS-T1-MESSAGE.               EL325
00656      MOVE WS-CARRIER-COUNT        TO  WS-T1-COUNT.                 EL325
00657      MOVE WS-CARRIER-TOTAL        TO  WS-T1-AMOUNT.                EL325
00658      MOVE AH-OVERRIDE-L6          TO  WS-TA-MESSAGE.               EL325
00659      MOVE WS-CARRIER-AH-COUNT     TO  WS-TA-COUNT.                 EL325
00660      MOVE WS-CARRIER-AH-TOTAL     TO  WS-TA-AMOUNT.                EL325
00661      MOVE LIFE-OVERRIDE-L6        TO  WS-TL-MESSAGE.               EL325
100518     ADD WS-CARRIER-LF-COUNT   WS-CARRIER-OT-COUNT                 EL325
100518        GIVING  WS-TL-COUNT.                                       EL325
100518     ADD WS-CARRIER-LF-TOTAL   WS-CARRIER-OT-TOTAL                 EL325
100518        GIVING  WS-TL-AMOUNT.                                      EL325
010716     IF DTE-CLIENT = 'DCC' or 'VPP'
122702         MOVE 'IU    '            TO  WS-TU-MESSAGE                EL325
122702         MOVE WS-CARRIER-IU-COUNT TO  WS-TU-COUNT                  EL325
122702         MOVE WS-CARRIER-IU-TOTAL TO  WS-TU-AMOUNT
122702     END-IF.
00664      MOVE WS-DETAIL1              TO  PRT.                         EL325
00665      PERFORM WRITE-A-LINE.                                        EL325
00666                                                                   EL325
00667      COMPUTE WS-CARRIER-COUNT-VOID = WS-CARRIER-AH-COUNT-VOID +   EL325
00668                                      WS-CARRIER-LF-COUNT-VOID +   EL325
121203                                     WS-CARRIER-IU-COUNT-VOID +   EL325
052614                                     WS-CARRIER-FL-COUNT-VOID +
022122                                     WS-CARRIER-BR-COUNT-VOID +
022122                                     WS-CARRIER-HS-COUNT-VOID +
100518                                     WS-CARRIER-OT-COUNT-VOID +
121203                                     WS-CARRIER-GP-COUNT-VOID.    EL325
00669      COMPUTE WS-CARRIER-TOTAL-VOID = WS-CARRIER-AH-TOTAL-VOID +   EL325
00670                                      WS-CARRIER-LF-TOTAL-VOID +   EL325
121203                                     WS-CARRIER-IU-TOTAL-VOID +   EL325
052614                                     WS-CARRIER-FL-TOTAL-VOID +
022122                                     WS-CARRIER-BR-TOTAL-VOID +
022122                                     WS-CARRIER-HS-TOTAL-VOID +
100518                                     WS-CARRIER-OT-TOTAL-VOID +
121203                                     WS-CARRIER-GP-TOTAL-VOID.    EL325
00671                                                                      CL**2
00672      IF  WS-CARRIER-COUNT-VOID NOT = ZERO                         EL325
00673          MOVE     SPACES                    TO  WS-DETAIL1       
00674          MOVE     '      VOID '             TO  WS-T1-MESSAGE   
00675          MOVE     WS-CARRIER-COUNT-VOID     TO  WS-T1-COUNT    
00676          MOVE     WS-CARRIER-TOTAL-VOID     TO  WS-T1-AMOUNT  
00677          MOVE     '    VOID'                TO  WS-TA-MESSAGE
00678          MOVE     WS-CARRIER-AH-COUNT-VOID  TO  WS-TA-COUNT 
00679          MOVE     WS-CARRIER-AH-TOTAL-VOID  TO  WS-TA-AMOUNT   
00680          MOVE     '    VOID'                TO  WS-TL-MESSAGE 
100518         ADD WS-CARRIER-LF-COUNT-VOID WS-CARRIER-OT-COUNT-VOID
100518            GIVING WS-TL-COUNT
100518         ADD WS-CARRIER-LF-TOTAL-VOID WS-CARRIER-OT-TOTAL-VOID
100518            GIVING WS-TL-AMOUNT
010716         IF DTE-CLIENT = 'DCC' or 'VPP'
122702             MOVE '    VOID'                TO  WS-TU-MESSAGE   
122702             MOVE WS-CARRIER-IU-COUNT-VOID  TO  WS-TU-COUNT    
122702             MOVE WS-CARRIER-IU-TOTAL-VOID  TO  WS-TU-AMOUNT
122702         END-IF 
00683          MOVE     WS-DETAIL1                TO  PRT       
00684          PERFORM  WRITE-A-LINE                                    EL325
00685          MOVE     SPACES                    TO  WS-DETAIL1   
00686          MOVE     '      NET    '           TO  WS-T1-MESSAGE   
00687          SUBTRACT WS-CARRIER-TOTAL-VOID FROM WS-CARRIER-TOTAL     EL325
00688                                          GIVING WS-T1-AMOUNT   
00689          MOVE     '     NET'                TO  WS-TA-MESSAGE     
00690          SUBTRACT WS-CARRIER-AH-TOTAL-VOID                        EL325
00691                    FROM WS-CARRIER-AH-TOTAL GIVING WS-TA-AMOUNT   EL325
00692          MOVE     '     NET'                TO  WS-TL-MESSAGE   
100518         COMPUTE WS-TL-AMOUNT =                                   EL325
100518            WS-CARRIER-LF-TOTAL + WS-CARRIER-OT-TOTAL             EL325
100518             - WS-CARRIER-LF-TOTAL-VOID                           EL325
100518             - WS-CARRIER-OT-TOTAL-VOID                           EL325
010716         IF DTE-CLIENT = 'DCC' or 'VPP'
122702             MOVE '     NET'                TO  WS-TU-MESSAGE  
122702             SUBTRACT WS-CARRIER-IU-TOTAL-VOID  
122702                 FROM WS-CARRIER-IU-TOTAL
122702                 GIVING WS-TU-AMOUNT
122702         END-IF
00695          MOVE     WS-DETAIL1                TO  PRT    
00696          PERFORM  WRITE-A-LINE.                                   EL325
052614
052614**WRITE GAP AND FAM
010716     IF DTE-CLIENT = 'DCC' or 'VPP'
052614         MOVE SPACES             TO  WS-DETAIL1
052614         IF WS-CARRIER-COUNT-VOID NOT = ZERO 
052614            MOVE '0'             TO  WS-DETAIL1
052614         END-IF
052614
052614         MOVE 'GAP   '           TO  WS-TG-MESSAGE
052614         MOVE WS-CARRIER-GP-COUNT   TO  WS-TG-COUNT
052614         MOVE WS-CARRIER-GP-TOTAL   TO  WS-TG-AMOUNT
052614
052614         MOVE 'FAM   '           TO  WS-TF-MESSAGE
052614         MOVE WS-CARRIER-FL-COUNT   TO  WS-TF-COUNT
052614         MOVE WS-CARRIER-FL-TOTAL   TO  WS-TF-AMOUNT
052614         MOVE WS-DETAIL1         TO  PRT
052614         PERFORM WRITE-A-LINE
052614
052614         IF  WS-CARRIER-COUNT-VOID NOT = ZERO
052614             MOVE     SPACES     TO   WS-DETAIL1
052614
052614             MOVE '    VOID'               TO WS-TG-MESSAGE
052614             MOVE WS-CARRIER-GP-COUNT-VOID TO WS-TG-COUNT
052614             MOVE WS-CARRIER-GP-TOTAL-VOID TO WS-TG-AMOUNT
052614
052614             MOVE '    VOID'               TO WS-TF-MESSAGE
052614             MOVE WS-CARRIER-FL-COUNT-VOID TO WS-TF-COUNT
052614             MOVE WS-CARRIER-FL-TOTAL-VOID TO WS-TF-AMOUNT
052614
052614             MOVE     WS-DETAIL1          TO   PRT
052614             PERFORM  WRITE-A-LINE
052614
052614             MOVE     SPACES              TO   WS-DETAIL1
052614
052614             MOVE '     NET'              TO   WS-TG-MESSAGE
052614             SUBTRACT WS-CARRIER-GP-TOTAL-VOID
052614                 FROM WS-CARRIER-GP-TOTAL
052614                 GIVING WS-TG-AMOUNT
052614
052614             MOVE '     NET'              TO   WS-TF-MESSAGE
052614             SUBTRACT WS-CARRIER-FL-TOTAL-VOID
052614                 FROM WS-CARRIER-FL-TOTAL
052614                 GIVING WS-TF-AMOUNT
052614
052614             MOVE     WS-DETAIL1          TO   PRT
052614             PERFORM  WRITE-A-LINE
052614         END-IF
052614     END-IF.
00697                                                                   EL325



022122**WRITE BRV AND HOS
022122     IF DTE-CLIENT = 'DCC' or 'VPP'
022122         MOVE SPACES             TO  WS-DETAIL1
022122         IF WS-CARRIER-COUNT-VOID NOT = ZERO 
022122            MOVE '0'             TO  WS-DETAIL1
022122         END-IF
022122
022122         MOVE 'BRV   '           TO  WS-TB-MESSAGE
022122         MOVE WS-CARRIER-BR-COUNT   TO  WS-TB-COUNT
022122         MOVE WS-CARRIER-BR-TOTAL   TO  WS-TB-AMOUNT
022122
022122         MOVE 'HOS   '           TO  WS-TH-MESSAGE
022122         MOVE WS-CARRIER-HS-COUNT   TO  WS-TH-COUNT
022122         MOVE WS-CARRIER-HS-TOTAL   TO  WS-TH-AMOUNT
022122         MOVE WS-DETAIL1         TO  PRT
022122         PERFORM WRITE-A-LINE
022122
022122         IF  WS-CARRIER-COUNT-VOID NOT = ZERO
022122             MOVE     SPACES     TO   WS-DETAIL1
022122
022122             MOVE '    VOID'               TO WS-TB-MESSAGE
022122             MOVE WS-CARRIER-BR-COUNT-VOID TO WS-TB-COUNT
022122             MOVE WS-CARRIER-BR-TOTAL-VOID TO WS-TB-AMOUNT
022122
022122             MOVE '    VOID'               TO WS-TH-MESSAGE
022122             MOVE WS-CARRIER-HS-COUNT-VOID TO WS-TH-COUNT
022122             MOVE WS-CARRIER-HS-TOTAL-VOID TO WS-TH-AMOUNT
022122
022122             MOVE     WS-DETAIL1          TO   PRT
022122             PERFORM  WRITE-A-LINE
022122
022122             MOVE     SPACES              TO   WS-DETAIL1
022122
022122             MOVE '     NET'              TO   WS-TG-MESSAGE
022122             SUBTRACT WS-CARRIER-BR-TOTAL-VOID
022122                 FROM WS-CARRIER-BR-TOTAL
022122                 GIVING WS-TB-AMOUNT
022122
022122             MOVE '     NET'              TO   WS-TF-MESSAGE
022122             SUBTRACT WS-CARRIER-HS-TOTAL-VOID
022122                 FROM WS-CARRIER-HS-TOTAL
022122                 GIVING WS-TH-AMOUNT
022122
022122             MOVE     WS-DETAIL1          TO   PRT
022122             PERFORM  WRITE-A-LINE
022122         END-IF
022122     END-IF

           .
00699  3130-SORT-OUTPUT-PROCEDURE.                                      EL325
00700      IF    WS-CARRIER-REINS-COUNT EQUAL ZERO                      EL325
00701        AND WS-CARRIER-REINS-COUNT-VOID EQUAL ZERO                 EL325
00702            GO TO 3135-SORT-OUTPUT-PROCEDURE.                      EL325
00703                                                                   EL325
00704      MOVE     '0'                          TO  WS-DETAIL1.        EL325
00705      MOVE     'TOTAL CLMS - REIN ONLY'     TO  WS-T1-MESSAGE.     EL325
00706      MOVE     WS-CARRIER-REINS-COUNT       TO  WS-T1-COUNT.       EL325
00707      MOVE     WS-CARRIER-REINS-TOTAL       TO  WS-T1-AMOUNT.      EL325
00708      MOVE     WS-DETAIL1                   TO  PRT.               EL325
00709      PERFORM  WRITE-A-LINE.                                       EL325
00710      MOVE     SPACES                       TO  WS-DETAIL1.        EL325
00711      MOVE     '         VOID        '      TO  WS-T1-MESSAGE.     EL325
00712      MOVE     WS-CARRIER-REINS-COUNT-VOID  TO  WS-T1-COUNT.       EL325
00713      MOVE     WS-CARRIER-REINS-TOTAL-VOID  TO  WS-T1-AMOUNT.      EL325
00714      MOVE     WS-DETAIL1                   TO  PRT.               EL325
00715      PERFORM  WRITE-A-LINE.                                       EL325
00716      MOVE     SPACES                       TO  WS-DETAIL1.        EL325
00717      MOVE     '         NET         '      TO  WS-T1-MESSAGE.     EL325
00718      SUBTRACT WS-CARRIER-REINS-TOTAL-VOID FROM                    EL325
00719               WS-CARRIER-REINS-TOTAL     GIVING WS-T1-AMOUNT.     EL325
00720      MOVE     WS-DETAIL1                   TO  PRT.               EL325
00721      PERFORM  WRITE-A-LINE.                                       EL325
00722      MOVE     SPACES                       TO  WS-DETAIL1.        EL325
00723                                                                   EL325
00724  3135-SORT-OUTPUT-PROCEDURE.                                      EL325
00725                                                                   EL325
00726      ADD WS-CARRIER-LF-COUNT         TO  WS-FINAL-LF-COUNT.       EL325
00727      ADD WS-CARRIER-LF-TOTAL         TO  WS-FINAL-LF-TOTAL.       EL325
00728      ADD WS-CARRIER-LF-COUNT-VOID    TO  WS-FINAL-LF-COUNT-VOID.  EL325
00729      ADD WS-CARRIER-LF-TOTAL-VOID    TO  WS-FINAL-LF-TOTAL-VOID.  EL325
00730      ADD WS-CARRIER-AH-COUNT           TO  WS-FINAL-AH-COUNT.     EL325
00731      ADD WS-CARRIER-AH-TOTAL           TO  WS-FINAL-AH-TOTAL.     EL325
00732      ADD WS-CARRIER-AH-COUNT-VOID      TO  WS-FINAL-AH-COUNT-VOID.EL325
00733      ADD WS-CARRIER-AH-TOTAL-VOID      TO  WS-FINAL-AH-TOTAL-VOID.EL325
122702     ADD WS-CARRIER-IU-COUNT           TO  WS-FINAL-IU-COUNT.     EL325
122702     ADD WS-CARRIER-IU-TOTAL           TO  WS-FINAL-IU-TOTAL.     EL325
122702     ADD WS-CARRIER-IU-COUNT-VOID      TO  WS-FINAL-IU-COUNT-VOID.EL325
122702     ADD WS-CARRIER-IU-TOTAL-VOID      TO  WS-FINAL-IU-TOTAL-VOID.EL325
121203     ADD WS-CARRIER-GP-COUNT           TO  WS-FINAL-GP-COUNT.     EL325
121203     ADD WS-CARRIER-GP-TOTAL           TO  WS-FINAL-GP-TOTAL.     EL325
121203     ADD WS-CARRIER-GP-COUNT-VOID      TO  WS-FINAL-GP-COUNT-VOID.EL325
121203     ADD WS-CARRIER-GP-TOTAL-VOID      TO  WS-FINAL-GP-TOTAL-VOID.EL325

052614     ADD WS-CARRIER-FL-COUNT           TO  WS-FINAL-FL-COUNT.
052614     ADD WS-CARRIER-FL-TOTAL           TO  WS-FINAL-FL-TOTAL.
052614     ADD WS-CARRIER-FL-COUNT-VOID      TO  WS-FINAL-FL-COUNT-VOID.
052614     ADD WS-CARRIER-FL-TOTAL-VOID      TO  WS-FINAL-FL-TOTAL-VOID.

022122     ADD WS-CARRIER-BR-COUNT           TO  WS-FINAL-BR-COUNT.
022122     ADD WS-CARRIER-BR-TOTAL           TO  WS-FINAL-BR-TOTAL.
022122     ADD WS-CARRIER-BR-COUNT-VOID      TO  WS-FINAL-BR-COUNT-VOID.
022122     ADD WS-CARRIER-BR-TOTAL-VOID      TO  WS-FINAL-BR-TOTAL-VOID.

022122     ADD WS-CARRIER-HS-COUNT           TO  WS-FINAL-HS-COUNT.
022122     ADD WS-CARRIER-HS-TOTAL           TO  WS-FINAL-HS-TOTAL.
022122     ADD WS-CARRIER-HS-COUNT-VOID      TO  WS-FINAL-HS-COUNT-VOID.
022122     ADD WS-CARRIER-HS-TOTAL-VOID      TO  WS-FINAL-HS-TOTAL-VOID.

100518     ADD WS-CARRIER-OT-COUNT           TO  WS-FINAL-OT-COUNT.
100518     ADD WS-CARRIER-OT-TOTAL           TO  WS-FINAL-OT-TOTAL.
100518     ADD WS-CARRIER-OT-COUNT-VOID      TO  WS-FINAL-OT-COUNT-VOID.
100518     ADD WS-CARRIER-OT-TOTAL-VOID      TO  WS-FINAL-OT-TOTAL-VOID.
00734      ADD WS-CARRIER-REINS-COUNT   TO  WS-FINAL-REINS-COUNT.       EL325
00735      ADD WS-CARRIER-REINS-TOTAL   TO  WS-FINAL-REINS-TOTAL.       EL325
00736      ADD WS-CARRIER-REINS-COUNT-VOID                              EL325
00737                                   TO  WS-FINAL-REINS-COUNT-VOID.  EL325
00738      ADD WS-CARRIER-REINS-TOTAL-VOID                              EL325
00739                                   TO  WS-FINAL-REINS-TOTAL-VOID.  EL325
00740                                                                   EL325
00741      MOVE ZERO               TO   WS-CARRIER-AH-COUNT             EL325
00742                                   WS-CARRIER-AH-TOTAL             EL325
00743                                   WS-CARRIER-AH-COUNT-VOID        EL325
00744                                   WS-CARRIER-AH-TOTAL-VOID        EL325
00745                                   WS-CARRIER-LF-COUNT             EL325
00746                                   WS-CARRIER-LF-TOTAL             EL325
00747                                   WS-CARRIER-LF-COUNT-VOID        EL325
00748                                   WS-CARRIER-LF-TOTAL-VOID        EL325
122702                                  WS-CARRIER-IU-COUNT             EL325
122702                                  WS-CARRIER-IU-TOTAL             EL325
122702                                  WS-CARRIER-IU-COUNT-VOID        EL325
122702                                  WS-CARRIER-IU-TOTAL-VOID        EL325
121203                                  WS-CARRIER-GP-COUNT             EL325
121203                                  WS-CARRIER-GP-TOTAL             EL325
121203                                  WS-CARRIER-GP-COUNT-VOID        EL325
121203                                  WS-CARRIER-GP-TOTAL-VOID        EL325

052614                                  WS-CARRIER-FL-COUNT
052614                                  WS-CARRIER-FL-TOTAL
052614                                  WS-CARRIER-FL-COUNT-VOID
052614                                  WS-CARRIER-FL-TOTAL-VOID

022122                                  WS-CARRIER-BR-COUNT
022122                                  WS-CARRIER-BR-TOTAL
022122                                  WS-CARRIER-BR-COUNT-VOID
022122                                  WS-CARRIER-BR-TOTAL-VOID

022122                                  WS-CARRIER-HS-COUNT
022122                                  WS-CARRIER-HS-TOTAL
022122                                  WS-CARRIER-HS-COUNT-VOID
022122                                  WS-CARRIER-HS-TOTAL-VOID

100518                                  WS-CARRIER-OT-COUNT
100518                                  WS-CARRIER-OT-TOTAL
100518                                  WS-CARRIER-OT-COUNT-VOID
100518                                  WS-CARRIER-OT-TOTAL-VOID
00749                                   WS-CARRIER-REINS-COUNT          EL325
00750                                   WS-CARRIER-REINS-TOTAL          EL325
00751                                   WS-CARRIER-REINS-COUNT-VOID     EL325
00752                                   WS-CARRIER-REINS-TOTAL-VOID.    EL325
00753                                                                   EL325
00754      IF SWR-SB-CARRIER NOT = HIGH-VALUES                          EL325
00755          MOVE +99                TO  WS-LINE-COUNT                EL325
00756          MOVE SWR-SB-CARRIER     TO  WS-CONTROL-CARRIER           EL325
00757          PERFORM 8100-GET-CARRIER-NAME.                           EL325
00758                                                                   EL325
00759      EJECT                                                        EL325
00760      IF SWR-SB-CARRIER NOT = HIGH-VALUES                          EL325
00761          GO TO 3200-SORT-OUTPUT-PROCEDURE.                        EL325
00762                                                                   EL325
00763      COMPUTE WS-FINAL-COUNT = WS-FINAL-AH-COUNT +                 EL325
00764                               WS-FINAL-LF-COUNT +                 EL325
121203                              WS-FINAL-IU-COUNT +                 EL325
052614                              WS-FINAL-FL-COUNT +
022122                              WS-FINAL-BR-COUNT +
022122                              WS-FINAL-HS-COUNT +
100518                              WS-FINAL-OT-COUNT +
121203                              WS-FINAL-GP-COUNT.                  EL325
00765      COMPUTE WS-FINAL-TOTAL = WS-FINAL-AH-TOTAL +                 EL325
00766                               WS-FINAL-LF-TOTAL +                 EL325
121203                              WS-FINAL-IU-TOTAL +                 EL325
052614                              WS-FINAL-FL-TOTAL +
022122                              WS-FINAL-BR-TOTAL +
022122                              WS-FINAL-HS-TOTAL +
100518                              WS-FINAL-OT-TOTAL +
121203                              WS-FINAL-GP-TOTAL.                  EL325
00767                                                                      CL**2
00768      MOVE     +1                     TO   WS-HEADING-SW.          EL325
00769      MOVE     '-'                    TO   WS-DETAIL1.             EL325
00770      MOVE     'TOTAL REPORT'         TO   WS-T1-MESSAGE.          EL325
00771      MOVE     WS-FINAL-COUNT         TO   WS-T1-COUNT.            EL325
00772      MOVE     WS-FINAL-TOTAL         TO   WS-T1-AMOUNT.           EL325
00773      MOVE     AH-OVERRIDE-L6         TO   WS-TA-MESSAGE.          EL325
00774      MOVE     WS-FINAL-AH-COUNT      TO   WS-TA-COUNT.            EL325
00775      MOVE     WS-FINAL-AH-TOTAL      TO   WS-TA-AMOUNT.           EL325
00776      MOVE     LIFE-OVERRIDE-L6       TO   WS-TL-MESSAGE.          EL325
100518     ADD WS-FINAL-LF-COUNT     WS-FINAL-OT-COUNT                  EL325
100518        GIVING WS-TL-COUNT.                                       EL325
100518     ADD WS-FINAL-LF-TOTAL     WS-FINAL-OT-TOTAL                  EL325
100518        GIVING WS-TL-AMOUNT.                                      EL325
010716     IF DTE-CLIENT = 'DCC' or 'VPP'
122702         MOVE 'IU    '               TO   WS-TU-MESSAGE           EL325
122702         MOVE WS-FINAL-IU-COUNT      TO   WS-TU-COUNT             EL325
122702         MOVE WS-FINAL-IU-TOTAL      TO   WS-TU-AMOUNT 
122702     END-IF.
00779      MOVE     WS-DETAIL1             TO   PRT.                    EL325
00780      PERFORM  WRITE-A-LINE.                                       EL325
00781                                                                   EL325
00782      COMPUTE WS-FINAL-COUNT-VOID = WS-FINAL-AH-COUNT-VOID +       EL325
00783                                    WS-FINAL-LF-COUNT-VOID +       EL325
121203                                   WS-FINAL-IU-COUNT-VOID +       EL325
052614                                   WS-FINAL-FL-COUNT-VOID +
022122                                   WS-FINAL-BR-COUNT-VOID +
022122                                   WS-FINAL-HS-COUNT-VOID +
100518                                   WS-FINAL-OT-COUNT-VOID +
121203                                   WS-FINAL-GP-COUNT-VOID.        EL325
00784      COMPUTE WS-FINAL-TOTAL-VOID = WS-FINAL-AH-TOTAL-VOID +       EL325
00785                                    WS-FINAL-LF-TOTAL-VOID +       EL325
121203                                   WS-FINAL-IU-TOTAL-VOID +       EL325
052614                                   WS-FINAL-FL-TOTAL-VOID +
022122                                   WS-FINAL-BR-TOTAL-VOID +
022122                                   WS-FINAL-HS-TOTAL-VOID +
100518                                   WS-FINAL-OT-TOTAL-VOID +
121203                                   WS-FINAL-GP-TOTAL-VOID.        EL325
00786                                                                      CL**2
00787      MOVE     SPACES                 TO   WS-DETAIL1.             EL325
00788      MOVE     '      VOID '          TO   WS-T1-MESSAGE.          EL325
00789      MOVE     WS-FINAL-COUNT-VOID    TO   WS-T1-COUNT.            EL325
00790      MOVE     WS-FINAL-TOTAL-VOID    TO   WS-T1-AMOUNT.           EL325
00791      MOVE     '    VOID'             TO   WS-TA-MESSAGE.          EL325
00792      MOVE     WS-FINAL-AH-COUNT-VOID TO   WS-TA-COUNT.            EL325
00793      MOVE     WS-FINAL-AH-TOTAL-VOID TO   WS-TA-AMOUNT.           EL325
00794      MOVE     '    VOID'             TO   WS-TL-MESSAGE.          EL325
00795      ADD WS-FINAL-LF-COUNT-VOID WS-FINAL-OT-COUNT-VOID
00795         GIVING WS-TL-COUNT.
00796      ADD WS-FINAL-LF-TOTAL-VOID WS-FINAL-OT-TOTAL-VOID
00796         GIVING WS-TL-AMOUNT.
010716     IF DTE-CLIENT = 'DCC' or 'VPP'
122702         MOVE '    VOID'             TO   WS-TU-MESSAGE
122702         MOVE WS-FINAL-IU-COUNT-VOID TO   WS-TU-COUNT
122702         MOVE WS-FINAL-IU-TOTAL-VOID TO   WS-TU-AMOUNT
122702     END-IF
00797      MOVE     WS-DETAIL1             TO   PRT.                    EL325
00798      PERFORM  WRITE-A-LINE.                                       EL325

070714     if me-do-update
070714        compute hld-325-clms-tot =
070714           ws-final-total - ws-final-total-void
070714     end-if

00800      MOVE     SPACES                 TO   WS-DETAIL1.             EL325
00801      MOVE     '      NET    '        TO   WS-T1-MESSAGE.          EL325
00802      SUBTRACT WS-FINAL-TOTAL-VOID FROM    WS-FINAL-TOTAL          EL325
00803                                    GIVING WS-T1-AMOUNT.           EL325
00804      MOVE     '     NET'             TO   WS-TA-MESSAGE.          EL325
00805      SUBTRACT WS-FINAL-AH-TOTAL-VOID FROM WS-FINAL-AH-TOTAL       EL325
00806                                    GIVING WS-TA-AMOUNT.           EL325
00807      MOVE     '     NET'             TO   WS-TL-MESSAGE.          EL325
100518     COMPUTE WS-TL-AMOUNT =                                       EL325
100518        WS-FINAL-LF-TOTAL + WS-FINAL-OT-TOTAL                     EL325
100518         - WS-FINAL-LF-TOTAL-VOID                                 EL325
100518         - WS-FINAL-OT-TOTAL-VOID.                                EL325
010716     IF DTE-CLIENT = 'DCC' or 'VPP'
122702         MOVE '     NET'             TO   WS-TU-MESSAGE
122702         SUBTRACT WS-FINAL-IU-TOTAL-VOID 
122702             FROM WS-FINAL-IU-TOTAL 
122702             GIVING WS-TU-AMOUNT
122702     END-IF.
00810      MOVE     WS-DETAIL1             TO   PRT.                    EL325
00811      PERFORM  WRITE-A-LINE.                                       EL325
052614
052614**WRITE GAP AND FAM
010716     IF DTE-CLIENT = 'DCC' or 'VPP'
052614         MOVE '0'                TO  WS-DETAIL1
052614
052614         MOVE 'GAP   '           TO  WS-TG-MESSAGE
052614         MOVE WS-FINAL-GP-COUNT  TO  WS-TG-COUNT
052614         MOVE WS-FINAL-GP-TOTAL  TO  WS-TG-AMOUNT
052614
052614         MOVE 'FAM   '           TO  WS-TF-MESSAGE
052614         MOVE WS-FINAL-FL-COUNT  TO  WS-TF-COUNT
052614         MOVE WS-FINAL-FL-TOTAL  TO  WS-TF-AMOUNT
052614         MOVE WS-DETAIL1         TO  PRT
052614         PERFORM WRITE-A-LINE
052614
052614         MOVE     SPACES         TO   WS-DETAIL1
052614
052614         MOVE '    VOID'             TO WS-TG-MESSAGE
052614         MOVE WS-FINAL-GP-COUNT-VOID TO WS-TG-COUNT
052614         MOVE WS-FINAL-GP-TOTAL-VOID TO WS-TG-AMOUNT
052614
052614         MOVE '    VOID'             TO WS-TF-MESSAGE
052614         MOVE WS-FINAL-FL-COUNT-VOID TO WS-TF-COUNT
052614         MOVE WS-FINAL-FL-TOTAL-VOID TO WS-TF-AMOUNT
052614
052614         MOVE     WS-DETAIL1         TO   PRT
052614         PERFORM  WRITE-A-LINE
052614
052614         MOVE     SPACES             TO   WS-DETAIL1
052614
052614         MOVE '     NET'             TO   WS-TG-MESSAGE
052614         SUBTRACT WS-FINAL-GP-TOTAL-VOID
052614             FROM WS-FINAL-GP-TOTAL
052614             GIVING WS-TG-AMOUNT
052614
052614         MOVE '     NET'             TO   WS-TF-MESSAGE
052614         SUBTRACT WS-FINAL-FL-TOTAL-VOID
052614             FROM WS-FINAL-FL-TOTAL
052614             GIVING WS-TF-AMOUNT
052614
052614         MOVE     WS-DETAIL1         TO   PRT
052614         PERFORM  WRITE-A-LINE
052614     END-IF.

022122**WRITE BRV AND HOS
022122     IF DTE-CLIENT = 'DCC' or 'VPP'
022122         MOVE '0'                TO  WS-DETAIL1
022122
022122         MOVE 'BRV   '           TO  WS-TB-MESSAGE
022122         MOVE WS-FINAL-BR-COUNT  TO  WS-TB-COUNT
022122         MOVE WS-FINAL-BR-TOTAL  TO  WS-TB-AMOUNT
022122
022122         MOVE 'HOS   '           TO  WS-TH-MESSAGE
022122         MOVE WS-FINAL-HS-COUNT  TO  WS-TH-COUNT
022122         MOVE WS-FINAL-HS-TOTAL  TO  WS-TH-AMOUNT
022122         MOVE WS-DETAIL1         TO  PRT
022122         PERFORM WRITE-A-LINE
022122
022122         MOVE     SPACES         TO   WS-DETAIL1
022122
022122         MOVE '    VOID'             TO WS-TB-MESSAGE
022122         MOVE WS-FINAL-BR-COUNT-VOID TO WS-TB-COUNT
022122         MOVE WS-FINAL-BR-TOTAL-VOID TO WS-TB-AMOUNT
022122
022122         MOVE '    VOID'             TO WS-TH-MESSAGE
022122         MOVE WS-FINAL-HS-COUNT-VOID TO WS-TH-COUNT
022122         MOVE WS-FINAL-HS-TOTAL-VOID TO WS-TH-AMOUNT
022122
022122         MOVE     WS-DETAIL1         TO   PRT
022122         PERFORM  WRITE-A-LINE
022122
022122         MOVE     SPACES             TO   WS-DETAIL1
022122
022122         MOVE '     NET'             TO   WS-TB-MESSAGE
022122         SUBTRACT WS-FINAL-BR-TOTAL-VOID
022122             FROM WS-FINAL-BR-TOTAL
022122             GIVING WS-TB-AMOUNT
022122
022122         MOVE '     NET'             TO   WS-TH-MESSAGE
022122         SUBTRACT WS-FINAL-HS-TOTAL-VOID
022122             FROM WS-FINAL-HS-TOTAL
022122             GIVING WS-TH-AMOUNT
022122
022122         MOVE     WS-DETAIL1         TO   PRT
022122         PERFORM  WRITE-A-LINE
022122     END-IF

           .
00814  3150-SORT-OUTPUT-PROCEDURE.                                      EL325
00815      IF    WS-FINAL-REINS-COUNT       = ZERO                      EL325
00816        AND WS-FINAL-REINS-COUNT-VOID = ZERO                       EL325
00817            GO TO 3155-SORT-OUTPUT-PROCEDURE.                      EL325
00818                                                                   EL325
00819      MOVE     '0'                        TO  WS-DETAIL1.          EL325
00820      MOVE     'TOTAL REPORT REINS  '     TO  WS-T1-MESSAGE.       EL325
00821      MOVE     WS-FINAL-REINS-COUNT       TO  WS-T1-COUNT.         EL325
00822      MOVE     WS-FINAL-REINS-TOTAL       TO  WS-T1-AMOUNT.        EL325
00823      MOVE     WS-DETAIL1                 TO  PRT.                 EL325
00824      PERFORM  WRITE-A-LINE.                                       EL325
00825      MOVE     SPACES                     TO  WS-DETAIL1.          EL325
00826      MOVE     '      VOID          '     TO  WS-T1-MESSAGE.       EL325
00827      MOVE     WS-FINAL-REINS-COUNT-VOID  TO  WS-T1-COUNT.         EL325
00828      MOVE     WS-FINAL-REINS-TOTAL-VOID  TO  WS-T1-AMOUNT.        EL325
00829      MOVE     WS-DETAIL1                 TO  PRT.                 EL325
00830      PERFORM  WRITE-A-LINE.                                       EL325
00831      MOVE     SPACES                     TO  WS-DETAIL1.          EL325
00832      MOVE     '      NET           '     TO  WS-T1-MESSAGE.       EL325
00833      SUBTRACT WS-FINAL-REINS-TOTAL-VOID  FROM                     EL325
00834               WS-FINAL-REINS-TOTAL GIVING  WS-T1-AMOUNT.          EL325
00835      MOVE     WS-DETAIL1                 TO  PRT.                 EL325
00836      PERFORM  WRITE-A-LINE.                                       EL325
00837                                                                   EL325
00838  3155-SORT-OUTPUT-PROCEDURE.                                      EL325
00839                                                                   EL325
00840      GO TO 3900-EXIT.                                             EL325
00841                                                                   EL325
00842      EJECT                                                        EL325
00843  3200-SORT-OUTPUT-PROCEDURE.                                      EL325
00844      MOVE    SWR-BA-CHECK-WRITTEN-DT  TO  DC-BIN-DATE-1.          EL325
00845      MOVE    SPACES                   TO  DC-OPTION-CODE.         EL325
00846      PERFORM 8500-DATE-CONVERSION.                                EL325
00847      MOVE    DC-GREG-DATE-1-EDIT      TO  WS-WORK-DATE.           EL325
00848      MOVE    '0'                      TO  WS-DETAIL1.             EL325
00849      MOVE    SWR-SB-CHECK-NO          TO  WS-D1-CHECK-NO.         EL325
00850                                                                   EL325
00851      IF  (WS-WORK-MO = RUN-MO AND                                 EL325
00852           WS-WORK-YR = RUN-YR)                                    EL325
00853           IF  SWR-BA-VOID-DT = LOW-VALUES                         EL325
00854               MOVE SWR-BA-PAYMENT-AMOUNT  TO  WS-D1-AMOUNT        EL325
00855           ELSE                                                    EL325
00856               MULTIPLY SWR-BA-PAYMENT-AMOUNT BY -1                EL325
00857                        GIVING  WS-D1-AMOUNT                       EL325
00858      ELSE                                                         EL325
00859          IF  SWR-BA-VOID-DT = LOW-VALUES                          EL325
00860              MOVE SWR-BA-PAYMENT-AMOUNT  TO  WS-D1-AMOUNT         EL325
00861          ELSE                                                     EL325
00862              MULTIPLY SWR-BA-PAYMENT-AMOUNT BY -1                 EL325
00863                       GIVING  WS-D1-AMOUNT.                       EL325
00864                                                                   EL325
00865      IF SWR-BA-CERT-STATUS = '9'                                  EL325
00866          IF  (WS-WORK-MO = RUN-MO AND                             EL325
00867              WS-WORK-YR = RUN-YR)                                 EL325
00868              IF SWR-BA-VOID-DT = LOW-VALUES                       EL325
00869                 ADD SWR-BA-PAYMENT-AMOUNT                         EL325
00870                                       TO  WS-CARRIER-REINS-TOTAL  EL325
00871                 ADD +1                TO  WS-CARRIER-REINS-COUNT  EL325
00872                 GO TO 3250-BYPASS-REINS-ONLY                      EL325
00873              ELSE                                                 EL325
00874                 ADD SWR-BA-PAYMENT-AMOUNT                         EL325
00875                                  TO  WS-CARRIER-REINS-TOTAL-VOID  EL325
00876                 ADD +1           TO  WS-CARRIER-REINS-COUNT-VOID  EL325
00877                 GO TO 3250-BYPASS-REINS-ONLY                      EL325
00878          ELSE                                                     EL325
00879              IF SWR-BA-VOID-DT = LOW-VALUES                       EL325
00880                 ADD SWR-BA-PAYMENT-AMOUNT                         EL325
00881                                  TO  WS-CARRIER-REINS-TOTAL       EL325
00882                 ADD +1           TO  WS-CARRIER-REINS-COUNT       EL325
00883                 GO TO 3250-BYPASS-REINS-ONLY                      EL325
00884              ELSE                                                 EL325
00885                 ADD SWR-BA-PAYMENT-AMOUNT                         EL325
00886                             TO  WS-CARRIER-REINS-TOTAL-VOID       EL325
00887                 ADD +1      TO  WS-CARRIER-REINS-COUNT-VOID       EL325
00888                 GO TO 3250-BYPASS-REINS-ONLY.                     EL325
00889                                                                   EL325
00890      IF  (WS-WORK-MO = RUN-MO AND                                 EL325
00891          WS-WORK-YR = RUN-YR)                                     EL325
00892          IF  SWR-BA-VOID-DT = LOW-VALUES                          EL325
122702             EVALUATE TRUE
122702             WHEN SWR-BA-CLAIM-TYPE = AH-OVERRIDE-L1  
00894                  ADD SWR-BA-PAYMENT-AMOUNT TO WS-ACCT-AH-TOTAL    EL325
00895                  ADD +1                    TO WS-ACCT-AH-COUNT    EL325
 
122702             WHEN SWR-BA-CLAIM-TYPE = LIFE-OVERRIDE-L1
00897                  ADD SWR-BA-PAYMENT-AMOUNT TO WS-ACCT-LF-TOTAL    EL325
00898                  ADD +1                    TO WS-ACCT-LF-COUNT    EL325

122702             WHEN SWR-BA-CLAIM-TYPE = 'I'
122702                 ADD SWR-BA-PAYMENT-AMOUNT TO WS-ACCT-IU-TOTAL
122702                 ADD +1                    TO WS-ACCT-IU-COUNT
121203
121203             WHEN SWR-BA-CLAIM-TYPE = 'G'
121203                 ADD SWR-BA-PAYMENT-AMOUNT TO WS-ACCT-GP-TOTAL
121203                 ADD +1                    TO WS-ACCT-GP-COUNT
052614
052614             WHEN SWR-BA-CLAIM-TYPE = 'F'
052614                 ADD SWR-BA-PAYMENT-AMOUNT TO WS-ACCT-FL-TOTAL
052614                 ADD +1                    TO WS-ACCT-FL-COUNT
052614
022122             WHEN SWR-BA-CLAIM-TYPE = 'B'
022122                 ADD SWR-BA-PAYMENT-AMOUNT TO WS-ACCT-BR-TOTAL
022122                 ADD +1                    TO WS-ACCT-BR-COUNT
022122
022122             WHEN SWR-BA-CLAIM-TYPE = 'H'
022122                 ADD SWR-BA-PAYMENT-AMOUNT TO WS-ACCT-HS-TOTAL
022122                 ADD +1                    TO WS-ACCT-HS-COUNT
052614
100518             WHEN SWR-BA-CLAIM-TYPE = 'O'
100518                 ADD SWR-BA-PAYMENT-AMOUNT TO WS-ACCT-OT-TOTAL
100518                 ADD +1                    TO WS-ACCT-OT-COUNT
122702             END-EVALUATE
00899          ELSE                                                     EL325
122702             EVALUATE TRUE
122702             WHEN SWR-BA-CLAIM-TYPE = AH-OVERRIDE-L1   
00901                  ADD SWR-BA-PAYMENT-AMOUNT                        EL325
00902                                        TO WS-ACCT-AH-TOTAL-VOID 
00903                  ADD +1                TO WS-ACCT-AH-COUNT-VOID

122702             WHEN SWR-BA-CLAIM-TYPE = LIFE-OVERRIDE-L1
00905                  ADD SWR-BA-PAYMENT-AMOUNT                        EL325
00906                                        TO WS-ACCT-LF-TOTAL-VOID  
00907                  ADD +1                TO WS-ACCT-LF-COUNT-VOID

122702             WHEN SWR-BA-CLAIM-TYPE = 'I'
122702                 ADD SWR-BA-PAYMENT-AMOUNT 
122702                                       TO WS-ACCT-IU-TOTAL-VOID
122702                 ADD +1                TO WS-ACCT-IU-COUNT-VOID
121203
121203             WHEN SWR-BA-CLAIM-TYPE = 'G'
121203                 ADD SWR-BA-PAYMENT-AMOUNT 
121203                                       TO WS-ACCT-GP-TOTAL-VOID
121203                 ADD +1                TO WS-ACCT-GP-COUNT-VOID
052614
052614             WHEN SWR-BA-CLAIM-TYPE = 'F'
052614                 ADD SWR-BA-PAYMENT-AMOUNT 
052614                                       TO WS-ACCT-FL-TOTAL-VOID
052614                 ADD +1                TO WS-ACCT-FL-COUNT-VOID
100518
022122             WHEN SWR-BA-CLAIM-TYPE = 'B'
022122                 ADD SWR-BA-PAYMENT-AMOUNT 
022122                                       TO WS-ACCT-BR-TOTAL-VOID
022122                 ADD +1                TO WS-ACCT-BR-COUNT-VOID
022122
022122             WHEN SWR-BA-CLAIM-TYPE = 'H'
022122                 ADD SWR-BA-PAYMENT-AMOUNT 
022122                                       TO WS-ACCT-HS-TOTAL-VOID
022122                 ADD +1                TO WS-ACCT-HS-COUNT-VOID
100518
100518             WHEN SWR-BA-CLAIM-TYPE = 'O'
100518                 ADD SWR-BA-PAYMENT-AMOUNT
100518                                       TO WS-ACCT-OT-TOTAL-VOID
100518                 ADD +1                TO WS-ACCT-OT-COUNT-VOID
122702             END-EVALUATE.  

00909      IF  (WS-WORK-MO NOT = RUN-MO OR                              EL325
00910          WS-WORK-YR NOT = RUN-YR)                                 EL325
00911          IF  SWR-BA-VOID-DT = LOW-VALUES                          EL325
122702             EVALUATE TRUE
122702             WHEN SWR-BA-CLAIM-TYPE = AH-OVERRIDE-L1 
00913                  ADD SWR-BA-PAYMENT-AMOUNT                        EL325
00914                                        TO WS-ACCT-AH-TOTAL   
00915                  ADD +1                TO WS-ACCT-AH-COUNT  

122702             WHEN SWR-BA-CLAIM-TYPE = LIFE-OVERRIDE-L1
00917                  ADD SWR-BA-PAYMENT-AMOUNT                        EL325
00918                                        TO WS-ACCT-LF-TOTAL  
00919                  ADD +1                TO WS-ACCT-LF-COUNT 

122702             WHEN SWR-BA-CLAIM-TYPE = 'I'
122702                 ADD SWR-BA-PAYMENT-AMOUNT
122702                                       TO WS-ACCT-IU-TOTAL
122702                 ADD +1                TO WS-ACCT-IU-COUNT
121203
121203             WHEN SWR-BA-CLAIM-TYPE = 'G'
121203                 ADD SWR-BA-PAYMENT-AMOUNT
121203                                       TO WS-ACCT-GP-TOTAL
121203                 ADD +1                TO WS-ACCT-GP-COUNT
052614
052614             WHEN SWR-BA-CLAIM-TYPE = 'F'
052614                 ADD SWR-BA-PAYMENT-AMOUNT
052614                                       TO WS-ACCT-FL-TOTAL
052614                 ADD +1                TO WS-ACCT-FL-COUNT
100518
022122             WHEN SWR-BA-CLAIM-TYPE = 'B'
022122                 ADD SWR-BA-PAYMENT-AMOUNT
022122                                       TO WS-ACCT-BR-TOTAL
022122                 ADD +1                TO WS-ACCT-BR-COUNT
022122
022122             WHEN SWR-BA-CLAIM-TYPE = 'H'
022122                 ADD SWR-BA-PAYMENT-AMOUNT
022122                                       TO WS-ACCT-HS-TOTAL
022122                 ADD +1                TO WS-ACCT-HS-COUNT
100518
100518             WHEN SWR-BA-CLAIM-TYPE = 'O'
100518                 ADD SWR-BA-PAYMENT-AMOUNT
100518                                       TO WS-ACCT-OT-TOTAL
100518                 ADD +1                TO WS-ACCT-OT-COUNT
122702             END-EVALUATE 
00920          ELSE                                                     EL325
122702             EVALUATE TRUE
122702             WHEN SWR-BA-CLAIM-TYPE = AH-OVERRIDE-L1   
00922                  ADD SWR-BA-PAYMENT-AMOUNT                        EL325
00923                                        TO WS-ACCT-AH-TOTAL-VOID 
00924                  ADD +1                TO WS-ACCT-AH-COUNT-VOID

122702             WHEN SWR-BA-CLAIM-TYPE = LIFE-OVERRIDE-L1
00926                  ADD SWR-BA-PAYMENT-AMOUNT                        EL325
00927                                        TO WS-ACCT-LF-TOTAL-VOID
00928                  ADD +1                TO WS-ACCT-LF-COUNT-VOID

122702             WHEN SWR-BA-CLAIM-TYPE = 'I'
122702                 ADD SWR-BA-PAYMENT-AMOUNT
122702                                       TO WS-ACCT-IU-TOTAL-VOID
122702                 ADD +1                TO WS-ACCT-IU-COUNT-VOID
121203
121203             WHEN SWR-BA-CLAIM-TYPE = 'G'
121203                 ADD SWR-BA-PAYMENT-AMOUNT
121203                                       TO WS-ACCT-GP-TOTAL-VOID
121203                 ADD +1                TO WS-ACCT-GP-COUNT-VOID
052614
052614             WHEN SWR-BA-CLAIM-TYPE = 'F'
052614                 ADD SWR-BA-PAYMENT-AMOUNT
052614                                       TO WS-ACCT-FL-TOTAL-VOID
052614                 ADD +1                TO WS-ACCT-FL-COUNT-VOID
100518
022122             WHEN SWR-BA-CLAIM-TYPE = 'B'
022122                 ADD SWR-BA-PAYMENT-AMOUNT
022122                                       TO WS-ACCT-BR-TOTAL-VOID
022122                 ADD +1                TO WS-ACCT-BR-COUNT-VOID
022122
022122             WHEN SWR-BA-CLAIM-TYPE = 'H'
022122                 ADD SWR-BA-PAYMENT-AMOUNT
022122                                       TO WS-ACCT-HS-TOTAL-VOID
022122                 ADD +1                TO WS-ACCT-HS-COUNT-VOID
100518
100518             WHEN SWR-BA-CLAIM-TYPE = 'O'
100518                 ADD SWR-BA-PAYMENT-AMOUNT
100518                                       TO WS-ACCT-OT-TOTAL-VOID
100518                 ADD +1                TO WS-ACCT-OT-COUNT-VOID
122702             END-EVALUATE.
00929                                                                   EL325
00930  3250-BYPASS-REINS-ONLY.                                          EL325
00931                                                                   EL325
00932      IF  SWR-BA-CHECK-WRITTEN-DT NOT = LOW-VALUES                 EL325
00933          MOVE    SWR-BA-CHECK-WRITTEN-DT TO  DC-BIN-DATE-1        EL325
00934          MOVE    SPACES                  TO  DC-OPTION-CODE       EL325
00935          PERFORM 8500-DATE-CONVERSION                             EL325
00936          MOVE    DC-GREG-DATE-1-EDIT     TO  WS-D1-DATE-PAID.     EL325
00937                                                                   EL325
122702     EVALUATE TRUE
122702     WHEN SWR-BA-PAYMENT-TYPE = '1'    
00939          MOVE 'PARTIAL '  TO WS-D1-PAYMENT-TYPE

122702     WHEN SWR-BA-PAYMENT-TYPE = '2'            
00942          MOVE 'FINAL   '  TO WS-D1-PAYMENT-TYPE  

122702     WHEN SWR-BA-PAYMENT-TYPE = '3'              
00945          MOVE 'LUMP SUM'  TO WS-D1-PAYMENT-TYPE

122702     WHEN SWR-BA-PAYMENT-TYPE = '4'            
00948          MOVE 'ADDL    '  TO WS-D1-PAYMENT-TYPE 

122702     WHEN SWR-BA-PAYMENT-TYPE = '5'             
00951          MOVE 'CHG EXP '  TO WS-D1-PAYMENT-TYPE 

122702     WHEN SWR-BA-PAYMENT-TYPE = '6'          
00954          MOVE 'OTH EXP '  TO WS-D1-PAYMENT-TYPE

122702     WHEN SWR-BA-PAYMENT-TYPE = '7' OR '8'       
00958          MOVE 'REFUND  '  TO WS-D1-PAYMENT-TYPE 

122702     WHEN OTHER
               MOVE SWR-BA-PAYMENT-TYPE TO WS-D1-PAYMENT-TYPE
122702     END-EVALUATE.
00963                                                                   EL325
00964      MOVE SWR-BA-INSURED-LAST-NAME  TO  WS-D1-INSURED.            EL325
00965                                                                   EL325
122702     EVALUATE TRUE
122702     WHEN SWR-BA-CLAIM-TYPE = AH-OVERRIDE-L1   
00967          MOVE AH-OVERRIDE-L6     TO  WS-D1-CLAIM-TYPE             EL325

122702     WHEN SWR-BA-CLAIM-TYPE = LIFE-OVERRIDE-L1
00969          MOVE LIFE-OVERRIDE-L6   TO  WS-D1-CLAIM-TYPE

122702     WHEN SWR-BA-CLAIM-TYPE = 'I'
122702         MOVE 'IU'               TO  WS-D1-CLAIM-TYPE
121203
121203     WHEN SWR-BA-CLAIM-TYPE = 'G'
121203         MOVE 'GP'               TO  WS-D1-CLAIM-TYPE
052614
052614     WHEN SWR-BA-CLAIM-TYPE = 'F'
052614         MOVE 'FL'               TO  WS-D1-CLAIM-TYPE
100518
022122     WHEN SWR-BA-CLAIM-TYPE = 'B'
022122         MOVE 'BR'               TO  WS-D1-CLAIM-TYPE
022122
022122     WHEN SWR-BA-CLAIM-TYPE = 'H'
022122         MOVE 'HS'               TO  WS-D1-CLAIM-TYPE
100518
100518     WHEN SWR-BA-CLAIM-TYPE = 'O'
100518         MOVE 'OT'               TO  WS-D1-CLAIM-TYPE
122702     END-EVALUATE.
00970                                                                   EL325
00971      IF  SWR-BA-INCURRED-DT NOT = LOW-VALUES                      EL325
00972          MOVE SWR-BA-INCURRED-DT     TO  DC-BIN-DATE-1            EL325
00973          MOVE SPACES                 TO  DC-OPTION-CODE           EL325
00974          PERFORM 8500-DATE-CONVERSION                             EL325
00975          MOVE DC-GREG-DATE-1-EDIT    TO  WS-D1-INCURRED-DATE.     EL325
00976                                                                   EL325
00977      MOVE    SWR-BA-CLAIM-NO        TO  WS-D1-CLAIM-NO.           EL325
00978      MOVE    SWR-BA-CLAIM-STATUS    TO  WS-D1-STATUS.             EL325
00979      MOVE    SWR-BA-CERT-NO         TO  WS-D1-CERT-NO.            EL325
00980      MOVE    SWR-BA-CERT-EFF-DT     TO  DC-BIN-DATE-1.            EL325
00981      MOVE    SPACES                 TO  DC-OPTION-CODE.           EL325
00982      PERFORM 8500-DATE-CONVERSION.                                EL325
00983      MOVE    DC-GREG-DATE-1-EDIT    TO  WS-D1-EFFECTIVE-DATE.     EL325
00984      MOVE    SWR-BA-STATE           TO  WS-D1-STATE.              EL325
00985      MOVE    SWR-BA-GROUPING        TO  WS-D1-GROUP.              EL325
00986      MOVE    WS-DETAIL1             TO  PRT.                      EL325
00987      PERFORM WRITE-A-LINE.                                        EL325
00988      MOVE    SPACES                 TO  WS-DETAIL1.               EL325
00989                                                                   EL325
00990      IF SWR-BA-VOID-DT NOT = LOW-VALUES AND                       EL325
00991           SWR-BA-CERT-STATUS = '9'                                EL325
00992          MOVE 'R E I N/V O I D'  TO  WS-D2-MESSAGE                EL325
00993      ELSE                                                         EL325
00994          IF SWR-BA-VOID-DT NOT = LOW-VALUES                       EL325
00995             MOVE '*** V O I D ***'  TO  WS-D2-MESSAGE             EL325
00996          ELSE                                                     EL325
00997             IF SWR-BA-CERT-STATUS = '9'                           EL325
00998                 MOVE '*** R E I N ***'  TO  WS-D2-MESSAGE.        EL325
00999                                                                   EL325
01000      IF SWR-BA-PAID-THRU-DT NOT = LOW-VALUES                      EL325
01001         IF DTE-CLAIM-PAID-THRU-TO EQUAL ' '                       EL325
01002            MOVE SWR-BA-PAID-THRU-DT    TO  DC-BIN-DATE-1          EL325
01003            MOVE SPACES                 TO  DC-OPTION-CODE         EL325
01004            PERFORM 8500-DATE-CONVERSION                           EL325
01005            MOVE DC-GREG-DATE-1-EDIT    TO  WS-D1-PAID-THRU-DATE   EL325
01006         ELSE                                                      EL325
01007            MOVE SWR-BA-PAID-THRU-DT    TO  DC-BIN-DATE-1          EL325
01008            MOVE '6'                    TO  DC-OPTION-CODE         EL325
01009            MOVE +1        TO DC-ELAPSED-DAYS                      EL325
01010            MOVE +0        TO DC-ELAPSED-MONTHS                    EL325
01011            PERFORM 8500-DATE-CONVERSION                           EL325
01012            MOVE DC-GREG-DATE-1-EDIT    TO  WS-D1-PAID-THRU-DATE.  EL325
01013                                                                   EL325
01014      MOVE    SWR-BA-CERT-EFF-DT     TO  DC-BIN-DATE-1.            EL325
01015      MOVE    SWR-BA-ORIG-TERM       TO  DC-ELAPSED-MONTHS.        EL325
01016      MOVE    '6'                    TO  DC-OPTION-CODE.           EL325
01017      PERFORM 8500-DATE-CONVERSION.                                EL325
01018      MOVE    DC-GREG-DATE-1-EDIT    TO  WS-D1-EXPIRE-DATE.        EL325
01019      MOVE    WS-DETAIL1             TO  PRT.                      EL325
01020      MOVE '99'                      TO  WS-LINE-COUNT-MAX.        EL325
01021      PERFORM WRITE-A-LINE.                                        EL325
01022      MOVE '55'                      TO  WS-LINE-COUNT-MAX.        EL325
01023                                                                   EL325
01024      GO TO 3100-SORT-OUTPUT-PROCEDURE.                            EL325
01025                                                                   EL325
01026  3900-EXIT.                                                       EL325
01027      EXIT.                                                        EL325
01028                                                                   EL325
01029      EJECT                                                        EL325
01030  8100-GET-CARRIER-NAME SECTION.                                   EL325
01031      MOVE +1                     TO  WS-INDEX.                    EL325
01032      MOVE SWR-SB-CARRIER         TO  WS-H4-CARRIER.               EL325
01033                                                                   EL325
01034  8110-GET-CARRIER-NAME.                                           EL325
01035      IF  SWR-SB-CARRIER = CARRIER-SUB (WS-INDEX)                  EL325
01036          MOVE CARRIER-PIC (WS-INDEX) TO WS-H5-CARRIER-NAME        EL325
01037      ELSE                                                         EL325
01038          IF  WS-INDEX LESS THAN +25                               EL325
01039              ADD +1  TO  WS-INDEX                                 EL325
01040              GO TO 8110-GET-CARRIER-NAME.                            CL**2
01041                                                                   EL325
01042  8190-EXIT.                                                       EL325
01043      EXIT.                                                        EL325
01044                                                                   EL325
01045      EJECT                                                        EL325
01046  8200-GET-ACCT-ADDR SECTION.                                      EL325
01047      MOVE SPACES TO AM-CONTROL-BY-VAR-GRP.                        EL325
01048      MOVE WS-LAST-COMPANY        TO AM-COMPANY-CD-A1.             EL325
01049      MOVE WS-LAST-ACCOUNT        TO AM-VG-ACCOUNT.                EL325
01050                                                                   EL325
01051      IF DTE-COMP-VG = ' '                                         EL325
01052          MOVE WS-LAST-STATE      TO AM-VG-STATE.                  EL325
01053                                                                   EL325
01054      IF DTE-COMP-VG = '4'                                         EL325
01055          MOVE WS-LAST-CARRIER    TO AM-VG-CARRIER.                EL325
01056                                                                   EL325
01057      IF DTE-COMP-VG = '2'                                         EL325
01058          MOVE WS-LAST-CARRIER    TO AM-VG-CARRIER                 EL325
01059          MOVE WS-LAST-STATE      TO AM-VG-STATE.                  EL325
01060                                                                   EL325
01061      IF DTE-COMP-VG = '1'                                         EL325
01062          MOVE WS-LAST-CARRIER    TO AM-VG-CARRIER                 EL325
01063          MOVE WS-LAST-GROUPING   TO AM-VG-GROUPING                EL325
01064          MOVE WS-LAST-STATE      TO AM-VG-STATE.                  EL325
01065                                                                   EL325
01066      MOVE AM-CONTROL-BY-VAR-GRP  TO WS-LAST-VG-CONTROL.           EL325
01067                                                                   EL325
01068      START ERACCT                                                 EL325
01069          KEY IS GREATER THAN AM-CONTROL-BY-VAR-GRP.               EL325
01070                                                                   EL325
01071      IF AM-FILE-STATUS = '23'                                     EL325
01072          GO TO 8240-BLANK-ADDR.                                   EL325
01073                                                                   EL325
01074      IF AM-FILE-STATUS NOT = ZERO                                 EL325
01075          MOVE AM-FILE-STATUS      TO WS-ABEND-FILE-STATUS         EL325
01076          MOVE ' ERROR OCCURED START - ERACCT'                     EL325
01077                                   TO WS-ABEND-MESSAGE             EL325
01078          GO TO ABEND-PGM.                                         EL325
01079                                                                   EL325
01080  8210-READ-NEXT.                                                  EL325
01081      READ ERACCT NEXT RECORD.                                     EL325
01082      IF AM-FILE-STATUS = '23' OR '10'                             EL325
01083          GO TO 8299-EXIT.                                         EL325
01084                                                                   EL325
01085      IF AM-FILE-STATUS NOT = ZERO                                 EL325
01086          MOVE AM-FILE-STATUS      TO WS-ABEND-FILE-STATUS         EL325
01087          MOVE ' ERROR OCCURED READ - ERACCT'                      EL325
01088                                   TO WS-ABEND-MESSAGE             EL325
01089          GO TO ABEND-PGM.                                         EL325
01090                                                                   EL325
01091      IF DTE-COMP-VG = '3'                                         EL325
01092          IF WS-LAST-ACCOUNT = AM-VG-ACCOUNT                       EL325
01093              GO TO 8220-SAVE-ADDR                                 EL325
01094          ELSE                                                     EL325
01095              GO TO 8299-EXIT.                                     EL325
01096                                                                   EL325
01097      IF DTE-COMP-VG = ' '                                         EL325
01098          IF WS-LAST-ACCOUNT = AM-VG-ACCOUNT AND                   EL325
01099             WS-LAST-STATE   = AM-VG-STATE                         EL325
01100              GO TO 8220-SAVE-ADDR                                 EL325
01101          ELSE                                                     EL325
01102              GO TO 8299-EXIT.                                     EL325
01103                                                                   EL325
01104      IF DTE-COMP-VG = '4'                                         EL325
01105          IF WS-LAST-ACCOUNT = AM-VG-ACCOUNT AND                   EL325
01106             WS-LAST-CARRIER = AM-VG-CARRIER                       EL325
01107              GO TO 8220-SAVE-ADDR                                 EL325
01108          ELSE                                                     EL325
01109              GO TO 8299-EXIT.                                     EL325
01110                                                                   EL325
01111      IF DTE-COMP-VG = '2'                                         EL325
01112          IF WS-LAST-ACCOUNT = AM-VG-ACCOUNT AND                   EL325
01113             WS-LAST-STATE   = AM-VG-STATE   AND                   EL325
01114             WS-LAST-CARRIER = AM-VG-CARRIER                       EL325
01115              GO TO 8220-SAVE-ADDR                                 EL325
01116          ELSE                                                     EL325
01117              GO TO 8299-EXIT.                                     EL325
01118                                                                   EL325
01119      IF DTE-COMP-VG = '1'                                         EL325
01120          IF WS-LAST-ACCOUNT = AM-VG-ACCOUNT AND                   EL325
01121             WS-LAST-STATE   = AM-VG-STATE   AND                   EL325
01122             WS-LAST-CARRIER = AM-VG-CARRIER AND                   EL325
01123             WS-LAST-GROUPING = AM-VG-GROUPING                     EL325
01124              GO TO 8220-SAVE-ADDR                                 EL325
01125          ELSE                                                     EL325
01126              GO TO 8299-EXIT.                                     EL325
01127                                                                   EL325
01128  8220-SAVE-ADDR.                                                  EL325
01129      MOVE AM-NAME                    TO WS-H7-ACCOUNT-NAME.       EL325
01130      MOVE AM-PERSON                  TO WS-H7-ACCOUNT-PERSON.     EL325
01131      MOVE AM-ADDRS                   TO WS-H7-ACCOUNT-ADDRESS.    EL325
051810     MOVE SPACES                     TO WS-H7-ACCOUNT-CITY-ST.    EL325
051810     STRING AM-ADDR-CITY ' ' AM-ADDR-STATE
051810        DELIMITED BY '  ' INTO WS-H7-ACCOUNT-CITY-ST
051810     END-STRING
01133      MOVE AM-ZIP-PRIME               TO WS-H7-ACCOUNT-ZIP-PRIME.  EL325
01134      MOVE AM-ZIP-PLUS4               TO WS-H7-ACCOUNT-ZIP-PLUS4.  EL325
01135      GO TO 8210-READ-NEXT.                                        EL325
01136                                                                   EL325
01137  8240-BLANK-ADDR.                                                 EL325
01138      MOVE SPACES                     TO WS-H7-ACCOUNT-NAME        EL325
01139                                         WS-H7-ACCOUNT-PERSON      EL325
01140                                         WS-H7-ACCOUNT-ADDRESS     EL325
01141                                         WS-H7-ACCOUNT-CITY-ST     EL325
01142                                         WS-H7-ACCOUNT-ZIP-PRIME   EL325
01143                                         WS-H7-ACCOUNT-ZIP-PLUS4.  EL325
01144                                                                   EL325
01145  8299-EXIT.                                                       EL325
01146      EXIT.                                                        EL325
01147                                                                   EL325
01148      EJECT                                                        EL325
01149  8300-GET-PRODUCER-ADDR SECTION.                                  EL325
01150                                                                   EL325
01151      MOVE SPACES                 TO PD-CONTROL-BY-VAR-GRP.        EL325
01152      MOVE WS-LAST-COMPANY        TO PD-COMPANY-CD-A1.             EL325
01153      MOVE WS-LAST-ACCOUNT        TO PD-VG-PRODUCER.               EL325
01154                                                                   EL325
01155      IF DTE-MORTG-ACCESS-CNTL = ' '                               EL325
01156          MOVE WS-LAST-STATE      TO PD-VG-STATE.                  EL325
01157                                                                   EL325
01158      IF DTE-MORTG-ACCESS-CNTL = '4'                               EL325
01159          MOVE WS-LAST-CARRIER    TO PD-VG-CARRIER.                EL325
01160                                                                   EL325
01161      IF DTE-MORTG-ACCESS-CNTL = '2'                               EL325
01162          MOVE WS-LAST-CARRIER    TO PD-VG-CARRIER                 EL325
01163          MOVE WS-LAST-STATE      TO PD-VG-STATE.                  EL325
01164                                                                   EL325
01165      IF DTE-MORTG-ACCESS-CNTL = '1'                               EL325
01166          MOVE WS-LAST-CARRIER    TO PD-VG-CARRIER                 EL325
01167          MOVE WS-LAST-GROUPING   TO PD-VG-GROUPING                EL325
01168          MOVE WS-LAST-STATE      TO PD-VG-STATE.                  EL325
01169                                                                   EL325
01170      MOVE PD-CONTROL-BY-VAR-GRP  TO WS-LAST-VG-CONTROL.           EL325
01171                                                                   EL325
01172      START MPPROD                                                 EL325
01173          KEY IS GREATER THAN PD-CONTROL-BY-VAR-GRP.               EL325
01174                                                                   EL325
01175      IF PD-FILE-STATUS = '23'                                     EL325
01176          GO TO 8340-BLANK-ADDR.                                   EL325
01177                                                                   EL325
01178      IF PD-FILE-STATUS NOT = ZERO                                 EL325
01179          MOVE PD-FILE-STATUS      TO WS-ABEND-FILE-STATUS         EL325
01180          MOVE ' ERROR OCCURED START - MPPROD'                     EL325
01181                                   TO WS-ABEND-MESSAGE             EL325
01182          GO TO ABEND-PGM.                                         EL325
01183                                                                   EL325
01184  8310-READ-NEXT.                                                  EL325
01185      READ MPPROD NEXT RECORD.                                     EL325
01186      IF PD-FILE-STATUS = '23' OR '10'                             EL325
01187          GO TO 8399-EXIT.                                         EL325
01188                                                                   EL325
01189      IF PD-FILE-STATUS NOT = ZERO                                 EL325
01190          MOVE PD-FILE-STATUS      TO WS-ABEND-FILE-STATUS         EL325
01191          MOVE ' ERROR OCCURED READ - MPPROD'                      EL325
01192                                   TO WS-ABEND-MESSAGE             EL325
01193          GO TO ABEND-PGM.                                         EL325
01194                                                                   EL325
01195      IF DTE-COMP-VG = '3'                                         EL325
01196          IF WS-LAST-ACCOUNT = PD-VG-PRODUCER                      EL325
01197              GO TO 8320-SAVE-ADDR                                 EL325
01198          ELSE                                                     EL325
01199              GO TO 8399-EXIT.                                     EL325
01200                                                                   EL325
01201      IF DTE-COMP-VG = ' '                                         EL325
01202          IF WS-LAST-ACCOUNT = PD-VG-PRODUCER AND                  EL325
01203             WS-LAST-STATE   = PD-VG-STATE                         EL325
01204              GO TO 8320-SAVE-ADDR                                 EL325
01205          ELSE                                                     EL325
01206              GO TO 8399-EXIT.                                     EL325
01207                                                                   EL325
01208      IF DTE-COMP-VG = '4'                                         EL325
01209          IF WS-LAST-ACCOUNT = PD-VG-PRODUCER AND                  EL325
01210             WS-LAST-CARRIER = PD-VG-CARRIER                       EL325
01211              GO TO 8320-SAVE-ADDR                                 EL325
01212          ELSE                                                     EL325
01213              GO TO 8399-EXIT.                                     EL325
01214                                                                   EL325
01215      IF DTE-COMP-VG = '2'                                         EL325
01216          IF WS-LAST-ACCOUNT = PD-VG-PRODUCER AND                  EL325
01217             WS-LAST-STATE   = PD-VG-STATE   AND                   EL325
01218             WS-LAST-CARRIER = PD-VG-CARRIER                       EL325
01219              GO TO 8320-SAVE-ADDR                                 EL325
01220          ELSE                                                     EL325
01221              GO TO 8399-EXIT.                                     EL325
01222                                                                   EL325
01223      IF DTE-COMP-VG = '1'                                         EL325
01224          IF WS-LAST-ACCOUNT =  PD-VG-PRODUCER AND                 EL325
01225             WS-LAST-STATE   =  PD-VG-STATE    AND                 EL325
01226             WS-LAST-CARRIER =  PD-VG-CARRIER  AND                 EL325
01227             WS-LAST-GROUPING = PD-VG-GROUPING                     EL325
01228              GO TO 8320-SAVE-ADDR                                 EL325
01229          ELSE                                                     EL325
01230              GO TO 8399-EXIT.                                     EL325
01231                                                                   EL325
01232  8320-SAVE-ADDR.                                                  EL325
01233      MOVE PD-NAME                    TO WS-H7-ACCOUNT-NAME.       EL325
01234      MOVE PD-PERSON                  TO WS-H7-ACCOUNT-PERSON.     EL325
01235      MOVE PD-ADDRS                   TO WS-H7-ACCOUNT-ADDRESS.    EL325
01236      MOVE PD-CITY                    TO WS-H7-ACCOUNT-CITY-ST.    EL325
01237      MOVE PD-ZIP-PRIME               TO WS-H7-ACCOUNT-ZIP-PRIME.  EL325
01238      MOVE PD-ZIP-PLUS4               TO WS-H7-ACCOUNT-ZIP-PLUS4.  EL325
01239      GO TO 8310-READ-NEXT.                                        EL325
01240                                                                   EL325
01241  8340-BLANK-ADDR.                                                 EL325
01242      MOVE SPACES                     TO WS-H7-ACCOUNT-NAME        EL325
01243                                         WS-H7-ACCOUNT-PERSON      EL325
01244                                         WS-H7-ACCOUNT-ADDRESS     EL325
01245                                         WS-H7-ACCOUNT-CITY-ST     EL325
01246                                         WS-H7-ACCOUNT-ZIP-PRIME   EL325
01247                                         WS-H7-ACCOUNT-ZIP-PLUS4.  EL325
01248                                                                   EL325
01249  8399-EXIT.                                                       EL325
01250      EXIT.                                                        EL325
01251                                                                   EL325
01252      EJECT                                                        EL325
01253  8500-DATE-CONVERSION SECTION. COPY ELCDCS.                       EL325
01254                                                                   EL325
01255      EJECT                                                        EL325
01256  WRITE-A-LINE SECTION. COPY ELCWAL.                               EL325
01257                                                                   EL325
01258      EJECT                                                        EL325
01259  WRITE-HEADINGS SECTION.                                          EL325
01260                                                                   EL325
01261  WHS-010.                                                         EL325
01262      IF  WS-H2-DATE EQUAL SPACES                                  EL325
01263          MOVE WS-CURRENT-DATE    TO  WS-H2-DATE                   EL325
01264          MOVE COMPANY-NAME       TO  WS-H2-CLIENT-NAME            EL325
01265          MOVE ALPH-DATE          TO  WS-H3-DATE.                  EL325
01266                                                                   EL325
01267      ADD +1  TO  WS-PAGE.                                         EL325
01268      MOVE WS-PAGE                TO  WS-H3-PAGE.                  EL325
01269      MOVE PRT                    TO  WS-SAVE-PRINT-RECORD.        EL325
01270      MOVE ZERO                   TO  WS-LINE-COUNT.               EL325
01271                                                                   EL325
01272      MOVE WS-HEADING1            TO  PRT.                         EL325
01273      MOVE '1'                    TO  X.                           EL325
01274      PERFORM WRITE-PRINTER.                                       EL325
01275                                                                   EL325
01276      MOVE WS-HEADING2            TO  PRT.                         EL325
01277      MOVE ' '                    TO  X.                           EL325
01278      PERFORM WRITE-PRINTER.                                       EL325
01279                                                                   EL325
01280      MOVE WS-HEADING3            TO  PRT.                         EL325
01281      MOVE ' '                    TO  X.                           EL325
01282      PERFORM WRITE-PRINTER.                                       EL325
01283                                                                   EL325
01284      MOVE WS-HEADING4            TO  PRT.                         EL325
01285      MOVE ' '                    TO  X.                           EL325
01286      PERFORM WRITE-PRINTER.                                       EL325
01287                                                                   EL325
01288                                                                   EL325
01289      MOVE    WS-HEADING5            TO  PRT.                      EL325
01290      PERFORM WRITE-PRINTER.                                       EL325
01291                                                                   EL325
01292      IF WS-HEADING-SW NOT = ZERO                                  EL325
01293          MOVE ZERO               TO  WS-HEADING-SW                EL325
01294          MOVE +5                 TO  WS-LINE-COUNT                EL325
01295          GO TO WHS-020.                                           EL325
01296                                                                   EL325
01297      MOVE WS-HEADING6            TO PRT.                          EL325
01298      PERFORM WRITE-PRINTER.                                       EL325
01299      MOVE WS-HEADING7A           TO PRT.                          EL325
01300      PERFORM WRITE-PRINTER.                                       EL325
01301                                                                   EL325
01302      IF WS-H7-ACCOUNT-PERSON  NOT = SPACES                        EL325
01303          ADD +1 TO WS-LINE-COUNT                                  EL325
01304          MOVE WS-HEADING7B       TO PRT                           EL325
01305          PERFORM WRITE-PRINTER.                                   EL325
01306                                                                   EL325
01307      IF WS-H7-ACCOUNT-ADDRESS NOT = SPACES                        EL325
01308          ADD +1 TO WS-LINE-COUNT                                  EL325
01309          MOVE WS-HEADING7C       TO PRT                           EL325
01310          PERFORM WRITE-PRINTER.                                   EL325
01311                                                                   EL325
01312      IF WS-H7-ACCOUNT-CITY-ST = SPACES AND                        EL325
01313         WS-H7-ACCOUNT-ZIP-PRIME = SPACES                          EL325
01314             NEXT SENTENCE                                         EL325
01315      ELSE                                                         EL325
01316          ADD +1 TO WS-LINE-COUNT                                  EL325
01317          MOVE WS-HEADING7D       TO PRT                           EL325
01318          PERFORM WRITE-PRINTER.                                   EL325
01319                                                                   EL325
01320          MOVE WS-HEADING8        TO  PRT                          EL325
01321          PERFORM WRITE-PRINTER.                                   EL325
01322          MOVE WS-HEADING9        TO  PRT                          EL325
01323          PERFORM WRITE-PRINTER.                                   EL325
01324          ADD +12                 TO  WS-LINE-COUNT.               EL325
01325                                                                   EL325
01326  WHS-020. COPY ELCWHS2.                                           EL325
01327                                                                   EL325
01328  WRITE-PRINTER SECTION. COPY ELCWPS.                              EL325
01329                                                                   EL325
01330  WPS-020.                                                         EL325
01331                                                                   EL325
01332      IF DTE-FICH NOT = SPACE AND                                  EL325
01333          FICH-OPEN   = SPACE                                      EL325
01334          MOVE 'X' TO FICH-OPEN                                    EL325
01335          OPEN OUTPUT FICH.                                        EL325
01336                                                                   EL325
01337      IF DTE-PRT-OPT = 'S' OR 'T'                                  EL325
01338          IF (REPT-OPEN = SPACE) AND (DTE-ABEND-CD-1 = SPACE)      EL325
01339              OPEN I-O ELREPT                                      EL325
01340              IF DTE-F-1 NOT = ZERO AND                            EL325
01341                 DTE-VSAM-FLAGS NOT = '97'                         EL325
01342                  MOVE DTE-VSAM-FLAGS  TO  WS-ABEND-FILE-STATUS    EL325
01343                  MOVE 'ERROR OCCURED OPEN - ELREPT'               EL325
01344                                  TO  WS-ABEND-MESSAGE             EL325
01345                  GO TO ABEND-PGM                                  EL325
01346              ELSE                                                 EL325
01347                  MOVE '1'                   TO REPT-OPEN          EL325
01348                  MOVE DTE-CLASIC-COMPANY-CD TO RF-COMPANY-CD      EL325
01349                  MOVE '1'                   TO RF-RECORD-TYPE     EL325
01350                  MOVE OLC-REPORT-NAME       TO RF-REPORT-ID       EL325
01351                  MOVE ZERO                  TO RF-LINE-NUMBER     EL325
01352                  START ELREPT  KEY NOT LESS RF-CONTROL-PRIMARY    EL325
01353                  PERFORM DTE-REPORT-DELETE THRU DTE-DELETE-EXIT   EL325
01354                  MOVE DTE-CLASIC-COMPANY-CD TO RF-COMPANY-CD      EL325
01355                  MOVE '2'                   TO RF-RECORD-TYPE     EL325
01356                  MOVE OLC-REPORT-NAME       TO RF-REPORT-ID       EL325
01357                  MOVE ZERO                  TO RF-LINE-NUMBER     EL325
01358                  START ELREPT  KEY NOT LESS RF-CONTROL-PRIMARY    EL325
01359                  PERFORM DTE-REPORT-DELETE THRU DTE-DELETE-EXIT   EL325
01360                  MOVE DTE-CLASIC-COMPANY-CD TO RF-COMPANY-CD      EL325
01361                  MOVE '1'                   TO RF-RECORD-TYPE     EL325
01362                  MOVE OLC-REPORT-NAME       TO RF-REPORT-ID       EL325
01363                  MOVE SPACES                TO RF-REPORT-LINE-133.EL325
01364                                                                   EL325
01365      IF DTE-ABEND-CD-1 = '81' AND                                 EL325
01366         DTE-PRT-OPT    = 'S'                                      EL325
01367          MOVE +0302  TO WS-RETURN-CODE                            EL325
01368          GO TO ABEND-PGM.                                         EL325
01369                                                                   EL325
01370      IF DTE-PRT-OPT = 'S' OR 'T'                                  EL325
01371          MOVE X      TO RF-CTL-CHAR-133                           EL325
01372          MOVE P-DATA TO RF-DATA-133                               EL325
01373              IF DTE-ABEND-CD-1 = SPACES                           EL325
01374                  ADD +1 TO DTE-TOT-LINES                          EL325
01375                  MOVE DTE-TOT-LINES TO RF-LINE-NUMBER             EL325
01376                  WRITE REPORT-SAVE-FILE                           EL325
01377                      INVALID KEY                                  EL325
01378                          MOVE '88' TO DTE-ABEND-CD-1              EL325
01379                          CLOSE ELREPT                             EL325
01380                          MOVE SPACE TO REPT-OPEN.                 EL325
01381                                                                   EL325
01382      IF DTE-FICH NOT = SPACE                                      EL325
01383          MOVE X TO P-CTL                                          EL325
01384          WRITE FICH-REC FROM PRT.                                 EL325
01385                                                                   EL325
01386      IF DTE-PRT-OPT = 'P' OR 'B' OR 'T'                           EL325
01387          MOVE X TO P-CTL                                          EL325
01388          WRITE PRT.                                               EL325
01389                                                                   EL325
01390      GO TO DTE-PRINT-EXIT.                                        EL325
01391                                                                   EL325
01392  DTE-REPORT-DELETE.                                               EL325
01393      IF DTE-F-1 NOT = ZERO                                        EL325
01394          MOVE ZERO TO DTE-VSAM-FLAGS                              EL325
01395          GO TO DTE-DELETE-EXIT.                                   EL325
01396                                                                   EL325
01397      READ ELREPT   NEXT RECORD                                    EL325
01398            AT END   GO TO DTE-DELETE-EXIT.                        EL325
01399                                                                   EL325
01400      IF DTE-CLASIC-COMPANY-CD = RF-COMPANY-CD  AND                EL325
01401         OLC-REPORT-NAME       = RF-REPORT-ID                      EL325
01402          DELETE ELREPT RECORD                                     EL325
01403          GO TO DTE-REPORT-DELETE.                                 EL325
01404                                                                   EL325
01405  DTE-DELETE-EXIT.                                                 EL325
01406      EXIT.                                                        EL325
01407                                                                   EL325
01408  DTE-PRINT-EXIT.                                                  EL325
01409      EXIT.                                                        EL325
01410 ******************************************************************EL325
01411                                                                   EL325
01412      EJECT                                                        EL325
01413  OPEN-FILES SECTION.                                              EL325
01414                                                                   EL325
01415  OFS-010.                                                         EL325
01416      OPEN INPUT REPORTS-EXTRACT-FILE                              EL325
01417           ERACCT                                                  EL325
01418           MPPROD                                                  EL325
01419           OUTPUT PRNTR.                                           EL325
01420                                                                   EL325
01421      IF AM-FILE-STATUS  = '00' OR '97'                            EL325
01422          NEXT SENTENCE                                            EL325
01423        ELSE                                                       EL325
01424          MOVE AM-FILE-STATUS     TO WS-ABEND-FILE-STATUS          EL325
01425          MOVE 'ERROR ON OPEN ERACCT'                              EL325
01426                                  TO WS-ABEND-MESSAGE              EL325
01427          GO TO ABEND-PGM.                                         EL325
01428                                                                   EL325
01429      IF (PD-FILE-STATUS = '00' OR '97' OR '9%' OR '9+')
01430         CONTINUE
01431      ELSE
01432         MOVE PD-FILE-STATUS      TO WS-ABEND-FILE-STATUS
01433         MOVE 'ERROR ON OPEN - MPPROD'
01434                                  TO WS-ABEND-MESSAGE
01435         PERFORM ABEND-PGM
           END-IF
          .
01437  OFS-EXIT.                                                        EL325
01438      EXIT.                                                        EL325
01439                                                                   EL325
01440      EJECT                                                        EL325
01441  CLOSE-FILES SECTION.                                             EL325
01442                                                                   EL325
01443  CFS-010. COPY ELCPRTCX.                                          EL325
01444      CLOSE REPORTS-EXTRACT-FILE                                   EL325
01445            ERACCT                                                 EL325
01446            MPPROD                                                 EL325
01447            PRNTR.                                                 EL325
01448                                                                   EL325
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
070714           MOVE 'N'              TO ME-UPDATE-FLAG    
070714           CLOSE ERMEBL
070714        end-if
070714     end-if
070714     IF ME-DO-UPDATE
070714        move hld-325-clms-tot    to me-325-clms-tot
070714        ACCEPT WS-TIME-OF-DAY FROM TIME
070714        REWRITE MONTH-END-BALANCES
070714        CLOSE ERMEBL
070714     end-if

          .
01449  CFS-EXIT.                                                        EL325
01450      EXIT.                                                        EL325
01451                                                                   EL325
01452  ABEND-PGM SECTION. COPY ELCABEND.                                EL325
