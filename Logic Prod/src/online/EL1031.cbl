00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL1031.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 09/21/94 08:23:00.
00007 *                            VMOD=2.022.
00008 *
00008 *
00009 *AUTHOR.        LOGIC,INC.
00010 *               DALLAS, TEXAS.
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
00024 *
00025 *REMARKS.
00026 *        TRANSACTION - EXH8 - USER RECORD APPLICATION DESCRIPT
092908******************************************************************
092908*                   C H A N G E   L O G
092908*
092908* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
092908*-----------------------------------------------------------------
092908*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
092908* EFFECTIVE    NUMBER
092908*-----------------------------------------------------------------
092908* 092908    2008091100003  PEMA ADD NEW APPS 29 AND 30
012009* 012009    2008100900001  PEMA ADD NEW CLAIM APP
092908******************************************************************
00027
00028      EJECT
00029  ENVIRONMENT DIVISION.
00030  DATA DIVISION.
00031  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00032  77  FILLER  PIC  X(32) VALUE '********************************'.
00033  77  FILLER  PIC  X(32) VALUE '*    EL1031 WORKING STORAGE    *'.
00034  77  FILLER  PIC  X(32) VALUE '********* VMOD=2.022 ***********'.
00035
00036  01  ERROR-MESSAGES.
00037      12  ER-0000             PIC  X(4)       VALUE '0000'.
00038      12  ER-0004             PIC  X(4)       VALUE '0004'.
00039      12  ER-0007             PIC  X(4)       VALUE '0007'.
00040      12  ER-0029             PIC  X(4)       VALUE '0029'.
00041      12  ER-0042             PIC  X(4)       VALUE '0042'.
00042      12  ER-0068             PIC  X(4)       VALUE '0068'.
00043      12  ER-0073             PIC  X(4)       VALUE '0073'.
00044      12  ER-0086             PIC  X(4)       VALUE '0086'.
00045      12  ER-7008             PIC  X(4)       VALUE '7008'.
00046      12  ER-7299             PIC  X(4)       VALUE '7299'.
00047      12  ER-7320             PIC  X(4)       VALUE '7320'.
00048      12  ER-7322             PIC  X(4)       VALUE '7322'.
00049
00050  01  WS-DATE-AREA.
00051      12  SAVE-DATE           PIC  X(8)       VALUE SPACES.
00052      12  SAVE-BIN-DATE       PIC  XX         VALUE SPACES.
00053
00054      EJECT
00055  01  STANDARD-AREAS.
00056      12  MAP-NAME            PIC  X(8)       VALUE 'EL1031A'.
00057      12  MAPSET-NAME         PIC  X(8)       VALUE 'EL1031S'.
00058      12  TRANS-ID            PIC  X(4)       VALUE 'EXH8'.
00059      12  THIS-PGM            PIC  X(8)       VALUE 'EL1031'.
00060      12  PGM-NAME            PIC  X(8).
00061      12  TIME-IN             PIC S9(7).
00062      12  TIME-OUT-R  REDEFINES  TIME-IN.
00063          16  FILLER          PIC  X.
00064          16  TIME-OUT        PIC  99V99.
00065          16  FILLER          PIC  XX.
00066      12  XCTL-005            PIC  X(8)       VALUE 'EL005'.
00067      12  XCTL-010            PIC  X(8)       VALUE 'EL010'.
00068      12  XCTL-126            PIC  X(8)       VALUE 'EL126'.
00069      12  XCTL-626            PIC  X(8)       VALUE 'EL626'.
00070      12  XCTL-400            PIC  X(8)       VALUE 'LF400'.
00071      12  XCTL-800            PIC  X(8)       VALUE 'GL800'.
00072      12  LINK-001            PIC  X(8)       VALUE 'EL001'.
00073      12  LINK-004            PIC  X(8)       VALUE 'EL004'.
00074      12  LINK-ELDATCV        PIC  X(8)       VALUE 'ELDATCV'.
00075      12  FILE-ID-ELCNTL      PIC  X(8)       VALUE 'ELCNTL'.
00076
00077      EJECT
00078  01  SYSTEM-APP-TABLES.
00079
00080 **************************************
00081 *  CREDIT APPLICATIONS DESCRIPTIONS  *
00082 **************************************
00083
00084      12  CREDIT-DESCRIPT-TABLE.
00085          16  CR-001 PIC X(25) VALUE 'END USER REPORTING       '.
00086          16  CR-002 PIC X(25) VALUE 'PROGRAM OPTIONS          '.
00087          16  CR-003 PIC X(25) VALUE 'TEXT FILE (LETTERS FORMS)'.
00088          16  CR-004 PIC X(25) VALUE 'ACCOUNT MASTERS          '.
00089          16  CR-005 PIC X(25) VALUE 'COMPENSATION MASTERS     '.
00090          16  CR-006 PIC X(25) VALUE 'RATE MASTERS             '.
00091          16  CR-007 PIC X(25) VALUE 'REINSURANCE MASTERS      '.
00092          16  CR-008 PIC X(25) VALUE 'COMMISSION TABLES        '.
00093          16  CR-009 PIC X(25) VALUE 'MORTALITY TABLE CONTROLS '.
00094          16  CR-010 PIC X(25) VALUE 'LOAN OFFICERS            '.
00095          16  CR-011 PIC X(25) VALUE 'DATA ENTRY               '.
00096          16  CR-012 PIC X(25) VALUE 'REVIEW AND CORRECTION    '.
00097          16  CR-013 PIC X(25) VALUE 'FULL FILE EDIT           '.
00098          16  CR-014 PIC X(25) VALUE 'CLAIMS AND RESERVES      '.
00099          16  CR-015 PIC X(25) VALUE 'COMPENSATIONS (PYMT/ADJ) '.
00100          16  CR-016 PIC X(25) VALUE 'RETRO/REINS   (PYMT/ADJ) '.
00101          16  CR-017 PIC X(25) VALUE 'CHECK MAINTENANCE(CREDIT)'.
00102          16  CR-018 PIC X(25) VALUE 'ACCOUNT STATEMENTS       '.
00103          16  CR-019 PIC X(25) VALUE 'GENERAL AGENT STATEMENTS '.
00104          16  CR-020 PIC X(25) VALUE 'LOSS RATIO SELECTION     '.
00105          16  CR-021 PIC X(25) VALUE 'ONLINE STATEMENT PRINTING'.
00106          16  CR-022 PIC X(25) VALUE 'CHECKS TO PRINT  (CREDIT)'.
00107          16  CR-023 PIC X(25) VALUE 'CHECK RELEASE    (CREDIT)'.
00108          16  CR-024 PIC X(25) VALUE 'PRINT RELEASED CHECKS    '.
00109          16  CR-025 PIC X(25) VALUE 'ACCOUNT NOTEPAD          '.
00110          16  CR-026 PIC X(25) VALUE 'RETRO MASTER      (EL606)'.
00111          16  CR-027 PIC X(25) VALUE 'RETRO HISTORY     (EL607)'.
00112          16  CR-028 PIC X(25) VALUE 'BANK MASTER MAINTENANCE  '.
092908         16  CR-029 PIC X(25) VALUE 'STATE CONTROLS (EL106A)  '.
092908         16  CR-030 PIC X(25) VALUE 'REPORT CUSTOMIZATION 604A'.
00115          16  CR-031 PIC X(25) VALUE 'CERTIFICATE LOOK-UP      '.
00116          16  CR-032 PIC X(25) VALUE 'CERTIFICATE NOTES        '.
00117          16  CR-033 PIC X(25) VALUE 'CERTIFICATE CHANGES      '.
00118          16  CR-034 PIC X(25) VALUE '****** FUTURE USE  ******'.
00119          16  CR-035 PIC X(25) VALUE '****** FUTURE USE  ******'.
00120          16  CR-036 PIC X(25) VALUE '****** FUTURE USE  ******'.
00121          16  CR-037 PIC X(25) VALUE '****** FUTURE USE  ******'.
00122          16  CR-038 PIC X(25) VALUE '****** FUTURE USE  ******'.
00123          16  CR-039 PIC X(25) VALUE '****** FUTURE USE  ******'.
00124          16  CR-040 PIC X(25) VALUE '****** FUTURE USE  ******'.
00125          16  CR-041 PIC X(25) VALUE '****** FUTURE USE  ******'.
00126          16  CR-042 PIC X(25) VALUE '****** FUTURE USE  ******'.
00127          16  CR-043 PIC X(25) VALUE '****** FUTURE USE  ******'.
00128          16  CR-044 PIC X(25) VALUE '****** FUTURE USE  ******'.
00129
00130      EJECT
00131 **************************************
00132 *   CLAIM APPLICATIONS DESCRIPTIONS  *
00133 **************************************
00134
00135      12  CLAIM-DESCRIPT-TABLE.
00136          16  CL-001 PIC X(25) VALUE 'NEW CLAIM SETUP          '.
00137          16  CL-002 PIC X(25) VALUE 'RECORD MAIL RECEIVED     '.
00138          16  CL-003 PIC X(25) VALUE 'CLAIM AUDIT              '.
00139          16  CL-004 PIC X(25) VALUE 'BENEFICIARY MASTER       '.
00140          16  CL-005 PIC X(25) VALUE 'CLAIM MAINTENANCE        '.
00141          16  CL-006 PIC X(25) VALUE 'DENIAL PROCESSING        '.
00142          16  CL-007 PIC X(25) VALUE 'CLAIMS LETTER WRITER     '.
00143          16  CL-008 PIC X(25) VALUE 'NOTE / REMINDER RECORDING'.
00144          16  CL-009 PIC X(25) VALUE 'SETUP AUTOMATIC PAYMENT  '.
00145          16  CL-010 PIC X(25) VALUE 'PAYMENT PROCESSING       '.
00146          16  CL-011 PIC X(25) VALUE 'CHECKS TO PRINT  (CLAIMS)'.
00147          16  CL-012 PIC X(25) VALUE 'CHECK RELEASE    (CLAIMS)'.
00148          16  CL-013 PIC X(25) VALUE 'PRINT RELEASED CHECKS    '.
00149          16  CL-014 PIC X(25) VALUE 'CLAIM ADDRESS MAINTENANCE'.
00150          16  CL-015 PIC X(25) VALUE 'CLAIM TRAILER MAINTENANCE'.
00151          16  CL-016 PIC X(25) VALUE 'CLAIM STATUS/DISPOSITION '.
00152          16  CL-017 PIC X(25) VALUE 'SUPERVISOR REQUEST REPORT'.
00153          16  CL-018 PIC X(25) VALUE 'FILE FOLDER LABEL PRINT  '.
00154          16  CL-019 PIC X(25) VALUE 'CLAIMS STATUS PRINT      '.
00155          16  CL-020 PIC X(25) VALUE 'LETTER/ADDR LABEL PRINT  '.
00156          16  CL-021 PIC X(25) VALUE 'CLAIM LOOK-UP            '.
00157          16  CL-022 PIC X(25) VALUE 'REVIEW PENDING ACTIVITY  '.
00158          16  CL-023 PIC X(25) VALUE 'CHECK RECON      (EL146) '.
00159          16  CL-024 PIC X(25) VALUE 'POLICY FORM MSTR (EL1582)'.
00160          16  CL-025 PIC X(25) VALUE 'AUTO ACT MAINT   (EL145) '.
00161          16  CL-026 PIC X(25) VALUE 'INITIAL/PROG FORMS PRINT '.
00162          16  CL-027 PIC X(25) VALUE 'ONLINE REPORTS           '.
00163          16  CL-028 PIC X(25) VALUE 'PAYMENT APPROVAL         '.
012009         16  CL-029 PIC X(25) VALUE 'DENIAL MAINTENANCE       '.
00165          16  CL-030 PIC X(25) VALUE 'REJECT MAINTENANCE       '.
00166          16  CL-031 PIC X(25) VALUE '****** FUTURE USE  ******'.
00167          16  CL-032 PIC X(25) VALUE '****** FUTURE USE  ******'.
00168          16  CL-033 PIC X(25) VALUE '****** FUTURE USE  ******'.
00169          16  CL-034 PIC X(25) VALUE '****** FUTURE USE  ******'.
00170          16  CL-035 PIC X(25) VALUE '****** FUTURE USE  ******'.
00171          16  CL-036 PIC X(25) VALUE '****** FUTURE USE  ******'.
00172          16  CL-037 PIC X(25) VALUE '****** FUTURE USE  ******'.
00173          16  CL-038 PIC X(25) VALUE '****** FUTURE USE  ******'.
00174          16  CL-039 PIC X(25) VALUE '****** FUTURE USE  ******'.
00175          16  CL-040 PIC X(25) VALUE '****** FUTURE USE  ******'.
00176          16  CL-041 PIC X(25) VALUE '****** FUTURE USE  ******'.
00177          16  CL-042 PIC X(25) VALUE '****** FUTURE USE  ******'.
00178          16  CL-043 PIC X(25) VALUE '****** FUTURE USE  ******'.
00179          16  CL-044 PIC X(25) VALUE '****** FUTURE USE  ******'.
00180
00181      EJECT
00182 *******************************************
00183 *  CREDIT CARD APPLICATIONS DESCRIPTIONS  *
00184 *******************************************
00185
00186      12  CRDT-CRD-DESCRIPT-TABLE.
00187          16  CC-001 PIC X(25) VALUE '****** FUTURE USE  ******'.
00188          16  CC-002 PIC X(25) VALUE '****** FUTURE USE  ******'.
00189          16  CC-003 PIC X(25) VALUE '****** FUTURE USE  ******'.
00190          16  CC-004 PIC X(25) VALUE '****** FUTURE USE  ******'.
00191          16  CC-005 PIC X(25) VALUE '****** FUTURE USE  ******'.
00192          16  CC-006 PIC X(25) VALUE '****** FUTURE USE  ******'.
00193          16  CC-007 PIC X(25) VALUE '****** FUTURE USE  ******'.
00194          16  CC-008 PIC X(25) VALUE '****** FUTURE USE  ******'.
00195          16  CC-009 PIC X(25) VALUE '****** FUTURE USE  ******'.
00196          16  CC-010 PIC X(25) VALUE '****** FUTURE USE  ******'.
00197          16  CC-011 PIC X(25) VALUE '****** FUTURE USE  ******'.
00198          16  CC-012 PIC X(25) VALUE '****** FUTURE USE  ******'.
00199          16  CC-013 PIC X(25) VALUE '****** FUTURE USE  ******'.
00200          16  CC-014 PIC X(25) VALUE '****** FUTURE USE  ******'.
00201          16  CC-015 PIC X(25) VALUE '****** FUTURE USE  ******'.
00202          16  CC-016 PIC X(25) VALUE '****** FUTURE USE  ******'.
00203          16  CC-017 PIC X(25) VALUE '****** FUTURE USE  ******'.
00204          16  CC-018 PIC X(25) VALUE '****** FUTURE USE  ******'.
00205          16  CC-019 PIC X(25) VALUE '****** FUTURE USE  ******'.
00206          16  CC-020 PIC X(25) VALUE '****** FUTURE USE  ******'.
00207          16  CC-021 PIC X(25) VALUE '****** FUTURE USE  ******'.
00208          16  CC-022 PIC X(25) VALUE '****** FUTURE USE  ******'.
00209          16  CC-023 PIC X(25) VALUE '****** FUTURE USE  ******'.
00210          16  CC-024 PIC X(25) VALUE '****** FUTURE USE  ******'.
00211          16  CC-025 PIC X(25) VALUE '****** FUTURE USE  ******'.
00212          16  CC-026 PIC X(25) VALUE '****** FUTURE USE  ******'.
00213          16  CC-027 PIC X(25) VALUE '****** FUTURE USE  ******'.
00214          16  CC-028 PIC X(25) VALUE '****** FUTURE USE  ******'.
00215          16  CC-029 PIC X(25) VALUE '****** FUTURE USE  ******'.
00216          16  CC-030 PIC X(25) VALUE '****** FUTURE USE  ******'.
00217          16  CC-031 PIC X(25) VALUE '****** FUTURE USE  ******'.
00218          16  CC-032 PIC X(25) VALUE '****** FUTURE USE  ******'.
00219          16  CC-033 PIC X(25) VALUE '****** FURURE USE  ******'.
00220          16  CC-034 PIC X(25) VALUE '****** FUTURE USE  ******'.
00221          16  CC-035 PIC X(25) VALUE '****** FUTURE USE  ******'.
00222          16  CC-036 PIC X(25) VALUE '****** FUTURE USE  ******'.
00223          16  CC-037 PIC X(25) VALUE '****** FUTURE USE  ******'.
00224          16  CC-038 PIC X(25) VALUE '****** FUTURE USE  ******'.
00225          16  CC-039 PIC X(25) VALUE '****** FUTURE USE  ******'.
00226          16  CC-040 PIC X(25) VALUE '****** FUTURE USE  ******'.
00227          16  CC-041 PIC X(25) VALUE '****** FUTURE USE  ******'.
00228          16  CC-042 PIC X(25) VALUE '****** FUTURE USE  ******'.
00229          16  CC-043 PIC X(25) VALUE '****** FUTURE USE  ******'.
00230          16  CC-044 PIC X(25) VALUE '****** FUTURE USE  ******'.
00231
00232      EJECT
00233 ************************************
00234 *  A/R  APPLICATIONS DESCRIPTIONS  *
00235 ************************************
00236
00237      12  ACCT-RCV-DESCRIPT-TABLE.
00238          16  AR-001 PIC X(25) VALUE ' ACCOUNTS RECEIVABLE MENU'.
00239          16  AR-002 PIC X(25) VALUE ' REQUEST FILE PROCESSING '.
00240          16  AR-003 PIC X(25) VALUE ' REQUEST FILE DISPLAY    '.
00241          16  AR-004 PIC X(25) VALUE ' SUMMARY CROSS REFERENCE '.
00242          16  AR-005 PIC X(25) VALUE ' PAYMENT AND ADJUSTMENTS '.
00243          16  AR-006 PIC X(25) VALUE ' CHECK FILE MAINTENANCE  '.
00244          16  AR-007 PIC X(25) VALUE ' CHECK RELEASE           '.
00245          16  AR-008 PIC X(25) VALUE '****** FUTURE USE  ******'.
00246          16  AR-009 PIC X(25) VALUE '****** FUTURE USE  ******'.
00247          16  AR-010 PIC X(25) VALUE '****** FUTURE USE  ******'.
00248          16  AR-011 PIC X(25) VALUE '****** FUTURE USE  ******'.
00249          16  AR-012 PIC X(25) VALUE '****** FUTURE USE  ******'.
00250          16  AR-013 PIC X(25) VALUE '****** FUTURE USE  ******'.
00251          16  AR-014 PIC X(25) VALUE '****** FUTURE USE  ******'.
00252          16  AR-015 PIC X(25) VALUE '****** FUTURE USE  ******'.
00253          16  AR-016 PIC X(25) VALUE '****** FUTURE USE  ******'.
00254          16  AR-017 PIC X(25) VALUE '****** FUTURE USE  ******'.
00255          16  AR-018 PIC X(25) VALUE '****** FUTURE USE  ******'.
00256          16  AR-019 PIC X(25) VALUE '****** FUTURE USE  ******'.
00257          16  AR-020 PIC X(25) VALUE '****** FUTURE USE  ******'.
00258          16  AR-021 PIC X(25) VALUE '****** FUTURE USE  ******'.
00259          16  AR-022 PIC X(25) VALUE '****** FUTURE USE  ******'.
00260          16  AR-023 PIC X(25) VALUE '****** FUTURE USE  ******'.
00261          16  AR-024 PIC X(25) VALUE '****** FUTURE USE  ******'.
00262          16  AR-025 PIC X(25) VALUE '****** FUTURE USE  ******'.
00263          16  AR-026 PIC X(25) VALUE '****** FUTURE USE  ******'.
00264          16  AR-027 PIC X(25) VALUE '****** FUTURE USE  ******'.
00265          16  AR-028 PIC X(25) VALUE '****** FUTURE USE  ******'.
00266          16  AR-029 PIC X(25) VALUE '****** FUTURE USE  ******'.
00267          16  AR-030 PIC X(25) VALUE '****** FUTURE USE  ******'.
00268          16  AR-031 PIC X(25) VALUE '****** FUTURE USE  ******'.
00269          16  AR-032 PIC X(25) VALUE '****** FUTURE USE  ******'.
00270          16  AR-033 PIC X(25) VALUE '****** FURURE USE  ******'.
00271          16  AR-034 PIC X(25) VALUE '****** FUTURE USE  ******'.
00272          16  AR-035 PIC X(25) VALUE '****** FUTURE USE  ******'.
00273          16  AR-036 PIC X(25) VALUE '****** FUTURE USE  ******'.
00274          16  AR-037 PIC X(25) VALUE '****** FUTURE USE  ******'.
00275          16  AR-038 PIC X(25) VALUE '****** FUTURE USE  ******'.
00276          16  AR-039 PIC X(25) VALUE '****** FUTURE USE  ******'.
00277          16  AR-040 PIC X(25) VALUE '****** FUTURE USE  ******'.
00278          16  AR-041 PIC X(25) VALUE '****** FUTURE USE  ******'.
00279          16  AR-042 PIC X(25) VALUE '****** FUTURE USE  ******'.
00280          16  AR-043 PIC X(25) VALUE '****** FUTURE USE  ******'.
00281          16  AR-044 PIC X(25) VALUE '****** FUTURE USE  ******'.
00282
00283      EJECT
00284 ****************************************
00285 *  MORTGAGE APPLICATIONS DESCRIPTIONS  *
00286 ****************************************
00287
00288      12  MORTGAGE-DESCRIPT-TABLE.
00289          16  MP-001 PIC X(25) VALUE 'CONVENIENCE CODES (EM114)'.
00290          16  MP-002 PIC X(25) VALUE 'PRODUCER MASTER   (EM650)'.
00291          16  MP-003 PIC X(25) VALUE 'LETTER ARCHIVE(EM152/153)'.
00292          16  MP-004 PIC X(25) VALUE 'MORTALITY CODES   (EL602)'.
00293          16  MP-005 PIC X(25) VALUE 'POLICY MSTR LOOKUP(EM127)'.
00294          16  MP-006 PIC X(25) VALUE 'LETTER PRINTER    (EM178)'.
00295          16  MP-007 PIC X(25) VALUE 'TEXT FILE UPDATE  (EL104)'.
00296          16  MP-008 PIC X(25) VALUE 'PRODUCER PLANS    (EM659)'.
00297          16  MP-009 PIC X(25) VALUE 'DATA ENTRY        (EM630)'.
00298          16  MP-010 PIC X(25) VALUE 'REVIEW AND CORR   (EM631)'.
00299          16  MP-011 PIC X(25) VALUE 'GRP INV MATCHLIST(EM6361)'.
00300          16  MP-012 PIC X(25) VALUE 'RATE UPDATE       (EM656)'.
00301          16  MP-013 PIC X(25) VALUE 'SOLICITATION ENTRY(EM660)'.
00302          16  MP-014 PIC X(25) VALUE 'UNDERWRITERS      (EM160)'.
00303          16  MP-015 PIC X(25) VALUE 'INDIVIDUAL PAYMENT(EM637)'.
00304          16  MP-016 PIC X(25) VALUE 'POL INV MATCHLIST(EM6362)'.
00305          16  MP-017 PIC X(25) VALUE 'PAYMENT REVERSAL  (EM639)'.
00306          16  MP-018 PIC X(25) VALUE 'PREMIUM QOUTE     (EM680)'.
00307          16  MP-019 PIC X(25) VALUE 'CLAIMS AND RES    (EM632)'.
00308          16  MP-020 PIC X(25) VALUE 'MIB SEARCH       (EM1602)'.
00309          16  MP-021 PIC X(25) VALUE 'PLN SUMMARY RPT   (EM690)'.
00310          16  MP-022 PIC X(25) VALUE 'PROD SUMMARY RPT  (EM673)'.
00311          16  MP-023 PIC X(25) VALUE 'COMP MSTR SUMMARY (EM675)'.
00312          16  MP-024 PIC X(25) VALUE 'PYMNT AND ADJ RPT (EM682)'.
00313          16  MP-025 PIC X(25) VALUE 'UNDWRTR STATUS    (EM692)'.
00314          16  MP-026 PIC X(25) VALUE 'UNDWRTR WORK SHT  (EM693)'.
00315          16  MP-027 PIC X(25) VALUE 'APP FILE SUMMARY  (EM691)'.
00316          16  MP-028 PIC X(25) VALUE 'POL HLDR - GEN    (EM694)'.
00317          16  MP-029 PIC X(25) VALUE 'POL HLDR - CLAIMS (EM695)'.
00318          16  MP-030 PIC X(25) VALUE 'POL HLDR - BILL   (EM696)'.
00319          16  MP-031 PIC X(25) VALUE 'ISSUE POLICY      (EM697)'.
00320          16  MP-032 PIC X(25) VALUE 'POL STATUS RPT    (EM698)'.
00321          16  MP-033 PIC X(25) VALUE 'RETRO PYMNTS/ADJ  (EM634)'.
00322          16  MP-034 PIC X(25) VALUE 'GRP PRM PYMNT POST(EM638)'.
00323          16  MP-035 PIC X(25) VALUE 'COMP MSTR UPDATE  (EM652)'.
00324          16  MP-036 PIC X(25) VALUE 'ONLINE REPORTS    (EL179)'.
00325          16  MP-037 PIC X(25) VALUE 'ACCOUNT RECEIVABLE(EM250)'.
00326          16  MP-038 PIC X(25) VALUE 'COMP PYMNTS/ADJS  (EM235)'.
00327          16  MP-039 PIC X(25) VALUE 'BANK MSTR UPDATE  (EM611)'.
00328          16  MP-040 PIC X(25) VALUE 'LOAN OFFICER UPDT (EM610)'.
00329          16  MP-041 PIC X(25) VALUE 'REINSTATE CANCELS (EM128)'.
00330          16  MP-042 PIC X(25) VALUE 'CHECKS(EM677,83,85,86,87)'.
00331          16  MP-043 PIC X(25) VALUE 'GENERAL LEDGER    (EM605)'.
00332          16  MP-044 PIC X(25) VALUE '****** FUTURE USE  ******'.
00333
00334  01  INDIVIDUAL-TABLES    REDEFINES    SYSTEM-APP-TABLES.
00335      12  SYSTEM    OCCURS  5 TIMES.
00336          16  APP-DESCRIPT  OCCURS  44 TIMES  PIC X(25).
00337
00338      EJECT
00339  01  MISC-WORK-AREAS.
00340      12  SYS                 PIC S9(4)      VALUE +0    COMP.
00341      12  MAXSYS              PIC S9(4)      VALUE +4    COMP.
00342      12  SLOT                PIC S9(4)      VALUE +1    COMP.
00343      12  MAXSLOT             PIC S9(4)      VALUE +20   COMP.
00344      12  APP-SLOT            PIC S9(4)      VALUE +1    COMP.
00345      12  MAXAPP              PIC S9(4)      VALUE +44   COMP.
00346      12  DSCR-IDX            PIC S9(4)      VALUE +0    COMP.
00347      12  ONE                 PIC S9(4)      VALUE +1    COMP.
00348      12  SWITCH-MODS-CNT     PIC S9(3)      VALUE +0    COMP-3.
00349          88  NO-SWITCH-MODS                 VALUE +0.
00350      12  SWITCH-ERROR-CNT    PIC S9(7)      VALUE +0    COMP-3.
00351          88  NO-SWITCH-ERRORS               VALUE +0.
00352      12  GETMAIN-SPACE       PIC  X         VALUE SPACE.
00353
00354  01  SYSTEM-DESCRIPTIONS.
00355      12  CREDIT      PIC X(26) VALUE 'CREDIT APPLICATIONS -     '.
00356      12  CLAIMS      PIC X(26) VALUE 'CLAIMS APPLICATIONS -     '.
00357      12  CREDIT-CARD PIC X(26) VALUE 'CREDIT CARD APPLICATIONS -'.
00358      12  ACCT-RECV   PIC X(26) VALUE 'ACCT RECV APPLICATIONS -  '.
00359      12  LIFE        PIC X(26) VALUE 'CONVENIENCE APPLICATIONS -'.
00360      12  GNRL-LDGR   PIC X(26) VALUE 'GENERAL LDGR APPLICATIONS-'.
00361
00362  01  DESCRIPTION-TABLE  REDEFINES  SYSTEM-DESCRIPTIONS.
00363      12  DESCRIPT  OCCURS  6 TIMES  PIC X(26).
00364
00365  01  ACCESS-KEYS.
00366      12  ELCNTL-KEY.
00367          16  CK-COMP-ID      PIC  X(3).
00368          16  CK-REC-TYPE     PIC  X          VALUE '2'.
00369          16  CK-USER-CD      PIC  X(4)       VALUE SPACES.
00370          16  CK-SEQ          PIC S9(4)       VALUE +0      COMP.
00371
00372      EJECT
00373 *                            COPY ELCDATE.
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
00374      EJECT
00375 *                            COPY ELCLOGOF.
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
00376      EJECT
00377 *                            COPY ELCATTR.
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
00378      EJECT
00379 *                            COPY ELCEMIB.
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
00380      EJECT
00381 *                            COPY ELCINTF.
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
00382      12  FILLER  REDEFINES  PI-PROGRAM-WORK-AREA.
00383          16  PI-PREV-USER    PIC  X(4).
00384          16  PI-READ-SW      PIC S9(1).
00385              88  READ-PROCESSOR-ZERO    VALUE +0.
00386              88  READ-PROCESSOR-ONE     VALUE +1.
00387          16  PI-APPLICATION  PIC S9(1).
00388              88  PI-VALID-APP           VALUES ARE +1 +2 +3 +4.
00389              88  CREDIT-APP             VALUE +1.
00390              88  CLAIMS-APP             VALUE +2.
00391              88  CREDIT-CARD-APP        VALUE +3.
00392              88  ACCT-RECV-APP          VALUE +4.
00393              88  MORTGAGE-APP           VALUE +1.
00394          16  PI-HIGH-IDX     PIC S9(4)  COMP.
00395          16  PI-LOW-IDX      PIC S9(4)  COMP.
00396          16  FILLER          PIC X(630).
00397
00398      EJECT
00399 *                            COPY ELCJPFX.
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
00400                              PIC  X(750).
00401      EJECT
00402 *                            COPY ELCAID.
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
051007*00039    02  DFHPF22   PIC  X  VALUE  'Õ'.
051007   02  DFHPF22   PIC  X  VALUE  '['.
00040    02  DFHPF23   PIC  X  VALUE  '.'.
00041    02  DFHPF24   PIC  X  VALUE  '<'.
00042    02  DFHMSRE   PIC  X  VALUE  'X'.
00043    02  DFHSTRF   PIC  X  VALUE  'h'.
00044    02  DFHTRIG   PIC  X  VALUE  '"'.
00403
00404  01  FILLER  REDEFINES  DFHAID.
00405      12  FILLER              PIC  X(8).
00406      12  PF-VALUES           PIC  X          OCCURS 2.
00407      EJECT
00408 *                            COPY EL1031S.
       01  EL1031AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  RUNDTEL PIC S9(0004) COMP.
           05  RUNDTEF PIC  X(0001).
           05  FILLER REDEFINES RUNDTEF.
               10  RUNDTEA PIC  X(0001).
           05  RUNDTEI PIC  X(0008).
      *    -------------------------------
           05  RUNTIMEL PIC S9(0004) COMP.
           05  RUNTIMEF PIC  X(0001).
           05  FILLER REDEFINES RUNTIMEF.
               10  RUNTIMEA PIC  X(0001).
           05  RUNTIMEI PIC  X(0005).
      *    -------------------------------
           05  SCRDSCRL PIC S9(0004) COMP.
           05  SCRDSCRF PIC  X(0001).
           05  FILLER REDEFINES SCRDSCRF.
               10  SCRDSCRA PIC  X(0001).
           05  SCRDSCRI PIC  X(0026).
      *    -------------------------------
           05  SLOT1L PIC S9(0004) COMP.
           05  SLOT1F PIC  X(0001).
           05  FILLER REDEFINES SLOT1F.
               10  SLOT1A PIC  X(0001).
           05  SLOT1I PIC  X(0002).
      *    -------------------------------
           05  BRWS1L PIC S9(0004) COMP.
           05  BRWS1F PIC  X(0001).
           05  FILLER REDEFINES BRWS1F.
               10  BRWS1A PIC  X(0001).
           05  BRWS1I PIC  X(0001).
      *    -------------------------------
           05  UPDT1L PIC S9(0004) COMP.
           05  UPDT1F PIC  X(0001).
           05  FILLER REDEFINES UPDT1F.
               10  UPDT1A PIC  X(0001).
           05  UPDT1I PIC  X(0001).
      *    -------------------------------
           05  DSCR1L PIC S9(0004) COMP.
           05  DSCR1F PIC  X(0001).
           05  FILLER REDEFINES DSCR1F.
               10  DSCR1A PIC  X(0001).
           05  DSCR1I PIC  X(0025).
      *    -------------------------------
           05  SLOT2L PIC S9(0004) COMP.
           05  SLOT2F PIC  X(0001).
           05  FILLER REDEFINES SLOT2F.
               10  SLOT2A PIC  X(0001).
           05  SLOT2I PIC  X(0002).
      *    -------------------------------
           05  BRWS2L PIC S9(0004) COMP.
           05  BRWS2F PIC  X(0001).
           05  FILLER REDEFINES BRWS2F.
               10  BRWS2A PIC  X(0001).
           05  BRWS2I PIC  X(0001).
      *    -------------------------------
           05  UPDT2L PIC S9(0004) COMP.
           05  UPDT2F PIC  X(0001).
           05  FILLER REDEFINES UPDT2F.
               10  UPDT2A PIC  X(0001).
           05  UPDT2I PIC  X(0001).
      *    -------------------------------
           05  DSCR2L PIC S9(0004) COMP.
           05  DSCR2F PIC  X(0001).
           05  FILLER REDEFINES DSCR2F.
               10  DSCR2A PIC  X(0001).
           05  DSCR2I PIC  X(0025).
      *    -------------------------------
           05  SLOT3L PIC S9(0004) COMP.
           05  SLOT3F PIC  X(0001).
           05  FILLER REDEFINES SLOT3F.
               10  SLOT3A PIC  X(0001).
           05  SLOT3I PIC  X(0002).
      *    -------------------------------
           05  BRWS3L PIC S9(0004) COMP.
           05  BRWS3F PIC  X(0001).
           05  FILLER REDEFINES BRWS3F.
               10  BRWS3A PIC  X(0001).
           05  BRWS3I PIC  X(0001).
      *    -------------------------------
           05  UPDT3L PIC S9(0004) COMP.
           05  UPDT3F PIC  X(0001).
           05  FILLER REDEFINES UPDT3F.
               10  UPDT3A PIC  X(0001).
           05  UPDT3I PIC  X(0001).
      *    -------------------------------
           05  DSCR3L PIC S9(0004) COMP.
           05  DSCR3F PIC  X(0001).
           05  FILLER REDEFINES DSCR3F.
               10  DSCR3A PIC  X(0001).
           05  DSCR3I PIC  X(0025).
      *    -------------------------------
           05  SLOT4L PIC S9(0004) COMP.
           05  SLOT4F PIC  X(0001).
           05  FILLER REDEFINES SLOT4F.
               10  SLOT4A PIC  X(0001).
           05  SLOT4I PIC  X(0002).
      *    -------------------------------
           05  BRWS4L PIC S9(0004) COMP.
           05  BRWS4F PIC  X(0001).
           05  FILLER REDEFINES BRWS4F.
               10  BRWS4A PIC  X(0001).
           05  BRWS4I PIC  X(0001).
      *    -------------------------------
           05  UPDT4L PIC S9(0004) COMP.
           05  UPDT4F PIC  X(0001).
           05  FILLER REDEFINES UPDT4F.
               10  UPDT4A PIC  X(0001).
           05  UPDT4I PIC  X(0001).
      *    -------------------------------
           05  DSCR4L PIC S9(0004) COMP.
           05  DSCR4F PIC  X(0001).
           05  FILLER REDEFINES DSCR4F.
               10  DSCR4A PIC  X(0001).
           05  DSCR4I PIC  X(0025).
      *    -------------------------------
           05  SLOT5L PIC S9(0004) COMP.
           05  SLOT5F PIC  X(0001).
           05  FILLER REDEFINES SLOT5F.
               10  SLOT5A PIC  X(0001).
           05  SLOT5I PIC  X(0002).
      *    -------------------------------
           05  BRWS5L PIC S9(0004) COMP.
           05  BRWS5F PIC  X(0001).
           05  FILLER REDEFINES BRWS5F.
               10  BRWS5A PIC  X(0001).
           05  BRWS5I PIC  X(0001).
      *    -------------------------------
           05  UPDT5L PIC S9(0004) COMP.
           05  UPDT5F PIC  X(0001).
           05  FILLER REDEFINES UPDT5F.
               10  UPDT5A PIC  X(0001).
           05  UPDT5I PIC  X(0001).
      *    -------------------------------
           05  DSCR5L PIC S9(0004) COMP.
           05  DSCR5F PIC  X(0001).
           05  FILLER REDEFINES DSCR5F.
               10  DSCR5A PIC  X(0001).
           05  DSCR5I PIC  X(0025).
      *    -------------------------------
           05  SLOT6L PIC S9(0004) COMP.
           05  SLOT6F PIC  X(0001).
           05  FILLER REDEFINES SLOT6F.
               10  SLOT6A PIC  X(0001).
           05  SLOT6I PIC  X(0002).
      *    -------------------------------
           05  BRWS6L PIC S9(0004) COMP.
           05  BRWS6F PIC  X(0001).
           05  FILLER REDEFINES BRWS6F.
               10  BRWS6A PIC  X(0001).
           05  BRWS6I PIC  X(0001).
      *    -------------------------------
           05  UPDT6L PIC S9(0004) COMP.
           05  UPDT6F PIC  X(0001).
           05  FILLER REDEFINES UPDT6F.
               10  UPDT6A PIC  X(0001).
           05  UPDT6I PIC  X(0001).
      *    -------------------------------
           05  DSCR6L PIC S9(0004) COMP.
           05  DSCR6F PIC  X(0001).
           05  FILLER REDEFINES DSCR6F.
               10  DSCR6A PIC  X(0001).
           05  DSCR6I PIC  X(0025).
      *    -------------------------------
           05  SLOT7L PIC S9(0004) COMP.
           05  SLOT7F PIC  X(0001).
           05  FILLER REDEFINES SLOT7F.
               10  SLOT7A PIC  X(0001).
           05  SLOT7I PIC  X(0002).
      *    -------------------------------
           05  BRWS7L PIC S9(0004) COMP.
           05  BRWS7F PIC  X(0001).
           05  FILLER REDEFINES BRWS7F.
               10  BRWS7A PIC  X(0001).
           05  BRWS7I PIC  X(0001).
      *    -------------------------------
           05  UPDT7L PIC S9(0004) COMP.
           05  UPDT7F PIC  X(0001).
           05  FILLER REDEFINES UPDT7F.
               10  UPDT7A PIC  X(0001).
           05  UPDT7I PIC  X(0001).
      *    -------------------------------
           05  DSCR7L PIC S9(0004) COMP.
           05  DSCR7F PIC  X(0001).
           05  FILLER REDEFINES DSCR7F.
               10  DSCR7A PIC  X(0001).
           05  DSCR7I PIC  X(0025).
      *    -------------------------------
           05  SLOT8L PIC S9(0004) COMP.
           05  SLOT8F PIC  X(0001).
           05  FILLER REDEFINES SLOT8F.
               10  SLOT8A PIC  X(0001).
           05  SLOT8I PIC  X(0002).
      *    -------------------------------
           05  BRWS8L PIC S9(0004) COMP.
           05  BRWS8F PIC  X(0001).
           05  FILLER REDEFINES BRWS8F.
               10  BRWS8A PIC  X(0001).
           05  BRWS8I PIC  X(0001).
      *    -------------------------------
           05  UPDT8L PIC S9(0004) COMP.
           05  UPDT8F PIC  X(0001).
           05  FILLER REDEFINES UPDT8F.
               10  UPDT8A PIC  X(0001).
           05  UPDT8I PIC  X(0001).
      *    -------------------------------
           05  DSCR8L PIC S9(0004) COMP.
           05  DSCR8F PIC  X(0001).
           05  FILLER REDEFINES DSCR8F.
               10  DSCR8A PIC  X(0001).
           05  DSCR8I PIC  X(0025).
      *    -------------------------------
           05  SLOT9L PIC S9(0004) COMP.
           05  SLOT9F PIC  X(0001).
           05  FILLER REDEFINES SLOT9F.
               10  SLOT9A PIC  X(0001).
           05  SLOT9I PIC  X(0002).
      *    -------------------------------
           05  BRWS9L PIC S9(0004) COMP.
           05  BRWS9F PIC  X(0001).
           05  FILLER REDEFINES BRWS9F.
               10  BRWS9A PIC  X(0001).
           05  BRWS9I PIC  X(0001).
      *    -------------------------------
           05  UPDT9L PIC S9(0004) COMP.
           05  UPDT9F PIC  X(0001).
           05  FILLER REDEFINES UPDT9F.
               10  UPDT9A PIC  X(0001).
           05  UPDT9I PIC  X(0001).
      *    -------------------------------
           05  DSCR9L PIC S9(0004) COMP.
           05  DSCR9F PIC  X(0001).
           05  FILLER REDEFINES DSCR9F.
               10  DSCR9A PIC  X(0001).
           05  DSCR9I PIC  X(0025).
      *    -------------------------------
           05  SLOT10L PIC S9(0004) COMP.
           05  SLOT10F PIC  X(0001).
           05  FILLER REDEFINES SLOT10F.
               10  SLOT10A PIC  X(0001).
           05  SLOT10I PIC  X(0002).
      *    -------------------------------
           05  BRWS10L PIC S9(0004) COMP.
           05  BRWS10F PIC  X(0001).
           05  FILLER REDEFINES BRWS10F.
               10  BRWS10A PIC  X(0001).
           05  BRWS10I PIC  X(0001).
      *    -------------------------------
           05  UPDT10L PIC S9(0004) COMP.
           05  UPDT10F PIC  X(0001).
           05  FILLER REDEFINES UPDT10F.
               10  UPDT10A PIC  X(0001).
           05  UPDT10I PIC  X(0001).
      *    -------------------------------
           05  DSCR10L PIC S9(0004) COMP.
           05  DSCR10F PIC  X(0001).
           05  FILLER REDEFINES DSCR10F.
               10  DSCR10A PIC  X(0001).
           05  DSCR10I PIC  X(0025).
      *    -------------------------------
           05  SLOT11L PIC S9(0004) COMP.
           05  SLOT11F PIC  X(0001).
           05  FILLER REDEFINES SLOT11F.
               10  SLOT11A PIC  X(0001).
           05  SLOT11I PIC  X(0002).
      *    -------------------------------
           05  BRWS11L PIC S9(0004) COMP.
           05  BRWS11F PIC  X(0001).
           05  FILLER REDEFINES BRWS11F.
               10  BRWS11A PIC  X(0001).
           05  BRWS11I PIC  X(0001).
      *    -------------------------------
           05  UPDT11L PIC S9(0004) COMP.
           05  UPDT11F PIC  X(0001).
           05  FILLER REDEFINES UPDT11F.
               10  UPDT11A PIC  X(0001).
           05  UPDT11I PIC  X(0001).
      *    -------------------------------
           05  DSCR11L PIC S9(0004) COMP.
           05  DSCR11F PIC  X(0001).
           05  FILLER REDEFINES DSCR11F.
               10  DSCR11A PIC  X(0001).
           05  DSCR11I PIC  X(0025).
      *    -------------------------------
           05  SLOT12L PIC S9(0004) COMP.
           05  SLOT12F PIC  X(0001).
           05  FILLER REDEFINES SLOT12F.
               10  SLOT12A PIC  X(0001).
           05  SLOT12I PIC  X(0002).
      *    -------------------------------
           05  BRWS12L PIC S9(0004) COMP.
           05  BRWS12F PIC  X(0001).
           05  FILLER REDEFINES BRWS12F.
               10  BRWS12A PIC  X(0001).
           05  BRWS12I PIC  X(0001).
      *    -------------------------------
           05  UPDT12L PIC S9(0004) COMP.
           05  UPDT12F PIC  X(0001).
           05  FILLER REDEFINES UPDT12F.
               10  UPDT12A PIC  X(0001).
           05  UPDT12I PIC  X(0001).
      *    -------------------------------
           05  DSCR12L PIC S9(0004) COMP.
           05  DSCR12F PIC  X(0001).
           05  FILLER REDEFINES DSCR12F.
               10  DSCR12A PIC  X(0001).
           05  DSCR12I PIC  X(0025).
      *    -------------------------------
           05  SLOT13L PIC S9(0004) COMP.
           05  SLOT13F PIC  X(0001).
           05  FILLER REDEFINES SLOT13F.
               10  SLOT13A PIC  X(0001).
           05  SLOT13I PIC  X(0002).
      *    -------------------------------
           05  BRWS13L PIC S9(0004) COMP.
           05  BRWS13F PIC  X(0001).
           05  FILLER REDEFINES BRWS13F.
               10  BRWS13A PIC  X(0001).
           05  BRWS13I PIC  X(0001).
      *    -------------------------------
           05  UPDT13L PIC S9(0004) COMP.
           05  UPDT13F PIC  X(0001).
           05  FILLER REDEFINES UPDT13F.
               10  UPDT13A PIC  X(0001).
           05  UPDT13I PIC  X(0001).
      *    -------------------------------
           05  DSCR13L PIC S9(0004) COMP.
           05  DSCR13F PIC  X(0001).
           05  FILLER REDEFINES DSCR13F.
               10  DSCR13A PIC  X(0001).
           05  DSCR13I PIC  X(0025).
      *    -------------------------------
           05  SLOT14L PIC S9(0004) COMP.
           05  SLOT14F PIC  X(0001).
           05  FILLER REDEFINES SLOT14F.
               10  SLOT14A PIC  X(0001).
           05  SLOT14I PIC  X(0002).
      *    -------------------------------
           05  BRWS14L PIC S9(0004) COMP.
           05  BRWS14F PIC  X(0001).
           05  FILLER REDEFINES BRWS14F.
               10  BRWS14A PIC  X(0001).
           05  BRWS14I PIC  X(0001).
      *    -------------------------------
           05  UPDT14L PIC S9(0004) COMP.
           05  UPDT14F PIC  X(0001).
           05  FILLER REDEFINES UPDT14F.
               10  UPDT14A PIC  X(0001).
           05  UPDT14I PIC  X(0001).
      *    -------------------------------
           05  DSCR14L PIC S9(0004) COMP.
           05  DSCR14F PIC  X(0001).
           05  FILLER REDEFINES DSCR14F.
               10  DSCR14A PIC  X(0001).
           05  DSCR14I PIC  X(0025).
      *    -------------------------------
           05  SLOT15L PIC S9(0004) COMP.
           05  SLOT15F PIC  X(0001).
           05  FILLER REDEFINES SLOT15F.
               10  SLOT15A PIC  X(0001).
           05  SLOT15I PIC  X(0002).
      *    -------------------------------
           05  BRWS15L PIC S9(0004) COMP.
           05  BRWS15F PIC  X(0001).
           05  FILLER REDEFINES BRWS15F.
               10  BRWS15A PIC  X(0001).
           05  BRWS15I PIC  X(0001).
      *    -------------------------------
           05  UPDT15L PIC S9(0004) COMP.
           05  UPDT15F PIC  X(0001).
           05  FILLER REDEFINES UPDT15F.
               10  UPDT15A PIC  X(0001).
           05  UPDT15I PIC  X(0001).
      *    -------------------------------
           05  DSCR15L PIC S9(0004) COMP.
           05  DSCR15F PIC  X(0001).
           05  FILLER REDEFINES DSCR15F.
               10  DSCR15A PIC  X(0001).
           05  DSCR15I PIC  X(0025).
      *    -------------------------------
           05  SLOT16L PIC S9(0004) COMP.
           05  SLOT16F PIC  X(0001).
           05  FILLER REDEFINES SLOT16F.
               10  SLOT16A PIC  X(0001).
           05  SLOT16I PIC  X(0002).
      *    -------------------------------
           05  BRWS16L PIC S9(0004) COMP.
           05  BRWS16F PIC  X(0001).
           05  FILLER REDEFINES BRWS16F.
               10  BRWS16A PIC  X(0001).
           05  BRWS16I PIC  X(0001).
      *    -------------------------------
           05  UPDT16L PIC S9(0004) COMP.
           05  UPDT16F PIC  X(0001).
           05  FILLER REDEFINES UPDT16F.
               10  UPDT16A PIC  X(0001).
           05  UPDT16I PIC  X(0001).
      *    -------------------------------
           05  DSCR16L PIC S9(0004) COMP.
           05  DSCR16F PIC  X(0001).
           05  FILLER REDEFINES DSCR16F.
               10  DSCR16A PIC  X(0001).
           05  DSCR16I PIC  X(0025).
      *    -------------------------------
           05  SLOT17L PIC S9(0004) COMP.
           05  SLOT17F PIC  X(0001).
           05  FILLER REDEFINES SLOT17F.
               10  SLOT17A PIC  X(0001).
           05  SLOT17I PIC  X(0002).
      *    -------------------------------
           05  BRWS17L PIC S9(0004) COMP.
           05  BRWS17F PIC  X(0001).
           05  FILLER REDEFINES BRWS17F.
               10  BRWS17A PIC  X(0001).
           05  BRWS17I PIC  X(0001).
      *    -------------------------------
           05  UPDT17L PIC S9(0004) COMP.
           05  UPDT17F PIC  X(0001).
           05  FILLER REDEFINES UPDT17F.
               10  UPDT17A PIC  X(0001).
           05  UPDT17I PIC  X(0001).
      *    -------------------------------
           05  DSCR17L PIC S9(0004) COMP.
           05  DSCR17F PIC  X(0001).
           05  FILLER REDEFINES DSCR17F.
               10  DSCR17A PIC  X(0001).
           05  DSCR17I PIC  X(0025).
      *    -------------------------------
           05  SLOT18L PIC S9(0004) COMP.
           05  SLOT18F PIC  X(0001).
           05  FILLER REDEFINES SLOT18F.
               10  SLOT18A PIC  X(0001).
           05  SLOT18I PIC  X(0002).
      *    -------------------------------
           05  BRWS18L PIC S9(0004) COMP.
           05  BRWS18F PIC  X(0001).
           05  FILLER REDEFINES BRWS18F.
               10  BRWS18A PIC  X(0001).
           05  BRWS18I PIC  X(0001).
      *    -------------------------------
           05  UPDT18L PIC S9(0004) COMP.
           05  UPDT18F PIC  X(0001).
           05  FILLER REDEFINES UPDT18F.
               10  UPDT18A PIC  X(0001).
           05  UPDT18I PIC  X(0001).
      *    -------------------------------
           05  DSCR18L PIC S9(0004) COMP.
           05  DSCR18F PIC  X(0001).
           05  FILLER REDEFINES DSCR18F.
               10  DSCR18A PIC  X(0001).
           05  DSCR18I PIC  X(0025).
      *    -------------------------------
           05  SLOT19L PIC S9(0004) COMP.
           05  SLOT19F PIC  X(0001).
           05  FILLER REDEFINES SLOT19F.
               10  SLOT19A PIC  X(0001).
           05  SLOT19I PIC  X(0002).
      *    -------------------------------
           05  BRWS19L PIC S9(0004) COMP.
           05  BRWS19F PIC  X(0001).
           05  FILLER REDEFINES BRWS19F.
               10  BRWS19A PIC  X(0001).
           05  BRWS19I PIC  X(0001).
      *    -------------------------------
           05  UPDT19L PIC S9(0004) COMP.
           05  UPDT19F PIC  X(0001).
           05  FILLER REDEFINES UPDT19F.
               10  UPDT19A PIC  X(0001).
           05  UPDT19I PIC  X(0001).
      *    -------------------------------
           05  DSCR19L PIC S9(0004) COMP.
           05  DSCR19F PIC  X(0001).
           05  FILLER REDEFINES DSCR19F.
               10  DSCR19A PIC  X(0001).
           05  DSCR19I PIC  X(0025).
      *    -------------------------------
           05  SLOT20L PIC S9(0004) COMP.
           05  SLOT20F PIC  X(0001).
           05  FILLER REDEFINES SLOT20F.
               10  SLOT20A PIC  X(0001).
           05  SLOT20I PIC  X(0002).
      *    -------------------------------
           05  BRWS20L PIC S9(0004) COMP.
           05  BRWS20F PIC  X(0001).
           05  FILLER REDEFINES BRWS20F.
               10  BRWS20A PIC  X(0001).
           05  BRWS20I PIC  X(0001).
      *    -------------------------------
           05  UPDT20L PIC S9(0004) COMP.
           05  UPDT20F PIC  X(0001).
           05  FILLER REDEFINES UPDT20F.
               10  UPDT20A PIC  X(0001).
           05  UPDT20I PIC  X(0001).
      *    -------------------------------
           05  DSCR20L PIC S9(0004) COMP.
           05  DSCR20F PIC  X(0001).
           05  FILLER REDEFINES DSCR20F.
               10  DSCR20A PIC  X(0001).
           05  DSCR20I PIC  X(0025).
      *    -------------------------------
           05  ERRMSGL PIC S9(0004) COMP.
           05  ERRMSGF PIC  X(0001).
           05  FILLER REDEFINES ERRMSGF.
               10  ERRMSGA PIC  X(0001).
           05  ERRMSGI PIC  X(0076).
      *    -------------------------------
           05  ENTERPFL PIC S9(0004) COMP.
           05  ENTERPFF PIC  X(0001).
           05  FILLER REDEFINES ENTERPFF.
               10  ENTERPFA PIC  X(0001).
           05  ENTERPFI PIC  9(2).
       01  EL1031AO REDEFINES EL1031AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RUNDTEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RUNTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SCRDSCRO PIC  X(0026).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SLOT1O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BRWS1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  UPDT1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DSCR1O PIC  X(0025).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SLOT2O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BRWS2O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  UPDT2O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DSCR2O PIC  X(0025).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SLOT3O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BRWS3O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  UPDT3O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DSCR3O PIC  X(0025).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SLOT4O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BRWS4O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  UPDT4O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DSCR4O PIC  X(0025).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SLOT5O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BRWS5O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  UPDT5O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DSCR5O PIC  X(0025).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SLOT6O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BRWS6O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  UPDT6O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DSCR6O PIC  X(0025).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SLOT7O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BRWS7O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  UPDT7O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DSCR7O PIC  X(0025).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SLOT8O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BRWS8O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  UPDT8O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DSCR8O PIC  X(0025).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SLOT9O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BRWS9O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  UPDT9O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DSCR9O PIC  X(0025).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SLOT10O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BRWS10O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  UPDT10O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DSCR10O PIC  X(0025).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SLOT11O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BRWS11O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  UPDT11O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DSCR11O PIC  X(0025).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SLOT12O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BRWS12O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  UPDT12O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DSCR12O PIC  X(0025).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SLOT13O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BRWS13O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  UPDT13O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DSCR13O PIC  X(0025).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SLOT14O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BRWS14O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  UPDT14O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DSCR14O PIC  X(0025).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SLOT15O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BRWS15O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  UPDT15O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DSCR15O PIC  X(0025).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SLOT16O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BRWS16O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  UPDT16O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DSCR16O PIC  X(0025).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SLOT17O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BRWS17O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  UPDT17O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DSCR17O PIC  X(0025).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SLOT18O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BRWS18O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  UPDT18O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DSCR18O PIC  X(0025).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SLOT19O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BRWS19O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  UPDT19O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DSCR19O PIC  X(0025).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SLOT20O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BRWS20O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  UPDT20O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DSCR20O PIC  X(0025).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSGO PIC  X(0076).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ENTERPFO PIC  X(0002).
      *    -------------------------------
00409
00410  01  FILLER  REDEFINES  EL1031AI.
00411      12  FILLER                  PIC  X(60).
00412      12  APP-INFO                OCCURS 20 TIMES.
00413          16  SLOT-NUM-LENGTH     PIC S9(4)   COMP.
00414          16  SLOT-NUM-ATTRB      PIC  X.
00415          16  SLOT-NUM            PIC  99.
00416          16  BLANK-SLOT-NUM  REDEFINES  SLOT-NUM
00417                                  PIC  XX.
00418          16  BRWS-SWITCH-LENGTH  PIC S9(4)   COMP.
00419          16  BRWS-SWITCH-ATTRB   PIC  X.
00420          16  BRWS-SWITCH         PIC  X.
00421          16  UPDT-SWITCH-LENGTH  PIC S9(4)   COMP.
00422          16  UPDT-SWITCH-ATTRB   PIC  X.
00423          16  UPDT-SWITCH         PIC  X.
00424          16  DSCR-LENGTH         PIC S9(4)   COMP.
00425          16  DSCR-ATTRB          PIC  X.
00426          16  DSCR                PIC  X(25).
00427
00428      EJECT
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
00430  01  DFHCOMMAREA             PIC  X(1024).
00431
00432 *01 PARMLIST .
00433 *    12  FILLER              PIC S9(8)   COMP.
00434 *    12  ELCNTL-POINTER      PIC S9(8)   COMP.
00435      EJECT
00436 *                            COPY ELCCNTL.
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
00437
00438      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA CONTROL-FILE.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL1031' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00440
00441      MOVE EIBDATE                TO DC-JULIAN-YYDDD.
00442      MOVE '5'                    TO DC-OPTION-CODE.
00443
00444      PERFORM 9700-LINK-DATE-CONVERT  THRU  9799-EXIT.
00445
00446      MOVE DC-GREG-DATE-1-EDIT    TO SAVE-DATE.
00447      MOVE DC-BIN-DATE-1          TO SAVE-BIN-DATE.
00448      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.
00449      MOVE PI-APPLICATION         TO SYS.
00450
00451 ************************************************************
00452 *    IF ATTEMPTING TO EXECUTE PROGRAM WITHOUT SIGNING ON   *
00453 *    (COMM LENGTH = ZERO), SEND ERROR MESSAGE.             *
00454 ************************************************************
00455
00456      IF EIBCALEN  = 0
00457          GO TO 8800-UNAUTHORIZED-ACCESS.
00458
00459      IF PI-CALLING-PROGRAM  NOT = THIS-PGM
00460          IF PI-RETURN-TO-PROGRAM  NOT = THIS-PGM
00461              MOVE PI-SAVED-PROGRAM-5    TO PI-SAVED-PROGRAM-6
00462              MOVE PI-SAVED-PROGRAM-4    TO PI-SAVED-PROGRAM-5
00463              MOVE PI-SAVED-PROGRAM-3    TO PI-SAVED-PROGRAM-4
00464              MOVE PI-SAVED-PROGRAM-2    TO PI-SAVED-PROGRAM-3
00465              MOVE PI-SAVED-PROGRAM-1    TO PI-SAVED-PROGRAM-2
00466              MOVE PI-RETURN-TO-PROGRAM  TO PI-SAVED-PROGRAM-1
00467              MOVE PI-CALLING-PROGRAM    TO PI-RETURN-TO-PROGRAM
00468              MOVE THIS-PGM              TO PI-CALLING-PROGRAM
00469          ELSE
00470              MOVE PI-RETURN-TO-PROGRAM  TO PI-CALLING-PROGRAM
00471              MOVE PI-SAVED-PROGRAM-1    TO PI-RETURN-TO-PROGRAM
00472              MOVE PI-SAVED-PROGRAM-2    TO PI-SAVED-PROGRAM-1
00473              MOVE PI-SAVED-PROGRAM-3    TO PI-SAVED-PROGRAM-2
00474              MOVE PI-SAVED-PROGRAM-4    TO PI-SAVED-PROGRAM-3
00475              MOVE PI-SAVED-PROGRAM-5    TO PI-SAVED-PROGRAM-4
00476              MOVE PI-SAVED-PROGRAM-6    TO PI-SAVED-PROGRAM-5
00477              MOVE SPACES                TO PI-SAVED-PROGRAM-6.
00478
00479      
      * EXEC CICS  HANDLE CONDITION
00480 *        NOTOPEN   (8870-NOTOPEN)
00481 *        NOTFND    (8880-NOT-FOUND)
00482 *        PGMIDERR  (9600-PGMID-ERROR)
00483 *        ERROR     (9999-ABEND)
00484 *        END-EXEC.
      *    MOVE '"$JIL.                ! " #00003518' TO DFHEIV0
           MOVE X'22244A494C2E202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303033353138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00485
00486      IF EIBTRNID  NOT = TRANS-ID
00487          MOVE LOW-VALUES         TO EL1031AO
00488          MOVE PI-APPLICATION     TO SYS
00489          GO TO 7000-BUILD-OUTPUT-MAP.
00490
00491      IF EIBAID  = DFHCLEAR
00492          GO TO 9400-CLEAR.
00493
00494      EJECT
00495  0200-RECEIVE.
00496
00497      MOVE LOW-VALUES             TO EL1031AI.
00498
00499      IF EIBAID  = DFHPA1  OR  DFHPA2  OR  DFHPA3
00500          MOVE ER-7008            TO EMI-ERROR
00501          PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT
00502          MOVE -1                 TO BRWS1L
00503          GO TO 8200-SEND-DATAONLY.
00504
00505      
      * EXEC CICS  RECEIVE
00506 *        MAP     (MAP-NAME)
00507 *        MAPSET  (MAPSET-NAME)
00508 *        INTO    (EL1031AI)
00509 *        END-EXEC.
           MOVE LENGTH OF
            EL1031AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00003544' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033353434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL1031AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00510
00511      IF ENTERPFL  = ZERO
00512          GO TO 0300-CHECK-PFKEYS.
00513
00514      IF EIBAID  NOT = DFHENTER
00515          MOVE ER-0004            TO EMI-ERROR
00516          GO TO 0310-INPUT-ERROR.
00517
00518      IF ENTERPFI GREATER 0 AND LESS 25
00519          MOVE PF-VALUES (ENTERPFI)  TO EIBAID
00520      ELSE
00521          MOVE ER-0029               TO EMI-ERROR
00522          GO TO 0310-INPUT-ERROR.
00523
00524      EJECT
00525  0300-CHECK-PFKEYS.
00526
00527      IF EIBAID  = DFHPF23
00528          GO TO 8820-PF23.
00529
00530      IF EIBAID  = DFHPF24
00531          GO TO 9200-RETURN-MAIN-MENU.
00532
00533      IF EIBAID  =  DFHPF12
00534          GO TO 9500-PF12.
00535
00536      IF EIBAID  = DFHPF1
00537          GO TO 5000-ROLL-APP-FORWARD.
00538
00539      IF EIBAID  = DFHPF2
00540          GO TO 5100-ROLL-APP-BACKWARD.
00541
00542      IF EIBAID  = DFHENTER
00543          GO TO 1000-EDIT-DATA.
00544
00545      MOVE ER-0029                TO EMI-ERROR.
00546
00547  0310-INPUT-ERROR.
00548
00549      PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT.
00550
00551      MOVE AL-UNBON               TO ENTERPFA.
00552
00553      IF ENTERPFL = ZERO
00554          MOVE -1                 TO BRWS1L
00555      ELSE
00556          MOVE -1                 TO ENTERPFL.
00557
00558      GO TO 8200-SEND-DATAONLY.
00559
00560      EJECT
00561  1000-EDIT-DATA.
00562
00563      IF BRWS-SWITCH-LENGTH(SLOT) EQUAL ZERO AND
00564         UPDT-SWITCH-LENGTH(SLOT) EQUAL ZERO
00565         GO TO 1100-SWITCH-LENGTH-ZERO.
00566
00567      ADD +1                      TO SWITCH-MODS-CNT.
00568
00569      IF (BRWS-SWITCH-LENGTH(SLOT) GREATER THAN ZERO)  OR
00570         (UPDT-SWITCH-LENGTH(SLOT) GREATER THAN ZERO)
00571          IF PI-USER-ALMIGHTY-YES
00572              NEXT SENTENCE
00573          ELSE
00574              MOVE ER-0007            TO EMI-ERROR
00575              PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT
00576              MOVE LOW-VALUES         TO EL1031AO
00577              GO TO 8200-SEND-DATAONLY.
00578
00579      IF BRWS-SWITCH-LENGTH(SLOT) GREATER THAN ZERO
00580          IF BRWS-SWITCH(SLOT) NOT EQUAL 'Y' AND 'N'
00581              ADD +1              TO SWITCH-ERROR-CNT
00582              MOVE -1             TO BRWS-SWITCH-LENGTH(SLOT)
00583              MOVE AL-UABON       TO BRWS-SWITCH-ATTRB(SLOT).
00584
00585      IF UPDT-SWITCH-LENGTH(SLOT) GREATER THAN ZERO
00586          IF UPDT-SWITCH(SLOT) NOT EQUAL 'Y' AND 'N'
00587              ADD +1              TO SWITCH-ERROR-CNT
00588              MOVE -1             TO UPDT-SWITCH-LENGTH(SLOT)
00589              MOVE AL-UABON       TO UPDT-SWITCH-ATTRB(SLOT).
00590
00591  1100-SWITCH-LENGTH-ZERO.
00592
00593      IF SLOT LESS THAN MAXSLOT
00594          ADD +1                  TO SLOT
00595          GO TO 1000-EDIT-DATA.
00596
00597      IF NO-SWITCH-MODS
00598          MOVE -1                 TO BRWS-SWITCH-LENGTH(1)
00599          MOVE ER-7322            TO EMI-ERROR
00600          PERFORM 9000-ERROR-FORMAT THRU 9099-EXIT
00601          GO TO 8200-SEND-DATAONLY.
00602
00603      IF NOT NO-SWITCH-ERRORS
00604          MOVE ER-7320            TO EMI-ERROR
00605          PERFORM 9000-ERROR-FORMAT THRU 9099-EXIT
00606          GO TO 8200-SEND-DATAONLY.
00607
00608      MOVE PI-COMPANY-ID          TO CK-COMP-ID.
00609      MOVE PI-PREV-USER           TO CK-USER-CD.
00610      MOVE '2'                    TO CK-REC-TYPE.
00611
00612      IF READ-PROCESSOR-ONE
00613          MOVE PI-READ-SW         TO CK-SEQ.
00614
00615      
      * EXEC CICS  READ
00616 *        SET      (ADDRESS OF CONTROL-FILE)
00617 *        DATASET  (FILE-ID-ELCNTL)
00618 *        RIDFLD   (ELCNTL-KEY)
00619 *        UPDATE
00620 *        END-EXEC.
      *    MOVE '&"S        EU         (   #00003654' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033363534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ELCNTL, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00621
00622      IF CF-LAST-MAINT-BY     NOT = PI-UPDATE-BY  OR
00623         CF-LAST-MAINT-HHMMSS NOT = PI-UPDATE-HHMMSS
00624          
      * EXEC CICS  UNLOCK
00625 *            DATASET  ('ELCNTL')
00626 *            END-EXEC
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&*                    #   #00003663' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303033363633' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00627          MOVE ER-0068            TO EMI-ERROR
00628          PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT
00629          GO TO 7000-BUILD-OUTPUT-MAP.
00630
00631      MOVE 'B'                    TO JP-RECORD-TYPE.
00632      MOVE CONTROL-FILE           TO JP-RECORD-AREA.
00633
00634      PERFORM 8400-LOG-JOURNAL-RECORD  THRU  8499-EXIT.
00635
00636  1200-UPDATE-LOOP.
00637
00638      MOVE PI-APPLICATION         TO SYS.
00639
00640      MOVE PI-LOW-IDX             TO APP-SLOT.
00641
00642      PERFORM 2000-UPDATE-RECORD-SWITCHES THRU 2000-EXIT
00643          VARYING SLOT  FROM  +1  BY  +1
00644              UNTIL SLOT GREATER THAN MAXSLOT.
00645
00646      MOVE PI-PROCESSOR-ID        TO CF-LAST-MAINT-BY
00647                                     PI-UPDATE-BY.
00648      MOVE EIBTIME                TO CF-LAST-MAINT-HHMMSS
00649                                     PI-UPDATE-HHMMSS.
00650      MOVE EIBDATE                TO DC-JULIAN-YYDDD.
00651      MOVE '5'                    TO DC-OPTION-CODE.
00652      MOVE LINK-ELDATCV           TO PGM-NAME.
00653
00654      
      * EXEC CICS  LINK
00655 *        PROGRAM   (PGM-NAME)
00656 *        COMMAREA  (DATE-CONVERSION-DATA)
00657 *        LENGTH    (DC-COMM-LENGTH)
00658 *        END-EXEC.
      *    MOVE '."C                   ''   #00003693' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033363933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00659
00660      MOVE DC-BIN-DATE-1          TO CF-LAST-MAINT-DT.
00661      MOVE 'C'                    TO JP-RECORD-TYPE.
00662      MOVE CONTROL-FILE           TO JP-RECORD-AREA.
00663
00664      
      * EXEC CICS  REWRITE
00665 *        DATASET  ('ELCNTL')
00666 *        FROM     (CONTROL-FILE)
00667 *        END-EXEC.
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&& L                  %   #00003703' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303033373033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00668
00669      PERFORM 8400-LOG-JOURNAL-RECORD  THRU  8499-EXIT.
00670      MOVE ER-0000                TO EMI-ERROR.
00671      PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT.
00672
00673      MOVE PI-LOW-IDX             TO APP-SLOT.
00674      MOVE LOW-VALUES             TO EL1031AO.
00675      MOVE -1                     TO BRWS1L.
00676      GO TO 7000-BUILD-OUTPUT-MAP.
00677
00678  2000-UPDATE-RECORD-SWITCHES.
00679
00680      IF BRWS-SWITCH-LENGTH(SLOT) GREATER THAN ZERO
00681          MOVE BRWS-SWITCH(SLOT)  TO CF-BROWSE-APP(SYS APP-SLOT).
00682
00683      IF UPDT-SWITCH-LENGTH(SLOT) GREATER THAN ZERO
00684          MOVE UPDT-SWITCH(SLOT)  TO CF-UPDATE-APP(SYS APP-SLOT).
00685
00686      ADD +1                      TO APP-SLOT.
00687
00688  2000-EXIT.
00689      EXIT.
00690
00691      EJECT
00692  5000-ROLL-APP-FORWARD.
00693
00694      IF PI-HIGH-IDX GREATER THAN +44
00695          MOVE +01                TO PI-HIGH-IDX
00696          MOVE ER-7299            TO EMI-ERROR
00697          PERFORM 9000-ERROR-FORMAT THRU 9099-EXIT
00698          GO TO 7000-BUILD-OUTPUT-MAP.
00699
00700      COMPUTE APP-SLOT EQUAL PI-HIGH-IDX - +1.
00701
00702      GO TO 7000-BUILD-OUTPUT-MAP.
00703
00704      EJECT
00705  5100-ROLL-APP-BACKWARD.
00706
00707      IF PI-LOW-IDX LESS THAN MAXSLOT
00708          MOVE +1                 TO PI-LOW-IDX
00709      ELSE
00710          COMPUTE PI-LOW-IDX EQUAL PI-LOW-IDX - +19.
00711
00712      MOVE PI-LOW-IDX             TO APP-SLOT.
00713
00714      EJECT
00715  7000-BUILD-OUTPUT-MAP.
00716
00717      PERFORM 7500-READ-PROCESSOR THRU 7500-EXIT.
00718
00719      MOVE CF-LAST-MAINT-BY       TO PI-UPDATE-BY.
00720      MOVE CF-LAST-MAINT-HHMMSS   TO PI-UPDATE-HHMMSS.
00721
00722      MOVE SYS                    TO DSCR-IDX.
00723
00724      IF READ-PROCESSOR-ONE
00725          COMPUTE DSCR-IDX EQUAL DSCR-IDX + +4.
00726
00727      PERFORM 7100-FILL-APPLICATION-DISPLAY THRU 7100-EXIT
00728          VARYING SLOT FROM ONE BY ONE
00729              UNTIL SLOT GREATER THAN MAXSLOT OR
00730                  APP-SLOT GREATER THAN MAXAPP.
00731
00732      MOVE APP-SLOT               TO PI-HIGH-IDX.
00733
00734      PERFORM 7200-BLANK-DISPLAY THRU 7200-EXIT
00735          VARYING SLOT FROM SLOT BY ONE
00736              UNTIL SLOT GREATER THAN MAXSLOT.
00737
00738      GO TO 8100-SEND-INITIAL-MAP.
00739
00740  7100-FILL-APPLICATION-DISPLAY.
00741
00742      IF SLOT EQUAL ONE
00743          MOVE APP-SLOT           TO PI-LOW-IDX.
00744
00745      MOVE APP-DESCRIPT(DSCR-IDX APP-SLOT)
00746                                  TO DSCR(SLOT).
00747      MOVE CF-BROWSE-APP(SYS APP-SLOT)
00748                                  TO BRWS-SWITCH(SLOT).
00749      MOVE CF-UPDATE-APP(SYS APP-SLOT)
00750                                  TO UPDT-SWITCH(SLOT).
00751      MOVE APP-SLOT               TO SLOT-NUM(SLOT).
00752      ADD +1                      TO APP-SLOT.
00753
00754  7100-EXIT.
00755      EXIT.
00756
00757  7200-BLANK-DISPLAY.
00758
00759      MOVE SPACES                 TO DSCR(SLOT)
00760                                     BRWS-SWITCH(SLOT)
00761                                     UPDT-SWITCH(SLOT)
00762                                     BLANK-SLOT-NUM(SLOT).
00763      ADD +1                      TO APP-SLOT.
00764
00765  7200-EXIT.
00766      EXIT.
00767
00768      EJECT
00769  7500-READ-PROCESSOR.
00770
00771      MOVE PI-COMPANY-ID          TO CK-COMP-ID.
00772      MOVE PI-PREV-USER           TO CK-USER-CD.
00773      MOVE '2'                    TO CK-REC-TYPE.
00774      MOVE PI-READ-SW             TO CK-SEQ.
00775
00776      
      * EXEC CICS  READ
00777 *        SET      (ADDRESS OF CONTROL-FILE)
00778 *        DATASET  (FILE-ID-ELCNTL)
00779 *        RIDFLD   (ELCNTL-KEY)
00780 *        END-EXEC.
      *    MOVE '&"S        E          (   #00003815' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ELCNTL, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00781
00782  7500-EXIT.
00783      EXIT.
00784
00785      EJECT
00786  8100-SEND-INITIAL-MAP.
00787
00788      MOVE SAVE-DATE              TO RUNDTEO.
00789      MOVE EIBTIME                TO TIME-IN.
00790      MOVE TIME-OUT               TO RUNTIMEO.
00791
00792      IF READ-PROCESSOR-ONE
00793          COMPUTE SYS EQUAL SYS + +4.
00794
00795      MOVE DESCRIPT(SYS)          TO SCRDSCRO.
00796      MOVE -1                     TO BRWS1L.
00797      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO.
00798
00799      
      * EXEC CICS  SEND
00800 *        MAP     (MAP-NAME)
00801 *        MAPSET  (MAPSET-NAME)
00802 *        FROM    (EL1031AO)
00803 *        ERASE
00804 *        CURSOR
00805 *        END-EXEC.
           MOVE LENGTH OF
            EL1031AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00003838' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383338' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL1031AO, 
                 DFHEIV12, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00806
00807      GO TO 9100-RETURN-TRAN.
00808
00809  8200-SEND-DATAONLY.
00810
00811      MOVE SAVE-DATE              TO RUNDTEO.
00812      MOVE EIBTIME                TO TIME-IN.
00813      MOVE TIME-OUT               TO RUNTIMEO.
00814
00815      IF READ-PROCESSOR-ONE
00816          COMPUTE SYS EQUAL SYS + +4.
00817
00818      MOVE DESCRIPT(SYS)          TO SCRDSCRO.
00819      MOVE -1                     TO BRWS1L.
00820      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO
00821
00822      
      * EXEC CICS  SEND
00823 *        MAP     (MAP-NAME)
00824 *        MAPSET  (MAPSET-NAME)
00825 *        FROM    (EL1031AO)
00826 *        DATAONLY
00827 *        CURSOR
00828 *        END-EXEC.
           MOVE LENGTH OF
            EL1031AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00003861' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL1031AO, 
                 DFHEIV12, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00829
00830      GO TO 9100-RETURN-TRAN.
00831
00832  8300-SEND-TEXT.
00833
00834      
      * EXEC CICS  SEND TEXT
00835 *        FROM    (LOGOFF-TEXT)
00836 *        LENGTH  (LOGOFF-LENGTH)
00837 *        ERASE
00838 *        FREEKB
00839 *        END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00003873' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383733' TO DFHEIV0(25:11)
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
           
00840
00841      
      * EXEC CICS  RETURN
00842 *        END-EXEC.
      *    MOVE '.(                    &   #00003880' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00843
00844  8400-LOG-JOURNAL-RECORD.
00845
00846      IF PI-JOURNAL-FILE-ID  = ZERO
00847          GO TO 8499-EXIT.
00848
00849      MOVE PI-PROCESSOR-ID        TO JP-USER-ID.
00850      MOVE FILE-ID-ELCNTL         TO JP-FILE-ID.
00851      MOVE THIS-PGM               TO JP-PROGRAM-ID.
00852
012009*    EXEC CICS  JOURNAL
012009*        JFILEID  (PI-JOURNAL-FILE-ID)
012009*        JTYPEID  ('EL')
012009*        FROM     (JOURNAL-RECORD)
012009*        LENGTH   (773)
012009*        END-EXEC.
           .
00860  8499-EXIT.
00861      EXIT.
00862
00863  8800-UNAUTHORIZED-ACCESS.
00864
00865      MOVE UNACCESS-MSG           TO LOGOFF-MSG.
00866      GO TO 8300-SEND-TEXT.
00867
00868  8820-PF23.
00869
00870      MOVE EIBAID                 TO PI-ENTRY-CD-1.
00871      MOVE XCTL-005               TO PGM-NAME.
00872      GO TO 9300-XCTL.
00873
00874  8870-NOTOPEN.
00875
00876      MOVE ER-0042                TO EMI-ERROR.
00877      PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT.
00878      MOVE -1                     TO BRWS1L.
00879      GO TO 8200-SEND-DATAONLY.
00880
00881  8880-NOT-FOUND.
00882
00883      MOVE ER-0073                TO EMI-ERROR.
00884      PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT.
00885      MOVE -1                     TO BRWS1L.
00886      MOVE AL-UABON               TO BRWS1A.
00887      GO TO 8200-SEND-DATAONLY.
00888
00889  9100-RETURN-TRAN.
00890
00891      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.
00892      MOVE '103B'                 TO PI-CURRENT-SCREEN-NO.
00893
00894      
      * EXEC CICS  RETURN
00895 *        TRANSID   (TRANS-ID)
00896 *        COMMAREA  (PROGRAM-INTERFACE-BLOCK)
00897 *        LENGTH    (PI-COMM-LENGTH)
00898 *        END-EXEC.
      *    MOVE '.(CT                  &   #00003933' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393333' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00899
00900  9200-RETURN-MAIN-MENU.
00901
00902      IF PI-SESSION-IN-PROGRESS  = '1'
00903          MOVE XCTL-126           TO PGM-NAME.
00904
00905      IF PI-SESSION-IN-PROGRESS  = '2'
00906          MOVE XCTL-626           TO PGM-NAME.
00907
00908      IF PI-SESSION-IN-PROGRESS  = '4'
00909          MOVE XCTL-400           TO PGM-NAME.
00910
00911      IF PI-SESSION-IN-PROGRESS  = '5'
00912          MOVE XCTL-800           TO PGM-NAME.
00913
00914      GO TO 9300-XCTL.
00915
00916  9300-XCTL.
00917      
      * EXEC CICS  XCTL
00918 *        PROGRAM   (PGM-NAME)
00919 *        COMMAREA  (PROGRAM-INTERFACE-BLOCK)
00920 *        LENGTH    (PI-COMM-LENGTH)
00921 *        END-EXEC.
      *    MOVE '.$C                   $   #00003956' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393536' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00922
00923  9400-CLEAR.
00924      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME.
00925
00926      GO TO 9300-XCTL.
00927
00928  9500-PF12.
00929      MOVE XCTL-010               TO PGM-NAME.
00930
00931      GO TO 9300-XCTL.
00932
00933  9600-PGMID-ERROR.
00934      
      * EXEC CICS  HANDLE CONDITION
00935 *        PGMIDERR  (8300-SEND-TEXT)
00936 *        END-EXEC.
      *    MOVE '"$L                   ! # #00003973' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303033393733' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00937
00938      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.
00939      MOVE ' '                    TO PI-ENTRY-CD-1.
00940      MOVE XCTL-005               TO PGM-NAME.
00941      MOVE PGM-NAME               TO LOGOFF-PGM.
00942      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.
00943
00944      GO TO 9300-XCTL.
00945
00946  9700-LINK-DATE-CONVERT.
00947      
      * EXEC CICS  LINK
00948 *        PROGRAM   ('ELDATCV')
00949 *        COMMAREA  (DATE-CONVERSION-DATA)
00950 *        LENGTH    (DC-COMM-LENGTH)
00951 *        END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   ''   #00003986' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393836' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00952
00953  9799-EXIT.
00954      EXIT.
00955
00956  9000-ERROR-FORMAT.
00957      IF NOT EMI-ERRORS-COMPLETE
00958          MOVE LINK-001           TO PGM-NAME
00959          
      * EXEC CICS  LINK
00960 *            PROGRAM   (PGM-NAME)
00961 *            COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)
00962 *            LENGTH    (EMI-COMM-LENGTH)
00963 *            END-EXEC.
      *    MOVE '."C                   ''   #00003998' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393938' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00964
00965  9099-EXIT.
00966      EXIT.
00967
00968  9999-ABEND.
00969      MOVE LINK-004               TO PGM-NAME.
00970      MOVE DFHEIBLK               TO EMI-LINE1
00971
00972      
      * EXEC CICS  LINK
00973 *        PROGRAM   (PGM-NAME)
00974 *        COMMAREA  (EMI-LINE1)
00975 *        LENGTH    (72)
00976 *        END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   ''   #00004011' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034303131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00977
00978      GO TO 8200-SEND-DATAONLY.
00979

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1031' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 8870-NOTOPEN,
                     8880-NOT-FOUND,
                     9600-PGMID-ERROR,
                     9999-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1031' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
