00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL176 .
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 07/13/94 11:11:10.
00007 *                            VMOD=2.060
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
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC, INC.     *
00021 *            *                                                   *
00022 *            *****************************************************
00023
00024 *REMARKS.
00025 *        IN ORDER TO START THE PRINTING OF CHECKS, THIS FUNCTION
00026 *    IS USED TO QUALIFY THE CONTROL BATCHES TO BE PRINTED AND
00027 *    SPECIFY A PRINT TIME.  THE INDIVIDUAL CHECK WRITER PROGRAM
00028 *    IS STARTED BY THIS PROGRAM.
00029
00030 *    SCREENS     - EL176A - CHECK WRITER
00031
00032 *    ENTERED BY  - EL171  - REPORT MENU
00033
00034 *    EXIT TO     - EL171  - RESULT OF CLEAR
00035
00036 *    INPUT FILES - ELCHKQ - CHECK QUEUE
00037 *                  ELTRLR - ACTIVITY TRAILERS
00038 *                  ELCNTL - CONTROL FILE
00039 *                  ELMSTR - CLAIM MASTER
00040 *                  ELCERT - CERTIFICATE MASTER
00041 *                  ELBENE - BENEFICIARY MASTER
00042 *                  ERACCT - ACCOUNT MASTER
00043 *                  ERCOMP - COMPENSATION MASTER
00044 *                  MPPLAN - CONVENIENCE PRODUCER PLAN MASTER
00045 *                  MPPLCY - CONVENIENCE POLICY MASTER
00046 *                  MPPROD - CONVENIENCE PRODUCER MASTER
00047
00048 *    OUTPUT FILES - ELTRLR - PAYMENT TRAILERS
00049 *                   ELCHKQ - CHECK QUEUE
00050
00051 *    COMMAREA    - PASSED.  PRINT TIME PASSED TO CHECK WRITER AS
00052 *                  A 2 BYTE COMP NUMBER IN THE FIRST TWO BYTES OF
00053 *                  THE WORK AREA.  FORM TYPE IS PASSED AS
00054 *                  ENTRY-CD-1.
00055
00042 *
092602******************************************************************
092602*                   C H A N G E   L O G
092602*
092602* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
092602*-----------------------------------------------------------------
092602*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
092602* EFFECTIVE    NUMBER
092602*-----------------------------------------------------------------
CIDMOD*                - JWBA 7/94 PER IR94025-8010 TO ADD HANDLE
CIDMOD*   LGC123         CONDITIONS FOR CLOSED/DISABLED MICRFLAG AN
CIDMOD*                  MICR.DRAFTS FILE.
CIDMOD*
CIDMOD*                - JWBA 4/95 PER IR94196-8912 - ADD OF "NOTFN
CIDMOD*   LGC134         HANDLE CONDITION;
CSODJN*
CSODJN*                - DJNA 4/01/2000 CR#2000030100009
CSODJN*   CSODJN         DRAFT NUMBER EXPANSION.
102902* 102902    2001061800003  PEMA  ADD DCC TO MICR PROCESSING
121902* 121902    2001061800003  SMVA  ADD PROCESSING FOR NEW CLM TYP I
121203* 121203                   SMVA  ADD PROCESSING FOR NEW CLM TYP G
042704* 042704    2004042700002  PEMA  REMOVE NOTIFY NAME AND ADDRESS
070104* 070104    IR             PEMA  MODIFY LENGTH OF MICRDRFT
011105* 011105    2004071200002  PEMA  ADD ONE COMMENT LINE
030612* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
052614* 052614    2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
052814* 052814  CR2014012300001  PEMA  DCC CREDIT UNION CHANGES
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
080322* 080322  CR2021100800003  TANA  Add B and H claim types
092602******************************************************************
00056      EJECT
00057  ENVIRONMENT DIVISION.
00058
00059  DATA DIVISION.
00060
00061  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00062  77  LCP-WS-ADDR-COMP              PIC S9(8) COMP.
00063  77  LCP-WS-ADDR-PNTR              REDEFINES LCP-WS-ADDR-COMP
00064                                    USAGE POINTER.
CIDMOD
CIDMOD 01  WS-MOD-FIELDS.
CIDMOD     05  WS-FLAG-KEY                 PIC X(8) VALUE SPACES.
CIDMOD     05  MICR-CHK-SW                 PIC X VALUE 'N'.
CIDMOD
CIDMOD     05  CSO-DRAFT-420C.
CSODJN         10  CSO-DRAFT-KEY           PIC X(19).
CIDDAN         10  FILLER                  PIC X(1162).
CIDMOD
CSODJN     05  WS-MICR-KEY                 PIC X(19) VALUE SPACES.
070104*    05  REC-LGTH                    PIC S9(4) COMP VALUE +1181.
070104*    05  REC-LGTH                    PIC S9(4) COMP VALUE +1184.
011105     05  REC-LGTH                    PIC S9(4) COMP VALUE +1254.
CIDMOD
00066  77  FILLER  PIC X(32)  VALUE '********************************'.
00067  77  FILLER  PIC X(32)  VALUE '*    EL176 WORKING STORAGE     *'.
00068  77  FILLER  PIC X(32)  VALUE '********** VMOD=2.060 **********'.
00069
00070 *                            COPY ELCSCTM.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCSCTM                             *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = C.I.C.S. COMMON SECURITY MESSAGE AREA     *
00007 *                                                                *
00008 ******************************************************************
00009  01  SECURITY-MESSAGE.
00010      12  FILLER                          PIC X(30)
00011             VALUE '** LOGIC SECURITY VIOLATION -'.
00012      12  SM-READ                         PIC X(6).
00013      12  FILLER                          PIC X(5)
00014             VALUE ' PGM='.
00015      12  SM-PGM                          PIC X(6).
00016      12  FILLER                          PIC X(5)
00017             VALUE ' OPR='.
00018      12  SM-PROCESSOR-ID                 PIC X(4).
00019      12  FILLER                          PIC X(6)
00020             VALUE ' TERM='.
00021      12  SM-TERMID                       PIC X(4).
00022      12  FILLER                          PIC XX   VALUE SPACE.
00023      12  SM-JUL-DATE                     PIC 9(5).
00024      12  FILLER                          PIC X    VALUE SPACE.
00025      12  SM-TIME                         PIC 99.99.
00026
00071
00072 *                            COPY ELCSCRTY.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCSCRTY                            *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = C.I.C.S. COMMON SECURITY DATA AREA        *
00007 *        AREA ACQUIRED BY SIGN ON PROGRAM EL125 AND ADDRESS      *
00008 *        SAVED IN PI-SECURITY-ADDRESS.                           *
00009 *                                                                *
00010 ******************************************************************
00011  01  SECURITY-CONTROL.
00012      12  SC-COMM-LENGTH               PIC S9(4) VALUE +144 COMP.
00013      12  FILLER                       PIC XX    VALUE 'SC'.
00014      12  SC-CREDIT-CODES.
00015          16  SC-CREDIT-AUTHORIZATION OCCURS 40 TIMES.
00016              20  SC-CREDIT-DISPLAY    PIC X.
00017              20  SC-CREDIT-UPDATE     PIC X.
00018      12  SC-CLAIMS-CODES.
00019          16  SC-CLAIMS-AUTHORIZATION OCCURS 30 TIMES.
00020              20  SC-CLAIMS-DISPLAY    PIC X.
00021              20  SC-CLAIMS-UPDATE     PIC X.
00073
00074      EJECT
00075  01  WS-DATE-AREA.
00076      05  SAVE-DATE           PIC X(8)    VALUE SPACES.
00077      05  SAVE-BIN-DATE       PIC XX      VALUE SPACES.
CIDMOD     05  WS-RESPONSE         PIC S9(8)   COMP.
CIDMOD         88  WS-RESP-NORMAL              VALUE +00.
CIDMOD         88  WS-RESP-NOTFND              VALUE +13.
CIDMOD
00078
00079  01  FILLER                          COMP-3.
00080      05  WS-NOT-FOUND                PIC S9      VALUE ZERO.
00081      05  WS-PRINTER-STARTED-SW       PIC S9      VALUE ZERO.
00082      05  WS-READNEXT-SW              PIC S9      VALUE ZERO.
00083      05  WS-LAST-ERROR-COUNT         PIC S9(3)   VALUE ZERO.
00084      05  WS-COMPLETED-SUCCESSFUL     PIC S9      VALUE ZERO.
00085        88  TRANSACTION-SUCCESSFUL                    VALUE +1 +2.
00086        88  CHECKS-WITHOUT-ADDRESSES                  VALUE +2.
00087
00088      05  TIME-IN                     PIC S9(7)   VALUE ZERO.
00089      05  TIME-OUT REDEFINES TIME-IN  PIC S9(3)V9(4).
00090      05  WS-HHMM  REDEFINES TIME-IN  PIC S9(5)V99.
00091
00092      05  WS-ACTIVITY-TRAILERS-BROWSE-SW PIC S9 VALUE ZERO.
00093      05  WS-CHECK-QUEUE-BROWSE-SW       PIC S9 VALUE ZERO.
00094
00095      05  WS-NOT-RELEASED-COUNT       PIC S9(5)    VALUE ZERO.
00096      05  WS-NOT-RELEASED-AMOUNT      PIC S9(9)V99 VALUE ZERO.
00097      05  WS-RELEASED-COUNT           PIC S9(5)    VALUE ZERO.
00098      05  WS-RELEASED-AMOUNT          PIC S9(9)V99 VALUE ZERO.
00099
00100      EJECT
00101  01  FILLER      COMP SYNC.
00102      05  WS-PAYMENT-NOTE-SEQ-NO      PIC S9(4)  COMP VALUE +0.
00103      05  SC-ITEM                     PIC S9(4)   VALUE +0001.
00104
00105      05  WS-TS-LENGTH                PIC S9(4)   VALUE +1158.
00106
00107      05  WS-KEY-LENGTH               PIC S9(4)   VALUE ZERO.
00108
00109      05  WS-CHECK-QUE-COUNTER        PIC S9(8)   VALUE ZERO.
00110      05  WS-CHECK-COUNTER            PIC S9(4)   VALUE +10.
00111
00112      05  WS-LAST-CONTROL-GROUP       PIC S9(8)   VALUE ZERO.
00113      05  WS-TIMES-PRINTED            PIC S9(4)   VALUE ZERO.
00114
00115      05  WS-SEQUENCE-NUMBER          PIC S9(4)   VALUE ZERO.
00116      05  WS-BEGIN-NUMBER             PIC S9(4)   VALUE ZERO.
00117
00118      05  WS-ELCHKQ-POINTER           PIC S9(8)   VALUE ZERO.
00119      05  WS-INDEX                    PIC S9(4)   VALUE ZERO.
00120
00121      EJECT
00122  01  FILLER.
00123      05  WS-ERCOMP-KEY.
00124          10  WS-ERCOMP-COMPANY-CD    PIC X.
00125          10  WS-ERCOMP-CARRIER       PIC X.
00126          10  WS-ERCOMP-GROUPING      PIC X(6).
00127          10  WS-ERCOMP-RESP-NO       PIC X(10).
00128          10  WS-ERCOMP-ACCOUNT       PIC X(10).
00129          10  WS-ERCOMP-TYPE          PIC X.
00130
00131      05  WS-WORK-DATE.
00132          10  WS-WORK-MO              PIC XX.
00133          10  FILLER                  PIC X.
00134          10  WS-WORK-DA              PIC XX.
00135          10  FILLER                  PIC X.
00136          10  WS-WORK-YR              PIC XX.
00137      05  WS-CONTROL-FILE-KEY.
00138          10  WS-CFK-COMPANY-ID       PIC X(3)    VALUE SPACES.
00139          10  WS-CFK-RECORD-TYPE      PIC X       VALUE SPACES.
00140          10  FILLER                  PIC XX      VALUE SPACES.
00141          10  WS-CFK-BENEFIT-NO                   VALUE SPACES.
00142              15  FILLER              PIC X.
00143              15  WS-CFK-CARRIER      PIC X.
00144          10  WS-CFK-SEQUENCE-NO      PIC S9(4)   VALUE ZERO COMP.
00145
00146      05  WS-ACTIVITY-TRAILERS-KEY.
00147          10  WS-ATK-COMPANY-CD       PIC X.
00148          10  WS-ATK-CARRIER          PIC X.
00149          10  WS-ATK-CLAIM-NO         PIC X(7).
00150          10  WS-ATK-CERT-NO          PIC X(11).
00151          10  WS-ATK-SEQUENCE-NO      PIC S9(4)  COMP.
00152
00153      05  WS-CHECK-QUEUE-KEY.
00154          10  WS-CQK-COMPANY-CD       PIC X.
00155          10  WS-CQK-CONTROL-NUMBER   PIC S9(8) COMP.
00156          10  WS-CQK-SEQUENCE-NUMBER  PIC S9(4) COMP.
00157
00158      05  WS-CHECK-AIX-KEY.
00159          10  WS-CQK-COMPANY-CD-A1        PIC X.
00160          10  WS-CQK-CONTROL-NUMBER-A1    PIC S9(08)  COMP.
00161          10  WS-PAYEE-CARRIER            PIC X.
00162          10  WS-PAYEE-GROUPING           PIC X(06).
00163          10  WS-PAYEE-STATE              PIC XX.
00164          10  WS-PAYEE-BENE-ACCT          PIC X(10).
00165          10  WS-CQK-SEQUENCE-NUMBER-A1   PIC S9(04)   COMP.
00166
00167      05  WS-LAST-CHECK-QUEUE-KEY     PIC X(26) VALUE LOW-VALUE.
00168      05  WS-BROWSE-LENGTH            PIC S9           COMP.
00169
00170      05  WS-CLAIM-MASTER-KEY.
00171          10  WS-CK-COMPANY-CD        PIC X.
00172          10  WS-CK-CARRIER           PIC X.
00173          10  WS-CK-CLAIM-NO          PIC X(7).
00174          10  WS-CK-CERT-NO           PIC X(11).
00175
00176      05  WS-CERTIFICATE-MASTER-KEY.
00177          10  WS-CM-COMPANY-CD        PIC X.
00178          10  WS-CM-CARRIER           PIC X.
00179          10  WS-CM-GROUPING          PIC X(6).
00180          10  WS-CM-STATE             PIC XX.
00181          10  WS-CM-ACCOUNT           PIC X(10).
00182          10  WS-CM-CERT-EFF-DT       PIC XX.
00183          10  WS-CM-CERT-NO           PIC X(11).
00184
00185      05  WS-ACCOUNT-MASTER-KEY.
00186          10  WS-AK-COMPANY-CD        PIC X.
00187          10  WS-AK-CARRIER           PIC X.
00188          10  WS-AK-GROUPING          PIC X(6).
00189          10  WS-AK-STATE             PIC XX.
00190          10  WS-AK-ACCOUNT           PIC X(10).
00191          10  WS-AK-EXPIRATION-DT     PIC XX.
00192
00193      05  WS-ACCOUNT-HOLD-RECORD      PIC X(2000).
00194      05  WS-PRODUCER-HOLD-RECORD     PIC X(2000).
00195
00196      05  WS-BENEFICIARY-KEY.
00197          10  WS-BK-COMPANY-CD        PIC X.
00198          10  WS-BK-RECORD-TYPE       PIC X.
00199          10  WS-BK-BENEFICIARY       PIC X(10).
00200
00201      05  WS-POLICY-MASTER-KEY.
00202          10  WS-PM-COMPANY-CD        PIC X.
00203          10  WS-PM-CARRIER           PIC X.
00204          10  WS-PM-GROUPING          PIC X(06).
00205          10  WS-PM-STATE             PIC XX.
00206          10  WS-PM-PRODUCER          PIC X(10).
00207          10  WS-PM-EFF-DT            PIC XX.
00208          10  WS-PM-REFERENCE-NO      PIC X(20).
00209
00210      05  WS-PRODUCER-MASTER-KEY.
00211          10  WS-PD-COMPANY-CD        PIC X.
00212          10  WS-PD-CARRIER           PIC X.
00213          10  WS-PD-GROUPING          PIC X(06).
00214          10  WS-PD-STATE             PIC XX.
00215          10  WS-PD-PRODUCER          PIC X(10).
00216          10  WS-PD-EXP-DT            PIC XX.
00217
00218      05  WS-PLAN-MASTER-KEY.
00219          10  WS-PP-COMPANY-CD        PIC X.
00220          10  WS-PP-CARRIER           PIC X.
00221          10  WS-PP-GROUPING          PIC X(06).
00222          10  WS-PP-STATE             PIC XX.
00223          10  WS-PP-PRODUCER          PIC X(10).
00224          10  WS-PP-PLAN-CODE         PIC XX.
00225          10  WS-PP-REV-NO            PIC 9(03).
00226
00227      EJECT
00228      05  WS-MAPSET-NAME              PIC X(8)  VALUE 'EL176S'.
00229      05  WS-MAP-NAME                 PIC X(8)  VALUE 'EL176A'.
00230
00231      05  FILLER REDEFINES WS-MAP-NAME.
00232          20  FILLER                  PIC XX.
00233          20  WS-MAP-NUMBER           PIC X(6).
00234
00235      05  THIS-PGM                    PIC X(8)  VALUE 'EL176'.
00236
00237      05  WS-LAST-CARRIER             PIC X     VALUE LOW-VALUES.
00238
00239      05  WS-CHECK-QUEUE-DSID         PIC X(8) VALUE 'ELCHKQ'.
00240      05  WS-CHECK-QUEUE-AIX-DSID     PIC X(8) VALUE 'ELCHKQ2'.
00241      05  WS-ENQ-COMPANY-ID           PIC X(3) VALUE ZERO.
00242      05  WS-ACTIVITY-TRAILERS-DSID   PIC X(8) VALUE 'ELTRLR'.
00243      05  WS-CONTROL-FILE-DSID        PIC X(8) VALUE 'ELCNTL'.
00244      05  WS-CLAIM-MASTER-DSID        PIC X(8) VALUE 'ELMSTR'.
00245      05  WS-CERTIFICATE-MASTER-DSID  PIC X(8) VALUE 'ELCERT'.
00246      05  WS-ACCOUNT-MASTER-DSID      PIC X(8) VALUE 'ERACCT'.
00247      05  WS-BENEFICIARY-MASTER-DSID  PIC X(8) VALUE 'ELBENE'.
00248      05  WS-POLICY-MASTER-DSID       PIC X(8) VALUE 'MPPLCY'.
00249      05  WS-PRODUCER-MASTER-DSID     PIC X(8) VALUE 'MPPROD'.
00250      05  WS-PLAN-MASTER-DSID         PIC X(8) VALUE 'MPPLAN'.
00251
00252      05  WS-LOW-VALUES               PIC X VALUE LOW-VALUES.
00253      05  WS-SPACES                   PIC X VALUE SPACES.
00254
00255      05  WS-CHECK-WRITER-DATE        PIC XX VALUE LOW-VALUES.
00256
00257      05  WS-TRANS-ID                 PIC X(4)    VALUE 'EX46'.
00258      05  WS-CHECK-WRITER-TRANS-ID.
00259          10  WS-CHECK-TRANSID-1      PIC X       VALUE 'Q'.
00260          10  WS-CHECK-TRANSID-2      PIC XXX     VALUE SPACES.
00261
00262      05  WS-BENEFIT-NO               PIC XX      VALUE SPACES.
00263
00264      05  WS-AUTO-PAY-SW              PIC X       VALUE 'N'.
00265
00266      05  WS-EMPROD-GETMAIN-SW        PIC X       VALUE 'Y'.
00267      05  WS-ERACCT-GETMAIN-SW        PIC X       VALUE 'Y'.
00268      05  WS-ELCHKQ-GETMAIN-SW        PIC X       VALUE 'Y'.
00269
00270      05  FILE-SWITCH                 PIC X(04)   VALUE SPACES.
00271
00272      05  WS-AIG-CREDITOR-NAME        PIC X(30)   VALUE SPACES.
00273      05  WS-AIG-INS-CO-NAME          PIC X(30)   VALUE SPACES.
00274
00275      05  WS-DMD-FLAG-KEY             PIC X(8)    VALUE SPACES.
00276      05  WS-DMD-MICR-FLAG            PIC X(18)   VALUE SPACES.
00277
CSODJN     05  WS-DMD-DRFT-KEY             PIC X(19)   VALUE SPACES.
CSODJN     05  WS-DMD-MICR-DRFT            PIC X(705)  VALUE SPACES.
00280
00281      05  WS-DMD-CNTL-GRP-COUNT       PIC S9  COMP  VALUE ZEROS.
00282
00283      05  WS-COV-TYPE                 PIC X.
00284      05  WS-KIND.
00285          10  WS-RETRO-DAYS           PIC 99.
00286          10  WS-RETRO-ELIM           PIC X.
00287
00288      05  WS-TEXT-MESSAGE-LENGTH      PIC S9(4)   VALUE +70
00289                                      COMP SYNC.
00290
00291      05  WS-TEXT-MESSAGE             PIC X(70)   VALUE SPACES.
00292
00293      05  WS-TEMP-STORAGE-ITEM        PIC S9(4)   VALUE ZERO
00294                                      COMP SYNC.
00295
00296      05  WS-TEMP-STORAGE-KEY.
00297          10  WS-TSK-TERM-ID          PIC X(4)    VALUE SPACES.
00298          10  WS-TSK-TIME             PIC S9(7)   VALUE ZERO
00299                                      COMP-3.
00300
00301      05  WS-MINUTES                  PIC S99     VALUE ZERO.
00302
00303      05  WS-GREATEST-CHECK-NUMBER    PIC 9(7)    VALUE ZERO.
00304
00305      05  WS-GREATEST-CHECK-NUMBER-X  REDEFINES
00306          WS-GREATEST-CHECK-NUMBER    PIC X(7).
00307
00308      05  WS-CHECK-NUMBER             PIC 9(7)    VALUE ZERO.
00309
00310      05  WS-CHECK-NUMBER-X REDEFINES
00311          WS-CHECK-NUMBER             PIC X(7).
00312
00313      05  WS-ACKNO                    PIC 9(7)    VALUE ZERO.
00314
00315      05  WS-ACKNO-X REDEFINES
00316          WS-ACKNO                    PIC X(7).
00317
00318      05  WS-PAYMENT-TYPE             PIC X       VALUE ZERO.
00319      05  WS-PAYEE-CODE.
00320          10  WS-PAYEE-CD             PIC X.
00321          10  WS-PAYEE-SEQ            PIC X.
00322          10  WS-PAYEE-SEQ-NUM REDEFINES
00323              WS-PAYEE-SEQ            PIC 9.
00324
00325      05  WS-INIT-CONTROL-GROUP.
00326          10  FILLER                  PIC X    VALUE SPACES.
00327          10  FILLER                  PIC X(7) VALUE LOW-VALUES.
00328
00329      05  WS-SSN.
00330          10  WS-SSN-STATE            PIC XX.
00331          10  WS-SSN-ACCOUNT          PIC X(6).
00332          10  WS-SSN-LN3              PIC X(3).
00333
00334      05  WS-MEMBER-NUMBER.
00335          10  WS-MEMBER-STATE         PIC XX.
00336          10  WS-MEMBER-ACCOUNT       PIC X(6).
00337          10  WS-MEMBER-LN4           PIC X(4).
00338
00339      05  WS-GROUP.
00340          10  WS-GRP-1-3              PIC XXX.
00341          10  WS-GRP-4-6              PIC XXX.
00342
00343      05  WS-ZIP-UNPACKED                 PIC 9(9)  VALUE ZEROS.
00344
00345      05  WS-CARRIER-ADDRESS-DATA.
00346          10  WS-CARRIER-MAIL-TO-NAME     PIC X(30).
00347          10  WS-CARRIER-IN-CARE-OF       PIC X(30).
00348          10  WS-CARRIER-ADDRESS-LINE-1   PIC X(30).
00349          10  WS-CARRIER-ADDRESS-LINE-2   PIC X(30).
00350          10  WS-CARRIER-CITY-STATE       PIC X(30).
00351          10  WS-CARRIER-ZIP-CODE         PIC X(9).
00352          10  WS-CARRIER-PHONE-NO         PIC 9(11)     COMP-3.
00353
00354      05  WS-OLD-CHECK-QUEUE-RECORD   PIC X(100)  VALUE SPACES.
00355      05  WS-NEW-CHECK-QUEUE-RECORD   PIC X(100)  VALUE SPACES.
00356
00357      05  WS-WORK-PHONE               PIC X(10)   VALUE ZEROS.
00358      05  WS-NUMERIC-PHONE REDEFINES WS-WORK-PHONE
00359                                      PIC 9(10).
00360
00361      EJECT
00362
00363  01  ERROR-MESSAGES.
00364      12  ER-0002                 PIC X(4)  VALUE '0002'.
00365      12  ER-0004                 PIC X(4)  VALUE '0004'.
00366      12  ER-0008                 PIC X(4)  VALUE '0008'.
00367      12  ER-0029                 PIC X(4)  VALUE '0029'.
00368      12  ER-0042                 PIC X(4)  VALUE '0042'.
00369      12  ER-0070                 PIC X(4)  VALUE '0070'.
00370      12  ER-0154                 PIC X(4)  VALUE '0154'.
00371      12  ER-0168                 PIC X(4)  VALUE '0168'.
00372      12  ER-0169                 PIC X(4)  VALUE '0169'.
00373      12  ER-0172                 PIC X(4)  VALUE '0172'.
00374      12  ER-0330                 PIC X(4)  VALUE '0330'.
00375      12  ER-0361                 PIC X(4)  VALUE '0361'.
00376      12  ER-0362                 PIC X(4)  VALUE '0362'.
00377      12  ER-0364                 PIC X(4)  VALUE '0364'.
00378      12  ER-0365                 PIC X(4)  VALUE '0365'.
00379      12  ER-0366                 PIC X(4)  VALUE '0366'.
00380      12  ER-0367                 PIC X(4)  VALUE '0367'.
00381      12  ER-0368                 PIC X(4)  VALUE '0368'.
00382      12  ER-0369                 PIC X(4)  VALUE '0369'.
00383      12  ER-0370                 PIC X(4)  VALUE '0370'.
00384      12  ER-0371                 PIC X(4)  VALUE '0371'.
00385      12  ER-0379                 PIC X(4)  VALUE '0379'.
00386      12  ER-0380                 PIC X(4)  VALUE '0380'.
00387      12  ER-0381                 PIC X(4)  VALUE '0381'.
00388      12  ER-0382                 PIC X(4)  VALUE '0382'.
00389      12  ER-0383                 PIC X(4)  VALUE '0383'.
00390      12  ER-0385                 PIC X(4)  VALUE '0385'.
00391      12  ER-0387                 PIC X(4)  VALUE '0387'.
00392      12  ER-0389                 PIC X(4)  VALUE '0389'.
00393      12  ER-0390                 PIC X(4)  VALUE '0390'.
00394      12  ER-0391                 PIC X(4)  VALUE '0391'.
00395      12  ER-0392                 PIC X(4)  VALUE '0392'.
00396      12  ER-0393                 PIC X(4)  VALUE '0393'.
00397      12  ER-0394                 PIC X(4)  VALUE '0394'.
00398      12  ER-0395                 PIC X(4)  VALUE '0395'.
00399      12  ER-0490                 PIC X(4)  VALUE '0490'.
00400      12  ER-2055                 PIC X(4)  VALUE '2055'.
00401      12  ER-2370                 PIC X(4)  VALUE '2370'.
00402      12  ER-2936                 PIC X(4)  VALUE '2936'.
00403      12  ER-3027                 PIC X(4)  VALUE '3027'.
00404      12  ER-3028                 PIC X(4)  VALUE '3028'.
00405      12  ER-3130                 PIC X(4)  VALUE '3130'.
00406      12  ER-3776                 PIC X(4)  VALUE '3776'.
00407      12  ER-7675                 PIC X(4)  VALUE '7675'.
00408      12  ER-9808                 PIC X(4)  VALUE '9808'.
00409      12  ER-9883                 PIC X(4)  VALUE '9883'.
00410      12  ER-9886                 PIC X(4)  VALUE '9886'.
00411
00412      EJECT
00413 *    COPY ELCINTF.
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
011812*                   C H A N G E   L O G
011812*
011812* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
011812*-----------------------------------------------------------------
011812*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
011812* EFFECTIVE    NUMBER
011812*-----------------------------------------------------------------
011812* 011812    2011022800001  AJRA  ADD CSR IND TO USER SECURITY
011812******************************************************************
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
011812
011812     12  PI-PROCESSOR-CSR-IND            PIC X.
011812         88  PI-PROCESSOR-IS-CSR             VALUE 'Y' 'S'.
011812         88  PI-PROCESSOR-IS-CSR-SUPER       VALUE 'S'.
011812
011812     12  FILLER                          PIC X(3).
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
00414
00415 *    COPY ELC176PI.
00001 *****************************************************************
00002 *                                                               *
00002 *                                                               *
00003 *                            ELC176PI.                          *
00004 *                            VMOD=2.005                         *
00005 *****************************************************************.
00006
00007      12  FILLER                      REDEFINES
00008          PI-PROGRAM-WORK-AREA.
00009
00010          16  PI-TEMP-STORAGE-KEY.
00011              20  PI-TSK-TERM-ID      PIC X(4).
00012              20  PI-TSK-TIME         PIC S9(7)     COMP-3.
00013
00014          16  PI-PROCESSING-SW        PIC S9        COMP-3.
00015
00016          16  PI-NUMBER-OF-ALIGNMENT-CHECKS
00017                                      PIC S9        COMP-3.
00018          16  PI-ALIGNMENT-CONTROL-GROUP
00019                                      PIC S9(8)     COMP-3.
00020          16  PI-ALIGNMENT-SEQUENCE-NO
00021                                      PIC S9(4)     COMP-3.
00022          16  PI-NUMBER-OF-CONTROL-GROUPS
00023                                      PIC S9(4)     COMP-3.
00024
00025          16  PI-CONTROL-GROUPS                     COMP
00026              OCCURS 4 TIMES          INDEXED BY PI-INDEX.
00027              20  PI-CONTROL-GROUP    PIC S9(8).
00028              20  PI-HIGH-SEQUENCE    PIC S9(4).
00029
00030          16  PI-CHECK-PRINTER-ID     PIC X(4).
00031
00032          16  PI-PRINTER-STARTED-SW   PIC S9        COMP-3.
00033
00034          16  PI-ASSIGN-CHECK-NUMBERS PIC X.
00035
00036          16  PI-COMPANY-ADDRESS.
00037              20  PI-COMPANY-NAME             PIC X(30).
00038              20  PI-COMPANY-ADDRESS-LINE1    PIC X(30).
00039              20  PI-COMPANY-ADDRESS-LINE2    PIC X(30).
00040              20  PI-COMPANY-ADDRESS-LINE3    PIC X(30).
00041              20  PI-COMPANY-CITY-ST          PIC X(30).
00042              20  PI-COMPANY-ZIP.
00043                  24  PI-COMPANY-ZIP-CODE.
00044                      28  PI-COMPANY-ZIP-1ST  PIC X.
00045                          88  PI-COMPANY-CAN-POST-CODE
00046                                              VALUE 'A' THRU 'Z'.
00047                      28  FILLER              PIC X(4).
00048                  24  PI-COMPANY-ZIP-PLUS4    PIC X(4).
00049              20  PI-CO-CANADIAN-POSTAL-CODE
00050                      REDEFINES PI-COMPANY-ZIP.
00051                  24  PI-CO-CAN-POSTAL-1      PIC XXX.
00052                  24  PI-CO-CAN-POSTAL-2      PIC XXX.
00053                  24  FILLER                  PIC XXX.
00054              20  PI-COMPANY-PHONE-NUMBER     PIC S9(11) COMP-3.
00055
00056          16  PI-MONTH-END-SAVE               PIC XX.
00057
00058          16  PI-VALID-RCD-SW                 PIC X.
00059
00060          16  FILLER                          PIC X(419).
00061
00416             16  FILLER PIC XX.
00417
00418      EJECT
00419 *    COPY ELCCPA.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCCPA.                             *
00004 *                            VMOD=2.013                          *
00005 *                                                                *
00006 *   DESCRIPTION:  DATA TO BE PASSED TO CHECK WRITER ROUTINE.     *
00007 ******************************************************************
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
072110* 072110    2009122800001  AJRA  CHANGE COMMENT,COMMENT-2 FROM 40
072110*                                SEPARATE CITY AND STATE
013017* 013017  CR2016053100001  PEMA  ACH PROCESSING
      ******************************************************************
00008
00009  01  CHECK-PASS-AREA.
00010      12  CPA-ALIGNMENT               PIC S9(3)    COMP-3.
00011      12  CPA-CARRIER                 PIC X.
00012      12  CPA-GROUP                   PIC X(6).
00013      12  CPA-ACCOUNT                 PIC X(10).
00014      12  CPA-STATE                   PIC XX.
00015      12  CPA-CLAIM-NO                PIC X(7).
00016      12  CPA-CERT-NO                 PIC X(11).
00017      12  CPA-CERT-EFF-DT             PIC XX.
00018      12  CPA-CLAIM-TYPE              PIC X.
00019      12  CPA-PAYEE-TYPE-CD           PIC X.
00020      12  CPA-IND-GRP-TYPE            PIC X.
00021          88  CPA-INDIVIDUAL                     VALUE 'I'.
00022          88  CPA-GROUP-POLICY                   VALUE 'G'.
00023      12  CPA-PAYEE-NAME              PIC X(30).
00024      12  CPA-PAYEE-ADDRESS-LINE1     PIC X(30).
00025      12  CPA-PAYEE-ADDRESS-LINE2     PIC X(30).
00026      12  CPA-PAYEE-ADDRESS-LINE3     PIC X(30).
072110     12  CPA-PAYEE-CITY-STATE.
072110         16  CPA-PAYEE-CITY          PIC X(28).
072110         16  CPA-PAYEE-STATE         PIC X(2).
00028      12  CPA-PAYEE-ZIP.
00029          16  CPA-PAYEE-ZIP-CODE.
00030              24  CPA-ZIP-1ST-PAYEE   PIC X.
00031                  88  CPA-CAN-POST-CODE-PAYEE  VALUE 'A' THRU 'Z'.
00032              24  FILLER              PIC X(4).
00033          16  CPA-PAYEE-ZIP-PLUS4     PIC X(4).
00034      12  CPA-CANADIAN-POSTAL-CODE-PAYEE REDEFINES CPA-PAYEE-ZIP.
00035          16  CPA-CAN-POSTAL-1-PAYEE  PIC XXX.
00036          16  CPA-CAN-POSTAL-2-PAYEE  PIC XXX.
00037          16  FILLER                  PIC XXX.
00038      12  CPA-INSURED-NAME            PIC X(30).
00039      12  CPA-INSURED-ADDRESS-LINE1   PIC X(30).
00040      12  CPA-INSURED-ADDRESS-LINE2   PIC X(30).
00041      12  CPA-INSURED-ADDRESS-LINE3   PIC X(30).
072110     12  CPA-INSURED-CITY-STATE.
072110         16  CPA-INSURED-CITY        PIC X(28).
072110         16  CPA-INSURED-STATE       PIC X(2).
00043      12  CPA-INSURED-ZIP.
00044          16  CPA-INSURED-ZIP-CODE.
00045              24  CPA-ZIP-1ST-INS     PIC X.
00046                  88  CPA-CAN-POST-CODE-INS    VALUE 'A' THRU 'Z'.
00047              24  FILLER              PIC X(4).
00048          16  CPA-INSURED-ZIP-PLUS4   PIC X(4).
00049      12  CPA-CANADIAN-POSTAL-CODE-INS REDEFINES CPA-INSURED-ZIP.
00050          16  CPA-CAN-POSTAL-1-INS    PIC XXX.
00051          16  CPA-CAN-POSTAL-2-INS    PIC XXX.
00052          16  FILLER                  PIC XXX.
00053      12  CPA-INSURED-AGE             PIC 99.
00054      12  CPA-PAYMENT-TYPE            PIC X.
00055      12  CPA-PAYMENT-BY              PIC X(4).
00056      12  CPA-CHECK-DATE              PIC X(2).
00057      12  CPA-CHECK-NUMBER            PIC X(7).
00058      12  CPA-AMOUNT-PAID             PIC S9(7)V99    COMP-3.
00059      12  CPA-AMOUNT-PAID-TO-DATE     PIC S9(7)V99    COMP-3.
00060      12  CPA-DAYS-PAID               PIC S9(5)       COMP-3.
00061      12  CPA-DAILY-RATE              PIC S9(3)V99    COMP-3.
00062      12  CPA-ELIMINATION-DAYS        PIC S9(3)       COMP-3.
00063      12  CPA-CLAIM-CODE              PIC X.
00064      12  CPA-PAY-CODE                PIC X.
00065      12  CPA-INCURRED-DT             PIC XX.
00066      12  CPA-REPORTED-DT             PIC XX.
00067      12  CPA-PAID-THRU-DT            PIC XX.
00068      12  CPA-PAID-FROM-DT            PIC XX.
00069      12  CPA-PAID-DT                 PIC XX.
00070
00071      12  CPA-ACCOUNT-NAME            PIC X(30).
00072      12  CPA-ACCOUNT-IN-CARE-OF      PIC X(30).
00073      12  CPA-ACCOUNT-ADDRESS-LINE1   PIC X(30).
00074      12  CPA-ACCOUNT-ADDRESS-LINE2   PIC X(30).
072110     12  CPA-ACCOUNT-CITY-ST.
072110         16  CPA-ACCOUNT-CITY        PIC X(28).
072110         16  CPA-ACCOUNT-STATE       PIC X(2).
00076      12  CPA-ACCOUNT-ZIP-CODE.
00077          16  CPA-ACCOUNT-ZIP.
00078              24  CPA-ZIP-1ST-ACCT    PIC X.
00079                  88  CPA-CAN-POST-CODE-ACCT VALUE 'A' THRU 'Z'.
00080              24  FILLER              PIC X(4).
00081          16  CPA-ACCOUNT-ZIP-PLUS4   PIC X(4).
00082      12  CPA-CANADIAN-POSTAL-CODE-ACCT
00083              REDEFINES CPA-ACCOUNT-ZIP-CODE.
00084          16  CPA-CAN-POSTAL-1-ACCT   PIC XXX.
00085          16  CPA-CAN-POSTAL-2-ACCT   PIC XXX.
00086          16  FILLER                  PIC XXX.
00087      12  CPA-ACCOUNT-PHONE-NO        PIC S9(11)     COMP-3.
00088
00089      12  CPA-SOC-SEC-NO              PIC X(11).
00090      12  CPA-MEMBER-NUMBER           PIC X(12).
00091      12  CPA-LOAN-NUMBER             PIC X(8).
00092
00093      12  CPA-BENEFIT-TYPE            PIC X.
00094
00095      12  CPA-NO-OF-PMTS-MADE         PIC S9(3)       COMP-3.
00096
00097      12  CPA-EXPIRE-DT               PIC XX.
00098
00099      12  CPA-MONTHLY-BENEFIT         PIC S9(7)V99    COMP-3.
00100
072110     12  CPA-COMMENT                 PIC X(60).
00102
00103      12  CPA-CLAIM-STATUS            PIC X.
00104 *      88  CLAIM-IS-OPEN                             VALUE 'O'.
00105 *      88  CLAIM-IS-CLOSED                           VALUE 'C'.
00106
00107      12  CPA-LAST-CLOSE-REASON       PIC X.
00108 *      88  FINAL-PAID                                VALUE '1'.
00109 *      88  CLAIM-DENIED                              VALUE '2'.
00110 *      88  AUTO-CLOSE                                VALUE '3'.
00111
00112      12  CPA-INSURED-ADDR-TRLR-NAME  PIC X(30).
00113
00114      12  CPA-EXAMINER                PIC X(4).
00115      12  CPA-EXPENSE-TYPE            PIC X.
00116
00117      12  CPA-CARRIER-ADDRESS-DATA.
00118          16  CPA-CARRIER-NAME            PIC X(30).
00119          16  CPA-CARRIER-ADDRESS-LINE1   PIC X(30).
00120          16  CPA-CARRIER-ADDRESS-LINE2   PIC X(30).
00121          16  CPA-CARRIER-ADDRESS-LINE3   PIC X(30).
072110         16  CPA-CARRIER-CITY-STATE.
072110             24  CPA-CARRIER-CITY        PIC X(28).
072110             24  CPA-CARRIER-STATE       PIC X(2).
00123          16  CPA-CARRIER-ZIP.
00124              24  CPA-CARRIER-ZIP-CODE.
00125                  28  CPA-ZIP-1ST-CARRIER PIC X.
00126                      88  CPA-CAN-POST-CODE-CARR
00127                                          VALUE 'A' THRU 'Z'.
00128                  28  FILLER              PIC X(4).
00129              24  CPA-CARRIER-ZIP-PLUS4   PIC X(4).
00130          16  CPA-CANADIAN-POSTAL-CODE-CARR
00131                            REDEFINES CPA-CARRIER-ZIP.
00132              24  CPA-CAN-POSTAL-1-CARR   PIC XXX.
00133              24  CPA-CAN-POSTAL-2-CARR   PIC XXX.
00134              24  FILLER                  PIC XXX.
00135
00136      12  CPA-PAYMENT-ORIGIN              PIC X.
00137      12  CPA-BENEFIT-CD                  PIC XX.
072110     12  CPA-COMMENT-2                   PIC X(60).
00139
00140      12  CPA-NOTIFY-ADDRESS-DATA.
00141          16  CPA-NOTIFY-NAME             PIC X(30).
00142          16  CPA-NOTIFY-ADDRESS-LINE1    PIC X(30).
00143          16  CPA-NOTIFY-ADDRESS-LINE2    PIC X(30).
00144          16  CPA-NOTIFY-ADDRESS-LINE3    PIC X(30).
072110         16  CPA-NOTIFY-CITY-STATE.
072110             20  CPA-NOTIFY-CITY         PIC X(28).
072110             20  CPA-NOTIFY-STATE        PIC X(2).
00146          16  CPA-NOTIFY-ZIP.
00147              20  CPA-NOTIFY-ZIP-CODE     PIC X(5).
00148              20  CPA-NOTIFY-ZIP-PLUS4    PIC X(4).
00149      12  CPA-COVERAGE-TYPE               PIC X.
00150          88  CPA-LEVEL-COV                 VALUE 'L'.
00151          88  CPA-REDUCE-COV                VALUE 'R'.
00152          88  CPA-PROP-COV                  VALUE 'P'.
00153          88  CPA-DISAB-COV                 VALUE 'A'.
00154
00155      12  CPA-AUTO-PAY-END-DT             PIC X(02).
00156      12  CPA-COMPANY-NAME                PIC X(30).
00157      12  CPA-TOTAL-BENEFIT               PIC S9(09)V99  COMP-3.
00158      12  CPA-FORM-CTL-SEQ-NO             PIC S9(4)      COMP.
00159      12  CPA-BENEFICIARY                 PIC X(25).
00160      12  CPA-REFERENCE-NO.
013017         16  CPA-REFERENCE-PRIME         PIC X(17).
00162          16  CPA-REFERENCE-SFX           PIC X(02).
013017     12  CPA-ACH-PAYMENT                 PIC X.
00163      12  CPA-CV-LOAN-NUMBER              PIC X(20).
00164      12  CPA-CREDIT-CARD-NO REDEFINES
00165          CPA-CV-LOAN-NUMBER              PIC X(20).
00420
00421      EJECT
00422 *    COPY ELCNWA.
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
00423
00424 *    COPY EL176S.
       01  EL176AI.
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
       01  EL176AO REDEFINES EL176AI.
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
00425
00426  01  FILLER REDEFINES EL176AI.
00427      05  FILLER                      PIC X(35).
00428
00429      05  FILLER                      OCCURS 4 TIMES
00430                                      INDEXED BY EL176A-INDEX.
00431
00432          10  EL176A-CONTROL-GROUP-LENGTH   PIC S9(4)
00433                                            COMP.
00434          10  EL176A-CONTROL-GROUP-ATTRB    PIC X.
00435          10  EL176A-CONTROL-GROUP          PIC 9(7).
00436          10  EL176A-CONTROL-GROUP-X        REDEFINES
00437              EL176A-CONTROL-GROUP          PIC X(7).
00438
00439      EJECT
00440 *    COPY ELCJPFX.
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
           12  jp-date                     pic s9(5) comp-3.
           12  jp-time                     pic s9(7) comp-3.
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
00441           PIC X(750).
00442
00443      EJECT
00444 *    COPY ELCEMIB.
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
           12  emi-claim-no                pic x(7).
           12  emi-claim-type              pic x(6).
00070      12  FILLER                      PIC X(124)  VALUE SPACES.
00071      12  EMI-DATE-FIELD              PIC X(06)   VALUE SPACES.
00072      12  EMI-CLIENT-ID               PIC X(3)    VALUE SPACES.
00073      12  EMI-LIFE-OVERRIDE-L6        PIC X(6).
00074      12  EMI-AH-OVERRIDE-L6          PIC X(6).
00445
00446      EJECT
00447 *    COPY ELCDATE.
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
00448      EJECT
00449 *    COPY ELCLOGOF.
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
00450
00451      EJECT
00452 *    COPY ELCATTR.
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
00453
00454      EJECT
00455 *    COPY ELCAID.
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
00456
00457  01  FILLER REDEFINES DFHAID.
00458      05  FILLER                      PIC X(8).
00459      05  PF-VALUES                   PIC X
00460          OCCURS 24 TIMES.
00461
      ****************************************************************
      *                                                               
      * Copyright (c) 2007-2013 Dell Inc.                             
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
         02  DFHEIV29              PIC S9(9) COMP SYNC.               
         02  DFHEIV30              PIC S9(9) COMP SYNC.               
         02  DFHEIV31              PIC S9(9) COMP SYNC.               
         02  DFHEIV32              PIC S9(4) COMP SYNC.               
         02  DFHEIV33              PIC S9(4) COMP SYNC.               
         02  DFHEIV34              PIC S9(4) COMP SYNC.               
         02  DFHEIV35              PIC S9(4) COMP SYNC.               
         02  DFHEIV97              PIC S9(7) COMP-3 VALUE ZERO.       
         02  DFHEIV98              PIC S9(4) COMP SYNC VALUE ZERO.    
         02  FILLER                PIC X(02).                         
         02  DFHEIV99              PIC X(08) VALUE SPACE.             
         02  DFHEIVL0              PIC X(48) VALUE SPACE.             
         02  DFHEIVL1              PIC X(48) VALUE SPACE.             
         02  DFHEIVL2              PIC X(48) VALUE SPACE.             
         02  DFHEIVL3              PIC X(48) VALUE SPACE.             
         02  DFHEIVL4              PIC X(255) VALUE SPACE.            
         02  DFHEIVL5              PIC X(255) VALUE SPACE.            
       LINKAGE  SECTION.
      *****************************************************************
      *                                                               *
      * Copyright (c) 2007-2013 Dell Inc.                             *
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
           02  eibresp          pic s9(8) comp.
           02  eibresp2         pic s9(8) comp.
           02  dfheigdj         pic s9(4) comp.
           02  dfheigdk         pic s9(4) comp.
00463
00464  01  DFHCOMMAREA                     PIC X(1024).
00465
00466      EJECT
00467 *    COPY ELCCHKQ.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCCHKQ.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.007                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = CHECK QUE FILE                            *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 100  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELCHKQ                         RKP=2,LEN=7    *
00013 *       ALTERNATE PATH1 = ELCHKQ2 (BY PAYEE)      RKP=9,LEN=26   *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00017 ******************************************************************
00018  01  CHECK-QUE.
00019      12  CQ-RECORD-ID                PIC XX.
00020          88  VALID-CQ-ID         VALUE 'CQ'.
00021
00022      12  CQ-CONTROL-PRIMARY.
00023          16  CQ-COMPANY-CD           PIC X.
00024          16  CQ-CONTROL-NUMBER       PIC S9(8)       COMP.
00025          16  CQ-SEQUENCE-NUMBER      PIC S9(4)       COMP.
00026
00027      12  CQ-CONTROL-BY-PAYEE.
DJNA           16  CQ-CONTROL-BY-NUMBER.
DJNA               20  CQ-COMPANY-CD-A1     PIC X.
DJNA               20  CQ-CONTROL-NUMBER-A1 PIC S9(8)      COMP.
00030          16  CQ-PAYEE-CARRIER        PIC X.
00031          16  CQ-PAYEE-GROUPING       PIC X(6).
00032          16  CQ-PAYEE-STATE          PIC XX.
00033          16  CQ-PAYEE-BENE-ACCT      PIC X(10).
00034          16  CQ-SEQUENCE-NUMBER-A1   PIC S9(4)       COMP.
00035
00036      12  CQ-DMD-CONTROL  REDEFINES  CQ-CONTROL-BY-PAYEE.
00037          16  CQ-DMD-COMPANY-CD-A2    PIC X.
00038          16  CQ-DMD-PAYEE-TYPE-A2    PIC X.
00039          16  CQ-DMD-BENE-CODE-A2     PIC X(10).
00040          16  CQ-DMD-CLAIM-NO-A2      PIC X(7).
00041          16  CQ-DMD-TIME-SEQ-A2      PIC S9(7)       COMP.
00042          16  FILLER                  PIC X(3).
00043
00044      12  CQ-ENTRY-TYPE               PIC X.
00045              88  CHECK-ON-QUE           VALUE 'Q'.
00046              88  ALIGNMENT-CHECK        VALUE 'A'.
00047              88  SPOILED-CHECK          VALUE 'S'.
00048              88  PAYMENT-ABORTED        VALUE 'X'.
00049
00050      12  CQ-CLAIM-MAST-CNTL.
00051          16  CQ-CARRIER              PIC X.
00052          16  CQ-CLAIM-NO             PIC X(7).
00053          16  CQ-CERT-NO.
00054              20  CQ-CERT-PRIME       PIC X(10).
00055              20  CQ-CERT-SFX         PIC X.
00056          16  CQ-CLAIM-TYPE           PIC X.
00057              88  CQ-LIFE-CLAIM          VALUE 'L'.
00058              88  CQ-AH-CLAIM            VALUE 'A'.
00059          16  CQ-CLAIM-SUB-TYPE       PIC X.
00060              88  CQ-FIXED-COVERAGE      VALUE '1'.
00061              88  CQ-O-B-COVERAGE        VALUE '2'.
00062              88  CQ-OPEN-END-COVERAGE   VALUE '3'.
00063
00064      12  CQ-PMT-TRLR-SEQUENCE        PIC S9(4)       COMP.
00065      12  CQ-CHECK-NUMBER             PIC X(7).
00066      12  CQ-CHECK-AMOUNT             PIC S9(7)V99    COMP-3.
00067      12  CQ-PAYMENT-TYPE             PIC X.
00068              88  CQ-PARTIAL-PAYMENT        VALUE '1'.
00069              88  CQ-FINAL-PAYMENT          VALUE '2'.
00070              88  CQ-LUMP-SUM-PAYMENT       VALUE '3'.
00071              88  CQ-ADDITIONAL-PAYMENT     VALUE '4'.
00072              88  CQ-CHARGEABLE-EXPENSE     VALUE '5'.
00073              88  CQ-NON-CHARGEABLE-EXPENSE VALUE '6'.
00074              88  CQ-LIFE-PREMIUM-REFUND    VALUE '7'.
00075              88  CQ-AH-PREMIUM-REFUND      VALUE '8'.
00076      12  CQ-VOID-INDICATOR           PIC X.
00077              88  CHECK-IS-STOPPED          VALUE 'S'.
00078              88  CHECK-IS-VOID             VALUE 'V'.
00079      12  CQ-TIMES-PRINTED            PIC S9(4)       COMP.
00080      12  CQ-PRINT-AT-HHMM            PIC S9(4)       COMP.
00081      12  CQ-CHECK-BY-USER            PIC X(4).
00082      12  CQ-PRE-NUMBERING-SW         PIC X.
00083        88  CHECKS-WERE-NOT-PRE-NUMBERED    VALUE SPACE.
00084        88  CHECKS-WERE-PRE-NUMBERED        VALUE '1'.
00085
00086      12  CQ-CHECK-WRITTEN-DT         PIC XX.
00087      12  CQ-LAST-UPDATED-BY          PIC S9(4)       COMP.
00088      12  CQ-LEDGER-FLAG              PIC X(01).
00089      12  CQ-VOID-AFTER-LEDGER        PIC X(01).
00090      12  CQ-LAST-UPDATED-DT          PIC XX.
00091      12  CQ-LAST-UPDATED-HHMMSS      PIC S9(6)       COMP-3.
00092      12  CQ-APPLIED-TO-RCON-DT       PIC XX.
00093
00094      12  FILLER                      PIC X(04).
00095
00096 ******************************************************************
00468
00469      EJECT
00470 *    COPY ELCCNTL.
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
011812* 011812    2011022800001  AJRA  ADD CSR IND TO USER SECURITY
012913* 012913    2012092400007  AJRA  ADD CAUSAL STATE IND
032813* 032813    2011013100001  AJRA  ADD CLAIM REAUDIT FIELDS
091813* 091813    2013082900001  AJRA  ADD APPROVAL LEVEL 5
051414* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
102717* 102717  CR2017062000003  PEMA  COMM CAP CHANGES
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
032813
032813         16  CF-CO-REAUDIT-INTERVAL        PIC S9(5)   COMP-3.
031808
091813         16  CF-APPROV-LEV-5.
091813             20  CF-LIFE-PAY-APP-LEVEL-5    PIC S9(7)   COMP-3.
091813             20  CF-AH-PAY-APP-LEVEL-5      PIC S9(7)   COMP-3.
091813             20  CF-AH-APP-DAY-LEVEL-5      PIC S9(5)   COMP-3.
091813
091813         16  FILLER                         PIC X(68).
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
091813             88  APPROVAL-LEVEL-5                   VALUE '5'.
00492
00493          16  CF-PROC-MAX-EXP-PMT            PIC S9(7)V99  COMP-3.
00494
00495          16  CF-LANGUAGE-TYPE                   PIC X.
00496              88  CF-LANG-IS-ENG                     VALUE 'E'.
00497              88  CF-LANG-IS-FR                      VALUE 'F'.
011812
011812         16  CF-CSR-IND                         PIC X.
011812         16  FILLER                             PIC X(239).
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
                   20  cf-st-extra-periods        pic 9.
00607 *            20  FILLER                     PIC X.
00608
00609          16  CF-ST-COMMISSION-CAPS.
00610              20  CF-ST-COMM-CAP-SL          PIC S9V9(4) COMP-3.
00611              20  CF-ST-COMM-CAP-JL          PIC S9V9(4) COMP-3.
00612              20  CF-ST-COMM-CAP-SA          PIC S9V9(4) COMP-3.
00613              20  CF-ST-COMM-CAP-JA          PIC S9V9(4) COMP-3.
00614          16  CF-COMM-CAP-LIMIT-TO           PIC X.
00615                  88  ST-LIMIT-TO-ACCOUNT        VALUE 'A'.
102717                 88  ST-LIMIT-TO-GA             VALUE 'G'.
102717                 88  ST-LIMIT-TO-BOTH           VALUE 'B'.
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
012913         16  CF-ST-CAUSAL-STATE             PIC X.
022415         16  CF-ST-EXTRA-INTEREST-PERIODS   PIC 9.
022415         16  CF-ST-EXTRA-PAYMENTS           PIC 9.
040915         16  CF-ST-AGENT-SIG-EDIT           PIC X.
040915             88  CF-ST-EDIT-FOR-SIG           VALUE 'Y'.
070115         16  CF-ST-NET-ONLY-STATE           PIC X.
070115             88  CF-ST-IS-NET-ONLY            VALUE 'Y'.
102717         16  cf-commission-cap-required     pic x.
102717         16  CF-ST-GA-COMMISSION-CAPS.
102717             20  CF-ST-GA-COMM-CAP-SL       PIC S9V9(4) COMP-3.
102717             20  CF-ST-GA-COMM-CAP-JL       PIC S9V9(4) COMP-3.
102717             20  CF-ST-GA-COMM-CAP-SA       PIC S9V9(4) COMP-3.
102717             20  CF-ST-GA-COMM-CAP-JA       PIC S9V9(4) COMP-3.
102717         16  CF-ST-TOT-COMMISSION-CAPS.
102717             20  CF-ST-TOT-COMM-CAP-SL      PIC S9V9(4) COMP-3.
102717             20  CF-ST-TOT-COMM-CAP-JL      PIC S9V9(4) COMP-3.
102717             20  CF-ST-TOT-COMM-CAP-SA      PIC S9V9(4) COMP-3.
102717             20  CF-ST-TOT-COMM-CAP-JA      PIC S9V9(4) COMP-3.
102717         16  FILLER                         PIC X(156).
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
                   20  cf-maximum-benefits        pic s999 comp-3.
                   20  FILLER                     PIC X(09).
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
032813         16  CF-CARRIER-NEXT-AUDIT-CHK-NO   PIC S9(8)     COMP.
032813         16  FILLER                         PIC X(444).
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
00471
00472      EJECT
00473 *    COPY ELCTRLR.
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
061511* 061511    2011042000002  AJRA  ADD VFY 2ND BENE TO ADDRESS TRAIL
020413* 020413    2012071700001  AJRA  PRINT SURVEY AND PRINT CLM FORM I
021213* 021213    2012092400007  AJRA  CAUSAL STATE SEQUENCE NO
061013* 061013  CR2012113000002  PEMA  SPP CLAIM RELATED CHANGES
102413* 102413  CR2013100800001  AJRA  ADD SPECIAL RELEASE IND
022614* 022614    2013050100003  AJRA  ADD CERT CANCELLED NOTE TYPE - T
040814* 040814    2014030500002  AJRA  ADD ICD CODES
052614* 052614  CR2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
013017* 013017  CR2016053100001  PEMA  ACH PROCESSING
062217* 062217  CR2017050300002  TANA  ADD AUTH RCVD
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
102418* 102418  CR2018083000001  TANA  ADD ADD NEW CALL TYPE
022122* 022122  CR2021100800003  PEMA  Add B and H claim types
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
061511             88  AT-VFY-2ND-BENE-NOTE-TRL     VALUE +93.
021213             88  AT-VFY-CAUSAL-STATE          VALUE +94.
                   88  AT-ERROR-MSGS-TRL            VALUE +95.
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
052614             88  PAID-FOR-FAM                   VALUE 'F'.
022122             88  PAID-FOR-BRV                   VALUE 'B'.
022122             88  PAID-FOR-HOS                   VALUE 'H'.
100518             88  PAID-FOR-OTH                   VALUE 'O'.
00125          16  AT-CLAIM-PREM-TYPE          PIC X.
00126              88  AT-SINGLE-PREMIUM              VALUE '1'.
00127              88  AT-O-B-COVERAGE                VALUE '2'.
00128              88  AT-OPEN-END-COVERAGE           VALUE '3'.
00129          16  AT-AMOUNT-PAID              PIC S9(7)V99  COMP-3.
00130          16  AT-CHECK-NO                 PIC X(7).
00131          16  AT-PAID-FROM-DT             PIC XX.
00132          16  AT-PAID-THRU-DT             PIC XX.
00133          16  AT-DAYS-IN-PERIOD           PIC S9(4)     COMP.
013017         16  AT-ACH-PAYMENT              PIC X.
013017*        16  FILLER                      PIC X.
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
020413         16  FILLER REDEFINES AT-EOB-CODE3.
020413             20  AT-PRINT-CLM-FORM       PIC X.
020413             20  AT-PRINT-SURVEY         PIC X.
102413             20  AT-SPECIAL-RELEASE      PIC X.
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
062217             20  AT-AUTH-RCVD            PIC X(1).
062217             20  FILLER                  PIC X(18).
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
061511*         16  FILLER                      PIC X(23).
061511         16  AT-VFY-2ND-BENE-SSN         PIC X(9).
061511         16  AT-VFY-2ND-BENE-VERIFIED    PIC X.
061511         16  FILLER                      PIC X(13).
00319          16  AT-ADDRESS-LAST-MAINT-DT    PIC XX.
00320          16  AT-ADDRESS-LAST-UPDATED-BY  PIC X(4).
00321
00322      12  AT-GENERAL-INFO-TR  REDEFINES  AT-TRAILER-BODY.
00323          16  AT-INFO-LINE-1              PIC X(60).
061013         16  FILLER REDEFINES AT-INFO-LINE-1.
061013             20  AT-NOTE-ERROR-NO OCCURS 15
061013                                         PIC X(4).
00324          16  AT-INFO-LINE-2              PIC X(60).
040814         16  FILLER REDEFINES AT-INFO-LINE-2.
040814             20  AT-ICD-CODE-1           PIC X(8).
040814             20  AT-ICD-CODE-2           PIC X(8).
040814             20  FILLER                  PIC X(44).
00325          16  AT-INFO-TRAILER-TYPE        PIC X.
061013             88  AT-ERRORS-NOTE          VALUE 'E'.
00326              88  AT-PAYMENT-NOTE         VALUE 'P'.
00327              88  AT-CALL-NOTE            VALUE 'C'.
00328              88  AT-MAINT-NOTE           VALUE 'M'.
00329              88  AT-CERT-CHANGE          VALUE 'X'.
080106             88  AT-APPROVAL-NOTE        VALUE 'R'.
080106             88  AT-NOTE-FILE-NOTE       VALUE 'N'.
022614             88  AT-CERT-CANCELLED       VALUE 'T'.
00330          16  AT-CALL-TYPE                PIC X.
00331              88  AT-PHONE-CALL-IN        VALUE 'I'.
102418             88  AT-PHONE-CALL-NEW       VALUE 'N'.
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
040814         16  AT-OLD-ICD-CODE-1           PIC X(8).
040814         16  AT-OLD-ICD-CODE-2           PIC X(8).
040814         16  FILLER                      PIC X(9).
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
00474
00475      EJECT
00476 *    COPY ELCMSTR.
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
031213* 031213    2012113000002  PEMA  ADD ACCIDENT INDICATOR
051414* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
052614* 052614    2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
081817* 081817    2016100700001  TANA  ADD NBR OF EXTENSIONS
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
022122* 022122  CR2021100800003  PEMA  Add B and H claim types
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
052614*            88  FAMILY-LEAVE-CLAIM     VALUE 'F'.
100518*            88  OTHER-CLAIM            VALUE 'O'.
022122*            88  hospital-claim         value 'H'.
022122*            88  bereavement-claim      value 'B'.
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
051414         16  FILLER REDEFINES CL-MICROFILM-NO.
051414             20  CL-BENEFIT-PERIOD   PIC 99.
051414             20  FILLER              PIC X(8).
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
031213         16  CL-CRITICAL-PERIOD      PIC 99.
031213*        16  FILLER                  PIC XX.
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
081817         16  CL-NO-OF-EXTENSIONS     PIC 99.
081817         16  filler                  pic x(3).
      *        16  CL-CRIT-PER-RECURRENT   PIC X.
      *        16  CL-CRIT-PER-RTW-MOS     PIC 99.
      *        16  CL-RTW-DT               PIC XX.
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
CIDMOD     12  CL-YESNOSW                  PIC X.
031213     12  CL-ACCIDENT-CLAIM-SW        PIC X.
031213         88  CL-ACCIDENT-NOT-SET           VALUE ' '.
031213         88  CL-CLAIM-DUE-TO-ACCIDENT      VALUE 'Y'.
031213         88  CL-CLAIM-NOT-DUE-TO-ACCIDENT  VALUE 'N'.
051414     12  cl-insured-type             pic x.
051414         88  cl-claim-on-primary         value 'P'.
051414         88  cl-claim-on-co-borrower     value 'C'.
031213     12  cl-benefit-expiration-dt    PIC XX.
00477
00478      EJECT
00479 *    COPY ELCCERT.
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
032612* 032612  CR2011110200001  PEMA  AHL CHANGES
090314* 090314  CR2014081300001  PEMA  LOAD CERTS INVOLVED IN THAO
010716* 010716  CR2015082500001  PEMA CHG POLICY FEE TO CANCEL FEE
062017* 062017  CR2015091000001  PEMA RENAME INTEREST FIELD
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
090314         16  cm-temp-epiq                  pic xx.
090314             88  EPIQ-CLASS                  value 'EQ'.
090314*        16  FILLER                        PIC XX.
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
010716         16  CM-CANCEL-FEE                 PIC S9(3)V99  COMP-3.
00126          16  CM-AH-CEDED-BENEFIT           PIC S9(7)V99  COMP-3.
00127          16  FILLER                        PIC X.
00128
00129      12  CM-LOAN-INFORMATION.
00130          16  CM-LIVES                      PIC S9(7)     COMP-3.
011410         16  CM-DDF-IU-RATE-UP REDEFINES CM-LIVES
011410                                           PIC S9(5)V99  COMP-3.
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
062017         16  CM-REF-INTERFACE-SW           PIC X.
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
                   88  CM-LF-POLICY-IS-CASH         VALUE 'C'.
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
                   88  CM-AH-POLICY-IS-CASH         VALUE 'C'.
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
062017     12  CM-INT-ON-REFS                    PIC S9(7)V99   COMP-3.
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
032612     12  CM-LF-CLASS-CD REDEFINES CM-CLP-STATE PIC XX.
061405     12  CM-USER-RESERVED                  PIC XXX.
032612     12  FILLER REDEFINES CM-USER-RESERVED.
032612         16  CM-AH-CLASS-CD                PIC XX.
032612         16  F                             PIC X.
00286 ******************************************************************
00480
00481      EJECT
00482 *    COPY ERCACCT.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCACCT                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.031                          *
00006 *                                                                *
00007 *   CREDIT SYSTEM ACCOUNT MASTER FILE                            *
00008 *                                                                *
00009 *   THIS COPYBOOK IS USED FOR BOTH THE ONLINE AND BATCH          *
00010 *   VSAM ACCOUNT MASTER FILES.                                   *
00011 *                                                                *
00012 *   FILE DESCRIPTION = ACCOUNT OR PRODUCER FILES                 *
00013 *                                                                *
00014 *   FILE TYPE = VSAM,KSDS                                        *
00015 *   RECORD SIZE = 2000  RECFORM = FIX                            *
00016 *                                                                *
00017 *   BASE CLUSTER NAME = ERACCT                    RKP=2,LEN=26   *
00018 *       ALTERNATE PATH1 = ERACCT2 (ALT GROUPING) RKP=28,LEN=26   *
00019 *                                                                *
00020 *   LOG = NO                                                     *
00021 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00022 *                                                                *
00023 *                                                                *
00024 ******************************************************************
102004*                   C H A N G E   L O G
102004*
102004* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
102004*-----------------------------------------------------------------
102004*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
102004* EFFECTIVE    NUMBER
102004*-----------------------------------------------------------------
102004* 102004    2003031400002  PEMA  ADD NEW STATUS CODE
092705* 092705    2005050300006  PEMA  ADD SPP LEASES
022808* 022808    2007083100002  PEMA  ADD FREEZE STATUS
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
030211* 030211  CR2010012100001  PEMA  ADD EMAILS FROM RDS
031811* 031811  CR2011012700001  PEMA  ADD ACCT STATUS S - SUSPENDED
101711* 101711  CR2011092000001  PEMA  ADD UNEARNED FACTOR STATE FOR DCC
021916* 021916  CR2014010900001  TANA  ADD NEW STATUS CODE VALUES
102004******************************************************************
00025
00026  01  ACCOUNT-MASTER.
00027      12  AM-RECORD-ID                      PIC XX.
00028          88  VALID-AM-ID                      VALUE 'AM'.
00029
00030      12  AM-CONTROL-PRIMARY.
00031          16  AM-COMPANY-CD                 PIC X.
00032          16  AM-MSTR-CNTRL.
00033              20  AM-CONTROL-A.
00034                  24  AM-CARRIER            PIC X.
00035                  24  AM-GROUPING.
00036                      28 AM-GROUPING-PREFIX PIC XXX.
00037                      28 AM-GROUPING-PRIME  PIC XXX.
00038                  24  AM-STATE              PIC XX.
00039                  24  AM-ACCOUNT.
00040                      28  AM-ACCOUNT-PREFIX PIC X(4).
00041                      28  AM-ACCOUNT-PRIME  PIC X(6).
00042              20  AM-CNTRL-1   REDEFINES   AM-CONTROL-A
00043                                            PIC X(19).
00044              20  AM-CNTRL-B.
00045                  24  AM-EXPIRATION-DT      PIC XX.
00046                  24  FILLER                PIC X(4).
00047              20  AM-CNTRL-2 REDEFINES AM-CNTRL-B.
00048                  24  AM-EXPIRE-DT          PIC 9(11)  COMP-3.
00049
00050      12  AM-CONTROL-BY-VAR-GRP.
00051          16  AM-COMPANY-CD-A1              PIC X.
00052          16  AM-VG-CARRIER                 PIC X.
00053          16  AM-VG-GROUPING                PIC X(6).
00054          16  AM-VG-STATE                   PIC XX.
00055          16  AM-VG-ACCOUNT                 PIC X(10).
00056          16  AM-VG-DATE.
00057              20  AM-VG-EXPIRATION-DT       PIC XX.
00058              20  FILLER                    PIC X(4).
00059          16  AM-VG-EXP-DATE REDEFINES AM-VG-DATE
00060                                            PIC 9(11)      COMP-3.
030211     12  FILLER REDEFINES AM-CONTROL-BY-VAR-GRP.
030211         16  FILLER                        PIC X(10).
030211         16  AM-VG-KEY3.
030211             20  AM-VG3-ACCOUNT            PIC X(10).
030211             20  AM-VG3-EXP-DT             PIC XX.
030211         16  FILLER                        PIC X(4).
00061      12  AM-MAINT-INFORMATION.
00062          16  AM-LAST-MAINT-DT              PIC XX.
00063          16  AM-LAST-MAINT-HHMMSS          PIC S9(7)      COMP-3.
00064          16  AM-LAST-MAINT-USER            PIC X(4).
00065          16  FILLER                        PIC XX.
00066
00067      12  AM-EFFECTIVE-DT                   PIC XX.
00068      12  AM-EFFECT-DT                      PIC 9(11)      COMP-3.
00069
00070      12  AM-PREV-DATES  COMP-3.
00071          16  AM-PREV-EXP-DT                PIC 9(11).
00072          16  AM-PREV-EFF-DT                PIC 9(11).
00073
00074      12  AM-REPORT-CODE-1                  PIC X(10).
00075      12  AM-REPORT-CODE-2                  PIC X(10).
00076
00077      12  AM-CITY-CODE                      PIC X(4).
00078      12  AM-COUNTY-PARISH                  PIC X(6).
00079
00080      12  AM-NAME                           PIC X(30).
00081      12  AM-PERSON                         PIC X(30).
00082      12  AM-ADDRS                          PIC X(30).
00083      12  AM-CITY.
               16  AM-ADDR-CITY                  PIC X(28).
               16  AM-ADDR-STATE                 PIC XX.
00084      12  AM-ZIP.
00085          16  AM-ZIP-PRIME.
00086              20  AM-ZIP-PRI-1ST            PIC X.
00087                  88  AM-CANADIAN-POST-CODE    VALUE 'A' THRU 'Z'.
00088              20  FILLER                    PIC X(4).
00089          16  AM-ZIP-PLUS4                  PIC X(4).
00090      12  AM-CANADIAN-POSTAL-CODE  REDEFINES  AM-ZIP.
00091          16  AM-CAN-POSTAL-1               PIC XXX.
00092          16  AM-CAN-POSTAL-2               PIC XXX.
00093          16  FILLER                        PIC XXX.
00094      12  AM-TEL-NO.
00095          16  AM-AREA-CODE                  PIC 999.
00096          16  AM-TEL-PRE                    PIC 999.
00097          16  AM-TEL-NBR                    PIC 9(4).
00098      12  AM-TEL-LOC                        PIC X.
00099          88  AM-TEL-AT-HOME                   VALUE 'H'.
00100          88  AM-TEL-AT-BUSINESS               VALUE 'B'.
00101
00102      12  AM-COMM-STRUCTURE.
00103          16  AM-DEFN-1.
00104              20  AM-AGT-COMMS       OCCURS 10 TIMES.
00105                  24  AM-AGT.
00106                      28  AM-AGT-PREFIX     PIC X(4).
00107                      28  AM-AGT-PRIME      PIC X(6).
00108                  24  AM-COM-TYP            PIC X.
00109                  24  AM-L-COM              PIC SV9(5)     COMP-3.
00110                  24  AM-J-COM              PIC SV9(5)     COMP-3.
00111                  24  AM-A-COM              PIC SV9(5)     COMP-3.
00112                  24  AM-RECALC-LV-INDIC    PIC X.
00113                  24  AM-RETRO-LV-INDIC     PIC X.
00114                  24  AM-GL-CODES           PIC X.
00115                  24  AM-COMM-CHARGEBACK    PIC 9(02).
00116                  24  FILLER                PIC X(01).
00117          16  AM-DEFN-2   REDEFINES   AM-DEFN-1.
00118              20  AM-COM-TBLS        OCCURS 10 TIMES.
00119                  24  FILLER                PIC X(11).
00120                  24  AM-L-COMA             PIC XXX.
00121                  24  AM-J-COMA             PIC XXX.
00122                  24  AM-A-COMA             PIC XXX.
00123                  24  FILLER                PIC X(6).
00124
00125      12  AM-COMM-CHANGE-STATUS             PIC X.
00126          88  AM-COMMISSIONS-CHANGED           VALUE '*'.
00127
00128      12  AM-CSR-CODE                       PIC X(4).
00129
00130      12  AM-BILLING-STATUS                 PIC X.
00131          88  AM-ACCOUNT-BILLED                VALUE 'B'.
00132          88  AM-ACCOUNT-NOT-BILLED            VALUE ' '.
00133      12  AM-AUTO-REFUND-SW                 PIC X.
00134          88  AUTO-REFUNDS-USED                VALUE 'Y'.
00135          88  AUTO-REFUNDS-NOT-USED            VALUE 'N' ' '.
00136      12  AM-GPCD                           PIC 99.
00137      12  AM-IG                             PIC X.
00138          88  AM-HAS-INDIVIDUAL                VALUE '1'.
00139          88  AM-HAS-GROUP                     VALUE '2'.
00140      12  AM-STATUS                         PIC X.
00141          88  AM-ACCOUNT-ACTIVE                VALUE '0'.
00142          88  AM-ACCOUNT-INACTIVE              VALUE '1'.
00143          88  AM-ACCOUNT-TRANSFERRED           VALUE '2'.
102004         88  AM-ACCOUNT-CANCELLED             VALUE '3'.
022808         88  AM-ACCOUNT-FROZEN                VALUE '4'.
031811         88  AM-ACCOUNT-SUSPENDED             VALUE '5'.
021916         88  AM-ACCOUNT-DROPPED               VALUE '6'.
021916         88  AM-ACCOUNT-LAPSED                VALUE '7'.
021916         88  AM-ACCOUNT-RUN-OFF               VALUE '8'.
021916         88  AM-ACCOUNT-PENDING               VALUE '9'.
00144      12  AM-REMIT-TO                       PIC 99.
00145      12  AM-ID-NO                          PIC X(11).
00146
00147      12  AM-CAL-TABLE                      PIC XX.
00148      12  AM-LF-DEVIATION                   PIC XXX.
00149      12  AM-AH-DEVIATION                   PIC XXX.
00150      12  AM-LF-DEVIATION-PCT               PIC S9V9(6)    COMP-3.
00151      12  AM-AH-DEVIATION-PCT               PIC S9V9(6)    COMP-3.
00152      12  AM-LF-OB-RATE                     PIC S99V9(5)   COMP-3.
00153      12  AM-AH-OB-RATE                     PIC S99V9(5)   COMP-3.
00154      12  AM-LF-OB-RATE-JNT                 PIC S99V9(5)   COMP-3.
00155      12  AM-AH-OB-RATE-JNT                 PIC S99V9(5)   COMP-3.
00156
00157      12  AM-USER-FIELDS.
00158          16  AM-FLD-1                      PIC XX.
00159          16  AM-FLD-2                      PIC XX.
00160          16  AM-FLD-3                      PIC XX.
00161          16  AM-FLD-4                      PIC XX.
00162          16  AM-FLD-5                      PIC XX.
00163
00164      12  AM-1ST-PROD-DATE.
00165          16  AM-1ST-PROD-YR                PIC XX.
00166          16  AM-1ST-PROD-MO                PIC XX.
00167          16  AM-1ST-PROD-DA                PIC XX.
00168      12  AM-ANNIVERSARY-DATE               PIC 9(11)  COMP-3.
00169      12  AM-CERTS-PURGED-DATE.
00170          16  AM-PUR-YR                     PIC XX.
00171          16  AM-PUR-MO                     PIC XX.
00172          16  AM-PUR-DA                     PIC XX.
00173      12  AM-HI-CERT-DATE                   PIC 9(11)  COMP-3.
00174      12  AM-LO-CERT-DATE                   PIC 9(11)  COMP-3.
00175      12  AM-ENTRY-DATE                     PIC 9(11)  COMP-3.
00176      12  AM-INACTIVE-DATE.
00177          16  AM-INA-MO                     PIC 99.
00178          16  AM-INA-DA                     PIC 99.
00179          16  AM-INA-YR                     PIC 99.
00180      12  AM-AR-HI-CERT-DATE                PIC XX.
00181
00182      12  AM-LF-PSI-FACTOR                  PIC S9V9(6)    COMP-3.
00183      12  AM-AH-PSI-FACTOR                  PIC S9V9(6)    COMP-3.
00184
00185      12  AM-OB-PAYMENT-MODE                PIC X.
00186          88  AM-OB-PAID-MONTHLY               VALUE 'M' ' '.
00187          88  AM-OB-PAID-QUARTERLY             VALUE 'Q'.
00188          88  AM-OB-PAID-SEMI-ANNUALLY         VALUE 'S'.
00189          88  AM-OB-PAID-ANNUALLY              VALUE 'A'.
00190
00191      12  AM-AH-ONLY-INDICATOR              PIC X.
00192          88  AM-AH-ONLY-ALLOWED               VALUE 'Y' ' '.
00193          88  AM-NO-AH-ONLY                    VALUE 'N'.
00194
00195      12  AM-EDIT-LOAN-OFC                  PIC X(01).
00196
00197      12  AM-OVER-SHORT.
00198          16 AM-OVR-SHT-AMT                 PIC S999V99    COMP-3.
00199          16 AM-OVR-SHT-PCT                 PIC S9V9(4)    COMP-3.
00200
011410     12  AM-DCC-PRODUCT-CODE               PIC XXX.
041910     12  AM-DCC-CLP-STATE                  PIC XX.
00202
00203      12  AM-RECALC-COMM                    PIC X.
00204      12  AM-RECALC-REIN                    PIC X.
00205
00206      12  AM-REI-TABLE                      PIC XXX.
00207      12  AM-REI-ET-LF                      PIC X.
00208      12  AM-REI-ET-AH                      PIC X.
00209      12  AM-REI-PE-LF                      PIC X.
00210      12  AM-REI-PE-AH                      PIC X.
00211      12  AM-REI-PRT-ST                     PIC X.
00212      12  AM-REI-FEE-LF                     PIC S9V9999    COMP-3.
00213      12  AM-REI-FEE-AH                     PIC S9V9999    COMP-3.
00214      12  AM-REI-LF-TAX                     PIC S9V9999    COMP-3.
00215      12  AM-REI-GROUP-A                    PIC X(6).
00216      12  AM-REI-MORT                       PIC X(4).
00217      12  AM-REI-PRT-OW                     PIC X.
00218      12  AM-REI-PR-PCT                     PIC S9V9999    COMP-3.
00219      12  AM-REI-78-PCT                     PIC S9V9999    COMP-3.
00220      12  AM-REI-AH-TAX                     PIC S9V9999    COMP-3.
00221      12  AM-REI-GROUP-B                    PIC X(6).
00222
00223      12  AM-TRUST-TYPE                     PIC X(2).
00224
00225      12  AM-EMPLOYER-STMT-USED             PIC X.
00226      12  AM-GROUPED-CHECKS-Y-N             PIC X.
00227
00228      12  AM-STD-AH-TYPE                    PIC XX.
00229      12  AM-EARN-METHODS.
00230          16  AM-EARN-METHOD-R              PIC X.
00231              88 AM-REF-RL-R78                 VALUE 'R'.
00232              88 AM-REF-RL-PR                  VALUE 'P'.
00233              88 AM-REF-RL-MEAN                VALUE 'M'.
00234              88 AM-REF-RL-ANTICIPATION        VALUE 'A'.
00235          16  AM-EARN-METHOD-L              PIC X.
00236              88 AM-REF-LL-R78                 VALUE 'R'.
00237              88 AM-REF-LL-PR                  VALUE 'P'.
00238              88 AM-REF-LL-MEAN                VALUE 'M'.
00239              88 AM-REF-LL-ANTICIPATION        VALUE 'A'.
00240          16  AM-EARN-METHOD-A              PIC X.
00241              88 AM-REF-AH-R78                 VALUE 'R'.
00242              88 AM-REF-AH-PR                  VALUE 'P'.
00243              88 AM-REF-AH-MEAN                VALUE 'M'.
00244              88 AM-REF-AH-ANTICIPATION        VALUE 'A'.
00245              88 AM-REF-AH-CALIF-SPEC          VALUE 'C'.
00246              88 AM-REF-AH-NET                 VALUE 'N'.
00247
00248      12  AM-TOL-PREM                       PIC S999V99    COMP-3.
00249      12  AM-TOL-REF                        PIC S999V99    COMP-3.
00250      12  AM-TOL-CLM                        PIC S999V99    COMP-3.
00251
00252      12  AM-RET-Y-N                        PIC X.
00253      12  AM-RET-P-E                        PIC X.
00254      12  AM-LF-RET                         PIC S9V9999    COMP-3.
00255      12  AM-AH-RET                         PIC S9V9999    COMP-3.
00256      12  AM-RET-GRP                        PIC X(6).
00257      12  AM-RETRO-POOL  REDEFINES  AM-RET-GRP.
00258          16  AM-POOL-PRIME                 PIC XXX.
00259          16  AM-POOL-SUB                   PIC XXX.
00260      12  AM-RETRO-EARNINGS.
00261          16  AM-RET-EARN-R                 PIC X.
00262          16  AM-RET-EARN-L                 PIC X.
00263          16  AM-RET-EARN-A                 PIC X.
00264      12  AM-RET-ST-TAX-USE                 PIC X.
00265          88  CHARGE-ST-TAXES-ON-RETRO         VALUE 'Y' 'E' 'P'.
00266          88  TAXES-NOT-IN-RETRO               VALUE 'N' ' '.
00267      12  AM-RETRO-BEG-EARNINGS.
00268          16  AM-RET-BEG-EARN-R             PIC X.
00269          16  AM-RET-BEG-EARN-L             PIC X.
00270          16  AM-RET-BEG-EARN-A             PIC X.
00271      12  AM-RET-MIN-LOSS-L                 PIC SV999      COMP-3.
00272      12  AM-RET-MIN-LOSS-A                 PIC SV999      COMP-3.
00273
00274      12  AM-USER-SELECT-OPTIONS.
00275          16  AM-USER-SELECT-1              PIC X(10).
00276          16  AM-USER-SELECT-2              PIC X(10).
00277          16  AM-USER-SELECT-3              PIC X(10).
00278          16  AM-USER-SELECT-4              PIC X(10).
00279          16  AM-USER-SELECT-5              PIC X(10).
00280
00281      12  AM-LF-RPT021-EXP-PCT              PIC S9(3)V9(4) COMP-3.
00282
00283      12  AM-AH-RPT021-EXP-PCT              PIC S9(3)V9(4) COMP-3.
00284
00285      12  AM-RPT045A-SWITCH                 PIC X.
00286          88  RPT045A-OFF                   VALUE 'N'.
00287
00288      12  AM-INSURANCE-LIMITS.
00289          16  AM-MAX-MON-BEN                PIC S9(7)      COMP-3.
00290          16  AM-MAX-TOT-BEN                PIC S9(7)      COMP-3.
00291
00292      12  AM-PROFILE-CHANGE-SWITCH          PIC X.
00293          88  AM-PROFILE-DATA-CHANGED          VALUE '*'.
00294
00295      12  AM-DISMBR-COVERAGE-SW             PIC X.
00296          88  AM-DISMBR-COVERAGE               VALUE 'Y'.
00297          88  AM-NO-DISMBR-COVERAGE            VALUE 'N'.
00298
00299      12  AM-CANCEL-FEE                     PIC S9(3)V9(2) COMP-3.
00300
00301      12  AM-TOL-REF-PCT                    PIC S9V9(4)    COMP-3.
090803     12  AM-CLP-TOL-PCT                    PIC S9V9(4)    COMP-3.
092705     12  AM-SPP-LEASE-COMM                 PIC S9(5)V99   COMP-3.
           12  AM-DCC-MAX-MARKETING-FEE          PIC S9(5)      COMP-3.
           12  AM-DCC-UEF-STATE                  PIC XX.
           12  FILLER                            PIC XXX.
120406     12  AM-REPORT-CODE-3                  PIC X(10).
090803*    12  FILLER                            PIC X(22).
00303
00304      12  AM-RESERVE-DATE.
00305          16  AM-TARGET-LOSS-RATIO          PIC S9V9(4) COMP-3.
00306          16  AM-LIFE-IBNR-PCT              PIC S9V9(4) COMP-3.
00307          16  AM-CRDT-MODIFICATION-PCT      PIC S9V9(4) COMP-3.
00308
00309      12  AM-3RD-PARTY-NOTIF-LEVEL          PIC 99.
00310      12  AM-NOTIFICATION-TYPES.
00311          16  AM-NOTIF-OF-LETTERS           PIC X.
00312          16  AM-NOTIF-OF-PAYMENTS          PIC X.
00313          16  AM-NOTIF-OF-REPORTS           PIC X.
00314          16  AM-NOTIF-OF-STATUS            PIC X.
00315
00316      12  AM-BENEFIT-TABLE-USAGE            PIC X.
00317          88  AM-BENEFIT-TABLE-USED            VALUE 'Y'.
00318          88  AM-USE-DEVIATIONS-ONLY           VALUE 'D'.
00319          88  AM-EDIT-BENEFITS-ONLY            VALUE 'E'.
00320          88  AM-EDITS-NOT-USED                VALUE ' '  'N'.
00321
00322      12  AM-BENEFIT-CONTROLS.
00323          16  AM-ALLOWABLE-BENEFITS  OCCURS  20  TIMES.
00324              20  AM-BENEFIT-CODE           PIC XX.
00325              20  AM-BENEFIT-TYPE           PIC X.
00326              20  AM-BENEFIT-REVISION       PIC XXX.
00327              20  AM-BENEFIT-REM-TERM       PIC X.
00328              20  AM-BENEFIT-RETRO-Y-N      PIC X.
00329              20  FILLER                    PIC XX.
00330          16  FILLER                        PIC X(80).
00331
00332      12  AM-TRANSFER-DATA.
00333          16  AM-TRANSFERRED-FROM.
00334              20  AM-TRNFROM-CARRIER        PIC X.
00335              20  AM-TRNFROM-GROUPING.
00336                  24  AM-TRNFROM-GRP-PREFIX PIC XXX.
00337                  24  AM-TRNFROM-GRP-PRIME  PIC XXX.
00338              20  AM-TRNFROM-STATE          PIC XX.
00339              20  AM-TRNFROM-ACCOUNT.
00340                  24  AM-TRNFROM-ACCT-PREFIX PIC X(4).
00341                  24  AM-TRNFROM-ACCT-PRIME PIC X(6).
00342              20  AM-TRNFROM-DTE            PIC XX.
00343          16  AM-TRANSFERRED-TO.
00344              20  AM-TRNTO-CARRIER          PIC X.
00345              20  AM-TRNTO-GROUPING.
00346                  24  AM-TRNTO-GRP-PREFIX   PIC XXX.
00347                  24  AM-TRNTO-GRP-PRIME    PIC XXX.
00348              20  AM-TRNTO-STATE            PIC XX.
00349              20  AM-TRNTO-ACCOUNT.
00350                  24  AM-TRNTO-ACCT-PREFIX  PIC X(4).
00351                  24  AM-TRNTO-ACCT-PRIME   PIC X(6).
00352              20  AM-TRNTO-DTE              PIC XX.
00353          16  FILLER                        PIC X(10).
00354
00355      12  AM-SAVED-REMIT-TO                 PIC 99.
00356
00357      12  AM-COMM-STRUCTURE-SAVED.
00358          16  AM-DEFN-1-SAVED.
00359              20  AM-AGT-COMMS-SAVED    OCCURS 10 TIMES.
00360                  24  AM-AGT-SV             PIC X(10).
00361                  24  AM-COM-TYP-SV         PIC X.
00362                  24  AM-L-COM-SV           PIC SV9(5)     COMP-3.
00363                  24  AM-J-COM-SV           PIC SV9(5)     COMP-3.
00364                  24  AM-A-COM-SV           PIC SV9(5)     COMP-3.
00365                  24  AM-RECALC-LV-INDIC-SV PIC X.
00366                  24  FILLER                PIC X.
00367                  24  AM-GL-CODES-SV        PIC X.
00368                  24  AM-COM-CHARGEBACK-SV  PIC 99.
00369                  24  FILLER                PIC X.
00370          16  AM-DEFN-2-SAVED   REDEFINES   AM-DEFN-1-SAVED.
00371              20  AM-COM-TBLS-SAVED    OCCURS 10 TIMES.
00372                  24  FILLER                PIC X(11).
00373                  24  AM-L-COMA-SV          PIC XXX.
00374                  24  AM-J-COMA-SV          PIC XXX.
00375                  24  AM-A-COMA-SV          PIC XXX.
00376                  24  FILLER                PIC X(6).
00377
00378      12  AM-FLC-NET-PREMIUM-ALLOWANCE.
00379          16 AM-ACCOUNT-ALLOWANCE OCCURS  5 TIMES.
00380             20  AM-ALLOW-BEGIN-RANGE       PIC S9(5)      COMP-3.
00381             20  AM-ALLOW-END-RANGE         PIC S9(5)      COMP-3.
00382             20  AM-ALLOWANCE-AMT           PIC S9(5)V99   COMP-3.
00383
122806     12  AM-ORIG-DEALER-NO                 PIC X(10).
122806     12  FILLER                            PIC X(120).
00385
00386      12  AM-ACCOUNT-EXECUTIVE-DATA.
00387          16  AM-CONTROL-NAME               PIC X(30).
00388          16  AM-EXECUTIVE-ONE.
00389              20  AM-EXEC1-NAME             PIC X(15).
00390              20  AM-EXEC1-DIS-PERCENT      PIC S9(01)V9(04)
00391                                                           COMP-3.
00392              20  AM-EXEC1-LIFE-PERCENT     PIC S9(01)V9(04)
00393                                                           COMP-3.
00394          16  AM-EXECUTIVE-TWO.
00395              20  AM-EXEC2-NAME             PIC X(15).
00396              20  AM-EXEC2-DIS-PERCENT      PIC S9(01)V9(04)
00397                                                           COMP-3.
00398              20  AM-EXEC2-LIFE-PERCENT     PIC S9(01)V9(04)
00399                                                           COMP-3.
00400
00401      12  AM-RETRO-ADDITIONAL-DATA.
00402          16  AM-RETRO-QUALIFY-LIMIT        PIC S9(7)      COMP-3.
00403          16  AM-RETRO-PREM-P-E             PIC X.
00404          16  AM-RETRO-CLMS-P-I             PIC X.
00405          16  AM-RETRO-RET-BRACKET-LF.
00406              20  AM-RETRO-RET-METHOD-LF    PIC X.
00407                  88  AM-RETRO-USE-PCT-LF      VALUE 'P' ' '.
00408                  88  AM-RETRO-USE-SCALE-LF    VALUE 'S'.
00409              20  AM-RETRO-RET-BASIS-LF     PIC X.
00410                  88  AM-RETRO-EARN-BASIS-LF   VALUE 'E' ' '.
00411                  88  AM-RETRO-PAID-BASIS-LF   VALUE 'P'.
00412              20  AM-RETRO-BRACKETS-LF  OCCURS  3 TIMES.
00413                  24  AM-RETRO-RET-PCT-LF   PIC S9V9999    COMP-3.
00414                  24  AM-RETRO-RET-THRU-LF  PIC S9(7)      COMP-3.
00415          16  AM-RETRO-RET-BRACKET-AH.
00416              20  AM-RETRO-RET-METHOD-AH    PIC X.
00417                  88  AM-RETRO-USE-PCT-AH      VALUE 'P' ' '.
00418                  88  AM-RETRO-USE-SCALE-AH    VALUE 'S'.
00419                  88  AM-RETRO-USE-LIFE-METHOD VALUE 'L'.
00420              20  AM-RETRO-RET-BASIS-AH     PIC X.
00421                  88  AM-RETRO-EARN-BASIS-AH   VALUE 'E' ' '.
00422                  88  AM-RETRO-PAID-BASIS-AH   VALUE 'P'.
00423              20  AM-RETRO-BRACKETS-AH  OCCURS  3 TIMES.
00424                  24  AM-RETRO-RET-PCT-AH   PIC S9V9999    COMP-3.
00425                  24  AM-RETRO-RET-THRU-AH  PIC S9(7)      COMP-3.
00426
00427      12  AM-COMMENTS.
00428          16  AM-COMMENT-LINE           PIC X(50)   OCCURS 5 TIMES.
00429
00430      12  AM-CLIENT-OVERLAY-FLI   REDEFINES   AM-COMMENTS.
00431          16  AM-FLI-RETRO-SHARE-CODE       PIC X.
00432          16  AM-FLI-BILLING-CODE           PIC X.
00433          16  AM-FLI-ALT-STATE-CODE         PIC XX.
00434          16  AM-FLI-UNITED-IDENT           PIC X.
00435          16  AM-FLI-INTEREST-LOST-DATA.
00436              20  AM-FLI-BANK-NO            PIC X(5).
00437              20  AM-FLI-BANK-BALANCE       PIC S9(9)V99   COMP-3.
00438              20  AM-FLI-BANK-1ST-6-PREM    PIC S9(9)V99   COMP-3.
00439              20  AM-FLI-BANK-CAP-AMT       PIC S9(9)V99   COMP-3.
00440          16  AM-FLI-ALT-AGENT-CODES   OCCURS 10 TIMES.
00441              20  AM-FLI-AGT                PIC X(9).
00442              20  AM-FLI-AGT-COMM-ACC       PIC X.
00443              20  AM-FLI-AGT-SHARE-PCT      PIC S9V99      COMP-3.
00444          16  FILLER                        PIC X(102).
00445
00446      12  AM-CLIENT-OVERLAY-DMD   REDEFINES   AM-COMMENTS.
00447          16  AM-ALLOWABLE-DMD-BENEFITS  OCCURS 30 TIMES.
00448              20  AM-BENEFIT-DMD-CODE         PIC XX.
00449              20  AM-BENEFIT-DMD-TYPE         PIC X.
00450              20  AM-BENEFIT-DMD-REVISION     PIC XXX.
00451              20  AM-BENEFIT-DMD-REM-TERM     PIC X.
00452              20  AM-BENEFIT-DMD-RETRO-Y-N    PIC X.
00453          16  FILLER                          PIC X(10).
00454 ******************************************************************
00483
00484      EJECT
00485 *    COPY ELCBENE.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCBENE.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.006                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = BENEFICIARY FILE                          *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 500   RECFORM = FIX                            *
00011 *                                                                *
00012 *   BASE CLUSTER NAME = ELBENE                   RKP=2,LEN=12    *
00013 *     ALTERNATE PATH1 = ELBENE2 (ALT BY NAME)    RKP=14,LEN=42   *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00017 *                                                                *
CIDMOD*  NO  CID  MODS  TO  COPYBOOK  ELCBENE                          *
00018 ******************************************************************
013017*                   C H A N G E   L O G
013017*
013017* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
013017*-----------------------------------------------------------------
013017*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
013017* EFFECTIVE    NUMBER
013017*-----------------------------------------------------------------
013017* 013017  CR2016053100001  PEMA  ACH PROCESSING
082317* 082317  CR2017082100003  PEMA  Add sub type
032019* 032019  CR2019011400002  PEMA  Add email address for ach report
013017******************************************************************
00019
00020  01  BENEFICIARY-MASTER.
00021      12  BE-RECORD-ID                PIC XX.
00022          88  VALID-BE-ID                VALUE 'BE'.
00023
00024      12  BE-CONTROL-PRIMARY.
00025          16  BE-COMPANY-CD           PIC X.
00026          16  BE-RECORD-TYPE          PIC X.
00027              88  BENEFICIARY-RECORD  VALUE 'B'.
00028              88  ADJUSTOR-RECORD     VALUE 'A'.
00029          16  BE-BENEFICIARY          PIC X(10).
00030      12  BE-CONTROL-BY-NAME.
00031          16  BE-COMPANY-CD-A1        PIC X.
00032          16  BE-RECORD-TYPE-A1       PIC X.
00033          16  BE-MAIL-TO-NAME-A1      PIC X(30).
00034          16  BE-ALTERNATE-PRIME-A1   PIC X(10).
00035
00036      12  BE-LAST-MAINT-DT            PIC XX.
00037      12  BE-LAST-MAINT-BY            PIC X(4).
00038      12  BE-LAST-MAINT-HHMMSS        PIC S9(6)     COMP-3.
00039
00040      12  BE-ADDRESS-DATA.
00041          16  BE-MAIL-TO-NAME         PIC X(30).
00042          16  BE-ADDRESS-LINE-1       PIC X(30).
00043          16  BE-ADDRESS-LINE-2       PIC X(30).
00044          16  BE-ADDRESS-LINE-3       PIC X(30).
00045          16  BE-CITY-STATE.
051810             20  BE-CITY             PIC X(28).
051810             20  BE-STATE            PIC XX.
00046          16  BE-ZIP-CODE.
00047              20  BE-ZIP-PRIME.
00048                  24  BE-ZIP-1ST      PIC X.
00049                      88  BE-CANADIAN-POST-CODE
00050                                          VALUE 'A' THRU 'Z'.
00051                  24  FILLER          PIC X(4).
00052              20  BE-ZIP-PLUS4        PIC X(4).
00053          16  BE-CANADIAN-POSTAL-CODE  REDEFINES  BE-ZIP-CODE.
00054              20  BE-CAN-POSTAL-1     PIC XXX.
00055              20  BE-CAN-POSTAL-2     PIC XXX.
00056              20  FILLER              PIC XXX.
00057          16  BE-PHONE-NO             PIC 9(11)     COMP-3.
00058          16  BE-GROUP-CHECKS-Y-N     PIC X.
00059
00060 ******************************************************************
00061 *    THE BE-CARRIER FIELD IS USED BY 'AIG' TO DETERMINE HOW TO   *
00062 *    SET THE CARRIER CODE IN THE PENDING CLAIM FILE.             *
00063 ******************************************************************
00064      12  BE-CARRIER                  PIC X.
00065
00066      12  BE-ADDRESS-DATA2.
00067          16  BE-MAIL-TO-NAME2        PIC X(30).
00068          16  BE-ADDRESS-LINE-12      PIC X(30).
00069          16  BE-ADDRESS-LINE-22      PIC X(30).
00070          16  BE-ADDRESS-LINE-32      PIC X(30).
00071          16  BE-CITY-STATE2.
051810             20  BE-CITY2            PIC X(28).
051810             20  BE-STATE2           PIC XX.
00072          16  BE-ZIP-CODE2.
00073              20  BE-ZIP-PRIME2.
00074                  24  BE-ZIP-1ST2     PIC X.
00075                      88  BE-CANADIAN-POST-CODE2
00076                                          VALUE 'A' THRU 'Z'.
00077                  24  FILLER          PIC X(4).
00078              20  BE-ZIP-PLUS42       PIC X(4).
00079          16  BE-CANADIAN-POSTAL-CODE2 REDEFINES  BE-ZIP-CODE2.
00080              20  BE-CAN-POSTAL-12    PIC XXX.
00081              20  BE-CAN-POSTAL-22    PIC XXX.
00082              20  FILLER              PIC XXX.
00083          16  BE-PHONE-NO2            PIC 9(11)     COMP-3.
               16  BE-ACH-DATA.
                   20  BE-ACH-YES-OR-NO    PIC X.
                       88  BE-ON-ACH       VALUE 'Y'.
                       88  BE-NOT-ON-ACH   VALUE 'N' ' '.
                   20  BE-ACH-ABA-ROUTING-NUMBER
                                           PIC X(15).
                   20  BE-ACH-BANK-ACCOUNT-NUMBER
                                           PIC X(20).
                   20  BE-ACH-SUB-TYPE     PIC XX.
032019             20  BE-ACH-EMAIL-YN     PIC X.
032019                 88  BE-EMAIL-ACH-RPT  VALUE 'Y'.
032019             20  be-ach-email-addr   PIC X(40).
00084          16  BE-BILLING-STMT-DATA.
032019*            20  BE-BSR-PHONE-NUM    PIC 9(11)     COMP-3.
00091              20  BE-BSR-FAX-NUM      PIC 9(11)     COMP-3.
00092              20  BE-OUTPUT-TYPE      PIC X.
00093                  88  BE-FAX-OUTPUT         VALUE 'F'.
00094                  88  BE-PRINT-OUTPUT       VALUE 'P' ' '.
00095
032019     12  filler                      PIC X(16).
00097 ******************************************************************
00486
00487      EJECT
00488 *    COPY ERCCOMP.
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
071712* 071712  CR2012042700005  PEMA  ADD OVER 120 FOR AHL ONLY
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
071712     12  CO-AHL-OVER120-DATA REDEFINES CO-RPTCD2.
071712         16  CO-OV120                      PIC S9(7)V99   COMP-3.
071712         16  CO-CURRENT-OV120              PIC S9(7)V99   COMP-3.
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
00489
00490      EJECT
00491 *    COPY MPCPLCY.
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
00492
00493      EJECT
00494 *    COPY MPCPROD.
00001 ******************************************************************
00002 *                                                                *
00003 *                            MPCPROD                             *
00004 *                            VMOD=1.010                          *
00005 *                                                                *
00006 *   MORTGAGE SYSTEM PRODUCER MASTER FILE                         *
00007 *                                                                *
00008 *   THIS COPYBOOK IS USED FOR THE ONLINE                         *
00009 *   VSAM PRODUCER MASTER FILE.                                   *
00010 *                                                                *
00011 *   FILE DESCRIPTION = PRODUCER MASTER FILE                      *
00012 *                                                                *
00013 *   FILE TYPE = VSAM,KSDS                                        *
00014 *   RECORD SIZE = 2000 RECFORM = FIXED                           *
00015 *                                                                *
00016 *   BASE CLUSTER NAME = MPPROD                    RKP=02,LEN=22  *
00017 *       ALTERNATE PATH1 = MPPROD2 (ALT GROUPING)  RKP=48,LEN=22  *
00018 *       ALTERNATE PATH2 = MPPROD3 (PRODUCER NAME) RKP=90,LEN=56  *
00019 *                                                                *
00020 *   LOG = NO                                                     *
00021 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00022 *                                                                *
00023 *                                                                *
00024 ******************************************************************
00025
00026  01  PRODUCER-MASTER.
00027      12  PD-RECORD-ID                 PIC  X(02).
00028          88  PD-VALID-ID                   VALUE 'PD'.
00029
00030 ******************************************************************
00031 *   BASE CLUSTER NAME = MPPROD                    RKP=2,LEN=22   *
00032 ******************************************************************
00033
00034      12  PD-CONTROL-PRIMARY-BATCH.
00035          16  FILLER                   PIC  X(20).
00036          16  PD-EXPIRE-DT.
00037              20  PD-EXPIRE-DT-YY      PIC  9(02).
00038              20  PD-EXPIRE-DT-MM      PIC  9(02).
00039              20  PD-EXPIRE-DT-DD      PIC  9(02).
00040      12  FILLER REDEFINES PD-CONTROL-PRIMARY-BATCH.
00041          16  PD-CONTROL-PRIMARY.
00042              20  PD-COMPANY-CD        PIC  X(01).
00043              20  PD-MSTR-CNTRL.
00044                  24  PD-CONTROL-A.
00045                      28  PD-CARRIER   PIC  X(01).
00046                      28  PD-GROUPING.
00047                          32 PD-GROUPING-PREFIX
00048                                       PIC  X(03).
00049                          32 PD-GROUPING-PRIME
00050                                       PIC  X(03).
00051                      28  PD-STATE     PIC  X(02).
00052                      28  PD-PRODUCER.
00053                          32  PD-PRODUCER-PREFIX
00054                                       PIC  X(04).
00055                          32  PD-PRODUCER-PRIME
00056                                       PIC  X(06).
00057                  24  PD-CNTRL-B.
00058                      28  PD-EXPIRE-DATE
00059                                       PIC  X(02).
00060          16  FILLER REDEFINES PD-CONTROL-PRIMARY.
00061              20  FILLER               PIC  X(01).
00062              20  PD-CGSPE-KEY         PIC  X(21).
00063          16  FILLER                   PIC  X(04).
00064      12  FILLER                       PIC  X(20).
00065
00066 ******************************************************************
00067 *      ALTERNATE PATH1 = MPPROD2 (ALT GROUPING) RKP=48,LEN=22    *
00068 ******************************************************************
00069
00070      12  PD-CONTROL-BY-VAR-GRP.
00071          16  PD-VG-CCGSP-KEYLET.
00072              20  PD-COMPANY-CD-A1     PIC  X(01).
00073              20  PD-VG-CARRIER        PIC  X(01).
00074              20  PD-VG-GROUPING       PIC  X(06).
00075              20  PD-VG-STATE          PIC  X(02).
00076              20  PD-VG-PRODUCER       PIC  X(10).
00077          16  PD-VG-DATE.
00078              24  PD-VG-EXPIRE-DATE    PIC  X(02).
00079      12  FILLER                       PIC  X(20).
00080
00081
00082 ******************************************************************
00083 *      ALTERNATE PATH2 = MPPROD3 (NAME)         RKP=90,LEN=56    *
00084 ******************************************************************
00085
00086      12  PD-CONTROL-BY-NAME.
00087          16  PD-COMPANY-CD-A2         PIC  X(01).
00088          16  PD-NAME-A2               PIC  X(30).
00089          16  PD-CGSPE-KEY-A2.
00090              20  PD-CARRIER-A2        PIC  X(01).
00091              20  PD-GROUPING-A2       PIC  X(06).
00092              20  PD-STATE-A2          PIC  X(02).
00093              20  PD-PRODUCER-A2       PIC  X(10).
00094              20  PD-EXPIRE-DATE-A2    PIC  X(02).
00095          16  PD-CURRENT-DATE-BIN-A2   PIC  X(02).
00096          16  PD-CURRENT-TIME-BIN-A2   PIC S9(04) COMP.
00097      12  FILLER                       PIC  X(20).
00098
00099 ******************************************************************
00100 *                FILE SYNCHRONIZATION DATA                       *
00101 ******************************************************************
00102
00103      12  PD-MAINT-INFORMATION.
00104          16  PD-LAST-MAINT-DATE       PIC  X(02).
00105          16  PD-LAST-MAINT-HHMMSS     PIC S9(07) COMP-3.
00106          16  PD-LAST-MAINT-USER       PIC  X(04).
00107
00108 ******************************************************************
00109 *                PRODUCER SECURITY DATA                          *
00110 ******************************************************************
00111
00112      12  PD-SECURITY-ACCESS-CODE      PIC  X(01).
00113
00114 ******************************************************************
00115 *                DATES                                           *
00116 ******************************************************************
00117
00118      12  PD-ANNIVERSARY-DATE          PIC  X(02).
00119
00120      12  PD-AR-HI-DATE.
00121          16  PD-AR-HI-POLICY-DATE     PIC  X(02).
00122          16  FILLER                   PIC  X(04).
00123      12  PD-AR-HI-POLICY-DT REDEFINES PD-AR-HI-DATE.
00124          16  PD-AR-HI-POLICY-DT-YY    PIC  9(02).
00125          16  PD-AR-HI-POLICY-DT-MM    PIC  9(02).
00126          16  PD-AR-HI-POLICY-DT-DD    PIC  9(02).
00127
00128      12  PD-ENTRY-DATE                PIC  X(02).
00129
00130      12  PD-EFFECT-DTE.
00131          16  PD-EFFECT-DATE           PIC  X(02).
00132          16  FILLER                   PIC  X(04).
00133      12  PD-EFFECT-DT REDEFINES PD-EFFECT-DTE.
00134          16  PD-EFFECT-DT-YY          PIC  9(02).
00135          16  PD-EFFECT-DT-MM          PIC  9(02).
00136          16  PD-EFFECT-DT-DD          PIC  9(02).
00137
00138      12  PD-HI-DATE.
00139          16  PD-HI-POLICY-DATE        PIC  X(02).
00140          16  FILLER                   PIC  X(04).
00141      12  PD-HI-POLICY-DT REDEFINES PD-HI-DATE.
00142          16  PD-HI-POLICY-DT-YY       PIC  9(02).
00143          16  PD-HI-POLICY-DT-MM       PIC  9(02).
00144          16  PD-HI-POLICY-DT-DD       PIC  9(02).
00145
00146      12  PD-INACTIVE-DATE             PIC  X(02).
00147
00148      12  PD-LO-DATE.
00149          16  PD-LO-POLICY-DATE        PIC  X(02).
00150          16  FILLER                   PIC  X(04).
00151      12  PD-LO-POLICY-DT REDEFINES PD-LO-DATE.
00152          16  PD-LO-POLICY-DT-YY       PIC  9(02).
00153          16  PD-LO-POLICY-DT-MM       PIC  9(02).
00154          16  PD-LO-POLICY-DT-DD       PIC  9(02).
00155
00156      12  PD-POLICIES-PURGED-DATE      PIC  X(02).
00157
00158      12  PD-PREV-DATES.
00159          16  PD-PREV-EFF-DATE         PIC  X(02).
00160          16  FILLER                   PIC  X(04).
00161          16  PD-PREV-EXP-DATE         PIC  X(02).
00162          16  FILLER                   PIC  X(04).
00163      12  PD-PREV-DTS REDEFINES PD-PREV-DATES.
00164          16  PD-PREV-EFF-DT.
00165              20  PD-PREV-EFF-DT-YY    PIC  9(02).
00166              20  PD-PREV-EFF-DT-MM    PIC  9(02).
00167              20  PD-PREV-EFF-DT-DD    PIC  9(02).
00168          16  PD-PREV-EXP-DT.
00169              20  PD-PREV-EXP-DT-YY    PIC  9(02).
00170              20  PD-PREV-EXP-DT-MM    PIC  9(02).
00171              20  PD-PREV-EXP-DT-DD    PIC  9(02).
00172
00173      12  PD-1ST-PROD-DATE             PIC  X(02).
00174
00175      12  FILLER                       PIC  X(20).
00176
00177 ******************************************************************
00178 *                MORTGAGE BILLING DATA                           *
00179 ******************************************************************
00180
00181      12  PD-CONTACT                   PIC  X(30).
00182      12  PD-BILLING-MONTHS.
00183          16  PD-BILLING-MONTH-ANNUAL  PIC  9(02).
00184          16  PD-BILLING-MONTH-SEMIANN PIC  9(02).
00185      12  PD-BILLING-ADVANCE-ARREARS   PIC  X(01).
00186          88  PD-BILL-ADVANCE              VALUE '1'.
00187          88  PD-BILL-ARREARS              VALUE '2'.
00188      12  PD-BILLING-MODE              PIC  X(01).
00189          88  PD-ANNUAL-BILL               VALUE '1'.
00190          88  PD-SEMI-ANNUAL-BILL          VALUE '2'.
00191          88  PD-QUARTERLY-BILL            VALUE '3'.
00192          88  PD-MONTHLY-BILL              VALUE '4'.
00193          88  PD-BI-MONTHLY-BILL           VALUE '5'.
00194          88  PD-SINGLE-PREM-BILL          VALUE '6'.
00195      12  PD-BILLING-GROUPING-CODE     PIC  X(06).
00196      12  PD-BILLING-SCHEDULE          PIC  X(01).
00197          88  PD-BILL-1ST-WEEK             VALUE '1'.
00198          88  PD-BILL-2ND-WEEK             VALUE '2'.
00199          88  PD-BILL-3RD-WEEK             VALUE '3'.
00200          88  PD-BILL-4TH-WEEK             VALUE '4'.
00201          88  PD-BILL-5TH-WEEK             VALUE '5'.
00202          88  PD-HOLD-BILL                 VALUE '6'.
00203          88  PD-NO-BILL                   VALUE '7'.
00204      12  PD-BILLING-SEQUENCE          PIC  X(01).
00205          88  PD-BILL-NAME-SEQU            VALUE '1'.
00206          88  PD-BILL-LOAN-SEQU            VALUE '2'.
00207          88  PD-BILL-PLCY-SEQU            VALUE '3'.
00208      12  PD-BILLING-TYPE              PIC  X(01).
00209          88  PD-LIST-BILL                 VALUE '1'.
00210          88  PD-TAPE-BILL                 VALUE '2'.
00211          88  PD-TAPE-LIST-BILL            VALUE '3'.
00212          88  PD-GROUP-BILL            VALUES ARE '1' '2' '3'.
00213          88  PD-DIRECT-BILL               VALUE '4'.
00214          88  PD-PAC                   VALUES ARE '5' 'C' 'S'.
00215          88  PD-CREDIT-CARD               VALUE '6'.
00216          88  PD-INDIV-BILL
00217                               VALUES ARE '4' '5' '6' 'C' 'S'.
00218          88  PD-GROUP-BY-POLICY           VALUE '7'.
00219          88  PD-GROUP-BY-POLICY-PAC       VALUE '8'.
00220          88  PD-GROUP-BY-POLICY-CRDC      VALUE '9'.
00221          88  PD-GROUP-BY-BILL             VALUE '7' '8' '9'.
00222          88  PD-GROUP-BY-PROD             VALUE 'A'.
00223          88  PD-EFT-CHECKING              VALUE 'C'.
00224          88  PD-EFT-SAVINGS               VALUE 'S'.
00225      12  PD-DATE-PAID                 PIC  X(02).
00226      12  PD-LAST-BILLING-DATE         PIC  X(02).
00227      12  PD-LAST-BILL-TO-DATE         PIC  X(02).
00228      12  PD-MAX-MONTHS-BILL           PIC S9(03)  COMP-3.
00229      12  PD-PAID-TO-DATE              PIC  X(02).
00230      12  PD-PREV-BILLING-DATE         PIC  X(02).
00231      12  PD-PREV-BILL-TO-DATE         PIC  X(02).
00232
00233      12  FILLER                       PIC  X(20).
00234
00235 ******************************************************************
00236 *                PERSONAL DATA                                   *
00237 ******************************************************************
00238
00239      12  PD-ADDRS                     PIC  X(30).
00240      12  PD-CITY                      PIC  X(30).
00241      12  PD-CITY-CODE                 PIC  X(04).
00242      12  PD-COUNTY-CODE               PIC  X(03).
00243      12  PD-NAME                      PIC  X(30).
00244      12  PD-PARRISH-CODE              PIC  X(03).
00245      12  PD-PERSON                    PIC  X(30).
00246      12  PD-TEL-NO.
00247          16  PD-AREA-CODE             PIC  9(03).
00248          16  PD-TEL-PRE               PIC  9(03).
00249          16  PD-TEL-NBR               PIC  9(04).
00250      12  PD-ZIP.
00251          16  PD-ZIP-PRIME             PIC  X(05).
00252          16  PD-ZIP-PLUS4             PIC  X(04).
00253      12  PD-LANGUAGE-IND              PIC  X(01).
00254          88  PD-ENGLISH                          VALUE 'E'.
00255          88  PD-FRENCH                           VALUE 'F'.
00256          88  PD-SPANISH                          VALUE 'S'.
00257
00258      12  FILLER                       PIC  X(19).
00259
00260 ******************************************************************
00261 *                REINSURANCE DATA                                *
00262 ******************************************************************
00263
00264      12  PD-REINS-TBL-CODE            PIC  X(03).
00265      12  PD-REIN-RECALC               PIC  X(01).
00266
00267      12  PD-REI-AH-FEE                PIC S9(01)V9(04) COMP-3.
00268      12  PD-REI-AH-PE                 PIC  X(01).
00269      12  PD-REI-AH-TAX                PIC S9(01)V9(04) COMP-3.
00270
00271      12  PD-REI-GROUP-A               PIC  X(06).
00272      12  PD-REI-GROUP-B               PIC  X(06).
00273
00274      12  PD-REI-LF-FEE                PIC S9(01)V9(04) COMP-3.
00275      12  PD-REI-LF-PE                 PIC  X(01).
00276      12  PD-REI-LF-TAX                PIC S9(01)V9(04) COMP-3.
00277
00278      12  PD-REI-MORT                  PIC  X(04).
00279      12  PD-REI-PRT-OW                PIC  X(01).
00280      12  PD-REI-PRT-ST                PIC  X(01).
00281
00282      12  PD-REI-ADD-FEE               PIC S9(01)V9(04) COMP-3.
00283      12  PD-REI-ADD-PE                PIC  X(01).
00284      12  PD-REI-ADD-TAX               PIC S9(01)V9(04) COMP-3.
00285
00286      12  PD-REI-DIS-FEE               PIC S9(01)V9(04) COMP-3.
00287      12  PD-REI-DIS-PE                PIC  X(01).
00288      12  PD-REI-DIS-TAX               PIC S9(01)V9(04) COMP-3.
00289
00290      12  FILLER                       PIC  X(10).
00291 ******************************************************************
00292 *                RETRO DATA                                      *
00293 ******************************************************************
00294
00295      12  PD-RET-AH                    PIC S9(01)V9(04) COMP-3.
00296      12  PD-RET-GRP                   PIC  X(06).
00297      12  PD-RET-LF                    PIC S9(01)V9(04) COMP-3.
00298      12  PD-RET-MIN-LOSS-A            PIC SV9(03)      COMP-3.
00299      12  PD-RET-MIN-LOSS-L            PIC SV9(03)      COMP-3.
00300      12  PD-RET-P-E                   PIC  X(01).
00301      12  PD-RET-ST-TAX-USE            PIC  X(01).
00302          88  PD-CHARGE-ST-TAXES-ON-RETRO      VALUE 'Y' 'E' 'P'.
00303          88  PD-TAXES-NOT-IN-RETRO            VALUE 'N' ' '.
00304      12  PD-RET-Y-N                   PIC  X(01).
00305      12  PD-RET-ADD                   PIC S9(01)V9(04) COMP-3.
00306      12  PD-RET-MIN-LOSS-ADD          PIC SV9(03)      COMP-3.
00307      12  PD-RET-DIS                   PIC S9(01)V9(04) COMP-3.
00308      12  PD-RET-MIN-LOSS-DIS          PIC SV9(03)      COMP-3.
00309
00310      12  FILLER                       PIC  X(10).
00311
00312 ******************************************************************
00313 *                     MANAGEMENT OPTIONS                         *
00314 ******************************************************************
00315
00316      12  PD-DEFAULT-UNWTR-CODE        PIC  X(03).
00317      12  PD-LAPSE-NOTICE-CNTL         PIC  X(01).
00318      12  PD-CORRESPONDENCE-CNTL       PIC  X(01).
00319      12  PD-RETAIN-BILLING-DATA-MTHS  PIC S9(03)  COMP-3.
00320      12  PD-RETAIN-CLAIM-DATA-MTHS    PIC S9(03)  COMP-3.
00321      12  PD-RETAIN-COMMISSION-MTHS    PIC S9(03)  COMP-3.
00322      12  PD-RETAIN-DELINQUENCY-MTHS   PIC S9(03)  COMP-3.
00323      12  PD-RETAIN-INSD-PROFILE-MTHS  PIC S9(03)  COMP-3.
00324      12  PD-RETAIN-INS-COVERAGE-MTHS  PIC S9(03)  COMP-3.
00325      12  PD-RETAIN-STATUS-DISP-MTHS   PIC S9(03)  COMP-3.
00326      12  PD-NUM-BILLING-CYCLES-RETAINED
00327                                       PIC S9(03)  COMP-3.
00328      12  PD-RETAIN-UNDERWRITER-HST-MTHS
00329                                       PIC S9(03)  COMP-3.
00330
00331      12  FILLER                       PIC X(098).
00332
00333
00334 ******************************************************************
00335 *                MISCELLANEOUS DATA                              *
00336 ******************************************************************
00337
00338      12  PD-AH-RPT021-EXP-PCT         PIC S9(03)V9(04) COMP-3.
00339      12  PD-AUTO-REFUND-SW            PIC  X(01).
00340          88  PD-AUTO-REFUNDS-USED             VALUE 'Y'.
00341          88  PD-AUTO-REFUNDS-NOT-USED         VALUE 'N' ' '.
00342      12  PD-BUSINESS-TYPE             PIC  9(02).
00343      12  PD-CAL-TABLE                 PIC  X(02).
00344      12  PD-COMMENTS.
00345          16  PD-COMMENT-LINE          PIC  X(50)
00346                                            OCCURS 5 TIMES.
00347      12  PD-EMPLOYER-STMT-USED        PIC  X(01).
00348      12  PD-GROUPED-CHECKS-Y-N        PIC  X(01).
00349      12  PD-IG                        PIC  X(01).
00350          88  PD-HAS-INDIVIDUAL                VALUE 'I'
00351                                                     '1'.
00352          88  PD-HAS-GROUP                     VALUE 'G'
00353                                                     '2'.
00354      12  PD-LF-RPT021-EXP-PCT         PIC S9(03)V9(04) COMP-3.
00355      12  PD-REPORT-CODE-1             PIC  X(10).
00356      12  PD-REPORT-CODE-2             PIC  X(10).
00357      12  PD-RPT045A-SWITCH            PIC  X(01).
00358          88  PD-RPT045A-OFF                VALUE 'N'.
00359      12  PD-SPECIAL-BILLING-FREQ      PIC  X(01).
00360          88  PD-HAS-SPECIAL-BILL-FREQ         VALUE 'Y'.
00361          88  PD-NO-SPECIAL-BILL-FREQ          VALUE 'N' ' '.
00362      12  PD-STATUS                    PIC  X(01).
00363          88  PD-STATUS-ACTIVE                 VALUE '0'.
00364          88  PD-STATUS-INACTIVE               VALUE '1'.
00365      12  PD-STD-AH-TYPE               PIC  X(02).
00366      12  PD-TAX-NUMBER                PIC  X(11).
00367      12  PD-TOL-CLM                   PIC S9(03)V9(02) COMP-3.
00368      12  PD-USER-FIELDS.
00369          16  PD-USER-FLD-1            PIC  X(02).
00370          16  PD-USER-FLD-2            PIC  X(02).
00371          16  PD-USER-FLD-3            PIC  X(02).
00372          16  PD-USER-FLD-4            PIC  X(02).
00373          16  PD-USER-FLD-5            PIC  X(02).
00374      12  PD-USER-SELECT-OPTIONS.
00375          16  PD-USER-SELECT-1         PIC  X(10).
00376          16  PD-USER-SELECT-2         PIC  X(10).
00377          16  PD-USER-SELECT-3         PIC  X(10).
00378          16  PD-USER-SELECT-4         PIC  X(10).
00379          16  PD-USER-SELECT-5         PIC  X(10).
00380      12  PD-DIS-RPT021-EXP-PCT        PIC S9(03)V9(04) COMP-3.
00381      12  PD-ADD-RPT021-EXP-PCT        PIC S9(03)V9(04) COMP-3.
00382      12  FILLER                       PIC  X(20).
00383
00384 ******************************************************************
00385 *                CLIENT USE AREAS                                *
00386 ******************************************************************
00387
00388      12  PD-CLIENT-USE-AREA-1         PIC  X(30).
00389      12  PD-CLIENT-USE-AREA-2         PIC  X(30).
00390      12  PD-CLIENT-USE-AREA-3         PIC  X(11).
00391      12  PD-CLIENT-USE-AREA-4         PIC  X(30).
00392      12  PD-CLIENT-USE-AREA-5         PIC  X(30).
00393      12  PD-CLIENT-USE-AREA-6         PIC  X(11).
00394      12  PD-CLIENT-USE-AREA-7         PIC  X(30).
00395      12  PD-CLIENT-USE-AREA-8         PIC  X(30).
00396      12  PD-CLIENT-USE-AREA-9         PIC  X(11).
00397
00398 ******************************************************************
00399 *                TRANSFER DATA                                   *
00400 ******************************************************************
00401      12  PD-TRANSFERRED-FROM.
00402          16  PD-TRNFROM-CARRIER       PIC  X(01).
00403          16  PD-TRNFROM-GROUPING.
00404              20  PD-TRNFROM-GRP-PREFIX
00405                                       PIC  X(03).
00406              20  PD-TRNFROM-GRP-PRIME PIC  X(03).
00407          16  PD-TRNFROM-STATE         PIC  X(02).
00408          16  PD-TRNFROM-PRODUCER.
00409              20  PD-TRNFROM-PROD-PREFIX
00410                                       PIC  X(04).
00411              20  PD-TRNFROM-PROD-PRIME
00412                                       PIC  X(06).
00413          16  PD-TRNFROM-DATE          PIC  X(02).
00414      12  PD-TRANSFERRED-TO.
00415          16  PD-TRNTO-CARRIER         PIC  X(01).
00416          16  PD-TRNTO-GROUPING.
00417              20  PD-TRNTO-GRP-PREFIX  PIC  X(03).
00418              20  PD-TRNTO-GRP-PRIME   PIC  X(03).
00419          16  PD-TRNTO-STATE           PIC  X(02).
00420          16  PD-TRNTO-PRODUCER.
00421              20  PD-TRNTO-PROD-PREFIX PIC  X(04).
00422              20  PD-TRNTO-PROD-PRIME  PIC  X(06).
00423          16  PD-TRNTO-DATE            PIC  X(02).
00424      12  FILLER                       PIC  X(20).
00425
00426 ******************************************************************
00427 *                MORTGAGE PLANS SOLD                             *
00428 ******************************************************************
00429
00430      12  PD-PLANS-SOLD.
00431          16  PD-PRODUCER-PLANS  OCCURS 40 TIMES
00432                                 INDEXED BY PD-PLAN-NDX
00433                                            PD-PLAN-NDX2.
00434              20  PD-INDIVIDUAL-PLAN.
00435                  24  PD-PLAN-CODE     PIC  X(02).
00436                  24  PD-PLAN-REVISION PIC  X(03).
00437              20  PD-IBNR-PERCENT      PIC S9(01)V9(04) COMP-3.
00438      12  FILLER                       PIC  X(54).
00439
00440 ******************************************************************
00441 *                 AGENT AND COMMISSION DATA                      *
00442 ******************************************************************
00443
00444      12  PD-COMMISSION-INFORMATION.
00445          16  PD-REMIT-TO              PIC S9(03)   COMP-3.
00446          16  PD-RECALCULATION-SW      PIC  X(01).
00447              88  PD-RECALC-DETAIL             VALUE 'Y'.
00448              88  PD-RECALC-NO-DETAIL          VALUE 'I'.
00449              88  PD-IGNORE-RECALC             VALUE 'N'.
00450              88  PD-VALID-RECALCULATION-SW    VALUE 'Y' 'I' 'N'.
00451          16  PD-AGENT-DATA.
00452              20  PD-AGENT-ENTRY       OCCURS 5 TIMES
00453                                     INDEXED BY PD-AGENT-NDX
00454                                                PD-AGENT-NDX2.
00455                  24  PD-AGENT-NUMBER  PIC  X(10).
00456                  24  PD-AGENT-TYPE    PIC  X(01).
00457                      88  PD-AGENT-TYPE-A      VALUE 'C' 'D'.
00458                      88  PD-AGENT-TYPE-G      VALUE 'O' 'R'
00459                                                     'P' 'T'
00460                                                     'W'.
00461                      88  PD-AGENT-GROSS       VALUE 'C'.
00462                      88  PD-AGENT-REINS       VALUE 'R'.
00463                      88  PD-AGENT-GROSS-REINS VALUE 'D'.
00464                      88  PD-OVERWRITE-GROSS   VALUE 'O'.
00465                      88  PD-OVERWRITE-GROSS-REINS
00466                                           VALUE 'P'.
00467                      88  PD-OVERWRITE-REINS   VALUE 'T'.
00468                      88  PD-REINS-ONLY        VALUE 'W'.
00469                      88  PD-VALID-AGENT-TYPE  VALUE 'C' 'R'
00470                                                 'D' 'O' 'P'
00471                                                 'T' 'W'.
00472                  24  PD-COMMISSION-BILLED-PAID
00473                                       PIC  X(01).
00474                      88  PD-AGENT-BILLED      VALUE 'B'.
00475                      88  PD-AGENT-PAID        VALUE 'P'.
00476                  24  PD-COMP-RECALC-FLAG
00477                                       PIC  X(01).
00478                      88  PD-BYPASS-RECALC     VALUE 'N'.
00479                      88  PD-VALID-RECALC-FLAG VALUE ' ' 'N'.
00480      12  FILLER                       PIC  X(55).
00481
00482 ******************************************************************
00483 *                BANK DATA                                       *
00484 ******************************************************************
00485
00486      12  PD-BANK-ACCOUNT-NUMBER       PIC  X(20).
00487      12  PD-BANK-TRANSIT-NUMBER.
00488          16  PD-FEDERAL-NUMBER        PIC  X(04).
00489          16  PD-BANK-NUMBER           PIC  X(04).
00490      12  PD-CHARGE-CARD-EXP-DT        PIC  X(02).
00491      12  PD-CHARGE-CARD-TYPE          PIC  X(02).
00492          88  PD-AMERICAN-EXPRESS                 VALUE 'AE'.
00493          88  PD-CARTE-BLANCHE                    VALUE 'CB'.
00494          88  PD-DINERS-CLUB                      VALUE 'DN'.
00495          88  PD-DISCOVER                         VALUE 'DS'.
00496          88  PD-MASTER-CARD                      VALUE 'MC'.
00497          88  PD-VISA                             VALUE 'VI'.
00498      12  PD-SIGNATURE-NAME            PIC  X(25).
00499      12  PD-AUTHORIZATION-SW          PIC  X(01).
00500 ******************************************************************
00501 *                GENERIC FILLER                                  *
00502 ******************************************************************
00503
00504      12  PD-DATE-TEST                 PIC S9(08) COMP.
00505      12  FILLER                       PIC  X(62).
00506
00507 ******************************************************************
00495
00496      EJECT
00497 *    COPY MPCPLAN.
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
00498
00499      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA CHECK-QUE
                                CONTROL-FILE ACTIVITY-TRAILERS
                                CLAIM-MASTER CERTIFICATE-MASTER
                                ACCOUNT-MASTER BENEFICIARY-MASTER
                                COMPENSATION-MASTER POLICY-MASTER
                                PRODUCER-MASTER PRODUCER-PLANS.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL176' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00501
00502      MOVE EIBDATE               TO DC-JULIAN-YYDDD.
00503      MOVE '5'                   TO DC-OPTION-CODE.
00504      PERFORM 8500-DATE-CONVERSION.
00505      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.
00506      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE
00507                                     WS-CHECK-WRITER-DATE.
00508
00509      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.
00510
00511 *    NOTE *******************************************************
00512 *         *      ACCESS TO THIS MODULE CAN BE ONLY FROM AN XCTL *
00513 *         *  FROM ANOTHER MODULE.                               *
00514 *         *******************************************************.
00515
00516      IF EIBCALEN NOT GREATER THAN ZERO
00517          MOVE UNACCESS-MSG       TO  LOGOFF-MSG
00518          GO TO 8300-SEND-TEXT.
00519
00520      
      * EXEC CICS HANDLE CONDITION
00521 *        PGMIDERR   (9600-PGMIDERR)
00522 *        NOTOPEN    (8800-NOT-OPEN)
00523 *        NOTFND     (0180-MAIN-LOGIC)
00524 *        ENDFILE    (0190-MAIN-LOGIC)
00525 *        TERMIDERR  (0900-TERMIDERR)
00526 *        ENQBUSY    (0910-ENQ-BUSY)
00527 *        ERROR      (9990-ERROR)
00528 *    END-EXEC.
      *    MOVE '"$LJI''[).             ! " #00006767' TO DFHEIV0
           MOVE X'22244C4A49275B292E202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303036373637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00529
00530      MOVE +3                     TO  EMI-NUMBER-OF-LINES.
00531      MOVE +2                     TO  EMI-SWITCH2.
00532
00533      EJECT
00534  0010-MAIN-LOGIC.
00535      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00536          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
00537              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-6
00538              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-5
00539              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-4
00540              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-3
00541              MOVE PI-SAVED-PROGRAM-1   TO  PI-SAVED-PROGRAM-2
00542              MOVE PI-RETURN-TO-PROGRAM TO  PI-SAVED-PROGRAM-1
00543              MOVE PI-CALLING-PROGRAM   TO  PI-RETURN-TO-PROGRAM
00544              MOVE THIS-PGM             TO  PI-CALLING-PROGRAM
00545            ELSE
00546              MOVE PI-RETURN-TO-PROGRAM TO  PI-CALLING-PROGRAM
00547              MOVE PI-SAVED-PROGRAM-1   TO  PI-RETURN-TO-PROGRAM
00548              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-1
00549              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-2
00550              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-3
00551              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-4
00552              MOVE PI-SAVED-PROGRAM-6   TO  PI-SAVED-PROGRAM-5
00553              MOVE SPACES               TO  PI-SAVED-PROGRAM-6
00554        ELSE
00555          GO TO 0040-MAIN-LOGIC.
00556
00557  0015-MAIN-LOGIC.
00558 *    NOTE *******************************************************
00559 *         *     INITIALIZE THE WORK FIELDS FOR THE PROGRAM      *
00560 *         *  INTERFACE BLOCK FOR THIS MODULE.                   *
00561 *         *******************************************************.
00562
00563      MOVE SPACES                 TO  PI-PROGRAM-WORK-AREA.
00564
00565      MOVE ZEROS                  TO  PI-NUMBER-OF-CONTROL-GROUPS
00566                                      PI-NUMBER-OF-ALIGNMENT-CHECKS
00567                                      PI-ALIGNMENT-CONTROL-GROUP
00568                                      PI-ALIGNMENT-SEQUENCE-NO
00569                                      PI-PROCESSING-SW
00570                                      PI-CONTROL-GROUP (1)
00571                                      PI-CONTROL-GROUP (2)
00572                                      PI-CONTROL-GROUP (3)
00573                                      PI-CONTROL-GROUP (4)
00574                                      PI-HIGH-SEQUENCE (1)
00575                                      PI-HIGH-SEQUENCE (2)
00576                                      PI-HIGH-SEQUENCE (3)
00577                                      PI-HIGH-SEQUENCE (4)
00578                                      PI-COMPANY-ZIP-CODE
00579                                      PI-COMPANY-PHONE-NUMBER.
00580
00581      MOVE WS-PRINTER-STARTED-SW  TO  PI-PRINTER-STARTED-SW.
00582      MOVE WS-TEMP-STORAGE-KEY    TO  PI-TEMP-STORAGE-KEY.
00583
00584      MOVE LOW-VALUES             TO  EL176AI.
00585      MOVE -1                     TO  AOPTIONL.
00586
00587      PERFORM 8100-SEND-INITIAL-MAP.
00588
CIDMOD     MOVE 'N' TO MICR-CHK-SW.
CIDMOD
CIDMOD     EJECT
CIDMOD
CIDMOD
CIDMOD 0030-CHECK-MICR.
CIDMOD**   ----------------------------------------
CIDMOD**   --> IS.MICR.DRAFTS HANDLE CONDITIONS
CIDMOD**   ----------------------------------------
CIDMOD
CIDMOD     
      * EXEC CICS HANDLE CONDITION
CIDMOD*        NOTFND   (0030-MICR-CHK-SW)
CIDMOD*        NOTOPEN  (5000-MICR-CLOSED)
CIDMOD*        DISABLED (5000-MICR-CLOSED) END-EXEC.
      *    MOVE '"$IJc                 ! # #00006846' TO DFHEIV0
           MOVE X'2224494A6320202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303036383436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
CIDMOD
CSODJN*    MOVE '420C00001CCCCCCCCCC' TO WS-MICR-KEY.
070104     MOVE 'DCC2000000000000000' TO WS-MICR-KEY.
CIDMOD
CIDMOD     
      * EXEC CICS READ
CIDMOD*        DATASET ('MICRDRFT')
CIDMOD*        LENGTH  (REC-LGTH)
CIDMOD*        RIDFLD  (WS-MICR-KEY)
CIDMOD*        INTO (CSO-DRAFT-420C)
CIDMOD*        GTEQ
CIDMOD*    END-EXEC.
           MOVE 'MICRDRFT' TO DFHEIV1
      *    MOVE '&"IL       G          (   #00006854' TO DFHEIV0
           MOVE X'2622494C2020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036383534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CSO-DRAFT-420C, 
                 REC-LGTH, 
                 WS-MICR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
CIDMOD
CIDMOD 0030-MICR-CHK-SW.
CIDMOD
CIDMOD     MOVE 'Y' TO  MICR-CHK-SW.
CIDMOD
CIDMOD 0030-MICR-EXIT.
CIDMOD     EXIT.
CIDMOD**   ----------------------------------------
CIDMOD
CIDMOD
00590  0040-MAIN-LOGIC.
00591 *    NOTE *******************************************************
00592 *         *  AFTER THE FIRST TIME THROUGH THE PROPER ATTENTION  *
00593 *         *  KEY USAGE NEEDS TO BE CHECKED FOR VALIDITY         *
00594 *         *  BEFORE ANY FURTHER PROCESSING CAN BE DONE.         *
00595 *         *******************************************************.
00596
00597      IF EIBAID = DFHCLEAR
00598          GO TO 9400-CLEAR.
00599
00600      IF PI-PROCESSOR-ID = 'LGXX'
00601          NEXT SENTENCE
00602      ELSE
00603          
      * EXEC CICS READQ TS
00604 *            QUEUE   (PI-SECURITY-TEMP-STORE-ID)
00605 *            INTO    (SECURITY-CONTROL)
00606 *            LENGTH  (SC-COMM-LENGTH)
00607 *            ITEM    (SC-ITEM)
00608 *        END-EXEC
      *    MOVE '*$II   L              ''   #00006884' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036383834' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00609          MOVE SC-CLAIMS-DISPLAY (13)   TO  PI-DISPLAY-CAP
00610          MOVE SC-CLAIMS-UPDATE  (13)   TO  PI-MODIFY-CAP
00611          IF NOT MODIFY-CAP
00612              MOVE 'UPDATE'             TO  SM-READ
00613              PERFORM 9995-SECURITY-VIOLATION
00614              MOVE ER-0070              TO  EMI-ERROR
00615              GO TO 8100-SEND-INITIAL-MAP.
00616
00617      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00618          MOVE LOW-VALUES         TO  EL176AI
00619          MOVE ER-0008               TO  EMI-ERROR
00620          MOVE -1                 TO  APFKL
00621          PERFORM 8200-SEND-DATAONLY.
00622
00623      
      * EXEC CICS RECEIVE
00624 *        INTO   (EL176AI)
00625 *        MAPSET (WS-MAPSET-NAME)
00626 *        MAP    (WS-MAP-NAME)
00627 *    END-EXEC.
           MOVE LENGTH OF
            EL176AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00006904' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036393034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL176AI, 
                 DFHEIV11, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00628
00629      IF APFKL IS GREATER THAN ZERO
00630          IF EIBAID NOT = DFHENTER
00631              MOVE ER-0004        TO  EMI-ERROR
00632              MOVE AL-UNBOF       TO  APFKA
00633              MOVE -1             TO  APFKL
00634              PERFORM 8200-SEND-DATAONLY
00635            ELSE
00636              IF APFKO IS NUMERIC
00637                AND APFKO IS GREATER THAN ZERO
00638                AND APFKO IS LESS THAN '25'
00639                  MOVE PF-VALUES (APFKI)  TO  EIBAID
00640                ELSE
00641                  MOVE ER-0029        TO  EMI-ERROR
00642                  MOVE AL-UNBOF       TO  APFKA
00643                  MOVE -1             TO  APFKL
00644                  PERFORM 8200-SEND-DATAONLY.
00645
00646      IF EIBAID = DFHPF12
00647          MOVE 'EL010'            TO  THIS-PGM
00648          GO TO 9300-XCTL.
00649
00650      IF EIBAID = DFHPF23
00651          GO TO 9000-RETURN-CICS.
00652
00653      IF EIBAID = DFHPF24
00654          MOVE 'EL126'            TO  THIS-PGM
00655          GO TO 9300-XCTL.
00656
00657      IF EIBAID NOT = DFHENTER
00658          MOVE ER-0008               TO  EMI-ERROR
00659          MOVE -1                 TO  APFKL
00660          PERFORM 8200-SEND-DATAONLY.
00661
00662      IF PI-PROCESSING-SW NOT = ZERO
00663          GO TO 0240-MAIN-LOGIC.
00664
00665      EJECT
00666  0100-MAIN-LOGIC.
00667 *    NOTE *******************************************************
00668 *         *          SYNTAX CHECK THE MAP FIELDS                *
00669 *         *******************************************************.
00670
CIDMOD
CIDMOD     IF MICR-CHK-SW = 'Y'
CIDMOD         CONTINUE
CIDMOD     ELSE
CIDMOD         PERFORM 0030-CHECK-MICR THRU 0030-MICR-EXIT
CIDMOD     END-IF.
CIDMOD
00671      IF AOPTIONL NOT GREATER THAN ZERO
00672          MOVE -1                    TO  AOPTIONL
00673          MOVE AL-UNBON              TO  AOPTIONA
00674          MOVE ER-0002               TO  EMI-ERROR
00675          PERFORM 8200-SEND-DATAONLY.
00676
00677      IF (AOPTIONI GREATER THAN ZERO AND
00678          AOPTIONI LESS THAN '4')
00679        OR
00680          (PI-COMPANY-ID = ('POS' OR 'WSL' OR 'MLI') AND
00681           AOPTIONI = ('2' OR '3'))
00682              MOVE AL-UNNON           TO  AOPTIONA
00683            ELSE
00684              MOVE -1                 TO  AOPTIONL
00685              MOVE AL-UNBON           TO  AOPTIONA
00686              MOVE ER-0330            TO  EMI-ERROR
00687              PERFORM 9900-ERROR-FORMAT.
00688
00689      IF AALIGNL GREATER THAN ZERO
00690          IF AALIGNI IS NUMERIC
00691              MOVE AALIGNO      TO  PI-NUMBER-OF-ALIGNMENT-CHECKS
00692              MOVE AL-UNNON     TO  AALIGNA
00693            ELSE
00694              MOVE ER-0365      TO  EMI-ERROR
00695              MOVE -1           TO  AALIGNL
00696              MOVE AL-UNBON     TO  AALIGNA
00697              PERFORM 9900-ERROR-FORMAT.
00698
00699      IF ACKNOL GREATER THAN ZERO
00700          IF ACKNOI IS NUMERIC
00701              MOVE AL-UNNON       TO  ACKNOA
00702              MOVE ACKNOI         TO  WS-CHECK-NUMBER-X
00703            ELSE
00704              MOVE ER-0366        TO  EMI-ERROR
00705              MOVE -1             TO  ACKNOL
00706              MOVE AL-UNBON       TO  ACKNOA
00707              PERFORM 9900-ERROR-FORMAT
00708      ELSE
00709          MOVE ZEROS              TO  WS-CHECK-NUMBER.
00710
00711      IF AACNL GREATER THAN ZERO
00712          MOVE AACNI              TO  PI-ASSIGN-CHECK-NUMBERS
00713          IF AACNI = 'Y' OR 'N'
00714              MOVE AL-UANON       TO  AACNA
00715              MOVE AACNI          TO  PI-ENTRY-CD-1
00716            ELSE
00717              MOVE AL-UABON       TO  AACNA
00718              MOVE -1             TO  AACNL
00719              MOVE ER-0367        TO  EMI-ERROR
00720              PERFORM 9900-ERROR-FORMAT
00721        ELSE
00722          MOVE AL-UABOF           TO  AACNA
00723          MOVE -1                 TO  AACNL
00724          MOVE ER-0368            TO  EMI-ERROR
00725          PERFORM 9900-ERROR-FORMAT.
00726
00727      IF AACNI = 'Y'
00728        AND ACKNOL NOT GREATER THAN ZERO
00729          MOVE -1                 TO  ACKNOL
00730          MOVE AL-UNBOF           TO  ACKNOA
00731          MOVE ER-0392            TO  EMI-ERROR
00732          PERFORM 9900-ERROR-FORMAT.
00733
00734      IF AACNI = 'N'
00735        AND ACKNOL GREATER THAN ZERO
00736          MOVE -1                 TO  ACKNOL
00737          MOVE AL-UNBON           TO  ACKNOA
00738          MOVE ER-0393            TO  EMI-ERROR
00739          PERFORM 9900-ERROR-FORMAT.
00740
00741      EJECT
00742 *    NOTE *******************************************************
00743 *         *      CHECK THE VALIDITY OF ANY CONTROL GROUPS       *
00744 *         *  ENTERED.                                           *
00745 *         *******************************************************.
00746
00747      SET EL176A-INDEX
00748          PI-INDEX TO +1.
00749
00750  0120-MAIN-LOGIC.
00751
00752      IF PI-COMPANY-ID = 'DMD'
00753          GO TO 0150-DMD-CNTL-GRP-EDIT.
00754
00755      IF EL176A-CONTROL-GROUP-LENGTH (EL176A-INDEX)
00756                                  NOT GREATER THAN ZERO
00757          MOVE AL-UNNOF  TO  EL176A-CONTROL-GROUP-ATTRB
00758                                                     (EL176A-INDEX)
00759          GO TO 0190-MAIN-LOGIC.
00760
00761      IF EL176A-CONTROL-GROUP (EL176A-INDEX) IS NOT NUMERIC
00762          MOVE AL-UNBON  TO  EL176A-CONTROL-GROUP-ATTRB
00763                                                     (EL176A-INDEX)
00764          MOVE -1  TO  EL176A-CONTROL-GROUP-LENGTH (EL176A-INDEX)
00765          MOVE ER-0369               TO  EMI-ERROR
00766          PERFORM 9900-ERROR-FORMAT
00767          GO TO 0190-MAIN-LOGIC.
00768
00769      MOVE EL176A-CONTROL-GROUP (EL176A-INDEX)
00770                                  TO  PI-CONTROL-GROUP (PI-INDEX).
00771      SET PI-INDEX UP BY +1.
00772      MOVE AL-UNNON  TO  EL176A-CONTROL-GROUP-ATTRB (EL176A-INDEX).
00773
00774      IF PI-INDEX IS GREATER THAN +2
00775        AND PI-CONTROL-GROUP (PI-INDEX - 2)
00776                      NOT LESS THAN PI-CONTROL-GROUP (PI-INDEX - 1)
00777          MOVE ER-0385               TO  EMI-ERROR
00778          PERFORM 9900-ERROR-FORMAT
00779          MOVE -1 TO EL176A-CONTROL-GROUP-LENGTH (EL176A-INDEX)
00780          MOVE AL-UNBON TO EL176A-CONTROL-GROUP-ATTRB
00781                                                    (EL176A-INDEX).
00782
00783  0125-MAIN-LOGIC.
00784
00785      MOVE ZERO                   TO  WS-NOT-FOUND.
00786
00787      MOVE LOW-VALUES             TO  WS-CHECK-AIX-KEY.
00788
00789      MOVE PI-COMPANY-CD          TO  WS-CQK-COMPANY-CD-A1.
00790
00791      IF PI-COMPANY-ID NOT = 'DMD'
00792          MOVE EL176A-CONTROL-GROUP (EL176A-INDEX)
00793                                  TO  WS-CQK-CONTROL-NUMBER-A1
00794          MOVE +5                 TO  WS-BROWSE-LENGTH
00795      ELSE
00796          MOVE +1                 TO  WS-BROWSE-LENGTH.
00797
00798      MOVE 'CHKQ'                 TO  FILE-SWITCH.
00799
00800      IF WS-CHECK-QUEUE-BROWSE-SW = ZERO
00801          
      * EXEC CICS STARTBR
00802 *            DATASET   (WS-CHECK-QUEUE-AIX-DSID)
00803 *            RIDFLD    (WS-CHECK-AIX-KEY)
00804 *            GENERIC   EQUAL
00805 *            KEYLENGTH (WS-BROWSE-LENGTH)
00806 *        END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,   KG    E          &   #00007089' TO DFHEIV0
           MOVE X'262C2020204B472020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303037303839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-AIX-DSID, 
                 WS-CHECK-AIX-KEY, 
                 WS-BROWSE-LENGTH, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00807          MOVE +1                 TO  WS-CHECK-QUEUE-BROWSE-SW
00808        ELSE
00809          
      * EXEC CICS RESETBR
00810 *            DATASET   (WS-CHECK-QUEUE-AIX-DSID)
00811 *            RIDFLD    (WS-CHECK-AIX-KEY)
00812 *            GENERIC   EQUAL
00813 *            KEYLENGTH (5)
00814 *        END-EXEC.
           MOVE 5
             TO DFHEIV11
           MOVE 0
             TO DFHEIV12
      *    MOVE '&4   KG    E          &   #00007097' TO DFHEIV0
           MOVE X'26342020204B472020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303037303937' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-AIX-DSID, 
                 WS-CHECK-AIX-KEY, 
                 DFHEIV11, 
                 DFHEIV12, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00815
00816  0130-MAIN-LOGIC.
00817      
      * EXEC CICS READNEXT
00818 *        DATASET (WS-CHECK-QUEUE-AIX-DSID)
00819 *        RIDFLD  (WS-CHECK-AIX-KEY)
00820 *        SET     (ADDRESS OF CHECK-QUE)
00821 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00007105' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303037313035' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-AIX-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CHECK-AIX-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CHECK-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00822
00823      IF WS-CQK-COMPANY-CD-A1 NOT = PI-COMPANY-CD
00824          GO TO 0170-MAIN-LOGIC.
00825
00826      IF PI-COMPANY-ID NOT = 'DMD'
00827          IF WS-CQK-CONTROL-NUMBER-A1 NOT = EL176A-CONTROL-GROUP
00828                                                     (EL176A-INDEX)
00829              GO TO 0170-MAIN-LOGIC
00830          ELSE
00831              NEXT SENTENCE
00832      ELSE
00833          IF CQ-CONTROL-NUMBER NOT = PI-CONTROL-GROUP(1) AND
00834             CQ-CONTROL-NUMBER NOT = PI-CONTROL-GROUP(2) AND
00835             CQ-CONTROL-NUMBER NOT = PI-CONTROL-GROUP(3) AND
00836             CQ-CONTROL-NUMBER NOT = PI-CONTROL-GROUP(4)
00837                GO TO 0130-MAIN-LOGIC.
00838
00839      IF CQ-ENTRY-TYPE NOT = 'Q'
00840          GO TO 0130-MAIN-LOGIC.
00841
00842      IF NOT PI-NO-CARRIER-SECURITY
00843         IF CQ-CARRIER NOT = PI-CARRIER-SECURITY
00844            MOVE ER-2370 TO EMI-ERROR
00845            PERFORM 9900-ERROR-FORMAT
00846            MOVE -1 TO EL176A-CONTROL-GROUP-LENGTH (EL176A-INDEX)
00847            MOVE AL-UNBON TO EL176A-CONTROL-GROUP-ATTRB
00848                                               (EL176A-INDEX)
00849            GO TO 0190-MAIN-LOGIC.
00850
00851      IF PI-COMPANY-ID = 'POS' OR 'TAO' OR 'CSL'
00852          MOVE CQ-CARRIER         TO  PI-CARRIER.
00853
00854      IF AOPTIONI = '2'
00855        AND CQ-TIMES-PRINTED NOT GREATER THAN ZERO
00856          GO TO 0190-MAIN-LOGIC.
00857
00858      IF AOPTIONI = '3'
00859        AND CQ-TIMES-PRINTED GREATER THAN ZERO
00860          GO TO 0190-MAIN-LOGIC.
00861
00862      GO TO 0130-MAIN-LOGIC.
00863
00864      EJECT
00865
00866 *    NOTE *******************************************************
00867 *         *  DMD  - CHECK CONTROL GROUP REQUESTS                *
00868 *         *******************************************************.
00869
00870  0150-DMD-CNTL-GRP-EDIT.
00871
00872      IF AOPTIONI = 1
00873          GO TO 0190-MAIN-LOGIC.
00874 *****************************************************************
00875 ***       IF PROCESSING BY CONTROL GROUP DETERMINE THAT THERE   *
00876 ***       IS A LEAST ONE CONTROL GROUP ENTERED.                 *
00877 *****************************************************************
00878
00879      IF EL176A-CONTROL-GROUP-LENGTH (1)
00880                              NOT GREATER THAN ZERO AND
00881         EL176A-CONTROL-GROUP-LENGTH (2)
00882                              NOT GREATER THAN ZERO AND
00883         EL176A-CONTROL-GROUP-LENGTH (3)
00884                              NOT GREATER THAN ZERO AND
00885         EL176A-CONTROL-GROUP-LENGTH (4)
00886                              NOT GREATER THAN ZERO
00887          MOVE -1                 TO  ACG01L
00888          MOVE AL-UNBOF  TO  ACG01A ACG02A ACG03A ACG04A
00889          MOVE ER-0370            TO  EMI-ERROR
00890          PERFORM 9900-ERROR-FORMAT
00891          GO TO 0190-MAIN-LOGIC.
00892
00893 *****************************************************************
00894 ***       DETERMINE THAT CONTROL GROUPS REQUESTED ARE NUMERIC   *
00895 *****************************************************************
00896
00897      IF EL176A-CONTROL-GROUP-LENGTH (1)
00898                              GREATER THAN ZERO AND
00899         EL176A-CONTROL-GROUP (1) NOT NUMERIC
00900              MOVE AL-UNBON    TO  EL176A-CONTROL-GROUP-ATTRB (1)
00901              MOVE -1          TO  EL176A-CONTROL-GROUP-LENGTH (1)
00902              MOVE ER-0369     TO  EMI-ERROR
00903              PERFORM 9900-ERROR-FORMAT
00904              GO TO 0190-MAIN-LOGIC.
00905
00906      IF EL176A-CONTROL-GROUP-LENGTH (2)
00907                              GREATER THAN ZERO AND
00908         EL176A-CONTROL-GROUP (2) NOT NUMERIC
00909              MOVE AL-UNBON    TO  EL176A-CONTROL-GROUP-ATTRB (2)
00910              MOVE -1          TO  EL176A-CONTROL-GROUP-LENGTH (2)
00911              MOVE ER-0369     TO  EMI-ERROR
00912              PERFORM 9900-ERROR-FORMAT
00913              GO TO 0190-MAIN-LOGIC.
00914
00915      IF EL176A-CONTROL-GROUP-LENGTH (3)
00916                              GREATER THAN ZERO AND
00917         EL176A-CONTROL-GROUP (3) NOT NUMERIC
00918              MOVE AL-UNBON    TO  EL176A-CONTROL-GROUP-ATTRB (3)
00919              MOVE -1          TO  EL176A-CONTROL-GROUP-LENGTH (3)
00920              MOVE ER-0369     TO  EMI-ERROR
00921              PERFORM 9900-ERROR-FORMAT
00922              GO TO 0190-MAIN-LOGIC.
00923
00924      IF EL176A-CONTROL-GROUP-LENGTH (4)
00925                              GREATER THAN ZERO AND
00926         EL176A-CONTROL-GROUP (4) NOT NUMERIC
00927              MOVE AL-UNBON    TO  EL176A-CONTROL-GROUP-ATTRB (4)
00928              MOVE -1          TO  EL176A-CONTROL-GROUP-LENGTH (4)
00929              MOVE ER-0369     TO  EMI-ERROR
00930              PERFORM 9900-ERROR-FORMAT
00931              GO TO 0190-MAIN-LOGIC.
00932
00933 *****************************************************************
00934 ***       DETERMINE THAT CONTROL GROUPS REQUESTED ARE IN        *
00935 ***       SEQUENCE.                                             *
00936 *****************************************************************
00937
00938      IF EL176A-CONTROL-GROUP-LENGTH (2)
00939                              GREATER THAN ZERO AND
00940         EL176A-CONTROL-GROUP (2) NOT GREATER THAN
00941                                  EL176A-CONTROL-GROUP (1)
00942          MOVE ER-0385            TO  EMI-ERROR
00943          PERFORM 9900-ERROR-FORMAT
00944          MOVE -1              TO EL176A-CONTROL-GROUP-LENGTH (2)
00945          MOVE AL-UNBON        TO EL176A-CONTROL-GROUP-ATTRB (2)
00946          GO TO 0190-MAIN-LOGIC.
00947
00948      IF EL176A-CONTROL-GROUP-LENGTH (3)
00949                              GREATER THAN ZERO AND
00950         EL176A-CONTROL-GROUP (3) NOT GREATER THAN
00951                                  EL176A-CONTROL-GROUP (2)
00952          MOVE ER-0385            TO  EMI-ERROR
00953          PERFORM 9900-ERROR-FORMAT
00954          MOVE -1              TO EL176A-CONTROL-GROUP-LENGTH (3)
00955          MOVE AL-UNBON        TO EL176A-CONTROL-GROUP-ATTRB (3)
00956          GO TO 0190-MAIN-LOGIC.
00957
00958      IF EL176A-CONTROL-GROUP-LENGTH (4)
00959                              GREATER THAN ZERO AND
00960         EL176A-CONTROL-GROUP (4) NOT GREATER THAN
00961                                  EL176A-CONTROL-GROUP (3)
00962          MOVE ER-0385            TO  EMI-ERROR
00963          PERFORM 9900-ERROR-FORMAT
00964          MOVE -1              TO EL176A-CONTROL-GROUP-LENGTH (4)
00965          MOVE AL-UNBON        TO EL176A-CONTROL-GROUP-ATTRB (4)
00966          GO TO 0190-MAIN-LOGIC.
00967
00968 *****************************************************************
00969 ***       MOVE REQUESTED CONTROL GROUPS TO THE PI AREA WITH     *
00970 ***       ANY SKIP CONTROL FIELDS COMPRESSED.                   *
00971 *****************************************************************
00972
00973      MOVE +0                     TO  WS-DMD-CNTL-GRP-COUNT.
00974      SET EL176A-INDEX TO +1.
00975
00976  0150-CNTL-LOOP.
00977
00978      IF WS-DMD-CNTL-GRP-COUNT GREATER THAN +2
00979          GO TO 0150-END-CNTL-LOOP.
00980
00981      IF EL176A-CONTROL-GROUP-LENGTH (EL176A-INDEX)
00982                                     NOT GREATER THAN ZERO
00983          MOVE EL176A-CONTROL-GROUP (EL176A-INDEX + 1)
00984                                  TO  EL176A-CONTROL-GROUP
00985                                      (EL176A-INDEX)
00986          MOVE SPACES             TO EL176A-CONTROL-GROUP-X
00987                                      (EL176A-INDEX + 1).
00988      SET EL176A-INDEX UP BY +1.
00989      ADD +1  TO  WS-DMD-CNTL-GRP-COUNT.
00990      GO TO 0150-CNTL-LOOP.
00991
00992  0150-END-CNTL-LOOP.
00993
00994      MOVE EL176A-CONTROL-GROUP (1)  TO  PI-CONTROL-GROUP (1).
00995      MOVE EL176A-CONTROL-GROUP (2)  TO  PI-CONTROL-GROUP (2).
00996      MOVE EL176A-CONTROL-GROUP (3)  TO  PI-CONTROL-GROUP (3).
00997      MOVE EL176A-CONTROL-GROUP (4)  TO  PI-CONTROL-GROUP (4).
00998
00999      GO TO 0125-MAIN-LOGIC.
01000
01001  0170-MAIN-LOGIC.
01002      MOVE ER-0394                   TO  EMI-ERROR.
01003      PERFORM 9900-ERROR-FORMAT.
01004      MOVE -1 TO EL176A-CONTROL-GROUP-LENGTH (EL176A-INDEX).
01005      MOVE AL-UNBON TO EL176A-CONTROL-GROUP-ATTRB (EL176A-INDEX).
01006
01007      GO TO 0190-MAIN-LOGIC.
01008
01009  0180-MAIN-LOGIC.
01010      MOVE ER-0387                   TO  EMI-ERROR.
01011      PERFORM 9900-ERROR-FORMAT.
01012      MOVE -1 TO EL176A-CONTROL-GROUP-LENGTH (EL176A-INDEX).
01013      MOVE AL-UNBON TO EL176A-CONTROL-GROUP-ATTRB (EL176A-INDEX).
01014
01015  0190-MAIN-LOGIC.
01016
01017      IF PI-COMPANY-ID = 'DMD'
01018          NEXT SENTENCE
01019      ELSE
01020          IF EL176A-INDEX LESS THAN +4
01021              SET EL176A-INDEX UP BY +1
01022              GO TO 0120-MAIN-LOGIC.
01023
01024      IF WS-CHECK-QUEUE-BROWSE-SW NOT = ZERO
01025          MOVE ZERO               TO  WS-CHECK-QUEUE-BROWSE-SW
01026          
      * EXEC CICS ENDBR
01027 *            DATASET (WS-CHECK-QUEUE-AIX-DSID)
01028 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00007314' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303037333134' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-AIX-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01029
01030      IF EMI-FATAL-CTR GREATER THAN ZERO
01031          PERFORM 8200-SEND-DATAONLY.
01032
01033      IF PI-COMPANY-ID = 'DMD'
01034          GO TO 0200-MAIN-LOGIC.
01035
01036      IF AOPTIONI = ('2' OR '3')
01037        AND PI-INDEX NOT GREATER THAN +1
01038          MOVE -1                 TO  ACG01L
01039          MOVE AL-UNBOF  TO  ACG01A ACG02A ACG03A ACG04A
01040          MOVE ER-0370               TO  EMI-ERROR
01041          PERFORM 9900-ERROR-FORMAT.
01042
01043      IF PI-INDEX GREATER THAN +1
01044          NEXT SENTENCE
01045        ELSE
01046          GO TO 0200-MAIN-LOGIC.
01047
01048      SET PI-INDEX DOWN BY +1.
01049      SET PI-NUMBER-OF-CONTROL-GROUPS TO PI-INDEX.
01050      SET PI-INDEX
01051          EL176A-INDEX TO +1.
01052
01053  0195-MAIN-LOGIC.
01054      IF PI-CONTROL-GROUP (PI-INDEX) GREATER THAN ZERO
01055          MOVE PI-CONTROL-GROUP (PI-INDEX)
01056                      TO  EL176A-CONTROL-GROUP (EL176A-INDEX)
01057          MOVE AL-UNNON TO EL176A-CONTROL-GROUP-ATTRB
01058                                                     (EL176A-INDEX)
01059      ELSE
01060          MOVE SPACES TO  EL176A-CONTROL-GROUP-X (EL176A-INDEX)
01061          MOVE AL-UNNOF TO EL176A-CONTROL-GROUP-ATTRB
01062                                                    (EL176A-INDEX).
01063
01064      IF PI-INDEX LESS THAN +4
01065          SET PI-INDEX
01066              EL176A-INDEX UP BY +1
01067          GO TO 0195-MAIN-LOGIC.
01068
01069      SET EL176A-INDEX
01070          PI-INDEX TO +1.
01071
01072      EJECT
01073  0200-MAIN-LOGIC.
01074 *    NOTE *******************************************************
01075 *         *      ALL OF THE SYNTAX CHECKS HAVE BEEN SUCCESSFUL. *
01076 *         *  NOW DO THE PRE-EDIT.                               *
01077 *         *                                                     *
01078 *         *      BEFORE A CHECK BATCH IS QUEUED FOR PRINT, A    *
01079 *         *  PRE-EDIT IS DONE TO ASSURE CONSISTENCY IN          *
01080 *         *  PROCESSING.  THIS EDIT CONSISTS OF THE FOLLOWING   *
01081 *         *  STEPS:                                             *
01082 *         *                                                     *
01083 *         *  1. IF A STARTING CHECK NUMBER HAS BEEN ENTERED,    *
01084 *         *     THE NUMBER IS COMPARED TO OTHER NUMBERS IN THE  *
01085 *         *     FILE FOR OVERLAPS AND GAPS.                     *
01086 *         *                                                     *
01087 *         *  2. IF ANY CHECKS IN THE RELEASED GROUPS HAVE       *
01088 *         *     ALREADY BEEN QUEUED FOR PRINT OR PRINTED.       *
01089 *         *                                                     *
01090 *         *  3. IF PRE-NUMBERING IS NOT USED THAT ALL CHECKS    *
01091 *         *     HAVE A CHECK NUMBER ASSIGNED.                   *
01092 *         *                                                     *
01093 *         *  4. IF DUPLICATE CHECK NUMBERS ARE ASSIGNED.        *
01094 *         *                                                     *
01095 *         *      BEFORE A CHECK BATCH IS QUEUED FOR RE-PRINT, A *
01096 *         *  PRE-EDIT IS DONE TO ASSURE CONSISTENCY IN          *
01097 *         *  PROCESSING.  THIS EDIT CONSISTS OF THE FOLLOWING   *
01098 *         *  STEPS:                                             *
01099 *         *                                                     *
01100 *         *  1. ALL CHECKS IN THE INDICATED GROUP(S) MUST HAVE  *
01101 *         *     BEEN PREVIOUSLY PRINTED.                        *
01102 *         *                                                     *
01103 *         *  2. IF THE PRE-NUMBERING SWITCH IS SET IN ANY RECORD*
01104 *         *     IT MUST BE SET IN ALL RECORDS.                  *
01105 *         *                                                     *
01106 *         *  3. IF A STARTING CHECK NUMBER HAS BEEN ENTERED,    *
01107 *         *     THE NUMBER IS COMPARED TO OTHER NUMBERS IN THE  *
01108 *         *     FILE FOR OVERLAPS AND GAPS.                     *
01109 *         *******************************************************.
01110
01111      MOVE PI-COMPANY-ID          TO  WS-ENQ-COMPANY-ID.
01112
01113      
      * EXEC CICS ENQ
01114 *        RESOURCE (WS-CHECK-QUEUE-DSID)
01115 *        LENGTH   (11)
01116 *    END-EXEC.
           MOVE 11
             TO DFHEIV11
      *    MOVE '2$L                   $   #00007401' TO DFHEIV0
           MOVE X'32244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303037343031' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01117
01118      
      * EXEC CICS HANDLE CONDITION
01119 *        NOTFND  (0225-MAIN-LOGIC)
01120 *        ENDFILE (0230-MAIN-LOGIC)
01121 *    END-EXEC.
      *    MOVE '"$I''                  ! $ #00007406' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303037343036' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01122
01123      MOVE LOW-VALUES             TO  WS-CHECK-AIX-KEY
01124                                      WS-LAST-CHECK-QUEUE-KEY.
01125      MOVE +5                     TO  WS-KEY-LENGTH.
01126
01127      MOVE PI-COMPANY-CD          TO  WS-CQK-COMPANY-CD-A1.
01128      MOVE 'CHKQ'                 TO  FILE-SWITCH.
01129
01130      
      * EXEC CICS STARTBR
01131 *        DATASET (WS-CHECK-QUEUE-AIX-DSID)
01132 *        RIDFLD  (WS-CHECK-AIX-KEY)
01133 *        GTEQ
01134 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00007418' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303037343138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-AIX-DSID, 
                 WS-CHECK-AIX-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01135
01136  0210-MAIN-LOGIC.
01137      MOVE EMI-FATAL-CTR         TO  WS-LAST-ERROR-COUNT.
01138
01139      
      * EXEC CICS READNEXT
01140 *        DATASET (WS-CHECK-QUEUE-AIX-DSID)
01141 *        RIDFLD  (WS-CHECK-AIX-KEY)
01142 *        SET     (ADDRESS OF CHECK-QUE)
01143 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00007427' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303037343237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-AIX-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CHECK-AIX-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CHECK-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01144
01145      IF CQ-COMPANY-CD NOT = PI-COMPANY-CD
01146          GO TO 0230-MAIN-LOGIC.
01147
01148      MOVE '1'                   TO  PI-VALID-RCD-SW.
01149
01150 *    NOTE *******************************************************
01151 *         *      SAVE THE CHECK NUMBER SO AT THE END OF THE     *
01152 *         *  BROWSE YOU CAN CHECK FOR GAPS OR OVERLAPS.         *
01153 *         *******************************************************.
01154
01155      IF CQ-CHECK-NUMBER GREATER THAN WS-GREATEST-CHECK-NUMBER-X
01156          MOVE CQ-CHECK-NUMBER    TO  WS-GREATEST-CHECK-NUMBER-X.
01157
01158 *    NOTE *******************************************************
01159 *         *      IF YOU ARE PROCESSING BY GROUPS BYPASS ALL     *
01160 *         *  RECORDS IF NOT IN SPECEFIED GROUPS.  SAVE THE HIGH *
01161 *         *  SEQUENCE NUMBER IN EACH GROUP FOR REPRINT.         *
01162 *         *******************************************************.
01163
01164      IF AOPTIONI = '2' OR '3'
01165          NEXT SENTENCE
01166        ELSE
01167          GO TO 0213-MAIN-LOGIC.
01168
01169      SET PI-INDEX
01170          EL176A-INDEX TO +1.
01171
01172  0212-MAIN-LOGIC.
01173      IF CQ-CONTROL-NUMBER  = PI-CONTROL-GROUP (PI-INDEX)
01174          IF CQ-SEQUENCE-NUMBER GREATER PI-HIGH-SEQUENCE (PI-INDEX)
01175              MOVE CQ-SEQUENCE-NUMBER
01176                                  TO PI-HIGH-SEQUENCE (PI-INDEX)
01177              GO TO 0215-MAIN-LOGIC
01178            ELSE
01179              GO TO 0215-MAIN-LOGIC.
01180
01181      IF PI-INDEX LESS THAN +4
01182          SET PI-INDEX
01183              EL176A-INDEX UP BY +1
01184          GO TO 0212-MAIN-LOGIC.
01185
01186      GO TO 0210-MAIN-LOGIC.
01187
01188  0213-MAIN-LOGIC.
01189 *    NOTE *******************************************************
01190 *         *      IF YOU ARE PRINTING ALL CONTROL GROUPS BYPASS  *
01191 *         *  THE CONTROL GROUPS THAT HAVE ALREADY BEEN PRINTED. *
01192 *         *******************************************************.
01193
01194      IF CQ-CONTROL-NUMBER NOT = WS-LAST-CONTROL-GROUP
01195          MOVE CQ-CONTROL-NUMBER  TO  WS-LAST-CONTROL-GROUP
01196          MOVE CQ-TIMES-PRINTED   TO  WS-TIMES-PRINTED.
01197
01198      IF WS-TIMES-PRINTED GREATER THAN ZERO
01199          GO TO 0210-MAIN-LOGIC.
01200
01201      EJECT
01202  0215-MAIN-LOGIC.
01203      IF AOPTIONI = '1' OR '2'
01204          NEXT SENTENCE
01205        ELSE
01206          GO TO 0220-MAIN-LOGIC.
01207
01208      IF CQ-TIMES-PRINTED GREATER THAN ZERO
01209         MOVE -1 TO EL176A-CONTROL-GROUP-LENGTH (EL176A-INDEX)
01210         MOVE AL-UNBON TO EL176A-CONTROL-GROUP-ATTRB (EL176A-INDEX)
01211         MOVE ER-0379                TO  EMI-ERROR
01212         PERFORM 9900-ERROR-FORMAT.
01213
01214      IF AACNI = 'Y'
01215        AND CQ-CHECK-NUMBER NOT = SPACES
01216          MOVE ER-0382            TO  EMI-ERROR
01217          PERFORM 9900-ERROR-FORMAT
01218          MOVE -1                 TO  AACNL
01219          MOVE AL-UABON           TO  AACNA
01220        ELSE
01221      IF AACNI = 'N'
01222        AND CQ-CHECK-NUMBER = SPACES
01223          MOVE -1                 TO  AACNL
01224          MOVE AL-UABON           TO  AACNA
01225          MOVE ER-0383            TO  EMI-ERROR
01226          PERFORM 9900-ERROR-FORMAT.
01227
01228      GO TO 0210-MAIN-LOGIC.
01229
01230  0220-MAIN-LOGIC.
01231      IF CQ-TIMES-PRINTED NOT GREATER THAN ZERO
01232         MOVE -1 TO EL176A-CONTROL-GROUP-LENGTH (EL176A-INDEX)
01233         MOVE AL-UNBON TO EL176A-CONTROL-GROUP-ATTRB (EL176A-INDEX)
01234         MOVE ER-0389                TO  EMI-ERROR
01235         PERFORM 9900-ERROR-FORMAT.
01236
01237      IF CQ-PRE-NUMBERING-SW = '1'
01238        AND AACNI = 'N'
01239          MOVE -1                 TO  AACNL
01240          MOVE AL-UABON           TO  AACNA
01241          MOVE ER-0390            TO  EMI-ERROR
01242          PERFORM 9900-ERROR-FORMAT.
01243
01244      IF CQ-PRE-NUMBERING-SW = SPACES
01245        AND AACNI = 'Y'
01246          MOVE -1                 TO  AACNL
01247          MOVE AL-UABON           TO  AACNA
01248          MOVE ER-0391            TO  EMI-ERROR
01249          PERFORM 9900-ERROR-FORMAT.
01250
01251      GO TO 0210-MAIN-LOGIC.
01252
01253  0225-MAIN-LOGIC.
01254      MOVE ER-0490                TO  EMI-ERROR
01255      MOVE -1                     TO  AOPTIONL
01256      PERFORM 8200-SEND-DATAONLY
01257      PERFORM 9100-RETURN-TRAN.
01258
01259      EJECT
01260  0230-MAIN-LOGIC.
01261      
      * EXEC CICS ENDBR
01262 *        DATASET (WS-CHECK-QUEUE-AIX-DSID)
01263 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00007549' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303037353439' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-AIX-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01264
01265      IF PI-VALID-RCD-SW NOT = '1'
01266          MOVE ER-2936            TO  EMI-ERROR
01267          MOVE -1                 TO  AOPTIONL
01268          GO TO 0015-MAIN-LOGIC.
01269
01270      IF EMI-FATAL-CTR GREATER THAN ZERO
01271          PERFORM 8200-SEND-DATAONLY.
01272
01273 *    NOTE *******************************************************
01274 *         *      READ THE COMPANY RECORD FROM THE CONTROL FILE  *
01275 *         *  TO GET THE CICS/VS PRINTER TERMINAL ID AND CHECK   *
01276 *         *  TO SEE IF THE PRINTER HAS BEEN SPECIFIED.          *
01277 *         *******************************************************.
01278
01279      MOVE PI-COMPANY-ID          TO  WS-CONTROL-FILE-KEY.
01280      MOVE '1'                    TO  WS-CFK-RECORD-TYPE.
01281      MOVE ZERO                   TO  WS-CFK-SEQUENCE-NO.
01282      MOVE 'CNTL'                 TO  FILE-SWITCH.
01283
01284      
      * EXEC CICS READ
01285 *        DATASET (WS-CONTROL-FILE-DSID)
01286 *        RIDFLD  (WS-CONTROL-FILE-KEY)
01287 *        SET    (ADDRESS OF CONTROL-FILE)
01288 *    END-EXEC.
      *    MOVE '&"S        E          (   #00007572' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037353732' TO DFHEIV0(25:11)
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
           
01289
01290      IF CF-CHECK-PRINTER-ID = SPACES
01291          MOVE ER-0371            TO  EMI-ERROR
01292          MOVE -1                 TO  APFKL
01293          PERFORM 9900-ERROR-FORMAT.
01294
01295      IF APRTL GREATER THAN ZEROS
01296          MOVE AL-UANON           TO  APRTA
01297          MOVE APRTI              TO  PI-CHECK-PRINTER-ID
01298      ELSE
01299          MOVE CF-CHECK-PRINTER-ID
01300                                  TO  PI-CHECK-PRINTER-ID.
01301
01302      MOVE CF-COMPANY-ADDRESS     TO  PI-COMPANY-ADDRESS.
01303      MOVE CF-CURRENT-MONTH-END   TO  PI-MONTH-END-SAVE.
01304
01305      IF PI-COMPANY-ID = 'ADL' OR 'FLB' OR 'ALA' OR 'FND'
01306         NEXT SENTENCE
01307      ELSE
01308         GO TO 0235-MAIN-LOGIC.
01309
01310      MOVE SAVE-BIN-DATE     TO DC-BIN-DATE-1.
01311      MOVE PI-MONTH-END-SAVE TO DC-BIN-DATE-2.
01312      MOVE '1'               TO DC-OPTION-CODE.
01313      PERFORM 8500-DATE-CONVERSION.
01314
01315      IF NO-CONVERSION-ERROR
01316         IF DC-ELAPSED-MONTHS GREATER THAN +0
01317            MOVE DC-GREG-DATE-2-EDIT  TO WS-WORK-DATE
01318            MOVE '01'                 TO WS-WORK-DA
01319            MOVE WS-WORK-DATE         TO DC-GREG-DATE-1-EDIT
01320            MOVE '2'                  TO DC-OPTION-CODE
01321            PERFORM 8500-DATE-CONVERSION
01322            IF NO-CONVERSION-ERROR
01323               MOVE DC-BIN-DATE-1     TO WS-CHECK-WRITER-DATE
01324               MOVE ZEROS             TO DC-ELAPSED-MONTHS
01325               IF DC-DAY-OF-WEEK = +1
01326                  MOVE +1             TO DC-ELAPSED-DAYS
01327                  MOVE '6'            TO DC-OPTION-CODE
01328                  PERFORM 8500-DATE-CONVERSION
01329                  IF NO-CONVERSION-ERROR
01330                     MOVE DC-BIN-DATE-2 TO WS-CHECK-WRITER-DATE
01331                  ELSE
01332                     NEXT SENTENCE
01333               ELSE
01334               IF DC-DAY-OF-WEEK = +7
01335                  MOVE +2          TO DC-ELAPSED-DAYS
01336                  MOVE '6'         TO DC-OPTION-CODE
01337                  PERFORM 8500-DATE-CONVERSION
01338                  IF NO-CONVERSION-ERROR
01339                     MOVE DC-BIN-DATE-2 TO WS-CHECK-WRITER-DATE.
01340
01341  0235-MAIN-LOGIC.
01342
01343      IF AACNI = 'Y'
01344          IF WS-GREATEST-CHECK-NUMBER-X NOT LESS THAN ACKNOI
01345              MOVE ER-0380        TO  EMI-ERROR
01346              PERFORM 9900-ERROR-FORMAT
01347              MOVE -1             TO  ACKNOL
01348              MOVE AL-UNBON       TO  ACKNOA
01349            ELSE
01350              SUBTRACT +1 FROM ACKNOO GIVING WS-ACKNO
01351              IF WS-GREATEST-CHECK-NUMBER-X NOT = WS-ACKNO-X
01352                  MOVE ER-0381    TO  EMI-ERROR
01353                  PERFORM 9900-ERROR-FORMAT
01354                  MOVE -1         TO  ACKNOL
01355                  MOVE AL-UNBON   TO  ACKNOA.
01356
CIDMOD*    IF PI-COMPANY-ID NOT = 'DMD' AND 'LGX'
PEMMOD*    IF PI-COMPANY-ID NOT = 'DMD' AND 'LGX' AND 'CID' AND 'CSO'
01358      GO TO 0237-MAIN-LOGIC.
01359
01360      
      * EXEC CICS HANDLE CONDITION
01361 *        NOTOPEN    (8800-NOT-OPEN)
01362 *        NOTFND     (0236-MAIN-LOGIC)
01363 *    END-EXEC.
      *    MOVE '"$JI                  ! % #00007649' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303037363439' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01364
01365      MOVE 'MICR'                 TO  WS-DMD-FLAG-KEY.
01366      MOVE 'FLAG'                 TO  FILE-SWITCH.
01367
01368      
      * EXEC CICS READ
01369 *        DATASET ('MICRFLAG')
01370 *        RIDFLD  (WS-DMD-FLAG-KEY)
01371 *        INTO    (WS-DMD-MICR-FLAG)
01372 *    END-EXEC.
           MOVE LENGTH OF
            WS-DMD-MICR-FLAG
             TO DFHEIV11
           MOVE 'MICRFLAG' TO DFHEIV1
      *    MOVE '&"IL       E          (   #00007657' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037363537' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 WS-DMD-MICR-FLAG, 
                 DFHEIV11, 
                 WS-DMD-FLAG-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01373
01374  0236-MAIN-LOGIC.
01375
01376      
      * EXEC CICS HANDLE CONDITION
01377 *        NOTOPEN    (8800-NOT-OPEN)
01378 *        NOTFND     (0237-MAIN-LOGIC)
01379 *    END-EXEC.
      *    MOVE '"$JI                  ! & #00007665' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303037363635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01380
01381      MOVE '420E'                 TO  WS-DMD-DRFT-KEY.
01382      MOVE 'DRFT'                 TO  FILE-SWITCH.
01383
01384      
      * EXEC CICS READ
01385 *        DATASET ('MICRDRFT')
01386 *        RIDFLD  (WS-DMD-DRFT-KEY)
01387 *        INTO    (WS-DMD-MICR-DRFT)
01388 *    END-EXEC.
           MOVE LENGTH OF
            WS-DMD-MICR-DRFT
             TO DFHEIV11
           MOVE 'MICRDRFT' TO DFHEIV1
      *    MOVE '&"IL       E          (   #00007673' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037363733' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 WS-DMD-MICR-DRFT, 
                 DFHEIV11, 
                 WS-DMD-DRFT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01389
01390      MOVE '    '                 TO  FILE-SWITCH.
01391
01392  0237-MAIN-LOGIC.
01393
01394      IF EMI-FATAL-CTR GREATER THAN ZERO
01395          PERFORM 8200-SEND-DATAONLY.
01396
01397      MOVE AL-SANON               TO  AOPTIONA
01398                                      ACG01A
01399                                      ACG02A
01400                                      ACG03A
01401                                      ACG04A
01402                                      AACNA
01403                                      ACKNOA
01404                                      APRTA.
01405
01406      MOVE +1                     TO  PI-PROCESSING-SW.
01407      MOVE -1                     TO  AALIGNL.
01408
01409      IF PI-NUMBER-OF-ALIGNMENT-CHECKS GREATER THAN ZERO
01410          MOVE ER-0361               TO  EMI-ERROR
01411        ELSE
01412          MOVE ER-0362               TO  EMI-ERROR.
01413
01414      PERFORM 8200-SEND-DATAONLY.
01415
01416      
      * EXEC CICS HANDLE AID
01417 *        CLEAR (9400-CLEAR)
01418 *        PA1   (0040-MAIN-LOGIC)
01419 *        PA2   (0040-MAIN-LOGIC)
01420 *        PA3   (0040-MAIN-LOGIC)
01421 *    END-EXEC.
      *    MOVE '"&=!"#               V! '' #00007705' TO DFHEIV0
           MOVE X'22263D212223202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020562120' TO DFHEIV0(13:12)
           MOVE X'2720233030303037373035' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01422
01423      
      * EXEC CICS SYNCPOINT
01424 *    END-EXEC.
      *    MOVE '6"                    !   #00007712' TO DFHEIV0
           MOVE X'362220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303037373132' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01425
01426      GO TO 0040-MAIN-LOGIC.
01427
01428      EJECT
01429  0240-MAIN-LOGIC.
01430      IF AALIGNL GREATER THAN ZERO
01431          IF AALIGNI IS NUMERIC
01432              MOVE AALIGNO      TO  PI-NUMBER-OF-ALIGNMENT-CHECKS
01433              MOVE AL-UNNON     TO  AALIGNA
01434            ELSE
01435              MOVE ER-0365      TO  EMI-ERROR
01436              MOVE -1           TO  AALIGNL
01437              MOVE AL-UNBON     TO  AALIGNA
01438              PERFORM 8200-SEND-DATAONLY
01439              GO TO 0040-MAIN-LOGIC.
01440
01441      IF PI-NUMBER-OF-ALIGNMENT-CHECKS NOT GREATER THAN ZERO
01442          GO TO 0300-MAIN-LOGIC.
01443
01444      IF PI-ALIGNMENT-CONTROL-GROUP GREATER THAN ZERO
01445          GO TO 0245-MAIN-LOGIC.
01446
01447      MOVE PI-COMPANY-ID          TO  WS-CONTROL-FILE-KEY.
01448      MOVE '1'                    TO  WS-CFK-RECORD-TYPE.
01449      MOVE ZERO                   TO  WS-CFK-SEQUENCE-NO.
01450      MOVE 'CNTL'                 TO  FILE-SWITCH.
01451
01452      
      * EXEC CICS READ UPDATE
01453 *        DATASET (WS-CONTROL-FILE-DSID)
01454 *        RIDFLD  (WS-CONTROL-FILE-KEY)
01455 *        SET    (ADDRESS OF CONTROL-FILE)
01456 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00007741' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037373431' TO DFHEIV0(25:11)
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
           
01457
01458      ADD +1  TO  CF-CO-CHECK-QUE-COUNTER.
01459
01460      IF CO-QUE-COUNT-RESET
01461          MOVE +1                 TO  CF-CO-CHECK-QUE-COUNTER.
01462
01463      MOVE CF-CO-CHECK-QUE-COUNTER TO PI-ALIGNMENT-CONTROL-GROUP.
01464
01465      
      * EXEC CICS REWRITE
01466 *        DATASET (WS-CONTROL-FILE-DSID)
01467 *        FROM    (CONTROL-FILE)
01468 *    END-EXEC.
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00007754' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303037373534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CONTROL-FILE-DSID, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01469
01470      MOVE EIBTRMID               TO  PI-TSK-TERM-ID.
01471      MOVE EIBTIME                TO  PI-TSK-TIME.
01472      MOVE PI-COMPANY-ID          TO  WS-CHECK-TRANSID-2.
01473      IF PI-COMPANY-ID = 'COM'
01474          MOVE 'COM'              TO  WS-CHECK-TRANSID-2.
01475
01476      IF PI-COMPANY-ID = 'PEM' OR 'CLS' OR 'ACC' OR 'MOD'
01477          MOVE 'LGX'              TO  WS-CHECK-TRANSID-2.
01478
01479      IF PI-COMPANY-ID = 'KSM'
01480          MOVE 'KSA'              TO  WS-CHECK-TRANSID-2.
01481
01482      IF PI-COMPANY-ID = 'ADL' OR 'DEF' OR 'FLB' OR 'ALA'
01483          MOVE 'ADL'              TO  WS-CHECK-TRANSID-2.
01484
01485      IF PI-COMPANY-ID = 'FGL'
01486          MOVE 'CGL'              TO  WS-CHECK-TRANSID-2.
01487
01488      IF PI-COMPANY-ID = 'LAP'
01489          MOVE 'RMC'              TO  WS-CHECK-TRANSID-2.
01490
01491      IF PI-COMPANY-ID = 'TII'
01492          MOVE 'TIH'              TO  WS-CHECK-TRANSID-2.
01493
01494      IF PI-COMPANY-ID = 'CVL'  OR  'CNL'
01495          MOVE 'MNL'              TO  WS-CHECK-TRANSID-2.
01496
01497      IF PI-COMPANY-ID = 'AUK'
01498          MOVE 'AIG'              TO  WS-CHECK-TRANSID-2.
01499
01500      IF PI-COMPANY-ID = 'BPI'
01501          MOVE 'UCL'              TO  WS-CHECK-TRANSID-2.
01502
01503      IF PI-COMPANY-ID = 'ITY' OR 'FLC' OR 'FRO' OR 'FRN'
01504                               OR 'FRS' OR 'FRT' OR 'FRH'
01505                               OR 'OFI' OR 'OFJ' OR 'CAB'
01506                               OR 'AFL' OR 'AFC' OR 'SRL'
01507                               OR 'RIC'
01508          MOVE 'ITY'              TO  WS-CHECK-TRANSID-2.
01509
01510      IF PI-COMPANY-ID = 'JAL' OR 'JAI'
01511         NEXT SENTENCE
01512      ELSE
030612         IF PI-COMPANY-ID = 'DMD' OR 'CID' OR 'DCC' OR 'AHL'
01514              
      * EXEC CICS START
CIDMOD*                TRANSID ('EX47')
01516 *                FROM    (PROGRAM-INTERFACE-BLOCK)
01517 *                LENGTH  (PI-COMM-LENGTH)
PEMTMP*                TERMID  ('A199')
01518 *            END-EXEC
           MOVE 'EX47' TO DFHEIV5
      *    MOVE '0( LF                 1   #00007803' TO DFHEIV0
           MOVE X'3028204C4620202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020203120' TO DFHEIV0(13:12)
           MOVE X'2020233030303037383033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV5, 
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
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01519          ELSE
01520              
      * EXEC CICS START
01521 *                TRANSID (WS-CHECK-WRITER-TRANS-ID)
01522 *                FROM    (PROGRAM-INTERFACE-BLOCK)
01523 *                LENGTH  (PI-COMM-LENGTH)
01524 *                TERMID  (PI-CHECK-PRINTER-ID)
01525 *            END-EXEC.
      *    MOVE '0( LFT                1   #00007810' TO DFHEIV0
           MOVE X'3028204C4654202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020203120' TO DFHEIV0(13:12)
           MOVE X'2020233030303037383130' TO DFHEIV0(25:11)
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
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01526
01527      MOVE +1                     TO  PI-PRINTER-STARTED-SW.
01528
01529      
      * EXEC CICS GETMAIN
01530 *        SET     (ADDRESS OF CHECK-QUE)
01531 *        LENGTH  (100)
01532 *        INITIMG (WS-SPACES)
01533 *    END-EXEC.
           MOVE 100
             TO DFHEIV11
      *    MOVE ',"IL                  $   #00007819' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303037383139' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 WS-SPACES
           SET ADDRESS OF CHECK-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01534
01535  0245-MAIN-LOGIC.
01536      MOVE SPACES                 TO  CHECK-QUE.
01537
01538      MOVE 'CQ'                   TO  CQ-RECORD-ID.
01539
01540      MOVE LOW-VALUES             TO  CQ-CONTROL-BY-PAYEE.
01541      MOVE PI-COMPANY-CD          TO  CQ-COMPANY-CD
01542                                      CQ-COMPANY-CD-A1.
01543      MOVE 'A'                    TO  CQ-ENTRY-TYPE.
01544
01545      MOVE PI-ALIGNMENT-CONTROL-GROUP
01546                                  TO  CQ-CONTROL-NUMBER
01547                                      CQ-CONTROL-NUMBER-A1.
01548
01549      MOVE ZERO                   TO  CQ-CHECK-AMOUNT.
01550      MOVE +1                     TO  CQ-TIMES-PRINTED.
01551      MOVE WS-CHECK-WRITER-DATE   TO  CQ-CHECK-WRITTEN-DT.
01552
01553      MOVE +1760                  TO  CQ-LAST-UPDATED-BY.
01554      MOVE EIBTIME                TO  CQ-LAST-UPDATED-HHMMSS.
01555      MOVE LOW-VALUES             TO  CQ-APPLIED-TO-RCON-DT.
01556      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
01557      MOVE '5'                    TO  DC-OPTION-CODE.
01558      PERFORM 8500-DATE-CONVERSION.
01559      MOVE DC-BIN-DATE-1          TO  CQ-LAST-UPDATED-DT.
01560
01561  0250-MAIN-LOGIC.
01562      IF AACNI = 'Y'
01563          MOVE WS-CHECK-NUMBER-X  TO  CQ-CHECK-NUMBER
01564          ADD +1  TO  WS-CHECK-NUMBER
01565          MOVE '1'                TO  CQ-PRE-NUMBERING-SW.
01566
01567      MOVE PI-ALIGNMENT-SEQUENCE-NO  TO  CQ-SEQUENCE-NUMBER
01568                                  CQ-SEQUENCE-NUMBER-A1.
01569      ADD +1  TO  PI-ALIGNMENT-SEQUENCE-NO.
01570
01571      MOVE SPACES                 TO  CHECK-PASS-AREA.
01572
01573      MOVE +1                     TO  CPA-ALIGNMENT.
01574      MOVE CQ-CHECK-NUMBER        TO  CPA-CHECK-NUMBER.
01575
01576      IF PI-COMPANY-ID = 'CSL'
01577          MOVE PI-CARRIER         TO  CPA-CARRIER.
01578
01579      PERFORM 0800-PRINT-CHECK.
01580
01581      IF AACNI = 'Y'
01582          
      * EXEC CICS WRITE
01583 *            DATASET (WS-CHECK-QUEUE-DSID)
01584 *            RIDFLD  (CQ-CONTROL-PRIMARY)
01585 *            FROM    (CHECK-QUE)
01586 *        END-EXEC.
           MOVE LENGTH OF
            CHECK-QUE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00007872' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303037383732' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-DSID, 
                 CHECK-QUE, 
                 DFHEIV11, 
                 CQ-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01587
01588      SUBTRACT +1 FROM PI-NUMBER-OF-ALIGNMENT-CHECKS.
01589
01590      IF PI-NUMBER-OF-ALIGNMENT-CHECKS IS GREATER THAN ZERO
01591          GO TO 0250-MAIN-LOGIC.
01592
01593      MOVE SPACES                    TO  AALIGNI.
01594      MOVE AL-UNNOF                  TO  AALIGNA.
01595      MOVE -1                        TO  AALIGNL.
01596      MOVE ER-0362                   TO  EMI-ERROR.
01597      PERFORM 8200-SEND-DATAONLY.
01598
01599      
      * EXEC CICS SYNCPOINT
01600 *    END-EXEC.
      *    MOVE '6"                    !   #00007889' TO DFHEIV0
           MOVE X'362220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303037383839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01601
01602      GO TO 0040-MAIN-LOGIC.
01603
01604      EJECT
01605  0300-MAIN-LOGIC.
01606      MOVE PI-COMPANY-ID          TO  WS-CHECK-TRANSID-2.
01607
01608      IF PI-COMPANY-ID = 'COM'
01609          MOVE 'COM'              TO  WS-CHECK-TRANSID-2.
01610
01611      IF PI-COMPANY-ID = 'PEM' OR 'CLS' OR 'ACC' OR 'MOD'
01612          MOVE 'LGX'              TO  WS-CHECK-TRANSID-2.
01613
01614
01615      IF PI-COMPANY-ID = 'KSM'
01616          MOVE 'KSA'              TO  WS-CHECK-TRANSID-2.
01617
01618      IF PI-COMPANY-ID = 'ADL' OR 'DEF' OR 'FLB' OR 'ALA'
01619          MOVE 'ADL'              TO  WS-CHECK-TRANSID-2.
01620
01621      IF PI-COMPANY-ID = 'FGL'
01622          MOVE 'CGL'              TO  WS-CHECK-TRANSID-2.
01623
01624      IF PI-COMPANY-ID = 'LAP'
01625          MOVE 'RMC'              TO  WS-CHECK-TRANSID-2.
01626
01627      IF PI-COMPANY-ID = 'TII'
01628          MOVE 'TIH'              TO  WS-CHECK-TRANSID-2.
01629
01630      IF PI-COMPANY-ID = 'CVL'  OR  'CNL'
01631          MOVE 'MNL'              TO  WS-CHECK-TRANSID-2.
01632
01633      IF PI-COMPANY-ID = 'AUK'
01634          MOVE 'AIG'              TO  WS-CHECK-TRANSID-2.
01635
01636      IF PI-COMPANY-ID = 'BPI'
01637          MOVE 'UCL'              TO  WS-CHECK-TRANSID-2.
01638
01639      IF PI-COMPANY-ID = 'ITY' OR 'FLC' OR 'FRO' OR 'FRN'
01640                               OR 'FRT' OR 'FRH' OR 'FRS'
01641                               OR 'OFI' OR 'OFJ' OR 'CAB'
01642                               OR 'AFL' OR 'AFC' OR 'SRL'
01643                               OR 'RIC'
01644          MOVE 'ITY'              TO  WS-CHECK-TRANSID-2.
01645
01646 *    IF PI-COMPANY-ID = 'CSL'
01647 *        MOVE 'E'                TO  WS-CHECK-TRANSID-1
01648 *        MOVE 'X47'              TO  WS-CHECK-TRANSID-2.
01649
01650      IF PI-PRINTER-STARTED-SW = ZERO
01651         MOVE +1                     TO  PI-PRINTER-STARTED-SW
01652         MOVE EIBTRMID               TO  PI-TSK-TERM-ID
01653         MOVE EIBTIME                TO  PI-TSK-TIME
01654         IF PI-COMPANY-ID = 'JAL' OR 'JAI'
01655            NEXT SENTENCE
01656         ELSE
030612        IF PI-COMPANY-ID = 'DMD' OR 'CID' OR 'DCC' OR 'AHL'
01658              
      * EXEC CICS START
CIDMOD*                TRANSID ('EX47')
01660 *                FROM    (PROGRAM-INTERFACE-BLOCK)
01661 *                LENGTH  (PI-COMM-LENGTH)
PEMTMP*                TERMID  ('A199')
01662 *            END-EXEC
           MOVE 'EX47' TO DFHEIV5
      *    MOVE '0( LF                 1   #00007948' TO DFHEIV0
           MOVE X'3028204C4620202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020203120' TO DFHEIV0(13:12)
           MOVE X'2020233030303037393438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV5, 
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
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01663         ELSE
01664            
      * EXEC CICS START
01665 *               TRANSID (WS-CHECK-WRITER-TRANS-ID)
01666 *               FROM    (PROGRAM-INTERFACE-BLOCK)
01667 *               LENGTH  (PI-COMM-LENGTH)
01668 *               TERMID  (PI-CHECK-PRINTER-ID)
01669 *          END-EXEC.
      *    MOVE '0( LFT                1   #00007955' TO DFHEIV0
           MOVE X'3028204C4654202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020203120' TO DFHEIV0(13:12)
           MOVE X'2020233030303037393535' TO DFHEIV0(25:11)
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
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01670
01671      SET PI-INDEX TO +1.
01672
01673      MOVE LOW-VALUES             TO  WS-CHECK-AIX-KEY.
01674      MOVE PI-COMPANY-CD          TO  WS-CQK-COMPANY-CD-A1.
01675
01676      IF PI-COMPANY-ID NOT = 'DMD'
01677          MOVE +5                 TO  WS-KEY-LENGTH
01678      ELSE
01679          MOVE +1                 TO  WS-KEY-LENGTH.
01680
01681  0310-MAIN-LOGIC.
01682      MOVE LOW-VALUES             TO  WS-LAST-CHECK-QUEUE-KEY.
01683      MOVE 'CHKQ'                 TO  FILE-SWITCH.
01684
01685      IF AOPTIONI = '1'
01686          
      * EXEC CICS STARTBR
01687 *            DATASET (WS-CHECK-QUEUE-AIX-DSID)
01688 *            RIDFLD  (WS-CHECK-AIX-KEY)
01689 *            GTEQ
01690 *        END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00007977' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303037393737' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-AIX-DSID, 
                 WS-CHECK-AIX-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01691      ELSE
01692          IF PI-COMPANY-ID NOT = 'DMD'
01693              MOVE PI-CONTROL-GROUP (PI-INDEX)
01694                                  TO WS-CQK-CONTROL-NUMBER-A1
01695              ADD +1  PI-HIGH-SEQUENCE (PI-INDEX)
01696                                   GIVING WS-SEQUENCE-NUMBER
01697              MOVE ZERO               TO  WS-CQK-SEQUENCE-NUMBER-A1
01698              END-IF
01699              IF WS-CHECK-QUEUE-BROWSE-SW = ZERO
01700                  
      * EXEC CICS STARTBR
01701 *                    DATASET   (WS-CHECK-QUEUE-AIX-DSID)
01702 *                    RIDFLD    (WS-CHECK-AIX-KEY)
01703 *                    KEYLENGTH (WS-KEY-LENGTH)
01704 *                    GENERIC   EQUAL
01705 *                END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,   KG    E          &   #00007991' TO DFHEIV0
           MOVE X'262C2020204B472020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303037393931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-AIX-DSID, 
                 WS-CHECK-AIX-KEY, 
                 WS-KEY-LENGTH, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01706                  MOVE +1         TO  WS-CHECK-QUEUE-BROWSE-SW
01707              ELSE
01708                  
      * EXEC CICS RESETBR
01709 *                    DATASET   (WS-CHECK-QUEUE-AIX-DSID)
01710 *                    RIDFLD    (WS-CHECK-AIX-KEY)
01711 *                    KEYLENGTH (WS-KEY-LENGTH)
01712 *                    GENERIC   EQUAL
01713 *                END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&4   KG    E          &   #00007999' TO DFHEIV0
           MOVE X'26342020204B472020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303037393939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-AIX-DSID, 
                 WS-CHECK-AIX-KEY, 
                 WS-KEY-LENGTH, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01714
01715      EJECT
01716  0320-MAIN-LOGIC.
01717      
      * EXEC CICS HANDLE CONDITION
01718 *        NOTFND
01719 *        ENDFILE (0390-MAIN-LOGIC)
01720 *    END-EXEC.
      *    MOVE '"$¹''                  ! ( #00008008' TO DFHEIV0
           MOVE X'2224B9272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303038303038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01721
01722  0325-MAIN-LOGIC.
01723      
      * EXEC CICS READNEXT
01724 *        DATASET (WS-CHECK-QUEUE-AIX-DSID)
01725 *        RIDFLD  (WS-CHECK-AIX-KEY)
01726 *        SET     (ADDRESS OF CHECK-QUE)
01727 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00008014' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303038303134' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-AIX-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CHECK-AIX-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CHECK-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01728
01729      IF CQ-PRE-NUMBERING-SW NOT = '1'
01730          IF WS-CHECK-AIX-KEY NOT GREATER WS-LAST-CHECK-QUEUE-KEY
01731              GO TO 0325-MAIN-LOGIC.
01732
01733      IF CQ-PRE-NUMBERING-SW NOT = '1'
01734          MOVE WS-CHECK-AIX-KEY   TO  WS-LAST-CHECK-QUEUE-KEY.
01735
01736      IF CQ-COMPANY-CD NOT = PI-COMPANY-CD
01737          GO TO 0390-MAIN-LOGIC.
01738
01739      IF PI-COMPANY-ID NOT = 'DMD'
01740          IF AOPTIONI = '2' OR '3'
01741              IF CQ-CONTROL-NUMBER = PI-CONTROL-GROUP (PI-INDEX)
01742                  IF CQ-SEQUENCE-NUMBER NOT GREATER THAN
01743                                       PI-HIGH-SEQUENCE (PI-INDEX)
01744                      GO TO 0325-CONT-LOGIC
01745                  ELSE
01746                      GO TO 0325-MAIN-LOGIC
01747              ELSE
01748                  GO TO 0390-MAIN-LOGIC
01749          ELSE
01750              GO TO 0325-CONT-LOGIC.
01751
01752      IF AOPTIONI NOT = '2' AND '3'
01753          GO TO 0325-CONT-LOGIC.
01754
01755      IF CQ-CONTROL-NUMBER = PI-CONTROL-GROUP (1)
01756          IF CQ-SEQUENCE-NUMBER NOT GREATER THAN
01757                                    PI-HIGH-SEQUENCE (1)
01758              GO TO 0325-CONT-LOGIC
01759          ELSE
01760              GO TO 0325-MAIN-LOGIC
01761      ELSE
01762          IF CQ-CONTROL-NUMBER = PI-CONTROL-GROUP (2)
01763              IF CQ-SEQUENCE-NUMBER NOT GREATER THAN
01764                                        PI-HIGH-SEQUENCE (2)
01765                  GO TO 0325-CONT-LOGIC
01766              ELSE
01767                  GO TO 0325-MAIN-LOGIC
01768          ELSE
01769              IF CQ-CONTROL-NUMBER = PI-CONTROL-GROUP (3)
01770                  IF CQ-SEQUENCE-NUMBER NOT GREATER THAN
01771                                        PI-HIGH-SEQUENCE (3)
01772                      GO TO 0325-CONT-LOGIC
01773                  ELSE
01774                      GO TO 0325-MAIN-LOGIC
01775              ELSE
01776                  IF CQ-CONTROL-NUMBER = PI-CONTROL-GROUP (4)
01777                      IF CQ-SEQUENCE-NUMBER NOT GREATER THAN
01778                                        PI-HIGH-SEQUENCE (4)
01779                          GO TO 0325-CONT-LOGIC
01780                      ELSE
01781                          GO TO 0325-MAIN-LOGIC
01782                  ELSE
01783                      GO TO 0325-MAIN-LOGIC.
01784
01785  0325-CONT-LOGIC.
01786      IF CQ-ENTRY-TYPE NOT = 'Q'
01787          GO TO 0325-MAIN-LOGIC.
01788
01789      IF (AOPTIONI NOT = '3' AND
01790          CQ-TIMES-PRINTED GREATER THAN ZERO)
01791        OR
01792         (AOPTIONI = '3' AND
01793          CQ-TIMES-PRINTED NOT GREATER THAN ZERO)
01794              GO TO 0325-MAIN-LOGIC.
01795
01796      IF AOPTIONI = '1'
01797         IF NOT PI-NO-CARRIER-SECURITY
01798            IF PI-CARRIER-SECURITY NOT = CQ-CARRIER
01799               GO TO 0325-MAIN-LOGIC.
01800
01801      MOVE CQ-CONTROL-PRIMARY         TO  WS-CHECK-QUEUE-KEY.
01802
01803      
      * EXEC CICS ENDBR
01804 *        DATASET (WS-CHECK-QUEUE-AIX-DSID)
01805 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00008094' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303038303934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-AIX-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01806
01807      
      * EXEC CICS READ UPDATE
01808 *        DATASET (WS-CHECK-QUEUE-DSID)
01809 *        RIDFLD  (WS-CHECK-QUEUE-KEY)
01810 *        SET     (ADDRESS OF CHECK-QUE)
01811 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00008098' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038303938' TO DFHEIV0(25:11)
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
           
01812
01813      MOVE CHECK-QUE              TO  WS-OLD-CHECK-QUEUE-RECORD.
01814
01815      IF AACNI = 'Y'
01816          MOVE WS-CHECK-NUMBER-X  TO  CQ-CHECK-NUMBER
01817          ADD +1  TO  WS-CHECK-NUMBER
01818          MOVE '1'                TO  CQ-PRE-NUMBERING-SW.
01819
01820 *    NOTE *******************************************************
01821 *         *                PRINT THE CHECK                      *
01822 *         *******************************************************.
01823
01824      MOVE PI-COMPANY-CD          TO  WS-CK-COMPANY-CD
01825                                      WS-CM-COMPANY-CD.
01826      MOVE CQ-CARRIER             TO  WS-CK-CARRIER
01827                                      CPA-CARRIER.
01828      MOVE CQ-CLAIM-NO            TO  WS-CK-CLAIM-NO
01829                                      CPA-CLAIM-NO.
01830      MOVE CQ-CERT-NO             TO  WS-CK-CERT-NO
01831                                      CPA-CERT-NO.
01832      MOVE 'MSTR'                 TO  FILE-SWITCH.
01833
01834      IF CQ-CARRIER NOT = WS-LAST-CARRIER
01835          PERFORM 1000-GET-CARRIER-NAME.
01836
01837 *    NOTE *******************************************************
01838 *         *            READ THE CLAIM MASTER RECORD             *
01839 *         *******************************************************.
01840
01841      
      * EXEC CICS READ
01842 *        DATASET (WS-CLAIM-MASTER-DSID)
01843 *        RIDFLD  (WS-CLAIM-MASTER-KEY)
01844 *        SET     (ADDRESS OF CLAIM-MASTER)
01845 *    END-EXEC.
      *    MOVE '&"S        E          (   #00008132' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038313332' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CLAIM-MASTER-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CLAIM-MASTER-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01846
01847 *    NOTE *******************************************************
01848 *         *       READ THE CERTIFICATE MASTER RECORD            *
01849 *         *******************************************************.
01850
01851      IF CL-SYSTEM-IDENTIFIER = 'CV'
01852          GO TO 0325-READ-EMPLCY.
01853
01854      MOVE PI-COMPANY-CD          TO  WS-CM-COMPANY-CD.
01855      MOVE CL-CERT-CARRIER        TO  WS-CM-CARRIER.
01856      MOVE CL-CERT-GROUPING       TO  WS-CM-GROUPING.
01857      MOVE CL-CERT-STATE          TO  WS-CM-STATE.
01858      MOVE CL-CERT-ACCOUNT        TO  WS-CM-ACCOUNT.
01859      MOVE CL-CERT-EFF-DT         TO  WS-CM-CERT-EFF-DT.
01860      MOVE CL-CERT-NO             TO  WS-CM-CERT-NO.
01861      MOVE 'CERT'                 TO  FILE-SWITCH.
01862
01863      
      * EXEC CICS READ
01864 *        DATASET   (WS-CERTIFICATE-MASTER-DSID)
01865 *        RIDFLD    (WS-CERTIFICATE-MASTER-KEY)
01866 *        SET       (ADDRESS OF CERTIFICATE-MASTER)
01867 *    END-EXEC.
      *    MOVE '&"S        E          (   #00008154' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038313534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CERTIFICATE-MASTER-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CERTIFICATE-MASTER-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01868
01869      GO TO 0328-MAIN-LOGIC.
01870
01871 *    NOTE *******************************************************
01872 *         *      READ THE CONVENIENCE POLICY MASTER RECORD      *
01873 *         *******************************************************.
01874
01875  0325-READ-EMPLCY.
01876
01877      MOVE PI-COMPANY-CD          TO  WS-PM-COMPANY-CD.
01878      MOVE CL-CERT-CARRIER        TO  WS-PM-CARRIER.
01879      MOVE CL-CERT-GROUPING       TO  WS-PM-GROUPING.
01880      MOVE CL-CERT-STATE          TO  WS-PM-STATE.
01881      MOVE CL-CERT-ACCOUNT        TO  WS-PM-PRODUCER.
01882      MOVE CL-CERT-EFF-DT         TO  WS-PM-EFF-DT.
01883      MOVE CL-CV-REFERENCE-NO     TO  WS-PM-REFERENCE-NO.
01884      MOVE 'PLCY'                 TO  FILE-SWITCH.
01885
01886      
      * EXEC CICS READ
01887 *        DATASET   (WS-POLICY-MASTER-DSID)
01888 *        RIDFLD    (WS-POLICY-MASTER-KEY)
01889 *        SET       (ADDRESS OF POLICY-MASTER)
01890 *    END-EXEC.
      *    MOVE '&"S        E          (   #00008177' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038313737' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-POLICY-MASTER-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-POLICY-MASTER-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF POLICY-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01891
01892      EJECT
01893  0326-READ-EMPROD.
01894
01895 *    NOTE *******************************************************
01896 *         *     READ THE CONVENIENCE PRODUCER MASTER RECORD     *
01897 *         *******************************************************.
01898
01899      MOVE PI-COMPANY-CD          TO  WS-PD-COMPANY-CD.
01900      MOVE CL-CERT-CARRIER        TO  WS-PD-CARRIER.
01901      MOVE CL-CERT-GROUPING       TO  WS-PD-GROUPING.
01902      MOVE CL-CERT-STATE          TO  WS-PD-STATE.
01903      MOVE CL-CERT-ACCOUNT        TO  WS-PD-PRODUCER.
01904      MOVE CL-CERT-EFF-DT         TO  WS-PD-EXP-DT.
01905      MOVE SPACES                 TO  WS-PRODUCER-HOLD-RECORD.
01906      MOVE 'PROD'                 TO  FILE-SWITCH.
01907
01908  0326-STARTBR-EMPROD.
01909
01910      
      * EXEC CICS HANDLE CONDITION
01911 *        NOTFND   (0326-NOTFND-EMPROD)
01912 *    END-EXEC.
      *    MOVE '"$I                   ! ) #00008201' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303038323031' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01913
01914      
      * EXEC CICS STARTBR
01915 *        DATASET   (WS-PRODUCER-MASTER-DSID)
01916 *        RIDFLD    (WS-PRODUCER-MASTER-KEY)
01917 *        GTEQ
01918 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00008205' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303038323035' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-PRODUCER-MASTER-DSID, 
                 WS-PRODUCER-MASTER-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01919
01920      IF WS-EMPROD-GETMAIN-SW = 'Y'
01921          MOVE 'N'                TO  WS-EMPROD-GETMAIN-SW
01922          
      * EXEC CICS GETMAIN
01923 *            SET       (ADDRESS OF PRODUCER-MASTER)
01924 *            LENGTH    (2000)
01925 *            INITIMG   (WS-SPACES)
01926 *        END-EXEC.
           MOVE 2000
             TO DFHEIV11
      *    MOVE ',"IL                  $   #00008213' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303038323133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 WS-SPACES
           SET ADDRESS OF PRODUCER-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01927
01928  0326-READNEXT-EMPROD.
01929
01930      
      * EXEC CICS READNEXT
01931 *        DATASET   (WS-PRODUCER-MASTER-DSID)
01932 *        RIDFLD    (WS-PRODUCER-MASTER-KEY)
01933 *        INTO      (PRODUCER-MASTER)
01934 *    END-EXEC.
           MOVE LENGTH OF
            PRODUCER-MASTER
             TO DFHEIV12
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )   #00008221' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303038323231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-PRODUCER-MASTER-DSID, 
                 PRODUCER-MASTER, 
                 DFHEIV12, 
                 WS-PRODUCER-MASTER-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01935
01936      IF WS-PD-COMPANY-CD NOT = PI-COMPANY-CD     OR
01937         WS-PD-CARRIER    NOT = CL-CERT-CARRIER   OR
01938         WS-PD-GROUPING   NOT = CL-CERT-GROUPING  OR
01939         WS-PD-STATE      NOT = CL-CERT-STATE     OR
01940         WS-PD-PRODUCER   NOT = CL-CERT-ACCOUNT
01941          IF WS-PRODUCER-HOLD-RECORD = SPACES
01942              GO TO 0326-NOTFND-EMPROD
01943          ELSE
01944              MOVE WS-PRODUCER-HOLD-RECORD    TO  PRODUCER-MASTER
01945              GO TO 0326-ENDBR-EMPROD.
01946
01947      IF WS-PD-EXP-DT = HIGH-VALUES
01948          GO TO 0326-ENDBR-EMPROD
01949      ELSE
01950          MOVE PRODUCER-MASTER    TO  WS-PRODUCER-HOLD-RECORD.
01951
01952      GO TO 0326-READNEXT-EMPROD.
01953
01954  0326-NOTFND-EMPROD.
01955
01956      MOVE SPACES                 TO  PRODUCER-MASTER.
01957
01958      MOVE ZEROS                  TO  PD-ZIP
01959                                      PD-TEL-NO
01960                                      PD-TOL-CLM.
01961
01962      MOVE 'PRODUCER NOT FOUND'   TO  PD-NAME.
01963
01964  0326-ENDBR-EMPROD.
01965
01966      
      * EXEC CICS ENDBR
01967 *        DATASET   (WS-PRODUCER-MASTER-DSID)
01968 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00008257' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303038323537' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-PRODUCER-MASTER-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01969
01970      MOVE SPACES                 TO  CHECK-PASS-AREA.
01971
01972      GO TO 0354-UPDATE-ACTIVITY-TRLRS.
01973
01974      EJECT
01975  0328-MAIN-LOGIC.
01976
01977      IF PI-COMPANY-ID = 'AIG' OR 'AUK'
01978          NEXT SENTENCE
01979      ELSE
01980          GO TO 0329-MAIN-LOGIC.
01981
01982 *    NOTE *******************************************************
01983 *         *     READ THE BENEFICIARY MASTER RECORD              *
01984 *         *     1ST READ IS TO GET THE CREDITOR'S NAME          *
01985 *         *******************************************************.
01986
01987      MOVE CL-CURRENT-GROUPING        TO  WS-GROUP.
01988      MOVE WS-GRP-1-3                 TO  WS-BK-BENEFICIARY.
01989      MOVE 'BENE'                     TO  FILE-SWITCH.
01990      PERFORM 2000-GET-BENEFICIARY.
01991
01992      MOVE BE-MAIL-TO-NAME            TO  WS-AIG-CREDITOR-NAME.
01993
01994 *    NOTE *******************************************************
01995 *         *     READ THE BENEFICIARY MASTER RECORD              *
01996 *         *     2ND READ IS TO GET THE INSURANCE CO'S NAME      *
01997 *         *******************************************************.
01998
01999      MOVE CL-BENEFICIARY             TO  WS-BK-BENEFICIARY.
02000      PERFORM 2000-GET-BENEFICIARY.
02001
02002      MOVE BE-MAIL-TO-NAME            TO  WS-AIG-INS-CO-NAME.
02003
02004  0329-MAIN-LOGIC.
02005
02006 *    NOTE *******************************************************
02007 *         *          READ THE ACCOUNT MASTER RECORD             *
02008 *         *******************************************************.
02009
02010      MOVE PI-COMPANY-CD          TO  WS-AK-COMPANY-CD.
02011
02012      IF PI-COMPANY-ID = 'AIG' OR 'AUK'
02013          MOVE CL-CURRENT-CARRIER     TO  WS-AK-CARRIER
02014          MOVE CL-CURRENT-GROUPING    TO  WS-AK-GROUPING
02015          MOVE CL-CURRENT-STATE       TO  WS-AK-STATE
02016          MOVE CL-CURRENT-ACCOUNT     TO  WS-AK-ACCOUNT
02017          MOVE CL-CERT-EFF-DT         TO  WS-AK-EXPIRATION-DT
02018      ELSE
02019          MOVE CL-CERT-CARRIER        TO  WS-AK-CARRIER
02020          MOVE CL-CERT-GROUPING       TO  WS-AK-GROUPING
02021          MOVE CL-CERT-STATE          TO  WS-AK-STATE
02022          MOVE CL-CERT-ACCOUNT        TO  WS-AK-ACCOUNT
02023          MOVE CL-CERT-EFF-DT         TO  WS-AK-EXPIRATION-DT.
02024
02025      MOVE 'ACCT'                     TO  FILE-SWITCH.
02026      MOVE SPACES                     TO  WS-ACCOUNT-HOLD-RECORD.
02027
02028      
      * EXEC CICS HANDLE CONDITION
02029 *        ENDFILE (0340-MAIN-LOGIC)
02030 *        NOTFND  (0340-MAIN-LOGIC)
02031 *    END-EXEC.
      *    MOVE '"$''I                  ! * #00008319' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2A20233030303038333139' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02032
02033      
      * EXEC CICS STARTBR
02034 *        DATASET (WS-ACCOUNT-MASTER-DSID)
02035 *        RIDFLD  (WS-ACCOUNT-MASTER-KEY)
02036 *        GTEQ
02037 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00008324' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303038333234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACCOUNT-MASTER-DSID, 
                 WS-ACCOUNT-MASTER-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02038
02039      IF WS-ERACCT-GETMAIN-SW = 'Y'
02040          MOVE 'N'                TO  WS-ERACCT-GETMAIN-SW
02041          
      * EXEC CICS GETMAIN
02042 *            SET     (ADDRESS OF ACCOUNT-MASTER)
02043 *            LENGTH  (2000)
02044 *            INITIMG (WS-SPACES)
02045 *        END-EXEC.
           MOVE 2000
             TO DFHEIV11
      *    MOVE ',"IL                  $   #00008332' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303038333332' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 WS-SPACES
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02046
02047  0330-MAIN-LOGIC.
02048      
      * EXEC CICS READNEXT
02049 *        DATASET (WS-ACCOUNT-MASTER-DSID)
02050 *        RIDFLD  (WS-ACCOUNT-MASTER-KEY)
02051 *        INTO    (ACCOUNT-MASTER)
02052 *    END-EXEC.
           MOVE LENGTH OF
            ACCOUNT-MASTER
             TO DFHEIV12
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )   #00008339' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303038333339' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACCOUNT-MASTER-DSID, 
                 ACCOUNT-MASTER, 
                 DFHEIV12, 
                 WS-ACCOUNT-MASTER-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02053
02054      IF PI-COMPANY-ID = 'AIG' OR 'AUK'
02055          IF WS-AK-COMPANY-CD NOT = PI-COMPANY-CD        OR
02056             WS-AK-CARRIER    NOT = CL-CURRENT-CARRIER   OR
02057             WS-AK-GROUPING   NOT = CL-CURRENT-GROUPING  OR
02058             WS-AK-STATE      NOT = CL-CURRENT-STATE     OR
02059             WS-AK-ACCOUNT    NOT = CL-CURRENT-ACCOUNT
02060              IF WS-ACCOUNT-HOLD-RECORD = SPACES
02061                  GO TO 0340-MAIN-LOGIC
02062              ELSE
02063                  MOVE WS-ACCOUNT-HOLD-RECORD TO  ACCOUNT-MASTER
02064                  GO TO 0350-MAIN-LOGIC
02065          ELSE
02066              NEXT SENTENCE
02067      ELSE
02068          IF WS-AK-COMPANY-CD NOT = PI-COMPANY-CD     OR
02069             WS-AK-CARRIER    NOT = CL-CERT-CARRIER   OR
02070             WS-AK-GROUPING   NOT = CL-CERT-GROUPING  OR
02071             WS-AK-STATE      NOT = CL-CERT-STATE     OR
02072             WS-AK-ACCOUNT    NOT = CL-CERT-ACCOUNT
02073             IF WS-ACCOUNT-HOLD-RECORD = SPACES
02074                GO TO 0340-MAIN-LOGIC
02075            ELSE
02076                MOVE WS-ACCOUNT-HOLD-RECORD TO ACCOUNT-MASTER
02077                GO TO 0350-MAIN-LOGIC.
02078
02079      IF WS-AK-EXPIRATION-DT = HIGH-VALUES
02080          GO TO 0350-MAIN-LOGIC
02081      ELSE
02082          MOVE ACCOUNT-MASTER     TO  WS-ACCOUNT-HOLD-RECORD.
02083
02084      GO TO 0330-MAIN-LOGIC.
02085
02086  0340-MAIN-LOGIC.
02087      MOVE SPACES                 TO  ACCOUNT-MASTER.
02088
02089      MOVE ZERO                   TO  AM-ZIP
02090                                      AM-TEL-NO
02091                                      AM-TOL-PREM
02092                                      AM-TOL-REF
02093                                      AM-TOL-CLM.
02094
02095      MOVE 'ACCOUNT NOT FOUND'    TO  AM-NAME.
02096
02097  0350-MAIN-LOGIC.
02098      
      * EXEC CICS ENDBR
02099 *        DATASET (WS-ACCOUNT-MASTER-DSID)
02100 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00008389' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303038333839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACCOUNT-MASTER-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02101
02102      MOVE SPACES                 TO  CHECK-PASS-AREA.
02103
02104      IF AM-3RD-PARTY-NOTIF-LEVEL NOT NUMERIC
02105         MOVE ZEROS               TO  AM-3RD-PARTY-NOTIF-LEVEL.
02106
02107      IF AM-3RD-PARTY-NOTIF-LEVEL = ZEROS
02108         GO TO 0354-UPDATE-ACTIVITY-TRLRS.
02109
042704     IF PI-COMPANY-ID = 'AIG' OR 'AUK' OR 'CID' OR 'DCC'
030612        OR 'AHL'
02111          GO TO 0354-UPDATE-ACTIVITY-TRLRS.
02112
02113      
      * EXEC CICS HANDLE CONDITION
02114 *         NOTFND (0351-READ-COMP-MASTER)
02115 *    END-EXEC.
      *    MOVE '"$I                   ! + #00008405' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2B20233030303038343035' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02116
02117      MOVE WS-CLAIM-MASTER-KEY    TO  WS-ACTIVITY-TRAILERS-KEY.
02118      MOVE +29                    TO  WS-ATK-SEQUENCE-NO.
02119      MOVE 'TRLR'                 TO  FILE-SWITCH.
02120
02121      
      * EXEC CICS READ
02122 *         DATASET  (WS-ACTIVITY-TRAILERS-DSID)
02123 *         RIDFLD   (WS-ACTIVITY-TRAILERS-KEY)
02124 *         SET      (ADDRESS OF ACTIVITY-TRAILERS)
02125 *    END-EXEC.
      *    MOVE '&"S        E          (   #00008413' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038343133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ACTIVITY-TRAILERS-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02126
02127      MOVE AT-MAIL-TO-NAME    TO  CPA-NOTIFY-NAME.
02128      MOVE AT-ADDRESS-LINE-1  TO  CPA-NOTIFY-ADDRESS-LINE1.
02129      MOVE AT-ADDRESS-LINE-2  TO  CPA-NOTIFY-ADDRESS-LINE2.
02130      MOVE SPACES             TO  CPA-NOTIFY-ADDRESS-LINE3.
02131      MOVE AT-CITY-STATE      TO  CPA-NOTIFY-CITY-STATE.
02132      MOVE AT-ZIP             TO  CPA-NOTIFY-ZIP.
02133
02134      GO TO 0354-UPDATE-ACTIVITY-TRLRS.
02135
02136  0351-READ-COMP-MASTER.
02137
02138      MOVE PI-COMPANY-CD   TO WS-ERCOMP-COMPANY-CD.
02139      MOVE AM-CARRIER      TO WS-ERCOMP-CARRIER.
02140      MOVE AM-GROUPING     TO WS-ERCOMP-GROUPING.
02141      MOVE 'A'             TO WS-ERCOMP-TYPE.
02142      MOVE AM-AGT (AM-3RD-PARTY-NOTIF-LEVEL)
02143                           TO WS-ERCOMP-RESP-NO.
02144      IF AM-3RD-PARTY-NOTIF-LEVEL = AM-REMIT-TO
02145          IF AM-COM-TYP (AM-REMIT-TO) = 'O' OR 'P' OR
052814                                       'G' OR 'B' or 'S'
02147              MOVE 'G'            TO  WS-ERCOMP-TYPE
02148              MOVE LOW-VALUES     TO  WS-ERCOMP-ACCOUNT
02149          ELSE
02150              MOVE AM-AGT (AM-3RD-PARTY-NOTIF-LEVEL)
02151                                  TO WS-ERCOMP-ACCOUNT
02152      ELSE
02153          MOVE 'G'                TO WS-ERCOMP-TYPE
02154          MOVE LOW-VALUES         TO WS-ERCOMP-ACCOUNT.
02155
02156      IF PI-ZERO-CARRIER OR PI-ZERO-CAR-GROUP
02157         MOVE ZEROS TO WS-ERCOMP-CARRIER.
02158
02159      IF PI-ZERO-GROUPING OR PI-ZERO-CAR-GROUP
02160         MOVE ZEROS TO WS-ERCOMP-GROUPING.
02161
02162      MOVE 'COMP'                 TO  FILE-SWITCH.
02163
02164      
      * EXEC CICS HANDLE CONDITION
02165 *         NOTFND    (0354-UPDATE-ACTIVITY-TRLRS)
02166 *    END-EXEC.
      *    MOVE '"$I                   ! , #00008456' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2C20233030303038343536' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02167
02168      
      * EXEC CICS  READ
02169 *         SET      (ADDRESS OF COMPENSATION-MASTER)
02170 *         DATASET  ('ERCOMP')
02171 *         RIDFLD   (WS-ERCOMP-KEY)
02172 *    END-EXEC.
           MOVE 'ERCOMP' TO DFHEIV1
      *    MOVE '&"S        E          (   #00008460' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038343630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ERCOMP-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMPENSATION-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02173
02174      MOVE CO-ACCT-NAME          TO CPA-NOTIFY-NAME
02175      IF CO-ACCT-NAME = SPACES
02176         MOVE CO-MAIL-NAME       TO CPA-NOTIFY-NAME.
02177
02178      MOVE CO-ADDR-1             TO CPA-NOTIFY-ADDRESS-LINE1.
02179      MOVE CO-ADDR-2             TO CPA-NOTIFY-ADDRESS-LINE2.
02180      MOVE CO-ADDR-3             TO CPA-NOTIFY-CITY-STATE.
02181      MOVE CO-ZIP                TO CPA-NOTIFY-ZIP.
02182
02183  0354-UPDATE-ACTIVITY-TRLRS.
02184
02185      MOVE WS-CLAIM-MASTER-KEY    TO  WS-ACTIVITY-TRAILERS-KEY.
02186      MOVE CQ-PMT-TRLR-SEQUENCE   TO  WS-ATK-SEQUENCE-NO.
02187      MOVE 'TRLR'                 TO  FILE-SWITCH.
02188
02189      
      * EXEC CICS READ UPDATE
02190 *        DATASET (WS-ACTIVITY-TRAILERS-DSID)
02191 *        RIDFLD  (WS-ACTIVITY-TRAILERS-KEY)
02192 *        SET     (ADDRESS OF ACTIVITY-TRAILERS)
02193 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00008481' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038343831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ACTIVITY-TRAILERS-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02194
02195      MOVE ZERO                   TO  CPA-ALIGNMENT.
02196
02197      IF CL-SYSTEM-IDENTIFIER = 'CV'
02198          MOVE PM-CARRIER                 TO  CPA-CARRIER
02199          MOVE PM-GROUPING                TO  CPA-GROUP
02200          MOVE PM-PRODUCER                TO  CPA-ACCOUNT
02201          MOVE PM-STATE                   TO  CPA-STATE
02202          MOVE PM-REFERENCE-NUMBER        TO  CPA-REFERENCE-NO
02203          MOVE PM-INS-TYPE                TO  CPA-IND-GRP-TYPE
02204          MOVE PM-POLICY-EFF-DT           TO  CPA-CERT-EFF-DT
02205      ELSE
02206          IF PI-COMPANY-ID = 'AIG' OR 'AUK'
02207              MOVE CL-CURRENT-CARRIER     TO  CPA-CARRIER
02208              MOVE CL-CURRENT-GROUPING    TO  CPA-GROUP
02209              MOVE CL-CURRENT-STATE       TO  CPA-STATE
02210              MOVE CL-CURRENT-ACCOUNT     TO  CPA-ACCOUNT
02211              MOVE CM-CERT-NO             TO  CPA-CERT-NO
02212              MOVE CM-IND-GRP-TYPE        TO  CPA-IND-GRP-TYPE
02213              MOVE CM-CERT-EFF-DT         TO  CPA-CERT-EFF-DT
02214          ELSE
02215              MOVE CM-CARRIER             TO  CPA-CARRIER
02216              MOVE CM-GROUPING            TO  CPA-GROUP
02217              MOVE CM-ACCOUNT             TO  CPA-ACCOUNT
02218              MOVE CM-STATE               TO  CPA-STATE
02219              MOVE CM-CERT-NO             TO  CPA-CERT-NO
02220              MOVE CM-IND-GRP-TYPE        TO  CPA-IND-GRP-TYPE
02221              MOVE CM-CERT-EFF-DT         TO  CPA-CERT-EFF-DT.
02222
02223      MOVE CL-CLAIM-NO                    TO  CPA-CLAIM-NO.
02224      MOVE CL-CLAIM-STATUS                TO  CPA-CLAIM-STATUS.
02225      MOVE CL-LAST-CLOSE-REASON           TO  CPA-LAST-CLOSE-REASON
02226
02227      PERFORM 5000-MOVE-NAME.
02228      MOVE WS-NAME-WORK           TO  CPA-INSURED-NAME.
02229
02230      MOVE CL-CLAIM-TYPE          TO  CPA-CLAIM-TYPE.
02231      MOVE AT-PAYMENT-TYPE        TO  CPA-PAYMENT-TYPE.
02232      MOVE CQ-CHECK-BY-USER       TO  CPA-PAYMENT-BY.
02233
02234      MOVE AT-PAYMENT-NOTE-SEQ-NO TO WS-PAYMENT-NOTE-SEQ-NO.
02235
02236      IF PI-COMPANY-ID = 'LAP' OR 'RMC'
02237          MOVE AT-FORM-CTL-SEQ-NO TO CPA-FORM-CTL-SEQ-NO.
02238
02239      MOVE CQ-CHECK-NUMBER        TO  CPA-CHECK-NUMBER
02240                                      AT-CHECK-NO.
02241
02242      MOVE CQ-CHECK-AMOUNT        TO  CPA-AMOUNT-PAID.
02243      MOVE CL-TOTAL-PAID-AMT      TO  CPA-AMOUNT-PAID-TO-DATE.
02244      MOVE AT-DAYS-IN-PERIOD      TO  CPA-DAYS-PAID.
02245      MOVE AT-DAILY-RATE          TO  CPA-DAILY-RATE.
02246      MOVE AT-ELIMINATION-DAYS    TO  CPA-ELIMINATION-DAYS.
02247      MOVE AT-BENEFIT-TYPE        TO  CPA-BENEFIT-TYPE.
02248      MOVE AT-EXPENSE-TYPE        TO  CPA-EXPENSE-TYPE.
02249      MOVE CL-NO-OF-PMTS-MADE     TO  CPA-NO-OF-PMTS-MADE.
02250      MOVE CL-PROCESSOR-ID        TO  CPA-EXAMINER.
02251      MOVE AT-PAYMENT-ORIGIN      TO  CPA-PAYMENT-ORIGIN.
02252      MOVE CL-CCN-A5              TO  CPA-CREDIT-CARD-NO.
02253
02254      IF AT-PAYMENT-ORIGIN = '2'
02255          MOVE 'Y'                TO  WS-AUTO-PAY-SW
02256      ELSE
02257          MOVE 'N'                TO  WS-AUTO-PAY-SW.
02258
02259      IF (PI-COMPANY-ID = 'FIM' OR 'FMK' OR 'HER') AND
02260         (AT-PAYMENT-ORIGIN = '2')
02261          MOVE 'AUTO'             TO  CPA-EXAMINER.
02262
02263 *    NOTE *******************************************************
02264 *         *      CLAIM TYPE      MEANING                        *
02265 *         *          1         DEATH CLAIM (INDIVIDUAL)         *
02266 *         *          2         DISABILITY CLAIM (INDIVIDUAL)    *
02267 *         *          3         OUTSTANDING BALANCE (DEATH)      *
02268 *         *          4         OUTSTANDING BALANCE (DISABILITY) *
02269 *         *******************************************************.
02270
100518     IF CL-CLAIM-TYPE = 'L' OR 'O'
02272          IF CL-CLAIM-PREM-TYPE = '2'
02273              MOVE '3'            TO  CPA-CLAIM-CODE
02274            ELSE
02275              MOVE '1'            TO  CPA-CLAIM-CODE
02276        ELSE
02277          IF CL-CLAIM-PREM-TYPE = '2'
02278              MOVE '4'            TO  CPA-CLAIM-CODE
02279            ELSE
02280              MOVE '2'            TO  CPA-CLAIM-CODE.
02281
02282 *    NOTE *******************************************************
02283 *         *      PAY CODE       MEANING                         *
02284 *         *         A        ADDITIONAL DEATH CLAIM             *
02285 *         *         P        PARTIAL PAYMENT                    *
02286 *         *         F        FINAL PAYMENT                      *
02287 *         *         S        LUMP SUM DISABILITY                *
02288 *         *******************************************************.
02289
02290      MOVE AT-PAYMENT-TYPE        TO  CPA-PAY-CODE
02291                                      WS-PAYMENT-TYPE.
02292      INSPECT CPA-PAY-CODE CONVERTING '123456789' TO 'PFSA     '.
02293
02294      MOVE CL-INCURRED-DT         TO  CPA-INCURRED-DT.
02295      MOVE CL-REPORTED-DT         TO  CPA-REPORTED-DT.
02296
02297      IF NOT PI-USES-PAID-TO
02298         MOVE AT-PAID-THRU-DT     TO  CPA-PAID-THRU-DT
02299      ELSE
02300         MOVE AT-PAID-THRU-DT     TO  DC-BIN-DATE-1
02301         MOVE +1                  TO  DC-ELAPSED-DAYS
02302         MOVE +0                  TO  DC-ELAPSED-MONTHS
02303         MOVE '6'                 TO  DC-OPTION-CODE
02304         PERFORM 8500-DATE-CONVERSION
02305         IF NO-CONVERSION-ERROR
02306            MOVE DC-BIN-DATE-2    TO  CPA-PAID-THRU-DT.
02307
02308      MOVE AT-PAID-FROM-DT        TO  CPA-PAID-FROM-DT.
02309      MOVE CL-LAST-PMT-DT         TO  CPA-PAID-DT.
02310
02311      MOVE WS-CARRIER-ADDRESS-DATA  TO  CPA-CARRIER-ADDRESS-DATA.
02312
02313      IF CL-SYSTEM-IDENTIFIER = 'CV'
02314          GO TO 0354-PROCESS-CV-CLAIM.
02315
02316      IF PI-COMPANY-ID = 'AIG' OR 'AUK'
02317          MOVE WS-AIG-INS-CO-NAME TO  CPA-COMPANY-NAME.
02318
02319      MOVE AM-NAME                TO  CPA-ACCOUNT-NAME.
02320      MOVE AM-PERSON              TO  CPA-ACCOUNT-IN-CARE-OF.
02321      MOVE AM-ADDRS               TO  CPA-ACCOUNT-ADDRESS-LINE1.
02322      MOVE SPACES                 TO  CPA-ACCOUNT-ADDRESS-LINE2.
02323      MOVE AM-CITY                TO  CPA-ACCOUNT-CITY-ST.
02324      MOVE AM-ZIP                 TO  CPA-ACCOUNT-ZIP-CODE.
02325      MOVE AM-TEL-NO              TO  WS-WORK-PHONE.
02326      INSPECT WS-WORK-PHONE CONVERTING SPACES TO '0'.
02327      MOVE WS-NUMERIC-PHONE       TO  CPA-ACCOUNT-PHONE-NO.
02328
02329      IF CM-SSN-STATE   = CM-STATE AND
02330         CM-SSN-ACCOUNT = CM-ACCOUNT-PRIME
02331          MOVE SPACES             TO  CPA-SOC-SEC-NO
02332      ELSE
02333          MOVE CM-SOC-SEC-NO      TO  CPA-SOC-SEC-NO.
02334
02335      MOVE CM-STATE               TO  WS-SSN-STATE
02336                                      WS-MEMBER-STATE.
02337      MOVE CM-ACCOUNT             TO  WS-SSN-ACCOUNT
02338                                      WS-MEMBER-ACCOUNT.
02339      MOVE CM-INSURED-LAST-NAME   TO  WS-SSN-LN3
02340                                      WS-MEMBER-LN4.
02341
02342      IF CM-MEMB-STATE   = CM-STATE AND
02343         CM-MEMB-ACCOUNT = CM-ACCOUNT-PRIME
02344          MOVE SPACES             TO  CPA-MEMBER-NUMBER
02345      ELSE
02346          MOVE CM-MEMBER-NO       TO  CPA-MEMBER-NUMBER.
02347
02348      IF PI-COMPANY-ID = 'DMD'
02349          MOVE CM-POLICY-FORM-NO  TO  CPA-MEMBER-NUMBER.
02350
CIDMOD*    MOVE CM-LOAN-NUMBER         TO  CPA-LOAN-NUMBER
CIDMOD*    MOVE CM-BENEFICIARY         TO  CPA-BENEFICIARY
02353
02354      GO TO 0354-CONT.
02355
02356  0354-PROCESS-CV-CLAIM.
02357
02358      MOVE PD-NAME               TO  CPA-ACCOUNT-NAME.
02359      MOVE PD-PERSON             TO  CPA-ACCOUNT-IN-CARE-OF.
02360      MOVE PD-ADDRS              TO  CPA-ACCOUNT-ADDRESS-LINE1.
02361      MOVE SPACES                TO  CPA-ACCOUNT-ADDRESS-LINE2.
02362      MOVE PD-CITY               TO  CPA-ACCOUNT-CITY-ST.
02363      MOVE PD-ZIP                TO  CPA-ACCOUNT-ZIP-CODE.
02364      MOVE PD-TEL-NO             TO  WS-WORK-PHONE.
02365      INSPECT WS-WORK-PHONE CONVERTING SPACES TO '0'.
02366      MOVE WS-NUMERIC-PHONE      TO  CPA-ACCOUNT-PHONE-NO.
02367
02368      IF PM-SSN-STATE    = PM-STATE AND
02369         PM-SSN-PRODUCER = PM-PRODUCER-PRIME
02370          MOVE SPACES             TO  CPA-SOC-SEC-NO
02371      ELSE
02372          MOVE PM-SOC-SEC-NO      TO  CPA-SOC-SEC-NO.
02373
02374      MOVE PM-LOAN-NUMBER         TO  CPA-CV-LOAN-NUMBER.
02375
02376  0354-CONT.
02377
02378      MOVE AT-PAYEE-TYPE-CD       TO  WS-PAYEE-CODE
02379                                      CPA-PAYEE-TYPE-CD.
02380
02381      MOVE WS-CHECK-WRITER-DATE   TO  AT-CHECK-WRITTEN-DT
02382                                      CQ-CHECK-WRITTEN-DT
02383                                      CPA-CHECK-DATE.
02384
02385      IF AOPTIONI NOT = '3'
02386          MOVE PI-MONTH-END-SAVE  TO  AT-PMT-SELECT-DT.
02387
02388      IF AOPTIONI = '3'
02389        AND CQ-PRE-NUMBERING-SW = '1'
02390          MOVE WS-SEQUENCE-NUMBER TO  AT-CHECK-QUE-SEQUENCE.
02391
02392      MOVE +1760                  TO  CQ-LAST-UPDATED-BY.
02393      MOVE EIBTIME                TO  CQ-LAST-UPDATED-HHMMSS.
02394      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
02395      MOVE '5'                    TO  DC-OPTION-CODE.
02396      PERFORM 8500-DATE-CONVERSION.
02397      MOVE DC-BIN-DATE-1          TO  CQ-LAST-UPDATED-DT.
02398
02399      
      * EXEC CICS REWRITE
02400 *        DATASET (WS-ACTIVITY-TRAILERS-DSID)
02401 *        FROM    (ACTIVITY-TRAILERS)
02402 *    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00008691' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303038363931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02403
02404      IF WS-PAYMENT-NOTE-SEQ-NO GREATER THAN +0 AND
02405         WS-PAYMENT-NOTE-SEQ-NO LESS THAN +4096
02406         MOVE WS-CLAIM-MASTER-KEY    TO  WS-ACTIVITY-TRAILERS-KEY
02407         MOVE WS-PAYMENT-NOTE-SEQ-NO TO  WS-ATK-SEQUENCE-NO
02408         MOVE 'TRLR'                 TO  FILE-SWITCH
02409         
      * EXEC CICS HANDLE CONDITION
02410 *            NOTFND  (0354-CHECK-FOR-AUTO-PAY)
02411 *       END-EXEC
      *    MOVE '"$I                   ! - #00008701' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2D20233030303038373031' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02412         
      * EXEC CICS READ
02413 *            DATASET (WS-ACTIVITY-TRAILERS-DSID)
02414 *            RIDFLD  (WS-ACTIVITY-TRAILERS-KEY)
02415 *            SET     (ADDRESS OF ACTIVITY-TRAILERS)
02416 *       END-EXEC
      *    MOVE '&"S        E          (   #00008704' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038373034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ACTIVITY-TRAILERS-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02417         IF AT-TRAILER-TYPE = '6'
02418            MOVE AT-INFO-LINE-1 TO CPA-COMMENT
02419            MOVE AT-INFO-LINE-2 TO CPA-COMMENT-2.
02420
02421  0354-CHECK-FOR-AUTO-PAY.
02422
02423      IF WS-AUTO-PAY-SW = 'N'
02424          GO TO 0355-GET-ADDRESS.
02425
02426      
      * EXEC CICS HANDLE CONDITION
02427 *        NOTFND   (0355-GET-ADDRESS)
02428 *    END-EXEC.
      *    MOVE '"$I                   ! . #00008718' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2E20233030303038373138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02429
02430      MOVE WS-CLAIM-MASTER-KEY        TO  WS-ACTIVITY-TRAILERS-KEY.
02431      MOVE CL-AUTO-PAY-SEQ            TO  WS-ATK-SEQUENCE-NO.
02432      MOVE 'TRLR'                     TO  FILE-SWITCH.
02433
02434      
      * EXEC CICS READ
02435 *        DATASET   (WS-ACTIVITY-TRAILERS-DSID)
02436 *        RIDFLD    (WS-ACTIVITY-TRAILERS-KEY)
02437 *        SET       (ADDRESS OF ACTIVITY-TRAILERS)
02438 *    END-EXEC.
      *    MOVE '&"S        E          (   #00008726' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038373236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ACTIVITY-TRAILERS-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02439
02440      IF AT-TRAILER-TYPE = '3'
02441          NEXT SENTENCE
02442      ELSE
02443          GO TO 0355-GET-ADDRESS.
02444
02445      MOVE AT-SCHEDULE-END-DT         TO  CPA-AUTO-PAY-END-DT.
02446
02447  0355-GET-ADDRESS.
02448
02449      IF CL-ACCOUNT-ADDR-CNT NOT = ZERO
02450         MOVE CL-ACCOUNT-ADDR-CNT TO WS-ATK-SEQUENCE-NO
02451         ADD +20    TO WS-ATK-SEQUENCE-NO
02452         PERFORM 0360-MAIN-LOGIC THRU 0360-EXIT
02453         MOVE CPA-PAYEE-NAME          TO  CPA-ACCOUNT-NAME
02454         MOVE CPA-PAYEE-ADDRESS-LINE2 TO  CPA-ACCOUNT-ADDRESS-LINE1
02455         MOVE CPA-PAYEE-ADDRESS-LINE3 TO  CPA-ACCOUNT-ADDRESS-LINE2
02456         MOVE CPA-PAYEE-CITY-STATE    TO  CPA-ACCOUNT-CITY-ST
02457         MOVE CPA-PAYEE-ZIP           TO  CPA-ACCOUNT-ZIP-CODE.
02458
030612     IF PI-COMPANY-ID = 'CID' OR 'DCC' OR 'AHL'
CIDMOD        MOVE +91                 TO WS-ATK-SEQUENCE-NO
CIDMOD        
      * EXEC CICS READ
CIDMOD*          DATASET (WS-ACTIVITY-TRAILERS-DSID)
CIDMOD*          RIDFLD  (WS-ACTIVITY-TRAILERS-KEY)
CIDMOD*          SET     (ADDRESS OF ACTIVITY-TRAILERS)
CIDMOD*          RESP    (WS-RESPONSE)
CIDMOD*       END-EXEC
      *    MOVE '&"S        E          (  N#00008753' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303038373533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ACTIVITY-TRAILERS-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
CIDMOD        IF WS-RESP-NORMAL
CIDMOD           MOVE AT-INFO-LINE-1   TO CPA-BENEFICIARY
CIDMOD        ELSE
CIDMOD           MOVE SPACES           TO CPA-BENEFICIARY
CIDMOD        END-IF
CIDMOD     END-IF
CIDMOD
02459      MOVE CL-INSURED-ADDR-CNT TO WS-ATK-SEQUENCE-NO.
02460
02461      PERFORM 0360-MAIN-LOGIC THRU 0360-EXIT.
02462
02463      MOVE CPA-PAYEE-NAME           TO  CPA-INSURED-ADDR-TRLR-NAME.
02464      MOVE CPA-PAYEE-ADDRESS-LINE1  TO  CPA-INSURED-ADDRESS-LINE1.
02465      MOVE CPA-PAYEE-ADDRESS-LINE2  TO  CPA-INSURED-ADDRESS-LINE2.
02466      MOVE CPA-PAYEE-ADDRESS-LINE3  TO  CPA-INSURED-ADDRESS-LINE3.
02467      MOVE CPA-PAYEE-CITY-STATE     TO  CPA-INSURED-CITY-STATE.
02468      MOVE CPA-PAYEE-ZIP            TO  CPA-INSURED-ZIP.
02469
02470      IF WS-PAYEE-CD = 'I'
02471         GO TO 0370-MAIN-LOGIC.
02472
02473      IF PI-COMPANY-ID = 'AIG' OR 'AUK'
02474        IF WS-PAYEE-CD = 'B'
02475          IF WS-PAYEE-SEQ-NUM = 0
02476            MOVE CL-BENEFICIARY           TO  WS-BK-BENEFICIARY
02477            PERFORM 2000-GET-BENEFICIARY
02478            GO TO 0370-MAIN-LOGIC
02479          ELSE
02480            IF WS-PAYEE-SEQ-NUM = 9
02481              MOVE WS-AIG-CREDITOR-NAME   TO  CPA-PAYEE-NAME
02482              GO TO 0370-MAIN-LOGIC
02483            ELSE
02484              MOVE WS-PAYEE-SEQ-NUM       TO  WS-ATK-SEQUENCE-NO
02485              ADD +10                     TO  WS-ATK-SEQUENCE-NO
02486              GO TO 0360-MAIN-LOGIC.
02487
02488      IF WS-PAYEE-CD = 'B'
02489         IF (CL-BENIF-ADDR-CNT = +0) OR
02490            (WS-PAYEE-SEQ-NUM = 0 OR 9)
02491             MOVE CL-BENEFICIARY        TO  WS-BK-BENEFICIARY
02492             PERFORM 2000-GET-BENEFICIARY
02493             GO TO 0370-MAIN-LOGIC
02494         ELSE
02495             MOVE WS-PAYEE-SEQ-NUM   TO  WS-ATK-SEQUENCE-NO
02496             ADD +10                 TO  WS-ATK-SEQUENCE-NO
02497             GO TO 0360-MAIN-LOGIC.
02498
02499      IF WS-PAYEE-CD = 'O'
02500         MOVE WS-PAYEE-SEQ-NUM      TO  WS-ATK-SEQUENCE-NO
02501         ADD +50                    TO  WS-ATK-SEQUENCE-NO
02502         GO TO 0360-MAIN-LOGIC.
02503
02504      IF WS-PAYEE-CD = 'Q'
02505         MOVE WS-PAYEE-SEQ-NUM      TO  WS-ATK-SEQUENCE-NO
02506         ADD +60                    TO  WS-ATK-SEQUENCE-NO
02507         GO TO 0360-MAIN-LOGIC.
02508
02509      IF WS-PAYEE-CD = 'P'
02510         MOVE WS-PAYEE-SEQ-NUM      TO  WS-ATK-SEQUENCE-NO
02511         ADD +30                    TO  WS-ATK-SEQUENCE-NO
02512         GO TO 0360-MAIN-LOGIC.
02513
02514      IF WS-PAYEE-CD = 'E'
02515         MOVE WS-PAYEE-SEQ-NUM      TO  WS-ATK-SEQUENCE-NO
02516         ADD +40                    TO  WS-ATK-SEQUENCE-NO
02517         GO TO 0360-MAIN-LOGIC.
02518
02519      IF (WS-PAYEE-CD = 'A') AND
02520         (CL-ACCOUNT-ADDR-CNT NOT = +0) AND
02521         (WS-PAYEE-SEQ-NUM GREATER THAN 0)
02522         MOVE WS-PAYEE-SEQ-NUM     TO  WS-ATK-SEQUENCE-NO
02523         ADD +20                   TO  WS-ATK-SEQUENCE-NO
02524         GO TO 0360-MAIN-LOGIC.
02525
02526      IF CL-SYSTEM-IDENTIFIER = 'CV'
02527          MOVE PD-NAME           TO  CPA-PAYEE-NAME
02528          MOVE PD-PERSON         TO  CPA-PAYEE-ADDRESS-LINE1
02529          MOVE PD-ADDRS          TO  CPA-PAYEE-ADDRESS-LINE2
02530          MOVE SPACES            TO  CPA-PAYEE-ADDRESS-LINE3
02531          MOVE PD-CITY           TO  CPA-PAYEE-CITY-STATE
02532          MOVE PD-ZIP            TO  CPA-PAYEE-ZIP
02533      ELSE
02534          MOVE AM-NAME           TO  CPA-PAYEE-NAME
02535          MOVE AM-PERSON         TO  CPA-PAYEE-ADDRESS-LINE1
02536          MOVE AM-ADDRS          TO  CPA-PAYEE-ADDRESS-LINE2
02537          MOVE SPACES            TO  CPA-PAYEE-ADDRESS-LINE3
02538          MOVE AM-CITY           TO  CPA-PAYEE-CITY-STATE
02539          MOVE AM-ZIP            TO  CPA-PAYEE-ZIP.
02540
02541      GO TO 0370-MAIN-LOGIC.
02542
02543  0360-MAIN-LOGIC.
02544      
      * EXEC CICS HANDLE CONDITION
02545 *        NOTFND (0360-NO-TRLR)
02546 *    END-EXEC.
      *    MOVE '"$I                   ! / #00008851' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2F20233030303038383531' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02547
02548      IF WS-ATK-SEQUENCE-NO = +0
02549         GO TO 0360-NO-TRLR
02550      ELSE
02551         
      * EXEC CICS READ
02552 *            DATASET (WS-ACTIVITY-TRAILERS-DSID)
02553 *            RIDFLD  (WS-ACTIVITY-TRAILERS-KEY)
02554 *            SET     (ADDRESS OF ACTIVITY-TRAILERS)
02555 *       END-EXEC
      *    MOVE '&"S        E          (   #00008858' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038383538' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ACTIVITY-TRAILERS-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ACTIVITY-TRAILERS-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02556         MOVE AT-MAIL-TO-NAME    TO  CPA-PAYEE-NAME
02557         MOVE AT-ADDRESS-LINE-1  TO  CPA-PAYEE-ADDRESS-LINE2
02558         MOVE AT-ADDRESS-LINE-2  TO  CPA-PAYEE-ADDRESS-LINE3
02559         MOVE AT-CITY-STATE      TO  CPA-PAYEE-CITY-STATE
02560         MOVE AT-ZIP             TO  CPA-PAYEE-ZIP.
02561
02562      GO TO 0360-EXIT.
02563
02564  0360-NO-TRLR.
02565      MOVE WS-NAME-WORK           TO  CPA-PAYEE-NAME.
02566      MOVE ZERO                   TO  CPA-PAYEE-ZIP.
02567      MOVE SPACES                 TO  CPA-PAYEE-ADDRESS-LINE2
02568                                      CPA-PAYEE-ADDRESS-LINE3
02569                                      CPA-PAYEE-CITY-STATE.
02570      MOVE +1                     TO  WS-COMPLETED-SUCCESSFUL.
02571  0360-EXIT.
02572      EXIT.
02573
02574  0370-MAIN-LOGIC.
02575      IF CL-INSURED-BIRTH-DT NOT = LOW-VALUES
02576          MOVE EIBDATE            TO  DC-JULIAN-YYDDD
02577          MOVE '5'                TO  DC-OPTION-CODE
02578          PERFORM 8500-DATE-CONVERSION
02579          MOVE CL-INSURED-BIRTH-DT TO DC-BIN-DATE-2
02580          MOVE '1'                TO  DC-OPTION-CODE
02581          PERFORM 8500-DATE-CONVERSION
02582          DIVIDE DC-ELAPSED-MONTHS BY +12 GIVING CPA-INSURED-AGE
02583        ELSE
02584          MOVE ZERO               TO  CPA-INSURED-AGE.
02585
02586      IF CL-SYSTEM-IDENTIFIER = 'CV'
02587          GO TO 0372-READ-EMPLAN.
02588
052614     IF CL-CLAIM-TYPE = 'A' OR 'I' OR 'G' or 'F'
080322                            OR 'B' OR 'H'
02590          MOVE PI-COMPANY-ID      TO  WS-CFK-COMPANY-ID
02591          MOVE '5'                TO  WS-CFK-RECORD-TYPE
02592          MOVE CM-AH-BENEFIT-CD   TO  WS-CFK-BENEFIT-NO
02593                                      WS-BENEFIT-NO
02594          PERFORM 8700-LOCATE-BENEFIT
02595          IF WS-KIND NOT = SPACES
02596             MOVE WS-COV-TYPE        TO  CPA-COVERAGE-TYPE
02597             MOVE WS-RETRO-ELIM      TO  CPA-BENEFIT-TYPE
02598             MOVE WS-RETRO-DAYS      TO  CPA-ELIMINATION-DAYS
02599             MOVE CM-AH-BENEFIT-AMT  TO  CPA-MONTHLY-BENEFIT
02600           ELSE
02601             MOVE ZERO               TO  CPA-ELIMINATION-DAYS
02602                                         CPA-MONTHLY-BENEFIT
02603        ELSE
02604          MOVE ZERO               TO  CPA-ELIMINATION-DAYS
02605                                      CPA-MONTHLY-BENEFIT
02606          MOVE PI-COMPANY-ID      TO  WS-CFK-COMPANY-ID
02607          MOVE '4'                TO  WS-CFK-RECORD-TYPE
02608          MOVE CM-LF-BENEFIT-CD   TO  WS-CFK-BENEFIT-NO
02609                                      WS-BENEFIT-NO
02610          PERFORM 8700-LOCATE-BENEFIT
02611          IF WS-KIND NOT = SPACES
02612             MOVE WS-COV-TYPE        TO  CPA-COVERAGE-TYPE.
02613
02614      IF CM-CERT-EFF-DT = LOW-VALUES
02615          GO TO 0375-MAIN-LOGIC.
02616
02617      MOVE CM-CERT-EFF-DT         TO  DC-BIN-DATE-1.
02618
052614     IF CL-CLAIM-TYPE = 'A' OR 'I' OR 'G' or 'F'
080322                            OR 'B' OR 'H'
02620          MOVE CM-AH-ORIG-TERM    TO  DC-ELAPSED-MONTHS
02621          MOVE CM-AH-BENEFIT-CD   TO  CPA-BENEFIT-CD
02622        ELSE
02623          MOVE CM-LF-BENEFIT-AMT  TO  CPA-TOTAL-BENEFIT
02624          MOVE CM-LF-BENEFIT-CD   TO  CPA-BENEFIT-CD
02625          MOVE CM-LF-ORIG-TERM    TO  DC-ELAPSED-MONTHS.
02626
02627      MOVE ZERO                   TO  DC-ELAPSED-DAYS
02628      MOVE '6'                    TO  DC-OPTION-CODE
02629      PERFORM 8500-DATE-CONVERSION
02630
02631      MOVE DC-BIN-DATE-2          TO  CPA-EXPIRE-DT.
02632
02633      GO TO 0375-MAIN-LOGIC.
02634
02635      EJECT
02636  0372-READ-EMPLAN.
02637
02638      
      * EXEC CICS HANDLE CONDITION
02639 *        NOTFND   (0375-MAIN-LOGIC)
02640 *    END-EXEC.
      *    MOVE '"$I                   ! 0 #00008947' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3020233030303038393437' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02641
02642      MOVE PM-COMPANY-CD          TO  WS-PP-COMPANY-CD.
02643      MOVE PM-CARRIER             TO  WS-PP-CARRIER.
02644      MOVE PM-GROUPING            TO  WS-PP-GROUPING.
02645      MOVE PM-STATE               TO  WS-PP-STATE.
02646      MOVE PM-PRODUCER            TO  WS-PP-PRODUCER.
02647      MOVE PM-INS-PLAN-CD         TO  WS-PP-PLAN-CODE.
02648      MOVE PM-INS-PLAN-REVISION   TO  WS-PP-REV-NO.
02649      MOVE 'PLAN'                 TO  FILE-SWITCH.
02650
02651      
      * EXEC CICS READ
02652 *        DATASET   (WS-PLAN-MASTER-DSID)
02653 *        RIDFLD    (WS-PLAN-MASTER-KEY)
02654 *        SET       (ADDRESS OF PRODUCER-PLANS)
02655 *    END-EXEC.
      *    MOVE '&"S        E          (   #00008960' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038393630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-PLAN-MASTER-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-PLAN-MASTER-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PRODUCER-PLANS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02656
02657      MOVE PP-BENEFIT-TYPE            TO  CPA-COVERAGE-TYPE.
02658
02659      IF PM-AH-MORT-PLAN
02660          MOVE PM-INS-MONTH-BENEFIT   TO  CPA-MONTHLY-BENEFIT
02661      ELSE
02662          MOVE PM-INS-TOTAL-BENEFIT   TO  CPA-TOTAL-BENEFIT.
02663
02664      MOVE PM-INS-PLAN-CD             TO  CPA-BENEFIT-CD.
02665
02666      IF PM-POLICY-EFF-DT = LOW-VALUES
02667          GO TO 0375-MAIN-LOGIC.
02668
02669      MOVE PM-POLICY-EFF-DT           TO  DC-BIN-DATE-1.
02670      MOVE PM-LOAN-TERM               TO  DC-ELAPSED-MONTHS.
02671      MOVE +0                         TO  DC-ELAPSED-DAYS.
02672      MOVE '6'                        TO  DC-OPTION-CODE.
02673      PERFORM 8500-DATE-CONVERSION.
02674      MOVE DC-BIN-DATE-2              TO  CPA-EXPIRE-DT.
02675      EJECT
02676  0375-MAIN-LOGIC.
02677      ADD +1  TO  CQ-TIMES-PRINTED.
02678
02679      IF AOPTIONI = '3'
02680        AND CQ-PRE-NUMBERING-SW = '1'
02681          MOVE CHECK-QUE          TO  WS-NEW-CHECK-QUEUE-RECORD
02682          MOVE WS-OLD-CHECK-QUEUE-RECORD  TO  CHECK-QUE
02683          MOVE SPACES             TO  CQ-PAYEE-BENE-ACCT
02684          MOVE 'S'                TO  CQ-ENTRY-TYPE.
02685          MOVE LOW-VALUES         TO  CQ-APPLIED-TO-RCON-DT.
02686
02687      MOVE +1760                  TO  CQ-LAST-UPDATED-BY.
02688      MOVE EIBTIME                TO  CQ-LAST-UPDATED-HHMMSS.
02689      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
02690      MOVE '5'                    TO  DC-OPTION-CODE.
02691      PERFORM 8500-DATE-CONVERSION.
02692      MOVE DC-BIN-DATE-1          TO  CQ-LAST-UPDATED-DT.
02693
02694      
      * EXEC CICS REWRITE
02695 *        DATASET (WS-CHECK-QUEUE-DSID)
02696 *        FROM    (CHECK-QUE)
02697 *    END-EXEC.
           MOVE LENGTH OF
            CHECK-QUE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00009003' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303039303033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-DSID, 
                 CHECK-QUE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02698
02699      IF AOPTIONI NOT = '3'
02700          GO TO 0380-MAIN-LOGIC.
02701
02702      IF WS-ELCHKQ-GETMAIN-SW = 'Y'
02703          MOVE 'N'                TO  WS-ELCHKQ-GETMAIN-SW
02704          
      * EXEC CICS GETMAIN
02705 *            SET    (WS-ELCHKQ-POINTER)
02706 *            LENGTH (100)
02707 *        END-EXEC.
           MOVE 100
             TO DFHEIV11
      *    MOVE '," L                  $   #00009013' TO DFHEIV0
           MOVE X'2C22204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303039303133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-ELCHKQ-POINTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02708
02709      MOVE WS-ELCHKQ-POINTER             TO LCP-WS-ADDR-COMP
02710      SET ADDRESS OF CHECK-QUE TO LCP-WS-ADDR-PNTR.
02711
02712      MOVE WS-NEW-CHECK-QUEUE-RECORD  TO  CHECK-QUE
02713
02714      IF CQ-PRE-NUMBERING-SW NOT = '1'
02715          GO TO 0380-MAIN-LOGIC.
02716
02717      MOVE 'Q'                    TO  CQ-ENTRY-TYPE.
02718
02719      MOVE SPACE                  TO  CQ-LEDGER-FLAG.
02720
02721      MOVE WS-SEQUENCE-NUMBER     TO  CQ-SEQUENCE-NUMBER
02722                                      CQ-SEQUENCE-NUMBER-A1.
02723
02724      ADD +1  TO  WS-SEQUENCE-NUMBER.
02725
02726      MOVE +1760                  TO  CQ-LAST-UPDATED-BY.
02727      MOVE EIBTIME                TO  CQ-LAST-UPDATED-HHMMSS.
02728      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
02729      MOVE '5'                    TO  DC-OPTION-CODE.
02730      PERFORM 8500-DATE-CONVERSION.
02731      MOVE DC-BIN-DATE-1          TO  CQ-LAST-UPDATED-DT.
02732      MOVE LOW-VALUES             TO  CQ-APPLIED-TO-RCON-DT.
02733
02734      
      * EXEC CICS WRITE
02735 *        DATASET (WS-CHECK-QUEUE-DSID)
02736 *        RIDFLD  (CQ-CONTROL-PRIMARY)
02737 *        FROM    (CHECK-QUE)
02738 *    END-EXEC.
           MOVE LENGTH OF
            CHECK-QUE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00009043' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303039303433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-DSID, 
                 CHECK-QUE, 
                 DFHEIV11, 
                 CQ-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02739
02740  0380-MAIN-LOGIC.
02741      PERFORM 0800-PRINT-CHECK.
02742
02743      
      * EXEC CICS STARTBR
02744 *        DATASET (WS-CHECK-QUEUE-AIX-DSID)
02745 *        RIDFLD  (WS-CHECK-AIX-KEY)
02746 *        GTEQ
02747 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00009052' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303039303532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-AIX-DSID, 
                 WS-CHECK-AIX-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02748
02749      GO TO 0320-MAIN-LOGIC.
02750
02751      EJECT
02752  0390-MAIN-LOGIC.
02753      IF AOPTIONI NOT = '1'
02754        AND PI-INDEX LESS THAN PI-NUMBER-OF-CONTROL-GROUPS
02755          SET PI-INDEX UP BY +1
02756          GO TO 0310-MAIN-LOGIC.
02757
02758      
      * EXEC CICS ENDBR
02759 *        DATASET (WS-CHECK-QUEUE-AIX-DSID)
02760 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00009067' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303039303637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CHECK-QUEUE-AIX-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02761
02762      IF PI-CHECK-PRINTER-ID NOT = 'R2T7'
02763          PERFORM 0700-END-PRINT.
02764
02765      MOVE PI-PRINTER-STARTED-SW  TO  WS-PRINTER-STARTED-SW.
02766      MOVE PI-TEMP-STORAGE-KEY    TO  WS-TEMP-STORAGE-KEY.
02767      ADD +1  TO  WS-COMPLETED-SUCCESSFUL.
02768      GO TO 0015-MAIN-LOGIC.
02769
02770  0700-END-PRINT SECTION.
02771      MOVE HIGH-VALUES            TO  CHECK-PASS-AREA.
02772      MOVE +1                     TO  WS-TS-LENGTH.
02773
02774      PERFORM 0800-PRINT-CHECK.
02775
02776      MOVE ZERO                   TO  PI-PRINTER-STARTED-SW.
02777
02778  0700-EXIT.
02779      EXIT.
02780
02781  0800-PRINT-CHECK SECTION.
02782
02783      IF PI-COMPANY-ID = 'JAL' OR 'JAI'
02784         NEXT SENTENCE
02785      ELSE
02786         
      * EXEC CICS WRITEQ TS
02787 *            QUEUE  (PI-TEMP-STORAGE-KEY)
02788 *            ITEM   (WS-TEMP-STORAGE-ITEM)
02789 *            FROM   (CHECK-PASS-AREA)
02790 *            LENGTH (WS-TS-LENGTH)
02791 *       END-EXEC.
      *    MOVE '*" I   L              ''   #00009095' TO DFHEIV0
           MOVE X'2A2220492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303039303935' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-TEMP-STORAGE-KEY, 
                 CHECK-PASS-AREA, 
                 WS-TS-LENGTH, 
                 WS-TEMP-STORAGE-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02792
02793  0800-EXIT.
02794      EXIT.
02795
02796      EJECT
02797  0900-TERMIDERR SECTION.
02798      
      * EXEC CICS SYNCPOINT
02799 *        ROLLBACK
02800 *    END-EXEC.
      *    MOVE '6"R                   !   #00009107' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303039313037' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02801
02802      MOVE ER-0371                TO  EMI-ERROR.
02803      MOVE -1                     TO  APFKL.
02804      PERFORM 8200-SEND-DATAONLY.
02805      PERFORM 9100-RETURN-TRAN.
02806
02807  0910-ENQ-BUSY.
02808      MOVE ER-0395                TO  EMI-ERROR.
02809      MOVE -1                     TO  AOPTIONL.
02810      PERFORM 8200-SEND-DATAONLY.
02811      PERFORM 9100-RETURN-TRAN.
02812
02813      EJECT
02814  1000-GET-CARRIER-NAME SECTION.
02815      
      * EXEC CICS HANDLE CONDITION
02816 *        NOTFND (1020-CARRIER-NOT-FOUND)
02817 *    END-EXEC.
      *    MOVE '"$I                   ! 1 #00009124' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3120233030303039313234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02818
02819      MOVE PI-COMPANY-ID          TO  WS-CONTROL-FILE-KEY.
02820      MOVE '6'                    TO  WS-CFK-RECORD-TYPE.
02821      MOVE CQ-CARRIER             TO  WS-CFK-CARRIER.
02822      MOVE ZERO                   TO  WS-CFK-SEQUENCE-NO.
02823      MOVE 'CNTL'                 TO  FILE-SWITCH.
02824
02825      
      * EXEC CICS READ
02826 *        DATASET (WS-CONTROL-FILE-DSID)
02827 *        RIDFLD  (WS-CONTROL-FILE-KEY)
02828 *        SET     (ADDRESS OF CONTROL-FILE)
02829 *    END-EXEC.
      *    MOVE '&"S        E          (   #00009134' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039313334' TO DFHEIV0(25:11)
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
           
02830
02831      MOVE CF-MAIL-TO-NAME        TO  WS-CARRIER-MAIL-TO-NAME.
02832      MOVE CF-IN-CARE-OF          TO  WS-CARRIER-IN-CARE-OF.
02833      MOVE CF-ADDRESS-LINE-1      TO  WS-CARRIER-ADDRESS-LINE-1.
02834      MOVE CF-ADDRESS-LINE-2      TO  WS-CARRIER-ADDRESS-LINE-2.
02835      MOVE CF-CITY-STATE          TO  WS-CARRIER-CITY-STATE.
02836
02837      IF CF-ZIP-CODE-NUM NOT NUMERIC
02838          MOVE ZEROS              TO CF-ZIP-CODE-NUM.
02839      IF CF-ZIP-CODE-NUM NOT = ZEROS
02840          MOVE CF-ZIP-CODE-NUM    TO  WS-ZIP-UNPACKED
02841          MOVE WS-ZIP-UNPACKED    TO  WS-CARRIER-ZIP-CODE
02842      ELSE
02843          MOVE CF-ZIP-CODE        TO  WS-CARRIER-ZIP-CODE.
02844
02845      MOVE CF-PHONE-NO            TO  WS-CARRIER-PHONE-NO.
02846      GO TO 1090-EXIT.
02847
02848  1020-CARRIER-NOT-FOUND.
02849      MOVE SPACES                 TO  WS-CARRIER-ADDRESS-DATA.
02850
02851  1090-EXIT.
02852      EXIT.
02853
02854      EJECT
02855  2000-GET-BENEFICIARY SECTION.
02856      
      * EXEC CICS HANDLE CONDITION
02857 *        NOTFND (2080-GET-BENEFICIARY)
02858 *    END-EXEC.
      *    MOVE '"$I                   ! 2 #00009165' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3220233030303039313635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02859
02860      MOVE PI-COMPANY-CD          TO  WS-BK-COMPANY-CD.
02861      MOVE 'B'                    TO  WS-BK-RECORD-TYPE.
02862
02863      
      * EXEC CICS READ
02864 *        DATASET (WS-BENEFICIARY-MASTER-DSID)
02865 *        RIDFLD  (WS-BENEFICIARY-KEY)
02866 *        SET     (ADDRESS OF BENEFICIARY-MASTER)
02867 *    END-EXEC.
      *    MOVE '&"S        E          (   #00009172' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039313732' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-BENEFICIARY-MASTER-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-BENEFICIARY-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF BENEFICIARY-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02868
02869      MOVE BE-MAIL-TO-NAME        TO  CPA-PAYEE-NAME.
02870      MOVE BE-ADDRESS-LINE-1      TO  CPA-PAYEE-ADDRESS-LINE1.
02871      MOVE BE-ADDRESS-LINE-2      TO  CPA-PAYEE-ADDRESS-LINE2.
02872      MOVE SPACES                 TO  CPA-PAYEE-ADDRESS-LINE3.
02873      MOVE BE-CITY-STATE          TO  CPA-PAYEE-CITY-STATE.
02874      MOVE BE-ZIP-CODE            TO  CPA-PAYEE-ZIP.
02875
02876      GO TO 2090-EXIT.
02877
02878  2080-GET-BENEFICIARY.
02879      MOVE SPACES                 TO  CPA-PAYEE-NAME
02880                                      CPA-PAYEE-ADDRESS-LINE1
02881                                      CPA-PAYEE-ADDRESS-LINE2
02882                                      CPA-PAYEE-CITY-STATE.
02883      MOVE ZERO                   TO  CPA-PAYEE-ZIP.
02884
02885  2090-EXIT.
02886      EXIT.
02887
02888      EJECT
02889  5000-MOVE-NAME SECTION. 
      *                        COPY ELCMNS.
00001 *****************************************************************
00002 *                                                               *
00002 *                                                               *
00003 *                            ELCMNS.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.002                         *
00006 *                                                               *
00007 *                     M O V E   N A M E   R O U T I N E         *
00008 *                                                               *
00009 *                THE FOLLOWING ROUTINE MOVES THE INSURRED'S     *
00010 *            NAME FROM THE CLAIM MASTER TO A WORK AREA WITH     *
00011 *            NO EMBEDDED BLANKS.                                *
00012 *                                                               *
00013 *                  FIELD               VALUE                    *
00014 *                                                               *
00015 *                LAST NAME (CL15)      SMITH                    *
00016 *                1ST NAME  (CL12)      JOHN                     *
00017 *                MID NAME  (CL12)      ALLEN                    *
00018 *                                                               *
00019 *                AFTER NAME HAS BEEN MOVED WS-NAME-WORK (CL30)  *
00020 *                                                               *
00021 *                        SMITH, JOHN ALLEN                      *
00022 *                                                               *
00023 *                TO USE THIS ROUTINE YOU ALSO NEED A WORKING    *
00024 *            STORAGE COPYBOOK:                                  *
00025 *                                                               *
00026 *                01  WS-NAME-WORK-AREA COPY ELCNWA.             *
00027 *                                                               *
00028 *****************************************************************.
00029
00030      MOVE SPACES                 TO  WS-NAME-WORK-AREA.
00031      MOVE ZERO                   TO  WS-NAME-SW.
00032      SET NWA-INDEX TO +1.
00033
00034      IF CL-INSURED-1ST-NAME = SPACES  AND
00035         CL-INSURED-MID-INIT = SPACES
00036          MOVE +1                 TO  WS-NAME-SW.
00037
00038      MOVE CL-INSURED-LAST-NAME  TO  WS-NAME-WORK2.
00039      PERFORM 5100-MOVE-NAME THRU 5190-EXIT.
00040
00041      MOVE CL-INSURED-1ST-NAME   TO  WS-NAME-WORK2.
00042      PERFORM 5100-MOVE-NAME THRU 5190-EXIT.
00043
00044      SET NWA-INDEX UP BY +1.
00045      MOVE CL-INSURED-MID-INIT   TO  WS-NAME-WORK2.
00046      PERFORM 5100-MOVE-NAME THRU 5190-EXIT.
00047
00048  5000-EXIT.
00049      EXIT.
00050
00051      EJECT
00052  5100-MOVE-NAME SECTION.
00053      IF WS-NAME-SW GREATER THAN +1
00054          GO TO 5190-EXIT.
00055
00056      IF WS-NAME-WORK2 = SPACES
00057          GO TO 5190-EXIT.
00058
00059      SET NWA-INDEX2 TO +1.
00060      SET NWA-INDEX3 TO +2.
00061
00062  5110-MOVE-NAME.
00063      MOVE WS-NW2 (NWA-INDEX2)  TO  WS-NW (NWA-INDEX).
00064
00065      IF NWA-INDEX LESS THAN +30
00066          SET NWA-INDEX UP BY +1
00067        ELSE
00068          ADD +2  TO  WS-NAME-SW
00069          GO TO 5190-EXIT.
00070
00071      IF NWA-INDEX2 LESS THAN +20
00072          SET NWA-INDEX3 UP BY +1
00073          SET NWA-INDEX2 UP BY +1.
00074
00075      IF WS-NW2 (NWA-INDEX2) = SPACES AND
00076         WS-NW2 (NWA-INDEX3) = SPACES
00077          IF WS-NAME-SW = ZERO
00078              MOVE ','            TO  WS-NW (NWA-INDEX)
00079              SET NWA-INDEX UP BY +2
00080              MOVE +1             TO  WS-NAME-SW
00081              GO TO 5190-EXIT
00082            ELSE
00083              GO TO 5190-EXIT.
00084
00085      GO TO 5110-MOVE-NAME.
00086
00087  5190-EXIT.
00088      EXIT.
00089
02890
CIDMOD
CIDMOD 5000-MICR-CLOSED SECTION.
CIDMOD
CIDMOD     MOVE +9836 TO EMI-ERROR.
CIDMOD     MOVE ZERO TO PI-PROCESSING-SW.
CIDMOD     PERFORM 8200-SEND-DATAONLY.
CIDMOD
CIDMOD 5000-MICR-EXIT.
CIDMOD
CIDMOD*5000-FLAG-CLOSED SECTION.
CIDMOD*
CIDMOD*    MOVE +9837 TO EMI-ERROR.
CIDMOD*    MOVE ZERO TO PI-PROCESSING-SW.
CIDMOD*    PERFORM 8200-SEND-DATAONLY.
CIDMOD*
CIDMOD*5000-FLAG-EXIT.
CIDMOD
02891      EJECT
02892  8100-SEND-INITIAL-MAP SECTION.
02893      IF EMI-ERROR NOT = ZERO
02894          PERFORM 9900-ERROR-FORMAT
02895        ELSE
02896          IF TRANSACTION-SUCCESSFUL
02897              PERFORM 9900-ERROR-FORMAT
02898              IF CHECKS-WITHOUT-ADDRESSES
02899                  MOVE ER-0364       TO  EMI-ERROR
02900                  PERFORM 9900-ERROR-FORMAT.
02901
02902      MOVE EIBTIME                TO  TIME-IN.
02903
02904      MOVE SAVE-DATE              TO  ADATEO.
02905      MOVE TIME-OUT               TO  ATIMEO.
02906      MOVE EMI-MESSAGE-AREA (1)   TO  AEMSG1O.
02907      MOVE EMI-MESSAGE-AREA (2)   TO  AEMSG2O.
02908      MOVE EMI-MESSAGE-AREA (3)   TO  AEMSG3O.
02909
02910      
      * EXEC CICS SEND
02911 *        FROM   (EL176AI)
02912 *        MAPSET (WS-MAPSET-NAME)
02913 *        MAP    (WS-MAP-NAME)
02914 *        CURSOR ERASE
02915 *    END-EXEC.
           MOVE LENGTH OF
            EL176AI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00009327' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303039333237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL176AI, 
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
           
02916
02917      PERFORM 9100-RETURN-TRAN.
02918
02919  8100-EXIT.
02920      EXIT.
02921
02922      EJECT
02923  8200-SEND-DATAONLY SECTION.
02924      IF EMI-ERROR NOT = 3130
02925          PERFORM 9900-ERROR-FORMAT
02926        ELSE
02927          IF TRANSACTION-SUCCESSFUL
02928              PERFORM 9900-ERROR-FORMAT
02929              IF CHECKS-WITHOUT-ADDRESSES
02930                  MOVE ER-0364       TO  EMI-ERROR
02931                  PERFORM 9900-ERROR-FORMAT.
02932
02933      MOVE EIBTIME                TO  TIME-IN.
02934
02935      MOVE SAVE-DATE              TO  ADATEO.
02936      MOVE TIME-OUT               TO  ATIMEO.
02937      MOVE EMI-MESSAGE-AREA (1)   TO  AEMSG1O.
02938      MOVE EMI-MESSAGE-AREA (2)   TO  AEMSG2O.
02939      MOVE EMI-MESSAGE-AREA (3)   TO  AEMSG3O.
02940
02941      
      * EXEC CICS SEND DATAONLY
02942 *        FROM   (EL176AI)
02943 *        MAPSET (WS-MAPSET-NAME)
02944 *        MAP    (WS-MAP-NAME)
02945 *        CURSOR
02946 *    END-EXEC.
           MOVE LENGTH OF
            EL176AI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00009358' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303039333538' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL176AI, 
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
           
02947
02948      IF PI-PROCESSING-SW = ZERO
02949          PERFORM 9100-RETURN-TRAN.
02950
02951      MOVE ZERO                   TO  EMI-SUB
02952                                      EMI-NOTE-CTR
02953                                      EMI-WARNING-CTR
02954                                      EMI-FORCABLE-CTR
02955                                      EMI-FATAL-CTR.
02956
02957      MOVE '1'                    TO  EMI-SWITCH-AREA-1
02958                                      EMI-SWITCH-AREA-2.
02959
02960      MOVE SPACES                    TO  EMI-ERROR-LINES.
02961
02962  8200-EXIT.
02963      EXIT.
02964
02965      EJECT
02966  8300-SEND-TEXT SECTION.
02967      IF PI-PRINTER-STARTED-SW NOT = ZERO
02968          PERFORM 0700-END-PRINT.
02969
02970      
      * EXEC CICS SEND TEXT
02971 *        FROM   (LOGOFF-TEXT)
02972 *        LENGTH (LOGOFF-LENGTH)
02973 *        ERASE  FREEKB
02974 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00009387' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303039333837' TO DFHEIV0(25:11)
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
           
02975
02976      
      * EXEC CICS RETURN
02977 *    END-EXEC.
      *    MOVE '.(                    ''   #00009393' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303039333933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02978
02979  8300-EXIT.
02980      EXIT.
02981
02982      EJECT
02983  8500-DATE-CONVERSION SECTION.
02984      
      * EXEC CICS LINK
02985 *        PROGRAM  ('ELDATCV')
02986 *        COMMAREA (DATE-CONVERSION-DATA)
02987 *        LENGTH   (DC-COMM-LENGTH)
02988 *    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   (   #00009401' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039343031' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02989
02990  8500-EXIT.
02991      EXIT.
02992
02993      EJECT
02994  8700-LOCATE-BENEFIT SECTION.
02995      
      * EXEC CICS HANDLE CONDITION
02996 *        NOTFND (8700-EXIT)
02997 *    END-EXEC.
      *    MOVE '"$I                   ! 3 #00009412' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3320233030303039343132' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02998
02999      MOVE SPACES                 TO  WS-KIND.
03000
03001      MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.
03002      MOVE WS-BENEFIT-NO          TO  WS-CFK-BENEFIT-NO.
03003      MOVE 'CNTL'                 TO  FILE-SWITCH.
03004
03005      
      * EXEC CICS READ
03006 *        DATASET (WS-CONTROL-FILE-DSID)
03007 *        RIDFLD  (WS-CONTROL-FILE-KEY)
03008 *        SET     (ADDRESS OF CONTROL-FILE)
03009 *        GTEQ
03010 *    END-EXEC.
      *    MOVE '&"S        G          (   #00009422' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039343232' TO DFHEIV0(25:11)
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
           
03011
03012      IF WS-CFK-COMPANY-ID NOT = CF-COMPANY-ID
03013        OR WS-CFK-RECORD-TYPE NOT = CF-RECORD-TYPE
03014          GO TO 8700-EXIT.
03015
03016      MOVE +1                     TO  WS-INDEX.
03017
03018  8700-LOOKUP-BENEFIT.
03019      IF WS-BENEFIT-NO = CF-BENEFIT-CODE (WS-INDEX)
03020          MOVE CF-BENEFIT-ALPHA (WS-INDEX)    TO WS-KIND
03021          MOVE CF-LF-COVERAGE-TYPE (WS-INDEX) TO WS-COV-TYPE
03022          GO TO 8700-EXIT.
03023
03024      IF CF-BENEFIT-CODE (WS-INDEX) NOT LESS CF-HI-BEN-IN-REC
03025          GO TO 8700-EXIT.
03026
03027      IF WS-INDEX LESS THAN +8
03028          ADD +1  TO  WS-INDEX
03029          GO TO 8700-LOOKUP-BENEFIT.
03030
03031  8700-EXIT.
03032      EXIT.
03033
03034      EJECT
03035  8800-NOT-OPEN    SECTION.
03036
03037      IF FILE-SWITCH = 'BENE'
03038          MOVE ER-7675            TO  EMI-ERROR.
03039
03040      IF FILE-SWITCH = 'ACCT'
03041          MOVE ER-0168            TO  EMI-ERROR.
03042
03043      IF FILE-SWITCH = 'CHKQ'
03044          MOVE ER-3776            TO  EMI-ERROR.
03045
03046      IF FILE-SWITCH = 'MSTR'
03047          MOVE ER-0154            TO  EMI-ERROR.
03048
03049      IF FILE-SWITCH = 'CERT'
03050          MOVE ER-0169            TO  EMI-ERROR.
03051
03052      IF FILE-SWITCH = 'PLCY'
03053          MOVE ER-9883            TO  EMI-ERROR.
03054
03055      IF FILE-SWITCH = 'PROD'
03056          MOVE ER-9886            TO  EMI-ERROR.
03057
03058      IF FILE-SWITCH = 'TRLR'
03059          MOVE ER-0172            TO  EMI-ERROR.
03060
03061      IF FILE-SWITCH = 'COMP'
03062          MOVE ER-2055            TO  EMI-ERROR.
03063
03064      IF FILE-SWITCH = 'PLAN'
03065          MOVE ER-9808            TO  EMI-ERROR.
03066
03067      IF FILE-SWITCH = 'FLAG'
03068          MOVE ER-3027            TO  EMI-ERROR.
03069
03070      IF FILE-SWITCH = 'DRFT'
03071          MOVE ER-3028            TO  EMI-ERROR.
03072
03073      MOVE ZERO                   TO  PI-PROCESSING-SW.
03074      MOVE -1                     TO  AOPTIONL.
03075      GO TO 8200-SEND-DATAONLY.
03076      EJECT
03077  9000-RETURN-CICS SECTION.
03078      MOVE 'EL005'                TO  THIS-PGM.
03079      MOVE EIBAID                 TO  PI-ENTRY-CD-1.
03080      PERFORM 9300-XCTL.
03081
03082  9000-EXIT.
03083      EXIT.
03084
03085  9100-RETURN-TRAN SECTION.
03086      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.
03087      MOVE WS-MAP-NUMBER          TO  PI-CURRENT-SCREEN-NO.
03088
03089      
      * EXEC CICS RETURN
03090 *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
03091 *        LENGTH   (PI-COMM-LENGTH)
03092 *        TRANSID  (WS-TRANS-ID)
03093 *    END-EXEC.
      *    MOVE '.(CT                  ''   #00009506' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303039353036' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03094
03095  9100-EXIT.
03096      EXIT.
03097
03098  9300-XCTL SECTION.
03099      IF PI-PRINTER-STARTED-SW NOT = ZERO
03100          PERFORM 0700-END-PRINT.
03101
03102      MOVE DFHENTER               TO  EIBAID.
03103
03104      
      * EXEC CICS XCTL
03105 *        PROGRAM  (THIS-PGM)
03106 *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
03107 *        LENGTH   (PI-COMM-LENGTH)
03108 *    END-EXEC.
      *    MOVE '.$C                   %   #00009521' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303039353231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 THIS-PGM, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03109
03110  9300-EXIT.
03111      EXIT.
03112
03113      EJECT
03114  9400-CLEAR SECTION.
03115      MOVE PI-RETURN-TO-PROGRAM  TO  THIS-PGM.
03116      PERFORM 9300-XCTL.
03117
03118  9400-EXIT.
03119      EXIT.
03120
03121  9600-PGMIDERR SECTION.
03122      
      * EXEC CICS HANDLE CONDITION
03123 *        PGMIDERR (8300-SEND-TEXT)
03124 *    END-EXEC.
      *    MOVE '"$L                   ! 4 #00009539' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3420233030303039353339' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03125
03126      MOVE THIS-PGM               TO  PI-CALLING-PROGRAM.
03127
03128      MOVE 'EL005'                TO  THIS-PGM
03129                                      LOGOFF-PGM.
03130      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.
03131      MOVE SPACES                 TO  PI-ENTRY-CD-1.
03132      PERFORM 9300-XCTL.
03133
03134  9600-EXIT.
03135      EXIT.
03136
03137  9900-ERROR-FORMAT SECTION.
03138      IF EMI-ERRORS-COMPLETE
03139          MOVE ER-3130               TO  EMI-ERROR
03140          GO TO 9900-EXIT.
03141
03142      
      * EXEC CICS LINK
03143 *        PROGRAM  ('EL001')
03144 *        COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)
03145 *        LENGTH   (EMI-COMM-LENGTH)
03146 *    END-EXEC.
           MOVE 'EL001' TO DFHEIV1
      *    MOVE '."C                   (   #00009559' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039353539' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03147
03148      MOVE ER-3130                   TO  EMI-ERROR.
03149
03150  9900-EXIT.
03151      EXIT.
03152
03153      EJECT
03154  9990-ERROR SECTION.
03155      MOVE DFHEIBLK TO EMI-LINE1.
03156      
      * EXEC CICS LINK
03157 *        PROGRAM  ('EL004')
03158 *        COMMAREA (EMI-LINE1)
03159 *        LENGTH   (72)
03160 *    END-EXEC.
           MOVE 'EL004' TO DFHEIV1
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00009573' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039353733' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03161
03162      PERFORM 8200-SEND-DATAONLY.
03163      GO TO 9100-RETURN-TRAN.
03164
03165  9990-EXIT.
03166      EXIT.
03167
03168  9995-SECURITY-VIOLATION.
03169 *                            COPY ELCSCTP.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCSCTP                             *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   DESCRIPTION = C.I.C.S. COMMON SECURITY-MESSAGE LINK          *
00007 ******************************************************************
00008
00008
00009      MOVE EIBDATE          TO SM-JUL-DATE.
00010      MOVE EIBTRMID         TO SM-TERMID.
00011      MOVE THIS-PGM         TO SM-PGM.
00012      MOVE EIBTIME          TO TIME-IN.
00013      MOVE TIME-OUT         TO SM-TIME.
00014      MOVE PI-PROCESSOR-ID  TO SM-PROCESSOR-ID.
00015
00016      
      * EXEC CICS LINK
00017 *         PROGRAM  ('EL003')
00018 *         COMMAREA (SECURITY-MESSAGE)
00019 *         LENGTH   (80)
00020 *    END-EXEC.
           MOVE 'EL003' TO DFHEIV1
           MOVE 80
             TO DFHEIV11
      *    MOVE '."C                   (   #00009603' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039363033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 SECURITY-MESSAGE, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00021
00022 ******************************************************************
00023
03170
03171  9995-EXIT.
03172      EXIT.
03173
03174  9999-LAST-PARAGRAPH SECTION.
03175      MOVE ZEROS TO RETURN-CODE.
03176      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL176' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL176' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9600-PGMIDERR,
                     8800-NOT-OPEN,
                     0180-MAIN-LOGIC,
                     0190-MAIN-LOGIC,
                     0900-TERMIDERR,
                     0910-ENQ-BUSY,
                     9990-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 0030-MICR-CHK-SW,
                     5000-MICR-CLOSED,
                     5000-MICR-CLOSED
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 0225-MAIN-LOGIC,
                     0230-MAIN-LOGIC
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 8800-NOT-OPEN,
                     0236-MAIN-LOGIC
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 8800-NOT-OPEN,
                     0237-MAIN-LOGIC
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 9400-CLEAR,
                     0040-MAIN-LOGIC,
                     0040-MAIN-LOGIC,
                     0040-MAIN-LOGIC
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 9999-DFHBACK,
                     0390-MAIN-LOGIC
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 0326-NOTFND-EMPROD
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 0340-MAIN-LOGIC,
                     0340-MAIN-LOGIC
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 0351-READ-COMP-MASTER
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 12
               GO TO 0354-UPDATE-ACTIVITY-TRLRS
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 13
               GO TO 0354-CHECK-FOR-AUTO-PAY
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 14
               GO TO 0355-GET-ADDRESS
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 15
               GO TO 0360-NO-TRLR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 16
               GO TO 0375-MAIN-LOGIC
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 17
               GO TO 1020-CARRIER-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 18
               GO TO 2080-GET-BENEFICIARY
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 19
               GO TO 8700-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 20
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL176' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
