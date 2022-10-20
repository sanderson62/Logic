00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL1832.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 02/12/96 09:33:34.
00007 *                            VMOD=2.019.
00008 *
00009 *AUTHOR.           LOGIC,INC.
00010 *                  DALLAS,TEXAS.
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
00024 *REMARKS. TRANSACTION EX63 - FORMS PRINTER.
00025 *        THIS PROGRAM IS USED TO PRINT INITIAL OR PROGRESS FORMS
00026 *        DEPENDING ON THE VALUE OF THE PI-ENTRY-CODES AND FORM
00027 *        TYPE PASSED.
00028
00029 *        PRINT INITIAL FORMS     CODE-1 = 1
00030 *                                CODE-2 = 1
00031
00032 *        PRINT FOLLOW-UP FORMS   CODE-1 = 1
00033 *                                CODE-2 = 2
00034
00035 *        RE-PRINT FORMS          CODE-1 = 0
00036 *                                CODE-2 = 3
00037
00038 *        PRINT ALIGNMENT FORMS   CODE-1 = 0
00039 *                                CODE-2 = 2
00040      EJECT
00041  ENVIRONMENT DIVISION.
00042  DATA DIVISION.
00043  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00044  77  FILLER  PIC X(32)  VALUE '********************************'.
00045  77  FILLER  PIC X(32)  VALUE '*   EL1832 WORKING STORAGE     *'.
00046  77  FILLER  PIC X(32)  VALUE '******** VMOD=2.019 ************'.
00047
00048  01  WS-CONSTANTS.
00049      12  THIS-PGM                    PIC X(8)    VALUE 'EL1832'.
00050      12  PGM-NAME                    PIC X(8).
00051      12  LINK-ELDATCV                PIC X(8)    VALUE 'ELDATCV'.
00052      12  ARCH-ID                     PIC X(8)    VALUE 'ELARCH'.
00053      12  ARCH-ID2                    PIC X(8)    VALUE 'ELARCH2'.
00054      12  ACTV-ID                     PIC X(8)    VALUE 'ELTRLR'.
00055      12  CLAM-ID                     PIC X(8)    VALUE 'ELMSTR'.
00056      12  CERT-ID                     PIC X(8)    VALUE 'ELCERT'.
00057      12  ACCT-ID                     PIC X(8)    VALUE 'ERACCT'.
00058      12  ARCH-KEY.
00059          16  ARCH-PARTIAL-KEY.
00060              20  ARCH-CO             PIC X.
00061              20  ARCH-NUMBER         PIC S9(8)   COMP.
00062              20  ARCH-REC-TYPE       PIC X.
00063          16  ARCH-SEQ                PIC S9(4)   VALUE +0  COMP.
00064
00065      12  ARCH-KEY2.
00066          16  ARCH-PARTIAL-KEY2.
00067              20  ARCH-CO2            PIC X.
00068              20  ARCH-REC-TYPE2      PIC X.
00069          16  ARCH-NUMBER2            PIC S9(8)   COMP.
00070          16  ARCH-SEQ2               PIC S9(4)   VALUE +0  COMP.
00071
00072      12  ACTV-KEY.
00073        14  ACTV-PARTIAL-KEY.
00074          16  ACTV-CO                 PIC X.
00075          16  ACTV-CARRIER            PIC X.
00076          16  ACTV-CLAIM              PIC X(7).
00077          16  ACTV-CERT-NO            PIC X(11).
00078        14  ACTV-SEQ                  PIC S9(4)   COMP.
00079
00080      12  ELCNTL-KEY.
00081          16  CNTL-COMP-ID            PIC X(3).
00082          16  CNTL-REC-TYPE           PIC X.
00083          16  CNTL-ACCESS             PIC X(4).
00084          16  CNTL-CARR-ACCESS REDEFINES CNTL-ACCESS.
00085              20  CNTL-CARR           PIC X.
00086              20  FILLER              PIC X(3).
00087          16  CNTL-SEQ-NO             PIC S9(4)    COMP.
00088
00089      12  CLAM-KEY.
00090          16  CLAM-CO                 PIC X.
00091          16  CLAM-CARRIER            PIC X.
00092          16  CLAM-CLAIM              PIC X(7).
00093          16  CLAM-CERT               PIC X(11).
00094
00095      12  CERT-KEY.
00096          16  CERT-CO                 PIC X.
00097          16  CERT-PRIME              PIC X(21).
00098          16  CERT-CERT               PIC X(11).
00099
00100      12  ACCT-KEY.
00101          16  ACCT-PARTIAL-KEY.
00102              20  ACCT-CO             PIC X.
00103              20  ACCT-CARRIER        PIC X.
00104              20  ACCT-GROUPING       PIC X(6).
00105              20  ACCT-STATE          PIC XX.
00106              20  ACCT-ACCOUNT        PIC X(10).
00107          16  ACCT-EXP-DATE           PIC XX.
00108          16  FILLER                  PIC X(04).
00109
00110      12  ACCT-SAVE-KEY               PIC X(20).
00111
00112      12  WS-INSURED-ACTV-KEY.
00113          16  WS-INS-COMP-CD-KEY      PIC X(01).
00114          16  WS-INS-CARR-KEY         PIC X(01).
00115          16  WS-INS-CLAIM-KEY        PIC X(07).
00116          16  WS-INS-CERT-KEY         PIC X(11).
00117          16  WS-INS-SEQ-KEY          PIC S9(4)    COMP.
00118
00119      12  WS-ACTV-KEY                 PIC X(20)    VALUE SPACES.
00120      12  WS-PRINT-SW                 PIC X        VALUE SPACES.
00121      12  SAVE-DATE                   PIC X(8)     VALUE SPACES.
00122      12  CURRENT-SAVE                PIC XX   VALUE LOW-VALUES.
00123      12  CURRENT-4                   PIC XX   VALUE LOW-VALUES.
00124      12  CURRENT-10                  PIC XX   VALUE LOW-VALUES.
00125      12  CURRENT-30                  PIC XX   VALUE LOW-VALUES.
00126      12  ERROR-LINE                  PIC X(80)    VALUE SPACES.
00127      12  HEADER-BROWSE-STARTED       PIC X        VALUE 'N'.
00128      12  HEADER-SW                   PIC X        VALUE SPACE.
00129          88  HEADER-REC-FOUND                     VALUE SPACE.
00130
00131      12  FORM-CONTROL-SW             PIC X        VALUE SPACE.
00132          88  FORM-REC-FOUND                       VALUE SPACE.
00133
00134      12  OPTION-CODES                PIC XX       VALUE SPACES.
00135          88  PRINT-NEW-FORMS                      VALUE '11'.
00136          88  PRINT-FOLLOW-UP                      VALUE '12'.
00137          88  PRINT-ALIGNMENT                      VALUE ' 2'.
00138          88  REPRINT-FORMS                        VALUE ' 3'.
00139
00140      12  SUB                         PIC 9        VALUE 0.
00141      12  SUB-1                       PIC 9        VALUE 0.
00142      12  PRT                         PIC 9        VALUE 1.
00143
00144      12  WS-LABEL-HOLD-AREA.
00145            18  FILLER                PIC X(20)    VALUE SPACES.
00146            18  WS-ZIP-LABEL          PIC X(10)    VALUE SPACES.
00147
00148      12  GETMAIN-SPACE               PIC X        VALUE SPACE.
00149      12  TRANS-DATA-LENGTH           PIC S9(4)    VALUE +133 COMP.
00150      12  MSG-MESSAGE.
00151          16  FILLER   PIC X(25) VALUE 'FORM WILL BE DELETED FOR '.
00152          16  MSG-CARRIER             PIC X.
00153          16  FILLER                  PIC X        VALUE SPACE.
00154          16  MSG-CLAIM               PIC X(7).
00155          16  FILLER                  PIC X        VALUE SPACE.
00156          16  MSG-CERT                PIC X(11).
00157          16  FILLER                  PIC X(10)    VALUE ALL '*'.
00158          16  MSG-ARCH-NO             PIC Z(10).
00159          16  FILLER                  PIC X(5)     VALUE SPACES.
00160          16  MSG-COUNT               PIC X(5)     VALUE SPACES.
00161          16  FILLER                  PIC X(5)     VALUE SPACES.
00162          16  MSG-KIND                PIC X(6).
00163          16  FILLER                  PIC X(5)     VALUE SPACES.
00164          16  MSG-TYPE                PIC X.
00165          16  FILLER                  PIC X(39)    VALUE SPACES.
00166
00167      12  WS-ZIP-WORK.
00168          16  WS-ZIP-PRIME            PIC X(05).
00169          16  WS-ZIP-DASH             PIC X.
00170          16  WS-ZIP-PLUS4            PIC X(04).
00171      12  WS-CANADIAN-ZIP-WORK  REDEFINES  WS-ZIP-WORK.
00172          16  WS-CAN-POSTAL-1         PIC XXX.
00173          16  FILLER                  PIC X.
00174          16  WS-CAN-POSTAL-2         PIC XXX.
00175          16  FILLER                  PIC XXX.
00176      12  WS-WORK-ACCOUNT.
00177          16  FILLER                  PIC X(04)    VALUE SPACES.
00178          16  WS-WRK-ACCT             PIC X(06)    VALUE SPACES.
00179      12  WS-WORK-CERT-NO.
00180          16  FILLER                  PIC X(03)    VALUE SPACES.
00181          16  WS-WRK-CERT             PIC X(08)    VALUE SPACES.
00182
00183      EJECT
00184  01  WS-PRINT-WORK-AREAS.
00185      12  WS-CLAIM-TABLE OCCURS 3 TIMES.
00186          16  WS-CARRIER              PIC X.
00187          16  WS-CLAIM                PIC X(7).
00188          16  WS-CERT                 PIC X(11).
00189
00190      12  CERT-READ-SW                PIC X.
00191          88  CERT-READ                            VALUE 'Y'.
00192      12  ACCT-BROWSE-SW              PIC X.
00193          88  ACCT-BROWSE-STARTED                  VALUE 'Y'.
00194          88  ACCT-FOUND                           VALUE 'F'.
00195      12  WS-NAME-WORK                PIC X(30).
00196      12  WS-ADDR1-WORK               PIC X(30).
00197      12  WS-ADDR2-WORK               PIC X(30).
00198      12  WS-CITY-STATE-WORK          PIC X(30).
00199      12  WS-SAVE-ACCT                PIC X(10).
00200      12  WS-SAVE-MEMBER              PIC X(12).
00201      12  WS-SAVE-SS                  PIC X(11).
00202      12  WS-SAVE-LOAN-NO             PIC X(8).
00203      12  WS-SAVE-CLAIM               PIC X(8).
00204      12  WS-SAVE-ACCT-NAME           PIC X(30).
00205      12  WS-SPEC-INST1               PIC X(28).
00206      12  WS-SPEC-INST2               PIC X(28).
00207      12  WS-SPEC-INST3               PIC X(28).
00208      12  WS-ADDR-TYPE                PIC X.
00209      12  WS-ADDR-SEQ                 PIC S9(4)    COMP.
00210      12  WS-PAY-FREQ                 PIC XX.
00211      12  WS-TOTAL-PAID               PIC 9(5)V99  VALUE ZEROS.
00212      12  WS-PAID-THRU-DT             PIC XX.
00213      12  WS-INCURRED-DT              PIC XX.
00214      12  WS-CERT-EFF-DT              PIC XX.
00215      12  WS-GREG-PAID-THRU           PIC X(8).
00216      12  WS-GREG-INCURRED            PIC X(8).
00217      12  WS-BEN-DAYS-HOLD            PIC 99       VALUE ZEROS.
00218      12  WS-BENEFIT-SAVE.
00219          16  WS-BEN-DAYS             PIC 99       VALUE ZEROS.
00220          16  WS-BEN-TYPE             PIC X        VALUE SPACES.
00221      12  WS-ACCESS.
00222          16  FILLER                  PIC XX       VALUE SPACES.
00223          16  WS-BEN-CD               PIC XX       VALUE SPACES.
00224      12  WS-INSURED-NAME.
00225          16  WS-INSURED-FNAME        PIC X(10).
00226          16  WS-INSURED-LNAME        PIC X(15).
00227      12  WS-CERT-NO                  PIC X(11).
00228      12  WS-EMPLOYER-SW              PIC X(01).
00229          88  EMPLOYER-FORM-REQUIRED VALUE 'Y'.
00230      12  WS-INSURED-MAIL-TO-NAME     PIC X(30).
00231      12  WS-INSURED-ADDR-LINE-1      PIC X(30).
00232      12  WS-INSURED-ADDR-LINE-2      PIC X(30).
00233      12  WS-INSURED-CITY-STATE       PIC X(30).
00234      12  WS-INSURED-ZIP.
00235          16  WS-INSURED-ZIP-PRIME    PIC X(05).
00236          16  WS-INSURED-ZIP-PLUS4    PIC X(04).
00237      12  WS-INSURED-POST-CODE  REDEFINES  WS-INSURED-ZIP.
00238          16  WS-INSURED-POSTAL-1     PIC XXX.
00239          16  WS-INSURED-POSTAL-2     PIC XXX.
00240          16  FILLER                  PIC XXX.
00241      12  WS-COMPANY-MAIL-TO-NAME     PIC X(30).
00242      12  WS-COMPANY-IN-CARE-OF       PIC X(30).
00243      12  WS-COMPANY-ADDR-LINE-1      PIC X(30).
00244      12  WS-COMPANY-ADDR-LINE-2      PIC X(30).
00245      12  WS-COMPANY-CITY-STATE       PIC X(30).
00246      12  WS-COMPANY-ZIP.
00247          16  WS-COMPANY-ZIP-PRIME    PIC X(05).
00248          16  WS-COMPANY-ZIP-PLUS4    PIC X(04).
00249      12  WS-COMPANY-POST-CODE  REDEFINES  WS-COMPANY-ZIP.
00250          16  WS-COMPANY-POSTAL-1     PIC XXX.
00251          16  WS-COMPANY-POSTAL-2     PIC XXX.
00252          16  FILLER                  PIC XXX.
00253      12  WS-LAST-PMT-AMT             PIC 9(7)V99.
00254
00255      EJECT
00256  01  PROG-PRINT-LINE-1.
00257      12  FILLER                      PIC X(59).
00258      12  PROG-PRINT-SW               PIC X(01).
00259      12  FILLER                      PIC X(01).
00260      12  PROG-PRINT-DATE             PIC X(08).
00261
00262  01  PROG-PRINT-LINE-2.
00263      12  FILLER                      PIC X(07).
00264      12  PROG-NAME-ADDR              PIC X(30).
00265      12  FILLER                      PIC X(12).
00266      12  PROG-SPEC-INST              PIC X(30).
00267
00268  01  PROG-PRINT-LINE-3.
00269      12  FILLER                      PIC X(04).
00270      12  PROG-ACCOUNT-NAME           PIC X(28).
00271      12  FILLER                      PIC X(08).
00272      12  PROG-CLAIM                  PIC X(07).
00273      12  FILLER                      PIC X(01).
00274      12  PROG-CARRIER                PIC X(01).
00275      12  FILLER                      PIC X(10).
00276      12  PROG-MEMBER-NO              PIC X(12).
00277      12  FILLER                      PIC X(06).
00278      12  PROG-ACCOUNT-NO             PIC X(06).
00279
00280      EJECT
00281  01  INIT-PRINT-LINE-1.
00282      12  FILLER                      PIC X(60).
00283      12  INIT-ELIM-DAYS              PIC Z9.
00284
00285  01  INIT-PRINT-LINE-2.
00286      12  FILLER                      PIC X(07).
00287      12  INIT-NAME-1                 PIC X(30).
00288      12  FILLER                      PIC X(19).
00289      12  INIT-ACCOUNT-NAME           PIC X(26).
00290
00291  01  INIT-PRINT-LINE-3 REDEFINES INIT-PRINT-LINE-2.
00292      12  FILLER                      PIC X(70).
00293      12  INIT-CLAIM-ACCT             PIC X(10).
00294      12  FILLER                      PIC X(01).
00295      12  INIT-CLAIM-CARRIER          PIC X(01).
00296
00297  01  INIT-PRINT-LINE-4-1.
00298      12  FILLER                      PIC X(70).
00299      12  INIT-PRINT-SW               PIC X(01).
00300      12  FILLER                      PIC X(01).
00301      12  INIT-PRINT-DATE             PIC X(08).
00302
00303      EJECT
00304  01  LBL-PRINT-LINE-1.
00305      12  FILLER                      PIC X(07).
00306      12  LBL-INSURED-NAME            PIC X(30).
00307      12  FILLER                      PIC X(13).
00308      12  LBL-CLAIM-NO                PIC X(07).
00309      12  FILLER                      PIC X(26).
00310
00311  01  LBL-PRINT-LINE-2.
00312      12  FILLER                      PIC X(07).
00313      12  LBL-INSURED-ADDR1           PIC X(30).
00314      12  FILLER                      PIC X(46).
00315
00316  01  LBL-PRINT-LINE-3.
00317      12  FILLER                      PIC X(07).
00318      12  LBL-INSURED-ADDR2           PIC X(30).
00319      12  FILLER                      PIC X(46).
00320
00321  01  LBL-PRINT-LINE-4.
00322      12  FILLER                      PIC X(07).
00323      12  LBL-INSURED-CITY-ST         PIC X(35).
00324      12  FILLER                      PIC X(41).
00325
00326  01  LBL-PRINT-LINE-5.
00327      12  FILLER                      PIC X(42).
00328      12  LBL-PAID-THRU-DATE          PIC X(08).
00329      12  FILLER                      PIC X(33).
00330      EJECT
00331
00332  01  RMC-LAP-PRINT-LINE-1.
00333      12  FILLER                      PIC X(17).
00334      12  RMC-LAP-CLAIM-NO            PIC X(07).
00335      12  FILLER                      PIC X(12).
00336      12  RMC-LAP-POLICY-NO           PIC X(11).
00337      12  FILLER                      PIC X(39).
00338
00339  01  RMC-LAP-PRINT-LINE-2.
00340      12  FILLER                      PIC X(10).
00341      12  RMC-LAP-INSURED-NAME        PIC X(30).
00342      12  FILLER                      PIC X(44).
00343
00344  01  RMC-LAP-PRINT-LINE-3.
00345      12  FILLER                      PIC X(10).
00346      12  RMC-LAP-INSURED-ADDR1       PIC X(30).
00347      12  FILLER                      PIC X(44).
00348
00349  01  RMC-LAP-PRINT-LINE-4.
00350      12  FILLER                      PIC X(10).
00351      12  RMC-LAP-INSURED-ADDR2       PIC X(35).
00352      12  FILLER                      PIC X(40).
00353
00354  01  RMC-LAP-PRINT-LINE-5.
00355      12  FILLER                      PIC X(10).
00356      12  RMC-LAP-INSURED-CITY-ST     PIC X(35).
00357      12  FILLER                      PIC X(40).
00358
00359  01  RMC-LAP-PRINT-LINE-6.
00360      12  FILLER                      PIC X(03).
00361      12  RMC-LAP-TEXT-LINE-1         PIC X(28).
00362      12  FILLER                      PIC X(52).
00363
00364  01  RMC-LAP-PRINT-LINE-7.
00365      12  FILLER                      PIC X(03).
00366      12  RMC-LAP-TEXT-LINE-2         PIC X(28).
00367      12  FILLER                      PIC X(52).
00368
00369  01  RMC-LAP-PRINT-LINE-8.
00370      12  FILLER                      PIC X(03).
00371      12  RMC-LAP-TEXT-LINE-3         PIC X(28).
00372      12  FILLER                      PIC X(52).
00373
00374 *                                    COPY ELCDMD34.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCDMD34.                           *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = DMD DLO034 PARAMETER AREA                 *
00007 *                                                                *
00008 *    LENGTH = 272    RECFRM = FIXED                              *
00009 *                                                                *
00010 ******************************************************************
00011  01  DLO034-COMMUNICATION-AREA.
00012      12  DL34-PROCESS-TYPE             PIC X.
00013      12  DL34-COMPANY-ID               PIC XXX.
00014      12  DL34-PRINT-PROGRAM-ID         PIC X(8).
00015      12  DL34-USERID                   PIC X(4).
00016      12  DL34-PRINT-LINE               PIC X(250).
00017      12  DL34-OVERRIDE-PRINTER-ID      PIC X(4).
00018      12  DL34-RETURN-CODE              PIC XX.
00019  01  DLO034-REC-LENGTH                 PIC S9(4) COMP VALUE +272.
00375      EJECT
00376 *                                    COPY ELCINTF.
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
00377      12  PI-WA REDEFINES PI-PROGRAM-WORK-AREA.
00378          16  PI-PRINT-DATE           PIC X(8).
00379          16  PI-PRINT-DATE-BIN       PIC XX.
00380          16  PI-PRINT-ID             PIC X(4).
00381          16  PI-FORM-TYPE            PIC X.
00382          16  FILLER                  PIC X(625).
00383      EJECT
00384 *                                    COPY ELCJPFX.
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
00385                                      PIC X(200).
00386      EJECT
00387 *                                    COPY ELPRTCVD.
00001 *****************************************************************
00002 *                                                               *
00003 *                            ELPRTCVD.                          *
00004 *                            VMOD=2.001                         *
00005 *****************************************************************.
00006
00007 ******************************************************************
00008 ***   WORK AREAS  FOR TERMINAL ONLINE PRINT ROUTINE
00009 ***                 -ELPRTCVD-
00010 ***   TO BE USED WITH PROCEDURE COPY MEMBER -ELPRTCVP-
00011 ******************************************************************
00012
00013  01  S-WORK-AREA                     SYNC.
00014      12  WS-LINE-LEN                 PIC S9(4)       VALUE +80
00015                                      COMP.
00016
00017      12  WS-LINE-LENGTH              PIC S9(4)       VALUE ZERO
00018                                      COMP.
00019
00020      12  WS-BUFFER-SIZE              PIC S9(4)       VALUE +1916
00021                                      COMP.
00022
00023      12  WS-BUFFER-LENGTH            PIC S9(4)       VALUE ZERO
00024                                      COMP.
00025
00026      12  WS-PROG-END                 PIC X           VALUE SPACES.
00027
00028      12  WS-PRINT-AREA.
00029          16  WS-PASSED-CNTL-CHAR     PIC X           VALUE SPACES.
00030            88  SINGLE-SPACE                          VALUE ' '.
00031            88  DOUBLE-SPACE                          VALUE '0'.
00032            88  TRIPLE-SPACE                          VALUE '-'.
00033            88  TOP-PAGE                              VALUE '1'.
00034
00035          16  WS-PASSED-DATA.
00036              20  WS-PRINT-BYTE       PIC X
00037                  OCCURS 132 TIMES    INDEXED BY PRT-INDEX.
00038
00039      12  WS-LINE-CNT                 PIC S9(3)        VALUE ZERO
00040                                      COMP-3.
00041      12  WS-WCC-CNTL                 PIC X(1)         VALUE 'H'.
00042
00043      12  WS-EM                       PIC S9(4)        VALUE +25
00044                                      COMP.
00045      12  FILLER   REDEFINES WS-EM.
00046          16  FILLER                  PIC X.
00047          16  T-EM                    PIC X.
00048
00049 *    12  WS-SS                       PIC S9(4)        VALUE +21
00049      12  WS-SS                       PIC S9(4)        VALUE +10
00050                                      COMP.
00051      12  FILLER   REDEFINES WS-SS.
00052          16  FILLER                  PIC X.
00053          16  T-SS                    PIC X.
00054
00055      12  WS-TP                       PIC S9(4)      VALUE +12
00056                                      COMP.
00057      12  FILLER   REDEFINES WS-TP.
00058          16  FILLER                  PIC X.
00059          16  T-TP                    PIC X.
00060
00061      12  WS-FIRST-TIME-SW            PIC X           VALUE '1'.
00062          88  FIRST-TIME                              VALUE '1'.
00063          88  FIRST-LINE-NEXT-BUFFER                  VALUE '2'.
00064
00065      12  WS-BUFFER-AREA.
00066          16  WS-BUFFER-BYTE          PIC X
00067              OCCURS 1920 TIMES       INDEXED BY BUFFER-INDEX
00068                                                 BUFFER-INDEX2.
00069
00070 ******************************************************************
00388      EJECT
00389 *                                    COPY ELCDATE.
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
00390      EJECT
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
00392 *                                    COPY ELCARCH.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCARCH.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.007                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = LETTERS SENT TO ARCHIVE FILE              *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 090  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELARCH                        RKP=2,LEN=8     *
00013 *       ALTERNATE PATH1 = ELARCH2 (RECORD TYPE)  RKP=10,LEN=8    *
00014 *                                                                *
00015 *   LOG = NO                                                     *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
CIDMOD*                                                                *
CIDMOD*  THERE ARE CID MODS IN COPYBOOK ELCARCH                        *
00017 ******************************************************************
       01  DFHCOMMAREA       PIC X(01).
00018  01  LETTER-ARCHIVE.
00019      12  LA-RECORD-ID                PIC XX.
00020          88  VALID-LA-ID                VALUE 'LA'.
00021
00022      12  LA-CONTROL-PRIMARY.
00023          16  LA-COMPANY-CD           PIC X.
00024          16  LA-ARCHIVE-NO           PIC S9(8)     COMP.
00025          16  LA-RECORD-TYPE          PIC X.
00026              88  LA-HEADER-DATA         VALUE '1'.
00027              88  LA-ADDRESS-DATA        VALUE '2'.
00028              88  LA-TEXT-DATA           VALUE '3'.
00029              88  LA-FORM-CONTROL-HDR    VALUE '4'.
00030          16  LA-LINE-SEQ-NO          PIC S9(4)     COMP.
00031
00032      12  LA-CONTROL-BY-TYPE.
00033          16  LA-COMPANY-CD-A1        PIC X.
00034          16  LA-RECORD-TYPE-A1       PIC X.
00035          16  LA-ARCHIVE-NO-A1        PIC S9(8)     COMP.
00036          16  LA-LINE-SEQ-NO-A1       PIC S9(4)     COMP.
00037
00038      12  LA-TEXT-RECORD.
00039          16  LA-SKIP-CONTROL         PIC XX.
00040              88  NO-LINES-SKIPPED       VALUE SPACES.
00041              88  SKIP-TO-NEXT-PAGE      VALUE '99'.
00042          16  LA-TEXT-LINE            PIC X(70).
00043
00044      12  LA-ADDRESS-RECORD  REDEFINES  LA-TEXT-RECORD.
00045          16  FILLER                  PIC XX.
00046          16  LA-ADDRESS-LINE         PIC X(30).
00047          16  FILLER                  PIC X(40).
00048
00049      12  LA-HEADER-RECORD  REDEFINES  LA-TEXT-RECORD.
00050          16  FILLER                  PIC XX.
00051          16  LA-CARRIER              PIC X.
00052          16  LA-CLAIM-NO             PIC X(7).
00053          16  LA-CERT-NO.
00054              20  LA-CERT-PRIME       PIC X(10).
00055              20  LA-CERT-SFX         PIC X.
00056          16  LA-NO-OF-COPIES         PIC S9.
00057          16  LA-RESEND-DATE          PIC XX.
00058          16  LA-PROCESSOR-CD         PIC X(4).
00059          16  LA-CREATION-DT          PIC XX.
00060          16  LA-INITIAL-PRINT-DATE   PIC XX.
00061          16  LA-RESEND-PRINT-DATE    PIC XX.
00062          16  LA-CORR-TRLR-SEQ        PIC S9(4)    COMP.
00063          16  LA-1ST-RESEND-PRINT-DT  PIC XX.
CIDMOD*
00064 * -----  16  LA-DMD-ADDITIONAL-FIELDS.
00065 *   I        20  LA-DMD-LETTER-FORM      PIC X(4).
00066 *   I        20  LA-DMD-PROD-CODE        PIC XX.
00067 *   I        20  LA-DMD-RES-ST           PIC XX.
00068 *   I        20  LA-DMD-CORR-TRLR-SEQ    PIC S9(4)    COMP.
00069 *   I        20  LA-DMD-LETTER-STATUS    PIC X.
00070 *  NEW           88  LA-DMD-LETTER-ONLINE   VALUE '1'.
00071 *  DMD           88  LA-DMD-LETTER-PURGED   VALUE '2'.
00072 *  CHGS          88  LA-DMD-LETTER-RELOADED VALUE '3'.
00073 *   I        20  LA-DMD-LETTER-PURGE-DT  PIC XX.
00074 *   I        20  LA-DMD-LETTER-RELOAD-DT PIC XX.
00075 *   I        20  LA-DMD-UND-CODE         PIC XX.
00076 *   I        20  LA-DMD-BEN-CODE         PIC XX.
00077 *   V    16  FILLER                  PIC X(15).
CIDMOD* -----
CIDMOD*
CIDMOD* REINSERTED  CSO  MODS
CIDMOD*
CIDMOD         16  FILLER.
CIDMOD             20  FILLER                  PIC X(29).
CIDMOD             20  LA-CSO-LETTER-STATUS    PIC X.
CIDMOD                 88  LA-CSO-LETTER-ONLINE   VALUE '1'.
CIDMOD                 88  LA-CSO-LETTER-PURGED   VALUE '2'.
CIDMOD                 88  LA-CSO-LETTER-RELOADED VALUE '3'.
CIDMOD             20  LA-CSO-LETTER-PURGE-DT  PIC XX.
CIDMOD             20  LA-CSO-LETTER-RELOAD-DT PIC XX.
CIDMOD*
00078
00079      12  LA-FORM-CONTROL-HEADER REDEFINES  LA-TEXT-RECORD.
00080          16  FILLER                  PIC XX.
00081          16  LA4-CARRIER             PIC X.
00082          16  LA4-CLAIM-NO            PIC X(7).
00083          16  LA4-CERT-NO.
00084              20  LA4-CERT-PRIME      PIC X(10).
00085              20  LA4-CERT-SFX        PIC X.
00086          16  LA4-NO-OF-COPIES        PIC S9.
00087          16  LA4-RESEND-DATE         PIC XX.
00088          16  LA4-PROCESSOR-CD        PIC X(4).
00089          16  LA4-CREATION-DT         PIC XX.
00090          16  LA4-INITIAL-PRINT-DATE  PIC XX.
00091          16  LA4-RESEND-PRINT-DATE   PIC XX.
00092          16  LA4-FORM-TRLR-SEQ       PIC S9(4)    COMP.
00093          16  LA4-FORM-TYPE           PIC X.
00094              88  LA4-INITIAL-FORM    VALUE '1'.
00095              88  LA4-PROGRESS-FORM   VALUE '2'.
00096          16  LA4-FORM-REM-PRINT-DT   PIC X(02).
00097          16  LA4-STATE               PIC X(02).
00098          16  FILLER                  PIC X(31).
00099 ******************************************************************
00393      EJECT
00394 *                                    COPY ELCTRLR.
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
00395      EJECT
00396 *                                    COPY ELCCERT.
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
00397      EJECT
00398 *                                    COPY ELCMSTR.
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
00399      EJECT
00400 *                                    COPY ERCACCT.
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
090803     12  FILLER                            PIC X(5).
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
00401      EJECT
00402 *                                    COPY ELCCNTL.
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
00403      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA LETTER-ARCHIVE
                                ACTIVITY-TRAILERS CERTIFICATE-MASTER
                                CLAIM-MASTER ACCOUNT-MASTER
                                CONTROL-FILE.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL1832' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00405
00406      MOVE EIBDATE                TO DC-JULIAN-YYDDD.
00407      MOVE '5'                    TO DC-OPTION-CODE.
00408      PERFORM 9700-DATE-LINK  THRU  9700-EXIT.
00409      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.
00410      MOVE DC-BIN-DATE-1          TO  CURRENT-SAVE.
00411      MOVE SPACES                 TO DL34-PROCESS-TYPE.
CIDMOD*    MOVE 'N'                    TO CSO-PRINT-STARTED-SW.
00412
00413      MOVE 85                     TO WS-LINE-LEN.
00414
00415  0100-RETRIEVE-LOOP.
00416      
      * EXEC CICS HANDLE CONDITION
00417 *         ENDDATA    (200-END-DATA)
00418 *         NOTFND     (300-NOT-FOUND)
00419 *         END-EXEC.
      *    MOVE '"$&I                  ! " #00004224' TO DFHEIV0
           MOVE X'222426492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303034323234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00420
00421      
      * EXEC CICS RETRIEVE
00422 *         INTO       (PROGRAM-INTERFACE-BLOCK)
00423 *         LENGTH     (PI-COMM-LENGTH)
00424 *         END-EXEC.
      *    MOVE '0*I L                 &   #00004229' TO DFHEIV0
           MOVE X'302A49204C20202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323239' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00425
00426 * DLO034 OPEN WHEN DMD OR CID
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'
00428          IF DL34-PROCESS-TYPE IS EQUAL TO SPACES
00429              MOVE 'O'                TO DL34-PROCESS-TYPE
00430              MOVE PI-COMPANY-ID      TO DL34-COMPANY-ID
00431              MOVE THIS-PGM           TO DL34-PRINT-PROGRAM-ID
00432              MOVE PI-PROCESSOR-ID    TO DL34-USERID
00433              MOVE SPACES             TO DL34-PRINT-LINE
00434              MOVE PI-ALT-DMD-PRT-ID  TO DL34-OVERRIDE-PRINTER-ID
00435              
      * EXEC CICS LINK
00436 *                PROGRAM    ('DLO034')
00437 *                COMMAREA   (DLO034-COMMUNICATION-AREA)
00438 *                LENGTH     (DLO034-REC-LENGTH)
00439 *            END-EXEC
           MOVE 'DLO034' TO DFHEIV1
      *    MOVE '."C                   ''   #00004243' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DLO034-COMMUNICATION-AREA, 
                 DLO034-REC-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00440              IF DL34-RETURN-CODE NOT = 'OK'
00441                  MOVE  '**DLO034 OPEN ERROR - ABORT**'
00442                                      TO ERROR-LINE
00443                  PERFORM 400-SEND-TEXT
00444                  
      * EXEC CICS RETURN
00445 *                END-EXEC.
      *    MOVE '.(                    &   #00004252' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00446
CIDMOD
CIDMOD*    IF CSO-PRINT-STARTED-SW = 'Y'
CIDMOD*      MOVE 'L'  TO  DRS-SW
CIDMOD*      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
CIDMOD*    MOVE 'F'  TO  DRS-SW.
CIDMOD*    PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
CIDMOD*    MOVE ' '  TO  DRS-SW.
CIDMOD
00447      PERFORM 1000-INITIALIZE.
00448
00449      IF PRINT-ALIGNMENT
00450         PERFORM 6300-ALIGN-ROUTINE THRU 6399-EXIT
00451      ELSE
00452         PERFORM 5000-BROWSE-ARCHIVE-HEADERS THRU 5099-EXIT.
00453
00454  200-END-DATA.
00455
00456 * DLO034 CLOSE
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'
00458          MOVE 'C'                TO DL34-PROCESS-TYPE
00459          MOVE PI-COMPANY-ID      TO DL34-COMPANY-ID
00460          MOVE THIS-PGM           TO DL34-PRINT-PROGRAM-ID
00461          MOVE PI-PROCESSOR-ID    TO DL34-USERID
00462          MOVE SPACES             TO DL34-PRINT-LINE
00463                                     DL34-OVERRIDE-PRINTER-ID
00464          
      * EXEC CICS LINK
00465 *            PROGRAM    ('DLO034')
00466 *            COMMAREA   (DLO034-COMMUNICATION-AREA)
00467 *            LENGTH     (DLO034-REC-LENGTH)
00468 *        END-EXEC
           MOVE 'DLO034' TO DFHEIV1
      *    MOVE '."C                   ''   #00004280' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DLO034-COMMUNICATION-AREA, 
                 DLO034-REC-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00469          IF DL34-RETURN-CODE NOT = 'OK'
00470              MOVE  '**DLO034 CLOSE ERROR - ABORT**'
00471                                  TO ERROR-LINE
00472              PERFORM 400-SEND-TEXT.
00473
CIDMOD*    MOVE 'L' TO DRS-SW.
CIDMOD*    PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
CIDMOD
00474      
      * EXEC CICS RETURN
00475 *         END-EXEC.
      *    MOVE '.(                    &   #00004293' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00476
00477  300-NOT-FOUND.
00478      MOVE 'NO COMMUNICATION AREA FOUND'  TO ERROR-LINE
00479      PERFORM 400-SEND-TEXT
00480      GO TO 200-END-DATA.
00481
00482  400-SEND-TEXT.
00483      
      * EXEC CICS SEND TEXT
00484 *         FROM    (ERROR-LINE)
00485 *         LENGTH  (70)
00486 *         END-EXEC.
           MOVE 70
             TO DFHEIV11
      *    MOVE '8&      T       H   F -   #00004302' TO DFHEIV0
           MOVE X'382620202020202054202020' TO DFHEIV0(1:12)
           MOVE X'202020204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333032' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERROR-LINE, 
                 DFHEIV11, 
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
           
00487      EJECT
00488  1000-INITIALIZE.
00489      MOVE CURRENT-SAVE           TO DC-BIN-DATE-1.
00490      MOVE -4                     TO DC-ELAPSED-DAYS.
00491      MOVE ZEROS                  TO DC-ELAPSED-MONTHS.
00492      MOVE '6'                    TO DC-OPTION-CODE.
00493      PERFORM 9700-DATE-LINK THRU 9700-EXIT.
00494      MOVE DC-BIN-DATE-2          TO CURRENT-4.
00495
00496      MOVE CURRENT-SAVE           TO DC-BIN-DATE-1.
00497      MOVE -10                    TO DC-ELAPSED-DAYS.
00498      MOVE ZEROS                  TO DC-ELAPSED-MONTHS.
00499      MOVE '6'                    TO DC-OPTION-CODE.
00500      PERFORM 9700-DATE-LINK THRU 9700-EXIT.
00501      MOVE DC-BIN-DATE-2          TO CURRENT-10.
00502
00503      MOVE CURRENT-SAVE           TO DC-BIN-DATE-1.
00504      MOVE -30                    TO DC-ELAPSED-DAYS.
00505      MOVE ZEROS                  TO DC-ELAPSED-MONTHS.
00506      MOVE '6'                    TO DC-OPTION-CODE.
00507      PERFORM 9700-DATE-LINK THRU 9700-EXIT.
00508      MOVE DC-BIN-DATE-2          TO CURRENT-30.
00509
00510      MOVE PI-COMPANY-CD          TO ARCH-CO2
00511                                     ACTV-CO
00512                                     ARCH-CO.
00513
00514      MOVE PI-ENTRY-CODES         TO OPTION-CODES.
00515
00516      
      * EXEC CICS GETMAIN
00517 *        SET     (ADDRESS OF LETTER-ARCHIVE)
00518 *        LENGTH  (90)
00519 *        INITIMG (GETMAIN-SPACE)
00520 *        END-EXEC.
           MOVE 90
             TO DFHEIV11
      *    MOVE ',"IL                  $   #00004335' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333335' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 GETMAIN-SPACE
           SET ADDRESS OF LETTER-ARCHIVE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00521
00522      EJECT
00523  5000-BROWSE-ARCHIVE-HEADERS.
00524      MOVE '4'                    TO ARCH-REC-TYPE2.
00525      MOVE ZEROS                  TO ARCH-NUMBER2.
00526      MOVE ZEROS                  TO ARCH-SEQ2.
00527
00528  5005-SET-HANDLE.
00529      
      * EXEC CICS HANDLE CONDITION
00530 *         NOTFND    (5099-EXIT)
00531 *         NOTOPEN   (8860-ARCH2-NOT-OPEN)
00532 *         END-EXEC.
      *    MOVE '"$IJ                  ! # #00004348' TO DFHEIV0
           MOVE X'2224494A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303034333438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00533
00534      
      * EXEC CICS STARTBR
00535 *         DATASET   (ARCH-ID2)
00536 *         RIDFLD    (ARCH-KEY2)
00537 *         END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00004353' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ARCH-ID2, 
                 ARCH-KEY2, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00538
00539      MOVE 'Y'                    TO HEADER-BROWSE-STARTED.
00540
00541  5010-READ-NEXT.
00542      
      * EXEC CICS HANDLE CONDITION
00543 *         NOTFND   (5050-END-BR)
00544 *         ENDFILE  (5050-END-BR)
00545 *         NOTOPEN  (8860-ARCH2-NOT-OPEN)
00546 *         END-EXEC.
      *    MOVE '"$I''J                 ! $ #00004361' TO DFHEIV0
           MOVE X'222449274A20202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303034333631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00547
00548      
      * EXEC CICS READNEXT
00549 *         DATASET   (ARCH-ID2)
00550 *         RIDFLD    (ARCH-KEY2)
00551 *         INTO      (LETTER-ARCHIVE)
00552 *         END-EXEC.
           MOVE LENGTH OF
            LETTER-ARCHIVE
             TO DFHEIV12
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )   #00004367' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ARCH-ID2, 
                 LETTER-ARCHIVE, 
                 DFHEIV12, 
                 ARCH-KEY2, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00553
00554      IF (PI-COMPANY-CD  NOT = ARCH-CO2)  OR
00555         (ARCH-REC-TYPE2 NOT = '4')
00556           GO TO 5050-END-BR.
00557
00558      IF (PI-FORM-TYPE = 'I' AND NOT LA4-INITIAL-FORM) OR
00559         (PI-FORM-TYPE = 'P' AND NOT LA4-PROGRESS-FORM)
00560         GO TO 5010-READ-NEXT.
00561
00562      PERFORM 5050-END-BR.
00563
00564      IF PRINT-NEW-FORMS
00565         MOVE 'I'                 TO WS-PRINT-SW
00566         PERFORM 6000-NEW-FORMS THRU 6099-EXIT
00567         GO TO 5020-CHECK-DATES.
00568
00569      IF PRINT-FOLLOW-UP
00570         MOVE 'F'                 TO WS-PRINT-SW
00571         PERFORM 6100-FOLLOW-UP-FORMS THRU 6199-EXIT
00572         GO TO 5020-CHECK-DATES.
00573
00574      IF REPRINT-FORMS
00575         MOVE 'R'                 TO WS-PRINT-SW
00576         PERFORM 6200-REPRINT-FORMS THRU 6299-EXIT.
00577
00578  5020-CHECK-DATES.
00579 *    IF LA4-INITIAL-PRINT-DATE = LOW-VALUES
00580 *       IF LA4-CREATION-DT LESS THAN CURRENT-30
00581 *          PERFORM 8200-DELETE-ARCHIVE THRU 8299-EXIT
00582 *          GO TO 5010-READ-NEXT
00583 *          ELSE
00584 *          GO TO 5010-READ-NEXT.
00585
00586      IF LA4-INITIAL-PRINT-DATE = LOW-VALUES
00587          ADD +1                  TO  ARCH-SEQ2
00588            GO TO 5005-SET-HANDLE.
00589
00590      IF LA4-RESEND-DATE NOT = LOW-VALUES
00591         IF LA4-RESEND-PRINT-DATE = LOW-VALUES
00592            PERFORM 5030-CHECK-OLD-DATES THRU 5030-EXIT
00593           ELSE
00594            IF LA4-RESEND-PRINT-DATE LESS THAN CURRENT-4
00595               MOVE '2'      TO MSG-KIND
00596               PERFORM 8200-DELETE-ARCHIVE THRU 8299-EXIT
00597              ELSE
00598               NEXT SENTENCE
00599        ELSE
00600        IF LA4-INITIAL-PRINT-DATE LESS THAN CURRENT-4 AND
00601           LA4-RESEND-PRINT-DATE LESS THAN CURRENT-4
00602               MOVE '3'      TO MSG-KIND
00603            PERFORM 8200-DELETE-ARCHIVE THRU 8299-EXIT.
00604
00605      ADD +1                      TO  ARCH-SEQ2.
00606
00607      GO TO 5005-SET-HANDLE.
00608      EJECT
00609  5030-CHECK-OLD-DATES.
00610      IF LA4-RESEND-DATE LESS THAN CURRENT-10
00611               MOVE '4'      TO MSG-KIND
00612         PERFORM 8200-DELETE-ARCHIVE THRU 8299-EXIT.
00613
00614  5030-EXIT.
00615       EXIT.
00616
00617  5050-END-BR.
00618      IF HEADER-BROWSE-STARTED = 'Y'
00619         MOVE 'N'                 TO HEADER-BROWSE-STARTED
00620         
      * EXEC CICS ENDBR
00621 *            DATASET  (ARCH-ID2)
00622 *            END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00004439' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343339' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ARCH-ID2, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00623  5099-EXIT.
00624       EXIT.
00625
00626      EJECT
00627  6000-NEW-FORMS.
00628      IF LA4-INITIAL-PRINT-DATE NOT = LOW-VALUES
00629         GO TO 6099-EXIT.
00630
00631      IF PI-PRINT-DATE-BIN GREATER THAN LOW-VALUES
00632          IF LA4-CREATION-DT NOT = PI-PRINT-DATE-BIN
00633              GO TO 6099-EXIT.
00634
00635      PERFORM 8100-READ-HEADER THRU 8199-EXIT.
00636
00637      MOVE SPACE                  TO FORM-CONTROL-SW.
00638
00639      PERFORM 8000-READ-FORM-CONTROL THRU 8099-EXIT.
00640
00641      IF (FORM-REC-FOUND AND
00642         AT-FORM-SEND-ON-DT GREATER THAN CURRENT-SAVE) OR
00643         NOT FORM-REC-FOUND
00644         
      * EXEC CICS UNLOCK
00645 *            DATASET  (ARCH-ID)
00646 *            END-EXEC
      *    MOVE '&*                    #   #00004463' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343633' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ARCH-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00647         IF FORM-REC-FOUND
00648            PERFORM 6900-UNLOCK-FORM THRU 6999-EXIT
00649            GO TO 6099-EXIT
00650         ELSE
00651            GO TO 6099-EXIT.
00652
00653      MOVE CURRENT-SAVE           TO LA4-INITIAL-PRINT-DATE.
00654
00655      
      * EXEC CICS REWRITE
00656 *         DATASET  (ARCH-ID)
00657 *         FROM     (LETTER-ARCHIVE)
00658 *         END-EXEC.
           MOVE LENGTH OF
            LETTER-ARCHIVE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00004474' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343734' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ARCH-ID, 
                 LETTER-ARCHIVE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00659
00660      MOVE CURRENT-SAVE           TO AT-FORM-PRINTED-DT.
00661
00662      
      * EXEC CICS REWRITE
00663 *         DATASET  (ACTV-ID)
00664 *         FROM     (ACTIVITY-TRAILERS)
00665 *         END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00004481' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTV-ID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00666
00667      PERFORM 7200-PRINT-FORMS-RECORDS THRU 7299-EXIT.
00668
00669  6099-EXIT.
00670       EXIT.
00671
00672      EJECT
00673  6100-FOLLOW-UP-FORMS.
00674      IF LA4-RESEND-DATE = LOW-VALUES
00675         GO TO 6199-EXIT.
00676
00677      IF LA4-RESEND-DATE NOT GREATER THAN CURRENT-SAVE AND
00678         LA4-RESEND-PRINT-DATE = LOW-VALUES
00679         NEXT SENTENCE
00680      ELSE
00681         GO TO 6199-EXIT.
00682
00683      IF PI-PRINT-DATE-BIN GREATER THAN LOW-VALUES
00684          IF LA4-RESEND-DATE NOT = PI-PRINT-DATE-BIN
00685              GO TO 6199-EXIT.
00686
00687      MOVE SPACES                 TO FORM-CONTROL-SW
00688                                     HEADER-SW.
00689
00690      PERFORM 8100-READ-HEADER THRU 8199-EXIT.
00691
00692      IF NOT HEADER-REC-FOUND
00693         GO TO 6199-EXIT.
00694
00695      PERFORM 8000-READ-FORM-CONTROL THRU 8099-EXIT.
00696
00697      IF NOT FORM-REC-FOUND
00698         
      * EXEC CICS UNLOCK
00699 *            DATASET (ARCH-ID)
00700 *            END-EXEC
      *    MOVE '&*                    #   #00004517' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034353137' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ARCH-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00701         GO TO 6199-EXIT.
00702
00703      IF AT-FORM-ANSWERED-DT = LOW-VALUES AND
00704         AT-FORM-RE-SEND-DT  = LA4-RESEND-DATE
00705         PERFORM 6500-UPDATE-RESEND-PRINT THRU 6599-EXIT
00706         PERFORM 6800-REWRITE-HEADER      THRU 6899-EXIT
00707         PERFORM 6600-UPDATE-FORM-TRLR    THRU 6699-EXIT
00708         PERFORM 7200-PRINT-FORMS-RECORDS THRU 7299-EXIT
00709         GO TO 6199-EXIT.
00710
00711      IF AT-FORM-ANSWERED-DT NOT = LOW-VALUES
00712         PERFORM 6700-ZERO-RESEND-IN-HEADER THRU 6799-EXIT
00713         PERFORM 6800-REWRITE-HEADER        THRU 6899-EXIT
00714         PERFORM 6900-UNLOCK-FORM           THRU 6999-EXIT
00715         GO TO 6199-EXIT.
00716
00717      MOVE AT-FORM-RE-SEND-DT     TO LA4-RESEND-DATE.
00718
00719      IF LA4-RESEND-DATE NOT GREATER THAN CURRENT-SAVE
00720         NEXT SENTENCE
00721        ELSE
00722         PERFORM 6800-REWRITE-HEADER THRU 6899-EXIT
00723         PERFORM 6900-UNLOCK-FORM    THRU 6999-EXIT
00724         GO TO 6199-EXIT.
00725
00726      PERFORM 6500-UPDATE-RESEND-PRINT THRU 6599-EXIT.
00727      PERFORM 6800-REWRITE-HEADER      THRU 6899-EXIT.
00728      PERFORM 6600-UPDATE-FORM-TRLR    THRU 6699-EXIT.
00729      PERFORM 7200-PRINT-FORMS-RECORDS THRU 7299-EXIT.
00730
00731  6199-EXIT.
00732       EXIT.
00733      EJECT
00734  6200-REPRINT-FORMS.
00735      IF LA4-INITIAL-PRINT-DATE   = PI-PRINT-DATE-BIN OR
00736         LA4-RESEND-PRINT-DATE    = PI-PRINT-DATE-BIN
00737           PERFORM 7200-PRINT-FORMS-RECORDS THRU 7299-EXIT.
00738
00739  6299-EXIT.
00740       EXIT.
00741      EJECT
00742  6300-ALIGN-ROUTINE.
00743
00744      IF PI-COMPANY-ID   EQUAL   'RMC' OR 'LAP'
00745         MOVE  '1'                TO WS-PASSED-CNTL-CHAR
00746         MOVE  SPACES             TO WS-PASSED-DATA
00747         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
00748
00749         MOVE SPACES              TO WS-PASSED-CNTL-CHAR
00750         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT 8 TIMES
00751
00752         MOVE  SPACES               TO RMC-LAP-PRINT-LINE-1
00753         MOVE  ZEROS                TO RMC-LAP-CLAIM-NO
00754         MOVE  ZEROS                TO RMC-LAP-POLICY-NO
00755         MOVE  RMC-LAP-PRINT-LINE-1 TO WS-PASSED-DATA
00756         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
00757
00758         MOVE SPACES              TO WS-PASSED-DATA
00759                                     WS-PASSED-CNTL-CHAR
00760         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT 3 TIMES
00761
00762         MOVE  SPACES               TO RMC-LAP-PRINT-LINE-2
00763         MOVE  ALL '*'              TO RMC-LAP-INSURED-NAME
00764         MOVE  RMC-LAP-PRINT-LINE-2 TO WS-PASSED-DATA
00765         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
00766
00767         MOVE SPACES              TO WS-PASSED-DATA
00768                                     WS-PASSED-CNTL-CHAR
00769         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
00770
00771         MOVE  SPACES               TO RMC-LAP-PRINT-LINE-3
00772         MOVE  ALL '*'              TO RMC-LAP-INSURED-ADDR1
00773         MOVE  RMC-LAP-PRINT-LINE-3 TO WS-PASSED-DATA
00774         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
00775
00776         MOVE  SPACES             TO WS-PASSED-CNTL-CHAR
00777                                     WS-PASSED-DATA
00778         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
00779
00780         MOVE  SPACES               TO RMC-LAP-PRINT-LINE-4
00781         MOVE  ALL '*'              TO RMC-LAP-INSURED-ADDR2
00782         MOVE  RMC-LAP-PRINT-LINE-4 TO WS-PASSED-DATA
00783         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
00784
00785         MOVE  SPACES             TO WS-PASSED-CNTL-CHAR
00786                                     WS-PASSED-DATA
00787         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
00788
00789         MOVE  SPACES               TO RMC-LAP-PRINT-LINE-5
00790         MOVE  ALL '*'              TO RMC-LAP-INSURED-CITY-ST
00791         MOVE  RMC-LAP-PRINT-LINE-5 TO WS-PASSED-DATA
00792         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
00793
00794         MOVE SPACES              TO WS-PASSED-DATA
00795                                     WS-PASSED-CNTL-CHAR
00796         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT 37 TIMES
00797
00798         MOVE  SPACES               TO RMC-LAP-PRINT-LINE-6
00799         MOVE  ALL '*'              TO RMC-LAP-TEXT-LINE-1
00800         MOVE  RMC-LAP-PRINT-LINE-6 TO WS-PASSED-DATA
00801         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
00802
00803         MOVE  SPACES             TO WS-PASSED-CNTL-CHAR
00804                                     WS-PASSED-DATA
00805         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
00806
00807         MOVE  SPACES               TO RMC-LAP-PRINT-LINE-7
00808         MOVE  ALL '*'              TO RMC-LAP-TEXT-LINE-2
00809         MOVE  RMC-LAP-PRINT-LINE-7 TO WS-PASSED-DATA
00810         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
00811
00812         MOVE  SPACES             TO WS-PASSED-CNTL-CHAR
00813                                     WS-PASSED-DATA
00814         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
00815
00816         MOVE  SPACES               TO RMC-LAP-PRINT-LINE-8
00817         MOVE  ALL '*'              TO RMC-LAP-TEXT-LINE-2
00818         MOVE  RMC-LAP-PRINT-LINE-8 TO WS-PASSED-DATA
00819         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
00820
00821         MOVE  SPACES             TO WS-PASSED-CNTL-CHAR
00822                                     WS-PASSED-DATA
00823         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
00824
00825         MOVE  'X'                TO WS-PROG-END
00826         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
00827         GO TO  6399-EXIT.
00828
00829      IF PI-COMPANY-ID   EQUAL   'LBL'
00830         MOVE  '1'                TO WS-PASSED-CNTL-CHAR
00831         MOVE  SPACES             TO WS-PASSED-DATA
00832         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
00833
00834         MOVE SPACES              TO WS-PASSED-CNTL-CHAR
00835         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT 3 TIMES
00836
00837         MOVE  SPACES             TO LBL-PRINT-LINE-1
00838         MOVE  ALL '*'            TO LBL-INSURED-NAME
00839         MOVE  ZEROS              TO LBL-CLAIM-NO
00840         MOVE  LBL-PRINT-LINE-1   TO WS-PASSED-DATA
00841         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
00842
00843         MOVE SPACES              TO WS-PASSED-DATA
00844                                     WS-PASSED-CNTL-CHAR
00845         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
00846
00847         MOVE  SPACES             TO LBL-PRINT-LINE-2
00848         MOVE  ALL '*'            TO LBL-INSURED-ADDR1
00849         MOVE  LBL-PRINT-LINE-2   TO WS-PASSED-DATA
00850         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
00851
00852         MOVE  SPACES             TO WS-PASSED-CNTL-CHAR
00853                                     WS-PASSED-DATA
00854         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
00855
00856         MOVE  SPACES             TO LBL-PRINT-LINE-3
00857         MOVE  ALL '*'            TO LBL-INSURED-ADDR2
00858         MOVE  LBL-PRINT-LINE-3   TO WS-PASSED-DATA
00859         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
00860
00861         MOVE  SPACES             TO WS-PASSED-CNTL-CHAR
00862                                     WS-PASSED-DATA
00863         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
00864
00865         MOVE  SPACES             TO LBL-PRINT-LINE-4
00866         MOVE  ALL '*'            TO LBL-INSURED-CITY-ST
00867         MOVE  LBL-PRINT-LINE-4   TO WS-PASSED-DATA
00868         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
00869
00870         MOVE  SPACES             TO WS-PASSED-DATA
00871         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
00872
00873         MOVE  SPACES             TO LBL-PRINT-LINE-5
00874         MOVE  ALL '*'            TO LBL-PAID-THRU-DATE
00875         MOVE  LBL-PRINT-LINE-5   TO WS-PASSED-DATA
00876         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
00877
00878         MOVE  'X'                TO WS-PROG-END
00879         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
00880         GO TO  6399-EXIT.
00881
00882      IF PI-FORM-TYPE = 'I'
00883         MOVE '1'                 TO WS-PASSED-CNTL-CHAR
00884         MOVE SPACES              TO WS-PASSED-DATA
00885         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
00886
00887         MOVE SPACES              TO WS-PASSED-CNTL-CHAR
00888         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT 4 TIMES
00889
00890         MOVE SPACES              TO INIT-PRINT-LINE-1
00891         MOVE ZEROS               TO INIT-ELIM-DAYS
00892         MOVE INIT-PRINT-LINE-1   TO WS-PASSED-DATA
00893         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
00894
00895         MOVE SPACES              TO WS-PASSED-DATA
00896                                     WS-PASSED-CNTL-CHAR
00897         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT 13 TIMES
00898
00899         MOVE SPACES              TO INIT-PRINT-LINE-2
00900         MOVE ALL '*'             TO INIT-ACCOUNT-NAME
00901         MOVE INIT-PRINT-LINE-2   TO WS-PASSED-DATA
00902         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
00903
00904         MOVE SPACES              TO INIT-PRINT-LINE-2
00905         MOVE ALL '*'             TO INIT-ACCOUNT-NAME
00906                                     INIT-NAME-1
00907         MOVE INIT-PRINT-LINE-2   TO WS-PASSED-DATA
00908         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT 2 TIMES
00909
00910         MOVE SPACES              TO INIT-PRINT-LINE-2
00911         MOVE ALL '*'             TO INIT-NAME-1
00912         MOVE INIT-PRINT-LINE-2   TO WS-PASSED-DATA
00913         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
00914
00915         MOVE SPACES              TO INIT-PRINT-LINE-2
00916                                     INIT-PRINT-LINE-3
00917         MOVE ALL '*'             TO INIT-NAME-1
00918                                     INIT-CLAIM-ACCT
00919                                     INIT-CLAIM-CARRIER
00920         MOVE INIT-PRINT-LINE-3   TO WS-PASSED-DATA
00921         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
00922
00923         MOVE SPACES              TO INIT-PRINT-LINE-3
00924         MOVE ALL '*'             TO INIT-CLAIM-ACCT
00925         MOVE INIT-PRINT-LINE-3   TO WS-PASSED-DATA
00926         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
00927
00928         MOVE SPACES              TO INIT-PRINT-LINE-4-1
00929         MOVE ALL '*'             TO INIT-PRINT-SW
00930                                     INIT-PRINT-DATE
00931         MOVE INIT-PRINT-LINE-4-1 TO WS-PASSED-DATA
00932         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
00933
00934         MOVE 'X'                 TO WS-PROG-END
00935         MOVE SPACES              TO WS-PASSED-DATA
CIDMOD*       PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
00937       ELSE
00938         MOVE '1'                 TO WS-PASSED-CNTL-CHAR
00939         MOVE SPACES              TO WS-PASSED-DATA
00940         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
00941
00942         MOVE SPACES              TO WS-PASSED-CNTL-CHAR
00943         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT 8 TIMES
00944
00945         MOVE SPACES              TO PROG-PRINT-LINE-1
00946         MOVE ALL '*'             TO PROG-PRINT-SW
00947                                     PROG-PRINT-DATE
00948         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
00949
00950         MOVE SPACES              TO WS-PASSED-DATA
00951                                     WS-PASSED-CNTL-CHAR
00952         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT 2 TIMES
00953
00954         MOVE SPACES              TO PROG-PRINT-LINE-2
00955         MOVE ALL '*'             TO PROG-NAME-ADDR
00956         MOVE PROG-PRINT-LINE-2   TO WS-PASSED-DATA
00957         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
00958
00959         MOVE SPACES              TO PROG-PRINT-LINE-2
00960         MOVE ALL '*'             TO PROG-NAME-ADDR
00961                                     PROG-SPEC-INST
00962         MOVE PROG-PRINT-LINE-2   TO WS-PASSED-DATA
00963         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT 3 TIMES
00964
00965         MOVE SPACES              TO WS-PASSED-DATA
00966         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT 5 TIMES
00967
00968         MOVE SPACES              TO PROG-PRINT-LINE-3
00969         MOVE ALL '*'             TO PROG-ACCOUNT-NAME
00970                                     PROG-CLAIM
00971                                     PROG-CARRIER
00972                                     PROG-MEMBER-NO
00973                                     PROG-ACCOUNT-NO
00974         MOVE PROG-PRINT-LINE-3   TO WS-PASSED-DATA
00975         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
00976
CIDMOD        MOVE 'X'                 TO WS-PROG-END.
CIDMOD*       PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
00979
00980  6399-EXIT.
00981       EXIT.
00982
00983      EJECT
00984  6500-UPDATE-RESEND-PRINT.
00985      MOVE CURRENT-SAVE           TO LA4-RESEND-PRINT-DATE.
00986      MOVE LOW-VALUES             TO LA4-RESEND-DATE.
00987
00988  6599-EXIT.
00989       EXIT.
00990      EJECT
00991  6600-UPDATE-FORM-TRLR.
00992      MOVE CURRENT-SAVE           TO AT-FORM-REPRINT-DT.
00993
00994      
      * EXEC CICS REWRITE
00995 *         DATASET  (ACTV-ID)
00996 *         FROM     (ACTIVITY-TRAILERS)
00997 *         END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00004813' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTV-ID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00998
00999  6699-EXIT.
01000       EXIT.
01001      EJECT
01002  6700-ZERO-RESEND-IN-HEADER.
01003
01004      MOVE LOW-VALUES             TO LA4-RESEND-DATE.
01005
01006  6799-EXIT.
01007       EXIT.
01008      EJECT
01009  6800-REWRITE-HEADER.
01010      
      * EXEC CICS HANDLE CONDITION
01011 *        DUPKEY  (6899-EXIT)
01012 *        END-EXEC.
      *    MOVE '"$$                   ! % #00004829' TO DFHEIV0
           MOVE X'222424202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303034383239' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01013
01014      
      * EXEC CICS REWRITE
01015 *         DATASET  (ARCH-ID)
01016 *         FROM     (LETTER-ARCHIVE)
01017 *         END-EXEC.
           MOVE LENGTH OF
            LETTER-ARCHIVE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00004833' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383333' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ARCH-ID, 
                 LETTER-ARCHIVE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01018
01019  6899-EXIT.
01020       EXIT.
01021      EJECT
01022  6900-UNLOCK-FORM.
01023      
      * EXEC CICS UNLOCK
01024 *         DATASET  (ACTV-ID)
01025 *         END-EXEC.
      *    MOVE '&*                    #   #00004842' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383432' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTV-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01026
01027  6999-EXIT.
01028       EXIT.
01029      EJECT
01030  7200-PRINT-FORMS-RECORDS.
01031      MOVE SPACES                 TO PROG-PRINT-LINE-1
01032                                     PROG-PRINT-LINE-2
01033                                     PROG-PRINT-LINE-3
01034                                     INIT-PRINT-LINE-1
01035                                     INIT-PRINT-LINE-2
01036                                     INIT-PRINT-LINE-3
01037                                     INIT-PRINT-LINE-4-1
01038                                     LBL-PRINT-LINE-1
01039                                     LBL-PRINT-LINE-2
01040                                     LBL-PRINT-LINE-3
01041                                     LBL-PRINT-LINE-4
01042                                     LBL-PRINT-LINE-5
01043                                     RMC-LAP-PRINT-LINE-1
01044                                     RMC-LAP-PRINT-LINE-2
01045                                     RMC-LAP-PRINT-LINE-3
01046                                     RMC-LAP-PRINT-LINE-4
01047                                     RMC-LAP-PRINT-LINE-5
01048                                     RMC-LAP-PRINT-LINE-6
01049                                     RMC-LAP-PRINT-LINE-7
01050                                     RMC-LAP-PRINT-LINE-8
01051                                     WS-PRINT-WORK-AREAS.
01052
01053      MOVE ZEROS                  TO WS-ADDR-SEQ
01054                                     WS-TOTAL-PAID
01055                                     WS-BEN-DAYS-HOLD.
01056
01057      MOVE ARCH-NUMBER2           TO ARCH-NUMBER.
01058      MOVE '4'                    TO ARCH-REC-TYPE.
01059      MOVE ZEROS                  TO ARCH-SEQ.
01060
01061      
      * EXEC CICS READ
01062 *         DATASET   (ARCH-ID)
01063 *         RIDFLD    (ARCH-KEY)
01064 *         INTO      (LETTER-ARCHIVE)
01065 *         END-EXEC.
           MOVE LENGTH OF
            LETTER-ARCHIVE
             TO DFHEIV11
      *    MOVE '&"IL       E          (   #00004880' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ARCH-ID, 
                 LETTER-ARCHIVE, 
                 DFHEIV11, 
                 ARCH-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01066
01067      MOVE LA4-CARRIER             TO ACTV-CARRIER.
01068      MOVE LA4-CLAIM-NO            TO ACTV-CLAIM.
01069      MOVE LA4-CERT-NO             TO ACTV-CERT-NO.
01070      MOVE LA4-FORM-TRLR-SEQ       TO ACTV-SEQ.
01071
01072      
      * EXEC CICS HANDLE CONDITION
01073 *         NOTOPEN  (8870-ACTV-NOT-OPEN)
01074 *         NOTFND   (7299-EXIT)
01075 *         END-EXEC.
      *    MOVE '"$JI                  ! & #00004891' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303034383931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01076
01077      
      * EXEC CICS READ
01078 *         DATASET  (ACTV-ID)
01079 *         RIDFLD   (ACTV-KEY)
01080 *         SET      (ADDRESS OF ACTIVITY-TRAILERS)
01081 *         END-EXEC.
      *    MOVE '&"S        E          (   #00004896' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383936' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTV-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01082
01083      MOVE AT-CARRIER             TO WS-CARRIER (1).
01084      MOVE AT-CLAIM-NO            TO WS-CLAIM   (1).
01085      MOVE AT-CERT-NO             TO WS-CERT    (1).
01086
01087      IF NOT FORM-CONTROL-TR
01088         MOVE 'THIS FORM RECORD IS INVALID'     TO WS-SPEC-INST1
01089         MOVE 'PLEASE CONTACT DATA PROCESSING'  TO WS-SPEC-INST2
01090         MOVE 'TO INFORM THEM OF THIS ERROR'    TO WS-SPEC-INST3
01091         GO TO 7205-CLAIM-LOOP.
01092
01093      MOVE SPACES                 TO WS-CLAIM-TABLE (2)
01094                                     WS-CLAIM-TABLE (3).
01095      MOVE AT-INSTRUCT-LN-1       TO WS-SPEC-INST1.
01096      MOVE AT-INSTRUCT-LN-2       TO WS-SPEC-INST2.
01097      MOVE AT-INSTRUCT-LN-3       TO WS-SPEC-INST3.
01098      MOVE AT-FORM-ADDR-SEQ-NO    TO WS-ADDR-SEQ.
01099      MOVE AT-FORM-ADDRESS        TO WS-ADDR-TYPE.
01100      MOVE 0                      TO SUB.
01101      MOVE 1                      TO PRT.
01102
01103  7205-CLAIM-LOOP.
01104      ADD 1 TO SUB.
01105
01106      IF SUB = 4
01107         GO TO 7250-PRINT-FORM.
01108
01109      IF WS-CLAIM-TABLE (SUB)  =  SPACES  OR  LOW-VALUES
01110         GO TO 7205-CLAIM-LOOP.
01111
01112      MOVE WS-CARRIER (SUB)       TO CLAM-CARRIER.
01113      MOVE WS-CLAIM (SUB)         TO CLAM-CLAIM.
01114      MOVE WS-CERT (SUB)          TO CLAM-CERT.
01115      MOVE PI-COMPANY-CD          TO CLAM-CO.
01116
01117      
      * EXEC CICS HANDLE CONDITION
01118 *         NOTOPEN  (8880-CLAM-NOT-OPEN)
01119 *         NOTFND   (7205-CLAIM-LOOP)
01120 *         END-EXEC.
      *    MOVE '"$JI                  ! '' #00004936' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303034393336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01121
01122      
      * EXEC CICS READ
01123 *         DATASET  (CLAM-ID)
01124 *         RIDFLD   (CLAM-KEY)
01125 *         SET      (ADDRESS OF CLAIM-MASTER)
01126 *         UPDATE
01127 *         END-EXEC.
      *    MOVE '&"S        EU         (   #00004941' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034393431' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLAM-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CLAM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01128
01129      IF CL-INCURRED-DT GREATER THAN WS-INCURRED-DT
01130         MOVE CL-INCURRED-DT         TO DC-BIN-DATE-1
01131                                        WS-INCURRED-DT
01132         MOVE SPACE                  TO DC-OPTION-CODE
01133         PERFORM 9700-DATE-LINK THRU 9700-EXIT
01134         MOVE DC-GREG-DATE-1-EDIT    TO WS-GREG-INCURRED.
01135
01136      MOVE CURRENT-SAVE              TO CL-LAST-MAINT-DT.
01137      MOVE PI-PROCESSOR-ID           TO CL-LAST-MAINT-USER.
01138      MOVE EIBTIME                   TO CL-LAST-MAINT-HHMMSS.
01139      MOVE '2'                       TO CL-LAST-MAINT-TYPE.
01140
01141      MOVE CL-CLAIM-NO               TO WS-SAVE-CLAIM.
01142      MOVE CL-COMPANY-CD             TO ACCT-CO
01143                                        CERT-CO.
01144      MOVE CL-CARRIER                TO ACCT-CARRIER.
01145      MOVE CL-CERT-GROUPING          TO ACCT-GROUPING.
01146      MOVE CL-CERT-STATE             TO ACCT-STATE.
01147      MOVE CL-CERT-ACCOUNT           TO ACCT-ACCOUNT.
01148      MOVE CL-CERT-EFF-DT            TO WS-CERT-EFF-DT.
01149      MOVE CL-CERT-KEY-DATA          TO CERT-PRIME.
01150
01151      
      * EXEC CICS HANDLE CONDITION
01152 *        DUPKEY (7206-CLAIM-REWRITE)
01153 *        END-EXEC.
      *    MOVE '"$$                   ! ( #00004970' TO DFHEIV0
           MOVE X'222424202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303034393730' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01154
01155      
      * EXEC CICS REWRITE
01156 *         DATASET  (CLAM-ID)
01157 *         FROM     (CLAIM-MASTER)
01158 *         END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00004974' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303034393734' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLAM-ID, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01159
01160  7206-CLAIM-REWRITE.
01161      
      * EXEC CICS HANDLE CONDITION
01162 *         NOTOPEN  (9000-CERT-NOT-OPEN)
01163 *         NOTFND   (7205-CLAIM-LOOP)
01164 *         END-EXEC.
      *    MOVE '"$JI                  ! ) #00004980' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303034393830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01165
01166      MOVE WS-CERT (SUB)          TO CERT-CERT.
01167
01168      
      * EXEC CICS READ
01169 *         DATASET  (CERT-ID)
01170 *         RIDFLD   (CERT-KEY)
01171 *         SET      (ADDRESS OF CERTIFICATE-MASTER)
01172 *         END-EXEC.
      *    MOVE '&"S        E          (   #00004987' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034393837' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CERT-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01173
01174      MOVE CM-CERT-NO             TO WS-CERT-NO.
01175      MOVE CM-INSURED-LAST-NAME   TO WS-INSURED-LNAME.
01176      MOVE CM-INSURED-FIRST-NAME  TO WS-INSURED-FNAME.
01177      MOVE CM-LOAN-NUMBER         TO WS-SAVE-LOAN-NO.
01178      IF CM-PAYMENT-MODE  = SPACES
01179         MOVE 'M'                 TO WS-PAY-FREQ
01180        ELSE
01181         IF CM-PAYMENT-MODE = '1'
01182            MOVE 'W'              TO WS-PAY-FREQ
01183           ELSE
01184            IF CM-PAYMENT-MODE = '2'
01185               MOVE 'SM'          TO WS-PAY-FREQ
01186              ELSE
01187               IF CM-PAYMENT-MODE = '3'
01188                  MOVE 'BW'       TO WS-PAY-FREQ
01189                 ELSE
01190                  IF CM-PAYMENT-MODE = '4'
01191                     MOVE 'SA'    TO WS-PAY-FREQ.
01192
01193      IF CERT-READ
01194         GO TO 7220-BROWSE-PAYMENTS.
01195
01196  7210-STARTBR-ACCOUNT.
01197
01198      MOVE 'Y'                    TO CERT-READ-SW.
01199      MOVE CM-ACCOUNT             TO WS-SAVE-ACCT.
01200      MOVE CM-MEMBER-NO           TO WS-SAVE-MEMBER.
01201
01202      IF CM-SSN-STATE   = CM-STATE AND
01203         CM-SSN-ACCOUNT = CM-ACCOUNT
01204         MOVE SPACES              TO WS-SAVE-SS
01205        ELSE
01206         MOVE CM-SOC-SEC-NO       TO WS-SAVE-SS.
01207
01208      
      * EXEC CICS HANDLE CONDITION
01209 *         NOTOPEN  (9100-ACCOUNT-NOT-OPEN)
01210 *         NOTFND   (7215-END-ACCT)
01211 *         ENDFILE  (7215-END-ACCT)
01212 *         END-EXEC.
      *    MOVE '"$JI''                 ! * #00005027' TO DFHEIV0
           MOVE X'22244A492720202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2A20233030303035303237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01213
01214      
      * EXEC CICS STARTBR
01215 *         RIDFLD    (ACCT-KEY)
01216 *         DATASET   (ACCT-ID)
01217 *         KEYLENGTH (20)
01218 *         GENERIC
01219 *         END-EXEC.
           MOVE 20
             TO DFHEIV11
           MOVE 0
             TO DFHEIV12
      *    MOVE '&,   KG    G          &   #00005033' TO DFHEIV0
           MOVE X'262C2020204B472020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303333' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACCT-ID, 
                 ACCT-KEY, 
                 DFHEIV11, 
                 DFHEIV12, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01220
01221      MOVE ACCT-PARTIAL-KEY       TO ACCT-SAVE-KEY.
01222      MOVE 'Y'                    TO ACCT-BROWSE-SW.
01223
01224  7210-READNEXT-ACCOUNT.
01225      
      * EXEC CICS READNEXT
01226 *         DATASET   (ACCT-ID)
01227 *         SET       (ADDRESS OF ACCOUNT-MASTER)
01228 *         RIDFLD    (ACCT-KEY)
01229 *         END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00005044' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACCT-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01230
01231      IF ACCT-PARTIAL-KEY NOT = ACCT-SAVE-KEY
01232         GO TO 7215-END-ACCT.
01233
01234      IF WS-CERT-EFF-DT  NOT LESS  AM-EFFECTIVE-DT AND
01235         WS-CERT-EFF-DT      LESS  AM-EXPIRATION-DT
01236         NEXT SENTENCE
01237        ELSE
01238         GO TO 7210-READNEXT-ACCOUNT.
01239
01240      MOVE AM-NAME                TO WS-SAVE-ACCT-NAME.
01241      MOVE 'F'                    TO ACCT-BROWSE-SW.
01242
01243      IF WS-ADDR-TYPE = 'A'
01244         MOVE AM-NAME             TO WS-NAME-WORK
01245         MOVE AM-PERSON           TO WS-ADDR1-WORK
01246         MOVE AM-ADDRS            TO WS-ADDR2-WORK
01247 *       MOVE AM-CITY             TO WS-CITY-STATE-WORK
              STRING AM-ADDR-CITY ' ' AM-ADDR-STATE
                 DELIMITED BY '  ' INTO WS-CITY-STATE-WORK
              END-STRING
01248         MOVE SPACES              TO WS-ZIP-WORK
01249         IF AM-CANADIAN-POST-CODE
01250             MOVE AM-CAN-POSTAL-1  TO WS-CAN-POSTAL-1
01251             MOVE AM-CAN-POSTAL-2  TO WS-CAN-POSTAL-2
01252         ELSE
01253             MOVE AM-ZIP-PRIME     TO WS-ZIP-PRIME
01254             IF AM-ZIP-PLUS4 NOT = SPACES  AND  ZEROS
01255                 MOVE '-'          TO WS-ZIP-DASH
01256                 MOVE AM-ZIP-PLUS4 TO WS-ZIP-PLUS4.
01257
01258  7215-END-ACCT.
01259      IF ACCT-BROWSE-STARTED OR ACCT-FOUND
01260          
      * EXEC CICS ENDBR
01261 *             DATASET  (ACCT-ID)
01262 *             END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005082' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACCT-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01263
01264  7220-BROWSE-PAYMENTS.
01265      IF PI-FORM-TYPE = 'I'
01266         PERFORM 7300-FORMAT-INITIAL-DATA THRU 7399-EXIT
01267         GO TO 7205-CLAIM-LOOP.
01268
01269      MOVE ZEROS              TO ACTV-SEQ.
01270      MOVE CLAM-KEY           TO WS-ACTV-KEY   ACTV-PARTIAL-KEY.
01271
01272      
      * EXEC CICS STARTBR
01273 *         DATASET(ACTV-ID)
01274 *         RIDFLD(ACTV-KEY)
01275 *         END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00005094' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTV-ID, 
                 ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01276
01277      
      * EXEC CICS HANDLE CONDITION
01278 *         ENDFILE  (7240-END-BR)
01279 *         END-EXEC.
      *    MOVE '"$''                   ! + #00005099' TO DFHEIV0
           MOVE X'222427202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2B20233030303035303939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01280
01281  7230-READ-NEXT.
01282      
      * EXEC CICS READNEXT
01283 *         DATASET   (ACTV-ID)
01284 *         RIDFLD    (ACTV-KEY)
01285 *         SET       (ADDRESS OF ACTIVITY-TRAILERS)
01286 *         END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00005104' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTV-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01287
01288      IF ACTV-PARTIAL-KEY NOT = WS-ACTV-KEY
01289         GO TO 7240-END-BR.
01290
01291      IF AT-TRAILER-TYPE NOT = '2'
01292         GO TO 7230-READ-NEXT.
01293
01294      IF AT-VOID-DT  NOT = LOW-VALUES
01295         GO TO 7230-READ-NEXT.
01296
01297      IF ADDITIONAL-PAYMENT OR CHARGEABLE-EXPENSE OR
01298         NON-CHARGEABLE-EXPENSE
01299         GO TO 7230-READ-NEXT.
01300
01301 *    MOVE ACTV-CLAIM             TO PROG-CLAIM-NO     (PRT).
01302 *    MOVE WS-SAVE-LOAN-NO        TO PROG-LOAN-NO      (PRT).
01303 *    MOVE AT-PAID-FROM-DT        TO DC-BIN-DATE-1.
01304 *    MOVE SPACE                  TO DC-OPTION-CODE.
01305 *    PERFORM 9700-DATE-LINK THRU 9700-EXIT.
01306 *    MOVE DC-GREG-DATE-1-EDIT    TO PROG-PAID-FROM    (PRT).
01307
01308 *    IF NOT PI-USES-PAID-TO
01309 *       MOVE AT-PAID-THRU-DT     TO DC-BIN-DATE-1
01310 *       MOVE SPACE               TO DC-OPTION-CODE
01311 *       PERFORM 9700-DATE-LINK THRU 9700-EXIT
01312 *       MOVE DC-GREG-DATE-1-EDIT TO PROG-PAID-THRU (PRT)
01313 *    ELSE
01314 *       MOVE AT-PAID-THRU-DT     TO DC-BIN-DATE-1
01315 *       MOVE '6'                 TO DC-OPTION-CODE
01316 *       MOVE +1                  TO DC-ELAPSED-DAYS
01317 *       MOVE +0                  TO DC-ELAPSED-MONTHS
01318 *       PERFORM 9700-DATE-LINK THRU 9700-EXIT
01319 *       MOVE DC-GREG-DATE-1-EDIT TO PROG-PAID-THRU (PRT).
01320
01321      IF AT-PAID-THRU-DT GREATER THAN WS-PAID-THRU-DT
01322         MOVE AT-PAID-THRU-DT     TO WS-PAID-THRU-DT
01323         MOVE DC-GREG-DATE-1-EDIT TO WS-GREG-PAID-THRU.
01324
01325 *    MOVE AT-DAYS-IN-PERIOD      TO PROG-NO-DAYS      (PRT).
01326 *    MOVE AT-DAYS-IN-PERIOD      TO PROG-DAYS-PAID    (PRT).
01327 *    MOVE AT-ELIMINATION-DAYS    TO PROG-ELIM-DAYS    (PRT).
01328 *    MOVE AT-DAILY-RATE          TO PROG-DAILY-RATE   (PRT).
01329 *    MOVE AT-AMOUNT-PAID         TO PROG-PAYMENT-AMT  (PRT).
01330 *    ADD AT-AMOUNT-PAID          TO WS-TOTAL-PAID.
01331      ADD  1                      TO PRT.
01332
01333  7240-END-BR.
01334      
      * EXEC CICS ENDBR
01335 *         DATASET   (ACTV-ID)
01336 *         END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005156' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313536' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTV-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01337
01338      GO TO 7205-CLAIM-LOOP.
01339
01340      EJECT
01341  7250-PRINT-FORM.
01342      IF WS-ADDR-TYPE = 'A'  AND WS-ADDR-SEQ = ZEROS
01343         GO TO 7255-CHECK-FORM.
01344
01345      MOVE WS-ADDR-SEQ             TO ACTV-SEQ.
01346      MOVE LA4-CARRIER             TO ACTV-CARRIER.
01347      MOVE LA4-CLAIM-NO            TO ACTV-CLAIM.
01348      MOVE LA4-CERT-NO             TO ACTV-CERT-NO.
01349
01350      
      * EXEC CICS HANDLE CONDITION
01351 *         NOTFND   (7255-CHECK-FORM)
01352 *         END-EXEC.
      *    MOVE '"$I                   ! , #00005172' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2C20233030303035313732' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01353
01354      
      * EXEC CICS READ
01355 *         DATASET  (ACTV-ID)
01356 *         RIDFLD   (ACTV-KEY)
01357 *         SET      (ADDRESS OF ACTIVITY-TRAILERS)
01358 *         END-EXEC.
      *    MOVE '&"S        E          (   #00005176' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTV-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01359
01360      MOVE AT-MAIL-TO-NAME        TO WS-NAME-WORK.
01361      MOVE AT-ADDRESS-LINE-1      TO WS-ADDR1-WORK.
01362      MOVE AT-ADDRESS-LINE-2      TO WS-ADDR2-WORK.
01363 *    MOVE AT-CITY-STATE          TO WS-CITY-STATE-WORK.
           STRING AT-CITY ' ' AT-STATE
              DELIMITED BY '  ' INTO WS-CITY-STATE-WORK
           END-STRING
01364      MOVE SPACES                 TO WS-ZIP-WORK.
01365      IF AT-CANADIAN-POST-CODE
01366          MOVE AT-CAN-POSTAL-1    TO WS-CAN-POSTAL-1
01367          MOVE AT-CAN-POSTAL-2    TO WS-CAN-POSTAL-2
01368      ELSE
01369          MOVE AT-ZIP-CODE        TO WS-ZIP-PRIME
01370          IF AT-ZIP-PLUS4 NOT = SPACES  AND  ZEROS
01371              MOVE '-'            TO WS-ZIP-DASH
01372              MOVE AT-ZIP-PLUS4   TO WS-ZIP-PLUS4.
01373
01374
01375      EJECT
01376  7255-CHECK-FORM.
01377      IF PI-FORM-TYPE = 'P'
01378          GO TO 7260-PRINT-PROG.
01379
01380      MOVE '1'                 TO WS-PASSED-CNTL-CHAR.
CIDMOD     MOVE INIT-PRINT-LINE-1   TO WS-PASSED-DATA.
01381      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
01382
01383      MOVE SPACES              TO WS-PASSED-CNTL-CHAR
01384                                  WS-PASSED-DATA.
01385      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT 4 TIMES.
01386
01387 ****************  MAY BE GARBAGE????????????????
01388      MOVE INIT-PRINT-LINE-1   TO WS-PASSED-DATA.
01389      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
01390
01391      MOVE SPACES              TO WS-PASSED-DATA
01392                                  WS-PASSED-CNTL-CHAR.
01393      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT 13 TIMES.
01394
01395      MOVE SPACES              TO INIT-PRINT-LINE-2.
01396      MOVE WS-SAVE-ACCT-NAME   TO INIT-ACCOUNT-NAME.
01397      MOVE INIT-PRINT-LINE-2   TO WS-PASSED-DATA.
01398      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
01399
01400      MOVE WS-SAVE-MEMBER      TO INIT-ACCOUNT-NAME.
01401      MOVE WS-NAME-WORK        TO INIT-NAME-1.
01402      MOVE INIT-PRINT-LINE-2   TO WS-PASSED-DATA.
01403      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
01404
01405      MOVE WS-SAVE-SS          TO INIT-ACCOUNT-NAME.
01406      MOVE WS-ADDR1-WORK       TO INIT-NAME-1.
01407      MOVE INIT-PRINT-LINE-2   TO WS-PASSED-DATA.
01408      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
01409
01410      MOVE SPACES              TO INIT-PRINT-LINE-2.
01411      MOVE WS-ADDR2-WORK       TO INIT-NAME-1.
01412      MOVE INIT-PRINT-LINE-2   TO WS-PASSED-DATA.
01413      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
01414
01415      MOVE WS-CITY-STATE-WORK  TO WS-LABEL-HOLD-AREA.
01416      MOVE WS-ZIP-WORK         TO WS-ZIP-LABEL.
01417
01418      MOVE SPACES              TO INIT-PRINT-LINE-2
01419                                  INIT-PRINT-LINE-3.
01420      MOVE WS-LABEL-HOLD-AREA  TO INIT-NAME-1.
01421      MOVE WS-SAVE-CLAIM       TO INIT-CLAIM-ACCT.
01422      MOVE WS-CARRIER (1)      TO INIT-CLAIM-CARRIER.
01423      MOVE INIT-PRINT-LINE-3   TO WS-PASSED-DATA.
01424      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
01425
01426      MOVE SPACES              TO INIT-PRINT-LINE-3.
01427      MOVE WS-SAVE-ACCT        TO INIT-CLAIM-ACCT.
01428      MOVE INIT-PRINT-LINE-3   TO WS-PASSED-DATA.
01429      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
01430
01431      MOVE SPACES              TO INIT-PRINT-LINE-4-1.
01432      MOVE WS-PRINT-SW         TO INIT-PRINT-SW.
01433      MOVE SAVE-DATE           TO INIT-PRINT-DATE.
01434      MOVE INIT-PRINT-LINE-4-1 TO WS-PASSED-DATA.
01435      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
01436
01437      MOVE 'X'                 TO WS-PROG-END.
01438      MOVE SPACES              TO WS-PASSED-DATA.
CIDMOD*    PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
01440      GO TO 7299-EXIT.
01441      EJECT
01442  7260-PRINT-PROG.
01443
01444      IF  PI-COMPANY-ID   EQUAL   'RMC' OR 'LAP'
01445          GO TO 7260-PRINT-RMC-LAP-PROG.
01446
01447      IF  PI-COMPANY-ID   EQUAL   'LBL'
01448          NEXT SENTENCE
01449      ELSE
01450         GO TO 7260-PRINT-PROG-CONT.
01451
01452      MOVE  '1'                TO WS-PASSED-CNTL-CHAR.
01453      MOVE  SPACES             TO WS-PASSED-DATA.
01454      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
01455
01456      MOVE  SPACES             TO WS-PASSED-DATA
01457                                  WS-PASSED-CNTL-CHAR.
01458      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT 3 TIMES.
01459
01460      MOVE  SPACES             TO LBL-PRINT-LINE-1.
01461      MOVE  WS-NAME-WORK       TO LBL-INSURED-NAME.
01462      MOVE  CLAM-CLAIM         TO LBL-CLAIM-NO.
01463      MOVE  LBL-PRINT-LINE-1   TO WS-PASSED-DATA.
01464      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
01465
01466      IF  WS-ADDR1-WORK     =    SPACES
01467          MOVE  SPACES             TO LBL-PRINT-LINE-3
01468          MOVE  WS-ADDR2-WORK      TO LBL-INSURED-ADDR2
01469          MOVE  LBL-PRINT-LINE-3   TO LBL-PRINT-LINE-2
01470          MOVE  LBL-PRINT-LINE-2   TO WS-PASSED-DATA
01471          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
01472
01473          MOVE  SPACES             TO LBL-PRINT-LINE-4
01474          MOVE  WS-CITY-STATE-WORK TO WS-LABEL-HOLD-AREA
01475          MOVE  WS-ZIP-WORK        TO WS-ZIP-LABEL
01476          MOVE  WS-LABEL-HOLD-AREA TO LBL-INSURED-CITY-ST
01477          MOVE  LBL-PRINT-LINE-4   TO LBL-PRINT-LINE-3
01478          MOVE  LBL-PRINT-LINE-3   TO WS-PASSED-DATA
01479          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
01480
01481          MOVE  SPACES             TO LBL-PRINT-LINE-5
01482          MOVE  WS-PAID-THRU-DT    TO DC-BIN-DATE-1
01483          MOVE  ' '                TO DC-OPTION-CODE
01484          PERFORM 9700-DATE-LINK THRU 9700-EXIT
01485          MOVE  DC-GREG-DATE-1-EDIT  TO LBL-PAID-THRU-DATE
01486          MOVE  LBL-PRINT-LINE-5   TO LBL-PRINT-LINE-4
01487          MOVE  LBL-PRINT-LINE-4   TO WS-PASSED-DATA
01488          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
01489
01490          MOVE  'X'                TO WS-PROG-END
01491          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
01492          GO TO 7299-EXIT.
01493
01494      IF  WS-ADDR1-WORK  NOT =  SPACES   AND
01495          WS-ADDR2-WORK      =  SPACES
01496          MOVE  SPACES             TO LBL-PRINT-LINE-2
01497          MOVE  WS-ADDR1-WORK      TO LBL-INSURED-ADDR1
01498          MOVE  LBL-PRINT-LINE-2   TO WS-PASSED-DATA
01499          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
01500
01501          MOVE  SPACES             TO LBL-PRINT-LINE-4
01502          MOVE  WS-CITY-STATE-WORK TO WS-LABEL-HOLD-AREA
01503          MOVE  WS-ZIP-WORK        TO WS-ZIP-LABEL
01504          MOVE  WS-LABEL-HOLD-AREA TO LBL-INSURED-CITY-ST
01505          MOVE  LBL-PRINT-LINE-4   TO LBL-PRINT-LINE-3
01506          MOVE  LBL-PRINT-LINE-3   TO WS-PASSED-DATA
01507          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
01508
01509          MOVE  SPACES             TO LBL-PRINT-LINE-5
01510          MOVE  WS-PAID-THRU-DT    TO DC-BIN-DATE-1
01511          MOVE  ' '                TO DC-OPTION-CODE
01512          PERFORM 9700-DATE-LINK THRU 9700-EXIT
01513          MOVE  DC-GREG-DATE-1-EDIT  TO LBL-PAID-THRU-DATE
01514          MOVE  LBL-PRINT-LINE-5   TO LBL-PRINT-LINE-4
01515          MOVE  LBL-PRINT-LINE-4   TO WS-PASSED-DATA
01516          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
01517
01518          MOVE  'X'                TO WS-PROG-END
01519          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
01520          GO TO 7299-EXIT.
01521
01522      IF  WS-ADDR1-WORK   NOT =  SPACES AND
01523          WS-ADDR2-WORK   NOT =  SPACES
01524          MOVE  SPACES             TO LBL-PRINT-LINE-2
01525          MOVE  WS-ADDR1-WORK      TO LBL-INSURED-ADDR1
01526          MOVE  LBL-PRINT-LINE-2   TO WS-PASSED-DATA
01527          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
01528
01529          MOVE  SPACES             TO LBL-PRINT-LINE-3
01530          MOVE  WS-ADDR2-WORK      TO LBL-INSURED-ADDR2
01531          MOVE  LBL-PRINT-LINE-3   TO WS-PASSED-DATA
01532          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
01533
01534          MOVE  SPACES             TO LBL-PRINT-LINE-4
01535          MOVE  WS-CITY-STATE-WORK TO WS-LABEL-HOLD-AREA
01536          MOVE  WS-ZIP-WORK        TO WS-ZIP-LABEL
01537          MOVE  WS-LABEL-HOLD-AREA TO LBL-INSURED-CITY-ST
01538          MOVE  LBL-PRINT-LINE-4   TO WS-PASSED-DATA
01539          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
01540
01541          MOVE  SPACES             TO LBL-PRINT-LINE-5
01542          MOVE  WS-PAID-THRU-DT    TO DC-BIN-DATE-1
01543          MOVE  ' '                TO DC-OPTION-CODE
01544          PERFORM 9700-DATE-LINK THRU 9700-EXIT
01545          MOVE  DC-GREG-DATE-1-EDIT  TO LBL-PAID-THRU-DATE
01546          MOVE  LBL-PRINT-LINE-5   TO WS-PASSED-DATA
01547          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
01548
01549          MOVE  'X'                TO WS-PROG-END
01550          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
01551          GO TO 7299-EXIT.
01552
01553  7260-PRINT-RMC-LAP-PROG.
01554
01555      MOVE  '1'                TO WS-PASSED-CNTL-CHAR.
01556      MOVE  SPACES             TO WS-PASSED-DATA.
01557      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
01558
01559      MOVE  SPACES             TO WS-PASSED-DATA
01560                                  WS-PASSED-CNTL-CHAR.
01561      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT 8 TIMES.
01562
01563      MOVE  SPACES             TO RMC-LAP-PRINT-LINE-1.
01564      MOVE  CLAM-CLAIM         TO RMC-LAP-CLAIM-NO.
01565      MOVE  CLAM-CERT          TO RMC-LAP-POLICY-NO.
01566      MOVE  RMC-LAP-PRINT-LINE-1 TO WS-PASSED-DATA.
01567      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
01568
01569      MOVE  SPACES             TO WS-PASSED-DATA
01570                                  WS-PASSED-CNTL-CHAR.
01571      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT 3 TIMES.
01572
01573      MOVE  SPACES             TO RMC-LAP-PRINT-LINE-2.
01574      MOVE  WS-NAME-WORK       TO RMC-LAP-INSURED-NAME.
01575      MOVE  RMC-LAP-PRINT-LINE-2 TO WS-PASSED-DATA.
01576      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
01577
01578      IF  WS-ADDR1-WORK     =    SPACES
01579          MOVE  SPACES             TO RMC-LAP-PRINT-LINE-3
01580          MOVE  WS-ADDR2-WORK      TO RMC-LAP-INSURED-ADDR2
01581          MOVE  RMC-LAP-PRINT-LINE-4 TO RMC-LAP-PRINT-LINE-3
01582          MOVE  RMC-LAP-PRINT-LINE-3 TO WS-PASSED-DATA
01583          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
01584
01585          MOVE  SPACES             TO RMC-LAP-PRINT-LINE-5
01586          MOVE  WS-CITY-STATE-WORK TO WS-LABEL-HOLD-AREA
01587          MOVE  WS-ZIP-WORK        TO WS-ZIP-LABEL
01588          MOVE  WS-LABEL-HOLD-AREA TO RMC-LAP-INSURED-CITY-ST
01589          MOVE  RMC-LAP-PRINT-LINE-5 TO RMC-LAP-PRINT-LINE-4
01590          MOVE  RMC-LAP-PRINT-LINE-4 TO WS-PASSED-DATA
01591          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
01592
01593          MOVE  SPACES             TO WS-PASSED-DATA
01594                                      WS-PASSED-CNTL-CHAR
01595          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT 37 TIMES
01596
01597          MOVE SPACES              TO RMC-LAP-PRINT-LINE-6
01598          MOVE WS-SPEC-INST1       TO RMC-LAP-TEXT-LINE-1
01599          MOVE  RMC-LAP-PRINT-LINE-6 TO WS-PASSED-DATA
01600          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
01601
01602          MOVE SPACES              TO RMC-LAP-PRINT-LINE-7
01603          MOVE WS-SPEC-INST2       TO RMC-LAP-TEXT-LINE-2
01604          MOVE  RMC-LAP-PRINT-LINE-7 TO WS-PASSED-DATA
01605          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
01606
01607          MOVE SPACES              TO RMC-LAP-PRINT-LINE-8
01608          MOVE WS-SPEC-INST3       TO RMC-LAP-TEXT-LINE-3
01609          MOVE  RMC-LAP-PRINT-LINE-8 TO WS-PASSED-DATA
01610          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
01611
01612          MOVE  'X'                TO WS-PROG-END
01613          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
01614          GO TO 7299-EXIT.
01615
01616      IF  WS-ADDR1-WORK  NOT =  SPACES   AND
01617          WS-ADDR2-WORK      =  SPACES
01618          MOVE  SPACES             TO RMC-LAP-PRINT-LINE-3
01619          MOVE  WS-ADDR1-WORK      TO RMC-LAP-INSURED-ADDR1
01620          MOVE  RMC-LAP-PRINT-LINE-3 TO WS-PASSED-DATA
01621          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
01622
01623          MOVE  SPACES             TO RMC-LAP-PRINT-LINE-5
01624          MOVE  WS-CITY-STATE-WORK TO WS-LABEL-HOLD-AREA
01625          MOVE  WS-ZIP-WORK        TO WS-ZIP-LABEL
01626          MOVE  WS-LABEL-HOLD-AREA TO RMC-LAP-INSURED-CITY-ST
01627          MOVE  RMC-LAP-PRINT-LINE-5 TO RMC-LAP-PRINT-LINE-4
01628          MOVE  RMC-LAP-PRINT-LINE-4 TO WS-PASSED-DATA
01629          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
01630
01631          MOVE  SPACES             TO WS-PASSED-DATA
01632                                      WS-PASSED-CNTL-CHAR
01633          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT 37 TIMES
01634
01635          MOVE SPACES              TO RMC-LAP-PRINT-LINE-6
01636          MOVE WS-SPEC-INST1       TO RMC-LAP-TEXT-LINE-1
01637          MOVE  RMC-LAP-PRINT-LINE-6 TO WS-PASSED-DATA
01638          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
01639
01640          MOVE SPACES              TO RMC-LAP-PRINT-LINE-7
01641          MOVE WS-SPEC-INST2       TO RMC-LAP-TEXT-LINE-2
01642          MOVE  RMC-LAP-PRINT-LINE-7 TO WS-PASSED-DATA
01643          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
01644
01645          MOVE SPACES              TO RMC-LAP-PRINT-LINE-8
01646          MOVE WS-SPEC-INST3       TO RMC-LAP-TEXT-LINE-3
01647          MOVE  RMC-LAP-PRINT-LINE-8 TO WS-PASSED-DATA
01648
01649          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
01650          MOVE  'X'                TO WS-PROG-END
01651          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
01652          GO TO 7299-EXIT.
01653
01654      IF  WS-ADDR1-WORK   NOT =  SPACES AND
01655          WS-ADDR2-WORK   NOT =  SPACES
01656          MOVE  SPACES             TO RMC-LAP-PRINT-LINE-3
01657          MOVE  WS-ADDR1-WORK      TO RMC-LAP-INSURED-ADDR1
01658          MOVE  RMC-LAP-PRINT-LINE-3 TO WS-PASSED-DATA
01659          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
01660
01661          MOVE  SPACES             TO RMC-LAP-PRINT-LINE-4
01662          MOVE  WS-ADDR2-WORK      TO RMC-LAP-INSURED-ADDR2
01663          MOVE  RMC-LAP-PRINT-LINE-4 TO WS-PASSED-DATA
01664          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
01665
01666          MOVE  SPACES             TO RMC-LAP-PRINT-LINE-5
01667          MOVE  WS-CITY-STATE-WORK TO WS-LABEL-HOLD-AREA
01668          MOVE  WS-ZIP-WORK        TO WS-ZIP-LABEL
01669          MOVE  WS-LABEL-HOLD-AREA TO RMC-LAP-INSURED-CITY-ST
01670          MOVE  RMC-LAP-PRINT-LINE-5 TO WS-PASSED-DATA
01671          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
01672
01673          MOVE  SPACES             TO WS-PASSED-DATA
01674                                      WS-PASSED-CNTL-CHAR
01675          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT 37 TIMES
01676
01677          MOVE SPACES              TO RMC-LAP-PRINT-LINE-6
01678          MOVE WS-SPEC-INST1       TO RMC-LAP-TEXT-LINE-1
01679          MOVE  RMC-LAP-PRINT-LINE-6 TO WS-PASSED-DATA
01680          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
01681
01682          MOVE SPACES              TO RMC-LAP-PRINT-LINE-7
01683          MOVE WS-SPEC-INST2       TO RMC-LAP-TEXT-LINE-2
01684          MOVE  RMC-LAP-PRINT-LINE-7 TO WS-PASSED-DATA
01685          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
01686
01687          MOVE SPACES              TO RMC-LAP-PRINT-LINE-8
01688          MOVE WS-SPEC-INST3       TO RMC-LAP-TEXT-LINE-3
01689          MOVE  RMC-LAP-PRINT-LINE-8 TO WS-PASSED-DATA
01690
01691          MOVE  'X'                TO WS-PROG-END
01692          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
01693          GO TO 7299-EXIT.
01694
01695  7260-PRINT-PROG-CONT.
01696
01697      MOVE '1'                 TO WS-PASSED-CNTL-CHAR.
01698      MOVE SPACES              TO WS-PASSED-DATA.
01699      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
01700
01701      MOVE SPACES              TO WS-PASSED-DATA
01702                                  WS-PASSED-CNTL-CHAR.
01703      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT 8 TIMES.
01704
01705      MOVE SPACES              TO PROG-PRINT-LINE-1
01706                                  WS-PASSED-CNTL-CHAR.
01707      MOVE WS-PRINT-SW         TO PROG-PRINT-SW.
01708      MOVE SAVE-DATE           TO PROG-PRINT-DATE.
01709      MOVE PROG-PRINT-LINE-1   TO WS-PASSED-DATA.
01710      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
01711
01712      MOVE SPACES              TO WS-PASSED-DATA
01713                                  WS-PASSED-CNTL-CHAR.
01714      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT 2 TIMES.
01715
01716      MOVE SPACES              TO PROG-PRINT-LINE-2.
01717      MOVE WS-NAME-WORK        TO PROG-NAME-ADDR.
01718      MOVE PROG-PRINT-LINE-2   TO WS-PASSED-DATA.
01719      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
01720
01721      MOVE WS-ADDR1-WORK       TO PROG-NAME-ADDR.
01722      MOVE WS-SPEC-INST1       TO PROG-SPEC-INST.
01723      MOVE PROG-PRINT-LINE-2   TO WS-PASSED-DATA.
01724      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
01725
01726      MOVE WS-ADDR2-WORK       TO PROG-NAME-ADDR.
01727      MOVE WS-SPEC-INST2       TO PROG-SPEC-INST.
01728      MOVE PROG-PRINT-LINE-2   TO WS-PASSED-DATA.
01729      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
01730
01731      MOVE WS-CITY-STATE-WORK  TO WS-LABEL-HOLD-AREA.
01732      MOVE SPACES              TO PROG-SPEC-INST.
01733      MOVE WS-ZIP-WORK         TO WS-ZIP-LABEL.
01734
01735      MOVE WS-LABEL-HOLD-AREA  TO PROG-NAME-ADDR.
01736      MOVE WS-SPEC-INST3       TO PROG-SPEC-INST.
01737      MOVE PROG-PRINT-LINE-2   TO WS-PASSED-DATA.
01738      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
01739
01740      MOVE SPACES              TO WS-PASSED-DATA.
01741      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT 5 TIMES.
01742
01743      MOVE WS-SAVE-ACCT-NAME   TO PROG-ACCOUNT-NAME.
01744      MOVE WS-SAVE-CLAIM       TO PROG-CLAIM.
01745      MOVE WS-CARRIER (1)      TO PROG-CARRIER.
01746      MOVE WS-SAVE-MEMBER      TO PROG-MEMBER-NO .
01747      MOVE WS-SAVE-ACCT        TO WS-WORK-ACCOUNT.
01748      MOVE WS-WRK-ACCT         TO PROG-ACCOUNT-NO.
01749      MOVE PROG-PRINT-LINE-3   TO WS-PASSED-DATA.
01750      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
01751
01752      MOVE 'X'                 TO WS-PROG-END.
CIDMOD*    PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.
01754      GO TO 7299-EXIT.
01755
01756  7299-EXIT.
01757       EXIT.
01758      EJECT
01759  7300-FORMAT-INITIAL-DATA.
01760      MOVE CM-AH-BENEFIT-CD       TO WS-BEN-CD.
01761      MOVE WS-ACCESS              TO CNTL-ACCESS.
01762      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
01763      MOVE '5'                    TO CNTL-REC-TYPE.
01764      MOVE +0                     TO CNTL-SEQ-NO.
01765
01766
01767      
      * EXEC CICS HANDLE CONDITION
01768 *        ENDFILE  (7399-EXIT)
01769 *        NOTFND   (7399-EXIT)
01770 *        END-EXEC.
      *    MOVE '"$''I                  ! - #00005593' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2D20233030303035353933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01771
01772     
      * EXEC CICS READ
01773 *        DATASET  ('ELCNTL')
01774 *        SET      (ADDRESS OF CONTROL-FILE)
01775 *        RIDFLD   (ELCNTL-KEY)
01776 *        GTEQ
01777 *        END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S        G          (   #00005598' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353938' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01778
01779      IF (CNTL-COMP-ID  NOT = CF-COMPANY-ID)  OR
01780         (CNTL-REC-TYPE NOT = CF-RECORD-TYPE)
01781            GO TO 7399-EXIT.
01782
01783      PERFORM 7400-BENEFIT-DUMMY THRU 7400-DUMMY-EXIT
01784          VARYING SUB-1 FROM 1 BY 1 UNTIL
01785             ((SUB-1 GREATER 8) OR
01786             (CF-BENEFIT-CODE (SUB-1) = WS-BEN-CD)).
01787
01788      IF SUB-1 NOT = 9
01789         MOVE CF-BENEFIT-ALPHA (SUB-1) TO WS-BENEFIT-SAVE
01790         IF WS-BEN-DAYS GREATER THAN WS-BEN-DAYS-HOLD
01791            MOVE WS-BEN-DAYS      TO WS-BEN-DAYS-HOLD
01792            MOVE SPACES           TO INIT-PRINT-LINE-1
01793            MOVE WS-BEN-DAYS-HOLD TO INIT-ELIM-DAYS.
01794
01795  7399-EXIT.
01796
01797  7400-BENEFIT-DUMMY.
01798
01799  7400-DUMMY-EXIT.
01800      EXIT.
01801
01802      EJECT
01803  8000-READ-FORM-CONTROL.
01804      
      * EXEC CICS HANDLE CONDITION
01805 *         NOTOPEN  (8870-ACTV-NOT-OPEN)
01806 *         NOTFND   (8050-REC-NOT-FOUND)
01807 *         END-EXEC.
      *    MOVE '"$JI                  ! . #00005630' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2E20233030303035363330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01808
01809      
      * EXEC CICS READ
01810 *         DATASET  (ACTV-ID)
01811 *         RIDFLD   (ACTV-KEY)
01812 *         SET      (ADDRESS OF ACTIVITY-TRAILERS)
01813 *         UPDATE
01814 *         END-EXEC.
      *    MOVE '&"S        EU         (   #00005635' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363335' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTV-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01815
01816      GO TO 8099-EXIT.
01817
01818  8050-REC-NOT-FOUND.
01819      MOVE '1'                    TO FORM-CONTROL-SW.
01820
01821  8099-EXIT.
01822       EXIT.
01823      EJECT
01824  8100-READ-HEADER.
01825      MOVE ARCH-NUMBER2           TO ARCH-NUMBER.
01826      MOVE '4'                    TO ARCH-REC-TYPE.
01827      MOVE ZEROS                  TO ARCH-SEQ.
01828
01829      
      * EXEC CICS HANDLE CONDITION
01830 *         NOTFND  (8150-NOT-FOUND)
01831 *         END-EXEC.
      *    MOVE '"$I                   ! / #00005655' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2F20233030303035363535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01832
01833      
      * EXEC CICS READ
01834 *         DATASET (ARCH-ID)
01835 *         RIDFLD  (ARCH-KEY)
01836 *         INTO    (LETTER-ARCHIVE)
01837 *         UPDATE
01838 *         END-EXEC.
           MOVE LENGTH OF
            LETTER-ARCHIVE
             TO DFHEIV11
      *    MOVE '&"IL       EU         (   #00005659' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363539' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ARCH-ID, 
                 LETTER-ARCHIVE, 
                 DFHEIV11, 
                 ARCH-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01839
01840      MOVE LA4-CARRIER             TO ACTV-CARRIER.
01841      MOVE LA4-CLAIM-NO            TO ACTV-CLAIM.
01842      MOVE LA4-CERT-NO             TO ACTV-CERT-NO.
01843      MOVE LA4-FORM-TRLR-SEQ       TO ACTV-SEQ.
01844      GO TO 8199-EXIT.
01845
01846  8150-NOT-FOUND.
01847      MOVE '1'                    TO HEADER-SW.
01848
01849  8199-EXIT.
01850       EXIT.
01851      EJECT
01852  8200-DELETE-ARCHIVE.
01853      PERFORM 8100-READ-HEADER THRU 8199-EXIT.
01854
01855      IF PI-COMPANY-ID  = 'FIA'
01856         MOVE LA4-CARRIER            TO MSG-CARRIER
01857         MOVE LA4-CLAIM-NO           TO MSG-CLAIM
01858         MOVE LA4-CERT-NO            TO MSG-CERT
01859         MOVE LA-ARCHIVE-NO          TO MSG-ARCH-NO
01860         MOVE LA4-FORM-TYPE          TO MSG-TYPE
01861             
      * EXEC CICS WRITEQ TD
01862 *                QUEUE     ('CSMT')
01863 *                FROM      (MSG-MESSAGE)
01864 *                LENGTH    (TRANS-DATA-LENGTH)
01865 *                END-EXEC.
           MOVE 'CSMT' TO DFHEIV5
      *    MOVE '(" L                  &   #00005687' TO DFHEIV0
           MOVE X'2822204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363837' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV5, 
                 MSG-MESSAGE, 
                 TRANS-DATA-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01866
01867      
      * EXEC CICS DELETE
01868 *         DATASET  (ARCH-ID)
01869 *         END-EXEC.
      *    MOVE '&(                    &   #00005693' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ARCH-ID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01870
01871  8299-EXIT.
01872       EXIT.
01873      EJECT
01874  8850-FORM-TRLR-NOT-FND.
01875      MOVE 'FORM TRAILERS NOT FOUND' TO ERROR-LINE.
01876      PERFORM 400-SEND-TEXT.
01877      
      * EXEC CICS SYNCPOINT
01878 *              ROLLBACK
01879 *              END-EXEC.
      *    MOVE '6"R                   !   #00005703' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303035373033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01880
01881      GO TO 200-END-DATA.
01882
01883  8860-ARCH2-NOT-OPEN.
01884      MOVE 'LETTER ARCHIVE FILE NOT OPEN - ELARCH2' TO ERROR-LINE.
01885      PERFORM 400-SEND-TEXT.
01886      GO TO 200-END-DATA.
01887
01888  8870-ACTV-NOT-OPEN.
01889      MOVE 'ACTIVITY TRAILER FILE NOT OPEN - ELTRLR' TO ERROR-LINE.
01890      PERFORM 400-SEND-TEXT.
01891      GO TO 200-END-DATA.
01892
01893  8880-CLAM-NOT-OPEN.
01894      MOVE 'CLAIM MASTER FILE NOT OPEN - ELMSTR' TO ERROR-LINE.
01895      PERFORM 400-SEND-TEXT.
01896      
      * EXEC CICS SYNCPOINT
01897 *              ROLLBACK
01898 *              END-EXEC.
      *    MOVE '6"R                   !   #00005722' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303035373232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01899
01900      GO TO 200-END-DATA.
01901
CIDMOD 8890-ARCH-NOT-OPEN.
CIDMOD     MOVE 'LETTER ARCHIVE FILE NOT OPEN - ELARCH' TO ERROR-LINE.
CIDMOD     PERFORM 400-SEND-TEXT.
CIDMOD     GO TO 200-END-DATA.
CIDMOD
01902  9000-CERT-NOT-OPEN.
01903      MOVE 'CERT MASTER FILE NOT OPEN - ELCERT' TO ERROR-LINE.
01904      PERFORM 400-SEND-TEXT.
01905      
      * EXEC CICS SYNCPOINT
01906 *              ROLLBACK
01907 *              END-EXEC.
      *    MOVE '6"R                   !   #00005736' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303035373336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01908
01909      GO TO 200-END-DATA.
01910
01911  9100-ACCOUNT-NOT-OPEN.
01912      MOVE 'ACCOUNT MASTER FILE NOT OPEN - ERACCT' TO ERROR-LINE.
01913      PERFORM 400-SEND-TEXT.
01914      
      * EXEC CICS SYNCPOINT
01915 *              ROLLBACK
01916 *              END-EXEC.
      *    MOVE '6"R                   !   #00005745' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303035373435' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01917
01918      GO TO 200-END-DATA.
01919
01920      EJECT
01921
01922  9700-DATE-LINK.
01923      MOVE LINK-ELDATCV           TO PGM-NAME.
01924      
      * EXEC CICS LINK
01925 *        PROGRAM   (PGM-NAME)
01926 *        COMMAREA  (DATE-CONVERSION-DATA)
01927 *        LENGTH    (DC-COMM-LENGTH)
01928 *        END-EXEC.
      *    MOVE '."C                   ''   #00005755' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035373535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01929
01930  9700-EXIT.
01931       EXIT.
01932      EJECT
uktdel*9800-PRINT-ROUTINE.             COPY ELPRTCVP.
uktins 9800-PRINT-ROUTINE.
uktins*    COPY ELPRTCVP.
00001 ******************************************************************
00002 ***                                                              *
00003 ***                          ELPRTCVP.                           *
00004 ***                          VMOD=2.003                          *
00005 ***                                                              *
00006 ***     COPY MEMBER FOR TERMINAL ONLINE PRINT ROUTINE.           *
00007 ***     THIS ROUTINE WILL ACCOMODATE PRINTING TO A 3270          *
00008 ***     TERMINAL PRINTER. A BUFFER OF UP TO 1920 CHARACTERS      *
00009 ***     IS ACCUMULATED AND PRINTED COLLECTIVELY.                 *
00010 ***                                                              *
00011 ***     THIS ROUTINE TO BE USED ONLY WITH ACCOMPANIMENT          *
00012 ***      OF THE WORKING-STORAGE COPY MEMBER ( ELPRTCVD )         *
00013 ***     THE HOST PROGRAM MUST INITIALIZE THE FOLLOWING 3 FIELDS  *
00014 ***      FROM THE ABOVE COPY MEMBER FOR THIS PROCEDURE TO BE     *
00015 ***      SUCCESSFUL.                                             *
00016 ***      05  WS-LINE-LEN    PIC  S9(4)  COMP  VALUE +80.         *
00017 ***                         LENGTH OF THE LINE TO BE PRINTED     *
00018 ***                         DEFAULT IS 80, YOU CAN USE ANY NUMBER*
00019 ***                         UP TO 132.  THIS FIELD IS ONLY ACCEP-*
00020 ***                         TED THE FIRST TIME THRU THE ROUTINE. *
00021 ***      05  WS-PROG-END    PIC  X  VALUE SPACES.                *
00022 ***                         PROGRAM END SWITCH. INITIALIZED      *
00023 ***                         TO SPACE-     MOVE IN ANY NONBLANK   *
00024 ***                         TO IT WHEN PROGRAM IS FINISHED.      *
00025 ***      05  WS-PRINT-AREA.                                      *
00026 ***          10  WS-PASSED-CNTL-CHAR     PIC X.                  *
00027 ***          10  WS-PASSED-DATA          PIC X(132).             *
00028 ***                         USE THE DATA TO BE PRINTED IN THE    *
00029 ***                         WS-PASSED-DATA.                      *
00030 ***                         USE THE STANDARD CARRIAGE CONTROL    *
00031 ***                         CHARACTER IN THE WS-PASSED-CNTL-CHAR *
00032 ***                           SINGLE-SPACE            VALUE ' '  *
00033 ***                           DOUBLE-SPACE            VALUE '0'  *
00034 ***                           TRIPLE-SPACE            VALUE '-'  *
00035 ***                           TOP-PAGE                VALUE '1'  *
00036 ***      NOTE: A LINE COUNT IS PROVIDED IN FIELDNAME -WS-LINE-CNT*
00037 ***            THE USE OF THIS FIELD IS OPTIONAL.                *
00038 ***            THIS ROUTINE WILL ONLY ADD 1, 2, OR 3             *
00039 ***            TO THIS COUNT DEPENDING ON THE WS-PASSED-CNTL-CHAR*
00040 ***            AND RESET THE COUNT TO ZERO WHEN TOP-PAGE         *
00041 ***            CONDITION.                                        *
00042 ***                                                              *
00043 ******************************************************************
00044
00045  ELPRTCVP.
00046
pemuni*    IF PI-COMPANY-ID IS EQUAL TO 'DMD' OR 'CID'
pemuni     IF PI-COMPANY-ID IS EQUAL TO 'DMD' OR 'XXX'
00048          MOVE 'P'                TO DL34-PROCESS-TYPE
00049          MOVE THIS-PGM           TO DL34-PRINT-PROGRAM-ID
00050          MOVE PI-PROCESSOR-ID    TO DL34-USERID
00051          MOVE PI-COMPANY-ID      TO DL34-COMPANY-ID
00052          MOVE WS-PRINT-AREA      TO DL34-PRINT-LINE
00053          MOVE PI-ALT-DMD-PRT-ID  TO DL34-OVERRIDE-PRINTER-ID
00054
00055          
      * EXEC CICS LINK
00056 *            PROGRAM    ('DLO034')
00057 *            COMMAREA   (DLO034-COMMUNICATION-AREA)
00058 *            LENGTH     (DLO034-REC-LENGTH)
00059 *        END-EXEC
           MOVE 'DLO034' TO DFHEIV1
      *    MOVE '."C                   ''   #00005822' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DLO034-COMMUNICATION-AREA, 
                 DLO034-REC-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00060
00061             IF DL34-RETURN-CODE = 'OK'
00062                 GO TO ELPRTCVP-EXIT
00063             ELSE
00064 *               MOVE '8339'     TO EMI-ERROR ?????ERROR MESSAGE???
00065                 GO TO ELPRTCVP-EXIT.
00066
00067      IF NOT FIRST-TIME
00068          GO TO ELPRTCVP-020.
00069
00070      IF WS-LINE-LEN NOT GREATER ZERO
00071          GO TO ELPRTCVP-EXIT.
00072
00073      MOVE '2'                    TO WS-FIRST-TIME-SW.
00074      MOVE LOW-VALUES             TO WS-BUFFER-AREA.
00075
00076      SET BUFFER-INDEX TO +1
00077
00078      IF EIBTRMID IS EQUAL TO 'AFLP'
00079          NEXT SENTENCE
00080      ELSE
00081          IF NOT TOP-PAGE
00082              MOVE T-TP           TO WS-BUFFER-BYTE (BUFFER-INDEX)
00083              SET BUFFER-INDEX UP BY +1.
00084
00085  ELPRTCVP-020.
00086      IF WS-PROG-END = SPACES
00087          GO TO ELPRTCVP-030.
00088
00089      MOVE SPACES                 TO WS-PROG-END.
00090
00091      IF BUFFER-INDEX GREATER +1
00092          PERFORM ELPRTCVP-PRINT-BUFFER THRU ELPRTCVP-PRINT-EXIT.
00093
00094      MOVE '1'                    TO WS-FIRST-TIME-SW.
00095
00096      GO TO ELPRTCVP-EXIT.
00097
00098  ELPRTCVP-030.
00099      IF WS-PASSED-DATA = SPACES
00100          SET PRT-INDEX TO +1
00101          GO TO ELPRTCVP-050.
00102
00103      SET PRT-INDEX TO WS-LINE-LEN.
00104
00105  ELPRTCVP-040.
00106      IF WS-PRINT-BYTE (PRT-INDEX) NOT = SPACES
00107          GO TO ELPRTCVP-050.
00108
00109      IF PRT-INDEX GREATER +1
00110          SET PRT-INDEX DOWN BY +1
00111          GO TO ELPRTCVP-040.
00112
00113  ELPRTCVP-050.
00114      SET WS-LINE-LENGTH TO PRT-INDEX.
00115      SET BUFFER-INDEX2 TO BUFFER-INDEX.
00116      SET BUFFER-INDEX2 UP BY WS-LINE-LENGTH.
00117
00118      IF BUFFER-INDEX2 NOT LESS WS-BUFFER-SIZE
00119          PERFORM ELPRTCVP-PRINT-BUFFER THRU ELPRTCVP-PRINT-EXIT.
00120
00121      IF TRIPLE-SPACE
00122           ADD +2  TO  WS-LINE-CNT
00123           MOVE T-SS           TO WS-BUFFER-BYTE (BUFFER-INDEX)
00124                                  WS-BUFFER-BYTE (BUFFER-INDEX + 1)
00125           SET BUFFER-INDEX UP BY +2.
00126
00127      IF DOUBLE-SPACE
00128           ADD +1  TO  WS-LINE-CNT
00129           MOVE T-SS             TO WS-BUFFER-BYTE (BUFFER-INDEX)
00130           SET BUFFER-INDEX UP BY +1.
00131
00132      ADD +1 TO WS-LINE-CNT
00133 ************************************************************
00134 *     BYPASS NEW LINE SYMBOL                               *
00135 *        IF FIRST BUFFER SENT AND TOP-OF-FORM SET.         *
00136 *     OR IF FIRST LINE OF SUBSEQUENT BUFFERS.              *
00137 ************************************************************
00138
00139      IF (BUFFER-INDEX GREATER +1 AND
00140          WS-BUFFER-BYTE (BUFFER-INDEX - 1) = T-TP)  OR
00141          FIRST-LINE-NEXT-BUFFER
00142          MOVE ZERO               TO WS-FIRST-TIME-SW
00143      ELSE
00144          MOVE T-SS               TO WS-BUFFER-BYTE (BUFFER-INDEX)
00145          SET BUFFER-INDEX UP BY +1.
00146
00147 **   NOTE, SINGLE SPACE IS REQUIRED BEFORE TOP PAGE CHAR
00148
00149      IF TOP-PAGE
00150          MOVE +1                TO WS-LINE-CNT
00151          MOVE T-TP              TO WS-BUFFER-BYTE (BUFFER-INDEX)
00152          SET BUFFER-INDEX UP BY +1.
00153
00154      SET PRT-INDEX TO +1.
00155
00156  ELPRTCVP-060.
00157      MOVE WS-PRINT-BYTE (PRT-INDEX)
00158                                  TO WS-BUFFER-BYTE (BUFFER-INDEX).
00159      SET BUFFER-INDEX UP BY +1.
00160
00161      IF PRT-INDEX LESS WS-LINE-LENGTH
00162          SET PRT-INDEX UP BY +1
00163          GO TO ELPRTCVP-060.
00164
00165  ELPRTCVP-EXIT.
00166      EXIT.
00167
00168  ELPRTCVP-PRINT-BUFFER.
00169      IF WS-BUFFER-BYTE (BUFFER-INDEX - 1) = T-SS
00170         MOVE SPACE               TO WS-BUFFER-BYTE (BUFFER-INDEX)
00171         SET BUFFER-INDEX UP BY 1.
00172
00173      MOVE  T-EM                  TO  WS-BUFFER-BYTE (BUFFER-INDEX)
00174      SET WS-BUFFER-LENGTH TO BUFFER-INDEX.
00175
00176      
      * EXEC CICS SEND
00177 *        FROM    (WS-BUFFER-AREA)
00178 *        LENGTH  (WS-BUFFER-LENGTH)
00179 *        CTLCHAR (WS-WCC-CNTL)
00180 *        ERASE
00181 *    END-EXEC.
      *    MOVE '$$    C E         L F ,   #00005943' TO DFHEIV0
           MOVE X'242420202020432045202020' TO DFHEIV0(1:12)
           MOVE X'2020202020204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035393433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-BUFFER-AREA, 
                 WS-BUFFER-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 WS-WCC-CNTL, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00182
00183      SET BUFFER-INDEX TO +1.
00184      MOVE '2'                    TO WS-FIRST-TIME-SW.
00185
00186  ELPRTCVP-PRINT-EXIT.
00187      EXIT.
00188
01934
01935

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1832' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 200-END-DATA,
                     300-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 5099-EXIT,
                     8860-ARCH2-NOT-OPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 5050-END-BR,
                     5050-END-BR,
                     8860-ARCH2-NOT-OPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 6899-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 8870-ACTV-NOT-OPEN,
                     7299-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 8880-CLAM-NOT-OPEN,
                     7205-CLAIM-LOOP
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 7206-CLAIM-REWRITE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 9000-CERT-NOT-OPEN,
                     7205-CLAIM-LOOP
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 9100-ACCOUNT-NOT-OPEN,
                     7215-END-ACCT,
                     7215-END-ACCT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 7240-END-BR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 12
               GO TO 7255-CHECK-FORM
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 13
               GO TO 7399-EXIT,
                     7399-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 14
               GO TO 8870-ACTV-NOT-OPEN,
                     8050-REC-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 15
               GO TO 8150-NOT-FOUND
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1832' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
