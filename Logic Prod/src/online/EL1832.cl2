00001  IDENTIFICATION DIVISION.                                         06/26/96
00002                                                                   EL1832
00003  PROGRAM-ID.                 EL1832.                                 LV019
00004 *              PROGRAM CONVERTED BY                                  CL*17
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL*17
00006 *              CONVERSION DATE 02/12/96 09:33:34.                    CL*17
00007 *                            VMOD=2.019.                             CL*19
00008 *                                                                 EL1832
00009 *AUTHOR.           LOGIC,INC.                                        CL*17
00010 *                  DALLAS,TEXAS.                                     CL*17
00011                                                                   EL1832
00012 *DATE-COMPILED.                                                      CL*17
00013                                                                   EL1832
00014 *SECURITY.   *****************************************************   CL*17
00015 *            *                                                   *   CL*17
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL*17
00017 *            *                                                   *   CL*17
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL*17
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL*17
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL*17
00021 *            *                                                   *   CL*17
00022 *            *****************************************************   CL*17
00023                                                                   EL1832
00024 *REMARKS. TRANSACTION EX63 - FORMS PRINTER.                          CL**3
00025 *        THIS PROGRAM IS USED TO PRINT INITIAL OR PROGRESS FORMS     CL**3
00026 *        DEPENDING ON THE VALUE OF THE PI-ENTRY-CODES AND FORM       CL**3
00027 *        TYPE PASSED.                                                CL**3
00028                                                                   EL1832
00029 *        PRINT INITIAL FORMS     CODE-1 = 1                          CL**3
00030 *                                CODE-2 = 1                          CL**3
00031                                                                   EL1832
00032 *        PRINT FOLLOW-UP FORMS   CODE-1 = 1                          CL**3
00033 *                                CODE-2 = 2                          CL**3
00034                                                                   EL1832
00035 *        RE-PRINT FORMS          CODE-1 = 0                          CL**3
00036 *                                CODE-2 = 3                          CL**3
00037                                                                   EL1832
00038 *        PRINT ALIGNMENT FORMS   CODE-1 = 0                          CL**3
00039 *                                CODE-2 = 2                          CL**3
00040      EJECT                                                        EL1832
00041  ENVIRONMENT DIVISION.                                            EL1832
00042  DATA DIVISION.                                                   EL1832
00043  WORKING-STORAGE SECTION.                                         EL1832
00044  77  FILLER  PIC X(32)  VALUE '********************************'. EL1832
00045  77  FILLER  PIC X(32)  VALUE '*   EL1832 WORKING STORAGE     *'. EL1832
00046  77  FILLER  PIC X(32)  VALUE '******** VMOD=2.019 ************'.    CL*19
00047                                                                   EL1832
00048  01  WS-CONSTANTS.                                                EL1832
00049      12  THIS-PGM                    PIC X(8)    VALUE 'EL1832'.     CL*19
00050      12  PGM-NAME                    PIC X(8).                    EL1832
00051      12  LINK-ELDATCV                PIC X(8)    VALUE 'ELDATCV'. EL1832
00052      12  ARCH-ID                     PIC X(8)    VALUE 'ELARCH'.  EL1832
00053      12  ARCH-ID2                    PIC X(8)    VALUE 'ELARCH2'. EL1832
00054      12  ACTV-ID                     PIC X(8)    VALUE 'ELTRLR'.  EL1832
00055      12  CLAM-ID                     PIC X(8)    VALUE 'ELMSTR'.  EL1832
00056      12  CERT-ID                     PIC X(8)    VALUE 'ELCERT'.  EL1832
00057      12  ACCT-ID                     PIC X(8)    VALUE 'ERACCT'.  EL1832
00058      12  ARCH-KEY.                                                EL1832
00059          16  ARCH-PARTIAL-KEY.                                    EL1832
00060              20  ARCH-CO             PIC X.                       EL1832
00061              20  ARCH-NUMBER         PIC S9(8)   COMP.            EL1832
00062              20  ARCH-REC-TYPE       PIC X.                       EL1832
00063          16  ARCH-SEQ                PIC S9(4)   VALUE +0  COMP.  EL1832
00064                                                                      CL**4
00065      12  ARCH-KEY2.                                               EL1832
00066          16  ARCH-PARTIAL-KEY2.                                   EL1832
00067              20  ARCH-CO2            PIC X.                       EL1832
00068              20  ARCH-REC-TYPE2      PIC X.                       EL1832
00069          16  ARCH-NUMBER2            PIC S9(8)   COMP.            EL1832
00070          16  ARCH-SEQ2               PIC S9(4)   VALUE +0  COMP.  EL1832
00071                                                                      CL**4
00072      12  ACTV-KEY.                                                EL1832
00073        14  ACTV-PARTIAL-KEY.                                      EL1832
00074          16  ACTV-CO                 PIC X.                       EL1832
00075          16  ACTV-CARRIER            PIC X.                       EL1832
00076          16  ACTV-CLAIM              PIC X(7).                    EL1832
00077          16  ACTV-CERT-NO            PIC X(11).                   EL1832
00078        14  ACTV-SEQ                  PIC S9(4)   COMP.            EL1832
00079                                                                      CL**4
00080      12  ELCNTL-KEY.                                              EL1832
00081          16  CNTL-COMP-ID            PIC X(3).                    EL1832
00082          16  CNTL-REC-TYPE           PIC X.                       EL1832
00083          16  CNTL-ACCESS             PIC X(4).                    EL1832
00084          16  CNTL-CARR-ACCESS REDEFINES CNTL-ACCESS.                 CL**4
00085              20  CNTL-CARR           PIC X.                          CL**4
00086              20  FILLER              PIC X(3).                       CL**4
00087          16  CNTL-SEQ-NO             PIC S9(4)    COMP.           EL1832
00088                                                                      CL**4
00089      12  CLAM-KEY.                                                EL1832
00090          16  CLAM-CO                 PIC X.                       EL1832
00091          16  CLAM-CARRIER            PIC X.                       EL1832
00092          16  CLAM-CLAIM              PIC X(7).                    EL1832
00093          16  CLAM-CERT               PIC X(11).                   EL1832
00094                                                                      CL**4
00095      12  CERT-KEY.                                                EL1832
00096          16  CERT-CO                 PIC X.                       EL1832
00097          16  CERT-PRIME              PIC X(21).                   EL1832
00098          16  CERT-CERT               PIC X(11).                   EL1832
00099                                                                      CL**4
00100      12  ACCT-KEY.                                                EL1832
00101          16  ACCT-PARTIAL-KEY.                                    EL1832
00102              20  ACCT-CO             PIC X.                       EL1832
00103              20  ACCT-CARRIER        PIC X.                       EL1832
00104              20  ACCT-GROUPING       PIC X(6).                    EL1832
00105              20  ACCT-STATE          PIC XX.                      EL1832
00106              20  ACCT-ACCOUNT        PIC X(10).                   EL1832
00107          16  ACCT-EXP-DATE           PIC XX.                      EL1832
00108          16  FILLER                  PIC X(04).                      CL**3
00109                                                                      CL**3
00110      12  ACCT-SAVE-KEY               PIC X(20).                   EL1832
00111                                                                      CL**4
00112      12  WS-INSURED-ACTV-KEY.                                        CL**4
00113          16  WS-INS-COMP-CD-KEY      PIC X(01).                      CL**4
00114          16  WS-INS-CARR-KEY         PIC X(01).                      CL**4
00115          16  WS-INS-CLAIM-KEY        PIC X(07).                      CL**4
00116          16  WS-INS-CERT-KEY         PIC X(11).                      CL**4
00117          16  WS-INS-SEQ-KEY          PIC S9(4)    COMP.              CL**4
00118                                                                   EL1832
00119      12  WS-ACTV-KEY                 PIC X(20)    VALUE SPACES.      CL*18
00120      12  WS-PRINT-SW                 PIC X        VALUE SPACES.      CL*18
00121      12  SAVE-DATE                   PIC X(8)     VALUE SPACES.   EL1832
00122      12  CURRENT-SAVE                PIC XX   VALUE LOW-VALUES.      CL*18
00123      12  CURRENT-4                   PIC XX   VALUE LOW-VALUES.      CL*18
00124      12  CURRENT-10                  PIC XX   VALUE LOW-VALUES.      CL*18
00125      12  CURRENT-30                  PIC XX   VALUE LOW-VALUES.      CL*18
00126      12  ERROR-LINE                  PIC X(80)    VALUE SPACES.      CL*18
00127      12  HEADER-BROWSE-STARTED       PIC X        VALUE 'N'.      EL1832
00128      12  HEADER-SW                   PIC X        VALUE SPACE.    EL1832
00129          88  HEADER-REC-FOUND                     VALUE SPACE.    EL1832
00130                                                                   EL1832
00131      12  FORM-CONTROL-SW             PIC X        VALUE SPACE.    EL1832
00132          88  FORM-REC-FOUND                       VALUE SPACE.    EL1832
00133                                                                   EL1832
00134      12  OPTION-CODES                PIC XX       VALUE SPACES.      CL*18
00135          88  PRINT-NEW-FORMS                      VALUE '11'.     EL1832
00136          88  PRINT-FOLLOW-UP                      VALUE '12'.     EL1832
00137          88  PRINT-ALIGNMENT                      VALUE ' 2'.     EL1832
00138          88  REPRINT-FORMS                        VALUE ' 3'.     EL1832
00139                                                                   EL1832
00140      12  SUB                         PIC 9        VALUE 0.        EL1832
00141      12  SUB-1                       PIC 9        VALUE 0.        EL1832
00142      12  PRT                         PIC 9        VALUE 1.        EL1832
00143                                                                   EL1832
00144      12  WS-LABEL-HOLD-AREA.                                      EL1832
00145            18  FILLER                PIC X(20)    VALUE SPACES.      CL*18
00146            18  WS-ZIP-LABEL          PIC X(10)    VALUE SPACES.      CL*18
00147                                                                   EL1832
00148      12  GETMAIN-SPACE               PIC X        VALUE SPACE.    EL1832
00149      12  TRANS-DATA-LENGTH           PIC S9(4)    VALUE +133 COMP.EL1832
00150      12  MSG-MESSAGE.                                             EL1832
00151          16  FILLER   PIC X(25) VALUE 'FORM WILL BE DELETED FOR '.EL1832
00152          16  MSG-CARRIER             PIC X.                       EL1832
00153          16  FILLER                  PIC X        VALUE SPACE.    EL1832
00154          16  MSG-CLAIM               PIC X(7).                    EL1832
00155          16  FILLER                  PIC X        VALUE SPACE.    EL1832
00156          16  MSG-CERT                PIC X(11).                   EL1832
00157          16  FILLER                  PIC X(10)    VALUE ALL '*'.  EL1832
00158          16  MSG-ARCH-NO             PIC Z(10).                   EL1832
00159          16  FILLER                  PIC X(5)     VALUE SPACES.   EL1832
00160          16  MSG-COUNT               PIC X(5)     VALUE SPACES.   EL1832
00161          16  FILLER                  PIC X(5)     VALUE SPACES.   EL1832
00162          16  MSG-KIND                PIC X(6).                    EL1832
00163          16  FILLER                  PIC X(5)     VALUE SPACES.   EL1832
00164          16  MSG-TYPE                PIC X.                       EL1832
00165          16  FILLER                  PIC X(39)    VALUE SPACES.   EL1832
00166                                                                      CL**4
00167      12  WS-ZIP-WORK.                                                CL**8
00168          16  WS-ZIP-PRIME            PIC X(05).                      CL**8
00169          16  WS-ZIP-DASH             PIC X.                          CL**8
00170          16  WS-ZIP-PLUS4            PIC X(04).                      CL**8
00171      12  WS-CANADIAN-ZIP-WORK  REDEFINES  WS-ZIP-WORK.               CL**8
00172          16  WS-CAN-POSTAL-1         PIC XXX.                        CL**8
00173          16  FILLER                  PIC X.                          CL**8
00174          16  WS-CAN-POSTAL-2         PIC XXX.                        CL**8
00175          16  FILLER                  PIC XXX.                        CL**8
00176      12  WS-WORK-ACCOUNT.                                            CL**3
00177          16  FILLER                  PIC X(04)    VALUE SPACES.      CL**3
00178          16  WS-WRK-ACCT             PIC X(06)    VALUE SPACES.      CL**3
00179      12  WS-WORK-CERT-NO.                                            CL**3
00180          16  FILLER                  PIC X(03)    VALUE SPACES.      CL**3
00181          16  WS-WRK-CERT             PIC X(08)    VALUE SPACES.      CL**3
00182                                                                   EL1832
00183      EJECT                                                        EL1832
00184  01  WS-PRINT-WORK-AREAS.                                         EL1832
00185      12  WS-CLAIM-TABLE OCCURS 3 TIMES.                           EL1832
00186          16  WS-CARRIER              PIC X.                       EL1832
00187          16  WS-CLAIM                PIC X(7).                    EL1832
00188          16  WS-CERT                 PIC X(11).                   EL1832
00189                                                                   EL1832
00190      12  CERT-READ-SW                PIC X.                       EL1832
00191          88  CERT-READ                            VALUE 'Y'.         CL**4
00192      12  ACCT-BROWSE-SW              PIC X.                       EL1832
00193          88  ACCT-BROWSE-STARTED                  VALUE 'Y'.         CL**4
00194          88  ACCT-FOUND                           VALUE 'F'.         CL**4
00195      12  WS-NAME-WORK                PIC X(30).                   EL1832
00196      12  WS-ADDR1-WORK               PIC X(30).                   EL1832
00197      12  WS-ADDR2-WORK               PIC X(30).                   EL1832
00198      12  WS-CITY-STATE-WORK          PIC X(30).                   EL1832
00199      12  WS-SAVE-ACCT                PIC X(10).                   EL1832
00200      12  WS-SAVE-MEMBER              PIC X(12).                   EL1832
00201      12  WS-SAVE-SS                  PIC X(11).                   EL1832
00202      12  WS-SAVE-LOAN-NO             PIC X(8).                    EL1832
00203      12  WS-SAVE-CLAIM               PIC X(8).                    EL1832
00204      12  WS-SAVE-ACCT-NAME           PIC X(30).                   EL1832
00205      12  WS-SPEC-INST1               PIC X(28).                   EL1832
00206      12  WS-SPEC-INST2               PIC X(28).                   EL1832
00207      12  WS-SPEC-INST3               PIC X(28).                   EL1832
00208      12  WS-ADDR-TYPE                PIC X.                       EL1832
00209      12  WS-ADDR-SEQ                 PIC S9(4)    COMP.           EL1832
00210      12  WS-PAY-FREQ                 PIC XX.                      EL1832
00211      12  WS-TOTAL-PAID               PIC 9(5)V99  VALUE ZEROS.    EL1832
00212      12  WS-PAID-THRU-DT             PIC XX.                      EL1832
00213      12  WS-INCURRED-DT              PIC XX.                      EL1832
00214      12  WS-CERT-EFF-DT              PIC XX.                      EL1832
00215      12  WS-GREG-PAID-THRU           PIC X(8).                    EL1832
00216      12  WS-GREG-INCURRED            PIC X(8).                    EL1832
00217      12  WS-BEN-DAYS-HOLD            PIC 99       VALUE ZEROS.       CL*18
00218      12  WS-BENEFIT-SAVE.                                         EL1832
00219          16  WS-BEN-DAYS             PIC 99       VALUE ZEROS.       CL*18
00220          16  WS-BEN-TYPE             PIC X        VALUE SPACES.      CL*18
00221      12  WS-ACCESS.                                               EL1832
00222          16  FILLER                  PIC XX       VALUE SPACES.   EL1832
00223          16  WS-BEN-CD               PIC XX       VALUE SPACES.      CL*18
00224      12  WS-INSURED-NAME.                                         EL1832
00225          16  WS-INSURED-FNAME        PIC X(10).                   EL1832
00226          16  WS-INSURED-LNAME        PIC X(15).                   EL1832
00227      12  WS-CERT-NO                  PIC X(11).                   EL1832
00228      12  WS-EMPLOYER-SW              PIC X(01).                      CL**4
00229          88  EMPLOYER-FORM-REQUIRED VALUE 'Y'.                       CL**4
00230      12  WS-INSURED-MAIL-TO-NAME     PIC X(30).                      CL**4
00231      12  WS-INSURED-ADDR-LINE-1      PIC X(30).                      CL**4
00232      12  WS-INSURED-ADDR-LINE-2      PIC X(30).                      CL**4
00233      12  WS-INSURED-CITY-STATE       PIC X(30).                      CL**4
00234      12  WS-INSURED-ZIP.                                             CL**4
00235          16  WS-INSURED-ZIP-PRIME    PIC X(05).                      CL**4
00236          16  WS-INSURED-ZIP-PLUS4    PIC X(04).                      CL**4
00237      12  WS-INSURED-POST-CODE  REDEFINES  WS-INSURED-ZIP.            CL**8
00238          16  WS-INSURED-POSTAL-1     PIC XXX.                        CL**8
00239          16  WS-INSURED-POSTAL-2     PIC XXX.                        CL**8
00240          16  FILLER                  PIC XXX.                        CL**8
00241      12  WS-COMPANY-MAIL-TO-NAME     PIC X(30).                      CL**4
00242      12  WS-COMPANY-IN-CARE-OF       PIC X(30).                      CL**4
00243      12  WS-COMPANY-ADDR-LINE-1      PIC X(30).                      CL**4
00244      12  WS-COMPANY-ADDR-LINE-2      PIC X(30).                      CL**4
00245      12  WS-COMPANY-CITY-STATE       PIC X(30).                      CL**4
00246      12  WS-COMPANY-ZIP.                                             CL**4
00247          16  WS-COMPANY-ZIP-PRIME    PIC X(05).                      CL**4
00248          16  WS-COMPANY-ZIP-PLUS4    PIC X(04).                      CL**4
00249      12  WS-COMPANY-POST-CODE  REDEFINES  WS-COMPANY-ZIP.            CL**8
00250          16  WS-COMPANY-POSTAL-1     PIC XXX.                        CL**8
00251          16  WS-COMPANY-POSTAL-2     PIC XXX.                        CL**8
00252          16  FILLER                  PIC XXX.                        CL**8
00253      12  WS-LAST-PMT-AMT             PIC 9(7)V99.                    CL**4
00254                                                                   EL1832
00255      EJECT                                                        EL1832
00256  01  PROG-PRINT-LINE-1.                                           EL1832
00257      12  FILLER                      PIC X(59).                      CL**7
00258      12  PROG-PRINT-SW               PIC X(01).                      CL**7
00259      12  FILLER                      PIC X(01).                      CL**7
00260      12  PROG-PRINT-DATE             PIC X(08).                      CL**7
00261                                                                   EL1832
00262  01  PROG-PRINT-LINE-2.                                              CL**7
00263      12  FILLER                      PIC X(07).                      CL**7
00264      12  PROG-NAME-ADDR              PIC X(30).                   EL1832
00265      12  FILLER                      PIC X(12).                      CL**7
00266      12  PROG-SPEC-INST              PIC X(30).                   EL1832
00267                                                                   EL1832
00268  01  PROG-PRINT-LINE-3.                                              CL**7
00269      12  FILLER                      PIC X(04).                      CL**7
00270      12  PROG-ACCOUNT-NAME           PIC X(28).                   EL1832
00271      12  FILLER                      PIC X(08).                      CL**7
00272      12  PROG-CLAIM                  PIC X(07).                      CL**7
00273      12  FILLER                      PIC X(01).                      CL**7
00274      12  PROG-CARRIER                PIC X(01).                      CL**7
00275      12  FILLER                      PIC X(10).                      CL**7
00276      12  PROG-MEMBER-NO              PIC X(12).                   EL1832
00277      12  FILLER                      PIC X(06).                      CL**7
00278      12  PROG-ACCOUNT-NO             PIC X(06).                      CL**3
00279                                                                   EL1832
00280      EJECT                                                        EL1832
00281  01  INIT-PRINT-LINE-1.                                           EL1832
00282      12  FILLER                      PIC X(60).                      CL**7
00283      12  INIT-ELIM-DAYS              PIC Z9.                      EL1832
00284                                                                   EL1832
00285  01  INIT-PRINT-LINE-2.                                              CL**7
00286      12  FILLER                      PIC X(07).                      CL**7
00287      12  INIT-NAME-1                 PIC X(30).                   EL1832
00288      12  FILLER                      PIC X(19).                   EL1832
00289      12  INIT-ACCOUNT-NAME           PIC X(26).                   EL1832
00290                                                                   EL1832
00291  01  INIT-PRINT-LINE-3 REDEFINES INIT-PRINT-LINE-2.                  CL**7
00292      12  FILLER                      PIC X(70).                      CL**7
00293      12  INIT-CLAIM-ACCT             PIC X(10).                   EL1832
00294      12  FILLER                      PIC X(01).                      CL**7
00295      12  INIT-CLAIM-CARRIER          PIC X(01).                      CL**7
00296                                                                   EL1832
00297  01  INIT-PRINT-LINE-4-1.                                         EL1832
00298      12  FILLER                      PIC X(70).                      CL**7
00299      12  INIT-PRINT-SW               PIC X(01).                      CL**7
00300      12  FILLER                      PIC X(01).                      CL**7
00301      12  INIT-PRINT-DATE             PIC X(08).                      CL**7
00302                                                                   EL1832
00303      EJECT                                                        EL1832
00304  01  LBL-PRINT-LINE-1.                                               CL**9
00305      12  FILLER                      PIC X(07).                      CL**9
00306      12  LBL-INSURED-NAME            PIC X(30).                      CL**9
00307      12  FILLER                      PIC X(13).                      CL*10
00308      12  LBL-CLAIM-NO                PIC X(07).                      CL**9
00309      12  FILLER                      PIC X(26).                      CL**9
00310                                                                      CL**9
00311  01  LBL-PRINT-LINE-2.                                               CL**9
00312      12  FILLER                      PIC X(07).                      CL**9
00313      12  LBL-INSURED-ADDR1           PIC X(30).                      CL**9
00314      12  FILLER                      PIC X(46).                      CL**9
00315                                                                      CL**9
00316  01  LBL-PRINT-LINE-3.                                               CL**9
00317      12  FILLER                      PIC X(07).                      CL**9
00318      12  LBL-INSURED-ADDR2           PIC X(30).                      CL**9
00319      12  FILLER                      PIC X(46).                      CL*10
00320                                                                      CL**9
00321  01  LBL-PRINT-LINE-4.                                               CL**9
00322      12  FILLER                      PIC X(07).                      CL*10
00323      12  LBL-INSURED-CITY-ST         PIC X(35).                      CL*10
00324      12  FILLER                      PIC X(41).                      CL*10
00325                                                                      CL*10
00326  01  LBL-PRINT-LINE-5.                                               CL*10
00327      12  FILLER                      PIC X(42).                      CL**9
00328      12  LBL-PAID-THRU-DATE          PIC X(08).                      CL**9
00329      12  FILLER                      PIC X(33).                      CL**9
00330      EJECT                                                           CL**9
00331                                                                      CL**9
00332  01  RMC-LAP-PRINT-LINE-1.                                           CL*14
00333      12  FILLER                      PIC X(17).                      CL*11
00334      12  RMC-LAP-CLAIM-NO            PIC X(07).                      CL*14
00335      12  FILLER                      PIC X(12).                      CL*11
00336      12  RMC-LAP-POLICY-NO           PIC X(11).                      CL*14
00337      12  FILLER                      PIC X(39).                      CL*11
00338                                                                      CL*11
00339  01  RMC-LAP-PRINT-LINE-2.                                           CL*14
00340      12  FILLER                      PIC X(10).                      CL*11
00341      12  RMC-LAP-INSURED-NAME        PIC X(30).                      CL*14
00342      12  FILLER                      PIC X(44).                      CL*11
00343                                                                      CL*11
00344  01  RMC-LAP-PRINT-LINE-3.                                           CL*14
00345      12  FILLER                      PIC X(10).                      CL*11
00346      12  RMC-LAP-INSURED-ADDR1       PIC X(30).                      CL*14
00347      12  FILLER                      PIC X(44).                      CL*11
00348                                                                      CL*11
00349  01  RMC-LAP-PRINT-LINE-4.                                           CL*14
00350      12  FILLER                      PIC X(10).                      CL*11
00351      12  RMC-LAP-INSURED-ADDR2       PIC X(35).                      CL*14
00352      12  FILLER                      PIC X(40).                      CL*17
00353                                                                      CL*11
00354  01  RMC-LAP-PRINT-LINE-5.                                           CL*14
00355      12  FILLER                      PIC X(10).                      CL*11
00356      12  RMC-LAP-INSURED-CITY-ST     PIC X(35).                      CL*14
00357      12  FILLER                      PIC X(40).                      CL*11
00358                                                                      CL*14
00359  01  RMC-LAP-PRINT-LINE-6.                                           CL*14
00360      12  FILLER                      PIC X(03).                      CL*14
00361      12  RMC-LAP-TEXT-LINE-1         PIC X(28).                      CL*14
00362      12  FILLER                      PIC X(52).                      CL*14
00363                                                                      CL*14
00364  01  RMC-LAP-PRINT-LINE-7.                                           CL*14
00365      12  FILLER                      PIC X(03).                      CL*14
00366      12  RMC-LAP-TEXT-LINE-2         PIC X(28).                      CL*14
00367      12  FILLER                      PIC X(52).                      CL*14
00368                                                                      CL*14
00369  01  RMC-LAP-PRINT-LINE-8.                                           CL*14
00370      12  FILLER                      PIC X(03).                      CL*14
00371      12  RMC-LAP-TEXT-LINE-3         PIC X(28).                      CL*14
00372      12  FILLER                      PIC X(52).                      CL*14
00373                                                                      CL*11
00374                                      COPY ELCDMD34.                  CL*19
00375      EJECT                                                           CL*11
00376                                      COPY ELCINTF.                   CL**9
00377      12  PI-WA REDEFINES PI-PROGRAM-WORK-AREA.                    EL1832
00378          16  PI-PRINT-DATE           PIC X(8).                    EL1832
00379          16  PI-PRINT-DATE-BIN       PIC XX.                      EL1832
00380          16  PI-PRINT-ID             PIC X(4).                    EL1832
00381          16  PI-FORM-TYPE            PIC X.                       EL1832
00382          16  FILLER                  PIC X(625).                     CL*17
00383      EJECT                                                        EL1832
00384                                      COPY ELCJPFX.                   CL**9
00385                                      PIC X(200).                  EL1832
00386      EJECT                                                        EL1832
00387                                      COPY ELPRTCVD.                  CL**9
00388      EJECT                                                        EL1832
00389                                      COPY ELCDATE.                   CL**9
00390      EJECT                                                        EL1832
00391  LINKAGE SECTION.                                                 EL1832
00392                                      COPY ELCARCH.                   CL**9
00393      EJECT                                                        EL1832
00394                                      COPY ELCTRLR.                   CL**9
00395      EJECT                                                        EL1832
00396                                      COPY ELCCERT.                   CL**9
00397      EJECT                                                        EL1832
00398                                      COPY ELCMSTR.                   CL**9
00399      EJECT                                                        EL1832
00400                                      COPY ERCACCT.                   CL**9
00401      EJECT                                                           CL**9
00402                                      COPY ELCCNTL.                   CL**9
00403      EJECT                                                        EL1832
00404  PROCEDURE DIVISION.                                              EL1832
00405                                                                   EL1832
00406      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              EL1832
00407      MOVE '5'                    TO DC-OPTION-CODE.               EL1832
00408      PERFORM 9700-DATE-LINK  THRU  9700-EXIT.                     EL1832
00409      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.                   EL1832
00410      MOVE DC-BIN-DATE-1          TO  CURRENT-SAVE.                EL1832
00411      MOVE SPACES                 TO DL34-PROCESS-TYPE.               CL*19
CIDMOD*    MOVE 'N'                    TO CSO-PRINT-STARTED-SW.              000
00412                                                                   EL1832
00413      MOVE 85                     TO WS-LINE-LEN.                  EL1832
00414                                                                   EL1832
00415  0100-RETRIEVE-LOOP.                                              EL1832
00416      EXEC CICS HANDLE CONDITION                                   EL1832
00417           ENDDATA    (200-END-DATA)                               EL1832
00418           NOTFND     (300-NOT-FOUND)                              EL1832
00419           END-EXEC.                                               EL1832
00420                                                                      CL*19
00421      EXEC CICS RETRIEVE                                           EL1832
00422           INTO       (PROGRAM-INTERFACE-BLOCK)                    EL1832
00423           LENGTH     (PI-COMM-LENGTH)                             EL1832
00424           END-EXEC.                                               EL1832
00425                                                                      CL*19
00426 * DLO034 OPEN WHEN DMD OR CID                                        CL*19
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'                               CL*19
00428          IF DL34-PROCESS-TYPE IS EQUAL TO SPACES                     CL*19
00429              MOVE 'O'                TO DL34-PROCESS-TYPE            CL*19
00430              MOVE PI-COMPANY-ID      TO DL34-COMPANY-ID              CL*19
00431              MOVE THIS-PGM           TO DL34-PRINT-PROGRAM-ID        CL*19
00432              MOVE PI-PROCESSOR-ID    TO DL34-USERID                  CL*19
00433              MOVE SPACES             TO DL34-PRINT-LINE              CL*19
00434              MOVE PI-ALT-DMD-PRT-ID  TO DL34-OVERRIDE-PRINTER-ID     CL*19
00435              EXEC CICS LINK                                          CL*19
00436                  PROGRAM    ('DLO034')                               CL*19
00437                  COMMAREA   (DLO034-COMMUNICATION-AREA)              CL*19
00438                  LENGTH     (DLO034-REC-LENGTH)                      CL*19
00439              END-EXEC                                                CL*19
00440              IF DL34-RETURN-CODE NOT = 'OK'                          CL*19
00441                  MOVE  '**DLO034 OPEN ERROR - ABORT**'               CL*19
00442                                      TO ERROR-LINE                   CL*19
00443                  PERFORM 400-SEND-TEXT                               CL*19
00444                  EXEC CICS RETURN                                    CL*19
00445                  END-EXEC.                                           CL*19
00446                                                                   EL1832
CIDMOD                                                                       000
CIDMOD*    IF CSO-PRINT-STARTED-SW = 'Y'                                     000
CIDMOD*      MOVE 'L'  TO  DRS-SW                                            000
CIDMOD*      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                            000
CIDMOD*    MOVE 'F'  TO  DRS-SW.                                             000
CIDMOD*    PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                              000
CIDMOD*    MOVE ' '  TO  DRS-SW.                                             000
CIDMOD                                                                       EL1
00447      PERFORM 1000-INITIALIZE.                                     EL1832
00448                                                                   EL1832
00449      IF PRINT-ALIGNMENT                                           EL1832
00450         PERFORM 6300-ALIGN-ROUTINE THRU 6399-EXIT                 EL1832
00451      ELSE                                                         EL1832
00452         PERFORM 5000-BROWSE-ARCHIVE-HEADERS THRU 5099-EXIT.       EL1832
00453                                                                   EL1832
00454  200-END-DATA.                                                    EL1832
00455                                                                      CL*19
00456 * DLO034 CLOSE                                                       CL*19
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'                               CL*19
00458          MOVE 'C'                TO DL34-PROCESS-TYPE                CL*19
00459          MOVE PI-COMPANY-ID      TO DL34-COMPANY-ID                  CL*19
00460          MOVE THIS-PGM           TO DL34-PRINT-PROGRAM-ID            CL*19
00461          MOVE PI-PROCESSOR-ID    TO DL34-USERID                      CL*19
00462          MOVE SPACES             TO DL34-PRINT-LINE                  CL*19
00463                                     DL34-OVERRIDE-PRINTER-ID         CL*19
00464          EXEC CICS LINK                                              CL*19
00465              PROGRAM    ('DLO034')                                   CL*19
00466              COMMAREA   (DLO034-COMMUNICATION-AREA)                  CL*19
00467              LENGTH     (DLO034-REC-LENGTH)                          CL*19
00468          END-EXEC                                                    CL*19
00469          IF DL34-RETURN-CODE NOT = 'OK'                              CL*19
00470              MOVE  '**DLO034 CLOSE ERROR - ABORT**'                  CL*19
00471                                  TO ERROR-LINE                       CL*19
00472              PERFORM 400-SEND-TEXT.                                  CL*19
00473                                                                      CL*19
CIDMOD*    MOVE 'L' TO DRS-SW.                                               000
CIDMOD*    PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                              000
CIDMOD                                                                       000
00474      EXEC CICS RETURN                                             EL1832
00475           END-EXEC.                                               EL1832
00476                                                                   EL1832
00477  300-NOT-FOUND.                                                   EL1832
00478      MOVE 'NO COMMUNICATION AREA FOUND'  TO ERROR-LINE            EL1832
00479      PERFORM 400-SEND-TEXT                                        EL1832
00480      GO TO 200-END-DATA.                                          EL1832
00481                                                                   EL1832
00482  400-SEND-TEXT.                                                   EL1832
00483      EXEC CICS SEND TEXT                                          EL1832
00484           FROM    (ERROR-LINE)                                    EL1832
00485           LENGTH  (70)                                            EL1832
00486           END-EXEC.                                               EL1832
00487      EJECT                                                        EL1832
00488  1000-INITIALIZE.                                                 EL1832
00489      MOVE CURRENT-SAVE           TO DC-BIN-DATE-1.                EL1832
00490      MOVE -4                     TO DC-ELAPSED-DAYS.              EL1832
00491      MOVE ZEROS                  TO DC-ELAPSED-MONTHS.            EL1832
00492      MOVE '6'                    TO DC-OPTION-CODE.               EL1832
00493      PERFORM 9700-DATE-LINK THRU 9700-EXIT.                       EL1832
00494      MOVE DC-BIN-DATE-2          TO CURRENT-4.                    EL1832
00495                                                                   EL1832
00496      MOVE CURRENT-SAVE           TO DC-BIN-DATE-1.                EL1832
00497      MOVE -10                    TO DC-ELAPSED-DAYS.              EL1832
00498      MOVE ZEROS                  TO DC-ELAPSED-MONTHS.            EL1832
00499      MOVE '6'                    TO DC-OPTION-CODE.               EL1832
00500      PERFORM 9700-DATE-LINK THRU 9700-EXIT.                       EL1832
00501      MOVE DC-BIN-DATE-2          TO CURRENT-10.                   EL1832
00502                                                                   EL1832
00503      MOVE CURRENT-SAVE           TO DC-BIN-DATE-1.                EL1832
00504      MOVE -30                    TO DC-ELAPSED-DAYS.              EL1832
00505      MOVE ZEROS                  TO DC-ELAPSED-MONTHS.            EL1832
00506      MOVE '6'                    TO DC-OPTION-CODE.               EL1832
00507      PERFORM 9700-DATE-LINK THRU 9700-EXIT.                       EL1832
00508      MOVE DC-BIN-DATE-2          TO CURRENT-30.                   EL1832
00509                                                                   EL1832
00510      MOVE PI-COMPANY-CD          TO ARCH-CO2                      EL1832
00511                                     ACTV-CO                       EL1832
00512                                     ARCH-CO.                      EL1832
00513                                                                   EL1832
00514      MOVE PI-ENTRY-CODES         TO OPTION-CODES.                 EL1832
00515                                                                   EL1832
00516      EXEC CICS GETMAIN                                            EL1832
00517          SET     (ADDRESS OF LETTER-ARCHIVE)                         CL*17
00518          LENGTH  (90)                                             EL1832
00519          INITIMG (GETMAIN-SPACE)                                  EL1832
00520          END-EXEC.                                                EL1832
00521                                                                   EL1832
00522      EJECT                                                        EL1832
00523  5000-BROWSE-ARCHIVE-HEADERS.                                     EL1832
00524      MOVE '4'                    TO ARCH-REC-TYPE2.               EL1832
00525      MOVE ZEROS                  TO ARCH-NUMBER2.                 EL1832
00526      MOVE ZEROS                  TO ARCH-SEQ2.                    EL1832
00527                                                                   EL1832
00528  5005-SET-HANDLE.                                                 EL1832
00529      EXEC CICS HANDLE CONDITION                                   EL1832
00530           NOTFND    (5099-EXIT)                                   EL1832
00531           NOTOPEN   (8860-ARCH2-NOT-OPEN)                         EL1832
00532           END-EXEC.                                               EL1832
00533                                                                   EL1832
00534      EXEC CICS STARTBR                                            EL1832
00535           DATASET   (ARCH-ID2)                                    EL1832
00536           RIDFLD    (ARCH-KEY2)                                   EL1832
00537           END-EXEC.                                               EL1832
00538                                                                   EL1832
00539      MOVE 'Y'                    TO HEADER-BROWSE-STARTED.        EL1832
00540                                                                   EL1832
00541  5010-READ-NEXT.                                                  EL1832
00542      EXEC CICS HANDLE CONDITION                                   EL1832
00543           NOTFND   (5050-END-BR)                                  EL1832
00544           ENDFILE  (5050-END-BR)                                  EL1832
00545           NOTOPEN  (8860-ARCH2-NOT-OPEN)                          EL1832
00546           END-EXEC.                                               EL1832
00547                                                                   EL1832
00548      EXEC CICS READNEXT                                           EL1832
00549           DATASET   (ARCH-ID2)                                    EL1832
00550           RIDFLD    (ARCH-KEY2)                                   EL1832
00551           INTO      (LETTER-ARCHIVE)                              EL1832
00552           END-EXEC.                                               EL1832
00553                                                                   EL1832
00554      IF (PI-COMPANY-CD  NOT = ARCH-CO2)  OR                          CL*18
00555         (ARCH-REC-TYPE2 NOT = '4')                                   CL*18
00556           GO TO 5050-END-BR.                                      EL1832
00557                                                                   EL1832
00558      IF (PI-FORM-TYPE = 'I' AND NOT LA4-INITIAL-FORM) OR          EL1832
00559         (PI-FORM-TYPE = 'P' AND NOT LA4-PROGRESS-FORM)            EL1832
00560         GO TO 5010-READ-NEXT.                                     EL1832
00561                                                                   EL1832
00562      PERFORM 5050-END-BR.                                         EL1832
00563                                                                   EL1832
00564      IF PRINT-NEW-FORMS                                           EL1832
00565         MOVE 'I'                 TO WS-PRINT-SW                   EL1832
00566         PERFORM 6000-NEW-FORMS THRU 6099-EXIT                     EL1832
00567         GO TO 5020-CHECK-DATES.                                   EL1832
00568                                                                   EL1832
00569      IF PRINT-FOLLOW-UP                                           EL1832
00570         MOVE 'F'                 TO WS-PRINT-SW                   EL1832
00571         PERFORM 6100-FOLLOW-UP-FORMS THRU 6199-EXIT               EL1832
00572         GO TO 5020-CHECK-DATES.                                   EL1832
00573                                                                   EL1832
00574      IF REPRINT-FORMS                                             EL1832
00575         MOVE 'R'                 TO WS-PRINT-SW                   EL1832
00576         PERFORM 6200-REPRINT-FORMS THRU 6299-EXIT.                EL1832
00577                                                                   EL1832
00578  5020-CHECK-DATES.                                                EL1832
00579 *    IF LA4-INITIAL-PRINT-DATE = LOW-VALUES                       EL1832
00580 *       IF LA4-CREATION-DT LESS THAN CURRENT-30                   EL1832
00581 *          PERFORM 8200-DELETE-ARCHIVE THRU 8299-EXIT             EL1832
00582 *          GO TO 5010-READ-NEXT                                   EL1832
00583 *          ELSE                                                   EL1832
00584 *          GO TO 5010-READ-NEXT.                                  EL1832
00585                                                                      CL**6
00586      IF LA4-INITIAL-PRINT-DATE = LOW-VALUES                          CL**6
00587          ADD +1                  TO  ARCH-SEQ2                       CL**6
00588            GO TO 5005-SET-HANDLE.                                    CL**6
00589                                                                   EL1832
00590      IF LA4-RESEND-DATE NOT = LOW-VALUES                          EL1832
00591         IF LA4-RESEND-PRINT-DATE = LOW-VALUES                     EL1832
00592            PERFORM 5030-CHECK-OLD-DATES THRU 5030-EXIT            EL1832
00593           ELSE                                                    EL1832
00594            IF LA4-RESEND-PRINT-DATE LESS THAN CURRENT-4           EL1832
00595               MOVE '2'      TO MSG-KIND                           EL1832
00596               PERFORM 8200-DELETE-ARCHIVE THRU 8299-EXIT          EL1832
00597              ELSE                                                 EL1832
00598               NEXT SENTENCE                                       EL1832
00599        ELSE                                                       EL1832
00600        IF LA4-INITIAL-PRINT-DATE LESS THAN CURRENT-4 AND          EL1832
00601           LA4-RESEND-PRINT-DATE LESS THAN CURRENT-4               EL1832
00602               MOVE '3'      TO MSG-KIND                           EL1832
00603            PERFORM 8200-DELETE-ARCHIVE THRU 8299-EXIT.            EL1832
00604                                                                   EL1832
00605      ADD +1                      TO  ARCH-SEQ2.                   EL1832
00606                                                                   EL1832
00607      GO TO 5005-SET-HANDLE.                                       EL1832
00608      EJECT                                                        EL1832
00609  5030-CHECK-OLD-DATES.                                            EL1832
00610      IF LA4-RESEND-DATE LESS THAN CURRENT-10                      EL1832
00611               MOVE '4'      TO MSG-KIND                           EL1832
00612         PERFORM 8200-DELETE-ARCHIVE THRU 8299-EXIT.               EL1832
00613                                                                   EL1832
00614  5030-EXIT.                                                       EL1832
00615       EXIT.                                                       EL1832
00616                                                                   EL1832
00617  5050-END-BR.                                                     EL1832
00618      IF HEADER-BROWSE-STARTED = 'Y'                               EL1832
00619         MOVE 'N'                 TO HEADER-BROWSE-STARTED         EL1832
00620         EXEC CICS ENDBR                                           EL1832
00621              DATASET  (ARCH-ID2)                                  EL1832
00622              END-EXEC.                                            EL1832
00623  5099-EXIT.                                                       EL1832
00624       EXIT.                                                       EL1832
00625                                                                   EL1832
00626      EJECT                                                        EL1832
00627  6000-NEW-FORMS.                                                  EL1832
00628      IF LA4-INITIAL-PRINT-DATE NOT = LOW-VALUES                   EL1832
00629         GO TO 6099-EXIT.                                          EL1832
00630                                                                   EL1832
00631      IF PI-PRINT-DATE-BIN GREATER THAN LOW-VALUES                    CL**5
00632          IF LA4-CREATION-DT NOT = PI-PRINT-DATE-BIN                  CL**5
00633              GO TO 6099-EXIT.                                        CL**5
00634                                                                      CL**5
00635      PERFORM 8100-READ-HEADER THRU 8199-EXIT.                     EL1832
00636                                                                   EL1832
00637      MOVE SPACE                  TO FORM-CONTROL-SW.              EL1832
00638                                                                   EL1832
00639      PERFORM 8000-READ-FORM-CONTROL THRU 8099-EXIT.               EL1832
00640                                                                   EL1832
00641      IF (FORM-REC-FOUND AND                                       EL1832
00642         AT-FORM-SEND-ON-DT GREATER THAN CURRENT-SAVE) OR          EL1832
00643         NOT FORM-REC-FOUND                                        EL1832
00644         EXEC CICS UNLOCK                                          EL1832
00645              DATASET  (ARCH-ID)                                   EL1832
00646              END-EXEC                                             EL1832
00647         IF FORM-REC-FOUND                                         EL1832
00648            PERFORM 6900-UNLOCK-FORM THRU 6999-EXIT                EL1832
00649            GO TO 6099-EXIT                                        EL1832
00650         ELSE                                                      EL1832
00651            GO TO 6099-EXIT.                                       EL1832
00652                                                                   EL1832
00653      MOVE CURRENT-SAVE           TO LA4-INITIAL-PRINT-DATE.       EL1832
00654                                                                      CL*16
00655      EXEC CICS REWRITE                                            EL1832
00656           DATASET  (ARCH-ID)                                      EL1832
00657           FROM     (LETTER-ARCHIVE)                               EL1832
00658           END-EXEC.                                               EL1832
00659                                                                   EL1832
00660      MOVE CURRENT-SAVE           TO AT-FORM-PRINTED-DT.           EL1832
00661                                                                   EL1832
00662      EXEC CICS REWRITE                                            EL1832
00663           DATASET  (ACTV-ID)                                      EL1832
00664           FROM     (ACTIVITY-TRAILERS)                            EL1832
00665           END-EXEC.                                               EL1832
00666                                                                   EL1832
00667      PERFORM 7200-PRINT-FORMS-RECORDS THRU 7299-EXIT.             EL1832
00668                                                                   EL1832
00669  6099-EXIT.                                                       EL1832
00670       EXIT.                                                       EL1832
00671                                                                   EL1832
00672      EJECT                                                        EL1832
00673  6100-FOLLOW-UP-FORMS.                                            EL1832
00674      IF LA4-RESEND-DATE = LOW-VALUES                              EL1832
00675         GO TO 6199-EXIT.                                          EL1832
00676                                                                   EL1832
00677      IF LA4-RESEND-DATE NOT GREATER THAN CURRENT-SAVE AND         EL1832
00678         LA4-RESEND-PRINT-DATE = LOW-VALUES                        EL1832
00679         NEXT SENTENCE                                             EL1832
00680      ELSE                                                         EL1832
00681         GO TO 6199-EXIT.                                          EL1832
00682                                                                      CL**5
00683      IF PI-PRINT-DATE-BIN GREATER THAN LOW-VALUES                    CL**5
00684          IF LA4-RESEND-DATE NOT = PI-PRINT-DATE-BIN                  CL**5
00685              GO TO 6199-EXIT.                                        CL**5
00686                                                                   EL1832
00687      MOVE SPACES                 TO FORM-CONTROL-SW               EL1832
00688                                     HEADER-SW.                    EL1832
00689                                                                   EL1832
00690      PERFORM 8100-READ-HEADER THRU 8199-EXIT.                     EL1832
00691                                                                   EL1832
00692      IF NOT HEADER-REC-FOUND                                      EL1832
00693         GO TO 6199-EXIT.                                          EL1832
00694                                                                   EL1832
00695      PERFORM 8000-READ-FORM-CONTROL THRU 8099-EXIT.               EL1832
00696                                                                   EL1832
00697      IF NOT FORM-REC-FOUND                                        EL1832
00698         EXEC CICS UNLOCK                                          EL1832
00699              DATASET (ARCH-ID)                                    EL1832
00700              END-EXEC                                             EL1832
00701         GO TO 6199-EXIT.                                          EL1832
00702                                                                   EL1832
00703      IF AT-FORM-ANSWERED-DT = LOW-VALUES AND                      EL1832
00704         AT-FORM-RE-SEND-DT  = LA4-RESEND-DATE                     EL1832
00705         PERFORM 6500-UPDATE-RESEND-PRINT THRU 6599-EXIT           EL1832
00706         PERFORM 6800-REWRITE-HEADER      THRU 6899-EXIT           EL1832
00707         PERFORM 6600-UPDATE-FORM-TRLR    THRU 6699-EXIT           EL1832
00708         PERFORM 7200-PRINT-FORMS-RECORDS THRU 7299-EXIT           EL1832
00709         GO TO 6199-EXIT.                                          EL1832
00710                                                                   EL1832
00711      IF AT-FORM-ANSWERED-DT NOT = LOW-VALUES                      EL1832
00712         PERFORM 6700-ZERO-RESEND-IN-HEADER THRU 6799-EXIT         EL1832
00713         PERFORM 6800-REWRITE-HEADER        THRU 6899-EXIT         EL1832
00714         PERFORM 6900-UNLOCK-FORM           THRU 6999-EXIT         EL1832
00715         GO TO 6199-EXIT.                                          EL1832
00716                                                                   EL1832
00717      MOVE AT-FORM-RE-SEND-DT     TO LA4-RESEND-DATE.              EL1832
00718                                                                   EL1832
00719      IF LA4-RESEND-DATE NOT GREATER THAN CURRENT-SAVE             EL1832
00720         NEXT SENTENCE                                             EL1832
00721        ELSE                                                       EL1832
00722         PERFORM 6800-REWRITE-HEADER THRU 6899-EXIT                EL1832
00723         PERFORM 6900-UNLOCK-FORM    THRU 6999-EXIT                EL1832
00724         GO TO 6199-EXIT.                                          EL1832
00725                                                                   EL1832
00726      PERFORM 6500-UPDATE-RESEND-PRINT THRU 6599-EXIT.             EL1832
00727      PERFORM 6800-REWRITE-HEADER      THRU 6899-EXIT.             EL1832
00728      PERFORM 6600-UPDATE-FORM-TRLR    THRU 6699-EXIT.             EL1832
00729      PERFORM 7200-PRINT-FORMS-RECORDS THRU 7299-EXIT.             EL1832
00730                                                                   EL1832
00731  6199-EXIT.                                                       EL1832
00732       EXIT.                                                       EL1832
00733      EJECT                                                        EL1832
00734  6200-REPRINT-FORMS.                                              EL1832
00735      IF LA4-INITIAL-PRINT-DATE   = PI-PRINT-DATE-BIN OR           EL1832
00736         LA4-RESEND-PRINT-DATE    = PI-PRINT-DATE-BIN              EL1832
00737           PERFORM 7200-PRINT-FORMS-RECORDS THRU 7299-EXIT.        EL1832
00738                                                                   EL1832
00739  6299-EXIT.                                                       EL1832
00740       EXIT.                                                       EL1832
00741      EJECT                                                        EL1832
00742  6300-ALIGN-ROUTINE.                                              EL1832
00743                                                                      CL**4
00744      IF PI-COMPANY-ID   EQUAL   'RMC' OR 'LAP'                       CL*14
00745         MOVE  '1'                TO WS-PASSED-CNTL-CHAR              CL*11
00746         MOVE  SPACES             TO WS-PASSED-DATA                   CL*11
00747         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                          CL*11
00748                                                                      CL*11
00749         MOVE SPACES              TO WS-PASSED-CNTL-CHAR              CL*11
00750         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT 8 TIMES                  CL*11
00751                                                                      CL*11
00752         MOVE  SPACES               TO RMC-LAP-PRINT-LINE-1           CL*14
00753         MOVE  ZEROS                TO RMC-LAP-CLAIM-NO               CL*14
00754         MOVE  ZEROS                TO RMC-LAP-POLICY-NO              CL*14
00755         MOVE  RMC-LAP-PRINT-LINE-1 TO WS-PASSED-DATA                 CL*14
00756         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                          CL*11
00757                                                                      CL*11
00758         MOVE SPACES              TO WS-PASSED-DATA                   CL*11
00759                                     WS-PASSED-CNTL-CHAR              CL*11
00760         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT 3 TIMES                  CL*11
00761                                                                      CL*11
00762         MOVE  SPACES               TO RMC-LAP-PRINT-LINE-2           CL*14
00763         MOVE  ALL '*'              TO RMC-LAP-INSURED-NAME           CL*14
00764         MOVE  RMC-LAP-PRINT-LINE-2 TO WS-PASSED-DATA                 CL*14
00765         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                          CL*11
00766                                                                      CL*11
00767         MOVE SPACES              TO WS-PASSED-DATA                   CL*11
00768                                     WS-PASSED-CNTL-CHAR              CL*11
00769         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                          CL*11
00770                                                                      CL*11
00771         MOVE  SPACES               TO RMC-LAP-PRINT-LINE-3           CL*14
00772         MOVE  ALL '*'              TO RMC-LAP-INSURED-ADDR1          CL*14
00773         MOVE  RMC-LAP-PRINT-LINE-3 TO WS-PASSED-DATA                 CL*14
00774         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                          CL*11
00775                                                                      CL*11
00776         MOVE  SPACES             TO WS-PASSED-CNTL-CHAR              CL*11
00777                                     WS-PASSED-DATA                   CL*11
00778         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                          CL*11
00779                                                                      CL*11
00780         MOVE  SPACES               TO RMC-LAP-PRINT-LINE-4           CL*14
00781         MOVE  ALL '*'              TO RMC-LAP-INSURED-ADDR2          CL*14
00782         MOVE  RMC-LAP-PRINT-LINE-4 TO WS-PASSED-DATA                 CL*14
00783         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                          CL*11
00784                                                                      CL*11
00785         MOVE  SPACES             TO WS-PASSED-CNTL-CHAR              CL*11
00786                                     WS-PASSED-DATA                   CL*11
00787         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                          CL*11
00788                                                                      CL*11
00789         MOVE  SPACES               TO RMC-LAP-PRINT-LINE-5           CL*14
00790         MOVE  ALL '*'              TO RMC-LAP-INSURED-CITY-ST        CL*14
00791         MOVE  RMC-LAP-PRINT-LINE-5 TO WS-PASSED-DATA                 CL*14
00792         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                          CL*14
00793                                                                      CL*14
00794         MOVE SPACES              TO WS-PASSED-DATA                   CL*14
00795                                     WS-PASSED-CNTL-CHAR              CL*14
00796         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT 37 TIMES                 CL*15
00797                                                                      CL*14
00798         MOVE  SPACES               TO RMC-LAP-PRINT-LINE-6           CL*14
00799         MOVE  ALL '*'              TO RMC-LAP-TEXT-LINE-1            CL*14
00800         MOVE  RMC-LAP-PRINT-LINE-6 TO WS-PASSED-DATA                 CL*14
00801         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                          CL*14
00802                                                                      CL*14
00803         MOVE  SPACES             TO WS-PASSED-CNTL-CHAR              CL*14
00804                                     WS-PASSED-DATA                   CL*14
00805         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                          CL*14
00806                                                                      CL*14
00807         MOVE  SPACES               TO RMC-LAP-PRINT-LINE-7           CL*14
00808         MOVE  ALL '*'              TO RMC-LAP-TEXT-LINE-2            CL*14
00809         MOVE  RMC-LAP-PRINT-LINE-7 TO WS-PASSED-DATA                 CL*14
00810         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                          CL*14
00811                                                                      CL*14
00812         MOVE  SPACES             TO WS-PASSED-CNTL-CHAR              CL*14
00813                                     WS-PASSED-DATA                   CL*14
00814         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                          CL*14
00815                                                                      CL*14
00816         MOVE  SPACES               TO RMC-LAP-PRINT-LINE-8           CL*14
00817         MOVE  ALL '*'              TO RMC-LAP-TEXT-LINE-2            CL*14
00818         MOVE  RMC-LAP-PRINT-LINE-8 TO WS-PASSED-DATA                 CL*14
00819         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                          CL*14
00820                                                                      CL*14
00821         MOVE  SPACES             TO WS-PASSED-CNTL-CHAR              CL*14
00822                                     WS-PASSED-DATA                   CL*14
00823         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                          CL*11
00824                                                                      CL*11
00825         MOVE  'X'                TO WS-PROG-END                      CL*11
00826         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                          CL*11
00827         GO TO  6399-EXIT.                                            CL*11
00828                                                                      CL*11
00829      IF PI-COMPANY-ID   EQUAL   'LBL'                                CL**9
00830         MOVE  '1'                TO WS-PASSED-CNTL-CHAR              CL**9
00831         MOVE  SPACES             TO WS-PASSED-DATA                   CL**9
00832         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                          CL**9
00833                                                                      CL**9
00834         MOVE SPACES              TO WS-PASSED-CNTL-CHAR              CL**9
00835         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT 3 TIMES                  CL**9
00836                                                                      CL**9
00837         MOVE  SPACES             TO LBL-PRINT-LINE-1                 CL**9
00838         MOVE  ALL '*'            TO LBL-INSURED-NAME                 CL**9
00839         MOVE  ZEROS              TO LBL-CLAIM-NO                     CL**9
00840         MOVE  LBL-PRINT-LINE-1   TO WS-PASSED-DATA                   CL**9
00841         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                          CL**9
00842                                                                      CL**9
00843         MOVE SPACES              TO WS-PASSED-DATA                   CL**9
00844                                     WS-PASSED-CNTL-CHAR              CL**9
00845         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                          CL**9
00846                                                                      CL**9
00847         MOVE  SPACES             TO LBL-PRINT-LINE-2                 CL**9
00848         MOVE  ALL '*'            TO LBL-INSURED-ADDR1                CL**9
00849         MOVE  LBL-PRINT-LINE-2   TO WS-PASSED-DATA                   CL**9
00850         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                          CL**9
00851                                                                      CL**9
00852         MOVE  SPACES             TO WS-PASSED-CNTL-CHAR              CL**9
00853                                     WS-PASSED-DATA                   CL**9
00854         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                          CL**9
00855                                                                      CL**9
00856         MOVE  SPACES             TO LBL-PRINT-LINE-3                 CL**9
00857         MOVE  ALL '*'            TO LBL-INSURED-ADDR2                CL**9
00858         MOVE  LBL-PRINT-LINE-3   TO WS-PASSED-DATA                   CL**9
00859         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                          CL*10
00860                                                                      CL*10
00861         MOVE  SPACES             TO WS-PASSED-CNTL-CHAR              CL*10
00862                                     WS-PASSED-DATA                   CL*10
00863         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                          CL*10
00864                                                                      CL*10
00865         MOVE  SPACES             TO LBL-PRINT-LINE-4                 CL*10
00866         MOVE  ALL '*'            TO LBL-INSURED-CITY-ST              CL*10
00867         MOVE  LBL-PRINT-LINE-4   TO WS-PASSED-DATA                   CL*10
00868         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                          CL**9
00869                                                                      CL**9
00870         MOVE  SPACES             TO WS-PASSED-DATA                   CL**9
00871         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                          CL**9
00872                                                                      CL**9
00873         MOVE  SPACES             TO LBL-PRINT-LINE-5                 CL*10
00874         MOVE  ALL '*'            TO LBL-PAID-THRU-DATE               CL**9
00875         MOVE  LBL-PRINT-LINE-5   TO WS-PASSED-DATA                   CL*10
00876         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                          CL**9
00877                                                                      CL**9
00878         MOVE  'X'                TO WS-PROG-END                      CL**9
00879         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                          CL**9
00880         GO TO  6399-EXIT.                                            CL**9
00881                                                                      CL**9
00882      IF PI-FORM-TYPE = 'I'                                        EL1832
00883         MOVE '1'                 TO WS-PASSED-CNTL-CHAR           EL1832
00884         MOVE SPACES              TO WS-PASSED-DATA                   CL**7
00885         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                          CL**7
00886                                                                      CL**7
00887         MOVE SPACES              TO WS-PASSED-CNTL-CHAR              CL**7
00888         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT 4 TIMES                  CL**7
00889                                                                      CL**7
00890         MOVE SPACES              TO INIT-PRINT-LINE-1                CL**7
00891         MOVE ZEROS               TO INIT-ELIM-DAYS                   CL**7
00892         MOVE INIT-PRINT-LINE-1   TO WS-PASSED-DATA                EL1832
00893         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                       EL1832
00894                                                                      CL**7
00895         MOVE SPACES              TO WS-PASSED-DATA                EL1832
00896                                     WS-PASSED-CNTL-CHAR           EL1832
00897         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT 13 TIMES                 CL**7
00898                                                                      CL**7
00899         MOVE SPACES              TO INIT-PRINT-LINE-2                CL**7
00900         MOVE ALL '*'             TO INIT-ACCOUNT-NAME                CL**7
00901         MOVE INIT-PRINT-LINE-2   TO WS-PASSED-DATA                   CL**7
00902         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                          CL**7
00903                                                                      CL**7
00904         MOVE SPACES              TO INIT-PRINT-LINE-2                CL**7
00905         MOVE ALL '*'             TO INIT-ACCOUNT-NAME                CL**7
00906                                     INIT-NAME-1                      CL**7
00907         MOVE INIT-PRINT-LINE-2   TO WS-PASSED-DATA                   CL**7
00908         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT 2 TIMES                  CL**7
00909                                                                      CL**7
00910         MOVE SPACES              TO INIT-PRINT-LINE-2                CL**7
00911         MOVE ALL '*'             TO INIT-NAME-1                   EL1832
00912         MOVE INIT-PRINT-LINE-2   TO WS-PASSED-DATA                   CL**7
00913         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                          CL**7
00914                                                                      CL**7
00915         MOVE SPACES              TO INIT-PRINT-LINE-2                CL**7
00916                                     INIT-PRINT-LINE-3                CL**7
00917         MOVE ALL '*'             TO INIT-NAME-1                      CL**7
00918                                     INIT-CLAIM-ACCT                  CL**7
00919                                     INIT-CLAIM-CARRIER               CL**7
00920         MOVE INIT-PRINT-LINE-3   TO WS-PASSED-DATA                EL1832
00921         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                          CL**7
00922                                                                   EL1832
00923         MOVE SPACES              TO INIT-PRINT-LINE-3             EL1832
00924         MOVE ALL '*'             TO INIT-CLAIM-ACCT               EL1832
00925         MOVE INIT-PRINT-LINE-3   TO WS-PASSED-DATA                EL1832
00926         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                          CL**7
00927                                                                      CL**7
00928         MOVE SPACES              TO INIT-PRINT-LINE-4-1              CL**7
00929         MOVE ALL '*'             TO INIT-PRINT-SW                    CL**7
00930                                     INIT-PRINT-DATE                  CL**7
00931         MOVE INIT-PRINT-LINE-4-1 TO WS-PASSED-DATA                   CL**7
00932         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                          CL**7
00933                                                                      CL**7
00934         MOVE 'X'                 TO WS-PROG-END                   EL1832
00935         MOVE SPACES              TO WS-PASSED-DATA                EL1832
CIDMOD*       PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                       EL1832
00937       ELSE                                                        EL1832
00938         MOVE '1'                 TO WS-PASSED-CNTL-CHAR           EL1832
00939         MOVE SPACES              TO WS-PASSED-DATA                EL1832
00940         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                          CL**7
00941                                                                      CL**7
00942         MOVE SPACES              TO WS-PASSED-CNTL-CHAR           EL1832
00943         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT 8 TIMES                  CL**7
00944                                                                      CL**7
00945         MOVE SPACES              TO PROG-PRINT-LINE-1                CL**7
00946         MOVE ALL '*'             TO PROG-PRINT-SW                    CL**7
00947                                     PROG-PRINT-DATE                  CL**7
00948         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                          CL**7
00949                                                                      CL**7
00950         MOVE SPACES              TO WS-PASSED-DATA                   CL**7
00951                                     WS-PASSED-CNTL-CHAR              CL**7
00952         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT 2 TIMES                  CL**7
00953                                                                      CL**7
00954         MOVE SPACES              TO PROG-PRINT-LINE-2                CL**7
00955         MOVE ALL '*'             TO PROG-NAME-ADDR                   CL**7
00956         MOVE PROG-PRINT-LINE-2   TO WS-PASSED-DATA                   CL**7
00957         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                          CL**7
00958                                                                      CL**7
00959         MOVE SPACES              TO PROG-PRINT-LINE-2                CL**7
00960         MOVE ALL '*'             TO PROG-NAME-ADDR                EL1832
00961                                     PROG-SPEC-INST                EL1832
00962         MOVE PROG-PRINT-LINE-2   TO WS-PASSED-DATA                   CL**7
00963         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT 3 TIMES                  CL**7
00964                                                                      CL**7
00965         MOVE SPACES              TO WS-PASSED-DATA                   CL**7
00966         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT 5 TIMES                  CL**7
00967                                                                      CL**7
00968         MOVE SPACES              TO PROG-PRINT-LINE-3                CL**7
00969         MOVE ALL '*'             TO PROG-ACCOUNT-NAME             EL1832
00970                                     PROG-CLAIM                    EL1832
00971                                     PROG-CARRIER                     CL**7
00972                                     PROG-MEMBER-NO                EL1832
00973                                     PROG-ACCOUNT-NO               EL1832
00974         MOVE PROG-PRINT-LINE-3   TO WS-PASSED-DATA                   CL**7
00975         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                       EL1832
00976                                                                      CL**7
CIDMOD        MOVE 'X'                 TO WS-PROG-END.                  EL1832
CIDMOD*       PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                      EL1832
00979                                                                   EL1832
00980  6399-EXIT.                                                       EL1832
00981       EXIT.                                                       EL1832
00982                                                                      CL**4
00983      EJECT                                                        EL1832
00984  6500-UPDATE-RESEND-PRINT.                                        EL1832
00985      MOVE CURRENT-SAVE           TO LA4-RESEND-PRINT-DATE.        EL1832
00986      MOVE LOW-VALUES             TO LA4-RESEND-DATE.              EL1832
00987                                                                   EL1832
00988  6599-EXIT.                                                       EL1832
00989       EXIT.                                                       EL1832
00990      EJECT                                                        EL1832
00991  6600-UPDATE-FORM-TRLR.                                           EL1832
00992      MOVE CURRENT-SAVE           TO AT-FORM-REPRINT-DT.           EL1832
00993                                                                      CL*16
00994      EXEC CICS REWRITE                                            EL1832
00995           DATASET  (ACTV-ID)                                      EL1832
00996           FROM     (ACTIVITY-TRAILERS)                            EL1832
00997           END-EXEC.                                               EL1832
00998                                                                   EL1832
00999  6699-EXIT.                                                       EL1832
01000       EXIT.                                                       EL1832
01001      EJECT                                                        EL1832
01002  6700-ZERO-RESEND-IN-HEADER.                                      EL1832
01003                                                                      CL*16
01004      MOVE LOW-VALUES             TO LA4-RESEND-DATE.              EL1832
01005                                                                   EL1832
01006  6799-EXIT.                                                       EL1832
01007       EXIT.                                                       EL1832
01008      EJECT                                                        EL1832
01009  6800-REWRITE-HEADER.                                             EL1832
01010      EXEC CICS HANDLE CONDITION                                   EL1832
01011          DUPKEY  (6899-EXIT)                                         CL*16
01012          END-EXEC.                                                EL1832
01013                                                                   EL1832
01014      EXEC CICS REWRITE                                            EL1832
01015           DATASET  (ARCH-ID)                                      EL1832
01016           FROM     (LETTER-ARCHIVE)                               EL1832
01017           END-EXEC.                                               EL1832
01018                                                                   EL1832
01019  6899-EXIT.                                                       EL1832
01020       EXIT.                                                       EL1832
01021      EJECT                                                        EL1832
01022  6900-UNLOCK-FORM.                                                EL1832
01023      EXEC CICS UNLOCK                                             EL1832
01024           DATASET  (ACTV-ID)                                      EL1832
01025           END-EXEC.                                               EL1832
01026                                                                   EL1832
01027  6999-EXIT.                                                       EL1832
01028       EXIT.                                                       EL1832
01029      EJECT                                                        EL1832
01030  7200-PRINT-FORMS-RECORDS.                                        EL1832
01031      MOVE SPACES                 TO PROG-PRINT-LINE-1             EL1832
01032                                     PROG-PRINT-LINE-2                CL**7
01033                                     PROG-PRINT-LINE-3             EL1832
01034                                     INIT-PRINT-LINE-1             EL1832
01035                                     INIT-PRINT-LINE-2             EL1832
01036                                     INIT-PRINT-LINE-3             EL1832
01037                                     INIT-PRINT-LINE-4-1           EL1832
01038                                     LBL-PRINT-LINE-1                 CL**9
01039                                     LBL-PRINT-LINE-2                 CL**9
01040                                     LBL-PRINT-LINE-3                 CL**9
01041                                     LBL-PRINT-LINE-4                 CL**9
01042                                     LBL-PRINT-LINE-5                 CL*10
01043                                     RMC-LAP-PRINT-LINE-1             CL*14
01044                                     RMC-LAP-PRINT-LINE-2             CL*14
01045                                     RMC-LAP-PRINT-LINE-3             CL*14
01046                                     RMC-LAP-PRINT-LINE-4             CL*14
01047                                     RMC-LAP-PRINT-LINE-5             CL*14
01048                                     RMC-LAP-PRINT-LINE-6             CL*14
01049                                     RMC-LAP-PRINT-LINE-7             CL*14
01050                                     RMC-LAP-PRINT-LINE-8             CL*14
01051                                     WS-PRINT-WORK-AREAS.          EL1832
01052                                                                   EL1832
01053      MOVE ZEROS                  TO WS-ADDR-SEQ                   EL1832
01054                                     WS-TOTAL-PAID                 EL1832
01055                                     WS-BEN-DAYS-HOLD.             EL1832
01056                                                                   EL1832
01057      MOVE ARCH-NUMBER2           TO ARCH-NUMBER.                  EL1832
01058      MOVE '4'                    TO ARCH-REC-TYPE.                EL1832
01059      MOVE ZEROS                  TO ARCH-SEQ.                     EL1832
01060                                                                   EL1832
01061      EXEC CICS READ                                               EL1832
01062           DATASET   (ARCH-ID)                                     EL1832
01063           RIDFLD    (ARCH-KEY)                                    EL1832
01064           INTO      (LETTER-ARCHIVE)                              EL1832
01065           END-EXEC.                                               EL1832
01066                                                                   EL1832
01067      MOVE LA4-CARRIER             TO ACTV-CARRIER.                EL1832
01068      MOVE LA4-CLAIM-NO            TO ACTV-CLAIM.                  EL1832
01069      MOVE LA4-CERT-NO             TO ACTV-CERT-NO.                EL1832
01070      MOVE LA4-FORM-TRLR-SEQ       TO ACTV-SEQ.                    EL1832
01071                                                                   EL1832
01072      EXEC CICS HANDLE CONDITION                                   EL1832
01073           NOTOPEN  (8870-ACTV-NOT-OPEN)                           EL1832
01074           NOTFND   (7299-EXIT)                                    EL1832
01075           END-EXEC.                                               EL1832
01076                                                                   EL1832
01077      EXEC CICS READ                                               EL1832
01078           DATASET  (ACTV-ID)                                      EL1832
01079           RIDFLD   (ACTV-KEY)                                     EL1832
01080           SET      (ADDRESS OF ACTIVITY-TRAILERS)                    CL*17
01081           END-EXEC.                                               EL1832
01082                                                                   EL1832
01083      MOVE AT-CARRIER             TO WS-CARRIER (1).               EL1832
01084      MOVE AT-CLAIM-NO            TO WS-CLAIM   (1).               EL1832
01085      MOVE AT-CERT-NO             TO WS-CERT    (1).               EL1832
01086                                                                   EL1832
01087      IF NOT FORM-CONTROL-TR                                       EL1832
01088         MOVE 'THIS FORM RECORD IS INVALID'     TO WS-SPEC-INST1   EL1832
01089         MOVE 'PLEASE CONTACT DATA PROCESSING'  TO WS-SPEC-INST2   EL1832
01090         MOVE 'TO INFORM THEM OF THIS ERROR'    TO WS-SPEC-INST3   EL1832
01091         GO TO 7205-CLAIM-LOOP.                                    EL1832
01092                                                                   EL1832
01093      MOVE SPACES                 TO WS-CLAIM-TABLE (2)               CL**7
01094                                     WS-CLAIM-TABLE (3).              CL**7
01095      MOVE AT-INSTRUCT-LN-1       TO WS-SPEC-INST1.                EL1832
01096      MOVE AT-INSTRUCT-LN-2       TO WS-SPEC-INST2.                EL1832
01097      MOVE AT-INSTRUCT-LN-3       TO WS-SPEC-INST3.                EL1832
01098      MOVE AT-FORM-ADDR-SEQ-NO    TO WS-ADDR-SEQ.                  EL1832
01099      MOVE AT-FORM-ADDRESS        TO WS-ADDR-TYPE.                 EL1832
01100      MOVE 0                      TO SUB.                          EL1832
01101      MOVE 1                      TO PRT.                          EL1832
01102                                                                   EL1832
01103  7205-CLAIM-LOOP.                                                 EL1832
01104      ADD 1 TO SUB.                                                EL1832
01105                                                                   EL1832
01106      IF SUB = 4                                                   EL1832
01107         GO TO 7250-PRINT-FORM.                                    EL1832
01108                                                                   EL1832
01109      IF WS-CLAIM-TABLE (SUB)  =  SPACES  OR  LOW-VALUES           EL1832
01110         GO TO 7205-CLAIM-LOOP.                                    EL1832
01111                                                                   EL1832
01112      MOVE WS-CARRIER (SUB)       TO CLAM-CARRIER.                 EL1832
01113      MOVE WS-CLAIM (SUB)         TO CLAM-CLAIM.                   EL1832
01114      MOVE WS-CERT (SUB)          TO CLAM-CERT.                    EL1832
01115      MOVE PI-COMPANY-CD          TO CLAM-CO.                      EL1832
01116                                                                   EL1832
01117      EXEC CICS HANDLE CONDITION                                   EL1832
01118           NOTOPEN  (8880-CLAM-NOT-OPEN)                           EL1832
01119           NOTFND   (7205-CLAIM-LOOP)                              EL1832
01120           END-EXEC.                                               EL1832
01121                                                                   EL1832
01122      EXEC CICS READ                                               EL1832
01123           DATASET  (CLAM-ID)                                      EL1832
01124           RIDFLD   (CLAM-KEY)                                     EL1832
01125           SET      (ADDRESS OF CLAIM-MASTER)                         CL*17
01126           UPDATE                                                  EL1832
01127           END-EXEC.                                               EL1832
01128                                                                   EL1832
01129      IF CL-INCURRED-DT GREATER THAN WS-INCURRED-DT                EL1832
01130         MOVE CL-INCURRED-DT         TO DC-BIN-DATE-1              EL1832
01131                                        WS-INCURRED-DT             EL1832
01132         MOVE SPACE                  TO DC-OPTION-CODE             EL1832
01133         PERFORM 9700-DATE-LINK THRU 9700-EXIT                     EL1832
01134         MOVE DC-GREG-DATE-1-EDIT    TO WS-GREG-INCURRED.          EL1832
01135                                                                   EL1832
01136      MOVE CURRENT-SAVE              TO CL-LAST-MAINT-DT.          EL1832
01137      MOVE PI-PROCESSOR-ID           TO CL-LAST-MAINT-USER.        EL1832
01138      MOVE EIBTIME                   TO CL-LAST-MAINT-HHMMSS.      EL1832
01139      MOVE '2'                       TO CL-LAST-MAINT-TYPE.        EL1832
01140                                                                   EL1832
01141      MOVE CL-CLAIM-NO               TO WS-SAVE-CLAIM.             EL1832
01142      MOVE CL-COMPANY-CD             TO ACCT-CO                       CL**4
01143                                        CERT-CO.                      CL**4
01144      MOVE CL-CARRIER                TO ACCT-CARRIER.              EL1832
01145      MOVE CL-CERT-GROUPING          TO ACCT-GROUPING.             EL1832
01146      MOVE CL-CERT-STATE             TO ACCT-STATE.                EL1832
01147      MOVE CL-CERT-ACCOUNT           TO ACCT-ACCOUNT.              EL1832
01148      MOVE CL-CERT-EFF-DT            TO WS-CERT-EFF-DT.            EL1832
01149      MOVE CL-CERT-KEY-DATA          TO CERT-PRIME.                EL1832
01150                                                                   EL1832
01151      EXEC CICS HANDLE CONDITION                                   EL1832
01152          DUPKEY (7206-CLAIM-REWRITE)                              EL1832
01153          END-EXEC.                                                EL1832
01154                                                                   EL1832
01155      EXEC CICS REWRITE                                            EL1832
01156           DATASET  (CLAM-ID)                                      EL1832
01157           FROM     (CLAIM-MASTER)                                 EL1832
01158           END-EXEC.                                               EL1832
01159                                                                   EL1832
01160  7206-CLAIM-REWRITE.                                              EL1832
01161      EXEC CICS HANDLE CONDITION                                   EL1832
01162           NOTOPEN  (9000-CERT-NOT-OPEN)                           EL1832
01163           NOTFND   (7205-CLAIM-LOOP)                              EL1832
01164           END-EXEC.                                               EL1832
01165                                                                   EL1832
01166      MOVE WS-CERT (SUB)          TO CERT-CERT.                    EL1832
01167                                                                   EL1832
01168      EXEC CICS READ                                               EL1832
01169           DATASET  (CERT-ID)                                      EL1832
01170           RIDFLD   (CERT-KEY)                                     EL1832
01171           SET      (ADDRESS OF CERTIFICATE-MASTER)                   CL*17
01172           END-EXEC.                                               EL1832
01173                                                                   EL1832
01174      MOVE CM-CERT-NO             TO WS-CERT-NO.                   EL1832
01175      MOVE CM-INSURED-LAST-NAME   TO WS-INSURED-LNAME.             EL1832
01176      MOVE CM-INSURED-FIRST-NAME  TO WS-INSURED-FNAME.             EL1832
01177      MOVE CM-LOAN-NUMBER         TO WS-SAVE-LOAN-NO.              EL1832
01178      IF CM-PAYMENT-MODE  = SPACES                                 EL1832
01179         MOVE 'M'                 TO WS-PAY-FREQ                   EL1832
01180        ELSE                                                       EL1832
01181         IF CM-PAYMENT-MODE = '1'                                  EL1832
01182            MOVE 'W'              TO WS-PAY-FREQ                   EL1832
01183           ELSE                                                    EL1832
01184            IF CM-PAYMENT-MODE = '2'                               EL1832
01185               MOVE 'SM'          TO WS-PAY-FREQ                   EL1832
01186              ELSE                                                 EL1832
01187               IF CM-PAYMENT-MODE = '3'                            EL1832
01188                  MOVE 'BW'       TO WS-PAY-FREQ                   EL1832
01189                 ELSE                                              EL1832
01190                  IF CM-PAYMENT-MODE = '4'                         EL1832
01191                     MOVE 'SA'    TO WS-PAY-FREQ.                  EL1832
01192                                                                   EL1832
01193      IF CERT-READ                                                 EL1832
01194         GO TO 7220-BROWSE-PAYMENTS.                               EL1832
01195                                                                   EL1832
01196  7210-STARTBR-ACCOUNT.                                               CL**4
01197                                                                      CL**4
01198      MOVE 'Y'                    TO CERT-READ-SW.                 EL1832
01199      MOVE CM-ACCOUNT             TO WS-SAVE-ACCT.                 EL1832
01200      MOVE CM-MEMBER-NO           TO WS-SAVE-MEMBER.               EL1832
01201                                                                   EL1832
01202      IF CM-SSN-STATE   = CM-STATE AND                             EL1832
01203         CM-SSN-ACCOUNT = CM-ACCOUNT                               EL1832
01204         MOVE SPACES              TO WS-SAVE-SS                    EL1832
01205        ELSE                                                       EL1832
01206         MOVE CM-SOC-SEC-NO       TO WS-SAVE-SS.                   EL1832
01207                                                                   EL1832
01208      EXEC CICS HANDLE CONDITION                                   EL1832
01209           NOTOPEN  (9100-ACCOUNT-NOT-OPEN)                        EL1832
01210           NOTFND   (7215-END-ACCT)                                EL1832
01211           ENDFILE  (7215-END-ACCT)                                EL1832
01212           END-EXEC.                                               EL1832
01213                                                                   EL1832
01214      EXEC CICS STARTBR                                            EL1832
01215           RIDFLD    (ACCT-KEY)                                    EL1832
01216           DATASET   (ACCT-ID)                                     EL1832
01217           KEYLENGTH (20)                                          EL1832
01218           GENERIC                                                 EL1832
01219           END-EXEC.                                               EL1832
01220                                                                   EL1832
01221      MOVE ACCT-PARTIAL-KEY       TO ACCT-SAVE-KEY.                EL1832
01222      MOVE 'Y'                    TO ACCT-BROWSE-SW.               EL1832
01223                                                                   EL1832
01224  7210-READNEXT-ACCOUNT.                                           EL1832
01225      EXEC CICS READNEXT                                           EL1832
01226           DATASET   (ACCT-ID)                                     EL1832
01227           SET       (ADDRESS OF ACCOUNT-MASTER)                      CL*17
01228           RIDFLD    (ACCT-KEY)                                    EL1832
01229           END-EXEC.                                               EL1832
01230                                                                   EL1832
01231      IF ACCT-PARTIAL-KEY NOT = ACCT-SAVE-KEY                      EL1832
01232         GO TO 7215-END-ACCT.                                      EL1832
01233                                                                   EL1832
01234      IF WS-CERT-EFF-DT  NOT LESS  AM-EFFECTIVE-DT AND             EL1832
01235         WS-CERT-EFF-DT      LESS  AM-EXPIRATION-DT                EL1832
01236         NEXT SENTENCE                                             EL1832
01237        ELSE                                                       EL1832
01238         GO TO 7210-READNEXT-ACCOUNT.                              EL1832
01239                                                                   EL1832
01240      MOVE AM-NAME                TO WS-SAVE-ACCT-NAME.            EL1832
01241      MOVE 'F'                    TO ACCT-BROWSE-SW.               EL1832
01242                                                                   EL1832
01243      IF WS-ADDR-TYPE = 'A'                                           CL**4
01244         MOVE AM-NAME             TO WS-NAME-WORK                  EL1832
01245         MOVE AM-PERSON           TO WS-ADDR1-WORK                 EL1832
01246         MOVE AM-ADDRS            TO WS-ADDR2-WORK                 EL1832
01247 *       MOVE AM-CITY             TO WS-CITY-STATE-WORK            EL1832
              STRING AM-ADDR-CITY ' ' AM-ADDR-STATE
                 DELIMITED BY '  ' INTO WS-CITY-STATE-WORK
              END-STRING
01248         MOVE SPACES              TO WS-ZIP-WORK                      CL**8
01249         IF AM-CANADIAN-POST-CODE                                     CL**8
01250             MOVE AM-CAN-POSTAL-1  TO WS-CAN-POSTAL-1                 CL**8
01251             MOVE AM-CAN-POSTAL-2  TO WS-CAN-POSTAL-2                 CL**8
01252         ELSE                                                         CL**8
01253             MOVE AM-ZIP-PRIME     TO WS-ZIP-PRIME                    CL**8
01254             IF AM-ZIP-PLUS4 NOT = SPACES  AND  ZEROS                 CL**8
01255                 MOVE '-'          TO WS-ZIP-DASH                     CL**8
01256                 MOVE AM-ZIP-PLUS4 TO WS-ZIP-PLUS4.                   CL**8
01257                                                                   EL1832
01258  7215-END-ACCT.                                                   EL1832
01259      IF ACCT-BROWSE-STARTED OR ACCT-FOUND                         EL1832
01260          EXEC CICS ENDBR                                          EL1832
01261               DATASET  (ACCT-ID)                                  EL1832
01262               END-EXEC.                                           EL1832
01263                                                                   EL1832
01264  7220-BROWSE-PAYMENTS.                                            EL1832
01265      IF PI-FORM-TYPE = 'I'                                        EL1832
01266         PERFORM 7300-FORMAT-INITIAL-DATA THRU 7399-EXIT           EL1832
01267         GO TO 7205-CLAIM-LOOP.                                    EL1832
01268                                                                   EL1832
01269      MOVE ZEROS              TO ACTV-SEQ.                         EL1832
01270      MOVE CLAM-KEY           TO WS-ACTV-KEY   ACTV-PARTIAL-KEY.   EL1832
01271                                                                   EL1832
01272      EXEC CICS STARTBR                                            EL1832
01273           DATASET(ACTV-ID)                                        EL1832
01274           RIDFLD(ACTV-KEY)                                        EL1832
01275           END-EXEC.                                               EL1832
01276                                                                   EL1832
01277      EXEC CICS HANDLE CONDITION                                   EL1832
01278           ENDFILE  (7240-END-BR)                                  EL1832
01279           END-EXEC.                                               EL1832
01280                                                                   EL1832
01281  7230-READ-NEXT.                                                  EL1832
01282      EXEC CICS READNEXT                                           EL1832
01283           DATASET   (ACTV-ID)                                     EL1832
01284           RIDFLD    (ACTV-KEY)                                    EL1832
01285           SET       (ADDRESS OF ACTIVITY-TRAILERS)                   CL*17
01286           END-EXEC.                                               EL1832
01287                                                                   EL1832
01288      IF ACTV-PARTIAL-KEY NOT = WS-ACTV-KEY                        EL1832
01289         GO TO 7240-END-BR.                                        EL1832
01290                                                                   EL1832
01291      IF AT-TRAILER-TYPE NOT = '2'                                 EL1832
01292         GO TO 7230-READ-NEXT.                                     EL1832
01293                                                                   EL1832
01294      IF AT-VOID-DT  NOT = LOW-VALUES                              EL1832
01295         GO TO 7230-READ-NEXT.                                     EL1832
01296                                                                   EL1832
01297      IF ADDITIONAL-PAYMENT OR CHARGEABLE-EXPENSE OR               EL1832
01298         NON-CHARGEABLE-EXPENSE                                    EL1832
01299         GO TO 7230-READ-NEXT.                                     EL1832
01300                                                                   EL1832
01301 *    MOVE ACTV-CLAIM             TO PROG-CLAIM-NO     (PRT).         CL**7
01302 *    MOVE WS-SAVE-LOAN-NO        TO PROG-LOAN-NO      (PRT).         CL**7
01303 *    MOVE AT-PAID-FROM-DT        TO DC-BIN-DATE-1.                   CL**7
01304 *    MOVE SPACE                  TO DC-OPTION-CODE.                  CL**7
01305 *    PERFORM 9700-DATE-LINK THRU 9700-EXIT.                          CL**7
01306 *    MOVE DC-GREG-DATE-1-EDIT    TO PROG-PAID-FROM    (PRT).         CL**7
01307                                                                      CL**4
01308 *    IF NOT PI-USES-PAID-TO                                          CL**7
01309 *       MOVE AT-PAID-THRU-DT     TO DC-BIN-DATE-1                    CL**7
01310 *       MOVE SPACE               TO DC-OPTION-CODE                   CL**7
01311 *       PERFORM 9700-DATE-LINK THRU 9700-EXIT                        CL**7
01312 *       MOVE DC-GREG-DATE-1-EDIT TO PROG-PAID-THRU (PRT)             CL**7
01313 *    ELSE                                                            CL**7
01314 *       MOVE AT-PAID-THRU-DT     TO DC-BIN-DATE-1                    CL**7
01315 *       MOVE '6'                 TO DC-OPTION-CODE                   CL**7
01316 *       MOVE +1                  TO DC-ELAPSED-DAYS                  CL**7
01317 *       MOVE +0                  TO DC-ELAPSED-MONTHS                CL**7
01318 *       PERFORM 9700-DATE-LINK THRU 9700-EXIT                        CL**7
01319 *       MOVE DC-GREG-DATE-1-EDIT TO PROG-PAID-THRU (PRT).            CL**7
01320                                                                   EL1832
01321      IF AT-PAID-THRU-DT GREATER THAN WS-PAID-THRU-DT              EL1832
01322         MOVE AT-PAID-THRU-DT     TO WS-PAID-THRU-DT               EL1832
01323         MOVE DC-GREG-DATE-1-EDIT TO WS-GREG-PAID-THRU.            EL1832
01324                                                                   EL1832
01325 *    MOVE AT-DAYS-IN-PERIOD      TO PROG-NO-DAYS      (PRT).         CL**7
01326 *    MOVE AT-DAYS-IN-PERIOD      TO PROG-DAYS-PAID    (PRT).         CL**7
01327 *    MOVE AT-ELIMINATION-DAYS    TO PROG-ELIM-DAYS    (PRT).         CL**7
01328 *    MOVE AT-DAILY-RATE          TO PROG-DAILY-RATE   (PRT).         CL**7
01329 *    MOVE AT-AMOUNT-PAID         TO PROG-PAYMENT-AMT  (PRT).         CL**7
01330 *    ADD AT-AMOUNT-PAID          TO WS-TOTAL-PAID.                   CL**7
01331      ADD  1                      TO PRT.                          EL1832
01332                                                                   EL1832
01333  7240-END-BR.                                                     EL1832
01334      EXEC CICS ENDBR                                              EL1832
01335           DATASET   (ACTV-ID)                                     EL1832
01336           END-EXEC.                                               EL1832
01337                                                                   EL1832
01338      GO TO 7205-CLAIM-LOOP.                                       EL1832
01339                                                                   EL1832
01340      EJECT                                                        EL1832
01341  7250-PRINT-FORM.                                                 EL1832
01342      IF WS-ADDR-TYPE = 'A'  AND WS-ADDR-SEQ = ZEROS                  CL**4
01343         GO TO 7255-CHECK-FORM.                                    EL1832
01344                                                                   EL1832
01345      MOVE WS-ADDR-SEQ             TO ACTV-SEQ.                    EL1832
01346      MOVE LA4-CARRIER             TO ACTV-CARRIER.                EL1832
01347      MOVE LA4-CLAIM-NO            TO ACTV-CLAIM.                  EL1832
01348      MOVE LA4-CERT-NO             TO ACTV-CERT-NO.                EL1832
01349                                                                   EL1832
01350      EXEC CICS HANDLE CONDITION                                   EL1832
01351           NOTFND   (7255-CHECK-FORM)                              EL1832
01352           END-EXEC.                                               EL1832
01353                                                                   EL1832
01354      EXEC CICS READ                                               EL1832
01355           DATASET  (ACTV-ID)                                      EL1832
01356           RIDFLD   (ACTV-KEY)                                     EL1832
01357           SET      (ADDRESS OF ACTIVITY-TRAILERS)                    CL*17
01358           END-EXEC.                                               EL1832
01359                                                                   EL1832
01360      MOVE AT-MAIL-TO-NAME        TO WS-NAME-WORK.                 EL1832
01361      MOVE AT-ADDRESS-LINE-1      TO WS-ADDR1-WORK.                EL1832
01362      MOVE AT-ADDRESS-LINE-2      TO WS-ADDR2-WORK.                EL1832
01363 *    MOVE AT-CITY-STATE          TO WS-CITY-STATE-WORK.           EL1832
           STRING AT-CITY ' ' AT-STATE
              DELIMITED BY '  ' INTO WS-CITY-STATE-WORK
           END-STRING
01364      MOVE SPACES                 TO WS-ZIP-WORK.                     CL**8
01365      IF AT-CANADIAN-POST-CODE                                        CL**8
01366          MOVE AT-CAN-POSTAL-1    TO WS-CAN-POSTAL-1                  CL**8
01367          MOVE AT-CAN-POSTAL-2    TO WS-CAN-POSTAL-2                  CL**8
01368      ELSE                                                            CL**8
01369          MOVE AT-ZIP-CODE        TO WS-ZIP-PRIME                     CL**8
01370          IF AT-ZIP-PLUS4 NOT = SPACES  AND  ZEROS                    CL**8
01371              MOVE '-'            TO WS-ZIP-DASH                      CL**8
01372              MOVE AT-ZIP-PLUS4   TO WS-ZIP-PLUS4.                    CL**8
01373                                                                      CL**8
01374                                                                   EL1832
01375      EJECT                                                        EL1832
01376  7255-CHECK-FORM.                                                 EL1832
01377      IF PI-FORM-TYPE = 'P'                                        EL1832
01378          GO TO 7260-PRINT-PROG.                                      CL**4
01379                                                                   EL1832
01380      MOVE '1'                 TO WS-PASSED-CNTL-CHAR.             EL1832
CIDMOD     MOVE INIT-PRINT-LINE-1   TO WS-PASSED-DATA.                       000
01381      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL1832
01382                                                                   EL1832
01383      MOVE SPACES              TO WS-PASSED-CNTL-CHAR              EL1832
01384                                  WS-PASSED-DATA.                  EL1832
01385      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT 4 TIMES.                    CL**7
01386                                                                   EL1832
01387 ****************  MAY BE GARBAGE????????????????                     CL*19
01388      MOVE INIT-PRINT-LINE-1   TO WS-PASSED-DATA.                     CL**7
01389      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL1832
01390                                                                   EL1832
01391      MOVE SPACES              TO WS-PASSED-DATA                   EL1832
01392                                  WS-PASSED-CNTL-CHAR.             EL1832
01393      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT 13 TIMES.                   CL**7
01394                                                                   EL1832
01395      MOVE SPACES              TO INIT-PRINT-LINE-2.                  CL**7
01396      MOVE WS-SAVE-ACCT-NAME   TO INIT-ACCOUNT-NAME.               EL1832
01397      MOVE INIT-PRINT-LINE-2   TO WS-PASSED-DATA.                     CL**7
01398      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL1832
01399                                                                   EL1832
01400      MOVE WS-SAVE-MEMBER      TO INIT-ACCOUNT-NAME.               EL1832
01401      MOVE WS-NAME-WORK        TO INIT-NAME-1.                        CL**7
01402      MOVE INIT-PRINT-LINE-2   TO WS-PASSED-DATA.                     CL**7
01403      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL1832
01404                                                                   EL1832
01405      MOVE WS-SAVE-SS          TO INIT-ACCOUNT-NAME.               EL1832
01406      MOVE WS-ADDR1-WORK       TO INIT-NAME-1.                        CL**7
01407      MOVE INIT-PRINT-LINE-2   TO WS-PASSED-DATA.                     CL**7
01408      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                            CL**7
01409                                                                      CL**7
01410      MOVE SPACES              TO INIT-PRINT-LINE-2.                  CL**7
01411      MOVE WS-ADDR2-WORK       TO INIT-NAME-1.                     EL1832
01412      MOVE INIT-PRINT-LINE-2   TO WS-PASSED-DATA.                     CL**7
01413      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL1832
01414                                                                   EL1832
01415      MOVE WS-CITY-STATE-WORK  TO WS-LABEL-HOLD-AREA.              EL1832
01416      MOVE WS-ZIP-WORK         TO WS-ZIP-LABEL.                       CL**8
01417                                                                   EL1832
01418      MOVE SPACES              TO INIT-PRINT-LINE-2                   CL**7
01419                                  INIT-PRINT-LINE-3.                  CL**7
01420      MOVE WS-LABEL-HOLD-AREA  TO INIT-NAME-1.                     EL1832
01421      MOVE WS-SAVE-CLAIM       TO INIT-CLAIM-ACCT.                    CL**7
01422      MOVE WS-CARRIER (1)      TO INIT-CLAIM-CARRIER.                 CL**7
01423      MOVE INIT-PRINT-LINE-3   TO WS-PASSED-DATA.                  EL1832
01424      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL1832
01425                                                                   EL1832
01426      MOVE SPACES              TO INIT-PRINT-LINE-3.               EL1832
01427      MOVE WS-SAVE-ACCT        TO INIT-CLAIM-ACCT.                 EL1832
01428      MOVE INIT-PRINT-LINE-3   TO WS-PASSED-DATA.                  EL1832
01429      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL1832
01430                                                                   EL1832
01431      MOVE SPACES              TO INIT-PRINT-LINE-4-1.                CL**7
01432      MOVE WS-PRINT-SW         TO INIT-PRINT-SW.                   EL1832
01433      MOVE SAVE-DATE           TO INIT-PRINT-DATE.                 EL1832
01434      MOVE INIT-PRINT-LINE-4-1 TO WS-PASSED-DATA.                  EL1832
01435      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL1832
01436                                                                   EL1832
01437      MOVE 'X'                 TO WS-PROG-END.                     EL1832
01438      MOVE SPACES              TO WS-PASSED-DATA.                  EL1832
CIDMOD*    PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL1832
01440      GO TO 7299-EXIT.                                             EL1832
01441      EJECT                                                        EL1832
01442  7260-PRINT-PROG.                                                 EL1832
01443                                                                      CL**7
01444      IF  PI-COMPANY-ID   EQUAL   'RMC' OR 'LAP'                      CL*14
01445          GO TO 7260-PRINT-RMC-LAP-PROG.                              CL*14
01446                                                                      CL*11
01447      IF  PI-COMPANY-ID   EQUAL   'LBL'                               CL**9
01448          NEXT SENTENCE                                               CL*10
01449      ELSE                                                            CL*10
01450         GO TO 7260-PRINT-PROG-CONT.                                  CL*10
01451                                                                      CL**7
01452      MOVE  '1'                TO WS-PASSED-CNTL-CHAR.                CL*10
01453      MOVE  SPACES             TO WS-PASSED-DATA.                     CL*10
01454      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                            CL*10
01455                                                                   EL1832
01456      MOVE  SPACES             TO WS-PASSED-DATA                      CL*10
01457                                  WS-PASSED-CNTL-CHAR.                CL*10
01458      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT 3 TIMES.                    CL*10
01459                                                                   EL1832
01460      MOVE  SPACES             TO LBL-PRINT-LINE-1.                   CL*10
01461      MOVE  WS-NAME-WORK       TO LBL-INSURED-NAME.                   CL*10
01462      MOVE  CLAM-CLAIM         TO LBL-CLAIM-NO.                       CL*10
01463      MOVE  LBL-PRINT-LINE-1   TO WS-PASSED-DATA.                     CL*10
01464      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                            CL*10
01465                                                                      CL*10
01466      IF  WS-ADDR1-WORK     =    SPACES                               CL*10
01467          MOVE  SPACES             TO LBL-PRINT-LINE-3                CL*10
01468          MOVE  WS-ADDR2-WORK      TO LBL-INSURED-ADDR2               CL*10
01469          MOVE  LBL-PRINT-LINE-3   TO LBL-PRINT-LINE-2                CL*10
01470          MOVE  LBL-PRINT-LINE-2   TO WS-PASSED-DATA                  CL**9
01471          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                         CL**9
01472                                                                   EL1832
01473          MOVE  SPACES             TO LBL-PRINT-LINE-4                CL*10
01474          MOVE  WS-CITY-STATE-WORK TO WS-LABEL-HOLD-AREA              CL**9
01475          MOVE  WS-ZIP-WORK        TO WS-ZIP-LABEL                    CL**9
01476          MOVE  WS-LABEL-HOLD-AREA TO LBL-INSURED-CITY-ST             CL*10
01477          MOVE  LBL-PRINT-LINE-4   TO LBL-PRINT-LINE-3                CL*10
01478          MOVE  LBL-PRINT-LINE-3   TO WS-PASSED-DATA                  CL**9
01479          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                         CL**9
01480                                                                   EL1832
01481          MOVE  SPACES             TO LBL-PRINT-LINE-5                CL*10
01482          MOVE  WS-PAID-THRU-DT    TO DC-BIN-DATE-1                   CL*10
01483          MOVE  ' '                TO DC-OPTION-CODE                  CL*10
01484          PERFORM 9700-DATE-LINK THRU 9700-EXIT                       CL**9
01485          MOVE  DC-GREG-DATE-1-EDIT  TO LBL-PAID-THRU-DATE            CL*10
01486          MOVE  LBL-PRINT-LINE-5   TO LBL-PRINT-LINE-4                CL*10
01487          MOVE  LBL-PRINT-LINE-4   TO WS-PASSED-DATA                  CL**9
01488          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                         CL**9
01489                                                                   EL1832
01490          MOVE  'X'                TO WS-PROG-END                     CL**9
01491          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                         CL**9
01492          GO TO 7299-EXIT.                                            CL**9
01493                                                                      CL*10
01494      IF  WS-ADDR1-WORK  NOT =  SPACES   AND                          CL*10
01495          WS-ADDR2-WORK      =  SPACES                                CL*10
01496          MOVE  SPACES             TO LBL-PRINT-LINE-2                CL*10
01497          MOVE  WS-ADDR1-WORK      TO LBL-INSURED-ADDR1               CL*10
01498          MOVE  LBL-PRINT-LINE-2   TO WS-PASSED-DATA                  CL*10
01499          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                         CL*10
01500                                                                      CL*10
01501          MOVE  SPACES             TO LBL-PRINT-LINE-4                CL*10
01502          MOVE  WS-CITY-STATE-WORK TO WS-LABEL-HOLD-AREA              CL*10
01503          MOVE  WS-ZIP-WORK        TO WS-ZIP-LABEL                    CL*10
01504          MOVE  WS-LABEL-HOLD-AREA TO LBL-INSURED-CITY-ST             CL*10
01505          MOVE  LBL-PRINT-LINE-4   TO LBL-PRINT-LINE-3                CL*10
01506          MOVE  LBL-PRINT-LINE-3   TO WS-PASSED-DATA                  CL*10
01507          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                         CL*10
01508                                                                      CL*10
01509          MOVE  SPACES             TO LBL-PRINT-LINE-5                CL*10
01510          MOVE  WS-PAID-THRU-DT    TO DC-BIN-DATE-1                   CL*10
01511          MOVE  ' '                TO DC-OPTION-CODE                  CL*10
01512          PERFORM 9700-DATE-LINK THRU 9700-EXIT                       CL*10
01513          MOVE  DC-GREG-DATE-1-EDIT  TO LBL-PAID-THRU-DATE            CL*10
01514          MOVE  LBL-PRINT-LINE-5   TO LBL-PRINT-LINE-4                CL*10
01515          MOVE  LBL-PRINT-LINE-4   TO WS-PASSED-DATA                  CL*10
01516          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                         CL*10
01517                                                                      CL*10
01518          MOVE  'X'                TO WS-PROG-END                     CL*10
01519          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                         CL*10
01520          GO TO 7299-EXIT.                                            CL*10
01521                                                                      CL*10
01522      IF  WS-ADDR1-WORK   NOT =  SPACES AND                           CL*10
01523          WS-ADDR2-WORK   NOT =  SPACES                               CL*10
01524          MOVE  SPACES             TO LBL-PRINT-LINE-2                CL*10
01525          MOVE  WS-ADDR1-WORK      TO LBL-INSURED-ADDR1               CL*10
01526          MOVE  LBL-PRINT-LINE-2   TO WS-PASSED-DATA                  CL*10
01527          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                         CL*10
01528                                                                      CL*10
01529          MOVE  SPACES             TO LBL-PRINT-LINE-3                CL*10
01530          MOVE  WS-ADDR2-WORK      TO LBL-INSURED-ADDR2               CL*10
01531          MOVE  LBL-PRINT-LINE-3   TO WS-PASSED-DATA                  CL*10
01532          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                         CL*10
01533                                                                      CL*10
01534          MOVE  SPACES             TO LBL-PRINT-LINE-4                CL*10
01535          MOVE  WS-CITY-STATE-WORK TO WS-LABEL-HOLD-AREA              CL*10
01536          MOVE  WS-ZIP-WORK        TO WS-ZIP-LABEL                    CL*10
01537          MOVE  WS-LABEL-HOLD-AREA TO LBL-INSURED-CITY-ST             CL*10
01538          MOVE  LBL-PRINT-LINE-4   TO WS-PASSED-DATA                  CL*10
01539          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                         CL*10
01540                                                                      CL*10
01541          MOVE  SPACES             TO LBL-PRINT-LINE-5                CL*10
01542          MOVE  WS-PAID-THRU-DT    TO DC-BIN-DATE-1                   CL*10
01543          MOVE  ' '                TO DC-OPTION-CODE                  CL*10
01544          PERFORM 9700-DATE-LINK THRU 9700-EXIT                       CL*10
01545          MOVE  DC-GREG-DATE-1-EDIT  TO LBL-PAID-THRU-DATE            CL*10
01546          MOVE  LBL-PRINT-LINE-5   TO WS-PASSED-DATA                  CL*10
01547          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                         CL*11
01548                                                                      CL*11
01549          MOVE  'X'                TO WS-PROG-END                     CL*11
01550          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                         CL*11
01551          GO TO 7299-EXIT.                                            CL*11
01552                                                                      CL*11
01553  7260-PRINT-RMC-LAP-PROG.                                            CL*14
01554                                                                      CL*11
01555      MOVE  '1'                TO WS-PASSED-CNTL-CHAR.                CL*11
01556      MOVE  SPACES             TO WS-PASSED-DATA.                     CL*11
01557      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                            CL*11
01558                                                                      CL*11
01559      MOVE  SPACES             TO WS-PASSED-DATA                      CL*11
01560                                  WS-PASSED-CNTL-CHAR.                CL*11
01561      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT 8 TIMES.                    CL*11
01562                                                                      CL*11
01563      MOVE  SPACES             TO RMC-LAP-PRINT-LINE-1.               CL*14
01564      MOVE  CLAM-CLAIM         TO RMC-LAP-CLAIM-NO.                   CL*14
01565      MOVE  CLAM-CERT          TO RMC-LAP-POLICY-NO.                  CL*14
01566      MOVE  RMC-LAP-PRINT-LINE-1 TO WS-PASSED-DATA.                   CL*14
01567      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                            CL*11
01568                                                                      CL*11
01569      MOVE  SPACES             TO WS-PASSED-DATA                      CL*11
01570                                  WS-PASSED-CNTL-CHAR.                CL*11
01571      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT 3 TIMES.                    CL*11
01572                                                                      CL*11
01573      MOVE  SPACES             TO RMC-LAP-PRINT-LINE-2.               CL*14
01574      MOVE  WS-NAME-WORK       TO RMC-LAP-INSURED-NAME.               CL*14
01575      MOVE  RMC-LAP-PRINT-LINE-2 TO WS-PASSED-DATA.                   CL*14
01576      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                            CL*11
01577                                                                      CL*11
01578      IF  WS-ADDR1-WORK     =    SPACES                               CL*11
01579          MOVE  SPACES             TO RMC-LAP-PRINT-LINE-3            CL*14
01580          MOVE  WS-ADDR2-WORK      TO RMC-LAP-INSURED-ADDR2           CL*14
01581          MOVE  RMC-LAP-PRINT-LINE-4 TO RMC-LAP-PRINT-LINE-3          CL*14
01582          MOVE  RMC-LAP-PRINT-LINE-3 TO WS-PASSED-DATA                CL*14
01583          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                         CL*11
01584                                                                      CL*11
01585          MOVE  SPACES             TO RMC-LAP-PRINT-LINE-5            CL*14
01586          MOVE  WS-CITY-STATE-WORK TO WS-LABEL-HOLD-AREA              CL*11
01587          MOVE  WS-ZIP-WORK        TO WS-ZIP-LABEL                    CL*11
01588          MOVE  WS-LABEL-HOLD-AREA TO RMC-LAP-INSURED-CITY-ST         CL*14
01589          MOVE  RMC-LAP-PRINT-LINE-5 TO RMC-LAP-PRINT-LINE-4          CL*14
01590          MOVE  RMC-LAP-PRINT-LINE-4 TO WS-PASSED-DATA                CL*14
01591          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                         CL*14
01592                                                                      CL*14
01593          MOVE  SPACES             TO WS-PASSED-DATA                  CL*14
01594                                      WS-PASSED-CNTL-CHAR             CL*14
01595          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT 37 TIMES                CL*15
01596                                                                      CL*14
01597          MOVE SPACES              TO RMC-LAP-PRINT-LINE-6            CL*14
01598          MOVE WS-SPEC-INST1       TO RMC-LAP-TEXT-LINE-1             CL*14
01599          MOVE  RMC-LAP-PRINT-LINE-6 TO WS-PASSED-DATA                CL*14
01600          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                         CL*14
01601                                                                      CL*14
01602          MOVE SPACES              TO RMC-LAP-PRINT-LINE-7            CL*14
01603          MOVE WS-SPEC-INST2       TO RMC-LAP-TEXT-LINE-2             CL*14
01604          MOVE  RMC-LAP-PRINT-LINE-7 TO WS-PASSED-DATA                CL*14
01605          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                         CL*14
01606                                                                      CL*14
01607          MOVE SPACES              TO RMC-LAP-PRINT-LINE-8            CL*14
01608          MOVE WS-SPEC-INST3       TO RMC-LAP-TEXT-LINE-3             CL*14
01609          MOVE  RMC-LAP-PRINT-LINE-8 TO WS-PASSED-DATA                CL*14
01610          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                         CL*11
01611                                                                      CL*11
01612          MOVE  'X'                TO WS-PROG-END                     CL*11
01613          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                         CL*11
01614          GO TO 7299-EXIT.                                            CL*11
01615                                                                      CL*11
01616      IF  WS-ADDR1-WORK  NOT =  SPACES   AND                          CL*11
01617          WS-ADDR2-WORK      =  SPACES                                CL*11
01618          MOVE  SPACES             TO RMC-LAP-PRINT-LINE-3            CL*14
01619          MOVE  WS-ADDR1-WORK      TO RMC-LAP-INSURED-ADDR1           CL*14
01620          MOVE  RMC-LAP-PRINT-LINE-3 TO WS-PASSED-DATA                CL*14
01621          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                         CL*11
01622                                                                      CL*11
01623          MOVE  SPACES             TO RMC-LAP-PRINT-LINE-5            CL*14
01624          MOVE  WS-CITY-STATE-WORK TO WS-LABEL-HOLD-AREA              CL*11
01625          MOVE  WS-ZIP-WORK        TO WS-ZIP-LABEL                    CL*11
01626          MOVE  WS-LABEL-HOLD-AREA TO RMC-LAP-INSURED-CITY-ST         CL*14
01627          MOVE  RMC-LAP-PRINT-LINE-5 TO RMC-LAP-PRINT-LINE-4          CL*14
01628          MOVE  RMC-LAP-PRINT-LINE-4 TO WS-PASSED-DATA                CL*14
01629          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                         CL*11
01630                                                                      CL*11
01631          MOVE  SPACES             TO WS-PASSED-DATA                  CL*14
01632                                      WS-PASSED-CNTL-CHAR             CL*14
01633          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT 37 TIMES                CL*15
01634                                                                      CL*14
01635          MOVE SPACES              TO RMC-LAP-PRINT-LINE-6            CL*14
01636          MOVE WS-SPEC-INST1       TO RMC-LAP-TEXT-LINE-1             CL*14
01637          MOVE  RMC-LAP-PRINT-LINE-6 TO WS-PASSED-DATA                CL*14
01638          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                         CL*14
01639                                                                      CL*14
01640          MOVE SPACES              TO RMC-LAP-PRINT-LINE-7            CL*14
01641          MOVE WS-SPEC-INST2       TO RMC-LAP-TEXT-LINE-2             CL*14
01642          MOVE  RMC-LAP-PRINT-LINE-7 TO WS-PASSED-DATA                CL*14
01643          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                         CL*14
01644                                                                      CL*14
01645          MOVE SPACES              TO RMC-LAP-PRINT-LINE-8            CL*14
01646          MOVE WS-SPEC-INST3       TO RMC-LAP-TEXT-LINE-3             CL*14
01647          MOVE  RMC-LAP-PRINT-LINE-8 TO WS-PASSED-DATA                CL*14
01648                                                                      CL*14
01649          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                         CL*14
01650          MOVE  'X'                TO WS-PROG-END                     CL*11
01651          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                         CL*11
01652          GO TO 7299-EXIT.                                            CL*11
01653                                                                      CL*11
01654      IF  WS-ADDR1-WORK   NOT =  SPACES AND                           CL*11
01655          WS-ADDR2-WORK   NOT =  SPACES                               CL*11
01656          MOVE  SPACES             TO RMC-LAP-PRINT-LINE-3            CL*14
01657          MOVE  WS-ADDR1-WORK      TO RMC-LAP-INSURED-ADDR1           CL*14
01658          MOVE  RMC-LAP-PRINT-LINE-3 TO WS-PASSED-DATA                CL*14
01659          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                         CL*11
01660                                                                      CL*11
01661          MOVE  SPACES             TO RMC-LAP-PRINT-LINE-4            CL*14
01662          MOVE  WS-ADDR2-WORK      TO RMC-LAP-INSURED-ADDR2           CL*14
01663          MOVE  RMC-LAP-PRINT-LINE-4 TO WS-PASSED-DATA                CL*14
01664          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                         CL*11
01665                                                                      CL*11
01666          MOVE  SPACES             TO RMC-LAP-PRINT-LINE-5            CL*14
01667          MOVE  WS-CITY-STATE-WORK TO WS-LABEL-HOLD-AREA              CL*11
01668          MOVE  WS-ZIP-WORK        TO WS-ZIP-LABEL                    CL*11
01669          MOVE  WS-LABEL-HOLD-AREA TO RMC-LAP-INSURED-CITY-ST         CL*14
01670          MOVE  RMC-LAP-PRINT-LINE-5 TO WS-PASSED-DATA                CL*14
01671          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                         CL*10
01672                                                                      CL*14
01673          MOVE  SPACES             TO WS-PASSED-DATA                  CL*14
01674                                      WS-PASSED-CNTL-CHAR             CL*14
01675          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT 37 TIMES                CL*15
01676                                                                      CL*14
01677          MOVE SPACES              TO RMC-LAP-PRINT-LINE-6            CL*14
01678          MOVE WS-SPEC-INST1       TO RMC-LAP-TEXT-LINE-1             CL*14
01679          MOVE  RMC-LAP-PRINT-LINE-6 TO WS-PASSED-DATA                CL*14
01680          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                         CL*14
01681                                                                      CL*14
01682          MOVE SPACES              TO RMC-LAP-PRINT-LINE-7            CL*14
01683          MOVE WS-SPEC-INST2       TO RMC-LAP-TEXT-LINE-2             CL*14
01684          MOVE  RMC-LAP-PRINT-LINE-7 TO WS-PASSED-DATA                CL*14
01685          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                         CL*14
01686                                                                      CL*14
01687          MOVE SPACES              TO RMC-LAP-PRINT-LINE-8            CL*14
01688          MOVE WS-SPEC-INST3       TO RMC-LAP-TEXT-LINE-3             CL*14
01689          MOVE  RMC-LAP-PRINT-LINE-8 TO WS-PASSED-DATA                CL*14
01690                                                                      CL*10
01691          MOVE  'X'                TO WS-PROG-END                     CL*10
01692          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                         CL*10
01693          GO TO 7299-EXIT.                                            CL*10
01694                                                                      CL*10
01695  7260-PRINT-PROG-CONT.                                               CL*10
01696                                                                      CL*10
01697      MOVE '1'                 TO WS-PASSED-CNTL-CHAR.                CL*10
01698      MOVE SPACES              TO WS-PASSED-DATA.                     CL*10
01699      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                            CL*10
01700                                                                      CL*10
01701      MOVE SPACES              TO WS-PASSED-DATA                      CL*10
01702                                  WS-PASSED-CNTL-CHAR.                CL*10
01703      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT 8 TIMES.                    CL*10
01704                                                                      CL*10
01705      MOVE SPACES              TO PROG-PRINT-LINE-1                   CL*10
01706                                  WS-PASSED-CNTL-CHAR.                CL*10
01707      MOVE WS-PRINT-SW         TO PROG-PRINT-SW.                      CL*10
01708      MOVE SAVE-DATE           TO PROG-PRINT-DATE.                    CL*10
01709      MOVE PROG-PRINT-LINE-1   TO WS-PASSED-DATA.                     CL*10
01710      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                            CL*10
01711                                                                      CL*10
01712      MOVE SPACES              TO WS-PASSED-DATA                      CL*10
01713                                  WS-PASSED-CNTL-CHAR.                CL*10
01714      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT 2 TIMES.                    CL*10
01715                                                                      CL*10
01716      MOVE SPACES              TO PROG-PRINT-LINE-2.                  CL*10
01717      MOVE WS-NAME-WORK        TO PROG-NAME-ADDR.                     CL*10
01718      MOVE PROG-PRINT-LINE-2   TO WS-PASSED-DATA.                     CL*10
01719      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                            CL*10
01720                                                                      CL*10
01721      MOVE WS-ADDR1-WORK       TO PROG-NAME-ADDR.                     CL*10
01722      MOVE WS-SPEC-INST1       TO PROG-SPEC-INST.                     CL*10
01723      MOVE PROG-PRINT-LINE-2   TO WS-PASSED-DATA.                     CL*10
01724      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                            CL*10
01725                                                                      CL*10
01726      MOVE WS-ADDR2-WORK       TO PROG-NAME-ADDR.                     CL*10
01727      MOVE WS-SPEC-INST2       TO PROG-SPEC-INST.                     CL*10
01728      MOVE PROG-PRINT-LINE-2   TO WS-PASSED-DATA.                     CL*10
01729      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                            CL*10
01730                                                                      CL*10
01731      MOVE WS-CITY-STATE-WORK  TO WS-LABEL-HOLD-AREA.                 CL*10
01732      MOVE SPACES              TO PROG-SPEC-INST.                     CL*10
01733      MOVE WS-ZIP-WORK         TO WS-ZIP-LABEL.                       CL*10
01734                                                                      CL*10
01735      MOVE WS-LABEL-HOLD-AREA  TO PROG-NAME-ADDR.                     CL*10
01736      MOVE WS-SPEC-INST3       TO PROG-SPEC-INST.                     CL*10
01737      MOVE PROG-PRINT-LINE-2   TO WS-PASSED-DATA.                     CL*10
01738      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                            CL*10
01739                                                                      CL*10
01740      MOVE SPACES              TO WS-PASSED-DATA.                     CL*10
01741      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT 5 TIMES.                    CL*10
01742                                                                      CL*10
01743      MOVE WS-SAVE-ACCT-NAME   TO PROG-ACCOUNT-NAME.                  CL*10
01744      MOVE WS-SAVE-CLAIM       TO PROG-CLAIM.                         CL*10
01745      MOVE WS-CARRIER (1)      TO PROG-CARRIER.                       CL*10
01746      MOVE WS-SAVE-MEMBER      TO PROG-MEMBER-NO .                    CL*10
01747      MOVE WS-SAVE-ACCT        TO WS-WORK-ACCOUNT.                    CL*10
01748      MOVE WS-WRK-ACCT         TO PROG-ACCOUNT-NO.                    CL*10
01749      MOVE PROG-PRINT-LINE-3   TO WS-PASSED-DATA.                     CL*10
01750      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                            CL*10
01751                                                                      CL*10
01752      MOVE 'X'                 TO WS-PROG-END.                        CL*10
CIDMOD*    PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                            CL*10
01754      GO TO 7299-EXIT.                                                CL*10
01755                                                                   EL1832
01756  7299-EXIT.                                                       EL1832
01757       EXIT.                                                       EL1832
01758      EJECT                                                        EL1832
01759  7300-FORMAT-INITIAL-DATA.                                        EL1832
01760      MOVE CM-AH-BENEFIT-CD       TO WS-BEN-CD.                    EL1832
01761      MOVE WS-ACCESS              TO CNTL-ACCESS.                  EL1832
01762      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.                 EL1832
01763      MOVE '5'                    TO CNTL-REC-TYPE.                EL1832
01764      MOVE +0                     TO CNTL-SEQ-NO.                     CL*18
01765                                                                   EL1832
01766                                                                   EL1832
01767      EXEC CICS HANDLE CONDITION                                   EL1832
01768          ENDFILE  (7399-EXIT)                                     EL1832
01769          NOTFND   (7399-EXIT)                                     EL1832
01770          END-EXEC.                                                EL1832
01771                                                                   EL1832
01772     EXEC CICS READ                                                EL1832
01773          DATASET  ('ELCNTL')                                      EL1832
01774          SET      (ADDRESS OF CONTROL-FILE)                          CL*17
01775          RIDFLD   (ELCNTL-KEY)                                    EL1832
01776          GTEQ                                                     EL1832
01777          END-EXEC.                                                EL1832
01778                                                                   EL1832
01779      IF (CNTL-COMP-ID  NOT = CF-COMPANY-ID)  OR                      CL*18
01780         (CNTL-REC-TYPE NOT = CF-RECORD-TYPE)                         CL*18
01781            GO TO 7399-EXIT.                                       EL1832
01782                                                                   EL1832
01783      PERFORM 7400-BENEFIT-DUMMY THRU 7400-DUMMY-EXIT              EL1832
01784          VARYING SUB-1 FROM 1 BY 1 UNTIL                          EL1832
01785             ((SUB-1 GREATER 8) OR                                 EL1832
01786             (CF-BENEFIT-CODE (SUB-1) = WS-BEN-CD)).                  CL**4
01787                                                                   EL1832
01788      IF SUB-1 NOT = 9                                             EL1832
01789         MOVE CF-BENEFIT-ALPHA (SUB-1) TO WS-BENEFIT-SAVE          EL1832
01790         IF WS-BEN-DAYS GREATER THAN WS-BEN-DAYS-HOLD              EL1832
01791            MOVE WS-BEN-DAYS      TO WS-BEN-DAYS-HOLD              EL1832
01792            MOVE SPACES           TO INIT-PRINT-LINE-1                CL*19
01793            MOVE WS-BEN-DAYS-HOLD TO INIT-ELIM-DAYS.               EL1832
01794                                                                   EL1832
01795  7399-EXIT.                                                       EL1832
01796                                                                   EL1832
01797  7400-BENEFIT-DUMMY.                                              EL1832
01798                                                                   EL1832
01799  7400-DUMMY-EXIT.                                                 EL1832
01800      EXIT.                                                        EL1832
01801                                                                   EL1832
01802      EJECT                                                        EL1832
01803  8000-READ-FORM-CONTROL.                                          EL1832
01804      EXEC CICS HANDLE CONDITION                                   EL1832
01805           NOTOPEN  (8870-ACTV-NOT-OPEN)                           EL1832
01806           NOTFND   (8050-REC-NOT-FOUND)                           EL1832
01807           END-EXEC.                                               EL1832
01808                                                                   EL1832
01809      EXEC CICS READ                                               EL1832
01810           DATASET  (ACTV-ID)                                      EL1832
01811           RIDFLD   (ACTV-KEY)                                     EL1832
01812           SET      (ADDRESS OF ACTIVITY-TRAILERS)                    CL*17
01813           UPDATE                                                  EL1832
01814           END-EXEC.                                               EL1832
01815                                                                   EL1832
01816      GO TO 8099-EXIT.                                             EL1832
01817                                                                   EL1832
01818  8050-REC-NOT-FOUND.                                              EL1832
01819      MOVE '1'                    TO FORM-CONTROL-SW.              EL1832
01820                                                                   EL1832
01821  8099-EXIT.                                                       EL1832
01822       EXIT.                                                       EL1832
01823      EJECT                                                        EL1832
01824  8100-READ-HEADER.                                                EL1832
01825      MOVE ARCH-NUMBER2           TO ARCH-NUMBER.                  EL1832
01826      MOVE '4'                    TO ARCH-REC-TYPE.                EL1832
01827      MOVE ZEROS                  TO ARCH-SEQ.                     EL1832
01828                                                                   EL1832
01829      EXEC CICS HANDLE CONDITION                                   EL1832
01830           NOTFND  (8150-NOT-FOUND)                                EL1832
01831           END-EXEC.                                               EL1832
01832                                                                   EL1832
01833      EXEC CICS READ                                               EL1832
01834           DATASET (ARCH-ID)                                       EL1832
01835           RIDFLD  (ARCH-KEY)                                      EL1832
01836           INTO    (LETTER-ARCHIVE)                                EL1832
01837           UPDATE                                                  EL1832
01838           END-EXEC.                                               EL1832
01839                                                                   EL1832
01840      MOVE LA4-CARRIER             TO ACTV-CARRIER.                EL1832
01841      MOVE LA4-CLAIM-NO            TO ACTV-CLAIM.                  EL1832
01842      MOVE LA4-CERT-NO             TO ACTV-CERT-NO.                EL1832
01843      MOVE LA4-FORM-TRLR-SEQ       TO ACTV-SEQ.                    EL1832
01844      GO TO 8199-EXIT.                                             EL1832
01845                                                                   EL1832
01846  8150-NOT-FOUND.                                                  EL1832
01847      MOVE '1'                    TO HEADER-SW.                    EL1832
01848                                                                   EL1832
01849  8199-EXIT.                                                       EL1832
01850       EXIT.                                                       EL1832
01851      EJECT                                                        EL1832
01852  8200-DELETE-ARCHIVE.                                             EL1832
01853      PERFORM 8100-READ-HEADER THRU 8199-EXIT.                     EL1832
01854                                                                   EL1832
01855      IF PI-COMPANY-ID  = 'FIA'                                    EL1832
01856         MOVE LA4-CARRIER            TO MSG-CARRIER                EL1832
01857         MOVE LA4-CLAIM-NO           TO MSG-CLAIM                  EL1832
01858         MOVE LA4-CERT-NO            TO MSG-CERT                   EL1832
01859         MOVE LA-ARCHIVE-NO          TO MSG-ARCH-NO                EL1832
01860         MOVE LA4-FORM-TYPE          TO MSG-TYPE                   EL1832
01861             EXEC CICS WRITEQ TD                                   EL1832
01862                  QUEUE     ('CSMT')                               EL1832
01863                  FROM      (MSG-MESSAGE)                          EL1832
01864                  LENGTH    (TRANS-DATA-LENGTH)                    EL1832
01865                  END-EXEC.                                        EL1832
01866                                                                   EL1832
01867      EXEC CICS DELETE                                             EL1832
01868           DATASET  (ARCH-ID)                                      EL1832
01869           END-EXEC.                                               EL1832
01870                                                                   EL1832
01871  8299-EXIT.                                                       EL1832
01872       EXIT.                                                       EL1832
01873      EJECT                                                        EL1832
01874  8850-FORM-TRLR-NOT-FND.                                          EL1832
01875      MOVE 'FORM TRAILERS NOT FOUND' TO ERROR-LINE.                EL1832
01876      PERFORM 400-SEND-TEXT.                                       EL1832
01877      EXEC CICS SYNCPOINT                                          EL1832
01878                ROLLBACK                                           EL1832
01879                END-EXEC.                                          EL1832
01880                                                                   EL1832
01881      GO TO 200-END-DATA.                                          EL1832
01882                                                                   EL1832
01883  8860-ARCH2-NOT-OPEN.                                             EL1832
01884      MOVE 'LETTER ARCHIVE FILE NOT OPEN - ELARCH2' TO ERROR-LINE. EL1832
01885      PERFORM 400-SEND-TEXT.                                       EL1832
01886      GO TO 200-END-DATA.                                          EL1832
01887                                                                   EL1832
01888  8870-ACTV-NOT-OPEN.                                              EL1832
01889      MOVE 'ACTIVITY TRAILER FILE NOT OPEN - ELTRLR' TO ERROR-LINE.EL1832
01890      PERFORM 400-SEND-TEXT.                                       EL1832
01891      GO TO 200-END-DATA.                                          EL1832
01892                                                                   EL1832
01893  8880-CLAM-NOT-OPEN.                                              EL1832
01894      MOVE 'CLAIM MASTER FILE NOT OPEN - ELMSTR' TO ERROR-LINE.    EL1832
01895      PERFORM 400-SEND-TEXT.                                       EL1832
01896      EXEC CICS SYNCPOINT                                          EL1832
01897                ROLLBACK                                           EL1832
01898                END-EXEC.                                          EL1832
01899                                                                   EL1832
01900      GO TO 200-END-DATA.                                          EL1832
01901                                                                   EL1832
CIDMOD 8890-ARCH-NOT-OPEN.                                              000
CIDMOD     MOVE 'LETTER ARCHIVE FILE NOT OPEN - ELARCH' TO ERROR-LINE.  000
CIDMOD     PERFORM 400-SEND-TEXT.                                       000
CIDMOD     GO TO 200-END-DATA.                                          000
CIDMOD                                                                  000
01902  9000-CERT-NOT-OPEN.                                              EL1832
01903      MOVE 'CERT MASTER FILE NOT OPEN - ELCERT' TO ERROR-LINE.     EL1832
01904      PERFORM 400-SEND-TEXT.                                       EL1832
01905      EXEC CICS SYNCPOINT                                          EL1832
01906                ROLLBACK                                           EL1832
01907                END-EXEC.                                          EL1832
01908                                                                   EL1832
01909      GO TO 200-END-DATA.                                          EL1832
01910                                                                   EL1832
01911  9100-ACCOUNT-NOT-OPEN.                                           EL1832
01912      MOVE 'ACCOUNT MASTER FILE NOT OPEN - ERACCT' TO ERROR-LINE.  EL1832
01913      PERFORM 400-SEND-TEXT.                                       EL1832
01914      EXEC CICS SYNCPOINT                                          EL1832
01915                ROLLBACK                                           EL1832
01916                END-EXEC.                                          EL1832
01917                                                                   EL1832
01918      GO TO 200-END-DATA.                                          EL1832
01919                                                                   EL1832
01920      EJECT                                                        EL1832
01921                                                                   EL1832
01922  9700-DATE-LINK.                                                  EL1832
01923      MOVE LINK-ELDATCV           TO PGM-NAME.                     EL1832
01924      EXEC CICS LINK                                               EL1832
01925          PROGRAM   (PGM-NAME)                                     EL1832
01926          COMMAREA  (DATE-CONVERSION-DATA)                         EL1832
01927          LENGTH    (DC-COMM-LENGTH)                               EL1832
01928          END-EXEC.                                                EL1832
01929                                                                   EL1832
01930  9700-EXIT.                                                       EL1832
01931       EXIT.                                                       EL1832
01932      EJECT                                                        EL1832
uktdel*9800-PRINT-ROUTINE.             COPY ELPRTCVP.                   EL1832
uktins 9800-PRINT-ROUTINE.
uktins     COPY ELPRTCVP.
01934                                                                   EL1832
01935                                                                   EL1832
