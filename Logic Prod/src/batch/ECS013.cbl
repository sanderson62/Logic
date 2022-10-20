00001  IDENTIFICATION DIVISION.                                         03/19/98
00002                                                                   ECS013
00003  PROGRAM-ID.                ECS013.                                  LV004
00004 *              PROGRAM CONVERTED BY                                  CL**2
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**2
00006 *              CONVERSION DATE 11/28/95 10:55:10.                    CL**2
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE               CL**2
00008 *                           VMOD=2.006.                              CL**2
00009                                                                      CL**2
00010 *AUTHOR.     LOGIC, INC.                                             CL**2
00011 *            DALLAS, TEXAS.                                          CL**2
00012                                                                      CL**2
00013 *DATE-COMPILED.                                                      CL**2
00014                                                                      CL**2
00015 *SECURITY.   *****************************************************   CL**2
00016 *            *                                                   *   CL**2
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**2
00018 *            *                                                   *   CL**2
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**2
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**2
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**2
00022 *            *                                                   *   CL**2
00023 *            *****************************************************   CL**2
00024                                                                      CL**2
00025 *REMARKS.                                                            CL**2
00026 *         CREDIT LIFE OF SPRINGFIELD INTERFACE PROGRAM.              CL**2
00027      EJECT                                                           CL**2
00028                                                                      CL**2
00029  ENVIRONMENT DIVISION.                                               CL**2
00030  CONFIGURATION SECTION.                                              CL**2
00031  SPECIAL-NAMES.                                                      CL**2
00032      C02 IS LCP-CH2                                                  CL**2
00033      C03 IS LCP-CH3                                                  CL**2
00034      C04 IS LCP-CH4                                                  CL**2
00035      C05 IS LCP-CH5                                                  CL**2
00036      C06 IS LCP-CH6                                                  CL**2
00037      C07 IS LCP-CH7                                                  CL**2
00038      C08 IS LCP-CH8                                                  CL**2
00039      C09 IS LCP-CH9                                                  CL**2
00040      C10 IS LCP-CH10                                                 CL**2
00041      C11 IS LCP-CH11                                                 CL**2
00042      C12 IS LCP-CH12                                                 CL**2
00043      S01 IS LCP-P01                                                  CL**2
00044      S02 IS LCP-P02.                                                 CL**2
00045  INPUT-OUTPUT SECTION.                                               CL**2
00046  FILE-CONTROL.                                                       CL**2
00047      SELECT  EXTRACT     ASSIGN TO SYS010-UT-2400-S-SYS010.          CL**2
00048      SELECT  DISK-DATE   ASSIGN TO SYS019-UT-FBA1-S-SYS019.          CL**2
00049      SELECT  PRINTER     ASSIGN TO SYS008-UR-1403-S-SYS008.          CL**2
00050      SELECT  PREM-TAPE   ASSIGN TO SYS011-UT-2400-S-SYS011.          CL**2
00051      SELECT  CLAIM-TAPE  ASSIGN TO SYS012-UT-2400-S-SYS012.          CL**2
00052      SELECT  SORT-FILE   ASSIGN TO SYS001-UT-FBA1-S-SORTWK1.         CL**2
00053      SELECT  FICH        ASSIGN TO SYS020-UT-2400-S-SYS020.          CL**2
00054      SELECT  ERACCTT                                                 CL**2
00055                          ASSIGN TO SYS015-FBA1-ERACCTT               CL**2
00056                          ORGANIZATION IS INDEXED                     CL**2
00057                          ACCESS IS SEQUENTIAL                        CL**2
00058                          RECORD KEY IS AM-CONTROL-PRIMARY            CL**2
00059                          FILE STATUS IS ERACCT-FILE-STATUS.          CL**2
00060                                                                      CL**2
00061  EJECT                                                               CL**2
00062  DATA DIVISION.                                                      CL**2
00063  FILE SECTION.                                                       CL**2
00064                                                                      CL**2
00065  FD  EXTRACT                                                         CL**2
00066                                  COPY ECSEXTFD.                      CL**2
00067                                  COPY ECSEXT01.                      CL**2
00068  EJECT                                                               CL**2
00069  FD  ERACCTT.                                                        CL**2
00070                                  COPY ERCACCT.                       CL**2
00071  EJECT                                                               CL**2
00072  FD  DISK-DATE                                                       CL**2
00073                                  COPY ELCDTEFD.                      CL**2
00074  EJECT                                                               CL**2
00075  FD  PRINTER                                                         CL**2
00076                                  COPY ELCPRTFD.                      CL**2
00077                                                                      CL**4
00078  FD  FICH                                                            CL**2
00079                                  COPY ELCFCHFD.                      CL**2
00080  EJECT                                                               CL**2
00081  FD  PREM-TAPE                                                       CL**2
00082      RECORDING MODE IS F.                                            CL**2
00083  01  PREMIUM-RECORD              PIC X(120).                         CL**2
00084                                                                      CL**4
00085  EJECT                                                               CL**2
00086  FD  CLAIM-TAPE                                                      CL**2
00087      RECORDING MODE IS F.                                            CL**2
00088  01  CLAIM-RECORD                PIC X(195).                         CL**2
00089                                                                      CL**4
00090  EJECT                                                               CL**2
00091  SD  SORT-FILE.                                                      CL**2
00092  01  SORT-RECORD.                                                    CL**2
00093      12  SORT-KEY.                                                   CL**2
00094          16  SORT-TYPE           PIC X.                              CL**2
00095              88  PREMIUMS                        VALUE '1'.          CL**2
00096              88  CLAIMS                          VALUE '2'.          CL**2
00097          16  SORT-ACCOUNT        PIC X(7).                           CL**2
00098          16  FILLER              PIC XX.                             CL**2
00099      12  SORT-PREMIUM-RECORD.                                        CL**2
00100          16  PREMIUM-FIELDS.                                         CL**2
00101              20  P-ACCOUNT.                                          CL**2
00102                  24  P-ACCT-1    PIC X.                              CL**2
00103                  24  P-ACCT-NO   PIC X(6).                           CL**2
00104              20  FILLER          PIC X(6).                           CL**2
00105              20  P-ACCTNG-DTE    PIC 9(11)  COMP-3.                  CL**2
00106              20  FILLER          PIC X(11).                          CL**2
00107              20  P-LIFE-TYPE     PIC XX.                             CL**2
00108              20  P-AH-TYPE       PIC XX.                             CL**2
00109              20  FILLER          PIC X(8).                           CL**2
00110              20  P-FORM-NO       PIC X(7).                           CL**2
00111              20  P-NUM-INS       PIC 9.                              CL**2
00112              20  P-NAME          PIC X(11).                          CL**2
00113              20  P-EFFECT-DTE  COMP-3.                               CL**2
00114                  24  FILLER      PIC 999.                            CL**2
00115                  24  P-E-CC      PIC 99.                             CL**2
00116                  24  P-E-YR      PIC 99.                             CL**2
00117                  24  P-E-MO      PIC 99.                             CL**2
00118                  24  P-E-DA      PIC 99.                             CL**2
00119              20  P-AGE           PIC 99.                             CL**2
00120              20  P-TERM          PIC 999.                            CL**2
00121              20  P-ORIG-AMT      PIC S9(5).                          CL**2
00122              20  P-LIFE-PREM     PIC S9(4)V99.                       CL**2
00123              20  P-MTHLY-BEN     PIC S999V99.                        CL**2
00124              20  P-AH-PREM       PIC S9(4)V99.                       CL**2
00125              20  P-CANCEL-DTE  COMP-3.                               CL**2
00126                  24  FILLER      PIC 999.                            CL**2
00127                  24  P-C-CC      PIC 99.                             CL**2
00128                  24  P-C-YR      PIC 99.                             CL**2
00129                  24  P-C-MO      PIC 99.                             CL**2
00130                  24  P-C-DA      PIC 99.                             CL**2
00131              20  P-LIFE-REFUND   PIC S9(4)V99.                       CL**2
00132              20  P-AH-REFUND     PIC S9(4)V99.                       CL**2
00133              20  P-CERT-NO       PIC X(7).                           CL**2
00134              20  P-TYPE          PIC X.                              CL**2
00135                  88  ISSUE                       VALUE '1'.          CL**2
00136                  88  CANCELS                     VALUE '2'.          CL**2
00137          16  FILLER              PIC X(75).                          CL**2
00138                                                                      CL**2
00139      12  SORT-CLAIM-RECORD REDEFINES SORT-PREMIUM-RECORD.            CL**2
00140        16  CLAIM-FIELDS.                                             CL**2
00141              20  C-CLAIM-NO      PIC X(6).                           CL**2
00142              20  C-NAME          PIC X(21).                          CL**2
00143              20  FILLER          PIC X.                              CL**2
00144              20  C-CERT-NO       PIC X(8).                           CL**2
00145              20  C-AGE           PIC 99.                             CL**2
00146              20  C-SEX           PIC X.                              CL**2
00147              20  C-EFFECT-DTE    PIC 9(11)   COMP-3.                 CL**2
00148              20  C-TERM          PIC 999.                            CL**2
00149              20  C-ACCOUNT.                                          CL**2
00150                  24  C-ACCT-1    PIC X.                              CL**2
00151                  24  C-ACCT-NO   PIC X(6).                           CL**2
00152              20  C-INDEBT        PIC X.                              CL**2
00153              20  C-CREDITOR      PIC X.                              CL**2
00154              20  C-RATE          PIC 99V99.                          CL**2
00155              20  C-STATE         PIC XX.                             CL**2
00156              20  C-FORM-NO       PIC X(7).                           CL**2
00157              20  C-COV-CODE.                                         CL**2
00158                  24  C-COV-1     PIC X.                              CL**2
00159                  24  C-COV-2     PIC X.                              CL**2
00160              20  FILLER          PIC XXX.                            CL**2
00161              20  C-CAUSE         PIC XX.                             CL**2
00162              20  C-LOSS-DTE      PIC 9(11)    COMP-3.                CL**2
00163              20  C-REPORTED-DTE.                                     CL**2
00164                  24  C-R-YR      PIC 99.                             CL**2
00165                  24  C-R-MO      PIC 99.                             CL**2
00166                  24  C-R-DA      PIC 99.                             CL**2
00167              20  C-PAYEE         PIC X.                              CL**2
00168              20  C-TYPE          PIC X.                              CL**2
00169                  88  LIFE                        VALUE '1'.          CL**2
00170                  88  DISABILITY                  VALUE '2'.          CL**2
00171              20  C-ADJUST        PIC X.                              CL**2
00172              20  FILLER          PIC XX.                             CL**2
00173              20  C-CHECK-NO      PIC X(5).                           CL**2
00174              20  C-PAID-DTE      PIC 9(11)   COMP-3.                 CL**2
00175              20  C-AMOUNT-PAID   PIC S9(5)V99.                       CL**2
00176              20  C-INS-ADDRESS   PIC X(25).                          CL**2
00177              20  C-INS-CITY      PIC X(17).                          CL**2
00178              20  C-INS-STATE     PIC XXX.                            CL**2
00179              20  C-INS-ZIP       PIC X(5).                           CL**2
00180              20  C-LIFE-CLAIM.                                       CL**2
00181                  24  C-ORIG-AMT  PIC S9(5)V99.                       CL**2
00182                  24  C-UNPD-BAL  PIC S9(5)V99.                       CL**2
00183                  24  C-DELNQ-AMT PIC S9(5).                          CL**2
00184                  24  C-ORIG-PREM PIC S9(5)V99.                       CL**2
00185                  24  FILLER      PIC X(7).                           CL**2
00186              20  C-AH-CLAIM REDEFINES C-LIFE-CLAIM.                  CL**2
00187                  24  C-MTH-BEN   PIC S999V99.                        CL**2
00188                  24  C-RESERVE   PIC S9(5).                          CL**2
00189                  24  C-FROM-DTE.                                     CL**2
00190                      28  C-F-YR  PIC 99.                             CL**2
00191                      28  C-F-MO  PIC 99.                             CL**2
00192                      28  C-F-DA  PIC 99.                             CL**2
00193                  24  C-TO-DTE    PIC 9(11)  COMP-3.                  CL**2
00194                  24  C-PMT-NO    PIC 99.                             CL**2
00195                  24  C-DAYS-DIS  PIC 999.                            CL**2
00196                  24  C-ELIM-CD   PIC 9.                              CL**2
00197                  24  C-PMT-TYPE  PIC X.                              CL**2
00198                  24  FILLER      PIC X(4).                           CL**2
00199  EJECT                                                               CL**2
00200  WORKING-STORAGE SECTION.                                            CL**2
00201  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.         CL**2
00202  77  LCP-ASA                       PIC X.                            CL**2
00203  77  FILLER  PIC X(32) VALUE '********************************'.     CL**2
00204  77  FILLER  PIC X(32) VALUE '    ECS013  WORKING STORAGE     '.     CL**2
00205  77  FILLER  PIC X(32) VALUE '********VMOD=2.006**************'.     CL**2
00206                                                                      CL**2
00207                                  COPY ELCDTECX.                      CL**2
00208                                                                      CL**2
00209                                  COPY ELCDTEVR.                      CL**2
00210                                                                      CL**3
00211                                  COPY ELCEXTVR.                      CL**3
00212                                                                      CL**2
00213  01  MISC-WK-STORAGE.                                                CL**2
00214      12  X                       PIC X           VALUE SPACE.        CL**2
00215      12  WS-RETURN-CODE          PIC S9(4)       VALUE +0  COMP.     CL**2
00216      12  WS-ABEND-MESSAGE        PIC X(80)       VALUE SPACES.       CL**2
00217      12  WS-ABEND-FILE-STATUS    PIC XX          VALUE ZEROS.        CL**2
00218      12  WS-ZERO                 PIC S9          VALUE +0  COMP-3.   CL**2
00219      12  ERACCT-FILE-STATUS      PIC XX          VALUE ZEROS.        CL**2
00220      12  PGM-SUB                 PIC S999        VALUE +13 COMP-3.   CL**2
00221      12  LINE-CT                 PIC S99         VALUE +0  COMP-3.   CL**2
00222      12  PAGE-CT                 PIC S9(5)       VALUE +0  COMP-3.   CL**2
00223      12  WS-CHECK.                                                   CL**2
00224          16  FILLER              PIC XX.                             CL**2
00225          16  WS-CHK              PIC X(5).                           CL**2
00226      12  WS-CERT.                                                    CL**2
00227          16  WS-CERT-PRIME       PIC X(7).                           CL**2
00228          16  WS-CERT-SUF         PIC X.                              CL**2
00229      12  SPLIT-CLAIM.                                                CL**2
00230          16  S-CLM-1             PIC X.                              CL**2
00231          16  S-CLM-NO.                                               CL**2
00232              20  S-CLM-NO-1      PIC X.                              CL**2
00233              20  S-CLM-NO-2      PIC X.                              CL**2
00234              20  S-CLM-NO-3      PIC X.                              CL**2
00235              20  FILLER          PIC XXX.                            CL**2
00236      12  PREV-TYPE               PIC X           VALUE SPACES.       CL**2
00237      12  PREV-ACCOUNT            PIC X(6)        VALUE SPACES.       CL**2
00238      12  PREM-RECORDS            PIC 9(6)        VALUE ZERO.         CL**2
00239      12  CLM-RECORDS             PIC 9(6)        VALUE ZERO.         CL**2
00240      12  C-EFFECT-DTE-R.                                             CL**2
00241          16  FILLER              PIC 999.                            CL**2
00242          16  C-E-CC              PIC 99.                             CL**2
00243          16  4-E-YR              PIC 99.                             CL**2
00244          16  C-E-MO              PIC 99.                             CL**2
00245          16  C-E-DA              PIC 99.                             CL**2
00246      12  C-TO-DTE-R.                                                 CL**2
00247          16  FILLER              PIC 999.                            CL**2
00248          16  C-T-CC              PIC 99.                             CL**2
00249          16  C-T-YR              PIC 99.                             CL**2
00250          16  C-T-MO              PIC 99.                             CL**2
00251          16  C-T-DA              PIC 99.                             CL**2
00252      12  P-ACCTNG-DTE-R.                                             CL**2
00253          16  FILLER              PIC 999.                            CL**2
00254          16  P-A-CC              PIC 99.                             CL**2
00255          16  P-A-YR              PIC 99.                             CL**2
00256          16  P-A-MO              PIC 99.                             CL**2
00257          16  P-A-DA              PIC 99.                             CL**2
00258      12  C-LOSS-DTE-R.                                               CL**2
00259          16  FILLER              PIC 999.                            CL**2
00260          16  C-L-CC              PIC 99.                             CL**2
00261          16  C-L-YR              PIC 99.                             CL**2
00262          16  C-L-MO              PIC 99.                             CL**2
00263          16  C-L-DA              PIC 99.                             CL**2
00264      12  C-PAID-DTE-R.                                               CL**2
00265          16  FILLER              PIC 999.                            CL**2
00266          16  C-P-CC              PIC 99.                             CL**2
00267          16  C-P-YR              PIC 99.                             CL**2
00268          16  C-P-MO              PIC 99.                             CL**2
00269          16  C-P-DA              PIC 99.                             CL**2
00270                                                                      CL**2
00271  01  ACCOUNT-TOTALS.                                                 CL**2
00272      12  A-LIFE-ISS              PIC S9(9)V99    VALUE ZERO.         CL**2
00273      12  A-LIFE-CAN              PIC S9(9)V99    VALUE ZERO.         CL**2
00274      12  A-AH-ISS                PIC S9(9)V99    VALUE ZERO.         CL**2
00275      12  A-AH-CAN                PIC S9(9)V99    VALUE ZERO.         CL**2
00276      12  A-LIFE-CLM              PIC S9(9)V99    VALUE ZERO.         CL**2
00277      12  A-AH-CLM                PIC S9(9)V99    VALUE ZERO.         CL**2
00278                                                                      CL**2
00279  01  TYPE-TOTALS.                                                    CL**2
00280      12  T-LIFE-ISS              PIC S9(9)V99    VALUE ZERO.         CL**2
00281      12  T-LIFE-CAN              PIC S9(9)V99    VALUE ZERO.         CL**2
00282      12  T-AH-ISS                PIC S9(9)V99    VALUE ZERO.         CL**2
00283      12  T-AH-CAN                PIC S9(9)V99    VALUE ZERO.         CL**2
00284      12  T-LIFE-CLM              PIC S9(9)V99    VALUE ZERO.         CL**2
00285      12  T-AH-CLM                PIC S9(9)V99    VALUE ZERO.         CL**2
00286  EJECT                                                               CL**2
00287  01  HEADINGS.                                                       CL**2
00288      12  HEAD-1.                                                     CL**2
00289          16  FILLER              PIC X(53)       VALUE SPACES.       CL**2
00290          16  FILLER              PIC X(17)       VALUE               CL**2
00291              'INTERFACE LISTING'.                                    CL**2
00292          16  FILLER              PIC X(50)       VALUE SPACES.       CL**2
00293          16  FILLER              PIC X(8)        VALUE 'ECS013  '.   CL**2
00294      12  HEAD-2.                                                     CL**2
00295          16  FILLER              PIC X(47)       VALUE SPACES.       CL**2
00296          16  H1-CMP              PIC X(30).                          CL**2
00297          16  FILLER              PIC X(43)       VALUE SPACES.       CL**2
00298          16  H2-IPL              PIC X(8).                           CL**2
00299      12  HEAD-3.                                                     CL**2
00300          16  FILLER              PIC X(53)       VALUE SPACES.       CL**2
00301          16  H3-DATE             PIC X(18).                          CL**2
00302          16  FILLER              PIC X(41)       VALUE SPACES.       CL**2
00303          16  FILLER              PIC X(5)        VALUE 'PAGE '.      CL**2
00304          16  H1-PAGE             PIC ZZ,ZZZ.                         CL**2
00305      12  HEAD-4.                                                     CL**2
00306          16  FILLER              PIC X(17)       VALUE SPACES.       CL**2
00307          16  FILLER              PIC X(30)                           CL**2
00308              VALUE '----------- LIFE -----------  '.                 CL**2
00309          16  FILLER              PIC X(30)                           CL**2
00310              VALUE '-------- DISABILITY --------  '.                 CL**2
00311          16  FILLER              PIC X(55)       VALUE SPACES.       CL**2
00312      12  HEAD-5A.                                                    CL**2
00313          16  FILLER              PIC X(17)       VALUE SPACES.       CL**2
00314          16  FILLER              PIC X(30)                           CL**2
00315              VALUE '     ISSUES        CANCELS    '.                 CL**2
00316          16  FILLER              PIC X(30)                           CL**2
00317              VALUE '     ISSUES        CANCELS    '.                 CL**2
00318          16  FILLER              PIC X(55)       VALUE SPACES.       CL**2
00319      12  HEAD-5B.                                                    CL**2
00320          16  FILLER              PIC X(17)       VALUE SPACES.       CL**2
00321          16  FILLER              PIC X(30)                           CL**2
00322              VALUE '     CLAIMS                   '.                 CL**2
00323          16  FILLER              PIC X(30)                           CL**2
00324              VALUE '     CLAIMS                   '.                 CL**2
00325          16  FILLER              PIC X(55)       VALUE SPACES.       CL**2
00326  01  DETAIL-LINE.                                                    CL**2
00327      12  FILLER                  PIC X(5)        VALUE SPACES.       CL**2
00328      12  DTL-ACCOUNT             PIC X(7).                           CL**2
00329      12  FILLER                  PIC X(4)        VALUE SPACES.       CL**2
00330      12  DTL-AMOUNT              PIC ZZZ,ZZZ,ZZZ.99-                 CL**2
00331                                  OCCURS 4 TIMES.                     CL**2
00332      12  FILLER                  PIC X(56)       VALUE SPACES.       CL**2
00333  EJECT                                                               CL**2
00334  PROCEDURE DIVISION.                                                 CL**2
00335  0100-DATE-READ.                                                     CL**2
00336                                  COPY ELCDTERX.                      CL**2
00337                                                                      CL**2
00338  0110-SORT-ROUTINE.                                                  CL**2
00339      OPEN  INPUT   EXTRACT                                           CL**2
00340                    ERACCTT.                                          CL**2
00341                                                                      CL**2
00342      IF ERACCT-FILE-STATUS  = '00' OR '97'                           CL**2
00343          NEXT SENTENCE                                               CL**2
00344        ELSE                                                          CL**2
00345          MOVE +0302                      TO  WS-RETURN-CODE          CL**2
00346          MOVE 'ERROR OPENING ACCT MSTR'  TO  WS-ABEND-MESSAGE        CL**2
00347          GO TO ABEND-PGM.                                            CL**2
00348                                                                      CL**2
00349      SORT SORT-FILE                                                  CL**2
00350              ASCENDING KEY    SORT-TYPE                              CL**2
00351                               SORT-ACCOUNT                           CL**2
00352          INPUT PROCEDURE IS SORT-INTERFACE-RECORDS                   CL**2
00353          OUTPUT PROCEDURE IS PRINT-INTERFACE-REPORT.                 CL**2
00354                                                                      CL**2
00355      IF SORT-RETURN NOT = ZEROES                                     CL**2
00356          MOVE +0101                    TO WS-RETURN-CODE             CL**2
00357          MOVE 'INTERNAL SORT ABORTED'  TO  WS-ABEND-MESSAGE          CL**2
00358          GO TO ABEND-PGM.                                            CL**2
00359                                                                      CL**2
00360      GOBACK.                                                         CL**2
00361  EJECT                                                               CL**2
00362  SORT-INTERFACE-RECORDS SECTION.                                     CL**2
00363                                                                      CL**2
00364  0200-READ-ACCOUNT.                                                  CL**2
00365      READ  ERACCTT.                                                  CL**2
00366                                                                      CL**2
00367      IF ERACCT-FILE-STATUS = '10'                                    CL**2
00368          MOVE HIGH-VALUES              TO  AM-CONTROL-PRIMARY.       CL**2
00369                                                                      CL**2
00370      IF ERACCT-FILE-STATUS  = '00' OR '97'                           CL**2
00371          NEXT SENTENCE                                               CL**2
00372        ELSE                                                          CL**2
00373          MOVE +0302                      TO  WS-RETURN-CODE          CL**2
00374          MOVE 'ERROR READING ACCT MSTR'  TO  WS-ABEND-MESSAGE        CL**2
00375          GO TO ABEND-PGM.                                            CL**2
00376                                                                      CL**2
00377  0200-R-A-X.                                                         CL**2
00378      EXIT.                                                           CL**2
00379                                                                      CL**2
00380  0210-READ-EXTRACT.                                                  CL**2
00381      READ EXTRACT                                                    CL**2
00382          AT END GO TO 0590-CLOSE-FILES.                              CL**2
00383                                                                      CL**2
00384      IF DE-REIN = 'R'                                                CL**2
00385          GO TO 0210-READ-EXTRACT.                                    CL**2
00386                                                                      CL**2
00387      IF DE-ISSUE  OR                                                 CL**2
00388         DE-CANCEL OR                                                 CL**2
00389         DE-CLAIM                                                     CL**2
00390            NEXT SENTENCE                                             CL**2
00391        ELSE                                                          CL**2
00392            GO TO 0210-READ-EXTRACT.                                  CL**2
00393                                                                      CL**2
00394       COPY ELCEXTM1.                                                 CL**2
00395                                                                      CL**2
00396  0220-SPECIAL-CLIENT-CODING.                                         CL**2
00397      IF DTE-CLIENT = 'ITY'                                           CL**2
00398          IF DE-CARRIER NOT = '2'                                     CL**2
00399              GO TO 0210-READ-EXTRACT.                                CL**2
00400                                                                      CL**2
00401      IF DTE-CLIENT = 'MLI'                                           CL**2
00402          IF DE-CARRIER NOT = '4'                                     CL**2
00403              GO TO 0210-READ-EXTRACT                                 CL**2
00404          ELSE                                                        CL**2
00405              IF DE-ISSUE   AND  DE-CRT-SUF = 'X'                     CL**2
00406                  GO TO 0210-READ-EXTRACT.                            CL**2
00407                                                                      CL**2
00408  0230-FIND-ACCOUNT.                                                  CL**2
00409      IF DE-COMPANY-CD = AM-COMPANY-CD                                CL**2
00410          GO TO 0240-CHECK-STATE.                                     CL**2
00411                                                                      CL**2
00412      IF DE-COMPANY-CD LESS AM-COMPANY-CD                             CL**2
00413              DISPLAY 'INVALID COMPANY CODE  ' DE-COMPANY-CD          CL**2
00414              MOVE +0301                      TO  WS-RETURN-CODE      CL**2
00415              MOVE 'ERROR READING EXTR FILE'  TO  WS-ABEND-MESSAGE    CL**2
00416              GO TO ABEND-PGM.                                        CL**2
00417                                                                      CL**2
00418      PERFORM 0200-READ-ACCOUNT THRU 0200-R-A-X.                      CL**2
00419      GO TO 0230-FIND-ACCOUNT.                                        CL**2
00420                                                                      CL**2
00421  0240-CHECK-STATE.                                                   CL**2
00422      IF DE-STATE = AM-STATE                                          CL**2
00423          GO TO 0250-CHECK-ACCT.                                      CL**2
00424                                                                      CL**2
00425      IF DE-STATE LESS AM-STATE                                       CL**2
00426              DISPLAY 'INVALID  STATE CODE  '  DE-STATE               CL**2
00427              MOVE +0301                TO  WS-RETURN-CODE            CL**2
00428              MOVE 'INVALID STATE CODE' TO  WS-ABEND-MESSAGE          CL**2
00429              GO TO ABEND-PGM.                                        CL**2
00430                                                                      CL**2
00431      PERFORM 0200-READ-ACCOUNT THRU 0200-R-A-X.                      CL**2
00432      GO TO 0230-FIND-ACCOUNT.                                        CL**2
00433                                                                      CL**2
00434  0250-CHECK-ACCT.                                                    CL**2
00435      IF DE-ACCOUNT = AM-ACCOUNT                                      CL**2
00436          GO TO 0260-CHECK-DATE.                                      CL**2
00437                                                                      CL**2
00438      IF DE-ACCOUNT LESS AM-ACCOUNT                                   CL**2
00439              DISPLAY 'INVALID ACCOUNT NUMBER  '  DE-ACCOUNT          CL**2
00440              MOVE +0301                     TO  WS-RETURN-CODE       CL**2
00441              MOVE 'INVALID ACCOUNT NUMBER'  TO  WS-ABEND-MESSAGE     CL**2
00442              GO TO ABEND-PGM.                                        CL**2
00443                                                                      CL**2
00444      PERFORM 0200-READ-ACCOUNT THRU 0200-R-A-X.                      CL**2
00445      GO TO 0230-FIND-ACCOUNT.                                        CL**2
00446                                                                      CL**2
00447  0260-CHECK-DATE.                                                    CL**2
00448      IF DE-EFF LESS AM-EXPIRE-DT                                     CL**2
00449          GO TO 0270-ACCT-FOUND.                                      CL**2
00450                                                                      CL**2
00451      PERFORM 0200-READ-ACCOUNT THRU 0200-R-A-X.                      CL**2
00452      GO TO 0230-FIND-ACCOUNT.                                        CL**2
00453                                                                      CL**2
00454  0270-ACCT-FOUND.                                                    CL**2
00455      EXIT.                                                           CL**2
00456      EJECT                                                           CL**2
00457  0300-PREMIUM-EXTRACT.                                               CL**2
00458      IF DE-CLAIM                                                     CL**2
00459          GO TO 0400-CLAIM-EXTRACT.                                   CL**2
00460                                                                      CL**2
00461      MOVE SPACES                 TO SORT-RECORD.                     CL**2
00462      MOVE ZEROS                  TO P-ACCT-1      P-ORIG-AMT         CL**2
00463                                     P-MTHLY-BEN   P-CANCEL-DTE       CL**2
00464                                     P-LIFE-PREM   P-AH-PREM          CL**2
00465                                     P-NUM-INS     P-LIFE-REFUND      CL**2
00466                                     P-AH-REFUND.                     CL**2
00467      MOVE '1'                    TO SORT-TYPE.                       CL**2
00468      MOVE DE-ACCT-PRIME          TO P-ACCT-NO.                       CL**2
00469      MOVE P-ACCOUNT              TO SORT-ACCOUNT.                    CL**2
00470      MOVE RUN-DATE               TO P-ACCTNG-DTE                     CL**2
00471                                     P-ACCTNG-DTE-R.                  CL**2
00472      MOVE DE-LF-TYPE             TO P-LIFE-TYPE.                     CL**2
00473                                                                      CL**4
00474      IF DE-LF-TYPE = ZERO                                            CL**2
00475          MOVE 1                  TO P-NUM-INS                        CL**2
00476      ELSE                                                            CL**2
00477          MOVE CLAS-STARTL        TO CLAS-INDEXL                      CL**2
00478          PERFORM 0350-FIND-NUM-INSUREDS THRU 0359-NUM-INS-X.         CL**2
00479                                                                      CL**4
00480      MOVE DE-AH-TYPE             TO P-AH-TYPE.                       CL**2
00481      MOVE DE-LNAME               TO P-NAME.                          CL**2
00482      MOVE DE-EF-MO               TO P-E-MO.                          CL**2
00483      MOVE DE-EF-DA               TO P-E-DA.                          CL**2
00484      MOVE DE-EF-CC               TO P-E-CC.                          CL**2
00485      MOVE DE-EF-YR               TO P-E-YR.                          CL**2
00486      MOVE DE-AGE                 TO P-AGE.                           CL**2
00487                                                                      CL**4
00488      IF DE-LF-TERM IS GREATER THAN DE-AH-TERM                        CL**2
00489          MOVE DE-LF-TERM         TO P-TERM                           CL**2
00490      ELSE                                                            CL**2
00491          MOVE DE-AH-TERM         TO P-TERM.                          CL**2
00492                                                                      CL**4
00493      MOVE DE-LF-BEN              TO P-ORIG-AMT.                      CL**2
00494      MOVE DE-LF-PRM              TO P-LIFE-PREM.                     CL**2
00495      MOVE DE-AH-BEN              TO P-MTHLY-BEN.                     CL**2
00496      MOVE DE-AH-PRM              TO P-AH-PREM.                       CL**2
00497      MOVE DE-CRT-PRIME           TO P-CERT-NO.                       CL**2
00498      MOVE '1'                    TO P-TYPE.                          CL**2
00499                                                                      CL**2
00500      IF DE-CANCEL                                                    CL**2
00501          MOVE '2'                TO P-TYPE                           CL**2
00502          MOVE DE-LF-RFND         TO P-LIFE-REFUND                    CL**2
00503          MOVE DE-AH-RFND         TO P-AH-REFUND                      CL**2
00504          IF DE-LF-CANC-DTE = ZEROS                                   CL**2
00505              MOVE DE-AH-CANC-DTE TO P-CANCEL-DTE                     CL**2
00506          ELSE                                                        CL**2
00507              MOVE DE-LF-CANC-DTE TO P-CANCEL-DTE.                    CL**2
00508                                                                      CL**2
00509      GO TO 0500-EXTRACT-RELEASE.                                     CL**2
00510                                                                      CL**2
00511  0350-FIND-NUM-INSUREDS.                                             CL**2
00512      IF CLAS-I-BEN (CLAS-INDEXL) NOT = DE-LF-TYPE                    CL**2
00513          IF CLAS-INDEXL NOT = CLAS-MAXL                              CL**2
00514              ADD +1              TO CLAS-INDEXL                      CL**2
00515              GO TO 0350-FIND-NUM-INSUREDS                            CL**2
00516          ELSE                                                        CL**2
00517              MOVE 1              TO P-NUM-INS                        CL**2
00518              GO TO 0359-NUM-INS-X.                                   CL**2
00519                                                                      CL**4
00520      IF CLAS-I-JOINT (CLAS-INDEXL) = 'J'                             CL**2
00521          MOVE 2                  TO P-NUM-INS                        CL**2
00522      ELSE                                                            CL**2
00523          MOVE 1                  TO P-NUM-INS.                       CL**2
00524                                                                      CL**4
00525  0359-NUM-INS-X.                                                     CL**2
00526      EXIT.                                                           CL**2
00527  EJECT                                                               CL**2
00528  0400-CLAIM-EXTRACT.                                                 CL**2
00529      MOVE SPACES                 TO SORT-RECORD.                     CL**2
00530      MOVE '2'                    TO SORT-TYPE.                       CL**2
00531      MOVE DE-NAME                TO C-NAME.                          CL**2
00532      MOVE DE-CRT-PRIME           TO WS-CERT-PRIME.                   CL**2
00533      MOVE DE-CRT-SUF             TO WS-CERT-SUF.                     CL**2
00534      MOVE WS-CERT                TO C-CERT-NO.                       CL**2
00535      MOVE DE-AGE                 TO C-AGE.                           CL**2
00536      MOVE DE-SEX                 TO C-SEX.                           CL**2
00537      MOVE DE-EFF                 TO C-EFFECT-DTE                     CL**2
00538                                     C-EFFECT-DTE-R.                  CL**2
00539      MOVE ZERO                   TO C-ACCT-1.                        CL**2
00540      MOVE DE-ACCT-PRIME          TO C-ACCT-NO.                       CL**2
00541      MOVE C-ACCOUNT              TO SORT-ACCOUNT.                    CL**2
00542      MOVE DE-STATE               TO C-STATE.                         CL**2
00543      MOVE DE-CLM-CAUSE           TO C-CAUSE.                         CL**2
00544      MOVE DE-INCUR-CC            TO C-L-CC.                          CL**2
00545      MOVE DE-INCUR-YR            TO C-L-YR.                          CL**2
00546      MOVE DE-INCUR-MO            TO C-L-MO.                          CL**2
00547      MOVE DE-INCUR-DA            TO C-L-DA.                          CL**2
00548      MOVE C-LOSS-DTE-R           TO C-LOSS-DTE.                      CL**2
00549      MOVE DE-RPT-YR              TO C-R-YR.                          CL**2
00550      MOVE DE-RPT-MO              TO C-R-MO.                          CL**2
00551      MOVE DE-RPT-DA              TO C-R-DA.                          CL**2
00552      MOVE '1'                    TO C-PAYEE.                         CL**2
00553                                                                      CL**4
00554      IF DE-TYPE = '1' OR '3'                                         CL**2
00555          MOVE '1'                TO C-TYPE                           CL**2
00556      ELSE                                                            CL**2
00557          MOVE '2'                TO C-TYPE.                          CL**2
00558                                                                      CL**4
00559      MOVE DE-CNUM                TO SPLIT-CLAIM.                     CL**2
00560                                                                      CL**4
00561      IF DTE-CLIENT = 'MLI'                                           CL**2
00562          IF LIFE                                                     CL**2
00563               MOVE S-CLM-NO-2    TO S-CLM-NO-1                       CL**2
00564               MOVE S-CLM-NO-3    TO S-CLM-NO-2                       CL**2
00565               MOVE ZERO          TO S-CLM-NO-3                       CL**2
00566           ELSE                                                       CL**2
00567               MOVE S-CLM-NO-2    TO S-CLM-NO-1                       CL**2
00568               MOVE ZERO          TO S-CLM-NO-2.                      CL**2
00569                                                                      CL**4
00570      MOVE S-CLM-NO               TO C-CLAIM-NO.                      CL**2
00571      MOVE DE-CHECK               TO WS-CHECK.                        CL**2
00572      MOVE WS-CHK                 TO C-CHECK-NO.                      CL**2
00573      MOVE DE-PAY-CC              TO C-P-CC.                          CL**2
00574      MOVE DE-PAY-YR              TO C-P-YR.                          CL**2
00575      MOVE DE-PAY-MO              TO C-P-MO.                          CL**2
00576      MOVE DE-PAY-DA              TO C-P-DA.                          CL**2
00577      MOVE C-PAID-DTE-R           TO C-PAID-DTE.                      CL**2
00578      MOVE DE-CLAIM-AMT           TO C-AMOUNT-PAID.                   CL**2
00579                                                                      CL**2
00580      IF LIFE                                                         CL**2
00581          MOVE DE-CLM-AGE         TO C-AGE                            CL**2
00582          MOVE DE-LF-TERM         TO C-TERM                           CL**2
00583          MOVE DE-LF-TYPE         TO C-COV-CODE                       CL**2
00584          MOVE DE-LF-BEN          TO C-ORIG-AMT                       CL**2
00585          MOVE DE-LF-PRM          TO C-ORIG-PREM                      CL**2
00586          MOVE ZEROS              TO C-UNPD-BAL  C-DELNQ-AMT.         CL**2
00587                                                                      CL**2
00588      IF DISABILITY                                                   CL**2
00589          MOVE DE-AH-TYPE         TO C-COV-CODE                       CL**2
00590          MOVE DE-AH-BEN          TO C-MTH-BEN                        CL**2
00591          MOVE DE-AH-TERM         TO C-TERM                           CL**2
00592          COMPUTE C-RESERVE = C-AMOUNT-PAID + 1.00                    CL**2
00593          MOVE DE-PTO-CC          TO C-T-CC                           CL**2
00594          MOVE DE-PTO-YR          TO C-T-YR                           CL**2
00595          MOVE DE-PTO-MO          TO C-T-MO                           CL**2
00596          MOVE DE-PTO-DA          TO C-T-DA                           CL**2
00597          MOVE C-TO-DTE-R         TO C-TO-DTE                         CL**2
00598          MOVE DE-DAYS-DISAB      TO C-DAYS-DIS                       CL**2
00599          MOVE ZEROS              TO C-FROM-DTE C-PMT-NO C-ELIM-CD    CL**2
00600          PERFORM 0550-SET-ELIM-CODE THRU 0559-ELIM-EXIT              CL**2
00601          IF DE-PAY-CODE = 'F'                                        CL**2
00602              MOVE '3'            TO C-PMT-TYPE                       CL**2
00603          ELSE                                                        CL**2
00604              MOVE '2'            TO C-PMT-TYPE.                      CL**2
00605                                                                      CL**2
00606                                                                      CL**2
00607      GO TO 0500-EXTRACT-RELEASE.                                     CL**2
00608  EJECT                                                               CL**2
00609  0500-EXTRACT-RELEASE.                                               CL**2
00610      RELEASE SORT-RECORD.                                            CL**2
00611                                                                      CL**2
00612      GO TO 0210-READ-EXTRACT.                                        CL**2
00613                                                                      CL**2
00614                                                                      CL**2
00615  0550-SET-ELIM-CODE.                                                 CL**2
00616      IF DTE-CLIENT = 'ITY'  OR  'MLI'                                CL**2
00617          IF C-COV-2 = '2'  MOVE 5 TO C-ELIM-CD                       CL**2
00618          ELSE                                                        CL**2
00619               IF C-COV-2 = '5'  MOVE 4 TO C-ELIM-CD                  CL**2
00620               ELSE                                                   CL**2
00621                    IF C-COV-2 = '6'  MOVE 2 TO C-ELIM-CD             CL**2
00622                    ELSE                                              CL**2
00623                         IF C-COV-2 = '7'  MOVE 6 TO C-ELIM-CD.       CL**2
00624  0559-ELIM-EXIT.                                                     CL**2
00625      EXIT.                                                           CL**2
00626                                                                      CL**2
00627                                                                      CL**2
00628  0590-CLOSE-FILES.                                                   CL**2
00629      CLOSE  EXTRACT ERACCTT.                                         CL**2
00630                                                                      CL**2
00631  0599-SORT-SECTION-EXIT.                                             CL**2
00632      EXIT.                                                           CL**2
00633  EJECT                                                               CL**2
00634  PRINT-INTERFACE-REPORT SECTION.                                     CL**2
00635  0600-INITIALIZATION.                                                CL**2
00636      OPEN OUTPUT PREM-TAPE  CLAIM-TAPE  PRINTER.                     CL**2
00637      MOVE COMPANY-NAME           TO H1-CMP.                          CL**2
00638                                                                      CL**4
00639      IF DTE-CLIENT = 'MLI'                                           CL**2
00640          MOVE '   AMBASSADOR SERVICES INC.   ' TO H1-CMP.            CL**2
00641                                                                      CL**4
00642      MOVE WS-CURRENT-DATE        TO H2-IPL.                          CL**2
00643      MOVE ALPH-DATE              TO H3-DATE.                         CL**2
00644      PERFORM 0610-EXTRACT-RETURN.                                    CL**2
00645      MOVE SORT-TYPE              TO PREV-TYPE.                       CL**2
00646      MOVE SORT-ACCOUNT           TO PREV-ACCOUNT.                    CL**2
00647      PERFORM 0900-PRINT-HEADINGS THRU 0900-HDGS-EXIT.                CL**2
00648      GO TO 0630-ACCUMULATE-TOTALS.                                   CL**2
00649                                                                      CL**2
00650  0610-EXTRACT-RETURN.                                                CL**2
00651      RETURN SORT-FILE  AT END GO TO 0680-ACCOUNT-TOTAL-PRINT.        CL**2
00652                                                                      CL**2
00653  0620-TYPE-ACCT-CHECK.                                               CL**2
00654      IF SORT-TYPE NOT = PREV-TYPE                                    CL**2
00655          PERFORM 0680-ACCOUNT-TOTAL-PRINT THRU 0690-TYPE-EXIT        CL**2
00656          PERFORM 0900-PRINT-HEADINGS THRU 0900-HDGS-EXIT.            CL**2
00657                                                                      CL**2
00658      IF SORT-ACCOUNT NOT = PREV-ACCOUNT                              CL**2
00659          PERFORM 0680-ACCOUNT-TOTAL-PRINT THRU 0680-ACCT-EXIT.       CL**2
00660                                                                      CL**2
00661  0630-ACCUMULATE-TOTALS.                                             CL**2
00662      IF PREMIUMS                                                     CL**2
00663          IF ISSUE                                                    CL**2
00664              ADD P-LIFE-PREM TO A-LIFE-ISS  T-LIFE-ISS               CL**2
00665              ADD P-AH-PREM TO A-AH-ISS  T-AH-ISS                     CL**2
00666          ELSE                                                        CL**2
00667              ADD P-LIFE-REFUND TO A-LIFE-CAN  T-LIFE-CAN             CL**2
00668              ADD P-AH-REFUND TO A-AH-CAN  T-AH-CAN.                  CL**2
00669                                                                      CL**2
00670      IF CLAIMS                                                       CL**2
00671          IF LIFE                                                     CL**2
00672              ADD C-AMOUNT-PAID TO A-LIFE-CLM  T-LIFE-CLM             CL**2
00673          ELSE                                                        CL**2
00674              ADD C-AMOUNT-PAID TO A-AH-CLM  T-AH-CLM.                CL**2
00675                                                                      CL**2
00676  0640-WRITE-INTERFACE-TAPES.                                         CL**2
00677      IF PREMIUMS                                                     CL**2
00678          WRITE PREMIUM-RECORD FROM PREMIUM-FIELDS                    CL**2
00679            ADD 1 TO PREM-RECORDS.                                    CL**2
00680                                                                      CL**2
00681      IF CLAIMS                                                       CL**2
00682          WRITE CLAIM-RECORD FROM CLAIM-FIELDS                        CL**2
00683            ADD 1 TO CLM-RECORDS.                                     CL**2
00684                                                                      CL**2
00685      GO TO 0610-EXTRACT-RETURN.                                      CL**2
00686                                                                      CL**2
00687  0680-ACCOUNT-TOTAL-PRINT.                                           CL**2
00688      MOVE SPACES                 TO DETAIL-LINE.                     CL**2
00689      MOVE PREV-ACCOUNT           TO DTL-ACCOUNT.                     CL**2
00690                                                                      CL**4
00691      IF PREV-TYPE = '1'                                              CL**2
00692          MOVE A-LIFE-ISS         TO DTL-AMOUNT (1)                   CL**2
00693          MOVE A-LIFE-CAN         TO DTL-AMOUNT (2)                   CL**2
00694          MOVE A-AH-ISS           TO DTL-AMOUNT (3)                   CL**2
00695          MOVE A-AH-CAN           TO DTL-AMOUNT (4)                   CL**2
00696      ELSE                                                            CL**2
00697          MOVE A-LIFE-CLM         TO DTL-AMOUNT (1)                   CL**2
00698          MOVE A-AH-CLM           TO DTL-AMOUNT (3).                  CL**2
00699                                                                      CL**4
00700      IF LINE-CT GREATER THAN +55                                     CL**2
00701          PERFORM 0900-PRINT-HEADINGS THRU 0900-HDGS-EXIT.            CL**2
00702                                                                      CL**4
00703      MOVE DETAIL-LINE            TO P-DATA.                          CL**2
00704      MOVE '0'                    TO X.                               CL**2
00705      PERFORM 0910-PRINT-LINE THRU 0910-PRT-EXIT.                     CL**2
00706      ADD +2                      TO LINE-CT.                         CL**2
00707      MOVE ZEROS                  TO ACCOUNT-TOTALS.                  CL**2
00708      MOVE SORT-ACCOUNT           TO PREV-ACCOUNT.                    CL**2
00709                                                                      CL**4
00710  0680-ACCT-EXIT.                                                     CL**2
00711      EXIT.                                                           CL**2
00712                                                                      CL**2
00713  0690-TYPE-TOTAL-PRINT.                                              CL**2
00714      MOVE SPACES                 TO DETAIL-LINE.                     CL**2
00715      MOVE 'FINAL     '           TO DTL-ACCOUNT.                     CL**2
00716                                                                      CL**4
00717      IF PREV-TYPE = '1'                                              CL**2
00718          MOVE T-LIFE-ISS         TO DTL-AMOUNT (1)                   CL**2
00719          MOVE T-LIFE-CAN         TO DTL-AMOUNT (2)                   CL**2
00720          MOVE T-AH-ISS           TO DTL-AMOUNT (3)                   CL**2
00721          MOVE T-AH-CAN           TO DTL-AMOUNT (4)                   CL**2
00722      ELSE                                                            CL**2
00723          MOVE T-LIFE-CLM         TO DTL-AMOUNT (1)                   CL**2
00724          MOVE T-AH-CLM           TO DTL-AMOUNT (3).                  CL**2
00725                                                                      CL**4
00726      IF LINE-CT GREATER THAN +55                                     CL**2
00727          PERFORM 0900-PRINT-HEADINGS THRU 0900-HDGS-EXIT.            CL**2
00728                                                                      CL**4
00729      MOVE DETAIL-LINE            TO P-DATA.                          CL**2
00730      MOVE '-'                    TO X.                               CL**2
00731      PERFORM 0910-PRINT-LINE THRU 0910-PRT-EXIT.                     CL**2
00732      MOVE ZEROS                  TO TYPE-TOTALS.                     CL**2
00733      MOVE SORT-TYPE              TO PREV-TYPE.                       CL**2
00734                                                                      CL**4
00735  0690-TYPE-EXIT.                                                     CL**2
00736      EXIT.                                                           CL**2
00737                                                                      CL**2
00738  0699-EOJ.                                                           CL**2
00739                              COPY ELCPRTC.                           CL**2
00740      CLOSE PREM-TAPE  CLAIM-TAPE  PRINTER.                           CL**2
00741  EJECT                                                               CL**2
00742  PERFORMED-PROCEDURES SECTION.                                       CL**2
00743  0900-PRINT-HEADINGS.                                                CL**2
00744      ADD  +1                     TO  PAGE-CT.                        CL**2
00745      MOVE  PAGE-CT               TO   H1-PAGE.                       CL**2
00746      MOVE  HEAD-1                TO  P-DATA.                         CL**2
00747      MOVE  '1'                   TO  X.                              CL**2
00748      PERFORM 0910-PRINT-LINE THRU 0910-PRT-EXIT.                     CL**2
00749      MOVE  HEAD-2                TO  P-DATA.                         CL**2
00750      MOVE  ' '                   TO  X.                              CL**2
00751      PERFORM 0910-PRINT-LINE THRU 0910-PRT-EXIT.                     CL**2
00752      MOVE  HEAD-3                TO  P-DATA.                         CL**2
00753      MOVE  ' '                   TO  X.                              CL**2
00754      PERFORM 0910-PRINT-LINE THRU 0910-PRT-EXIT.                     CL**2
00755      MOVE  HEAD-4                TO  P-DATA.                         CL**2
00756      MOVE  '-'                   TO  X.                              CL**2
00757      PERFORM 0910-PRINT-LINE THRU 0910-PRT-EXIT.                     CL**2
00758                                                                      CL**4
00759      IF PREV-TYPE = '1'                                              CL**2
00760          MOVE HEAD-5A            TO P-DATA                           CL**2
00761      ELSE                                                            CL**2
00762          MOVE HEAD-5B            TO P-DATA.                          CL**2
00763                                                                      CL**4
00764      MOVE  ' '                   TO  X.                              CL**2
00765      PERFORM 0910-PRINT-LINE THRU 0910-PRT-EXIT.                     CL**2
00766      MOVE +6                     TO LINE-CT.                         CL**2
00767                                                                      CL**4
00768  0900-HDGS-EXIT.                                                     CL**2
00769      EXIT.                                                           CL**2
00770                                                                      CL**4
00771  0910-PRINT-LINE.                                                    CL**2
00772                              COPY ELCPRT2.                           CL**2
00773  0910-PRT-EXIT.                                                      CL**2
00774      EXIT.                                                           CL**2
00775                                                                      CL**4
00776  ABEND-PGM.                                                          CL**2
00777                              COPY ELCABEND.                          CL**2
00778 /                                                                    CL**2
00779  LCP-WRITE-POS-PRT SECTION.                                          CL**2
00780      IF LCP-ASA = '+'                                                CL**2
00781          WRITE PRT AFTER 0 LINE                                      CL**2
00782      ELSE                                                            CL**2
00783      IF LCP-ASA = ' '                                                CL**2
00784          WRITE PRT AFTER ADVANCING 1 LINE                            CL**2
00785      ELSE                                                            CL**2
00786      IF LCP-ASA = '0'                                                CL**2
00787          WRITE PRT AFTER ADVANCING 2 LINE                            CL**2
00788      ELSE                                                            CL**2
00789      IF LCP-ASA = '-'                                                CL**2
00790          WRITE PRT AFTER ADVANCING 3 LINE                            CL**2
00791      ELSE                                                            CL**2
00792      IF LCP-ASA = '1'                                                CL**2
00793          WRITE PRT AFTER ADVANCING PAGE                              CL**2
00794      ELSE                                                            CL**2
00795      IF LCP-ASA = '2'                                                CL**2
00796          WRITE PRT AFTER ADVANCING LCP-CH2                           CL**2
00797      ELSE                                                            CL**2
00798      IF LCP-ASA = '3'                                                CL**2
00799          WRITE PRT AFTER ADVANCING LCP-CH3                           CL**2
00800      ELSE                                                            CL**2
00801      IF LCP-ASA = '4'                                                CL**2
00802          WRITE PRT AFTER ADVANCING LCP-CH4                           CL**2
00803      ELSE                                                            CL**2
00804      IF LCP-ASA = '5'                                                CL**2
00805          WRITE PRT AFTER ADVANCING LCP-CH5                           CL**2
00806      ELSE                                                            CL**2
00807      IF LCP-ASA = '6'                                                CL**2
00808          WRITE PRT AFTER ADVANCING LCP-CH6                           CL**2
00809      ELSE                                                            CL**2
00810      IF LCP-ASA = '7'                                                CL**2
00811          WRITE PRT AFTER ADVANCING LCP-CH7                           CL**2
00812      ELSE                                                            CL**2
00813      IF LCP-ASA = '8'                                                CL**2
00814          WRITE PRT AFTER ADVANCING LCP-CH8                           CL**2
00815      ELSE                                                            CL**2
00816      IF LCP-ASA = '9'                                                CL**2
00817          WRITE PRT AFTER ADVANCING LCP-CH9                           CL**2
00818      ELSE                                                            CL**2
00819      IF LCP-ASA = 'A'                                                CL**2
00820          WRITE PRT AFTER ADVANCING LCP-CH10                          CL**2
00821      ELSE                                                            CL**2
00822      IF LCP-ASA = 'B'                                                CL**2
00823          WRITE PRT AFTER ADVANCING LCP-CH11                          CL**2
00824      ELSE                                                            CL**2
00825      IF LCP-ASA = 'C'                                                CL**2
00826          WRITE PRT AFTER ADVANCING LCP-CH12                          CL**2
00827      ELSE                                                            CL**2
00828      IF LCP-ASA = 'V'                                                CL**2
00829          WRITE PRT AFTER ADVANCING LCP-P01                           CL**2
00830      ELSE                                                            CL**2
00831      IF LCP-ASA = 'W'                                                CL**2
00832          WRITE PRT AFTER ADVANCING LCP-P02                           CL**2
00833      ELSE                                                            CL**2
00834      DISPLAY 'ASA CODE ERROR'.                                       CL**2
00835  LCP-WRITE-END-PRT.                                                  CL**2
00836      EXIT.                                                           CL**2
00837                                                                      CL**2
