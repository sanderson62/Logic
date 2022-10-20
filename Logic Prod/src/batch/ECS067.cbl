00001  IDENTIFICATION DIVISION.                                         03/10/98
00002                                                                   ECS067
00003  PROGRAM-ID.                ECS067.                                  LV002
00004 *              PROGRAM CONVERTED BY                               ECS067
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ECS067
00006 *              CONVERSION DATE 01/30/95 09:51:55.                 ECS067
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          ECS067
00008 *                           VMOD=2.024.                              CL**2
00009                                                                   ECS067
00010 *AUTHOR.        LOGIC, INC.                                       ECS067
00011 *               DALLAS, TEXAS.                                    ECS067
00012                                                                   ECS067
00013 *DATE-COMPILED.                                                   ECS067
00014                                                                   ECS067
00015 *SECURITY.   *****************************************************ECS067
00016 *            *                                                   *ECS067
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ECS067
00018 *            *                                                   *ECS067
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS067
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ECS067
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *ECS067
00022 *            *                                                   *ECS067
00023 *            *****************************************************ECS067
00024                                                                   ECS067
00025 *REMARKS.                                                         ECS067
00026 *        PRINT 1099 FORMS FROM THE COMPENSATION MASTER.           ECS067
00027 *                                                                 ECS067
00028 *        PROGRAM OPTION      -     DESCRIPTION.                   ECS067
00029 *                                                                 ECS067
00030 *           1 - NORMAL RUN                                        ECS067
00031 *           2 - BLANK TAX AMOUNT                                  ECS067
00032 *           3 - PRINT ACCOUNT 1099 FORMS ONLY                     ECS067
00033 *           4 - PRINT AGENT 1099 FORMS ONLY                       ECS067
00034 *           5 - BYPASS PRINT NO TAX I.D.                          ECS067
00035                                                                   ECS067
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
051810* 051810 CR2010042900001   PEMA  SPEARATE CITY AND STATE
      ******************************************************************
00036  ENVIRONMENT DIVISION.                                            ECS067
00037  INPUT-OUTPUT SECTION.                                            ECS067
00038  FILE-CONTROL.                                                    ECS067
00039                                                                   ECS067
00040      SELECT SORT-FILE        ASSIGN TO SYS001-DA-FBA1-S-SORTWK1.  ECS067
00041      SELECT PRNTR            ASSIGN TO SYS008-UR-1403-S-SYS008.   ECS067
00042      SELECT COMM-MSTR-IN     ASSIGN TO SYS010-UT-2400-S-SYS010.   ECS067
00043      SELECT GOVT-TAPE-OUT    ASSIGN TO SYS011-UT-2400-S-SYS011.   ECS067
00044      SELECT WORK-COMM-MSTR   ASSIGN TO SYS012-UT-FBA1-S-SYS012.   ECS067
00045      SELECT DISK-DATE        ASSIGN TO SYS019-UT-FBA1-S-SYS019.   ECS067
00046      SELECT FICH             ASSIGN TO SYS020-UT-2400-S-SYS020.   ECS067
00047  EJECT                                                            ECS067
00048  DATA DIVISION.                                                   ECS067
00049  FILE SECTION.                                                    ECS067
00050                                                                   ECS067
00051  SD  SORT-FILE.                                                   ECS067
00052                                                                   ECS067
00053  01  SORT-REC.                                                    ECS067
00054      12  SORT-ID             PIC  XX.                             ECS067
00055      12  SORT-KEY            PIC  X(29).                          ECS067
00056      12  FILLER              PIC  X(419).                         ECS067
00057      12  FILLER              PIC  X(250).                         ECS067
00058  EJECT                                                            ECS067
00059  FD  PRNTR                                                        ECS067
00060                              COPY ELCPRTFD.                       ECS067
00061  EJECT                                                            ECS067
00062  FD  COMM-MSTR-IN                                                 ECS067
00063                                                                   ECS067
00064                              COPY ECSCOIFD.                       ECS067
00065                                                                   ECS067
00066  EJECT                                                            ECS067
00067  FD  GOVT-TAPE-OUT                                                ECS067
00068      BLOCK CONTAINS 0 RECORDS
00069      RECORDING MODE F.                                            ECS067
00070                                                                   ECS067
00071  01  GOVT-REC                PIC  X(420).                         ECS067
00072  EJECT                                                            ECS067
00073  FD  WORK-COMM-MSTR                                               ECS067
00074      BLOCK CONTAINS 0 RECORDS
00075      RECORDING MODE F.                                            ECS067
00076                                                                   ECS067
00077  01  WORK-COMM-REC           PIC  X(700).                         ECS067
00078  EJECT                                                            ECS067
00079  FD  DISK-DATE                                                    ECS067
00080                              COPY ELCDTEFD.                       ECS067
00081  EJECT                                                            ECS067
00082  FD  FICH                                                         ECS067
00083                              COPY ELCFCHFD.                       ECS067
00084  EJECT                                                            ECS067
00085  WORKING-STORAGE SECTION.                                         ECS067
00086  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      ECS067
00087  77  FILLER PIC  X(32)  VALUE '********************************'. ECS067
00088  77  FILLER PIC  X(32)  VALUE '*           ECS067             *'. ECS067
00089  77  FILLER PIC  X(32)  VALUE '*********** VMOD=2.024 *********'.    CL**2
00090                                                                   ECS067
00091  77  PGM-SUB                 PIC S9(3)       VALUE +67  COMP-3.   ECS067
00092  77  RECORDS-IN-BLOCK        PIC S9(3)       VALUE +0   COMP-3.   ECS067
00093  77  X                       PIC  X          VALUE ' '.           ECS067
00094  77  PRT-1099-SWITCH         PIC  X          VALUE '0'.           ECS067
00095      88  PRT-4-LINES                         VALUE '1'.           ECS067
00096      88  PRT-3-LINES                         VALUE '2'.           ECS067
00097  77  SPACE-NP                PIC  X          VALUE '1'.           ECS067
00098  77  SPACE-1                 PIC  X          VALUE ' '.           ECS067
00099  77  SPACE-2                 PIC  X          VALUE '0'.           ECS067
00100  77  SPACE-3                 PIC  X          VALUE '-'.           ECS067
00101  77  A-RECORD-SWITCH         PIC  X          VALUE 'N'.           ECS067
00102      88  A-RECORD-WAITING                    VALUE 'Y'.           ECS067
00103  77  FIRST-A-SWITCH          PIC  X          VALUE 'Y'.           ECS067
00104      88  FIRST-A                             VALUE 'Y'.           ECS067
00105  77  NUMBER-OF-PAYERS        PIC  9(4)       VALUE ZERO.          ECS067
00106  77  NUMBER-OF-PAYEES        PIC  9(6)       VALUE ZERO.          ECS067
CIDMOD 77  CSO-1099-CNT            PIC  9(6)       VALUE ZERO.          00000655
CIDMOD 77  CSO-1099-BYP            PIC  9(6)       VALUE ZERO.          00000656
00107                                                                      CL**2
00108 *********                                                         ECS067
00109 *********THE FOLLOWING FIELDS MUST BE ALTERED TO YOUR ASSIGNED    ECS067
00110 *********TRANSMITTER CONTROL CODE AND COMPANY NAME AND ADDRESS.   ECS067
00111 *********                                                         ECS067
00112  77  TRANSMITTER-CNTL-CODE   PIC  X(5)       VALUE '18J95'.       ECS067
00113  77  TRANSMITTER-CNTL-NAME   PIC  X(40)      VALUE                ECS067
00114          'LOGIC INC                               '.              ECS067
00115  77  TRANSMITTER-CNTL-ADDR   PIC  X(40)      VALUE                ECS067
00116          '9330 LBJ FREEWAY                        '.              ECS067
00117  77  TRANSMITTER-CNTL-CITY   PIC  X(40)      VALUE                ECS067
00118          'DALLAS, TX  75243                       '.              ECS067
00119                                                                      CL**2
00120  01  WS.                                                          ECS067
00121      12  WS-RETURN-CODE          PIC S9(4)              COMP.     ECS067
00122      12  WS-ABEND-MESSAGE        PIC  X(80).                      ECS067
00123      12  WS-ABEND-FILE-STATUS    PIC  XX     VALUE ZEROS.         ECS067
00124      12  WS-ZERO                 PIC S9      VALUE ZERO COMP-3.   ECS067
00125  EJECT                                                            ECS067
00126  01  GOVT-REC-TYPE-A.                                             ECS067
00127      12  A-RECORD-TYPE           PIC  X.                          ECS067
00128      12  A-PAYMENT-YEAR          PIC  X(2).                       ECS067
00129      12  A-REEL-SEQ-NO           PIC  X(3).                       ECS067
00130      12  A-PAYER-FED-EIN         PIC  X(9).                       ECS067
00131      12  A-PAYER-NAME-CONTROL    PIC  X(4).                       ECS067
00132      12  FILLER                  PIC  X.                          ECS067
00133      12  A-COMB-FED-ST           PIC  X.                          ECS067
00134      12  A-TYP-OF-RETURN         PIC  X.                          ECS067
00135      12  A-AMT-INDICATOR         PIC  X(9).                       ECS067
00136      12  A-TAPE-FILER-INDICATOR  PIC  XX.                         ECS067
00137      12  FILLER                  PIC  X(10).                      ECS067
00138      12  A-TRNSMITR-CNTRL-CODE   PIC  X(5).                       ECS067
00139      12  A-FOREIGN-CORP-INDCTR   PIC  X.                          ECS067
00140      12  A-FIRST-PAYER-NAME      PIC  X(40).                      ECS067
00141      12  A-SECOND-PAYER-NAME     PIC  X(40).                      ECS067
00142      12  A-TRNSFR-AGT-INDICATOR  PIC  X.                          ECS067
00143      12  A-PAYER-SHIPPNG-ADDR    PIC  X(40).                      ECS067
00144      12  A-PAYER-CTY-ST-ZIP.                                      ECS067
00145          16  FILLER              PIC  X(30).                      ECS067
00146          16  A-PAYER-ZIP         PIC  X(10).                      ECS067
00147      12  A-TRNSMITR-NAME         PIC  X(80).                      ECS067
00148      12  A-TRNSMITR-ADDR         PIC  X(40).                      ECS067
00149      12  A-TRNSMITR-CTY-ST-ZIP   PIC  X(40).                      ECS067
00150      12  FILLER                  PIC  X(50).                      ECS067
00151  EJECT                                                            ECS067
00152  01  GOVT-REC-TYPE-B  REDEFINES  GOVT-REC-TYPE-A.                 ECS067
00153      12  B-RECORD-TYPE           PIC  X.                          ECS067
00154      12  B-PAYMENT-YEAR          PIC  XX.                         ECS067
00155      12  B-DOCUMNT-SPCFIC-CODE   PIC  XX.                         ECS067
00156      12  FILLER                  PIC  X.                          ECS067
00157      12  B-CORRECTION            PIC  X.                          ECS067
00158      12  B-NAME-CONTROL          PIC  X(4).                       ECS067
00159      12  FILLER                  PIC  X(2).                       ECS067
00160      12  B-TYPE-OF-TIN           PIC  X.                          ECS067
00161      12  B-TAXPAYER-ID-NO        PIC  X(9).                       ECS067
00162      12  B-ACCT-NO-FOR-PAYEE     PIC  X(20).                      ECS067
00163      12  FILLER                  PIC  X(01).                      ECS067
00164      12  B-PRCNT-TOT-DIST        PIC  X(02).                      ECS067
00165      12  FILLER                  PIC  X(04).                      ECS067
00166      12  B-PAYMENT-AMOUNT-01     PIC  9(8)V99.                    ECS067
00167      12  B-PAYMENT-AMOUNT-02     PIC  9(8)V99.                    ECS067
00168      12  B-PAYMENT-AMOUNT-03     PIC  9(8)V99.                    ECS067
00169      12  B-PAYMENT-AMOUNT-04     PIC  9(8)V99.                    ECS067
00170      12  B-PAYMENT-AMOUNT-05     PIC  9(8)V99.                    ECS067
00171      12  B-PAYMENT-AMOUNT-06     PIC  9(8)V99.                    ECS067
00172      12  B-PAYMENT-AMOUNT-07     PIC  9(8)V99.                    ECS067
00173      12  B-PAYMENT-AMOUNT-08     PIC  9(8)V99.                    ECS067
00174      12  B-PAYMENT-AMOUNT-09     PIC  9(8)V99.                    ECS067
00175      12  FILLER                  PIC  X(20).                      ECS067
00176      12  B-FRGN-CNTRY-INDCTR     PIC  X.                          ECS067
00177      12  B-FIRST-PAYEE-NAME      PIC  X(40).                      ECS067
00178      12  B-SECOND-PAYEE-NAME     PIC  X(40).                      ECS067
00179      12  B-PAYEE-MAILING-ADDR    PIC  X(40).                      ECS067
00180      12  B-PAYEE-CTY-ST          PIC  X(31).                      ECS067
00181      12  B-PAYEE-ZIP.                                             ECS067
00182          16  FILLER              PIC  X(5).                       ECS067
00183          16  B-PAYEE-ZIP-4       PIC  X(4).                       ECS067
00184      12  FILLER                  PIC  X(99).                      ECS067
00185                                                                   ECS067
00186  01  GOVT-REC-TYPE-C  REDEFINES  GOVT-REC-TYPE-A.                 ECS067
00187      12  C-RECORD-TYPE           PIC  X.                          ECS067
00188      12  C-NUMBER-OF-PAYEES      PIC  9(6).                       ECS067
00189      12  FILLER                  PIC  X(3).                       ECS067
00190      12  C-CONTROL-TOTAL-1       PIC  9(13)V99.                   ECS067
00191      12  C-CONTROL-TOTAL-2       PIC  9(13)V99.                   ECS067
00192      12  C-CONTROL-TOTAL-3       PIC  9(13)V99.                   ECS067
00193      12  C-CONTROL-TOTAL-4       PIC  9(13)V99.                   ECS067
00194      12  C-CONTROL-TOTAL-5       PIC  9(13)V99.                   ECS067
00195      12  C-CONTROL-TOTAL-6       PIC  9(13)V99.                   ECS067
00196      12  C-CONTROL-TOTAL-7       PIC  9(13)V99.                   ECS067
00197      12  C-CONTROL-TOTAL-8       PIC  9(13)V99.                   ECS067
00198      12  C-CONTROL-TOTAL-9       PIC  9(13)V99.                   ECS067
00199      12  FILLER                  PIC  X(275).                     ECS067
00200                                                                   ECS067
00201  01  GOVT-REC-TYPE-F  REDEFINES  GOVT-REC-TYPE-A.                 ECS067
00202      12  F-RECORD-TYPE           PIC  X.                          ECS067
00203      12  F-NUMBER-OF-A-RECORDS   PIC  9(4).                       ECS067
00204      12  F-ZEROS                 PIC  X(25).                      ECS067
00205      12  FILLER                  PIC  X(390).                     ECS067
00206  EJECT                                                            ECS067
00207  01  COMP-3-WORK     COMP-3.                                      ECS067
00208      12  TOTAL-COMPENSATION  PIC S9(9)V99        VALUE +0.        ECS067
00209      12  TOTAL-1099          PIC S9(9)           VALUE +0.        ECS067
00210      12  FINAL-COMPENSATION  PIC S9(9)V99        VALUE +0.        ECS067
00211      12  FINAL-1099          PIC S9(9)           VALUE +0.        ECS067
00212      12  A                   PIC S9(3)           VALUE +0.        ECS067
00213      12  B                   PIC S9(3)           VALUE +0.        ECS067
00214      12  C                   PIC S9(3)           VALUE +0.        ECS067
00215      12  D                   PIC S9(3)           VALUE +0.        ECS067
00216 *                                                                 ECS067
00217 *                                                                 ECS067
00218 * LEAVE THE ABOVE 2 LINE IN TO PREVENT LOSING THE LINE ABOVE      ECS067
00230                                                                   ECS067
00231  01  MISC-WS.                                                     ECS067
00232      12  REMIT-AREA.                                              ECS067
00233          16  REMIT-LN1       PIC  X(30).                          ECS067
00234          16  REMIT-LN2       PIC  X(30).                          ECS067
00235          16  REMIT-LN3       PIC  X(30).                          ECS067
00236          16  REMIT-LN4       PIC  X(30).                          ECS067
00237          16  REMIT-LN4A.                                          ECS067
00238              20  FILLER      PIC  X(20).                          ECS067
00239              20  REMIT-ZIP   PIC  X(10).                          ECS067
00240          16  REMIT-LN5       PIC  X(14).                          ECS067
00241      12  CUR-CARR            PIC  X          VALUE LOW-VALUE.     ECS067
00242      12  WS-SOC-SEC-NO.                                           ECS067
00243          16  WS-SS-TYPE      PIC X.                               ECS067
00244          16  WS-SS-NO        PIC X(12).                           ECS067
00245      12  A-NAME.                                                  ECS067
00246          16  A-CHAR          PIC  X      OCCURS 30 TIMES.         ECS067
00247      12  B-NAME.                                                  ECS067
00248          16  B-CHAR          PIC  X      OCCURS 30 TIMES.         ECS067
00249      12  TEST-IT.                                                 ECS067
00250          16  TEST-FIVE       PIC  X(5).                           ECS067
00251          16  FILLER          PIC  X(25).                          ECS067
00252      12  TEST-SOC-SEC.                                            ECS067
00253          16  T-SOC-SEC       PIC  X      OCCURS 30 TIMES.         ECS067
00254      12  NEW-SOC-SEC.                                             ECS067
00255          16  N-SOC-SEC       PIC  X      OCCURS 9 TIMES.          ECS067
00256                                                                   ECS067
00257  01  TELE-ZIP-LINE.                                               ECS067
00258      12  LINE5-TELE              PIC  X(12).                      ECS067
00259      12  FILLER                  PIC  X(01).                      ECS067
00260      12  LINE5-ZIP-CODE.                                          ECS067
00261          16  LINE5-ZIP-FIVE      PIC  X(05).                      ECS067
00262          16  LINE5-ZIP-DASH      PIC  X(01).                      ECS067
00263          16  LINE5-ZIP-FOUR      PIC  X(04).                      ECS067
00264      12  LINE5-POSTAL-CODE  REDEFINES  LINE5-ZIP-CODE.            ECS067
00265          16  LINE5-POST-CODE1    PIC  X(03).                      ECS067
00266          16  FILLER              PIC  X(01).                      ECS067
00267          16  LINE5-POST-CODE2    PIC  X(03).                      ECS067
00268          16  FILLER              PIC  X(03).                      ECS067
00269      12  FILLER                  PIC  X(07).                      ECS067
00270                                                                   ECS067
00271  01  WORK-ZIP-AREA.                                               ECS067
00272      12  WORK-ZIP-CODE.                                           ECS067
00273          16  WZC-PRIME       PIC  X(05).                          ECS067
00274          16  WZC-PLUS4       PIC  X(04).                          ECS067
00275      12  WORK-ZIP-CD  REDEFINES  WORK-ZIP-CODE.                   ECS067
00276          16  WZC-POS-1       PIC  X(01).                          ECS067
00277          16  FILLER          PIC  X(08).                          ECS067
00278      12  WORK-POSTAL-CD  REDEFINES  WORK-ZIP-CODE.                ECS067
00279          16  WZC-POST-CD1    PIC  X(03).                          ECS067
00280          16  WZC-POST-CD2    PIC  X(03).                          ECS067
00281          16  FILLER          PIC  X(03).                          ECS067
00282  EJECT                                                            ECS067
00283  01  P-REC.                                                       ECS067
00284      12  P-CCSW                  PIC  X.                          ECS067
00285      12  P-LINE.                                                  ECS067
00286          16  FILLER              PIC  X(6).                       ECS067
00287          16  P-NAME.                                              ECS067
00288              20  P-ADDR          PIC  X(30).                      ECS067
00289              20  P-ADDR-X  REDEFINES  P-ADDR.                     ECS067
00290                  24  P-ADD-FIL   PIC  X(21).                      ECS067
00291                  24  P-1099-X  REDEFINES  P-ADD-FIL.              ECS067
00292                      28  FILLER  PIC  X(03).                      ECS067
00293                      28  P-1099  PIC ZZZ,ZZZ,ZZZ-.                ECS067
00294                      28  FILLER  PIC  X(06).                      ECS067
00295                  24  P-ZIP.                                       ECS067
00296                      28  FILLER  PIC  X(5).                       ECS067
00297                      28  P-CARR  PIC  X.                          ECS067
00298                      28  FILLER  PIC  X(3).                       ECS067
00299              20  FILLER          PIC  X(3).                       ECS067
00300          16  FILLER  REDEFINES  P-NAME.                           ECS067
00301              20  P-PAYER-ID      PIC  X(15).                      ECS067
00302              20  FILLER          PIC  X(02).                      ECS067
00303              20  P-RECPT-ID      PIC  X(15).                      ECS067
00304              20  FILLER          PIC  X.                          ECS067
00305          16  FILLER  REDEFINES  P-NAME.                           ECS067
00306              20  P-ACCT-NUM      PIC  X(32).                      ECS067
00307              20  FILLER          PIC  X.                          ECS067
00308          16  P-REST.                                              ECS067
00309              20  P-AMT-1         PIC Z,ZZZ,ZZZ.ZZ-.               ECS067
00310              20  FILLER          PIC  X(3).                       ECS067
00311              20  P-AMT-2         PIC Z,ZZZ,ZZZ.ZZ-.               ECS067
00312              20  FILLER  REDEFINES  P-AMT-2.                      ECS067
00313                  24  FILLER      PIC  X(11).                      ECS067
00314                  24  P-CHK-BOX   PIC  X.                          ECS067
00315                  24  FILLER      PIC  X.                          ECS067
00316              20  FILLER          PIC  X(3).                       ECS067
00317          16  P-REST-T  REDEFINES  P-REST.                         ECS067
00318              20  FILLER          PIC  X(10).                      ECS067
00319              20  P-TOTAL         PIC ZZZ,ZZZ,ZZZ,ZZZ.ZZ-.         ECS067
00320              20  FILLER          PIC  XXX.                        ECS067
00321          16  P-REST-CORR  REDEFINES  P-REST.                      ECS067
00322              20  FILLER          PIC  X.                          ECS067
00323              20  P-CORR          PIC  X(2).                       ECS067
00324              20  FILLER          PIC  X(29).                      ECS067
00325  EJECT                                                            ECS067
00326  01  LINEUP-A.                                                    ECS067
00327      12  FILLER              PIC  X(40)      VALUE SPACES.        ECS067
00328      12  FILLER              PIC  X(2)       VALUE 'XX'.          ECS067
00329      12  FILLER              PIC  X(24)      VALUE SPACES.        ECS067
00330                                                                   ECS067
00331  01  LINEUP-1.                                                    ECS067
00332      12  FILLER              PIC  X(06)      VALUE SPACES.        ECS067
00333      12  FILLER              PIC  X(30)      VALUE                ECS067
00334              'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.                    ECS067
00335      12  FILLER              PIC  X(04)      VALUE SPACES.        ECS067
00336      12  FILLER              PIC  X(13)      VALUE                ECS067
00337              'Z,ZZZ,ZZZ,ZZ-'.                                     ECS067
00338      12  FILLER              PIC  X(16)      VALUE SPACES.        ECS067
00339                                                                   ECS067
00340  01  LINEUP-2.                                                    ECS067
00341      12  FILLER              PIC  X(06)      VALUE SPACES.        ECS067
00342      12  FILLER              PIC  X(30)      VALUE                ECS067
00343              'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.                    ECS067
00344      12  FILLER              PIC  X(33)      VALUE SPACES.        ECS067
00345                                                                   ECS067
00346  01  LINEUP-3.                                                    ECS067
00347      12  FILLER              PIC  X(6)       VALUE SPACES.        ECS067
00348      12  FILLER              PIC  X(30)      VALUE                ECS067
00349              'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.                    ECS067
00350      12  FILLER              PIC  X(04)      VALUE SPACES.        ECS067
00351      12  FILLER              PIC  X(13)      VALUE                ECS067
00352              'Z,ZZZ,ZZZ,ZZ-'.                                     ECS067
00353      12  FILLER              PIC  X(16)      VALUE SPACES.        ECS067
00354                                                                   ECS067
00355  01  LINEUP-4.                                                    ECS067
00356      12  FILLER              PIC  X(06)      VALUE SPACES.        ECS067
00357      12  FILLER              PIC  X(30)      VALUE                ECS067
00358              'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.                    ECS067
00359      12  FILLER              PIC  X(33)      VALUE SPACES.        ECS067
00360                                                                   ECS067
00361  01  LINEUP-5.                                                    ECS067
00362      12  FILLER              PIC  X(06)      VALUE SPACES.        ECS067
00363      12  FILLER              PIC  X(30)      VALUE                ECS067
00364              'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.                    ECS067
00365      12  FILLER              PIC  X(04)      VALUE SPACES.        ECS067
00366      12  FILLER              PIC  X(13)      VALUE                ECS067
00367              'Z,ZZZ,ZZZ,ZZ-'.                                     ECS067
00368      12  FILLER              PIC  X(16)      VALUE SPACES.        ECS067
00369                                                                   ECS067
00370  01  LINEUP-6.                                                    ECS067
00371      12  FILLER              PIC  X(06)      VALUE SPACES.        ECS067
00372      12  FILLER              PIC  X(15)      VALUE                ECS067
00373              'XXXXXXXXXXXXXXX'.                                   ECS067
00374      12  FILLER              PIC  X(02)      VALUE SPACES.        ECS067
00375      12  FILLER              PIC  X(16)      VALUE                ECS067
00376              'XXXXXXXXXXXXXXX'.                                   ECS067
00377      12  FILLER              PIC  X(01)      VALUE SPACES.        ECS067
00378      12  FILLER              PIC  X(13)      VALUE                ECS067
00379              'Z,ZZZ,ZZZ,ZZ-'.                                     ECS067
00380      12  FILLER              PIC  X(01)      VALUE SPACES.        ECS067
00381      12  FILLER              PIC  X(13)      VALUE                ECS067
00382              'Z,ZZZ,ZZZ,ZZ-'.                                     ECS067
00383      12  FILLER              PIC  X(03)      VALUE SPACES.        ECS067
00384  EJECT                                                            ECS067
00385  01  LINEUP-7.                                                    ECS067
00386      12  FILLER              PIC  X(06)      VALUE SPACES.        ECS067
00387      12  FILLER              PIC  X(30)      VALUE                ECS067
00388              'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.                    ECS067
00389      12  FILLER              PIC  X(04)      VALUE SPACES.        ECS067
00390      12  FILLER              PIC  X(13)      VALUE                ECS067
00391              'Z,ZZZ,ZZZ,ZZ-'.                                     ECS067
00392      12  FILLER              PIC  X(01)      VALUE SPACES.        ECS067
00393      12  FILLER              PIC  X(13)      VALUE                ECS067
00394              'Z,ZZZ,ZZZ,ZZ-'.                                     ECS067
00395      12  FILLER              PIC  X(03)      VALUE SPACES.        ECS067
00396                                                                   ECS067
00397  01  LINEUP-8.                                                    ECS067
00398      12  FILLER              PIC  X(06)      VALUE SPACES.        ECS067
00399      12  FILLER              PIC  X(30)      VALUE                ECS067
00400              'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.                    ECS067
00401      12  FILLER              PIC  X(04)      VALUE SPACES.        ECS067
00402      12  FILLER              PIC  X(13)      VALUE                ECS067
00403              'Z,ZZZ,ZZZ,ZZ-'.                                     ECS067
00404      12  FILLER              PIC  X(12)      VALUE SPACES.        ECS067
00405      12  FILLER              PIC  X(01)      VALUE 'X'.           ECS067
00406      12  FILLER              PIC  X(03)      VALUE SPACES.        ECS067
00407                                                                   ECS067
00408  01  LINEUP-9.                                                    ECS067
00409      12  FILLER              PIC  X(06)      VALUE SPACES.        ECS067
00410      12  FILLER              PIC  X(30)      VALUE                ECS067
00411              'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.                    ECS067
00412      12  FILLER              PIC  X(04)      VALUE SPACES.        ECS067
00413      12  FILLER              PIC  X(13)      VALUE                ECS067
00414              'Z,ZZZ,ZZZ,ZZ-'.                                     ECS067
00415      12  FILLER              PIC  X(01)      VALUE SPACES.        ECS067
00416      12  FILLER              PIC  X(13)      VALUE                ECS067
00417              'Z,ZZZ,ZZZ,ZZ-'.                                     ECS067
00418      12  FILLER              PIC  X(02)      VALUE SPACES.        ECS067
00419                                                                   ECS067
00420  01  LINEUP-10.                                                   ECS067
00421      12  FILLER              PIC  X(06)      VALUE SPACES.        ECS067
00422      12  FILLER              PIC  X(32)      VALUE                ECS067
00423              'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.                  ECS067
00424      12  FILLER              PIC  X(02)      VALUE SPACES.        ECS067
00425      12  FILLER              PIC  X(02)      VALUE 'XX'.          ECS067
00426      12  FILLER              PIC  X(01)      VALUE '/'.           ECS067
00427      12  FILLER              PIC  X(10)      VALUE                ECS067
00428              'XXXXXXXXXX'.                                        ECS067
00429      12  FILLER              PIC  X(16)      VALUE SPACES.        ECS067
00430  EJECT                                                            ECS067
00431                              COPY ERCCOMP.                        ECS067
00432  EJECT                                                            ECS067
00433                              COPY ELCDTECX.                       ECS067
00434  EJECT                                                            ECS067
00435                              COPY ELCDTEVR.                       ECS067
00436  EJECT                                                            ECS067
00437  PROCEDURE DIVISION.                                              ECS067
00438                                                                   ECS067
00439  0000-STANDARD-COPY  SECTION.                                     ECS067
00440                              COPY ELCDTERX.                       ECS067
00441  EJECT                                                            ECS067
00442  0100-INITIALIZE  SECTION.                                        ECS067
00443      OPEN INPUT  COMM-MSTR-IN.                                    ECS067
00444                                                                   ECS067
00445  0110-SORT-ROUTINE.                                               ECS067
00446      SORT SORT-FILE  ON ASCENDING KEY  SORT-KEY                   ECS067
00447          INPUT PROCEDURE 0200-INPUT-ROUTINE  THRU  0299-EXIT      ECS067
00448          OUTPUT PROCEDURE 0300-RETURN-COMP  THRU  0399-EXIT.      ECS067
00449                                                                   ECS067
00450      IF SORT-RETURN  IS NOT EQUAL TO  ZEROS                       ECS067
00451          MOVE '0101'             TO  WS-RETURN-CODE               ECS067
00452          MOVE 'BAD SORT RETURN CODE '                             ECS067
00453                                  TO  WS-ABEND-MESSAGE             ECS067
00454          GO TO 9999-ABORT.                                        ECS067
00455                                                                   ECS067
00456      GO TO 0400-OPEN-RTN.                                         ECS067
00457  EJECT                                                            ECS067
00458  0200-INPUT-ROUTINE  SECTION.                                     ECS067
00459                                                                   ECS067
00460  0210-READ-COMP.                                                  ECS067
00461      READ COMM-MSTR-IN  INTO  COMPENSATION-MASTER  AT END         ECS067
00462          CLOSE  COMM-MSTR-IN                                      ECS067
00463          GO TO 0299-EXIT.                                         ECS067
00464                                                                   ECS067
00465      IF DTE-CLIENT  =  'NCL'                                      ECS067
00466         IF CO-CORPORATION                                         ECS067
00467             GO TO 0210-READ-COMP.                                 ECS067
00468                                                                   ECS067
00469      IF DTE-CLIENT  =  'UCL'                                      ECS067
00470         IF CO-USER-CODE NOT = 'Y'                                 ECS067
00471             GO TO 0210-READ-COMP.                                 ECS067
00472                                                                   ECS067
00473      IF CO-COMPANY-TYPE                                           ECS067
00474          MOVE LOW-VALUES         TO  CO-GROUPING.                 ECS067
00475                                                                   ECS067
00476      IF DTE-CLIENT EQUAL 'MTL' OR 'SAL' OR 'FLA'                  ECS067
00477         MOVE CO-ACCT-NAME TO CO-MAIL-NAME.                        ECS067
00478                                                                   ECS067
00479      RELEASE SORT-REC  FROM  COMPENSATION-MASTER.                 ECS067
00480                                                                   ECS067
00481      GO TO 0210-READ-COMP.                                        ECS067
00482                                                                   ECS067
00483  0299-EXIT.                                                       ECS067
00484      EXIT.                                                        ECS067
00485  EJECT                                                            ECS067
00486  0300-RETURN-COMP  SECTION.                                       ECS067
00487      OPEN OUTPUT  WORK-COMM-MSTR.                                 ECS067
00488                                                                   ECS067
00489  0310-RETURN-REC.                                                 ECS067
00490      RETURN SORT-FILE  INTO  COMPENSATION-MASTER  AT END          ECS067
00491          CLOSE WORK-COMM-MSTR                                     ECS067
00492          GO TO 0399-EXIT.                                         ECS067
00493                                                                   ECS067
00494  0320-WRITE-COMP.                                                 ECS067
00495      WRITE WORK-COMM-REC  FROM  COMPENSATION-MASTER.              ECS067
00496                                                                   ECS067
00497      GO TO 0310-RETURN-REC.                                       ECS067
00498                                                                   ECS067
00499  0399-EXIT.                                                       ECS067
00500      EXIT.                                                        ECS067
00501  EJECT                                                            ECS067
00502  0400-OPEN-RTN.                                                   ECS067
00503      OPEN INPUT  WORK-COMM-MSTR                                   ECS067
00504           OUTPUT PRNTR                                            ECS067
00505                  GOVT-TAPE-OUT.                                   ECS067
00506                                                                   ECS067
00507      IF DTE-PGM-OPT LESS 1 OR GREATER 5                           ECS067
00508          MOVE 1                  TO  DTE-PGM-OPT.                 ECS067
00509                                                                   ECS067
00510      MOVE SPACES                 TO  P-REC.                       ECS067
00511                                                                   ECS067
00512      PERFORM 0500-LINE-UP  THRU  0599-EXIT  6  TIMES.             ECS067
00513                                                                   ECS067
00514      MOVE SPACES                 TO  P-REC.                       ECS067
00515                                                                   ECS067
00516      GO TO 0600-READ-RTN.                                         ECS067
00517                                                                   ECS067
00518  0500-LINE-UP.                                                    ECS067
00519      MOVE SPACE-NP               TO  P-CCSW.                      ECS067
00520                                                                   ECS067
00521      PERFORM 2100-PRT-RTN  THRU  2199-EXIT.                       ECS067
00522                                                                   ECS067
00523 ***  MOVE SPACE-1                TO  P-CCSW.                      ECS067
00524 ***  MOVE LINEUP-A               TO  P-LINE                       ECS067
00525                                                                   ECS067
00526 ***  PERFORM 2100-PRT-RTN  THRU  2199-EXIT.                       ECS067
00527                                                                   ECS067
00528 ***  MOVE SPACE-2                TO  P-CCSW.                      ECS067
00529      MOVE SPACE-1                TO  P-CCSW.                      ECS067
00530      MOVE LINEUP-1               TO  P-LINE.                      ECS067
00531                                                                   ECS067
00532      PERFORM 2100-PRT-RTN  THRU  2199-EXIT.                       ECS067
00533                                                                   ECS067
00534      MOVE SPACE-1                TO  P-CCSW.                      ECS067
00535      MOVE LINEUP-2               TO  P-LINE.                      ECS067
00536                                                                   ECS067
00537      PERFORM 2100-PRT-RTN  THRU  2199-EXIT.                       ECS067
00538                                                                   ECS067
00539      MOVE SPACE-1                TO  P-CCSW.                      ECS067
00540      MOVE LINEUP-3               TO  P-LINE.                      ECS067
00541                                                                   ECS067
00542      PERFORM 2100-PRT-RTN  THRU  2199-EXIT.                       ECS067
00543                                                                   ECS067
00544      MOVE SPACE-1                TO  P-CCSW.                      ECS067
00545      MOVE LINEUP-4               TO  P-LINE.                      ECS067
00546                                                                   ECS067
00547      PERFORM 2100-PRT-RTN  THRU  2199-EXIT.                       ECS067
00548                                                                   ECS067
00549      MOVE SPACE-1                TO  P-CCSW.                      ECS067
00550      MOVE LINEUP-5               TO  P-LINE.                      ECS067
00551                                                                   ECS067
00552      PERFORM 2100-PRT-RTN  THRU  2199-EXIT.                       ECS067
00553  EJECT                                                            ECS067
00554      MOVE SPACE-2                TO  P-CCSW.                      ECS067
00555      MOVE LINEUP-6               TO  P-LINE.                      ECS067
00556                                                                   ECS067
00557      PERFORM 2100-PRT-RTN  THRU  2199-EXIT.                       ECS067
00558                                                                   ECS067
00559      MOVE SPACE-2                TO  P-CCSW.                      ECS067
00560      MOVE LINEUP-7               TO  P-LINE.                      ECS067
00561                                                                   ECS067
00562      PERFORM 2100-PRT-RTN  THRU  2199-EXIT.                       ECS067
00563                                                                   ECS067
00564      MOVE SPACE-3                TO  P-CCSW.                      ECS067
00565      MOVE LINEUP-8               TO  P-LINE.                      ECS067
00566                                                                   ECS067
00567      PERFORM 2100-PRT-RTN  THRU  2199-EXIT.                       ECS067
00568                                                                   ECS067
00569      MOVE SPACE-2                TO  P-CCSW.                      ECS067
00570      MOVE LINEUP-9               TO  P-LINE.                      ECS067
00571                                                                   ECS067
00572      PERFORM 2100-PRT-RTN  THRU  2199-EXIT.                       ECS067
00573                                                                   ECS067
00574      MOVE SPACE-2                TO  P-CCSW.                      ECS067
00575      MOVE LINEUP-10              TO  P-LINE.                      ECS067
00576                                                                   ECS067
00577      PERFORM 2100-PRT-RTN  THRU  2199-EXIT.                       ECS067
00578                                                                   ECS067
00579  0599-EXIT.                                                       ECS067
00580      EXIT.                                                        ECS067
00581  EJECT                                                            ECS067
00582  0600-READ-RTN.                                                   ECS067
00583      READ WORK-COMM-MSTR  INTO  COMPENSATION-MASTER  AT END       ECS067
00584          GO TO 2200-END-OF-JOB.                                   ECS067
00585                                                                   ECS067
CIDMOD     IF  CO-CSO-1099  =  'Y'                                      00005341
CIDMOD         ADD  1  TO  CSO-1099-CNT                                 00005342
CIDMOD       ELSE                                                       00005343
CIDMOD         ADD  1  TO  CSO-1099-BYP                                 00005344
CIDMOD         GO  TO  0600-READ-RTN.                                   00005345
CIDMOD                                                                  00005346
00586      IF DTE-PGM-OPT = 2                                           ECS067
00587        OR  CO-COMPANY-TYPE                                        ECS067
00588          MOVE +0                 TO  CO-YTD-OV  CO-YTD-COM        ECS067
00589          GO TO 0620-ACCEPT-FOR-PROCESSING.                        ECS067
00590                                                                   ECS067
00591      IF DTE-PGM-OPT = 3                                           ECS067
00592        AND  CO-GEN-AGENT-TYPE                                     ECS067
00593          GO TO 0600-READ-RTN.                                     ECS067
00594                                                                   ECS067
00595      IF DTE-PGM-OPT = 4                                           ECS067
00596        AND  CO-ACCOUNT-TYPE                                       ECS067
00597          GO TO 0600-READ-RTN.                                     ECS067
00598                                                                   ECS067
00599      ADD CO-YTD-OV               TO  CO-YTD-COM.                  ECS067
00600                                                                   ECS067
00601      MOVE +0                     TO  CO-YTD-OV.                   ECS067
00602                                                                   ECS067
00603      IF DTE-TOT-OPT = 2                                           ECS067
00604          IF CO-YTD-COM LESS +600.00                               ECS067
00605              GO TO 0600-READ-RTN                                  ECS067
00606          ELSE                                                     ECS067
00607              NEXT SENTENCE                                        ECS067
00608      ELSE                                                         ECS067
00609          IF CO-YTD-COM LESS +0.01                                 ECS067
00610              GO TO 0600-READ-RTN.                                 ECS067
00611                                                                   ECS067
00612  0610-OPTION-5-CHECK.                                             ECS067
00613      IF DTE-PGM-OPT = 5                                           ECS067
00614        AND  CO-SOC-SEC = SPACE                                    ECS067
00615          GO TO 0600-READ-RTN.                                     ECS067
00616                                                                   ECS067
00617  0620-ACCEPT-FOR-PROCESSING.                                      ECS067
00618      IF CO-CARRIER NOT = CUR-CARR                                 ECS067
00619          PERFORM 1000-BREAK-RTN  THRU  1099-EXIT.                 ECS067
00620                                                                   ECS067
00621      IF CO-COMPANY-TYPE                                           ECS067
00622          PERFORM 0800-SET-REMIT-RTN  THRU  0899-EXIT              ECS067
00623          GO TO 0600-READ-RTN.                                     ECS067
00624                                                                   ECS067
00625      IF A-RECORD-WAITING                                          ECS067
00626          PERFORM 1200-A-RECORD-ROUTINE  THRU  1299-EXIT           ECS067
00627          MOVE 'N'                TO  A-RECORD-SWITCH.             ECS067
00628  EJECT                                                            ECS067
00629  0630-PRINT-1099.                                                 ECS067
00630      ADD CO-YTD-COM              TO  TOTAL-COMPENSATION.          ECS067
00631      ADD +1                      TO  TOTAL-1099.                  ECS067
00632                                                                   ECS067
00633      MOVE '1'                    TO  PRT-1099-SWITCH.             ECS067
00634                                                                   ECS067
00635      PERFORM 0700-BUMP-RTN  THRU  0799-EXIT.                      ECS067
00636                                                                   ECS067
00637      PERFORM 2000-PRT-HD-RTN  THRU  2099-EXIT.                    ECS067
00638                                                                   ECS067
00639      MOVE SPACE-2                TO  P-CCSW.                      ECS067
00640      MOVE REMIT-LN5              TO  P-PAYER-ID.                  ECS067
00641      MOVE CO-SOC-SEC             TO  P-RECPT-ID.                  ECS067
00642                                                                   ECS067
00643      IF DTE-CLIENT   =  'NCL'                                     ECS067
00644         MOVE CO-SOC-SEC          TO  WS-SOC-SEC-NO                ECS067
00645         IF WS-SS-TYPE  =  'S' OR 'T' OR 'P' OR 'X' OR 'I'         ECS067
00646             MOVE WS-SS-NO        TO  P-RECPT-ID.                  ECS067
00647                                                                   ECS067
00648      PERFORM 2100-PRT-RTN  THRU  2199-EXIT.                       ECS067
00649                                                                   ECS067
00650      IF PRT-3-LINES                                               ECS067
00651          GO TO 0640-PRINT-1099.                                   ECS067
00652                                                                   ECS067
00653      MOVE SPACE-2                TO  P-CCSW.                      ECS067
00654      MOVE CO-ACCT-NAME           TO  P-ADDR.                      ECS067
00655      MOVE CO-YTD-COM             TO  P-AMT-2.                     ECS067
00656                                                                   ECS067
00657      PERFORM 2100-PRT-RTN  THRU  2199-EXIT.                       ECS067
00658                                                                   ECS067
00659      MOVE SPACE-1                TO  P-CCSW.                      ECS067
00660      MOVE CO-ADDR-1              TO  P-ADDR.                      ECS067
00661                                                                   ECS067
00662      PERFORM 2100-PRT-RTN  THRU  2199-EXIT.                       ECS067
00663                                                                   ECS067
00664      MOVE SPACE-2                TO  P-CCSW.                      ECS067
00665      MOVE CO-ADDR-2              TO  P-ADDR.                      ECS067
00666                                                                   ECS067
00667      PERFORM 2100-PRT-RTN  THRU  2199-EXIT.                       ECS067
00668                                                                   ECS067
00669      MOVE SPACE-2                TO  P-CCSW.                      ECS067
00670 *    MOVE CO-ADDR-3              TO  P-ADDR.                      ECS067
           STRING CO-ADDR-CITY ' ' CO-ADDR-STATE
              DELIMITED BY '  ' INTO P-ADDR
           END-STRING
00671                                                                   ECS067
00672      IF CO-ZIP = ZEROS OR  SPACES                                 ECS067
00673          GO TO 0635-PRINT-1099.                                   ECS067
00674                                                                   ECS067
00675      MOVE CO-ZIP                 TO  WORK-ZIP-CODE.               ECS067
00676      MOVE SPACES                 TO  LINE5-ZIP-CODE.              ECS067
00677                                                                   ECS067
00678      IF WZC-POS-1  IS NOT NUMERIC                                 ECS067
00679          MOVE WZC-POST-CD1       TO  LINE5-POST-CODE1             ECS067
00680          MOVE WZC-POST-CD2       TO  LINE5-POST-CODE2             ECS067
00681      ELSE                                                         ECS067
00682          MOVE WZC-PRIME          TO  LINE5-ZIP-FIVE               ECS067
00683          IF WZC-PLUS4  IS EQUAL TO  '0000'  OR  '    '            ECS067
00684              MOVE ' '            TO  LINE5-ZIP-DASH               ECS067
00685              MOVE '    '         TO  LINE5-ZIP-FOUR               ECS067
00686          ELSE                                                     ECS067
00687              MOVE '-'            TO  LINE5-ZIP-DASH               ECS067
00688              MOVE WZC-PLUS4      TO  LINE5-ZIP-FOUR.              ECS067
00689                                                                   ECS067
00690      MOVE LINE5-ZIP-CODE         TO  P-ZIP.                       ECS067
00691                                                                   ECS067
00692  0635-PRINT-1099.                                                 ECS067
00693                                                                      CL**2
00694      PERFORM 2100-PRT-RTN  THRU  2199-EXIT.                          CL**2
00695                                                                      CL**2
00696      IF DTE-CLIENT NOT = 'MTL'                                       CL**2
00697          GO TO 0637-CREATE-B-RECORD.                                 CL**2
00698                                                                      CL**2
00699      MOVE SPACE-2                TO  P-CCSW.                         CL**2
00700                                                                      CL**2
00701      IF CO-ACCOUNT = LOW-VALUES  OR  ZEROS                           CL**2
00702          MOVE CO-RESP-NO         TO  P-ACCT-NUM                      CL**2
00703      ELSE                                                            CL**2
00704          MOVE CO-ACCOUNT         TO  P-ACCT-NUM.                     CL**2
00705                                                                      CL**2
00706      PERFORM 2100-PRT-RTN  THRU  2199-EXIT.                          CL**2
00707                                                                      CL**2
00708  0637-CREATE-B-RECORD.                                               CL**2
00709                                                                      CL**2
00710      PERFORM 1300-B-RECORD-ROUTINE  THRU  1399-EXIT.              ECS067
00711                                                                   ECS067
00712      GO TO 0600-READ-RTN.                                         ECS067
00713  EJECT                                                            ECS067
00714  0640-PRINT-1099.                                                 ECS067
00715      MOVE SPACE-2                TO  P-CCSW.                      ECS067
00716      MOVE CO-ACCT-NAME           TO  P-ADDR.                      ECS067
00717      MOVE CO-YTD-COM             TO  P-AMT-2.                     ECS067
00718                                                                   ECS067
00719      PERFORM 2100-PRT-RTN  THRU  2199-EXIT.                       ECS067
00720                                                                   ECS067
00721      MOVE SPACE-3                TO  P-CCSW.                      ECS067
00722      MOVE CO-ADDR-1              TO  P-ADDR.                      ECS067
00723                                                                   ECS067
00724      PERFORM 2100-PRT-RTN  THRU  2199-EXIT.                       ECS067
00725                                                                   ECS067
00726      MOVE SPACE-2                TO  P-CCSW.                      ECS067
00727      MOVE CO-ADDR-2              TO  P-ADDR.                      ECS067
00728                                                                   ECS067
00729      IF CO-ZIP = ZEROS  OR  SPACES                                ECS067
00730          GO TO 0645-PRINT-1099.                                   ECS067
00731                                                                   ECS067
00732      MOVE CO-ZIP                 TO  WORK-ZIP-CODE.               ECS067
00733      MOVE SPACES                 TO  LINE5-ZIP-CODE.              ECS067
00734                                                                   ECS067
00735      IF WZC-POS-1  IS NOT NUMERIC                                 ECS067
00736          MOVE WZC-POST-CD1       TO  LINE5-POST-CODE1             ECS067
00737          MOVE WZC-POST-CD2       TO  LINE5-POST-CODE2             ECS067
00738      ELSE                                                         ECS067
00739          MOVE WZC-PRIME          TO  LINE5-ZIP-FIVE               ECS067
00740          IF WZC-PLUS4  IS EQUAL TO  '0000'  OR  '    '            ECS067
00741              MOVE ' '            TO  LINE5-ZIP-DASH               ECS067
00742              MOVE '    '         TO  LINE5-ZIP-FOUR               ECS067
00743          ELSE                                                     ECS067
00744              MOVE '-'            TO  LINE5-ZIP-DASH               ECS067
00745              MOVE WZC-PLUS4      TO  LINE5-ZIP-FOUR.              ECS067
00746                                                                   ECS067
00747      MOVE LINE5-ZIP-CODE         TO  P-ZIP.                       ECS067
00748                                                                   ECS067
00749  0645-PRINT-1099.                                                 ECS067
00750                                                                      CL**2
00751      PERFORM 2100-PRT-RTN  THRU  2199-EXIT.                          CL**2
00752                                                                      CL**2
00753      IF DTE-CLIENT NOT = 'MTL'                                       CL**2
00754          GO TO 0647-CREATE-B-RECORD.                                 CL**2
00755                                                                      CL**2
00756      MOVE SPACE-2                TO  P-CCSW.                         CL**2
00757                                                                      CL**2
00758      IF CO-ACCOUNT = LOW-VALUES  OR  ZEROS                           CL**2
00759          MOVE CO-RESP-NO         TO  P-ACCT-NUM                      CL**2
00760      ELSE                                                            CL**2
00761          MOVE CO-ACCOUNT         TO  P-ACCT-NUM.                     CL**2
00762                                                                      CL**2
00763      PERFORM 2100-PRT-RTN  THRU  2199-EXIT.                          CL**2
00764                                                                      CL**2
00765  0647-CREATE-B-RECORD.                                               CL**2
00766                                                                   ECS067
00767      PERFORM 1400-B-RECORD-ROUTINE  THRU  1499-EXIT.              ECS067
00768                                                                   ECS067
00769      GO TO 0600-READ-RTN.                                         ECS067
00770  EJECT                                                            ECS067
00771  0700-BUMP-RTN.                                                   ECS067
00772      IF DTE-CLIENT = 'ELI' OR  'ELT'                              ECS067
00773          GO TO 0799-EXIT.                                         ECS067
00774                                                                   ECS067
00775      IF CO-MAIL-NAME = SPACES  OR CO-ACCT-NAME                    ECS067
00776          NEXT SENTENCE                                            ECS067
00777      ELSE                                                         ECS067
00778          MOVE CO-MAIL-NAME       TO  CO-ACCT-NAME.                ECS067
00779                                                                   ECS067
00780      IF CO-ADDR-1 = SPACES                                        ECS067
00781          MOVE '2'                TO  PRT-1099-SWITCH              ECS067
00782          MOVE CO-ADDR-2          TO  CO-ADDR-1                    ECS067
00783 *        MOVE CO-ADDR-3          TO  CO-ADDR-2                    ECS067
               STRING CO-ADDR-CITY ' ' CO-ADDR-STATE
                 DELIMITED BY '  ' INTO CO-ADDR-2
               END-STRING
00784          MOVE SPACES             TO  CO-ADDR-3.                   ECS067
00785                                                                   ECS067
00786      IF CO-ADDR-2 = SPACES                                        ECS067
00787          MOVE '2'                TO  PRT-1099-SWITCH              ECS067
00788 *        MOVE CO-ADDR-3          TO  CO-ADDR-2                    ECS067
               STRING CO-ADDR-CITY ' ' CO-ADDR-STATE
                 DELIMITED BY '  ' INTO CO-ADDR-2
               END-STRING
00789          MOVE SPACES             TO  CO-ADDR-3.                   ECS067
00790                                                                   ECS067
00791      IF CO-ADDR-3 = SPACES                                        ECS067
00792          MOVE '2'                TO  PRT-1099-SWITCH.             ECS067
00793                                                                   ECS067
00794  0799-EXIT.                                                       ECS067
00795      EXIT.                                                        ECS067
00796  EJECT                                                            ECS067
00797  0800-SET-REMIT-RTN.                                              ECS067
00798      PERFORM 0900-LEFT-RTN  THRU  0999-EXIT.                      ECS067
00799                                                                   ECS067
00800      MOVE CO-ACCT-NAME           TO  REMIT-LN1.                   ECS067
00801      MOVE CO-ADDR-1              TO  REMIT-LN2.                   ECS067
00802      MOVE CO-ADDR-2              TO  REMIT-LN3.                   ECS067
00803 *    MOVE CO-ADDR-3              TO  REMIT-LN4.                   ECS067
           STRING CO-ADDR-CITY ' ' CO-ADDR-STATE
              DELIMITED BY '  ' INTO REMIT-LN4
           END-STRING
00804      MOVE CO-ZIP                 TO  WORK-ZIP-CODE.               ECS067
00805      MOVE SPACES                 TO  LINE5-ZIP-CODE.              ECS067
00806                                                                   ECS067
00807      IF WZC-POS-1  IS NOT NUMERIC                                 ECS067
00808          MOVE WZC-POST-CD1       TO  LINE5-POST-CODE1             ECS067
00809          MOVE WZC-POST-CD2       TO  LINE5-POST-CODE2             ECS067
00810      ELSE                                                         ECS067
00811          MOVE WZC-PRIME          TO  LINE5-ZIP-FIVE               ECS067
00812          IF WZC-PLUS4  IS EQUAL TO  '0000'  OR  '    '            ECS067
00813              MOVE ' '            TO  LINE5-ZIP-DASH               ECS067
00814              MOVE '    '         TO  LINE5-ZIP-FOUR               ECS067
00815          ELSE                                                     ECS067
00816              MOVE '-'            TO  LINE5-ZIP-DASH               ECS067
00817              MOVE WZC-PLUS4      TO  LINE5-ZIP-FOUR.              ECS067
00818                                                                   ECS067
00819      MOVE LINE5-ZIP-CODE         TO  REMIT-ZIP.                   ECS067
00820      MOVE CO-SOC-SEC             TO  REMIT-LN5.                   ECS067
00821                                                                   ECS067
00822      IF DTE-CLIENT  =  'NCL'                                      ECS067
00823         MOVE CO-SOC-SEC          TO  WS-SOC-SEC-NO                ECS067
00824         IF WS-SS-TYPE  =  'S' OR 'T' OR 'P' OR 'X' OR 'I'         ECS067
00825             MOVE WS-SS-NO        TO  REMIT-LN5.                   ECS067
00826                                                                   ECS067
00827      PERFORM 1100-BUMP-RTN  THRU  1199-EXIT.                      ECS067
00828                                                                   ECS067
00829      MOVE 'Y'                    TO  A-RECORD-SWITCH.             ECS067
00830                                                                   ECS067
00831  0899-EXIT.                                                       ECS067
00832      EXIT.                                                        ECS067
00833                                                                      CL**2
00834  0900-LEFT-RTN.                                                   ECS067
00835      IF CO-ACCT-NAME = SPACES                                     ECS067
00836          GO TO 0999-EXIT.                                         ECS067
00837                                                                   ECS067
00838      MOVE CO-ACCT-NAME           TO  A-NAME.                      ECS067
00839      MOVE SPACES                 TO  B-NAME.                      ECS067
00840      MOVE +1                     TO  A  B.                        ECS067
00841                                                                   ECS067
00842  0910-LOOP-A.                                                     ECS067
00843      IF A-CHAR (A) = SPACES                                       ECS067
00844          ADD +1                  TO  A                            ECS067
00845          GO TO 0910-LOOP-A.                                       ECS067
00846                                                                   ECS067
00847  0920-LOOP-B.                                                     ECS067
00848      MOVE A-CHAR (A)             TO  B-CHAR (B).                  ECS067
00849                                                                   ECS067
00850      ADD +1                      TO  A.                           ECS067
00851                                                                   ECS067
00852      IF A GREATER +030                                            ECS067
00853          GO TO 0930-MOVE-NAME.                                    ECS067
00854                                                                   ECS067
00855      ADD +1                      TO  B.                           ECS067
00856                                                                   ECS067
00857      GO TO 0920-LOOP-B.                                           ECS067
00858                                                                   ECS067
00859  0930-MOVE-NAME.                                                  ECS067
00860      MOVE B-NAME                 TO  CO-ACCT-NAME.                ECS067
00861                                                                   ECS067
00862  0999-EXIT.                                                       ECS067
00863      EXIT.                                                        ECS067
00864  EJECT                                                            ECS067
00865  1000-BREAK-RTN.                                                  ECS067
00866      IF CUR-CARR = LOW-VALUE                                      ECS067
00867          GO TO 1010-INIT-BREAK.                                   ECS067
00868                                                                   ECS067
00869      PERFORM 2000-PRT-HD-RTN  THRU  2099-EXIT.                    ECS067
00870                                                                   ECS067
00871      MOVE SPACE-3                TO  P-CCSW.                      ECS067
00872      MOVE TOTAL-1099             TO  P-1099.                      ECS067
00873      MOVE TOTAL-COMPENSATION     TO  P-TOTAL.                     ECS067
00874                                                                   ECS067
00875      PERFORM 2100-PRT-RTN  THRU  2199-EXIT.                       ECS067
00876                                                                   ECS067
00877      MOVE SPACE-2                       TO  P-CCSW.               ECS067
00878      MOVE 'TOTAL 1099 FORMS FOR CARR '  TO  P-ADDR.               ECS067
00879      MOVE CUR-CARR                      TO  P-CARR.               ECS067
00880                                                                   ECS067
00881      PERFORM 2100-PRT-RTN  THRU  2199-EXIT.                       ECS067
00882                                                                   ECS067
00883      IF TOTAL-1099 NOT = ZEROS                                    ECS067
00884          PERFORM 1500-C-RECORD-ROUTINE THRU 1599-EXIT.            ECS067
00885                                                                   ECS067
00886      ADD TOTAL-COMPENSATION      TO  FINAL-COMPENSATION.          ECS067
00887      ADD TOTAL-1099              TO  FINAL-1099.                  ECS067
00888  EJECT                                                            ECS067
00889  1010-INIT-BREAK.                                                 ECS067
00890      IF CO-CARRIER = HIGH-VALUE                                   ECS067
00891          PERFORM 2000-PRT-HD-RTN  THRU  2099-EXIT                 ECS067
00892          MOVE SPACE-3             TO  P-CCSW                      ECS067
00893          MOVE FINAL-1099          TO  P-1099                      ECS067
00894          MOVE FINAL-COMPENSATION  TO  P-TOTAL                     ECS067
00895          PERFORM 2100-PRT-RTN  THRU  2199-EXIT                    ECS067
00896          MOVE SPACE-2             TO  P-CCSW                      ECS067
00897          MOVE 'TOTAL 1099 FORMS'  TO  P-ADDR                      ECS067
00898          PERFORM 2100-PRT-RTN  THRU  2199-EXIT                    ECS067
00899          PERFORM 1600-F-RECORD-ROUTINE  THRU  1699-EXIT           ECS067
00900          GO TO 1099-EXIT.                                         ECS067
00901                                                                   ECS067
00902      MOVE +0                     TO  TOTAL-1099                   ECS067
00903                                      TOTAL-COMPENSATION.          ECS067
00904      MOVE CO-CARRIER             TO  CUR-CARR.                    ECS067
00905      MOVE SPACES                 TO  REMIT-AREA.                  ECS067
00906      MOVE CNT-NAME (1)           TO  REMIT-LN1.                   ECS067
00907      MOVE CNT-NAME (2)           TO  REMIT-LN2.                   ECS067
00908      MOVE CNT-NAME (3)           TO  REMIT-LN3.                   ECS067
00909      MOVE CNT-NAME (4)           TO  REMIT-LN4.                   ECS067
00910      MOVE CNT-NAME (5)           TO  TELE-ZIP-LINE.               ECS067
00911      MOVE SPACES                 TO  REMIT-LN4A.                  ECS067
00912      MOVE LINE5-ZIP-CODE         TO  REMIT-ZIP.                   ECS067
00913      MOVE CNT-NAME (6)           TO  TEST-IT.                     ECS067
00914                                                                   ECS067
00915      IF TEST-FIVE = 'XX-XX'  OR  '??-??'                          ECS067
00916          MOVE SPACES             TO  REMIT-LN5                    ECS067
00917      ELSE                                                         ECS067
00918          MOVE CNT-NAME (6)       TO  REMIT-LN5.                   ECS067
00919                                                                   ECS067
00920      PERFORM 1100-BUMP-RTN  THRU  1199-EXIT.                      ECS067
00921                                                                   ECS067
00922      MOVE 'Y'                    TO  A-RECORD-SWITCH.             ECS067
00923                                                                   ECS067
00924  1099-EXIT.                                                       ECS067
00925      EXIT.                                                        ECS067
00926  EJECT                                                            ECS067
00927  1100-BUMP-RTN.                                                   ECS067
00928      IF REMIT-LN1 = SPACES                                        ECS067
00929          MOVE REMIT-LN2          TO  REMIT-LN1                    ECS067
00930          MOVE REMIT-LN3          TO  REMIT-LN2                    ECS067
00931          MOVE REMIT-LN4          TO  REMIT-LN3                    ECS067
00932          MOVE SPACES             TO  REMIT-LN4.                   ECS067
00933                                                                   ECS067
00934      IF REMIT-LN2 = SPACES                                        ECS067
00935          MOVE REMIT-LN3          TO  REMIT-LN2                    ECS067
00936          MOVE REMIT-LN4          TO  REMIT-LN3                    ECS067
00937          MOVE SPACES             TO  REMIT-LN4.                   ECS067
00938                                                                   ECS067
00939      IF REMIT-LN3 = SPACES                                        ECS067
00940          MOVE REMIT-LN4          TO  REMIT-LN3                    ECS067
00941          MOVE SPACES             TO  REMIT-LN4.                   ECS067
00942                                                                   ECS067
00943  1199-EXIT.                                                       ECS067
00944      EXIT.                                                        ECS067
00945  EJECT                                                            ECS067
00946  1200-A-RECORD-ROUTINE.                                           ECS067
00947      IF FIRST-A                                                   ECS067
00948          MOVE 'N'                TO  FIRST-A-SWITCH               ECS067
00949      ELSE                                                         ECS067
00950          IF RECORDS-IN-BLOCK LESS +23                             ECS067
00951              PERFORM 1700-NINES-RECORD  THRU  1799-EXIT.          ECS067
00952                                                                   ECS067
00953      MOVE SPACES                 TO  GOVT-REC-TYPE-A.             ECS067
00954      MOVE 'A'                    TO  A-RECORD-TYPE.               ECS067
00955      MOVE RUN-YR                 TO  A-PAYMENT-YEAR.              ECS067
00956      MOVE '001'                  TO  A-REEL-SEQ-NO.               ECS067
00957      MOVE REMIT-LN5              TO  TEST-SOC-SEC.                ECS067
00958                                                                   ECS067
00959      PERFORM 1900-EDIT-SOC-SEC  THRU  1999-EXIT.                  ECS067
00960                                                                   ECS067
00961      MOVE NEW-SOC-SEC            TO  A-PAYER-FED-EIN.             ECS067
00962      MOVE SPACES                 TO  A-PAYER-NAME-CONTROL.        ECS067
00963      MOVE 'LS'                   TO  A-TAPE-FILER-INDICATOR.      ECS067
00964      MOVE SPACE                  TO  A-COMB-FED-ST.               ECS067
00965      MOVE 'A'                    TO  A-TYP-OF-RETURN.             ECS067
00966      MOVE '7        '            TO  A-AMT-INDICATOR.             ECS067
00967      MOVE TRANSMITTER-CNTL-CODE  TO  A-TRNSMITR-CNTRL-CODE.       ECS067
00968      MOVE ' '                    TO  A-FOREIGN-CORP-INDCTR.       ECS067
00969      MOVE REMIT-LN1              TO  A-FIRST-PAYER-NAME.          ECS067
00970      MOVE SPACES                 TO  A-SECOND-PAYER-NAME.         ECS067
00971      MOVE '0'                    TO  A-TRNSFR-AGT-INDICATOR.      ECS067
00972      MOVE REMIT-LN2              TO  A-PAYER-SHIPPNG-ADDR.        ECS067
00973                                                                   ECS067
00974      IF REMIT-LN4 = SPACES                                        ECS067
00975          MOVE REMIT-LN3          TO  A-PAYER-CTY-ST-ZIP           ECS067
00976      ELSE                                                         ECS067
00977          MOVE REMIT-LN4          TO  A-PAYER-CTY-ST-ZIP.          ECS067
00978                                                                   ECS067
00979      MOVE REMIT-ZIP              TO  A-PAYER-ZIP.                 ECS067
00980      MOVE TRANSMITTER-CNTL-NAME  TO  A-TRNSMITR-NAME.             ECS067
00981      MOVE TRANSMITTER-CNTL-ADDR  TO  A-TRNSMITR-ADDR.             ECS067
00982      MOVE TRANSMITTER-CNTL-CITY  TO  A-TRNSMITR-CTY-ST-ZIP.       ECS067
00983                                                                   ECS067
00984      ADD 1                       TO  NUMBER-OF-PAYERS.            ECS067
00985                                                                   ECS067
00986      PERFORM 1800-WRITE-GOVT-RECORD  THRU  1899-EXIT.             ECS067
00987                                                                   ECS067
00988  1299-EXIT.                                                       ECS067
00989      EXIT.                                                        ECS067
00990  EJECT                                                            ECS067
00991  1300-B-RECORD-ROUTINE.                                           ECS067
00992      MOVE SPACES                 TO  GOVT-REC-TYPE-A.             ECS067
00993      MOVE 'B'                    TO  B-RECORD-TYPE.               ECS067
00994      MOVE RUN-YR                 TO  B-PAYMENT-YEAR.              ECS067
00995      MOVE '0 '                   TO  B-DOCUMNT-SPCFIC-CODE.       ECS067
00996      MOVE ' '                    TO  B-CORRECTION.                ECS067
00997      MOVE SPACES                 TO  B-NAME-CONTROL.              ECS067
00998      MOVE CO-SOC-SEC             TO  TEST-SOC-SEC.                ECS067
00999                                                                   ECS067
01000      IF T-SOC-SEC (3) = '-'                                       ECS067
01001          MOVE '1'                TO  B-TYPE-OF-TIN                ECS067
01002      ELSE                                                         ECS067
01003          IF T-SOC-SEC (4) = '-'                                   ECS067
01004              MOVE '2'            TO  B-TYPE-OF-TIN                ECS067
01005          ELSE                                                     ECS067
01006              MOVE ' '            TO  B-TYPE-OF-TIN.               ECS067
01007                                                                   ECS067
01008      PERFORM 1900-EDIT-SOC-SEC  THRU  1999-EXIT.                  ECS067
01009                                                                   ECS067
01010      MOVE NEW-SOC-SEC            TO  B-TAXPAYER-ID-NO.            ECS067
01011                                                                   ECS067
01012      IF CO-ACCOUNT = LOW-VALUES  OR  ZEROS                        ECS067
01013          MOVE CO-RESP-NO         TO  B-ACCT-NO-FOR-PAYEE          ECS067
01014      ELSE                                                         ECS067
01015          MOVE CO-ACCOUNT         TO  B-ACCT-NO-FOR-PAYEE.         ECS067
01016                                                                   ECS067
01017      MOVE ZEROS                  TO  B-PAYMENT-AMOUNT-01             CL**2
01018                                      B-PAYMENT-AMOUNT-02             CL**2
01019                                      B-PAYMENT-AMOUNT-03             CL**2
01020                                      B-PAYMENT-AMOUNT-04             CL**2
01021                                      B-PAYMENT-AMOUNT-05             CL**2
01022                                      B-PAYMENT-AMOUNT-06.            CL**2
01023      MOVE CO-YTD-COM             TO  B-PAYMENT-AMOUNT-07.         ECS067
01024      MOVE ZEROS                  TO  B-PAYMENT-AMOUNT-08.         ECS067
01025      MOVE ZEROS                  TO  B-PAYMENT-AMOUNT-09.         ECS067
01026      MOVE ' '                    TO  B-FRGN-CNTRY-INDCTR.         ECS067
01027      MOVE CO-ACCT-NAME           TO  B-FIRST-PAYEE-NAME.          ECS067
01028      MOVE SPACES                 TO  B-SECOND-PAYEE-NAME.         ECS067
01029      MOVE CO-ADDR-1              TO  B-PAYEE-MAILING-ADDR.        ECS067
051810     MOVE SPACES                 TO  B-PAYEE-CTY-ST.              ECS067
051810     STRING CO-ADDR-CITY ' ' CO-ADDR-STATE
051810        DELIMITED BY '  ' INTO B-PAYEE-CTY-ST
051810     END-STRING
01031      MOVE CO-ZIP                 TO  B-PAYEE-ZIP.                 ECS067
01032      MOVE SPACES                 TO  B-PAYEE-ZIP-4.               ECS067
01033                                                                   ECS067
01034      ADD 1                       TO  NUMBER-OF-PAYEES.            ECS067
01035                                                                   ECS067
01036      PERFORM 1800-WRITE-GOVT-RECORD  THRU  1899-EXIT.             ECS067
01037                                                                   ECS067
01038  1399-EXIT.                                                       ECS067
01039      EXIT.                                                        ECS067
01040  EJECT                                                            ECS067
01041  1400-B-RECORD-ROUTINE.                                           ECS067
01042      MOVE SPACES                 TO  GOVT-REC-TYPE-A.             ECS067
01043      MOVE 'B'                    TO  B-RECORD-TYPE.               ECS067
01044      MOVE RUN-YR                 TO  B-PAYMENT-YEAR.              ECS067
01045      MOVE '0'                    TO  B-DOCUMNT-SPCFIC-CODE.       ECS067
01046      MOVE ' '                    TO  B-CORRECTION.                ECS067
01047      MOVE SPACES                 TO  B-NAME-CONTROL.              ECS067
01048      MOVE CO-SOC-SEC             TO  TEST-SOC-SEC.                ECS067
01049                                                                   ECS067
01050      IF T-SOC-SEC (3) = '-'                                       ECS067
01051          MOVE '1'                TO  B-TYPE-OF-TIN                ECS067
01052      ELSE                                                         ECS067
01053          IF T-SOC-SEC (4) = '-'                                   ECS067
01054              MOVE '2'            TO  B-TYPE-OF-TIN                ECS067
01055          ELSE                                                     ECS067
01056              MOVE ' '            TO  B-TYPE-OF-TIN.               ECS067
01057                                                                   ECS067
01058      PERFORM 1900-EDIT-SOC-SEC  THRU  1999-EXIT.                  ECS067
01059                                                                   ECS067
01060      MOVE NEW-SOC-SEC            TO  B-TAXPAYER-ID-NO.            ECS067
01061                                                                   ECS067
01062      IF CO-ACCOUNT = LOW-VALUES  OR  ZEROS                        ECS067
01063          MOVE CO-RESP-NO         TO  B-ACCT-NO-FOR-PAYEE          ECS067
01064      ELSE                                                         ECS067
01065          MOVE CO-ACCOUNT         TO  B-ACCT-NO-FOR-PAYEE.         ECS067
01066                                                                   ECS067
01067      MOVE ZEROS                  TO  B-PAYMENT-AMOUNT-01             CL**2
01068                                      B-PAYMENT-AMOUNT-02             CL**2
01069                                      B-PAYMENT-AMOUNT-03             CL**2
01070                                      B-PAYMENT-AMOUNT-04             CL**2
01071                                      B-PAYMENT-AMOUNT-05             CL**2
01072                                      B-PAYMENT-AMOUNT-06.            CL**2
01073      MOVE CO-YTD-COM             TO  B-PAYMENT-AMOUNT-07.         ECS067
01074      MOVE ZEROS                  TO  B-PAYMENT-AMOUNT-08.         ECS067
01075      MOVE ZEROS                  TO  B-PAYMENT-AMOUNT-09.         ECS067
01076      MOVE ' '                    TO  B-FRGN-CNTRY-INDCTR.         ECS067
01077      MOVE CO-ACCT-NAME           TO  B-FIRST-PAYEE-NAME.          ECS067
01078      MOVE SPACES                 TO  B-SECOND-PAYEE-NAME.         ECS067
01079      MOVE CO-ADDR-1              TO  B-PAYEE-MAILING-ADDR.        ECS067
01080      MOVE CO-ADDR-2              TO  B-PAYEE-CTY-ST.              ECS067
01081      MOVE CO-ZIP                 TO  B-PAYEE-ZIP.                 ECS067
01082      MOVE SPACES                 TO  B-PAYEE-ZIP-4.               ECS067
01083                                                                   ECS067
01084      ADD 1                       TO  NUMBER-OF-PAYEES.            ECS067
01085                                                                   ECS067
01086      PERFORM 1800-WRITE-GOVT-RECORD  THRU  1899-EXIT.             ECS067
01087                                                                   ECS067
01088  1499-EXIT.                                                       ECS067
01089      EXIT.                                                        ECS067
01090  EJECT                                                            ECS067
01091  1500-C-RECORD-ROUTINE.                                           ECS067
01092      MOVE SPACES                 TO  GOVT-REC-TYPE-A.             ECS067
01093      MOVE 'C'                    TO  C-RECORD-TYPE.               ECS067
01094      MOVE NUMBER-OF-PAYEES       TO  C-NUMBER-OF-PAYEES.          ECS067
01095      MOVE ZEROS                  TO  NUMBER-OF-PAYEES.            ECS067
01096      MOVE ZEROS                  TO  C-CONTROL-TOTAL-1            ECS067
01097                                      C-CONTROL-TOTAL-2            ECS067
01098                                      C-CONTROL-TOTAL-3            ECS067
01099                                      C-CONTROL-TOTAL-4            ECS067
01100                                      C-CONTROL-TOTAL-5            ECS067
01101                                      C-CONTROL-TOTAL-6.           ECS067
01102      MOVE TOTAL-COMPENSATION     TO  C-CONTROL-TOTAL-7.           ECS067
01103      MOVE ZEROS                  TO  C-CONTROL-TOTAL-8            ECS067
01104                                      C-CONTROL-TOTAL-9.           ECS067
01105                                                                   ECS067
01106      PERFORM 1800-WRITE-GOVT-RECORD  THRU  1899-EXIT.             ECS067
01107                                                                   ECS067
01108  1599-EXIT.                                                       ECS067
01109      EXIT.                                                        ECS067
01110  EJECT                                                            ECS067
01111  1600-F-RECORD-ROUTINE.                                           ECS067
01112      MOVE SPACES                 TO  GOVT-REC-TYPE-A.             ECS067
01113      MOVE 'F'                    TO  F-RECORD-TYPE.               ECS067
01114      MOVE NUMBER-OF-PAYERS       TO  F-NUMBER-OF-A-RECORDS.       ECS067
01115                                                                   ECS067
01116 *** THE FOLLOWING STATEMENT MUST BE CHANGED FOR PROGRAMS          ECS067
01117 *** IN THE FIELD - IF 6250 BPI IT WILL BE OK - OTHERWISE          ECS067
01118 *** THE NUMBER OF REELS WILL DEPEND ON BPI OF OUTPUT TAPES.       ECS067
01119                                                                   ECS067
01120      MOVE ZEROS                  TO  F-ZEROS.                     ECS067
01121                                                                   ECS067
01122      PERFORM 1800-WRITE-GOVT-RECORD  THRU  1899-EXIT.             ECS067
01123                                                                   ECS067
01124  1699-EXIT.                                                       ECS067
01125      EXIT.                                                        ECS067
01126  EJECT                                                            ECS067
01127  1700-NINES-RECORD.                                               ECS067
01128                                                                   ECS067
01129  1710-DO-PADS.                                                    ECS067
01130      MOVE ALL '9'                TO  GOVT-REC-TYPE-A.             ECS067
01131                                                                   ECS067
01132      PERFORM 1800-WRITE-GOVT-RECORD  THRU  1899-EXIT.             ECS067
01133                                                                   ECS067
01134      IF RECORDS-IN-BLOCK LESS +23                                 ECS067
01135          GO TO 1710-DO-PADS.                                      ECS067
01136                                                                   ECS067
01137  1799-EXIT.                                                       ECS067
01138      EXIT.                                                        ECS067
01139  EJECT                                                            ECS067
01140  1800-WRITE-GOVT-RECORD.                                          ECS067
01141      WRITE GOVT-REC  FROM  GOVT-REC-TYPE-A.                       ECS067
01142                                                                   ECS067
01143      ADD +1  TO  RECORDS-IN-BLOCK.                                ECS067
01144                                                                   ECS067
01145      IF RECORDS-IN-BLOCK GREATER +23                              ECS067
01146          MOVE +1                 TO  RECORDS-IN-BLOCK.            ECS067
01147                                                                   ECS067
01148  1899-EXIT.                                                       ECS067
01149      EXIT.                                                        ECS067
01150  EJECT                                                            ECS067
01151  1900-EDIT-SOC-SEC.                                               ECS067
01152      IF TEST-SOC-SEC = SPACES                                     ECS067
01153          MOVE SPACES             TO  NEW-SOC-SEC                  ECS067
01154          GO TO 1999-EXIT.                                         ECS067
01155                                                                   ECS067
01156      MOVE SPACES                 TO  NEW-SOC-SEC.                 ECS067
01157      MOVE +1                     TO  C  D.                        ECS067
01158                                                                   ECS067
01159  1910-LOOP-1.                                                     ECS067
01160      IF T-SOC-SEC (C)  NUMERIC                                    ECS067
01161          MOVE T-SOC-SEC (C)      TO  N-SOC-SEC (D)                ECS067
01162          ADD +1                  TO  D                            ECS067
01163          IF D GREATER +9                                          ECS067
01164              GO TO 1999-EXIT.                                     ECS067
01165                                                                   ECS067
01166      ADD +1                      TO  C.                           ECS067
01167                                                                   ECS067
01168      GO TO 1910-LOOP-1.                                           ECS067
01169                                                                   ECS067
01170  1999-EXIT.                                                       ECS067
01171      EXIT.                                                        ECS067
01172  EJECT                                                            ECS067
01173  2000-PRT-HD-RTN.                                                 ECS067
01174      MOVE SPACE-NP               TO  P-CCSW.                      ECS067
01175                                                                   ECS067
01176      PERFORM 2100-PRT-RTN  THRU  2199-EXIT.                       ECS067
01177                                                                   ECS067
01178 ***  MOVE SPACE-1                TO  P-CCSW.                      ECS067
01179 ***  MOVE 'XX'                   TO  P-CORR.                      ECS067
01180                                                                   ECS067
01181 ***  PERFORM 2100-PRT-RTN  THRU  2199-EXIT.                       ECS067
01182                                                                   ECS067
01183 ***  MOVE SPACE-2                TO  P-CCSW.                      ECS067
01184      MOVE SPACE-1                TO  P-CCSW.                      ECS067
01185      MOVE REMIT-LN1              TO  P-ADDR.                      ECS067
01186                                                                   ECS067
01187      PERFORM 2100-PRT-RTN  THRU  2199-EXIT.                       ECS067
01188                                                                   ECS067
01189      MOVE SPACE-1                TO  P-CCSW.                      ECS067
01190      MOVE REMIT-LN2              TO  P-ADDR.                      ECS067
01191                                                                   ECS067
01192      PERFORM 2100-PRT-RTN  THRU  2199-EXIT.                       ECS067
01193                                                                   ECS067
01194      MOVE SPACE-1                TO  P-CCSW.                      ECS067
01195      MOVE REMIT-LN3              TO  P-ADDR.                      ECS067
01196                                                                   ECS067
01197      PERFORM 2100-PRT-RTN  THRU  2199-EXIT.                       ECS067
01198                                                                   ECS067
01199      MOVE SPACE-1                TO  P-CCSW.                      ECS067
01200                                                                   ECS067
01201      IF REMIT-LN4 = SPACES                                        ECS067
01202          MOVE REMIT-LN4A         TO  P-ADDR                       ECS067
01203      ELSE                                                         ECS067
01204          MOVE REMIT-LN4          TO  P-ADDR.                      ECS067
01205                                                                   ECS067
01206      PERFORM 2100-PRT-RTN  THRU  2199-EXIT.                       ECS067
01207                                                                   ECS067
01208      MOVE SPACE-1                TO  P-CCSW.                      ECS067
01209                                                                   ECS067
01210      IF REMIT-LN4 = SPACES                                        ECS067
01211          MOVE SPACES             TO  P-ADDR                       ECS067
01212      ELSE                                                         ECS067
01213          MOVE REMIT-LN4A         TO  P-ADDR.                      ECS067
01214                                                                   ECS067
01215      PERFORM 2100-PRT-RTN  THRU  2199-EXIT.                       ECS067
01216                                                                   ECS067
01217  2099-EXIT.                                                       ECS067
01218      EXIT.                                                        ECS067
01219  EJECT                                                            ECS067
01220  2100-PRT-RTN.                                                    ECS067
01221      MOVE P-CCSW                 TO  X.                           ECS067
01222      MOVE P-REC                  TO  PRT.                         ECS067
01223      MOVE SPACE-1                TO  P-REC.                       ECS067
01224                                                                   ECS067
01225  2120-PRT-COPY.                                                   ECS067
01226                              COPY ELCPRT2.                        ECS067
01227                                                                   ECS067
01228  2199-EXIT.                                                       ECS067
01229      EXIT.                                                        ECS067
01230  EJECT                                                            ECS067
01231  2200-END-OF-JOB.                                                 ECS067
01232      MOVE HIGH-VALUE             TO  CO-CARRIER.                  ECS067
01233                                                                   ECS067
01234      PERFORM 1000-BREAK-RTN  THRU  1099-EXIT.                     ECS067
01235 *                                                                 ECS067
01236 *                                                                 ECS067
01237 * LEAVE THE ABOVE 2 LINE IN TO PREVENT LOSING THE LINE ABOVE      ECS067
01255                                                                   ECS067
01256  2300-CLOSE-FILES.                                                ECS067
CIDDMOD    DISPLAY '***********************************************'.   00010373
CIDDMOD    DISPLAY '*'.                                                 00010374
CIDDMOD    DISPLAY 'CSO 1099 ACCEPTED = ' CSO-1099-CNT.                 00010375
CIDDMOD    DISPLAY '*'.                                                 00010376
CIDDMOD    DISPLAY 'CSO 1099 BYPASSED = ' CSO-1099-BYP.                 00010377
CIDDMOD    DISPLAY '*'.                                                 00010378
CIDDMOD    DISPLAY '***********************************************'.   00010379
01257      CLOSE WORK-COMM-MSTR                                         ECS067
01258            GOVT-TAPE-OUT.                                         ECS067
01259                                                                   ECS067
01260  2310-COPY-CLOSE.                                                 ECS067
01261                              COPY ELCPRTC.                        ECS067
01262  EJECT                                                            ECS067
01263  9000-STOP-RUN.                                                   ECS067
01264      GOBACK.                                                      ECS067
01265                                                                   ECS067
01266  9999-ABORT.                                                      ECS067
01267                                                                   ECS067
01268  ABEND-PGM SECTION.                                               ECS067
01269                              COPY ELCABEND.                       ECS067
