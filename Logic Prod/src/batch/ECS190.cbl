00001  IDENTIFICATION DIVISION.                                         08/27/98
00002                                                                   ECS190
00003  PROGRAM-ID.                 ECS190.                                 LV009
00004 *              PROGRAM CONVERTED BY                               ECS190
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ECS190
00006 *              CONVERSION DATE 04/21/98 09:17:57.                 ECS190
00007 *               PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE         ECS190
00008 *                            VMOD=2.005.                          ECS190
00009 *AUTHOR.        LOGIC INC.                                        ECS190
00010 *               DALLAS, TEXAS.                                    ECS190
00011 *                                                                 ECS190
00012 *DATE-COMPILED.                                                   ECS190
00013 *                                                                 ECS190
00014 *SECURITY.   *****************************************************ECS190
00015 *            *                                                   *ECS190
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC INC.      *ECS190
00017 *            *                                                   *ECS190
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS190
00019 *            *   OF LOGIC INC. IS EXPRESSLY PROHIBITED WITHOUT   *ECS190
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *ECS190
00021 *            *                                                   *ECS190
00022 *            *****************************************************ECS190
00023 *                                                                 ECS190
00024 *REMARKS.                                                         ECS190
00025 *        READS CPTR FROM ECS017 AND CREATES GENERAL LEDGER        ECS190
00026 *        INTERFACE RECORDS FOR INCOME (80 BYTE BLOCKED 10).       ECS190
00027                                                                   ECS190
00028  ENVIRONMENT DIVISION.                                            ECS190
00029  INPUT-OUTPUT SECTION.                                            ECS190
00030  FILE-CONTROL.                                                    ECS190
00031                                                                   ECS190
00032      SELECT SORT-FILE        ASSIGN TO SYS001-UT-2314-S-SORTWK1.  ECS190
00033      SELECT PRINT-FILE       ASSIGN TO SYS008-UR-1403-S-SYS008.   ECS190
00034      SELECT COMM-TRANS-IN    ASSIGN TO SYS010-UT-2400-S-SYS010.   ECS190
00035      SELECT ABC-INCOME       ASSIGN TO SYS011-UT-2400-S-SYS011.   ECS190
00036      SELECT DISK-DATE        ASSIGN TO SYS019-UT-2314-S-SYS019.   ECS190
00037      SELECT FICH             ASSIGN TO SYS020-UT-2400-S-SYS020.   ECS190
00038  EJECT                                                            ECS190
00039  DATA DIVISION.                                                   ECS190
00040  FILE SECTION.                                                    ECS190
00041                                                                   ECS190
00042  SD  SORT-FILE                                                    ECS190
00043                      .                                            ECS190
00044                                                                   ECS190
00045  01  SORT-REC.                                                    ECS190
00046      12  FILLER              PIC  X(4).                           ECS190
00047      12  SORT-KEY1           PIC  X(7).                           ECS190
00048      12  SORT-KEY3           PIC  X(10).                          ECS190
00049      12  FILLER              PIC  X(11).                          ECS190
00050      12  SORT-KEY2           PIC  XX.                             ECS190
00051      12  FILLER              PIC  X(236).                         ECS190
00052  EJECT                                                            ECS190
00053  FD  PRINT-FILE                                                   ECS190
00054                              COPY ELCPRTFD.                       ECS190
00055  EJECT                                                            ECS190
00056  FD  COMM-TRANS-IN                                                ECS190
00057                              COPY ECSCOMFD.                       ECS190
00058  EJECT                                                            ECS190
00059  FD  ABC-INCOME                                                   ECS190
00060      BLOCK CONTAINS 0 RECORDS
00061      RECORDING MODE F.                                            ECS190
00062                                                                   ECS190
00063  01  ABC-INCOME-REC          PIC  X(80).                          ECS190
00064  EJECT                                                            ECS190
00065  FD  DISK-DATE                                                    ECS190
00066                              COPY ELCDTEFD.                       ECS190
00067  EJECT                                                            ECS190
00068  FD  FICH                                                         ECS190
00069                              COPY ELCFCHFD.                       ECS190
00070  EJECT                                                            ECS190
00071  WORKING-STORAGE SECTION.                                         ECS190
00072  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      ECS190
00073  77  FILLER  PIC  X(32) VALUE '********************************'. ECS190
00074  77  FILLER  PIC  X(32) VALUE '     ECS190 WORKING-STORAGE     '. ECS190
00075  77  FILLER  PIC  X(32) VALUE '*********VMOD=2.005*************'. ECS190
00076                                                                   ECS190
00077  77  S1                      PIC S9(4)   COMP    VALUE +00.       ECS190
00078  77  S2                      PIC S9(4)   COMP    VALUE +00.       ECS190
00079  77  LINE-CNT                PIC S9(3)   COMP-3  VALUE +99.       ECS190
00080  77  PAGE-CNT                PIC S9(3)   COMP-3  VALUE +0.        ECS190
00081  77  PGM-SUB                 PIC S9(3)   COMP-3  VALUE +190.      ECS190
00082  77  X                       PIC  X              VALUE SPACE.     ECS190
00083  77  SPACE-NP                PIC  X              VALUE '1'.       ECS190
00084  77  SPACE-1                 PIC  X              VALUE ' '.       ECS190
00085  77  SPACE-2                 PIC  X              VALUE '0'.       ECS190
00086  77  SPACE-3                 PIC  X              VALUE '-'.       ECS190
00087  77  PREV-CARR               PIC  X              VALUE LOW-VALUE. ECS190
00088  77  PREV-COMP               PIC  X(6)           VALUE LOW-VALUE. ECS190
00089  77  PREV-STATE              PIC  XX             VALUE LOW-VALUE. ECS190
00090  77  PREV-ACCT               PIC  X(10)          VALUE LOW-VALUE. ECS190
00091  77  HOLD-STATE              PIC  XX.                             ECS190
00092                                                                   ECS190
00093  01  REQUIRED-STORAGE.                                            ECS190
00094      12  WS-RETURN-CODE          PIC S9(4)              COMP.     ECS190
00095      12  WS-ABEND-MESSAGE        PIC  X(80).                      ECS190
00096      12  WS-ABEND-FILE-STATUS    PIC  X(2)   VALUE ZEROS.         ECS190
00097      12  WS-ZERO                 PIC S9(1)   VALUE ZERO COMP-3.   ECS190
00098      12  3800-DATE-1-R           PIC 9(9).                           CL**8
00099      12  3800-DATE-1-RN REDEFINES 3800-DATE-1-R.                     CL**8
00100          16  FILLER              PIC  999.                        ECS190
00101          16  3800-YEAR           PIC  99.                            CL**7
00102          16  3800-MONTH          PIC  99.                         ECS190
00103          16  3800-DAY            PIC  99.                         ECS190
00104      12  WS-3800-JULIAN-DATE     PIC  9(9).                          CL**8
00105      12  WS-3800-JULIAN-DATE-R REDEFINES WS-3800-JULIAN-DATE.        CL**8
00106          16  FILLER              PIC  99.                            CL**8
00107          16  3800-JULIAN-CC      PIC  99.                            CL**8
00108          16  3800-JULIAN-YEAR    PIC  99.                            CL**8
00109          16  3800-JULIAN-DAY     PIC  9(3).                          CL**8
00110  EJECT                                                            ECS190
00111 ******************************************************************ECS190
00112 ******************************************************************ECS190
00113 **                                                              **ECS190
00114 **    THIS IS THE FORMAT OF THE INPUT TO THE GENERAL LEDGER     **ECS190
00115 **    SYSTEM.  IT MAY BE NECESSARY TO MODIFY THIS FORMAT TO     **ECS190
00116 **    MEET THE REQUIREMENTS OF YOUR GENERAL LEDGER SYSTEM.      **ECS190
00117 **                                                              **ECS190
00118 ******************************************************************ECS190
00119 ******************************************************************ECS190
00120                                                                   ECS190
00121  01  WS-3800-CARD.                                                ECS190
00122      12  3800-LITERAL            PIC  X(6)       VALUE '0LG00 '.  ECS190
00123      12  3800-DATE-1             PIC  9(09)      COMP-3.          ECS190
00124      12  3800-VOUCHER-ID         PIC  X(5)       VALUE '*****'.   ECS190
00125      12  3800-JULIAN-DATE        PIC  9(9)       COMP-3.             CL**8
00126      12  3800-GENERAL-LEDGER     PIC  X(7).                       ECS190
00127      12  3800-U-LITERAL          PIC  X          VALUE 'U'.       ECS190
00128      12  3800-AMOUNT             PIC  9(7)V99.                    ECS190
00129      12  3800-DEBIT-CREDIT       PIC  X.                          ECS190
00130      12  3800-DATE-2             PIC  X(6).                       ECS190
00131      12  3800-DESCRIPTION.                                        ECS190
00132          16  3800-ACCOUNT        PIC  X(10).                      ECS190
00133          16  3800-DESC           PIC  X(12)      VALUE            ECS190
00134                  'MONTHEND    '.                                  ECS190
00135          16  3800-SYSTEM         PIC  X          VALUE 'C'.       ECS190
00136      12  FILLER                  PIC  X          VALUE SPACES.    ECS190
00137      12  3800-STATE              PIC  XX.                         ECS190
00138      12  3800-COMPANY            PIC  X(3).                       ECS190
00139      12  FILLER                  PIC  X          VALUE SPACES.    ECS190
00140      12  3800-MISC-MONTH-YEAR    PIC  X(4)       VALUE '0000'.    ECS190
00141  EJECT                                                            ECS190
00142 ******************************************************************ECS190
00143 ******************************************************************ECS190
00144 **                                                              **ECS190
00145 **    THIS TABLE CONTAINS THE GENERAL LEDGER ACCOUNT NUMBERS    **ECS190
00146 **    FOR THE ACCTS RECEIVABLE, PREMIUM, COMMISSION, AND THE    **ECS190
00147 **    OVERRIDE COMMISSION ACCOUNTS.                             **ECS190
00148 **                                                              **ECS190
00149 **    BASED UPON CARRIER AND GROUPING CODE MODIFY THIS TABLE    **ECS190
00150 **    BY ADDING THE ACCOUNT NUMBERS FROM YOUR PARTICULAR        **ECS190
00151 **    CHART OF ACCOUNTS.                                        **ECS190
00152 **                                                              **ECS190
00153 ******************************************************************ECS190
00154 ******************************************************************ECS190
00155                                                                   ECS190
00156  01  GENERAL-LEDGER-TABLE.                                        ECS190
00157      12  FILLER              PIC  X(25)          VALUE            ECS190
00158              'U001163000040703024070902'.                         ECS190
00159      12  FILLER              PIC  X(28)          VALUE            ECS190
00160              '5270702527071252607035260712'.                      ECS190
00161      12  FILLER              PIC  X(25)          VALUE            ECS190
00162              'U002163000040703004070900'.                         ECS190
00163      12  FILLER              PIC  X(28)          VALUE            ECS190
00164              '5270700527070452607005260704'.                      ECS190
00165      12  FILLER              PIC  X(25)          VALUE            ECS190
00166              'U003163040040703404070940'.                         ECS190
00167      12  FILLER              PIC  X(28)          VALUE            ECS190
00168              '5270706527070952607065260705'.                      ECS190
00169      12  FILLER              PIC  X(25)          VALUE            ECS190
00170              'A001163000040707004070800'.                         ECS190
00171      12  FILLER              PIC  X(28)          VALUE            ECS190
00172              '5270700527080052707005270800'.                      ECS190
00173      12  FILLER              PIC  X(25)          VALUE            ECS190
00174              'A002163000040707004070800'.                         ECS190
00175      12  FILLER              PIC  X(28)          VALUE            ECS190
00176              '5270700527080052707005270800'.                      ECS190
00177      12  FILLER              PIC  X(25)          VALUE            ECS190
00178              'A003163000040707004070800'.                         ECS190
00179      12  FILLER              PIC  X(28)          VALUE            ECS190
00180              '5270700527080052707005270800'.                      ECS190
00181      12  FILLER              PIC  X(25)          VALUE            ECS190
00182              'F001115589050408905045890'.                         ECS190
00183      12  FILLER              PIC  X(28)          VALUE            ECS190
00184              '5540890554589055408905545890'.                      ECS190
00185      12  FILLER              PIC  X(25)          VALUE            ECS190
00186              'F002115589050408905045890'.                         ECS190
00187      12  FILLER              PIC  X(28)          VALUE            ECS190
00188              '5540890554589055408905545890'.                      ECS190
00189      12  FILLER              PIC  X(25)          VALUE            ECS190
00190              'F003115589050408905045890'.                         ECS190
00191      12  FILLER              PIC  X(28)          VALUE            ECS190
00192              '5540890554589055408905545890'.                      ECS190
00193                                                                   ECS190
00194  01  LEDGER-NUMBER-TABLE  REDEFINES  GENERAL-LEDGER-TABLE.        ECS190
00195      12  LEDGER-NUMBERS  OCCURS  09  TIMES       INDEXED  BY  L.  ECS190
00196          16  LEDGER-CARR-CO          PIC  X(4).                   ECS190
00197          16  LEDGER-ACCT-NUMBERS     PIC  X(49).                  ECS190
00198                                                                   ECS190
00199  01  GENERAL-LEDGER-NUMBERS.                                      ECS190
00200      12  GL-ACCT-REC-NO      PIC  X(7).                           ECS190
00201      12  GL-LF-PRM-NO        PIC  X(7).                           ECS190
00202      12  GL-AH-PRM-NO        PIC  X(7).                           ECS190
00203      12  GL-LF-COM-NO        PIC  X(7).                           ECS190
00204      12  GL-AH-COM-NO        PIC  X(7).                           ECS190
00205      12  GL-LF-OV-NO         PIC  X(7).                           ECS190
00206      12  GL-AH-OV-NO         PIC  X(7).                           ECS190
00207  EJECT                                                            ECS190
00208  01  FIELD-REPRESENTATIVE    PIC  X(6).                           ECS190
00209      88  FIELD-REP     VALUE '005500' '005511' '005535' '005541'  ECS190
00210                              '005544' '005545' '005549' '005599'  ECS190
00211                              '005602' '005627' '005634' '005710'  ECS190
00212                              '005729' '005751' '005755' '005758'  ECS190
00213                              '005759' '005761' '005762' '005768'  ECS190
00214                              '005769' '005770' '005772' '005777'  ECS190
00215                              '005785' '005699' '0A0002' '0A0004'  ECS190
00216                              '0A0006' '0A0007' '0A0009' '0A0014'  ECS190
00217                              '005795' '005797' '005798' '005799'  ECS190
00218                              '777777'.                            ECS190
00219                                                                   ECS190
00220  01  ACCOUNT-TOTALS      COMP-3.                                  ECS190
00221      12  ACCT-ACC-REC        PIC S9(7)V99        VALUE ZEROS.     ECS190
00222      12  ACCT-ACC-REC-DB     PIC S9(7)V99        VALUE ZEROS.     ECS190
00223      12  ACCT-ACC-REC-CR     PIC S9(7)V99        VALUE ZEROS.     ECS190
00224      12  ACCT-LF-PRM         PIC S9(7)V99        VALUE ZEROS.     ECS190
00225      12  ACCT-LF-PRM-DB      PIC S9(7)V99        VALUE ZEROS.     ECS190
00226      12  ACCT-LF-PRM-CR      PIC S9(7)V99        VALUE ZEROS.     ECS190
00227      12  ACCT-AH-PRM         PIC S9(7)V99        VALUE ZEROS.     ECS190
00228      12  ACCT-AH-PRM-DB      PIC S9(7)V99        VALUE ZEROS.     ECS190
00229      12  ACCT-AH-PRM-CR      PIC S9(7)V99        VALUE ZEROS.     ECS190
00230      12  ACCT-LF-COM         PIC S9(7)V99        VALUE ZEROS.     ECS190
00231      12  ACCT-LF-COM-DB      PIC S9(7)V99        VALUE ZEROS.     ECS190
00232      12  ACCT-LF-COM-CR      PIC S9(7)V99        VALUE ZEROS.     ECS190
00233      12  ACCT-AH-COM         PIC S9(7)V99        VALUE ZEROS.     ECS190
00234      12  ACCT-AH-COM-DB      PIC S9(7)V99        VALUE ZEROS.     ECS190
00235      12  ACCT-AH-COM-CR      PIC S9(7)V99        VALUE ZEROS.     ECS190
00236      12  ACCT-LF-OV          PIC S9(7)V99        VALUE ZEROS.     ECS190
00237      12  ACCT-LF-OV-DB       PIC S9(7)V99        VALUE ZEROS.     ECS190
00238      12  ACCT-LF-OV-CR       PIC S9(7)V99        VALUE ZEROS.     ECS190
00239      12  ACCT-AH-OV          PIC S9(7)V99        VALUE ZEROS.     ECS190
00240      12  ACCT-AH-OV-DB       PIC S9(7)V99        VALUE ZEROS.     ECS190
00241      12  ACCT-AH-OV-CR       PIC S9(7)V99        VALUE ZEROS.     ECS190
00242                                                                   ECS190
00243  01  COMPANY-TOTALS      COMP-3.                                  ECS190
00244      12  COMP-ACC-REC        PIC S9(7)V99        VALUE ZEROS.     ECS190
00245      12  COMP-ACC-REC-DB     PIC S9(7)V99        VALUE ZEROS.     ECS190
00246      12  COMP-ACC-REC-CR     PIC S9(7)V99        VALUE ZEROS.     ECS190
00247      12  COMP-LF-PRM         PIC S9(7)V99        VALUE ZEROS.     ECS190
00248      12  COMP-LF-PRM-DB      PIC S9(7)V99        VALUE ZEROS.     ECS190
00249      12  COMP-LF-PRM-CR      PIC S9(7)V99        VALUE ZEROS.     ECS190
00250      12  COMP-AH-PRM         PIC S9(7)V99        VALUE ZEROS.     ECS190
00251      12  COMP-AH-PRM-DB      PIC S9(7)V99        VALUE ZEROS.     ECS190
00252      12  COMP-AH-PRM-CR      PIC S9(7)V99        VALUE ZEROS.     ECS190
00253      12  COMP-LF-COM         PIC S9(7)V99        VALUE ZEROS.     ECS190
00254      12  COMP-LF-COM-DB      PIC S9(7)V99        VALUE ZEROS.     ECS190
00255      12  COMP-LF-COM-CR      PIC S9(7)V99        VALUE ZEROS.     ECS190
00256      12  COMP-AH-COM         PIC S9(7)V99        VALUE ZEROS.     ECS190
00257      12  COMP-AH-COM-DB      PIC S9(7)V99        VALUE ZEROS.     ECS190
00258      12  COMP-AH-COM-CR      PIC S9(7)V99        VALUE ZEROS.     ECS190
00259      12  COMP-LF-OV          PIC S9(7)V99        VALUE ZEROS.     ECS190
00260      12  COMP-LF-OV-DB       PIC S9(7)V99        VALUE ZEROS.     ECS190
00261      12  COMP-LF-OV-CR       PIC S9(7)V99        VALUE ZEROS.     ECS190
00262      12  COMP-AH-OV          PIC S9(7)V99        VALUE ZEROS.     ECS190
00263      12  COMP-AH-OV-DB       PIC S9(7)V99        VALUE ZEROS.     ECS190
00264      12  COMP-AH-OV-CR       PIC S9(7)V99        VALUE ZEROS.     ECS190
00265  EJECT                                                            ECS190
00266  01  CARRIER-TOTALS      COMP-3.                                  ECS190
00267      12  CARR-ACC-REC        PIC S9(7)V99        VALUE ZEROS.     ECS190
00268      12  CARR-ACC-REC-DB     PIC S9(7)V99        VALUE ZEROS.     ECS190
00269      12  CARR-ACC-REC-CR     PIC S9(7)V99        VALUE ZEROS.     ECS190
00270      12  CARR-LF-PRM         PIC S9(7)V99        VALUE ZEROS.     ECS190
00271      12  CARR-LF-PRM-DB      PIC S9(7)V99        VALUE ZEROS.     ECS190
00272      12  CARR-LF-PRM-CR      PIC S9(7)V99        VALUE ZEROS.     ECS190
00273      12  CARR-AH-PRM         PIC S9(7)V99        VALUE ZEROS.     ECS190
00274      12  CARR-AH-PRM-DB      PIC S9(7)V99        VALUE ZEROS.     ECS190
00275      12  CARR-AH-PRM-CR      PIC S9(7)V99        VALUE ZEROS.     ECS190
00276      12  CARR-LF-COM         PIC S9(7)V99        VALUE ZEROS.     ECS190
00277      12  CARR-LF-COM-DB      PIC S9(7)V99        VALUE ZEROS.     ECS190
00278      12  CARR-LF-COM-CR      PIC S9(7)V99        VALUE ZEROS.     ECS190
00279      12  CARR-AH-COM         PIC S9(7)V99        VALUE ZEROS.     ECS190
00280      12  CARR-AH-COM-DB      PIC S9(7)V99        VALUE ZEROS.     ECS190
00281      12  CARR-AH-COM-CR      PIC S9(7)V99        VALUE ZEROS.     ECS190
00282      12  CARR-LF-OV          PIC S9(7)V99        VALUE ZEROS.     ECS190
00283      12  CARR-LF-OV-DB       PIC S9(7)V99        VALUE ZEROS.     ECS190
00284      12  CARR-LF-OV-CR       PIC S9(7)V99        VALUE ZEROS.     ECS190
00285      12  CARR-AH-OV          PIC S9(7)V99        VALUE ZEROS.     ECS190
00286      12  CARR-AH-OV-DB       PIC S9(7)V99        VALUE ZEROS.     ECS190
00287      12  CARR-AH-OV-CR       PIC S9(7)V99        VALUE ZEROS.     ECS190
00288                                                                   ECS190
00289  01  GRAND-TOTALS        COMP-3.                                  ECS190
00290      12  GRND-ACC-REC        PIC S9(7)V99        VALUE ZEROS.     ECS190
00291      12  GRND-ACC-REC-DB     PIC S9(7)V99        VALUE ZEROS.     ECS190
00292      12  GRND-ACC-REC-CR     PIC S9(7)V99        VALUE ZEROS.     ECS190
00293      12  GRND-LF-PRM         PIC S9(7)V99        VALUE ZEROS.     ECS190
00294      12  GRND-LF-PRM-DB      PIC S9(7)V99        VALUE ZEROS.     ECS190
00295      12  GRND-LF-PRM-CR      PIC S9(7)V99        VALUE ZEROS.     ECS190
00296      12  GRND-AH-PRM         PIC S9(7)V99        VALUE ZEROS.     ECS190
00297      12  GRND-AH-PRM-DB      PIC S9(7)V99        VALUE ZEROS.     ECS190
00298      12  GRND-AH-PRM-CR      PIC S9(7)V99        VALUE ZEROS.     ECS190
00299      12  GRND-LF-COM         PIC S9(7)V99        VALUE ZEROS.     ECS190
00300      12  GRND-LF-COM-DB      PIC S9(7)V99        VALUE ZEROS.     ECS190
00301      12  GRND-LF-COM-CR      PIC S9(7)V99        VALUE ZEROS.     ECS190
00302      12  GRND-AH-COM         PIC S9(7)V99        VALUE ZEROS.     ECS190
00303      12  GRND-AH-COM-DB      PIC S9(7)V99        VALUE ZEROS.     ECS190
00304      12  GRND-AH-COM-CR      PIC S9(7)V99        VALUE ZEROS.     ECS190
00305      12  GRND-LF-OV          PIC S9(7)V99        VALUE ZEROS.     ECS190
00306      12  GRND-LF-OV-DB       PIC S9(7)V99        VALUE ZEROS.     ECS190
00307      12  GRND-LF-OV-CR       PIC S9(7)V99        VALUE ZEROS.     ECS190
00308      12  GRND-AH-OV          PIC S9(7)V99        VALUE ZEROS.     ECS190
00309      12  GRND-AH-OV-DB       PIC S9(7)V99        VALUE ZEROS.     ECS190
00310      12  GRND-AH-OV-CR       PIC S9(7)V99        VALUE ZEROS.     ECS190
00311  EJECT                                                            ECS190
00312  01  COMP-ZEROS          COMP-3.                                  ECS190
00313      12  FILLER              PIC S9(7)V99        VALUE ZEROS.     ECS190
00314      12  FILLER              PIC S9(7)V99        VALUE ZEROS.     ECS190
00315      12  FILLER              PIC S9(7)V99        VALUE ZEROS.     ECS190
00316      12  FILLER              PIC S9(7)V99        VALUE ZEROS.     ECS190
00317      12  FILLER              PIC S9(7)V99        VALUE ZEROS.     ECS190
00318      12  FILLER              PIC S9(7)V99        VALUE ZEROS.     ECS190
00319      12  FILLER              PIC S9(7)V99        VALUE ZEROS.     ECS190
00320      12  FILLER              PIC S9(7)V99        VALUE ZEROS.     ECS190
00321      12  FILLER              PIC S9(7)V99        VALUE ZEROS.     ECS190
00322      12  FILLER              PIC S9(7)V99        VALUE ZEROS.     ECS190
00323      12  FILLER              PIC S9(7)V99        VALUE ZEROS.     ECS190
00324      12  FILLER              PIC S9(7)V99        VALUE ZEROS.     ECS190
00325      12  FILLER              PIC S9(7)V99        VALUE ZEROS.     ECS190
00326      12  FILLER              PIC S9(7)V99        VALUE ZEROS.     ECS190
00327      12  FILLER              PIC S9(7)V99        VALUE ZEROS.     ECS190
00328      12  FILLER              PIC S9(7)V99        VALUE ZEROS.     ECS190
00329      12  FILLER              PIC S9(7)V99        VALUE ZEROS.     ECS190
00330      12  FILLER              PIC S9(7)V99        VALUE ZEROS.     ECS190
00331      12  FILLER              PIC S9(7)V99        VALUE ZEROS.     ECS190
00332      12  FILLER              PIC S9(7)V99        VALUE ZEROS.     ECS190
00333      12  FILLER              PIC S9(7)V99        VALUE ZEROS.     ECS190
00334                                                                   ECS190
00335  01  SEARCH-CARR-CO.                                              ECS190
00336      12  SEARCH-CARR         PIC  X.                              ECS190
00337      12  SEARCH-COMP         PIC  X(6).                           ECS190
00338  EJECT                                                            ECS190
00339                              COPY ECSCOM01.                       ECS190
00340  EJECT                                                            ECS190
00341  01  HEAD-LINE-1.                                                 ECS190
00342      12  FILLER              PIC  X(46)          VALUE SPACES.    ECS190
00343      12  FILLER              PIC  X(33)          VALUE            ECS190
00344              'GENERAL LEDGER INTERFACE (INCOME)'.                 ECS190
00345      12  FILLER              PIC  X(45)          VALUE SPACES.    ECS190
00346      12  FILLER              PIC  X(7)           VALUE 'ECS-190'. ECS190
00347      12  FILLER              PIC  X              VALUE SPACE.     ECS190
00348                                                                   ECS190
00349  01  HEAD-LINE-2.                                                 ECS190
00350      12  FILLER              PIC  X(47)          VALUE SPACES.    ECS190
00351      12  HD-CLIENT           PIC  X(30).                          ECS190
00352      12  FILLER              PIC  X(47)          VALUE SPACES.    ECS190
00353      12  HD-RUN              PIC  X(8).                           ECS190
00354                                                                   ECS190
00355  01  HEAD-LINE-3.                                                 ECS190
00356      12  FILLER              PIC  X(53)          VALUE SPACES.    ECS190
00357      12  HD-DATE             PIC  X(18).                          ECS190
00358      12  FILLER              PIC  X(41)          VALUE SPACES.    ECS190
00359      12  FILLER              PIC  X(5)           VALUE 'PAGE'.    ECS190
00360      12  HD-PAGE             PIC ZZ,ZZZ.                          ECS190
00361      12  FILLER              PIC  X(9)           VALUE SPACES.    ECS190
00362                                                                   ECS190
00363  01  HEAD-LINE-4.                                                 ECS190
00364      12  FILLER              PIC  X(44)          VALUE            ECS190
00365              '              AGENT OR        ACCOUNTS'.            ECS190
00366      12  FILLER              PIC  X(44)          VALUE            ECS190
00367              '    LIFE          HEALTH         LIFE'.             ECS190
00368      12  FILLER              PIC  X(44)          VALUE            ECS190
00369              '   HEALTH          LIFE          HEALTH'.           ECS190
00370                                                                   ECS190
00371  01  HEAD-LINE-5.                                                 ECS190
00372      12  FILLER              PIC  X(44)          VALUE            ECS190
00373              'CAR GROUP ST   ACCOUNT       RECEIVABLE'.           ECS190
00374      12  FILLER              PIC  X(44)          VALUE            ECS190
00375              '  PREMIUM        PREMIUM      COMMISSION'.          ECS190
00376      12  FILLER              PIC  X(44)          VALUE            ECS190
00377              ' COMMISSION      O/W COMM       O/W COMM'.          ECS190
00378                                                                   ECS190
00379  01  DETAIL-LINE-1.                                               ECS190
00380      12  DET-DESC.                                                ECS190
00381          16  FILLER          PIC  X              VALUE SPACES.    ECS190
00382          16  DET-CARR        PIC  X.                              ECS190
00383          16  FILLER          PIC  X              VALUE SPACES.    ECS190
00384          16  DET-COMP        PIC  X(6).                           ECS190
00385          16  FILLER          PIC  X              VALUE SPACES.    ECS190
00386          16  DET-ST          PIC  XX.                             ECS190
00387          16  FILLER          PIC  X              VALUE SPACES.    ECS190
00388          16  DET-ACCT        PIC  X(10).                          ECS190
00389          16  FILLER          PIC  X              VALUE SPACES.    ECS190
00390      12  DET-FIELD-REP       PIC  XX             VALUE SPACES.    ECS190
00391      12  FILLER              PIC  X              VALUE SPACES.    ECS190
00392      12  DET-ACC-REC         PIC ZZ,ZZZ,ZZZ.99-.                  ECS190
00393      12  FILLER              PIC  X              VALUE SPACES.    ECS190
00394      12  DET-LF-PREM         PIC ZZ,ZZZ,ZZZ.99-.                  ECS190
00395      12  FILLER              PIC  X              VALUE SPACES.    ECS190
00396      12  DET-AH-PREM         PIC ZZ,ZZZ,ZZZ.99-.                  ECS190
00397      12  FILLER              PIC  X              VALUE SPACES.    ECS190
00398      12  DET-LF-COMM         PIC ZZ,ZZZ,ZZZ.99-.                  ECS190
00399      12  FILLER              PIC  X              VALUE SPACES.    ECS190
00400      12  DET-AH-COMM         PIC ZZ,ZZZ,ZZZ.99-.                  ECS190
00401      12  FILLER              PIC  X              VALUE SPACES.    ECS190
00402      12  DET-LF-OV           PIC ZZ,ZZZ,ZZZ.99-.                  ECS190
00403      12  FILLER              PIC  X              VALUE SPACES.    ECS190
00404      12  DET-AH-OV           PIC ZZ,ZZZ,ZZZ.99-.                  ECS190
00405      12  FILLER              PIC  X.                              ECS190
00406                                                                   ECS190
00407  01  DETAIL-GL-LINE.                                              ECS190
00408      12  FILLER              PIC  X(10)          VALUE SPACES.    ECS190
00409      12  FILLER              PIC  X(16)          VALUE            ECS190
00410              '**********    **'.                                  ECS190
00411      12  DET-AR-GLNO         PIC  X(7).                           ECS190
00412      12  FILLER              PIC  X(9)           VALUE            ECS190
00413              '**     **'.                                         ECS190
00414      12  DET-LP-GLNO         PIC  X(7).                           ECS190
00415      12  FILLER              PIC  X(8)           VALUE            ECS190
00416              '**    **'.                                          ECS190
00417      12  DET-AP-GLNO         PIC  X(7).                           ECS190
00418      12  FILLER              PIC  X(9)           VALUE            ECS190
00419              '**     **'.                                         ECS190
00420      12  DET-LC-GLNO         PIC  X(7).                           ECS190
00421      12  FILLER              PIC  X(8)           VALUE            ECS190
00422              '**    **'.                                          ECS190
00423      12  DET-AC-GLNO         PIC  X(7).                           ECS190
00424      12  FILLER              PIC  X(9)           VALUE            ECS190
00425              '**     **'.                                         ECS190
00426      12  DET-LO-GLNO         PIC  X(7).                           ECS190
00427      12  FILLER              PIC  X(8)           VALUE            ECS190
00428              '**    **'.                                          ECS190
00429      12  DET-AO-GLNO         PIC  X(7).                           ECS190
00430      12  FILLER              PIC  X(6)           VALUE '**    '.  ECS190
00431  EJECT                                                            ECS190
00432  01  STATE-TABLE.                                                 ECS190
00433      12  FILLER              PIC  X(4)       VALUE 'AL01'.        ECS190
00434      12  FILLER              PIC  X(4)       VALUE 'AZ02'.        ECS190
00435      12  FILLER              PIC  X(4)       VALUE 'AR03'.        ECS190
00436      12  FILLER              PIC  X(4)       VALUE 'CA04'.        ECS190
00437      12  FILLER              PIC  X(4)       VALUE 'CO05'.        ECS190
00438      12  FILLER              PIC  X(4)       VALUE 'CT06'.        ECS190
00439      12  FILLER              PIC  X(4)       VALUE 'DE07'.        ECS190
00440      12  FILLER              PIC  X(4)       VALUE 'DC08'.        ECS190
00441      12  FILLER              PIC  X(4)       VALUE 'FL09'.        ECS190
00442      12  FILLER              PIC  X(4)       VALUE 'GA10'.        ECS190
00443      12  FILLER              PIC  X(4)       VALUE 'ID11'.        ECS190
00444      12  FILLER              PIC  X(4)       VALUE 'IL12'.        ECS190
00445      12  FILLER              PIC  X(4)       VALUE 'IN13'.        ECS190
00446      12  FILLER              PIC  X(4)       VALUE 'IA14'.        ECS190
00447      12  FILLER              PIC  X(4)       VALUE 'KS15'.        ECS190
00448      12  FILLER              PIC  X(4)       VALUE 'KY16'.        ECS190
00449      12  FILLER              PIC  X(4)       VALUE 'LA17'.        ECS190
00450      12  FILLER              PIC  X(4)       VALUE 'ME18'.        ECS190
00451      12  FILLER              PIC  X(4)       VALUE 'MD19'.        ECS190
00452      12  FILLER              PIC  X(4)       VALUE 'MA20'.        ECS190
00453      12  FILLER              PIC  X(4)       VALUE 'MI21'.        ECS190
00454      12  FILLER              PIC  X(4)       VALUE 'MN22'.        ECS190
00455      12  FILLER              PIC  X(4)       VALUE 'MS23'.        ECS190
00456      12  FILLER              PIC  X(4)       VALUE 'MO24'.        ECS190
00457      12  FILLER              PIC  X(4)       VALUE 'MT25'.        ECS190
00458      12  FILLER              PIC  X(4)       VALUE 'NE26'.        ECS190
00459      12  FILLER              PIC  X(4)       VALUE 'NV27'.        ECS190
00460      12  FILLER              PIC  X(4)       VALUE 'NH28'.        ECS190
00461      12  FILLER              PIC  X(4)       VALUE 'NJ29'.        ECS190
00462      12  FILLER              PIC  X(4)       VALUE 'NM30'.        ECS190
00463      12  FILLER              PIC  X(4)       VALUE 'NY31'.        ECS190
00464      12  FILLER              PIC  X(4)       VALUE 'NC32'.        ECS190
00465      12  FILLER              PIC  X(4)       VALUE 'ND33'.        ECS190
00466      12  FILLER              PIC  X(4)       VALUE 'OH34'.        ECS190
00467      12  FILLER              PIC  X(4)       VALUE 'OK35'.        ECS190
00468      12  FILLER              PIC  X(4)       VALUE 'OR36'.        ECS190
00469      12  FILLER              PIC  X(4)       VALUE 'PA37'.        ECS190
00470      12  FILLER              PIC  X(4)       VALUE 'RI38'.        ECS190
00471      12  FILLER              PIC  X(4)       VALUE 'SC39'.        ECS190
00472      12  FILLER              PIC  X(4)       VALUE 'SD40'.        ECS190
00473      12  FILLER              PIC  X(4)       VALUE 'TN41'.        ECS190
00474      12  FILLER              PIC  X(4)       VALUE 'TX42'.        ECS190
00475      12  FILLER              PIC  X(4)       VALUE 'UT43'.        ECS190
00476      12  FILLER              PIC  X(4)       VALUE 'VT44'.        ECS190
00477      12  FILLER              PIC  X(4)       VALUE 'VA45'.        ECS190
00478      12  FILLER              PIC  X(4)       VALUE 'WA46'.        ECS190
00479      12  FILLER              PIC  X(4)       VALUE 'WV47'.        ECS190
00480      12  FILLER              PIC  X(4)       VALUE 'WI48'.        ECS190
00481      12  FILLER              PIC  X(4)       VALUE 'WY49'.        ECS190
00482      12  FILLER              PIC  X(4)       VALUE 'AK50'.        ECS190
00483      12  FILLER              PIC  X(4)       VALUE 'HI55'.        ECS190
00484      12  FILLER              PIC  X(4)       VALUE HIGH-VALUES.   ECS190
00485                                                                   ECS190
00486  01  STATE-TBL  REDEFINES  STATE-TABLE.                           ECS190
00487      12  STATE-ENTRY     OCCURS  52  TIMES.                       ECS190
00488          16  STATE-CDE       PIC  XX.                             ECS190
00489          16  STATE-ARG       PIC  XX.                             ECS190
00490  EJECT                                                            ECS190
00491                              COPY ELCDATE.                        ECS190
00492                                                                   ECS190
00493                              COPY ELCDTECX.                       ECS190
00494                                                                   ECS190
00495                              COPY ELCDTEVR.                       ECS190
00496  EJECT                                                            ECS190
00497  PROCEDURE DIVISION.                                              ECS190
00498                                                                   ECS190
00499  INITIALIZATION SECTION.                                          ECS190
00500                                                                   ECS190
00501  0000-GET-DATE.                                                   ECS190
00502                              COPY ELCDTERX.                       ECS190
00503                                                                   ECS190
00504      MOVE ALPH-DATE              TO  HD-DATE.                     ECS190
00505      MOVE WS-CURRENT-DATE        TO  HD-RUN.                      ECS190
00506      MOVE COMPANY-NAME           TO  HD-CLIENT.                   ECS190
00507      MOVE RUN-DATE               TO  3800-DATE-1                  ECS190
00508                                      3800-DATE-1-R                ECS190
00509                                      3800-DATE-2.                 ECS190
00510      MOVE BIN-RUN-DATE           TO  DC-BIN-DATE-1.               ECS190
00511      MOVE ' '                    TO  DC-OPTION-CODE.              ECS190
00512                                                                   ECS190
00513      CALL 'ELDATCX'  USING  DATE-CONVERSION-DATA.                 ECS190
00514                                                                   ECS190
00515      MOVE DC-JULIAN-DATE         TO  WS-3800-JULIAN-DATE.            CL**8
00516      MOVE DC-ALPHA-CEN-N         TO  3800-JULIAN-CC.              ECS190
00517      MOVE WS-3800-JULIAN-DATE    TO  3800-JULIAN-DATE.               CL**8
00518      MOVE DTE-CLIENT             TO  3800-COMPANY.                ECS190
00519                                                                   ECS190
00520      IF DTE-CLIENT = 'CIM'                                        ECS190
00521          MOVE 'WCO'              TO  3800-COMPANY.                ECS190
00522                                                                   ECS190
00523      IF DTE-CLIENT = 'WFL'                                        ECS190
00524          MOVE 'FLA'              TO  3800-COMPANY.                ECS190
00525                                                                   ECS190
00526      IF DTE-CLIENT = 'UFL' OR  'UFR'                              ECS190
00527          MOVE 'UFL'              TO  3800-COMPANY.                ECS190
00528                                                                   ECS190
00529  0100-SORT-ROUTINE.                                               ECS190
00530      SORT SORT-FILE  ON ASCENDING KEY  SORT-KEY1                  ECS190
00531                                        SORT-KEY3                  ECS190
00532          INPUT PROCEDURE  SORT-PREMIUMS-COMMISSIONS               ECS190
00533          OUTPUT PROCEDURE CREATE-GEN-LEDGER-TRANSACTIONS.         ECS190
00534                                                                   ECS190
00535      IF SORT-RETURN NOT = ZEROS                                   ECS190
00536          MOVE '0101'             TO  WS-RETURN-CODE               ECS190
00537          MOVE 'INTERNAL SORT 01 ABORTED'                          ECS190
00538                                  TO  WS-ABEND-MESSAGE             ECS190
00539          GO TO ABEND-PGM.                                         ECS190
00540                                                                   ECS190
00541  0999-END-OF-JOB.                                                 ECS190
00542                              COPY ELCPRTC.                        ECS190
00543                                                                   ECS190
00544      CLOSE ABC-INCOME  PRINT-FILE.                                ECS190
00545                                                                   ECS190
00546      MOVE ZEROS  TO RETURN-CODE.
00546      GOBACK.                                                      ECS190
00547  EJECT                                                            ECS190
00548  SORT-PREMIUMS-COMMISSIONS SECTION.                               ECS190
00549                                                                   ECS190
00550  1000-INPUT-ROUTINE.                                              ECS190
00551      OPEN INPUT COMM-TRANS-IN.                                    ECS190
00552                                                                   ECS190
00553  1100-READ-CPTR.                                                  ECS190
00554      READ COMM-TRANS-IN  INTO  CP-RECORD  AT END                  ECS190
00555          GO TO 1900-END-INPUT.                                    ECS190
00556                                                                   ECS190
00557      IF CP-ISSUE     OR                                           ECS190
00558         CP-CANCEL    OR                                           ECS190
00559         CP-OVERWT    OR                                           ECS190
00560         CP-RC-ISSUE  OR                                           ECS190
00561         CP-RC-CANCEL OR                                           ECS190
00562         CP-RC-OVERWT                                              ECS190
00563          NEXT SENTENCE                                            ECS190
00564      ELSE                                                         ECS190
00565          GO TO 1100-READ-CPTR.                                    ECS190
00566                                                                   ECS190
00567      IF (CP-RC-ISSUE  OR  CP-RC-CANCEL  OR  CP-RC-OVERWT)         ECS190
00568          IF DTE-CLIENT = 'ADL' OR  'FLB'  OR  'ALA'               ECS190
00569              NEXT SENTENCE                                        ECS190
00570          ELSE                                                     ECS190
00571              GO TO 1100-READ-CPTR.                                ECS190
00572                                                                   ECS190
00573      IF DTE-CLIENT = 'UFR'                                        ECS190
00574          IF CP-OVERWT                                             ECS190
00575              GO TO 1100-READ-CPTR.                                ECS190
00576                                                                   ECS190
00577      IF DTE-CLIENT = 'ADL' OR  'LOK'  OR  'WFL'                   ECS190
00578                            OR  'FLB'  OR  'ALA'                   ECS190
00579          MOVE CP-STATE           TO  HOLD-STATE                   ECS190
00580          GO TO 1300-SET-CONTROLS.                                 ECS190
00581                                                                   ECS190
00582      IF CP-STATE = PREV-STATE                                     ECS190
00583          GO TO 1300-SET-CONTROLS.                                 ECS190
00584                                                                   ECS190
00585      MOVE +0                     TO  S1.                          ECS190
00586      MOVE CP-STATE               TO  PREV-STATE.                  ECS190
00587                                                                   ECS190
00588  1200-LOOP.                                                       ECS190
00589      ADD +1                      TO  S1.                          ECS190
00590                                                                   ECS190
00591      IF STATE-ARG (S1) = HIGH-VALUES                              ECS190
00592          MOVE 'XX'               TO  HOLD-STATE                   ECS190
00593          GO TO 1300-SET-CONTROLS.                                 ECS190
00594                                                                   ECS190
00595      IF CP-STATE = STATE-ARG (S1)                                 ECS190
00596          MOVE STATE-CDE (S1)     TO  HOLD-STATE                   ECS190
00597          GO TO 1300-SET-CONTROLS.                                 ECS190
00598                                                                   ECS190
00599      GO TO 1200-LOOP.                                             ECS190
00600                                                                   ECS190
00601  1300-SET-CONTROLS.                                               ECS190
00602      IF (CP-OVERWT  OR  CP-RC-OVERWT)                             ECS190
00603          MOVE CP-REMIT           TO  CP-ACCOUNT.                  ECS190
00604                                                                   ECS190
00605      MOVE HOLD-STATE             TO  CP-STATE.                    ECS190
00606                                                                   ECS190
00607      IF DTE-CLIENT = 'ADL' OR  'LOK'  OR  'FLB' OR 'ALA'          ECS190
00608          GO TO 1400-RELEASE-CPTR.                                 ECS190
00609                                                                   ECS190
00610      IF DTE-CLIENT = 'CIM'                                        ECS190
00611          MOVE 'W'                TO  CP-CARRIER.                  ECS190
00612                                                                   ECS190
00613      IF DTE-CLIENT = 'WFL'                                        ECS190
00614          MOVE 'F'                TO  CP-CARRIER.                  ECS190
00615                                                                   ECS190
00616      IF DTE-CLIENT = 'UFL'                                        ECS190
00617          MOVE 'U'                TO  CP-CARRIER.                  ECS190
00618                                                                   ECS190
00619      IF DTE-CLIENT = 'UFR'                                        ECS190
00620          MOVE 'R'                TO  CP-CARRIER.                  ECS190
00621                                                                   ECS190
00622      MOVE '000001'               TO  CP-GROUPING.                 ECS190
00623                                                                   ECS190
00624      IF CP-GPCD = '02' OR '05'                                    ECS190
00625          MOVE '000002'           TO  CP-GROUPING                  ECS190
00626      ELSE                                                         ECS190
00627          IF CP-GPCD = '04'                                        ECS190
00628              MOVE '000003'       TO  CP-GROUPING.                 ECS190
00629                                                                   ECS190
00630      IF DTE-CLIENT = 'CIM'                                        ECS190
00631          MOVE '000002'           TO  CP-GROUPING                  ECS190
00632          IF CP-GPCD = '01' OR '09' OR '15' OR '18'                ECS190
00633              MOVE '000001'       TO  CP-GROUPING.                 ECS190
00634                                                                   ECS190
00635  1400-RELEASE-CPTR.                                               ECS190
00636      RELEASE SORT-REC  FROM  CP-RECORD.                           ECS190
00637                                                                   ECS190
00638      GO TO 1100-READ-CPTR.                                        ECS190
00639                                                                   ECS190
00640  1900-END-INPUT.                                                  ECS190
00641      CLOSE COMM-TRANS-IN.                                         ECS190
00642                                                                   ECS190
00643  1999-XIT.                                                        ECS190
00644      EXIT.                                                        ECS190
00645  EJECT                                                            ECS190
00646  CREATE-GEN-LEDGER-TRANSACTIONS SECTION.                          ECS190
00647                                                                   ECS190
00648  2000-OUTPUT-ROUTINE.                                             ECS190
00649      OPEN OUTPUT ABC-INCOME  PRINT-FILE.                          ECS190
00650                                                                   ECS190
00651      PERFORM 2100-RETURN-SORTED-RECS.                             ECS190
00652                                                                   ECS190
00653      MOVE CP-CARRIER             TO  PREV-CARR.                   ECS190
00654      MOVE CP-GROUPING            TO  PREV-COMP.                   ECS190
00655      MOVE CP-STATE               TO  PREV-STATE.                  ECS190
00656      MOVE CP-ACCOUNT             TO  PREV-ACCT.                   ECS190
00657                                                                   ECS190
00658      PERFORM 8000-HEADING-ROUTINE  THRU  8099-XIT.                ECS190
00659                                                                   ECS190
00660      GO TO 2300-ACCUMULATE-TOTALS.                                ECS190
00661                                                                   ECS190
00662  2100-RETURN-SORTED-RECS.                                         ECS190
00663      RETURN SORT-FILE  INTO  CP-RECORD  AT END                    ECS190
00664          GO TO 3000-ACCT-BREAK.                                   ECS190
00665                                                                   ECS190
00666  2200-CHECK-BREAKS.                                               ECS190
00667      IF CP-CARRIER NOT = PREV-CARR                                ECS190
00668          PERFORM 3000-ACCT-BREAK  THRU  3099-XIT                  ECS190
00669          PERFORM 3100-COMP-BREAK  THRU  3199-XIT                  ECS190
00670          PERFORM 3200-CARR-BREAK  THRU  3299-XIT                  ECS190
00671          PERFORM 8000-HEADING-ROUTINE  THRU  8099-XIT.            ECS190
00672                                                                   ECS190
00673      IF CP-GROUPING NOT = PREV-COMP                               ECS190
00674          PERFORM 3000-ACCT-BREAK  THRU  3099-XIT                  ECS190
00675          PERFORM 3100-COMP-BREAK  THRU  3199-XIT                  ECS190
00676          PERFORM 8000-HEADING-ROUTINE  THRU  8099-XIT.            ECS190
00677                                                                   ECS190
00678      IF CP-ACCOUNT NOT = PREV-ACCT                                ECS190
00679          PERFORM 3000-ACCT-BREAK  THRU  3099-XIT.                 ECS190
00680                                                                   ECS190
00681  2300-ACCUMULATE-TOTALS.                                          ECS190
00682      IF CP-ISSUE    OR                                            ECS190
00683         CP-CANCEL   OR                                            ECS190
00684         CP-RC-ISSUE OR                                            ECS190
00685         CP-RC-CANCEL                                              ECS190
00686          COMPUTE ACCT-LF-PRM = ACCT-LF-PRM + CP-LF-PRM            ECS190
00687                              + CP-LF-PRM-ALT                      ECS190
00688          COMPUTE ACCT-AH-PRM = ACCT-AH-PRM + CP-AH-PRM            ECS190
00689          COMPUTE ACCT-LF-COM = ACCT-LF-COM + CP-LF-COM            ECS190
00690                              + CP-LF-COM-ALT                      ECS190
00691          COMPUTE ACCT-AH-COM = ACCT-AH-COM + CP-AH-COM            ECS190
00692      ELSE                                                         ECS190
00693 *        MOVE CP-RECORD TO CP-OW-RECORD                           ECS190
00694          COMPUTE ACCT-LF-OV = ACCT-LF-OV + CP-OW-LF-COM           ECS190
00695                             + CP-OW-LF-COM-ALT                    ECS190
00696          COMPUTE ACCT-AH-OV = ACCT-AH-OV + CP-OW-AH-COM.          ECS190
00697                                                                   ECS190
00698        GO TO 2100-RETURN-SORTED-RECS.                             ECS190
00699                                                                   ECS190
00700  2500-WRITE-ABC-REC.                                              ECS190
00701      WRITE ABC-INCOME-REC  FROM  WS-3800-CARD.                    ECS190
00702                                                                   ECS190
00703  2599-XIT.                                                        ECS190
00704      EXIT.                                                        ECS190
00705  EJECT                                                            ECS190
00706  3000-ACCT-BREAK.                                                 ECS190
00707      MOVE SPACES                 TO  DETAIL-LINE-1.               ECS190
00708      MOVE PREV-CARR              TO  DET-CARR  SEARCH-CARR.       ECS190
00709      MOVE PREV-COMP              TO  DET-COMP  SEARCH-COMP.       ECS190
00710                                                                   ECS190
00711      IF DTE-CLIENT = 'ADL' OR  'FLB'  OR  'ALA'                   ECS190
00712          MOVE 'A'                TO  SEARCH-CARR.                 ECS190
00713                                                                   ECS190
00714      PERFORM 8200-LEDGER-TABLE-LOOKUP  THRU  8299-XIT.            ECS190
00715                                                                   ECS190
00716      MOVE PREV-ACCT              TO  3800-ACCOUNT                 ECS190
00717                                      DET-ACCT                     ECS190
00718                                      FIELD-REPRESENTATIVE.        ECS190
00719                                                                   ECS190
00720      IF DTE-CLIENT = 'UFL'  OR  'UFR'  OR  'WFL'                  ECS190
00721          IF FIELD-REP                                             ECS190
00722              MOVE '**'           TO  DET-FIELD-REP.               ECS190
00723                                                                   ECS190
00724      MOVE PREV-STATE             TO  3800-STATE  DET-ST.          ECS190
00725                                                                   ECS190
00726      COMPUTE ACCT-ACC-REC = ACCT-LF-PRM + ACCT-AH-PRM             ECS190
00727                           - ACCT-LF-COM - ACCT-AH-COM             ECS190
00728                           - ACCT-LF-OV  - ACCT-AH-OV.             ECS190
00729                                                                   ECS190
00730      MOVE ACCT-ACC-REC           TO  DET-ACC-REC.                 ECS190
00731      MOVE ACCT-LF-PRM            TO  DET-LF-PREM.                 ECS190
00732      MOVE ACCT-AH-PRM            TO  DET-AH-PREM.                 ECS190
00733      MOVE ACCT-LF-COM            TO  DET-LF-COMM.                 ECS190
00734      MOVE ACCT-AH-COM            TO  DET-AH-COMM.                 ECS190
00735      MOVE ACCT-LF-OV             TO  DET-LF-OV.                   ECS190
00736      MOVE ACCT-AH-OV             TO  DET-AH-OV.                   ECS190
00737      MOVE SPACE-1                TO  X.                           ECS190
00738      MOVE DETAIL-LINE-1          TO  P-DATA.                      ECS190
00739                                                                   ECS190
00740      PERFORM 8100-PRINT-ROUTINE  THRU  8199-XIT.                  ECS190
00741                                                                   ECS190
00742  3050-GEN-LEDGER-ENTRIES.                                         ECS190
00743      MOVE GL-ACCT-REC-NO         TO  3800-GENERAL-LEDGER.         ECS190
00744                                                                   ECS190
00745      IF DTE-CLIENT = 'UFL' OR  'UFR'  OR  'WFL'                   ECS190
00746          IF FIELD-REP                                             ECS190
00747              MOVE '1420250'      TO  3800-GENERAL-LEDGER.         ECS190
00748                                                                   ECS190
00749      MOVE ACCT-ACC-REC           TO  3800-AMOUNT.                 ECS190
00750                                                                   ECS190
00751      IF ACCT-ACC-REC  NOT LESS ZERO                               ECS190
00752          MOVE 'D'                TO  3800-DEBIT-CREDIT            ECS190
00753          ADD 3800-AMOUNT         TO  ACCT-ACC-REC-DB              ECS190
00754      ELSE                                                         ECS190
00755          MOVE 'C'                TO  3800-DEBIT-CREDIT            ECS190
00756          ADD 3800-AMOUNT         TO  ACCT-ACC-REC-CR.             ECS190
00757                                                                   ECS190
00758      IF ACCT-ACC-REC NOT = ZERO                                   ECS190
00759          PERFORM 2500-WRITE-ABC-REC  THRU  2599-XIT.              ECS190
00760                                                                   ECS190
00761      MOVE GL-LF-PRM-NO           TO  3800-GENERAL-LEDGER.         ECS190
00762      MOVE ACCT-LF-PRM            TO  3800-AMOUNT.                 ECS190
00763                                                                   ECS190
00764      IF ACCT-LF-PRM NOT LESS ZERO                                 ECS190
00765          MOVE 'C'                TO  3800-DEBIT-CREDIT            ECS190
00766          ADD 3800-AMOUNT         TO  ACCT-LF-PRM-CR               ECS190
00767      ELSE                                                         ECS190
00768          MOVE 'D'                TO  3800-DEBIT-CREDIT            ECS190
00769          ADD 3800-AMOUNT         TO  ACCT-LF-PRM-DB.              ECS190
00770                                                                   ECS190
00771      IF ACCT-LF-PRM NOT = ZERO                                    ECS190
00772          PERFORM 2500-WRITE-ABC-REC  THRU  2599-XIT.              ECS190
00773                                                                   ECS190
00774      MOVE GL-AH-PRM-NO           TO  3800-GENERAL-LEDGER.         ECS190
00775      MOVE ACCT-AH-PRM            TO  3800-AMOUNT.                 ECS190
00776                                                                   ECS190
00777      IF ACCT-AH-PRM NOT LESS ZERO                                 ECS190
00778          MOVE 'C'                TO  3800-DEBIT-CREDIT            ECS190
00779          ADD 3800-AMOUNT         TO  ACCT-AH-PRM-CR               ECS190
00780      ELSE                                                         ECS190
00781          MOVE 'D'                TO  3800-DEBIT-CREDIT            ECS190
00782          ADD 3800-AMOUNT         TO  ACCT-AH-PRM-DB.              ECS190
00783                                                                   ECS190
00784      IF ACCT-AH-PRM NOT = ZERO                                    ECS190
00785          PERFORM 2500-WRITE-ABC-REC  THRU  2599-XIT.              ECS190
00786                                                                   ECS190
00787      MOVE GL-LF-COM-NO           TO  3800-GENERAL-LEDGER.         ECS190
00788      MOVE ACCT-LF-COM            TO  3800-AMOUNT.                 ECS190
00789                                                                   ECS190
00790      IF ACCT-LF-COM NOT LESS ZERO                                 ECS190
00791          MOVE 'D'                TO  3800-DEBIT-CREDIT            ECS190
00792          ADD 3800-AMOUNT         TO  ACCT-LF-COM-DB               ECS190
00793      ELSE                                                         ECS190
00794          MOVE 'C'                TO  3800-DEBIT-CREDIT            ECS190
00795          ADD 3800-AMOUNT         TO  ACCT-LF-COM-CR.              ECS190
00796                                                                   ECS190
00797      IF ACCT-LF-COM NOT = ZERO                                    ECS190
00798          PERFORM 2500-WRITE-ABC-REC  THRU  2599-XIT.              ECS190
00799                                                                   ECS190
00800      MOVE GL-AH-COM-NO           TO  3800-GENERAL-LEDGER.         ECS190
00801      MOVE ACCT-AH-COM            TO  3800-AMOUNT.                 ECS190
00802                                                                   ECS190
00803      IF ACCT-AH-COM NOT LESS ZERO                                 ECS190
00804          MOVE 'D'                TO  3800-DEBIT-CREDIT            ECS190
00805          ADD 3800-AMOUNT         TO  ACCT-AH-COM-DB               ECS190
00806      ELSE                                                         ECS190
00807          MOVE 'C'                TO  3800-DEBIT-CREDIT            ECS190
00808          ADD 3800-AMOUNT         TO  ACCT-AH-COM-CR.              ECS190
00809                                                                   ECS190
00810      IF ACCT-AH-COM NOT = ZERO                                    ECS190
00811          PERFORM 2500-WRITE-ABC-REC  THRU  2599-XIT.              ECS190
00812                                                                   ECS190
00813      MOVE GL-LF-OV-NO            TO  3800-GENERAL-LEDGER.         ECS190
00814      MOVE ACCT-LF-OV             TO  3800-AMOUNT.                 ECS190
00815                                                                   ECS190
00816      IF ACCT-LF-OV NOT LESS ZERO                                  ECS190
00817          MOVE 'D'                TO  3800-DEBIT-CREDIT            ECS190
00818          ADD 3800-AMOUNT         TO  ACCT-LF-OV-DB                ECS190
00819      ELSE                                                         ECS190
00820          MOVE 'C'                TO  3800-DEBIT-CREDIT            ECS190
00821          ADD 3800-AMOUNT         TO  ACCT-LF-OV-CR.               ECS190
00822                                                                   ECS190
00823      IF ACCT-LF-OV NOT = ZERO                                     ECS190
00824          PERFORM 2500-WRITE-ABC-REC  THRU  2599-XIT.              ECS190
00825                                                                   ECS190
00826      MOVE GL-AH-OV-NO            TO  3800-GENERAL-LEDGER.         ECS190
00827      MOVE ACCT-AH-OV             TO  3800-AMOUNT.                 ECS190
00828                                                                   ECS190
00829      IF ACCT-AH-OV NOT LESS ZERO                                  ECS190
00830          MOVE 'D'                TO  3800-DEBIT-CREDIT            ECS190
00831          ADD 3800-AMOUNT         TO  ACCT-AH-OV-DB                ECS190
00832      ELSE                                                         ECS190
00833          MOVE 'C'                TO  3800-DEBIT-CREDIT            ECS190
00834          ADD 3800-AMOUNT         TO  ACCT-AH-OV-CR.               ECS190
00835                                                                   ECS190
00836      IF ACCT-AH-OV NOT = ZERO                                     ECS190
00837          PERFORM 2500-WRITE-ABC-REC  THRU  2599-XIT.              ECS190
00838                                                                   ECS190
00839  3090-ACCT-RESET.                                                 ECS190
00840      COMPUTE COMP-ACC-REC = COMP-ACC-REC + ACCT-ACC-REC.          ECS190
00841      COMPUTE COMP-ACC-REC-DB = COMP-ACC-REC-DB + ACCT-ACC-REC-DB. ECS190
00842      COMPUTE COMP-ACC-REC-CR = COMP-ACC-REC-CR + ACCT-ACC-REC-CR. ECS190
00843                                                                   ECS190
00844      COMPUTE COMP-LF-PRM = COMP-LF-PRM + ACCT-LF-PRM.             ECS190
00845      COMPUTE COMP-LF-PRM-DB = COMP-LF-PRM-DB + ACCT-LF-PRM-DB.    ECS190
00846      COMPUTE COMP-LF-PRM-CR = COMP-LF-PRM-CR + ACCT-LF-PRM-CR.    ECS190
00847                                                                   ECS190
00848      COMPUTE COMP-AH-PRM = COMP-AH-PRM + ACCT-AH-PRM.             ECS190
00849      COMPUTE COMP-AH-PRM-DB = COMP-AH-PRM-DB + ACCT-AH-PRM-DB.    ECS190
00850      COMPUTE COMP-AH-PRM-CR = COMP-AH-PRM-CR + ACCT-AH-PRM-CR.    ECS190
00851                                                                   ECS190
00852      COMPUTE COMP-LF-COM = COMP-LF-COM + ACCT-LF-COM.             ECS190
00853      COMPUTE COMP-LF-COM-DB = COMP-LF-COM-DB + ACCT-LF-COM-DB.    ECS190
00854      COMPUTE COMP-LF-COM-CR = COMP-LF-COM-CR + ACCT-LF-COM-CR.    ECS190
00855                                                                   ECS190
00856      COMPUTE COMP-AH-COM = COMP-AH-COM + ACCT-AH-COM.             ECS190
00857      COMPUTE COMP-AH-COM-DB = COMP-AH-COM-DB + ACCT-AH-COM-DB.    ECS190
00858      COMPUTE COMP-AH-COM-CR = COMP-AH-COM-CR + ACCT-AH-COM-CR.    ECS190
00859                                                                   ECS190
00860      COMPUTE COMP-LF-OV = COMP-LF-OV  + ACCT-LF-OV.               ECS190
00861      COMPUTE COMP-LF-OV-DB = COMP-LF-OV-DB + ACCT-LF-OV-DB.       ECS190
00862      COMPUTE COMP-LF-OV-CR = COMP-LF-OV-CR + ACCT-LF-OV-CR.       ECS190
00863                                                                   ECS190
00864      COMPUTE COMP-AH-OV = COMP-AH-OV + ACCT-AH-OV.                ECS190
00865      COMPUTE COMP-AH-OV-DB = COMP-AH-OV-DB + ACCT-AH-OV-DB.       ECS190
00866      COMPUTE COMP-AH-OV-CR  = COMP-AH-OV-CR  + ACCT-AH-OV-CR.     ECS190
00867                                                                   ECS190
00868      MOVE COMP-ZEROS             TO  ACCOUNT-TOTALS.              ECS190
00869      MOVE CP-ACCOUNT             TO  PREV-ACCT.                   ECS190
00870      MOVE CP-STATE               TO  PREV-STATE.                  ECS190
00871                                                                   ECS190
00872  3099-XIT.                                                        ECS190
00873      EXIT.                                                        ECS190
00874                                                                   ECS190
00875  3100-COMP-BREAK.                                                 ECS190
00876      MOVE GL-ACCT-REC-NO         TO  DET-AR-GLNO.                 ECS190
00877      MOVE GL-LF-PRM-NO           TO  DET-LP-GLNO.                 ECS190
00878      MOVE GL-AH-PRM-NO           TO  DET-AP-GLNO.                 ECS190
00879      MOVE GL-LF-COM-NO           TO  DET-LC-GLNO.                 ECS190
00880      MOVE GL-AH-COM-NO           TO  DET-AC-GLNO.                 ECS190
00881      MOVE GL-LF-OV-NO            TO  DET-LO-GLNO.                 ECS190
00882      MOVE GL-AH-OV-NO            TO  DET-AO-GLNO.                 ECS190
00883      MOVE SPACE-3                TO  X.                           ECS190
00884      MOVE DETAIL-GL-LINE         TO  P-DATA.                      ECS190
00885                                                                   ECS190
00886      PERFORM 8100-PRINT-ROUTINE  THRU  8199-XIT.                  ECS190
00887                                                                   ECS190
00888      MOVE SPACES                 TO  DETAIL-LINE-1.               ECS190
00889      MOVE PREV-CARR              TO  DET-CARR.                    ECS190
00890      MOVE PREV-COMP              TO  DET-COMP.                    ECS190
00891      MOVE COMP-ACC-REC           TO  DET-ACC-REC.                 ECS190
00892      MOVE COMP-LF-PRM            TO  DET-LF-PREM.                 ECS190
00893      MOVE COMP-AH-PRM            TO  DET-AH-PREM.                 ECS190
00894      MOVE COMP-LF-COM            TO  DET-LF-COMM.                 ECS190
00895      MOVE COMP-AH-COM            TO  DET-AH-COMM.                 ECS190
00896      MOVE COMP-LF-OV             TO  DET-LF-OV.                   ECS190
00897      MOVE COMP-AH-OV             TO  DET-AH-OV.                   ECS190
00898      MOVE SPACE-1                TO  X.                           ECS190
00899      MOVE DETAIL-LINE-1          TO  P-DATA.                      ECS190
00900                                                                   ECS190
00901      PERFORM 8100-PRINT-ROUTINE  THRU  8199-XIT.                  ECS190
00902                                                                   ECS190
00903      MOVE SPACES                 TO  DETAIL-LINE-1.               ECS190
00904      MOVE '      TOTAL CREDITS'  TO  DET-DESC.                    ECS190
00905      MOVE COMP-ACC-REC-CR        TO  DET-ACC-REC.                 ECS190
00906      MOVE COMP-LF-PRM-CR         TO  DET-LF-PREM.                 ECS190
00907      MOVE COMP-AH-PRM-CR         TO  DET-AH-PREM.                 ECS190
00908      MOVE COMP-LF-COM-CR         TO  DET-LF-COMM.                 ECS190
00909      MOVE COMP-AH-COM-CR         TO  DET-AH-COMM.                 ECS190
00910      MOVE COMP-LF-OV-CR          TO  DET-LF-OV.                   ECS190
00911      MOVE COMP-AH-OV-CR          TO  DET-AH-OV.                   ECS190
00912      MOVE SPACE-2                TO  X.                           ECS190
00913      MOVE DETAIL-LINE-1          TO  P-DATA.                      ECS190
00914                                                                   ECS190
00915      PERFORM 8100-PRINT-ROUTINE  THRU  8199-XIT.                  ECS190
00916                                                                   ECS190
00917      MOVE SPACES                 TO  DETAIL-LINE-1.               ECS190
00918      MOVE '      TOTAL DEBITS '  TO  DET-DESC.                    ECS190
00919      MOVE COMP-ACC-REC-DB        TO  DET-ACC-REC.                 ECS190
00920      MOVE COMP-LF-PRM-DB         TO  DET-LF-PREM.                 ECS190
00921      MOVE COMP-AH-PRM-DB         TO  DET-AH-PREM.                 ECS190
00922      MOVE COMP-LF-COM-DB         TO  DET-LF-COMM.                 ECS190
00923      MOVE COMP-AH-COM-DB         TO  DET-AH-COMM.                 ECS190
00924      MOVE COMP-LF-OV-DB          TO  DET-LF-OV.                   ECS190
00925      MOVE COMP-AH-OV-DB          TO  DET-AH-OV.                   ECS190
00926      MOVE SPACE-1                TO  X.                           ECS190
00927      MOVE DETAIL-LINE-1          TO  P-DATA.                      ECS190
00928                                                                   ECS190
00929      PERFORM 8100-PRINT-ROUTINE  THRU  8199-XIT.                  ECS190
00930                                                                   ECS190
00931      COMPUTE CARR-ACC-REC = CARR-ACC-REC + COMP-ACC-REC.          ECS190
00932      COMPUTE CARR-ACC-REC-DB = CARR-ACC-REC-DB + COMP-ACC-REC-DB. ECS190
00933      COMPUTE CARR-ACC-REC-CR = CARR-ACC-REC-CR + COMP-ACC-REC-CR. ECS190
00934                                                                   ECS190
00935      COMPUTE CARR-LF-PRM = CARR-LF-PRM + COMP-LF-PRM.             ECS190
00936      COMPUTE CARR-LF-PRM-DB = CARR-LF-PRM-DB + COMP-LF-PRM-DB.    ECS190
00937      COMPUTE CARR-LF-PRM-CR = CARR-LF-PRM-CR + COMP-LF-PRM-CR.    ECS190
00938                                                                   ECS190
00939      COMPUTE CARR-AH-PRM = CARR-AH-PRM + COMP-AH-PRM.             ECS190
00940      COMPUTE CARR-AH-PRM-DB = CARR-AH-PRM-DB + COMP-AH-PRM-DB.    ECS190
00941      COMPUTE CARR-AH-PRM-CR = CARR-AH-PRM-CR + COMP-AH-PRM-CR.    ECS190
00942                                                                   ECS190
00943      COMPUTE CARR-LF-COM = CARR-LF-COM + COMP-LF-COM.             ECS190
00944      COMPUTE CARR-LF-COM-DB = CARR-LF-COM-DB + COMP-LF-COM-DB.    ECS190
00945      COMPUTE CARR-LF-COM-CR = CARR-LF-COM-CR + COMP-LF-COM-CR.    ECS190
00946                                                                   ECS190
00947      COMPUTE CARR-AH-COM = CARR-AH-COM + COMP-AH-COM.             ECS190
00948      COMPUTE CARR-AH-COM-DB = CARR-AH-COM-DB + COMP-AH-COM-DB.    ECS190
00949      COMPUTE CARR-AH-COM-CR = CARR-AH-COM-CR + COMP-AH-COM-CR.    ECS190
00950                                                                   ECS190
00951      COMPUTE CARR-LF-OV = CARR-LF-OV + COMP-LF-OV.                ECS190
00952      COMPUTE CARR-LF-OV-DB = CARR-LF-OV-DB + COMP-LF-OV-DB.       ECS190
00953      COMPUTE CARR-LF-OV-CR = CARR-LF-OV-CR + COMP-LF-OV-CR.       ECS190
00954                                                                   ECS190
00955      COMPUTE CARR-AH-OV = CARR-AH-OV + COMP-AH-OV.                ECS190
00956      COMPUTE CARR-AH-OV-DB = CARR-AH-OV-DB + COMP-AH-OV-DB.       ECS190
00957      COMPUTE CARR-AH-OV-CR = CARR-AH-OV-CR + COMP-AH-OV-CR.       ECS190
00958                                                                   ECS190
00959      MOVE COMP-ZEROS             TO  COMPANY-TOTALS.              ECS190
00960      MOVE CP-GROUPING            TO  PREV-COMP.                   ECS190
00961                                                                   ECS190
00962  3199-XIT.                                                        ECS190
00963      EXIT.                                                        ECS190
00964                                                                   ECS190
00965  3200-CARR-BREAK.                                                 ECS190
00966      MOVE SPACES                 TO  DETAIL-LINE-1.               ECS190
00967      MOVE PREV-CARR              TO  DET-CARR.                    ECS190
00968      MOVE CARR-ACC-REC           TO  DET-ACC-REC.                 ECS190
00969      MOVE CARR-LF-PRM            TO  DET-LF-PREM.                 ECS190
00970      MOVE CARR-AH-PRM            TO  DET-AH-PREM.                 ECS190
00971      MOVE CARR-LF-COM            TO  DET-LF-COMM.                 ECS190
00972      MOVE CARR-AH-COM            TO  DET-AH-COMM.                 ECS190
00973      MOVE CARR-LF-OV             TO  DET-LF-OV.                   ECS190
00974      MOVE CARR-AH-OV             TO  DET-AH-OV.                   ECS190
00975      MOVE SPACE-3                TO  X.                           ECS190
00976      MOVE SPACES                 TO  P-DATA.                      ECS190
00977                                                                   ECS190
00978      PERFORM 8100-PRINT-ROUTINE  THRU  8199-XIT.                  ECS190
00979                                                                   ECS190
00980      MOVE SPACE-3                TO  X.                           ECS190
00981      MOVE DETAIL-LINE-1          TO  P-DATA.                      ECS190
00982                                                                   ECS190
00983      PERFORM 8100-PRINT-ROUTINE  THRU  8199-XIT.                  ECS190
00984                                                                   ECS190
00985      MOVE SPACES                 TO  DETAIL-LINE-1.               ECS190
00986      MOVE '      TOTAL CREDITS'  TO  DET-DESC.                    ECS190
00987      MOVE CARR-ACC-REC-CR        TO  DET-ACC-REC.                 ECS190
00988      MOVE CARR-LF-PRM-CR         TO  DET-LF-PREM.                 ECS190
00989      MOVE CARR-AH-PRM-CR         TO  DET-AH-PREM.                 ECS190
00990      MOVE CARR-LF-COM-CR         TO  DET-LF-COMM.                 ECS190
00991      MOVE CARR-AH-COM-CR         TO  DET-AH-COMM.                 ECS190
00992      MOVE CARR-LF-OV-CR          TO  DET-LF-OV.                   ECS190
00993      MOVE CARR-AH-OV-CR          TO  DET-AH-OV.                   ECS190
00994      MOVE SPACE-2                TO  X.                           ECS190
00995      MOVE DETAIL-LINE-1          TO  P-DATA.                      ECS190
00996                                                                   ECS190
00997      PERFORM 8100-PRINT-ROUTINE  THRU  8199-XIT.                  ECS190
00998                                                                   ECS190
00999      MOVE SPACES                 TO  DETAIL-LINE-1.               ECS190
01000      MOVE '      TOTAL DEBITS '  TO  DET-DESC.                    ECS190
01001      MOVE CARR-ACC-REC-DB        TO  DET-ACC-REC.                 ECS190
01002      MOVE CARR-LF-PRM-DB         TO  DET-LF-PREM.                 ECS190
01003      MOVE CARR-AH-PRM-DB         TO  DET-AH-PREM.                 ECS190
01004      MOVE CARR-LF-COM-DB         TO  DET-LF-COMM.                 ECS190
01005      MOVE CARR-AH-COM-DB         TO  DET-AH-COMM.                 ECS190
01006      MOVE CARR-LF-OV-DB          TO  DET-LF-OV.                   ECS190
01007      MOVE CARR-AH-OV-DB          TO  DET-AH-OV.                   ECS190
01008      MOVE SPACE-1                TO  X.                           ECS190
01009      MOVE DETAIL-LINE-1          TO  P-DATA.                      ECS190
01010                                                                   ECS190
01011      PERFORM 8100-PRINT-ROUTINE  THRU  8199-XIT.                  ECS190
01012                                                                   ECS190
01013      COMPUTE GRND-ACC-REC = GRND-ACC-REC + CARR-ACC-REC.          ECS190
01014      COMPUTE GRND-ACC-REC-DB = GRND-ACC-REC-DB + CARR-ACC-REC-DB. ECS190
01015      COMPUTE GRND-ACC-REC-CR = GRND-ACC-REC-CR + CARR-ACC-REC-CR. ECS190
01016                                                                   ECS190
01017      COMPUTE GRND-LF-PRM = GRND-LF-PRM + CARR-LF-PRM.             ECS190
01018      COMPUTE GRND-LF-PRM-DB = GRND-LF-PRM-DB + CARR-LF-PRM-DB.    ECS190
01019      COMPUTE GRND-LF-PRM-CR = GRND-LF-PRM-CR + CARR-LF-PRM-CR.    ECS190
01020                                                                   ECS190
01021      COMPUTE GRND-AH-PRM = GRND-AH-PRM + CARR-AH-PRM.             ECS190
01022      COMPUTE GRND-AH-PRM-DB = GRND-AH-PRM-DB + CARR-AH-PRM-DB.    ECS190
01023      COMPUTE GRND-AH-PRM-CR = GRND-AH-PRM-CR + CARR-AH-PRM-CR.    ECS190
01024                                                                   ECS190
01025      COMPUTE GRND-LF-COM = GRND-LF-COM + CARR-LF-COM.             ECS190
01026      COMPUTE GRND-LF-COM-DB = GRND-LF-COM-DB + CARR-LF-COM-DB.    ECS190
01027      COMPUTE GRND-LF-COM-CR = GRND-LF-COM-CR + CARR-LF-COM-CR.    ECS190
01028                                                                   ECS190
01029      COMPUTE GRND-AH-COM = GRND-AH-COM + CARR-AH-COM.             ECS190
01030      COMPUTE GRND-AH-COM-DB = GRND-AH-COM-DB + CARR-AH-COM-DB.    ECS190
01031      COMPUTE GRND-AH-COM-CR = GRND-AH-COM-CR + CARR-AH-COM-CR.    ECS190
01032                                                                   ECS190
01033      COMPUTE GRND-LF-OV = GRND-LF-OV + CARR-LF-OV.                ECS190
01034      COMPUTE GRND-LF-OV-DB = GRND-LF-OV-DB + CARR-LF-OV-DB.       ECS190
01035      COMPUTE GRND-LF-OV-CR = GRND-LF-OV-CR + CARR-LF-OV-CR.       ECS190
01036                                                                   ECS190
01037      COMPUTE GRND-AH-OV = GRND-AH-OV + CARR-AH-OV.                ECS190
01038      COMPUTE GRND-AH-OV-DB = GRND-AH-OV-DB + CARR-AH-OV-DB.       ECS190
01039      COMPUTE GRND-AH-OV-CR = GRND-AH-OV-CR + CARR-AH-OV-CR.       ECS190
01040                                                                   ECS190
01041      MOVE COMP-ZEROS             TO  CARRIER-TOTALS.              ECS190
01042      MOVE CP-CARRIER             TO  PREV-CARR.                   ECS190
01043                                                                   ECS190
01044  3299-XIT.                                                        ECS190
01045      EXIT.                                                        ECS190
01046                                                                   ECS190
01047  3300-GRAND-TOTALS.                                               ECS190
01048      PERFORM 8000-HEADING-ROUTINE  THRU  8099-XIT.                ECS190
01049                                                                   ECS190
01050      MOVE SPACES                 TO  DETAIL-LINE-1.               ECS190
01051      MOVE 'GRAND TOTALS'         TO  DET-DESC.                    ECS190
01052      MOVE GRND-ACC-REC           TO  DET-ACC-REC.                 ECS190
01053      MOVE GRND-LF-PRM            TO  DET-LF-PREM.                 ECS190
01054      MOVE GRND-AH-PRM            TO  DET-AH-PREM.                 ECS190
01055      MOVE GRND-LF-COM            TO  DET-LF-COMM.                 ECS190
01056      MOVE GRND-AH-COM            TO  DET-AH-COMM.                 ECS190
01057      MOVE GRND-LF-OV             TO  DET-LF-OV.                   ECS190
01058      MOVE GRND-AH-OV             TO  DET-AH-OV.                   ECS190
01059      MOVE SPACE-3                TO  X.                           ECS190
01060      MOVE DETAIL-LINE-1          TO  P-DATA.                      ECS190
01061                                                                   ECS190
01062      PERFORM 8100-PRINT-ROUTINE  THRU  8199-XIT.                  ECS190
01063                                                                   ECS190
01064      MOVE SPACES                 TO  DETAIL-LINE-1.               ECS190
01065      MOVE '      TOTAL CREDITS'  TO  DET-DESC.                    ECS190
01066      MOVE GRND-ACC-REC-CR        TO  DET-ACC-REC.                 ECS190
01067      MOVE GRND-LF-PRM-CR         TO  DET-LF-PREM.                 ECS190
01068      MOVE GRND-AH-PRM-CR         TO  DET-AH-PREM.                 ECS190
01069      MOVE GRND-LF-COM-CR         TO  DET-LF-COMM.                 ECS190
01070      MOVE GRND-AH-COM-CR         TO  DET-AH-COMM.                 ECS190
01071      MOVE GRND-LF-OV-CR          TO  DET-LF-OV.                   ECS190
01072      MOVE GRND-AH-OV-CR          TO  DET-AH-OV.                   ECS190
01073      MOVE SPACE-2                TO  X.                           ECS190
01074      MOVE DETAIL-LINE-1          TO  P-DATA.                      ECS190
01075                                                                   ECS190
01076      PERFORM 8100-PRINT-ROUTINE  THRU  8199-XIT.                  ECS190
01077                                                                   ECS190
01078      MOVE SPACES                 TO  DETAIL-LINE-1.               ECS190
01079      MOVE '      TOTAL DEBITS '  TO  DET-DESC.                    ECS190
01080      MOVE GRND-ACC-REC-DB        TO  DET-ACC-REC.                 ECS190
01081      MOVE GRND-LF-PRM-DB         TO  DET-LF-PREM.                 ECS190
01082      MOVE GRND-AH-PRM-DB         TO  DET-AH-PREM.                 ECS190
01083      MOVE GRND-LF-COM-DB         TO  DET-LF-COMM.                 ECS190
01084      MOVE GRND-AH-COM-DB         TO  DET-AH-COMM.                 ECS190
01085      MOVE GRND-LF-OV-DB          TO  DET-LF-OV.                   ECS190
01086      MOVE GRND-AH-OV-DB          TO  DET-AH-OV.                   ECS190
01087      MOVE SPACE-1                TO  X.                           ECS190
01088      MOVE DETAIL-LINE-1          TO  P-DATA.                      ECS190
01089                                                                   ECS190
01090      PERFORM 8100-PRINT-ROUTINE  THRU  8199-XIT.                  ECS190
01091                                                                   ECS190
01092  3399-XIT.                                                        ECS190
01093      EXIT.                                                        ECS190
01094  EJECT                                                            ECS190
01095  PERFORMED-PROCEDURES SECTION.                                    ECS190
01096                                                                   ECS190
01097  8000-HEADING-ROUTINE.                                            ECS190
01098      MOVE ZEROS                  TO  LINE-CNT.                    ECS190
01099                                                                   ECS190
01100      ADD 1                       TO  PAGE-CNT.                    ECS190
01101                                                                   ECS190
01102      MOVE SPACE-NP               TO  X.                           ECS190
01103      MOVE PAGE-CNT               TO  HD-PAGE.                     ECS190
01104      MOVE HEAD-LINE-1            TO  P-DATA.                      ECS190
01105      PERFORM 8100-PRINT-ROUTINE  THRU  8199-XIT.                  ECS190
01106                                                                   ECS190
01107      MOVE SPACE-1                TO  X.                           ECS190
01108      MOVE HEAD-LINE-2            TO  P-DATA.                      ECS190
01109      PERFORM 8100-PRINT-ROUTINE  THRU  8199-XIT.                  ECS190
01110                                                                   ECS190
01111      MOVE HEAD-LINE-3            TO  P-DATA.                      ECS190
01112      PERFORM 8100-PRINT-ROUTINE  THRU  8199-XIT.                  ECS190
01113                                                                   ECS190
01114      MOVE SPACE-2                TO  X.                           ECS190
01115      MOVE HEAD-LINE-4            TO  P-DATA.                      ECS190
01116      PERFORM 8100-PRINT-ROUTINE  THRU  8199-XIT.                  ECS190
01117                                                                   ECS190
01118      MOVE SPACE-1                TO  X.                           ECS190
01119      MOVE HEAD-LINE-5            TO  P-DATA.                      ECS190
01120      PERFORM 8100-PRINT-ROUTINE  THRU  8199-XIT.                  ECS190
01121                                                                   ECS190
01122      MOVE SPACE-2                TO  X.                           ECS190
01123      MOVE SPACES                 TO  P-DATA.                      ECS190
01124      PERFORM 8100-PRINT-ROUTINE  THRU  8199-XIT.                  ECS190
01125                                                                   ECS190
01126  8099-XIT.                                                        ECS190
01127      EXIT.                                                        ECS190
01128                                                                   ECS190
01129  8100-PRINT-ROUTINE.                                              ECS190
01130                              COPY ELCPRT2.                        ECS190
01131                                                                   ECS190
01132      IF X = SPACE-1                                               ECS190
01133          ADD +1                  TO  LINE-CNT                     ECS190
01134      ELSE                                                         ECS190
01135          IF X = SPACE-2                                           ECS190
01136              ADD +2              TO  LINE-CNT                     ECS190
01137          ELSE                                                     ECS190
01138              IF X = SPACE-3                                       ECS190
01139                  ADD +3          TO  LINE-CNT                     ECS190
01140              ELSE                                                 ECS190
01141                  MOVE +0         TO  LINE-CNT.                    ECS190
01142                                                                   ECS190
01143      IF LINE-CNT GREATER +52                                      ECS190
01144          PERFORM 8000-HEADING-ROUTINE  THRU  8099-XIT.            ECS190
01145                                                                   ECS190
01146  8199-XIT.                                                        ECS190
01147      EXIT.                                                        ECS190
01148                                                                   ECS190
01149  8200-LEDGER-TABLE-LOOKUP.                                        ECS190
01150      SET L                       TO  1.                           ECS190
01151                                                                   ECS190
01152      SEARCH LEDGER-NUMBERS   VARYING  L                           ECS190
01153          AT END                                                   ECS190
01154              GO TO 8290-LOOKUP-DEFAULT                            ECS190
01155          WHEN SEARCH-CARR-CO = LEDGER-CARR-CO (L)                 ECS190
01156              MOVE LEDGER-ACCT-NUMBERS (L)                         ECS190
01157                                  TO  GENERAL-LEDGER-NUMBERS       ECS190
01158              GO TO 8299-XIT.                                      ECS190
01159                                                                   ECS190
01160  8290-LOOKUP-DEFAULT.                                             ECS190
01161      IF SEARCH-COMP = '000001'                                    ECS190
01162          MOVE LEDGER-ACCT-NUMBERS (1)                             ECS190
01163                                  TO  GENERAL-LEDGER-NUMBERS.      ECS190
01164      IF SEARCH-COMP = '000002'                                    ECS190
01165          MOVE LEDGER-ACCT-NUMBERS (2)                             ECS190
01166                                  TO  GENERAL-LEDGER-NUMBERS.      ECS190
01167      IF SEARCH-COMP = '000003'                                    ECS190
01168          MOVE LEDGER-ACCT-NUMBERS (3)                             ECS190
01169                                  TO  GENERAL-LEDGER-NUMBERS.      ECS190
01170                                                                   ECS190
01171  8299-XIT.                                                        ECS190
01172      EXIT.                                                        ECS190
01173                                                                   ECS190
01174  ABEND-PGM SECTION.                                               ECS190
01175                              COPY ELCABEND.                       ECS190
