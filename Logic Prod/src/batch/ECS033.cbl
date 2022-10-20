00001  IDENTIFICATION DIVISION.                                         05/07/98
00002                                                                   ECS033
00003  PROGRAM-ID.                ECS033.                                  LV014
00004 *              PROGRAM CONVERTED BY                               ECS033
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ECS033
00006 *              CONVERSION DATE 11/28/95 11:08:54.                 ECS033
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          ECS033
00008 *                            VMOD=2.011.                          ECS033
00009                                                                   ECS033
00010 *AUTHOR.        LOGIC, INC.                                       ECS033
00011 *               DALLAS, TEXAS.                                    ECS033
00012                                                                   ECS033
00013 *DATE-COMPILED.                                                   ECS033
00014                                                                   ECS033
00015 *SECURITY.   *****************************************************ECS033
00016 *            *                                                   *ECS033
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ECS033
00018 *            *                                                   *ECS033
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS033
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ECS033
00021 *            *   THE PRIOR WRITTED PERMISSION OF LOGIC, INC.     *   CL*12
00022 *            *                                                   *ECS033
00023 *            *****************************************************ECS033
00024                                                                   ECS033
00025 *REMARKS.                                                         ECS033
00026 *        READS CLAIMS-HISTORY FILE AND BUILDS 6 EXTRACT RECORDS   ECS033
00027 *        THAT ARE USED FOR CLAIMS SPREADS.                        ECS033
00028                                                                   ECS033
00029 *        IF PROGRAM OPTION IS TWO (2), ECS033 BUILDS ONLY TYPE    ECS033
00030 *        NINE (9) EXTRACTS.                                       ECS033
00031                                                                   ECS033
00032 *        THE EXTRACT RECORDS ARE :                                ECS033
00033 *                (1)  BUSINESS CLASS WITHIN STATE                 ECS033
00034 *                (2)  BUSINESS CLASS WITHIN COMPANY               ECS033
00035 *                (3)  BUSINESS CLASS                              ECS033
00036 *                (4)  STATE                                       ECS033
00037 *                (5)  COMPANY                                     ECS033
00038 *                (6)  HIGH-VALUES TOTAL RECORD                    ECS033
00039 *                (9)  REINSURED CLAIM PAYMENTS                    ECS033
00040                                                                   ECS033
00041 *        PROGRAM PROCESS OPTIONS :                                ECS033
00042 *                (1)  EXTRACT DIRECT CLAIMS.                      ECS033
00043 *                (2)  EXTRACT REINSURED CLAIMS.                   ECS033
031102******************************************************************
031102*                   C H A N G E   L O G
031102*
031102* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
031102*-----------------------------------------------------------------
031102*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
031102* EFFECTIVE    NUMBER
031102*-----------------------------------------------------------------
062002* 062002    2002061700006  PEMA  INCREASE INCURRED MONTHS TO 120
031102******************************************************************
00044                                                                   ECS033
00045  ENVIRONMENT DIVISION.                                            ECS033
00046  CONFIGURATION SECTION.                                           ECS033
00047                                                                      CL**3
00048  INPUT-OUTPUT SECTION.                                            ECS033
00049  FILE-CONTROL.                                                    ECS033
00050                                                                   ECS033
00051      SELECT SORT-FILE    ASSIGN TO SYS001-UT-3380-S-SORTWK1.      ECS033
00052      SELECT PRNTR        ASSIGN TO SYS008-UR-1403-S-SYS008.       ECS033
00053      SELECT CLM-REC      ASSIGN TO SYS010-UT-2400-S-SYS010.       ECS033
pemuni     SELECT ACCT-MSTR    ASSIGN TO ERACCTT                        ECS033
00055                          ACCESS IS SEQUENTIAL                     ECS033
00056                          ORGANIZATION IS INDEXED                  ECS033
00057                          FILE STATUS IS AM-FILE-STATUS            ECS033
00058                          RECORD KEY IS AM-CONTROL-PRIMARY.        ECS033
00059      SELECT CLM-XTR      ASSIGN TO SYS012-UT-2400-S-SYS012.       ECS033
00060      SELECT DISK-DATE    ASSIGN TO SYS019-UT-3380-S-SYS019.       ECS033
00061      SELECT FICH         ASSIGN TO SYS020-UT-2400-S-SYS020.       ECS033
00062  EJECT                                                            ECS033
00063  DATA DIVISION.                                                   ECS033
00064  FILE SECTION.                                                    ECS033
00065                                                                   ECS033
00066  SD  SORT-FILE.                                                   ECS033
00067                                                                   ECS033
00068  01  SORT-REC.                                                    ECS033
00069      12  SORT-CONTROL-74     PIC X(27).                              CL**3
00070      12  FILLER              PIC  X(21).                          ECS033
00071                                                                   ECS033
00072                                                                      CL**3
00073  FD  PRNTR                                                        ECS033
00074                              COPY ELCPRTFD.                       ECS033
00075                                                                      CL**3
00076  EJECT                                                            ECS033
00077  FD  CLM-REC                                                      ECS033
00078      BLOCK CONTAINS 0 RECORDS
00079      RECORDING MODE F.                                            ECS033
00080                                                                   ECS033
00081                              COPY ECSEXT01.                       ECS033
00082  EJECT                                                            ECS033
00083  FD  ACCT-MSTR.                                                   ECS033
00084                                                                   ECS033
00085                              COPY ERCACCT.                        ECS033
00086  EJECT                                                            ECS033
00087  FD  CLM-XTR                                                      ECS033
00088      BLOCK CONTAINS 0 RECORDS
00089      RECORDING MODE F.                                            ECS033
00090                                                                   ECS033
00091  01  CLMS-XTRCT              PIC  X(48).                          ECS033
00092                                                                      CL**3
00093  EJECT                                                            ECS033
00094  FD  DISK-DATE                                                    ECS033
00095                              COPY ELCDTEFD.                       ECS033
00096  EJECT                                                            ECS033
00097  FD  FICH                                                         ECS033
00098                              COPY ELCFCHFD.                       ECS033
00099  EJECT                                                            ECS033
00100  WORKING-STORAGE SECTION.                                         ECS033
00101  77  FILLER  PIC  X(32) VALUE '********************************'. ECS033
00102  77  FILLER  PIC  X(32) VALUE '     ECS033 WORKING STORAGE     '. ECS033
00103  77  FILLER  PIC  X(32) VALUE '********** VMOD=2.011 **********'.    CL**5
00104                                                                   ECS033
00105  77  SAVE-CONTROL            PIC X(19)           VALUE SPACES.    ECS033
00106  77  WK1                     PIC S9(5)   COMP-3  VALUE +0.        ECS033
00107  77  WK2                     PIC S9(5)   COMP-3  VALUE +0.        ECS033
00108  77  WK                      PIC S9(5)   COMP-3  VALUE +0.        ECS033
00109  77  X                       PIC  X              VALUE SPACE.     ECS033
00110  77  AM-TEST                 PIC  XX.                             ECS033
00111  77  AM-FILE-STATUS          PIC  XX             VALUE '00'.      ECS033
00112  77  WS-AMT                  PIC S9(7).                           ECS033
00113                                                                   ECS033
00114  01  VSAM-ERROR.                                                  ECS033
00115      12  VSAM-CONSTANT       PIC  XX             VALUE '06'.      ECS033
00116      12  VSAM-ERR-CODE       PIC  XX             VALUE '00'.      ECS033
00117                                                                   ECS033
00118  01  WS-WK.                                                       ECS033
00119      12  WS-DE-COMP.                                              ECS033
00120          16  WS-DE-CARR          PIC  X          VALUE SPACES.    ECS033
00121          16  WS-DE-GROUP         PIC  X(6)       VALUE SPACES.    ECS033
00122      12  WS-RETURN-CODE          PIC  X(4)       VALUE SPACES.    ECS033
00123      12  WS-ABEND-MESSAGE        PIC  X(80)      VALUE SPACES.    ECS033
00124      12  WS-ABEND-FILE-STATUS    PIC  X(4)       VALUE SPACES.    ECS033
00125      12  WS-ZERO                 PIC S9          VALUE ZERO.      ECS033
00126      12  PGM-SUB                 PIC S9(3) COMP VALUE +33.        ECS033
00127                                                                      CL**5
00128  01  C-GA.                                                        ECS033
00129      12  FILLER              PIC  X              VALUE '0'.       ECS033
00130      12  C-DIG               PIC  X              VALUE ' '.       ECS033
00131                                                                   ECS033
00132  01  CLMS-BLD.                                                    ECS033
00133      12  X-TYPE              PIC  X.                              ECS033
00134      12  X-ID                PIC  X.                              ECS033
00135      12  X-COMP.                                                  ECS033
00136          16  X-CARR          PIC  X.                              ECS033
00137          16  X-GROUP         PIC  X(6).                           ECS033
00138      12  X-STATE             PIC  XX.                             ECS033
00139      12  X-ACC.                                                   ECS033
00140          16  X-ACC-PREFIX    PIC  X(4).                           ECS033
00141          16  X-GA            PIC  XX.                             ECS033
00142          16  FILLER          PIC  X(4).                           ECS033
00143      12  X-CLASS             PIC  XX.                             ECS033
00144      12  X-PD                PIC S99.                             ECS033
062002     12  X-INC               PIC S999        COMP-3.              ECS033
00146      12  X-AMT               PIC S9(9)V99    COMP-3.              ECS033
00147      12  X-ECS033-PGM-OPT    PIC X.                               ECS033
00148      12  FILLER              PIC X(14)  VALUE SPACES.                CL**6
00149                                                                   ECS033
00150  EJECT                                                            ECS033
00151  01  HEAD-1.                                                      ECS033
00152      12  FILLER              PIC  X(52)          VALUE SPACES.    ECS033
00153      12  FILLER              PIC  X(21)          VALUE            ECS033
00154              'CLAIMS SPREAD EXTRACT'.                             ECS033
00155      12  FILLER              PIC  X(46)          VALUE SPACES.    ECS033
00156      12  FILLER              PIC  X(7)           VALUE 'ECS033 '. ECS033
00157                                                                   ECS033
00158  01  HEAD-2.                                                      ECS033
00159      12  FILLER              PIC  X(47)          VALUE SPACES.    ECS033
00160      12  HD-CLIENT           PIC  X(30)          VALUE SPACES.    ECS033
00161      12  FILLER              PIC  X(42)          VALUE SPACES.    ECS033
00162      12  HD-DATE             PIC  X(8)           VALUE SPACES.    ECS033
00163                                                                   ECS033
00164  01  HEAD-3.                                                      ECS033
00165      12  FILLER              PIC  X(53)          VALUE SPACES.    ECS033
00166      12  HD-ALF-DTE          PIC  X(18)          VALUE SPACES.    ECS033
00167      12  FILLER              PIC  X(48)          VALUE SPACES.    ECS033
00168      12  FILLER              PIC  X(5)           VALUE 'PAGE '.   ECS033
00169      12  HD-PAGE             PIC ZZ,ZZZ.                          ECS033
00170                                                                   ECS033
00171                              COPY ELCDTECX.                       ECS033
00172                                                                   ECS033
00173                              COPY ELCDTEVR.                       ECS033
00174                                                                   ECS033
00175                              COPY ELCEXTVR.                       ECS033
00176                                                                   ECS033
00177                              COPY ELCDATE.                           CL**7
00178  EJECT                                                            ECS033
00179  PROCEDURE DIVISION.                                              ECS033
00180                                                                   ECS033
00181  0000-OPEN-EM.                                                    ECS033
00182                              COPY ELCDTERX.                       ECS033
00183                                                                   ECS033
00184      OPEN  INPUT CLM-REC ACCT-MSTR                                ECS033
00185            OUTPUT CLM-XTR.                                        ECS033
00186                                                                   ECS033
00187      IF AM-FILE-STATUS  = '00' OR '97'                            ECS033
00188          NEXT SENTENCE                                            ECS033
00189        ELSE                                                       ECS033
00190          MOVE AM-FILE-STATUS     TO  WS-ABEND-FILE-STATUS         ECS033
00191          MOVE AM-FILE-STATUS     TO  VSAM-ERR-CODE                ECS033
00192          MOVE VSAM-ERROR         TO  WS-RETURN-CODE               ECS033
00193          MOVE 'ERACCT OPEN ERROR '                                ECS033
00194                                  TO  WS-ABEND-MESSAGE             ECS033
00195          GO TO ABEND-PGM.                                         ECS033
00196                                                                   ECS033
00197  0100-SORT-ROUTINE.                                               ECS033
00198      SORT SORT-FILE  ASCENDING KEY SORT-CONTROL-74                ECS033
00199          INPUT PROCEDURE 0200-READ-CLAIMS      THRU  0799-EXIT    ECS033
00200          OUTPUT PROCEDURE 0800-WRITE-EXTRACTS  THRU  0899-EXIT.   ECS033
00201                                                                   ECS033
00202      IF SORT-RETURN  NOT = ZEROS                                  ECS033
00203          MOVE 'INTERNAL SORT 01 ABORTED'                          ECS033
00204                                  TO  WS-ABEND-MESSAGE             ECS033
00205          MOVE '0101'             TO  WS-RETURN-CODE               ECS033
00206          GO TO ABEND-PGM.                                         ECS033
00207                                                                   ECS033
00208      GO TO 1300-E-O-J.                                            ECS033
00209  EJECT                                                            ECS033
00210  0200-READ-CLAIMS  SECTION.                                       ECS033
00211      PERFORM 0600-READ-MSTR  THRU  0699-EXIT.                     ECS033
00212                                                                   ECS033
00213      COMPUTE WK = (RUN-CCYY * 12) + RUN-MO.                       ECS033
00214                                                                   ECS033
00215  0210-R-CLMS.                                                     ECS033
00216                                                                   ECS033
00217      READ CLM-REC  AT END                                         ECS033
00218          GO TO 0700-CLOSE-EM.                                     ECS033
00219                                                                   ECS033
00220      COPY ELCEXTM1.                                               ECS033
00221                                                                   ECS033
00222  0220-MATCH-EM.                                                   ECS033
00223                                                                   ECS033
00224      IF (DTE-PGM-OPT = '2') AND                                      CL**4
00225         (DE-REIN NOT = 'R')                                          CL**4
00226         GO TO 0210-R-CLMS                                         ECS033
00227      ELSE                                                         ECS033
00228      IF (DTE-PGM-OPT NOT = '2') AND                                  CL**4
00229         (DE-REIN  = 'R')                                             CL**4
00230         GO TO 0210-R-CLMS.                                        ECS033
00231                                                                   ECS033
00232      IF DE-RECORD-ID  NOT = 'DE'                                  ECS033
00233         GO TO 0210-R-CLMS.                                        ECS033
00234                                                                   ECS033
00235      IF DTE-PGM-OPT = '2'                                            CL**4
00236         IF DE-TYPE = '2' OR '4'                                      CL**4
00237            MOVE '2'              TO DE-TYPE                       ECS033
00238         ELSE                                                      ECS033
00239            GO TO 0210-R-CLMS.                                     ECS033
00240                                                                   ECS033
00241      IF DE-PAY NOT NUMERIC                                           CL**4
00242          IF DE-INCUR NOT NUMERIC                                     CL**4
00243              GO TO 0210-R-CLMS                                       CL**4
00244          ELSE                                                     ECS033
00245              MOVE DE-INCUR       TO DE-PAY                           CL**4
00246                                     WS-DE-PAY-N.                     CL**4
00247                                                                   ECS033
00248      IF DTE-CLIENT = 'GTL'  OR                                       CL**4
00249         DTE-PGM-OPT = '2'                                            CL**4
00250         MOVE DE-PAY              TO DE-CLM-PROC-DT                   CL**4
00251                                     WS-DE-CLM-PROC-DT-N.             CL**4
00252                                                                      CL**4
00253      IF DE-CLM-PROC-DT = ZEROS                                       CL**4
00254          MOVE DE-PAY             TO DE-CLM-PROC-DT                   CL**4
00255                                     WS-DE-CLM-PROC-DT-N.             CL**4
00256                                                                   ECS033
00257      IF DE-INCUR GREATER THAN DE-CLM-PROC-DT                         CL*13
00258          MOVE DE-PAY             TO DE-INCUR                         CL**4
00259                                     WS-DE-INCUR-N.                   CL**4
00260                                                                   ECS033
00261      COMPUTE WK1 = WK - +12.                                      ECS033
00262                                                                   ECS033
00263      COMPUTE WK2 = (DE-CP-CCYY * 12) + DE-CP-MO.                     CL**4
00264                                                                   ECS033
00265      MOVE +0                     TO  X-PD.                        ECS033
00266                                                                   ECS033
00267      IF WK2  IS NOT GREATER THAN  WK1                             ECS033
00268          MOVE +13                TO  X-PD.                        ECS033
00269                                                                   ECS033
00270      IF WK2 GREATER WK                                            ECS033
00271          GO TO 0210-R-CLMS.                                       ECS033
00272                                                                   ECS033
00273      MOVE '99'                   TO  X-CLASS.                     ECS033
00274                                                                   ECS033
00275      IF DTE-CLIENT  =  'FIA'                                      ECS033
00276          MOVE DE-ACC-GPCD        TO  AM-TEST                      ECS033
00277          GO TO 0240-SET-X-CLASS.                                  ECS033
00278                                                                   ECS033
00279      IF DE-CNTRL1 EQUAL SAVE-CONTROL                              ECS033
00280          GO TO 0235-CONTINUE                                      ECS033
00281      ELSE                                                         ECS033
00282          MOVE 'XX'               TO  AM-TEST.                     ECS033
00283                                                                   ECS033
00284  0230-MATCH-MSTR.                                                 ECS033
00285      IF AM-CNTRL-1 LESS DE-CNTRL1                                 ECS033
00286          PERFORM 0600-READ-MSTR  THRU  0699-EXIT                  ECS033
00287          GO TO 0230-MATCH-MSTR.                                   ECS033
00288                                                                   ECS033
00289      IF AM-CNTRL-1  = DE-CNTRL1                                   ECS033
00290          MOVE DE-CNTRL1 TO SAVE-CONTROL                           ECS033
00291          MOVE AM-GPCD TO AM-TEST                                  ECS033
00292          PERFORM 0600-READ-MSTR  THRU  0699-EXIT                  ECS033
00293          GO TO 0230-MATCH-MSTR.                                   ECS033
00294                                                                   ECS033
00295  0235-CONTINUE.                                                   ECS033
00296      IF AM-TEST  = 'XX'                                           ECS033
00297          MOVE DE-ACC-GPCD TO AM-TEST.                             ECS033
00298                                                                   ECS033
00299  0240-SET-X-CLASS.                                                ECS033
00300      MOVE AM-TEST                TO  X-CLASS.                     ECS033
00301                                                                   ECS033
00302  0250-FILL-XTR.                                                   ECS033
00303      MOVE DE-TYPE                TO  X-TYPE.                      ECS033
00304      MOVE '1'                    TO  X-ID.                        ECS033
00305      MOVE DE-STATE               TO  X-STATE.                     ECS033
00306      MOVE SPACES                 TO  X-ACC  X-COMP.               ECS033
00307                                                                   ECS033
00308      IF X-PD  NOT = +13                                           ECS033
00309          IF DE-CP-MO GREATER RUN-MO                                  CL**4
00310              COMPUTE X-PD = DE-CP-MO - RUN-MO                        CL**4
00311          ELSE                                                        CL**4
00312              COMPUTE X-PD = DE-CP-MO + (+12 - RUN-MO).               CL**4
00313                                                                   ECS033
00314      IF DTE-PGM-OPT EQUAL '2'                                     ECS033
00315         MOVE DE-REI-CLAIM-AMT    TO  X-AMT                        ECS033
00316      ELSE                                                         ECS033
00317         MOVE DE-CLAIM-AMT        TO  X-AMT.                       ECS033
00318                                                                   ECS033
00319      COMPUTE WK1 = (DE-INCUR-CCYY * 12) + DE-INCUR-MO.               CL**3
00320                                                                   ECS033
00321 *    COMPUTE X-INC = +84 - (WK - WK1).                            ECS033
062002     COMPUTE X-INC = +120 - (WK - WK1).                           ECS033
00322                                                                   ECS033
00323      IF X-INC  IS NOT GREATER THAN  +0                            ECS033
00324          MOVE +1                 TO  X-INC.                       ECS033
00325                                                                   ECS033
00326  0260-WR-OUT.                                                     ECS033
00327                                                                   ECS033
00328 *    IF X-INC LESS +1 OR GREATER +84                              ECS033
062002     IF X-INC LESS +1 OR GREATER +120                             ECS033
00329          GO TO 0210-R-CLMS.                                       ECS033
00330                                                                   ECS033
00331      IF X-PD LESS +1 OR GREATER +13                               ECS033
00332          GO TO 0210-R-CLMS.                                       ECS033
00333                                                                   ECS033
00334      IF DTE-PGM-OPT EQUAL '2'                                     ECS033
00335         MOVE '2'                 TO  X-ECS033-PGM-OPT             ECS033
00336         MOVE HIGH-VALUES         TO  X-CLASS                      ECS033
00337                                      X-STATE                      ECS033
00338         MOVE DE-REI-COMP         TO  X-GROUP                      ECS033
00339         MOVE DE-ACCOUNT          TO  X-ACC                        ECS033
00340         MOVE '9'                 TO  X-ID                         ECS033
00341         PERFORM 0300-RELEASE-SORT THRU 0399-EXIT                  ECS033
00342         GO TO 0210-R-CLMS.                                        ECS033
00343                                                                   ECS033
00344      PERFORM 0300-RELEASE-SORT  THRU  0399-EXIT.                  ECS033
00345                                                                   ECS033
00346      MOVE '2'                    TO  X-ID.                        ECS033
00347      MOVE HIGH-VALUES            TO  X-ACC  X-STATE.              ECS033
00348                                                                   ECS033
00349      MOVE DE-CARRIER             TO WS-DE-CARR                    ECS033
00350      MOVE DE-GROUPING            TO WS-DE-GROUP                   ECS033
00351      MOVE WS-DE-COMP             TO  X-COMP.                      ECS033
00352                                                                   ECS033
00353      PERFORM 0300-RELEASE-SORT  THRU  0399-EXIT.                  ECS033
00354                                                                   ECS033
00355      MOVE '3'                    TO  X-ID.                        ECS033
00356      MOVE HIGH-VALUES            TO  X-COMP  X-ACC  X-STATE.      ECS033
00357                                                                   ECS033
00358      PERFORM 0300-RELEASE-SORT  THRU  0399-EXIT.                  ECS033
00359                                                                   ECS033
00360      MOVE '4'                    TO  X-ID.                        ECS033
00361      MOVE HIGH-VALUES            TO  X-CLASS.                     ECS033
00362      MOVE DE-STATE               TO  X-STATE.                     ECS033
00363                                                                   ECS033
00364      PERFORM 0300-RELEASE-SORT  THRU  0399-EXIT.                  ECS033
00365                                                                   ECS033
00366      MOVE '5'                    TO  X-ID.                        ECS033
00367      MOVE HIGH-VALUES            TO  X-STATE.                     ECS033
00368      MOVE WS-DE-COMP             TO  X-COMP.                      ECS033
00369                                                                   ECS033
00370      PERFORM 0300-RELEASE-SORT  THRU  0399-EXIT.                  ECS033
00371                                                                   ECS033
00372      MOVE '6'                    TO  X-ID.                        ECS033
00373      MOVE HIGH-VALUES            TO  X-COMP.                      ECS033
00374                                                                   ECS033
00375      PERFORM 0300-RELEASE-SORT  THRU  0399-EXIT.                  ECS033
00376                                                                   ECS033
00377      GO TO 0210-R-CLMS.                                           ECS033
00378                                                                   ECS033
00379  0300-RELEASE-SORT.                                               ECS033
00380                                                                      CL**6
00381      RELEASE SORT-REC  FROM  CLMS-BLD.                            ECS033
00382                                                                   ECS033
00383  0399-EXIT.                                                       ECS033
00384      EXIT.                                                        ECS033
00385                                                                   ECS033
00386  EJECT                                                               CL**4
00387  0600-READ-MSTR.                                                  ECS033
00388      READ ACCT-MSTR.                                              ECS033
00389                                                                   ECS033
00390      IF AM-FILE-STATUS  =  '00' OR '10'                           ECS033
00391          NEXT SENTENCE                                            ECS033
00392      ELSE                                                         ECS033
00393          MOVE AM-FILE-STATUS     TO  WS-ABEND-FILE-STATUS         ECS033
00394          MOVE AM-FILE-STATUS     TO  VSAM-ERR-CODE                ECS033
00395          MOVE VSAM-ERROR         TO  WS-RETURN-CODE               ECS033
00396          MOVE 'VSAM SEQUENCE ERROR '                              ECS033
00397                                  TO  WS-ABEND-MESSAGE             ECS033
00398          GO TO ABEND-PGM.                                         ECS033
00399                                                                   ECS033
00400      IF AM-FILE-STATUS  = '10'                                    ECS033
00401          MOVE HIGH-VALUES        TO  AM-CNTRL-1.                  ECS033
00402                                                                   ECS033
00403  0699-EXIT.                                                       ECS033
00404      EXIT.                                                        ECS033
00405                                                                   ECS033
00406  0700-CLOSE-EM.                                                   ECS033
00407      CLOSE CLM-REC  ACCT-MSTR.                                    ECS033
00408                                                                   ECS033
00409  0799-EXIT.                                                       ECS033
00410      EXIT.                                                        ECS033
00411  EJECT                                                            ECS033
00412  0800-WRITE-EXTRACTS  SECTION.                                    ECS033
00413                                                                   ECS033
00414  0810-RETURN-EXTRACT.                                             ECS033
00415      RETURN SORT-FILE  AT END                                     ECS033
00416          CLOSE CLM-XTR                                            ECS033
00417          GO TO 0899-EXIT.                                         ECS033
00418                                                                   ECS033
00419      WRITE CLMS-XTRCT  FROM  SORT-REC.                            ECS033
00420                                                                   ECS033
00421      GO TO 0810-RETURN-EXTRACT.                                   ECS033
00422                                                                   ECS033
00423  0899-EXIT.                                                       ECS033
00424      EXIT.                                                        ECS033
00425                                                                   ECS033
00426  EJECT                                                            ECS033
00427  1000-END-JOB  SECTION.                                           ECS033
00428                                                                   ECS033
00429  1100-PRT-RTN.                                                    ECS033
00430                              COPY ELCPRT2.                        ECS033
00431  1199-EXIT.                                                       ECS033
00432      EXIT.                                                        ECS033
00433                                                                   ECS033
00434  1200-PRINT-HEADINGS.                                             ECS033
00435      MOVE '1'                    TO  X.                           ECS033
00436      MOVE HEAD-1                 TO  P-DATA.                      ECS033
00437                                                                   ECS033
00438      PERFORM 1100-PRT-RTN  THRU  1199-EXIT.                       ECS033
00439                                                                   ECS033
00440      MOVE ' '                    TO  X.                           ECS033
00441      MOVE WS-CURRENT-DATE        TO  HD-DATE.                     ECS033
00442      MOVE COMPANY-NAME           TO  HD-CLIENT.                   ECS033
00443      MOVE HEAD-2                 TO  P-DATA.                      ECS033
00444                                                                   ECS033
00445      PERFORM 1100-PRT-RTN  THRU  1199-EXIT.                       ECS033
00446                                                                   ECS033
00447      MOVE 1                      TO  HD-PAGE.                     ECS033
00448      MOVE ALPH-DATE              TO  HD-ALF-DTE.                  ECS033
00449      MOVE HEAD-3                 TO  P-DATA.                      ECS033
00450                                                                   ECS033
00451      PERFORM 1100-PRT-RTN  THRU  1199-EXIT.                       ECS033
00452                                                                   ECS033
00453  1299-EXIT.                                                       ECS033
00454      EXIT.                                                        ECS033
00455  EJECT                                                            ECS033
00456  1300-E-O-J.                                                      ECS033
00457      OPEN OUTPUT PRNTR.                                           ECS033
00458                                                                   ECS033
00459      PERFORM 1200-PRINT-HEADINGS  THRU  1299-EXIT.                ECS033
00460                                                                   ECS033
00461      MOVE '-'                    TO  X.                           ECS033
00462      IF DTE-PGM-OPT EQUAL '2'                                     ECS033
00463         MOVE 'REINSURANCE CLAIM EXTRACT COMPLETED'                ECS033
00464                                  TO  P-DATA                       ECS033
00465      ELSE                                                         ECS033
00466         MOVE 'CLAIMS EXTRACT CREATE COMPLETED'                    ECS033
00467                                  TO  P-DATA.                      ECS033
00468      PERFORM 1100-PRT-RTN  THRU  1199-EXIT.                       ECS033
00469                                                                   ECS033
00470  1310-CLOSE-FICH.                                                 ECS033
00471                              COPY ELCPRTC.                        ECS033
00472                                                                   ECS033
00473  1320-CLOSE-PRINTER.                                              ECS033
00474      CLOSE PRNTR.                                                 ECS033
00475                                                                   ECS033
00476      GOBACK.                                                      ECS033
00477                                                                   ECS033
00478  DATE-CARD-ROUTINE SECTION.                                          CL**3
00479  COPY ELCDCS.                                                        CL**3
00480                                                                      CL**3
00481  ABEND-PGM  SECTION.                                              ECS033
00482                             COPY ELCABEND.                        ECS033
00483                                                                      CL**3
