00001  IDENTIFICATION DIVISION.                                         05/06/98
00002                                                                   ECS077
00003  PROGRAM-ID.                ECS077.                                  LV006
00004 *              PROGRAM CONVERTED BY                               ECS077
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ECS077
00006 *              CONVERSION DATE 02/08/96 18:32:15.                 ECS077
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          ECS077
00008 *                           VMOD=2.007.                           ECS077
00009 *AUTHOR.        LOGIC, INC.                                       ECS077
00010 *               DALLAS, TEXAS.                                    ECS077
00011                                                                   ECS077
00012 *DATE-COMPILED.                                                   ECS077
00013                                                                   ECS077
00014 *SECURITY.   *****************************************************ECS077
00015 *            *                                                   *ECS077
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ECS077
00017 *            *                                                   *ECS077
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS077
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ECS077
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *ECS077
00021 *            *                                                   *ECS077
00022 *            *****************************************************ECS077
00023                                                                   ECS077
00024 *REMARKS.                                                         ECS077
00025 *        THIS PROGRAM PRINTS THE ACCOUNT MASTER.                  ECS077
00026                                                                   ECS077
00027  ENVIRONMENT DIVISION.                                            ECS077
00028  INPUT-OUTPUT SECTION.                                            ECS077
00029  FILE-CONTROL.                                                    ECS077
00030                                                                   ECS077
00031      SELECT ACCT-PRT         ASSIGN TO SYS008.
pemuni*           organization is line sequential.
00032      SELECT ACCT-SEQUENTIAL  ASSIGN TO SYS010-UT-2400-S-SYS010.   ECS077
00033      SELECT ACCT-MAS         ASSIGN TO SYS021-FBA1-ERACCT         ECS077
00034                                ORGANIZATION IS INDEXED            ECS077
00035                                ACCESS IS SEQUENTIAL               ECS077
00036                                RECORD KEY IS ACCT-MASTER-KEY      ECS077
00037                                FILE STATUS IS ERACCT-FILE-STATUS. ECS077
00038                                                                   ECS077
00039      SELECT ELCNTL           ASSIGN TO SYS009-FBA1-ELCNTL         ECS077
00040                              ACCESS       DYNAMIC                 ECS077
00041                              ORGANIZATION INDEXED                 ECS077
00042                              FILE STATUS  ELCNTL-FILE-STATUS      ECS077
00043                              RECORD KEY   CF-CONTROL-PRIMARY.     ECS077
00044                                                                   ECS077
00045      SELECT DISK-DATE        ASSIGN TO SYS019-UT-FBA1-S-SYS019.   ECS077
00046      SELECT FICH             ASSIGN TO SYS020-UT-2400-S-SYS020.   ECS077
00047  EJECT                                                            ECS077
00048  DATA DIVISION.                                                   ECS077
00049  FILE SECTION.                                                    ECS077
00050                                                                   ECS077
00051  FD  ACCT-PRT                                                     ECS077
00052                                  COPY ELCPRTFD.                   ECS077
00053  EJECT                                                            ECS077
00054  FD  ACCT-SEQUENTIAL                                              ECS077
00055      BLOCK CONTAINS 0 RECORDS
00056      RECORDING MODE F.                                            ECS077
00057                                                                   ECS077
00058  01  ACCT-RECORD-IN              PIC X(2000).                     ECS077
00059                                                                   ECS077
00060  FD  ACCT-MAS.                                                    ECS077
00061                                                                   ECS077
00062  01  ACC-MSTR-REC.                                                ECS077
00063      12  FILLER                  PIC XX.                          ECS077
00064      12  ACCT-MASTER-KEY         PIC X(26).                       ECS077
00065      12  FILLER                  PIC X(1972).                     ECS077
00066  EJECT                                                            ECS077
00067  FD  ELCNTL.                                                      ECS077
00068                                  COPY ELCCNTL.                    ECS077
00069                                                                   ECS077
00070  FD  DISK-DATE                                                    ECS077
00071                                  COPY ELCDTEFD.                   ECS077
00072  EJECT                                                            ECS077
00073  FD  FICH                                                         ECS077
00074                                  COPY ELCFCHFD.                   ECS077
00075  EJECT                                                            ECS077
00076  WORKING-STORAGE SECTION.                                         ECS077
00077  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      ECS077
00078  77  FILLER  PIC X(32) VALUE '********************************'.  ECS077
00079  77  FILLER  PIC X(32) VALUE '     ECS077 WORKING-STORAGE     '.  ECS077
00080  77  FILLER  PIC X(32) VALUE '*****V/M=2.007******************'.  ECS077
00081                                                                   ECS077
00082  77  CTR                     PIC S99    COMP     VALUE +0.        ECS077
00083  77  PGM-SUB                 PIC S999   COMP-3   VALUE +77.       ECS077
00084  77  SETS                    PIC S999   COMP-3   VALUE +80.       ECS077
00085  77  PAGER                   PIC S9(5)  COMP-3   VALUE +1.        ECS077
00086  77  X                       PIC X.                               ECS077
00087  77  SAV-CARRIER             PIC X               VALUE LOW-VALUE. ECS077
00088      EJECT                                                        ECS077
00089      COPY ERCACCT.                                                ECS077
00090                                                                   ECS077
00091      COPY ELCACCTV.                                               ECS077
00092      EJECT                                                        ECS077
00093  01  FILE-STATUS.                                                 ECS077
00094      12  ERACCT-FILE-STATUS      PIC XX          VALUE '00'.      ECS077
00095      12  ELCNTL-FILE-STATUS      PIC XX          VALUE '00'.      ECS077
00096                                                                   ECS077
00097  01  WS-EXP-DT                   PIC 9(11).                          CL**6
00098  01  WS-EXP-DT-N  REDEFINES  WS-EXP-DT.                              CL**5
00099      12  FILLER                  PIC 999.                         ECS077
00100      12  WS-EXP-DT-FILL          PIC 9(8).                        ECS077
00101                                                                   ECS077
00102  01  WS-LAST-MAINT-DT.                                            ECS077
00103      12  WS-L-MO                 PIC XX.                          ECS077
00104      12  WS-L-DA                 PIC XX.                          ECS077
00105      12  WS-L-YR                 PIC XX.                          ECS077
00106                                                                   ECS077
00107  01  WS.                                                          ECS077
00108      12  WS-CUSTOMIZATION-FLAG   PIC X           VALUE SPACE.     ECS077
00109          88  CUSTOMIZATION-FOUND                 VALUE SPACE.     ECS077
00110      12  WS-RETURN-CODE          PIC S9(4)       VALUE ZERO.      ECS077
00111      12  WS-ZERO                 PIC S9          VALUE ZERO.      ECS077
00112      12  WS-ABEND-MESSAGE        PIC X(80)       VALUE SPACES.    ECS077
00113      12  WS-ABEND-FILE-STATUS    PIC XX          VALUE ZERO.      ECS077
00114  EJECT                                                            ECS077
00115  01  HEAD-A.                                                      ECS077
00116      12  FILLER              PIC X(48)           VALUE SPACES.    ECS077
00117      12  FILLER              PIC X(27)           VALUE            ECS077
00118              'ACCOUNT MASTER FILE SUMMARY'.                       ECS077
00119      12  FILLER              PIC X(45)           VALUE SPACES.    ECS077
00120      12  FILLER              PIC X(8)            VALUE 'ECS077'.  ECS077
00121                                                                   ECS077
00122  01  HEAD-B.                                                      ECS077
00123      12  FILLER              PIC X(47)           VALUE SPACES.    ECS077
00124      12  HA-NAME             PIC X(30).                           ECS077
00125      12  FILLER              PIC X(43)           VALUE SPACES.    ECS077
00126      12  HB-IPL              PIC X(8).                            ECS077
00127                                                                   ECS077
00128  01  HEAD-C.                                                      ECS077
00129      12  FILLER              PIC X(53)           VALUE SPACES.    ECS077
00130      12  HC-DATE             PIC X(18).                           ECS077
00131      12  FILLER              PIC X(49)           VALUE SPACES.    ECS077
00132      12  FILLER              PIC X(5)            VALUE 'PAGE'.    ECS077
00133      12  HC-PAGE             PIC ZZ,ZZ9.                          ECS077
00134                                                                   ECS077
00135  01  HEAD-D.                                                      ECS077
00136      12  FILLER              PIC X               VALUE SPACES.    ECS077
00137      12  FILLER              PIC X(44)           VALUE            ECS077
00138              'CAR GROUP ST  ACCOUNT     EFFECTIVE DATES   '.      ECS077
00139      12  FILLER              PIC X(44)           VALUE            ECS077
00140              'CERTIFICATE DATES BUS P I REI RM  * RATING *'.      ECS077
00141      12  FILLER              PIC X(44)           VALUE            ECS077
00142              '  *------ COMMISSION LEVELS ---------* LAST '.      ECS077
00143                                                                   ECS077
00144  01  HEAD-E.                                                      ECS077
00145      12  FILLER              PIC X               VALUE SPACES.    ECS077
00146      12  FILLER              PIC X(44)           VALUE            ECS077
00147              '                           FROM      TO     '.      ECS077
00148      12  FILLER              PIC X(37)           VALUE            ECS077
00149              '  HIGH     LOW    TYP E G TAB CD  CL '.             ECS077
00150      12  HEAD-E-LF-OVRD      PIC XX.                              ECS077
00151      12  FILLER              PIC XX              VALUE            ECS077
00152              '- '.                                                ECS077
00153      12  HEAD-E-AH-OVRD-1    PIC XX.                              ECS077
00154      12  FILLER              PIC X(33)           VALUE            ECS077
00155              '-  LV    AGENT     SINGLE  JOINT '.                 ECS077
00156      12  HEAD-E-AH-OVRD-2    PIC X(6).                            ECS077
00157      12  FILLER              PIC X(6)            VALUE            ECS077
00158              ' MAINT'.                                            ECS077
00159  01  HEAD-F.                                                      ECS077
00160      12  FILLER              PIC X               VALUE SPACES.    ECS077
00161      12  FILLER              PIC X(18)           VALUE            ECS077
00162              '      ACCOUNT NAME'.                                ECS077
00163      12  FILLER              PIC X(63)           VALUE SPACES.    ECS077
00164      12  FILLER              PIC X(7)            VALUE            ECS077
00165              'DEV DEV'.                                           ECS077
00166      12  FILLER              PIC X(44)           VALUE SPACES.    ECS077
00167                                                                   ECS077
00168  01  DATA-1.                                                      ECS077
00169      12  FILLER              PIC XX              VALUE SPACES.    ECS077
00170      12  D-CARR              PIC X.                               ECS077
00171      12  FILLER              PIC X               VALUE SPACE.     ECS077
00172      12  D-GROUP             PIC X(6).                            ECS077
00173      12  FILLER              PIC X               VALUE SPACE.     ECS077
00174      12  D-ST                PIC XX.                              ECS077
00175      12  FILLER              PIC X               VALUE SPACE.     ECS077
00176      12  D-ACCT              PIC X(10).                           ECS077
00177      12  FILLER              PIC XX              VALUE SPACES.    ECS077
00178      12  D-EFF.                                                   ECS077
00179          16 D-F-MO           PIC XX.                              ECS077
00180          16 D-F-D1           PIC X               VALUE '/'.       ECS077
00181          16 D-F-DA           PIC XX.                              ECS077
00182          16 D-F-D2           PIC X               VALUE '/'.       ECS077
00183          16 D-F-YR           PIC XX.                              ECS077
00184      12  FILLER              PIC X               VALUE SPACES.    ECS077
00185      12  D-EXP.                                                   ECS077
00186          16  D-T-MO          PIC XX.                              ECS077
00187          16  D-D-1           PIC X               VALUE '/'.       ECS077
00188          16  D-T-DA          PIC XX.                              ECS077
00189          16  D-D-2           PIC X               VALUE '/'.       ECS077
00190          16  D-T-YR          PIC XX.                              ECS077
00191      12  FILLER              PIC XX              VALUE SPACES.    ECS077
00192      12  D-HIGHS.                                                 ECS077
00193          16  D-H-MO          PIC XX.                              ECS077
00194          16  D-H-D1          PIC X               VALUE '/'.       ECS077
00195          16  D-H-DA          PIC XX.                              ECS077
00196          16  D-H-D2          PIC X               VALUE '/'.       ECS077
00197          16  D-H-YR          PIC XX.                              ECS077
00198      12  FILLER              PIC X               VALUE SPACES.    ECS077
00199      12  D-LOWS.                                                  ECS077
00200          16  D-L-MO          PIC XX.                              ECS077
00201          16  D-L-D1          PIC X               VALUE '/'.       ECS077
00202          16  D-L-DA          PIC XX.                              ECS077
00203          16  D-L-D2          PIC X               VALUE '/'.       ECS077
00204          16  D-L-YR          PIC XX.                              ECS077
00205      12  FILLER              PIC XX              VALUE SPACES.    ECS077
00206      12  D-BUS               PIC XX.                              ECS077
00207      12  FILLER              PIC X               VALUE SPACES.    ECS077
00208      12  D-PE                PIC X.                               ECS077
00209      12  FILLER              PIC X               VALUE SPACES.    ECS077
00210      12  D-IG                PIC X.                               ECS077
00211      12  FILLER              PIC X               VALUE SPACES.    ECS077
00212      12  D-REI               PIC XXX.                             ECS077
00213      12  FILLER              PIC X               VALUE SPACES.    ECS077
00214      12  D-REM               PIC XX.                              ECS077
00215      12  FILLER              PIC XX              VALUE SPACES.    ECS077
00216      12  D-CLS               PIC XX.                              ECS077
00217      12  FILLER              PIC X               VALUE SPACES.    ECS077
00218      12  D-LF-DEV            PIC XXX.                             ECS077
00219      12  FILLER              PIC X               VALUE SPACES.    ECS077
00220      12  D-AH-DEV            PIC XXX.                             ECS077
00221      12  FILLER              PIC XX              VALUE SPACES.    ECS077
00222      12  D-COMM              PIC X(42).                           ECS077
00223                                                                   ECS077
00224  01  DATA-2.                                                      ECS077
00225      12  FILLER              PIC X(7)            VALUE SPACES.    ECS077
00226      12  D2-ACCT-NAME        PIC X(30)           VALUE SPACES.    ECS077
00227      12  FILLER              PIC X(54)           VALUE SPACES.    ECS077
00228      12  D2-COMM             PIC X(42).                           ECS077
00229                                                                   ECS077
00230  01  COMM-BLD.                                                    ECS077
00231      12  CB-NO               PIC Z9.                              ECS077
00232      12  FILLER              PIC X               VALUE SPACE.     ECS077
00233      12  CB-AGT              PIC X(10).                           ECS077
00234      12  CB-DASH             PIC X               VALUE '-'.       ECS077
00235      12  CB-COM              PIC X.                               ECS077
00236      12  FILLER              PIC X               VALUE SPACES.    ECS077
00237      12  CB-SGL              PIC ZZ.999-.                         ECS077
00238      12  CBR-SGL REDEFINES                                        ECS077
00239          CB-SGL              PIC X(7).                            ECS077
00240      12  CB-JNT              PIC ZZ.999-.                         ECS077
00241      12  CBR-JNT REDEFINES                                        ECS077
00242          CB-JNT              PIC X(7).                            ECS077
00243      12  CB-AH               PIC ZZ.999-.                         ECS077
00244      12  CBR-AH REDEFINES                                         ECS077
00245          CB-AH               PIC X(7).                            ECS077
00246      12  CB-MAINT.                                                ECS077
00247          16  CB-M-MO         PIC XX.                              ECS077
00248          16  CB-M-D1         PIC X               VALUE '/'.       ECS077
00249          16  CB-M-YR         PIC XX.                              ECS077
00250  EJECT                                                            ECS077
00251  COPY ELCDTECX.                                                   ECS077
00252                                                                   ECS077
00253  COPY ELCDTEVR.                                                   ECS077
00254                                                                   ECS077
00255  EJECT                                                            ECS077
00256  COPY ELCDATE.                                                       CL**3
00257  EJECT                                                            ECS077
00258  PROCEDURE DIVISION.                                              ECS077
00259                                                                   ECS077
00260  0000-OPEN-INIT.                                                  ECS077
00261                              COPY ELCDTERX.                       ECS077
00262                                                                   ECS077
00263      MOVE COMPANY-NAME           TO HA-NAME.                      ECS077
00264      MOVE WS-CURRENT-DATE        TO HB-IPL.                       ECS077
00265      MOVE ALPH-DATE              TO HC-DATE.                      ECS077
00266                                                                   ECS077
00267      OPEN OUTPUT ACCT-PRT.                                        ECS077
00268                                                                   ECS077
00269      IF DTE-PGM-OPT = '2'                                         ECS077
00270         OPEN INPUT ACCT-SEQUENTIAL                                ECS077
00271         GO TO 1000-READ-LOOP.                                     ECS077
00272                                                                   ECS077
00273      OPEN INPUT  ACCT-MAS.                                        ECS077
00274                                                                   ECS077
00275      IF ERACCT-FILE-STATUS  = '00' OR '97'                        ECS077
00276          NEXT SENTENCE                                            ECS077
00277        ELSE                                                       ECS077
00278          MOVE 'ERROR OCCURED OPEN - ERACCTT'  TO                  ECS077
00279                             WS-ABEND-MESSAGE                      ECS077
00280          MOVE ERACCT-FILE-STATUS  TO  WS-ABEND-FILE-STATUS        ECS077
00281          GO TO ABEND-PGM.                                         ECS077
00282                                                                   ECS077
00283      MOVE LOW-VALUES             TO AM-CONTROL-PRIMARY.           ECS077
00284      MOVE DTE-CLASIC-COMPANY-CD  TO AM-COMPANY-CD.                ECS077
00285                                                                   ECS077
00286      MOVE AM-CONTROL-PRIMARY TO ACCT-MASTER-KEY.                  ECS077
00287                                                                   ECS077
00288      START ACCT-MAS                                               ECS077
00289          KEY IS GREATER THAN ACCT-MASTER-KEY                      ECS077
00290          INVALID KEY                                              ECS077
00291              MOVE HIGH-VALUES    TO AM-MSTR-CNTRL                 ECS077
00292              GO TO 9999-END-JOB.                                  ECS077
00293                                                                   ECS077
00294      IF ERACCT-FILE-STATUS NOT = ZERO                             ECS077
00295          MOVE 'ERROR OCCURED START - ERACCTT'  TO                 ECS077
00296                             WS-ABEND-MESSAGE                      ECS077
00297          MOVE ERACCT-FILE-STATUS  TO  WS-ABEND-FILE-STATUS        ECS077
00298          GO TO ABEND-PGM.                                         ECS077
00299                                                                   ECS077
00300  0050-OPEN-CONTROL-FILE.                                          ECS077
00301      OPEN INPUT ELCNTL.                                           ECS077
00302                                                                   ECS077
00303      IF ELCNTL-FILE-STATUS  = '00' OR '97'                        ECS077
00304          NEXT SENTENCE                                            ECS077
00305        ELSE                                                       ECS077
00306          MOVE '**** ELCNTL OPEN ERROR ****'                       ECS077
00307                                  TO WS-ABEND-MESSAGE              ECS077
00308          MOVE ELCNTL-FILE-STATUS TO WS-ABEND-FILE-STATUS          ECS077
00309          GO TO ABEND-PGM.                                         ECS077
00310                                                                   ECS077
00311  0055-READ-REPORT-RECORD.                                         ECS077
00312      MOVE SPACES                 TO CF-CONTROL-PRIMARY.           ECS077
00313      MOVE DTE-CLIENT             TO CF-COMPANY-ID.                ECS077
00314      MOVE 'C'                    TO CF-RECORD-TYPE.               ECS077
00315      MOVE PGM-SUB                TO CF-CUSTOM-REPORT-NO.          ECS077
00316      MOVE +0                     TO CF-SEQUENCE-NO.               ECS077
00317                                                                   ECS077
00318      READ ELCNTL.                                                 ECS077
00319                                                                   ECS077
00320      IF ELCNTL-FILE-STATUS NOT = '00'                             ECS077
00321          MOVE 'X'                TO WS-CUSTOMIZATION-FLAG.        ECS077
00322                                                                   ECS077
00323      EJECT                                                        ECS077
00324  1000-READ-LOOP.                                                  ECS077
00325      IF DTE-PGM-OPT NOT = '2'                                     ECS077
00326         GO TO 1010-READ-VSAM-INPUT.                               ECS077
00327                                                                   ECS077
00328      READ ACCT-SEQUENTIAL INTO ACCOUNT-MASTER AT END              ECS077
00329         GO TO 9999-END-JOB.                                       ECS077
00330                                                                   ECS077
00331      COPY ELCACCTI.                                               ECS077
00332                                                                   ECS077
00333      GO TO 1050-BUILD-ACCT-PRINT.                                 ECS077
00334                                                                   ECS077
00335  1010-READ-VSAM-INPUT.                                            ECS077
00336      READ ACCT-MAS.                                               ECS077
00337                                                                   ECS077
00338      IF ERACCT-FILE-STATUS = '10'                                 ECS077
00339          GO TO 9999-END-JOB.                                      ECS077
00340                                                                   ECS077
00341      MOVE ACC-MSTR-REC TO ACCOUNT-MASTER.                         ECS077
00342                                                                   ECS077
00343      COPY ELCACCTI.                                               ECS077
00344                                                                   ECS077
00345      IF AM-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD                 ECS077
00346          GO TO 9999-END-JOB.                                      ECS077
00347                                                                   ECS077
00348      IF ERACCT-FILE-STATUS NOT = ZERO                             ECS077
00349          MOVE 'ERROR OCCURED READ - ERACCTT'  TO                  ECS077
00350                               WS-ABEND-MESSAGE                    ECS077
00351          MOVE ERACCT-FILE-STATUS  TO  WS-ABEND-FILE-STATUS        ECS077
00352          GO TO ABEND-PGM.                                         ECS077
00353                                                                   ECS077
00354  1050-BUILD-ACCT-PRINT.                                           ECS077
00355 ***********   CHECK IF ACTIVE ACCOUNTS ONLY TO BE SELECTED        ECS077
00356      IF CUSTOMIZATION-FOUND                                       ECS077
00357          IF CF-ACTIVE-ACCOUNTS                                    ECS077
00358              IF NOT AM-ACCOUNT-ACTIVE                             ECS077
00359                  GO TO 1000-READ-LOOP.                            ECS077
00360 ***********                                                       ECS077
00361                                                                   ECS077
00362      ADD +1                      TO SETS.                         ECS077
00363      MOVE AM-CARRIER             TO D-CARR.                       ECS077
00364      MOVE AM-GROUPING            TO D-GROUP.                      ECS077
00365      MOVE AM-STATE               TO D-ST.                         ECS077
00366      MOVE AM-ACCOUNT             TO D-ACCT.                       ECS077
00367                                                                   ECS077
00368      IF AM-EFFECT-DT = ZEROS                                      ECS077
00369          MOVE AM-EFFECTIVE-DT        TO DC-BIN-DATE-1             ECS077
00370          MOVE SPACE                  TO DC-OPTION-CODE            ECS077
00371          PERFORM 8500-DATE-CONVERSION THRU 8590-EXIT              ECS077
00372          MOVE DC-GREG-DATE-1-EDIT    TO D-EFF                     ECS077
00373      ELSE                                                         ECS077
00374          MOVE '/'                    TO D-F-D1, D-F-D2            ECS077
00375          MOVE AM-EFF-YR              TO D-F-YR                    ECS077
00376          MOVE AM-EFF-MO              TO D-F-MO                    ECS077
00377          MOVE AM-EFF-DA              TO D-F-DA.                   ECS077
00378                                                                   ECS077
00379      MOVE AM-EXPIRE-DT           TO WS-EXP-DT.                    ECS077
00380                                                                   ECS077
00381      IF (AM-EXPIRE-DT = 99999999999  OR  00099999999)  OR            CL**2
00382          AM-EXPIRATION-DT = HIGH-VALUES                              CL**2
00383          MOVE ' CURRENT'         TO D-EXP                         ECS077
00384      ELSE                                                         ECS077
00385          IF WS-EXP-DT-FILL NOT NUMERIC                            ECS077
00386              MOVE AM-EXPIRATION-DT   TO DC-BIN-DATE-1             ECS077
00387              MOVE SPACE              TO DC-OPTION-CODE            ECS077
00388              PERFORM 8500-DATE-CONVERSION THRU 8590-EXIT          ECS077
00389              MOVE DC-GREG-DATE-1-EDIT TO D-EXP                    ECS077
00390          ELSE                                                     ECS077
00391              MOVE '/'                TO D-D-1  D-D-2              ECS077
00392              MOVE AM-EXP-YR          TO D-T-YR                    ECS077
00393              MOVE AM-EXP-MO          TO D-T-MO                    ECS077
00394              MOVE AM-EXP-DA          TO D-T-DA.                   ECS077
00395                                                                   ECS077
00396      MOVE AM-HI-YR               TO D-H-YR.                       ECS077
00397      MOVE AM-HI-MO               TO D-H-MO.                       ECS077
00398      MOVE AM-HI-DA               TO D-H-DA.                       ECS077
00399                                                                   ECS077
00400      IF AM-HI-CERT-DATE = ZEROS                                   ECS077
00401          MOVE '  NONE'           TO D-HIGHS                       ECS077
00402      ELSE                                                         ECS077
00403          MOVE '/'                TO D-H-D1  D-H-D2.               ECS077
00404                                                                   ECS077
00405      MOVE AM-LO-YR               TO D-L-YR.                       ECS077
00406      MOVE AM-LO-MO               TO D-L-MO.                       ECS077
00407      MOVE AM-LO-DA               TO D-L-DA.                       ECS077
00408                                                                   ECS077
00409      IF AM-LO-CERT-DATE = ZEROS                                   ECS077
00410          MOVE '  NONE'           TO D-LOWS                        ECS077
00411      ELSE                                                         ECS077
00412          MOVE '/'                TO D-L-D1 D-L-D2.                ECS077
00413                                                                   ECS077
00414      MOVE AM-GPCD                TO D-BUS.                        ECS077
00415      MOVE AM-RET-P-E             TO D-PE.                         ECS077
00416      MOVE AM-IG                  TO D-IG.                         ECS077
00417      MOVE AM-REI-TABLE           TO D-REI.                        ECS077
00418      INSPECT D-IG CONVERTING '12' TO 'IG'.                        ECS077
00419      MOVE AM-REMIT-TO            TO D-REM.                        ECS077
00420      MOVE AM-CAL-TABLE           TO D-CLS.                        ECS077
00421      MOVE AM-LF-DEVIATION        TO D-LF-DEV.                     ECS077
00422      MOVE AM-AH-DEVIATION        TO D-AH-DEV.                     ECS077
00423      MOVE +1                     TO CTR.                          ECS077
00424      PERFORM 2000-COMM-BUILD THRU 2200-EXIT.                      ECS077
00425      MOVE COMM-BLD               TO D-COMM.                       ECS077
00426                                                                   ECS077
00427      IF SETS GREATER +58                                          ECS077
00428          MOVE PAGER              TO HC-PAGE                       ECS077
00429          MOVE HEAD-A             TO PRT                           ECS077
00430          MOVE '1'                TO X                             ECS077
00431          PERFORM 9000-PRINT THRU 9000-EXIT                        ECS077
00432          MOVE HEAD-B             TO PRT                           ECS077
00433          MOVE ' '                TO X                             ECS077
00434          PERFORM 9000-PRINT THRU 9000-EXIT                        ECS077
00435          MOVE HEAD-C             TO PRT                           ECS077
00436          MOVE ' '                TO X                             ECS077
00437          PERFORM 9000-PRINT THRU 9000-EXIT                        ECS077
00438          MOVE HEAD-D             TO PRT                           ECS077
00439          MOVE '0'                TO X                             ECS077
00440          PERFORM 9000-PRINT THRU 9000-EXIT                        ECS077
00441          MOVE LIFE-OVERRIDE-L2   TO HEAD-E-LF-OVRD                ECS077
00442          MOVE AH-OVERRIDE-L2     TO HEAD-E-AH-OVRD-1              ECS077
00443          MOVE AH-OVERRIDE-L6     TO HEAD-E-AH-OVRD-2              ECS077
00444          MOVE HEAD-E             TO PRT                           ECS077
00445          MOVE ' '                TO X                             ECS077
00446          PERFORM 9000-PRINT THRU 9000-EXIT                        ECS077
00447          MOVE HEAD-F             TO PRT                           ECS077
00448          MOVE ' '                TO X                             ECS077
00449          PERFORM 9000-PRINT THRU 9000-EXIT                        ECS077
00450          MOVE SPACES             TO PRT                           ECS077
00451          MOVE ' '                TO X                             ECS077
00452          PERFORM 9000-PRINT THRU 9000-EXIT                        ECS077
00453          MOVE +11                TO SETS                          ECS077
00454          ADD +1                  TO PAGER.                        ECS077
00455                                                                   ECS077
00456      MOVE AM-CARRIER             TO SAV-CARRIER.                  ECS077
00457      MOVE DATA-1                 TO PRT.                          ECS077
00458      MOVE ' '                    TO X.                            ECS077
00459      PERFORM 9000-PRINT THRU 9000-EXIT.                           ECS077
00460      PERFORM 2000-COMM-BUILD THRU 2200-EXIT 9 TIMES.              ECS077
00461      MOVE SPACES                 TO PRT.                          ECS077
00462      ADD +1                      TO SETS.                         ECS077
00463      PERFORM 9000-PRINT THRU 9000-EXIT.                           ECS077
00464      GO TO 1000-READ-LOOP.                                        ECS077
00465                                                                   ECS077
00466  2000-COMM-BUILD.                                                 ECS077
00467      IF AM-AGT (CTR) = ZEROS AND                                  ECS077
00468         CTR = +2                                                  ECS077
00469          MOVE SPACES             TO COMM-BLD                      ECS077
00470          GO TO 2100-FIRST-LN-XIT.                                 ECS077
00471                                                                   ECS077
00472      IF AM-AGT (CTR) = ZEROS AND                                  ECS077
00473         CTR NOT = +1                                              ECS077
00474          ADD +1                  TO CTR                           ECS077
00475          GO TO 2200-EXIT.                                         ECS077
00476                                                                   ECS077
00477      MOVE CTR                    TO CB-NO.                        ECS077
00478      MOVE AM-AGT (CTR)           TO CB-AGT.                       ECS077
00479      MOVE '-'                    TO CB-DASH.                      ECS077
00480      MOVE AM-COM-TYP (CTR)       TO CB-COM.                       ECS077
00481                                                                   ECS077
00482      IF AM-L-COM (CTR) NUMERIC                                    ECS077
00483          MULTIPLY AM-L-COM (CTR) BY +100 GIVING CB-SGL            ECS077
00484      ELSE                                                         ECS077
00485          MOVE AM-L-COMA (CTR)    TO CBR-SGL.                      ECS077
00486                                                                   ECS077
00487      IF AM-J-COM (CTR) NUMERIC                                    ECS077
00488          MULTIPLY AM-J-COM (CTR) BY +100 GIVING CB-JNT            ECS077
00489      ELSE                                                         ECS077
00490          MOVE AM-J-COMA (CTR)    TO CBR-JNT.                      ECS077
00491                                                                   ECS077
00492      IF AM-A-COM (CTR) NUMERIC                                    ECS077
00493          MULTIPLY AM-A-COM (CTR) BY +100 GIVING CB-AH             ECS077
00494      ELSE                                                         ECS077
00495          MOVE AM-A-COMA (CTR)    TO CBR-AH.                       ECS077
00496                                                                   ECS077
00497      IF CTR NOT = +1                                              ECS077
00498          GO TO 2100-FIRST-LN-XIT.                                 ECS077
00499                                                                   ECS077
00500      MOVE AM-NAME                TO D2-ACCT-NAME.                 ECS077
00501                                                                   ECS077
00502      MOVE AM-LAST-MAINT-DT       TO DC-BIN-DATE-1.                ECS077
00503      MOVE SPACE                  TO DC-OPTION-CODE.               ECS077
00504      PERFORM 8500-DATE-CONVERSION THRU 8590-EXIT.                 ECS077
00505      MOVE DC-GREG-DATE-1-MDY     TO WS-LAST-MAINT-DT.             ECS077
00506                                                                   ECS077
00507      MOVE WS-L-MO                TO CB-M-MO.                      ECS077
00508      MOVE '/'                    TO CB-M-D1.                      ECS077
00509      MOVE WS-L-YR                TO CB-M-YR.                      ECS077
00510                                                                   ECS077
00511      IF AM-LAST-MAINT-DT = LOW-VALUES                             ECS077
00512          MOVE ' NONE'            TO CB-MAINT.                     ECS077
00513                                                                   ECS077
00514      ADD +1                      TO CTR.                          ECS077
00515      GO TO 2200-EXIT.                                             ECS077
00516                                                                   ECS077
00517  2100-FIRST-LN-XIT.                                               ECS077
00518      MOVE SPACES                 TO CB-MAINT.                     ECS077
00519      MOVE COMM-BLD               TO D2-COMM.                      ECS077
00520      MOVE DATA-2                 TO PRT.                          ECS077
00521      MOVE ' '                    TO X.                            ECS077
00522      PERFORM 9000-PRINT THRU 9000-EXIT.                           ECS077
00523                                                                   ECS077
00524      ADD +1                      TO SETS                          ECS077
00525                                     CTR.                          ECS077
00526      MOVE SPACES                 TO D2-ACCT-NAME.                 ECS077
00527                                                                   ECS077
00528  2200-EXIT.                                                       ECS077
00529      EXIT.                                                        ECS077
00530      EJECT                                                        ECS077
00531  8500-DATE-CONVERSION.                                            ECS077
00532                                  COPY ELCDCS.                     ECS077
00533      EJECT                                                        ECS077
00534  9000-PRINT.                                                      ECS077
00535                                  COPY ELCPRT2.                    ECS077
00536                                                                   ECS077
00537  9000-EXIT.                                                       ECS077
00538      EXIT.                                                        ECS077
00539                                                                   ECS077
00540  9999-END-JOB.                                                    ECS077
00541                                  COPY ELCPRTC.                    ECS077
00542                                                                   ECS077
00543      CLOSE ACCT-PRT.                                              ECS077
00544                                                                   ECS077
00545      IF DTE-PGM-OPT = '2'                                         ECS077
00546         CLOSE ACCT-SEQUENTIAL                                     ECS077
00547      ELSE                                                         ECS077
00548         CLOSE ACCT-MAS                                            ECS077
00549         IF ERACCT-FILE-STATUS NOT = ZERO                          ECS077
00550            MOVE 'ERROR OCCURED CLOSE - ERACCTT' TO                ECS077
00551                               WS-ABEND-MESSAGE                    ECS077
00552            MOVE ERACCT-FILE-STATUS  TO  WS-ABEND-FILE-STATUS      ECS077
00553            GO TO ABEND-PGM.                                       ECS077
00554                                                                   ECS077
00555      GOBACK.                                                      ECS077
00556                                                                   ECS077
00557  ABEND-PGM SECTION.              COPY ELCABEND SUPPRESS.          ECS077
