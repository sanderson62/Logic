00001  IDENTIFICATION DIVISION.                                         09/09/98
00002                                                                   ECS265
00003  PROGRAM-ID.                 ECS265.                                 LV020
00004 *           PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE             ECS265
00005 *                            VMOD=2.005                              CL**2
00006 *                                                                 ECS265
00007 *AUTHOR.     LOGIC, INC.                                          ECS265
00008 *            DALLAS, TEXAS.                                       ECS265
00009                                                                   ECS265
00010 *DATE-COMPILED.                                                   ECS265
00011                                                                   ECS265
00012 *SECURITY.   *****************************************************ECS265
00013 *            *                                                   *ECS265
00014 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ECS265
00015 *            *                                                   *ECS265
00016 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS265
00017 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ECS265
00018 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC, INC.     *ECS265
00019 *            *                                                   *ECS265
00020 *            *****************************************************ECS265
00021                                                                   ECS265
00022  ENVIRONMENT DIVISION.                                            ECS265
00023  INPUT-OUTPUT SECTION.                                            ECS265
00024  FILE-CONTROL.                                                    ECS265
00025      SELECT  OLD-CERT        ASSIGN TO SYS010-UT-2400-S-SYS010.   ECS265
00026      SELECT  NEW-CERT        ASSIGN TO SYS011-UT-2400-S-SYS011.   ECS265
00027      SELECT  CLAIM-FILE      ASSIGN TO SYS012-UT-2400-S-SYS012.   ECS265
00028      SELECT  DISK-DATE       ASSIGN TO SYS019-UT-2314-S-SYS019.   ECS265
00029      SELECT  SORT-FILE       ASSIGN TO SYS001-UT-2314-S-SORTWK1.  ECS265
00030                                                                   ECS265
00031  DATA DIVISION.                                                   ECS265
00032  FILE SECTION.                                                    ECS265
00033  FD  DISK-DATE                                                    ECS265
00034                              COPY ELCDTEFD.                       ECS265
00035      EJECT                                                        ECS265
00036  FD  OLD-CERT                                                     ECS265
00037      RECORDING MODE IS F                                          ECS265
00038      LABEL RECORDS ARE STANDARD                                   ECS265
00039      BLOCK CONTAINS 0 RECORDS
00040      RECORD CONTAINS 1056 CHARACTERS.                             ECS265
00041                              COPY ECSCRT01.                       ECS265
00042                                                                   ECS265
00043      EJECT                                                        ECS265
00044  FD  NEW-CERT                                                     ECS265
00045      RECORDING MODE IS F                                          ECS265
00046      LABEL RECORDS ARE STANDARD                                   ECS265
00047      BLOCK CONTAINS 0 RECORDS
00048      RECORD CONTAINS 1056 CHARACTERS.                             ECS265
00049  01  NEW-REC                 PIC X(1056).                         ECS265
00050                                                                   ECS265
00051      EJECT                                                        ECS265
00052  FD  CLAIM-FILE                                                   ECS265
00053      RECORDING MODE F                                             ECS265
00054      LABEL RECORDS STANDARD                                       ECS265
00055      BLOCK CONTAINS 0 RECORDS
00056      RECORD CONTAINS 510 CHARACTERS.                              ECS265
00057                              COPY ECSEXT01.                       ECS265
00058                                                                   ECS265
00059      EJECT                                                        ECS265
00060  SD  SORT-FILE                                                    ECS265
00061      RECORDING MODE F                                             ECS265
00062 *    LABEL RECORDS ARE STANDARD                                   ECS265
00063      RECORD CONTAINS 47 CHARACTERS.                               ECS265
00064  01  SORT-REC.                                                    ECS265
00065      03 SORT-PARM        PIC X(40).                               ECS265
00066      03 SORT-DATA        PIC X(7).                                ECS265
00067                                                                      CL*17
00068  EJECT                                                            ECS265
00069  WORKING-STORAGE     SECTION.                                     ECS265
00070  77  FILLER  PIC X(32)   VALUE '********************************'.ECS265
00071  77  FILLER  PIC X(32)   VALUE '* WORKING STORAGE STARTS HERE  *'.ECS265
00072  77  FILLER  PIC X(32)   VALUE '********** VMOD=2.005 **********'.   CL**2
00073                                                                      CL*17
00074                              COPY ELCDTECX.                       ECS265
00075                              COPY ELCDTEVR.                       ECS265
00076  01  MISC.                                                        ECS265
00077      12  PGM-SUB                 PIC S999 COMP-3 VALUE +001.         CL*17
00078      12  ABEND-OPTION            PIC X     VALUE 'Y'.                CL*17
00079      12  ABEND-CODE              PIC X(4)  VALUE '9999'.          ECS265
00080      12  WS-ZERO                 PIC S9    VALUE +0 COMP-3.       ECS265
00081      12  WS-RETURN-CODE          PIC S9(4) VALUE +0 COMP-3.       ECS265
00082      12  WS-ABEND-MESSAGE        PIC X(80) VALUE SPACES.             CL*17
00083      12  WS-ABEND-FILE-STATUS    PIC XX    VALUE ZERO.               CL*17
00084      12  IN-CNT                  PIC 9(7)  VALUE ZERO.            ECS265
00085      12  OUT-CNT                 PIC 9(7)  VALUE ZERO.            ECS265
00086      12  DEL-CNT                 PIC 9(7)  VALUE ZERO.            ECS265
00087      12  CAN-CNT                 PIC 9(7)  VALUE ZERO.            ECS265
00088      12  DTH-CNT                 PIC 9(7)  VALUE ZERO.            ECS265
00089      12  NO-CLAIMS               PIC 9(7)  VALUE ZERO.            ECS265
00090      12  PROC-CLAIMS             PIC 9(7)  VALUE ZERO.            ECS265
00091      12  TOT-CERT                PIC 9(7)  VALUE ZERO.               CL*17
00092      12  TOT-CONV-CERT           PIC 9(7)  VALUE ZERO.               CL*17
00093      12  SAVE-CM-DATE.                                            ECS265
00094          16  SAVE-CM-YR      PIC 99      VALUE ZEROS.             ECS265
00095          16  SAVE-CM-MO      PIC 99      VALUE ZEROS.             ECS265
00096          16  SAVE-CM-DA      PIC 99      VALUE ZEROS.             ECS265
00097      12  WORK-CM-DATE.                                            ECS265
00098          16  WORK-CM-YR      PIC 99.                              ECS265
00099          16  WORK-CM-MO      PIC 99.                              ECS265
00100          16  WORK-CM-DA      PIC 99.                              ECS265
00101      12  CERT-FLAGS.                                              ECS265
00102          16  FLAG-1          PIC X       VALUE ' '.               ECS265
00103          16  FLAG-2          PIC X       VALUE ' '.               ECS265
00104          16  FLAG-3          PIC X       VALUE ' '.               ECS265
00105      12  WS-CR-AH-DEX-CCYY   PIC 9(04)   VALUE 0.                 ECS265
00106      12  WS-CR-LF-DEX-CCYY   PIC 9(04)   VALUE 0.                 ECS265
00107      12  WS-CR-ENTRY-CCYY    PIC 9(04)   VALUE 0.                 ECS265
00108      12  WS-CR-AH-CEX-CCYY   PIC 9(04)   VALUE 0.                 ECS265
00109      12  WS-CR-LF-CEX-CCYY   PIC 9(04)   VALUE 0.                 ECS265
00110                                                                      CL*17
00111  01  WORK-REC.                                                    ECS265
00112      03 W-SEQ.                                                    ECS265
00113         05 W-COMP        PIC X(7).                                ECS265
00114         05 W-STATE       PIC X(2).                                ECS265
00115         05 W-ACCT        PIC X(10).                               ECS265
00116         05 W-CERT-DT     PIC 9(11)      COMP-3.                      CL**6
00117         05 W-CERT        PIC X(11).                               ECS265
00118      03 W-SEQ-2.                                                  ECS265
00119         05 W-DATE        PIC 9(07)      COMP-3.                      CL**6
00120      03 W-TYP            PIC X.                                   ECS265
00121         88 W-USED   VALUE ' '.                                    ECS265
00122         88 W-DTH    VALUE '1'.                                    ECS265
00123         88 W-AH     VALUE '2'.                                    ECS265
00124         88 W-OB-DTH VALUE '3'.                                    ECS265
00125         88 W-OB-AH  VALUE '4'.                                    ECS265
00126      03 W-AMT            PIC S9(9)V99     COMP-3.                 ECS265
00127                                                                      CL*17
00128  01  DISPLAY-AMT         PIC X(11).                               ECS265
00129  01  WS-W-CERT-DT        PIC 9(11).                                  CL**6
00130  01  WS-W-CERT-DT-R REDEFINES WS-W-CERT-DT.                          CL**6
00131      05 FILLER           PIC XXX.                                    CL**6
00132      05 W-CERT-CC        PIC XX.                                     CL**6
00133      05 W-CERT-YR        PIC XX.                                     CL**6
00134      05 W-CERT-MO        PIC XX.                                     CL**6
00135      05 W-CERT-DA        PIC XX.                                     CL**6
00136  01  WS-W-DATE           PIC 9(07).                                  CL*10
00137  01  WS-W-DATE-R REDEFINES WS-W-DATE.                                CL*10
00138      05 FILLER           PIC 9(01).                                  CL**6
00139      05 W-CCYY           PIC 9(04).                                  CL**6
00140      05 W-CCYR REDEFINES W-CCYY.                                     CL**6
00141         10 W-CC          PIC 99.                                     CL**6
00142         10 W-YR          PIC 99.                                     CL**6
00143      05 W-MO             PIC 99.                                     CL**6
00144  EJECT                                                            ECS265
00145  COPY ELCEXTVR.                                                   ECS265
00146                                                                   ECS265
00147  COPY ELCCRTVR.                                                      CL**3
00148                                                                      CL**3
00149  PROCEDURE DIVISION.                                              ECS265
00150                                                                      CL*17
00151  INITIAL-SECTION SECTION.                                         ECS265
00152  STANDARD-READ.                                                   ECS265
00153                               COPY ELCDTERX.                      ECS265
00154                                                                      CL*17
00155  SORT-ROUTINE SECTION.                                            ECS265
00156  SORT-RTN.                                                        ECS265
00157      SORT SORT-FILE ON ASCENDING SORT-PARM                        ECS265
00158         INPUT  PROCEDURE INPUT-RTN  THRU INPUT-XIT                   CL*17
00159         OUTPUT PROCEDURE OUTPUT-RTN THRU OUTPUT-XIT.              ECS265
00160                                                                   ECS265
00161      IF SORT-RETURN NOT = 0                                       ECS265
00162         MOVE '0101' TO ABEND-CODE                                 ECS265
00163         GO TO ABEND-PGM.                                          ECS265
00164                                                                   ECS265
00165      GO TO E-O-J.                                                 ECS265
00166                                                                      CL*17
00167  EJECT                                                            ECS265
00168  INPUT-RTN SECTION.                                               ECS265
00169  OPEN-INPUT.                                                      ECS265
00170      OPEN INPUT CLAIM-FILE.                                       ECS265
00171                                                                   ECS265
00172      IF EP-DT = RUN-DATE                                             CL*17
00173         SUBTRACT 1 FROM RUN-CCYY.                                 ECS265
00174                                                                      CL*15
00175  READ-INPUT.                                                      ECS265
00176      READ CLAIM-FILE AT END                                       ECS265
00177          GO TO END-INPUT.                                         ECS265
00178                                                                   ECS265
00179      IF DE-REIN NOT = SPACE                                       ECS265
00180          GO TO READ-INPUT.                                        ECS265
00181                                                                   ECS265
00182      IF DE-TRANS NOT = 'X'                                        ECS265
00183          GO TO READ-INPUT.                                        ECS265
00184                                                                   ECS265
00185      IF DE-DTH     OR                                             ECS265
00186         DE-AH      OR                                             ECS265
00187         DE-OB-DTH  OR                                             ECS265
00188         DE-OB-AH                                                  ECS265
00189         NEXT SENTENCE                                                CL*17
00190      ELSE                                                         ECS265
00191         GO TO READ-INPUT.                                         ECS265
00192                                                                   ECS265
00193      COPY ELCEXTM1.                                               ECS265
00194                                                                   ECS265
00195      IF DE-PROC-DT NUMERIC                                        ECS265
00196         MOVE DE-PD-CC TO DE-PAY-CC                                ECS265
00197         MOVE DE-PD-YR TO DE-PAY-YR                                ECS265
00198         MOVE DE-PD-MO TO DE-PAY-MO                                ECS265
00199         MOVE DE-PD-DA TO DE-PAY-DA                                ECS265
00200         MOVE WS-DE-PAY TO DE-PAY.                                 ECS265
00201                                                                   ECS265
00202      IF DE-PAY-CCYY GREATER THAN EP-CCYY                          ECS265
00203         GO TO READ-INPUT.                                         ECS265
00204                                                                      CL*17
00205      IF DE-PAY-YR = EP-YR AND                                     ECS265
00206         DE-PAY-MO GREATER THAN EP-MO                              ECS265
00207         GO TO READ-INPUT.                                         ECS265
00208                                                                   ECS265
00209      IF DE-PAY-CCYY LESS THAN RUN-CCYY                            ECS265
00210         GO TO READ-INPUT.                                         ECS265
00211                                                                      CL*17
00212      IF DE-PAY-YR = RUN-YR AND                                    ECS265
00213         DE-PAY-MO NOT GREATER THAN RUN-MO                         ECS265
00214         GO TO READ-INPUT.                                         ECS265
00215                                                                   ECS265
00216      MOVE DE-CONTROL       TO W-SEQ.                                 CL*17
00217      MOVE DE-PAY-CC        TO W-CC.                                  CL*17
00218      MOVE DE-PAY-YR        TO W-YR.                                  CL*17
00219      MOVE DE-PAY-MO        TO W-MO.                                  CL*17
00220      MOVE DE-TYPE          TO W-TYP.                                 CL*17
00221      MOVE WS-W-DATE        TO W-DATE.                                CL*17
00222      MOVE DE-CLAIM-AMT     TO W-AMT.                                 CL*17
00223      MOVE WORK-REC         TO SORT-REC.                              CL*17
00224                                                                   ECS265
00225      RELEASE SORT-REC.                                            ECS265
00226      ADD +1 TO NO-CLAIMS.                                         ECS265
00227                                                                   ECS265
00228      GO TO READ-INPUT.                                               CL*17
00229                                                                   ECS265
00230  END-INPUT.                                                       ECS265
00231      CLOSE CLAIM-FILE.                                            ECS265
00232                                                                      CL*17
00233  INPUT-XIT.                                                          CL*17
00234       EXIT.                                                          CL*17
00235                                                                      CL*17
00236  EJECT                                                            ECS265
00237  OUTPUT-RTN SECTION.                                              ECS265
00238                                                                      CL*17
00239  OPEN-OUTPUT.                                                     ECS265
00240      OPEN INPUT  OLD-CERT                                         ECS265
00241           OUTPUT NEW-CERT.                                        ECS265
00242                                                                      CL*17
00243      MOVE LOW-VALUE TO WORK-REC.                                  ECS265
00244      MOVE SPACES    TO W-TYP.                                        CL*17
00245      MOVE ZEROES    TO W-AMT.                                        CL*17
00246                                                                   ECS265
00247  READ-OLD-CERT.                                                   ECS265
00248      READ OLD-CERT                                                ECS265
00249          AT END  GO TO END-OUTPUT.                                ECS265
00250                                                                      CL*17
00251      ADD +1 TO IN-CNT.                                            ECS265
00252                                                                   ECS265
00253      COPY ELCCRTM1.                                               ECS265
00254                                                                   ECS265
00255      MOVE CR-ENTRY-CC  TO   WS-CR-ENTRY-CCYY(1:2).                ECS265
00256      MOVE CR-ENTRY-YR  TO   WS-CR-ENTRY-CCYY(3:2).                ECS265
00257                                                                      CL*17
00258      IF WS-CR-ENTRY-CCYY  GREATER THAN EP-CCYY                    ECS265
00259          ADD  +1  TO  DEL-CNT                                     ECS265
00260          GO TO READ-OLD-CERT.                                     ECS265
00261                                                                   ECS265
00262      IF CR-ENTRY-YR  =  EP-YR AND                                 ECS265
00263         CR-ENTRY-MO GREATER THAN EP-MO                            ECS265
00264          ADD  +1  TO  DEL-CNT                                     ECS265
00265          GO TO READ-OLD-CERT.                                     ECS265
00266                                                                   ECS265
00267      IF CR-POLICY-IS-DECLINED OR                                  ECS265
00268         CR-POLICY-IS-VOID                                         ECS265
00269          GO TO READ-OLD-CERT.                                     ECS265
00270                                                                   ECS265
00271      IF CR-LFTYP = ZERO                                           ECS265
00272          GO TO AH-CHECKS.                                         ECS265
00273                                                                   ECS265
00274      IF NO-CLAIMS = +0                                            ECS265
00275         SUBTRACT CR-DTHAMT-YTD FROM CR-DTHAMT                     ECS265
00276         MOVE +0 TO CR-DTHAMT-YTD CR-DTHAMT-LAST                   ECS265
00277                    CR-DTHEXP-YTD CR-DTHEXP                           CL*17
00278      ELSE                                                         ECS265
00279         MOVE  ZERO  TO                                            ECS265
00280            CR-NUM-DTH-CLM CR-DTHAMT CR-DTHAMT-YTD                    CL*17
00281            CR-DTHAMT-LAST CR-DTHEXP CR-DTHEXP-YTD.                ECS265
00282                                                                      CL*17
00283      MOVE CR-LF-CEX-CC  TO  WS-CR-LF-CEX-CCYY(1:2).               ECS265
00284      MOVE CR-LF-CEX-YR  TO  WS-CR-LF-CEX-CCYY(3:2).               ECS265
00285                                                                      CL*17
00286      IF WS-CR-LF-CEX-CCYY GREATER THAN EP-CCYY OR                    CL*17
00287         (CR-LF-CEX-YR = EP-YR AND CR-LF-CEX-MO GREATER THAN EP-MO)ECS265
00288          ADD  +1  TO  CAN-CNT                                     ECS265
00289          MOVE  ZERO  TO  CR-LFRFND  CR-LF-CANC-DT                 ECS265
00290                          CR-LF-CANCEL-EXIT-DATE                      CL*17
00291                          CR-LFRFND-CALC                              CL*17
CIDMOD              WS-CR-LF-CANC-DT-N
CIDMOD              WS-CR-LF-CANCEL-EXIT-DATE-N
00292          MOVE  SPACE  TO  CR-LF-STATUS-AT-CANCEL.                 ECS265
00293                                                                   ECS265
00294      MOVE CR-LF-DEX-CC  TO  WS-CR-LF-DEX-CCYY(1:2).                  CL*17
00295      MOVE CR-LF-DEX-YR  TO  WS-CR-LF-DEX-CCYY(3:2).                  CL*17
00296                                                                      CL*17
00297      IF WS-CR-LF-DEX-CCYY GREATER THAN EP-CCYY OR                    CL*17
00298         (CR-LF-DEX-YR = EP-YR AND CR-LF-DEX-MO GREATER THAN EP-MO)ECS265
00299          ADD  +1  TO  DTH-CNT                                     ECS265
00300          MOVE  ZERO  TO                                           ECS265
00301                CR-LF-CLAIM-EXIT-DATE                              ECS265
00302                CR-NUM-DTH-CLM CR-DTHAMT  CR-DTHAMT-YTD            ECS265
00303                CR-DTHAMT-LAST CR-DTHEXP  CR-DTHEXP-YTD            ECS265
CIDMOD               WS-CR-DTH-DT-N        CR-DTH-DT
CIDMOD               WS-CR-DTH-PAY-DT-N    CR-DTH-PAY-DT
CIDMOD               WS-CR-LF-CLAIM-EXIT-DATE-N
00304          MOVE SPACE TO  CR-LF-STATUS-AT-DEATH.                    ECS265
00305                                                                   ECS265
00306      IF CR-LF-STATUS-AT-CANCEL = SPACE                            ECS265
00307          IF CR-LF-STATUS-AT-DEATH = SPACE                         ECS265
00308              MOVE CR-ENTRY-STATUS TO CR-LF-CURRENT-STATUS         ECS265
00309          ELSE                                                     ECS265
00310              MOVE CR-ENTRY-STATUS TO CR-LF-STATUS-AT-DEATH        ECS265
00311              IF CR-LF-CURRENT-STATUS = '6' OR '7'                 ECS265
00312                  NEXT SENTENCE                                    ECS265
00313              ELSE                                                 ECS265
00314                  MOVE '7' TO CR-LF-CURRENT-STATUS                 ECS265
00315      ELSE                                                         ECS265
00316          IF CR-LF-STATUS-AT-DEATH = SPACE                         ECS265
00317              MOVE CR-ENTRY-STATUS TO CR-LF-STATUS-AT-CANCEL       ECS265
00318              MOVE '8' TO CR-LF-CURRENT-STATUS.                    ECS265
00319                                                                      CL*17
00320  AH-CHECKS.                                                       ECS265
00321      IF CR-AHTYP = ZERO                                           ECS265
00322          GO TO WT-REC.                                            ECS265
00323                                                                   ECS265
00324      IF NO-CLAIMS = +0                                            ECS265
00325         SUBTRACT CR-DISAMT-YTD FROM CR-DISAMT                     ECS265
00326         MOVE +0 TO CR-DISAMT-YTD CR-DISAMT-LAST                   ECS265
00327                    CR-DISEXP-YTD CR-DISEXP                           CL*17
00328      ELSE                                                         ECS265
00329         MOVE  ZERO  TO                                            ECS265
00330            CR-NUM-DIS-CLM CR-DISAMT CR-DISAMT-YTD                 ECS265
00331            CR-DISEXP CR-DISEXP-YTD                                ECS265
00332            CR-DISAMT-LAST.                                        ECS265
00333                                                                   ECS265
00334      MOVE CR-AH-CEX-CC  TO  WS-CR-AH-CEX-CCYY(1:2).               ECS265
00335      MOVE CR-AH-CEX-YR  TO  WS-CR-AH-CEX-CCYY(3:2).               ECS265
00336                                                                      CL*17
00337      IF WS-CR-AH-CEX-CCYY GREATER THAN EP-CCYY OR                    CL*17
00338         (CR-AH-CEX-YR = EP-YR AND CR-AH-CEX-MO GREATER THAN EP-MO)ECS265
00339          ADD  +1  TO  CAN-CNT                                     ECS265
00340          MOVE  ZERO  TO  CR-AHRFND CR-AH-CANC-DT                  ECS265
00341                          CR-AH-CANCEL-EXIT-DATE                      CL*17
00342                          CR-AHRFND-CALC                              CL*17
CIDMOD                      WS-CR-AH-CANC-DT-N
CIDMOD                      WS-CR-AH-CANCEL-EXIT-DATE-N
00343          MOVE SPACE  TO  CR-AH-STATUS-AT-CANCEL.                     CL*17
00344                                                                   ECS265
00345      MOVE CR-AH-DEX-CC  TO  WS-CR-AH-DEX-CCYY(1:2).               ECS265
00346      MOVE CR-AH-DEX-YR  TO  WS-CR-AH-DEX-CCYY(3:2).               ECS265
00347                                                                      CL*17
00348      IF WS-CR-AH-DEX-CCYY GREATER THAN EP-CCYY OR                    CL*17
00349         (CR-AH-DEX-YR = EP-YR AND CR-AH-DEX-MO GREATER THAN EP-MO)ECS265
00350          ADD  +1  TO  DTH-CNT                                     ECS265
00351          MOVE  ZERO  TO  CR-DISAB-CLAIM-DATA                      ECS265
00352                          CR-AH-SETTLEMENT-EXIT-DATE               ECS265
00353                          CR-NUM-DIS-CLM CR-DISAMT  CR-DISAMT-YTD  ECS265
00354                          CR-DISAMT-LAST                           ECS265
CIDMOD            WS-CR-AH-SETTLEMENT-EXIT-DATEN
00355          MOVE SPACE TO  CR-AH-STATUS-AT-SETTLEMENT.               ECS265
00356                                                                   ECS265
00357      IF CR-AH-STATUS-AT-CANCEL = SPACE                            ECS265
00358          IF CR-AH-STATUS-AT-SETTLEMENT = SPACE                    ECS265
00359              MOVE CR-ENTRY-STATUS TO CR-AH-CURRENT-STATUS         ECS265
00360          ELSE                                                     ECS265
00361              MOVE CR-ENTRY-STATUS TO CR-AH-STATUS-AT-SETTLEMENT   ECS265
00362              IF CR-AH-CURRENT-STATUS = '6' OR '7'                 ECS265
00363                  NEXT SENTENCE                                    ECS265
00364              ELSE                                                 ECS265
00365                  MOVE '7' TO CR-AH-CURRENT-STATUS                 ECS265
00366      ELSE                                                         ECS265
00367          IF CR-AH-STATUS-AT-SETTLEMENT = SPACE                    ECS265
00368              MOVE CR-ENTRY-STATUS TO CR-AH-STATUS-AT-CANCEL       ECS265
00369              MOVE '8' TO CR-AH-CURRENT-STATUS.                    ECS265
00370                                                                   ECS265
00371  WT-REC.                                                          ECS265
00372      PERFORM MATCH-CLAIM-RTN THRU MATCH-CLAIM-XIT.                ECS265
00373                                                                      CL*17
00374      COPY ELCCRTM2.                                               ECS265
00375                                                                      CL*17
00376      WRITE NEW-REC FROM CERTIFICATE-RECORD.                       ECS265
00377      ADD +1 TO OUT-CNT.                                           ECS265
00378      GO TO READ-OLD-CERT.                                         ECS265
00379                                                                   ECS265
00380  MATCH-CLAIM-RTN.                                                 ECS265
00381      IF W-SEQ LESS THAN CR-FULL-CONTROL                           ECS265
00382         GO TO CHECK-USAGE.                                        ECS265
00383      IF W-SEQ GREATER THAN CR-FULL-CONTROL                        ECS265
00384         GO TO MATCH-CLAIM-XIT.                                    ECS265
00385      IF W-SEQ = HIGH-VALUE                                        ECS265
00386         GO TO MATCH-CLAIM-XIT.                                    ECS265
00387                                                                   ECS265
00388  MATCH-RTN.                                                       ECS265
00389      IF W-DTH OR                                                     CL*17
00390         W-OB-DTH                                                     CL*17
00391          GO TO MATCH-LIFE.                                           CL*17
00392                                                                   ECS265
00393  MATCH-AH.                                                        ECS265
00394      MOVE CR-AH-DEX-CC   TO WS-CR-AH-DEX-CCYY(1:2).               ECS265
00395      MOVE CR-AH-DEX-YR   TO WS-CR-AH-DEX-CCYY(3:2).               ECS265
00396                                                                      CL*17
00397      IF CR-AH-DEX-YR GREATER THAN ZERO                            ECS265
00398          IF W-CCYY GREATER THAN WS-CR-AH-DEX-CCYY                 ECS265
00399              MOVE W-CC  TO CR-AH-DEX-CC                           ECS265
00400              MOVE W-YR  TO CR-AH-DEX-YR                              CL*20
00401              MOVE W-MO  TO CR-AH-DEX-MO                              CL*20
00402              MOVE WS-CR-AH-SETTLEMENT-EXIT-DATE  TO                  CL*17
00403                      CR-AH-SETTLEMENT-EXIT-DATE.                     CL*17
00404                                                                      CL*17
00405      ADD W-AMT TO CR-DISAMT-YTD                                      CL*17
00406                   CR-DISAMT.                                         CL*17
00407                                                                      CL*17
00408      MOVE W-AMT TO CR-DISAMT-LAST.                                ECS265
00409                                                                      CL*17
00410      ADD +1 TO CR-NUM-DIS-CLM.                                    ECS265
00411                                                                      CL*17
00412      MOVE SPACES TO W-TYP.                                        ECS265
00413                                                                      CL*17
00414      GO TO CHECK-USAGE.                                           ECS265
00415                                                                   ECS265
00416  MATCH-LIFE.                                                      ECS265
00417      IF W-CCYY GREATER THAN WS-CR-LF-DEX-CCYY                     ECS265
00418          MOVE W-CC  TO CR-LF-DEX-CC                               ECS265
00419          MOVE W-YR  TO CR-LF-DEX-YR                                  CL*20
00420          MOVE W-MO  TO CR-LF-DEX-MO                                  CL*20
00421          MOVE WS-CR-LF-CLAIM-EXIT-DATE TO                         ECS265
00422                  CR-LF-CLAIM-EXIT-DATE.                              CL*17
00423                                                                      CL*17
00424      ADD +1 TO CR-NUM-DTH-CLM.                                    ECS265
00425                                                                      CL*17
00426      ADD W-AMT TO CR-DTHAMT-YTD                                      CL*17
00427                   CR-DTHAMT.                                         CL*17
00428                                                                      CL*17
00429      MOVE W-AMT TO CR-DTHAMT-LAST.                                ECS265
00430                                                                      CL*17
00431      MOVE SPACES TO W-TYP.                                        ECS265
00432                                                                   ECS265
00433  CHECK-USAGE.                                                     ECS265
00434      IF W-SEQ = LOW-VALUES                                           CL*17
00435          GO TO RETURN-RTN.                                           CL*17
00436                                                                      CL*17
00437      IF W-USED                                                    ECS265
00438         ADD +1 TO PROC-CLAIMS                                     ECS265
00439         GO TO RETURN-RTN.                                         ECS265
00440                                                                      CL*17
00441      DISPLAY W-SEQ ' ' W-TYP ' ' W-AMT ' NO MATCH ' W-DATE.       ECS265
00442                                                                   ECS265
00443  RETURN-RTN.                                                      ECS265
00444      RETURN SORT-FILE AT END                                      ECS265
00445         MOVE HIGH-VALUE TO WORK-REC                               ECS265
00446         GO TO MATCH-CLAIM-XIT.                                    ECS265
00447                                                                      CL*17
00448      MOVE SORT-REC     TO WORK-REC.                                  CL*17
00449      MOVE W-DATE       TO WS-W-DATE.                                 CL*17
00450      MOVE W-CERT-DT    TO WS-W-CERT-DT.                              CL*17
00451      GO TO MATCH-CLAIM-RTN.                                       ECS265
00452                                                                      CL*17
00453  MATCH-CLAIM-XIT.                                                    CL*17
00454             EXIT.                                                    CL*17
00455                                                                   ECS265
00456  END-OUTPUT.                                                      ECS265
00457      MOVE HIGH-VALUE TO CERTIFICATE-RECORD.                       ECS265
00458                                                                      CL*17
00459      COPY ELCCRTM1.                                               ECS265
00460                                                                      CL*17
00461      PERFORM MATCH-CLAIM-RTN THRU MATCH-CLAIM-XIT.                ECS265
00462                                                                      CL*17
00463      CLOSE OLD-CERT                                               ECS265
00464            NEW-CERT.                                              ECS265
00465                                                                      CL*17
00466      DISPLAY  '   FROM ' WS-RUN-DATE '  THRU ' WS-EP-DT.             CL**7
00467      DISPLAY  '   IN COUNT       ' IN-CNT.                        ECS265
00468      DISPLAY  '   DELETED        '  DEL-CNT.                      ECS265
00469      DISPLAY  '   OUT COUNT      '  OUT-CNT.                      ECS265
00470      DISPLAY  '   CANCELLED      '  CAN-CNT.                      ECS265
00471      DISPLAY  '   DEATH          '  DTH-CNT.                      ECS265
00472      DISPLAY  '   TOTAL CLAIMS   '  NO-CLAIMS.                    ECS265
00473      DISPLAY  '     PROCESSED    '  PROC-CLAIMS.                  ECS265
00474                                                                      CL*17
00475  OUTPUT-XIT.                                                         CL*17
00476        EXIT.                                                         CL*17
00477                                                                      CL*17
00478  END-OF-JOB SECTION.                                              ECS265
00479                                                                      CL*17
00480  E-O-J.                                                           ECS265
00481      MOVE ZEROS  TO RETURN-CODE.
00481      GOBACK.                                                      ECS265
00482                                                                      CL*17
00483  ABEND-PGM SECTION.                                               ECS265
00484                                COPY ELCABEND.                     ECS265
