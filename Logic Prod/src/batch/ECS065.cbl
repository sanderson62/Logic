      $SET ALTER
00001  IDENTIFICATION DIVISION.                                         03/06/98
00002                                                                   ECS065
00003  PROGRAM-ID.                ECS065.                                  LV003
00004 *              PROGRAM CONVERTED BY                               ECS065
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ECS065
00006 *              CONVERSION DATE 02/08/96 18:16:14.                 ECS065
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          ECS065
00008 *                           VMOD=2.008.                           ECS065
00009                                                                   ECS065
00010 *AUTHOR.        LOGIC, INC.                                       ECS065
00011 *               DALLAS, TEXAS.                                    ECS065
00012                                                                   ECS065
00013 *DATE-COMPILED.                                                   ECS065
00014                                                                   ECS065
00015 *SECURITY.   *****************************************************ECS065
00016 *            *                                                   *ECS065
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ECS065
00018 *            *                                                   *ECS065
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS065
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ECS065
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *ECS065
00022 *            *                                                   *ECS065
00023 *            *****************************************************ECS065
00024                                                                   ECS065
00025 *REMARKS.                                                         ECS065
00026 *        PRINTS MAILING LABELS FROM THE COMPENSATION MASTER.      ECS065
00027 *                                                                 ECS065
00028 *        PROGRAM OPTION      -     DESCRIPTION.                   ECS065
00029 *                                                                 ECS065
00030 *        1   -   1 UP FOR MASTERS FOR STATEMENTS ONLY             ECS065
00031 *        2   -   2 UP FOR MASTERS FOR STATEMENTS ONLY             ECS065
00032 *        3   -   3 UP FOR MASTERS FOR STATEMENTS ONLY             ECS065
00033 *                                                                 ECS065
00034 *        4   -   1 UP FOR ALL COMPENSATION MASTERS                ECS065
00035 *        5   -   2 UP FOR ALL COMPENSATION MASTERS                ECS065
00036 *        6   -   3 UP FOR ALL COMPENSATION MASTERS                ECS065
CIDMOD*                                                                 ECS065
090104******************************************************************
090104*                   C H A N G E   L O G
090104*
090104* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
090104*-----------------------------------------------------------------
090104*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
090104* EFFECTIVE    NUMBER
090104*-----------------------------------------------------------------
CIDMOD* CIDMOD IS TO ADD ACCOUNT NUMBER AS 1ST PRINT LINE ON LABELS.    ECS065
102004* 102004   2004081900010   PEMA  REMOVE X'S FROM 2ND COLUMN OVER 
050605* 050605   2005040600009   PEMA  REMOVE ALL ALIGNMENTS
051106* 051106   2005071300004   AJRA  ADD PRIMARY CONTACT FOR DCC LABELS
051206* 051206   2005071400004   AJRA  CHANGE MAX-ALIGN FROM 14 TO 1
022007* 022007                   PEMA  REMOVE EL562 STATEMENTS
051810* 051810 CR2010042900001   PEMA  SPEARATE CITY AND STATE
030612* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
010716* 010716  CR2015082500001  PEMA  VPP CHANGES
062121* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
102004******************************************************************
00037  EJECT                                                            ECS065
00038  ENVIRONMENT DIVISION.                                            ECS065
00039  INPUT-OUTPUT SECTION.                                            ECS065
00040  FILE-CONTROL.                                                    ECS065
00041                                                                   ECS065
00042      SELECT PRNTR            ASSIGN TO SYS008-UR-1403-S-SYS008.   ECS065
00043      SELECT COMM-MSTR-IN     ASSIGN TO SYS010-UT-2400-S-SYS010.   ECS065
00044      SELECT WORK-FILE        ASSIGN TO SYS015-UT-FBA1-S-SYS015.   ECS065
00045      SELECT DISK-DATE        ASSIGN TO SYS019-UT-FBA1-S-SYS019.   ECS065
00046      SELECT FICH             ASSIGN TO SYS020-UT-2400-S-SYS020.   ECS065
00047  EJECT                                                            ECS065
00048  DATA DIVISION.                                                   ECS065
00049  FILE SECTION.                                                    ECS065
00050                                                                   ECS065
00051  FD  PRNTR                                                        ECS065
00052                              COPY ELCPRTFD.                       ECS065
00053  EJECT                                                            ECS065
00054  FD  COMM-MSTR-IN                                                 ECS065
00055                              COPY ECSCOIFD.                       ECS065
00056  EJECT                                                            ECS065
00057  FD  WORK-FILE                                                    ECS065
00058      BLOCK CONTAINS 0 RECORDS
00059      RECORDING MODE IS F.                                            CL**3
00060                                                                   ECS065
00061  01  WORK-REC.                                                    ECS065
00062      12  FILLER              PIC  X(700).                         ECS065
00063  EJECT                                                            ECS065
00064  FD  DISK-DATE                                                    ECS065
00065                              COPY ELCDTEFD.                       ECS065
00066  EJECT                                                            ECS065
00067  FD  FICH                                                         ECS065
00068                              COPY ELCFCHFD.                       ECS065
00069  EJECT                                                            ECS065
00070  WORKING-STORAGE SECTION.                                         ECS065
00071  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      ECS065
00072  77  FILLER PIC X(32) VALUE '********************************'.   ECS065
00073  77  FILLER PIC X(32) VALUE '*            ECS065            *'.   ECS065
00074  77  FILLER PIC X(32) VALUE '*********** VMOD=2.008 *********'.   ECS065
00075                                                                   ECS065
00076  77  K0                      PIC S9          VALUE +0   COMP-3.   ECS065
00077  77  K1                      PIC S9          VALUE +1   COMP-3.   ECS065
00078  77  K2                      PIC S9          VALUE +2   COMP-3.   ECS065
00079  77  K3                      PIC S9          VALUE +3   COMP-3.   ECS065
00080  77  X1                      PIC S9(3)       VALUE +0   COMP-3.   ECS065
00081  77  PGM-SUB                 PIC S9(3)       VALUE +65  COMP-3.   ECS065
00082  77  MAX-NDX                 PIC S9(3)       VALUE +3   COMP-3.   ECS065
00083  77  STMT-SW                 PIC S9(3)       VALUE +1   COMP-3.   ECS065
00084  77  A                       PIC S9(3)       VALUE +0   COMP-3.   ECS065
00085  77  B                       PIC S9(3)       VALUE +0   COMP-3.   ECS065
00086  77  NBR-OF-LABELS           PIC S9(7)       VALUE +0   COMP-3.   ECS065
00087  77  MAX-ALIGN-CTR           PIC S9(9)       VALUE +0   COMP-3.   ECS065
00088  77  ALIGN-CTR               PIC S9(9)       VALUE +0   COMP-3.   ECS065
00089  77  X                       PIC  X          VALUE ' '.           ECS065
00090  77  SPACE-NP                PIC  X          VALUE '1'.           ECS065
00091  77  SPACE-1                 PIC  X          VALUE ' '.           ECS065
00092  77  SPACE-2                 PIC  X          VALUE '0'.           ECS065
00093  77  SPACE-3                 PIC  X          VALUE '-'.           ECS065
00094 *                                                                 ECS065
00095 *                                                                 ECS065
00096 * LEAVE THE ABOVE 2 LINE IN TO PREVENT LOSING THE LINE ABOVE      ECS065
00107                                                                   ECS065
CIDMOD                                                                  00000922
CIDMOD 01  CO-ACCT-CK.                                                  00000923
CIDMOD     12  CO-ACCT-X               PIC X(10).                       00000924
CIDMOD     12  CO-ACCT-N  REDEFINES                                     00000925
CIDMOD         CO-ACCT-X               PIC 9(10).                       00000926
CIDMOD                                                                  00000927
00108  01  WS.                                                          ECS065
00109      12  WS-RETURN-CODE          PIC S9(4)              COMP.     ECS065
00110      12  WS-ABEND-MESSAGE        PIC  X(80).                      ECS065
00111      12  WS-ABEND-FILE-STATUS    PIC  X(2)   VALUE ZEROS.         ECS065
00112      12  WS-ZERO                 PIC S9      VALUE ZERO COMP-3.   ECS065
00113  EJECT                                                            ECS065
00114                              COPY ERCCOMP.                        ECS065
00115  EJECT                                                            ECS065
00116  01  MISC-WS.                                                     ECS065
00117      12  A-NAME.                                                  ECS065
00118          16  A-CHAR          PIC  X      OCCURS 30 TIMES.         ECS065
00119      12  B-NAME.                                                  ECS065
00120          16  B-CHAR          PIC  X      OCCURS 30 TIMES.         ECS065
00121                                                                   ECS065
00122      12  WS-CITY-STATE.                                           ECS065
00123          16  WS-CITY         PIC  X(28).                          ECS065
00124          16  WS-STATE        PIC  XX.                             ECS065
00125                                                                   ECS065
00126  01  ACCT-WORK-LINE.                                              ECS065
00127      12  FILLER              PIC X(20)   VALUE SPACES.            ECS065
00128      12  WK-ACCT-NO.                                              ECS065
022007         16  WK-ACCT-1ST     PIC X(6)    VALUE SPACES.
022007         16  WK-ACCT-LAST    PIC X(4)    VALUE SPACES.            ECS065
00131      12  FILLER              PIC X(6)    VALUE SPACES.            ECS065
00132                                                                   ECS065
CIDMOD 01  LABEL-FILLER.                                                00000950
CIDMOD     12  LINE-FILLER-CC      PIC  X        VALUE SPACES.          00000951
CIDMOD     12  LINE-FILLER         PIC  X(35)    VALUE SPACES.          00000952
CIDMOD                                                                  00000953
00133  01  LABEL-AREA.                                                  ECS065
CIDMOD     12  LINE-0.                                                  00000955
CIDMOD         16  LN-0        OCCURS 3 TIMES.                          00000956
CIDMOD             20  FILLER      PIC  X(26).                          00000957
CIDMOD             20  L0-ACCT     PIC  X(10).                          00000958
00134      12  LINE-1.                                                  ECS065
00135          16  LN-1        OCCURS 3 TIMES.                          ECS065
00136              20  FILLER          PIC  X.                          ECS065
00137              20  L1-INFO         PIC  X(33).                      ECS065
00138              20  FILLER          PIC  X.                          ECS065
00139              20  L1-SP           PIC  X.                          ECS065
00140      12  LINE-2.                                                  ECS065
00141          16  LN-2        OCCURS 3 TIMES.                          ECS065
00142              20  FILLER          PIC  X.                          ECS065
00143              20  L2-INFO         PIC  X(33).                      ECS065
00144              20  FILLER          PIC  X.                          ECS065
00145              20  L2-SP           PIC  X.                          ECS065
00146      12  LINE-3.                                                  ECS065
00147          16  LN-3        OCCURS 3 TIMES.                          ECS065
00148              20  FILLER          PIC  X.                          ECS065
00149              20  L3-INFO         PIC  X(33).                      ECS065
00150              20  FILLER          PIC  X.                          ECS065
00151              20  L3-SP           PIC  X.                          ECS065
00152      12  LINE-4.                                                  ECS065
00153          16  LN-4        OCCURS 3 TIMES.                          ECS065
00154              20  FILLER          PIC  X.                          ECS065
00155              20  L4-INFO         PIC  X(33).                      ECS065
00156              20  FILLER          PIC  X.                          ECS065
00157              20  L4-SP           PIC  X.                          ECS065
00158      12  LINE-5.                                                  ECS065
00159          16  LN-5        OCCURS 3 TIMES.                          ECS065
00160              20  FILLER          PIC  X.                          ECS065
00161              20  L5-INFO.                                         ECS065
00162                  24  L5-CITY-ST.                                  ECS065
00163                      28  L5-CITY PIC  X(22).                      ECS065
00164                      28  L5-ST   PIC  XX.                         ECS065
00165                  24  L5-ZIP      PIC  X(9).                       ECS065
00166              20  FILLER          PIC  X.                          ECS065
00167              20  L5-SP           PIC  X.                          ECS065
00168                                                                   ECS065
00169  01  LABEL-AREA-X REDEFINES LABEL-AREA.                           ECS065
CIDMOD     12  LABEL-LINE-0.                                            00001364
CIDMOD         16  LAB-1-L0        PIC  X(36).                          00001365
CIDMOD         16  LAB-2-L0        PIC  X(36).                          00001366
CIDMOD         16  LAB-3-L0        PIC  X(36).                          00001367
00170      12  LABEL-LINE-1.                                            ECS065
00171          16  LAB-1-L1        PIC  X(36).                          ECS065
00172          16  LAB-2-L1        PIC  X(36).                          ECS065
00173          16  LAB-3-L1        PIC  X(36).                          ECS065
00174      12  LABEL-LINE-2.                                            ECS065
00175          16  LAB-1-L2        PIC  X(36).                          ECS065
00176          16  LAB-2-L2        PIC  X(36).                          ECS065
00177          16  LAB-3-L2        PIC  X(36).                          ECS065
00178      12  LABEL-LINE-3.                                            ECS065
00179          16  LAB-1-L3        PIC  X(36).                          ECS065
00180          16  LAB-2-L3        PIC  X(36).                          ECS065
00181          16  LAB-3-L3        PIC  X(36).                          ECS065
00182      12  LABEL-LINE-4.                                            ECS065
00183          16  LAB-1-L4        PIC  X(36).                          ECS065
00184          16  LAB-2-L4        PIC  X(36).                          ECS065
00185          16  LAB-3-L4        PIC  X(36).                          ECS065
00186      12  LABEL-LINE-5.                                            ECS065
00187          16  LAB-1-L5        PIC  X(36).                          ECS065
00188          16  LAB-2-L5        PIC  X(36).                          ECS065
00189          16  LAB-3-L5        PIC  X(36).                          ECS065
00190  EJECT                                                            ECS065
00191  01  P-REC.                                                       ECS065
00192      12  P-CCSW              PIC  X.                              ECS065
00193      12  P-LN.                                                    ECS065
00194          16  FILLER          PIC  X(132).                         ECS065
00195  EJECT                                                            ECS065
00196                              COPY ELCDTECX.                       ECS065
00197  EJECT                                                            ECS065
00198                              COPY ELCDTEVR.                       ECS065
00199  EJECT                                                            ECS065
00200  PROCEDURE DIVISION.                                              ECS065
00201                                                                   ECS065
00202  0000-STANDARD-COPY.                                              ECS065
00203                              COPY ELCDTERX.                       ECS065
00204  EJECT                                                            ECS065
00205  1000-INTL-INPUT.                                                 ECS065
00206      OPEN INPUT  COMM-MSTR-IN                                     ECS065
00207           OUTPUT WORK-FILE PRNTR.                                 ECS065
00208                                                                   ECS065
00209      IF DTE-PGM-OPT  IS LESS THAN  1                              ECS065
00210        OR  DTE-PGM-OPT  IS GREATER THAN  6                        ECS065
00211          MOVE 3                  TO  DTE-PGM-OPT.                 ECS065
00212                                                                   ECS065
00213      IF DTE-PGM-OPT = (1 OR  2  OR  3)                            ECS065
051206*102004        MOVE +14                 TO MAX-ALIGN-CTR
051206         MOVE +1                 TO MAX-ALIGN-CTR
102004     END-IF
00215                                                                   ECS065
00216      IF DTE-PGM-OPT = (4 OR  5  OR  6)                            ECS065
00217          MOVE +001               TO  MAX-ALIGN-CTR.               ECS065
00218                                                                   ECS065
00219      IF DTE-PGM-OPT  IS GREATER THAN  3                           ECS065
00220          MOVE +2                 TO  STMT-SW                      ECS065
00221          SUBTRACT 3              FROM  DTE-PGM-OPT                ECS065
00222      ELSE                                                         ECS065
00223          MOVE +1                 TO  STMT-SW.                     ECS065
00224                                                                   ECS065
00225      MOVE DTE-PGM-OPT            TO  MAX-NDX.                     ECS065
050605*    MOVE ALL 'X'                TO  LABEL-AREA.                  ECS065
050605     MOVE SPACES                 TO  LABEL-AREA.                  ECS065
00227      MOVE SPACES                 TO  L1-SP (K1)  L2-SP (K1)       ECS065
00228                                      L3-SP (K1)  L4-SP (K1)       ECS065
00229                                      L5-SP (K1).                  ECS065
00230      MOVE SPACES                 TO  L1-SP (K2)  L2-SP (K2)       ECS065
00231                                      L3-SP (K2)  L4-SP (K2)       ECS065
00232                                      L5-SP (K2).                  ECS065
00233      MOVE SPACES                 TO  L1-SP (K3)  L2-SP (K3)       ECS065
00234                                      L3-SP (K3)  L4-SP (K3)       ECS065
00235                                                  L5-SP (K3).      ECS065
00236      MOVE COMPANY-NAME           TO  L1-INFO (1)  L1-INFO (2)     ECS065
00237                                      L1-INFO (3).                 ECS065
00238                                                                   ECS065
102004     IF MAX-NDX  < +3
102004        MOVE SPACES              TO  LAB-3-L0  LAB-3-L1
102004                                     LAB-3-L2  LAB-3-L3
102004                                     LAB-3-L4  LAB-3-L5
102004     END-IF

00243      IF MAX-NDX < +2
102004        MOVE SPACES              TO  LAB-2-L0  LAB-2-L1
102004                                     LAB-2-L2  LAB-2-L3
102004                                     LAB-2-L4  LAB-2-L5
102004     END-IF
00246                                                                   ECS065
050605*    MOVE MAX-NDX                TO  X1.                          ECS065
00248                                                                   ECS065
00249      GO TO 2000-RD-COMM.                                          ECS065
00250  EJECT                                                            ECS065
00251  2000-RD-COMM.                                                    ECS065
00252      GO TO 2100-READ-ACCT.                                        ECS065
00253                                                                   ECS065
00254  2100-READ-ACCT.                                                  ECS065
00255      READ COMM-MSTR-IN                                            ECS065
00256          AT END                                                   ECS065
00257              GO TO 2800-END-ACCT.                                 ECS065
00258                                                                   ECS065
00259      MOVE COMP-IN-RECORD         TO  COMPENSATION-MASTER.         ECS065
00260                                                                   ECS065
00261      IF (CO-AUTO-GENERATED-THIS-RUN)
                      OR
00262         (CO-AUTO-GENERATED)
                      OR
062121        ((DTE-CLIENT = 'CID' OR 'AHL' or 'FNL')
              AND (CO-ACCOUNT-TYPE))
00263         GO TO 2000-RD-COMM
           END-IF
00264                                                                   ECS065
00265      IF CO-ACCOUNT-TYPE                                           ECS065
00266        OR  CO-COMPANY-TYPE                                        ECS065
00267          GO TO 2999-BRANCH.                                       ECS065
00268                                                                   ECS065
00269      MOVE COMPENSATION-MASTER    TO  WORK-REC.                    ECS065
00270                                                                   ECS065
00271      WRITE WORK-REC.                                              ECS065
00272                                                                   ECS065
00273      GO TO 2000-RD-COMM.                                          ECS065
00274                                                                   ECS065
00275  2200-READ-GA.                                                    ECS065
00276      READ WORK-FILE                                               ECS065
00277          AT END                                                   ECS065
00278              GO TO 9990-E-O-J.                                    ECS065
00279                                                                   ECS065
00280      MOVE WORK-REC               TO  COMPENSATION-MASTER.         ECS065
00281                                                                   ECS065
00282      GO TO 2999-BRANCH.                                           ECS065
00283                                                                   ECS065
00284  2800-END-ACCT.                                                   ECS065
00285      CLOSE WORK-FILE.                                             ECS065
00286                                                                   ECS065
00287      OPEN INPUT WORK-FILE.                                        ECS065
00288                                                                   ECS065
00289      ALTER 2000-RD-COMM  TO PROCEED TO  2200-READ-GA.             ECS065
00290                                                                   ECS065
00291      GO TO 2000-RD-COMM.                                          ECS065
00292                                                                   ECS065
00293  2999-BRANCH.                                                     ECS065
00294      GO TO 3000-SELECT-MASTER                                     ECS065
00295            3500-SELECT-ALL                                        ECS065
00296            DEPENDING ON  STMT-SW.                                 ECS065
00297                                                                   ECS065
00298  3000-SELECT-MASTER.                                              ECS065
00299      IF CO-STATEMENT-THIS-RUN                                     ECS065
00300          GO TO 3500-SELECT-ALL.                                   ECS065
00301                                                                   ECS065
00302      GO TO 2000-RD-COMM.                                          ECS065
00303                                                                   ECS065
00304  3500-SELECT-ALL.                                                 ECS065
00305      IF CO-COMPANY-TYPE                                           ECS065
00306          PERFORM 5000-LEFT-RTN  THRU  5199-EXIT.                  ECS065
00307                                                                   ECS065
00308      GO TO 4000-FORMAT-LABEL-RTN.                                 ECS065
00309  EJECT                                                            ECS065
00310  4000-FORMAT-LABEL-RTN.                                           ECS065
00311      ADD K1                      TO  X1.                          ECS065
00312                                                                   ECS065
00313      IF X1  IS GREATER THAN  MAX-NDX                              ECS065
00314          PERFORM 4800-GEN-LABEL-RTN  THRU  4999-EXIT.             ECS065
00315                                                                   ECS065
00316      ADD +1                      TO  NBR-OF-LABELS.               ECS065
CIDMOD                                                                  00002760
022007     IF CO-ACCOUNT NOT = LOW-VALUES
022007        MOVE CO-ACCOUNT          TO  L0-ACCT (X1)
022007     END-IF
CIDMOD                                                                  00002762
CIDMOD*    DISPLAY ' '.                                                 00002769
CIDMOD*    DISPLAY '     L0-ACCT = ' L0-ACCT (X1).                      00002771
CIDMOD                                                                  00002772
051106*    MOVE CO-MAIL-NAME           TO  L1-INFO (X1).                ECS065
051106*    MOVE CO-ACCT-NAME           TO  L2-INFO (X1).                ECS065
010716     IF DTE-CLIENT = 'DCC' or 'VPP'
051106         IF CO-CONTROL-NAME > SPACES
051106             MOVE CO-CONTROL-NAME  TO  L1-INFO (X1)
051106             IF CO-MAIL-NAME > SPACES
051106                 MOVE CO-MAIL-NAME TO  L2-INFO (X1)
051106             ELSE
051106                 MOVE CO-ACCT-NAME TO  L2-INFO (X1)
051106             END-IF
051106         ELSE
051106             MOVE CO-MAIL-NAME     TO  L1-INFO (X1)
051106             MOVE CO-ACCT-NAME     TO  L2-INFO (X1)
051106         END-IF
051106     ELSE
051106         MOVE CO-MAIL-NAME       TO  L1-INFO (X1)
051106         MOVE CO-ACCT-NAME       TO  L2-INFO (X1)
051106     END-IF.
00320                                                                   ECS065
00321      IF DTE-CLIENT = 'UCL'                                        ECS065
00322          NEXT SENTENCE                                            ECS065
00323        ELSE                                                       ECS065
00324          GO TO 4000-CONTINUE.                                     ECS065
00325                                                                   ECS065
00326      MOVE CO-RESP-NO             TO  WK-ACCT-NO.                  ECS065
00327      IF WK-ACCT-1ST = ZEROS                                       ECS065
00328          MOVE SPACES             TO  WK-ACCT-1ST.                 ECS065
00329      MOVE ACCT-WORK-LINE         TO  L1-INFO (X1).                ECS065
00330      MOVE CO-MAIL-NAME           TO  L2-INFO (X1).                ECS065
00331                                                                   ECS065
00332  4000-CONTINUE.                                                   ECS065
00333      MOVE CO-ADDR-1              TO  L3-INFO (X1).                ECS065
00334      MOVE CO-ADDR-2              TO  L4-INFO (X1).                ECS065
051810     MOVE CO-ADDR-CITY           TO  L5-CITY (X1).                ECS065
051810     MOVE CO-ADDR-STATE          TO  L5-ST   (X1)
00336      MOVE CO-ZIP                 TO  L5-ZIP (X1).                 ECS065
00337                                                                   ECS065
00338      IF DTE-CLIENT = 'UCL'                                        ECS065
00339          NEXT SENTENCE                                            ECS065
00340       ELSE                                                        ECS065
051106*    IF CO-MAIL-NAME = CO-ACCT-NAME                               ECS065
051106     IF L1-INFO (X1) = L2-INFO (X1)
00342          MOVE SPACES             TO  L1-INFO (X1).                ECS065
00343                                                                   ECS065
00344      IF DTE-CLIENT = 'UAL'                                        ECS065
051810         MOVE CO-ADDR-CITY       TO WS-CITY
051810         MOVE CO-ADDR-STATE      TO WS-STATE
00346          IF WS-STATE NOT = SPACES                                 ECS065
00347              MOVE WS-STATE       TO L5-ST (X1).                   ECS065
00348                                                                   ECS065
00349      PERFORM 4200-BUMP-RTN  THRU  4299-EXIT  2  TIMES.            ECS065
00350                                                                   ECS065
00351      GO TO 2000-RD-COMM.                                          ECS065
00352  EJECT                                                            ECS065
00353  4200-BUMP-RTN.                                                   ECS065
00354      IF L1-INFO (X1) = SPACES                                     ECS065
00355          MOVE L2-INFO (X1)       TO  L1-INFO (X1)                 ECS065
00356          MOVE SPACES             TO  L2-INFO (X1).                ECS065
00357                                                                   ECS065
00358      IF L2-INFO (X1) = SPACES                                     ECS065
00359          MOVE L3-INFO (X1)       TO  L2-INFO (X1)                 ECS065
00360          MOVE SPACES             TO  L3-INFO (X1).                ECS065
00361                                                                   ECS065
00362      IF L3-INFO (X1) = SPACES                                     ECS065
00363          MOVE L4-INFO (X1)       TO  L3-INFO (X1)                 ECS065
00364          MOVE SPACES             TO  L4-INFO (X1).                ECS065
00365                                                                   ECS065
00366      IF L4-INFO (X1) = SPACES                                     ECS065
00367          MOVE L5-INFO (X1)       TO  L4-INFO (X1)                 ECS065
00368          MOVE SPACES             TO  L5-INFO (X1).                ECS065
00369                                                                   ECS065
00370  4299-EXIT.                                                       ECS065
00371      EXIT.                                                        ECS065
00372  EJECT                                                            ECS065
00373  4800-GEN-LABEL-RTN.                                              ECS065
00374      MOVE SPACE-NP               TO  P-CCSW.                      ECS065
00375                                                                   ECS065
CIDMOD                                                                  00002818
CIDMOD*    DISPLAY ' '                                                  00002819
CIDMOD*    DISPLAY 'L0-ACCT = ' L0-ACCT (X1)                            00002820
CIDMOD     MOVE LINE-0              TO  P-LN                            00002821
CIDMOD     PERFORM 8800-PRT-RTN  THRU  8999-EXIT                        00002822
CIDMOD     MOVE SPACE-1                TO  P-CCSW.                      00002823
CIDMOD                                                                  00002824
CIDMOD                                                                  00002826
CIDMOD     MOVE LABEL-FILLER        TO  P-LN                            00002828
CIDMOD     MOVE SPACE-1                TO  P-CCSW.                      00002829
CIDMOD     PERFORM 8800-PRT-RTN  THRU  8999-EXIT.                       00002830
CIDMOD                                                                  00002831
CIDMOD                                                                  00002832
00376      MOVE LINE-1                 TO  P-LN.                        ECS065
00377                                                                   ECS065
00378      PERFORM 8800-PRT-RTN  THRU  8999-EXIT.                       ECS065
00379                                                                   ECS065
00380      MOVE LINE-2                 TO  P-LN.                        ECS065
00381                                                                   ECS065
00382      PERFORM 8800-PRT-RTN  THRU  8999-EXIT.                       ECS065
00383                                                                   ECS065
00384      MOVE LINE-3                 TO  P-LN.                        ECS065
00385                                                                   ECS065
00386      PERFORM 8800-PRT-RTN  THRU  8999-EXIT.                       ECS065
00387                                                                   ECS065
00388      MOVE LINE-4                 TO  P-LN.                        ECS065
00389                                                                   ECS065
00390      PERFORM 8800-PRT-RTN  THRU  8999-EXIT.                       ECS065
00391                                                                   ECS065
00392      MOVE LINE-5                 TO  P-LN.                        ECS065
00393                                                                   ECS065
00394      PERFORM 8800-PRT-RTN  THRU  8999-EXIT.                       ECS065
00395                                                                   ECS065
00396      ADD +1                      TO  ALIGN-CTR.                   ECS065
00397                                                                   ECS065
00398      IF ALIGN-CTR  IS LESS THAN  MAX-ALIGN-CTR                    ECS065
00399          GO TO 4800-GEN-LABEL-RTN.                                ECS065
00400                                                                   ECS065
00401      MOVE SPACES                 TO  LABEL-AREA.                  ECS065
00402      MOVE K1                     TO  X1.                          ECS065
00403                                                                   ECS065
00404  4999-EXIT.                                                       ECS065
00405      EXIT.                                                        ECS065
00406  EJECT                                                            ECS065
00407  5000-LEFT-RTN.                                                   ECS065
00408      IF CO-ACCT-NAME = SPACES                                     ECS065
00409          GO TO 5199-EXIT.                                         ECS065
00410                                                                   ECS065
00411      MOVE CO-ACCT-NAME           TO  A-NAME.                      ECS065
00412      MOVE SPACES                 TO  B-NAME.                      ECS065
00413      MOVE +1                     TO  A  B.                        ECS065
00414                                                                   ECS065
00415  5050-LOOP-A.                                                     ECS065
00416      IF A-CHAR (A) = SPACES                                       ECS065
00417          ADD +1                  TO  A                            ECS065
00418          GO TO 5050-LOOP-A.                                       ECS065
00419                                                                   ECS065
00420  5100-LOOP-B.                                                     ECS065
00421      MOVE A-CHAR (A)             TO  B-CHAR (B).                  ECS065
00422                                                                   ECS065
00423      ADD +1                      TO  A.                           ECS065
00424                                                                   ECS065
00425      IF A  IS GREATER THAN  +30                                   ECS065
00426          GO TO 5150-MOVE-NAME.                                    ECS065
00427                                                                   ECS065
00428      ADD +1                      TO  B.                           ECS065
00429                                                                   ECS065
00430      GO TO 5100-LOOP-B.                                           ECS065
00431                                                                   ECS065
00432  5150-MOVE-NAME.                                                  ECS065
00433      MOVE B-NAME                 TO  CO-ACCT-NAME.                ECS065
00434                                                                   ECS065
00435  5199-EXIT.                                                       ECS065
00436      EXIT.                                                        ECS065
00437  EJECT                                                            ECS065
00438  8800-PRT-RTN.                                                    ECS065
00439      MOVE P-CCSW                 TO  X  P-CTL.                    ECS065
00440      MOVE P-LN                   TO  P-DATA.                      ECS065
00441      MOVE SPACE-1                TO  P-REC.                       ECS065
00442                                                                   ECS065
00443  8900-PRT-COPY.                                                   ECS065
00444                              COPY ELCPRT2.                        ECS065
00445                                                                   ECS065
00446  8999-EXIT.                                                       ECS065
00447      EXIT.                                                        ECS065
00448  EJECT                                                            ECS065
00449  9990-E-O-J.                                                      ECS065
00450      IF NBR-OF-LABELS = +0                                        ECS065
00451          GO TO 9995-CLOSE-FILES.                                  ECS065
00452                                                                   ECS065
00453      PERFORM 4800-GEN-LABEL-RTN  THRU  4999-EXIT.                 ECS065
00454 *                                                                 ECS065
00455 *                                                                 ECS065
00456 * LEAVE THE ABOVE 2 LINE IN TO PREVENT LOSING THE LINE ABOVE      ECS065
00464                                                                   ECS065
00465  9995-CLOSE-FILES.                                                ECS065
00466      CLOSE COMM-MSTR-IN                                           ECS065
00467            WORK-FILE                                              ECS065
00468            PRNTR.                                                 ECS065
00469                                                                   ECS065
00470  9996-CLOSE-FICH.                                                 ECS065
00471                              COPY ELCPRTC.                        ECS065
00472                                                                   ECS065
00473      GOBACK.                                                      ECS065
00474                                                                   ECS065
00475  ABEND-PGM SECTION.                                               ECS065
00476                              COPY ELCABEND.                       ECS065
