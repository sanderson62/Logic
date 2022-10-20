00001  IDENTIFICATION DIVISION.                                         03/27/98
00002                                                                   ECS069
00003  PROGRAM-ID.                ECS069.                                  LV003
00004 *              PROGRAM CONVERTED BY                               ECS069
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ECS069
00006 *              CONVERSION DATE 02/08/96 18:24:54.                 ECS069
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          ECS069
00008 *                           VMOD=2.009.                           ECS069
00009                                                                   ECS069
00010 *AUTHOR.        LOGIC, INC.                                       ECS069
00011 *               DALLAS, TEXAS.                                    ECS069
00012                                                                   ECS069
00013 *DATE-COMPILED.                                                   ECS069
00014                                                                   ECS069
00015 *SECURITY.   *****************************************************ECS069
00016 *            *                                                   *ECS069
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ECS069
00018 *            *                                                   *ECS069
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS069
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ECS069
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *ECS069
00022 *            *                                                   *ECS069
00023 *            *****************************************************ECS069
00024                                                                   ECS069
00025 *REMARKS.                                                         ECS069
00026 *        THIS PROGRAM PRINTS THE COMPENSATION MASTER.             ECS069
00027                                                                   ECS069
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
00028  ENVIRONMENT DIVISION.                                            ECS069
00029  INPUT-OUTPUT SECTION.                                            ECS069
00030  FILE-CONTROL.                                                    ECS069
00031                                                                   ECS069
00032      SELECT PRNTR            ASSIGN TO SYS008-UR-1403-S-SYS008.   ECS069
00033      SELECT COMM-MSTR-IN     ASSIGN TO SYS010-UT-2400-S-SYS010.   ECS069
00034      SELECT DISK-DATE        ASSIGN TO SYS019-UT-FBA1-S-SYS019.   ECS069
00035      SELECT FICH             ASSIGN TO SYS020-UT-2400-S-SYS020.   ECS069
00036  EJECT                                                            ECS069
00037  DATA DIVISION.                                                   ECS069
00038  FILE SECTION.                                                    ECS069
00039                                                                   ECS069
00040  FD  PRNTR                                                        ECS069
00041                              COPY ELCPRTFD.                       ECS069
00042  EJECT                                                            ECS069
00043  FD  COMM-MSTR-IN                                                 ECS069
00044                              COPY ECSCOIFD.                       ECS069
00045  EJECT                                                            ECS069
00046  FD  DISK-DATE                                                    ECS069
00047                              COPY ELCDTEFD.                       ECS069
00048  EJECT                                                            ECS069
00049  FD  FICH                                                         ECS069
00050                              COPY ELCFCHFD.                       ECS069
00051  EJECT                                                            ECS069
00052  WORKING-STORAGE SECTION.                                         ECS069
00053  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      ECS069
00054  77  FILLER PIC X(32) VALUE '********************************'.   ECS069
00055  77  FILLER PIC X(32) VALUE '*            ECS069            *'.   ECS069
00056  77  FILLER PIC X(32) VALUE '*********** VMOD=2.009 *********'.   ECS069
00057                                                                   ECS069
00058  77  PGM-SUB                 PIC S9(3)       VALUE +69  COMP-3.   ECS069
00059  77  SPACE-NP                PIC  X          VALUE '1'.           ECS069
00060  77  SPACE-1                 PIC  X          VALUE ' '.           ECS069
00061  77  SPACE-2                 PIC  X          VALUE '0'.           ECS069
00062  77  SPACE-3                 PIC  X          VALUE '-'.           ECS069
00063  77  X                       PIC  X          VALUE '1'.           ECS069
00064                                                                   ECS069
00065  01  REQUIRED-STORAGE.                                            ECS069
00066      12  WS-RETURN-CODE          PIC S9(4)              COMP.     ECS069
00067      12  WS-ABEND-MESSAGE        PIC  X(80).                      ECS069
00068      12  WS-ABEND-FILE-STATUS    PIC  X(2)   VALUE ZEROS.         ECS069
00069      12  WS-ZERO                 PIC S9      VALUE ZERO COMP-3.   ECS069
00070      12  CMR-DATE.                                                ECS069
00071          16  CMR-MONTH           PIC  9(2).                       ECS069
00072          16  CMR-DAY             PIC  9(2).                       ECS069
00073          16  CMR-YEAR            PIC  9(2).                       ECS069
00074  EJECT                                                            ECS069
00075  01  MISC-WORKING-STORAGE.                                        ECS069
00076      12  COMP-3-AREA     COMP-3.                                  ECS069
00077          16  PGCTR           PIC S9(5)           VALUE +0.        ECS069
00078          16  LNCTR           PIC S9(3)           VALUE +66.       ECS069
00079          16  WORK-AMT        PIC S9(9)V9(2)      VALUE +0.        ECS069
00080      12  SUB-TOTALS      COMP-3.                                  ECS069
00081          16  S-CTR           PIC S9(9)           VALUE +0.        ECS069
00082          16  S-BAL           PIC S9(9)V9(2).                      ECS069
00083          16  S-ADJ           PIC S9(9)V9(2).                      ECS069
00084          16  S-YTD           PIC S9(9)V9(2).                      ECS069
00085      12  CARRIER-TOTALS  COMP-3.                                  ECS069
00086          16  C-CTR           PIC S9(9)           VALUE +0.        ECS069
00087          16  C-BAL           PIC S9(9)V9(2).                      ECS069
00088          16  C-ADJ           PIC S9(9)V9(2).                      ECS069
00089          16  C-YTD           PIC S9(9)V9(2).                      ECS069
00090      12  FINAL-TOTALS    COMP-3.                                  ECS069
00091          16  F-CTR           PIC S9(9)           VALUE +0.        ECS069
00092          16  F-BAL           PIC S9(9)V9(2).                      ECS069
00093          16  F-ADJ           PIC S9(9)V9(2).                      ECS069
00094          16  F-YTD           PIC S9(9)V9(2).                      ECS069
00095      12  CUR-CARR-GROUP.                                          ECS069
00096          16  CUR-CARR        PIC  X.                              ECS069
00097          16  CUR-GROUP       PIC  X(6).                           ECS069
00098      12  SAVE-COMPANY-NAME   PIC  X(30).                          ECS069
00099  EJECT                                                            ECS069
00100  01  HD1.                                                         ECS069
00101      12  FILLER              PIC  X(44)      VALUE SPACES.        ECS069
00102      12  FILLER              PIC  X(44)      VALUE                ECS069
00103              '        COMPENSATION MASTER LISTING         '.      ECS069
00104      12  FILLER              PIC  X(31)      VALUE SPACES.        ECS069
00105      12  FILLER              PIC  X(8)       VALUE 'ECS069'.      ECS069
00106                                                                   ECS069
00107  01  HD2.                                                         ECS069
00108      12  FILLER              PIC  X(51)      VALUE SPACES.        ECS069
00109      12  HD-CO               PIC  X(30).                          ECS069
00110      12  FILLER              PIC  X(38)      VALUE SPACES.        ECS069
00111      12  HD-RUN-DT           PIC  X(8)       VALUE SPACES.        ECS069
00112                                                                   ECS069
00113  01  HD3.                                                         ECS069
00114      12  FILLER              PIC  X(57)      VALUE SPACES.        ECS069
00115      12  HD-DT               PIC  X(18).                          ECS069
00116      12  FILLER              PIC  X(44)      VALUE SPACES.        ECS069
00117      12  FILLER              PIC  X(5)       VALUE 'PAGE '.       ECS069
00118      12  HD-PG               PIC ZZ,ZZ9.                          ECS069
00119                                                                   ECS069
00120  01  HD4.                                                         ECS069
00121      12  FILLER              PIC  X(19)      VALUE                ECS069
00122              'CARRIER GROUPING - '.                               ECS069
00123      12  HD-CARR             PIC  X          VALUE SPACES.        ECS069
00124      12  HD-GROUP            PIC  X(6)       VALUE SPACES.        ECS069
00125      12  FILLER              PIC  X(3)       VALUE ' - '.         ECS069
00126      12  HD-NAME             PIC  X(30)      VALUE SPACES.        ECS069
00127                                                                   ECS069
00128  01  HD5.                                                         ECS069
00129      12  FILLER              PIC  X(44)      VALUE                ECS069
00130              ' FINAN. RESPON AGT CAR MAILING              '.      ECS069
00131      12  FILLER              PIC  X(44)      VALUE                ECS069
00132              '          ACCOUNT                        LAS'.      ECS069
00133      12  FILLER              PIC  X(44)      VALUE                ECS069
00134              'T     CURRENT    YEAR TO DATE  YEAR TO DATE '.      ECS069
00135                                                                   ECS069
00136  01  HD6.                                                         ECS069
00137      12  FILLER              PIC  X(44)      VALUE                ECS069
00138              ' /ACCT. NUMBER TYP BAL ADDRESS              '.      ECS069
00139      12  FILLER              PIC  X(44)      VALUE                ECS069
00140              '          NAME                           MAI'.      ECS069
00141      12  FILLER              PIC  X(44)       VALUE               ECS069
00142              'NT    BALANCE    ADJUSTMENTS   COMPENSATION '.      ECS069
00143  EJECT                                                            ECS069
00144  01  P-REC.                                                       ECS069
00145      12  P-CCSW              PIC  X.                              ECS069
00146      12  P-LINE.                                                  ECS069
00147          16 FILLER           PIC  X(132).                         ECS069
00148      12  P-LINE-1  REDEFINES  P-LINE.                             ECS069
00149          16  FILLER          PIC  X(3).                           ECS069
00150          16  P-RESP          PIC  X(10).                          ECS069
00151          16  P-ACCT  REDEFINES                                    ECS069
00152              P-RESP          PIC  X(10).                          ECS069
00153          16  FILLER          PIC  X(3).                           ECS069
00154          16  P-TYPE          PIC  X.                              ECS069
00155          16  FILLER          PIC  X(3).                           ECS069
00156          16  P-CARY          PIC  X.                              ECS069
00157          16  FILLER          PIC  X(2).                           ECS069
00158          16  P-ADDR          PIC  X(30).                          ECS069
00159          16  FILLER          PIC  X.                              ECS069
00160          16  P-NAME.                                              ECS069
00161              20  P-TELFONE-SOC-SEC.                               ECS069
00162                  24  P-AREA  PIC  X(3).                           ECS069
00163                  24  P-DSH1  PIC  X.                              ECS069
00164                  24  P-PREF  PIC  X(3).                           ECS069
00165                  24  P-DSH2  PIC  X.                              ECS069
00166                  24  P-FONE  PIC  X(4).                           ECS069
00167                  24  FILLER  PIC  X(3).                           ECS069
00168                  24  P-SOC   PIC  X(13).                          ECS069
00169                  24  FILLER  PIC  X(2).                           ECS069
00170              20  P-ZIP-R  REDEFINES  P-TELFONE-SOC-SEC.           ECS069
00171                  24  P-ZIP   PIC  X(9).                           ECS069
00172                  24  FILLER  PIC  X(21).                          ECS069
00173          16  FILLER          PIC  X.                              ECS069
00174          16  P-MO            PIC  X(2).                           ECS069
00175          16  P-DSH3          PIC  X.                              ECS069
00176          16  P-YR            PIC  X(2).                           ECS069
00177          16  FILLER          PIC  X.                              ECS069
00178          16  P-BAL           PIC Z,ZZZ,ZZZ.ZZ-.                   ECS069
00179          16  P-ADJ           PIC ZZ,ZZZ,ZZZ.ZZ-.                  ECS069
00180          16  P-YTD           PIC ZZ,ZZZ,ZZZ.ZZ-.                  ECS069
00181      12  P-LINE-2  REDEFINES  P-LINE.                             ECS069
00182          16  FILLER          PIC  X(18).                          ECS069
00183          16  P-CAR-GROUP     PIC  X(7).                           ECS069
00184          16  FILLER          PIC  X.                              ECS069
00185          16  P-DESC          PIC  X(30).                          ECS069
00186          16  FILLER          PIC  X(20).                          ECS069
00187          16  P-CTR           PIC ZZZ,ZZZ,ZZZ-.                    ECS069
00188          16  FILLER          PIC  X.                              ECS069
00189          16  P-BAL-X         PIC ZZZ,ZZZ,ZZZ.ZZ-.                 ECS069
00190          16  FILLER          PIC  X(13).                          ECS069
00191          16  P-YTD-X         PIC ZZZ,ZZZ,ZZZ.ZZ-.                 ECS069
00192      12  P-LINE-3  REDEFINES  P-LINE.                             ECS069
00193          16  FILLER          PIC  X(103).                         ECS069
00194          16  P-ADJ-X         PIC ZZZ,ZZZ,ZZZ.ZZ-.                 ECS069
00195          16  FILLER          PIC  X(14).                          ECS069
00196  EJECT                                                            ECS069
00197                              COPY ERCCOMP.                        ECS069
00198  EJECT                                                            ECS069
00199                              COPY ELCDATE.                        ECS069
00200  EJECT                                                            ECS069
00201                              COPY ELCDTECX.                       ECS069
00202  EJECT                                                            ECS069
00203                              COPY ELCDTEVR.                       ECS069
00204  EJECT                                                            ECS069
00205  PROCEDURE DIVISION.                                              ECS069
00206                                                                   ECS069
00207  0000-STANDARD-RTN.                                               ECS069
00208                              COPY ELCDTERX.                       ECS069
00209  EJECT                                                            ECS069
00210  1000-INITIALIZE-OUTPUT.                                          ECS069
00211                                                                      CL**3
00212      IF DTE-FICH = SPACE                                             CL**3
00213          DISPLAY ' FICH IS SPACE '                                   CL**3
00214      ELSE                                                            CL**3
00215          DISPLAY ' IFCH IS ' DTE-FICH.                               CL**3
00216                                                                      CL**3
00217      OPEN INPUT   COMM-MSTR-IN                                    ECS069
00218           OUTPUT  PRNTR.                                          ECS069
00219                                                                   ECS069
00220      MOVE LOW-VALUE              TO  COMPENSATION-MASTER          ECS069
00221                                      COMP-IN-RECORD               ECS069
00222                                      CUR-CARR-GROUP.              ECS069
00223      MOVE COMPANY-NAME           TO  HD-CO.                       ECS069
00224      MOVE SPACES                 TO  SAVE-COMPANY-NAME.           ECS069
00225      MOVE ALPH-DATE              TO  HD-DT.                       ECS069
00226      MOVE WS-CURRENT-DATE        TO  HD-RUN-DT.                   ECS069
00227      MOVE SPACE-NP               TO  P-REC.                       ECS069
00228      MOVE +066                   TO  LNCTR.                       ECS069
00229                                                                   ECS069
00230      GO TO 2000-PROCESS-RTN.                                      ECS069
00231  EJECT                                                            ECS069
00232  2000-PROCESS-RTN.                                                ECS069
00233      PERFORM 8000-MSTR-CONTROL-RTN  THRU  8099-EXIT.              ECS069
00234                                                                   ECS069
00235      IF CO-CARR-GROUP NOT = CUR-CARR-GROUP                        ECS069
00236          PERFORM 6000-BREAK-RTN  THRU  6999-EXIT.                 ECS069
00237                                                                   ECS069
00238      IF CO-CARR-GROUP = HIGH-VALUE                                ECS069
00239          GO TO 9990-E-O-J.                                        ECS069
00240                                                                   ECS069
00241      IF CO-COMPANY-TYPE                                           ECS069
00242          MOVE CO-ACCT-NAME       TO  SAVE-COMPANY-NAME            ECS069
00243          MOVE +066               TO  LNCTR.                       ECS069
00244                                                                   ECS069
00245      IF LNCTR  IS GREATER THAN  +056                              ECS069
00246          PERFORM 8600-HD-RTN  THRU  8699-EXIT.                    ECS069
00247                                                                   ECS069
00248      IF CO-RESP-NO NOT = LOW-VALUE                                ECS069
00249          MOVE CO-RESP-NO         TO  P-RESP.                      ECS069
00250                                                                   ECS069
00251      IF CO-YTD-OVR-UNDR  IS NOT NUMERIC                           ECS069
00252          MOVE ZEROS              TO  CO-YTD-OVR-UNDR.             ECS069
00253                                                                   ECS069
00254      MOVE CO-TYPE                TO  P-TYPE.                      ECS069
00255      MOVE CO-BALANCE-CONTROL     TO  P-CARY.                      ECS069
00256      MOVE CO-MAIL-NAME           TO  P-ADDR.                      ECS069
00257      MOVE CO-ACCT-NAME           TO  P-NAME.                      ECS069
00258      MOVE CO-LAST-MAINT-DT       TO  DC-BIN-DATE-1.               ECS069
00259      MOVE ' '                    TO  DC-OPTION-CODE.              ECS069
00260                                                                   ECS069
00261      PERFORM 8500-DATE-CONVERSION  THRU  8599-EXIT.               ECS069
00262                                                                   ECS069
00263      IF DATE-CONVERSION-ERROR                                     ECS069
00264          MOVE '??'               TO  P-MO                         ECS069
00265          MOVE '??'               TO  P-YR                         ECS069
00266      ELSE                                                         ECS069
00267          MOVE DC-GREG-DATE-1-MDY TO  CMR-DATE                     ECS069
00268          MOVE CMR-MONTH          TO  P-MO                         ECS069
00269          MOVE CMR-YEAR           TO  P-YR.                        ECS069
00270                                                                   ECS069
00271 ***************************************************************** ECS069
00272 *    APPLIES TO A/R USERS.                                        ECS069
00273 *    TOTAL OPTION ALLOWS PROGRAM TO USE EITHER THE AMOUNTS AS     ECS069
00274 *    OF THE LAST MONTH END OR AS OF THE LAST CYCLE (OPTION 2).    ECS069
00275 ***************************************************************** ECS069
00276                                                                   ECS069
00277      IF DTE-SYS-G-AR-USED NOT = '1'                               ECS069
00278          MOVE CO-END-BAL         TO  P-BAL                        ECS069
00279          MOVE CO-YTD-OVR-UNDR    TO  P-ADJ                        ECS069
00280          COMPUTE WORK-AMT  =  CO-YTD-OV  +  CO-YTD-COM            ECS069
00281          ADD CO-END-BAL      TO  S-BAL                            ECS069
00282          ADD CO-YTD-OVR-UNDR TO  S-ADJ                            ECS069
00283          ADD CO-YTD-OV       TO  S-YTD                            ECS069
00284          ADD CO-YTD-COM      TO  S-YTD                            ECS069
00285      ELSE                                                         ECS069
00286          IF DTE-TOT-OPT = '1'                                     ECS069
00287              MOVE CO-END-BAL     TO  P-BAL                        ECS069
00288              MOVE CO-YTD-OVR-UNDR                                 ECS069
00289                                  TO  P-ADJ                        ECS069
00290              COMPUTE WORK-AMT  =  CO-YTD-OV  +  CO-YTD-COM        ECS069
00291              ADD CO-END-BAL      TO  S-BAL                        ECS069
00292              ADD CO-YTD-OVR-UNDR TO  S-ADJ                        ECS069
00293              ADD CO-YTD-OV       TO  S-YTD                        ECS069
00294              ADD CO-YTD-COM      TO  S-YTD                        ECS069
00295          ELSE                                                     ECS069
00296              MOVE CO-CURRENT-END-BAL                              ECS069
00297                                  TO  P-BAL                        ECS069
00298              MOVE CO-YTD-OVR-UNDR                                 ECS069
00299                                  TO  P-ADJ                        ECS069
00300              COMPUTE WORK-AMT  =  CO-CURRENT-YTD-OV               ECS069
00301                                +  CO-CURRENT-YTD-COM              ECS069
00302              ADD CO-CURRENT-END-BAL      TO  S-BAL                ECS069
00303              ADD CO-YTD-OVR-UNDR         TO  S-ADJ                ECS069
00304              ADD CO-CURRENT-YTD-OV       TO  S-YTD                ECS069
00305              ADD CO-CURRENT-YTD-COM      TO  S-YTD.               ECS069
00306                                                                   ECS069
00307      ADD +1                      TO  S-CTR.                       ECS069
00308      MOVE WORK-AMT               TO  P-YTD.                       ECS069
00309                                                                   ECS069
00310      MOVE '-'                    TO  P-DSH3.                      ECS069
00311      MOVE SPACE-2                TO  P-CCSW.                      ECS069
00312                                                                   ECS069
00313      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       ECS069
00314                                                                   ECS069
00315      IF CO-ACCOUNT NOT = LOW-VALUE                                ECS069
00316          MOVE CO-ACCOUNT         TO  P-ACCT.                      ECS069
00317                                                                   ECS069
00318      MOVE CO-ADDR-1              TO  P-ADDR.                      ECS069
00319      MOVE CO-AREA-CODE           TO  P-AREA.                      ECS069
00320      MOVE CO-PREFIX              TO  P-PREF.                      ECS069
00321      MOVE CO-PHONE               TO  P-FONE.                      ECS069
00322      MOVE '-'                    TO  P-DSH1  P-DSH2.              ECS069
00323      MOVE CO-SOC-SEC             TO  P-SOC.                       ECS069
00324                                                                   ECS069
00325      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       ECS069
00326                                                                   ECS069
00327      IF CO-ADDR-2 NOT = SPACES                                    ECS069
00328          MOVE CO-ADDR-2          TO  P-ADDR                       ECS069
00329          PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                   ECS069
00330                                                                   ECS069
031810     MOVE SPACES                 TO  P-ADDR
031810     STRING CO-ADDR-CITY ' ' CO-ADDR-STATE
031810        DELIMITED BY '  ' INTO P-ADDR
031810     END-STRING
00332      MOVE CO-ZIP                 TO  P-ZIP.                       ECS069
00333                                                                   ECS069
00334      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       ECS069
00335                                                                   ECS069
00336      GO TO 2000-PROCESS-RTN.                                      ECS069
00337  EJECT                                                            ECS069
00338  6000-BREAK-RTN.                                                  ECS069
00339      IF CUR-CARR-GROUP = LOW-VALUE                                ECS069
00340          GO TO 6500-INITIALIZE-ALL.                               ECS069
00341                                                                   ECS069
00342  6100-COMPANY-BREAK.                                              ECS069
00343      MOVE CUR-CARR-GROUP         TO  P-CAR-GROUP.                 ECS069
00344      MOVE ' TOTALS'              TO  P-DESC.                      ECS069
00345      MOVE S-CTR                  TO  P-CTR.                       ECS069
00346      MOVE S-BAL                  TO  P-BAL-X.                     ECS069
00347      MOVE S-YTD                  TO  P-YTD-X.                     ECS069
00348      MOVE SPACE-2                TO  P-CCSW.                      ECS069
00349                                                                   ECS069
00350      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       ECS069
00351                                                                   ECS069
00352      MOVE S-ADJ                  TO  P-ADJ-X.                     ECS069
00353      MOVE SPACE-1                TO  P-CCSW.                      ECS069
00354                                                                   ECS069
00355      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       ECS069
00356                                                                   ECS069
00357      ADD S-CTR                   TO  C-CTR.                       ECS069
00358      ADD S-BAL                   TO  C-BAL.                       ECS069
00359      ADD S-ADJ                   TO  C-ADJ.                       ECS069
00360      ADD S-YTD                   TO  C-YTD.                       ECS069
00361                                                                   ECS069
00362  6200-CARRIER-BREAK.                                              ECS069
00363      IF CO-CARRIER = CUR-CARR                                     ECS069
00364          GO TO 6700-INITIALIZE-COMPANY.                           ECS069
00365                                                                   ECS069
00366      MOVE SPACES                 TO  CUR-GROUP.                   ECS069
00367                                                                   ECS069
00368      PERFORM 8600-HD-RTN  THRU  8699-EXIT.                        ECS069
00369                                                                   ECS069
00370      MOVE CUR-CARR-GROUP         TO  P-CAR-GROUP.                 ECS069
00371      MOVE ' TOTALS'              TO  P-DESC.                      ECS069
00372      MOVE C-CTR                  TO  P-CTR.                       ECS069
00373      MOVE C-BAL                  TO  P-BAL-X.                     ECS069
00374      MOVE C-YTD                  TO  P-YTD-X.                     ECS069
00375      MOVE SPACE-2                TO  P-CCSW.                      ECS069
00376                                                                   ECS069
00377      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       ECS069
00378                                                                   ECS069
00379      MOVE C-ADJ                  TO  P-ADJ-X.                     ECS069
00380      MOVE SPACE-1                TO  P-CCSW.                      ECS069
00381                                                                   ECS069
00382      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       ECS069
00383                                                                   ECS069
00384      ADD C-CTR                   TO  F-CTR.                       ECS069
00385      ADD C-BAL                   TO  F-BAL.                       ECS069
00386      ADD C-ADJ                   TO  F-ADJ.                       ECS069
00387      ADD C-YTD                   TO  F-YTD.                       ECS069
00388                                                                   ECS069
00389  6030-FINAL-BREAK.                                                ECS069
00390      IF CO-CARR-GROUP NOT = HIGH-VALUE                            ECS069
00391          GO TO 6600-INITIALIZE-CARRIER.                           ECS069
00392                                                                   ECS069
00393      MOVE SPACES                 TO  CUR-CARR.                    ECS069
00394      MOVE 'FINAL TOTALS'         TO  SAVE-COMPANY-NAME.           ECS069
00395                                                                   ECS069
00396      PERFORM 8600-HD-RTN  THRU  8699-EXIT.                        ECS069
00397                                                                   ECS069
00398      MOVE 'FINAL TOTALS'         TO  P-DESC.                      ECS069
00399      MOVE F-CTR                  TO  P-CTR.                       ECS069
00400      MOVE F-BAL                  TO  P-BAL-X.                     ECS069
00401      MOVE F-YTD                  TO  P-YTD-X.                     ECS069
00402                                                                   ECS069
00403      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       ECS069
00404                                                                   ECS069
00405      MOVE F-ADJ                  TO  P-ADJ-X.                     ECS069
00406      MOVE SPACE-1                TO  P-CCSW.                      ECS069
00407                                                                   ECS069
00408      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       ECS069
00409                                                                   ECS069
00410      GO TO 6999-EXIT.                                             ECS069
00411                                                                   ECS069
00412  6500-INITIALIZE-ALL.                                             ECS069
00413      MOVE +0                     TO  F-CTR  F-BAL  F-ADJ  F-YTD.  ECS069
00414      MOVE SPACES                 TO  SAVE-COMPANY-NAME.           ECS069
00415                                                                   ECS069
00416  6600-INITIALIZE-CARRIER.                                         ECS069
00417      MOVE CO-CARRIER             TO  CUR-CARR.                    ECS069
00418      MOVE +0                     TO  C-CTR  C-BAL  C-ADJ  C-YTD.  ECS069
00419      MOVE SPACES                 TO  SAVE-COMPANY-NAME.           ECS069
00420                                                                   ECS069
00421  6700-INITIALIZE-COMPANY.                                         ECS069
00422      MOVE CO-GROUPING            TO  CUR-GROUP.                   ECS069
00423      MOVE +0                     TO  S-CTR  S-BAL  S-ADJ  S-YTD.  ECS069
00424      MOVE +066                   TO  LNCTR.                       ECS069
00425                                                                   ECS069
00426  6999-EXIT.                                                       ECS069
00427      EXIT.                                                        ECS069
00428  EJECT                                                            ECS069
00429  8000-MSTR-CONTROL-RTN.                                           ECS069
00430      READ COMM-MSTR-IN                                            ECS069
00431          AT END                                                   ECS069
00432              MOVE HIGH-VALUE     TO  COMP-IN-RECORD               ECS069
00433                                      COMPENSATION-MASTER          ECS069
00434              GO TO 8099-EXIT.                                     ECS069
00435                                                                   ECS069
00436      MOVE COMP-IN-RECORD         TO  COMPENSATION-MASTER.         ECS069
00437                                                                   ECS069
00438      IF DTE-CLIENT NOT = 'NCL'                                    ECS069
00439          GO TO 8099-EXIT.                                         ECS069
00440                                                                   ECS069
00441      IF CO-CARR-GROUP = '9999999'                                 ECS069
00442          GO TO 8000-MSTR-CONTROL-RTN.                             ECS069
00443      IF CO-END-BAL    =  ZERO                                     ECS069
00444          GO TO 8000-MSTR-CONTROL-RTN.                             ECS069
00445                                                                   ECS069
00446  8099-EXIT.                                                       ECS069
00447      EXIT.                                                        ECS069
00448  EJECT                                                            ECS069
00449  8500-DATE-CONVERSION.                                            ECS069
00450                              COPY ELCDCS.                         ECS069
00451                                                                   ECS069
00452  8599-EXIT.                                                       ECS069
00453      EXIT.                                                        ECS069
00454  EJECT                                                            ECS069
00455  8600-HD-RTN.                                                     ECS069
00456      MOVE HD1                    TO  P-LINE.                      ECS069
00457      MOVE SPACE-NP               TO  P-CCSW.                      ECS069
00458                                                                   ECS069
00459      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       ECS069
00460                                                                   ECS069
00461      ADD +1                      TO  PGCTR.                       ECS069
00462                                                                   ECS069
00463      MOVE PGCTR                  TO  HD-PG.                       ECS069
00464      MOVE HD2                    TO  P-LINE.                      ECS069
00465      MOVE SPACE-1                TO  P-CCSW.                      ECS069
00466                                                                   ECS069
00467      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       ECS069
00468                                                                   ECS069
00469      MOVE HD3                    TO  P-LINE.                      ECS069
00470      MOVE SPACE-1                TO  P-CCSW.                      ECS069
00471                                                                   ECS069
00472      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       ECS069
00473                                                                   ECS069
00474      MOVE SPACE-2                TO  P-CCSW.                      ECS069
00475      MOVE CUR-CARR               TO  HD-CARR.                     ECS069
00476      MOVE CUR-GROUP              TO  HD-GROUP.                    ECS069
00477      MOVE SAVE-COMPANY-NAME      TO  HD-NAME.                     ECS069
00478      MOVE HD4                    TO  P-LINE.                      ECS069
00479                                                                   ECS069
00480      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       ECS069
00481                                                                   ECS069
00482      MOVE SPACE-2                TO  P-CCSW.                      ECS069
00483      MOVE HD5                    TO  P-LINE.                      ECS069
00484                                                                   ECS069
00485      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       ECS069
00486                                                                   ECS069
00487      MOVE HD6                    TO  P-LINE.                      ECS069
00488                                                                   ECS069
00489      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       ECS069
00490                                                                   ECS069
00491      MOVE +9                     TO  LNCTR.                       ECS069
00492      MOVE SPACE-2                TO  P-CCSW.                      ECS069
00493                                                                   ECS069
00494  8699-EXIT.                                                       ECS069
00495      EXIT.                                                        ECS069
00496                                                                   ECS069
00497  8800-PRT-RTN.                                                    ECS069
00498      MOVE P-REC                  TO  PRT.                         ECS069
00499      MOVE P-CCSW                 TO  X.                           ECS069
00500                                                                   ECS069
00501      IF P-CCSW = SPACE-1                                          ECS069
00502          ADD +1                  TO  LNCTR                        ECS069
00503      ELSE                                                         ECS069
00504          IF P-CCSW = SPACE-2                                      ECS069
00505              ADD +2              TO  LNCTR                        ECS069
00506          ELSE                                                     ECS069
00507              IF P-CCSW = SPACE-3                                  ECS069
00508                  ADD +3          TO  LNCTR.                       ECS069
00509                                                                   ECS069
00510      MOVE SPACES                 TO  P-REC.                       ECS069
00511      MOVE SPACE-1                TO  P-CCSW.                      ECS069
00512                                                                   ECS069
00513  8850-COPY-PRT-RTN.                                               ECS069
00514                              COPY ELCPRT2.                        ECS069
00515                                                                   ECS069
00516  8899-EXIT.                                                       ECS069
00517      EXIT.                                                        ECS069
00518  EJECT                                                            ECS069
00519  9990-E-O-J.                                                      ECS069
00520                              COPY ELCPRTC.                        ECS069
00521                                                                   ECS069
00522  9995-CLOSE.                                                      ECS069
00523      CLOSE COMM-MSTR-IN                                           ECS069
00524            PRNTR.                                                 ECS069
00525                                                                   ECS069
00526  9999-STOP-RUN.                                                   ECS069
00527      GOBACK.                                                      ECS069
00528                                                                   ECS069
00529  ABEND-PGM SECTION.                                               ECS069
00530                              COPY ELCABEND.                       ECS069
