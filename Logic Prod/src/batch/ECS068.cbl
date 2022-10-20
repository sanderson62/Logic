00001  IDENTIFICATION DIVISION.                                         03/10/98
00002                                                                   ECS068
00003  PROGRAM-ID.                ECS068.                                  LV002
00004 *              PROGRAM CONVERTED BY                               ECS068
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ECS068
00006 *              CONVERSION DATE 02/08/96 18:23:25.                 ECS068
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          ECS068
00008 *                           VMOD=2.004.                           ECS068
00009                                                                   ECS068
00010 *AUTHOR.        LOGIC, INC.                                       ECS068
00011 *               DALLAS, TEXAS.                                    ECS068
00012                                                                   ECS068
00013 *DATE-COMPILED.                                                   ECS068
00014                                                                   ECS068
00015 *SECURITY.   *****************************************************ECS068
00016 *            *                                                   *ECS068
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ECS068
00018 *            *                                                   *ECS068
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS068
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ECS068
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *ECS068
00022 *            *                                                   *ECS068
00023 *            *****************************************************ECS068
00024                                                                   ECS068
00025 *REMARKS.                                                         ECS068
00026 *        PRINTS MAILING LABELS FROM THE ACCOUNT MASTER.           ECS068
00027 *                                                                 ECS068
00028  EJECT                                                            ECS068
00029  ENVIRONMENT DIVISION.                                            ECS068
00030  CONFIGURATION SECTION.                                           ECS068
00031  SPECIAL-NAMES.                                                   ECS068
00032      C02 IS LCP-CH2                                               ECS068
00033      C03 IS LCP-CH3                                               ECS068
00034      C04 IS LCP-CH4                                               ECS068
00035      C05 IS LCP-CH5                                               ECS068
00036      C06 IS LCP-CH6                                               ECS068
00037      C07 IS LCP-CH7                                               ECS068
00038      C08 IS LCP-CH8                                               ECS068
00039      C09 IS LCP-CH9                                               ECS068
00040      C10 IS LCP-CH10                                              ECS068
00041      C11 IS LCP-CH11                                              ECS068
00042      C12 IS LCP-CH12                                              ECS068
00043      S01 IS LCP-P01                                               ECS068
00044      S02 IS LCP-P02.                                              ECS068
00045  INPUT-OUTPUT SECTION.                                            ECS068
00046  FILE-CONTROL.                                                    ECS068
00047                                                                   ECS068
00048      SELECT PRNTR            ASSIGN TO SYS008-UR-1403-S-SYS008.   ECS068
00049      SELECT ACCT-MSTR-IN     ASSIGN TO SYS010-UT-2400-S-SYS010.   ECS068
00050      SELECT DISK-DATE        ASSIGN TO SYS019-UT-FBA1-S-SYS019.   ECS068
00051  EJECT                                                            ECS068
00052  DATA DIVISION.                                                   ECS068
00053  FILE SECTION.                                                    ECS068
00054                                                                   ECS068
00055  FD  PRNTR                                                        ECS068
00056      COPY ELCPRTFD.                                               ECS068
00057  EJECT                                                            ECS068
00058  FD  ACCT-MSTR-IN                                                 ECS068
00059      BLOCK CONTAINS 0 RECORDS
00060      RECORDING MODE F.                                               CL**2
00061                                                                      CL**2
00062      COPY ERCACCT.                                                ECS068
00063  EJECT                                                            ECS068
00064  FD  DISK-DATE                                                    ECS068
00065      COPY ELCDTEFD.                                               ECS068
00066  EJECT                                                            ECS068
00067  WORKING-STORAGE SECTION.                                         ECS068
00068  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      ECS068
00069  77  LCP-ASA                       PIC X.                         ECS068
00070  77  FILLER PIC X(32) VALUE '********************************'.   ECS068
00071  77  FILLER PIC X(32) VALUE '*            ECS068            *'.   ECS068
00072  77  FILLER PIC X(32) VALUE '*********** VMOD=2.004 *********'.   ECS068
00073                                                                   ECS068
00074  77  X1                      PIC S9(3)       VALUE +0   COMP-3.   ECS068
00075  77  PGM-SUB                 PIC S9(3)       VALUE +68  COMP-3.   ECS068
00076  77  MAX-NDX                 PIC S9(3)       VALUE +3   COMP-3.   ECS068
00077  77  STMT-SW                 PIC S9(3)       VALUE +1   COMP-3.   ECS068
00078  77  NBR-OF-LABELS           PIC S9(7)       VALUE +0   COMP-3.   ECS068
00079  77  MAX-ALIGN-CTR           PIC S9(9)       VALUE +20  COMP-3.   ECS068
00080  77  ALIGN-CTR               PIC S9(9)       VALUE +0   COMP-3.   ECS068
00081  77  X                       PIC  X          VALUE ' '.           ECS068
00082 *                                                                 ECS068
00083 *                                                                 ECS068
00084 * LEAVE THE ABOVE 2 LINES IN TO PREVENT LOSING THE LINE ABOVE     ECS068
00095                                                                   ECS068
00096  01  WS.                                                          ECS068
00097      12  WS-RETURN-CODE          PIC S9(4)              COMP.     ECS068
00098      12  WS-ABEND-MESSAGE        PIC  X(80).                      ECS068
00099      12  WS-ABEND-FILE-STATUS    PIC  X(2)   VALUE ZEROS.         ECS068
00100      12  WS-ZERO                 PIC S9      VALUE ZERO COMP-3.   ECS068
00101  EJECT                                                            ECS068
00102  EJECT                                                            ECS068
00103  01  MISC-WS.                                                     ECS068
00104      12  WS-AM-CONTROL           PIC X(19)  VALUE SPACES.         ECS068
00105                                                                   ECS068
00106  01  LABEL-AREA.                                                  ECS068
00107      12  LINE-1.                                                  ECS068
00108          16  LN-1        OCCURS 3 TIMES.                          ECS068
00109              20  FILLER          PIC  X.                          ECS068
00110              20  L1-INFO         PIC  X(33).                      ECS068
00111              20  FILLER          PIC  X.                          ECS068
00112              20  L1-SP           PIC  X.                          ECS068
00113      12  LINE-2.                                                  ECS068
00114          16  LN-2        OCCURS 3 TIMES.                          ECS068
00115              20  FILLER          PIC  X.                          ECS068
00116              20  L2-INFO         PIC  X(33).                      ECS068
00117              20  FILLER          PIC  X.                          ECS068
00118              20  L2-SP           PIC  X.                          ECS068
00119      12  LINE-3.                                                  ECS068
00120          16  LN-3        OCCURS 3 TIMES.                          ECS068
00121              20  FILLER          PIC  X.                          ECS068
00122              20  L3-INFO         PIC  X(33).                      ECS068
00123              20  FILLER          PIC  X.                          ECS068
00124              20  L3-SP           PIC  X.                          ECS068
00125      12  LINE-4.                                                  ECS068
00126          16  LN-4        OCCURS 3 TIMES.                          ECS068
00127              20  FILLER          PIC  X.                          ECS068
00128              20  L4-INFO         PIC  X(33).                      ECS068
00129              20  FILLER          PIC  X.                          ECS068
00130              20  L4-SP           PIC  X.                          ECS068
00131      12  LINE-5.                                                  ECS068
00132          16  LN-5        OCCURS 3 TIMES.                          ECS068
00133              20  FILLER          PIC  X.                          ECS068
00134              20  L5-INFO.                                         ECS068
00135                  24  L5-CITY-ST.                                  ECS068
00136                      28  L5-CITY PIC  X(22).                      ECS068
00137                      28  L5-ST   PIC  XX.                         ECS068
00138                  24  L5-ZIP      PIC  X(9).                       ECS068
00139              20  FILLER          PIC  X.                          ECS068
00140              20  L5-SP           PIC  X.                          ECS068
00141                                                                   ECS068
00142  01  LABEL-AREA-X REDEFINES LABEL-AREA.                           ECS068
00143      12  LABEL-LINE-1.                                            ECS068
00144          16  LAB-1-L1        PIC  X(36).                          ECS068
00145          16  LAB-2-L1        PIC  X(36).                          ECS068
00146          16  LAB-3-L1        PIC  X(36).                          ECS068
00147      12  LABEL-LINE-2.                                            ECS068
00148          16  LAB-1-L2        PIC  X(36).                          ECS068
00149          16  LAB-2-L2        PIC  X(36).                          ECS068
00150          16  LAB-3-L2        PIC  X(36).                          ECS068
00151      12  LABEL-LINE-3.                                            ECS068
00152          16  LAB-1-L3        PIC  X(36).                          ECS068
00153          16  LAB-2-L3        PIC  X(36).                          ECS068
00154          16  LAB-3-L3        PIC  X(36).                          ECS068
00155      12  LABEL-LINE-4.                                            ECS068
00156          16  LAB-1-L4        PIC  X(36).                          ECS068
00157          16  LAB-2-L4        PIC  X(36).                          ECS068
00158          16  LAB-3-L4        PIC  X(36).                          ECS068
00159      12  LABEL-LINE-5.                                            ECS068
00160          16  LAB-1-L5        PIC  X(36).                          ECS068
00161          16  LAB-2-L5        PIC  X(36).                          ECS068
00162          16  LAB-3-L5        PIC  X(36).                          ECS068
00163  EJECT                                                            ECS068
00164  01  P-REC.                                                       ECS068
00165      12  P-CCSW              PIC  X.                              ECS068
00166      12  P-LN.                                                    ECS068
00167          16  FILLER          PIC  X(132).                         ECS068
00168                                                                   ECS068
00169      COPY ELCDTECX.                                               ECS068
00170                                                                   ECS068
00171      COPY ELCDTEVR.                                               ECS068
00172  EJECT                                                            ECS068
00173  PROCEDURE DIVISION.                                              ECS068
00174                                                                   ECS068
00175  0000-STANDARD-COPY.                                              ECS068
00176      COPY ELCDTERX.                                               ECS068
00177                                                                   ECS068
00178  1000-INTL-INPUT.                                                 ECS068
00179      OPEN INPUT  ACCT-MSTR-IN                                     ECS068
00180           OUTPUT  PRNTR.                                          ECS068
00181                                                                   ECS068
00182      MOVE ALL 'X'                TO  LABEL-AREA.                  ECS068
00183      MOVE SPACES                 TO  L1-SP (1)   L2-SP (1)        ECS068
00184                                      L3-SP (1)   L4-SP (1)        ECS068
00185                                      L5-SP (1)                    ECS068
00186                                      L1-SP (2)   L2-SP (2)        ECS068
00187                                      L3-SP (2)   L4-SP (2)        ECS068
00188                                      L5-SP (2)                    ECS068
00189                                      L1-SP (3)   L2-SP (3)        ECS068
00190                                      L3-SP (3)   L4-SP (3)        ECS068
00191                                                  L5-SP (3).       ECS068
00192      MOVE COMPANY-NAME           TO  L1-INFO (1)  L1-INFO (2)     ECS068
00193                                      L1-INFO (3).                 ECS068
00194                                                                   ECS068
00195      MOVE MAX-NDX                TO  X1.                          ECS068
00196                                                                   ECS068
00197  EJECT                                                            ECS068
00198  2100-READ-ACCT.                                                  ECS068
00199      READ ACCT-MSTR-IN                                            ECS068
00200          AT END GO TO 9000-EOJ.                                   ECS068
00201                                                                   ECS068
00202      IF AM-CONTROL-A = WS-AM-CONTROL                              ECS068
00203          GO TO 2100-READ-ACCT.                                    ECS068
00204                                                                   ECS068
00205      MOVE AM-CONTROL-A       TO WS-AM-CONTROL.                    ECS068
00206                                                                   ECS068
00207  4000-FORMAT-LABEL-RTN.                                           ECS068
00208      ADD 1 TO  X1.                                                ECS068
00209                                                                   ECS068
00210      IF X1 GREATER MAX-NDX                                        ECS068
00211          PERFORM 4800-GEN-LABEL-RTN  THRU  4999-EXIT.             ECS068
00212                                                                   ECS068
00213      ADD +1                      TO  NBR-OF-LABELS.               ECS068
00214                                                                   ECS068
00215      MOVE AM-NAME                TO  L1-INFO (X1).                ECS068
00216      MOVE AM-PERSON              TO  L2-INFO (X1).                ECS068
00217      MOVE AM-ADDRS               TO  L3-INFO (X1).                ECS068
00218      MOVE AM-CITY                TO  L4-INFO (X1).                ECS068
00219      MOVE AM-ZIP                 TO  L5-ZIP (X1).                 ECS068
00220                                                                   ECS068
00221      IF AM-NAME = AM-PERSON                                       ECS068
00222          MOVE SPACES             TO  L1-INFO (X1).                ECS068
00223                                                                   ECS068
00224      PERFORM 4200-BUMP-RTN  THRU  4299-EXIT  2  TIMES.            ECS068
00225                                                                   ECS068
00226      GO TO 2100-READ-ACCT.                                        ECS068
00227  EJECT                                                            ECS068
00228  4200-BUMP-RTN.                                                   ECS068
00229      IF L1-INFO (X1) = SPACES                                     ECS068
00230          MOVE L2-INFO (X1)       TO  L1-INFO (X1)                 ECS068
00231          MOVE SPACES             TO  L2-INFO (X1).                ECS068
00232                                                                   ECS068
00233      IF L2-INFO (X1) = SPACES                                     ECS068
00234          MOVE L3-INFO (X1)       TO  L2-INFO (X1)                 ECS068
00235          MOVE SPACES             TO  L3-INFO (X1).                ECS068
00236                                                                   ECS068
00237      IF L3-INFO (X1) = SPACES                                     ECS068
00238          MOVE L4-INFO (X1)       TO  L3-INFO (X1)                 ECS068
00239          MOVE SPACES             TO  L4-INFO (X1).                ECS068
00240                                                                   ECS068
00241      IF L4-INFO (X1) = SPACES                                     ECS068
00242          MOVE L5-INFO (X1)       TO  L4-INFO (X1)                 ECS068
00243          MOVE SPACES             TO  L5-INFO (X1).                ECS068
00244                                                                   ECS068
00245  4299-EXIT.                                                       ECS068
00246      EXIT.                                                        ECS068
00247  EJECT                                                            ECS068
00248  4800-GEN-LABEL-RTN.                                              ECS068
00249      MOVE '1'                    TO  P-CCSW.                      ECS068
00250      MOVE LINE-1                 TO  P-LN.                        ECS068
00251                                                                   ECS068
00252      PERFORM 8800-PRT-RTN  THRU  8999-EXIT.                       ECS068
00253                                                                   ECS068
00254      MOVE LINE-2                 TO  P-LN.                        ECS068
00255      PERFORM 8800-PRT-RTN  THRU  8999-EXIT.                       ECS068
00256                                                                   ECS068
00257      MOVE LINE-3                 TO  P-LN.                        ECS068
00258      PERFORM 8800-PRT-RTN  THRU  8999-EXIT.                       ECS068
00259                                                                   ECS068
00260      MOVE LINE-4                 TO  P-LN.                        ECS068
00261      PERFORM 8800-PRT-RTN  THRU  8999-EXIT.                       ECS068
00262                                                                   ECS068
00263      MOVE LINE-5                 TO  P-LN.                        ECS068
00264      PERFORM 8800-PRT-RTN  THRU  8999-EXIT.                       ECS068
00265                                                                   ECS068
00266      ADD +1                      TO  ALIGN-CTR.                   ECS068
00267                                                                   ECS068
00268      IF ALIGN-CTR LESS MAX-ALIGN-CTR                              ECS068
00269          GO TO 4800-GEN-LABEL-RTN.                                ECS068
00270                                                                   ECS068
00271      MOVE SPACES                 TO  LABEL-AREA.                  ECS068
00272      MOVE 1                      TO  X1.                          ECS068
00273                                                                   ECS068
00274  4999-EXIT.                                                       ECS068
00275      EXIT.                                                        ECS068
00276  EJECT                                                            ECS068
00277  8800-PRT-RTN.                                                    ECS068
00278      MOVE P-CCSW                 TO  X  P-CTL.                    ECS068
00279      MOVE P-LN                   TO  P-DATA.                      ECS068
00280      MOVE ' '                    TO  P-REC.                       ECS068
00281                                                                   ECS068
00282  8900-PRT-COPY.                                                   ECS068
00283      MOVE X                      TO  P-CTL.                       ECS068
00284      MOVE X TO LCP-ASA                                            ECS068
00285      PERFORM LCP-WRITE-POS-PRT                                    ECS068
00286          THRU LCP-WRITE-END-PRT.                                  ECS068
00287                                                                   ECS068
00288  8999-EXIT.                                                       ECS068
00289      EXIT.                                                        ECS068
00290  EJECT                                                            ECS068
00291  9000-EOJ.                                                        ECS068
00292      IF NBR-OF-LABELS = +0                                        ECS068
00293          GO TO 9995-CLOSE-FILES.                                  ECS068
00294                                                                   ECS068
00295      PERFORM 4800-GEN-LABEL-RTN  THRU  4999-EXIT.                 ECS068
00296                                                                   ECS068
00297  9995-CLOSE-FILES.                                                ECS068
00298      CLOSE ACCT-MSTR-IN                                           ECS068
00299            PRNTR.                                                 ECS068
00300                                                                   ECS068
00301      GOBACK.                                                      ECS068
00302                                                                   ECS068
00303  ABEND-PGM SECTION.                                               ECS068
00304                              COPY ELCABEND.                       ECS068
00305 /                                                                 ECS068
00306  LCP-WRITE-POS-PRT SECTION.                                       ECS068
00307      IF LCP-ASA = '+'                                             ECS068
00308          WRITE PRT AFTER 0 LINE                                   ECS068
00309      ELSE                                                         ECS068
00310      IF LCP-ASA = ' '                                             ECS068
00311          WRITE PRT AFTER ADVANCING 1 LINE                         ECS068
00312      ELSE                                                         ECS068
00313      IF LCP-ASA = '0'                                             ECS068
00314          WRITE PRT AFTER ADVANCING 2 LINE                         ECS068
00315      ELSE                                                         ECS068
00316      IF LCP-ASA = '-'                                             ECS068
00317          WRITE PRT AFTER ADVANCING 3 LINE                         ECS068
00318      ELSE                                                         ECS068
00319      IF LCP-ASA = '1'                                             ECS068
00320          WRITE PRT AFTER ADVANCING PAGE                           ECS068
00321      ELSE                                                         ECS068
00322      IF LCP-ASA = '2'                                             ECS068
00323          WRITE PRT AFTER ADVANCING LCP-CH2                        ECS068
00324      ELSE                                                         ECS068
00325      IF LCP-ASA = '3'                                             ECS068
00326          WRITE PRT AFTER ADVANCING LCP-CH3                        ECS068
00327      ELSE                                                         ECS068
00328      IF LCP-ASA = '4'                                             ECS068
00329          WRITE PRT AFTER ADVANCING LCP-CH4                        ECS068
00330      ELSE                                                         ECS068
00331      IF LCP-ASA = '5'                                             ECS068
00332          WRITE PRT AFTER ADVANCING LCP-CH5                        ECS068
00333      ELSE                                                         ECS068
00334      IF LCP-ASA = '6'                                             ECS068
00335          WRITE PRT AFTER ADVANCING LCP-CH6                        ECS068
00336      ELSE                                                         ECS068
00337      IF LCP-ASA = '7'                                             ECS068
00338          WRITE PRT AFTER ADVANCING LCP-CH7                        ECS068
00339      ELSE                                                         ECS068
00340      IF LCP-ASA = '8'                                             ECS068
00341          WRITE PRT AFTER ADVANCING LCP-CH8                        ECS068
00342      ELSE                                                         ECS068
00343      IF LCP-ASA = '9'                                             ECS068
00344          WRITE PRT AFTER ADVANCING LCP-CH9                        ECS068
00345      ELSE                                                         ECS068
00346      IF LCP-ASA = 'A'                                             ECS068
00347          WRITE PRT AFTER ADVANCING LCP-CH10                       ECS068
00348      ELSE                                                         ECS068
00349      IF LCP-ASA = 'B'                                             ECS068
00350          WRITE PRT AFTER ADVANCING LCP-CH11                       ECS068
00351      ELSE                                                         ECS068
00352      IF LCP-ASA = 'C'                                             ECS068
00353          WRITE PRT AFTER ADVANCING LCP-CH12                       ECS068
00354      ELSE                                                         ECS068
00355      IF LCP-ASA = 'V'                                             ECS068
00356          WRITE PRT AFTER ADVANCING LCP-P01                        ECS068
00357      ELSE                                                         ECS068
00358      IF LCP-ASA = 'W'                                             ECS068
00359          WRITE PRT AFTER ADVANCING LCP-P02                        ECS068
00360      ELSE                                                         ECS068
00361      DISPLAY 'ASA CODE ERROR'.                                    ECS068
00362  LCP-WRITE-END-PRT.                                               ECS068
00363      EXIT.                                                        ECS068
