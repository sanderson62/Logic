00001  IDENTIFICATION DIVISION.                                         03/06/98
00002                                                                   EL327
00003  PROGRAM-ID.                 EL327 .                                 LV002
00004 *              PROGRAM CONVERTED BY                               EL327
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL327
00006 *              CONVERSION DATE 02/14/96 13:42:18.                 EL327
00007 *           PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE             EL327
00008 *                            VMOD=2.005                           EL327
00009                                                                   EL327
00009                                                                   EL327
00010 *AUTHOR.     LOGIC, INC.                                          EL327
00011 *            DALLAS, TEXAS.                                       EL327
00015 *SECURITY.   *****************************************************EL327
00016 *            *                                                   *EL327
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *EL327
00018 *            *                                                   *EL327
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL327
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL327
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *EL327
00022 *            *                                                   *EL327
00023 *            *****************************************************EL327
00012                                                                   EL327
00025 *REMARKS.                                                         EL327
00026 *        THIS PROGRAM PRINTS A REGISTER OF ALL EXPENSE CHECKS     EL327
00027 *    ISSUED IN THE REPORTED MONTH.                                EL327
122702******************************************************************
122702*                   C H A N G E   L O G
122702*
122702* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
122702*-----------------------------------------------------------------
122702*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
122702* EFFECTIVE    NUMBER
122702*-----------------------------------------------------------------
122702* 122702    2001061800003  SMVA  ADD PROCESSING FOR NEW CLM TYP I
122702*                                AND G FOR DCC
052804* 052804  CR2004051200002  SMVA  INCR SZ OF ELCEXTR TO ADD RPT CD 1
052614* 052614    2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
022122* 022122  CR2021100800003  PEMA  Add B and H claim types
122702******************************************************************
00028                                                                   EL327
00029      EJECT                                                        EL327
00030  ENVIRONMENT DIVISION.                                            EL327
00031                                                                   EL327
00032  INPUT-OUTPUT SECTION.                                            EL327
00033                                                                   EL327
00034  FILE-CONTROL.                                                    EL327
00035                                                                   EL327
00036      SELECT REPORTS-EXTRACT-FILE                                  EL327
00037          ASSIGN TO SYS010-UT-2400-S-SYS010.                       EL327
00038                                                                   EL327
00039      SELECT SORT-WORK-FILE                                        EL327
00040          ASSIGN TO SYS001-UT-FBA1-S-SORTWK1.                      EL327
00041                                                                   EL327
00042      SELECT DISK-DATE        ASSIGN TO SYS019-FBA1-S-SYS019.      EL327
00043                                                                   EL327
00044      SELECT PRNTR            ASSIGN TO SYS008-UR-1403-S-SYS008.   EL327
00045                                                                   EL327
00046      SELECT FICH             ASSIGN TO SYS020-UT-2400-S-SYS020.   EL327
00047                                                                   EL327
00048      SELECT ELREPT           ASSIGN TO SYS018-FBA1-ELREPT         EL327
00049                              ORGANIZATION IS INDEXED              EL327
00050                              ACCESS IS DYNAMIC                    EL327
00051                              RECORD KEY IS RF-CONTROL-PRIMARY     EL327
00052                              FILE STATUS IS DTE-VSAM-FLAGS.       EL327
00053                                                                   EL327
00054      EJECT                                                        EL327
00055  DATA DIVISION.                                                   EL327
00056                                                                   EL327
00057  FILE SECTION.                                                    EL327
00058                                                                   EL327
00059  FD  REPORTS-EXTRACT-FILE COPY ELCEXTFD.                          EL327
00060                                                                   EL327
052804 01  FILLER                          PIC X(319).                  EL327
00062                                                                   EL327
00063  SD  SORT-WORK-FILE.                                              EL327
00064                                                                   EL327
00065                                  COPY ELCEXTR.                    EL327
00066                                                                   EL327
00067  FD  DISK-DATE               COPY ELCDTEFD.                       EL327
00068                                                                   EL327
00069  FD  PRNTR                   COPY ELCPRTFD.                       EL327
00070                                                                   EL327
00071  FD  FICH                    COPY ELCFCHFD.                       EL327
00072                                                                   EL327
00073  FD  ELREPT                  COPY ELCRPTFD.                       EL327
00074                                                                   EL327
00075                              COPY ELCREPT.                        EL327
00076                                                                   EL327
00077      EJECT                                                        EL327
00078  WORKING-STORAGE SECTION.                                         EL327
00079  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      EL327
00080                                                                   EL327
00081  77  FILLER  PIC X(32)   VALUE '********************************'.EL327
00082  77  FILLER  PIC X(32)   VALUE '*     EL327  WORKING STORAGE   *'.EL327
00083  77  FILLER  PIC X(32)   VALUE '*********** VM 2.005 ***********'.EL327
00084                                                                   EL327
00085  01  FILLER                          COMP-3.                      EL327
00086      05  WS-LINE-COUNT               PIC S9(3)       VALUE +99.   EL327
00087      05  WS-LINE-COUNT-MAX           PIC S9(3)       VALUE +60.   EL327
00088      05  WS-PAGE                     PIC S9(5)       VALUE ZERO.  EL327
00089      05  WS-REPORT-SW                PIC S9          VALUE ZERO.  EL327
00090      05  WS-PRINT-SW                 PIC S9          VALUE ZERO.  EL327
00091      05  WS-RECORD-COUNT             PIC S9(9)       VALUE ZERO.  EL327
00092      05  WS-RECORD-COUNT-A           PIC S9(9)       VALUE ZERO.  EL327
00093      05  WS-RECORD-COUNT-B           PIC S9(9)       VALUE ZERO.  EL327
00094      05  WS-RECORD-COUNT-C           PIC S9(9)       VALUE ZERO.  EL327
00095      05  WS-RECORD-COUNT-AA          PIC S9(9)       VALUE ZERO.  EL327
00096      05  WS-RECORD-COUNT-AX          PIC S9(9)       VALUE ZERO.  EL327
00097      05  WS-RECORD-COUNT-BA          PIC S9(9)       VALUE ZERO.  EL327
00098      05  WS-RECORD-COUNT-BX          PIC S9(9)       VALUE ZERO.  EL327
00099      05  WS-RECORDS-RELEASED         PIC S9(9)       VALUE ZERO.  EL327
00100      05  WS-RETURN-CODE              PIC S9(3)       VALUE ZERO.  EL327
00101      05  WS-ZERO                     PIC S9          VALUE ZERO.  EL327
00102                                                                   EL327
00103      05  WS-INCURRED-AGE             PIC S9(3)       VALUE ZERO.  EL327
00104      05  WS-YEAR                     REDEFINES                    EL327
00105          WS-INCURRED-AGE             PIC S9(3).                   EL327
00106                                                                   EL327
00107      05   WS-AT-CURR-CHARGED         PIC S9(5)V99      VALUE ZERO.EL327
00108      05   WS-AT-CURR-NON-CHARGED     PIC S9(5)V99      VALUE ZERO.EL327
00109      05   WS-AT-ITD-CHARGED          PIC S9(5)V99      VALUE ZERO.EL327
00110      05   WS-AT-ITD-NON-CHARGED      PIC S9(5)V99      VALUE ZERO.EL327
00111                                                                   EL327
00112      05   WS-CT-CURR-CHARGED         PIC S9(5)V99      VALUE ZERO.EL327
00113      05   WS-CT-CURR-NON-CHARGED     PIC S9(5)V99      VALUE ZERO.EL327
00114      05   WS-CT-ITD-CHARGED          PIC S9(5)V99      VALUE ZERO.EL327
00115      05   WS-CT-ITD-NON-CHARGED      PIC S9(5)V99      VALUE ZERO.EL327
00116                                                                   EL327
00117      05   WS-GT-CURR-CHARGED         PIC S9(5)V99      VALUE ZERO.EL327
00118      05   WS-GT-CURR-NON-CHARGED     PIC S9(5)V99      VALUE ZERO.EL327
00119      05   WS-GT-ITD-CHARGED          PIC S9(5)V99      VALUE ZERO.EL327
00120      05   WS-GT-ITD-NON-CHARGED      PIC S9(5)V99      VALUE ZERO.EL327
00121                                                                   EL327
00122      EJECT                                                        EL327
00123  01  FILLER                          COMP SYNC.                   EL327
00124      05  PGM-SUB                     PIC S9(4)       VALUE +327.  EL327
00125      05  WS-INDEX                    PIC S9(4)       VALUE ZERO.  EL327
00126                                                                   EL327
00127  01  FILLER.                                                      EL327
00128      05  ABEND-CODE                  PIC X(4).                    EL327
00129      05  ABEND-OPTION                PIC X.                       EL327
00130      05  OLC-REPORT-NAME             PIC X(8) VALUE 'EL327'.      EL327
00131      05  X                           PIC X           VALUE SPACE. EL327
00132                                                                   EL327
00133      05  WS-SAVE-PRINT-RECORD        PIC X(133)      VALUE SPACES.EL327
00134                                                                   EL327
00135      05  WS-LAST-CARRIER             PIC X           VALUE SPACES.EL327
00136      05  WS-LAST-ACCOUNT             PIC X(10)       VALUE SPACES.EL327
00137                                                                   EL327
00138      05  WS-ABEND-MESSAGE            PIC X(80)       VALUE SPACES.EL327
00139                                                                   EL327
00140      05  WS-ABEND-FILE-STATUS        PIC XX          VALUE ZERO.  EL327
00141                                                                   EL327
00142      05  WS-FILE-ERROR-MESSAGE.                                   EL327
00143          10  FILLER                  PIC X(24)       VALUE        EL327
00144              'ERROR OCCURED OPENING - '.                          EL327
00145          10  WS-FEM-FILE-NAME        PIC X(8).                    EL327
00146                                                                   EL327
00147      05  WS-DATE-WORK.                                            EL327
00148          10  WS-DW-MONTH             PIC 99.                      EL327
00149          10  FILLER                  PIC X(4).                    EL327
00150          10  WS-DW-YEAR              PIC 99.                      EL327
00151                                                                   EL327
00152      EJECT                                                        EL327
00153  01  WS-HEADING1.                                                 EL327
00154      05  FILLER                      PIC X(50)       VALUE '1'.   EL327
00155      05  WS-H1-TITLE                 PIC X(70)       VALUE        EL327
00156          'CLAIM EXPENSE REGISTER'.                                EL327
00157      05  WS-H1-REPORT-NUMBER         PIC X(9) VALUE 'EL327'.      EL327
00158                                                                   EL327
00159  01  WS-HEADING2.                                                 EL327
00160      05  FILLER                      PIC X(45)       VALUE SPACES.EL327
00161      05  WS-H2-CLIENT-NAME           PIC X(75)       VALUE SPACES.EL327
00162      05  WS-H2-DATE                  PIC X(8)        VALUE SPACES.EL327
00163                                                                   EL327
00164  01  WS-HEADING3.                                                 EL327
00165      05  FILLER                      PIC X(51)       VALUE SPACES.EL327
00166      05  WS-H3-DATE                  PIC X(69)       VALUE SPACES.EL327
00167      05  FILLER                      PIC X(5)       VALUE 'PAGE '.EL327
00168      05  WS-H3-PAGE                  PIC ZZ,ZZ9.                  EL327
00169      05  FILLER                      PIC X(11)       VALUE SPACES.EL327
00170                                                                   EL327
00171  01  WS-HEADING4.                                                 EL327
00172      05  FILLER                      PIC X(11)       VALUE        EL327
00173          '0CARRIER - '.                                           EL327
00174      05  WS-H4-CARRIER               PIC X           VALUE SPACES.EL327
00175      05  FILLER                      PIC X(121)      VALUE SPACES.EL327
00176                                                                   EL327
00177  01  WS-HEADING5.                                                 EL327
00178      05  FILLER                      PIC X(11)       VALUE SPACES.EL327
00179      05  WS-H5-CARRIER-NAME          PIC X(30)       VALUE SPACES.EL327
00180      05  FILLER                      PIC X(92)       VALUE SPACES.EL327
00181                                                                   EL327
00182  01  WS-HEADING8.                                                 EL327
00183      05  FILLER                      PIC X(64)       VALUE        EL327
00184          '- CLAIM    CERT    CLAIM'.                              EL327
00185      05  FILLER                      PIC X(69)       VALUE        EL327
00186          'CLAIM  EXPENSE  CHECK   CHECK   CHARGED   NON-CHARGED'. EL327
00187                                                                   EL327
00188  01  WS-HEADING9.                                                 EL327
00189      05  FILLER                      PIC X(62)       VALUE        EL327
00190          '  NUMBER  NUMBER    TYPE     INSURED'.                  EL327
00191      05  FILLER                      PIC X(71)       VALUE        EL327
00192          'INCURRED   TYPE    DATE   NUMBER   AMOUNT     AMOUNT'.  EL327
00193                                                                   EL327
00194      EJECT                                                        EL327
00195  01  WS-DETAIL1.                                                  EL327
00196      05  FILLER                      PIC X.                       EL327
00197      05  WS-D1-CLAIM-NUMBER          PIC X(7).                    EL327
00198      05  FILLER                      PIC X.                       EL327
00199      05  WS-D1-CERT-NUMBER           PIC X(11).                   EL327
00200      05  FILLER                      PIC X.                       EL327
00201      05  WS-D1-CLAIM-TYPE            PIC X(6).                    EL327
00202      05  FILLER                      PIC XX.                      EL327
00203      05  WS-D1-INSURED               PIC X(30).                   EL327
00204      05  WS-D1-ACCOUNT-INFO REDEFINES WS-D1-INSURED.              EL327
00205          10  WS-D1-ACCT-DESC         PIC X(15).                   EL327
00206          10  WS-D1-ACCT-NUMBER       PIC X(10).                   EL327
00207          10  FILLER                  PIC X(5).                    EL327
00208      05  FILLER                      PIC X(3).                    EL327
00209      05  WS-D1-INCURRED-DATE         PIC X(8).                    EL327
00210      05  FILLER                      PIC X(4).                    EL327
00211      05  WS-D1-EXPENSE-TYPE          PIC X.                       EL327
00212      05  FILLER                      PIC X(4).                    EL327
00213      05  WS-D1-CHECK-DATE            PIC X(8).                    EL327
00214      05  FILLER                      PIC X.                       EL327
00215      05  WS-D1-CHECK-NUMBER          PIC X(8).                    EL327
00216      05  WS-D1-CURR-CHARGED          PIC ZZ,ZZ9.99-.              EL327
00217      05  WS-D1-CURR-NON-CHARGED      PIC ZZ,ZZ9.99-.              EL327
00218                                                                   EL327
00219  01  FILLER                          REDEFINES                    EL327
00220      WS-DETAIL1.                                                  EL327
00221      05  FILLER                      PIC X(72).                   EL327
00222      05  WS-D2-VOIDED                PIC X(6).                    EL327
00223                                                                   EL327
00224                                  COPY ELCDTECX.                   EL327
00225                                                                   EL327
00226                                  COPY ELCDTEVR.                   EL327
00227                                                                   EL327
00228                                  COPY ELCDATE.                    EL327
00229                                                                   EL327
00230      EJECT                                                        EL327
00231  PROCEDURE DIVISION.                                              EL327
00232                                                                   EL327
00233  0000-DATE-CARD-READ SECTION. COPY ELCDTERX.                      EL327
00234                                                                   EL327
00235  1000-MAIN-LOGIC SECTION.                                         EL327
00236                                                                   EL327
00237      PERFORM OPEN-FILES.                                          EL327
00238                                                                   EL327
00239      SORT SORT-WORK-FILE                                          EL327
00240          ON ASCENDING KEY EX-SA-CARRIER                           EL327
00241                           EX-BA-ACCOUNT                           EL327
00242                           EX-BA-CLAIM-NO                          EL327
00243                           EX-BA-CERT-NO                           EL327
00244                           EX-BA-CERT-EFF-DT                       EL327
00245                           EX-BA-CHECK-WRITTEN-DT                  EL327
00246          INPUT PROCEDURE IS 2000-SORT-INPUT-PROCEDURE             EL327
00247          OUTPUT PROCEDURE IS 3000-SORT-OUTPUT-PROCEDURE           EL327
00248                                                                   EL327
122702     IF WS-RECORDS-RELEASED NOT = +0
00249          IF SORT-RETURN > ZERO  
00250              MOVE 'SORT FAILED'      TO  WS-ABEND-MESSAGE   
00251              MOVE SORT-RETURN        TO  WS-RETURN-CODE  
00252              GO TO ABEND-PGM
122702         END-IF
122702     END-IF.
00253                                                                   EL327
00254      PERFORM CLOSE-FILES.                                         EL327
00255                                                                   EL327
00256      GOBACK.                                                      EL327
00257                                                                   EL327
00258      EJECT                                                        EL327
00259  2000-SORT-INPUT-PROCEDURE SECTION.                               EL327
00260                                                                   EL327
00261  2100-SORT-INPUT-PROCEDURE.                                       EL327
00262      READ REPORTS-EXTRACT-FILE INTO REPORTS-EXTRACT-RECORD        EL327
00263          AT END                                                   EL327
00264              GO TO 2900-EXIT.                                     EL327
00265                                                                   EL327
00266      ADD +1  TO  WS-RECORD-COUNT.                                 EL327
00267                                                                   EL327
00268      IF EX-POSITIONING-CODE LESS THAN '9'                         EL327
00269          GO TO 2100-SORT-INPUT-PROCEDURE.                         EL327
00270                                                                   EL327
00271      IF EX-POSITIONING-CODE GREATER THAN '9'                      EL327
00272          GO TO 2900-EXIT.                                         EL327
00273                                                                   EL327
00274      IF EX-EXTRACT-CODE LESS THAN 'B'                             EL327
00275          GO TO 2100-SORT-INPUT-PROCEDURE.                         EL327
00276                                                                   EL327
00277      IF EX-EXTRACT-CODE GREATER THAN 'B'                          EL327
00278          GO TO 2900-EXIT.                                         EL327
00279                                                                   EL327
00280      IF EX-COMPANY-CD LESS THAN DTE-CLASIC-COMPANY-CD             EL327
00281          GO TO 2100-SORT-INPUT-PROCEDURE.                         EL327
00282                                                                   EL327
00283      IF EX-COMPANY-CD GREATER THAN DTE-CLASIC-COMPANY-CD          EL327
00284          GO TO 2900-EXIT.                                         EL327
00285                                                                   EL327
00286      IF EX-RECORD-TYPE GREATER THAN 'A'                           EL327
00287          GO TO 2900-EXIT.                                         EL327
00288                                                                   EL327
00289      IF EX-BA-PAYMENT-TYPE = '5' OR '6'                           EL327
00290          NEXT SENTENCE                                            EL327
00291      ELSE                                                         EL327
00292          GO TO 2100-SORT-INPUT-PROCEDURE.                         EL327
00293                                                                   EL327
00294      MOVE    EX-BA-CHECK-WRITTEN-DT TO  DC-BIN-DATE-1.            EL327
00295      MOVE    SPACES                 TO  DC-OPTION-CODE.           EL327
00296      PERFORM 8500-DATE-CONVERSION.                                EL327
00297      MOVE    DC-GREG-DATE-1-EDIT    TO  WS-DATE-WORK.             EL327
00298                                                                   EL327
00299      IF    WS-DW-MONTH = RUN-MO                                   EL327
00300        AND WS-DW-YEAR  = RUN-YR                                   EL327
00301            NEXT SENTENCE                                          EL327
00302      ELSE                                                         EL327
00303          GO TO 2100-SORT-INPUT-PROCEDURE.                         EL327
00304                                                                   EL327
00305      RELEASE REPORTS-EXTRACT-RECORD.                              EL327
00306                                                                   EL327
00307      IF SORT-RETURN GREATER THAN ZERO                             EL327
00308          MOVE 'ERROR OCCURED SORT - RELEASE'  TO  WS-ABEND-MESSAGEEL327
00309          MOVE SORT-RETURN        TO  WS-RETURN-CODE               EL327
00310          GO TO ABEND-PGM.                                         EL327
00311                                                                   EL327
00312      ADD +1  TO  WS-RECORDS-RELEASED.                             EL327
00313                                                                   EL327
00314      GO TO 2100-SORT-INPUT-PROCEDURE.                             EL327
00315                                                                   EL327
00316  2900-EXIT.                                                       EL327
00317      EXIT.                                                        EL327
00318                                                                   EL327
00319      EJECT                                                        EL327
00320  3000-SORT-OUTPUT-PROCEDURE SECTION.                              EL327
00321      IF WS-RECORDS-RELEASED NOT GREATER THAN ZERO                 EL327
00322          MOVE '0 NO EXPENSE RECORDS FOR THIS REPORT' TO PRT       EL327
00323          PERFORM WRITE-A-LINE                                     EL327
00324          GO TO 3900-EXIT.                                         EL327
00325                                                                   EL327
00326  3100-SORT-OUTPUT-PROCEDURE.                                      EL327
00327      RETURN SORT-WORK-FILE                                        EL327
00328          AT END                                                   EL327
00329              MOVE HIGH-VALUES    TO  EX-SB-CARRIER                EL327
00330                                      EX-BA-ACCOUNT.               EL327
00331                                                                   EL327
00332      IF SORT-RETURN GREATER THAN ZERO                             EL327
00333          MOVE 'ERROR OCCURED SORT - RETURN'  TO  WS-ABEND-MESSAGE EL327
00334          MOVE SORT-RETURN        TO  WS-RETURN-CODE               EL327
00335          GO TO ABEND-PGM.                                         EL327
00336                                                                   EL327
00337      IF  WS-LAST-CARRIER EQUAL SPACES                             EL327
00338          MOVE    EX-SB-CARRIER       TO  WS-LAST-CARRIER          EL327
00339          PERFORM 8100-GET-CARRIER-NAME                            EL327
00340          MOVE    EX-BA-ACCOUNT       TO  WS-LAST-ACCOUNT.         EL327
00341                                                                   EL327
00342      EJECT                                                        EL327
00343      IF    WS-LAST-CARRIER = EX-SA-CARRIER                        EL327
00344        AND WS-LAST-ACCOUNT = EX-BA-ACCOUNT                        EL327
00345          GO TO 3200-SORT-OUTPUT-PROCEDURE.                        EL327
00346                                                                   EL327
00347      MOVE '0'                    TO  WS-DETAIL1.                  EL327
00348      MOVE WS-AT-CURR-CHARGED     TO  WS-D1-CURR-CHARGED.          EL327
00349      MOVE WS-AT-CURR-NON-CHARGED TO  WS-D1-CURR-NON-CHARGED.      EL327
00350      MOVE 'TOTAL ACCOUNT  '      TO  WS-D1-ACCT-DESC              EL327
00351      MOVE WS-LAST-ACCOUNT        TO  WS-D1-ACCT-NUMBER            EL327
00352      MOVE WS-DETAIL1             TO  PRT.                         EL327
00353      PERFORM WRITE-A-LINE.                                        EL327
00354      MOVE SPACES                 TO  WS-DETAIL1.                  EL327
00355      MOVE WS-DETAIL1             TO  PRT.                         EL327
00356      PERFORM WRITE-A-LINE.                                        EL327
00357      ADD WS-AT-CURR-CHARGED      TO  WS-CT-CURR-CHARGED.          EL327
00358      ADD WS-AT-CURR-NON-CHARGED  TO  WS-CT-CURR-NON-CHARGED.      EL327
00359      MOVE ZERO                   TO  WS-AT-CURR-CHARGED           EL327
00360                                      WS-AT-CURR-NON-CHARGED.      EL327
00361                                                                   EL327
00362      IF     EX-SB-CARRIER = WS-LAST-CARRIER                       EL327
00363         AND WS-LINE-COUNT GREATER THAN +50                        EL327
00364             MOVE +99                TO  WS-LINE-COUNT             EL327
00365      ELSE                                                         EL327
00366         MOVE  1                 TO  P-CTL                         EL327
00367         MOVE SPACES             TO  P-DATA.                       EL327
00368                                                                   EL327
00369      MOVE EX-BA-ACCOUNT         TO  WS-LAST-ACCOUNT.              EL327
00370                                                                   EL327
00371      EJECT                                                        EL327
00372      IF EX-SB-CARRIER = WS-LAST-CARRIER                           EL327
00373          GO TO 3200-SORT-OUTPUT-PROCEDURE.                        EL327
00374                                                                   EL327
00375      MOVE '-'                    TO  WS-DETAIL1.                  EL327
00376                                                                   EL327
00377      MOVE WS-CT-CURR-CHARGED     TO  WS-D1-CURR-CHARGED.          EL327
00378      MOVE WS-CT-CURR-NON-CHARGED TO  WS-D1-CURR-NON-CHARGED.      EL327
00379      MOVE 'TOTAL CARRIER'        TO  WS-D1-INSURED.               EL327
00380                                                                   EL327
00381      MOVE WS-DETAIL1             TO  PRT.                         EL327
00382      PERFORM WRITE-A-LINE.                                        EL327
00383                                                                   EL327
00384      ADD WS-CT-CURR-CHARGED      TO  WS-GT-CURR-CHARGED.          EL327
00385      ADD WS-CT-CURR-NON-CHARGED  TO  WS-GT-CURR-NON-CHARGED.      EL327
00386                                                                   EL327
00387      MOVE ZERO                   TO  WS-CT-CURR-CHARGED           EL327
00388                                      WS-CT-CURR-NON-CHARGED.      EL327
00389                                                                   EL327
00390      IF EX-SB-CARRIER NOT = HIGH-VALUES                           EL327
00391          MOVE +99                TO  WS-LINE-COUNT.               EL327
00392                                                                   EL327
00393      MOVE EX-SB-CARRIER         TO  WS-LAST-CARRIER.              EL327
00394      PERFORM 8100-GET-CARRIER-NAME.                               EL327
00395                                                                   EL327
00396      EJECT                                                        EL327
00397      IF EX-SB-CARRIER NOT = HIGH-VALUES                           EL327
00398          GO TO 3200-SORT-OUTPUT-PROCEDURE.                        EL327
00399                                                                   EL327
00400      MOVE '-'                    TO  WS-DETAIL1.                  EL327
00401                                                                   EL327
00402      MOVE 'TOTAL REPORT'         TO  WS-D1-INSURED.               EL327
00403      MOVE WS-GT-CURR-CHARGED     TO  WS-D1-CURR-CHARGED.          EL327
00404      MOVE WS-GT-CURR-NON-CHARGED TO  WS-D1-CURR-NON-CHARGED.      EL327
00405                                                                   EL327
00406      MOVE WS-DETAIL1             TO  PRT.                         EL327
00407      PERFORM WRITE-A-LINE.                                        EL327
00408                                                                   EL327
00409      GO TO 3900-EXIT.                                             EL327
00410                                                                   EL327
00411      EJECT                                                        EL327
00412  3200-SORT-OUTPUT-PROCEDURE.                                      EL327
00413      MOVE SPACES                 TO  WS-DETAIL1.                  EL327
00414                                                                   EL327
00415      MOVE EX-BA-CLAIM-NO        TO  WS-D1-CLAIM-NUMBER.           EL327
00416      MOVE EX-BA-CERT-NO         TO  WS-D1-CERT-NUMBER.            EL327
00417                                                                   EL327
122702     EVALUATE TRUE
122702     WHEN EX-BA-CLAIM-TYPE = AH-OVERRIDE-L1  
00419          MOVE AH-OVERRIDE-L6     TO  WS-D1-CLAIM-TYPE             EL327

122702     WHEN EX-BA-CLAIM-TYPE = LIFE-OVERRIDE-L1
00422          MOVE LIFE-OVERRIDE-L6   TO  WS-D1-CLAIM-TYPE    

122702     WHEN EX-BA-CLAIM-TYPE = 'I'
122702         MOVE '  IU  '           TO  WS-D1-CLAIM-TYPE

122702     WHEN EX-BA-CLAIM-TYPE = 'G'
122702         MOVE ' GAP  '           TO  WS-D1-CLAIM-TYPE
052614
052614     WHEN EX-BA-CLAIM-TYPE = 'F'
052614         MOVE ' FAM  '           TO  WS-D1-CLAIM-TYPE
100518
022122     WHEN EX-BA-CLAIM-TYPE = 'B'
022122         MOVE ' BRV  '           TO  WS-D1-CLAIM-TYPE
022122
022122     WHEN EX-BA-CLAIM-TYPE = 'H'
022122         MOVE ' HOS  '           TO  WS-D1-CLAIM-TYPE
100518
100518     WHEN EX-BA-CLAIM-TYPE = 'O'
100518         MOVE ' OTH  '           TO  WS-D1-CLAIM-TYPE

122702     END-EVALUATE.
00425                                                                   EL327
00426      MOVE EX-BA-INSURED-LAST-NAME    TO  WS-D1-INSURED.           EL327
00427                                                                   EL327
00428      IF EX-BA-INCURRED-DT NOT = LOW-VALUES                        EL327
00429          MOVE    EX-BA-INCURRED-DT    TO  DC-BIN-DATE-1           EL327
00430          MOVE    SPACES               TO  DC-OPTION-CODE          EL327
00431          PERFORM 8500-DATE-CONVERSION                             EL327
00432          MOVE    DC-GREG-DATE-1-EDIT  TO  WS-D1-INCURRED-DATE.    EL327
00433                                                                   EL327
00434      MOVE EX-BA-EXPENSE-TYPE     TO  WS-D1-EXPENSE-TYPE.          EL327
00435                                                                   EL327
00436      IF EX-BA-CHECK-WRITTEN-DT NOT = LOW-VALUES                   EL327
00437          MOVE    EX-BA-CHECK-WRITTEN-DT TO  DC-BIN-DATE-1         EL327
00438          MOVE    SPACES                 TO  DC-OPTION-CODE        EL327
00439          PERFORM 8500-DATE-CONVERSION                             EL327
00440          MOVE    DC-GREG-DATE-1-EDIT    TO  WS-D1-CHECK-DATE.     EL327
00441                                                                   EL327
00442      MOVE EX-SB-CHECK-NO         TO  WS-D1-CHECK-NUMBER.          EL327
00443                                                                   EL327
00444      IF EX-BA-VOID-DT NOT = LOW-VALUES                            EL327
00445          MULTIPLY -1 BY EX-BA-PAYMENT-AMOUNT.                     EL327
00446                                                                   EL327
00447      IF EX-BA-PAYMENT-TYPE = '5'                                  EL327
00448          MOVE EX-BA-PAYMENT-AMOUNT  TO  WS-D1-CURR-CHARGED        EL327
00449          ADD  EX-BA-PAYMENT-AMOUNT  TO  WS-AT-CURR-CHARGED        EL327
00450      ELSE                                                         EL327
00451          MOVE EX-BA-PAYMENT-AMOUNT  TO  WS-D1-CURR-NON-CHARGED    EL327
00452          ADD  EX-BA-PAYMENT-AMOUNT  TO  WS-AT-CURR-NON-CHARGED.   EL327
00453                                                                   EL327
00454      MOVE WS-DETAIL1             TO  PRT.                         EL327
00455      PERFORM WRITE-A-LINE.                                        EL327
00456                                                                   EL327
00457      IF EX-BA-VOID-DT NOT = LOW-VALUES                            EL327
00458          MOVE SPACES             TO  WS-DETAIL1                   EL327
00459          MOVE 'VOIDED'           TO  WS-D2-VOIDED                 EL327
00460          MOVE EX-BA-VOID-DT      TO  DC-BIN-DATE-1                EL327
00461          MOVE SPACES             TO  DC-OPTION-CODE               EL327
00462          PERFORM 8500-DATE-CONVERSION                             EL327
00463          MOVE DC-GREG-DATE-1-EDIT  TO  WS-D1-CHECK-DATE           EL327
00464          MOVE WS-DETAIL1         TO  PRT                          EL327
00465          PERFORM WRITE-A-LINE.                                    EL327
00466                                                                   EL327
00467      GO TO 3100-SORT-OUTPUT-PROCEDURE.                            EL327
00468                                                                   EL327
00469  3900-EXIT.                                                       EL327
00470      EXIT.                                                        EL327
00471                                                                   EL327
00472      EJECT                                                        EL327
00473  8100-GET-CARRIER-NAME SECTION.                                   EL327
00474      MOVE +1                     TO  WS-INDEX.                    EL327
00475      MOVE EX-SB-CARRIER         TO  WS-H4-CARRIER.                EL327
00476                                                                   EL327
00477  8110-GET-CARRIER-NAME.                                           EL327
00478      IF EX-SB-CARRIER = CARRIER-SUB (WS-INDEX)                    EL327
00479          MOVE CARRIER-PIC (WS-INDEX) TO WS-H5-CARRIER-NAME        EL327
00480      ELSE                                                         EL327
00481          IF WS-INDEX LESS THAN +25                                EL327
00482              ADD +1  TO  WS-INDEX                                 EL327
00483              GO TO 8110-GET-CARRIER-NAME.                         EL327
00484                                                                   EL327
00485  8190-EXIT.                                                       EL327
00486      EXIT.                                                        EL327
00487                                                                   EL327
00488      EJECT                                                        EL327
00489  8500-DATE-CONVERSION SECTION. COPY ELCDCS.                       EL327
00490                                                                   EL327
00491  WRITE-A-LINE SECTION. COPY ELCWAL.                               EL327
00492                                                                   EL327
00493  WRITE-HEADINGS SECTION.                                          EL327
00494                                                                   EL327
00495  WHS-010.                                                         EL327
00496      IF  WS-H2-DATE EQUAL SPACES                                  EL327
00497          MOVE WS-CURRENT-DATE    TO  WS-H2-DATE                   EL327
00498          MOVE COMPANY-NAME       TO  WS-H2-CLIENT-NAME            EL327
00499          MOVE ALPH-DATE          TO  WS-H3-DATE.                  EL327
00500                                                                   EL327
00501      ADD +1  TO  WS-PAGE.                                         EL327
00502      MOVE WS-PAGE                TO  WS-H3-PAGE.                  EL327
00503      MOVE PRT                    TO  WS-SAVE-PRINT-RECORD.        EL327
00504      MOVE ZERO                   TO  WS-LINE-COUNT.               EL327
00505                                                                   EL327
00506      MOVE WS-HEADING1            TO  PRT.                         EL327
00507      MOVE '1'                    TO  X.                           EL327
00508      PERFORM WRITE-PRINTER.                                       EL327
00509                                                                   EL327
00510      MOVE WS-HEADING2            TO  PRT.                         EL327
00511      MOVE ' '                    TO  X.                           EL327
00512      PERFORM WRITE-PRINTER.                                       EL327
00513                                                                   EL327
00514      MOVE WS-HEADING3            TO  PRT.                         EL327
00515      MOVE ' '                    TO  X.                           EL327
00516      PERFORM WRITE-PRINTER.                                       EL327
00517                                                                   EL327
00518      MOVE WS-HEADING4            TO  PRT.                         EL327
00519      MOVE ' '                    TO  X.                           EL327
00520      PERFORM WRITE-PRINTER.                                       EL327
00521                                                                   EL327
00522      MOVE WS-HEADING5            TO  PRT.                         EL327
00523      PERFORM WRITE-PRINTER.                                       EL327
00524                                                                   EL327
00525      MOVE WS-HEADING8            TO  PRT.                         EL327
00526      PERFORM WRITE-PRINTER.                                       EL327
00527                                                                   EL327
00528      MOVE WS-HEADING9            TO  PRT.                         EL327
00529      PERFORM WRITE-PRINTER.                                       EL327
00530                                                                   EL327
00531      MOVE +11                    TO  WS-LINE-COUNT.               EL327
00532                                                                   EL327
00533  WHS-020. COPY ELCWHS2.                                           EL327
00534                                                                   EL327
00535  WRITE-PRINTER SECTION. COPY ELCWPS.                              EL327
00536                                                                   EL327
00537  WPS-020.                                                         EL327
00538                                                                   EL327
00539      IF DTE-FICH NOT = SPACE AND                                  EL327
00540          FICH-OPEN   = SPACE                                      EL327
00541          MOVE 'X' TO FICH-OPEN                                    EL327
00542          OPEN OUTPUT FICH.                                        EL327
00543                                                                   EL327
00544      IF DTE-PRT-OPT = 'S' OR 'T'                                  EL327
00545          IF (REPT-OPEN = SPACE) AND (DTE-ABEND-CD-1 = SPACE)      EL327
00546              OPEN I-O ELREPT                                      EL327
00547              IF DTE-F-1 NOT = ZERO AND                            EL327
00548                 DTE-VSAM-FLAGS NOT = '97'                         EL327
00549                  MOVE DTE-VSAM-FLAGS  TO  WS-ABEND-FILE-STATUS    EL327
00550                  MOVE 'ERROR OCCURED OPEN - ELREPT'               EL327
00551                                  TO  WS-ABEND-MESSAGE             EL327
00552                  GO TO ABEND-PGM                                  EL327
00553              ELSE                                                 EL327
00554                  MOVE '1'                   TO REPT-OPEN          EL327
00555                  MOVE DTE-CLASIC-COMPANY-CD TO RF-COMPANY-CD      EL327
00556                  MOVE '1'                   TO RF-RECORD-TYPE     EL327
00557                  MOVE OLC-REPORT-NAME       TO RF-REPORT-ID       EL327
00558                  MOVE ZERO                  TO RF-LINE-NUMBER     EL327
00559                  START ELREPT  KEY NOT LESS RF-CONTROL-PRIMARY    EL327
00560                  PERFORM DTE-REPORT-DELETE THRU DTE-DELETE-EXIT   EL327
00561                  MOVE DTE-CLASIC-COMPANY-CD TO RF-COMPANY-CD      EL327
00562                  MOVE '2'                   TO RF-RECORD-TYPE     EL327
00563                  MOVE OLC-REPORT-NAME       TO RF-REPORT-ID       EL327
00564                  MOVE ZERO                  TO RF-LINE-NUMBER     EL327
00565                  START ELREPT  KEY NOT LESS RF-CONTROL-PRIMARY    EL327
00566                  PERFORM DTE-REPORT-DELETE THRU DTE-DELETE-EXIT   EL327
00567                  MOVE DTE-CLASIC-COMPANY-CD TO RF-COMPANY-CD      EL327
00568                  MOVE '1'                   TO RF-RECORD-TYPE     EL327
00569                  MOVE OLC-REPORT-NAME       TO RF-REPORT-ID       EL327
00570                  MOVE SPACES                TO RF-REPORT-LINE-133.EL327
00571                                                                   EL327
00572      IF DTE-ABEND-CD-1 = '81' AND                                 EL327
00573         DTE-PRT-OPT    = 'S'                                      EL327
00574          MOVE +0302  TO WS-RETURN-CODE                            EL327
00575          GO TO ABEND-PGM.                                         EL327
00576                                                                   EL327
00577      IF DTE-PRT-OPT = 'S' OR 'T'                                  EL327
00578          MOVE X      TO RF-CTL-CHAR-133                           EL327
00579          MOVE P-DATA TO RF-DATA-133                               EL327
00580              IF DTE-ABEND-CD-1 = SPACES                           EL327
00581                  ADD +1 TO DTE-TOT-LINES                          EL327
00582                  MOVE DTE-TOT-LINES TO RF-LINE-NUMBER             EL327
00583                  WRITE REPORT-SAVE-FILE                           EL327
00584                      INVALID KEY                                  EL327
00585                          MOVE '88' TO DTE-ABEND-CD-1              EL327
00586                          CLOSE ELREPT                             EL327
00587                          MOVE SPACE TO REPT-OPEN.                 EL327
00588                                                                   EL327
00589      IF DTE-FICH NOT = SPACE                                      EL327
00590          MOVE X TO P-CTL                                          EL327
00591          WRITE FICH-REC FROM PRT.                                 EL327
00592                                                                   EL327
00593      IF DTE-PRT-OPT = 'P' OR 'B' OR 'T'                           EL327
00594          MOVE X TO P-CTL                                          EL327
00595          WRITE PRT.                                               EL327
00596                                                                   EL327
00597      GO TO DTE-PRINT-EXIT.                                        EL327
00598                                                                   EL327
00599  DTE-REPORT-DELETE.                                               EL327
00600      IF DTE-F-1 NOT = ZERO                                        EL327
00601          MOVE ZERO TO DTE-VSAM-FLAGS                              EL327
00602          GO TO DTE-DELETE-EXIT.                                   EL327
00603                                                                   EL327
00604      READ ELREPT   NEXT RECORD                                    EL327
00605            AT END   GO TO DTE-DELETE-EXIT.                        EL327
00606                                                                   EL327
00607      IF DTE-CLASIC-COMPANY-CD = RF-COMPANY-CD  AND                EL327
00608         OLC-REPORT-NAME       = RF-REPORT-ID                      EL327
00609          DELETE ELREPT RECORD                                     EL327
00610          GO TO DTE-REPORT-DELETE.                                 EL327
00611                                                                   EL327
00612  DTE-DELETE-EXIT.                                                 EL327
00613      EXIT.                                                        EL327
00614                                                                   EL327
00615  DTE-PRINT-EXIT.                                                  EL327
00616      EXIT.                                                        EL327
00617                                                                   EL327
00618      EJECT                                                        EL327
00619  OPEN-FILES SECTION.                                              EL327
00620                                                                   EL327
00621  OFS-010.                                                         EL327
00622      OPEN INPUT REPORTS-EXTRACT-FILE                              EL327
00623           OUTPUT PRNTR.                                           EL327
00624                                                                   EL327
00625  OFS-EXIT.                                                        EL327
00626      EXIT.                                                        EL327
00627                                                                   EL327
00628  CLOSE-FILES SECTION.                                             EL327
00629                                                                   EL327
00630  CFS-010. COPY ELCPRTCX.                                          EL327
00631      CLOSE REPORTS-EXTRACT-FILE                                   EL327
00632            PRNTR.                                                 EL327
00633                                                                   EL327
00634  CFS-EXIT.                                                        EL327
00635      EXIT.                                                        EL327
00636                                                                   EL327
00637                                                                   EL327
00638  ABEND-PGM SECTION. COPY ELCABEND.                                EL327
00639                                                                   EL327
