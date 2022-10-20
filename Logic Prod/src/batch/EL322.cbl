00001  IDENTIFICATION DIVISION.                                         04/18/98
00002                                                                   EL322
00003  PROGRAM-ID.                 EL322 .                                 LV005
00004 *              PROGRAM CONVERTED BY                               EL322
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL322
00006 *              CONVERSION DATE 02/14/96 13:40:07.                 EL322
00007 *           PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE             EL322
00008 *                            VMOD=2.008                           EL322
00009                                                                   EL322
00009                                                                   EL322
00010 *AUTHOR.     LOGIC,INC.                                           EL322
00011 *            DALLAS, TEXAS.                                       EL322
00012                                                                   EL322
00016 *SECURITY.   *****************************************************EL322
00017 *            *                                                   *EL322
00018 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *EL322
00019 *            *                                                   *EL322
00020 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL322
00021 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL322
00022 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *EL322
00023 *            *                                                   *EL322
00024 *            *****************************************************EL322
00026 *REMARKS.                                                         EL322
00027 *        THIS PROGRAM PRINTS A REGISTER OF ALL CHECKS ISSUED      EL322
00028 *    IN THE REPORTED MONTH.   BOTH ONLINE CREATED AND OFFLINE     EL322
00029 *    CHECKS ARE REPORTED.                                         EL322
122602******************************************************************
122602*                   C H A N G E   L O G
122602*
122602* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
122602*-----------------------------------------------------------------
122602*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
122602* EFFECTIVE    NUMBER
122602*-----------------------------------------------------------------
122602* 122602    2001061800003  SMVA  ADD PROCESSING FOR NEW CLM TYP I
121203* 121203    2003080800002  SMVA  ADD PROCESSING FOR NEW CLM TYP G
052614* 052614    2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
022122* 022122  CR2021100800003  PEMA  Add B and H claim types
122602******************************************************************
00030                                                                   EL322
00031      EJECT                                                        EL322
00032  ENVIRONMENT DIVISION.                                            EL322
00033                                                                   EL322
00034  INPUT-OUTPUT SECTION.                                            EL322
00035                                                                   EL322
00036  FILE-CONTROL.                                                    EL322
00037                                                                   EL322
00038      SELECT REPORTS-EXTRACT-FILE                                  EL322
00039                              ASSIGN TO SYS010-UT-2400-S-SYS010.   EL322
00040                                                                   EL322
00041      SELECT ELI-TAPE-OUT     ASSIGN TO SYS011-UT-2400-S-SYS011.   EL322
00042                                                                   EL322
00043      SELECT DISK-DATE        ASSIGN TO SYS019-FBA1-S-SYS019.      EL322
00044                                                                   EL322
00045      SELECT PRNTR            ASSIGN TO SYS008-UR-1403-S-SYS008.   EL322
00046                                                                   EL322
00047      SELECT FICH             ASSIGN TO SYS020-UT-2400-S-SYS020.   EL322
00048                                                                   EL322
00049      SELECT ELREPT           ASSIGN TO SYS018-FBA1-ELREPT         EL322
00050                              ORGANIZATION IS INDEXED              EL322
00051                              ACCESS IS DYNAMIC                    EL322
00052                              RECORD KEY IS RF-CONTROL-PRIMARY     EL322
00053                              FILE STATUS IS DTE-VSAM-FLAGS.       EL322
00054                                                                   EL322
00055      EJECT                                                        EL322
00056  DATA DIVISION.                                                   EL322
00057                                                                   EL322
00058  FILE SECTION.                                                    EL322
00059                                                                   EL322
00060  FD  REPORTS-EXTRACT-FILE COPY ELCEXTFD.                          EL322
00061                           COPY ELCEXTR.                           EL322
00062                                                                   EL322
00063      EJECT                                                        EL322
00064  FD  ELI-TAPE-OUT                                                 EL322
00065      BLOCK CONTAINS 0 RECORDS
00066      RECORDING MODE F.                                            EL322
00067  01  ELI-TAPE-RECORD.                                             EL322
00068      05  FILLER                  PIC X(250).                      EL322
00069      05  ELI-CHK-WRITTEN-DATE    PIC 9(6).                        EL322
00070                                                                   EL322
00071  FD  DISK-DATE               COPY ELCDTEFD.                       EL322
00072                                                                   EL322
00073  FD  PRNTR                   COPY ELCPRTFD.                       EL322
00074                                                                   EL322
00075  FD  FICH                    COPY ELCFCHFD.                       EL322
00076                                                                   EL322
00077  FD  ELREPT                  COPY ELCRPTFD.                       EL322
00078                                                                   EL322
00079                              COPY ELCREPT.                        EL322
00080                                                                   EL322
00081      EJECT                                                        EL322
00082  WORKING-STORAGE SECTION.                                         EL322
00083  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      EL322
00084                                                                   EL322
00085  77  FILLER  PIC X(32)   VALUE '********************************'.EL322
00086  77  FILLER  PIC X(32)   VALUE '*     EL322  WORKING STORAGE   *'.EL322
00087  77  FILLER  PIC X(32)   VALUE '********** V/M 2.008 ***********'.EL322
00088                                                                   EL322
00089  01  FILLER                          COMP-3.                      EL322
00090      05  WS-LINE-COUNT               PIC S9(3)       VALUE +99.   EL322
00091      05  WS-LINE-COUNT-MAX           PIC S9(3)       VALUE +60.   EL322
00092      05  WS-PAGE                     PIC S9(5)       VALUE ZERO.  EL322
00093      05  WS-REPORT-SW                PIC S9          VALUE +1.    EL322
00094      05  WS-PRINT-SW                 PIC S9          VALUE ZERO.  EL322
00095      05  WS-RECORD-COUNT             PIC S9(9)       VALUE ZERO.  EL322
00096      05  WS-RETURN-CODE              PIC S9(3)       VALUE ZERO.  EL322
00097      05  WS-ZERO                     PIC S9          VALUE ZERO.  EL322
00098                                                                   EL322
00099      05  WS-CARRIER-TOTAL            PIC S9(9)V99 VALUE ZERO.     EL322
00100      05  WS-CARRIER-REINS-TOTAL      PIC S9(9)V99 VALUE ZERO.     EL322
00101      05  WS-CHECK-TOTAL              PIC S9(9)V99 VALUE ZERO.     EL322
00102      05  WS-CHECK-REINS-TOTAL        PIC S9(9)V99 VALUE ZERO.     EL322
00103                                                                   EL322
00104      EJECT                                                        EL322
00105  01  FILLER                          COMP SYNC.                   EL322
00106      05  PGM-SUB                     PIC S9(4)       VALUE +322.  EL322
00107      05  WS-INDEX                    PIC S9(4)       VALUE ZERO.  EL322
00108                                                                   EL322
00109  01  FILLER.                                                      EL322
00110      05  ABEND-CODE                  PIC X(4).                    EL322
00111      05  ABEND-OPTION                PIC X.                       EL322
00112      05  OLC-REPORT-NAME             PIC X(5) VALUE 'EL322'.      EL322
00113      05  X                           PIC X           VALUE SPACE. EL322
00114                                                                   EL322
00115      05  WS-PAID-DATE.                                            EL322
00116          10  WS-PAID-MO              PIC 99.                      EL322
00117          10  FILLER                  PIC X.                       EL322
00118          10  WS-PAID-DA              PIC 99.                      EL322
00119          10  FILLER                  PIC X.                       EL322
00120          10  WS-PAID-YR              PIC 99.                      EL322
00121                                                                   EL322
00122      05  WS-SELECT-DATE.                                          EL322
00123          10  WS-SELECT-MO            PIC 99.                      EL322
00124          10  FILLER                  PIC X.                       EL322
00125          10  WS-SELECT-DA              PIC 99.                    EL322
00126          10  FILLER                  PIC X.                       EL322
00127          10  WS-SELECT-YR              PIC 99.                    EL322
00128                                                                   EL322
00129      05  WS-LAST-CARRIER             PIC X           VALUE SPACES.EL322
00130                                                                   EL322
00131      05  WS-SAVE-PRINT-RECORD        PIC X(133)      VALUE SPACES.EL322
00132                                                                   EL322
00133      05  WS-ABEND-MESSAGE            PIC X(80)       VALUE SPACES.EL322
00134                                                                   EL322
00135      05  WS-ABEND-FILE-STATUS        PIC XX          VALUE ZERO.  EL322
00136                                                                   EL322
00137      05  WS-FILE-ERROR-MESSAGE.                                   EL322
00138          10  FILLER                  PIC X(24)       VALUE        EL322
00139              'ERROR OCCURED OPENING - '.                          EL322
00140          10  WS-FEM-FILE-NAME        PIC X(8).                    EL322
00141                                                                   EL322
00142      EJECT                                                        EL322
00143  01  WS-HEADING1.                                                 EL322
00144      05  FILLER                      PIC X(51)       VALUE '1'.   EL322
00145      05  WS-H1-TITLE                 PIC X(69)       VALUE        EL322
00146          'CHECK USAGE REPORT'.                                    EL322
00147      05  WS-H1-REPORT-NUMBER         PIC X(9) VALUE 'EL322'.      EL322
00148                                                                   EL322
00149  01  WS-HEADING2.                                                 EL322
00150      05  FILLER                      PIC X(46)       VALUE SPACES.EL322
00151      05  WS-H2-CLIENT-NAME           PIC X(74)       VALUE SPACES.EL322
00152      05  WS-H2-DATE                  PIC X(8)        VALUE SPACES.EL322
00153      05  FILLER                      PIC X           VALUE SPACES.EL322
00154                                                                   EL322
00155  01  WS-HEADING3.                                                 EL322
00156      05  FILLER                      PIC X(51)       VALUE SPACES.EL322
00157      05  WS-H3-DATE                  PIC X(69)       VALUE SPACES.EL322
00158      05  FILLER                      PIC X(5)        VALUE 'PAGE'.EL322
00159      05  WS-H3-PAGE                  PIC ZZ,ZZ9.                  EL322
00160      05  FILLER                      PIC X(11)       VALUE SPACES.EL322
00161                                                                   EL322
00162  01  WS-HEADING4.                                                 EL322
00163      05  FILLER                      PIC X(29)       VALUE        EL322
00164          '0CHECK'.                                                EL322
00165      05  FILLER                      PIC X(42)       VALUE        EL322
00166          'DATE     CONTROL'.                                      EL322
00167      05  FILLER                      PIC X(36)       VALUE        EL322
00168          'TIMES   PAYMENT'.                                       EL322
00169      05  FILLER                      PIC X(26)       VALUE        EL322
00170          'CLAIM'.                                                 EL322
00171                                                                   EL322
00172  01  WS-HEADING5.                                                 EL322
00173      05  FILLER                      PIC X(71)       VALUE        EL322
00174          ' NUMBER  CAR      AMOUNT     PAID      GROUP   USED FOR EL322
00175 -        ' '.                                                     EL322
00176      05  FILLER                      PIC X(62)       VALUE        EL322
00177          'PRINTED    BY    CLAIM NO  CERT NO   TYPE'.             EL322
00178                                                                   EL322
00179      EJECT                                                        EL322
00180  01  WS-DETAIL1.                                                  EL322
00181      05  FILLER                      PIC X.                       EL322
00182      05  WS-D1-CHECK-NUMBER          PIC X(7).                    EL322
00183      05  FILLER                      PIC X(3).                    EL322
00184      05  WS-D1-CARRIER               PIC X.                       EL322
00185      05  FILLER                      PIC XX.                      EL322
00186      05  WS-D1-AMOUNT                PIC Z,ZZZ,ZZ9.99-.           EL322
00187      05  FILLER                      PIC X.                       EL322
00188      05  WS-D1-DATE-PAID             PIC X(8).                    EL322
00189      05  FILLER                      PIC XX.                      EL322
00190      05  WS-D1-CONTROL-GROUP         PIC Z(7)9.                   EL322
00191      05  FILLER                      PIC XX.                      EL322
00192      05  WS-D1-USED-FOR              PIC X(20).                   EL322
00193      05  FILLER                      PIC X(6).                    EL322
00194      05  WS-D1-TIMES-PRINTED         PIC 9.                       EL322
00195      05  FILLER                      PIC X(6).                    EL322
00196      05  WS-D1-PAYMENT-BY            PIC X(4).                    EL322
00197      05  FILLER                      PIC X(3).                    EL322
00198      05  WS-D1-CLAIM-NO              PIC X(7).                    EL322
00199      05  FILLER                      PIC X.                       EL322
00200      05  WS-D1-CERT-NO               PIC X(11).                   EL322
00201      05  FILLER                      PIC X.                       EL322
00202      05  WS-D1-CLAIM-TYPE            PIC X(6).                    EL322
00203      05  FILLER                      PIC X.                       EL322
00204      05  WS-D1-MESSAGE               PIC X(18).                   EL322
00205                                                                   EL322
00206      EJECT                                                        EL322
00207  01  WS-DETAIL2                      REDEFINES                    EL322
00208      WS-DETAIL1.                                                  EL322
00209      05  FILLER                      PIC X(23).                   EL322
00210      05  WS-D2-START-CHECK-NO        PIC X(7).                    EL322
00211      05  FILLER                      PIC X(6).                    EL322
00212      05  WS-D2-END-CHECK-NO          PIC X(7).                    EL322
00213      05  FILLER                      PIC X(90).                   EL322
00214                                                                   EL322
00215      EJECT                                                        EL322
00216  01  WS-TOTAL-LINE1                  REDEFINES                    EL322
00217      WS-DETAIL1.                                                  EL322
00218      05  FILLER                      PIC X(7).                    EL322
00219      05  WS-T1-AMOUNT                PIC ZZZ,ZZZ,ZZ9.99-.         EL322
00220      05  FILLER                      PIC X(1).                    EL322
00221      05  WS-T1-DESCRIPTION           PIC X(50).                   EL322
00222      05  FILLER                      PIC X(60).                   EL322
00223                                                                   EL322
00224      EJECT                                                        EL322
00225                COPY ELCDTECX.                                        CL**2
00226                                                                   EL322
00227                COPY ELCDTEVR.                                        CL**3
00228                                                                      CL**3
00229                COPY ELCDATE.                                         CL**5
00230                                                                   EL322
00231      EJECT                                                        EL322
00232  PROCEDURE DIVISION.                                              EL322
00233                                                                   EL322
00234  0000-DATE-CARD-READ SECTION. COPY ELCDTERX.                         CL**2
00235                                                                   EL322
00236  1000-MAIN-LOGIC SECTION.                                         EL322
00237                                                                   EL322
00238      PERFORM OPEN-FILES.                                          EL322
00239                                                                   EL322
00240      PERFORM 3000-PRINT-REPORT.                                   EL322
00241                                                                   EL322
00242      PERFORM CLOSE-FILES.                                         EL322
00243                                                                   EL322
00244      GOBACK.                                                      EL322
00245                                                                   EL322
00246      EJECT                                                        EL322
00247  3000-PRINT-REPORT SECTION.                                       EL322
00248                                                                   EL322
00249  3100-PRINT-REPORT.                                               EL322
00250      READ REPORTS-EXTRACT-FILE                                    EL322
00251          AT END                                                   EL322
00252              MOVE HIGH-VALUES    TO  EX-COMPANY-CD                EL322
00253              GO TO 3110-PRINT-REPORT.                             EL322
00254                                                                   EL322
00255      IF EX-POSITIONING-CODE LESS THAN '9'                         EL322
00256          GO TO 3100-PRINT-REPORT.                                 EL322
00257                                                                   EL322
00258      IF EX-POSITIONING-CODE GREATER THAN '9'                      EL322
00259          MOVE HIGH-VALUES  TO  EX-COMPANY-CD                      EL322
00260          GO TO 3110-PRINT-REPORT.                                 EL322
00261                                                                   EL322
00262      IF EX-EXTRACT-CODE LESS THAN 'C'                             EL322
00263          GO TO 3100-PRINT-REPORT.                                 EL322
00264                                                                   EL322
00265      IF EX-EXTRACT-CODE GREATER THAN 'C'                          EL322
00266          MOVE HIGH-VALUES  TO  EX-COMPANY-CD                      EL322
00267          GO TO  3110-PRINT-REPORT.                                EL322
00268                                                                   EL322
00269      IF EX-COMPANY-CD LESS THAN DTE-CLASIC-COMPANY-CD             EL322
00270          GO TO 3100-PRINT-REPORT.                                 EL322
00271                                                                   EL322
00272      IF EX-COMPANY-CD GREATER THAN DTE-CLASIC-COMPANY-CD          EL322
00273          MOVE HIGH-VALUES  TO  EX-COMPANY-CD                      EL322
00274          GO TO  3110-PRINT-REPORT.                                EL322
00275                                                                   EL322
00276      IF EX-RECORD-TYPE GREATER THAN 'A'                           EL322
00277          MOVE HIGH-VALUES  TO  EX-COMPANY-CD                      EL322
00278          GO TO  3110-PRINT-REPORT.                                EL322
00279                                                                   EL322
00280      MOVE    EX-CA-CHECK-WRITTEN-DATE TO  DC-BIN-DATE-1.          EL322
00281      MOVE    SPACES                   TO  DC-OPTION-CODE.         EL322
00282      PERFORM 8500-DATE-CONVERSION.                                EL322
00283      MOVE    DC-GREG-DATE-1-EDIT      TO  WS-PAID-DATE.           EL322
00284                                                                   EL322
00285      MOVE    EX-CA-PMT-SELECT-DT      TO  DC-BIN-DATE-1.          EL322
00286      MOVE    SPACES                   TO  DC-OPTION-CODE.         EL322
00287      PERFORM 8500-DATE-CONVERSION.                                EL322
00288      MOVE    DC-GREG-DATE-1-EDIT      TO  WS-SELECT-DATE.         EL322
00289                                                                   EL322
00290      IF RUN-MO = WS-SELECT-MO AND                                 EL322
00291         RUN-YR = WS-SELECT-YR                                     EL322
00292          GO TO 3110-PRINT-REPORT.                                 EL322
00293                                                                   EL322
00294      GO TO 3100-PRINT-REPORT.                                     EL322
00295                                                                   EL322
00296      EJECT                                                        EL322
00297  3110-PRINT-REPORT.                                               EL322
00298      IF WS-RECORD-COUNT LESS THAN +1                              EL322
00299        AND EX-COMPANY-CD = HIGH-VALUES                            EL322
00300          MOVE '0NO RECORDS EXTRACTED FOR THIS REPORT' TO PRT      EL322
00301          PERFORM WRITE-A-LINE                                     EL322
00302          GO TO 3900-EXIT.                                         EL322
00303                                                                   EL322
00304      IF  WS-LAST-CARRIER EQUAL SPACES                             EL322
00305          MOVE EX-SB-CARRIER   TO  WS-LAST-CARRIER.                EL322
00306                                                                   EL322
00307      ADD +1  TO  WS-RECORD-COUNT.                                 EL322
00308                                                                   EL322
00309      IF    EX-SB-CARRIER = WS-LAST-CARRIER                        EL322
00310        AND EX-COMPANY-CD = DTE-CLASIC-COMPANY-CD                  EL322
00311            GO TO 3300-PRINT-REPORT.                               EL322
00312                                                                   EL322
00313      MOVE    '-'                 TO  WS-TOTAL-LINE1.              EL322
00314      MOVE    'TOTAL CARRIER'     TO  WS-T1-DESCRIPTION.           EL322
00315      MOVE    WS-CARRIER-TOTAL    TO  WS-T1-AMOUNT.                EL322
00316      ADD     WS-CARRIER-TOTAL    TO  WS-CHECK-TOTAL.              EL322
00317      MOVE    ZERO                TO  WS-CARRIER-TOTAL.            EL322
00318      MOVE    WS-TOTAL-LINE1      TO  PRT.                         EL322
00319      PERFORM WRITE-A-LINE.                                        EL322
00320                                                                   EL322
00321      MOVE    SPACES              TO  PRT.                         EL322
00322      MOVE    '0'                     TO  WS-TOTAL-LINE1.          EL322
00323      MOVE    'TOTAL CLAIMS WITH REIN ONLY CERTIFICATES'           EL322
00324                                      TO  WS-T1-DESCRIPTION.       EL322
00325      MOVE    WS-CARRIER-REINS-TOTAL  TO  WS-T1-AMOUNT.            EL322
00326      ADD     WS-CARRIER-REINS-TOTAL  TO  WS-CHECK-REINS-TOTAL.    EL322
00327      MOVE    ZERO                    TO  WS-CARRIER-REINS-TOTAL.  EL322
00328      MOVE    WS-TOTAL-LINE1      TO  PRT.                         EL322
00329      PERFORM WRITE-A-LINE.                                        EL322
00330      MOVE    EX-SB-CARRIER       TO  WS-LAST-CARRIER.             EL322
00331                                                                   EL322
00332      EJECT                                                        EL322
00333  3200-PRINT-REPORT.                                               EL322
00334      IF EX-COMPANY-CD NOT = HIGH-VALUES                           EL322
00335          MOVE +99  TO  WS-LINE-COUNT                              EL322
00336          GO TO 3300-PRINT-REPORT.                                    CL**4
00337                                                                   EL322
00338      MOVE    +1                 TO  WS-REPORT-SW.                 EL322
00339      MOVE    '-'                TO  WS-TOTAL-LINE1.               EL322
00340                                                                   EL322
00341      MOVE    '(NOT INCLUDING MULTIPLE PRINTINGS)'                 EL322
00342                                 TO  WS-T1-DESCRIPTION.            EL322
00343                                                                   EL322
00344      MOVE    WS-CHECK-TOTAL     TO  WS-T1-AMOUNT.                 EL322
00345      MOVE    WS-TOTAL-LINE1     TO  PRT.                          EL322
00346      PERFORM WRITE-A-LINE.                                        EL322
00347                                                                   EL322
00348      MOVE    SPACES             TO  PRT.                          EL322
00349      MOVE    '0'                TO  WS-TOTAL-LINE1.               EL322
00350                                                                   EL322
00351      MOVE    'TOTAL REPORT CLAIMS WITH REIN ONLY CERTIFICATES'    EL322
00352                                      TO  WS-T1-DESCRIPTION.       EL322
00353                                                                   EL322
00354      MOVE    WS-CHECK-REINS-TOTAL    TO  WS-T1-AMOUNT.            EL322
00355      MOVE    WS-TOTAL-LINE1     TO  PRT.                          EL322
00356      PERFORM WRITE-A-LINE.                                        EL322
00357      GO TO 3900-EXIT.                                                CL**4
00358                                                                   EL322
00359      EJECT                                                        EL322
00360  3300-PRINT-REPORT.                                               EL322
00361      MOVE SPACES                TO  WS-DETAIL1.                   EL322
00362      MOVE EX-SB-CHECK-NO        TO  WS-D1-CHECK-NUMBER.           EL322
00363      MOVE EX-SB-CARRIER         TO  WS-D1-CARRIER.                EL322
00364      MOVE EX-CA-PAYMENT-AMOUNT  TO  WS-D1-AMOUNT.                 EL322
00365                                                                   EL322
00366      IF EX-CA-ENTRY-TYPE = 'Q'                                    EL322
00367          IF  EX-CA-CERT-STATUS = '9'                              EL322
00368              ADD EX-CA-PAYMENT-AMOUNT TO WS-CARRIER-REINS-TOTAL   EL322
00369          ELSE                                                     EL322
00370              ADD EX-CA-PAYMENT-AMOUNT TO WS-CARRIER-TOTAL.        EL322
00371                                                                   EL322
00372      IF EX-CA-CHECK-WRITTEN-DATE NOT = LOW-VALUES                 EL322
00373         MOVE    EX-CA-CHECK-WRITTEN-DATE TO DC-BIN-DATE-1         EL322
00374         MOVE    SPACES                   TO  DC-OPTION-CODE       EL322
00375         PERFORM 8500-DATE-CONVERSION                              EL322
00376         MOVE    DC-GREG-DATE-1-EDIT      TO  WS-D1-DATE-PAID.     EL322
00377                                                                   EL322
00378      IF DTE-CLIENT = 'ELI' OR 'ELT'                               EL322
00379         MOVE  REPORTS-EXTRACT-RECORD TO ELI-TAPE-RECORD           EL322
00380         MOVE  DC-GREG-DATE-1-MDY     TO ELI-CHK-WRITTEN-DATE      EL322
00381         WRITE ELI-TAPE-RECORD.                                    EL322
00382                                                                   EL322
00383      MOVE EX-CA-CONTROL-NUMBER   TO  WS-D1-CONTROL-GROUP.         EL322
00384                                                                   EL322
00385      IF  EX-CA-PAYMENT-TYPE = '1'                                 EL322
00386          MOVE 'PARTIAL PAYMENT'           TO  WS-D1-USED-FOR      EL322
00387      ELSE                                                         EL322
00388        IF EX-CA-PAYMENT-TYPE = '2'                                EL322
00389           MOVE 'FINAL PAYMENT'            TO  WS-D1-USED-FOR      EL322
00390        ELSE                                                       EL322
00391          IF EX-CA-PAYMENT-TYPE = '3'                              EL322
00392             MOVE 'LUMP SUM PAYMENT'       TO  WS-D1-USED-FOR      EL322
00393          ELSE                                                     EL322
00394            IF EX-CA-PAYMENT-TYPE = '4'                            EL322
00395               MOVE 'ADDITIONAL PAYMENT'   TO  WS-D1-USED-FOR      EL322
00396            ELSE                                                   EL322
00397              IF EX-CA-PAYMENT-TYPE = '5'                          EL322
00398                 MOVE 'CHARGABLE EXPENSE'  TO  WS-D1-USED-FOR      EL322
00399              ELSE                                                 EL322
00400                IF EX-CA-PAYMENT-TYPE = '6'                        EL322
00401                   MOVE 'OTHER EXPENSE'    TO  WS-D1-USED-FOR      EL322
00402                ELSE                                               EL322
00403                   MOVE EX-CA-PAYMENT-TYPE TO  WS-D1-USED-FOR.     EL322
00404                                                                   EL322
00405      IF EX-CA-ENTRY-TYPE = 'A'                                    EL322
00406          MOVE 'ALIGNMENT CHECK'       TO  WS-D1-USED-FOR          EL322
00407          MOVE '*** ALIGNMENT ***'     TO  WS-D1-MESSAGE           EL322
00408      ELSE                                                         EL322
00409          IF EX-CA-ENTRY-TYPE = 'S'                                EL322
00410             MOVE 'SPOILED CHECK'         TO  WS-D1-USED-FOR       EL322
00411             MOVE '***  SPOILED  ***'     TO  WS-D1-MESSAGE        EL322
00412          ELSE                                                     EL322
00413              IF EX-CA-ENTRY-TYPE = 'X'                            EL322
00414                 MOVE 'ABORTED CHECK'         TO  WS-D1-USED-FOR   EL322
00415                 MOVE '***  ABORTED  ***'     TO  WS-D1-MESSAGE.   EL322
00416                                                                   EL322
00417      MOVE EX-CA-TIMES-PRINTED  TO  WS-D1-TIMES-PRINTED.           EL322
00418                                                                   EL322
00419      IF  EX-CA-ENTRY-TYPE NOT = 'A'                               EL322
00420          MOVE EX-CA-CHECK-BY-USER   TO  WS-D1-PAYMENT-BY          EL322
00421          MOVE EX-CA-CLAIM-NO        TO  WS-D1-CLAIM-NO            EL322
00422          MOVE EX-CA-CERT-NO         TO  WS-D1-CERT-NO             EL322
122602         EVALUATE TRUE
122602         WHEN EX-CA-CLAIM-TYPE = LIFE-OVERRIDE-L1    
00424              MOVE LIFE-OVERRIDE-L6  TO  WS-D1-CLAIM-TYPE          EL322

122602         WHEN EX-CA-CLAIM-TYPE = AH-OVERRIDE-L1
00426              MOVE AH-OVERRIDE-L6    TO  WS-D1-CLAIM-TYPE

122602         WHEN EX-CA-CLAIM-TYPE = 'I'
122602             MOVE 'IU'              TO  WS-D1-CLAIM-TYPE
121203
121203         WHEN EX-CA-CLAIM-TYPE = 'G'
121203             MOVE 'GP'              TO  WS-D1-CLAIM-TYPE
052614
052614         WHEN EX-CA-CLAIM-TYPE = 'F'
052614             MOVE 'FL'              TO  WS-D1-CLAIM-TYPE
100518
022122         WHEN EX-CA-CLAIM-TYPE = 'B'
022122             MOVE 'BR'              TO  WS-D1-CLAIM-TYPE
022122
022122         WHEN EX-CA-CLAIM-TYPE = 'H'
022122             MOVE 'HS'              TO  WS-D1-CLAIM-TYPE
100518
100518         WHEN EX-CA-CLAIM-TYPE = 'O'
100518             MOVE 'OT'              TO  WS-D1-CLAIM-TYPE
122602         END-EVALUATE.

00427                                                                   EL322
00428      IF EX-CA-PAYMENT-AMOUNT LESS THAN ZERO                       EL322
00429          AND EX-CA-CERT-STATUS = '9'                              EL322
00430              MOVE 'V O I D / R E I N '   TO  WS-D1-MESSAGE        EL322
00431      ELSE                                                         EL322
00432          IF  EX-CA-CERT-STATUS = '9'                              EL322
00433              MOVE ' *** R E I N  *** '   TO  WS-D1-MESSAGE        EL322
00434          ELSE                                                     EL322
00435              IF EX-CA-PAYMENT-AMOUNT LESS THAN ZERO               EL322
00436              MOVE ' *** V O I D  *** '   TO  WS-D1-MESSAGE.       EL322
00437                                                                   EL322
00438      MOVE    WS-DETAIL1             TO  PRT.                      EL322
00439      PERFORM WRITE-A-LINE.                                        EL322
00440                                                                   EL322
00441      GO TO 3100-PRINT-REPORT.                                     EL322
00442                                                                   EL322
00443  3900-EXIT.                                                       EL322
00444      EXIT.                                                        EL322
00445                                                                   EL322
00446      EJECT                                                        EL322
00447  8500-DATE-CONVERSION SECTION. COPY ELCDCS.                       EL322
00448                                                                   EL322
00449      EJECT                                                        EL322
00450  WRITE-A-LINE SECTION. COPY ELCWAL.                               EL322
00451                                                                   EL322
00452      EJECT                                                        EL322
00453  WRITE-HEADINGS SECTION.                                          EL322
00454                                                                   EL322
00455  WHS-010.                                                         EL322
00456      IF  WS-H2-DATE EQUAL SPACES                                  EL322
00457          MOVE WS-CURRENT-DATE    TO  WS-H2-DATE                   EL322
00458          MOVE COMPANY-NAME       TO  WS-H2-CLIENT-NAME            EL322
00459          MOVE ALPH-DATE          TO  WS-H3-DATE.                  EL322
00460                                                                   EL322
00461      ADD +1  TO  WS-PAGE.                                         EL322
00462      MOVE WS-PAGE                TO  WS-H3-PAGE.                  EL322
00463      MOVE PRT                    TO  WS-SAVE-PRINT-RECORD.        EL322
00464      MOVE ZERO                   TO  WS-LINE-COUNT.               EL322
00465                                                                   EL322
00466      MOVE WS-HEADING1            TO  PRT.                         EL322
00467      MOVE '1'                    TO  X.                           EL322
00468      PERFORM WRITE-PRINTER.                                       EL322
00469                                                                   EL322
00470      MOVE WS-HEADING2            TO  PRT.                         EL322
00471      MOVE ' '                    TO  X.                           EL322
00472      PERFORM WRITE-PRINTER.                                       EL322
00473                                                                   EL322
00474      MOVE WS-HEADING3            TO  PRT.                         EL322
00475      MOVE ' '                    TO  X.                           EL322
00476      PERFORM WRITE-PRINTER.                                       EL322
00477                                                                   EL322
00478      MOVE WS-HEADING4            TO  PRT.                         EL322
00479      MOVE ' '                    TO  X.                           EL322
00480      PERFORM WRITE-PRINTER.                                       EL322
00481                                                                   EL322
00482      MOVE    WS-HEADING5     TO  PRT.                             EL322
00483      PERFORM WRITE-PRINTER.                                       EL322
00484      MOVE    +8              TO  WS-LINE-COUNT.                   EL322
00485                                                                   EL322
00486  WHS-020. COPY ELCWHS2.                                           EL322
00487                                                                   EL322
00488      EJECT                                                        EL322
00489  WRITE-PRINTER SECTION. COPY ELCWPS.                              EL322
00490                                                                   EL322
00491  WPS-020.                                                         EL322
00492                                                                   EL322
00493      IF DTE-FICH NOT = SPACE AND                                  EL322
00494          FICH-OPEN   = SPACE                                      EL322
00495          MOVE 'X' TO FICH-OPEN                                    EL322
00496          OPEN OUTPUT FICH.                                        EL322
00497                                                                   EL322
00498      IF DTE-PRT-OPT = 'S' OR 'T'                                  EL322
00499          IF (REPT-OPEN = SPACE) AND (DTE-ABEND-CD-1 = SPACE)      EL322
00500              OPEN I-O ELREPT                                      EL322
00501              IF DTE-F-1 NOT = ZERO AND                            EL322
00502                 DTE-VSAM-FLAGS NOT = '97'                         EL322
00503                  MOVE DTE-VSAM-FLAGS  TO  WS-ABEND-FILE-STATUS    EL322
00504                  MOVE 'ERROR OCCURED OPEN - ELREPT'               EL322
00505                                  TO  WS-ABEND-MESSAGE             EL322
00506                  GO TO ABEND-PGM                                  EL322
00507              ELSE                                                 EL322
00508                  MOVE '1'                   TO REPT-OPEN          EL322
00509                  MOVE DTE-CLASIC-COMPANY-CD TO RF-COMPANY-CD      EL322
00510                  MOVE '1'                   TO RF-RECORD-TYPE     EL322
00511                  MOVE OLC-REPORT-NAME       TO RF-REPORT-ID       EL322
00512                  MOVE ZERO                  TO RF-LINE-NUMBER     EL322
00513                  START ELREPT  KEY NOT LESS RF-CONTROL-PRIMARY    EL322
00514                  PERFORM DTE-REPORT-DELETE THRU DTE-DELETE-EXIT   EL322
00515                  MOVE DTE-CLASIC-COMPANY-CD TO RF-COMPANY-CD      EL322
00516                  MOVE '2'                   TO RF-RECORD-TYPE     EL322
00517                  MOVE OLC-REPORT-NAME       TO RF-REPORT-ID       EL322
00518                  MOVE ZERO                  TO RF-LINE-NUMBER     EL322
00519                  START ELREPT  KEY NOT LESS RF-CONTROL-PRIMARY    EL322
00520                  PERFORM DTE-REPORT-DELETE THRU DTE-DELETE-EXIT   EL322
00521                  MOVE DTE-CLASIC-COMPANY-CD TO RF-COMPANY-CD      EL322
00522                  MOVE '1'                   TO RF-RECORD-TYPE     EL322
00523                  MOVE OLC-REPORT-NAME       TO RF-REPORT-ID       EL322
00524                  MOVE SPACES                TO RF-REPORT-LINE-133.EL322
00525                                                                   EL322
00526      IF DTE-ABEND-CD-1 = '81' AND                                 EL322
00527         DTE-PRT-OPT    = 'S'                                      EL322
00528          MOVE +0302  TO WS-RETURN-CODE                            EL322
00529          GO TO ABEND-PGM.                                         EL322
00530                                                                   EL322
00531      IF DTE-PRT-OPT = 'S' OR 'T'                                  EL322
00532          MOVE X      TO RF-CTL-CHAR-133                           EL322
00533          MOVE P-DATA TO RF-DATA-133                               EL322
00534              IF DTE-ABEND-CD-1 = SPACES                           EL322
00535                  ADD +1 TO DTE-TOT-LINES                          EL322
00536                  MOVE DTE-TOT-LINES TO RF-LINE-NUMBER             EL322
00537                  WRITE REPORT-SAVE-FILE                           EL322
00538                      INVALID KEY                                  EL322
00539                          MOVE '88' TO DTE-ABEND-CD-1              EL322
00540                          CLOSE ELREPT                             EL322
00541                          MOVE SPACE TO REPT-OPEN.                 EL322
00542                                                                   EL322
00543      IF DTE-FICH NOT = SPACE                                      EL322
00544          MOVE X TO P-CTL                                          EL322
00545          WRITE FICH-REC FROM PRT.                                 EL322
00546                                                                   EL322
00547      IF DTE-PRT-OPT = 'P' OR 'B' OR 'T'                           EL322
00548          MOVE X TO P-CTL                                          EL322
00549          WRITE PRT.                                               EL322
00550                                                                   EL322
00551      GO TO DTE-PRINT-EXIT.                                        EL322
00552                                                                   EL322
00553  DTE-REPORT-DELETE.                                               EL322
00554      IF DTE-F-1 NOT = ZERO                                        EL322
00555          MOVE ZERO TO DTE-VSAM-FLAGS                              EL322
00556          GO TO DTE-DELETE-EXIT.                                   EL322
00557                                                                   EL322
00558      READ ELREPT   NEXT RECORD                                    EL322
00559            AT END   GO TO DTE-DELETE-EXIT.                        EL322
00560                                                                   EL322
00561      IF DTE-CLASIC-COMPANY-CD = RF-COMPANY-CD  AND                EL322
00562         OLC-REPORT-NAME       = RF-REPORT-ID                      EL322
00563          DELETE ELREPT RECORD                                     EL322
00564          GO TO DTE-REPORT-DELETE.                                 EL322
00565                                                                   EL322
00566  DTE-DELETE-EXIT.                                                 EL322
00567      EXIT.                                                        EL322
00568                                                                   EL322
00569  DTE-PRINT-EXIT.                                                  EL322
00570      EXIT.                                                        EL322
00571                                                                   EL322
00572      EJECT                                                        EL322
00573  OPEN-FILES SECTION.                                              EL322
00574                                                                   EL322
00575  OFS-010.                                                         EL322
00576      OPEN INPUT REPORTS-EXTRACT-FILE                              EL322
00577           OUTPUT PRNTR.                                           EL322
00578                                                                   EL322
00579      IF DTE-CLIENT = 'ELI' OR 'ELT'                               EL322
00580         OPEN OUTPUT ELI-TAPE-OUT.                                 EL322
00581                                                                   EL322
00582  OFS-EXIT.                                                        EL322
00583      EXIT.                                                        EL322
00584                                                                   EL322
00585                                                                   EL322
00586      EJECT                                                        EL322
00587  CLOSE-FILES SECTION.                                             EL322
00588                                                                   EL322
00589  CFS-010. COPY ELCPRTCX.                                          EL322
00590      CLOSE REPORTS-EXTRACT-FILE                                   EL322
00591            PRNTR.                                                 EL322
00592                                                                   EL322
00593      IF DTE-CLIENT = 'ELI' OR 'ELT'                               EL322
00594         CLOSE ELI-TAPE-OUT.                                       EL322
00595                                                                   EL322
00596  CFS-EXIT.                                                        EL322
00597      EXIT.                                                        EL322
00598                                                                   EL322
00599  ABEND-PGM SECTION. COPY ELCABEND.                                   CL**2
00600                                                                   EL322
