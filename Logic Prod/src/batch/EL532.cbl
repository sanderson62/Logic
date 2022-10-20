00001  IDENTIFICATION DIVISION.                                         04/18/98
00002                                                                   EL532
00003  PROGRAM-ID.                 EL532 .                                 LV006
00004 *              PROGRAM CONVERTED BY                               EL532
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL532
00006 *              CONVERSION DATE 04/10/96 10:17:14.                 EL532
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            EL532
00008 *                            VMOD=2.004                           EL532
00009                                                                   EL532
00010 *AUTHOR.     LOGIC,INC.                                           EL532
00011 *            DALLAS, TEXAS.                                       EL532
00012                                                                   EL532
00013 *DATE-COMPILED.                                                   EL532
00014                                                                   EL532
00015 *SECURITY.   *****************************************************EL532
00016 *            *                                                   *EL532
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *EL532
00018 *            *                                                   *EL532
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL532
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL532
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *EL532
00022 *            *                                                   *EL532
00023 *            *****************************************************EL532
00024                                                                   EL532
00025 *REMARKS.                                                         EL532
00026 *        THIS PROGRAM PRINTS A LIST OF CERTIFICATE NOTES          EL532
00027 *    ASSOCIATED WITH EACH CERTIFICATE.  THE REPORT IS PRINTED     EL532
00028 *    IN KEY SEQUENCE (CARRIER. GROUP, ST, ACCOUNT, CERT. EFF-DT,  EL532
00029 *    AND CERT. NO.                                                EL532
00030                                                                   EL532
00031      EJECT                                                        EL532
00032  ENVIRONMENT DIVISION.                                            EL532
00033                                                                   EL532
00034  INPUT-OUTPUT SECTION.                                            EL532
00035                                                                   EL532
00036  FILE-CONTROL.                                                    EL532
00037                                                                   EL532
00038      SELECT DISK-DATE        ASSIGN TO SYS019-FBA1-S-SYS019.      EL532
00039                                                                   EL532
00040      SELECT PRNTR            ASSIGN TO SYS008-UR-1403-S-SYS008.   EL532
00041                                                                   EL532
00042      SELECT CERT-NOTES       ASSIGN TO SYS022-FBA1-ERNOTE         EL532
00043                              ORGANIZATION IS INDEXED              EL532
00044                              ACCESS IS DYNAMIC                    EL532
00045                              RECORD KEY IS CN-CONTROL-PRIMARY     EL532
00046                              FILE STATUS IS ERNOTE-STATUS.        EL532
00047                                                                   EL532
00048      SELECT FICH             ASSIGN TO SYS020-UT-2400-S-SYS020.   EL532
00049                                                                   EL532
00050      SELECT ELREPT           ASSIGN TO SYS018-FBA1-ELREPT         EL532
00051                              ORGANIZATION IS INDEXED              EL532
00052                              ACCESS IS DYNAMIC                    EL532
00053                              RECORD KEY IS RF-CONTROL-PRIMARY     EL532
00054                              FILE STATUS IS DTE-VSAM-FLAGS.       EL532
00055                                                                   EL532
00056      EJECT                                                        EL532
00057  DATA DIVISION.                                                   EL532
00058                                                                   EL532
00059  FILE SECTION.                                                    EL532
00060                                                                   EL532
00061  FD  DISK-DATE               COPY ELCDTEFD.                       EL532
00062                                                                   EL532
00063  FD  PRNTR                   COPY ELCPRTFD.                       EL532
00064                                                                   EL532
00065  FD  FICH                    COPY ELCFCHFD.                       EL532
00066                                                                   EL532
00067  FD  ELREPT                  COPY ELCRPTFD.                       EL532
00068                                                                   EL532
00069      COPY ELCREPT.                                                EL532
00070                                                                   EL532
00071  FD  CERT-NOTES.                                                  EL532
00072                                                                   EL532
00073      COPY ERCNOTE.                                                EL532
00074                                                                   EL532
00075      EJECT                                                        EL532
00076  WORKING-STORAGE SECTION.                                         EL532
00077  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      EL532
00078                                                                   EL532
00079  77  FILLER  PIC X(32)   VALUE '********************************'.EL532
00080  77  FILLER  PIC X(32)   VALUE '*     EL532  WORKING STORAGE   *'.EL532
00081  77  FILLER  PIC X(32)   VALUE '******** VMOD=2.004 ************'.EL532
00082                                                                   EL532
00083  01  ERNOTE-STATUS                   PIC XX          VALUE SPACES.EL532
00084                                                                   EL532
00085  01  FILLER                          COMP-3.                      EL532
00086      05  WS-LINE-COUNT               PIC S9(3)       VALUE +99.   EL532
00087      05  WS-LINE-COUNT-MAX           PIC S9(3)       VALUE +60.   EL532
00088      05  WS-PAGE                     PIC S9(5)       VALUE ZERO.  EL532
00089      05  WS-REPORT-SW                PIC S9          VALUE +1.    EL532
00090      05  WS-PRINT-SW                 PIC S9          VALUE ZERO.  EL532
00091      05  WS-RECORD-COUNT             PIC S9(9)       VALUE ZERO.  EL532
00092      05  WS-ZERO                     PIC S9          VALUE ZERO.  EL532
00093                                                                   EL532
00094      05  WS-CARRIER-TOTAL            PIC S9(9)V99 VALUE ZERO.     EL532
00095      05  WS-CHECK-TOTAL              PIC S9(9)V99 VALUE ZERO.     EL532
00096                                                                   EL532
00097      EJECT                                                        EL532
00098  01  FILLER                          COMP SYNC.                   EL532
00099      05  PGM-SUB                     PIC S9(4)       VALUE +322.  EL532
00100      05  WS-SUB1                     PIC S9(4)       VALUE ZERO.  EL532
00101                                                                   EL532
00102  01  FILLER.                                                      EL532
00103      05  WS-RETURN-CODE              PIC S9(4)        COMP.       EL532
00104      05  OLC-REPORT-NAME             PIC X(5) VALUE 'EL532'.      EL532
00105      05  X                           PIC X           VALUE SPACE. EL532
00106                                                                   EL532
00107      05  WS-LAST-MONTH               PIC 99          VALUE ZERO.  EL532
00108      05  WS-LAST-MONTH-X             REDEFINES                    EL532
00109          WS-LAST-MONTH               PIC XX.                      EL532
00110                                                                   EL532
00111      05  WS-MONTH                    PIC XX          VALUE ZERO.  EL532
00112                                                                   EL532
00113      05  WS-LAST-CARRIER             PIC X           VALUE SPACES.EL532
00114                                                                   EL532
00115      05  WS-SAVE-PRINT-RECORD        PIC X(133)      VALUE SPACES.EL532
00116                                                                   EL532
00117      05  WS-ABEND-MESSAGE            PIC X(80)       VALUE SPACES.EL532
00118                                                                   EL532
00119      05  WS-ABEND-FILE-STATUS        PIC XX          VALUE ZERO.  EL532
00120                                                                   EL532
00121      05  WS-FILE-ERROR-MESSAGE.                                   EL532
00122          10  FILLER                  PIC X(24)       VALUE        EL532
00123              'ERROR OCCURED OPENING - '.                          EL532
00124          10  WS-FEM-FILE-NAME        PIC X(8).                    EL532
00125                                                                   EL532
00126      EJECT                                                        EL532
00127  01  WS-HEADING1.                                                 EL532
00128      05  FILLER                      PIC X(51)       VALUE '1'.   EL532
00129      05  WS-H1-TITLE                 PIC X(69)       VALUE        EL532
00130          'CERTIFICATE   NOTES'.                                   EL532
00131      05  WS-H1-REPORT-NUMBER         PIC X(5) VALUE 'EL532'.      EL532
00132                                                                   EL532
00133  01  WS-HEADING2.                                                 EL532
00134      05  FILLER                      PIC X(46)       VALUE SPACES.EL532
00135      05  WS-H2-CLIENT-NAME           PIC X(78)       VALUE SPACES.EL532
00136      05  WS-H2-DATE                  PIC X(8)        VALUE SPACES.EL532
00137      05  FILLER                      PIC X           VALUE SPACES.EL532
00138                                                                   EL532
00139  01  WS-HEADING3.                                                 EL532
00140      05  FILLER                      PIC X(51)       VALUE SPACES.EL532
00141      05  WS-H3-DATE                  PIC X(60)       VALUE SPACES.EL532
00142      05  FILLER                      PIC X(5)        VALUE 'PAGE'.EL532
00143      05  WS-H3-PAGE                  PIC ZZ,ZZ9.                  EL532
00144      05  FILLER                      PIC X(11)       VALUE SPACES.EL532
00145                                                                   EL532
00146  01  WS-HEADING4.                                                 EL532
00147      05  FILLER                      PIC X           VALUE '0'.   EL532
00148      05  FILLER                      PIC X(43)       VALUE SPACES.EL532
00149      05  FILLER                      PIC X(25)       VALUE        EL532
00150          'CERT.'.                                                 EL532
00151      05  FILLER                      PIC X(20)       VALUE        EL532
00152          ' * BILLING LINES *'.                                    EL532
00153                                                                   EL532
00154  01  WS-HEADING5.                                                 EL532
00155      05  FILLER                      PIC X           VALUE ' '.   EL532
00156      05  FILLER                      PIC X(54)       VALUE        EL532
00157          '   CAR     GROUP     ST.    ACCOUNT       CERT / SUFX '.EL532
00158      05  FILLER                      PIC X(31)       VALUE        EL532
00159          '   EFF. DT.      START   ENDING'.                       EL532
00160                                                                   EL532
00161      EJECT                                                        EL532
00162  01  WS-DETAIL1.                                                  EL532
00163      05  FILLER                      PIC X.                       EL532
00164      05  FILLER                      PIC X(4).                    EL532
00165      05  WS-D1-CARRIER               PIC X.                       EL532
00166      05  FILLER                      PIC X(5).                    EL532
00167      05  WS-D1-GROUPING              PIC X(6).                    EL532
00168      05  FILLER                      PIC X(4).                    EL532
00169      05  WS-D1-ST                    PIC XX.                      EL532
00170      05  FILLER                      PIC X(4).                    EL532
00171      05  WS-D1-ACCOUNT               PIC X(10).                   EL532
00172      05  FILLER                      PIC X(4).                    EL532
00173      05  WS-D1-CERT-PRIME            PIC X(10).                   EL532
00174      05  FILLER                      PIC X.                       EL532
00175      05  WS-D1-CERT-SFX              PIC X.                       EL532
00176      05  FILLER                      PIC X(4).                    EL532
00177      05  WS-D1-CERT-EFF-DT           PIC X(8).                    EL532
00178      05  FILLER                      PIC X(8).                    EL532
00179      05  WS-D1-BL-START              PIC Z.                       EL532
00180      05  FILLER                      PIC X(8).                    EL532
00181      05  WS-D1-BL-ENDING             PIC Z.                       EL532
00182      05  FILLER                      PIC X(50).                   EL532
00183      EJECT                                                        EL532
00184  01  WS-DETAIL2                      REDEFINES                    EL532
00185      WS-DETAIL1.                                                  EL532
00186      05  FILLER                      PIC X.                       EL532
00187      05  FILLER                      PIC X(22).                   EL532
00188      05  WS-D2-LINE-NO               PIC 9.                       EL532
00189      05  FILLER                      PIC X(3).                    EL532
00190      05  WS-D2-CERT-NOTE             PIC X(77).                   EL532
00191                                                                   EL532
00192      EJECT                                                        EL532
00193      COPY ELCDTECX.                                               EL532
00194                                                                   EL532
00195      COPY ELCDTEVR.                                                  CL**2
00196                                                                      CL**2
00197      EJECT                                                        EL532
00198                                                                   EL532
00199      COPY ELCDATE.                                                   CL**6
00200                                                                   EL532
00201      EJECT                                                        EL532
00202  PROCEDURE DIVISION.                                              EL532
00203                                                                   EL532
00204  1000-MAIN-LOGIC SECTION.                                         EL532
00205                                                                   EL532
00206      PERFORM 8600-DATE-CARD-READ.                                 EL532
00207                                                                   EL532
00208      PERFORM OPEN-FILES.                                          EL532
00209                                                                   EL532
00210      PERFORM 3000-PRINT-REPORT                                    EL532
00211                                                                   EL532
00212      PERFORM CLOSE-FILES                                          EL532
00213                                                                   EL532
00214      GOBACK.                                                      EL532
00215                                                                   EL532
00216      EJECT                                                        EL532
00217  3000-PRINT-REPORT SECTION.                                       EL532
00218                                                                   EL532
00219  3100-READ-CERT-NOTES.                                            EL532
00220                                                                   EL532
00221      READ CERT-NOTES NEXT RECORD.                                 EL532
00222                                                                   EL532
00223      IF  ERNOTE-STATUS = '10'                                     EL532
00224          GO TO 3900-PRINT-REPORT-EXIT.                            EL532
00225                                                                   EL532
00226      IF  ERNOTE-STATUS = '00' OR '97'                             EL532
00227          NEXT SENTENCE                                            EL532
00228      ELSE                                                         EL532
00229          MOVE +0301                  TO  WS-RETURN-CODE           EL532
00230          MOVE 'ER-NOTE READ ERROR'   TO  WS-ABEND-MESSAGE         EL532
00231          GO TO ABEND-PGM.                                         EL532
00232                                                                   EL532
00233      IF  CN-COMPANY-CD GREATER THAN DTE-CLASIC-COMPANY-CD         EL532
00234          GO TO 3900-PRINT-REPORT-EXIT.                            EL532
00235                                                                   EL532
00236      IF  CN-COMPANY-CD LESS THAN DTE-CLASIC-COMPANY-CD            EL532
00237          GO TO 3100-READ-CERT-NOTES.                              EL532
00238                                                                   EL532
00239      ADD +1                      TO  WS-RECORD-COUNT.             EL532
00240                                                                   EL532
00241      IF  WS-LINE-COUNT IS GREATER THAN +52                        EL532
00242          PERFORM WRITE-HEADINGS.                                  EL532
00243                                                                   EL532
00244      MOVE SPACES                 TO  PRT.                         EL532
00245      MOVE '0'                    TO  P-CTL.                          CL**5
00246      PERFORM WRITE-A-LINE.                                        EL532
00247      MOVE SPACES                 TO  WS-DETAIL1.                  EL532
00248      MOVE CN-CARRIER             TO  WS-D1-CARRIER.               EL532
00249      MOVE CN-GROUPING            TO  WS-D1-GROUPING.              EL532
00250      MOVE CN-ACCOUNT             TO  WS-D1-ACCOUNT.               EL532
00251      MOVE CN-STATE               TO  WS-D1-ST.                    EL532
00252      MOVE CN-CERT-PRIME          TO  WS-D1-CERT-PRIME.            EL532
00253      MOVE CN-CERT-SFX            TO  WS-D1-CERT-SFX.              EL532
00254      MOVE CN-BILLING-START-LINE-NO                                EL532
00255                                  TO  WS-D1-BL-START.              EL532
00256      MOVE CN-BILLING-END-LINE-NO TO  WS-D1-BL-ENDING.             EL532
00257                                                                   EL532
00258      MOVE CN-CERT-EFF-DT         TO  DC-BIN-DATE-1.               EL532
00259      MOVE SPACE                  TO  DC-OPTION-CODE.              EL532
00260      PERFORM 8500-DATE-CONVERSION.                                EL532
00261      MOVE  DC-GREG-DATE-1-EDIT   TO  WS-D1-CERT-EFF-DT.           EL532
00262      MOVE  WS-DETAIL1            TO  PRT.                         EL532
00263      MOVE '0'                    TO  P-CTL.                          CL**5
00264      PERFORM WRITE-A-LINE.                                        EL532
00265      MOVE SPACES                 TO PRT.                          EL532
00266      PERFORM WRITE-A-LINE.                                        EL532
00267                                                                   EL532
00268      MOVE SPACES                 TO  WS-DETAIL1.                  EL532
00269      MOVE  +0                    TO  WS-SUB1.                     EL532
00270                                                                   EL532
00271  3200-PRINT-CERT-NOTES.                                           EL532
00272      ADD +1                      TO  WS-SUB1.                     EL532
00273      IF  WS-SUB1 GREATER THAN +10                                 EL532
00274          GO TO 3100-READ-CERT-NOTES.                              EL532
00275                                                                   EL532
00276      IF  CN-LINE (WS-SUB1) = SPACES                               EL532
00277          GO TO 3200-PRINT-CERT-NOTES.                             EL532
00278                                                                   EL532
00279      MOVE WS-SUB1                TO  WS-D2-LINE-NO.               EL532
00280      MOVE CN-LINE (WS-SUB1)      TO  WS-D2-CERT-NOTE.             EL532
00281      MOVE WS-DETAIL2             TO  PRT.                         EL532
00282      MOVE ' '                    TO  P-CTL.                          CL**5
00283      PERFORM WRITE-A-LINE.                                        EL532
00284                                                                   EL532
00285      GO TO 3200-PRINT-CERT-NOTES.                                 EL532
00286                                                                   EL532
00287  3900-PRINT-REPORT-EXIT.                                          EL532
00288      EXIT.                                                        EL532
00289                                                                   EL532
00290      EJECT                                                        EL532
00291  8500-DATE-CONVERSION SECTION. COPY ELCDCS.                       EL532
00292                                                                   EL532
00293      EJECT                                                        EL532
00294                                                                   EL532
00295  8600-DATE-CARD-READ SECTION. COPY ELCDTERX SUPPRESS.             EL532
00296                                                                   EL532
00297                                                                   EL532
00298  WRITE-A-LINE SECTION. COPY ELCWAL.                               EL532
00299                                                                   EL532
00300      EJECT                                                        EL532
00301  WRITE-HEADINGS SECTION.                                          EL532
00302 ***************************************************************** EL532
00303 *                            ELCWHS1.                           * EL532
00304 *                            VMOD=2.001                         * EL532
00305 *    THIS SECTION CONTROLS THE WRITING OF THE HEADINGS          * EL532
00306 *****************************************************************.EL532
00307  WHS-010.                                                         EL532
00308      IF  WS-H2-DATE EQUAL SPACES                                  EL532
00309          MOVE WS-CURRENT-DATE    TO  WS-H2-DATE                   EL532
00310          MOVE COMPANY-NAME       TO  WS-H2-CLIENT-NAME            EL532
00311          MOVE ALPH-DATE          TO  WS-H3-DATE.                  EL532
00312                                                                   EL532
00313      ADD +1  TO  WS-PAGE.                                         EL532
00314      MOVE WS-PAGE                TO  WS-H3-PAGE.                  EL532
00315      MOVE PRT                    TO  WS-SAVE-PRINT-RECORD.        EL532
00316      MOVE ZERO                   TO  WS-LINE-COUNT.               EL532
00317                                                                   EL532
00318      MOVE WS-HEADING1            TO  PRT.                         EL532
00319      MOVE '1'                    TO  X.                           EL532
00320      PERFORM WRITE-PRINTER.                                       EL532
00321                                                                   EL532
00322      MOVE WS-HEADING2            TO  PRT.                         EL532
00323      MOVE ' '                    TO  X.                           EL532
00324      PERFORM WRITE-PRINTER.                                       EL532
00325                                                                   EL532
00326      MOVE WS-HEADING3            TO  PRT.                         EL532
00327      MOVE ' '                    TO  X.                           EL532
00328      PERFORM WRITE-PRINTER.                                       EL532
00329                                                                   EL532
00330      MOVE WS-HEADING4            TO  PRT.                         EL532
00331      MOVE ' '                    TO  X.                           EL532
00332      PERFORM WRITE-PRINTER.                                       EL532
00333                                                                   EL532
00334                                                                   EL532
00335      MOVE WS-HEADING5            TO  PRT                          EL532
00336                                                                   EL532
00337      PERFORM WRITE-PRINTER.                                       EL532
00338                                                                   EL532
00339      MOVE +8                     TO  WS-LINE-COUNT.               EL532
00340                                                                   EL532
00341  WHS-020. COPY ELCWHS2.                                           EL532
00342                                                                   EL532
00343      EJECT                                                        EL532
00344  WRITE-PRINTER SECTION. COPY ELCWPS.                              EL532
00345                                                                   EL532
00346  WPS-020. COPY ELCPRT2X.                                          EL532
00347                                                                   EL532
00348      EJECT                                                        EL532
00349  OPEN-FILES SECTION.                                              EL532
00350                                                                   EL532
00351  OFS-010.                                                         EL532
00352      OPEN INPUT CERT-NOTES                                        EL532
00353           OUTPUT PRNTR.                                           EL532
00354                                                                   EL532
00355      IF  ERNOTE-STATUS = '00' OR '97'                             EL532
00356          NEXT SENTENCE                                            EL532
00357      ELSE                                                         EL532
00358          MOVE +0302                     TO  WS-RETURN-CODE        EL532
00359          MOVE 'ER-NOTE OPEN ERROR'      TO  WS-ABEND-MESSAGE      EL532
00360          GO TO ABEND-PGM.                                         EL532
00361                                                                   EL532
00362      MOVE BIN-RUN-DATE           TO  WS-LAST-MONTH-X.                CL**4
00363                                                                   EL532
00364  OFS-EXIT.                                                        EL532
00365      EXIT.                                                        EL532
00366                                                                   EL532
00367      EJECT                                                        EL532
00368  CLOSE-FILES SECTION.                                             EL532
00369                                                                   EL532
00370  CFS-010. COPY ELCPRTCX.                                          EL532
00371                                                                   EL532
00372      CLOSE CERT-NOTES                                             EL532
00373            PRNTR.                                                 EL532
00374                                                                   EL532
00375  CFS-EXIT.                                                        EL532
00376      EXIT.                                                        EL532
00377                                                                   EL532
00378      EJECT                                                        EL532
00379  ABEND-PGM SECTION. COPY ELCABEND SUPPRESS.                       EL532
00380                                                                   EL532
