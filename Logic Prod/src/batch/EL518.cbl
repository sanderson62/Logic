00001  IDENTIFICATION DIVISION.                                         04/18/98
00002                                                                   EL518
00003  PROGRAM-ID.                 EL518 .                                 LV005
00004 *              PROGRAM CONVERTED BY                               EL518
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL518
00006 *              CONVERSION DATE 02/23/96 16:08:22.                 EL518
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            EL518
00008 *                            VMOD=2.009.                          EL518
00009                                                                   EL518
00010 *AUTHOR.     LOGIC, INC.                                          EL518
00011 *            DALLAS, TEXAS.                                       EL518
00012                                                                   EL518
00013 *DATE-COMPILED.                                                   EL518
00014                                                                   EL518
00015 *SECURITY.   *****************************************************EL518
00016 *            *                                                   *EL518
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *EL518
00018 *            *                                                   *EL518
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL518
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL518
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *EL518
00022 *            *                                                   *EL518
00023 *            *****************************************************EL518
00024                                                                   EL518
00025 *REMARKS.                                                         EL518
00026 *                 THIS IS THE BATCH JOB TO EDIT THE CLAIMS        EL518
00027 *                 RECORDS FOR THE ONLINE CREDIT SYSTEM.           EL518
00028                                                                   EL518
00029  ENVIRONMENT DIVISION.                                            EL518
00030                                                                   EL518
00031  INPUT-OUTPUT SECTION.                                            EL518
00032                                                                   EL518
00033  FILE-CONTROL.                                                    EL518
00034                                                                   EL518
00035      SELECT DISK-DATE        ASSIGN TO SYS019-UT-3380-S-SYS019.   EL518
00036                                                                   EL518
00037      SELECT ELCNTLF          ASSIGN TO SYS021-3380-ELCNTL         EL518
00038                              ORGANIZATION IS INDEXED              EL518
00039                              ACCESS IS DYNAMIC                    EL518
00040                              RECORD KEY IS CF-CONTROL-PRIMARY     EL518
00041                              FILE STATUS IS ELCNTLF-FILE-STATUS.  EL518
00042                                                                   EL518
00043      SELECT ERPNDCF          ASSIGN TO SYS022-3380-ERPNDC         EL518
00044                              ORGANIZATION IS INDEXED              EL518
00045                              ACCESS IS SEQUENTIAL                 EL518
00046                              RECORD KEY IS PC-CONTROL-PRIMARY     EL518
00047                              FILE STATUS IS ERPNDCF-FILE-STATUS.  EL518
00048                                                                   EL518
00049      SELECT ELCERTF          COPY ELCCERTS.                       EL518
00050                                                                   EL518
00051      SELECT ERACCTF          ASSIGN TO SYS022-3380-ERACCT2        EL518
00052                              ORGANIZATION IS INDEXED              EL518
00053                              ACCESS IS DYNAMIC                    EL518
00054                              RECORD KEY IS AM-CONTROL-BY-VAR-GRP  EL518
00055                              FILE STATUS IS ERACCTF-FILE-STATUS.  EL518
00056                                                                   EL518
00057      SELECT ELCERRF          ASSIGN TO SYS022-3380-ELERRS         EL518
00058                              ORGANIZATION IS INDEXED              EL518
00059                              ACCESS IS DYNAMIC                    EL518
00060                              RECORD KEY IS EM-CONTROL-PRIMARY     EL518
00061                              FILE STATUS IS ELCERRF-FILE-STATUS.  EL518
00062                                                                   EL518
00063       EJECT                                                       EL518
00064  DATA DIVISION.                                                   EL518
00065                                                                   EL518
00066  FILE SECTION.                                                    EL518
00067                                                                   EL518
00068  FD  DISK-DATE               COPY ELCDTEFD.                       EL518
00069                                                                   EL518
00070      EJECT                                                        EL518
00071  FD  ELCNTLF.                                                     EL518
00072                                                                   EL518
00073                              COPY ELCCNTL.                        EL518
00074                                                                   EL518
00075      EJECT                                                        EL518
00076  FD  ERPNDCF.                                                     EL518
00077                                                                   EL518
00078                              COPY ERCPNDC.                        EL518
00079                                                                   EL518
00080      EJECT                                                        EL518
00081  FD  ELCERTF.                                                     EL518
00082                                                                   EL518
00083                              COPY ELCCERT.                        EL518
00084      EJECT                                                        EL518
00085                                                                   EL518
00086  FD  ERACCTF.                                                     EL518
00087                                                                   EL518
00088                                      COPY ERCACCT.                EL518
00089                                                                   EL518
00090  FD  ELCERRF.                                                     EL518
00091                                                                   EL518
00092                                      COPY ELCERRS.                EL518
00093                                                                   EL518
00094                                                                   EL518
00095  WORKING-STORAGE SECTION.                                         EL518
00096  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      EL518
00097  77  FILLER   PIC X(32) VALUE '********************************'. EL518
00098  77  FILLER   PIC X(32) VALUE '**  EL518   WORKING STORAGE   **'. EL518
00099  77  FILLER   PIC X(32) VALUE '*********** VMOD 2.009**********'. EL518
00100                                                                   EL518
00101  77  WS-ZERO                     PIC S9    VALUE ZERO  COMP-3.    EL518
00102  77  WS-RETURN-CODE              PIC S9(4) COMP VALUE ZERO.       EL518
00103  77  WS-ABEND-MESSAGE            PIC X(80) VALUE SPACES.          EL518
00104  77  WS-ABEND-FILE-STATUS        PIC XX    VALUE ZERO.            EL518
00105  77  PGM-SUB                     PIC S9(4) COMP   VALUE +518.     EL518
00106  77  SUB-X                       PIC S9(4) COMP   VALUE   +0.     EL518
00107  77  SUB                         PIC S9(4) COMP   VALUE   +0.     EL518
00108  77  JP-RECORD-TYPE              PIC X.                           EL518
00109                                                                   EL518
00110  01  EMSG.                                                        EL518
00111      12  WS-FILE-ERROR-MESSAGE.                                   EL518
00112          16  FILLER              PIC X(24)   VALUE                EL518
00113              'ERROR OCCURED OPENING - '.                          EL518
00114          16  WS-FEM-FILE-NAME    PIC X(8).                        EL518
00115                                                                   EL518
00116  01  WX-AREA.                                                     EL518
00117      12  PH-COBOL                PIC X.                           EL518
00118      12  HOLD-ELCNTL             PIC X(505) OCCURS 11 TIMES.      EL518
00119      12  EIBDATE                 PIC S9(7)     COMP-3.            EL518
00120                                                                   EL518
00121  01  STATUS-SWITCH.                                               EL518
00122      12  ELCNTLF-FILE-STATUS     PIC XX.                          EL518
00123      12  ERPNDCF-FILE-STATUS     PIC XX.                          EL518
00124      12  ELCERT-FILE-STATUS      PIC XX.                          EL518
00125      12  ERACCTF-FILE-STATUS     PIC XX.                          EL518
00126      12  ELCERRF-FILE-STATUS     PIC XX.                          EL518
00127                                                                   EL518
00128      12  EMI-SEVERITY            PIC X OCCURS 100 TIMES.          EL518
00129                                                                   EL518
00130      12  EMI-SEVERITY-SAVE       PIC X.                           EL518
00131      12  EMI-WARNING-CTR         PIC S999  COMP-3  VALUE +0.      EL518
00132      12  EMI-FORCABLE-CTR        PIC S999  COMP-3  VALUE +0.      EL518
00133      12  EMI-FATAL-CTR           PIC S999  COMP-3  VALUE +0.      EL518
00134                                                                   EL518
00135      12  ELCNTL-POINTER          PIC S9(8)   COMP.                EL518
00136      12  ERACCT-POINTER          PIC S9(8)   COMP.                EL518
00137      12  ELCERT-POINTER          PIC S9(8)   COMP.                EL518
00138                                                                   EL518
00139                                  COPY ELCDTECX.                   EL518
00140                                                                      CL**2
00141                                  COPY ELCDTEVR.                      CL**2
00142      EJECT                                                        EL518
00143                                  COPY ELC53WS.                    EL518
00144                                                                   EL518
00145  01  HOLD-WORK                   PIC X(2000).                     EL518
00146                                                                   EL518
00147      EJECT                                                        EL518
00148                                  COPY ELCDATE.                       CL**5
00149      EJECT                                                        EL518
00150                                  COPY ELCCALC.                    EL518
00151      EJECT                                                        EL518
00152                                                                   EL518
00153  01  EL518-WS.                                                    EL518
00154      12  WK-PC-WORK-AREA.                                         EL518
00155          16  WK-PC-CNTL-RECORD-FOUND-SW  PIC X.                   EL518
00156          16  WK-PC-LAST-CARRIER          PIC X.                   EL518
00157          16  WK-PC-CERT-ACCESS-CNTL      PIC X.                   EL518
00158          16  WK-PC-CO-CLAIM-REJECT-SW    PIC X.                   EL518
00159          16  WK-PC-CLAIM-SYSTEM-SW       PIC X.                   EL518
00160          16  WK-PC-CO-TOL-CLAIM          PIC S9(3)V99  COMP-3.    EL518
00161          16  WK-PC-RESERVE-CONTROLS      PIC X(4).                EL518
00162          16  WK-PC-CREDIT-EDIT-CONTROLS  PIC X(12).               EL518
00163      12  WK-PC-RECORD-ADDRESSES.                                  EL518
00164          16  WK-PC-ACCT-ADDR             PIC S9(8)     COMP.      EL518
00165          16  WK-PC-STATE-ADDR            PIC S9(8)     COMP.      EL518
00166      12  WK-MISC.                                                 EL518
00167          16  WK-PC-REM-TRM-CALC-OPTION   PIC X.                   EL518
00168          16  FILLER                      PIC X(20).               EL518
00169                                                                   EL518
00170  PROCEDURE DIVISION.                                              EL518
00171                                                                   EL518
00172      MOVE ELC53WS-WORK           TO HOLD-WORK.                    EL518
00173      MOVE LOW-VALUES             TO WK-PC-RECORD-ADDRESSES.       EL518
00174      MOVE ZEROS                  TO ELCNTL-POINTER                EL518
00175                                     ERACCT-POINTER                EL518
00176                                     ELCERT-POINTER.               EL518
00177                                                                   EL518
00178  0000-LOAD-DATE-CARD.            COPY ELCDTERX SUPPRESS.          EL518
00179                                                                   EL518
00180      IF DTE-SYS-E-CLASIC-CLAIMS EQUAL 'Y'                         EL518
00181          MOVE '*** USER HAS ONLINE CLAIMS SYSTEM ***'             EL518
00182                                  TO WS-ABEND-MESSAGE              EL518
00183          GO TO ABEND-PGM.                                         EL518
00184                                                                   EL518
00185      MOVE WS-CD-YY               TO DC-GREG-DATE-1-MDY.           EL518
00186      COMPUTE DC-GREG-DATE-1-MDY = DC-GREG-DATE-1-MDY +            EL518
00187          (WS-CD-DD * 100).                                        EL518
00188      COMPUTE DC-GREG-DATE-1-MDY = DC-GREG-DATE-1-MDY +            EL518
00189          (WS-CD-MM * 10000).                                      EL518
00190      MOVE 4                      TO DC-OPTION-CODE.               EL518
00191      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    EL518
00192      MOVE DC-JULIAN-DATE         TO EIBDATE.                         CL**4
00193      MOVE LOW-VALUES             TO WK-PC-RECORD-ADDRESSES.       EL518
00194                                                                   EL518
00195      OPEN INPUT ELCNTLF.                                          EL518
00196      IF ELCNTLF-FILE-STATUS  = '00' OR '97'                       EL518
00197          NEXT SENTENCE                                            EL518
00198        ELSE                                                       EL518
00199          MOVE ELCNTLF-FILE-STATUS TO WS-ABEND-FILE-STATUS         EL518
00200          MOVE 'ELCNTLF'             TO WS-FEM-FILE-NAME           EL518
00201          MOVE EMSG TO WS-ABEND-MESSAGE                            EL518
00202          GO TO ABEND-PGM.                                         EL518
00203                                                                   EL518
00204      OPEN  I-O ERPNDCF.                                           EL518
00205      IF ERPNDCF-FILE-STATUS  = '00' OR '97'                       EL518
00206          NEXT SENTENCE                                            EL518
00207        ELSE                                                       EL518
00208          MOVE ERPNDCF-FILE-STATUS TO WS-ABEND-FILE-STATUS         EL518
00209          MOVE 'ERPNDCF'             TO WS-FEM-FILE-NAME           EL518
00210          MOVE EMSG TO WS-ABEND-MESSAGE                            EL518
00211          GO TO ABEND-PGM.                                         EL518
00212                                                                   EL518
00213      MOVE LOW-VALUES             TO PC-CONTROL-PRIMARY.           EL518
00214      MOVE ZEROS                  TO PC-RECORD-SEQUENCE.           EL518
00215      MOVE DTE-CLASIC-COMPANY-CD  TO PC-COMPANY-CD.                EL518
00216      START ERPNDCF KEY NOT LESS THAN PC-CONTROL-PRIMARY.          EL518
00217                                                                   EL518
00218      IF ERPNDCF-FILE-STATUS NOT = '00'                            EL518
00219          GO TO 9999-DONE.                                         EL518
00220                                                                   EL518
00221      OPEN I-O ELCERTF.                                            EL518
00222      IF ELCERT-FILE-STATUS  = '00' OR '97'                        EL518
00223          NEXT SENTENCE                                            EL518
00224        ELSE                                                       EL518
00225          MOVE ELCERT-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL518
00226          MOVE 'ELCERTF'             TO WS-FEM-FILE-NAME           EL518
00227          MOVE EMSG TO WS-ABEND-MESSAGE                            EL518
00228          GO TO ABEND-PGM.                                         EL518
00229                                                                   EL518
00230                                                                   EL518
00231      OPEN INPUT ERACCTF.                                          EL518
00232      IF ERACCTF-FILE-STATUS  = '00' OR '97'                       EL518
00233          NEXT SENTENCE                                            EL518
00234        ELSE                                                       EL518
00235          MOVE ERACCTF-FILE-STATUS TO WS-ABEND-FILE-STATUS         EL518
00236          MOVE 'ERACCTF'             TO WS-FEM-FILE-NAME           EL518
00237          MOVE EMSG TO WS-ABEND-MESSAGE                            EL518
00238          GO TO ABEND-PGM.                                         EL518
00239                                                                   EL518
00240                                                                   EL518
00241      OPEN INPUT ELCERRF.                                          EL518
00242      IF ELCERRF-FILE-STATUS  = '00' OR '97'                       EL518
00243          NEXT SENTENCE                                            EL518
00244        ELSE                                                       EL518
00245          MOVE ELCERRF-FILE-STATUS TO WS-ABEND-FILE-STATUS         EL518
00246          MOVE 'ELCERRF'             TO WS-FEM-FILE-NAME           EL518
00247          MOVE EMSG TO WS-ABEND-MESSAGE                            EL518
00248          GO TO ABEND-PGM.                                         EL518
00249                                                                   EL518
00250      MOVE 0 TO SUB.                                               EL518
00251                                                                   EL518
00252  0000-LOAD-ERRS.                                                  EL518
00253      ADD 1 TO SUB.                                                EL518
00254      IF SUB GREATER THAN 100                                      EL518
00255          CLOSE ELCERRF                                            EL518
00256          GO TO 0000-EXIT.                                         EL518
00257      MOVE 'W'                    TO EMI-SEVERITY (SUB).           EL518
00258      COMPUTE EM-MESSAGE-NUMBER = 2800 + SUB.                      EL518
00259                                                                   EL518
00260      READ ELCERRF.                                                EL518
00261      IF ELCERRF-FILE-STATUS NOT = '00'                            EL518
00262          GO TO 0000-LOAD-ERRS.                                    EL518
00263                                                                   EL518
00264      MOVE EM-ERROR-SEVERITY      TO EMI-SEVERITY (SUB).           EL518
00265      GO TO 0000-LOAD-ERRS.                                        EL518
00266                                                                   EL518
00267  0000-EXIT.                                                       EL518
00268      GO TO 0100-TEST.                                             EL518
00269                                                                   EL518
00270  0100-READ-PENDING.                                               EL518
00271      MOVE HOLD-WORK TO ELC53WS-WORK.                              EL518
00272      MOVE ZEROS                  TO EMI-WARNING-CTR               EL518
00273                                     EMI-FORCABLE-CTR              EL518
00274                                     EMI-FATAL-CTR.                EL518
00275      READ ERPNDCF.                                                EL518
00276                                                                   EL518
00277      MOVE LIFE-OVERRIDE-L1       TO PC-LF-OVERRIDE-L1.            EL518
00278      MOVE AH-OVERRIDE-L1         TO PC-AH-OVERRIDE-L1.            EL518
00279                                                                   EL518
00280  0100-TEST.                                                       EL518
00281      IF DTE-CLASIC-COMPANY-CD NOT = PC-COMPANY-CD                 EL518
00282          GO TO 9999-DONE.                                         EL518
00283                                                                   EL518
00284      IF ERPNDCF-FILE-STATUS NOT = '00'                            EL518
00285          GO TO 9999-DONE.                                         EL518
00286      IF PC-CREDIT-ACCEPT-DT NOT = LOW-VALUES                      EL518
00287          GO TO 0100-READ-PENDING.                                 EL518
00288                                                                   EL518
00289  0000-ELC53PD.                   COPY ELC53PD.                    EL518
00290                                                                   EL518
00291  7200-FIND-BENEFIT.                                               EL518
00292      MOVE 'N'                    TO BEN-SEARCH-SW.                EL518
00293                                                                   EL518
00294      MOVE ELCNTL-KEY TO CF-CONTROL-PRIMARY.                       EL518
00295                                                                   EL518
00296      START ELCNTLF KEY NOT LESS THAN CF-CONTROL-PRIMARY.          EL518
00297                                                                   EL518
00298      IF ELCNTLF-FILE-STATUS NOT = '00'                            EL518
00299          GO TO 7200-EXIT.                                         EL518
00300                                                                   EL518
00301      READ ELCNTLF NEXT.                                           EL518
00302                                                                   EL518
00303      IF ELCNTLF-FILE-STATUS NOT = '00'                            EL518
00304          GO TO 7200-EXIT.                                         EL518
00305                                                                   EL518
00306      IF (CNTL-COMP-ID NOT = CF-COMPANY-ID) OR                     EL518
00307         (CNTL-REC-TYPE NOT = CF-RECORD-TYPE)                      EL518
00308            GO TO 7200-EXIT.                                       EL518
00309                                                                   EL518
00310      PERFORM 7200-BENEFIT-DUMMY THRU 7200-DUMMY-EXIT              EL518
00311          VARYING SUB3 FROM 1 BY 1 UNTIL                           EL518
00312             ((SUB3 GREATER 8) OR                                  EL518
00313             (CF-BENEFIT-CODE (SUB3) = WS-BEN-CD)).                EL518
00314                                                                   EL518
00315      IF SUB3 NOT = 9                                              EL518
00316          MOVE 'Y'                TO BEN-SEARCH-SW.                EL518
00317                                                                   EL518
00318      GO TO 7200-EXIT.                                             EL518
00319                                                                   EL518
00320  7200-BENEFIT-DUMMY.                                              EL518
00321      EXIT.                                                        EL518
00322  7200-DUMMY-EXIT.                                                 EL518
00323      EXIT.                                                        EL518
00324                                                                   EL518
00325  7200-EXIT.                                                       EL518
00326      EXIT.                                                        EL518
00327      EJECT                                                        EL518
00328                                                                   EL518
00329  7600-GET-REMAIN-TERM.                                            EL518
00330      CALL 'ELRTRMX' USING CALCULATION-PASS-AREA.                  EL518
00331                                                                   EL518
00332  7600-EXIT.                                                       EL518
00333      EXIT.                                                        EL518
00334      EJECT                                                        EL518
00335                                                                   EL518
00336  7700-GET-REMAIN-AMT.                                             EL518
00337      CALL 'ELRAMTX' USING CALCULATION-PASS-AREA.                  EL518
00338                                                                   EL518
00339  7700-EXIT.                                                       EL518
00340      EXIT.                                                        EL518
00341      EJECT                                                        EL518
00342                                                                   EL518
00343  7800-REWRITE-CERT.                                               EL518
00344      REWRITE CERTIFICATE-MASTER.                                  EL518
00345                                                                   EL518
00346  7800-EXIT.                                                       EL518
00347      EXIT.                                                        EL518
00348                                                                   EL518
00349  8100-JOURNAL-CERT.                                               EL518
00350 *     NOT JOURNALIZED.                                            EL518
CIDMOD     GO TO 8100-EXIT.                                             EL518
00352  8100-EXIT.                                                       EL518
00353      EXIT.                                                        EL518
00355 *    EJECT                                                        EL518
00356  8200-START-BR.                                                   EL518
00357      MOVE ERACCT-KEY TO AM-CONTROL-BY-VAR-GRP.                    EL518
00358                                                                   EL518
00359  8200-EXIT.                                                       EL518
00360      EXIT.                                                        EL518
00361                                                                   EL518
00362  8300-READ-ACCT.                                                  EL518
00363      MOVE ERACCT-KEY TO AM-CONTROL-BY-VAR-GRP.                    EL518
00364                                                                   EL518
00365      IF ACCOUNT-BROWSE-STARTED-SW = 'Y'                           EL518
00366          START ERACCTF KEY NOT LESS THAN AM-CONTROL-BY-VAR-GRP.   EL518
00367                                                                   EL518
00368      IF ACCOUNT-BROWSE-STARTED-SW = 'Y'                           EL518
00369      IF ERACCTF-FILE-STATUS NOT = '00'                            EL518
00370          DISPLAY 'START ERROR - ' ERACCTF-FILE-STATUS             EL518
00371          GO TO 0199-ACCT-NOT-FOUND.                               EL518
00372                                                                   EL518
00373      MOVE SPACE TO ACCOUNT-BROWSE-STARTED-SW.                     EL518
00374                                                                   EL518
00375      READ ERACCTF NEXT.                                           EL518
00376      IF ERACCTF-FILE-STATUS NOT = '00'                            EL518
00377          DISPLAY 'READ ERROR - ' ERACCTF-FILE-STATUS              EL518
00378          GO TO 0199-ACCT-NOT-FOUND.                               EL518
00379                                                                   EL518
00380      MOVE AM-CONTROL-BY-VAR-GRP TO ERACCT-KEY.                    EL518
00381                                                                   EL518
00382  8300-EXIT.                                                       EL518
00383      EXIT.                                                        EL518
00384                                                                   EL518
00385  8500-DATE-CONVERT.                                               EL518
00386      CALL 'ELDATCX' USING DATE-CONVERSION-DATA.                   EL518
00387                                                                   EL518
00388  8500-EXIT.                                                       EL518
00389      EXIT.                                                        EL518
00390      EJECT                                                        EL518
00391                                                                   EL518
00392  8600-SRL-ACCOUNT-MASTER.                                         EL518
00393 *    SERVICE RELOAD ACCOUNT-MASTER.                               EL518
CIDMOD     GO TO 8600-EXIT.
00394  8600-EXIT.                                                       EL518
00395      EXIT.                                                        EL518
00396  8700-SRL-CERT-MASTER.                                            EL518
00397      MOVE ELCERT-KEY TO CM-CONTROL-PRIMARY.                       EL518
00398      READ ELCERTF.                                                EL518
00399                                                                   EL518
00400      IF ELCERT-FILE-STATUS NOT = '00'                             EL518
00401          GO TO 0299-CERT-NOT-FOUND.                               EL518
00402                                                                   EL518
00403  8700-EXIT.                                                       EL518
00404      EXIT.                                                        EL518
00405  8800-SRL-CONT-FILE.                                              EL518
00406 *    SERVICE RELOAD CONTROL-FILE.                                 EL518
CIDMOD     GO TO 8800-EXIT.
00407  8800-EXIT.                                                       EL518
00408      EXIT.                                                        EL518
00409                                                                   EL518
00410  8900-GETMAIN.                                                    EL518
00411      MOVE 99 TO ERACCT-POINTER.                                   EL518
00412                                                                   EL518
00413  8900-EXIT.                                                       EL518
00414      EXIT.                                                        EL518
00415                                                                   EL518
00416  9000-READ-CONTROL.                                               EL518
00417      MOVE ELCNTL-KEY TO CF-CONTROL-PRIMARY.                       EL518
00418                                                                   EL518
00419      START ELCNTLF KEY NOT LESS THAN CF-CONTROL-PRIMARY.          EL518
00420                                                                   EL518
00421      IF ELCNTLF-FILE-STATUS NOT = '00'                            EL518
00422          DISPLAY 'START ERROR - ' ELCNTLF-FILE-STATUS             EL518
00423          GO TO 9000-NOT-FOUND.                                    EL518
00424                                                                   EL518
00425      READ ELCNTLF NEXT.                                           EL518
00426      IF ELCNTLF-FILE-STATUS NOT = '00'                            EL518
00427          DISPLAY 'READ ERROR - ' ELCNTLF-FILE-STATUS              EL518
00428          GO TO 9000-NOT-FOUND.                                    EL518
00429                                                                   EL518
00430      MOVE CF-CONTROL-PRIMARY TO ELCNTL-KEY.                       EL518
00431      GO TO 9000-EXIT.                                             EL518
00432                                                                   EL518
00433  9000-NOT-FOUND.                                                  EL518
00434      IF CTL-READ = 'C'                                            EL518
00435          GO TO 0099-COMPANY-NOT-FOUND.                            EL518
00436                                                                   EL518
00437      IF CTL-READ = 'M'                                            EL518
00438          GO TO 0301-CARRIER-NOT-FOUND.                            EL518
00439                                                                   EL518
00440      IF CTL-READ = 'S'                                            EL518
00441          GO TO 0310-STATE-NOT-FOUND.                              EL518
00442                                                                   EL518
00443      GO TO 9000-EXIT.                                             EL518
00444                                                                   EL518
00445  9000-NOT-OPEN.                                                   EL518
00446      IF CTL-READ = 'C'                                            EL518
00447          GO TO 0098-COMPANY-NOT-OPEN.                             EL518
00448      GO TO 9000-EXIT.                                             EL518
00449                                                                   EL518
00450  9000-EXIT.                                                       EL518
00451      EXIT.                                                        EL518
00452      EJECT                                                        EL518
00453  9100-READ-CONTROL.                                               EL518
00454      MOVE 'S' TO CTL-READ.                                        EL518
00455      PERFORM 9000-READ-CONTROL THRU 9000-EXIT.                    EL518
00456                                                                   EL518
00457  9100-EXIT.                                                       EL518
00458      EXIT.                                                        EL518
00459                                                                   EL518
00460  9200-CONTROL-GETMAIN.                                            EL518
00461 *    DUMMY PARAGRAPH.                                             EL518
CIDMOD     GO TO 9200-EXIT.                                             EL518
00463  9200-EXIT.                                                       EL518
00464      EXIT.                                                        EL518
00465      EJECT                                                        EL518
00466                                                                   EL518
00467  9250-UNLOCK.                                                     EL518
00468      GO TO 9990-RETURN.                                           EL518
00469                                                                   EL518
00470  9990-RETURN.                                                     EL518
00471      IF PC-ERROR-FLAGS = SPACES                                   EL518
00472          GO TO 9990-REWRITE-RECORD.                               EL518
00473                                                                   EL518
00474      MOVE 1  TO SUB.                                              EL518
00475                                                                   EL518
00476  9990-ERR-LOOP.                                                   EL518
00477      IF PC-ERR-FLAG (SUB) NOT = SPACES                            EL518
00478          MOVE EMI-SEVERITY (SUB) TO EMI-SEVERITY-SAVE             EL518
00479          PERFORM 9999-INCREMENT-ERROR-COUNTERS THRU 9999-EXIT.    EL518
00480                                                                   EL518
00481      ADD 1   TO SUB.                                              EL518
00482      IF SUB LESS THAN 101                                         EL518
00483          GO TO 9990-ERR-LOOP.                                     EL518
00484                                                                   EL518
00485  9990-SET-ERROR-FLAGS.                                            EL518
00486      IF EMI-FATAL-CTR NOT = ZEROS                                 EL518
00487          MOVE 'X'                TO PC-FATAL-FLAG.                EL518
00488                                                                   EL518
00489      IF EMI-FORCABLE-CTR NOT = ZEROS                              EL518
00490          IF PC-CLAIM-FORCE                                        EL518
00491              MOVE 'F'            TO PC-FORCE-ER-CD                EL518
00492          ELSE                                                     EL518
00493              MOVE 'X'            TO PC-FORCE-ER-CD.               EL518
00494                                                                   EL518
00495      IF EMI-WARNING-CTR NOT = ZEROS                               EL518
00496          MOVE 'W'                TO PC-WARN-ER-CD.                EL518
00497                                                                   EL518
00498  9990-REWRITE-RECORD.                                             EL518
00499      REWRITE PENDING-CLAIMS.                                      EL518
00500      GO TO 0100-READ-PENDING.                                     EL518
00501                                                                   EL518
00502  9999-INCREMENT-ERROR-COUNTERS.                                   EL518
00503      IF EMI-SEVERITY-SAVE = 'W'                                   EL518
00504          ADD 1                   TO EMI-WARNING-CTR               EL518
00505      ELSE                                                         EL518
00506          IF EMI-SEVERITY-SAVE = 'F'                               EL518
00507              ADD 1               TO EMI-FORCABLE-CTR              EL518
00508          ELSE                                                     EL518
00509              IF EMI-SEVERITY-SAVE = 'X'                           EL518
00510                  ADD 1           TO EMI-FATAL-CTR.                EL518
00511                                                                   EL518
00512  9999-EXIT.                                                       EL518
00513      EXIT.                                                        EL518
00514      EJECT                                                        EL518
00515                                                                   EL518
00516  9999-DONE.                                                       EL518
00517      CLOSE      ELCNTLF                                           EL518
00518                 ERPNDCF                                           EL518
00519                 ELCERTF                                           EL518
00520                 ERACCTF.                                          EL518
00521                                                                   EL518
00522      IF ERPNDCF-FILE-STATUS NOT = '00'                            EL518
00523      IF ERPNDCF-FILE-STATUS NOT = '10'                            EL518
00524          MOVE ERPNDCF-FILE-STATUS TO WS-ABEND-FILE-STATUS         EL518
00525          MOVE 'ERPNDCF'             TO WS-FEM-FILE-NAME           EL518
00526          MOVE EMSG TO WS-ABEND-MESSAGE                            EL518
00527          GO TO ABEND-PGM.                                         EL518
00528                                                                   EL518
00529      GOBACK.                                                      EL518
00530                                                                   EL518
00531  ABEND-PGM.   COPY ELCABEND SUPPRESS.                             EL518
00532                                                                   EL518
