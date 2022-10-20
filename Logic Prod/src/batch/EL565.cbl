00001  IDENTIFICATION DIVISION.                                         04/18/98
00002                                                                   EL565
00003  PROGRAM-ID.                 EL565 .                                 LV002
00004 *              PROGRAM CONVERTED BY                               EL565
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL565
00006 *              CONVERSION DATE 02/12/96 16:51:03.                 EL565
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          EL565
00008 *                            VMOD=2.008                           EL565
00009                                                                   EL565
00010 *AUTHOR.     LOGIC INC.                                           EL565
00011 *            DALLAS, TEXAS.                                       EL565
00012                                                                   EL565
00013 *DATE-COMPILED.                                                   EL565
00014                                                                   EL565
00015 *SECURITY.   *****************************************************EL565
00016 *            *                                                   *EL565
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *EL565
00018 *            *                                                   *EL565
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL565
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL565
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *EL565
00022 *            *                                                   *EL565
00023 *            *****************************************************EL565
00024                                                                   EL565
00025 *REMARKS.                                                         EL565
00026                                                                   EL565
00027 *       GENERAL FUNCTION OF THIS PROGRAM IS TO READ THE           EL565
00028 *       COMPENSATION MASTER AND CREATE A GENERAL AGENT PAYMENT    EL565
00029 *       RECORD FOR EACH GENERAL AGENT WITH AN OUTSTANDING BAL.    EL565
00030                                                                   EL565
00031 *       INPUT FILES-    COMPENSATION MASTER                       EL565
00032 *                       CONTROL FILE                              EL565
00033 *                       DISK-DATE                                 EL565
00034                                                                   EL565
00035 *       OUTPUT-         PAYMENT AND ADJUSTMENT FILE               EL565
00036                                                                   EL565
00037      EJECT                                                        EL565
00038  ENVIRONMENT DIVISION.                                            EL565
00039                                                                   EL565
00040  INPUT-OUTPUT SECTION.                                            EL565
00041                                                                   EL565
00042  FILE-CONTROL.                                                    EL565
00043                                                                   EL565
00044      SELECT DISK-DATE        ASSIGN TO SYS019-FBA1-S-SYS019.      EL565
00045                                                                   EL565
00046      SELECT ERCOMP           ASSIGN TO SYS021-FBA1-ERCOMP         EL565
00047                              ORGANIZATION IS INDEXED              EL565
00048                              ACCESS IS DYNAMIC                    EL565
00049                              RECORD KEY IS CO-CONTROL-PRIMARY     EL565
00050                              FILE STATUS IS ERCOMP-FILE-STATUS.   EL565
00051                                                                   EL565
00052      SELECT ELCNTL           ASSIGN TO SYS022-FBA1-ELCNTL         EL565
00053                              ORGANIZATION IS INDEXED              EL565
00054                              ACCESS IS DYNAMIC                    EL565
00055                              RECORD KEY IS CF-CONTROL-PRIMARY     EL565
00056                              FILE STATUS IS ELCNTL-FILE-STATUS.   EL565
00057                                                                   EL565
00058      SELECT ERPYAJ           ASSIGN TO SYS023-FBA1-ERPYAJ         EL565
00059                              ORGANIZATION IS INDEXED              EL565
00060                              ACCESS IS DYNAMIC                    EL565
00061                              RECORD KEY IS PY-CONTROL-PRIMARY     EL565
00062                              FILE STATUS IS ERPYAJ-FILE-STATUS.   EL565
00063      EJECT                                                        EL565
00064  DATA DIVISION.                                                   EL565
00065                                                                   EL565
00066  FILE SECTION.                                                    EL565
00067                                                                   EL565
00068  FD  DISK-DATE                       COPY ELCDTEFD.               EL565
00069                                                                   EL565
00070  FD  ERCOMP.                                                      EL565
00071                                                                   EL565
00072                              COPY ERCCOMP.                        EL565
00073      EJECT                                                        EL565
00074                                                                   EL565
00075  FD  ELCNTL.                                                      EL565
00076                                                                   EL565
00077                              COPY ELCCNTL.                        EL565
00078      EJECT                                                        EL565
00079                                                                   EL565
00080  FD  ERPYAJ.                                                      EL565
00081                                                                   EL565
00082                              COPY ERCPYAJ.                        EL565
00083      EJECT                                                        EL565
00084  WORKING-STORAGE SECTION.                                         EL565
00085  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      EL565
00086                                                                   EL565
00087  77  FILLER  PIC X(32) VALUE '********************************'.  EL565
00088  77  FILLER  PIC X(32) VALUE '      EL565 WORKING-STORAGE     '.  EL565
00089  77  FILLER  PIC X(32) VALUE '******** VMOD=2.008 ************'.  EL565
00090                                                                   EL565
00091  01  STANDARD-AREAS.                                              EL565
00092      12  WS-ABEND-MESSAGE            PIC X(80)   VALUE SPACES.    EL565
00093      12  WS-ABEND-FILE-STATUS        PIC XX      VALUE ZERO.      EL565
00094      12  ERCOMP-FILE-STATUS          PIC XX      VALUE ZERO.      EL565
00095      12  ELCNTL-FILE-STATUS          PIC XX      VALUE ZERO.      EL565
00096      12  ERPYAJ-FILE-STATUS          PIC XX      VALUE ZERO.      EL565
00097                                                                   EL565
00098      12  WS-RETURN-CODE       PIC S9(4)    COMP    VALUE ZERO.    EL565
00099      12  WS-ZERO              PIC S9(4)    COMP    VALUE ZERO.    EL565
00100      12  WS-SAVE-DATE                PIC XX.                      EL565
00101      12  WS-SAVE-TIME                PIC 9(6).                    EL565
00102      12  ABEND-CODE                  PIC XXXX   VALUE SPACES.     EL565
00103      12  ABEND-OPTION                PIC X      VALUE SPACES.     EL565
00104      12  PGM-SUB                     PIC S999 COMP VALUE +565.    EL565
00105      12  WS-CR-CHECK-NO-METHOD  PIC X.                            EL565
00106          88  WS-CHECK-NO-MANUAL       VALUE '1'.                  EL565
00107          88  WS-CHECK-NO-AUTO-SEQ     VALUE '2'.                  EL565
00108          88  WS-CHECK-NO-AT-PRINT     VALUE '4'.                  EL565
00109      12  WS-CR-MONTH-END-DT     PIC XX.                           EL565
00110                                                                   EL565
00111      12  WS-ERPYAJ-RECORDS-WRITTEN   PIC 9(6)   VALUE ZERO.       EL565
00112      12  WS-CURRENT-DT               PIC XX     VALUE '  '.       EL565
00113                                                                   EL565
00114      12  WS-HOLD-DESCRIPTION         PIC X(30)  VALUE SPACES.     EL565
00115                                                                   EL565
00116      EJECT                                                        EL565
00117                                                                   EL565
00118                                      COPY ELCDATE.                   CL**2
00119                                                                   EL565
00120      EJECT                                                        EL565
00121                                                                   EL565
00122                                      COPY ELCDTECX.               EL565
00123                                                                   EL565
00124                                      COPY ELCDTEVR.               EL565
00125                                                                   EL565
00126  PROCEDURE DIVISION.                                              EL565
00127                                                                   EL565
00128  0000-LOAD-DATE-CARD.                COPY ELCDTERX.               EL565
00129      EJECT                                                        EL565
00130                                                                   EL565
00131      PERFORM 8000-INITIALIZE-ROUTINE.                             EL565
00132                                                                   EL565
00133      PERFORM 1000-PROCESS-COMPENSATION-MST.                       EL565
00134                                                                   EL565
00135      PERFORM 9000-EOJ-ROUTINE.                                    EL565
00136                                                                   EL565
00137      GOBACK.                                                      EL565
00138                                                                   EL565
00139  1000-PROCESS-COMPENSATION-MST     SECTION.                       EL565
00140                                                                   EL565
00141      MOVE LOW-VALUES             TO  CO-CONTROL-PRIMARY.          EL565
00142      MOVE DTE-CLASIC-COMPANY-CD  TO  CO-COMPANY-CD.               EL565
00143                                                                   EL565
00144      START ERCOMP  KEY NOT LESS THAN  CO-CONTROL-PRIMARY.         EL565
00145                                                                   EL565
00146      IF ERCOMP-FILE-STATUS  =  '23'                               EL565
00147          DISPLAY '*** EL565  ERCOMP START ERROR - JOB WILL ABEND' EL565
00148              UPON CONSOLE                                         EL565
00149          MOVE ERCOMP-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL565
00150          PERFORM ABEND-PGM.                                       EL565
00151                                                                   EL565
00152  1200-CRCOMP-LOOP.                                                EL565
00153      READ ERCOMP NEXT RECORD.                                     EL565
00154      IF ERCOMP-FILE-STATUS = '10'                                 EL565
00155          GO TO 1900-EXIT.                                         EL565
00156                                                                   EL565
00157      IF ERCOMP-FILE-STATUS NOT = ZEROS                            EL565
00158          DISPLAY '*** EL565  ERCOMP READ ERROR - JOB WILL ABEND'  EL565
00159              UPON CONSOLE                                         EL565
00160          MOVE ERCOMP-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL565
00161          PERFORM ABEND-PGM.                                       EL565
00162                                                                   EL565
00163      IF CO-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD                 EL565
00164         GO TO 1900-EXIT.                                          EL565
00165                                                                   EL565
00166      IF CO-COMPANY-TYPE                                           EL565
00167          GO TO 1200-CRCOMP-LOOP.                                  EL565
00168                                                                   EL565
00169      IF  CO-END-BAL LESS THAN +0                                  EL565
00170          NEXT SENTENCE                                            EL565
00171         ELSE                                                      EL565
00172          GO TO 1200-CRCOMP-LOOP.                                  EL565
00173                                                                   EL565
00174      IF CO-GEN-AGENT-TYPE                                         EL565
00175          MOVE 'G.A. COMPENSATION             '                    EL565
00176                                  TO WS-HOLD-DESCRIPTION           EL565
00177          GO TO 1250-BUILD-OUTPUT.                                 EL565
00178                                                                   EL565
00179      IF DTE-PRC-OPT NOT = '2'                                     EL565
00180          GO TO 1200-CRCOMP-LOOP                                   EL565
00181      ELSE                                                         EL565
00182          IF CO-NO-BALANCE                                         EL565
00183              GO TO 1200-CRCOMP-LOOP                               EL565
00184          ELSE                                                     EL565
00185              MOVE 'ACCOUNT COMPENSATION   '                       EL565
00186                                  TO WS-HOLD-DESCRIPTION.          EL565
00187                                                                   EL565
00188  1250-BUILD-OUTPUT.                                               EL565
00189      MOVE SPACES                 TO PENDING-PAY-ADJ.              EL565
00190      MOVE 'PY'                   TO PY-RECORD-ID.                 EL565
00191      MOVE CO-COMPANY-CD          TO PY-COMPANY-CD.                EL565
00192      MOVE CO-CARRIER             TO PY-CARRIER.                   EL565
00193      MOVE CO-GROUPING            TO PY-GROUPING.                  EL565
00194      MOVE CO-RESP-NO             TO PY-FIN-RESP.                  EL565
00195      MOVE CO-ACCOUNT             TO PY-ACCOUNT                    EL565
00196      MOVE 'C'                    TO PY-RECORD-TYPE.               EL565
00197      MOVE WS-HOLD-DESCRIPTION    TO PY-ENTRY-COMMENT.             EL565
00198                                                                   EL565
00199      ACCEPT WS-TIME-OF-DAY       FROM TIME.                       EL565
00200                                                                   EL565
00201      MOVE WS-TIME                TO PY-FILE-SEQ-NO.               EL565
00202      COMPUTE PY-ENTRY-AMT = -1 * CO-END-BAL.                      EL565
00203                                                                   EL565
00204      IF WS-CHECK-NO-AT-PRINT                                      EL565
00205         MOVE SPACES              TO PY-CHECK-NUMBER               EL565
00206        ELSE                                                       EL565
00207         PERFORM 6000-READ-COMPANY-RECORD                          EL565
00208         MOVE CF-CR-CHECK-COUNTER TO PY-CHECK-NUMBER               EL565
00209         PERFORM 6100-REWRITE-COMPANY.                             EL565
00210                                                                   EL565
00211      MOVE DTE-CLIENT             TO PY-LAST-MAINT-BY.             EL565
00212                                                                   EL565
00213      ACCEPT WS-TIME-OF-DAY       FROM TIME.                       EL565
00214                                                                   EL565
00215      MOVE WS-TIME                TO PY-LAST-MAINT-HHMMSS.         EL565
00216      MOVE WS-CURRENT-DT          TO PY-LAST-MAINT-DT.             EL565
00217      MOVE LOW-VALUES             TO PY-BILLED-DATE.               EL565
00218      MOVE ZEROS                  TO PY-CHECK-QUE-CONTROL          EL565
00219                                     PY-CHECK-QUE-SEQUENCE.        EL565
00220      MOVE LOW-VALUES             TO PY-CREDIT-ACCEPT-DT           EL565
00221                                     PY-REPORTED-DT                EL565
00222                                     PY-CHECK-WRITTEN-DT           EL565
00223                                     PY-AR-DATE.                   EL565
00224      MOVE WS-CR-MONTH-END-DT     TO PY-CREDIT-SELECT-DT.          EL565
00225      MOVE 'G'                    TO PY-CHECK-ORIGIN-SW.           EL565
00226                                                                   EL565
00227      WRITE PENDING-PAY-ADJ.                                       EL565
00228                                                                   EL565
00229      ADD +1  TO WS-ERPYAJ-RECORDS-WRITTEN.                        EL565
00230      IF ERPYAJ-FILE-STATUS NOT = ZEROS                            EL565
00231          DISPLAY '*** EL565  ERPYAJ WRITE  ERROR - JOB WILL ABEND'EL565
00232              UPON CONSOLE                                         EL565
00233          MOVE ERPYAJ-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL565
00234          PERFORM ABEND-PGM.                                       EL565
00235                                                                   EL565
00236      GO TO 1200-CRCOMP-LOOP.                                      EL565
00237                                                                   EL565
00238  1900-EXIT.                                                       EL565
00239      EXIT.                                                        EL565
00240                                                                   EL565
00241      EJECT                                                        EL565
00242                                                                   EL565
00243  6000-READ-COMPANY-RECORD     SECTION.                            EL565
00244      MOVE SPACES                 TO CF-CONTROL-PRIMARY            EL565
00245      MOVE DTE-CLIENT             TO CF-COMPANY-ID                 EL565
00246      MOVE '1'                    TO CF-RECORD-TYPE                EL565
00247      MOVE ZEROS                  TO CF-SEQUENCE-NO                EL565
00248      READ ELCNTL                                                  EL565
00249      IF ELCNTL-FILE-STATUS NOT = ZEROS                            EL565
00250          MOVE 'ERROR OCCURED READING CNTL RECORD ' TO             EL565
00251                                            WS-ABEND-MESSAGE       EL565
00252          MOVE ELCNTL-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL565
00253          GO TO ABEND-PGM.                                         EL565
00254                                                                   EL565
00255  6000-EXIT.                                                       EL565
00256      EXIT.                                                        EL565
00257                                                                   EL565
00258  6100-REWRITE-COMPANY     SECTION.                                EL565
00259      IF NOT CR-CHECK-CNT-RESET-VALUE                              EL565
00260          ADD +1              TO CF-CR-CHECK-COUNTER               EL565
00261      ELSE                                                         EL565
00262          MOVE +1             TO CF-CR-CHECK-COUNTER.              EL565
00263      REWRITE CONTROL-FILE.                                        EL565
00264      IF ELCNTL-FILE-STATUS NOT = ZEROS                            EL565
00265          MOVE 'ERROR OCCURED REWRITING CNTL RECORD ' TO           EL565
00266                                            WS-ABEND-MESSAGE       EL565
00267          MOVE ELCNTL-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL565
00268          GO TO ABEND-PGM.                                         EL565
00269                                                                   EL565
00270  6100-EXIT.                                                       EL565
00271      EXIT.                                                        EL565
00272      EJECT                                                        EL565
00273  8000-INITIALIZE-ROUTINE        SECTION.                          EL565
00274                                                                   EL565
00275      MOVE WS-CURRENT-DATE        TO DC-GREG-DATE-1-EDIT.          EL565
00276      MOVE '2'                    TO DC-OPTION-CODE.               EL565
00277      PERFORM 8500-DATE-CONVERSION.                                EL565
00278                                                                   EL565
00279      IF DATE-CONVERSION-ERROR                                     EL565
00280          DISPLAY '*** EL565  DATE CONVERSION ERROR JOB WILL ABEND'EL565
00281              UPON CONSOLE                                         EL565
00282          MOVE DC-ERROR-CODE      TO WS-ABEND-FILE-STATUS          EL565
00283          PERFORM ABEND-PGM.                                       EL565
00284                                                                   EL565
00285      MOVE DC-BIN-DATE-1          TO WS-CURRENT-DT.                EL565
00286      MOVE DC-GREG-DATE-1-ALPHA   TO ALPH-DATE.                    EL565
00287                                                                   EL565
00288      OPEN INPUT ERCOMP.                                           EL565
00289      IF ERCOMP-FILE-STATUS  = '00' OR '97'                        EL565
00290          NEXT SENTENCE                                            EL565
00291        ELSE                                                       EL565
00292          DISPLAY '*** EL565  ERCOMP OPEN ERROR - JOB WILL ABEND'  EL565
00293              UPON CONSOLE                                         EL565
00294          MOVE ERCOMP-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL565
00295          PERFORM ABEND-PGM.                                       EL565
00296                                                                   EL565
00297      OPEN I-O ERPYAJ.                                             EL565
00298      IF ERPYAJ-FILE-STATUS  = '00' OR '97'                        EL565
00299          NEXT SENTENCE                                            EL565
00300        ELSE                                                       EL565
00301          DISPLAY '*** EL565  ERPYAJ OPEN ERROR - JOB WILL ABEND'  EL565
00302              UPON CONSOLE                                         EL565
00303          MOVE ERPYAJ-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL565
00304          PERFORM ABEND-PGM.                                       EL565
00305                                                                   EL565
00306      OPEN I-O ELCNTL.                                             EL565
00307      IF ELCNTL-FILE-STATUS  = '00' OR '97'                        EL565
00308          NEXT SENTENCE                                            EL565
00309        ELSE                                                       EL565
00310          DISPLAY '*** EL565  ELCNTL OPEN ERROR - JOB WILL ABEND'  EL565
00311              UPON CONSOLE                                         EL565
00312          MOVE ELCNTL-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL565
00313          PERFORM ABEND-PGM.                                       EL565
00314                                                                   EL565
00315                                                                   EL565
00316      PERFORM 6000-READ-COMPANY-RECORD.                            EL565
00317                                                                   EL565
00318      IF  CF-COMPENSATION-MSTR-MAINT-DT = LOW-VALUES               EL565
00319          DISPLAY '*** EL565  ERCOMP ERROR - JOB WILL ABEND'       EL565
00320              UPON CONSOLE                                         EL565
00321          DISPLAY '*** MONTH END PROCESSING HAS NOT COMPLETED ****'EL565
00322              UPON CONSOLE                                         EL565
00323          MOVE '  '               TO WS-ABEND-FILE-STATUS          EL565
00324          PERFORM ABEND-PGM.                                       EL565
00325                                                                   EL565
00326      MOVE CF-CR-CHECK-NO-METHOD  TO WS-CR-CHECK-NO-METHOD         EL565
00327      MOVE CF-CR-MONTH-END-DT TO WS-CR-MONTH-END-DT.               EL565
00328      IF WS-CHECK-NO-MANUAL                                        EL565
00329         MOVE 'INVALID CHECK CONTROL - MANUAL' TO                  EL565
00330                             WS-ABEND-MESSAGE                      EL565
00331         GO TO ABEND-PGM.                                          EL565
00332                                                                   EL565
00333                                                                   EL565
00334      EJECT                                                        EL565
00335                                                                   EL565
00336  8500-DATE-CONVERSION SECTION. COPY ELCDCS.                       EL565
00337      EJECT                                                        EL565
00338                                                                   EL565
00339  9000-EOJ-ROUTINE      SECTION.                                   EL565
00340                                                                   EL565
00341      DISPLAY 'ERPYAJ RECORDS WRITTEN ' WS-ERPYAJ-RECORDS-WRITTEN. EL565
00342                                                                   EL565
00343      CLOSE ERCOMP.                                                EL565
00344      IF ERCOMP-FILE-STATUS NOT = ZEROS                            EL565
00345          DISPLAY '*** EL565  ERCOMP CLOSE ERROR - JOB WILL ABEND' EL565
00346              UPON CONSOLE                                         EL565
00347          MOVE ERCOMP-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL565
00348          PERFORM ABEND-PGM.                                       EL565
00349                                                                   EL565
00350      CLOSE ERPYAJ.                                                EL565
00351      IF ERPYAJ-FILE-STATUS NOT = ZEROS                            EL565
00352          DISPLAY '*** EL565  ERPYAJ CLOSE ERROR - JOB WILL ABEND' EL565
00353              UPON CONSOLE                                         EL565
00354          MOVE ERPYAJ-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL565
00355          PERFORM ABEND-PGM.                                       EL565
00356                                                                   EL565
00357      CLOSE ELCNTL.                                                EL565
00358      IF ELCNTL-FILE-STATUS NOT = ZEROS                            EL565
00359          DISPLAY '*** EL565  ERPYAJ CLOSE ERROR - JOB WILL ABEND' EL565
00360              UPON CONSOLE                                         EL565
00361          MOVE ELCNTL-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL565
00362          PERFORM ABEND-PGM.                                       EL565
00363                                                                   EL565
00364  9100-EXIT.                                                       EL565
00365      EXIT.                                                        EL565
00366  ABEND-PGM SECTION. COPY ELCABEND.                                EL565
00367                                                                   EL565
