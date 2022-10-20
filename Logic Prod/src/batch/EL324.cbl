00001  IDENTIFICATION DIVISION.                                         04/18/98
00002                                                                   EL324
00003  PROGRAM-ID.                 EL324 .                                 LV003
00004 *              PROGRAM CONVERTED BY                               EL324
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL324
00006 *              CONVERSION DATE 02/14/96 13:39:26.                 EL324
00007 *           PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE             EL324
00008 *                            VMOD=2.006                           EL324
00009                                                                   EL324
00009                                                                   EL324
00010 *AUTHOR.     LOGIC,INC.                                           EL324
00011 *            DALLAS, TEXAS.                                       EL324
00012                                                                   EL324
00013 *DATE-COMPILED.                                                   EL324
00014                                                                   EL324
00015 *SECURITY.   *****************************************************EL324
00016 *            *                                                   *EL324
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *EL324
00018 *            *                                                   *EL324
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL324
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL324
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *EL324
00022 *            *                                                   *EL324
00023 *            *****************************************************EL324
00024                                                                   EL324
00025 *REMARKS.                                                         EL324
00026 *        THIS PROGRAM PRINTS A REGISTER OF ALL CORESPONDENCE FOR  EL324
00027 *    WHICH ACTIVITY HAS BEEN RECORDED OR THAT HAS ACTIVITY DUE.   EL324
00028 *    ACTIVITY REPORTED INCLUDES.                                  EL324
00029 *                                                                 EL324
00030 *        1.  LETTERS SENT                                         EL324
00031 *        2.  LETTERS RECEIVED                                     EL324
00032 *        3.  LETTERS SCHEDULED FOR RESENT (NOT RECEIVED)          EL324
00033 *        4.  LETTERS SCHEDULED FOR FOLLOW-UP  (NOT RECEIVED)      EL324
00034 *        5.  LETTERS RE-PRINTED.                                  EL324
00035 *                                                                 EL324
00036      EJECT                                                        EL324
00037  ENVIRONMENT DIVISION.                                            EL324
00038                                                                   EL324
00039  INPUT-OUTPUT SECTION.                                            EL324
00040                                                                   EL324
00041  FILE-CONTROL.                                                    EL324
00042                                                                   EL324
00043      SELECT REPORTS-EXTRACT-FILE                                  EL324
00044          ASSIGN TO SYS010-UT-2400-S-SYS010.                       EL324
00045                                                                   EL324
00046      SELECT DISK-DATE        ASSIGN TO SYS019-UT-FBA1-S-SYS019.   EL324
00047                                                                   EL324
00048      SELECT PRNTR            ASSIGN TO SYS008-UR-1403-S-SYS008.   EL324
00049                                                                   EL324
00050      SELECT FICH             ASSIGN TO SYS020-UT-2400-S-SYS020.   EL324
00051                                                                   EL324
00052      SELECT ELREPT           ASSIGN TO SYS018-FBA1-ELREPT         EL324
00053                              ORGANIZATION IS INDEXED              EL324
00054                              ACCESS IS DYNAMIC                    EL324
00055                              RECORD KEY IS RF-CONTROL-PRIMARY     EL324
00056                              FILE STATUS IS DTE-VSAM-FLAGS.       EL324
00057                                                                   EL324
00058      EJECT                                                        EL324
00059  DATA DIVISION.                                                   EL324
00060                                                                   EL324
00061  FILE SECTION.                                                    EL324
00062                                                                   EL324
00063  FD  REPORTS-EXTRACT-FILE COPY ELCEXTFD.                          EL324
00064                                                                   EL324
00065                              COPY ELCEXTR.                        EL324
00066                                                                   EL324
00067      EJECT                                                        EL324
00068  FD  DISK-DATE               COPY ELCDTEFD.                       EL324
00069                                                                   EL324
00070  FD  PRNTR                   COPY ELCPRTFD.                       EL324
00071                                                                   EL324
00072  FD  FICH                    COPY ELCFCHFD.                       EL324
00073                                                                   EL324
00074  FD  ELREPT                  COPY ELCRPTFD.                       EL324
00075                                                                   EL324
00076                              COPY ELCREPT.                        EL324
00077                                                                   EL324
00078      EJECT                                                        EL324
00079  WORKING-STORAGE SECTION.                                         EL324
00080  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      EL324
00081                                                                   EL324
00082  77  FILLER  PIC X(32)   VALUE '********************************'.EL324
00083  77  FILLER  PIC X(32)   VALUE '*     EL324  WORKING STORAGE   *'.EL324
00084  77  FILLER  PIC X(32)   VALUE '********* VMOD=2.006 ***********'.EL324
00085                                                                   EL324
00086  01  FILLER                          COMP-3.                      EL324
00087      05  WS-LINE-COUNT               PIC S9(3)       VALUE +99.   EL324
00088      05  WS-LINE-COUNT-MAX           PIC S9(3)       VALUE +59.   EL324
00089      05  WS-PAGE                     PIC S9(5)       VALUE ZERO.  EL324
00090      05  WS-REPORT-SW                PIC S9          VALUE +1.    EL324
00091      05  WS-PRINT-SW                 PIC S9          VALUE ZERO.  EL324
00092      05  WS-RECORD-COUNT             PIC S9(9)       VALUE ZERO.  EL324
00093      05  WS-RETURN-CODE              PIC S9(3)       VALUE ZERO.  EL324
00094      05  WS-ZERO                     PIC S9          VALUE ZERO.  EL324
00095                                                                   EL324
00096      EJECT                                                        EL324
00097  01  FILLER                          COMP SYNC.                   EL324
00098      05  PGM-SUB                     PIC S9(4)       VALUE +324.  EL324
00099      05  WS-INDEX                    PIC S9(4)       VALUE ZERO.  EL324
00100                                                                   EL324
00101  01  FILLER.                                                      EL324
00102      05  ABEND-CODE                  PIC X(4).                    EL324
00103      05  ABEND-OPTION                PIC X.                       EL324
00104      05  OLC-REPORT-NAME             PIC X(5) VALUE 'EL324'.      EL324
00105      05  X                           PIC X           VALUE SPACE. EL324
00106                                                                   EL324
00107      05  WS-SAVE-PRINT-RECORD        PIC X(133)      VALUE SPACES.EL324
00108                                                                   EL324
00109      05  WS-ABEND-MESSAGE            PIC X(80)       VALUE SPACES.EL324
00110                                                                   EL324
00111      05  WS-ABEND-FILE-STATUS        PIC XX          VALUE ZERO.  EL324
00112                                                                   EL324
00113      05  WS-FILE-ERROR-MESSAGE.                                   EL324
00114          10  FILLER                  PIC X(24)       VALUE        EL324
00115              'ERROR OCCURED OPENING - '.                          EL324
00116          10  WS-FEM-FILE-NAME        PIC X(8).                    EL324
00117                                                                   EL324
00118      EJECT                                                        EL324
00119  01  WS-HEADING1.                                                 EL324
00120      05  FILLER                      PIC X(49)       VALUE '1'.   EL324
00121      05  WS-H1-TITLE                 PIC X(71)       VALUE        EL324
00122          'CORRESPONDENCE REGISTER'.                               EL324
00123      05  WS-H1-REPORT-NUMBER         PIC X(9) VALUE 'EL324'.      EL324
00124                                                                   EL324
00125  01  WS-HEADING2.                                                 EL324
00126      05  FILLER                      PIC X(45)       VALUE SPACES.EL324
00127      05  WS-H2-CLIENT-NAME           PIC X(75)       VALUE SPACES.EL324
00128      05  WS-H2-DATE                  PIC X(8)        VALUE SPACES.EL324
00129      05  FILLER                      PIC X           VALUE SPACES.EL324
00130                                                                   EL324
00131  01  WS-HEADING3.                                                 EL324
00132      05  FILLER                      PIC X(51)       VALUE SPACES.EL324
00133      05  WS-H3-DATE                  PIC X(69)       VALUE SPACES.EL324
00134      05  FILLER                      PIC X(5)        VALUE 'PAGE'.EL324
00135      05  WS-H3-PAGE                  PIC ZZ,ZZ9.                  EL324
00136      05  FILLER                      PIC X(11)       VALUE SPACES.EL324
00137                                                                   EL324
00138  01  WS-HEADING4.                                                 EL324
00139      05  FILLER                      PIC X(47)       VALUE        EL324
00140          '-    CLAIM      CERT     INSURED'.                      EL324
00141      05  FILLER                      PIC X(86)       VALUE        EL324
00142          'ARCHIVE    SEND    INITIAL  ANSWER   RE-SEND  RE-SEND  AEL324
00143 -        'DDRESSEE NAME/'.                                        EL324
00144                                                                   EL324
00145  01  WS-HEADING5.                                                 EL324
00146      05  FILLER                      PIC X(41)       VALUE        EL324
00147          ' CAR NUMBER    NUMBER    LAST NAME'.                    EL324
00148      05  FILLER                      PIC X(92)       VALUE        EL324
00149          'FORM   NUMBER    DATE     PRINT  RECEIVED   DATE    PRINEL324
00150 -        'TED            TYPE'.                                   EL324
00151                                                                   EL324
00152      EJECT                                                        EL324
00153  01  WS-DETAIL1.                                                  EL324
00154      05  FILLER                      PIC XX.                      EL324
00155      05  WS-D1-CARRIER               PIC X.                       EL324
00156      05  FILLER                      PIC X.                       EL324
00157      05  WS-D1-CLAIM-NO              PIC X(7).                    EL324
00158      05  FILLER                      PIC X.                       EL324
00159      05  WS-D1-CERT-NO               PIC X(11).                   EL324
00160      05  FILLER                      PIC X.                       EL324
00161      05  WS-D1-INSURED-LAST-NAME     PIC X(15).                   EL324
00162      05  FILLER                      PIC XX.                      EL324
00163      05  WS-D1-FORM                  PIC X(4).                    EL324
00164      05  FILLER                      PIC X.                       EL324
00165      05  WS-D1-ARCHIVE-NUMBER        PIC Z(7)9.                   EL324
00166                                                                   EL324
00167      05  WS-D1-ARCHIVE-NUMBER-X      REDEFINES                    EL324
00168          WS-D1-ARCHIVE-NUMBER        PIC X(8).                    EL324
00169      05  FILLER                      PIC XX.                      EL324
00170      05  WS-D1-SEND-DATE             PIC X(8).                    EL324
00171      05  FILLER                      PIC X.                       EL324
00172      05  WS-D1-INITIAL-PRINT         PIC X(8).                    EL324
00173      05  FILLER                      PIC X.                       EL324
00174      05  WS-D1-ANSWER-RECEIVED       PIC X(8).                    EL324
00175      05  FILLER                      PIC X.                       EL324
00176      05  WS-D1-RESEND-DATE           PIC X(8).                    EL324
00177      05  FILLER                      PIC X.                       EL324
00178      05  WS-D1-RESEND-PRINTED        PIC X(8).                    EL324
00179      05  FILLER                      PIC XX.                      EL324
00180      05  WS-D1-ADDRESSEE-NAME        PIC X(30).                   EL324
00181      05  FILLER                      PIC X.                       EL324
00182                                                                   EL324
00183      EJECT                                                        EL324
00184  01  WS-DETAIL2                      REDEFINES                    EL324
00185      WS-DETAIL1.                                                  EL324
00186      05  FILLER                      PIC X(24).                   EL324
00187      05  WS-D2-DESCRIPTION           PIC X(7).                    EL324
00188      05  WS-D2-REASON                PIC X(70).                   EL324
00189      05  FILLER                      PIC X.                       EL324
00190      05  WS-D2-ADDRESSEE-TYPE        PIC X(11).                   EL324
00191      05  FILLER                      PIC X(10).                   EL324
00192      05  WS-D2-BY-DESC               PIC X(5).                    EL324
00193      05  WS-D2-BY                    PIC X(4).                    EL324
00194      05  FILLER                      PIC X.                       EL324
00195                                                                   EL324
00196      EJECT                                                        EL324
00197                                  COPY ELCDTECX.                   EL324
00198      EJECT                                                        EL324
00199                                  COPY ELCDTEVR.                   EL324
00200                                                                   EL324
00201                                  COPY ELCDATE.                       CL**3
00202                                                                   EL324
00203      EJECT                                                        EL324
00204  PROCEDURE DIVISION.                                              EL324
00205                                                                   EL324
00206  0000-DATE-CARD-READ SECTION. COPY ELCDTERX.                      EL324
00207                                                                   EL324
00208  1000-MAIN-LOGIC SECTION.                                         EL324
00209                                                                   EL324
00210      PERFORM OPEN-FILES.                                          EL324
00211                                                                   EL324
00212      PERFORM 3000-PRINT-REPORT.                                   EL324
00213                                                                   EL324
00214      IF WS-RECORD-COUNT  EQUAL  ZERO                              EL324
00215         PERFORM WRITE-HEADINGS                                    EL324
00216         MOVE '**** NO TRANSACTION FOR THIS REPORT ****'           EL324
00217                                    TO  P-DATA                     EL324
00218         MOVE '-'                   TO  X                          EL324
00219         PERFORM WRITE-A-LINE.                                     EL324
00220                                                                   EL324
00221      PERFORM CLOSE-FILES.                                         EL324
00222                                                                   EL324
00223      GOBACK.                                                      EL324
00224                                                                   EL324
00225      EJECT                                                        EL324
00226  3000-PRINT-REPORT SECTION.                                       EL324
00227                                                                   EL324
00228  3100-PRINT-REPORT.                                               EL324
00229      READ REPORTS-EXTRACT-FILE                                    EL324
00230          AT END                                                   EL324
00231              GO TO 3900-EXIT.                                     EL324
00232                                                                   EL324
00233      IF EX-POSITIONING-CODE LESS THAN '5'                         EL324
00234          GO TO 3100-PRINT-REPORT.                                 EL324
00235                                                                   EL324
00236      IF EX-POSITIONING-CODE GREATER THAN '5'                      EL324
00237          GO TO 3900-EXIT.                                         EL324
00238                                                                   EL324
00239      IF EX-EXTRACT-CODE LESS THAN 'E'                             EL324
00240          GO TO 3100-PRINT-REPORT.                                 EL324
00241                                                                   EL324
00242      IF EX-EXTRACT-CODE GREATER THAN 'E'                          EL324
00243          GO TO 3900-EXIT.                                         EL324
00244                                                                   EL324
00245      IF EX-COMPANY-CD LESS THAN DTE-CLASIC-COMPANY-CD             EL324
00246          GO TO 3100-PRINT-REPORT.                                 EL324
00247                                                                   EL324
00248      IF EX-COMPANY-CD GREATER THAN DTE-CLASIC-COMPANY-CD          EL324
00249          GO TO 3900-EXIT.                                         EL324
00250                                                                   EL324
00251      IF EX-RECORD-TYPE GREATER THAN 'A'                           EL324
00252          GO TO 3900-EXIT.                                         EL324
00253                                                                   EL324
PEMTST     IF DTE-PGM-OPT = 2
PEMTST        IF (BIN-RUN-DATE = EX-EA-LETTER-SENT-DT) OR
PEMTST           (BIN-RUN-DATE = EX-EA-INITIAL-PRINT-DT)
PEMTST           CONTINUE
PEMTST        ELSE
PEMTST           GO TO 3100-PRINT-REPORT
PEMTST        END-IF
PEMTST     END-IF
PEMTST
00254      EJECT                                                        EL324
00255      MOVE '0'                     TO  WS-DETAIL1.                 EL324
00256      MOVE EX-SE-CARRIER           TO  WS-D1-CARRIER.              EL324
00257      MOVE EX-SE-CLAIM-NO          TO  WS-D1-CLAIM-NO.             EL324
00258      MOVE EX-SE-CERT-NO           TO  WS-D1-CERT-NO.              EL324
00259      MOVE EX-EA-INSURED-LAST-NAME TO  WS-D1-INSURED-LAST-NAME.    EL324
00260                                                                   EL324
00261      IF  EX-EA-LETTER-ARCHIVE-NO GREATER THAN ZERO                EL324
00262          MOVE EX-EA-STD-LETTER-FORM   TO  WS-D1-FORM              EL324
00263          MOVE EX-EA-LETTER-ARCHIVE-NO TO  WS-D1-ARCHIVE-NUMBER    EL324
00264      ELSE                                                         EL324
00265          MOVE 'FORM'                  TO  WS-D1-ARCHIVE-NUMBER-X  EL324
00266          IF  EX-EA-STD-LETTER-FORM = '1'                          EL324
00267              MOVE 'INIT'  TO  WS-D1-FORM                          EL324
00268          ELSE                                                     EL324
00269              IF  EX-EA-STD-LETTER-FORM = '2'                      EL324
00270                  MOVE 'INIT'  TO  WS-D1-FORM                      EL324
00271              ELSE                                                 EL324
00272                  MOVE EX-EA-STD-LETTER-FORM TO  WS-D1-FORM.       EL324
00273                                                                   EL324
00274      IF EX-EA-LETTER-SENT-DT NOT = LOW-VALUES                     EL324
00275          MOVE    EX-EA-LETTER-SENT-DT   TO  DC-BIN-DATE-1         EL324
00276          MOVE    SPACES                 TO  DC-OPTION-CODE        EL324
00277          PERFORM 8500-DATE-CONVERSION                             EL324
00278          MOVE    DC-GREG-DATE-1-EDIT    TO  WS-D1-SEND-DATE.      EL324
00279                                                                   EL324
00280      IF EX-EA-INITIAL-PRINT-DT NOT = LOW-VALUES                   EL324
00281          MOVE    EX-EA-INITIAL-PRINT-DT TO  DC-BIN-DATE-1         EL324
00282          MOVE    SPACES                 TO  DC-OPTION-CODE        EL324
00283          PERFORM 8500-DATE-CONVERSION                             EL324
00284          MOVE    DC-GREG-DATE-1-EDIT    TO  WS-D1-INITIAL-PRINT.  EL324
00285                                                                   EL324
00286      IF EX-EA-LETTER-ANSWERED-DT NOT = LOW-VALUES                 EL324
00287          MOVE    EX-EA-LETTER-ANSWERED-DT TO  DC-BIN-DATE-1       EL324
00288          MOVE    SPACES                   TO  DC-OPTION-CODE      EL324
00289          PERFORM 8500-DATE-CONVERSION                             EL324
00290          MOVE    DC-GREG-DATE-1-EDIT    TO  WS-D1-ANSWER-RECEIVED.EL324
00291                                                                   EL324
00292      IF EX-EA-AUTO-RE-SEND-DT NOT = LOW-VALUES                    EL324
00293          MOVE    EX-EA-AUTO-RE-SEND-DT  TO  DC-BIN-DATE-1         EL324
00294          MOVE    SPACES                 TO  DC-OPTION-CODE        EL324
00295          PERFORM 8500-DATE-CONVERSION                             EL324
00296          MOVE    DC-GREG-DATE-1-EDIT    TO  WS-D1-RESEND-DATE.    EL324
00297                                                                   EL324
00298      IF EX-EA-RESEND-PRINT-DT NOT = LOW-VALUES                    EL324
00299          MOVE    EX-EA-RESEND-PRINT-DT  TO  DC-BIN-DATE-1         EL324
00300          MOVE    SPACES                 TO  DC-OPTION-CODE        EL324
00301          PERFORM 8500-DATE-CONVERSION                             EL324
00302          MOVE    DC-GREG-DATE-1-EDIT    TO  WS-D1-RESEND-PRINTED. EL324
00303                                                                   EL324
00304      MOVE    EX-EA-ADDRESEE-NAME    TO  WS-D1-ADDRESSEE-NAME.     EL324
00305      MOVE    WS-DETAIL1             TO  PRT.                      EL324
00306      PERFORM WRITE-A-LINE.                                        EL324
00307      MOVE    SPACES                 TO  WS-DETAIL2.               EL324
00308      MOVE    'REASON-'              TO  WS-D2-DESCRIPTION.        EL324
00309      MOVE    EX-EA-REASON           TO  WS-D2-REASON.             EL324
00310                                                                   EL324
00311      IF EX-EA-ADDRESEE-TYPE = 'I'                                 EL324
00312         MOVE 'INSURED'     TO  WS-D2-ADDRESSEE-TYPE               EL324
00313      ELSE                                                         EL324
00314         IF EX-EA-ADDRESEE-TYPE = 'B'                              EL324
00315            MOVE 'BENEFICIARY' TO  WS-D2-ADDRESSEE-TYPE            EL324
00316         ELSE                                                      EL324
00317            IF EX-EA-ADDRESEE-TYPE = 'A'                           EL324
00318               MOVE 'ACCOUNT'     TO  WS-D2-ADDRESSEE-TYPE         EL324
00319            ELSE                                                   EL324
00320               IF EX-EA-ADDRESEE-TYPE = 'P'                        EL324
00321                  MOVE 'PHYSICIAN'   TO  WS-D2-ADDRESSEE-TYPE      EL324
00322               ELSE                                                EL324
00323                  IF EX-EA-ADDRESEE-TYPE = 'E'                     EL324
00324                     MOVE 'EMPLOYER'    TO                         EL324
00325                             WS-D2-ADDRESSEE-TYPE                  EL324
00326                  ELSE                                             EL324
00327                     IF EX-EA-ADDRESEE-TYPE = 'O'                  EL324
00328                        MOVE 'OTHER 1'     TO                      EL324
00329                                    WS-D2-ADDRESSEE-TYPE           EL324
00330                     ELSE                                          EL324
00331                        IF EX-EA-ADDRESEE-TYPE = 'Q'               EL324
00332                           MOVE 'OTHER 2'     TO                   EL324
00333                                        WS-D2-ADDRESSEE-TYPE       EL324
00334                        ELSE                                       EL324
00335                           MOVE EX-EA-ADDRESEE-TYPE TO             EL324
00336                                     WS-D2-ADDRESSEE-TYPE.         EL324
00337                                                                   EL324
00338      ADD +1                      TO  WS-RECORD-COUNT.             EL324
00339      MOVE 'BY - '                TO  WS-D2-BY-DESC.               EL324
00340      MOVE EX-EA-RECORDED-BY      TO  WS-D2-BY.                    EL324
00341      MOVE WS-DETAIL2             TO  PRT.                         EL324
00342      PERFORM WRITE-A-LINE.                                        EL324
00343      GO TO 3100-PRINT-REPORT.                                     EL324
00344                                                                   EL324
00345  3900-EXIT.                                                       EL324
00346      EXIT.                                                        EL324
00347                                                                   EL324
00348      EJECT                                                        EL324
00349  8500-DATE-CONVERSION SECTION. COPY ELCDCS.                       EL324
00350                                                                   EL324
00351      EJECT                                                        EL324
00352  WRITE-A-LINE SECTION. COPY ELCWAL.                               EL324
00353                                                                   EL324
00354  WRITE-HEADINGS SECTION.                                          EL324
00355                                                                   EL324
00356  WHS-010.                                                         EL324
00357      IF  WS-H2-DATE EQUAL SPACES                                  EL324
00358          MOVE WS-CURRENT-DATE    TO  WS-H2-DATE                   EL324
00359          MOVE COMPANY-NAME       TO  WS-H2-CLIENT-NAME            EL324
00360          MOVE ALPH-DATE          TO  WS-H3-DATE.                  EL324
00361                                                                   EL324
00362      ADD +1  TO  WS-PAGE.                                         EL324
00363      MOVE WS-PAGE                TO  WS-H3-PAGE.                  EL324
00364      MOVE PRT                    TO  WS-SAVE-PRINT-RECORD.        EL324
00365      MOVE ZERO                   TO  WS-LINE-COUNT.               EL324
00366                                                                   EL324
00367      MOVE WS-HEADING1            TO  PRT.                         EL324
00368      MOVE '1'                    TO  X.                           EL324
00369      PERFORM WRITE-PRINTER.                                       EL324
00370                                                                   EL324
00371      MOVE WS-HEADING2            TO  PRT.                         EL324
00372      MOVE ' '                    TO  X.                           EL324
00373      PERFORM WRITE-PRINTER.                                       EL324
00374                                                                   EL324
00375      MOVE WS-HEADING3            TO  PRT.                         EL324
00376      MOVE ' '                    TO  X.                           EL324
00377      PERFORM WRITE-PRINTER.                                       EL324
00378                                                                   EL324
00379      MOVE WS-HEADING4            TO  PRT.                         EL324
00380      MOVE ' '                    TO  X.                           EL324
00381      PERFORM WRITE-PRINTER.                                       EL324
00382                                                                   EL324
00383                                                                   EL324
00384      MOVE WS-HEADING5            TO  PRT.                         EL324
00385                                                                   EL324
00386      PERFORM WRITE-PRINTER.                                       EL324
00387                                                                   EL324
00388      MOVE +7                     TO  WS-LINE-COUNT.               EL324
00389                                                                   EL324
00390  WHS-020. COPY ELCWHS2.                                           EL324
00391                                                                   EL324
00392  WRITE-PRINTER SECTION. COPY ELCWPS.                              EL324
00393                                                                   EL324
00394                                                                   EL324
00395      IF DTE-FICH NOT = SPACE AND                                  EL324
00396          FICH-OPEN   = SPACE                                      EL324
00397          MOVE 'X' TO FICH-OPEN                                    EL324
00398          OPEN OUTPUT FICH.                                        EL324
00399                                                                   EL324
00400      IF DTE-PRT-OPT = 'S' OR 'T'                                  EL324
00401          IF (REPT-OPEN = SPACE) AND (DTE-ABEND-CD-1 = SPACE)      EL324
00402              OPEN I-O ELREPT                                      EL324
00403              IF DTE-F-1 NOT = ZERO AND                            EL324
00404                 DTE-VSAM-FLAGS NOT = '97'                         EL324
00405                  MOVE DTE-VSAM-FLAGS  TO  WS-ABEND-FILE-STATUS    EL324
00406                  MOVE 'ERROR OCCURED OPEN - ELREPT'               EL324
00407                                  TO  WS-ABEND-MESSAGE             EL324
00408                  GO TO ABEND-PGM                                  EL324
00409              ELSE                                                 EL324
00410                  MOVE '1'                   TO REPT-OPEN          EL324
00411                  MOVE DTE-CLASIC-COMPANY-CD TO RF-COMPANY-CD      EL324
00412                  MOVE '1'                   TO RF-RECORD-TYPE     EL324
00413                  MOVE OLC-REPORT-NAME       TO RF-REPORT-ID       EL324
00414                  MOVE ZERO                  TO RF-LINE-NUMBER     EL324
00415                  START ELREPT  KEY NOT LESS RF-CONTROL-PRIMARY    EL324
00416                  PERFORM DTE-REPORT-DELETE THRU DTE-DELETE-EXIT   EL324
00417                  MOVE DTE-CLASIC-COMPANY-CD TO RF-COMPANY-CD      EL324
00418                  MOVE '2'                   TO RF-RECORD-TYPE     EL324
00419                  MOVE OLC-REPORT-NAME       TO RF-REPORT-ID       EL324
00420                  MOVE ZERO                  TO RF-LINE-NUMBER     EL324
00421                  START ELREPT  KEY NOT LESS RF-CONTROL-PRIMARY    EL324
00422                  PERFORM DTE-REPORT-DELETE THRU DTE-DELETE-EXIT   EL324
00423                  MOVE DTE-CLASIC-COMPANY-CD TO RF-COMPANY-CD      EL324
00424                  MOVE '1'                   TO RF-RECORD-TYPE     EL324
00425                  MOVE OLC-REPORT-NAME       TO RF-REPORT-ID       EL324
00426                  MOVE SPACES                TO RF-REPORT-LINE-133.EL324
00427                                                                   EL324
00428      IF DTE-ABEND-CD-1 = '81' AND                                 EL324
00429         DTE-PRT-OPT    = 'S'                                      EL324
00430          MOVE +0302  TO WS-RETURN-CODE                            EL324
00431          GO TO ABEND-PGM.                                         EL324
00432                                                                   EL324
00433      IF DTE-PRT-OPT = 'S' OR 'T'                                  EL324
00434          MOVE X      TO RF-CTL-CHAR-133                           EL324
00435          MOVE P-DATA TO RF-DATA-133                               EL324
00436              IF DTE-ABEND-CD-1 = SPACES                           EL324
00437                  ADD +1 TO DTE-TOT-LINES                          EL324
00438                  MOVE DTE-TOT-LINES TO RF-LINE-NUMBER             EL324
00439                  WRITE REPORT-SAVE-FILE                           EL324
00440                      INVALID KEY                                  EL324
00441                          MOVE '88' TO DTE-ABEND-CD-1              EL324
00442                          CLOSE ELREPT                             EL324
00443                          MOVE SPACE TO REPT-OPEN.                 EL324
00444                                                                   EL324
00445      IF DTE-FICH NOT = SPACE                                      EL324
00446          MOVE X TO P-CTL                                          EL324
00447          WRITE FICH-REC FROM PRT.                                 EL324
00448                                                                   EL324
00449      IF DTE-PRT-OPT = 'P' OR 'B' OR 'T'                           EL324
00450          MOVE X TO P-CTL                                          EL324
00451          WRITE PRT.                                               EL324
00452                                                                   EL324
00453      GO TO DTE-PRINT-EXIT.                                        EL324
00454                                                                   EL324
00455  DTE-REPORT-DELETE.                                               EL324
00456      IF DTE-F-1 NOT = ZERO                                        EL324
00457          MOVE ZERO TO DTE-VSAM-FLAGS                              EL324
00458          GO TO DTE-DELETE-EXIT.                                   EL324
00459                                                                   EL324
00460      READ ELREPT   NEXT RECORD                                    EL324
00461            AT END   GO TO DTE-DELETE-EXIT.                        EL324
00462                                                                   EL324
00463      IF DTE-CLASIC-COMPANY-CD = RF-COMPANY-CD  AND                EL324
00464         OLC-REPORT-NAME       = RF-REPORT-ID                      EL324
00465          DELETE ELREPT RECORD                                     EL324
00466          GO TO DTE-REPORT-DELETE.                                 EL324
00467                                                                   EL324
00468  DTE-DELETE-EXIT.                                                 EL324
00469      EXIT.                                                        EL324
00470                                                                   EL324
00471  DTE-PRINT-EXIT.                                                  EL324
00472      EXIT.                                                        EL324
00473 ******************************************************************EL324
00474                                                                   EL324
00475      EJECT                                                        EL324
00476  OPEN-FILES SECTION.                                              EL324
00477                                                                   EL324
00478  OFS-010.                                                         EL324
00479      OPEN INPUT REPORTS-EXTRACT-FILE                              EL324
00480           OUTPUT PRNTR.                                           EL324
00481                                                                   EL324
00482  OFS-EXIT.                                                        EL324
00483      EXIT.                                                        EL324
00484                                                                   EL324
00485      EJECT                                                        EL324
00486  CLOSE-FILES SECTION.                                             EL324
00487                                                                   EL324
00488  CFS-010. COPY ELCPRTCX.                                          EL324
00489      CLOSE REPORTS-EXTRACT-FILE                                   EL324
00490            PRNTR.                                                 EL324
00491                                                                   EL324
00492  CFS-EXIT.                                                        EL324
00493      EXIT.                                                        EL324
00494                                                                   EL324
00495                                                                   EL324
00496  ABEND-PGM SECTION. COPY ELCABEND.                                EL324
00497                                                                   EL324
