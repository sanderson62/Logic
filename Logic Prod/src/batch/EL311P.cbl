00001  IDENTIFICATION DIVISION.                                         04/18/98
00002                                                                   EL311
00003  PROGRAM-ID.                 EL311 .                                 LV005
00004 *              PROGRAM CONVERTED BY                               EL311
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL311
00006 *              CONVERSION DATE 02/13/96 08:07:10.                 EL311
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          EL311
00008 *                            VMOD=2.010.                          EL311
00009                                                                   EL311
00010 *AUTHOR.     LOGIC INC.                                           EL311
00011 *            DALLAS, TEXAS.                                       EL311
00012                                                                   EL311
00013 *DATE-COMPILED.                                                   EL311
00014                                                                   EL311
00015 *SECURITY.   *****************************************************EL311
00016 *            *                                                   *EL311
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *EL311
00018 *            *                                                   *EL311
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL311
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL311
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *EL311
00022 *            *                                                   *EL311
00023 *            *****************************************************EL311
00024                                                                   EL311
00025 *REMARKS.                                                         EL311
00026 *       GENERAL FUNCTION IS TO PRODUCE THE LETTERS OFFLINE.       EL311
00027 *                                                                 EL311
00028 *       DESCRIPTION.                                              EL311
00029 *         PRINT OR REPRINT LETTERS IN A BATCH MODE.               EL311
00030 *                                                                 EL311
00031 *         THIS PROGRAM WILL:                                      EL311
00032 *           1. PRINT ALL LETTERS THAT ARE READY TO BE PRINTED     EL311
00033 *                       -- OR --                                  EL311
00034 *           2. REPRINT LETTERS BY DATE IN THE CONTROL CARD        EL311
00035 *                                                                 EL311
00036 *       NOTE: FOLLOWING FILES MUST BE CLOSED PRIOR TO RUNNING :   EL311
00037 *                    1. ELARCH                                    EL311
00038 *                    2. ELTRLR                                    EL311
00039 *                    3. ELCNTL                                    EL311
00040 *                                                                 EL311
00041 *       INPUT FILES  -           LETTER ARCHIVE                   EL311
00042 *                                DATE CARD FILE                   EL311
00043 *                                ACTIVITY TRAILERS                EL311
00044 *                                CONTROL FILE                     EL311
00045 *                                CONTROL FILE (CARD INPUT)        EL311
00046 *                                                                 EL311
00047 *       OUTPUT FILES -           LETTER ARCHIVE                   EL311
00048 *                                ACTIVITY TRAILERS                EL311
00049                                                                   EL311
00050      EJECT                                                        EL311
00051                                                                   EL311
00052  ENVIRONMENT DIVISION.                                            EL311
00053  INPUT-OUTPUT SECTION.                                            EL311
00054  FILE-CONTROL.                                                    EL311
00055                                                                   EL311
00056      SELECT CONTROL-CARD   ASSIGN TO SYS006-UR-2540R-S-SYS006.    EL311
00057                                                                   EL311
00058      SELECT PRNTR          ASSIGN TO SYS008-UR-1403-S-SYS008.     EL311
00059                                                                   EL311
00060      SELECT DISK-DATE      ASSIGN TO SYS019-FBA1-S-SYS019.        EL311
00061                                                                   EL311
00062      SELECT ELARCH         ASSIGN TO SYS018-FBA1-ELARCH           EL311
00063                               ORGANIZATION INDEXED                EL311
00064                               ACCESS       DYNAMIC                EL311
00065                               RECORD KEY   LA-CONTROL-PRIMARY     EL311
00066                               FILE STATUS  ELARCH-FILE-STATUS.    EL311
00067                                                                   EL311
00068      SELECT ELTRLR          ASSIGN TO SYS018-FBA1-ELTRLR          EL311
00069                               ORGANIZATION INDEXED                EL311
00070                               ACCESS       DYNAMIC                EL311
00071                               RECORD KEY   AT-CONTROL-PRIMARY     EL311
00072                               FILE STATUS  ELTRLR-FILE-STATUS.    EL311
00073                                                                   EL311
00074      SELECT ELCNTL          ASSIGN TO SYS020-FBA1-ELCNTL          EL311
00075                               ORGANIZATION INDEXED                EL311
00076                               ACCESS       DYNAMIC                EL311
00077                               RECORD KEY   CF-CONTROL-PRIMARY     EL311
00078                               FILE STATUS  ELCNTL-FILE-STATUS.    EL311
00079                                                                   EL311
00080      EJECT                                                        EL311
00081  DATA DIVISION.                                                   EL311
00082  FILE SECTION.                                                    EL311
00083  FD  PRNTR
111703     RECORDING MODE F
111703     LABEL RECORDS OMITTED
111703     BLOCK CONTAINS 0 RECORDS
111703     RECORD CONTAINS 80 CHARACTERS.
111703 01  PRT.
111703     12  P-CTL               PIC  X.
111703     12  P-DATA              PIC  X(79).
00084                                                                   EL311
00085  FD  DISK-DATE                   COPY ELCDTEFD.                   EL311
00086      EJECT                                                        EL311
00087  FD  ELARCH.                                                      EL311
00088                                  COPY ELCARCH.                    EL311
00089      EJECT                                                        EL311
00090  FD  ELTRLR.                                                      EL311
00091                                  COPY ELCTRLR.                    EL311
00092      EJECT                                                        EL311
00093  FD  ELCNTL.                                                      EL311
00094                                  COPY ELCCNTL.                    EL311
00095                                                                   EL311
00096  FD  CONTROL-CARD                                                 EL311
00097      BLOCK CONTAINS 0 RECORDS
00098      RECORDING MODE IS F.                                         EL311
00099  01  CONTROL-RECORD.                                              EL311
00100      05 CX-ID                         PIC XX.                     EL311
00101      05 CX-GROUP                      PIC X(6).                   EL311
00102      05 CX-N-GROUP REDEFINES CX-GROUP PIC 9(6).                   EL311
00103      05 CX-COMMENTS                   PIC X(69).                  EL311
00104      05 CX-CLINT-ID                   PIC XXX.                    EL311
00105                                                                   EL311
00106      EJECT                                                        EL311
00107  WORKING-STORAGE SECTION.                                         EL311
00108  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      EL311
00109  77  FILLER  PIC X(32) VALUE '********************************'.  EL311
00110  77  FILLER  PIC X(32) VALUE '      EL311 WORKING-STORAGE     '.  EL311
00111  77  FILLER  PIC X(32) VALUE '******** VMOD=2.010 ************'.  EL311
00112                                                                   EL311
00113  77  WS-ELARCH-EOF-SW            PIC X    VALUE SPACE.            EL311
00114      88  END-OF-ELARCH-FILE               VALUE 'E'.              EL311
00115                                                                   EL311
00116  77  X                           PIC X     VALUE SPACE.           EL311
00117  77  PGM-SUB                     PIC S9(4) VALUE +311   COMP.     EL311
00118  77  ABEND-CODE                  PIC X(4)  VALUE SPACE.           EL311
00119  77  ABEND-OPTION                PIC X     VALUE SPACE.           EL311
00120      EJECT                                                        EL311
00121                                                                   EL311
00122  01  WORK-AREAS.                                                  EL311
00123      12  ELARCH-FILE-STATUS      PIC XX  VALUE ZEROS.             EL311
00124      12  ELTRLR-FILE-STATUS      PIC XX  VALUE ZEROS.             EL311
00125      12  ELCNTL-FILE-STATUS      PIC XX  VALUE ZEROS.             EL311
00126                                                                   EL311
00127      12  WS-SKIP-CONTROL         PIC XX  VALUE ZEROS.             EL311
00128      12  WS-SKIP REDEFINES                                        EL311
00129          WS-SKIP-CONTROL         PIC 99.                          EL311
00130                                                                   EL311
00131      12  WS-NO-OF-COPIES         PIC S9 VALUE ZEROS.              EL311
00132                                                                   EL311
00133      12  END-OF-PROCESS-SWT      PIC X VALUE 'N'.                 EL311
00134          88 END-OF-PROCESS             VALUE 'Y'.                 EL311
00135          88 NOT-END-OF-PROCESS         VALUE 'N'.                 EL311
00136                                                                   EL311
00137      12  WS-BIN-DATE             PIC X(02)   VALUE LOW-VALUES.    EL311
00138      12  WS-CURRENT-BIN-DT       PIC X(02)   VALUE LOW-VALUES.    EL311
00139      12  WS-RESET-DATE           PIC X(02)   VALUE LOW-VALUES.    EL311
00140                                                                   EL311
00141      12  WS-RETURN-CODE          PIC S9(3)   VALUE ZEROS.         EL311
00142      12  WS-ABEND-MESSAGE        PIC X(80)   VALUE SPACES.        EL311
00143      12  WS-ABEND-FILE-STATUS    PIC XX      VALUE ZEROS.         EL311
00144                                                                   EL311
00145      12  WS-ZERO                 PIC S9      VALUE ZERO.          EL311
00146      12  WS-LA-CONTROL-PRIMARY   PIC X(8)    VALUE SPACES.        EL311
00147      12  W-ENGLISH-POUND-SIGN    PIC S9(04) COMP VALUE +239.      EL311
00148      12  FILLER REDEFINES W-ENGLISH-POUND-SIGN.                   EL311
00149          16  FILLER              PIC  X(01).                      EL311
00150          16  W-ENGLISH-POUND     PIC  X(01).                      EL311
00151                                                                   EL311
00152      EJECT                                                        EL311
00153                                  COPY ELCDATE.                       CL**5
00154                                                                   EL311
00155      EJECT                                                        EL311
00156                                  COPY ELC176W1.                   EL311
00157                                                                   EL311
00158                                  COPY ELCDTECX.                      CL**2
00159                                                                      CL**3
00160                                  COPY ELCDTEVR.                      CL**3
00161      EJECT                                                        EL311
00162  PROCEDURE DIVISION.                                              EL311
00163  0010-LOAD-DATE-CARD SECTION.                                     EL311
00164                                 COPY ELCDTERX.                       CL**2
00165                                                                   EL311
00166  0020-SETUP SECTION.                                              EL311
00167                                                                   EL311
00168      PERFORM 9000-INITIALIZE.                                     EL311
00169                                                                   EL311
00170      MOVE SPACES                 TO  CF-CONTROL-PRIMARY           EL311
00171      MOVE DTE-CLIENT             TO  CF-COMPANY-ID.               EL311
00172      MOVE '1'                    TO  CF-RECORD-TYPE.              EL311
00173      MOVE +0                     TO  CF-SEQUENCE-NO.              EL311
00174                                                                   EL311
00175      READ ELCNTL.                                                 EL311
00176                                                                   EL311
00177      IF ELCNTL-FILE-STATUS IS NOT EQUAL TO '00'                   EL311
00178          MOVE 'ERROR OCCURED READ - ELCNTL'                       EL311
00179                                  TO  WS-ABEND-MESSAGE             EL311
00180          MOVE ELCNTL-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL311
00181          GO TO ABEND-PGM.                                         EL311
00182                                                                   EL311
00183      IF CF-RECORD-TYPE IS NOT EQUAL TO '1'                        EL311
00184          MOVE 'NO COMPANY RECORD FOUND'                           EL311
00185                                  TO  WS-ABEND-MESSAGE             EL311
00186          GO TO ABEND-PGM.                                         EL311
00187                                                                   EL311
00188  0020-EXIT.                                                       EL311
00189       EXIT.                                                       EL311
00190      EJECT                                                        EL311
00191  0100-MAIN-LOGIC SECTION.                                         EL311
00192                                                                   EL311
00193      PERFORM 1000-INPUT-PROCEDURE.                                EL311
00194                                                                   EL311
00195      PERFORM 1100-OUTPUT-PROCEDURE                                EL311
00196              UNTIL END-OF-PROCESS.                                EL311
00197                                                                   EL311
00198      PERFORM 9100-CLOSE-FILES.                                    EL311
00199                                                                   EL311
00200      GO TO 9999-PROGRAM-END.                                      EL311
00201                                                                   EL311
00202  0100-EXIT.                                                       EL311
00203       EXIT.                                                       EL311
00204      EJECT                                                        EL311
00205  1000-INPUT-PROCEDURE SECTION.                                    EL311
00206      READ CONTROL-CARD AT END                                     EL311
00207           MOVE SPACES   TO CONTROL-RECORD                         EL311
00208           MOVE 'LC'     TO CX-ID                                  EL311
00209           MOVE 'ALL'    TO CX-GROUP                               EL311
00210           GO TO 1000-EXIT.                                        EL311
00211                                                                   EL311
00212      IF CX-GROUP = 'ALL'                                          EL311
00213         GO TO 1000-EXIT.                                          EL311
00214                                                                   EL311
00215      IF CX-GROUP NOT NUMERIC                                      EL311
00216          MOVE 'Y'       TO END-OF-PROCESS-SWT                     EL311
00217          DISPLAY ' EL311 DATE INVALID IN CONTROL CARD '           EL311
00218          GO TO 1000-EXIT.                                         EL311
00219                                                                   EL311
00220      MOVE CX-N-GROUP   TO DC-GREG-DATE-1-MDY.                     EL311
00221      MOVE '4'          TO DC-OPTION-CODE.                         EL311
00222      PERFORM 8500-DATE-CONVERSION.                                EL311
00223      IF DATE-CONVERSION-ERROR                                     EL311
00224          MOVE 'Y'       TO END-OF-PROCESS-SWT                     EL311
00225          DISPLAY ' EL311 DATE INVALID IN CONTROL CARD'            EL311
00226          GO TO 1000-EXIT.                                         EL311
00227                                                                   EL311
00228      MOVE DC-BIN-DATE-1 TO WS-BIN-DATE.                           EL311
00229                                                                   EL311
00230  1000-EXIT.                                                       EL311
00231       EXIT.                                                       EL311
00232      EJECT                                                        EL311
00233  1100-OUTPUT-PROCEDURE SECTION.                                   EL311
00234                                                                   EL311
00235      IF CX-GROUP = 'ALL'                                          EL311
00236          PERFORM 1200-CONTROL-ALL-BUILD                           EL311
00237      ELSE                                                         EL311
00238          PERFORM 1300-CONTROL-BY-DATE.                            EL311
00239                                                                   EL311
00240  1100-EXIT.                                                       EL311
00241       EXIT.                                                       EL311
00242                                                                   EL311
00243      EJECT                                                        EL311
00244  1200-CONTROL-ALL-BUILD SECTION.                                  EL311
00245                                                                   EL311
00246      PERFORM 1350-START-ARCHIVE.                                  EL311
00247                                                                   EL311
00248  1200-CAB-05.                                                     EL311
00249      PERFORM 1400-READ-ARCHIVE.                                   EL311
00250                                                                   EL311
00251  1200-CAB-10.                                                     EL311
00252      IF END-OF-PROCESS                                            EL311
00253          GO TO 1200-EXIT.                                         EL311
00254                                                                   EL311
00255      IF LA-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD                 EL311
00256          MOVE 'Y'            TO END-OF-PROCESS-SWT                EL311
00257          GO TO 1200-EXIT.                                         EL311
00258                                                                   EL311
00259      IF LA-HEADER-DATA                                            EL311
00260          NEXT SENTENCE                                            EL311
00261      ELSE                                                         EL311
00262          GO TO 1200-CAB-05.                                       EL311
00263                                                                   EL311
00264      MOVE LA-COMPANY-CD      TO AT-COMPANY-CD.                    EL311
00265      MOVE LA-CARRIER         TO AT-CARRIER.                       EL311
00266      MOVE LA-CLAIM-NO        TO AT-CLAIM-NO.                      EL311
00267      MOVE LA-CERT-NO         TO AT-CERT-NO.                       EL311
00268      MOVE LA-CORR-TRLR-SEQ   TO AT-SEQUENCE-NO.                   EL311
00269                                                                   EL311
00270      READ ELTRLR RECORD INVALID KEY                               EL311
00271          GO TO 1200-CAB-05.                                       EL311
00272                                                                   EL311
00273      IF AT-LETTER-ANSWERED-DT IS NOT EQUAL TO LOW-VALUES          EL311
00274          GO TO 1200-CAB-05.                                       EL311
00275                                                                   EL311
00276      IF LA-INITIAL-PRINT-DATE = LOW-VALUE OR                      EL311
00277         CX-ID = 'LR'                                              EL311
00278           MOVE LA-COMPANY-CD      TO AT-COMPANY-CD                EL311
00279           MOVE LA-CARRIER         TO AT-CARRIER                   EL311
00280           MOVE LA-CLAIM-NO        TO AT-CLAIM-NO                  EL311
00281           MOVE LA-CERT-NO         TO AT-CERT-NO                   EL311
00282           MOVE LA-CORR-TRLR-SEQ   TO AT-SEQUENCE-NO               EL311
00283           PERFORM 1450-PRINT-INITIAL                              EL311
00284           PERFORM 1475-MORE-COPIES                                EL311
00285           GO TO 1200-CAB-10.                                      EL311
00286                                                                   EL311
00287      IF LA-RESEND-DATE = LOW-VALUES                               EL311
00288          GO TO 1200-CAB-05.                                       EL311
00289                                                                   EL311
00290      IF LA-RESEND-DATE NOT GREATER WS-CURRENT-BIN-DT AND          EL311
00291          LA-RESEND-PRINT-DATE = LOW-VALUES                        EL311
00292          NEXT SENTENCE                                            EL311
00293      ELSE                                                         EL311
00294          GO TO 1200-CAB-05.                                       EL311
00295                                                                   EL311
00296      MOVE LA-NO-OF-COPIES        TO  WS-NO-OF-COPIES.             EL311
00297                                                                   EL311
00298      MOVE LA-CONTROL-PRIMARY     TO  WS-LA-CONTROL-PRIMARY.       EL311
00299                                                                   EL311
00300      IF AT-LETTER-ANSWERED-DT = LOW-VALUES    AND                 EL311
00301         AT-AUTO-RE-SEND-DT    = LA-RESEND-DATE                    EL311
00302          NEXT SENTENCE                                            EL311
00303        ELSE                                                       EL311
00304          GO TO 1200-CAB-05.                                       EL311
00305                                                                   EL311
00306      IF LA-HEADER-DATA                                            EL311
00307          MOVE SPACES             TO  PRT                          EL311
00308          WRITE PRT AFTER ADVANCING PAGE.                          EL311
00309                                                                   EL311
00310      MOVE WS-CURRENT-BIN-DT      TO  AT-RESEND-PRINT-DATE         EL311
00311                                      LA-RESEND-PRINT-DATE         EL311
00312                                      LA-1ST-RESEND-PRINT-DT.      EL311
00313                                                                   EL311
00314      IF (DTE-CLIENT IS EQUAL TO 'AIG' OR 'AUK')                   EL311
00315          NEXT SENTENCE                                            EL311
00316      ELSE                                                         EL311
00317          MOVE LOW-VALUES         TO  AT-AUTO-RE-SEND-DT           EL311
00318                                      LA-RESEND-DATE               EL311
00319          GO TO 1200-CAB-15.                                       EL311
00320                                                                   EL311
00321      MOVE LA-CREATION-DT         TO  DC-BIN-DATE-1.               EL311
00322      MOVE LA-RESEND-DATE         TO  DC-BIN-DATE-2.               EL311
00323      MOVE '1'                    TO  DC-OPTION-CODE.              EL311
00324      MOVE +0                     TO  DC-ELAPSED-MONTHS            EL311
00325                                      DC-ELAPSED-DAYS.             EL311
00326      PERFORM 8500-DATE-CONVERSION.                                EL311
00327                                                                   EL311
00328      IF DC-ELAPSED-DAYS IS NOT EQUAL TO +30                       EL311
00329          MOVE LOW-VALUES         TO  AT-AUTO-RE-SEND-DT           EL311
00330                                      LA-RESEND-DATE               EL311
00331          GO TO 1200-CAB-15.                                       EL311
00332                                                                   EL311
00333      MOVE WS-CURRENT-BIN-DT      TO  DC-BIN-DATE-1.               EL311
00334      MOVE '6'                    TO  DC-OPTION-CODE.              EL311
00335      MOVE +0                     TO  DC-ELAPSED-MONTHS.           EL311
00336      MOVE +30                    TO  DC-ELAPSED-DAYS.             EL311
00337      PERFORM 8500-DATE-CONVERSION.                                EL311
00338      IF NO-CONVERSION-ERROR                                       EL311
00339          MOVE DC-BIN-DATE-2      TO  LA-RESEND-DATE               EL311
00340                                      AT-AUTO-RE-SEND-DT           EL311
00341          MOVE LOW-VALUES         TO  LA-RESEND-PRINT-DATE         EL311
00342                                      AT-RESEND-PRINT-DATE.        EL311
00343                                                                   EL311
00344  1200-CAB-15.                                                     EL311
00345                                                                   EL311
00346      REWRITE ACTIVITY-TRAILERS                                    EL311
00347           INVALID KEY                                             EL311
00348              DISPLAY ' EL311 REWRITE ERROR - ELTRLR'              EL311
00349              MOVE 'Y'       TO END-OF-PROCESS-SWT                 EL311
00350              GO TO ABEND-PGM.                                     EL311
00351                                                                   EL311
00352      REWRITE LETTER-ARCHIVE                                       EL311
00353           INVALID KEY                                             EL311
00354              DISPLAY ' EL311 REWRITE ERROR - ELARCH'              EL311
00355              MOVE 'Y'        TO END-OF-PROCESS-SWT                EL311
00356              GO TO ABEND-PGM.                                     EL311
00357                                                                   EL311
00358  1200-CAB-30.                                                     EL311
00359      PERFORM 1400-READ-ARCHIVE.                                   EL311
00360                                                                   EL311
00361      IF END-OF-PROCESS                                            EL311
00362          GO TO 1200-EXIT.                                         EL311
00363                                                                   EL311
00364      IF LA-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD                 EL311
00365          GO TO 1200-CAB-10.                                       EL311
00366                                                                   EL311
00367      IF LA-HEADER-DATA                                            EL311
00368          PERFORM 1475-MORE-COPIES                                 EL311
00369          GO TO 1200-CAB-10.                                       EL311
00370                                                                   EL311
00371      IF LA-TEXT-DATA                                              EL311
00372          NEXT SENTENCE                                            EL311
00373      ELSE                                                         EL311
00374          GO TO 1200-CAB-30.                                       EL311
00375                                                                   EL311
00376      PERFORM 1500-WRITE-LETTER.                                   EL311
00377                                                                   EL311
00378      GO TO 1200-CAB-30.                                           EL311
00379                                                                   EL311
00380  1200-EXIT.                                                       EL311
00381       EXIT.                                                       EL311
00382      EJECT                                                        EL311
00383                                                                   EL311
00384  1300-CONTROL-BY-DATE SECTION.                                    EL311
00385                                                                   EL311
00386      PERFORM 1350-START-ARCHIVE.                                  EL311
00387                                                                   EL311
00388  1300-CBD-05.                                                     EL311
00389      PERFORM 1400-READ-ARCHIVE.                                   EL311
00390                                                                   EL311
00391  1300-CBD-10.                                                     EL311
00392      IF END-OF-PROCESS                                            EL311
00393          GO TO 1300-EXIT.                                         EL311
00394                                                                   EL311
00395      IF LA-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD                 EL311
00396          MOVE 'Y'                TO  END-OF-PROCESS-SWT           EL311
00397          GO TO 1300-EXIT.                                         EL311
00398                                                                   EL311
00399      IF LA-HEADER-DATA                                            EL311
00400          NEXT SENTENCE                                            EL311
00401      ELSE                                                         EL311
00402          GO TO 1300-CBD-05.                                       EL311
00403                                                                   EL311
00404      IF WS-BIN-DATE = LA-INITIAL-PRINT-DATE  OR                   EL311
00405                       LA-RESEND-PRINT-DATE   OR                   EL311
00406                       LA-1ST-RESEND-PRINT-DT                      EL311
00407          NEXT SENTENCE                                            EL311
00408      ELSE                                                         EL311
00409          GO TO 1300-CBD-05.                                       EL311
00410                                                                   EL311
00411      MOVE SPACES                 TO PRT.                          EL311
00412      WRITE PRT AFTER ADVANCING PAGE.                              EL311
00413      PERFORM 2450-PRINT.                                          EL311
00414                                                                   EL311
00415      GO TO 1300-CBD-10.                                           EL311
00416                                                                   EL311
00417  1300-EXIT.                                                       EL311
00418       EXIT.                                                       EL311
00419      EJECT                                                        EL311
00420                                                                   EL311
00421  1350-START-ARCHIVE SECTION.                                      EL311
00422                                                                   EL311
00423      MOVE LOW-VALUES             TO  LA-CONTROL-PRIMARY.          EL311
00424      MOVE DTE-CLASIC-COMPANY-CD  TO  LA-COMPANY-CD.               EL311
00425      MOVE '1'                    TO  LA-RECORD-TYPE.              EL311
00426      MOVE CF-STARTING-ARCH-NO    TO  LA-ARCHIVE-NO.               EL311
00427                                                                   EL311
00428      START ELARCH                                                 EL311
00429          KEY NOT LESS LA-CONTROL-PRIMARY.                         EL311
00430                                                                   EL311
00431      IF ELARCH-FILE-STATUS NOT = ZEROS                            EL311
00432          MOVE 'ERROR OCCURED START - ELARCH'                      EL311
00433                                      TO  WS-ABEND-MESSAGE         EL311
00434          MOVE ELARCH-FILE-STATUS     TO  WS-ABEND-FILE-STATUS     EL311
00435          GO TO ABEND-PGM.                                         EL311
00436                                                                   EL311
00437  1350-EXIT.                                                       EL311
00438       EXIT.                                                       EL311
00439                                                                   EL311
00440  1400-READ-ARCHIVE SECTION.                                       EL311
00441                                                                   EL311
00442      READ ELARCH NEXT RECORD AT END                               EL311
00443          MOVE 'Y' TO END-OF-PROCESS-SWT.                          EL311
00444                                                                   EL311
00445  1400-EXIT.                                                       EL311
00446       EXIT.                                                       EL311
00447      EJECT                                                        EL311
00448                                                                   EL311
00449  1450-PRINT-INITIAL SECTION.                                      EL311
00450                                                                   EL311
00451      MOVE LA-NO-OF-COPIES        TO  WS-NO-OF-COPIES.             EL311
00452                                                                   EL311
00453      MOVE LA-CONTROL-PRIMARY     TO  WS-LA-CONTROL-PRIMARY.       EL311
00454                                                                   EL311
00455      IF LA-HEADER-DATA                                            EL311
00456          MOVE SPACES TO PRT                                       EL311
00457          WRITE PRT AFTER ADVANCING PAGE.                          EL311
00458                                                                   EL311
00459      MOVE WS-CURRENT-BIN-DT TO LA-INITIAL-PRINT-DATE              EL311
00460                                AT-INITIAL-PRINT-DATE.             EL311
00461                                                                   EL311
00462      IF CX-ID IS EQUAL TO 'LR'                                    EL311
00463          GO TO 1450-PIS-10.                                       EL311
00464                                                                   EL311
00465      REWRITE LETTER-ARCHIVE                                       EL311
00466           INVALID KEY                                             EL311
00467               DISPLAY ' EL311 REWRITE ERROR - ELARCH'             EL311
00468               MOVE 'Y'           TO  END-OF-PROCESS-SWT           EL311
00469               GO TO ABEND-PGM.                                    EL311
00470                                                                   EL311
00471      REWRITE ACTIVITY-TRAILERS                                    EL311
00472           INVALID KEY                                             EL311
00473               DISPLAY ' EL311 REWRITE ERROR - ELTRLR'             EL311
00474               MOVE 'Y'           TO  END-OF-PROCESS-SWT           EL311
00475               GO TO ABEND-PGM.                                    EL311
00476                                                                   EL311
00477  1450-PIS-10.                                                     EL311
00478      PERFORM 1400-READ-ARCHIVE.                                   EL311
00479                                                                   EL311
00480      IF END-OF-PROCESS                                            EL311
00481          GO TO 1450-EXIT.                                         EL311
00482                                                                   EL311
00483      IF LA-HEADER-DATA                                            EL311
00484          GO TO 1450-EXIT.                                         EL311
00485                                                                   EL311
00486      IF LA-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD                 EL311
00487          GO TO 1450-EXIT.                                         EL311
00488                                                                   EL311
00489      IF LA-TEXT-DATA                                              EL311
00490          PERFORM 1500-WRITE-LETTER.                               EL311
00491                                                                   EL311
00492      GO TO 1450-PIS-10.                                           EL311
00493                                                                   EL311
00494  1450-EXIT.                                                       EL311
00495       EXIT.                                                       EL311
00496      EJECT                                                        EL311
00497                                                                   EL311
00498  1475-MORE-COPIES SECTION.                                        EL311
00499       MOVE 'N'                  TO END-OF-PROCESS-SWT.            EL311
00500                                                                   EL311
00501      IF WS-NO-OF-COPIES = ZERO OR +1                              EL311
00502          GO TO 1475-MORE-COPIES-DONE.                             EL311
00503                                                                   EL311
00504      COMPUTE WS-NO-OF-COPIES = (WS-NO-OF-COPIES - 1).             EL311
00505                                                                   EL311
00506      MOVE WS-LA-CONTROL-PRIMARY TO LA-CONTROL-PRIMARY.            EL311
00507      MOVE '1'                   TO LA-RECORD-TYPE.                EL311
00508      MOVE ZEROS                 TO LA-LINE-SEQ-NO.                EL311
00509                                                                   EL311
00510      START ELARCH KEY = LA-CONTROL-PRIMARY                        EL311
00511         INVALID KEY                                               EL311
00512            MOVE 'Y' TO END-OF-PROCESS-SWT                         EL311
00513            DISPLAY ' EL311 START ERROR - ELARCH'                  EL311
00514            GO TO 1475-MORE-COPIES-DONE.                           EL311
00515                                                                   EL311
00516      PERFORM 1400-READ-ARCHIVE.                                   EL311
00517                                                                   EL311
00518      IF END-OF-PROCESS                                            EL311
00519          GO TO 1475-MORE-COPIES.                                  EL311
00520                                                                   EL311
00521      IF LA-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD                 EL311
00522          GO TO 1475-MORE-COPIES.                                  EL311
00523                                                                   EL311
00524      IF LA-HEADER-DATA                                            EL311
00525          MOVE SPACES TO PRT                                       EL311
00526          WRITE PRT AFTER ADVANCING PAGE.                          EL311
00527                                                                   EL311
00528  1475-MCS-10.                                                     EL311
00529      PERFORM 1400-READ-ARCHIVE.                                   EL311
00530                                                                   EL311
00531      IF END-OF-PROCESS                                            EL311
00532          GO TO 1475-MORE-COPIES.                                  EL311
00533                                                                   EL311
00534      IF LA-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD                 EL311
00535          GO TO 1475-MORE-COPIES.                                  EL311
00536                                                                   EL311
00537      IF LA-HEADER-DATA                                            EL311
00538          GO TO 1475-MORE-COPIES.                                  EL311
00539                                                                   EL311
00540      IF LA-TEXT-DATA                                              EL311
00541          PERFORM 1500-WRITE-LETTER.                               EL311
00542                                                                   EL311
00543      GO TO 1475-MCS-10.                                           EL311
00544                                                                   EL311
00545  1475-MORE-COPIES-DONE.                                           EL311
00546      IF ELARCH-FILE-STATUS NOT = '00'                             EL311
00547          MOVE 'Y'            TO END-OF-PROCESS-SWT.               EL311
00548                                                                   EL311
00549  1475-EXIT.                                                       EL311
00550       EXIT.                                                       EL311
00551      EJECT                                                        EL311
00552                                                                   EL311
00553  1500-WRITE-LETTER SECTION.                                       EL311
00554                                                                   EL311
00555      IF  DTE-CLIENT IS EQUAL TO 'AUK'                             EL311
00556          INSPECT LA-TEXT-LINE                                     EL311
00557              REPLACING ALL '$' BY W-ENGLISH-POUND.                EL311
00558                                                                   EL311
00559      MOVE SPACES             TO PRT.                              EL311
00560      MOVE LA-TEXT-LINE       TO P-DATA.                           EL311
00561      WRITE PRT AFTER ADVANCING 1 LINES.                           EL311
00562                                                                   EL311
00563      IF NO-LINES-SKIPPED                                          EL311
00564          GO TO 1500-EXIT.                                         EL311
00565                                                                   EL311
00566      IF LA-SKIP-CONTROL = '99'                                    EL311
00567          MOVE SPACES          TO PRT                              EL311
00568          WRITE PRT AFTER ADVANCING PAGE                           EL311
00569          GO TO 1500-EXIT.                                         EL311
00570                                                                   EL311
00571      IF LA-SKIP-CONTROL GREATER '00' AND LESS '99'                EL311
00572          MOVE SPACES          TO PRT                              EL311
00573          MOVE LA-SKIP-CONTROL TO WS-SKIP-CONTROL                  EL311
00574          PERFORM 1525-WRITE-BLANK-LINES WS-SKIP TIMES.            EL311
00575                                                                   EL311
00576  1500-EXIT.                                                       EL311
00577       EXIT.                                                       EL311
00578      EJECT                                                        EL311
00579                                                                   EL311
00580  1525-WRITE-BLANK-LINES SECTION.                                  EL311
00581      MOVE SPACES              TO PRT.                             EL311
00582      WRITE PRT AFTER ADVANCING 1 LINES.                           EL311
00583                                                                   EL311
00584  1525-EXIT.                                                       EL311
00585       EXIT.                                                       EL311
00586      EJECT                                                        EL311
00587                                                                   EL311
00588  2450-PRINT SECTION.                                              EL311
00589                                                                   EL311
00590      PERFORM 1400-READ-ARCHIVE.                                   EL311
00591                                                                   EL311
00592      IF END-OF-PROCESS                                            EL311
00593          GO TO 2450-EXIT.                                         EL311
00594                                                                   EL311
00595      IF LA-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD                 EL311
00596          GO TO 2450-EXIT.                                         EL311
00597                                                                   EL311
00598      IF LA-HEADER-DATA                                            EL311
00599          GO TO 2450-EXIT.                                         EL311
00600                                                                   EL311
00601      IF LA-TEXT-DATA                                              EL311
00602          NEXT SENTENCE                                            EL311
00603       ELSE                                                        EL311
00604          GO TO 2450-PRINT.                                        EL311
00605                                                                   EL311
00606      PERFORM 1500-WRITE-LETTER.                                   EL311
00607                                                                   EL311
00608      GO TO 2450-PRINT.                                            EL311
00609                                                                   EL311
00610  2450-EXIT.                                                       EL311
00611       EXIT.                                                       EL311
00612      EJECT                                                        EL311
00613                                                                   EL311
00614  8500-DATE-CONVERSION SECTION. COPY ELCDCS.                       EL311
00615                                                                   EL311
00616  9000-INITIALIZE SECTION.                                         EL311
00617      OPEN I-O    ELTRLR                                           EL311
00618                  ELARCH                                           EL311
00619           INPUT  CONTROL-CARD                                     EL311
00620                  ELCNTL                                           EL311
00621           OUTPUT PRNTR.                                           EL311
00622                                                                   EL311
00623      IF ELTRLR-FILE-STATUS   = '00' OR '97'                       EL311
00624          NEXT SENTENCE                                            EL311
00625        ELSE                                                       EL311
00626          DISPLAY  '* ERROR ON OPEN - ELTRLR ' AT-CONTROL-PRIMARY  EL311
00627          GO TO ABEND-PGM.                                         EL311
00628                                                                   EL311
00629      IF ELARCH-FILE-STATUS   = '00' OR '97'                       EL311
00630          NEXT SENTENCE                                            EL311
00631        ELSE                                                       EL311
00632          DISPLAY  '* ERROR ON OPEN - ELARCH ' LA-CONTROL-PRIMARY  EL311
00633          GO TO ABEND-PGM.                                         EL311
00634                                                                   EL311
00635      IF (ELCNTL-FILE-STATUS IS EQUAL TO '00' OR '97')             EL311
00636          NEXT SENTENCE                                            EL311
00637      ELSE                                                         EL311
00638          MOVE 'ERROR OCCURED OPEN - ELCNTL'                       EL311
00639                                  TO  WS-ABEND-MESSAGE             EL311
00640          MOVE ELCNTL-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL311
00641          GO TO ABEND-PGM.                                         EL311
00642                                                                   EL311
CIDMOD     MOVE WS-CURRENT-DATE        TO  DC-GREG-DATE-1-EDIT.         00005921
CIDMOD     MOVE '2'                    TO  DC-OPTION-CODE.              00005922
CIDMOD     DISPLAY '* * * * * * * * * * * * * * * * * * * * * '.        00005923
CIDMOD     DISPLAY 'CURRENT DATE USED FOR RUN IS - - ' WS-CURRENT-DATE. 00005924
CIDMOD     DISPLAY '* * * * * * * * * * * * * * * * * * * * * '.        00005925
00610                                                                   00005926
CIDMOD*    MOVE BIN-RUN-DATE           TO  WS-CURRENT-BIN-DT.           EL311
00644                                                                   EL311
CIDMOD     PERFORM 8500-DATE-CONVERSION.                                00005932
CIDMOC     IF NO-CONVERSION-ERROR                                       00005933
CIDMOD         MOVE DC-BIN-DATE-1      TO  WS-CURRENT-BIN-DT            00005934
CIDMOD     ELSE                                                         00005935
CSOMOD         DISPLAY '* * * * * * * * * * * * * * * * * * * * * '     00005936
CSOMOD         DISPLAY 'DATE CONVERSION ERROR - LOW-VALUES USED '       00005937
CSOMOD         DISPLAY '* * * * * * * * * * * * * * * * * * * * * '     00005938
00619          MOVE LOW-VALUES         TO  WS-CURRENT-BIN-DT.           00005939
00620                                                                   00005940
00645 *    MOVE SPACES          TO PRT.                                 EL311
00646 *    WRITE PRT AFTER ADVANCING PAGE.                              EL311
00647                                                                   EL311
00648  9000-EXIT.                                                       EL311
00649       EXIT.                                                       EL311
00650      EJECT                                                        EL311
00651  9100-CLOSE-FILES SECTION.                                        EL311
00652      CLOSE ELARCH                                                 EL311
00653            ELTRLR                                                 EL311
00654            ELCNTL                                                 EL311
00655            PRNTR                                                  EL311
00656            CONTROL-CARD.                                          EL311
00657                                                                   EL311
00658  9100-EXIT.                                                       EL311
00659       EXIT.                                                       EL311
00660                                                                   EL311
00661  9999-PROGRAM-END SECTION.                                        EL311
00662      GOBACK.                                                      EL311
00663                                                                   EL311
00664  ABEND-PGM SECTION.              COPY ELCABEND.                      CL**2
