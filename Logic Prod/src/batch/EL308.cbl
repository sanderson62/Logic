00001  IDENTIFICATION DIVISION.                                         04/06/98
00002                                                                   EL308
00003  PROGRAM-ID.                 EL308 .                                 LV003
00004 *              PROGRAM CONVERTED BY                               EL308
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL308
00006 *              CONVERSION DATE 02/13/96 07:47:15.                 EL308
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            EL308
00008 *                            VMOD=2.006.                          EL308
00009                                                                   EL308
00010 *AUTHOR.     LOGIC, INC.                                          EL308
00011 *            DALLAS, TEXAS.                                       EL308
00012                                                                   EL308
00013 *DATE-COMPILED.                                                   EL308
00014                                                                   EL308
00015 *SECURITY.   *****************************************************EL308
00016 *            *                                                   *EL308
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *EL308
00018 *            *                                                   *EL308
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL308
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL308
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *EL308
00022 *            *                                                   *EL308
00023 *            *****************************************************EL308
00024                                                                   EL308
00025 *REMARKS.                                                         EL308
00026 *        THIS PROGRAM IS RUN AT MONTH END TO DELETE ANY LETTER    EL308
00027 *    WHOSE PRINT DATE IS 1 YEAR OR OLDER.                         EL308
00028                                                                   EL308
00029 *    INPUT FILES  - ELCNTL - CONTROL FILE                         EL308
00030 *                   LETTERS - SEQUENTIAL COPY OF LETTER FILE         CL**2
00031 *    OUTPUT FILES - PRINTED REPORT                                EL308
00032 *                   OUTFILE - NEW SEQUENTIAL COPY OF LETTER FILE     CL**2
00033 *                   ELTRLR  - ACTIVITY TRAILERS                   EL308
00034                                                                   EL308
00035                                                                   EL308
00036      EJECT                                                        EL308
00037  ENVIRONMENT DIVISION.                                            EL308
00038  CONFIGURATION SECTION.                                           EL308
00039  SPECIAL-NAMES.                                                   EL308
00040      C02 IS LCP-CH2                                               EL308
00041      C03 IS LCP-CH3                                               EL308
00042      C04 IS LCP-CH4                                               EL308
00043      C05 IS LCP-CH5                                               EL308
00044      C06 IS LCP-CH6                                               EL308
00045      C07 IS LCP-CH7                                               EL308
00046      C08 IS LCP-CH8                                               EL308
00047      C09 IS LCP-CH9                                               EL308
00048      C10 IS LCP-CH10                                              EL308
00049      C11 IS LCP-CH11                                              EL308
00050      C12 IS LCP-CH12                                              EL308
00051      S01 IS LCP-P01                                               EL308
00052      S02 IS LCP-P02.                                              EL308
00053                                                                   EL308
00054  INPUT-OUTPUT SECTION.                                            EL308
00055                                                                   EL308
00056  FILE-CONTROL.                                                    EL308
00057                                                                   EL308
00058      SELECT PRNTR            ASSIGN TO SYS008-UR-1403-S-SYS008.   EL308
00059                                                                   EL308
00060      SELECT LETTERS          ASSIGN TO SYS011-FBA1-S-SYS011.      EL308
00061                                                                   EL308
00062      SELECT ELCNTL           ASSIGN TO SYS021-FBA1-ELCNTL         EL308
00063                              ORGANIZATION IS INDEXED              EL308
00064                              ACCESS IS DYNAMIC                    EL308
00065                              RECORD KEY IS CF-CONTROL-PRIMARY     EL308
00066                              FILE STATUS IS ELCNTL-FILE-STATUS.   EL308
00067                                                                   EL308
00068      SELECT OUTFILE          ASSIGN TO SYS012-FBA1-S-SYS012.      EL308
00069                                                                   EL308
00070      SELECT ELTRLR           ASSIGN TO SYS022-FBA1-ELTRLR         EL308
00071                              ORGANIZATION IS INDEXED              EL308
00072                              ACCESS IS DYNAMIC                    EL308
00073                              RECORD KEY IS AT-CONTROL-PRIMARY     EL308
00074                              FILE STATUS IS ELTRLR-FILE-STATUS.   EL308
00075      EJECT                                                        EL308
00076  DATA DIVISION.                                                   EL308
00077                                                                   EL308
00078  FILE SECTION.                                                    EL308
00079                                                                   EL308
00080  FD  ELCNTL.                                                      EL308
00081                                  COPY ELCCNTL.                    EL308
00082                                                                   EL308
00083  FD  LETTERS                                                      EL308
00084      BLOCK CONTAINS 0 RECORDS.
00085                                  COPY ELCARCH.                    EL308
00086                                                                   EL308
00087  FD  ELTRLR.                                                      EL308
00088                                  COPY ELCTRLR.                    EL308
00089                                                                   EL308
00090  FD  PRNTR                       COPY ELCPRTFD.                   EL308
00091                                                                   EL308
00092  FD  OUTFILE                                                      EL308
00093      BLOCK CONTAINS 0 RECORDS.
00094  01  OUTFILE-REC          PIC X(90).                              EL308
00095                                                                   EL308
00096  WORKING-STORAGE SECTION.                                         EL308
00097  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      EL308
00098  01  LCP-TIME-OF-DAY-68            PIC 9(6).                      EL308
00099  01  LCP-TIME-OF-DAY-74.                                          EL308
00100      05  LCP-TIME-74               PIC 9(6).                      EL308
00101      05  FILLER                    PIC 9(2).                      EL308
00102  01  LCP-CURRENT-DATE-68.                                         EL308
00103      05  LCP-MONTH                 PIC X(2).                      EL308
00104      05  FILLER                    PIC X VALUE '/'.               EL308
00105      05  LCP-DAY1                  PIC X(2).                      EL308
00106      05  FILLER                    PIC X VALUE '/'.               EL308
00107      05  LCP-YEAR                  PIC X(2).                      EL308
00108  01  LCP-DATE-NEW-74.                                             EL308
00109      05  LCP-YEAR                  PIC X(2).                      EL308
00110      05  LCP-MONTH                 PIC X(2).                      EL308
00111      05  LCP-DAY1                  PIC X(2).                      EL308
00112  77  LCP-ASA                       PIC X.                         EL308
00113                                                                   EL308
00114  77  FILLER  PIC X(32)   VALUE '********************************'.EL308
00115  77  FILLER  PIC X(32)   VALUE '*     EL308  WORKING STORAGE   *'.EL308
00116  77  FILLER  PIC X(32)   VALUE '******** VMOD=2.006 ************'.EL308
00117                                                                   EL308
00118  01  FILLER                          COMP-3.                      EL308
00119      05  WS-LINE-COUNT               PIC S9(3)   VALUE +99.       EL308
00120      05  WS-LINE-COUNT-MAX           PIC S9(3)   VALUE +60.       EL308
00121      05  WS-PAGE                     PIC S9(5)   VALUE ZERO.      EL308
00122      05  WS-REPORT-SW                PIC S9      VALUE +1.        EL308
00123      05  WS-RECORD-COUNT             PIC S9(9)   VALUE ZERO.      EL308
00124      05  WS-RETURN-CODE              PIC S9(3)   VALUE ZERO.      EL308
00125      05  WS-ZERO                     PIC S9      VALUE ZERO.      EL308
00126      05  WS-CURRENT-TIME             PIC S9(7)   VALUE ZERO.      EL308
00127                                                                   EL308
00128      05  WS-WORK                     PIC S9(7)   VALUE ZERO.      EL308
00129      05  WS-CONTROL-TOTAL            PIC S9(9)   VALUE ZERO.      EL308
00130                                                                   EL308
00131      05  WS-LETTERS-READ             PIC S9(9)   VALUE ZERO.      EL308
00132      05  WS-RECORDS-READ             PIC S9(9)   VALUE ZERO.      EL308
00133      05  WS-LETTERS-DELETED          PIC S9(9)   VALUE ZERO.      EL308
00134      05  WS-RECORDS-DELETED          PIC S9(9)   VALUE ZERO.      EL308
00135      05  WS-LETTERS-OUTPUT           PIC S9(9)   VALUE ZERO.      EL308
00136      05  WS-RECORDS-OUTPUT           PIC S9(9)   VALUE ZERO.      EL308
00137                                                                   EL308
00138      05  WS-TOTAL-RECORDS-READ       PIC S9(9)   VALUE ZERO.      EL308
00139      05  WS-TOTAL-RECORDS-DEL        PIC S9(9)   VALUE ZERO.      EL308
00140      05  WS-TOTAL-RECORDS-OUT        PIC S9(9)   VALUE ZERO.      EL308
00141                                                                   EL308
00142      05  WS-LA-ARCHIVE-INPUT         PIC S9(9)   VALUE ZERO.      EL308
00143      05  WS-LA-ARCHIVE-OUTPUT        PIC S9(9)   VALUE ZERO.      EL308
00144      05  WS-LA-HISTORY-DROPPED       PIC S9(9)   VALUE ZERO.      EL308
00145                                                                   EL308
00146      EJECT                                                        EL308
00147  01  FILLER   COMP   SYNC.                                        EL308
00148      05  PGM-SUB                     PIC S9(4)   VALUE +308.      EL308
00149      05  WS-INDEX                    PIC S9(4)   VALUE ZERO.      EL308
00150      05  WS-LENGTH                   REDEFINES                    EL308
00151          WS-INDEX                    PIC S9(4).                   EL308
00152      05  MAX-TABLE-INDX              PIC S9(4)   VALUE +100.      EL308
00153                                                                   EL308
00154  01  FILLER.                                                      EL308
00155      05  WS-LAST-COMPANY-ID      PIC X(3) VALUE LOW-VALUES.       EL308
00156      05  WS-LAST-CLAIM-KEY       PIC X(20) VALUE LOW-VALUES.      EL308
00157      05  WS-LAST-RECORD-ID       PIC XX          VALUE 'AT'.      EL308
00158                                                                   EL308
00159      05  WS-DISPLAY-TIME             PIC 99B99B99.                EL308
00160      05  WS-DISPLAY-COUNT            PIC S9(9)   VALUE ZERO.      EL308
00161      05  WS-COMPARE-CD               PIC S9(4)   COMP VALUE +205. EL308
00162                                                                   EL308
00163      05  FILLER             REDEFINES  WS-COMPARE-CD.             EL308
00164          10  FILLER                  PIC X.                       EL308
00165          10  WS-SELECT-CD            PIC X.                       EL308
00166                                                                   EL308
00167      05  X                           PIC X.                       EL308
00168      05  ABEND-CODE                  PIC X(4).                    EL308
00169      05  ABEND-OPTION                PIC X.                       EL308
00170      05  OLC-REPORT-NAME             PIC X(8) VALUE 'EL308'.      EL308
00171                                                                   EL308
00172      05  WS-SAVE-PRINT-RECORD        PIC X(133)  VALUE SPACES.    EL308
00173                                                                   EL308
00174      05  WS-LAST-COMPANY-CD          PIC X VALUE LOW-VALUES.      EL308
00175                                                                   EL308
00176      05  WS-DISPLAY-AMOUNT           PIC Z,ZZZ,ZZ9.99-.           EL308
00177                                                                   EL308
00178      05  WS-ABEND-MESSAGE            PIC X(80)   VALUE SPACES.    EL308
00179                                                                   EL308
00180      05  WS-ABEND-FILE-STATUS        PIC XX      VALUE ZERO.      EL308
00181                                                                   EL308
00182      05  ELCNTL-FILE-STATUS          PIC XX      VALUE ZERO.      EL308
00183      05  ELARCH-FILE-STATUS          PIC XX      VALUE ZERO.      EL308
00184      05  ELTRLR-FILE-STATUS          PIC X(02)   VALUE ZEROS.     EL308
00185                                                                   EL308
00186      05  WS-FILE-ERROR-MESSAGE.                                   EL308
00187          10  FILLER                  PIC X(24)   VALUE            EL308
00188              'ERROR OCCURED OPENING - '.                          EL308
00189          10  WS-FEM-FILE-NAME        PIC X(8).                    EL308
00190                                                                   EL308
00191      05  WS-CURRENT-DATE             PIC XX  VALUE LOW-VALUES.    EL308
00192      05  WS-RUN-DATE                 PIC XX  VALUE LOW-VALUES.    EL308
00193      05  WS-YEAR-AGO                 PIC XX  VALUE LOW-VALUES.    EL308
00194                                                                   EL308
00195      05  WS-DISPLAY-DATE             PIC X(6).                    EL308
00196      05  WS-DATE-YMD.                                             EL308
00197          10 WS-YY                    PIC XX.                      EL308
00198          10 WS-MM                    PIC XX.                      EL308
00199          10 WS-DD                    PIC XX.                      EL308
00200                                                                   EL308
00201      05  WS-PRINT-DATE-YMD.                                       EL308
00202          10 WS-PRINT-YY                    PIC XX.                EL308
00203          10 FILLER                         PIC X  VALUE '/'.      EL308
00204          10 WS-PRINT-MM                    PIC XX.                EL308
00205          10 FILLER                         PIC X  VALUE '/'.      EL308
00206          10 WS-PRINT-DD                    PIC XX.                EL308
00207                                                                   EL308
00208      05  WS-PURGE-SW                 PIC X   VALUE SPACE.         EL308
00209        88  DO-NOT-PURGE                      VALUE SPACE.         EL308
00210        88  LETTER-SHOULD-BE-PURGED           VALUE 'Y'.           EL308
00211                                                                   EL308
00212      05  WS-COMPANY-ID               PIC XXX.                     EL308
00213      05  WS-COMPANY-CD               PIC X      VALUE LOW-VALUES. EL308
00214                                                                   EL308
00215      05  WS-COMPANY-NAME.                                         EL308
00216          10  WS-CN-CHAR              PIC X                        EL308
00217              OCCURS 30 TIMES         INDEXED BY CN1.              EL308
00218                                                                   EL308
00219      05  WS-COMPANY-NAME2.                                        EL308
00220          10  WS-CN2-CHAR             PIC X                        EL308
00221              OCCURS 30 TIMES         INDEXED BY CN2.              EL308
00222                                                                   EL308
00223      05  WS-INITIALS.                                             EL308
00224          10  WS-INITIAL1             PIC X.                       EL308
00225          10  WS-INITIAL2             PIC X.                       EL308
00226                                                                   EL308
00227      EJECT                                                        EL308
00228      05  WS-COMPANY-ID-TABLE OCCURS 100 TIMES                     EL308
00229                       INDEXED BY TBL.                             EL308
00230          10  WS-TABLED-CD                PIC X.                   EL308
00231          10  WS-TABLED-ID                PIC XXX.                 EL308
00232          10  WS-TABLED-NAME              PIC X(30).               EL308
00233                                                                   EL308
00234  01  WS-HEADING1.                                                 EL308
00235      05  FILLER                      PIC X(49)   VALUE '1'.       EL308
00236      05  WS-H1-TITLE                 PIC X(71)   VALUE            EL308
00237          'MONTHLY LETTER ARCHIVE REVIEW & PURGE'.                 EL308
00238      05  WS-H1-REPORT-NUMBER         PIC X(9) VALUE 'EL308'.      EL308
00239                                                                   EL308
00240  01  WS-HEADING2.                                                 EL308
00241      05  FILLER                      PIC X       VALUE SPACES.    EL308
00242      05  WS-H2-COMPANY-ID            PIC XXX     VALUE 'XXX'.     EL308
00243      05  FILLER                      PIC X(3)    VALUE ' - '.     EL308
00244      05  WS-H2-COMPANY-NAME          PIC X(30)   VALUE SPACES.    EL308
00245      05  FILLER                      PIC X(83)   VALUE SPACES.    EL308
00246      05  FILLER                      PIC X(5)    VALUE 'PAGE'.    EL308
00247      05  WS-H2-PAGE                  PIC ZZ,ZZ9.                  EL308
00248      05  FILLER                      PIC X(2)    VALUE SPACES.    EL308
00249                                                                   EL308
00250  01  WS-HEADING3.                                                 EL308
00251      05  FILLER                      PIC X(58)   VALUE SPACES.    EL308
00252      05  WS-H3-DATE                  PIC X(20)   VALUE SPACES.    EL308
00253      05  FILLER                      PIC X(55)   VALUE SPACES.    EL308
00254                                                                   EL308
00255  01  WS-HEADING4.                                                 EL308
00256      05  FILLER                      PIC X(5)    VALUE SPACES.    EL308
00257      05  FILLER                      PIC X(26)                    EL308
00258                    VALUE 'EFFECTIVE DATE OF PURGE : '.            EL308
00259      05  WS-H4-DATE                  PIC X(8)    VALUE SPACES.    EL308
00260      05  FILLER                      PIC X(55)   VALUE SPACES.    EL308
00261                                                                   EL308
00262  01  WS-DETAIL1.                                                  EL308
00263      05  WS-D1-LINE-CNTL             PIC X.                       EL308
00264      05  WS-D1-DESC                  PIC X(30).                   EL308
00265      05  WS-D1-COUNT                 PIC ZZZ,ZZZ,ZZ9-.            EL308
00266      05  FILLER                      PIC X(88).                   EL308
00267                                                                   EL308
00268                                  COPY ELCDATE.                    EL308
00269                                                                   EL308
00270                                  COPY ELCNWA.                     EL308
00271                                                                   EL308
00272      EJECT                                                        EL308
00273  PROCEDURE DIVISION.                                              EL308
00274                                                                   EL308
00275  0000-MAIN-LOGIC SECTION.                                         EL308
00276                                                                   EL308
00277      PERFORM OPEN-FILES.                                          EL308
00278                                                                   EL308
00279      PERFORM 0100-PROCESS-CONTROL-FILE THRU 0199-EXIT.            EL308
00280                                                                   EL308
00281      CLOSE ELCNTL.                                                EL308
00282                                                                   EL308
00283      IF ELCNTL-FILE-STATUS NOT = ZERO                             EL308
00284          MOVE 'ELCNTL  '         TO  WS-FEM-FILE-NAME             EL308
00285          MOVE 'ERROR OCCURED CLOSING -' TO  WS-FILE-ERROR-MESSAGE EL308
00286          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          EL308
00287          MOVE ELCNTL-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL308
00288          PERFORM ABEND-PGM.                                       EL308
00289                                                                   EL308
00290      MOVE LOW-VALUES             TO WS-COMPANY-ID.                EL308
00291                                                                   EL308
00292      PERFORM 0200-PROCESS-LETTER-FILE  THRU 0299-EXIT.            EL308
00293                                                                   EL308
00294      PERFORM 0998-TOTAL-REPORTING      THRU 0999-EXIT.            EL308
00295                                                                   EL308
00296      PERFORM CLOSE-FILES                                          EL308
00297                                                                   EL308
00298      GOBACK.                                                      EL308
00299                                                                   EL308
00300  0100-PROCESS-CONTROL-FILE SECTION.                               EL308
00301                                                                   EL308
00302 *    NOTE ******************************************************* EL308
00303 *         *                                                     * EL308
00304 *         *      THE PROCESSING LOGIC OF THIS PROGRAM IS AS     * EL308
00305 *         *  FOLLOWS:                                           * EL308
00306 *         *                                                     * EL308
00307 *         *      1.  READ THE CONTROL FILE SEQUENTIALLY         * EL308
00308 *         *          PROCESSING EACH COMPANY RECORD.            * EL308
00309 *         *                                                     * EL308
00310 *         *      2.  CREATES TABLE WITH COMPANY CODE,           * EL308
00311 *         *          COMPANY NAME, AND COMPANY ID FOR           * EL308
00312 *         *          FOR REPORTING PURPOSES.                    * EL308
00313 *         *                                                     * EL308
00314 *         *      3.  READS THE ARCHIVE LETTER FILE SEQUENTIALLY *    CL**2
00315 *         *          DELETING ANY LETTER WHOSE PRINT DATE       * EL308
00316 *         *          IS MORE THAN 6 MONTHS OLD                  * EL308
00317 *         *                                                     * EL308
00318 *         *      4.  PRODUCES A REPORT BY COMPANY SHOWING       * EL308
00319 *         *          HOW MANY RECORDS WERE READ, DELETED, AND   * EL308
00320 *         *          REWRITTEN TO THE ARCHIVE FILE.             * EL308
00321 *         *                                                     * EL308
00322 *         *      5.  CLOSES ALL FILES.                          * EL308
00323 *         *******************************************************.EL308
00324                                                                   EL308
00325 ***************************************************************** EL308
00326 *   READS THE CONTROL FILE SEQUENTIALLY TABLING INFORMATION     * EL308
00327 *   FOR EACH COMPANY.                                           * EL308
00328 ***************************************************************** EL308
00329      MOVE LOW-VALUES             TO  CF-CONTROL-PRIMARY.          EL308
00330                                                                   EL308
00331      START ELCNTL                                                 EL308
00332          KEY IS GREATER THAN CF-CONTROL-PRIMARY                   EL308
00333                                                                   EL308
00334      IF ELCNTL-FILE-STATUS NOT = ZERO                             EL308
00335          MOVE 'ERROR OCCURED START INITIAL - ELCNTL'              EL308
00336                                  TO  WS-ABEND-MESSAGE             EL308
00337          MOVE ELCNTL-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL308
00338          PERFORM ABEND-PGM.                                       EL308
00339                                                                   EL308
00340      SET TBL                     TO +1.                           EL308
00341      SET TBL                     DOWN BY +1.                      EL308
00342                                                                   EL308
00343      EJECT                                                        EL308
00344  0110-READ-CONTROL-FILE.                                          EL308
00345      READ ELCNTL NEXT                                             EL308
00346                                                                   EL308
00347      IF ELCNTL-FILE-STATUS = '10'                                 EL308
00348          GO TO 0199-EXIT.                                         EL308
00349                                                                   EL308
00350      IF ELCNTL-FILE-STATUS NOT = ZERO                             EL308
00351          MOVE 'ERROR OCCURED READNEXT - ELCNTL'                   EL308
00352                                  TO  WS-ABEND-MESSAGE             EL308
00353          MOVE ELCNTL-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL308
00354          PERFORM ABEND-PGM.                                       EL308
00355                                                                   EL308
00356      IF CF-RECORD-TYPE NOT = '1'                                  EL308
00357          GO TO 0110-READ-CONTROL-FILE.                            EL308
00358                                                                   EL308
00359      SET TBL                     UP BY +1.                        EL308
00360                                                                   EL308
00361      MOVE CF-CL-MAIL-TO-NAME     TO  WS-COMPANY-NAME.             EL308
00362      MOVE CF-COMPANY-ID          TO  WS-TABLED-ID (TBL).          EL308
00363      MOVE CF-COMPANY-CD          TO  WS-TABLED-CD (TBL).          EL308
00364                                                                   EL308
00365      MOVE SPACES                 TO  WS-COMPANY-NAME2.            EL308
00366      SET CN1                     TO  +30.                         EL308
00367      PERFORM 0500-MOVE-NAME      THRU 0599-EXIT.                  EL308
00368                                                                   EL308
00369      GO TO 0110-READ-CONTROL-FILE.                                EL308
00370                                                                   EL308
00371  0199-EXIT.                                                       EL308
00372     EJECT                                                         EL308
00373                                                                   EL308
00374 ***************************************************************** EL308
00375 *               READ AND PROCESS ARCHIVE LETTERS                * EL308
00376 *****************************************************************.EL308
00377  0200-PROCESS-LETTER-FILE.                                        EL308
00378                                                                   EL308
00379      READ LETTERS                                                 EL308
00380         AT END                                                    EL308
00381            PERFORM 0900-REPORTING-ROUTINE THRU 0995-EXIT          EL308
00382            GO TO 0299-EXIT.                                       EL308
00383                                                                   EL308
00384      IF WS-COMPANY-CD  EQUAL LOW-VALUES                           EL308
00385          MOVE LA-COMPANY-CD      TO WS-COMPANY-CD.                EL308
00386                                                                   EL308
00387      IF LA-COMPANY-CD         NOT = WS-COMPANY-CD                 EL308
00388          SET TBL                 TO +1                            EL308
00389          PERFORM 0900-REPORTING-ROUTINE THRU 0995-EXIT            EL308
00390          MOVE LA-COMPANY-CD      TO WS-COMPANY-CD.                EL308
00391                                                                   EL308
00392      ADD +1                      TO WS-RECORDS-READ.              EL308
00393      ADD +1                      TO WS-TOTAL-RECORDS-READ.        EL308
00394                                                                   EL308
00395      IF LA-RECORD-TYPE IS EQUAL TO '4'                            EL308
00396         MOVE SPACES              TO WS-PURGE-SW                   EL308
00397         MOVE LETTER-ARCHIVE      TO OUTFILE-REC                   EL308
00398         PERFORM 0800-WRITE-OUTFILE THRU 0899-EXIT                 EL308
00399         GO TO 0200-PROCESS-LETTER-FILE.                           EL308
00400                                                                   EL308
00401      IF LA-RECORD-TYPE EQUAL  '1'                                 EL308
00402         ADD +1                   TO WS-LETTERS-READ               EL308
00403         PERFORM 0600-CHECK-PRINT-DATE THRU 0699-EXIT.             EL308
00404                                                                   EL308
00405      IF LETTER-SHOULD-BE-PURGED                                   EL308
00406         PERFORM 0700-COUNT-DELETES THRU 0799-EXIT                 EL308
00407      ELSE                                                         EL308
00408         MOVE LETTER-ARCHIVE TO OUTFILE-REC                        EL308
00409         PERFORM 0800-WRITE-OUTFILE THRU 0899-EXIT.                EL308
00410                                                                   EL308
00411      GO TO 0200-PROCESS-LETTER-FILE.                              EL308
00412                                                                   EL308
00413  0299-EXIT.                                                       EL308
00414                                                                   EL308
00415  EJECT                                                            EL308
00416 ***************************************************************** EL308
00417 *   DELETES SPACES IN COMPANY NAME AND TABLES THE NAME          * EL308
00418 *   FOR REPORTING PURPOSES.                                     * EL308
00419 *****************************************************************.EL308
00420  0500-MOVE-NAME.                                                  EL308
00421      IF WS-CN-CHAR (CN1) = SPACES                                 EL308
00422          IF CN1 GREATER THAN +1                                   EL308
00423              SET CN1             DOWN BY +1                       EL308
00424              GO TO 0500-MOVE-NAME                                 EL308
00425            ELSE                                                   EL308
00426              GO TO 0599-EXIT.                                     EL308
00427                                                                   EL308
00428      SET WS-LENGTH               TO CN1.                          EL308
00429                                                                   EL308
00430      SUBTRACT WS-LENGTH FROM +30 GIVING WS-LENGTH.                EL308
00431      IF WS-LENGTH NOT GREATER THAN ZERO                           EL308
00432          MOVE WS-COMPANY-NAME    TO  WS-TABLED-NAME (TBL)         EL308
00433          GO TO 0599-EXIT.                                         EL308
00434                                                                   EL308
00435      DIVIDE +2 INTO WS-LENGTH ROUNDED.                            EL308
00436                                                                   EL308
00437      IF WS-LENGTH NOT GREATER THAN ZERO                           EL308
00438          GO TO 0599-EXIT.                                         EL308
00439                                                                   EL308
00440      SET CN2                     TO CN1.                          EL308
00441      SET CN2                     UP BY WS-LENGTH.                 EL308
00442                                                                   EL308
00443  0530-MOVE-NAME.                                                  EL308
00444      MOVE WS-CN-CHAR (CN1)       TO WS-CN2-CHAR (CN2).            EL308
00445                                                                   EL308
00446      IF CN1 GREATER THAN +1                                       EL308
00447          SET CN1                                                  EL308
00448              CN2                 DOWN BY +1                       EL308
00449          GO TO 0530-MOVE-NAME.                                    EL308
00450                                                                   EL308
00451      MOVE WS-COMPANY-NAME2       TO  WS-TABLED-NAME (TBL).        EL308
00452  0599-EXIT.                                                       EL308
00453      EJECT                                                        EL308
00454 ***************************************************************** EL308
00455 *        DETERMINES WHETHER THE LETTER SHOULD BE PURGED         * EL308
00456 *****************************************************************.EL308
00457  0600-CHECK-PRINT-DATE.                                           EL308
00458      MOVE SPACES                 TO WS-PURGE-SW.                  EL308
00459      IF LA-RESEND-DATE NOT EQUAL TO LOW-VALUES AND SPACES         EL308
00460          IF LA-RESEND-PRINT-DATE EQUAL LOW-VALUES OR SPACES       EL308
00461              GO TO 0699-EXIT.                                     EL308
00462                                                                   EL308
00463      IF LA-RESEND-PRINT-DATE NOT EQUAL TO LOW-VALUES AND SPACES   EL308
00464          IF LA-RESEND-PRINT-DATE NOT GREATER THAN                 EL308
00465                                       WS-YEAR-AGO                 EL308
00466              MOVE 'Y' TO WS-PURGE-SW                              EL308
00467              GO TO 0699-EXIT                                      EL308
00468          ELSE                                                     EL308
00469              GO TO 0699-EXIT.                                     EL308
00470                                                                   EL308
00471      IF LA-INITIAL-PRINT-DATE NOT EQUAL TO LOW-VALUES AND SPACES  EL308
00472          IF LA-INITIAL-PRINT-DATE NOT GREATER THAN                EL308
00473                                  WS-YEAR-AGO                      EL308
00474              MOVE 'Y'            TO WS-PURGE-SW.                  EL308
00475                                                                   EL308
00476  0699-EXIT.                                                       EL308
00477  EJECT                                                            EL308
00478 ***************************************************************** EL308
00479 *  COUNT RECORDS DELETED FROM FILE FOR REPORTING  PURPOSES      * EL308
00480 *****************************************************************.EL308
00481  0700-COUNT-DELETES.                                              EL308
00482                                                                   EL308
00483      IF LA-RECORD-TYPE EQUAL '1'                                  EL308
00484          ADD +1                  TO  WS-LETTERS-DELETED           EL308
00485          PERFORM 0750-UPDATE-CORR-TRLR THRU 0750-EXIT.            EL308
00486                                                                   EL308
00487      ADD +1                      TO  WS-RECORDS-DELETED.          EL308
00488      ADD +1                      TO  WS-TOTAL-RECORDS-DEL.        EL308
00489      GO TO 0799-EXIT.                                             EL308
00490                                                                   EL308
00491  0750-UPDATE-CORR-TRLR.                                           EL308
00492                                                                   EL308
00493      MOVE LA-COMPANY-CD          TO  AT-COMPANY-CD.               EL308
00494      MOVE LA-CARRIER             TO  AT-CARRIER.                  EL308
00495      MOVE LA-CLAIM-NO            TO  AT-CLAIM-NO.                 EL308
00496      MOVE LA-CERT-NO             TO  AT-CERT-NO.                  EL308
00497      MOVE LA-CORR-TRLR-SEQ       TO  AT-SEQUENCE-NO.              EL308
00498                                                                   EL308
00499      READ ELTRLR.                                                 EL308
00500                                                                   EL308
00501      IF ELTRLR-FILE-STATUS IS EQUAL TO '23'                       EL308
00502          GO TO 0750-EXIT.                                         EL308
00503                                                                   EL308
00504      IF ELTRLR-FILE-STATUS IS NOT EQUAL TO '00'                   EL308
00505          MOVE 'ERROR OCCURED READ - ELTRLR '                      EL308
00506                                  TO  WS-ABEND-MESSAGE             EL308
00507          MOVE ELTRLR-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL308
00508          GO TO ABEND-PGM.                                         EL308
00509                                                                   EL308
00510      IF AT-TRAILER-TYPE IS NOT EQUAL TO '4'                       EL308
00511          GO TO 0750-EXIT.                                         EL308
00512                                                                   EL308
00513      MOVE WS-CURRENT-DATE        TO  AT-LETTER-PURGED-DT          EL308
00514                                      AT-CORR-LAST-MAINT-DT.       EL308
00515      MOVE 'E308'                 TO  AT-CORR-LAST-UPDATED-BY.     EL308
00516                                                                   EL308
00517      REWRITE ACTIVITY-TRAILERS.                                   EL308
00518                                                                   EL308
00519      IF ELTRLR-FILE-STATUS IS NOT EQUAL TO '00'                   EL308
00520          MOVE 'ERROR OCCURED REWRITE - ELTRLR '                   EL308
00521                                  TO  WS-ABEND-MESSAGE             EL308
00522          MOVE ELTRLR-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL308
00523          GO TO ABEND-PGM.                                         EL308
00524                                                                   EL308
00525  0750-EXIT.                                                       EL308
00526      EXIT.                                                        EL308
00527                                                                   EL308
00528  0799-EXIT.                                                       EL308
00529  EJECT                                                            EL308
00530 ***************************************************************** EL308
00531 *  WRITES OUTFILE                                               * EL308
00532 *****************************************************************.EL308
00533                                                                   EL308
00534  0800-WRITE-OUTFILE.                                              EL308
00535                                                                   EL308
00536      WRITE OUTFILE-REC.                                           EL308
00537                                                                   EL308
00538      IF LA-RECORD-TYPE EQUAL '1'                                  EL308
00539          ADD +1                  TO  WS-LETTERS-OUTPUT.           EL308
00540                                                                   EL308
00541      ADD +1                      TO  WS-RECORDS-OUTPUT.           EL308
00542      ADD +1                      TO  WS-TOTAL-RECORDS-OUT.        EL308
00543                                                                   EL308
00544  0899-EXIT.                                                       EL308
00545  EJECT                                                            EL308
00546 ***************************************************************** EL308
00547 *  GENERATES REPORT BY COMPANY                                  * EL308
00548 *****************************************************************.EL308
00549                                                                   EL308
00550  0900-REPORTING-ROUTINE.                                          EL308
00551      IF TBL  EQUAL +100                                           EL308
00552          DISPLAY 'COMPANY CD NOT FOUND'                           EL308
00553          MOVE SPACES             TO WS-H2-COMPANY-ID              EL308
00554          MOVE SPACES             TO WS-H2-COMPANY-NAME            EL308
00555          GO TO 0900-CONTINUE.                                     EL308
00556                                                                   EL308
00557      IF WS-COMPANY-CD EQUAL WS-TABLED-CD (TBL)                    EL308
00558          MOVE WS-TABLED-ID (TBL) TO WS-H2-COMPANY-ID              EL308
00559          MOVE WS-TABLED-NAME (TBL)                                EL308
00560                                  TO WS-H2-COMPANY-NAME            EL308
00561          GO TO 0900-CONTINUE                                      EL308
00562      ELSE                                                         EL308
00563          SET TBL UP BY +1                                         EL308
00564          GO TO 0900-REPORTING-ROUTINE.                            EL308
00565                                                                   EL308
00566  0900-CONTINUE.                                                   EL308
00567                                                                   EL308
00568      MOVE '0'                    TO WS-D1-LINE-CNTL.              EL308
00569      MOVE +99                    TO WS-LINE-COUNT.                EL308
00570      MOVE  'LETTERS READ'        TO WS-D1-DESC.                   EL308
00571      MOVE WS-LETTERS-READ        TO WS-D1-COUNT.                  EL308
00572      MOVE WS-DETAIL1             TO PRT.                          EL308
00573      PERFORM WRITE-A-LINE.                                        EL308
00574                                                                   EL308
00575      MOVE ' '                    TO WS-D1-LINE-CNTL.              EL308
00576      MOVE  'LETTERS DELETED'     TO WS-D1-DESC.                   EL308
00577      MOVE WS-LETTERS-DELETED     TO WS-D1-COUNT.                  EL308
00578      MOVE WS-DETAIL1             TO PRT.                          EL308
00579      PERFORM WRITE-A-LINE.                                        EL308
00580                                                                   EL308
00581      MOVE  'LETTERS WRITTEN'     TO WS-D1-DESC.                   EL308
00582      MOVE WS-LETTERS-OUTPUT      TO WS-D1-COUNT.                  EL308
00583      MOVE WS-DETAIL1             TO PRT.                          EL308
00584      PERFORM WRITE-A-LINE.                                        EL308
00585                                                                   EL308
00586                                                                   EL308
00587      MOVE '0'                    TO WS-D1-LINE-CNTL.              EL308
00588      MOVE  'RECORDS READ'        TO WS-D1-DESC.                   EL308
00589      MOVE WS-RECORDS-READ        TO WS-D1-COUNT.                  EL308
00590      MOVE WS-DETAIL1             TO PRT.                          EL308
00591      PERFORM WRITE-A-LINE.                                        EL308
00592                                                                   EL308
00593      MOVE ' '                    TO WS-D1-LINE-CNTL.              EL308
00594      MOVE  'RECORDS DELETED'     TO WS-D1-DESC.                   EL308
00595      MOVE WS-RECORDS-DELETED     TO WS-D1-COUNT.                  EL308
00596      MOVE WS-DETAIL1             TO PRT.                          EL308
00597      PERFORM WRITE-A-LINE.                                        EL308
00598                                                                   EL308
00599      MOVE  'RECORDS WRITTEN'     TO WS-D1-DESC.                   EL308
00600      MOVE WS-RECORDS-OUTPUT      TO WS-D1-COUNT.                  EL308
00601      MOVE WS-DETAIL1             TO PRT.                          EL308
00602      PERFORM WRITE-A-LINE.                                        EL308
00603                                                                   EL308
00604      MOVE ZERO                   TO WS-LETTERS-READ               EL308
00605                                     WS-RECORDS-READ               EL308
00606                                     WS-LETTERS-DELETED            EL308
00607                                     WS-RECORDS-DELETED            EL308
00608                                     WS-LETTERS-OUTPUT             EL308
00609                                     WS-RECORDS-OUTPUT.            EL308
00610                                                                   EL308
00611  0995-EXIT.                                                       EL308
00612  EJECT                                                            EL308
00613 ***************************************************************** EL308
00614 *   REPORTS TOTAL FOR ENTIRE RUN                                * EL308
00615 *****************************************************************.EL308
00616  0998-TOTAL-REPORTING.                                            EL308
00617      MOVE SPACES             TO WS-H2-COMPANY-ID.                 EL308
00618      MOVE SPACES             TO WS-H2-COMPANY-NAME.               EL308
00619                                                                   EL308
00620      MOVE +99                    TO WS-LINE-COUNT.                EL308
00621      MOVE '0'                    TO P-CTL.                        EL308
00622      MOVE  'TOTAL RECORDS READ-  - ' TO WS-D1-DESC.               EL308
00623      MOVE WS-TOTAL-RECORDS-READ  TO WS-D1-COUNT.                  EL308
00624      MOVE WS-DETAIL1             TO PRT.                          EL308
00625      PERFORM WRITE-A-LINE.                                        EL308
00626                                                                   EL308
00627      MOVE  '      RECORDS DELETED - ' TO WS-D1-DESC.              EL308
00628      MOVE WS-TOTAL-RECORDS-DEL   TO WS-D1-COUNT.                  EL308
00629      MOVE WS-DETAIL1             TO PRT.                          EL308
00630      PERFORM WRITE-A-LINE.                                        EL308
00631                                                                   EL308
00632      MOVE  '      RECORDS WRITTEN -'     TO WS-D1-DESC.           EL308
00633      MOVE WS-TOTAL-RECORDS-OUT   TO WS-D1-COUNT.                  EL308
00634      MOVE WS-DETAIL1             TO PRT.                          EL308
00635      PERFORM WRITE-A-LINE.                                        EL308
00636  0999-EXIT.                                                       EL308
00637  EJECT                                                            EL308
00638                                                                   EL308
00639 ***************************************************************** EL308
00640 *                        DATE CONVERSION                        * EL308
00641 *****************************************************************.EL308
00642  8500-DATE-CONVERSION SECTION.   COPY ELCDCS.                     EL308
00643                                                                   EL308
00644      EJECT                                                        EL308
00645  WRITE-A-LINE SECTION.           COPY ELCWAL.                     EL308
00646                                                                   EL308
00647      EJECT                                                        EL308
00648  WRITE-HEADINGS SECTION.                                          EL308
00649                                                                   EL308
00650 *    NOTE ******************************************************* EL308
00651 *         *      THIS SECTION CONTROLS THE WRITING OF THE       * EL308
00652 *         *  HEADINGS.                                          * EL308
00653 *         *******************************************************.EL308
00654                                                                   EL308
00655  WHS-010.                                                         EL308
00656      ADD +1  TO  WS-PAGE.                                         EL308
00657      MOVE WS-PAGE                TO  WS-H2-PAGE.                  EL308
00658                                                                   EL308
00659      MOVE PRT                    TO  WS-SAVE-PRINT-RECORD.        EL308
00660                                                                   EL308
00661      MOVE WS-HEADING1            TO  PRT.                         EL308
00662      PERFORM WRITE-PRINTER.                                       EL308
00663                                                                   EL308
00664      MOVE WS-HEADING2            TO  PRT.                         EL308
00665      PERFORM WRITE-PRINTER.                                       EL308
00666                                                                   EL308
00667      MOVE WS-HEADING3            TO  PRT.                         EL308
00668      PERFORM WRITE-PRINTER.                                       EL308
00669                                                                   EL308
00670      MOVE WS-HEADING4            TO  PRT.                         EL308
00671      PERFORM WRITE-PRINTER.                                       EL308
00672                                                                   EL308
00673      MOVE +6                     TO  WS-LINE-COUNT.               EL308
00674                                                                   EL308
00675  WHS-020.                        COPY ELCWHS2.                    EL308
00676                                                                   EL308
00677      EJECT                                                        EL308
00678  WRITE-PRINTER SECTION.          COPY ELCWPS.                     EL308
00679                                                                   EL308
00680      MOVE P-CTL TO LCP-ASA                                        EL308
00681      PERFORM LCP-WRITE-POS-PRT                                    EL308
00682          THRU LCP-WRITE-END-PRT.                                  EL308
00683                                                                   EL308
00684  WPS-EXIT.                                                        EL308
00685      EXIT.                                                        EL308
00686                                                                   EL308
00687      EJECT                                                        EL308
00688 ***************************************************************** EL308
00689 *                          OPENS  FILES                         * EL308
00690 *****************************************************************.EL308
00691  OPEN-FILES SECTION.                                              EL308
00692                                                                   EL308
00693  OFS-010.                                                         EL308
00694      OPEN INPUT ELCNTL                                            EL308
00695                 LETTERS                                           EL308
00696          OUTPUT PRNTR                                             EL308
00697                 OUTFILE                                           EL308
00698          I-O    ELTRLR.                                           EL308
00699                                                                   EL308
00700      IF ELCNTL-FILE-STATUS  = '00' OR '97'                        EL308
00701          NEXT SENTENCE                                            EL308
00702        ELSE                                                       EL308
00703          MOVE 'ELCNTL  '         TO  WS-FEM-FILE-NAME             EL308
00704          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          EL308
00705          MOVE ELCNTL-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL308
00706          PERFORM ABEND-PGM.                                       EL308
00707                                                                   EL308
00708      IF ELTRLR-FILE-STATUS  = '00' OR '97'                        EL308
00709          NEXT SENTENCE                                            EL308
00710      ELSE                                                         EL308
00711          MOVE 'ELTRLR  '         TO  WS-FEM-FILE-NAME             EL308
00712          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          EL308
00713          MOVE ELTRLR-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL308
00714          GO TO ABEND-PGM.                                         EL308
00715      ACCEPT  LCP-DATE-NEW-74 FROM DATE                            EL308
00716      MOVE CORRESPONDING LCP-DATE-NEW-74 TO LCP-CURRENT-DATE-68    EL308
00717                                                                   EL308
00718      MOVE  LCP-CURRENT-DATE-68 TO DC-GREG-DATE-1-EDIT.            EL308
00719      MOVE '2'                    TO  DC-OPTION-CODE.              EL308
00720      PERFORM 8500-DATE-CONVERSION.                                EL308
00721      MOVE DC-BIN-DATE-1          TO  WS-CURRENT-DATE              EL308
00722                                      WS-RUN-DATE.                 EL308
00723      MOVE DC-GREG-DATE-1-ALPHA   TO  WS-H3-DATE.                  EL308
00724                                                                   EL308
00725 * CLACULATE PURGE DATE.                                           EL308
00726      MOVE WS-CURRENT-DATE        TO  DC-BIN-DATE-1.               EL308
00727      MOVE -12                    TO  DC-ELAPSED-MONTHS.           EL308
00728      MOVE '6'                    TO  DC-OPTION-CODE.              EL308
00729      PERFORM 8500-DATE-CONVERSION.                                EL308
00730      MOVE DC-BIN-DATE-2          TO  WS-YEAR-AGO.                 EL308
00731                                                                   EL308
00732      MOVE WS-YEAR-AGO            TO  DC-BIN-DATE-1.               EL308
00733      MOVE ' '                    TO  DC-OPTION-CODE.              EL308
00734      PERFORM 8500-DATE-CONVERSION.                                EL308
00735      MOVE DC-GREG-DATE-1-YMD     TO  WS-DATE-YMD.                 EL308
00736      MOVE WS-YY                  TO  WS-PRINT-YY.                 EL308
00737      MOVE WS-MM                  TO  WS-PRINT-MM.                 EL308
00738      MOVE WS-DD                  TO  WS-PRINT-DD.                 EL308
00739      MOVE WS-PRINT-DATE-YMD      TO  WS-H4-DATE.                  EL308
00740      ACCEPT LCP-TIME-OF-DAY-74 FROM TIME                          EL308
00741      MOVE LCP-TIME-74 TO LCP-TIME-OF-DAY-68                       EL308
00742                                                                   EL308
00743      MOVE  LCP-TIME-OF-DAY-68 TO WS-CURRENT-TIME.                 EL308
00744                                                                   EL308
00745      MOVE +99                    TO  WS-LINE-COUNT.               EL308
00746                                                                   EL308
00747  OFS-EXIT.                                                        EL308
00748      EXIT.                                                        EL308
00749                                                                   EL308
00750      EJECT                                                        EL308
00751 ***************************************************************** EL308
00752 *                          CLOSE  FILES                         * EL308
00753 *****************************************************************.EL308
00754  CLOSE-FILES SECTION.                                             EL308
00755                                                                   EL308
00756  CFS-010.                                                         EL308
00757      CLOSE LETTERS                                                EL308
00758            OUTFILE                                                EL308
00759            PRNTR                                                  EL308
00760            ELTRLR.                                                EL308
00761                                                                   EL308
00762      IF ELTRLR-FILE-STATUS IS EQUAL TO '00'                       EL308
00763          NEXT SENTENCE                                            EL308
00764      ELSE                                                         EL308
00765          MOVE 'ERROR OCCURED CLOSE - ELTRLR '                     EL308
00766                                  TO  WS-ABEND-MESSAGE             EL308
00767          MOVE ELTRLR-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL308
00768          GO TO ABEND-PGM.                                         EL308
00769                                                                   EL308
00770  CFS-EXIT.                                                        EL308
00771      EXIT.                                                        EL308
00772                                                                   EL308
00773  ABEND-PGM SECTION.              COPY ELCABEND SUPPRESS.          EL308
00774 /                                                                 EL308
00775  LCP-WRITE-POS-PRT SECTION.                                       EL308
00776      IF LCP-ASA = '+'                                             EL308
00777          WRITE PRT AFTER 0 LINE                                   EL308
00778      ELSE                                                         EL308
00779      IF LCP-ASA = ' '                                             EL308
00780          WRITE PRT AFTER ADVANCING 1 LINE                         EL308
00781      ELSE                                                         EL308
00782      IF LCP-ASA = '0'                                             EL308
00783          WRITE PRT AFTER ADVANCING 2 LINE                         EL308
00784      ELSE                                                         EL308
00785      IF LCP-ASA = '-'                                             EL308
00786          WRITE PRT AFTER ADVANCING 3 LINE                         EL308
00787      ELSE                                                         EL308
00788      IF LCP-ASA = '1'                                             EL308
00789          WRITE PRT AFTER ADVANCING PAGE                           EL308
00790      ELSE                                                         EL308
00791      IF LCP-ASA = '2'                                             EL308
00792          WRITE PRT AFTER ADVANCING LCP-CH2                        EL308
00793      ELSE                                                         EL308
00794      IF LCP-ASA = '3'                                             EL308
00795          WRITE PRT AFTER ADVANCING LCP-CH3                        EL308
00796      ELSE                                                         EL308
00797      IF LCP-ASA = '4'                                             EL308
00798          WRITE PRT AFTER ADVANCING LCP-CH4                        EL308
00799      ELSE                                                         EL308
00800      IF LCP-ASA = '5'                                             EL308
00801          WRITE PRT AFTER ADVANCING LCP-CH5                        EL308
00802      ELSE                                                         EL308
00803      IF LCP-ASA = '6'                                             EL308
00804          WRITE PRT AFTER ADVANCING LCP-CH6                        EL308
00805      ELSE                                                         EL308
00806      IF LCP-ASA = '7'                                             EL308
00807          WRITE PRT AFTER ADVANCING LCP-CH7                        EL308
00808      ELSE                                                         EL308
00809      IF LCP-ASA = '8'                                             EL308
00810          WRITE PRT AFTER ADVANCING LCP-CH8                        EL308
00811      ELSE                                                         EL308
00812      IF LCP-ASA = '9'                                             EL308
00813          WRITE PRT AFTER ADVANCING LCP-CH9                        EL308
00814      ELSE                                                         EL308
00815      IF LCP-ASA = 'A'                                             EL308
00816          WRITE PRT AFTER ADVANCING LCP-CH10                       EL308
00817      ELSE                                                         EL308
00818      IF LCP-ASA = 'B'                                             EL308
00819          WRITE PRT AFTER ADVANCING LCP-CH11                       EL308
00820      ELSE                                                         EL308
00821      IF LCP-ASA = 'C'                                             EL308
00822          WRITE PRT AFTER ADVANCING LCP-CH12                       EL308
00823      ELSE                                                         EL308
00824      IF LCP-ASA = 'V'                                             EL308
00825          WRITE PRT AFTER ADVANCING LCP-P01                        EL308
00826      ELSE                                                         EL308
00827      IF LCP-ASA = 'W'                                             EL308
00828          WRITE PRT AFTER ADVANCING LCP-P02                        EL308
00829      ELSE                                                         EL308
00830      DISPLAY 'ASA CODE ERROR'.                                    EL308
00831  LCP-WRITE-END-PRT.                                               EL308
00832      EXIT.                                                        EL308
00833                                                                   EL308
