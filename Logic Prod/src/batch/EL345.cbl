00001  IDENTIFICATION DIVISION.                                         04/18/98
00002                                                                   EL345
00003  PROGRAM-ID.                 EL345 .                                 LV006
00004 *              PROGRAM CONVERTED BY                               EL345
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL345
00006 *              CONVERSION DATE 02/15/96 19:14:03.                 EL345
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            EL345
00008 *                            VMOD=2.003                           EL345
00009                                                                   EL345
00009                                                                   EL345
00010 *AUTHOR.     LOGIC INC.                                           EL345
00011 *            DALLAS, TEXAS.                                       EL345
00012                                                                   EL345
00013 *DATE-COMPILED.                                                   EL345
00014                                                                   EL345
00015 *SECURITY.   *****************************************************EL345
00016 *            *                                                   *EL345
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *EL345
00018 *            *                                                   *EL345
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL345
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL345
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *EL345
00022 *            *                                                   *EL345
00023 *            *****************************************************EL345
00024                                                                   EL345
00025 *REMARKS.                                                         EL345
00026 ***************************************************************** EL345
00027 *       THIS PROGRAM PRODUCES THE LIFE CLAIMS OVER TEN THOUSAND * EL345
00028 *     DOLLARS REPORT.  IT IS PRODUCED MONTHLY, AND REPORTS      * EL345
00029 *     ONLY THOSE CLAIMS THAT OCCURRED IN THE REPORTING MONTH.   * EL345
00030 *       THE CLAIMS EXTRACT FILE IS READ, AND DATA IS EXTRACTED  * EL345
00031 *     FROM SELECTED RECORDS. THE EXTRACTED DATA IS RUN THROUGH  * EL345
00032 *     A SORT, SEQUENCING BY STATE AND ACCOUNT NUMBER.           * EL345
00033 *       ONCE SORTED, THE REPORT IS PRODUCED.  NO TOTALS ARE     * EL345
00034 *     REPORTED.                                                 * EL345
00035 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EL345
00036 *                                                                 EL345
00037 *       THE FILE STATISTICS  AND PROCESSOR STATISTICS:            EL345
00038 *                                                                 EL345
00039 *       INPUTS:    CLAIM DETAIL EXTRACT FILE                      EL345
00040 *                  DATE CARD FILE                                 EL345
00041 *                                                                 EL345
00042 *       OUTPUTS:   LIFE CLAIMS OVER $10,000.00 (MONTHLY)          EL345
00043 *                                                                 EL345
00044 ******************************************************************EL345
00045                                                                   EL345
00046      EJECT                                                        EL345
00047  ENVIRONMENT DIVISION.                                            EL345
00048  CONFIGURATION SECTION.                                           EL345
00049  SPECIAL-NAMES.                                                   EL345
00050      C01 IS TO-TOP-OF-PAGE.                                       EL345
00051                                                                   EL345
00052  INPUT-OUTPUT SECTION.                                            EL345
00053                                                                   EL345
00054  FILE-CONTROL.                                                    EL345
00055                                                                   EL345
00056      SELECT DISK-DATE          ASSIGN TO SYS019-FBA1-S-SYS019.    EL345
00057                                                                   EL345
00058      SELECT EXTRACT-FILE       ASSIGN TO SYS010-UT-2400-S-SYS010. EL345
00059                                                                   EL345
00060      SELECT REPORT-FILE        ASSIGN TO SYS008-UR-1403-S-SYS008. EL345
00061                                                                   EL345
00062      SELECT SORT-FILE          ASSIGN TO SYS001-UT-FBA1-S-SORTWK1.EL345
00063                                                                   EL345
00064  DATA DIVISION.                                                   EL345
00065                                                                   EL345
00066  FILE SECTION.                                                    EL345
00067                                                                   EL345
00068  FD  EXTRACT-FILE                                                 EL345
00069      BLOCK  CONTAINS   0 RECORDS                                  EL345
00070      RECORDING MODE F.                                            EL345
00071  01  EXTRACT-RECORD              PIC X(510).                      EL345
00072                                                                   EL345
00073                                                                   EL345
00074  FD  REPORT-FILE            COPY ELCPRTFD.                        EL345
00075                                                                   EL345
00076                                                                   EL345
00077  FD  DISK-DATE              COPY ELCDTEFD.                        EL345
00078                                                                   EL345
00079                                                                   EL345
00080  SD  SORT-FILE.                                                   EL345
00081  01  SORT-RECORD.                                                 EL345
00082      12  SR-KEY.                                                  EL345
00083          16  SR-STATE            PIC  X(02).                      EL345
00084          16  SR-ACCOUNT          PIC  X(10).                      EL345
00085      12  SR-INCURRED-DATE        PIC  9(11)  COMP-3.              EL345
00091      12  SR-EFFECTIVE-DATE       PIC  9(11)     COMP-3.              CL**4
00092      12  SR-LOAN-TYPE            PIC  X(02).                      EL345
00093      12  SR-TRANS                PIC  X(03).                      EL345
00094      12  SR-CLAIM-NUMBER         PIC  X(07).                      EL345
00095      12  SR-BENEFIT-AMT          PIC S9(07)V99  COMP-3.           EL345
00096      12  SR-PAID-AMT             PIC S9(07)V99  COMP-3.           EL345
00097                                                                   EL345
00098      EJECT                                                        EL345
00099  WORKING-STORAGE SECTION.                                         EL345
00100  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      EL345
00101                                                                   EL345
00102  01  FILLER.                                                      EL345
00103      12  FILLER  PIC X(32) VALUE '******************************'.EL345
00104      12  FILLER  PIC X(32) VALUE '    EL345 WORKING-STORAGE     '.EL345
00105      12  FILLER  PIC X(32) VALUE '******** VMOD=2.003 **********'.EL345
00106                                                                   EL345
00107  01  EL345-WORKING-STORAGE.                                       EL345
00108      12  WS-PAGE-COUNT           PIC S9(05)   VALUE +0  COMP-3.   EL345
00109      12  WS-LINE-COUNT           PIC S9(03)   VALUE +99 COMP-3.   EL345
00110      12  WS-CC                   PIC  9(01)   VALUE 1.            EL345
00111      12  WS-RUN-DT               PIC  X(02)   VALUE SPACES.          CL**2
00112      12  WS-EOF-SW               PIC  X(01)   VALUE 'N'.          EL345
00113          88  WS-EOF-YES                       VALUE 'Y'.          EL345
00114      12  WS-SORT-EOF-SW          PIC  X(01)   VALUE 'N'.          EL345
00115          88  WS-SORT-EOF-YES                  VALUE 'Y'.          EL345
00116      12  TEN-THOUSAND-DOLLARS    PIC S9(7)V99 VALUE +25000.00     EL345
00117                                               COMP-3.                CL**5
00118      12  WS-SR-INCUR-DATE        PIC  9(11).                         CL**4
00119      12  WS-SR-INCUR-DATE-N REDEFINES WS-SR-INCUR-DATE.
00120          16  FILLER              PIC  999.                           CL**4
00121          16  SR-INCUR-CC         PIC  99.                            CL**4
00122          16  SR-INCUR-YR         PIC  99.                            CL**4
00123          16  SR-INCUR-MO         PIC  99.                            CL**4
00124          16  SR-INCUR-DA         PIC  99.                            CL**4
00118      12  WS-SR-EFFECTIVE-DATE    PIC  9(11).                         CL**4
00119      12  WS-SR-EFFECTIVE-DATE-N REDEFINES WS-SR-EFFECTIVE-DATE.      CL**4
00120          16  FILLER              PIC  999.                           CL**4
00121          16  SR-EFFECTIVE-CC     PIC  99.                            CL**4
00122          16  SR-EFFECTIVE-YR     PIC  99.                            CL**4
00123          16  SR-EFFECTIVE-MO     PIC  99.                            CL**4
00124          16  SR-EFFECTIVE-DA     PIC  99.                            CL**4
00125      12  WS-RETURN-CODE          PIC S9(03)   VALUE ZERO.         EL345
00126      12  WS-ABEND-FILE-STATUS    PIC  X(02)   VALUE ZERO.         EL345
00127      12  WS-ABEND-MESSAGE        PIC  X(80)   VALUE SPACES.       EL345
00128      12  WS-ZERO                 PIC S9(01)   VALUE +0    COMP-3. EL345
00129      12  PGM-SUB                 PIC S9(04)   VALUE +345  COMP.   EL345
00130                                                                   EL345
00131     EJECT                                                         EL345
00132                                  COPY ECSEXT01.                   EL345
00133                                  COPY ELCEXTVR.                      CL**4
00134     EJECT                                                         EL345
00135  01  REPORT-LINES.                                                EL345
00136      12  WS-HEADING-1.                                            EL345
00137          16  FILLER              PIC X(54)    VALUE SPACES.       EL345
00138          16  FILLER              PIC X(24)    VALUE               EL345
00139              'LIFE CLAIMS OVER $25,000'.                          EL345
00140          16  FILLER              PIC X(46)    VALUE SPACES.       EL345
00141          16  WS-H1-REPORT-ID     PIC X(06)    VALUE 'EL345'.      EL345
00142          16  FILLER              PIC X(03)    VALUE SPACES.       EL345
00143                                                                   EL345
00144      12  WS-HEADING-2.                                            EL345
00145          16  FILLER              PIC X(51)    VALUE SPACES.       EL345
00146          16  WS-H2-COMPANY-NAME  PIC X(30).                       EL345
00147          16  FILLER              PIC X(43)    VALUE SPACES.       EL345
00148          16  WS-H2-RUN-DATE.                                      EL345
00149              20  WS-H2-RUN-MM    PIC X(02).                       EL345
00150              20  FILLER          PIC X(01)    VALUE '/'.          EL345
00151          16  WS-H2-RUN-DD        PIC X(02).                       EL345
00152          16  FILLER              PIC X(01)    VALUE '/'.          EL345
00153          16  WS-H2-RUN-YY        PIC X(02).                       EL345
00154                                                                   EL345
00155      12  WS-HEADING-3.                                            EL345
00156          16  FILLER              PIC X(57)      VALUE SPACES.     EL345
00157          16  WS-H3-REPORT-DATE   PIC X(18).                       EL345
00158          16  FILLER              PIC X(49)      VALUE SPACES.     EL345
00159          16  FILLER              PIC X(05)      VALUE 'PAGE '.    EL345
00160          16  WS-H3-PAGE          PIC ZZ9.                         EL345
00161                                                                   EL345
00162      12  WS-HEADING-4.                                            EL345
00163          16  FILLER              PIC X(50)      VALUE             EL345
00164              '                   ACCOUNT         INCURRED      E'.EL345
00165          16  FILLER              PIC X(50)      VALUE             EL345
00166              'FFECTIVE    LOAN              CLAIM           BENE'.EL345
00167          16  FILLER              PIC X(33)      VALUE             EL345
00168              'FIT             PAID             '.                 EL345
00169                                                                   EL345
00170      12  WS-HEADING-5.                                            EL345
00171          16  FILLER              PIC X(50)      VALUE             EL345
00172              '          STATE    NUMBER            DATE         '.EL345
00173          16  FILLER              PIC X(50)      VALUE             EL345
00174              ' DATE       TYPE    TRANS     NUMBER          AMOU'.EL345
00175          16  FILLER              PIC X(33)      VALUE             EL345
00176              'NT             AMOUNT            '.                 EL345
00177                                                                   EL345
00178      12  WS-DETAIL1.                                              EL345
00179          16  FILLER              PIC X(11)      VALUE  SPACES.    EL345
00180          16  WS-D1-STATE         PIC X(02).                       EL345
00181          16  FILLER              PIC X(06)      VALUE  SPACES.    EL345
00182          16  WS-D1-ACCOUNT-NBR   PIC X(10).                       EL345
00183          16  FILLER              PIC X(06)      VALUE  SPACES.    EL345
00184          16  WS-D1-INCURRED-DATE.                                 EL345
00185              20  WS-D1-INCURRED-MO   PIC X(02).                   EL345
00186              20  FILLER              PIC X(01)  VALUE  '/'.       EL345
00187              20  WS-D1-INCURRED-DA   PIC X(02).                   EL345
00188              20  FILLER              PIC X(01)  VALUE  '/'.       EL345
00189              20  WS-D1-INCURRED-YR   PIC X(02).                   EL345
00190          16  FILLER              PIC X(06)      VALUE  SPACES.    EL345
00191          16  WS-D1-EFFECTIVE-DATE.                                EL345
00192              20  WS-D1-EFFECTIVE-MO  PIC X(02).                   EL345
00193              20  FILLER              PIC X(01)  VALUE  '/'.       EL345
00194              20  WS-D1-EFFECTIVE-DA  PIC X(02).                   EL345
00195              20  FILLER              PIC X(01)  VALUE  '/'.       EL345
00196              20  WS-D1-EFFECTIVE-YR  PIC X(02).                   EL345
00197          16  FILLER              PIC X(06)      VALUE  SPACES.    EL345
00198          16  WS-D1-LOAN-TYPE     PIC X(02).                       EL345
00199          16  FILLER              PIC X(06)      VALUE  SPACES.    EL345
00200          16  WS-D1-TRANS         PIC X(03).                       EL345
00201          16  FILLER              PIC X(06)      VALUE  SPACES.    EL345
00202          16  WS-D1-CLAIM-NBR     PIC X(07).                       EL345
00203          16  FILLER              PIC X(06)      VALUE  SPACES.    EL345
00204          16  WS-D1-BENEFIT-AMT   PIC Z,ZZZ,ZZ9.99-.               EL345
00205          16  FILLER              PIC X(06)      VALUE  SPACES.    EL345
00206          16  WS-D1-PAID-AMT      PIC Z,ZZZ,ZZ9.99-.               EL345
00207          16  FILLER              PIC X(09)      VALUE  SPACES.    EL345
00208                                                                   EL345
00209      EJECT                                                        EL345
00210                                  COPY ELCDATE.                       CL**6
00211                                  COPY ELCDTECX.                      CL**2
00212                                  COPY ELCDTEVR.                      CL**3
00213      EJECT                                                        EL345
00214  PROCEDURE DIVISION.                                              EL345
00215                                                                   EL345
00216  0000-LOAD-DATE-CARD.            COPY ELCDTERX.                      CL**2
00217                                                                   EL345
00218      SORT SORT-FILE                                               EL345
00219          ON ASCENDING KEY SR-KEY                                  EL345
00220              INPUT  PROCEDURE IS 1000-INPUT-PROCEDURE             EL345
00221              OUTPUT PROCEDURE IS 2000-OUTPUT-PROCEDURE.           EL345
00222                                                                   EL345
00223      GOBACK.                                                      EL345
00224                                                                   EL345
00225  1000-INPUT-PROCEDURE            SECTION.                         EL345
00226                                                                   EL345
00227      OPEN INPUT EXTRACT-FILE.                                     EL345
00228                                                                   EL345
00229      READ EXTRACT-FILE INTO DETAIL-EXTRACT                        EL345
00230          AT END MOVE 'Y' TO WS-EOF-SW.                            EL345
00231                                                                   EL345
00232      PERFORM 1100-INPUT-LOOP                                      EL345
00233          UNTIL WS-EOF-YES.                                        EL345
00234                                                                   EL345
00235      CLOSE EXTRACT-FILE.                                          EL345
00236                                                                   EL345
00237  1100-INPUT-LOOP                 SECTION.                         EL345
00238                                                                   EL345
00239 *** SELECT ONLY CLAIM RECORDS                                     EL345
00240      IF NOT DE-CLAIM                                              EL345
00241          GO TO 1190-READ-NEXT.                                    EL345
00242                                                                   EL345
00243 *** SELECT ONLY LIFE RECORDS                                      EL345
00244      IF NOT DE-DEATH                                              EL345
00245          GO TO 1190-READ-NEXT.                                    EL345
00246                                                                   EL345
           IF DE-REIN = 'R'
              GO TO 1190-READ-NEXT
           END-IF
00247     COPY ELCEXTM1.                                                   CL**4
00248                                                                      CL**4
00249 *** SELECT RECORDS FOR CURRENT YEAR                               EL345
00250 *    IF DE-CP-YR NOT = RUN-YR                                     EL345
00251 *        GO TO 1190-READ-NEXT.                                    EL345
00252                                                                   EL345
00253 *** SELECT RECORDS FOR CURRENT MONTH                              EL345
00254 *    IF DE-CP-MO NOT = RUN-MO                                     EL345
00255 *        GO TO 1190-READ-NEXT.                                    EL345
00256                                                                   EL345
00257 *** SELECT RECORDS OVER TEN THOUSAND DOLLARS                      EL345
00258      IF DE-CLAIM-AMT < TEN-THOUSAND-DOLLARS                       EL345
00259          GO TO 1190-READ-NEXT.                                    EL345
00260                                                                   EL345
00261      MOVE DE-STATE               TO SR-STATE.                     EL345
00262      MOVE DE-ACCOUNT             TO SR-ACCOUNT.                   EL345
00263      MOVE DE-INCUR               TO SR-INCURRED-DATE.             EL345
00264      MOVE DE-EFF                 TO SR-EFFECTIVE-DATE                CL**4
00265                                     WS-SR-EFFECTIVE-DATE.            CL**4
00266      MOVE DE-LF-TYPE             TO SR-LOAN-TYPE.                 EL345
00267      MOVE DE-PAY-CODE            TO SR-TRANS.                     EL345
00268      MOVE DE-CNUM                TO SR-CLAIM-NUMBER.              EL345
00269                                                                   EL345
00270      IF DE-LF-BEN             NUMERIC                             EL345
00271          MOVE DE-LF-BEN          TO SR-BENEFIT-AMT                EL345
00272        ELSE                                                       EL345
00273          MOVE ZERO               TO SR-BENEFIT-AMT.               EL345
00274                                                                   EL345
           IF DE-LF-BEN-ALT NUMERIC
              ADD DE-LF-BEN-ALT TO SR-BENEFIT-AMT
           END-IF
00275      IF DE-CLAIM-AMT          NUMERIC                             EL345
00276          MOVE DE-CLAIM-AMT       TO SR-PAID-AMT                   EL345
00277        ELSE                                                       EL345
00278          MOVE ZERO               TO SR-PAID-AMT.                  EL345
00279                                                                   EL345
00280      RELEASE SORT-RECORD.                                         EL345
00281                                                                   EL345
00282  1190-READ-NEXT.                                                  EL345
00283                                                                   EL345
00284      READ EXTRACT-FILE INTO DETAIL-EXTRACT                        EL345
00285          AT END MOVE 'Y' TO WS-EOF-SW.                            EL345
00286                                                                   EL345
00287      EJECT                                                        EL345
00288  2000-OUTPUT-PROCEDURE            SECTION.                        EL345
00289                                                                   EL345
00290      OPEN OUTPUT REPORT-FILE.                                     EL345
00291                                                                   EL345
00292      MOVE COMPANY-NAME           TO WS-H2-COMPANY-NAME.           EL345
00293      MOVE RUN-MO                 TO WS-H2-RUN-MM.                 EL345
00294      MOVE RUN-DA                 TO WS-H2-RUN-DD.                 EL345
00295      MOVE RUN-YR                 TO WS-H2-RUN-YY.                 EL345
00296      MOVE ALPH-DATE              TO WS-H3-REPORT-DATE.            EL345
00297                                                                   EL345
00298      RETURN SORT-FILE                                             EL345
00299          AT END                                                   EL345
00300              PERFORM 3000-PRINT-HEADINGS                          EL345
00301              MOVE ' NO TRANSACTIONS FOR THIS RUN'                 EL345
00302                                      TO  WS-DETAIL1               EL345
00303              WRITE PRT FROM WS-DETAIL1                            EL345
00304                  AFTER ADVANCING 2 LINES                          EL345
00305              MOVE 'Y'                TO  WS-SORT-EOF-SW.          EL345
00306                                                                   EL345
00307      PERFORM 2100-REPORT-LOOP                                     EL345
00308          UNTIL WS-SORT-EOF-YES.                                   EL345
00309                                                                   EL345
00310      CLOSE REPORT-FILE.                                           EL345
00311                                                                   EL345
00312      EJECT                                                        EL345
00313  2100-REPORT-LOOP                 SECTION.                        EL345
00314                                                                   EL345
00315      IF WS-LINE-COUNT > +55                                       EL345
00316          PERFORM 3000-PRINT-HEADINGS.                             EL345
00317                                                                   EL345
00318      MOVE SR-STATE                TO WS-D1-STATE.                 EL345
00319      MOVE SR-ACCOUNT              TO WS-D1-ACCOUNT-NBR.           EL345
           MOVE SR-INCURRED-DATE        TO WS-SR-INCUR-DATE
00320      MOVE SR-INCUR-MO             TO WS-D1-INCURRED-MO.           EL345
00321      MOVE SR-INCUR-DA             TO WS-D1-INCURRED-DA.           EL345
00322      MOVE SR-INCUR-YR             TO WS-D1-INCURRED-YR.           EL345
           MOVE SR-EFFECTIVE-DATE       TO WS-SR-EFFECTIVE-DATE
00323      MOVE SR-EFFECTIVE-MO         TO WS-D1-EFFECTIVE-MO.          EL345
00324      MOVE SR-EFFECTIVE-DA         TO WS-D1-EFFECTIVE-DA.          EL345
00325      MOVE SR-EFFECTIVE-YR         TO WS-D1-EFFECTIVE-YR.          EL345
00326      MOVE SR-LOAN-TYPE            TO WS-D1-LOAN-TYPE.             EL345
00327      MOVE SR-TRANS                TO WS-D1-TRANS.                 EL345
00328      MOVE SR-CLAIM-NUMBER         TO WS-D1-CLAIM-NBR.             EL345
00329      MOVE SR-BENEFIT-AMT          TO WS-D1-BENEFIT-AMT            EL345
00330      MOVE SR-PAID-AMT             TO WS-D1-PAID-AMT.              EL345
00331                                                                   EL345
00332      WRITE PRT FROM WS-DETAIL1                                    EL345
00333          AFTER ADVANCING 2 LINES.                                 EL345
           ADD +2 TO WS-LINE-COUNT
00334                                                                   EL345
00335      RETURN SORT-FILE                                             EL345
00336          AT END MOVE 'Y' TO WS-SORT-EOF-SW.                       EL345
00337                                                                   EL345
00338      EJECT                                                        EL345
00339  3000-PRINT-HEADINGS          SECTION.                            EL345
00340                                                                   EL345
00341      MOVE +7                     TO WS-LINE-COUNT.                EL345
00342      ADD  +1                     TO WS-PAGE-COUNT.                EL345
00343      MOVE WS-PAGE-COUNT          TO WS-H3-PAGE.                   EL345
00344      MOVE 2                      TO WS-CC.                        EL345
00345                                                                   EL345
00346      WRITE PRT FROM WS-HEADING-1                                  EL345
00347          AFTER ADVANCING TO-TOP-OF-PAGE.                          EL345
00348      WRITE PRT FROM WS-HEADING-2                                  EL345
00349          AFTER ADVANCING 1 LINES.                                 EL345
00350      WRITE PRT FROM WS-HEADING-3                                  EL345
00351          AFTER ADVANCING 1 LINES.                                 EL345
00352      WRITE PRT FROM WS-HEADING-4                                  EL345
00353          AFTER ADVANCING 2 LINES.                                 EL345
00354      WRITE PRT FROM WS-HEADING-5                                  EL345
00355          AFTER ADVANCING 1 LINES.                                 EL345
00356                                                                   EL345
00357  ABEND-PGM                    SECTION.                            EL345
00358      COPY ELCABEND.                                               EL345
00359 *                                                                 EL345
00360 * ** ***  END OF REPORT PROGRAM EL345  *** ** *                   EL345
