00001  IDENTIFICATION DIVISION.                                         10/01/97
00002                                                                   ECS060
00003  PROGRAM-ID.                ECS060.                                  LV001
00004 *              PROGRAM CONVERTED BY                               ECS060
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ECS060
00006 *              CONVERSION DATE 10/03/95 15:54:52.                 ECS060
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          ECS060
00008 *                           VMOD=2.006.                           ECS060
00009                                                                   ECS060
00010 *AUTHOR.        LOGIC, INC.                                       ECS060
00011 *               DALLAS, TEXAS.                                    ECS060
00012                                                                   ECS060
00013 *DATE-COMPILED.                                                   ECS060
00014                                                                   ECS060
00015 *SECURITY.   *****************************************************ECS060
00016 *            *                                                   *ECS060
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ECS060
00018 *            *                                                   *ECS060
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS060
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ECS060
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *ECS060
00022 *            *                                                   *ECS060
00023 *            *****************************************************ECS060
00024                                                                   ECS060
00025 *REMARKS.                                                         ECS060
00026 *        PRINT 1099 SUMMARY FROM THE COMPENSATION MASTER.         ECS060
00027                                                                   ECS060
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
051810* 051810 CR2010042900001   PEMA  SPEARATE CITY AND STATE
00028  ENVIRONMENT DIVISION.                                            ECS060
00029  INPUT-OUTPUT SECTION.                                            ECS060
00030  FILE-CONTROL.                                                    ECS060
00031                                                                   ECS060
00032      SELECT PRNTR            ASSIGN TO SYS008-UR-1403-S-SYS008.   ECS060
00033      SELECT COMM-MSTR-IN     ASSIGN TO SYS010-UT-2400-S-SYS010.   ECS060
00034      SELECT DISK-DATE        ASSIGN TO SYS019-UT-FBA1-S-SYS019.   ECS060
00035      SELECT FICH             ASSIGN TO SYS020-UT-2400-S-SYS020.   ECS060
00036  EJECT                                                            ECS060
00037  DATA DIVISION.                                                   ECS060
00038  FILE SECTION.                                                    ECS060
00039                                                                   ECS060
00040  FD  PRNTR                                                        ECS060
00041                              COPY ELCPRTFD.                       ECS060
00042  EJECT                                                            ECS060
00043  FD  COMM-MSTR-IN                                                 ECS060
00044                              COPY ECSCOIFD.                       ECS060
00045  EJECT                                                            ECS060
00046  FD  DISK-DATE                                                    ECS060
00047                              COPY ELCDTEFD.                       ECS060
00048  EJECT                                                            ECS060
00049  FD  FICH                                                         ECS060
00050                              COPY ELCFCHFD.                       ECS060
00051  EJECT                                                            ECS060
00052  WORKING-STORAGE SECTION.                                         ECS060
00053  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      ECS060
00054  77  FILLER PIC X(32) VALUE '********************************'.   ECS060
00055  77  FILLER PIC X(32) VALUE '*           ECS060             *'.   ECS060
00056  77  FILLER PIC X(32) VALUE '*********** V/M=2.006  *********'.   ECS060
00057                                                                   ECS060
00058  77  PGM-SUB                 PIC S999        VALUE +60  COMP-3.   ECS060
00059  77  LNCTR                   PIC S999        VALUE +66  COMP-3.   ECS060
00060  77  PGCTR                   PIC S9(5)       VALUE +0   COMP-3.   ECS060
00061  77  X                       PIC  X          VALUE ' '.           ECS060
00062  77  SPACE-NP                PIC  X          VALUE '1'.           ECS060
00063  77  SPACE-1                 PIC  X          VALUE ' '.           ECS060
00064  77  SPACE-2                 PIC  X          VALUE '0'.           ECS060
00065  77  SPACE-3                 PIC  X          VALUE '-'.           ECS060
00066  77  SAVE-COMPANY            PIC  X(30)      VALUE SPACES.        ECS060
00067  77  SAVE-TAX                PIC  X(30)      VALUE SPACES.        ECS060
00068  EJECT                                                            ECS060
00069                              COPY ERCCOMP.                        ECS060
00070  EJECT                                                            ECS060
00071  01  COMP-3-WORK     COMP-3.                                      ECS060
00072      12  TOTALS-AREA.                                             ECS060
00073          16  T-COMP          PIC S9(9)V99     VALUE +0.           ECS060
00074          16  T-1099          PIC S9(9)        VALUE +0.           ECS060
00075      12  COMPNY-AREA.                                             ECS060
00076          16  C-COMP          PIC S9(9)V99     VALUE +0.           ECS060
00077          16  C-1099          PIC S9(9)        VALUE +0.           ECS060
00078      12  CARRIR-AREA.                                             ECS060
00079          16  R-COMP          PIC S9(9)V99     VALUE +0.           ECS060
00080          16  R-1099          PIC S9(9)        VALUE +0.           ECS060
00081                                                                   ECS060
00082  01  MISC-WS.                                                     ECS060
00083      12  CUR-SEQ.                                                 ECS060
00084          16  CUR-CARR            PIC  X      VALUE LOW-VALUE.     ECS060
00085          16  CUR-GROUP           PIC  X(6)   VALUE LOW-VALUE.     ECS060
00086      12  WS-RETURN-CODE          PIC S9(4)              COMP.     ECS060
00087      12  WS-ABEND-MESSAGE        PIC  X(80).                      ECS060
00088      12  WS-ABEND-FILE-STATUS    PIC  XX     VALUE ZEROS.         ECS060
00089      12  WS-ZERO                 PIC S9      VALUE ZERO COMP-3.   ECS060
00090  EJECT                                                            ECS060
00091  01  HEADINGS.                                                    ECS060
00092      12  HD1.                                                     ECS060
00093          16  FILLER          PIC  X(49)      VALUE SPACES.        ECS060
00094          16  FILLER          PIC  X(32)      VALUE                ECS060
00095                  'NONEMPLOYEE COMPENSATION SUMMARY'.              ECS060
00096          16  FILLER          PIC  X(39)      VALUE SPACES.        ECS060
00097          16  FILLER          PIC  X(6)       VALUE 'ECS060'.      ECS060
00098      12  HD2.                                                     ECS060
00099          16  FILLER          PIC  X(19)      VALUE                ECS060
00100                  'TAX IDENTIFICATION '.                           ECS060
00101          16  HD-TAX          PIC  X(31)      VALUE SPACES.        ECS060
00102          16  HD-COMPANY      PIC  X(30)      VALUE SPACES.        ECS060
00103          16  FILLER          PIC  X(44)      VALUE SPACES.        ECS060
00104          16  HD-DATE         PIC  X(8)       VALUE SPACES.        ECS060
00105      12  HD3.                                                     ECS060
00106          16  FILLER          PIC  X(57)      VALUE SPACES.        ECS060
00107          16  HD-ALPH         PIC  X(18)      VALUE SPACES.        ECS060
00108          16  FILLER          PIC  X(38)      VALUE SPACES.        ECS060
00109          16  FILLER          PIC  X(5)       VALUE 'PAGE '.       ECS060
00110          16  HD-PAGE         PIC  ZZZZZ-.                         ECS060
00111          16  FILLER          PIC  X(8)       VALUE SPACES.        ECS060
00112      12  HD4.                                                     ECS060
00113          16  FILLER          PIC  X(44)      VALUE                ECS060
00114                  '                                            '.  ECS060
00115          16  FILLER          PIC  X(44)      VALUE                ECS060
00116                  '                                            '.  ECS060
00117          16  FILLER          PIC  X(44)      VALUE                ECS060
00118                  '            TAX            YEAR TO DATE     '.  ECS060
00119      12  HD5.                                                     ECS060
00120          16  FILLER          PIC  X(44)      VALUE                ECS060
00121                  'CAR  GRP FINANCIAL/RESP                 ACCO'.  ECS060
00122          16  FILLER          PIC  X(44)      VALUE                ECS060
00123                  'UNT NAME                 MAILING ADDRESS    '.  ECS060
00124          16  FILLER          PIC  X(44)      VALUE                ECS060
00125                  '       IDENTIFICATION      COMPENSATION     '.  ECS060
00126  EJECT                                                            ECS060
00127  01  P-REC.                                                       ECS060
00128      12  P-CCSW              PIC  X.                              ECS060
00129      12  P-LINE.                                                  ECS060
00130          16  P-CARR          PIC  X.                              ECS060
00131          16  P-DSH1          PIC  X.                              ECS060
00132          16  P-GROUP         PIC  X(6).                           ECS060
00133          16  P-DSH2          PIC  X.                              ECS060
00134          16  P-RESP          PIC  X(10).                          ECS060
00135          16  P-DSH3          PIC  X.                              ECS060
00136          16  P-ACCT          PIC  X(10).                          ECS060
00137          16  FILLER          PIC  X.                              ECS060
00138          16  P-NAME          PIC  X(30).                          ECS060
00139          16  FILLER          PIC  X.                              ECS060
00140          16  P-ADDR.                                              ECS060
00141              20  FILLER      PIC  X(21).                          ECS060
00142              20  P-ZIP       PIC  X(9).                           ECS060
00143          16  FILLER          PIC  X.                              ECS060
00144          16  P-TAX.                                               ECS060
00145              20  FILLER      PIC  X(3).                           ECS060
00146              20  P-1099      PIC ZZZ,ZZZ,ZZZ,ZZ9-.                ECS060
00147          16  FILLER          PIC  X.                              ECS060
00148          16  P-AMT           PIC Z,ZZZ,ZZZ,ZZZ.99-.               ECS060
00149          16  FILLER          PIC  X.                              ECS060
00150          16  P-AST           PIC  X.                              ECS060
00151  EJECT                                                            ECS060
00152                              COPY ELCDTECX.                       ECS060
00153  EJECT                                                            ECS060
00154                              COPY ELCDTEVR.                       ECS060
00155  EJECT                                                            ECS060
00156  PROCEDURE DIVISION.                                              ECS060
00157                                                                   ECS060
00158  0000-STANDARD-COPY.                                              ECS060
00159                              COPY ELCDTERX.                       ECS060
00160                                                                   ECS060
00161      MOVE COMPANY-NAME           TO  HD-COMPANY  SAVE-COMPANY.    ECS060
00162      MOVE CNT-NAME (6)           TO  SAVE-TAX  HD-TAX.            ECS060
00163      MOVE WS-CURRENT-DATE        TO  HD-DATE.                     ECS060
00164      MOVE ALPH-DATE              TO  HD-ALPH.                     ECS060
00165                                                                   ECS060
00166      GO TO 0100-OPEN-RTN.                                         ECS060
00167  EJECT                                                            ECS060
00168  0100-OPEN-RTN.                                                   ECS060
00169      OPEN INPUT  COMM-MSTR-IN                                     ECS060
00170           OUTPUT PRNTR.                                           ECS060
00171                                                                   ECS060
00172      MOVE SPACE-NP               TO  P-REC.                       ECS060
00173                                                                   ECS060
00174  0110-READ-RTN.                                                   ECS060
00175      READ COMM-MSTR-IN                                            ECS060
00176          AT END                                                   ECS060
00177              GO TO 0600-END-OF-JOB.                               ECS060
00178                                                                   ECS060
00179      MOVE COMP-IN-RECORD         TO  COMPENSATION-MASTER.         ECS060
00180                                                                   ECS060
00181      IF CO-CARR-GROUP NOT = CUR-SEQ                               ECS060
00182          PERFORM 0300-BREAK-RTN  THRU  0399-EXIT.                 ECS060
00183                                                                   ECS060
00184      IF CO-COMPANY-TYPE                                           ECS060
00185          PERFORM 0200-SET-REMIT-RTN  THRU  0299-EXIT              ECS060
00186          GO TO 0110-READ-RTN.                                     ECS060
00187                                                                   ECS060
00188      ADD CO-YTD-OV               TO  CO-YTD-COM.                  ECS060
00189                                                                   ECS060
00190      IF DTE-CLIENT = 'UAL'                                        ECS060
00191          IF CO-YTD-COM = +0.00                                    ECS060
00192              GO TO 0110-READ-RTN.                                 ECS060
00193                                                                   ECS060
00194                                                                   ECS060
00195      IF DTE-TOT-OPT = 2                                           ECS060
00196          IF CO-YTD-COM GREATER +599.99                            ECS060
00197              ADD CO-YTD-COM      TO  C-COMP                       ECS060
00198              ADD +1              TO  C-1099                       ECS060
00199          ELSE                                                     ECS060
00200              NEXT SENTENCE                                        ECS060
00201      ELSE                                                         ECS060
00202          IF CO-YTD-COM GREATER +0                                 ECS060
00203              ADD CO-YTD-COM      TO  C-COMP                       ECS060
00204              ADD +1              TO  C-1099.                      ECS060
00205                                                                   ECS060
00206  0120-PRINT-LINE.                                                 ECS060
00207      IF LNCTR GREATER +051                                        ECS060
00208          PERFORM 0400-HD-RTN  THRU  0499-EXIT.                    ECS060
00209                                                                   ECS060
00210      MOVE SPACE-2                TO  P-CCSW.                      ECS060
00211      MOVE CO-CARRIER             TO  P-CARR.                      ECS060
00212      MOVE '-'                    TO  P-DSH1.                      ECS060
00213      MOVE CO-GROUPING            TO  P-GROUP.                     ECS060
00214      MOVE '-'                    TO  P-DSH2.                      ECS060
00215      MOVE CO-RESP-NO             TO  P-RESP.                      ECS060
00216                                                                   ECS060
00217      IF CO-ACCOUNT NOT = LOW-VALUES                               ECS060
00218          MOVE '-'                TO  P-DSH3                       ECS060
00219          MOVE CO-ACCOUNT         TO  P-ACCT.                      ECS060
00220                                                                   ECS060
00221      MOVE CO-ACCT-NAME           TO  P-NAME.                      ECS060
00222      MOVE CO-MAIL-NAME           TO  P-ADDR.                      ECS060
00223      MOVE CO-SOC-SEC             TO  P-TAX.                       ECS060
00224      MOVE CO-YTD-COM             TO  P-AMT.                       ECS060
00225                                                                   ECS060
00226      IF DTE-TOT-OPT = 2                                           ECS060
00227          IF CO-YTD-COM GREATER +599                               ECS060
00228              MOVE '*'            TO  P-AST                        ECS060
00229          ELSE                                                     ECS060
00230              NEXT SENTENCE                                        ECS060
00231      ELSE                                                         ECS060
00232          IF CO-YTD-COM GREATER +0                                 ECS060
00233              MOVE '*'            TO  P-AST.                       ECS060
00234                                                                   ECS060
00235      PERFORM 0500-PRT-RTN  THRU  0599-EXIT.                       ECS060
00236                                                                   ECS060
00237      IF CO-ADDR-1 NOT = SPACES                                    ECS060
00238          MOVE CO-ADDR-1          TO  P-ADDR.                      ECS060
00239                                                                   ECS060
00240      IF CO-ADDR-2 NOT = SPACES                                    ECS060
00241          PERFORM 0500-PRT-RTN  THRU  0599-EXIT                    ECS060
00242          MOVE CO-ADDR-2          TO  P-ADDR.                      ECS060
00243                                                                   ECS060
00244      IF CO-ADDR-3 NOT = SPACES                                    ECS060
00245          PERFORM 0500-PRT-RTN  THRU  0599-EXIT                    ECS060
051810         MOVE SPACES             TO P-ADDR
051810         STRING CO-ADDR-CITY ' ' CO-ADDR-STATE
051810            DELIMITED BY '  ' INTO P-ADDR
051810         END-STRING.
00247                                                                   ECS060
00248      MOVE CO-ZIP                 TO  P-ZIP.                       ECS060
00249                                                                   ECS060
00250      PERFORM 0500-PRT-RTN  THRU  0599-EXIT.                       ECS060
00251                                                                   ECS060
00252      GO TO 0110-READ-RTN.                                         ECS060
00253  EJECT                                                            ECS060
00254  0200-SET-REMIT-RTN.                                              ECS060
00255      MOVE CO-ACCT-NAME           TO  SAVE-COMPANY.                ECS060
00256      MOVE CO-SOC-SEC             TO  SAVE-TAX.                    ECS060
00257      MOVE +066                   TO  LNCTR.                       ECS060
00258                                                                   ECS060
00259  0299-EXIT.                                                       ECS060
00260      EXIT.                                                        ECS060
00261  EJECT                                                            ECS060
00262  0300-BREAK-RTN.                                                  ECS060
00263      IF CUR-SEQ = LOW-VALUES                                      ECS060
00264          GO TO 0310-INIT-BREAK.                                   ECS060
00265                                                                   ECS060
00266      IF LNCTR GREATER +060                                        ECS060
00267          PERFORM 0400-HD-RTN  THRU  0499-EXIT.                    ECS060
00268                                                                   ECS060
00269      MOVE SPACE-2                TO  P-CCSW.                      ECS060
00270      MOVE CUR-CARR               TO  P-CARR.                      ECS060
00271      MOVE CUR-GROUP              TO  P-GROUP.                     ECS060
00272      MOVE '-'                    TO  P-DSH1.                      ECS060
00273      MOVE 'TOTALS '              TO  P-ADDR.                      ECS060
00274      MOVE C-1099                 TO  P-1099.                      ECS060
00275      MOVE C-COMP                 TO  P-AMT.                       ECS060
00276                                                                   ECS060
00277      PERFORM 0500-PRT-RTN  THRU  0599-EXIT.                       ECS060
00278                                                                   ECS060
00279      ADD C-1099                  TO  R-1099.                      ECS060
00280      ADD C-COMP                  TO  R-COMP.                      ECS060
00281                                                                   ECS060
00282      MOVE CO-GROUPING            TO  CUR-GROUP.                   ECS060
00283      MOVE +0                     TO  C-1099  C-COMP.              ECS060
00284                                                                   ECS060
00285      IF CUR-SEQ = CO-CARR-GROUP                                   ECS060
00286          GO TO 0310-INIT-BREAK.                                   ECS060
00287                                                                   ECS060
00288      PERFORM 0400-HD-RTN  THRU  0499-EXIT.                        ECS060
00289                                                                   ECS060
00290      MOVE SPACE-2                TO  P-CCSW.                      ECS060
00291      MOVE CUR-CARR               TO  P-CARR.                      ECS060
00292      MOVE 'CARRIER TOTALS '      TO  P-ADDR.                      ECS060
00293      MOVE R-1099                 TO  P-1099.                      ECS060
00294      MOVE R-COMP                 TO  P-AMT.                       ECS060
00295                                                                   ECS060
00296      PERFORM 0500-PRT-RTN  THRU  0599-EXIT.                       ECS060
00297                                                                   ECS060
00298      ADD R-1099                  TO  T-1099.                      ECS060
00299      ADD R-COMP                  TO  T-COMP.                      ECS060
00300                                                                   ECS060
00301      MOVE +0                     TO  R-1099  R-COMP.              ECS060
00302      MOVE CO-CARRIER             TO  CUR-CARR.                    ECS060
00303                                                                   ECS060
00304      IF CUR-SEQ NOT = HIGH-VALUES                                 ECS060
00305          GO TO 0310-INIT-BREAK.                                   ECS060
00306                                                                   ECS060
00307      MOVE 'FINAL TOTALS'         TO  P-ADDR.                      ECS060
00308      MOVE T-1099                 TO  P-1099.                      ECS060
00309      MOVE T-COMP                 TO  P-AMT.                       ECS060
00310      MOVE SPACE-2                TO  P-CCSW.                      ECS060
00311                                                                   ECS060
00312      PERFORM 0500-PRT-RTN  THRU  0599-EXIT.                       ECS060
00313                                                                   ECS060
00314      GO TO 0399-EXIT.                                             ECS060
00315                                                                   ECS060
00316  0310-INIT-BREAK.                                                 ECS060
00317      MOVE CO-CARR-GROUP          TO  CUR-SEQ.                     ECS060
00318      MOVE COMPANY-NAME           TO  SAVE-COMPANY.                ECS060
00319      MOVE CNT-NAME (6)           TO  SAVE-TAX.                    ECS060
00320      MOVE +066                   TO  LNCTR.                       ECS060
00321                                                                   ECS060
00322  0399-EXIT.                                                       ECS060
00323      EXIT.                                                        ECS060
00324  EJECT                                                            ECS060
00325  0400-HD-RTN.                                                     ECS060
00326      MOVE SAVE-COMPANY           TO  HD-COMPANY.                  ECS060
00327      MOVE SAVE-TAX               TO  HD-TAX.                      ECS060
00328      MOVE SPACE-NP               TO  P-CCSW.                      ECS060
00329      MOVE HD1                    TO  P-LINE.                      ECS060
00330                                                                   ECS060
00331      PERFORM 0500-PRT-RTN  THRU  0599-EXIT.                       ECS060
00332                                                                   ECS060
00333      ADD +1                      TO  PGCTR.                       ECS060
00334                                                                   ECS060
00335      MOVE PGCTR                  TO  HD-PAGE.                     ECS060
00336      MOVE HD2                    TO  P-LINE.                      ECS060
00337                                                                   ECS060
00338      PERFORM 0500-PRT-RTN  THRU  0599-EXIT.                       ECS060
00339                                                                   ECS060
00340      MOVE HD3                    TO  P-LINE.                      ECS060
00341                                                                   ECS060
00342      PERFORM 0500-PRT-RTN  THRU  0599-EXIT.                       ECS060
00343                                                                   ECS060
00344      MOVE HD4                    TO  P-LINE.                      ECS060
00345      MOVE SPACE-2                TO  P-CCSW.                      ECS060
00346                                                                   ECS060
00347      PERFORM 0500-PRT-RTN  THRU  0599-EXIT.                       ECS060
00348                                                                   ECS060
00349      MOVE HD5                    TO  P-LINE.                      ECS060
00350                                                                   ECS060
00351      PERFORM 0500-PRT-RTN  THRU  0599-EXIT.                       ECS060
00352                                                                   ECS060
00353      MOVE SPACE-2 TO P-CCSW.                                      ECS060
00354                                                                   ECS060
00355  0499-EXIT.                                                       ECS060
00356      EXIT.                                                        ECS060
00357                                                                   ECS060
00358  0500-PRT-RTN.                                                    ECS060
00359      MOVE P-CCSW                 TO  X.                           ECS060
00360      MOVE P-REC                  TO  PRT.                         ECS060
00361                                                                   ECS060
00362      IF P-CCSW = SPACE-1                                          ECS060
00363          ADD +1                  TO  LNCTR                        ECS060
00364      ELSE                                                         ECS060
00365          IF P-CCSW = SPACE-2                                      ECS060
00366              ADD +2              TO  LNCTR                        ECS060
00367          ELSE                                                     ECS060
00368              IF P-CCSW = SPACE-3                                  ECS060
00369                  ADD +3          TO  LNCTR                        ECS060
00370              ELSE                                                 ECS060
00371                  IF P-CCSW = SPACE-NP                             ECS060
00372                      MOVE +0     TO  LNCTR.                       ECS060
00373                                                                   ECS060
00374      MOVE SPACE-1                TO  P-REC.                       ECS060
00375                                                                   ECS060
00376  0510-PRT-COPY.                                                   ECS060
00377                              COPY ELCPRT2.                        ECS060
00378                                                                   ECS060
00379  0599-EXIT.                                                       ECS060
00380      EXIT.                                                        ECS060
00381  EJECT                                                            ECS060
00382  0600-END-OF-JOB.                                                 ECS060
00383      MOVE HIGH-VALUE             TO  CO-CONTROL-PRIMARY.          ECS060
00384                                                                   ECS060
00385      PERFORM 0300-BREAK-RTN  THRU  0399-EXIT.                     ECS060
00386                                                                   ECS060
00387      CLOSE COMM-MSTR-IN PRNTR.                                    ECS060
00388                                                                   ECS060
00389  0610-COPY-CLOSE.                                                 ECS060
00390                              COPY ELCPRTC.                        ECS060
00391                                                                   ECS060
00392  0620-STOP-RUN.                                                   ECS060
00393      GOBACK.                                                      ECS060
00394                                                                   ECS060
00395  ABEND-PGM SECTION.                                               ECS060
00396                              COPY ELCABEND.                       ECS060
