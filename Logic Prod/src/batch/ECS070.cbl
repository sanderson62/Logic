00001  IDENTIFICATION DIVISION.                                         01/09/98
00002                                                                   ECS070
00003  PROGRAM-ID.                ECS070.                                  LV002
00004 *              PROGRAM CONVERTED BY                               ECS070
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ECS070
00006 *              CONVERSION DATE 02/08/96 18:26:19.                 ECS070
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          ECS070
00008 *                           VMOD=2.005.                           ECS070
00009                                                                   ECS070
00010 *AUTHOR.     LOGIC, INC.                                          ECS070
00011 *            DALLAS, TEXAS.                                       ECS070
00012                                                                   ECS070
00013 *DATE-COMPILED.                                                   ECS070
00014                                                                   ECS070
00015 *SECURITY.   *****************************************************ECS070
00016 *            *                                                   *ECS070
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ECS070
00018 *            *                                                   *ECS070
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS070
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ECS070
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *ECS070
00022 *            *                                                   *ECS070
00023 *            *****************************************************ECS070
00024                                                                   ECS070
00025 *REMARKS.                                                         ECS070
00026                                                                   ECS070
00027 *    THIS PROGRAM PRINTS THE COMMISSION TABLE FILE CREATED AND    ECS070
00028 *        AND MAINTAINED BY EL653.                                 ECS070
00029                                                                   ECS070
00030  EJECT                                                            ECS070
00031  ENVIRONMENT DIVISION.                                            ECS070
00032  INPUT-OUTPUT SECTION.                                            ECS070
00033  FILE-CONTROL.                                                    ECS070
00034                                                                   ECS070
00035      SELECT COMM-IN        ASSIGN TO SYS010-FBA1-ERCTBL           ECS070
00036              ORGANIZATION IS INDEXED                              ECS070
00037              ACCESS IS SEQUENTIAL                                 ECS070
00038              RECORD KEY IS CT-CONTROL-PRIMARY                     ECS070
00039              FILE STATUS IS COMMISSION-FILE-STATUS.               ECS070
00040                                                                   ECS070
00041      SELECT  PRNTR       ASSIGN TO SYS008-UR-1403-S-SYS008.       ECS070
00042      SELECT  DISK-DATE   ASSIGN TO SYS019-UT-FBA1-S-SYS019.       ECS070
00043      SELECT  FICH        ASSIGN TO SYS020-UT-2400-S-SYS020.       ECS070
00044                                                                   ECS070
00045  EJECT                                                            ECS070
00046  DATA DIVISION.                                                   ECS070
00047  FILE SECTION.                                                    ECS070
00048                                                                   ECS070
00049  FD  COMM-IN.                                                     ECS070
00050                                                                   ECS070
00051                          COPY ERCCTBL.                            ECS070
00052                                                                   ECS070
00053  EJECT                                                            ECS070
00054  FD  DISK-DATE                                                    ECS070
00055                                  COPY ELCDTEFD.                   ECS070
00056  EJECT                                                            ECS070
00057  FD  PRNTR                                                        ECS070
00058                                  COPY ELCPRTFD.                   ECS070
00059  EJECT                                                            ECS070
00060  FD  FICH                                                         ECS070
00061                                  COPY ELCFCHFD.                   ECS070
00062  EJECT                                                            ECS070
00063  WORKING-STORAGE SECTION.                                         ECS070
00064  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      ECS070
00065  77  FILLER  PIC X(32) VALUE '********************************'.  ECS070
00066  77  FILLER  PIC X(32) VALUE '     ECS070 WORKING STORAGE     '.  ECS070
00067  77  FILLER  PIC X(32) VALUE '*****VMOD=2.005*****************'.  ECS070
00068                                                                   ECS070
00069  01  WS-ABEND.                                                    ECS070
00070      12  WS-RETURN-CODE          PIC S9(4)       VALUE ZERO.      ECS070
00071      12  WS-ZERO                 PIC S9          VALUE ZERO.      ECS070
00072      12  WS-ABEND-MESSAGE        PIC X(80)       VALUE SPACES.    ECS070
00073      12  WS-ABEND-FILE-STATUS    PIC XX          VALUE ZERO.      ECS070
00074                                                                   ECS070
00075  01  COMMISSION-FILE-STATUS      PIC XX          VALUE ZERO.      ECS070
00076                                                                   ECS070
00077  EJECT                                                            ECS070
00078  01  HEAD-1.                                                      ECS070
00079      03  FILLER      PIC X(53)   VALUE SPACES.                    ECS070
00080      03  FILLER      PIC X(17)   VALUE 'COMMISSION TABLES'.       ECS070
00081      03  FILLER      PIC X(49)   VALUE SPACES.                    ECS070
00082      03  FILLER      PIC X(8)    VALUE 'ECS070'.                  ECS070
00083                                                                   ECS070
00084  01  HEAD-2.                                                      ECS070
00085      03  FILLER      PIC X(47)   VALUE SPACES.                    ECS070
00086      03  HD-CO       PIC X(30).                                   ECS070
00087      03  FILLER      PIC X(42)   VALUE SPACES.                    ECS070
00088      03  HD-DT       PIC X(8).                                    ECS070
00089                                                                   ECS070
00090  01  HEAD-3.                                                      ECS070
00091      03  FILLER      PIC X(53)   VALUE SPACES.                    ECS070
00092      03  HD-ADT      PIC X(18).                                   ECS070
00093      03  FILLER      PIC X(48)   VALUE SPACES.                    ECS070
00094      03  FILLER      PIC X(5)    VALUE 'PAGE'.                    ECS070
00095      03  HD-PG       PIC ZZ,ZZ9.                                  ECS070
00096                                                                   ECS070
00097  01  HEAD-4.                                                      ECS070
00098      03  FILLER      PIC X(20)   VALUE ' COMMISSION TABLE -'.     ECS070
00099      03  H4-TAB      PIC X(3).                                    ECS070
00100      03  FILLER      PIC X(7)    VALUE SPACE.                     ECS070
00101      03  FILLER      PIC X(15)   VALUE 'BENEFIT TYPE -'.          ECS070
00102      03  H5-TYPE     PIC X(6).                                    ECS070
00103      03  FILLER      PIC X(6)    VALUE SPACE.                     ECS070
00104      03  FILLER      PIC X(15)   VALUE 'BENEFIT CODE -'.          ECS070
00105      03  H6-CODE     PIC XX.                                      ECS070
00106      03  FILLER      PIC X(7)    VALUE SPACE.                     ECS070
00107      03  FILLER      PIC X(22)   VALUE 'BENEFIT DESCRIPTION -'.   ECS070
00108      03  H7-DESC     PIC X(10).                                   ECS070
00109                                                                   ECS070
00110  01  HEAD-8.                                                      ECS070
00111      03  FILLER      PIC X(40)   VALUE                            ECS070
00112        '   BENEFIT    ****** TERMS ******       '.                ECS070
00113      03  FILLER      PIC X(40)   VALUE                            ECS070
00114        '   BENEFIT    ****** TERMS ******       '.                ECS070
00115      03  FILLER      PIC X(40)   VALUE                            ECS070
00116        '   BENEFIT    ****** TERMS ******       '.                ECS070
00117                                                                   ECS070
00118  01  HEAD-9.                                                      ECS070
00119      03  FILLER                  PIC X.                           ECS070
00120      03  H9-X                    OCCURS 3 TIMES.                  ECS070
00121          05  H9-BEN              PIC ZZZZ,ZZZ.ZZ-.                ECS070
00122          05  FILLER              PIC X.                           ECS070
00123          05  H9-TRM1             PIC ZZZ.                         ECS070
00124          05  FILLER              PIC X(5).                        ECS070
00125          05  H9-TRM2             PIC ZZZ.                         ECS070
00126          05  FILLER              PIC X(5).                        ECS070
00127          05  H9-TRM3             PIC ZZZ.                         ECS070
00128          05  FILLER              PIC X(8).                        ECS070
00129                                                                   ECS070
00130  01  HEAD-10.                                                     ECS070
00131      03  FILLER                  PIC X.                           ECS070
00132      03  H10-X                   OCCURS 3 TIMES.                  ECS070
00133          05  H10-CD              PIC X.                           ECS070
00134          05  FILLER              PIC XX.                          ECS070
00135          05  H10-AGE             PIC ZZ.                          ECS070
00136          05  FILLER              PIC X(6).                        ECS070
00137          05  H10-PC1A.                                            ECS070
00138              07  H10-PC1         PIC .ZZZZZ-.                     ECS070
00139          05  FILLER              PIC X.                           ECS070
00140          05  H10-PC2A.                                            ECS070
00141              07  H10-PC2         PIC .ZZZZZ-.                     ECS070
00142          05  FILLER              PIC X.                           ECS070
00143          05  H10-PC3A.                                            ECS070
00144              07  H10-PC3         PIC .ZZZZZ-.                     ECS070
00145          05  FILLER              PIC X(6).                        ECS070
00146                                                                   ECS070
00147  01  MISC.                                                        ECS070
00148      03  X                       PIC X       VALUE SPACE.         ECS070
00149      03  SUB                     PIC S9(3)   VALUE +0   COMP.     ECS070
00150      03  CT-LINE-COUNT           PIC S9(3)   VALUE +99  COMP-3.   ECS070
00151      03  CT-PAGE-COUNT           PIC S9(5)   VALUE +0   COMP-3.   ECS070
00152      03  PGM-SUB                 PIC S999    VALUE +070 COMP.     ECS070
00153      03  X1                      PIC S999    VALUE +0   COMP.     ECS070
00154      03  X2                      PIC S999    VALUE +0   COMP.     ECS070
00155      03  B1                      PIC S999    VALUE +1   COMP.     ECS070
00156                                                                   ECS070
00157                                  COPY ELCDTECX.                   ECS070
00158                                                                   ECS070
00159                                  COPY ELCDTEVR.                   ECS070
00160                                                                   ECS070
00161  EJECT                                                            ECS070
00162  PROCEDURE DIVISION.                                              ECS070
00163                                                                   ECS070
00164  0100-SET-START.                                                  ECS070
00165                                  COPY ELCDTERX.                   ECS070
00166                                                                   ECS070
00167      MOVE COMPANY-NAME           TO HD-CO.                        ECS070
00168      MOVE ALPH-DATE              TO HD-ADT.                       ECS070
00169      MOVE WS-CURRENT-DATE        TO HD-DT.                        ECS070
00170                                                                   ECS070
00171      OPEN INPUT COMM-IN                                           ECS070
00172           OUTPUT PRNTR.                                           ECS070
00173                                                                   ECS070
00174      IF COMMISSION-FILE-STATUS  = '00' OR '97'                    ECS070
00175          NEXT SENTENCE                                            ECS070
00176        ELSE                                                       ECS070
00177          MOVE COMMISSION-FILE-STATUS TO WS-ABEND-FILE-STATUS      ECS070
00178          MOVE 'OPEN ERROR - ERCTBL ' TO WS-ABEND-MESSAGE          ECS070
00179          GO TO ABEND-PGM.                                         ECS070
00180                                                                   ECS070
00181      MOVE LOW-VALUES                 TO CT-CONTROL-PRIMARY.       ECS070
00182      MOVE DTE-CLASIC-COMPANY-CD      TO CT-COMPANY-CD.            ECS070
00183                                                                   ECS070
00184      START COMM-IN KEY IS NOT LESS THAN CT-CONTROL-PRIMARY.       ECS070
00185                                                                   ECS070
00186      IF COMMISSION-FILE-STATUS  = '23'                            ECS070
00187          GO TO 0350-END-OF-JOB.                                   ECS070
00188                                                                   ECS070
00189      IF COMMISSION-FILE-STATUS NOT = ZERO                         ECS070
00190          MOVE COMMISSION-FILE-STATUS TO WS-ABEND-FILE-STATUS      ECS070
00191          MOVE 'START ERROR - ERCTBL ' TO WS-ABEND-MESSAGE         ECS070
00192          GO TO ABEND-PGM.                                         ECS070
00193                                                                   ECS070
00194  EJECT                                                            ECS070
00195  0120-READ-COMM-IN.                                               ECS070
00196                                                                   ECS070
00197      READ COMM-IN.                                                ECS070
00198                                                                   ECS070
00199      IF COMMISSION-FILE-STATUS = '10'                             ECS070
00200          GO TO 0350-END-OF-JOB.                                   ECS070
00201                                                                   ECS070
00202      IF CT-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD                 ECS070
00203          GO TO 0350-END-OF-JOB.                                   ECS070
00204                                                                   ECS070
00205      IF COMMISSION-FILE-STATUS NOT = ZERO                         ECS070
00206          MOVE COMMISSION-FILE-STATUS TO WS-ABEND-FILE-STATUS      ECS070
00207          MOVE 'READ ERROR - ERCTBL ' TO WS-ABEND-MESSAGE          ECS070
00208          GO TO ABEND-PGM.                                         ECS070
00209                                                                   ECS070
00210  0130-PRINT-COMM-TBL.                                             ECS070
00211                                                                   ECS070
00212      PERFORM 0240-OVERFLOW-CHECK THRU 0250-O-C-X.                 ECS070
00213                                                                   ECS070
00214      MOVE SPACE                  TO HEAD-9                        ECS070
00215                                     HEAD-10.                      ECS070
00216                                                                   ECS070
00217      MOVE CT-TABLE               TO H4-TAB.                       ECS070
00218                                                                   ECS070
00219      IF CT-BEN-TYPE = LIFE-OVERRIDE-L1                            ECS070
00220          MOVE LIFE-OVERRIDE-L6   TO H5-TYPE                       ECS070
00221      ELSE                                                         ECS070
00222          MOVE AH-OVERRIDE-L6     TO H5-TYPE.                      ECS070
00223                                                                   ECS070
00224      MOVE CT-BEN-CODE            TO H6-CODE.                      ECS070
00225                                                                   ECS070
00226      PERFORM 0190-GET-DESC THRU 0230-G-D-X.                       ECS070
00227                                                                   ECS070
00228      IF CT-TBF (1) NOT = ZERO                                     ECS070
00229          MOVE CT-TRM (1)         TO H9-TRM1 (1)                   ECS070
00230          MOVE CT-TRM (2)         TO H9-TRM2 (1)                   ECS070
00231          MOVE CT-TRM (3)         TO H9-TRM3 (1)                   ECS070
00232          MOVE CT-TBF (1)         TO H9-BEN (1).                   ECS070
00233                                                                   ECS070
00234      IF CT-TBF (2) NOT = ZERO                                     ECS070
00235          MOVE CT-TRM (1)         TO H9-TRM1 (2)                   ECS070
00236          MOVE CT-TRM (2)         TO H9-TRM2 (2)                   ECS070
00237          MOVE CT-TRM (3)         TO H9-TRM3 (2)                   ECS070
00238          MOVE CT-TBF (2)         TO H9-BEN (2).                   ECS070
00239                                                                   ECS070
00240      IF CT-TBF (3) NOT = ZERO                                     ECS070
00241          MOVE CT-TRM (1)         TO H9-TRM1 (3)                   ECS070
00242          MOVE CT-TRM (2)         TO H9-TRM2 (3)                   ECS070
00243          MOVE CT-TRM (3)         TO H9-TRM3 (3)                   ECS070
00244          MOVE CT-TBF (3)         TO H9-BEN (3).                   ECS070
00245                                                                   ECS070
00246  0140-PARTIAL-PRINT.                                              ECS070
00247      MOVE HEAD-4                 TO P-DATA.                       ECS070
00248      MOVE '0'                    TO P-CTL.                           CL**2
00249      PERFORM 0310-WT-LINE THRU 0330-W-L-X.                        ECS070
00250                                                                   ECS070
00251      MOVE HEAD-8                 TO P-DATA.                       ECS070
00252      MOVE '0'                    TO P-CTL                            CL**2
00253      PERFORM 0310-WT-LINE THRU 0330-W-L-X.                        ECS070
00254                                                                   ECS070
00255      MOVE HEAD-9                 TO P-DATA.                       ECS070
00256      MOVE '-'                    TO P-CTL.                           CL**2
00257      PERFORM 0310-WT-LINE THRU 0330-W-L-X.                        ECS070
00258                                                                   ECS070
00259  0150-BUILD-HEAD-10.                                              ECS070
00260      IF CT-TBF (1) NOT = ZERO                                     ECS070
00261          MOVE 'A'                TO H10-CD (1)                    ECS070
00262          MOVE CT-AGE (1)         TO H10-AGE (1)                   ECS070
00263          MOVE 1                  TO X1                            ECS070
00264          MOVE 1                  TO X2                            ECS070
00265          PERFORM 0160-MOVE-RATE THRU 0170-MOVE-RATE-EXIT.         ECS070
00266                                                                   ECS070
00267      IF CT-TBF (2) NOT = ZERO                                     ECS070
00268          MOVE 'A'                TO H10-CD (2)                    ECS070
00269          MOVE CT-AGE (1)         TO H10-AGE (2)                   ECS070
00270          MOVE 10                 TO X1                            ECS070
00271          MOVE 2                  TO X2                            ECS070
00272          PERFORM 0160-MOVE-RATE THRU 0170-MOVE-RATE-EXIT.         ECS070
00273                                                                   ECS070
00274      IF CT-TBF (3) NOT = ZERO                                     ECS070
00275          MOVE 'A'                TO H10-CD (3)                    ECS070
00276          MOVE CT-AGE (1)         TO H10-AGE (3)                   ECS070
00277          MOVE 19                 TO X1                            ECS070
00278          MOVE 3                  TO X2                            ECS070
00279          PERFORM 0160-MOVE-RATE THRU 0170-MOVE-RATE-EXIT.         ECS070
00280                                                                   ECS070
00281      MOVE HEAD-10                TO P-DATA.                       ECS070
00282      MOVE '0'                    TO P-CTL.                           CL**2
00283      PERFORM 0310-WT-LINE THRU 0330-W-L-X.                        ECS070
00284                                                                   ECS070
00285      MOVE SPACE                  TO HEAD-10.                      ECS070
00286                                                                   ECS070
00287      IF CT-TBF (1) NOT = ZERO                                     ECS070
00288          MOVE 'G'                TO H10-CD (1)                    ECS070
00289          MOVE CT-AGE (2)         TO H10-AGE (1)                   ECS070
00290          MOVE 4                  TO X1                            ECS070
00291          MOVE 1                  TO X2                            ECS070
00292          PERFORM 0160-MOVE-RATE THRU 0170-MOVE-RATE-EXIT.         ECS070
00293                                                                   ECS070
00294      IF CT-TBF (2) NOT = ZERO                                     ECS070
00295          MOVE 'G'                TO H10-CD (2)                    ECS070
00296          MOVE CT-AGE (2)         TO H10-AGE (2)                   ECS070
00297          MOVE 13                 TO X1                            ECS070
00298          MOVE 2                  TO X2                            ECS070
00299          PERFORM 0160-MOVE-RATE THRU 0170-MOVE-RATE-EXIT.         ECS070
00300                                                                   ECS070
00301      IF CT-TBF (3) NOT = ZERO                                     ECS070
00302          MOVE 'G'                TO H10-CD (3)                    ECS070
00303          MOVE CT-AGE (2)         TO H10-AGE (3)                   ECS070
00304          MOVE 22                 TO X1                            ECS070
00305          MOVE 3                  TO X2                            ECS070
00306          PERFORM 0160-MOVE-RATE THRU 0170-MOVE-RATE-EXIT.         ECS070
00307                                                                   ECS070
00308      MOVE HEAD-10                TO P-DATA.                       ECS070
00309      MOVE '-'                    TO P-CTL.                           CL**2
00310      PERFORM 0310-WT-LINE THRU 0330-W-L-X.                        ECS070
00311                                                                   ECS070
00312      MOVE SPACE                  TO HEAD-10.                      ECS070
00313                                                                   ECS070
00314      IF CT-TBF (1) NOT = ZERO                                     ECS070
00315          MOVE 'E'                TO H10-CD (1)                    ECS070
00316          MOVE CT-AGE (3)         TO H10-AGE (1)                   ECS070
00317          MOVE 7                  TO X1                            ECS070
00318          MOVE 1                  TO X2                            ECS070
00319          PERFORM 0160-MOVE-RATE THRU 0170-MOVE-RATE-EXIT.         ECS070
00320                                                                   ECS070
00321      IF CT-TBF (2) NOT = ZERO                                     ECS070
00322          MOVE 'E'                TO H10-CD (2)                    ECS070
00323          MOVE CT-AGE (3)         TO H10-AGE (2)                   ECS070
00324          MOVE 16                 TO X1                            ECS070
00325          MOVE 2                  TO X2                            ECS070
00326          PERFORM 0160-MOVE-RATE THRU 0170-MOVE-RATE-EXIT.         ECS070
00327                                                                   ECS070
00328      IF CT-TBF (3) NOT = ZERO                                     ECS070
00329          MOVE 'E'                TO H10-CD (3)                    ECS070
00330          MOVE CT-AGE (3)         TO H10-AGE (3)                   ECS070
00331          MOVE 25                 TO X1                            ECS070
00332          MOVE 3                  TO X2                            ECS070
00333          PERFORM 0160-MOVE-RATE THRU 0170-MOVE-RATE-EXIT.         ECS070
00334                                                                   ECS070
00335      MOVE HEAD-10                TO P-DATA.                       ECS070
00336      MOVE '-'                    TO P-CTL.                           CL**2
00337      PERFORM 0310-WT-LINE THRU 0330-W-L-X.                        ECS070
00338                                                                   ECS070
00339      PERFORM 0290-ADD-TO-LINE-COUNT THRU 0300-A-T-L-C-X.          ECS070
00340      GO TO 0120-READ-COMM-IN.                                     ECS070
00341                                                                   ECS070
00342  0160-MOVE-RATE.                                                  ECS070
00343      IF CT-RT (X1) NUMERIC                                        ECS070
00344          MOVE CT-RT (X1)         TO H10-PC1 (X2)                  ECS070
00345      ELSE                                                         ECS070
00346          MOVE CT-RTX (X1)        TO H10-PC1A (X2).                ECS070
00347                                                                   ECS070
00348      ADD B1 TO X1.                                                ECS070
00349                                                                   ECS070
00350      IF CT-RT (X1) NUMERIC                                        ECS070
00351          MOVE CT-RT (X1)         TO H10-PC2 (X2)                  ECS070
00352      ELSE                                                         ECS070
00353          MOVE CT-RTX (X1)        TO H10-PC2A (X2).                ECS070
00354                                                                   ECS070
00355      ADD B1 TO X1.                                                ECS070
00356                                                                   ECS070
00357      IF CT-RT (X1) NUMERIC                                        ECS070
00358          MOVE CT-RT (X1)         TO H10-PC3 (X2)                  ECS070
00359      ELSE                                                         ECS070
00360          MOVE CT-RTX (X1)        TO H10-PC3A (X2).                ECS070
00361                                                                   ECS070
00362  0170-MOVE-RATE-EXIT.                                             ECS070
00363      EXIT.                                                        ECS070
00364                                                                   ECS070
00365  EJECT                                                            ECS070
00366  0190-GET-DESC.                                                   ECS070
00367      MOVE CLAS-STARTL            TO CLAS-INDEXL.                  ECS070
00368      MOVE CLAS-STARTA            TO CLAS-INDEXA.                  ECS070
00369                                                                   ECS070
00370      IF CT-BEN-CODE = 'AA'                                        ECS070
00371              MOVE 'ALL'          TO H7-DESC                       ECS070
00372              GO TO 0230-G-D-X.                                    ECS070
00373                                                                   ECS070
00374      IF CT-BEN-TYPE = AH-OVERRIDE-L1                              ECS070
00375         GO TO 0210-GET-AH-DESC.                                   ECS070
00376                                                                   ECS070
00377      IF CLAS-MAXL = ZEROES                                        ECS070
00378         MOVE 'UNKNOWN'           TO H7-DESC                       ECS070
00379         GO TO 0230-G-D-X.                                         ECS070
00380                                                                   ECS070
00381  0200-GET-LIFE-DESC.                                              ECS070
00382      IF CLAS-INDEXL GREATER THAN CLAS-MAXL                        ECS070
00383         MOVE 'UNKNOWN'           TO H7-DESC                       ECS070
00384         GO TO 0230-G-D-X.                                         ECS070
00385                                                                   ECS070
00386      IF CT-BEN-CODE NOT = CLAS-I-BEN (CLAS-INDEXL)                ECS070
00387         ADD 1                    TO CLAS-INDEXL                   ECS070
00388         GO TO 0200-GET-LIFE-DESC.                                 ECS070
00389                                                                   ECS070
00390      MOVE CLAS-I-AB10 (CLAS-INDEXL) TO H7-DESC.                   ECS070
00391      MOVE CLAS-INDEXL            TO SUB.                          ECS070
00392      GO TO 0230-G-D-X.                                            ECS070
00393                                                                   ECS070
00394  0210-GET-AH-DESC.                                                ECS070
00395      IF CLAS-MAXA = ZEROES                                        ECS070
00396         MOVE 'UNKNOWN'           TO H7-DESC                       ECS070
00397         GO TO 0230-G-D-X.                                         ECS070
00398                                                                   ECS070
00399  0220-GET-AH-LOOP.                                                ECS070
00400      IF CLAS-INDEXA GREATER THAN CLAS-MAXA                        ECS070
00401         MOVE 'UNKNOWN'           TO H7-DESC                       ECS070
00402         GO TO 0230-G-D-X.                                         ECS070
00403                                                                   ECS070
00404      IF CT-BEN-CODE NOT = CLAS-I-BEN (CLAS-INDEXA)                ECS070
00405         ADD 1                    TO CLAS-INDEXA                   ECS070
00406         GO TO 0220-GET-AH-LOOP.                                   ECS070
00407                                                                   ECS070
00408      MOVE CLAS-I-AB10 (CLAS-INDEXA) TO H7-DESC.                   ECS070
00409      MOVE CLAS-INDEXA            TO SUB.                          ECS070
00410                                                                   ECS070
00411  0230-G-D-X.                                                      ECS070
00412      EXIT.                                                        ECS070
00413                                                                   ECS070
00414  0240-OVERFLOW-CHECK.                                             ECS070
00415      IF CT-LINE-COUNT GREATER +43                                 ECS070
00416          PERFORM 0270-HEADING-ROUTINE THRU 0280-H-R-X.            ECS070
00417                                                                   ECS070
00418  0250-O-C-X.                                                      ECS070
00419      EXIT.                                                        ECS070
00420                                                                   ECS070
00421  EJECT                                                            ECS070
00422  0270-HEADING-ROUTINE.                                            ECS070
00423      MOVE 0                      TO CT-LINE-COUNT.                ECS070
00424      ADD 1                       TO CT-PAGE-COUNT.                ECS070
00425      MOVE CT-PAGE-COUNT          TO HD-PG.                        ECS070
00426      MOVE HEAD-1                 TO P-DATA.                       ECS070
00427      MOVE '1'                    TO P-CTL.                           CL**2
00428      PERFORM 0310-WT-LINE THRU 0330-W-L-X.                        ECS070
00429      MOVE HEAD-2                 TO P-DATA.                       ECS070
00430      MOVE ' '                    TO P-CTL.                           CL**2
00431      PERFORM 0310-WT-LINE THRU 0330-W-L-X.                        ECS070
00432      MOVE HEAD-3                 TO P-DATA.                       ECS070
00433      MOVE ' '                    TO P-CTL.                           CL**2
00434      PERFORM 0310-WT-LINE THRU 0330-W-L-X.                        ECS070
00435                                                                   ECS070
00436  0280-H-R-X.                                                      ECS070
00437      EXIT.                                                        ECS070
00438                                                                   ECS070
00439  0290-ADD-TO-LINE-COUNT.                                          ECS070
00440      ADD 8                       TO CT-LINE-COUNT.                ECS070
00441                                                                   ECS070
00442  0300-A-T-L-C-X.                                                  ECS070
00443      EXIT.                                                        ECS070
00444                                                                   ECS070
00445  0310-WT-LINE.                                                    ECS070
00446                                                                   ECS070
00447      MOVE P-CTL                  TO X.                            ECS070
00448                                                                   ECS070
00449  0320-WT-LINE-Y.                                                  ECS070
00450                                  COPY ELCPRT2.                    ECS070
00451  0330-W-L-X.                                                      ECS070
00452      EXIT.                                                        ECS070
00453                                                                   ECS070
00454  EJECT                                                            ECS070
00455  0350-END-OF-JOB.                                                 ECS070
00456                                                                   ECS070
00457      CLOSE COMM-IN                                                ECS070
00458            PRNTR.                                                 ECS070
00459                                                                   ECS070
00460  0360-END-OF-JOB-1.                                               ECS070
00461                                  COPY ELCPRTC.                    ECS070
00462                                                                   ECS070
00463      GOBACK.                                                      ECS070
00464                                                                   ECS070
00465  ABEND-PGM SECTION.              COPY ELCABEND.                   ECS070
