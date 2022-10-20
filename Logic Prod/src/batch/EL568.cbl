00001  IDENTIFICATION DIVISION.                                         03/19/98
00002                                                                   EL568
00003  PROGRAM-ID.                 EL568 .                                 LV003
00004 *              PROGRAM CONVERTED BY                               EL568
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL568
00006 *              CONVERSION DATE 02/12/96 16:51:48.                 EL568
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            EL568
00008 *                            VMOD=2.004.                          EL568
00009                                                                   EL568
00010 *AUTHOR.        LOGIC, INC.                                       EL568
00011 *               DALLAS, TEXAS.                                    EL568
00012                                                                   EL568
00013 *DATE-COMPILED.                                                   EL568
00014                                                                   EL568
00015 *SECURITY.   *****************************************************EL568
00016 *            *                                                   *EL568
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *EL568
00018 *            *                                                   *EL568
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL568
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL568
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *EL568
00022 *            *                                                   *EL568
00023 *            *****************************************************EL568
00024                                                                   EL568
00025                                                                   EL568
00026 ******************************************************************EL568
00027 *        THIS PROGRAM CREATES  PENDING NET PREMIUM REPORTS       *EL568
00028 *        BY GENERAL AGENT AND STATE FOR WDS.                     *EL568
00029 ******************************************************************EL568
00030                                                                   EL568
00031  ENVIRONMENT DIVISION.                                            EL568
00032  INPUT-OUTPUT SECTION.                                            EL568
00033  FILE-CONTROL.                                                    EL568
00034                                                                   EL568
00035      SELECT  SORT-FILE       ASSIGN TO SYS001-UT-FBA1-S-SORTWK1.  EL568
00036                                                                   EL568
00037      SELECT  ELREPT          ASSIGN TO SYS015-FBA1-ELREPT         EL568
00038                              ORGANIZATION IS INDEXED              EL568
00039                              ACCESS IS DYNAMIC                    EL568
00040                              RECORD KEY IS RF-CONTROL-PRIMARY     EL568
00041                              FILE STATUS IS DTE-VSAM-FLAGS.       EL568
00042                                                                   EL568
00043      SELECT  ERPNDB          ASSIGN TO SYS011-FBA1-ERPNDB         EL568
00044                              ORGANIZATION IS INDEXED              EL568
00045                              ACCESS IS DYNAMIC                    EL568
00046                              RECORD KEY IS PB-CONTROL-PRIMARY     EL568
00047                            ALTERNATE KEY IS PB-CONTROL-BY-ACCOUNT EL568
00048                              FILE STATUS IS ERPNDB-FILE-STATUS.   EL568
00049                                                                   EL568
00050      SELECT  ERACCT          ASSIGN TO SYS012-FBA1-ERACCT         EL568
00051                              ORGANIZATION IS INDEXED              EL568
00052                              ACCESS IS DYNAMIC                    EL568
00053                              RECORD KEY IS AM-CONTROL-PRIMARY     EL568
00054                              FILE STATUS IS ERACCT-FILE-STATUS.   EL568
00055                                                                   EL568
00056      SELECT  ERCOMP          ASSIGN TO SYS013-FBA1-ERCOMP         EL568
00057                              ORGANIZATION IS INDEXED              EL568
00058                              ACCESS IS DYNAMIC                    EL568
00059                              RECORD KEY IS CO-CONTROL-PRIMARY     EL568
00060                              FILE STATUS IS ERCOMP-FILE-STATUS.   EL568
00061                                                                   EL568
00062      SELECT  TEMP-FILE       ASSIGN TO SYS014-UT-FBA1-S-SYS014.   EL568
00063                                                                   EL568
00064      SELECT  DISK-DATE       ASSIGN TO SYS019-UT-FBA1-S-SYS019.   EL568
00065                                                                   EL568
00066      SELECT  FICH            ASSIGN TO SYS020-UT-2400-S-SYS020.   EL568
00067                                                                   EL568
00068      SELECT  PRNTR           ASSIGN TO SYS008-UR-1403-S-SYS008.   EL568
00069                                                                   EL568
00070  EJECT                                                            EL568
00071  DATA DIVISION.                                                   EL568
00072  FILE SECTION.                                                    EL568
00073                                                                   EL568
00074  SD  SORT-FILE.                                                      CL**3
00075                                                                      CL**3
00076  01  SORT-REC-1.                                                  EL568
00077      12  FILLER                       PIC X(9).                   EL568
00078      12  SORT-CNTL-1B                 PIC X(10).                  EL568
00079      12  FILLER                       PIC X(30).                  EL568
00080      12  SORT-CNTL-1                  PIC X(10).                  EL568
00081      12  FILLER                       PIC X(41).                  EL568
00082                                                                   EL568
00083  01  SORT-REC-2.                                                  EL568
00084      12  FILLER                       PIC X(67).                  EL568
00085      12  SORT-CNTL-2                  PIC X(20).                  EL568
00086      12  FILLER                       PIC X(13).                  EL568
00087                                                                   EL568
00088  EJECT                                                            EL568
00089  FD  ELREPT                                                       EL568
00090                                       COPY ELCRPTFD.              EL568
00091                                       COPY ELCREPT.               EL568
00092                                                                   EL568
00093  EJECT                                                            EL568
00094  FD  ERPNDB.                                                      EL568
00095                                       COPY ERCPNDB.               EL568
00096                                                                   EL568
00097  EJECT                                                            EL568
00098  FD  ERACCT.                                                      EL568
00099                                       COPY ERCACCT.               EL568
00100                                                                   EL568
00101  EJECT                                                            EL568
00102  FD  ERCOMP.                                                      EL568
00103                                       COPY ERCCOMP.               EL568
00104                                                                   EL568
00105  EJECT                                                            EL568
00106  FD  TEMP-FILE                                                    EL568
00107      BLOCK CONTAINS 0 RECORDS
00108                                .                                  EL568
00109                                                                   EL568
00110  01  TEMP-REC                         PIC X(100).                 EL568
00111                                                                   EL568
00112  EJECT                                                            EL568
00113  FD  FICH                                                         EL568
00114                              COPY ELCFCHFD.                       EL568
00115  EJECT                                                            EL568
00116  FD  PRNTR                                                        EL568
00117                              COPY ELCPRTFD.                       EL568
00118  EJECT                                                            EL568
00119  FD  DISK-DATE                                                    EL568
00120                              COPY ELCDTEFD.                       EL568
00121  EJECT                                                            EL568
00122  WORKING-STORAGE SECTION.                                         EL568
00123  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      EL568
00124  77  FILLER  PIC X(32) VALUE '********************************'.  EL568
00125  77  FILLER  PIC X(32) VALUE '     EL568  WORKING STORAGE    '.   EL568
00126  77  FILLER  PIC X(32) VALUE '*****VMOD=2.004 ****************'.  EL568
00127                                                                   EL568
00128  77  PGM-SUB                         PIC S9(4)  COMP  VALUE +568. EL568
00129  77  ERPNDB-FILE-STATUS              PIC XX       VALUE ZEROS.    EL568
00130  77  ERACCT-FILE-STATUS              PIC XX       VALUE ZEROS.    EL568
00131  77  ERCOMP-FILE-STATUS              PIC XX       VALUE ZEROS.    EL568
00132  77  WS-PAGE-NO                      PIC 999      VALUE ZEROS.    EL568
00133  77  WS-LN-CNT                       PIC 999      VALUE 99.       EL568
00134  77  X                               PIC X        VALUE SPACE.    EL568
00135  77  FIRST-READ                      PIC X        VALUE 'Y'.      EL568
00136                                                                   EL568
00137  01  WS-ABEND.                                                    EL568
00138      12  WS-RETURN-CODE                 PIC S9(4)    VALUE ZEROS. EL568
00139      12  WS-ZERO                        PIC S9       VALUE ZEROS. EL568
00140      12  WS-ABEND-MESSAGE               PIC X(80)    VALUE SPACES.EL568
00141      12  WS-ABEND-FILE-STATUS           PIC XX       VALUE ZEROS. EL568
00142                                                                   EL568
00143                                                                   EL568
00144  01  WS-MISC-FIELDS.                                              EL568
00145      12  OLC-REPORT-NAME                PIC X(5)  VALUE 'EL568'.  EL568
00146      12  TEMP-EOF-SW                    PIC X     VALUE 'N'.      EL568
00147      12  PREV-AGENT                     PIC X(10) VALUE SPACES.   EL568
00148      12  PREV-STATE-NAME                PIC X(20) VALUE SPACES.   EL568
00149      12  PREV-STATE                     PIC XX    VALUE SPACES.   EL568
00150      12  SAVE-EXP-DT                    PIC XX   VALUE LOW-VALUES.EL568
00151      12  SAVE-PB-VG-DATA.                                         EL568
00152          16  VG-CARRIER                 PIC X.                    EL568
00153          16  VG-GROUPING                PIC X(6).                 EL568
00154          16  VG-STATE                   PIC XX.                   EL568
00155          16  VG-ACCOUNT                 PIC X(10).                EL568
00156                                                                   EL568
00157  01  WS-MISC-COMP3          COMP-3.                               EL568
00158      12  WK-NET-PREM                    PIC S9(9)V99 VALUE +0.    EL568
00159      12  WK-GA-PREM                     PIC S9(9)V99 VALUE +0.    EL568
00160      12  WK-TOTAL-PREM                  PIC S9(9)V99 VALUE +0.    EL568
00161      12  A-NDX                          PIC S999     VALUE +0.    EL568
00162                                                                   EL568
00163  01  TEMP-WORK-FILE.                                              EL568
00164      12  TW-CARRIER                     PIC X.                    EL568
00165      12  TW-GROUPING                    PIC X(6).                 EL568
00166      12  TW-STATE                       PIC XX.                   EL568
00167      12  TW-ACCOUNT                     PIC X(10).                EL568
00168      12  TW-ACCOUNT-NAME                PIC X(30).                EL568
00169      12  TW-GEN-AGENT                   PIC X(10).                EL568
00170      12  TW-NET-PREM                    PIC S9(9)V99  COMP-3.     EL568
00171      12  TW-EOM-DT                      PIC XX.                   EL568
00172      12  TW-STATE-NAME                  PIC X(20).                EL568
00173      12  FILLER                         PIC X(13).                EL568
00174                                                                   EL568
00175  01  PRT-HD1.                                                     EL568
00176      12  FILLER                        PIC X(30)    VALUE SPACES. EL568
00177      12  FILLER                        PIC X(20)    VALUE         EL568
00178                    'NET PREMIUM REPORTED'.                        EL568
00179      12  FILLER                        PIC X(22)    VALUE SPACES. EL568
00180      12  FILLER                        PIC X(6)     VALUE         EL568
00181                    'EL568-'.                                      EL568
00182      12  HD1-RPT-CODE                  PIC X.                     EL568
00183                                                                   EL568
00184  01  PRT-HD2.                                                     EL568
00185      12  FILLER                        PIC X(32)    VALUE SPACES. EL568
00186      12  HD2-RPT-NAME                  PIC X(18).                 EL568
00187      12  FILLER                        PIC X(21)    VALUE SPACES. EL568
00188      12  HD2-DATE                      PIC X(8).                  EL568
00189                                                                   EL568
00190  01  PRT-HD3.                                                     EL568
00191      12  FILLER                        PIC X(31)    VALUE SPACES. EL568
00192      12  HD3-ALPHA-DATE                PIC X(18).                 EL568
00193      12  FILLER                        PIC X(22)    VALUE SPACES. EL568
00194      12  FILLER                        PIC X(5)     VALUE         EL568
00195                     'PAGE '.                                      EL568
00196      12  HD3-PG-NO                     PIC ZZ9.                   EL568
00197                                                                   EL568
00198  01  PRT-HD4-A.                                                   EL568
00199      12  FILLER                        PIC X(7)     VALUE         EL568
00200                     'ACCOUNT'.                                    EL568
00201      12  FILLER                        PIC X(30)    VALUE         EL568
00202                     '                              '.             EL568
00203      12  FILLER                        PIC X(30)    VALUE         EL568
00204                     '         MONTH-END            '.             EL568
00205      12  FILLER                        PIC X(30)    VALUE         EL568
00206                     ' NET PREMIUM                  '.             EL568
00207      12  FILLER                        PIC X(35)    VALUE SPACES. EL568
00208                                                                   EL568
00209  01  PRT-HD4-2.                                                   EL568
00210      12  FILLER                        PIC X(15)    VALUE SPACES. EL568
00211      12  FILLER                        PIC X(30)    VALUE         EL568
00212                     'STATE                         '.             EL568
00213      12  FILLER                        PIC X(30)    VALUE         EL568
00214                     '     NET PREMIUM              '.             EL568
00215      12  FILLER                        PIC X(42)    VALUE SPACES. EL568
00216                                                                   EL568
00217  01  PRT-HD5-A.                                                   EL568
00218      12  FILLER                        PIC X(20)    VALUE         EL568
00219                     'NUMBER        ACCOUN'.                       EL568
00220      12  FILLER                        PIC X(19)    VALUE         EL568
00221                     'T NAME             '.                        EL568
00222      12  FILLER                        PIC X(20)    VALUE         EL568
00223                     '         DATE       '.                       EL568
00224      12  FILLER                        PIC X(21)    VALUE         EL568
00225                     '           REPORTED  '.                      EL568
00226                                                                   EL568
00227  01  PRT-HD4-B.                                                   EL568
00228      12  FILLER                        PIC X(20)    VALUE         EL568
00229                     '             STATE  '.                       EL568
00230      12  FILLER                        PIC X(20)    VALUE         EL568
00231                     '             NET PRE'.                       EL568
00232      12  FILLER                        PIC X(20)    VALUE         EL568
00233                     'MIUM                '.                       EL568
00234      12  FILLER                        PIC X(20)    VALUE         EL568
00235                     '                    '.                       EL568
00236                                                                   EL568
00237  01  PRT-GA-HD.                                                   EL568
00238      12  GA-NUMBER                     PIC X(10)    VALUE SPACES. EL568
00239      12  FILLER                        PIC X(4).                  EL568
00240      12  GA-NAME                       PIC X(40).                 EL568
00241                                                                   EL568
00242  01  PRT-DETAIL.                                                  EL568
00243      12  PRT-ACCOUNT                   PIC X(10).                 EL568
00244      12  FILLER                        PIC XX       VALUE SPACES. EL568
00245      12  PRT-ACCT-NAME                 PIC X(30).                 EL568
00246      12  FILLER                        PIC X(4)     VALUE SPACES. EL568
00247      12  PRT-EOM-DATE                  PIC X(8).                  EL568
00248      12  FILLER                        PIC X(11)    VALUE SPACES. EL568
00249      12  PRT-PREM-AMT                  PIC ZZ,ZZZ,ZZZ.ZZ-.        EL568
00250                                                                   EL568
00251  01  PRT-DETAIL-2.                                                EL568
00252      12  FILLER                        PIC X(13)    VALUE SPACES. EL568
00253      12  PRT-STATE                     PIC X(20).                 EL568
00254      12  FILLER                        PIC X(15)    VALUE SPACES. EL568
00255      12  PRT-PREM-AMT-2                PIC ZZ,ZZZ,ZZZ.ZZ-.        EL568
00256                                                                   EL568
00257  01  TOTAL-LINE.                                                  EL568
00258      12  FILLER                        PIC X(40)   VALUE SPACES.  EL568
00259      12  TL-OVERALL                    PIC X(10).                 EL568
00260      12  FILLER                        PIC X(14)   VALUE          EL568
00261                 '      TOTAL   '.                                 EL568
00262      12  TL-AMT                        PIC $ZZ,ZZZ,ZZ9.99-.       EL568
00263                                                                   EL568
00264  01  TOTAL-LINE-2.                                                EL568
00265      12  FILLER                        PIC X(23)   VALUE SPACES.  EL568
00266      12  TL-OVERALL-2                  PIC X(10).                 EL568
00267      12  FILLER                        PIC X(14)   VALUE          EL568
00268                 '      TOTAL   '.                                 EL568
00269      12  TL-AMT-2                      PIC $ZZ,ZZZ,ZZ9.99-.       EL568
00270  EJECT                                                            EL568
00271                              COPY ELCDATE.                        EL568
00272  EJECT                                                            EL568
00273                              COPY ELCDTECX.                       EL568
00274  EJECT                                                            EL568
00275                              COPY ELCDTEVR.                          CL**2
00276  EJECT                                                               CL**2
00277  PROCEDURE DIVISION.                                              EL568
00278                                                                   EL568
00279 ******************************************************************EL568
00280  0000-READ-DATE-CARD    SECTION.                                  EL568
00281 ******************************************************************EL568
00282                              COPY ELCDTERX.                       EL568
00283                                                                   EL568
00284  0000-EXIT.                                                       EL568
00285      EJECT                                                        EL568
00286 ******************************************************************EL568
00287  0500-MAINLINE  SECTION.                                          EL568
00288 ******************************************************************EL568
00289                                                                   EL568
00290      PERFORM 1000-INITIALIZATION.                                 EL568
00291                                                                   EL568
00292      PERFORM 2000-READ-INPUT.                                     EL568
00293                                                                   EL568
00294      PERFORM 4000-PRODUCE-REPORTS.                                EL568
00295                                                                   EL568
00296      PERFORM 9999-EOJ.                                            EL568
00297                                                                   EL568
00298  0500-EXIT.                                                       EL568
00299      EJECT                                                        EL568
00300 ******************************************************************EL568
00301  1000-INITIALIZATION  SECTION.                                    EL568
00302 ******************************************************************EL568
00303                                                                   EL568
00304      OPEN INPUT ERPNDB                                            EL568
00305                 ERACCT                                            EL568
00306                 ERCOMP                                            EL568
00307          OUTPUT PRNTR                                             EL568
00308                 TEMP-FILE.                                        EL568
00309                                                                   EL568
00310                                                                   EL568
00311      IF ERPNDB-FILE-STATUS = '00' OR '97'                         EL568
00312          NEXT SENTENCE                                            EL568
00313      ELSE                                                         EL568
00314         MOVE 'ERROR OCCURRED OPEN - ERPNDB' TO WS-ABEND-MESSAGE   EL568
00315         MOVE ERPNDB-FILE-STATUS TO WS-ABEND-FILE-STATUS           EL568
00316         GO TO ABEND-PGM.                                          EL568
00317                                                                   EL568
00318      IF ERACCT-FILE-STATUS = '00' OR '97'                         EL568
00319          NEXT SENTENCE                                            EL568
00320      ELSE                                                         EL568
00321         MOVE 'ERROR OCCURRED OPEN - ERACCT' TO WS-ABEND-MESSAGE   EL568
00322         MOVE ERACCT-FILE-STATUS TO WS-ABEND-FILE-STATUS           EL568
00323         GO TO ABEND-PGM.                                          EL568
00324                                                                   EL568
00325      IF ERCOMP-FILE-STATUS = '00' OR '97'                         EL568
00326          NEXT SENTENCE                                            EL568
00327      ELSE                                                         EL568
00328         MOVE 'ERROR OCCURRED OPEN - ERCOMP' TO WS-ABEND-MESSAGE   EL568
00329         MOVE ERCOMP-FILE-STATUS TO WS-ABEND-FILE-STATUS           EL568
00330         GO TO ABEND-PGM.                                          EL568
00331                                                                   EL568
00332      MOVE CLASIC-CREDIT-EOM-DT  TO  DC-BIN-DATE-1.                EL568
00333      MOVE ' '          TO    DC-OPTION-CODE.                      EL568
00334      CALL 'ELDATCX'  USING  DATE-CONVERSION-DATA.                 EL568
00335                                                                   EL568
00336      IF DATE-CONVERSION-ERROR                                     EL568
00337         MOVE 'DATE CONVERSION ERROR - 1    ' TO WS-ABEND-MESSAGE  EL568
00338         GO TO ABEND-PGM.                                          EL568
00339                                                                   EL568
00340      MOVE DC-GREG-DATE-1-ALPHA  TO  HD3-ALPHA-DATE.               EL568
00341                                                                   EL568
00342      MOVE    SPACES            TO   TEMP-WORK-FILE.               EL568
00343      ACCEPT  WS-ACCEPT-DATE    FROM     DATE.                     EL568
00344      MOVE    WS-AD-YY          TO   WS-CD-YY.                     EL568
00345      MOVE    WS-AD-MM          TO   WS-CD-MM.                     EL568
00346      MOVE    WS-AD-DD          TO   WS-CD-DD.                     EL568
00347                                                                   EL568
00348      MOVE WS-CURRENT-DATE      TO   HD2-DATE                      EL568
00349                                     DC-GREG-DATE-1-EDIT.          EL568
00350                                                                   EL568
00351      MOVE '2'          TO    DC-OPTION-CODE.                      EL568
00352      CALL 'ELDATCX'  USING  DATE-CONVERSION-DATA.                 EL568
00353                                                                   EL568
00354      IF DATE-CONVERSION-ERROR                                     EL568
00355         MOVE 'DATE CONVERSION ERROR - 2    ' TO WS-ABEND-MESSAGE  EL568
00356         GO TO ABEND-PGM.                                          EL568
00357                                                                   EL568
00358      MOVE LOW-VALUES  TO  AM-CONTROL-PRIMARY.                     EL568
00359      MOVE DTE-CLASIC-COMPANY-CD  TO  AM-COMPANY-CD.               EL568
00360                                                                   EL568
00361      START ERACCT KEY NOT LESS THAN  AM-CONTROL-PRIMARY.          EL568
00362                                                                   EL568
00363      IF ERACCT-FILE-STATUS = '00'                                 EL568
00364          NEXT SENTENCE                                            EL568
00365      ELSE                                                         EL568
00366         MOVE 'ERROR OCCURRED STRT - ERACCT' TO WS-ABEND-MESSAGE   EL568
00367         MOVE ERACCT-FILE-STATUS TO WS-ABEND-FILE-STATUS           EL568
00368         GO TO ABEND-PGM.                                          EL568
00369                                                                   EL568
00370                                                                   EL568
00371                                                                   EL568
00372  1000-EXIT.                                                       EL568
00373      EJECT                                                        EL568
00374 ******************************************************************EL568
00375  2000-READ-INPUT       SECTION.                                   EL568
00376 ******************************************************************EL568
00377                                                                   EL568
00378      PERFORM 7000-READ-ERACCT.                                    EL568
00379                                                                   EL568
00380                                                                   EL568
00381      IF ERACCT-FILE-STATUS = '10'  OR                             EL568
00382         AM-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD                 EL568
00383             PERFORM 3000-FIND-ERPNDB                              EL568
00384              IF WK-NET-PREM NOT = ZEROS OR                        EL568
00385                 SAVE-EXP-DT  NOT LESS THAN DC-BIN-DATE-1          EL568
00386                     MOVE WK-NET-PREM TO TW-NET-PREM               EL568
00387                     PERFORM 8000-WRITE-TEMP                       EL568
00388                     GO TO 2000-EXIT                               EL568
00389              ELSE                                                 EL568
00390                 GO TO 2000-EXIT.                                  EL568
00391                                                                   EL568
00392      IF FIRST-READ = 'Y'                                          EL568
00393          MOVE AM-CARRIER  TO  TW-CARRIER                          EL568
00394          MOVE AM-GROUPING TO  TW-GROUPING                         EL568
00395          MOVE AM-STATE    TO  TW-STATE                            EL568
00396          MOVE AM-ACCOUNT  TO  TW-ACCOUNT                          EL568
00397          MOVE AM-NAME     TO  TW-ACCOUNT-NAME                     EL568
00398          MOVE AM-EXPIRATION-DT TO SAVE-EXP-DT                     EL568
00399          MOVE 'N'         TO  FIRST-READ                          EL568
00400          PERFORM 3500-FIND-STATE-NAME                             EL568
00401          IF DTE-CLIENT = 'WDS'                                    EL568
00402              MOVE AM-AGT (03) TO TW-GEN-AGENT                     EL568
00403              GO TO 2000-READ-INPUT                                EL568
00404          ELSE                                                     EL568
00405              MOVE AM-AGT (AM-REMIT-TO) TO TW-GEN-AGENT            EL568
00406              GO TO 2000-READ-INPUT.                               EL568
00407                                                                   EL568
00408      IF  AM-CARRIER  NOT =  TW-CARRIER    OR                      EL568
00409          AM-GROUPING NOT =  TW-GROUPING OR                        EL568
00410          AM-STATE    NOT =  TW-STATE      OR                      EL568
00411          AM-ACCOUNT  NOT =  TW-ACCOUNT                            EL568
00412              PERFORM  3000-FIND-ERPNDB                            EL568
00413      ELSE                                                         EL568
00414              GO TO 2000-MOVES.                                    EL568
00415                                                                   EL568
00416      IF WK-NET-PREM NOT = ZEROS    OR                             EL568
00417         SAVE-EXP-DT  NOT LESS THAN   DC-BIN-DATE-1                EL568
00418             MOVE WK-NET-PREM  TO  TW-NET-PREM                     EL568
00419                                                                   EL568
00420             PERFORM 8000-WRITE-TEMP.                              EL568
00421                                                                   EL568
00422                                                                   EL568
00423  2000-MOVES.                                                      EL568
00424                                                                   EL568
00425      MOVE SPACES          TO  TEMP-WORK-FILE.                     EL568
00426      MOVE AM-CARRIER      TO  TW-CARRIER.                         EL568
00427      MOVE AM-GROUPING     TO  TW-GROUPING.                        EL568
00428      MOVE AM-STATE        TO  TW-STATE.                           EL568
00429      MOVE AM-ACCOUNT      TO  TW-ACCOUNT.                         EL568
00430      MOVE AM-NAME         TO  TW-ACCOUNT-NAME.                    EL568
00431      IF DTE-CLIENT = 'WDS'                                        EL568
00432          MOVE AM-AGT (03) TO  TW-GEN-AGENT                        EL568
00433          ELSE                                                     EL568
00434          MOVE AM-AGT (AM-REMIT-TO) TO TW-GEN-AGENT.               EL568
00435      MOVE AM-EXPIRATION-DT TO SAVE-EXP-DT.                        EL568
00436      PERFORM 3500-FIND-STATE-NAME.                                EL568
00437                                                                   EL568
00438      GO TO 2000-READ-INPUT.                                       EL568
00439                                                                   EL568
00440  2000-EXIT.                                                       EL568
00441      EJECT                                                        EL568
00442 ******************************************************************EL568
00443  3000-FIND-ERPNDB        SECTION.                                 EL568
00444 ******************************************************************EL568
00445                                                                   EL568
00446      MOVE +0             TO   WK-NET-PREM.                        EL568
00447      MOVE LOW-VALUES     TO   PB-CONTROL-BY-ACCOUNT.              EL568
00448      MOVE DTE-CLASIC-COMPANY-CD TO   PB-COMPANY-CD-A1.            EL568
00449      MOVE TW-CARRIER     TO   PB-CARRIER  VG-CARRIER.             EL568
00450      MOVE TW-GROUPING    TO   PB-GROUPING VG-GROUPING.            EL568
00451      MOVE TW-STATE       TO   PB-STATE  VG-STATE.                 EL568
00452      MOVE TW-ACCOUNT     TO   PB-ACCOUNT VG-ACCOUNT.              EL568
00453                                                                   EL568
00454      IF DTE-COMP-VG = ' '                                         EL568
00455          MOVE SPACES TO PB-CARRIER VG-CARRIER                     EL568
00456                         PB-GROUPING VG-GROUPING                   EL568
00457      ELSE                                                         EL568
00458      IF DTE-COMP-VG = '2'                                         EL568
00459          MOVE SPACES TO PB-GROUPING VG-GROUPING                   EL568
00460      ELSE                                                         EL568
00461      IF DTE-COMP-VG = '3'                                         EL568
00462          MOVE SPACES TO PB-CARRIER VG-CARRIER                     EL568
00463                         PB-GROUPING VG-GROUPING                   EL568
00464                         PB-STATE VG-STATE                         EL568
00465      ELSE                                                         EL568
00466      IF DTE-COMP-VG = '4'                                         EL568
00467          MOVE SPACES TO PB-GROUPING VG-GROUPING                   EL568
00468                         PB-STATE VG-STATE.                        EL568
00469                                                                   EL568
00470      START ERPNDB KEY NOT LESS THAN PB-CONTROL-BY-ACCOUNT.        EL568
00471                                                                   EL568
00472  3000-READ-NEXT.                                                  EL568
00473                                                                   EL568
00474      READ ERPNDB NEXT RECORD.                                     EL568
00475                                                                   EL568
00476      IF ERPNDB-FILE-STATUS = '10'  OR                             EL568
00477         PB-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD                 EL568
00478             GO TO 3000-EXIT.                                      EL568
00479                                                                   EL568
00480      IF PB-RECORD-TYPE NOT = '9'                                  EL568
00481          GO TO 3000-READ-NEXT.                                    EL568
00482                                                                   EL568
00483      IF PB-COMPANY-CD  NOT =  DTE-CLASIC-COMPANY-CD  OR           EL568
00484         PB-CARRIER     NOT =  VG-CARRIER     OR                   EL568
00485         PB-GROUPING    NOT =  VG-GROUPING    OR                   EL568
00486         PB-STATE       NOT =  VG-STATE       OR                   EL568
00487         PB-ACCOUNT     NOT =  VG-ACCOUNT                          EL568
00488             GO TO 3000-EXIT.                                      EL568
00489                                                                   EL568
00490      COMPUTE WK-NET-PREM = WK-NET-PREM +                          EL568
00491                            PB-B-LF-ISS-PRM-REMITTED -             EL568
00492                            PB-B-LF-CAN-PRM-REMITTED +             EL568
00493                            PB-B-AH-ISS-PRM-REMITTED -             EL568
00494                            PB-B-AH-CAN-PRM-REMITTED.              EL568
00495                                                                   EL568
00496      MOVE PB-CREDIT-SELECT-DT  TO  TW-EOM-DT.                     EL568
00497                                                                   EL568
00498      GO TO 3000-READ-NEXT.                                        EL568
00499                                                                   EL568
00500  3000-EXIT.                                                       EL568
00501      EJECT                                                        EL568
00502 ******************************************************************EL568
00503  3500-FIND-STATE-NAME    SECTION.                                 EL568
00504 ******************************************************************EL568
00505                                                                   EL568
00506      MOVE +0   TO  A-NDX.                                         EL568
00507                                                                   EL568
00508  3500-LOOP.                                                       EL568
00509                                                                   EL568
00510      ADD +1    TO  A-NDX.                                         EL568
00511                                                                   EL568
00512      IF A-NDX GREATER THAN CLAS-MAXS                              EL568
00513          MOVE SPACES TO TW-STATE-NAME                             EL568
00514          GO TO 3500-EXIT.                                         EL568
00515                                                                   EL568
00516      IF AM-STATE  =  STATE-SUB (A-NDX)                            EL568
00517          MOVE STATE-PIC (A-NDX) TO TW-STATE-NAME                  EL568
00518          GO TO 3500-EXIT.                                         EL568
00519                                                                   EL568
00520      GO TO 3500-LOOP.                                             EL568
00521                                                                   EL568
00522  3500-EXIT.                                                       EL568
00523      EJECT                                                        EL568
00524 ******************************************************************EL568
00525  4000-PRODUCE-REPORTS    SECTION.                                 EL568
00526 ******************************************************************EL568
00527                                                                   EL568
00528      CLOSE  TEMP-FILE.                                            EL568
00529                                                                   EL568
00530      SORT  SORT-FILE   ASCENDING KEY  SORT-CNTL-1                 EL568
00531                                       SORT-CNTL-1B                EL568
00532                        USING          TEMP-FILE                   EL568
00533                        GIVING         TEMP-FILE.                  EL568
00534                                                                   EL568
00535      OPEN INPUT TEMP-FILE.                                        EL568
00536                                                                   EL568
00537      MOVE 'BY GENERAL AGENT' TO HD2-RPT-NAME.                     EL568
00538      MOVE 'Y' TO FIRST-READ.                                      EL568
00539      MOVE '1' TO HD1-RPT-CODE.                                    EL568
00540      MOVE 1   TO WS-PAGE-NO.                                      EL568
00541      PERFORM 4100-REPORT-1.                                       EL568
00542                                                                   EL568
00543                                                                   EL568
00544      CLOSE  TEMP-FILE.                                            EL568
00545                                                                   EL568
00546      SORT  SORT-FILE   ASCENDING KEY  SORT-CNTL-2                 EL568
00547                        USING          TEMP-FILE                   EL568
00548                        GIVING         TEMP-FILE.                  EL568
00549                                                                   EL568
00550      OPEN INPUT TEMP-FILE.                                        EL568
00551                                                                   EL568
00552      MOVE '    BY STATE    ' TO HD2-RPT-NAME.                     EL568
00553      MOVE 'Y' TO FIRST-READ.                                      EL568
00554      MOVE 'N' TO TEMP-EOF-SW.                                     EL568
00555      MOVE '2' TO HD1-RPT-CODE.                                    EL568
00556      MOVE 1   TO WS-PAGE-NO.                                      EL568
00557      PERFORM 4200-REPORT-2.                                       EL568
00558                                                                   EL568
00559                                                                   EL568
00560  4000-EXIT.                                                       EL568
00561      EJECT                                                        EL568
00562 ******************************************************************EL568
00563  4100-REPORT-1           SECTION.                                 EL568
00564 ******************************************************************EL568
00565                                                                   EL568
00566      PERFORM 7200-READ-TEMP.                                      EL568
00567                                                                   EL568
00568      IF TEMP-EOF-SW = 'Y'                                         EL568
00569          PERFORM 4300-GA-BREAK                                    EL568
00570          PERFORM 4500-FINAL-TOTAL                                 EL568
00571          GO TO 4100-EXIT.                                         EL568
00572                                                                   EL568
00573      IF TW-GEN-AGENT = ZEROS                                      EL568
00574          GO TO 4100-REPORT-1.                                     EL568
00575                                                                   EL568
00576      IF FIRST-READ = 'Y'                                          EL568
00577          MOVE TW-GEN-AGENT  TO  PREV-AGENT                        EL568
00578          MOVE 'N'           TO  FIRST-READ                        EL568
00579          PERFORM 6300-HEADING-ROUTINE                             EL568
00580          GO TO 4100-CONT.                                         EL568
00581                                                                   EL568
00582      IF TW-GEN-AGENT  NOT =  PREV-AGENT                           EL568
00583          PERFORM 4300-GA-BREAK                                    EL568
00584          PERFORM 6300-HEADING-ROUTINE.                            EL568
00585                                                                   EL568
00586  4100-CONT.                                                       EL568
00587                                                                   EL568
00588      IF TW-EOM-DT = SPACES                                        EL568
00589          MOVE SPACES TO DC-GREG-DATE-1-EDIT                       EL568
00590          GO TO 4100-SKIP.                                         EL568
00591                                                                   EL568
00592      MOVE TW-EOM-DT TO       DC-BIN-DATE-1.                       EL568
00593      MOVE ' '          TO    DC-OPTION-CODE.                      EL568
00594      CALL 'ELDATCX' USING DATE-CONVERSION-DATA.                   EL568
00595                                                                   EL568
00596      IF DATE-CONVERSION-ERROR                                     EL568
00597         MOVE 'DATE CONVERSION ERROR - 3    ' TO WS-ABEND-MESSAGE  EL568
00598         GO TO ABEND-PGM.                                          EL568
00599                                                                   EL568
00600  4100-SKIP.                                                       EL568
00601                                                                   EL568
00602      MOVE TW-ACCOUNT           TO  PRT-ACCOUNT.                   EL568
00603      MOVE TW-ACCOUNT-NAME      TO  PRT-ACCT-NAME.                 EL568
00604      MOVE DC-GREG-DATE-1-EDIT  TO  PRT-EOM-DATE.                  EL568
00605      MOVE TW-NET-PREM          TO  PRT-PREM-AMT.                  EL568
00606      MOVE PRT-DETAIL           TO  P-DATA.                        EL568
00607      MOVE ' '                  TO  X.                             EL568
00608      PERFORM 6400-PRINT-LINE.                                     EL568
00609      ADD  TW-NET-PREM          TO  WK-GA-PREM.                    EL568
00610                                                                   EL568
00611  4100-MOVE-PREV.                                                  EL568
00612                                                                   EL568
00613      MOVE TW-GEN-AGENT  TO   PREV-AGENT.                          EL568
00614      GO TO 4100-REPORT-1.                                         EL568
00615                                                                   EL568
00616  4100-EXIT.                                                       EL568
00617      EJECT                                                        EL568
00618 ******************************************************************EL568
00619  4200-REPORT-2           SECTION.                                 EL568
00620 ******************************************************************EL568
00621                                                                   EL568
00622      PERFORM 7200-READ-TEMP.                                      EL568
00623                                                                   EL568
00624      IF TEMP-EOF-SW = 'Y'                                         EL568
00625          GO TO 4200-CONT.                                         EL568
00626                                                                   EL568
00627      IF TW-GEN-AGENT = ZEROS                                      EL568
00628          GO TO 4200-REPORT-2.                                     EL568
00629                                                                   EL568
00630      IF FIRST-READ = 'Y'                                          EL568
00631          MOVE TW-STATE      TO  PREV-STATE                        EL568
00632          MOVE TW-STATE-NAME TO  PREV-STATE-NAME                   EL568
00633          MOVE 'N'           TO  FIRST-READ                        EL568
00634          PERFORM 6350-HEADING-ROUTINE.                            EL568
00635                                                                   EL568
00636      IF TW-STATE      NOT =  PREV-STATE                           EL568
00637          GO TO 4200-CONT.                                         EL568
00638                                                                   EL568
00639      ADD TW-NET-PREM     TO   WK-GA-PREM.                         EL568
00640      MOVE TW-STATE-NAME  TO  PREV-STATE-NAME.                     EL568
00641                                                                   EL568
00642      GO TO 4200-REPORT-2.                                         EL568
00643                                                                   EL568
00644  4200-CONT.                                                       EL568
00645                                                                   EL568
00646      IF WK-GA-PREM = ZEROS                                        EL568
00647          GO TO 4200-MOVE-PREV.                                    EL568
00648                                                                   EL568
00649      MOVE PREV-STATE-NAME      TO  PRT-STATE.                     EL568
00650      MOVE WK-GA-PREM           TO  PRT-PREM-AMT-2.                EL568
00651      MOVE PRT-DETAIL-2         TO  P-DATA.                        EL568
00652      MOVE ' '                  TO  X.                             EL568
00653      PERFORM 6400-PRINT-LINE.                                     EL568
00654      ADD  WK-GA-PREM           TO  WK-TOTAL-PREM.                 EL568
00655      MOVE +0                   TO  WK-GA-PREM.                    EL568
00656                                                                   EL568
00657  4200-MOVE-PREV.                                                  EL568
00658                                                                   EL568
00659      IF TEMP-EOF-SW = 'Y'                                         EL568
00660          PERFORM 4550-FINAL-TOTAL                                 EL568
00661          GO TO 4200-EXIT.                                         EL568
00662                                                                   EL568
00663      MOVE TW-STATE        TO   PREV-STATE.                        EL568
00664      MOVE TW-STATE-NAME   TO   PREV-STATE-NAME.                   EL568
00665      ADD  TW-NET-PREM     TO   WK-GA-PREM.                        EL568
00666      GO TO 4200-REPORT-2.                                         EL568
00667                                                                   EL568
00668  4200-EXIT.                                                       EL568
00669      EJECT                                                        EL568
00670 ******************************************************************EL568
00671  4300-GA-BREAK          SECTION.                                  EL568
00672 ******************************************************************EL568
00673                                                                   EL568
00674      MOVE SPACES         TO   TL-OVERALL.                         EL568
00675      MOVE WK-GA-PREM     TO   TL-AMT.                             EL568
00676      MOVE TOTAL-LINE     TO   P-DATA.                             EL568
00677      MOVE '0'            TO   X.                                  EL568
00678      PERFORM 6400-PRINT-LINE.                                     EL568
00679      ADD  WK-GA-PREM     TO   WK-TOTAL-PREM.                      EL568
00680                                                                   EL568
00681      MOVE +0             TO   WK-GA-PREM.                         EL568
00682                                                                   EL568
00683                                                                   EL568
00684  4300-EXIT.                                                       EL568
00685      EJECT                                                        EL568
00686 ******************************************************************EL568
00687  4500-FINAL-TOTAL       SECTION.                                  EL568
00688 ******************************************************************EL568
00689                                                                   EL568
00690      MOVE '   OVERALL'   TO   TL-OVERALL.                         EL568
00691      MOVE WK-TOTAL-PREM  TO   TL-AMT.                             EL568
00692      MOVE TOTAL-LINE     TO   P-DATA.                             EL568
00693      MOVE '0'            TO   X.                                  EL568
00694      PERFORM 6400-PRINT-LINE.                                     EL568
00695                                                                   EL568
00696      MOVE +0             TO   WK-TOTAL-PREM.                      EL568
00697                                                                   EL568
00698                                                                   EL568
00699  4500-EXIT.                                                       EL568
00700      EJECT                                                        EL568
00701 ******************************************************************EL568
00702  4550-FINAL-TOTAL       SECTION.                                  EL568
00703 ******************************************************************EL568
00704                                                                   EL568
00705      MOVE '   OVERALL'   TO   TL-OVERALL-2.                       EL568
00706      MOVE WK-TOTAL-PREM  TO   TL-AMT-2                            EL568
00707      MOVE TOTAL-LINE-2   TO   P-DATA.                             EL568
00708      MOVE '0'            TO   X.                                  EL568
00709      PERFORM 6400-PRINT-LINE.                                     EL568
00710                                                                   EL568
00711      MOVE +0             TO   WK-TOTAL-PREM.                      EL568
00712                                                                   EL568
00713                                                                   EL568
00714  4550-EXIT.                                                       EL568
00715      EJECT                                                        EL568
00716 ******************************************************************EL568
00717  6000-GET-GA-NAME       SECTION.                                  EL568
00718 ******************************************************************EL568
00719                                                                   EL568
00720      MOVE LOW-VALUES     TO   CO-CONTROL-PRIMARY.                 EL568
00721      MOVE DTE-CLASIC-COMPANY-CD TO CO-COMPANY-CD.                 EL568
00722      MOVE TW-CARRIER     TO   CO-CARRIER.                         EL568
00723      MOVE TW-GROUPING    TO   CO-GROUPING.                        EL568
00724      MOVE TW-GEN-AGENT   TO   CO-RESP-NO  GA-NUMBER.              EL568
00725      MOVE 'G'            TO   CO-TYPE.                            EL568
00726                                                                   EL568
00727      IF DTE-COMPENSATION-ACCESS = '1'                             EL568
00728          MOVE ZERO  TO  CO-CARRIER                                EL568
00729      ELSE                                                         EL568
00730      IF DTE-COMPENSATION-ACCESS = '2'                             EL568
00731          MOVE ZERO  TO  CO-GROUPING                               EL568
00732      ELSE                                                         EL568
00733      IF DTE-COMPENSATION-ACCESS = '3'                             EL568
00734          MOVE ZERO  TO  CO-CARRIER CO-GROUPING.                   EL568
00735                                                                   EL568
00736                                                                   EL568
00737      READ ERCOMP.                                                 EL568
00738                                                                   EL568
00739      IF ERCOMP-FILE-STATUS = '00'                                 EL568
00740          MOVE CO-ACCT-NAME TO  GA-NAME                            EL568
00741      ELSE                                                         EL568
00742         MOVE SPACES TO GA-NAME.                                   EL568
00743                                                                   EL568
00744  6000-EXIT.                                                       EL568
00745      EJECT                                                        EL568
00746 ******************************************************************EL568
00747  6300-HEADING-ROUTINE   SECTION.                                  EL568
00748 ******************************************************************EL568
00749                                                                   EL568
00750      PERFORM 6000-GET-GA-NAME.                                    EL568
00751      MOVE +0              TO    WS-LN-CNT.                        EL568
00752      MOVE WS-PAGE-NO      TO    HD3-PG-NO.                        EL568
00753      MOVE PRT-HD1         TO    P-DATA.                           EL568
00754      MOVE '1'             TO    X.                                EL568
00755      PERFORM 6400-PRINT-LINE.                                     EL568
00756      MOVE PRT-HD2         TO    P-DATA.                           EL568
00757      MOVE ' '             TO    X.                                EL568
00758      PERFORM 6400-PRINT-LINE.                                     EL568
00759      MOVE PRT-HD3         TO    P-DATA.                           EL568
00760      PERFORM 6400-PRINT-LINE.                                     EL568
00761      MOVE PRT-GA-HD       TO    P-DATA.                           EL568
00762      MOVE '0'             TO    X.                                EL568
00763      PERFORM 6400-PRINT-LINE.                                     EL568
00764      MOVE PRT-HD4-A       TO    P-DATA.                           EL568
00765      PERFORM 6400-PRINT-LINE.                                     EL568
00766      MOVE PRT-HD5-A       TO    P-DATA.                           EL568
00767      MOVE ' '             TO    X.                                EL568
00768      PERFORM 6400-PRINT-LINE.                                     EL568
00769      MOVE SPACES          TO    P-DATA.                           EL568
00770      PERFORM 6400-PRINT-LINE.                                     EL568
00771      MOVE 10              TO    WS-LN-CNT.                        EL568
00772      ADD +1               TO    WS-PAGE-NO.                       EL568
00773                                                                   EL568
00774  6300-EXIT.                                                       EL568
00775      EJECT                                                        EL568
00776 ******************************************************************EL568
00777  6350-HEADING-ROUTINE   SECTION.                                  EL568
00778 ******************************************************************EL568
00779                                                                   EL568
00780      MOVE +0              TO    WS-LN-CNT.                        EL568
00781      MOVE WS-PAGE-NO      TO    HD3-PG-NO.                        EL568
00782      MOVE PRT-HD1         TO    P-DATA.                           EL568
00783      MOVE '1'             TO    X.                                EL568
00784      PERFORM 6400-PRINT-LINE.                                     EL568
00785      MOVE PRT-HD2         TO    P-DATA.                           EL568
00786      MOVE ' '             TO    X.                                EL568
00787      PERFORM 6400-PRINT-LINE.                                     EL568
00788      MOVE PRT-HD3         TO    P-DATA.                           EL568
00789      PERFORM 6400-PRINT-LINE.                                     EL568
00790      MOVE '0'             TO    X.                                EL568
00791      MOVE PRT-HD4-2       TO    P-DATA.                           EL568
00792      PERFORM 6400-PRINT-LINE.                                     EL568
00793      MOVE ' '             TO    X.                                EL568
00794      MOVE SPACES          TO    P-DATA.                           EL568
00795      PERFORM 6400-PRINT-LINE.                                     EL568
00796      MOVE 7               TO    WS-LN-CNT.                        EL568
00797      ADD +1               TO    WS-PAGE-NO.                       EL568
00798                                                                   EL568
00799  6350-EXIT.                                                       EL568
00800      EJECT                                                        EL568
00801 ******************************************************************EL568
00802  6400-PRINT-LINE        SECTION.                                  EL568
00803 ******************************************************************EL568
00804                                                                   EL568
00805      IF WS-LN-CNT GREATER THAN 56                                 EL568
00806          IF HD1-RPT-CODE = '1'                                    EL568
00807              PERFORM 6300-HEADING-ROUTINE                         EL568
00808          ELSE                                                     EL568
00809              PERFORM 6350-HEADING-ROUTINE.                        EL568
00810                                                                   EL568
00811      ADD +1   TO   WS-LN-CNT.                                     EL568
00812                                                                   EL568
00813                       COPY ELCPRT2X.                              EL568
00814                                                                   EL568
00815                                                                   EL568
00816  6400-EXIT.                                                       EL568
00817      EJECT                                                        EL568
00818 ******************************************************************EL568
00819  7000-READ-ERACCT       SECTION.                                  EL568
00820 ******************************************************************EL568
00821                                                                   EL568
00822      READ ERACCT NEXT RECORD.                                     EL568
00823                                                                   EL568
00824      IF ERACCT-FILE-STATUS = '00' OR '10'                         EL568
00825          NEXT SENTENCE                                            EL568
00826      ELSE                                                         EL568
00827         MOVE 'ERROR OCCURRED READ - ERACCT' TO WS-ABEND-MESSAGE   EL568
00828         MOVE ERACCT-FILE-STATUS TO WS-ABEND-FILE-STATUS           EL568
00829         GO TO ABEND-PGM.                                          EL568
00830                                                                   EL568
00831  7000-EXIT.                                                       EL568
00832 ******************************************************************EL568
00833  7200-READ-TEMP         SECTION.                                  EL568
00834 ******************************************************************EL568
00835                                                                   EL568
00836      READ TEMP-FILE   INTO  TEMP-WORK-FILE  AT END                EL568
00837          MOVE 'Y' TO TEMP-EOF-SW.                                 EL568
00838                                                                   EL568
00839                                                                   EL568
00840  7200-EXIT.                                                       EL568
00841      EJECT                                                        EL568
00842 ******************************************************************EL568
00843  8000-WRITE-TEMP        SECTION.                                  EL568
00844 ******************************************************************EL568
00845                                                                   EL568
00846      WRITE TEMP-REC  FROM  TEMP-WORK-FILE.                        EL568
00847                                                                   EL568
00848  8000-EXIT.                                                       EL568
00849      EJECT                                                        EL568
00850 ******************************************************************EL568
00851  9999-EOJ               SECTION.                                  EL568
00852 ******************************************************************EL568
00853                                                                   EL568
00854      CLOSE      ERPNDB                                            EL568
00855                 ERACCT                                            EL568
00856                 PRNTR.                                            EL568
00857                                                                   EL568
00858                             COPY ELCPRTCX.                        EL568
00859                                                                   EL568
00860                                                                   EL568
00861      GOBACK.                                                      EL568
00862                                                                   EL568
00863  9999-EXIT.                                                       EL568
00864      EJECT                                                        EL568
00865 ******************************************************************EL568
00866  ABEND-PGM  SECTION.                                              EL568
00867 ******************************************************************EL568
00868                                                                   EL568
00869                                COPY ELCABEND.                     EL568
00870                                                                   EL568
00871 ******************************************************************EL568
