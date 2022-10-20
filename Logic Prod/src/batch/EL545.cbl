00001  IDENTIFICATION DIVISION.                                         03/19/98
00002                                                                   EL545
00003  PROGRAM-ID.                 EL545 .                                 LV004
00004 *              PROGRAM CONVERTED BY                               EL545
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL545
00006 *              CONVERSION DATE 07/20/94 16:04:27.                 EL545
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          EL545
00008 *                            VMOD=2.009.                          EL545
00009                                                                   EL545
00010 *AUTHOR.        LOGIC, INC.                                       EL545
00011 *               DALLAS, TEXAS.                                    EL545
00012                                                                   EL545
00013 *DATE-COMPILED.                                                   EL545
00014                                                                   EL545
00015 *SECURITY.   *****************************************************EL545
00016 *            *                                                   *EL545
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *EL545
00018 *            *                                                   *EL545
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL545
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL545
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *EL545
00022 *            *                                                   *EL545
00023 *            *****************************************************EL545
00024                                                                   EL545
00025 *REMARKS.                                                         EL545
00026 *        THIS PROGRAM SORTS THE LOSS RATIO FILE INTO PROPER       EL545
00027 *        SEQUENCE AND UPDATES RECORDS WITH ACCOUNT MASTER         EL545
00028 *        DATE RANGE INFORMATION AND GENERAL AGENTS NAMES,         EL545
00029 *        AND LOADS THE ON-LINE ERLOSS FILE.                       EL545
00030                                                                   EL545
00031                                                                   EL545
00032  ENVIRONMENT DIVISION.                                            EL545
00033  INPUT-OUTPUT SECTION.                                            EL545
00034  FILE-CONTROL.                                                    EL545
00035                                                                   EL545
00036      SELECT LOSS-RATIOS-IN   ASSIGN TO SYS010-UT-2400-S-SYS010.   EL545
00037      SELECT ERACCTT          ASSIGN TO SYS015-FBA1-ERACCTT        EL545
00038                              ACCESS IS DYNAMIC                    EL545
00039                              ORGANIZATION IS INDEXED              EL545
00040                              FILE STATUS IS AM-FILE-STATUS        EL545
00041                              RECORD KEY IS AM-CONTROL-PRIMARY.    EL545
00042      SELECT ERCOMP           ASSIGN TO SYS016-FBA1-ERCOMP         EL545
00043                              ACCESS IS DYNAMIC                    EL545
00044                              ORGANIZATION IS INDEXED              EL545
00045                              FILE STATUS IS CO-FILE-STATUS        EL545
00046                              RECORD KEY IS CO-CONTROL-PRIMARY.    EL545
00047      SELECT ERLOSS           ASSIGN TO SYS017-FBA1-ERLOSS         EL545
00048                              ACCESS IS DYNAMIC                    EL545
00049                              ORGANIZATION IS INDEXED              EL545
00050                              FILE STATUS IS LR-FILE-STATUS        EL545
00051                              RECORD KEY IS LR-CONTROL-PRIMARY.    EL545
00052      SELECT DISK-DATE        ASSIGN TO SYS019-UT-3380-S-SYS019.   EL545
00053      SELECT SORT-WORK        ASSIGN TO SYS001-UT-3380-S-SORTWK1.  EL545
00054      SELECT PRINTER          ASSIGN TO SYS008-UR-1403-S-SYS008.   EL545
00055      SELECT FICH             ASSIGN TO SYS020-UT-2400-S-SYS020.   EL545
00056                                                                   EL545
00057  EJECT                                                            EL545
00058  DATA DIVISION.                                                   EL545
00059  FILE SECTION.                                                    EL545
00060                                                                   EL545
00061  FD  LOSS-RATIOS-IN                                               EL545
00062      BLOCK CONTAINS 0 RECORDS
00063      RECORDING MODE F.                                               CL**4
00064                                                                   EL545
00065  01  LOSS-RATIO-RCD-IN             PIC X(525).                    EL545
00066                                                                   EL545
00067  SD  SORT-WORK.                                                      CL**2
00068                                                                   EL545
00069  01  SORT-RECORD.                                                 EL545
00070      12  SW-RECORD-ID            PIC XX.                          EL545
00071                                                                   EL545
00072      12  SW-CONTROL.                                              EL545
00073          16  SW-COMPANY-CD       PIC X.                           EL545
00074          16  SW-RCD-TYPE         PIC X.                           EL545
00075          16  SW-REIN-CO          PIC XXX.                         EL545
00076          16  SW-RPT-CD-1         PIC X(10).                       EL545
00077          16  SW-CARRIER          PIC X.                           EL545
00078          16  SW-GROUPING         PIC X(6).                        EL545
00079          16  SW-GA-RPT-CD-2      PIC X(10).                       EL545
00080          16  SW-STATE            PIC XX.                          EL545
00081          16  SW-ACCOUNT          PIC X(10).                       EL545
00082          16  SW-REIN-SUB         PIC XXX.                         EL545
00083                                                                   EL545
00084      12  SW-DATA.                                                 EL545
00085          16  FILLER              PIC X(248).                      EL545
00086                                                                   EL545
00087          16  SW-TOTALS  OCCURS  3  TIMES.                         EL545
00088              20  SW-YTD-NET      PIC S9(11)V99      COMP-3.       EL545
00089              20  SW-YTD-EARN     PIC S9(11)V99      COMP-3.       EL545
00090              20  SW-YTD-PAID     PIC S9(11)V99      COMP-3.       EL545
00091              20  SW-YTD-RESV     PIC S9(11)V99      COMP-3.       EL545
00092              20  SW-YTD-INCUR    PIC S9(11)V99      COMP-3.       EL545
00093              20  SW-YTD-RATIO    PIC S9(4)V9        COMP-3.       EL545
00094              20  SW-ITD-NET      PIC S9(11)V99      COMP-3.       EL545
00095              20  SW-ITD-EARN     PIC S9(11)V99      COMP-3.       EL545
00096              20  SW-ITD-PAID     PIC S9(11)V99      COMP-3.       EL545
00097              20  SW-ITD-RESV     PIC S9(11)V99      COMP-3.       EL545
00098              20  SW-ITD-INCUR    PIC S9(11)V99      COMP-3.       EL545
00099              20  SW-ITD-RATIO    PIC S9(4)V9        COMP-3.       EL545
00100                                                                   EL545
00101  FD  ERLOSS                                                       EL545
00102      BLOCK CONTAINS 0 RECORDS.
00103                                                                   EL545
00104  01  LOSS-RATIO-RECORD.                                           EL545
00105      12  LR-RCD-ID               PIC XX.                          EL545
00106                                                                   EL545
00107      12  LR-CONTROL-PRIMARY.                                      EL545
00108          16  LR-COMPANY-CODE     PIC X.                           EL545
00109          16  FILLER              PIC X(46).                       EL545
00110                                                                   EL545
00111      12  LR-DATA-AREA            PIC X(476).                      EL545
00112                                                                   EL545
00113  EJECT                                                            EL545
00114  FD  ERACCTT                                                      EL545
00115      BLOCK CONTAINS 0 RECORDS.
00116                                                                   EL545
00117                                    COPY ERCACCT.                  EL545
00118                                                                   EL545
00119  EJECT                                                            EL545
00120  FD  ERCOMP                                                       EL545
00121      BLOCK CONTAINS 0 RECORDS.
00122                                                                   EL545
00123                                    COPY ERCCOMP.                  EL545
00124                                                                   EL545
00125  EJECT                                                            EL545
00126  FD  DISK-DATE                                                    EL545
00127                                    COPY ELCDTEFD.                 EL545
00128                                                                   EL545
00129  EJECT                                                            EL545
00130  FD  PRINTER                                                      EL545
00131                                    COPY ELCPRTFD.                 EL545
00132                                                                   EL545
00133  FD  FICH                                                         EL545
00134                                    COPY ELCFCHFD.                 EL545
00135  EJECT                                                            EL545
00136  WORKING-STORAGE SECTION.                                         EL545
00137  77  FILLER  PIC X(32) VALUE '********************************'.  EL545
00138  77  FILLER  PIC X(32) VALUE '     EL545  WORKING STORAGE     '.  EL545
00139  77  FILLER  PIC X(32) VALUE '*********VMOD=2.009*************'.  EL545
00140                                                                   EL545
00141  01  MISC.                                                        EL545
00142      12  X1                  PIC S999    COMP.                    EL545
00143      12  X2                  PIC S999    COMP.                    EL545
00144                                                                   EL545
00145      12  X                   PIC X               VALUE SPACE.     EL545
00146                                                                   EL545
00147      12  WS-LOSS             PIC S9(5)V9(4)      VALUE ZERO.      EL545
00148                                                                   EL545
00149      12  RECORD-COUNT        PIC S9(7)   COMP-3  VALUE ZERO.      EL545
00150      12  DELETE-COUNT        PIC S9(7)   COMP-3  VALUE ZERO.      EL545
00151                                                                   EL545
00152      12  WS-RETURN-CODE      PIC X(4)            VALUE ZEROS.     EL545
00153      12  ABEND-OPTION        PIC X               VALUE 'Y'.       EL545
00154      12  WS-ABEND-MESSAGE    PIC X(80)           VALUE SPACES.    EL545
00155      12  WS-ABEND-FILE-STATUS PIC XX             VALUE SPACES.    EL545
00156      12  WS-ZERO             PIC S9              VALUE ZERO.      EL545
00157      12  PGM-SUB             PIC S999    COMP-3  VALUE +545.      EL545
00158                                                                   EL545
00159      12  GEN-AGENT-NAME      PIC X(30)   VALUE SPACES.            EL545
00160                                                                   EL545
00161      12  ZERO-ACCT-RANGE     PIC X(68)   VALUE SPACES.            EL545
00162                                                                   EL545
00163      12  PRIOR-CONTROL.                                           EL545
00164          16  PRIOR-COMP-CD   PIC X       VALUE LOW-VALUE.         EL545
00165          16  PRIOR-RCD-TYPE  PIC X       VALUE SPACE.             EL545
00166          16  PRIOR-REIN-CO   PIC XXX     VALUE SPACES.            EL545
00167          16  PRIOR-RPT-CD-1  PIC X(10)   VALUE SPACES.            EL545
00168          16  PRIOR-CARRIER   PIC X       VALUE SPACE.             EL545
00169          16  PRIOR-GROUPING  PIC X(6)    VALUE SPACES.            EL545
00170          16  PRIOR-GA-RPT-2  PIC X(10)   VALUE SPACES.            EL545
00171          16  PRIOR-STATE     PIC XX      VALUE SPACES.            EL545
00172          16  PRIOR-ACCOUNT   PIC X(10)   VALUE SPACES.            EL545
00173          16  PRIOR-REIN-SUB  PIC XXX     VALUE SPACES.            EL545
00174      12  FILE-RECORDS-FOUND  PIC X       VALUE 'N'.               EL545
00175          88  FILE-RECORDS   VALUE  'Y'.                           EL545
00176                                                                   EL545
00177  01  AM-FILE-STATUS          PIC XX      VALUE '00'.              EL545
00178  01  CO-FILE-STATUS          PIC XX      VALUE '00'.              EL545
00179  01  LR-FILE-STATUS.                                              EL545
00180      12  LR1                 PIC X       VALUE '0'.               EL545
00181      12  LR2                 PIC X       VALUE '0'.               EL545
00182                                                                   EL545
00183  01  AM-MASTER-KEY.                                               EL545
00184      12  AMK-CARRIER         PIC X       VALUE LOW-VALUE.         EL545
00185      12  AMK-GROUPING        PIC X(6)    VALUE LOW-VALUES.        EL545
00186      12  AMK-STATE           PIC XX      VALUE LOW-VALUES.        EL545
00187      12  AMK-ACCOUNT         PIC X(10)   VALUE LOW-VALUES.        EL545
00188                                                                   EL545
00189  01  CO-MASTER-KEY.                                               EL545
00190      12  COK-CARRIER         PIC X       VALUE SPACE.             EL545
00191      12  COK-GROUPING        PIC X(6)    VALUE SPACES.            EL545
00192      12  COK-GEN-AGENT       PIC X(10)   VALUE SPACES.            EL545
00193                                                                   EL545
00194  EJECT                                                            EL545
00195                              COPY ERCLOSS.                        EL545
00196                                                                   EL545
00197  EJECT                                                            EL545
00198  01  PRINT-LINES.                                                 EL545
00199      12  HDR-1.                                                   EL545
00200          16  FILLER          PIC X(48)       VALUE SPACES.        EL545
00201          16  FILLER          PIC X(76)       VALUE                EL545
00202             '   LOSS RATIO FILE LOAD'.                            EL545
00203          16  FILLER          PIC X(7)        VALUE 'EL545 '.         CL**2
00204                                                                   EL545
00205      12  HDR-2.                                                   EL545
00206          16  FILLER          PIC X(47)       VALUE SPACES.        EL545
00207          16  H2-COMPANY      PIC X(30).                           EL545
00208          16  FILLER          PIC X(47)       VALUE SPACES.        EL545
00209          16  H2-DATE         PIC X(8).                            EL545
00210                                                                   EL545
00211      12  HDR-3.                                                   EL545
00212          16  FILLER          PIC X(54)       VALUE SPACES.        EL545
00213          16  H3-DATE         PIC X(18).                           EL545
00214          16  FILLER          PIC X(41)       VALUE SPACES.        EL545
00215          16  FILLER          PIC X(7)        VALUE 'PAGE  1'.     EL545
00216                                                                   EL545
00217      12  DETAIL-LINE.                                             EL545
00218          16  DTL-1           PIC X(20)       VALUE                EL545
00219                        '   SUCCESSFUL LOAD  '.                    EL545
00220          16  FILLER          PIC X(15)       VALUE SPACES.        EL545
00221          16  DTL-RCD-COUNT   PIC Z,ZZZ,ZZ9.                       EL545
00222          16  FILLER          PIC X(11)       VALUE                EL545
00223                        '  RECORDS ('.                             EL545
00224          16  DTL-DEL-COUNT   PIC Z,ZZZ,ZZ9.                       EL545
00225          16  FILLER          PIC X(10)       VALUE                EL545
00226                        '  DELETED)'.                              EL545
00227                                                                   EL545
00228  EJECT                                                            EL545
00229                              COPY ELCDTECX.                       EL545
00230  EJECT                                                            EL545
00231                              COPY ELCDTEVR.                       EL545
00232  EJECT                                                            EL545
00233                              COPY ELCACCTV.                       EL545
00234                                                                   EL545
00235  EJECT                                                            EL545
00236  PROCEDURE DIVISION.                                              EL545
00237                                                                   EL545
00238  0000-READ-DATE.                                                  EL545
00239                              COPY ELCDTERX.                       EL545
00240                                                                   EL545
00241  0070-INITIALIZATION.                                             EL545
00242                                                                   EL545
00243      MOVE SPACES                    TO LR-ACCT-RANGES (1).        EL545
00244      MOVE ZEROS                     TO LR-AGT-NO (1 1)            EL545
00245                                        LR-AGT-NO (1 2)            EL545
00246                                        LR-AGT-NO (1 3)            EL545
00247                                        LR-SNG-PCT (1 1)           EL545
00248                                        LR-SNG-PCT (1 2)           EL545
00249                                        LR-SNG-PCT (1 3)           EL545
00250                                        LR-JNT-PCT (1 1)           EL545
00251                                        LR-JNT-PCT (1 2)           EL545
00252                                        LR-JNT-PCT (1 3)           EL545
00253                                        LR-A-H-PCT (1 1)           EL545
00254                                        LR-A-H-PCT (1 2)           EL545
00255                                        LR-A-H-PCT (1 3).          EL545
00256      MOVE LR-ACCT-RANGES (1)       TO ZERO-ACCT-RANGE.            EL545
00257                                                                   EL545
00258  SET-SORT-SEQUENCE SECTION.                                       EL545
00259                                                                   EL545
00260          SORT SORT-WORK ON ASCENDING SW-CONTROL                   EL545
00261          INPUT PROCEDURE SORT-INPUT-PROCEDURE                     EL545
00262          OUTPUT PROCEDURE SORT-OUTPUT-PROCEDURE.                  EL545
00263                                                                   EL545
00264      IF SORT-RETURN NOT = ZEROS                                   EL545
00265          MOVE '0101' TO WS-RETURN-CODE                            EL545
00266          GO TO ABEND-PGM.                                         EL545
00267                                                                   EL545
00268      GO TO 9000-PRINT-REPORT.                                     EL545
00269                                                                   EL545
00270  EJECT                                                            EL545
00271  SORT-INPUT-PROCEDURE SECTION.                                    EL545
00272                                                                   EL545
00273  0099-OPEN-FILES.                                                 EL545
00274                                                                   EL545
00275      OPEN INPUT LOSS-RATIOS-IN                                    EL545
00276                 ERACCTT                                           EL545
00277                 ERCOMP.                                           EL545
00278                                                                   EL545
00279      IF AM-FILE-STATUS  NOT = '00'  AND  '97'                     EL545
00280          MOVE AM-FILE-STATUS         TO WS-ABEND-FILE-STATUS      EL545
00281          MOVE 'ERACCTT OPEN ERROR'   TO WS-ABEND-MESSAGE          EL545
00282          GO TO ABEND-PGM.                                         EL545
00283                                                                   EL545
00284      IF CO-FILE-STATUS  NOT = '00'  AND  '97'                     EL545
00285          MOVE CO-FILE-STATUS         TO WS-ABEND-FILE-STATUS      EL545
00286          MOVE 'ERCOMP OPEN ERROR'    TO WS-ABEND-MESSAGE          EL545
00287          GO TO ABEND-PGM.                                         EL545
00288                                                                   EL545
00289                                                                   EL545
00290  0100-READ-LOSS-RATIOS.                                           EL545
00291                                                                   EL545
00292      READ LOSS-RATIOS-IN  INTO  LOSS-RATIO-MASTER                 EL545
00293                AT END CLOSE LOSS-RATIOS-IN                        EL545
00294                          GO TO INPUT-EXIT.                        EL545
00295                                                                   EL545
00296  0200-UPDATE-LOSS-RATIO-MASTER.                                   EL545
00297                                                                   EL545
00298      MOVE 'Y' TO FILE-RECORDS-FOUND.                              EL545
00299                                                                   EL545
00300      IF STATE-RECORD                                              EL545
00301          GO TO 0900-RELEASE-SORT-RECORD.                          EL545
00302      IF LR-ACCOUNT = LOW-VALUES                                   EL545
00303          GO TO 0800-SET-VG-CONTROL.                               EL545
00304                                                                   EL545
00305      MOVE LR-CARRIER                 TO AMK-CARRIER.              EL545
00306      MOVE LR-GROUPING                TO AMK-GROUPING.             EL545
00307      MOVE LR-STATE                   TO AMK-STATE.                EL545
00308      MOVE LR-ACCOUNT                 TO AMK-ACCOUNT.              EL545
00309                                                                   EL545
00310      MOVE LOW-VALUES                 TO AM-CONTROL-PRIMARY.       EL545
00311      MOVE DTE-CLASIC-COMPANY-CD      TO AM-COMPANY-CD.            EL545
00312      MOVE AM-MASTER-KEY              TO AM-CONTROL-A.             EL545
00313                                                                   EL545
00314      START ERACCTT  KEY NOT LESS THAN AM-CONTROL-PRIMARY.         EL545
00315                                                                   EL545
00316      IF AM-FILE-STATUS NOT = '00'                                 EL545
00317          MOVE AM-FILE-STATUS         TO WS-ABEND-FILE-STATUS      EL545
00318          MOVE 'ERACCTT START ERROR'  TO WS-ABEND-MESSAGE          EL545
00319          DISPLAY '*** AM-KEY - ' AM-CONTROL-PRIMARY               EL545
00320          GO TO ABEND-PGM.                                         EL545
00321                                                                   EL545
00322                                                                   EL545
00323  0300-ERACCTT-READ-NEXT.                                          EL545
00324                                                                   EL545
00325      READ ERACCTT   NEXT RECORD.                                  EL545
00326                                                                   EL545
00327      COPY ELCACCTI.                                               EL545
00328                                                                   EL545
00329      IF AM-FILE-STATUS  = '10'                                    EL545
00330          GO TO 0800-SET-VG-CONTROL.                               EL545
00331                                                                   EL545
00332      IF AM-FILE-STATUS NOT = '00'                                 EL545
00333          MOVE AM-FILE-STATUS         TO WS-ABEND-FILE-STATUS      EL545
00334          MOVE 'ERACCTT READ ERROR'   TO WS-ABEND-MESSAGE          EL545
00335          DISPLAY '*** AM-KEY - ' AM-CONTROL-PRIMARY               EL545
00336          GO TO ABEND-PGM.                                         EL545
00337                                                                   EL545
00338      IF AM-MASTER-KEY NOT = AM-CONTROL-A                          EL545
00339          GO TO 0800-SET-VG-CONTROL.                               EL545
00340                                                                   EL545
00341      MOVE AM-STATUS                  TO LR-ACCT-STATUS.           EL545
00342                                                                   EL545
00343      MOVE LR-ACCT-RANGES (1)         TO LR-ACCT-RANGES (2).       EL545
00344      MOVE ZERO-ACCT-RANGE            TO LR-ACCT-RANGES (1).       EL545
00345                                                                   EL545
00346      MOVE AM-EXP-DA                  TO LR-EXP-DATE (1) (5:2)     EL545
00347      MOVE AM-EXP-MO                  TO LR-EXP-DATE (1) (3:2)     EL545
00348      MOVE AM-EXP-YR                  TO LR-EXP-DATE (1) (1:2)     EL545
00349                                                                   EL545
00350      MOVE AM-REI-TABLE               TO LR-REI-TAB (1).           EL545
00351      MOVE AM-RET-Y-N                 TO LR-RETRO (1).             EL545
00352      MOVE AM-RET-P-E                 TO LR-BASIS (1).             EL545
00353                                                                   EL545
00354      MOVE +0                         TO X1.                       EL545
00355      MOVE +1                         TO X2.                       EL545
00356                                                                   EL545
00357  0400-G-A-COMM-LOOP.                                              EL545
00358                                                                   EL545
00359      ADD +1 TO X1.                                                EL545
00360      IF X1 GREATER THAN +10                                       EL545
00361          GO TO 0300-ERACCTT-READ-NEXT.                            EL545
00362                                                                   EL545
00363      IF AM-COM-TYP (X1) = 'R'  OR  'T'  OR  'W'                   EL545
00364                   OR 'U' OR 'V'                                   EL545
00365          GO TO 0400-G-A-COMM-LOOP.                                EL545
00366                                                                   EL545
00367  0500-LOAD-G-A-COMM-LOOP.                                         EL545
00368                                                                   EL545
00369      MOVE AM-AGT (X1)                TO LR-AGT-NO (1 X2).         EL545
00370      MOVE AM-L-COMA (X1)             TO LR-SNG-PCT-X (1 X2).      EL545
00371      MOVE AM-J-COMA (X1)             TO LR-JNT-PCT-X (1 X2).      EL545
00372      MOVE AM-A-COMA (X1)             TO LR-A-H-PCT-X (1 X2).      EL545
00373                                                                   EL545
00374      ADD +1 TO X2.                                                EL545
00375      IF X2 LESS THAN +4                                           EL545
00376          GO TO 0400-G-A-COMM-LOOP.                                EL545
00377                                                                   EL545
00378      GO TO 0300-ERACCTT-READ-NEXT.                                EL545
00379                                                                   EL545
00380                                                                   EL545
00381  0700-UPDATE-GA-CNTL.                                             EL545
00382                                                                   EL545
00383      IF DTE-COMPENSATION-ACCESS EQUAL '1' OR '3'                  EL545
00384          MOVE '0'                    TO LR-CARRIER.               EL545
00385                                                                   EL545
00386      IF DTE-COMPENSATION-ACCESS EQUAL '2' OR '3'                  EL545
00387          MOVE '000000'               TO LR-GROUPING.              EL545
00388                                                                   EL545
00389      IF DTE-COMP-VG = '3'  OR  '4'                                EL545
00390          MOVE LOW-VALUES         TO LR-STATE.                     EL545
00391                                                                   EL545
00392      PERFORM 0750-GET-G-A-NAME   THRU  0799-G-A-NAME-EXIT.        EL545
00393                                                                   EL545
00394  0749-UPDATE-CNTRL.                                               EL545
00395                                                                   EL545
00396  0750-GET-G-A-NAME.                                               EL545
00397                                                                   EL545
00398      MOVE LR-CARRIER                 TO COK-CARRIER.              EL545
00399      MOVE LR-GROUPING                TO COK-GROUPING.             EL545
00400      MOVE LR-GA-RPT-CD-2             TO COK-GEN-AGENT.            EL545
00401                                                                   EL545
00402      IF CO-MASTER-KEY = CO-CTL-1  AND                             EL545
00403         CO-GEN-AGENT-TYPE                                         EL545
00404          MOVE GEN-AGENT-NAME         TO LR-G-A-NAME               EL545
00405            GO TO 0799-G-A-NAME-EXIT.                              EL545
00406                                                                   EL545
00407      MOVE LOW-VALUES                 TO CO-CONTROL-PRIMARY.       EL545
00408      MOVE DTE-CLASIC-COMPANY-CD      TO CO-COMPANY-CD.            EL545
00409      MOVE CO-MASTER-KEY              TO CO-CTL-1.                 EL545
00410      MOVE 'G'                        TO CO-TYPE.                  EL545
00411                                                                   EL545
00412      READ ERCOMP.                                                 EL545
00413                                                                   EL545
00414      IF CO-FILE-STATUS  = '23'                                    EL545
00415          MOVE 'UNKNOWN GENERAL AGENT' TO LR-G-A-NAME              EL545
00416                                          GEN-AGENT-NAME           EL545
00417          GO TO 0799-G-A-NAME-EXIT.                                EL545
00418                                                                   EL545
00419      IF CO-FILE-STATUS NOT = '00'                                 EL545
00420          MOVE CO-FILE-STATUS         TO WS-ABEND-FILE-STATUS      EL545
00421          MOVE 'ERCOMP READ ERROR'    TO WS-ABEND-MESSAGE          EL545
00422          DISPLAY '*** CO-KEY - ' CO-CONTROL-PRIMARY               EL545
00423          GO TO ABEND-PGM.                                         EL545
00424                                                                   EL545
00425      MOVE CO-ACCT-NAME               TO LR-G-A-NAME               EL545
00426                                         GEN-AGENT-NAME.           EL545
00427                                                                   EL545
00428  0799-G-A-NAME-EXIT.                                              EL545
00429      EXIT.                                                        EL545
00430                                                                   EL545
00431                                                                   EL545
00432  0800-SET-VG-CONTROL.                                             EL545
00433                                                                   EL545
00434      IF G-A-RECORD                                                EL545
00435          PERFORM 0700-UPDATE-GA-CNTL THRU 0749-UPDATE-CNTRL       EL545
00436                GO TO 0900-RELEASE-SORT-RECORD.                    EL545
00437                                                                   EL545
00438      IF DTE-COMP-VG = ' '  OR  '3'                                EL545
00439          MOVE LOW-VALUES             TO LR-CARRIER.               EL545
00440                                                                   EL545
00441      IF DTE-COMP-VG = ' '  OR  '2'  OR  '3'  OR  '4'              EL545
00442          MOVE LOW-VALUES             TO LR-GROUPING.              EL545
00443                                                                   EL545
00444      IF DTE-COMP-VG = '3'  OR  '4'                                EL545
00445          MOVE LOW-VALUES             TO LR-STATE.                 EL545
00446                                                                   EL545
00447  0900-RELEASE-SORT-RECORD.                                        EL545
00448                                                                   EL545
00449      RELEASE SORT-RECORD  FROM  LOSS-RATIO-MASTER.                EL545
00450                                                                   EL545
00451                                                                   EL545
00452      GO TO 0100-READ-LOSS-RATIOS.                                 EL545
00453                                                                   EL545
00454  INPUT-EXIT.                                                      EL545
00455      EXIT.                                                        EL545
00456                                                                   EL545
00457  EJECT                                                            EL545
00458  SORT-OUTPUT-PROCEDURE SECTION.                                   EL545
00459                                                                   EL545
00460  1000-OPEN-OUTPUT-FILES.                                          EL545
00461                                                                   EL545
00462      OPEN I-O    ERLOSS                                           EL545
00463           OUTPUT PRINTER.                                         EL545
00464                                                                   EL545
00465      IF LR-FILE-STATUS  NOT = '00'  AND  '97'                     EL545
00466          MOVE LR-FILE-STATUS         TO WS-ABEND-FILE-STATUS      EL545
00467          MOVE 'ERLOSS OPEN ERROR'    TO WS-ABEND-MESSAGE          EL545
00468          GO TO ABEND-PGM.                                         EL545
00469                                                                   EL545
00470      PERFORM 1900-DELETE-OLD-LOSS-RATIOS THRU 1999-DELETE-XIT.    EL545
00471                                                                   EL545
00472 ***** READ THE SORT-WORK FILE TO ESTABLISH COMPARE FIELDS.        EL545
00473                                                                   EL545
00474      RETURN SORT-WORK                                             EL545
00475                 AT END                                            EL545
00476                   IF FILE-RECORDS                                 EL545
00477                      PERFORM 1400-COMPUTE-LOSS-RATIOS             EL545
00478                         THRU 1599-WRITE-LOSS-RATIO-XIT            EL545
00479                      GO TO 1600-OUTPUT-EXIT                       EL545
00480                   ELSE                                            EL545
00481                      MOVE SPACES TO DTL-1                         EL545
00482                      MOVE '    NO RECORDS FOUND' TO DTL-1         EL545
00483                      GO TO 1600-OUTPUT-EXIT                       EL545
00484                   END-IF.                                         EL545
00485                                                                   EL545
00486      MOVE SORT-RECORD            TO LOSS-RATIO-MASTER.            EL545
00487      MOVE LR-CONTROL             TO PRIOR-CONTROL.                EL545
00488                                                                   EL545
00489  1100-RETURN-SORTED-RECORDS.                                      EL545
00490                                                                   EL545
00491      RETURN SORT-WORK                                             EL545
00492                 AT END                                            EL545
00493                        PERFORM 1400-COMPUTE-LOSS-RATIOS           EL545
00494                           THRU 1599-WRITE-LOSS-RATIO-XIT          EL545
00495                        GO TO 1600-OUTPUT-EXIT.                    EL545
00496                                                                   EL545
00497  1100-CHECK-CONTROL.                                              EL545
00498                                                                   EL545
00499      IF SW-CONTROL = PRIOR-CONTROL                                EL545
00500          MOVE +0 TO X1                                            EL545
00501          PERFORM 1300-ACCUMULATE-TOTALS THRU                      EL545
00502                  1399-ACCUM-XIT        3  TIMES                   EL545
00503          GO TO 1100-RETURN-SORTED-RECORDS.                        EL545
00504                                                                   EL545
00505      PERFORM 1400-COMPUTE-LOSS-RATIOS THRU                        EL545
00506              1499-COMPUTE-LOSS-RATIO-XIT.                         EL545
00507                                                                   EL545
00508      PERFORM 1500-WRITE-LOSS-RATIO-MASTER THRU                    EL545
00509              1599-WRITE-LOSS-RATIO-XIT.                           EL545
00510                                                                   EL545
00511      MOVE SORT-RECORD            TO LOSS-RATIO-MASTER.            EL545
00512      MOVE LR-CONTROL             TO PRIOR-CONTROL.                EL545
00513                                                                   EL545
00514      GO TO 1100-RETURN-SORTED-RECORDS.                            EL545
00515                                                                   EL545
00516  1300-ACCUMULATE-TOTALS.                                          EL545
00517                                                                   EL545
00518      ADD +1 TO X1.                                                EL545
00519                                                                   EL545
00520      COMPUTE LR-YTD-NET (X1) =                                    EL545
00521                    LR-YTD-NET (X1) + SW-YTD-NET (X1).             EL545
00522      COMPUTE LR-YTD-EARN (X1) =                                   EL545
00523                    LR-YTD-EARN (X1) + SW-YTD-EARN (X1).           EL545
00524      COMPUTE LR-YTD-PAID (X1) =                                   EL545
00525                    LR-YTD-PAID (X1) + SW-YTD-PAID (X1).           EL545
00526      COMPUTE LR-YTD-RESV (X1) =                                   EL545
00527                    LR-YTD-RESV (X1) + SW-YTD-RESV (X1).           EL545
00528      COMPUTE LR-YTD-INCUR (X1) =                                  EL545
00529                    LR-YTD-INCUR (X1) + SW-YTD-INCUR (X1).         EL545
00530                                                                   EL545
00531      COMPUTE LR-ITD-NET (X1) =                                    EL545
00532                    LR-ITD-NET (X1) + SW-ITD-NET (X1).             EL545
00533      COMPUTE LR-ITD-EARN (X1) =                                   EL545
00534                    LR-ITD-EARN (X1) + SW-ITD-EARN (X1).           EL545
00535      COMPUTE LR-ITD-PAID (X1) =                                   EL545
00536                    LR-ITD-PAID (X1) + SW-ITD-PAID (X1).           EL545
00537      COMPUTE LR-ITD-RESV (X1) =                                   EL545
00538                    LR-ITD-RESV (X1) + SW-ITD-RESV (X1).           EL545
00539      COMPUTE LR-ITD-INCUR (X1) =                                  EL545
00540                    LR-ITD-INCUR (X1) + SW-ITD-INCUR (X1).         EL545
00541                                                                   EL545
00542  1399-ACCUM-XIT.                                                  EL545
00543      EXIT.                                                        EL545
00544                                                                   EL545
00545                                                                   EL545
00546                                                                   EL545
00547  1400-COMPUTE-LOSS-RATIOS.                                        EL545
00548                                                                   EL545
00549      IF LR-YTD-EARN (1) = ZEROS                                   EL545
00550          MOVE ZEROS                TO WS-LOSS                     EL545
00551      ELSE                                                         EL545
00552          COMPUTE WS-LOSS ROUNDED =                                EL545
00553                         LR-YTD-INCUR (1) / LR-YTD-EARN (1).       EL545
00554      COMPUTE LR-YTD-RATIO (1) = WS-LOSS * 100.                    EL545
00555      IF WS-LOSS GREATER THAN +9.999                               EL545
00556          MOVE +9999.9              TO LR-YTD-RATIO (1).           EL545
00557      IF WS-LOSS LESS THAN -9.999                                  EL545
00558          MOVE -9999.9              TO LR-YTD-RATIO (1).           EL545
00559                                                                   EL545
00560      IF LR-ITD-EARN (1) = ZEROS                                   EL545
00561          MOVE ZEROS                TO WS-LOSS                     EL545
00562      ELSE                                                         EL545
00563          COMPUTE WS-LOSS ROUNDED =                                EL545
00564                         LR-ITD-INCUR (1) / LR-ITD-EARN (1).       EL545
00565      COMPUTE LR-ITD-RATIO (1) = WS-LOSS * 100.                    EL545
00566      IF WS-LOSS GREATER THAN +9.999                               EL545
00567          MOVE +9999.9              TO LR-ITD-RATIO (1).           EL545
00568      IF WS-LOSS LESS THAN -9.999                                  EL545
00569          MOVE -9999.9              TO LR-ITD-RATIO (1).           EL545
00570                                                                   EL545
00571      IF LR-YTD-EARN (2) = ZEROS                                   EL545
00572          MOVE ZEROS                TO WS-LOSS                     EL545
00573      ELSE                                                         EL545
00574          COMPUTE WS-LOSS ROUNDED =                                EL545
00575                         LR-YTD-INCUR (2) / LR-YTD-EARN (2).       EL545
00576      COMPUTE LR-YTD-RATIO (2) = WS-LOSS * 100.                    EL545
00577      IF WS-LOSS GREATER THAN +9.999                               EL545
00578          MOVE +9999.9              TO LR-YTD-RATIO (2).           EL545
00579      IF WS-LOSS LESS THAN -9.999                                  EL545
00580          MOVE -9999.9              TO LR-YTD-RATIO (2).           EL545
00581                                                                   EL545
00582      IF LR-ITD-EARN (2) = ZEROS                                   EL545
00583          MOVE ZEROS                TO WS-LOSS                     EL545
00584      ELSE                                                         EL545
00585          COMPUTE WS-LOSS ROUNDED =                                EL545
00586                         LR-ITD-INCUR (2) / LR-ITD-EARN (2).       EL545
00587      COMPUTE LR-ITD-RATIO (2) = WS-LOSS * 100.                    EL545
00588      IF WS-LOSS GREATER THAN +9.999                               EL545
00589          MOVE +9999.9              TO LR-ITD-RATIO (2).           EL545
00590      IF WS-LOSS LESS THAN -9.999                                  EL545
00591          MOVE -9999.9              TO LR-ITD-RATIO (2).           EL545
00592                                                                   EL545
00593      IF LR-YTD-EARN (3) = ZEROS                                   EL545
00594          MOVE ZEROS                TO WS-LOSS                     EL545
00595      ELSE                                                         EL545
00596          COMPUTE WS-LOSS ROUNDED =                                EL545
00597                         LR-YTD-INCUR (3) / LR-YTD-EARN (3).       EL545
00598      COMPUTE LR-YTD-RATIO (3) = WS-LOSS * 100.                    EL545
00599      IF WS-LOSS GREATER THAN +9.999                               EL545
00600          MOVE +9999.9              TO LR-YTD-RATIO (3).           EL545
00601      IF WS-LOSS LESS THAN -9.999                                  EL545
00602          MOVE -9999.9              TO LR-YTD-RATIO (3).           EL545
00603                                                                   EL545
00604      IF LR-ITD-EARN (3) = ZEROS                                   EL545
00605          MOVE ZEROS                TO WS-LOSS                     EL545
00606      ELSE                                                         EL545
00607          COMPUTE WS-LOSS ROUNDED =                                EL545
00608                         LR-ITD-INCUR (3) / LR-ITD-EARN (3).       EL545
00609      COMPUTE LR-ITD-RATIO (3) = WS-LOSS * 100.                    EL545
00610      IF WS-LOSS GREATER THAN +9.999                               EL545
00611          MOVE +9999.9              TO LR-ITD-RATIO (3).           EL545
00612      IF WS-LOSS LESS THAN -9.999                                  EL545
00613          MOVE -9999.9              TO LR-ITD-RATIO (3).           EL545
00614                                                                   EL545
00615  1499-COMPUTE-LOSS-RATIO-XIT.                                     EL545
00616      EXIT.                                                        EL545
00617                                                                   EL545
00618                                                                   EL545
00619  1500-WRITE-LOSS-RATIO-MASTER.                                    EL545
00620                                                                   EL545
00621      MOVE LOSS-RATIO-MASTER        TO LOSS-RATIO-RECORD.          EL545
00622                                                                   EL545
00623      WRITE LOSS-RATIO-RECORD.                                     EL545
00624                                                                   EL545
00625      IF LR1 NOT = '0'                                             EL545
00626          MOVE LR-FILE-STATUS       TO WS-ABEND-FILE-STATUS        EL545
00627          MOVE 'ERLOSS WRITE ERROR' TO WS-ABEND-MESSAGE            EL545
00628          GO TO ABEND-PGM.                                         EL545
00629                                                                   EL545
00630      ADD +1 TO RECORD-COUNT.                                      EL545
00631                                                                   EL545
00632  1599-WRITE-LOSS-RATIO-XIT.                                       EL545
00633      EXIT.                                                        EL545
00634                                                                   EL545
00635  1600-OUTPUT-EXIT.                                                EL545
00636      EXIT.                                                        EL545
00637                                                                   EL545
00638  1900-DELETE-OLD-LOSS-RATIOS SECTION.                             EL545
00639                                                                   EL545
00640      MOVE LOW-VALUES               TO LR-CONTROL-PRIMARY.         EL545
00641      MOVE DTE-CLASIC-COMPANY-CD    TO LR-COMPANY-CODE.            EL545
00642                                                                   EL545
00643      START ERLOSS  KEY NOT LESS THAN LR-CONTROL-PRIMARY.          EL545
00644                                                                   EL545
00645      IF LR-FILE-STATUS = '10'  OR  '23'                           EL545
00646          GO TO 1999-DELETE-XIT.                                   EL545
00647                                                                   EL545
00648      IF LR-FILE-STATUS  NOT = '00'                                EL545
00649          MOVE LR-FILE-STATUS         TO WS-ABEND-FILE-STATUS      EL545
00650          MOVE 'ERLOSS START ERROR'   TO WS-ABEND-MESSAGE          EL545
00651          GO TO ABEND-PGM.                                         EL545
00652                                                                   EL545
00653  1910-ERLOSS-READ-NEXT.                                           EL545
00654                                                                   EL545
00655      READ ERLOSS  NEXT RECORD.                                    EL545
00656                                                                   EL545
00657      IF LR1 = '1'                                                 EL545
00658          GO TO 1999-DELETE-XIT.                                   EL545
00659                                                                   EL545
00660      IF LR-FILE-STATUS  NOT = '00'                                EL545
00661          MOVE LR-FILE-STATUS         TO WS-ABEND-FILE-STATUS      EL545
00662          MOVE 'ERLOSS READ ERROR '   TO WS-ABEND-MESSAGE          EL545
00663          GO TO ABEND-PGM.                                         EL545
00664                                                                   EL545
00665      IF DTE-CLASIC-COMPANY-CD NOT = LR-COMPANY-CODE               EL545
00666          GO TO 1999-DELETE-XIT.                                   EL545
00667                                                                   EL545
00668      DELETE ERLOSS  RECORD.                                       EL545
00669                                                                   EL545
00670      IF LR1 NOT = '0'                                             EL545
00671          MOVE LR-FILE-STATUS         TO WS-ABEND-FILE-STATUS      EL545
00672          MOVE 'ERLOSS DELETE ERROR'  TO WS-ABEND-MESSAGE          EL545
00673          GO TO ABEND-PGM.                                         EL545
00674                                                                   EL545
00675      ADD +1 TO DELETE-COUNT.                                      EL545
00676                                                                   EL545
00677      GO TO 1910-ERLOSS-READ-NEXT.                                 EL545
00678                                                                   EL545
00679  1999-DELETE-XIT.                                                 EL545
00680      EXIT.                                                        EL545
00681                                                                   EL545
00682                                                                   EL545
00683                                                                   EL545
00684  EJECT                                                            EL545
00685  ABEND-PGM SECTION.                                               EL545
00686                           COPY ELCABEND.                          EL545
00687                                                                   EL545
00688  9000-PRINT-REPORT.                                               EL545
00689                                                                   EL545
00690      MOVE COMPANY-NAME              TO H2-COMPANY.                EL545
00691      MOVE WS-CURRENT-DATE           TO H2-DATE.                   EL545
00692      MOVE ALPH-DATE                 TO H3-DATE.                   EL545
00693                                                                   EL545
00694      MOVE HDR-1                     TO PRT.                       EL545
00695      MOVE '1'                       TO X.                         EL545
00696      PERFORM 9800-PRINT-A-LINE THRU 9899-PRINT-XIT.               EL545
00697                                                                   EL545
00698      MOVE HDR-2                     TO PRT.                       EL545
00699      MOVE ' '                       TO X.                         EL545
00700      PERFORM 9800-PRINT-A-LINE THRU 9899-PRINT-XIT.               EL545
00701                                                                   EL545
00702      MOVE HDR-3                     TO PRT.                       EL545
00703      MOVE ' '                       TO X.                         EL545
00704      PERFORM 9800-PRINT-A-LINE THRU 9899-PRINT-XIT.               EL545
00705                                                                   EL545
00706      MOVE RECORD-COUNT              TO DTL-RCD-COUNT.             EL545
00707      MOVE DELETE-COUNT              TO DTL-DEL-COUNT.             EL545
00708      MOVE DETAIL-LINE               TO PRT.                       EL545
00709      MOVE '-'                       TO X.                         EL545
00710      PERFORM 9800-PRINT-A-LINE THRU 9899-PRINT-XIT.               EL545
00711                                                                   EL545
00712                                                                   EL545
00713      GO TO 9999-END-OF-JOB.                                       EL545
00714                                                                   EL545
00715                                                                   EL545
00716  9800-PRINT-A-LINE.                                               EL545
00717                                     COPY ELCPRT2.                 EL545
00718                                                                   EL545
00719  9899-PRINT-XIT.                                                  EL545
00720      EXIT.                                                        EL545
00721                                                                   EL545
00722                                                                   EL545
00723  9999-END-OF-JOB.                                                 EL545
00724                                                                   EL545
00725      CLOSE ERACCTT                                                EL545
00726            ERCOMP                                                 EL545
00727            ERLOSS                                                 EL545
00728            PRINTER.                                               EL545
00729                                                                   EL545
00730      IF AM-FILE-STATUS NOT = '00'                                 EL545
00731          MOVE AM-FILE-STATUS         TO WS-ABEND-FILE-STATUS      EL545
00732          MOVE 'ERACCTT CLOSE ERROR'  TO WS-ABEND-MESSAGE          EL545
00733          GO TO ABEND-PGM.                                         EL545
00734                                                                   EL545
00735      IF CO-FILE-STATUS  NOT = '00'                                EL545
00736          MOVE CO-FILE-STATUS         TO WS-ABEND-FILE-STATUS      EL545
00737          MOVE 'ERCOMP CLOSE ERROR'   TO WS-ABEND-MESSAGE          EL545
00738          GO TO ABEND-PGM.                                         EL545
00739                                                                   EL545
00740      IF LR-FILE-STATUS  NOT = '00'                                EL545
00741          MOVE LR-FILE-STATUS         TO WS-ABEND-FILE-STATUS      EL545
00742          MOVE 'ERLOSS CLOSE ERROR'   TO WS-ABEND-MESSAGE          EL545
00743          GO TO ABEND-PGM.                                         EL545
00744                                                                   EL545
00745                                      COPY ELCPRTC.                EL545
00746                                                                   EL545
00747      GOBACK.                                                      EL545
