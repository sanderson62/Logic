00001  IDENTIFICATION DIVISION.                                         05/14/98
00002                                                                   EL540
00003  PROGRAM-ID.                 EL540 .                                 LV005
00004 *              PROGRAM CONVERTED BY                               EL540
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL540
00006 *              CONVERSION DATE 02/12/96 16:38:58.                 EL540
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            EL540
00008 *                            VMOD=2.007.                          EL540
00009                                                                   EL540
00010 *AUTHOR. LOGIC, INC.                                              EL540
00011 *        DALLAS, TEXAS.                                           EL540
00012                                                                   EL540
00013 *DATE-COMPILED.                                                   EL540
00014                                                                   EL540
00015 *SECURITY.   *****************************************************   CL**3
00016 *            *                                                   *   CL**3
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**3
00018 *            *                                                   *   CL**3
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**3
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**3
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**3
00022 *            *                                                   *   CL**3
00023 *            *****************************************************   CL**3
00024                                                                      CL**3
00025 *REMARKS.  GENERATES, SORTS AND PRINTS DELINQUENT PREMIUM         EL540
00026 *          REPORT.                                                EL540
122002******************************************************************
122002*                   C H A N G E   L O G
122002*
122002* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
122002*-----------------------------------------------------------------
122002*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
122002* EFFECTIVE    NUMBER
122002*-----------------------------------------------------------------
122002* 122002                   PEMA  ADD MONTHLY PRODUCT PROCESSING  
122002******************************************************************
00027      EJECT                                                        EL540
00028  ENVIRONMENT DIVISION.                                            EL540
00029  CONFIGURATION SECTION.                                           EL540
00030  INPUT-OUTPUT SECTION.                                            EL540
00031  FILE-CONTROL.                                                    EL540
00032      SELECT SORT-WORK ASSIGN SYS001-UT-FBA1-S-SORTWK1.            EL540
00033                                                                   EL540
00034      SELECT EXTRACT-INTERFACE-FILE ASSIGN SYS010-UT-2400-S-SYS010.EL540
00035                                                                   EL540
00036      SELECT ERPNDB ASSIGN SYS024-FBA1-ERPNDB2                     EL540
00037          ORGANIZATION INDEXED                                     EL540
00038          ACCESS DYNAMIC                                           EL540
00039          RECORD KEY PB-CONTROL-BY-ACCT                            EL540
00040          FILE STATUS PB-STATUS.                                   EL540
00041                                                                   EL540
00042      SELECT ERACCT ASSIGN SYS011-FBA1-ERACCT                      EL540
00043          ORGANIZATION INDEXED                                     EL540
00044          ACCESS DYNAMIC                                           EL540
00045          RECORD KEY AM-CONTROL-PRIMARY                            EL540
00046          FILE STATUS AM-FILE-STATUS.                              EL540
00047                                                                   EL540
00048      SELECT ELREPT ASSIGN SYS010-FBA1-ELREPT                      EL540
00049          ORGANIZATION INDEXED                                     EL540
00050          ACCESS DYNAMIC                                           EL540
00051          RECORD KEY RF-CONTROL-PRIMARY                            EL540
00052          FILE STATUS DTE-VSAM-FLAGS.                              EL540
00053                                                                   EL540
00054      SELECT PRINT-FILE ASSIGN SYS008-UR-1403-S-SYS008.            EL540
00055                                                                   EL540
00056      SELECT FICH ASSIGN SYS020-UT-2400-S-SYS020.                  EL540
00057                                                                   EL540
00058      SELECT DISK-DATE ASSIGN SYS019-UT-FBA1-SYS019.               EL540
00059      EJECT                                                        EL540
00060  DATA DIVISION.                                                   EL540
00061  FILE SECTION.                                                    EL540
00062                                                                      CL**5
00063  FD  EXTRACT-INTERFACE-FILE                                       EL540
00064                                  COPY ERCEXTFD.                   EL540
00065  01  EXTRACT-INTERFACE-FILE-RECORD   PIC X(629).                  EL540
00066      EJECT                                                        EL540
00067  FD  ERPNDB.                                                      EL540
00068  01  PB-RECORD.                                                   EL540
00069      12  FILLER                  PIC X(13).                       EL540
00070      12  PB-CONTROL-BY-ACCT      PIC X(36).                       EL540
00071      12  FILLER                  PIC X(536).                      EL540
00072                                                                   EL540
00073  FD  ERACCT.                                                      EL540
00074                                  COPY ERCACCT.                    EL540
00075      EJECT                                                        EL540
00076  FD  ELREPT COPY ELCRPTFD.                                        EL540
00077                                  COPY ELCREPT.                    EL540
00078      EJECT                                                        EL540
00079  SD  SORT-WORK.                                                   EL540
00080  01  SORT-RECORD.                                                 EL540
00081      12  SORT-CTL.                                                EL540
00082          16  SC-CO               PIC X.                           EL540
00083          16  SC-CARR             PIC X.                           EL540
00084          16  SC-GRP              PIC X(6).                        EL540
00085          16  SC-ST               PIC XX.                          EL540
00086          16  SC-ACCT             PIC X(10).                       EL540
00087      12  SORT-DTA.                                                EL540
00088          16  FILLER              PIC X(20).                       EL540
00089          16  SC-CTR              PIC S9(9)V99 COMP-3 OCCURS 12.   EL540
00090      EJECT                                                        EL540
00091  FD  PRINT-FILE                  COPY ELCPRTFD.                   EL540
00092      EJECT                                                        EL540
00093  FD  FICH                        COPY ELCFCHFD.                   EL540
00094      EJECT                                                        EL540
00095  FD  DISK-DATE                   COPY ELCDTEFD.                   EL540
00096      EJECT                                                        EL540
00097  WORKING-STORAGE SECTION.                                         EL540
00098  77  LCP-ASA                       PIC X.                         EL540
00099  77  FILLER PIC X(32) VALUE '********************************'.   EL540
00100  77  FILLER PIC X(32) VALUE '*    WORKING-STORAGE EL540     *'.   EL540
00101  77  FILLER PIC X(32) VALUE '**********VMOD=2.007 ***********'.   EL540
00102                                                                   EL540
00103  77  WS-ZERO                     PIC S9 COMP-3 VALUE +0.          EL540
00104  77  WS-RETURN-CODE              PIC S9999 VALUE +0 COMP.         EL540
00105  77  WS-ABEND-FILE-STATUS        PIC XX VALUE ZERO.               EL540
00106  77  WS-ABEND-MESSAGE            PIC X(80) VALUE SPACES.          EL540
00107  77  X                           PIC X.                           EL540
00108  77  OLC-REPORT-NAME             PIC X(6) VALUE ' EL540'.         EL540
00109        EJECT                                                      EL540
00110                                  COPY ERCEXTR.                    EL540
00111        EJECT                                                      EL540
00112                                  COPY ERCPNDB.                    EL540
00113                                                                   EL540
00114        EJECT                                                      EL540
00115  01  PGM-SUB                     PIC S999 COMP VALUE +540.        EL540
00116  01  CONAMREC.                                                    EL540
00117      12  CONA-LOVAL              PIC X(20).                       EL540
00118      12  CONA-NAME               PIC X(30).                       EL540
00119      12  CONA-ANDT               PIC X(18).                       EL540
00120  01  XT-REC.                                                      EL540
00121      12  XT-CO                   PIC X.                           EL540
00122      12  XT-CARR                 PIC X.                           EL540
00123      12  XT-GRP                  PIC X(6).                        EL540
00124      12  XT-ST                   PIC XX.                          EL540
00125      12  XT-ACCT                 PIC X(10).                       EL540
00126      12  XT-NAME                 PIC X(20).                       EL540
00127      12  XT-CTR                  PIC S9(9)V99 COMP-3 OCCURS 12.   EL540
00128                                                                   EL540
00129  01  TIME-WARP.                                                   EL540
00130      12  TW-HR                   PIC 99.                          EL540
00131      12  TW-MN                   PIC 99.                          EL540
00132      12  TW-SC                   PIC 99.                          EL540
00133                                                                   EL540
00134  01  HD-1.                                                        EL540
00135      12  FILLER                  PIC X(12) VALUE SPACE.           EL540
00136      12  FILLER                  PIC X(8) VALUE SPACE.            EL540
00137      12  FILLER                  PIC X(32) VALUE SPACE.           EL540
00138      12  FILLER                  PIC X(67) VALUE                  EL540
00139              'DELINQUENT PREMIUM REPORT'.                         EL540
00140      12  FILLER                  PIC X(6) VALUE 'EL540 '.         EL540
00141                                                                   EL540
00142  01  HD-2.                                                        EL540
00143      12  FILLER                  PIC X(50) VALUE SPACE.           EL540
00144      12  FILLER                  PIC X(69) VALUE                  EL540
00145              'FOR BUSINESS RECEIVED THIS MONTH'.                  EL540
00146      12  H1-DATE                 PIC X(8).                        EL540
00147                                                                   EL540
00148  01  HD-3.                                                        EL540
00149      12  FILLER                  PIC X(51) VALUE SPACE.           EL540
00150      12  H3-CO                   PIC X(30).                       EL540
00151                                                                   EL540
00152  01  HD-4.                                                        EL540
00153      12  FILLER                  PIC X(57) VALUE SPACE.           EL540
00154      12  H4-ANDT                 PIC X(62).                       EL540
00155      12  FILLER                  PIC X(5) VALUE 'PAGE'.           EL540
00156      12  H4-PGCT                 PIC ZZZ9.                        EL540
00157                                                                   EL540
00158  01  HD-5.                                                        EL540
00159      12  FILLER                  PIC X(38) VALUE                  EL540
00160              'CARR GROUP ST ACCOUNT NO  ACCOUNT NAME'.            EL540
00161      12  FILLER                  PIC X(25) VALUE SPACE.           EL540
00162      12  FILLER                  PIC X(7)  VALUE 'WRITTEN'.       EL540
00163      12  FILLER                  PIC X(12) VALUE SPACE.           EL540
00164      12  FILLER                  PIC X(9)  VALUE 'CANCELLED'.     EL540
00165      12  FILLER                  PIC X(18) VALUE SPACE.           EL540
00166      12  FILLER                  PIC XXX   VALUE 'NET'.           EL540
00167                                                                   EL540
00168  01  DT-1.                                                        EL540
00169      12  FILLER                  PIC XX.                          EL540
00170      12  D1-CARR                 PIC XX.                          EL540
00171      12  D1-GRP                  PIC X(7).                        EL540
00172      12  D1-ST                   PIC XXX.                         EL540
00173      12  D1-ACCT                 PIC X(12).                       EL540
00174      12  D1-NAME                 PIC X(30).                       EL540
00175                                                                   EL540
00176  01  DT-2.                                                        EL540
00177      12  FILLER                  PIC X(21).                       EL540
00178      12  D2-VAR                  PIC X(36).                       EL540
00179      12  D2-CTRPCT               OCCURS 3.                        EL540
00180          16  D2-CTR              PIC ZZ,ZZZ,ZZZ.ZZ-.              EL540
00181          16  D2-PCT              PIC ZZZZ.Z-.                     EL540
00182                                                                   EL540
00183  01  MI-LINE.                                                     EL540
00184      12  FILLER                  PIC X(6) VALUE 'STATE'.          EL540
00185      12  MI-ST                   PIC XXX.                         EL540
00186      12  FILLER                  PIC X(6) VALUE 'TOTALS'.         EL540
00187                                                                   EL540
00188  01  IN-LINE.                                                     EL540
00189      12  FILLER                  PIC X(6) VALUE 'GROUP'.          EL540
00190      12  IN-GRP                  PIC XXXXXXX.                     EL540
00191      12  FILLER                  PIC X(6) VALUE 'TOTALS'.         EL540
00192                                                                   EL540
00193  01  MA-LINE.                                                     EL540
00194      12  FILLER                  PIC X(8) VALUE 'CARRIER'.        EL540
00195      12  MA-CARR                 PIC XX.                          EL540
00196      12  FILLER                  PIC X(6) VALUE 'TOTALS'.         EL540
00197        EJECT                                                      EL540
00198  01  PB-STATUS.                                                   EL540
00199      12  PB-STAT-1               PIC X.                           EL540
00200      12  PB-STAT-2               PIC X.                           EL540
00201                                                                   EL540
00202  01  AM-FILE-STATUS.                                              EL540
00203      12  AM-STAT-1               PIC X.                           EL540
00204      12  AM-STAT-2               PIC X.                           EL540
00205        EJECT                                                      EL540
00206  01  CTLKEY.                                                      EL540
00207      12  CTLCO                   PIC X.                           EL540
00208      12  CTLCARR                 PIC X.                           EL540
00209      12  CTLGRP                  PIC X(6).                        EL540
00210      12  CTLST                   PIC XX.                          EL540
00211      12  CTLACCT                 PIC X(10).                       EL540
00212                                                                   EL540
00213  01  PRVKEY.                                                      EL540
00214      12  PREVCO                  PIC X.                           EL540
00215      12  PREVCARR                PIC X.                           EL540
00216      12  PREVGRP                 PIC X(6).                        EL540
00217      12  PREVST                  PIC XX.                          EL540
00218      12  PREVACCT                PIC X(10) VALUE LOW-VALUES.      EL540
00219                                                                   EL540
00220  01  PGCT                        PIC 9999 VALUE ZERO.             EL540
00221  01  LNCT                        PIC 99 VALUE 90.                 EL540
00222  01  TTLS COMP-3.                                                 EL540
00223      12  TTL                     PIC S9(9)V99 COMP-3 OCCURS 12.   EL540
00224  01  CTRSETS.                                                     EL540
00225      12  CTRSET OCCURS 4.                                         EL540
00226          16  CTR                 PIC S9(9)V99 COMP-3 OCCURS 12.   EL540
00227  01  AX                          PIC 9999 COMP.                   EL540
00228  01  BX                          PIC 9999 COMP.                   EL540
00229  01  CX                          PIC 9999 COMP.                   EL540
00230  01  EOM-MOYR                    PIC S9(5).                       EL540
00231  01  ENT-MOYR                    PIC S9(5).                       EL540
00232  01  H-DATE.                                                         CL**5
00233      12  H-CCYY                  PIC 9(04).                       EL540
00234      12  H-CCYR REDEFINES H-CCYY.                                 EL540
00235          16  H-CC                PIC 99.                          EL540
00236          16  H-YR                PIC 99.                          EL540
00237      12  H-MO                    PIC 99.                          EL540
00238      12  H-DA                    PIC 99.                          EL540
00239  01  LVL-SW                      PIC 9       VALUE ZERO.          EL540
00240  01  Z-SW                        PIC X.                           EL540
00241  01  WK-CTR1                     PIC S9(9)V99.                    EL540
00242  01  WK-CTR2                     PIC S9(9)V99.                    EL540
00243  01  WKL                         PIC S9(5)V99 COMP-3.             EL540
00244  01  WKD                         PIC S9(5)V99 COMP-3.             EL540
00245  01  FSTX                        PIC 9 VALUE ZERO.                EL540
00246  01  WS-RUN-DT                   PIC XX.                          EL540
00247  01  EOJ-SW                      PIC 9 VALUE ZERO.                EL540
00248                                                                   EL540
00249        EJECT                                                      EL540
00250                                  COPY ELCDATE.                       CL**4
00251                                                                   EL540
00252                                  COPY ELCDTECX.                   EL540
00253                                                                   EL540
00254                                  COPY ELCDTEVR.                   EL540
00255        EJECT                                                      EL540
00256  PROCEDURE DIVISION.                                              EL540
00257                                                                   EL540
00258  0000-GET-DATE.                                                   EL540
00259                  COPY ELCDTERX.                                   EL540
00260                                                                   EL540
00261  0100-OPEN-FILES.                                                 EL540
00262      IF DTE-PGM-OPT NOT = 2                                       EL540
00263          OPEN INPUT ERPNDB                                        EL540
00264        ELSE                                                       EL540
00265          OPEN INPUT EXTRACT-INTERFACE-FILE.                       EL540
00266                                                                   EL540
00267      IF DTE-PGM-OPT    NOT = 2    AND                             EL540
00268         PB-STATUS      NOT = '00' AND                             EL540
00269         PB-STATUS      NOT = '97'                                 EL540
00270          MOVE PB-STATUS          TO WS-ABEND-FILE-STATUS          EL540
00271          MOVE 'ERROR OCCURRED OPEN - ERPNDB'                      EL540
00272                                  TO WS-ABEND-MESSAGE              EL540
00273          GO TO ABEND-PGM.                                         EL540
00274                                                                   EL540
00275      MOVE BIN-RUN-DATE           TO WS-RUN-DT.                    EL540
00276      OPEN INPUT ERACCT.                                           EL540
00277                                                                   EL540
00278      IF AM-FILE-STATUS  = '00' OR '97'                            EL540
00279          NEXT SENTENCE                                            EL540
00280        ELSE                                                       EL540
00281          MOVE AM-FILE-STATUS        TO WS-ABEND-FILE-STATUS       EL540
00282          MOVE 'ERACCT OPEN ERROR- ' TO WS-ABEND-MESSAGE           EL540
00283          GO TO ABEND-PGM.                                         EL540
00284                                                                   EL540
00285      EJECT                                                        EL540
00286  0120-SORT-RECS SECTION.                                          EL540
00287      SORT SORT-WORK                                               EL540
00288                     ASCENDING KEY SORT-CTL                        EL540
00289          INPUT PROCEDURE  0200-GET-RECS                           EL540
00290          OUTPUT PROCEDURE 1000-PUT-REPT.                          EL540
00291                                                                   EL540
00292      GO TO EOJ.                                                   EL540
00293                                                                   EL540
00294      EJECT                                                        EL540
00295  0200-GET-RECS SECTION.                                           EL540
00296      MOVE LOW-VALUES             TO CONA-LOVAL                    EL540
00297                                     PB-CONTROL-BY-ACCOUNT.        EL540
00298      MOVE COMPANY-NAME           TO CONA-NAME.                    EL540
00299      MOVE ALPH-DATE              TO CONA-ANDT.                    EL540
00300      MOVE CONAMREC               TO SORT-RECORD.                  EL540
00301      COMPUTE EOM-MOYR = (RUN-CCYY * 12) + RUN-MO.                    CL**5
00302                                                                   EL540
00303      PERFORM 0680-ZERO-TTLS.                                      EL540
00304                                                                   EL540
00305      MOVE DTE-CLASIC-COMPANY-CD  TO PB-COMPANY-CD-A1.             EL540
00306                                                                   EL540
00307      IF DTE-PGM-OPT = 2                                           EL540
00308          GO TO 0700-RELEASE-RECORD.                               EL540
00309                                                                   EL540
00310      MOVE PB-CONTROL-BY-ACCOUNT  TO PB-CONTROL-BY-ACCT.           EL540
00311                                                                   EL540
00312      START ERPNDB KEY NOT LESS PB-CONTROL-BY-ACCT.                EL540
00313                                                                   EL540
00314      IF PB-STATUS = '23'                                          EL540
00315          GO TO 0990-END-INPUT.                                    EL540
00316                                                                   EL540
00317      IF PB-STAT-1 NOT = '0'                                       EL540
00318          MOVE PB-STATUS          TO WS-ABEND-FILE-STATUS          EL540
00319          MOVE 'ERROR OCCURRED START - ERPNDB'                     EL540
00320                                  TO WS-ABEND-MESSAGE              EL540
00321          GO TO ABEND-PGM.                                         EL540
00322                                                                   EL540
00323      GO TO 0700-RELEASE-RECORD.                                   EL540
00324                                                                   EL540
00325      EJECT                                                        EL540
00326  0220-READ-PNDB.                                                  EL540
00327      IF DTE-PGM-OPT NOT = 2                                       EL540
00328          GO TO 0230-READ-PNDB.                                    EL540
00329                                                                   EL540
00330      READ EXTRACT-INTERFACE-FILE INTO EXTRACT-INTERFACE-RECORD    EL540
00331          AT END GO TO 0990-END-INPUT.                             EL540
00332                                                                   EL540
00333      IF EX-EXTRACT-CODE GREATER 'A'                               EL540
00334          GO TO 0990-END-INPUT.                                    EL540
00335                                                                   EL540
00336      IF EX-RECORD-TYPE GREATER 'E'                                EL540
00337          GO TO 0990-END-INPUT.                                    EL540
00338                                                                   EL540
00339      IF EX-COMPANY-CD LESS DTE-CLASIC-COMPANY-CD                  EL540
00340          GO TO 0220-READ-PNDB.                                    EL540
00341                                                                   EL540
00342      IF EX-COMPANY-CD GREATER DTE-CLASIC-COMPANY-CD               EL540
00343          GO TO 0990-END-INPUT.                                    EL540
00344                                                                   EL540
00345      IF EX-RECORD-TYPE NOT = 'A'                                  EL540
00346          GO TO 0220-READ-PNDB.                                    EL540
00347                                                                   EL540
00348      MOVE EX-DATA-AREAS          TO PENDING-BUSINESS.             EL540
00349                                                                   EL540
00350      IF PB-CREDIT-ACCEPT-DT = WS-RUN-DT OR LOW-VALUES             EL540
00351          NEXT SENTENCE                                            EL540
00352        ELSE                                                       EL540
00353          GO TO 0220-READ-PNDB.                                    EL540
00354                                                                   EL540
00355      GO TO 0250-READ-PNDB.                                        EL540
00356                                                                   EL540
00357  0230-READ-PNDB.                                                  EL540
00358      READ ERPNDB NEXT RECORD INTO PENDING-BUSINESS.               EL540
00359                                                                   EL540
00360      IF PB-STAT-1 = '1'                                           EL540
00361          GO TO 0990-END-INPUT.                                    EL540
00362                                                                   EL540
00363      IF PB-STAT-1 NOT = '0'                                       EL540
00364          MOVE PB-STATUS          TO WS-ABEND-FILE-STATUS          EL540
00365          MOVE 'ERROR OCCURRED READ - ERPNDB'                      EL540
00366                                  TO WS-ABEND-MESSAGE              EL540
00367          GO TO ABEND-PGM.                                         EL540
00368                                                                   EL540
00369      IF PB-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD                 EL540
00370          GO TO 0990-END-INPUT.                                    EL540
00371                                                                   EL540
00372  0250-READ-PNDB.                                                  EL540
00373      IF PB-ISSUE AND CLASIC-CREATED-CERT                          EL540
00374          GO TO 0220-READ-PNDB.                                    EL540
00375                                                                   EL540
00376      IF PB-BATCH-TRAILER                                          EL540
00377          GO TO 0220-READ-PNDB.                                    EL540
00378                                                                   EL540
00379      IF PB-ALT-CHG-SEQ-NO NOT = ZERO                              EL540
00380          GO TO 0220-READ-PNDB.                                    EL540
00381                                                                   EL540
00382      IF (PB-ISSUE OR PB-CANCELLATION) AND                         EL540
00383         (PB-POLICY-IS-DECLINED OR                                 EL540
00384          PB-POLICY-IS-VOIDED)                                     EL540
00385            GO TO 0220-READ-PNDB.                                  EL540
00386                                                                   EL540
00387                                                                   EL540
00388      IF (PB-ISSUE OR PB-CANCELLATION) AND                         EL540
00389         (PB-UNFORCED-ERRORS OR PB-FATAL-ERRORS OR                 EL540
00390          PB-RECORD-RETURNED OR                                    EL540
00391          PB-RECORD-ON-HOLD)                                       EL540
00392            GO TO 0220-READ-PNDB.                                  EL540
00393                                                                   EL540
00394      MOVE DTE-CLASIC-COMPANY-CD  TO CTLCO.                        EL540
00395      MOVE PB-SV-CARRIER          TO CTLCARR.                      EL540
00396      MOVE PB-SV-GROUPING         TO CTLGRP.                       EL540
00397      MOVE PB-SV-STATE            TO CTLST.                        EL540
00398      MOVE PB-ACCOUNT             TO CTLACCT.                      EL540
00399                                                                   EL540
00400      IF PREVACCT = LOW-VALUES                                     EL540
00401          MOVE CTLKEY             TO PRVKEY.                       EL540
00402                                                                   EL540
00403      IF CTLKEY  NOT = PRVKEY                                      EL540
00404          PERFORM 0740-MINOR THRU 0740-EXIT.                       EL540
00405                                                                   EL540
00406      IF PB-ISSUE OR PB-CANCELLATION                               EL540
00407          NEXT SENTENCE                                            EL540
00408        ELSE                                                       EL540
00409          GO TO 0220-READ-PNDB.                                    EL540
00410                                                                   EL540
00411      IF PB-ISSUE AND                                              EL540
122002        (PB-REIN-ONLY-CERT OR PB-REISSUED-CERT
122002         OR PB-MONTHLY-CERT)
00413          GO TO 0220-READ-PNDB.                                    EL540
00414                                                                   EL540
00415      MOVE PB-CREDIT-SELECT-DT    TO DC-BIN-DATE-1.                EL540
00416      MOVE SPACE                  TO DC-OPTION-CODE.               EL540
00417      PERFORM 0650-DATE-RTN.                                       EL540
00418      MOVE DC-GREG-DATE-CYMD      TO H-DATE.                          CL**5
00419      COMPUTE ENT-MOYR = (H-CCYY * 12) + H-MO.                        CL**5
00420      SUBTRACT ENT-MOYR FROM EOM-MOYR GIVING ENT-MOYR.             EL540
00421                                                                   EL540
00422      IF ENT-MOYR LESS ZERO OR GREATER 2                           EL540
00423          MOVE 10                 TO ENT-MOYR                      EL540
00424          GO TO BY-QTR.                                            EL540
00425                                                                   EL540
00426      IF PB-ISSUE                                                  EL540
00427          MOVE PB-CERT-EFF-DT     TO DC-BIN-DATE-1                 EL540
00428          GO TO 0260-COMPUTE-DIFF.                                 EL540
00429                                                                   EL540
00430      IF PB-C-LF-CANCEL-DT = PB-C-AH-CANCEL-DT                     EL540
00431          MOVE PB-C-LF-CANCEL-DT  TO DC-BIN-DATE-1                 EL540
00432              GO TO 0260-COMPUTE-DIFF.                             EL540
00433                                                                   EL540
00434      IF PB-C-LF-CANCEL-AMT = ZERO                                 EL540
00435          MOVE PB-C-AH-CANCEL-DT  TO DC-BIN-DATE-1                 EL540
00436          GO TO 0260-COMPUTE-DIFF.                                 EL540
00437                                                                   EL540
00438      IF PB-C-AH-CANCEL-AMT = ZERO                                 EL540
00439          MOVE PB-C-LF-CANCEL-DT  TO DC-BIN-DATE-1                 EL540
00440          GO TO 0260-COMPUTE-DIFF.                                 EL540
00441                                                                   EL540
00442      IF PB-C-LF-CANCEL-DT LESS THAN PB-C-AH-CANCEL-DT             EL540
00443          MOVE PB-C-LF-CANCEL-DT  TO DC-BIN-DATE-1                 EL540
00444      ELSE                                                         EL540
00445          MOVE PB-C-AH-CANCEL-DT  TO DC-BIN-DATE-1.                EL540
00446                                                                   EL540
00447  0260-COMPUTE-DIFF.                                               EL540
00448      MOVE SPACE                  TO DC-OPTION-CODE.               EL540
00449      PERFORM 0650-DATE-RTN.                                       EL540
00450      MOVE DC-GREG-DATE-CYMD      TO H-DATE.                          CL**5
00451      COMPUTE ENT-MOYR = (H-CCYY * 12) + H-MO.                        CL**5
00452      SUBTRACT ENT-MOYR FROM EOM-MOYR GIVING ENT-MOYR.             EL540
00453      ADD 1 TO ENT-MOYR.                                           EL540
00454                                                                   EL540
00455      IF ENT-MOYR LESS 1                                           EL540
00456          MOVE 1                  TO ENT-MOYR.                     EL540
00457                                                                   EL540
00458      IF ENT-MOYR GREATER 3                                        EL540
00459          MOVE 3                  TO ENT-MOYR.                     EL540
00460                                                                   EL540
00461      IF ENT-MOYR = 2                                              EL540
00462          ADD 2                   TO ENT-MOYR.                     EL540
00463                                                                   EL540
00464      IF ENT-MOYR = 3                                              EL540
00465          ADD 4                   TO ENT-MOYR.                     EL540
00466                                                                   EL540
00467  BY-QTR.                                                          EL540
00468      IF PB-CANCELLATION                                           EL540
00469          ADD 1                   TO ENT-MOYR.                     EL540
00470                                                                   EL540
00471      IF PB-ISSUE                                                  EL540
00472          PERFORM ADD-UP-ISSUE.                                    EL540
00473                                                                   EL540
00474      IF PB-CANCELLATION                                           EL540
00475          PERFORM ADD-UP-CANCEL.                                   EL540
00476                                                                   EL540
00477      GO TO 0220-READ-PNDB.                                        EL540
00478                                                                   EL540
00479  ADD-UP-ISSUE.                                                    EL540
00480      MOVE PB-I-LF-PREMIUM-AMT    TO WKL.                          EL540
00481      MOVE PB-I-AH-PREMIUM-AMT    TO WKD.                          EL540
00482                                                                   EL540
00483      IF PB-OVERRIDE-LIFE OR PB-OVERRIDE-BOTH                      EL540
00484          MOVE PB-I-LF-PREM-CALC  TO WKL.                          EL540
00485                                                                   EL540
00486      ADD WKL TO TTL (ENT-MOYR).                                   EL540
00487                                                                   EL540
00488      IF PB-OVERRIDE-AH OR PB-OVERRIDE-BOTH                        EL540
00489          MOVE PB-I-AH-PREM-CALC  TO WKD.                          EL540
00490                                                                   EL540
00491      ADD WKD TO TTL (ENT-MOYR).                                   EL540
00492                                                                   EL540
00493  ADD-UP-CANCEL.                                                   EL540
00494      MOVE PB-C-LF-CANCEL-AMT     TO WKL.                          EL540
00495      MOVE PB-C-AH-CANCEL-AMT     TO WKD.                          EL540
00496                                                                   EL540
00497      IF PB-OVERRIDE-LIFE OR PB-OVERRIDE-BOTH                      EL540
00498          MOVE PB-C-LF-REF-CALC   TO WKL.                          EL540
00499                                                                   EL540
00500      IF PB-OVERRIDE-AH OR PB-OVERRIDE-BOTH                        EL540
00501          MOVE PB-C-AH-REF-CALC   TO WKD.                          EL540
00502                                                                   EL540
00503      ADD WKL TO TTL (ENT-MOYR).                                   EL540
00504      ADD WKD TO TTL (ENT-MOYR).                                   EL540
00505                                                                   EL540
00506  0650-DATE-RTN.                                                   EL540
00507      CALL 'ELDATCX' USING DATE-CONVERSION-DATA.                   EL540
00508                                                                   EL540
00509  0680-ZERO-TTLS.                                                  EL540
00510      MOVE ZERO TO TTL (1) TTL (2) TTL (3) TTL (4) TTL (5)         EL540
00511                   TTL (6) TTL (7) TTL (8) TTL (9) TTL (10)        EL540
00512                   TTL (11) TTL (12).                              EL540
00513                                                                   EL540
00514  0700-RELEASE-RECORD.                                             EL540
00515      RELEASE SORT-RECORD.                                         EL540
00516      GO TO 0220-READ-PNDB.                                        EL540
00517                                                                   EL540
00518  0740-MINOR.                                                      EL540
00519      SUBTRACT TTL (2) FROM TTL (1) GIVING TTL (3).                EL540
00520      SUBTRACT TTL (5) FROM TTL (4) GIVING TTL (6).                EL540
00521      SUBTRACT TTL (8) FROM TTL (7) GIVING TTL (9).                EL540
00522      SUBTRACT TTL (11) FROM TTL (10) GIVING TTL (12).             EL540
00523      MOVE PRVKEY                 TO AM-CONTROL-PRIMARY.           EL540
00524      MOVE LOW-VALUE              TO AM-EXPIRATION-DT.             EL540
00525      START ERACCT KEY NOT LESS AM-CONTROL-PRIMARY.                EL540
00526                                                                   EL540
00527      IF AM-FILE-STATUS = '23'                                     EL540
00528          GO TO 0740-INVALID.                                      EL540
00529                                                                   EL540
00530      IF AM-STAT-1 NOT = '0'                                       EL540
00531          MOVE AM-FILE-STATUS          TO WS-ABEND-FILE-STATUS     EL540
00532          MOVE 'ERROR OCCURRED START - ERACCT' TO WS-ABEND-MESSAGE EL540
00533          GO TO ABEND-PGM.                                         EL540
00534                                                                   EL540
00535  NEXT-ACCT.                                                       EL540
00536      READ ERACCT NEXT RECORD.                                     EL540
00537                                                                   EL540
00538      IF AM-STAT-1 = '1'                                           EL540
00539          GO TO 0740-INVALID.                                      EL540
00540                                                                   EL540
00541      IF AM-STAT-1 NOT = '0'                                       EL540
00542          MOVE AM-FILE-STATUS          TO WS-ABEND-FILE-STATUS     EL540
00543          MOVE 'ERROR OCCURRED READ - ERACCT' TO WS-ABEND-MESSAGE  EL540
00544          GO TO ABEND-PGM.                                         EL540
00545                                                                   EL540
00546      IF PREVCO   = AM-COMPANY-CD AND                              EL540
00547         PREVCARR = AM-CARRIER    AND                              EL540
00548         PREVGRP  = AM-GROUPING   AND                              EL540
00549         PREVST   = AM-STATE      AND                              EL540
00550         PREVACCT = AM-ACCOUNT                                     EL540
00551          GO TO 0740-VALID.                                        EL540
00552                                                                   EL540
00553  0740-INVALID.                                                    EL540
00554      MOVE DTE-CLASIC-COMPANY-CD  TO XT-CO.                        EL540
00555      MOVE SPACES                 TO XT-CARR                       EL540
00556                                     XT-GRP                        EL540
00557                                     XT-ST.                        EL540
00558      MOVE '** INVALID ACCOUNT **' TO XT-NAME.                     EL540
00559                                                                   EL540
00560  0740-FINAL.                                                      EL540
00561      MOVE PREVACCT               TO XT-ACCT.                      EL540
00562      PERFORM MOV-TTL VARYING BX FROM 1 BY 1 UNTIL BX = 13.        EL540
00563      PERFORM 0680-ZERO-TTLS.                                      EL540
00564      MOVE XT-REC                 TO SORT-RECORD.                  EL540
00565      RELEASE SORT-RECORD.                                         EL540
00566      MOVE CTLKEY                 TO PRVKEY.                       EL540
00567      GO TO 0740-EXIT.                                             EL540
00568                                                                   EL540
00569  0740-VALID.                                                      EL540
00570      MOVE AM-COMPANY-CD          TO XT-CO.                        EL540
00571      MOVE AM-CARRIER             TO XT-CARR.                      EL540
00572      MOVE AM-GROUPING            TO XT-GRP.                       EL540
00573      MOVE AM-STATE               TO XT-ST.                        EL540
00574      MOVE AM-NAME                TO XT-NAME.                      EL540
00575      GO TO 0740-FINAL.                                            EL540
00576                                                                   EL540
00577  MOV-TTL.                                                         EL540
00578      MOVE TTL (BX)               TO XT-CTR (BX).                  EL540
00579                                                                   EL540
00580  0740-EXIT.                                                       EL540
00581      EXIT.                                                        EL540
00582                                                                   EL540
00583  0990-END-INPUT.                                                  EL540
00584      PERFORM 0740-MINOR THRU 0740-EXIT.                           EL540
00585                                                                   EL540
00586  0999-BEGIN-SORT.                                                 EL540
00587      EXIT.                                                        EL540
00588                                                                   EL540
00589  1000-PUT-REPT SECTION.                                           EL540
00590      RETURN SORT-WORK AT END                                      EL540
00591                       GO TO 1000-OUTPUT-EXIT.                     EL540
00592                                                                   EL540
00593      MOVE WS-CURRENT-DATE        TO H1-DATE.                      EL540
00594 *    ACCEPT WS-TIME-OF-DAY   FROM TIME.                           EL540
00595 *    MOVE WS-TIME                TO TIME-WARP.                    EL540
00596 *    MOVE TW-HR                  TO H1-HR.                        EL540
00597 *    MOVE TW-MN                  TO H1-MN.                        EL540
00598      MOVE SORT-RECORD            TO CONAMREC.                     EL540
00599      MOVE CONA-NAME              TO H3-CO.                        EL540
00600      MOVE CONA-ANDT              TO H4-ANDT.                      EL540
00601      PERFORM ZCTRS.                                               EL540
00602                                                                   EL540
00603      IF DTE-PRT-OPT = 'P' OR 'B' OR 'T'                           EL540
00604          OPEN OUTPUT PRINT-FILE.                                  EL540
00605                                                                   EL540
00606      MOVE LOW-VALUES TO PREVACCT.                                 EL540
00607                                                                   EL540
00608  PRINT-LOOP.                                                      EL540
00609      RETURN SORT-WORK AT END GO TO 1000-OUTPUT-EXIT.              EL540
00610                                                                   EL540
00611      IF PREVACCT = LOW-VALUES                                     EL540
00612          MOVE SC-CARR            TO PREVCARR                      EL540
00613          MOVE SC-GRP             TO PREVGRP                       EL540
00614          MOVE SC-ST              TO PREVST                        EL540
00615          MOVE SC-ACCT            TO PREVACCT.                     EL540
00616                                                                   EL540
00617      IF SC-CARR NOT = PREVCARR                                    EL540
00618          MOVE 3                  TO LVL-SW                        EL540
00619          PERFORM MINOREST THRU MAJOR.                             EL540
00620                                                                   EL540
00621      IF SC-GRP NOT = PREVGRP                                      EL540
00622          MOVE 2                  TO LVL-SW                        EL540
00623          PERFORM MINOREST THRU INT.                               EL540
00624                                                                   EL540
00625      IF SC-ST NOT = PREVST                                        EL540
00626          MOVE 1                  TO LVL-SW                        EL540
00627          PERFORM MINOREST THRU MINOR.                             EL540
00628                                                                   EL540
00629      IF SC-ACCT NOT = PREVACCT                                    EL540
00630          PERFORM MINOREST.                                        EL540
00631                                                                   EL540
00632      IF FSTX = ZERO                                               EL540
00633          MOVE 1                  TO FSTX                          EL540
00634          MOVE SORT-RECORD        TO XT-REC                        EL540
00635          GO TO PRINT-LOOP.                                        EL540
00636                                                                   EL540
00637      ADD SC-CTR (1)              TO XT-CTR (1).                   EL540
00638      ADD SC-CTR (2)              TO XT-CTR (2).                   EL540
00639      ADD SC-CTR (3)              TO XT-CTR (3).                   EL540
00640      ADD SC-CTR (4)              TO XT-CTR (4).                   EL540
00641      ADD SC-CTR (5)              TO XT-CTR (5).                   EL540
00642      ADD SC-CTR (6)              TO XT-CTR (6).                   EL540
00643      ADD SC-CTR (7)              TO XT-CTR (7).                   EL540
00644      ADD SC-CTR (8)              TO XT-CTR (8).                   EL540
00645      ADD SC-CTR (9)              TO XT-CTR (9).                   EL540
00646      ADD SC-CTR (10)             TO XT-CTR (10).                  EL540
00647      ADD SC-CTR (11)             TO XT-CTR (11).                  EL540
00648      ADD SC-CTR (12)             TO XT-CTR (12).                  EL540
00649      GO TO PRINT-LOOP.                                            EL540
00650                                                                   EL540
00651  PL-MINOREST.                                                     EL540
00652      IF LNCT GREATER 50                                           EL540
00653          PERFORM PGCHG.                                           EL540
00654                                                                   EL540
00655      MOVE SPACE                  TO DT-1.                         EL540
00656      MOVE XT-CARR                TO D1-CARR.                      EL540
00657      MOVE XT-GRP                 TO D1-GRP.                       EL540
00658      MOVE XT-ST                  TO D1-ST.                        EL540
00659      MOVE XT-ACCT                TO D1-ACCT.                      EL540
00660      MOVE XT-NAME                TO D1-NAME.                      EL540
00661      MOVE DT-1                   TO P-DATA.                       EL540
00662      MOVE ZERO                   TO P-CTL.                        EL540
00663      PERFORM 7000-PRINT-LINE.                                     EL540
00664                                                                   EL540
00665      MOVE SPACE                  TO DT-2.                         EL540
00666      MOVE '     THIS MONTH   '   TO D2-VAR.                       EL540
00667      MOVE XT-CTR (1)             TO D2-CTR (1).                   EL540
00668      MOVE XT-CTR (2)             TO D2-CTR (2).                   EL540
00669      MOVE XT-CTR (3)             TO D2-CTR (3).                   EL540
00670      MOVE XT-CTR (1)             TO WK-CTR1.                      EL540
00671      ADD XT-CTR (4)              TO WK-CTR1.                      EL540
00672      ADD XT-CTR (7)              TO WK-CTR1.                      EL540
00673      ADD XT-CTR (10)             TO WK-CTR1.                      EL540
00674      COMPUTE D2-PCT (1) ROUNDED =                                 EL540
00675          XT-CTR (1) * 100 / WK-CTR1 ON SIZE ERROR                 EL540
00676          MOVE ZERO               TO D2-PCT (1).                   EL540
00677                                                                   EL540
00678      MOVE XT-CTR (2)             TO WK-CTR2.                      EL540
00679      ADD XT-CTR (5)              TO WK-CTR2.                      EL540
00680      ADD XT-CTR (8)              TO WK-CTR2.                      EL540
00681      ADD XT-CTR (11)             TO WK-CTR2.                      EL540
00682      COMPUTE D2-PCT (2) ROUNDED =                                 EL540
00683          XT-CTR (2) * 100 / WK-CTR2 ON SIZE ERROR                 EL540
00684          MOVE ZERO               TO D2-PCT (2).                   EL540
00685                                                                   EL540
00686      MOVE DT-2                   TO P-DATA.                       EL540
00687      MOVE SPACE                  TO P-CTL.                           CL**2
00688      PERFORM 7000-PRINT-LINE.                                     EL540
00689                                                                   EL540
00690      MOVE '     LAST MONTH   '   TO D2-VAR.                       EL540
00691      MOVE XT-CTR (4)             TO D2-CTR (1).                   EL540
00692      MOVE XT-CTR (5)             TO D2-CTR (2).                   EL540
00693      MOVE XT-CTR (6)             TO D2-CTR (3).                   EL540
00694      COMPUTE D2-PCT (1) ROUNDED =                                 EL540
00695          XT-CTR (4) * 100 / WK-CTR1 ON SIZE ERROR                 EL540
00696          MOVE ZERO               TO D2-PCT (1).                   EL540
00697      COMPUTE D2-PCT (2) ROUNDED =                                 EL540
00698          XT-CTR (5) * 100 / WK-CTR2 ON SIZE ERROR                 EL540
00699          MOVE ZERO TO D2-PCT (2).                                 EL540
00700                                                                   EL540
00701      MOVE DT-2                   TO P-DATA.                       EL540
00702      MOVE SPACE                  TO P-CTL.                           CL**2
00703      PERFORM 7000-PRINT-LINE.                                     EL540
00704                                                                   EL540
00705      MOVE '     MONTH BEFORE LAST' TO D2-VAR.                     EL540
00706      MOVE XT-CTR (7)             TO D2-CTR (1).                   EL540
00707      MOVE XT-CTR (8)             TO D2-CTR (2).                   EL540
00708      MOVE XT-CTR (9)             TO D2-CTR (3).                   EL540
00709      COMPUTE D2-PCT (1) ROUNDED =                                 EL540
00710          XT-CTR (7) * 100 / WK-CTR1 ON SIZE ERROR                 EL540
00711          MOVE ZERO               TO D2-PCT (1).                   EL540
00712      COMPUTE D2-PCT (2) ROUNDED =                                 EL540
00713          XT-CTR (8) * 100 / WK-CTR2 ON SIZE ERROR                 EL540
00714          MOVE ZERO               TO D2-PCT (2).                   EL540
00715                                                                   EL540
00716      MOVE DT-2                   TO P-DATA.                       EL540
00717      MOVE SPACE                  TO P-CTL.                           CL**2
00718      PERFORM 7000-PRINT-LINE.                                     EL540
00719                                                                   EL540
00720      MOVE '     OTHER MONTHS   ' TO D2-VAR.                       EL540
00721      MOVE XT-CTR (10)            TO D2-CTR (1).                   EL540
00722      MOVE XT-CTR (11)            TO D2-CTR (2).                   EL540
00723      MOVE XT-CTR (12)            TO D2-CTR (3).                   EL540
00724      COMPUTE D2-PCT (1) ROUNDED =                                 EL540
00725          XT-CTR (10) * 100 / WK-CTR1 ON SIZE ERROR                EL540
00726          MOVE ZERO               TO D2-PCT (1).                   EL540
00727      COMPUTE D2-PCT (2) ROUNDED =                                 EL540
00728          XT-CTR (11) * 100 / WK-CTR2 ON SIZE ERROR                EL540
00729          MOVE ZERO               TO D2-PCT (2).                   EL540
00730                                                                   EL540
00731      MOVE DT-2                   TO P-DATA.                       EL540
00732      MOVE SPACE                  TO P-CTL.                           CL**2
00733      PERFORM 7000-PRINT-LINE.                                     EL540
00734                                                                   EL540
00735      ADD 6   TO LNCT.                                             EL540
00736      PERFORM ADD-MIN VARYING BX FROM 1 BY 1 UNTIL BX = 13.        EL540
00737                                                                   EL540
00738  ADD-MIN.                                                         EL540
00739      ADD XT-CTR (BX)             TO CTR (1, BX).                  EL540
00740                                                                   EL540
00741  ZCTRS.                                                           EL540
00742      MOVE 1  TO AX.                                               EL540
00743      PERFORM ZCTR VARYING BX FROM 1 BY 1 UNTIL BX = 13.           EL540
00744      MOVE 2  TO AX.                                               EL540
00745      PERFORM ZCTR VARYING BX FROM 1 BY 1 UNTIL BX = 13.           EL540
00746      MOVE 3  TO AX.                                               EL540
00747      PERFORM ZCTR VARYING BX FROM 1 BY 1 UNTIL BX = 13.           EL540
00748      MOVE 4 TO AX.                                                EL540
00749      PERFORM ZCTR VARYING BX FROM 1 BY 1 UNTIL BX = 13.           EL540
00750                                                                   EL540
00751  ZCTR.                                                            EL540
00752      MOVE ZERO                   TO CTR (AX, BX).                 EL540
00753                                                                   EL540
00754  ZCK.                                                             EL540
00755      IF CTR (AX, BX) NOT = ZERO                                   EL540
00756          MOVE 'N' TO Z-SW.                                        EL540
00757                                                                   EL540
00758  MINOREST.                                                        EL540
00759      PERFORM PL-MINOREST.                                         EL540
00760                                                                   EL540
00761      IF EOJ-SW = 1                                                EL540
00762          GO TO MINOR.                                             EL540
00763                                                                   EL540
00764      MOVE SORT-RECORD            TO XT-REC.                       EL540
00765      MOVE SC-ACCT                TO PREVACCT.                     EL540
00766      MOVE ZERO TO SC-CTR (1)  SC-CTR (2)  SC-CTR (3)              EL540
00767                   SC-CTR (4)  SC-CTR (5)  SC-CTR (6)              EL540
00768                   SC-CTR (7)  SC-CTR (8)  SC-CTR (9)              EL540
00769                   SC-CTR (10) SC-CTR (11) SC-CTR (12).            EL540
00770                                                                   EL540
00771  MINOR.                                                           EL540
00772      IF LNCT GREATER 50                                           EL540
00773          PERFORM PGCHG.                                           EL540
00774                                                                   EL540
00775      MOVE 2                      TO AX.                           EL540
00776      MOVE 'Y'                    TO Z-SW.                         EL540
00777      PERFORM ZCK VARYING BX FROM 1 BY 1 UNTIL BX = 13.            EL540
00778      MOVE PREVST                 TO MI-ST.                        EL540
00779      MOVE MI-LINE                TO DT-2.                         EL540
00780      MOVE '     THIS MONTH   '   TO D2-VAR.                       EL540
00781      MOVE CTR (1, 1)             TO D2-CTR (1).                   EL540
00782      MOVE CTR (1, 2)             TO D2-CTR (2).                   EL540
00783      MOVE CTR (1, 3)             TO D2-CTR (3).                   EL540
00784      MOVE CTR (1, 1)             TO WK-CTR1.                      EL540
00785      ADD CTR (1, 4)              TO WK-CTR1.                      EL540
00786      ADD CTR (1, 7)              TO WK-CTR1.                      EL540
00787      ADD CTR (1, 10)             TO WK-CTR1.                      EL540
00788      MOVE CTR (1, 2)             TO WK-CTR2.                      EL540
00789      ADD CTR (1, 5)              TO WK-CTR2.                      EL540
00790      ADD CTR (1, 8)              TO WK-CTR2.                      EL540
00791      ADD CTR (1, 11)             TO WK-CTR2.                      EL540
00792      COMPUTE D2-PCT (1) ROUNDED =                                 EL540
00793          CTR (1, 1) * 100 / WK-CTR1 ON SIZE ERROR                 EL540
00794          MOVE ZERO               TO D2-PCT (1).                   EL540
00795      COMPUTE D2-PCT (2) ROUNDED =                                 EL540
00796          CTR (1, 2) * 100 / WK-CTR2 ON SIZE ERROR                 EL540
00797          MOVE ZERO               TO D2-PCT (2).                   EL540
00798                                                                   EL540
00799      MOVE DT-2                   TO P-DATA.                       EL540
00800      MOVE ZERO                   TO P-CTL.                        EL540
00801                                                                   EL540
00802      IF LVL-SW GREATER 1 AND                                      EL540
00803         Z-SW = 'Y'                                                EL540
00804          NEXT SENTENCE                                            EL540
00805        ELSE                                                       EL540
00806          PERFORM 7000-PRINT-LINE.                                 EL540
00807                                                                   EL540
00808      MOVE SPACES                 TO DT-2.                         EL540
00809      MOVE '     LAST MONTH   '   TO D2-VAR.                       EL540
00810      COMPUTE D2-PCT (1) ROUNDED =                                 EL540
00811          CTR (1, 4) * 100 / WK-CTR1 ON SIZE ERROR                 EL540
00812          MOVE ZERO               TO D2-PCT (1).                   EL540
00813      COMPUTE D2-PCT (2) ROUNDED =                                 EL540
00814          CTR (1, 5) * 100 / WK-CTR2 ON SIZE ERROR                 EL540
00815          MOVE ZERO               TO D2-PCT (2).                   EL540
00816                                                                   EL540
00817      MOVE CTR (1, 4)             TO D2-CTR (1).                   EL540
00818      MOVE CTR (1, 5)             TO D2-CTR (2).                   EL540
00819      MOVE CTR (1, 6)             TO D2-CTR (3).                   EL540
00820      MOVE DT-2                   TO P-DATA.                       EL540
00821      MOVE SPACE                  TO P-CTL.                           CL**2
00822                                                                   EL540
00823      IF LVL-SW GREATER 1 AND Z-SW = 'Y' NEXT SENTENCE ELSE        EL540
00824          PERFORM 7000-PRINT-LINE.                                 EL540
00825                                                                   EL540
00826      MOVE '     MONTH BEFORE LAST   ' TO D2-VAR.                  EL540
00827      MOVE CTR (1, 7)             TO D2-CTR (1).                   EL540
00828      MOVE CTR (1, 8)             TO D2-CTR (2).                   EL540
00829      MOVE CTR (1, 9)             TO D2-CTR (3).                   EL540
00830      COMPUTE D2-PCT (1) ROUNDED =                                 EL540
00831          CTR (1, 7) * 100 / WK-CTR1 ON SIZE ERROR                 EL540
00832          MOVE ZERO               TO D2-PCT (1).                   EL540
00833      COMPUTE D2-PCT (2) ROUNDED =                                 EL540
00834          CTR (1, 8) * 100 / WK-CTR2 ON SIZE ERROR                 EL540
00835          MOVE ZERO               TO D2-PCT (2).                   EL540
00836                                                                   EL540
00837      MOVE DT-2                   TO P-DATA.                       EL540
00838      MOVE SPACE                  TO P-CTL.                           CL**2
00839                                                                   EL540
00840      IF LVL-SW GREATER 1 AND Z-SW = 'Y'                           EL540
00841          NEXT SENTENCE                                            EL540
00842        ELSE                                                       EL540
00843          PERFORM 7000-PRINT-LINE.                                 EL540
00844                                                                   EL540
00845      MOVE '     OTHER MONTHS   ' TO D2-VAR.                       EL540
00846      MOVE CTR (1, 10)            TO D2-CTR (1).                   EL540
00847      MOVE CTR (1, 11)            TO D2-CTR (2).                   EL540
00848      MOVE CTR (1, 12)            TO D2-CTR (3).                   EL540
00849      COMPUTE D2-PCT (1) ROUNDED =                                 EL540
00850          CTR (1, 10) * 100 / WK-CTR1 ON SIZE ERROR                EL540
00851          MOVE ZERO               TO D2-PCT (1).                   EL540
00852      COMPUTE D2-PCT (2) ROUNDED =                                 EL540
00853          CTR (1, 11) * 100 / WK-CTR2 ON SIZE ERROR                EL540
00854          MOVE ZERO               TO D2-PCT (2).                   EL540
00855                                                                   EL540
00856      MOVE DT-2                   TO P-DATA.                       EL540
00857      MOVE SPACE                  TO P-CTL.                           CL**2
00858                                                                   EL540
00859      IF LVL-SW GREATER 1 AND Z-SW = 'Y'                           EL540
00860          NEXT SENTENCE                                            EL540
00861         ELSE                                                      EL540
00862          PERFORM 7000-PRINT-LINE.                                 EL540
00863                                                                   EL540
00864      MOVE 1                      TO AX.                           EL540
00865      MOVE 2                      TO CX.                           EL540
00866      PERFORM ROLL VARYING BX FROM 1 BY 1 UNTIL BX = 13.           EL540
00867                                                                   EL540
00868      IF EOJ-SW = ZERO                                             EL540
00869          MOVE SC-ST              TO PREVST.                       EL540
00870                                                                   EL540
00871      IF LVL-SW GREATER 1 AND Z-SW = 'Y'                           EL540
00872          NEXT SENTENCE                                            EL540
00873         ELSE                                                      EL540
00874          ADD 5 TO LNCT.                                           EL540
00875                                                                   EL540
00876  INT.                                                             EL540
00877      IF LNCT GREATER 50                                           EL540
00878          PERFORM PGCHG.                                           EL540
00879                                                                   EL540
00880      MOVE 3                      TO AX.                           EL540
00881      MOVE 'Y'                    TO Z-SW.                         EL540
00882      PERFORM ZCK VARYING BX FROM 1 BY 1 UNTIL BX = 13.            EL540
00883      MOVE PREVGRP                TO IN-GRP.                       EL540
00884      MOVE IN-LINE                TO DT-2.                         EL540
00885      MOVE '     THIS MONTH   '   TO D2-VAR.                       EL540
00886      MOVE CTR (2, 1)             TO D2-CTR (1).                   EL540
00887      MOVE CTR (2, 2)             TO D2-CTR (2).                   EL540
00888      MOVE CTR (2, 3)             TO D2-CTR (3).                   EL540
00889      MOVE CTR (2, 1)             TO WK-CTR1.                      EL540
00890      ADD CTR (2, 4)              TO WK-CTR1.                      EL540
00891      ADD CTR (2, 7)              TO WK-CTR1.                      EL540
00892      ADD CTR (2, 10)             TO WK-CTR1.                      EL540
00893      MOVE CTR (2, 2)             TO WK-CTR2.                      EL540
00894      ADD CTR (2, 5)              TO WK-CTR2.                      EL540
00895      ADD CTR (2, 8)              TO WK-CTR2.                      EL540
00896      ADD CTR (2, 11)             TO WK-CTR2.                      EL540
00897                                                                   EL540
00898      COMPUTE D2-PCT (1) ROUNDED =                                 EL540
00899          CTR (2, 1) * 100 / WK-CTR1 ON SIZE ERROR                 EL540
00900          MOVE ZERO               TO D2-PCT (1).                   EL540
00901      COMPUTE D2-PCT (2) ROUNDED =                                 EL540
00902          CTR (2, 2) * 100 / WK-CTR2 ON SIZE ERROR                 EL540
00903          MOVE ZERO               TO D2-PCT (2).                   EL540
00904                                                                   EL540
00905      MOVE DT-2                   TO P-DATA.                       EL540
00906      MOVE ZERO                   TO P-CTL.                        EL540
00907                                                                   EL540
00908      IF LVL-SW GREATER 2 AND Z-SW = 'Y'                           EL540
00909          NEXT SENTENCE                                            EL540
00910         ELSE                                                      EL540
00911          PERFORM 7000-PRINT-LINE.                                 EL540
00912                                                                   EL540
00913      MOVE SPACE                  TO DT-2.                         EL540
00914      MOVE '     LAST MONTH   '   TO D2-VAR.                       EL540
00915      MOVE CTR (2, 4)             TO D2-CTR (1).                   EL540
00916      MOVE CTR (2, 5)             TO D2-CTR (2).                   EL540
00917      MOVE CTR (2, 6)             TO D2-CTR (3).                   EL540
00918                                                                   EL540
00919      COMPUTE D2-PCT (1) ROUNDED =                                 EL540
00920          CTR (2, 4) * 100 / WK-CTR1 ON SIZE ERROR                 EL540
00921          MOVE ZERO               TO D2-PCT (1).                   EL540
00922      COMPUTE D2-PCT (2) ROUNDED =                                 EL540
00923          CTR (2, 5) * 100 / WK-CTR2 ON SIZE ERROR                 EL540
00924          MOVE ZERO               TO D2-PCT (2).                   EL540
00925                                                                   EL540
00926      MOVE DT-2                   TO P-DATA.                       EL540
00927      MOVE SPACE                  TO P-CTL.                           CL**2
00928                                                                   EL540
00929      IF LVL-SW GREATER 2 AND Z-SW = 'Y'                           EL540
00930          NEXT SENTENCE                                            EL540
00931         ELSE                                                      EL540
00932          PERFORM 7000-PRINT-LINE.                                 EL540
00933                                                                   EL540
00934      MOVE '     MONTH BEFORE LAST   ' TO D2-VAR.                  EL540
00935      MOVE CTR (2, 7)             TO D2-CTR (1).                   EL540
00936      MOVE CTR (2, 8)             TO D2-CTR (2).                   EL540
00937      MOVE CTR (2, 9)             TO D2-CTR (3).                   EL540
00938      COMPUTE D2-PCT (1) ROUNDED =                                 EL540
00939          CTR (2, 7) * 100 / WK-CTR1 ON SIZE ERROR                 EL540
00940          MOVE ZERO               TO D2-PCT (1).                   EL540
00941      COMPUTE D2-PCT (2) ROUNDED =                                 EL540
00942          CTR (2, 8) * 100 / WK-CTR2 ON SIZE ERROR                 EL540
00943          MOVE ZERO               TO D2-PCT (2).                   EL540
00944      MOVE DT-2                   TO P-DATA.                       EL540
00945      MOVE SPACE                  TO P-CTL.                           CL**2
00946                                                                   EL540
00947      IF LVL-SW GREATER 2 AND Z-SW = 'Y'                           EL540
00948          NEXT SENTENCE                                            EL540
00949         ELSE                                                      EL540
00950          PERFORM 7000-PRINT-LINE.                                 EL540
00951                                                                   EL540
00952      MOVE '     OTHER MONTHS   ' TO D2-VAR.                       EL540
00953      MOVE CTR (2, 10)            TO D2-CTR (1).                   EL540
00954      MOVE CTR (2, 11)            TO D2-CTR (2).                   EL540
00955      MOVE CTR (2, 12)            TO D2-CTR (3).                   EL540
00956      COMPUTE D2-PCT (1) ROUNDED =                                 EL540
00957          CTR (2, 10) * 100 / WK-CTR1 ON SIZE ERROR                EL540
00958          MOVE ZERO               TO D2-PCT (1).                   EL540
00959      COMPUTE D2-PCT (2) ROUNDED =                                 EL540
00960          CTR (2, 11) * 100 / WK-CTR2 ON SIZE ERROR                EL540
00961          MOVE ZERO               TO D2-PCT (2).                   EL540
00962      MOVE DT-2                   TO P-DATA.                       EL540
00963      MOVE SPACE                  TO P-CTL.                           CL**2
00964                                                                   EL540
00965      IF LVL-SW GREATER 2 AND Z-SW = 'Y'                           EL540
00966          NEXT SENTENCE                                            EL540
00967         ELSE                                                      EL540
00968          PERFORM 7000-PRINT-LINE.                                 EL540
00969                                                                   EL540
00970      MOVE 2                      TO AX.                           EL540
00971      MOVE 3                      TO CX.                           EL540
00972      PERFORM ROLL VARYING BX FROM 1 BY 1 UNTIL BX = 13.           EL540
00973                                                                   EL540
00974      IF EOJ-SW = ZERO                                             EL540
00975          MOVE SC-GRP             TO PREVGRP.                      EL540
00976                                                                   EL540
00977      IF LVL-SW GREATER 2 AND Z-SW = 'Y'                           EL540
00978          NEXT SENTENCE                                            EL540
00979         ELSE                                                      EL540
00980          ADD 5 TO LNCT.                                           EL540
00981                                                                   EL540
00982  MAJOR.                                                           EL540
00983      IF LNCT GREATER 50                                           EL540
00984          PERFORM PGCHG.                                           EL540
00985                                                                   EL540
00986      MOVE PREVCARR               TO MA-CARR.                      EL540
00987      MOVE MA-LINE                TO DT-2.                         EL540
00988      MOVE '     THIS MONTH   '   TO D2-VAR.                       EL540
00989      MOVE CTR (3, 1)             TO D2-CTR (1).                   EL540
00990      MOVE CTR (3, 2)             TO D2-CTR (2).                   EL540
00991      MOVE CTR (3, 3)             TO D2-CTR (3).                   EL540
00992      MOVE CTR (3, 1)             TO WK-CTR1.                      EL540
00993      ADD CTR (3, 4)              TO WK-CTR1.                      EL540
00994      ADD CTR (3, 7)              TO WK-CTR1.                      EL540
00995      ADD CTR (3, 10)             TO WK-CTR1.                      EL540
00996      COMPUTE D2-PCT (1) ROUNDED =                                 EL540
00997          CTR (3, 1) * 100 / WK-CTR1 ON SIZE ERROR                 EL540
00998          MOVE ZERO               TO D2-PCT (1).                   EL540
00999      MOVE CTR (3, 2)             TO WK-CTR2.                      EL540
01000      ADD CTR (3, 5)              TO WK-CTR2.                      EL540
01001      ADD CTR (3, 8)              TO WK-CTR2.                      EL540
01002      ADD CTR (3, 11)             TO WK-CTR2.                      EL540
01003                                                                   EL540
01004      COMPUTE D2-PCT (2) ROUNDED =                                 EL540
01005          CTR (3, 2) * 100 / WK-CTR2 ON SIZE ERROR                 EL540
01006          MOVE ZERO               TO D2-PCT (2).                   EL540
01007                                                                   EL540
01008      MOVE DT-2                   TO P-DATA.                       EL540
01009      MOVE ZERO                   TO P-CTL.                        EL540
01010      PERFORM 7000-PRINT-LINE.                                     EL540
01011      MOVE SPACE                  TO DT-2.                         EL540
01012      MOVE '     LAST MONTH   '   TO D2-VAR.                       EL540
01013      MOVE CTR (3, 4)             TO D2-CTR (1).                   EL540
01014      MOVE CTR (3, 5)             TO D2-CTR (2).                   EL540
01015      MOVE CTR (3, 6)             TO D2-CTR (3).                   EL540
01016                                                                   EL540
01017      COMPUTE D2-PCT (1) ROUNDED =                                 EL540
01018          CTR (3, 4) * 100 / WK-CTR1 ON SIZE ERROR                 EL540
01019          MOVE ZERO               TO D2-PCT (1).                   EL540
01020      COMPUTE D2-PCT (2) ROUNDED =                                 EL540
01021          CTR (3, 5) * 100 / WK-CTR2 ON SIZE ERROR                 EL540
01022          MOVE ZERO               TO D2-PCT (2).                   EL540
01023                                                                   EL540
01024      MOVE DT-2                   TO P-DATA.                       EL540
01025      MOVE SPACE                  TO P-CTL.                           CL**2
01026      PERFORM 7000-PRINT-LINE.                                     EL540
01027      MOVE '     MONTH BEFORE LAST   ' TO D2-VAR.                  EL540
01028      MOVE CTR (3, 7)             TO D2-CTR (1).                   EL540
01029      MOVE CTR (3, 8)             TO D2-CTR (2).                   EL540
01030      MOVE CTR (3, 9)             TO D2-CTR (3).                   EL540
01031                                                                   EL540
01032      COMPUTE D2-PCT (1) ROUNDED =                                 EL540
01033          CTR (3, 7) * 100 / WK-CTR1 ON SIZE ERROR                 EL540
01034          MOVE ZERO               TO D2-PCT (1).                   EL540
01035      COMPUTE D2-PCT (2) ROUNDED =                                 EL540
01036          CTR (3, 8) * 100 / WK-CTR2 ON SIZE ERROR                 EL540
01037          MOVE ZERO               TO D2-PCT (2).                   EL540
01038                                                                   EL540
01039      MOVE DT-2                   TO P-DATA.                       EL540
01040      MOVE SPACE                  TO P-CTL.                           CL**2
01041      PERFORM 7000-PRINT-LINE.                                     EL540
01042      MOVE '     OTHER MONTHS   ' TO D2-VAR.                       EL540
01043      MOVE CTR (3, 10)            TO D2-CTR (1).                   EL540
01044      MOVE CTR (3, 11)            TO D2-CTR (2).                   EL540
01045      MOVE CTR (3, 12)            TO D2-CTR (3).                   EL540
01046                                                                   EL540
01047      COMPUTE D2-PCT (1) ROUNDED =                                 EL540
01048          CTR (3, 10) * 100 / WK-CTR1 ON SIZE ERROR                EL540
01049          MOVE ZERO               TO D2-PCT (1).                   EL540
01050      COMPUTE D2-PCT (2) ROUNDED =                                 EL540
01051          CTR (3, 11) * 100 / WK-CTR2 ON SIZE ERROR                EL540
01052          MOVE ZERO               TO D2-PCT (2).                   EL540
01053                                                                   EL540
01054      MOVE DT-2                   TO P-DATA.                       EL540
01055      MOVE SPACE                  TO P-CTL.                           CL**2
01056      PERFORM 7000-PRINT-LINE.                                     EL540
01057      MOVE 3                      TO AX.                           EL540
01058      MOVE 4                      TO CX.                           EL540
01059      PERFORM ROLL VARYING BX FROM 1 BY 1 UNTIL BX = 13.           EL540
01060                                                                   EL540
01061      IF EOJ-SW = ZERO                                             EL540
01062          MOVE SC-CARR            TO PREVCARR.                     EL540
01063                                                                   EL540
01064      MOVE 90                     TO LNCT.                         EL540
01065                                                                   EL540
01066  PGCHG.                                                           EL540
01067      MOVE HD-1                   TO P-DATA.                       EL540
01068      MOVE '1'                    TO P-CTL.                        EL540
01069      PERFORM 7000-PRINT-LINE.                                     EL540
01070      ADD 1   TO PGCT.                                             EL540
01071      MOVE ZERO                   TO LNCT.                         EL540
01072      MOVE HD-2                   TO P-DATA.                       EL540
01073      MOVE SPACE                  TO P-CTL.                           CL**2
01074      PERFORM 7000-PRINT-LINE.                                     EL540
01075      MOVE HD-3                   TO P-DATA.                       EL540
01076      MOVE SPACE                  TO P-CTL.                           CL**2
01077      PERFORM 7000-PRINT-LINE.                                     EL540
01078      MOVE PGCT                   TO H4-PGCT.                      EL540
01079      MOVE HD-4                   TO P-DATA.                       EL540
01080      MOVE SPACE                  TO P-CTL.                           CL**2
01081      PERFORM 7000-PRINT-LINE.                                     EL540
01082      MOVE HD-5                   TO P-DATA.                       EL540
01083      MOVE ZERO                   TO P-CTL.                        EL540
01084      PERFORM 7000-PRINT-LINE.                                     EL540
01085                                                                   EL540
01086  ROLL.                                                            EL540
01087      ADD CTR (AX, BX)            TO CTR (CX, BX).                 EL540
01088      MOVE ZERO                   TO CTR (AX, BX).                 EL540
01089                                                                   EL540
01090  1000-OUTPUT-EXIT.                                                EL540
01091      EXIT.                                                        EL540
01092                                                                   EL540
01093  7000-PRINT-LINE SECTION.                                         EL540
01094      MOVE P-CTL                  TO X.                            EL540
01095                                  COPY ELCPRT2X.                   EL540
01096      EJECT                                                        EL540
01097  ABEND-PGM SECTION.                                               EL540
01098                                  COPY ELCABEND.                   EL540
01099      EJECT                                                        EL540
01100  EOJ SECTION.                                                     EL540
01101      MOVE 1                      TO EOJ-SW.                       EL540
01102      PERFORM MINOREST THRU MAJOR.                                 EL540
01103      PERFORM PGCHG.                                               EL540
01104      MOVE 'FINAL TOTALS'         TO DT-2.                         EL540
01105      MOVE '     THIS MONTH   '   TO D2-VAR.                       EL540
01106      MOVE CTR (4, 1)             TO D2-CTR (1).                   EL540
01107      MOVE CTR (4, 2)             TO D2-CTR (2).                   EL540
01108      MOVE CTR (4, 3)             TO D2-CTR (3).                   EL540
01109      MOVE CTR (4, 1)             TO WK-CTR1.                      EL540
01110      ADD CTR (4, 4)              TO WK-CTR1.                      EL540
01111      ADD CTR (4, 7)              TO WK-CTR1.                      EL540
01112      ADD CTR (4, 10)             TO WK-CTR1.                      EL540
01113                                                                   EL540
01114      COMPUTE D2-PCT (1) ROUNDED =                                 EL540
01115          CTR (4, 1) * 100 / WK-CTR1 ON SIZE ERROR                 EL540
01116          MOVE ZERO               TO D2-PCT (1).                   EL540
01117                                                                   EL540
01118      MOVE CTR (4, 2)             TO WK-CTR2.                      EL540
01119      ADD CTR (4, 5)              TO WK-CTR2.                      EL540
01120      ADD CTR (4, 8)              TO WK-CTR2.                      EL540
01121      ADD CTR (4, 11)             TO WK-CTR2.                      EL540
01122                                                                   EL540
01123      COMPUTE D2-PCT (2) ROUNDED =                                 EL540
01124          CTR (4, 2) * 100 / WK-CTR2 ON SIZE ERROR                 EL540
01125          MOVE ZERO               TO D2-PCT (2).                   EL540
01126                                                                   EL540
01127      MOVE DT-2                   TO P-DATA.                       EL540
01128      MOVE ZERO                   TO P-CTL.                        EL540
01129      PERFORM 7000-PRINT-LINE.                                     EL540
01130      MOVE SPACE                  TO DT-2.                         EL540
01131      MOVE '     LAST MONTH   '   TO D2-VAR.                       EL540
01132      MOVE CTR (4, 4)             TO D2-CTR (1).                   EL540
01133      MOVE CTR (4, 5)             TO D2-CTR (2).                   EL540
01134      MOVE CTR (4, 6)             TO D2-CTR (3).                   EL540
01135                                                                   EL540
01136      COMPUTE D2-PCT (1) ROUNDED =                                 EL540
01137          CTR (4, 4) * 100 / WK-CTR1 ON SIZE ERROR                 EL540
01138          MOVE ZERO               TO D2-PCT (1).                   EL540
01139      COMPUTE D2-PCT (2) ROUNDED =                                 EL540
01140          CTR (4, 5) * 100 / WK-CTR2 ON SIZE ERROR                 EL540
01141          MOVE ZERO               TO D2-PCT (2).                   EL540
01142                                                                   EL540
01143      MOVE DT-2                   TO P-DATA.                       EL540
01144      MOVE SPACE                  TO P-CTL.                           CL**2
01145      PERFORM 7000-PRINT-LINE.                                     EL540
01146      MOVE '     MONTH BEFORE LAST   '   TO D2-VAR.                EL540
01147      MOVE CTR (4, 7)             TO D2-CTR (1).                   EL540
01148      MOVE CTR (4, 8)             TO D2-CTR (2).                   EL540
01149      MOVE CTR (4, 9)             TO D2-CTR (3).                   EL540
01150                                                                   EL540
01151      COMPUTE D2-PCT (1) ROUNDED =                                 EL540
01152          CTR (4, 7) * 100 / WK-CTR1 ON SIZE ERROR                 EL540
01153          MOVE ZERO               TO D2-PCT (1).                   EL540
01154      COMPUTE D2-PCT (2) ROUNDED =                                 EL540
01155          CTR (4, 8) * 100 / WK-CTR2 ON SIZE ERROR                 EL540
01156          MOVE ZERO               TO D2-PCT (2).                   EL540
01157                                                                   EL540
01158      MOVE DT-2                   TO P-DATA.                       EL540
01159      MOVE SPACE                  TO P-CTL.                           CL**2
01160      PERFORM 7000-PRINT-LINE.                                     EL540
01161      MOVE '     OTHER MONTHS   ' TO D2-VAR.                       EL540
01162      MOVE CTR (4, 10)            TO D2-CTR (1).                   EL540
01163      MOVE CTR (4, 11)            TO D2-CTR (2).                   EL540
01164      MOVE CTR (4, 12)            TO D2-CTR (3).                   EL540
01165                                                                   EL540
01166      COMPUTE D2-PCT (1) ROUNDED =                                 EL540
01167          CTR (4, 10) * 100 / WK-CTR1 ON SIZE ERROR                EL540
01168          MOVE ZERO               TO D2-PCT (1).                   EL540
01169      COMPUTE D2-PCT (2) ROUNDED =                                 EL540
01170          CTR (4, 11) * 100 / WK-CTR2 ON SIZE ERROR                EL540
01171          MOVE ZERO               TO D2-PCT (2).                   EL540
01172                                                                   EL540
01173      MOVE DT-2                   TO P-DATA.                       EL540
01174      MOVE SPACE                  TO P-CTL.                           CL**2
01175      PERFORM 7000-PRINT-LINE.                                     EL540
01176      MOVE 'GRAND TOTAL'          TO DT-2.                         EL540
01177      ADD CTR (4, 1) CTR (4, 4) CTR (4, 7) CTR (4, 10)             EL540
01178          GIVING D2-CTR (1).                                       EL540
01179      ADD CTR (4, 2) CTR (4, 5) CTR (4, 8) CTR (4, 11)             EL540
01180          GIVING D2-CTR (2).                                       EL540
01181      ADD CTR (4, 3) CTR (4, 6) CTR (4, 9) CTR (4, 12)             EL540
01182          GIVING D2-CTR (3).                                       EL540
01183      MOVE DT-2                   TO P-DATA.                       EL540
01184      MOVE '0'                    TO P-CTL.                        EL540
01185      PERFORM 7000-PRINT-LINE.                                     EL540
01186                                                                   EL540
01187      IF DTE-PGM-OPT = 2                                           EL540
01188          CLOSE EXTRACT-INTERFACE-FILE                             EL540
01189         ELSE                                                      EL540
01190          CLOSE ERPNDB.                                            EL540
01191                                                                   EL540
01192      CLOSE ERACCT.                                                EL540
01193                                                                   EL540
01194      IF DTE-PRT-OPT = 'P' OR 'T' OR 'B'                           EL540
01195          CLOSE PRINT-FILE.                                        EL540
01196                                                                   EL540
01197      COPY ELCPRTCX.                                               EL540
01198                                                                   EL540
01199      GOBACK.                                                      EL540
