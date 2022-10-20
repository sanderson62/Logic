00001  IDENTIFICATION DIVISION.                                         05/14/98
00002                                                                   EL539S
00003  PROGRAM-ID.                 EL539S .                                LV008
00004 *              PROGRAM CONVERTED BY                               EL539S
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL539S
00006 *              CONVERSION DATE 02/12/96 16:38:04.                 EL539S
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            EL539S
00008 *                            VMOD=2.015.                          EL539S
00009                                                                   EL539S
00010 *AUTHOR.        LOGIC, INC.                                       EL539S
00011 *               DALLAS, TEXAS.                                    EL539S
00012                                                                   EL539S
00013 *DATE-COMPILED.                                                   EL539S
00014                                                                   EL539S
00015 *SECURITY.   *****************************************************EL539S
00016 *            *                                                   *EL539S
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *EL539S
00018 *            *                                                   *EL539S
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL539S
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL539S
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *EL539S
00022 *            *                                                   *EL539S
00023 *            *****************************************************EL539S
00024                                                                   EL539S
00025 *REMARKS.                                                         EL539S
00026 *        GENERATES, SORTS AND PRINTS PENETRATION REPORT.          EL539S
00026 *        THIS IS A SPECIAL REPORT FOR SUNFLOWER BANK              EL539S
00026 *        IR SORTS IN LOAN OFFICER ORDER                           EL539S
122002******************************************************************
122002*                   C H A N G E   L O G
122002*
122002* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
122002*-----------------------------------------------------------------
122002*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
122002* EFFECTIVE    NUMBER
122002*-----------------------------------------------------------------
092905* 092905 CR2005080300007   PEMA  ADD DISCLAIMER TO ACCOUNT SUMMARY
110105* 110105    2005071200004  PEMA  INCREASE SIZE OF LOAN OFFICER
091312* 091312 CR2012091100003   PEMA  ALLOW ENTRY CD P
122002******************************************************************
00027  EJECT                                                            EL539S
00028  ENVIRONMENT DIVISION.                                            EL539S
00029  CONFIGURATION SECTION.                                           EL539S
00030  INPUT-OUTPUT SECTION.                                            EL539S
00031  FILE-CONTROL.                                                    EL539S
00032                                                                   EL539S
00033      SELECT SORT-WORK    ASSIGN SYS001-UT-FBA1-S-SORTWK1.         EL539S
00034                                                                   EL539S
00035      SELECT PRINT-FILE   ASSIGN SYS008-UR-1403-S-SYS008.          EL539S
00036                                                                   EL539S
00037      SELECT EXTRACT-INTERFACE-FILE                                EL539S
00038                          ASSIGN SYS010-UT-2400-S-SYS010.          EL539S
00039                                                                   EL539S
00040      SELECT DISK-DATE    ASSIGN SYS019-UT-FBA1-SYS019.            EL539S
00041                                                                   EL539S
00042      SELECT FICH         ASSIGN SYS020-UT-2400-S-SYS020.          EL539S
00043                                                                   EL539S
00044      SELECT ELREPT       ASSIGN SYS021-FBA1-ELREPT                EL539S
00045                          ORGANIZATION IS INDEXED                  EL539S
00046                          ACCESS IS DYNAMIC                        EL539S
00047                          RECORD KEY IS RF-CONTROL-PRIMARY         EL539S
00048                          FILE STATUS IS DTE-VSAM-FLAGS.           EL539S
00049                                                                   EL539S
00050      SELECT ERACCT       ASSIGN SYS022-FBA1-ERACCT                EL539S
00051                          ORGANIZATION IS INDEXED                  EL539S
00052                          ACCESS IS DYNAMIC                        EL539S
00053                          RECORD KEY IS AM-CONTROL-PRIMARY         EL539S
00054                          FILE STATUS IS AM-FILE-STATUS.           EL539S
00055                                                                   EL539S
00056      SELECT ERLOFC       ASSIGN SYS023-FBA1-ERLOFC                EL539S
00057                          ORGANIZATION IS INDEXED                  EL539S
00058                          ACCESS IS DYNAMIC                        EL539S
00059                          RECORD KEY IS LO-CONTROL-PRIMARY         EL539S
00060                          FILE STATUS IS LO-STATUS.                EL539S
00061                                                                   EL539S
00062      SELECT ERPNDB       ASSIGN SYS024-FBA1-ERPNDB2               EL539S
00063                          ORGANIZATION IS INDEXED                  EL539S
00064                          ACCESS IS DYNAMIC                        EL539S
00065                          RECORD KEY IS PB-CONTROL-BY-ACCT         EL539S
00066                          FILE STATUS IS PB-STATUS.                EL539S
00067  EJECT                                                            EL539S
00068  DATA DIVISION.                                                   EL539S
00069  FILE SECTION.                                                    EL539S
00070                                                                   EL539S
00071  SD  SORT-WORK.                                                   EL539S
00072                                                                   EL539S
00073  01  SORT-RECORD.                                                 EL539S
00074      12  SORT-CTL.                                                EL539S
110105         16  SC-LNOFF        PIC  X(5).
00075          16  SC-ACCT-INFO.                                        EL539S
00076              20  SC-CO       PIC  X(01).                          EL539S
00077              20  SC-CARR     PIC  X(01).                          EL539S
00078              20  SC-GRP      PIC  X(06).                          EL539S
00079              20  SC-ST       PIC  X(02).                          EL539S
00080              20  SC-ACCT     PIC  X(10).                          EL539S
PEMMOD*        16  SC-LNOFF        PIC  X(03).                          EL539S
00082          16  SC-EFFDT        PIC  X(06).                          EL539S
00083          16  SC-CERT         PIC  X(11).                          EL539S
00084      12  SORT-DTA            PIC  X(91).                          EL539S
00085  EJECT                                                            EL539S
00086  FD  PRINT-FILE                                                   EL539S
00087                          COPY ELCPRTFD.                           EL539S
00088  EJECT                                                            EL539S
00089  FD  EXTRACT-INTERFACE-FILE                                       EL539S
00090                          COPY ERCEXTFD.                           EL539S
00091                                                                   EL539S
00092  01  EXTRACT-INTERFACE-FILE-RECORD   PIC  X(629).                 EL539S
00093  EJECT                                                            EL539S
00094  FD  DISK-DATE                                                    EL539S
00095                          COPY ELCDTEFD.                           EL539S
00096  EJECT                                                            EL539S
00097  FD  FICH                                                         EL539S
00098                          COPY ELCFCHFD.                           EL539S
00099  EJECT                                                            EL539S
00100  FD  ELREPT                                                       EL539S
00101                          COPY ELCRPTFD.                           EL539S
00102                                                                   EL539S
00103      COPY ELCREPT.                                                EL539S
00104  EJECT                                                            EL539S
00105  FD  ERACCT.                                                      EL539S
00106                                                                   EL539S
00107      COPY ERCACCT.                                                EL539S
00108  EJECT                                                            EL539S
00109  FD  ERLOFC.                                                      EL539S
00110                                                                   EL539S
00111      COPY ERCLOFC.                                                EL539S
00112  EJECT                                                            EL539S
00113  FD  ERPNDB.                                                      EL539S
00114                                                                   EL539S
00115  01  PB-RECORD.                                                   EL539S
00116      12  FILLER              PIC  X(13).                          EL539S
00117      12  PB-CONTROL-BY-ACCT.                                      EL539S
00118          16  PB-COMP-CD      PIC  X(01).                          EL539S
00119          16  FILLER          PIC  X(35).                          EL539S
00120      12  FILLER              PIC  X(536).                         EL539S
00121  EJECT                                                            EL539S
00122  WORKING-STORAGE SECTION.                                         EL539S
00123  77  LCP-ASA                       PIC X.                         EL539S
00124  77  FILLER  PIC  X(32) VALUE '********************************'. EL539S
00125  77  FILLER  PIC  X(32) VALUE '*   EL539S WORKING-STORAGE     *'.  EL539S
00126  77  FILLER  PIC  X(32) VALUE '***********VMOD=2.015 **********'. EL539S
00127                                                                   EL539S
00128  77  PGM-SUB                 PIC S9(03)  COMP    VALUE +539.      EL539S
PEMMOD 77  WK1                     PIC S999    COMP-3  VALUE +0.
PEMMOD 77  WK2                     PIC S999    COMP-3  VALUE +0.
00129  77  WS-RETURN-CODE          PIC S9(04)  COMP    VALUE +0.        EL539S
00130  77  TBLX                    PIC S9(04)  COMP.                    EL539S
00131  77  AX                      PIC S9(04)  COMP    VALUE +0.        EL539S
00132  77  WS-ZERO                 PIC S9(01)  COMP-3  VALUE +0.        EL539S
00133  77  LNCT                    PIC S9(02)  COMP-3  VALUE +80.       EL539S
00134  77  PGCT                    PIC S9(05)  COMP-3  VALUE +0.        EL539S
00135  77  X                       PIC  X(01).                          EL539S
00136  77  LO-OPT1                 PIC  X(01).                          EL539S
00137  77  LO-OPT2                 PIC  X(01).                          EL539S
00138  77  SKIP-THIS-ACCT          PIC  X(01)          VALUE 'N'.       EL539S
00139  77  SKIP-THIS-LOAN-OFCR     PIC  X(01)          VALUE 'N'.       EL539S
00140  77  HAVE-DATA               PIC  X(01)          VALUE 'N'.       EL539S
00141  77  LO-UPD                  PIC  X(01).                          EL539S
00142  77  HAVE-HIGH-VALUE         PIC  X(01)          VALUE 'N'.       EL539S
00143  77  PRNTG-SUMM              PIC  X(01)          VALUE 'N'.       EL539S
00144  77  PRNTG-DTL               PIC  X(01)          VALUE 'N'.       EL539S
00145  77  ANY-COMP                PIC  X(01).                          EL539S
00146  77  WS-ABEND-FILE-STATUS    PIC  X(02)          VALUE ZERO.      EL539S
00147  77  OLC-REPORT-NAME         PIC  X(06)          VALUE 'EL539S'.  EL539S
00148  77  SAVE-ACCOUNT            PIC  X(10).                          EL539S
00149  77  LST-ACCT                PIC  X(20)          VALUE LOW-VALUE. EL539S
00150  77  KEY-20                  PIC  X(20).                          EL539S
110105 77  KEY-23                  PIC  X(25).                          EL539S
110105 77  HLD-CTL                 PIC  X(25)          VALUE LOW-VALUE. EL539S
00154  77  WS-ABEND-MESSAGE        PIC  X(80)          VALUE SPACES.    EL539S
PEMMOD 77  WS-WORK-AMT             PIC S9(9)V99 COMP-3 VALUE +0.
PEMMOD 77  WS-FIX-CNT              PIC 9(05) VALUE ZEROS.
00155  EJECT                                                            EL539S
00156  01  MISC-WS.                                                     EL539S
PEMMOD     12  LST-OFFCR.                                               EL539S
110105         16  LST-LO-CODE     PIC  X(5)   VALUE LOW-VALUES.
PEMMOD         16  LST-REST        PIC  X(20)  VALUE LOW-VALUES.
PEMMOD     12  WS-BANK             PIC  X(10) VALUE SPACES.
00157      12  PB-STATUS.                                               EL539S
00158          16  PB-STAT-1       PIC  X(01).                          EL539S
00159          16  PB-STAT-2       PIC  X(01).                          EL539S
00160      12  AM-FILE-STATUS.                                          EL539S
00161          16  AM-STAT-1       PIC  X(01).                          EL539S
00162          16  AM-STAT-2       PIC  X(01).                          EL539S
00163      12  LO-STATUS.                                               EL539S
00164          16  LO-STAT-1       PIC  X(01).                          EL539S
00165          16  LO-STAT-2       PIC  X(01).                          EL539S
00166      12  CTLKEY.                                                  EL539S
110105         16  CTLOFCR         PIC  X(05).                          EL539S
00167          16  CPGCTL.                                              EL539S
00168              20  CTLCO       PIC  X(01).                          EL539S
00169              20  CTLCARR     PIC  X(01).                          EL539S
00170              20  CTLGRP      PIC  X(06).                          EL539S
00171              20  CTLST       PIC  X(02).                          EL539S
00172              20  CTLACCT     PIC  X(10).                          EL539S
PEMMOD*        16  FILLER.                                              EL539S
PEMMOD*            20  CTLOFCR     PIC  X(03).                          EL539S
00175      12  PRVKEY.                                                  EL539S
110105         16  PLO             PIC  X(05)      VALUE LOW-VALUES.    EL539S
00176          16  PPGCTL          PIC  X(20)      VALUE LOW-VALUES.    EL539S
PEMMOD*        16  PLO             PIC  X(03)      VALUE LOW-VALUES.    EL539S
00178      12  BUILD-LO-KEY.                                            EL539S
00179          16  BLK-COMPANY     PIC  X(01).                          EL539S
00180          16  BLK-CARRIER     PIC  X(01).                          EL539S
00181          16  BLK-GROUPING    PIC  X(06).                          EL539S
00182          16  BLK-STATE       PIC  X(02).                          EL539S
00183          16  BLK-ACCT        PIC  X(10).                          EL539S
00184      12  TOT-COMM            PIC S9(11)V99  COMP-3.               EL539S
00185      12  HOLD-PROCESS-MO-YR.                                      EL539S
00186          16  HOLD-PROCESS-CC PIC  XX.                             EL539S
00187          16  HOLD-PROCESS-YR PIC  XX.                             EL539S
00188          16  HOLD-PROCESS-MO PIC  XX.                             EL539S
00189      12  WS-PEND-YRMODA      PIC 9(11).                              CL**5
00190      12  WS-PEND-YRMODA-R REDEFINES WS-PEND-YRMODA.                  CL**5
00191          16  FILLER          PIC  9(03).                             CL**2
00192          16  PEND-CC         PIC  9(02).                             CL**2
00193          16  PEND-YR         PIC  9(02).                             CL**2
00194          16  PEND-MO         PIC  9(02).                             CL**2
00195          16  PEND-DA         PIC  9(02).                             CL**2
00196  EJECT                                                            EL539S
00197  01  CONAMREC.                                                    EL539S
00198      12  CONA-LOVAL          PIC  X(20).                          EL539S
00199      12  CONA-NAME           PIC  X(30).                          EL539S
00200      12  CONA-ANDT           PIC  X(18).                          EL539S
00201                                                                   EL539S
00202  01  ACCT-REC.                                                    EL539S
110105     12  ACCT-LO-OFF-CD      PIC  X(5).
00203      12  ACCT-INFO.                                               EL539S
00204          16  ACCT-CO         PIC  X(01).                          EL539S
00205          16  ACCT-CARR       PIC  X(01).                          EL539S
00206          16  ACCT-GRP        PIC  X(06).                          EL539S
00207          16  ACCT-ST         PIC  X(02).                          EL539S
00208          16  ACCT-NUM        PIC  X(10).                          EL539S
110105     12  ACCT-LOVAL          PIC  X(05).                          EL539S
00210      12  ACCT-NAME           PIC  X(30).                          EL539S
00211      12  ACCT-PERSON         PIC  X(20).                          EL539S
00212      12  ACCT-ADDR1          PIC  X(20).                          EL539S
00213      12  ACCT-CTY-ST         PIC  X(20).                          EL539S
00214      12  ACCT-ZIP.                                                EL539S
00215          16  ACCT-ZIP-PRIME  PIC  X(05).                          EL539S
00216          16  ACCT-ZIP-PLUS4  PIC  X(04).                          EL539S
00217      12  ACCT-CANADIAN-POSTAL-CODE REDEFINES ACCT-ZIP.            EL539S
00218          16  ACCT-CAN-POSTAL-CODE-1.                              EL539S
00219              20  FILLER      PIC  X(01).                          EL539S
00220                  88 ACCT-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'.   EL539S
00221              20  FILLER      PIC  X(02).                          EL539S
00222          16  ACCT-CAN-POSTAL-CODE-2                               EL539S
00223                              PIC  X(03).                          EL539S
00224          16  ACCT-CAN-FILLER PIC  X(03).                          EL539S
00225                                                                   EL539S
00226  01  LN-REC.                                                      EL539S
110105     12  LN-OFFCODE          PIC  X(05).                          EL539S
00227      12  LN-CO               PIC  X(01).                          EL539S
00228      12  LN-CARR             PIC  X(01).                          EL539S
00229      12  LN-GRP              PIC  X(06).                          EL539S
00230      12  LN-ST               PIC  X(02).                          EL539S
00231      12  LN-ACCT             PIC  X(10).                          EL539S
PEMMOD*    12  LN-OFFCODE          PIC  X(03).                          EL539S
00233      12  LN-LOVAL            PIC  X(06).                          EL539S
00234      12  LN-OFFCR            PIC  X(30).                          EL539S
00235      12  LN-OPT1             PIC  X(01).                          EL539S
00236      12  LN-OPT2             PIC  X(01).                          EL539S
00237                                                                   EL539S
00238  01  PEND-REC.                                                    EL539S
110105     12  PEND-OFFICER        PIC  X(05).                          EL539S
PEMMOD     12  PEND-ACCT-CTL.
00239          16  PEND-CO         PIC  X(01).                          EL539S
00240          16  PEND-CARR       PIC  X(01).                          EL539S
00241          16  PEND-GRP        PIC  X(06).                          EL539S
00242          16  PEND-ST         PIC  X(02).                          EL539S
00243          16  PEND-ACCT       PIC  X(10).                          EL539S
PEMMOD*    12  PEND-OFFICER        PIC  X(03).                          EL539S
00245      12  PEND-YRMODA         PIC  9(11)  COMP-3.                  EL539S
00246      12  PEND-CERT-PRIME     PIC  X(10).                          EL539S
00247      12  PEND-CERT-SFX       PIC  X(01).                          EL539S
00248      12  PEND-NAME           PIC  X(15).                          EL539S
00249      12  PEND-INIT           PIC  X(02).                          EL539S
00250      12  PEND-AGE            PIC  9(02).                          EL539S
00251      12  PEND-LF-TYP         PIC  X(03).                          EL539S
00252      12  PEND-LF-PRM         PIC S9(07)V99   COMP-3.              EL539S
00253      12  PEND-LF-COM         PIC S9(07)V99   COMP-3.              EL539S
00254      12  PEND-LF-AMT         PIC S9(09)V99   COMP-3.              EL539S
00255      12  PEND-AH-TYP         PIC  X(03).                          EL539S
00256      12  PEND-AH-PRM         PIC S9(07)V99   COMP-3.              EL539S
00257      12  PEND-AH-AMT         PIC S9(09)V99   COMP-3.              EL539S
00258      12  PEND-AH-COM         PIC S9(07)V99   COMP-3.              EL539S
00259      12  PEND-LF-TERM        PIC S9(03)      COMP-3.              EL539S
00260      12  PEND-AH-TERM        PIC S9(03)      COMP-3.              EL539S
PEMMOD     12  PEND-OFFICER-NAME   PIC X(30).
00261  EJECT                                                            EL539S
00262  01  HD-1.                                                        EL539S
00263      12  FILLER              PIC  X(46)          VALUE SPACES.    EL539S
00264      12  FILLER              PIC  X(31)          VALUE            EL539S
00265              'LOAN PRODUCTION AND PENETRATION'.                   EL539S
00266      12  FILLER              PIC  X(42)          VALUE SPACES.    EL539S
00267      12  FILLER              PIC  X(06)          VALUE 'EL539S'.  EL539S
00268                                                                   EL539S
00269  01  HD-2.                                                        EL539S
00270      12  FILLER              PIC  X(48)          VALUE SPACES.    EL539S
00271      12  HD2-CO              PIC  X(30).                          EL539S
00272      12  FILLER              PIC  X(41)          VALUE SPACES.    EL539S
00273      12  HD2-RUN             PIC  X(08).                          EL539S
00274                                                                   EL539S
00275  01  HD-3.                                                        EL539S
00276      12  FILLER              PIC  X(54)          VALUE SPACES.    EL539S
00277      12  HD3-DATE            PIC  X(18).                          EL539S
00278      12  FILLER              PIC  X(47)          VALUE SPACES.    EL539S
00279      12  FILLER              PIC  X(05)          VALUE 'PAGE '.   EL539S
00280      12  HD3-PG              PIC ZZZ,ZZ9.                         EL539S
00281                                                                   EL539S
00282  01  HD-4.                                                        EL539S
00283      12  FILLER              PIC  X(24)          VALUE            EL539S
00284              'CARR  GROUP ST  ACCOUNT'.                           EL539S
00285                                                                   EL539S
00286  01  HD-5.                                                        EL539S
00287      12  FILLER              PIC  X(02)          VALUE SPACE.     EL539S
00288      12  H5-CARR             PIC  X(01).                          EL539S
00289      12  FILLER              PIC  X(02)          VALUE SPACE.     EL539S
00290      12  H5-GRP              PIC  X(06).                          EL539S
00291      12  FILLER              PIC  X(01)          VALUE SPACE.     EL539S
00292      12  H5-ST               PIC  X(02).                          EL539S
00293      12  FILLER              PIC  X(01)          VALUE SPACE.     EL539S
00294      12  H5-ACCT             PIC  X(10).                          EL539S
00295                                                                   EL539S
00296  01  HD-6.                                                        EL539S
00297      12  H6-NA               PIC  X(30).                          EL539S
00298      12  FILLER              PIC  X(27)          VALUE SPACES.    EL539S
00299      12  H6-DS               PIC  X(19).                          EL539S
00300                                                                   EL539S
00301  01  HD-7.                                                        EL539S
00302      12  H7-NA               PIC  X(30).                          EL539S
00303      12  FILLER              PIC  X(27)          VALUE SPACES.    EL539S
00304      12  FILLER              PIC  X(19)          VALUE            EL539S
00305              '-------------------'.                               EL539S
00306                                                                   EL539S
00307  01  HD-8.                                                        EL539S
00308      12  H8-NA               PIC  X(30).                          EL539S
00309                                                                   EL539S
00310  01  HD-9.                                                        EL539S
00311      12  H9-NA               PIC  X(30).                          EL539S
00312      12  H9-ZIP.                                                  EL539S
00313          16  H9-ZIP-PRIME    PIC  X(05).                          EL539S
00314          16  H9-DASH         PIC  X(01).                          EL539S
00315          16  H9-ZIP-PLUS4    PIC  X(04).                          EL539S
00316      12  H9-CANADIAN-POSTAL-CODE REDEFINES H9-ZIP.                EL539S
00317          16  H9-CAN-POSTAL-CODE-1                                 EL539S
00318                              PIC  X(03).                          EL539S
00319          16  H9-DASH-CAN     PIC  X(01).                          EL539S
00320          16  H9-CAN-POSTAL-CODE-2                                 EL539S
00321                              PIC  X(03).                          EL539S
00322          16  H9-CAN-FILLER   PIC  X(03).                          EL539S
00323  EJECT                                                            EL539S
00324  01  HD-10.                                                       EL539S
110105     12  H10-LOF             PIC  X(05).                          EL539S
00326      12  H10-DSH             PIC  X(01).                          EL539S
00327      12  H10-LONAME          PIC  X(20).                          EL539S
00328                                                                   EL539S
00329  01  HD-11.                                                       EL539S
00330      12  FILLER              PIC  X(44)          VALUE            EL539S
00331              'CERTIFICATE  EFFECTIVE     INSURED          '.      EL539S
00332      12  FILLER              PIC  X(15)          VALUE            EL539S
00333              '-------------  '.                                   EL539S
00334      12  H11-LF              PIC  X(06).                          EL539S
00335      12  FILLER              PIC  X(33)          VALUE            EL539S
00336              '  --------------  -------------  '.                 EL539S
00337      12  H11-AH              PIC  X(06).                          EL539S
00338      12  FILLER              PIC  X(17)          VALUE            EL539S
00339              '  -------------- '.                                 EL539S
00340                                                                   EL539S
00341  01  HD-12.                                                       EL539S
00342      12  FILLER              PIC  X(44)          VALUE            EL539S
00343              'NUMBER          DATE        NAME       AGE T'.      EL539S
00344      12  FILLER              PIC  X(44)          VALUE            EL539S
PEMMOD             'ERM TYPE   PREM/REFUND       LIFE BEN  TERM '.      EL539S
00346      12  FILLER              PIC  X(34)          VALUE            EL539S
PEMMOD             'TYPE  PREM/REFUND        A&H BEN  '.                EL539S
00348      12  HD9-VAR             PIC  X(10).                          EL539S
00349  EJECT                                                            EL539S
00350  01  DT-1.                                                        EL539S
00351      12  D1-CERT-PRIME       PIC  X(10).                          EL539S
00352      12  FILLER              PIC  X(01).                          EL539S
00353      12  D1-CERT-SFX         PIC  X(01).                          EL539S
00354      12  FILLER              PIC  X(01).                          EL539S
00355      12  D1-EFF.                                                  EL539S
00356          16  D1-EFMO         PIC  9(02).                          EL539S
00357          16  D1-SL1          PIC  X(01).                          EL539S
00358          16  D1-EFDA         PIC  9(02).                          EL539S
00359          16  D1-SL2          PIC  X(01).                          EL539S
00360          16  D1-EFYR         PIC  9(02).                          EL539S
00361      12  FILLER              PIC  X(01).                          EL539S
00362      12  D1-NAME             PIC  X(14).                          EL539S
00363      12  FILLER              PIC  X(01).                          EL539S
00364      12  D1-INIT             PIC  X(01).                          EL539S
00365      12  FILLER              PIC  X(01).                          EL539S
00366      12  D1-AGE              PIC  9(02).                          EL539S
00367      12  FILLER              PIC  X(02).                          EL539S
00368      12  D1-LF-TERM          PIC ZZ9-            BLANK WHEN ZERO. EL539S
00369      12  FILLER              PIC  X(01).                          EL539S
00370      12  D1-LTYP             PIC  X(03).                          EL539S
00371      12  FILLER              PIC  X(01).                          EL539S
00372      12  D1-LPRM             PIC ZZ,ZZZ,ZZZ.99-  BLANK WHEN ZERO. EL539S
00373      12  FILLER              PIC  X(01).                          EL539S
00374      12  D1-LBEN             PIC ZZZ,ZZZ,ZZZ.99- BLANK WHEN ZERO. EL539S
00375      12  FILLER              PIC  X(01).                          EL539S
00376      12  D1-AH-TERM          PIC ZZ9-            BLANK WHEN ZERO. EL539S
00377      12  FILLER              PIC  X(01).                          EL539S
00378      12  D1-HTYP             PIC  X(03).                          EL539S
00379      12  FILLER              PIC  X(01).                          EL539S
00380      12  D1-HPRM             PIC Z,ZZZ,ZZZ.99-   BLANK WHEN ZERO. EL539S
00381      12  FILLER              PIC  X(01).                          EL539S
00382      12  D1-HBEN             PIC ZZZ,ZZZ,ZZZ.99- BLANK WHEN ZERO. EL539S
00383      12  D1-COMM             PIC ZZZZ,ZZZ.99-    BLANK WHEN ZERO. EL539S
00384                                                                   EL539S
00385  01  SUM-HD1.                                                     EL539S
00386      12  FILLER              PIC  X(44)          VALUE            EL539S
00387              '                        ----- LOANS ----- --'.      EL539S
00388      12  FILLER              PIC  X(18)          VALUE            EL539S
00389              '----------------- '.                                EL539S
00390      12  SUM-HD1-LF          PIC  X(06).                          EL539S
00391      12  FILLER              PIC  X(37)          VALUE            EL539S
00392              ' ----------------- ------------------'.             EL539S
00393      12  SUM-HD1-AH          PIC  X(06).                          EL539S
00394      12  FILLER              PIC  X(21)          VALUE            EL539S
00395              '  ------------------ '.                             EL539S
00396                                                                   EL539S
00397  01  SUM-HD2.                                                     EL539S
00398      12  FILLER              PIC X(44)           VALUE            EL539S
00399              'CODE  NAME              COUNT     AMOUNT  CO'.      EL539S
00400      12  FILLER              PIC X(44)           VALUE            EL539S
00401              'UNT   PCT       BENEFIT   PCT     PREM/REF C'.      EL539S
00402      12  FILLER              PIC X(44)           VALUE            EL539S
00403              'OUNT   PCT       BENEFIT   PCT     PREM/REF '.      EL539S

092905 01  DISC-LINE-1.
           12  FILLER                  PIC X(6)       VALUE SPACES.
           12  FILLER                  PIC X(126)     VALUE
             'Just a reminder, if you are using the reports to pay compe
      -      'nsation to those people listed, they must be licensed '.

092905 01  DISC-LINE-2.
           12  FILLER                  PIC XX         VALUE SPACES.
           12  FILLER                  PIC X(130)     VALUE
             'and appointed with CSO.  To determine who is licensed and 
      -      'appointed with CSO, please contact our Licensing'.

092905 01  DISC-LINE-3.
           12  FILLER                  PIC XX         VALUE SPACES.
           12  FILLER                  PIC X(130)     VALUE
             'Department at 1-800-826-6587.'.

00405  01  SUM-DT.                                                      EL539S
110105     12  SD-CODE             PIC  X(05).                          EL539S
00407      12  SD-DASH             PIC  X(01).                          EL539S
00408      12  SD-NAME             PIC  X(20).                          EL539S
00409      12  SD-LNCT             PIC ZZZZZ-.                          EL539S
00410      12  SD-LNAMT            PIC ZZZ,ZZZ,ZZZ-.                    EL539S
00411      12  SD-LICT             PIC ZZZZZ-.                          EL539S
00412      12  SD-LCCT             PIC ZZZ.Z-.                          EL539S
00413      12  SD-LBEN             PIC ZZ,ZZZ,ZZZ.ZZ-.                  EL539S
00414      12  SD-BPCT             PIC ZZZ.Z-.                          EL539S
00415      12  SD-LPRM             PIC Z,ZZZ,ZZZ.ZZ-.                   EL539S
00416      12  SD-DICT             PIC ZZZZZ-.                          EL539S
00417      12  SD-DCPC             PIC ZZZ.Z-.                          EL539S
00418      12  SD-DBEN             PIC ZZ,ZZZ,ZZZ.ZZ-.                  EL539S
00419      12  SD-DBPC             PIC ZZZ.Z-.                          EL539S
00420      12  SD-DPRM             PIC Z,ZZZ,ZZZ.ZZ-.                   EL539S
00421                                                                   EL539S
00422  01  FILLER                  PIC  X(16)          VALUE            EL539S
00423          'TABLE BEGIN HERE'.                                      EL539S
00424  01  TBL.                                                         EL539S
00425      12  TBL-CTL.                                                 EL539S
00426          16  TBL-CO          PIC  X(01).                          EL539S
00427          16  TBL-CARR        PIC  X(01).                          EL539S
00428          16  TBL-GRP         PIC  X(06).                          EL539S
00429          16  TBL-ST          PIC  X(02).                          EL539S
00430          16  TBL-ACCT        PIC  X(10).                          EL539S
00431      12  TBL-NAME            PIC  X(20).                          EL539S
00432      12  TBL-MAIL1           PIC  X(20).                          EL539S
00433      12  TBL-MAIL2           PIC  X(20).                          EL539S
00434      12  TBL-MAIL3           PIC  X(20).                          EL539S
00435      12  TBL-ZIP.                                                 EL539S
00436          16  TBL-ZIP-PRIME   PIC  9(05).                          EL539S
00437          16  TBL-ZIP-PLUS4   PIC  9(04).                          EL539S
00438      12  TBL-CANADIAN-POSTAL-CODE REDEFINES TBL-ZIP.              EL539S
00439          16  TBL-CAN-POSTAL-CODE-1                                EL539S
00440                              PIC  X(03).                          EL539S
00441          16  TBL-CAN-POSTAL-CODE-2                                EL539S
00442                              PIC  X(03).                          EL539S
00443          16  TBL-CAN-FILLER PIC   X(03).                          EL539S
00444      12  TBL-ENT         OCCURS  201  TIMES.                      EL539S
110105         16  TBL-CODE        PIC  X(05).                          EL539S
PEMMOD         16  TBL-LO-NAME     PIC  X(30).
00446          16  TBL-LICT        PIC S9(05)      COMP-3.              EL539S
00447          16  TBL-LBEN        PIC S9(09)V99   COMP-3.              EL539S
00448          16  TBL-LPRM        PIC S9(07)V99   COMP-3.              EL539S
00449          16  TBL-LCCT        PIC S9(05)      COMP-3.              EL539S
00450          16  TBL-LREF        PIC S9(07)V99   COMP-3.              EL539S
00451          16  TBL-LCOM        PIC S9(07)V99   COMP-3.              EL539S
00452          16  TBL-LOCT        PIC S9(5)       COMP-3.              EL539S
00453          16  TBL-LVOL        PIC S9(9)V99    COMP-3.              EL539S
00454          16  TBL-DICT        PIC S9(05)      COMP-3.              EL539S
00455          16  TBL-DBEN        PIC S9(09)V99   COMP-3.              EL539S
00456          16  TBL-DPRM        PIC S9(07)V99   COMP-3.              EL539S
00457          16  TBL-DCCT        PIC S9(05)      COMP-3.              EL539S
00458          16  TBL-DREF        PIC S9(07)V99   COMP-3.              EL539S
00459          16  TBL-DCOM        PIC S9(07)V99   COMP-3.              EL539S
PEMMOD     12  LO-ENT.
PEMMOD         16  LO-LICT         PIC S9(07)     VALUE +0 COMP-3.
PEMMOD         16  LO-LBEN         PIC S9(11)V99  VALUE +0 COMP-3.
PEMMOD         16  LO-LPRM         PIC S9(09)V99  VALUE +0 COMP-3.
PEMMOD         16  LO-LCCT         PIC S9(07)     VALUE +0 COMP-3.
PEMMOD         16  LO-LREF         PIC S9(09)V99  VALUE +0 COMP-3.
PEMMOD         16  LO-LCOM         PIC S9(09)V99  VALUE +0 COMP-3.
PEMMOD         16  LO-LOCT         PIC S9(7)      VALUE +0 COMP-3.
PEMMOD         16  LO-LVOL         PIC S9(11)V99  VALUE +0 COMP-3.
PEMMOD         16  LO-DICT         PIC S9(07)     VALUE +0 COMP-3.
PEMMOD         16  LO-DBEN         PIC S9(11)V99  VALUE +0 COMP-3.
PEMMOD         16  LO-DPRM         PIC S9(09)V99  VALUE +0 COMP-3.
PEMMOD         16  LO-DCCT         PIC S9(07)     VALUE +0 COMP-3.
PEMMOD         16  LO-DREF         PIC S9(09)V99  VALUE +0 COMP-3.
PEMMOD         16  LO-DCOM         PIC S9(09)V99  VALUE +0 COMP-3.
PEMMOD     12  FIN-ENT.
PEMMOD         16  FIN-LICT        PIC S9(07)     VALUE +0 COMP-3.
PEMMOD         16  FIN-LBEN        PIC S9(11)V99  VALUE +0 COMP-3.
PEMMOD         16  FIN-LPRM        PIC S9(09)V99  VALUE +0 COMP-3.
PEMMOD         16  FIN-LCCT        PIC S9(07)     VALUE +0 COMP-3.
PEMMOD         16  FIN-LREF        PIC S9(09)V99  VALUE +0 COMP-3.
PEMMOD         16  FIN-LCOM        PIC S9(09)V99  VALUE +0 COMP-3.
PEMMOD         16  FIN-LOCT        PIC S9(7)      VALUE +0 COMP-3.
PEMMOD         16  FIN-LVOL        PIC S9(11)V99  VALUE +0 COMP-3.
PEMMOD         16  FIN-DICT        PIC S9(07)     VALUE +0 COMP-3.
PEMMOD         16  FIN-DBEN        PIC S9(11)V99  VALUE +0 COMP-3.
PEMMOD         16  FIN-DPRM        PIC S9(09)V99  VALUE +0 COMP-3.
PEMMOD         16  FIN-DCCT        PIC S9(07)     VALUE +0 COMP-3.
PEMMOD         16  FIN-DREF        PIC S9(09)V99  VALUE +0 COMP-3.
PEMMOD         16  FIN-DCOM        PIC S9(09)V99  VALUE +0 COMP-3.
00460  EJECT                                                            EL539S
00461      COPY ERCEXTR.                                                EL539S
00462  EJECT                                                            EL539S
00463      COPY ERCPNDB.                                                EL539S
00464  EJECT                                                            EL539S
00465      COPY ELCDATE.                                                   CL**6
00466  EJECT                                                            EL539S
00467      COPY ELCDTECX.                                               EL539S
00468  EJECT                                                               CL**2
00469      COPY ELCDTEVR.                                                  CL**2
00470  EJECT                                                            EL539S
00471  PROCEDURE DIVISION.                                              EL539S
00472                                                                   EL539S
00473  0000-GET-DATE.                                                   EL539S
PEMMOD
PEMMOD     ACCEPT      WS-BANK
00474                              COPY ELCDTERX.                       EL539S
00475  EJECT                                                            EL539S
00476  0100-PUT-CO.                                                     EL539S
00477      MOVE LIFE-OVERRIDE-L6       TO  H11-LF                       EL539S
00478                                      SUM-HD1-LF.                  EL539S
00479      MOVE AH-OVERRIDE-L6         TO  H11-AH                       EL539S
00480                                      SUM-HD1-AH.                  EL539S
00481      MOVE LOW-VALUES             TO  CONA-LOVAL.                  EL539S
00482      MOVE COMPANY-NAME           TO  CONA-NAME.                   EL539S
00483      MOVE ALPH-DATE              TO  CONA-ANDT.                   EL539S
00484                                                                   EL539S
00485      IF DTE-PGM-OPT  IS NOT EQUAL TO  2                           EL539S
00486          OPEN INPUT ERPNDB                                        EL539S
00487      ELSE                                                         EL539S
00488          OPEN INPUT EXTRACT-INTERFACE-FILE.                       EL539S
00489                                                                   EL539S
00490      OPEN INPUT ERACCT                                            EL539S
00491           I-O   ERLOFC.                                           EL539S
00492                                                                   EL539S
00493      IF DTE-PGM-OPT  IS NOT EQUAL TO  2                           EL539S
00494          IF PB-STATUS  IS EQUAL TO  '00'  OR  '97'                EL539S
00495              NEXT SENTENCE                                        EL539S
00496          ELSE                                                     EL539S
00497              MOVE PB-STATUS      TO  WS-ABEND-FILE-STATUS         EL539S
00498              MOVE 'ERROR ON OPEN - ERPNDB'                        EL539S
00499                                  TO  WS-ABEND-MESSAGE             EL539S
00500              GO TO ABEND-PGM.                                     EL539S
00501                                                                   EL539S
00502      IF AM-FILE-STATUS  IS EQUAL TO  '00'  OR  '97'               EL539S
00503          NEXT SENTENCE                                            EL539S
00504      ELSE                                                         EL539S
00505          MOVE AM-FILE-STATUS     TO  WS-ABEND-FILE-STATUS         EL539S
00506          MOVE 'ERROR ON OPEN - ERACCT'                            EL539S
00507                                  TO  WS-ABEND-MESSAGE             EL539S
00508          GO TO ABEND-PGM.                                         EL539S
00509                                                                   EL539S
00510      IF LO-STATUS  IS EQUAL TO  '00'  OR  '97'                    EL539S
00511          NEXT SENTENCE                                            EL539S
00512      ELSE                                                         EL539S
00513          MOVE LO-STATUS          TO  WS-ABEND-FILE-STATUS         EL539S
00514          MOVE 'ERROR ON OPEN - ERLOFC'                            EL539S
00515                                  TO  WS-ABEND-MESSAGE             EL539S
00516          GO TO ABEND-PGM.                                         EL539S
00517                                                                   EL539S
00518      IF DTE-PRT-OPT  IS EQUAL TO  'P'  OR  'B'  OR  'T'           EL539S
00519          OPEN OUTPUT PRINT-FILE.                                  EL539S
00520                                                                   EL539S
00521  0200-SORT-RECS SECTION.                                          EL539S
00522      SORT SORT-WORK  ON ASCENDING KEY  SORT-CTL                   EL539S
00523          INPUT PROCEDURE 0300-INPUT-SECT  THRU  1099-EXIT         EL539S
00524          OUTPUT PROCEDURE 1100-PUT-REPORTS  THRU  1199-EXIT.      EL539S
00525                                                                   EL539S
00526      IF SORT-RETURN  IS NOT EQUAL TO  ZEROS                       EL539S
00527          MOVE '0101'             TO  WS-RETURN-CODE               EL539S
00528          MOVE 'BAD SORT RETURN CODE '                             EL539S
00529                                  TO  WS-ABEND-MESSAGE             EL539S
00530          GO TO ABEND-PGM.                                         EL539S
00531                                                                   EL539S
PEMMOD     DISPLAY '  FIX COUNTS  ' WS-FIX-CNT

00532      GO TO 9000-EOJ.                                              EL539S
00533  EJECT                                                            EL539S
00534  0300-INPUT-SECT SECTION.                                         EL539S
00535      MOVE CONAMREC               TO  SORT-RECORD.                 EL539S
00536                                                                   EL539S
00537  0310-RELEASE.                                                    EL539S
PEMMOD     MOVE SPACES                 TO  SC-ST
PEMMOD
00538      RELEASE SORT-RECORD.                                         EL539S
00539                                                                   EL539S
00540  0319-EXIT.                                                       EL539S
00541      EXIT.                                                        EL539S
00542                                                                   EL539S
00543  0400-READ-INPUT.                                                 EL539S
00544      IF DTE-PGM-OPT  IS EQUAL TO  2                               EL539S
00545          GO TO 0410-ISSUES-CANCELS.                               EL539S
00546                                                                   EL539S
00547      MOVE LOW-VALUE              TO  PB-CONTROL-BY-ACCOUNT.       EL539S
00548      MOVE DTE-CLASIC-COMPANY-CD  TO  PB-COMP-CD.                  EL539S
00549                                                                   EL539S
00550      MOVE PB-CONTROL-BY-ACCOUNT  TO  PB-CONTROL-BY-ACCT.          EL539S
00551                                                                   EL539S
00552      START ERPNDB  KEY  GREATER  THAN  PB-CONTROL-BY-ACCT.        EL539S
00553                                                                   EL539S
00554      IF PB-STATUS  IS EQUAL TO  '23'                              EL539S
00555          GO TO 0800-END-INPUT.                                    EL539S
00556                                                                   EL539S
00557      IF PB-STAT-1  IS NOT EQUAL TO  '0'                           EL539S
00558          MOVE PB-STATUS          TO  WS-ABEND-FILE-STATUS         EL539S
00559          MOVE 'ERROR OCCURRED START - ERPNDB'                     EL539S
00560                                  TO  WS-ABEND-MESSAGE             EL539S
00561          GO TO ABEND-PGM.                                         EL539S
00562                                                                   EL539S
00563      GO TO 0420-ISSUES-CANCELS.                                   EL539S
00564                                                                   EL539S
00565  0410-ISSUES-CANCELS.                                             EL539S
00566      READ EXTRACT-INTERFACE-FILE  INTO  EXTRACT-INTERFACE-RECORD  EL539S
00567          AT END                                                   EL539S
00568              GO TO 0800-END-INPUT.                                EL539S
00569                                                                   EL539S
00570      IF EX-EXTRACT-CODE  IS GREATER THAN  'A'                     EL539S
00571          GO TO 0800-END-INPUT.                                    EL539S
00572                                                                   EL539S
00573      IF EX-RECORD-TYPE  IS GREATER THAN  'E'                      EL539S
00574          GO TO 0800-END-INPUT.                                    EL539S
00575                                                                   EL539S
00576      IF EX-COMPANY-CD  IS LESS THAN  DTE-CLASIC-COMPANY-CD        EL539S
00577          GO TO 0410-ISSUES-CANCELS.                               EL539S
00578                                                                   EL539S
00579      IF EX-COMPANY-CD  IS GREATER THAN  DTE-CLASIC-COMPANY-CD     EL539S
00580          GO TO 0800-END-INPUT.                                    EL539S
00581                                                                   EL539S
00582      IF EX-RECORD-TYPE  IS NOT EQUAL TO  'A'                      EL539S
00583          GO TO 0410-ISSUES-CANCELS.                               EL539S
00584                                                                   EL539S
00585      MOVE EX-DATA-AREAS          TO  PENDING-BUSINESS.            EL539S
00586                                                                   EL539S
00587      GO TO 0430-PICKUP.                                           EL539S
00588                                                                   EL539S
00589  0420-ISSUES-CANCELS.                                             EL539S
00590      IF DTE-PGM-OPT  IS EQUAL TO  2                               EL539S
00591          GO TO 0410-ISSUES-CANCELS.                               EL539S
00592                                                                   EL539S
00593      READ ERPNDB  NEXT RECORD  INTO  PENDING-BUSINESS.            EL539S
00594                                                                   EL539S
00595      IF PB-STAT-1  IS EQUAL TO  '1'                               EL539S
00596          GO TO 0800-END-INPUT.                                    EL539S
00597                                                                   EL539S
00598      IF PB-STAT-1  IS NOT EQUAL TO  '0'                           EL539S
00599          MOVE PB-STATUS          TO  WS-ABEND-FILE-STATUS         EL539S
00600          MOVE 'ERROR OCCURRED READ - ERPNDB'                      EL539S
00601                                  TO  WS-ABEND-MESSAGE             EL539S
00602          GO TO ABEND-PGM.                                         EL539S
00603                                                                   EL539S
00604  0430-PICKUP.                                                     EL539S
00605      IF PB-COMPANY-CD  IS NOT EQUAL TO  DTE-CLASIC-COMPANY-CD     EL539S
00606          GO TO 0800-END-INPUT.                                    EL539S
00607                                                                   EL539S
00608      IF PB-ISSUE                                                  EL539S
00609        AND CLASIC-CREATED-CERT                                    EL539S
00610          GO TO 0420-ISSUES-CANCELS.                               EL539S
00611                                                                   EL539S
00612      IF PB-ISSUE                                                  EL539S
00613        OR PB-CANCELLATION                                         EL539S
00614          NEXT SENTENCE                                            EL539S
00615      ELSE                                                         EL539S
00616          GO TO 0420-ISSUES-CANCELS.                               EL539S
00617                                                                   EL539S
PEMMOD*    IF PB-CERT-NO = '0610031323 '
PEMMOD*       DISPLAY ' FOUND CERT '
PEMMOD*    END-IF
00618      IF PB-REIN-ONLY-CERT                                         EL539S
00619        OR PB-REISSUED-CERT                                        EL539S
091312*      OR PB-PREM-ACCTNG-ONLY                                     EL539S
00621        OR PB-POLICY-IS-DECLINED                                   EL539S
00622        OR PB-POLICY-IS-VOIDED                                     EL539S
00623          GO TO 0420-ISSUES-CANCELS.                               EL539S
00624                                                                   EL539S
00625      IF PB-UNFORCED-ERRORS                                        EL539S
00626        OR PB-FATAL-ERRORS                                         EL539S
00627        OR PB-RECORD-ON-HOLD                                       EL539S
00628        OR PB-RECORD-RETURNED                                      EL539S
00629          GO TO 0420-ISSUES-CANCELS.                               EL539S
00630                                                                   EL539S
PEMMOD*    MOVE PB-CONTROL-BY-ACCOUNT  TO  PEND-REC.                    EL539S
PEMMOD     MOVE PB-CONTROL-BY-ACCOUNT  TO  PEND-ACCT-CTL                EL539S
00651                                                                   EL539S
00652      IF DTE-COMP-VG  IS EQUAL TO  ' '                             EL539S
00653          MOVE PB-SV-CARRIER      TO  PEND-CARR                    EL539S
00654          MOVE PB-SV-GROUPING     TO  PEND-GRP.                    EL539S
00655                                                                   EL539S
00656      IF DTE-COMP-VG  IS EQUAL TO  '2'                             EL539S
00657          MOVE PB-SV-GROUPING     TO  PEND-GRP.                    EL539S
00658                                                                   EL539S
00659      IF DTE-COMP-VG  IS EQUAL TO  '3'                             EL539S
00660          MOVE PB-SV-CARRIER      TO  PEND-CARR                    EL539S
00661          MOVE PB-SV-GROUPING     TO  PEND-GRP                     EL539S
00662          MOVE PB-SV-STATE        TO  PEND-ST.                     EL539S
00663                                                                   EL539S
00664      IF DTE-COMP-VG  IS EQUAL TO  '4'                             EL539S
00665          MOVE PB-SV-GROUPING     TO  PEND-GRP                     EL539S
00666          MOVE PB-SV-STATE        TO  PEND-ST.                     EL539S
00667                                                                   EL539S
PEMMOD*    MOVE PEND-REC               TO  KEY-20.                      EL539S
PEMMOD     MOVE PEND-ACCT-CTL          TO  KEY-20.                      EL539S
00669                                                                   EL539S
00670      IF (SKIP-THIS-ACCT  = 'Y') AND                               EL539S
00671         (KEY-20  = LST-ACCT)                                      EL539S
00672          GO TO 0420-ISSUES-CANCELS.                               EL539S
00673                                                                   EL539S
00674      IF KEY-20  IS NOT EQUAL TO  LST-ACCT                         EL539S
00675          PERFORM 0700-GET-ACCT  THRU  0799-EXIT.                  EL539S
00676                                                                   EL539S
00677      IF SKIP-THIS-ACCT  IS EQUAL TO  'Y'                          EL539S
00678          GO TO 0420-ISSUES-CANCELS.                               EL539S
00679                                                                   EL539S
00680      MOVE PB-CERT-EFF-DT         TO  DC-BIN-DATE-1.               EL539S
00681      MOVE SPACE                  TO  DC-OPTION-CODE.              EL539S
00682                                                                   EL539S
00683      PERFORM 0500-DATE-RTN  THRU  0599-EXIT.                      EL539S
00684                                                                   EL539S
00685      MOVE DC-GREG-DATE-CYMD      TO  PEND-YRMODA.                    CL**7
00686      MOVE PB-CERT-PRIME          TO  PEND-CERT-PRIME.             EL539S
00687      MOVE PB-CERT-SFX            TO  PEND-CERT-SFX.               EL539S
00688                                                                   EL539S
00689      IF NOT PB-ISSUE                                              EL539S
00690          GO TO 0440-CHK-CANCEL.                                   EL539S
00691                                                                   EL539S
00692      IF PB-I-LOAN-OFFICER  IS EQUAL TO  SPACE                     EL539S
00693          MOVE '999'              TO  PB-I-LOAN-OFFICER.           EL539S
00694                                                                   EL539S
060906     IF PB-I-LOAN-OFFICER (5:1) = LOW-VALUES
060906        MOVE SPACES TO PB-I-LOAN-OFFICER (5:1)
060906     END-IF
060906     IF PB-I-LOAN-OFFICER (4:1) = LOW-VALUES
060906        MOVE SPACES TO PB-I-LOAN-OFFICER (4:1)
060906     END-IF
00695      MOVE PB-I-LOAN-OFFICER      TO  PEND-OFFICER.                EL539S
00696      MOVE PEND-REC               TO  KEY-23.                      EL539S
00697                                                                   EL539S
00698      IF KEY-23  IS NOT EQUAL TO  LST-OFFCR                        EL539S
00699          PERFORM 0600-GET-LOAN-OFFICER  THRU  0699-EXIT.          EL539S
00700                                                                   EL539S
00701      IF SKIP-THIS-LOAN-OFCR EQUAL 'Y'                             EL539S
00702          GO TO 0420-ISSUES-CANCELS.                               EL539S
00703                                                                   EL539S
PEMMOD     MOVE LO-OFFICER-NAME        TO  PEND-OFFICER-NAME
00704      MOVE PB-I-INSURED-LAST-NAME TO  PEND-NAME.                   EL539S
00705      MOVE PB-I-INSURED-1ST-INIT  TO  PEND-INIT.                   EL539S
00706      MOVE PB-I-AGE               TO  PEND-AGE.                    EL539S
00707      MOVE PB-I-LF-ABBR           TO  PEND-LF-TYP.                 EL539S
00708      MOVE PB-I-LF-PREMIUM-AMT    TO  PEND-LF-PRM.                 EL539S
00709                                                                   EL539S
00710      IF PB-I-LF-ALT-PREMIUM-AMT  IS GREATER THAN  ZEROS           EL539S
00711          COMPUTE PEND-LF-PRM = PEND-LF-PRM                        EL539S
00712                              + PB-I-LF-ALT-PREMIUM-AMT.           EL539S
00713                                                                   EL539S
00714      MOVE PB-I-LF-BENEFIT-AMT    TO  PEND-LF-AMT.                 EL539S
00715                                                                   EL539S
00716      IF PB-I-LF-ALT-BENEFIT-AMT  IS GREATER THAN  ZEROS           EL539S
00717          COMPUTE PEND-LF-AMT = PEND-LF-AMT                        EL539S
00718                              + PB-I-LF-ALT-BENEFIT-AMT.           EL539S
00719                                                                   EL539S
00720      MOVE PB-I-AH-ABBR           TO  PEND-AH-TYP.                 EL539S
00721      MOVE PB-I-AH-PREMIUM-AMT    TO  PEND-AH-PRM.                 EL539S
00722                                                                   EL539S
00734                                                                   EL539S
00735  0435-CONTINUE.                                                   EL539S

PEMMOD*    MULTIPLY PB-I-AH-BENEFIT-AMT  BY  PB-I-AH-TERM               EL539S
PEMMOD*        GIVING  PEND-AH-AMT.                                     EL539S
PEMMOD     MOVE PB-I-AH-BENEFIT-AMT    TO  PEND-AH-AMT
00738                                                                   EL539S
00739      MOVE PB-I-LF-TERM           TO  PEND-LF-TERM.                EL539S
00740      MOVE PB-I-AH-TERM           TO  PEND-AH-TERM.                EL539S
00741                                                                   EL539S
00742      COMPUTE PEND-LF-COM ROUNDED =                                EL539S
00743          PEND-LF-PRM * PB-I-LIFE-COMMISSION.                      EL539S
00744      COMPUTE PEND-AH-COM ROUNDED =                                EL539S
00745          PEND-AH-PRM * PB-I-AH-COMMISSION.                        EL539S
00746                                                                   EL539S
00747      MOVE PEND-REC               TO  SORT-RECORD.                 EL539S
00748                                                                   EL539S
00749      PERFORM 0310-RELEASE  THRU  0319-EXIT.                       EL539S
00750                                                                   EL539S
00751      GO TO 0420-ISSUES-CANCELS.                                   EL539S
00752  EJECT                                                            EL539S
00753  0440-CHK-CANCEL.                                                 EL539S
00754      IF NOT PB-CANCELLATION                                       EL539S
00755          GO TO 0420-ISSUES-CANCELS.                               EL539S
00756                                                                   EL539S
      *    MOVE '  ' TO PB-CI-LOAN-OFFICER (4:2)
00757      IF PB-CI-LOAN-OFFICER  IS EQUAL TO  SPACE                    EL539S
00758          MOVE '999'              TO  PB-CI-LOAN-OFFICER.          EL539S
           display ' cancel loan officer ' pb-ci-loan-officer
00759                                                                   EL539S
060906     IF PB-CI-LOAN-OFFICER (5:1) = LOW-VALUES
060906        MOVE SPACES TO PB-CI-LOAN-OFFICER (5:1)
060906     END-IF
060906     IF PB-CI-LOAN-OFFICER (4:1) = LOW-VALUES
060906        MOVE SPACES TO PB-CI-LOAN-OFFICER (4:1)
060906     END-IF
00760      MOVE PB-CI-LOAN-OFFICER     TO  PEND-OFFICER.                EL539S
00761      MOVE PEND-REC               TO  KEY-23.                      EL539S
00762                                                                   EL539S
00763      IF KEY-23  IS NOT EQUAL TO  LST-OFFCR                        EL539S
00764          PERFORM 0600-GET-LOAN-OFFICER  THRU  0699-EXIT.          EL539S
00765                                                                   EL539S
00766      IF SKIP-THIS-LOAN-OFCR EQUAL 'Y'                             EL539S
00767          GO TO 0420-ISSUES-CANCELS.                               EL539S
00768                                                                   EL539S
PEMMOD     MOVE LO-OFFICER-NAME        TO  PEND-OFFICER-NAME
00769      MOVE PB-CI-LAST-NAME        TO  PEND-NAME.                   EL539S
00770      MOVE PB-CI-INITIALS         TO  PEND-INIT.                   EL539S
00771      MOVE PB-CI-INSURED-AGE      TO  PEND-AGE.                    EL539S
00772      MOVE PB-CI-LF-ABBR          TO  PEND-LF-TYP.                 EL539S
00773      MOVE PB-CI-AH-ABBR          TO  PEND-AH-TYP.                 EL539S
00774      MOVE PB-CI-LF-TERM          TO  PEND-LF-TERM.                EL539S
00775      MOVE PB-CI-AH-TERM          TO  PEND-AH-TERM.                EL539S
00776                                                                   EL539S
00777      MULTIPLY PB-C-LF-CANCEL-AMT  BY  -1  GIVING  PEND-LF-PRM.    EL539S
00778      MULTIPLY PB-C-AH-CANCEL-AMT  BY  -1  GIVING  PEND-AH-PRM.    EL539S
00779                                                                   EL539S
00791  0535-CONTINUE.                                                   EL539S
00790                                                                   EL539S
00792      MOVE ZERO                   TO  PEND-LF-AMT  PEND-AH-AMT.    EL539S
00793                                                                   EL539S
00794      COMPUTE PEND-LF-COM ROUNDED = PB-CI-LIFE-COMMISSION          EL539S
00795                                  * PEND-LF-PRM.                   EL539S
00796      COMPUTE PEND-AH-COM ROUNDED = PB-CI-AH-COMMISSION            EL539S
00797                                  * PEND-AH-PRM.                   EL539S
00798                                                                   EL539S
00799      MOVE PEND-REC               TO  SORT-RECORD.                 EL539S
00800                                                                   EL539S
00801      PERFORM 0310-RELEASE  THRU  0319-EXIT.                       EL539S
00802                                                                   EL539S
00803      GO TO 0420-ISSUES-CANCELS.                                   EL539S
00804  EJECT                                                            EL539S
00805  0500-DATE-RTN.                                                   EL539S
00806      CALL 'ELDATCX'  USING  DATE-CONVERSION-DATA.                 EL539S
00807                                                                   EL539S
00808      IF DC-ERROR-CODE  IS NOT EQUAL TO  SPACE                     EL539S
00809          MOVE ZERO               TO  DC-CONVERSION-DATES.         EL539S
00810                                                                   EL539S
00811  0599-EXIT.                                                       EL539S
00812      EXIT.                                                        EL539S
00813                                                                   EL539S
00814  0600-GET-LOAN-OFFICER.                                           EL539S
00815      MOVE PEND-REC               TO  LST-OFFCR.                   EL539S
00816      MOVE SPACES                 TO  LO-CONTROL-PRIMARY.          EL539S
00817      MOVE PB-COMPANY-CD-A1       TO  LO-COMPANY-CD.               EL539S
00818      MOVE PB-CARRIER             TO  LO-CARRIER.                  EL539S
00819      MOVE PB-GROUPING            TO  LO-GROUPING.                 EL539S
00820      MOVE PB-STATE               TO  LO-STATE.                    EL539S
00821      MOVE PB-ACCOUNT             TO  LO-ACCOUNT.                  EL539S
00822      MOVE PEND-OFFICER           TO  LO-OFFICER-CODE.             EL539S
00823                                                                   EL539S
           if pb-cancellation
              display ' 0600 lo key ' lo-control-primary (2:24)
           END-IF

00824      READ ERLOFC.                                                 EL539S
00825                                                                   EL539S
00826      IF LO-STAT-1  IS EQUAL TO  '1'                               EL539S
00827          GO TO 0620-INV-LO.                                       EL539S
00828                                                                   EL539S
00829      IF LO-STATUS  IS EQUAL TO  '23'                              EL539S
00830          GO TO 0620-INV-LO.                                       EL539S
00831                                                                   EL539S
00832      IF LO-COMPANY-CD  IS NOT EQUAL TO  PB-COMPANY-CD-A1          EL539S
00833          GO TO 0620-INV-LO.                                       EL539S
00834                                                                   EL539S
00835      IF LO-STATUS  IS NOT EQUAL TO  ZERO                          EL539S
00836          MOVE LO-STATUS          TO  WS-ABEND-FILE-STATUS         EL539S
00837          MOVE 'ERROR OCCURRED READ - ERLOFC'                      EL539S
00838                                  TO  WS-ABEND-MESSAGE             EL539S
00839          GO TO ABEND-PGM.                                         EL539S
00840                                                                   EL539S
PEMMOD     IF (LO-CONTROL-PRIMARY (1:20) > KEY-23 (4:20))
PEMMOD                          OR
PEMMOD        ((LO-CONTROL-PRIMARY (1:20) = KEY-23 (4:20)) AND
110105        (LO-CONTROL-PRIMARY (21:5) > KEY-23 (1:5)))
PEMMOD        GO TO 0620-INV-LO.                                        EL539S
PEMMOD
PEMMOD*    IF LO-CONTROL-PRIMARY  IS GREATER THAN  KEY-23               EL539S
PEMMOD*        GO TO 0620-INV-LO.                                       EL539S
00843                                                                   EL539S
PEMMOD*    MOVE LO-OFFICER-NAME        TO  LN-OFFCR.                    EL539S
00845      MOVE LO-OPT1                TO  LN-OPT1.                     EL539S
00846      MOVE LO-OPT2                TO  LN-OPT2.                     EL539S
PEMMOD*    MOVE LO-CONTROL-PRIMARY     TO  LST-OFFCR.                   EL539S
PEMMOD     MOVE LO-CONTROL-PRIMARY     TO  LST-REST                     EL539S
PEMMOD     MOVE LO-OFFICER-CODE        TO  LST-LO-CODE.
00848                                                                   EL539S
00849  0610-PUT-LO.                                                     EL539S
00850      MOVE LO-COMPANY-CD          TO  LN-CO.                       EL539S
00851      MOVE PEND-CARR              TO  LN-CARR.                     EL539S
00852      MOVE PEND-GRP               TO  LN-GRP.                      EL539S
00853      MOVE PEND-ST                TO  LN-ST.                       EL539S
00854      MOVE LO-ACCOUNT             TO  LN-ACCT.                     EL539S
00855      MOVE LO-OFFICER-CODE        TO  LN-OFFCODE.                  EL539S
00856      MOVE LOW-VALUES             TO  LN-LOVAL.                    EL539S
PEMMOD*    MOVE LO-OFFICER-NAME        TO  LN-OFFCR.                    EL539S
00858      MOVE LO-COMP-CONTROL        TO  LN-OPT1.                     EL539S
00859      MOVE LO-DETAIL-CONTROL      TO  LN-OPT2.                     EL539S
00860      MOVE LN-REC                 TO  SORT-RECORD.                 EL539S
00861                                                                   EL539S
PEMMOD*    PERFORM 0310-RELEASE  THRU  0319-EXIT.                       EL539S
00863                                                                   EL539S
00864      MOVE 'N'                    TO  SKIP-THIS-LOAN-OFCR.         EL539S
00865                                                                   EL539S
00866      GO TO 0699-EXIT.                                             EL539S
00867                                                                   EL539S
00868  0620-INV-LO.                                                     EL539S
00869      MOVE HIGH-VALUES            TO  PEND-OFFICER  LST-OFFCR.     EL539S
00870                                                                   EL539S
00871      IF DTE-FMT-OPT EQUAL '2'                                     EL539S
00872          MOVE 'Y'                TO  SKIP-THIS-LOAN-OFCR          EL539S
00873          GO TO 0699-EXIT.                                         EL539S
00874                                                                   EL539S
00875      IF HAVE-HIGH-VALUE  IS EQUAL TO  'Y'                         EL539S
00876          GO TO 0699-EXIT.                                         EL539S
00877                                                                   EL539S
00878      MOVE PEND-CO                TO  LN-CO.                       EL539S
00879      MOVE PEND-CARR              TO  LN-CARR.                     EL539S
00880      MOVE PEND-GRP               TO  LN-GRP.                      EL539S
00881      MOVE PEND-ST                TO  LN-ST.                       EL539S
00882      MOVE PB-ACCOUNT             TO  LN-ACCT.                     EL539S
00883      MOVE HIGH-VALUES            TO  LN-OFFCODE.                  EL539S
00884      MOVE LOW-VALUE              TO  LN-LOVAL.                    EL539S
PEMMOD*    MOVE 'UNASSIGNED'           TO  LN-OFFCR.                    EL539S
PEMMOD     MOVE 'UNASSIGNED'           TO  LO-OFFICER-NAME              EL539S
00886      MOVE 'N'                    TO  LN-OPT1.                     EL539S
00887      MOVE 'D'                    TO  LN-OPT2.                     EL539S
00888      MOVE 'Y'                    TO  HAVE-HIGH-VALUE.             EL539S
00889      MOVE LN-REC                 TO  SORT-RECORD.                 EL539S
00890      MOVE LN-REC                 TO  LST-OFFCR.                   EL539S
00891                                                                   EL539S
PEMMOD*    PERFORM 0310-RELEASE  THRU  0319-EXIT.                       EL539S
00893      .                                                            EL539S
00894  0699-EXIT.                                                       EL539S
00895      EXIT.                                                        EL539S
00896  EJECT                                                            EL539S
00897  0700-GET-ACCT.                                                   EL539S
00898      MOVE SPACE                  TO  AM-CONTROL-PRIMARY.          EL539S
00899      MOVE 'N'                    TO  HAVE-HIGH-VALUE.             EL539S
00900      MOVE LOW-VALUES             TO  LST-OFFCR  HLD-CTL.          EL539S
00901      MOVE PEND-CO                TO  AM-COMPANY-CD.               EL539S
00902      MOVE PEND-CARR              TO  AM-CARRIER.                  EL539S
00903      MOVE PEND-GRP               TO  AM-GROUPING.                 EL539S
00904      MOVE PEND-ST                TO  AM-STATE.                    EL539S
00905      MOVE PB-ACCOUNT             TO  AM-ACCOUNT.                  EL539S
PEMMOD     MOVE PB-CERT-EFF-DT         TO  AM-EXPIRATION-DT
00906                                                                   EL539S
00907      START ERACCT  KEY  NOT LESS THAN  AM-CONTROL-PRIMARY.        EL539S
00908                                                                   EL539S
00909      IF AM-FILE-STATUS  IS EQUAL TO  '23'                         EL539S
00910          GO TO 0720-INVALID-ACCOUNT.                              EL539S
00911                                                                   EL539S
00912      IF AM-STAT-1  IS NOT EQUAL TO  '0'                           EL539S
00913          MOVE AM-FILE-STATUS     TO  WS-ABEND-FILE-STATUS         EL539S
00914          MOVE 'ERROR OCCURRED START - ERACCT'                     EL539S
00915                                  TO  WS-ABEND-MESSAGE             EL539S
00916          GO TO ABEND-PGM.                                         EL539S
00917                                                                   EL539S
00918  0710-NEXT-ACCT.                                                  EL539S
00919      READ ERACCT  NEXT RECORD.                                    EL539S
00920                                                                   EL539S
00921      IF AM-STAT-1  IS EQUAL TO  '1'                               EL539S
00922          GO TO 0720-INVALID-ACCOUNT.                              EL539S
00923                                                                   EL539S
00924      IF AM-COMPANY-CD  IS NOT EQUAL TO  PB-COMPANY-CD-A1          EL539S
00925          GO TO 0720-INVALID-ACCOUNT.                              EL539S
00926                                                                   EL539S
00927      IF AM-STAT-1  IS NOT EQUAL TO  '0'                           EL539S
00928          MOVE AM-FILE-STATUS     TO  WS-ABEND-FILE-STATUS         EL539S
00929          MOVE 'ERROR OCCURRED READ - ERACCT'                      EL539S
00930                                  TO  WS-ABEND-MESSAGE             EL539S
00931          GO TO ABEND-PGM.                                         EL539S
00932                                                                   EL539S
00933      IF PB-ACCOUNT  IS EQUAL TO  AM-ACCOUNT                       EL539S
00934          GO TO 0740-VALID-ACCOUNT.                                EL539S
00935                                                                   EL539S
00936  0720-INVALID-ACCOUNT.                                            EL539S
00937      MOVE 'Y'                    TO  SKIP-THIS-ACCT.              EL539S
00938      MOVE KEY-20                 TO  LST-ACCT.                    EL539S
00939                                                                   EL539S
00940      GO TO 0799-EXIT.                                             EL539S
00941                                                                   EL539S
00942  0730-PUT-ACCOUNT.                                                EL539S
PEMMOD     MOVE LO-OFFICER-CODE        TO  ACCT-LO-OFF-CD
00943      MOVE AM-COMPANY-CD          TO  ACCT-CO.                     EL539S
00944      MOVE AM-CARRIER             TO  ACCT-CARR.                   EL539S
00945      MOVE AM-GROUPING            TO  ACCT-GRP.                    EL539S
00946      MOVE AM-STATE               TO  ACCT-ST.                     EL539S
00947      MOVE AM-ACCOUNT             TO  ACCT-NUM.                    EL539S
PEMMOD*    MOVE '111'                  TO  ACCT-LOVAL.                  EL539S
PEMMOD     MOVE LOW-VALUES             TO  ACCT-LOVAL.                  EL539S
00949      MOVE ACCT-REC               TO  SORT-RECORD.                 EL539S
00950                                                                   EL539S
PEMMOD     IF (AM-EDIT-LOAN-OFC NOT = 'Y')                              EL539S
PEMMOD                  OR
PEMMOD        ((WS-BANK NOT = SPACES) AND
PEMMOD         (WS-BANK NOT = AM-USER-SELECT-1))
00952          MOVE 'Y'                TO  SKIP-THIS-ACCT               EL539S
PEMMOD         MOVE KEY-20             TO  LST-ACCT                     EL539S
00953          GO TO 0799-EXIT                                          EL539S
00954      ELSE                                                         EL539S
00955          MOVE 'N'                TO  SKIP-THIS-ACCT.              EL539S
00956                                                                   EL539S
PEMMOD*    PERFORM 0310-RELEASE  THRU  0319-EXIT.                       EL539S
00958                                                                   EL539S
00959      MOVE KEY-20                 TO  LST-ACCT.                    EL539S
00960      MOVE 'N'                    TO  HAVE-HIGH-VALUE.             EL539S
00961                                                                   EL539S
00962      GO TO 0799-EXIT.                                             EL539S
00963                                                                   EL539S
00964  0740-VALID-ACCOUNT.                                              EL539S
00965      MOVE 'N'                    TO  SKIP-THIS-ACCT.              EL539S
00966      MOVE AM-NAME                TO  ACCT-NAME.                   EL539S
00967      MOVE AM-PERSON              TO  ACCT-PERSON.                 EL539S
00968      MOVE AM-ADDRS               TO  ACCT-ADDR1.                  EL539S
00969      MOVE AM-CITY                TO  ACCT-CTY-ST.                 EL539S
00970                                                                   EL539S
00971      IF  AM-CANADIAN-POST-CODE                                    EL539S
00972          MOVE AM-CAN-POSTAL-1    TO ACCT-CAN-POSTAL-CODE-1        EL539S
00973          MOVE AM-CAN-POSTAL-2    TO ACCT-CAN-POSTAL-CODE-2        EL539S
00974          MOVE SPACES             TO ACCT-CAN-FILLER               EL539S
00975                                                                   EL539S
00976      ELSE                                                         EL539S
00977          MOVE AM-ZIP-PRIME       TO ACCT-ZIP-PRIME                EL539S
00978                                                                   EL539S
00979          IF  AM-ZIP-PLUS4 = SPACES OR ZEROS                       EL539S
00980              MOVE SPACES         TO ACCT-ZIP-PLUS4                EL539S
00981                                                                   EL539S
00982          ELSE                                                     EL539S
00983              MOVE AM-ZIP-PLUS4   TO ACCT-ZIP-PLUS4.               EL539S
00984                                                                   EL539S
00985      GO TO 0730-PUT-ACCOUNT.                                      EL539S
00986                                                                   EL539S
00987  0799-EXIT.                                                       EL539S
00988      EXIT.                                                        EL539S
00989                                                                   EL539S
00990  0800-END-INPUT.                                                  EL539S
00991                                                                   EL539S
00992 *0810-GET-LOAN-OFFICERS.                                          EL539S
00993 *    MOVE SPACES                 TO  SAVE-ACCOUNT                 EL539S
00994 *                                    LO-CONTROL-PRIMARY.          EL539S
00995 *    MOVE DTE-CLASIC-COMPANY-CD  TO  LO-COMPANY-CD.               EL539S
00996 *                                                                 EL539S
00997 *    START ERLOFC  KEY  NOT LESS THAN  LO-CONTROL-PRIMARY.        EL539S
00998 *                                                                 EL539S
00999 *    IF LO-STATUS  IS NOT EQUAL TO  ZERO                          EL539S
01000 *        MOVE LO-STATUS          TO  WS-ABEND-FILE-STATUS         EL539S
01001 *        MOVE 'ERROR OCCURRED START - EROFCR'                     EL539S
01002 *                                TO  WS-ABEND-MESSAGE             EL539S
01003 *        GO TO ABEND-PGM.                                         EL539S
01004 *                                                                 EL539S
01005 *0820-GET-NEXT-OFFICER.                                           EL539S
01006 *    READ ERLOFC  NEXT RECORD.                                    EL539S
01007 *                                                                 EL539S
01008 *    IF DTE-FMT-OPT EQUAL '2'                                     EL539S
01009 *        GO TO 1099-EXIT.                                         EL539S
01010 *                                                                 EL539S
01011 *    IF LO-STATUS  IS EQUAL TO  '23'  OR  '10'                    EL539S
01012 *        GO TO 1099-EXIT.                                         EL539S
01013 *                                                                 EL539S
01014 *    IF LO-STATUS  IS NOT EQUAL TO  ZERO                          EL539S
01015 *        MOVE LO-STATUS          TO  WS-ABEND-FILE-STATUS         EL539S
01016 *        MOVE 'ERROR OCCURRED READ - EROFCR'                      EL539S
01017 *                                TO  WS-ABEND-MESSAGE             EL539S
01018 *        GO TO ABEND-PGM.                                         EL539S
01019 *                                                                 EL539S
01020 *    IF LO-COMPANY-CD  IS NOT EQUAL TO  DTE-CLASIC-COMPANY-CD     EL539S
01021 *        GO TO 1099-EXIT.                                         EL539S
01022 *                                                                 EL539S
01023 *    IF LO-LOAN-COUNT (RUN-MO) IS GREATER THAN  ZERO              EL539S
01024 *      OR LO-LOAN-VOLUME (RUN-MO) IS GREATER THAN  ZERO           EL539S
01025 *        NEXT SENTENCE                                            EL539S
01026 *    ELSE                                                         EL539S
01027 *        GO TO 0820-GET-NEXT-OFFICER.                             EL539S
01028 *                                                                 EL539S
01029 *    MOVE LO-COMPANY-CD          TO  LN-CO.                       EL539S
01030 *    MOVE LO-CARRIER             TO  LN-CARR.                     EL539S
01031 *    MOVE LO-GROUPING            TO  LN-GRP.                      EL539S
01032 *    MOVE LO-STATE               TO  LN-ST.                       EL539S
01033 *    MOVE LO-ACCOUNT             TO  LN-ACCT.                     EL539S
01034 *                                                                 EL539S
01035 *    IF DTE-COMP-VG  IS EQUAL TO  ' '                             EL539S
01036 *        MOVE LO-SV-CARRIER      TO  LN-CARR                      EL539S
01037 *        MOVE LO-SV-GROUPING     TO  LN-GRP.                      EL539S
01038 *                                                                 EL539S
01039 *    IF DTE-COMP-VG  IS EQUAL TO  '2'                             EL539S
01040 *        MOVE LO-SV-GROUPING     TO  LN-GRP.                      EL539S
01041 *                                                                 EL539S
01042 *    IF DTE-COMP-VG  IS EQUAL TO  '3'                             EL539S
01043 *        MOVE LO-SV-CARRIER      TO  LN-CARR                      EL539S
01044 *        MOVE LO-SV-GROUPING     TO  LN-GRP                       EL539S
01045 *        MOVE LO-SV-STATE        TO  LN-ST.                       EL539S
01046 *                                                                 EL539S
01047 *    IF DTE-COMP-VG  IS EQUAL TO  '4'                             EL539S
01048 *        MOVE LO-SV-GROUPING     TO  LN-GRP                       EL539S
01049 *        MOVE LO-SV-STATE        TO  LN-ST.                       EL539S
01050 *                                                                 EL539S
01051 *    MOVE LO-OFFICER-CODE        TO  LN-OFFCODE.                  EL539S
01052 *    MOVE LOW-VALUES             TO  LN-LOVAL.                    EL539S
01053 *    MOVE LO-OFFICER-NAME        TO  LN-OFFCR.                    EL539S
01054 *    MOVE LO-COMP-CONTROL        TO  LN-OPT1.                     EL539S
01055 *    MOVE LO-DETAIL-CONTROL      TO  LN-OPT2.                     EL539S
01056 *                                                                 EL539S
PEMMOD*    IF LO-ACCOUNT  IS EQUAL TO  SAVE-ACCOUNT                     EL539S
PEMMOD*        GO TO 0830-RELEASE-LOAN-OFFICER.                         EL539S
01059 *                                                                 EL539S
01060 *    MOVE LO-ACCOUNT             TO  SAVE-ACCOUNT.                EL539S
01061 *                                                                 EL539S
01062 *    PERFORM 0900-GET-ACCOUNT  THRU  0999-EXIT.                   EL539S
01063 *                                                                 EL539S
01064 *0830-RELEASE-LOAN-OFFICER.                                       EL539S
01065 *    IF SKIP-THIS-ACCT  IS EQUAL TO  'Y'                          EL539S
01066 *        GO TO 0820-GET-NEXT-OFFICER.                             EL539S
01067 *                                                                 EL539S
01068 *    MOVE LN-REC                 TO  SORT-RECORD.                 EL539S
01069 *                                                                 EL539S
01070 *    PERFORM 0310-RELEASE  THRU  0319-EXIT.                       EL539S
01071 *                                                                 EL539S
01072 *    GO TO 0820-GET-NEXT-OFFICER.                                 EL539S
01073  EJECT                                                            EL539S
01074 *0900-GET-ACCOUNT.                                                EL539S
01075 *    MOVE LN-CO                  TO  AM-COMPANY-CD.               EL539S
01076 *    MOVE LN-CARR                TO  AM-CARRIER.                  EL539S
01077 *    MOVE LN-GRP                 TO  AM-GROUPING.                 EL539S
01078 *    MOVE LN-ST                  TO  AM-STATE.                    EL539S
01079 *    MOVE LN-ACCT                TO  AM-ACCOUNT.                  EL539S
01080 *                                                                 EL539S
01081 *                                                                 EL539S
01082 *    START ERACCT  KEY  NOT LESS THAN  AM-CONTROL-PRIMARY.        EL539S
01083 *                                                                 EL539S
01084 *    IF AM-FILE-STATUS  IS EQUAL TO  '23'                         EL539S
01085 *        GO TO 0920-INVALID-ACCOUNT.                              EL539S
01086 *                                                                 EL539S
01087 *    IF AM-STAT-1  IS NOT EQUAL TO  '0'                           EL539S
01088 *        MOVE AM-FILE-STATUS     TO  WS-ABEND-FILE-STATUS         EL539S
01089 *        MOVE 'ERROR OCCURRED START - ERACCT'                     EL539S
01090 *                                TO  WS-ABEND-MESSAGE             EL539S
01091 *        GO TO ABEND-PGM.                                         EL539S
01092 *                                                                 EL539S
01093 *0910-NEXT-ACCT.                                                  EL539S
01094 *    READ ERACCT  NEXT RECORD.                                    EL539S
01095 *                                                                 EL539S
01096 *    IF AM-STAT-1  IS EQUAL TO  '1'                               EL539S
01097 *        GO TO 0920-INVALID-ACCOUNT.                              EL539S
01098 *                                                                 EL539S
01099 *    IF AM-STAT-1  IS NOT EQUAL TO  '0'                           EL539S
01100 *        MOVE AM-FILE-STATUS     TO  WS-ABEND-FILE-STATUS         EL539S
01101 *        MOVE 'ERROR OCCURRED READ - ERACCT'                      EL539S
01102 *                                TO  WS-ABEND-MESSAGE             EL539S
01103 *        GO TO ABEND-PGM.                                         EL539S
01104 *                                                                 EL539S
01105 *    IF AM-COMPANY-CD  IS NOT EQUAL TO  DTE-CLASIC-COMPANY-CD     EL539S
01106 *        GO TO 0920-INVALID-ACCOUNT.                              EL539S
01107 *                                                                 EL539S
01108 *    IF AM-ACCOUNT  IS EQUAL TO  SAVE-ACCOUNT                     EL539S
01109 *        GO TO 0930-VALID-ACCOUNT.                                EL539S
01110 *                                                                 EL539S
01111 *0920-INVALID-ACCOUNT.                                            EL539S
01112 *    MOVE 'Y'                    TO  SKIP-THIS-ACCT.              EL539S
01113 *                                                                 EL539S
01114 *    GO TO 0999-EXIT.                                             EL539S
01115 *                                                                 EL539S
01116 *0930-VALID-ACCOUNT.                                              EL539S
01117 *    MOVE AM-COMPANY-CD          TO  ACCT-CO.                     EL539S
01118 *    MOVE AM-CARRIER             TO  ACCT-CARR.                   EL539S
01119 *    MOVE AM-GROUPING            TO  ACCT-GRP.                    EL539S
01120 *    MOVE AM-STATE               TO  ACCT-ST.                     EL539S
01121 *    MOVE AM-ACCOUNT             TO  ACCT-NUM.                    EL539S
PEMMOD*    MOVE '111'                  TO  ACCT-LOVAL.                  EL539S
PEMMOD*    MOVE LOW-VALUES             TO  ACCT-LOVAL.                  EL539S
01123 *    MOVE AM-NAME                TO  ACCT-NAME.                   EL539S
01124 *    MOVE AM-PERSON              TO  ACCT-PERSON.                 EL539S
01125 *    MOVE AM-ADDRS               TO  ACCT-ADDR1.                  EL539S
01126 *    MOVE AM-CITY                TO  ACCT-CTY-ST.                 EL539S
01127 *                                                                 EL539S
01128 *    IF  AM-CANADIAN-POST-CODE                                    EL539S
01129 *        MOVE AM-CAN-POSTAL-1    TO ACCT-CAN-POSTAL-CODE-1        EL539S
01130 *        MOVE AM-CAN-POSTAL-2    TO ACCT-CAN-POSTAL-CODE-2        EL539S
01131 *        MOVE SPACES             TO ACCT-CAN-FILLER               EL539S
01132 *                                                                 EL539S
01133 *    ELSE                                                         EL539S
01134 *        MOVE AM-ZIP-PRIME       TO ACCT-ZIP-PRIME                EL539S
01135 *                                                                 EL539S
01136 *        IF  AM-ZIP-PLUS4 = SPACES OR ZEROS                       EL539S
01137 *            MOVE SPACES         TO ACCT-ZIP-PLUS4                EL539S
01138 *                                                                 EL539S
01139 *        ELSE                                                     EL539S
01140 *            MOVE AM-ZIP-PLUS4   TO ACCT-ZIP-PLUS4.               EL539S
01141 *                                                                 EL539S
01142 *    MOVE ACCT-REC               TO  SORT-RECORD.                 EL539S
01143 *                                                                 EL539S
01144 *    IF AM-EDIT-LOAN-OFC EQUAL 'N'                                EL539S
01145 *        MOVE 'Y'                TO  SKIP-THIS-ACCT               EL539S
01146 *        GO TO 0999-EXIT                                          EL539S
01147 *    ELSE                                                         EL539S
01148 *        MOVE 'N'                TO  SKIP-THIS-ACCT.              EL539S
01149 *                                                                 EL539S
01150 *    PERFORM 0310-RELEASE  THRU  0319-EXIT.                       EL539S
01151 *                                                                 EL539S
01152 *0999-EXIT.                                                       EL539S
01153 *    EXIT.                                                        EL539S
01154                                                                   EL539S
01155  1099-EXIT.                                                       EL539S
01156      EXIT.                                                        EL539S
01157  EJECT                                                            EL539S
01158  1100-PUT-REPORTS SECTION.                                        EL539S
01159      MOVE LOW-VALUES             TO  CONAMREC  ACCT-REC           EL539S
01160                                      LN-REC    PEND-REC.          EL539S
01161                                                                   EL539S
01162      RETURN SORT-WORK                                             EL539S
01163          AT END                                                   EL539S
01164              GO TO 1150-EOF.                                      EL539S
01165                                                                   EL539S
01166      MOVE SORT-RECORD            TO  CONAMREC.                    EL539S
01167      MOVE CONA-NAME              TO  HD2-CO.                      EL539S
01168      MOVE CONA-ANDT              TO  HD3-DATE.                    EL539S
01169      MOVE WS-CURRENT-DATE        TO  HD2-RUN.                     EL539S
01170                                                                   EL539S
01171      PERFORM 1400-INITIALIZE-TABLE  THRU  1499-EXIT               EL539S
01172          VARYING  TBLX  FROM  1  BY  1                            EL539S
01173              UNTIL  TBLX  IS EQUAL TO  202.                       EL539S
01174                                                                   EL539S
01175      MOVE LOW-VALUES             TO  LN-OFFCODE.                  EL539S
01176                                                                   EL539S
01177  1110-PRINT-LOOP.                                                 EL539S
01178      RETURN SORT-WORK                                             EL539S
01179          AT END                                                   EL539S
01180              GO TO 1150-EOF.                                      EL539S
01181                                                                   EL539S
PEMMOD     MOVE SC-LNOFF               TO  CTLOFCR.                     EL539S
01182      MOVE SC-CO                  TO  CTLCO.                       EL539S
01183      MOVE SC-CARR                TO  CTLCARR.                     EL539S
01184      MOVE SC-GRP                 TO  CTLGRP.                      EL539S
01185      MOVE SC-ST                  TO  CTLST.                       EL539S
01186      MOVE SC-ACCT                TO  CTLACCT.                     EL539S
PEMMOD*    MOVE SC-LNOFF               TO  CTLOFCR.                     EL539S
01188                                                                   EL539S
01189      IF PRVKEY  IS EQUAL TO  LOW-VALUE                            EL539S
01190          MOVE CTLKEY             TO  PRVKEY.                      EL539S
01191                                                                   EL539S
PEMMOD*    IF CPGCTL  IS NOT EQUAL TO  PPGCTL                           EL539S
PEMMOD     IF CTLOFCR NOT = PLO                                         EL539S
01193          PERFORM 2000-MINORS.                                     EL539S
01194                                                                   EL539S
PEMMOD*    IF SC-LNOFF  IS EQUAL TO  LOW-VALUE                          EL539S
PEMMOD*        GO TO 1130-SET-ACCOUNT.                                  EL539S
01197                                                                   EL539S
01198 *    IF SC-EFFDT  IS EQUAL TO  LOW-VALUE                          EL539S
01199 *       GO TO 1140-SET-LOAN-OFFICER                               EL539S
PEMMOD*    ELSE
PEMMOD*       IF SC-EFFDT (1:3) = LOW-VALUES
01196 *          GO TO 1130-SET-ACCOUNT.                                EL539S
01200 *                                                                 EL539S
01201      MOVE 'Y'                    TO  HAVE-DATA.                   EL539S
01202      MOVE SORT-RECORD            TO  PEND-REC.                    EL539S
01203                                                                   EL539S
PEMMOD     IF PEND-OFFICER = HIGH-VALUES                                EL539S
PEMMOD        MOVE SPACE               TO  H10-LOF                      EL539S
PEMMOD     ELSE                                                         EL539S
PEMMOD        MOVE PEND-OFFICER        TO  H10-LOF                      EL539S
PEMMOD     END-IF
PEMMOD                                                                  EL539S
PEMMOD     MOVE '-'                    TO  H10-DSH.                     EL539S
PEMMOD     MOVE PEND-OFFICER-NAME      TO  H10-LONAME.                  EL539S
01204 *    IF LN-OPT2  IS EQUAL TO  'S'                                 EL539S
01205 *        GO TO 1120-ADD-ONLY.                                     EL539S
01206                                                                   EL539S
01207 *    IF LNCT  IS GREATER THAN  45                                 EL539S
PEMMOD     IF LNCT  IS GREATER THAN  43                                 EL539S
01208          PERFORM 1700-PAGE-HEADING  THRU  1799-EXIT.              EL539S
01209                                                                   EL539S
01210      MOVE 'Y'                    TO  PRNTG-DTL.                   EL539S
01211      MOVE SPACE                  TO  DT-1.                        EL539S
01212      MOVE PEND-CERT-PRIME        TO  D1-CERT-PRIME.               EL539S
01213      MOVE PEND-CERT-SFX          TO  D1-CERT-SFX.                 EL539S
01214      MOVE PEND-YRMODA            TO  WS-PEND-YRMODA.                 CL**7
01215      MOVE PEND-MO                TO  D1-EFMO.                     EL539S
01216      MOVE PEND-DA                TO  D1-EFDA.                     EL539S
01217      MOVE PEND-YR                TO  D1-EFYR.                     EL539S
01218      MOVE '/'                    TO  D1-SL1  D1-SL2.              EL539S
01219      MOVE PEND-NAME              TO  D1-NAME.                     EL539S
01220      MOVE PEND-INIT              TO  D1-INIT.                     EL539S
01221      MOVE PEND-AGE               TO  D1-AGE.                      EL539S
01222      MOVE PEND-LF-TERM           TO  D1-LF-TERM.                  EL539S
01223      MOVE PEND-AH-TERM           TO  D1-AH-TERM.                  EL539S
01224      MOVE PEND-LF-TYP            TO  D1-LTYP.                     EL539S
01225      MOVE PEND-LF-PRM            TO  D1-LPRM.                     EL539S
01226      MOVE PEND-LF-AMT            TO  D1-LBEN.                     EL539S
01227      MOVE PEND-AH-TYP            TO  D1-HTYP.                     EL539S
01228      MOVE PEND-AH-PRM            TO  D1-HPRM.                     EL539S
01229      MOVE PEND-AH-AMT            TO  D1-HBEN.                     EL539S
01230                                                                   EL539S
PEMMOD*    IF LN-OPT1  IS EQUAL TO  'Y'                                 EL539S
PEMMOD*       ADD PEND-LF-COM  PEND-AH-COM  GIVING  D1-COMM.            EL539S
01233                                                                   EL539S
01234      IF LNCT  IS EQUAL TO  3                                      EL539S
01235          MOVE ZERO               TO  P-CTL                        EL539S
01236      ELSE                                                         EL539S
01237          MOVE SPACE              TO  P-CTL.                          CL**3
01238                                                                   EL539S
01239      MOVE DT-1                   TO  P-DATA.                      EL539S
01240                                                                   EL539S
01241      PERFORM 1600-PRINT-LINE  THRU  1699-EXIT.                    EL539S
01242                                                                   EL539S
01243      ADD 1                       TO  LNCT.                        EL539S
01244                                                                   EL539S
01245  1120-ADD-ONLY.                                                   EL539S

           MOVE 1 TO TBLX
01345      MOVE PEND-OFFICER           TO  TBL-CODE (TBLX).             EL539S
PEMMOD     MOVE PEND-OFFICER-NAME      TO  TBL-LO-NAME (TBLX).
01246      ADD PEND-LF-AMT             TO  TBL-LBEN (TBLX)              EL539S
PEMMOD                                     FIN-LBEN
01247      ADD PEND-AH-AMT             TO  TBL-DBEN (TBLX)              EL539S
PEMMOD                                     FIN-DBEN
01248      ADD PEND-LF-COM             TO  TBL-LCOM (TBLX)              EL539S
PEMMOD                                     FIN-LCOM
01249      ADD PEND-AH-COM             TO  TBL-DCOM (TBLX)              EL539S
PEMMOD                                     FIN-DCOM
01250                                                                   EL539S
01251      IF PEND-LF-PRM  IS GREATER THAN  ZERO                        EL539S
01252          ADD 1                   TO  TBL-LICT (TBLX)              EL539S
PEMMOD                                     FIN-LICT
01253          ADD PEND-LF-PRM         TO  TBL-LPRM (TBLX)              EL539S
PEMMOD                                     FIN-LPRM
01254      ELSE                                                         EL539S
01255          IF PEND-LF-PRM  IS LESS THAN  ZERO                       EL539S
01256              ADD 1               TO  TBL-LCCT (TBLX)              EL539S
PEMMOD                                     FIN-LCCT
01257              ADD PEND-LF-PRM     TO  TBL-LREF (TBLX)              EL539S
PEMMOD                                     FIN-LREF.
01258                                                                   EL539S
01259      IF PEND-AH-PRM  IS GREATER THAN  ZERO                        EL539S
01260          ADD 1                   TO  TBL-DICT (TBLX)              EL539S
PEMMOD                                     FIN-DICT
01261          ADD PEND-AH-PRM         TO  TBL-DPRM (TBLX)              EL539S
PEMMOD                                     FIN-DPRM
01262      ELSE                                                         EL539S
01263          IF PEND-AH-PRM  IS LESS THAN  ZERO                       EL539S
01264              ADD 1               TO  TBL-DCCT (TBLX)              EL539S
PEMMOD                                     FIN-DCCT
01265              ADD PEND-AH-PRM     TO  TBL-DREF (TBLX)              EL539S
PEMMOD                                     FIN-DREF.
01266                                                                   EL539S
01267      GO TO 1110-PRINT-LOOP.                                       EL539S
01268                                                                   EL539S
01269  1130-SET-ACCOUNT.                                                EL539S
01270      IF SC-ACCT-INFO  IS EQUAL TO  ACCT-INFO                      EL539S
01271          GO TO 1110-PRINT-LOOP.                                   EL539S
01272                                                                   EL539S
01273      IF PRNTG-DTL  IS EQUAL TO  'Y'                               EL539S
01274          PERFORM 1300-DETAIL-TOTALS  THRU  1399-EXIT.             EL539S
01275                                                                   EL539S
01276      MOVE SORT-RECORD            TO  ACCT-REC.                    EL539S
01277      MOVE ACCT-CARR              TO  H5-CARR.                     EL539S
01278      MOVE ACCT-GRP               TO  H5-GRP.                      EL539S
01279      MOVE ACCT-ST                TO  H5-ST.                       EL539S
01280      MOVE ACCT-NUM               TO  H5-ACCT.                     EL539S
01281      MOVE ACCT-NAME              TO  H6-NA.                       EL539S
01282      MOVE ACCT-PERSON            TO  H7-NA.                       EL539S
01283      MOVE ACCT-ADDR1             TO  H8-NA.                       EL539S
01284      MOVE ACCT-CTY-ST            TO  H9-NA.                       EL539S
01285                                                                   EL539S
01286      IF  ACCT-CANADIAN-POST-CODE                                  EL539S
01287          MOVE ACCT-CAN-POSTAL-CODE-1                              EL539S
01288                                  TO H9-CAN-POSTAL-CODE-1          EL539S
01289          MOVE ACCT-CAN-POSTAL-CODE-2                              EL539S
01290                                  TO H9-CAN-POSTAL-CODE-2          EL539S
01291          MOVE SPACES             TO H9-DASH-CAN                   EL539S
01292                                     H9-CAN-FILLER                 EL539S
01293                                                                   EL539S
01294      ELSE                                                         EL539S
01295          MOVE ACCT-ZIP-PRIME     TO H9-ZIP-PRIME                  EL539S
01296          MOVE SPACES             TO H9-DASH                       EL539S
01297                                                                   EL539S
01298          IF  ACCT-ZIP-PLUS4 = SPACES OR ZEROS                     EL539S
01299              MOVE SPACES         TO H9-ZIP-PLUS4                  EL539S
01300                                                                   EL539S
01301          ELSE                                                     EL539S
01302              MOVE ACCT-ZIP-PLUS4 TO H9-ZIP-PLUS4.                 EL539S
01303                                                                   EL539S
01304      MOVE 'N'                    TO  HAVE-DATA.                   EL539S
01305      MOVE LOW-VALUES             TO  LN-OFFCODE.                  EL539S
01306      MOVE ZERO                   TO  TBLX.                        EL539S
01307      MOVE 80                     TO  LNCT.                        EL539S
01308      MOVE ACCT-CO                TO  TBL-CO.                      EL539S
01309      MOVE ACCT-CARR              TO  TBL-CARR.                    EL539S
01310      MOVE ACCT-GRP               TO  TBL-GRP.                     EL539S
01311      MOVE ACCT-ST                TO  TBL-ST.                      EL539S
01312      MOVE ACCT-NUM               TO  TBL-ACCT.                    EL539S
01313      MOVE ACCT-NAME              TO  TBL-NAME.                    EL539S
01314      MOVE ACCT-PERSON            TO  TBL-MAIL1.                   EL539S
01315      MOVE ACCT-ADDR1             TO  TBL-MAIL2.                   EL539S
01316      MOVE ACCT-CTY-ST            TO  TBL-MAIL3.                   EL539S
01317      MOVE ACCT-ZIP               TO  TBL-ZIP.                     EL539S
01318                                                                   EL539S
01319      GO TO 1110-PRINT-LOOP.                                       EL539S
01320                                                                   EL539S
01321  1140-SET-LOAN-OFFICER.                                           EL539S
01322      IF SC-LNOFF  IS EQUAL TO  LN-OFFCODE                         EL539S
01323          GO TO 1110-PRINT-LOOP.                                   EL539S
01324                                                                   EL539S
01325      IF PRNTG-DTL  IS EQUAL TO  'Y'                               EL539S
01326          PERFORM 1300-DETAIL-TOTALS  THRU  1399-EXIT.             EL539S
01327                                                                   EL539S
01328      MOVE SORT-RECORD            TO  LN-REC.                      EL539S
01329                                                                   EL539S
01330      IF LN-OFFCODE  IS EQUAL TO  HIGH-VALUES                      EL539S
01331          MOVE SPACE              TO  H10-LOF                      EL539S
01332      ELSE                                                         EL539S
01333          MOVE LN-OFFCODE         TO  H10-LOF.                     EL539S
01334                                                                   EL539S
01335      MOVE '-'                    TO  H10-DSH.                     EL539S
01336      MOVE LN-OFFCR               TO  H10-LONAME.                  EL539S
01337                                                                   EL539S
01338      ADD 1                       TO  TBLX.                        EL539S
01339                                                                   EL539S
01340      IF TBLX  IS GREATER THAN  200                                EL539S
01341          DISPLAY ' TOO MANY ENTRIES FOR TABLE ' SORT-CTL ' ' TBLX EL539S
01342          MOVE 1                  TO  TBLX                         EL539S
01343          GO TO ABEND-PGM.                                         EL539S
01344                                                                   EL539S
01345      MOVE LN-OFFCODE             TO  TBL-CODE (TBLX).             EL539S
PEMMOD     MOVE LN-OFFCR               TO  TBL-LO-NAME (TBLX).
01346      MOVE 80                     TO  LNCT.                        EL539S
01347                                                                   EL539S
01348      GO TO 1110-PRINT-LOOP.                                       EL539S
01349                                                                   EL539S
01350  1150-EOF.                                                        EL539S
01351      PERFORM 2000-MINORS.                                         EL539S
01352                                                                   EL539S
PEMMOD     PERFORM 3000-FINAL-TOTALS   THRU 3000-EXIT
PEMMOD
01353      IF DTE-PGM-OPT  IS EQUAL TO  2                               EL539S
01354          CLOSE EXTRACT-INTERFACE-FILE                             EL539S
01355      ELSE                                                         EL539S
01356          CLOSE ERPNDB.                                            EL539S
01357                                                                   EL539S
01358      CLOSE ERLOFC  ERACCT.                                        EL539S
01359                                                                   EL539S
01360      IF DTE-PRT-OPT  IS EQUAL TO  'P'  OR  'B'  OR  'T'           EL539S
01361          CLOSE PRINT-FILE.                                        EL539S
01362                                                                   EL539S
01363  1199-EXIT.                                                       EL539S
01364      EXIT.                                                        EL539S
01365  EJECT                                                            EL539S
01366  1200-END-SORT-SECTION.                                           EL539S
01367                                                                   EL539S
01368  1300-DETAIL-TOTALS.                                              EL539S
PEMMOD
PEMMOD     IF LNCT  IS GREATER THAN  34                                 EL539S
PEMMOD        PERFORM 1700-PAGE-HEADING                                 EL539S
PEMMOD                                 THRU  1799-EXIT                  EL539S
PEMMOD     END-IF
PEMMOD
01369      MOVE 'N'                    TO  PRNTG-DTL.                   EL539S
01370      MOVE SPACE                  TO  DT-1.                        EL539S
01371      MOVE 'TOTALS'               TO  D1-EFF.                      EL539S
01372      MOVE TBL-LBEN (TBLX)        TO  D1-LBEN.                     EL539S
01373      MOVE TBL-DBEN (TBLX)        TO  D1-HBEN.                     EL539S
01374                                                                   EL539S
01375      ADD TBL-LPRM (TBLX)  TBL-LREF (TBLX)  GIVING  D1-LPRM.       EL539S
01376      ADD TBL-DPRM (TBLX)  TBL-DREF (TBLX)  GIVING  D1-HPRM.       EL539S
01377                                                                   EL539S
PEMMOD*    IF LN-OPT1  IS EQUAL TO  'Y'                                 EL539S
PEMMOD*       ADD TBL-LCOM (TBLX)  TBL-DCOM (TBLX)  GIVING  D1-COMM.    EL539S
01380                                                                   EL539S
01381      MOVE DT-1                   TO  P-DATA.                      EL539S
01382      MOVE ZERO                   TO  P-CTL.                       EL539S
01383                                                                   EL539S
01384      PERFORM 1600-PRINT-LINE  THRU  1699-EXIT.                    EL539S
01385                                                                   EL539S
PEMMOD     MOVE '     TOTAL ISSUED PREMIUM ' TO DT-1
PEMMOD     COMPUTE WS-WORK-AMT =
PEMMOD        TBL-LPRM (TBLX) + TBL-DPRM (TBLX)
PEMMOD     MOVE WS-WORK-AMT           TO D1-LBEN
PEMMOD     MOVE DT-1                  TO P-DATA
PEMMOD     MOVE '0'                   TO P-CTL
PEMMOD     PERFORM 1600-PRINT-LINE THRU 1699-EXIT
PEMMOD
PEMMOD     MOVE '     TOTAL CANCELLED PREMIUM ' TO DT-1
PEMMOD     COMPUTE WS-WORK-AMT =
PEMMOD        TBL-LREF (TBLX) + TBL-DREF (TBLX)
PEMMOD     MOVE WS-WORK-AMT           TO D1-LBEN
PEMMOD     MOVE DT-1                  TO P-DATA
PEMMOD     MOVE '0'                   TO P-CTL
PEMMOD     PERFORM 1600-PRINT-LINE THRU 1699-EXIT
PEMMOD
PEMMOD     MOVE '     NET PREMIUM FOR LOAN OFFICER ' TO DT-1
PEMMOD     COMPUTE WS-WORK-AMT =
PEMMOD        TBL-LPRM (TBLX) + TBL-DPRM (TBLX) +
PEMMOD        TBL-LREF (TBLX) + TBL-DREF (TBLX)
PEMMOD     MOVE WS-WORK-AMT           TO D1-LBEN
PEMMOD     MOVE DT-1                  TO P-DATA
PEMMOD     MOVE '0'                   TO P-CTL
PEMMOD     PERFORM 1600-PRINT-LINE THRU 1699-EXIT
PEMMOD
PEMMOD     .
01386  1399-EXIT.                                                       EL539S
01387      EXIT.                                                        EL539S
01388                                                                   EL539S
01389  1400-INITIALIZE-TABLE.                                           EL539S
01390      MOVE LOW-VALUE              TO  TBL-CODE (TBLX).             EL539S
PEMMOD     MOVE SPACES                 TO  TBL-LO-NAME (TBLX)
01391      MOVE ZEROS                  TO  TBL-LICT (TBLX)              EL539S
01392                                      TBL-LBEN (TBLX)              EL539S
01393                                      TBL-LPRM (TBLX)              EL539S
01394                                      TBL-LCCT (TBLX)              EL539S
01395                                      TBL-LREF (TBLX)              EL539S
01396                                      TBL-LCOM (TBLX)              EL539S
01397                                      TBL-LOCT (TBLX)              EL539S
01398                                      TBL-LVOL (TBLX)              EL539S
01399                                      TBL-DICT (TBLX)              EL539S
01400                                      TBL-DBEN (TBLX)              EL539S
01401                                      TBL-DPRM (TBLX)              EL539S
01402                                      TBL-DCCT (TBLX)              EL539S
01403                                      TBL-DREF (TBLX)              EL539S
01404                                      TBL-DCOM (TBLX).             EL539S
01405                                                                   EL539S
01406  1499-EXIT.                                                       EL539S
01407      EXIT.                                                        EL539S
01408                                                                   EL539S
01409  1500-RE-INITIALIZE.                                              EL539S
01410      ADD TBL-LICT (TBLX)         TO  TBL-LICT (201).              EL539S
01411      ADD TBL-LBEN (TBLX)         TO  TBL-LBEN (201).              EL539S
01412      ADD TBL-LPRM (TBLX)         TO  TBL-LPRM (201).              EL539S
01413      ADD TBL-LCCT (TBLX)         TO  TBL-LCCT (201).              EL539S
01414      ADD TBL-LREF (TBLX)         TO  TBL-LREF (201).              EL539S
01415      ADD TBL-LCOM (TBLX)         TO  TBL-LCOM (201).              EL539S
01416      ADD TBL-DICT (TBLX)         TO  TBL-DICT (201).              EL539S
01417      ADD TBL-DBEN (TBLX)         TO  TBL-DBEN (201).              EL539S
01418      ADD TBL-DPRM (TBLX)         TO  TBL-DPRM (201).              EL539S
01419      ADD TBL-DCCT (TBLX)         TO  TBL-DCCT (201).              EL539S
01420      ADD TBL-DREF (TBLX)         TO  TBL-DREF (201).              EL539S
01421      ADD TBL-DCOM (TBLX)         TO  TBL-DCOM (201).              EL539S
01422                                                                   EL539S
01423  1599-EXIT.                                                       EL539S
01424      EXIT.                                                        EL539S
01425  EJECT                                                            EL539S
01426  1600-PRINT-LINE SECTION.                                         EL539S
01427      MOVE P-CTL                  TO  X.                           EL539S
01428                                                                   EL539S
01429      COPY ELCPRT2X.                                               EL539S
01430                                                                   EL539S
01431  1699-EXIT.                                                       EL539S
01432      EXIT.                                                        EL539S
01433  EJECT                                                            EL539S
01434  1700-PAGE-HEADING SECTION.                                       EL539S
01435      ADD 1                       TO  PGCT.                        EL539S
01436                                                                   EL539S
01437      MOVE ZERO                   TO  LNCT.                        EL539S
01438      MOVE PGCT                   TO  HD3-PG.                      EL539S
01439      MOVE '1'                    TO  P-CTL.                          CL**3
01440      MOVE HD-1                   TO  P-DATA.                      EL539S
01441                                                                   EL539S
01442      PERFORM 1600-PRINT-LINE  THRU  1699-EXIT.                    EL539S
01443                                                                   EL539S
01444      MOVE SPACE                  TO  P-CTL.                          CL**3
01445      MOVE HD-2                   TO  P-DATA.                      EL539S
01446                                                                   EL539S
01447      PERFORM 1600-PRINT-LINE  THRU  1699-EXIT.                    EL539S
01448                                                                   EL539S
01449      MOVE SPACE                  TO  P-CTL.                          CL**3
01450      MOVE HD-3                   TO  P-DATA.                      EL539S
01451                                                                   EL539S
01452      PERFORM 1600-PRINT-LINE  THRU  1699-EXIT.                    EL539S
01453                                                                   EL539S
01454 *    MOVE ZERO                   TO  P-CTL.                       EL539S
01455 *    MOVE HD-4                   TO  P-DATA.                      EL539S
01456 *                                                                 EL539S
01457 *    PERFORM 1600-PRINT-LINE  THRU  1699-EXIT.                    EL539S
01458 *                                                                 EL539S
01459 *    MOVE HD-5                   TO  P-DATA.                      EL539S
01460 *    MOVE ZERO                   TO  P-CTL.                       EL539S
01461 *                                                                 EL539S
01462 *    PERFORM 1600-PRINT-LINE  THRU  1699-EXIT.                    EL539S
01463 *                                                                 EL539S
01464 *    IF PRNTG-SUMM  IS EQUAL TO  'Y'                              EL539S
01465 *       MOVE 'PRODUCTION SUMMARY '                                EL539S
01466 *                                TO  H6-DS                        EL539S
01467 *    ELSE                                                         EL539S
01464 *       IF PRNTG-SUMM  IS EQUAL TO  'F'                           EL539S
01465 *          MOVE '    FINAL TOTALS   '                             EL539S
01466 *                                TO  H6-DS                        EL539S
01467 *       ELSE                                                      EL539S
01468 *          MOVE 'PRODUCTION DETAIL  '                             EL539S
01469 *                                TO  H6-DS                        EL539S
PEMMOD*       END-IF
PEMMOD*    END-IF
01470 *                                                                 EL539S
01471 *    MOVE HD-6                   TO  P-DATA.                      EL539S
01472 *    MOVE ZERO                   TO  P-CTL.                       EL539S
01473 *                                                                 EL539S
01474 *    PERFORM 1600-PRINT-LINE  THRU  1699-EXIT.                    EL539S
01475 *                                                                 EL539S
01476 *    MOVE HD-7                   TO  P-DATA.                      EL539S
01477 *    MOVE SPACE                  TO  P-CTL.                          CL**3
01478 *                                                                 EL539S
01479 *    PERFORM 1600-PRINT-LINE  THRU  1699-EXIT.                    EL539S
01480 *                                                                 EL539S
01481 *    MOVE HD-8                   TO  P-DATA.                      EL539S
01482 *    MOVE SPACE                  TO  P-CTL.                          CL**3
01483 *                                                                 EL539S
01484 *    PERFORM 1600-PRINT-LINE  THRU  1699-EXIT.                    EL539S
01485 *                                                                 EL539S
01486 *    MOVE HD-9                   TO  P-DATA.                      EL539S
01487 *    MOVE ' '                    TO  P-CTL.                          CL**8
01488 *                                                                 EL539S
01489 *    PERFORM 1600-PRINT-LINE  THRU  1699-EXIT.                    EL539S
01490                                                                   EL539S
PEMMOD*    IF PRNTG-SUMM  IS EQUAL TO  'Y'                              EL539S
PEMMOD*        GO TO 1710-BY-SUM1AND2.                                  EL539S
01493                                                                   EL539S
01494      MOVE ZERO                   TO  P-CTL.                       EL539S
01495      MOVE HD-10                  TO  P-DATA.                      EL539S
01496                                                                   EL539S
01497      PERFORM 1600-PRINT-LINE  THRU  1699-EXIT.                    EL539S
01498                                                                   EL539S
PEMMOD*    IF LN-OPT1  IS EQUAL TO  'Y'                                 EL539S
PEMMOD*    MOVE 'COMMISSION'       TO  HD9-VAR                          EL539S
PEMMOD*    ELSE                                                         EL539S
PEMMOD     MOVE SPACE              TO  HD9-VAR.                         EL539S
01503                                                                   EL539S
01504      MOVE ZERO                   TO  P-CTL.                       EL539S
01505      MOVE HD-11                  TO  P-DATA.                      EL539S
01506                                                                   EL539S
01507      PERFORM 1600-PRINT-LINE  THRU  1699-EXIT.                    EL539S
01508                                                                   EL539S
01509      MOVE HD-12                  TO  P-DATA.                      EL539S
01510      MOVE SPACE                  TO  P-CTL.                          CL**3
01511                                                                   EL539S
01512      PERFORM 1600-PRINT-LINE  THRU  1699-EXIT.                    EL539S
01513                                                                   EL539S
01514      ADD 3                       TO  LNCT.                        EL539S
01515                                                                   EL539S
01516      GO TO 1799-EXIT.                                             EL539S
01517                                                                   EL539S
01518  1710-BY-SUM1AND2.                                                EL539S
01519      MOVE SUM-HD1                TO  P-DATA.                      EL539S
01520      MOVE ZERO                   TO  P-CTL.                       EL539S
01521                                                                   EL539S
01522      PERFORM 1600-PRINT-LINE  THRU  1699-EXIT.                    EL539S
01523                                                                   EL539S
01524      MOVE SUM-HD2                TO  P-DATA.                      EL539S
01525      MOVE SPACE                  TO  P-CTL.                          CL**3
01526                                                                   EL539S
01527      PERFORM 1600-PRINT-LINE  THRU  1699-EXIT.                    EL539S
01528                                                                   EL539S
01529      ADD 2                       TO  LNCT.                        EL539S
01530                                                                   EL539S
01531  1799-EXIT.                                                       EL539S
01532      EXIT.                                                        EL539S
01533  EJECT                                                            EL539S
01534  2000-MINORS SECTION.                                             EL539S
01535      IF PRNTG-DTL  IS EQUAL TO  'Y'                               EL539S
01536          PERFORM 1300-DETAIL-TOTALS  THRU  1399-EXIT.             EL539S
01537                                                                   EL539S
01538      IF HAVE-DATA  IS EQUAL TO  'N'                               EL539S
01539          MOVE CTLKEY             TO  PRVKEY                       EL539S
01540          GO TO 2199-MINORS-EX.                                    EL539S
01541                                                                   EL539S
01542      MOVE 80                     TO  LNCT.                        EL539S
01543      MOVE 'Y'                    TO  PRNTG-SUMM.                  EL539S
01544      MOVE 'N'                    TO  ANY-COMP.                    EL539S
01545                                                                   EL539S
01546 *    PERFORM 2100-MINOR-LOOP  THRU  2180-NON-UPD-LO               EL539S
01547 *        VARYING  AX  FROM  1  BY  1                              EL539S
01548 *            UNTIL  AX  IS GREATER THAN  TBLX.                    EL539S
01549                                                                   EL539S
01550 *    PERFORM 1500-RE-INITIALIZE  THRU  1599-EXIT                  EL539S
01551 *        VARYING  TBLX  FROM  1  BY  1                            EL539S
01552 *            UNTIL  TBLX  IS EQUAL TO  AX.                        EL539S
01553                                                                   EL539S
01554      PERFORM 1400-INITIALIZE-TABLE  THRU  1499-EXIT               EL539S
01555          VARYING  TBLX  FROM  1  BY  1                            EL539S
PEMMOD*        UNTIL  TBLX  IS EQUAL TO  AX.                            EL539S
PEMMOD         UNTIL  TBLX  > 201.                                      EL539S
01557                                                                   EL539S
01558      MOVE CTLKEY                 TO  PRVKEY.                      EL539S
01559      MOVE 201                    TO  AX.                          EL539S
01560      MOVE HIGH-VALUE             TO  TBL-CODE (201).              EL539S
01561                                                                   EL539S
01562 *    PERFORM 2100-MINOR-LOOP  THRU  2180-NON-UPD-LO.              EL539S
01563 *                                                                 EL539S
PEMMOD*    MOVE '     TOTAL ISSUED PREMIUM ' TO SUM-DT
PEMMOD*    COMPUTE WS-WORK-AMT =
PEMMOD*       TBL-LPRM (AX) + TBL-DPRM (AX)
PEMMOD*    MOVE WS-WORK-AMT           TO SD-LBEN
PEMMOD*    MOVE SUM-DT                TO P-DATA
PEMMOD*    MOVE '0'                   TO P-CTL
PEMMOD*    PERFORM 1600-PRINT-LINE THRU 1699-EXIT
PEMMOD*
PEMMOD*    MOVE '     TOTAL CANCELLED PREMIUM ' TO SUM-DT
PEMMOD*    COMPUTE WS-WORK-AMT =
PEMMOD*       TBL-LREF (AX) + TBL-DREF (AX)
PEMMOD*    MOVE WS-WORK-AMT           TO SD-LBEN
PEMMOD*    MOVE SUM-DT                TO P-DATA
PEMMOD*    MOVE '0'                   TO P-CTL
PEMMOD*    PERFORM 1600-PRINT-LINE THRU 1699-EXIT
PEMMOD*
PEMMOD*    MOVE '     NET PREMIUM FOR ACCOUNT      ' TO SUM-DT
PEMMOD*    COMPUTE WS-WORK-AMT =
PEMMOD*       TBL-LPRM (AX) + TBL-DPRM (AX) +
PEMMOD*       TBL-LREF (AX) + TBL-DREF (AX)
PEMMOD*    MOVE WS-WORK-AMT           TO SD-LBEN
PEMMOD*    MOVE SUM-DT                TO P-DATA
PEMMOD*    MOVE '0'                   TO P-CTL
PEMMOD*    PERFORM 1600-PRINT-LINE THRU 1699-EXIT
PEMMOD*
01564      MOVE 201                    TO  TBLX.                        EL539S
01565                                                                   EL539S
01566      PERFORM 1400-INITIALIZE-TABLE  THRU  1499-EXIT.              EL539S
01567                                                                   EL539S
01568      MOVE 'N'                    TO  PRNTG-SUMM.                  EL539S
01569                                                                   EL539S
PEMMOD     DIVIDE PGCT BY +2
PEMMOD          GIVING WK1
PEMMOD          REMAINDER WK2
PEMMOD     MOVE +0                     TO PGCT
PEMMOD
PEMMOD     IF WK2  NOT = +0
PEMMOD        MOVE SPACES              TO PRT
PEMMOD        MOVE '1'                 TO P-CTL
PEMMOD        PERFORM 1600-PRINT-LINE  THRU 1699-EXIT
PEMMOD     END-IF
PEMMOD
01570      GO TO 2199-MINORS-EX.                                        EL539S
01571                                                                   EL539S
01572  2100-MINOR-LOOP.                                                 EL539S
01573      MOVE TBL-CTL                TO  BUILD-LO-KEY.                EL539S
01574                                                                   EL539S
01575      IF DTE-COMP-VG  IS EQUAL TO  1                               EL539S
01576          GO TO 2105-LO-KEY-OK.                                    EL539S
01577                                                                   EL539S
01578      IF DTE-COMP-VG  IS EQUAL TO  3                               EL539S
01579          MOVE SPACES             TO  BLK-CARRIER                  EL539S
01580                                      BLK-GROUPING                 EL539S
01581                                      BLK-STATE                    EL539S
01582          GO TO 2105-LO-KEY-OK.                                    EL539S
01583                                                                   EL539S
01584      IF DTE-COMP-VG  IS EQUAL TO  2                               EL539S
01585          MOVE SPACES             TO  BLK-GROUPING                 EL539S
01586          GO TO 2105-LO-KEY-OK.                                    EL539S
01587                                                                   EL539S
01588      IF DTE-COMP-VG  IS EQUAL TO  4                               EL539S
01589          MOVE SPACES             TO  BLK-GROUPING                 EL539S
01590                                      BLK-STATE                    EL539S
01591          GO TO 2105-LO-KEY-OK.                                    EL539S
01592                                                                   EL539S
01593      IF DTE-COMP-VG  IS EQUAL TO  ' '                             EL539S
01594          MOVE SPACES             TO  BLK-CARRIER                  EL539S
01595                                      BLK-GROUPING                 EL539S
01596          GO TO 2105-LO-KEY-OK.                                    EL539S
01597                                                                   EL539S
01598  2105-LO-KEY-OK.                                                  EL539S
01599      MOVE BUILD-LO-KEY           TO  LO-CONTROL-PRIMARY.          EL539S
01600      MOVE 'Y'                    TO  LO-UPD.                      EL539S
01601                                                                   EL539S
01602      IF TBL-CODE (AX)  IS EQUAL TO  HIGH-VALUE                    EL539S
01603          MOVE 'N'                TO  LO-UPD                       EL539S
01604          GO TO 2110-BY-GETLO.                                     EL539S
01605                                                                   EL539S
01606      MOVE TBL-CODE (AX)          TO  LO-OFFICER-CODE.             EL539S
01607                                                                   EL539S
01608      READ ERLOFC  INVALID KEY                                     EL539S
01609          MOVE 'N'                TO  LO-UPD.                      EL539S
01610                                                                   EL539S
01611  2110-BY-GETLO.                                                   EL539S
01612      IF LNCT  IS GREATER THAN  36                                 EL539S
01613          PERFORM 1700-PAGE-HEADING  THRU  1799-EXIT.              EL539S
01614                                                                   EL539S
01615      MOVE SPACE                  TO  SUM-DT.                      EL539S
01616      MOVE TBL-CODE (AX)          TO  SD-CODE.                     EL539S
01617      MOVE '-'                    TO  SD-DASH.                     EL539S
PEMMOD     MOVE TBL-LO-NAME (AX)       TO  SD-NAME.
01618                                                                   EL539S
01619 *    IF LO-UPD  IS EQUAL TO  'N'                                  EL539S
01620 *        MOVE 'UNASSIGNED'       TO  SD-NAME                      EL539S
01621 *    ELSE                                                         EL539S
01622 *        MOVE LO-OFFICER-NAME    TO  SD-NAME.                     EL539S
01623                                                                   EL539S
01623                                                                   EL539S
01624      IF AX = 201                                                  EL539S
01625          MOVE 'TOTALS'           TO  SD-NAME                      EL539S
01626          IF DTE-CLIENT = 'UCL'                                    EL539S
01627              NEXT SENTENCE                                        EL539S
01628            ELSE                                                   EL539S
01629              GO TO 2110-CONTINUE                                  EL539S
01630        ELSE                                                       EL539S
01631          GO TO 2110-CONTINUE.                                     EL539S
01632                                                                   EL539S
01633      MOVE TBL-LOCT (AX)          TO  SD-LNCT.                     EL539S
01634      MOVE TBL-LVOL (AX)          TO  SD-LNAMT.                    EL539S
01635      COMPUTE SD-BPCT ROUNDED = TBL-LBEN (AX) * 100                EL539S
01636                              / TBL-LVOL (AX)                      EL539S
01637          ON SIZE ERROR                                            EL539S
01638              MOVE ZERO           TO  SD-BPCT.                     EL539S
01639                                                                   EL539S
01640      COMPUTE SD-LCCT ROUNDED = TBL-LICT (AX) * 100                EL539S
01641                              / TBL-LOCT (AX)                      EL539S
01642          ON SIZE ERROR                                            EL539S
01643              MOVE ZERO           TO  SD-LCCT.                     EL539S
01644                                                                   EL539S
01645      COMPUTE SD-DBPC ROUNDED = TBL-DBEN (AX) * 100                EL539S
01646                              / TBL-LVOL (AX)                      EL539S
01647          ON SIZE ERROR                                            EL539S
01648              MOVE ZERO           TO  SD-BPCT.                     EL539S
01649                                                                   EL539S
01650      COMPUTE SD-DCPC ROUNDED = TBL-DICT (AX) * 100                EL539S
01651                              / TBL-LOCT (AX)                      EL539S
01652          ON SIZE ERROR                                            EL539S
01653              MOVE ZERO           TO  SD-DCPC.                     EL539S
01654                                                                   EL539S
01655  2110-CONTINUE.                                                   EL539S
01656      IF LO-UPD  IS EQUAL TO  'N'                                  EL539S
01657          NEXT SENTENCE                                            EL539S
01658      ELSE                                                         EL539S
01659          MOVE LO-LOAN-COUNT (RUN-MO)                              EL539S
01660                                  TO  SD-LNCT                      EL539S
01661          MOVE LO-LOAN-VOLUME (RUN-MO)                             EL539S
01662                                  TO  SD-LNAMT.                    EL539S
01663                                                                   EL539S
01664      MOVE TBL-LICT (AX)          TO  SD-LICT.                     EL539S
01665      MOVE TBL-LBEN (AX)          TO  SD-LBEN.                     EL539S
01666      MOVE TBL-LPRM (AX)          TO  SD-LPRM.                     EL539S
01667      MOVE TBL-DICT (AX)          TO  SD-DICT.                     EL539S
01668      MOVE TBL-DBEN (AX)          TO  SD-DBEN.                     EL539S
01669      MOVE TBL-DPRM (AX)          TO  SD-DPRM.                     EL539S
01670                                                                   EL539S
01671      IF LO-UPD  IS EQUAL TO  'N'                                  EL539S
01672          GO TO 2130-BYCOMP.                                       EL539S
01673                                                                   EL539S
01674      COMPUTE SD-LCCT ROUNDED = TBL-LICT (AX) * 100                EL539S
01675                              / LO-LOAN-COUNT (RUN-MO)             EL539S
01676          ON SIZE ERROR                                            EL539S
01677              MOVE ZERO           TO  SD-LCCT.                     EL539S
01678                                                                   EL539S
01679      COMPUTE SD-BPCT ROUNDED = TBL-LBEN (AX) * 100                EL539S
01680                              / LO-LOAN-VOLUME (RUN-MO)            EL539S
01681          ON SIZE ERROR                                            EL539S
01682              MOVE ZERO           TO  SD-BPCT.                     EL539S
01683                                                                   EL539S
01684      COMPUTE SD-DCPC ROUNDED = TBL-DICT (AX) * 100                EL539S
01685                              / LO-LOAN-COUNT (RUN-MO)             EL539S
01686          ON SIZE ERROR                                            EL539S
01687              MOVE ZERO           TO  SD-DCPC.                     EL539S
01688                                                                   EL539S
01689      COMPUTE SD-DBPC ROUNDED = TBL-DBEN (AX) * 100                EL539S
01690                              / LO-LOAN-VOLUME (RUN-MO)            EL539S
01691          ON SIZE ERROR                                            EL539S
01692              MOVE ZERO           TO  SD-BPCT.                     EL539S
01693                                                                   EL539S
01694  2130-BYCOMP.                                                     EL539S
01695      MOVE SUM-DT                 TO  P-DATA.                      EL539S
01696      MOVE '0'                    TO  P-CTL.                          CL**3
01697                                                                   EL539S
01698      PERFORM 1600-PRINT-LINE  THRU  1699-EXIT.                    EL539S
01699                                                                   EL539S
01700      ADD 2                       TO  LNCT.                        EL539S
01701                                                                   EL539S
01702      IF TBL-LCCT (AX)  IS EQUAL TO  ZERO                          EL539S
01703        AND TBL-LREF (AX) IS EQUAL TO  ZERO                        EL539S
01704        AND TBL-DCCT (AX) IS EQUAL TO  ZERO                        EL539S
01705        AND TBL-DREF (AX) IS EQUAL TO  ZERO                        EL539S
01706          GO TO 2150-BY-REFUNDS.                                   EL539S
01707                                                                   EL539S
01708      MOVE SPACE                  TO  SUM-DT.                      EL539S
01709      MOVE 'CANCELLATIONS'        TO  SD-NAME.                     EL539S
01710      MOVE TBL-LCCT (AX)          TO  SD-LICT.                     EL539S
01711      MOVE TBL-LREF (AX)          TO  SD-LPRM.                     EL539S
01712      MOVE TBL-DCCT (AX)          TO  SD-DICT.                     EL539S
01713      MOVE TBL-DREF (AX)          TO  SD-DPRM.                     EL539S
01714      MOVE SUM-DT                 TO  P-DATA.                      EL539S
01715      MOVE SPACE                  TO  P-CTL.                          CL**3
01716                                                                   EL539S
01717      PERFORM 1600-PRINT-LINE  THRU  1699-EXIT.                    EL539S
01718                                                                   EL539S
01719      ADD 1                       TO  LNCT.                        EL539S
01720                                                                   EL539S
01721  2150-BY-REFUNDS.                                                 EL539S
01722      IF AX  IS EQUAL TO  201                                      EL539S
01723        AND ANY-COMP  IS EQUAL TO  'Y'                             EL539S
01724          NEXT SENTENCE                                            EL539S
01725      ELSE                                                         EL539S
01726          IF LO-UPD  IS EQUAL TO  'Y'                              EL539S
01727            AND LO-SUPPRESS-COMP                                   EL539S
01728              GO TO 2170-BY-COMP.                                  EL539S
01729                                                                   EL539S
01730      IF AX  IS EQUAL TO  201                                      EL539S
01731        AND ANY-COMP  IS EQUAL TO  'N'                             EL539S
01732          GO TO 2170-BY-COMP.                                      EL539S
01733                                                                   EL539S
01734      MOVE SPACE                  TO  SUM-DT.                      EL539S
01735      MOVE 'COMPENSATION'         TO  SD-NAME.                     EL539S
01736      MOVE TBL-LCOM (AX)          TO  SD-LPRM.                     EL539S
01737      MOVE TBL-DCOM (AX)          TO  SD-DPRM.                     EL539S
01738      MOVE SUM-DT                 TO  P-DATA.                      EL539S
01739      MOVE SPACE                  TO  P-CTL.                          CL**3
01740                                                                   EL539S
01741      PERFORM 1600-PRINT-LINE  THRU  1699-EXIT.                    EL539S
01742                                                                   EL539S
01743      ADD 1                       TO  LNCT.                        EL539S
01744                                                                   EL539S
01745      MOVE 'Y'                    TO  ANY-COMP.                    EL539S
01746                                                                   EL539S
01747  2170-BY-COMP.                                                    EL539S
01748      IF LO-UPD  IS EQUAL TO  'N'                                  EL539S
01749        OR TBL-CODE (AX)  IS EQUAL TO  HIGH-VALUE                  EL539S
01750          GO TO 2180-NON-UPD-LO.                                   EL539S
01751                                                                   EL539S
01752      MOVE TBL-LICT (AX)          TO  LO-LF-COUNT (RUN-MO).        EL539S
01753                                                                   EL539S
01754      ADD .5  TBL-LPRM (AX)  GIVING  LO-LF-PREM (RUN-MO).          EL539S
01755      ADD .5  TBL-LBEN (AX)  GIVING  LO-LF-BENEFIT (RUN-MO).       EL539S
01756                                                                   EL539S
01757      MOVE TBL-DICT (AX)          TO  LO-AH-COUNT (RUN-MO).        EL539S
01758                                                                   EL539S
01759      ADD .5  TBL-DPRM (AX)  GIVING  LO-AH-PREM (RUN-MO).          EL539S
01760      ADD .5  TBL-DBEN (AX)  GIVING  LO-AH-BENEFIT (RUN-MO).       EL539S
01761                                                                   EL539S
01762      IF DTE-CLIENT = 'UCL'                                        EL539S
01763          ADD TBL-LCOM (AX) TBL-DCOM (AX)  GIVING TOT-COMM         EL539S
01764          MOVE EP-MO              TO  HOLD-PROCESS-MO              EL539S
01765          MOVE EP-YR              TO  HOLD-PROCESS-YR              EL539S
01766          MOVE EP-CC              TO  HOLD-PROCESS-CC              EL539S
01767          MOVE TOT-COMM           TO  LO-TOTAL-COMMISSION          EL539S
01768          MOVE HOLD-PROCESS-MO-YR TO  LO-PROCESS-MO-YR.            EL539S
01769                                                                   EL539S
01770      REWRITE LOAN-OFFICER-MASTER.                                 EL539S
01771                                                                   EL539S
01772  2180-NON-UPD-LO.                                                 EL539S
01773      EXIT.                                                        EL539S
01774                                                                   EL539S
01775  2199-MINORS-EX.                                                  EL539S
01776      EXIT.                                                        EL539S
PEMMOD 3000-FINAL-TOTALS SECTION.
PEMMOD
PEMMOD     MOVE SPACES                 TO HD-5
                                          HD-7
                                          HD-8
                                          HD-9
                                          HD-10
                                          H6-NA

PEMMOD     MOVE 'F'                    TO  PRNTG-SUMM                   EL539S
PEMMOD     PERFORM 1700-PAGE-HEADING   THRU  1799-EXIT                  EL539S
PEMMOD
01369      MOVE 'N'                    TO  PRNTG-DTL                    EL539S
01370      MOVE SPACE                  TO  DT-1                         EL539S
01371      MOVE 'GRAND '               TO  D1-EFF                       EL539S
01372      MOVE FIN-LBEN               TO  D1-LBEN                      EL539S
01373      MOVE FIN-DBEN               TO  D1-HBEN                      EL539S
01374                                                                   EL539S
01375      ADD FIN-LPRM  FIN-LREF   GIVING  D1-LPRM                     EL539S
01376      ADD FIN-DPRM  FIN-DREF   GIVING  D1-HPRM                     EL539S
01377                                                                   EL539S
PEMMOD*    IF LN-OPT1  IS EQUAL TO  'Y'                                 EL539S
PEMMOD*       ADD FIN-LCOM  FIN-DCOM  GIVING  D1-COMM                   EL539S
PEMMOD*    END-IF
01380                                                                   EL539S
01381      MOVE DT-1                   TO  P-DATA                       EL539S
01382      MOVE ZERO                   TO  P-CTL                        EL539S
01383                                                                   EL539S
01384      PERFORM 1600-PRINT-LINE     THRU  1699-EXIT                  EL539S
01385                                                                   EL539S
PEMMOD     MOVE ' GRAND TOT ISSUED PREMIUM ' TO DT-1
PEMMOD     COMPUTE WS-WORK-AMT =
PEMMOD        FIN-LPRM  + FIN-DPRM
PEMMOD     MOVE WS-WORK-AMT            TO D1-LBEN
PEMMOD     MOVE DT-1                   TO P-DATA
PEMMOD     MOVE '0'                    TO P-CTL
PEMMOD     PERFORM 1600-PRINT-LINE     THRU 1699-EXIT
PEMMOD
PEMMOD     MOVE ' GRAND TOT CANCELLED PREMIUM ' TO DT-1
PEMMOD     COMPUTE WS-WORK-AMT =
PEMMOD        FIN-LREF  + FIN-DREF
PEMMOD     MOVE WS-WORK-AMT            TO D1-LBEN
PEMMOD     MOVE DT-1                   TO P-DATA
PEMMOD     MOVE '0'                    TO P-CTL
PEMMOD     PERFORM 1600-PRINT-LINE     THRU 1699-EXIT
PEMMOD
PEMMOD     MOVE ' GRAND TOTAL NET PREMIUM          ' TO DT-1
PEMMOD     COMPUTE WS-WORK-AMT =
PEMMOD        FIN-LPRM  + FIN-DPRM  +
PEMMOD        FIN-LREF  + FIN-DREF
PEMMOD     MOVE WS-WORK-AMT            TO D1-LBEN
PEMMOD     MOVE DT-1                   TO P-DATA
PEMMOD     MOVE '0'                    TO P-CTL
PEMMOD     PERFORM 1600-PRINT-LINE THRU 1699-EXIT

092905     MOVE DISC-LINE-1            TO P-DATA
092905     MOVE '0'                    TO P-CTL
092905     PERFORM 1600-PRINT-LINE     THRU 1699-EXIT

092905     MOVE DISC-LINE-2            TO P-DATA
092905     MOVE ' '                    TO P-CTL
092905     PERFORM 1600-PRINT-LINE     THRU 1699-EXIT

092905     MOVE DISC-LINE-3            TO P-DATA
092905     MOVE ' '                    TO P-CTL
092905     PERFORM 1600-PRINT-LINE     THRU 1699-EXIT

PEMMOD
PEMMOD     .
PEMMOD 3000-EXIT.
PEMMOD     EXIT.
PEMMOD
01777  EJECT                                                            EL539S
01778  ABEND-PGM SECTION.                                               EL539S
01779      COPY ELCABEND.                                               EL539S
01780                                                                   EL539S
01781  9000-EOJ SECTION.                                                EL539S
01782      COPY ELCPRTCX.                                               EL539S
01783                                                                   EL539S
01784      GOBACK.                                                      EL539S

