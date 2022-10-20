00001  IDENTIFICATION DIVISION.                                         06/04/98
00002                                                                   ECS017
00003  PROGRAM-ID.          ECS017.                                        LV016
00004 *              PROGRAM CONVERTED BY                               ECS017
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ECS017
00006 *              CONVERSION DATE 07/08/94 13:48:34.                 ECS017
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          ECS017
00008 *                     VMOD=2.030                                     CL**8
00009                                                                   ECS017
00010 *AUTHOR.        LOGIC, INC.                                       ECS017
00011 *               DALLAS, TEXAS.                                    ECS017
00012 *DATE-COMPILED.                                                   ECS017
00013                                                                   ECS017
00014 *SECURITY.   *****************************************************ECS017
00015 *            *                                                   *ECS017
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ECS017
00017 *            *                                                   *ECS017
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS017
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ECS017
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *ECS017
00021 *            *                                                   *ECS017
00022 *            *****************************************************ECS017
00023 *REMARKS.                                                         ECS017
00024 *        CREATE STANDARD COMMISSION TRANSACTIONS                  ECS017
00025 *                                                                 ECS017
00026 *    OPTION      PASS CLAIMS - PASS COMPENSATION - PASS OVERWRITE ECS017
00027 *       1           YES      -       YES         -       YES      ECS017
00028 *       2           YES      -       YES         -        NO      ECS017
00029 *       3           YES      -        NO         -       YES      ECS017
00030 *       4           YES      -        NO         -        NO      ECS017
00031 *       5            NO      -       YES         -       YES      ECS017
00032 *       6            NO      -       YES         -        NO      ECS017
00033 *       7            NO      -        NO         -       YES      ECS017
00034 *       8            NO      -        NO         -        NO      ECS017
122002******************************************************************
122002*                   C H A N G E   L O G
122002*
122002* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
122002*-----------------------------------------------------------------
122002*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
122002* EFFECTIVE    NUMBER
122002*-----------------------------------------------------------------
122002* 122002                   PEMA  ADD DCC PROCESSING            
070803* 070803                   SMVA  FIX Z TRANS PICKING UP WRONG 
070803*                                ENTRY STATUS
100703* 100703    2003080800002  PEMA  ADD SUPER GAP PROCESSING
040504* 040504  CR2003080800002  PEMA  ADD DEALER INCENTIVE PROCESSING
020305* 020305    2005020000000  PEMA  ADD CLP STATE PROCESSING
092705* 092705  CR2005050300006  PEMA  ADD SPP LEASES
052306* 052306  CR2006050800002  PEMA  ADD COMM TYPE 'J'
042209* 042209  CR2009031600001  AJRA  CHG COMM TYPE 'I' AND 'J' STD ACCUM
101409* 101409  IR2009100200001  PEMA  ADD ROUNDED CLAUSE
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
091911* 091911  IR2011090200003  PEMA  FIX LF PREM PROB WITH SPP
112911* 112911  CR2011083000003  PEMA  ADD SPPDDF TRUNCATED
072512* 072512  CR2012060400001  PEMA  SPP CORRECTIONS
100413* 100413  IR2013100100001  PEMA  INCREASE PDEF OCCURS
041414* 041414  IR2014040100003  PEMA  ACCOUNT FOR NO CHGBACK
051414* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
052814* 052814  CR2014012300001  PEMA  DCC CREDIT UNION CHANGES
022615* 022615  IR2015022600001  PEMA  ALLOW LF CLM PMTS FOR CARR 7
010716* 010716  CR2015082500001  PEMA  VPP CHANGES
111116* 111116  IR2016110700002  PEMA  force calc of ahcomm when lf cov
010617* 010617  IR2017010400001  PEMA  ALLOW LF AMTS FOR CU BENCD
032922* 032922  CR2021100800003  PEMA  Increase number of Prod Defs to 11
122002******************************************************************
00035                                                                   ECS017
00036  ENVIRONMENT DIVISION.                                            ECS017
00037  INPUT-OUTPUT SECTION.                                            ECS017
00038  FILE-CONTROL.                                                    ECS017
00039                                                                   ECS017
00040      SELECT RECALC-EXTR      ASSIGN TO
      *       '/data/test/seqfiles/DC.WW.COMRCALC'.
              SYS004.
00041      SELECT END-PRINT        ASSIGN TO
      *       '/data/test/seqfiles/ECS017_SYS008'.
              SYS008.
00042      SELECT COMM-TRANS       ASSIGN TO
      *       '/data/test/seqfiles/ZI.XX.CTRN'.
              SYS013.
00043      SELECT EXTR-DISK        ASSIGN TO
      *       '/data/test/seqfiles/DC.WW.PRCMEXTR'.
              SYS014.
00044      SELECT EP-EXTR          ASSIGN TO
      *       '/data/test/seqfiles/DC.DD.DET010.TEMP'.
              SYS018.
00045      SELECT ERACCTT          ASSIGN TO
      *       '/data/test/seqfiles/ERACCTT.DAT'
              ERACCTT
00046                              ORGANIZATION IS INDEXED              ECS017
00047                              ACCESS IS SEQUENTIAL                 ECS017
00048                              RECORD KEY IS AM-CONTROL-PRIMARY     ECS017
00049                              FILE STATUS IS ERACCTT-FILE-STATUS.  ECS017

041105     SELECT ERCOMP           ASSIGN TO ERCOMP
00046                              ORGANIZATION IS INDEXED
00047                              ACCESS IS DYNAMIC
00048                              RECORD KEY IS CO-CONTROL-PRIMARY
00049                              FILE STATUS IS ERCOMP-FILE-STATUS.

           SELECT ERPDEF           ASSIGN TO ERPDEF
                                   ACCESS IS DYNAMIC                    
                                   ORGANIZATION IS INDEXED              
                                   FILE STATUS IS ERPDEF-FILE-STATUS    
                                   RECORD KEY IS PD-CONTROL-PRIMARY.    

00050      SELECT DISK-DATE        ASSIGN TO
      *       '/data/test/seqfiles/DC.DD.ER.DATECARD'.
              SYS019.
00051      SELECT FICH             ASSIGN TO
      *       '/data/test/seqfiles/DC.EX.FICH020'.
              SYS020.
00052  EJECT                                                            ECS017
00053  DATA DIVISION.                                                   ECS017
00054  FILE SECTION.                                                    ECS017
00055                                                                   ECS017
00056  FD  RECALC-EXTR                                                  ECS017
00057      BLOCK CONTAINS 0 RECORDS
00058      RECORDING MODE F.                                            ECS017
011410 01  RECALC-RECORD           PIC X(165).
00060                                                                   ECS017
00061  FD  END-PRINT                                                    ECS017
00062                              COPY ELCPRTFD.                       ECS017
00063  EJECT                                                            ECS017
00064  FD  COMM-TRANS                                                   ECS017
00065                              COPY ECSCOMFD.                       ECS017
00066                                                                   ECS017
00067                              COPY ECSCOM01.                       ECS017
00068  EJECT                                                            ECS017
00069  FD  EXTR-DISK                                                    ECS017
00070      BLOCK CONTAINS 0 RECORDS
00071      RECORDING MODE F.                                            ECS017
011410 01  EXTR-RECORD             PIC X(165).
00073                                                                   ECS017
00074  FD  ERACCTT.                                                     ECS017
00075                                                                   ECS017
00076                              COPY ERCACCT.                        ECS017
00077  EJECT                                                            ECS017
       FD  ERCOMP.

                                   COPY ERCCOMP.

00078  FD  EP-EXTR                                                      ECS017
00079                              COPY ECSEXTFD.                       ECS017
00080                                                                   ECS017
00081                              COPY ECSEXT01.                       ECS017

       FD  ERPDEF.

                                   COPY ERCPDEF.

00083  FD  DISK-DATE                                                    ECS017
00084                              COPY ELCDTEFD.                       ECS017
00085                                                                   ECS017
00086  FD  FICH                                                         ECS017
00087                              COPY ELCFCHFD.                       ECS017
00088  EJECT                                                            ECS017
00089  WORKING-STORAGE SECTION.                                         ECS017
00090  77  FILLER PIC X(32) VALUE '********************************'.   ECS017
00091  77  FILLER PIC X(32) VALUE '*   ECS017 WORKING STORAGE     *'.   ECS017
00092  77  FILLER PIC X(32) VALUE '********** VMOD=2.030 **********'.      CL**8
00093                                                                   ECS017
00094  77  ACCT-SW                 PIC S9       COMP-3 VALUE +0.        ECS017
00095  77  NDX                     PIC S999     COMP-3 VALUE +0.        ECS017
040504 77  WS-SAVE-NDX             PIC S999     COMP-3 VALUE +0.
00096  77  RV-NDX                  PIC S999     COMP-3 VALUE +0.        ECS017
00097  77  PGM-SUB                 PIC S999     COMP-3 VALUE +017.      ECS017
00098  77  PRCM-LF-OW-COMM         PIC S9(7)V99 COMP-3 VALUE +0.        ECS017
00099  77  PRCM-LF-OW-SF           PIC S9(7)V99 COMP-3 VALUE +0.        ECS017
00100  77  PRCM-AH-OW-COMM         PIC S9(7)V99 COMP-3 VALUE +0.        ECS017
00101  77  PRCM-AH-OW-SF           PIC S9(7)V99 COMP-3 VALUE +0.        ECS017
040504 77  PRCM-DLR-INC            PIC S9(7)V99 COMP-3 VALUE +0.
011410 77  PRCM-LF-LMBA-FEE        PIC S9(7)V99 COMP-3 VALUE +0.
011410 77  PRCM-AH-LMBA-FEE        PIC S9(7)V99 COMP-3 VALUE +0.
       77  PRCM-BANK-FEE           PIC S9(7)V99 COMP-3 VALUE +0.
       77  PRCM-CSO-ADMIN          PIC S9(7)V99 COMP-3 VALUE +0.
00102  77  RECALC-LF-OW-COMM       PIC S9(7)V99 COMP-3 VALUE +0.        ECS017
00103  77  RECALC-LF-OW-SF         PIC S9(7)V99 COMP-3 VALUE +0.        ECS017
00104  77  RECALC-AH-OW-COMM       PIC S9(7)V99 COMP-3 VALUE +0.        ECS017
00105  77  RECALC-AH-OW-SF         PIC S9(7)V99 COMP-3 VALUE +0.        ECS017
100703 77  CNC-FACT                PIC S9(3)V9(7) COMP-3 VALUE +0.
011410 77  WS-WORK-REF             PIC S9(9)V99 COMP-3 VALUE +0.
00106  77  X                       PIC X               VALUE SPACE.        CL**3
       77  S1                      PIC S999 COMP-3 VALUE +0.

011410 77  WS-DUP-AGT-SW           PIC X  VALUE ' '.
011410     88  DUP-AGENT                VALUE 'Y'.
       77  WS-DONE-SW              PIC X  VALUE ' '.
           88  ALREADY-DONE             VALUE 'Y'.
       77  B-SUB                   PIC S9(4)   COMP.
100413 77  D1                      PIC S9(5) COMP-3 VALUE +0.
       77  P1                      PIC S999 COMP-3 VALUE +0.
       77  P2                      PIC S999 COMP-3 VALUE +0.
       77  CNC-WK                  PIC S9(7)V99 COMP-3 VALUE +0.
       77  WS-FACT-CERT            PIC X(11)   VALUE SPACES.
       77  WS-MONTH                PIC S999     COMP-3 VALUE +0.
       77  WS-HI-UEF               PIC S9V999   COMP-3 VALUE +0.
       77  WS-LO-UEF               PIC S9V999   COMP-3 VALUE +0.
       77  WS-CLP-MO3              PIC S9(7)V99 COMP-3 VALUE +0.
       77  WS-COMM-MO3             PIC S9(7)V99 COMP-3 VALUE +0.
       77  DD-IU-SW                    PIC X   VALUE ' '.
           88  DD-IU-PRESENT                 VALUE 'Y'.
       77  W-FACTOR            PIC S9(09)V9(08) COMP-3 VALUE +0.
       77  WS-DDF-TERM             PIC S999     COMP-3 VALUE +0.
00107                                                                   ECS017
00108  01  BINARY-WORK-AREA    COMP    SYNC.                            ECS017
00109      12  X1                  PIC S999            VALUE +0.        ECS017
00110      12  X4                  PIC S999            VALUE +0.        ECS017
00111      12  B1                  PIC S999            VALUE +1.        ECS017
00112      12  B2                  PIC S999            VALUE +2.        ECS017
00113      12  FAL                 PIC S999            VALUE +0.        ECS017
00114      12  AGTNDX              PIC S999            VALUE +0.        ECS017
00115                                                                   ECS017
00116  01  WS.                                                          ECS017
00117      12  WS-RETURN-CODE         PIC S9(4) COMP.                   ECS017
00118      12  WS-ABEND-MESSAGE       PIC X(80).                        ECS017
00119      12  WS-ABEND-FILE-STATUS   PIC XX  VALUE ZEROS.              ECS017
00120      12  WS-ZERO                PIC S9  VALUE ZERO COMP-3.        ECS017
00121      12  WS-DTE-PGM-OPT         PIC 9   VALUE 0.                     CL*15
00122      12  ERACCTT-FILE-STATUS    PIC XX  VALUE ZEROS.              ECS017
041105     12  ERCOMP-FILE-STATUS     PIC XX  VALUE '00'.
           12  ERPDEF-FILE-STATUS     PIC XX VALUE ZEROS.
           12  B-CNT                  PIC S999 VALUE +0 COMP-3.
00123                                                                   ECS017
00124  01  WS-RUN-DATE-R.                                               ECS017
00125      12  WS-RUN-YR              PIC 99.                           ECS017
00126      12  WS-RUN-MO              PIC 99.                           ECS017
00127      12  WS-RUN-DA              PIC 99.                           ECS017
00128                                                                   ECS017
00129  01  ERROR-MESSAGES.                                              ECS017
00130      12  ER-0504             PIC X(4)            VALUE '0504'.    ECS017
00131      12  ER-0514             PIC X(4)            VALUE '0514'.    ECS017
00132                                                                   ECS017
041105 01  WS-ERCOMP-KEY.
           12  WS-ERCOMP-COMP-CD   PIC X.
           12  WS-ERCOMP-CARRIER   PIC X.
           12  WS-ERCOMP-GROUPING  PIC X(6).
           12  WS-ERCOMP-FIN-RESP  PIC X(10).
           12  WS-ERCOMP-ACCOUNT   PIC X(10).
           12  WS-ERCOMP-TYPE      PIC X.
           
00133  01  MISC-WORK-AREA.                                              ECS017
00134      12  STRIP-SIGN          PIC 9V9(5).                          ECS017
00135      12  OTHER-WORK.                                              ECS017
00136          16  WK-CTL.                                              ECS017
00137              20  W-CARRIER   PIC X.                               ECS017
00138              20  W-GROUPING  PIC X(6).                            ECS017
00139              20  W-ST        PIC XX.                              ECS017
00140              20  W-ACCT      PIC X(10).                           ECS017
00141              20  W-DATE      PIC 9(11)    COMP-3.                    CL**7
00142          16  SV-ST           PIC XX.                              ECS017
00143                                                                   ECS017
00144      12  MONTHS-DIFF-LF      PIC S9(5).                           ECS017
00145      12  MONTHS-DIFF-AH      PIC S9(5).                           ECS017
00146                                                                   ECS017
00147      12  SAVE-NCL-REGION     PIC X(6).                            ECS017
00148      12  SAVE-NCL-POOL-CODE  PIC XXX.                             ECS017
00149                                                                   ECS017
00150      12  DMD-CERT-NUMBER.                                         ECS017
00151          16  DMD-RESIDENT-STATE PIC XX.                           ECS017
00152          16  DMD-SYSTEM-ID      PIC X.                            ECS017
00153          16  DMD-COV-CATEGORY   PIC X.                            ECS017
00154          16  DMD-BENEFIT-CODE   PIC XX.                           ECS017
00155          16  DMD-COVERAGE-RULE  PIC X.                            ECS017
00156          16  DMD-SEQ-NUMBER     PIC X(4).                         ECS017
00157                                                                   ECS017
00158  01  COMP-3-WORK-AREA    COMP-3.                                  ECS017
00159      12  KX                  PIC S9.                              ECS017
00160      12  L-OW                PIC S9(9)V99.                        ECS017
00161      12  L-OW-ALT            PIC S9(9)V99.                        ECS017
00162      12  A-OW                PIC S9(7)V99.                        ECS017
00163      12  TEST-ZERO.                                               ECS017
00164          16  TEST-IS-ZERO-1.                                      ECS017
00165              20  FILLER      PIC S9(9)V99    VALUE ZERO.          ECS017
00166              20  FILLER      PIC S9(7)V99    VALUE ZERO.          ECS017
00167              20  FILLER      PIC S9(7)V99    VALUE ZERO.          ECS017
00168              20  FILLER      PIC S9(9)V99    VALUE ZERO.          ECS017
00169              20  FILLER      PIC S9(7)V99    VALUE ZERO.          ECS017
00170              20  FILLER      PIC S9(7)V99    VALUE ZERO.
011410             20  FILLER      PIC S9          VALUE ZERO.
00171          16  TEST-IS-ZERO-2.                                      ECS017
00172              20  FILLER      PIC S9(7)V99    VALUE ZERO.          ECS017
00173              20  FILLER      PIC S9(7)V99    VALUE ZERO.          ECS017
00174              20  FILLER      PIC S9(7)V99    VALUE ZERO.          ECS017
00175      12  OW-TOTALS       OCCURS 20.                               ECS017
00176          16  OW-LIFE.                                             ECS017
00177              20  OW-LF-AMT           PIC S9(9)V99.                ECS017
00178              20  OW-LF-PRM           PIC S9(7)V99.                ECS017
00179              20  OW-LF-COM           PIC S9(7)V99.                ECS017
00180              20  OW-LF-AMT-ALT       PIC S9(9)V99.                ECS017
00181              20  OW-LF-PRM-ALT       PIC S9(7)V99.                ECS017
00182              20  OW-LF-COM-ALT       PIC S9(7)V99.
011410             20  OW-LF-CNT           PIC S9.
00183          16  OW-AH.                                               ECS017
00184              20  OW-AH-AMT           PIC S9(7)V99.                ECS017
00185              20  OW-AH-PRM           PIC S9(7)V99.                ECS017
00186              20  OW-AH-COM           PIC S9(7)V99.                ECS017
011904             20  OW-AH-RFND          PIC S9(7)V99.
011410             20  OW-AH-CNT           PIC S9.
00187          16  OW-LIFE-RC.                                          ECS017
00188              20  OW-LF-AMT-RC        PIC S9(9)V99.                ECS017
00189              20  OW-LF-PRM-RC        PIC S9(7)V99.                ECS017
00190              20  OW-LF-COM-RC        PIC S9(7)V99.                ECS017
00191              20  OW-LF-AMT-RC-ALT    PIC S9(9)V99.                ECS017
00192              20  OW-LF-PRM-RC-ALT    PIC S9(7)V99.                ECS017
00193              20  OW-LF-COM-RC-ALT    PIC S9(7)V99.                ECS017
00194          16  OW-AH-RC.                                            ECS017
00195              20  OW-AH-AMT-RC        PIC S9(7)V99.                ECS017
00196              20  OW-AH-PRM-RC        PIC S9(7)V99.                ECS017
00197              20  OW-AH-COM-RC        PIC S9(7)V99.                ECS017
00198          16  OW-LIFE-BILLED.                                      ECS017
00199              20  OW-LF-PRM-B         PIC S9(9)V99.                ECS017
00200              20  OW-LF-COM-B         PIC S9(7)V99.                ECS017
00201              20  OW-LF-PRM-B-ALT     PIC S9(9)V99.                ECS017
00202              20  OW-LF-COM-B-ALT     PIC S9(7)V99.                ECS017
00203          16  OW-AH-BILLED.                                        ECS017
00204              20  OW-AH-PRM-B         PIC S9(7)V99.                ECS017
00205              20  OW-AH-COM-B         PIC S9(7)V99.                ECS017
00206                                                                   ECS017
00207  01  X-AGENTS.                                                    ECS017
00208      12  X-AGTS          OCCURS 20.                               ECS017
00209          16 TBL-AGT          PIC X(10).                           ECS017
00210          16 TBL-COM-TYP      PIC X.                               ECS017
00211          16 TBL-GL-CODE      PIC X.                               ECS017
00212          16 TBL-ASSIGN-AGT   PIC X(10).                           ECS017
041105         16 TBL-BK-NAME      PIC X(30).
011410         16 TBL-DE-CONTROL   PIC X(36).
00213                                                                   ECS017
00214  01  WS-ASSIGN-TABLE.                                             ECS017
00215      12  WS-ASSIGN-ENTRY OCCURS 6 TIMES.                          ECS017
00216          16 WS-ASSIGN-AGT    PIC X(10).                           ECS017
00217                                                                   ECS017
00218  01  SAVE-OW                 PIC X(270).                          ECS017
00219  01  SAVE-OW-CPTR            PIC X(270).                          ECS017
00220  01  SAVE-CPTR               PIC X(270).                          ECS017
00221                                                                   ECS017
00222  01  P-LINE.                                                      ECS017
00223      12  FILLER      PIC X(24)   VALUE 'COMMISSION FILE HAS BEEN'.ECS017
00224      12  FILLER      PIC X(13)   VALUE ' CREATED FOR '.           ECS017
00225      12  P-RUN-DATE  PIC X(87)   VALUE SPACES.                    ECS017
00226      12  FILLER      PIC X(8)    VALUE 'ECS-017 '.                ECS017
00227                                                                   ECS017
00228  01  HDNG-1.                                                      ECS017
00229      12  FILLER      PIC X(52)   VALUE SPACES.                    ECS017
00230      12  FILLER      PIC X(20)   VALUE 'COMPENSATION EXTRACT'.    ECS017
00231      12  FILLER      PIC X(52)   VALUE SPACES.                    ECS017
00232      12  FILLER      PIC X(8)    VALUE 'ECS-017'.                 ECS017
00233                                                                   ECS017
00234  01  HDNG-2.                                                      ECS017
00235      12  FILLER      PIC X(47)   VALUE SPACES.                    ECS017
00236      12  HD-CLIENT   PIC X(30).                                   ECS017
00237      12  FILLER      PIC X(47)   VALUE SPACES.                    ECS017
00238      12  HD-RUN      PIC X(8).                                    ECS017
00239                                                                   ECS017
00240  01  HDNG-3.                                                      ECS017
00241      12  FILLER      PIC X(52)   VALUE SPACES.                    ECS017
00242      12  HD-DATE     PIC X(18).                                   ECS017
00243      12  FILLER      PIC X(42)   VALUE SPACES.                    ECS017
00244      12  FILLER      PIC X(11)   VALUE 'PAGE      1'.             ECS017
00245                                                                   ECS017
00246  01  COMP-MESS.                                                   ECS017
00247      12  FILLER      PIC X(40)   VALUE SPACES.                    ECS017
00248      12  FILLER      PIC X(17)   VALUE '*** COMPENSATION '.       ECS017
00249      12  FILLER      PIC X(75)   VALUE 'EXTRACTS GENERATED ***'.  ECS017

051414 01  max-pdef                    pic s9(5) comp-3 value +5000.
       01  DCC-DDF-WORK-AREA.
051414     05  F OCCURS 5000.
               10  DD-STATE                 PIC XX.
               10  DD-PRODUCT-CD            PIC XXX.
               10  DD-FILLER                PIC X(7).
               10  DD-BEN-TYPE              PIC X.
               10  DD-BEN-CODE              PIC XX.
               10  DD-PROD-EXP-DT           PIC XX.
               10  DD-1ST-YR-ALLOW          PIC S999V99 COMP-3.
032922         10  DD-PROD-DATA OCCURS 11.
                   15  DD-PROD-CODE         PIC X.
               10  DD-EARN-FACTORS.                                                  
                   15  F OCCURS 15.
                       20  F OCCURS 15.
                           25  DD-UEP-FACTOR    PIC S9V9(3)     COMP-3.
               10  DD-TRUNCATED             PIC X.


00250                                                                   ECS017
00251  01  SV-EXTR-REC.                                                 ECS017
00252      12  EX-CC.                                                   ECS017
00253          16  EX-CARRIER      PIC X.                               ECS017
00254          16  EX-GROUPING     PIC X(6).                            ECS017
00255      12  EX-ST               PIC XX.                              ECS017
00256      12  EX-ACCT             PIC X(10).                           ECS017
00257      12  EX-TRAN-TYPE        PIC X.                               ECS017
00258      12  EX-LTYPE.                                                ECS017
00259          16  EX-LTYP         PIC XX.                              ECS017
00260          16  EX-LZ           PIC X.                               ECS017
00261      12  EX-LAMT             PIC S9(9)V99    COMP-3.
011410     12  EX-SPPDD-LCLP       PIC S9(7)V99    COMP-3.
00262      12  EX-LCOM             PIC S9(7)V99    COMP-3.              ECS017
00263      12  EX-LOW              PIC S9(7)V99    COMP-3.              ECS017
00264      12  EX-ATYPE.                                                ECS017
00265          16  EX-ATYP         PIC XX.                              ECS017
00266          16  EX-AZ           PIC X.                               ECS017
00267      12  EX-AAMT             PIC S9(9)V99    COMP-3.              ECS017
011410     12  EX-SPPDD-ACLP       PIC S9(7)V99    COMP-3.
00268      12  EX-ACOM             PIC S9(7)V99    COMP-3.              ECS017
00269      12  EX-AOW              PIC S9(7)V99    COMP-3.              ECS017
040504     12  EX-DLR-INC          PIC S9(7)V99    COMP-3.
011410     12  EX-LF-LMBA-FEE      PIC S9(7)V99    COMP-3.
011410     12  EX-AH-LMBA-FEE      PIC S9(7)V99    COMP-3.
           12  EX-BANK-FEE         PIC S9(7)V99    COMP-3.
           12  EX-CSO-ADMIN        PIC S9(7)V99    COMP-3.
00270      12  EX-IG               PIC X.                               ECS017
00271      12  EX-RECALC           PIC X.                               ECS017
00272      12  EX-COM-TYPE         PIC X.                               ECS017
00273                                                                   ECS017
00274  01  X-REC.                                                       ECS017
00275      12  X-CARRIER           PIC X.                               ECS017
00276      12  X-GROUPING          PIC X(6).                            ECS017
00277      12  X-ST                PIC XX.                              ECS017
00278      12  X-ACCT              PIC X(10).                           ECS017
00279      12  X-IG                PIC X.                               ECS017
00280      12  X-TYPE.                                                  ECS017
00281          16  X-TYP           PIC XX.                              ECS017
00282          16  X-OB            PIC X.                               ECS017
00283      12  X-CODE              PIC 9.                               ECS017
00284 ***      VALUE 1 - ISSUE OR RECALC ISSUE                             CL*10
00285 ***      VALUE 2 - CANCEL OR RECALC CANCEL                           CL*10
00286 ***      VALUE 3 - CLAIM                                             CL*10
00287      12  X-AMT               PIC S9(9)V99   COMP-3.               ECS017
00288      12  X-BASE              PIC S9(7)V99   COMP-3.               ECS017
00289      12  X-OVER              PIC S9(7)V99   COMP-3.               ECS017
040504     12  X-DLR-INC           PIC S9(7)V99   COMP-3.
011410     12  X-LF-LMBA-FEE       PIC S9(7)V99   COMP-3.
011410     12  X-AH-LMBA-FEE       PIC S9(7)V99   COMP-3.
           12  X-BANK-FEE          PIC S9(7)V99   COMP-3.
           12  X-CSO-ADMIN         PIC S9(7)V99   COMP-3.
00290      12  X-PROCESSED         PIC 9(07)      COMP-3.                  CL**5
00291      12  X-RECALC            PIC X.                               ECS017
00292      12  X-ACCT-COM-TYPE     PIC X.                               ECS017
00293      12  X-OWRT-COM-TYPE     PIC X.                               ECS017
00294      12  X-DMD-RESIDENT-ST   PIC XX.                              ECS017
00295      12  FILLER              PIC X.
011410     12  X-SPPDD-CLP         PIC S9(7)V99   COMP-3.
011410     12  FILLER              PIC X(85).
00296                                                                   ECS017
00297  01  WS-X-REC.                                                    ECS017
00298      12  WS-X-PROCESSED      PIC 9(07).                              CL**4
00299      12  WS-X-PROCESSED-R REDEFINES WS-X-PROCESSED.                  CL**4
00300          16  FILLER          PIC 9.                               ECS017
00301          16  X-CC            PIC 99.                              ECS017
00302          16  X-YR            PIC 99.                              ECS017
00303          16  X-MO            PIC 99.                              ECS017
00304                                                                   ECS017
00305  EJECT                                                               CL*11
00306                              COPY ELCDTECX.                          CL*11
00307  EJECT                                                               CL*11
00308                              COPY ELCDTEVR.                          CL*11
00309  EJECT                                                               CL*11
00310                              COPY ELCEXTVR.                          CL*11
00311  EJECT                                                               CL*11
00312                              COPY ELCACCTV.                          CL*11
00313                                                                   ECS017
00314  EJECT                                                               CL*11
00315  PROCEDURE DIVISION.                                              ECS017
00316                                                                   ECS017
00317  0000-STANDARD-COPY.                                              ECS017
00318                              COPY ELCDTERX SUPPRESS.              ECS017
00319                                                                   ECS017
00320      MOVE RUN-YR              TO WS-RUN-YR                        ECS017
00321      MOVE RUN-MO              TO WS-RUN-MO                        ECS017
00322      MOVE RUN-DA              TO WS-RUN-DA                        ECS017
00323                                                                   ECS017
00324      IF DTE-PGM-OPT LESS '1' OR GREATER '8'                          CL*14
00325          MOVE '1' TO               DTE-PGM-OPT.                      CL*14
00326                                                                   ECS017
00327      MOVE ALPH-DATE              TO P-RUN-DATE.                   ECS017
00328                                                                   ECS017
00329      OPEN INPUT  EP-EXTR  ERACCTT ERCOMP                          ECS017
00330           OUTPUT COMM-TRANS  EXTR-DISK  END-PRINT  RECALC-EXTR.   ECS017

           IF DTE-CLIENT = 'DCC' or 'VPP'
              OPEN INPUT ERPDEF
           END-IF
00331                                                                   ECS017
00332      IF ERACCTT-FILE-STATUS  = '00' OR '97'                       ECS017
00333          NEXT SENTENCE                                            ECS017
00334        ELSE                                                       ECS017
00335          MOVE ERACCTT-FILE-STATUS                                 ECS017
00336                                  TO WS-ABEND-FILE-STATUS          ECS017
00337          MOVE ' ERACCTT OPEN ERROR- '                             ECS017
00338                                  TO WS-ABEND-MESSAGE              ECS017
00339          GO TO ABEND-PGM.                                         ECS017
00340                                                                   ECS017

041105     IF ERCOMP-FILE-STATUS  = '00' OR '97'
041105        CONTINUE
041105     ELSE
041105        MOVE ERCOMP-FILE-STATUS  TO WS-ABEND-FILE-STATUS
041105        MOVE ' ERCOMP  OPEN ERROR- '
041105                                 TO WS-ABEND-MESSAGE
041105        PERFORM ABEND-PGM
           END-IF

           IF ERPDEF-FILE-STATUS  = '00' OR '97'
              CONTINUE
           ELSE
              MOVE ERPDEF-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              MOVE ' ERPDEF  OPEN ERROR- '
                                       TO WS-ABEND-MESSAGE
              PERFORM ABEND-PGM
           END-IF

00341      MOVE '1'                    TO X.                               CL**2
00342      MOVE HDNG-1                 TO P-DATA.                       ECS017
00343      PERFORM 8000-PRT-RTN THRU 8099-PRT-XIT.                      ECS017
00344                                                                   ECS017
00345      MOVE ' '                    TO X.                               CL**2
00346      MOVE COMPANY-NAME           TO HD-CLIENT.                    ECS017
00347      MOVE WS-CURRENT-DATE        TO HD-RUN.                       ECS017
00348      MOVE HDNG-2                 TO P-DATA.                       ECS017
00349      PERFORM 8000-PRT-RTN THRU 8099-PRT-XIT.                      ECS017
00350                                                                   ECS017
00351      MOVE ' '                    TO X.                               CL**2
00352      MOVE ALPH-DATE              TO HD-DATE.                      ECS017
00353      MOVE HDNG-3                 TO P-DATA.                       ECS017
00354      PERFORM 8000-PRT-RTN THRU 8099-PRT-XIT.                      ECS017
00355                                                                   ECS017
00356      PERFORM 1600-GET-ACC-MSTR-RTN THRU 1699-EXIT.                ECS017
00357                                                                   ECS017
00358      PERFORM 2800-CLEAR-TOTALS-RTN THRU 2899-EXIT.                ECS017

           IF DTE-CLIENT = 'DCC' or 'VPP'
              MOVE 0 TO B-SUB
              MOVE DTE-CLASIC-COMPANY-CD
                                       TO PD-CONTROL-PRIMARY
              START ERPDEF KEY >= PD-CONTROL-PRIMARY
              IF ERPDEF-FILE-STATUS = '00'
                 PERFORM UNTIL ERPDEF-FILE-STATUS NOT = '00'
                    READ ERPDEF NEXT RECORD
                    IF ERPDEF-FILE-STATUS = '00'
                       ADD 1 TO B-SUB
100413                 IF B-SUB > max-pdef
                          DISPLAY ' DDF UEP TABLE BLEW '
                          GO TO ABEND-PGM
                       END-IF
                       MOVE PD-STATE   TO DD-STATE (B-SUB)
                       MOVE PD-PRODUCT-CD
                                       TO DD-PRODUCT-CD (B-SUB)
                       MOVE PD-BEN-TYPE
                                       TO DD-BEN-TYPE (B-SUB)
                       MOVE PD-BEN-CODE
                                       TO DD-BEN-CODE (B-SUB)
                       MOVE PD-PROD-EXP-DT
                                       TO DD-PROD-EXP-DT (B-SUB)
                       MOVE PD-1ST-YR-ADMIN-ALLOW
                                       TO DD-1ST-YR-ALLOW (B-SUB)
                       MOVE PD-TRUNCATED TO DD-TRUNCATED (B-SUB)
                       PERFORM VARYING P2 FROM +1 BY +1 UNTIL
032922                    P2 > +11
                          MOVE PD-PROD-CODE (P2)
                                       TO DD-PROD-CODE (B-SUB P2)
                       END-PERFORM
                       MOVE PD-EARN-FACTORS
                                       TO DD-EARN-FACTORS (B-SUB)
                    END-IF
                 END-PERFORM
              DISPLAY ' DCC PRODUCT DEF TABLE LOADED SUCCESSFULLY '
              END-IF
              CLOSE ERPDEF
           END-IF
00359                                                                   ECS017
00360      GO TO 1000-READ-EXTRACT.                                     ECS017
00361                                                                   ECS017
00362  EJECT                                                            ECS017
00363  1000-READ-EXTRACT.                                               ECS017
00364      READ EP-EXTR AT END                                          ECS017
00365          GO TO 9900-END-JOB.                                      ECS017
00366                                                                   ECS017
00367      IF DE-REIN NOT = SPACE                                       ECS017
00368          GO TO 1000-READ-EXTRACT.                                 ECS017
00369                                                                   ECS017
00370      IF NOT DE-ISSUE     AND                                      ECS017
00371         NOT DE-CANCEL    AND                                      ECS017
00372         NOT DE-RC-ISSUE  AND                                      ECS017
00373         NOT DE-RC-CANCEL AND                                      ECS017
00374         NOT DE-CLAIM                                              ECS017
00375              GO TO 1000-READ-EXTRACT.                             ECS017
00376                                                                   ECS017
00377      IF DE-ENTRY-STATUS = 'D' OR 'V'                              ECS017
00378          GO TO 1000-READ-EXTRACT.                                 ECS017
00379                                                                   ECS017
00380      IF DE-ISSUE                                                  ECS017
122002         IF DE-ENTRY-STATUS =  '5' OR '9'                         ECS017
00382              GO TO 1000-READ-EXTRACT.                             ECS017
00383                                                                   ECS017
00384      IF DE-RC-ISSUE                                               ECS017
122002         IF DE-ENTRY-STATUS =  '5' OR '9'                         ECS017
00386              GO TO 1000-READ-EXTRACT.                             ECS017
00387                                                                   ECS017
00388      IF DE-CANCEL                                                 ECS017
00389          IF DE-ENTRY-STATUS =  '9'                                ECS017
00390              GO TO 1000-READ-EXTRACT.                             ECS017
00391                                                                   ECS017
00392      IF DE-RC-CANCEL                                              ECS017
00393          IF DE-ENTRY-STATUS =  '9'                                ECS017
00394              GO TO 1000-READ-EXTRACT.                             ECS017
00395                                                                   ECS017
00396      IF DE-CLAIM                                                  ECS017
00397          IF DE-PAY-CODE = 'Y' OR 'Z'                              ECS017
00398              GO TO 1000-READ-EXTRACT.                             ECS017
00399                                                                   ECS017
00400      COPY ELCEXTM1.                                               ECS017
00401      MOVE DE-CARRIER             TO W-CARRIER.                    ECS017
00402      MOVE DE-GROUPING            TO W-GROUPING.                   ECS017
00403      MOVE DE-STATE               TO W-ST.                         ECS017
00404      MOVE DE-ACCOUNT             TO W-ACCT.                       ECS017
00405      MOVE DE-EFF                 TO W-DATE.                       ECS017
00406                                                                   ECS017
00407  1200-MATCH-ACCT.                                                 ECS017
00408      IF AM-MSTR-CNTRL GREATER THAN WK-CTL                         ECS017
00409          GO TO 1400-HAVE-ACCT.                                    ECS017
00410                                                                   ECS017
00411      IF ACCT-SW = +1                                              ECS017
00412          PERFORM 2600-ACCT-BREAK-RTN THRU 2699-EXIT.              ECS017
00413                                                                   ECS017
00414      PERFORM 1600-GET-ACC-MSTR-RTN THRU 1699-EXIT.                ECS017
00415                                                                   ECS017
00416      GO TO 1200-MATCH-ACCT.                                       ECS017
00417                                                                   ECS017
00418  1400-HAVE-ACCT.                                                  ECS017
00419      PERFORM 2000-FORMAT-RECORD-RTN THRU 2099-EXIT.               ECS017
00420                                                                   ECS017
00421      PERFORM 2200-CALC-REMIT-TO-RTN THRU 2299-EXIT.               ECS017
00422                                                                   ECS017
00423      PERFORM 2100-REVERSE-RTN       THRU 2199-EXIT.               ECS017
00424                                                                   ECS017
00425      IF DE-CLAIM                                                  ECS017
00426         IF DE-CP-YR = WS-RUN-YR AND DE-CP-MO = WS-RUN-MO          ECS017
00427            PERFORM 3000-WRT-CCM-CLAIM-RTN THRU 3099-EXIT          ECS017
00428            PERFORM 5000-BUILD-CLAIM-RTN   THRU 5099-EXIT          ECS017
00429            GO TO 1000-READ-EXTRACT                                ECS017
00430            ELSE                                                   ECS017
00431            GO TO 1000-READ-EXTRACT.                               ECS017
00432                                                                   ECS017
00433      MOVE  +1                TO ACCT-SW.                          ECS017
00434                                                                   ECS017
00435      PERFORM 2400-ACCT-COMM-RTN THRU 2499-EXIT.                   ECS017
00436                                                                   ECS017
00437      GO TO 1000-READ-EXTRACT.                                     ECS017
00438                                                                   ECS017
00439  EJECT                                                            ECS017
00440  1600-GET-ACC-MSTR-RTN.                                           ECS017
00441      READ ERACCTT.                                                ECS017
00442                                                                   ECS017
00443      IF ERACCTT-FILE-STATUS = '10'                                ECS017
00444          MOVE HIGH-VALUES        TO ACCOUNT-MASTER                ECS017
00445       ELSE                                                        ECS017
00446          IF ERACCTT-FILE-STATUS NOT = ZERO                        ECS017
00447              MOVE ERACCTT-FILE-STATUS                             ECS017
00448                              TO WS-ABEND-FILE-STATUS              ECS017
00449              MOVE ' ERACCTT READ ERROR- '                         ECS017
00450                              TO WS-ABEND-MESSAGE                  ECS017
00451              GO TO ABEND-PGM.                                     ECS017
00452                                                                   ECS017
00453      MOVE +0                     TO ACCT-SW.                      ECS017
00454                                                                   ECS017
00455  1699-EXIT.                                                       ECS017
00456      EXIT.                                                        ECS017
00457  EJECT                                                            ECS017
00458  2000-FORMAT-RECORD-RTN.                                          ECS017
00459      PERFORM 6000-CLEAR-EXTR-RTN THRU 6099-EXIT.                  ECS017
00460                                                                   ECS017
00461      MOVE SPACES                 TO CP-RECORD.                    ECS017
00462      MOVE '%%'                   TO CP-RECORD-ID.                 ECS017
00463      MOVE DTE-CLASIC-COMPANY-CD  TO CP-COMPANY-CD.                ECS017
00464      MOVE DE-CARRIER             TO CP-CARRIER.                   ECS017
00465      MOVE DE-GROUPING            TO CP-GROUPING.                  ECS017
00466      MOVE DE-ACCOUNT             TO CP-ACCOUNT.                   ECS017
00467      MOVE DE-TRANS               TO CP-TRANS.                     ECS017
122002     MOVE DE-ENTRY-STATUS        TO CP-ENTRY-STATUS
00468      MOVE DE-STATE               TO CP-STATE.                     ECS017
00469      MOVE DE-EFF                 TO CP-EFF.                       ECS017
00470      MOVE DE-CERT                TO CP-CERT.                      ECS017
00471      MOVE DE-REPORT-CODE-2       TO CP-NCL-REGION                 ECS017
00472                                     SAVE-NCL-REGION.              ECS017
           MOVE SPACES                 TO CP-NCL-POOL-CODE
                                          SAVE-NCL-POOL-CODE

00473 *    MOVE DE-NCL-POOL-CODE       TO CP-NCL-POOL-CODE              ECS017
00474 *                                   SAVE-NCL-POOL-CODE.           ECS017
00475      MOVE DE-NAME                TO CP-INSUREDS-NAME.             ECS017
00476      MOVE DE-AGE                 TO CP-AGE.                       ECS017
00477      MOVE DE-SEX                 TO CP-SEX.                       ECS017
00478      MOVE DE-LF-TYPE             TO CP-LF-TYPE.                   ECS017
00479      MOVE DE-AH-TYPE             TO CP-AH-TYPE.                   ECS017
00480      IF DE-LF-PRM NOT NUMERIC MOVE ZEROS TO DE-LF-PRM.            ECS017
00481      IF DE-AH-PRM NOT NUMERIC MOVE ZEROS TO DE-AH-PRM.            ECS017
00482      IF DE-LF-PRM-ALT NOT NUMERIC MOVE ZEROS TO DE-LF-PRM-ALT.    ECS017
00483      IF DE-LF-BEN NOT NUMERIC MOVE ZEROS TO DE-LF-BEN.            ECS017
00484      IF DE-AH-BEN NOT NUMERIC MOVE ZEROS TO DE-AH-BEN.            ECS017
00485      IF DE-LF-BEN-ALT NOT NUMERIC MOVE ZEROS TO DE-LF-BEN-ALT.    ECS017
011904
00486      MOVE DE-LF-BEN              TO CP-LF-AMT.                    ECS017
00487      MOVE DE-LF-PRM              TO CP-LF-PRM.                    ECS017
00488      MOVE DE-LF-BEN-ALT          TO CP-LF-AMT-ALT.                ECS017
00489      MOVE DE-LF-PRM-ALT          TO CP-LF-PRM-ALT
00490      MOVE DE-AH-BEN              TO CP-AH-AMT.                    ECS017
00491      MOVE DE-AH-PRM              TO CP-AH-PRM.                    ECS017
00492                                                                   ECS017
011904     IF DTE-CLIENT = 'DCC' or 'VPP'
011904        IF DE-ENTRY-STATUS = 'M'
011904           IF DE-MOB-NET-TOT-FEES NOT NUMERIC
011904              MOVE ZEROS         TO DE-MOB-NET-TOT-FEES
011904           END-IF
011904           IF DE-LF-TYPE (1:1) = 'N'
011904              MOVE DE-MOB-NET-TOT-FEES
011904                                 TO CP-LF-PRM
011904              MOVE ZEROS         TO CP-AH-PRM
011904           ELSE
011904              IF DE-AH-TYPE (1:1) = 'N'
011904                 MOVE DE-MOB-NET-TOT-FEES
011904                                 TO CP-AH-PRM
011904                 MOVE ZEROS      TO CP-LF-PRM
011904              END-IF
011904           END-IF
011904        END-IF
011904     END-IF
011904
00493      IF NOT DE-CLAIM                                              ECS017
00494          MOVE DE-ACC-GPCD        TO CP-GPCD                       ECS017
00495          MOVE DE-BILL-STATUS     TO CP-BILL-STATUS.               ECS017
00496                                                                   ECS017
00497      MOVE DE-LF-TERM             TO CP-LF-TERM.                   ECS017
00498      MOVE DE-AH-TERM             TO CP-AH-TERM.                   ECS017
00499      MOVE DE-MEMBER-NO           TO CP-MEMBER.                    ECS017
00500      MOVE DE-SOC-SEC-NO          TO CP-SOC-SEC.                   ECS017
00501      MOVE DE-IG                  TO CP-IG.                        ECS017
00502      MOVE DE-LF-CANC-DTE         TO CP-LF-CNC.                    ECS017
00503      MOVE DE-AH-CANC-DTE         TO CP-AH-CNC.                    ECS017
00504      MOVE +0                     TO CP-LF-COM  CP-AH-COM          ECS017
00505                                     CP-LF-COM-ALT                 ECS017
00506                                     CP-LPC     CP-APC.            ECS017
00507                                                                   ECS017
00508      IF DE-CLAIM                                                  ECS017
00509          MOVE DE-ACC-GPCD        TO CP-CLM-GPCD                   ECS017
00510          MOVE +0                 TO CP-CLM-LF-AMT                 ECS017
00511                                     CP-CLM-AH-AMT.                ECS017
00512                                                                   ECS017
100703     IF DTE-CLIENT = 'DCC' or 'VPP'
100703        PERFORM 6300-CHECK-TYPES THRU 6399-EXIT
100703     END-IF
100703     .
00513  2099-EXIT.                                                       ECS017
00514      EXIT.                                                        ECS017
00515  EJECT                                                            ECS017
00516  2100-REVERSE-RTN.                                                ECS017
00517      IF DE-CLAIM                                                  ECS017
00518          GO TO 2110-CLAIM-RTN.                                    ECS017
00519                                                                   ECS017
00520      IF DE-ISSUE                                                  ECS017
00521          GO TO 2120-ISSUE-RTN.                                    ECS017
00522                                                                   ECS017
00523      IF DE-CANCEL                                                 ECS017
00524          GO TO 2130-CANCEL-RTN.                                   ECS017
00525                                                                   ECS017
00526      IF DE-RC-ISSUE                                               ECS017
00527          GO TO 2140-RC-ISSUE-RTN.                                 ECS017
00528                                                                   ECS017
00529      IF DE-RC-CANCEL                                              ECS017
00530          GO TO 2150-RC-CANCEL-RTN.                                ECS017
00531                                                                   ECS017
00532      GO TO 2199-EXIT.                                             ECS017
00533                                                                   ECS017
00534  2110-CLAIM-RTN.                                                  ECS017
00535 * NO REVERSE NECESSARY.                                           ECS017
00536      GO TO 2199-EXIT.                                             ECS017
00537                                                                   ECS017
00538  2120-ISSUE-RTN.                                                  ECS017
00539 * NO REVERSE NECESSARY.                                           ECS017
00540      GO TO 2199-EXIT.                                             ECS017
00541                                                                   ECS017
00542  2130-CANCEL-RTN.                                                 ECS017
00543 * MOVE REFUND AMOUNTS TO PREMIUM AMOUNTS                          ECS017
00544      PERFORM 2160-REFUND-RTN THRU 2169-EXIT.                      ECS017
00545                                                                   ECS017
00546 * REVERSE PREMIUM AND BENEFIT AMOUNT                              ECS017
00547      PERFORM 2170-REV-PRM-RTN THRU 2179-EXIT.                     ECS017
00548                                                                   ECS017
00549      GO TO 2199-EXIT.                                             ECS017
00550                                                                   ECS017
00551  2140-RC-ISSUE-RTN.                                               ECS017
00552 * NO REVERSE NECESSARY FOR THE NEW RECALCULATED.                  ECS017
      *  IF RECALC-TYPE = O THEN WE NEED TO BACK OUT THE OLD
00553      IF DE-RECALC-TYPE = 'N'                                      ECS017
00554          GO TO 2199-EXIT.                                         ECS017
00555                                                                   ECS017
00556      PERFORM 2190-MOVE-AGT-NO THRU 2195-EXIT.                     ECS017
00557                                                                   ECS017
00558 * REVERSE PREMIUM AND COMMISSION FOR THE OLD RECALCULATED.        ECS017
00559      PERFORM 2170-REV-PRM-RTN THRU 2179-EXIT.                     ECS017
00560                                                                   ECS017
00561      PERFORM 2180-REV-COM-RTN THRU 2189-EXIT.                     ECS017
00562                                                                   ECS017
00563      GO TO 2199-EXIT.                                             ECS017
00564                                                                   ECS017
00565  2150-RC-CANCEL-RTN.                                              ECS017
00566 * MOVE REFUND AMOUNTS TO PREMIUM AMOUNTS                          ECS017
00567      PERFORM 2160-REFUND-RTN THRU 2169-EXIT.                      ECS017
00568                                                                   ECS017
00569 * REVERSE COMMISSION AMOUNTS FOR OLD RECALCULATED.                ECS017
00570      IF DE-RECALC-TYPE = 'O'                                      ECS017
00571          PERFORM 2190-MOVE-AGT-NO THRU 2195-EXIT                  ECS017
00572          PERFORM 2180-REV-COM-RTN THRU 2189-EXIT                  ECS017
00573          GO TO 2199-EXIT.                                         ECS017
00574                                                                   ECS017
00575 * REVERSE PREMIUM FOR NEW RECALCULATED.                           ECS017
00576      PERFORM 2170-REV-PRM-RTN THRU 2179-EXIT.                     ECS017
00577                                                                   ECS017
00578      GO TO 2199-EXIT.                                             ECS017
00579                                                                   ECS017
00580  EJECT                                                            ECS017
00581  2160-REFUND-RTN.                                                 ECS017
00582      MOVE DE-LF-RFND             TO CP-LF-PRM.                    ECS017
00583      MOVE ZEROS                  TO CP-LF-PRM-ALT.                ECS017
00584      MOVE DE-AH-RFND             TO CP-AH-PRM.
           if dte-client = 'VPP'
              move de-cancel-fee       to cp-cancel-fee
              if de-ah-rfnd < cp-cancel-fee
                 move de-ah-rfnd       to cp-cancel-fee
              end-if
              if de-ah-rfnd = de-ah-prm
                 move zeros to cp-cancel-fee
              end-if
           end-if

           .
00586  2169-EXIT.                                                       ECS017
00587      EXIT.                                                        ECS017
00588                                                                   ECS017
00589  2170-REV-PRM-RTN.                                                ECS017
00590      COMPUTE CP-LF-AMT     = CP-LF-AMT * -1.                      ECS017
00591      COMPUTE CP-LF-AMT-ALT = CP-LF-AMT-ALT * -1.                  ECS017
00592      COMPUTE CP-LF-PRM     = CP-LF-PRM * -1.                      ECS017
00593      COMPUTE CP-LF-PRM-ALT = CP-LF-PRM-ALT * -1.                  ECS017
00594      COMPUTE CP-AH-AMT     = CP-AH-AMT * -1.                      ECS017
00595      COMPUTE CP-AH-PRM     = CP-AH-PRM * -1.                      ECS017
00596                                                                   ECS017
00597  2179-EXIT.                                                       ECS017
00598      EXIT.                                                        ECS017
00599                                                                   ECS017
00600  2180-REV-COM-RTN.                                                ECS017
00601      MOVE +1                     TO RV-NDX.                       ECS017
00602                                                                   ECS017
00603  2185-LOOP.                                                       ECS017
00604      COMPUTE DE-L-PC (RV-NDX) = DE-L-PC (RV-NDX) * -1.            ECS017
00605      COMPUTE DE-A-PC (RV-NDX) = DE-A-PC (RV-NDX) * -1.            ECS017
00606                                                                   ECS017
00607      ADD +1 TO RV-NDX.                                            ECS017
00608                                                                   ECS017
00609      IF RV-NDX LESS THAN +11                                      ECS017
00610          GO TO 2185-LOOP.                                         ECS017
00611                                                                   ECS017
00612  2189-EXIT.                                                       ECS017
00613      EXIT.                                                        ECS017
00614                                                                   ECS017
00615  2190-MOVE-AGT-NO.                                                ECS017
00616      MOVE DE-REMIT-TO            TO RV-NDX.                       ECS017
00617                                                                   ECS017
00618      IF DE-AGT (RV-NDX) = SPACES OR ZEROS                         ECS017
00619          GO TO 2195-EXIT.                                         ECS017
00620                                                                   ECS017
00621      MOVE DE-AGT (RV-NDX)        TO CP-REMIT.                     ECS017
00622                                                                   ECS017
00623  2195-EXIT.                                                       ECS017
00624      EXIT.                                                        ECS017
00625                                                                   ECS017
00626  2199-EXIT.                                                       ECS017
00627      EXIT.                                                        ECS017
00628  EJECT                                                            ECS017
00629  2200-CALC-REMIT-TO-RTN.                                          ECS017
00630      MOVE +1                     TO FAL.                          ECS017
00631                                                                   ECS017
00632      IF DE-CLAIM                                                  ECS017
00633          GO TO 2260-CALC-REMIT-TO-CLM.                            ECS017
00634                                                                   ECS017
00635  2220-FIND-1ST-ACCT-LEV.                                          ECS017
00636      IF FAL = +11                                                 ECS017
00637          MOVE +1                 TO FAL                           ECS017
00638          GO TO 2230-PLUG-1ST-LEVEL.                               ECS017
00639                                                                   ECS017
052814     IF DE-AGT-TYPE (FAL) = 'C' OR 'D' OR 'F'
00641          NEXT SENTENCE                                            ECS017
00642      ELSE                                                         ECS017
00643          ADD +1 TO FAL                                            ECS017
00644          GO TO 2220-FIND-1ST-ACCT-LEV.                            ECS017
00645                                                                   ECS017
00646  2230-PLUG-1ST-LEVEL.                                             ECS017

00647      MOVE DE-AGT (FAL)           TO CP-ACCOUNT.                   ECS017
00648      MOVE DE-AGT-TYPE (FAL)      TO CP-COM-TYPE.                  ECS017
00649      MOVE DE-GL-CODES (FAL)      TO CP-GL-CODE.                   ECS017
00650                                                                   ECS017
00651      IF DTE-CLIENT = 'ACM'                                        ECS017
00652          MOVE AM-USER-SELECT-1   TO CP-ASSIGN-AGT.                ECS017
00653                                                                   ECS017
00654      IF CP-ACCOUNT NOT = ZEROS AND SPACES                         ECS017
00655          GO TO 2240-CALC-REMIT-CONT                               ECS017
00656      ELSE                                                         ECS017
00657          MOVE +1                 TO AGTNDX.                       ECS017
00658                                                                   ECS017
00659  2231-FIND-AGENT-LOOP.                                            ECS017
00660                                                                   ECS017
052814     if agtndx > +10
052814        move am-account to cp-account
052814                           cp-remit
052814        display ' fixed remit stuff '
052814        go to 2299-exit
052814     end-if
052814
052814     IF AM-COM-TYP (FAL) = 'C' OR 'D' OR 'F'
00662          NEXT SENTENCE                                            ECS017
00663      ELSE                                                         ECS017
00664          ADD +1 TO AGTNDX                                         ECS017
00665          GO TO 2231-FIND-AGENT-LOOP.                              ECS017
00666                                                                   ECS017
00667      MOVE AM-AGT (AGTNDX)        TO CP-ACCOUNT.                   ECS017
00668                                                                   ECS017
00669  2240-CALC-REMIT-CONT.                                            ECS017
00670                                                                   ECS017
00671      IF AM-REMIT-TO NOT NUMERIC OR                                ECS017
00672         AM-REMIT-TO = ZEROS                                       ECS017
00673          MOVE '01'               TO AM-REMIT-TO.                  ECS017
00674                                                                   ECS017
00675      MOVE AM-REMIT-TO            TO X1.                           ECS017
00676                                                                   ECS017
00677      IF AM-AGT (X1) = ZEROS                                       ECS017
00678          MOVE +1                 TO X1.                           ECS017
00679                                                                   ECS017
00680      MOVE AM-AGT (X1)            TO CP-REMIT.                     ECS017
00681                                                                   ECS017
00682      IF CP-REMIT NOT = ZEROS AND SPACES                           ECS017
00683          GO TO 2299-EXIT                                          ECS017
00684      ELSE                                                         ECS017
00685          MOVE +1                 TO AGTNDX.                       ECS017
00686                                                                   ECS017
00687  2241-FIND-REMIT-LOOP.                                            ECS017
00688                                                                   ECS017
052814     IF AM-COM-TYP (FAL) = 'C' OR 'D' OR 'F'
00690          NEXT SENTENCE                                            ECS017
00691      ELSE                                                         ECS017
00692          ADD +1 TO AGTNDX                                         ECS017
00693          GO TO 2241-FIND-REMIT-LOOP.                              ECS017
00694                                                                   ECS017
00695      MOVE AM-AGT (AGTNDX)        TO CP-REMIT.                     ECS017
00696                                                                   ECS017
00697      GO TO 2299-EXIT.                                             ECS017
00698                                                                   ECS017
00699  2260-CALC-REMIT-TO-CLM.                                          ECS017
00700      IF FAL = +11                                                 ECS017
00701          MOVE +1                 TO FAL                           ECS017
00702          GO TO 2270-PLUG-1ST-LEVEL-CLM.                           ECS017
00703                                                                   ECS017
052814     IF AM-COM-TYP (FAL) = 'C' OR 'D' OR 'F'
00705          NEXT SENTENCE                                            ECS017
00706      ELSE                                                         ECS017
00707          ADD +1 TO FAL                                            ECS017
00708          GO TO 2260-CALC-REMIT-TO-CLM.                            ECS017
00709                                                                   ECS017
00710  2270-PLUG-1ST-LEVEL-CLM.                                         ECS017
00711      MOVE AM-AGT (FAL)           TO CP-ACCOUNT.                   ECS017
00712                                                                   ECS017
00713      IF CP-ACCOUNT = SPACES OR ZEROS                              ECS017
00714          MOVE AM-ACCOUNT         TO CP-ACCOUNT.                   ECS017
00715                                                                   ECS017
00716  2280-CALC-REMIT-CONT-CLM.                                        ECS017
00717                                                                   ECS017
00718      IF AM-REMIT-TO NOT NUMERIC OR                                ECS017
00719         AM-REMIT-TO = ZEROS                                       ECS017
00720          MOVE '01'               TO AM-REMIT-TO.                  ECS017
00721                                                                   ECS017
00722      MOVE AM-REMIT-TO            TO X1.                           ECS017
00723                                                                   ECS017
00724      IF AM-AGT (X1) = ZEROS                                       ECS017
00725          MOVE +1                 TO X1.                           ECS017
00726                                                                   ECS017
00727      MOVE AM-AGT (X1)            TO CP-REMIT.                     ECS017
00728                                                                   ECS017
00729      IF CP-REMIT = ZEROS OR SPACES                                ECS017
00730          MOVE AM-ACCOUNT         TO CP-REMIT.                     ECS017
00731                                                                   ECS017
00732  2299-EXIT.                                                       ECS017
00733      EXIT.                                                        ECS017
00734  EJECT                                                            ECS017
00735  2400-ACCT-COMM-RTN.                                              ECS017
011410     MOVE ' '                    TO WS-DUP-AGT-SW

00736      MOVE +0                     TO X1.                           ECS017
00737      MOVE +1                     TO NDX.                          ECS017
00738                                                                   ECS017
00739  2410-CHECK-COMM-CHARGE-BACK.                                     ECS017
00740      IF NOT DE-CANCEL  AND                                        ECS017
00741         NOT DE-RC-CANCEL                                          ECS017
00742          GO TO 2420-ACCT-COMM-LOOP.                               ECS017
00743                                                                   ECS017
00744      IF DTE-CLIENT = 'FFL' OR 'SBF'                               ECS017
00745          IF DE-GROUPING = '000ARM'                                ECS017
00746              GO TO 2440-OW-LOOP.                                  ECS017
00747                                                                   ECS017
00748      IF (DTE-CLIENT = 'UCL') AND                                  ECS017
00749          (AM-FLD-1 = 'MD')                                        ECS017
00750          GO TO 2412-COMPUTE-MO-DIFF                               ECS017
00751      ELSE                                                         ECS017
00752          GO TO 2420-ACCT-COMM-LOOP.                               ECS017
00753 **** UCL ONLY CODE                                                ECS017
00754                                                                   ECS017
00755  2412-COMPUTE-MO-DIFF.                                            ECS017
00756                                                                   ECS017
00757      IF DE-LF-CANC-DTE > ZERO                                     ECS017
00758          COMPUTE MONTHS-DIFF-LF =                                 ECS017
00759              ((DE-LF-CANC-CCYY * 12)+ DE-LF-CANC-MO) -            ECS017
00760                                                                   ECS017
00761                  ((DE-EF-CCYY * 12)+ DE-EF-MO)                    ECS017
00762      ELSE                                                         ECS017
00763          MOVE ZEROS              TO  MONTHS-DIFF-LF.              ECS017
00764                                                                   ECS017
00765      IF DE-AH-CANC-DTE > ZERO                                     ECS017
00766          COMPUTE MONTHS-DIFF-AH =                                 ECS017
00767              (((DE-AH-CANC-CCYY * 12) + DE-AH-CANC-MO) -          ECS017
00768                   ((DE-EF-CCYY  * 12) + DE-EF-MO))                ECS017
00769      ELSE                                                         ECS017
00770          MOVE ZEROS              TO  MONTHS-DIFF-AH.              ECS017
00771                                                                   ECS017
00772 ***************************************************************** ECS017
00773 *  AS PER CREDIT LIFE DEPT.  WE WILL ASSUME THE LIFE AND A & H  * ECS017
00774 *  COVERAGE WILL ALWAYS HAVE THE SAME EFFECTIVE DATE. THEREFORE * ECS017
00775 *  ONLY ONE CHECK WILL BE NECESSARY FOR CLIENT 'UCL'.           * ECS017
00776 *  F. RHORER  7/2/90                                            * ECS017
00777 ***************************************************************** ECS017
00778                                                                   ECS017
00779      IF DTE-CLIENT = 'UCL'                                        ECS017
00780        IF MONTHS-DIFF-LF > 6                                      ECS017
00781             GO TO 2490-OW-L-X                                     ECS017
00782        ELSE                                                       ECS017
00783           GO TO 2420-ACCT-COMM-LOOP.                              ECS017
00784                                                                   ECS017
00785      IF MONTHS-DIFF-LF > 6                                        ECS017
00786          IF MONTHS-DIFF-AH > 6                                    ECS017
00787              GO TO 2490-OW-L-X.                                   ECS017
00788                                                                   ECS017
00789      IF MONTHS-DIFF-LF < 6                                        ECS017
00790          IF MONTHS-DIFF-AH < 6                                    ECS017
00791              GO TO 2420-ACCT-COMM-LOOP.                           ECS017
00792                                                                   ECS017
00793      IF MONTHS-DIFF-LF < 6                                        ECS017
00794          MOVE ZEROS              TO  CP-AH-PRM                    ECS017
00795          GO TO 2420-ACCT-COMM-LOOP.                               ECS017
00796                                                                   ECS017
00797      IF MONTHS-DIFF-AH < 6                                        ECS017
00798          MOVE ZEROS              TO  CP-LF-PRM                    ECS017
00799          GO TO 2420-ACCT-COMM-LOOP.                               ECS017
00800                                                                   ECS017
00801      IF MONTHS-DIFF-LF = 6                                        ECS017
00802          IF MONTHS-DIFF-AH = 6                                    ECS017
00803              IF DE-LF-CANC-DA < DE-EF-DA                          ECS017
00804                  IF DE-AH-CANC-DA < DE-EF-DA                      ECS017
00805                      GO TO 2420-ACCT-COMM-LOOP.                   ECS017
00806                                                                   ECS017
00807      IF MONTHS-DIFF-LF = 6                                        ECS017
00808          IF DE-LF-CANC-DA < DE-EF-DA                              ECS017
00809              MOVE ZEROS          TO  CP-AH-PRM                    ECS017
00810              GO TO 2420-ACCT-COMM-LOOP.                           ECS017
00811                                                                   ECS017
00812      IF MONTHS-DIFF-AH = 6                                        ECS017
00813          IF DE-AH-CANC-DA < DE-EF-DA                              ECS017
00814              MOVE ZEROS          TO  CP-LF-PRM                    ECS017
00815              GO TO 2420-ACCT-COMM-LOOP.                           ECS017
00816                                                                   ECS017
00817      GO TO 2490-OW-L-X.                                           ECS017
00818                                                                   ECS017
00819 **** END OF UCL ONLY CODE                                         ECS017
00820                                                                   ECS017
00821  2420-ACCT-COMM-LOOP.                                             ECS017
00822      ADD +1 TO X1.                                                ECS017
00823                                                                   ECS017
00824      IF DTE-CLIENT = 'NCL'                                        ECS017
00825          IF X1 GREATER THAN +10                                   ECS017
00826             MOVE  CP-RECORD          TO SAVE-CPTR.                ECS017
00827                                                                   ECS017
00828      IF X1 GREATER THAN +10                                       ECS017
00829          MOVE 0                  TO X1
041105                                    B-CNT
00830          GO TO 2440-OW-LOOP.                                      ECS017
00831                                                                   ECS017
052814     IF DE-AGT-TYPE (X1) NOT = 'C' AND 'D' AND 'F'
00833          GO TO 2420-ACCT-COMM-LOOP.                               ECS017
00834                                                                   ECS017
00835      COMPUTE L-OW ROUNDED     = CP-LF-PRM * DE-L-PC (X1).         ECS017
00836      COMPUTE L-OW-ALT ROUNDED = CP-LF-PRM-ALT * DE-L-PC (X1).     ECS017
092705     IF CLAS-I-BEN-CATEGORY (CLAS-INDEXA) = 'G' OR 'L'
100703        IF DE-CANCEL
100703           COMPUTE CNC-FACT = DE-AH-RFND / DE-AH-PRM
                 IF (AM-DCC-PRODUCT-CODE = 'DDF')
                    and (dte-client = 'DCC')
                    COMPUTE A-OW = (DE-AH-PRM - DE-REI-AHPRM -
                       DE-ADDL-CLP)
                    PERFORM 2540-CALC-DDF-COMM THRU 2540-EXIT
                    COMPUTE A-OW = CNC-WK * -1
                 ELSE
                    IF DE-CANCEL-REASON = 'R'
                       MOVE +1            TO CNC-FACT
                    END-IF
101409              COMPUTE A-OW ROUNDED = (CNC-FACT *
100703                 (DE-AH-PRM - DE-LF-PRM-ALT
040504                 - DE-ADDL-CLP)) * -1
                 END-IF
100703        ELSE
      *          display ' ah comm ' de-cert ' ' cp-ah-prm ' '
      *             cp-lf-prm-alt ' ' de-addl-clp ' ' de-lf-prm-alt
100703           COMPUTE A-OW = CP-AH-PRM - CP-LF-PRM-ALT
040504           - DE-ADDL-CLP
100703        END-IF
100703     ELSE
00837         COMPUTE A-OW ROUNDED     = CP-AH-PRM * DE-A-PC (X1)
100703     END-IF
00838                                                                   ECS017
00839      ADD L-OW         TO CP-LF-COM.                               ECS017
00840      ADD L-OW-ALT     TO CP-LF-COM-ALT.                           ECS017
00841      ADD A-OW         TO CP-AH-COM.                               ECS017
00842      ADD DE-L-PC (X1) TO CP-LPC.                                  ECS017
00843      ADD DE-A-PC (X1) TO CP-APC.                                  ECS017
00844                                                                   ECS017
00845      GO TO 2420-ACCT-COMM-LOOP.                                   ECS017
00846                                                                   ECS017
00847  2440-OW-LOOP.                                                    ECS017
00848                                                                   ECS017
011410     MOVE ZEROS TO A-OW L-OW
00849      ADD +1 TO X1.                                                ECS017
00850                                                                   ECS017
00851      IF X1 GREATER +10                                            ECS017
00852          GO TO 2490-OW-L-X.                                       ECS017
00853                                                                   ECS017
00854      IF DE-AGT (X1) = ZERO OR SPACES                              ECS017
00855          GO TO 2440-OW-LOOP.                                      ECS017
00856                                                                   ECS017
00857      IF DE-AGT-TYPE (X1) NOT = 'O' AND 'P' AND 'A'
052814                     AND 'G' and 'S'
100703                     AND 'B' AND 'I' AND 'K' AND 'L'
052306                     AND 'J' AND 'M' AND 'N'
00859          GO TO 2440-OW-LOOP.                                      ECS017
00860                                                                   ECS017
122002     IF CP-MONTHLY-ISSUE
122002        GO TO 2440-OW-LOOP
122002     END-IF
122002
00861      IF DTE-CLIENT NOT = 'UCL'                                    ECS017
00862          GO TO 2460-FIND-AGT.                                     ECS017
00863                                                                   ECS017
00864 *******   UCL CODE  ******                                        ECS017
00865 *******   PELLEGRINI CODE GOES HERE                               ECS017
00866 *******   08-20-90  --  AS PER FPN -- JAY PELLEGRINI WILL BE      ECS017
00867 *******   CHARGED BACK COMMISSIONS IN CARRIERS OTHER THAN         ECS017
00868 *******   1, 2, 3, OR 6.                                          ECS017
00869 *******   01/06/92  --  AS PER FPN  -- JAY PELLEGRINI WILL NOW BE ECS017
00870 *******   CHARGED BACK COMMISSIONS IN CARRIER 2.                  ECS017
00871      IF (DE-AGT-TYPE (X1) = 'O' OR 'G') AND                       ECS017
00872          (DE-AGT (X1) = '9999999999' OR '0721032702') AND         ECS017
00873          (DE-CARRIER = '1' OR '3' OR '6') AND                     ECS017
00874 ******   (DE-CARRIER = '1' OR '2' OR '3' OR '6') AND              ECS017
00875          (DE-IG = '2' OR 'G' OR SPACES)                           ECS017
00876           GO TO 2490-OW-L-X.                                      ECS017
00877 *******   END OF PELLEGRINI CODE                                  ECS017
00878                                                                   ECS017
00879  2460-FIND-AGT.                                                   ECS017

00880      IF DE-AGT (X1)      = TBL-AGT (NDX)
011410        IF DE-CONTROL = TBL-DE-CONTROL (NDX)
011410           SET DUP-AGENT TO TRUE
011410        ELSE
011410           MOVE DE-CONTROL       TO TBL-DE-CONTROL (NDX)
011410        END-IF
00881         IF DE-AGT-TYPE (X1) = TBL-COM-TYP (NDX)
00882          GO TO 2465-HAVE-AGENT.                                   ECS017
00883                                                                   ECS017
00884      IF TBL-AGT (NDX) = ZEROS                                     ECS017
00885          MOVE DE-AGT (X1)        TO TBL-AGT (NDX)                 ECS017
00886          MOVE DE-AGT-TYPE (X1)   TO TBL-COM-TYP (NDX)             ECS017
00887          MOVE DE-GL-CODES (X1)   TO TBL-GL-CODE (NDX)
011410         MOVE DE-CONTROL         TO TBL-DE-CONTROL (NDX)
00888          IF DTE-CLIENT = 'ACM'                                    ECS017
00889              IF X1 LESS THAN +6                                   ECS017
00890                  MOVE AM-USER-SELECT-OPTIONS                      ECS017
00891                                  TO WS-ASSIGN-TABLE               ECS017
00892                  MOVE WS-ASSIGN-AGT (X1)                          ECS017
00893                                  TO TBL-ASSIGN-AGT (NDX)          ECS017
00894              ELSE                                                 ECS017
00895                  NEXT SENTENCE                                    ECS017
00896          ELSE                                                     ECS017
00897              NEXT SENTENCE                                        ECS017
00898      ELSE                                                         ECS017
011410         IF DE-AGT (X1) NOT = TBL-AGT (NDX)
011410            MOVE ' ' TO WS-DUP-AGT-SW
011410         END-IF
00899          ADD +1 TO NDX                                            ECS017
00900          GO TO 2460-FIND-AGT.                                     ECS017
00901                                                                   ECS017
00902  2465-HAVE-AGENT.                                                 ECS017
00903                                                                   ECS017
041105     IF DE-AGT-TYPE (X1) = 'B'
              ADD +1                   TO B-CNT
              IF B-CNT = +1
                 MOVE DE-AGT (X1)      TO WS-ERCOMP-FIN-RESP
              ELSE
                 IF B-CNT > +1
      *             MOVE DE-AGT (X1)   TO WS-ERCOMP-FIN-RESP
                    MOVE 'B'           TO WS-ERCOMP-TYPE
                    MOVE LOW-VALUES    TO WS-ERCOMP-ACCOUNT
                    PERFORM 2700-GET-BANK-NAME
                                       THRU 2700-EXIT
                    MOVE CO-ACCT-NAME  TO TBL-BK-NAME (NDX)
                    MOVE WS-ERCOMP-FIN-RESP
                                       TO TBL-ASSIGN-AGT (NDX)
                 END-IF
              END-IF
041105     END-IF
              
      * NOTE AGT TYPE 'K' IS ALL OR NOTHING SO ECS010 SHOULD PLUG
      * ZEROS IN THE COMMISSION PCT IF IT IS PASSED THE NO CHGBACK
      * AGT TYPES B I AND L ARE EARNED JUST LIKE REGULAR COMMISSIONS SO
      * WE NEED TO CALC A RATIO HERE

           IF DE-AGT-TYPE (X1) = 'B' OR 'I' OR 'L'
071107                        OR 'J' OR 'K' OR 'N'
011410        IF DE-LF-PRM > ZEROS
011410           COMPUTE L-OW = DE-L-PC (X1) * +1000
111116        end-if
111116        IF DE-AH-PRM > ZEROS
111116           COMPUTE A-OW = DE-A-PC (X1) * +1000
111116        END-IF
011410        IF DE-CANCEL
011410           IF DE-LF-PRM > ZEROS
011410              COMPUTE CNC-FACT = DE-LF-RFND / DE-LF-PRM
011410              COMPUTE L-OW ROUNDED = (CNC-FACT * L-OW) * -1
011410           end-if
011410           IF (DE-AH-PRM > ZEROS)
                    IF (DE-AGT-TYPE (X1) = 'I' OR 'N')
                       and (dte-client = 'DCC')
                       PERFORM 2530-CALC-DDF-FEES THRU 2530-EXIT
                       COMPUTE A-OW = CNC-WK * -1
PEMTST              ELSE
072512                 compute cnc-fact = DE-AH-RFND / DE-AH-PRM
                       IF DE-CANCEL-REASON = 'R'
                          MOVE 1    TO CNC-FACT
                       END-IF
                       if de-cancel-fee > de-ah-rfnd
                          move +0 to cnc-fact
                       end-if
011410                 COMPUTE A-OW ROUNDED =
                         (CNC-FACT * A-OW) * -1
      *                display ' BIJKLN ' DE-ACCOUNT ' '
      *                   DE-CERT-NO ' ' A-OW
                    END-IF
011410           END-IF
              END-IF   
060105        IF (DE-RC-ISSUE)
060105           AND (DE-RECALC-TYPE = 'O')
060105           COMPUTE L-OW = L-OW * -1
060105           COMPUTE A-OW = A-OW * -1
060105        END-IF
           ELSE
100703        IF (DE-AGT-TYPE (X1) = 'M')
011410           AND (CLAS-I-BEN-CATEGORY (CLAS-INDEXA) = 'G' OR 'L')
100703           COMPUTE A-OW = DE-A-PC (X1) * DE-LF-PRM-ALT
                 IF DE-CANCEL
100703              COMPUTE CNC-FACT = DE-AH-RFND / DE-AH-PRM
101409              COMPUTE A-OW ROUNDED = (CNC-FACT * A-OW) * -1
                 END-IF
                 IF (DE-RC-ISSUE)
                     AND (DE-RECALC-TYPE = 'O')
100703               COMPUTE A-OW = A-OW * -1
100703           END-IF
100703        ELSE
011410          IF (DE-AGT-TYPE (X1) = 'A')
011410                    OR
011410             ((DE-AGT-TYPE (X1) = 'O' OR 'P' OR 'M')
011410             AND ((CLAS-I-BEN-CATEGORY (CLAS-INDEXL) = 'D')
011410                OR (CLAS-I-BEN-CATEGORY (CLAS-INDEXA) = 'D')))
011410           COMPUTE L-OW ROUNDED     =
011410                                 DE-REI-LFPRM * DE-L-PC (X1)
011410           IF (DE-CANCEL)
011410              AND (DE-LF-PRM NOT = ZEROS)
011410              COMPUTE CNC-FACT = DE-LF-RFND / DE-LF-PRM
011410              COMPUTE WS-WORK-REF ROUNDED =
011410                 DE-REI-LFPRM * CNC-FACT
011410              COMPUTE L-OW ROUNDED = WS-WORK-REF *
011410                 DE-L-PC (X1)
011410              COMPUTE L-OW = L-OW * -1
011410           END-IF
011410
011410           COMPUTE A-OW ROUNDED     =
011410                                 DE-REI-AHPRM * DE-A-PC (X1)
011410           IF (DE-CANCEL)
011410              AND (DE-AH-PRM NOT = ZEROS)
011410              COMPUTE CNC-FACT = DE-AH-RFND / DE-AH-PRM
011410              COMPUTE A-OW ROUNDED = DE-REI-AHPRM * CNC-FACT *
011410                 DE-A-PC (X1) * -1
011410           END-IF
011410          ELSE
00904            COMPUTE L-OW ROUNDED     =
                                       CP-LF-PRM * DE-L-PC (X1)
00905            COMPUTE L-OW-ALT ROUNDED =
                                       CP-LF-PRM-ALT * DE-L-PC (X1)
00906            COMPUTE A-OW ROUNDED     =
                                       CP-AH-PRM * DE-A-PC (X1)
011410          END-IF
100703        END-IF
           END-IF
00907                                                                   ECS017
00908      IF DE-RC-CANCEL OR DE-RC-ISSUE                               ECS017
00909          IF DTE-CLIENT = 'NCL'                                    ECS017
00910              GO TO 2465-OW-RC-RECORD                              ECS017
00911          ELSE                                                     ECS017
00912              GO TO 2480-ACCUM-RECALC-OW.                          ECS017
00913                                                                   ECS017
00914      IF DTE-CLIENT NOT = 'NCL'                                    ECS017
00915          GO TO 2470-ACCUM-STD-OW.                                 ECS017
00916                                                                   ECS017
00917 *****   NCL ONLY CODE TO OUTPUT DETAIL OVERWRITE ISSUES & CANCELS ECS017
00918                                                                   ECS017
00919      MOVE LOW-VALUES             TO CP-ACCOUNT.                   ECS017
00920      MOVE 'Z'                    TO CP-TRANS.                     ECS017
00921      MOVE  AM-NAME               TO CP-AM-NAME.                   ECS017
00922      MOVE  AM-ACCOUNT            TO CP-AM-NO.                     ECS017
00923      MOVE L-OW                   TO CP-OW-LF-COM                  ECS017
00924                                     CP-OW-LF-COM-BILLED.          ECS017
00925      MOVE L-OW-ALT               TO CP-OW-LF-COM-ALT.             ECS017
00926      MOVE A-OW                   TO CP-OW-AH-COM                  ECS017
00927                                     CP-OW-AH-COM-BILLED.          ECS017
00928      MOVE DE-AGT-TYPE (X1)       TO CP-COM-TYPE.                  ECS017
00929                                                                   ECS017
00930      IF AM-REMIT-TO   =  (X1)                                     ECS017
00931          MOVE DE-AGT (X1)            TO CP-REMIT                  ECS017
00932          MOVE  'Y'                   TO CP-NCL-GA-SWITCH          ECS017
00933      ELSE                                                         ECS017
00934          MOVE DE-AGT (X1)            TO CP-REMIT                  ECS017
00935          MOVE  SPACES                TO CP-NCL-GA-SWITCH.         ECS017
00936                                                                   ECS017
00937      IF DE-GA-BILL-STATUS (X1)  =  'B'                            ECS017
00938          MOVE  'B'                   TO CP-OW-BILL-STATUS         ECS017
00939      ELSE                                                         ECS017
00940          MOVE  SPACES                TO CP-OW-BILL-STATUS.        ECS017
00941                                                                   ECS017
00942      MOVE CP-LF-TYPE                 TO EX-LTYP.                  ECS017
00943      MOVE CP-AH-TYPE                 TO EX-ATYP.                  ECS017
00944                                                                   ECS017
00945      PERFORM 6200-CHECK-OB-RTN THRU 6299-EXIT.                    ECS017
00946                                                                   ECS017
00947      PERFORM 3900-WRT-CCM-RTN THRU 3999-EXIT.                     ECS017
00948      GO TO 2470-ACCUM-STD-OW.                                     ECS017
00949                                                                   ECS017
00950 ***** END OF NCL DETAIL ISSUE & CANCEL CODE                       ECS017
00951                                                                   ECS017
00952 *****   NCL ONLY CODE TO OUTPUT DETAIL OVERWRITE RECALCS          ECS017
00953                                                                   ECS017
00954  2465-OW-RC-RECORD.                                               ECS017
00955                                                                   ECS017
00956      MOVE LOW-VALUES             TO CP-ACCOUNT.                   ECS017
00957      MOVE 'N'                    TO CP-TRANS.                     ECS017
00958      MOVE  AM-NAME               TO CP-AM-NAME.                   ECS017
00959      MOVE  AM-ACCOUNT            TO CP-AM-NO.                     ECS017
00960      MOVE L-OW                   TO CP-OW-LF-COM.                 ECS017
00961      MOVE L-OW-ALT               TO CP-OW-LF-COM-ALT.             ECS017
00962      MOVE A-OW                   TO CP-OW-AH-COM.                 ECS017
00963      MOVE DE-L-PC (X1)           TO CP-LPC.                       ECS017
00964      MOVE DE-A-PC (X1)           TO CP-APC.                       ECS017
00965      MOVE DE-AGT-TYPE (X1)       TO CP-COM-TYPE.                  ECS017
00966                                                                   ECS017
00967      IF AM-REMIT-TO = (X1)                                        ECS017
00968          MOVE DE-AGT (X1)            TO CP-REMIT                  ECS017
00969          MOVE  'Y'                   TO CP-NCL-GA-SWITCH          ECS017
00970      ELSE                                                         ECS017
00971          MOVE DE-AGT (X1)            TO CP-REMIT                  ECS017
00972          MOVE  SPACES                TO CP-NCL-GA-SWITCH.         ECS017
00973                                                                   ECS017
00974      IF DE-GA-BILL-STATUS (X1) = 'B'                              ECS017
00975          MOVE  'B'                   TO CP-OW-BILL-STATUS         ECS017
00976      ELSE                                                         ECS017
00977          MOVE  SPACES                TO CP-OW-BILL-STATUS.        ECS017
00978                                                                   ECS017
00979      MOVE CP-LF-TYPE                 TO EX-LTYP.                  ECS017
00980      MOVE CP-AH-TYPE                 TO EX-ATYP.                  ECS017
00981                                                                   ECS017
00982      PERFORM 6200-CHECK-OB-RTN THRU 6299-EXIT.                    ECS017
00983                                                                   ECS017
00984      IF DE-RECALC-CODE = 'Y' OR '2'                               ECS017
00985          PERFORM 3900-WRT-CCM-RTN THRU 3999-EXIT.                 ECS017
00986                                                                   ECS017
00987      GO TO 2480-ACCUM-RECALC-OW.                                  ECS017
00988                                                                   ECS017
00989 ***** END OF NCL DETAIL RECALC CODE                               ECS017
00990                                                                   ECS017
00991  2470-ACCUM-STD-OW.                                               ECS017
122002
052814     IF CP-LF-TYPE NOT = '00' AND '  ' AND 'DD' and 'CU'
00992         ADD CP-LF-AMT            TO OW-LF-AMT (NDX)
011410        IF CLAS-I-BEN-CATEGORY (CLAS-INDEXL) = 'D'
011410           IF DE-ISSUE
011410              IF NOT DUP-AGENT
011410                 ADD DE-REI-LFPRM   TO OW-LF-PRM (NDX)
011410              END-IF
011410           ELSE
011410              IF DE-CANCEL
011410                 COMPUTE CNC-FACT = DE-LF-RFND / DE-LF-PRM
011410                 COMPUTE WS-WORK-REF ROUNDED =
011410                    DE-REI-LFPRM * CNC-FACT * -1
011410                 ADD WS-WORK-REF TO OW-LF-PRM (NDX)
011410              END-IF
011410           END-IF
011410        ELSE
011410           ADD CP-LF-PRM         TO OW-LF-PRM (NDX)
011410        END-IF
00994         ADD CP-LF-AMT-ALT        TO OW-LF-AMT-ALT (NDX)
      *       IF AM-DCC-PRODUCT-CODE NOT = 'DDF'
00995            ADD CP-LF-PRM-ALT        TO OW-LF-PRM-ALT (NDX)
      *       END-IF
011904     END-IF


011410     IF CP-AH-TYPE NOT = '00' AND '  '
011410        IF CLAS-I-BEN-CATEGORY (CLAS-INDEXA) = 'D'
011410           IF DE-ISSUE
011410              IF NOT DUP-AGENT
011410                 ADD DE-REI-AHPRM   TO OW-AH-PRM (NDX)
011410              END-IF
011410           ELSE
011410              IF DE-CANCEL
011410                 COMPUTE CNC-FACT = DE-AH-RFND / DE-AH-PRM
011410                 COMPUTE WS-WORK-REF ROUNDED =
011410                    DE-REI-AHPRM * CNC-FACT * -1
011410                 ADD WS-WORK-REF TO OW-AH-PRM (NDX)
011410              END-IF
011410           END-IF
011410        END-IF
011410     END-IF


00996      ADD CP-AH-AMT            TO OW-AH-AMT (NDX)
00998      ADD L-OW                 TO OW-LF-COM (NDX)
00999      ADD L-OW-ALT             TO OW-LF-COM-ALT (NDX)
      *    display ' o/w rtn ' cp-account ' ' cp-cert ' ' a-ow
01000      ADD A-OW                 TO OW-AH-COM (NDX)
           MOVE ' '                 TO WS-DONE-SW
011904     IF DE-AGT-TYPE (X1) = 'B' OR 'L' OR 'K' OR 'M'
042209                         OR 'I' OR 'J' OR 'N'
092705        AND (CLAS-I-BEN-CATEGORY (CLAS-INDEXA) = 'G' OR 'L')
              PERFORM VARYING S1 FROM +1 BY +1 UNTIL
                 S1 = X1
                 IF DE-AGT (X1) = DE-AGT (S1)
                    SET ALREADY-DONE TO TRUE
                 END-IF
              END-PERFORM
              IF NOT ALREADY-DONE
                 IF DE-ISSUE
      *             ADD +1.0           TO OW-AH-PRM  (NDX)
                    ADD +1.0           TO OW-AH-CNT  (NDX)
                 ELSE
                    IF DE-CANCEL
                       SUBTRACT +1.0   FROM OW-AH-CNT (NDX)
                    END-IF
                 END-IF
              END-IF
              IF DE-AGT-TYPE (X1) = 'B'
                 ADD DE-AH-RFND        TO OW-AH-RFND (NDX)
              END-IF
           ELSE
011410        IF (CLAS-I-BEN-CATEGORY (CLAS-INDEXA) = 'D')
011410           OR (CLAS-I-BEN-CATEGORY (CLAS-INDEXL) = 'D')
011410           CONTINUE
              ELSE
052306           IF DE-AGT-TYPE (X1) NOT = 'I' AND 'J'
                    ADD CP-AH-PRM         TO OW-AH-PRM (NDX)
                 END-IF
              END-IF
           END-IF

011410     EVALUATE TRUE
011410        WHEN DE-AGT-TYPE (X1) = 'G'
011410           ADD L-OW              TO PRCM-LF-OW-SF
011410           ADD L-OW-ALT          TO PRCM-LF-OW-SF
011410           ADD A-OW              TO PRCM-AH-OW-SF
011410        WHEN DE-AGT-TYPE (X1) = 'I' OR 'J'
011410           ADD L-OW              TO PRCM-DLR-INC
011410           ADD A-OW              TO PRCM-DLR-INC
011410        WHEN DE-AGT-TYPE (X1) = 'L'
011410           ADD A-OW              TO PRCM-AH-LMBA-FEE
011410        WHEN DE-AGT-TYPE (X1) = 'A'
011410           ADD L-OW              TO PRCM-LF-LMBA-FEE
011410           ADD L-OW-ALT          TO PRCM-LF-LMBA-FEE
011410           ADD A-OW              TO PRCM-AH-LMBA-FEE
011410        WHEN DE-AGT-TYPE (X1) = 'B'
011410           ADD A-OW              TO PRCM-BANK-FEE
052814        WHEN DE-AGT-TYPE (X1) = 'N' or 'S'
                 ADD A-OW              TO PRCM-CSO-ADMIN
011410        WHEN OTHER
011410           ADD L-OW              TO PRCM-LF-OW-COMM
011410           ADD L-OW-ALT          TO PRCM-LF-OW-COMM
011410           ADD A-OW              TO PRCM-AH-OW-COMM
011410     END-EVALUATE

01010                                                                   ECS017
01011      IF X1 LESS THAN +6                                           ECS017
01012          IF DE-GA-BILL-STATUS (X1) = 'B'                          ECS017
01013              ADD CP-LF-PRM      TO OW-LF-PRM-B (NDX)              ECS017
01014              ADD CP-AH-PRM      TO OW-AH-PRM-B (NDX)              ECS017
01015              ADD CP-LF-PRM-ALT  TO OW-LF-PRM-B-ALT (NDX)          ECS017
01016              ADD L-OW           TO OW-LF-COM-B (NDX)              ECS017
01017              ADD L-OW-ALT       TO OW-LF-COM-B-ALT (NDX)          ECS017
01018              ADD A-OW           TO OW-AH-COM-B (NDX).             ECS017
01019                                                                   ECS017
01020      GO TO 2440-OW-LOOP.                                          ECS017
01021                                                                   ECS017
01022  2480-ACCUM-RECALC-OW.                                            ECS017
01023                                                                   ECS017
01024      IF DE-AGT-TYPE (X1) = 'G'
01025          ADD L-OW          TO RECALC-LF-OW-SF                     ECS017
01026          ADD L-OW-ALT      TO RECALC-LF-OW-SF                     ECS017
01027          ADD A-OW          TO RECALC-AH-OW-SF                     ECS017
01028      ELSE                                                         ECS017
01029          ADD L-OW          TO RECALC-LF-OW-COMM                   ECS017
01030          ADD L-OW-ALT      TO RECALC-LF-OW-COMM                   ECS017
01031          ADD A-OW          TO RECALC-AH-OW-COMM.                  ECS017
01032                                                                   ECS017
01033      IF DE-RECALC-CODE = 'Y' OR '2'                               ECS017
01034          NEXT SENTENCE                                            ECS017
01035      ELSE                                                         ECS017
01036          GO TO 2440-OW-LOOP.                                      ECS017
01037                                                                   ECS017
01038      ADD CP-LF-AMT     TO OW-LF-AMT-RC     (NDX).                 ECS017
01039      ADD CP-LF-PRM     TO OW-LF-PRM-RC     (NDX).                 ECS017
01040      ADD CP-LF-AMT-ALT TO OW-LF-AMT-RC-ALT (NDX).                 ECS017
01041      ADD CP-LF-PRM-ALT TO OW-LF-PRM-RC-ALT (NDX).                 ECS017
01042      ADD CP-AH-AMT     TO OW-AH-AMT-RC     (NDX).                 ECS017
01043      ADD CP-AH-PRM     TO OW-AH-PRM-RC     (NDX).                 ECS017
01044      ADD L-OW          TO OW-LF-COM-RC     (NDX).                 ECS017
01045      ADD L-OW-ALT      TO OW-LF-COM-RC-ALT (NDX).                 ECS017
01046      ADD A-OW          TO OW-AH-COM-RC     (NDX).                 ECS017
01047                                                                   ECS017
01048      GO TO 2440-OW-LOOP.                                          ECS017
01049                                                                   ECS017
01050  2490-OW-L-X.                                                     ECS017
01051 * PRODUCER LEVEL RECORDS.                                         ECS017
01052                                                                   ECS017
01053      IF DE-ISSUE OR DE-CANCEL                                     ECS017
01054          NEXT SENTENCE                                            ECS017
01055      ELSE                                                         ECS017
01056          GO TO 2495-RECALC.                                       ECS017
01057                                                                   ECS017
01058      IF DTE-CLIENT = 'NCL'                                        ECS017
01059         MOVE  SAVE-CPTR        TO CP-RECORD.                      ECS017
01060                                                                   ECS017
01061      PERFORM 5200-BUILD-RECALC THRU 5299-EXIT.                    ECS017
01062                                                                   ECS017
01063      PERFORM 3900-WRT-CCM-RTN    THRU 3999-EXIT.                  ECS017
01064                                                                   ECS017
01065      IF NOT DE-REVERSE                                            ECS017
01066          GO TO 2499-EXIT.                                         ECS017
01067                                                                   ECS017
01068      IF DE-CANCEL                                                 ECS017
01069          GO TO 2492-CANCEL.                                       ECS017
01070                                                                   ECS017
01071      PERFORM 2000-FORMAT-RECORD-RTN THRU 2099-EXIT.               ECS017
01072                                                                   ECS017
01073      PERFORM 2200-CALC-REMIT-TO-RTN THRU 2299-EXIT.               ECS017
01074                                                                   ECS017
01075      PERFORM 2500-REV-ACCT-COMM-RTN THRU 2500-XIT.                ECS017
01076                                                                   ECS017
01077      IF DE-BILLED-LFPRM GREATER THAN ZEROS                        ECS017
01078          COMPUTE CP-LF-AMT = CP-LF-AMT * -1                       ECS017
01079          COMPUTE CP-LF-PRM = DE-BILLED-LFPRM * -1                 ECS017
01080          COMPUTE CP-LF-COM ROUNDED = CP-LF-PRM * CP-LPC           ECS017
01081      ELSE                                                         ECS017
01082          MOVE ZEROS              TO CP-LF-PRM  CP-LF-COM          ECS017
01083                                     CP-LF-PRM-ALT  CP-LF-COM-ALT. ECS017
01084                                                                   ECS017
01085      IF DE-BILLED-AHPRM GREATER THAN ZEROS                        ECS017
01086          COMPUTE CP-AH-PRM         = DE-BILLED-AHPRM * -1         ECS017
01087          COMPUTE CP-AH-COM ROUNDED = CP-AH-PRM * CP-APC           ECS017
01088      ELSE                                                         ECS017
01089          MOVE ZEROS              TO CP-AH-PRM  CP-AH-COM.         ECS017
01090                                                                   ECS017
01091      PERFORM 3900-WRT-CCM-RTN THRU 3999-EXIT.                     ECS017
01092                                                                   ECS017
01093      GO TO 2499-EXIT.                                             ECS017
01094                                                                   ECS017
01095  2492-CANCEL.                                                     ECS017
01096      PERFORM 2000-FORMAT-RECORD-RTN THRU 2099-EXIT.               ECS017
01097                                                                   ECS017
01098      PERFORM 2200-CALC-REMIT-TO-RTN THRU 2299-EXIT.               ECS017
01099                                                                   ECS017
01100      PERFORM 2500-REV-ACCT-COMM-RTN THRU 2500-XIT.                ECS017
01101                                                                   ECS017
01102      IF DE-BILLED-LFRFND GREATER THAN ZEROS                       ECS017
01103          COMPUTE CP-LF-AMT = CP-LF-AMT * -1                       ECS017
01104          COMPUTE CP-LF-PRM = DE-BILLED-LFRFND * -1                ECS017
01105          COMPUTE CP-LF-COM ROUNDED = CP-LF-PRM * CP-LPC           ECS017
01106      ELSE                                                         ECS017
01107          MOVE ZEROS              TO CP-LF-PRM  CP-LF-COM          ECS017
01108                                     CP-LF-PRM-ALT  CP-LF-COM-ALT. ECS017
01109                                                                   ECS017
01110      IF DE-BILLED-AHRFND GREATER THAN ZEROS                       ECS017
01111          COMPUTE CP-AH-PRM         = DE-BILLED-AHRFND * -1        ECS017
01112          COMPUTE CP-AH-COM ROUNDED = CP-AH-PRM * CP-APC           ECS017
01113      ELSE                                                         ECS017
01114          MOVE ZEROS              TO CP-AH-PRM  CP-AH-COM.         ECS017
01115                                                                   ECS017
01116      PERFORM 3900-WRT-CCM-RTN THRU 3999-EXIT.                     ECS017
01117                                                                   ECS017
01118      GO TO 2499-EXIT.                                             ECS017
01119                                                                   ECS017
01120  2495-RECALC.                                                     ECS017
01121 * RECALCULATED PRODUCER LEVEL RECORDS.                            ECS017
01122                                                                   ECS017
01123      IF DTE-CLIENT = 'NCL'                                        ECS017
01124         MOVE  SAVE-CPTR        TO CP-RECORD.                      ECS017
01125                                                                   ECS017
01126      PERFORM 5200-BUILD-RECALC THRU 5299-EXIT.                    ECS017
01127                                                                   ECS017
01128      IF DE-RECALC-CODE = 'Y' OR '2'                               ECS017
01129          PERFORM 3900-WRT-CCM-RTN THRU 3999-EXIT.                 ECS017
01130                                                                   ECS017
01131  2499-EXIT.                                                       ECS017
01132      EXIT.                                                        ECS017
01133  EJECT                                                            ECS017
01134  2500-REV-ACCT-COMM-RTN.                                          ECS017
01135      MOVE +0                     TO X1.                           ECS017
01136      MOVE +1                     TO NDX.                          ECS017
01137                                                                   ECS017
01138  2500-REV-ACCT-COMM-LOOP.                                         ECS017
01139      ADD +1 TO X1.                                                ECS017
01140                                                                   ECS017
01141      IF X1 GREATER THAN +10                                       ECS017
01142          MOVE 0                  TO X1                            ECS017
01143          GO TO 2500-XIT.                                          ECS017
01144                                                                   ECS017
052814     IF DE-AGT-TYPE (X1) NOT = 'C' AND 'D' AND 'F'
01146          GO TO 2500-REV-ACCT-COMM-LOOP.                           ECS017
01147                                                                   ECS017
01148      ADD DE-L-PC (X1) TO CP-LPC.                                  ECS017
01149      ADD DE-A-PC (X1) TO CP-APC.                                  ECS017
01150                                                                   ECS017
01151      GO TO 2500-REV-ACCT-COMM-LOOP.                               ECS017
01152                                                                   ECS017
01153  2500-XIT.                                                        ECS017
01154      EXIT.                                                        ECS017

       2520-REF-CSO-ADMIN-FEE.

           IF DE-CERT-NO = WS-FACT-CERT
              CONTINUE
           ELSE
              PERFORM 2550-GET-DDF-FACTORS THRU 2550-EXIT
           END-IF

           DISPLAY ' MADE IT TO 2520 ' DE-CERT-NO ' ' DE-AH-TERM ' '
             DE-DCC-DDF-REM-TRM3 ' ' A-OW ' ' WS-HI-UEF ' ' WS-LO-UEF

           MOVE ZEROS                  TO CNC-WK

           EVALUATE TRUE
072512        when de-ah-rfnd = de-ah-prm
072512           move a-ow             to cnc-wk
              WHEN (DE-DCC-DDF-REM-TRM3 = 0)
                 OR (DE-CANCEL-REASON = 'R')
041414           or (a-ow = zeros)
041414           DISPLAY ' REM TERM = 0 OR REPO or a-ow = 0 '
                 MOVE 0                TO CNC-WK

              WHEN DE-DCC-DDF-REM-TRM3 = DE-AH-TERM
                 DISPLAY ' REM TERM  = ORIG TERM '
                 MOVE A-OW             TO CNC-WK

              WHEN (DE-AH-TERM - DE-DCC-DDF-REM-TRM3) < 4
                 DISPLAY ' ORIG TERM - REM TRM < 4 '
                 COMPUTE W-FACTOR ROUNDED = A-OW -
                    (DD-1ST-YR-ALLOW (D1)
                    * WS-MONTH / 3) - (A-OW - DD-1ST-YR-ALLOW (D1))
                    * (1 - WS-HI-UEF) * WS-MONTH / 12
                 COMPUTE CNC-WK = W-FACTOR * 1

      **** (AdminFee-Yr1 AF)-((AdminFee- Yr1 AF)*(1 - UEF1)*mo/12)

              WHEN (DE-AH-TERM - DE-DCC-DDF-REM-TRM3) < 13
                 DISPLAY ' ORIG TERM - REM TRM < 13 '
                 COMPUTE W-FACTOR ROUNDED = (a-ow -
                    DD-1ST-YR-ALLOW (D1))
                    - (a-ow - DD-1ST-YR-ALLOW (D1))
                    * (1 - WS-hi-UEF) * ws-MONTH / 12
                 COMPUTE CNC-WK = W-FACTOR * 1

      ****  (AdminFee-Yr1 AF)*{UEF1 - (UEF1 -UEF2)*mo/12}

              WHEN OTHER
                 DISPLAY ' OTHER '
                 COMPUTE W-FACTOR ROUNDED = (a-ow -
                    DD-1ST-YR-ALLOW (D1))
                    * (WS-HI-UEF - (WS-HI-UEF - WS-LO-UEF)
                    * ws-MONTH / 12)
                 COMPUTE CNC-WK = W-FACTOR * 1
           END-EVALUATE

           DISPLAY ' CSO REF ADM FEE ' DE-CERT-NO ' ' CNC-WK

           .
       2520-EXIT.
           EXIT.

       2530-CALC-DDF-FEES.

      *****   THIS PARA IS ONLY FOR COMM TYPES N AND I
      *****   DCC DDF REFUNDS ONLY

           IF DE-CERT-NO = WS-FACT-CERT
              CONTINUE
           ELSE
              PERFORM 2550-GET-DDF-FACTORS THRU 2550-EXIT
           END-IF

           COMPUTE WS-MONTH =
              FUNCTION REM(DE-AH-TERM - DE-DCC-DDF-REM-TRM3, 12)
           IF WS-MONTH = 0
              MOVE 12 TO WS-MONTH
           END-IF
           DISPLAY ' WS MONTH ' WS-MONTH

           EVALUATE TRUE
              WHEN DE-AGT-TYPE (X1) = 'N'
                 PERFORM 2520-REF-CSO-ADMIN-FEE
                                       THRU 2520-EXIT
              WHEN (DE-AGT-TYPE (X1) = 'I')
                 AND (DE-CANCEL-REASON = 'R')
                    MOVE A-OW TO CNC-WK
              WHEN (DE-AGT-TYPE (X1) = 'I')
                 EVALUATE TRUE
                    WHEN ((DE-AH-TERM - DE-DCC-DDF-REM-TRM3) < 4)
                       OR (NOT DD-IU-PRESENT)
                       COMPUTE CNC-FACT = DE-REI-AHRFND / DE-REI-AHPRM
                       COMPUTE CNC-WK ROUNDED = CNC-FACT * A-OW
                     WHEN ((DE-AH-TERM - DE-DCC-DDF-REM-TRM3) < 13)
                        AND (DD-IU-PRESENT)
                        COMPUTE WS-CLP-MO3 = DE-REI-AHPRM -
                           (DE-REI-AHPRM -
                           DE-IU-RATE-UP) * (WS-HI-UEF -
                           WS-LO-UEF) * 3 / 12
                        DISPLAY ' UECLPMO3 ' WS-CLP-MO3
                        COMPUTE CNC-FACT = WS-CLP-MO3 / DE-REI-AHPRM
                        COMPUTE WS-COMM-MO3 ROUNDED = CNC-FACT * A-OW
                        DISPLAY ' UEMGTFO3 ' WS-COMM-MO3
                        COMPUTE CNC-WK = WS-COMM-MO3 - (WS-COMM-MO3 -
                          A-OW * WS-HI-UEF) * (WS-MONTH - 3) / 9
                    WHEN (DE-AH-TERM - DE-DCC-DDF-REM-TRM3) > 12
                       COMPUTE CNC-WK = A-OW *
                          (WS-HI-UEF - (WS-HI-UEF - WS-LO-UEF) *
                          WS-MONTH / 12)
                 END-EVALUATE
                 DISPLAY ' CSO REF MGT FEE ' DE-CERT-NO ' ' CNC-WK
           END-EVALUATE

           .
       2530-EXIT.
           EXIT.

       2540-CALC-DDF-COMM.

           IF DE-CERT-NO = WS-FACT-CERT
              DISPLAY ' FACTORS ALREADY BUILT IN 2540 ' DE-CERT-NO
           ELSE
              PERFORM 2550-GET-DDF-FACTORS THRU 2550-EXIT
           END-IF

           COMPUTE WS-MONTH =
              FUNCTION REM(DE-AH-TERM - DE-DCC-DDF-REM-TRM3, 12)
           IF WS-MONTH = 0
              MOVE 12 TO WS-MONTH
           END-IF
           DISPLAY ' WS MONTH ' WS-MONTH

           EVALUATE TRUE
              WHEN DE-CANCEL-REASON = 'R'
                 MOVE A-OW             TO CNC-WK
              WHEN ((DE-AH-TERM - DE-DCC-DDF-REM-TRM3) < 4)
                 OR (NOT DD-IU-PRESENT)
                    COMPUTE CNC-FACT = DE-REI-AHRFND / DE-REI-AHPRM
                    COMPUTE CNC-WK = CNC-FACT * A-OW
              WHEN (DE-AH-TERM - DE-DCC-DDF-REM-TRM3) < 13
                  COMPUTE WS-CLP-MO3 = DE-REI-AHPRM - (DE-REI-AHPRM -
                     DE-IU-RATE-UP) * (WS-HI-UEF -
                     WS-LO-UEF) * 3 / 12
                  DISPLAY ' UECLPMO3 ' WS-CLP-MO3
                  COMPUTE CNC-FACT = WS-CLP-MO3 / DE-REI-AHPRM
                  COMPUTE WS-COMM-MO3 ROUNDED = CNC-FACT * A-OW
                  DISPLAY ' UECOMMO3 ' WS-COMM-MO3
                  COMPUTE CNC-WK = WS-COMM-MO3 - (WS-COMM-MO3 -
                    A-OW * WS-HI-UEF) * (WS-MONTH - 3) / 9
              WHEN (DE-AH-TERM - DE-DCC-DDF-REM-TRM3) > 12
                 COMPUTE CNC-WK = a-ow * (WS-HI-UEF - (WS-HI-UEF -
                    WS-LO-UEF) * ws-MONTH / 12)
           END-EVALUATE

           DISPLAY ' CSO REF comm    ' DE-CERT-NO ' ' CNC-WK

           .
       2540-EXIT.
           EXIT.

       2550-GET-DDF-FACTORS.

           MOVE ZEROS                  TO CNC-WK
                                          WS-LO-UEF WS-HI-UEF
           move ' '                    to dd-iu-sw

051414     if de-clp-state = spaces
051414        move de-state            to de-clp-state
051414     end-if
           PERFORM VARYING D1 FROM +1 BY +1 UNTIL
051414        ((DE-clp-STATE = DD-STATE (D1))
              AND (AM-DCC-PRODUCT-CODE = DD-PRODUCT-CD (D1))
              AND ('A'           = DD-BEN-TYPE   (D1))
              AND (DE-AH-TYPE    = DD-BEN-CODE   (D1))
              AND (DE-EFF        < DD-PROD-EXP-DT (D1)))
                              OR
100413        D1 > max-pdef
           END-PERFORM
100413     IF D1 > max-pdef
              DISPLAY ' COULD NOT FIND UEP FACTORS '
              GO TO 2550-EXIT
           END-IF

           PERFORM VARYING P1 FROM +1 BY +1 UNTIL
032922        (P1 > +11)
              OR (DD-PROD-CODE (D1 P1) = 'I')
           END-PERFORM
032922     IF P1 < +12
              display ' setting iu present to true '
              SET DD-IU-PRESENT        TO TRUE
           END-IF

           MOVE DE-AH-TERM             TO WS-DDF-TERM
           IF (DE-LOAN-TERM > WS-DDF-TERM)
              AND (DD-TRUNCATED (D1) = 'Y')
              MOVE DE-LOAN-TERM        TO WS-DDF-TERM
              DISPLAY ' FOUND TRUNCATED ' DE-CERT-NO
           END-IF

           EVALUATE TRUE
              WHEN WS-DDF-TERM > +168
                 MOVE 15               TO P1
              WHEN WS-DDF-TERM > +156
                 MOVE 14               TO P1
              WHEN WS-DDF-TERM > +144
                 MOVE 13               TO P1
              WHEN WS-DDF-TERM > +132
                 MOVE 12               TO P1
              WHEN WS-DDF-TERM > +120
                 MOVE 11               TO P1
              WHEN WS-DDF-TERM > +108
                 MOVE 10               TO P1
              WHEN WS-DDF-TERM > +96
                 MOVE 9                TO P1
              WHEN WS-DDF-TERM > +84
                 MOVE 8                TO P1
              WHEN WS-DDF-TERM > +72
                 MOVE 7                TO P1
              WHEN WS-DDF-TERM > +60
                 MOVE 6                TO P1
              WHEN WS-DDF-TERM > +48
                 MOVE 5                TO P1
              WHEN WS-DDF-TERM > +36
                 MOVE 4                TO P1
              WHEN WS-DDF-TERM > +24
                 MOVE 3                TO P1
              WHEN WS-DDF-TERM > +12
                 MOVE 2                TO P1
              WHEN OTHER
                 MOVE 1                TO P1
           END-EVALUATE

           EVALUATE TRUE
      *       WHEN ((DE-AH-TERM - DE-DCC-DDF-REM-TRM3) < +13)
      *          and (dd-iu-present)
      *          MOVE 2                TO P2
      *       WHEN (DE-AH-TERM - DE-DCC-DDF-REM-TRM3) < +13
      *          MOVE 1                TO P2
              WHEN (DE-AH-TERM - DE-DCC-DDF-REM-TRM3) < +25
                 MOVE 2                TO P2
              WHEN (DE-AH-TERM - DE-DCC-DDF-REM-TRM3) < +37
                 MOVE 3                TO P2
              WHEN (DE-AH-TERM - DE-DCC-DDF-REM-TRM3) < +49
                 MOVE 4                TO P2
              WHEN (DE-AH-TERM - DE-DCC-DDF-REM-TRM3) < +61
                 MOVE 5                TO P2
              WHEN (DE-AH-TERM - DE-DCC-DDF-REM-TRM3) < +73
                 MOVE 6                TO P2
              WHEN (DE-AH-TERM - DE-DCC-DDF-REM-TRM3) < +85
                 MOVE 7                TO P2
              WHEN (DE-AH-TERM - DE-DCC-DDF-REM-TRM3) < +97
                 MOVE 8                TO P2
              WHEN (DE-AH-TERM - DE-DCC-DDF-REM-TRM3) < +109
                 MOVE 9                TO P2
              WHEN (DE-AH-TERM - DE-DCC-DDF-REM-TRM3) < +121
                 MOVE 10               TO P2
              WHEN (DE-AH-TERM - DE-DCC-DDF-REM-TRM3) < +133
                 MOVE 11               TO P2
              WHEN (DE-AH-TERM - DE-DCC-DDF-REM-TRM3) < +145
                 MOVE 12               TO P2
              WHEN (DE-AH-TERM - DE-DCC-DDF-REM-TRM3) < +157
                 MOVE 13               TO P2
              WHEN (DE-AH-TERM - DE-DCC-DDF-REM-TRM3) < +169
                 MOVE 14               TO P2
              WHEN OTHER
                 MOVE 15               TO P2
           END-EVALUATE

           MOVE DE-CERT-NO             TO WS-FACT-CERT
           MOVE DD-UEP-FACTOR (D1 P1 P2 + 1)
                                       TO WS-LO-UEF
           MOVE DD-UEP-FACTOR (D1 P1 P2)
                                       TO WS-HI-UEF

           .
       2550-EXIT.
           EXIT.

01156  2600-ACCT-BREAK-RTN.                                             ECS017
01157                                                                   ECS017
01158      MOVE +0                     TO X1.                           ECS017
           .
01159                                                                   ECS017
01160  2620-BUMP-LOOP.                                                  ECS017
01161      ADD +1 TO X1.                                                ECS017
01162                                                                   ECS017
01163      IF X1 GREATER THAN +20 OR                                    ECS017
01164         TBL-AGT (X1) = ZEROS                                      ECS017
01165          GO TO 2690-END-ACCOUNT.                                  ECS017
01166                                                                   ECS017
01167  2640-CALC-GA-OW.                                                 ECS017
01168      MOVE SPACES                 TO CP-RECORD.                    ECS017
01169      MOVE DTE-CLASIC-COMPANY-CD  TO CP-COMPANY-CD.                ECS017
01170      MOVE '%%'                   TO CP-RECORD-ID.                 ECS017
01171      MOVE AM-CARRIER             TO CP-CARRIER.                   ECS017
01172      MOVE AM-GROUPING            TO CP-GROUPING.                  ECS017
01173      MOVE LOW-VALUES             TO CP-ACCOUNT.                   ECS017
01174      MOVE TBL-AGT (X1)           TO CP-REMIT.                     ECS017
01175      MOVE TBL-COM-TYP (X1)       TO CP-COM-TYPE.                  ECS017
01176      MOVE 'Z'                    TO CP-TRANS.                     ECS017
01177      MOVE AM-STATE               TO CP-STATE.                     ECS017
01178      MOVE SAVE-NCL-REGION        TO CP-NCL-REGION.                ECS017
01179      MOVE SAVE-NCL-POOL-CODE     TO CP-NCL-POOL-CODE.             ECS017

041105     IF (TBL-COM-TYP (X1) = 'B' OR 'R')
041105        AND TBL-BK-NAME (X1) NOT = ZEROS
041105        MOVE TBL-BK-NAME (X1)    TO CP-AM-NAME
              MOVE TBL-ASSIGN-AGT (X1) TO CP-AM-NO
041105     ELSE
01180         MOVE AM-NAME             TO CP-AM-NAME
              MOVE AM-ACCOUNT          TO CP-AM-NO
041105     END-IF

01181      MOVE OW-LF-AMT (X1)         TO CP-OW-LF-AMT.                 ECS017
           MOVE OW-LF-PRM (X1)         TO CP-OW-LF-PRM

01183      MOVE OW-LF-COM (X1)         TO CP-OW-LF-COM.                 ECS017
01184      MOVE OW-LF-AMT-ALT (X1)     TO CP-OW-LF-AMT-ALT.             ECS017
01185      MOVE OW-LF-PRM-ALT (X1)     TO CP-OW-LF-PRM-ALT.             ECS017
01186      MOVE OW-LF-COM-ALT (X1)     TO CP-OW-LF-COM-ALT.             ECS017
01187      MOVE OW-AH-AMT (X1)         TO CP-OW-AH-AMT.                 ECS017
           MOVE OW-AH-PRM (X1)         TO CP-OW-AH-PRM

01189      MOVE OW-AH-COM (X1)         TO CP-OW-AH-COM.                 ECS017
011904     MOVE OW-AH-RFND (X1)        TO CP-OW-AH-RFND
01190      MOVE OW-LF-PRM-B (X1)       TO CP-OW-LF-PRM-BILLED.          ECS017
01191      MOVE OW-LF-COM-B (X1)       TO CP-OW-LF-COM-BILLED.          ECS017
01192      ADD  OW-LF-PRM-B-ALT (X1)   TO CP-OW-LF-PRM-BILLED.          ECS017
01193      ADD  OW-LF-COM-B-ALT (X1)   TO CP-OW-LF-COM-BILLED.          ECS017
01194      MOVE OW-AH-BILLED (X1)      TO CP-AH-OW-BILLED.              ECS017
01195 *    MOVE AM-ACCOUNT             TO CP-AM-NO.                     ECS017
01196      MOVE TBL-GL-CODE (X1)       TO CP-GL-CODE.                   ECS017
01197      MOVE AM-GPCD                TO CP-GPCD.
011410     MOVE OW-LF-CNT (X1)         TO CP-OW-LF-CNT
011410     MOVE OW-AH-CNT (X1)         TO CP-OW-AH-CNT
01198      MOVE CP-OW-RECORD           TO SAVE-OW.                      ECS017
01199                                                                   ECS017
01200      IF OW-LF-PRM-B (X1) GREATER THAN ZEROS OR                    ECS017
01201         OW-AH-PRM-B (X1) GREATER THAN ZEROS                       ECS017
01202          MOVE 'B'                TO CP-OW-BILL-STATUS.            ECS017
01203                                                                   ECS017
01204      IF DTE-CLIENT = 'ACM'                                        ECS017
01205          IF X1 LESS THAN +6                                       ECS017
01206              MOVE TBL-ASSIGN-AGT (X1)                             ECS017
01207                                  TO CP-ASSIGN-AGT.                ECS017
01208                                                                   ECS017
01209      IF DTE-CLIENT = 'NCL'                                        ECS017
01210         GO TO 2640-WRITE-NCL-RC-OW.                               ECS017
01211                                                                   ECS017
01212      PERFORM 3900-WRT-CCM-RTN THRU 3999-EXIT.                     ECS017
01213                                                                   ECS017
01214  2640-WRITE-NCL-RC-OW.                                            ECS017
01215                                                                   ECS017
01216      IF OW-LIFE-RC (X1) NOT = TEST-IS-ZERO-1 OR                   ECS017
01217         OW-AH-RC   (X1) NOT = TEST-IS-ZERO-2                      ECS017
01218          MOVE SAVE-OW                TO CP-OW-RECORD              ECS017
01219          MOVE OW-LF-AMT-RC (X1)      TO CP-OW-LF-AMT              ECS017
01220          MOVE OW-LF-PRM-RC (X1)      TO CP-OW-LF-PRM              ECS017
01221          MOVE OW-LF-COM-RC (X1)      TO CP-OW-LF-COM              ECS017
01222          MOVE OW-LF-AMT-RC-ALT (X1)  TO CP-OW-LF-AMT-ALT          ECS017
01223          MOVE OW-LF-PRM-RC-ALT (X1)  TO CP-OW-LF-PRM-ALT          ECS017
01224          MOVE OW-LF-COM-RC-ALT (X1)  TO CP-OW-LF-COM-ALT          ECS017
01225          MOVE OW-AH-AMT-RC (X1)      TO CP-OW-AH-AMT              ECS017
01226          MOVE OW-AH-PRM-RC (X1)      TO CP-OW-AH-PRM              ECS017
01227          MOVE OW-AH-COM-RC (X1)      TO CP-OW-AH-COM              ECS017
01228          MOVE '6'                    TO CP-TRANS                  ECS017
01229          PERFORM 3900-WRT-CCM-RTN THRU 3999-EXIT.                 ECS017
01230                                                                   ECS017
01231      GO TO 2620-BUMP-LOOP.                                        ECS017
01232                                                                   ECS017
01233  2690-END-ACCOUNT.                                                ECS017
01234      PERFORM 2800-CLEAR-TOTALS-RTN THRU 2899-EXIT.                ECS017
01235                                                                   ECS017
01236  2699-EXIT.                                                       ECS017
01237      EXIT.                                                        ECS017
01238  EJECT                                                            ECS017
       2700-GET-BANK-NAME.
       
           MOVE DTE-CLASIC-COMPANY-CD  TO WS-ERCOMP-COMP-CD
           MOVE DE-CARRIER             TO WS-ERCOMP-CARRIER
           MOVE DE-GROUPING            TO WS-ERCOMP-GROUPING
           MOVE WS-ERCOMP-KEY          TO CO-CONTROL-PRIMARY
           
           READ ERCOMP
           IF ERCOMP-FILE-STATUS NOT = '00'
              DISPLAY ' BAD READ ON ERCOMP ' ERCOMP-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF
           .
       2700-EXIT.
           EXIT.



01239  2800-CLEAR-TOTALS-RTN.                                           ECS017
01240      MOVE ZEROS                  TO OW-LF-AMT (1)                 ECS017
01241                                     OW-LF-AMT-RC (1)              ECS017
01242                                     OW-LF-PRM-B (1)               ECS017
01243                                     OW-LF-AMT-ALT (1)             ECS017
01244                                     OW-LF-AMT-RC-ALT (1)          ECS017
01245                                     OW-LF-PRM-B-ALT (1)           ECS017
01246                                     OW-LF-PRM (1)                 ECS017
01247                                     OW-LF-COM (1)                 ECS017
01248                                     OW-LF-PRM-ALT (1)             ECS017
01249                                     OW-LF-COM-ALT (1)             ECS017
01250                                     OW-AH-AMT (1)                 ECS017
01251                                     OW-AH-PRM (1)                 ECS017
01252                                     OW-AH-COM (1)                 ECS017
011904                                    OW-AH-RFND (1)
01253                                     OW-LF-PRM-RC (1)              ECS017
01254                                     OW-LF-COM-RC (1)              ECS017
01255                                     OW-LF-PRM-RC-ALT (1)          ECS017
01256                                     OW-LF-COM-RC-ALT (1)          ECS017
01257                                     OW-AH-AMT-RC (1)              ECS017
01258                                     OW-AH-PRM-RC (1)              ECS017
01259                                     OW-AH-COM-RC (1)              ECS017
01260                                     OW-LF-COM-B (1)               ECS017
01261                                     OW-LF-COM-B-ALT (1)           ECS017
01262                                     OW-AH-PRM-B (1)               ECS017
01263                                     OW-AH-COM-B (1)
011410                                    OW-LF-CNT (1)
011410                                    OW-AH-CNT (1).
01264                                                                   ECS017
01265      MOVE B2                     TO X1.                           ECS017
01266                                                                   ECS017
01267  2820-CLEAR-LOOP.                                                 ECS017
01268      MOVE OW-TOTALS (1)          TO OW-TOTALS (X1).               ECS017
01269      ADD B1 TO X1.                                                ECS017
01270                                                                   ECS017
01271      IF X1 NOT = +21                                              ECS017
01272          GO TO 2820-CLEAR-LOOP.                                   ECS017
01273                                                                   ECS017
01274      MOVE ZEROS                  TO X-AGENTS.                     ECS017
01275                                                                   ECS017
01276  2899-EXIT.                                                       ECS017
01277      EXIT.                                                        ECS017
01278                                                                   ECS017
01279  EJECT                                                            ECS017
01280  3000-WRT-CCM-CLAIM-RTN.                                          ECS017
01281      MOVE '99'                   TO CP-RECORD-ID.                 ECS017
01282                                                                   ECS017
01419                                                                   ECS017
01283      IF DE-DTH OR DE-OB-DTH                                       ECS017
01284          MOVE ZEROS              TO CP-AH-TYPE                    ECS017
01285          MOVE DE-CLAIM-AMT       TO CP-CLM-LF-AMT                 ECS017
01286      ELSE                                                         ECS017
01287          MOVE ZEROS              TO CP-LF-TYPE                    ECS017
01288          MOVE DE-CLAIM-AMT       TO CP-CLM-AH-AMT.                ECS017
01289                                                                   ECS017
01290      PERFORM 3900-WRT-CCM-RTN THRU 3999-EXIT.                     ECS017
01291                                                                   ECS017
01292  3099-EXIT.                                                       ECS017
01293      EXIT.                                                        ECS017
01294                                                                   ECS017
01295  EJECT                                                            ECS017
01296  3900-WRT-CCM-RTN.                                                ECS017
01297                                                                   ECS017
01298      IF DE-CLAIM                                                  ECS017
01299          GO TO 3905-WRT-CCM-RTN.                                  ECS017
01300                                                                   ECS017
01301      IF CP-COMPENSATION                                           ECS017
01302          IF CP-OVERWT OR                                          ECS017
01303             CP-RC-OVERWT                                          ECS017
01304              GO TO 3905-WRT-CCM-RTN.                              ECS017
01305                                                                   ECS017
01306      MOVE SPACES                 TO CP-ALT-RECORD-CONTROL         ECS017
01307                                     CP-ALT-BILLING-DATA.          ECS017
01308                                                                   ECS017
01309      MOVE DE-CARRIER             TO CP-ALT-CARRIER.               ECS017
01310      MOVE DE-GROUPING            TO CP-ALT-COMPANY.               ECS017
01311      MOVE DE-STATE               TO CP-ALT-STATE.                 ECS017
01312      MOVE DE-ACCOUNT             TO CP-ALT-ACCOUNT.               ECS017
01313      MOVE DE-EFF                 TO CP-ALT-EFF-DATE.              ECS017
01314      MOVE DE-CERT                TO CP-ALT-CERT-NO.               ECS017
01315      MOVE DE-TRANS               TO CP-ALT-RECORD-TYPE.           ECS017
01316      MOVE SPACES                 TO CP-ALT-CLAIM-CNTRL.           ECS017
01317      MOVE DE-BILL-SW             TO CP-ALT-RECORD-BILL.           ECS017
01318      MOVE DE-REFUND-SW           TO CP-ALT-REFUND-SW.             ECS017
01319                                                                   ECS017
01320  3905-WRT-CCM-RTN.                                                ECS017

020206     IF CP-TRANS NOT = 'Z' AND '6'
122002         MOVE DE-ENTRY-STATUS    TO CP-ENTRY-STATUS
070803     END-IF.

01321      MOVE DTE-PGM-OPT TO WS-DTE-PGM-OPT.                             CL*15
01322      GO TO 3910-OPTION-1                                          ECS017
01323            3920-OPTION-2                                          ECS017
01324            3930-OPTION-3                                          ECS017
01325            3940-OPTION-4                                          ECS017
01326            3950-OPTION-5                                          ECS017
01327            3960-OPTION-6                                          ECS017
01328            3970-OPTION-7                                          ECS017
01329            3980-OPTION-8                                          ECS017
01330                DEPENDING ON WS-DTE-PGM-OPT.                          CL*16
01331                                                                   ECS017
01332  3910-OPTION-1.                                                   ECS017
01333 * SELECT CLAIMS - COMPENSATION - OVERWRITE.                       ECS017
01334 * DROP          -              -          .                       ECS017
01335                                                                   ECS017
01336      GO TO 3990-WRT-CCM.                                          ECS017
01337                                                                   ECS017
01338  3920-OPTION-2.                                                   ECS017
01339 * SELECT CLAIMS - COMPENSATION -          .                       ECS017
01340 * DROP          -              - OVERWRITE.                       ECS017
01341                                                                   ECS017
01342      IF CP-OVERWT OR                                              ECS017
01343         CP-RC-OVERWT                                              ECS017
01344          GO TO 3999-EXIT.                                         ECS017
01345                                                                   ECS017
01346      GO TO 3990-WRT-CCM.                                          ECS017
01347                                                                   ECS017
01348  3930-OPTION-3.                                                   ECS017
01349 * SELECT CLAIMS -              - OVERWRITE.                       ECS017
01350 *  DROP         - COMPENSATION -          .                       ECS017
01351                                                                   ECS017
01352      IF CP-ISSUE    OR                                            ECS017
01353         CP-CANCEL   OR                                            ECS017
01354         CP-RC-ISSUE OR                                            ECS017
01355         CP-RC-CANCEL                                              ECS017
01356          GO TO 3999-EXIT.                                         ECS017
01357                                                                   ECS017
01358      GO TO 3990-WRT-CCM.                                          ECS017
01359                                                                   ECS017
01360  3940-OPTION-4.                                                   ECS017
01361 * SELECT CLAIMS -              -          .                       ECS017
01362 * DROP          - COMPENSATION - OVERWRITE.                       ECS017
01363                                                                   ECS017
01364      IF CP-CLAIM                                                  ECS017
01365          GO TO 3990-WRT-CCM.                                      ECS017
01366                                                                   ECS017
01367      GO TO 3999-EXIT.                                             ECS017
01368                                                                   ECS017
01369  3950-OPTION-5.                                                   ECS017
01370 * SELECT        - COMPENSATION - OVERWRITE.                       ECS017
01371 * DROP   CLAIMS -              -          .                       ECS017
01372                                                                   ECS017
01373      IF CP-CLAIM                                                  ECS017
01374          GO TO 3999-EXIT.                                         ECS017
01375                                                                   ECS017
01376      GO TO 3990-WRT-CCM.                                          ECS017
01377                                                                   ECS017
01378  3960-OPTION-6.                                                   ECS017
01379 * SELECT        - COMPENSATION -          .                       ECS017
01380 * DROP   CLAIMS -              - OVERWRITE.                       ECS017
01381                                                                   ECS017
01382      IF CP-CLAIM OR                                               ECS017
01383         CP-OVERWT OR                                              ECS017
01384         CP-RC-OVERWT                                              ECS017
01385          GO TO 3999-EXIT.                                         ECS017
01386                                                                   ECS017
01387      GO TO 3990-WRT-CCM.                                          ECS017
01388                                                                   ECS017
01389  3970-OPTION-7.                                                   ECS017
01390 * SELECT        -              - OVERWRITE.                       ECS017
01391 * DROP   CLAIMS - COMPENSATION -          .                       ECS017
01392                                                                   ECS017
01393      IF CP-OVERWT OR                                              ECS017
01394         CP-RC-OVERWT                                              ECS017
01395          GO TO 3990-WRT-CCM.                                      ECS017
01396                                                                   ECS017
01397      GO TO 3999-EXIT.                                             ECS017
01398                                                                   ECS017
01399  3980-OPTION-8.                                                   ECS017
01400 * SELECT        -              -          .                       ECS017
01401 * DROP   CLAIMS - COMPENSATION - OVERWRITE.                       ECS017
01402                                                                   ECS017
01403      GO TO 3999-EXIT.                                             ECS017
01404                                                                   ECS017
01405  3990-WRT-CCM.                                                    ECS017
01406      IF DTE-CLIENT = 'NCL'                                        ECS017
01407          MOVE  CP-RECORD          TO  SAVE-OW-CPTR.               ECS017
01408                                                                   ECS017
           IF (DTE-CLIENT = 'DCC' or 'VPP')
              AND (AM-DCC-PRODUCT-CODE = 'DDF')
              MOVE '00'                TO CP-LF-TYPE
           END-IF
01409      WRITE CP-RECORD.                                             ECS017
01410                                                                   ECS017
01411      IF DTE-CLIENT = 'NCL'                                        ECS017
01412          MOVE  SAVE-OW-CPTR       TO  CP-RECORD.                  ECS017
01413                                                                   ECS017
01414  3999-EXIT.                                                       ECS017
01415      EXIT.                                                        ECS017
01416  EJECT                                                            ECS017
01417  5000-BUILD-CLAIM-RTN.                                            ECS017
01418      PERFORM 6000-CLEAR-EXTR-RTN THRU 6099-EXIT.                  ECS017

01419                                                                   ECS017
01420      MOVE DE-CARRIER             TO EX-CARRIER.                   ECS017
01421      MOVE DE-GROUPING            TO EX-GROUPING.                  ECS017
01422      MOVE DE-STATE               TO EX-ST.                        ECS017
01423      MOVE DE-ACCOUNT             TO EX-ACCT.                      ECS017
01424      MOVE DE-TRANS               TO EX-TRAN-TYPE.                 ECS017
01425      MOVE DE-RECALC-CODE         TO EX-RECALC.                    ECS017
01426                                                                   ECS017
01427      IF DE-DTH OR DE-OB-DTH                                       ECS017
01428          MOVE SPACES             TO EX-ATYP                       ECS017
01429          MOVE DE-LF-TYPE         TO EX-LTYP                       ECS017
01430          MOVE DE-CLAIM-AMT       TO EX-LAMT                       ECS017
01431      ELSE                                                         ECS017
01432          MOVE SPACES             TO EX-LTYP                       ECS017
01433          MOVE DE-AH-TYPE         TO EX-ATYP                       ECS017
01434          MOVE DE-CLAIM-AMT       TO EX-AAMT.                      ECS017
01435                                                                   ECS017
01436      MOVE DE-IG                  TO EX-IG.                        ECS017
01437                                                                   ECS017
01438      PERFORM 6200-CHECK-OB-RTN    THRU 6299-EXIT.                 ECS017
01439                                                                   ECS017
01440      PERFORM 5400-BUILD-PREM-COMM THRU 5499-EXIT.                 ECS017
01441                                                                   ECS017
01442  5099-EXIT.                                                       ECS017
01443      EXIT.                                                        ECS017
01444  EJECT                                                            ECS017
01445  5200-BUILD-RECALC.                                               ECS017
01446      MOVE CP-CARRIER             TO EX-CARRIER.                   ECS017
01447      MOVE CP-GROUPING            TO EX-GROUPING.                  ECS017
01448      MOVE CP-STATE               TO EX-ST.                        ECS017
01449      MOVE DE-ACCOUNT             TO EX-ACCT.                      ECS017
01450      MOVE DE-RECALC-CODE         TO EX-RECALC.                    ECS017
01451      MOVE CP-TRANS               TO EX-TRAN-TYPE.                 ECS017
01452      MOVE CP-LF-TYPE             TO EX-LTYP.                      ECS017
01453      MOVE CP-LF-PRM              TO EX-LAMT.

           EVALUATE TRUE
              WHEN (DTE-CLIENT = 'DCC' or 'VPP')
                 AND (CLAS-I-BEN-CATEGORY (CLAS-INDEXL) = 'D')
011410           MOVE DE-REI-LFPRM     TO EX-SPPDD-LCLP
011410           IF DE-CANCEL
011410              AND DE-LF-PRM NOT = ZEROS 
011410              COMPUTE CNC-FACT = DE-LF-RFND / DE-LF-PRM
011410              COMPUTE EX-SPPDD-LCLP ROUNDED =
011410                 DE-REI-LFPRM * CNC-FACT
011410           END-IF
              WHEN (DTE-CLIENT = 'DCC' or 'VPP')
                 AND (CLAS-I-BEN-CATEGORY (CLAS-INDEXL) = 'G')
                 AND (DE-REI-LFPRM > ZEROS)
                 MOVE DE-REI-LFPRM     TO EX-SPPDD-LCLP
                 IF (DE-CANCEL)
                    AND (DE-LF-PRM NOT = ZEROS)
                    MOVE DE-REI-LFRFND TO EX-SPPDD-LCLP
                 END-IF
           END-EVALUATE


01454      MOVE CP-LF-COM              TO EX-LCOM.                      ECS017
01455      ADD  CP-LF-PRM-ALT          TO EX-LAMT.                      ECS017
01456      ADD  CP-LF-COM-ALT          TO EX-LCOM.                      ECS017
01457      MOVE CP-AH-TYPE             TO EX-ATYP.                      ECS017
01458      MOVE CP-AH-PRM              TO EX-AAMT.                      ECS017


           EVALUATE TRUE
              WHEN (DTE-CLIENT = 'DCC' or 'VPP')
                 AND (CLAS-I-BEN-CATEGORY (CLAS-INDEXA) = 'D')
011410           MOVE DE-REI-AHPRM     TO EX-SPPDD-ACLP
011410           IF DE-CANCEL
011410              AND DE-AH-PRM NOT = ZEROS 
011410              COMPUTE CNC-FACT = DE-AH-RFND / DE-AH-PRM
011410              COMPUTE EX-SPPDD-ACLP ROUNDED =
011410                 DE-REI-AHPRM * CNC-FACT
011410           END-IF
              WHEN (DTE-CLIENT = 'DCC' or 'VPP')
                 AND (CLAS-I-BEN-CATEGORY (CLAS-INDEXA) = 'G')
                 AND (DE-REI-AHPRM > ZEROS)
                 MOVE DE-REI-AHPRM     TO EX-SPPDD-ACLP
                 IF (DE-CANCEL)
                    AND (DE-AH-PRM NOT = ZEROS)
                    MOVE DE-REI-AHRFND TO EX-SPPDD-ACLP
                 END-IF
           END-EVALUATE



01459      MOVE CP-AH-COM              TO EX-ACOM.                      ECS017
040504*    MOVE DE-ADDL-CLP            TO EX-DLR-INC
01460      MOVE CP-COM-TYPE            TO EX-COM-TYPE.                  ECS017
01461      MOVE DE-IG                  TO EX-IG.                        ECS017
01462      MOVE RECALC-LF-OW-COMM      TO EX-LOW.                       ECS017
01463      ADD  RECALC-LF-OW-SF        TO EX-LOW.                       ECS017
01464      MOVE RECALC-AH-OW-COMM      TO EX-AOW.                       ECS017
01465      ADD  RECALC-AH-OW-SF        TO EX-AOW.                       ECS017
01466                                                                   ECS017
01467      PERFORM 6200-CHECK-OB-RTN    THRU 6299-EXIT.                 ECS017
01468                                                                   ECS017
01469      IF DE-RC-ISSUE OR DE-RC-CANCEL                               ECS017
01470          PERFORM 5500-BUILD-RECALC  THRU  5599-EXIT               ECS017
01471      ELSE                                                         ECS017
01472          PERFORM 5400-BUILD-PREM-COMM THRU 5499-EXIT.             ECS017
01473                                                                   ECS017
01474  5299-EXIT.                                                       ECS017
01475      EXIT.                                                        ECS017
01476  EJECT                                                            ECS017
01477  5400-BUILD-PREM-COMM.                                            ECS017
01478      MOVE SPACES                 TO X-REC.                        ECS017
01479      MOVE ZEROS                  TO X-IG                          ECS017
01480                                     X-CODE                        ECS017
01481                                     X-AMT
011410                                    X-SPPDD-CLP
01482                                     X-BASE                        ECS017
01483                                     X-OVER                        ECS017
040504                                    X-DLR-INC
011410                                    X-LF-LMBA-FEE
011410                                    X-AH-LMBA-FEE
                                          X-BANK-FEE
                                          X-CSO-ADMIN
01484                                     X-PROCESSED                   ECS017
01485                                     WS-X-PROCESSED.               ECS017
01486      MOVE EX-CARRIER             TO X-CARRIER.                    ECS017
01487      MOVE EX-GROUPING            TO X-GROUPING.                   ECS017
01488      MOVE EX-ST                  TO X-ST.                         ECS017
01489      MOVE EX-ACCT                TO X-ACCT.                       ECS017
01490      MOVE EX-RECALC              TO X-RECALC                      ECS017
01491      MOVE EX-COM-TYPE            TO X-ACCT-COM-TYPE.              ECS017
01492                                                                   ECS017
01493      IF EX-TRAN-TYPE = 'X'                                        ECS017
01494          MOVE 3                  TO X-CODE                        ECS017
01495      ELSE                                                         ECS017
01496          IF CP-CANCEL OR CP-RC-CANCEL                             ECS017
01497              MOVE 2              TO X-CODE                        ECS017
01498          ELSE                                                     ECS017
01499              MOVE 1              TO X-CODE.                       ECS017
01500                                                                   ECS017
01501 *  FORMAT LIFE.                                                   ECS017
01502                                                                   ECS017
01503      IF EX-IG = '1' OR 'I'                                        ECS017
01504          MOVE 1                  TO X-IG                          ECS017
01505      ELSE                                                         ECS017
01506          MOVE 3 TO X-IG.                                          ECS017
01507                                                                   ECS017
01508      MOVE EX-LAMT                TO X-AMT
011410     MOVE EX-SPPDD-LCLP          TO X-SPPDD-CLP
01509      MOVE RUN-CC                 TO X-CC.                            CL**4
01510      MOVE RUN-YR                 TO X-YR.                            CL**4
01511      MOVE RUN-MO                 TO X-MO.                         ECS017
01512      MOVE EX-LTYPE               TO X-TYPE.                       ECS017
01513                                                                   ECS017
052814*    IF (X-TYP NOT = SPACES AND '00' AND 'DD' AND 'CU')
022615     IF (X-TYP NOT = SPACES AND '00' AND 'DD')
01515          NEXT SENTENCE                                            ECS017
01516      ELSE                                                         ECS017
01517          GO TO 5450-AH.                                           ECS017
01518                                                                   ECS017
01519      IF X-CODE = '3'                                              ECS017
01520          PERFORM 5600-SELECT-FILE-RTN THRU 5699-EXIT              ECS017
01521          GO TO 5499-EXIT.                                         ECS017
01522                                                                   ECS017
01523      MOVE EX-LCOM                TO X-BASE.                       ECS017
01524                                                                   ECS017
01525      IF PRCM-LF-OW-COMM = ZEROS AND                               ECS017
01526         PRCM-LF-OW-SF   = ZEROS AND
011410        PRCM-LF-LMBA-FEE = ZEROS
01527          PERFORM 5600-SELECT-FILE-RTN THRU 5699-EXIT              ECS017
01528          GO TO 5450-AH.                                           ECS017
01529                                                                   ECS017
01530      IF PRCM-LF-OW-SF   = ZEROS                                   ECS017
01531          MOVE PRCM-LF-OW-COMM    TO X-OVER
011410         MOVE PRCM-LF-LMBA-FEE   TO X-LF-LMBA-FEE
01532          MOVE 'O'                TO X-OWRT-COM-TYPE               ECS017
01533          PERFORM 5600-SELECT-FILE-RTN THRU 5699-EXIT              ECS017
01534          GO TO 5450-AH                                            ECS017
01535      ELSE                                                         ECS017
01536          MOVE PRCM-LF-OW-COMM    TO X-OVER                        ECS017
01537          MOVE 'O'                TO X-OWRT-COM-TYPE               ECS017
01538          PERFORM 5600-SELECT-FILE-RTN THRU 5699-EXIT              ECS017
01539          MOVE EX-CARRIER         TO X-CARRIER                     ECS017
01540          MOVE EX-GROUPING        TO X-GROUPING                    ECS017
01541          MOVE EX-ACCT            TO X-ACCT                        ECS017
01542          MOVE PRCM-LF-OW-SF      TO X-OVER                        ECS017
01543          MOVE 'G'                TO X-OWRT-COM-TYPE               ECS017
01544          MOVE ZEROS              TO X-AMT
011410         MOVE ZEROS              TO X-SPPDD-CLP
01545          MOVE ZEROS              TO X-BASE                        ECS017
01546          PERFORM 5600-SELECT-FILE-RTN THRU 5699-EXIT.             ECS017
01547                                                                   ECS017
01548  5450-AH.                                                         ECS017
01549 *  FORMAT A-H.                                                    ECS017
01550                                                                   ECS017
052814     IF X-TYP = SPACES  OR '00' OR 'DD'
01552          NEXT SENTENCE                                            ECS017
01553      ELSE                                                         ECS017
01554          MOVE ZEROS              TO X-IG                          ECS017
01555                                     X-AMT
011410                                    X-SPPDD-CLP
01556                                     X-BASE                        ECS017
01557                                     X-OVER                        ECS017
040504                                    X-DLR-INC
011410                                    X-LF-LMBA-FEE
011410                                    X-AH-LMBA-FEE
                                          X-BANK-FEE
                                          X-CSO-ADMIN
01558          MOVE EX-CARRIER         TO X-CARRIER                     ECS017
01559          MOVE EX-GROUPING        TO X-GROUPING                    ECS017
01560          MOVE EX-ST              TO X-ST                          ECS017
01561          MOVE EX-ACCT            TO X-ACCT.                       ECS017
01562                                                                   ECS017
01563      IF EX-ATYPE NOT = SPACES  AND  NOT = '00'                    ECS017
01564          NEXT SENTENCE                                            ECS017
01565      ELSE                                                         ECS017
01566          GO TO 5499-EXIT.                                         ECS017
01567                                                                   ECS017
01568      MOVE EX-ATYPE               TO X-TYPE.                       ECS017
01569      MOVE EX-AAMT                TO X-AMT.                        ECS017
011410     MOVE EX-SPPDD-ACLP          TO X-SPPDD-CLP
01570      MOVE EX-ACOM                TO X-BASE.                       ECS017
01571                                                                   ECS017
01572      IF EX-IG = '1'                                               ECS017
01573          MOVE 2                  TO X-IG                          ECS017
01574      ELSE                                                         ECS017
01575          MOVE 4                  TO X-IG.                         ECS017
01576                                                                   ECS017
01577      IF (PRCM-AH-OW-COMM   = ZEROS)
01578         AND (PRCM-AH-OW-SF = ZEROS)
              AND (PRCM-DLR-INC  = ZEROS)
011410        AND (PRCM-AH-LMBA-FEE = ZEROS)
              AND (PRCM-BANK-FEE = ZEROS)
              AND (PRCM-CSO-ADMIN = ZEROS)
01579         PERFORM 5600-SELECT-FILE-RTN
                                       THRU 5699-EXIT
01580         GO TO 5499-EXIT
           END-IF
01581                                                                   ECS017
01582      IF PRCM-AH-OW-SF   = ZEROS                                   ECS017
01583          MOVE PRCM-AH-OW-COMM    TO X-OVER                        ECS017
040504         MOVE PRCM-DLR-INC       TO X-DLR-INC
011410         MOVE PRCM-AH-LMBA-FEE   TO X-AH-LMBA-FEE
               MOVE PRCM-BANK-FEE      TO X-BANK-FEE
               MOVE PRCM-CSO-ADMIN     TO X-CSO-ADMIN
01584          MOVE 'O'                TO X-OWRT-COM-TYPE               ECS017
01585          PERFORM 5600-SELECT-FILE-RTN THRU 5699-EXIT              ECS017
01586          GO TO 5499-EXIT                                          ECS017
01587      ELSE                                                         ECS017
01588          MOVE PRCM-AH-OW-COMM    TO X-OVER                        ECS017
01589          MOVE 'O'                TO X-OWRT-COM-TYPE               ECS017
01590          PERFORM 5600-SELECT-FILE-RTN THRU 5699-EXIT              ECS017
01591          MOVE EX-CARRIER         TO X-CARRIER                     ECS017
01592          MOVE EX-GROUPING        TO X-GROUPING                    ECS017
01593          MOVE EX-ACCT            TO X-ACCT                        ECS017
01594          MOVE PRCM-AH-OW-SF      TO X-OVER                        ECS017
01595          MOVE 'G'                TO X-OWRT-COM-TYPE               ECS017
01596          MOVE ZEROS              TO X-AMT
011410         MOVE ZEROS              TO X-SPPDD-CLP
01597          MOVE ZEROS              TO X-BASE                        ECS017
01598          PERFORM 5600-SELECT-FILE-RTN THRU 5699-EXIT.             ECS017
01599                                                                   ECS017
01600  5499-EXIT.                                                       ECS017
01601      EXIT.                                                        ECS017
01602  EJECT                                                            ECS017
01603  5500-BUILD-RECALC.                                               ECS017
01604      MOVE SPACES                 TO X-REC.                        ECS017
01605      MOVE ZEROS                  TO X-IG                          ECS017
01606                                     X-CODE                        ECS017
01607                                     X-AMT
011410                                    X-SPPDD-CLP
01608                                     X-BASE                        ECS017
01609                                     X-OVER                        ECS017
040504                                    X-DLR-INC
011410                                    X-LF-LMBA-FEE
011410                                    X-AH-LMBA-FEE
                                          X-BANK-FEE
                                          X-CSO-ADMIN
01610                                     X-PROCESSED                   ECS017
01611                                     WS-X-PROCESSED.               ECS017
01612      MOVE EX-CARRIER             TO X-CARRIER.                    ECS017
01613      MOVE EX-GROUPING            TO X-GROUPING.                   ECS017
01614      MOVE EX-ST                  TO X-ST.                         ECS017
01615      MOVE EX-ACCT                TO X-ACCT.                       ECS017
01616      MOVE EX-RECALC              TO X-RECALC                      ECS017
01617      MOVE EX-COM-TYPE            TO X-ACCT-COM-TYPE.              ECS017
01618                                                                   ECS017
01619      IF CP-CANCEL OR CP-RC-CANCEL                                 ECS017
01620          MOVE 2                  TO X-CODE                        ECS017
01621      ELSE                                                         ECS017
01622          MOVE 1                  TO X-CODE.                       ECS017
01623                                                                   ECS017
01624 *  FORMAT LIFE.                                                   ECS017
01625                                                                   ECS017
01626      IF EX-IG = '1' OR 'I'                                        ECS017
01627          MOVE 1                  TO X-IG                          ECS017
01628      ELSE                                                         ECS017
01629          MOVE 3 TO X-IG.                                          ECS017
01630                                                                   ECS017
01631      MOVE EX-LAMT                TO X-AMT.                        ECS017
01632      MOVE RUN-CC                 TO X-CC.                            CL**4
01633      MOVE RUN-YR                 TO X-YR.                            CL**4
01634      MOVE RUN-MO                 TO X-MO.                         ECS017
01635      MOVE EX-LTYPE               TO X-TYPE.                       ECS017
01636                                                                   ECS017
052814     IF X-TYP NOT = SPACES AND '00' AND 'DD' and 'CU'
01638          NEXT SENTENCE                                            ECS017
01639      ELSE                                                         ECS017
01640          GO TO 5550-AH.                                           ECS017
01641                                                                   ECS017
011410* HAD TO ADD THIS FOR SPPDD 
011410     MOVE EX-LF-LMBA-FEE         TO X-LF-LMBA-FEE
011410     MOVE EX-AH-LMBA-FEE         TO X-AH-LMBA-FEE
01642      MOVE EX-LCOM                TO X-BASE.                       ECS017
01643      MOVE EX-LOW                 TO X-OVER.                       ECS017
01644                                                                   ECS017
01645      PERFORM 5600-SELECT-FILE-RTN THRU 5699-EXIT.                 ECS017
01646                                                                   ECS017
01647  5550-AH.                                                         ECS017
01648 *  FORMAT A-H.                                                    ECS017
01649                                                                   ECS017
052814     IF X-TYP = SPACES OR '00' OR 'DD' OR 'CU'
01651          NEXT SENTENCE                                            ECS017
01652      ELSE                                                         ECS017
01653          MOVE ZEROS              TO X-IG                          ECS017
01654                                     X-AMT
011410                                    X-SPPDD-CLP
01655                                     X-BASE                        ECS017
01656                                     X-OVER                        ECS017
                                          X-DLR-INC
011410                                    X-LF-LMBA-FEE
011410                                    X-AH-LMBA-FEE
                                          X-BANK-FEE
                                          X-CSO-ADMIN
01657          MOVE EX-CARRIER         TO X-CARRIER                     ECS017
01658          MOVE EX-GROUPING        TO X-GROUPING                    ECS017
01659          MOVE EX-ST              TO X-ST                          ECS017
01660          MOVE EX-ACCT            TO X-ACCT.                       ECS017
01661                                                                   ECS017
01662      IF EX-ATYPE NOT = SPACES  AND  NOT = '00'                    ECS017
01663          NEXT SENTENCE                                            ECS017
01664      ELSE                                                         ECS017
01665          GO TO 5599-EXIT.                                         ECS017
01666                                                                   ECS017
01667      MOVE EX-ATYPE               TO X-TYPE.                       ECS017
01668      MOVE EX-AAMT                TO X-AMT.                        ECS017
01669      MOVE EX-ACOM                TO X-BASE.                       ECS017
01670      MOVE EX-AOW                 TO X-OVER.                       ECS017
040504     MOVE EX-DLR-INC             TO X-DLR-INC
           MOVE EX-LF-LMBA-FEE         TO X-LF-LMBA-FEE
           MOVE EX-AH-LMBA-FEE         TO X-AH-LMBA-FEE
           MOVE EX-BANK-FEE            TO X-BANK-FEE
           MOVE EX-CSO-ADMIN           TO X-CSO-ADMIN
01671                                                                   ECS017
01672      IF EX-IG = '1'                                               ECS017
01673          MOVE 2                  TO X-IG                          ECS017
01674      ELSE                                                         ECS017
01675          MOVE 4                  TO X-IG.                         ECS017
01676                                                                   ECS017
01677      PERFORM 5600-SELECT-FILE-RTN THRU 5699-EXIT.                 ECS017
01678                                                                   ECS017
01679  5599-EXIT.                                                       ECS017
01680      EXIT.                                                        ECS017
01681  EJECT                                                            ECS017
01682  5600-SELECT-FILE-RTN.                                            ECS017
01683                                                                   ECS017
01684      IF DE-RC-ISSUE OR DE-RC-CANCEL                               ECS017
01685          PERFORM 5700-WRT-REC-RTN THRU 5799-EXIT                  ECS017
01686      ELSE                                                         ECS017
01687          PERFORM 5800-WRT-STD-RTN THRU 5899-EXIT.                 ECS017
01688                                                                   ECS017
01689      MOVE HIGH-VALUES            TO X-GROUPING                    ECS017
01690                                     X-DMD-RESIDENT-ST             ECS017
01691                                     X-ACCT.                       ECS017
01692                                                                   ECS017
01693      IF DE-RC-ISSUE OR DE-RC-CANCEL                               ECS017
01694          PERFORM 5700-WRT-REC-RTN THRU 5799-EXIT                  ECS017
01695      ELSE                                                         ECS017
01696          PERFORM 5800-WRT-STD-RTN THRU 5899-EXIT.                 ECS017
01697                                                                   ECS017
01698      MOVE HIGH-VALUES            TO X-CARRIER                     ECS017
01699                                     X-DMD-RESIDENT-ST.            ECS017
01700                                                                   ECS017
01701      IF DE-RC-ISSUE OR DE-RC-CANCEL                               ECS017
01702          PERFORM 5700-WRT-REC-RTN THRU 5799-EXIT                  ECS017
01703      ELSE                                                         ECS017
01704          PERFORM 5800-WRT-STD-RTN THRU 5899-EXIT.                 ECS017
01705                                                                   ECS017
01706  5699-EXIT.                                                       ECS017
01707      EXIT.                                                        ECS017
01708                                                                   ECS017
01709  EJECT                                                            ECS017
01710 ****** OUTPUT IS COMRCALC SYS004  -  INPUT TO ECS018              ECS017
01711  5700-WRT-REC-RTN.                                                ECS017
01712      IF DTE-CLIENT = 'DMD'                                        ECS017
01713          MOVE DE-CERT (5:2)      TO X-TYP                         ECS017
01714          MOVE SPACE              TO X-OB                          ECS017
01715          IF X-ACCT NOT = HIGH-VALUES                              ECS017
01716              MOVE DE-CERT (1:2)  TO X-DMD-RESIDENT-ST.            ECS017
01717                                                                   ECS017
020305     IF DTE-CLIENT = 'DCC' or 'VPP'
020305        MOVE DE-CLP-STATE        TO X-DMD-RESIDENT-ST
020305     END-IF
01718      MOVE WS-X-PROCESSED         TO X-PROCESSED.                  ECS017
01719      MOVE X-REC                  TO RECALC-RECORD.                ECS017
01720                                                                   ECS017
122002     IF DE-ENTRY-STATUS = 'M'
              CONTINUE
           ELSE
01721         WRITE RECALC-RECORD
           END-IF
           
01722      .
01723  5799-EXIT.                                                       ECS017
01724      EXIT.                                                        ECS017
01725                                                                   ECS017
01726  EJECT                                                            ECS017
01727 ****** OUTPUT IS PRCMEXTR SYS014  -  INPUT TO ECS019              ECS017
01728  5800-WRT-STD-RTN.                                                ECS017
01729                                                                   ECS017
01730      IF DTE-CLIENT = 'DMD'                                        ECS017
01731          MOVE DE-CERT (5:2)      TO X-TYP                         ECS017
01732          IF X-ACCT NOT = HIGH-VALUES                              ECS017
01733              MOVE DE-CERT (1:2)  TO X-DMD-RESIDENT-ST.            ECS017
01734                                                                   ECS017
01735      IF (DTE-CLIENT = 'HER' AND                                   ECS017
01736         DE-GROUPING EQUAL '600000')                               ECS017
01737         IF X-GROUPING = HIGH-VALUES                               ECS017
01738             NEXT SENTENCE                                         ECS017
01739         ELSE                                                      ECS017
01740             IF DE-EFF GREATER THAN 19871027                       ECS017
01741                 MOVE ' AFTER'    TO X-GROUPING                    ECS017
01742             ELSE                                                  ECS017
01743                 MOVE 'BEFORE'    TO X-GROUPING.                   ECS017
01744                                                                   ECS017
020305     IF DTE-CLIENT = 'DCC' or 'VPP'
020305        MOVE DE-CLP-STATE        TO X-DMD-RESIDENT-ST
020305     END-IF
01745      MOVE WS-X-PROCESSED         TO X-PROCESSED.                  ECS017

01746      MOVE X-REC                  TO EXTR-RECORD.                  ECS017
01747                                                                   ECS017
122002     IF DE-ENTRY-STATUS = 'M'
              AND (X-CODE = 1 OR 2)
              CONTINUE
           ELSE
122002        WRITE EXTR-RECORD
122002     END-IF
01749      .                                                            ECS017
01750  5899-EXIT.                                                       ECS017
01751      EXIT.                                                        ECS017
01752  EJECT                                                            ECS017
01753  6000-CLEAR-EXTR-RTN.                                             ECS017
01754      MOVE SPACES                 TO SV-EXTR-REC.                  ECS017
01755      MOVE ZEROS                  TO EX-LAMT  EX-LCOM  EX-LOW
011410                                    EX-SPPDD-LCLP EX-SPPDD-ACLP
01756                                     EX-AAMT EX-ACOM EX-AOW  EX-IG ECS017
040504                                    EX-DLR-INC
011410                                    EX-LF-LMBA-FEE
011410                                    EX-AH-LMBA-FEE
                                          EX-BANK-FEE
                                          EX-CSO-ADMIN
01757                                     PRCM-LF-OW-COMM               ECS017
01758                                     PRCM-LF-OW-SF                 ECS017
01759                                     PRCM-AH-OW-COMM               ECS017
01760                                     PRCM-AH-OW-SF                 ECS017
040504                                    PRCM-DLR-INC
011410                                    PRCM-LF-LMBA-FEE
011410                                    PRCM-AH-LMBA-FEE
                                          PRCM-BANK-FEE
                                          PRCM-CSO-ADMIN
01761                                     RECALC-LF-OW-COMM             ECS017
01762                                     RECALC-LF-OW-SF               ECS017
01763                                     RECALC-AH-OW-COMM             ECS017
01764                                     RECALC-AH-OW-SF.              ECS017
01765                                                                   ECS017
01766  6099-EXIT.                                                       ECS017
01767      EXIT.                                                        ECS017
01768                                                                   ECS017
01769  EJECT                                                            ECS017
01770  6200-CHECK-OB-RTN.                                               ECS017
01771      MOVE SPACES                 TO EX-AZ EX-LZ.                  ECS017
01772      MOVE CLAS-STARTL            TO CLAS-INDEXL.                  ECS017
01773      MOVE CLAS-STARTA            TO CLAS-INDEXA.                  ECS017
01774                                                                   ECS017
091911     IF EX-LTYP = '  ' OR '00' OR 'DD' OR 'CU'
01776          GO TO 6230-CHECK-AH.                                     ECS017
01777                                                                   ECS017
01778  6210-CHECK-LIFE.                                                 ECS017
01779      MOVE EX-LTYP                TO CLAS-LOOK.                    ECS017
01780                                                                   ECS017
01781      IF CLAS-STARTL = ZEROS                                       ECS017
01782          GO TO 6230-CHECK-AH.                                     ECS017
01783                                                                   ECS017
01784  6220-LOOP-LIFE.                                                  ECS017
01785      IF CLAS-INDEXL GREATER THAN CLAS-MAXL                        ECS017
01786           GO TO 6230-CHECK-AH.                                    ECS017
01787                                                                   ECS017
01788      IF CLAS-LOOK NOT = CLAS-I-BEN (CLAS-INDEXL)                  ECS017
01789          ADD 1 TO CLAS-INDEXL                                     ECS017
01790          GO TO 6220-LOOP-LIFE.                                    ECS017
01791                                                                   ECS017
01792      IF CLAS-I-BAL (CLAS-INDEXL) = 'B'                            ECS017
01793          MOVE '1'                TO EX-LZ                         ECS017
01794                                     CP-OB-1ST-RENEWAL.            ECS017
01795                                                                   ECS017
01796  6230-CHECK-AH.                                                   ECS017
01797      IF EX-ATYP = '  ' OR '00'                                    ECS017
01798          GO TO 6280-CHECK-1YR-REIN.                               ECS017
01799                                                                   ECS017
01800      MOVE EX-ATYP                TO CLAS-LOOK.                    ECS017
01801                                                                   ECS017
01802      IF CLAS-STARTA = ZEROS                                       ECS017
01803          GO TO 6280-CHECK-1YR-REIN.                               ECS017
01804                                                                   ECS017
01805  6240-LOOP-AH.                                                    ECS017
01806      IF CLAS-INDEXA GREATER THAN CLAS-MAXA                        ECS017
01807          GO TO 6280-CHECK-1YR-REIN.                               ECS017
01808                                                                   ECS017
01809      IF CLAS-LOOK NOT = CLAS-I-BEN (CLAS-INDEXA)                  ECS017
01810          ADD 1 TO CLAS-INDEXA                                     ECS017
01811          GO TO 6240-LOOP-AH.                                      ECS017
01812                                                                   ECS017
01813      IF CLAS-I-BAL (CLAS-INDEXA) = 'B'                            ECS017
01814          MOVE '1'                TO EX-AZ                         ECS017
01815                                     CP-OB-1ST-RENEWAL.            ECS017
01816                                                                   ECS017
01817  6280-CHECK-1YR-REIN.                                             ECS017
01818      IF EX-LZ NOT = '1' AND                                       ECS017
01819         EX-AZ NOT = '1'                                           ECS017
01820          GO TO 6299-EXIT.                                         ECS017
01821                                                                   ECS017
01822      MOVE AM-ANNIVERSARY-DATE    TO WS-AM-ANNIVERSARY-DATE-N.        CL*12
01823      ADD 1 TO AM-AN-CCYY.                                            CL*12
01824                                                                      CL*12
01825      IF WS-AM-ANNIVERSARY-DATE-N NOT LESS THAN DE-EFF                CL*12
01826          GO TO 6299-EXIT.                                         ECS017
01827                                                                   ECS017
01828      IF EX-LZ = '1'                                               ECS017
01829          MOVE '2'                TO EX-LZ                         ECS017
01830                                     CP-OB-1ST-RENEWAL.            ECS017
01831                                                                   ECS017
01832      IF EX-AZ = '1'                                               ECS017
01833          MOVE '2'                TO EX-AZ                         ECS017
01834                                     CP-OB-1ST-RENEWAL.            ECS017
01835                                                                   ECS017
01836  6299-EXIT.                                                       ECS017
01837      EXIT.                                                        ECS017
01838  EJECT                                                            ECS017
100703 6300-CHECK-TYPES.
100703
100703     MOVE SPACES                 TO EX-AZ EX-LZ
100703     MOVE CLAS-STARTL            TO CLAS-INDEXL
100703     MOVE CLAS-STARTA            TO CLAS-INDEXA
100703
052814     IF DE-LF-TYPE = '  ' OR '00' OR 'DD' or 'CU'
100703        GO TO 6330-CHECK-AH
100703     END-IF
100703     .
100703                                                                        
100703 6310-CHECK-LIFE.                                                       
100703     MOVE DE-LF-TYPE             TO CLAS-LOOK
100703
100703     IF CLAS-STARTL = ZEROS                                             
100703        GO TO 6330-CHECK-AH
100703     END-IF
100703     .
100703                                                                        
100703 6320-LOOP-LIFE.                                                        
100703     IF CLAS-INDEXL GREATER THAN CLAS-MAXL                              
100703        GO TO 6330-CHECK-AH
100703     END-IF
100703                                                                        
100703     IF CLAS-LOOK NOT = CLAS-I-BEN (CLAS-INDEXL)                        
100703        ADD 1 TO CLAS-INDEXL                                           
100703        GO TO 6320-LOOP-LIFE
100703     END-IF
100703     .
100703                                                                        
100703 6330-CHECK-AH.                                                         
100703     IF DE-AH-TYPE = '  ' OR '00'
100703        GO TO 6399-EXIT
100703     END-IF
100703                                                                        
100703     MOVE DE-AH-TYPE             TO CLAS-LOOK
100703                                                                        
100703     IF CLAS-STARTA = ZEROS                                             
100703        GO TO 6399-EXIT
100703     END-IF
100703     .
100703 6340-LOOP-AH.                                                          
100703
100703     IF CLAS-INDEXA GREATER THAN CLAS-MAXA
100703        GO TO 6399-EXIT
100703     END-IF
100703
100703     IF CLAS-LOOK NOT = CLAS-I-BEN (CLAS-INDEXA)
100703        ADD 1 TO CLAS-INDEXA
100703        GO TO 6340-LOOP-AH
100703     END-IF
100703
100703     .
100703 6399-EXIT.                                                             
100703     EXIT.                                                              
01839  8000-PRT-RTN.                                                    ECS017
01840                              COPY ELCPRT2.                        ECS017
01841                                                                   ECS017
01842  8099-PRT-XIT.                                                    ECS017
01843      EXIT.                                                        ECS017
01844                                                                   ECS017
01845  EJECT                                                            ECS017
01846  9900-END-JOB.                                                    ECS017
01847      IF ACCT-SW = +1                                              ECS017
01848          PERFORM 2600-ACCT-BREAK-RTN THRU 2699-EXIT.              ECS017
01849                                                                   ECS017
01850      MOVE '-'                    TO P-CTL.                           CL**2
01851      MOVE P-LINE                 TO P-DATA.                       ECS017
01852      MOVE P-CTL                  TO X.                            ECS017
01853      PERFORM 8000-PRT-RTN THRU 8099-PRT-XIT.                      ECS017
01854                                                                   ECS017
01855      MOVE '-'                    TO X.                               CL**2
01856      MOVE COMP-MESS              TO P-DATA.                       ECS017
01857      PERFORM 8000-PRT-RTN THRU 8099-PRT-XIT.                      ECS017
01858                                                                   ECS017
01859      CLOSE EP-EXTR                                                ECS017
01860            ERACCTT                                                ECS017
01861            EXTR-DISK                                              ECS017
01862            RECALC-EXTR                                            ECS017
01863            END-PRINT                                              ECS017
01864            COMM-TRANS.                                            ECS017
01865                                                                   ECS017
01866  9920-CLOSE-FICH.                                                 ECS017
01867                              COPY ELCPRTC.                        ECS017
01868                                                                   ECS017
01869  9999-STOP-RUN.                                                   ECS017
01870      MOVE ZEROS  TO RETURN-CODE.                                  ECS017
01871      GOBACK.                                                      ECS017
01872                                                                   ECS017
01873                                                                   ECS017
01874  ABEND-PGM SECTION.                                               ECS017
01875                              COPY ELCABEND SUPPRESS.              ECS017
01876                                                                   ECS017
