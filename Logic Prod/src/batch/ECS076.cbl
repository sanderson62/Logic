00001  IDENTIFICATION DIVISION.                                         04/18/98
00002                                                                   ECS076
00003  PROGRAM-ID.                ECS076.                                  LV003
00004 *                           VMOD=2.010                            ECS076
00005 *              PROGRAM CONVERTED BY                               ECS076
00006 *              COBOL CONVERSION AID PO 5785-ABJ                   ECS076
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          ECS076
00008 *              CONVERSION DATE 03/23/94 12:54:59.                 ECS076
00009                                                                   ECS076
00010 *AUTHOR.        LOGIC, INC.                                       ECS076
00011 *               DALLAS, TEXAS.                                    ECS076
00012                                                                   ECS076
00013 *DATE-COMPILED.                                                   ECS076
00014                                                                      CL**2
00015 *SECURITY.   *****************************************************ECS076
00016 *            *                                                   *ECS076
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ECS076
00018 *            *                                                   *ECS076
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS076
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ECS076
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *ECS076
00022 *            *                                                   *ECS076
00023 *            *****************************************************ECS076
00024                                                                   ECS076
00025 *REMARKS.                                                         ECS076
00026 *        THIS PROGRAM PRINTS THE ACCOUNT MASTER.                  ECS076
00027                                                                   ECS076
102004******************************************************************
102004*                   C H A N G E   L O G
102004*
102004* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
102004*-----------------------------------------------------------------
102004*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
102004* EFFECTIVE    NUMBER
102004*-----------------------------------------------------------------
102004* 102004    2003031400002  PEMA  ADD NEW STATUS CODE
051810* 051810 CR2010042900001   PEMA  SPEARATE CITY AND STATE
031811* 031811 CR2011012700001   PEMA  ADD ACCT STATUS S - SUSPENDED
021916* 021916 CR2014010900001   TANA  ADD ACCT STATUS D,L,R,P
102004******************************************************************
00028  ENVIRONMENT DIVISION.                                            ECS076
00029  CONFIGURATION SECTION.                                           ECS076
00030  SPECIAL-NAMES.                                                   ECS076
00031      C02 IS LCP-CH2                                               ECS076
00032      C03 IS LCP-CH3                                               ECS076
00033      C04 IS LCP-CH4                                               ECS076
00034      C05 IS LCP-CH5                                               ECS076
00035      C06 IS LCP-CH6                                               ECS076
00036      C07 IS LCP-CH7                                               ECS076
00037      C08 IS LCP-CH8                                               ECS076
00038      C09 IS LCP-CH9                                               ECS076
00039      C10 IS LCP-CH10                                              ECS076
00040      C11 IS LCP-CH11                                              ECS076
00041      C12 IS LCP-CH12                                              ECS076
00042      S01 IS LCP-P01                                               ECS076
00043      S02 IS LCP-P02.                                              ECS076
00044  INPUT-OUTPUT SECTION.                                            ECS076
00045  FILE-CONTROL.                                                    ECS076
00046                                                                   ECS076
00047      SELECT PRNTR            ASSIGN TO SYS008-UR-1403-S-SYS008.   ECS076
00048      SELECT ACCT-SEQUENTIAL  ASSIGN TO SYS010-UT-2400-S-SYS010.   ECS076
00049      SELECT ACC-MSTR         ASSIGN TO SYS021-FBA1-ERACCTT        ECS076
00050                              ORGANIZATION IS INDEXED              ECS076
00051                              ACCESS IS SEQUENTIAL                 ECS076
00052                              RECORD KEY IS ACC-MSTR-KEY           ECS076
00053                              FILE STATUS IS ERACCT-FILE-STATUS.   ECS076
00054      SELECT PLAN-MSTR        ASSIGN TO SYS022-FBA1-ERPLAN         ECS076
00055                              ORGANIZATION IS INDEXED              ECS076
00056                              ACCESS IS DYNAMIC                    ECS076
00057                              RECORD KEY IS PL-CONTROL-PRIMARY     ECS076
00058                              FILE STATUS IS ERPLAN-FILE-STATUS.   ECS076
00059      SELECT DISK-DATE        ASSIGN TO SYS019-UT-FBA1-S-SYS019.   ECS076
00060      SELECT FICH             ASSIGN TO SYS020-UT-2400-S-SYS020.   ECS076
00061  EJECT                                                            ECS076
00062  DATA DIVISION.                                                   ECS076
00063  FILE SECTION.                                                    ECS076
00064                                                                   ECS076
00065  FD  PRNTR                                                        ECS076
00066                                  COPY ELCPRTFD.                   ECS076
00067  EJECT                                                            ECS076
00068  FD  ACCT-SEQUENTIAL                                              ECS076
00069      BLOCK CONTAINS 0 RECORDS
00070      RECORDING MODE F.                                               CL**2
00071                                                                      CL**2
00072                                                                   ECS076
00073  01  ACCT-RECORD-IN              PIC X(2000).                     ECS076
00074  EJECT                                                            ECS076
00075  FD  ACC-MSTR.                                                       CL**2
00076                                                                   ECS076
00077  01  ACC-MSTR-REC.                                                ECS076
00078      12  FILLER                  PIC XX.                          ECS076
00079      12  ACC-MSTR-KEY            PIC X(26).                       ECS076
00080      12  FILLER                  PIC X(1972).                     ECS076
00081  EJECT                                                            ECS076
00082  FD  PLAN-MSTR.                                                      CL**2
00083                                                                   ECS076
00084                                  COPY ERCPLAN.                    ECS076
00085  EJECT                                                            ECS076
00086  FD  DISK-DATE                                                    ECS076
00087                                  COPY ELCDTEFD.                   ECS076
00088  EJECT                                                            ECS076
00089  FD  FICH                                                         ECS076
00090                                  COPY ELCFCHFD.                   ECS076
00091  EJECT                                                            ECS076
00092  WORKING-STORAGE SECTION.                                         ECS076
00093  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      ECS076
00094  77  LCP-ASA                       PIC X.                         ECS076
00095  77  FILLER  PIC X(32) VALUE '********************************'.  ECS076
00096  77  FILLER  PIC X(32) VALUE '     ECS076 WORKING-STORAGE     '.  ECS076
00097  77  FILLER  PIC X(32) VALUE '********* V/M=2.010 ************'.  ECS076
00098                                                                   ECS076
00099  77  X1                          PIC S999    COMP    VALUE ZERO.  ECS076
00100  77  X2                          PIC S999    COMP.                ECS076
00101  77  X3                          PIC S999    COMP.                ECS076
00102  77  X4                          PIC S999    COMP.                ECS076
00103  77  WS-LINE                     PIC S999    COMP-3  VALUE ZERO.  ECS076
00104  77  PGM-SUB                     PIC S999    COMP-3  VALUE +076.  ECS076
00105  77  WS-LINES                    PIC S999     COMP-3 VALUE +99.   ECS076
00106  77  WRK-COMM                    PIC S99V9(3) COMP-3.             ECS076
00107  77  WS-PAGE                     PIC S9(5)    COMP-3 VALUE ZERO.  ECS076
00108  77  X                           PIC X.                           ECS076
00109  77  AM-EARN-METHOD              PIC X.                           ECS076
00110  77  CAL-ST-CD                   PIC XX              VALUE SPACES.ECS076
00111  77  UTAH-ST-CD                  PIC XX              VALUE SPACES.ECS076
00112  77  WS-RECORD-COUNT             PIC S9(7)    COMP-3 VALUE +0.    ECS076
00113  77  FIRST-TIME-SWITCH           PIC X               VALUE 'Y'.   ECS076
00114      88  FIRST-TIME                                  VALUE 'Y'.   ECS076
00115  77  PLAN-MASTER-SWITCH          PIC X               VALUE 'N'.   ECS076
00116      88  PLAN-MASTER-FOUND                           VALUE 'Y'.   ECS076
00117                                                                   ECS076
00118  01  WS.                                                          ECS076
00119      12  WRK-TIME                PIC 999999.                      ECS076
00120      12  WRK-TIME-R  REDEFINES  WRK-TIME.                         ECS076
00121          16  WRK-TIME-HH         PIC XX.                          ECS076
00122          16  WRK-TIME-MM         PIC XX.                          ECS076
00123          16  WRK-TIME-SS         PIC XX.                          ECS076
00124                                                                   ECS076
00125  01  FILE-STATUS.                                                 ECS076
00126      12  ERACCT-FILE-STATUS      PIC XX              VALUE '00'.  ECS076
00127      12  ERPLAN-FILE-STATUS      PIC XX              VALUE '00'.  ECS076
00128                                                                   ECS076
00129  01  CORE-NDX                    PIC X(3200).                     ECS076
00130                                                                   ECS076
00131  01  WS-SAVE-ALLOWABLE-BENEFITS.                                  ECS076
00132      12  WS-SAVE-BENEFIT-CODE     PIC XX.                         ECS076
00133      12  WS-SAVE-BENEFIT-TYPE     PIC X.                          ECS076
00134      12  WS-SAVE-BENEFIT-REVISION PIC XXX.                        ECS076
00135      12  FILLER                   PIC XX.                         ECS076
00136                                                                   ECS076
00137  01  WS.                                                          ECS076
00138      12  WS-RETURN-CODE          PIC S9(4)    COMP   VALUE ZERO.  ECS076
00139      12  WS-ZERO                 PIC S9              VALUE ZERO.  ECS076
00140      12  WS-ABEND-MESSAGE        PIC X(80)           VALUE SPACES.ECS076
00141      12  WS-ABEND-FILE-STATUS    PIC XX              VALUE ZERO.  ECS076
00142      12  X-REMIT                 PIC 99              VALUE ZEROS. ECS076
00143                                                                   ECS076
00144  01  WS-EXP-DT COMP-3.                                            ECS076
00145      12  FILLER                  PIC 9(3).                        ECS076
00146      12  WS-EXP-DT-FILL          PIC 9(8).                        ECS076
00147  EJECT                                                            ECS076
00148  01  PRT-LINES.                                                   ECS076
00149      12  HDR-1.                                                   ECS076
00150          16  FILLER          PIC X(55)           VALUE SPACES.    ECS076
00151          16  HD-1A           PIC X(22)           VALUE            ECS076
00152                  'ACCOUNT MASTER LISTING'.                        ECS076
00153          16  FILLER          PIC X(43)           VALUE SPACES.    ECS076
00154          16  FILLER          PIC X(7)            VALUE 'ECS076 '. ECS076
00155      12  HDR-2.                                                   ECS076
00156          16  FILLER          PIC X(52)           VALUE SPACES.    ECS076
00157          16  H2-COMPANY      PIC X(30).                           ECS076
00158          16  FILLER          PIC X(38)           VALUE SPACES.    ECS076
00159          16  H2-IPL          PIC X(8).                            ECS076
00160      12  HDR-3.                                                   ECS076
00161          16  FILLER          PIC X(58)           VALUE SPACES.    ECS076
00162          16  H3-DATE         PIC X(18).                           ECS076
00163          16  FILLER          PIC X(44)           VALUE SPACES.    ECS076
00164          16  FILLER          PIC X(5)            VALUE 'PAGE '.   ECS076
00165          16  H3-PAGE         PIC ZZ,ZZ9.                          ECS076
00166      12  HDR-5.                                                   ECS076
00167          16  FILLER          PIC X               VALUE '0'.       ECS076
00168          16  FILLER          PIC X(132)          VALUE ALL '*'.   ECS076
00169      12  DTL-1.                                                   ECS076
00170          16  FILLER      PIC X(10)   VALUE '0CARRIER  '.          ECS076
00171          16  D1-CARR     PIC X.                                   ECS076
00172          16  FILLER      PIC X(18)   VALUE '           NAME   '.  ECS076
00173          16  D1-NAME     PIC X(30).                               ECS076
00174          16  FILLER      PIC X(13)   VALUE ' EFFECT DATE '.       ECS076
00175          16  D1-EFF-DTE  PIC X(8).                                ECS076
00176          16  FILLER      PIC X(2)    VALUE SPACES.                ECS076
00177          16  D1-LF-OVRD1 PIC X(2).                                ECS076
00178          16  FILLER      PIC X(10)   VALUE ' DEV TBL= '.          ECS076
00179          16  D1-LF-DEV-T PIC XXX.                                 ECS076
00180          16  FILLER      PIC X(6)    VALUE ' PCT= '.              ECS076
00181          16  D1-LF-DEV-P PIC Z.999999.                            ECS076
00182          16  FILLER      PIC X(12)   VALUE SPACES.                ECS076
00183          16  D1-CC-HDR   PIC X(8).                                ECS076
00184          16  D1-CAL-CLS  PIC XX.                                  ECS076
00185      12  DTL-2.                                                   ECS076
00186          16  FILLER      PIC X(10)   VALUE ' GROUPING '.          ECS076
00187          16  D2-GROUP    PIC X(6).                                ECS076
00188          16  FILLER      PIC X(13)   VALUE '      CONTR  '.       ECS076
00189          16  D2-ADDR1    PIC X(30).                               ECS076
00190          16  FILLER      PIC X(13)   VALUE ' EXPIRE DATE '.       ECS076
00191          16  D2-EXP-DTE  PIC X(8).                                ECS076
00192          16  FILLER      PIC XX      VALUE SPACES.                ECS076
00193          16  D2-AH-OVRD1 PIC XX.                                  ECS076
00194          16  FILLER      PIC X(10)   VALUE ' DEV TBL= '.          ECS076
00195          16  D2-AH-DEV-T PIC X(3).                                ECS076
00196          16  FILLER      PIC X(6)    VALUE ' PCT= '.              ECS076
00197          16  D2-AH-DEV-P PIC Z.999999.                            ECS076
00198          16  FILLER      PIC X(22)   VALUE SPACES.                ECS076
00199      12  DTL-3.                                                   ECS076
00200          16  FILLER      PIC X(10)   VALUE ' STATE    '.          ECS076
00201          16  D3-STATE    PIC XX.                                  ECS076
00202          16  FILLER      PIC X(17)   VALUE '          ADDRS  '.   ECS076
00203          16  D3-ADDR2    PIC X(30).                               ECS076
00204          16  FILLER      PIC X(13)   VALUE ' CONTRACT DT '.       ECS076
00205          16  D3-CON-DTE  PIC X(8).                                ECS076
00206          16  FILLER      PIC XX      VALUE SPACES.                ECS076
00207          16  D3-LF-OVRD1 PIC XX.                                  ECS076
00208          16  FILLER      PIC X(10)   VALUE ' OB RATE= '.          ECS076
00209          16  D3-LF-OB    PIC 99.99999.                            ECS076
00210          16  FILLER      PIC X(5)    VALUE ' JNT '.               ECS076
00211          16  D3-LF-OVRD2 PIC XX.                                  ECS076
00212          16  FILLER      PIC XX      VALUE '= '.                  ECS076
00213          16  D3-JLF-OB   PIC 99.99999.                            ECS076
00214          16  FILLER      PIC X(14)   VALUE SPACES.                ECS076
00215      12  DTL-4.                                                   ECS076
00216          16  FILLER      PIC X(10)   VALUE ' ACCOUNT  '.          ECS076
00217          16  D4-ACCOUNT  PIC X(10).                               ECS076
00218          16  FILLER      PIC X(9)    VALUE '  CITYST '.           ECS076
00219          16  D4-ADDR3    PIC X(30).                               ECS076
00220          16  FILLER      PIC X(13)   VALUE ' 1ST PROD DT '.       ECS076
00221          16  D4-PROD-DTE PIC X(8).                                ECS076
00222          16  FILLER      PIC XX      VALUE SPACES.                ECS076
00223          16  D4-AH-OVRD1 PIC XX.                                  ECS076
00224          16  FILLER      PIC X(10)   VALUE ' OB RATE= '.          ECS076
00225          16  D4-AH-OB    PIC 99.99999.                            ECS076
00226          16  D4-SPECIAL.                                          ECS076
00227              20  D4-FILL1    PIC X(05)   VALUE ' JNT '.           ECS076
00228              20  D4-AH-OVRD2 PIC XX.                              ECS076
00229              20  D4-FILL2    PIC XX      VALUE '= '.              ECS076
00230              20  D4-JAH-OB   PIC 99.99999.                        ECS076
00231          16  FILLER      PIC X(8)    VALUE '  CITY= '.            ECS076
00232          16  D4-CITY     PIC XXXX.                                ECS076
00233          16  FILLER      PIC XX      VALUE SPACES.                ECS076
00234      12  DTL-5.                                                   ECS076
00235          16  FILLER      PIC X(10)   VALUE ' POL-TYPE '.          ECS076
00236          16  D5-POL-TYPE PIC X(10).                               ECS076
00237          16  FILLER      PIC X(9)    VALUE '  ZIP CD '.           ECS076
00238          16  D5-AMER-ZIP.                                         ECS076
00239              24  D5-ZIP          PIC X(5).                        ECS076
00240              24  D5-DASH         PIC X.                           ECS076
00241              24  D5-ZIP-EXT      PIC X(4).                        ECS076
00242          16  D5-CANADIAN-POSTAL-CODE REDEFINES D5-AMER-ZIP.       ECS076
00243              24  D5-CAN-POSTAL-CODE-1                             ECS076
00244                                  PIC X(3).                        ECS076
00245              24  D5-DASH-CAN     PIC X.                           ECS076
00246              24  D5-CAN-POSTAL-CODE-2                             ECS076
00247                                  PIC X(3).                        ECS076
00248              24  D5-CAN-FILLER   PIC X(3).                        ECS076
00249          16  FILLER      PIC X(9)    VALUE '   PHONE '.           ECS076
00250          16  D5-TEL-LOC  PIC X(8).                                ECS076
00251          16  FILLER      PIC X(16)   VALUE '    HI  CERT DT'.     ECS076
00252          16  D5-HCRT-DTE PIC X(8).                                ECS076
00253          16  FILLER      PIC X(9)    VALUE '  STATE= '.           ECS076
00254          16  D5-ST-NAME  PIC X(15).                               ECS076
00255          16  FILLER      PIC X(19)   VALUE '   COUNTY/PARISH='.   ECS076
00256          16  D5-COUNTY   PIC X(6).                                ECS076
00257      12  DTL-6.                                                   ECS076
00258          16  FILLER      PIC X(10)   VALUE ' STATUS   '.          ECS076
00259          16  D6-STATUS   PIC X(8).                                ECS076
00260          16  FILLER      PIC X(11)   VALUE '    TAX ID '.         ECS076
00261          16  D6-TAX-ID   PIC X(11).                               ECS076
00262          16  FILLER      PIC XX      VALUE SPACES.                ECS076
00263          16  D6-AR-CD.                                            ECS076
00264              20  D6-LP   PIC X.                                   ECS076
00265              20  D6-AC   PIC 999.                                 ECS076
00266              20  D6-RP   PIC X.                                   ECS076
00267          16  FILLER      PIC X       VALUE SPACES.                ECS076
00268          16  D6-TEL-NBR.                                          ECS076
00269              20  D6-PRE  PIC 999.                                 ECS076
00270              20  D6-DASH PIC X.                                   ECS076
00271              20  D6-NBR  PIC 9(4).                                ECS076
00272          16  FILLER      PIC X(16)   VALUE '    LOW CERT DT'.     ECS076
00273          16  D6-LCRT-DTE PIC X(8).                                ECS076
00274          16  FILLER      PIC X(15)   VALUE '  USER FIELDS= '.     ECS076
00275          16  D6-USER-1   PIC XX.                                  ECS076
00276          16  FILLER      PIC X       VALUE '-'.                   ECS076
00277          16  D6-USER-2   PIC XX.                                  ECS076
00278          16  FILLER      PIC X       VALUE '-'.                   ECS076
00279          16  D6-USER-3   PIC XX.                                  ECS076
00280          16  FILLER      PIC X       VALUE '-'.                   ECS076
00281          16  D6-USER-4   PIC XX.                                  ECS076
00282          16  FILLER      PIC X       VALUE '-'.                   ECS076
00283          16  D6-USER-5   PIC XX.                                  ECS076
00284          16  FILLER      PIC X(13)   VALUE '     DEFAULT '.       ECS076
00285          16  D6-AH-OVRD1 PIC XX.                                  ECS076
00286          16  FILLER      PIC X(7)    VALUE ' TYPE= '.             ECS076
00287          16  D6-STD-AH-TYPE                                       ECS076
00288                          PIC XX      VALUE SPACES.                ECS076
00289      12  DTL-7.                                                   ECS076
00290          16  FILLER      PIC X(22)   VALUE SPACES.                ECS076
00291          16  FILLER      PIC X(11)   VALUE 'LAST MAINT '.         ECS076
00292          16  D7-MNT-DTE  PIC X(8).                                ECS076
00293          16  FILLER      PIC X       VALUE SPACE.                 ECS076
00294          16  D7-MNT-TIME PIC X(8).                                ECS076
00295          16  FILLER      PIC XXXX    VALUE ' BY '.                ECS076
00296          16  D7-MNT-BY   PIC XXXX.                                ECS076
00297          16  FILLER      PIC X(24)   VALUE SPACES.                ECS076
00298          16  FILLER      PIC X(9)    VALUE 'BUS TYPE '.           ECS076
00299          16  D7-BUS-TYPE PIC XX.                                  ECS076
00300          16  FILLER      PIC X       VALUE SPACES.                ECS076
00301          16  D7-BUS-DESC PIC X(24).                               ECS076
00302          16  FILLER      PIC X(13)   VALUE '  REMIT LVL= '.       ECS076
00303          16  D7-REM-LVL  PIC ZZ.                                  ECS076
00304      12  DTL-8.                                                   ECS076
00305          16  FILLER      PIC X       VALUE '0'.                   ECS076
00306          16  D8-TOLERANC PIC X(15).                               ECS076
00307          16  D8-REFUND   PIC X(26).                               ECS076
00308          16  D8-REIN     PIC X(32).                               ECS076
00309          16  D8-RETRO1   PIC X(35).                               ECS076
00310          16  D8-RETRO2   PIC X(24).                               ECS076
00311      12  DTL-13.                                                  ECS076
00312          16  FILLER          PIC X(51)           VALUE            ECS076
00313             '0*--- C O M M I S S I O N   S T R U C T U R E ----*'.ECS076
00314          16  FILLER          PIC X(51)           VALUE            ECS076
00315             '   *---------------- C O M M E N T S --------------'.ECS076
00316          16  FILLER          PIC X(7)            VALUE            ECS076
00317                  '----*  '.                                       ECS076
00318          16  D13-RETRO1      PIC X(24).                           ECS076
00319      12  DTL-14.                                                  ECS076
00320          16  FILLER          PIC X(30)           VALUE            ECS076
00321             ' LVL  AGENT     SINGLE JOINT'.                       ECS076
00322          16  D14-AH-OVRD1    PIC X(6).                            ECS076
00323          16  FILLER          PIC X(24)           VALUE            ECS076
00324              ' TYP RET REC GL NC  LINE'.
00325          16  FILLER          PIC X(49)           VALUE SPACE.     ECS076
00326          16  D14-RETRO1      PIC X(24).                           ECS076
00327      12  DTL-15.                                                  ECS076
00328          16  FILLER          PIC X.                               ECS076
00329          16  D15-LEVEL       PIC Z9.                              ECS076
00330          16  FILLER          PIC XX.                              ECS076
00331          16  D15-AGENT       PIC X(10).                           ECS076
00332          16  FILLER          PIC X.                               ECS076
00333          16  D15-SINGL       PIC 99.999-.                         ECS076
00334          16  D15-SINGL-R REDEFINES                                ECS076
00335              D15-SINGL       PIC X(7).                            ECS076
00336          16  D15-JOINT       PIC 99.999-.                         ECS076
00337          16  D15-JOINT-R REDEFINES                                ECS076
00338              D15-JOINT       PIC X(7).                            ECS076
00339          16  D15-AH          PIC 99.999-.                         ECS076
00340          16  D15-AH-R REDEFINES                                   ECS076
00341              D15-AH          PIC X(7).                            ECS076
00342          16  FILLER          PIC X.                               ECS076
00343          16  D15-TYPE        PIC X.                               ECS076
00344          16  FILLER          PIC XXX.                             ECS076
00345          16  D15-RET-LEV     PIC X.                               ECS076
00346          16  FILLER          PIC XXX.                             ECS076
00347          16  D15-REC-LEV     PIC X.                               ECS076
00348          16  FILLER          PIC XXX.                             ECS076
00349          16  D15-GL-CODE     PIC X.                               ECS076
00350          16  FILLER          PIC X.
122806         16  D15-NCB         PIC XX.
               16  FILLER          PIC X.
00351          16  D15-LINE        PIC 9.                               ECS076
00352          16  FILLER          PIC X.                               ECS076
00353          16  D15-COMMENT     PIC X(50).                           ECS076
00354          16  FILLER          PIC XX              VALUE SPACES.    ECS076
00355          16  D15-RETRO1      PIC X(24).                           ECS076
00356      12  DTL-16.                                                  ECS076
00357          16  FILLER          PIC X(51)           VALUE            ECS076
00358             '0*---  L I M I T S :   L I F E   B E N E F I T S  -'.ECS076
00359          16  FILLER          PIC X(51)           VALUE            ECS076
00360             '---------------------------------------------------'.ECS076
00361          16  FILLER          PIC X(7)            VALUE            ECS076
00362             '----*  '.                                            ECS076
00363      12  DTL-17.                                                  ECS076
00364          16  FILLER          PIC X(51)           VALUE            ECS076
00365            ' BENE  *MAX-AGE*    *AGE-TERM-BENEFIT*   *AGE TERM-'. ECS076
00366          16  FILLER          PIC X(82)           VALUE            ECS076
00367            'BENEFIT*   *AGE-TERM-BENEFIT*   *AGE-TERM-BENEFIT*'.  ECS076
00368      12  DTL-18.                                                  ECS076
00369          16  FILLER          PIC XX              VALUE SPACES.    ECS076
00370          16  D18-BENE        PIC XX.                              ECS076
00371          16  FILLER          PIC X(6)            VALUE SPACES.    ECS076
00372          16  D18-MAX-AGE     PIC Z9-.                             ECS076
00373          16  FILLER          PIC X(9)            VALUE SPACES.    ECS076
00374          16  D18-LIMITS.                                          ECS076
00375              20  D18-LIMIT-LEVELS OCCURS 4.                       ECS076
00376                  24  D18-AGE PIC Z9-.                             ECS076
00377                  24  FILLER  PIC X.                               ECS076
00378                  24  D18-TRM PIC Z99-.                            ECS076
00379                  24  D18-BEN PIC ZZZZ,999-.                       ECS076
00380                  24  FILLER  PIC XXXX.                            ECS076
00381          16  FILLER          PIC X(27)           VALUE SPACES.    ECS076
00382      12  DTL-19.                                                  ECS076
00383          16  FILLER          PIC X(51)           VALUE            ECS076
00384             '0*---  L I M I T S :   A & H   B E N E F I T S  ---'.ECS076
00385          16  FILLER          PIC X(51)           VALUE            ECS076
00386             '---------------------------------------------------'.ECS076
00387          16  FILLER          PIC X(31)           VALUE            ECS076
00388             '----------------------------*  '.                    ECS076
00389      12  DTL-20.                                                  ECS076
00390          16  FILLER          PIC X(45)           VALUE            ECS076
00391                  ' BENE  *MAX-AGE*    *AGE-TERM-MO/AMT-BENEFIT*'. ECS076
00392          16  FILLER          PIC X(44)           VALUE            ECS076
00393                  '   *AGE-TERM-MO/AMT-BENEFIT*   *AGE-TERM-MO/'.  ECS076
00394          16  FILLER          PIC X(44)           VALUE            ECS076
00395                  'AMT-BENEFIT*   *AGE-TERM-MO/AMT-BENEFIT*    '.  ECS076
00396      12  DTL-21.                                                  ECS076
00397          16  FILLER          PIC XX              VALUE SPACES.    ECS076
00398          16  D21-BENE        PIC XX.                              ECS076
00399          16  FILLER          PIC X(6)            VALUE SPACES.    ECS076
00400          16  D21-MAX-AGE     PIC Z9-.                             ECS076
00401          16  FILLER          PIC X(9)            VALUE SPACES.    ECS076
00402          16  D21-LIMITS.                                          ECS076
00403              20  D21-LIMIT-LEVELS OCCURS 4.                       ECS076
00404                  24  D21-AGE PIC Z9-.                             ECS076
00405                  24  FILLER  PIC X.                               ECS076
00406                  24  D21-TRM PIC Z99-.                            ECS076
00407                  24  D21-AMT PIC ZZZ99-.                          ECS076
00408                  24  D21-BEN PIC ZZZZ,999-.                       ECS076
00409                  24  FILLER  PIC X(5).                            ECS076
00410          16  FILLER          PIC X(4)            VALUE SPACES.    ECS076
00411      12  DTL-22.                                                  ECS076
00412          16  FILLER      PIC X(10)   VALUE ' DEV TBL= '.          ECS076
00413          16  D22-PL-DEV-T PIC X(3).                               ECS076
00414          16  FILLER      PIC X(6)    VALUE ' PCT= '.              ECS076
00415          16  D22-PL-DEV-P PIC Z.999999.                           ECS076
00416          16  D22-PL-DEV-R REDEFINES                               ECS076
00417              D22-PL-DEV-P PIC X(8).                               ECS076
00418          16  FILLER      PIC X(22)   VALUE SPACES.                ECS076
00419                                                                   ECS076
00420                                                                   ECS076
00421  01  PRT-TABLE.                                                   ECS076
00422      12  PRT-LINE        OCCURS 80.                               ECS076
00423          16  PRT-SW          PIC X.                               ECS076
00424          16  FILLER          PIC X(132).                          ECS076
00425  EJECT                                                            ECS076
00426  01  DATE-SETS.                                                   ECS076
00427      12  DS-MO               PIC XX.                              ECS076
00428      12  FILLER              PIC X               VALUE '/'.       ECS076
00429      12  DS-DA               PIC XX.                              ECS076
00430      12  FILLER              PIC X               VALUE '/'.       ECS076
00431      12  DS-YR               PIC XX.                              ECS076
00432                                                                   ECS076
00433  01  TIME-SETS.                                                   ECS076
00434      12  TS-HH               PIC XX.                              ECS076
00435      12  FILLER              PIC X               VALUE '.'.       ECS076
00436      12  TS-MM               PIC XX.                              ECS076
00437      12  FILLER              PIC X               VALUE '.'.       ECS076
00438      12  TS-SS               PIC XX.                              ECS076
00439                                                                   ECS076
050303 01  REPORT-CODES.
050303     12  FILLER              PIC X(10)  VALUE ' RPTCD1   '.
050303     12  DET-RPTCD1          PIC X(12)  VALUE SPACES.
050303     12  FILLER              PIC X(7)   VALUE 'RPTCD2 '.
050303     12  DET-RPTCD2          PIC X(10)  VALUE SPACES.
120806     12  FILLER              PIC X(21)  VALUE SPACES.
120806     12  FILLER              PIC X(7)   VALUE 'RPTCD3 '.
120806     12  DET-RPTCD3          PIC X(10)  VALUE SPACES.
122806     12  FILLER              PIC X(5)   VALUE SPACES.
122806     12  FILLER              PIC X(12)  VALUE 'ORIG DLR NO '.
122806     12  DET-ORIG-DLR-NO     PIC X(10)  VALUE SPACES.
050303
00440  01  TOLERANCE-MESSAGES.                                          ECS076
00441      12  TOLERANCE-MSG-1                                          ECS076
00442                              PIC X(13) VALUE '* TOLERANCES*'.     ECS076
00443      12  TOLERANCE-MSG-2.                                         ECS076
00444          16  TOLERANCE-TYPE.                                      ECS076
00445              20  TOL-LF-OVRD PIC XX.                              ECS076
00446              20  TOL-CLM     PIC X(5).                            ECS076
00447          16  TOLERANCE-AMT   PIC Z99.99.                          ECS076
00448          16  TOL-AMT-R  REDEFINES TOLERANCE-AMT                   ECS076
00449                              PIC X(6).                            ECS076
00450                                                                   ECS076
00451  01  RETRO1-MESSAGES.                                             ECS076
00452      12  RETRO1-MSG-1.                                            ECS076
00453          16  FILLER     PIC X(21) VALUE '*-- RETROACTIVES ARE '.  ECS076
00454          16  R1M1-D1    PIC X(9).                                 ECS076
00455          16  FILLER     PIC XX    VALUE '-*'.                     ECS076
00456      12  RETRO1-MSG-2.                                            ECS076
00457          16  FILLER     PIC X(14) VALUE 'CALCULATED ON '.         ECS076
00458          16  R1M2-D1    PIC X(18).                                ECS076
00459      12  RETRO1-MSG-3.                                            ECS076
00460          16  FILLER     PIC X(10) VALUE 'RETENTION '.             ECS076
00461          16  R1M3-OVRD1 PIC XX.                                   ECS076
00462          16  FILLER     PIC XX    VALUE '= '.                     ECS076
00463          16  R1M3-LF-RT PIC Z.9999-.                              ECS076
00464          16  FILLER     PIC X     VALUE SPACE.                    ECS076
00465          16  R1M3-OVRD2 PIC XX.                                   ECS076
00466          16  FILLER     PIC XX    VALUE '= '.                     ECS076
00467          16  R1M3-AH-RT PIC Z.9999-.                              ECS076
00468      12  RETRO1-MSG-4.                                            ECS076
00469          16  FILLER     PIC X(15) VALUE 'MIN LOSS RATIO '.        ECS076
00470          16  R1M4-OVRD1 PIC XX.                                   ECS076
00471          16  FILLER     PIC XX    VALUE '= '.                     ECS076
00472          16  R1M4-LF-ML PIC .999.                                 ECS076
00473          16  FILLER     PIC X     VALUE SPACE.                    ECS076
00474          16  R1M4-OVRD2 PIC XX.                                   ECS076
00475          16  FILLER     PIC XX    VALUE '= '.                     ECS076
00476          16  R1M4-AH-ML PIC .999.                                 ECS076
00477      12  RETRO1-MSG-5.                                            ECS076
00478          16  FILLER     PIC X(10) VALUE 'GROUP CD= '.             ECS076
00479          16  R1M5-GP-CD PIC X(6).                                 ECS076
00480          16  FILLER     PIC X(15) VALUE '   ST TAX USE= '.        ECS076
00481          16  R1M5-TX-US PIC X.                                    ECS076
00482                                                                   ECS076
00483  01  EARNING-METHOD-MESSAGES.                                     ECS076
00484      12  EARN-METH-MSG.                                           ECS076
00485          16  EMM-D1                PIC X(4).                      ECS076
00486          16  EMM-D2                PIC X(20).                     ECS076
00487      12  REFUND-EARN-METH-MSG      PIC X(24)   VALUE              ECS076
00488                             '* REFUND EARNING METH -*'.           ECS076
00489      12  RETRO-EARN-METH-MSG       PIC X(24)   VALUE              ECS076
00490                             '* RETRO EARNING METHOD *'.           ECS076
00491      12  RETRO-BEG-EARN-METH-MSG   PIC X(24)   VALUE              ECS076
00492                             '* RETRO BEG EARN METH -*'.           ECS076
00493                                                                   ECS076
00494  01  REIN-MESSAGES.                                               ECS076
00495      12  REIN-MSG-1.                                              ECS076
00496          16  FILLER          PIC X(11)   VALUE 'REINSURANCE'.     ECS076
00497          16  FILLER          PIC X(6)    VALUE ' TBL= '.          ECS076
00498          16  R1-TBL          PIC XXX.                             ECS076
00499          16  FILLER          PIC X       VALUE SPACE.             ECS076
00500          16  R1-LF-OVRD      PIC XX.                              ECS076
00501          16  FILLER          PIC X       VALUE '='.               ECS076
00502          16  R1-LF-PE        PIC X.                               ECS076
00503          16  FILLER          PIC X       VALUE SPACE.             ECS076
00504          16  R1-AH-OVRD      PIC XX.                              ECS076
00505          16  FILLER          PIC X       VALUE '='.               ECS076
00506          16  R1-AH-PE        PIC X.                               ECS076
00507          16  FILLER          PIC X(2)    VALUE SPACES.            ECS076
00508      12  REIN-MSG-2.                                              ECS076
00509          16  FILLER          PIC XXX     VALUE 'ST='.             ECS076
00510          16  R2-ST           PIC X.                               ECS076
00511          16  FILLER          PIC X       VALUE SPACE.             ECS076
00512          16  R2-AH-OVRD-1    PIC XX.                              ECS076
00513          16  FILLER          PIC X(4)    VALUE '-78='.            ECS076
00514          16  R2-AH-78        PIC Z.9999-.                         ECS076
00515          16  R2-AH-OVRD-2    PIC XX.                              ECS076
00516          16  FILLER          PIC X(4)    VALUE '-PR='.            ECS076
00517          16  R2-AH-PR        PIC Z.9999-.                         ECS076
00518      12  REIN-MSG-3.                                              ECS076
00519          16  FILLER          PIC XXX     VALUE 'OW='.             ECS076
00520          16  R3-OW           PIC X.                               ECS076
00521          16  FILLER          PIC X(6)    VALUE ' MORT='.          ECS076
00522          16  R3-MORT         PIC X(4).                            ECS076
00523          16  FILLER          PIC X(17)   VALUE                    ECS076
00524                                      '     FEE    TAX  '.         ECS076
00525      12  REIN-MSG-4.                                              ECS076
00526          16  FILLER          PIC X(6)    VALUE 'GRP-A='.          ECS076
00527          16  R4-GRP-A        PIC X(6).                            ECS076
00528          16  FILLER          PIC XX      VALUE SPACE.             ECS076
00529          16  R4-LF-OVRD      PIC XX.                              ECS076
00530          16  FILLER          PIC X       VALUE SPACE.             ECS076
00531          16  R4-LF-FEE       PIC Z.9999-.                         ECS076
00532          16  R4-LF-TAX       PIC Z.9999-.                         ECS076
00533      12  REIN-MSG-5.                                              ECS076
00534          16  FILLER          PIC X(6)    VALUE 'GRP-B='.          ECS076
00535          16  R5-GRP-B        PIC X(6).                            ECS076
00536          16  FILLER          PIC XX      VALUE SPACE.             ECS076
00537          16  R5-AH-OVRD      PIC XX.                              ECS076
00538          16  FILLER          PIC X       VALUE SPACE.             ECS076
00539          16  R5-AH-FEE       PIC Z.9999-.                         ECS076
00540          16  R5-AH-TAX       PIC Z.9999-.                         ECS076
00541                                                                   ECS076
00542  01  LIAB-LIMITS-MSGS.                                            ECS076
00543      12  LIAB-LIMIT-STD-MSG.                                      ECS076
00544          16  FILLER          PIC X               VALUE '0'.       ECS076
00545          16  STD-MSG-OVRD    PIC X(6).                            ECS076
00546          16  FILLER          PIC X(37)           VALUE            ECS076
00547              ' STATE STANDARD LIABILITY LIMITS USED'.             ECS076
00548      12  EXCPT-MSG.                                               ECS076
00549          16  FILLER          PIC X               VALUE '0'.       ECS076
00550          16  EXC-MSG-OVRD    PIC X(6).                            ECS076
00551          16  FILLER          PIC X(27)           VALUE            ECS076
00552              ' LIABILITY LIMIT EXCEPTIONS'.                       ECS076
00553  EJECT                                                            ECS076
00554                              COPY ERCACCT.                        ECS076
00555  EJECT                                                            ECS076
00556                              COPY ELCACCTV.                       ECS076
00557  EJECT                                                            ECS076
00558                              COPY ELCDTECX.                       ECS076
00559  EJECT                                                            ECS076
00560                              COPY ELCDTEVR.                       ECS076
00561  EJECT                                                            ECS076
00562                              COPY ELCDATE.                           CL**3
00563  EJECT                                                            ECS076
00564  PROCEDURE DIVISION.                                              ECS076
00565                                                                   ECS076
00566  0002-DATE-READ.                                                  ECS076
00567                              COPY ELCDTERX.                       ECS076
00568  EJECT                                                            ECS076
00569  0100-OPEN-FILES.                                                 ECS076
00570      MOVE COMPANY-NAME           TO H2-COMPANY.                   ECS076
00571                                                                   ECS076
00572      ACCEPT WS-ACCEPT-DATE       FROM  DATE.                      ECS076
00573                                                                   ECS076
00574      MOVE WS-AD-YY               TO WS-CD-YY.                     ECS076
00575      MOVE WS-AD-MM               TO WS-CD-MM.                     ECS076
00576      MOVE WS-AD-DD               TO WS-CD-DD.                     ECS076
00577      MOVE WS-CURRENT-DATE        TO H2-IPL.                       ECS076
00578      MOVE ALPH-DATE              TO H3-DATE.                      ECS076
00579      MOVE SPACES                 TO PRT-TABLE.                    ECS076
00580                                                                   ECS076
00581      OPEN INPUT  PLAN-MSTR.                                       ECS076
00582                                                                   ECS076
00583      IF ERPLAN-FILE-STATUS  NOT = '00' AND '97'                   ECS076
00584          MOVE 'ERROR OCCURED OPEN - ERPLAN'   TO  WS-ABEND-MESSAGEECS076
00585          MOVE ERPLAN-FILE-STATUS  TO  WS-ABEND-FILE-STATUS        ECS076
00586          PERFORM ABEND-PGM.                                       ECS076
00587                                                                   ECS076
00588      OPEN OUTPUT PRNTR.                                           ECS076
00589                                                                   ECS076
00590      IF DTE-PGM-OPT = '2'                                         ECS076
00591         OPEN INPUT ACCT-SEQUENTIAL                                ECS076
00592         GO TO 0200-READ-LOOP.                                     ECS076
00593                                                                   ECS076
00594      OPEN INPUT  ACC-MSTR.                                        ECS076
00595                                                                   ECS076
00596      IF ERACCT-FILE-STATUS  NOT = '00' AND '97'                   ECS076
00597          MOVE 'ERROR OCCURED OPEN - ERACCTT'  TO  WS-ABEND-MESSAGEECS076
00598          MOVE ERACCT-FILE-STATUS  TO  WS-ABEND-FILE-STATUS        ECS076
00599          PERFORM ABEND-PGM.                                       ECS076
00600                                                                   ECS076
00601      MOVE LOW-VALUES             TO AM-CONTROL-PRIMARY.           ECS076
00602      MOVE DTE-CLASIC-COMPANY-CD  TO AM-COMPANY-CD.                ECS076
00603      MOVE AM-CONTROL-PRIMARY     TO ACC-MSTR-KEY.                 ECS076
00604                                                                   ECS076
00605      START ACC-MSTR                                               ECS076
00606          KEY IS GREATER THAN ACC-MSTR-KEY                         ECS076
00607          INVALID KEY                                              ECS076
00608              MOVE HIGH-VALUES    TO AM-MSTR-CNTRL                 ECS076
00609              PERFORM 9200-HDR-RTN-A                               ECS076
00610              MOVE '0'            TO X                             ECS076
00611              MOVE 'NO INPUT RECORDS ON FILE TO BE PRINTED'        ECS076
00612                                  TO P-DATA                        ECS076
00613              PERFORM 9300-PRT-RTN THRU 9399-PRT-RTN-EXIT          ECS076
00614              GO TO 9000-END-JOB.                                  ECS076
00615                                                                   ECS076
00616      IF ERACCT-FILE-STATUS NOT = ZERO                             ECS076
00617          MOVE 'ERROR OCCURED START - ERACCTT'  TO WS-ABEND-MESSAGEECS076
00618          MOVE ERACCT-FILE-STATUS  TO  WS-ABEND-FILE-STATUS        ECS076
00619          PERFORM ABEND-PGM.                                       ECS076
00620                                                                   ECS076
00621  0200-READ-LOOP.                                                  ECS076
00622      IF DTE-PGM-OPT NOT = '2'                                     ECS076
00623          GO TO 0210-READ-VSAM-INPUT.                              ECS076
00624                                                                   ECS076
00625      READ ACCT-SEQUENTIAL  INTO  ACCOUNT-MASTER  AT END           ECS076
00626          GO TO 9000-END-JOB.                                      ECS076
00627                                                                   ECS076
00628      COPY ELCACCTI.                                               ECS076
00629                                                                   ECS076
00630      IF FIRST-TIME                                                ECS076
00631          MOVE 'N'                TO  FIRST-TIME-SWITCH            ECS076
00632          MOVE +1                 TO  X1                           ECS076
00633          GO TO 0300-C-C-C.                                        ECS076
00634                                                                   ECS076
00635      GO TO 1000-BUILD-PRT.                                        ECS076
00636                                                                   ECS076
00637  0210-READ-VSAM-INPUT.                                            ECS076
00638      READ ACC-MSTR.                                               ECS076
00639                                                                   ECS076
00640      IF ERACCT-FILE-STATUS = '10'                                 ECS076
00641          GO TO 9000-END-JOB.                                      ECS076
00642                                                                   ECS076
00643      IF ERACCT-FILE-STATUS NOT EQUAL TO ZERO                      ECS076
00644          MOVE 'ERROR OCCURED READ - ERACCTT'                      ECS076
00645                                  TO  WS-ABEND-MESSAGE             ECS076
00646          MOVE ERACCT-FILE-STATUS                                  ECS076
00647                                  TO  WS-ABEND-FILE-STATUS         ECS076
00648          PERFORM ABEND-PGM.                                       ECS076
00649                                                                   ECS076
00650      MOVE ACC-MSTR-REC           TO  ACCOUNT-MASTER.              ECS076
00651                                                                   ECS076
00652      COPY ELCACCTI.                                               ECS076
00653                                                                   ECS076
00654      IF AM-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD                 ECS076
00655          GO TO 9000-END-JOB.                                      ECS076
00656                                                                   ECS076
00657      IF FIRST-TIME                                                ECS076
00658          MOVE 'N'                TO  FIRST-TIME-SWITCH            ECS076
00659          MOVE +1                 TO  X1                           ECS076
00660          GO TO 0300-C-C-C.                                        ECS076
00661                                                                   ECS076
00662      GO TO 1000-BUILD-PRT.                                        ECS076
00663                                                                   ECS076
00664  0300-C-C-C.                                                      ECS076
00665      IF STATE-ABBR (X1) = 'CA'                                    ECS076
00666          MOVE STATE-SUB (X1)     TO CAL-ST-CD                     ECS076
00667          MOVE +1                 TO X1                            ECS076
00668          GO TO 0310-C-C-C.                                        ECS076
00669                                                                   ECS076
00670      ADD +1 TO X1.                                                ECS076
00671                                                                   ECS076
00672      IF STATE-SUB (X1) NOT = HIGH-VALUES                          ECS076
00673          GO TO 0300-C-C-C.                                        ECS076
00674                                                                   ECS076
00675      MOVE +1                     TO X1.                           ECS076
00676                                                                   ECS076
00677  0310-C-C-C.                                                      ECS076
00678      IF STATE-ABBR (X1) = 'UT'                                    ECS076
00679          MOVE STATE-SUB (X1)     TO UTAH-ST-CD                    ECS076
00680          MOVE +0                 TO X1                            ECS076
00681          GO TO 1000-BUILD-PRT.                                    ECS076
00682                                                                   ECS076
00683      ADD +1                      TO X1.                           ECS076
00684                                                                   ECS076
00685      IF STATE-SUB (X1) NOT = HIGH-VALUES                          ECS076
00686          GO TO 0310-C-C-C.                                        ECS076
00687                                                                   ECS076
00688      MOVE +0                     TO X1.                           ECS076
00689                                                                   ECS076
00690  1000-BUILD-PRT.                                                  ECS076
00691                                                                   ECS076
00692  1200-DTL-1.                                                      ECS076
00693      MOVE AM-CARRIER             TO D1-CARR.                      ECS076
00694      MOVE AM-NAME                TO D1-NAME.                      ECS076
00695                                                                   ECS076
00696      IF AM-EFFECT-DT NOT NUMERIC                                  ECS076
00697          MOVE AM-EFFECTIVE-DT        TO DC-BIN-DATE-1             ECS076
00698          MOVE SPACE                  TO DC-OPTION-CODE            ECS076
00699          PERFORM 8500-DATE-CONVERSION                             ECS076
00700          MOVE DC-GREG-DATE-1-EDIT    TO D1-EFF-DTE                ECS076
00701      ELSE                                                         ECS076
00702          MOVE AM-EFF-MO              TO DS-MO                     ECS076
00703          MOVE AM-EFF-DA              TO DS-DA                     ECS076
00704          MOVE AM-EFF-YR              TO DS-YR                     ECS076
00705          MOVE DATE-SETS              TO D1-EFF-DTE.               ECS076
00706                                                                   ECS076
00707      MOVE LIFE-OVERRIDE-L2       TO D1-LF-OVRD1.                  ECS076
00708      MOVE AM-LF-DEVIATION        TO D1-LF-DEV-T.                  ECS076
00709      MOVE AM-LF-DEVIATION-PCT    TO D1-LF-DEV-P.                  ECS076
00710                                                                   ECS076
00711      IF AM-STATE = CAL-ST-CD                                      ECS076
00712          MOVE 'CLASS = '         TO D1-CC-HDR                     ECS076
00713          MOVE AM-CAL-TABLE       TO D1-CAL-CLS                    ECS076
00714      ELSE                                                         ECS076
00715          MOVE SPACES             TO D1-CC-HDR  D1-CAL-CLS.        ECS076
00716                                                                   ECS076
00717      ADD +1                      TO X1.                           ECS076
00718      MOVE DTL-1                  TO PRT-LINE (X1).                ECS076
00719      ADD +2                      TO WS-LINE.                      ECS076
00720                                                                   ECS076
00721  1300-DTL-2.                                                      ECS076
00722      MOVE AM-GROUPING            TO D2-GROUP.                     ECS076
00723      MOVE AM-PERSON              TO D2-ADDR1.                     ECS076
00724      MOVE AM-EXPIRE-DT               TO WS-EXP-DT.                ECS076
00725                                                                   ECS076
00726      IF WS-EXP-DT-FILL NOT NUMERIC                                ECS076
00727          MOVE AM-EXPIRATION-DT       TO DC-BIN-DATE-1             ECS076
00728          MOVE SPACE                  TO DC-OPTION-CODE            ECS076
00729          PERFORM 8500-DATE-CONVERSION                             ECS076
00730          MOVE DC-GREG-DATE-1-EDIT    TO D2-EXP-DTE                ECS076
00731      ELSE                                                         ECS076
00732          MOVE AM-EXP-MO              TO DS-MO                     ECS076
00733          MOVE AM-EXP-DA              TO DS-DA                     ECS076
00734          MOVE AM-EXP-YR              TO DS-YR                     ECS076
00735          MOVE DATE-SETS              TO D2-EXP-DTE.               ECS076
00736                                                                   ECS076
00737      IF AM-EXPIRE-DT = 999999999 OR                               ECS076
00738                    AM-EXPIRATION-DT = HIGH-VALUES                 ECS076
00739          MOVE 'CURRENT'          TO D2-EXP-DTE.                   ECS076
00740                                                                   ECS076
00741      MOVE AH-OVERRIDE-L2         TO D2-AH-OVRD1.                  ECS076
00742      MOVE AM-AH-DEVIATION        TO D2-AH-DEV-T.                  ECS076
00743      MOVE AM-AH-DEVIATION-PCT    TO D2-AH-DEV-P.                  ECS076
00744                                                                   ECS076
00745      ADD +1                      TO X1.                           ECS076
00746      MOVE DTL-2                  TO PRT-LINE (X1).                ECS076
00747      ADD +2                      TO WS-LINE.                      ECS076
00748                                                                   ECS076
00749  1400-DTL-3.                                                      ECS076
00750      MOVE AM-STATE               TO D3-STATE.                     ECS076
00751      MOVE AM-ADDRS               TO D3-ADDR2.                     ECS076
00752      MOVE AM-AN-MO               TO DS-MO.                        ECS076
00753      MOVE AM-AN-DA               TO DS-DA.                        ECS076
00754      MOVE AM-AN-YR               TO DS-YR.                        ECS076
00755      MOVE DATE-SETS              TO D3-CON-DTE.                   ECS076
00756      MOVE LIFE-OVERRIDE-L2       TO D3-LF-OVRD1, D3-LF-OVRD2.     ECS076
00757      MOVE AM-LF-OB-RATE          TO D3-LF-OB.                     ECS076
00758      MOVE AM-LF-OB-RATE-JNT      TO D3-JLF-OB.                    ECS076
00759                                                                   ECS076
00760      ADD +1                      TO X1.                           ECS076
00761      MOVE DTL-3                  TO PRT-LINE (X1).                ECS076
00762      ADD +1                      TO WS-LINE.                      ECS076
00763                                                                   ECS076
00764  1500-DTL-4.                                                      ECS076
00765      MOVE AM-ACCOUNT             TO D4-ACCOUNT.                   ECS076
051810     MOVE SPACES                 TO D4-ADDR3
051810     STRING AM-ADDR-CITY ' ' AM-ADDR-STATE
051810        DELIMITED BY '  ' INTO D4-ADDR3
051810     END-STRING
00767                                                                   ECS076
00768      IF AM-1ST-PROD-DATE = ZEROS  OR  SPACES                      ECS076
00769          MOVE '            '     TO D4-PROD-DTE                   ECS076
00770      ELSE                                                         ECS076
00771          MOVE AM-1ST-PROD-MO     TO DS-MO                         ECS076
00772          MOVE AM-1ST-PROD-DA     TO DS-DA                         ECS076
00773          MOVE AM-1ST-PROD-YR     TO DS-YR                         ECS076
00774          MOVE DATE-SETS          TO D4-PROD-DTE.                  ECS076
00775                                                                   ECS076
00776      MOVE AH-OVERRIDE-L2         TO D4-AH-OVRD1.                  ECS076
00777      MOVE AM-AH-OB-RATE          TO D4-AH-OB.                     ECS076
00778                                                                   ECS076
00779      IF AM-AH-OB-RATE-JNT GREATER THAN ZERO                       ECS076
00780          MOVE ' JNT '            TO D4-FILL1                      ECS076
00781          MOVE '= '               TO D4-FILL2                      ECS076
00782          MOVE AH-OVERRIDE-L2     TO D4-AH-OVRD2                   ECS076
00783          MOVE AM-AH-OB-RATE-JNT  TO D4-JAH-OB                     ECS076
00784      ELSE                                                         ECS076
00785          MOVE SPACES             TO D4-SPECIAL.                   ECS076
00786                                                                   ECS076
00787      MOVE AM-CITY-CODE           TO D4-CITY.                      ECS076
00788                                                                   ECS076
00789      ADD +1                      TO X1.                           ECS076
00790      MOVE DTL-4                  TO PRT-LINE (X1).                ECS076
00791      ADD +1                      TO WS-LINE.                      ECS076
00792                                                                   ECS076
00793  1600-DTL-5.                                                      ECS076
00794      IF AM-IG = '1'                                               ECS076
00795          MOVE 'INDIVIDUAL'       TO D5-POL-TYPE                   ECS076
00796      ELSE                                                         ECS076
00797          MOVE 'GROUP'            TO D5-POL-TYPE.                  ECS076
00798                                                                   ECS076
00799      IF  AM-CANADIAN-POST-CODE                                    ECS076
00800          MOVE AM-CAN-POSTAL-1    TO D5-CAN-POSTAL-CODE-1          ECS076
00801          MOVE AM-CAN-POSTAL-2    TO D5-CAN-POSTAL-CODE-2          ECS076
00802          MOVE SPACES             TO D5-DASH-CAN                   ECS076
00803                                     D5-CAN-FILLER                 ECS076
00804      ELSE                                                         ECS076
00805          MOVE AM-ZIP             TO D5-ZIP                        ECS076
00806          IF AM-ZIP-PLUS4 = SPACES OR ZEROS                        ECS076
00807              MOVE SPACES         TO D5-ZIP-EXT  D5-DASH           ECS076
00808          ELSE                                                     ECS076
00809              MOVE AM-ZIP-PLUS4   TO D5-ZIP-EXT                    ECS076
00810              MOVE '-'            TO D5-DASH.                      ECS076
00811                                                                   ECS076
00812      IF AM-TEL-NBR NOT NUMERIC OR AM-TEL-NBR = ZEROS              ECS076
00813          MOVE SPACES             TO D5-TEL-LOC                    ECS076
00814      ELSE                                                         ECS076
00815          IF AM-TEL-AT-HOME                                        ECS076
00816              MOVE 'HOME'         TO D5-TEL-LOC                    ECS076
00817          ELSE                                                     ECS076
00818              MOVE 'BUSINESS'     TO D5-TEL-LOC.                   ECS076
00819                                                                   ECS076
00820      IF AM-HI-CERT-DATE = ZEROS                                   ECS076
00821          MOVE 'NO CERTS'         TO D5-HCRT-DTE                   ECS076
00822      ELSE                                                         ECS076
00823          MOVE AM-HI-MO           TO DS-MO                         ECS076
00824          MOVE AM-HI-DA           TO DS-DA                         ECS076
00825          MOVE AM-HI-YR           TO DS-YR                         ECS076
00826          MOVE DATE-SETS          TO D5-HCRT-DTE.                  ECS076
00827                                                                   ECS076
00828      MOVE 'UNKNOWN'              TO D5-ST-NAME.                   ECS076
00829      MOVE CLAS-STARTS            TO CLAS-INDEXS.                  ECS076
00830                                                                   ECS076
00831  1610-STATE-LOOKUP.                                               ECS076
00832      IF CLAS-INDEXS GREATER CLAS-MAXS OR                          ECS076
00833         CLAS-STARTS = ZEROS                                       ECS076
00834          GO TO 1620-E-ST-LOOKUP.                                  ECS076
00835                                                                   ECS076
00836      IF AM-STATE NOT = STATE-SUB (CLAS-INDEXS)                    ECS076
00837          ADD +1                  TO CLAS-INDEXS                   ECS076
00838          GO TO 1610-STATE-LOOKUP.                                 ECS076
00839                                                                   ECS076
00840      MOVE STATE-PIC (CLAS-INDEXS) TO D5-ST-NAME.                  ECS076
00841                                                                   ECS076
00842  1620-E-ST-LOOKUP.                                                ECS076
00843      MOVE AM-COUNTY-PARISH       TO D5-COUNTY.                    ECS076
00844                                                                   ECS076
00845      ADD +1                      TO X1.                           ECS076
00846      MOVE DTL-5                  TO PRT-LINE (X1).                ECS076
00847      ADD +1                      TO WS-LINE.                      ECS076
00848                                                                   ECS076
00849  1700-DTL-6.                                                      ECS076
102004
102004     EVALUATE AM-STATUS
102004        WHEN '0'
102004           MOVE 'ACTIVE'         TO D6-STATUS
102004        WHEN '1'
102004           MOVE 'INACTIVE'       TO D6-STATUS
102004        WHEN '2'
102004           MOVE 'TRANSFER'       TO D6-STATUS
102004        WHEN '3'
102004           MOVE 'CANCEL'         TO D6-STATUS
031811        WHEN '4'
031811           MOVE 'FROZEN'         TO D6-STATUS
031811        WHEN '5'
031811           MOVE 'SUSPEND'        TO D6-STATUS
021916        WHEN '6'
021916           MOVE 'DROPPED'        TO D6-STATUS
021916        WHEN '7'
021916           MOVE 'LAPSED'         TO D6-STATUS
021916        WHEN '8'
021916           MOVE 'RUN-OFF'        TO D6-STATUS
021916        WHEN '9'
021916           MOVE 'PENDING'        TO D6-STATUS
102004        WHEN OTHER
102004           MOVE 'UNKNOWN'        TO D6-STATUS
102004     END-EVALUATE
102004
00850 *    IF AM-STATUS = '0'                                           ECS076
00851 *      OR AM-STATUS = 'A'                                         ECS076
00852 *        MOVE 'ACTIVE'           TO D6-STATUS                     ECS076
00853 *    ELSE                                                         ECS076
00854 *        MOVE 'INACTIVE'         TO D6-STATUS.                    ECS076
00855                                                                   ECS076
00856      MOVE AM-ID-NO               TO D6-TAX-ID.                    ECS076
00857                                                                   ECS076
00858      MOVE SPACES                 TO D6-AR-CD  D6-TEL-NBR.         ECS076
00859                                                                   ECS076
00860      IF AM-TEL-NBR NOT NUMERIC OR AM-TEL-NBR = ZEROS              ECS076
00861          MOVE SPACES             TO D6-TEL-NBR, D6-AR-CD          ECS076
00862      ELSE                                                         ECS076
00863          MOVE AM-TEL-PRE         TO D6-PRE                        ECS076
00864          MOVE AM-TEL-NBR         TO D6-NBR                        ECS076
00865          MOVE '-'                TO D6-DASH                       ECS076
00866          IF AM-AREA-CODE NOT NUMERIC OR AM-AREA-CODE = ZEROS      ECS076
00867              MOVE SPACES         TO D6-AR-CD                      ECS076
00868          ELSE                                                     ECS076
00869              MOVE '('            TO D6-LP                         ECS076
00870              MOVE ')'            TO D6-RP                         ECS076
00871              MOVE AM-AREA-CODE   TO D6-AC.                        ECS076
00872                                                                   ECS076
00873      IF AM-LO-CERT-DATE = ZEROS                                   ECS076
00874          MOVE 'NO CERTS'         TO D6-LCRT-DTE                   ECS076
00875      ELSE                                                         ECS076
00876          MOVE AM-LO-MO           TO DS-MO                         ECS076
00877          MOVE AM-LO-DA           TO DS-DA                         ECS076
00878          MOVE AM-LO-YR           TO DS-YR                         ECS076
00879          MOVE DATE-SETS          TO D6-LCRT-DTE.                  ECS076
00880                                                                   ECS076
00881      MOVE AM-FLD-1              TO D6-USER-1.                     ECS076
00882      MOVE AM-FLD-2              TO D6-USER-2.                     ECS076
00883      MOVE AM-FLD-3              TO D6-USER-3.                     ECS076
00884      MOVE AM-FLD-4              TO D6-USER-4.                     ECS076
00885      MOVE AM-FLD-5              TO D6-USER-5.                     ECS076
00886      MOVE AH-OVERRIDE-L2        TO D6-AH-OVRD1.                   ECS076
00887      MOVE AM-STD-AH-TYPE        TO D6-STD-AH-TYPE.                ECS076
00888                                                                   ECS076
00889      ADD +1                      TO X1.                           ECS076
00890      MOVE DTL-6                  TO PRT-LINE (X1).                ECS076
00891      ADD +1                      TO WS-LINE.                      ECS076
00892                                                                   ECS076
00893  1800-DTL-7.                                                      ECS076
00894      MOVE AM-LAST-MAINT-DT       TO DC-BIN-DATE-1.                ECS076
00895      MOVE SPACE                  TO DC-OPTION-CODE.               ECS076
00896      PERFORM 8500-DATE-CONVERSION.                                ECS076
00897      MOVE DC-GREG-DATE-1-EDIT    TO D7-MNT-DTE.                   ECS076
00898      MOVE AM-LAST-MAINT-HHMMSS   TO WRK-TIME.                     ECS076
00899      MOVE WRK-TIME-HH            TO TS-HH.                        ECS076
00900      MOVE WRK-TIME-MM            TO TS-MM.                        ECS076
00901      MOVE WRK-TIME-SS            TO TS-SS.                        ECS076
00902      MOVE TIME-SETS              TO D7-MNT-TIME.                  ECS076
00903      MOVE AM-LAST-MAINT-USER     TO D7-MNT-BY.                    ECS076
00904                                                                   ECS076
00905      MOVE CLAS-STARTB            TO CLAS-INDEXB.                  ECS076
00906      MOVE AM-GPCD                TO D7-BUS-TYPE.                  ECS076
00907                                                                   ECS076
00908  1820-BCT-LOOP.                                                   ECS076
00909      IF CLAS-INDEXB GREATER THAN CLAS-MAXB OR                     ECS076
00910         CLAS-STARTB = ZEROS                                       ECS076
00911          MOVE 'UNKNOWN'          TO D7-BUS-DESC                   ECS076
00912          GO TO 1830-E-BCT-LOOP.                                   ECS076
00913                                                                   ECS076
00914      IF AM-GPCD = CLAS-BUSC-CODE (CLAS-INDEXB)                    ECS076
00915          MOVE CLAS-BUSC-DESC (CLAS-INDEXB) TO D7-BUS-DESC         ECS076
00916      ELSE                                                         ECS076
00917          ADD +1                  TO CLAS-INDEXB                   ECS076
00918          GO TO 1820-BCT-LOOP.                                     ECS076
00919                                                                   ECS076
00920  1830-E-BCT-LOOP.                                                 ECS076
00921      MOVE AM-REMIT-TO            TO D7-REM-LVL.                   ECS076
00922                                                                   ECS076
00923      ADD +1                      TO X1.                           ECS076
00924      MOVE DTL-7                  TO PRT-LINE (X1).                ECS076
00925      ADD +1                      TO WS-LINE.                      ECS076
050303
050303     IF (AM-REPORT-CODE-1 = SPACES)
              AND (AM-REPORT-CODE-2 = SPACES)
              AND (AM-REPORT-CODE-3 = SPACES)
              CONTINUE
           ELSE
              MOVE AM-REPORT-CODE-1    TO DET-RPTCD1
              MOVE AM-REPORT-CODE-2    TO DET-RPTCD2
              MOVE AM-REPORT-CODE-3    TO DET-RPTCD3
122806        MOVE AM-ORIG-DEALER-NO (3:8)
                                       TO DET-ORIG-DLR-NO
              ADD +1                   TO X1
              MOVE REPORT-CODES        TO PRT-LINE (X1)
050303        ADD +1                   TO WS-LINE
050303     END-IF
050303     .
00927  1900-DTL-8A.                                                     ECS076
00928      MOVE TOLERANCE-MSG-1        TO D8-TOLERANC.                  ECS076
00929      MOVE REFUND-EARN-METH-MSG   TO D8-REFUND.                    ECS076
00930                                                                   ECS076
00931      IF AM-REI-TABLE NOT = SPACES                                 ECS076
00932          MOVE AM-REI-TABLE       TO R1-TBL                        ECS076
00933          MOVE LIFE-OVERRIDE-L2   TO R1-LF-OVRD                    ECS076
00934          MOVE AM-REI-PE-LF       TO R1-LF-PE                      ECS076
00935          MOVE AH-OVERRIDE-L2     TO R1-AH-OVRD                    ECS076
00936          MOVE AM-REI-PE-AH       TO R1-AH-PE                      ECS076
00937          MOVE REIN-MSG-1         TO D8-REIN                       ECS076
00938      ELSE                                                         ECS076
00939          MOVE SPACES             TO D8-REIN.                      ECS076
00940                                                                   ECS076
00941      IF AM-RET-Y-N = 'Y' OR 'S' OR 'Q' OR 'I' OR                  ECS076
00942                       'G' OR 'L' OR 'D'                           ECS076
00943          MOVE 'USED ----'            TO R1M1-D1                   ECS076
00944          MOVE RETRO-EARN-METH-MSG    TO D8-RETRO2                 ECS076
00945      ELSE                                                         ECS076
00946          MOVE 'NOT USED '            TO R1M1-D1                   ECS076
00947          MOVE SPACES                 TO D8-RETRO2.                ECS076
00948                                                                   ECS076
00949      MOVE RETRO1-MSG-1           TO D8-RETRO1.                    ECS076
00950                                                                   ECS076
00951      ADD +1                      TO X1.                           ECS076
00952      MOVE DTL-8                  TO PRT-LINE (X1).                ECS076
00953      ADD +2                      TO WS-LINE.                      ECS076
00954      MOVE SPACE                  TO DTL-8.                        ECS076
00955                                                                   ECS076
00956  2000-DTL-8B.                                                     ECS076
00957      MOVE 'PREM'                 TO TOLERANCE-TYPE.               ECS076
00958                                                                   ECS076
00959      IF AM-TOL-PREM NOT NUMERIC OR AM-TOL-PREM = ZEROS            ECS076
00960          MOVE 'STND '            TO TOL-AMT-R                     ECS076
00961      ELSE                                                         ECS076
00962          MOVE AM-TOL-PREM        TO TOLERANCE-AMT.                ECS076
00963                                                                   ECS076
00964      MOVE TOLERANCE-MSG-2        TO D8-TOLERANC.                  ECS076
00965                                                                   ECS076
00966      MOVE 'RED'                  TO EMM-D1.                       ECS076
00967      MOVE SPACES                 TO EMM-D2.                       ECS076
00968      MOVE AM-EARN-METHOD-R       TO AM-EARN-METHOD.               ECS076
00969      PERFORM 4000-FIND-EARN-METH THRU 4999-FEM-EXIT.              ECS076
00970      MOVE EARN-METH-MSG          TO D8-REFUND.                    ECS076
00971                                                                   ECS076
00972      IF AM-REI-TABLE NOT = SPACES                                 ECS076
00973          MOVE AM-REI-PRT-ST      TO R2-ST                         ECS076
00974          MOVE AH-OVERRIDE-L2     TO R2-AH-OVRD-1                  ECS076
00975          MOVE AM-REI-78-PCT      TO R2-AH-78                      ECS076
00976          MOVE AH-OVERRIDE-L2     TO R2-AH-OVRD-2                  ECS076
00977          MOVE AM-REI-PR-PCT      TO R2-AH-PR                      ECS076
00978          MOVE REIN-MSG-2         TO D8-REIN                       ECS076
00979      ELSE                                                         ECS076
00980          MOVE SPACES             TO D8-REIN.                      ECS076
00981                                                                   ECS076
00982      IF AM-RET-Y-N = 'Y' OR 'S' OR 'Q' OR 'I' OR                  ECS076
00983                       'G' OR 'L' OR 'D'                           ECS076
00984          IF AM-RET-P-E = 'P'                                      ECS076
00985              MOVE 'PAID COMMISSION'      TO R1M2-D1               ECS076
00986              MOVE RETRO1-MSG-2           TO D8-RETRO1             ECS076
00987          ELSE                                                     ECS076
00988              MOVE 'EARNED COMMISSION'    TO R1M2-D1               ECS076
00989              MOVE RETRO1-MSG-2           TO D8-RETRO1             ECS076
00990      ELSE                                                         ECS076
00991          MOVE SPACES                     TO D8-RETRO1.            ECS076
00992                                                                   ECS076
00993      IF AM-RET-Y-N = 'Y' OR 'S' OR 'Q' OR 'I' OR                  ECS076
00994                       'G' OR 'L' OR 'D'                           ECS076
00995          MOVE 'RED'                  TO EMM-D1                    ECS076
00996          MOVE SPACES                 TO EMM-D2                    ECS076
00997          MOVE AM-RET-EARN-R          TO AM-EARN-METHOD            ECS076
00998          PERFORM 4000-FIND-EARN-METH THRU 4999-FEM-EXIT           ECS076
00999          MOVE EARN-METH-MSG          TO D8-RETRO2                 ECS076
01000      ELSE                                                         ECS076
01001          MOVE SPACES                 TO D8-RETRO2.                ECS076
01002                                                                   ECS076
01003      ADD +1                      TO X1.                           ECS076
01004      MOVE DTL-8                  TO PRT-LINE (X1).                ECS076
01005      ADD +1                      TO WS-LINE.                      ECS076
01006      MOVE SPACES                 TO DTL-8.                        ECS076
01007                                                                   ECS076
01008  2100-DTL-8C.                                                     ECS076
01009      MOVE 'REFUND'               TO TOLERANCE-TYPE.               ECS076
01010                                                                   ECS076
01011      IF AM-TOL-REF NOT NUMERIC OR AM-TOL-REF = ZEROS              ECS076
01012          MOVE 'STND '            TO TOL-AMT-R                     ECS076
01013      ELSE                                                         ECS076
01014          MOVE AM-TOL-REF         TO TOLERANCE-AMT.                ECS076
01015                                                                   ECS076
01016      MOVE TOLERANCE-MSG-2        TO D8-TOLERANC.                  ECS076
01017                                                                   ECS076
01018      MOVE 'LEV'                  TO EMM-D1.                       ECS076
01019      MOVE SPACES                 TO EMM-D2.                       ECS076
01020      MOVE AM-EARN-METHOD-L       TO AM-EARN-METHOD.               ECS076
01021      PERFORM 4000-FIND-EARN-METH THRU 4999-FEM-EXIT.              ECS076
01022      MOVE EARN-METH-MSG          TO D8-REFUND.                    ECS076
01023                                                                   ECS076
01024      IF AM-REI-TABLE NOT = SPACES                                 ECS076
01025          MOVE AM-REI-PRT-OW      TO R3-OW                         ECS076
01026          MOVE AM-REI-MORT        TO R3-MORT                       ECS076
01027          MOVE REIN-MSG-3         TO D8-REIN                       ECS076
01028      ELSE                                                         ECS076
01029          MOVE SPACES             TO D8-REIN.                      ECS076
01030                                                                   ECS076
01031      IF (DTE-CLIENT = 'VSL' OR 'MON')  OR                         ECS076
01032         (AM-RET-Y-N = 'Y' OR 'S' OR 'Q' OR 'I' OR                 ECS076
01033                       'G' OR 'L' OR 'D')                          ECS076
01034          MOVE LIFE-OVERRIDE-L2           TO R1M3-OVRD1            ECS076
01035          MOVE AM-LF-RET                  TO R1M3-LF-RT            ECS076
01036          MOVE AH-OVERRIDE-L2             TO R1M3-OVRD2            ECS076
01037          MOVE AM-AH-RET                  TO R1M3-AH-RT            ECS076
01038          MOVE RETRO1-MSG-3               TO D8-RETRO1             ECS076
01039      ELSE                                                         ECS076
01040          MOVE SPACES                     TO D8-RETRO1.            ECS076
01041                                                                   ECS076
01042      IF AM-RET-Y-N = 'Y' OR 'S' OR 'Q' OR 'I' OR                  ECS076
01043                       'G' OR 'L' OR 'D'                           ECS076
01044          MOVE 'LEV'                  TO EMM-D1                    ECS076
01045          MOVE SPACES                 TO EMM-D2                    ECS076
01046          MOVE AM-RET-EARN-L          TO AM-EARN-METHOD            ECS076
01047          PERFORM 4000-FIND-EARN-METH THRU 4999-FEM-EXIT           ECS076
01048          MOVE EARN-METH-MSG          TO D8-RETRO2                 ECS076
01049      ELSE                                                         ECS076
01050          MOVE SPACES                 TO D8-RETRO2.                ECS076
01051                                                                   ECS076
01052      ADD +1                      TO X1.                           ECS076
01053      MOVE DTL-8                  TO PRT-LINE (X1).                ECS076
01054      ADD +1                      TO WS-LINE.                      ECS076
01055      MOVE SPACES                 TO DTL-8.                        ECS076
01056                                                                   ECS076
01057  2200-DTL-8D.                                                     ECS076
01058      MOVE LIFE-OVERRIDE-L2       TO TOL-LF-OVRD.                  ECS076
01059      MOVE ' CLM '                TO TOL-CLM.                      ECS076
01060                                                                   ECS076
01061      IF AM-TOL-CLM NOT NUMERIC OR AM-TOL-CLM = ZEROS              ECS076
01062          MOVE 'STND '            TO TOL-AMT-R                     ECS076
01063      ELSE                                                         ECS076
01064          MOVE AM-TOL-CLM         TO TOLERANCE-AMT.                ECS076
01065                                                                   ECS076
01066      MOVE TOLERANCE-MSG-2        TO D8-TOLERANC.                  ECS076
01067                                                                   ECS076
01068      MOVE AH-OVERRIDE-L2         TO EMM-D1.                       ECS076
01069      MOVE SPACES                 TO EMM-D2.                       ECS076
01070      MOVE AM-EARN-METHOD-A       TO AM-EARN-METHOD.               ECS076
01071      PERFORM 4000-FIND-EARN-METH THRU 4999-FEM-EXIT.              ECS076
01072      MOVE EARN-METH-MSG          TO D8-REFUND.                    ECS076
01073                                                                   ECS076
01074      IF AM-REI-TABLE NOT = SPACES                                 ECS076
01075          MOVE AM-REI-GROUP-A     TO R4-GRP-A                      ECS076
01076          MOVE LIFE-OVERRIDE-L2   TO R4-LF-OVRD                    ECS076
01077          MOVE AM-REI-FEE-LF      TO R4-LF-FEE                     ECS076
01078          MOVE AM-REI-LF-TAX      TO R4-LF-TAX                     ECS076
01079          MOVE REIN-MSG-4         TO D8-REIN                       ECS076
01080      ELSE                                                         ECS076
01081          MOVE SPACES             TO D8-REIN.                      ECS076
01082                                                                   ECS076
01083      IF (DTE-CLIENT = 'VSL' OR 'MON')  OR                         ECS076
01084         (AM-RET-Y-N = 'Y' OR 'S' OR 'Q' OR 'I' OR                 ECS076
01085                       'G' OR 'L' OR 'D')                          ECS076
01086          MOVE LIFE-OVERRIDE-L2           TO R1M4-OVRD1            ECS076
01087          MOVE AM-RET-MIN-LOSS-L          TO R1M4-LF-ML            ECS076
01088          MOVE AH-OVERRIDE-L2             TO R1M4-OVRD2            ECS076
01089          MOVE AM-RET-MIN-LOSS-A          TO R1M4-AH-ML            ECS076
01090          MOVE RETRO1-MSG-4               TO D8-RETRO1             ECS076
01091      ELSE                                                         ECS076
01092          MOVE SPACES                     TO D8-RETRO1.            ECS076
01093                                                                   ECS076
01094      IF AM-RET-Y-N = 'Y' OR 'S' OR 'Q' OR 'I' OR                  ECS076
01095                       'G' OR 'L' OR 'D'                           ECS076
01096          MOVE AH-OVERRIDE-L2         TO EMM-D1                    ECS076
01097          MOVE SPACES                 TO EMM-D2                    ECS076
01098          MOVE AM-RET-EARN-A          TO AM-EARN-METHOD            ECS076
01099          PERFORM 4000-FIND-EARN-METH THRU 4999-FEM-EXIT           ECS076
01100          MOVE EARN-METH-MSG          TO D8-RETRO2                 ECS076
01101      ELSE                                                         ECS076
01102          MOVE SPACES                 TO D8-RETRO2.                ECS076
01103                                                                   ECS076
01104      ADD +1                      TO X1.                           ECS076
01105      MOVE DTL-8                  TO PRT-LINE (X1).                ECS076
01106      ADD +1                      TO WS-LINE.                      ECS076
01107      MOVE SPACES                 TO DTL-8.                        ECS076
01108                                                                   ECS076
01109  2300-DTL-8E.                                                     ECS076
01110      IF AM-REI-TABLE NOT = SPACES                                 ECS076
01111          MOVE AM-REI-GROUP-B     TO R5-GRP-B                      ECS076
01112          MOVE AH-OVERRIDE-L2     TO R5-AH-OVRD                    ECS076
01113          MOVE AM-REI-FEE-AH      TO R5-AH-FEE                     ECS076
01114          MOVE AM-REI-AH-TAX      TO R5-AH-TAX                     ECS076
01115          MOVE REIN-MSG-5         TO D8-REIN                       ECS076
01116      ELSE                                                         ECS076
01117          MOVE SPACES             TO D8-REIN.                      ECS076
01118                                                                   ECS076
01119      IF (DTE-CLIENT = 'VSL' OR 'MON')  OR                         ECS076
01120         (AM-RET-Y-N = 'Y' OR 'S' OR 'Q' OR 'I' OR                 ECS076
01121                       'G' OR 'L' OR 'D')                          ECS076
01122          MOVE AM-RETRO-POOL              TO R1M5-GP-CD            ECS076
01123          MOVE AM-RET-ST-TAX-USE          TO R1M5-TX-US            ECS076
01124          MOVE RETRO1-MSG-5               TO D8-RETRO1             ECS076
01125      ELSE                                                         ECS076
01126          MOVE SPACES                     TO D8-RETRO1.            ECS076
01127                                                                   ECS076
01128      ADD +1                      TO X1.                           ECS076
01129      MOVE DTL-8                  TO PRT-LINE (X1).                ECS076
01130      ADD +1                      TO WS-LINE.                      ECS076
01131      MOVE '0'                    TO DTL-8.                        ECS076
01132                                                                   ECS076
01133  2500-DTL-13.                                                     ECS076
01134      IF AM-RET-Y-N = 'Y' OR 'S' OR 'Q' OR 'I' OR                  ECS076
01135                       'G' OR 'L' OR 'D'                           ECS076
01136          MOVE RETRO-BEG-EARN-METH-MSG                             ECS076
01137                                      TO D13-RETRO1                ECS076
01138      ELSE                                                         ECS076
01139          MOVE SPACES                 TO D13-RETRO1.               ECS076
01140                                                                   ECS076
01141      ADD +1                      TO X1.                           ECS076
01142      MOVE DTL-13                 TO PRT-LINE (X1).                ECS076
01143      ADD +2                      TO WS-LINE.                      ECS076
01144                                                                   ECS076
01145  2600-DTL-14.                                                     ECS076
01146      MOVE AH-OVERRIDE-L6         TO D14-AH-OVRD1.                 ECS076
01147                                                                   ECS076
01148      IF AM-RET-Y-N = 'Y' OR 'S' OR 'Q' OR 'I' OR                  ECS076
01149                       'G' OR 'L' OR 'D'                           ECS076
01150          MOVE 'RED '                 TO EMM-D1                    ECS076
01151          MOVE SPACES                 TO EMM-D2                    ECS076
01152          MOVE AM-RET-BEG-EARN-R      TO AM-EARN-METHOD            ECS076
01153          PERFORM 4000-FIND-EARN-METH THRU 4999-FEM-EXIT           ECS076
01154          MOVE EARN-METH-MSG          TO D14-RETRO1                ECS076
01155      ELSE                                                         ECS076
01156          MOVE SPACES                 TO D14-RETRO1.               ECS076
01157                                                                   ECS076
01158      ADD +1                      TO X1.                           ECS076
01159      MOVE DTL-14                 TO PRT-LINE (X1).                ECS076
01160      ADD +1                      TO WS-LINE.                      ECS076
01161                                                                   ECS076
01162  2700-DTL-15.                                                     ECS076
01163      MOVE +1                     TO X2 X3 X4.                     ECS076
01164                                                                   ECS076
01165  2710-15-LOOP.                                                    ECS076
01166      MOVE SPACES                 TO DTL-15.                       ECS076
01167                                                                   ECS076
01168  2720-COMMISH.                                                    ECS076
01169      IF X2 GREATER +10                                            ECS076
01170          GO TO 2740-COMMENT.                                      ECS076
01171                                                                   ECS076
01172      IF AM-AGT (X2) NOT = ZEROS OR                                ECS076
01173         AM-COM-TYP (X2) NOT = SPACES                              ECS076
01174          GO TO 2730-COMMISH-OK.                                   ECS076
01175                                                                   ECS076
01176      ADD +1                      TO X2.                           ECS076
01177      GO TO 2720-COMMISH.                                          ECS076
01178                                                                   ECS076
01179  2730-COMMISH-OK.                                                 ECS076
01180      MOVE X2                     TO D15-LEVEL.                    ECS076
01181      MOVE AM-AGT (X2)            TO D15-AGENT.                    ECS076
01182                                                                   ECS076
01183      IF AM-L-COM (X2) NUMERIC                                     ECS076
01184          COMPUTE WRK-COMM = AM-L-COM (X2) * +100                  ECS076
01185          MOVE WRK-COMM           TO D15-SINGL                     ECS076
01186      ELSE                                                         ECS076
01187          MOVE AM-L-COMA (X2)     TO D15-SINGL-R.                  ECS076
01188                                                                   ECS076
01189      IF AM-J-COM (X2) NUMERIC                                     ECS076
01190          COMPUTE WRK-COMM = AM-J-COM (X2) * +100                  ECS076
01191          MOVE WRK-COMM           TO D15-JOINT                     ECS076
01192      ELSE                                                         ECS076
01193          MOVE AM-J-COMA (X2)     TO D15-JOINT-R.                  ECS076
01194                                                                   ECS076
01195      IF AM-A-COM (X2)  NUMERIC                                    ECS076
01196          COMPUTE WRK-COMM = AM-A-COM (X2) * +100                  ECS076
01197          MOVE WRK-COMM           TO D15-AH                        ECS076
01198      ELSE                                                         ECS076
01199          MOVE AM-A-COMA (X2)     TO D15-AH-R.                     ECS076
01200                                                                   ECS076
01201      MOVE AM-COM-TYP (X2)        TO D15-TYPE.                     ECS076
01202      MOVE AM-RETRO-LV-INDIC (X2) TO D15-RET-LEV.                  ECS076
01203      MOVE AM-RECALC-LV-INDIC (X2)                                 ECS076
01204                                  TO D15-REC-LEV.                  ECS076
01205      MOVE AM-GL-CODES (X2)       TO D15-GL-CODE
           MOVE AM-COMM-CHARGEBACK (X2)
                                       TO D15-NCB
           .
01207  2740-COMMENT.                                                    ECS076
01208      IF X3 GREATER +5                                             ECS076
01209          GO TO 2750-RET-BEG-EARN.                                 ECS076
01210                                                                   ECS076
01211      IF AM-COMMENT-LINE (X3) = SPACES                             ECS076
01212          ADD +1                  TO X3                            ECS076
01213          GO TO 2740-COMMENT.                                      ECS076
01214                                                                   ECS076
01215      MOVE AM-COMMENT-LINE (X3)   TO D15-COMMENT.                  ECS076
01216      MOVE X3                     TO D15-LINE.                     ECS076
01217                                                                   ECS076
01218  2750-RET-BEG-EARN.                                               ECS076
01219      MOVE SPACES                 TO D15-RETRO1.                   ECS076
01220                                                                   ECS076
01221      IF X4 = +1                                                   ECS076
01222          IF AM-RET-Y-N = 'Y' OR 'S' OR 'Q' OR 'I' OR              ECS076
01223                          'G' OR 'L' OR 'D'                        ECS076
01224              MOVE 'LEV '             TO EMM-D1                    ECS076
01225              MOVE SPACES             TO EMM-D2                    ECS076
01226              MOVE AM-RET-BEG-EARN-L  TO AM-EARN-METHOD            ECS076
01227              PERFORM 4000-FIND-EARN-METH THRU 4999-FEM-EXIT       ECS076
01228              MOVE EARN-METH-MSG      TO D15-RETRO1                ECS076
01229          ELSE                                                     ECS076
01230              MOVE SPACES             TO D15-RETRO1.               ECS076
01231                                                                   ECS076
01232      IF X4 = +2                                                   ECS076
01233          IF AM-RET-Y-N = 'Y' OR 'S' OR 'Q' OR 'I' OR              ECS076
01234                          'G' OR 'L' OR 'D'                        ECS076
01235              MOVE AH-OVERRIDE-L2     TO EMM-D1                    ECS076
01236              MOVE SPACES             TO EMM-D2                    ECS076
01237              MOVE AM-RET-BEG-EARN-A  TO AM-EARN-METHOD            ECS076
01238              PERFORM 4000-FIND-EARN-METH THRU 4999-FEM-EXIT       ECS076
01239              MOVE EARN-METH-MSG      TO D15-RETRO1                ECS076
01240          ELSE                                                     ECS076
01241              MOVE SPACES             TO D15-RETRO1.               ECS076
01242                                                                   ECS076
01243      ADD +1 TO X4.                                                ECS076
01244                                                                   ECS076
01245  2760-CHECK-PRT.                                                  ECS076
01246      IF DTL-15 NOT = SPACES                                       ECS076
01247          ADD +1                  TO X1                            ECS076
01248          MOVE DTL-15             TO PRT-LINE (X1)                 ECS076
01249          ADD +1                  TO WS-LINE                       ECS076
01250          ADD +1                  TO X2 X3                         ECS076
01251          GO TO 2710-15-LOOP.                                      ECS076
01252                                                                   ECS076
01253  2800-DTL-16-17-18.                                               ECS076
01254                                                                   ECS076
01255      ADD +1                      TO X1.                           ECS076
01256      MOVE DTL-16                 TO PRT-LINE (X1).                ECS076
01257      ADD +2                      TO WS-LINE.                      ECS076
01258                                                                   ECS076
01259      ADD +1                      TO X1.                           ECS076
01260      MOVE DTL-17                 TO PRT-LINE (X1).                ECS076
01261      ADD +1                      TO WS-LINE.                      ECS076
01262                                                                   ECS076
01263      MOVE +0                     TO X2.                           ECS076
01264                                                                   ECS076
01265  2810-NEXT-BENEFIT.                                               ECS076
01266                                                                   ECS076
01267      ADD +1                      TO X2.                           ECS076
01268      IF X2 GREATER THAN +20                                       ECS076
01269          IF DTE-CLIENT = 'DMD'                                    ECS076
01270              MOVE +0             TO X2                            ECS076
01271              GO TO 2810-NEXT-DMD-BENEFIT                          ECS076
01272          ELSE                                                     ECS076
01273              GO TO 2900-DTL-19-20-21.                             ECS076
01274                                                                   ECS076
01275      IF AM-BENEFIT-TYPE (X2) NOT = LIFE-OVERRIDE-L1               ECS076
01276          GO TO 2810-NEXT-BENEFIT.                                 ECS076
01277                                                                   ECS076
01278      MOVE AM-ALLOWABLE-BENEFITS (X2)                              ECS076
01279                                TO WS-SAVE-ALLOWABLE-BENEFITS.     ECS076
01280                                                                   ECS076
01281      PERFORM 5000-READ-PLAN-MASTER THRU 5999-READ-PLAN-EXIT.      ECS076
01282                                                                   ECS076
01283      IF NOT PLAN-MASTER-FOUND                                     ECS076
01284          GO TO 2810-NEXT-BENEFIT.                                 ECS076
01285                                                                   ECS076
01286      MOVE SPACES                 TO DTL-18.                       ECS076
01287      MOVE ZEROS                  TO D22-PL-DEV-T                  ECS076
01288                                     D22-PL-DEV-R.                 ECS076
01289      MOVE +1                     TO X3  X4.                       ECS076
01290                                                                   ECS076
01291      MOVE AM-BENEFIT-CODE (X2)   TO D18-BENE.                     ECS076
01292      MOVE PL-ATT-AGE             TO D18-MAX-AGE.                  ECS076
01293                                                                   ECS076
01294      IF PL-DEV-CODE EQUAL SPACES OR ZEROS                         ECS076
01295         NEXT SENTENCE                                             ECS076
01296      ELSE                                                         ECS076
01297         MOVE PL-DEV-CODE         TO D22-PL-DEV-T.                 ECS076
01298                                                                   ECS076
01299      IF PL-DEV-PCT NUMERIC                                        ECS076
01300         IF PL-DEV-PCT NOT EQUAL ZEROS                             ECS076
01301            MOVE PL-DEV-PCT       TO D22-PL-DEV-P.                 ECS076
01302                                                                   ECS076
01303  2820-NEXT-LIMIT.                                                 ECS076
01304                                                                   ECS076
01305      IF PL-LM-AGE (X3) = ZEROS  AND                               ECS076
01306         PL-LM-DUR (X3) = ZEROS  AND                               ECS076
01307         PL-LM-AMT (X3) = ZEROS                                    ECS076
01308          NEXT SENTENCE                                            ECS076
01309      ELSE                                                         ECS076
01310          MOVE PL-LM-AGE (X3)     TO D18-AGE (X4)                  ECS076
01311          MOVE PL-LM-DUR (X3)     TO D18-TRM (X4)                  ECS076
01312          MOVE PL-LM-AMT (X3)     TO D18-BEN (X4).                 ECS076
01313                                                                   ECS076
01314      IF X3 = +4                                                   ECS076
01315        MOVE +0                   TO X4                            ECS076
01316        IF (D18-LIMITS NOT = SPACES) OR                            ECS076
01317           (D22-PL-DEV-T NOT EQUAL ZEROS) OR                       ECS076
01318           (D22-PL-DEV-R NOT EQUAL ZEROS)                          ECS076
01319          ADD +1                  TO X1                            ECS076
01320          MOVE DTL-18             TO PRT-LINE (X1)                 ECS076
01321          ADD +1                  TO WS-LINE                       ECS076
01322          MOVE SPACES             TO DTL-18.                       ECS076
01323                                                                   ECS076
01324      IF X3 = +8                                                   ECS076
01325        IF D18-LIMITS NOT = SPACES                                 ECS076
01326          ADD +1                  TO X1                            ECS076
01327          MOVE DTL-18             TO PRT-LINE (X1)                 ECS076
01328          ADD +1                  TO WS-LINE.                      ECS076
01329                                                                   ECS076
01330      IF X3 = +8                                                   ECS076
01331        IF (D22-PL-DEV-T NOT EQUAL ZEROS) OR                       ECS076
01332           (D22-PL-DEV-R NOT EQUAL ZEROS)                          ECS076
01333          ADD +1                  TO X1                            ECS076
01334          MOVE DTL-22             TO PRT-LINE (X1)                 ECS076
01335          ADD +1                  TO WS-LINE                       ECS076
01336          GO TO 2810-NEXT-BENEFIT                                  ECS076
01337        ELSE                                                       ECS076
01338          GO TO 2810-NEXT-BENEFIT.                                 ECS076
01339                                                                   ECS076
01340      ADD +1 TO X3.                                                ECS076
01341      ADD +1 TO X4.                                                ECS076
01342                                                                   ECS076
01343      GO TO 2820-NEXT-LIMIT.                                       ECS076
01344                                                                   ECS076
01345  2810-NEXT-DMD-BENEFIT.                                           ECS076
01346                                                                   ECS076
01347      ADD +1                      TO X2.                           ECS076
01348      IF X2 GREATER THAN +30                                       ECS076
01349          GO TO 2900-DTL-19-20-21.                                 ECS076
01350                                                                   ECS076
01351      IF AM-BENEFIT-DMD-TYPE (X2) NOT = LIFE-OVERRIDE-L1           ECS076
01352          GO TO 2810-NEXT-DMD-BENEFIT.                             ECS076
01353                                                                   ECS076
01354      MOVE AM-ALLOWABLE-DMD-BENEFITS (X2)                          ECS076
01355                                TO WS-SAVE-ALLOWABLE-BENEFITS.     ECS076
01356                                                                   ECS076
01357      PERFORM 5000-READ-PLAN-MASTER THRU 5999-READ-PLAN-EXIT.      ECS076
01358                                                                   ECS076
01359      IF NOT PLAN-MASTER-FOUND                                     ECS076
01360          GO TO 2810-NEXT-DMD-BENEFIT.                             ECS076
01361                                                                   ECS076
01362      MOVE SPACES                 TO DTL-18.                       ECS076
01363      MOVE ZEROS                  TO D22-PL-DEV-T                  ECS076
01364                                     D22-PL-DEV-R.                 ECS076
01365      MOVE +1                     TO X3  X4.                       ECS076
01366                                                                   ECS076
01367      MOVE AM-BENEFIT-DMD-CODE (X2)   TO D18-BENE.                 ECS076
01368      MOVE PL-ATT-AGE                 TO D18-MAX-AGE.              ECS076
01369                                                                   ECS076
01370      IF PL-DEV-CODE EQUAL SPACES OR ZEROS                         ECS076
01371         NEXT SENTENCE                                             ECS076
01372      ELSE                                                         ECS076
01373         MOVE PL-DEV-CODE         TO D22-PL-DEV-T.                 ECS076
01374                                                                   ECS076
01375      IF PL-DEV-PCT NUMERIC                                        ECS076
01376         IF PL-DEV-PCT NOT EQUAL ZEROS                             ECS076
01377            MOVE PL-DEV-PCT       TO D22-PL-DEV-P.                 ECS076
01378                                                                   ECS076
01379  2820-NEXT-DMD-LIMIT.                                             ECS076
01380                                                                   ECS076
01381      IF PL-LM-AGE (X3) = ZEROS  AND                               ECS076
01382         PL-LM-DUR (X3) = ZEROS  AND                               ECS076
01383         PL-LM-AMT (X3) = ZEROS                                    ECS076
01384          NEXT SENTENCE                                            ECS076
01385      ELSE                                                         ECS076
01386          MOVE PL-LM-AGE (X3)     TO D18-AGE (X4)                  ECS076
01387          MOVE PL-LM-DUR (X3)     TO D18-TRM (X4)                  ECS076
01388          MOVE PL-LM-AMT (X3)     TO D18-BEN (X4).                 ECS076
01389                                                                   ECS076
01390      IF X3 = +4                                                   ECS076
01391        MOVE +0                   TO X4                            ECS076
01392        IF (D18-LIMITS NOT = SPACES) OR                            ECS076
01393           (D22-PL-DEV-T NOT EQUAL ZEROS) OR                       ECS076
01394           (D22-PL-DEV-R NOT EQUAL ZEROS)                          ECS076
01395          ADD +1                  TO X1                            ECS076
01396          MOVE DTL-18             TO PRT-LINE (X1)                 ECS076
01397          ADD +1                  TO WS-LINE                       ECS076
01398          MOVE SPACES             TO DTL-18.                       ECS076
01399                                                                   ECS076
01400      IF X3 = +8                                                   ECS076
01401        IF D18-LIMITS NOT = SPACES                                 ECS076
01402          ADD +1                  TO X1                            ECS076
01403          MOVE DTL-18             TO PRT-LINE (X1)                 ECS076
01404          ADD +1                  TO WS-LINE.                      ECS076
01405                                                                   ECS076
01406      IF X3 = +8                                                   ECS076
01407        IF (D22-PL-DEV-T NOT EQUAL ZEROS) OR                       ECS076
01408           (D22-PL-DEV-R NOT EQUAL ZEROS)                          ECS076
01409          ADD +1                  TO X1                            ECS076
01410          MOVE DTL-22             TO PRT-LINE (X1)                 ECS076
01411          ADD +1                  TO WS-LINE                       ECS076
01412          GO TO 2810-NEXT-DMD-BENEFIT                              ECS076
01413        ELSE                                                       ECS076
01414          GO TO 2810-NEXT-DMD-BENEFIT.                             ECS076
01415                                                                   ECS076
01416      ADD +1                      TO X3.                           ECS076
01417      ADD +1                      TO X4.                           ECS076
01418                                                                   ECS076
01419      GO TO 2820-NEXT-DMD-LIMIT.                                   ECS076
01420                                                                   ECS076
01421  2900-DTL-19-20-21.                                               ECS076
01422                                                                   ECS076
01423      ADD +1                      TO X1.                           ECS076
01424      MOVE DTL-19                 TO PRT-LINE (X1).                ECS076
01425      ADD +2                      TO WS-LINE.                      ECS076
01426                                                                   ECS076
01427      ADD +1                      TO X1.                           ECS076
01428      MOVE DTL-20                 TO PRT-LINE (X1).                ECS076
01429      ADD +1                      TO WS-LINE.                      ECS076
01430                                                                   ECS076
01431      MOVE +0                     TO X2.                           ECS076
01432                                                                   ECS076
01433  2910-NEXT-BENEFIT.                                               ECS076
01434                                                                   ECS076
01435      ADD +1                      TO X2.                           ECS076
01436      IF X2 GREATER THAN +20                                       ECS076
01437          IF DTE-CLIENT EQUAL 'DMD'                                ECS076
01438            MOVE +0               TO X2                            ECS076
01439            GO TO 2910-NEXT-DMD-BENEFIT                            ECS076
01440          ELSE                                                     ECS076
01441              GO TO 3000-PRINT.                                    ECS076
01442                                                                   ECS076
01443      IF AM-BENEFIT-TYPE (X2) NOT = AH-OVERRIDE-L1                 ECS076
01444          GO TO 2910-NEXT-BENEFIT.                                 ECS076
01445                                                                   ECS076
01446      MOVE AM-ALLOWABLE-BENEFITS (X2)                              ECS076
01447                                TO WS-SAVE-ALLOWABLE-BENEFITS.     ECS076
01448                                                                   ECS076
01449      PERFORM 5000-READ-PLAN-MASTER THRU 5999-READ-PLAN-EXIT.      ECS076
01450                                                                   ECS076
01451      IF NOT PLAN-MASTER-FOUND                                     ECS076
01452          GO TO 2910-NEXT-BENEFIT.                                 ECS076
01453                                                                   ECS076
01454      MOVE SPACES                 TO DTL-21.                       ECS076
01455      MOVE ZEROS                  TO D22-PL-DEV-T                  ECS076
01456                                     D22-PL-DEV-R.                 ECS076
01457      MOVE +1                     TO X3  X4.                       ECS076
01458                                                                   ECS076
01459      MOVE AM-BENEFIT-CODE (X2)   TO D21-BENE.                     ECS076
01460      MOVE PL-ATT-AGE             TO D21-MAX-AGE.                  ECS076
01461                                                                   ECS076
01462      IF PL-DEV-CODE EQUAL SPACES OR ZEROS                         ECS076
01463         NEXT SENTENCE                                             ECS076
01464      ELSE                                                         ECS076
01465         MOVE PL-DEV-CODE         TO D22-PL-DEV-T.                 ECS076
01466                                                                   ECS076
01467      IF PL-DEV-PCT NUMERIC                                        ECS076
01468         IF PL-DEV-PCT NOT EQUAL ZEROS                             ECS076
01469            MOVE PL-DEV-PCT       TO D22-PL-DEV-P.                 ECS076
01470                                                                   ECS076
01471  2920-NEXT-LIMIT.                                                 ECS076
01472                                                                   ECS076
01473      IF PL-LM-AGE (X3) = ZEROS  AND                               ECS076
01474         PL-LM-DUR (X3) = ZEROS  AND                               ECS076
01475         PL-LM-MOA (X3) = ZEROS  AND                               ECS076
01476         PL-LM-AMT (X3) = ZEROS                                    ECS076
01477          NEXT SENTENCE                                            ECS076
01478      ELSE                                                         ECS076
01479          MOVE PL-LM-AGE (X3)     TO D21-AGE (X4)                  ECS076
01480          MOVE PL-LM-DUR (X3)     TO D21-TRM (X4)                  ECS076
01481          MOVE PL-LM-MOA (X3)     TO D21-AMT (X4)                  ECS076
01482          MOVE PL-LM-AMT (X3)     TO D21-BEN (X4).                 ECS076
01483                                                                   ECS076
01484      IF X3 = +4                                                   ECS076
01485        MOVE +0                   TO X4                            ECS076
01486        IF (D21-LIMITS NOT = SPACES) OR                            ECS076
01487           (D22-PL-DEV-T NOT EQUAL ZEROS) OR                       ECS076
01488           (D22-PL-DEV-R NOT EQUAL ZEROS)                          ECS076
01489          ADD +1                  TO X1                            ECS076
01490          MOVE DTL-21             TO PRT-LINE (X1)                 ECS076
01491          ADD +1                  TO WS-LINE                       ECS076
01492          MOVE SPACES             TO DTL-21.                       ECS076
01493                                                                   ECS076
01494      IF X3 = +8                                                   ECS076
01495        IF D21-LIMITS NOT = SPACES                                 ECS076
01496          ADD +1                  TO X1                            ECS076
01497          MOVE DTL-21             TO PRT-LINE (X1)                 ECS076
01498          ADD +1                  TO WS-LINE.                      ECS076
01499                                                                   ECS076
01500      IF X3 = +8                                                   ECS076
01501        IF (D22-PL-DEV-T NOT EQUAL ZEROS) OR                       ECS076
01502           (D22-PL-DEV-R NOT EQUAL ZEROS)                          ECS076
01503          ADD +1                  TO X1                            ECS076
01504          MOVE DTL-22             TO PRT-LINE (X1)                 ECS076
01505          ADD +1                  TO WS-LINE                       ECS076
01506          GO TO 2910-NEXT-BENEFIT                                  ECS076
01507        ELSE                                                       ECS076
01508          GO TO 2910-NEXT-BENEFIT.                                 ECS076
01509                                                                   ECS076
01510      ADD +1 TO X3.                                                ECS076
01511      ADD +1 TO X4.                                                ECS076
01512                                                                   ECS076
01513      GO TO 2920-NEXT-LIMIT.                                       ECS076
01514                                                                   ECS076
01515  2910-NEXT-DMD-BENEFIT.                                           ECS076
01516                                                                   ECS076
01517      ADD +1                      TO X2.                           ECS076
01518      IF X2 GREATER THAN +30                                       ECS076
01519          GO TO 3000-PRINT.                                        ECS076
01520                                                                   ECS076
01521      IF AM-BENEFIT-DMD-TYPE (X2) NOT = AH-OVERRIDE-L1             ECS076
01522          GO TO 2910-NEXT-DMD-BENEFIT.                             ECS076
01523                                                                   ECS076
01524      MOVE AM-ALLOWABLE-DMD-BENEFITS (X2)                          ECS076
01525                                TO WS-SAVE-ALLOWABLE-BENEFITS.     ECS076
01526                                                                   ECS076
01527      PERFORM 5000-READ-PLAN-MASTER THRU 5999-READ-PLAN-EXIT.      ECS076
01528                                                                   ECS076
01529      IF NOT PLAN-MASTER-FOUND                                     ECS076
01530          GO TO 2910-NEXT-DMD-BENEFIT.                             ECS076
01531                                                                   ECS076
01532      MOVE SPACES                 TO DTL-21.                       ECS076
01533      MOVE ZEROS                  TO D22-PL-DEV-T                  ECS076
01534                                     D22-PL-DEV-R.                 ECS076
01535      MOVE +1                     TO X3  X4.                       ECS076
01536                                                                   ECS076
01537      MOVE AM-BENEFIT-DMD-CODE (X2)   TO D21-BENE.                 ECS076
01538      MOVE PL-ATT-AGE             TO D21-MAX-AGE.                  ECS076
01539                                                                   ECS076
01540      IF PL-DEV-CODE EQUAL SPACES OR ZEROS                         ECS076
01541         NEXT SENTENCE                                             ECS076
01542      ELSE                                                         ECS076
01543         MOVE PL-DEV-CODE         TO D22-PL-DEV-T.                 ECS076
01544                                                                   ECS076
01545      IF PL-DEV-PCT NUMERIC                                        ECS076
01546         IF PL-DEV-PCT NOT EQUAL ZEROS                             ECS076
01547            MOVE PL-DEV-PCT       TO D22-PL-DEV-P.                 ECS076
01548                                                                   ECS076
01549  2920-NEXT-DMD-LIMIT.                                             ECS076
01550                                                                   ECS076
01551      IF PL-LM-AGE (X3) = ZEROS  AND                               ECS076
01552         PL-LM-DUR (X3) = ZEROS  AND                               ECS076
01553         PL-LM-MOA (X3) = ZEROS  AND                               ECS076
01554         PL-LM-AMT (X3) = ZEROS                                    ECS076
01555          NEXT SENTENCE                                            ECS076
01556      ELSE                                                         ECS076
01557          MOVE PL-LM-AGE (X3)     TO D21-AGE (X4)                  ECS076
01558          MOVE PL-LM-DUR (X3)     TO D21-TRM (X4)                  ECS076
01559          MOVE PL-LM-MOA (X3)     TO D21-AMT (X4)                  ECS076
01560          MOVE PL-LM-AMT (X3)     TO D21-BEN (X4).                 ECS076
01561                                                                   ECS076
01562      IF X3 = +4                                                   ECS076
01563        MOVE +0                   TO X4                            ECS076
01564        IF (D21-LIMITS NOT = SPACES) OR                            ECS076
01565           (D22-PL-DEV-T NOT EQUAL ZEROS) OR                       ECS076
01566           (D22-PL-DEV-R NOT EQUAL ZEROS)                          ECS076
01567          ADD +1                  TO X1                            ECS076
01568          MOVE DTL-21             TO PRT-LINE (X1)                 ECS076
01569          ADD +1                  TO WS-LINE                       ECS076
01570          MOVE SPACES             TO DTL-21.                       ECS076
01571                                                                   ECS076
01572      IF X3 = +8                                                   ECS076
01573        IF D21-LIMITS NOT = SPACES                                 ECS076
01574          ADD +1                  TO X1                            ECS076
01575          MOVE DTL-21             TO PRT-LINE (X1)                 ECS076
01576          ADD +1                  TO WS-LINE.                      ECS076
01577                                                                   ECS076
01578      IF X3 = +8                                                   ECS076
01579        IF (D22-PL-DEV-T NOT EQUAL ZEROS) OR                       ECS076
01580           (D22-PL-DEV-R NOT EQUAL ZEROS)                          ECS076
01581          ADD +1                  TO X1                            ECS076
01582          MOVE DTL-22             TO PRT-LINE (X1)                 ECS076
01583          ADD +1                  TO WS-LINE                       ECS076
01584          GO TO 2910-NEXT-DMD-BENEFIT                              ECS076
01585        ELSE                                                       ECS076
01586          GO TO 2910-NEXT-DMD-BENEFIT.                             ECS076
01587                                                                   ECS076
01588      ADD +1 TO X3.                                                ECS076
01589      ADD +1 TO X4.                                                ECS076
01590                                                                   ECS076
01591      GO TO 2920-NEXT-DMD-LIMIT.                                   ECS076
01592                                                                   ECS076
01593                                                                   ECS076
01594  3000-PRINT.                                                      ECS076
01595      ADD WS-LINE                 TO WS-LINES.                     ECS076
01596                                                                   ECS076
01597      IF WS-LINES GREATER +60                                      ECS076
01598          PERFORM 9200-HDR-RTN-A                                   ECS076
01599          ADD WS-LINE             TO WS-LINES                      ECS076
01600      ELSE                                                         ECS076
01601          PERFORM 9210-HDR-RTN-B.                                  ECS076
01602                                                                   ECS076
01603      MOVE ZEROS                  TO WS-LINE.                      ECS076
01604      MOVE +1                     TO X2.                           ECS076
01605                                                                   ECS076
01606  3010-PRT-LOOP.                                                   ECS076
01607      MOVE PRT-LINE (X2)          TO PRT.                          ECS076
01608      MOVE PRT-SW (X2)            TO X.                            ECS076
01609      PERFORM 9300-PRT-RTN THRU 9399-PRT-RTN-EXIT.                 ECS076
01610      ADD +1                      TO X2.                           ECS076
01611                                                                   ECS076
01612      IF X2 LESS X1 OR = X1                                        ECS076
01613          GO TO 3010-PRT-LOOP.                                     ECS076
01614                                                                   ECS076
01615      MOVE SPACES                 TO PRT-TABLE.                    ECS076
01616      MOVE +0                     TO X1.                           ECS076
01617                                                                   ECS076
01618      GO TO 0200-READ-LOOP.                                        ECS076
01619                                                                   ECS076
01620  EJECT                                                            ECS076
01621  4000-FIND-EARN-METH.                                             ECS076
01622      IF AM-EARN-METHOD = ' '                                      ECS076
01623          MOVE 'STANDARD'             TO EMM-D2.                   ECS076
01624                                                                   ECS076
01625      IF AM-EARN-METHOD = 'R'                                      ECS076
01626          MOVE 'RULE OF 78'           TO EMM-D2.                   ECS076
01627                                                                   ECS076
01628      IF AM-EARN-METHOD = 'P'                                      ECS076
01629          MOVE 'PRO-RATA'             TO EMM-D2.                   ECS076
01630                                                                   ECS076
01631      IF AM-EARN-METHOD = 'M'                                      ECS076
01632          MOVE 'MID-POINT'            TO EMM-D2.                   ECS076
01633                                                                   ECS076
01634      IF AM-EARN-METHOD = 'A'                                      ECS076
01635          MOVE 'RULE-OF-ANTICIPATION' TO EMM-D2.                   ECS076
01636                                                                   ECS076
01637      IF AM-EARN-METHOD = 'C'                                      ECS076
01638          MOVE 'CALIF. REGULATIONS'   TO EMM-D2.                   ECS076
01639                                                                   ECS076
01640      IF AM-EARN-METHOD = 'O'                                      ECS076
01641          MOVE '1/3 R & 2/3 P'        TO EMM-D2.                   ECS076
01642                                                                   ECS076
01643  4999-FEM-EXIT.                                                   ECS076
01644      EXIT.                                                        ECS076
01645                                                                   ECS076
01646                                                                   ECS076
01647  5000-READ-PLAN-MASTER.                                           ECS076
01648                                                                   ECS076
01649      MOVE 'N'                        TO PLAN-MASTER-SWITCH.       ECS076
01650                                                                   ECS076
01651      MOVE AM-COMPANY-CD              TO PL-COMPANY-CD.            ECS076
01652      MOVE AM-CONTROL-A               TO PL-CONTROL-A.             ECS076
01653      MOVE WS-SAVE-BENEFIT-TYPE       TO PL-BENEFIT-TYPE.          ECS076
01654      MOVE WS-SAVE-BENEFIT-CODE       TO PL-BENEFIT-CODE.          ECS076
01655      MOVE WS-SAVE-BENEFIT-REVISION   TO PL-REVISION-NO.           ECS076
01656                                                                   ECS076
01657      READ PLAN-MSTR.                                              ECS076
01658                                                                   ECS076
01659      IF ERPLAN-FILE-STATUS NOT = '00'                             ECS076
01660          GO TO 5999-READ-PLAN-EXIT.                               ECS076
01661                                                                   ECS076
01662      MOVE +0                         TO X3.                       ECS076
01663      PERFORM 5100-NUMERIC-CHECK 8 TIMES.                          ECS076
01664                                                                   ECS076
01665      IF PL-LM-AGE (1) = ZEROS  AND                                ECS076
01666         PL-LM-DUR (1) = ZEROS  AND                                ECS076
01667         PL-LM-MOA (1) = ZEROS  AND                                ECS076
01668         PL-LM-AMT (1) = ZEROS                                     ECS076
01669          GO TO 5999-READ-PLAN-EXIT.                               ECS076
01670                                                                   ECS076
01671      MOVE 'Y'                        TO PLAN-MASTER-SWITCH.       ECS076
01672                                                                   ECS076
01673      GO TO 5999-READ-PLAN-EXIT.                                   ECS076
01674                                                                   ECS076
01675  5100-NUMERIC-CHECK.                                              ECS076
01676                                                                   ECS076
01677      ADD +1 TO X3.                                                ECS076
01678                                                                   ECS076
01679      IF PL-LM-AGE (X3) NOT NUMERIC                                ECS076
01680          MOVE ZEROS                  TO PL-LM-AGE (X3).           ECS076
01681      IF PL-LM-DUR (X3) NOT NUMERIC                                ECS076
01682          MOVE ZEROS                  TO PL-LM-DUR (X3).           ECS076
01683      IF PL-LM-MOA (X3) NOT NUMERIC                                ECS076
01684          MOVE ZEROS                  TO PL-LM-MOA (X3).           ECS076
01685      IF PL-LM-AMT (X3) NOT NUMERIC                                ECS076
01686          MOVE ZEROS                  TO PL-LM-AMT (X3).           ECS076
01687                                                                   ECS076
01688  5999-READ-PLAN-EXIT.                                             ECS076
01689      EXIT.                                                        ECS076
01690                                                                   ECS076
01691  EJECT                                                            ECS076
01692  8500-DATE-CONVERSION SECTION.                                    ECS076
01693                                  COPY ELCDCS.                     ECS076
01694  EJECT                                                            ECS076
01695  9000-END-JOB SECTION.                                            ECS076
01696                                  COPY ELCPRTC.                    ECS076
01697                                                                   ECS076
01698      CLOSE PRNTR                                                  ECS076
01699            PLAN-MSTR.                                             ECS076
01700                                                                   ECS076
01701      IF ERPLAN-FILE-STATUS NOT = ZERO                             ECS076
01702          MOVE 'ERROR OCCURED CLOSE - ERPLAN'                      ECS076
01703                                  TO  WS-ABEND-MESSAGE             ECS076
01704          MOVE ERPLAN-FILE-STATUS                                  ECS076
01705                                  TO  WS-ABEND-FILE-STATUS         ECS076
01706          PERFORM ABEND-PGM.                                       ECS076
01707                                                                   ECS076
01708                                                                   ECS076
01709      IF DTE-PGM-OPT = '2'                                         ECS076
01710          CLOSE ACCT-SEQUENTIAL                                    ECS076
01711      ELSE                                                         ECS076
01712          CLOSE ACC-MSTR                                           ECS076
01713          IF ERACCT-FILE-STATUS NOT = ZERO                         ECS076
01714              MOVE 'ERROR OCCURED CLOSE - ERACCTT'                 ECS076
01715                                  TO  WS-ABEND-MESSAGE             ECS076
01716              MOVE ERACCT-FILE-STATUS                              ECS076
01717                                  TO  WS-ABEND-FILE-STATUS         ECS076
01718              PERFORM ABEND-PGM.                                   ECS076
01719                                                                   ECS076
01720      GOBACK.                                                      ECS076
01721  EJECT                                                            ECS076
01722  9200-HDR-RTN-A.                                                  ECS076
01723      ADD +1                      TO WS-PAGE.                      ECS076
01724      MOVE WS-PAGE                TO H3-PAGE.                      ECS076
01725      MOVE HDR-1                  TO PRT.                          ECS076
01726      MOVE '1'                    TO X.                            ECS076
01727      PERFORM 9300-PRT-RTN THRU 9399-PRT-RTN-EXIT.                 ECS076
01728      MOVE HDR-2                  TO PRT.                          ECS076
01729      MOVE ' '                    TO X.                            ECS076
01730      PERFORM 9300-PRT-RTN THRU 9399-PRT-RTN-EXIT.                 ECS076
01731      MOVE HDR-3                  TO PRT.                          ECS076
01732      MOVE ' '                    TO X.                            ECS076
01733      PERFORM 9300-PRT-RTN THRU 9399-PRT-RTN-EXIT.                 ECS076
01734      MOVE +6                     TO WS-LINES.                     ECS076
01735                                                                   ECS076
01736  9210-HDR-RTN-B.                                                  ECS076
01737      MOVE HDR-5                  TO PRT.                          ECS076
01738      MOVE '0'                    TO X.                            ECS076
01739      PERFORM 9300-PRT-RTN THRU 9399-PRT-RTN-EXIT.                 ECS076
01740      ADD +2                      TO WS-LINES.                     ECS076
01741                                                                   ECS076
01742  9299-HDR-RTN-EXIT.                                               ECS076
01743      EXIT.                                                        ECS076
01744  EJECT                                                            ECS076
01745  9300-PRT-RTN.                                                    ECS076
01746                                  COPY ELCPRT2.                    ECS076
01747                                                                   ECS076
01748  9399-PRT-RTN-EXIT.                                               ECS076
01749      EXIT.                                                        ECS076
01750  EJECT                                                            ECS076
01751  ABEND-PGM SECTION.              COPY ELCABEND.                   ECS076
01752                                                                   ECS076
01753  LCP-WRITE-POS-PRT SECTION.                                       ECS076
01754      IF LCP-ASA = '+'                                             ECS076
01755          WRITE PRT AFTER 0 LINE                                   ECS076
01756      ELSE                                                         ECS076
01757      IF LCP-ASA = ' '                                             ECS076
01758          WRITE PRT AFTER ADVANCING 1 LINE                         ECS076
01759      ELSE                                                         ECS076
01760      IF LCP-ASA = '0'                                             ECS076
01761          WRITE PRT AFTER ADVANCING 2 LINE                         ECS076
01762      ELSE                                                         ECS076
01763      IF LCP-ASA = '-'                                             ECS076
01764          WRITE PRT AFTER ADVANCING 3 LINE                         ECS076
01765      ELSE                                                         ECS076
01766      IF LCP-ASA = '1'                                             ECS076
01767          WRITE PRT AFTER ADVANCING PAGE                           ECS076
01768      ELSE                                                         ECS076
01769      IF LCP-ASA = '2'                                             ECS076
01770          WRITE PRT AFTER ADVANCING LCP-CH2                        ECS076
01771      ELSE                                                         ECS076
01772      IF LCP-ASA = '3'                                             ECS076
01773          WRITE PRT AFTER ADVANCING LCP-CH3                        ECS076
01774      ELSE                                                         ECS076
01775      IF LCP-ASA = '4'                                             ECS076
01776          WRITE PRT AFTER ADVANCING LCP-CH4                        ECS076
01777      ELSE                                                         ECS076
01778      IF LCP-ASA = '5'                                             ECS076
01779          WRITE PRT AFTER ADVANCING LCP-CH5                        ECS076
01780      ELSE                                                         ECS076
01781      IF LCP-ASA = '6'                                             ECS076
01782          WRITE PRT AFTER ADVANCING LCP-CH6                        ECS076
01783      ELSE                                                         ECS076
01784      IF LCP-ASA = '7'                                             ECS076
01785          WRITE PRT AFTER ADVANCING LCP-CH7                        ECS076
01786      ELSE                                                         ECS076
01787      IF LCP-ASA = '8'                                             ECS076
01788          WRITE PRT AFTER ADVANCING LCP-CH8                        ECS076
01789      ELSE                                                         ECS076
01790      IF LCP-ASA = '9'                                             ECS076
01791          WRITE PRT AFTER ADVANCING LCP-CH9                        ECS076
01792      ELSE                                                         ECS076
01793      IF LCP-ASA = 'A'                                             ECS076
01794          WRITE PRT AFTER ADVANCING LCP-CH10                       ECS076
01795      ELSE                                                         ECS076
01796      IF LCP-ASA = 'B'                                             ECS076
01797          WRITE PRT AFTER ADVANCING LCP-CH11                       ECS076
01798      ELSE                                                         ECS076
01799      IF LCP-ASA = 'C'                                             ECS076
01800          WRITE PRT AFTER ADVANCING LCP-CH12                       ECS076
01801      ELSE                                                         ECS076
01802      IF LCP-ASA = 'V'                                             ECS076
01803          WRITE PRT AFTER ADVANCING LCP-P01                        ECS076
01804      ELSE                                                         ECS076
01805      IF LCP-ASA = 'W'                                             ECS076
01806          WRITE PRT AFTER ADVANCING LCP-P02                        ECS076
01807      ELSE                                                         ECS076
01808      DISPLAY 'ASA CODE ERROR'.                                    ECS076
01809  LCP-WRITE-END-PRT.                                               ECS076
01810      EXIT.                                                        ECS076
