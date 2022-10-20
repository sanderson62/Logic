00001  IDENTIFICATION DIVISION.                                         03/19/98
00002                                                                   ECS096
00003  PROGRAM-ID.                 ECS096.                                 LV003
00004 *              PROGRAM CONVERTED BY                               ECS096
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ECS096
00006 *              CONVERSION DATE 02/09/96 09:33:03.                 ECS096
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          ECS096
00008 *                            VMOD=2.015.                          ECS096
00009                                                                   ECS096
00010 *AUTHOR.        LOGIC, INC.                                       ECS096
00011 *               DALLAS, TEXAS.                                    ECS096
00012                                                                   ECS096
00013 *DATE-COMPILED.                                                   ECS096
00014                                                                   ECS096
00015 *SECURITY.   *****************************************************ECS096
00016 *            *                                                   *ECS096
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ECS096
00018 *            *                                                   *ECS096
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS096
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ECS096
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *ECS096
00022 *            *                                                   *ECS096
00023 *            *****************************************************ECS096
00024                                                                   ECS096
00025 *REMARKS.                                                         ECS096
00026 *        GENERATE NEW MORTALITY RESERVES FROM INPUT PARAMETER     ECS096
00027 *        PROVIDED BY THE MORTALITY TABLE CONTROL RECORDS FOUND    ECS096
00028 *        ON THE ONLINE ELCNTL FILE.                               ECS096
00029                                                                   ECS096
101005******************************************************************
101005*                   C H A N G E   L O G
101005*
101005* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
101005*-----------------------------------------------------------------
101005*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
101005* EFFECTIVE    NUMBER
101005*-----------------------------------------------------------------
101005* 101005    2005083100004  PEMA  ADD 2001CSO AN & AL TABLES
101005******************************************************************
00030  ENVIRONMENT DIVISION.                                            ECS096
00031  INPUT-OUTPUT SECTION.                                            ECS096
00032  FILE-CONTROL.                                                    ECS096
00033                                                                   ECS096
00034      SELECT DISK-DATE        ASSIGN TO SYS019-UT-3380-S-SYS019.   ECS096
00035                                                                   ECS096
00036      SELECT ELCNTL           ASSIGN TO SYS010-3380-ELCNTL         ECS096
00037                              ORGANIZATION IS INDEXED              ECS096
00038                              ACCESS IS DYNAMIC                    ECS096
00039                              RECORD KEY IS CF-CONTROL-PRIMARY     ECS096
00040                              FILE STATUS IS W-ELCNTL-FILE-STATUS. ECS096
00041                                                                   ECS096
00042      SELECT FICH             ASSIGN TO SYS020-UT-2400-S-SYS020.   ECS096
00043      SELECT PRNTR            ASSIGN TO SYS008-UR-1403-S-SYS008.   ECS096
00044      SELECT NEW-RFAC-FILE    ASSIGN TO SYS011-UT-2400-S-SYS011.   ECS096
00045                                  EJECT                            ECS096
00046  DATA DIVISION.                                                   ECS096
00047  FILE SECTION.                                                    ECS096
00048                                  EJECT                            ECS096
00049  FD  DISK-DATE                                                    ECS096
00050                              COPY ELCDTEFD.                       ECS096
00051                                  EJECT                            ECS096
00052  FD  ELCNTL.                                                      ECS096
00053                                                                   ECS096
00054      COPY ELCCNTL SUPPRESS.                                       ECS096
00055                                  EJECT                            ECS096
00056  FD  FICH                                                         ECS096
00057                              COPY ELCFCHFD.                       ECS096
00058                                  EJECT                            ECS096
00059  FD  PRNTR                                                        ECS096
00060                              COPY ELCPRTFD.                       ECS096
00061  FD  NEW-RFAC-FILE                                                ECS096
00062      BLOCK CONTAINS 0 RECORDS
00063      RECORDING MODE IS F.                                         ECS096
00064                                                                   ECS096
00065  01  NEW-RFAC-REC            PIC  X(1240).                        ECS096
00066                                  EJECT                            ECS096
00067  WORKING-STORAGE SECTION.                                         ECS096
00068  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      ECS096
00069  77  FILLER PIC  X(32) VALUE '********************************'.  ECS096
00070  77  FILLER PIC  X(32) VALUE '     ECS096 WORKING-STORAGE     '.  ECS096
00071  77  FILLER PIC  X(32) VALUE '********** VMOD=2.015 **********'.  ECS096
00072                                                                   ECS096
00073  01  MISC-WORK-AREAS     SYNC.                                    ECS096
00074      12  X                   PIC  X              VALUE SPACE.     ECS096
00075      12  DX-AGE-HOLD         PIC  9(4)           VALUE ZEROS.     ECS096
00076      12  MAX-TERM            PIC S999            VALUE +240.      ECS096
00077      12  PGM-SUB             PIC S999    COMP-3  VALUE +296.      ECS096
00078      12  SW-ON               PIC  X              VALUE '1'.       ECS096
00079      12  SW-OFF              PIC  X              VALUE SPACE.     ECS096
00080      12  W-LAST-MORT-TABLE-IND                                    ECS096
00081                              PIC  X              VALUE SPACE.     ECS096
00082          88  W-LAST-MORT-TABLE-PROCESSED         VALUE 'Y'.       ECS096
00083      12  OUTPUT-SW           PIC  X              VALUE SPACE.     ECS096
00084          88  GOOD-OUTPUT                         VALUE '1'.       ECS096
00085          88  NO-OUTPUT                           VALUE SPACE.     ECS096
00086      12  W-ELCNTL-FILE-STATUS                                     ECS096
00087                              PIC  XX         VALUE '00'.          ECS096
00088          88  W-VALID-CNTL-FILE-STATUS        VALUE '00' '97'.     ECS096
00089          88  W-CNTL-NOT-FOUND                VALUE '10' '23'.     ECS096
00090                                                                   ECS096
00091  01  COMP-WORK-AREA      COMP.                                    ECS096
00092      12  X1                  PIC S9(4)           VALUE +0.        ECS096
00093      12  X2                  PIC S9(4)           VALUE +0.        ECS096
00094      12  X3                  PIC S9(4)           VALUE +0.        ECS096
00095      12  X4                  PIC S9(4)           VALUE +0.        ECS096
00096      12  B1                  PIC S9(4)           VALUE +1.        ECS096
00097      12  B2                  PIC S9(4)           VALUE +2.        ECS096
00098      12  B3                  PIC S9(4)           VALUE +3.        ECS096
00099      12  B4                  PIC S9(4)           VALUE +4.        ECS096
00100      12  B5                  PIC S9(4)           VALUE +5.        ECS096
00101      12  B101                PIC S9(4)           VALUE +101.      ECS096
00102      12  CTS                 PIC S9(4)           VALUE +0.        ECS096
00103      12  CTR                 PIC S9(4)           VALUE +0.        ECS096
00104      12  TX                  PIC S9(5)           VALUE +0.        ECS096
00105      12  SUB1                PIC S9(4)           VALUE +0.        ECS096
00106      12  SUB2                PIC S9(4)           VALUE +0.        ECS096
00107      12  SUBX                PIC S9(4)           VALUE +0.        ECS096
00108      12  TABLE-SIZE          PIC S9(4)           VALUE +100.      ECS096
00109      12  TABLE-SIZE-VX       PIC S9(4)           VALUE +0.        ECS096
00110                                                                   ECS096
00111  01  COMP-3-WORK-AREA    COMP-3.                                  ECS096
00112      12  HOLD-1ST-LX         PIC S9(9)           VALUE +0.        ECS096
00113      12  HOLD-2ND-LX         PIC S9(9)           VALUE +0.        ECS096
00114      12  MONTHLY-LX          PIC S9(9)           VALUE +0.        ECS096
00115      12  NEW-AL-VALUE        PIC S9(9)           VALUE +0.        ECS096
00116      12  CTA                 PIC S9(5)           VALUE +0.        ECS096
00117      12  CTT                 PIC S9(5)           VALUE +0.        ECS096
00118      12  CTX                 PIC S9(5)           VALUE +0.        ECS096
00119      12  M                   PIC S9(5)           VALUE +0.        ECS096
00120      12  P                   PIC S9(5)           VALUE +0.        ECS096
00121      12  XX                  PIC S9(5)           VALUE +0.        ECS096
00122      12  R                   PIC S9(5)           VALUE +0.        ECS096
00123      12  T                   PIC S9(5)           VALUE +0.        ECS096
00124      12  RSVR                PIC S9(8)V9(9)      VALUE +0.        ECS096
00125      12  FACTOR-A            PIC S9(8)V9(9)      VALUE +0.        ECS096
00126      12  FACTOR-B            PIC S9(8)V9(9)      VALUE +0.        ECS096
00127      12  CX                  PIC S9(9)V99        VALUE +0.        ECS096
00128      12  ANSB                PIC S9(8)V9(9)      VALUE +0.        ECS096
00129      12  ANSC                PIC S9(8)V9(9)      VALUE +0.        ECS096
00130      12  ANSD                PIC S9(8)V9(9)      VALUE +0.        ECS096
00131      12  V                   PIC S99V9(9)        VALUE +0.        ECS096
00132      12  DX                  PIC S9(9)V99        VALUE +0.        ECS096
00133      12  DY                  PIC S9(9)V99        VALUE +0.        ECS096
00134      12  DX-WORK             PIC S9(9)V99        VALUE +0.        ECS096
00135      12  CX-WORK             PIC S9(9)V99        VALUE +0.        ECS096
00136      12  LINE-CNT            PIC S999            VALUE +99.       ECS096
00137      12  PAGE-CNT            PIC S9(5)           VALUE +0.        ECS096
00138      12  RFAC-OLD            PIC S9(7)           VALUE +0.        ECS096
00139      12  RFAC-NEW            PIC S9(7)           VALUE +0.        ECS096
00140      12  RFAC-DUP            PIC S9(7)           VALUE +0.        ECS096
00141      12  RFAC-OUT            PIC S9(7)           VALUE +0.        ECS096
00142      12  RFAC-DEL            PIC S9(7)           VALUE +0.        ECS096
00143      12  VX-RESULT           PIC S9V9(12)        VALUE +0.        ECS096
00144      12  VX-MULTIPLIER       PIC S9V9(12)        VALUE +0.        ECS096
00145      12  SV-RES-ADJ          PIC S9V9(4)         VALUE +0.        ECS096
00146      12  SV-JOINT-FACTOR     PIC S9V9(4)         VALUE +0.        ECS096
00147                                  EJECT                            ECS096
00148  01  TABLE-CONTROL.                                               ECS096
00149      12  TABLE-CODE          PIC  X(5).                           ECS096
00150      12  TABLE-TYPE          PIC  X.                              ECS096
00151      12  TABLE-AL-AN         PIC  XX.                             ECS096
00152                                                                   ECS096
00153  01  TABLE-CONTROL-HOLD.                                          ECS096
00154      12  TABLE-CODE-HOLD     PIC  X(5)           VALUE SPACES.    ECS096
00155      12  TABLE-TYPE-HOLD     PIC  X              VALUE SPACE.     ECS096
00156          88  TABLE-NETPAY                        VALUE 'N'.       ECS096
00157      12  TABLE-AL-AN-HOLD    PIC  XX             VALUE SPACES.    ECS096
00158                                  EJECT                            ECS096
00159 * ******LX-SAVE-AREA******                                        ECS096
00160  01  LX-SAVE.                                                     ECS096
00161      12  LX-TABLE            PIC  X(5).                           ECS096
101005         88  01CSO                       VALUE '01CSO'.           ECS096
00162          88  41CSO                       VALUE '41CSO'.           ECS096
00163          88  58CET                       VALUE '58CET'.           ECS096
00164          88  58CSO                       VALUE '58CSO'.           ECS096
00165          88  58FSO                       VALUE '58FSO'.           ECS096
CIDMOD         88  58UET                       VALUE '58UET'.           00000351
CIDMOD         88  58USO                       VALUE '58USO'.           00000351
00166          88  60CSG                       VALUE '60CSG'.           ECS096
CIDMOD         88  80CSO                       VALUE '80CSO'.           ECS096
00167          88  80MSO                       VALUE '80MSO'.           ECS096
00168          88  80FSO                       VALUE '80FSO'.           ECS096
00169          88  80MET                       VALUE '80MET'.           ECS096
00170          88  80FET                       VALUE '80FET'.           ECS096
00171          88  80GBT                       VALUE '80GBT'.           ECS096
CIDMOD         88  80UET                       VALUE '80UET'.           ECS096
LGC188         88  80USO                       VALUE '80USO'.           ECS096
00172          88  USERTBL                     VALUE 'XXXXX'.           ECS096
00173          88  NULLTBL                     VALUE 'ZERO '.           ECS096
00174          88  GOOD-TABLE                  VALUE '41CSO' '58CET'    ECS096
00175                                                '58CSO' '60CSG'    ECS096
00176                                                '58FSO' '80MSO'    ECS096
00177                                                '80FSO' '80MET'    ECS096
00178                                                '80FET' '80GBT'    ECS096
CIDMOD                                               '58UET' '58USO'
CIDMOD                                               '80CSO' '80UET'
101005                                               '80USO' '01CSO'
00179                                                'XXXXX' 'ZERO '.   ECS096
00180      12  LX-TABLE-TYPE       PIC  X.                              ECS096
00181          88  SINGLE                      VALUE 'S'.               ECS096
00182          88  JOINT                       VALUE 'J' 'C'.           ECS096
00183          88  GOOD-BENEFIT                VALUE 'S' 'J' 'C'.       ECS096
00184      12  LX-INTEREST         PIC V9(4)         COMP-3.            ECS096
00185      12  LX-AGE-METHOD       PIC  XX.                             ECS096
00186          88  AGE-LAST                    VALUE 'AL'.              ECS096
00187          88  AGE-NEAR                    VALUE 'AN'.              ECS096
00188          88  GOOD-AL-AN                  VALUE 'AL' 'AN'.         ECS096
00189      12  LX-RES-ADJ          PIC S9V9(4)       COMP-3.            ECS096
00190      12  LX-ADJUSTMENT-DIRECTION                                  ECS096
00191                              PIC  X.                              ECS096
00192          88  LX-PLUS                     VALUE '+'.               ECS096
00193          88  LX-MINUS                    VALUE '-' ' '.           ECS096
00194      12  LX-JOINT-FACTOR     PIC S9V9(4)       COMP-3.            ECS096
00195      12  LX-JOINT-CODE       PIC  X.                              ECS096
00196      12  LX-PC-Q             PIC  X.                              ECS096
00197          88  LX-Q-ADJ                    VALUE 'Y'.               ECS096
00198      12  LX-MORT-CDE         PIC  X(4).                           ECS096
00199      12  LX-COMMENTS         PIC  X(15).                          ECS096
00200                                  EJECT                            ECS096
00201 ******RFAC-REC****LENGTH=1240******                               ECS096
00202  01  RF-REC.                                                      ECS096
00203      12  RF-ID               PIC  XX             VALUE 'RF'.      ECS096
00204      12  RF-CTL.                                                  ECS096
00205          16  RF-CODE.                                             ECS096
00206              20  RF-TBL      PIC  X              VALUE SPACE.     ECS096
00207              20  RF-INT      PIC  XX             VALUE SPACE.     ECS096
00208              20  RF-TYPE     PIC  X              VALUE SPACE.     ECS096
00209      12  RF-INT-ADJ          PIC S9V9(4) COMP-3 VALUE +0.         ECS096
00210      12  RF-RESV-ADJ         PIC S9V9(4) COMP-3 VALUE +0.         ECS096
00211      12  RF-JOINT-ADJ        PIC S9V9(4) COMP-3 VALUE +0.         ECS096
00212      12  RF-PLUS-MINUS       PIC  X.                              ECS096
00213      12  FILLER              PIC  X(24)          VALUE SPACE.     ECS096
00214      12  CX-DX-FACTORS.                                           ECS096
00215          16  RF-CX-FACTOR        OCCURS  100  TIMES               ECS096
00216                              PIC S9(9)V99 COMP-3.                 ECS096
00217          16  RF-DX-FACTOR        OCCURS  100  TIMES               ECS096
00218                              PIC S9(9)V99 COMP-3.                 ECS096
00219                                  EJECT                            ECS096
00220 * ******LX-WORK-AREA*****                                         ECS096
00221  01  LX-WORK-TABLE.                                               ECS096
00222      12  LX-OCCURS       OCCURS  100  TIMES  COMP-3.              ECS096
00223          16  LX-VALUE        PIC S9(9).                           ECS096
00224                                                                   ECS096
00225 * ******JX-WORK-AREA*****                                         ECS096
00226  01  JX-WORK-TABLE.                                               ECS096
00227      12  JX-OCCURS       OCCURS  100  TIMES  COMP-3.              ECS096
00228          16  JX-VALUE        PIC S9(9).                           ECS096
00229                                                                   ECS096
00230 * ******VX-WORK-AREA*****                                         ECS096
00231  01  VX-WORK-TABLE.                                               ECS096
00232      12  VX-OCCURS       OCCURS  100  TIMES  COMP-3.              ECS096
00233          16  VX-VALUE        PIC S9V9(12).                        ECS096
00234                                                                   ECS096
00235 * ******AL-WORK-AREA*****                                         ECS096
00236  01  AL-WORK-TABLE.                                               ECS096
00237      12  AL-OCCURS       OCCURS  100  TIMES  COMP-3.              ECS096
00238          16  AL-VALUE        PIC S9(9).                           ECS096
00239                                                                   ECS096
00240 * ******DX-WORK-AREA*****                                         ECS096
00241  01  DX-WORK-TABLE.                                               ECS096
00242      12  DX-OCCURS       OCCURS  101  TIMES  COMP-3.              ECS096
00243          16  DX-VALUE        PIC S9(9)V99.                        ECS096
00244                                                                   ECS096
00245 * ******CX-WORK-AREA*****                                         ECS096
00246  01  CX-WORK-TABLE.                                               ECS096
00247      12  CX-OCCURS       OCCURS  100  TIMES  COMP-3.              ECS096
00248          16  CX-VALUE        PIC S9(9)V99.                        ECS096
00249                                  EJECT                            ECS096
00250  01  HEAD-LINE-1.                                                 ECS096
00251      12  FILLER              PIC  X(49)          VALUE SPACES.    ECS096
00252      12  FILLER              PIC  X(25)          VALUE            ECS096
00253              'RESERVE FACTOR GENERATION'.                         ECS096
00254      12  FILLER              PIC  X(45)          VALUE SPACES.    ECS096
00255      12  FILLER              PIC  X(8)           VALUE 'ECS096'.  ECS096
00256                                                                   ECS096
00257  01  HEAD-LINE-2.                                                 ECS096
00258      12  FILLER              PIC  X(47)          VALUE SPACES.    ECS096
00259      12  HD-CLIENT           PIC  X(30)          VALUE SPACES.    ECS096
00260      12  FILLER              PIC  X(42)          VALUE SPACES.    ECS096
00261      12  HD-RUN              PIC  X(8)           VALUE SPACES.    ECS096
00262                                                                   ECS096
00263  01  HEAD-LINE-3.                                                 ECS096
00264      12  FILLER              PIC  X(53)          VALUE SPACES.    ECS096
00265      12  HD-DATE             PIC  X(18)          VALUE SPACES.    ECS096
00266      12  FILLER              PIC  X(48)          VALUE SPACES.    ECS096
00267      12  FILLER              PIC  X(5)           VALUE 'PAGE '.   ECS096
00268      12  HD-PAGE             PIC ZZ,ZZZ.                          ECS096
00269                                                                   ECS096
00270  01  HEAD-LINE-4.                                                 ECS096
00271      12  FILLER              PIC  X(17)    VALUE                  ECS096
00272              ' MORTALITY CODE:'.                                  ECS096
00273      12  HD-MORT             PIC  X(6)     VALUE SPACES.          ECS096
00274      12  FILLER              PIC  X(7)     VALUE 'TABLE:'.        ECS096
00275      12  HD-TABLE            PIC  X(7)     VALUE SPACES.          ECS096
00276      12  FILLER              PIC  X(10)    VALUE 'INTEREST:'.     ECS096
00277      12  HD-INT              PIC .9999.                           ECS096
00278      12  FILLER              PIC  X(14)    VALUE '  AGE METHOD:'. ECS096
00279      12  HD-AGE              PIC  X(4)     VALUE SPACES.          ECS096
00280      12  FILLER              PIC  X(9)     VALUE 'BENEFIT:'.      ECS096
00281      12  HD-BEN              PIC  XX       VALUE SPACES.          ECS096
00282                                                                   ECS096
00283  01  HEAD-LINE-5.                                                 ECS096
00284      12  FILLER              PIC  X(23)      VALUE SPACES.        ECS096
00285      12  FILLER              PIC  X(13)      VALUE                ECS096
00286              'RESERVE ADJ:'.                                      ECS096
00287      12  HD-RES-ADJ          PIC 9.9999.                          ECS096
00288      12  FILLER              PIC  X(17)      VALUE                ECS096
00289              '  ADJ DIRECTION:'.                                  ECS096
00290      12  HD-ADJ-DIR          PIC  XXX.                            ECS096
00291      12  FILLER              PIC  X(14)      VALUE                ECS096
00292              'JOINT FACTOR:'.                                     ECS096
00293      12  HD-JOINT-FACT       PIC 99.9999.                         ECS096
00294      12  FILLER              PIC  X(14)      VALUE                ECS096
00295              '  JOINT CODE:'.                                     ECS096
00296      12  HD-JNT-CODE         PIC  X(9).                           ECS096
00297      12  FILLER              PIC  X(7)       VALUE                ECS096
00298              'Q ADJ:'.                                            ECS096
00299      12  HD-PC-Q-ADJ         PIC  XXX.                            ECS096
00300                                                                   ECS096
00301  01  HEAD-LINE-6.                                                 ECS096
00302      12  FILLER              PIC  X(22)      VALUE '    AGE'.     ECS096
00303      12  FILLER              PIC  X(22)      VALUE 'LX'.          ECS096
00304      12  FILLER              PIC  X(24)      VALUE 'VX'.          ECS096
00305      12  FILLER              PIC  X(23)      VALUE 'DX'.          ECS096
00306      12  FILLER              PIC  X(10)      VALUE 'CX'.          ECS096
00307                                                                   ECS096
00308  01  DETAIL-LINE.                                                 ECS096
00309      12  FILLER              PIC  XXX        VALUE SPACES.        ECS096
00310      12  DTL-AGE             PIC ZZZ9.                            ECS096
00311      12  FILLER              PIC  X(10)      VALUE SPACES.        ECS096
00312      12  DTL-LX              PIC ZZZ,ZZZ,ZZZ.                     ECS096
00313      12  FILLER              PIC  X(10)      VALUE SPACES.        ECS096
00314      12  DTL-VX              PIC Z.Z(12).                         ECS096
00315      12  FILLER              PIC  X(10)      VALUE SPACES.        ECS096
00316      12  DTL-DX              PIC ZZZ,ZZZ,ZZZ.9.                   ECS096
00317      12  FILLER              PIC  X(10)      VALUE SPACES.        ECS096
00318      12  DTL-CX              PIC ZZZ,ZZZ,ZZZ.9.                   ECS096
00319                                                                   ECS096
00320  01  NETPAY-HEAD.                                                 ECS096
00321      12  FILLER              PIC  X(50)          VALUE SPACES.    ECS096
00322      12  FILLER              PIC  X(21)          VALUE            ECS096
00323              '****** NET PAY ******'.                             ECS096
00324                                                                   ECS096
00325  01  TOTAL-LINE.                                                  ECS096
00326      12  FILLER              PIC  X              VALUE SPACE.     ECS096
00327      12  TL-DESC             PIC  X(20)          VALUE SPACES.    ECS096
00328      12  FILLER              PIC  X              VALUE SPACE.     ECS096
00329      12  TL-CNT              PIC Z,ZZZ,ZZZ.                       ECS096

      *** AHL TABLEA  58CSO ALB JOINT USING 3 YEAR SET BACK.
       01  AHL-USER-TABLEA     COMP-3.
           12  FILLER              PIC S9(9)       VALUE +09964600.
           12  FILLER              PIC S9(9)       VALUE +09920467.
           12  FILLER              PIC S9(9)       VALUE +09904197.
           12  FILLER              PIC S9(9)       VALUE +09889440.
           12  FILLER              PIC S9(9)       VALUE +09836370.
           12  FILLER              PIC S9(9)       VALUE +09807717.
           12  FILLER              PIC S9(9)       VALUE +09781206.
           12  FILLER              PIC S9(9)       VALUE +09755791.
           12  FILLER              PIC S9(9)       VALUE +09731319.
           12  FILLER              PIC S9(9)       VALUE +09707638.
           12  FILLER              PIC S9(9)       VALUE +09684547.
           12  FILLER              PIC S9(9)       VALUE +09661754.
           12  FILLER              PIC S9(9)       VALUE +09638965.
           12  FILLER              PIC S9(9)       VALUE +09615846.
           12  FILLER              PIC S9(9)       VALUE +09592109.
           12  FILLER              PIC S9(9)       VALUE +09567617.
           12  FILLER              PIC S9(9)       VALUE +09542231.
           12  FILLER              PIC S9(9)       VALUE +09515817.
           12  FILLER              PIC S9(9)       VALUE +09488336.
           12  FILLER              PIC S9(9)       VALUE +09459845.
           12  FILLER              PIC S9(9)       VALUE +09430352.
           12  FILLER              PIC S9(9)       VALUE +09399870.
           12  FILLER              PIC S9(9)       VALUE +09368453.
           12  FILLER              PIC S9(9)       VALUE +09336113.
           12  FILLER              PIC S9(9)       VALUE +09302952.
           12  FILLER              PIC S9(9)       VALUE +09269167.
           12  FILLER              PIC S9(9)       VALUE +09234810.
           12  FILLER              PIC S9(9)       VALUE +09199889.
           12  FILLER              PIC S9(9)       VALUE +09164458.
           12  FILLER              PIC S9(9)       VALUE +09128477.
           12  FILLER              PIC S9(9)       VALUE +09091954.
           12  FILLER              PIC S9(9)       VALUE +09054896.
           12  FILLER              PIC S9(9)       VALUE +09017222.
           12  FILLER              PIC S9(9)       VALUE +08978849.
           12  FILLER              PIC S9(9)       VALUE +08939654.
           12  FILLER              PIC S9(9)       VALUE +08899381.
           12  FILLER              PIC S9(9)       VALUE +08857780.
           12  FILLER              PIC S9(9)       VALUE +08814606.
           12  FILLER              PIC S9(9)       VALUE +08769488.
           12  FILLER              PIC S9(9)       VALUE +08722064.
           12  FILLER              PIC S9(9)       VALUE +08671982.
           12  FILLER              PIC S9(9)       VALUE +08618815.
           12  FILLER              PIC S9(9)       VALUE +08562192.
           12  FILLER              PIC S9(9)       VALUE +08501758.
           12  FILLER              PIC S9(9)       VALUE +08437015.
           12  FILLER              PIC S9(9)       VALUE +08367419.
           12  FILLER              PIC S9(9)       VALUE +08292446.
           12  FILLER              PIC S9(9)       VALUE +08211541.
           12  FILLER              PIC S9(9)       VALUE +08124233.
           12  FILLER              PIC S9(9)       VALUE +08030054.
           12  FILLER              PIC S9(9)       VALUE +07928510.
           12  FILLER              PIC S9(9)       VALUE +07819071.
           12  FILLER              PIC S9(9)       VALUE +07701248.
           12  FILLER              PIC S9(9)       VALUE +07574535.
           12  FILLER              PIC S9(9)       VALUE +07438420.
           12  FILLER              PIC S9(9)       VALUE +07292400.
           12  FILLER              PIC S9(9)       VALUE +07135965.
           12  FILLER              PIC S9(9)       VALUE +06968602.
           12  FILLER              PIC S9(9)       VALUE +06789914.
           12  FILLER              PIC S9(9)       VALUE +06599622.
           12  FILLER              PIC S9(9)       VALUE +06397493.
           12  FILLER              PIC S9(9)       VALUE +06183420.
           12  FILLER              PIC S9(9)       VALUE +05957431.
           12  FILLER              PIC S9(9)       VALUE +05719622.
           12  FILLER              PIC S9(9)       VALUE +05470246.
           12  FILLER              PIC S9(9)       VALUE +05209739.
           12  FILLER              PIC S9(9)       VALUE +04938667.
           12  FILLER              PIC S9(9)       VALUE +04657819.
           12  FILLER              PIC S9(9)       VALUE +04368282.
           12  FILLER              PIC S9(9)       VALUE +04071587.
           12  FILLER              PIC S9(9)       VALUE +03769821.
           12  FILLER              PIC S9(9)       VALUE +03465551.
           12  FILLER              PIC S9(9)       VALUE +03161669.
           12  FILLER              PIC S9(9)       VALUE +02861209.
           12  FILLER              PIC S9(9)       VALUE +02567011.
           12  FILLER              PIC S9(9)       VALUE +02281623.
           12  FILLER              PIC S9(9)       VALUE +02007404.
           12  FILLER              PIC S9(9)       VALUE +01746548.
           12  FILLER              PIC S9(9)       VALUE +01501113.
           12  FILLER              PIC S9(9)       VALUE +01273078.
           12  FILLER              PIC S9(9)       VALUE +01064229.
           12  FILLER              PIC S9(9)       VALUE +00875900.
           12  FILLER              PIC S9(9)       VALUE +00708891.
           12  FILLER              PIC S9(9)       VALUE +00563422.
           12  FILLER              PIC S9(9)       VALUE +00439109.
           12  FILLER              PIC S9(9)       VALUE +00335034.
           12  FILLER              PIC S9(9)       VALUE +00249841.
           12  FILLER              PIC S9(9)       VALUE +00181790.
           12  FILLER              PIC S9(9)       VALUE +00128836.
           12  FILLER              PIC S9(9)       VALUE +00088755.
           12  FILLER              PIC S9(9)       VALUE +00059288.
           12  FILLER              PIC S9(9)       VALUE +00038278.
           12  FILLER              PIC S9(9)       VALUE +00023788.
           12  FILLER              PIC S9(9)       VALUE +00014154.
           12  FILLER              PIC S9(9)       VALUE +00008007.
           12  FILLER              PIC S9(9)       VALUE +00004263.
           12  FILLER              PIC S9(9)       VALUE +00002094.
           12  FILLER              PIC S9(9)       VALUE +00000907.
           12  FILLER              PIC S9(9)       VALUE +00000305.
           12  FILLER              PIC S9(9)       VALUE +00000055.

      *** AHL TABLEB  80CSO ALB JOINT USING 3 YEAR SET BACK.
       01  AHL-USER-TABLEB     COMP-3.
           12  FILLER              PIC S9(9)       VALUE +09964600.
           12  FILLER              PIC S9(9)       VALUE +09938393.
           12  FILLER              PIC S9(9)       VALUE +09928157.
           12  FILLER              PIC S9(9)       VALUE +09918328.
           12  FILLER              PIC S9(9)       VALUE +09890079.
           12  FILLER              PIC S9(9)       VALUE +09872581.
           12  FILLER              PIC S9(9)       VALUE +09856002.
           12  FILLER              PIC S9(9)       VALUE +09840140.
           12  FILLER              PIC S9(9)       VALUE +09824894.
           12  FILLER              PIC S9(9)       VALUE +09810162.
           12  FILLER              PIC S9(9)       VALUE +09795746.
           12  FILLER              PIC S9(9)       VALUE +09781450.
           12  FILLER              PIC S9(9)       VALUE +09766685.
           12  FILLER              PIC S9(9)       VALUE +09750967.
           12  FILLER              PIC S9(9)       VALUE +09733910.
           12  FILLER              PIC S9(9)       VALUE +09715035.
           12  FILLER              PIC S9(9)       VALUE +09694157.
           12  FILLER              PIC S9(9)       VALUE +09671291.
           12  FILLER              PIC S9(9)       VALUE +09646740.
           12  FILLER              PIC S9(9)       VALUE +09620805.
           12  FILLER              PIC S9(9)       VALUE +09593884.
           12  FILLER              PIC S9(9)       VALUE +09566463.
           12  FILLER              PIC S9(9)       VALUE +09538738.
           12  FILLER              PIC S9(9)       VALUE +09510999.
           12  FILLER              PIC S9(9)       VALUE +09483435.
           12  FILLER              PIC S9(9)       VALUE +09456142.
           12  FILLER              PIC S9(9)       VALUE +09429210.
           12  FILLER              PIC S9(9)       VALUE +09402449.
           12  FILLER              PIC S9(9)       VALUE +09375576.
           12  FILLER              PIC S9(9)       VALUE +09348687.
           12  FILLER              PIC S9(9)       VALUE +09321408.
           12  FILLER              PIC S9(9)       VALUE +09293558.
           12  FILLER              PIC S9(9)       VALUE +09264955.
           12  FILLER              PIC S9(9)       VALUE +09235422.
           12  FILLER              PIC S9(9)       VALUE +09204786.
           12  FILLER              PIC S9(9)       VALUE +09172872.
           12  FILLER              PIC S9(9)       VALUE +09139512.
           12  FILLER              PIC S9(9)       VALUE +09104266.
           12  FILLER              PIC S9(9)       VALUE +09066975.
           12  FILLER              PIC S9(9)       VALUE +09027303.
           12  FILLER              PIC S9(9)       VALUE +08984742.
           12  FILLER              PIC S9(9)       VALUE +08938885.
           12  FILLER              PIC S9(9)       VALUE +08889339.
           12  FILLER              PIC S9(9)       VALUE +08835813.
           12  FILLER              PIC S9(9)       VALUE +08777940.
           12  FILLER              PIC S9(9)       VALUE +08715547.
           12  FILLER              PIC S9(9)       VALUE +08648473.
           12  FILLER              PIC S9(9)       VALUE +08576659.
           12  FILLER              PIC S9(9)       VALUE +08499890.
           12  FILLER              PIC S9(9)       VALUE +08418053.
           12  FILLER              PIC S9(9)       VALUE +08330886.
           12  FILLER              PIC S9(9)       VALUE +08237908.
           12  FILLER              PIC S9(9)       VALUE +08138429.
           12  FILLER              PIC S9(9)       VALUE +08031977.
           12  FILLER              PIC S9(9)       VALUE +07917817.
           12  FILLER              PIC S9(9)       VALUE +07795448.
           12  FILLER              PIC S9(9)       VALUE +07664367.
           12  FILLER              PIC S9(9)       VALUE +07524311.
           12  FILLER              PIC S9(9)       VALUE +07375324.
           12  FILLER              PIC S9(9)       VALUE +07217448.
           12  FILLER              PIC S9(9)       VALUE +07050727.
           12  FILLER              PIC S9(9)       VALUE +06875083.
           12  FILLER              PIC S9(9)       VALUE +06690142.
           12  FILLER              PIC S9(9)       VALUE +06495225.
           12  FILLER              PIC S9(9)       VALUE +06289498.
           12  FILLER              PIC S9(9)       VALUE +06072170.
           12  FILLER              PIC S9(9)       VALUE +05842617.
           12  FILLER              PIC S9(9)       VALUE +05600779.
           12  FILLER              PIC S9(9)       VALUE +05347245.
           12  FILLER              PIC S9(9)       VALUE +05083037.
           12  FILLER              PIC S9(9)       VALUE +04809121.
           12  FILLER              PIC S9(9)       VALUE +04526586.
           12  FILLER              PIC S9(9)       VALUE +04236296.
           12  FILLER              PIC S9(9)       VALUE +03938709.
           12  FILLER              PIC S9(9)       VALUE +03634569.
           12  FILLER              PIC S9(9)       VALUE +03325376.
           12  FILLER              PIC S9(9)       VALUE +03013437.
           12  FILLER              PIC S9(9)       VALUE +02702052.
           12  FILLER              PIC S9(9)       VALUE +02395308.
           12  FILLER              PIC S9(9)       VALUE +02097479.
           12  FILLER              PIC S9(9)       VALUE +01812425.
           12  FILLER              PIC S9(9)       VALUE +01543456.
           12  FILLER              PIC S9(9)       VALUE +01293266.
           12  FILLER              PIC S9(9)       VALUE +01063943.
           12  FILLER              PIC S9(9)       VALUE +00857341.
           12  FILLER              PIC S9(9)       VALUE +00675090.
           12  FILLER              PIC S9(9)       VALUE +00518226.
           12  FILLER              PIC S9(9)       VALUE +00386985.
           12  FILLER              PIC S9(9)       VALUE +00280568.
           12  FILLER              PIC S9(9)       VALUE +00197122.
           12  FILLER              PIC S9(9)       VALUE +00133947.
           12  FILLER              PIC S9(9)       VALUE +00087828.
           12  FILLER              PIC S9(9)       VALUE +00055409.
           12  FILLER              PIC S9(9)       VALUE +00033494.
           12  FILLER              PIC S9(9)       VALUE +00019263.
           12  FILLER              PIC S9(9)       VALUE +00010399.
           12  FILLER              PIC S9(9)       VALUE +00005131.
           12  FILLER              PIC S9(9)       VALUE +00002188.
           12  FILLER              PIC S9(9)       VALUE +00000708.
           12  FILLER              PIC S9(9)       VALUE +00000119.

      *** AHL TABLE1  58CSO ALB JOINT.
       01  AHL-USER-TABLE1     COMP-3.
           12  FILLER              PIC S9(9)       VALUE +09964600.
           12  FILLER              PIC S9(9)       VALUE +09881360.
           12  FILLER              PIC S9(9)       VALUE +09849963.
           12  FILLER              PIC S9(9)       VALUE +09821713.
           12  FILLER              PIC S9(9)       VALUE +09794722.
           12  FILLER              PIC S9(9)       VALUE +09768881.
           12  FILLER              PIC S9(9)       VALUE +09744084.
           12  FILLER              PIC S9(9)       VALUE +09720226.
           12  FILLER              PIC S9(9)       VALUE +09697105.
           12  FILLER              PIC S9(9)       VALUE +09674476.
           12  FILLER              PIC S9(9)       VALUE +09652044.
           12  FILLER              PIC S9(9)       VALUE +09629519.
           12  FILLER              PIC S9(9)       VALUE +09606663.
           12  FILLER              PIC S9(9)       VALUE +09583189.
           12  FILLER              PIC S9(9)       VALUE +09558815.
           12  FILLER              PIC S9(9)       VALUE +09533405.
           12  FILLER              PIC S9(9)       VALUE +09506826.
           12  FILLER              PIC S9(9)       VALUE +09478944.
           12  FILLER              PIC S9(9)       VALUE +09449771.
           12  FILLER              PIC S9(9)       VALUE +09419414.
           12  FILLER              PIC S9(9)       VALUE +09387932.
           12  FILLER              PIC S9(9)       VALUE +09355431.
           12  FILLER              PIC S9(9)       VALUE +09322156.
           12  FILLER              PIC S9(9)       VALUE +09288254.
           12  FILLER              PIC S9(9)       VALUE +09253826.
           12  FILLER              PIC S9(9)       VALUE +09219018.
           12  FILLER              PIC S9(9)       VALUE +09183835.
           12  FILLER              PIC S9(9)       VALUE +09148283.
           12  FILLER              PIC S9(9)       VALUE +09112365.
           12  FILLER              PIC S9(9)       VALUE +09075952.
           12  FILLER              PIC S9(9)       VALUE +09038960.
           12  FILLER              PIC S9(9)       VALUE +09001307.
           12  FILLER              PIC S9(9)       VALUE +08962867.
           12  FILLER              PIC S9(9)       VALUE +08923563.
           12  FILLER              PIC S9(9)       VALUE +08883274.
           12  FILLER              PIC S9(9)       VALUE +08841793.
           12  FILLER              PIC S9(9)       VALUE +08798874.
           12  FILLER              PIC S9(9)       VALUE +08754232.
           12  FILLER              PIC S9(9)       VALUE +08707372.
           12  FILLER              PIC S9(9)       VALUE +08657767.
           12  FILLER              PIC S9(9)       VALUE +08604948.
           12  FILLER              PIC S9(9)       VALUE +08548333.
           12  FILLER              PIC S9(9)       VALUE +08487448.
           12  FILLER              PIC S9(9)       VALUE +08421880.
           12  FILLER              PIC S9(9)       VALUE +08351207.
           12  FILLER              PIC S9(9)       VALUE +08275049.
           12  FILLER              PIC S9(9)       VALUE +08193005.
           12  FILLER              PIC S9(9)       VALUE +08104608.
           12  FILLER              PIC S9(9)       VALUE +08009340.
           12  FILLER              PIC S9(9)       VALUE +07906642.
           12  FILLER              PIC S9(9)       VALUE +07795913.
           12  FILLER              PIC S9(9)       VALUE +07676565.
           12  FILLER              PIC S9(9)       VALUE +07548078.
           12  FILLER              PIC S9(9)       VALUE +07409955.
           12  FILLER              PIC S9(9)       VALUE +07261713.
           12  FILLER              PIC S9(9)       VALUE +07102963.
           12  FILLER              PIC S9(9)       VALUE +06933328.
           12  FILLER              PIC S9(9)       VALUE +06752419.
           12  FILLER              PIC S9(9)       VALUE +06559918.
           12  FILLER              PIC S9(9)       VALUE +06355595.
           12  FILLER              PIC S9(9)       VALUE +06139266.
           12  FILLER              PIC S9(9)       VALUE +05910898.
           12  FILLER              PIC S9(9)       VALUE +05670696.
           12  FILLER              PIC S9(9)       VALUE +05418983.
           12  FILLER              PIC S9(9)       VALUE +05156289.
           12  FILLER              PIC S9(9)       VALUE +04883359.
           12  FILLER              PIC S9(9)       VALUE +04601090.
           12  FILLER              PIC S9(9)       VALUE +04310583.
           12  FILLER              PIC S9(9)       VALUE +04013195.
           12  FILLER              PIC S9(9)       VALUE +03710698.
           12  FILLER              PIC S9(9)       VALUE +03405385.
           12  FILLER              PIC S9(9)       VALUE +03100019.
           12  FILLER              PIC S9(9)       VALUE +02797818.
           12  FILLER              PIC S9(9)       VALUE +02502271.
           12  FILLER              PIC S9(9)       VALUE +02216715.
           12  FILLER              PIC S9(9)       VALUE +01944037.
           12  FILLER              PIC S9(9)       VALUE +01686624.
           12  FILLER              PIC S9(9)       VALUE +01446252.
           12  FILLER              PIC S9(9)       VALUE +01224158.
           12  FILLER              PIC S9(9)       VALUE +01021305.
           12  FILLER              PIC S9(9)       VALUE +00838506.
           12  FILLER              PIC S9(9)       VALUE +00676337.
           12  FILLER              PIC S9(9)       VALUE +00535077.
           12  FILLER              PIC S9(9)       VALUE +00414596.
           12  FILLER              PIC S9(9)       VALUE +00314188.
           12  FILLER              PIC S9(9)       VALUE +00232548.
           12  FILLER              PIC S9(9)       VALUE +00167877.
           12  FILLER              PIC S9(9)       VALUE +00118026.
           12  FILLER              PIC S9(9)       VALUE +00080670.
           12  FILLER              PIC S9(9)       VALUE +00053485.
           12  FILLER              PIC S9(9)       VALUE +00034304.
           12  FILLER              PIC S9(9)       VALUE +00021207.
           12  FILLER              PIC S9(9)       VALUE +00012577.
           12  FILLER              PIC S9(9)       VALUE +00007110.
           12  FILLER              PIC S9(9)       VALUE +00003800.
           12  FILLER              PIC S9(9)       VALUE +00001896.
           12  FILLER              PIC S9(9)       VALUE +00000864.
           12  FILLER              PIC S9(9)       VALUE +00000342.
           12  FILLER              PIC S9(9)       VALUE +00000103.
           12  FILLER              PIC S9(9)       VALUE +00000016.


      *** AHL TABLE2  80CSO ALB JOINT.
       01  AHL-USER-TABLE2     COMP-3.
           12  FILLER              PIC S9(9)       VALUE +09964600.
           12  FILLER              PIC S9(9)       VALUE +09919709.
           12  FILLER              PIC S9(9)       VALUE +09901168.
           12  FILLER              PIC S9(9)       VALUE +09883452.
           12  FILLER              PIC S9(9)       VALUE +09866164.
           12  FILLER              PIC S9(9)       VALUE +09849398.
           12  FILLER              PIC S9(9)       VALUE +09833350.
           12  FILLER              PIC S9(9)       VALUE +09818016.
           12  FILLER              PIC S9(9)       VALUE +09803393.
           12  FILLER              PIC S9(9)       VALUE +09789183.
           12  FILLER              PIC S9(9)       VALUE +09775190.
           12  FILLER              PIC S9(9)       VALUE +09761216.
           12  FILLER              PIC S9(9)       VALUE +09746482.
           12  FILLER              PIC S9(9)       VALUE +09730407.
           12  FILLER              PIC S9(9)       VALUE +09712511.
           12  FILLER              PIC S9(9)       VALUE +09692513.
           12  FILLER              PIC S9(9)       VALUE +09670329.
           12  FILLER              PIC S9(9)       VALUE +09646071.
           12  FILLER              PIC S9(9)       VALUE +09620235.
           12  FILLER              PIC S9(9)       VALUE +09593124.
           12  FILLER              PIC S9(9)       VALUE +09565226.
           12  FILLER              PIC S9(9)       VALUE +09536933.
           12  FILLER              PIC S9(9)       VALUE +09508532.
           12  FILLER              PIC S9(9)       VALUE +09480216.
           12  FILLER              PIC S9(9)       VALUE +09452174.
           12  FILLER              PIC S9(9)       VALUE +09424310.
           12  FILLER              PIC S9(9)       VALUE +09396810.
           12  FILLER              PIC S9(9)       VALUE +09369391.
           12  FILLER              PIC S9(9)       VALUE +09341771.
           12  FILLER              PIC S9(9)       VALUE +09313953.
           12  FILLER              PIC S9(9)       VALUE +09285660.
           12  FILLER              PIC S9(9)       VALUE +09256711.
           12  FILLER              PIC S9(9)       VALUE +09226928.
           12  FILLER              PIC S9(9)       VALUE +09196135.
           12  FILLER              PIC S9(9)       VALUE +09164068.
           12  FILLER              PIC S9(9)       VALUE +09130558.
           12  FILLER              PIC S9(9)       VALUE +09095257.
           12  FILLER              PIC S9(9)       VALUE +09057641.
           12  FILLER              PIC S9(9)       VALUE +09017378.
           12  FILLER              PIC S9(9)       VALUE +08974056.
           12  FILLER              PIC S9(9)       VALUE +08927272.
           12  FILLER              PIC S9(9)       VALUE +08876636.
           12  FILLER              PIC S9(9)       VALUE +08821951.
           12  FILLER              PIC S9(9)       VALUE +08763030.
           12  FILLER              PIC S9(9)       VALUE +08699786.
           12  FILLER              PIC S9(9)       VALUE +08631971.
           12  FILLER              PIC S9(9)       VALUE +08559527.
           12  FILLER              PIC S9(9)       VALUE +08482320.
           12  FILLER              PIC S9(9)       VALUE +08400069.
           12  FILLER              PIC S9(9)       VALUE +08312513.
           12  FILLER              PIC S9(9)       VALUE +08219254.
           12  FILLER              PIC S9(9)       VALUE +08119850.
           12  FILLER              PIC S9(9)       VALUE +08013577.
           12  FILLER              PIC S9(9)       VALUE +07899779.
           12  FILLER              PIC S9(9)       VALUE +07777713.
           12  FILLER              PIC S9(9)       VALUE +07647114.
           12  FILLER              PIC S9(9)       VALUE +07507863.
           12  FILLER              PIC S9(9)       VALUE +07360133.
           12  FILLER              PIC S9(9)       VALUE +07204228.
           12  FILLER              PIC S9(9)       VALUE +07040284.
           12  FILLER              PIC S9(9)       VALUE +06867951.
           12  FILLER              PIC S9(9)       VALUE +06686394.
           12  FILLER              PIC S9(9)       VALUE +06494451.
           12  FILLER              PIC S9(9)       VALUE +06290663.
           12  FILLER              PIC S9(9)       VALUE +06074006.
           12  FILLER              PIC S9(9)       VALUE +05844092.
           12  FILLER              PIC S9(9)       VALUE +05601545.
           12  FILLER              PIC S9(9)       VALUE +05347497.
           12  FILLER              PIC S9(9)       VALUE +05083632.
           12  FILLER              PIC S9(9)       VALUE +04811346.
           12  FILLER              PIC S9(9)       VALUE +04531146.
           12  FILLER              PIC S9(9)       VALUE +04243093.
           12  FILLER              PIC S9(9)       VALUE +03946924.
           12  FILLER              PIC S9(9)       VALUE +03642521.
           12  FILLER              PIC S9(9)       VALUE +03330868.
           12  FILLER              PIC S9(9)       VALUE +03014611.
           12  FILLER              PIC S9(9)       VALUE +02697829.
           12  FILLER              PIC S9(9)       VALUE +02385419.
           12  FILLER              PIC S9(9)       VALUE +02082526.
           12  FILLER              PIC S9(9)       VALUE +01793634.
           12  FILLER              PIC S9(9)       VALUE +01522076.
           12  FILLER              PIC S9(9)       VALUE +01270266.
           12  FILLER              PIC S9(9)       VALUE +01040099.
           12  FILLER              PIC S9(9)       VALUE +00833141.
           12  FILLER              PIC S9(9)       VALUE +00650985.
           12  FILLER              PIC S9(9)       VALUE +00494929.
           12  FILLER              PIC S9(9)       VALUE +00365343.
           12  FILLER              PIC S9(9)       VALUE +00261352.
           12  FILLER              PIC S9(9)       VALUE +00180851.
           12  FILLER              PIC S9(9)       VALUE +00120824.
           12  FILLER              PIC S9(9)       VALUE +00077758.
           12  FILLER              PIC S9(9)       VALUE +00048066.
           12  FILLER              PIC S9(9)       VALUE +00028424.
           12  FILLER              PIC S9(9)       VALUE +00015982.
           12  FILLER              PIC S9(9)       VALUE +00008451.
           12  FILLER              PIC S9(9)       VALUE +00004110.
           12  FILLER              PIC S9(9)       VALUE +00001755.
           12  FILLER              PIC S9(9)       VALUE +00000596.
           12  FILLER              PIC S9(9)       VALUE +00000127.
           12  FILLER              PIC S9(9)       VALUE +00000008.

00332 *** USER-TABLE1 58CET ALB SINGLE LIFE INT 3.5%.                   ECS096
00333  01  HER-USER-TABLE1     COMP-3.                                  ECS096
00334      12  FILLER              PIC S9(9)       VALUE +09960850.     ECS096
00335      12  FILLER              PIC S9(9)       VALUE +09909248.     ECS096
00336      12  FILLER              PIC S9(9)       VALUE +09885564.     ECS096
00337      12  FILLER              PIC S9(9)       VALUE +09863420.     ECS096
00338      12  FILLER              PIC S9(9)       VALUE +09841918.     ECS096
00339      12  FILLER              PIC S9(9)       VALUE +09821003.     ECS096
00340      12  FILLER              PIC S9(9)       VALUE +09800624.     ECS096
00341      12  FILLER              PIC S9(9)       VALUE +09780728.     ECS096
00342      12  FILLER              PIC S9(9)       VALUE +09761216.     ECS096
00343      12  FILLER              PIC S9(9)       VALUE +09741986.     ECS096
00344      12  FILLER              PIC S9(9)       VALUE +09722892.     ECS096
00345      12  FILLER              PIC S9(9)       VALUE +09703738.     ECS096
00346      12  FILLER              PIC S9(9)       VALUE +09684380.     ECS096
00347      12  FILLER              PIC S9(9)       VALUE +09664624.     ECS096
00348      12  FILLER              PIC S9(9)       VALUE +09644280.     ECS096
00349      12  FILLER              PIC S9(9)       VALUE +09623304.     ECS096
00350      12  FILLER              PIC S9(9)       VALUE +09601652.     ECS096
00351      12  FILLER              PIC S9(9)       VALUE +09579281.     ECS096
00352      12  FILLER              PIC S9(9)       VALUE +09556243.     ECS096
00353      12  FILLER              PIC S9(9)       VALUE +09532687.     ECS096
00354      12  FILLER              PIC S9(9)       VALUE +09508712.     ECS096
00355      12  FILLER              PIC S9(9)       VALUE +09484370.     ECS096
00356      12  FILLER              PIC S9(9)       VALUE +09459759.     ECS096
00357      12  FILLER              PIC S9(9)       VALUE +09434928.     ECS096
00358      12  FILLER              PIC S9(9)       VALUE +09409925.     ECS096
00359      12  FILLER              PIC S9(9)       VALUE +09384800.     ECS096
00360      12  FILLER              PIC S9(9)       VALUE +09359508.     ECS096
00361      12  FILLER              PIC S9(9)       VALUE +09334004.     ECS096
00362      12  FILLER              PIC S9(9)       VALUE +09308242.     ECS096
00363      12  FILLER              PIC S9(9)       VALUE +09282133.     ECS096
00364      12  FILLER              PIC S9(9)       VALUE +09255632.     ECS096
00365      12  FILLER              PIC S9(9)       VALUE +09228699.     ECS096
00366      12  FILLER              PIC S9(9)       VALUE +09201290.     ECS096
00367      12  FILLER              PIC S9(9)       VALUE +09173365.     ECS096
00368      12  FILLER              PIC S9(9)       VALUE +09144836.     ECS096
00369      12  FILLER              PIC S9(9)       VALUE +09115528.     ECS096
00370      12  FILLER              PIC S9(9)       VALUE +09085038.     ECS096
00371      12  FILLER              PIC S9(9)       VALUE +09052924.     ECS096
00372      12  FILLER              PIC S9(9)       VALUE +09018751.     ECS096
00373      12  FILLER              PIC S9(9)       VALUE +08982048.     ECS096
00374      12  FILLER              PIC S9(9)       VALUE +08942440.     ECS096
00375      12  FILLER              PIC S9(9)       VALUE +08899610.     ECS096
00376      12  FILLER              PIC S9(9)       VALUE +08853292.     ECS096
00377      12  FILLER              PIC S9(9)       VALUE +08803233.     ECS096
00378      12  FILLER              PIC S9(9)       VALUE +08749144.     ECS096
00379      12  FILLER              PIC S9(9)       VALUE +08690708.     ECS096
00380      12  FILLER              PIC S9(9)       VALUE +08627535.     ECS096
00381      12  FILLER              PIC S9(9)       VALUE +08559173.     ECS096
00382      12  FILLER              PIC S9(9)       VALUE +08485107.     ECS096
00383      12  FILLER              PIC S9(9)       VALUE +08404854.     ECS096
00384      12  FILLER              PIC S9(9)       VALUE +08317884.     ECS096
00385      12  FILLER              PIC S9(9)       VALUE +08223665.     ECS096
00386      12  FILLER              PIC S9(9)       VALUE +08121760.     ECS096
00387      12  FILLER              PIC S9(9)       VALUE +08011702.     ECS096
00388      12  FILLER              PIC S9(9)       VALUE +07893046.     ECS096
00389      12  FILLER              PIC S9(9)       VALUE +07765340.     ECS096
00390      12  FILLER              PIC S9(9)       VALUE +07628062.     ECS096
00391      12  FILLER              PIC S9(9)       VALUE +07480634.     ECS096
00392      12  FILLER              PIC S9(9)       VALUE +07322492.     ECS096
00393      12  FILLER              PIC S9(9)       VALUE +07153170.     ECS096
00394      12  FILLER              PIC S9(9)       VALUE +06972259.     ECS096
00395      12  FILLER              PIC S9(9)       VALUE +06779417.     ECS096
00396      12  FILLER              PIC S9(9)       VALUE +06574440.     ECS096
00397      12  FILLER              PIC S9(9)       VALUE +06357178.     ECS096
00398      12  FILLER              PIC S9(9)       VALUE +06127577.     ECS096
00399      12  FILLER              PIC S9(9)       VALUE +05885654.     ECS096
00400      12  FILLER              PIC S9(9)       VALUE +05631517.     ECS096
00401      12  FILLER              PIC S9(9)       VALUE +05365398.     ECS096
00402      12  FILLER              PIC S9(9)       VALUE +05087711.     ECS096
00403      12  FILLER              PIC S9(9)       VALUE +04799422.     ECS096
00404      12  FILLER              PIC S9(9)       VALUE +04502208.     ECS096
00405      12  FILLER              PIC S9(9)       VALUE +04198444.     ECS096
00406      12  FILLER              PIC S9(9)       VALUE +03891041.     ECS096
00407      12  FILLER              PIC S9(9)       VALUE +03583157.     ECS096
00408      12  FILLER              PIC S9(9)       VALUE +03277641.     ECS096
00409      12  FILLER              PIC S9(9)       VALUE +02976714.     ECS096
00410      12  FILLER              PIC S9(9)       VALUE +02682121.     ECS096
00411      12  FILLER              PIC S9(9)       VALUE +02395295.     ECS096
00412      12  FILLER              PIC S9(9)       VALUE +02117650.     ECS096
00413      12  FILLER              PIC S9(9)       VALUE +01850986.     ECS096
00414      12  FILLER              PIC S9(9)       VALUE +01597664.     ECS096
00415      12  FILLER              PIC S9(9)       VALUE +01360258.     ECS096
00416      12  FILLER              PIC S9(9)       VALUE +01141251.     ECS096
00417      12  FILLER              PIC S9(9)       VALUE +00942734.     ECS096
00418      12  FILLER              PIC S9(9)       VALUE +00766055.     ECS096
00419      12  FILLER              PIC S9(9)       VALUE +00611723.     ECS096
00420      12  FILLER              PIC S9(9)       VALUE +00479476.     ECS096
00421      12  FILLER              PIC S9(9)       VALUE +00368402.     ECS096
00422      12  FILLER              PIC S9(9)       VALUE +00277026.     ECS096
00423      12  FILLER              PIC S9(9)       VALUE +00203448.     ECS096
00424      12  FILLER              PIC S9(9)       VALUE +00145514.     ECS096
00425      12  FILLER              PIC S9(9)       VALUE +00100980.     ECS096
00426      12  FILLER              PIC S9(9)       VALUE +00067646.     ECS096
00427      12  FILLER              PIC S9(9)       VALUE +00043447.     ECS096
00428      12  FILLER              PIC S9(9)       VALUE +00026514.     ECS096
00429      12  FILLER              PIC S9(9)       VALUE +00015158.     ECS096
00430      12  FILLER              PIC S9(9)       VALUE +00007894.     ECS096
00431      12  FILLER              PIC S9(9)       VALUE +00003491.     ECS096
00432      12  FILLER              PIC S9(9)       VALUE +00001056.     ECS096
00433      12  FILLER              PIC S9(9)       VALUE +00000122.     ECS096
00434                                                                   ECS096
00435                                  EJECT                            ECS096
00436 *** USER-TABLE2 58CET ALB JOINT  LIFE INT 3.5%.                   ECS096
00437  01  HER-USER-TABLE2     COMP-3.                                  ECS096
00438      12  FILLER              PIC S9(9)       VALUE +09960850.     ECS096
00439      12  FILLER              PIC S9(9)       VALUE +09909248.     ECS096
00440      12  FILLER              PIC S9(9)       VALUE +09885564.     ECS096
00441      12  FILLER              PIC S9(9)       VALUE +09863420.     ECS096
00442      12  FILLER              PIC S9(9)       VALUE +09790932.     ECS096
00443      12  FILLER              PIC S9(9)       VALUE +09746774.     ECS096
00444      12  FILLER              PIC S9(9)       VALUE +09704761.     ECS096
00445      12  FILLER              PIC S9(9)       VALUE +09663947.     ECS096
00446      12  FILLER              PIC S9(9)       VALUE +09624172.     ECS096
00447      12  FILLER              PIC S9(9)       VALUE +09585281.     ECS096
00448      12  FILLER              PIC S9(9)       VALUE +09547073.     ECS096
00449      12  FILLER              PIC S9(9)       VALUE +09509257.     ECS096
00450      12  FILLER              PIC S9(9)       VALUE +09471591.     ECS096
00451      12  FILLER              PIC S9(9)       VALUE +09433743.     ECS096
00452      12  FILLER              PIC S9(9)       VALUE +09395339.     ECS096
00453      12  FILLER              PIC S9(9)       VALUE +09356203.     ECS096
00454      12  FILLER              PIC S9(9)       VALUE +09316108.     ECS096
00455      12  FILLER              PIC S9(9)       VALUE +09274838.     ECS096
00456      12  FILLER              PIC S9(9)       VALUE +09232408.     ECS096
00457      12  FILLER              PIC S9(9)       VALUE +09188929.     ECS096
00458      12  FILLER              PIC S9(9)       VALUE +09144463.     ECS096
00459      12  FILLER              PIC S9(9)       VALUE +09099117.     ECS096
00460      12  FILLER              PIC S9(9)       VALUE +09053135.     ECS096
00461      12  FILLER              PIC S9(9)       VALUE +09006662.     ECS096
00462      12  FILLER              PIC S9(9)       VALUE +08959799.     ECS096
00463      12  FILLER              PIC S9(9)       VALUE +08912688.     ECS096
00464      12  FILLER              PIC S9(9)       VALUE +08865336.     ECS096
00465      12  FILLER              PIC S9(9)       VALUE +08817749.     ECS096
00466      12  FILLER              PIC S9(9)       VALUE +08769933.     ECS096
00467      12  FILLER              PIC S9(9)       VALUE +08721766.     ECS096
00468      12  FILLER              PIC S9(9)       VALUE +08673166.     ECS096
00469      12  FILLER              PIC S9(9)       VALUE +08624060.     ECS096
00470      12  FILLER              PIC S9(9)       VALUE +08574328.     ECS096
00471      12  FILLER              PIC S9(9)       VALUE +08523900.     ECS096
00472      12  FILLER              PIC S9(9)       VALUE +08472664.     ECS096
00473      12  FILLER              PIC S9(9)       VALUE +08420428.     ECS096
00474      12  FILLER              PIC S9(9)       VALUE +08366793.     ECS096
00475      12  FILLER              PIC S9(9)       VALUE +08311289.     ECS096
00476      12  FILLER              PIC S9(9)       VALUE +08253380.     ECS096
00477      12  FILLER              PIC S9(9)       VALUE +08192298.     ECS096
00478      12  FILLER              PIC S9(9)       VALUE +08127342.     ECS096
00479      12  FILLER              PIC S9(9)       VALUE +08057883.     ECS096
00480      12  FILLER              PIC S9(9)       VALUE +07983324.     ECS096
00481      12  FILLER              PIC S9(9)       VALUE +07903179.     ECS096
00482      12  FILLER              PIC S9(9)       VALUE +07817001.     ECS096
00483      12  FILLER              PIC S9(9)       VALUE +07724379.     ECS096
00484      12  FILLER              PIC S9(9)       VALUE +07624871.     ECS096
00485      12  FILLER              PIC S9(9)       VALUE +07517977.     ECS096
00486      12  FILLER              PIC S9(9)       VALUE +07403142.     ECS096
00487      12  FILLER              PIC S9(9)       VALUE +07279818.     ECS096
00488      12  FILLER              PIC S9(9)       VALUE +07147403.     ECS096
00489      12  FILLER              PIC S9(9)       VALUE +07005293.     ECS096
00490      12  FILLER              PIC S9(9)       VALUE +06853050.     ECS096
00491      12  FILLER              PIC S9(9)       VALUE +06690233.     ECS096
00492      12  FILLER              PIC S9(9)       VALUE +06516489.     ECS096
00493      12  FILLER              PIC S9(9)       VALUE +06331611.     ECS096
00494      12  FILLER              PIC S9(9)       VALUE +06135396.     ECS096
00495      12  FILLER              PIC S9(9)       VALUE +05927706.     ECS096
00496      12  FILLER              PIC S9(9)       VALUE +05708513.     ECS096
00497      12  FILLER              PIC S9(9)       VALUE +05477929.     ECS096
00498      12  FILLER              PIC S9(9)       VALUE +05236191.     ECS096
00499      12  FILLER              PIC S9(9)       VALUE +04983734.     ECS096
00500      12  FILLER              PIC S9(9)       VALUE +04721293.     ECS096
00501      12  FILLER              PIC S9(9)       VALUE +04449810.     ECS096
00502      12  FILLER              PIC S9(9)       VALUE +04170467.     ECS096
00503      12  FILLER              PIC S9(9)       VALUE +03884696.     ECS096
00504      12  FILLER              PIC S9(9)       VALUE +03594127.     ECS096
00505      12  FILLER              PIC S9(9)       VALUE +03300611.     ECS096
00506      12  FILLER              PIC S9(9)       VALUE +03006220.     ECS096
00507      12  FILLER              PIC S9(9)       VALUE +02713426.     ECS096
00508      12  FILLER              PIC S9(9)       VALUE +02425108.     ECS096
00509      12  FILLER              PIC S9(9)       VALUE +02144442.     ECS096
00510      12  FILLER              PIC S9(9)       VALUE +01874815.     ECS096
00511      12  FILLER              PIC S9(9)       VALUE +01619552.     ECS096
00512      12  FILLER              PIC S9(9)       VALUE +01381508.     ECS096
00513      12  FILLER              PIC S9(9)       VALUE +01162804.     ECS096
00514      12  FILLER              PIC S9(9)       VALUE +00964823.     ECS096
00515      12  FILLER              PIC S9(9)       VALUE +00788177.     ECS096
00516      12  FILLER              PIC S9(9)       VALUE +00632841.     ECS096
00517      12  FILLER              PIC S9(9)       VALUE +00498408.     ECS096
00518      12  FILLER              PIC S9(9)       VALUE +00384192.     ECS096
00519      12  FILLER              PIC S9(9)       VALUE +00289187.     ECS096
00520      12  FILLER              PIC S9(9)       VALUE +00212074.     ECS096
00521      12  FILLER              PIC S9(9)       VALUE +00151209.     ECS096
00522      12  FILLER              PIC S9(9)       VALUE +00104613.     ECS096
00523      12  FILLER              PIC S9(9)       VALUE +00070087.     ECS096
00524      12  FILLER              PIC S9(9)       VALUE +00045379.     ECS096
00525      12  FILLER              PIC S9(9)       VALUE +00028333.     ECS096
00526      12  FILLER              PIC S9(9)       VALUE +00017013.     ECS096
00527      12  FILLER              PIC S9(9)       VALUE +00009793.     ECS096
00528      12  FILLER              PIC S9(9)       VALUE +00005382.     ECS096
00529      12  FILLER              PIC S9(9)       VALUE +00002808.     ECS096
00530      12  FILLER              PIC S9(9)       VALUE +00001382.     ECS096
00531      12  FILLER              PIC S9(9)       VALUE +00000635.     ECS096
00532      12  FILLER              PIC S9(9)       VALUE +00000269.     ECS096
00533      12  FILLER              PIC S9(9)       VALUE +00000103.     ECS096
00534      12  FILLER              PIC S9(9)       VALUE +00000034.     ECS096
00535      12  FILLER              PIC S9(9)       VALUE +00000009.     ECS096
00536      12  FILLER              PIC S9(9)       VALUE +00000002.     ECS096
00537      12  FILLER              PIC S9(9)       VALUE +00000000.     ECS096
00538                                                                   ECS096
00539                                  EJECT                            ECS096
00540 *** USER-TABLE3 60CSG ALB SINGLE LIFE INT 3.5%.                   ECS096
00541  01  HER-USER-TABLE3     COMP-3.                                  ECS096
00542      12  FILLER              PIC S9(9)       VALUE +10000000.     ECS096
00543      12  FILLER              PIC S9(9)       VALUE +09948050.     ECS096
00544      12  FILLER              PIC S9(9)       VALUE +09928850.     ECS096
00545      12  FILLER              PIC S9(9)       VALUE +09911425.     ECS096
00546      12  FILLER              PIC S9(9)       VALUE +09894724.     ECS096
00547      12  FILLER              PIC S9(9)       VALUE +09878695.     ECS096
00548      12  FILLER              PIC S9(9)       VALUE +09863284.     ECS096
00549      12  FILLER              PIC S9(9)       VALUE +09848440.     ECS096
00550      12  FILLER              PIC S9(9)       VALUE +09834012.     ECS096
00551      12  FILLER              PIC S9(9)       VALUE +09819900.     ECS096
00552      12  FILLER              PIC S9(9)       VALUE +09805956.     ECS096
00553      12  FILLER              PIC S9(9)       VALUE +09791884.     ECS096
00554      12  FILLER              PIC S9(9)       VALUE +09777539.     ECS096
00555      12  FILLER              PIC S9(9)       VALUE +09762726.     ECS096
00556      12  FILLER              PIC S9(9)       VALUE +09747204.     ECS096
00557      12  FILLER              PIC S9(9)       VALUE +09730877.     ECS096
00558      12  FILLER              PIC S9(9)       VALUE +09713702.     ECS096
00559      12  FILLER              PIC S9(9)       VALUE +09695683.     ECS096
00560      12  FILLER              PIC S9(9)       VALUE +09676825.     ECS096
00561      12  FILLER              PIC S9(9)       VALUE +09657375.     ECS096
00562      12  FILLER              PIC S9(9)       VALUE +09637480.     ECS096
00563      12  FILLER              PIC S9(9)       VALUE +09617097.     ECS096
00564      12  FILLER              PIC S9(9)       VALUE +09596324.     ECS096
00565      12  FILLER              PIC S9(9)       VALUE +09575260.     ECS096
00566      12  FILLER              PIC S9(9)       VALUE +09553955.     ECS096
00567      12  FILLER              PIC S9(9)       VALUE +09532459.     ECS096
00568      12  FILLER              PIC S9(9)       VALUE +09510820.     ECS096
00569      12  FILLER              PIC S9(9)       VALUE +09489040.     ECS096
00570      12  FILLER              PIC S9(9)       VALUE +09467073.     ECS096
00571      12  FILLER              PIC S9(9)       VALUE +09444873.     ECS096
00572      12  FILLER              PIC S9(9)       VALUE +09422394.     ECS096
00573      12  FILLER              PIC S9(9)       VALUE +09399545.     ECS096
00574      12  FILLER              PIC S9(9)       VALUE +09376234.     ECS096
00575      12  FILLER              PIC S9(9)       VALUE +09352278.     ECS096
00576      12  FILLER              PIC S9(9)       VALUE +09327447.     ECS096
00577      12  FILLER              PIC S9(9)       VALUE +09301517.     ECS096
00578      12  FILLER              PIC S9(9)       VALUE +09274217.     ECS096
00579      12  FILLER              PIC S9(9)       VALUE +09245328.     ECS096
00580      12  FILLER              PIC S9(9)       VALUE +09214541.     ECS096
00581      12  FILLER              PIC S9(9)       VALUE +09181507.     ECS096
00582      12  FILLER              PIC S9(9)       VALUE +09145974.     ECS096
00583      12  FILLER              PIC S9(9)       VALUE +09107607.     ECS096
00584      12  FILLER              PIC S9(9)       VALUE +09066076.     ECS096
00585      12  FILLER              PIC S9(9)       VALUE +09021063.     ECS096
00586      12  FILLER              PIC S9(9)       VALUE +08972259.     ECS096
00587      12  FILLER              PIC S9(9)       VALUE +08919368.     ECS096
00588      12  FILLER              PIC S9(9)       VALUE +08862061.     ECS096
00589      12  FILLER              PIC S9(9)       VALUE +08799982.     ECS096
00590      12  FILLER              PIC S9(9)       VALUE +08732706.     ECS096
00591      12  FILLER              PIC S9(9)       VALUE +08659788.     ECS096
00592      12  FILLER              PIC S9(9)       VALUE +08580811.     ECS096
00593      12  FILLER              PIC S9(9)       VALUE +08495346.     ECS096
00594      12  FILLER              PIC S9(9)       VALUE +08402874.     ECS096
00595      12  FILLER              PIC S9(9)       VALUE +08302838.     ECS096
00596      12  FILLER              PIC S9(9)       VALUE +08194694.     ECS096
00597      12  FILLER              PIC S9(9)       VALUE +08077960.     ECS096
00598      12  FILLER              PIC S9(9)       VALUE +07952267.     ECS096
00599      12  FILLER              PIC S9(9)       VALUE +07817317.     ECS096
00600      12  FILLER              PIC S9(9)       VALUE +07672931.     ECS096
00601      12  FILLER              PIC S9(9)       VALUE +07519051.     ECS096
00602      12  FILLER              PIC S9(9)       VALUE +07355549.     ECS096
00603      12  FILLER              PIC S9(9)       VALUE +07182215.     ECS096
00604      12  FILLER              PIC S9(9)       VALUE +06998674.     ECS096
00605      12  FILLER              PIC S9(9)       VALUE +06804601.     ECS096
00606      12  FILLER              PIC S9(9)       VALUE +06599884.     ECS096
00607      12  FILLER              PIC S9(9)       VALUE +06384365.     ECS096
00608      12  FILLER              PIC S9(9)       VALUE +06157720.     ECS096
00609      12  FILLER              PIC S9(9)       VALUE +05919663.     ECS096
00610      12  FILLER              PIC S9(9)       VALUE +05670060.     ECS096
00611      12  FILLER              PIC S9(9)       VALUE +05409124.     ECS096
00612      12  FILLER              PIC S9(9)       VALUE +05137694.     ECS096
00613      12  FILLER              PIC S9(9)       VALUE +04857202.     ECS096
00614      12  FILLER              PIC S9(9)       VALUE +04569558.     ECS096
00615      12  FILLER              PIC S9(9)       VALUE +04277106.     ECS096
00616      12  FILLER              PIC S9(9)       VALUE +03982093.     ECS096
00617      12  FILLER              PIC S9(9)       VALUE +03686283.     ECS096
00618      12  FILLER              PIC S9(9)       VALUE +03391049.     ECS096
00619      12  FILLER              PIC S9(9)       VALUE +03097520.     ECS096
00620      12  FILLER              PIC S9(9)       VALUE +02806833.     ECS096
00621      12  FILLER              PIC S9(9)       VALUE +02520592.     ECS096
00622      12  FILLER              PIC S9(9)       VALUE +02241147.     ECS096
00623      12  FILLER              PIC S9(9)       VALUE +01971313.     ECS096
00624      12  FILLER              PIC S9(9)       VALUE +01714106.     ECS096
00625      12  FILLER              PIC S9(9)       VALUE +01472434.     ECS096
00626      12  FILLER              PIC S9(9)       VALUE +01248727.     ECS096
00627      12  FILLER              PIC S9(9)       VALUE +01044741.     ECS096
00628      12  FILLER              PIC S9(9)       VALUE +00861567.     ECS096
00629      12  FILLER              PIC S9(9)       VALUE +00699657.     ECS096
00630      12  FILLER              PIC S9(9)       VALUE +00558833.     ECS096
00631      12  FILLER              PIC S9(9)       VALUE +00438338.     ECS096
00632      12  FILLER              PIC S9(9)       VALUE +00336944.     ECS096
00633      12  FILLER              PIC S9(9)       VALUE +00253110.     ECS096
00634      12  FILLER              PIC S9(9)       VALUE +00185114.     ECS096
00635      12  FILLER              PIC S9(9)       VALUE +00131154.     ECS096
00636      12  FILLER              PIC S9(9)       VALUE +00089431.     ECS096
00637      12  FILLER              PIC S9(9)       VALUE +00058072.     ECS096
00638      12  FILLER              PIC S9(9)       VALUE +00035151.     ECS096
00639      12  FILLER              PIC S9(9)       VALUE +00018746.     ECS096
00640      12  FILLER              PIC S9(9)       VALUE +00007363.     ECS096
00641      12  FILLER              PIC S9(9)       VALUE +00001099.     ECS096
00642                                                                   ECS096
00643                                  EJECT                            ECS096
00644 *** USER-TABLE4 60CSG ALB JOINT  LIFE INT 3.5%.                   ECS096
00645  01  HER-USER-TABLE4     COMP-3.                                  ECS096
00646      12  FILLER              PIC S9(9)       VALUE +10000000.     ECS096
00647      12  FILLER              PIC S9(9)       VALUE +09948050.     ECS096
00648      12  FILLER              PIC S9(9)       VALUE +09928850.     ECS096
00649      12  FILLER              PIC S9(9)       VALUE +09911425.     ECS096
00650      12  FILLER              PIC S9(9)       VALUE +09843321.     ECS096
00651      12  FILLER              PIC S9(9)       VALUE +09808408.     ECS096
00652      12  FILLER              PIC S9(9)       VALUE +09775920.     ECS096
00653      12  FILLER              PIC S9(9)       VALUE +09744760.     ECS096
00654      12  FILLER              PIC S9(9)       VALUE +09714721.     ECS096
00655      12  FILLER              PIC S9(9)       VALUE +09685646.     ECS096
00656      12  FILLER              PIC S9(9)       VALUE +09657337.     ECS096
00657      12  FILLER              PIC S9(9)       VALUE +09629350.     ECS096
00658      12  FILLER              PIC S9(9)       VALUE +09601446.     ECS096
00659      12  FILLER              PIC S9(9)       VALUE +09573286.     ECS096
00660      12  FILLER              PIC S9(9)       VALUE +09544348.     ECS096
00661      12  FILLER              PIC S9(9)       VALUE +09514402.     ECS096
00662      12  FILLER              PIC S9(9)       VALUE +09483220.     ECS096
00663      12  FILLER              PIC S9(9)       VALUE +09450578.     ECS096
00664      12  FILLER              PIC S9(9)       VALUE +09416397.     ECS096
00665      12  FILLER              PIC S9(9)       VALUE +09380884.     ECS096
00666      12  FILLER              PIC S9(9)       VALUE +09344194.     ECS096
00667      12  FILLER              PIC S9(9)       VALUE +09306296.     ECS096
00668      12  FILLER              PIC S9(9)       VALUE +09267529.     ECS096
00669      12  FILLER              PIC S9(9)       VALUE +09228138.     ECS096
00670      12  FILLER              PIC S9(9)       VALUE +09188131.     ECS096
00671      12  FILLER              PIC S9(9)       VALUE +09147657.     ECS096
00672      12  FILLER              PIC S9(9)       VALUE +09106857.     ECS096
00673      12  FILLER              PIC S9(9)       VALUE +09065786.     ECS096
00674      12  FILLER              PIC S9(9)       VALUE +09024449.     ECS096
00675      12  FILLER              PIC S9(9)       VALUE +08982849.     ECS096
00676      12  FILLER              PIC S9(9)       VALUE +08940947.     ECS096
00677      12  FILLER              PIC S9(9)       VALUE +08898618.     ECS096
00678      12  FILLER              PIC S9(9)       VALUE +08855734.     ECS096
00679      12  FILLER              PIC S9(9)       VALUE +08812085.     ECS096
00680      12  FILLER              PIC S9(9)       VALUE +08767377.     ECS096
00681      12  FILLER              PIC S9(9)       VALUE +08721321.     ECS096
00682      12  FILLER              PIC S9(9)       VALUE +08673506.     ECS096
00683      12  FILLER              PIC S9(9)       VALUE +08623533.     ECS096
00684      12  FILLER              PIC S9(9)       VALUE +08570923.     ECS096
00685      12  FILLER              PIC S9(9)       VALUE +08515131.     ECS096
00686      12  FILLER              PIC S9(9)       VALUE +08455756.     ECS096
00687      12  FILLER              PIC S9(9)       VALUE +08392245.     ECS096
00688      12  FILLER              PIC S9(9)       VALUE +08324027.     ECS096
00689      12  FILLER              PIC S9(9)       VALUE +08250644.     ECS096
00690      12  FILLER              PIC S9(9)       VALUE +08171585.     ECS096
00691      12  FILLER              PIC S9(9)       VALUE +08086370.     ECS096
00692      12  FILLER              PIC S9(9)       VALUE +07994525.     ECS096
00693      12  FILLER              PIC S9(9)       VALUE +07895575.     ECS096
00694      12  FILLER              PIC S9(9)       VALUE +07789025.     ECS096
00695      12  FILLER              PIC S9(9)       VALUE +07674360.     ECS096
00696      12  FILLER              PIC S9(9)       VALUE +07551102.     ECS096
00697      12  FILLER              PIC S9(9)       VALUE +07418739.     ECS096
00698      12  FILLER              PIC S9(9)       VALUE +07276714.     ECS096
00699      12  FILLER              PIC S9(9)       VALUE +07124512.     ECS096
00700      12  FILLER              PIC S9(9)       VALUE +06961679.     ECS096
00701      12  FILLER              PIC S9(9)       VALUE +06787812.     ECS096
00702      12  FILLER              PIC S9(9)       VALUE +06602643.     ECS096
00703      12  FILLER              PIC S9(9)       VALUE +06406056.     ECS096
00704      12  FILLER              PIC S9(9)       VALUE +06198168.     ECS096
00705      12  FILLER              PIC S9(9)       VALUE +05979354.     ECS096
00706      12  FILLER              PIC S9(9)       VALUE +05750070.     ECS096
00707      12  FILLER              PIC S9(9)       VALUE +05510868.     ECS096
00708      12  FILLER              PIC S9(9)       VALUE +05262341.     ECS096
00709      12  FILLER              PIC S9(9)       VALUE +05005160.     ECS096
00710      12  FILLER              PIC S9(9)       VALUE +04740181.     ECS096
00711      12  FILLER              PIC S9(9)       VALUE +04468211.     ECS096
00712      12  FILLER              PIC S9(9)       VALUE +04190085.     ECS096
00713      12  FILLER              PIC S9(9)       VALUE +03906911.     ECS096
00714      12  FILLER              PIC S9(9)       VALUE +03619975.     ECS096
00715      12  FILLER              PIC S9(9)       VALUE +03330789.     ECS096
00716      12  FILLER              PIC S9(9)       VALUE +03041343.     ECS096
00717      12  FILLER              PIC S9(9)       VALUE +02754064.     ECS096
00718      12  FILLER              PIC S9(9)       VALUE +02471732.     ECS096
00719      12  FILLER              PIC S9(9)       VALUE +02197447.     ECS096
00720      12  FILLER              PIC S9(9)       VALUE +01934184.     ECS096
00721      12  FILLER              PIC S9(9)       VALUE +01684469.     ECS096
00722      12  FILLER              PIC S9(9)       VALUE +01450388.     ECS096
00723      12  FILLER              PIC S9(9)       VALUE +01233462.     ECS096
00724      12  FILLER              PIC S9(9)       VALUE +01034679.     ECS096
00725      12  FILLER              PIC S9(9)       VALUE +00854746.     ECS096
00726      12  FILLER              PIC S9(9)       VALUE +00694200.     ECS096
00727      12  FILLER              PIC S9(9)       VALUE +00553315.     ECS096
00728      12  FILLER              PIC S9(9)       VALUE +00432056.     ECS096
00729      12  FILLER              PIC S9(9)       VALUE +00329994.     ECS096
00730      12  FILLER              PIC S9(9)       VALUE +00246163.     ECS096
00731      12  FILLER              PIC S9(9)       VALUE +00179080.     ECS096
00732      12  FILLER              PIC S9(9)       VALUE +00126860.     ECS096
00733      12  FILLER              PIC S9(9)       VALUE +00087368.     ECS096
00734      12  FILLER              PIC S9(9)       VALUE +00058384.     ECS096
00735      12  FILLER              PIC S9(9)       VALUE +00037766.     ECS096
00736      12  FILLER              PIC S9(9)       VALUE +00023575.     ECS096
00737      12  FILLER              PIC S9(9)       VALUE +00014145.     ECS096
00738      12  FILLER              PIC S9(9)       VALUE +00008114.     ECS096
00739      12  FILLER              PIC S9(9)       VALUE +00004419.     ECS096
00740      12  FILLER              PIC S9(9)       VALUE +00002264.     ECS096
00741      12  FILLER              PIC S9(9)       VALUE +00001075.     ECS096
00742      12  FILLER              PIC S9(9)       VALUE +00000461.     ECS096
00743      12  FILLER              PIC S9(9)       VALUE +00000168.     ECS096
00744      12  FILLER              PIC S9(9)       VALUE +00000043.     ECS096
00745      12  FILLER              PIC S9(9)       VALUE +00000004.     ECS096
00746                                                                   ECS096
00747                                  EJECT                            ECS096
00748  01  HER-USER-TABLE      COMP-3.                                  ECS096
00749      12  FILLER              PIC S9(9)       VALUE +10000000.     ECS096
00750      12  FILLER              PIC S9(9)       VALUE +09844013.     ECS096
00751      12  FILLER              PIC S9(9)       VALUE +09794658.     ECS096
00752      12  FILLER              PIC S9(9)       VALUE +09750241.     ECS096
00753      12  FILLER              PIC S9(9)       VALUE +09707193.     ECS096
00754      12  FILLER              PIC S9(9)       VALUE +09665497.     ECS096
00755      12  FILLER              PIC S9(9)       VALUE +09624945.     ECS096
00756      12  FILLER              PIC S9(9)       VALUE +09585523.     ECS096
00757      12  FILLER              PIC S9(9)       VALUE +09547028.     ECS096
00758      12  FILLER              PIC S9(9)       VALUE +09509259.     ECS096
00759      12  FILLER              PIC S9(9)       VALUE +09472019.     ECS096
00760      12  FILLER              PIC S9(9)       VALUE +09434925.     ECS096
00761      12  FILLER              PIC S9(9)       VALUE +09397600.     ECS096
00762      12  FILLER              PIC S9(9)       VALUE +09359860.     ECS096
00763      12  FILLER              PIC S9(9)       VALUE +09321150.     ECS096
00764      12  FILLER              PIC S9(9)       VALUE +09281298.     ECS096
00765      12  FILLER              PIC S9(9)       VALUE +09240320.     ECS096
00766      12  FILLER              PIC S9(9)       VALUE +09198048.     ECS096
00767      12  FILLER              PIC S9(9)       VALUE +09154501.     ECS096
00768      12  FILLER              PIC S9(9)       VALUE +09109882.     ECS096
00769      12  FILLER              PIC S9(9)       VALUE +09064571.     ECS096
00770      12  FILLER              PIC S9(9)       VALUE +09018581.     ECS096
00771      12  FILLER              PIC S9(9)       VALUE +08972105.     ECS096
00772      12  FILLER              PIC S9(9)       VALUE +08925332.     ECS096
00773      12  FILLER              PIC S9(9)       VALUE +08878268.     ECS096
00774      12  FILLER              PIC S9(9)       VALUE +08831098.     ECS096
00775      12  FILLER              PIC S9(9)       VALUE +08783827.     ECS096
00776      12  FILLER              PIC S9(9)       VALUE +08736283.     ECS096
00777      12  FILLER              PIC S9(9)       VALUE +08688474.     ECS096
00778      12  FILLER              PIC S9(9)       VALUE +08640233.     ECS096
00779      12  FILLER              PIC S9(9)       VALUE +08591398.     ECS096
00780      12  FILLER              PIC S9(9)       VALUE +08541983.     ECS096
00781      12  FILLER              PIC S9(9)       VALUE +08491830.     ECS096
00782      12  FILLER              PIC S9(9)       VALUE +08440955.     ECS096
00783      12  FILLER              PIC S9(9)       VALUE +08389207.     ECS096
00784      12  FILLER              PIC S9(9)       VALUE +08336438.     ECS096
00785      12  FILLER              PIC S9(9)       VALUE +08282173.     ECS096
00786      12  FILLER              PIC S9(9)       VALUE +08225455.     ECS096
00787      12  FILLER              PIC S9(9)       VALUE +08165683.     ECS096
00788      12  FILLER              PIC S9(9)       VALUE +08101952.     ECS096
00789      12  FILLER              PIC S9(9)       VALUE +08033554.     ECS096
00790      12  FILLER              PIC S9(9)       VALUE +07959975.     ECS096
00791      12  FILLER              PIC S9(9)       VALUE +07880733.     ECS096
00792      12  FILLER              PIC S9(9)       VALUE +07795537.     ECS096
00793      12  FILLER              PIC S9(9)       VALUE +07703976.     ECS096
00794      12  FILLER              PIC S9(9)       VALUE +07605681.     ECS096
00795      12  FILLER              PIC S9(9)       VALUE +07500178.     ECS096
00796      12  FILLER              PIC S9(9)       VALUE +07386906.     ECS096
00797      12  FILLER              PIC S9(9)       VALUE +07265232.     ECS096
00798      12  FILLER              PIC S9(9)       VALUE +07134470.     ECS096
00799      12  FILLER              PIC S9(9)       VALUE +06994189.     ECS096
00800      12  FILLER              PIC S9(9)       VALUE +06843654.     ECS096
00801      12  FILLER              PIC S9(9)       VALUE +06682556.     ECS096
00802      12  FILLER              PIC S9(9)       VALUE +06510598.     ECS096
00803      12  FILLER              PIC S9(9)       VALUE +06327523.     ECS096
00804      12  FILLER              PIC S9(9)       VALUE +06133264.     ECS096
00805      12  FILLER              PIC S9(9)       VALUE +05927711.     ECS096
00806      12  FILLER              PIC S9(9)       VALUE +05710764.     ECS096
00807      12  FILLER              PIC S9(9)       VALUE +05482379.     ECS096
00808      12  FILLER              PIC S9(9)       VALUE +05242735.     ECS096
00809      12  FILLER              PIC S9(9)       VALUE +04992364.     ECS096
00810      12  FILLER              PIC S9(9)       VALUE +04731858.     ECS096
00811      12  FILLER              PIC S9(9)       VALUE +04462217.     ECS096
00812      12  FILLER              PIC S9(9)       VALUE +04184661.     ECS096
00813      12  FILLER              PIC S9(9)       VALUE +03900577.     ECS096
00814      12  FILLER              PIC S9(9)       VALUE +03611642.     ECS096
00815      12  FILLER              PIC S9(9)       VALUE +03319619.     ECS096
00816      12  FILLER              PIC S9(9)       VALUE +03026561.     ECS096
00817      12  FILLER              PIC S9(9)       VALUE +02734635.     ECS096
00818      12  FILLER              PIC S9(9)       VALUE +02446337.     ECS096
00819      12  FILLER              PIC S9(9)       VALUE +02164850.     ECS096
00820      12  FILLER              PIC S9(9)       VALUE +01893659.     ECS096
00821      12  FILLER              PIC S9(9)       VALUE +01636417.     ECS096
00822      12  FILLER              PIC S9(9)       VALUE +01396378.     ECS096
00823      12  FILLER              PIC S9(9)       VALUE +01176146.     ECS096
00824      12  FILLER              PIC S9(9)       VALUE +00977051.     ECS096
00825      12  FILLER              PIC S9(9)       VALUE +00799557.     ECS096
00826      12  FILLER              PIC S9(9)       VALUE +00643431.     ECS096
00827      12  FILLER              PIC S9(9)       VALUE +00508048.     ECS096
00828      12  FILLER              PIC S9(9)       VALUE +00392557.     ECS096
00829      12  FILLER              PIC S9(9)       VALUE +00296069.     ECS096
00830      12  FILLER              PIC S9(9)       VALUE +00217463.     ECS096
00831      12  FILLER              PIC S9(9)       VALUE +00155215.     ECS096
00832      12  FILLER              PIC S9(9)       VALUE +00107464.     ECS096
00833      12  FILLER              PIC S9(9)       VALUE +00072049.     ECS096
00834      12  FILLER              PIC S9(9)       VALUE +00046688.     ECS096
00835      12  FILLER              PIC S9(9)       VALUE +00029176.     ECS096
00836      12  FILLER              PIC S9(9)       VALUE +00017539.     ECS096
00837      12  FILLER              PIC S9(9)       VALUE +00010113.     ECS096
00838      12  FILLER              PIC S9(9)       VALUE +00005572.     ECS096
00839      12  FILLER              PIC S9(9)       VALUE +00002919.     ECS096
00840      12  FILLER              PIC S9(9)       VALUE +00001444.     ECS096
00841      12  FILLER              PIC S9(9)       VALUE +00000669.     ECS096
00842      12  FILLER              PIC S9(9)       VALUE +00000286.     ECS096
00843      12  FILLER              PIC S9(9)       VALUE +00000111.     ECS096
00844      12  FILLER              PIC S9(9)       VALUE +00000038.     ECS096
00845      12  FILLER              PIC S9(9)       VALUE +00000011.     ECS096
00846      12  FILLER              PIC S9(9)       VALUE +00000003.     ECS096
00847      12  FILLER              PIC S9(9)       VALUE +00000000.     ECS096
00848      12  FILLER              PIC S9(9)       VALUE +00000000.     ECS096
00849                                                                   ECS096
00850                                  EJECT                            ECS096
00851  01  NCL-USER-TABLE      COMP-3.                                  ECS096
00852      12  FILLER              PIC S9(9)       VALUE +09999999.     ECS096
00853      12  FILLER              PIC S9(9)       VALUE +09949179.     ECS096
00854      12  FILLER              PIC S9(9)       VALUE +09925600.     ECS096
00855      12  FILLER              PIC S9(9)       VALUE +09903584.     ECS096
00856      12  FILLER              PIC S9(9)       VALUE +09882212.     ECS096
00857      12  FILLER              PIC S9(9)       VALUE +09861380.     ECS096
00858      12  FILLER              PIC S9(9)       VALUE +09841106.     ECS096
00859      12  FILLER              PIC S9(9)       VALUE +09821344.     ECS096
00860      12  FILLER              PIC S9(9)       VALUE +09801996.     ECS096
00861      12  FILLER              PIC S9(9)       VALUE +09782902.     ECS096
00862      12  FILLER              PIC S9(9)       VALUE +09763924.     ECS096
00863      12  FILLER              PIC S9(9)       VALUE +09744884.     ECS096
00864      12  FILLER              PIC S9(9)       VALUE +09725707.     ECS096
00865      12  FILLER              PIC S9(9)       VALUE +09706118.     ECS096
00866      12  FILLER              PIC S9(9)       VALUE +09686046.     ECS096
00867      12  FILLER              PIC S9(9)       VALUE +09665397.     ECS096
00868      12  FILLER              PIC S9(9)       VALUE +09644054.     ECS096
00869      12  FILLER              PIC S9(9)       VALUE +09622029.     ECS096
00870      12  FILLER              PIC S9(9)       VALUE +09599378.     ECS096
00871      12  FILLER              PIC S9(9)       VALUE +09576166.     ECS096
00872      12  FILLER              PIC S9(9)       VALUE +09552399.     ECS096
00873      12  FILLER              PIC S9(9)       VALUE +09528251.     ECS096
00874      12  FILLER              PIC S9(9)       VALUE +09503820.     ECS096
00875      12  FILLER              PIC S9(9)       VALUE +09479110.     ECS096
00876      12  FILLER              PIC S9(9)       VALUE +09454162.     ECS096
00877      12  FILLER              PIC S9(9)       VALUE +09429070.     ECS096
00878      12  FILLER              PIC S9(9)       VALUE +09403840.     ECS096
00879      12  FILLER              PIC S9(9)       VALUE +09378392.     ECS096
00880      12  FILLER              PIC S9(9)       VALUE +09352675.     ECS096
00881      12  FILLER              PIC S9(9)       VALUE +09326695.     ECS096
00882      12  FILLER              PIC S9(9)       VALUE +09300280.     ECS096
00883      12  FILLER              PIC S9(9)       VALUE +09273498.     ECS096
00884      12  FILLER              PIC S9(9)       VALUE +09246269.     ECS096
00885      12  FILLER              PIC S9(9)       VALUE +09218566.     ECS096
00886      12  FILLER              PIC S9(9)       VALUE +09190267.     ECS096
00887      12  FILLER              PIC S9(9)       VALUE +09161280.     ECS096
00888      12  FILLER              PIC S9(9)       VALUE +09131252.     ECS096
00889      12  FILLER              PIC S9(9)       VALUE +09099782.     ECS096
00890      12  FILLER              PIC S9(9)       VALUE +09066514.     ECS096
00891      12  FILLER              PIC S9(9)       VALUE +09030938.     ECS096
00892      12  FILLER              PIC S9(9)       VALUE +08992699.     ECS096
00893      12  FILLER              PIC S9(9)       VALUE +08951462.     ECS096
00894      12  FILLER              PIC S9(9)       VALUE +08906934.     ECS096
00895      12  FILLER              PIC S9(9)       VALUE +08858823.     ECS096
00896      12  FILLER              PIC S9(9)       VALUE +08806816.     ECS096
00897      12  FILLER              PIC S9(9)       VALUE +08750593.     ECS096
00898      12  FILLER              PIC S9(9)       VALUE +08689813.     ECS096
00899      12  FILLER              PIC S9(9)       VALUE +08624089.     ECS096
00900      12  FILLER              PIC S9(9)       VALUE +08552884.     ECS096
00901      12  FILLER              PIC S9(9)       VALUE +08475716.     ECS096
00902      12  FILLER              PIC S9(9)       VALUE +08392114.     ECS096
00903      12  FILLER              PIC S9(9)       VALUE +08301532.     ECS096
00904      12  FILLER              PIC S9(9)       VALUE +08203539.     ECS096
00905      12  FILLER              PIC S9(9)       VALUE +08097636.     ECS096
00906      12  FILLER              PIC S9(9)       VALUE +07983340.     ECS096
00907      12  FILLER              PIC S9(9)       VALUE +07860224.     ECS096
00908      12  FILLER              PIC S9(9)       VALUE +07727752.     ECS096
00909      12  FILLER              PIC S9(9)       VALUE +07585351.     ECS096
00910      12  FILLER              PIC S9(9)       VALUE +07432532.     ECS096
00911      12  FILLER              PIC S9(9)       VALUE +07268775.     ECS096
00912      12  FILLER              PIC S9(9)       VALUE +07093604.     ECS096
00913      12  FILLER              PIC S9(9)       VALUE +06906653.     ECS096
00914      12  FILLER              PIC S9(9)       VALUE +06707630.     ECS096
00915      12  FILLER              PIC S9(9)       VALUE +06496349.     ECS096
00916      12  FILLER              PIC S9(9)       VALUE +06272695.     ECS096
00917      12  FILLER              PIC S9(9)       VALUE +06036646.     ECS096
00918      12  FILLER              PIC S9(9)       VALUE +05788206.     ECS096
00919      12  FILLER              PIC S9(9)       VALUE +05527554.     ECS096
00920      12  FILLER              PIC S9(9)       VALUE +05255023.     ECS096
00921      12  FILLER              PIC S9(9)       VALUE +04971432.     ECS096
00922      12  FILLER              PIC S9(9)       VALUE +04678143.     ECS096
00923      12  FILLER              PIC S9(9)       VALUE +04377211.     ECS096
00924      12  FILLER              PIC S9(9)       VALUE +04071219.     ECS096
00925      12  FILLER              PIC S9(9)       VALUE +03763054.     ECS096
00926      12  FILLER              PIC S9(9)       VALUE +03455618.     ECS096
00927      12  FILLER              PIC S9(9)       VALUE +03151196.     ECS096
00928      12  FILLER              PIC S9(9)       VALUE +02851840.     ECS096
00929      12  FILLER              PIC S9(9)       VALUE +02559211.     ECS096
00930      12  FILLER              PIC S9(9)       VALUE +02274929.     ECS096
00931      12  FILLER              PIC S9(9)       VALUE +02000735.     ECS096
00932      12  FILLER              PIC S9(9)       VALUE +01738867.     ECS096
00933      12  FILLER              PIC S9(9)       VALUE +01491838.     ECS096
00934      12  FILLER              PIC S9(9)       VALUE +01262131.     ECS096
00935      12  FILLER              PIC S9(9)       VALUE +01051930.     ECS096
00936      12  FILLER              PIC S9(9)       VALUE +00862978.     ECS096
00937      12  FILLER              PIC S9(9)       VALUE +00696078.     ECS096
00938      12  FILLER              PIC S9(9)       VALUE +00551488.     ECS096
00939      12  FILLER              PIC S9(9)       VALUE +00428615.     ECS096
00940      12  FILLER              PIC S9(9)       VALUE +00326281.     ECS096
00941      12  FILLER              PIC S9(9)       VALUE +00242877.     ECS096
00942      12  FILLER              PIC S9(9)       VALUE +00176262.     ECS096
00943      12  FILLER              PIC S9(9)       VALUE +00124351.     ECS096
00944      12  FILLER              PIC S9(9)       VALUE +00084910.     ECS096
00945      12  FILLER              PIC S9(9)       VALUE +00055757.     ECS096
00946      12  FILLER              PIC S9(9)       VALUE +00034960.     ECS096
00947      12  FILLER              PIC S9(9)       VALUE +00020689.     ECS096
00948      12  FILLER              PIC S9(9)       VALUE +00011289.     ECS096
00949      12  FILLER              PIC S9(9)       VALUE +00005362.     ECS096
00950      12  FILLER              PIC S9(9)       VALUE +00001943.     ECS096
00951      12  FILLER              PIC S9(9)       VALUE +00000390.     ECS096
00952                                                                   ECS096
00953                                  EJECT                            ECS096
00954 * SPECIAL USER MORTALITY TABLE                                    ECS096
00955 *                                                                 ECS096
00956 *      USE CODE XXXXX IN LX CARD TO SPECIFY                       ECS096
00957 *                                                                 ECS096
00958 *      IN THE TABLE BELOW, ENTER THE SPECIAL TABLE                ECS096
00959 *                                                                 ECS096
00960  01  FILLER                  PIC  X(16)  SYNC    VALUE            ECS096
00961          '***USER-TABLE***'.                                      ECS096
00962                                                                   ECS096
00963  01  USER-TABLE      COMP-3.                                      ECS096
00964      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
00965      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
00966      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
00967      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
00968      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
00969      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
00970      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
00971      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
00972      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
00973      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
00974      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
00975      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
00976      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
00977      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
00978      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
00979      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
00980      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
00981      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
00982      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
00983      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
00984      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
00985      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
00986      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
00987      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
00988      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
00989      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
00990      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
00991      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
00992      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
00993      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
00994      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
00995      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
00996      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
00997      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
00998      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
00999      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01000      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01001      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01002      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01003      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01004      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01005      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01006      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01007      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01008      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01009      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01010      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01011      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01012      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01013      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01014      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01015      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01016      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01017      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01018      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01019      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01020      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01021      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01022      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01023      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01024      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01025      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01026      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01027      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01028      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01029      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01030      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01031      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01032      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01033      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01034      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01035      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01036      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01037      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01038      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01039      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01040      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01041      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01042      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01043      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01044      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01045      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01046      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01047      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01048      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01049      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01050      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01051      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01052      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01053      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01054      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01055      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01056      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01057      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01058      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01059      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01060      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01061      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01062      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01063      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01064                                                                   ECS096
01065  01  USER-TABLE-R  REDEFINES  USER-TABLE.                         ECS096
01066      12  USER-OCCURS     OCCURS  100  TIMES  COMP-3.              ECS096
01067          16  USER-VALUE      PIC S9(9).                           ECS096
01068                                  EJECT                            ECS096
01069  01  FILLER                  PIC  X(16)  SYNC    VALUE            ECS096
01070          '***41CSO-TABLE**'.                                      ECS096
01071                                                                   ECS096
01072  01  41CSO-TABLE      COMP-3.                                     ECS096
01073      12  FILLER              PIC S9(9)       VALUE +01023102.     ECS096
01074      12  FILLER              PIC S9(9)       VALUE +01000000.     ECS096
01075      12  FILLER              PIC S9(9)       VALUE +00994230.     ECS096
01076      12  FILLER              PIC S9(9)       VALUE +00990114.     ECS096
01077      12  FILLER              PIC S9(9)       VALUE +00986767.     ECS096
01078      12  FILLER              PIC S9(9)       VALUE +00983817.     ECS096
01079      12  FILLER              PIC S9(9)       VALUE +00981102.     ECS096
01080      12  FILLER              PIC S9(9)       VALUE +00978541.     ECS096
01081      12  FILLER              PIC S9(9)       VALUE +00976124.     ECS096
01082      12  FILLER              PIC S9(9)       VALUE +00973869.     ECS096
01083      12  FILLER              PIC S9(9)       VALUE +00971804.     ECS096
01084      12  FILLER              PIC S9(9)       VALUE +00969890.     ECS096
01085      12  FILLER              PIC S9(9)       VALUE +00968038.     ECS096
01086      12  FILLER              PIC S9(9)       VALUE +00966179.     ECS096
01087      12  FILLER              PIC S9(9)       VALUE +00964266.     ECS096
01088      12  FILLER              PIC S9(9)       VALUE +00962270.     ECS096
01089      12  FILLER              PIC S9(9)       VALUE +00960201.     ECS096
01090      12  FILLER              PIC S9(9)       VALUE +00958098.     ECS096
01091      12  FILLER              PIC S9(9)       VALUE +00955942.     ECS096
01092      12  FILLER              PIC S9(9)       VALUE +00953743.     ECS096
01093      12  FILLER              PIC S9(9)       VALUE +00951483.     ECS096
01094      12  FILLER              PIC S9(9)       VALUE +00949171.     ECS096
01095      12  FILLER              PIC S9(9)       VALUE +00946789.     ECS096
01096      12  FILLER              PIC S9(9)       VALUE +00944337.     ECS096
01097      12  FILLER              PIC S9(9)       VALUE +00941806.     ECS096
01098      12  FILLER              PIC S9(9)       VALUE +00939197.     ECS096
01099      12  FILLER              PIC S9(9)       VALUE +00936492.     ECS096
01100      12  FILLER              PIC S9(9)       VALUE +00933692.     ECS096
01101      12  FILLER              PIC S9(9)       VALUE +00930788.     ECS096
01102      12  FILLER              PIC S9(9)       VALUE +00927763.     ECS096
01103      12  FILLER              PIC S9(9)       VALUE +00924609.     ECS096
01104      12  FILLER              PIC S9(9)       VALUE +00921317.     ECS096
01105      12  FILLER              PIC S9(9)       VALUE +00917880.     ECS096
01106      12  FILLER              PIC S9(9)       VALUE +00914282.     ECS096
01107      12  FILLER              PIC S9(9)       VALUE +00910515.     ECS096
01108      12  FILLER              PIC S9(9)       VALUE +00906554.     ECS096
01109      12  FILLER              PIC S9(9)       VALUE +00902393.     ECS096
01110      12  FILLER              PIC S9(9)       VALUE +00898007.     ECS096
01111      12  FILLER              PIC S9(9)       VALUE +00893382.     ECS096
01112      12  FILLER              PIC S9(9)       VALUE +00888504.     ECS096
01113      12  FILLER              PIC S9(9)       VALUE +00883342.     ECS096
01114      12  FILLER              PIC S9(9)       VALUE +00877883.     ECS096
01115      12  FILLER              PIC S9(9)       VALUE +00872098.     ECS096
01116      12  FILLER              PIC S9(9)       VALUE +00865967.     ECS096
01117      12  FILLER              PIC S9(9)       VALUE +00859464.     ECS096
01118      12  FILLER              PIC S9(9)       VALUE +00852554.     ECS096
01119      12  FILLER              PIC S9(9)       VALUE +00845214.     ECS096
01120      12  FILLER              PIC S9(9)       VALUE +00837413.     ECS096
01121      12  FILLER              PIC S9(9)       VALUE +00829114.     ECS096
01122      12  FILLER              PIC S9(9)       VALUE +00820292.     ECS096
01123      12  FILLER              PIC S9(9)       VALUE +00810900.     ECS096
01124      12  FILLER              PIC S9(9)       VALUE +00800910.     ECS096
01125      12  FILLER              PIC S9(9)       VALUE +00790282.     ECS096
01126      12  FILLER              PIC S9(9)       VALUE +00778981.     ECS096
01127      12  FILLER              PIC S9(9)       VALUE +00766961.     ECS096
01128      12  FILLER              PIC S9(9)       VALUE +00754191.     ECS096
01129      12  FILLER              PIC S9(9)       VALUE +00740631.     ECS096
01130      12  FILLER              PIC S9(9)       VALUE +00726241.     ECS096
01131      12  FILLER              PIC S9(9)       VALUE +00710990.     ECS096
01132      12  FILLER              PIC S9(9)       VALUE +00694843.     ECS096
01133      12  FILLER              PIC S9(9)       VALUE +00677771.     ECS096
01134      12  FILLER              PIC S9(9)       VALUE +00659749.     ECS096
01135      12  FILLER              PIC S9(9)       VALUE +00640761.     ECS096
01136      12  FILLER              PIC S9(9)       VALUE +00620782.     ECS096
01137      12  FILLER              PIC S9(9)       VALUE +00599824.     ECS096
01138      12  FILLER              PIC S9(9)       VALUE +00577882.     ECS096
01139      12  FILLER              PIC S9(9)       VALUE +00554975.     ECS096
01140      12  FILLER              PIC S9(9)       VALUE +00531133.     ECS096
01141      12  FILLER              PIC S9(9)       VALUE +00506403.     ECS096
01142      12  FILLER              PIC S9(9)       VALUE +00480850.     ECS096
01143      12  FILLER              PIC S9(9)       VALUE +00454548.     ECS096
01144      12  FILLER              PIC S9(9)       VALUE +00427593.     ECS096
01145      12  FILLER              PIC S9(9)       VALUE +00400112.     ECS096
01146      12  FILLER              PIC S9(9)       VALUE +00372240.     ECS096
01147      12  FILLER              PIC S9(9)       VALUE +00344136.     ECS096
01148      12  FILLER              PIC S9(9)       VALUE +00315982.     ECS096
01149      12  FILLER              PIC S9(9)       VALUE +00287973.     ECS096
01150      12  FILLER              PIC S9(9)       VALUE +00254322.     ECS096
01151      12  FILLER              PIC S9(9)       VALUE +00233251.     ECS096
01152      12  FILLER              PIC S9(9)       VALUE +00206989.     ECS096
01153      12  FILLER              PIC S9(9)       VALUE +00177765.     ECS096
01154      12  FILLER              PIC S9(9)       VALUE +00157799.     ECS096
01155      12  FILLER              PIC S9(9)       VALUE +00135297.     ECS096
01156      12  FILLER              PIC S9(9)       VALUE +00114440.     ECS096
01157      12  FILLER              PIC S9(9)       VALUE +00095378.     ECS096
01158      12  FILLER              PIC S9(9)       VALUE +00078221.     ECS096
01159      12  FILLER              PIC S9(9)       VALUE +00063036.     ECS096
01160      12  FILLER              PIC S9(9)       VALUE +00049838.     ECS096
01161      12  FILLER              PIC S9(9)       VALUE +00038593.     ECS096
01162      12  FILLER              PIC S9(9)       VALUE +00029215.     ECS096
01163      12  FILLER              PIC S9(9)       VALUE +00021577.     ECS096
01164      12  FILLER              PIC S9(9)       VALUE +00015514.     ECS096
01165      12  FILLER              PIC S9(9)       VALUE +00010833.     ECS096
01166      12  FILLER              PIC S9(9)       VALUE +00007327.     ECS096
01167      12  FILLER              PIC S9(9)       VALUE +00004787.     ECS096
01168      12  FILLER              PIC S9(9)       VALUE +00003251.     ECS096
01169      12  FILLER              PIC S9(9)       VALUE +00002008.     ECS096
01170      12  FILLER              PIC S9(9)       VALUE +00001005.     ECS096
01171      12  FILLER              PIC S9(9)       VALUE +00000454.     ECS096
01172      12  FILLER              PIC S9(9)       VALUE +00000125.     ECS096
01173                                                                   ECS096
01174  01  41CSO-TABLE-R  REDEFINES  41CSO-TABLE.                       ECS096
01175      12  41CSO-OCCURS    OCCURS  100  TIMES  COMP-3.              ECS096
01176          16  41CSO-VALUE     PIC S9(9).                           ECS096
01177                                  EJECT                            ECS096
01178  01  FILLER                  PIC  X(16)  SYNC    VALUE            ECS096
01179          '***58CET-TABLE**'.                                      ECS096
01180                                                                   ECS096
01181  01  58CET-TABLE     COMP-3.                                      ECS096
01182      12  FILLER              PIC S9(9)       VALUE +10000000.     ECS096
01183      12  FILLER              PIC S9(9)       VALUE +09921700.     ECS096
01184      12  FILLER              PIC S9(9)       VALUE +09896797.     ECS096
01185      12  FILLER              PIC S9(9)       VALUE +09874331.     ECS096
01186      12  FILLER              PIC S9(9)       VALUE +09852509.     ECS096
01187      12  FILLER              PIC S9(9)       VALUE +09831326.     ECS096
01188      12  FILLER              PIC S9(9)       VALUE +09810680.     ECS096
01189      12  FILLER              PIC S9(9)       VALUE +09790568.     ECS096
01190      12  FILLER              PIC S9(9)       VALUE +09770889.     ECS096
01191      12  FILLER              PIC S9(9)       VALUE +09751543.     ECS096
01192      12  FILLER              PIC S9(9)       VALUE +09732430.     ECS096
01193      12  FILLER              PIC S9(9)       VALUE +09713354.     ECS096
01194      12  FILLER              PIC S9(9)       VALUE +09694122.     ECS096
01195      12  FILLER              PIC S9(9)       VALUE +09674637.     ECS096
01196      12  FILLER              PIC S9(9)       VALUE +09654611.     ECS096
01197      12  FILLER              PIC S9(9)       VALUE +09633950.     ECS096
01198      12  FILLER              PIC S9(9)       VALUE +09612659.     ECS096
01199      12  FILLER              PIC S9(9)       VALUE +09590646.     ECS096
01200      12  FILLER              PIC S9(9)       VALUE +09567916.     ECS096
01201      12  FILLER              PIC S9(9)       VALUE +09544570.     ECS096
01202      12  FILLER              PIC S9(9)       VALUE +09520804.     ECS096
01203      12  FILLER              PIC S9(9)       VALUE +09496621.     ECS096
01204      12  FILLER              PIC S9(9)       VALUE +09472120.     ECS096
01205      12  FILLER              PIC S9(9)       VALUE +09447398.     ECS096
01206      12  FILLER              PIC S9(9)       VALUE +09422457.     ECS096
01207      12  FILLER              PIC S9(9)       VALUE +09397393.     ECS096
01208      12  FILLER              PIC S9(9)       VALUE +09372208.     ECS096
01209      12  FILLER              PIC S9(9)       VALUE +09346809.     ECS096
01210      12  FILLER              PIC S9(9)       VALUE +09321199.     ECS096
01211      12  FILLER              PIC S9(9)       VALUE +09295286.     ECS096
01212      12  FILLER              PIC S9(9)       VALUE +09268980.     ECS096
01213      12  FILLER              PIC S9(9)       VALUE +09242285.     ECS096
01214      12  FILLER              PIC S9(9)       VALUE +09215113.     ECS096
01215      12  FILLER              PIC S9(9)       VALUE +09187468.     ECS096
01216      12  FILLER              PIC S9(9)       VALUE +09159262.     ECS096
01217      12  FILLER              PIC S9(9)       VALUE +09130410.     ECS096
01218      12  FILLER              PIC S9(9)       VALUE +09100645.     ECS096
01219      12  FILLER              PIC S9(9)       VALUE +09069430.     ECS096
01220      12  FILLER              PIC S9(9)       VALUE +09036417.     ECS096
01221      12  FILLER              PIC S9(9)       VALUE +09001085.     ECS096
01222      12  FILLER              PIC S9(9)       VALUE +08963010.     ECS096
01223      12  FILLER              PIC S9(9)       VALUE +08921870.     ECS096
01224      12  FILLER              PIC S9(9)       VALUE +08877350.     ECS096
01225      12  FILLER              PIC S9(9)       VALUE +08829235.     ECS096
01226      12  FILLER              PIC S9(9)       VALUE +08777231.     ECS096
01227      12  FILLER              PIC S9(9)       VALUE +08721057.     ECS096
01228      12  FILLER              PIC S9(9)       VALUE +08660358.     ECS096
01229      12  FILLER              PIC S9(9)       VALUE +08594712.     ECS096
01230      12  FILLER              PIC S9(9)       VALUE +08523634.     ECS096
01231      12  FILLER              PIC S9(9)       VALUE +08446580.     ECS096
01232      12  FILLER              PIC S9(9)       VALUE +08363128.     ECS096
01233      12  FILLER              PIC S9(9)       VALUE +08272639.     ECS096
01234      12  FILLER              PIC S9(9)       VALUE +08174691.     ECS096
01235      12  FILLER              PIC S9(9)       VALUE +08068829.     ECS096
01236      12  FILLER              PIC S9(9)       VALUE +07954574.     ECS096
01237      12  FILLER              PIC S9(9)       VALUE +07831517.     ECS096
01238      12  FILLER              PIC S9(9)       VALUE +07699164.     ECS096
01239      12  FILLER              PIC S9(9)       VALUE +07556960.     ECS096
01240      12  FILLER              PIC S9(9)       VALUE +07404309.     ECS096
01241      12  FILLER              PIC S9(9)       VALUE +07240674.     ECS096
01242      12  FILLER              PIC S9(9)       VALUE +07065667.     ECS096
01243      12  FILLER              PIC S9(9)       VALUE +06878851.     ECS096
01244      12  FILLER              PIC S9(9)       VALUE +06679983.     ECS096
01245      12  FILLER              PIC S9(9)       VALUE +06468896.     ECS096
01246      12  FILLER              PIC S9(9)       VALUE +06245460.     ECS096
01247      12  FILLER              PIC S9(9)       VALUE +06009694.     ECS096
01248      12  FILLER              PIC S9(9)       VALUE +05761614.     ECS096
01249      12  FILLER              PIC S9(9)       VALUE +05501420.     ECS096
01250      12  FILLER              PIC S9(9)       VALUE +05229375.     ECS096
01251      12  FILLER              PIC S9(9)       VALUE +04946047.     ECS096
01252      12  FILLER              PIC S9(9)       VALUE +04652796.     ECS096
01253      12  FILLER              PIC S9(9)       VALUE +04351621.     ECS096
01254      12  FILLER              PIC S9(9)       VALUE +04045267.     ECS096
01255      12  FILLER              PIC S9(9)       VALUE +03736815.     ECS096
01256      12  FILLER              PIC S9(9)       VALUE +03429499.     ECS096
01257      12  FILLER              PIC S9(9)       VALUE +03125783.     ECS096
01258      12  FILLER              PIC S9(9)       VALUE +02827646.     ECS096
01259      12  FILLER              PIC S9(9)       VALUE +02536596.     ECS096
01260      12  FILLER              PIC S9(9)       VALUE +02253994.     ECS096
01261      12  FILLER              PIC S9(9)       VALUE +01981306.     ECS096
01262      12  FILLER              PIC S9(9)       VALUE +01720665.     ECS096
01263      12  FILLER              PIC S9(9)       VALUE +01474662.     ECS096
01264      12  FILLER              PIC S9(9)       VALUE +01245853.     ECS096
01265      12  FILLER              PIC S9(9)       VALUE +01036649.     ECS096
01266      12  FILLER              PIC S9(9)       VALUE +00848819.     ECS096
01267      12  FILLER              PIC S9(9)       VALUE +00683291.     ECS096
01268      12  FILLER              PIC S9(9)       VALUE +00540155.     ECS096
01269      12  FILLER              PIC S9(9)       VALUE +00418798.     ECS096
01270      12  FILLER              PIC S9(9)       VALUE +00318006.     ECS096
01271      12  FILLER              PIC S9(9)       VALUE +00236046.     ECS096
01272      12  FILLER              PIC S9(9)       VALUE +00170850.     ECS096
01273      12  FILLER              PIC S9(9)       VALUE +00120179.     ECS096
01274      12  FILLER              PIC S9(9)       VALUE +00081782.     ECS096
01275      12  FILLER              PIC S9(9)       VALUE +00053509.     ECS096
01276      12  FILLER              PIC S9(9)       VALUE +00033385.     ECS096
01277      12  FILLER              PIC S9(9)       VALUE +00019642.     ECS096
01278      12  FILLER              PIC S9(9)       VALUE +00010673.     ECS096
01279      12  FILLER              PIC S9(9)       VALUE +00005115.     ECS096
01280      12  FILLER              PIC S9(9)       VALUE +00001867.     ECS096
01281      12  FILLER              PIC S9(9)       VALUE +00000245.     ECS096
01282                                                                   ECS096
01283  01  58CET-TABLE-R  REDEFINES  58CET-TABLE.                       ECS096
01284      12  58CET-OCCURS    OCCURS  100  TIMES  COMP-3.              ECS096
01285          16  58CET-VALUE     PIC S9(9).                           ECS096
01286                                  EJECT                            ECS096
01287  01  FILLER                  PIC  X(16)  SYNC    VALUE            ECS096
01288          '***58CSO-TABLE**'.                                      ECS096
01289                                                                   ECS096
01290  01  58CSO-TABLE     COMP-3.                                      ECS096
01291      12  FILLER              PIC S9(9)       VALUE +10000000.     ECS096
01292      12  FILLER              PIC S9(9)       VALUE +09929200.     ECS096
01293      12  FILLER              PIC S9(9)       VALUE +09911725.     ECS096
01294      12  FILLER              PIC S9(9)       VALUE +09896659.     ECS096
01295      12  FILLER              PIC S9(9)       VALUE +09882210.     ECS096
01296      12  FILLER              PIC S9(9)       VALUE +09868375.     ECS096
01297      12  FILLER              PIC S9(9)       VALUE +09855053.     ECS096
01298      12  FILLER              PIC S9(9)       VALUE +09842241.     ECS096
01299      12  FILLER              PIC S9(9)       VALUE +09829840.     ECS096
01300      12  FILLER              PIC S9(9)       VALUE +09817749.     ECS096
01301      12  FILLER              PIC S9(9)       VALUE +09805870.     ECS096
01302      12  FILLER              PIC S9(9)       VALUE +09794005.     ECS096
01303      12  FILLER              PIC S9(9)       VALUE +09781958.     ECS096
01304      12  FILLER              PIC S9(9)       VALUE +09769633.     ECS096
01305      12  FILLER              PIC S9(9)       VALUE +09756737.     ECS096
01306      12  FILLER              PIC S9(9)       VALUE +09743175.     ECS096
01307      12  FILLER              PIC S9(9)       VALUE +09728950.     ECS096
01308      12  FILLER              PIC S9(9)       VALUE +09713967.     ECS096
01309      12  FILLER              PIC S9(9)       VALUE +09698230.     ECS096
01310      12  FILLER              PIC S9(9)       VALUE +09681840.     ECS096
01311      12  FILLER              PIC S9(9)       VALUE +09664994.     ECS096
01312      12  FILLER              PIC S9(9)       VALUE +09647694.     ECS096
01313      12  FILLER              PIC S9(9)       VALUE +09630039.     ECS096
01314      12  FILLER              PIC S9(9)       VALUE +09612127.     ECS096
01315      12  FILLER              PIC S9(9)       VALUE +09593960.     ECS096
01316      12  FILLER              PIC S9(9)       VALUE +09575636.     ECS096
01317      12  FILLER              PIC S9(9)       VALUE +09557155.     ECS096
01318      12  FILLER              PIC S9(9)       VALUE +09538423.     ECS096
01319      12  FILLER              PIC S9(9)       VALUE +09519442.     ECS096
01320      12  FILLER              PIC S9(9)       VALUE +09500118.     ECS096
01321      12  FILLER              PIC S9(9)       VALUE +09480358.     ECS096
01322      12  FILLER              PIC S9(9)       VALUE +09460165.     ECS096
01323      12  FILLER              PIC S9(9)       VALUE +09439447.     ECS096
01324      12  FILLER              PIC S9(9)       VALUE +09418208.     ECS096
01325      12  FILLER              PIC S9(9)       VALUE +09396358.     ECS096
01326      12  FILLER              PIC S9(9)       VALUE +09373807.     ECS096
01327      12  FILLER              PIC S9(9)       VALUE +09350279.     ECS096
01328      12  FILLER              PIC S9(9)       VALUE +09325594.     ECS096
01329      12  FILLER              PIC S9(9)       VALUE +09299482.     ECS096
01330      12  FILLER              PIC S9(9)       VALUE +09271491.     ECS096
01331      12  FILLER              PIC S9(9)       VALUE +09241359.     ECS096
01332      12  FILLER              PIC S9(9)       VALUE +09208737.     ECS096
01333      12  FILLER              PIC S9(9)       VALUE +09173375.     ECS096
01334      12  FILLER              PIC S9(9)       VALUE +09135122.     ECS096
01335      12  FILLER              PIC S9(9)       VALUE +09093740.     ECS096
01336      12  FILLER              PIC S9(9)       VALUE +09048999.     ECS096
01337      12  FILLER              PIC S9(9)       VALUE +09000587.     ECS096
01338      12  FILLER              PIC S9(9)       VALUE +08948114.     ECS096
01339      12  FILLER              PIC S9(9)       VALUE +08891204.     ECS096
01340      12  FILLER              PIC S9(9)       VALUE +08829410.     ECS096
01341      12  FILLER              PIC S9(9)       VALUE +08762306.     ECS096
01342      12  FILLER              PIC S9(9)       VALUE +08689404.     ECS096
01343      12  FILLER              PIC S9(9)       VALUE +08610244.     ECS096
01344      12  FILLER              PIC S9(9)       VALUE +08524486.     ECS096
01345      12  FILLER              PIC S9(9)       VALUE +08431654.     ECS096
01346      12  FILLER              PIC S9(9)       VALUE +08331317.     ECS096
01347      12  FILLER              PIC S9(9)       VALUE +08223010.     ECS096
01348      12  FILLER              PIC S9(9)       VALUE +08106161.     ECS096
01349      12  FILLER              PIC S9(9)       VALUE +07980191.     ECS096
01350      12  FILLER              PIC S9(9)       VALUE +07844528.     ECS096
01351      12  FILLER              PIC S9(9)       VALUE +07698698.     ECS096
01352      12  FILLER              PIC S9(9)       VALUE +07542106.     ECS096
01353      12  FILLER              PIC S9(9)       VALUE +07374370.     ECS096
01354      12  FILLER              PIC S9(9)       VALUE +07195099.     ECS096
01355      12  FILLER              PIC S9(9)       VALUE +07003925.     ECS096
01356      12  FILLER              PIC S9(9)       VALUE +06800531.     ECS096
01357      12  FILLER              PIC S9(9)       VALUE +06584614.     ECS096
01358      12  FILLER              PIC S9(9)       VALUE +06355865.     ECS096
01359      12  FILLER              PIC S9(9)       VALUE +06114088.     ECS096
01360      12  FILLER              PIC S9(9)       VALUE +05859253.     ECS096
01361      12  FILLER              PIC S9(9)       VALUE +05592012.     ECS096
01362      12  FILLER              PIC S9(9)       VALUE +05313586.     ECS096
01363      12  FILLER              PIC S9(9)       VALUE +05025855.     ECS096
01364      12  FILLER              PIC S9(9)       VALUE +04731089.     ECS096
01365      12  FILLER              PIC S9(9)       VALUE +04431800.     ECS096
01366      12  FILLER              PIC S9(9)       VALUE +04129906.     ECS096
01367      12  FILLER              PIC S9(9)       VALUE +03826895.     ECS096
01368      12  FILLER              PIC S9(9)       VALUE +03523881.     ECS096
01369      12  FILLER              PIC S9(9)       VALUE +03221884.     ECS096
01370      12  FILLER              PIC S9(9)       VALUE +02922055.     ECS096
01371      12  FILLER              PIC S9(9)       VALUE +02626372.     ECS096
01372      12  FILLER              PIC S9(9)       VALUE +02337524.     ECS096
01373      12  FILLER              PIC S9(9)       VALUE +02058541.     ECS096
01374      12  FILLER              PIC S9(9)       VALUE +01792639.     ECS096
01375      12  FILLER              PIC S9(9)       VALUE +01542781.     ECS096
01376      12  FILLER              PIC S9(9)       VALUE +01311348.     ECS096
01377      12  FILLER              PIC S9(9)       VALUE +01100037.     ECS096
01378      12  FILLER              PIC S9(9)       VALUE +00909929.     ECS096
01379      12  FILLER              PIC S9(9)       VALUE +00741474.     ECS096
01380      12  FILLER              PIC S9(9)       VALUE +00594477.     ECS096
01381      12  FILLER              PIC S9(9)       VALUE +00468174.     ECS096
01382      12  FILLER              PIC S9(9)       VALUE +00361365.     ECS096
01383      12  FILLER              PIC S9(9)       VALUE +00272552.     ECS096
01384      12  FILLER              PIC S9(9)       VALUE +00200072.     ECS096
01385      12  FILLER              PIC S9(9)       VALUE +00142191.     ECS096
01386      12  FILLER              PIC S9(9)       VALUE +00097165.     ECS096
01387      12  FILLER              PIC S9(9)       VALUE +00063037.     ECS096
01388      12  FILLER              PIC S9(9)       VALUE +00037787.     ECS096
01389      12  FILLER              PIC S9(9)       VALUE +00019331.     ECS096
01390      12  FILLER              PIC S9(9)       VALUE +00006415.     ECS096
01391                                                                   ECS096
01392  01  58CSO-TABLE-R  REDEFINES  58CSO-TABLE.                       ECS096
01393      12  58CSO-OCCURS    OCCURS  100  TIMES  COMP-3.              ECS096
01394          16  58CSO-VALUE     PIC S9(9).                           ECS096
01395                                  EJECT                            ECS096
01396  01  FILLER                  PIC  X(16)  SYNC    VALUE            ECS096
01397          '***58FSO-TABLE**'.                                      ECS096
01398                                                                   ECS096
01399  01  58FSO-TABLE     COMP-3.                                      ECS096
01400      12  FILLER              PIC S9(9)       VALUE +10014660.     ECS096
01401      12  FILLER              PIC S9(9)       VALUE +09952569.     ECS096
01402      12  FILLER              PIC S9(9)       VALUE +09935948.     ECS096
01403      12  FILLER              PIC S9(9)       VALUE +09921938.     ECS096
01404      12  FILLER              PIC S9(9)       VALUE +09908543.     ECS096
01405      12  FILLER              PIC S9(9)       VALUE +09895761.     ECS096
01406      12  FILLER              PIC S9(9)       VALUE +09883490.     ECS096
01407      12  FILLER              PIC S9(9)       VALUE +09871729.     ECS096
01408      12  FILLER              PIC S9(9)       VALUE +09860377.     ECS096
01409      12  FILLER              PIC S9(9)       VALUE +09849333.     ECS096
01410      12  FILLER              PIC S9(9)       VALUE +09838400.     ECS096
01411      12  FILLER              PIC S9(9)       VALUE +09827479.     ECS096
01412      12  FILLER              PIC S9(9)       VALUE +09816472.     ECS096
01413      12  FILLER              PIC S9(9)       VALUE +09805281.     ECS096
01414      12  FILLER              PIC S9(9)       VALUE +09793809.     ECS096
01415      12  FILLER              PIC S9(9)       VALUE +09781958.     ECS096
01416      12  FILLER              PIC S9(9)       VALUE +09769633.     ECS096
01417      12  FILLER              PIC S9(9)       VALUE +09756737.     ECS096
01418      12  FILLER              PIC S9(9)       VALUE +09743175.     ECS096
01419      12  FILLER              PIC S9(9)       VALUE +09728950.     ECS096
01420      12  FILLER              PIC S9(9)       VALUE +09713967.     ECS096
01421      12  FILLER              PIC S9(9)       VALUE +09698230.     ECS096
01422      12  FILLER              PIC S9(9)       VALUE +09681840.     ECS096
01423      12  FILLER              PIC S9(9)       VALUE +09664994.     ECS096
01424      12  FILLER              PIC S9(9)       VALUE +09647694.     ECS096
01425      12  FILLER              PIC S9(9)       VALUE +09630039.     ECS096
01426      12  FILLER              PIC S9(9)       VALUE +09612127.     ECS096
01427      12  FILLER              PIC S9(9)       VALUE +09593960.     ECS096
01428      12  FILLER              PIC S9(9)       VALUE +09575636.     ECS096
01429      12  FILLER              PIC S9(9)       VALUE +09557155.     ECS096
01430      12  FILLER              PIC S9(9)       VALUE +09538423.     ECS096
01431      12  FILLER              PIC S9(9)       VALUE +09519442.     ECS096
01432      12  FILLER              PIC S9(9)       VALUE +09500118.     ECS096
01433      12  FILLER              PIC S9(9)       VALUE +09480358.     ECS096
01434      12  FILLER              PIC S9(9)       VALUE +09460165.     ECS096
01435      12  FILLER              PIC S9(9)       VALUE +09439447.     ECS096
01436      12  FILLER              PIC S9(9)       VALUE +09418208.     ECS096
01437      12  FILLER              PIC S9(9)       VALUE +09396358.     ECS096
01438      12  FILLER              PIC S9(9)       VALUE +09373807.     ECS096
01439      12  FILLER              PIC S9(9)       VALUE +09350279.     ECS096
01440      12  FILLER              PIC S9(9)       VALUE +09325594.     ECS096
01441      12  FILLER              PIC S9(9)       VALUE +09299482.     ECS096
01442      12  FILLER              PIC S9(9)       VALUE +09271491.     ECS096
01443      12  FILLER              PIC S9(9)       VALUE +09241359.     ECS096
01444      12  FILLER              PIC S9(9)       VALUE +09208737.     ECS096
01445      12  FILLER              PIC S9(9)       VALUE +09173375.     ECS096
01446      12  FILLER              PIC S9(9)       VALUE +09135122.     ECS096
01447      12  FILLER              PIC S9(9)       VALUE +09093740.     ECS096
01448      12  FILLER              PIC S9(9)       VALUE +09048999.     ECS096
01449      12  FILLER              PIC S9(9)       VALUE +09000587.     ECS096
01450      12  FILLER              PIC S9(9)       VALUE +08948114.     ECS096
01451      12  FILLER              PIC S9(9)       VALUE +08891204.     ECS096
01452      12  FILLER              PIC S9(9)       VALUE +08829410.     ECS096
01453      12  FILLER              PIC S9(9)       VALUE +08762306.     ECS096
01454      12  FILLER              PIC S9(9)       VALUE +08689404.     ECS096
01455      12  FILLER              PIC S9(9)       VALUE +08610244.     ECS096
01456      12  FILLER              PIC S9(9)       VALUE +08524486.     ECS096
01457      12  FILLER              PIC S9(9)       VALUE +08431654.     ECS096
01458      12  FILLER              PIC S9(9)       VALUE +08331317.     ECS096
01459      12  FILLER              PIC S9(9)       VALUE +08223010.     ECS096
01460      12  FILLER              PIC S9(9)       VALUE +08106161.     ECS096
01461      12  FILLER              PIC S9(9)       VALUE +07980191.     ECS096
01462      12  FILLER              PIC S9(9)       VALUE +07844528.     ECS096
01463      12  FILLER              PIC S9(9)       VALUE +07698698.     ECS096
01464      12  FILLER              PIC S9(9)       VALUE +07542106.     ECS096
01465      12  FILLER              PIC S9(9)       VALUE +07374370.     ECS096
01466      12  FILLER              PIC S9(9)       VALUE +07195099.     ECS096
01467      12  FILLER              PIC S9(9)       VALUE +07003925.     ECS096
01468      12  FILLER              PIC S9(9)       VALUE +06800531.     ECS096
01469      12  FILLER              PIC S9(9)       VALUE +06584614.     ECS096
01470      12  FILLER              PIC S9(9)       VALUE +06355865.     ECS096
01471      12  FILLER              PIC S9(9)       VALUE +06114088.     ECS096
01472      12  FILLER              PIC S9(9)       VALUE +05859253.     ECS096
01473      12  FILLER              PIC S9(9)       VALUE +05592012.     ECS096
01474      12  FILLER              PIC S9(9)       VALUE +05313586.     ECS096
01475      12  FILLER              PIC S9(9)       VALUE +05025855.     ECS096
01476      12  FILLER              PIC S9(9)       VALUE +04731089.     ECS096
01477      12  FILLER              PIC S9(9)       VALUE +04431800.     ECS096
01478      12  FILLER              PIC S9(9)       VALUE +04129906.     ECS096
01479      12  FILLER              PIC S9(9)       VALUE +03826895.     ECS096
01480      12  FILLER              PIC S9(9)       VALUE +03523881.     ECS096
01481      12  FILLER              PIC S9(9)       VALUE +03221884.     ECS096
01482      12  FILLER              PIC S9(9)       VALUE +02922055.     ECS096
01483      12  FILLER              PIC S9(9)       VALUE +02626372.     ECS096
01484      12  FILLER              PIC S9(9)       VALUE +02337524.     ECS096
01485      12  FILLER              PIC S9(9)       VALUE +02058541.     ECS096
01486      12  FILLER              PIC S9(9)       VALUE +01792639.     ECS096
01487      12  FILLER              PIC S9(9)       VALUE +01542781.     ECS096
01488      12  FILLER              PIC S9(9)       VALUE +01311348.     ECS096
01489      12  FILLER              PIC S9(9)       VALUE +01100037.     ECS096
01490      12  FILLER              PIC S9(9)       VALUE +00909929.     ECS096
01491      12  FILLER              PIC S9(9)       VALUE +00741474.     ECS096
01492      12  FILLER              PIC S9(9)       VALUE +00594477.     ECS096
01493      12  FILLER              PIC S9(9)       VALUE +00468174.     ECS096
01494      12  FILLER              PIC S9(9)       VALUE +00361365.     ECS096
01495      12  FILLER              PIC S9(9)       VALUE +00272552.     ECS096
01496      12  FILLER              PIC S9(9)       VALUE +00200072.     ECS096
01497      12  FILLER              PIC S9(9)       VALUE +00142191.     ECS096
01498      12  FILLER              PIC S9(9)       VALUE +00097165.     ECS096
01499      12  FILLER              PIC S9(9)       VALUE +00063037.     ECS096
01500                                                                   ECS096
01501  01  58FSO-TABLE-R  REDEFINES  58FSO-TABLE.                       ECS096
01502      12  58FSO-OCCURS    OCCURS  100  TIMES  COMP-3.              ECS096
01503          16  58FSO-VALUE     PIC S9(9).                           ECS096
01504                                  EJECT                            ECS096
CIDMOD 01  FILLER                  PIC  X(16)  SYNC    VALUE            00006027
CIDMOD         '***58UET-TABLE**'.                                      00006028
CIDMOD                                                                  00006029
CIDMOD 01  58UET-TABLE     COMP-3.                                      00006030
CIDMOD     12  FILLER              PIC S9(09)      VALUE +10000000.     00006031
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09949661.     00006032
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09926180.     00006033
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09904273.     00006034
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09883008.     00006035
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09862333.     00006036
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09842194.     00006037
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09822539.     00006038
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09803267.     00006039
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09784264.     00006040
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09765380.     00006041
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09746450.     00006042
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09727342.     00006043
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09707892.     00006044
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09687938.     00006045
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09667419.     00006046
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09646276.     00006047
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09624451.     00006048
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09601969.     00006049
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09578919.     00006050
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09555360.     00006051
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09531342.     00006052
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09506980.     00006053
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09482338.     00006054
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09457466.     00006055
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09432427.     00006056
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09407205.     00006057
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09381782.     00006058
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09356142.     00006059
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09330206.     00006060
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09303932.     00006061
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09277277.     00006062
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09250182.     00006063
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09222608.     00006064
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09194479.     00006065
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09165659.     00006066
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09135852.     00006067
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09104721.     00006068
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09071908.     00006069
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09036959.     00006070
CIDMOD     12  FILLER              PIC S9(09)      VALUE +08999478.     00006071
CIDMOD     12  FILLER              PIC S9(09)      VALUE +08959111.     00006072
CIDMOD     12  FILLER              PIC S9(09)      VALUE +08915535.     00006073
CIDMOD     12  FILLER              PIC S9(09)      VALUE +08868454.     00006074
CIDMOD     12  FILLER              PIC S9(09)      VALUE +08817569.     00006075
CIDMOD     12  FILLER              PIC S9(09)      VALUE +08762578.     00006076
CIDMOD     12  FILLER              PIC S9(09)      VALUE +08703128.     00006077
CIDMOD     12  FILLER              PIC S9(09)      VALUE +08638812.     00006078
CIDMOD     12  FILLER              PIC S9(09)      VALUE +08569176.     00006079
CIDMOD     12  FILLER              PIC S9(09)      VALUE +08493756.     00006080
CIDMOD     12  FILLER              PIC S9(09)      VALUE +08412040.     00006081
CIDMOD     12  FILLER              PIC S9(09)      VALUE +08323505.     00006082
CIDMOD     12  FILLER              PIC S9(09)      VALUE +08227686.     00006083
CIDMOD     12  FILLER              PIC S9(09)      VALUE +08124099.     00006084
CIDMOD     12  FILLER              PIC S9(09)      VALUE +08012270.     00006085
CIDMOD     12  FILLER              PIC S9(09)      VALUE +07891736.     00006086
CIDMOD     12  FILLER              PIC S9(09)      VALUE +07761997.     00006087
CIDMOD     12  FILLER              PIC S9(09)      VALUE +07622499.     00006088
CIDMOD     12  FILLER              PIC S9(09)      VALUE +07472702.     00006089
CIDMOD     12  FILLER              PIC S9(09)      VALUE +07312112.     00006090
CIDMOD     12  FILLER              PIC S9(09)      VALUE +07140264.     00006091
CIDMOD     12  FILLER              PIC S9(09)      VALUE +06956741.     00006092
CIDMOD     12  FILLER              PIC S9(09)      VALUE +06761245.     00006093
CIDMOD     12  FILLER              PIC S9(09)      VALUE +06553543.     00006094
CIDMOD     12  FILLER              PIC S9(09)      VALUE +06333479.     00006095
CIDMOD     12  FILLER              PIC S9(09)      VALUE +06100995.     00006096
CIDMOD     12  FILLER              PIC S9(09)      VALUE +05856106.     00006097
CIDMOD     12  FILLER              PIC S9(09)      VALUE +05598944.     00006098
CIDMOD     12  FILLER              PIC S9(09)      VALUE +05329787.     00006099
CIDMOD     12  FILLER              PIC S9(09)      VALUE +05049341.     00006100
CIDMOD     12  FILLER              PIC S9(09)      VALUE +04758876.     00006101
CIDMOD     12  FILLER              PIC S9(09)      VALUE +04460230.     00006102
CIDMOD     12  FILLER              PIC S9(09)      VALUE +04155812.     00006103
CIDMOD     12  FILLER              PIC S9(09)      VALUE +03848422.     00006104
CIDMOD     12  FILLER              PIC S9(09)      VALUE +03540833.     00006105
CIDMOD     12  FILLER              PIC S9(09)      VALUE +03235494.     00006106
CIDMOD     12  FILLER              PIC S9(09)      VALUE +02934548.     00006107
CIDMOD     12  FILLER              PIC S9(09)      VALUE +02639810.     00006108
CIDMOD     12  FILLER              PIC S9(09)      VALUE +02352908.     00006109
CIDMOD     12  FILLER              PIC S9(09)      VALUE +02075650.     00006110
CIDMOD     12  FILLER              PIC S9(09)      VALUE +01810210.     00006111
CIDMOD     12  FILLER              PIC S9(09)      VALUE +01558969.     00006112
CIDMOD     12  FILLER              PIC S9(09)      VALUE +01324375.     00006113
CIDMOD     12  FILLER              PIC S9(09)      VALUE +01108740.     00006114
CIDMOD     12  FILLER              PIC S9(09)      VALUE +00913861.     00006115
CIDMOD     12  FILLER              PIC S9(09)      VALUE +00740844.     00006116
CIDMOD     12  FILLER              PIC S9(09)      VALUE +00590070.     00006117
CIDMOD     12  FILLER              PIC S9(09)      VALUE +00461209.     00006118
CIDMOD     12  FILLER              PIC S9(09)      VALUE +00353257.     00006119
CIDMOD     12  FILLER              PIC S9(09)      VALUE +00264669.     00006120
CIDMOD     12  FILLER              PIC S9(09)      VALUE +00193518.     00006121
CIDMOD     12  FILLER              PIC S9(09)      VALUE +00137661.     00006122
CIDMOD     12  FILLER              PIC S9(09)      VALUE +00094882.     00006123
CIDMOD     12  FILLER              PIC S9(09)      VALUE +00063017.     00006124
CIDMOD     12  FILLER              PIC S9(09)      VALUE +00040039.     00006125
CIDMOD     12  FILLER              PIC S9(09)      VALUE +00024070.     00006126
CIDMOD     12  FILLER              PIC S9(09)      VALUE +00013412.     00006127
CIDMOD     12  FILLER              PIC S9(09)      VALUE +00006607.     00006128
CIDMOD     12  FILLER              PIC S9(09)      VALUE +00002532.     00006129
CIDMOD     12  FILLER              PIC S9(09)      VALUE +00000600.     00006130
CIDMOD                                                                  00006132
CIDMOD 01  58UET-TABLE-R  REDEFINES  58UET-TABLE.                       00006133
CIDMOD     12  58UET-OCCURS    OCCURS  100  TIMES  COMP-3.              00006134
CIDMOD         16  58UET-VALUE     PIC S9(09).                          00006135
CIDMOD                                                                  00006132
CIDMOD                                 EJECT                            00006136
CIDMOD 01  FILLER                  PIC  X(16)  SYNC    VALUE            00006137
CIDMOD         '***58USO-TABLE**'.                                      00006138
CIDMOD                                                                  00006139
CIDMOD 01  58USO-TABLE     COMP-3.                                      00006140
CIDMOD     12  FILLER              PIC S9(09)      VALUE +10000000.     00006141
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09957170.     00006142
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09941139.     00006143
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09926655.     00006144
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09912787.     00006145
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09899484.     00006146
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09886694.     00006147
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09874365.     00006148
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09862398.     00006149
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09850671.     00006150
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09839047.     00006151
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09827349.     00006152
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09815448.     00006153
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09803179.     00006154
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09790376.     00006155
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09776982.     00006156
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09762933.     00006157
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09748162.     00006158
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09732701.     00006159
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09716632.     00006160
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09700017.     00006161
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09682906.     00006162
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09665419.     00006163
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09647615.     00006164
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09629545.     00006165
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09611268.     00006166
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09592776.     00006167
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09574041.     00006168
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09555056.     00006169
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09535736.     00006170
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09516035.     00006171
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09495908.     00006172
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09475293.     00006173
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09454153.     00006174
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09432409.     00006175
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09409913.     00006176
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09386501.     00006177
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09361983.     00006178
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09336051.     00006179
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09308378.     00006180
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09278694.     00006181
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09246673.     00006182
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09212063.     00006183
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09174644.     00006184
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09134165.     00006185
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09090358.     00006186
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09042933.     00006187
CIDMOD     12  FILLER              PIC S9(09)      VALUE +08991542.     00006188
CIDMOD     12  FILLER              PIC S9(09)      VALUE +08935813.     00006189
CIDMOD     12  FILLER              PIC S9(09)      VALUE +08875326.     00006190
CIDMOD     12  FILLER              PIC S9(09)      VALUE +08809649.     00006191
CIDMOD     12  FILLER              PIC S9(09)      VALUE +08738335.     00006192
CIDMOD     12  FILLER              PIC S9(09)      VALUE +08660957.     00006193
CIDMOD     12  FILLER              PIC S9(09)      VALUE +08577084.     00006194
CIDMOD     12  FILLER              PIC S9(09)      VALUE +08486270.     00006195
CIDMOD     12  FILLER              PIC S9(09)      VALUE +08388058.     00006196
CIDMOD     12  FILLER              PIC S9(09)      VALUE +08281966.     00006197
CIDMOD     12  FILLER              PIC S9(09)      VALUE +08167451.     00006198
CIDMOD     12  FILLER              PIC S9(09)      VALUE +08043968.     00006199
CIDMOD     12  FILLER              PIC S9(09)      VALUE +07910985.     00006200
CIDMOD     12  FILLER              PIC S9(09)      VALUE +07767946.     00006201
CIDMOD     12  FILLER              PIC S9(09)      VALUE +07614335.     00006202
CIDMOD     12  FILLER              PIC S9(09)      VALUE +07449706.     00006203
CIDMOD     12  FILLER              PIC S9(09)      VALUE +07273632.     00006204
CIDMOD     12  FILLER              PIC S9(09)      VALUE +07085710.     00006205
CIDMOD     12  FILLER              PIC S9(09)      VALUE +06885603.     00006206
CIDMOD     12  FILLER              PIC S9(09)      VALUE +06672955.     00006207
CIDMOD     12  FILLER              PIC S9(09)      VALUE +06447482.     00006208
CIDMOD     12  FILLER              PIC S9(09)      VALUE +06208990.     00006209
CIDMOD     12  FILLER              PIC S9(09)      VALUE +05957600.     00006210
CIDMOD     12  FILLER              PIC S9(09)      VALUE +05693893.     00006211
CIDMOD     12  FILLER              PIC S9(09)      VALUE +05418952.     00006212
CIDMOD     12  FILLER              PIC S9(09)      VALUE +05134365.     00006213
CIDMOD     12  FILLER              PIC S9(09)      VALUE +04842142.     00006214
CIDMOD     12  FILLER              PIC S9(09)      VALUE +04544341.     00006215
CIDMOD     12  FILLER              PIC S9(09)      VALUE +04242788.     00006216
CIDMOD     12  FILLER              PIC S9(09)      VALUE +03939089.     00006217
CIDMOD     12  FILLER              PIC S9(09)      VALUE +03634605.     00006218
CIDMOD     12  FILLER              PIC S9(09)      VALUE +03330585.     00006219
CIDMOD     12  FILLER              PIC S9(09)      VALUE +03028508.     00006220
CIDMOD     12  FILLER              PIC S9(09)      VALUE +02730390.     00006221
CIDMOD     12  FILLER              PIC S9(09)      VALUE +02438678.     00006222
CIDMOD     12  FILLER              PIC S9(09)      VALUE +02156177.     00006223
CIDMOD     12  FILLER              PIC S9(09)      VALUE +01885898.     00006224
CIDMOD     12  FILLER              PIC S9(09)      VALUE +01630689.     00006225
CIDMOD     12  FILLER              PIC S9(09)      VALUE +01392982.     00006226
CIDMOD     12  FILLER              PIC S9(09)      VALUE +01174694.     00006227
CIDMOD     12  FILLER              PIC S9(09)      VALUE +00977154.     00006228
CIDMOD     12  FILLER              PIC S9(09)      VALUE +00801021.     00006229
CIDMOD     12  FILLER              PIC S9(09)      VALUE +00646311.     00006230
CIDMOD     12  FILLER              PIC S9(09)      VALUE +00512476.     00006231
CIDMOD     12  FILLER              PIC S9(09)      VALUE +00398511.     00006232
CIDMOD     12  FILLER              PIC S9(09)      VALUE +00303077.     00006233
CIDMOD     12  FILLER              PIC S9(09)      VALUE +00224614.     00006234
CIDMOD     12  FILLER              PIC S9(09)      VALUE +00161450.     00006235
CIDMOD     12  FILLER              PIC S9(09)      VALUE +00111753.     00006236
CIDMOD     12  FILLER              PIC S9(09)      VALUE +00073511.     00006237
CIDMOD     12  FILLER              PIC S9(09)      VALUE +00044574.     00006238
CIDMOD     12  FILLER              PIC S9(09)      VALUE +00023014.     00006239
CIDMOD     12  FILLER              PIC S9(09)      VALUE +00008360.     00006240
CIDMOD                                                                  00006242
CIDMOD 01  58USO-TABLE-R  REDEFINES  58USO-TABLE.                       00006243
CIDMOD     12  58USO-OCCURS    OCCURS  100  TIMES  COMP-3.              00006244
CIDMOD         16  58USO-VALUE     PIC S9(09).                          00006245
CIDMOD                                                                  00006242
01504                                  EJECT                            ECS096
01505  01  FILLER                  PIC  X(16)  SYNC    VALUE            ECS096
01506          '***60CSG-TABLE**'.                                      ECS096
01507                                                                   ECS096
01508  01  60CSG-TABLE     COMP-3.                                      ECS096
01509      12  FILLER              PIC S9(9)       VALUE +10000000.     ECS096
01510      12  FILLER              PIC S9(9)       VALUE +09916800.     ECS096
01511      12  FILLER              PIC S9(9)       VALUE +09896272.     ECS096
01512      12  FILLER              PIC S9(9)       VALUE +09878558.     ECS096
01513      12  FILLER              PIC S9(9)       VALUE +09861567.     ECS096
01514      12  FILLER              PIC S9(9)       VALUE +09845295.     ECS096
01515      12  FILLER              PIC S9(9)       VALUE +09829641.     ECS096
01516      12  FILLER              PIC S9(9)       VALUE +09814602.     ECS096
01517      12  FILLER              PIC S9(9)       VALUE +09800076.     ECS096
01518      12  FILLER              PIC S9(9)       VALUE +09785866.     ECS096
01519      12  FILLER              PIC S9(9)       VALUE +09771970.     ECS096
01520      12  FILLER              PIC S9(9)       VALUE +09758094.     ECS096
01521      12  FILLER              PIC S9(9)       VALUE +09743945.     ECS096
01522      12  FILLER              PIC S9(9)       VALUE +09729524.     ECS096
01523      12  FILLER              PIC S9(9)       VALUE +09714443.     ECS096
01524      12  FILLER              PIC S9(9)       VALUE +09698608.     ECS096
01525      12  FILLER              PIC S9(9)       VALUE +09681926.     ECS096
01526      12  FILLER              PIC S9(9)       VALUE +09664402.     ECS096
01527      12  FILLER              PIC S9(9)       VALUE +09646040.     ECS096
01528      12  FILLER              PIC S9(9)       VALUE +09626844.     ECS096
01529      12  FILLER              PIC S9(9)       VALUE +09607302.     ECS096
01530      12  FILLER              PIC S9(9)       VALUE +09587223.     ECS096
01531      12  FILLER              PIC S9(9)       VALUE +09566706.     ECS096
01532      12  FILLER              PIC S9(9)       VALUE +09545851.     ECS096
01533      12  FILLER              PIC S9(9)       VALUE +09524755.     ECS096
01534      12  FILLER              PIC S9(9)       VALUE +09503420.     ECS096
01535      12  FILLER              PIC S9(9)       VALUE +09481942.     ECS096
01536      12  FILLER              PIC S9(9)       VALUE +09460323.     ECS096
01537      12  FILLER              PIC S9(9)       VALUE +09438564.     ECS096
01538      12  FILLER              PIC S9(9)       VALUE +09416572.     ECS096
01539      12  FILLER              PIC S9(9)       VALUE +09394349.     ECS096
01540      12  FILLER              PIC S9(9)       VALUE +09371803.     ECS096
01541      12  FILLER              PIC S9(9)       VALUE +09348842.     ECS096
01542      12  FILLER              PIC S9(9)       VALUE +09325376.     ECS096
01543      12  FILLER              PIC S9(9)       VALUE +09301130.     ECS096
01544      12  FILLER              PIC S9(9)       VALUE +09275924.     ECS096
01545      12  FILLER              PIC S9(9)       VALUE +09249488.     ECS096
01546      12  FILLER              PIC S9(9)       VALUE +09221555.     ECS096
01547      12  FILLER              PIC S9(9)       VALUE +09191954.     ECS096
01548      12  FILLER              PIC S9(9)       VALUE +09160242.     ECS096
01549      12  FILLER              PIC S9(9)       VALUE +09126166.     ECS096
01550      12  FILLER              PIC S9(9)       VALUE +09089479.     ECS096
01551      12  FILLER              PIC S9(9)       VALUE +09049758.     ECS096
01552      12  FILLER              PIC S9(9)       VALUE +09006772.     ECS096
01553      12  FILLER              PIC S9(9)       VALUE +08960117.     ECS096
01554      12  FILLER              PIC S9(9)       VALUE +08909582.     ECS096
01555      12  FILLER              PIC S9(9)       VALUE +08854788.     ECS096
01556      12  FILLER              PIC S9(9)       VALUE +08795461.     ECS096
01557      12  FILLER              PIC S9(9)       VALUE +08731166.     ECS096
01558      12  FILLER              PIC S9(9)       VALUE +08661491.     ECS096
01559      12  FILLER              PIC S9(9)       VALUE +08585963.     ECS096
01560      12  FILLER              PIC S9(9)       VALUE +08504225.     ECS096
01561      12  FILLER              PIC S9(9)       VALUE +08415781.     ECS096
01562      12  FILLER              PIC S9(9)       VALUE +08320094.     ECS096
01563      12  FILLER              PIC S9(9)       VALUE +08216592.     ECS096
01564      12  FILLER              PIC S9(9)       VALUE +08104764.     ECS096
01565      12  FILLER              PIC S9(9)       VALUE +07984165.     ECS096
01566      12  FILLER              PIC S9(9)       VALUE +07854502.     ECS096
01567      12  FILLER              PIC S9(9)       VALUE +07715477.     ECS096
01568      12  FILLER              PIC S9(9)       VALUE +07567031.     ECS096
01569      12  FILLER              PIC S9(9)       VALUE +07409107.     ECS096
01570      12  FILLER              PIC S9(9)       VALUE +07241513.     ECS096
01571      12  FILLER              PIC S9(9)       VALUE +07064024.     ECS096
01572      12  FILLER              PIC S9(9)       VALUE +06876121.     ECS096
01573      12  FILLER              PIC S9(9)       VALUE +06677676.     ECS096
01574      12  FILLER              PIC S9(9)       VALUE +06468598.     ECS096
01575      12  FILLER              PIC S9(9)       VALUE +06248666.     ECS096
01576      12  FILLER              PIC S9(9)       VALUE +06017465.     ECS096
01577      12  FILLER              PIC S9(9)       VALUE +05774841.     ECS096
01578      12  FILLER              PIC S9(9)       VALUE +05520690.     ECS096
01579      12  FILLER              PIC S9(9)       VALUE +05255531.     ECS096
01580      12  FILLER              PIC S9(9)       VALUE +04980509.     ECS096
01581      12  FILLER              PIC S9(9)       VALUE +04697317.     ECS096
01582      12  FILLER              PIC S9(9)       VALUE +04408056.     ECS096
01583      12  FILLER              PIC S9(9)       VALUE +04115273.     ECS096
01584      12  FILLER              PIC S9(9)       VALUE +03820908.     ECS096
01585      12  FILLER              PIC S9(9)       VALUE +03526545.     ECS096
01586      12  FILLER              PIC S9(9)       VALUE +03233348.     ECS096
01587      12  FILLER              PIC S9(9)       VALUE +02942411.     ECS096
01588      12  FILLER              PIC S9(9)       VALUE +02654908.     ECS096
01589      12  FILLER              PIC S9(9)       VALUE +02372824.     ECS096
01590      12  FILLER              PIC S9(9)       VALUE +02098810.     ECS096
01591      12  FILLER              PIC S9(9)       VALUE +01835787.     ECS096
01592      12  FILLER              PIC S9(9)       VALUE +01586799.     ECS096
01593      12  FILLER              PIC S9(9)       VALUE +01354571.     ECS096
01594      12  FILLER              PIC S9(9)       VALUE +01141213.     ECS096
01595      12  FILLER              PIC S9(9)       VALUE +00948120.     ECS096
01596      12  FILLER              PIC S9(9)       VALUE +00776074.     ECS096
01597      12  FILLER              PIC S9(9)       VALUE +00625213.     ECS096
01598      12  FILLER              PIC S9(9)       VALUE +00495069.     ECS096
01599      12  FILLER              PIC S9(9)       VALUE +00384629.     ECS096
01600      12  FILLER              PIC S9(9)       VALUE +00292491.     ECS096
01601      12  FILLER              PIC S9(9)       VALUE +00217011.     ECS096
01602      12  FILLER              PIC S9(9)       VALUE +00156415.     ECS096
01603      12  FILLER              PIC S9(9)       VALUE +00108902.     ECS096
01604      12  FILLER              PIC S9(9)       VALUE +00072693.     ECS096
01605      12  FILLER              PIC S9(9)       VALUE +00045884.     ECS096
01606      12  FILLER              PIC S9(9)       VALUE +00026586.     ECS096
01607      12  FILLER              PIC S9(9)       VALUE +00012952.     ECS096
01608      12  FILLER              PIC S9(9)       VALUE +00003865.     ECS096
01609                                                                   ECS096
01610  01  60CSG-TABLE-R  REDEFINES  60CSG-TABLE.                       ECS096
01611      12  60CSG-OCCURS    OCCURS  100  TIMES  COMP-3.              ECS096
01612          16  60CSG-VALUE     PIC S9(9).                           ECS096
01613                                  EJECT                            ECS096
CIDMOD 01  FILLER                  PIC  X(28)  SYNC    VALUE            00006356
CIDMOD         '***80CSO-TABLE  AGE LAST ***'.                          00006357
CIDMOD                                                                  00006358
CIDMOD 01  80CSO-TABLE     COMP-3.                                      00006359
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09979100.     00006360
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09952855.     00006361
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09942604.     00006362
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09932760.     00006363
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09923126.     00006364
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09913897.     00006365
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09905173.     00006366
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09896952.     00006367
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09889232.     00006368
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09881815.     00006369
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09874502.     00006370
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09867097.     00006371
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09859104.     00006372
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09850034.     00006373
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09839494.     00006374
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09827293.     00006375
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09813339.     00006376
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09797735.     00006377
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09780883.     00006378
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09763082.     00006379
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09744727.     00006380
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09726212.     00006381
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09707733.     00006382
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09689482.     00006383
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09671654.     00006384
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09654245.     00006385
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09637350.     00006386
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09620773.     00006387
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09604322.     00006388
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09587994.     00006389
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09571503.     00006390
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09554753.     00006391
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09537554.     00006392
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09519719.     00006393
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09501156.     00006394
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09481678.     00006395
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09461103.     00006396
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09439153.     00006397
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09415650.     00006398
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09390416.     00006399
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09363184.     00006400
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09333690.     00006401
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09301769.     00006402
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09267259.     00006403
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09229912.     00006404
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09189577.     00006405
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09146110.     00006406
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09099282.     00006407
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09048963.     00006408
CIDMOD     12  FILLER              PIC S9(09)      VALUE +08994941.     00006409
CIDMOD     12  FILLER              PIC S9(09)      VALUE +08936834.     00006410
CIDMOD     12  FILLER              PIC S9(09)      VALUE +08874276.     00006411
CIDMOD     12  FILLER              PIC S9(09)      VALUE +08806565.     00006412
CIDMOD     12  FILLER              PIC S9(09)      VALUE +08733206.     00006413
CIDMOD     12  FILLER              PIC S9(09)      VALUE +08653472.     00006414
CIDMOD     12  FILLER              PIC S9(09)      VALUE +08566851.     00006415
CIDMOD     12  FILLER              PIC S9(09)      VALUE +08472958.     00006416
CIDMOD     12  FILLER              PIC S9(09)      VALUE +08371537.     00006417
CIDMOD     12  FILLER              PIC S9(09)      VALUE +08262372.     00006418
CIDMOD     12  FILLER              PIC S9(09)      VALUE +08145212.     00006419
CIDMOD     12  FILLER              PIC S9(09)      VALUE +08019613.     00006420
CIDMOD     12  FILLER              PIC S9(09)      VALUE +07884883.     00006421
CIDMOD     12  FILLER              PIC S9(09)      VALUE +07740117.     00006422
CIDMOD     12  FILLER              PIC S9(09)      VALUE +07584386.     00006423
CIDMOD     12  FILLER              PIC S9(09)      VALUE +07416846.     00006424
CIDMOD     12  FILLER              PIC S9(09)      VALUE +07236840.     00006425
CIDMOD     12  FILLER              PIC S9(09)      VALUE +07044195.     00006426
CIDMOD     12  FILLER              PIC S9(09)      VALUE +06838997.     00006427
CIDMOD     12  FILLER              PIC S9(09)      VALUE +06621586.     00006428
CIDMOD     12  FILLER              PIC S9(09)      VALUE +06392148.     00006429
CIDMOD     12  FILLER              PIC S9(09)      VALUE +06150461.     00006430
CIDMOD     12  FILLER              PIC S9(09)      VALUE +05896016.     00006431
CIDMOD     12  FILLER              PIC S9(09)      VALUE +05628160.     00006432
CIDMOD     12  FILLER              PIC S9(09)      VALUE +05346302.     00006433
CIDMOD     12  FILLER              PIC S9(09)      VALUE +05050438.     00006434
CIDMOD     12  FILLER              PIC S9(09)      VALUE +04741856.     00006435
CIDMOD     12  FILLER              PIC S9(09)      VALUE +04422966.     00006436
CIDMOD     12  FILLER              PIC S9(09)      VALUE +04096993.     00006437
CIDMOD     12  FILLER              PIC S9(09)      VALUE +03767718.     00006438
CIDMOD     12  FILLER              PIC S9(09)      VALUE +03438721.     00006439
CIDMOD     12  FILLER              PIC S9(09)      VALUE +03112868.     00006440
CIDMOD     12  FILLER              PIC S9(09)      VALUE +02792429.     00006441
CIDMOD     12  FILLER              PIC S9(09)      VALUE +02479426.     00006442
CIDMOD     12  FILLER              PIC S9(09)      VALUE +02175919.     00006443
CIDMOD     12  FILLER              PIC S9(09)      VALUE +01884694.     00006444
CIDMOD     12  FILLER              PIC S9(09)      VALUE +01609303.     00006445
CIDMOD     12  FILLER              PIC S9(09)      VALUE +01353456.     00006446
CIDMOD     12  FILLER              PIC S9(09)      VALUE +01120377.     00006447
CIDMOD     12  FILLER              PIC S9(09)      VALUE +00912289.     00006448
CIDMOD     12  FILLER              PIC S9(09)      VALUE +00730260.     00006449
CIDMOD     12  FILLER              PIC S9(09)      VALUE +00574211.     00006450
CIDMOD     12  FILLER              PIC S9(09)      VALUE +00443044.     00006451
CIDMOD     12  FILLER              PIC S9(09)      VALUE +00334893.     00006452
CIDMOD     12  FILLER              PIC S9(09)      VALUE +00247342.     00006453
CIDMOD     12  FILLER              PIC S9(09)      VALUE +00177559.     00006454
CIDMOD     12  FILLER              PIC S9(09)      VALUE +00122521.     00006455
CIDMOD     12  FILLER              PIC S9(09)      VALUE +00079411.     00006456
CIDMOD     12  FILLER              PIC S9(09)      VALUE +00045980.     00006457
CIDMOD     12  FILLER              PIC S9(09)      VALUE +00021105.     00006458
CIDMOD     12  FILLER              PIC S9(09)      VALUE +00005379.     00006459
CIDMOD                                                                  00006460
CIDMOD 01  80CSO-TABLE-R  REDEFINES  80CSO-TABLE.                       00006461
CIDMOD     12  80CSO-OCCURS    OCCURS  100  TIMES  COMP-3.              00006462
CIDMOD         16  80CSO-VALUE     PIC S9(09).                          00006463
CIDMOD                                                                  00006460
01613                                  EJECT                            ECS096
01614  01  FILLER                  PIC  X(28)  SYNC    VALUE            ECS096
01615          '***80MSO-TABLE  AGE LAST ***'.                          ECS096
01616                                                                   ECS096
01617  01  80MSO-TABLE     COMP-3.                                      ECS096
01618      12  FILLER              PIC S9(9)       VALUE +09979100.     ECS096
01619      12  FILLER              PIC S9(9)       VALUE +09952873.     ECS096
01620      12  FILLER              PIC S9(9)       VALUE +09942621.     ECS096
01621      12  FILLER              PIC S9(9)       VALUE +09932828.     ECS096
01622      12  FILLER              PIC S9(9)       VALUE +09923242.     ECS096
01623      12  FILLER              PIC S9(9)       VALUE +09914063.     ECS096
01624      12  FILLER              PIC S9(9)       VALUE +09905338.     ECS096
01625      12  FILLER              PIC S9(9)       VALUE +09897117.     ECS096
01626      12  FILLER              PIC S9(9)       VALUE +09889397.     ECS096
01627      12  FILLER              PIC S9(9)       VALUE +09881980.     ECS096
01628      12  FILLER              PIC S9(9)       VALUE +09874717.     ECS096
01629      12  FILLER              PIC S9(9)       VALUE +09867311.     ECS096
01630      12  FILLER              PIC S9(9)       VALUE +09859318.     ECS096
01631      12  FILLER              PIC S9(9)       VALUE +09850023.     ECS096
01632      12  FILLER              PIC S9(9)       VALUE +09839708.     ECS096
01633      12  FILLER              PIC S9(9)       VALUE +09827508.     ECS096
01634      12  FILLER              PIC S9(9)       VALUE +09813553.     ECS096
01635      12  FILLER              PIC S9(9)       VALUE +09797950.     ECS096
01636      12  FILLER              PIC S9(9)       VALUE +09781049.     ECS096
01637      12  FILLER              PIC S9(9)       VALUE +09763248.     ECS096
01638      12  FILLER              PIC S9(9)       VALUE +09744893.     ECS096
01639      12  FILLER              PIC S9(9)       VALUE +09726329.     ECS096
01640      12  FILLER              PIC S9(9)       VALUE +09707849.     ECS096
01641      12  FILLER              PIC S9(9)       VALUE +09689646.     ECS096
01642      12  FILLER              PIC S9(9)       VALUE +09671817.     ECS096
01643      12  FILLER              PIC S9(9)       VALUE +09654455.     ECS096
01644      12  FILLER              PIC S9(9)       VALUE +09637560.     ECS096
01645      12  FILLER              PIC S9(9)       VALUE +09620983.     ECS096
01646      12  FILLER              PIC S9(9)       VALUE +09604579.     ECS096
01647      12  FILLER              PIC S9(9)       VALUE +09588203.     ECS096
01648      12  FILLER              PIC S9(9)       VALUE +09571712.     ECS096
01649      12  FILLER              PIC S9(9)       VALUE +09554914.     ECS096
01650      12  FILLER              PIC S9(9)       VALUE +09537667.     ECS096
01651      12  FILLER              PIC S9(9)       VALUE +09519832.     ECS096
01652      12  FILLER              PIC S9(9)       VALUE +09501222.     ECS096
01653      12  FILLER              PIC S9(9)       VALUE +09481697.     ECS096
01654      12  FILLER              PIC S9(9)       VALUE +09461075.     ECS096
01655      12  FILLER              PIC S9(9)       VALUE +09439126.     ECS096
01656      12  FILLER              PIC S9(9)       VALUE +09415623.     ECS096
01657      12  FILLER              PIC S9(9)       VALUE +09390343.     ECS096
01658      12  FILLER              PIC S9(9)       VALUE +09363066.     ECS096
01659      12  FILLER              PIC S9(9)       VALUE +09333527.     ECS096
01660      12  FILLER              PIC S9(9)       VALUE +09301562.     ECS096
01661      12  FILLER              PIC S9(9)       VALUE +09267009.     ECS096
01662      12  FILLER              PIC S9(9)       VALUE +09229666.     ECS096
01663      12  FILLER              PIC S9(9)       VALUE +09189366.     ECS096
01664      12  FILLER              PIC S9(9)       VALUE +09145828.     ECS096
01665      12  FILLER              PIC S9(9)       VALUE +09099006.     ECS096
01666      12  FILLER              PIC S9(9)       VALUE +09048694.     ECS096
01667      12  FILLER              PIC S9(9)       VALUE +08994634.     ECS096
01668      12  FILLER              PIC S9(9)       VALUE +08936535.     ECS096
01669      12  FILLER              PIC S9(9)       VALUE +08873944.     ECS096
01670      12  FILLER              PIC S9(9)       VALUE +08806246.     ECS096
01671      12  FILLER              PIC S9(9)       VALUE +08732859.     ECS096
01672      12  FILLER              PIC S9(9)       VALUE +08653101.     ECS096
01673      12  FILLER              PIC S9(9)       VALUE +08568459.     ECS096
01674      12  FILLER              PIC S9(9)       VALUE +08472550.     ECS096
01675      12  FILLER              PIC S9(9)       VALUE +08371116.     ECS096
01676      12  FILLER              PIC S9(9)       VALUE +08261986.     ECS096
01677      12  FILLER              PIC S9(9)       VALUE +08144864.     ECS096
01678      12  FILLER              PIC S9(9)       VALUE +08019269.     ECS096
01679      12  FILLER              PIC S9(9)       VALUE +07884512.     ECS096
01680      12  FILLER              PIC S9(9)       VALUE +07739771.     ECS096
01681      12  FILLER              PIC S9(9)       VALUE +07584078.     ECS096
01682      12  FILLER              PIC S9(9)       VALUE +07416554.     ECS096
01683      12  FILLER              PIC S9(9)       VALUE +07236579.     ECS096
01684      12  FILLER              PIC S9(9)       VALUE +07043946.     ECS096
01685      12  FILLER              PIC S9(9)       VALUE +06838779.     ECS096
01686      12  FILLER              PIC S9(9)       VALUE +06621349.     ECS096
01687      12  FILLER              PIC S9(9)       VALUE +06391887.     ECS096
01688      12  FILLER              PIC S9(9)       VALUE +06150214.     ECS096
01689      12  FILLER              PIC S9(9)       VALUE +05895800.     ECS096
01690      12  FILLER              PIC S9(9)       VALUE +05627972.     ECS096
01691      12  FILLER              PIC S9(9)       VALUE +05346100.     ECS096
01692      12  FILLER              PIC S9(9)       VALUE +05055637.     ECS096
01693      12  FILLER              PIC S9(9)       VALUE +04741677.     ECS096
01694      12  FILLER              PIC S9(9)       VALUE +04422776.     ECS096
01695      12  FILLER              PIC S9(9)       VALUE +04096797.     ECS096
01696      12  FILLER              PIC S9(9)       VALUE +03767521.     ECS096
01697      12  FILLER              PIC S9(9)       VALUE +03438547.     ECS096
01698      12  FILLER              PIC S9(9)       VALUE +03112713.     ECS096
01699      12  FILLER              PIC S9(9)       VALUE +02792305.     ECS096
01700      12  FILLER              PIC S9(9)       VALUE +02459322.     ECS096
01701      12  FILLER              PIC S9(9)       VALUE +02175823.     ECS096
01702      12  FILLER              PIC S9(9)       VALUE +01884602.     ECS096
01703      12  FILLER              PIC S9(9)       VALUE +01609222.     ECS096
01704      12  FILLER              PIC S9(9)       VALUE +01353395.     ECS096
01705      12  FILLER              PIC S9(9)       VALUE +01125678.     ECS096
01706      12  FILLER              PIC S9(9)       VALUE +00912244.     ECS096
01707      12  FILLER              PIC S9(9)       VALUE +00730224.     ECS096
01708      12  FILLER              PIC S9(9)       VALUE +00574180.     ECS096
01709      12  FILLER              PIC S9(9)       VALUE +00443022.     ECS096
01710      12  FILLER              PIC S9(9)       VALUE +00334877.     ECS096
01711      12  FILLER              PIC S9(9)       VALUE +00299331.     ECS096
01712      12  FILLER              PIC S9(9)       VALUE +00177551.     ECS096
01713      12  FILLER              PIC S9(9)       VALUE +00122515.     ECS096
01714      12  FILLER              PIC S9(9)       VALUE +00079407.     ECS096
01715      12  FILLER              PIC S9(9)       VALUE +00045977.     ECS096
01716      12  FILLER              PIC S9(9)       VALUE +00021104.     ECS096
01717      12  FILLER              PIC S9(9)       VALUE +00005379.     ECS096
01718                                                                   ECS096
01719  01  80MSO-TABLE-R  REDEFINES  80MSO-TABLE.                       ECS096
01720      12  80MSO-OCCURS    OCCURS  100  TIMES  COMP-3.              ECS096
01721          16  80MSO-VALUE     PIC S9(9).                           ECS096
01722                                  EJECT                            ECS096
01723  01  FILLER                  PIC  X(28)  SYNC    VALUE            ECS096
01724          '***80MSO-TABLE  AGE NEAR ***'.                          ECS096
01725                                                                   ECS096
01726  01  80MSO-AN-TABLE  COMP-3.                                      ECS096
01727      12  FILLER              PIC S9(9)       VALUE +09958200.     ECS096
01728      12  FILLER              PIC S9(9)       VALUE +09947545.     ECS096
01729      12  FILLER              PIC S9(9)       VALUE +09937697.     ECS096
01730      12  FILLER              PIC S9(9)       VALUE +09927958.     ECS096
01731      12  FILLER              PIC S9(9)       VALUE +09918526.     ECS096
01732      12  FILLER              PIC S9(9)       VALUE +09909599.     ECS096
01733      12  FILLER              PIC S9(9)       VALUE +09901077.     ECS096
01734      12  FILLER              PIC S9(9)       VALUE +09893156.     ECS096
01735      12  FILLER              PIC S9(9)       VALUE +09885637.     ECS096
01736      12  FILLER              PIC S9(9)       VALUE +09878322.     ECS096
01737      12  FILLER              PIC S9(9)       VALUE +09871111.     ECS096
01738      12  FILLER              PIC S9(9)       VALUE +09863510.     ECS096
01739      12  FILLER              PIC S9(9)       VALUE +09855126.     ECS096
01740      12  FILLER              PIC S9(9)       VALUE +09845369.     ECS096
01741      12  FILLER              PIC S9(9)       VALUE +09834047.     ECS096
01742      12  FILLER              PIC S9(9)       VALUE +09820968.     ECS096
01743      12  FILLER              PIC S9(9)       VALUE +09806138.     ECS096
01744      12  FILLER              PIC S9(9)       VALUE +09789762.     ECS096
01745      12  FILLER              PIC S9(9)       VALUE +09772336.     ECS096
01746      12  FILLER              PIC S9(9)       VALUE +09754159.     ECS096
01747      12  FILLER              PIC S9(9)       VALUE +09735626.     ECS096
01748      12  FILLER              PIC S9(9)       VALUE +09717031.     ECS096
01749      12  FILLER              PIC S9(9)       VALUE +09698666.     ECS096
01750      12  FILLER              PIC S9(9)       VALUE +09680626.     ECS096
01751      12  FILLER              PIC S9(9)       VALUE +09663007.     ECS096
01752      12  FILLER              PIC S9(9)       VALUE +09645903.     ECS096
01753      12  FILLER              PIC S9(9)       VALUE +09629216.     ECS096
01754      12  FILLER              PIC S9(9)       VALUE +09612750.     ECS096
01755      12  FILLER              PIC S9(9)       VALUE +09596408.     ECS096
01756      12  FILLER              PIC S9(9)       VALUE +09579998.     ECS096
01757      12  FILLER              PIC S9(9)       VALUE +09563425.     ECS096
01758      12  FILLER              PIC S9(9)       VALUE +09546402.     ECS096
01759      12  FILLER              PIC S9(9)       VALUE +09528932.     ECS096
01760      12  FILLER              PIC S9(9)       VALUE +09510732.     ECS096
01761      12  FILLER              PIC S9(9)       VALUE +09491711.     ECS096
01762      12  FILLER              PIC S9(9)       VALUE +09471683.     ECS096
01763      12  FILLER              PIC S9(9)       VALUE +09450466.     ECS096
01764      12  FILLER              PIC S9(9)       VALUE +09427785.     ECS096
01765      12  FILLER              PIC S9(9)       VALUE +09403461.     ECS096
01766      12  FILLER              PIC S9(9)       VALUE +09377225.     ECS096
01767      12  FILLER              PIC S9(9)       VALUE +09348906.     ECS096
01768      12  FILLER              PIC S9(9)       VALUE +09318148.     ECS096
01769      12  FILLER              PIC S9(9)       VALUE +09284975.     ECS096
01770      12  FILLER              PIC S9(9)       VALUE +09249042.     ECS096
01771      12  FILLER              PIC S9(9)       VALUE +09210289.     ECS096
01772      12  FILLER              PIC S9(9)       VALUE +09168382.     ECS096
01773      12  FILLER              PIC S9(9)       VALUE +09123274.     ECS096
01774      12  FILLER              PIC S9(9)       VALUE +09074738.     ECS096
01775      12  FILLER              PIC S9(9)       VALUE +09022649.     ECS096
01776      12  FILLER              PIC S9(9)       VALUE +08966618.     ECS096
01777      12  FILLER              PIC S9(9)       VALUE +08906452.     ECS096
01778      12  FILLER              PIC S9(9)       VALUE +08841435.     ECS096
01779      12  FILLER              PIC S9(9)       VALUE +08771057.     ECS096
01780      12  FILLER              PIC S9(9)       VALUE +08694661.     ECS096
01781      12  FILLER              PIC S9(9)       VALUE +08611540.     ECS096
01782      12  FILLER              PIC S9(9)       VALUE +08521377.     ECS096
01783      12  FILLER              PIC S9(9)       VALUE +08423722.     ECS096
01784      12  FILLER              PIC S9(9)       VALUE +08318510.     ECS096
01785      12  FILLER              PIC S9(9)       VALUE +08205461.     ECS096
01786      12  FILLER              PIC S9(9)       VALUE +08084266.     ECS096
01787      12  FILLER              PIC S9(9)       VALUE +07954271.     ECS096
01788      12  FILLER              PIC S9(9)       VALUE +07814753.     ECS096
01789      12  FILLER              PIC S9(9)       VALUE +07664788.     ECS096
01790      12  FILLER              PIC S9(9)       VALUE +07503368.     ECS096
01791      12  FILLER              PIC S9(9)       VALUE +07329740.     ECS096
01792      12  FILLER              PIC S9(9)       VALUE +07143418.     ECS096
01793      12  FILLER              PIC S9(9)       VALUE +06944474.     ECS096
01794      12  FILLER              PIC S9(9)       VALUE +06733084.     ECS096
01795      12  FILLER              PIC S9(9)       VALUE +06509613.     ECS096
01796      12  FILLER              PIC S9(9)       VALUE +06274160.     ECS096
01797      12  FILLER              PIC S9(9)       VALUE +06026268.     ECS096
01798      12  FILLER              PIC S9(9)       VALUE +05765331.     ECS096
01799      12  FILLER              PIC S9(9)       VALUE +05490613.     ECS096
01800      12  FILLER              PIC S9(9)       VALUE +05201587.     ECS096
01801      12  FILLER              PIC S9(9)       VALUE +04898907.     ECS096
01802      12  FILLER              PIC S9(9)       VALUE +04584446.     ECS096
01803      12  FILLER              PIC S9(9)       VALUE +04261105.     ECS096
01804      12  FILLER              PIC S9(9)       VALUE +03932489.     ECS096
01805      12  FILLER              PIC S9(9)       VALUE +03602553.     ECS096
01806      12  FILLER              PIC S9(9)       VALUE +03274541.     ECS096
01807      12  FILLER              PIC S9(9)       VALUE +02950885.     ECS096
01808      12  FILLER              PIC S9(9)       VALUE +02633724.     ECS096
01809      12  FILLER              PIC S9(9)       VALUE +02324920.     ECS096
01810      12  FILLER              PIC S9(9)       VALUE +02026726.     ECS096
01811      12  FILLER              PIC S9(9)       VALUE +01742478.     ECS096
01812      12  FILLER              PIC S9(9)       VALUE +01475966.     ECS096
01813      12  FILLER              PIC S9(9)       VALUE +01230823.     ECS096
01814      12  FILLER              PIC S9(9)       VALUE +01009829.     ECS096
01815      12  FILLER              PIC S9(9)       VALUE +00814659.     ECS096
01816      12  FILLER              PIC S9(9)       VALUE +00645788.     ECS096
01817      12  FILLER              PIC S9(9)       VALUE +00502572.     ECS096
01818      12  FILLER              PIC S9(9)       VALUE +00383472.     ECS096
01819      12  FILLER              PIC S9(9)       VALUE +00286281.     ECS096
01820      12  FILLER              PIC S9(9)       VALUE +00208381.     ECS096
01821      12  FILLER              PIC S9(9)       VALUE +00146721.     ECS096
01822      12  FILLER              PIC S9(9)       VALUE +00098309.     ECS096
01823      12  FILLER              PIC S9(9)       VALUE +00060504.     ECS096
01824      12  FILLER              PIC S9(9)       VALUE +00031450.     ECS096
01825      12  FILLER              PIC S9(9)       VALUE +00010757.     ECS096
01826      12  FILLER              PIC S9(9)       VALUE +00000000.     ECS096
01827                                                                   ECS096
01828  01  80MSO-TABLE-AN-R  REDEFINES  80MSO-AN-TABLE.                 ECS096
01829      12  80MSO-AN-OCCURS    OCCURS  100  TIMES  COMP-3.           ECS096
01830          16  80MSO-AN-VALUE  PIC S9(9).                           ECS096
01831                                  EJECT                            ECS096
01832  01  FILLER                  PIC  X(27)  SYNC    VALUE            ECS096
01833          '***80FSO-TABLE AGE LAST ***'.                           ECS096
01834                                                                   ECS096
01835  01  80FSO-TABLE     COMP-3.                                      ECS096
01836      12  FILLER              PIC S9(9)       VALUE +09985550.     ECS096
01837      12  FILLER              PIC S9(9)       VALUE +09966763.     ECS096
01838      12  FILLER              PIC S9(9)       VALUE +09958390.     ECS096
01839      12  FILLER              PIC S9(9)       VALUE +09950423.     ECS096
01840      12  FILLER              PIC S9(9)       VALUE +09942662.     ECS096
01841      12  FILLER              PIC S9(9)       VALUE +09935055.     ECS096
01842      12  FILLER              PIC S9(9)       VALUE +09927653.     ECS096
01843      12  FILLER              PIC S9(9)       VALUE +09920456.     ECS096
01844      12  FILLER              PIC S9(9)       VALUE +09913412.     ECS096
01845      12  FILLER              PIC S9(9)       VALUE +09906522.     ECS096
01846      12  FILLER              PIC S9(9)       VALUE +09899736.     ECS096
01847      12  FILLER              PIC S9(9)       VALUE +09892955.     ECS096
01848      12  FILLER              PIC S9(9)       VALUE +09885981.     ECS096
01849      12  FILLER              PIC S9(9)       VALUE +09878715.     ECS096
01850      12  FILLER              PIC S9(9)       VALUE +09871059.     ECS096
01851      12  FILLER              PIC S9(9)       VALUE +09862916.     ECS096
01852      12  FILLER              PIC S9(9)       VALUE +09854286.     ECS096
01853      12  FILLER              PIC S9(9)       VALUE +09845171.     ECS096
01854      12  FILLER              PIC S9(9)       VALUE +09835670.     ECS096
01855      12  FILLER              PIC S9(9)       VALUE +09825835.     ECS096
01856      12  FILLER              PIC S9(9)       VALUE +09815764.     ECS096
01857      12  FILLER              PIC S9(9)       VALUE +09805458.     ECS096
01858      12  FILLER              PIC S9(9)       VALUE +09794868.     ECS096
01859      12  FILLER              PIC S9(9)       VALUE +09784094.     ECS096
01860      12  FILLER              PIC S9(9)       VALUE +09773087.     ECS096
01861      12  FILLER              PIC S9(9)       VALUE +09761848.     ECS096
01862      12  FILLER              PIC S9(9)       VALUE +09750378.     ECS096
01863      12  FILLER              PIC S9(9)       VALUE +09738629.     ECS096
01864      12  FILLER              PIC S9(9)       VALUE +09726554.     ECS096
01865      12  FILLER              PIC S9(9)       VALUE +09714104.     ECS096
01866      12  FILLER              PIC S9(9)       VALUE +09701232.     ECS096
01867      12  FILLER              PIC S9(9)       VALUE +09687893.     ECS096
01868      12  FILLER              PIC S9(9)       VALUE +09674087.     ECS096
01869      12  FILLER              PIC S9(9)       VALUE +09659818.     ECS096
01870      12  FILLER              PIC S9(9)       VALUE +09644942.     ECS096
01871      12  FILLER              PIC S9(9)       VALUE +09629365.     ECS096
01872      12  FILLER              PIC S9(9)       VALUE +09612947.     ECS096
01873      12  FILLER              PIC S9(9)       VALUE +09595404.     ECS096
01874      12  FILLER              PIC S9(9)       VALUE +09576550.     ECS096
01875      12  FILLER              PIC S9(9)       VALUE +09556153.     ECS096
01876      12  FILLER              PIC S9(9)       VALUE +09533984.     ECS096
01877      12  FILLER              PIC S9(9)       VALUE +09509865.     ECS096
01878      12  FILLER              PIC S9(9)       VALUE +09483667.     ECS096
01879      12  FILLER              PIC S9(9)       VALUE +09455407.     ECS096
01880      12  FILLER              PIC S9(9)       VALUE +09425105.     ECS096
01881      12  FILLER              PIC S9(9)       VALUE +09392684.     ECS096
01882      12  FILLER              PIC S9(9)       VALUE +09358121.     ECS096
01883      12  FILLER              PIC S9(9)       VALUE +09321392.     ECS096
01884      12  FILLER              PIC S9(9)       VALUE +09282338.     ECS096
01885      12  FILLER              PIC S9(9)       VALUE +09240756.     ECS096
01886      12  FILLER              PIC S9(9)       VALUE +09196450.     ECS096
01887      12  FILLER              PIC S9(9)       VALUE +09149230.     ECS096
01888      12  FILLER              PIC S9(9)       VALUE +09098868.     ECS096
01889      12  FILLER              PIC S9(9)       VALUE +09044963.     ECS096
01890      12  FILLER              PIC S9(9)       VALUE +08987263.     ECS096
01891      12  FILLER              PIC S9(9)       VALUE +08925707.     ECS096
01892      12  FILLER              PIC S9(9)       VALUE +08860290.     ECS096
01893      12  FILLER              PIC S9(9)       VALUE +08791187.     ECS096
01894      12  FILLER              PIC S9(9)       VALUE +08718668.     ECS096
01895      12  FILLER              PIC S9(9)       VALUE +08642781.     ECS096
01896      12  FILLER              PIC S9(9)       VALUE +08563234.     ECS096
01897      12  FILLER              PIC S9(9)       VALUE +08479328.     ECS096
01898      12  FILLER              PIC S9(9)       VALUE +08389931.     ECS096
01899      12  FILLER              PIC S9(9)       VALUE +08293556.     ECS096
01900      12  FILLER              PIC S9(9)       VALUE +08188798.     ECS096
01901      12  FILLER              PIC S9(9)       VALUE +08074846.     ECS096
01902      12  FILLER              PIC S9(9)       VALUE +07951383.     ECS096
01903      12  FILLER              PIC S9(9)       VALUE +07818522.     ECS096
01904      12  FILLER              PIC S9(9)       VALUE +07676781.     ECS096
01905      12  FILLER              PIC S9(9)       VALUE +07526372.     ECS096
01906      12  FILLER              PIC S9(9)       VALUE +07366617.     ECS096
01907      12  FILLER              PIC S9(9)       VALUE +07196019.     ECS096
01908      12  FILLER              PIC S9(9)       VALUE +07012277.     ECS096
01909      12  FILLER              PIC S9(9)       VALUE +06812652.     ECS096
01910      12  FILLER              PIC S9(9)       VALUE +06594710.     ECS096
01911      12  FILLER              PIC S9(9)       VALUE +06356985.     ECS096
01912      12  FILLER              PIC S9(9)       VALUE +06099153.     ECS096
01913      12  FILLER              PIC S9(9)       VALUE +05821950.     ECS096
01914      12  FILLER              PIC S9(9)       VALUE +05526903.     ECS096
01915      12  FILLER              PIC S9(9)       VALUE +05215633.     ECS096
01916      12  FILLER              PIC S9(9)       VALUE +04889298.     ECS096
01917      12  FILLER              PIC S9(9)       VALUE +04548684.     ECS096
01918      12  FILLER              PIC S9(9)       VALUE +04194651.     ECS096
01919      12  FILLER              PIC S9(9)       VALUE +03828679.     ECS096
01920      12  FILLER              PIC S9(9)       VALUE +03453865.     ECS096
01921      12  FILLER              PIC S9(9)       VALUE +03075257.     ECS096
01922      12  FILLER              PIC S9(9)       VALUE +02699188.     ECS096
01923      12  FILLER              PIC S9(9)       VALUE +02332584.     ECS096
01924      12  FILLER              PIC S9(9)       VALUE +01982285.     ECS096
01925      12  FILLER              PIC S9(9)       VALUE +01654448.     ECS096
01926      12  FILLER              PIC S9(9)       VALUE +01354093.     ECS096
01927      12  FILLER              PIC S9(9)       VALUE +01084825.     ECS096
01928      12  FILLER              PIC S9(9)       VALUE +00848864.     ECS096
01929      12  FILLER              PIC S9(9)       VALUE +00646108.     ECS096
01930      12  FILLER              PIC S9(9)       VALUE +00475916.     ECS096
01931      12  FILLER              PIC S9(9)       VALUE +00335411.     ECS096
01932      12  FILLER              PIC S9(9)       VALUE +00221029.     ECS096
01933      12  FILLER              PIC S9(9)       VALUE +00129550.     ECS096
01934      12  FILLER              PIC S9(9)       VALUE +00059951.     ECS096
01935      12  FILLER              PIC S9(9)       VALUE +00015350.     ECS096
01936                                                                   ECS096
01937  01  80FSO-TABLE-R REDEFINES 80FSO-TABLE.                         ECS096
01938      12  80FSO-OCCURS    OCCURS  100  TIMES  COMP-3.              ECS096
01939          16  80FSO-VALUE     PIC S9(9).                           ECS096
01940                                  EJECT                            ECS096
01941  01  FILLER                  PIC  X(27)  SYNC    VALUE            ECS096
01942          '***80FSO-TABLE AGE NEAR ***'.                           ECS096
01943                                                                   ECS096
01944  01  80FSO-AN-TABLE  COMP-3.                                      ECS096
01945      12  FILLER              PIC S9(9)       VALUE +09971100.     ECS096
01946      12  FILLER              PIC S9(9)       VALUE +09962425.     ECS096
01947      12  FILLER              PIC S9(9)       VALUE +09962425.     ECS096
01948      12  FILLER              PIC S9(9)       VALUE +09946491.     ECS096
01949      12  FILLER              PIC S9(9)       VALUE +09946491.     ECS096
01950      12  FILLER              PIC S9(9)       VALUE +09931278.     ECS096
01951      12  FILLER              PIC S9(9)       VALUE +09924028.     ECS096
01952      12  FILLER              PIC S9(9)       VALUE +09924028.     ECS096
01953      12  FILLER              PIC S9(9)       VALUE +09916883.     ECS096
01954      12  FILLER              PIC S9(9)       VALUE +09909941.     ECS096
01955      12  FILLER              PIC S9(9)       VALUE +09903103.     ECS096
01956      12  FILLER              PIC S9(9)       VALUE +09896369.     ECS096
01957      12  FILLER              PIC S9(9)       VALUE +09889541.     ECS096
01958      12  FILLER              PIC S9(9)       VALUE +09882421.     ECS096
01959      12  FILLER              PIC S9(9)       VALUE +09875009.     ECS096
01960      12  FILLER              PIC S9(9)       VALUE +09867109.     ECS096
01961      12  FILLER              PIC S9(9)       VALUE +09858722.     ECS096
01962      12  FILLER              PIC S9(9)       VALUE +09849849.     ECS096
01963      12  FILLER              PIC S9(9)       VALUE +09840492.     ECS096
01964      12  FILLER              PIC S9(9)       VALUE +09830848.     ECS096
01965      12  FILLER              PIC S9(9)       VALUE +09820821.     ECS096
01966      12  FILLER              PIC S9(9)       VALUE +09810509.     ECS096
01967      12  FILLER              PIC S9(9)       VALUE +09800012.     ECS096
01968      12  FILLER              PIC S9(9)       VALUE +09789330.     ECS096
01969      12  FILLER              PIC S9(9)       VALUE +09778464.     ECS096
01970      12  FILLER              PIC S9(9)       VALUE +09767317.     ECS096
01971      12  FILLER              PIC S9(9)       VALUE +09755987.     ECS096
01972      12  FILLER              PIC S9(9)       VALUE +09744377.     ECS096
01973      12  FILLER              PIC S9(9)       VALUE +09732489.     ECS096
01974      12  FILLER              PIC S9(9)       VALUE +09720226.     ECS096
01975      12  FILLER              PIC S9(9)       VALUE +09707590.     ECS096
01976      12  FILLER              PIC S9(9)       VALUE +09694485.     ECS096
01977      12  FILLER              PIC S9(9)       VALUE +09680913.     ECS096
01978      12  FILLER              PIC S9(9)       VALUE +09666876.     ECS096
01979      12  FILLER              PIC S9(9)       VALUE +09652376.     ECS096
01980      12  FILLER              PIC S9(9)       VALUE +09637125.     ECS096
01981      12  FILLER              PIC S9(9)       VALUE +09621224.     ECS096
01982      12  FILLER              PIC S9(9)       VALUE +09604291.     ECS096
01983      12  FILLER              PIC S9(9)       VALUE +09586139.     ECS096
01984      12  FILLER              PIC S9(9)       VALUE +09566583.     ECS096
01985      12  FILLER              PIC S9(9)       VALUE +09545345.     ECS096
01986      12  FILLER              PIC S9(9)       VALUE +09522245.     ECS096
01987      12  FILLER              PIC S9(9)       VALUE +09497106.     ECS096
01988      12  FILLER              PIC S9(9)       VALUE +09469849.     ECS096
01989      12  FILLER              PIC S9(9)       VALUE +09440587.     ECS096
01990      12  FILLER              PIC S9(9)       VALUE +09409244.     ECS096
01991      12  FILLER              PIC S9(9)       VALUE +09375747.     ECS096
01992      12  FILLER              PIC S9(9)       VALUE +09340119.     ECS096
01993      12  FILLER              PIC S9(9)       VALUE +09302292.     ECS096
01994      12  FILLER              PIC S9(9)       VALUE +09262013.     ECS096
01995      12  FILLER              PIC S9(9)       VALUE +09219130.     ECS096
01996      12  FILLER              PIC S9(9)       VALUE +09173403.     ECS096
01997      12  FILLER              PIC S9(9)       VALUE +09124692.     ECS096
01998      12  FILLER              PIC S9(9)       VALUE +09072681.     ECS096
01999      12  FILLER              PIC S9(9)       VALUE +09016884.     ECS096
02000      12  FILLER              PIC S9(9)       VALUE +08957282.     ECS096
02001      12  FILLER              PIC S9(9)       VALUE +08893775.     ECS096
02002      12  FILLER              PIC S9(9)       VALUE +08826449.     ECS096
02003      12  FILLER              PIC S9(9)       VALUE +08755573.     ECS096
02004      12  FILLER              PIC S9(9)       VALUE +08681413.     ECS096
02005      12  FILLER              PIC S9(9)       VALUE +08603801.     ECS096
02006      12  FILLER              PIC S9(9)       VALUE +08522323.     ECS096
02007      12  FILLER              PIC S9(9)       VALUE +08435992.     ECS096
02008      12  FILLER              PIC S9(9)       VALUE +08343534.     ECS096
02009      12  FILLER              PIC S9(9)       VALUE +08243245.     ECS096
02010      12  FILLER              PIC S9(9)       VALUE +08134022.     ECS096
02011      12  FILLER              PIC S9(9)       VALUE +08015347.     ECS096
02012      12  FILLER              PIC S9(9)       VALUE +07887101.     ECS096
02013      12  FILLER              PIC S9(9)       VALUE +07749629.     ECS096
02014      12  FILLER              PIC S9(9)       VALUE +07603626.     ECS096
02015      12  FILLER              PIC S9(9)       VALUE +07448816.     ECS096
02016      12  FILLER              PIC S9(9)       VALUE +07284123.     ECS096
02017      12  FILLER              PIC S9(9)       VALUE +07107629.     ECS096
02018      12  FILLER              PIC S9(9)       VALUE +06916647.     ECS096
02019      12  FILLER              PIC S9(9)       VALUE +06708387.     ECS096
02020      12  FILLER              PIC S9(9)       VALUE +06480771.     ECS096
02021      12  FILLER              PIC S9(9)       VALUE +06232946.     ECS096
02022      12  FILLER              PIC S9(9)       VALUE +05965116.     ECS096
02023      12  FILLER              PIC S9(9)       VALUE +05678552.     ECS096
02024      12  FILLER              PIC S9(9)       VALUE +05375033.     ECS096
02025      12  FILLER              PIC S9(9)       VALUE +05056025.     ECS096
02026      12  FILLER              PIC S9(9)       VALUE +04722378.     ECS096
02027      12  FILLER              PIC S9(9)       VALUE +04374811.     ECS096
02028      12  FILLER              PIC S9(9)       VALUE +04014327.     ECS096
02029      12  FILLER              PIC S9(9)       VALUE +03642881.     ECS096
02030      12  FILLER              PIC S9(9)       VALUE +03264714.     ECS096
02031      12  FILLER              PIC S9(9)       VALUE +02885681.     ECS096
02032      12  FILLER              PIC S9(9)       VALUE +02512591.     ECS096
02033      12  FILLER              PIC S9(9)       VALUE +02152486.     ECS096
02034      12  FILLER              PIC S9(9)       VALUE +01812006.     ECS096
02035      12  FILLER              PIC S9(9)       VALUE +01496826.     ECS096
02036      12  FILLER              PIC S9(9)       VALUE +01211306.     ECS096
02037      12  FILLER              PIC S9(9)       VALUE +00958301.     ECS096
02038      12  FILLER              PIC S9(9)       VALUE +00739032.     ECS096
02039      12  FILLER              PIC S9(9)       VALUE +00553158.     ECS096
02040      12  FILLER              PIC S9(9)       VALUE +00398655.     ECS096
02041      12  FILLER              PIC S9(9)       VALUE +00272154.     ECS096
02042      12  FILLER              PIC S9(9)       VALUE +00169895.     ECS096
02043      12  FILLER              PIC S9(9)       VALUE +00089200.     ECS096
02044      12  FILLER              PIC S9(9)       VALUE +00030698.     ECS096
02045      12  FILLER              PIC S9(9)       VALUE +00000000.     ECS096
02046                                                                   ECS096
02047  01  80FSO-AN-TABLE-R REDEFINES 80FSO-AN-TABLE.                   ECS096
02048      12  80FSO-AN-OCCURS OCCURS  100  TIMES  COMP-3.              ECS096
02049          16  80FSO-AN-VALUE  PIC S9(9).                           ECS096
02050                                  EJECT                            ECS096
02051  01  FILLER                  PIC  X(16)  SYNC    VALUE            ECS096
02052          '***80MET-TABLE**'.                                      ECS096
02053                                                                   ECS096
02054  01  80MET-TABLE     COMP-3.                                      ECS096
CIDMOD     12  FILLER              PIC S9(9)       VALUE +10000000.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +09965800.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +09948061.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +09930751.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +09913670.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +09897015.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +09880883.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +09865271.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +09850178.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +09835402.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +09820748.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +09806016.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +09790719.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +09774369.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +09756579.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +09737164.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +09716034.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +09693298.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +09669356.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +09644506.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +09619141.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +09593650.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +09568227.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +09543062.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +09518346.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +09494074.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +09470339.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +09446947.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +09423708.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +09400620.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +09377400.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +09353957.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +09330104.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +09305659.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +09280534.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +09254548.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +09227525.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +09199197.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +09169391.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +09137482.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +09103033.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +09065711.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +09025368.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +08981866.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +08934801.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +08884052.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +08829415.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +08770611.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +08707550.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +08639979.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +08567404.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +08489440.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +08405225.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +08314196.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +08215507.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +08108623.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +07993075.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +07868703.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +07735329.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +07592766.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +07440531.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +07278030.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +07104304.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +06918455.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +06719757.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +06507749.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +06282516.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +06044597.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +05794773.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +05533719.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +05261737.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +04978760.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +04684715.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +04379740.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +04064661.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +03741805.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +03414659.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +03087501.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +02764919.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +02451045.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +02149101.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +01861508.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +01590249.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +01337193.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +01104535.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +00894717.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +00709806.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +00550902.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +00417887.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +00309491.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +00223514.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +00157140.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +00107273.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +00070815.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +00044842.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +00026773.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +00014526.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +00006576.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +00001951.
CIDMOD     12  FILLER              PIC S9(9)       VALUE +00000061.
02155                                                                   ECS096
02156  01  80MET-TABLE-R  REDEFINES  80MET-TABLE.                       ECS096
02157      12  80MET-OCCURS    OCCURS  100  TIMES  COMP-3.              ECS096
02158          16  80MET-VALUE     PIC S9(9).                           ECS096
02159                                                                   ECS096
02160                                  EJECT                            ECS096
02161  01  FILLER                  PIC  X(16)  SYNC    VALUE            ECS096
02162          '***80FET-TABLE**'.                                      ECS096
02163                                                                   ECS096
02164  01  80FET-TABLE     COMP-3.                                      ECS096
02165      12  FILLER              PIC S9(9)       VALUE +00892572.     ECS096
02166      12  FILLER              PIC S9(9)       VALUE +00889216.     ECS096
02167      12  FILLER              PIC S9(9)       VALUE +00887775.     ECS096
02168      12  FILLER              PIC S9(9)       VALUE +00886390.     ECS096
02169      12  FILLER              PIC S9(9)       VALUE +00885025.     ECS096
02170      12  FILLER              PIC S9(9)       VALUE +00883680.     ECS096
02171      12  FILLER              PIC S9(9)       VALUE +00882346.     ECS096
02172      12  FILLER              PIC S9(9)       VALUE +00881040.     ECS096
02173      12  FILLER              PIC S9(9)       VALUE +00879745.     ECS096
02174      12  FILLER              PIC S9(9)       VALUE +00878469.     ECS096
02175      12  FILLER              PIC S9(9)       VALUE +00877204.     ECS096
02176      12  FILLER              PIC S9(9)       VALUE +00875950.     ECS096
02177      12  FILLER              PIC S9(9)       VALUE +00874689.     ECS096
02178      12  FILLER              PIC S9(9)       VALUE +00873403.     ECS096
02179      12  FILLER              PIC S9(9)       VALUE +00872093.     ECS096
02180      12  FILLER              PIC S9(9)       VALUE +00870741.     ECS096
02181      12  FILLER              PIC S9(9)       VALUE +00869348.     ECS096
02182      12  FILLER              PIC S9(9)       VALUE +00867914.     ECS096
02183      12  FILLER              PIC S9(9)       VALUE +00866439.     ECS096
02184      12  FILLER              PIC S9(9)       VALUE +00864940.     ECS096
02185      12  FILLER              PIC S9(9)       VALUE +00863409.     ECS096
02186      12  FILLER              PIC S9(9)       VALUE +00861855.     ECS096
02187      12  FILLER              PIC S9(9)       VALUE +00860286.     ECS096
02188      12  FILLER              PIC S9(9)       VALUE +00858703.     ECS096
02189      12  FILLER              PIC S9(9)       VALUE +00857106.     ECS096
02190      12  FILLER              PIC S9(9)       VALUE +00855486.     ECS096
02191      12  FILLER              PIC S9(9)       VALUE +00853852.     ECS096
02192      12  FILLER              PIC S9(9)       VALUE +00852196.     ECS096
02193      12  FILLER              PIC S9(9)       VALUE +00850517.     ECS096
02194      12  FILLER              PIC S9(9)       VALUE +00848807.     ECS096
02195      12  FILLER              PIC S9(9)       VALUE +00847067.     ECS096
02196      12  FILLER              PIC S9(9)       VALUE +00845288.     ECS096
02197      12  FILLER              PIC S9(9)       VALUE +00843471.     ECS096
02198      12  FILLER              PIC S9(9)       VALUE +00841615.     ECS096
02199      12  FILLER              PIC S9(9)       VALUE +00839721.     ECS096
02200      12  FILLER              PIC S9(9)       VALUE +00837764.     ECS096
02201      12  FILLER              PIC S9(9)       VALUE +00835753.     ECS096
02202      12  FILLER              PIC S9(9)       VALUE +00833655.     ECS096
02203      12  FILLER              PIC S9(9)       VALUE +00831454.     ECS096
02204      12  FILLER              PIC S9(9)       VALUE +00829134.     ECS096
02205      12  FILLER              PIC S9(9)       VALUE +00826671.     ECS096
02206      12  FILLER              PIC S9(9)       VALUE +00824050.     ECS096
02207      12  FILLER              PIC S9(9)       VALUE +00821224.     ECS096
02208      12  FILLER              PIC S9(9)       VALUE +00818161.     ECS096
02209      12  FILLER              PIC S9(9)       VALUE +00814872.     ECS096
02210      12  FILLER              PIC S9(9)       VALUE +00811352.     ECS096
02211      12  FILLER              PIC S9(9)       VALUE +00807595.     ECS096
02212      12  FILLER              PIC S9(9)       VALUE +00803605.     ECS096
02213      12  FILLER              PIC S9(9)       VALUE +00799370.     ECS096
02214      12  FILLER              PIC S9(9)       VALUE +00794870.     ECS096
02215      12  FILLER              PIC S9(9)       VALUE +00790085.     ECS096
02216      12  FILLER              PIC S9(9)       VALUE +00784989.     ECS096
02217      12  FILLER              PIC S9(9)       VALUE +00779573.     ECS096
02218      12  FILLER              PIC S9(9)       VALUE +00773796.     ECS096
02219      12  FILLER              PIC S9(9)       VALUE +00767606.     ECS096
02220      12  FILLER              PIC S9(9)       VALUE +00761012.     ECS096
02221      12  FILLER              PIC S9(9)       VALUE +00753995.     ECS096
02222      12  FILLER              PIC S9(9)       VALUE +00746576.     ECS096
02223      12  FILLER              PIC S9(9)       VALUE +00738782.     ECS096
02224      12  FILLER              PIC S9(9)       VALUE +00730648.     ECS096
02225      12  FILLER              PIC S9(9)       VALUE +00722158.     ECS096
02226      12  FILLER              PIC S9(9)       VALUE +00713268.     ECS096
02227      12  FILLER              PIC S9(9)       VALUE +00703874.     ECS096
02228      12  FILLER              PIC S9(9)       VALUE +00693844.     ECS096
02229      12  FILLER              PIC S9(9)       VALUE +00682999.     ECS096
02230      12  FILLER              PIC S9(9)       VALUE +00671231.     ECS096
02231      12  FILLER              PIC S9(9)       VALUE +00658498.     ECS096
02232      12  FILLER              PIC S9(9)       VALUE +00644801.     ECS096
02233      12  FILLER              PIC S9(9)       VALUE +00630190.     ECS096
02234      12  FILLER              PIC S9(9)       VALUE +00614757.     ECS096
02235      12  FILLER              PIC S9(9)       VALUE +00598484.     ECS096
02236      12  FILLER              PIC S9(9)       VALUE +00581284.     ECS096
02237      12  FILLER              PIC S9(9)       VALUE +00562974.     ECS096
02238      12  FILLER              PIC S9(9)       VALUE +00543309.     ECS096
02239      12  FILLER              PIC S9(9)       VALUE +00522044.     ECS096
02240      12  FILLER              PIC S9(9)       VALUE +00499017.     ECS096
02241      12  FILLER              PIC S9(9)       VALUE +00474211.     ECS096
02242      12  FILLER              PIC S9(9)       VALUE +00447722.     ECS096
02243      12  FILLER              PIC S9(9)       VALUE +00419762.     ECS096
02244      12  FILLER              PIC S9(9)       VALUE +00390593.     ECS096
02245      12  FILLER              PIC S9(9)       VALUE +00360455.     ECS096
02246      12  FILLER              PIC S9(9)       VALUE +00329532.     ECS096
02247      12  FILLER              PIC S9(9)       VALUE +00298002.     ECS096
02248      12  FILLER              PIC S9(9)       VALUE +00266080.     ECS096
02249      12  FILLER              PIC S9(9)       VALUE +00234073.     ECS096
02250      12  FILLER              PIC S9(9)       VALUE +00202485.     ECS096
02251      12  FILLER              PIC S9(9)       VALUE +00171924.     ECS096
02252      12  FILLER              PIC S9(9)       VALUE +00143027.     ECS096
02253      12  FILLER              PIC S9(9)       VALUE +00116378.     ECS096
02254      12  FILLER              PIC S9(9)       VALUE +00092447.     ECS096
02255      12  FILLER              PIC S9(9)       VALUE +00071543.     ECS096
02256      12  FILLER              PIC S9(9)       VALUE +00053802.     ECS096
02257      12  FILLER              PIC S9(9)       VALUE +00039193.     ECS096
02258      12  FILLER              PIC S9(9)       VALUE +00027535.     ECS096
02259      12  FILLER              PIC S9(9)       VALUE +00018532.     ECS096
02260      12  FILLER              PIC S9(9)       VALUE +00011803.     ECS096
02261      12  FILLER              PIC S9(9)       VALUE +00006934.     ECS096
02262      12  FILLER              PIC S9(9)       VALUE +00003547.     ECS096
02263      12  FILLER              PIC S9(9)       VALUE +00001357.     ECS096
02264      12  FILLER              PIC S9(9)       VALUE +00000200.     ECS096
02265                                                                   ECS096
02266  01  80FET-TABLE-R  REDEFINES  80FET-TABLE.                       ECS096
02267      12  80FET-OCCURS    OCCURS  100  TIMES  COMP-3.              ECS096
02268          16  80FET-VALUE     PIC S9(9).                           ECS096
02269                                                                   ECS096
02270                                  EJECT                            ECS096
02271  01  FILLER                  PIC  X(16)  SYNC    VALUE            ECS096
02272          '***80GBT-TABLE**'.                                      ECS096
02273                                                                   ECS096
02274  01  80GBT-TABLE     COMP-3.                                      ECS096
02275      12  FILLER              PIC S9(9)       VALUE +10000000.     ECS096
02276      12  FILLER              PIC S9(9)       VALUE +09965500.     ECS096
02277      12  FILLER              PIC S9(9)       VALUE +09948060.     ECS096
02278      12  FILLER              PIC S9(9)       VALUE +09931149.     ECS096
02279      12  FILLER              PIC S9(9)       VALUE +09914464.     ECS096
02280      12  FILLER              PIC S9(9)       VALUE +09898205.     ECS096
02281      12  FILLER              PIC S9(9)       VALUE +09882367.     ECS096
02282      12  FILLER              PIC S9(9)       VALUE +09866951.     ECS096
02283      12  FILLER              PIC S9(9)       VALUE +09851953.     ECS096
02284      12  FILLER              PIC S9(9)       VALUE +09837274.     ECS096
02285      12  FILLER              PIC S9(9)       VALUE +09822715.     ECS096
02286      12  FILLER              PIC S9(9)       VALUE +09808177.     ECS096
02287      12  FILLER              PIC S9(9)       VALUE +09793072.     ECS096
02288      12  FILLER              PIC S9(9)       VALUE +09777110.     ECS096
02289      12  FILLER              PIC S9(9)       VALUE +09759902.     ECS096
02290      12  FILLER              PIC S9(9)       VALUE +09741261.     ECS096
02291      12  FILLER              PIC S9(9)       VALUE +09721194.     ECS096
02292      12  FILLER              PIC S9(9)       VALUE +09699710.     ECS096
02293      12  FILLER              PIC S9(9)       VALUE +09677206.     ECS096
02294      12  FILLER              PIC S9(9)       VALUE +09653981.     ECS096
02295      12  FILLER              PIC S9(9)       VALUE +09630232.     ECS096
02296      12  FILLER              PIC S9(9)       VALUE +09606253.     ECS096
02297      12  FILLER              PIC S9(9)       VALUE +09582334.     ECS096
02298      12  FILLER              PIC S9(9)       VALUE +09558665.     ECS096
02299      12  FILLER              PIC S9(9)       VALUE +09535246.     ECS096
02300      12  FILLER              PIC S9(9)       VALUE +09512171.     ECS096
02301      12  FILLER              PIC S9(9)       VALUE +09489437.     ECS096
02302      12  FILLER              PIC S9(9)       VALUE +09466947.     ECS096
02303      12  FILLER              PIC S9(9)       VALUE +09444605.     ECS096
02304      12  FILLER              PIC S9(9)       VALUE +09422221.     ECS096
02305      12  FILLER              PIC S9(9)       VALUE +09399702.     ECS096
02306      12  FILLER              PIC S9(9)       VALUE +09376955.     ECS096
02307      12  FILLER              PIC S9(9)       VALUE +09353794.     ECS096
02308      12  FILLER              PIC S9(9)       VALUE +09330035.     ECS096
02309      12  FILLER              PIC S9(9)       VALUE +09305591.     ECS096
02310      12  FILLER              PIC S9(9)       VALUE +09280372.     ECS096
02311      12  FILLER              PIC S9(9)       VALUE +09254109.     ECS096
02312      12  FILLER              PIC S9(9)       VALUE +09226624.     ECS096
02313      12  FILLER              PIC S9(9)       VALUE +09197745.     ECS096
02314      12  FILLER              PIC S9(9)       VALUE +09166932.     ECS096
02315      12  FILLER              PIC S9(9)       VALUE +09133748.     ECS096
02316      12  FILLER              PIC S9(9)       VALUE +09097761.     ECS096
02317      12  FILLER              PIC S9(9)       VALUE +09058823.     ECS096
02318      12  FILLER              PIC S9(9)       VALUE +09016790.     ECS096
02319      12  FILLER              PIC S9(9)       VALUE +08971526.     ECS096
02320      12  FILLER              PIC S9(9)       VALUE +08922810.     ECS096
02321      12  FILLER              PIC S9(9)       VALUE +08870344.     ECS096
02322      12  FILLER              PIC S9(9)       VALUE +08814017.     ECS096
02323      12  FILLER              PIC S9(9)       VALUE +08753730.     ECS096
02324      12  FILLER              PIC S9(9)       VALUE +08689215.     ECS096
02325      12  FILLER              PIC S9(9)       VALUE +08620048.     ECS096
02326      12  FILLER              PIC S9(9)       VALUE +08545744.     ECS096
02327      12  FILLER              PIC S9(9)       VALUE +08465755.     ECS096
02328      12  FILLER              PIC S9(9)       VALUE +08379405.     ECS096
02329      12  FILLER              PIC S9(9)       VALUE +08285974.     ECS096
02330      12  FILLER              PIC S9(9)       VALUE +08185051.     ECS096
02331      12  FILLER              PIC S9(9)       VALUE +08076272.     ECS096
02332      12  FILLER              PIC S9(9)       VALUE +07959489.     ECS096
02333      12  FILLER              PIC S9(9)       VALUE +07834764.     ECS096
02334      12  FILLER              PIC S9(9)       VALUE +07701886.     ECS096
02335      12  FILLER              PIC S9(9)       VALUE +07560325.     ECS096
02336      12  FILLER              PIC S9(9)       VALUE +07409421.     ECS096
02337      12  FILLER              PIC S9(9)       VALUE +07248341.     ECS096
02338      12  FILLER              PIC S9(9)       VALUE +07075830.     ECS096
02339      12  FILLER              PIC S9(9)       VALUE +06891009.     ECS096
02340      12  FILLER              PIC S9(9)       VALUE +06693306.     ECS096
02341      12  FILLER              PIC S9(9)       VALUE +06482869.     ECS096
02342      12  FILLER              PIC S9(9)       VALUE +06260117.     ECS096
02343      12  FILLER              PIC S9(9)       VALUE +06025739.     ECS096
02344      12  FILLER              PIC S9(9)       VALUE +05780371.     ECS096
02345      12  FILLER              PIC S9(9)       VALUE +05524185.     ECS096
02346      12  FILLER              PIC S9(9)       VALUE +05256814.     ECS096
02347      12  FILLER              PIC S9(9)       VALUE +04977677.     ECS096
02348      12  FILLER              PIC S9(9)       VALUE +04686334.     ECS096
02349      12  FILLER              PIC S9(9)       VALUE +04383128.     ECS096
02350      12  FILLER              PIC S9(9)       VALUE +04069690.     ECS096
02351      12  FILLER              PIC S9(9)       VALUE +03748917.     ECS096
02352      12  FILLER              PIC S9(9)       VALUE +03424599.     ECS096
02353      12  FILLER              PIC S9(9)       VALUE +03101043.     ECS096
02354      12  FILLER              PIC S9(9)       VALUE +02782193.     ECS096
02355      12  FILLER              PIC S9(9)       VALUE +02471172.     ECS096
02356      12  FILLER              PIC S9(9)       VALUE +02170381.     ECS096
02357      12  FILLER              PIC S9(9)       VALUE +01881894.     ECS096
02358      12  FILLER              PIC S9(9)       VALUE +01607834.     ECS096
02359      12  FILLER              PIC S9(9)       VALUE +01350950.     ECS096
02360      12  FILLER              PIC S9(9)       VALUE +01114426.     ECS096
02361      12  FILLER              PIC S9(9)       VALUE +00901280.     ECS096
02362      12  FILLER              PIC S9(9)       VALUE +00713706.     ECS096
02363      12  FILLER              PIC S9(9)       VALUE +00552708.     ECS096
02364      12  FILLER              PIC S9(9)       VALUE +00418035.     ECS096
02365      12  FILLER              PIC S9(9)       VALUE +00308288.     ECS096
02366      12  FILLER              PIC S9(9)       VALUE +00221218.     ECS096
02367      12  FILLER              PIC S9(9)       VALUE +00153981.     ECS096
02368      12  FILLER              PIC S9(9)       VALUE +00103495.     ECS096
02369      12  FILLER              PIC S9(9)       VALUE +00066620.     ECS096
02370      12  FILLER              PIC S9(9)       VALUE +00040377.     ECS096
02371      12  FILLER              PIC S9(9)       VALUE +00022246.     ECS096
02372      12  FILLER              PIC S9(9)       VALUE +00010290.     ECS096
02373      12  FILLER              PIC S9(9)       VALUE +00003239.     ECS096
02374      12  FILLER              PIC S9(9)       VALUE +00000411.     ECS096
02375                                                                   ECS096
02376  01  80GBT-TABLE-R  REDEFINES  80GBT-TABLE.                       ECS096
02377      12  80GBT-OCCURS    OCCURS  100  TIMES  COMP-3.              ECS096
02378          16  80GBT-VALUE     PIC S9(9).                           ECS096
02379                                                                   ECS096
02380                                  EJECT                            ECS096
CIDMOD 01  FILLER                  PIC  X(16)  SYNC    VALUE            00019566
CIDMOD         '***80UET-TABLE**'.                                      00019567
CIDMOD                                                                  00019568
CIDMOD 01  80UET-TABLE     COMP-3.                                      00019569
CIDMOD     12  FILLER              PIC S9(09)      VALUE +10000000.     00019570
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09968170.     00019571
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09950995.     00019572
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09934247.     00019573
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09917727.     00019574
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09901541.     00019575
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09885788.     00019576
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09870465.     00019577
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09855570.     00019578
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09840935.     00019579
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09826419.     00019580
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09811886.     00019581
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09796903.     00019582
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09781101.     00019583
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09764179.     00019584
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09745979.     00019585
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09726438.     00019586
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09705633.     00019587
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09683873.     00019588
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09661368.     00019589
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09638422.     00019590
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09615309.     00019591
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09592194.     00019592
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09569211.     00019593
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09546494.     00019594
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09524012.     00019595
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09501859.     00019596
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09479872.     00019597
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09457888.     00019598
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09435908.     00019599
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09413734.     00019600
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09391272.     00019601
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09368395.     00019602
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09344974.     00019603
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09320892.     00019604
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09296024.     00019605
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09270190.     00019606
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09243122.     00019607
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09214643.     00019608
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09184235.     00019609
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09151539.     00019610
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09116242.     00019611
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09078054.     00019612
CIDMOD     12  FILLER              PIC S9(09)      VALUE +09036885.     00019613
CIDMOD     12  FILLER              PIC S9(09)      VALUE +08992460.     00019614
CIDMOD     12  FILLER              PIC S9(09)      VALUE +08944647.     00019615
CIDMOD     12  FILLER              PIC S9(09)      VALUE +08893313.     00019616
CIDMOD     12  FILLER              PIC S9(09)      VALUE +08838246.     00019617
CIDMOD     12  FILLER              PIC S9(09)      VALUE +08779312.     00019618
CIDMOD     12  FILLER              PIC S9(09)      VALUE +08716295.     00019619
CIDMOD     12  FILLER              PIC S9(09)      VALUE +08648752.     00019620
CIDMOD     12  FILLER              PIC S9(09)      VALUE +08576353.     00019621
CIDMOD     12  FILLER              PIC S9(09)      VALUE +08498403.     00019622
CIDMOD     12  FILLER              PIC S9(09)      VALUE +08414345.     00019623
CIDMOD     12  FILLER              PIC S9(09)      VALUE +08323504.     00019624
CIDMOD     12  FILLER              PIC S9(09)      VALUE +08225453.     00019625
CIDMOD     12  FILLER              PIC S9(09)      VALUE +08119888.     00019626
CIDMOD     12  FILLER              PIC S9(09)      VALUE +08006745.     00019627
CIDMOD     12  FILLER              PIC S9(09)      VALUE +07885971.     00019628
CIDMOD     12  FILLER              PIC S9(09)      VALUE +07757477.     00019629
CIDMOD     12  FILLER              PIC S9(09)      VALUE +07620767.     00019630
CIDMOD     12  FILLER              PIC S9(09)      VALUE +07475134.     00019631
CIDMOD     12  FILLER              PIC S9(09)      VALUE +07319510.     00019632
CIDMOD     12  FILLER              PIC S9(09)      VALUE +07152669.     00019633
CIDMOD     12  FILLER              PIC S9(09)      VALUE +06973637.     00019634
CIDMOD     12  FILLER              PIC S9(09)      VALUE +06781758.     00019635
CIDMOD     12  FILLER              PIC S9(09)      VALUE +06577010.     00019636
CIDMOD     12  FILLER              PIC S9(09)      VALUE +06359804.     00019637
CIDMOD     12  FILLER              PIC S9(09)      VALUE +06130838.     00019638
CIDMOD     12  FILLER              PIC S9(09)      VALUE +05890657.     00019639
CIDMOD     12  FILLER              PIC S9(09)      VALUE +05639214.     00019640
CIDMOD     12  FILLER              PIC S9(09)      VALUE +05375981.     00019641
CIDMOD     12  FILLER              PIC S9(09)      VALUE +05100199.     00019642
CIDMOD     12  FILLER              PIC S9(09)      VALUE +04811155.     00019643
CIDMOD     12  FILLER              PIC S9(09)      VALUE +04508846.     00019644
CIDMOD     12  FILLER              PIC S9(09)      VALUE +04194751.     00019645
CIDMOD     12  FILLER              PIC S9(09)      VALUE +03871671.     00019646
CIDMOD     12  FILLER              PIC S9(09)      VALUE +03543377.     00019647
CIDMOD     12  FILLER              PIC S9(09)      VALUE +03214197.     00019648
CIDMOD     12  FILLER              PIC S9(09)      VALUE +02888181.     00019649
CIDMOD     12  FILLER              PIC S9(09)      VALUE +02568647.     00019650
CIDMOD     12  FILLER              PIC S9(09)      VALUE +02258239.     00019651
CIDMOD     12  FILLER              PIC S9(09)      VALUE +01959343.     00019652
CIDMOD     12  FILLER              PIC S9(09)      VALUE +01674415.     00019653
CIDMOD     12  FILLER              PIC S9(09)      VALUE +01406552.     00019654
CIDMOD     12  FILLER              PIC S9(09)      VALUE +01159386.     00019655
CIDMOD     12  FILLER              PIC S9(09)      VALUE +00936363.     00019656
CIDMOD     12  FILLER              PIC S9(09)      VALUE +00740026.     00019657
CIDMOD     12  FILLER              PIC S9(09)      VALUE +00571608.     00019658
CIDMOD     12  FILLER              PIC S9(09)      VALUE +00430953.     00019659
CIDMOD     12  FILLER              PIC S9(09)      VALUE +00316638.     00019660
CIDMOD     12  FILLER              PIC S9(09)      VALUE +00226262.     00019661
CIDMOD     12  FILLER              PIC S9(09)      VALUE +00156792.     00019662
CIDMOD     12  FILLER              PIC S9(09)      VALUE +00104895.     00019663
CIDMOD     12  FILLER              PIC S9(09)      VALUE +00067189.     00019664
CIDMOD     12  FILLER              PIC S9(09)      VALUE +00040501.     00019665
CIDMOD     12  FILLER              PIC S9(09)      VALUE +00022146.     00019666
CIDMOD     12  FILLER              PIC S9(09)      VALUE +00010087.     00019667
CIDMOD     12  FILLER              PIC S9(09)      VALUE +00003008.     00019668
CIDMOD     12  FILLER              PIC S9(09)      VALUE +00000096.     00019669
CIDMOD                                                                  00019670
CIDMOD 01  80UET-TABLE-R  REDEFINES  80UET-TABLE.                       00019671
CIDMOD     12  80UET-OCCURS    OCCURS  100  TIMES  COMP-3.              00019672
CIDMOD         16  80UET-VALUE     PIC S9(09).                          00019673
CIDMOD                                                                  00019674
CIDMOD                                 EJECT                            ECS096
CIDMOD 01  FILLER                  PIC  X(16)  SYNC    VALUE            00019566
LGC188         '***80USO-TABLE**'.                                      00019567
LGC188                                                                  00019568
LGC188 01  80USO-TABLE     COMP-3.                                      00019569
LGC188     12  FILLER              PIC S9(09)      VALUE +10000000.     00019570
LGC188     12  FILLER              PIC S9(09)      VALUE +09975950.     00019571
LGC188     12  FILLER              PIC S9(09)      VALUE +09966243.     00019572
LGC188     12  FILLER              PIC S9(09)      VALUE +09956945.     00019573
LGC188     12  FILLER              PIC S9(09)      VALUE +09947854.     00019574
LGC188     12  FILLER              PIC S9(09)      VALUE +09939080.     00019575
LGC188     12  FILLER              PIC S9(09)      VALUE +09930721.     00019576
LGC188     12  FILLER              PIC S9(09)      VALUE +09922777.     00019577
LGC188     12  FILLER              PIC S9(09)      VALUE +09915245.     00019578
LGC188     12  FILLER              PIC S9(09)      VALUE +09907958.     00019579
LGC188     12  FILLER              PIC S9(09)      VALUE +09900774.     00019580
LGC188     12  FILLER              PIC S9(09)      VALUE +09893557.     00019581
LGC188     12  FILLER              PIC S9(09)      VALUE +09885870.     00019582
LGC188     12  FILLER              PIC S9(09)      VALUE +09877338.     00019583
LGC188     12  FILLER              PIC S9(09)      VALUE +09867658.     00019584
LGC188     12  FILLER              PIC S9(09)      VALUE +09856666.     00019585
LGC188     12  FILLER              PIC S9(09)      VALUE +09844296.     00019586
LGC188     12  FILLER              PIC S9(09)      VALUE +09830622.     00019587
LGC188     12  FILLER              PIC S9(09)      VALUE +09815955.     00019588
LGC188     12  FILLER              PIC S9(09)      VALUE +09800504.     00019589
LGC188     12  FILLER              PIC S9(09)      VALUE +09784578.     00019590
LGC188     12  FILLER              PIC S9(09)      VALUE +09768453.     00019591
LGC188     12  FILLER              PIC S9(09)      VALUE +09752296.     00019592
LGC188     12  FILLER              PIC S9(09)      VALUE +09736244.     00019593
LGC188     12  FILLER              PIC S9(09)      VALUE +09720432.     00019594
LGC188     12  FILLER              PIC S9(09)      VALUE +09704831.     00019595
LGC188     12  FILLER              PIC S9(09)      VALUE +09689536.     00019596
LGC188     12  FILLER              PIC S9(09)      VALUE +09674382.     00019597
LGC188     12  FILLER              PIC S9(09)      VALUE +09659203.     00019598
LGC188     12  FILLER              PIC S9(09)      VALUE +09643999.     00019599
LGC188     12  FILLER              PIC S9(09)      VALUE +09628569.     00019600
LGC188     12  FILLER              PIC S9(09)      VALUE +09612816.     00019601
LGC188     12  FILLER              PIC S9(09)      VALUE +09596609.     00019602
LGC188     12  FILLER              PIC S9(09)      VALUE +09579815.     00019603
LGC188     12  FILLER              PIC S9(09)      VALUE +09562313.     00019604
LGC188     12  FILLER              PIC S9(09)      VALUE +09543972.     00019605
LGC188     12  FILLER              PIC S9(09)      VALUE +09524608.     00019606
LGC188     12  FILLER              PIC S9(09)      VALUE +09503939.     00019607
LGC188     12  FILLER              PIC S9(09)      VALUE +09481786.     00019608
LGC188     12  FILLER              PIC S9(09)      VALUE +09457939.     00019609
LGC188     12  FILLER              PIC S9(09)      VALUE +09432157.     00019610
LGC188     12  FILLER              PIC S9(09)      VALUE +09404200.     00019611
LGC188     12  FILLER              PIC S9(09)      VALUE +09373928.     00019612
LGC188     12  FILLER              PIC S9(09)      VALUE +09341203.     00019613
LGC188     12  FILLER              PIC S9(09)      VALUE +09305884.     00019614
LGC188     12  FILLER              PIC S9(09)      VALUE +09267814.     00019615
LGC188     12  FILLER              PIC S9(09)      VALUE +09226896.     00019616
LGC188     12  FILLER              PIC S9(09)      VALUE +09182976.     00019617
LGC188     12  FILLER              PIC S9(09)      VALUE +09135886.     00019618
LGC188     12  FILLER              PIC S9(09)      VALUE +09085428.     00019619
LGC188     12  FILLER              PIC S9(09)      VALUE +09031288.     00019620
LGC188     12  FILLER              PIC S9(09)      VALUE +08973136.     00019621
LGC188     12  FILLER              PIC S9(09)      VALUE +08910405.     00019622
LGC188     12  FILLER              PIC S9(09)      VALUE +08842623.     00019623
LGC188     12  FILLER              PIC S9(09)      VALUE +08769185.     00019624
LGC188     12  FILLER              PIC S9(09)      VALUE +08689719.     00019625
LGC188     12  FILLER              PIC S9(09)      VALUE +08603943.     00019626
LGC188     12  FILLER              PIC S9(09)      VALUE +08511717.     00019627
LGC188     12  FILLER              PIC S9(09)      VALUE +08412956.     00019628
LGC188     12  FILLER              PIC S9(09)      VALUE +08307491.     00019629
LGC188     12  FILLER              PIC S9(09)      VALUE +08194891.     00019630
LGC188     12  FILLER              PIC S9(09)      VALUE +08074426.     00019631
LGC188     12  FILLER              PIC S9(09)      VALUE +07945122.     00019632
LGC188     12  FILLER              PIC S9(09)      VALUE +07805836.     00019633
LGC188     12  FILLER              PIC S9(09)      VALUE +07655558.     00019634
LGC188     12  FILLER              PIC S9(09)      VALUE +07493528.     00019635
LGC188     12  FILLER              PIC S9(09)      VALUE +07319521.     00019636
LGC188     12  FILLER              PIC S9(09)      VALUE +07133576.     00019637
LGC188     12  FILLER              PIC S9(09)      VALUE +06936033.     00019638
LGC188     12  FILLER              PIC S9(09)      VALUE +06727037.     00019639
LGC188     12  FILLER              PIC S9(09)      VALUE +06506148.     00019640
LGC188     12  FILLER              PIC S9(09)      VALUE +06272531.     00019641
LGC188     12  FILLER              PIC S9(09)      VALUE +06025017.     00019642
LGC188     12  FILLER              PIC S9(09)      VALUE +05762345.     00019643
LGC188     12  FILLER              PIC S9(09)      VALUE +05483822.     00019644
LGC188     12  FILLER              PIC S9(09)      VALUE +05189971.     00019645
LGC188     12  FILLER              PIC S9(09)      VALUE +04882502.     00019646
LGC188     12  FILLER              PIC S9(09)      VALUE +04564040.     00019647
LGC188     12  FILLER              PIC S9(09)      VALUE +04237880.     00019648
LGC188     12  FILLER              PIC S9(09)      VALUE +03907241.     00019649
LGC188     12  FILLER              PIC S9(09)      VALUE +03574723.     00019650
LGC188     12  FILLER              PIC S9(09)      VALUE +03242420.     00019651
LGC188     12  FILLER              PIC S9(09)      VALUE +02912303.     00019652
LGC188     12  FILLER              PIC S9(09)      VALUE +02586527.     00019653
LGC188     12  FILLER              PIC S9(09)      VALUE +02268234.     00019654
LGC188     12  FILLER              PIC S9(09)      VALUE +01961637.     00019655
LGC188     12  FILLER              PIC S9(09)      VALUE +01671368.     00019656
LGC188     12  FILLER              PIC S9(09)      VALUE +01401788.     00019657
LGC188     12  FILLER              PIC S9(09)      VALUE +01156384.     00019658
LGC188     12  FILLER              PIC S9(09)      VALUE +00937498.     00019659
LGC188     12  FILLER              PIC S9(09)      VALUE +00746206.     00019660
LGC188     12  FILLER              PIC S9(09)      VALUE +00582372.     00019661
LGC188     12  FILLER              PIC S9(09)      VALUE +00444827.     00019662
LGC188     12  FILLER              PIC S9(09)      VALUE +00331570.     00019663
LGC188     12  FILLER              PIC S9(09)      VALUE +00239886.     00019664
LGC188     12  FILLER              PIC S9(09)      VALUE +00166590.     00019665
LGC188     12  FILLER              PIC S9(09)      VALUE +00108515.     00019666
LGC188     12  FILLER              PIC S9(09)      VALUE +00063063.     00019667
LGC188     12  FILLER              PIC S9(09)      VALUE +00029017.     00019668
LGC188     12  FILLER              PIC S9(09)      VALUE +00007405.     00019669
LGC188                                                                  00019670
LGC188 01  80USO-TABLE-R  REDEFINES  80USO-TABLE.                       00019671
LGC188     12  80USO-OCCURS    OCCURS  100  TIMES  COMP-3.              00019672
LGC188         16  80USO-VALUE     PIC S9(09).                          00019673
LGC188                                                                  00019674

052507*01  FILLER                  PIC  X(27)  SYNC    VALUE
052507*        '***01CSO-TABLE AGE LAST ***'.

052507*01  01CSO-TABLE     COMP-3.
052507*    12  FILLER      PIC S9(9)               VALUE +10000000.
052507*    12  FILLER      PIC S9(9)               VALUE +09992800.
052507*    12  FILLER      PIC S9(9)               VALUE +09988203.
052507*    12  FILLER      PIC S9(9)               VALUE +09984907.
052507*    12  FILLER      PIC S9(9)               VALUE +09982511.
052507*    12  FILLER      PIC S9(9)               VALUE +09980414.
052507*    12  FILLER      PIC S9(9)               VALUE +09978319.
052507*    12  FILLER      PIC S9(9)               VALUE +09976123.
052507*    12  FILLER      PIC S9(9)               VALUE +09973929.
052507*    12  FILLER      PIC S9(9)               VALUE +09971734.
052507*    12  FILLER      PIC S9(9)               VALUE +09969441.
052507*    12  FILLER      PIC S9(9)               VALUE +09967048.
052507*    12  FILLER      PIC S9(9)               VALUE +09964257.
052507*    12  FILLER      PIC S9(9)               VALUE +09960870.
052507*    12  FILLER      PIC S9(9)               VALUE +09956885.
052507*    12  FILLER      PIC S9(9)               VALUE +09951708.
052507*    12  FILLER      PIC S9(9)               VALUE +09945140.
052507*    12  FILLER      PIC S9(9)               VALUE +09937382.
052507*    12  FILLER      PIC S9(9)               VALUE +09928538.
052507*    12  FILLER      PIC S9(9)               VALUE +09919106.
052507*    12  FILLER      PIC S9(9)               VALUE +09909385.
052507*    12  FILLER      PIC S9(9)               VALUE +09899476.
052507*    12  FILLER      PIC S9(9)               VALUE +09889477.
052507*    12  FILLER      PIC S9(9)               VALUE +09879390.
052507*    12  FILLER      PIC S9(9)               VALUE +09869116.
052507*    12  FILLER      PIC S9(9)               VALUE +09858654.
052507*    12  FILLER      PIC S9(9)               VALUE +09847908.
052507*    12  FILLER      PIC S9(9)               VALUE +09836682.
052507*    12  FILLER      PIC S9(9)               VALUE +09825173.
052507*    12  FILLER      PIC S9(9)               VALUE +09813776.
052507*    12  FILLER      PIC S9(9)               VALUE +09802490.
052507*    12  FILLER      PIC S9(9)               VALUE +09791315.
052507*    12  FILLER      PIC S9(9)               VALUE +09780251.
052507*    12  FILLER      PIC S9(9)               VALUE +09769101.
052507*    12  FILLER      PIC S9(9)               VALUE +09757769.
052507*    12  FILLER      PIC S9(9)               VALUE +09746157.
052507*    12  FILLER      PIC S9(9)               VALUE +09734072.
052507*    12  FILLER      PIC S9(9)               VALUE +09721320.
052507*    12  FILLER      PIC S9(9)               VALUE +09707808.
052507*    12  FILLER      PIC S9(9)               VALUE +09693343.
052507*    12  FILLER      PIC S9(9)               VALUE +09677931.
052507*    12  FILLER      PIC S9(9)               VALUE +09661285.
052507*    12  FILLER      PIC S9(9)               VALUE +09643218.
052507*    12  FILLER      PIC S9(9)               VALUE +09623450.
052507*    12  FILLER      PIC S9(9)               VALUE +09601604.
052507*    12  FILLER      PIC S9(9)               VALUE +09577408.
052507*    12  FILLER      PIC S9(9)               VALUE +09550879.
052507*    12  FILLER      PIC S9(9)               VALUE +09521940.
052507*    12  FILLER      PIC S9(9)               VALUE +09490993.
052507*    12  FILLER      PIC S9(9)               VALUE +09458534.
052507*    12  FILLER      PIC S9(9)               VALUE +09424105.
052507*    12  FILLER      PIC S9(9)               VALUE +09387257.
052507*    12  FILLER      PIC S9(9)               VALUE +09347267.
052507*    12  FILLER      PIC S9(9)               VALUE +09303335.
052507*    12  FILLER      PIC S9(9)               VALUE +09254865.
052507*    12  FILLER      PIC S9(9)               VALUE +09200909.
052507*    12  FILLER      PIC S9(9)               VALUE +09140919.
052507*    12  FILLER      PIC S9(9)               VALUE +09074556.
052507*    12  FILLER      PIC S9(9)               VALUE +09002413.
052507*    12  FILLER      PIC S9(9)               VALUE +08924722.
052507*    12  FILLER      PIC S9(9)               VALUE +08840651.
052507*    12  FILLER      PIC S9(9)               VALUE +08748709.
052507*    12  FILLER      PIC S9(9)               VALUE +08647311.
052507*    12  FILLER      PIC S9(9)               VALUE +08535069.
052507*    12  FILLER      PIC S9(9)               VALUE +08411566.
052507*    12  FILLER      PIC S9(9)               VALUE +08276645.
052507*    12  FILLER      PIC S9(9)               VALUE +08130562.
052507*    12  FILLER      PIC S9(9)               VALUE +07973886.
052507*    12  FILLER      PIC S9(9)               VALUE +07806754.
052507*    12  FILLER      PIC S9(9)               VALUE +07629228.
052507*    12  FILLER      PIC S9(9)               VALUE +07440862.
052507*    12  FILLER      PIC S9(9)               VALUE +07240406.
052507*    12  FILLER      PIC S9(9)               VALUE +07025293.
052507*    12  FILLER      PIC S9(9)               VALUE +06793880.
052507*    12  FILLER      PIC S9(9)               VALUE +06547126.
052507*    12  FILLER      PIC S9(9)               VALUE +06285503.
052507*    12  FILLER      PIC S9(9)               VALUE +06009255.
052507*    12  FILLER      PIC S9(9)               VALUE +05718167.
052507*    12  FILLER      PIC S9(9)               VALUE +05411273.
052507*    12  FILLER      PIC S9(9)               VALUE +05088112.
052507*    12  FILLER      PIC S9(9)               VALUE +04749854.
052507*    12  FILLER      PIC S9(9)               VALUE +04398270.
052507*    12  FILLER      PIC S9(9)               VALUE +04036732.
052507*    12  FILLER      PIC S9(9)               VALUE +03670116.
052507*    12  FILLER      PIC S9(9)               VALUE +03302297.
052507*    12  FILLER      PIC S9(9)               VALUE +02936766.
052507*    12  FILLER      PIC S9(9)               VALUE +02577423.
052507*    12  FILLER      PIC S9(9)               VALUE +02229033.
052507*    12  FILLER      PIC S9(9)               VALUE +01896929.
052507*    12  FILLER      PIC S9(9)               VALUE +01586478.
052507*    12  FILLER      PIC S9(9)               VALUE +01302451.
052507*    12  FILLER      PIC S9(9)               VALUE +01049411.
052507*    12  FILLER      PIC S9(9)               VALUE +00829800.
052507*    12  FILLER      PIC S9(9)               VALUE +00643145.
052507*    12  FILLER      PIC S9(9)               VALUE +00487851.
052507*    12  FILLER      PIC S9(9)               VALUE +00361566.
052507*    12  FILLER      PIC S9(9)               VALUE +00261730.
052507*    12  FILLER      PIC S9(9)               VALUE +00185057.
052507*    12  FILLER      PIC S9(9)               VALUE +00127530.
052507*    12  FILLER      PIC S9(9)               VALUE +00085451.

052507*01  01CSO-TABLE-R REDEFINES 01CSO-TABLE.
052507*    12  01CSO-OCCURS    OCCURS  100  TIMES  COMP-3.
052507*        16  01CSO-VALUE     PIC S9(9).

101005 01  FILLER                  PIC  X(27)  SYNC    VALUE
101005         '***01CSO-TABLE AGE LAST ***'.


101005 01  01CSO-TABLE     COMP-3.
           12  FILLER      PIC S9(9)               VALUE +10000000.
           12  FILLER      PIC S9(9)               VALUE +09992800.
           12  FILLER      PIC S9(9)               VALUE +09988203.
           12  FILLER      PIC S9(9)               VALUE +09984907.
           12  FILLER      PIC S9(9)               VALUE +09982510.
           12  FILLER      PIC S9(9)               VALUE +09980414.
           12  FILLER      PIC S9(9)               VALUE +09978318.
           12  FILLER      PIC S9(9)               VALUE +09976123.
           12  FILLER      PIC S9(9)               VALUE +09973928.
           12  FILLER      PIC S9(9)               VALUE +09971734.
           12  FILLER      PIC S9(9)               VALUE +09969440.
           12  FILLER      PIC S9(9)               VALUE +09967048.
           12  FILLER      PIC S9(9)               VALUE +09964257.
           12  FILLER      PIC S9(9)               VALUE +09960869.
           12  FILLER      PIC S9(9)               VALUE +09956885.
           12  FILLER      PIC S9(9)               VALUE +09951707.
           12  FILLER      PIC S9(9)               VALUE +09945139.
           12  FILLER      PIC S9(9)               VALUE +09937382.
           12  FILLER      PIC S9(9)               VALUE +09928538.
           12  FILLER      PIC S9(9)               VALUE +09919105.
           12  FILLER      PIC S9(9)               VALUE +09909385.
           12  FILLER      PIC S9(9)               VALUE +09899475.
           12  FILLER      PIC S9(9)               VALUE +09889477.
           12  FILLER      PIC S9(9)               VALUE +09879390.
           12  FILLER      PIC S9(9)               VALUE +09869115.
           12  FILLER      PIC S9(9)               VALUE +09858654.
           12  FILLER      PIC S9(9)               VALUE +09847908.
           12  FILLER      PIC S9(9)               VALUE +09836681.
           12  FILLER      PIC S9(9)               VALUE +09825172.
           12  FILLER      PIC S9(9)               VALUE +09813775.
           12  FILLER      PIC S9(9)               VALUE +09802489.
           12  FILLER      PIC S9(9)               VALUE +09791314.
           12  FILLER      PIC S9(9)               VALUE +09780250.
           12  FILLER      PIC S9(9)               VALUE +09769101.
           12  FILLER      PIC S9(9)               VALUE +09757769.
           12  FILLER      PIC S9(9)               VALUE +09746157.
           12  FILLER      PIC S9(9)               VALUE +09734072.
           12  FILLER      PIC S9(9)               VALUE +09721320.
           12  FILLER      PIC S9(9)               VALUE +09707807.
           12  FILLER      PIC S9(9)               VALUE +09693343.
           12  FILLER      PIC S9(9)               VALUE +09677930.
           12  FILLER      PIC S9(9)               VALUE +09661284.
           12  FILLER      PIC S9(9)               VALUE +09643218.
           12  FILLER      PIC S9(9)               VALUE +09623449.
           12  FILLER      PIC S9(9)               VALUE +09601604.
           12  FILLER      PIC S9(9)               VALUE +09577408.
           12  FILLER      PIC S9(9)               VALUE +09550878.
           12  FILLER      PIC S9(9)               VALUE +09521939.
           12  FILLER      PIC S9(9)               VALUE +09490993.
           12  FILLER      PIC S9(9)               VALUE +09458534.
           12  FILLER      PIC S9(9)               VALUE +09424105.
           12  FILLER      PIC S9(9)               VALUE +09387256.
           12  FILLER      PIC S9(9)               VALUE +09347267.
           12  FILLER      PIC S9(9)               VALUE +09303335.
           12  FILLER      PIC S9(9)               VALUE +09254864.
           12  FILLER      PIC S9(9)               VALUE +09200908.
           12  FILLER      PIC S9(9)               VALUE +09140918.
           12  FILLER      PIC S9(9)               VALUE +09074555.
           12  FILLER      PIC S9(9)               VALUE +09002413.
           12  FILLER      PIC S9(9)               VALUE +08924722.
           12  FILLER      PIC S9(9)               VALUE +08840651.
           12  FILLER      PIC S9(9)               VALUE +08748708.
           12  FILLER      PIC S9(9)               VALUE +08647311.
           12  FILLER      PIC S9(9)               VALUE +08535068.
           12  FILLER      PIC S9(9)               VALUE +08411566.
           12  FILLER      PIC S9(9)               VALUE +08276644.
           12  FILLER      PIC S9(9)               VALUE +08130562.
           12  FILLER      PIC S9(9)               VALUE +07973886.
           12  FILLER      PIC S9(9)               VALUE +07806753.
           12  FILLER      PIC S9(9)               VALUE +07629228.
           12  FILLER      PIC S9(9)               VALUE +07440862.
           12  FILLER      PIC S9(9)               VALUE +07240405.
           12  FILLER      PIC S9(9)               VALUE +07025293.
           12  FILLER      PIC S9(9)               VALUE +06793879.
           12  FILLER      PIC S9(9)               VALUE +06547126.
           12  FILLER      PIC S9(9)               VALUE +06285503.
           12  FILLER      PIC S9(9)               VALUE +06009255.
           12  FILLER      PIC S9(9)               VALUE +05718166.
           12  FILLER      PIC S9(9)               VALUE +05411272.
           12  FILLER      PIC S9(9)               VALUE +05088111.
           12  FILLER      PIC S9(9)               VALUE +04749853.
           12  FILLER      PIC S9(9)               VALUE +04398269.
           12  FILLER      PIC S9(9)               VALUE +04036732.
           12  FILLER      PIC S9(9)               VALUE +03670116.
           12  FILLER      PIC S9(9)               VALUE +03302296.
           12  FILLER      PIC S9(9)               VALUE +02936765.
           12  FILLER      PIC S9(9)               VALUE +02577423.
           12  FILLER      PIC S9(9)               VALUE +02229032.
           12  FILLER      PIC S9(9)               VALUE +01896929.
           12  FILLER      PIC S9(9)               VALUE +01586477.
           12  FILLER      PIC S9(9)               VALUE +01302450.
           12  FILLER      PIC S9(9)               VALUE +01049410.
           12  FILLER      PIC S9(9)               VALUE +00829800.
           12  FILLER      PIC S9(9)               VALUE +00643145.
           12  FILLER      PIC S9(9)               VALUE +00487851.
           12  FILLER      PIC S9(9)               VALUE +00361566.
           12  FILLER      PIC S9(9)               VALUE +00261730.
           12  FILLER      PIC S9(9)               VALUE +00185056.
           12  FILLER      PIC S9(9)               VALUE +00127529.
           12  FILLER      PIC S9(9)               VALUE +00085451.

101005 01  01CSO-TABLE-R REDEFINES 01CSO-TABLE.
101005     12  01CSO-OCCURS    OCCURS  100  TIMES  COMP-3.
101005         16  01CSO-VALUE     PIC S9(9).

101005 01  FILLER                  PIC  X(27)  SYNC    VALUE
101005         '***01CSO-TABLE AGE NEAR ***'.

101005 01  01CSO-AN-TABLE  COMP-3.
           12  FILLER      PIC S9(9)               VALUE +10000000.
           12  FILLER      PIC S9(9)               VALUE +09990300.
           12  FILLER      PIC S9(9)               VALUE +09984705.
           12  FILLER      PIC S9(9)               VALUE +09980811.
           12  FILLER      PIC S9(9)               VALUE +09978117.
           12  FILLER      PIC S9(9)               VALUE +09976021.
           12  FILLER      PIC S9(9)               VALUE +09973926.
           12  FILLER      PIC S9(9)               VALUE +09971732.
           12  FILLER      PIC S9(9)               VALUE +09969538.
           12  FILLER      PIC S9(9)               VALUE +09967345.
           12  FILLER      PIC S9(9)               VALUE +09965052.
           12  FILLER      PIC S9(9)               VALUE +09962760.
           12  FILLER      PIC S9(9)               VALUE +09960070.
           12  FILLER      PIC S9(9)               VALUE +09956784.
           12  FILLER      PIC S9(9)               VALUE +09952901.
           12  FILLER      PIC S9(9)               VALUE +09948223.
           12  FILLER      PIC S9(9)               VALUE +09942154.
           12  FILLER      PIC S9(9)               VALUE +09934797.
           12  FILLER      PIC S9(9)               VALUE +09926154.
           12  FILLER      PIC S9(9)               VALUE +09916823.
           12  FILLER      PIC S9(9)               VALUE +09907105.
           12  FILLER      PIC S9(9)               VALUE +09897198.
           12  FILLER      PIC S9(9)               VALUE +09887300.
           12  FILLER      PIC S9(9)               VALUE +09877215.
           12  FILLER      PIC S9(9)               VALUE +09867042.
           12  FILLER      PIC S9(9)               VALUE +09856681.
           12  FILLER      PIC S9(9)               VALUE +09846135.
           12  FILLER      PIC S9(9)               VALUE +09835107.
           12  FILLER      PIC S9(9)               VALUE +09823600.
           12  FILLER      PIC S9(9)               VALUE +09812106.
           12  FILLER      PIC S9(9)               VALUE +09800822.
           12  FILLER      PIC S9(9)               VALUE +09789650.
           12  FILLER      PIC S9(9)               VALUE +09778587.
           12  FILLER      PIC S9(9)               VALUE +09767537.
           12  FILLER      PIC S9(9)               VALUE +09756305.
           12  FILLER      PIC S9(9)               VALUE +09744792.
           12  FILLER      PIC S9(9)               VALUE +09733001.
           12  FILLER      PIC S9(9)               VALUE +09720543.
           12  FILLER      PIC S9(9)               VALUE +09707517.
           12  FILLER      PIC S9(9)               VALUE +09693539.
           12  FILLER      PIC S9(9)               VALUE +09678610.
           12  FILLER      PIC S9(9)               VALUE +09662641.
           12  FILLER      PIC S9(9)               VALUE +09645345.
           12  FILLER      PIC S9(9)               VALUE +09626440.
           12  FILLER      PIC S9(9)               VALUE +09605743.
           12  FILLER      PIC S9(9)               VALUE +09582785.
           12  FILLER      PIC S9(9)               VALUE +09557391.
           12  FILLER      PIC S9(9)               VALUE +09529674.
           12  FILLER      PIC S9(9)               VALUE +09499465.
           12  FILLER      PIC S9(9)               VALUE +09467832.
           12  FILLER      PIC S9(9)               VALUE +09434505.
           12  FILLER      PIC S9(9)               VALUE +09399032.
           12  FILLER      PIC S9(9)               VALUE +09360872.
           12  FILLER      PIC S9(9)               VALUE +09319028.
           12  FILLER      PIC S9(9)               VALUE +09273086.
           12  FILLER      PIC S9(9)               VALUE +09222084.
           12  FILLER      PIC S9(9)               VALUE +09165183.
           12  FILLER      PIC S9(9)               VALUE +09102127.
           12  FILLER      PIC S9(9)               VALUE +09032587.
           12  FILLER      PIC S9(9)               VALUE +08957887.
           12  FILLER      PIC S9(9)               VALUE +08877356.
           12  FILLER      PIC S9(9)               VALUE +08789825.
           12  FILLER      PIC S9(9)               VALUE +08693664.
           12  FILLER      PIC S9(9)               VALUE +08587167.
           12  FILLER      PIC S9(9)               VALUE +08469437.
           12  FILLER      PIC S9(9)               VALUE +08340363.
           12  FILLER      PIC S9(9)               VALUE +08199828.
           12  FILLER      PIC S9(9)               VALUE +08048377.
           12  FILLER      PIC S9(9)               VALUE +07886685.
           12  FILLER      PIC S9(9)               VALUE +07714361.
           12  FILLER      PIC S9(9)               VALUE +07531993.
           12  FILLER      PIC S9(9)               VALUE +07337894.
           12  FILLER      PIC S9(9)               VALUE +07131332.
           12  FILLER      PIC S9(9)               VALUE +06907979.
           12  FILLER      PIC S9(9)               VALUE +06668825.
           12  FILLER      PIC S9(9)               VALUE +06414876.
           12  FILLER      PIC S9(9)               VALUE +06146028.
           12  FILLER      PIC S9(9)               VALUE +05862819.
           12  FILLER      PIC S9(9)               VALUE +05564285.
           12  FILLER      PIC S9(9)               VALUE +05249569.
           12  FILLER      PIC S9(9)               VALUE +04918531.
           12  FILLER      PIC S9(9)               VALUE +04573545.
           12  FILLER      PIC S9(9)               VALUE +04215940.
           12  FILLER      PIC S9(9)               VALUE +03851092.
           12  FILLER      PIC S9(9)               VALUE +03483274.
           12  FILLER      PIC S9(9)               VALUE +03116033.
           12  FILLER      PIC S9(9)               VALUE +02752797.
           12  FILLER      PIC S9(9)               VALUE +02397934.
           12  FILLER      PIC S9(9)               VALUE +02056588.
           12  FILLER      PIC S9(9)               VALUE +01734259.
           12  FILLER      PIC S9(9)               VALUE +01436174.
           12  FILLER      PIC S9(9)               VALUE +01166662.
           12  FILLER      PIC S9(9)               VALUE +00930483.
           12  FILLER      PIC S9(9)               VALUE +00727796.
           12  FILLER      PIC S9(9)               VALUE +00557462.
           12  FILLER      PIC S9(9)               VALUE +00417461.
           12  FILLER      PIC S9(9)               VALUE +00305093.
           12  FILLER      PIC S9(9)               VALUE +00217946.
           12  FILLER      PIC S9(9)               VALUE +00151869.
           12  FILLER      PIC S9(9)               VALUE +00102986.

101005 01  01CSO-AN-TABLE-R REDEFINES 01CSO-AN-TABLE.
101005     12  01CSO-AN-OCCURS OCCURS  100  TIMES  COMP-3.
101005         16  01CSO-AN-VALUE  PIC S9(9).

02381                              COPY ELCDTECX.                       ECS096
02382                              COPY ELCDTEVR.                       ECS096
02383                                  EJECT                            ECS096
02384  01  W-PROGRAM-ABEND-STORAGE.                                     ECS096
02385      12  FILLER                  PIC  X(14)                       ECS096
02386                                  VALUE 'ABEND STORAGE:'.          ECS096
02387      12  WS-RETURN-CODE          PIC S9(4) COMP    VALUE +0.      ECS096
02388      12  WS-ABEND-MESSAGE.                                        ECS096
02389          16  WS-MESSAGE          PIC  X(40)        VALUE SPACES.  ECS096
02390          16  WS-MESSAGE-DATA     PIC  X(40)        VALUE SPACES.  ECS096
02391      12  WS-ABEND-FILE-STATUS    PIC  XX           VALUE '00'.    ECS096
02392      12  WS-ZERO                 PIC S9 COMP-3 VALUE +0.          ECS096
02393      12  WS-ABEND-CODE           PIC S9(4).                       ECS096
02394      12  WORK-ABEND-CODE REDEFINES WS-ABEND-CODE.                 ECS096
02395          16  WAC-1               PIC  X.                          ECS096
02396          16  WAC-2               PIC  X.                          ECS096
02397          16  WAC-3-4             PIC  99.                         ECS096
02398                                  EJECT                            ECS096
02399  PROCEDURE DIVISION.                                              ECS096
02400                                                                   ECS096
02401  0000-INITIALIZE SECTION.                                         ECS096
02402                                                                   ECS096
02403  0000-DATE-ROUTINE.                                               ECS096
02404                              COPY ELCDTERX.                       ECS096
02405                                  EJECT                            ECS096
02406      MOVE COMPANY-NAME           TO HD-CLIENT.                    ECS096
02407      MOVE ALPH-DATE              TO HD-DATE.                      ECS096
02408      MOVE WS-CURRENT-DATE        TO HD-RUN.                       ECS096
02409                                                                   ECS096
02410      IF DTE-CLIENT = 'HER'                                        ECS096
02411          MOVE HER-USER-TABLE     TO USER-TABLE.                   ECS096
02412                                                                   ECS096
02413      IF DTE-CLIENT = 'NCL'                                        ECS096
02414          MOVE NCL-USER-TABLE     TO USER-TABLE.                   ECS096
02415                                                                   ECS096
02416      PERFORM 0100-PROCESS-ONLINE THRU 0100-EXIT                   ECS096
02417                                                                   ECS096
02418      GO TO 9999-STOP-RUN.                                         ECS096
02419                                                                   ECS096
02420  0000-EXIT.                                                       ECS096
02421      EXIT.                                                        ECS096
02422                                  EJECT                            ECS096
02423  0100-PROCESS-ONLINE SECTION.                                     ECS096
02424                                                                   ECS096
02425  0100-INPUT.                                                      ECS096
02426                                                                   ECS096
02427      OPEN INPUT  ELCNTL                                           ECS096
02428           OUTPUT NEW-RFAC-FILE                                    ECS096
02429                  PRNTR.                                           ECS096
02430                                                                   ECS096
02431      IF  NOT W-VALID-CNTL-FILE-STATUS                             ECS096
02432          MOVE '**** BAD OPEN ON CNTL-FILE ****'                   ECS096
02433                                   TO WS-ABEND-MESSAGE             ECS096
02434          MOVE '0'                 TO WAC-1                        ECS096
02435          MOVE '1'                 TO WAC-2                        ECS096
02436          MOVE W-ELCNTL-FILE-STATUS                                ECS096
02437                                   TO WAC-3-4                      ECS096
02438                                      WS-ABEND-FILE-STATUS         ECS096
02439          MOVE WS-ABEND-CODE       TO WS-RETURN-CODE               ECS096
02440          GO TO ABEND-PGM.                                         ECS096
02441                                                                   ECS096
02442      MOVE SPACES                  TO CF-CONTROL-PRIMARY.          ECS096
02443      MOVE DTE-CLIENT              TO CF-COMPANY-ID.               ECS096
02444      MOVE '7'                     TO CF-RECORD-TYPE.              ECS096
02445      MOVE +0                      TO CF-SEQUENCE-NO.              ECS096
02446                                                                   ECS096
02447      START ELCNTL                                                 ECS096
02448          KEY IS EQUAL CF-CONTROL-PRIMARY.                         ECS096
02449                                                                   ECS096
02450      IF  W-ELCNTL-FILE-STATUS GREATER THAN '00'                   ECS096
02451          MOVE '**** ERROR STARTING ELCNTL FILE ****'              ECS096
02452                                  TO WS-MESSAGE                    ECS096
02453          MOVE CF-CONTROL-PRIMARY TO WS-MESSAGE-DATA               ECS096
02454          MOVE '0'                TO WAC-1                         ECS096
02455          MOVE '2'                TO WAC-2                         ECS096
02456          MOVE W-ELCNTL-FILE-STATUS                                ECS096
02457                                  TO WAC-3-4                       ECS096
02458                                     WS-ABEND-FILE-STATUS          ECS096
02459          MOVE WS-ABEND-CODE      TO WS-RETURN-CODE                ECS096
02460          GO TO ABEND-PGM.                                         ECS096
02461                                                                   ECS096
02462      MOVE +1                     TO CLAS-INDEXM.                  ECS096
02463      MOVE +99                    TO LINE-CNT.                     ECS096
02464                                                                   ECS096
02465      PERFORM 0200-PROCESS-MORT-RCRD THRU 0200-EXIT                ECS096
02466              UNTIL                                                ECS096
02467          W-LAST-MORT-TABLE-PROCESSED.                             ECS096
02468                                                                   ECS096
02469  0100-EXIT.                                                       ECS096
02470      EXIT.                                                        ECS096
02471                                  EJECT                            ECS096
02472  0200-PROCESS-MORT-RCRD.                                          ECS096
02473                                                                   ECS096
02474      READ ELCNTL RECORD                                           ECS096
02475                                                                   ECS096
02476      IF  W-ELCNTL-FILE-STATUS EQUAL '00'                          ECS096
02477          PERFORM 0300-PROCESS-ONLINE-TABLES THRU 0300-EXIT        ECS096
02478                  VARYING                                          ECS096
02479              CF-MORT-NDX FROM +1 BY +1                            ECS096
02480                  UNTIL                                            ECS096
02481              CF-MORT-NDX GREATER THAN +9                          ECS096
02482                  OR                                               ECS096
02483              CF-MORT-TABLE (CF-MORT-NDX) EQUAL LOW-VALUES         ECS096
02484          IF  CF-MORT-TABLE (CF-MORT-NDX) EQUAL LOW-VALUES         ECS096
02485              MOVE 'Y'            TO W-LAST-MORT-TABLE-IND         ECS096
02486              GO TO 0200-EXIT                                      ECS096
02487          ELSE                                                     ECS096
02488              ADD +1              TO CF-SEQUENCE-NO                ECS096
02489              GO TO 0200-EXIT                                      ECS096
02490      ELSE                                                         ECS096
02491          IF  W-ELCNTL-FILE-STATUS EQUAL '10' OR '23'              ECS096
02492              MOVE 'Y'            TO W-LAST-MORT-TABLE-IND         ECS096
02493              GO TO 0200-EXIT                                      ECS096
02494      ELSE                                                         ECS096
02495          MOVE '**** ERROR READING CNTL FILE ****'                 ECS096
02496                                  TO WS-MESSAGE                    ECS096
02497          MOVE CF-CONTROL-PRIMARY TO WS-MESSAGE-DATA               ECS096
02498          MOVE '0'                TO WAC-1                         ECS096
02499          MOVE '4'                TO WAC-2                         ECS096
02500          MOVE W-ELCNTL-FILE-STATUS                                ECS096
02501                                  TO WAC-3-4                       ECS096
02502                                     WS-ABEND-FILE-STATUS          ECS096
02503          MOVE WS-ABEND-CODE      TO WS-RETURN-CODE                ECS096
02504          GO TO ABEND-PGM.                                         ECS096
02505                                                                   ECS096
02506  0200-EXIT.                                                       ECS096
02507      EXIT.                                                        ECS096
02508                                  EJECT                            ECS096
02509  0300-PROCESS-ONLINE-TABLES.                                      ECS096
02510                                                                   ECS096
02511      MOVE CF-MORT-TABLE-LINE (CF-MORT-NDX)                        ECS096
02512                                  TO LX-SAVE.                      ECS096
02513                                                                   ECS096
02514      IF  NULLTBL                                                  ECS096
02515          GO TO 0300-EXIT.                                         ECS096
02516                                                                   ECS096
02517      ADD 1                       TO RFAC-NEW.                     ECS096
02518      PERFORM 1000-CALC-DX.                                        ECS096
02519      PERFORM 2000-PRINT-DX-TABLE.                                 ECS096
02520      PERFORM 3000-CALC-RESERVE.                                   ECS096
02521                                                                   ECS096
02522  0300-EXIT.                                                       ECS096
02523      EXIT.                                                        ECS096
02524                                  EJECT                            ECS096
02525  1000-CALC-DX   SECTION.                                          ECS096
02526                                                                   ECS096
02527  1000-CALC.                                                       ECS096
02528                                                                   ECS096
02529      MOVE LX-TABLE               TO TABLE-CODE.                   ECS096
02530      MOVE LX-AGE-METHOD          TO TABLE-AL-AN.                  ECS096
02531      MOVE SPACE                  TO TABLE-TYPE.                   ECS096
02532                                                                   ECS096
02533      MOVE +0                     TO SUB2.                         ECS096
02534                                                                   ECS096
02535      IF 58CET                                                     ECS096
02536          MOVE 58CET-TABLE        TO LX-WORK-TABLE.                ECS096
02537                                                                   ECS096
02538      IF 58CSO                                                     ECS096
02539          MOVE 58CSO-TABLE        TO LX-WORK-TABLE.                ECS096

      ****  FOR AHL WE PLUG AN IN AGE METHOD BECAUSE THE LX'S ARE
      ****  ALREADY ADJUSTED FOR AL BUT WE STILL WANT TO CALCULATE
      ****  MORTALITY AGE AS AGE LAST SO WE SET UP EL602 (ELCNTL) WITH
      ****  AGE LAST.  ?? 

           IF (DTE-CLIENT = 'AHL')
              AND (LX-TABLE = '58CSO')
              AND (LX-TABLE-TYPE = 'J')
              MOVE AHL-USER-TABLE1     TO LX-WORK-TABLE
              MOVE 'AN'                TO LX-AGE-METHOD
           END-IF

02541      IF 58FSO                                                     ECS096
02542          MOVE 58FSO-TABLE        TO LX-WORK-TABLE.                ECS096
02543                                                                   ECS096
CIDMOD     IF 58UET                                                     00021241
CIDMOD         MOVE 58UET-TABLE        TO LX-WORK-TABLE.                00021242
CIDMOD                                                                  00021243
CIDMOD     IF 58USO                                                     00021241
CIDMOD         MOVE 58USO-TABLE        TO LX-WORK-TABLE.                00021242
CIDMOD                                                                  00021243
02544      IF 41CSO                                                     ECS096
02545          MOVE 41CSO-TABLE        TO LX-WORK-TABLE.                ECS096
02546                                                                   ECS096
02547      IF 60CSG                                                     ECS096
02548          MOVE 60CSG-TABLE        TO LX-WORK-TABLE.                ECS096
02549                                                                   ECS096
CIDMOD     IF 80CSO                                                     00021241
CIDMOD         MOVE 80CSO-TABLE        TO LX-WORK-TABLE.                00021242
CIDMOD                                                                  00021243
           IF (DTE-CLIENT = 'AHL')
              AND (LX-TABLE = '80CSO')
              AND (LX-TABLE-TYPE = 'J')
              MOVE 'AN'                TO LX-AGE-METHOD
              MOVE AHL-USER-TABLE2     TO LX-WORK-TABLE
           END-IF

02550      IF 80MSO                                                     ECS096
02551          IF AGE-LAST                                              ECS096
02552              MOVE 80MSO-TABLE    TO LX-WORK-TABLE                 ECS096
02553          ELSE                                                     ECS096
02554              IF AGE-NEAR                                          ECS096
02555                  MOVE 80MSO-AN-TABLE                              ECS096
02556                                  TO LX-WORK-TABLE.                ECS096
02557                                                                   ECS096
02558      IF 80FSO                                                     ECS096
02559          IF AGE-LAST                                              ECS096
02560              MOVE 80FSO-TABLE    TO LX-WORK-TABLE                 ECS096
02561          ELSE                                                     ECS096
02562              IF AGE-NEAR                                          ECS096
02563                  MOVE 80FSO-AN-TABLE                              ECS096
02564                                  TO LX-WORK-TABLE.                ECS096
02565                                                                   ECS096
02566      IF 80MET                                                     ECS096
02567          MOVE 80MET-TABLE        TO LX-WORK-TABLE.                ECS096
02568                                                                   ECS096
02569      IF 80FET                                                     ECS096
02570          MOVE 80FET-TABLE        TO LX-WORK-TABLE.                ECS096
02571                                                                   ECS096
02572      IF 80GBT                                                     ECS096
02573          MOVE 80GBT-TABLE        TO LX-WORK-TABLE.                ECS096
02574                                                                   ECS096
CIDMOD     IF 80UET                                                     ECS096
CIDMOD         MOVE 80UET-TABLE        TO LX-WORK-TABLE.                ECS096
CIDMOD                                                                  ECS096
LGC188     IF 80USO                                                     ECS096
LGC188         MOVE 80USO-TABLE        TO LX-WORK-TABLE.                ECS096
LGC188                                                                  ECS096
101005     IF 01CSO
101005        IF AGE-LAST
101005           MOVE 01CSO-TABLE      TO LX-WORK-TABLE
101005        ELSE
101005           IF AGE-NEAR
101005              MOVE 01CSO-AN-TABLE
101005                                 TO LX-WORK-TABLE
                 END-IF
              END-IF
           END-IF

02575      IF USERTBL                                                   ECS096
02576         IF DTE-CLIENT EQUAL 'HER'                                 ECS096
02577            IF (LX-MORT-CDE EQUAL 'L360' OR 'L310') AND            ECS096
02578               (LX-TABLE-TYPE EQUAL 'S')                           ECS096
02579               MOVE HER-USER-TABLE1 TO LX-WORK-TABLE               ECS096
02580            ELSE                                                   ECS096
02581            IF (LX-MORT-CDE EQUAL 'L360' OR 'L310') AND            ECS096
02582               (LX-TABLE-TYPE EQUAL 'J')                           ECS096
02583               MOVE HER-USER-TABLE2 TO LX-WORK-TABLE               ECS096
02584            ELSE                                                   ECS096
02585            IF (LX-MORT-CDE EQUAL 'G350') AND                      ECS096
02586               (LX-TABLE-TYPE EQUAL 'S')                           ECS096
02587               MOVE HER-USER-TABLE3 TO LX-WORK-TABLE               ECS096
02588            ELSE                                                   ECS096
02589            IF (LX-MORT-CDE EQUAL 'G350') AND                      ECS096
02590               (LX-TABLE-TYPE EQUAL 'J')                           ECS096
02591               MOVE HER-USER-TABLE4 TO LX-WORK-TABLE               ECS096
02592            ELSE                                                   ECS096
02593               MOVE USER-TABLE    TO LX-WORK-TABLE                 ECS096
02594         ELSE                                                      ECS096
02595            MOVE USER-TABLE       TO LX-WORK-TABLE.                ECS096
02596                                                                   ECS096
02597                                  EJECT                            ECS096
02598  1030-INTERPOLATE-LX-TABLE.                                       ECS096
02599                                                                   ECS096
02600      IF 58CET                                                     ECS096
02601        AND  AGE-LAST                                              ECS096
02602          PERFORM 1600-INTERPOLATE-LX-TABLE THRU 1699-EXIT.        ECS096
02603                                                                   ECS096
02604      IF 58CSO                                                     ECS096
02605        AND  AGE-LAST                                              ECS096
02606          PERFORM 1600-INTERPOLATE-LX-TABLE THRU 1699-EXIT.        ECS096
02607                                                                   ECS096
02608      IF 58FSO                                                     ECS096
02609        AND  AGE-LAST                                              ECS096
02610          PERFORM 1600-INTERPOLATE-LX-TABLE THRU 1699-EXIT.        ECS096
02611                                                                   ECS096
CIDMOD*    IF 80MET                                                     ECS096
CIDMOD*      AND  AGE-LAST                                              ECS096
CIDMOD*        PERFORM 1600-INTERPOLATE-LX-TABLE THRU 1699-EXIT.        ECS096
02615                                                                   ECS096
02616      IF 80FET                                                     ECS096
02617        AND  AGE-LAST                                              ECS096
02618          PERFORM 1600-INTERPOLATE-LX-TABLE THRU 1699-EXIT.        ECS096
02619                                                                   ECS096
02620      IF 80GBT                                                     ECS096
02621        AND  AGE-LAST                                              ECS096
02622          PERFORM 1600-INTERPOLATE-LX-TABLE THRU 1699-EXIT.        ECS096
02623                                                                   ECS096
02624      IF LX-Q-ADJ                                                  ECS096
02625          PERFORM 1300-ADJ-Q THRU 1399-ADJ-X.                      ECS096
02626                                                                   ECS096
02627      IF LX-JOINT-CODE   IS EQUAL TO 'A'                           ECS096
02628          PERFORM 1100-JOINT-A THRU 1199-EXIT.                     ECS096
02629                                                                   ECS096
02630      IF LX-JOINT-CODE   IS EQUAL TO 'V'                           ECS096
02631          PERFORM 1200-JOINT-V THRU 1299-EXIT.                     ECS096
02632                                                                   ECS096
02633      PERFORM 1700-BUILD-VX-TABLE THRU 1799-EXIT.                  ECS096
02634                                                                   ECS096
02635      PERFORM 1800-PRODUCE-DX THRU 1899-EXIT.                      ECS096
02636                                                                   ECS096
02637      PERFORM 1900-PRODUCE-CX THRU 1989-EXIT.                      ECS096
02638      GO TO 1999-EXIT.                                             ECS096
02639                                  EJECT                            ECS096
02640  1100-JOINT-A.                                                    ECS096
02641 *         ********  ADJUSTMENT METHOD A  ***********              ECS096
02642 *  ADJUST LX TABLE FOR JOINT LX = (L(X) * L(X)) / L(1)            ECS096
02643                                                                   ECS096
02644      MOVE B1                     TO X1.                           ECS096
02645                                                                   ECS096
02646  1100-JOINT-LOOP.                                                 ECS096
02647                                                                   ECS096
02648      IF  LX-VALUE (X1) NUMERIC                                    ECS096
02649          IF  LX-VALUE (X1) GREATER THAN ZERO                      ECS096
02650              COMPUTE LX-VALUE (X1) ROUNDED =                      ECS096
02651                  (LX-VALUE (X1) * LX-VALUE (X1)) / LX-VALUE (1)   ECS096
02652          ELSE                                                     ECS096
02653              NEXT SENTENCE                                        ECS096
02654      ELSE                                                         ECS096
02655          GO TO 1199-EXIT.                                         ECS096
02656                                                                   ECS096
02657      ADD B1                      TO X1.                           ECS096
02658                                                                   ECS096
02659      IF X1  IS LESS THAN  B101                                    ECS096
02660          GO TO 1100-JOINT-LOOP.                                   ECS096
02661                                                                   ECS096
02662  1199-EXIT.                                                       ECS096
02663      EXIT.                                                        ECS096
02664                                  EJECT                            ECS096
02665  1200-JOINT-V.                                                    ECS096
02666 *         ********  ADJUSTMENT METHOD V  ***********              ECS096
02667 *  ADJUST LX TABLE FOR JOINT L(X+1) = L(X) * L (X+1) * L (X-2)    ECS096
02668 *                                   / ( L (X) * L (X-3))          ECS096
02669                                                                   ECS096
02670      MOVE LX-VALUE (1)           TO JX-VALUE (1).                 ECS096
02671                                                                   ECS096
02672      COMPUTE FACTOR-A ROUNDED = LX-VALUE (2) / LX-VALUE (1).      ECS096
02673                                                                   ECS096
02674      COMPUTE JX-VALUE (2) ROUNDED = LX-VALUE (2) * FACTOR-A.      ECS096
02675                                                                   ECS096
02676      COMPUTE JX-VALUE (3) ROUNDED = JX-VALUE (2) * FACTOR-A       ECS096
02677                                   * LX-VALUE (3) / LX-VALUE (2).  ECS096
02678                                                                   ECS096
02679      COMPUTE JX-VALUE (4) ROUNDED = JX-VALUE (3) * LX-VALUE (4)   ECS096
02680                                   * FACTOR-A / LX-VALUE (3).      ECS096
02681                                                                   ECS096
02682      MOVE B5                     TO X1.                           ECS096
02683                                                                   ECS096
02684  1200-JOINT-LOOP.                                                 ECS096
02685                                                                   ECS096
02686      COMPUTE X2 = X1 - B1.                                        ECS096
02687      COMPUTE X3 = X2 - B2.                                        ECS096
02688      COMPUTE X4 = X3 - B1.                                        ECS096
02689                                                                   ECS096
02690      COMPUTE FACTOR-A ROUNDED = LX-VALUE (X3) / LX-VALUE (X4).    ECS096
02691                                                                   ECS096
02692      COMPUTE JX-VALUE (X1) ROUNDED = JX-VALUE (X2) *              ECS096
02693          FACTOR-A * LX-VALUE (X1) / LX-VALUE (X2).                ECS096
02694                                                                   ECS096
02695      ADD B1                      TO X1.                           ECS096
02696                                                                   ECS096
02697      IF X1  IS LESS THAN  B101                                    ECS096
02698          GO TO 1200-JOINT-LOOP.                                   ECS096
02699                                                                   ECS096
02700      MOVE JX-WORK-TABLE          TO LX-WORK-TABLE.                ECS096
02701                                                                   ECS096
02702  1299-EXIT.                                                       ECS096
02703      EXIT.                                                        ECS096
02704                                  EJECT                            ECS096
02705  1300-ADJ-Q.                                                      ECS096
02706 *  ADJUST LX TABLE FOR RESERVE ADJUSTMENT FACTOR                  ECS096
02707 *                                                                 ECS096
02708                                                                   ECS096
02709      MOVE LX-VALUE (1)           TO JX-VALUE (1).                 ECS096
02710      MOVE B2                     TO X1.                           ECS096
02711                                                                   ECS096
02712  1300-ADJ-LOOP.                                                   ECS096
02713                                                                   ECS096
02714      COMPUTE X2 = X1 - B1.                                        ECS096
02715                                                                   ECS096
02716      COMPUTE FACTOR-A ROUNDED = JX-VALUE (X2) / LX-VALUE (X2).    ECS096
02717                                                                   ECS096
02718      COMPUTE FACTOR-B ROUNDED =                                   ECS096
02719          (LX-VALUE (X2) - LX-VALUE (X1)) * LX-RES-ADJ             ECS096
02720                                                                   ECS096
02721      COMPUTE JX-VALUE (X1) ROUNDED = FACTOR-A *                   ECS096
02722          (LX-VALUE (X2) - FACTOR-B).                              ECS096
02723                                                                   ECS096
02724      ADD B1                      TO X1.                           ECS096
02725                                                                   ECS096
02726      IF X1  IS LESS THAN  B101                                    ECS096
02727          GO TO 1300-ADJ-LOOP.                                     ECS096
02728                                                                   ECS096
02729      MOVE JX-WORK-TABLE          TO LX-WORK-TABLE.                ECS096
02730                                                                   ECS096
02731  1399-ADJ-X.                                                      ECS096
02732      EXIT.                                                        ECS096
02733                                  EJECT                            ECS096
02734  1600-INTERPOLATE-LX-TABLE.                                       ECS096
02735                                                                   ECS096
02736      MOVE +0                     TO SUB1.                         ECS096
02737      MOVE +1                     TO SUB2.                         ECS096
02738                                                                   ECS096
02739      PERFORM 1650-COMPUTE-LX-MEAN THRU 1650-EXIT 100 TIMES.       ECS096
02740                                                                   ECS096
02741      MOVE AL-WORK-TABLE          TO LX-WORK-TABLE.                ECS096
02742                                                                   ECS096
02743      GO TO 1699-EXIT.                                             ECS096
02744                                                                   ECS096
02745  1650-COMPUTE-LX-MEAN.                                            ECS096
02746                                                                   ECS096
02747      ADD +1                      TO SUB1   SUB2.                  ECS096
02748                                                                   ECS096
02749      IF SUB1  IS EQUAL TO +100                                    ECS096
02750          COMPUTE AL-VALUE (SUB1) = LX-VALUE (SUB1) / 2            ECS096
02751      ELSE                                                         ECS096
02752          MOVE LX-VALUE (SUB1)    TO HOLD-1ST-LX                   ECS096
02753          MOVE LX-VALUE (SUB2)    TO HOLD-2ND-LX                   ECS096
02754          COMPUTE NEW-AL-VALUE = HOLD-2ND-LX +                     ECS096
02755              ((HOLD-1ST-LX - HOLD-2ND-LX) / 2)                    ECS096
02756          MOVE NEW-AL-VALUE       TO AL-VALUE (SUB1).              ECS096
02757                                                                   ECS096
02758  1650-EXIT.                                                       ECS096
02759      EXIT.                                                        ECS096
02760                                                                   ECS096
02761  1699-EXIT.                                                       ECS096
02762      EXIT.                                                        ECS096
02763                                  EJECT                            ECS096
02764  1700-BUILD-VX-TABLE.                                             ECS096
02765      MOVE 1.0                    TO VX-VALUE (1).                 ECS096
02766                                                                   ECS096
02767      COMPUTE VX-MULTIPLIER = 1 / (1 + LX-INTEREST).               ECS096
02768                                                                   ECS096
02769      MOVE VX-MULTIPLIER          TO VX-VALUE (2)                  ECS096
02770                                      VX-RESULT.                   ECS096
02771      MOVE +2                     TO SUB1.                         ECS096
02772                                                                   ECS096
02773      COMPUTE TABLE-SIZE-VX = TABLE-SIZE - 2.                      ECS096
02774                                                                   ECS096
02775      PERFORM 1750-VX-MULTIPLY-LOOP THRU 1759-EXIT                 ECS096
02776          TABLE-SIZE-VX  TIMES.                                    ECS096
02777                                                                   ECS096
02778      GO TO 1799-EXIT.                                             ECS096
02779                                                                   ECS096
02780  1750-VX-MULTIPLY-LOOP.                                           ECS096
02781      ADD +1                      TO SUB1.                         ECS096
02782                                                                   ECS096
02783      COMPUTE VX-RESULT = VX-RESULT * VX-MULTIPLIER.               ECS096
02784                                                                   ECS096
02785      MOVE VX-RESULT              TO VX-VALUE (SUB1).              ECS096
02786                                                                   ECS096
02787  1759-EXIT.                                                       ECS096
02788      EXIT.                                                        ECS096
02789                                                                   ECS096
02790  1799-EXIT.                                                       ECS096
02791      EXIT.                                                        ECS096
02792                                  EJECT                            ECS096
02793  1800-PRODUCE-DX.                                                 ECS096
02794                                                                   ECS096
02795      MOVE +0                     TO SUB1.                         ECS096
02796                                                                   ECS096
02797      PERFORM 1850-GENERATE-DX THRU 1859-EXIT                      ECS096
02798          100 TIMES.                                               ECS096
02799                                                                   ECS096
02800      GO TO 1899-EXIT.                                             ECS096
02801                                                                   ECS096
02802  1850-GENERATE-DX.                                                ECS096
02803                                                                   ECS096
02804      ADD +1                      TO SUB1.                         ECS096
02805                                                                   ECS096
02806      COMPUTE DX-WORK ROUNDED =                                    ECS096
02807          LX-VALUE (SUB1) * VX-VALUE (SUB1).                       ECS096
02808                                                                   ECS096
02809      MOVE DX-WORK                TO DX-VALUE (SUB1).              ECS096
02810                                                                   ECS096
02811  1859-EXIT.                                                       ECS096
02812      EXIT.                                                        ECS096
02813                                                                   ECS096
02814  1899-EXIT.                                                       ECS096
02815      EXIT.                                                        ECS096
02816                                  EJECT                            ECS096
02817  1900-PRODUCE-CX.                                                 ECS096
02818      MOVE +0                     TO SUB1   DX-VALUE (101).        ECS096
02819                                                                   ECS096
02820      COMPUTE V = 1.0 / (1 + LX-INTEREST).                         ECS096
02821                                                                   ECS096
02822      PERFORM 1950-GENERATE-CX THRU 1959-EXIT                      ECS096
02823          100 TIMES                                                ECS096
02824                                                                   ECS096
02825      GO TO 1989-EXIT.                                             ECS096
02826                                                                   ECS096
02827  1950-GENERATE-CX.                                                ECS096
02828                                                                   ECS096
02829      ADD +1                      TO SUB1.                         ECS096
02830                                                                   ECS096
02831      COMPUTE SUB2 = SUB1 + 1.                                     ECS096
02832                                                                   ECS096
02833      MOVE DX-VALUE (SUB1)        TO DX.                           ECS096
02834      MOVE DX-VALUE (SUB2)        TO DY.                           ECS096
02835                                                                   ECS096
02836      COMPUTE CX-WORK ROUNDED = (V * DX) - DY.                     ECS096
02837                                                                   ECS096
02838      MOVE CX-WORK                TO CX-VALUE (SUB1).              ECS096
02839                                                                   ECS096
02840  1959-EXIT.                                                       ECS096
02841      EXIT.                                                        ECS096
02842                                                                   ECS096
02843  1989-EXIT.                                                       ECS096
02844      EXIT.                                                        ECS096
02845                                                                   ECS096
02846  1999-EXIT.                                                       ECS096
02847      EXIT.                                                        ECS096
02848                                  EJECT                            ECS096
02849  2000-PRINT-DX-TABLE SECTION.                                     ECS096
02850                                                                   ECS096
02851      PERFORM 4100-PRINT-DX                                        ECS096
02852          VARYING  SUB1  FROM  1  BY  1                            ECS096
02853              UNTIL  SUB1  IS GREATER THAN  +100.                  ECS096
02854                                                                   ECS096
02855      MOVE 99                     TO LINE-CNT.                     ECS096
02856                                                                   ECS096
02857  2999-EXIT.                                                       ECS096
02858      EXIT.                                                        ECS096
02859                                  EJECT                            ECS096
02860  3000-CALC-RESERVE SECTION.                                       ECS096
02861                                                                   ECS096
02862      COMPUTE RF-INT-ADJ = LX-INTEREST + 1.0.                      ECS096
02863                                                                   ECS096
02864      MOVE LX-RES-ADJ             TO RF-RESV-ADJ.                  ECS096
02865      MOVE +1                     TO RF-JOINT-ADJ.                 ECS096
02866 *    MOVE LX-JOINT-FACTOR        TO RF-JOINT-ADJ.                 ECS096
02867      MOVE LX-ADJUSTMENT-DIRECTION                                 ECS096
02868                                  TO RF-PLUS-MINUS.                ECS096
02869                                                                   ECS096
02870      PERFORM 4200-WRITE-CX-DX.                                    ECS096
02871                                                                   ECS096
02872  3999-EXIT.                                                       ECS096
02873      EXIT.                                                        ECS096
02874                                  EJECT                            ECS096
02875  4100-PRINT-DX SECTION.                                           ECS096
02876                                                                   ECS096
02877      IF LINE-CNT  IS GREATER THAN  56                             ECS096
02878          PERFORM 8000-HEADING-ROUTINE THRU 8199-EXIT.             ECS096
02879                                                                   ECS096
02880      MOVE SPACES                 TO DETAIL-LINE.                  ECS096
02881                                                                   ECS096
02882      COMPUTE DTL-AGE = SUB1 - 1.                                  ECS096
02883                                                                   ECS096
02884      MOVE LX-VALUE (SUB1)        TO DTL-LX.                       ECS096
02885      MOVE VX-VALUE (SUB1)        TO DTL-VX.                       ECS096
02886      MOVE DX-VALUE (SUB1)        TO DTL-DX.                       ECS096
02887      MOVE CX-VALUE (SUB1)        TO DTL-CX.                       ECS096
02888      MOVE DETAIL-LINE            TO P-DATA.                       ECS096
02889                                                                   ECS096
02890      PERFORM 8500-PRT-RTN.                                        ECS096
02891                                                                   ECS096
02892      ADD 1                       TO LINE-CNT.                     ECS096
02893                                                                   ECS096
02894  4199-EXIT.                                                       ECS096
02895      EXIT.                                                        ECS096
02896                                  EJECT                            ECS096
02897  4200-WRITE-CX-DX SECTION.                                        ECS096
02898      MOVE LX-MORT-CDE            TO RF-CODE.                      ECS096
02899      MOVE LX-TABLE-TYPE          TO RF-TYPE.                      ECS096
02900      MOVE +1                     TO SUB1.                         ECS096
02901                                                                   ECS096
02902  4200-CLEAR-1.                                                    ECS096
02903      MOVE +0                     TO RF-CX-FACTOR (SUB1)           ECS096
02904                                                                   ECS096
02905      ADD +1                      TO SUB1.                         ECS096
02906                                                                   ECS096
02907      IF SUB1  IS LESS THAN  101                                   ECS096
02908          GO TO 4200-CLEAR-1.                                      ECS096
02909                                                                   ECS096
02910      MOVE +1                     TO SUB1.                         ECS096
02911                                                                   ECS096
02912  4200-CLEAR-2.                                                    ECS096
02913      MOVE +0                     TO RF-DX-FACTOR (SUB1)           ECS096
02914                                                                   ECS096
02915      ADD +1                      TO SUB1.                         ECS096
02916                                                                   ECS096
02917      IF SUB1  IS LESS THAN  101                                   ECS096
02918          GO TO 4200-CLEAR-2.                                      ECS096
02919                                                                   ECS096
02920      MOVE +0                     TO SUB1.                         ECS096
02921                                                                   ECS096
02922  4200-LOOP.                                                       ECS096
02923      ADD +1                      TO SUB1.                         ECS096
02924                                                                   ECS096
02925      MOVE CX-VALUE (SUB1)        TO RF-CX-FACTOR (SUB1).          ECS096
02926      MOVE DX-VALUE (SUB1)        TO RF-DX-FACTOR (SUB1).          ECS096
02927                                                                   ECS096
02928      IF SUB1  IS LESS THAN  100                                   ECS096
02929          GO TO 4200-LOOP.                                         ECS096
02930                                                                   ECS096
02931      MOVE RF-REC                 TO NEW-RFAC-REC.                 ECS096
02932                                                                   ECS096
02933      WRITE NEW-RFAC-REC.                                          ECS096
02934                                                                   ECS096
02935      ADD 1                       TO RFAC-OUT.                     ECS096
02936                                                                   ECS096
02937  4299-EXIT.                                                       ECS096
02938      EXIT.                                                        ECS096
02939                                  EJECT                            ECS096
02940  8000-HEADING SECTION.                                            ECS096
02941                                                                   ECS096
02942  8000-HEADING-ROUTINE.                                            ECS096
02943      ADD 1                       TO PAGE-CNT.                     ECS096
02944                                                                   ECS096
02945      MOVE PAGE-CNT               TO HD-PAGE.                      ECS096
02946      MOVE HEAD-LINE-1            TO P-DATA.                       ECS096
02947      MOVE '1'                    TO X.                            ECS096
02948                                                                   ECS096
02949      PERFORM 8500-PRT-RTN.                                        ECS096
02950                                                                   ECS096
02951      MOVE HEAD-LINE-2            TO P-DATA.                       ECS096
02952      MOVE SPACE                  TO X.                            ECS096
02953                                                                   ECS096
02954      PERFORM 8500-PRT-RTN.                                        ECS096
02955                                                                   ECS096
02956      MOVE HEAD-LINE-3            TO P-DATA.                       ECS096
02957                                                                   ECS096
02958      PERFORM 8500-PRT-RTN.                                        ECS096
02959                                                                   ECS096
02960      MOVE 4                      TO LINE-CNT.                     ECS096
02961                                                                   ECS096
02962  8099-EXIT.                                                       ECS096
02963      EXIT.                                                        ECS096
02964                                  EJECT                            ECS096
02965  8100-HEAD-LINE-4-6.                                              ECS096
02966                                                                   ECS096
02967  8100-HEAD.                                                       ECS096
02968                                                                   ECS096
02969      MOVE LX-MORT-CDE            TO HD-MORT.                      ECS096
02970      MOVE LX-INTEREST            TO HD-INT.                       ECS096
02971      MOVE LX-AGE-METHOD          TO HD-AGE.                       ECS096
02972                                                                   ECS096
02973      IF  LX-TABLE EQUAL 'XXXXX'                                   ECS096
02974              AND                                                  ECS096
02975          LX-COMMENTS GREATER THAN SPACES                          ECS096
02976          MOVE LX-COMMENTS        TO HD-TABLE                      ECS096
02977      ELSE                                                         ECS096
02978          MOVE LX-TABLE           TO HD-TABLE.                     ECS096
02979 *                                                                 ECS096
02980 ***NOTE THIS MIGHT STILL BE VALID FOR THOSE THAT ARE NOT LISTED** ECS096
02981 *    IF (58CSO  OR  58CET  OR  58FSO  OR  80MSO  OR  80FSO        ECS096
02982 *      OR  80MET  OR  80FET  OR 80GBT)                            ECS096
02983 *      AND  AGE-NEAR                                              ECS096
02984 *        MOVE 'AGE NEAR'         TO HD-AGE.                       ECS096
02985 *                                                                 ECS096
02986 *    IF (58CSO  OR  58CET  OR  58FSO  OR  80MSO  OR  80FSO        ECS096
02987 *      OR  80MET OR 80FET  OR  80GBT)                             ECS096
02988 *      AND  AGE-LAST                                              ECS096
02989 *        MOVE 'AGE LAST'         TO HD-AGE.                       ECS096
02990                                                                   ECS096
02991      MOVE LX-TABLE-TYPE          TO HD-BEN.                       ECS096
02992      MOVE HEAD-LINE-4            TO P-DATA.                       ECS096
02993      MOVE '0'                    TO X.                            ECS096
02994                                                                   ECS096
02995      PERFORM 8500-PRT-RTN.                                        ECS096
02996                                                                   ECS096
02997      MOVE LX-RES-ADJ             TO HD-RES-ADJ.                   ECS096
02998      MOVE LX-JOINT-FACTOR        TO HD-JOINT-FACT.                ECS096
02999                                                                   ECS096
03000      IF LX-JOINT-CODE   IS EQUAL TO 'A'                           ECS096
03001          MOVE 'TABLE-A'          TO HD-JNT-CODE                   ECS096
03002      ELSE                                                         ECS096
03003          IF LX-JOINT-CODE   IS EQUAL TO 'V'                       ECS096
03004              MOVE 'TABLE-V'      TO HD-JNT-CODE                   ECS096
03005          ELSE                                                     ECS096
03006              MOVE 'NONE'         TO HD-JNT-CODE.                  ECS096
03007                                                                   ECS096
03008      IF LX-PC-Q         IS EQUAL TO 'Y'                           ECS096
03009          MOVE 'YES'              TO HD-PC-Q-ADJ                   ECS096
03010      ELSE                                                         ECS096
03011          MOVE 'NO'               TO HD-PC-Q-ADJ.                  ECS096
03012                                                                   ECS096
03013      MOVE HEAD-LINE-5            TO P-DATA.                       ECS096
03014                                                                   ECS096
03015      PERFORM 8500-PRT-RTN.                                        ECS096
03016                                                                   ECS096
03017      MOVE HEAD-LINE-6            TO P-DATA.                       ECS096
03018      MOVE '0'                    TO X.                            ECS096
03019                                                                   ECS096
03020      PERFORM 8500-PRT-RTN.                                        ECS096
03021                                                                   ECS096
03022      MOVE SPACES                 TO P-DATA.                       ECS096
03023                                                                   ECS096
03024      PERFORM 8500-PRT-RTN.                                        ECS096
03025                                                                   ECS096
03026      ADD 5                       TO LINE-CNT.                     ECS096
03027                                                                   ECS096
03028  8199-EXIT.                                                       ECS096
03029      EXIT.                                                        ECS096
03030                                  EJECT                            ECS096
03031  8500-PRT-RTN.                                                    ECS096
03032                              COPY ELCPRT2.                        ECS096
03033                                                                   ECS096
03034      MOVE SPACE                  TO X.                            ECS096
03035                                                                   ECS096
03036  8599-EXIT.                                                       ECS096
03037      EXIT.                                                        ECS096
03038                                  EJECT                            ECS096
03039  9250-EOJ-MESSAGE.                                                ECS096
03040      PERFORM 8000-HEADING-ROUTINE THRU 8099-EXIT.                 ECS096
03041                                                                   ECS096
03042      MOVE 'PROGRAM COMPLETED'    TO P-DATA.                       ECS096
03043      MOVE '0'                    TO X.                            ECS096
03044                                                                   ECS096
03045      PERFORM 8500-PRT-RTN.                                        ECS096
03046                                                                   ECS096
03047      MOVE SPACES                 TO P-DATA.                       ECS096
03048                                                                   ECS096
03049      PERFORM 8500-PRT-RTN.                                        ECS096
03050                                                                   ECS096
03051      MOVE 'CONTROL RECORDS '     TO TL-DESC.                      ECS096
03052      MOVE RFAC-NEW               TO TL-CNT.                       ECS096
03053      MOVE TOTAL-LINE             TO P-DATA.                       ECS096
03054      MOVE '0'                    TO X.                            ECS096
03055                                                                   ECS096
03056      PERFORM 8500-PRT-RTN.                                        ECS096
03057                                                                   ECS096
03058      MOVE 'OUTPUT RFAC RECORDS'  TO TL-DESC.                      ECS096
03059      MOVE RFAC-OUT               TO TL-CNT.                       ECS096
03060      MOVE TOTAL-LINE             TO P-DATA.                       ECS096
03061      MOVE '0'                    TO X.                            ECS096
03062                                                                   ECS096
03063      PERFORM 8500-PRT-RTN.                                        ECS096
03064                                                                   ECS096
03065  9299-EXIT.                                                       ECS096
03066      EXIT.                                                        ECS096
03067                                                                   ECS096
03068  9990-STOP-RUN-RTN SECTION.                                       ECS096
03069                                                                   ECS096
03070  9999-STOP-RUN.                                                   ECS096
03071      PERFORM 9250-EOJ-MESSAGE THRU 9299-EXIT.                     ECS096
03072                                                                   ECS096
03073  9999-CLOSE-UP.                                                   ECS096
03074                              COPY ELCPRTC.                        ECS096
03075                                                                   ECS096
03076      CLOSE ELCNTL                                                 ECS096
03077            NEW-RFAC-FILE                                          ECS096
03078            PRNTR.                                                 ECS096
03079                                                                   ECS096
03080      GOBACK.                                                      ECS096
03081                                                                   ECS096
03082  ABEND-PGM SECTION.                                               ECS096
03083                              COPY ELCABEND.                       ECS096
