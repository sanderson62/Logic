00001  IDENTIFICATION DIVISION.                                         03/09/98
00002                                                                   ECS049
00003  PROGRAM-ID.                ECS049.                                  LV006
00004 *              PROGRAM CONVERTED BY                               ECS049
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ECS049
00006 *              CONVERSION DATE 02/20/96 19:05:51.                 ECS049
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          ECS049
00008 *                           VMOD=2.004                            ECS049
00009                                                                   ECS049
00010 *AUTHOR.        LOGIC, INC.                                       ECS049
00011 *               DALLAS, TEXAS.                                    ECS049
00012                                                                   ECS049
00013 *DATE-COMPILED.                                                   ECS049
00014                                                                   ECS049
00015 *SECURITY.   *****************************************************ECS049
00016 *            *                                                   *ECS049
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ECS049
00018 *            *                                                   *ECS049
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS049
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ECS049
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *ECS049
00022 *            *                                                   *ECS049
00023 *            *****************************************************ECS049
00024                                                                   ECS049
00025 *REMARKS.                                                         ECS049
00026 *            THIS PROGRAM WILL PRINT THE REINSURANCE TABLES.      ECS049
00027                                                                   ECS049
121003******************************************************************
121003*                   C H A N G E   L O G
121003*
121003* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
121003*-----------------------------------------------------------------
121003*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
121003* EFFECTIVE    NUMBER
121003*-----------------------------------------------------------------
121003* 121003                   PEMA  ADD OPTION 'F' TO REPORT 
121003******************************************************************
00028  EJECT                                                            ECS049
00029  ENVIRONMENT DIVISION.                                            ECS049
00030  INPUT-OUTPUT SECTION.                                            ECS049
00031  FILE-CONTROL.                                                    ECS049
00032                                                                   ECS049
00033      SELECT PRNTR          ASSIGN TO SYS008-UR-1403-S-SYS008.     ECS049
00034      SELECT DISK-DATE      ASSIGN TO SYS019-UT-FBA1-S-SYS019.     ECS049
00035      SELECT FICH           ASSIGN TO SYS020-UT-2400-S-SYS020.     ECS049
00036                                                                   ECS049
00037      SELECT ERREIN         ASSIGN       TO ERRTBLT                ECS049
00038                            ORGANIZATION IS INDEXED                ECS049
00039                            ACCESS       IS SEQUENTIAL             ECS049
00040                            RECORD KEY   IS RE-CONTROL-PRIMARY     ECS049
00041                            FILE STATUS  IS REIN-FILE-STATUS.      ECS049
00042                                                                   ECS049
00043  DATA DIVISION.                                                   ECS049
00044  FILE SECTION.                                                    ECS049
00045                                                                   ECS049
00046  EJECT                                                            ECS049
00047  FD  PRNTR                                                        ECS049
00048                              COPY ELCPRTFD.                       ECS049
00049                                                                   ECS049
00050  FD  ERREIN.                                                      ECS049
00051                                                                   ECS049
00052                              COPY ERCREIN.                        ECS049
00053                                                                   ECS049
00054  EJECT                                                            ECS049
00055  FD  DISK-DATE                                                    ECS049
00056                              COPY ELCDTEFD.                       ECS049
00057                                                                   ECS049
00058  FD  FICH                                                         ECS049
00059                              COPY ELCFCHFD.                       ECS049
00060  EJECT                                                            ECS049
00061  WORKING-STORAGE SECTION.                                         ECS049
00062  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      ECS049
00063  77  FILLER  PIC X(32) VALUE '********************************'.  ECS049
00064  77  FILLER  PIC X(32) VALUE '     ECS049                     '.  ECS049
00065  77  FILLER  PIC X(32) VALUE '**** V/M=2.004 *****************'.  ECS049
00066                                                                   ECS049
00067 *                                                                 ECS049
00068 *                                                                 ECS049
00069 * LEAVE THE ABOVE 2 LINES IN TO PREVENT LOSING THE LINE ABOVE     ECS049
00081  EJECT                                                            ECS049
00082                                                                   ECS049
00083  01  WS-TABLE-WORK-AREA.                                          ECS049
00084      05  WS-REI-COMP-NO.                                          ECS049
00085          07  WS-REI-COMP     PIC  XXX               VALUE SPACES. ECS049
00086          07  WS-REI-COMP-SUB PIC  XXX               VALUE SPACES. ECS049
00087      05  WS-LF-QC            PIC  X                 VALUE SPACES. ECS049
00088      05  WS-AH-QC            PIC  X                 VALUE SPACES. ECS049
00089      05  WS-LO-DATE          PIC  9(11)     COMP-3  VALUE 0.         CL**3
00090      05  WS-HI-DATE          PIC  9(11)     COMP-3  VALUE 0.         CL**3
00091      05  WS-LFAGE-LO         PIC  99                VALUE ZEROS.  ECS049
00092      05  WS-LFAGE-HI         PIC  99                VALUE ZEROS.  ECS049
00093      05  WS-AHAGE-LO         PIC  99                VALUE ZEROS.  ECS049
00094      05  WS-AHAGE-HI         PIC  99                VALUE ZEROS.  ECS049
00095      05  WS-LFTRM-LO         PIC S999       COMP-3  VALUE ZEROS.  ECS049
00096      05  WS-LFTRM-HI         PIC S999       COMP-3  VALUE ZEROS.  ECS049
00097      05  WS-AHTRM-LO         PIC S999       COMP-3  VALUE ZEROS.  ECS049
00098      05  WS-AHTRM-HI         PIC S999       COMP-3  VALUE ZEROS.  ECS049
00099      05  WS-LF-PCT           PIC S9V9(4)    COMP-3  VALUE ZEROS.  ECS049
00100      05  WS-AH-PCT           PIC S9V9(4)    COMP-3  VALUE ZEROS.  ECS049
00101      05  WS-LF-LIM-LO        PIC S9(9)V99   COMP-3  VALUE ZEROS.  ECS049
00102      05  WS-LF-LIM-HI        PIC S9(9)V99   COMP-3  VALUE ZEROS.  ECS049
00103      05  WS-LF-LO            PIC S9(9)V99   COMP-3  VALUE ZEROS.  ECS049
00104      05  WS-LF-HI            PIC S9(9)V99   COMP-3  VALUE ZEROS.  ECS049
00105      05  WS-AHBEN-LIM-LO     PIC S9(7)V99   COMP-3  VALUE ZEROS.  ECS049
00106      05  WS-AHBEN-LIM-HI     PIC S9(7)V99   COMP-3  VALUE ZEROS.  ECS049
00107      05  WS-AHBEN-LO         PIC S9(7)V99   COMP-3  VALUE ZEROS.  ECS049
00108      05  WS-AHBEN-HI         PIC S9(7)V99   COMP-3  VALUE ZEROS.  ECS049
00109      05  WS-AHMOA-LIM-LO     PIC S9(7)V99   COMP-3  VALUE ZEROS.  ECS049
00110      05  WS-AHMOA-LIM-HI     PIC S9(7)V99   COMP-3  VALUE ZEROS.  ECS049
00111      05  WS-AHMOA-LO         PIC S9(7)V99   COMP-3  VALUE ZEROS.  ECS049
00112      05  WS-AHMOA-HI         PIC S9(7)V99   COMP-3  VALUE ZEROS.  ECS049
00113      05  WS-LF-BEN-CODE      PIC  X                 VALUE SPACES. ECS049
00114      05  WS-AH-BEN-CODE      PIC  X                 VALUE SPACES. ECS049
00115      05  WS-INTERACTIVE      PIC  X                 VALUE SPACES. ECS049
00116      05  WS-REMAINING        PIC  X                 VALUE SPACES. ECS049
00117      05  WS-CEDING-TYPE-FLAG PIC  X                 VALUE SPACES. ECS049
00118      05  WS-FILLER-20        PIC  X(20)             VALUE SPACES. ECS049
00119  EJECT                                                            ECS049
00120  01  MISC.                                                        ECS049
00121      12  PGM-SUB             PIC S999    COMP-3  VALUE +049.      ECS049
00122      12  LINE-CNT            PIC S999    COMP-3  VALUE +99.       ECS049
00123      12  PAGE-CNT            PIC S9(5)   COMP-3  VALUE ZERO.      ECS049
00124      12  X                   PIC X               VALUE SPACE.     ECS049
00125      12  WS-DATE             PIC 9(11).                              CL**5
00126      12  WS-DATE-R REDEFINES WS-DATE.                                CL**5
00127          16 FILLER           PIC XXX.                             ECS049
00128          16  WS-CC           PIC XX.                              ECS049
00129          16  WS-YR           PIC XX.                              ECS049
00130          16  WS-MO           PIC XX.                              ECS049
00131          16  WS-DA           PIC XX.                              ECS049
00132      12  OPEN-SW             PIC X               VALUE SPACE.     ECS049
00133      12  MISS-B-SW           PIC X               VALUE SPACE.     ECS049
00134          88  NO-MISS-B               VALUE SPACE.                 ECS049
00135      12  TABLE-DATA-SUB      PIC 9(5)  COMP-3    VALUE ZEROS.     ECS049
00136      12  REIN-FILE-STATUS    PIC XX              VALUE ZERO.      ECS049
00137                                                                   ECS049
00138  01  ABEND-DATA.                                                  ECS049
00139      05  WS-RETURN-CODE       PIC S9(4) COMP     VALUE ZEROS.     ECS049
00140      05  WS-ABEND-MESSAGE     PIC  X(80)         VALUE SPACES.    ECS049
00141      05  WS-ABEND-FILE-STATUS PIC  XX            VALUE '00'.      ECS049
00142      05  WS-ZERO              PIC S9     COMP-3  VALUE ZEROS.     ECS049
00143                                                                   ECS049
00144  01  PROGRAM-FLAGS.                                               ECS049
00145      05  INPUT-FILE-FLAG      PIC X              VALUE 'N'.       ECS049
00146          88 REINSURANCE-FILE-EOF                 VALUE 'Y'.       ECS049
00147      05  FIRST-COMPANY-FLAG   PIC 9              VALUE 0.         ECS049
00148          88 FIRST-COMPANY-RECORD                 VALUE 0.         ECS049
00149                                                                   ECS049
00150  01  MESSAGES.                                                    ECS049
00151      12  NO-MISS-B-MSG.                                           ECS049
00152          16  FILLER          PIC X(33)           VALUE            ECS049
00153                  ' ****  THERE ARE NO MISSING B REC'.             ECS049
00154          16  FILLER          PIC X(39)           VALUE            ECS049
00155                  'ORDS FOR ANY REINSURANCE COMPANIES ****'.       ECS049
00156      12  ERR-68.                                                  ECS049
00157          16  FILLER          PIC X(22)           VALUE            ECS049
00158                  ' REINSURANCE COMPANY ('.                        ECS049
00159          16  E68-FLD         PIC X(6).                            ECS049
00160          16  FILLER          PIC X(23)           VALUE            ECS049
00161                  ') NEEDS A TYPE B RECORD'.                       ECS049
00162                                                                   ECS049
00163  01  FILLER.                                                      ECS049
00164      05  TOTAL-DESC.                                              ECS049
00165          10  AH-CONSTANT1        PIC XX          VALUE SPACES.    ECS049
00166          10  FILLER              PIC X(09)       VALUE            ECS049
00167              ' TOT BENE'.                                         ECS049
00168      05  MONTHLY-DESC.                                            ECS049
00169          10  AH-CONSTANT2        PIC XX          VALUE SPACES.    ECS049
00170          10  FILLER              PIC X(09)  VALUE                 ECS049
00171              ' MO. BENE'.                                         ECS049
00172      EJECT                                                        ECS049
00173  01  PRT-LINES.                                                   ECS049
00174      12  HDR-1.                                                   ECS049
00175          16  FILLER          PIC X(46)   VALUE SPACES.            ECS049
00176          16  FILLER          PIC X(33)   VALUE                    ECS049
00177                  'REINSURANCE TABLES - FILE LISTING'.             ECS049
00178          16  FILLER          PIC X(41)   VALUE SPACES.            ECS049
00179          16  FILLER          PIC X(8)    VALUE 'ECS049'.          ECS049
00180                                                                   ECS049
00181      12  HDR-2.                                                   ECS049
00182          16  FILLER          PIC X(47)   VALUE SPACES.            ECS049
00183          16  H2-COMP         PIC X(30).                           ECS049
00184          16  FILLER          PIC X(43)   VALUE SPACES.            ECS049
00185          16  H2-DATE         PIC X(8).                            ECS049
00186                                                                   ECS049
00187      12  HDR-3.                                                   ECS049
00188          16  FILLER          PIC X(53)   VALUE SPACES.            ECS049
00189          16  H3-DATE         PIC X(18).                           ECS049
00190          16  FILLER          PIC X(49)   VALUE SPACES.            ECS049
00191          16  FILLER          PIC X(5)    VALUE 'PAGE'.            ECS049
00192          16  H3-PAGE         PIC ZZ,ZZZ-.                         ECS049
00193                                                                   ECS049
00194      12  HDR-4.                                                   ECS049
00195          16  FILLER          PIC X(41)   VALUE  SPACES.           ECS049
00196          16  FILLER          PIC X(47)   VALUE                    ECS049
00197              '**----------- REINSURANCE  LIMITS -----------**'.   ECS049
00198          16  FILLER          PIC X(45)   VALUE                    ECS049
00199                  '  **------ REINSURANCE AMOUNTS ------**'.       ECS049
00200                                                                   ECS049
00201      12  HDR-5.                                                   ECS049
00202          16  FILLER          PIC X(45)   VALUE                    ECS049
00203                  ' LV  REIN    BENEFIT    EFFECT   EXPIRE  LOW '. ECS049
00204          16  FILLER          PIC X(44)   VALUE                    ECS049
00205                  'HIGH LOW HIGH         LOW            HIGH   '.  ECS049
00206          16  FILLER          PIC X(44)   VALUE                    ECS049
00207                  '                 LOW            HIGH    QBIR'.  ECS049
00208                                                                   ECS049
00209      12  HDR-6.                                                   ECS049
00210          16  FILLER          PIC X(45)   VALUE                    ECS049
00211                  ' NO  COMP  DESCRIPTION   DATE     DATE   AGE '. ECS049
00212          16  FILLER          PIC X(44)   VALUE                    ECS049
00213                  'AGE TERM TERM       BENEFIT        BENEFIT  '.  ECS049
00214          16  FILLER          PIC X(44)   VALUE                    ECS049
00215                  ' PERCENT       BENEFIT        BENEFIT   CCNM'.  ECS049
00216                                                                   ECS049
00217      12  DTL-HDR.                                                 ECS049
00218          16  FILLER          PIC X(16)   VALUE '   TABLE CODE = '.ECS049
00219          16  DH-TABLE        PIC XXX.                             ECS049
00220          16  FILLER          PIC X(17)   VALUE '   STATE CODES -'.ECS049
00221          16  DH-LF-OVRD      PIC XX.                              ECS049
00222          16  FILLER          PIC X(03)   VALUE ' = '.             ECS049
00223          16  DH-LF-NSP-ST    PIC XX.                              ECS049
00224          16  FILLER          PIC XX      VALUE SPACES.            ECS049
00225          16  DH-AH-OVRD      PIC XX.                              ECS049
00226          16  FILLER          PIC X(03)   VALUE ' = '.             ECS049
00227          16  DH-AH-NSP-ST    PIC XX.                              ECS049
00228          16  FILLER          PIC X(81)   VALUE SPACES.            ECS049
00229                                                                   ECS049
00230      12  DTL-1.                                                   ECS049
00231          16  FILLER          PIC X.                               ECS049
00232          16  D1-LEVEL        PIC 99.                              ECS049
00233          16  FILLER          PIC X.                               ECS049
00234          16  D1-COMP         PIC X(6).                            ECS049
00235          16  FILLER          PIC X.                               ECS049
00236          16  D1-BEN-DESC     PIC X(11).                           ECS049
00237          16  FILLER          PIC X.                               ECS049
00238          16  D1-EFF-MO       PIC XX.                              ECS049
00239          16  D1-SLSH-1       PIC X.                               ECS049
00240          16  D1-EFF-DA       PIC XX.                              ECS049
00241          16  D1-SLSH-2       PIC X.                               ECS049
00242          16  D1-EFF-YR       PIC XX.                              ECS049
00243          16  FILLER          PIC X.                               ECS049
00244          16  D1-EXP-MO       PIC XX.                              ECS049
00245          16  D1-SLSH-3       PIC X.                               ECS049
00246          16  D1-EXP-DA       PIC XX.                              ECS049
00247          16  D1-SLSH-4       PIC X.                               ECS049
00248          16  D1-EXP-YR       PIC XX.                              ECS049
00249          16  FILLER          PIC XX.                              ECS049
00250          16  D1-LO-AGE       PIC 99.                              ECS049
00251          16  FILLER          PIC XX.                              ECS049
00252          16  D1-HI-AGE       PIC 99.                              ECS049
00253          16  FILLER          PIC XX.                              ECS049
00254          16  D1-LO-TRM       PIC 999.                             ECS049
00255          16  FILLER          PIC XX.                              ECS049
00256          16  D1-HI-TRM       PIC 999.                             ECS049
00257          16  FILLER          PIC X.                               ECS049
00258          16  D1-LO-LIM       PIC ZZZ,ZZZ,ZZ9.99-.                 ECS049
00259          16  D1-HI-LIM       PIC ZZZ,ZZZ,ZZ9.99-.                 ECS049
00260          16  FILLER          PIC XX.                              ECS049
00261          16  D1-PCT          PIC ZZ9.99-.                         ECS049
00262          16  D1-LO           PIC ZZZ,ZZZ,ZZ9.99-.                 ECS049
00263          16  D1-HI           PIC ZZZ,ZZZ,ZZ9.99-.                 ECS049
00264          16  FILLER          PIC X.                               ECS049
00265          16  D1-QC           PIC X.                               ECS049
00266          16  D1-BEN-CODE     PIC X.                               ECS049
00267          16  D1-IN           PIC X.                               ECS049
00268          16  D1-RM           PIC X.                               ECS049
00269                                                                   ECS049
00270      12  CO-DTL-1.                                                ECS049
00271          16  FILLER          PIC X(53)   VALUE                    ECS049
00272          ' *------- REINSURANCE COMPANY -------*   RPT OPTIONS:'. ECS049
00273          16  FILLER          PIC X(03)   VALUE ' A-'.             ECS049
00274          16  CD1-RPT-OPT-A   PIC X.                               ECS049
00275          16  FILLER          PIC X(03)   VALUE ' B-'.             ECS049
00276          16  CD1-RPT-OPT-B   PIC X.                               ECS049
00277          16  FILLER          PIC X(03)   VALUE ' C-'.             ECS049
00278          16  CD1-RPT-OPT-C   PIC X.                               ECS049
00279          16  FILLER          PIC X(03)   VALUE ' D-'.             ECS049
00280          16  CD1-RPT-OPT-D   PIC X.                               ECS049
00281          16  FILLER          PIC X(03)   VALUE ' E-'.             ECS049
00282          16  CD1-RPT-OPT-E   PIC X.                               ECS049
121003         16  FILLER          PIC X(03)   VALUE ' F-'.
121003         16  CD1-RPT-OPT-F   PIC X.
00283          16  FILLER          PIC X(30)   VALUE                    ECS049
00284          '   CEDING COMPANY NAME (FROM) '.                        ECS049
00285          16  CD1-CEDE-NAME   PIC X(30).                           ECS049
00286                                                                   ECS049
00287      12  CO-DTL-2.                                                ECS049
00288          16  FILLER          PIC X(133)  VALUE                    ECS049
00289          '  CODE           NAME'.                                 ECS049
00290                                                                   ECS049
00291      12  CO-DTL-3.                                                ECS049
00292          16  FILLER          PIC X       VALUE SPACES.            ECS049
00293          16  CD3-COMP        PIC X(6).                            ECS049
00294          16  FILLER          PIC X       VALUE SPACES.            ECS049
00295          16  CD3-NAME        PIC X(30).                           ECS049
00296          16  FILLER          PIC X(40)  VALUE                     ECS049
00297          '   PRINT   MORTALITY  CLAIM  CLAIM      '.              ECS049
00298          16  FILLER          PIC X(55)  VALUE                     ECS049
00299          '*-------------- CEDING FEE BRACKETS -----------------*'.ECS049
00300                                                                   ECS049
00301      12  CO-DTL-4.                                                ECS049
00302          16  FILLER          PIC X(38)   VALUE SPACES.            ECS049
00303          16  FILLER          PIC X(40)  VALUE                     ECS049
00304          '  TAX O/W   CODE SW    P/I   STARTS     '.              ECS049
00305          16  FILLER          PIC X(55)  VALUE                     ECS049
00306          '       PERCENT     THRU            PERCENT     THRU   '.ECS049
00307                                                                   ECS049
00308      12  CO-DTL-5.                                                ECS049
00309          16  FILLER          PIC X(18)  VALUE '   CESSION TYPE -'.ECS049
00310          16  CD5-CEDE-TYPE   PIC X.                               ECS049
00311          16  FILLER          PIC X(10)  VALUE '   GROUP '.        ECS049
00312          16  CD5-REIN-GROUP  PIC X(6).                            ECS049
00313          16  FILLER          PIC X(06)   VALUE SPACES.            ECS049
00314          16  CD5-ST          PIC X.                               ECS049
00315          16  FILLER          PIC XXX     VALUE SPACES.            ECS049
00316          16  CD5-OW          PIC X.                               ECS049
00317          16  FILLER          PIC XXXX    VALUE SPACES.            ECS049
00318          16  CD5-MORT        PIC X(4).                            ECS049
00319          16  FILLER          PIC XX      VALUE SPACES.            ECS049
00320          16  CD5-MORT-SW     PIC X.                               ECS049
00321          16  FILLER          PIC X(5)    VALUE SPACES.            ECS049
00322          16  CD5-CLM-CD      PIC X.                               ECS049
00323          16  FILLER          PIC X(3)    VALUE SPACES.            ECS049
00324          16  CD5-CLM-MO      PIC XX.                              ECS049
00325          16  CD5-SLSH-1      PIC X.                               ECS049
00326          16  CD5-CLM-DA      PIC XX.                              ECS049
00327          16  CD5-SLSH-2      PIC X.                               ECS049
00328          16  CD5-CLM-YR      PIC XX.                              ECS049
00329          16  FILLER          PIC X(4)    VALUE SPACES.            ECS049
00330          16  CD5-LF-OVRD     PIC X(7).                            ECS049
00331          16  CD5-LF-BR-FEE1  PIC ZZ9.99-.                         ECS049
00332          16  CD5-LF-BR-AMT1  PIC Z,ZZZ,ZZ9.99-.                   ECS049
00333          16  FILLER          PIC X       VALUE SPACES.            ECS049
00334          16  CD5-AH-OVRD     PIC X(7).                            ECS049
00335          16  CD5-AH-BR-FEE1  PIC ZZ9.99-.                         ECS049
00336          16  CD5-AH-BR-AMT1  PIC Z,ZZZ,ZZ9.99-.                   ECS049
00337                                                                   ECS049
00338      12  CO-DTL-6.                                                ECS049
00339          16  FILLER          PIC X(85)  VALUE SPACES.             ECS049
00340          16  CD6-LF-BR-FEE2  PIC ZZ9.99-.                         ECS049
00341          16  CD6-LF-BR-AMT2  PIC Z,ZZZ,ZZ9.99-.                   ECS049
00342          16  FILLER          PIC X(8)    VALUE SPACES.            ECS049
00343          16  CD6-AH-BR-FEE2  PIC ZZ9.99-.                         ECS049
00344          16  CD6-AH-BR-AMT2  PIC Z,ZZZ,ZZ9.99-.                   ECS049
00345                                                                   ECS049
00346      12  CO-DTL-7.                                                ECS049
00347          16  FILLER          PIC X(52)  VALUE                     ECS049
00348          '         METHOD FEES   PRORATA RULE-78 IBNR FEE COM '.  ECS049
00349          16  FILLER          PIC X(33)  VALUE                     ECS049
00350          'TAX  CLM-%  CLM-MAX-AMT    MTHD  '.                     ECS049
00351          16  CD7-LF-BR-FEE3  PIC ZZ9.99-.                         ECS049
00352          16  CD7-LF-BR-AMT3  PIC Z,ZZZ,ZZ9.99-.                   ECS049
00353          16  FILLER          PIC X(8)   VALUE '  MTHD  '.         ECS049
00354          16  CD7-AH-BR-FEE3  PIC ZZ9.99-.                         ECS049
00355          16  CD7-AH-BR-AMT3  PIC Z,ZZZ,ZZ9.99-.                   ECS049
00356                                                                   ECS049
00357      12  CO-DTL-8.                                                ECS049
00358          16  FILLER          PIC XXX    VALUE SPACES.             ECS049
00359          16  CD8-LF-OVRD     PIC X(6).                            ECS049
00360          16  FILLER          PIC XX     VALUE SPACES.             ECS049
00361          16  CD8-LF-PE       PIC X.                               ECS049
00362          16  FILLER          PIC XXX    VALUE SPACES.             ECS049
00363          16  CD8-LF-FEE      PIC ZZ9.99-.                         ECS049
00364          16  CD8-LF-MSG REDEFINES CD8-LF-FEE                      ECS049
00365                              PIC X(7).                            ECS049
00366          16  FILLER          PIC X       VALUE SPACES.            ECS049
00367          16  CD8-LF-PR-PCT   PIC ZZ9.99-.                         ECS049
00368          16  FILLER          PIC X       VALUE SPACES.            ECS049
00369          16  CD8-LF-78-PCT   PIC ZZ9.99-.                         ECS049
00370          16  FILLER          PIC X       VALUE SPACES.            ECS049
00371          16  CD8-LF-IBNR-PCT PIC Z9.9-.                           ECS049
00372          16  FILLER          PIC X       VALUE SPACES.            ECS049
00373          16  CD8-Z-L-FE      PIC X.                               ECS049
00374          16  FILLER          PIC XXX     VALUE SPACES.            ECS049
00375          16  CD8-L-COMM      PIC X.                               ECS049
00376          16  FILLER          PIC XXX     VALUE SPACES.            ECS049
00377          16  CD8-L-TAX       PIC X.                               ECS049
00378          16  FILLER          PIC XX      VALUE SPACES.            ECS049
00379          16  CD8-LF-CLM-PCT  PIC ZZ9.99-.                         ECS049
00380          16  CD8-LF-CLM-MAX  PIC Z,ZZZ,ZZ9.99-.                   ECS049
00381          16  FILLER          PIC X(5)    VALUE SPACES.            ECS049
00382          16  CD8-LF-FEE-MTHD PIC X.                               ECS049
00383          16  FILLER          PIC X(3)    VALUE SPACES.            ECS049
00384          16  CD8-LF-BR-FEE4  PIC ZZ9.99-.                         ECS049
00385          16  CD8-LF-BR-AMT4  PIC Z,ZZZ,ZZ9.99-.                   ECS049
00386          16  FILLER          PIC X(4)    VALUE SPACES.            ECS049
00387          16  CD8-AH-FEE-MTHD PIC X.                               ECS049
00388          16  FILLER          PIC X(3)    VALUE SPACES.            ECS049
00389          16  CD8-AH-BR-FEE4  PIC ZZ9.99-.                         ECS049
00390          16  CD8-AH-BR-AMT4  PIC Z,ZZZ,ZZ9.99-.                   ECS049
00391                                                                   ECS049
00392      12  CO-DTL-9.                                                ECS049
00393          16  FILLER          PIC XXX    VALUE SPACES.             ECS049
00394          16  CD9-AH-OVRD     PIC X(6).                            ECS049
00395          16  FILLER          PIC XX     VALUE SPACES.             ECS049
00396          16  CD9-AH-PE       PIC X.                               ECS049
00397          16  FILLER          PIC XXX    VALUE SPACES.             ECS049
00398          16  CD9-AH-FEE      PIC ZZ9.99-.                         ECS049
00399          16  CD9-AH-MSG REDEFINES CD9-AH-FEE                      ECS049
00400                              PIC X(7).                            ECS049
00401          16  FILLER          PIC X       VALUE SPACES.            ECS049
00402          16  CD9-AH-PR-PCT   PIC ZZ9.99-.                         ECS049
00403          16  FILLER          PIC X       VALUE SPACES.            ECS049
00404          16  CD9-AH-78-PCT   PIC ZZ9.99-.                         ECS049
00405          16  FILLER          PIC X       VALUE SPACES.            ECS049
00406          16  CD9-AH-IBNR-PCT PIC Z9.9-.                           ECS049
00407          16  FILLER          PIC X       VALUE SPACES.            ECS049
00408          16  CD9-Z-A-FE      PIC X.                               ECS049
00409          16  FILLER          PIC XXX     VALUE SPACES.            ECS049
00410          16  CD9-A-COMM      PIC X.                               ECS049
00411          16  FILLER          PIC XXX     VALUE SPACES.            ECS049
00412          16  CD9-A-TAX       PIC X.                               ECS049
00413          16  FILLER          PIC XX     VALUE SPACES.             ECS049
00414          16  CD9-AH-CLM-PCT  PIC ZZ9.99-.                         ECS049
00415          16  CD9-AH-CLM-MAX  PIC Z,ZZZ,ZZ9.99-.                   ECS049
00416          16  FILLER          PIC X(9)   VALUE '   BASIS '.        ECS049
00417          16  CD9-LF-BR-FEE5  PIC ZZ9.99-.                         ECS049
00418          16  CD9-LF-BR-AMT5  PIC Z,ZZZ,ZZ9.99-.                   ECS049
00419          16  FILLER          PIC X(8)   VALUE '  BASIS '.         ECS049
00420          16  CD9-AH-BR-FEE5  PIC ZZ9.99-.                         ECS049
00421          16  CD9-AH-BR-AMT5  PIC Z,ZZZ,ZZ9.99-.                   ECS049
00422                                                                   ECS049
00423      12  CO-DTL-10.                                               ECS049
00424          16  FILLER          PIC X(81)  VALUE SPACES.             ECS049
00425          16  CD10-LF-FEE-BAS PIC X.                               ECS049
00426          16  FILLER          PIC X(3)   VALUE SPACES.             ECS049
00427          16  CD10-LF-BR-FEE6 PIC ZZ9.99-.                         ECS049
00428          16  CD10-LF-BR-AMT6 PIC Z,ZZZ,ZZ9.99-.                   ECS049
00429          16  FILLER          PIC X(4)   VALUE SPACES.             ECS049
00430          16  CD10-AH-FEE-BAS PIC X.                               ECS049
00431          16  FILLER          PIC X(3)   VALUE SPACES.             ECS049
00432          16  CD10-AH-BR-FEE6 PIC ZZ9.99-.                         ECS049
00433          16  CD10-AH-BR-AMT6 PIC Z,ZZZ,ZZ9.99-.                   ECS049
00434                                                                   ECS049
00435      EJECT                                                        ECS049
00436  01  A-TYPE-TABLE.                                                ECS049
00437      12  A-SUB               PIC 9(5) COMP-3   VALUE ZEROS.       ECS049
00438      12  A-TABLE-MAX         PIC 9(5) COMP-3   VALUE 2501.        ECS049
00439      12  A-TYPE-CODES        OCCURS 2500 TIMES.                   ECS049
00440          16  A-TYPE-COMPANY  PIC X(6).                            ECS049
00441                                                                   ECS049
00442  01  B-TYPE-TABLE.                                                ECS049
00443      12  B-SUB               PIC 9(5)  COMP-3  VALUE ZEROS.       ECS049
00444      12  B-TABLE-MAX         PIC 9(5)  COMP-3  VALUE 2501.        ECS049
00445      12  B-TYPE-CODES        OCCURS 2500 TIMES.                   ECS049
00446          16  B-TYPE-COMPANY  PIC X(6).                            ECS049
00447                                                                   ECS049
00448                                                                   ECS049
00449                              COPY ELCREINV.                       ECS049
00450                                                                   ECS049
00451                              COPY ELCDTECX.                       ECS049
00452                                                                   ECS049
00453                              COPY ELCDTEVR.                       ECS049
00454  EJECT                                                            ECS049
00455  PROCEDURE DIVISION.                                              ECS049
00456                                                                   ECS049
00457  0000-READ-DATE-CARD.                                             ECS049
00458                              COPY ELCDTERX.                       ECS049
00459                                                                   ECS049
00460  0000-MAINLINE.                                                   ECS049
00461      PERFORM 010-INITILIZATION THRU 010-EXIT.                     ECS049
00462                                                                   ECS049
00463      PERFORM 020-OPEN-FILES    THRU 020-EXIT.                     ECS049
00464                                                                   ECS049
00465      PERFORM 100-PROCESS-DATA  THRU 100-EXIT                      ECS049
00466              UNTIL REINSURANCE-FILE-EOF.                          ECS049
00467                                                                   ECS049
00468      PERFORM 500-EOJ           THRU 500-EXIT.                     ECS049
00469                                                                   ECS049
00470      PERFORM 900-ACCOUNTING    THRU 900-EXIT.                     ECS049
00471                                                                   ECS049
00472      PERFORM 030-CLOSE-FILES   THRU 030-EXIT.                     ECS049
00473                                                                   ECS049
00474      PERFORM 035-CLOSE-FICH    THRU 035-EXIT.                     ECS049
00475                                                                   ECS049
00476      GOBACK.                                                      ECS049
00477                                                                   ECS049
00478      EJECT                                                        ECS049
00479  010-INITILIZATION.                                               ECS049
00480      MOVE WS-CURRENT-DATE        TO H2-DATE.                      ECS049
00481      MOVE ALPH-DATE              TO H3-DATE.                      ECS049
00482      MOVE COMPANY-NAME           TO H2-COMP.                      ECS049
00483                                                                   ECS049
00484      MOVE LIFE-OVERRIDE-L2       TO DH-LF-OVRD.                   ECS049
00485                                                                   ECS049
00486      MOVE LIFE-OVERRIDE-L6       TO CD5-LF-OVRD                   ECS049
00487                                     CD8-LF-OVRD.                  ECS049
00488                                                                   ECS049
00489      MOVE AH-OVERRIDE-L2         TO DH-AH-OVRD.                   ECS049
00490                                                                   ECS049
00491      MOVE AH-OVERRIDE-L6         TO CD5-AH-OVRD                   ECS049
00492                                     CD9-AH-OVRD.                  ECS049
00493                                                                   ECS049
00494      MOVE AH-OVERRIDE-L2         TO AH-CONSTANT1                  ECS049
00495                                     AH-CONSTANT2.                 ECS049
00496                                                                   ECS049
00497      MOVE +0                     TO PAGE-CNT.                     ECS049
00498                                                                   ECS049
00499      PERFORM 011-CLEAR-A-B-TABLES THRU 011-EXIT                   ECS049
00500              VARYING A-SUB FROM 1 BY 1                            ECS049
00501              UNTIL   A-SUB = A-TABLE-MAX.                         ECS049
00502                                                                   ECS049
00503  010-EXIT.                                                        ECS049
00504      EXIT.                                                        ECS049
00505                                                                   ECS049
00506  011-CLEAR-A-B-TABLES.                                            ECS049
00507      MOVE HIGH-VALUES            TO A-TYPE-CODES (A-SUB)          ECS049
00508                                     B-TYPE-CODES (A-SUB).         ECS049
00509                                                                   ECS049
00510  011-EXIT.                                                        ECS049
00511      EXIT.                                                        ECS049
00512      EJECT                                                        ECS049
00513  020-OPEN-FILES.                                                  ECS049
00514      OPEN INPUT  ERREIN.                                          ECS049
00515                                                                   ECS049
00516      IF REIN-FILE-STATUS  = '00' OR '97'                          ECS049
00517          NEXT SENTENCE                                            ECS049
00518        ELSE                                                       ECS049
00519          MOVE    'ERROR OCCURED DURING OPEN - ERREIN'             ECS049
00520                  TO WS-ABEND-MESSAGE                              ECS049
00521          MOVE    REIN-FILE-STATUS TO WS-ABEND-FILE-STATUS         ECS049
00522          PERFORM ABEND-PGM.                                       ECS049
00523                                                                   ECS049
00524                                                                   ECS049
00525      OPEN OUTPUT PRNTR.                                           ECS049
00526                                                                   ECS049
00527  020-EXIT.                                                        ECS049
00528      EXIT.                                                        ECS049
00529                                                                   ECS049
00530  030-CLOSE-FILES.                                                 ECS049
00531      CLOSE ERREIN                                                 ECS049
00532            PRNTR.                                                 ECS049
00533                                                                   ECS049
00534      IF  REIN-FILE-STATUS  NOT = '00'                             ECS049
00535          MOVE    'ERROR OCCURED DURING CLOSE - ERREIN'            ECS049
00536                  TO WS-ABEND-MESSAGE                              ECS049
00537          MOVE    REIN-FILE-STATUS TO WS-ABEND-FILE-STATUS         ECS049
00538          PERFORM ABEND-PGM.                                       ECS049
00539                                                                   ECS049
00540  030-EXIT.                                                        ECS049
00541      EXIT.                                                        ECS049
00542                                                                   ECS049
00543  035-CLOSE-FICH.                                                  ECS049
00544                              COPY ELCPRTC.                        ECS049
00545                                                                   ECS049
00546  035-EXIT.                                                        ECS049
00547      EXIT.                                                        ECS049
00548      EJECT                                                        ECS049
00549  100-PROCESS-DATA.                                                ECS049
00550      PERFORM 150-READ-REIN THRU 150-EXIT.                         ECS049
00551                                                                   ECS049
00552      IF  REINSURANCE-FILE-EOF                                     ECS049
00553          GO TO 100-EXIT.                                          ECS049
00554                                                                   ECS049
00555      IF  RE-TABLE-RECORD                                          ECS049
00556          PERFORM 200-FORMAT-TABLE-RECORD THRU 200-EXIT            ECS049
00557      ELSE                                                         ECS049
00558          IF  RE-COMPANY-RECORD                                    ECS049
00559              PERFORM 300-FORMAT-COMPANY-RECORD THRU 300-EXIT      ECS049
00560          ELSE                                                     ECS049
00561              MOVE 'Y'            TO INPUT-FILE-FLAG.              ECS049
00562                                                                   ECS049
00563  100-EXIT.                                                        ECS049
00564      EXIT.                                                        ECS049
00565                                                                   ECS049
00566  150-READ-REIN.                                                   ECS049
00567      READ ERREIN.                                                 ECS049
00568                                                                   ECS049
00569      IF  REIN-FILE-STATUS  =  '00'                                ECS049
00570          NEXT SENTENCE                                            ECS049
00571      ELSE                                                         ECS049
00572          IF  REIN-FILE-STATUS  =  '10'                            ECS049
00573              MOVE 'Y'            TO INPUT-FILE-FLAG               ECS049
00574          ELSE                                                     ECS049
00575              MOVE    'ERROR OCCURED DURING READ - ERREIN'         ECS049
00576                                  TO WS-ABEND-MESSAGE              ECS049
00577              MOVE    REIN-FILE-STATUS TO WS-ABEND-FILE-STATUS     ECS049
00578              PERFORM ABEND-PGM.                                   ECS049
00579                                                                   ECS049
00580      IF RE-CODE NOT = 'A'                                            CL**6
00581         PERFORM REIN-DATE-LOAD.                                      CL**6
00582                                                                   ECS049
00583  150-EXIT.                                                        ECS049
00584      EXIT.                                                        ECS049
00585      EJECT                                                        ECS049
00586  200-FORMAT-TABLE-RECORD.                                         ECS049
00587      MOVE +99 TO LINE-CNT.                                        ECS049
00588                                                                   ECS049
00589      PERFORM 210-PRINT-TABLE-RECORD THRU 210-EXIT                 ECS049
00590              VARYING TABLE-DATA-SUB FROM +1 BY +1                 ECS049
00591              UNTIL   TABLE-DATA-SUB GREATER THAN +30.             ECS049
00592                                                                   ECS049
00593  200-EXIT.                                                        ECS049
00594      EXIT.                                                        ECS049
00595                                                                   ECS049
00596  210-PRINT-TABLE-RECORD.                                          ECS049
00597      IF  LINE-CNT GREATER THAN +50                                ECS049
00598          MOVE RE-TABLE           TO   DH-TABLE                    ECS049
00599          MOVE RE-NSP-ST-CD-LF    TO   DH-LF-NSP-ST                ECS049
00600          MOVE RE-NSP-ST-CD-AH    TO   DH-AH-NSP-ST                ECS049
00601          PERFORM 230-TABLE-RPT-HEADINGS THRU 230-EXIT.            ECS049
00602                                                                   ECS049
00603      MOVE SPACES                 TO X PRT.                        ECS049
00604                                                                   ECS049
00605      MOVE RE-COMP-INFO (TABLE-DATA-SUB) TO WS-TABLE-WORK-AREA.    ECS049
00606                                                                   ECS049
00607      IF  WS-REI-COMP-NO = SPACES                                  ECS049
00608          MOVE +99                TO TABLE-DATA-SUB                ECS049
00609          GO TO 210-EXIT.                                          ECS049
00610                                                                   ECS049
00611      PERFORM  220-BUILD-A-TABLE   THRU  220-EXIT                  ECS049
00612               VARYING A-SUB FROM 1 BY 1                           ECS049
00613               UNTIL   A-SUB GREATER THAN A-TABLE-MAX.             ECS049
00614                                                                   ECS049
00615      PERFORM  800-FICH-PRNT       THRU  800-EXIT.                 ECS049
00616                                                                   ECS049
00617      MOVE     SPACES             TO   DTL-1.                      ECS049
00618      MOVE     TABLE-DATA-SUB     TO   D1-LEVEL.                   ECS049
00619      MOVE     WS-REI-COMP-NO     TO   D1-COMP.                    ECS049
00620      MOVE     LIFE-OVERRIDE-L6   TO   D1-BEN-DESC.                ECS049
00621      MOVE     WS-LF-BEN-CODE     TO   D1-BEN-CODE.                ECS049
00622      MOVE     WS-LF-QC           TO   D1-QC.                      ECS049
00623      MOVE     WS-INTERACTIVE     TO   D1-IN.                      ECS049
00624      MOVE     WS-REMAINING       TO   D1-RM.                      ECS049
00625      MOVE     WS-LO-DATE         TO   WS-DATE.                    ECS049
00626      MOVE     WS-MO              TO   D1-EFF-MO.                  ECS049
00627      MOVE     WS-DA              TO   D1-EFF-DA.                  ECS049
00628      MOVE     WS-YR              TO   D1-EFF-YR.                  ECS049
00629      MOVE     WS-HI-DATE         TO   WS-DATE.                    ECS049
00630      MOVE     WS-MO              TO   D1-EXP-MO.                  ECS049
00631      MOVE     WS-DA              TO   D1-EXP-DA.                  ECS049
00632      MOVE     WS-YR              TO   D1-EXP-YR.                  ECS049
00633      MOVE     '/'                TO   D1-SLSH-1                   ECS049
00634                                       D1-SLSH-2                   ECS049
00635                                       D1-SLSH-3                   ECS049
00636                                       D1-SLSH-4.                  ECS049
00637      MOVE     WS-LFAGE-LO        TO   D1-LO-AGE.                  ECS049
00638      MOVE     WS-LFAGE-HI        TO   D1-HI-AGE.                  ECS049
00639      MOVE     WS-LFTRM-LO        TO   D1-LO-TRM.                  ECS049
00640      MOVE     WS-LFTRM-HI        TO   D1-HI-TRM.                  ECS049
00641      MOVE     WS-LF-LIM-LO       TO   D1-LO-LIM.                  ECS049
00642      MOVE     WS-LF-LIM-HI       TO   D1-HI-LIM.                  ECS049
00643      MOVE     WS-LF-LO           TO   D1-LO.                      ECS049
00644      MOVE     WS-LF-HI           TO   D1-HI.                      ECS049
00645                                                                   ECS049
00646      MULTIPLY WS-LF-PCT BY +100  GIVING D1-PCT.                   ECS049
00647                                                                   ECS049
00648      MOVE     DTL-1              TO   PRT.                        ECS049
00649                                                                   ECS049
00650      PERFORM  800-FICH-PRNT       THRU  800-EXIT.                 ECS049
00651                                                                   ECS049
00652      MOVE     SPACES             TO   DTL-1.                      ECS049
00653      MOVE     TOTAL-DESC         TO   D1-BEN-DESC.                ECS049
00654      MOVE     WS-AH-BEN-CODE     TO   D1-BEN-CODE.                ECS049
00655      MOVE     WS-AH-QC           TO   D1-QC.                      ECS049
00656      MOVE     WS-AHAGE-LO        TO   D1-LO-AGE.                  ECS049
00657      MOVE     WS-AHAGE-HI        TO   D1-HI-AGE.                  ECS049
00658      MOVE     WS-AHTRM-LO        TO   D1-LO-TRM.                  ECS049
00659      MOVE     WS-AHTRM-HI        TO   D1-HI-TRM.                  ECS049
00660      MOVE     WS-AHBEN-LIM-LO    TO   D1-LO-LIM.                  ECS049
00661      MOVE     WS-AHBEN-LIM-HI    TO   D1-HI-LIM.                  ECS049
00662      MOVE     WS-AHBEN-LO        TO   D1-LO.                      ECS049
00663      MOVE     WS-AHBEN-HI        TO   D1-HI.                      ECS049
00664                                                                   ECS049
00665      MULTIPLY WS-AH-PCT BY +100  GIVING D1-PCT.                   ECS049
00666                                                                   ECS049
00667      MOVE     DTL-1              TO   PRT.                        ECS049
00668      PERFORM  800-FICH-PRNT       THRU  800-EXIT.                 ECS049
00669                                                                   ECS049
00670      MOVE     SPACES             TO   DTL-1.                      ECS049
00671      MOVE     MONTHLY-DESC       TO   D1-BEN-DESC.                ECS049
00672      MOVE     WS-AHMOA-LIM-LO    TO   D1-LO-LIM.                  ECS049
00673      MOVE     WS-AHMOA-LIM-HI    TO   D1-HI-LIM.                  ECS049
00674      MOVE     WS-AHMOA-LO        TO   D1-LO.                      ECS049
00675      MOVE     WS-AHMOA-HI        TO   D1-HI.                      ECS049
00676      MOVE     DTL-1              TO   PRT.                        ECS049
00677      PERFORM  800-FICH-PRNT       THRU  800-EXIT.                 ECS049
00678                                                                   ECS049
00679      ADD      +4                 TO   LINE-CNT.                   ECS049
00680                                                                   ECS049
00681  210-EXIT.                                                        ECS049
00682      EXIT.                                                        ECS049
00683      EJECT                                                        ECS049
00684  220-BUILD-A-TABLE.                                               ECS049
00685 ****************************************************************  ECS049
00686 ** A-TABLE CONTAINS A LIST OF UNIQUE COMPANY NUMBERS EXTRACTED    ECS049
00687 ** FROM THE INPUT TABLE RECORDS.  A-TABLE-MAX IS THE TABLE SIZE   ECS049
00688 ** PLUS 1 ENTRY, A TABLE OVERFLOW CONDITION.                      ECS049
00689 ****************************************************************  ECS049
00690                                                                   ECS049
00691      IF  A-SUB = A-TABLE-MAX                                      ECS049
00692          MOVE +0201              TO WS-RETURN-CODE                ECS049
00693          MOVE 'A-TABLE FILLED WITH 2500 ENTRYS, CANNOT CONTINUE.' ECS049
00694                                  TO WS-ABEND-MESSAGE              ECS049
00695          GO TO ABEND-PGM.                                            CL**6
00696                                                                   ECS049
00697      IF  A-TYPE-COMPANY (A-SUB) = HIGH-VALUES                     ECS049
00698          MOVE WS-REI-COMP-NO     TO A-TYPE-COMPANY (A-SUB)        ECS049
00699          MOVE 3000 TO A-SUB                                       ECS049
00700      ELSE                                                         ECS049
00701          IF  WS-REI-COMP-NO = A-TYPE-COMPANY (A-SUB)              ECS049
00702              MOVE 3000           TO A-SUB.                        ECS049
00703                                                                   ECS049
00704  220-EXIT.                                                        ECS049
00705      EXIT.                                                        ECS049
00706                                                                   ECS049
00707  230-TABLE-RPT-HEADINGS.                                          ECS049
00708      PERFORM 700-RPT-HEADINGS THRU 700-EXIT.                         CL**6
00709                                                                   ECS049
00710      MOVE    '0'                TO  X.                               CL**6
00711      MOVE    DTL-HDR            TO  PRT.                             CL**6
00712      PERFORM 800-FICH-PRNT    THRU 800-EXIT.                         CL**6
00713                                                                   ECS049
00714      MOVE    '0'                TO  X.                               CL**6
00715      MOVE    HDR-4              TO  PRT.                             CL**6
00716      PERFORM 800-FICH-PRNT    THRU 800-EXIT.                         CL**6
00717                                                                   ECS049
00718      MOVE    SPACE              TO  X.                               CL**6
00719      MOVE    HDR-5              TO  PRT.                             CL**6
00720      PERFORM 800-FICH-PRNT     THRU 800-EXIT.                     ECS049
00721                                                                   ECS049
00722      MOVE    HDR-6              TO  PRT.                             CL**6
00723      PERFORM 800-FICH-PRNT     THRU 800-EXIT.                     ECS049
00724                                                                   ECS049
00725      MOVE    SPACES             TO  PRT.                             CL**6
00726      PERFORM 800-FICH-PRNT     THRU 800-EXIT.                     ECS049
00727                                                                   ECS049
00728      MOVE   +9                  TO  LINE-CNT.                        CL**6
00729                                                                   ECS049
00730  230-EXIT.                                                        ECS049
00731      EXIT.                                                        ECS049
00732      EJECT                                                        ECS049
00733  300-FORMAT-COMPANY-RECORD.                                       ECS049
00734      IF  FIRST-COMPANY-RECORD                                     ECS049
00735          MOVE 1                  TO FIRST-COMPANY-FLAG            ECS049
00736          MOVE +99                TO LINE-CNT.                     ECS049
00737                                                                   ECS049
00738      IF  LINE-CNT GREATER THAN +40                                ECS049
00739          PERFORM  330-COMPANY-RPT-HEADINGS THRU 330-EXIT.         ECS049
00740                                                                   ECS049
00741      PERFORM  320-BUILD-B-TABLE   THRU  320-EXIT                  ECS049
00742               VARYING B-SUB FROM 1 BY 1                           ECS049
00743               UNTIL   B-SUB GREATER THAN B-TABLE-MAX.             ECS049
00744                                                                   ECS049
00745      MOVE RE-CEDING-STMT-OPT-A   TO   CD1-RPT-OPT-A.              ECS049
00746      MOVE RE-CEDING-STMT-OPT-B   TO   CD1-RPT-OPT-B.              ECS049
00747      MOVE RE-CEDING-STMT-OPT-C   TO   CD1-RPT-OPT-C.              ECS049
00748      MOVE RE-CEDING-STMT-OPT-D   TO   CD1-RPT-OPT-D.              ECS049
00749      MOVE RE-CEDING-STMT-OPT-E   TO   CD1-RPT-OPT-E.              ECS049
121003     MOVE RE-STATE-EXHIBIT-OPT-F TO   CD1-RPT-OPT-F.
00750      MOVE    RE-CEDE-NAME        TO  CD1-CEDE-NAME.               ECS049
00751                                                                   ECS049
00752      MOVE    '-'                 TO  X.                           ECS049
00753      MOVE    CO-DTL-1            TO  PRT.                         ECS049
00754      PERFORM 800-FICH-PRNT  THRU 800-EXIT.                        ECS049
00755                                                                   ECS049
00756      ADD     +3                  TO  LINE-CNT.                    ECS049
00757                                                                   ECS049
00758      MOVE    ' '                 TO  X.                           ECS049
00759      MOVE    CO-DTL-2            TO  PRT.                         ECS049
00760      PERFORM 800-FICH-PRNT  THRU 800-EXIT.                        ECS049
00761                                                                   ECS049
00762      ADD     +1                  TO  LINE-CNT.                    ECS049
00763                                                                   ECS049
00764      MOVE     RE-COMPANY         TO   CD3-COMP.                   ECS049
00765      MOVE     RE-NAME            TO   CD3-NAME.                   ECS049
00766                                                                   ECS049
00767      MOVE    ' '                 TO  X.                           ECS049
00768      MOVE    CO-DTL-3            TO  PRT.                         ECS049
00769      PERFORM 800-FICH-PRNT  THRU 800-EXIT.                        ECS049
00770                                                                   ECS049
00771      ADD     +1                  TO  LINE-CNT.                    ECS049
00772                                                                   ECS049
00773      MOVE    ' '                 TO  X.                           ECS049
00774      MOVE    CO-DTL-4            TO  PRT.                         ECS049
00775      PERFORM 800-FICH-PRNT  THRU 800-EXIT.                        ECS049
00776                                                                   ECS049
00777      ADD     +1                  TO  LINE-CNT.                    ECS049
00778                                                                   ECS049
00779      MOVE RE-CEDING-TYPE-FLAG    TO   CD5-CEDE-TYPE.              ECS049
00780      MOVE RE-REINS-GROUPING-CODE TO   CD5-REIN-GROUP.             ECS049
00781      MOVE     RE-PRT-ST          TO   CD5-ST.                     ECS049
00782      MOVE     RE-PRT-OW          TO   CD5-OW.                     ECS049
00783      MOVE     RE-MORT-CODE       TO   CD5-MORT.                   ECS049
00784      MOVE     RE-MORT-SW         TO   CD5-MORT-SW.                ECS049
00785      MOVE     RE-CLAIM-CODE      TO   CD5-CLM-CD.                 ECS049
00786                                                                   ECS049
00787      IF RE-CLM-INCURRED-LIM NUMERIC AND                           ECS049
00788         RE-CLM-INCURRED-LIM NOT EQUAL ZEROS                       ECS049
00789          MOVE RE-CLM-MO          TO   CD5-CLM-MO                  ECS049
00790          MOVE '/'                TO   CD5-SLSH-1                  ECS049
00791          MOVE RE-CLM-DA          TO   CD5-CLM-DA                  ECS049
00792          MOVE '/'                TO   CD5-SLSH-2                  ECS049
00793          MOVE RE-CLM-YR          TO   CD5-CLM-YR                  ECS049
00794      ELSE                                                         ECS049
00795          MOVE SPACES             TO   CD5-CLM-MO                  ECS049
00796                                       CD5-SLSH-1                  ECS049
00797                                       CD5-CLM-DA                  ECS049
00798                                       CD5-SLSH-2                  ECS049
00799                                       CD5-CLM-YR.                 ECS049
00800                                                                   ECS049
00801      IF RE-LF-FEE-RANGE-PCT (1) IS NUMERIC                        ECS049
00802          MULTIPLY RE-LF-FEE-RANGE-PCT (1) BY +100                 ECS049
00803                              GIVING   CD5-LF-BR-FEE1              ECS049
00804      ELSE                                                         ECS049
00805          MOVE ZEROS              TO   CD5-LF-BR-FEE1.             ECS049
00806                                                                   ECS049
00807      IF RE-LF-FEE-THRU-AMT (1) IS NUMERIC                         ECS049
00808          MOVE RE-LF-FEE-THRU-AMT (1)                              ECS049
00809                                  TO   CD5-LF-BR-AMT1              ECS049
00810      ELSE                                                         ECS049
00811          MOVE ZEROS              TO   CD5-LF-BR-AMT1.             ECS049
00812                                                                   ECS049
00813      IF RE-AH-FEE-RANGE-PCT (1) IS NUMERIC                        ECS049
00814          MULTIPLY RE-AH-FEE-RANGE-PCT (1) BY +100                 ECS049
00815                              GIVING   CD5-AH-BR-FEE1              ECS049
00816      ELSE                                                         ECS049
00817          MOVE ZEROS              TO   CD5-AH-BR-FEE1.             ECS049
00818                                                                   ECS049
00819      IF RE-AH-FEE-THRU-AMT (1) IS NUMERIC                         ECS049
00820          MOVE RE-AH-FEE-THRU-AMT (1)                              ECS049
00821                                  TO   CD5-AH-BR-AMT1              ECS049
00822      ELSE                                                         ECS049
00823          MOVE ZEROS              TO   CD5-AH-BR-AMT1.             ECS049
00824                                                                   ECS049
00825      MOVE    ' '                 TO  X.                           ECS049
00826      MOVE    CO-DTL-5            TO  PRT.                         ECS049
00827      PERFORM 800-FICH-PRNT  THRU 800-EXIT.                        ECS049
00828                                                                   ECS049
00829      ADD     +1                  TO  LINE-CNT.                    ECS049
00830                                                                   ECS049
00831      IF RE-LF-FEE-RANGE-PCT (2) IS NUMERIC                        ECS049
00832          MULTIPLY RE-LF-FEE-RANGE-PCT (2) BY +100                 ECS049
00833                              GIVING   CD6-LF-BR-FEE2              ECS049
00834      ELSE                                                         ECS049
00835          MOVE ZEROS              TO   CD6-LF-BR-FEE2.             ECS049
00836                                                                   ECS049
00837      IF RE-LF-FEE-THRU-AMT (2) IS NUMERIC                         ECS049
00838          MOVE RE-LF-FEE-THRU-AMT (2)                              ECS049
00839                                  TO   CD6-LF-BR-AMT2              ECS049
00840      ELSE                                                         ECS049
00841          MOVE ZEROS              TO   CD6-LF-BR-AMT2.             ECS049
00842                                                                   ECS049
00843      IF RE-AH-FEE-RANGE-PCT (2) IS NUMERIC                        ECS049
00844          MULTIPLY RE-AH-FEE-RANGE-PCT (2) BY +100                 ECS049
00845                              GIVING   CD6-AH-BR-FEE2              ECS049
00846      ELSE                                                         ECS049
00847          MOVE ZEROS              TO   CD6-AH-BR-FEE2.             ECS049
00848                                                                   ECS049
00849      IF RE-AH-FEE-THRU-AMT (2) IS NUMERIC                         ECS049
00850          MOVE RE-AH-FEE-THRU-AMT (2)                              ECS049
00851                                  TO   CD6-AH-BR-AMT2              ECS049
00852      ELSE                                                         ECS049
00853          MOVE ZEROS              TO   CD6-AH-BR-AMT2.             ECS049
00854                                                                   ECS049
00855      MOVE    ' '                 TO  X.                           ECS049
00856      MOVE    CO-DTL-6            TO  PRT.                         ECS049
00857      PERFORM 800-FICH-PRNT  THRU 800-EXIT.                        ECS049
00858                                                                   ECS049
00859      ADD     +1                  TO  LINE-CNT.                    ECS049
00860                                                                   ECS049
00861      IF RE-LF-FEE-RANGE-PCT (3) IS NUMERIC                        ECS049
00862          MULTIPLY RE-LF-FEE-RANGE-PCT (3) BY +100                 ECS049
00863                              GIVING   CD7-LF-BR-FEE3              ECS049
00864      ELSE                                                         ECS049
00865          MOVE ZEROS              TO   CD7-LF-BR-FEE3.             ECS049
00866                                                                   ECS049
00867      IF RE-LF-FEE-THRU-AMT (3) IS NUMERIC                         ECS049
00868          MOVE RE-LF-FEE-THRU-AMT (3)                              ECS049
00869                                  TO   CD7-LF-BR-AMT3              ECS049
00870      ELSE                                                         ECS049
00871          MOVE ZEROS              TO   CD7-LF-BR-AMT3.             ECS049
00872                                                                   ECS049
00873      IF RE-AH-FEE-RANGE-PCT (3) IS NUMERIC                        ECS049
00874          MULTIPLY RE-AH-FEE-RANGE-PCT (3) BY +100                 ECS049
00875                              GIVING   CD7-AH-BR-FEE3              ECS049
00876      ELSE                                                         ECS049
00877          MOVE ZEROS              TO   CD7-AH-BR-FEE3.             ECS049
00878                                                                   ECS049
00879      IF RE-AH-FEE-THRU-AMT (3) IS NUMERIC                         ECS049
00880          MOVE RE-AH-FEE-THRU-AMT (3)                              ECS049
00881                                  TO   CD7-AH-BR-AMT3              ECS049
00882      ELSE                                                         ECS049
00883          MOVE ZEROS              TO   CD7-AH-BR-AMT3.             ECS049
00884                                                                   ECS049
00885      MOVE    ' '                 TO  X.                           ECS049
00886      MOVE    CO-DTL-7            TO  PRT.                         ECS049
00887      PERFORM 800-FICH-PRNT  THRU 800-EXIT.                        ECS049
00888                                                                   ECS049
00889      ADD     +1                  TO  LINE-CNT.                    ECS049
00890                                                                   ECS049
00891      MOVE     RE-LF-PE           TO   CD8-LF-PE.                  ECS049
00892                                                                   ECS049
00893      IF RE-LF-FEE IS NUMERIC                                      ECS049
00894          MULTIPLY RE-LF-FEE BY +100 GIVING CD8-LF-FEE             ECS049
00895      ELSE                                                         ECS049
00896          MOVE ZEROS              TO   CD8-LF-FEE.                 ECS049
00897                                                                   ECS049
00898      IF RE-LF-FEE = ZEROES                                        ECS049
00899          MOVE 'ACCOUNT'          TO CD8-LF-MSG.                   ECS049
00900                                                                   ECS049
00901      IF RE-ZERO-LF-FEE = 'Y'                                      ECS049
00902          MOVE ' ZERO  '          TO CD8-LF-MSG.                   ECS049
00903                                                                   ECS049
00904      IF RE-LF-PR-PCT IS NUMERIC                                   ECS049
00905          MULTIPLY RE-LF-PR-PCT BY +100 GIVING CD8-LF-PR-PCT       ECS049
00906      ELSE                                                         ECS049
00907          MOVE ZEROS              TO   CD8-LF-PR-PCT.              ECS049
00908                                                                   ECS049
00909      IF RE-LF-78-PCT IS NUMERIC                                   ECS049
00910          MULTIPLY RE-LF-78-PCT BY +100 GIVING CD8-LF-78-PCT       ECS049
00911      ELSE                                                         ECS049
00912          MOVE ZEROS              TO   CD8-LF-78-PCT.              ECS049
00913                                                                   ECS049
00914      IF RE-LF-IBNR-PCT IS NUMERIC                                 ECS049
00915          MULTIPLY RE-LF-IBNR-PCT BY +100 GIVING CD8-LF-IBNR-PCT   ECS049
00916      ELSE                                                         ECS049
00917          MOVE ZEROS              TO   CD8-LF-IBNR-PCT.            ECS049
00918                                                                   ECS049
00919      MOVE    RE-ZERO-LF-FEE      TO  CD8-Z-L-FE.                  ECS049
00920      MOVE    RE-LF-COMM          TO  CD8-L-COMM.                  ECS049
00921      MOVE    RE-LF-TAX           TO  CD8-L-TAX.                   ECS049
00922                                                                   ECS049
00923      IF RE-LF-CLM-PCT IS NUMERIC                                  ECS049
00924          MULTIPLY RE-LF-CLM-PCT BY +100 GIVING CD8-LF-CLM-PCT     ECS049
00925      ELSE                                                         ECS049
00926          MOVE ZEROS              TO   CD8-LF-CLM-PCT.             ECS049
00927                                                                   ECS049
00928      IF RE-LF-CLM-MAX IS NUMERIC                                  ECS049
00929          MOVE RE-LF-CLM-MAX      TO  CD8-LF-CLM-MAX               ECS049
00930      ELSE                                                         ECS049
00931          MOVE ZEROS              TO   CD8-LF-CLM-MAX.             ECS049
00932                                                                   ECS049
00933      MOVE    RE-LF-FEE-METHOD    TO  CD8-LF-FEE-MTHD.             ECS049
00934      MOVE    RE-AH-FEE-METHOD    TO  CD8-AH-FEE-MTHD.             ECS049
00935                                                                   ECS049
00936      IF RE-LF-FEE-RANGE-PCT (4) IS NUMERIC                        ECS049
00937          MULTIPLY RE-LF-FEE-RANGE-PCT (4) BY +100                 ECS049
00938                              GIVING   CD8-LF-BR-FEE4              ECS049
00939      ELSE                                                         ECS049
00940          MOVE ZEROS              TO   CD8-LF-BR-FEE4.             ECS049
00941                                                                   ECS049
00942      IF RE-LF-FEE-THRU-AMT (4) IS NUMERIC                         ECS049
00943          MOVE RE-LF-FEE-THRU-AMT (4)                              ECS049
00944                                  TO   CD8-LF-BR-AMT4              ECS049
00945      ELSE                                                         ECS049
00946          MOVE ZEROS              TO   CD8-LF-BR-AMT4.             ECS049
00947                                                                   ECS049
00948      IF RE-AH-FEE-RANGE-PCT (4) IS NUMERIC                        ECS049
00949          MULTIPLY RE-AH-FEE-RANGE-PCT (4) BY +100                 ECS049
00950                              GIVING   CD8-AH-BR-FEE4              ECS049
00951      ELSE                                                         ECS049
00952          MOVE ZEROS              TO   CD8-AH-BR-FEE4.             ECS049
00953                                                                   ECS049
00954      IF RE-AH-FEE-THRU-AMT (4) IS NUMERIC                         ECS049
00955          MOVE RE-AH-FEE-THRU-AMT (4)                              ECS049
00956                                  TO   CD8-AH-BR-AMT4              ECS049
00957      ELSE                                                         ECS049
00958          MOVE ZEROS              TO   CD8-AH-BR-AMT4.             ECS049
00959                                                                   ECS049
00960      MOVE    ' '                 TO  X.                           ECS049
00961      MOVE    CO-DTL-8            TO  PRT.                         ECS049
00962      PERFORM 800-FICH-PRNT  THRU 800-EXIT.                        ECS049
00963                                                                   ECS049
00964      ADD     +1                  TO  LINE-CNT.                    ECS049
00965                                                                   ECS049
00966      MOVE     RE-AH-PE           TO   CD9-AH-PE.                  ECS049
00967                                                                   ECS049
00968      IF RE-AH-FEE IS NUMERIC                                      ECS049
00969          MULTIPLY RE-AH-FEE BY +100 GIVING CD9-AH-FEE             ECS049
00970      ELSE                                                         ECS049
00971          MOVE ZEROS              TO   CD9-AH-FEE.                 ECS049
00972                                                                   ECS049
00973                                                                   ECS049
00974      IF RE-AH-FEE = ZEROES                                        ECS049
00975          MOVE 'ACCOUNT'          TO CD9-AH-MSG.                   ECS049
00976                                                                   ECS049
00977      IF RE-ZERO-AH-FEE = 'Y'                                      ECS049
00978          MOVE ' ZERO  '          TO CD9-AH-MSG.                   ECS049
00979                                                                   ECS049
00980                                                                   ECS049
00981      IF RE-AH-PR-PCT IS NUMERIC                                   ECS049
00982          MULTIPLY RE-AH-PR-PCT BY +100 GIVING CD9-AH-PR-PCT       ECS049
00983      ELSE                                                         ECS049
00984          MOVE ZEROS              TO   CD9-AH-PR-PCT.              ECS049
00985                                                                   ECS049
00986      IF RE-AH-78-PCT IS NUMERIC                                   ECS049
00987          MULTIPLY RE-AH-78-PCT BY +100 GIVING CD9-AH-78-PCT       ECS049
00988      ELSE                                                         ECS049
00989          MOVE ZEROS              TO   CD9-AH-78-PCT.              ECS049
00990                                                                   ECS049
00991      IF RE-AH-IBNR-PCT IS NUMERIC                                 ECS049
00992          MULTIPLY RE-AH-IBNR-PCT BY +100 GIVING CD9-AH-IBNR-PCT   ECS049
00993      ELSE                                                         ECS049
00994          MOVE ZEROS              TO   CD9-AH-IBNR-PCT.            ECS049
00995                                                                   ECS049
00996      MOVE    RE-ZERO-AH-FEE      TO  CD9-Z-A-FE.                  ECS049
00997      MOVE    RE-AH-COMM          TO  CD9-A-COMM.                  ECS049
00998      MOVE    RE-AH-TAX           TO  CD9-A-TAX.                   ECS049
00999                                                                   ECS049
01000      IF RE-AH-CLM-PCT IS NUMERIC                                  ECS049
01001          MULTIPLY RE-AH-CLM-PCT BY +100 GIVING CD9-AH-CLM-PCT     ECS049
01002      ELSE                                                         ECS049
01003          MOVE ZEROS              TO   CD9-AH-CLM-PCT.             ECS049
01004                                                                   ECS049
01005      IF RE-AH-CLM-MAX IS NUMERIC                                  ECS049
01006          MOVE RE-AH-CLM-MAX      TO  CD9-AH-CLM-MAX               ECS049
01007      ELSE                                                         ECS049
01008          MOVE ZEROS              TO   CD9-AH-CLM-MAX.             ECS049
01009                                                                   ECS049
01010      IF RE-LF-FEE-RANGE-PCT (5) IS NUMERIC                        ECS049
01011          MULTIPLY RE-LF-FEE-RANGE-PCT (5) BY +100                 ECS049
01012                              GIVING   CD9-LF-BR-FEE5              ECS049
01013      ELSE                                                         ECS049
01014          MOVE ZEROS              TO   CD9-LF-BR-FEE5.             ECS049
01015                                                                   ECS049
01016      IF RE-LF-FEE-THRU-AMT (5) IS NUMERIC                         ECS049
01017          MOVE RE-LF-FEE-THRU-AMT (5)                              ECS049
01018                                  TO   CD9-LF-BR-AMT5              ECS049
01019      ELSE                                                         ECS049
01020          MOVE ZEROS              TO   CD9-LF-BR-AMT5.             ECS049
01021                                                                   ECS049
01022      IF RE-AH-FEE-RANGE-PCT (5) IS NUMERIC                        ECS049
01023          MULTIPLY RE-AH-FEE-RANGE-PCT (5) BY +100                 ECS049
01024                              GIVING   CD9-AH-BR-FEE5              ECS049
01025      ELSE                                                         ECS049
01026          MOVE ZEROS              TO   CD9-AH-BR-FEE5.             ECS049
01027                                                                   ECS049
01028      IF RE-LF-FEE-THRU-AMT (5) IS NUMERIC                         ECS049
01029          MOVE RE-LF-FEE-THRU-AMT (5)                              ECS049
01030                                  TO   CD9-AH-BR-AMT5              ECS049
01031      ELSE                                                         ECS049
01032          MOVE ZEROS              TO   CD9-AH-BR-AMT5.             ECS049
01033                                                                   ECS049
01034      MOVE    ' '                 TO  X.                           ECS049
01035      MOVE    CO-DTL-9            TO  PRT.                         ECS049
01036      PERFORM 800-FICH-PRNT  THRU 800-EXIT.                        ECS049
01037                                                                   ECS049
01038      ADD     +1                  TO  LINE-CNT.                    ECS049
01039                                                                   ECS049
01040      MOVE     RE-LF-FEE-BASIS    TO   CD10-LF-FEE-BAS.            ECS049
01041      MOVE     RE-AH-FEE-BASIS    TO   CD10-AH-FEE-BAS.            ECS049
01042                                                                   ECS049
01043      IF RE-LF-FEE-RANGE-PCT (6) IS NUMERIC                        ECS049
01044          MULTIPLY RE-LF-FEE-RANGE-PCT (6) BY +100                 ECS049
01045                              GIVING   CD10-LF-BR-FEE6             ECS049
01046      ELSE                                                         ECS049
01047          MOVE ZEROS              TO   CD10-LF-BR-FEE6.            ECS049
01048                                                                   ECS049
01049      IF RE-LF-FEE-THRU-AMT (6) IS NUMERIC                         ECS049
01050          MOVE RE-LF-FEE-THRU-AMT (6)                              ECS049
01051                                  TO   CD10-LF-BR-AMT6             ECS049
01052      ELSE                                                         ECS049
01053          MOVE ZEROS              TO   CD10-LF-BR-AMT6.            ECS049
01054                                                                   ECS049
01055      IF RE-AH-FEE-RANGE-PCT (6) IS NUMERIC                        ECS049
01056          MULTIPLY RE-AH-FEE-RANGE-PCT (6) BY +100                 ECS049
01057                              GIVING   CD10-AH-BR-FEE6             ECS049
01058      ELSE                                                         ECS049
01059          MOVE ZEROS              TO   CD10-AH-BR-FEE6.            ECS049
01060                                                                   ECS049
01061      IF RE-LF-FEE-THRU-AMT (6) IS NUMERIC                         ECS049
01062          MOVE RE-LF-FEE-THRU-AMT (6)                              ECS049
01063                                  TO   CD10-AH-BR-AMT6             ECS049
01064      ELSE                                                         ECS049
01065          MOVE ZEROS              TO   CD10-AH-BR-AMT6.            ECS049
01066                                                                   ECS049
01067      MOVE    ' '                 TO  X.                           ECS049
01068      MOVE    CO-DTL-10           TO  PRT.                         ECS049
01069      PERFORM 800-FICH-PRNT  THRU 800-EXIT.                        ECS049
01070                                                                   ECS049
01071      ADD     +1                  TO  LINE-CNT.                    ECS049
01072                                                                   ECS049
01073  300-EXIT.                                                        ECS049
01074      EXIT.                                                        ECS049
01075      EJECT                                                        ECS049
01076  320-BUILD-B-TABLE.                                               ECS049
01077 ****************************************************************  ECS049
01078 ** B-TABLE CONTAINS A LIST OF UNIQUE COMPANY NUMBERS EXTRACTED    ECS049
01079 ** FROM THE INPUT COMPANY RECORDS.  B-TABLE-MAX IS THE TABLE SIZE ECS049
01080 ** PLUS 1 ENTRY, A TABLE OVERFLOW CONDITION.                      ECS049
01081 ****************************************************************  ECS049
01082                                                                   ECS049
01083      IF  B-SUB = B-TABLE-MAX                                      ECS049
01084          MOVE +0202              TO WS-RETURN-CODE                ECS049
01085          MOVE 'B-TABLE FILLED WITH 2500 ENTRYS, CANNOT CONTINUE.' ECS049
01086                                  TO WS-ABEND-MESSAGE              ECS049
01087          GO TO ABEND-PGM.                                         ECS049
01088                                                                   ECS049
01089      IF  B-TYPE-COMPANY (B-SUB) = HIGH-VALUES                     ECS049
01090          MOVE RE-COMPANY         TO B-TYPE-COMPANY (B-SUB)        ECS049
01091          MOVE 3000               TO B-SUB                         ECS049
01092      ELSE                                                         ECS049
01093          IF  RE-COMPANY = B-TYPE-COMPANY (B-SUB)                  ECS049
01094              MOVE 3000           TO B-SUB.                        ECS049
01095                                                                   ECS049
01096  320-EXIT.                                                        ECS049
01097      EXIT.                                                        ECS049
01098      EJECT                                                        ECS049
01099  330-COMPANY-RPT-HEADINGS.                                        ECS049
01100      PERFORM 700-RPT-HEADINGS THRU 700-EXIT.                      ECS049
01101                                                                   ECS049
01102  330-EXIT.                                                        ECS049
01103      EXIT.                                                        ECS049
01104      EJECT                                                        ECS049
01105                                                                   ECS049
01106  500-EOJ.                                                         ECS049
01107      PERFORM 520-NEW-REPORT-PAGE THRU 520-EXIT.                   ECS049
01108                                                                   ECS049
01109      MOVE ZEROS                  TO A-SUB.                        ECS049
01110                                                                   ECS049
01111      PERFORM 510-TABLE-COMPARE-LOGIC THRU 510-EXIT.               ECS049
01112                                                                   ECS049
01113      IF NO-MISS-B                                                 ECS049
01114          MOVE '0'                TO X                             ECS049
01115          MOVE NO-MISS-B-MSG      TO PRT                           ECS049
01116          PERFORM 800-FICH-PRNT THRU 800-EXIT.                     ECS049
01117                                                                   ECS049
01118  500-EXIT.                                                        ECS049
01119      EXIT.                                                        ECS049
01120      EJECT                                                        ECS049
01121  510-TABLE-COMPARE-LOGIC.                                         ECS049
01122      MOVE ZEROS                  TO B-SUB.                        ECS049
01123                                                                   ECS049
01124      ADD  1     TO A-SUB.                                         ECS049
01125                                                                   ECS049
01126      IF A-TYPE-COMPANY (A-SUB) = HIGH-VALUES                      ECS049
01127          GO TO 510-EXIT.                                          ECS049
01128                                                                   ECS049
01129  510-A-B-COMPARE.                                                 ECS049
01130      ADD +1 TO B-SUB.                                             ECS049
01131                                                                   ECS049
01132      IF  B-TYPE-COMPANY (B-SUB) = HIGH-VALUES                     ECS049
01133          MOVE    'X'             TO  MISS-B-SW                    ECS049
01134          MOVE    A-TYPE-COMPANY (A-SUB) TO  E68-FLD               ECS049
01135          MOVE    '0'             TO  X                            ECS049
01136          MOVE    ERR-68          TO  PRT                          ECS049
01137          PERFORM 800-FICH-PRNT         THRU 800-EXIT              ECS049
01138          GO TO 510-TABLE-COMPARE-LOGIC.                           ECS049
01139                                                                   ECS049
01140      IF B-TYPE-COMPANY (B-SUB) = A-TYPE-COMPANY (A-SUB)           ECS049
01141          GO TO 510-TABLE-COMPARE-LOGIC.                           ECS049
01142                                                                   ECS049
01143      GO TO 510-A-B-COMPARE.                                       ECS049
01144                                                                   ECS049
01145  510-EXIT.                                                        ECS049
01146      EXIT.                                                        ECS049
01147      EJECT                                                        ECS049
01148                                                                   ECS049
01149  520-NEW-REPORT-PAGE.                                             ECS049
01150      PERFORM 700-RPT-HEADINGS THRU 700-EXIT.                      ECS049
01151                                                                   ECS049
01152      MOVE '0'                    TO X.                            ECS049
01153      MOVE SPACES                 TO PRT.                          ECS049
01154      PERFORM 800-FICH-PRNT THRU 800-EXIT.                         ECS049
01155                                                                   ECS049
01156  520-EXIT.                                                        ECS049
01157      EXIT.                                                        ECS049
01158                                                                   ECS049
01159  700-RPT-HEADINGS.                                                ECS049
01160      ADD     +1  TO  PAGE-CNT.                                    ECS049
01161      MOVE    PAGE-CNT            TO  H3-PAGE.                     ECS049
01162      MOVE    '1'                 TO  X.                           ECS049
01163      MOVE    HDR-1               TO  PRT.                         ECS049
01164      PERFORM 800-FICH-PRNT THRU 800-EXIT.                         ECS049
01165                                                                   ECS049
01166      MOVE    SPACE               TO  X.                           ECS049
01167      MOVE    HDR-2               TO  PRT.                         ECS049
01168      PERFORM 800-FICH-PRNT THRU 800-EXIT.                         ECS049
01169                                                                   ECS049
01170      MOVE    SPACE               TO  X.                           ECS049
01171      MOVE    HDR-3               TO  PRT.                         ECS049
01172      PERFORM 800-FICH-PRNT THRU 800-EXIT.                         ECS049
01173                                                                   ECS049
01174      MOVE    +1                  TO  LINE-CNT.                    ECS049
01175                                                                   ECS049
01176  700-EXIT.                                                        ECS049
01177      EXIT.                                                        ECS049
01178      EJECT                                                        ECS049
01179                                                                   ECS049
01180  800-FICH-PRNT.                                                   ECS049
01181                              COPY ELCPRT2.                        ECS049
01182                                                                   ECS049
01183  800-EXIT.                                                        ECS049
01184      EXIT.                                                        ECS049
01185      EJECT                                                        ECS049
01186  900-ACCOUNTING.                                                  ECS049
01187 *                                                                 ECS049
01188 *                                                                 ECS049
01189 * LEAVE THE ABOVE 2 LINE IN TO PREVENT LOSING THE LINE ABOVE      ECS049
01201                                                                   ECS049
01202  REIN-DATE-LOAD.                                                     CL**2
01203      COPY ELCRENM1.                                                  CL**2
01204                                                                   ECS049
01205  900-EXIT.                                                        ECS049
01206      EXIT.                                                        ECS049
01207                                                                   ECS049
01208  ABEND-PGM  SECTION.                                              ECS049
01209                             COPY ELCABEND.                        ECS049
01210                                                                   ECS049
