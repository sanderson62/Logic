00001  IDENTIFICATION DIVISION.                                         00000010
00002                                                                   00000020
00003  PROGRAM-ID.                 ECSABEND.                            00000030
00004 *                            VMOD=2.001.                          00000031
00005                                                                   00000050
00006  AUTHOR.     LOGIC, INC.                                          00000060
00007              DALLAS, TEXAS.                                       00000070
00008                                                                   00000080
00009  DATE-COMPILED.                                                   00000090
00010                                                                   00000100
00011              *****************************************************00000110
00012              *                                                   *00000120
00013              *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *00000130
00014              *                                                   *00000140
00015              *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *00000150
00016              *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *00000160
00017              *   THE PRIOR WRITTEN PERMISSION OF LOGIC, INC.     *00000170
00018              *                                                   *00000180
00019              *****************************************************00000190
00020                                                                   00000200
00021  REMARKS.                                                         00000210
00022          STANDARD ABEND MODULE FOR DOS.                           00000220
00023                                                                   00000230
00024          THE STANDARD VSAM FILE ERROR ABEND WILL BE AS FOLLOWS:   00000240
00025                                                                   00000250
00026          ABEND CODE - ABCD    WHERE                               00000260
00027                                                                   00000270
00028                                                                   00000280
00029              A = FILE WHICH ERROR OCCURED                         00000290
00030                  1 - ACCOUNT MASTER                               00000300
00031                  2 - REINSURANCE TABLE                            00000310
00032                  3 - RATE FILE                                    00000320
00033                  4 - COMMISSION TABLE                             00000330
00034                  5 - MORTALITY TABLE                              00000340
00035                                                                   00000350
00036              B = VSAM REQUEST WHICH CAUSED ERROR                  00000360
00037                  1 - OPEN                                         00000370
00038                  2 - CLOSE                                        00000380
00039                  3 - START                                        00000390
00040                  4 - READ                                         00000400
00041                  5 - READNEXT                                     00000410
00042                  6 - WRITE                                        00000420
00043                  7 - REWRITE                                      00000430
00044                  8 - DELETE                                       00000440
00045                                                                   00000450
00046              CD = COBOL FILE STATUS VALUES                        00000460
00047                  00 - SUCCESSFUL                                  00000470
00048                  02 - DUPLICATE KEY, AND DUPLICATES SPECIFIED     00000480
00049                                                                   00000490
00050                  10 - AT END                                      00000500
00051                                                                   00000510
00052                  20 - INVALID KEY NO FURTHER INFORMATION          00000520
00053                  21 - SEQUENCE ERROR                              00000530
00054                  22 - DUPLICATE KEY                               00000540
00055                  23 - NO RECORD FOUND                             00000550
00056                  24 - BOUNDARY VIOLATION                          00000560
00057                                                                   00000570
00058                  30 - PERMANENT ERROR                             00000580
00059                  34 - BOUNDARY VIOLATION (SEQUENTIAL FILE)        00000590
00060                                                                   00000600
00061                  90 - OTHER ERROR (NO FURTHER INFORMATION)        00000610
00062                  91 - PASSWORD FAILURE                            00000620
00063                  92 - LOGIC ERROR                                 00000630
00064                  93 - RESOURCE NOT AVAILABLE                      00000640
00065                  94 - NO CURRENT RECORD POINTER FOR SEQUENTIAL    00000650
00066                       REQUEST                                     00000660
00067                  95 - INVALID OR INCOMPLETE FILE INFORMATION      00000670
00068                  96 - NO FILE IDENTIFICATION                      00000680
00069                  97 - OPEN STATEMENT EXECUTION SUCCESSFUL         00000690
00070                       FILE INTEGRITY VERIFIED                     00000700
00071  EJECT                                                            00000710
00072  ENVIRONMENT DIVISION.                                            00000720
00073                                                                   00000730
00074  DATA DIVISION.                                                   00000740
00075                                                                   00000750
00076  WORKING-STORAGE SECTION.                                         00000760
00077  77  FILLER   PIC X(32) VALUE '********************************'. 00000770
00078  77  FILLER   PIC X(32) VALUE '     ECSABEND WORKING-STORAGE   '. 00000780
00079  77  FILLER   PIC X(32) VALUE '******** VMOD=2.001 ************'. 00000790
00080                                                                   00000800
00081  77  X1                          PIC S9(4)   COMP    VALUE ZERO.  00000810
00082  77  X2                          PIC S9(4)   COMP    VALUE ZERO.  00000820
00083                                                                   00000830
00084  01  ERROR-MESSAGE.                                               00000840
00085      12  FILLER                  PIC X(33)           VALUE        00000850
00086              '0100INTETNAL SORT    ABORTED     '.                 00000860
00087      12  FILLER                  PIC X(33)           VALUE        00000870
00088              '0200PROGRAM TABLE    EXCEEDED    '.                 00000880
00089      12  FILLER                  PIC X(33)           VALUE        00000890
00090              '0301FATAL DATA ERROR             '.                 00000900
00091      12  FILLER                  PIC X(33)           VALUE        00000910
00092              '0302FATAL FILE ERROR             '.                 00000920
00093      12  FILLER                  PIC X(33)           VALUE        00000930
00094              '0401INVALID LIFE TYPE            '.                 00000940
00095      12  FILLER                  PIC X(33)           VALUE        00000950
00096              '0402INVALID A & H  TYPE          '.                 00000960
00097      12  FILLER                  PIC X(33)           VALUE        00000970
00098              '0500SYS0   DISK EXCEEDED OR ERROR'.                 00000980
00099      12  FILLER                  PIC X(33)           VALUE        00000990
00100              '0600SYS0   SEQUENCE ERROR        '.                 00001000
00101      12  FILLER                  PIC X(33)           VALUE        00001010
00102              '0700SYS0   NO INPUT DATA         '.                 00001020
00103      12  FILLER                  PIC X(33)           VALUE        00001030
00104              '0998DATE CARD INVALID FOR PROGRAM'.                 00001040
00105      12  FILLER                  PIC X(33)           VALUE        00001050
00106              '0999MISSING OR INVALID DATE CARD '.                 00001060
00107      12  FILLER                  PIC X(33)           VALUE        00001070
00108              '9999UNKNOWN ABEND                '.                 00001080
00109                                                                   00001090
00110  01  FILLER REDEFINES ERROR-MESSAGE.                              00001100
00111      12  FILLER          OCCURS 2 TIMES.                          00001110
00112          16  E-M-CODE            PIC  X(4).                       00001120
00113          16  E-M                 PIC  X(29).                      00001130
00114                                                                   00001140
00115  01  DTL-MESS.                                                    00001150
00116      12  FILLER                  PIC  X(17)      VALUE            00001160
00117              'ECS ABORT CODE - '.                                 00001170
00118      12  D-CODE.                                                  00001180
00119          16  D-CODE-1.                                            00001190
00120              20  D-FILE          PIC  X.                          00001200
00121              20  D-REQUEST       PIC  X.                          00001210
00122          16  D-CODE-2.                                            00001220
00123              20  D-FILE-STATUS1  PIC  X.                          00001230
00124              20  D-FILE-STATUS2  PIC  X.                          00001240
00125      12  FILLER                  PIC  XXX        VALUE ' - '.     00001250
00126      12  D-MESS.                                                  00001260
00127          16  FILLER              PIC  X(4).                       00001270
00128          16  D-MESS-1            PIC  XX.                         00001280
00129          16  FILLER              PIC  X(8).                       00001290
00130          16  D-MESS-2            PIC  XX.                         00001300
00131          16  FILLER              PIC  X(13).                      00001310
00132                                                                   00001320
00133  01  ABEND-MESSAGE.                                               00001330
00134      12  FILLER                  PIC X(14)       VALUE            00001340
00135              'ERROR OCCURED'.                                     00001350
00136      12  ABEND-FILE              PIC X(17)       VALUE SPACES.    00001360
00137      12  FILLER                  PIC XXX         VALUE ' - '.     00001370
00138      12  ABEND-REQUEST           PIC X(9)        VALUE SPACES.    00001380
00139      12  FILLER                  PIC X(12)       VALUE            00001390
00140              'FILE STATUS='.                                      00001400
00141      12  ABEND-FILE-STATUS       PIC XX          VALUE ZERO.      00001410
00142                                                                   00001420
00143  LINKAGE SECTION.                                                 00001430
00144                                                                   00001440
00145  01  ABEND-CODE.                                                  00001450
00146      12  A-CODE-1                PIC  XX.                         00001460
00147      12  A-CODE-2                PIC  XX.                         00001470
00148                                                                   00001480
00149  01  ABEND-OPTION                PIC  X.                          00001490
00150                                                                   00001500
00151  EJECT                                                            00001510
00152  PROCEDURE DIVISION USING ABEND-CODE ABEND-OPTION.                00001520
00153                                                                   00001530
00154      MOVE ABEND-CODE TO D-CODE.                                   00001540
00155                                                                   00001550
00156      IF D-FILE = '1' OR '2' OR '3' OR '4' OR '5'                  00001560
00157          GO TO 3000-FILE-ERROR.                                   00001570
00158                                                                   00001580
00159      IF A-CODE-1 = '01' OR '02' OR '05' OR '06' OR '07'           00001590
00160          MOVE ZERO TO A-CODE-2.                                   00001600
00161                                                                   00001610
00162  1000-FIND-MESSAGE.                                               00001620
00163      ADD 1 TO X1.                                                 00001630
00164                                                                   00001640
00165      IF E-M-CODE (X1) = '9999' OR ABEND-CODE                      00001650
00166          GO TO 2000-FOUND-MESSAGE.                                00001660
00167                                                                   00001670
00168      GO TO 1000-FIND-MESSAGE.                                     00001680
00169                                                                   00001690
00170  2000-FOUND-MESSAGE.                                              00001700
00171      MOVE E-M (X1) TO D-MESS.                                     00001710
00172                                                                   00001720
00173      IF D-CODE-1 = '01' OR '02'                                   00001730
00174          MOVE D-CODE-2 TO D-MESS-2.                               00001740
00175                                                                   00001750
00176      IF D-CODE-1 = '05' OR '06' OR '07'                           00001760
00177          MOVE D-CODE-2 TO D-MESS-1.                               00001770
00178                                                                   00001780
00179      DISPLAY DTL-MESS UPON CONSOLE.                               00001790
00180      DISPLAY DTL-MESS.                                            00001800
00181                                                                   00001810
00182      DIVIDE X2 INTO X1.                                           00001820
00183                                                                   00001830
00184 **** NOTE ABOVE WILL CAUSE DECIMAL DIVIDE EXCEPTION AND CANCEL*** 00001840
00185                                                                   00001850
00186      GOBACK.                                                      00001860
00187                                                                   00001870
00188  EJECT                                                            00001880
00189  3000-FILE-ERROR.                                                 00001890
00190      IF D-FILE = '1'                                              00001900
00191          MOVE 'ACCOUNT MASTER'     TO  ABEND-FILE                 00001910
00192       ELSE                                                        00001920
00193      IF D-FILE = '2'                                              00001930
00194          MOVE 'REINSURANCE TABLE'  TO  ABEND-FILE                 00001940
00195       ELSE                                                        00001950
00196      IF D-FILE = '3'                                              00001960
00197          MOVE 'RATE FILE'          TO  ABEND-FILE                 00001970
00198       ELSE                                                        00001980
00199      IF D-FILE = '4'                                              00001990
00200          MOVE 'COMMISSION TABLE'   TO  ABEND-FILE                 00002000
00201       ELSE                                                        00002010
00202      IF D-FILE = '5'                                              00002020
00203          MOVE 'MORTALITY TABLE'    TO  ABEND-FILE                 00002030
00204       ELSE                                                        00002040
00205          MOVE D-FILE               TO  ABEND-FILE.                00002050
00206                                                                   00002060
00207      IF D-REQUEST = '1'                                           00002070
00208          MOVE 'OPEN'       TO  ABEND-REQUEST                      00002080
00209       ELSE                                                        00002090
00210      IF D-REQUEST = '2'                                           00002100
00211          MOVE 'CLOSE'      TO  ABEND-REQUEST                      00002110
00212       ELSE                                                        00002120
00213      IF D-REQUEST = '3'                                           00002130
00214          MOVE 'START'      TO  ABEND-REQUEST                      00002140
00215       ELSE                                                        00002150
00216      IF D-REQUEST = '4'                                           00002160
00217          MOVE 'READ'       TO  ABEND-REQUEST                      00002170
00218       ELSE                                                        00002180
00219      IF D-REQUEST = '5'                                           00002190
00220          MOVE 'READNEXT'   TO  ABEND-REQUEST                      00002200
00221       ELSE                                                        00002210
00222      IF D-REQUEST = '6'                                           00002220
00223          MOVE 'WRITE'      TO  ABEND-REQUEST                      00002230
00224       ELSE                                                        00002240
00225      IF D-REQUEST = '7'                                           00002250
00226          MOVE 'REWRITE'    TO  ABEND-REQUEST                      00002260
00227       ELSE                                                        00002270
00228      IF D-REQUEST = '8'                                           00002280
00229          MOVE 'DELETE'     TO  ABEND-REQUEST                      00002290
00230          MOVE D-REQUEST    TO  ABEND-REQUEST.                     00002300
00231                                                                   00002310
00232      MOVE D-CODE-2         TO  ABEND-FILE-STATUS.                 00002320
00233                                                                   00002330
00234      DISPLAY ABEND-MESSAGE.                                       00002340
00235      DISPLAY ABEND-MESSAGE UPON CONSOLE.                          00002350
00236                                                                   00002360
00237      DIVIDE X2 INTO X1.                                           00002370
00238                                                                   00002380
00239      GOBACK.                                                      00002390
