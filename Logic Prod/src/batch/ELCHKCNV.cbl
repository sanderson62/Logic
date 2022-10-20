00001  IDENTIFICATION DIVISION.                                         00000010
00002                                                                   00000020
00003  PROGRAM-ID.                 ELCHKCNV.                            00000030
00004 *                            VMOD=2.001.                          00000031
00005                                                                   00000050
00006  AUTHOR.        LOGIC, INC.                                       00000060
00007                 DALLAS, TEXAS.                                    00000070
00008                                                                   00000080
00009  DATE-COMPILED.                                                   00000090
00010                                                                   00000100
00011  SECURITY.   *****************************************************00000110
00012              *                                                   *00000120
00013              *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *00000130
00014              *                                                   *00000140
00015              *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *00000150
00016              *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *00000160
00017              *   THE PRIOR WRITTEN PERMISSION OF LOGIC, INC.     *00000170
00018              *                                                   *00000180
00019              *****************************************************00000190
00020                                                                   00000200
00021 *REMARKS.                                                         00000210
00022 *        CONVERT OLD CHECK QUE RECORDS TO REVISED CHECK QUE FORMAT00000220
00023                                                                   00000230
00024  EJECT                                                            00000240
00025  ENVIRONMENT DIVISION.                                            00000250
00026  CONFIGURATION SECTION.                                           00000260
00027  INPUT-OUTPUT SECTION.                                            00000270
00028  FILE-CONTROL.                                                    00000280
00029                                                                   00000290
00030      SELECT CHECK-QUE-IN   ASSIGN TO SYS007-UT-2400-S-ELCHKQI.    00000300
00031                                                                   00000310
00032      SELECT CHECK-QUE-OUT  ASSIGN TO SYS008-UT-2400-S-ELCHKQO.    00000320
00033                                                                   00000330
00034  EJECT                                                            00000340
00035  DATA DIVISION.                                                   00000350
00036  FILE SECTION.                                                    00000360
00037                                                                   00000370
00038  FD  CHECK-QUE-IN                                                 00000380
00039      LABEL RECORDS ARE STANDARD                                   00000390
00040      RECORD CONTAINS 60 CHARACTERS                                00000400
00041      BLOCK CONTAINS 0 RECORDS                                     00000410
00042      DATA RECORD IS CHECK-QUE.                                    00000420
00043                                                                   00000430
00044  01  CHECK-QUE.                                                   00000440
00045      12  CQ-RECORD-ID                PIC XX.                      00000450
00046          88  VALID-CQ-ID         VALUE 'CQ'.                      00000460
00047                                                                   00000470
00048      12  CQ-CONTROL-PRIMARY.                                      00000480
00049          16  CQ-COMPANY-CD           PIC X.                       00000490
00050          16  CQ-CONTROL-NUMBER       PIC S9(8)       COMP.        00000500
00051          16  CQ-SEQUENCE-NUMBER      PIC S9(4)       COMP.        00000510
00052                                                                   00000520
00053      12  CQ-ENTRY-TYPE               PIC X.                       00000530
00054              88  CHECK-ON-QUE           VALUE 'Q'.                00000540
00055              88  ALIGNMENT-CHECK        VALUE 'A'.                00000550
00056              88  SPOILED-CHECK          VALUE 'S'.                00000560
00057              88  PAYMENT-ABORTED        VALUE 'X'.                00000570
00058                                                                   00000580
00059      12  CQ-CLAIM-MAST-CNTL.                                      00000590
00060          16  CQ-CARRIER              PIC X.                       00000600
00061          16  CQ-CLAIM-NO             PIC X(7).                    00000610
00062          16  CQ-CERT-NO.                                          00000620
00063              20  CQ-CERT-PRIME       PIC X(10).                   00000630
00064              20  CQ-CERT-SFX         PIC X.                       00000640
00065          16  CQ-CLAIM-TYPE           PIC X.                       00000650
00066              88  CQ-LIFE-CLAIM          VALUE 'L'.                00000660
00067              88  CQ-AH-CLAIM            VALUE 'A'.                00000670
00068          16  CQ-CLAIM-SUB-TYPE       PIC X.                       00000680
00069              88  CQ-FIXED-COVERAGE      VALUE '1'.                00000690
00070              88  CQ-O-B-COVERAGE        VALUE '2'.                00000700
00071              88  CQ-OPEN-END-COVERAGE   VALUE '3'.                00000710
00072                                                                   00000720
00073      12  CQ-PMT-TRLR-SEQUENCE        PIC S9(4)       COMP.        00000730
00074      12  CQ-CHECK-NUMBER             PIC X(7).                    00000740
00075      12  CQ-CHECK-AMOUNT             PIC S9(7)V99    COMP-3.      00000750
00076      12  CQ-PAYMENT-TYPE             PIC X.                       00000760
00077              88  CQ-PARTIAL-PAYMENT        VALUE '1'.             00000770
00078              88  CQ-FINAL-PAYMENT          VALUE '2'.             00000780
00079              88  CQ-LUMP-SUM-PAYMENT       VALUE '3'.             00000790
00080              88  CQ-ADDITIONAL-PAYMENT     VALUE '4'.             00000800
00081              88  CQ-CHARGEABLE-EXPENSE     VALUE '5'.             00000810
00082              88  CQ-NON-CHARGEABLE-EXPENSE VALUE '6'.             00000820
00083              88  CQ-LIFE-PREMIUM-REFUND    VALUE '7'.             00000830
00084              88  CQ-AH-PREMIUM-REFUND      VALUE '8'.             00000840
00085      12  CQ-VOID-INDICATOR           PIC X.                       00000850
00086              88  CHECK-IS-VOID             VALUE 'V'.             00000860
00087      12  CQ-TIMES-PRINTED            PIC S9(4)       COMP.        00000870
00088      12  CQ-PRINT-AT-HHMM            PIC S9(4)       COMP.        00000880
00089      12  CQ-CHECK-BY-USER            PIC X(4).                    00000890
00090      12  CQ-PRE-NUMBERING-SW         PIC X.                       00000900
00091        88  CHECKS-WERE-NOT-PRE-NUMBERED    VALUE SPACE.           00000910
00092        88  CHECKS-WERE-PRE-NUMBERED        VALUE '1'.             00000920
00093                                                                   00000930
00094      12  CQ-CHECK-WRITTEN-DT         PIC XX.                      00000940
00095      12  CQ-LAST-UPDATED-BY          PIC S9(4)       COMP.        00000950
00096                                                                   00000960
00097 ******************************************************************00000970
00098      EJECT                                                        00000980
00099  FD  CHECK-QUE-OUT                                                00000990
00100      RECORD CONTAINS 100 CHARACTERS                               00001000
00101      BLOCK CONTAINS 0 RECORDS                                     00001010
00102      LABEL RECORDS ARE STANDARD.                                  00001020
00103                                                                   00001030
00104                              COPY ELCCHKQ                         00001040
00105      REPLACING CHECK-QUE BY CHECK-QUE-RCD.                        00001050
00106  EJECT                                                            00001060
00107  WORKING-STORAGE SECTION.                                         00001070
00108  77  FILLER  PIC X(32)  VALUE '********************************'. 00001080
00109  77  FILLER  PIC X(32)  VALUE '*   ELCHKCNV WORKING-STORAGE    '. 00001090
00110  77  FILLER  PIC X(32)  VALUE '******** VMOD=2.001 ************'. 00001100
00111  77  CHKQ-IN-CNT             PIC S9(7)   COMP-3  VALUE +0.        00001110
00112  77  CHKQ-OUT-CNT            PIC S9(7)   COMP-3  VALUE +0.        00001120
00113                                                                   00001130
00114  01  WS.                                                          00001140
00115      12  WS-RETURN-CODE        PIC S9(4)   COMP   VALUE +0.       00001150
00116      12  WS-ABEND-MESSAGE      PIC X(80)          VALUE SPACES.   00001160
00117      12  WS-ABEND-FILE-STATUS  PIC XX             VALUE ZEROS.    00001170
00118      12  WS-ZERO               PIC S9      COMP-3 VALUE +0.       00001180
00119                                                                   00001190
00120      12  WS-ABEND-CODE         PIC 9(4).                          00001200
00121      12  ABEND-CODE  REDEFINES  WS-ABEND-CODE.                    00001210
00122          16  ABEND-CODE-1    PIC XX.                              00001220
00123          16  ABEND-CODE-2    PIC XX.                              00001230
00124                                                                   00001240
00125      EJECT                                                        00001250
00126  PROCEDURE DIVISION.                                              00001260
00127                                                                   00001270
00128 ******************************************************************00001280
00129 ***           O P E N   F I L E S   R O U T I N E              ***00001290
00130 ******************************************************************00001300
00131                                                                   00001310
00132  0080-OPEN-FILES.                                                 00001320
00133                                                                   00001330
00134      OPEN INPUT  CHECK-QUE-IN                                     00001340
00135           OUTPUT CHECK-QUE-OUT.                                   00001350
00136                                                                   00001360
00137      EJECT                                                        00001370
00138 ******************************************************************00001380
00139 ***       R E A D   C H E C K   Q U E  R O U T I N E           ***00001390
00140 ******************************************************************00001400
00141                                                                   00001410
00142  0200-CHECK-QUE-READ-ROUTINE.                                     00001420
00143                                                                   00001430
00144      READ CHECK-QUE-IN                                            00001440
00145          AT END GO TO 9990-FINAL-CLOSE.                           00001450
00146                                                                   00001460
00147      ADD +1                      TO CHKQ-IN-CNT.                  00001470
00148                                                                   00001480
00149      MOVE SPACES                 TO CHECK-QUE-RCD.                00001490
00150      MOVE LOW-VALUES             TO CQ-CONTROL-BY-PAYEE.          00001500
00151                                                                   00001510
00152      MOVE 'CQ' TO CQ-RECORD-ID   OF CHECK-QUE-RCD.                00001520
00153                                                                   00001530
00154      MOVE CQ-COMPANY-CD          OF CHECK-QUE  TO                 00001540
00155           CQ-COMPANY-CD          OF CHECK-QUE-RCD                 00001550
00156           CQ-COMPANY-CD-A1       OF CHECK-QUE-RCD.                00001560
00157                                                                   00001570
00158      MOVE CQ-CONTROL-NUMBER      OF CHECK-QUE  TO                 00001580
00159           CQ-CONTROL-NUMBER      OF CHECK-QUE-RCD                 00001590
00160           CQ-CONTROL-NUMBER-A1   OF CHECK-QUE-RCD.                00001600
00161                                                                   00001610
00162      MOVE CQ-SEQUENCE-NUMBER     OF CHECK-QUE  TO                 00001620
00163           CQ-SEQUENCE-NUMBER     OF CHECK-QUE-RCD                 00001630
00164           CQ-SEQUENCE-NUMBER-A1  OF CHECK-QUE-RCD.                00001640
00165                                                                   00001650
00166      MOVE CQ-ENTRY-TYPE          OF CHECK-QUE  TO                 00001660
00167           CQ-ENTRY-TYPE          OF CHECK-QUE-RCD.                00001670
00168                                                                   00001680
00169      MOVE CQ-CARRIER             OF CHECK-QUE  TO                 00001690
00170           CQ-CARRIER             OF CHECK-QUE-RCD.                00001700
00171                                                                   00001710
00172      MOVE CQ-CLAIM-NO            OF CHECK-QUE  TO                 00001720
00173           CQ-CLAIM-NO            OF CHECK-QUE-RCD.                00001730
00174                                                                   00001740
00175      MOVE CQ-CERT-NO             OF CHECK-QUE  TO                 00001750
00176           CQ-CERT-NO             OF CHECK-QUE-RCD.                00001760
00177                                                                   00001770
00178      MOVE CQ-CLAIM-TYPE          OF CHECK-QUE  TO                 00001780
00179           CQ-CLAIM-TYPE          OF CHECK-QUE-RCD.                00001790
00180                                                                   00001800
00181      MOVE CQ-CLAIM-SUB-TYPE      OF CHECK-QUE  TO                 00001810
00182           CQ-CLAIM-SUB-TYPE      OF CHECK-QUE-RCD.                00001820
00183                                                                   00001830
00184      MOVE CQ-PMT-TRLR-SEQUENCE   OF CHECK-QUE  TO                 00001840
00185           CQ-PMT-TRLR-SEQUENCE   OF CHECK-QUE-RCD.                00001850
00186                                                                   00001860
00187      MOVE CQ-CHECK-NUMBER        OF CHECK-QUE  TO                 00001870
00188           CQ-CHECK-NUMBER        OF CHECK-QUE-RCD.                00001880
00189                                                                   00001890
00190      IF CQ-CHECK-AMOUNT OF CHECK-QUE NOT NUMERIC                  00001900
00191         MOVE +0 TO CQ-CHECK-AMOUNT OF CHECK-QUE.                  00001910
00192                                                                   00001920
00193      MOVE CQ-CHECK-AMOUNT        OF CHECK-QUE  TO                 00001930
00194           CQ-CHECK-AMOUNT        OF CHECK-QUE-RCD.                00001940
00195                                                                   00001950
00196      MOVE CQ-PAYMENT-TYPE        OF CHECK-QUE  TO                 00001960
00197           CQ-PAYMENT-TYPE        OF CHECK-QUE-RCD.                00001970
00198                                                                   00001980
00199      MOVE CQ-VOID-INDICATOR      OF CHECK-QUE  TO                 00001990
00200           CQ-VOID-INDICATOR      OF CHECK-QUE-RCD.                00002000
00201                                                                   00002010
00202      MOVE CQ-TIMES-PRINTED       OF CHECK-QUE  TO                 00002020
00203           CQ-TIMES-PRINTED       OF CHECK-QUE-RCD.                00002030
00204                                                                   00002040
00205      MOVE CQ-PRINT-AT-HHMM       OF CHECK-QUE  TO                 00002050
00206           CQ-PRINT-AT-HHMM       OF CHECK-QUE-RCD.                00002060
00207                                                                   00002070
00208      MOVE CQ-CHECK-BY-USER       OF CHECK-QUE  TO                 00002080
00209           CQ-CHECK-BY-USER       OF CHECK-QUE-RCD.                00002090
00210                                                                   00002100
00211      MOVE CQ-PRE-NUMBERING-SW    OF CHECK-QUE  TO                 00002110
00212           CQ-PRE-NUMBERING-SW    OF CHECK-QUE-RCD.                00002120
00213                                                                   00002130
00214      MOVE CQ-CHECK-WRITTEN-DT    OF CHECK-QUE  TO                 00002140
00215           CQ-CHECK-WRITTEN-DT    OF CHECK-QUE-RCD.                00002150
00216                                                                   00002160
00217      MOVE CQ-LAST-UPDATED-BY     OF CHECK-QUE  TO                 00002170
00218           CQ-LAST-UPDATED-BY     OF CHECK-QUE-RCD.                00002180
00219                                                                   00002190
00220      WRITE CHECK-QUE-RCD.                                         00002200
00221                                                                   00002210
00222      ADD +1                      TO CHKQ-OUT-CNT.                 00002220
00223                                                                   00002230
00224      GO TO 0200-CHECK-QUE-READ-ROUTINE.                           00002240
00225                                                                   00002250
00226  1000-EXIT.                                                       00002260
00227      EXIT.                                                        00002270
00228      EJECT                                                        00002280
00229 ******************************************************************00002290
00230 ***          E N D   O F   J O B   P R O C E S S I N G         ***00002300
00231 ******************************************************************00002310
00232                                                                   00002320
00233  9990-FINAL-CLOSE.                                                00002330
00234                                                                   00002340
00235      DISPLAY '**** CHECK QUE RCDS READ = ' CHKQ-IN-CNT.           00002350
00236      DISPLAY '**** CHECK RCDS  WRITTEN = ' CHKQ-OUT-CNT.          00002360
00237                                                                   00002370
00238      CLOSE CHECK-QUE-IN                                           00002380
00239            CHECK-QUE-OUT.                                         00002390
00240                                                                   00002400
00241      GO TO 9999-END-THE-JOB.                                      00002410
00242                                                                   00002420
00243      EJECT                                                        00002430
00244  ABEND-PGM.                                                       00002440
00245                                  COPY ELCABEND.                   00002450
00246  SKIP3                                                            00002460
00247  9999-END-THE-JOB.                                                00002470
00248                                                                   00002480
00249      GOBACK.                                                      00002490
