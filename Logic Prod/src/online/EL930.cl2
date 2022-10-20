00001  ID DIVISION.                                                     00000010
00002                                                                   00000020
00003  PROGRAM-ID.                 EL930.                               00000030
00004 *                            VMOD=2.001.                          00000031
00005                                                                   00000050
00006  AUTHOR.     LOGIC,INC.                                           00000060
00007              DALLAS, TEXAS.                                       00000070
00008                                                                   00000080
00009  DATE-COMPILED.                                                   00000090
00010  SECURITY.   *****************************************************00000100
00011              *                                                   *00000110
00012              *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *00000120
00013              *                                                   *00000130
00014              *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *00000140
00015              *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *00000150
00016              *   THE PRIOR WRITTEN PERMISSION OF LOGIC, INC.     *00000160
00017              *                                                   *00000170
00018              *****************************************************00000180
00019                                                                   00000190
00020  REMARKS.    TRANSACTION - EXI1 - NEW BUSINESS - DATA ENTRY.      00000200
00021                                                                   00000210
00022  ENVIRONMENT DIVISION.                                            00000220
00023                                                                   00000230
00024      EJECT                                                        00000240
00025  DATA DIVISION.                                                   00000250
00026  WORKING-STORAGE SECTION.                                         00000260
00027  77  FILLER  PIC X(32)  VALUE '********************************'. 00000270
00028  77  FILLER  PIC X(32)  VALUE '*    EL930 WORKING STORAGE     *'. 00000280
00029  77  FILLER  PIC X(32)  VALUE '************ V/M 2.001 *********'. 00000290
00030                                                                   00000300
00031                              COPY ELCSCTM.                        00000301
00032                                                                   00000320
00033                              COPY ELCSCRTY.                       00000321
00034                                                                   00000340
00035  01  STANDARD-AREAS.                                              00000350
00036      12  SC-ITEM                 PIC S9(4) COMP VALUE +1.         00000360
00037      12  GETMAIN-SPACE           PIC X       VALUE SPACE.         00000370
00038      12  MAP-NAME                PIC X(8)    VALUE 'EL930A  '.    00000380
00039      12  MAPSET-NAME             PIC X(8)    VALUE 'EL930S  '.    00000390
00040      12  SCREEN-NUMBER           PIC X(4)    VALUE '930A'.        00000400
00041      12  TRANS-ID                PIC X(4)    VALUE 'EXI1'.        00000410
00042      12  EDIT-TRANS              PIC X(4)    VALUE 'EXBT'.        00000420
00043      12  PASS-AREA-LEN           PIC S9(4)   COMP VALUE +16.      00000430
00044      12  THIS-PGM                PIC X(8)    VALUE 'EL930   '.    00000440
00045      12  PGM-NAME                PIC X(8).                        00000450
00046      12  TIME-IN                 PIC S9(7).                       00000460
00047      12  TIME-OUT-R  REDEFINES TIME-IN.                           00000470
00048          16  FILLER              PIC X.                           00000480
00049          16  TIME-OUT            PIC 99V99.                       00000490
00050          16  FILLER              PIC X(2).                        00000500
00051      12  XCTL-005                PIC X(8)    VALUE 'EL005   '.    00000510
00052      12  XCTL-010                PIC X(8)    VALUE 'EL010   '.    00000520
00053      12  XCTL-626                PIC X(8)    VALUE 'EL626   '.    00000530
00054      12  XCTL-9301               PIC X(8)    VALUE 'EL9301  '.    00000540
00055      12  XCTL-9302               PIC X(8)    VALUE 'EL9302  '.    00000550
00056      12  XCTL-633                PIC X(8)    VALUE 'EL633   '.    00000560
00057      12  LINK-001                PIC X(8)    VALUE 'EL001   '.    00000570
00058      12  LINK-004                PIC X(8)    VALUE 'EL004   '.    00000580
00059      12  LINK-CLDATCV            PIC X(8)    VALUE 'ELDATCV '.    00000590
00060      12  ELCNTL-FILE-ID          PIC X(8)    VALUE 'ELCNTL  '.    00000600
00061      12  ERPNDT-FILE-ID          PIC X(8)    VALUE 'ERPNDT  '.    00000610
00062      12  ELCERT-FILE-ID          PIC X(8)    VALUE 'ELCERT  '.    00000620
00063      12  ERNOTE-FILE-ID          PIC X(8)    VALUE 'ERNOTE  '.    00000630
00064      12  ERMAIL-FILE-ID          PIC X(8)    VALUE 'ERMAIL  '.    00000640
00065      12  ERPNDM-FILE-ID          PIC X(8)    VALUE 'ERPNDM  '.    00000650
00066      12  WS-CURRENT-BIN-DT       PIC XX      VALUE SPACES.        00000660
00067      12  WS-CURRENT-DT           PIC X(8)    VALUE SPACES.        00000670
00068      12  WS-SYNC-CNTR            PIC S9(3)   VALUE +0 COMP-3.     00000680
00069                                                                   00000690
00070      12  WS-RECORD-LENGTHS   COMP.                                00000700
00071         16  WS-ERNOTE-RECORD-LENGTH  PIC S9(4)   VALUE +500.      00000710
00072         16  WS-ERPNDT-RECORD-LENGTH  PIC S9(4)   VALUE +585.      00000720
00073         16  WS-ERMAIL-RECORD-LENGTH  PIC S9(4)   VALUE +250.      00000730
00074         16  WS-ERPNDM-RECORD-LENGTH  PIC S9(4)   VALUE +250.      00000740
00075         16  WS-ELCERT-RECORD-LENGTH  PIC S9(4)   VALUE +450.      00000750
00076         16  WS-ELCNTL-RECORD-LENGTH  PIC S9(4)   VALUE +504.      00000760
00077         16  WS-JOURNAL-RECORD-LENGTH PIC S9(4)   VALUE ZEROS.     00000770
00078                                                                   00000780
00079  01  BATCH-TO-PROCESS.                                            00000790
00080      05  EDIT-COMPANY-CD         PIC X.                           00000800
00081      05  EDIT-BATCH              PIC X(6).                        00000810
00082      05  EDIT-COMPANY-ID         PIC XXX.                         00000820
00083      05  EDIT-RESTART-BATCH      PIC X(6).                        00000830
00084                                                                   00000840
00085      EJECT                                                        00000850
00086                                                                   00000860
00087  01  ERROR-MESSAGES.                                              00000870
00088      12  ER-0008                 PIC X(4)    VALUE '0008'.        00000880
00089      12  ER-0023                 PIC X(4)    VALUE '0023'.        00000890
00090      12  ER-0029                 PIC X(4)    VALUE '0029'.        00000900
00091      12  ER-0070                 PIC X(4)    VALUE '0070'.        00000910
00092      12  ER-0194                 PIC X(4)    VALUE '0194'.        00000920
00093      12  ER-0195                 PIC X(4)    VALUE '0195'.        00000930
00094      12  ER-0196                 PIC X(4)    VALUE '0196'.        00000940
00095      12  ER-0197                 PIC X(4)    VALUE '0197'.        00000950
00096      12  ER-0340                 PIC X(4)    VALUE '0340'.        00000960
00097      12  ER-2201                 PIC X(4)    VALUE '2201'.        00000970
00098      12  ER-2208                 PIC X(4)    VALUE '2208'.        00000980
00099      12  ER-2209                 PIC X(4)    VALUE '2209'.        00000990
00100      12  ER-2210                 PIC X(4)    VALUE '2210'.        00001000
00101      12  ER-2211                 PIC X(4)    VALUE '2211'.        00001010
00102      12  ER-2212                 PIC X(4)    VALUE '2212'.        00001020
00103      12  ER-2213                 PIC X(4)    VALUE '2213'.        00001030
00104      12  ER-2214                 PIC X(4)    VALUE '2214'.        00001040
00105      12  ER-2215                 PIC X(4)    VALUE '2215'.        00001050
00106      12  ER-2216                 PIC X(4)    VALUE '2216'.        00001060
00107      12  ER-2229                 PIC X(4)    VALUE '2229'.        00001070
00108      12  ER-2242                 PIC X(4)    VALUE '2242'.        00001080
00109      12  ER-2248                 PIC X(4)    VALUE '2248'.        00001090
00110      12  ER-2370                 PIC X(4)    VALUE '2370'.        00001100
00111      12  ER-2371                 PIC X(4)    VALUE '2371'.        00001110
00112      12  ER-2402                 PIC X(4)    VALUE '2402'.        00001120
00113      12  ER-2422                 PIC X(4)    VALUE '2422'.        00001130
00114                                                                   00001140
00115      EJECT                                                        00001150
00116                                                                   00001160
00117  01  ACCESS-KEYS.                                                 00001170
00118      12  ELCNTL-KEY.                                              00001180
00119          16  CNTL-COMP-ID        PIC X(3)  VALUE SPACES.          00001190
00120          16  CNTL-REC-TYPE       PIC X     VALUE SPACES.          00001200
00121          16  CNTL-ACCESS.                                         00001210
00122              20  CNTL-STATE      PIC XX    VALUE SPACES.          00001220
00123              20  FILLER          PIC X     VALUE SPACES.          00001230
00124              20  CNTL-CARRIER    PIC X     VALUE SPACES.          00001240
00125          16  CNTL-SEQ            PIC S9(4) VALUE +0 COMP.         00001250
00126                                                                   00001260
00127      12  ERPNDT-KEY.                                              00001270
00128          16  PNDT-COMP-CD        PIC X     VALUE SPACE.           00001280
00129          16  PNDT-ENTRY-BATCH    PIC X(6)  VALUE SPACES.          00001290
00130          16  PNDT-BATCH-SEQ      PIC S9(4) VALUE +0 COMP.         00001300
00131          16  PNDT-BATCH-CHG-SEQ  PIC S9(4) VALUE +0 COMP.         00001310
00132                                                                   00001320
00133      12  ELCERT-KEY.                                              00001330
00134          16  CERT-COMPANY-CD     PIC X     VALUE SPACES.          00001340
00135          16  CERT-CARRIER        PIC X     VALUE SPACES.          00001350
00136          16  CERT-GROUPING       PIC X(6)  VALUE SPACES.          00001360
00137          16  CERT-STATE          PIC XX    VALUE SPACES.          00001370
00138          16  FILLER              PIC X(23) VALUE SPACES.          00001380
00139                                                                   00001390
00140  01  FILLER.                                                      00001400
00141      12  WS-DEEDIT-FIELD         PIC S9(8)V99.                    00001410
00142      12  WS-DT-DEEDIT-FIELD REDEFINES                             00001420
00143          WS-DEEDIT-FIELD         PIC X(10).                       00001430
00144      12  QUESTION-MARKS          PIC X(6)  VALUE '??????'.        00001440
00145      12  WS-BATCH-NO             PIC 9(6)  VALUE ZEROS.           00001450
00146      12  WS-OBAL                 PIC S9(8)V99  VALUE ZEROS.       00001460
00147      12  WS-OCNT                 PIC S9(5)     VALUE ZEROS.       00001470
00148      12  WS-DELETE-CNT           PIC S9(5)     VALUE ZEROS.       00001480
00149      12  WS-PF1-SW               PIC X         VALUE SPACES.      00001490
00150          88  WS-PF1                            VALUE 'Y'.         00001500
00151                                                                   00001510
00152      12  WS-SAV-PNDT-KEY.                                         00001520
00153          16  WS-SAV-COMP-CD        PIC X.                         00001530
00154          16  WS-SAV-ENTRY-BATCH    PIC X(6).                      00001540
00155          16  WS-SAV-BATCH-SEQ      PIC S9(4) COMP.                00001550
00156          16  WS-SAV-BATCH-CHG-SEQ  PIC S9(4) COMP.                00001560
00157                                                                   00001570
00158      12  DATE-TEST-AREA          PIC 9(6).                        00001580
00159      12  DATE-TEST-AREA-R  REDEFINES DATE-TEST-AREA.              00001590
00160          16  DATE-TEST-MM        PIC 99.                          00001600
00161          16  DATE-TEST-DD        PIC 99.                          00001610
00162          16  DATE-TEST-YY        PIC 99.                          00001620
00163      12  DIVIDE-RESULT           PIC 99.                          00001630
00164      12  DIVIDE-REMAINDER        PIC 9.                           00001640
00165      12  WS-CERT-NOTE-SW         PIC X   VALUE ' '.               00001650
00166          88 CERT-NOTES-ARE-PRESENT  VALUE 'Y'.                    00001660
00167      12  WS-CERT-ADDRESS-SW      PIC X   VALUE ' '.               00001670
00168          88 CERT-ADDRESS-PRESENT     VALUE 'Y'.                   00001680
00169      12  WS-PRM-HEADER.                                           00001690
00170          16  WS-PRM-OVERRIDE     PIC XX    VALUE SPACES.          00001700
00171          16  FILLER              PIC X(8)  VALUE '-PREMIUM'.      00001710
00172      12  WS-REFUND-HEADER.                                        00001720
00173          16  WS-REFUND-OVERRIDE  PIC XX    VALUE SPACES.          00001730
00174          16  FILLER              PIC X(7)  VALUE '-REFUND'.       00001740
00175                                                                   00001750
00176      EJECT                                                        00001760
00177                                                                   00001770
00178                              COPY ELCDATE.                        00001771
00179                                                                   00001790
00180      EJECT                                                        00001800
00181                              COPY ELCLOGOF.                       00001801
00182                                                                   00001820
00183      EJECT                                                        00001830
00184                              COPY ELCATTR.                        00001831
00185                                                                   00001850
00186      EJECT                                                        00001860
00187                                    COPY ELCEMIB.                  00001861
00188                                                                   00001880
00189      EJECT                                                        00001890
00190                              COPY ELCINTF.                        00001891
00191      COPY ELC930PI.                                               00001910
00192                                                                   00001920
00193      EJECT                                                        00001930
00194                              COPY ELCJPFX.                        00001931
00195                              PIC X(585).                          00001950
00196                                                                   00001960
00197      EJECT                                                        00001970
00198                              COPY ELCAID.                         00001971
00199  01  FILLER    REDEFINES DFHAID.                                  00001990
00200      12  FILLER              PIC X(8).                            00002000
00201      12  PF-VALUES           PIC X       OCCURS 2.                00002010
00202                                                                   00002020
00203      EJECT                                                        00002030
00204                              COPY EL930S.                         00002031
00205                                                                   00002050
00206      EJECT                                                        00002060
00207  LINKAGE SECTION.                                                 00002070
00208  01  DFHCOMMAREA             PIC X(1024).                         00002080
00209                                                                   00002090
00210      EJECT                                                        00002100
00211  01  PARMLIST      COMP.                                          00002110
00212      02  FILLER              PIC S9(8).                           00002120
00213      02  ELCNTL-POINTER      PIC S9(8).                           00002130
00214      02  ERPNDT-POINTER      PIC S9(8).                           00002140
00215      02  ELCERT-POINTER      PIC S9(8).                           00002150
00216      02  ERNOTE-POINTER      PIC S9(8).                           00002160
00217      02  ERMAIL-POINTER      PIC S9(8).                           00002170
00218      02  ERPNDM-POINTER      PIC S9(8).                           00002180
00219                                                                   00002190
00220                              COPY ELCCNTL.                        00002191
00221      EJECT                                                        00002210
00222                              COPY ERCPNDB.                        00002211
00223      EJECT                                                        00002230
00224                              COPY ELCCERT.                        00002231
00225      EJECT                                                        00002250
00226                              COPY ERCNOTE.                        00002251
00227      EJECT                                                        00002270
00228                              COPY ERCMAIL.                        00002271
00229      EJECT                                                        00002290
00230                              COPY ERCPNDM.                        00002291
00231      EJECT                                                        00002310
00232                                                                   00002320
00233  PROCEDURE DIVISION.                                              00002330
00234      SERVICE RELOAD PARMLIST.                                     00002340
00235      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.      00002350
00236      MOVE 2                      TO EMI-NUMBER-OF-LINES.          00002360
00237      IF EIBCALEN = 0                                              00002370
00238          GO TO 8800-UNAUTHORIZED-ACCESS.                          00002380
00239      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              00002390
00240      MOVE '5'                    TO DC-OPTION-CODE.               00002400
00241      MOVE LINK-CLDATCV           TO PGM-NAME.                     00002410
00242                                                                   00002420
00243      EXEC CICS LINK                                               00002430
00244          PROGRAM(PGM-NAME)                                        00002440
00245          COMMAREA(DATE-CONVERSION-DATA)                           00002450
00246          LENGTH(DC-COMM-LENGTH)                                   00002460
00247          END-EXEC.                                                00002470
00248      MOVE DC-BIN-DATE-1          TO WS-CURRENT-BIN-DT.            00002480
00249      MOVE DC-GREG-DATE-1-EDIT    TO WS-CURRENT-DT.                00002490
00250                                                                   00002500
00251      MOVE PI-CALLING-PROGRAM TO PI-SAVE-CALLING-PGM.              00002510
00252      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         00002520
00253          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   00002530
00254              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6      00002540
00255              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5      00002550
00256              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4      00002560
00257              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3      00002570
00258              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2      00002580
00259              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1      00002590
00260              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM    00002600
00261              MOVE THIS-PGM TO PI-CALLING-PROGRAM                  00002610
00262          ELSE                                                     00002620
00263              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM      00002630
00264              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM    00002640
00265              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1      00002650
00266              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2      00002660
00267              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3      00002670
00268              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4      00002680
00269              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5      00002690
00270              MOVE SPACES               TO PI-SAVED-PROGRAM-6.     00002700
00271                                                                   00002710
00272      MOVE LOW-VALUES             TO EL930AI.                      00002720
00273                                                                   00002730
00274      IF PI-SAVE-CALLING-PGM = XCTL-9302                           00002740
00275          GO TO 4000-SHOW-TOTALS.                                  00002750
00276      IF PI-SAVE-CALLING-PGM = XCTL-9301                           00002760
00277          MOVE 'N'                TO PI-EL930-FIRST-TIME-SW        00002770
00278          IF PI-ISS-CNT-ENTERED = ZEROS                            00002780
00279            AND PI-CAN-CNT-ENTERED = ZEROS                         00002790
00280              MOVE 'Y'            TO PI-VERIFY-DELETE-SW           00002800
00281              GO TO 3000-DELETE-ENTERED-BATCH                      00002810
00282          ELSE                                                     00002820
00283              GO TO 4000-SHOW-TOTALS.                              00002830
00284      IF EIBTRNID NOT = TRANS-ID                                   00002840
00285          MOVE ZEROS              TO PI-LAST-SEQ-NO-ADDED          00002850
00286          MOVE QUESTION-MARKS     TO BATCHI                        00002860
00287          MOVE AL-UANON           TO BATCHA                        00002870
00288          GO TO 8100-SEND-INITIAL-MAP.                             00002880
00289                                                                   00002890
00290      EXEC CICS HANDLE CONDITION                                   00002900
00291          PGMIDERR  (9600-PGMID-ERROR)                             00002910
00292          ERROR     (9990-ABEND)                                   00002920
00293          END-EXEC.                                                00002930
00294                                                                   00002940
00295      IF EIBAID = DFHCLEAR                                         00002950
00296          GO TO 9400-CLEAR.                                        00002960
00297                                                                   00002970
00298      IF PI-PROCESSOR-ID = 'LGXX'                                  00002980
00299          GO TO 0200-RECEIVE.                                      00002990
00300                                                                   00003000
00301      EXEC CICS READQ TS                                           00003010
00302          QUEUE  (PI-SECURITY-TEMP-STORE-ID)                       00003020
00303          INTO   (SECURITY-CONTROL)                                00003030
00304          LENGTH (SC-COMM-LENGTH)                                  00003040
00305          ITEM   (SC-ITEM)                                         00003050
00306      END-EXEC.                                                    00003060
00307                                                                   00003070
00308      MOVE SC-CREDIT-DISPLAY (11)  TO PI-DISPLAY-CAP.              00003080
00309      MOVE SC-CREDIT-UPDATE  (11)  TO PI-MODIFY-CAP.               00003090
00310                                                                   00003100
00311      IF NOT DISPLAY-CAP                                           00003110
00312          MOVE 'READ'          TO SM-READ                          00003120
00313          PERFORM 9995-SECURITY-VIOLATION                          00003130
00314          MOVE ER-0070         TO  EMI-ERROR                       00003140
00315          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 00003150
00316          GO TO 8100-SEND-INITIAL-MAP.                             00003160
00317                                                                   00003170
00318      EJECT                                                        00003180
00319  0200-RECEIVE.                                                    00003190
00320      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       00003200
00321          MOVE ER-0008            TO EMI-ERROR                     00003210
00322          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 00003220
00323          MOVE -1                 TO MAINTL                        00003230
00324          GO TO 8200-SEND-DATAONLY.                                00003240
00325                                                                   00003250
00326      EXEC CICS RECEIVE                                            00003260
00327          MAP      (MAP-NAME)                                      00003270
00328          MAPSET   (MAPSET-NAME)                                   00003280
00329          INTO     (EL930AI)                                       00003290
00330          END-EXEC.                                                00003300
00331                                                                   00003310
00332      IF PFENTERL = 0                                              00003320
00333          GO TO 0300-CHECK-PFKEYS.                                 00003330
00334      IF (PFENTERI NUMERIC)                                        00003340
00335        AND (PFENTERI GREATER THAN 0 AND LESS THAN 25)             00003350
00336          MOVE PF-VALUES (PFENTERI) TO EIBAID                      00003360
00337      ELSE                                                         00003370
00338          MOVE ER-0029            TO EMI-ERROR                     00003380
00339          GO TO 0320-INPUT-ERROR.                                  00003390
00340                                                                   00003400
00341  0300-CHECK-PFKEYS.                                               00003410
00342      IF EIBAID = DFHPF23                                          00003420
00343          GO TO 8810-PF23.                                         00003430
00344      IF EIBAID = DFHPF24                                          00003440
00345          GO TO 9200-RETURN-MAIN-MENU.                             00003450
00346      IF EIBAID = DFHPF12                                          00003460
00347          GO TO 9500-PF12.                                         00003470
00348                                                                   00003480
00349      IF EIBAID = DFHPF1                                           00003490
00350         IF PI-DATA-UPDATED                                        00003500
00351              MOVE 'Y'            TO WS-PF1-SW                     00003510
00352              MOVE SPACE          TO PI-NB-MONTH-END-DT            00003520
00353              PERFORM 2000-WRITE-BATCH-TOTAL-REC THRU 2990-EXIT    00003530
00354              MOVE SPACES TO BATCH-TO-PROCESS                      00003540
00355              MOVE PI-COMPANY-ID  TO EDIT-COMPANY-ID               00003550
00356              MOVE PI-COMPANY-CD  TO EDIT-COMPANY-CD               00003560
00357              MOVE PI-SAV-ENTRY-BATCH TO EDIT-BATCH                00003570
00358              EXEC CICS START                                      00003580
00359                 TRANSID       (EDIT-TRANS)                        00003590
00360                 FROM          (BATCH-TO-PROCESS)                  00003600
00361                 LENGTH        (PASS-AREA-LEN)                     00003610
00362                 END-EXEC                                          00003620
00363              MOVE SPACES         TO PI-UPDATE-SW                  00003630
00364                                     PI-KEYED-SWITCHES             00003640
00365              MOVE ZEROS          TO PI-ISS-CNT-ENTERED            00003650
00366              MOVE ZEROS          TO PI-CAN-CNT-ENTERED            00003660
00367              MOVE ZEROS          TO EMI-ERROR                     00003670
00368              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             00003680
00369              MOVE LOW-VALUES     TO EL930AO                       00003690
00370              MOVE QUESTION-MARKS TO BATCHI                        00003700
00371              MOVE AL-UANON       TO BATCHA                        00003710
00372              MOVE -1             TO MAINTL                        00003720
00373              GO TO 8100-SEND-INITIAL-MAP                          00003730
00374          ELSE                                                     00003740
00375              IF  MAINTI = 'N'                                     00003750
00376                  MOVE -1             TO MAINTL                    00003760
00377                  MOVE ER-2211        TO EMI-ERROR                 00003770
00378                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         00003780
00379                  GO TO 8200-SEND-DATAONLY.                        00003790
00380                                                                   00003800
00381                                                                   00003810
00382      IF EIBAID = DFHPF1                                           00003820
00383              MOVE SPACE          TO PI-NB-MONTH-END-DT            00003830
00384              MOVE SPACES TO BATCH-TO-PROCESS                      00003840
00385              MOVE PI-COMPANY-ID  TO EDIT-COMPANY-ID               00003850
00386              MOVE PI-COMPANY-CD  TO EDIT-COMPANY-CD               00003860
00387              MOVE PI-SAV-ENTRY-BATCH TO EDIT-BATCH                00003870
00388              EXEC CICS START                                      00003880
00389                 TRANSID       (EDIT-TRANS)                        00003890
00390                 FROM          (BATCH-TO-PROCESS)                  00003900
00391                 LENGTH        (PASS-AREA-LEN)                     00003910
00392                 END-EXEC                                          00003920
00393              MOVE SPACE          TO PI-UPDATE-SW                  00003930
00394                                     PI-KEYED-SWITCHES             00003940
00395              MOVE ZEROS          TO PI-ISS-CNT-ENTERED            00003950
00396              MOVE ZEROS          TO PI-CAN-CNT-ENTERED            00003960
00397              MOVE ZEROS          TO EMI-ERROR                     00003970
00398              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             00003980
00399              MOVE LOW-VALUES     TO EL930AO                       00003990
00400              MOVE QUESTION-MARKS TO BATCHI                        00004000
00401              MOVE AL-UANON       TO BATCHA                        00004010
00402              MOVE -1             TO MAINTL                        00004020
00403              GO TO 8100-SEND-INITIAL-MAP.                         00004030
00404                                                                   00004040
00405                                                                   00004050
00406      IF EIBAID = DFHPF3                                           00004060
00407         IF BATCHL GREATER THAN ZEROS                              00004070
00408            MOVE AL-UNNON         TO BATCHA                        00004080
00409            GO TO 6000-BROWSE-BATCH-HEADERS                        00004090
00410         ELSE                                                      00004100
00411            GO TO 6000-BROWSE-BATCH-HEADERS.                       00004110
00412                                                                   00004120
00413      IF EIBAID = DFHPF4                                           00004130
00414          IF NOT PI-EL930-FIRST-TIME                               00004140
00415              MOVE XCTL-9302      TO PGM-NAME                      00004150
00416              GO TO 9300-XCTL                                      00004160
00417          ELSE                                                     00004170
00418              PERFORM 0400-PRIME-BATCH-TOTALS THRU 0490-EXIT       00004180
00419              IF EMI-ERROR = ZEROS                                 00004190
00420                  MOVE 'N'        TO PI-EL930-FIRST-TIME-SW        00004200
00421                  MOVE XCTL-9302  TO PGM-NAME                      00004210
00422                  GO TO 9300-XCTL                                  00004220
00423              ELSE                                                 00004230
00424                  GO TO 8200-SEND-DATAONLY.                        00004240
00425                                                                   00004250
00426      IF EIBAID = DFHPF2                                           00004260
00427          MOVE  AL-UANON          TO  BATCHA                       00004270
00428          MOVE  BATCHI TO PI-SAV-ENTRY-BATCH                       00004280
00429          GO TO 3000-DELETE-ENTERED-BATCH.                         00004290
00430                                                                   00004300
00431      IF  MAINTI = 'S'                                             00004310
00432          MOVE  MAINTI            TO  PI-MAINT-FUNC                00004320
00433          PERFORM 0400-PRIME-BATCH-TOTALS  THRU  0490-EXIT         00004330
00434              IF EMI-ERROR = ZEROS                                 00004340
00435                  MOVE 'N'        TO PI-EL930-FIRST-TIME-SW        00004350
00436                  GO TO 4000-SHOW-TOTALS                           00004360
00437              ELSE                                                 00004370
00438                  GO TO 8200-SEND-DATAONLY.                        00004380
00439                                                                   00004390
00440      IF EIBAID = DFHENTER                                         00004400
00441          GO TO 0330-EDIT-DATA.                                    00004410
00442  0320-INPUT-ERROR.                                                00004420
00443      MOVE ER-0029                TO EMI-ERROR.                    00004430
00444      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    00004440
00445      MOVE AL-UNBON               TO PFENTERA.                     00004450
00446      IF PFENTERL = 0                                              00004460
00447          MOVE -1                 TO MAINTL                        00004470
00448      ELSE                                                         00004480
00449          MOVE -1                 TO PFENTERL.                     00004490
00450      GO TO 8200-SEND-DATAONLY.                                    00004500
00451                                                                   00004510
00452      EJECT                                                        00004520
00453  0330-EDIT-DATA.                                                  00004530
00454                                                                   00004540
00455      IF MODIFY-CAP                                                00004550
00456          NEXT SENTENCE                                            00004560
00457        ELSE                                                       00004570
00458          IF MAINTI NOT = 'S'                                      00004580
00459          MOVE 'UPDATE'       TO SM-READ                           00004590
00460          PERFORM 9995-SECURITY-VIOLATION                          00004600
00461          MOVE ER-0070        TO EMI-ERROR                         00004610
00462          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 00004620
00463          GO TO 8100-SEND-INITIAL-MAP.                             00004630
00464                                                                   00004640
00465      MOVE SPACE                  TO  PI-NB-MONTH-END-DT.          00004650
00466                                                                   00004660
00467      PERFORM 1250-READ-COMPANY-REC THRU 1250-EXIT.                00004670
00468      IF MAINTI = 'N' OR 'C' OR 'B'                                00004680
00469          NEXT SENTENCE                                            00004690
00470      ELSE                                                         00004700
00471          MOVE ER-0023            TO EMI-ERROR                     00004710
00472          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 00004720
00473          MOVE -1                 TO MAINTL                        00004730
00474          MOVE AL-UABON           TO MAINTA                        00004740
00475          GO TO 8200-SEND-DATAONLY.                                00004750
00476                                                                   00004760
00477      IF  NOT EMI-NO-ERRORS                                        00004770
00478          GO TO 8200-SEND-DATAONLY.                                00004780
00479                                                                   00004790
00480      MOVE MAINTI                 TO PI-MAINT-FUNC.                00004800
00481      MOVE AL-UANON               TO MAINTA.                       00004810
00482                                                                   00004820
00483      MOVE PI-COMPANY-CD          TO PI-SAV-COMP-CD.               00004830
00484      MOVE ZEROS                  TO PI-SAV-BATCH-SEQ              00004840
00485                                     PI-SAV-BATCH-CHG-SEQ.         00004850
00486      MOVE SPACES                 TO PI-SAV-CERT-EFF-DT            00004860
00487                                     PI-SAV-CERT-NO.               00004870
00488                                                                   00004880
00489                                                                   00004890
00490      IF BATCHL = ZEROS                                            00004900
00491          MOVE -1                 TO BATCHL                        00004910
00492          MOVE AL-UNBON           TO BATCHA                        00004920
00493          MOVE ER-2201            TO EMI-ERROR                     00004930
00494          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 00004940
00495      ELSE                                                         00004950
00496          IF BATCHI = QUESTION-MARKS OR SPACES                     00004960
00497              IF MAINTI = 'N'                                      00004970
00498                  PERFORM 1300-GET-BATCH-NO THRU 1300-EXIT         00004980
00499              ELSE                                                 00004990
00500                  MOVE -1         TO BATCHL                        00005000
00501                  MOVE AL-UNBON   TO BATCHA                        00005010
00502                  MOVE ER-2201    TO EMI-ERROR                     00005020
00503                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         00005030
00504          ELSE                                                     00005040
00505              IF BATCHI EQUAL PI-SAV-ENTRY-BATCH                   00005050
00506                  MOVE AL-UANON   TO BATCHA                        00005060
00507              ELSE                                                 00005070
00508                  MOVE BATCHI TO PI-SAV-ENTRY-BATCH                00005080
00509                  IF MAINTI = 'C' OR 'B'                           00005090
00510                      MOVE AL-UANON TO BATCHA                      00005100
00511                      MOVE ZEROS  TO ELFISSL  ELFCANL              00005110
00512                                     EAHISSL  EAHCANL              00005120
00513                                     EISSCNTL ECANCNTL             00005130
00514                                     PI-LF-ISS-REMITTED            00005140
00515                                     PI-LF-CAN-REMITTED            00005150
00516                                     PI-AH-ISS-REMITTED            00005160
00517                                     PI-AH-CAN-REMITTED            00005170
00518                                     PI-ISS-CNT-REMITTED           00005180
00519                                     PI-CAN-CNT-REMITTED.          00005190
00520                                                                   00005200
00521  0340-CHECK-TOTALS.                                               00005210
00522                                                                   00005220
00523 *    ********************************************************     00005230
00524 *    *         THE FOLLOWING FIELDS ARE NOT REQUIRED        *     00005240
00525 *    *         IF ENTERED THE ONLY REQUIREMENT IS NUMERIC   *     00005250
00526 *    ********************************************************     00005260
00527                                                                   00005270
00528      IF ELFISSL GREATER THAN ZEROS                                00005280
00529          MOVE ELFISSI            TO WS-DEEDIT-FIELD               00005290
00530          PERFORM 8600-DEEDIT THRU 8600-EXIT                       00005300
00531          MOVE WS-DEEDIT-FIELD    TO PI-LF-ISS-REMITTED            00005310
00532          MOVE AL-UNNON           TO ELFISSA                       00005320
00533      ELSE                                                         00005330
00534          MOVE ZEROS              TO PI-LF-ISS-REMITTED.           00005340
00535                                                                   00005350
00536      IF ELFCANL GREATER THAN ZEROS                                00005360
00537          MOVE ELFCANI            TO WS-DEEDIT-FIELD               00005370
00538          PERFORM 8600-DEEDIT THRU 8600-EXIT                       00005380
00539          MOVE WS-DEEDIT-FIELD    TO PI-LF-CAN-REMITTED            00005390
00540          MOVE AL-UNNON           TO ELFCANA                       00005400
00541      ELSE                                                         00005410
00542          MOVE ZEROS              TO PI-LF-CAN-REMITTED.           00005420
00543                                                                   00005430
00544      IF EAHISSL GREATER THAN ZEROS                                00005440
00545          MOVE EAHISSI            TO WS-DEEDIT-FIELD               00005450
00546          PERFORM 8600-DEEDIT THRU 8600-EXIT                       00005460
00547          MOVE WS-DEEDIT-FIELD    TO PI-AH-ISS-REMITTED            00005470
00548          MOVE AL-UNNON           TO EAHISSA                       00005480
00549      ELSE                                                         00005490
00550          MOVE ZEROS              TO PI-AH-ISS-REMITTED.           00005500
00551                                                                   00005510
00552      IF EAHCANL GREATER THAN ZEROS                                00005520
00553          MOVE EAHCANI            TO WS-DEEDIT-FIELD               00005530
00554          PERFORM 8600-DEEDIT THRU 8600-EXIT                       00005540
00555          MOVE WS-DEEDIT-FIELD    TO PI-AH-CAN-REMITTED            00005550
00556          MOVE AL-UNNON           TO EAHCANA                       00005560
00557      ELSE                                                         00005570
00558          MOVE ZEROS              TO PI-AH-CAN-REMITTED.           00005580
00559                                                                   00005590
00560      IF EISSCNTL GREATER THAN ZEROS                               00005600
00561          MOVE AL-UNNON           TO EISSCNTA                      00005610
00562          EXEC CICS BIF DEEDIT                                     00005620
00563              FIELD(EISSCNTI)                                      00005630
00564              LENGTH(06)                                           00005640
00565              END-EXEC                                             00005650
00566          MOVE EISSCNTI           TO PI-ISS-CNT-REMITTED           00005660
00567      ELSE                                                         00005670
00568          MOVE ZEROS              TO PI-ISS-CNT-REMITTED.          00005680
00569                                                                   00005690
00570      IF ECANCNTL GREATER THAN ZEROS                               00005700
00571          MOVE AL-UNNON           TO ECANCNTA                      00005710
00572          EXEC CICS BIF DEEDIT                                     00005720
00573              FIELD(ECANCNTI)                                      00005730
00574              LENGTH(06)                                           00005740
00575              END-EXEC                                             00005750
00576          MOVE ECANCNTI           TO PI-CAN-CNT-REMITTED           00005760
00577      ELSE                                                         00005770
00578          MOVE ZEROS              TO PI-CAN-CNT-REMITTED.          00005780
00579                                                                   00005790
00580 ******************************************************************00005800
00581 *                                                                *00005810
00582 *               EDIT FOR NEW MONTH-END-DATE                      *00005820
00583 *                        04/13/84                                *00005830
00584 *                                                                *00005840
00585 ******************************************************************00005850
00586                                                                   00005860
00587      IF MNTHNDTL GREATER THAN ZEROS NEXT SENTENCE                 00005870
00588        ELSE                                                       00005880
00589         GO TO 0340-ERROR-CHECK.                                   00005890
00590                                                                   00005900
00591      MOVE MNTHNDTI               TO WS-DT-DEEDIT-FIELD.           00005910
00592      PERFORM 8600-DEEDIT THRU 8600-EXIT.                          00005920
00593      MOVE WS-DT-DEEDIT-FIELD     TO DC-GREG-DATE-1-MDY.           00005930
00594      MOVE '4' TO DC-OPTION-CODE.                                  00005940
00595      MOVE LINK-CLDATCV TO PGM-NAME.                               00005950
00596      EXEC CICS LINK                                               00005960
00597           PROGRAM(PGM-NAME)                                       00005970
00598           COMMAREA(DATE-CONVERSION-DATA)                          00005980
00599           LENGTH(DC-COMM-LENGTH)                                  00005990
00600           END-EXEC.                                               00006000
00601      IF DATE-CONVERSION-ERROR                                     00006010
00602         GO TO 0340-DAY-ERROR.                                     00006020
00603      MOVE DC-GREG-DATE-1-EDIT    TO MNTHNDTO.                     00006030
00604      MOVE DC-BIN-DATE-1          TO PI-NB-MONTH-END-DT.           00006040
00605      MOVE DC-GREG-DATE-1-MDY     TO DATE-TEST-AREA.               00006050
00606      IF DATE-TEST-MM = 4 OR 6 OR 9 OR 11                          00006060
00607         IF DATE-TEST-DD  NOT = 30                                 00006070
00608            GO TO 0340-DAY-ERROR                                   00006080
00609         ELSE                                                      00006090
00610            GO TO 0340-ERROR-CHECK.                                00006100
00611                                                                   00006110
00612      IF DATE-TEST-MM = 1 OR 3 OR 5 OR 7 OR 8 OR 10 OR 12          00006120
00613         IF DATE-TEST-DD  NOT = 31                                 00006130
00614            GO TO 0340-DAY-ERROR                                   00006140
00615         ELSE                                                      00006150
00616            GO TO 0340-ERROR-CHECK.                                00006160
00617      DIVIDE DATE-TEST-YY  BY 4 GIVING DIVIDE-RESULT               00006170
00618                                REMAINDER DIVIDE-REMAINDER         00006180
00619      IF (DATE-TEST-YY = 0) OR (DIVIDE-REMAINDER NOT = 0)          00006190
00620         IF DATE-TEST-DD  NOT = 28                                 00006200
00621            GO TO 0340-DAY-ERROR                                   00006210
00622                                                                   00006220
00623         ELSE                                                      00006230
00624            GO TO 0340-ERROR-CHECK                                 00006240
00625      ELSE                                                         00006250
00626         IF DATE-TEST-DD = 29                                      00006260
00627            GO TO 0340-ERROR-CHECK.                                00006270
00628                                                                   00006280
00629  0340-DAY-ERROR.                                                  00006290
00630      MOVE -1 TO MNTHNDTL.                                         00006300
00631      MOVE AL-UABON TO MNTHNDTA.                                   00006310
00632      MOVE ER-0340 TO EMI-ERROR.                                   00006320
00633      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    00006330
00634                                                                   00006340
00635                                                                   00006350
00636  0340-ERROR-CHECK.                                                00006360
00637      IF NOT EMI-NO-ERRORS                                         00006370
00638          GO TO 8200-SEND-DATAONLY.                                00006380
00639      IF PI-DELETE-IS-OK                                           00006390
00640          MOVE SPACE              TO PI-VERIFY-DELETE-SW.          00006400
00641      IF MAINTI = 'N'                                              00006410
00642        AND PI-DATA-UPDATED                                        00006420
00643          MOVE 'C'                TO PI-MAINT-FUNC                 00006430
00644          GO TO 0360-XCTL-EL9301.                                  00006440
00645                                                                   00006450
00646      EXEC CICS HANDLE CONDITION                                   00006460
00647                NOTFND  (0350-NO-RECORDS)                          00006470
00648                ENDFILE (0350-NO-RECORDS)                          00006480
00649                NOTOPEN (7000-PNDT-FILE-NOTOPEN)                   00006490
00650                END-EXEC.                                          00006500
00651      EXEC CICS READ                                               00006510
00652          SET (ERPNDT-POINTER)                                     00006520
00653          DATASET (ERPNDT-FILE-ID)                                 00006530
00654          RIDFLD (PI-SAV-ENDING-ERPNDT-KEY)                        00006540
00655          GTEQ                                                     00006550
00656          END-EXEC.                                                00006560
00657      SERVICE RELOAD PENDING-BUSINESS.                             00006570
00658      IF PI-SAV-COMP-CD = PB-COMPANY-CD                            00006580
00659        AND PI-SAV-ENTRY-BATCH = PB-ENTRY-BATCH                    00006590
00660          IF MAINTI = 'N'                                          00006600
00661              MOVE ER-2229        TO EMI-ERROR                     00006610
00662              MOVE -1             TO MAINTL                        00006620
00663              MOVE AL-UABON       TO MAINTA                        00006630
00664              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             00006640
00665              GO TO 8200-SEND-DATAONLY                             00006650
00666          ELSE                                                     00006660
00667              NEXT SENTENCE                                        00006670
00668      ELSE                                                         00006680
00669          GO TO 0350-NO-RECORDS.                                   00006690
00670                                                                   00006700
00671                                                                   00006710
00672      IF PB-BILLED-DT NOT = LOW-VALUES                             00006720
00673          MOVE ER-2402            TO EMI-ERROR                     00006730
00674          MOVE -1                 TO BATCHL                        00006740
00675          MOVE AL-UABON           TO BATCHA                        00006750
00676          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 00006760
00677          GO TO 8200-SEND-DATAONLY.                                00006770
00678                                                                   00006780
00679      IF PI-SAV-COMP-CD = PB-COMPANY-CD                            00006790
00680        AND PI-SAV-ENTRY-BATCH = PB-ENTRY-BATCH                    00006800
00681        AND MAINTI NOT = 'N'                                       00006810
00682          MOVE PB-CARRIER         TO PI-SAV-CARRIER                00006820
00683          MOVE PB-GROUPING        TO PI-SAV-GROUPING               00006830
00684          MOVE PB-STATE           TO PI-SAV-STATE                  00006840
00685          MOVE PB-ACCOUNT         TO PI-SAV-ACCOUNT                00006850
00686          PERFORM 2000-WRITE-BATCH-TOTAL-REC THRU 2990-EXIT        00006860
00687          MOVE 'EL930B  '         TO PI-MAP-NAME.                  00006870
00688          GO TO 0360-XCTL-EL9301.                                  00006880
00689                                                                   00006890
00690  0350-NO-RECORDS.                                                 00006900
00691      IF MAINTI = 'N'                                              00006910
00692          MOVE SPACE              TO PI-ISSUE-ADDED-SW             00006920
00693          MOVE ZEROS              TO PI-LF-ISS-ENTERED             00006930
00694                                     PI-LF-CAN-ENTERED             00006940
00695                                     PI-AH-ISS-ENTERED             00006950
00696                                     PI-AH-CAN-ENTERED             00006960
00697                                     PI-ISS-CNT-ENTERED            00006970
00698                                     PI-CAN-CNT-ENTERED            00006980
00699          PERFORM 2000-WRITE-BATCH-TOTAL-REC THRU 2990-EXIT        00006990
00700          IF PI-LF-ISS-REMITTED = ZEROS                            00007000
00701            AND PI-AH-ISS-REMITTED = ZEROS                         00007010
00702            AND PI-LF-CAN-REMITTED = ZEROS                         00007020
00703            AND PI-AH-CAN-REMITTED = ZEROS                         00007030
00704            AND PI-ISS-CNT-REMITTED = ZEROS                        00007040
00705            AND PI-CAN-CNT-REMITTED = ZEROS                        00007050
00706              MOVE 'EL930B  '     TO PI-MAP-NAME                   00007060
00707              GO TO 0360-XCTL-EL9301                               00007070
00708          ELSE                                                     00007080
00709              IF PI-LF-ISS-REMITTED = ZEROS                        00007090
00710                AND PI-AH-ISS-REMITTED = ZEROS                     00007100
00711                AND PI-ISS-CNT-REMITTED = ZEROS                    00007110
00712                  MOVE 'EL930C  '     TO PI-MAP-NAME               00007120
00713                  GO TO 0360-XCTL-EL9301                           00007130
00714              ELSE                                                 00007140
00715                  MOVE 'EL930B  '     TO PI-MAP-NAME               00007150
00716                  GO TO 0360-XCTL-EL9301                           00007160
00717      ELSE                                                         00007170
00718          MOVE ER-2212            TO EMI-ERROR                     00007180
00719          MOVE -1                 TO BATCHL                        00007190
00720          MOVE AL-UABON           TO BATCHA                        00007200
00721          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 00007210
00722          GO TO 8200-SEND-DATAONLY.                                00007220
00723                                                                   00007230
00724  0360-XCTL-EL9301.                                                00007240
00725      IF PI-MAINT-FUNC = 'N'                                       00007250
00726          MOVE SPACES             TO PI-KEYED-SWITCHES.            00007260
00727                                                                   00007270
00728      MOVE XCTL-9301           TO PGM-NAME.                        00007280
00729                                                                   00007290
00730      GO TO 9300-XCTL.                                             00007300
00731      EJECT                                                        00007310
00732  0400-PRIME-BATCH-TOTALS.                                         00007320
00733      EXEC CICS HANDLE CONDITION                                   00007330
00734          NOTFND (0480-NO-BATCH-TRAILER)                           00007340
00735          END-EXEC.                                                00007350
00736      MOVE PI-COMPANY-CD          TO PNDT-COMP-CD.                 00007360
00737      MOVE 9999                   TO PNDT-BATCH-SEQ.               00007370
00738      MOVE BATCHI                 TO PNDT-ENTRY-BATCH.             00007380
00739                                                                   00007390
00740      EXEC CICS READ                                               00007400
00741          DATASET (ERPNDT-FILE-ID)                                 00007410
00742          SET (ERPNDT-POINTER)                                     00007420
00743          RIDFLD (ERPNDT-KEY)                                      00007430
00744          END-EXEC.                                                00007440
00745      SERVICE RELOAD PENDING-BUSINESS.                             00007450
00746                                                                   00007460
00747      IF PB-BILLED-DT NOT = LOW-VALUES                             00007470
00748          MOVE ER-2402            TO EMI-ERROR                     00007480
00749          MOVE -1                 TO BATCHL                        00007490
00750          MOVE AL-UABON           TO BATCHA                        00007500
00751          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 00007510
00752          GO TO 0490-EXIT.                                         00007520
00753                                                                   00007530
00754      MOVE PI-COMPANY-CD          TO PI-SAV-COMP-CD.               00007540
00755      MOVE PB-ENTRY-BATCH           TO PI-SAV-ENTRY-BATCH.         00007550
00756      MOVE PB-CARRIER               TO PI-SAV-CARRIER.             00007560
00757      MOVE PB-GROUPING              TO PI-SAV-GROUPING.            00007570
00758      MOVE PB-STATE                 TO PI-SAV-STATE.               00007580
00759      MOVE PB-ACCOUNT               TO PI-SAV-ACCOUNT.             00007590
00760      MOVE PB-B-LF-ISS-PRM-REMITTED TO PI-LF-ISS-REMITTED.         00007600
00761      MOVE PB-B-LF-ISS-PRM-ENTERED  TO PI-LF-ISS-ENTERED.          00007610
00762      MOVE PB-B-AH-ISS-PRM-REMITTED TO PI-AH-ISS-REMITTED.         00007620
00763      MOVE PB-B-AH-ISS-PRM-ENTERED  TO PI-AH-ISS-ENTERED.          00007630
00764      MOVE PB-B-LF-CAN-PRM-REMITTED TO PI-LF-CAN-REMITTED.         00007640
00765      MOVE PB-B-LF-CAN-PRM-ENTERED  TO PI-LF-CAN-ENTERED.          00007650
00766      MOVE PB-B-AH-CAN-PRM-REMITTED TO PI-AH-CAN-REMITTED.         00007660
00767      MOVE PB-B-AH-CAN-PRM-ENTERED  TO PI-AH-CAN-ENTERED.          00007670
00768      MOVE PB-B-ISSUE-CNT-REMITTED  TO PI-ISS-CNT-REMITTED.        00007680
00769      MOVE PB-B-ISSUE-CNT-ENTERED   TO PI-ISS-CNT-ENTERED.         00007690
00770      MOVE PB-B-CANCEL-CNT-REMITTED TO PI-CAN-CNT-REMITTED.        00007700
00771      MOVE PB-B-CANCEL-CNT-ENTERED  TO PI-CAN-CNT-ENTERED.         00007710
00772                                                                   00007720
00773 ******************************************************************00007730
00774 *                                                                *00007740
00775 *    IF THE MONTH-END-DATE IN THE TRAILER RECORD (BATCH HEADER)  *00007750
00776 *       IS NOT EQUAL TO THE ACCOUNTS MONTH-END-DATE PRIME THE    *00007760
00777 *       TRAILER'S MONTH-END-DATE.                                *00007770
00778 *                                                                *00007780
00779 ******************************************************************00007790
00780                                                                   00007800
00781      MOVE SPACE                    TO PI-NB-MONTH-END-DT.         00007810
00782      IF  PB-CREDIT-SELECT-DT = PI-CR-MONTH-END-DT                 00007820
00783          NEXT SENTENCE                                            00007830
00784        ELSE                                                       00007840
00785          MOVE PB-CREDIT-SELECT-DT  TO  PI-NB-MONTH-END-DT.        00007850
00786                                                                   00007860
00787      GO TO 0490-EXIT.                                             00007870
00788                                                                   00007880
00789  0480-NO-BATCH-TRAILER.                                           00007890
00790      MOVE ER-2212                TO EMI-ERROR.                    00007900
00791      MOVE -1                     TO BATCHL.                       00007910
00792      MOVE AL-UABON               TO BATCHA.                       00007920
00793      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    00007930
00794                                                                   00007940
00795  0490-EXIT.                                                       00007950
00796      EXIT.                                                        00007960
00797      EJECT                                                        00007970
00798      EJECT                                                        00007980
00799  1250-READ-COMPANY-REC.                                           00007990
00800      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.                 00008000
00801      MOVE '1'                    TO CNTL-REC-TYPE.                00008010
00802      MOVE SPACES                 TO CNTL-ACCESS.                  00008020
00803      MOVE +0                     TO CNTL-SEQ.                     00008030
00804                                                                   00008040
00805      EXEC CICS HANDLE CONDITION                                   00008050
00806          NOTFND   (1250-NO-COMPANY-REC)                           00008060
00807          NOTOPEN  (7100-CNTL-FILE-NOTOPEN)                        00008070
00808          END-EXEC.                                                00008080
00809                                                                   00008090
00810      EXEC CICS READ                                               00008100
00811          DATASET   (ELCNTL-FILE-ID)                               00008110
00812          SET       (ELCNTL-POINTER)                               00008120
00813          RIDFLD    (ELCNTL-KEY)                                   00008130
00814          END-EXEC.                                                00008140
00815      SERVICE RELOAD CONTROL-FILE.                                 00008150
00816      MOVE CF-CREDIT-EDIT-CONTROLS TO PI-CREDIT-EDIT-CONTROLS.     00008160
00817      GO TO 1250-EXIT.                                             00008170
00818  1250-NO-COMPANY-REC.                                             00008180
00819      MOVE ER-2248                TO EMI-ERROR.                    00008190
00820      MOVE -1                     TO BATCHL.                       00008200
00821      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    00008210
00822      GO TO 8200-SEND-DATAONLY.                                    00008220
00823  1250-EXIT.                                                       00008230
00824      EXIT.                                                        00008240
00825      EJECT                                                        00008250
00826  1300-GET-BATCH-NO.                                               00008260
00827      EXEC CICS HANDLE CONDITION                                   00008270
00828          NOTFND   (1300-NO-COMPANY-REC)                           00008280
00829          NOTOPEN  (7100-CNTL-FILE-NOTOPEN)                        00008290
00830          END-EXEC.                                                00008300
00831                                                                   00008310
00832      EXEC CICS READ                                               00008320
00833          DATASET   (ELCNTL-FILE-ID)                               00008330
00834          SET       (ELCNTL-POINTER)                               00008340
00835          RIDFLD    (ELCNTL-KEY)                                   00008350
00836          UPDATE                                                   00008360
00837          END-EXEC.                                                00008370
00838      SERVICE RELOAD CONTROL-FILE.                                 00008380
00839                                                                   00008390
00840      MOVE 'B'                    TO  JP-RECORD-TYPE.              00008400
00841      MOVE CONTROL-FILE           TO  JP-RECORD-AREA.              00008410
00842                                                                   00008420
00843      COMPUTE WS-JOURNAL-RECORD-LENGTH =                           00008430
00844              WS-ELCNTL-RECORD-LENGTH  +  23.                      00008440
00845                                                                   00008450
00846      PERFORM 8400-LOG-JOURNAL-RECORD.                             00008460
00847                                                                   00008470
00848  1300-ASSIGN-NUMBER-LOOP.                                         00008480
00849      IF CF-LAST-BATCH-RESET                                       00008490
00850          MOVE ZEROS              TO CF-LAST-BATCH-NO              00008500
00851          ADD +1                  TO CF-LAST-BATCH-NO              00008510
00852      ELSE                                                         00008520
00853          ADD +1                  TO CF-LAST-BATCH-NO.             00008530
00854      MOVE CF-LAST-BATCH-NO       TO WS-BATCH-NO.                  00008540
00855      MOVE WS-BATCH-NO            TO PI-SAV-ENTRY-BATCH.           00008550
00856      EXEC CICS HANDLE CONDITION                                   00008560
00857          NOTFND  (1300-REWRITE-COMPANY-REC)                       00008570
00858          NOTOPEN (7000-PNDT-FILE-NOTOPEN)                         00008580
00859          END-EXEC.                                                00008590
00860      EXEC CICS READ                                               00008600
00861          SET (ERPNDT-POINTER)                                     00008610
00862          DATASET (ERPNDT-FILE-ID)                                 00008620
00863          RIDFLD (PI-SAV-ENDING-ERPNDT-KEY)                        00008630
00864          GTEQ                                                     00008640
00865          END-EXEC.                                                00008650
00866      SERVICE RELOAD PENDING-BUSINESS.                             00008660
00867      IF PI-SAV-COMP-CD NOT = PB-COMPANY-CD                        00008670
00868          GO TO 1300-REWRITE-COMPANY-REC.                          00008680
00869      IF PI-SAV-ENTRY-BATCH = PB-ENTRY-BATCH                       00008690
00870          GO TO 1300-ASSIGN-NUMBER-LOOP.                           00008700
00871  1300-REWRITE-COMPANY-REC.                                        00008710
00872      MOVE WS-BATCH-NO            TO BATCHI.                       00008720
00873      MOVE AL-UANON               TO BATCHA.                       00008730
00874                                                                   00008740
00875      MOVE 'C'                    TO  JP-RECORD-TYPE.              00008750
00876      MOVE CONTROL-FILE           TO  JP-RECORD-AREA.              00008760
00877                                                                   00008770
00878      COMPUTE WS-JOURNAL-RECORD-LENGTH =                           00008780
00879              WS-ELCNTL-RECORD-LENGTH  +  23.                      00008790
00880                                                                   00008800
00881      EXEC CICS REWRITE                                            00008810
00882          DATASET (ELCNTL-FILE-ID)                                 00008820
00883          FROM (CONTROL-FILE)                                      00008830
00884          END-EXEC.                                                00008840
00885                                                                   00008850
00886      PERFORM 8400-LOG-JOURNAL-RECORD.                             00008860
00887                                                                   00008870
00888      GO TO 1300-EXIT.                                             00008880
00889                                                                   00008890
00890  1300-NO-COMPANY-REC.                                             00008900
00891      MOVE ER-2248                TO EMI-ERROR.                    00008910
00892      MOVE -1                     TO BATCHL.                       00008920
00893      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    00008930
00894      GO TO 8200-SEND-DATAONLY.                                    00008940
00895  1300-EXIT.                                                       00008950
00896      EXIT.                                                        00008960
00897      EJECT                                                        00008970
00898  2000-WRITE-BATCH-TOTAL-REC.                                      00008980
00899      MOVE PI-COMPANY-CD          TO PNDT-COMP-CD.                 00008990
00900      MOVE PI-SAV-ENTRY-BATCH     TO PNDT-ENTRY-BATCH.             00009000
00901      MOVE 9999                   TO PNDT-BATCH-SEQ.               00009010
00902                                                                   00009020
00903      EXEC CICS HANDLE CONDITION                                   00009030
00904          NOTFND (2100-ADD-BATCH-TOTAL-REC)                        00009040
00905          NOTOPEN (7000-PNDT-FILE-NOTOPEN)                         00009050
00906          END-EXEC.                                                00009060
00907                                                                   00009070
00908      EXEC CICS READ                                               00009080
00909          SET(ERPNDT-POINTER)                                      00009090
00910          DATASET(ERPNDT-FILE-ID)                                  00009100
00911          RIDFLD(ERPNDT-KEY)                                       00009110
00912          UPDATE                                                   00009120
00913          END-EXEC.                                                00009130
00914      SERVICE RELOAD PENDING-BUSINESS.                             00009140
00915                                                                   00009150
00916 ******************************************************************00009160
00917 *                                                                *00009170
00918 *    IF THE MONTH-END-DATE IN THE TRAILER RECORD (BATCH HEADER)  *00009180
00919 *       IS NOT EQUAL TO THE ACCOUNTS MONTH-END-DATE AND THE      *00009190
00920 *       OPERATOR HAS NOT ENTERED A NEW MONTH-END-DATE USE THE    *00009200
00921 *       MONTH-END-DATE IN TRAILER RECORD FOR ALL DETAIL RECORDS. *00009210
00922 *                                                                *00009220
00923 ******************************************************************00009230
00924                                                                   00009240
00925      IF  PI-NB-MONTH-END-DT = SPACES                              00009250
00926          IF  PB-CREDIT-SELECT-DT = PI-CR-MONTH-END-DT             00009260
00927              NEXT SENTENCE                                        00009270
00928          ELSE                                                     00009280
00929              MOVE PB-CREDIT-SELECT-DT  TO  PI-NB-MONTH-END-DT.    00009290
00930                                                                   00009300
00931      MOVE 'B'                    TO JP-RECORD-TYPE.               00009310
00932      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.               00009320
00933                                                                   00009330
00934      COMPUTE WS-JOURNAL-RECORD-LENGTH =                           00009340
00935              WS-ERPNDT-RECORD-LENGTH  +  23.                      00009350
00936                                                                   00009360
00937      PERFORM 8400-LOG-JOURNAL-RECORD.                             00009370
00938                                                                   00009380
00939      IF ELFISSL NOT = ZEROS                                       00009390
00940          MOVE PI-LF-ISS-REMITTED  TO PB-B-LF-ISS-PRM-REMITTED.    00009400
00941      IF EAHISSL NOT = ZEROS                                       00009410
00942          MOVE PI-AH-ISS-REMITTED  TO PB-B-AH-ISS-PRM-REMITTED.    00009420
00943      IF EISSCNTL NOT = ZEROS                                      00009430
00944          MOVE PI-ISS-CNT-REMITTED TO PB-B-ISSUE-CNT-REMITTED.     00009440
00945      IF ECANCNTL NOT = ZEROS                                      00009450
00946          MOVE PI-CAN-CNT-REMITTED TO PB-B-CANCEL-CNT-REMITTED.    00009460
00947      IF ELFCANL NOT = ZEROS                                       00009470
00948          MOVE PI-LF-CAN-REMITTED  TO PB-B-LF-CAN-PRM-REMITTED.    00009480
00949      IF EAHCANL NOT = ZEROS                                       00009490
00950          MOVE PI-AH-CAN-REMITTED  TO PB-B-AH-CAN-PRM-REMITTED.    00009500
00951      IF WS-PF1                                                    00009510
00952          MOVE PI-LF-ISS-ENTERED   TO PB-B-LF-ISS-PRM-ENTERED      00009520
00953          MOVE PI-AH-ISS-ENTERED   TO PB-B-AH-ISS-PRM-ENTERED      00009530
00954          MOVE PI-ISS-CNT-ENTERED  TO PB-B-ISSUE-CNT-ENTERED       00009540
00955          MOVE PI-CAN-CNT-ENTERED  TO PB-B-CANCEL-CNT-ENTERED      00009550
00956          MOVE PI-LF-CAN-ENTERED   TO PB-B-LF-CAN-PRM-ENTERED      00009560
00957          MOVE PI-AH-CAN-ENTERED   TO PB-B-AH-CAN-PRM-ENTERED      00009570
00958          MOVE PI-LAST-SEQ-NO-ADDED TO PB-B-HIGHEST-SEQ-NO.        00009580
00959                                                                   00009590
00960      MOVE PI-PROCESSOR-ID        TO PB-LAST-MAINT-BY.             00009600
00961      MOVE EIBTIME                TO PB-LAST-MAINT-HHMMSS.         00009610
00962      MOVE WS-CURRENT-BIN-DT      TO PB-LAST-MAINT-DT.             00009620
00963      MOVE 'C'                    TO JP-RECORD-TYPE.               00009630
00964      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.               00009640
00965                                                                   00009650
00966      COMPUTE WS-JOURNAL-RECORD-LENGTH =                           00009660
00967              WS-ERPNDT-RECORD-LENGTH  +  23.                      00009670
00968                                                                   00009680
00969      EXEC CICS REWRITE                                            00009690
00970          DATASET(ERPNDT-FILE-ID)                                  00009700
00971          FROM(PENDING-BUSINESS)                                   00009710
00972          END-EXEC.                                                00009720
00973      PERFORM 8400-LOG-JOURNAL-RECORD.                             00009730
00974                                                                   00009740
00975      IF  PI-NB-MONTH-END-DT = PB-CREDIT-SELECT-DT                 00009750
00976          GO TO 2990-EXIT.                                         00009760
00977                                                                   00009770
00978      IF PI-NB-MONTH-END-DT GREATER THAN SPACES                    00009780
00979         PERFORM 5000-UPDATE-ENTIRE-BATCH THRU                     00009790
00980                 5900-EXIT.                                        00009800
00981                                                                   00009810
00982      GO TO 2990-EXIT.                                             00009820
00983      EJECT                                                        00009830
00984  2100-ADD-BATCH-TOTAL-REC.                                        00009840
00985      EXEC CICS GETMAIN                                            00009850
00986          SET(ERPNDT-POINTER)                                      00009860
00987          LENGTH(585)                                              00009870
00988          INITIMG(GETMAIN-SPACE)                                   00009880
00989          END-EXEC.                                                00009890
00990      SERVICE RELOAD PENDING-BUSINESS.                             00009900
00991      MOVE 'PB'                   TO PB-RECORD-ID.                 00009910
00992                                                                   00009920
00993      MOVE HIGH-VALUES            TO PB-CONTROL-BY-ACCOUNT.        00009930
00994                                                                   00009940
00995      MOVE PI-COMPANY-CD          TO PB-COMPANY-CD                 00009950
00996                                     PB-COMPANY-CD-A1.             00009960
00997      MOVE PI-COMPANY-ID          TO PB-COMPANY-ID.                00009970
00998      MOVE PI-SAV-ENTRY-BATCH     TO PB-ENTRY-BATCH                00009980
00999                                     PB-CERT-NO.                   00009990
01000      MOVE 9999                   TO PB-BATCH-SEQ-NO.              00010000
01001      MOVE HIGH-VALUES            TO PB-CERT-EFF-DT.               00010010
01002      MOVE ZEROS                  TO PB-BATCH-CHG-SEQ-NO           00010020
01003                                     PB-ALT-CHG-SEQ-NO.            00010030
01004                                                                   00010040
01005      MOVE '9'                    TO PB-RECORD-TYPE.               00010050
01006      MOVE PI-LF-ISS-REMITTED     TO PB-B-LF-ISS-PRM-REMITTED.     00010060
01007      MOVE PI-LF-ISS-ENTERED      TO PB-B-LF-ISS-PRM-ENTERED.      00010070
01008      MOVE PI-AH-ISS-REMITTED     TO PB-B-AH-ISS-PRM-REMITTED.     00010080
01009      MOVE PI-AH-ISS-ENTERED      TO PB-B-AH-ISS-PRM-ENTERED.      00010090
01010      MOVE PI-ISS-CNT-REMITTED    TO PB-B-ISSUE-CNT-REMITTED.      00010100
01011      MOVE PI-ISS-CNT-ENTERED     TO PB-B-ISSUE-CNT-ENTERED.       00010110
01012      MOVE PI-CAN-CNT-REMITTED    TO PB-B-CANCEL-CNT-REMITTED.     00010120
01013      MOVE PI-CAN-CNT-ENTERED     TO PB-B-CANCEL-CNT-ENTERED.      00010130
01014      MOVE PI-LF-CAN-REMITTED     TO PB-B-LF-CAN-PRM-REMITTED.     00010140
01015      MOVE PI-LF-CAN-ENTERED      TO PB-B-LF-CAN-PRM-ENTERED.      00010150
01016      MOVE PI-AH-CAN-REMITTED     TO PB-B-AH-CAN-PRM-REMITTED.     00010160
01017      MOVE PI-AH-CAN-ENTERED      TO PB-B-AH-CAN-PRM-ENTERED.      00010170
01018      MOVE ZEROS                  TO PB-B-LF-ISS-PRM-COMPUTED      00010180
01019                                     PB-B-LF-CAN-PRM-COMPUTED      00010190
01020                                     PB-B-AH-ISS-PRM-COMPUTED      00010200
01021                                     PB-B-AH-CAN-PRM-COMPUTED      00010210
01022                                     PB-LF-BILLED-AMTS             00010220
01023                                     PB-AH-BILLED-AMTS             00010230
01024                                     PB-CHG-COUNT                  00010240
01025                                     PB-CALC-TOLERANCE.            00010250
01026      MOVE LOW-VALUES             TO PB-CREDIT-ACCEPT-DT           00010260
01027                                     PB-BILLED-DT                  00010270
01028                                     PB-ACCT-EFF-DT                00010280
01029                                     PB-ACCT-EXP-DT.               00010290
01030      IF  PI-NB-MONTH-END-DT GREATER THAN SPACES                   00010300
01031          MOVE PI-NB-MONTH-END-DT TO PB-CREDIT-SELECT-DT           00010310
01032         ELSE                                                      00010320
01033          MOVE PI-CR-MONTH-END-DT     TO PB-CREDIT-SELECT-DT.      00010330
01034      MOVE PI-LAST-SEQ-NO-ADDED   TO PB-B-HIGHEST-SEQ-NO.          00010340
01035                                                                   00010350
01036      MOVE PI-PROCESSOR-ID        TO PB-LAST-MAINT-BY              00010360
01037                                     PB-INPUT-BY.                  00010370
01038      MOVE EIBTIME                TO PB-LAST-MAINT-HHMMSS.         00010380
01039      MOVE WS-CURRENT-BIN-DT      TO PB-LAST-MAINT-DT              00010390
01040                                     PB-INPUT-DT.                  00010400
01041      MOVE 'A'                    TO JP-RECORD-TYPE.               00010410
01042      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.               00010420
01043                                                                   00010430
01044                                                                   00010440
01045      COMPUTE WS-JOURNAL-RECORD-LENGTH =                           00010450
01046              WS-ERPNDT-RECORD-LENGTH  +  23.                      00010460
01047                                                                   00010470
01048      MOVE PI-AM-NAME             TO  PB-ACCOUNT-NAME.             00010480
01049      MOVE PB-CONTROL-PRIMARY     TO  ERPNDT-KEY.                  00010490
01050                                                                   00010500
01051      EXEC CICS WRITE                                              00010510
01052          DATASET(ERPNDT-FILE-ID)                                  00010520
01053          FROM(PENDING-BUSINESS)                                   00010530
01054          RIDFLD(PB-CONTROL-PRIMARY)                               00010540
01055          END-EXEC.                                                00010550
01056      PERFORM 8400-LOG-JOURNAL-RECORD.                             00010560
01057                                                                   00010570
01058  2990-EXIT.                                                       00010580
01059      EXIT.                                                        00010590
01060      EJECT                                                        00010600
01061  3000-DELETE-ENTERED-BATCH.                                       00010610
01062                                                                   00010620
01063      MOVE SPACE          TO PI-NB-MONTH-END-DT.                   00010630
01064                                                                   00010640
01065      IF NOT PI-DELETE-IS-OK                                       00010650
01066          MOVE 'Y'                TO PI-VERIFY-DELETE-SW           00010660
01067          GO TO 3300-FIRST-PF2.                                    00010670
01068      MOVE SPACE                  TO PI-VERIFY-DELETE-SW.          00010680
01069      MOVE PI-COMPANY-CD          TO PNDT-COMP-CD.                 00010690
01070      MOVE PI-SAV-ENTRY-BATCH     TO PNDT-ENTRY-BATCH.             00010700
01071      MOVE 1                      TO PNDT-BATCH-SEQ.               00010710
01072                                                                   00010720
01073      EXEC CICS SYNCPOINT                                          00010730
01074           END-EXEC.                                               00010740
01075                                                                   00010750
01076      EXEC CICS HANDLE CONDITION                                   00010760
01077          NOTFND(3200-NO-RECORDS)                                  00010770
01078          NOTOPEN(7000-PNDT-FILE-NOTOPEN)                          00010780
01079          END-EXEC.                                                00010790
01080                                                                   00010800
01081                                                                   00010810
01082      EXEC CICS STARTBR                                            00010820
01083          DATASET(ERPNDT-FILE-ID)                                  00010830
01084          RIDFLD(ERPNDT-KEY)                                       00010840
01085          END-EXEC.                                                00010850
01086                                                                   00010860
01087  3000-GET-NEXT-RECORD.                                            00010870
01088                                                                   00010880
01089      EXEC CICS HANDLE CONDITION                                   00010890
01090          NOTFND(3120-END-ROUTINE)                                 00010900
01091          END-EXEC.                                                00010910
01092                                                                   00010920
01093      ADD +1                      TO  WS-SYNC-CNTR.                00010930
01094      IF  WS-SYNC-CNTR GREATER THAN +100                           00010940
01095          MOVE +0                 TO  WS-SYNC-CNTR                 00010950
01096          EXEC  CICS SYNCPOINT                                     00010960
01097                END-EXEC.                                          00010970
01098                                                                   00010980
01099      ON 1                                                         00010990
01100         GO TO 3005-READ-NEXT-RECORD.                              00011000
01101                                                                   00011010
01102      EXEC CICS STARTBR                                            00011020
01103          DATASET(ERPNDT-FILE-ID)                                  00011030
01104          RIDFLD(ERPNDT-KEY)                                       00011040
01105          END-EXEC.                                                00011050
01106                                                                   00011060
01107      EXEC CICS HANDLE CONDITION                                   00011070
01108          NOTFND(3100-STOP-BROWSE)                                 00011080
01109          ENDFILE(3100-STOP-BROWSE)                                00011090
01110          END-EXEC.                                                00011100
01111                                                                   00011110
01112  3005-READ-NEXT-RECORD.                                           00011120
01113                                                                   00011130
01114      EXEC CICS READNEXT                                           00011140
01115          SET(ERPNDT-POINTER)                                      00011150
01116          DATASET(ERPNDT-FILE-ID)                                  00011160
01117          RIDFLD(ERPNDT-KEY)                                       00011170
01118          END-EXEC.                                                00011180
01119                                                                   00011190
01120      SERVICE RELOAD PENDING-BUSINESS.                             00011200
01121                                                                   00011210
01122      IF PB-COMPANY-CD = PI-SAV-COMP-CD                            00011220
01123        AND PB-ENTRY-BATCH = PI-SAV-ENTRY-BATCH                    00011230
01124          NEXT SENTENCE                                            00011240
01125      ELSE                                                         00011250
01126          GO TO 3100-STOP-BROWSE.                                  00011260
01127                                                                   00011270
01128      IF PB-BILLED-DT NOT = LOW-VALUES                             00011280
01129          MOVE ER-2402            TO EMI-ERROR                     00011290
01130          MOVE -1                 TO BATCHL                        00011300
01131          MOVE AL-UABON           TO BATCHA                        00011310
01132          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 00011320
01133          GO TO 8200-SEND-DATAONLY.                                00011330
01134                                                                   00011340
01135 ******************************************************************00011350
01136 *                                                                *00011360
01137 *                DELETE PENDING ADDRESS RECORD                   *00011370
01138 *                                                                *00011380
01139 ******************************************************************00011390
01140                                                                   00011400
01141      IF PB-I-MAIL-ADDRS-PRESENT                                   00011410
01142         NEXT SENTENCE                                             00011420
01143        ELSE                                                       00011430
01144         GO TO 3006-DELETE-CERTIFICATE.                            00011440
01145                                                                   00011450
01146      EXEC CICS HANDLE CONDITION                                   00011460
01147           NOTOPEN  (3006-DELETE-CERTIFICATE)                      00011470
01148           NOTFND   (3006-DELETE-CERTIFICATE)                      00011480
01149           END-EXEC.                                               00011490
01150                                                                   00011500
01151      EXEC CICS READ                                               00011510
01152          EQUAL                                                    00011520
01153          DATASET   (ERPNDM-FILE-ID)                               00011530
01154          SET       (ERPNDM-POINTER)                               00011540
01155          RIDFLD    (ERPNDT-KEY)                                   00011550
01156          UPDATE                                                   00011560
01157          END-EXEC.                                                00011570
01158                                                                   00011580
01159         SERVICE RELOAD PENDING-MAILING-DATA.                      00011590
01160                                                                   00011600
01161      MOVE 'D'                    TO  JP-RECORD-TYPE.              00011610
01162      MOVE PENDING-MAILING-DATA   TO  JP-RECORD-AREA.              00011620
01163                                                                   00011630
01164      COMPUTE WS-JOURNAL-RECORD-LENGTH =                           00011640
01165              WS-ERPNDM-RECORD-LENGTH  +  23.                      00011650
01166                                                                   00011660
01167      EXEC CICS DELETE                                             00011670
01168          DATASET   (ERPNDM-FILE-ID)                               00011680
01169          END-EXEC.                                                00011690
01170                                                                   00011700
01171      PERFORM 8400-LOG-JOURNAL-RECORD.                             00011710
01172                                                                   00011720
01173  3006-DELETE-CERTIFICATE.                                         00011730
01174                                                                   00011740
01175      EXEC CICS HANDLE CONDITION                                   00011750
01176           NOTFND  (3020-CONTINUE-DELETE)                          00011760
01177           END-EXEC.                                               00011770
01178                                                                   00011780
01179      MOVE PB-CONTROL-BY-ACCOUNT  TO ELCERT-KEY.                   00011790
01180      MOVE PB-SV-CARRIER          TO CERT-CARRIER.                 00011800
01181      MOVE PB-SV-GROUPING         TO CERT-GROUPING.                00011810
01182      MOVE PB-SV-STATE            TO CERT-STATE.                   00011820
01183      EXEC CICS READ                                               00011830
01184          SET     (ELCERT-POINTER)                                 00011840
01185          DATASET (ELCERT-FILE-ID)                                 00011850
01186          RIDFLD  (ELCERT-KEY)                                     00011860
01187          UPDATE                                                   00011870
01188          END-EXEC.                                                00011880
01189                                                                   00011890
01190      SERVICE RELOAD CERTIFICATE-MASTER.                           00011900
01191                                                                   00011910
01192      IF INSURED-ADDR-PRESENT                                      00011920
01193         MOVE 'Y'                  TO WS-CERT-ADDRESS-SW           00011930
01194      ELSE                                                         00011940
01195         MOVE ' '                  TO WS-CERT-ADDRESS-SW.          00011950
01196                                                                   00011960
01197      IF  CERT-NOTES-ARE-NOT-PRESENT                               00011970
01198          MOVE ' '                TO WS-CERT-NOTE-SW               00011980
01199      ELSE                                                         00011990
01200          MOVE 'Y'                TO WS-CERT-NOTE-SW.              00012000
01201                                                                   00012010
01202      IF PB-CANCELLATION                                           00012020
01203         IF PB-CI-LF-PRIOR-CANCEL-DT = LOW-VALUES                  00012030
01204            MOVE SPACE                 TO CM-CREDIT-INTERFACE-SW-2 00012040
01205            MOVE ZERO                  TO CM-LF-ITD-CANCEL-AMT     00012050
01206            MOVE LOW-VALUE             TO CM-LF-CANCEL-EXIT-DT     00012060
01207            MOVE LOW-VALUE             TO CM-LF-CANCEL-DT          00012070
01208            MOVE CM-LF-STATUS-AT-CANCEL TO CM-LF-CURRENT-STATUS    00012080
01209            MOVE SPACE                 TO CM-LF-STATUS-AT-CANCEL   00012090
01210         ELSE                                                      00012100
01211            MOVE SPACE                 TO CM-CREDIT-INTERFACE-SW-2 00012110
01212            MOVE PB-CI-LF-CANCEL-AMT   TO CM-LF-ITD-CANCEL-AMT     00012120
01213            MOVE PB-CI-LF-PRIOR-CANCEL-DT TO CM-LF-CANCEL-DT       00012130
01214            MOVE PB-CI-LF-POLICY-STATUS   TO CM-LF-CURRENT-STATUS  00012140
01215            MOVE PB-CI-LIVES           TO CM-LIVES.                00012150
01216                                                                   00012160
01217      IF PB-CANCELLATION                                           00012170
01218         IF PB-CI-AH-PRIOR-CANCEL-DT = LOW-VALUES                  00012180
01219            MOVE SPACE                 TO CM-CREDIT-INTERFACE-SW-2 00012190
01220            MOVE ZERO                  TO CM-AH-ITD-CANCEL-AMT     00012200
01221            MOVE LOW-VALUE             TO CM-AH-CANCEL-EXIT-DT     00012210
01222            MOVE LOW-VALUE             TO CM-AH-CANCEL-DT          00012220
01223            MOVE CM-AH-STATUS-AT-CANCEL TO CM-AH-CURRENT-STATUS    00012230
01224            MOVE SPACE                 TO CM-AH-STATUS-AT-CANCEL   00012240
01225            GO TO 3010-REWRITE-CERT                                00012250
01226         ELSE                                                      00012260
01227            MOVE SPACE                 TO CM-CREDIT-INTERFACE-SW-2 00012270
01228            MOVE PB-CI-AH-CANCEL-AMT   TO CM-AH-ITD-CANCEL-AMT     00012280
01229            MOVE PB-CI-AH-PRIOR-CANCEL-DT TO CM-AH-CANCEL-DT       00012290
01230            MOVE PB-CI-AH-POLICY-STATUS   TO CM-AH-CURRENT-STATUS  00012300
01231            MOVE PB-CI-LIVES           TO CM-LIVES                 00012310
01232            GO TO 3010-REWRITE-CERT.                               00012320
01233                                                                   00012330
01234      IF CERT-ADDED-BATCH  AND                                     00012340
01235         (NO-CLAIM-ATTACHED OR CM-CLAIM-ATTACHED-COUNT = ZEROS)    00012350
01236         GO TO 3010-REWRITE-CERT.                                  00012360
01237                                                                   00012370
01238      IF NO-CLAIM-ATTACHED  NEXT SENTENCE                          00012380
01239        ELSE                                                       00012390
01240         MOVE SPACE                  TO CM-CREDIT-INTERFACE-SW-1   00012400
01241         MOVE '2'                    TO CM-CLAIM-INTERFACE-SW      00012410
01242         GO TO 3010-REWRITE-CERT.                                  00012420
01243                                                                   00012430
01244         EXEC CICS DELETE                                          00012440
01245              DATASET    (ELCERT-FILE-ID)                          00012450
01246              END-EXEC.                                            00012460
01247                                                                   00012470
01248 ******************************************************************00012480
01249 *                                                                *00012490
01250 *       DELETE CERTIFICATE NOTES FOR ALL DELETED CERTIFICATES    *00012500
01251 *                                                                *00012510
01252 ******************************************************************00012520
01253                                                                   00012530
01254      IF CERT-NOTES-ARE-PRESENT                                    00012540
01255         NEXT SENTENCE                                             00012550
01256      ELSE                                                         00012560
01257         GO TO 3007-DELETE-MAILING-DATA.                           00012570
01258                                                                   00012580
01259      EXEC CICS HANDLE CONDITION                                   00012590
01260           NOTFND   (3007-DELETE-MAILING-DATA)                     00012600
01261           END-EXEC.                                               00012610
01262                                                                   00012620
01263      EXEC CICS READ                                               00012630
01264          EQUAL                                                    00012640
01265          DATASET   (ERNOTE-FILE-ID)                               00012650
01266          SET       (ERNOTE-POINTER)                               00012660
01267          RIDFLD    (ELCERT-KEY)                                   00012670
01268          UPDATE                                                   00012680
01269          END-EXEC.                                                00012690
01270                                                                   00012700
01271         SERVICE RELOAD CERTIFICATE-NOTE.                          00012710
01272                                                                   00012720
01273      MOVE 'D'                    TO  JP-RECORD-TYPE.              00012730
01274      MOVE CERTIFICATE-NOTE       TO  JP-RECORD-AREA.              00012740
01275                                                                   00012750
01276      COMPUTE WS-JOURNAL-RECORD-LENGTH =                           00012760
01277              WS-ERNOTE-RECORD-LENGTH  +  23.                      00012770
01278                                                                   00012780
01279      EXEC CICS DELETE                                             00012790
01280          DATASET   (ERNOTE-FILE-ID)                               00012800
01281          END-EXEC.                                                00012810
01282                                                                   00012820
01283      PERFORM 8400-LOG-JOURNAL-RECORD.                             00012830
01284                                                                   00012840
01285                                                                   00012850
01286                                                                   00012860
01287 ******************************************************************00012870
01288 *                                                                *00012880
01289 *     DELETE MAILING ADDRESS RECORDS FOR DELETED CERTIFICATES    *00012890
01290 *                                                                *00012900
01291 ******************************************************************00012910
01292                                                                   00012920
01293  3007-DELETE-MAILING-DATA.                                        00012930
01294                                                                   00012940
01295      IF CERT-ADDRESS-PRESENT                                      00012950
01296         NEXT SENTENCE                                             00012960
01297      ELSE                                                         00012970
01298         GO TO 3020-CONTINUE-DELETE.                               00012980
01299                                                                   00012990
01300      EXEC CICS HANDLE CONDITION                                   00013000
01301           NOTOPEN  (3020-CONTINUE-DELETE)                         00013010
01302           NOTFND   (3020-CONTINUE-DELETE)                         00013020
01303           END-EXEC.                                               00013030
01304                                                                   00013040
01305      EXEC CICS READ                                               00013050
01306          EQUAL                                                    00013060
01307          DATASET   (ERMAIL-FILE-ID)                               00013070
01308          SET       (ERMAIL-POINTER)                               00013080
01309          RIDFLD    (ELCERT-KEY)                                   00013090
01310          UPDATE                                                   00013100
01311          END-EXEC.                                                00013110
01312                                                                   00013120
01313         SERVICE RELOAD MAILING-DATA.                              00013130
01314                                                                   00013140
01315      MOVE 'D'                    TO  JP-RECORD-TYPE.              00013150
01316      MOVE MAILING-DATA           TO  JP-RECORD-AREA.              00013160
01317                                                                   00013170
01318      COMPUTE WS-JOURNAL-RECORD-LENGTH =                           00013180
01319              WS-ERMAIL-RECORD-LENGTH  +  23.                      00013190
01320                                                                   00013200
01321      EXEC CICS DELETE                                             00013210
01322          DATASET   (ERMAIL-FILE-ID)                               00013220
01323          END-EXEC.                                                00013230
01324                                                                   00013240
01325      PERFORM 8400-LOG-JOURNAL-RECORD.                             00013250
01326                                                                   00013260
01327      GO TO 3020-CONTINUE-DELETE.                                  00013270
01328                                                                   00013280
01329  3010-REWRITE-CERT.                                               00013290
01330                                                                   00013300
01331      MOVE SPACE                  TO CM-CREDIT-INTERFACE-SW-1.     00013310
01332                                                                   00013320
01333         EXEC CICS REWRITE                                         00013330
01334              DATASET    (ELCERT-FILE-ID)                          00013340
01335              FROM       (CERTIFICATE-MASTER)                      00013350
01336              END-EXEC.                                            00013360
01337                                                                   00013370
01338  3020-CONTINUE-DELETE.                                            00013380
01339                                                                   00013390
01340      COMPUTE WS-JOURNAL-RECORD-LENGTH =                           00013400
01341              WS-ERPNDT-RECORD-LENGTH  +  23.                      00013410
01342                                                                   00013420
01343      MOVE 'D'                    TO JP-RECORD-TYPE.               00013430
01344      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.               00013440
01345                                                                   00013450
01346      EXEC CICS ENDBR                                              00013460
01347          DATASET(ERPNDT-FILE-ID)                                  00013470
01348          END-EXEC.                                                00013480
01349                                                                   00013490
01350      EXEC CICS DELETE                                             00013500
01351          DATASET(ERPNDT-FILE-ID)                                  00013510
01352          RIDFLD(ERPNDT-KEY)                                       00013520
01353          END-EXEC.                                                00013530
01354                                                                   00013540
01355      PERFORM 8400-LOG-JOURNAL-RECORD.                             00013550
01356      ADD 1   TO WS-DELETE-CNT.                                    00013560
01357      GO TO 3000-GET-NEXT-RECORD.                                  00013570
01358                                                                   00013580
01359  3100-STOP-BROWSE.                                                00013590
01360                                                                   00013600
01361      EXEC CICS ENDBR                                              00013610
01362          DATASET(ERPNDT-FILE-ID)                                  00013620
01363          END-EXEC.                                                00013630
01364                                                                   00013640
01365  3120-END-ROUTINE.                                                00013650
01366                                                                   00013660
01367      MOVE SPACE                  TO PI-UPDATE-SW.                 00013670
01368      IF WS-DELETE-CNT NOT GREATER THAN ZERO                       00013680
01369          GO TO 3200-NO-RECORDS.                                   00013690
01370      IF PI-SAVE-CALLING-PGM NOT = XCTL-9301                       00013700
01371          MOVE ZEROS              TO EMI-ERROR                     00013710
01372          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                00013720
01373      MOVE SPACES                 TO PI-PROGRAM-WORK-AREA.         00013730
01374      MOVE LOW-VALUES             TO EL930AO.                      00013740
01375      MOVE QUESTION-MARKS         TO BATCHI.                       00013750
01376      MOVE AL-UANON               TO BATCHA.                       00013760
01377      MOVE -1                     TO MAINTL.                       00013770
01378      GO TO 8100-SEND-INITIAL-MAP.                                 00013780
01379                                                                   00013790
01380  3200-NO-RECORDS.                                                 00013800
01381      MOVE ER-2242                TO EMI-ERROR.                    00013810
01382      MOVE -1 TO BATCHL                                            00013820
01383      MOVE AL-UABON               TO BATCHA.                       00013830
01384      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    00013840
01385      GO TO 8200-SEND-DATAONLY.                                    00013850
01386                                                                   00013860
01387  3300-FIRST-PF2.                                                  00013870
01388      MOVE ER-2422                TO EMI-ERROR.                    00013880
01389      MOVE -1                     TO PFENTERL.                     00013890
01390      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    00013900
01391      GO TO 8100-SEND-INITIAL-MAP.                                 00013910
01392      EJECT                                                        00013920
01393  4000-SHOW-TOTALS.                                                00013930
01394                                                                   00013940
01395      MOVE LOW-VALUES             TO EL930AI.                      00013950
01396                                                                   00013960
01397      MOVE PI-SAV-ENTRY-BATCH     TO BATCHO.                       00013970
01398                                                                   00013980
01399      MOVE PI-MAINT-FUNC          TO MAINTO.                       00013990
01400                                                                   00014000
01401                                                                   00014010
01402      IF PI-DATA-UPDATED                                           00014020
01403          MOVE AL-SANON           TO BATCHA                        00014030
01404                                     MAINTA                        00014040
01405      ELSE                                                         00014050
01406          MOVE AL-UANON           TO BATCHA                        00014060
01407                                     MAINTA.                       00014070
01408                                                                   00014080
01409      MOVE PI-LF-ISS-ENTERED      TO ALFISSO.                      00014090
01410                                                                   00014100
01411      IF PI-LF-ISS-REMITTED NOT = ZEROS                            00014110
01412          MOVE PI-LF-ISS-REMITTED TO ELFISSO                       00014120
01413          MOVE AL-UNNON           TO ELFISSA                       00014130
01414          COMPUTE WS-OBAL = PI-LF-ISS-REMITTED - PI-LF-ISS-ENTERED 00014140
01415          IF WS-OBAL NOT = ZEROS                                   00014150
01416              MOVE WS-OBAL        TO OLFISSO.                      00014160
01417                                                                   00014170
01418      MOVE PI-LF-CAN-ENTERED      TO ALFCANO.                      00014180
01419      IF PI-LF-CAN-REMITTED NOT = ZEROS                            00014190
01420          MOVE PI-LF-CAN-REMITTED TO ELFCANO                       00014200
01421          MOVE AL-UNNON           TO ELFCANA                       00014210
01422          COMPUTE WS-OBAL = PI-LF-CAN-REMITTED - PI-LF-CAN-ENTERED 00014220
01423          IF WS-OBAL NOT = ZEROS                                   00014230
01424              MOVE WS-OBAL        TO OLFCANO.                      00014240
01425                                                                   00014250
01426      MOVE PI-AH-ISS-ENTERED      TO AAHISSO.                      00014260
01427      IF PI-AH-ISS-REMITTED NOT = ZEROS                            00014270
01428          MOVE PI-AH-ISS-REMITTED TO EAHISSO                       00014280
01429          MOVE AL-UNNON           TO EAHISSA                       00014290
01430          COMPUTE WS-OBAL = PI-AH-ISS-REMITTED - PI-AH-ISS-ENTERED 00014300
01431          IF WS-OBAL NOT = ZEROS                                   00014310
01432              MOVE WS-OBAL        TO OAHISSO.                      00014320
01433                                                                   00014330
01434      MOVE PI-AH-CAN-ENTERED      TO AAHCANO.                      00014340
01435      IF PI-AH-CAN-REMITTED NOT = ZEROS                            00014350
01436          MOVE PI-AH-CAN-REMITTED TO EAHCANO                       00014360
01437          MOVE AL-UNNON           TO EAHCANA                       00014370
01438          COMPUTE WS-OBAL = PI-AH-CAN-REMITTED - PI-AH-CAN-ENTERED 00014380
01439          IF WS-OBAL NOT = ZEROS                                   00014390
01440              MOVE WS-OBAL        TO OAHCANO.                      00014400
01441                                                                   00014410
01442      MOVE PI-ISS-CNT-ENTERED     TO AISSCNTO.                     00014420
01443      IF PI-ISS-CNT-REMITTED NOT = ZEROS                           00014430
01444          MOVE PI-ISS-CNT-REMITTED TO EISSCNTO                     00014440
01445          MOVE AL-UNNON            TO EISSCNTA                     00014450
01446          COMPUTE WS-OCNT = PI-ISS-CNT-REMITTED -                  00014460
01447                                 PI-ISS-CNT-ENTERED                00014470
01448          IF WS-OCNT NOT = ZEROS                                   00014480
01449              MOVE WS-OCNT        TO OISSCNTO.                     00014490
01450                                                                   00014500
01451      MOVE PI-CAN-CNT-ENTERED     TO ACANCNTO.                     00014510
01452      IF PI-CAN-CNT-REMITTED NOT = ZEROS                           00014520
01453          MOVE PI-CAN-CNT-REMITTED TO ECANCNTO                     00014530
01454          MOVE AL-UNNON            TO ECANCNTA                     00014540
01455          COMPUTE WS-OCNT = PI-CAN-CNT-REMITTED -                  00014550
01456                                 PI-CAN-CNT-ENTERED                00014560
01457          IF WS-OCNT NOT = ZEROS                                   00014570
01458              MOVE WS-OCNT        TO OCANCNTO.                     00014580
01459                                                                   00014590
01460                                                                   00014600
01461      IF  PI-NB-MONTH-END-DT GREATER THAN SPACES                   00014610
01462          MOVE PI-NB-MONTH-END-DT TO DC-BIN-DATE-1                 00014620
01463        ELSE                                                       00014630
01464          MOVE PI-CR-MONTH-END-DT TO DC-BIN-DATE-1.                00014640
01465                                                                   00014650
01466      MOVE  SPACE                 TO  DC-OPTION-CODE.              00014660
01467      PERFORM  9700-DATE-LINK.                                     00014670
01468                                                                   00014680
01469      IF  DATE-CONVERSION-ERROR                                    00014690
01470          MOVE LOW-VALUES TO MNTHNDTO                              00014700
01471          GO TO 0490-EXIT.                                         00014710
01472                                                                   00014720
01473      MOVE DC-GREG-DATE-1-EDIT    TO  MNTHNDTO.                    00014730
01474      MOVE AL-UANON               TO  MNTHNDTA.                    00014740
01475                                                                   00014750
01476      GO TO 8100-SEND-INITIAL-MAP.                                 00014760
01477      EJECT                                                        00014770
01478                                                                   00014780
01479 ***************************************************************** 00014790
01480 *                                                               * 00014800
01481 *            U P D A T E   E N T I R E   B A T C H              * 00014810
01482 *                                                               * 00014820
01483 *  THIS SECTION UPDATES AN ENTIRE BATCH WITH A NEW MONTH END     *00014830
01484 *  DATE.                                                        * 00014840
01485 *                                                               * 00014850
01486 ***************************************************************** 00014860
01487                                                                   00014870
01488  5000-UPDATE-ENTIRE-BATCH.                                        00014880
01489                                                                   00014890
01490      MOVE ZEROS                  TO  PNDT-BATCH-SEQ               00014900
01491                                      PNDT-BATCH-CHG-SEQ.          00014910
01492                                                                   00014920
01493      EXEC CICS HANDLE CONDITION                                   00014930
01494          NOTFND(5900-EXIT)                                        00014940
01495          NOTOPEN(7000-PNDT-FILE-NOTOPEN)                          00014950
01496          END-EXEC.                                                00014960
01497                                                                   00014970
01498      EXEC CICS STARTBR                                            00014980
01499          DATASET(ERPNDT-FILE-ID)                                  00014990
01500          RIDFLD(ERPNDT-KEY)                                       00015000
01501          END-EXEC.                                                00015010
01502                                                                   00015020
01503  5010-GET-NEXT-RECORD.                                            00015030
01504      EXEC CICS HANDLE CONDITION                                   00015040
01505          NOTFND(5100-STOP-BROWSE)                                 00015050
01506          ENDFILE(5100-STOP-BROWSE)                                00015060
01507          END-EXEC.                                                00015070
01508                                                                   00015080
01509      EXEC CICS READNEXT                                           00015090
01510          SET(ERPNDT-POINTER)                                      00015100
01511          DATASET(ERPNDT-FILE-ID)                                  00015110
01512          RIDFLD(ERPNDT-KEY)                                       00015120
01513          END-EXEC.                                                00015130
01514                                                                   00015140
01515                                                                   00015150
01516      SERVICE RELOAD PENDING-BUSINESS.                             00015160
01517                                                                   00015170
01518      ON 1                                                         00015180
01519           MOVE PB-CONTROL-PRIMARY     TO WS-SAV-PNDT-KEY.         00015190
01520                                                                   00015200
01521      IF PB-COMPANY-CD = WS-SAV-COMP-CD                            00015210
01522        AND PB-ENTRY-BATCH = WS-SAV-ENTRY-BATCH                    00015220
01523          NEXT SENTENCE                                            00015230
01524      ELSE                                                         00015240
01525          GO TO 5100-STOP-BROWSE.                                  00015250
01526                                                                   00015260
01527                                                                   00015270
01528      IF  PB-CREDIT-SELECT-DT = PI-NB-MONTH-END-DT                 00015280
01529          GO TO 5010-GET-NEXT-RECORD.                              00015290
01530                                                                   00015300
01531                                                                   00015310
01532                                                                   00015320
01533                                                                   00015330
01534      EXEC CICS READ                                               00015340
01535          SET     (ERPNDT-POINTER)                                 00015350
01536          DATASET (ERPNDT-FILE-ID)                                 00015360
01537          RIDFLD  (ERPNDT-KEY)                                     00015370
01538          UPDATE                                                   00015380
01539          END-EXEC.                                                00015390
01540                                                                   00015400
01541      SERVICE RELOAD PENDING-BUSINESS.                             00015410
01542                                                                   00015420
01543                                                                   00015430
01544      COMPUTE WS-JOURNAL-RECORD-LENGTH =                           00015440
01545              WS-ERPNDT-RECORD-LENGTH  +  23.                      00015450
01546                                                                   00015460
01547      MOVE 'B'                    TO JP-RECORD-TYPE.               00015470
01548      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.               00015480
01549      PERFORM 8400-LOG-JOURNAL-RECORD.                             00015490
01550                                                                   00015500
01551      MOVE  PI-NB-MONTH-END-DT    TO PB-CREDIT-SELECT-DT.          00015510
01552                                                                   00015520
01553      MOVE 'C'                    TO JP-RECORD-TYPE.               00015530
01554      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.               00015540
01555                                                                   00015550
01556  5050-REWRITE-PENDING-BUS.                                        00015560
01557         EXEC CICS REWRITE                                         00015570
01558              DATASET    (ERPNDT-FILE-ID)                          00015580
01559              FROM       (PENDING-BUSINESS)                        00015590
01560              END-EXEC.                                            00015600
01561                                                                   00015610
01562                                                                   00015620
01563      COMPUTE WS-JOURNAL-RECORD-LENGTH =                           00015630
01564              WS-ERPNDT-RECORD-LENGTH  +  23.                      00015640
01565                                                                   00015650
01566      PERFORM 8400-LOG-JOURNAL-RECORD.                             00015660
01567                                                                   00015670
01568      GO TO 5010-GET-NEXT-RECORD.                                  00015680
01569                                                                   00015690
01570  5100-STOP-BROWSE.                                                00015700
01571                                                                   00015710
01572      EXEC CICS ENDBR                                              00015720
01573          DATASET(ERPNDT-FILE-ID)                                  00015730
01574          END-EXEC.                                                00015740
01575                                                                   00015750
01576  5900-EXIT.                                                       00015760
01577       EXIT.                                                       00015770
01578                                                                   00015780
01579      EJECT                                                        00015790
01580                                                                   00015800
01581                                                                   00015810
01582  6000-BROWSE-BATCH-HEADERS.                                       00015820
01583                                                                   00015830
01584      EXEC CICS HANDLE CONDITION                                   00015840
01585          NOTFND (6200-END-OF-FILE)                                00015850
01586          END-EXEC.                                                00015860
01587                                                                   00015870
01588      MOVE SPACES                 TO ERPNDT-KEY.                   00015880
01589                                                                   00015890
01590      MOVE PI-COMPANY-CD          TO PNDT-COMP-CD.                 00015900
01591                                                                   00015910
01592      IF  BATCHI = QUESTION-MARKS                                  00015920
01593          NEXT SENTENCE                                            00015930
01594      ELSE                                                         00015940
01595          MOVE BATCHI             TO PNDT-ENTRY-BATCH              00015950
01596          MOVE 9999               TO PNDT-BATCH-SEQ.               00015960
01597                                                                   00015970
01598      EXEC CICS STARTBR                                            00015980
01599          DATASET (ERPNDT-FILE-ID)                                 00015990
01600          RIDFLD  (ERPNDT-KEY)                                     00016000
01601          END-EXEC.                                                00016010
01602                                                                   00016020
01603      EXEC CICS HANDLE CONDITION                                   00016030
01604          ENDFILE  (6100-END-BROWSE)                               00016040
01605          END-EXEC.                                                00016050
01606                                                                   00016060
01607  6010-READ-NEXT-RECORD.                                           00016070
01608                                                                   00016080
01609      EXEC CICS READNEXT                                           00016090
01610           DATASET   (ERPNDT-FILE-ID)                              00016100
01611           SET       (ERPNDT-POINTER)                              00016110
01612           RIDFLD    (ERPNDT-KEY)                                  00016120
01613           END-EXEC.                                               00016130
01614                                                                   00016140
01615      SERVICE RELOAD PENDING-BUSINESS.                             00016150
01616                                                                   00016160
01617      IF PB-COMPANY-CD NOT = PI-COMPANY-CD                         00016170
01618         GO TO 6100-END-BROWSE.                                    00016180
01619                                                                   00016190
01620      IF PB-BATCH-TRAILER                                          00016200
01621         MOVE PB-ENTRY-BATCH      TO BATCHI                        00016210
01622         MOVE +6                  TO BATCHL                        00016220
01623         PERFORM 6100-END-BROWSE                                   00016230
01624         PERFORM 0400-PRIME-BATCH-TOTALS THRU 0490-EXIT            00016240
01625         GO TO 4000-SHOW-TOTALS.                                   00016250
01626                                                                   00016260
01627      MOVE PB-ENTRY-BATCH     TO PNDT-ENTRY-BATCH.                 00016270
01628      MOVE 9998               TO PNDT-BATCH-SEQ.                   00016280
01629                                                                   00016290
01630      GO TO 6010-READ-NEXT-RECORD.                                 00016300
01631                                                                   00016310
01632  6100-END-BROWSE.                                                 00016320
01633                                                                   00016330
01634      EXEC CICS ENDBR                                              00016340
01635          DATASET(ERPNDT-FILE-ID)                                  00016350
01636          END-EXEC.                                                00016360
01637                                                                   00016370
01638  6200-END-OF-FILE.                                                00016380
01639                                                                   00016390
01640      MOVE -1                  TO MAINTL.                          00016400
01641      MOVE 2251                TO EMI-ERROR.                       00016410
01642      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    00016420
01643      GO TO 8100-SEND-INITIAL-MAP.                                 00016430
01644                                                                   00016440
01645      EJECT                                                        00016450
01646                                                                   00016460
01647  7000-PNDT-FILE-NOTOPEN.                                          00016470
01648      MOVE -1 TO                  MAINTL.                          00016480
01649      MOVE 2216                   TO EMI-ERROR.                    00016490
01650      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    00016500
01651      GO TO 8200-SEND-DATAONLY.                                    00016510
01652                                                                   00016520
01653                                                                   00016530
01654                                                                   00016540
01655  7100-CNTL-FILE-NOTOPEN.                                          00016550
01656      MOVE -1                     TO MAINTL.                       00016560
01657      MOVE 2214                   TO EMI-ERROR.                    00016570
01658      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    00016580
01659      GO TO 8200-SEND-DATAONLY.                                    00016590
01660                                                                   00016600
01661                                                                   00016610
01662      EJECT                                                        00016620
01663                                                                   00016630
01664  8100-SEND-INITIAL-MAP.                                           00016640
01665      MOVE WS-CURRENT-DT          TO DATEO.                        00016650
01666      MOVE EIBTIME                TO TIME-IN.                      00016660
01667      MOVE TIME-OUT               TO TIMEO.                        00016670
01668      MOVE -1                     TO MAINTL.                       00016680
01669      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.                     00016690
01670      MOVE EMI-MESSAGE-AREA (2)   TO ERRMSG2O.                     00016700
01671                                                                   00016710
01672      MOVE PI-LIFE-OVERRIDE-L2    TO WS-PRM-OVERRIDE               00016720
01673                                     WS-REFUND-OVERRIDE.           00016730
01674      MOVE WS-PRM-HEADER          TO LFPHDGO.                      00016740
01675      MOVE WS-REFUND-HEADER       TO LFRHDGO.                      00016750
01676                                                                   00016760
01677      MOVE PI-AH-OVERRIDE-L2      TO WS-PRM-OVERRIDE               00016770
01678                                     WS-REFUND-OVERRIDE.           00016780
01679      MOVE WS-PRM-HEADER          TO AHPHDGO.                      00016790
01680      MOVE WS-REFUND-HEADER       TO AHRHDGO.                      00016800
01681                                                                   00016810
01682                                                                   00016820
01683      EXEC CICS SEND                                               00016830
01684          MAP      (MAP-NAME)                                      00016840
01685          MAPSET   (MAPSET-NAME)                                   00016850
01686          FROM     (EL930AO)                                       00016860
01687          ERASE                                                    00016870
01688          CURSOR                                                   00016880
01689          END-EXEC.                                                00016890
01690      GO TO 9100-RETURN-TRAN.                                      00016900
01691                                                                   00016910
01692      EJECT                                                        00016920
01693                                                                   00016930
01694  8200-SEND-DATAONLY.                                              00016940
01695      MOVE WS-CURRENT-DT          TO DATEO.                        00016950
01696      MOVE EIBTIME                TO TIME-IN.                      00016960
01697      MOVE TIME-OUT               TO TIMEO.                        00016970
01698      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.                     00016980
01699      MOVE EMI-MESSAGE-AREA (2)   TO ERRMSG2O.                     00016990
01700      EXEC CICS SEND                                               00017000
01701          MAP      (MAP-NAME)                                      00017010
01702          MAPSET   (MAPSET-NAME)                                   00017020
01703          FROM     (EL930AO)                                       00017030
01704          DATAONLY                                                 00017040
01705          ERASEAUP                                                 00017050
01706          CURSOR                                                   00017060
01707          END-EXEC.                                                00017070
01708      GO TO 9100-RETURN-TRAN.                                      00017080
01709                                                                   00017090
01710  8300-SEND-TEXT.                                                  00017100
01711      EXEC CICS SEND TEXT                                          00017110
01712          FROM     (LOGOFF-TEXT)                                   00017120
01713          LENGTH   (LOGOFF-LENGTH)                                 00017130
01714          ERASE                                                    00017140
01715          FREEKB                                                   00017150
01716          END-EXEC.                                                00017160
01717      EXEC CICS RETURN                                             00017170
01718          END-EXEC.                                                00017180
01719      EJECT                                                        00017190
01720  8400-LOG-JOURNAL-RECORD.                                         00017200
01721      MOVE PI-PROCESSOR-ID        TO JP-USER-ID.                   00017210
01722      MOVE ERPNDT-FILE-ID         TO JP-FILE-ID.                   00017220
01723      MOVE THIS-PGM               TO JP-PROGRAM-ID.                00017230
01724                                                                   00017240
01725 *    EXEC CICS JOURNAL                                            00017250
01726 *        JFILEID     (PI-JOURNAL-FILE-ID)                         00017260
01727 *        JTYPEID     ('EL')                                       00017270
01728 *        FROM        (JOURNAL-RECORD)                             00017280
01729 *        LENGTH      (WS-JOURNAL-RECORD-LENGTH)                   00017290
01730 *        END-EXEC.                                                00017300
01731                                                                   00017310
01732                                                                   00017320
01733                                                                   00017330
01734                                                                   00017340
01735  8600-DEEDIT.                                                     00017350
01736                                                                   00017360
01737      EXEC CICS BIF DEEDIT                                         00017370
01738          FIELD(WS-DEEDIT-FIELD)                                   00017380
01739          LENGTH(10)                                               00017390
01740          END-EXEC.                                                00017400
01741  8600-EXIT.                                                       00017410
01742      EXIT.                                                        00017420
01743      EJECT                                                        00017430
01744  8800-UNAUTHORIZED-ACCESS.                                        00017440
01745      MOVE UNACCESS-MSG           TO LOGOFF-MSG.                   00017450
01746      GO TO 8300-SEND-TEXT.                                        00017460
01747                                                                   00017470
01748  8810-PF23.                                                       00017480
01749                                                                   00017490
01750      IF NOT PI-DATA-UPDATED                                       00017500
01751          NEXT SENTENCE                                            00017510
01752      ELSE                                                         00017520
01753          MOVE -1                 TO PFENTERL                      00017530
01754          MOVE ER-2213            TO EMI-ERROR                     00017540
01755          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 00017550
01756          GO TO 4000-SHOW-TOTALS.                                  00017560
01757                                                                   00017570
01758      MOVE EIBAID                 TO PI-ENTRY-CD-1.                00017580
01759      MOVE XCTL-005               TO PGM-NAME.                     00017590
01760      GO TO 9300-XCTL.                                             00017600
01761  9000-RETURN-CICS.                                                00017610
01762      EXEC CICS RETURN                                             00017620
01763          END-EXEC.                                                00017630
01764                                                                   00017640
01765  9100-RETURN-TRAN.                                                00017650
01766      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.             00017660
01767      MOVE SCREEN-NUMBER          TO PI-CURRENT-SCREEN-NO.         00017670
01768      EXEC CICS RETURN                                             00017680
01769          TRANSID    (TRANS-ID)                                    00017690
01770          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     00017700
01771          LENGTH     (PI-COMM-LENGTH)                              00017710
01772          END-EXEC.                                                00017720
01773                                                                   00017730
01774  9200-RETURN-MAIN-MENU.                                           00017740
01775                                                                   00017750
01776      IF NOT PI-DATA-UPDATED                                       00017760
01777          NEXT SENTENCE                                            00017770
01778      ELSE                                                         00017780
01779          MOVE -1                 TO PFENTERL                      00017790
01780          MOVE ER-2213            TO EMI-ERROR                     00017800
01781          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 00017810
01782          GO TO 4000-SHOW-TOTALS.                                  00017820
01783                                                                   00017830
01784      MOVE XCTL-626               TO PGM-NAME.                     00017840
01785      GO TO 9300-XCTL.                                             00017850
01786                                                                   00017860
01787  9300-XCTL.                                                       00017870
01788      EXEC CICS XCTL                                               00017880
01789          PROGRAM    (PGM-NAME)                                    00017890
01790          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     00017900
01791          LENGTH     (PI-COMM-LENGTH)                              00017910
01792          END-EXEC.                                                00017920
01793                                                                   00017930
01794  9400-CLEAR.                                                      00017940
01795      IF NOT PI-DATA-UPDATED                                       00017950
01796          NEXT SENTENCE                                            00017960
01797      ELSE                                                         00017970
01798          MOVE -1                 TO PFENTERL                      00017980
01799          MOVE ER-2213            TO EMI-ERROR                     00017990
01800          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 00018000
01801          GO TO 4000-SHOW-TOTALS.                                  00018010
01802      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME.                     00018020
01803      GO TO 9300-XCTL.                                             00018030
01804                                                                   00018040
01805  9500-PF12.                                                       00018050
01806      MOVE XCTL-010               TO PGM-NAME.                     00018060
01807      GO TO 9300-XCTL.                                             00018070
01808                                                                   00018080
01809  9600-PGMID-ERROR.                                                00018090
01810      EXEC CICS HANDLE CONDITION                                   00018100
01811          PGMIDERR    (8300-SEND-TEXT)                             00018110
01812          END-EXEC.                                                00018120
01813      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.           00018130
01814      MOVE ' '                    TO PI-ENTRY-CD-1.                00018140
01815      MOVE XCTL-005               TO PGM-NAME.                     00018150
01816      MOVE PGM-NAME               TO LOGOFF-PGM.                   00018160
01817      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.                  00018170
01818      GO TO 9300-XCTL.                                             00018180
01819                                                                   00018190
01820  9700-DATE-LINK.                                                  00018200
01821      MOVE LINK-CLDATCV           TO PGM-NAME.                     00018210
01822      EXEC CICS LINK                                               00018220
01823          PROGRAM    (PGM-NAME)                                    00018230
01824          COMMAREA   (DATE-CONVERSION-DATA)                        00018240
01825          LENGTH     (DC-COMM-LENGTH)                              00018250
01826          END-EXEC.                                                00018260
01827                                                                   00018270
01828                                                                   00018280
01829  9900-ERROR-FORMAT.                                               00018290
01830      IF NOT EMI-ERRORS-COMPLETE                                   00018300
01831          MOVE LINK-001           TO PGM-NAME                      00018310
01832          EXEC CICS LINK                                           00018320
01833              PROGRAM    (PGM-NAME)                                00018330
01834              COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)           00018340
01835              LENGTH     (EMI-COMM-LENGTH)                         00018350
01836              END-EXEC.                                            00018360
01837  9900-EXIT.                                                       00018370
01838      EXIT.                                                        00018380
01839                                                                   00018390
01840  9990-ABEND.                                                      00018400
01841      MOVE LINK-004               TO PGM-NAME.                     00018410
01842      MOVE DFHEIBLK               TO EMI-LINE1.                    00018420
01843      EXEC CICS LINK                                               00018430
01844          PROGRAM   (PGM-NAME)                                     00018440
01845          COMMAREA  (EMI-LINE1)                                    00018450
01846          LENGTH    (72)                                           00018460
01847          END-EXEC.                                                00018470
01848      MOVE -1                     TO PFENTERL.                     00018480
01849      GO TO 8200-SEND-DATAONLY.                                    00018490
01850      GOBACK.                                                      00018500
01851                                                                   00018510
01852  9995-SECURITY-VIOLATION.                                         00018520
01853                              COPY ELCSCTP.                        00018530
01854                                                                   00018540
01855  9995-EXIT.                                                       00018550
01856      EXIT.                                                        00018560
01857                                                                   00018570
01858                                                                   00018580
