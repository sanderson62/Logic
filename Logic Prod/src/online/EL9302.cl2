00001  ID DIVISION.                                                     00000010
00002                                                                   00000020
00003  PROGRAM-ID.                 EL9302.                              00000030
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
00020  REMARKS. TRANSACTION - EXI3 - NEW BUSINESS - BATCH BALANCE.      00000200
00021                                                                   00000210
00022  ENVIRONMENT DIVISION.                                            00000220
00023                                                                   00000230
00024      EJECT                                                        00000240
00025  DATA DIVISION.                                                   00000250
00026  WORKING-STORAGE SECTION.                                         00000260
00027  77  FILLER  PIC X(32)  VALUE '********************************'. 00000270
00028  77  FILLER  PIC X(32)  VALUE '*    EL9302 WORKING STORAGE    *'. 00000280
00029  77  FILLER  PIC X(32)  VALUE '************ V/M 2.001 *********'. 00000290
00030                                                                   00000300
00031  01  STANDARD-AREAS.                                              00000310
00032      12  MAP-NAME            PIC X(8)    VALUE 'EL930D '.         00000320
00033      12  MAPSET-NAME         PIC X(8)    VALUE 'EL9302S '.        00000330
00034      12  SCREEN-NUMBER       PIC X(4)    VALUE '930C'.            00000340
00035      12  TRANS-ID            PIC X(4)    VALUE 'EXI3'.            00000350
00036      12  THIS-PGM            PIC X(8)    VALUE 'EL9302  '.        00000360
00037      12  PGM-NAME            PIC X(8).                            00000370
00038      12  TIME-IN             PIC S9(7).                           00000380
00039      12  TIME-OUT-R  REDEFINES TIME-IN.                           00000390
00040          16  FILLER          PIC X.                               00000400
00041          16  TIME-OUT        PIC 99V99.                           00000410
00042          16  FILLER          PIC X(2).                            00000420
00043      12  XCTL-005            PIC X(8)    VALUE 'EL005   '.        00000430
00044      12  XCTL-010            PIC X(8)    VALUE 'EL010   '.        00000440
00045      12  XCTL-626            PIC X(8)    VALUE 'EL626   '.        00000450
00046      12  XCTL-930            PIC X(8)    VALUE 'EL930   '.        00000460
00047      12  XCTL-9301           PIC X(8)    VALUE 'EL9301  '.        00000470
00048      12  LINK-001            PIC X(8)    VALUE 'EL001   '.        00000480
00049      12  LINK-004            PIC X(8)    VALUE 'EL004   '.        00000490
00050      12  LINK-ELDATCV        PIC X(8)    VALUE 'ELDATCV '.        00000500
00051      12  ERPNDT-FILE-ID      PIC X(8)    VALUE 'ERPNDT  '.        00000510
00052      12  ELCNTL-FILE-ID      PIC X(8)    VALUE 'ELCNTL  '.        00000520
00053      12  DATA-UPDATE-SW      PIC X       VALUE SPACES.            00000530
00054          88  DATA-CHANGED                      VALUE 'Y'.         00000540
00055      12  ERROR-SW            PIC X       VALUE SPACES.            00000550
00056          88  EDIT-ERRORS                       VALUE 'Y'.         00000560
00057      12  WS-DEEDIT-FIELD     PIC S9(7)V99.                        00000570
00058      12  WS-MIN-PREMIUM      PIC S9(3)V99 COMP-3 VALUE ZEROS.     00000580
00059      12  WS-CURRENT-DT       PIC X(8)    VALUE SPACES.            00000590
00060      12  WS-CURRENT-BIN-DT   PIC XX      VALUE SPACES.            00000600
00061      12  WS-PRM-HEADER.                                           00000610
00062          16  WS-PRM-OVERRIDE     PIC XX    VALUE SPACES.          00000620
00063          16  FILLER              PIC X(8)  VALUE '-PREMIUM'.      00000630
00064      12  WS-REFUND-HEADER.                                        00000640
00065          16  WS-REFUND-OVERRIDE  PIC XX    VALUE SPACES.          00000650
00066          16  FILLER              PIC X(7)  VALUE '-REFUND'.       00000660
00067                                                                   00000670
00068      12  ER-0002                 PIC X(4)  VALUE '0002'.          00000680
00069      12  ER-0004                 PIC X(4)  VALUE '0004'.          00000690
00070      12  ER-0008                 PIC X(4)  VALUE '0008'.          00000700
00071      12  ER-0029                 PIC X(4)  VALUE '0029'.          00000710
00072      12  ER-0070                 PIC X(4)  VALUE '0070'.          00000720
00073      12  ER-2212                 PIC X(4)  VALUE '2122'.          00000730
00074      12  ER-2233                 PIC X(4)  VALUE '2233'.          00000740
00075      12  ER-2237                 PIC X(4)  VALUE '2237'.          00000750
00076      12  ER-2238                 PIC X(4)  VALUE '2238'.          00000760
00077      12  ER-2433                 PIC X(4)  VALUE '2433'.          00000770
00078                                                                   00000780
00079  01  ACCESS-KEYS.                                                 00000790
00080      12  ELCNTL-KEY.                                              00000800
00081          16  CNTL-COMP-ID        PIC X(3)  VALUE SPACES.          00000810
00082          16  CNTL-REC-TYPE       PIC X     VALUE '1'.             00000820
00083          16  CNTL-ACCESS         PIC X(4)  VALUE SPACES.          00000830
00084          16  CNTL-SEQ            PIC S9(4) VALUE +0 COMP.         00000840
00085      12  ERPNDT-KEY.                                              00000850
00086          16  PNDT-COMP-CD        PIC X     VALUE SPACE.           00000860
00087          16  PNDT-ENTRY-BATCH    PIC X(6)  VALUE SPACES.          00000870
00088          16  PNDT-BATCH-SEQ      PIC S9(4) VALUE +0 COMP.         00000880
00089          16  PNDT-BATCH-CHG-SEQ  PIC S9(4) VALUE +0 COMP.         00000890
00090      12  ERPNDT-UPDATE-KEY.                                       00000900
00091          16  ERPNDT-COMP-CD      PIC X     VALUE SPACE.           00000910
00092          16  ERPNDT-ENTRY-BATCH  PIC X(6)  VALUE SPACES.          00000920
00093          16  ERPNDT-SEQ-NO       PIC S9(4) VALUE +0 COMP.         00000930
00094          16  ERPNDT-CHG-SEQ-NO   PIC S9(4) VALUE +0 COMP.         00000940
00095      EJECT                                                        00000950
00096                              COPY ELCDATE.                        00000951
00097                                                                   00000970
00098      EJECT                                                        00000980
00099                              COPY ELCLOGOF.                       00000981
00100                                                                   00001000
00101      EJECT                                                        00001010
00102                              COPY ELCATTR.                        00001011
00103                                                                   00001030
00104      EJECT                                                        00001040
00105                                    COPY ELCEMIB.                  00001041
00106                                                                   00001060
00107      EJECT                                                        00001070
00108                              COPY ELCINTF.                        00001071
00109      COPY ELC930PI.                                               00001090
00110                                                                   00001100
00111      EJECT                                                        00001110
00112                              COPY ELCJPFX.                        00001111
00113                              PIC X(585).                          00001130
00114                                                                   00001140
00115      EJECT                                                        00001150
00116                              COPY ELCAID.                         00001151
00117  01  FILLER    REDEFINES DFHAID.                                  00001170
00118      12  FILLER              PIC X(8).                            00001180
00119      12  PF-VALUES           PIC X       OCCURS 2.                00001190
00120                                                                   00001200
00121      EJECT                                                        00001210
00122                             COPY EL9302S.                         00001211
00123  01  MAP-IN REDEFINES EL930DI.                                    00001230
00124      12  FILLER                  PIC X(111).                      00001240
00125      12  DATA-IN.                                                 00001250
00126          16  FILLER           OCCURS 16 TIMES                     00001260
00127                               INDEXED BY INDX.                    00001270
00128              20  SEQ-LEN             PIC S9(4)  COMP.             00001280
00129              20  SEQ-ATTRB           PIC X.                       00001290
00130              20  SEQ                 PIC 9(4).                    00001300
00131              20  CERT-LEN            PIC S9(4)  COMP.             00001310
00132              20  CERT-ATTRB          PIC X.                       00001320
00133              20  CERT                PIC X(11).                   00001330
00134              20  LFPREM-LEN          PIC S9(4)  COMP.             00001340
00135              20  LFPREM-ATTRB        PIC X.                       00001350
00136              20  LFPREM              PIC S9(9)V99.                00001360
00137              20  ALT-LFPREM-LEN      PIC S9(4)  COMP.             00001370
00138              20  ALT-LFPREM-ATTRB    PIC X.                       00001380
00139              20  ALT-LFPREM          PIC S9(9)V99.                00001390
00140              20  AHPREM-LEN          PIC S9(4)  COMP.             00001400
00141              20  AHPREM-ATTRB        PIC X.                       00001410
00142              20  AHPREM              PIC S9(9)V99.                00001420
00143              20  LFCANCEL-LEN        PIC S9(4)  COMP.             00001430
00144              20  LFCANCEL-ATTRB      PIC X.                       00001440
00145              20  LFCANCEL            PIC S9(9)V99.                00001450
00146              20  AHCANCEL-LEN        PIC S9(4)  COMP.             00001460
00147              20  AHCANCEL-ATTRB      PIC X.                       00001470
00148              20  AHCANCEL            PIC S9(9)V99.                00001480
00149      12  DATA-OUT REDEFINES DATA-IN.                              00001490
00150          16  FILLER           OCCURS 16 TIMES                     00001500
00151                               INDEXED BY ONDX.                    00001510
00152              20  FILLER              PIC X(3).                    00001520
00153              20  SEQ-OUT             PIC 9(4).                    00001530
00154              20  FILLER              PIC X(3).                    00001540
00155              20  CERT-OUT            PIC X(11).                   00001550
00156              20  FILLER              PIC X(3).                    00001560
00157              20  LFPREM-ED           PIC Z(7).99-.                00001570
00158              20  FILLER              PIC X(3).                    00001580
00159              20  ALT-LFPREM-ED       PIC Z(7).99-.                00001590
00160              20  FILLER              PIC X(3).                    00001600
00161              20  AHPREM-ED           PIC Z(7).99-.                00001610
00162              20  FILLER              PIC X(3).                    00001620
00163              20  LFCANCEL-ED         PIC Z(7).99-.                00001630
00164              20  FILLER              PIC X(3).                    00001640
00165              20  AHCANCEL-ED         PIC Z(7).99-.                00001650
00166                                                                   00001660
00167      12  FILLER                  PIC X(40).                       00001670
00168      EJECT                                                        00001680
00169  LINKAGE SECTION.                                                 00001690
00170  01  DFHCOMMAREA             PIC X(1024).                         00001700
00171                                                                   00001710
00172      EJECT                                                        00001720
00173  01  PARMLIST.                                                    00001730
00174      02  FILLER              PIC S9(8)   COMP.                    00001740
00175      02  ERPNDT-POINTER      PIC S9(8)   COMP.                    00001750
00176      02  ELCNTL-POINTER      PIC S9(8)   COMP.                    00001760
00177                                                                   00001770
00178                              COPY ERCPNDB.                        00001771
00179      EJECT                                                        00001790
00180                              COPY ELCCNTL.                        00001791
00181      EJECT                                                        00001810
00182  PROCEDURE DIVISION.                                              00001820
00183      SERVICE RELOAD PARMLIST.                                     00001830
00184      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.      00001840
00185      IF EIBCALEN = 0                                              00001850
00186          GO TO 8800-UNAUTHORIZED-ACCESS.                          00001860
00187      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              00001870
00188      MOVE '5'                    TO DC-OPTION-CODE.               00001880
00189      MOVE LINK-ELDATCV           TO PGM-NAME                      00001890
00190                                                                   00001900
00191      EXEC CICS LINK                                               00001910
00192          PROGRAM  (PGM-NAME)                                      00001920
00193          COMMAREA (DATE-CONVERSION-DATA)                          00001930
00194          LENGTH   (DC-COMM-LENGTH)                                00001940
00195          END-EXEC.                                                00001950
00196      MOVE DC-BIN-DATE-1          TO WS-CURRENT-BIN-DT.            00001960
00197      MOVE DC-GREG-DATE-1-EDIT    TO WS-CURRENT-DT.                00001970
00198                                                                   00001980
00199      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         00001990
00200          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   00002000
00201              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6      00002010
00202              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5      00002020
00203              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4      00002030
00204              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3      00002040
00205              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2      00002050
00206              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1      00002060
00207              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM    00002070
00208              MOVE THIS-PGM TO PI-CALLING-PROGRAM                  00002080
00209          ELSE                                                     00002090
00210              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM      00002100
00211              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM    00002110
00212              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1      00002120
00213              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2      00002130
00214              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3      00002140
00215              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4      00002150
00216              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5      00002160
00217              MOVE SPACES               TO PI-SAVED-PROGRAM-6.     00002170
00218                                                                   00002180
00219      MOVE LOW-VALUES             TO EL930DI.                      00002190
00220                                                                   00002200
00221      IF EIBTRNID NOT = TRANS-ID                                   00002210
00222          MOVE ZEROS              TO PI-SAV-BATCH-SEQ              00002220
00223                                     PI-SAV-BATCH-CHG-SEQ          00002230
00224          MOVE PI-COMPANY-CD      TO ERPNDT-COMP-CD                00002240
00225          MOVE PI-SAV-ENTRY-BATCH TO ERPNDT-ENTRY-BATCH            00002250
00226          MOVE DFHENTER           TO EIBAID                        00002260
00227          GO TO 2000-BROWSE-FORWARD.                               00002270
00228                                                                   00002280
00229      EXEC CICS HANDLE CONDITION                                   00002290
00230          PGMIDERR  (9600-PGMID-ERROR)                             00002300
00231          ERROR     (9990-ABEND)                                   00002310
00232          END-EXEC.                                                00002320
00233                                                                   00002330
00234      IF EIBAID = DFHCLEAR                                         00002340
00235          GO TO 9400-CLEAR.                                        00002350
00236                                                                   00002360
00237      EJECT                                                        00002370
00238  0200-RECEIVE.                                                    00002380
00239      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       00002390
00240          MOVE ER-0008            TO EMI-ERROR                     00002400
00241          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 00002410
00242          MOVE -1                 TO PFENTERL                      00002420
00243          GO TO 8200-SEND-DATAONLY.                                00002430
00244      EXEC CICS RECEIVE                                            00002440
00245          MAP      (MAP-NAME)                                      00002450
00246          MAPSET   (MAPSET-NAME)                                   00002460
00247          INTO     (EL930DI)                                       00002470
00248          END-EXEC.                                                00002480
00249                                                                   00002490
00250      IF PFENTERL = 0                                              00002500
00251          GO TO 0300-CHECK-PFKEYS.                                 00002510
00252      IF EIBAID NOT = DFHENTER                                     00002520
00253          MOVE ER-0004            TO EMI-ERROR                     00002530
00254          GO TO 0320-INPUT-ERROR.                                  00002540
00255      IF (PFENTERI NUMERIC)                                        00002550
00256        AND (PFENTERI GREATER THAN 0 AND LESS THAN 25)             00002560
00257          MOVE PF-VALUES (PFENTERI) TO EIBAID                      00002570
00258      ELSE                                                         00002580
00259          MOVE ER-0029            TO EMI-ERROR                     00002590
00260          GO TO 0320-INPUT-ERROR.                                  00002600
00261                                                                   00002610
00262  0300-CHECK-PFKEYS.                                               00002620
00263      IF EIBAID = DFHPF12                                          00002630
00264          GO TO 9500-PF12.                                         00002640
00265      IF EIBAID = DFHENTER                                         00002650
00266        OR DFHPF1                                                  00002660
00267        OR DFHPF2                                                  00002670
00268          GO TO 1000-EDIT-DATA.                                    00002680
00269  0320-INPUT-ERROR.                                                00002690
00270      MOVE ER-0029                TO EMI-ERROR.                    00002700
00271      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    00002710
00272      MOVE AL-UNBON               TO PFENTERA.                     00002720
00273      MOVE -1                     TO PFENTERL.                     00002730
00274      GO TO 8200-SEND-DATAONLY.                                    00002740
00275                                                                   00002750
00276      EJECT                                                        00002760
00277  1000-EDIT-DATA.                                                  00002770
00278      MOVE PI-COMPANY-CD          TO ERPNDT-COMP-CD.               00002780
00279      MOVE PI-SAV-ENTRY-BATCH     TO ERPNDT-ENTRY-BATCH.           00002790
00280      PERFORM 1200-ELCNTL-READ THRU 1290-EXIT.                     00002800
00281      IF EMI-ERROR NOT = ZEROS                                     00002810
00282          GO TO 8200-SEND-DATAONLY.                                00002820
00283      SET INDX                    TO 1.                            00002830
00284  1100-EDIT-LOOP.                                                  00002840
00285                                                                   00002850
00286      IF LFPREM-LEN (INDX) NOT = ZEROS                             00002860
00287          MOVE LFPREM (INDX)      TO WS-DEEDIT-FIELD               00002870
00288          PERFORM 8600-DEEDIT THRU 8600-EXIT                       00002880
00289          MOVE WS-DEEDIT-FIELD    TO LFPREM (INDX)                 00002890
00290          MOVE 'Y'                TO DATA-UPDATE-SW.               00002900
00291                                                                   00002910
00292      IF ALT-LFPREM-LEN (INDX) NOT = ZEROS                         00002920
00293          MOVE ALT-LFPREM (INDX)  TO WS-DEEDIT-FIELD               00002930
00294          PERFORM 8600-DEEDIT THRU 8600-EXIT                       00002940
00295          MOVE WS-DEEDIT-FIELD    TO ALT-LFPREM (INDX)             00002950
00296          MOVE 'Y'                TO DATA-UPDATE-SW.               00002960
00297                                                                   00002970
00298      IF LFCANCEL-LEN (INDX) NOT = ZEROS                           00002980
00299          MOVE LFCANCEL (INDX)    TO WS-DEEDIT-FIELD               00002990
00300          PERFORM 8600-DEEDIT THRU 8600-EXIT                       00003000
00301          MOVE WS-DEEDIT-FIELD    TO LFCANCEL (INDX)               00003010
00302          MOVE 'Y'                TO DATA-UPDATE-SW.               00003020
00303                                                                   00003030
00304      IF AHPREM-LEN (INDX) NOT = ZEROS                             00003040
00305          MOVE AHPREM (INDX)      TO WS-DEEDIT-FIELD               00003050
00306          PERFORM 8600-DEEDIT THRU 8600-EXIT                       00003060
00307          MOVE WS-DEEDIT-FIELD    TO AHPREM (INDX)                 00003070
00308          MOVE 'Y'                TO DATA-UPDATE-SW.               00003080
00309                                                                   00003090
00310      IF AHCANCEL-LEN (INDX) NOT = ZEROS                           00003100
00311          MOVE AHCANCEL (INDX)    TO WS-DEEDIT-FIELD               00003110
00312          PERFORM 8600-DEEDIT THRU 8600-EXIT                       00003120
00313          MOVE WS-DEEDIT-FIELD    TO AHCANCEL (INDX)               00003130
00314          MOVE 'Y'                TO DATA-UPDATE-SW.               00003140
00315                                                                   00003150
00316      IF DATA-CHANGED AND EMI-ERROR = ZEROS                        00003160
00317          PERFORM 5000-UPDATE-PNDT-FILE THRU 5990-EXIT.            00003170
00318                                                                   00003180
00319      MOVE SPACE                  TO DATA-UPDATE-SW.               00003190
00320                                                                   00003200
00321      IF EMI-ERROR NOT = ZEROS                                     00003210
00322          MOVE 'Y'                TO ERROR-SW                      00003220
00323          MOVE ZEROS              TO EMI-ERROR.                    00003230
00324                                                                   00003240
00325      IF INDX LESS THAN 16                                         00003250
00326          SET INDX UP BY 1                                         00003260
00327          GO TO 1100-EDIT-LOOP.                                    00003270
00328                                                                   00003280
00329      IF EDIT-ERRORS                                               00003290
00330          GO TO 8200-SEND-DATAONLY.                                00003300
00331                                                                   00003310
00332      IF EIBAID = DFHPF1 OR DFHENTER                               00003320
00333          GO TO 2000-BROWSE-FORWARD                                00003330
00334      ELSE                                                         00003340
00335          GO TO 3000-BROWSE-BACKWARD.                              00003350
00336      EJECT                                                        00003360
00337  1200-ELCNTL-READ.                                                00003370
00338      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.                 00003380
00339      EXEC CICS HANDLE CONDITION                                   00003390
00340          NOTFND (1280-NO-RECORD)                                  00003400
00341          END-EXEC.                                                00003410
00342      EXEC CICS READ                                               00003420
00343          DATASET (ELCNTL-FILE-ID)                                 00003430
00344          SET (ELCNTL-POINTER)                                     00003440
00345          RIDFLD (ELCNTL-KEY)                                      00003450
00346          END-EXEC.                                                00003460
00347      SERVICE RELOAD CONTROL-FILE.                                 00003470
00348      MOVE CF-MIN-PREMIUM         TO WS-MIN-PREMIUM.               00003480
00349      GO TO 1290-EXIT.                                             00003490
00350                                                                   00003500
00351  1280-NO-RECORD.                                                  00003510
00352      MOVE ER-0002                TO EMI-ERROR.                    00003520
00353      MOVE -1                     TO PFENTERL.                     00003530
00354      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    00003540
00355                                                                   00003550
00356                                                                   00003560
00357  1290-EXIT.                                                       00003570
00358      EXIT.                                                        00003580
00359      EJECT                                                        00003590
00360  2000-BROWSE-FORWARD.                                             00003600
00361      IF PI-BATCH-EOF                                              00003610
00362          MOVE SPACE              TO PI-BATCH-EOF-SW               00003620
00363          GO TO 2100-SKIP-ADD.                                     00003630
00364      IF PI-SAV-BATCH-SEQ NOT = ZEROS                              00003640
00365          ADD +16                 TO PI-SAV-BATCH-SEQ.             00003650
00366                                                                   00003660
00367  2100-SKIP-ADD.                                                   00003670
00368      PERFORM 6000-PNDT-START-BROWSE THRU 6090-EXIT.               00003680
00369      IF EMI-ERROR NOT = ZEROS                                     00003690
00370          GO TO 8200-SEND-DATAONLY.                                00003700
00371      PERFORM 4000-FORMAT-SCREEN THRU 4990-EXIT.                   00003710
00372                                                                   00003720
00373      IF EMI-ERROR = ZEROS                                         00003730
00374          GO TO 8100-SEND-INITIAL-MAP                              00003740
00375      ELSE                                                         00003750
00376          GO TO 8200-SEND-DATAONLY.                                00003760
00377                                                                   00003770
00378                                                                   00003780
00379                                                                   00003790
00380                                                                   00003800
00381  3000-BROWSE-BACKWARD.                                            00003810
00382      SUBTRACT 16                 FROM PI-SAV-BATCH-SEQ.           00003820
00383      IF PI-SAV-BATCH-SEQ LESS THAN 1                              00003830
00384          MOVE +1                 TO PI-SAV-BATCH-SEQ              00003840
00385          MOVE ER-2238            TO EMI-ERROR                     00003850
00386          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 00003860
00387          MOVE ZEROS              TO EMI-ERROR.                    00003870
00388                                                                   00003880
00389      PERFORM 6000-PNDT-START-BROWSE THRU 6090-EXIT.               00003890
00390      IF EMI-ERROR NOT = ZEROS                                     00003900
00391          GO TO 8200-SEND-DATAONLY.                                00003910
00392      PERFORM 4000-FORMAT-SCREEN THRU 4990-EXIT.                   00003920
00393                                                                   00003930
00394      IF EMI-ERROR = ZEROS                                         00003940
00395          GO TO 8100-SEND-INITIAL-MAP                              00003950
00396      ELSE                                                         00003960
00397          GO TO 8200-SEND-DATAONLY.                                00003970
00398      EJECT                                                        00003980
00399  4000-FORMAT-SCREEN.                                              00003990
00400      MOVE LOW-VALUES             TO DATA-OUT.                     00004000
00401      SET ONDX                    TO 1.                            00004010
00402  4100-FORMAT-LOOP.                                                00004020
00403      PERFORM 6100-PNDT-READ-NEXT THRU 6190-EXIT.                  00004030
00404      IF PB-COMPANY-CD = PI-SAV-COMP-CD                            00004040
00405        AND PB-ENTRY-BATCH = PI-SAV-ENTRY-BATCH                    00004050
00406          NEXT SENTENCE                                            00004060
00407      ELSE                                                         00004070
00408          MOVE ER-2237            TO EMI-ERROR                     00004080
00409          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 00004090
00410          MOVE ZEROS              TO EMI-ERROR                     00004100
00411          PERFORM 6200-PNDT-END-BROWSE THRU 6290-EXIT              00004110
00412          PERFORM 7000-PROTECT-FIELDS VARYING INDX FROM ONDX       00004120
00413                              BY 1 UNTIL INDX IS GREATER THAN 16   00004130
00414          MOVE 'Y'                TO PI-BATCH-EOF-SW               00004140
00415          GO TO 4990-EXIT.                                         00004150
00416                                                                   00004160
00417      IF ONDX = 1                                                  00004170
00418          MOVE PB-BATCH-SEQ-NO    TO PI-SAV-BATCH-SEQ.             00004180
00419                                                                   00004190
00420      MOVE PB-BATCH-SEQ-NO        TO SEQ-OUT (ONDX).               00004200
00421      SET INDX                    TO ONDX.                         00004210
00422      MOVE AL-SANON               TO SEQ-ATTRB (INDX).             00004220
00423      MOVE PB-CERT-NO             TO CERT-OUT (ONDX).              00004230
00424      IF PB-ISSUE                                                  00004240
00425          NEXT SENTENCE                                            00004250
00426      ELSE                                                         00004260
00427          GO TO 4200-FORMAT-CANCEL.                                00004270
00428                                                                   00004280
00429      IF PB-I-LF-PREMIUM-AMT NOT = ZEROS                           00004290
00430          MOVE PB-I-LF-PREMIUM-AMT TO LFPREM-ED (ONDX).            00004300
00431                                                                   00004310
00432      IF PB-I-LF-ALT-PREMIUM-AMT NOT = ZEROS                       00004320
00433          MOVE PB-I-LF-ALT-PREMIUM-AMT TO ALT-LFPREM-ED (ONDX).    00004330
00434                                                                   00004340
00435                                                                   00004350
00436      IF PB-I-AH-PREMIUM-AMT NOT = ZEROS                           00004360
00437          MOVE PB-I-AH-PREMIUM-AMT TO AHPREM-ED (ONDX).            00004370
00438      MOVE AL-SANOF               TO LFCANCEL-ATTRB (INDX)         00004380
00439                                     AHCANCEL-ATTRB (INDX).        00004390
00440      GO TO 4300-INDX-CHECK.                                       00004400
00441                                                                   00004410
00442  4200-FORMAT-CANCEL.                                              00004420
00443      IF PB-C-LF-CANCEL-AMT NOT = ZEROS                            00004430
00444          MOVE PB-C-LF-CANCEL-AMT TO LFCANCEL-ED (ONDX).           00004440
00445      IF PB-C-AH-CANCEL-AMT NOT = ZEROS                            00004450
00446          MOVE PB-C-AH-CANCEL-AMT TO AHCANCEL-ED (ONDX).           00004460
00447      MOVE AL-SANOF               TO LFPREM-ATTRB (INDX)           00004470
00448                                     ALT-LFPREM-ATTRB (INDX)       00004480
00449                                     AHPREM-ATTRB (INDX).          00004490
00450                                                                   00004500
00451  4300-INDX-CHECK.                                                 00004510
00452      IF ONDX LESS THAN 16                                         00004520
00453          SET ONDX UP BY 1                                         00004530
00454          GO TO 4100-FORMAT-LOOP.                                  00004540
00455                                                                   00004550
00456      PERFORM 6200-PNDT-END-BROWSE THRU 6290-EXIT.                 00004560
00457                                                                   00004570
00458  4990-EXIT.                                                       00004580
00459      EXIT.                                                        00004590
00460      EJECT                                                        00004600
00461  5000-UPDATE-PNDT-FILE.                                           00004610
00462      MOVE SEQ (INDX)             TO ERPNDT-SEQ-NO.                00004620
00463      EXEC CICS HANDLE CONDITION                                   00004630
00464          NOTFND (5800-REC-NOTFND)                                 00004640
00465          END-EXEC.                                                00004650
00466                                                                   00004660
00467      EXEC CICS READ                                               00004670
00468          SET (ERPNDT-POINTER)                                     00004680
00469          DATASET (ERPNDT-FILE-ID)                                 00004690
00470          RIDFLD (ERPNDT-UPDATE-KEY)                               00004700
00471          UPDATE                                                   00004710
00472          END-EXEC.                                                00004720
00473      SERVICE RELOAD PENDING-BUSINESS.                             00004730
00474      MOVE 'B'                    TO JP-RECORD-TYPE.               00004740
00475      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.               00004750
00476      PERFORM 8400-LOG-JOURNAL-RECORD.                             00004760
00477                                                                   00004770
00478      IF NOT PB-ISSUE                                              00004780
00479          GO TO 5100-UPDATE-CANCEL.                                00004790
00480                                                                   00004800
00481      IF LFPREM-LEN (INDX) NOT = ZEROS                             00004810
00482          SUBTRACT PB-I-LF-PREMIUM-AMT FROM PI-LF-ISS-ENTERED      00004820
00483          ADD LFPREM (INDX)       TO PI-LF-ISS-ENTERED             00004830
00484          MOVE LFPREM (INDX)      TO PB-I-LF-PREMIUM-AMT.          00004840
00485                                                                   00004850
00486      IF  ALT-LFPREM-LEN (INDX) NOT = ZEROS                        00004860
00487          SUBTRACT PB-I-LF-ALT-PREMIUM-AMT FROM PI-LF-ISS-ENTERED  00004870
00488          ADD  ALT-LFPREM (INDX)   TO PI-LF-ISS-ENTERED            00004880
00489          MOVE ALT-LFPREM (INDX)   TO PB-I-LF-ALT-PREMIUM-AMT.     00004890
00490                                                                   00004900
00491                                                                   00004910
00492      IF AHPREM-LEN (INDX) NOT = ZEROS                             00004920
00493          SUBTRACT PB-I-AH-PREMIUM-AMT FROM PI-AH-ISS-ENTERED      00004930
00494          ADD AHPREM (INDX)       TO PI-AH-ISS-ENTERED             00004940
00495          MOVE AHPREM (INDX)      TO PB-I-AH-PREMIUM-AMT.          00004950
00496                                                                   00004960
00497      GO TO 5200-PNDT-REWRITE.                                     00004970
00498                                                                   00004980
00499  5100-UPDATE-CANCEL.                                              00004990
00500      IF LFCANCEL-LEN (INDX) NOT = ZEROS                           00005000
00501          SUBTRACT PB-C-LF-CANCEL-AMT FROM PI-LF-CAN-ENTERED       00005010
00502          ADD LFCANCEL (INDX)       TO PI-LF-CAN-ENTERED           00005020
00503          MOVE LFCANCEL (INDX)      TO PB-C-LF-CANCEL-AMT.         00005030
00504                                                                   00005040
00505      IF AHCANCEL-LEN (INDX) NOT = ZEROS                           00005050
00506          SUBTRACT PB-C-AH-CANCEL-AMT FROM PI-AH-CAN-ENTERED       00005060
00507          ADD AHCANCEL (INDX)       TO PI-AH-CAN-ENTERED           00005070
00508          MOVE AHCANCEL (INDX)      TO PB-C-AH-CANCEL-AMT.         00005080
00509                                                                   00005090
00510  5200-PNDT-REWRITE.                                               00005100
00511      MOVE PI-PROCESSOR-ID        TO PB-LAST-MAINT-BY.             00005110
00512      MOVE EIBTIME                TO PB-LAST-MAINT-HHMMSS.         00005120
00513      MOVE WS-CURRENT-BIN-DT      TO PB-LAST-MAINT-DT.             00005130
00514      MOVE 'C'                    TO JP-RECORD-TYPE.               00005140
00515      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.               00005150
00516                                                                   00005160
00517      EXEC CICS REWRITE                                            00005170
00518          DATASET(ERPNDT-FILE-ID)                                  00005180
00519          FROM(PENDING-BUSINESS)                                   00005190
00520          END-EXEC.                                                00005200
00521      MOVE 'Y'                    TO PI-UPDATE-SW.                 00005210
00522      PERFORM 8400-LOG-JOURNAL-RECORD.                             00005220
00523      GO TO 5990-EXIT.                                             00005230
00524                                                                   00005240
00525  5800-REC-NOTFND.                                                 00005250
00526      MOVE ER-2433                TO EMI-ERROR                     00005260
00527      MOVE -1                     TO LFPREM-LEN (INDX).            00005270
00528      MOVE AL-SANOF               TO SEQ-ATTRB (INDX).             00005280
00529      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    00005290
00530                                                                   00005300
00531  5990-EXIT.                                                       00005310
00532      EXIT.                                                        00005320
00533      EJECT                                                        00005330
00534  6000-PNDT-START-BROWSE.                                          00005340
00535      MOVE PI-COMPANY-CD          TO PNDT-COMP-CD.                 00005350
00536      MOVE PI-SAV-ENTRY-BATCH     TO PNDT-ENTRY-BATCH.             00005360
00537      MOVE PI-SAV-BATCH-SEQ       TO PNDT-BATCH-SEQ.               00005370
00538      EXEC CICS HANDLE CONDITION                                   00005380
00539          NOTFND (6010-REC-NOT-FND)                                00005390
00540          END-EXEC.                                                00005400
00541      EXEC CICS STARTBR                                            00005410
00542          DATASET(ERPNDT-FILE-ID)                                  00005420
00543          RIDFLD(ERPNDT-KEY)                                       00005430
00544          END-EXEC.                                                00005440
00545      GO TO 6090-EXIT.                                             00005450
00546                                                                   00005460
00547  6010-REC-NOT-FND.                                                00005470
00548      MOVE -1                     TO PFENTERL.                     00005480
00549      MOVE ER-2212                TO EMI-ERROR.                    00005490
00550      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    00005500
00551                                                                   00005510
00552  6090-EXIT.                                                       00005520
00553      EXIT.                                                        00005530
00554      EJECT                                                        00005540
00555  6100-PNDT-READ-NEXT.                                             00005550
00556      EXEC CICS HANDLE CONDITION                                   00005560
00557          ENDFILE (6110-END-OF-FILE)                               00005570
00558          END-EXEC.                                                00005580
00559      EXEC CICS READNEXT                                           00005590
00560          SET (ERPNDT-POINTER)                                     00005600
00561          DATASET (ERPNDT-FILE-ID)                                 00005610
00562          RIDFLD (ERPNDT-KEY)                                      00005620
00563          END-EXEC.                                                00005630
00564      SERVICE RELOAD PENDING-BUSINESS.                             00005640
00565      IF PB-BATCH-TRAILER                                          00005650
00566          MOVE HIGH-VALUES        TO PB-CONTROL-PRIMARY.           00005660
00567      GO TO 6190-EXIT.                                             00005670
00568                                                                   00005680
00569  6110-END-OF-FILE.                                                00005690
00570      MOVE HIGH-VALUES            TO PB-CONTROL-PRIMARY.           00005700
00571                                                                   00005710
00572  6190-EXIT.                                                       00005720
00573      EXIT.                                                        00005730
00574      EJECT                                                        00005740
00575  6200-PNDT-END-BROWSE.                                            00005750
00576      EXEC CICS ENDBR                                              00005760
00577          DATASET (ERPNDT-FILE-ID)                                 00005770
00578          END-EXEC.                                                00005780
00579                                                                   00005790
00580  6290-EXIT.                                                       00005800
00581      EXIT.                                                        00005810
00582      EJECT                                                        00005820
00583  7000-PROTECT-FIELDS.                                             00005830
00584      MOVE AL-SANOF               TO SEQ-ATTRB (INDX)              00005840
00585                                     LFPREM-ATTRB (INDX)           00005850
00586                                     AHPREM-ATTRB (INDX)           00005860
00587                                     ALT-LFPREM-ATTRB (INDX)       00005870
00588                                     LFCANCEL-ATTRB (INDX)         00005880
00589                                     AHCANCEL-ATTRB (INDX).        00005890
00590      EJECT                                                        00005900
00591  8100-SEND-INITIAL-MAP.                                           00005910
00592      MOVE PI-SAV-ENTRY-BATCH     TO BATCHO.                       00005920
00593      MOVE WS-CURRENT-DT          TO DATEO.                        00005930
00594      MOVE EIBTIME                TO TIME-IN.                      00005940
00595      MOVE TIME-OUT               TO TIMEO.                        00005950
00596      MOVE -1                     TO LFPRE1L.                      00005960
00597      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO.                      00005970
00598                                                                   00005980
00599      MOVE PI-LIFE-OVERRIDE-L2    TO WS-PRM-OVERRIDE               00005990
00600                                     WS-REFUND-OVERRIDE.           00006000
00601      MOVE WS-PRM-HEADER          TO LFPMHDO.                      00006010
00602      MOVE WS-REFUND-HEADER       TO LFRFHDO.                      00006020
00603                                                                   00006030
00604      MOVE PI-AH-OVERRIDE-L2      TO WS-PRM-OVERRIDE               00006040
00605                                     WS-REFUND-OVERRIDE.           00006050
00606      MOVE WS-PRM-HEADER          TO AHPMHDO.                      00006060
00607      MOVE WS-REFUND-HEADER       TO AHRFHDO.                      00006070
00608                                                                   00006080
00609      EXEC CICS SEND                                               00006090
00610          MAP      (MAP-NAME)                                      00006100
00611          MAPSET   (MAPSET-NAME)                                   00006110
00612          FROM     (EL930DO)                                       00006120
00613          ERASE                                                    00006130
00614          CURSOR                                                   00006140
00615          END-EXEC.                                                00006150
00616      GO TO 9100-RETURN-TRAN.                                      00006160
00617                                                                   00006170
00618  8200-SEND-DATAONLY.                                              00006180
00619      MOVE WS-CURRENT-DT          TO DATEO.                        00006190
00620      MOVE EIBTIME                TO TIME-IN.                      00006200
00621      MOVE TIME-OUT               TO TIMEO.                        00006210
00622      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO.                      00006220
00623      EXEC CICS SEND                                               00006230
00624          MAP      (MAP-NAME)                                      00006240
00625          MAPSET   (MAPSET-NAME)                                   00006250
00626          FROM     (EL930DO)                                       00006260
00627          DATAONLY                                                 00006270
00628          ERASEAUP                                                 00006280
00629          CURSOR                                                   00006290
00630          END-EXEC.                                                00006300
00631      GO TO 9100-RETURN-TRAN.                                      00006310
00632                                                                   00006320
00633  8300-SEND-TEXT.                                                  00006330
00634      EXEC CICS SEND TEXT                                          00006340
00635          FROM     (LOGOFF-TEXT)                                   00006350
00636          LENGTH   (LOGOFF-LENGTH)                                 00006360
00637          ERASE                                                    00006370
00638          FREEKB                                                   00006380
00639          END-EXEC.                                                00006390
00640      EXEC CICS RETURN                                             00006400
00641          END-EXEC.                                                00006410
00642                                                                   00006420
00643  8400-LOG-JOURNAL-RECORD.                                         00006430
00644      MOVE PI-PROCESSOR-ID        TO JP-USER-ID.                   00006440
00645      MOVE ERPNDT-FILE-ID         TO JP-FILE-ID.                   00006450
00646      MOVE THIS-PGM               TO JP-PROGRAM-ID.                00006460
pemuni*    EXEC CICS JOURNAL                                            00006470
pemuni*        JFILEID     (PI-JOURNAL-FILE-ID)                         00006480
pemuni*        JTYPEID     ('CL')                                       00006490
pemuni*        FROM        (JOURNAL-RECORD)                             00006500
pemuni*        LENGTH      (503)                                        00006510
pemuni*        END-EXEC.                                                00006520
00653                                                                   00006530
00654                                                                   00006540
00655                                                                   00006550
00656                                                                   00006560
00657  8600-DEEDIT.                                                     00006570
00658                                                                   00006580
00659      EXEC CICS BIF DEEDIT                                         00006590
00660          FIELD(WS-DEEDIT-FIELD)                                   00006600
00661          LENGTH(09)                                               00006610
00662          END-EXEC.                                                00006620
00663  8600-EXIT.                                                       00006630
00664      EXIT.                                                        00006640
00665  8800-UNAUTHORIZED-ACCESS.                                        00006650
00666      MOVE UNACCESS-MSG           TO LOGOFF-MSG.                   00006660
00667      GO TO 8300-SEND-TEXT.                                        00006670
00668                                                                   00006680
00669  8810-PF23.                                                       00006690
00670      MOVE EIBAID                 TO PI-ENTRY-CD-1.                00006700
00671      MOVE XCTL-005               TO PGM-NAME.                     00006710
00672      GO TO 9300-XCTL.                                             00006720
00673  9000-RETURN-CICS.                                                00006730
00674      EXEC CICS RETURN                                             00006740
00675          END-EXEC.                                                00006750
00676                                                                   00006760
00677  9100-RETURN-TRAN.                                                00006770
00678      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.             00006780
00679      MOVE SCREEN-NUMBER          TO PI-CURRENT-SCREEN-NO.         00006790
00680      EXEC CICS RETURN                                             00006800
00681          TRANSID    (TRANS-ID)                                    00006810
00682          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     00006820
00683          LENGTH     (PI-COMM-LENGTH)                              00006830
00684          END-EXEC.                                                00006840
00685                                                                   00006850
00686  9200-RETURN-MAIN-MENU.                                           00006860
00687      MOVE XCTL-626               TO PGM-NAME.                     00006870
00688      GO TO 9300-XCTL.                                             00006880
00689                                                                   00006890
00690  9300-XCTL.                                                       00006900
00691      EXEC CICS XCTL                                               00006910
00692          PROGRAM    (PGM-NAME)                                    00006920
00693          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     00006930
00694          LENGTH     (PI-COMM-LENGTH)                              00006940
00695          END-EXEC.                                                00006950
00696                                                                   00006960
00697  9400-CLEAR.                                                      00006970
00698      MOVE XCTL-930               TO PGM-NAME.                     00006980
00699      GO TO 9300-XCTL.                                             00006990
00700                                                                   00007000
00701  9500-PF12.                                                       00007010
00702      MOVE XCTL-010               TO PGM-NAME.                     00007020
00703      GO TO 9300-XCTL.                                             00007030
00704                                                                   00007040
00705  9600-PGMID-ERROR.                                                00007050
00706      EXEC CICS HANDLE CONDITION                                   00007060
00707          PGMIDERR    (8300-SEND-TEXT)                             00007070
00708          END-EXEC.                                                00007080
00709      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.           00007090
00710      MOVE ' '                    TO PI-ENTRY-CD-1.                00007100
00711      MOVE XCTL-005               TO PGM-NAME.                     00007110
00712      MOVE PGM-NAME               TO LOGOFF-PGM.                   00007120
00713      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.                  00007130
00714      GO TO 9300-XCTL.                                             00007140
00715                                                                   00007150
00716  9900-ERROR-FORMAT.                                               00007160
00717      IF NOT EMI-ERRORS-COMPLETE                                   00007170
00718          MOVE LINK-001           TO PGM-NAME                      00007180
00719          EXEC CICS LINK                                           00007190
00720              PROGRAM    (PGM-NAME)                                00007200
00721              COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)           00007210
00722              LENGTH     (EMI-COMM-LENGTH)                         00007220
00723              END-EXEC.                                            00007230
00724  9900-EXIT.                                                       00007240
00725      EXIT.                                                        00007250
00726                                                                   00007260
00727  9990-ABEND.                                                      00007270
00728      MOVE LINK-004               TO PGM-NAME.                     00007280
00729      MOVE DFHEIBLK               TO EMI-LINE1                     00007290
00730      EXEC CICS LINK                                               00007300
00731          PROGRAM   (PGM-NAME)                                     00007310
00732          COMMAREA  (EMI-LINE1)                                    00007320
00733          LENGTH    (72)                                           00007330
00734          END-EXEC.                                                00007340
00735      MOVE -1                     TO PFENTERL.                     00007350
00736      GO TO 8200-SEND-DATAONLY.                                    00007360
00737      GOBACK.                                                      00007370
00738                                                                   00007380
00739                                                                   00007390
