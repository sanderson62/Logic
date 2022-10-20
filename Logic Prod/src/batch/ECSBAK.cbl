       IDENTIFICATION DIVISION.
       PROGRAM-ID. ECSBAK.
       AUTHOR.     PABLO.
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

00024      SELECT  OLD-CERT            ASSIGN TO SYS010.
00025      SELECT  NEW-CERT            ASSIGN TO SYS011.
00026      SELECT  CLAIM-FILE          ASSIGN TO SYS012.
00027      SELECT  DISK-DATE           ASSIGN TO SYS019.
00028      SELECT  SORT-FILE           ASSIGN TO SORTWK1.
00029                                                                   00000290
00030  DATA DIVISION.                                                   00000300
00031  FILE SECTION.                                                    00000310
00032                                                                   00000320
00033  FD  OLD-CERT                                                     00000330
00034      RECORDING MODE IS F                                          00000340
00035      LABEL RECORDS ARE STANDARD                                   00000350
00036      RECORD CONTAINS 1056 CHARACTERS                              00000360
00037      BLOCK CONTAINS 0 RECORDS                                     00000370
00038      DATA RECORD IS CERTIFICATE-RECORD.                           00000380
00039                                  COPY ECSCRT01.
00040  SKIP3                                                            00000400
00041  FD  NEW-CERT                                                     00000410
00042      RECORDING MODE IS F                                          00000420
00043      LABEL RECORDS ARE STANDARD                                   00000430
00044      RECORD CONTAINS 1056 CHARACTERS                              00000440
00045      BLOCK CONTAINS 0 RECORDS                                     00000450
00046      DATA RECORD IS NEW-REC.                                      00000460
00047  01  NEW-REC                     PIC X(1056).
00048  SKIP3                                                            00000480
00049  FD  CLAIM-FILE                                                   00000490
00050      RECORDING MODE F                                             00000500
00051      LABEL RECORDS STANDARD                                       00000510
00052      RECORD CONTAINS 510 CHARACTERS                               00000520
00053      BLOCK CONTAINS 0 RECORDS                                     00000530
00054      DATA RECORD IS DETAIL-EXTRACT.                               00000540
00055                                  COPY ECSEXT01.
00056  SKIP3                                                            00000560
00057  SD  SORT-FILE                                                    00000570
00058      RECORDING MODE F                                             00000580
00059      RECORD CONTAINS 47 CHARACTERS                                00000590
00060      DATA RECORD IS SORT-REC.                                     00000600
00061  01  SORT-REC.                                                    00000610
00062      03 SORT-PARM                PIC X(40).
00063      03 SORT-DATA                PIC X(7).
00064                                                                   00000640
00065  FD  DISK-DATE                                                    00000650
00066                                  COPY ELCDTEFD.
00067  EJECT                                                            00000670
00068  WORKING-STORAGE SECTION.                                         00000680
00069  77  FILLER  PIC X(32) VALUE '********************************'.  00000690
00070  77  FILLER  PIC X(32) VALUE '*    ECSBAK WORKING STORAGE    *'.  00000700
00071  77  FILLER  PIC X(32) VALUE '**********VMOD=2.001************'.  00000701
00072                                                                   00000720
00073  01  MISC.                                                        00000730
00074      12  PGM-SUB               PIC S999 COMP-3 VALUE +001.        00000740
00075      12  WS-ZERO               PIC S9    VALUE ZERO.              00000750
00076      12  WS-RETURN-CODE        PIC S9(4) VALUE +0    COMP.        00000760
00077      12  WS-ABEND-FILE-STATUS  PIC S9(4) VALUE ZERO.              00000770
00078      12  WS-ABEND-MESSAGE      PIC X(80) VALUE SPACES.            00000780
00079                                                                   00000790
00080      12  IN-CNT          PIC 9(7)  VALUE ZERO.                    00000800
00081      12  OUT-CNT         PIC 9(7)  VALUE ZERO.                    00000810
00082      12  DEL-CNT         PIC 9(7)  VALUE ZERO.                    00000820
00083      12  CAN-CNT         PIC 9(7)  VALUE ZERO.                    00000830
00084      12  DTH-CNT         PIC 9(7)  VALUE ZERO.                    00000840
00085      12  LUMP-CNT        PIC 9(7)  VALUE ZERO.                    00000850
00086      12  NO-CLAIMS       PIC 9(7)  VALUE ZERO.                    00000860
00087      12  PROC-CLAIMS     PIC 9(7)  VALUE ZERO.                    00000870
00088      12  TOT-CERT        PIC 9(7)   VALUE ZERO.                   00000880
00089      12  TOT-CONV-CERT   PIC 9(7)   VALUE ZERO.                   00000890
00090      12  SAVE-CM-DATE.                                            00000900
00091          16  SAVE-CM-YR      PIC 99      VALUE ZEROS.             00000910
00092          16  SAVE-CM-MO      PIC 99      VALUE ZEROS.             00000920
00093          16  SAVE-CM-DA      PIC 99      VALUE ZEROS.             00000930
00094      12  WORK-CM-DATE.                                            00000940
00095          16  WORK-CM-YR      PIC 99.                              00000950
00096          16  WORK-CM-MO      PIC 99.                              00000960
00097          16  WORK-CM-DA      PIC 99.                              00000970
00098      12  CERT-FLAGS.                                              00000980
00099          16  FLAG-1          PIC X       VALUE ' '.               00000990
00100          16  FLAG-2          PIC X       VALUE ' '.               00001000
00101          16  FLAG-3          PIC X       VALUE ' '.               00001010
00102  01  WORK-REC.                                                    00001020
00103      03 W-SEQ.                                                    00001030
00104         05 W-COMP        PIC X(7).                                00001040
00105         05 W-STATE       PIC X(2).                                00001050
00106         05 W-ACCT        PIC X(10).                               00001060
00107         05 W-CERT-DT     PIC 9(11) COMP-3.
              05  FILLER REDEFINES W-CERT-DT.
                  07  FILLER   PIC XXX.
                  07  W-CERT-CCYY PIC 9(4).
                  07  W-CERT-MO   PIC 99.
                  07  W-CERT-DA   PIC 99.
00108 *          07 W-CERT-YR  PIC XX.                                  00001080
00109 *          07 W-CERT-MO  PIC XX.                                  00001090
00110 *          07 W-CERT-DA  PIC XX.                                  00001100
00111         05 W-CERT        PIC X(11).                               00001110
00112      03 W-SEQ-2.                                                  00001120
00113         05 W-DATE        PIC 9(7) COMP-3.
              05  FILLER REDEFINES W-DATE.
                  07  FILLER   PIC X.
                  07  W-CCYR   PIC 9(4).
                  07  W-MO     PIC 99.
00114 *          07 W-YR       PIC XX.                                  00001140
00115 *          07 W-MO       PIC XX.                                  00001150
00116      03 W-TYP            PIC X.                                   00001160
00117         88 W-USED   VALUE ' '.                                    00001170
00118         88 W-DTH    VALUE '1'.                                    00001180
00119         88 W-AH     VALUE '2'.                                    00001190
00120         88 W-OB-DTH VALUE '3'.                                    00001200
00121         88 W-OB-AH  VALUE '4'.                                    00001210
00122      03 W-AMT            PIC S9(9)V99     COMP-3.                 00001220

00145                                  COPY ELCEXTVR.

00147                                  COPY ELCCRTVR.

00075                                  COPY ELCDTEVR.
00124                                  COPY ELCDTECX.

00127  PROCEDURE DIVISION.                                              00001270
00128  INITIAL-SECTION SECTION.                                         00001280
00129  STANDARD-READ.                                                   00001290
00130                                   COPY ELCDTERX.

00132  SORT-ROUTINE SECTION.                                            00001320
00133  SORT-RTN.                                                        00001330
00134      SORT SORT-FILE ON ASCENDING SORT-PARM                        00001340
00135         INPUT PROCEDURE INPUT-RTN THRU INPUT-XIT                  00001350
00136         OUTPUT PROCEDURE OUTPUT-RTN THRU OUTPUT-XIT.              00001360
00137                                                                   00001370
00138      IF SORT-RETURN NOT = 0                                       00001380
00139         MOVE +101                TO WS-RETURN-CODE
00140         PERFORM ABEND-PGM.                                        00001400
00141                                                                   00001410
00142      GO TO E-O-J.                                                 00001420
00143  EJECT                                                            00001430
00144  INPUT-RTN SECTION.                                               00001440
00145  OPEN-INPUT.                                                      00001450
00146                                                                   00001460
00147      OPEN INPUT CLAIM-FILE.                                       00001470
00148                                                                   00001480
           DISPLAY ' RUN-CCYY ' RUN-CCYY RUN-MO

           .
00149  READ-INPUT.                                                      00001490
00150                                                                   00001500
00151      READ CLAIM-FILE AT END                                       00001510
00152          GO TO END-INPUT.                                         00001520
00153                                                                   00001530
00154      IF (DE-REIN NOT = SPACE)                                     00001540
00155         OR (DE-ENTRY-STATUS EQUAL 'D' OR 'V')                     00001541
00156          GO TO READ-INPUT.                                        00001542
00157                                                                   00001543
00158      IF DE-DTH     OR                                             00001544
00159         DE-AH      OR                                             00001545
00160         DE-OB-DTH  OR                                             00001546
00161         DE-OB-AH                                                  00001547
00162         CONTINUE
00163      ELSE                                                         00001549
00164         GO TO READ-INPUT
           END-IF

00193                                  COPY ELCEXTM1.

00166      IF DE-CP-CCYY < RUN-CCYY                                     00001552
00167         GO TO READ-INPUT.

00169      IF (DE-CP-CCYY = RUN-CCYY)
              AND (DE-CP-MO NOT > THAN RUN-MO)
00171         GO TO READ-INPUT
           END-IF
00172                                                                   00001558
00173 *    IF DE-PAY-YR LESS THAN RUN-YR                                00001559
00174 *        GO TO READ-INPUT.                                        00001560
00175 *    IF DE-PAY-YR = RUN-YR AND                                    00001561
00176 *       DE-PAY-MO NOT GREATER THAN RUN-MO                         00001562
00177 *        GO TO READ-INPUT.                                        00001563
00178                                                                   00001564
00179      MOVE DE-CONTROL             TO W-SEQ.
00180      MOVE DE-PAY-YR              TO W-CCYR.
00181      MOVE DE-PAY-MO              TO W-MO.  
00182      MOVE DE-TYPE                TO W-TYP.   
00183      MOVE DE-CLAIM-AMT           TO W-AMT.
00184      MOVE WORK-REC               TO SORT-REC. 
00185                                                                   00001850
00186      RELEASE SORT-REC.                                            00001851
00187                                                                   00001870
00188      ADD +1 TO NO-CLAIMS.                                         00001871
00189                                                                   00001890
00190      GO TO READ-INPUT.                                            00001891
00191                                                                   00001910
00192  END-INPUT.                                                       00001911
00193                                                                   00001930
00194      CLOSE CLAIM-FILE.                                            00001931
00195                                                                   00001932
00196  INPUT-XIT.                                                       00001933
00197       EXIT.                                                       00001934
00198  EJECT                                                            00001935
00199  OUTPUT-RTN SECTION.                                              00001936
00200                                                                   00002000
00201  OPEN-OUTPUT.                                                     00002001
00202                                                                   00002002
00203      OPEN INPUT  OLD-CERT                                         00002003
00204           OUTPUT NEW-CERT.                                        00002004
00205      MOVE LOW-VALUE              TO WORK-REC.
00206      MOVE SPACES                 TO W-TYP.      
00207      MOVE ZEROES                 TO W-AMT.      
00208                                                                   00002080
00209  READ-OLD-CERT.                                                   00002081
00210                                                                   00002082
00211      READ OLD-CERT                                                00002083
00212          AT END  GO TO END-OUTPUT.                                00002084
00213                                                                   00002130
00214      ADD +1 TO IN-CNT.                                            00002131

                                       COPY ELCCRTM1.

00216      IF CR-ENTRY-CCYY > RUN-CCYY
00217         ADD  +1  TO  DEL-CNT
      *       DISPLAY ' ENTERED AFTER ' RUN-CCYY
      *           CR-FULL-CONTROL
00218         GO TO READ-OLD-CERT
           END-IF

00220      IF (CR-ENTRY-CCYY  =  RUN-CCYY)
              AND (CR-ENTRY-MO > RUN-MO)  
      *       DISPLAY ' ENTERED AFTER ' RUN-CCYY CR-FULL-CONTROL
00222         ADD +1                   TO DEL-CNT
00223         GO TO READ-OLD-CERT
           END-IF

            .
00225  RESET-LIFE-STATUS.

00227      IF CR-LFTYP = ZEROS OR SPACES
00228         GO TO RESET-A-H-STATUS
           END-IF

00230      IF (CR-LF-CEX-CCYY > RUN-CCYY)
                          OR   
00231         ((CR-LF-CEX-CCYY = RUN-CCYY)
              AND (CR-LF-CEX-MO > RUN-MO))
      *       DISPLAY ' LIFE CANCEL AFTER ' RUN-CCYY 
      *          CR-FULL-CONTROL
00233         ADD  +1  TO  CAN-CNT            
00234         MOVE ZEROS               TO CR-LFRFND CR-LFRFND-CALC
00235           CR-LF-CANC-DT  CR-LF-CANCEL-EXIT-DATE
00236         MOVE SPACE                TO CR-LF-STATUS-AT-CANCEL
           END-IF

00238      IF (CR-LF-DEX-CCYY > RUN-CCYY)
                      OR
00239         ((CR-LF-DEX-CCYY = RUN-CCYY)  AND            
00240         (CR-LF-DEX-MO > RUN-MO))
00241         ADD +1                   TO DTH-CNT
      *       DISPLAY ' DEATH AFTER ' RUN-CCYY CR-FULL-CONTROL
00242         MOVE  ZEROS      TO  CR-NUM-DTH-CLM    CR-DTH-DT
00243                               CR-DTH-RPT-DT     CR-DTH-PAY-DT
00244                               CR-DTHAMT         CR-DTHAMT-YTD
00245                               CR-DTHAMT-LAST    CR-DTHEXP
00246                               CR-DTHEXP-YTD     CR-DTH-AGE
00247         MOVE SPACES      TO  CR-LF-STATUS-AT-DEATH
00248                               CR-DTH-PAY-CD
00249                               CR-DEATH-CAUSE
           END-IF

00251      IF CR-LF-STATUS-AT-CANCEL = SPACE         
00252         IF CR-LF-STATUS-AT-DEATH = SPACE      
00253            MOVE CR-ENTRY-STATUS  TO CR-LF-CURRENT-STATUS
00254         ELSE                                           
00255            MOVE CR-ENTRY-STATUS  TO CR-LF-STATUS-AT-DEATH
00256            IF CR-LF-CURRENT-STATUS = '6' OR '7' 
00257               CONTINUE
00258            ELSE                                 
00259               MOVE '7'           TO CR-LF-CURRENT-STATUS
                 END-IF
              END-IF
00260      ELSE                                       
00261         IF CR-LF-STATUS-AT-DEATH = SPACE        
00262            MOVE CR-ENTRY-STATUS  TO CR-LF-STATUS-AT-CANCEL
00263            MOVE '8'              TO CR-LF-CURRENT-STATUS
              END-IF
           END-IF

00264      .
00266  RESET-A-H-STATUS.                                                00002642

00268      IF CR-AHTYP = ZEROS OR SPACES
00269         GO TO WT-REC
           END-IF

           IF CR-AH-CANCEL-EXIT-DATE NOT NUMERIC
              MOVE ZEROS               TO CR-AH-CANCEL-EXIT-DATE
           END-IF

00273      IF (CR-AH-CEX-CCYY > RUN-CCYY)
                      OR
00274         ((CR-AH-CEX-CCYY = RUN-CCYY)
              AND (CR-AH-CEX-MO > RUN-MO))
      *       DISPLAY '  AH  CANCEL AFTER ' RUN-CCYY RUN-MO
      *          CR-FULL-CONTROL
00276         ADD +1                   TO CAN-CNT         
00277         MOVE  ZEROS      TO CR-AHRFND      CR-AHRFND-CALC
00278                            CR-AH-CANC-DT  CR-AH-CANCEL-EXIT-DATE
00279         MOVE  SPACE      TO CR-AH-STATUS-AT-CANCEL
           END-IF

           IF CR-AH-SETTLEMENT-EXIT-DATE NOT NUMERIC
              MOVE ZEROS               TO CR-AH-SETTLEMENT-EXIT-DATE
           END-IF

00284      IF (CR-AH-DEX-CCYY  > RUN-CCYY)
                     OR
00285         ((CR-AH-DEX-CCYY = RUN-CCYY)
              AND (CR-AH-DEX-MO > RUN-MO))
00287         ADD  +1  TO  LUMP-CNT
      *       DISPLAY ' SETTLED AFTER ' RUN-CCYY RUN-MO
      *          CR-FULL-CONTROL
00288         MOVE  ZEROS      TO  CR-NUM-DIS-CLM    CR-DIS-DT
00289                               CR-DIS-RPT-DT     CR-DIS-PAY-DT     00002692
00290                               CR-DIS-PTO-DT                       00002693
00291                               CR-DISAMT         CR-DISAMT-YTD     00002694
00292                               CR-DISAMT-LAST    CR-DISEXP         00002695
00293                               CR-DISEXP-YTD     CR-DAYS-DISAB     00002696
00294          MOVE  SPACES     TO  CR-AH-STATUS-AT-SETTLEMENT          00002697
00295                               CR-DIS-PAY-CD                       00002698
00296                               CR-DISAB-CAUSE
           END-IF

00298      IF CR-AH-STATUS-AT-CANCEL = SPACE
00299         IF CR-AH-STATUS-AT-SETTLEMENT = SPACE
00300            MOVE CR-ENTRY-STATUS  TO CR-AH-CURRENT-STATUS
00301         ELSE                                            
00302            MOVE CR-ENTRY-STATUS  TO CR-AH-STATUS-AT-SETTLEMENT
00303            IF CR-AH-CURRENT-STATUS = '6' OR '7' 
00304               CONTINUE
00305            ELSE                                 
00306               MOVE '6'           TO CR-AH-CURRENT-STATUS
                 END-IF
              END-IF
00307      ELSE                                         
00308         IF CR-AH-STATUS-AT-SETTLEMENT = SPACE    
00309            MOVE CR-ENTRY-STATUS  TO CR-AH-STATUS-AT-CANCEL
00310            MOVE '8'              TO CR-AH-CURRENT-STATUS
              END-IF
           END-IF

           .
00312  WT-REC.                                                          00003111

00314      PERFORM MATCH-CLAIM-RTN     THRU MATCH-CLAIM-XIT.

00316      WRITE NEW-REC FROM CERTIFICATE-RECORD.                       00003151

00318      ADD +1 TO OUT-CNT.                                           00003153

00320      GO TO READ-OLD-CERT.                                         00003155

00322  MATCH-CLAIM-RTN.                                                 00003157

00324      IF W-SEQ < CR-FULL-CONTROL 
00325         GO TO CHECK-USAGE
           END-IF

00326      IF W-SEQ > CR-FULL-CONTROL
00327         GO TO MATCH-CLAIM-XIT
           END-IF

00328      IF W-SEQ = HIGH-VALUE                                        00003163
00329         GO TO MATCH-CLAIM-XIT
           END-IF

00330      .                                                            00003165
00331  MATCH-RTN.                                                       00003166

00333      IF W-DTH
              GO TO MATCH-LIFE
           END-IF

00334      IF W-OB-DTH
              GO TO MATCH-LIFE
           END-IF

00335      .                                                            00003170
00336  MATCH-AH.                                                        00003171

00338      SUBTRACT W-AMT              FROM CR-DISAMT
00339      IF CR-DISAMT NEGATIVE         
00340          MOVE ZEROS              TO CR-DISAMT
           END-IF
00341      MOVE ZEROS                  TO CR-DISAMT-YTD
                                          CR-DISAMT-LAST
00342      SUBTRACT +1                 FROM CR-NUM-DIS-CLM
           IF CR-NUM-DIS-CLM NEGATIVE
              MOVE ZEROS               TO CR-NUM-DIS-CLM
           END-IF
           
00343      MOVE SPACES                 TO W-TYP
00344      GO TO CHECK-USAGE

           .
00346  MATCH-LIFE.                                                      00003451
00347                                                                   00003470
00348      SUBTRACT W-AMT              FROM CR-DTHAMT

00349      IF CR-DTHAMT NEGATIVE                                        00003472
00350         MOVE ZEROS               TO CR-DTHAMT
           END-IF

00351      MOVE ZEROS                  TO CR-DTHAMT-YTD
                                          CR-DTHAMT-LAST
                                          CR-NUM-DTH-CLM
00352      MOVE SPACES                 TO W-TYP

           .
00354  CHECK-USAGE.                                                     00003477
00355                                                                   00003478
00356      IF W-SEQ = LOW-VALUES
              GO TO RETURN-RTN
           END-IF

00357      IF W-USED
00358         ADD +1                   TO PROC-CLAIMS
00359         GO TO RETURN-RTN
           END-IF
00360                                                                   00003600
00361 *    DISPLAY W-SEQ ' ' W-TYP ' ' W-AMT ' NO MATCH ' W-DATE.       00003601

           .
00363  RETURN-RTN.                                                      00003621
00364                                                                   00003640
00365      RETURN SORT-FILE AT END                                      00003641
00366         MOVE HIGH-VALUE TO WORK-REC                               00003642
00367         GO TO MATCH-CLAIM-XIT.                                    00003643
00368                                                                   00003680
00369      MOVE SORT-REC TO WORK-REC.                                   00003681
00370                                                                   00003682
00371      GO TO MATCH-CLAIM-RTN.                                       00003683
00372                                                                   00003684
00373  MATCH-CLAIM-XIT.  EXIT.                                          00003685
00374                                                                   00003686
00375  END-OUTPUT.                                                      00003687
00376                                                                   00003688
00377      MOVE HIGH-VALUE TO CERTIFICATE-RECORD.                       00003689
00378      PERFORM MATCH-CLAIM-RTN THRU MATCH-CLAIM-XIT.                00003690
00379      CLOSE OLD-CERT                                               00003691
00380            NEW-CERT.                                              00003692
00381      DISPLAY  '   IN COUNT       ' IN-CNT.                        00003693
00382      DISPLAY  '   DELETED        '  DEL-CNT.                      00003694
00383      DISPLAY  '   OUT COUNT      '  OUT-CNT.                      00003695
00384      DISPLAY  '   CANCELLED      '  CAN-CNT.                      00003696
00385      DISPLAY  '   DEATH          '  DTH-CNT.                      00003697
00386      DISPLAY  '   LUMP SUM       '  LUMP-CNT.                     00003698
00387      DISPLAY  '   TOTAL CLAIMS   '  NO-CLAIMS.                    00003699
00388      DISPLAY  '     PROCESSED    '  PROC-CLAIMS.                  00003700
00389                                                                   00003890
00390  OUTPUT-XIT.                                                      00003891
00391        EXIT.                                                      00003892
00392                                                                   00003893
00393  END-OF-JOB SECTION.                                              00003894
00394                                                                   00003895
00395  ABEND-PGM.                                                       00003896
00396      DISPLAY CERTIFICATE-RECORD.                                  00003897
00397                                 COPY ELCABEND.                    00003898
00398                                                                   00003899
00399  E-O-J.                                                           00003900
00400                                                                   00003901
00401      GOBACK.                                                      00003902
