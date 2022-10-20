000100 IDENTIFICATION DIVISION.                                         00000100
000300 PROGRAM-ID.    ISD034.                                           00000200
000600 AUTHOR.        CHRIS SLOUP.                                      00000300
000600 INSTALLATION.  CENTRAL STATES HEALTH & LIFE CO. OF OMAHA         00000400
002200 DATE-WRITTEN.  09-28-94.                                         00000500
002300**************************************************************    00000600
003100*  THIS PROGRAM CONVERTS DOLLAR AMOUNTS TO VERBAGE AMOUNTS   *    00000700
003700**************************************************************    00000800
003900 ENVIRONMENT DIVISION.                                            00000900
004000 CONFIGURATION SECTION.                                           00001000
004300                                                                  00001100
004400 DATA DIVISION.                                                   00001200
004600 WORKING-STORAGE SECTION.                                         00001300
004700                                                                  00001400
004800 01  WMSG-BEGSTOR                PICTURE X(40)   VALUE            00001500
004900     '* * * * *  BEGIN WK-STOR ISD034    * * *'.                  00001600
005300 01  WORKING-STORAGE-AREAS.                                       00001700
005400     12  WI-INDICES.                                              00001800
005500         16  FILLER          PICTURE X(32)   VALUE                00001900
005600                               '* * * * * * INDICES  * * * * * *'.00002000
005700     12  WI-SUB                              PICTURE S9(04)       00002100
005800                                             COMPUTATIONAL.       00002200
005900                                                                  00002300
006000     12  WS-SWITCHES.                                             00002400
006100         16  FILLER          PICTURE X(32)   VALUE                00002500
006200                               '* * * * * * SWITCHES * * * * * *'.00002600
006300     12  WS-POL-FND                          PICTURE X(01)        00002700
006400                                             VALUE  SPACE.        00002800
006500     12  WS-BAP-SWITCH                       PICTURE X(01)        00002900
006600                                             VALUE  SPACE.        00003000
006700                                                                  00003100
006800     12  WC-COUNTERS.                                             00003200
006900         16  FILLER          PICTURE X(32)   VALUE                00003300
007000                               '* * * * *  COUNTERS  * * * * * *'.00003400
007100         16  WA-WORK-RATIO                   PICTURE  S9(4)V9(5)  00003500
007200                                COMPUTATIONAL-3   VALUE  ZEROES.  00003600
007300                                                                  00003700
007400     12  WK-CONSTANTS.                                            00003800
007500         16  FILLER          PICTURE X(32)   VALUE                00003900
007600                               '* * * * *  CONSTANTS * * * * * *'.00004000
007700 01  TRANSLATE-TO-ENGLISH-TBLS.                                   00004100
007800     03  WW-TIE-INDEX-CNTRS      COMPUTATIONAL.                   00004200
007900         05  WW-TIE-1                PICTURE S99.                 00004300
008000         05  WW-TIE-2                PICTURE S99.                 00004400
008100         05  WW-TIE-3                PICTURE S99.                 00004500
008200         05  WW-TIE-4                PICTURE S99.                 00004600
008300         05  WW-TIE-MAX              PICTURE S99     VALUE +77.   00004700
008400     03  WW-COMMON-AMOUNT            PICTURE 9(11)V99.            00004800
008500     03  WW-COM-AMT-1    REDEFINES WW-COMMON-AMOUNT.              00004900
008600         05  WW-COM-AMT  OCCURS 13   PICTURE 9.                   00005000
008700     03  WW-COM-AMT-2    REDEFINES WW-COMMON-AMOUNT.              00005100
008800         05  WW-COM-AMT-BILL         PICTURE 99.                  00005200
008900         05  WW-COM-AMT-MILL         PICTURE 999.                 00005300
009000         05  WW-COM-AMT-THOU         PICTURE 999.                 00005400
009100         05  WW-COM-AMT-UNITS        PICTURE 999.                 00005500
009200         05  WW-COM-AMT-CENTS        PICTURE 99.                  00005600
009300     03  WW-UNITS-TABLE              PICTURE X(45)   VALUE 'ONE  T00005700
009400-            'WO  THREEFOUR FIVE SIX  SEVENEIGHTNINE '.           00005800
009500     03  WW-UNITS-TBL-RED    REDEFINES WW-UNITS-TABLE.            00005900
009600         05  WW-UNITS-WORD OCCURS 9  PICTURE X(5).                00006000
009700     03  WW-UNITS-LENGTH             PICTURE 9(9)                 00006100
009800                 VALUE 335443554.                                 00006200
009900     03  WW-UNITS-LNGTH-RED  REDEFINES WW-UNITS-LENGTH.           00006300
010000         05  WW-UNITS-LNG  OCCURS 9  PICTURE 9.                   00006400
010100     03  WW-TEENS-TABLE              PICTURE X(81)   VALUE 'ELEVEN00006500
010200-            '   TWELVE   THIRTEEN FOURTEEN FIFTEEN  SIXTEEN  SEVE00006600
010300-            'NTEENEIGHTEEN NINETEEN '.                           00006700
010400     03  WW-TEENS-TBL-RED    REDEFINES WW-TEENS-TABLE.            00006800
010500         05  WW-TEENS-WORD OCCURS 9  PICTURE X(9).                00006900
010600     03  WW-TEENS-LENGTH             PICTURE 9(9)                 00007000
010700                 VALUE 668877988.                                 00007100
010800     03  WW-TEENS-LNGTH-RED  REDEFINES WW-TEENS-LENGTH.           00007200
010900         05  WW-TEENS-LNG  OCCURS 9  PICTURE 9.                   00007300
011000     03  WW-TENS-TABLE               PICTURE X(63)   VALUE 'TEN   00007400
011100-            ' TWENTY THIRTY FORTY  FIFTY  SIXTY  SEVENTYEIGHTY NI00007500
011200-            'NETY '.                                             00007600
011300     03  WW-TENS-TBL-RED     REDEFINES WW-TENS-TABLE.             00007700
011400         05  WW-TENS-WORD OCCURS 9   PICTURE X(7).                00007800
011500     03  WW-TENS-LENGTH              PICTURE 9(9)                 00007900
011600                 VALUE 366555766.                                 00008000
011700     03  WW-TENS-LNGTH-RED   REDEFINES WW-TENS-LENGTH.            00008100
011800         05  WW-TENS-LNG   OCCURS 9  PICTURE 9.                   00008200
011900     03  WW-TIE-COM-MV               PICTURE X(12).               00008300
012000     03  WW-TIE-COM-MV-R     REDEFINES WW-TIE-COM-MV.             00008400
012100         05  WW-TIE-COM-MV-LTR  OCCURS 12    PICTURE X.           00008500
015000*                                                                 00008600
015100*    FILE WORK AREAS                                              00008700
015200 01  WMSG-ENDSTOR                PICTURE X(40)   VALUE            00008800
015300     '* * * * *  E N D WK-STOR ISD034    * * *'.                  00008900
015400/                                                                 00009000
015600*                                                                 00009100
015700 LINKAGE SECTION.                                                 00009200
015700 01  WW-TIE-WORK-AREA.                                            00009300
015800     12  WA-CHECK-AMOUNT                 PICTURE  S9(13)V99       00009400
015900                                         COMPUTATIONAL-3.         00009500
016000     12  WA-SCRIPT-OPT               PICTURE X(01).               00009600
016100     12  WW-TIE-WORK-LN-1            PICTURE X(77).               00009700
016200     12  WW-TIE-WORK-LN-2            PICTURE X(77).               00009800
016300     12  WW-TIE-WORK-LN-3            PICTURE X(77).               00009900
016400     12  WW-TIE-WORK-LN-RED  REDEFINES WW-TIE-WORK-LN-3.          00010000
016500         16  WW-TIE-LETTER OCCURS 77 PICTURE X.                   00010100
016600                                                                  00010200
016610 PROCEDURE DIVISION USING WW-TIE-WORK-AREA.                       00010300
018100                                                                  00010400
017700 S0000-MAINLINE.                                                  00010500
018100                                                                  00010600
098100     MOVE  WA-CHECK-AMOUNT                 TO WW-COMMON-AMOUNT.   00010700
020200     PERFORM S1000-TRANSLATE-INTO-ENGLISH  THRU S1000-EXIT.       00010800
018500     GOBACK.                                                      00010900
018100                                                                  00011000
018100 S0000-EXIT.                                                      00011100
018100       EXIT.                                                      00011200
018100       EJECT                                                      00011300
018100                                                                  00011400
020700                                                                  00011500
020800 S1000-TRANSLATE-INTO-ENGLISH.                                    00011600
021000**************************************************************    00011700
021000*    NOTE.                                                   *    00011800
021100*        THIS SECTION WILL TRANSLATE A 13 DIGIT AMOUNT FIELD *    00011900
021200*            CONSISTING OF 11 WHOLE AND TWO DECIMAL NUMBERS  *    00012000
021300*            INTO ENGLISH LANGUAGE WORDS.                    *    00012100
021000**************************************************************    00012200
021400                                                                  00012300
021600     MOVE    SPACES                  TO  WW-TIE-WORK-LN-1.        00012400
021700     MOVE    SPACES                  TO  WW-TIE-WORK-LN-2.        00012500
021800     MOVE    SPACES                  TO  WW-TIE-WORK-LN-3.        00012600
021900     MOVE    1                       TO  WW-TIE-1.                00012700
022000                                                                  00012800
022100     IF  WW-COMMON-AMOUNT EQUAL TO ZERO                           00012900
022200         PERFORM TIE-890                                          00013000
022300         GO  TO  S1000-EXIT.                                      00013100
022400                                                                  00013200
022500     IF  WW-COM-AMT-BILL       EQUAL  ZERO                        00013300
022600     AND WW-COM-AMT-MILL       EQUAL  ZERO                        00013400
022700     AND WW-COM-AMT-THOU       EQUAL  ZERO                        00013500
022800     AND WW-COM-AMT-UNITS      EQUAL  ZERO                        00013600
022900         GO TO TIE-180.                                           00013700
023000                                                                  00013800
023100     IF  WW-COM-AMT-BILL  EQUAL  ZERO                             00013900
023200         GO  TO  TIE-050.                                         00014000
023300                                                                  00014100
023400     IF  WW-COM-AMT (1)   EQUAL  1                                00014200
023500         IF  WW-COM-AMT (2)  NOT EQUAL  ZERO                      00014300
023600             MOVE    WW-COM-AMT (2)  TO  WW-TIE-2                 00014400
023700             PERFORM TIE-840                                      00014500
023800             GO  TO  TIE-040.                                     00014600
023900                                                                  00014700
024000     IF  WW-COM-AMT (1)   EQUAL  ZERO                             00014800
024100         GO  TO  TIE-030.                                         00014900
024200                                                                  00015000
024300     MOVE    WW-COM-AMT (1)          TO  WW-TIE-2.                00015100
024400     PERFORM TIE-850.                                             00015200
024500     IF  WW-COM-AMT (2)  EQUAL  ZERO                              00015300
024600         GO  TO  TIE-040.                                         00015400
024700     MOVE    '-'                     TO  WW-TIE-COM-MV.           00015500
024800     MOVE     1                      TO  WW-TIE-3.                00015600
024900     PERFORM TIE-800.                                             00015700
025000                                                                  00015800
025100 TIE-030.                                                         00015900
025200     MOVE   WW-COM-AMT (2)           TO  WW-TIE-2.                00016000
025300     PERFORM TIE-820.                                             00016100
025400                                                                  00016200
025500 TIE-040.                                                         00016300
025600     PERFORM TIE-860.                                             00016400
025700     MOVE     'BILLION'              TO  WW-TIE-COM-MV.           00016500
025800     MOVE      7                     TO  WW-TIE-3.                00016600
025900     PERFORM TIE-800.                                             00016700
026000                                                                  00016800
026100                                                                  00016900
026200     IF  WW-COM-AMT-MILL       EQUAL  ZERO                        00017000
026300     AND WW-COM-AMT-THOU       EQUAL  ZERO                        00017100
026400     AND WW-COM-AMT-UNITS      EQUAL  ZERO                        00017200
026500         GO TO TIE-160.                                           00017300
026600                                                                  00017400
026700     MOVE    ' ,'                    TO  WW-TIE-COM-MV.           00017500
026800     MOVE      2                     TO  WW-TIE-3.                00017600
026900     PERFORM TIE-860.                                             00017700
027000                                                                  00017800
027100 TIE-050.                                                         00017900
027200     IF  WW-COM-AMT-MILL       EQUAL  ZERO                        00018000
027300         GO  TO  TIE-090.                                         00018100
027400                                                                  00018200
027500     IF  WW-COM-AMT (3)        EQUAL  ZERO                        00018300
027600         GO  TO  TIE-060.                                         00018400
027700                                                                  00018500
027800     MOVE    WW-COM-AMT (3)          TO  WW-TIE-2.                00018600
027900     PERFORM TIE-820.                                             00018700
028000     PERFORM TIE-830.                                             00018800
028100     IF  WW-COM-AMT (4)        EQUAL  ZERO                        00018900
028200         IF  WW-COM-AMT (5)    EQUAL  ZERO                        00019000
028300             GO  TO  TIE-080.                                     00019100
028400     PERFORM TIE-860.                                             00019200
028500                                                                  00019300
028600 TIE-060.                                                         00019400
028700     IF  WW-COM-AMT (4)        EQUAL  1                           00019500
028800         IF  WW-COM-AMT (5)    NOT EQUAL  ZERO                    00019600
028900             MOVE    WW-COM-AMT (5)  TO  WW-TIE-2                 00019700
029000             PERFORM TIE-840                                      00019800
029100             GO  TO  TIE-080.                                     00019900
029200                                                                  00020000
029300     IF  WW-COM-AMT (4)        EQUAL  ZERO                        00020100
029400         GO  TO  TIE-070.                                         00020200
029500                                                                  00020300
029600     MOVE    WW-COM-AMT (4)          TO  WW-TIE-2.                00020400
029700     PERFORM TIE-850.                                             00020500
029800     IF  WW-COM-AMT (5)        EQUAL  ZERO                        00020600
029900         GO  TO  TIE-080.                                         00020700
030000                                                                  00020800
030100     MOVE    '-'                     TO  WW-TIE-COM-MV.           00020900
030200     MOVE     1                      TO  WW-TIE-3.                00021000
030300     PERFORM TIE-800.                                             00021100
030400                                                                  00021200
030500 TIE-070.                                                         00021300
030600     MOVE    WW-COM-AMT (5)          TO  WW-TIE-2.                00021400
030700     PERFORM TIE-820.                                             00021500
030800                                                                  00021600
030900 TIE-080.                                                         00021700
031000     PERFORM TIE-860.                                             00021800
031100     MOVE     'MILLION'              TO  WW-TIE-COM-MV.           00021900
031200     MOVE      7                     TO  WW-TIE-3.                00022000
031300     PERFORM TIE-800.                                             00022100
031400     IF  WW-COM-AMT-THOU       EQUAL  ZERO                        00022200
031500         IF  WW-COM-AMT-UNITS  EQUAL  ZERO                        00022300
031600             GO  TO  TIE-160.                                     00022400
031700     MOVE    ' ,'                    TO  WW-TIE-COM-MV.           00022500
031800     MOVE      2                     TO  WW-TIE-3.                00022600
031900     PERFORM TIE-860.                                             00022700
032000                                                                  00022800
032100 TIE-090.                                                         00022900
032200     IF  WW-COM-AMT-THOU       EQUAL  ZERO                        00023000
032300         GO  TO  TIE-130.                                         00023100
032400                                                                  00023200
032500     IF  WW-COM-AMT (6)        EQUAL  ZERO                        00023300
032600         GO  TO  TIE-100.                                         00023400
032700     MOVE    WW-COM-AMT (6)          TO  WW-TIE-2.                00023500
032800     PERFORM TIE-820.                                             00023600
032900     PERFORM TIE-830.                                             00023700
033000     IF  WW-COM-AMT (7)        EQUAL  ZERO                        00023800
033100         IF  WW-COM-AMT (8)    EQUAL  ZERO                        00023900
033200             GO  TO  TIE-120.                                     00024000
033300     PERFORM TIE-860.                                             00024100
033400                                                                  00024200
033500 TIE-100.                                                         00024300
033600     IF  WW-COM-AMT (7)        EQUAL  1                           00024400
033700         IF  WW-COM-AMT (8)    NOT EQUAL  ZERO                    00024500
033800             MOVE    WW-COM-AMT (8)  TO  WW-TIE-2                 00024600
033900             PERFORM TIE-840                                      00024700
034000             GO  TO  TIE-120.                                     00024800
034100                                                                  00024900
034200     IF  WW-COM-AMT (7)        EQUAL  ZERO                        00025000
034300         GO  TO  TIE-110.                                         00025100
034400                                                                  00025200
034500     MOVE    WW-COM-AMT (7)          TO  WW-TIE-2.                00025300
034600     PERFORM TIE-850.                                             00025400
034700     IF  WW-COM-AMT (8)        EQUAL  ZERO                        00025500
034800         GO  TO  TIE-120.                                         00025600
034900                                                                  00025700
035000     MOVE    '-'                     TO  WW-TIE-COM-MV.           00025800
035100     MOVE     1                      TO  WW-TIE-3.                00025900
035200     PERFORM TIE-800.                                             00026000
035300                                                                  00026100
035400 TIE-110.                                                         00026200
035500     MOVE    WW-COM-AMT (8)          TO  WW-TIE-2.                00026300
035600     PERFORM TIE-820.                                             00026400
035700                                                                  00026500
035800 TIE-120.                                                         00026600
035900     PERFORM TIE-860.                                             00026700
036000     MOVE    'THOUSAND '             TO  WW-TIE-COM-MV.           00026800
036100     MOVE      9                     TO  WW-TIE-3.                00026900
036200     PERFORM TIE-800.                                             00027000
036300                                                                  00027100
036400 TIE-130.                                                         00027200
036500     IF  WW-COM-AMT-UNITS      EQUAL  ZERO                        00027300
036600         GO  TO  TIE-170.                                         00027400
036700                                                                  00027500
036800     IF  WW-COM-AMT (9)        EQUAL  ZERO                        00027600
036900         GO  TO  TIE-140.                                         00027700
037000     MOVE    WW-COM-AMT (9)          TO  WW-TIE-2.                00027800
037100     PERFORM TIE-820.                                             00027900
037200     PERFORM TIE-830.                                             00028000
037300     IF  WW-COM-AMT (10)       EQUAL  ZERO                        00028100
037400         IF  WW-COM-AMT (11)   EQUAL  ZERO                        00028200
037500             GO  TO  TIE-160.                                     00028300
037600     PERFORM TIE-860.                                             00028400
037700                                                                  00028500
037800 TIE-140.                                                         00028600
037900     IF  WW-COM-AMT (10)       EQUAL  1                           00028700
038000         IF  WW-COM-AMT (11)   NOT EQUAL  ZERO                    00028800
038100             MOVE    WW-COM-AMT (11) TO  WW-TIE-2                 00028900
038200             PERFORM TIE-840                                      00029000
038300             GO  TO  TIE-160.                                     00029100
038400                                                                  00029200
038500     IF  WW-COM-AMT (10)       EQUAL  ZERO                        00029300
038600         GO  TO  TIE-150.                                         00029400
038700                                                                  00029500
038800     MOVE    WW-COM-AMT (10)         TO  WW-TIE-2.                00029600
038900     PERFORM TIE-850.                                             00029700
039000     IF  WW-COM-AMT (11)       EQUAL  ZERO                        00029800
039100         GO  TO  TIE-160.                                         00029900
039200                                                                  00030000
039300     MOVE    '-'                     TO  WW-TIE-COM-MV.           00030100
039400     MOVE     1                      TO  WW-TIE-3.                00030200
039500     PERFORM TIE-800.                                             00030300
039600                                                                  00030400
039700 TIE-150.                                                         00030500
039800     MOVE    WW-COM-AMT (11)         TO  WW-TIE-2.                00030600
039900     PERFORM TIE-820.                                             00030700
040000                                                                  00030800
040100 TIE-160.                                                         00030900
040200     PERFORM TIE-860.                                             00031000
040300                                                                  00031100
040400 TIE-170.                                                         00031200
040500     MOVE 'DOLLARS ' TO WW-TIE-COM-MV.                            00031300
040600     MOVE 8 TO WW-TIE-3.                                          00031400
040700     IF  WW-COMMON-AMOUNT  LESS  THAN  2.00,                      00031500
040800         MOVE SPACE TO WW-TIE-COM-MV-LTR (7),                     00031600
040900         MOVE 7 TO WW-TIE-3.                                      00031700
041000     PERFORM TIE-800.                                             00031800
041100                                                                  00031900
041200 TIE-180.                                                         00032000
041300     IF WW-COM-AMT-CENTS EQUAL TO ZERO,                           00032100
041400         MOVE 'NO' TO WW-TIE-COM-MV,                              00032200
041500         MOVE 2 TO WW-TIE-3,                                      00032300
041600         PERFORM TIE-800,                                         00032400
041700         GO TO TIE-200.                                           00032500
041800     IF WW-COM-AMT (12) EQUAL 1 AND WW-COM-AMT (13) NOT EQUAL     00032600
041900         ZERO, MOVE WW-COM-AMT (13) TO WW-TIE-2,                  00032700
042000             PERFORM TIE-840,                                     00032800
042100             GO TO TIE-200.                                       00032900
042200     IF WW-COM-AMT (12) EQUAL TO ZERO, GO TO TIE-190.             00033000
042300     MOVE WW-COM-AMT (12)         TO  WW-TIE-2.                   00033100
042400     PERFORM TIE-850.                                             00033200
042500     IF WW-COM-AMT (13) EQUAL TO ZERO, GO TO TIE-200.             00033300
042600     MOVE '-' TO WW-TIE-COM-MV.                                   00033400
042700     MOVE 1 TO WW-TIE-3.                                          00033500
042800     PERFORM TIE-800.                                             00033600
042900                                                                  00033700
043000 TIE-190.                                                         00033800
043100     MOVE WW-COM-AMT (13) TO WW-TIE-2.                            00033900
043200     PERFORM TIE-820.                                             00034000
043300                                                                  00034100
043400 TIE-200.                                                         00034200
043500     PERFORM TIE-860.                                             00034300
043600     MOVE 'CENTS' TO WW-TIE-COM-MV,                               00034400
043700     MOVE 5 TO WW-TIE-3.                                          00034500
043800     IF WW-COM-AMT-CENTS EQUAL TO 1,                              00034600
043900         MOVE SPACE TO WW-TIE-COM-MV-LTR (5),                     00034700
044000         MOVE 4 TO WW-TIE-3.                                      00034800
044100     PERFORM TIE-800.                                             00034900
044200     IF  WW-TIE-WORK-LN-1 EQUAL  TO  SPACES                       00035000
044300         MOVE WW-TIE-WORK-LN-3   TO  WW-TIE-WORK-LN-1             00035100
044400         MOVE SPACES             TO  WW-TIE-WORK-LN-3             00035200
044500     ELSE                                                         00035300
044600     IF  WW-TIE-WORK-LN-2 EQUAL  TO  SPACES                       00035400
044700         MOVE WW-TIE-WORK-LN-3   TO  WW-TIE-WORK-LN-2             00035500
044800         MOVE SPACES             TO  WW-TIE-WORK-LN-3.            00035600
044900     GO TO S1000-EXIT.                                            00035700
045000                                                                  00035800
045100 TIE-800.                                                         00035900
045200     ADD WW-TIE-1 WW-TIE-3 GIVING WW-TIE-4.                       00036000
045300     IF WW-TIE-4 > WW-TIE-MAX, PERFORM TIE-900.                   00036100
045400     MOVE 1 TO WW-TIE-4.                                          00036200
045500     PERFORM TIE-810 WW-TIE-3 TIMES.                              00036300
045600                                                                  00036400
045700 TIE-810.                                                         00036500
045800     MOVE WW-TIE-COM-MV-LTR (WW-TIE-4) TO WW-TIE-LETTER           00036600
045900         (WW-TIE-1).                                              00036700
046000     ADD 1 TO WW-TIE-1.                                           00036800
046100     ADD 1 TO WW-TIE-4.                                           00036900
046200                                                                  00037000
046300 TIE-820.                                                         00037100
046400     MOVE WW-UNITS-WORD (WW-TIE-2) TO WW-TIE-COM-MV.              00037200
046500     MOVE WW-UNITS-LNG (WW-TIE-2) TO WW-TIE-3.                    00037300
046600     PERFORM TIE-800.                                             00037400
046700                                                                  00037500
046800 TIE-830.                                                         00037600
046900     MOVE ' HUNDRED' TO WW-TIE-COM-MV.                            00037700
047000     MOVE 8 TO WW-TIE-3.                                          00037800
047100     PERFORM TIE-800.                                             00037900
047200                                                                  00038000
047300 TIE-840.                                                         00038100
047400     MOVE WW-TEENS-WORD (WW-TIE-2) TO WW-TIE-COM-MV.              00038200
047500     MOVE WW-TEENS-LNG (WW-TIE-2) TO WW-TIE-3.                    00038300
047600     PERFORM TIE-800.                                             00038400
047700                                                                  00038500
047800 TIE-850.                                                         00038600
047900     MOVE WW-TENS-WORD (WW-TIE-2) TO WW-TIE-COM-MV.               00038700
048000     MOVE WW-TENS-LNG (WW-TIE-2) TO WW-TIE-3.                     00038800
048100     PERFORM TIE-800.                                             00038900
048200                                                                  00039000
048300 TIE-860.                                                         00039100
048400     MOVE SPACES TO WW-TIE-COM-MV.                                00039200
048500     MOVE 1 TO WW-TIE-3.                                          00039300
048600     PERFORM TIE-800.                                             00039400
048700                                                                  00039500
048800 TIE-890.                                                         00039600
048900     MOVE 'NO DOLLARS ' TO WW-TIE-COM-MV.                         00039700
049000     MOVE  11           TO WW-TIE-3.                              00039800
049100     PERFORM TIE-800.                                             00039900
049200     MOVE 'AND NO CENTS' TO WW-TIE-COM-MV.                        00040000
049300     MOVE  12            TO WW-TIE-3.                             00040100
049400     PERFORM TIE-800.                                             00040200
049500 TIE-900.                                                         00040300
049600     IF WW-TIE-WORK-LN-2 NOT EQUAL TO SPACE                       00040400
049700         GO TO S1000-EXIT.                                        00040500
049800     IF  WW-TIE-WORK-LN-1 EQUAL  TO  SPACES                       00040600
049900         MOVE WW-TIE-WORK-LN-3   TO  WW-TIE-WORK-LN-1             00040700
050000         MOVE SPACES             TO  WW-TIE-WORK-LN-3             00040800
050100     ELSE                                                         00040900
050200     IF  WW-TIE-WORK-LN-2 EQUAL  TO  SPACES                       00041000
050300         MOVE WW-TIE-WORK-LN-3   TO  WW-TIE-WORK-LN-2             00041100
050400         MOVE SPACES             TO  WW-TIE-WORK-LN-3.            00041200
050500     MOVE 1 TO WW-TIE-1.                                          00041300
050600                                                                  00041400
050700 S1000-EXIT.                                                      00041500
050800     EXIT.                                                        00041600
050800     EJECT                                                        00041700
050600                                                                  00041800
