000100 IDENTIFICATION DIVISION.                                         00000100
000200 PROGRAM-ID.    CIDRE010.                                         00000200
000300 DATE-WRITTEN.  AUG, 1994.                                        00000300
000350                                                                  00000310
000400***************************************************************** 00000400
000500*  N O T E:                                                     * 00000500
000700*  ========                                                     * 00000501
000500*                                                               * 00000520
000500*  THIS IS A "CLONE" OF CIRE010 AND IS FOR CID ADMIN.           * 00000521
000500*                                                               * 00000522
000500*  IT PRODUCES A REPORT OF REQUESTED ACCOUNTS THAT ARE SENT     * 00000523
000550*    TO THE FINANCIAL DEPT. FOR BALANCING.                      * 00000530
000550*                                                               * 00000531
000550*  MOST REPORTS ARE PRODUCED ONLY AT YEAR-END (DECEMBER MOE).   * 00000532
000550*       =======                                                 * 00000533
000550*                                                               * 00000534
000550*  - NOTE -                                                     * 00000535
000550*  PER PHONE CALL FROM JANET DYER IN CORP. ACCTNG. ON 7/5/96:   * 00000536
000550*  CENTENNIAL (ACCT 122-124) AND WHEELS (ACCT 701, 703, 705)    * 00000537
000550*   SHOULD PRINT IN BOTH JUNE AND DECEMBER.                     * 00000538
000550*                   ====                                        * 00000539
000550*                                                               * 00000540
000550*  DISK FILES ARE PRODUCED SEMI-ANNUALLY FOR REQUESTED ACCOUNTS.* 00000541
000550*  ==========                                                   * 00000542
000550*                                                               * 00000543
000500*      THESE ACCOUNTS ARE:                                      * 00000544
000550*                                                               * 00000545
000500*      1) CENSTAT LIFE          (PRINT AT YEAR END ONLY)        * 00000546
000500*      2) CORNHUSKER LIFE         "    "    "   "    "          * 00000548
000500*      3) MARQUETTE LIFE          "    "    "   "    "          * 00000549
000550*                                                               * 00000550
000500*      4) CENTENNIAL LIFE       (PRINT IN JUNE AND DECEMBER)    * 00000551
000500*      5) WHEELS LIFE              "    "   "   "     "         * 00000552
000550*                                                               * 00000553
000550*      ALL ACCOUNTS ARE WRITTEN TO DISK FILES SEMI-ANNUALLY.    * 00000554
000550*      === ========                                             * 00000560
000550*                                                               * 00000570
000600***************************************************************** 00000600
000550*                                                               * 00000700
000400***************************************************************** 00000710
000500*  SELECT PRINT LINES FROM THE ECS-045 FICH REPORTS.            * 00000720
000600***************************************************************** 00000730
000550*                                                               * 00000731
000800 ENVIRONMENT DIVISION.                                            00000800
000700                                                                  00000810
000900 INPUT-OUTPUT SECTION.                                            00000900
000700                                                                  00000910
001000 FILE-CONTROL.                                                    00001000
001100                                                                  00001100
001200     SELECT ECS045-FICHE                                          00001200
001300         ASSIGN TO SYS010.                                        00001300
001100                                                                  00001400
001200     SELECT CENSTAT-REPORT                                        00001500
001300         ASSIGN TO SYS012.                                        00001600
001400                                                                  00001610
001200     SELECT CENSTAT-DISK                                          00001611
001300         ASSIGN TO SYS013.                                        00001612
001400                                                                  00001613
001200     SELECT CENTEN-DISK                                           00001614
001300         ASSIGN TO SYS020.                                        00001615
001100                                                                  00001616
001200     SELECT CENTEN-REPORT                                         00001617
001300         ASSIGN TO SYS024.                                        00001618
001400                                                                  00001619
001200     SELECT CORNHUSK-DISK                                         00001620
001300         ASSIGN TO SYS021.                                        00001621
001100                                                                  00001622
001200     SELECT CORNHUSK-REPORT                                       00001623
001300         ASSIGN TO SYS025.                                        00001624
001400                                                                  00001625
001200     SELECT MARQUET-DISK                                          00001626
001300         ASSIGN TO SYS022.                                        00001627
001100                                                                  00001628
001200     SELECT MARQUET-REPORT                                        00001629
001300         ASSIGN TO SYS026.                                        00001630
001400                                                                  00001631
001200     SELECT WHEELS-DISK                                           00001632
001300         ASSIGN TO SYS023.                                        00001633
001100                                                                  00001634
001200     SELECT WHEELS-REPORT                                         00001635
001300         ASSIGN TO SYS027.                                        00001636
001400                                                                  00001637
001500     SELECT DISK-DATE                                             00002320
001600         ASSIGN TO SYS019.                                        00002330
002400                                                                  00002400
002600 DATA DIVISION.                                                   00002600
002500                                                                  00002610
002700 FILE SECTION.                                                    00002700
002800                                                                  00003410
002900 FD  DISK-DATE         COPY ELCDTEFD.                             00003420
002800                                                                  00003421
003600 FD  ECS045-FICHE                                                 00003600
003700     LABEL RECORDS ARE STANDARD                                   00003700
003800     RECORDING MODE IS F                                          00003800
003900     RECORD CONTAINS 133 CHARACTERS                               00003900
004000     BLOCK CONTAINS 0 RECORDS.                                    00004000
004100 01  FICHE-RECORD.                                                00004100
004200     05  CC                  PIC X(001).                          00004200
004300     05  FILLER              PIC X(132).                          00004300
003500                                                                  00004310
003600 FD  CENSTAT-DISK                                                 00004320
003700     LABEL RECORDS ARE STANDARD                                   00004330
003800     RECORDING MODE IS F                                          00004340
003900     RECORD CONTAINS 179 CHARACTERS                               00004350
004000     BLOCK CONTAINS 0 RECORDS.                                    00004360
004100 01  CENSTAT-DISK-REC             PIC X(179).                     00004370
003500                                                                  00004371
003600 FD  CENSTAT-REPORT                                               00004372
003700     LABEL RECORDS ARE STANDARD                                   00004373
003800     RECORDING MODE IS F                                          00004374
003900     RECORD CONTAINS 179 CHARACTERS                               00004375
004000     BLOCK CONTAINS 0 RECORDS.                                    00004376
004100 01  CENSTAT-RECORD               PIC X(179).                     00004377
003500                                                                  00004378
003600 FD  CENTEN-DISK                                                  00004379
003700     LABEL RECORDS ARE STANDARD                                   00004380
003800     RECORDING MODE IS F                                          00004381
003900     RECORD CONTAINS 179 CHARACTERS                               00004382
004000     BLOCK CONTAINS 0 RECORDS.                                    00004383
004100 01  CENTEN-DISK-REC             PIC X(179).                      00004390
003500                                                                  00004391
003600 FD  CENTEN-REPORT                                                00004392
003700     LABEL RECORDS ARE STANDARD                                   00004393
003800     RECORDING MODE IS F                                          00004394
003900     RECORD CONTAINS 179 CHARACTERS                               00004395
004000     BLOCK CONTAINS 0 RECORDS.                                    00004396
004100 01  CENTEN-RECORD                PIC X(179).                     00004397
003500                                                                  00004398
003600 FD  CORNHUSK-DISK                                                00004399
003700     LABEL RECORDS ARE STANDARD                                   00004400
003800     RECORDING MODE IS F                                          00004401
003900     RECORD CONTAINS 179 CHARACTERS                               00004402
004000     BLOCK CONTAINS 0 RECORDS.                                    00004403
004100 01  CORNHUSK-DISK-REC             PIC X(179).                    00004404
003500                                                                  00004405
003600 FD  CORNHUSK-REPORT                                              00004406
003700     LABEL RECORDS ARE STANDARD                                   00004407
003800     RECORDING MODE IS F                                          00004408
003900     RECORD CONTAINS 179 CHARACTERS                               00004409
004000     BLOCK CONTAINS 0 RECORDS.                                    00004410
004100 01  CORNHUSK-RECORD              PIC X(179).                     00004411
003500                                                                  00004412
003600 FD  MARQUET-DISK                                                 00004413
003700     LABEL RECORDS ARE STANDARD                                   00004414
003800     RECORDING MODE IS F                                          00004415
003900     RECORD CONTAINS 179 CHARACTERS                               00004416
004000     BLOCK CONTAINS 0 RECORDS.                                    00004417
004100 01  MARQUET-DISK-REC             PIC X(179).                     00004418
003500                                                                  00004419
003600 FD  MARQUET-REPORT                                               00004421
003700     LABEL RECORDS ARE STANDARD                                   00004422
003800     RECORDING MODE IS F                                          00004423
003900     RECORD CONTAINS 179 CHARACTERS                               00004424
004000     BLOCK CONTAINS 0 RECORDS.                                    00004425
004100 01  MARQUET-RECORD              PIC X(179).                      00004426
003500                                                                  00004427
003600 FD  WHEELS-DISK                                                  00004428
003700     LABEL RECORDS ARE STANDARD                                   00004429
003800     RECORDING MODE IS F                                          00004430
003900     RECORD CONTAINS 179 CHARACTERS                               00004431
004000     BLOCK CONTAINS 0 RECORDS.                                    00004432
004100 01  WHEELS-DISK-REC             PIC X(179).                      00004433
003500                                                                  00004434
003600 FD  WHEELS-REPORT                                                00004435
003700     LABEL RECORDS ARE STANDARD                                   00004436
003800     RECORDING MODE IS F                                          00004437
003900     RECORD CONTAINS 179 CHARACTERS                               00004438
004000     BLOCK CONTAINS 0 RECORDS.                                    00004439
004100 01  WHEELS-RECORD               PIC X(179).                      00004440
004400                                                                  00004450
004800     EJECT                                                        00004800
004900                                                                  00004900
005000 WORKING-STORAGE SECTION.                                         00005000
005100                                                                  00005100
005200 01  WORK.                                                        00005200
005300     05  EOF-SW              PIC X       VALUE 'N'.               00005300
005400         88  EOF                         VALUE 'Y'.               00005400
005500     05  WS-RETURN-CODE      PIC 99      VALUE ZEROS.             00005500
005500     05  WS-ABEND-MESSAGE    PIC X(80)   VALUE SPACES.            00005501
005500     05  PGM-SUB             PIC 999     VALUE ZEROS.             00005502
005500     05  SEL-SW              PIC X       VALUE SPACE.             00005503
005500     05  CENSTAT-SW          PIC X       VALUE SPACE.             00005510
005500     05  CENTEN-SW           PIC X       VALUE SPACE.             00005520
005500     05  CORNHUSK-SW         PIC X       VALUE SPACE.             00005530
005500     05  MARQUET-SW          PIC X       VALUE SPACE.             00005540
005500     05  WHEELS-SW           PIC X       VALUE SPACE.             00005550
005700     05  S0C7                PIC S9      COMP-3.                  00005700
005800     05  SAVE-COMPANY-NAME   PIC X(30)   VALUE SPACE.             00005800
005900                                                                  00005900
006000***************************************************************** 00006000
006100*   DATE CARD INFORMATION                                       * 00006100
006200***************************************************************** 00006200
005900                                                                  00006210
005900     COPY ELCDTECX.                                               00006411
Y2KMOD     COPY ELCDTEVR.                                               00006411
005900                                                                  00006412
006000***************************************************************** 00006420
006100*   COMPANIES TO BE SELECTED FOR REPORTING                      * 00006430
006200***************************************************************** 00006440
005900                                                                  00006450
010200 01  DATE-CARD-LINE.                                              00010200
010300     05  FILLER                 PIC X(21)                         00010300
010300                      VALUE 'DATE CARD MONTH IS = '.              00010301
010300     05  PR-DATE                PIC X(02)    VALUE SPACES.        00010302
010300     05  FILLER                 PIC X(109)   VALUE SPACES.        00010303
010100                                                                  00010304
010200 01  ROUTE-RECORD.                                                00010305
010300     05  R-CC                   PIC X(01)    VALUE SPACES.        00010306
010300     05  R-REC.                                                   00010307
010300         10  FILLER             PIC X(10)    VALUE SPACES.        00010310
010400         10  R-ROUTE-TO         PIC X(10)    VALUE 'ROUTE TO: '.  00010400
010300         10  FILLER             PIC X(05)    VALUE SPACES.        00010410
010500         10  R-INFO             PIC X(60)    VALUE SPACES.        00010500
010600         10  FILLER             PIC X(47)    VALUE SPACES.        00010600
010100                                                                  00010700
010200 01  STARS.                                                       00010701
010300     05  FILLER             PIC X(50)   VALUE                     00010702
010400         '* * * * * * * * * * * * * * * * * * * * * * * * * '.    00010703
010300     05  FILLER             PIC X(50)   VALUE                     00010704
010400         '* * * * * * * * * * * * * * * * * * * * * * * * * '.    00010705
010300     05  FILLER             PIC X(30)   VALUE                     00010707
010400         '* * * * * * * * * * * * * * * '.                        00010708
010100                                                                  00010712
010200 01  WORK-RECORD.                                                 00010720
010300     05  WORK-KEY.                                                00010730
010400         10  WORK-COMPANY       PIC X(030)    VALUE SPACE.        00010740
010500         10  WORK-COPY          PIC 9(001)    VALUE ZERO.         00010750
010600**       10  WORK-REPORT        PIC X(008)    VALUE SPACE.        00010760
Y2KMOD         10  WORK-REPORT        PIC X(007)    VALUE SPACE.        00010760
010700         10  WORK-LINE-NO       PIC 9(007)    VALUE ZERO.         00010770
010800     05  WORK-LINE              PIC X(133)    VALUE SPACE.        00010800
010900                                                                  00010900
011000 01  HDG1.                                                        00011000
011100**   05  FILLER                 PIC X(125).                       00011100
011200**   05  HDG1-REPORT            PIC X(008).                       00011200
Y2KMOD     05  FILLER                 PIC X(120).                       00011100
Y2KMOD     05  HDG1-REPORT            PIC X(007).                       00011200
Y2KMOD     05  FILLER                 PIC X(006).                       00011200
010900                                                                  00011210
011300 01  HDG2                       PIC X(133).                       00011300
010900                                                                  00011310
011400 01  HDG3.                                                        00011400
011500     10  FILLER                 PIC X(001).                       00011500
011600     10  HDG3-MSG               PIC X(015).                       00011600
011700     10  FILLER                 PIC X(008).                       00011700
011800     10  HDG3-COMPANY           PIC X(030).                       00011800
011900     10  FILLER                 PIC X(079).                       00011900
010900                                                                  00011910
012000 01  HDG4.                                                        00012000
012100     10  FILLER                 PIC X(021).                       00012100
012200     10  HDG4-MSG               PIC X(016).                       00012200
012300     10  FILLER                 PIC X(096).                       00012300
010900                                                                  00012310
012400 01  HDG5                       PIC X(133).                       00012400
010900                                                                  00012410
012500 01  HDG6.                                                        00012500
012600     10  FILLER                 PIC X(002).                       00012600
012700     10  HDG6-PRIME             PIC X(003).                       00012700
012800     10  FILLER                 PIC X(128).                       00012800
010900                                                                  00012810
012900 01  HDG7.                                                        00012900
013000     10  FILLER                 PIC X(002).                       00013000
013100     10  HDG7-PRIME             PIC X(003).                       00013100
013200     10  FILLER                 PIC X(032).                       00013200
013300     10  HDG7-ACCT              PIC X(010).                       00013300
013400     10  FILLER                 PIC X(086).                       00013400
010900                                                                  00013410
012900 01  WK-DATE.                                                     00013420
013000     10  WK-MO                  PIC X(02)  VALUE SPACES.          00013430
013100     10  FILLER                 PIC X(01)  VALUE SPACES.          00013440
013000     10  WK-DA                  PIC X(02)  VALUE SPACES.          00013450
013100     10  FILLER                 PIC X(01)  VALUE SPACES.          00013460
013000     10  WK-YR                  PIC X(02)  VALUE SPACES.          00013470
013500                                                                  00013500
013800 PROCEDURE DIVISION.                                              00013800
013500                                                                  00013830
013500 0000-DATE-ROUTINE SECTION.                                       00013840
013500                        COPY ELCDTERX.                            00013851
013500                                                                  00013852
TSTMOD     MOVE  RUN-MO         TO   WK-MO.                             00013854
013500                                                                  00013855
TSTMOD     DISPLAY '  '.                                                00013856
TSTMOD     DISPLAY 'RUN-MO = '  RUN-MO.                                 00013857
013500                                                                  00013860
013900     PERFORM INITIALIZATION.                                      00013900
013500                                                                  00013910
TSTMOD     MOVE ' ' TO R-CC.                                            00013950
TSTMOD      MOVE STARS TO R-REC.                                        00013951
TSTMOD        MOVE  ROUTE-RECORD TO WORK-LINE.                          00013953
TSTMOD         WRITE CENSTAT-RECORD FROM WORK-RECORD.                   00013954
TSTMOD         WRITE CENTEN-RECORD FROM WORK-RECORD.                    00013955
TSTMOD         WRITE CORNHUSK-RECORD FROM WORK-RECORD.                  00013956
TSTMOD         WRITE MARQUET-RECORD FROM WORK-RECORD.                   00013957
TSTMOD         WRITE WHEELS-RECORD FROM WORK-RECORD.                    00013958
TSTMOD                                                                  00013959
CIDMOD     MOVE  ' ' TO R-CC.                                           00013960
CIDMOD      MOVE  SPACES TO R-REC.                                      00013961
015300       MOVE  ROUTE-RECORD TO WORK-LINE.                           00013962
022800         WRITE CENSTAT-RECORD FROM WORK-RECORD.                   00013963
022800         WRITE CENTEN-RECORD FROM WORK-RECORD.                    00013964
022800         WRITE CORNHUSK-RECORD FROM WORK-RECORD.                  00013965
022800         WRITE MARQUET-RECORD FROM WORK-RECORD.                   00013966
022800         WRITE WHEELS-RECORD FROM WORK-RECORD.                    00013967
TSTMOD                                                                  00013969
CIDMOD     MOVE  ' ' TO R-CC.                                           00013970
CIDMOD      MOVE  'ROUTE TO: ' TO R-ROUTE-TO.                           00013971
CIDMOD       MOVE  'C O R P O R A T E   F I N A N C E' TO R-INFO.       00013972
015300        MOVE  ROUTE-RECORD TO WORK-LINE.                          00013973
022800         WRITE CENSTAT-RECORD FROM WORK-RECORD.                   00013974
022800         WRITE CENTEN-RECORD FROM WORK-RECORD.                    00013975
022800         WRITE CORNHUSK-RECORD FROM WORK-RECORD.                  00013976
022800         WRITE MARQUET-RECORD FROM WORK-RECORD.                   00013977
022800         WRITE WHEELS-RECORD FROM WORK-RECORD.                    00013978
TSTMOD                                                                  00013979
CIDMOD     MOVE  ' ' TO R-CC.                                           00013980
CIDMOD      MOVE  SPACES TO R-REC.                                      00013981
015300       MOVE  ROUTE-RECORD TO WORK-LINE.                           00013982
022800         WRITE CENSTAT-RECORD FROM WORK-RECORD.                   00013983
022800         WRITE CENTEN-RECORD FROM WORK-RECORD.                    00013984
022800         WRITE CORNHUSK-RECORD FROM WORK-RECORD.                  00013985
022800         WRITE MARQUET-RECORD FROM WORK-RECORD.                   00013986
022800         WRITE WHEELS-RECORD FROM WORK-RECORD.                    00013987
TSTMOD                                                                  00013988
CIDMOD     MOVE  ' ' TO R-CC.                                           00013989
CIDMOD      MOVE  '    ATTN: ' TO R-ROUTE-TO.                           00013991
CIDMOD       MOVE                                                       00013992
CIDMOD        'C R E D I T   I N S U R A N C E   A C C O U N T A N T'   00013993
CIDMOD            TO  R-INFO.                                           00013994
015300        MOVE  ROUTE-RECORD TO WORK-LINE.                          00013995
022800         WRITE CENSTAT-RECORD FROM WORK-RECORD.                   00013996
022800         WRITE CENTEN-RECORD FROM WORK-RECORD.                    00013997
022800         WRITE CORNHUSK-RECORD FROM WORK-RECORD.                  00013998
022800         WRITE MARQUET-RECORD FROM WORK-RECORD.                   00013999
022800         WRITE WHEELS-RECORD FROM WORK-RECORD.                    00014000
TSTMOD                                                                  00014001
CIDMOD     MOVE  ' ' TO R-CC.                                           00014002
CIDMOD      MOVE  SPACES TO R-REC.                                      00014003
015300       MOVE  ROUTE-RECORD TO WORK-LINE.                           00014004
022800         WRITE CENSTAT-RECORD FROM WORK-RECORD.                   00014005
022800         WRITE CENTEN-RECORD FROM WORK-RECORD.                    00014006
022800         WRITE CORNHUSK-RECORD FROM WORK-RECORD.                  00014007
022800         WRITE MARQUET-RECORD FROM WORK-RECORD.                   00014008
022800         WRITE WHEELS-RECORD FROM WORK-RECORD.                    00014009
TSTMOD                                                                  00014010
CIDMOD     MOVE  ' ' TO R-CC.                                           00014011
CIDMOD      MOVE  SPACES TO R-ROUTE-TO.                                 00014012
CIDMOD       MOVE                                                       00014013
CIDMOD        'C E N T E N N I A L   C A P I T A L   S T M T   P R E P' 00014014
CIDMOD           TO  R-INFO.                                            00014015
015300        MOVE  ROUTE-RECORD TO WORK-LINE.                          00014016
022800         WRITE CENTEN-RECORD FROM WORK-RECORD.                    00014017
TSTMOD                                                                  00014018
CIDMOD     MOVE                                                         00014019
CIDMOD       'C E N S T A T   C A P I T A L   S T M T   P R E P'        00014020
CIDMOD           TO  R-INFO.                                            00014021
015300     MOVE  ROUTE-RECORD TO WORK-LINE.                             00014022
022800     WRITE CENSTAT-RECORD FROM WORK-RECORD.                       00014023
TSTMOD                                                                  00014024
CIDMOD     MOVE                                                         00014025
CIDMOD       'C O R N H U S K E R   C A P I T A L   S T M T   P R E P'  00014026
CIDMOD           TO  R-INFO.                                            00014027
015300     MOVE  ROUTE-RECORD TO WORK-LINE.                             00014028
022800     WRITE CORNHUSK-RECORD FROM WORK-RECORD.                      00014029
TSTMOD                                                                  00014030
CIDMOD     MOVE                                                         00014031
CIDMOD       'M A R Q U E T T E   C A P I T A L   S T M T   P R E P'    00014032
CIDMOD           TO  R-INFO.                                            00014033
015300     MOVE  ROUTE-RECORD TO WORK-LINE.                             00014034
022800     WRITE MARQUET-RECORD FROM WORK-RECORD.                       00014035
TSTMOD                                                                  00014036
CIDMOD     MOVE                                                         00014037
CIDMOD       'W H E E L S   C A P I T A L   S T M T   P R E P'          00014038
CIDMOD           TO  R-INFO.                                            00014039
015300     MOVE  ROUTE-RECORD TO WORK-LINE.                             00014040
022800     WRITE WHEELS-RECORD FROM WORK-RECORD.                        00014041
CIDMOD                                                                  00014042
TSTMOD                                                                  00014043
CIDMOD     MOVE  ' ' TO R-CC.                                           00014044
CIDMOD      MOVE  SPACES TO R-REC.                                      00014045
015300       MOVE  ROUTE-RECORD TO WORK-LINE.                           00014046
TSTMOD         WRITE CENSTAT-RECORD FROM WORK-RECORD.                   00014047
022800         WRITE CENTEN-RECORD FROM WORK-RECORD.                    00014048
022800         WRITE CORNHUSK-RECORD FROM WORK-RECORD.                  00014049
022800         WRITE MARQUET-RECORD FROM WORK-RECORD.                   00014050
022800         WRITE WHEELS-RECORD FROM WORK-RECORD.                    00014051
CIDMOD                                                                  00014052
CIDMOD     MOVE  ' ' TO R-CC.                                           00014053
CIDMOD      MOVE  SPACES       TO R-ROUTE-TO.                           00014054
CIDMOD       MOVE  '"045-A" OUTPUT FROM PROGRAM "CIDRE010"' TO R-INFO.  00014055
015300        MOVE  ROUTE-RECORD TO WORK-LINE.                          00014056
TSTMOD         WRITE CENSTAT-RECORD FROM WORK-RECORD.                   00014057
022800         WRITE CENTEN-RECORD FROM WORK-RECORD.                    00014058
022800         WRITE CORNHUSK-RECORD FROM WORK-RECORD.                  00014059
022800         WRITE MARQUET-RECORD FROM WORK-RECORD.                   00014060
022800         WRITE WHEELS-RECORD FROM WORK-RECORD.                    00014061
CIDMOD                                                                  00014062
CIDMOD     MOVE  ' ' TO R-CC.                                           00014063
CIDMOD      MOVE  SPACES TO R-REC.                                      00014064
015300       MOVE  ROUTE-RECORD TO WORK-LINE.                           00014065
TSTMOD         WRITE CENSTAT-RECORD FROM WORK-RECORD.                   00014066
022800         WRITE CENTEN-RECORD FROM WORK-RECORD.                    00014067
022800         WRITE CORNHUSK-RECORD FROM WORK-RECORD.                  00014068
022800         WRITE MARQUET-RECORD FROM WORK-RECORD.                   00014069
022800         WRITE WHEELS-RECORD FROM WORK-RECORD.                    00014070
CIDMOD                                                                  00014071
CIDMOD     MOVE  ' ' TO R-CC.                                           00014072
CIDMOD      MOVE  SPACES       TO R-ROUTE-TO.                           00014073
CIDMOD       MOVE  'DISK FILE ID = CI.DL.FICH045A.CENSTAT ' TO R-INFO.  00014074
015300        MOVE  ROUTE-RECORD TO WORK-LINE.                          00014075
TSTMOD         WRITE CENSTAT-RECORD FROM WORK-RECORD.                   00014076
CIDMOD                                                                  00014077
CIDMOD     MOVE  ' ' TO R-CC.                                           00014078
CIDMOD      MOVE  SPACES       TO R-ROUTE-TO.                           00014079
CIDMOD       MOVE  'DISK FILE ID = CI.DL.FICH045A.CENTENNI' TO R-INFO.  00014080
015300        MOVE  ROUTE-RECORD TO WORK-LINE.                          00014081
022800         WRITE CENTEN-RECORD FROM WORK-RECORD.                    00014082
CIDMOD                                                                  00014083
CIDMOD     MOVE  ' ' TO R-CC.                                           00014084
CIDMOD      MOVE  SPACES       TO R-ROUTE-TO.                           00014085
CIDMOD       MOVE  'DISK FILE ID = CI.DL.FICH045A.CORNHUSK' TO R-INFO.  00014086
015300        MOVE  ROUTE-RECORD TO WORK-LINE.                          00014087
022800         WRITE CORNHUSK-RECORD FROM WORK-RECORD.                  00014088
CIDMOD                                                                  00014089
CIDMOD     MOVE  ' ' TO R-CC.                                           00014090
CIDMOD      MOVE  SPACES       TO R-ROUTE-TO.                           00014091
CIDMOD       MOVE  'DISK FILE ID = CI.DL.FICH045A.MARQUETT' TO R-INFO.  00014092
015300        MOVE  ROUTE-RECORD TO WORK-LINE.                          00014093
022800         WRITE MARQUET-RECORD FROM WORK-RECORD.                   00014094
CIDMOD                                                                  00014095
CIDMOD     MOVE  ' ' TO R-CC.                                           00014096
CIDMOD      MOVE  SPACES       TO R-ROUTE-TO.                           00014097
CIDMOD       MOVE  'DISK FILE ID = CI.DL.FICH045A.WHEELS  ' TO R-INFO.  00014098
015300        MOVE  ROUTE-RECORD TO WORK-LINE.                          00014099
022800         WRITE WHEELS-RECORD FROM WORK-RECORD.                    00014100
CIDMOD                                                                  00014101
CIDMOD     MOVE  ' ' TO R-CC.                                           00014102
CIDMOD      MOVE  SPACES TO R-REC.                                      00014103
015300       MOVE  ROUTE-RECORD TO WORK-LINE.                           00014104
TSTMOD         WRITE CENSTAT-RECORD FROM WORK-RECORD.                   00014105
022800         WRITE CENTEN-RECORD FROM WORK-RECORD.                    00014106
022800         WRITE CORNHUSK-RECORD FROM WORK-RECORD.                  00014107
022800         WRITE MARQUET-RECORD FROM WORK-RECORD.                   00014108
022800         WRITE WHEELS-RECORD FROM WORK-RECORD.                    00014109
TSTMOD                                                                  00014110
CIDMOD     MOVE  ' ' TO R-CC.                                           00014111
CIDMOD      MOVE  STARS  TO R-REC.                                      00014112
015300       MOVE  ROUTE-RECORD TO WORK-LINE.                           00014113
TSTMOD         WRITE CENSTAT-RECORD FROM WORK-RECORD.                   00014114
022800         WRITE CENTEN-RECORD FROM WORK-RECORD.                    00014115
022800         WRITE CORNHUSK-RECORD FROM WORK-RECORD.                  00014116
022800         WRITE MARQUET-RECORD FROM WORK-RECORD.                   00014117
022800         WRITE WHEELS-RECORD FROM WORK-RECORD.                    00014118
CIDMOD                                                                  00014119
CIDMOD     MOVE  ' ' TO R-CC.                                           00014120
CIDMOD      MOVE  SPACES TO R-REC.                                      00014121
015300       MOVE  ROUTE-RECORD TO WORK-LINE.                           00014122
TSTMOD         WRITE CENSTAT-RECORD FROM WORK-RECORD.                   00014123
022800         WRITE CENTEN-RECORD FROM WORK-RECORD.                    00014124
022800         WRITE CORNHUSK-RECORD FROM WORK-RECORD.                  00014125
022800         WRITE MARQUET-RECORD FROM WORK-RECORD.                   00014126
022800         WRITE WHEELS-RECORD FROM WORK-RECORD.                    00014127
TSTMOD                                                                  00014128
TSTMOD     IF  RUN-MO =  12                                             00014129
TSTMOD         GO TO ROUTE-TO-DONE.                                     00014130
TSTMOD                                                                  00014131
TSTMOD     MOVE ' ' TO R-CC.                                            00014132
TSTMOD      MOVE SPACES  TO  R-REC.                                     00014133
TSTMOD       MOVE  ROUTE-RECORD TO WORK-LINE.                           00014134
TSTMOD         WRITE CENSTAT-RECORD FROM WORK-RECORD.                   00014139
TSTMOD         WRITE CORNHUSK-RECORD FROM WORK-RECORD.                  00014141
TSTMOD         WRITE MARQUET-RECORD FROM WORK-RECORD.                   00014143
TSTMOD                                                                  00014144
TSTMOD     MOVE ' ' TO R-CC.                                            00014145
TSTMOD      MOVE 'PAPER REPORTS FOR CENSTAT, CORNHUSKER AND MARQUETTE ' 00014146
TSTMOD           TO  R-REC.                                             00014147
TSTMOD       MOVE  ROUTE-RECORD TO WORK-LINE.                           00014148
TSTMOD         WRITE CENSTAT-RECORD FROM WORK-RECORD.                   00014150
TSTMOD         WRITE CORNHUSK-RECORD FROM WORK-RECORD.                  00014151
TSTMOD         WRITE MARQUET-RECORD FROM WORK-RECORD.                   00014152
TSTMOD                                                                  00014153
TSTMOD      MOVE 'PROCESS ONLY AT YEAR-END '                            00014154
TSTMOD           TO  R-REC.                                             00014155
TSTMOD       MOVE  ROUTE-RECORD TO WORK-LINE.                           00014156
TSTMOD         WRITE CENSTAT-RECORD FROM WORK-RECORD.                   00014158
TSTMOD         WRITE CORNHUSK-RECORD FROM WORK-RECORD.                  00014159
TSTMOD         WRITE MARQUET-RECORD FROM WORK-RECORD.                   00014160
TSTMOD                                                                  00014161
TSTMOD     MOVE ' ' TO R-CC.                                            00014162
TSTMOD      MOVE SPACES  TO  R-REC.                                     00014163
TSTMOD       MOVE  ROUTE-RECORD TO WORK-LINE.                           00014164
TSTMOD         WRITE CENSTAT-RECORD FROM WORK-RECORD.                   00014166
TSTMOD         WRITE CORNHUSK-RECORD FROM WORK-RECORD.                  00014167
TSTMOD         WRITE MARQUET-RECORD FROM WORK-RECORD.                   00014168
TSTMOD                                                                  00014169
TSTMOD     MOVE ' ' TO R-CC.                                            00014170
TSTMOD      MOVE 'RUN MONTH IS NOT DECEMBER  '                          00014171
TSTMOD           TO  R-REC.                                             00014172
TSTMOD       MOVE  ROUTE-RECORD TO WORK-LINE.                           00014173
TSTMOD         WRITE CENSTAT-RECORD FROM WORK-RECORD.                   00014175
TSTMOD         WRITE CORNHUSK-RECORD FROM WORK-RECORD.                  00014176
TSTMOD         WRITE MARQUET-RECORD FROM WORK-RECORD.                   00014177
TSTMOD                                                                  00014178
TSTMOD     MOVE ' ' TO R-CC.                                            00014179
TSTMOD      MOVE SPACES  TO  R-REC.                                     00014180
TSTMOD       MOVE  ROUTE-RECORD TO WORK-LINE.                           00014181
TSTMOD         WRITE CENSTAT-RECORD FROM WORK-RECORD.                   00014183
TSTMOD         WRITE CORNHUSK-RECORD FROM WORK-RECORD.                  00014184
TSTMOD         WRITE MARQUET-RECORD FROM WORK-RECORD.                   00014185
TSTMOD                                                                  00014187
TSTMOD     MOVE ' ' TO R-CC.                                            00014188
TSTMOD      MOVE RUN-MO TO PR-DATE.                                     00014189
TSTMOD       MOVE DATE-CARD-LINE                                        00014190
TSTMOD              TO  R-REC.                                          00014191
TSTMOD        MOVE  ROUTE-RECORD TO WORK-LINE.                          00014192
TSTMOD         WRITE CENSTAT-RECORD FROM WORK-RECORD.                   00014194
TSTMOD         WRITE CORNHUSK-RECORD FROM WORK-RECORD.                  00014196
TSTMOD         WRITE MARQUET-RECORD FROM WORK-RECORD.                   00014197
TSTMOD                                                                  00014199
TSTMOD     MOVE ' ' TO R-CC.                                            00014200
TSTMOD      MOVE SPACES  TO  R-REC.                                     00014201
TSTMOD       MOVE  ROUTE-RECORD TO WORK-LINE.                           00014202
TSTMOD         WRITE CENSTAT-RECORD FROM WORK-RECORD.                   00014204
TSTMOD         WRITE CORNHUSK-RECORD FROM WORK-RECORD.                  00014206
TSTMOD         WRITE MARQUET-RECORD FROM WORK-RECORD.                   00014207
TSTMOD                                                                  00014209
TSTMOD     MOVE ' ' TO R-CC.                                            00014210
TSTMOD      MOVE 'RUN WILL PROCEED WITHOUT PAPER REPORTING '            00014211
TSTMOD           TO  R-REC.                                             00014212
TSTMOD       MOVE  ROUTE-RECORD TO WORK-LINE.                           00014213
TSTMOD         WRITE CENSTAT-RECORD FROM WORK-RECORD.                   00014215
TSTMOD         WRITE CORNHUSK-RECORD FROM WORK-RECORD.                  00014217
TSTMOD         WRITE MARQUET-RECORD FROM WORK-RECORD.                   00014218
TSTMOD                                                                  00014220
TSTMOD     MOVE ' ' TO R-CC.                                            00014221
TSTMOD      MOVE SPACES  TO  R-REC.                                     00014222
TSTMOD       MOVE  ROUTE-RECORD TO WORK-LINE.                           00014223
TSTMOD         WRITE CENSTAT-RECORD FROM WORK-RECORD.                   00014225
TSTMOD         WRITE CORNHUSK-RECORD FROM WORK-RECORD.                  00014227
TSTMOD         WRITE MARQUET-RECORD FROM WORK-RECORD.                   00014228
TSTMOD                                                                  00014230
TSTMOD     MOVE ' ' TO R-CC.                                            00014231
TSTMOD      MOVE 'ALL DISK FILES WILL BE BUILT FOR C. I. D.'            00014232
TSTMOD           TO  R-REC.                                             00014233
TSTMOD       MOVE  ROUTE-RECORD TO WORK-LINE.                           00014234
TSTMOD         WRITE CENSTAT-RECORD FROM WORK-RECORD.                   00014236
TSTMOD         WRITE CORNHUSK-RECORD FROM WORK-RECORD.                  00014238
TSTMOD         WRITE MARQUET-RECORD FROM WORK-RECORD.                   00014239
TSTMOD                                                                  00014241
TSTMOD     MOVE ' ' TO R-CC.                                            00014242
TSTMOD      MOVE SPACES  TO  R-REC.                                     00014243
TSTMOD       MOVE  ROUTE-RECORD TO WORK-LINE.                           00014244
TSTMOD         WRITE CENSTAT-RECORD FROM WORK-RECORD.                   00014246
TSTMOD         WRITE CORNHUSK-RECORD FROM WORK-RECORD.                  00014248
TSTMOD         WRITE MARQUET-RECORD FROM WORK-RECORD.                   00014249
TSTMOD                                                                  00014251
TSTMOD     MOVE ' ' TO R-CC.                                            00014252
TSTMOD      MOVE STARS TO R-REC.                                        00014253
TSTMOD       MOVE  ROUTE-RECORD TO WORK-LINE.                           00014254
TSTMOD         WRITE CENSTAT-RECORD FROM WORK-RECORD.                   00014256
TSTMOD         WRITE CORNHUSK-RECORD FROM WORK-RECORD.                  00014258
TSTMOD         WRITE MARQUET-RECORD FROM WORK-RECORD.                   00014259
TSTMOD                                                                  00014261
014000 ROUTE-TO-DONE.                                                   00014262
TSTMOD                                                                  00014263
014000     PERFORM PROCESS-FILE UNTIL EOF.                              00014264
013500                                                                  00014265
014100     PERFORM END-OF-JOB.                                          00014266
013500                                                                  00014270
014200     STOP RUN.                                                    00014300
014300                                                                  00014980
014500 PROCESS-FILE SECTION.                                            00014990
014400                                                                  00014991
014600     READ ECS045-FICHE                                            00014992
014700         AT END MOVE 'Y' TO EOF-SW.                               00014993
014800     IF EOF                                                       00014994
014900         GO TO PROCESS-FILE-X.                                    00014995
015000     IF CC = '1'                                                  00014996
015100         PERFORM CHECK-REPORT.                                    00014997
015200     IF SEL-SW = 'Y'                                              00014998
015300         MOVE FICHE-RECORD TO WORK-LINE                           00014999
015400         PERFORM WRITE-REPORT.                                    00015000
014400                                                                  00015010
015500 PROCESS-FILE-X.                                                  00015020
015600     EXIT.                                                        00015030
015700                                                                  00015040
015900 CHECK-REPORT SECTION.                                            00015050
015800                                                                  00015060
016000****************************************************************  00015070
016100*  THIS ROUTINE CHECKS THE HEADING LINES ON EACH PAGE. IT SETS *  00015080
016200*  THE SWITCHES TO 'Y' IF HEADINGS ARE TO BE PRINTED.          *  00015090
016300*  THEY ARE SET TO "N" IF THE PAGE IS NOT TO BE PRINTED.       *  00015091
016400****************************************************************  00015092
015800                                                                  00015093
TSTMOD*  NOTE - INITIALIZE THE FOLLOWING 6 SWITCHES TO "NO".            00015094
015800                                                                  00015093
TSTMOD     MOVE 'N' TO CENSTAT-SW.                                      00015094
TSTMOD     MOVE 'N' TO CENTEN-SW.                                       00015095
TSTMOD     MOVE 'N' TO CORNHUSK-SW.                                     00015096
TSTMOD     MOVE 'N' TO MARQUET-SW                                       00015097
TSTMOD     MOVE 'N' TO WHEELS-SW.                                       00015098
016500     MOVE 'N' TO SEL-SW.                                          00015099
016600     MOVE FICHE-RECORD  TO  HDG1.                                 00015100
016700**   IF HDG1-REPORT NOT = ('ECS-045A')                            00015101
Y2KMOD     IF HDG1-REPORT NOT = ('ECS045A')                             00015101
016800         GO TO CHECK-REPORT-X.                                    00015102
016900     READ ECS045-FICHE INTO HDG2.                                 00015103
017000     READ ECS045-FICHE INTO HDG3.                                 00015104
017100     READ ECS045-FICHE INTO HDG4.                                 00015105
017200     READ ECS045-FICHE INTO HDG5.                                 00015110
017300     READ ECS045-FICHE INTO HDG6.                                 00015120
017400     READ ECS045-FICHE INTO HDG7.                                 00015130
017500     IF HDG3-COMPANY NOT = SPACES                                 00015140
017600         MOVE HDG3-COMPANY TO SAVE-COMPANY-NAME.                  00015150
017700**   IF HDG1-REPORT = 'ECS-045A'                                  00015160
Y2KMOD     IF HDG1-REPORT = 'ECS045A'                                   00015160
017800         PERFORM CHECK-045A.                                      00015170
018100     IF SEL-SW = 'Y'                                              00015180
018200         PERFORM WRITE-HEADINGS.                                  00015190
015800                                                                  00015191
018300 CHECK-REPORT-X.                                                  00015192
018400     EXIT.                                                        00015193
018500                                                                  00015194
018700 CHECK-045A SECTION.                                              00015195
018800                                                                  00015196
018800*    CENSTAT                                                      00015197
018800                                                                  00015198
018900     IF (HDG6-PRIME = '150' OR '155')                             00015199
019100         AND  (HDG3-MSG = 'REIN COMP TOTAL')                      00015200
019200           MOVE 'Y' TO SEL-SW                                     00015201
019200             MOVE 'Y' TO CENSTAT-SW                               00015202
019300             GO TO CHECK-045A-X.                                  00015203
019400                                                                  00015204
019500     IF (HDG7-PRIME = '150' OR '155')                             00015205
019200         MOVE 'Y' TO CENSTAT-SW                                   00015206
019800           MOVE 'Y' TO SEL-SW                                     00015207
019300             GO TO CHECK-045A-X.                                  00015208
018800                                                                  00015209
018800*    CENTENNIAL                                                   00015210
018800                                                                  00015211
018900     IF (HDG6-PRIME = '122' OR '123' OR '124')                    00015212
019100         AND  (HDG3-MSG = 'REIN COMP TOTAL')                      00015213
019200           MOVE 'Y' TO SEL-SW                                     00015214
019200            MOVE 'Y' TO CENTEN-SW                                 00015215
019300             GO TO CHECK-045A-X.                                  00015216
019400                                                                  00015217
019500     IF (HDG7-PRIME = '122' OR '123' OR '124')                    00015220
019800         MOVE 'Y' TO SEL-SW                                       00015230
019200           MOVE 'Y' TO CENTEN-SW                                  00015231
019300             GO TO CHECK-045A-X.                                  00015232
018800                                                                  00015233
018800*    CORNHUSKER                                                   00015234
018800                                                                  00015235
018900     IF (HDG6-PRIME = '180' OR '181')                             00015236
019100         AND  (HDG3-MSG = 'REIN COMP TOTAL')                      00015237
019200           MOVE 'Y' TO SEL-SW                                     00015238
019200            MOVE 'Y' TO CORNHUSK-SW                               00015239
019300             GO TO CHECK-045A-X.                                  00015240
019400                                                                  00015241
019500     IF (HDG7-PRIME = '180' OR '181')                             00015242
019800         MOVE 'Y' TO SEL-SW                                       00015243
019200           MOVE 'Y' TO CORNHUSK-SW                                00015244
019300             GO TO CHECK-045A-X.                                  00015245
018800                                                                  00015246
018800*    MARQUETTE                                                    00015247
018800                                                                  00015248
PEMMOD*    IF (HDG6-PRIME = '850')                                      00015249
PEMMOD     IF (HDG6-PRIME = '850' OR '990')                             00015249
019100         AND  (HDG3-MSG = 'REIN COMP TOTAL')                      00015250
019200           MOVE 'Y' TO SEL-SW                                     00015251
019200             MOVE 'Y' TO MARQUET-SW                               00015252
019300             GO TO CHECK-045A-X.                                  00015253
019400                                                                  00015254
PEMMOD*    IF HDG7-PRIME = '850'                                        00015255
PEMMOD     IF HDG7-PRIME = '850' OR '990'                               00015255
019800         MOVE 'Y' TO SEL-SW                                       00015256
019200           MOVE 'Y' TO MARQUET-SW                                 00015257
019300             GO TO CHECK-045A-X.                                  00015258
018800                                                                  00015259
018800*    WHEELS                                                       00015260
018800                                                                  00015261
018900     IF (HDG6-PRIME = '701' OR '703' OR '705')                    00015262
019100         AND  (HDG3-MSG = 'REIN COMP TOTAL')                      00015263
019800         MOVE 'Y' TO SEL-SW                                       00015264
019200           MOVE 'Y' TO WHEELS-SW                                  00015265
019300             GO TO CHECK-045A-X.                                  00015266
019400                                                                  00015267
019500     IF (HDG7-PRIME = '701' OR '703' OR '705')                    00015268
019800         MOVE 'Y' TO SEL-SW                                       00015269
019200           MOVE 'Y' TO WHEELS-SW                                  00015270
019300             GO TO CHECK-045A-X.                                  00015280
019900                                                                  00015285
020700 CHECK-045A-X.                                                    00015286
020800     EXIT.                                                        00015287
020900                                                                  00015288
022400 WRITE-REPORT SECTION.                                            00015289
022300                                                                  00015290
022500     ADD 1 TO WORK-LINE-NO.                                       00015291
022300                                                                  00015292
022600**   IF WORK-REPORT = 'ECS-045A'                                  00015293
Y2KMOD     IF WORK-REPORT = 'ECS045A'                                   00015293
022700        MOVE 1 TO WORK-COPY.                                      00015294
022900                                                                  00015296
DMBMOD     IF  RUN-MO =  12                                             00015297
023400         NEXT SENTENCE                                            00015298
023400     ELSE                                                         00015299
023400           GO TO WRITE-DISK-RECORDS.                              00015300
022900                                                                  00015301
022900*    CENSTAT                                                      00015302
022900                                                                  00015303
TSTMOD     IF  CENSTAT-SW = 'Y'                                         00015304
TSTMOD           WRITE CENSTAT-RECORD FROM WORK-RECORD                  00015305
023400           GO TO WRITE-DISK-RECORDS.                              00015307
022900                                                                  00015320
022900*    CORNHUSKER                                                   00015321
022900                                                                  00015322
TSTMOD     IF  CORNHUSK-SW =  'Y'                                       00015323
TSTMOD           WRITE CORNHUSK-RECORD FROM WORK-RECORD                 00015325
023400           GO TO WRITE-DISK-RECORDS.                              00015327
022900                                                                  00015331
022900*    MARQUETTE                                                    00015332
022900                                                                  00015333
TSTMOD     IF  MARQUET-SW =  'Y'                                        00015334
TSTMOD           WRITE MARQUET-RECORD FROM WORK-RECORD                  00015336
023400           GO TO WRITE-DISK-RECORDS.                              00015338
TSTMOD                                                                  00015342
023400 WRITE-DISK-RECORDS.                                              00015343
022900                                                                  00015344
TSTMOD     IF  MARQUET-SW =  'Y'                                        00015345
TSTMOD         WRITE MARQUET-DISK-REC FROM WORK-RECORD                  00015346
023400           GO TO WRITE-REPORT-X.                                  00015347
022900                                                                  00015348
TSTMOD     IF  CORNHUSK-SW =  'Y'                                       00015349
TSTMOD         WRITE CORNHUSK-DISK-REC FROM WORK-RECORD                 00015350
023400           GO TO WRITE-REPORT-X.                                  00015351
022900                                                                  00015352
TSTMOD     IF  CENSTAT-SW = 'Y'                                         00015353
TSTMOD         WRITE CENSTAT-DISK-REC FROM WORK-RECORD                  00015354
023400           GO TO WRITE-REPORT-X.                                  00015355
022900                                                                  00015356
022900*    WHEELS AND CENTENNIAL PRODUCE DISK AND PAPER IN EVERY RUN.   00015357
022900*                                       === -----                 00015358
022900*    WHEELS                                                       00015359
022900                                                                  00015360
TSTMOD     IF  WHEELS-SW =  'Y'                                         00015361
TSTMOD           WRITE WHEELS-RECORD FROM WORK-RECORD                   00015362
TSTMOD           WRITE WHEELS-DISK-REC FROM WORK-RECORD                 00015363
023400           GO TO WRITE-REPORT-X.                                  00015364
022900                                                                  00015365
022900*    CENTENNIAL                                                   00015366
022900                                                                  00015367
TSTMOD     IF  CENTEN-SW =  'Y'                                         00015368
TSTMOD           WRITE CENTEN-RECORD FROM WORK-RECORD                   00015369
TSTMOD           WRITE CENTEN-DISK-REC FROM WORK-RECORD                 00015370
023400           GO TO WRITE-REPORT-X.                                  00015371
TSTMOD                                                                  00015373
023400 WRITE-REPORT-X.                                                  00015374
023500     EXIT.                                                        00015375
023600                                                                  00015376
023800 WRITE-HEADINGS SECTION.                                          00015377
023600                                                                  00015378
023900     MOVE SAVE-COMPANY-NAME TO WORK-COMPANY.                      00015379
024000     MOVE HDG1-REPORT       TO WORK-REPORT.                       00015380
024100     MOVE HDG1        TO WORK-LINE.                               00015381
024200     PERFORM WRITE-REPORT.                                        00015382
024300     MOVE HDG2        TO WORK-LINE.                               00015383
024400     PERFORM WRITE-REPORT.                                        00015384
024500     MOVE HDG3        TO WORK-LINE.                               00015385
024600     PERFORM WRITE-REPORT.                                        00015390
024700     MOVE HDG4        TO WORK-LINE.                               00015391
024800     PERFORM WRITE-REPORT.                                        00015392
024900     MOVE HDG5        TO WORK-LINE.                               00015393
025000     PERFORM WRITE-REPORT.                                        00015394
025100     MOVE HDG6        TO WORK-LINE.                               00015395
025200     PERFORM WRITE-REPORT.                                        00015396
025300                                                                  00015397
025400 WRITE-HEADINGS-X.                                                00015398
025500     EXIT.                                                        00015399
025600                                                                  00015400
025800 INITIALIZATION SECTION.                                          00015410
025700                                                                  00015420
025700                                                                  00015440
026000     OPEN INPUT  ECS045-FICHE.                                    00015450
025700                                                                  00015460
026100     OPEN OUTPUT CENSTAT-DISK.                                    00015473
026100     OPEN OUTPUT CENTEN-DISK.                                     00015475
026100     OPEN OUTPUT CORNHUSK-DISK.                                   00015477
026100     OPEN OUTPUT MARQUET-DISK.                                    00015479
026100     OPEN OUTPUT WHEELS-DISK.                                     00015481
CIDMOD                                                                  00015482
026100     OPEN OUTPUT CENSTAT-REPORT.                                  00015483
026100     OPEN OUTPUT CENTEN-REPORT.                                   00015484
026100     OPEN OUTPUT CORNHUSK-REPORT.                                 00015485
026100     OPEN OUTPUT MARQUET-REPORT.                                  00015486
026100     OPEN OUTPUT WHEELS-REPORT.                                   00015487
CIDMOD                                                                  00015488
026200 INITIALIZATION-X.                                                00015870
026300     EXIT.                                                        00015880
026500                                                                  00015890
028900                                                                  00016010
029100 END-OF-JOB SECTION.                                              00016020
029000                                                                  00016030
029200     CLOSE ECS045-FICHE                                           00016040
029300           CENSTAT-DISK                                           00016051
029300           CENTEN-DISK                                            00016053
029300           CORNHUSK-DISK                                          00016055
029300           MARQUET-DISK                                           00016057
029300           WHEELS-DISK                                            00016059
029300           CENSTAT-REPORT                                         00016067
029300           CENTEN-REPORT                                          00016068
029300           CORNHUSK-REPORT                                        00016069
029300           MARQUET-REPORT                                         00016070
029300           WHEELS-REPORT.                                         00016071
029000                                                                  00016072
029400 END-OF-JOB-X.                                                    00016073
029500     EXIT.                                                        00016080
029600                                                                  00016090
029800 ABEND-PGM SECTION.                                               00016091
029700                                                                  00016092
029900     DISPLAY                                                      00016093
029900     'PROGRAM CIDRE010 INTENTIONALLY ABENDED WITH A S0C7.'.       00016094
030000     DISPLAY 'SEE ERROR MESSAGE ABOVE.'.                          00016095
030100     MOVE +16 TO RETURN-CODE.                                     00016096
030200     ADD +1 TO S0C7.                                              00016097
029700                                                                  00016098
030300 ABEND-X.                                                         00016099
030400     EXIT.                                                        00016100
029700                                                                  00016110
