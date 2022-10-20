       IDENTIFICATION DIVISION.                                         00000010
       PROGRAM-ID.    DATEEDIT.                                         00000020
                                                                        00000030
      ****************************************************************  00000040
      *                                                              *  00000050
      *  PARM-INPUT:                                                 *  00000060
      *  ----------                                                  *  00000070
      *    MMDDYYYY  OR  YYYYMMDD  OR  MM/DD/YYYY                    *  00000080
      *                                                              *  00000090
      *    IF THE FIRST 2 POSITIONS OF PARM-INPUT = '19' OR '20',    *  00000100
      *        EDIT FOR YYYYMMDD                                     *  00000110
      *    ELSE                                                      *  00000120
      *    IF PARM-INPUT IS NOT NUMERIC,                             *  00000130
      *        EDIT FOR MM/DD/YYYY                                   *  00000140
      *    ELSE                                                      *  00000150
      *        EDIT FOR MMDDYYYY.                                    *  00000160
      *                                                              *  00000170
      *                                                              *  00000180
      *  PARM-RETURN-CODE:                                           *  00000190
      *  -----------------                                           *  00000200
      *      V = DATE IS VALID                                       *  00000210
      *      I = DATE IS INVALID                                     *  00000220
      *                                                              *  00000230
      ****************************************************************  00000240
                                                                        00000250
       ENVIRONMENT DIVISION.                                            00000260
                                                                        00000270
       DATA DIVISION.                                                   00000280
                                                                        00000290
       WORKING-STORAGE SECTION.                                         00000300
                                                                        00000310
       01  FILLER                  COMP-3.                              00000320
           05  WS-WORK1            PIC S9(3).                           00000330
           05  WS-LEAP-SW          PIC S9.                              00000340
               88  LEAP-YEAR       VALUE +0.                            00000350
                                                                        00000360
       01  WS-INPUT-DATE           PIC X(10).                           00000370
       01  WS-DATE-1 REDEFINES WS-INPUT-DATE.                           00000380
           05  WS-MM-1             PIC XX.                              00000390
           05  WS-DD-1             PIC XX.                              00000400
           05  WS-YR-1             PIC XXXX.                            00000410
       01  WS-DATE-2 REDEFINES WS-INPUT-DATE.                           00000420
           05  WS-YR-2             PIC XXXX.                            00000430
           05  WS-MM-2             PIC XX.                              00000440
           05  WS-DD-2             PIC XX.                              00000450
       01  WS-DATE-3 REDEFINES WS-INPUT-DATE.                           00000460
           05  WS-MM-3             PIC XX.                              00000470
           05  FILLER              PIC X.                               00000480
           05  WS-DD-3             PIC XX.                              00000490
           05  FILLER              PIC X.                               00000500
           05  WS-YR-3             PIC XXXX.                            00000510
                                                                        00000520
       01  WS-EDIT-DATE.                                                00000530
           05  WS-EDIT-MONTH       PIC XX.                              00000540
           05  WS-EDIT-DAY         PIC XX.                              00000550
           05  WS-EDIT-YEAR.                                            00000560
               10  WS-EDIT-CENT    PIC XX.                              00000570
               10  WS-EDIT-YR      PIC XX.                              00000580
                                                                        00000590
                                                                        00000600
                                                                        00000610
       LINKAGE SECTION.                                                 00000620
                                                                        00000630
       01  PARM-INPUT             PIC X(10).                            00000640
       01  PARM-RETURN-CODE       PIC X.                                00000650
                                                                        00000660
                                                                        00000670
                                                                        00000680
       PROCEDURE DIVISION USING PARM-INPUT, PARM-RETURN-CODE.           00000690
                                                                        00000700
           MOVE PARM-INPUT  TO  WS-INPUT-DATE.                          00000710
                                                                        00000720
           IF WS-DATE-1 IS NUMERIC                                      00000730
               IF WS-MM-1 = '19' OR '20'                                00000740
                   MOVE WS-MM-2 TO WS-EDIT-MONTH                        00000750
                   MOVE WS-DD-2 TO WS-EDIT-DAY                          00000760
                   MOVE WS-YR-2 TO WS-EDIT-YEAR                         00000770
               ELSE                                                     00000780
                   MOVE WS-MM-1 TO WS-EDIT-MONTH                        00000790
                   MOVE WS-DD-1 TO WS-EDIT-DAY                          00000800
                   MOVE WS-YR-1 TO WS-EDIT-YEAR                         00000810
           ELSE                                                         00000820
               MOVE WS-MM-3 TO WS-EDIT-MONTH                            00000830
               MOVE WS-DD-3 TO WS-EDIT-DAY                              00000840
               MOVE WS-YR-3 TO WS-EDIT-YEAR.                            00000850
                                                                        00000860
           MOVE 'I' TO  PARM-RETURN-CODE.                               00000870
                                                                        00000880
           IF WS-EDIT-DATE IS NOT NUMERIC                               00000890
               GO TO 9999-EXIT.                                         00000900
                                                                        00000910
           IF WS-EDIT-MONTH < '01' OR WS-EDIT-MONTH > '12'              00000920
               GO TO 9999-EXIT.                                         00000930
                                                                        00000940
           IF WS-EDIT-DAY < '01'   OR WS-EDIT-DAY > '31'                00000950
               GO TO 9999-EXIT.                                         00000960
                                                                        00000970
           IF WS-EDIT-CENT < '19'  OR WS-EDIT-CENT > '20'               00000980
               GO TO 9999-EXIT.                                         00000990
                                                                        00001000
           IF WS-EDIT-MONTH = '04' OR '06' OR '09' OR '11'              00001010
               IF WS-EDIT-DAY > '30'                                    00001020
                   GO TO 9999-EXIT.                                     00001030
                                                                        00001040
           IF WS-EDIT-MONTH = '02'                                      00001050
               CONTINUE                                                 00001060
           ELSE                                                         00001070
               MOVE 'V'  TO  PARM-RETURN-CODE                           00001080
               GO TO 9999-EXIT.                                         00001090
                                                                        00001100
           MOVE WS-EDIT-YR  TO WS-WORK1.                                00001110
           DIVIDE WS-WORK1 BY +4                                        00001120
               GIVING WS-WORK1 REMAINDER WS-LEAP-SW.                    00001130
                                                                        00001140
      *** 1900 WAS NOT A LEAP YEAR ***                                  00001150
           IF WS-EDIT-YEAR = '1900'                                     00001160
               MOVE +1 TO WS-LEAP-SW.                                   00001170
                                                                        00001180
           IF LEAP-YEAR AND WS-EDIT-DAY > '29'                          00001190
               GO TO 9999-EXIT.                                         00001200
                                                                        00001210
           IF NOT LEAP-YEAR AND WS-EDIT-DAY > '28'                      00001220
               GO TO 9999-EXIT.                                         00001230
                                                                        00001240
           MOVE 'V'  TO  PARM-RETURN-CODE.                              00001250
                                                                        00001260
       9999-EXIT.                                                       00001270
           GOBACK.                                                      00001280
                                                                        00001290
