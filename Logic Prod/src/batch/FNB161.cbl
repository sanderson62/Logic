       TITLE 'EXTRACT AGENT HIERARCHY FROM THE POLICY MASTER FILE'      00012022
                                                                        00020012
       IDENTIFICATION DIVISION.                                         00041013
       PROGRAM-ID.    FNB161.                                           00042013
       AUTHOR         DAN DRYDEN.                                       00043013
       DATE-WRITTEN   MAY, 1998.                                        00044013
                                                                        00045013
      ***************************************************************** 00046013
      *                         C H A N G E S                         * 00047014
      ***************************************************************** 00048013
      * DATE      DESCRIPTION                                         * 00049014
      * --------  --------------------------------------------------- * 00049114
      * 01/01/99  1998011500013 - FREEDOM SYSTEM INSTALL              * 00049217
DAN01 * 03/03/00  2000022400003 - CHECK P-TRAILER FOR ISSUE DATE      * 00049317
      *                           CHANGE TO 20 LEVELS OF HIERARCHY    * 00049417
      *                                                               * 00049616
      ***************************************************************** 00049716
                                                                        00050011
       DATA DIVISION.                                                   00060000
       WORKING-STORAGE SECTION.                                         00070000
                                                                        00080000
       01  FILLER.                                                      00090000
           05  SUB          PIC S9(4) COMP SYNC VALUE +0.               00100000
           05  BIN-LVL      PIC S9(2) COMP SYNC VALUE +0.               00110000
           05  REDEFINES BIN-LVL.                                       00120000
               10  FILLER   PIC X.                                      00130000
               10  HEX-LVL  PIC X.                                      00140000
           05  ISS-DATE     PIC S9(5) COMP-3.                           00150000
                                                                        00160009
       COPY TWKAREA SUPPRESS.                                           00170000
                                                                        00180009
       LINKAGE SECTION.                                                 00190000
                                                                        00200000
       COPY PMTRLRCC SUPPRESS.                                          00210000
                                                                        00220000
       01  PARM-HIERARCHY.                                              00230000
DAN01      05  OCCURS 20 TIMES.                                         00240019
               10  PARM-AGT-LVL  PIC 99.                                00250009
               10  PARM-AGT-NO   PIC X(5).                              00260009
                                                                        00270009
                                                                        00280000
           EJECT                                                        00290000
      *                                                                 00300000
       PROCEDURE DIVISION USING PMRECORD, PARM-HIERARCHY.               00310000
      *                                                                 00320000
           INITIALIZE PARM-HIERARCHY                                    00330000
           MOVE +0 TO ISS-DATE                                          00340023
                                                                        00350023
           MOVE SPACE TO 6-BYTE                                         00470009
DAN01      MOVE 'P'   TO PARMREC                                        00480019
           CALL 'PMRDBL' USING PMRECORD, TWKAREA, 4-BYTE, 6-BYTE        00510016
           IF PARMRETURN = '1'                                          00520016
              IF PMAHCD = '1'                                           00521019
                 MOVE PTDATE TO ISS-DATE                                00530019
              ELSE                                                      00540019
                 MOVE AHPISSDATE TO ISS-DATE                            00550019
              END-IF                                                    00551019
           ELSE                                                         00551123
              IF PMAHCD = '1'                                           00551423
                 MOVE PMTAREA TO ATRAILER                               00551524
                 MOVE ATIDAT TO ISS-DATE                                00551624
              ELSE                                                      00551724
                 MOVE PMTAREA TO AHATRAIL                               00551825
                 MOVE AHAISSUE TO ISS-DATE                              00551924
              END-IF                                                    00552024
           END-IF                                                       00553024
                                                                        00564823
           MOVE ZERO  TO SUB                                            00565016
           MOVE SPACE TO 6-BYTE                                         00566016
           MOVE 'C'   TO PARMREC                                        00567016
                                                                        00568016
           PERFORM UNTIL PARMRETURN = '0'                               00569016
              CALL 'PMRDBL' USING PMRECORD, TWKAREA, 4-BYTE, 6-BYTE     00569116
              IF PARMRETURN = '1'                                       00569216
                 PERFORM 100-GET-HIARC THRU 100-EXIT                    00569316
                 MOVE '*' TO PARMNEXT                                   00569416
              END-IF                                                    00569516
           END-PERFORM                                                  00569616
              .                                                         00570000
       FNB161-EXIT.                                                     00580000
           GOBACK.                                                      00590000
                                                                        00600000
                                                                        00610000
      *                                                                 00620000
       100-GET-HIARC.                                                   00630000
      *                                                                 00640015
           IF (ISS-DATE < CTSTRTDATE) OR                                00650016
              (ISS-DATE > CTSTOPDATE)                                   00660016
              GO TO 100-EXIT.                                           00670016
                                                                        00680000
           IF SUB = ZERO                                                00690000
              MOVE +1 TO SUB                                            00700016
              MOVE 01     TO PARM-AGT-LVL (SUB)                         00710009
              MOVE CTWNUM TO PARM-AGT-NO  (SUB)                         00720009
           END-IF                                                       00730000
                                                                        00740009
           IF SUB < 20                                                  00750014
              ADD 1 TO SUB                                              00760000
              MOVE CT1LEV  TO HEX-LVL                                   00770009
              MOVE BIN-LVL TO PARM-AGT-LVL (SUB)                        00780009
              MOVE CT1NUM  TO PARM-AGT-NO  (SUB)                        00790009
           END-IF                                                       00800000
                                                                        00810009
           IF SUB < 20                                                  00820014
              ADD 1 TO SUB                                              00830000
              MOVE CT2LEV  TO HEX-LVL                                   00840009
              MOVE BIN-LVL TO PARM-AGT-LVL (SUB)                        00850009
              MOVE CT2NUM  TO PARM-AGT-NO  (SUB)                        00860009
           END-IF                                                       00870000
                                                                        00880009
           IF SUB < 20                                                  00890014
              ADD 1 TO SUB                                              00900000
              MOVE CT3LEV  TO HEX-LVL                                   00910009
              MOVE BIN-LVL TO PARM-AGT-LVL (SUB)                        00920009
              MOVE CT3NUM  TO PARM-AGT-NO  (SUB)                        00930009
           END-IF                                                       00940000
           .                                                            00950000
       100-EXIT.                                                        00960000
           EXIT.                                                        00970000
                                                                        00980000
